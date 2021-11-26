
//  _____ _            _         _   _            _          _
// |  ___(_)_ __ ___  (_)_ __   | |_| |__   ___  | |    __ _| | _____
// | |_  | | '__/ _ \ | | '_ \  | __| '_ \ / _ \ | |   / _` | |/ / _ \
// |  _| | | | |  __/ | | | | | | |_| | | |  __/ | |__| (_| |   <  __/
// |_|   |_|_|  \___| |_|_| |_|  \__|_| |_|\___| |_____\__,_|_|\_\___|
//
//
// An scala implementation of the solo Tru'ng bots for the game
// Fire in the Lake, designed by Mark Herman and Volko Ruhnke
// published by GMT Games.
//
// Copyright (c) 2021 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


package fitl.cards

import scala.collection.immutable.ListMap
import fitl.FireInTheLake._
import fitl.EventHelpers._
import fitl.Bot
import fitl.Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }
import fitl.Human

// Unshaded Text
// Delta boats: Remove all NVA/VC from Mekong LoCs. US or ARVN free
// Sweep into/in then free Assault each Lowland touching Mekong.
//
// Shaded Text
// VC river fortifications: Place 2 VC Guerrillas per Mekong LoC space,
// then Sabotage each that has more VC than COIN.
//
// Tips
// Whichever Faction executed the unshaded text would choose either US or ARVN
// to Sweep and Assault, and that Sweeping/Assaulting Faction would choose how (5.1).
// The Sweep could occur even during Monsoon. Free US Assault can add an ARVN Assault
// at cost 0 (3.2.4). "Mekong" means any of the 3 river LoC spaces touching Can Tho.
// "COIN" means either US or ARVN pieces.

object Card_025 extends EventCard(25, "TF-116 Riverines",
  DualEvent,
  List(US, VC, NVA, ARVN),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val LowlandTouchingMekong = List(KienPhong, KienHoa_VinhBinh, BaXuyen, KienGiang_AnXuyen)
  

  def unshadedEffective(faction: Faction): Boolean = {
    val emptyGroups = new MovingGroups()
    
    (spaces(MekongLoCs) exists (_.pieces.has(InsurgentPieces))) ||
    (LowlandTouchingMekong exists { name =>
      val sp = game.getSpace(name)
      sp.pieces.has(InsurgentPieces) &&
      (sp.pieces.has(ARVNForces) || sweepSources(name, ARVN, emptyGroups).nonEmpty)
    })
  }

  def executeUnshaded(faction: Faction): Unit = {
    loggingControlChanges {
      var removedSome = false
      for (name <- MekongLoCs) {
        val sp       = game.getSpace(name)
        val toRemove = sp.pieces.only(InsurgentPieces)
        if (toRemove.nonEmpty && !removedSome) {
          removedSome = true
          log("Remove all NVA/VC from Mekong LoCs")
        }
        removePieces(name, toRemove)
      }
      if (!removedSome)
        log("There are no NVA/VC pieces on Mekong LoCs")

      if (game.isHuman(faction)) {
        def nextSpace(assaulter: Faction, candidates: List[String]): Unit = if (candidates.nonEmpty) {
          val choices = (candidates map (n => n -> n)) :+
                ("finished" -> "Do not Sweep/Assault any more spaces")
          askMenu(choices, "\nSelect next space to Sweep/Assault:").head match {
            case "finished" =>
            case name =>
              val params = Params(event = true, free = true, singleTarget = Some(name))
              Human.initTurnVariables(false)
              Human.executeSweep(assaulter, params)
              Human.initTurnVariables(false)
              Human.performAssault(assaulter, name, params)
              val addArvnAssault =
                assaulter == US &&
                assaultEffective(ARVN, false, false)(game.getSpace(name)) &&
                askYorN(s"\nAdd a free ARVN assault in $name? (y/n) ")
              if (addArvnAssault) {
                Human.initTurnVariables(false)
                Human.performAssault(ARVN, name, params)
              }
              nextSpace(assaulter, candidates filterNot (_ == name))
          }
        }
        log("\nSweep/Assault each Lowland Province touch Mekong")
        log(separator())
        val choices = List(US, ARVN).map(f => f -> f.toString)
        val assaulter = askMenu(choices, "Sweep/Assault using which faction:").head
        nextSpace(assaulter, LowlandTouchingMekong)
      }
      else {
        // Only ARVN Bot will execute this command
        log("\nSweep/Assault each Lowland Province touch Mekong")
        log(separator())
        for (name <- LowlandTouchingMekong if game.getSpace(name).pieces.has(InsurgentPieces)) {
          val params = Params(event = true, free = true, singleTarget = Some(name))
          Bot.initTurnVariables()  // Treat each space as a fresh turn
          ARVN_Bot.sweepOp(params, 3)
          Bot.initTurnVariables()  // Treat each space as a fresh turn
          Bot.performAssault(ARVN, name, params)
        }
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(VCGuerrillas_U)

  def executeShaded(faction: Faction): Unit = {
    val maxGs = MekongLoCs.size * 2
    val numAvailGs = game.availablePieces.totalOf(VCGuerrillas_U) min maxGs

    if (numAvailGs == maxGs) {
      loggingControlChanges {
        for (name <- MekongLoCs)
          placePieces(name, Pieces(vcGuerrillas_U = 2))
      }
    }
    else if (game.isHuman(faction)) {
      def nextLoc(candidates: List[String]): Unit = if (candidates.nonEmpty) {
        val name = askSimpleMenu(candidates, "\nPlace guerrillas on which Mekong LoC:").head
        val pieces = askPiecesToPlace(name, Set(VCGuerrillas_U), 2)
        placePieces(name, pieces)
        nextLoc(candidates filterNot (_ == name))
      }
      println("There are less than 6 available VC Guerrillas")
      println("You may move guerrillas from other spaces to make up the difference")
      loggingControlChanges {
        nextLoc(MekongLoCs)
      }
    }
    else {
      def nextLoC(numRemaining: Int, candidates: List[Space]): Unit = if (numRemaining > 0) {
        val sp = VC_Bot.pickSpacePlaceGuerrillas(candidates)
        val num = numRemaining min 2
        placePieces(sp.name, Pieces(vcGuerrillas_U = num))
        nextLoC(numRemaining - num, candidates filterNot (_.name == sp.name))
      }
      loggingControlChanges {
        nextLoC(numAvailGs, spaces(MekongLoCs))
      }
    }

    // Place sabotage markers where VC > COIN
    if (game.terrorMarkersAvailable > 0) {
      log()
      for (name <- MekongLoCs) {
        if (game.terrorMarkersAvailable > 0) {
          val sp = game.getSpace(name)
          if (sp.pieces.totalOf(VCPieces) > sp.pieces.totalOf(CoinPieces))
            addTerror(name, 1)
        }
      }
    }
    else
      log("\nThere are no available sabotage markers")
  }
}
