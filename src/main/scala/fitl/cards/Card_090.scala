
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
// COIN portfolio: Place any 2 ARVN pieces from anywhere
// (even out of play) into any COIN Control spaces.
//
// Shaded Text
// The enemy owns the night: Place any 1 Guerrilla in each Province
// with ARVN. ARVN Troops Redeploy as if no Bases.
//
// Tips
// "Pieces" include Bases. For the shaded effect, the Redeploy is once and 
// immediate—ARVN Troops depart all Provinces and LoCs for Cities with no NVA
// Control or Saigon (6.4.2).

object Card_090 extends EventCard(90, "Walt Rostow",
  DualEvent,
  List(ARVN, VC, NVA, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = false  // Not executed by Bots

  def placeArvnPieces(faction: Faction, coinControlledSpaces: List[String]): Unit = {
    val oopArvn = game.outOfPlay.only(ARVNPieces)
    val numOop = oopArvn.total match {
      case 0 => 0
      case 1 =>
          val desc = oopArvn.getTypes.head.genericSingular
          if (askYorN(s"\nWould you like to place the $desc from the OUT OF PLAY box? (y/n) ")) 1 else 0
      case n =>
        askInt("How many OUT OF PLAY ARVN pieces would you like to place:", 0, 2)
    }
    val numOther = 2 - numOop

    placeOutOfPlayPiecesOnMap(faction, numOop, ARVNPieces, coinControlledSpaces)
    placePiecesOnMap(faction, numOther, ARVNPieces, coinControlledSpaces)
  }


  def executeUnshaded(faction: Faction): Unit = {
    val candidates = spaceNames(game.nonLocSpaces filter (_.coinControlled))
    if (candidates.isEmpty)
      log("There are no COIN controlled spaces")  // Not likely!
    else
      loggingControlChanges {
        placeArvnPieces(faction, candidates)
      }
  }

  def placeVCGuerrillas(faction: Faction): Unit = {
    val (factionGuerrilla, otherGuerrilla) = if (faction == NVA)
      (NVAGuerrillas_U, VCGuerrillas_U)
    else
      (VCGuerrillas_U, NVAGuerrillas_U)
    val spacesWithArvn = game.spaces filter (sp => sp.isProvince && sp.pieces.has(ARVNPieces))

    if (game.isHuman(faction)) {
      def availGuerrillas = game.availablePieces.only(Guerrillas)
      def willContinue: Boolean = {
        println("\nThere are no VC or NVA Guerrillas in the AVAILABLE box.")
        askYorN(s"Do you wish to continue placing $faction Guerrillas from the map? (y/n) ")
      }

      def nextPlacement(candidates: List[String]): Unit = if (candidates.nonEmpty) {
        if (availGuerrillas.nonEmpty || willContinue) {
          val choices = List(
            choice(true,                      factionGuerrilla, s"Place ${factionGuerrilla.genericSingular}"),
            choice(availGuerrillas.has(otherGuerrilla), otherGuerrilla,   s"Place ${otherGuerrilla.genericSingular}")
          ).flatten

          val name  = askSimpleMenu(candidates, "\nChoose space to place a Guerrilla:").head
          val gtype = askMenu(choices, s"\nPlace which type of Guerrilla at $name:").head

          val num = if (gtype == factionGuerrilla)
            Human.numToPlace(gtype, 1)
          else
            1  // Other will always come from Available

          placePieces(name, Pieces().set(num, gtype))
          val newCandidates =  if (num == 0)
            candidates
          else
            candidates filterNot (_ == name)
          nextPlacement(newCandidates)
        }
      }

      val oneEach = if (availGuerrillas.totalOf(factionGuerrilla) >= spacesWithArvn.size) {
        if (availGuerrillas.has(otherGuerrilla)) {
          val choices = List(
            true  -> s"Place only ${factionGuerrilla.genericPlural} in all spaces",
            false -> s"Place ${factionGuerrilla.genericPlural} in some spaces and ${otherGuerrilla.genericPlural} in others")
          askMenu(choices, "\nChoose one:").head
        }
        else
          true
      }
      else
        false

      if (oneEach) {
        for (name <- spaceNames(spacesWithArvn))
          placePieces(name, Pieces().set(1, factionGuerrilla))
      }
      else
        nextPlacement(spaceNames(spacesWithArvn))
    }
    else {

      def nextPlacement(candidates: List[Space]): Unit = if (candidates.nonEmpty) {
        val guerrilla = if (game.availablePieces.has(factionGuerrilla))
          Pieces().set(1, factionGuerrilla)
        else if (game.availablePieces.has(otherGuerrilla))
          Pieces().set(1, otherGuerrilla)
        else
          Pieces()

        if (guerrilla.nonEmpty) {
          val sp = Bot.pickSpacePlaceForces(faction)(candidates)
          placePieces(sp.name, guerrilla)
          nextPlacement(candidates filterNot (_.name == sp.name))
        }
      }

      nextPlacement(spacesWithArvn)
    }
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {

    loggingControlChanges {
      placeVCGuerrillas(faction: Faction)
      log("\nARVN Troops Redeploy (not to COIN bases)")
      log(separator())
      if (game.isBot(ARVN))
        Bot.ARVN_Bot.redeployARVNForces(troops = true, police = false, ignoreCoinBases = true)
      else
        Human.redeployARVNForces(troops = true, police = false, ignoreCoinBases = true)
    }

  }
}
