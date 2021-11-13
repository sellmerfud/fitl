
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
// Minority fighters: Remove any 4 Insurgent pieces total from spaces with Irregulars.
//
// Shaded Text
// Tribal secession: Replace all Irregulars with VC Guerrillas.
// 1 Neutral Highland to Active Opposition.
// -3 Patronage.
//
// Tips 
// "Pieces" include unTunneled Bases (1.4, 5.1.1). "Insurgent" means NVA or VC.
// "Irregulars" are US Special Forces pieces, not ARVN Rangers. Replace Irregulars
// that are on the map (only), with Guerrillas 1 for 1 in place (Irregulars to Available).
// "1 Highland" means 1 Highland space.

object Card_029 extends EventCard(29, "Tribesmen",
  DualEvent,
  List(US, VC, ARVN, NVA),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  val isTribesmenUnshadedSpace = (sp: Space) =>
    sp.pieces.has(Irregulars) &&
    sp.pieces.has(NVABase::VCBase::InsurgentForces)


  def unshadedEffective(faction: Faction): Boolean =
    game.spaces exists isTribesmenUnshadedSpace

  def executeUnshaded(faction: Faction): Unit = {
    val validSpaces = spaceNames(game.spaces filter isTribesmenUnshadedSpace)
    if (validSpaces.isEmpty)
      log("There are no spaces with Irregulars and Insurgent pieces")
    else
      loggingControlChanges {
        removePiecesFromMap(faction, 4, NVABase::VCBase::InsurgentForces, false, validSpaces)
      }
  }
  
  val highlandCandidate = (sp: Space) =>
    sp.population > 0 && sp.isHighland && sp.support == Neutral

  def shadedEffective(faction: Faction): Boolean = {
    game.patronage > 0 ||
    (game.availablePieces.has(VCGuerrillas_U) && game.totalOnMap(_.pieces.totalOf(Irregulars)) > 0) ||
    (game.nonLocSpaces exists highlandCandidate)
  }

  def executeShaded(faction: Faction): Unit = {
    val totalIrreg = game.totalOnMap(_.pieces.totalOf(Irregulars))
    val numAvailGs = game.availablePieces.totalOf(VCGuerrillas_U)

    def replaceAll(): Unit = {
      for (sp <- game.spaces if sp.pieces.has(Irregulars)) {
        val irreg = sp.pieces.only(Irregulars)
        removeToAvailable(sp.name, irreg)
        placePieces(sp.name, Pieces(vcGuerrillas_U = irreg.total))
      }
    }

    def humanNextIrreg(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter (_.pieces.has(Irregulars)))
      if (candidates.nonEmpty) {
        val name  = askCandidate("Replace Irregulars in which space: ", candidates)
        val sp    = game.getSpace(name)
        val irreg = sp.pieces.only(Irregulars)
        val num   = askInt(s"Replace how many Irregulars in $name", 0, irreg.total)
        val dead  = askPieces(irreg, num)
        removeToAvailable(name, dead)
        placePieces(name, Pieces(vcGuerrillas_U = num))
        humanNextIrreg(numRemaining - num)
      }
    }

    def botNextIrreg(): Unit = {
      val candidates = game.spaces filter (_.pieces.has(Irregulars))
      if (candidates.nonEmpty && game.availablePieces.has(VCGuerrillas_U)) {
        val sp = VC_Bot.pickSpaceRemoveReplace(candidates)
        val irreg = Bot.selectEnemyRemovePlaceActivate(sp.pieces.only(Irregulars), 1)
        removeToAvailable(sp.name, irreg)
        placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
        botNextIrreg()
      }
    }

    loggingControlChanges {
      if (numAvailGs >= totalIrreg)
          replaceAll()
      else if (game.isHuman(faction)) {
        val gdisp = thereAre(numAvailGs, "available VC Guerrilla")
        println(s"There ${gdisp} and ${amountOf(totalIrreg, "Irregulars")} on the map")
        val numRemove = askInt("How many VC Guerrillas do you wish to voluntarily remove", 0, totalIrreg - numAvailGs)
        if (numRemove > 0)
          voluntaryRemoval(numRemove, VCGuerrillas_U)
        val num = numAvailGs + numRemove
        if (num == totalIrreg)
          replaceAll()
        else
          humanNextIrreg(num)
      }
      else { // Bot
        botNextIrreg()
      }

      val highland = game.nonLocSpaces filter highlandCandidate
      if (highland.isEmpty)
        log(s"\nThere are no Neutral Highland provinces")
      else {
        val name = if (game.isHuman(faction))
          askCandidate("\nSet which Neutral highland province to Active Opposition: ", spaceNames(highland))
        else
          VC_Bot.pickSpaceTowardActiveOpposition(highland).name
        setSupport(name, ActiveOpposition)
      }
      decreasePatronage(3)
    }
  }
}
