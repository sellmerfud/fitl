
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
import scala.util.Random.shuffle
import fitl.FireInTheLake._
import fitl.EventHelpers._
import fitl.Bot
import fitl.Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }
import fitl.Human

// Unshaded Text
// USO: Move any US Troops from a Province to a COIN Control City.
// For each 2 moved (round down), 1 Casualty piece to Available.
//
// Shaded Text
// Show lowers op tempo: NVA or VC move up to 3 US Troops from any Provinces
// to Cities, placing a Guerrilla where each Troop was.
//
// Tips
// "Pieces" include Bases.

object Card_091 extends EventCard(91, "Bob Hope",
  DualEvent,
  List(VC, US, NVA, ARVN),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  val unshadedOriginCandidate = (sp: Space) => sp.isProvince && sp.pieces.has(USTroops)
  val unshadedDestCandidate   = (sp: Space) => sp.isCity && sp.coinControlled

  def unshadedOriginCandidates = game.nonLocSpaces filter unshadedOriginCandidate
  def unshadedDestCandidates   = game.nonLocSpaces filter unshadedDestCandidate

  // Returns Some(origin, dest) or None
  def botSpaces: Option[(String, String)] = unshadedDestCandidates match {
    case Nil   => None
    case dests =>
      val dest = US_Bot.pickSpacePlaceCubesSpecialForces(troops = true)(dests).name
      val origins = spaceNames(unshadedOriginCandidates).toSet
      val optOrigin = Bot.selectMoveOrigin(US, dest, EventMove(Some(origins)), Set(USTroops), Params(), Set.empty)
      optOrigin map (origin => (origin, dest))
  }

  def botMovingTroops(origin: String, dest: String): Int = {
    val toKeep = Bot.selectPiecesToKeep(origin, dest, US, EventMove(None), Set(USTroops), Params())
    val sp     = game.getSpace(origin)
    val movers = sp.pieces.only(USTroops) - toKeep
    movers.total
  }

  def unshadedEffective(faction: Faction): Boolean = {
    game.casualties.has(USPieces) && {
      botSpaces match {
        case Some((origin, dest)) => botMovingTroops(origin, dest) > 1 // So we recover at least one casualty
        case None                 => false
      }
    }
  }

  def executeUnshaded(faction: Faction): Unit = {
    if (game.isHuman(faction)) {
      if (unshadedOriginCandidates.isEmpty)
        log("There are no US Troops in a Province")
      else if (unshadedDestCandidates.isEmpty)
        log("There are no COIN controlled cities")
      else {
        val origin    = askSimpleMenu(spaceNames(unshadedOriginCandidates), "\nSelect a Province with US Troops:").head
        val dest      = askSimpleMenu(spaceNames(unshadedDestCandidates), "\nSelect a COIN controlled city:").head
        val maxTroops = game.getSpace(origin).pieces.totalOf(USTroops)
        val numTroops = askInt(s"\nMove how many troops to $dest", 0, maxTroops)
        val usCas     = game.casualties.only(USPieces)
        val toAvail   = askPieces(usCas, numTroops/2, prompt = Some("Select US Casualties to move to Available"))
        
        println()
        loggingControlChanges {
          movePieces(Pieces(usTroops = numTroops), origin, dest)
          if (usCas.isEmpty)
            log("There are no US Casualties to move to the Available box")
          else if (toAvail.isEmpty)
            log("No US Casualties moved to the Available box")
          else
            moveCasualtiesToAvailable(toAvail, Some("One casualty to Avaialable for each 2 US Troops moved"))
        }
      }
    }
    else {
      val (origin, dest) = botSpaces.get
      // Lose any stray single troop
      val numTroops = (botMovingTroops(origin, dest) / 2 * 2) min (game.casualties.totalOf(USPieces) * 2)
      loggingControlChanges {
        Bot.doEventMoveTo(dest, US, numTroops, true, Set(USTroops), onlyFrom = Some(Set(origin)))
        var toAvail = Bot.selectFriendlyToPlaceOrMove(game.casualties.only(USPieces), numTroops / 2)
        moveCasualtiesToAvailable(toAvail, Some("One casualty to Avaialable for each 2 US Troops moved"))
      }
    }
  }

  val shadedOriginCandidate = (sp: Space) => sp.isProvince && sp.pieces.has(USTroops)

  def shadedOriginCandidates = game.nonLocSpaces filter shadedOriginCandidate
  def shadedDestCandidates   = game.citySpaces
  def shadedBotDests         = game.citySpaces filter (_.population == 1)

  def shadedEffective(faction: Faction): Boolean = shadedOriginCandidates.nonEmpty

  def executeShaded(faction: Faction): Unit = {
    if (game.isHuman(faction)) {
      def nextRemoval(numRemaining: Int): Unit = if (numRemaining > 0 && shadedOriginCandidates.nonEmpty) {
        val origin    = askSimpleMenu(spaceNames(shadedOriginCandidates), "\nSelect a Province with US Troops:").head
        val dest      = askSimpleMenu(spaceNames(shadedDestCandidates), "\nSelect a destination city:").head
        val maxTroops = game.getSpace(origin).pieces.totalOf(USTroops) min numRemaining
        val numTroops = askInt(s"\nMove how many US Troops to $dest", 0, maxTroops)
        val numGs     = Human.numToPlace(NVAGuerrillas_U, 1)

        log()
        movePieces(Pieces(usTroops = numTroops), origin, dest)
        placePieces(origin, Pieces(nvaGuerrillas_U = numGs))
        nextRemoval(numRemaining - numTroops)
      }

      loggingControlChanges {
        nextRemoval(3)
      }
    }
    else {
      def nextRemoval(numRemaining: Int): Unit = if (numRemaining > 0 && shadedOriginCandidates.nonEmpty) {
        val origin = NVA_Bot.pickSpaceRemoveReplace(shadedOriginCandidates)
        val dest   = shuffle(shadedBotDests).head

        log()
        movePieces(Pieces(usTroops = 1), origin.name, dest.name)
        if (game.availablePieces.has(NVAGuerrillas_U))
          placePieces(origin.name, Pieces(nvaGuerrillas_U = 1))
        nextRemoval(numRemaining - 1)
      }

      loggingControlChanges {
        nextRemoval(3)
      }
    }
  }
}
