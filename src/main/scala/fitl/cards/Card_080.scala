
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

// Single Event Text
// Wind down seen: Remove 1-4 US pieces from map to Available. For each piece,
// Patronage +2, shift a space 1 level toward Active Opposition, and place 4
// NVA Troops outside the South. Stay Eligible.
//
// Tips
// The Faction executing the Event decides the details. "Pieces" can include Bases.
// A Province with 0 Population cannot shift (1.6). "Outside the South" means any
// North Vietnam, Laos, or Cambodia Provinces. 
// The Faction executing the Event stays Eligible for the next card.

object Card_080 extends EventCard(80, "Light at the End of the Tunnel",
  SingleEvent,
  List(ARVN, NVA, VC, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (Performed   -> Unshaded),
          VC   -> (Critical    -> Unshaded))) {


  val shiftCandidate = (sp: Space) =>
    sp.canHaveSupport &&
    sp.support > ActiveOpposition

  def unshadedEffective(faction: Faction): Boolean = {
    val usOnMap = game.totalOnMap(_.pieces.totalOf(USPieces)) > 0
    faction match {
      case US   => false
      case ARVN => usOnMap && game.patronage < EdgeTrackMax
      case NVA  => usOnMap && game.availablePieces.has(NVATroops)
      case VC   => usOnMap && (game.nonLocSpaces exists shiftCandidate)
    }
  }

  // ARVN Bot removes US pieces from the spaces with the most US Pieces
  def arvnBotRemovePieces(numRemaining: Int): Unit = {
    val candidates = game.spaces filter (_.pieces.has(USPieces))
    if (numRemaining > 0 && candidates.nonEmpty) {
      val sp = Bot.pickSpaceWithMostPieces(USPieces)(candidates)
      val piece = Bot.selectFriendlyRemoval(sp.pieces.only(USPieces), 1)
      removeToAvailable(sp.name, piece)
      arvnBotRemovePieces(numRemaining - 1)
    }
  }
  
  val arvnShiftPriorities = List(
    new Bot.BooleanPriority[Space]("1 Pop", _.population == 1),
    new Bot.BooleanPriority[Space]("Active Support", _.support == ActiveSupport),
    new Bot.BooleanPriority[Space]("Neutral", _.support == Neutral),
    new Bot.BooleanPriority[Space]("Passive Opposition", _.support == PassiveOpposition)
  )

  def shiftSpaces(faction: Faction, num: Int, shiftCandidates: List[Space]): Unit = {
    val selectedSpaces = if (shiftCandidates.isEmpty)
      Nil
    else if (game.isHuman(faction)) {
      val numSpaces = num min shiftCandidates.size
      val prompt = s"\nSelect ${amountOf(numSpaces, "space")} to shift toward Active Opposition:"
      askSimpleMenu(spaceNames(shiftCandidates), prompt, numSpaces)
    }
    else {
      def nextSpace(remaining: Int, candidates: List[Space]): List[String] = {
        if (remaining > 0 && candidates.nonEmpty) {
          val name = faction match {
            case VC  => VC_Bot.pickSpaceTowardActiveOpposition(candidates).name
            case NVA => Bot.pickSpaceWithMostSupport(candidates).name
            case _   => Bot.bestCandidate(candidates, arvnShiftPriorities).name
          }
          name::nextSpace(remaining - 1, candidates filterNot (_.name == name))
        }
        else
          Nil
      }
      nextSpace(num, shiftCandidates).reverse
    }

    if (selectedSpaces.isEmpty)
      log("\nThere are no spaces that can be shifted toward Active Opposition")
    else
      for (name <- selectedSpaces)
        decreaseSupport(name, 1)
  }

  def executeUnshaded(faction: Faction): Unit = {
    val numUSOnMap = game.totalOnMap(_.pieces.totalOf(USPieces))
    if (numUSOnMap == 0)
      log("There are no US pieces on the map")
    else {
      val shiftCandidates = game.nonLocSpaces filter shiftCandidate
      val num =  if (game.isHuman(faction))
        askInt("\nRemove how many US pieces from the map", 1, numUSOnMap min 4)
      else
        numUSOnMap min d3  // Bots roll d3 to determine number

      loggingControlChanges {
        if (faction == ARVN && game.isBot(ARVN))
          arvnBotRemovePieces(num)
        else
          removePiecesFromMap(faction, num, USPieces, false, spaceNames(game.spaces), usToAvailable = true)
        log()
        increasePatronage(num * 2)
        log()
        shiftSpaces(faction, num, shiftCandidates)
        log()
        if (game.isHuman(faction) || faction == NVA)
          placePiecesOnMap(faction, num * 4, Set(NVATroops), NorthVietnam::LaosCambodia)
        else {
          // ARVN and VC place all troop in North Vietnam
          val numTroops = game.availablePieces.totalOf(NVATroops) min (num * 4)
          val troops = Pieces().set(numTroops, NVATroops)
          placePieces(NorthVietnam, troops)
        }
      }
      remainEligibleNextTurn(faction)      
    }
  }

  // Single event - These functions are not used
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
