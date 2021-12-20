
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

// US Pivotal event
//
// Play if 2+ cards in RVN Leader box and Support+Available > 40.
//
// Unrestricted air war: NVA removes 2 Bases, reduces Resources to half
// (round down), Ineligible through next card.
// 3 US Casualties to Available.
//
// Tips
// Do not count Duong Van Minh—an RVN leader but not a card—for the precondition (2.4.1).
// The NVA Faction decides which 2 of its Bases to remove, then loses Resources and is
// marked Ineligible. The US decides which Casualties (including Bases) then move to Available.

object Card_121 extends EventCard(121, "Linebacker II",
  SingleEvent,
  List(US, ARVN, VC, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Unshaded),
          VC   -> (NotExecuted -> Unshaded))) {

  // Is US pivotal event playable?
  def unshadedEffective(faction: Faction): Boolean = {
    val threshold = if (game.peaceTalks) 25 else 40
    game.numCardsInLeaderBox >= 2 && game.usPoints > threshold
  }

  def executeUnshaded(faction: Faction): Unit = {
    val numBases     = game.totalOnMap(_.pieces.totalOf(NVABases)) min 2
    val usCasualties = game.casualties.only(USPieces)
    val numUS        = usCasualties.total min 3

    log("\nNVA must remove 2 Bases from the map")
    log(separator(char = '='))
    if (numBases == 0)
      log("There are no NVA Bases on the map")
    else
      removePiecesFromMap(NVA, numBases, Seq(NVABase, NVATunnel),
                          friendly = true,
                          validSpaces = spaceNames(game.nonLocSpaces))


    log("\nReduce NVA resources by half (rounded down)")
    log(separator(char = '='))
    if (game.trackResources(NVA))
      decreaseResources(NVA, game.nvaResources / 2)
    else
      log("NVA resources are not used by the NVA Bot")

    makeIneligibleThroughNextTurn(NVA, false)

    log("\n3 US Casualties to Available")
    log(separator(char = '='))
    val toAvail = if (numUS == usCasualties.total)
      usCasualties
    else if (game.isHuman(US))
      askPieces(usCasualties, numUS)
    else
      Bot.selectFriendlyToPlaceOrMove(usCasualties, numUS)

    if (toAvail.isEmpty)
      log("\nThere are no US Casualties")
    else {
      println()
      moveCasualtiesToAvailable(toAvail)
    }
  }

  // Shaded functions not used for Pivotal Event  
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
