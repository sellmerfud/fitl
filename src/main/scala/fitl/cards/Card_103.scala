
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
// National Guard imposes order: Any 2 US Casualties to Available.
// 1 free US LimOp. US Eligible.
//
// Shaded Text
// National Guard overreacts: Up to 3 US Troop Casualties out of play.
// Aid –6. US Ineligible through next card.
//
// Tips
// For the unshaded text, place the US Eligibility Cylinder from wherever
// it is into the “Eligible Factions” box. If US executed the Event and
// ARVN 2nd Eligible, ARVN may execute Ops & Special Activity as usual.

object Card_103 extends EventCard(103, "Kent State",
  DualEvent,
  List(VC, NVA, US, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Ignored -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.casualties.has(USPieces)

  def executeUnshaded(faction: Faction): Unit = {
    val usCasualties = game.casualties.only(USPieces)
    val toAvail = if (game.isHuman(faction))
      askPieces(usCasualties, 2 min usCasualties.total, prompt = Some("Select US Casualties to move to Available"))
    else
      Bot.selectFriendlyToPlaceOrMove(usCasualties, 2 min usCasualties.total)

    println()
    if (toAvail.isEmpty)
      log("There are no US Casualties")
    else
      moveCasualtiesToAvailable(toAvail)

    log("\nUS executes a free Limited Op")
    log(separator(char = '='))
    pause()

    val limOpParams = Params(free = true, maxSpaces = Some(1))
    if (game.isHuman(US))
      Human.executeCoinOp(US, limOpParams)
    else {
      Bot.executeOp(US, limOpParams)
      pause()
    }

    // US becomes eligible
    // This is a bit funky because the US cannot act again if
    // it just took the action.
    // So if the the US has acted or is the currently executing
    // this event,  we mark it to stay eligible for the next turn,
    // otherwise, we move the US cylinder to the eligible box.
    println()
    if (faction == US || game.sequence.acted(US))
      remainEligibleNextTurn(US)
    else
      makeEligible(US)
  }

  def shadedEffective(faction: Faction): Boolean =
    game.casualties.has(USTroops) ||
    (game.trackResources(ARVN) && game.usAid > 0)

  def executeShaded(faction: Faction): Unit = {
    val maxCasualties = game.casualties.totalOf(USTroops) min 3
    val numTroops = if (game.isHuman(faction))
      askInt("\nMove how many US Troop Casualties to Out Of Play", 0, maxCasualties)
    else
      maxCasualties

    println()
    moveCasualtiesToOutOfPlay(Pieces(usTroops = numTroops))
    decreaseUsAid(6)
    makeIneligibleThroughNextTurn(US, faction == US)
  }
}
