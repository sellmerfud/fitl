
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
// Daring rescue: 2 Troop Casualties to Available.
// NVA Ineligible through next card. US Eligible.
//
// Shaded Text
// No prisoners there: Any 2 Casualties out of play.
// US Ineligible through next card
//
// Tips
// For the unshaded text, place the US Eligibility Cylinder
// from wherever it is into the “Eligible Factions” box.
// If US executed the Event and ARVN 2nd Eligible, 
// ARVN may execute Ops & Special Activity as usual.

object Card_054 extends EventCard(54, "Son Tay",
  DualEvent,
  List(NVA, VC, US, ARVN),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.casualties.has(USTroops) ||
    game.sequence.willBeEligibeNextTurn(NVA)

  def executeUnshaded(faction: Faction): Unit = {
    val num = game.casualties.totalOf(USTroops) min 2

    if (num == 0)
      log("There are no US Troops in the Casualties box")
    else
      moveCasualtiesToAvailable(Pieces(usTroops = num))

    makeIneligibleThroughNextTurn(NVA)
    remainEligibleNextTurn(US)
  }

  def shadedEffective(faction: Faction): Boolean =
    game.casualties.nonEmpty

  def executeShaded(faction: Faction): Unit = {
    if (game.casualties.isEmpty)
      log("There are no pieces in the Casualties box")
    else {
      val pieces = if (game.isHuman(faction))
        askPieces(game.casualties, 2, prompt = Some("Select causualties to move Out of Play"))
      else
        Bot.selectEnemyRemovePlaceActivate(game.casualties, 2)

      moveCasualtiesToOutOfPlay(pieces)
    }

    makeIneligibleThroughNextTurn(US)
  }
}
