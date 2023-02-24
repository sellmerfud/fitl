
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
// Accelerated Pacification Campaign: US and ARVN immediately Pacify
// as if Support Phase, but cost is 0. Shift at most 1 level per space.
//
// Shaded Text
// False progress: If Tet Offensive played, return it to VC. If not,
// VC execute "General uprising" as on the card (without using it).
//
// Tips
// For the unshaded text, the Pacification also removes Terror markers for cost 0.
// For the shaded text, if Event #124 "Tet Offensive" has not been executed
// (including because it is not used in this scenario), execute the "Tet Offensive"
// Event text that follows the flavor text "General uprising".
// The disposition of the "Tet Offensive" card is not affected.

object Card_096 extends EventCard(96, "APC",
  DualEvent,
  List(VC, US, ARVN, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = {
    game.nonLocSpaces exists { sp =>
      pacifyCandidate(US)(sp) ||
      pacifyCandidate(ARVN)(sp)
    }
  }

  def executeUnshaded(faction: Faction): Unit = {
    val params = SupportParams(
    forEvent = true,
    free     = true,
    maxLevels = 1,
    factions = Set(US, ARVN))

    coupSupportPhase(params)
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {

    if (game.pivotCardsAvailable(VC)) {
      // Tet Offensive has not been played, to execute the event
      log("VC executes the Tet Offensive event")
      log(separator(char = '='))
      Card_124.executeUnshaded(VC)
    }
    else {
      log("Return the Tet Offensive pivotal event card to VC")
      game = game.copy(pivotCardsAvailable = game.pivotCardsAvailable + VC)
    }
  }
}
