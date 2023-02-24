
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
// Gruesome protests close elite ranks: Patronage +3 or, if Saigon at Active Support, +6.
//
// Shaded Text
// Anti-regime self-immolation: Shift Saigon 1 level toward Active Opposition. Aid –12.
//
// Tips
// Adjust the ARVN victory marker (COIN Control + Patronage) when increasing or decreasing Patronage,
// the US victory marker (Support + Available) if changing Support, and the VC victory marker
// (Opposition + Bases) if adding Opposition.

object Card_107 extends EventCard(107, "Burning Bonze",
  DualEvent,
  List(VC, NVA, ARVN, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.patronage < EdgeTrackMax

  def executeUnshaded(faction: Faction): Unit = {
    val num = if (game.getSpace(Saigon).support == ActiveSupport) 6 else 3

    increasePatronage(num)
  }

  def shadedEffective(faction: Faction): Boolean =
    (game.getSpace(Saigon).support != ActiveOpposition) ||
    (game.trackResources(ARVN) && game.usAid > 0)

  def executeShaded(faction: Faction): Unit = {
    decreaseSupport(Saigon, 1)
    decreaseUsAid(12)
  }
}
