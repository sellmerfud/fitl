
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
// Air-delivered seismic intrusion detector: Through Coup,
// –6 NVA Resources at any Trail# change.
// MOMENTUM
//
// Shaded Text
// Dubious technology: Improve Trail by 1 box and to a minimum of 2.
// ARVN Resources –9.
//
// Tips
// The unshaded text drops NVA Resources upon both Improving and Degrading
// the Trail via any means (player action, Event, or Coup Round Sequence).

object Card_007 extends EventCard(7, "ADSID",
  DualEvent,
  List(US, NVA, VC, ARVN),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Ignored -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = game.trackResources(NVA)
      
  def executeUnshaded(faction: Faction): Unit = playMomentum(Mo_ADSID)

  def shadedEffective(faction: Faction): Boolean =
    game.trail < TrailMax || (game.trackResources(ARVN) && game.arvnResources > 0)
 
  def executeShaded(faction: Faction): Unit = {
    if (game.trail < TrailMax) {
      val num = if (game.trail < 2) 2 - game.trail else 1
      improveTrail(num)
      decreaseResources(ARVN, 9)
    }
  }
}
