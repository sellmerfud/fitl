
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
// Air defense suppression: Remove shaded SA-2s or, if no 
// shaded SA-2s, Degrade Trail 2 boxes and NVA Resources –9.
//
// Shaded Text
// Complex strike packages: Until Coup, Air Strike either 
// Degrades Trail or may remove just 1 piece (not 1-6).
// MOMENTUM
// 
// Tips
// Unshaded “Wild Weasels” can remove shaded “SA-2s” only at the
// moment that “Wild Weasels” is executed. Shaded effect means that
// US has to choose with each Air Strike Special Activity whether to
// remove enemy pieces or Degrade the Trail, not both.

object Card_005 extends EventCard(5, "Wild Weasels",
  DualEvent,
  List(US, NVA, ARVN, VC),
  ListMap(US   -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          ARVN -> (Performed   -> Unshaded),
          VC   -> (Performed   -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean =
    capabilityInPlay(SA2s_Shaded) ||
    game.trail > TrailMin         ||
    (game.trackResources(NVA) && game.nvaResources > 0)

  def executeUnshaded(faction: Faction): Unit = {
    if (capabilityInPlay(SA2s_Shaded))
      removeCapabilityFromPlay(SA2s_Shaded)
    else {
      degradeTrail(2)
      decreaseResources(NVA, 9)
    }
  }

  def shadedEffective(faction: Faction): Boolean = true
  def executeShaded(faction: Faction): Unit = playMomentum(Mo_WildWeasels)
}
