
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

// US CAPABILITY
//
// Unshaded Text
// Armored Punch: 2 non-Lowland US Assault spaces each remove 2 extra enemy pieces.
//
// Shaded Text
// RPGs: After US/ARVN Patrol, NVA removes up to 2 cubes that moved (US to Casualties).
//
// Tips
// The unshaded text changes US Assault only by adding to the number of pieces to be
// removed from some spaces – restrictions on which pieces may be removed (3.2.4) remain
// the same. “US to Casualties” means that any US pieces removed go to the Casualties box,
// while ARVN pieces removed go to ARVN Available Forces as usual (1.4.1).

object Card_014 extends EventCard(14, "M-48 Patton",
  DualEvent,
  List(US, ARVN, NVA, VC),
  ListMap(US   -> (Critical  -> Unshaded),
          ARVN -> (Performed -> Unshaded),
          NVA  -> (Critical  -> Shaded),
          VC   -> (Critical  -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = playCapability(M48Patton_Unshaded)

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = playCapability(M48Patton_Shaded)
}
