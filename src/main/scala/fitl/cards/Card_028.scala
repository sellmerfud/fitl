
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
// Mobile counter-guerrilla ops: Each US Assault space may remove
// 1 Underground Guerrilla.
//
// Shaded Text
// Villagers in the crossfire: Each US and ARVN Assault Province shifts
// by 1 level toward Active Opposition.
//
// Tips
// The unshaded effect changes US Assault only by allowing inclusion of 1
// Underground Guerrilla per space among the pieces to be removed (3.2.4) –
// it therefore adds to the total removed only if that number would otherwise
// be 0. The shaded effect applies to Provinces only, not Cities, and to US-only,
// ARVN-only, and US plus ARVN Assaults.

object Card_028 extends EventCard(28, "Search and Destroy",
  DualEvent,
  List(US, VC, ARVN, NVA),
  ListMap(US   -> (Critical  -> Unshaded),
          ARVN -> (Critical  -> Unshaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Critical  -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = playCapability(SearchAndDestroy_Unshaded)

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = playCapability(SearchAndDestroy_Shaded)
}
