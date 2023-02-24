
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
// Dustoff: This Commitment, all Troop Casualties Available (mark).
// MOMENTUM
//
// Shaded Text
// Hueys diverted: Executing Faction remains Eligible. 
// Until Coup, no Air Lift (mark).
// MOMENTUM
//
// Tips
// Place the "Medevac" marker on the corresponding side onto the set-aside
// Momentum card as a reminder of which text was executed. For unshaded, upon
// the coming Commitment Phase, immediately move all US Troops that are in the
// Casualties box to Available; no Troops go out of play; Base Casualties still do.
// The Casualties will already have reduced Aid in the preceding Resources Phase (6.2.5, 6.5).
// If unshaded Event 73 "Great Society" occurs while unshaded "Medevac" is in effect,
// "Medevac" affects the immediate Commitment Phase and then stays in effect to affect
// the coming Coup Round’s Commitment Phase as well. Note also that unshaded "Medevac"
// played during the final campaign would have no effect unless unshaded "Great Society"
// subsequently occurs, because there otherwise will be no further Commitment Phase (6.4.5, 6.5).

object Card_015 extends EventCard(15, "Medevac",
  DualEvent,
  List(US, ARVN, NVA, VC),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = {
    playMomentum(Mo_Medevac_Unshaded)
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    remainEligibleNextTurn(faction)
    playMomentum(Mo_Medevac_Shaded)
  }
}
