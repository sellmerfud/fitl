
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
// Tough terrain: Degrade the Trail by 2 boxes.
// Until Coup, Infiltrate is max 1 space.
// MOMENTUM
//
// Shaded Text
// The way through: NVA free Infiltrate.
// Then NVA add 3 times and VC 2 times Trail value in Resources.
//
// Tips
// The Trail Degrades at once; the limit on Infiltrate lasts until the next Coup Round

object Card_046 extends EventCard(46, "559th Transport Grp",
  DualEvent,
  List(NVA, ARVN, VC, US),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Ignored -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = {
    if (game.trail > TrailMin)
      degradeTrail(2)
    playMomentum(Mo_559TransportGrp)
  }

  def shadedEffective(faction: Faction): Boolean = false // Bot never executes

  def executeShaded(faction: Faction): Unit = {
    val params = Params(event = true, free = true)

    log("\nNVA performs free Infiltrate activity")
    log(separator(char = '='))
    Human.doInfiltrate(params)
    log()
    increaseResources(NVA, game.trail * 3)
    increaseResources(VC, game.trail * 2)
  }
}
