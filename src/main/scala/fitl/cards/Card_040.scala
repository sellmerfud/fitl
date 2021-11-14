
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
// Release negotiations keep US at war:
// Free Air Strike.  2 US Troops from Casualties to Available.
//
// Shaded Text
// Air campaign creates hostages: 3 US Troops from Available to Casualties.
//
// Tips
// Whichever Faction executes the unshaded Event decides the specifics of the
// Air Strike Special Activity (spaces targeted, whether the Trail is Degraded, etc.).

object Card_040 extends EventCard(40, "PoWs",
  DualEvent,
  List(NVA, US, VC, ARVN),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    airStrikeEffective || game.casualties.has(USTroops)

  def executeUnshaded(faction: Faction): Unit = {
    loggingControlChanges {
      if (game.isHuman(faction))
        Human.doAirStrike(Params(event = true, free = true))
      else
        US_Bot.airStrikeActivity(Params(event = true, free = true))

      val num = game.casualties.totalOf(USTroops) min 2
      if (num == 0)
        log("\nThere are no US Troop casualties")
      else
        moveCasualtiesToAvailable(Pieces(usTroops = num))
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(USTroops)

  def executeShaded(faction: Faction): Unit = {
    val num = game.availablePieces.totalOf(USTroops) min 3
    if (num == 0)
      log("There are no available US Troops")
    else
      moveAvailableToCasualties(Pieces(usTroops = num))
  }
}
