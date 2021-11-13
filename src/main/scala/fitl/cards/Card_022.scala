
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
// US Marines arrive: US places up to 6 Troops in Da Nang,
// up to 3 from out of play.
//
// Shaded Text
// VC fire closes air base: Remove all Support within 1 space of Da Nang.
// No Air Strike until Coup.
// MOMENTUM
//
// Tips
// Removal of Support is instant and once; the ban on Air Strikes lasts
// until the next Coup Round (5.4).

object Card_022 extends EventCard(22, "Da Nang",
  DualEvent,
  List(US, VC, NVA, ARVN),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Critical    -> Shaded))) {
            
  def unshadedEffective(faction: Faction): Boolean = game.outOfPlay.has(USTroops)

  def executeUnshaded(faction: Faction): Unit = {
    val maxOop   = game.outOfPlay.totalOf(USTroops)
    val maxAvail = game.availablePieces.totalOf(USTroops)
    val (numOop, numAvail) = if (game.isHuman(US)) {
      val numOop   = askInt("Place how many OUT OF PLAY US Troops in Da Nang", 0, 3 min maxOop)
      val numAvail = askInt("Place how many AVAILABLE US Troops in Da Nang", 0, 6 - numOop min maxAvail)
      (numOop, numAvail)
    }
    else
      (maxOop min 3, 0) // US Bot will only place troops from Out of Play
    
    loggingControlChanges {
      placePiecesFromOutOfPlay(DaNang, Pieces(usTroops = numOop))
      placePieces(DaNang, Pieces(usTroops = numAvail))
    }
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    loggingPointsChanges {
      for (sp <- withOrAdjacent(DaNang)(sp => !sp.isLoC && sp.support > Neutral))
        setSupport(sp.name, Neutral)
    }
    log()
    playMomentum(Mo_DaNang)
  }
}
