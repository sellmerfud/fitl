
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
// Incident and resolution: US free Air Strikes, then moves 6 US pieces
// from out-of-play to any Cities.
//
// Shaded Text
// Congressional regrets: Aid –1 per Casualty. All Casualties out of play.
//
// Tips
// The free Air Strike otherwise follows the usual rule (4.2.3), so it can be
// in any spaces with US/ARVN pieces, it shifts Support/Opposition, and it
// can Degrade the Trail. “Pieces” include Bases (1.4).

object Card_001 extends EventCard(1, "Gulf of Tonkin",
  DualEvent,
  List(US, ARVN, NVA, VC),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean =
    airStrikeEffective || game.outOfPlay.has(USPieces)

  def executeUnshaded(faction: Faction): Unit = {
    val numCasualties = game.casualties.totalOf(USPieces) min 6
    if (game.isHuman(US)) {
      loggingControlChanges {
        Human.doAirStrike(Params(event = true))
        Human.moveUSOutOfPlayToCities(6)
      }
    }
    else {
      loggingControlChanges {
        US_Bot.airStrikeActivity(Params(event = true))
        US_Bot.moveOutOfPlayToCities(6)
      }
    }
  }


  def shadedEffective(faction: Faction): Boolean = 
    isInsurgent(faction) && game.casualties.nonEmpty

  def executeShaded(faction: Faction): Unit = {
    // Aid -1 per Casualty.  All Casualties out of play.
    decreaseUsAid(game.casualties.total)
    moveAllCasualtiesToOutOfPlay(game.casualties)
  }
}
