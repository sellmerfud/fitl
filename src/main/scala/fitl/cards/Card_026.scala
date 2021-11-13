
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
// Long Range Recon Patrol: US places 3 Irregulars outside the South
// then free Air Strikes.
//
// Shaded Text
// Patrols ambushed: 3 Irregulars map to Casualties. Shift each space they
// were in 1 level toward Active Opposition.
//
// Tips
// "Irregulars" are US Special Forces pieces, not ARVN Rangers.
// "Outside the South" means any Laos or Cambodia Provinces.
// The free Air Strike otherwise follows the usual rule (4.2.3) so can be
// in any spaces with US/ARVN pieces (not only in those that just received
// Irregulars or outside the South), shifts Support/Opposition,
// and can Degrade the Trail.

object Card_026 extends EventCard(26, "LRRP",
  DualEvent,
  List(US, VC, ARVN, NVA),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  // COIN Bots never play this event.
  def unshadedEffective(faction: Faction): Boolean = false
    
  def executeUnshaded(faction: Faction): Unit = {
    //  Human only
    loggingControlChanges {
      if (game.piecesToPlace.has(Irregulars_U))
        placePiecesOnMap(US, 3, Set(Irregulars_U), LaosCambodia)
      else
        log("There are no Irregulars that can be placed.")
      Human.doAirStrike(Params(event = true, free = true))
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    game.totalOnMap(_.pieces.totalOf(Irregulars)) > 0

  def executeShaded(faction: Faction): Unit = {
    val numOnMap = game.totalOnMap(_.pieces.totalOf(Irregulars))
    def irregSpaces = game.spaces filter (_.pieces.has(Irregulars))

    // Save all control/scoring messages until the end.
    loggingControlChanges {
      val spacesUsed = if (numOnMap == 0) {
        log("There are no Irregulars on the map")
        Set.empty
      }
      else 
        removePiecesFromMap(faction, 3, Irregulars, false, spaceNames(game.spaces))

      for (name <- spacesUsed) {
        val sp = game.getSpace(name)
        if (!sp.isLoC && sp.support > ActiveOpposition)
          decreaseSupport(name, 1)
      }
    }
  }

}
