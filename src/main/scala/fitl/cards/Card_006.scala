
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
// Robin Olds ambushes MiGs: Free Air Strike any 1 space outside the
// South with 6 hits and Degrade Trail 2 boxes.
//
// Shaded Text
// MiG ace "Colonel Tomb": 2 Available US Troops to Casualties.
// Improve Trail by 2 boxes.
//
// Tips
// "Any 1 space outside the South" means any 1 North Vietnam, Laos, or
// Cambodia Province, even if no US or ARVN piece there.
// Degrade of the Trail is 2 boxes total, instead of the usual.

object Card_006 extends EventCard(6, "Aces",
  DualEvent,
  List(US, NVA, VC, ARVN),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = {
    val validSpaces = spaces(NorthVietnam::LaosCambodia)
    val canStrike = validSpaces exists (sp => vulnerableInsurgents(sp.pieces, false).nonEmpty)
    canStrike || game.trail > TrailMin
  }

  def executeUnshaded(faction: Faction): Unit = {
    val validNames = NorthVietnam::LaosCambodia
    val params = Params(
      event     = true,
      maxSpaces = Some(1),
      airstrike = AirStrikeParams(maxHits = Some(6), noCoin = true, canDegradeTrail = false),
      onlyIn    = Some(validNames.toSet)
    )

    loggingControlChanges {
      if (game.isHuman(US))
        Human.doAirStrike(params)
      else
        US_Bot.airStrikeActivity(params)
    }
    degradeTrail(2)
  }


  def shadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(USTroops) || game.trail < TrailMax

  def executeShaded(faction: Faction): Unit = {
    val numTroops = game.availablePieces.totalOf(USTroops) min 2
    moveAvailableToCasualties(Pieces(usTroops = numTroops))
    improveTrail(2)
  }
}
