
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
// Ambassador proposes US protectorate: Aid +20.
//
// Shaded Text
// Internecine enabler: Remove up to 3 ARVN pieces. Patronage +2 for each. ARVN Ineligible through next card.
//
// Tips
// "Pieces" can include Bases.

object Card_079 extends EventCard(79, "Henry Cabot Lodge",
  DualEvent,
  List(ARVN, NVA, VC, US),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Critical    -> Shaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.usAid < EdgeTrackMax

  def executeUnshaded(faction: Faction): Unit = increaseUsAid(20)

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    val maxRemove = game.totalOnMap(_.pieces.totalOf(ARVNPieces)) min 3
    val num = if (game.isHuman(faction))
      askInt("\nRemove how many ARVN pieces", 0, maxRemove)
    else if (faction == ARVN)
      maxRemove
    else  // NVA and VC
      (((49 - game.arvnPoints) / 2) max 0) min maxRemove

    log(s"\n$faction chooses to remove ${amountOf(num, "ARVN piece")}")
    log()
    loggingControlChanges {
      removePiecesFromMap(faction, num, ARVNPieces, friendly = faction == ARVN, spaceNames(game.spaces))
      increasePatronage(num * 2)
      makeIneligibleThroughNextTurn(ARVN, faction == ARVN)
    }

  }
}
