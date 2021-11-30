
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
// Dissident becomes RVN minister: Shift Saigon 1 level toward
// Active Support. Patronage +5.
//
// Shaded Text
// Oppositionist Assemblyman: Shift Saigon 1 level toward Neutral. Patronage –5.
// ARVN Ineligible through next card.
//
// Tips
// Adjust the US victory marker (Support + Available) when changing Support and
// the ARVN victory marker (COIN Control + Patronage) when increasing or decreasing
// Patronage.

object Card_088 extends EventCard(88, "Phan Quang Dan",
  DualEvent,
  List(ARVN, VC, NVA, US),
  ListMap(US   -> (Critical  -> Unshaded),
          ARVN -> (Critical  -> Unshaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Performed -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = faction match {
    case US => game.getSpace(Saigon).support != ActiveSupport
    case _  => game.patronage < EdgeTrackMax
  }

  def executeUnshaded(faction: Faction): Unit = {
    loggingPointsChanges {
      increaseSupport(Saigon, 1)
      increasePatronage(5)
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    game.getSpace(Saigon).support != Neutral ||
    game.patronage > 0 ||
    game.sequence.willBeEligibeNextTurn(ARVN)

  def executeShaded(faction: Faction): Unit = {
    loggingPointsChanges {
      if (game.getSpace(Saigon).support < Neutral)
        increaseSupport(Saigon, 1)
      else if (game.getSpace(Saigon).support > Neutral)
        decreaseSupport(Saigon, 1)
      decreasePatronage(5)
    }
    log()
    makeIneligibleThroughNextTurn(ARVN)
  }
}
