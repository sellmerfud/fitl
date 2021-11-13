
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

// NVA Pivotal event
//
// Play if 2+ cards in RVN Leader box and more NVA Troops than US Troops on map.
//
// Invasion: NVA free Marches. Then NVA Troops on LoCs with no US/ARVN may move 1 space.
// Then all NVA Troops free Attack.

object Card_122 extends EventCard(122, "Easter Offensive",
  SingleEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Critical    -> Unshaded),
          VC   -> (NotExecuted -> Unshaded))) {

  // Is NVA pivotal event playable?
  def unshadedEffective(faction: Faction): Boolean =
    game.numCardsInLeaderBox >= 2 &&
    game.totalOnMap(_.pieces.totalOf(NVATroops)) > game.totalOnMap(_.pieces.totalOf(USTroops))

  def executeUnshaded(faction: Faction): Unit = pivotalNotYet(NVA)

  // Shaded functions not used for Pivotal Event  
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
