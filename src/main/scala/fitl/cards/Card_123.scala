
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

// ARVN Pivotal event
//
// Play if 2+ cards in RVN Leader box and <20 US Troops on map.
// 
// Mechanization: +12 ARVN Resources. +12 Aid.
// All out-of-play ARVN Available. Place 4 ARVN cubes anywhere.
//
// Tips
// Do not count Minh as a card for the precondition.
// "On map" means in South Vietnam, Laos, and Cambodia combined.

object Card_123 extends EventCard(123, "Vietnamization",
  SingleEvent,
  List(ARVN, US, NVA, VC),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (NotExecuted -> Unshaded),
          VC   -> (NotExecuted -> Unshaded))) {

  // Is VC pivotal event playable?
  def unshadedEffective(faction: Faction): Boolean =
    game.numCardsInLeaderBox >= 2 && game.totalOnMap(_.pieces.totalOf(USTroops)) < 20

  def executeUnshaded(faction: Faction): Unit = {
    log("\n+12 ARVN Resources and +12 US Aid")
    log(separator(char = '='))
    if (game.trackResources(ARVN)) {
      increaseResources(ARVN, 12)
      increaseUsAid(12)
    }
    else
      log("ARVN resources and US Aid are not being used used")

    log("\nAll Out of Play ARVN pieces to Available")
    log(separator(char = '='))
    if (game.outOfPlay.has(ARVNPieces))
      moveOutOfPlayToAvailable(game.outOfPlay.only(ARVNPieces))
    else
      log("There are no Out of Play ARVN pieces")

    log("\nPlace 4 ARVN Cubes anywhere")
    log(separator(char = '='))
    val numCubes = if (game.isHuman(ARVN)) 4 else game.availablePieces.totalOf(ARVNCubes) min 4
    if (numCubes == 0)
      log("The ARVN Bot places no cubes because there are none available.")
    else
      placePiecesOnMap(ARVN, numCubes, ARVNCubes, spaceNames(game.spaces filterNot (_.isNorthVietnam)))
  }

  // Shaded functions not used for Pivotal Event  
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
