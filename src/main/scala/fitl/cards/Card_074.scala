
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
import scala.util.Random.shuffle
import fitl.FireInTheLake._
import fitl.EventHelpers._
import fitl.Bot
import fitl.Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }
import fitl.Human

// Unshaded Text
// Sudden incursion: Place up to 6 ARVN Troops in a Laos space.
// ARVN executes a free LimOp there. Degrade Trail 2 boxes.
//
// Shaded Text
// Southern escalation: NVA Resources +6 and +1 more for
// each ARVN piece in Laos.
//
// Tips
// ARVN decides the details of the LimOp.

object Card_074 extends EventCard(74, "Lam Son 719",
  DualEvent,
  List(ARVN, NVA, US, VC),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Ignored -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(ARVNTroops) ||
    game.trail > TrailMin

  def executeUnshaded(faction: Faction): Unit = {
    val availTroops = game.availablePieces.totalOf(ARVNTroops)

    val  name = if (game.isHuman(faction)) {
      val maxTroops = game.piecesToPlace.totalOf(ARVNTroops)
      val num = askInt("\nPlace how many ARVN Troops in a Laos space", 0 , maxTroops min 6)
      val finalNum = Human.numToPlace(ARVNTroops, num)
      val choices = Laos.sorted(SpaceNameOrdering)

      if (finalNum > 0) {
        val name = askSimpleMenu(choices, "\nPlace ARVN Troops in which space:").head
        placePieces(name, Pieces().set(finalNum, ARVNTroops))
        name
      }
      else
        askSimpleMenu(choices, "\nPerform ARVN Limited Op in which space:").head
    }
    else if (availTroops > 0) {
      val name = NVA_Bot.pickSpacePlaceTroops(spaces(Laos)).name
      val num  = availTroops min 6
      placePieces(name, Pieces().set(num, ARVNTroops))
      name
    }
    else
      shuffle(Laos).head

    val params = Params(event = true, free = true, onlyIn = Some(Set(name)), maxSpaces = Some(1))
    if (game.isHuman(ARVN))
      Human.executeCoinOp(ARVN, params)
    else {
      // The Trung cards would never resolve an op in Laos
      // so we will attempt to Assault in the space.
      val sp = game.getSpace(name)
      if (assaultEffective(ARVN, NormalTroops, false, false)(sp))
        Bot.performAssault(ARVN, name, params)
      else
        log(s"\nARVN chooses to not perform a Limited Op in $name")
    }
    log()
    degradeTrail(2)
  }

  def shadedEffective(faction: Faction): Boolean = false  // Not executed by Bots.

  def executeShaded(faction: Faction): Unit = {
    val arvnInLaos = game.totalOnMap { sp =>
      if (isInLaos(sp.name))
        sp.pieces.totalOf(ARVNPieces)
      else
        0
    }

    increaseResources(NVA, 6 + arvnInLaos)
  }
}
