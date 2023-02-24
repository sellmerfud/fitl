
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
// Soviet escalation matched: Place any 6 ARVN pieces anywhere in South Vietnam.
//
// Shaded Text
// Heavy divisions, big guns: NVA in any 3 spaces places enough Troops
// to double their number. It then free Bombards.
//
// Tips "Pieces" include Bases. For the unshaded Event, the executing
// Faction would decide the pieces and locations. For the shaded,
// NVA decides; in each selected space, place the same number of NVA
// Troops as are already there, then execute a Bombard Special Activity.
// The Bombard need not involve any of the Troops just placed.

object Card_049 extends EventCard(49, "Russian Arms",
  DualEvent,
  List(NVA, ARVN, VC, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Ignored -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.availablePieces.has(ARVNPieces)

  def executeUnshaded(faction: Faction): Unit = {
    val validSpaces = spaceNames(game.spaces filter (sp => isInSouthVietnam(sp.name)))
    placePiecesOnMap(faction, 6, ARVNPieces, validSpaces)
  }

  def shadedEffective(faction: Faction): Boolean = false  // Never executed by Bots

  // Human player only
  def executeShaded(faction: Faction): Unit = {
    val candidates = spaceNames(game.spaces filter (_.pieces.has(NVATroops)))
    val choices = candidates map (n => n -> n)
    val num = choices.size min 3
    val selectedSpaces = askMenu(choices, s"\nSelect ${amountOf(num, "space")} to double the amount of NVA Troops", numChoices = num)

    println()
    if (selectedSpaces.isEmpty)
      log("There are no spaces with NVA Troops")
    else {
      loggingControlChanges {
        for (name <- selectedSpaces) {
          val sp = game.getSpace(name)
          val numInSpace = sp.pieces.totalOf(NVATroops)
          val num        = Human.numToPlace(NVATroops, numInSpace)
          placePieces(name, Pieces(nvaTroops = num))
        }
      }
    }

    log()
    Human.doBombard(Params(event = true, free = true))
  }
}
