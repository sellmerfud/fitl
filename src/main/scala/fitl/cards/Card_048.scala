
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

// Shaded Text
// CIDG camp holds out: Remove up to 3 Guerrillas from a Province
// with a COIN Base. Set the space to Active Support.
//
// Shaded Text
// Camp overrun: Remove a COIN Base from a Province with 0-2 COIN
// cubes (US to Casualties) and set it to Active Opposition.
//
// Tips "COIN" means US or ARVN. "US to Casualties" means that any
// US pieces removed go to the Casualties box, while ARVN piece removed
// go to ARVN Available Forces as usual (1.4.1).

object Card_048 extends EventCard(48, "Nam Dong",
  DualEvent,
  List(NVA, ARVN, VC, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(CoinBases) &&
    (sp.pieces.has(Guerrillas) || sp.support != ActiveSupport)

  def unshadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askCandidate("Execute the event in which space: ", spaceNames(candidates))
      else if (faction == US)
        US_Bot.pickSpaceTowardActiveSupport(candidates).name
      else
        ARVN_Bot.pickSpaceTowardPassiveSupport(candidates).name

      val sp = game.getSpace(name)
      val maxNum = sp.pieces.totalOf(Guerrillas) min 3
      val guerrillas = if (maxNum == 0)
        Pieces()
      else if (game.isHuman(faction)) {
        val num = askInt("Remove how many guerrillas", 0, maxNum)
        askPieces(sp.pieces.only(Guerrillas), num)
      }
      else
        Bot.selectEnemyRemovePlaceActivate(sp.pieces.only(Guerrillas), maxNum)

      log()
      loggingControlChanges {
        removePieces(name, guerrillas)
        setSupport(name, ActiveSupport)
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(CoinBases) &&
    sp.pieces.totalOf(CoinCubes) < 3

  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val (name, base) = if (game.isHuman(faction)) {
        val name = askCandidate("Execute the event in which space: ", spaceNames(candidates))
        val base = askPieces(game.getSpace(name).pieces.only(CoinBases), 1)
        (name, base)
      }
      else {
        val sp = VC_Bot.pickSpaceTowardActiveOpposition(candidates)
        val base = Bot.selectEnemyRemovePlaceActivate(sp.pieces.only(CoinBases), 1)
        (sp.name, base)
      }

      log()
      loggingControlChanges {
        removePieces(name, base)
        setSupport(name, ActiveOpposition)
      }
    }
  }
}
