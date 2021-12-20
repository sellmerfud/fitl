
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
// US justifies its war: Up to 3 US or 6 ARVN out-of-play pieces to Available.
// Or ARVN Resources and Aid each +9.
//
// Shaded Text
// US public doubts war’s purpose: 3 Available US Troops out of play. Aid –9.
//
// Tips
// "Pieces" can include Bases.

object Card_082 extends EventCard(82, "Domino Theory",
  DualEvent,
  List(ARVN, VC, US, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.outOfPlay.has(USPieces)

  def executeUnshaded(faction: Faction): Unit = {
    val oop = game.outOfPlay
    val canRes = game.trackResources(ARVN) && (game.arvnResources < EdgeTrackMax || game.usAid < EdgeTrackMax)
    if (game.isHuman(faction)) {
      val choices = List(
        choice(oop.has(USPieces),   "us-pieces",   "Move up to 3 Out Of Play US Pieces to Available"),
        choice(oop.has(ARVNPieces), "arvn-pieces", "Move up to 6 Out Of Play ARVN Pieces to Available"),
        choice(canRes,              "arvn-res",    "Add +9 ARVN resources and +9 US Aid")
      ).flatten
      val piecesPrompt = "\nMoving Out of Play pieces to Available"
      if (choices.isEmpty)
        log("The event has no effect")
      else {
        askMenu(choices, "\nChoose one:").head match {
          case "us-pieces" =>
            val num    = askInt("\nMove how many US Pieces", 0, oop.totalOf(USPieces) min 3)
            val pieces = askPieces(oop.only(USPieces), num, prompt = Some(piecesPrompt))
            println()
            moveOutOfPlayToAvailable(pieces)

          case "arvn-pieces" =>
            val num    = askInt("\nMove how many ARVN Pieces", 0, oop.totalOf(ARVNPieces) min 6)
            val pieces = askPieces(oop.only(ARVNPieces), num, prompt = Some(piecesPrompt))
            println()
            moveOutOfPlayToAvailable(pieces)

          case _ =>
            println()
            increaseResources(ARVN, 9)
            increaseUsAid(9)
        }
      }
    }
    else {
      // US Bot
      val pieces = Bot.selectFriendlyToPlaceOrMove(game.outOfPlay.only(USPieces), 3)
      moveOutOfPlayToAvailable(pieces)
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    game.outOfPlay.has(USTroops) ||
    (game.trackResources(ARVN) && game.usAid > 0)

  def executeShaded(faction: Faction): Unit = {
    val oopTroops = game.outOfPlay.only(USTroops)
    val pieces = if (oopTroops.isEmpty)
      Pieces()
    else if (game.isHuman(faction))
      askPieces(oopTroops, 3)
    else
      Bot.selectEnemyRemoveReplaceActivate(oopTroops, 3)

    println()
    loggingPointsChanges {
      moveAvailableToOutOfPlay(pieces)
      decreaseUsAid(9)      
    }
  }
}
