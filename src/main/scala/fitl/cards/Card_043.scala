
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
// Free World aids Saigon: 2 ARVN or 2 US Bases out-of-play to Available.
// Then ARVN Resources +6 or Aid +12.
//
// Shaded Text
// Moscow aids Hanoi: Improve the Trail 1 box. 
// Then either Improve it 1 more box or add +10 NVA Resources.
//
// Tips
// Choose either 2 ARVN or 2 US Bases, not 1 ARVN Base and 1 US Base.

object Card_043 extends EventCard(43, "Economic Aid",
  DualEvent,
  List(NVA, ARVN, US, VC),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.outOfPlay.has(CoinBases) ||
    game.usAid < EdgeTrackMax     ||
    (faction == ARVN && game.trackResources(ARVN) && game.arvnResources < EdgeTrackMax)


  def executeUnshaded(faction: Faction): Unit = {
    val numUS   = game.outOfPlay.totalOf(USBase) min 2
    val numARVN = game.outOfPlay.totalOf(ARVNBase) min 2

    if (numUS == 0 && numARVN == 0)
      log("There are no Out of Play US or ARVN bases")
    else {
      val baseType = if (game.isHuman(faction)) {
        val choices = List(
          choice(numUS   > 0, USBase,   amountOf(numUS, "US Base")),
          choice(numARVN > 0, ARVNBase, amountOf(numARVN, "ARVN Base")),
        ).flatten
        val sorted = if (faction == US) choices else choices.reverse
        askMenu(choices, "\nChoose Out of Play Bases to move to Available:").head
      }
      else faction match {
        case US => if (numUS   > 0) USBase else ARVNBase
        case _  => if (numARVN > 0) USBase else ARVNBase
      }
      
      val bases = baseType match {
        case USBase => Pieces(usBases = numUS)
        case _      => Pieces(arvnBases = numARVN)
      }
      moveOutOfPlayToAvailable(bases)
    }

    if (game.isHuman(faction)) {
      val choices = List(
        "arvn" -> "Increase ARVN resources +6",
        "aid"  -> "Increase US Aid +12")
      askMenu(choices, "\nChoose one:").head match {
        case "arvn" =>
          log()
          increaseResources(ARVN, 6)
        case _      =>
          log()
          increaseUsAid(12)
      }
    }
    else if (faction == ARVN && game.trackResources(ARVN) && game.arvnResources < EdgeTrackMax)
      increaseResources(ARVN, 6)
    else
      increaseUsAid(12)
  }

  def shadedEffective(faction: Faction): Boolean = false  // Not executed by Bots
  def executeShaded(faction: Faction): Unit = {
    val choices = List(
      "trail-2" -> "Improve the Trail by 2 boxes",
      "trail-1" -> "Improve the Trail by 1 box and increase NVA resources +10")

    askMenu(choices, "\nChoose one:").head match {
      case "trail-2" =>
        log()
        improveTrail(2)
      case _         =>
        log()
        improveTrail(1)
        increaseResources(NVA, 10)
    }
  }
}
