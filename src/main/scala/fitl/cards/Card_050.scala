
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
// Known communist: 4 out-of-play US Troops to South Vietnam, or 
// ARVN Resources +9. ARVN executes any 2 free Limited Operations.
//
// Shaded Text
// Revolutionary unifier: VC then NVA each execute any 3 free Lim- ited Operations.
//
// Tips
// The Faction executing the unshaded Event decides where the US Troops go.
// The Limited Operations can be the same or different, in any including the
// same spaces, and may be selected as they occur, so that results of previous
// choices may be taken into account.

object Card_050 extends EventCard(50, "Uncle Ho",
  DualEvent,
  List(NVA, ARVN, VC, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = {
    val numOopTroops = game.outOfPlay.totalOf(USTroops) min 4
    val validSpaces  = spaceNames(game.spaces) filter isInSouthVietnam

    if (game.isHuman(faction)) {
      val choices = List(
        choice(numOopTroops > 0, "troops",   s"Place ${amountOf(numOopTroops, "Out of Play US Troop")} in South Vietnam"),
        choice(true,             "resources", "Add +9 ARVN resources")
      ).flatten
      askMenu(choices, "\nChoose one:").head match {
        case "troops" =>
          loggingControlChanges {
            placeOutOfPlayPiecesOnMap(faction, 4, Set(USTroops), validSpaces)
          }

        case _ =>
          increaseResources(ARVN, 9)
      }
    }
    else {
      if (faction == ARVN && game.trackResources(ARVN))
        increaseResources(ARVN, 9)
      else if (numOopTroops > 0)
        loggingControlChanges {
          placeOutOfPlayPiecesOnMap(faction, 4, Set(USTroops), validSpaces)
        }

    }

    val params = Params(free = true, maxSpaces = Some(1))
    for (i <- 1 to 2) {
      log(s"\nARVN executes ${ordinal(i)} of 2 Limited Operations")
      if (game.isHuman(ARVN))
        Human.executeOp(ARVN, params)
      else
        Bot.executeOp(ARVN, params)
    }
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    val params = Params(free = true, maxSpaces = Some(1))

    for (f <- Seq(VC, NVA)) {
      for (i <- 1 to 3) {
        log(s"\n$f executes ${ordinal(i)} of 3 Limited Operations")
        if (game.isHuman(f))
          Human.executeOp(f, params)
        else
          Bot.executeOp(f, params)
      }
    }
  }
}
