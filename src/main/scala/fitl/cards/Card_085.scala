
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
// Increased help to civilians: Shift 3 COIN-Controlled spaces each
// 1 level toward Active Support.
//
// Shaded Text
// More aid, more corruption: Increase or decrease any or all of ARVN
// Resources, Aid, and Patronage by 2 each.
//
// Tips
// A Province with 0 Population cannot shift (1.6). The shaded effect may
// increase some and decrease other totals and may thus be useful for any Faction.

object Card_085 extends EventCard(85, "USAID",
  DualEvent,
  List(ARVN, VC, US, NVA),
  ListMap(US   -> (Critical  -> Unshaded),
          ARVN -> (Critical  -> Shaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Performed -> Shaded))) {


  val unshadedCandidate = (sp: Space) =>
    sp.coinControlled &&
    sp.population > 0 &&
    sp.support < ActiveSupport

  def unshadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 3)
      spaceNames(candidates)
    else if (game.isHuman(faction))
      askSimpleMenu(spaceNames(candidates), "\nSelect 3 COIN controlled spaces:", numChoices = 3)
    else 
      spaceNames(Bot.pickSpaces(3, candidates)(US_Bot.pickSpaceTowardActiveSupport))

    if (selectedSpaces.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      println()
      loggingPointsChanges {
        for (name <- selectedSpaces)
          increaseSupport(name, 1)
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = faction match {
    case ARVN => game.patronage < EdgeTrackMax
    case _    => game.patronage > 0 || (game.trackResources(ARVN) && 
                                        (game.arvnResources > 0 || game.usAid > 0))
  }

  def executeShaded(faction: Faction): Unit = {
    val (resAction, aidAction, patAction) = if (game.isHuman(faction)) {
      val choices = List(
        "increase"  -> "Increase by +2",
        "decrease"  -> "Decrease by -2",
        "no-change" -> "No change"
      )

      val r = askMenu(choices, "\nChoose action for ARVN resources:").head
      val a = askMenu(choices, "\nChoose action for US Aid:").head
      val p = askMenu(choices, "\nChoose action for Patronage:").head
      println()
      (r, a, p) 
    }
    else if (faction == ARVN)
      ("increase", "increase", "increase") 
    else
      ("decrease", "decrease", "decrease") 

    resAction match {
      case "increase" => increaseResources(ARVN, 2)
      case "decrease" => decreaseResources(ARVN, 2)
      case _ =>
    }
    aidAction match {
      case "increase" => increaseUsAid(2)
      case "decrease" => decreaseUsAid(2)
      case _ =>
    }
    patAction match {
      case "increase" => increasePatronage(2)
      case "decrease" => decreasePatronage(2)
      case _ =>
    }
  }
}
