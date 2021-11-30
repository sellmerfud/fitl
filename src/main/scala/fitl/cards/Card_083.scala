
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
// Clean vote: 3 Passive Support spaces to Active Support. Aid +10.
//
// Shaded Text
// Ballot stuffing defeats opposition candidate Druong Dinh Dzu:
// Shift 2 Cities each 1 level toward Active Opposition. Aid –15.
//
// Tips
// The spaces affected may include Saigon.

object Card_083 extends EventCard(83, "Election",
  DualEvent,
  List(ARVN, VC, US, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.canHaveSupport &&
    sp.support == PassiveSupport

  def unshadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists unshadedCandidate
  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 3)
      spaceNames(candidates)
    else if (game.isHuman(faction)) {
      val prompt = s"\nSelect 3 spaces to set to Active Support:"
      askSimpleMenu(spaceNames(candidates), prompt, numChoices = 3)
    }
    else
      spaceNames(Bot.pickSpaces(3, candidates)(US_Bot.pickSpaceTowardActiveSupport))

    loggingPointsChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces that qualify for the event")
      else {
        println()
        for (name <- selectedSpaces)
          setSupport(name, ActiveSupport)
        log()
        increaseUsAid(9)
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isCity &&
    sp.support > ActiveOpposition

  def shadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate
    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 2)
      spaceNames(candidates)
    else if (game.isHuman(faction)) {
      val prompt = "\nSelect 2 cities:"
      askSimpleMenu(spaceNames(candidates), prompt, numChoices = 2)
    }
    else
      spaceNames(Bot.pickSpaces(2, candidates)(VC_Bot.pickSpaceTowardActiveOpposition))

    loggingPointsChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces that qualify for the event")
      else {
        println()
        for (name <- selectedSpaces)
          decreaseSupport(name, 1)
          log()
          decreaseUsAid(15)
      }
    }
  }
}
