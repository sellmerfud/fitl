
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
// Onerous VC Taxation: Shift 4 Provinces with any VC each
// by 1 level toward Active Support.
//
// Shaded Text
// Local government corruption: Shift 3 Provinces with Police each
// by 1 level toward Active Opposition. Patronage +6 or –6.
//
// Tips
// A Province with 0 Population cannot be shifted from Neutral (1.6).

object Card_105 extends EventCard(105, "Rural Pressure",
  DualEvent,
  List(VC, NVA, US, ARVN),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Critical    -> Shaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  val unshadedCandidate = (sp: Space) =>
    sp.isProvince           &&
    sp.pieces.has(VCPieces) &&
    sp.support < ActiveSupport

  def unshadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates     = game.nonLocSpaces filter unshadedCandidate
    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 4)
      spaceNames(candidates)
    else if (game.isHuman(faction))
      askSimpleMenu(spaceNames(candidates), "\nChoose 4 Provinces with VC pieces:", numChoices = 4)
    else
      Bot.pickSpaces(4, candidates)(US_Bot.pickSpaceTowardActiveSupport) map (_.name)

    loggingPointsChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces that qualify for the event")
      else {
        println()
        for (name <- selectedSpaces)
          increaseSupport(name, 1)
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince             &&
    sp.pieces.has(ARVNPolice) &&
    sp.support > ActiveOpposition

  def shadedEffective(faction: Faction): Boolean = faction match {
    case ARVN => game.patronage < EdgeTrackMax
    case _    => game.nonLocSpaces exists shadedCandidate
  }

  // Event instructions for ARVN:
  // Shift 1-Pop spaces at Active Support first, the Neutral
  def arvnPickSpace(candidates: List[Space]): Space = {

    val priorities = List(
      new Bot.BooleanPriority[Space]("1-Pop, Active Support", sp => sp.population == 1 && sp.support == ActiveSupport),
      new Bot.BooleanPriority[Space]("1-Pop, Neutral", sp => sp.population == 1 && sp.support == Neutral),
      new Bot.BooleanPriority[Space]("1-Pop", sp => sp.population == 1)
    )

    Bot.botDebug(s"\nUS Select space (Shift Toward Active Support): [${andList(candidates.sorted)}]")
    Bot.botDebug(separator(char = '#'))
    Bot.bestCandidate(candidates, priorities)
  }


  def executeShaded(faction: Faction): Unit = {
    val candidates     = game.nonLocSpaces filter shadedCandidate
    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 3)
      spaceNames(candidates)
    else if (game.isHuman(faction))
      askSimpleMenu(spaceNames(candidates), "\nChoose 3 Provinces with Police:", numChoices = 3)
    else if (faction == ARVN)
      Bot.pickSpaces(3, candidates)(arvnPickSpace) map (_.name)
      else
      Bot.pickSpaces(3, candidates)(VC_Bot.pickSpaceTowardActiveOpposition) map (_.name)

    val addPatronage = if (game.isHuman(faction)) {
      val choices = List(true -> "Increase Patronage +6", false -> "Decrease Patronage -6")
       askMenu(choices, "\nChoose one:").head
    }
    else
      faction == ARVN


    loggingPointsChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces that qualify for the event")
      else {
        println()
        for (name <- selectedSpaces)
          decreaseSupport(name, 1)
      }

      log()
      if (addPatronage)
        increasePatronage(6)
      else
        decreasePatronage(6)
    }
  }
}
