
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
// Census-grievance teams: Place 1 Police into each of 6 Provinces.
//
// Shaded Text
// Local Viet Minh tradition: Shift 3 Provinces with ARVN each 1 level
// toward Active Opposition. Place a VC Guerrilla in each.
//
// Tips
// A Province with 0 Population cannot be shifted from Neutral (1.6).

object Card_112 extends EventCard(112, "Colonel Chau",
  DualEvent,
  List(VC, ARVN, US, NVA),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.isProvince &&
    !sp.isNorthVietnam

  def pickUSSpace(candidates: List[Space]): Space = {
    val priorities = List(
      new Bot.BooleanPriority[Space]("With US Troops", _.pieces.has(USTroops)),
      new Bot.BooleanPriority[Space]("Without Police", !_.pieces.has(ARVNPolice)),
      new Bot.HighestScore[Space]("Most Pop.", _.population)
    )

    Bot.bestCandidate(candidates, priorities)
  }

  def unshadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(ARVNPolice)

  def humanPlacePolice(faction: Faction): Unit = {
    val numAvailPolice = game.availablePieces.totalOf(ARVNPolice) min 6
    val candidates     = spaceNames(game.nonLocSpaces filter unshadedCandidate)

    if (numAvailPolice == 6 || !isCoin(faction))  {
      val prompt = s"\nSelect ${amountOf(numAvailPolice, "Province")}:"
      val selectedSpaces = askSimpleMenu(candidates, prompt, numChoices = numAvailPolice)
      for (name <- selectedSpaces)
        placePieces(name, Pieces(arvnPolice = 1))
    }
    else {
      // There are less than 6 available Police.  A COIN player may
      // volunatarily remove Police from the map...

      def nextPolice(count: Int, candidates: List[String]): Unit = if (count <= 6) {
        val availPolice = game.availablePieces.has(ARVNPolice)
        val spaceChoices = candidates map (n => n -> n)
        val choices = if (availPolice)
          spaceChoices
        else
          spaceChoices :+ ("finished", "Finished placing Police")

        if (!availPolice)
          println("\nThere are no available Police. Placing Police will require volunatry removal")

        askMenu(choices, s"\nSelect a Province to place a ${ordinal(count)} Police cube:").head match {
          case "finished" =>
          case name =>
            if (!availPolice)
              voluntaryRemoval(1, ARVNPolice)
            println()
            placePieces(name, Pieces(arvnPolice = 1))
            nextPolice(count + 1, candidates filterNot (_ == name))
        }
      }

      nextPolice(1, candidates)
    }
  }


  def botPlacePolice(faction: Faction): Unit = {
    val candidates     = game.nonLocSpaces filter unshadedCandidate
    val numPolice      = game.availablePieces.totalOf(ARVNPolice) min 6
    val selector       = if (faction == US) pickUSSpace _ else ARVN_Bot.pickSpacePlaceCubesRangers _
    val selectedSpaces = Bot.pickSpaces(numPolice, candidates)(selector)

    for (sp <- selectedSpaces)
      placePieces(sp.name, Pieces(arvnPolice = 1))
  }

  def executeUnshaded(faction: Faction): Unit = {
    loggingControlChanges {
      if (game.isHuman(faction))
        humanPlacePolice(faction)
      else
        botPlacePolice(faction)
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(ARVNPieces)

  val shadedShiftCandidate = (sp: Space) =>
    shadedCandidate(sp) &&
    sp.canHaveSupport &&
    sp.support > ActiveOpposition

  def humanDoShaded(faction: Faction): Unit = {
    val candidates = spaceNames(game.nonLocSpaces filter shadedCandidate)

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val numAvailGs = game.availablePieces.totalOf(VCGuerrillas_U) min 3
      val numSpaces  = candidates.size min numAvailGs

      def nextSpace(count: Int, candidates: List[String]): Unit = if (count <= numSpaces) {
        val name = askSimpleMenu(candidates, s"\nSelect ${ordinal(count)} Province:").head
        val availG = game.availablePieces.has(VCGuerrillas_U)
        val num  = (availG, faction) match {
          case (true, _)   => 1
          case (false, VC) => Human.numToPlace(VCGuerrillas_U, 1) // Allow voluntary removal
          case (false, _)  => 0  // Other factions cannot voluntarily remove VC pieces
        }
          
        println()
        decreaseSupport(name, 1)
        placePieces(name, Pieces(vcGuerrillas_U = num))
        nextSpace(count + 1, candidates filterNot (_ == name))
      }

      nextSpace(1, candidates)
    }
  }
  
  def botDoShaded(): Unit = {
    val candidates     = game.nonLocSpaces filter shadedCandidate
    val numShiftSpaces = game.nonLocSpaces count shadedShiftCandidate
    val maxPlace       = candidates.size min game.availablePieces.totalOf(VCGuerrillas_U)
    val numSpaces      = (maxPlace max numShiftSpaces) min 3

    val selectedSpaces = Bot.pickSpaces(numSpaces, candidates)(VC_Bot.pickSpaceTowardActiveOpposition)

    for (sp <- selectedSpaces) {
      decreaseSupport(sp.name, 1)
      if (game.availablePieces.has(VCGuerrillas_U))
        placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    (game.nonLocSpaces exists shadedShiftCandidate) ||
    (game.availablePieces.has(VCGuerrillas_U) && (game.nonLocSpaces exists shadedCandidate))

  def executeShaded(faction: Faction): Unit = {
    val candiates = game.nonLocSpaces filter shadedCandidate
    loggingControlChanges {
      if (game.isHuman(faction))
        humanDoShaded(faction)
      else
        botDoShaded()
    }

  }
}
