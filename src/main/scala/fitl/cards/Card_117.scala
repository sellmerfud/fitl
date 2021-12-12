
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
// ARVN general takes initiative: ARVN places 3 of its Troops from out of play or
// Available into 1 or 2 adjacent spaces then free Sweeps each.
//
// Shaded Text
// Corps CO ignores Saigon: Remove a die roll of ARVN pieces from 1 or 2 adjacent
// spaces. ARVN Ineligible through next card.
//
// Tips
// For the unshaded text, the ARVN Faction decides from and to where to place the Troops.

object Card_117 extends EventCard(117, "Corps Commanders",
  DualEvent,
  List(VC, ARVN, NVA, US),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  def humanUnshaded(): Unit = {
    val candidates = spaceNames(game.spaces filterNot (_.isNorthVietnam))
    val name1    = askCandidate("\nSelect a space to place ARVN Troops: ", candidates)
    val adj      = getAdjacent(name1).toList.sorted(SpaceNameOrdering)
    val choices  = (adj map (n => n -> n)) :+ ("none" -> "Do not select an adjacent space")
    val selected = askMenu(choices, s"\nSelect a space adjacent to $name1:").head match {
      case "none" => Set(name1)
      case name2  => Set(name1, name2)
    }

    val maxOop   = game.outOfPlay.totalOf(ARVNTroops) min 3
    val numOop   = if (maxOop > 0) askInt("Place how many Troops from OUT OF PLAY", 0, maxOop) else 0
    val maxAvail = game.piecesToPlace.totalOf(ARVNTroops) -
                   (selected map (n => game.getSpace(n).pieces.totalOf(ARVNTroops))).sum
    val numAvail = (3 - numOop) min maxAvail

    println()
    if (numOop > 0 && selected.size == 1)
      placePiecesFromOutOfPlay(selected.head, Pieces(arvnTroops = numOop))
    else
      placeOutOfPlayPiecesOnMap(ARVN, numOop, Set(ARVNTroops), selected)

    println()
    if (numAvail > 0 && selected.size == 1 && game.availablePieces.totalOf(ARVNTroops) >= numAvail)
      placePieces(selected.head, Pieces(arvnTroops = numAvail))
    else
      placePiecesOnMap(ARVN, numAvail, Set(ARVNTroops), selected)
    println()
    val params = Params(event = true, free = true, sweep = SweepParams(explicitSpaces = selected))
    Human.executeSweep(ARVN, params)
  }

  def botUnshaded(): Unit = {
    var selected = Set.empty[String]
    
    def nextPlacement(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = if (selected.size == 2)
        spaces(selected)
      else if (selected.size == 1)
        spaces(getAdjacent(selected.head) + selected.head)
      else
        game.spaces filterNot (_.isNorthVietnam)

      val sp = ARVN_Bot.pickSpacePlaceCubesRangers(candidates)
      if (game.outOfPlay.has(ARVNTroops))
        placePiecesFromOutOfPlay(sp.name, Pieces(arvnTroops = 1))
      else
        placePieces(sp.name, Pieces(arvnTroops = 1))
      selected += sp.name
      nextPlacement(numRemaining - 1)
    }

    val numTroops = (game.outOfPlay.totalOf(ARVNTroops) + game.availablePieces.totalOf(ARVNTroops)) min 3
    nextPlacement(numTroops)

    val params = Params(event = true, free = true, onlyIn = Some(selected))
    ARVN_Bot.sweepOp(params, 6)
  }

  def unshadedEffective(faction: Faction): Boolean =
    game.outOfPlay.has(ARVNTroops) ||
    game.availablePieces.has(ARVNTroops)

  def executeUnshaded(faction: Faction): Unit = {
    loggingControlChanges {
      if (game.isHuman(ARVN))
        humanUnshaded()
      else
        botUnshaded()
    }
  }

  val hasArvn = (sp: Space) => sp.pieces.has(ARVNPieces)

  def humanShaded(faction: Faction, numToRemove: Int): Unit = {
    val candidates = spaceNames(game.spaces filter hasArvn)
    if (candidates.isEmpty)
      log("There are no ARVN Pieces on the map")
    else {
      val name1    = askCandidate("\nSelect a space with ARVN Pieces: ", candidates)
      val adj      = spaceNames(spaces(getAdjacent(name1)) filter hasArvn).toList.sorted(SpaceNameOrdering)
      val selected = if (adj.isEmpty)
        Set(name1)
      else {
        val choices  = (adj map (n => n -> n)) :+ ("none" -> "Do not select an adjacent space")
        askMenu(choices, s"\nSelect a space with ARVN Pieces adjacent to $name1:").head match {
          case "none" => Set(name1)
          case name2  => Set(name1, name2)
        }
      }

      val numInSelected = (selected.toList map (n => game.getSpace(n).pieces.totalOf(ARVNPieces))).sum
      val num = numInSelected min numToRemove
      println()
      removePiecesFromMap(faction, num, ARVNPieces, false, selected)
    }
  }

  def botShaded(numToRemove: Int): Unit = {
    var selected = Set.empty[String]

    def nextRemoval(numRemaining: Int): Unit = {
      val candidates = if (selected.size == 2)
        spaces(selected) filter hasArvn
      else if (selected.size == 1)
        spaces(getAdjacent(selected.head) + selected.head) filter hasArvn
      else
        game.spaces filter hasArvn

      if (numRemaining >  0 && candidates.nonEmpty) {
        val sp       = NVA_Bot.pickSpaceRemoveReplace(candidates)
        val toRemove = Bot.selectEnemyRemoveReplaceActivate(sp.pieces.only(ARVNPieces), 1)
        removePieces(sp.name, toRemove)
        selected += sp.name
        nextRemoval(numRemaining - 1)
      }
    }

    nextRemoval(numToRemove)
  }


  def shadedEffective(faction: Faction): Boolean =
    game.totalOnMap(_.pieces.totalOf(ARVNPieces)) > 0

  def executeShaded(faction: Faction): Unit = {
    val die = d6

    log(s"\nRolling d6 to determine number of ARVN pieces to remove: $die")
    log(separator())

    loggingControlChanges {
      if (game.isHuman(faction))
        humanShaded(faction, die)
      else
        botShaded(die)
    }
    log()
    makeIneligibleThroughNextTurn(ARVN)
  }
}
