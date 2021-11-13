
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
// 9th Division: US moves up to 3 US Troops from out of play to Available
// or South Vietnam, or from the map to Available.
//
// Shaded Text
// Worn out formation: US takes 3 of its Troops from the map out of play.
//
// Tips
// Adjust "Support+Available" score marker when US Troops or
// Bases move in or out of Available (7.2).

object Card_009 extends EventCard(9, "Psychedelic Cookie", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = {
    // For Nixon policy, US bot will move troops to available (from out of play first)
    // For LBJ and JFK it will move troops to S. Vietnam
    if (game.usPolicy == USPolicy_Nixon)
      game.outOfPlay.has(USTroops) || game.totalOnMap(_.pieces.totalOf(USTroops)) > 0
    else
      game.outOfPlay.has(USTroops)
  }

  def executeUnshaded(faction: Faction): Unit = {
    val numOutOfPlay = game.outOfPlay.totalOf(USTroops) min 3
    val numOnMap     = game.totalOnMap(_.pieces.totalOf(USTroops)) min 3
    if (game.isHuman(US)) {
      def outOfPlayToAvailableOrMap(numRemaining: Int): Unit = if (numRemaining > 0) {
        val choices = List(
          "available" -> "Move troops from Out of play to Available",
          "map"       -> "Move troops form Out of play to South Vietnam",
          "finished"  -> "Finished moving troops from Out of play"
        )
        println(s"\n${numOnMap - numRemaining} of $numOnMap troops moved from Out of play")
        askMenu(choices, "\nChoose one:").head match {
          case "available" =>
            val num = askInt("Move how many troops to Available", 0, numRemaining)
            moveOutOfPlayToAvailable(Pieces(usTroops = num))
            outOfPlayToAvailableOrMap(numRemaining - num)

          case "map" =>
            val candidates = spaceNames(game.spaces filter (sp => isInSouthVietnam(sp.name)))
            val name = askCandidate("\nMove troops to which space: ", candidates)
            val num = askInt(s"Move how many troops to $name", 0, numRemaining)
            moveOutOfPlayToMap(Pieces(usTroops = num), name)
            outOfPlayToAvailableOrMap(numRemaining - num)
            
          case _ =>
        }
      }

      def mapToAvailable(numRemaining: Int): Unit = if (numRemaining > 0) {
        val candidates = spaceNames(game.spaces filter (_.pieces.has(USTroops)))
        if (candidates.nonEmpty) {
          println(s"\n${numOnMap - numRemaining} of $numOnMap troops removed to Available")
          if (askYorN("\nDo you wish to remove troops to Available? (y/n) ")) {
            val name   = askCandidate("\nRemove troops from which space: ", candidates)
            val sp     = game.getSpace(name)
            val maxNum = sp.pieces.totalOf(USTroops) min numRemaining
            val num    = askInt(s"Remove how many troops from $name", 0, maxNum)
            removeToAvailable(name, Pieces(usTroops = num))
            mapToAvailable(numRemaining - num)
          }
        }
      }

      val choices = List(
        "from-oop" -> "Move US Troops from Out of Play to Available or South Vietname",
        "from-map" -> "Move US Troops from the map to Available"
      )
      askMenu(choices, "\nChoose one:").head match {
        case "from-oop" => loggingControlChanges(outOfPlayToAvailableOrMap(numOutOfPlay))
        case _          => loggingControlChanges(mapToAvailable(numOnMap))
      }
    }
    else if (game.usPolicy == USPolicy_Nixon) {
      if (numOutOfPlay > 0)
        moveOutOfPlayToAvailable(Pieces(usTroops = numOutOfPlay))
      else {
        loggingControlChanges {
          for (i <- 1 to numOnMap) {
            val candidates = game.spaces filter (_.pieces.has(USTroops))
            val sp = Bot.pickSpaceRemoveFriendlyPieces(candidates, Set(USTroops))
            removeToAvailable(sp.name, Pieces(usTroops = 1))
          }
        }
      }
    }
    else { // JFK or LBJ
      loggingControlChanges{
        for (i <- 1 to numOutOfPlay) {
            val candidates = game.spaces filter (sp => isInSouthVietnam(sp.name))
            val sp = Bot.pickSpacePlaceForces(US, troops = true)(candidates)
            moveOutOfPlayToMap(Pieces(usTroops = 1), sp.name)
        }
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    game.totalOnMap(_.pieces.totalOf(USTroops)) > 0

  def executeShaded(faction: Faction): Unit = {
    val totalTroops = game.totalOnMap(_.pieces.totalOf(USTroops)) min 3

    if (game.isHuman(US)) {

      def removeFromMap(numRemaining: Int): Unit = if (numRemaining > 0) {
          val candidates = spaceNames(game.spaces filter (_.pieces.has(USTroops)))
          println(s"\n${totalTroops - numRemaining} of $totalTroops troops removed to Out of Play")
          val name = askCandidate("\nRemove troops from which space: ", candidates)
            val num = askInt(s"Remove how many troops from $name", 0, numRemaining)
            removeToOutOfPlay(name, Pieces(usTroops = num))
            removeFromMap(numRemaining - num)

      }

      loggingControlChanges(removeFromMap(totalTroops))
    }
    else {
      loggingControlChanges {
        for (i <- 1 to totalTroops) {
          val candidates = game.spaces filter (_.pieces.has(USTroops))
          val sp = Bot.pickSpaceRemoveFriendlyPieces(candidates, Set(USTroops))
          removeToOutOfPlay(sp.name, Pieces(usTroops = 1))
        }
      }
    }
  }
}
