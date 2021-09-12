
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

package fitl

import java.io.IOException
import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.language.implicitConversions
import FUtil.Pathname
import FireInTheLake._

// Functions to handle human commands/special activities
object Human {
  
  case class Params(
    includeSpecialActivity: Boolean = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    onlyIn: Option[Set[String]]     = None   // Limit command to the given spaces
  ) {
    val limCmdOnly = maxSpaces == Some(1)
  }


  // Aid in keeping track of when a special activity can be taken
  object SpecialActivity {
    private var allowSpecialActivity = false
    private var specialActivityTaken = false

    def init(params: Params): Unit = {
      allowSpecialActivity = params.includeSpecialActivity
      specialActivityTaken = false
    }
    def allowed = allowSpecialActivity && !specialActivityTaken
    def taken   = specialActivityTaken
    def completed() = specialActivityTaken = true
    def cancelled() = specialActivityTaken = false
  }

  // Use during a turn to keep track of pieces that have already moved
  // in each space.
  object MovingGroups {
    var groups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())

    def init(): Unit = { groups = Map.empty.withDefaultValue(Pieces()) }
    def apply(name: String): Pieces = groups(name)
    def add(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) + pieces)
    def remove(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) - pieces)

    def toList = groups.toList.sortBy(_._1)
    def size = groups.size
  }

  // A human player has opted to take an action on the current card.
  def act(): Unit = {
    object Pass extends Exception
    // Save the game state to handle the user aborting the action.
    val savedState = game
    val faction = game.nextUp.get

    try {
      val action = if (game.executingPivotalEvent)
        Event
      else {
        val choices: List[(Option[Action], String)] =
          (game.sequence.availableActions map (a => Some(a) -> a.toString)) :+ (None -> "Pass")

        askMenu(choices).head getOrElse { throw Pass }
      }

      game = game.copy(sequence = game.sequence.addActor(faction, action))
      log()
      log(s"Move the $faction cylinder to the $action box")

      action match {
        case Event         => executeEvent(faction)
        case OpPlusSpecial => executeCmd(faction, Params(includeSpecialActivity = true))
        case OpOnly        => executeCmd(faction)
        case LimitedOp     => executeCmd(faction, Params(maxSpaces = Some(1)))
      }

    }
    catch {
      case Pass =>
        factionPasses(faction)
        
      case AbortAction =>
        println("\n>>>> Aborting the current action <<<<")
        println(separator())
        displayGameStateDifferences(game, savedState)
        game = savedState
    }
  }

  def executeEvent(faction: Faction): Unit = {
    println("executeEvent() not implemented")

  }
  // Ask user to select command and execute it.
  def executeCmd(faction: Faction, params: Params = Params()): Unit = {
    SpecialActivity.init(params)
    MovingGroups.init()

    // faction match {
    //   case Dux       => DuxHuman.executeCmd(params)
    //   case Civitates => CivHuman.executeCmd(params)
    //   case Saxon     => SaxHuman.executeCmd(params)
    //   case Scotti    => ScoHuman.executeCmd(params)
    // }
  }

}
