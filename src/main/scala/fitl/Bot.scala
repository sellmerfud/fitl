
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

object Bot {
  
  def botLog(msg: => String) = if (game.botLogging) log(msg)
  def msgResult(msg: String, result: Any): String = {
    val resultStr = result match {
      case true  => "yes"
      case false => "no"
      case other => result.toString
    }
    
    s"$msg $resultStr"
  }
  
  
  case class Params(
    includeSpecial: Boolean         = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    assaultRemovesTwoExtra: Boolean = false, // M48 Patton (unshaded)
    onlyIn: Option[Set[String]]     = None   // Limit command to the given spaces
  ) {
    val limOpOnly = maxSpaces == Some(1)

    def spaceAllowed(name: String) = {
      (onlyIn map (allowed =>  allowed.contains(name)) getOrElse true)
    }

  }
  
    
  case class ActionEntry(val action: Action, desc: String, test: (Faction) => Boolean)
  
  def isFirstOnNextCard(faction: Faction) = {
    val nextCard = deck(game.onDeckCard)
    !nextCard.isCoup && faction == nextCard.factionOrder.head
  }
  
  val firstEligibileTable = List(
    // Event is Critical and Effective?
    ActionEntry(Event, "Current Event is Critical and Effective?",
    (faction) => {
      val card = deck(game.currentCard)
      card.eventPriority(faction) == Critical && card.eventEffective(faction)
    }),
    
    // Event is critical for Next eligible faction?
    ActionEntry(OpOnly, "Next Eligible could choose Critical Event?",
    (faction) => {
      val card = deck(game.currentCard)
      game.followingFaction map (next => card.eventPriority(next) == Critical) getOrElse false
    }),
    
    // Is first to act on next csrd and the event is Critical?
    ActionEntry(Pass, "First choice on upcoming Critical Event?",
      (faction) => {
      val nextCard = deck(game.onDeckCard)
      
      isFirstOnNextCard(faction) && nextCard.eventPriority(faction) == Critical
    }),
    
    // Otherwise...
    ActionEntry(OpPlusSpecial, "Otherwise...", (_) => true)
  )
  
  val secondEligibileTable = List(
    // Active fAction will be 1st Eligible on upcoming Critical Event and
    // cannot execute current Critical event?
    ActionEntry(Pass, "Will be 1st Eligible on upcoming Critical Event and cannot execute current Critical Event?",
      (faction) => {
      val card     = deck(game.currentCard)
      val nextCard = deck(game.onDeckCard)
      
      isFirstOnNextCard(faction) &&
      nextCard.eventPriority(faction) == Critical &&
      !(game.sequence.canDo(Event) &&
        card.eventPriority(faction) == Critical &&
        card.eventEffective(faction))
    }),
    
    // Event is Critical or Performed and it is Effective
    ActionEntry(Event, "1st Eligibile chose Op+SA and Event is Performed/Critical and Effective?",
      (faction) => {
      val card     = deck(game.currentCard)
      val priority = card.eventPriority(faction)
      
      game.sequence.canDo(Event) &&
      (priority == Critical || priority == Performed) &&
      card.eventEffective(faction)
    }),

    // Will be 1st Eligibile on upcoming card
    ActionEntry(Pass, "Will be 1st Eligibile on upcoming card?", (faction) => isFirstOnNextCard(faction)),
    
    // 1st Eligible chose Op Only or Op + SA?
    ActionEntry(LimitedOp, "1st Eligible chose Op Only or Op+SA?", (_) => { !game.sequence.canDo(OpPlusSpecial) }),
    
    // Otherwise...
    ActionEntry(OpPlusSpecial, "Otherwise...", (_) => true)
  )
  
  //  Choose an action for a Bot faction using the NP Elgibility Table
  //  A `prevEntry` is supplied if we previously chose an action but it
  //  was not able to be carried out effectively.  So we continue with the
  //  next row of the table.
  //  Note: The logic for determing if the Bot will choose its Pivotal Event
  //        is assumed to have already been done.
  def chooseAction(faction: Faction, prevEntry: Option[ActionEntry]): Option[ActionEntry] = {
    val isFirstEligible = game.sequence.numActors == 0
    
    botLog {
      val which = if (isFirstEligible) "1st" else "2nd"
      if (prevEntry.isEmpty)
        s"\nChoose Action using NP Eligiblity Table ($which eligible)"
      else
        s"\nChoose Action using NP Eligiblity Table ($which eligible) after ${prevEntry.get.action} was not possible"
    }
    botLog(separator())
    
    val table = {
      val t = if (isFirstEligible)
        firstEligibileTable
      else
        secondEligibileTable
      
      prevEntry match {
        case None       => t
        case Some(prev) => t drop (t.indexOf(prev) + 1)
      }
    }
    
    //  In case we previously used the last entry in the table and it was
    //  not possible to carry it out.
    if (table.isEmpty)
      None
    else 
      table find { entry =>
        val result = entry.test(faction)
        botLog(msgResult(entry.desc, result))
        result
      }
  }
  
  //  A bot is the next eligible faction
  //  Decide what type of action the Bot will take.
  //  We use the NP Elgibility Table.
  //  If an Op/Op+Sa/Lim-Op is selected and upon trying the operation we
  //  determine that it could not be carried out, then we continue down the
  //  table.
  def act(): Unit = {
    
    val faction = game.actingFaction.get
    val card = deck(game.currentCard)
    
    if (game.executingPivotalEvent) {
      //  The Pivotal events are single events which are always in the executeUnshaded() function.
      card.executeEvent(faction)
    }
    else {
      
      
      def selectAction(prevEntry: Option[ActionEntry]): Unit = {
        chooseAction(faction, prevEntry) match {          
          case None =>  // Could not carry out an action so Pass
            factionPasses(faction)
            
          case Some(ActionEntry(Pass, _, _)) => // TAble resolve to Pass
            factionPasses(faction)
            
            case Some(ActionEntry(Event, _, _)) =>
              card.executeEvent(faction)
              
            case entry @ Some(ActionEntry(action, _, _)) =>  // LimitedOp, OpOnly, or OpPlusSpecial
              val params = action match {
                case OpPlusSpecial => Params(includeSpecial = true)
                case LimitedOp     => Params(maxSpaces = Some(1))
                case _             => Params()
              }
              
              if (executeOp(faction, params)) {
                game = game.copy(sequence = game.sequence.addActor(faction, action))
                log()
                log(s"Move the $faction cylinder to the $action box")
              }
              else
                selectAction(entry)  // The operation could not be carried out so try again
        }
      }
      
      selectAction(None)
      
    }
  }
  
  
  // Return TRUE if the operation was carried out
  // It may not be able to be done if:
  // ARVN resources == 0 and one of the COIN factions is Human
  // There are not eligible spaces to operate on, etc.
  def executeOp(faction: Faction, params: Params): Boolean = {
      true
  }

  
}
