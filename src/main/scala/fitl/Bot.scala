
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
  
  // Used to implement the Eligibility Tables
  case class ActionEntry(val action: Action, desc: String, test: (Faction) => Boolean)
  
  // Possible results when attempting to execute the instructions
  // on a Trung Card
  sealed trait TrungResult
  case object TrungComplete extends TrungResult
  case object TrungDraw     extends TrungResult
  case object TrungFlip     extends TrungResult
  case object TrungNoOp     extends TrungResult
  
  
  // Trung Card defintion
  // The front and back of each card are treated a seperate
  // entities internally.
  
  trait TrungCard {
    val faction: Faction
    val id: String
    val flipSide: TrungCard
    
    def isFront = id.length == 1
    
    override def toString() = s"Trung: $faction - $id"
    
    def execute(faction: Faction, params: Params): TrungResult
  }
  
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
              val sa    = action == OpPlusSpecial
              val maxsp = if (action == LimitedOp) Some(1) else None
              val params = Params(includeSpecial = sa, maxSpaces = maxsp)
              
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
  // We keep track of the first card that we drew for the
  // off chance that we cycle through all of them and cannot perform
  // and operation.  Probably won't happend but this will prevent and
  // endless loop.
  def executeOp(faction: Faction, params: Params): Boolean = {
    val firstCard = drawTrungCard(faction)

    def executeCard(trungCard: TrungCard): Boolean = {
       trungCard.execute(faction, params) match {
         case  TrungComplete =>
           true
           
         case  TrungDraw =>
           val nextCard = drawTrungCard(faction)
           if (nextCard == firstCard)
             false
           else
             executeCard(nextCard)
           
         case  TrungFlip =>
           executeCard(trungCard.flipSide)
           
         case  TrungNoOp =>
           false
       }
    }

    executeCard(firstCard)
  }


  
  // The Trung Deck contains only the face up cards.
  // The face down are accessed from the flipSide of the face up cards.
  val TrungDeck = List(
    Trung_US_A,  Trung_ARVN_G,  Trung_NVA_N,  Trung_VC_U,
    Trung_US_B,  Trung_ARVN_H,  Trung_NVA_P,  Trung_VC_V,
    Trung_US_C,  Trung_ARVN_J,  Trung_NVA_Q,  Trung_VC_W,
    Trung_US_D,  Trung_ARVN_K,  Trung_NVA_R,  Trung_VC_X,
    Trung_US_E,  Trung_ARVN_L,  Trung_NVA_S,  Trung_VC_Y,
    Trung_US_F,  Trung_ARVN_M,  Trung_NVA_T,  Trung_VC_Z
  )


  //================================================================
  // US Trung Cards
  //================================================================

  object Trung_US_A extends TrungCard {
    val faction = US
    val id = "A"
    lazy val flipSide = Trung_US_AA
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_AA extends TrungCard {
    val faction = US
    val id = "AA"
    lazy val flipSide = Trung_US_A
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_B extends TrungCard {
    val faction = US
    val id = "B"
    lazy val flipSide = Trung_US_BB
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_BB extends TrungCard {
    val faction = US
    val id = "BB"
    lazy val flipSide = Trung_US_B
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_C extends TrungCard {
    val faction = US
    val id = "C"
    lazy val flipSide = Trung_US_CC
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_CC extends TrungCard {
    val faction = US
    val id = "CC"
    lazy val flipSide = Trung_US_C
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_D extends TrungCard {
    val faction = US
    val id = "D"
    lazy val flipSide = Trung_US_DD
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_DD extends TrungCard {
    val faction = US
    val id = "DD"
    lazy val flipSide = Trung_US_D
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_E extends TrungCard {
    val faction = US
    val id = "E"
    lazy val flipSide = Trung_US_EE
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_EE extends TrungCard {
    val faction = US
    val id = "EE"
    lazy val flipSide = Trung_US_E
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_F extends TrungCard {
    val faction = US
    val id = "F"
    lazy val flipSide = Trung_US_FF
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_FF extends TrungCard {
    val faction = US
    val id = "FF"
    lazy val flipSide = Trung_US_F
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }


  //================================================================
  // US Trung Cards
  //================================================================

  object Trung_ARVN_G extends TrungCard {
    val faction = ARVN
    val id = "G"
    lazy val flipSide = Trung_ARVN_GG
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_GG extends TrungCard {
    val faction = ARVN
    val id = "GG"
    lazy val flipSide = Trung_ARVN_G
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_H extends TrungCard {
    val faction = ARVN
    val id = "H"
    lazy val flipSide = Trung_ARVN_HH
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_HH extends TrungCard {
    val faction = ARVN
    val id = "HH"
    lazy val flipSide = Trung_ARVN_H
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_J extends TrungCard {
    val faction = ARVN
    val id = "J"
    lazy val flipSide = Trung_ARVN_JJ
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_JJ extends TrungCard {
    val faction = ARVN
    val id = "JJ"
    lazy val flipSide = Trung_ARVN_J
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_K extends TrungCard {
    val faction = ARVN
    val id = "K"
    lazy val flipSide = Trung_ARVN_KK
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_KK extends TrungCard {
    val faction = ARVN
    val id = "KK"
    lazy val flipSide = Trung_ARVN_K
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_L extends TrungCard {
    val faction = ARVN
    val id = "L"
    lazy val flipSide = Trung_ARVN_LL
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_LL extends TrungCard {
    val faction = ARVN
    val id = "LL"
    lazy val flipSide = Trung_ARVN_L
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_M extends TrungCard {
    val faction = ARVN
    val id = "M"
    lazy val flipSide = Trung_ARVN_MM
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_MM extends TrungCard {
    val faction = ARVN
    val id = "MM"
    lazy val flipSide = Trung_ARVN_M
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }


  //================================================================
  // US Trung Cards
  //================================================================

  object Trung_NVA_N extends TrungCard {
    val faction = NVA
    val id = "N"
    lazy val flipSide = Trung_NVA_NN
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_NN extends TrungCard {
    val faction = NVA
    val id = "NN"
    lazy val flipSide = Trung_NVA_N
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_P extends TrungCard {
    val faction = NVA
    val id = "P"
    lazy val flipSide = Trung_NVA_PP
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_PP extends TrungCard {
    val faction = NVA
    val id = "PP"
    lazy val flipSide = Trung_NVA_P
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_Q   extends TrungCard {
    val faction = NVA
    val id = "Q"
    lazy val flipSide = Trung_NVA_QQ  
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_QQ   extends TrungCard {
    val faction = NVA
    val id = "QQ"
    lazy val flipSide = Trung_NVA_Q
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_R   extends TrungCard {
    val faction = NVA
    val id = "R"
    lazy val flipSide = Trung_NVA_RR  
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_RR   extends TrungCard {
    val faction = NVA
    val id = "RR"
    lazy val flipSide = Trung_NVA_R
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_S   extends TrungCard {
    val faction = NVA
    val id = "S"
    lazy val flipSide = Trung_NVA_SS
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_SS   extends TrungCard {
    val faction = NVA
    val id = "SS"
    lazy val flipSide = Trung_NVA_S
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_T   extends TrungCard {
    val faction = NVA
    val id = "T"
    lazy val flipSide = Trung_NVA_TT
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_TT   extends TrungCard {
    val faction = NVA
    val id = "TT"
    lazy val flipSide = Trung_NVA_T
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  
  //================================================================
  // US Trung Cards
  //================================================================
  
  object Trung_VC_U extends TrungCard {
    val faction = VC
    val id = "U"
    lazy val flipSide = Trung_VC_UU
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_UU extends TrungCard {
    val faction = VC
    val id = "UU"
    lazy val flipSide = Trung_VC_U
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_V extends TrungCard {
    val faction = VC
    val id = "V"
    lazy val flipSide = Trung_VC_VV
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_VV extends TrungCard {
    val faction = VC
    val id = "VV"
    lazy val flipSide = Trung_VC_V
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_W extends TrungCard {
    val faction = VC
    val id = "W"
    lazy val flipSide = Trung_VC_WW
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_WW extends TrungCard {
    val faction = VC
    val id = "WW"
    lazy val flipSide = Trung_VC_W
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_X extends TrungCard {
    val faction = VC
    val id = "X"
    lazy val flipSide = Trung_VC_XX
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_XX extends TrungCard {
    val faction = VC
    val id = "XX"
    lazy val flipSide = Trung_VC_X
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_Y extends TrungCard {
    val faction = VC
    val id = "Y"
    lazy val flipSide = Trung_VC_YY
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_YY extends TrungCard {
    val faction = VC
    val id = "YY"
    lazy val flipSide = Trung_VC_Y
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_Z extends TrungCard {
    val faction = VC
    val id = "Z"
    lazy val flipSide = Trung_VC_ZZ
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_ZZ extends TrungCard {
    val faction = VC
    val id = "ZZ"
    lazy val flipSide = Trung_VC_Z
    
    
    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  
  
  
  
  
  
}
