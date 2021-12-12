
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
// RF/PF–Regional and Popular Forces: Place up to 8 Police in the South.
//
// Shaded Text
// Ill-trained, thoroughly subverted: Replace 5 Police outside Cities with
// 1 VC piece each—1 of the VC pieces may be a Base.
// 
// Tips
// In the unshaded text, “in the South” means into any spaces in South Vietnam,
// including any LoCs. For shaded, only 1 VC piece placed may be a Base, the others
// must be Guerrillas.

object Card_113 extends EventCard(113, "Ruff Puff",
  DualEvent,
  List(VC, ARVN, US, NVA),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {


  def humanPlacePolice(faction: Faction): Unit = {
    val candidates = spaceNames(game.spaces filter (sp => isInSouthVietnam(sp.name)))
    val maxPolice  = if (isCoin(faction))
      game.piecesToPlace.totalOf(ARVNPolice) min 8
    else
      game.availablePieces.totalOf(ARVNPolice) min 8
    val numPolice  = askInt("\nHow many Police do you wish to place in South Vietnam", 0, maxPolice)

    placePiecesOnMap(faction, numPolice, Set(ARVNPolice), candidates)
  }

  def botPlacePolice(faction: Faction): Unit = {
    def pickUSSpace(candidates: List[Space]): Space = {
      val priorities = List(
        new Bot.BooleanPriority[Space]("With US Troops", _.pieces.has(USTroops)),
        new Bot.BooleanPriority[Space]("Without Police", !_.pieces.has(ARVNPolice)),
        new Bot.HighestScore[Space]("Most Pop.", _.population)
      )

      Bot.bestCandidate(candidates, priorities)
    }

    def nextPolice(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = game.spaces filter (sp => isInSouthVietnam(sp.name))
      val sp = if (faction == US)
        pickUSSpace(candidates)
      else
        ARVN_Bot.pickSpacePlaceCubesRangers(candidates)

      placePieces(sp.name, Pieces(arvnPolice = 1))
      nextPolice(numRemaining - 1)
    }

    nextPolice(game.availablePieces.totalOf(ARVNPolice) min 8)
  }

  def unshadedEffective(faction: Faction): Boolean = game.availablePieces.has(ARVNPolice)

  def executeUnshaded(faction: Faction): Unit = {
    loggingControlChanges {
      if (game.isHuman(faction))
        humanPlacePolice(faction)
      else
        botPlacePolice(faction)
    }
  }

  val shadedCandidate = (sp: Space) =>
    !sp.isCity &&
    sp.pieces.has(ARVNPolice)

  def shadedEffective(faction: Faction): Boolean = false  // Not executed by Bots

  def executeShaded(faction: Faction): Unit = {
    // Human only

    def askPlaceBase(sp: Space, faction: Faction): Boolean = {
      val haveBase = if (faction == VC)
        game.piecesToPlace.has(VCBase)
      else
        game.availablePieces.has(VCBase)

      sp.canTakeBase &&
      haveBase &&
      askYorN(s"\nDo you wish to place a VC Base in ${sp.name}? (y/n) ")
    }

    def nextReplacement(count: Int, canPlaceBase: Boolean): Unit = {
      val candidates = spaceNames(game.spaces filter shadedCandidate)
      if (count <= 5 && candidates.nonEmpty) {
        println(s"\n${amountOf(count - 1, "Police cube")} replaced so far")
        val name      = askSimpleMenu(candidates, s"Select a space with a Police cube:").head
        val sp        = game.getSpace(name)
        val toPlace = if (canPlaceBase && askPlaceBase(sp, faction)) {
          if (!game.availablePieces.has(VCBase))
            voluntaryRemoval(1, VCBase)
          Pieces(vcBases = 1)
        }
        else {
          val num = (faction, game.availablePieces.has(VCGuerrillas_U)) match {
            case (_,  true)  => 1
            case (VC, false) => Human.numToPlace(VCGuerrillas_U, 1)
            case (_,  false) => 0
          }
          Pieces(vcGuerrillas_U = num)
        }

        println()
        removePieces(name, Pieces(arvnPolice = 1))
        placePieces(name, toPlace)
        nextReplacement(count + 1, canPlaceBase && !toPlace.has(VCBase))
      }
    }

    if (!(game.spaces exists shadedCandidate))
      log("There are no Police outside of the Cities")
    else
      loggingControlChanges {
        nextReplacement(1, true)
      }
  }
}
