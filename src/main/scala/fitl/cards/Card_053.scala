
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
import scala.collection.mutable.ListBuffer
import fitl.FireInTheLake._
import fitl.EventHelpers._
import fitl.Bot
import fitl.Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }
import fitl.Human

// Unshaded Text
// Ineffective tactics: Remove 2 NVA Troops each from up to 3
// spaces in South Vietnam. Remain Eligible.
//
// Shaded Text
// Facilities damaged: Remove up to 1 US and 2 ARVN Bases from
// any Provinces (US to Casualties).
//
// Tips
// Removal of pieces may change Control (1.7). The Bases may not be 
// removed from Cities. "US to Casualties" means that any US Base
// removed goes to the Casualties box, while ARVN Bases removed go
// to ARVN Available Forces as usual (1.4.1).

object Card_053 extends EventCard(53, "Sappers",
  DualEvent,
  List(NVA, VC, US, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    isInSouthVietnam(sp.name) &&
    sp.pieces.has(NVATroops)

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate

    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (game.isHuman(faction)) {
      val num = askInt("\nRemove NVA Troops from how many spaces", 0, candidates.size min 3)
      val choices = spaceNames(candidates) map (n => n -> n) 
      if (num > 0)
        askMenu(choices, s"\nSelect ${amountOf(num, "space")} to remove NVA Troops", numChoices = num)
      else
        Nil
    }
    else {  // Bot
      def nextSpace(count: Int, candidates: List[Space]): List[String] = {
        if (count <= 3 && candidates.nonEmpty) {
          val name = Bot.pickSpaceRemoveReplace(faction)(candidates).name
          name::nextSpace(count + 1, candidates filterNot (_.name == name))
        }
        else
          Nil
      }

      if (candidates.size <= 3)
        spaceNames(candidates)
      else
        nextSpace(1, candidates).reverse
    }

    if (selectedSpaces.isEmpty)
      log("There are no spaces in South Vietnam with NVA Troops")
    else {
      println()
      loggingControlChanges {
        for (name <- selectedSpaces) {
          val sp = game.getSpace(name)
          val num = sp.pieces.totalOf(NVATroops) min 2
          removePieces(name, Pieces(nvaTroops = num))
        }
      }
    }
    
    remainEligibleNextTurn(faction)
  }

  val hasUSBase = (sp: Space) => sp.isProvince && sp.pieces.has(USBase)
  val hasARVNBase = (sp: Space) => sp.isProvince && sp.pieces.has(ARVNBase)
  val shadedCandidate = (sp: Space) => hasUSBase(sp) || hasARVNBase(sp)

  def shadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit =  {
    val usCandidates = game.nonLocSpaces filter hasUSBase
    val arvnCandidates = game.nonLocSpaces filter hasARVNBase
    val maxARVN = (arvnCandidates map (_.pieces.totalOf(ARVNBase))).sum
    case class Selection(name: String, baseType: PieceType)

    def reduceArvn(arvn: List[Space], name: String): List[Space] = {
      val sp = arvn.find(_.name == name).get
      if (sp.pieces.totalOf(ARVNBase) == 1)
        arvn filterNot(_.name == name)
      else
        arvn map { sp => 
          if (sp.name == name)
            sp.copy(pieces = sp.pieces - Pieces(arvnBases = 1))
          else
            sp
        }
    }

    val usSelection = if (usCandidates.isEmpty) {
      log("There are no US Bases in any Provinces")
      Nil
    }
    else if (game.isBot(faction)) {
      val sp = Bot.pickSpaceRemoveReplace(faction)(usCandidates)
      Selection(sp.name, USBase)::Nil
    }
    else if (askYorN("Do you wish to remove a US Base? (y/n) ")) {
      val name = askSimpleMenu(spaceNames(usCandidates), "\nRemove a US Base from which Province:").head
      Selection(name, USBase)::Nil
    }
    else
      Nil

    val arvnSelection = if (maxARVN == 0) {
      log("There are no ARVN Bases in any Provinces")
      Nil
    }
    else if (game.isBot(faction)) {
      def nextSpace(numRemaining: Int, candidates: List[Space]): List[Selection] = {
        if (numRemaining > 0 && candidates.nonEmpty) {
          val sp = Bot.pickSpaceRemoveReplace(faction)(candidates)
          Selection(sp.name, ARVNBase)::nextSpace(numRemaining - 1, reduceArvn(candidates, sp.name))
        }
        else
          Nil
      }
      nextSpace(2, arvnCandidates)
    }
    else {
      val num = askInt("Remove how many ARVN Bases", 0, maxARVN min 2)
      if (num == 0)
        Nil
      else if (num == maxARVN) {
        val b = new ListBuffer[Selection]()
        for (sp <- arvnCandidates)
          for (i <- 1 to sp.pieces.totalOf(ARVNBase))
            b += Selection(sp.name, ARVNBase)
        b.toList
      }
      else {
        def nextSpace(numRemaining: Int, candidates: List[Space]): List[Selection] = {
          if (numRemaining > 0 && candidates.nonEmpty) {
            val name = askSimpleMenu(spaceNames(candidates), "\nRemove an ARVN Base from which Province:").head
            Selection(name, ARVNBase)::nextSpace(numRemaining - 1, reduceArvn(candidates, name))
          }
          else
            Nil
        }
        nextSpace(num, arvnCandidates).reverse
      }
    }

    val selections = usSelection:::arvnSelection
    log()
    if (selections.isEmpty)
      log("No US or ARVN bases removed")
    else
      for (Selection(name, baseType) <- usSelection:::arvnSelection)
        removePieces(name, Pieces().set(1, baseType))

  }
}
