
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
// VC caught off guard: Remove all VC from a coastal Province
// with or adjacent to US Troops.
//
// Shaded Text
// Slipped away: In up to 3 Provinces, flip all VC Guerrillas Underground.
// Stay Eligible.
//
// Tips
// "Coastal" Provinces are those touching or across a Highway from ocean (1.3.7).
// For the shaded text, the Faction executing the Event stays Eligible.

object Card_024 extends EventCard(24, "Operation Starlite", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (Ignored -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Ignored -> Shaded),
              VC   -> (Performed   -> Shaded))) {
  
  val isStarliteUnshadedSpace = (sp: Space) =>
    sp.isProvince &&
    sp.coastal    &&
    sp.pieces.has(VCPieces) &&
    withOrAdjacentExists(sp.name)(_.pieces.has(USTroops))

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists isStarliteUnshadedSpace

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter isStarliteUnshadedSpace
    val name = if (game.isHuman(faction)) {
      if (candidates.nonEmpty)
        Some(askCandidate("\nExecute event in which space: ", spaceNames(candidates)))
      else
        None
    }
    else {
      //  I am narrowing the selection to spaces that contain
      //  a VC base (if any) because that makes more sense.
      val withVCBase = List(new Bot.BooleanPriority[Space]("With VC Base", _.pieces.has(VCBase)))
      val narrowed = Bot.narrowCandidates(candidates, withVCBase)
      Some(Bot.pickSpaceRemoveReplace(faction)(narrowed).name)
    }

    name match {
      case Some(name) =>
        val sp = game.getSpace(name)
        removePieces(name, sp.pieces.only(VCPieces))
      case None =>
        log("There are no coastal spaces that qualify.")
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    game.spaces exists { sp =>
      sp.isProvince &&
      sp.pieces.has(VCGuerrillas_A)
    }
    
  def executeShaded(faction: Faction): Unit = {
    def nextSpace(count: Int, total: Int, candidates: List[Space]): List[String] = {
      if (count > total || candidates.isEmpty)
        Nil
      else {
        val name = if (game.isHuman(faction))
          askCandidate(s"Select ${ordinal(count)} space: ", spaceNames(candidates))
        else
          VC_Bot.pickSpacePlaceGuerrillas(candidates).name
        name::nextSpace(count + 1, total, candidates filterNot (_.name == name))
      }
    }

    val candidates = game.spaces filter { sp => sp.isProvince && sp.pieces.has(VCGuerrillas_A) }
    val maxNum = candidates.size min 3
    if (candidates.nonEmpty) {
      val num = if (game.isHuman(faction))
        askInt("\nExecute event in how many Provinces", 0, maxNum)
      else
        maxNum
      if (num == candidates.size) {
        for (sp <- candidates)
          hidePieces(sp.name, sp.pieces.only(VCGuerrillas_A))
      }
      else {
        for (name <- nextSpace(1, num, candidates).reverse) {
          val sp = game.getSpace(name)
          hidePieces(name, sp.pieces.only(VCGuerrillas_A))
        }
      }
    }
    else
      log("There are no Provinces with underground VC Guerrillas.")
    remainEligibleNextTurn(faction)
  }
}
