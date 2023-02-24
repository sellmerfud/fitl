
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
// Obsolete: VC must remove 1 VC Guerrilla from each space with at least 2 and no NVA Base.
//
// Shaded Text
// NLF gets US arms captured in Korea: Place any 1 VC piece in each of 3 spaces.
//
// Tips
// For the unshaded text, because VC are deciding on removal, they may remove Active
// rather than Underground VC Guerrillas. For shaded, a "piece" may be a Base.

object Card_118 extends EventCard(118, "Korean War Arms",
  DualEvent,
  List(VC, ARVN, NVA, US),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.pieces.totalOf(VCGuerrillas) > 1 &&
    !sp.pieces.has(NVABases)

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate

    loggingControlChanges {
      for (sp <- candidates) {
        val guerrilla = if (sp.pieces.has(VCGuerrillas_A))
          Pieces(vcGuerrillas_A = 1)
        else
          Pieces(vcGuerrillas_U = 1)

        removePieces(sp.name, guerrilla)
      }
    }
  }

  def humanShaded(faction: Faction): Unit = {
    
    def nextPiece(spacesUsed: Set[String]): Unit = if (spacesUsed.size < 3) {
      val pool = (if (faction == VC) game.piecesToPlace else game.availablePieces).only(VCPieces)

      if (pool.nonEmpty) {
        val candidates = spaceNames(game.spaces filterNot (sp => spacesUsed(sp.name) && (pool.has(VCGuerrillas_U) || sp.canTakeBase)))
        val count      = spacesUsed.size + 1
        val name       = askCandidate(s"\nSelect ${ordinal(count)} space: ", candidates)
        val sp         = game.getSpace(name)
        val baseOK     = sp.canTakeBase && pool.has(VCBase)
        val gOK        = pool.has(VCGuerrillas_U)
        val toPlace    = (baseOK, gOK) match {
          case (true, true) =>
            val choices = List(Pieces(vcGuerrillas_U = 1) -> "VC Guerrilla", Pieces(vcBases = 1) -> "VC Base")
            askMenu(choices, s"\nSelect piece to place in $name").head
          case (true, false)  => Pieces(vcBases = 1)
          case (false, true)  => Pieces(vcGuerrillas_U = 1)
          case (false, false) => Pieces()
        }

        println()
        if (faction == VC && !game.availablePieces.contains(toPlace)) {
          voluntaryRemoval(1, if (toPlace.hasBase(VC)) VCBase else VCGuerrillas_U)
          println()
        }
        
        placePieces(name, toPlace)
        nextPiece(spacesUsed + name)
      }
    }

    if (faction != VC && game.availablePieces.totalOf(VCPieces) == 0)
      log("There are no available VC Pieces")
    else
      nextPiece(Set.empty)
  }

  def botShaded(): Unit = {    
    def nextPiece(spacesUsed: Set[String]): Unit = if (spacesUsed.size < 3) {
      val baseCandidates = game.nonLocSpaces filter { sp => !spacesUsed(sp.name) && sp.canTakeBase && sp.pieces.totalOf(Guerrillas) > 1 }
      if (game.availablePieces.has(VCBase) && baseCandidates.nonEmpty) {
        val sp = VC_Bot.pickSpacePlaceBases(baseCandidates)

        placePieces(sp.name, Pieces(vcBases = 1))
        nextPiece(spacesUsed + sp.name)
      }
      else if (game.availablePieces.has(VCGuerrillas_U)) {
        val candidates = game.spaces filter { sp => !spacesUsed(sp.name) }
        val sp         = VC_Bot.pickSpacePlaceGuerrillas(candidates)

        placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
        nextPiece(spacesUsed + sp.name)
      }
    }
    
    nextPiece(Set.empty)
  }


  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(VCPieces)

  def executeShaded(faction: Faction): Unit = {
    loggingControlChanges {
      if (game.isHuman(faction))
        humanShaded(faction)
      else
        botShaded()
    }
  }
}
