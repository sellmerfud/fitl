
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
// Strategic reconnaissance: Outside the South, flip all Insurgents
// Active and remove 1 NVA Base.
//
// Shaded Text
// SR-71 pilot must outrun SA-2s: Place 1 NVA Base at NVA Control outside
// the South and flip any 3 NVA Guerrillas Underground.
//
// Tips
// "At NVA Control outside the South" means place 1 available NVA Base into
// any North Vietnam, Laos, or Cambodia Province that already has NVA Control
// and where there are not already 2 Bases (1.3.5, 1.4.2, 5.1.1).

object Card_012 extends EventCard(12, "Capt Buck Adams",
    DualEvent,
    List(US, ARVN, NVA, VC),
    ListMap(US   -> (NotExecuted -> Unshaded),
    ARVN -> (Performed   -> Unshaded),
    NVA  -> (Critical    -> Shaded),
    VC   -> (NotExecuted -> Shaded))) {
    
  def unshadedEffective(faction: Faction): Boolean =
    game.spaces exists { sp =>
      isOutsideSouth(sp.name) &&
      sp.pieces.has(NVABase::UndergroundGuerrillas)
    }

  def executeUnshaded(faction: Faction): Unit = {
    for (sp <- game.spaces; if isOutsideSouth(sp.name) && sp.pieces.has(UndergroundGuerrillas)) {
      val underground = sp.pieces.only(UndergroundGuerrillas)
      revealPieces(sp.name, underground)
    }
    
    val baseSpaces = game.spaces filter { sp =>
      isOutsideSouth(sp.name) && sp.pieces.has(NVABase)
    }
    if (baseSpaces.nonEmpty)
      removePiecesFromMap(faction, 1, Set(NVABase), false, spaceNames(baseSpaces))
  }

    
  def shadedEffective(faction: Faction): Boolean = {
    val baseAvail = game.availablePieces.has(NVABase)
    val canPlaceBase = game.spaces.exists { sp =>
      isOutsideSouth(sp.name) &&
      sp.totalBases < 2 &&
      sp.nvaControlled
    }

    (baseAvail && canPlaceBase) ||
    (game totalOnMap (_.pieces.totalOf(NVAGuerrillas_A))) > 0
  }

  def executeShaded(faction: Faction): Unit = {
    val baseSpaces = game.spaces filter { sp =>
      isOutsideSouth(sp.name) && sp.totalBases < 2
    }
    def guerrillaSpaces = game.spaces filter (sp => sp.pieces.has(NVAGuerrillas_A))

    def humanFlip(numRemaining: Int): Unit = if (numRemaining > 0) {
      val totalActive = baseSpaces.map(_.pieces.totalOf(NVAGuerrillas_A)).sum
      if (totalActive == numRemaining) {
        for (sp <- baseSpaces)
          hidePieces(sp.name, sp.pieces.only(NVAGuerrillas_A))
      }
      else {
        val candidates = spaceNames(guerrillaSpaces)
        if (candidates.nonEmpty) {
          val name   = askCandidate("Flip guerrillas in which space: ", candidates)
          val maxNum = game.getSpace(name).pieces.totalOf(NVAGuerrillas_A) min numRemaining
          val num  = askInt("Flip how many guerrillas in $name", 0, maxNum)
          hidePieces(name, Pieces(nvaGuerrillas_A = num))
          humanFlip(numRemaining - num)
        }
      }
    }

    def botFlip(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = guerrillaSpaces
      if (candidates.nonEmpty) {
        // Event instructions say to use Place Guerrillas priority
        val sp = NVA_Bot.pickSpacePlaceGuerrillas(candidates)
        hidePieces(sp.name, Pieces(nvaGuerrillas_A = 1))
        botFlip(numRemaining - 1)
      }
    }

    placePiecesOnMap(faction, 1, Set(NVABase), spaceNames(baseSpaces))
    if (game.isHuman(faction))
      humanFlip(3)
    else
      botFlip(3)
  }
}
