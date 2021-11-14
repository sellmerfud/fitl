
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
// A Shau Valley campaign: Move 4 US Troops from any spaces to a Highland.
// Remove 1 NVA or VC Base there, even if Tunneled.
//
// Shaded Text
// Prepared defenses: Place a Tunnel on an NVA or VC Highland Base.
// 3 US Troops there to Casualties.
//
// Tips
// "A Highland" means 1 Highland space. "Highland Base" means a Base in a Highland space.
// This is one of just a few Events that can force removal of Tunneled Bases (5.1.1).

object Card_036 extends EventCard(36, "Hamburger Hill",
  DualEvent,
  List(NVA, US, VC, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val hasHighlandUntunneledBase = (sp: Space) =>
     sp.isHighland &&
     sp.pieces.has(InsurgentNonTunnels)

  val hasHighlandUntunneledVCBase = (sp: Space) =>
     sp.isHighland &&
     sp.pieces.has(VCBase)

  def unshadedBotCandidates: List[Space] =
    game.nonLocSpaces filter { sp =>
      !sp.isNorthVietnam &&
      sp.isHighland &&
      sp.pieces.has(InsurgentBases) &&
      Bot.canMoveTo(US, sp.name, EventMove, Set(USTroops))
    }

  def unshadedEffective(faction: Faction): Boolean = unshadedBotCandidates.nonEmpty

  def executeUnshaded(faction: Faction): Unit = {
    val withTroops = game.spaces filter (_.pieces.has(USTroops))
    
    if (game.isHuman(faction)) {
      // Make sure there is at least US Troop on the map.
      // And in the unlikely case that all of the troops on the map
      // are in a single Highland province, don't allow that province to
      // be selected as the destination.
      if (withTroops.nonEmpty)
      {
        val prohibited = if (withTroops.size == 1 && withTroops.head.isHighland)
          withTroops.headOption map (_.name)
        else
          None

        val candidates  = spaceNames(game.spaces filter (sp => sp.isHighland && prohibited != Some(sp.name)))
        val name = askCandidate("Move US Troops to which space: ", candidates)

        loggingControlChanges {
          moveMapPiecesToSpace(US, 4, name, Set(USTroops), spaceNames(game.spaces))
          val sp = game.getSpace(name)        
          if (sp.pieces.has(InsurgentBases)) {
            val base = askPieces(sp.pieces, 1, InsurgentBases, Some("Remove one NVA or VC Base"))
            removeToAvailable(name, base)
          }
        }
      }
      else
        log("There are no US Troops on the map")
    }
    else {
      val sp = VC_Bot.pickSpaceRemoveReplace(unshadedBotCandidates)
      val base = Bot.selectEnemyRemovePlaceActivate(sp.pieces.only(InsurgentBases), 1)

      loggingControlChanges {
        Bot.doEventMoveTo(sp.name, US, 4, Set(USTroops))
        removeToAvailable(sp.name, base)
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists hasHighlandUntunneledVCBase

  def executeShaded(faction: Faction): Unit = {
    
    val (name, base) = if (game.isHuman(faction)) {
      val candidates = spaceNames(game.nonLocSpaces filter hasHighlandUntunneledBase)
      if (candidates.nonEmpty) {
        val name = askCandidate("Place a Tunnel on a NVA or VC base in which space: ", candidates)
        val bases = game.getSpace(name).pieces.only(InsurgentNonTunnels)
        val base = askPieces(bases, 1, prompt = Some("Select base to receive tunnel marker"))
        println()
        (name, base)
      }
      else {
        log("There are no untunneled Highland bases")
        ("", Pieces())
      }
    }
    else {
      val candidates = game.nonLocSpaces filter hasHighlandUntunneledVCBase
      val sp = VC_Bot.pickSpacePlaceBases(candidates)
      (sp.name, Pieces(vcBases = 1))
    }

    if (name != "") {
      val sp = game.getSpace(name)
      val numTroops = sp.pieces.totalOf(USTroops) min 3

      addTunnelMarker(name, base) 
      if (numTroops > 0)
        removeToCasualties(name, Pieces(usTroops = numTroops))
    }
  }
}
