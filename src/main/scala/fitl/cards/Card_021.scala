
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
// 23rd Division: US moves up to 2 US Troops each from the map and
// out of play to any 1 space or Available.
//
// Shaded Text
// US divisions "clean out" NLF: In 1 or 2 Provinces with US Troops,
// remove 1 VC piece to set to Active Opposition.
//
// Tips
// For the shaded Event, the VC "piece" could be a Guerrilla or an unTunneled
// Base (1.4, 5.1.1). If there are Provinces that have US Troops, VC, and could
// be set (changed) to Active Opposition, such Provinces would have to be chosen
// first. Note that COIN Factions might advantageously use the shaded text (5.2)!

object Card_021 extends EventCard(21, "Americal", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded))) {
      
  def unshadedEffective(faction: Faction): Boolean =
    game.outOfPlay.totalOf(USTroops) > 0 ||
    game.totalOnMap(_.pieces.totalOf(USTroops)) > 0
        
  def executeUnshaded(faction: Faction): Unit = {
    // Move up to 2 US Troops EACH from the map and Out of Play
    // All Troops moved must go to either Available or to a single space on the map.
    // US alwasy does the removal regardless of who is executing the event.
    val (destName: Option[String], oopTroops: Pieces, numMapTroops: Int) = if (game.isHuman(US)) {
      val destChoices = List("available" -> "Available", "map" -> "A space on the map")
      val destName = askMenu(destChoices, "\nChoose destination for US Troops:").head match {
        case "available" => None // Destination is the available Box
        case _ =>
          val candidates = spaceNames(game.spaces filterNot (_.isNorthVietnam))
          Some(askCandidate("Move US Troops to which space: ", candidates))
      }

      val maxOOP    = game.outOfPlay.totalOf(USTroops) min 2
      val numOOP    = askInt("\nMove how many US Troops from Out Of Play", 0, maxOOP)
      val oopTroops = Pieces(usTroops = numOOP)
      val origins   = game.spaces filter { sp =>
        Some(sp.name) != destName &&
        sp.pieces.has(USTroops)
      }
      val numOnMap  = (origins map (_.pieces.totalOf(USTroops))).sum
      val maxMap    = numOnMap min 2
      val numMap    = askInt("Move how many US Troops from the map", 0, maxMap)
      (destName, oopTroops, numMap)
    }
    else {
      // JFK/LBJ: move Troops to South Vietnam
      // Nixon:   move Troops to Available
      val destName = if (game.usPolicy == USPolicy_Nixon)
        None  // To available
      else
        Some(Bot.pickSpacePlaceForces(US)(game.spaces filter (sp => isInSouthVietnam(sp.name))).name)
      val oopTroops  = Pieces(usTroops = game.outOfPlay.totalOf(USTroops) min 2)
      val totalOnMap = game.totalOnMap(_.pieces.totalOf(USTroops))
      val numMap     = destName match {
        case None => totalOnMap min 2
        case Some(dest) => (totalOnMap - game.getSpace(dest).pieces.totalOf(USTroops)) min 2
      }
      (destName, oopTroops, numMap)
    }

    val originNames = spaceNames(game.spaces)
    loggingControlChanges {
      destName match {
        case None =>
          moveOutOfPlayToAvailable(oopTroops)
          removePiecesFromMap(US, numMapTroops, Set(USTroops), true, originNames, usToAvailable = true)
        case Some(dest) =>
          placePiecesFromOutOfPlay(dest, oopTroops)
          moveMapPiecesToSpace(US, numMapTroops, true, dest, Set(USTroops), Some(originNames.toSet))
      }
    }
  }

  val isAmericalShadedSpace = (sp: Space) =>
    sp.isProvince &&
    sp.population > 0 &&
    sp.pieces.has(USTroops) &&
    sp.pieces.has(VCBase::VCGuerrillas)  // Not Tunneled bases

  val isBotAmericalShadedSpace = (sp: Space) =>
    sp.support != ActiveOpposition &&
    isAmericalShadedSpace(sp)
  
  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists isBotAmericalShadedSpace
    
  def executeShaded(faction: Faction): Unit = {
    if (game.isHuman(faction)) {
      def nextProvince(spaceNum: Int): Unit = if (spaceNum <= 2) {
        val candidates = spaceNames(game.nonLocSpaces filter isAmericalShadedSpace)
        if (candidates.nonEmpty) {
          val choices = candidates.map(n => n -> n) :+ "finished" -> "Finished selecting spaces for the Americal Event"
          askMenu(choices, s"\nSelect ${ordinal(spaceNum)} Americal space:").head match {
            case "finished" => 
            case name =>
              val sp      = game.getSpace(name)
              val allowed = if (faction == VC) VCGuerrillas :+ VCBase else VCBase::VCGuerrillas
              val piece   = askPieces(sp.pieces, 1, allowed, Some(s"\nSelecting piece to remove from $name"))
              removePieces(name, piece)
              setSupport(name, ActiveOpposition)
              nextProvince(spaceNum + 1)
          }
        }
      }

      loggingControlChanges(nextProvince(1))
    }
    else {
      // VC Bot
      def nextProvince(numRemaining: Int): Unit = if (numRemaining > 0) {
        val candidates = game.nonLocSpaces filter isBotAmericalShadedSpace

        if (candidates.nonEmpty) {
          val sp    = Bot.pickSpaceRemoveFriendlyPieces(candidates, VCBase::VCGuerrillas)
          val piece = Bot.selectFriendlyRemoval(sp.pieces.only(VCBase::VCGuerrillas), 1)
          removePieces(sp.name, piece)
          setSupport(sp.name, ActiveOpposition)
          nextProvince(numRemaining - 1)
        }
      }
      loggingControlChanges(nextProvince(2))
    }
  }
}
