
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
// a copy of this software and associated documentation files (the
//
// Permission is hereby granted, free of charge, to any person obtaining
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

import scala.collection.immutable.ListMap
import scala.util.Random.shuffle
import FireInTheLake._
import Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }

object EventHelpers {
  // Temporary Functions
  def unshadedNotYet(): Unit = {
    log(s"\n${eventDeck(game.currentCard)}: Unshaded event not yet implemented")
  }
  
  def shadedNotYet(): Unit = {
    log(s"\n${eventDeck(game.currentCard)}: Shaded event not yet implemented")
  }
  def singleNotYet(): Unit = {
    log(s"\n${eventDeck(game.currentCard)}: Event not yet implemented")
  }
  def coupNotYet(): Unit = {
    log(s"\n${eventDeck(game.currentCard)}: Coup event not yet implemented")
  }
  
  def pivotalNotYet(faction: Faction): Unit = {
    log(s"\n${eventDeck(game.currentCard)}: $faction pivotal event not yet implemented")
  }

    // Returns true if an air strike would be effective in the space.
  val canAirStrike = (sp: Space) =>
      sp.pieces.has(CoinPieces) &&
      numExposedInsurgents(sp.pieces) > 0

  def airStrikeEffective: Boolean = {
    val prohibited = momentumInPlay(Mo_RollingThunder) ||
                     momentumInPlay(Mo_DaNang)         ||
                     momentumInPlay(Mo_BombingPause)

    if (prohibited)
      false
    else if (capabilityInPlay(ArcLight_Unshaded))
      game.spaces exists (sp => numExposedInsurgents(sp.pieces) > 0)
    else
      game.spaces exists canAirStrike
  }

  // Remove the given number pieces from the map.
  // US pieces are removed to casualties unless available is specified
  // all other pieces are removed to available.
  // Returns the set of spaces where pieces were removed.
  def removePiecesFromMap(
    faction: Faction,
    numToRemove: Int,
    pieceTypes: TraversableOnce[PieceType],
    friendly: Boolean,
    validSpaces: TraversableOnce[String],
    usToAvailable: Boolean = false): Set[String] = {
    val validNames = validSpaces.toSet
    val hasPieces = (sp: Space) => validNames(sp.name) && sp.pieces.has(pieceTypes)
    val desc = if (pieceTypes.size > 3)
      "pieces"
    else
      andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter hasPieces)
      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc removed: ${numToRemove - numRemaining} of ${numToRemove}")
        val name     = askCandidate(s"Remove $desc from which space: ", candidates)
        val sp       = game.getSpace(name)
        val pieces   = sp.pieces.only(pieceTypes)
        val minNum   = if (candidates.size == 1) numRemaining min pieces.total else 0
        val num      = askInt(s"Remove how many pieces from $name", minNum, numRemaining min pieces.total)
        val toRemove = askPieces(pieces, num, pieceTypes.toSeq)
        if (num > 0) {
          println()
          if (usToAvailable)
            removeToAvailable(sp.name, toRemove)
          else
            removePieces(name, toRemove)
          spacesUsed += name
        }
        nextHumanRemoval(numRemaining - num)
      }
    }

    def nextBotRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = game.spaces filter hasPieces
      if (candidates.nonEmpty) {
        val sp = if (friendly)
          Bot.pickSpaceRemoveFriendlyPieces(candidates, pieceTypes)
        else {
          // Always prioritize space with an ememy base if possible
          val priorities = List(new Bot.BooleanPriority[Space](
            "With enemy base",
            sp => includesEnemyBase(faction, sp.pieces.only(pieceTypes).getTypes)
          ))
          val narrowed = Bot.narrowCandidates(candidates, priorities)

          Bot.pickSpaceRemoveReplace(faction)(narrowed)
        }
        val pieces   = sp.pieces.only(pieceTypes)
        val toRemove = if (friendly)
          Bot.selectFriendlyRemoval(pieces, 1)
        else
          Bot.selectEnemyRemovePlaceActivate(pieces, 1)

        if (usToAvailable)
          removeToAvailable(sp.name, toRemove)
        else
          removePieces(sp.name, toRemove)
        spacesUsed += sp.name
        nextBotRemoval(numRemaining - 1)
      }
    }

    def removeAll(): Unit = {
      for {
        name <- validSpaces
        sp   = game.getSpace(name)
        dead = sp.pieces.only(pieceTypes)
        if dead.nonEmpty
      } {
        if (usToAvailable)
          removeToAvailable(name, dead)
        else
          removePieces(name, dead)
        spacesUsed += name
      }
    }

    loggingControlChanges {
      val totalPieces = spaces(validSpaces).foldLeft(0)((sum, sp) => sum + sp.pieces.totalOf(pieceTypes))
  
      if (totalPieces <= numToRemove)
        removeAll()
      else if (game.isHuman(faction))
        nextHumanRemoval(numToRemove)
      else
        nextBotRemoval(numToRemove)
    }
    spacesUsed
  }

  def removePiecesToOutOfPlay(
    faction: Faction,
    numToRemove: Int,
    pieceTypes: TraversableOnce[PieceType],
    friendly: Boolean,
    validSpaces: TraversableOnce[String]): Set[String] = {
    val validNames = validSpaces.toSet
    val hasPieces = (sp: Space) => validNames(sp.name) && sp.pieces.has(pieceTypes)
    val desc = andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter hasPieces)
      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc removed to Out of Play: ${numToRemove - numRemaining} of ${numToRemove}")
        val name     = askCandidate(s"Remove $desc from which space: ", candidates)
        val sp       = game.getSpace(name)
        val pieces   = sp.pieces.only(pieceTypes)
        val num      = askInt(s"Remove how many pieces from $name", 0, numRemaining min pieces.total)
        val toRemove = askPieces(pieces, num, pieceTypes.toSeq)
        if (num > 0) {
          println()
          removeToOutOfPlay(name, toRemove)
          spacesUsed += name
        }
        nextHumanRemoval(numRemaining - num)
      }
    }

    def nextBotRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = game.spaces filter hasPieces
      if (candidates.nonEmpty) {
        val sp = if (friendly)
          Bot.pickSpaceRemoveFriendlyPieces(candidates, pieceTypes)
        else
          Bot.pickSpaceRemoveReplace(faction)(candidates)
        val pieces   = sp.pieces.only(pieceTypes)
        val toRemove = if (friendly)
          Bot.selectFriendlyRemoval(pieces, 1)
        else
          Bot.selectEnemyRemovePlaceActivate(pieces, 1)
        removeToOutOfPlay(sp.name, toRemove)
        spacesUsed += sp.name
        nextBotRemoval(numRemaining - 1)
      }
    }

    def removeAll(): Unit = {
      for {
        name <- validSpaces
        sp   = game.getSpace(name)
        dead = sp.pieces.only(pieceTypes)
        if dead.nonEmpty
      } {
        removeToOutOfPlay(name, dead)
        spacesUsed += name
      }
    }

    val totalPieces = spaces(validSpaces).foldLeft(0)((sum, sp) => sum + sp.pieces.totalOf(pieceTypes))

    loggingControlChanges {
      if (totalPieces <= numToRemove)
        removeAll()
      else if (game.isHuman(faction))
        nextHumanRemoval(numToRemove)
      else
        nextBotRemoval(numToRemove)
    }
    spacesUsed
  }

  // Place pieces from available
  // Returns the set of spaces where pieces were placed
  def placePiecesOnMap(faction: Faction, numToPlace: Int, pieceTypes: TraversableOnce[PieceType],
                          validSpaces: TraversableOnce[String]): Set[String] = {
    val validNames = validSpaces.toSet
    val isValid = (sp: Space) => validNames(sp.name)
    val canTakeBase  = (sp: Space) => isValid(sp) && sp.totalBases < 2
    val desc = andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanPlacement(numRemaining: Int): Unit = if (numRemaining > 0) {
      val bases      = game.piecesToPlace.only(pieceTypes).only(BasePieces)
      val forces     = game.piecesToPlace.only(pieceTypes).except(BasePieces)
      val candidates = if (forces.nonEmpty)
        spaceNames(game.spaces filter isValid)
      else
        spaceNames(game.spaces filter canTakeBase)

      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc placed: ${numToPlace - numRemaining} of ${numToPlace}")
        val name      = askCandidate(s"Place $desc in which space: ", candidates)
        val sp        = game.getSpace(name)
        val placeBase = bases.nonEmpty && canTakeBase(sp) &&
                        askYorN(s"Do you wish to place a base in $name? (y/n) ")
        val pieces    = if (placeBase)
          askToPlaceBase(name, bases.explode().head)
        else
          askPiecesToPlace(name, forces.getTypes, numRemaining)
        if (pieces.nonEmpty) {
          println()
          placePieces(name, pieces)
          spacesUsed += name
        }
        nextHumanPlacement(numRemaining - pieces.total)
      }
    }

    def nextBotPlacement(numRemaining: Int, availPieces: Pieces): Unit = if (numRemaining > 0 && availPieces.nonEmpty) {
      val piece = Bot.selectFriendlyToPlaceOrMove(availPieces, 1)
      val optSpace = if (piece.has(BasePieces)) {
        val candidates = game.spaces filter canTakeBase
        if (candidates.nonEmpty)
          Some(Bot.pickSpacePlaceBases(faction)(candidates))
        else
          None
      }
      else {
        val candidates = game.spaces filter isValid
        if (candidates.nonEmpty) {
          val isTroop = piece.has(USTroops::NVATroops::ARVNTroops::Nil)
          Some(Bot.pickSpacePlaceForces(faction, isTroop)(candidates))
        }
        else
          None
      }
      optSpace match {
        case Some(sp) =>
          placePieces(sp.name, piece)
          spacesUsed += sp.name
          nextBotPlacement(numRemaining - 1, availPieces - piece)
        case None =>
          // It is possible that there are base available but no
          // space can accomodate a base, so remove the piece from
          // consideration and continue
          nextBotPlacement(numRemaining, availPieces - piece)
      }
    }

    loggingControlChanges {
      if (game.isHuman(faction))
        nextHumanPlacement(numToPlace min game.piecesToPlace.only(pieceTypes).total)
      else
        nextBotPlacement(numToPlace, game.availablePieces.only(pieceTypes))
    }
    spacesUsed
  }

  // Place pieces from Out Of Play
  // Returns the set of spaces where pieces were placed
  def placeOutOfPlayPiecesOnMap(faction: Faction, numToPlace: Int, pieceTypes: TraversableOnce[PieceType],
                                validSpaces: TraversableOnce[String]): Set[String] = {
    val actualNum = numToPlace min game.outOfPlay.totalOf(pieceTypes)
    val validNames = validSpaces.toSet
    val isValid = (sp: Space) => validNames(sp.name)
    val canTakeBase  = (sp: Space) => isValid(sp) && sp.totalBases < 2
    val desc = andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanPlacement(numRemaining: Int): Unit = if (numRemaining > 0) {
      val bases      = game.outOfPlay.only(pieceTypes).only(BasePieces)
      val forces     = game.outOfPlay.only(pieceTypes).except(BasePieces)
      val candidates = if (forces.nonEmpty)
        spaceNames(game.spaces filter isValid)
      else
        spaceNames(game.spaces filter canTakeBase)

      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc placed: ${actualNum - numRemaining} of ${actualNum}")
        val name      = askCandidate(s"Place $desc in which space: ", candidates)
        val sp        = game.getSpace(name)
        val placeBase = bases.nonEmpty && canTakeBase(sp) &&
                        askYorN(s"Do you wish to place a base in $name? (y/n) ")
        val pieces    = if (placeBase)
          askPieces(bases, 1)
        else {
          val num = askInt(s"\nPlace how many pieces in $name", 0, numRemaining)
          askPieces(forces, num)
        }
        if (pieces.nonEmpty) {
          println()
          placePiecesFromOutOfPlay(name, pieces)
          spacesUsed += name
        }
        nextHumanPlacement(numRemaining - pieces.total)
      }
    }

    def nextBotPlacement(numRemaining: Int, oopPieces: Pieces): Unit = if (numRemaining > 0 && oopPieces.nonEmpty) {
      val piece = Bot.selectFriendlyToPlaceOrMove(oopPieces, 1)
      val optSpace = if (piece.has(BasePieces)) {
        val candidates = game.spaces filter canTakeBase
        if (candidates.nonEmpty)
          Some(Bot.pickSpacePlaceBases(faction)(candidates))
        else
          None
      }
      else {
        val candidates = game.spaces filter isValid
        if (candidates.nonEmpty) {
          val isTroop = piece.has(USTroops::NVATroops::ARVNTroops::Nil)
          Some(Bot.pickSpacePlaceForces(faction, isTroop)(candidates))
        }
        else
          None
      }
      optSpace match {
        case Some(sp) =>
          placePiecesFromOutOfPlay(sp.name, piece)
          spacesUsed += sp.name
          nextBotPlacement(numRemaining - 1, oopPieces - piece)
        case None =>
          // It is possible that there are base available but no
          // space can accomodate a base, so remove the piece from
          // consideration and continue
          nextBotPlacement(numRemaining, oopPieces - piece)
      }
    }

    if (actualNum > 0) {
      loggingControlChanges {
        if (game.isHuman(faction))
          nextHumanPlacement(actualNum)
        else
          nextBotPlacement(actualNum, game.outOfPlay.only(pieceTypes))
      }
    }
    spacesUsed
  }

  // Move the given number pieces from one or more spaces on the map to
  // the given destination space.
  def moveMapPiecesToSpace(
    faction: Faction,
    numToMove: Int,
    mandatory: Boolean,
    destName: String,
    pieceTypes: TraversableOnce[PieceType],
    onlyFrom: Option[Set[String]] = None): Unit = {
    val hasPieces = (sp: Space) => {
      onlyFrom match {
        case Some(validNames) =>
          sp.name != destName && validNames(sp.name) && sp.pieces.has(pieceTypes)
        case None =>
          sp.name != destName && sp.pieces.has(pieceTypes)
      }
    }
      
    val desc = andList(pieceTypes)

    def nextHumanMove(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter hasPieces)
      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc moved to $destName: ${numToMove - numRemaining} of ${numToMove}")
        val name   = askCandidate("Move pieces from which space: ", candidates)
        val sp     = game.getSpace(name)
        val pieces = sp.pieces.only(pieceTypes)
        val num    = askInt(s"Move how many pieces from $name", 0, numRemaining min pieces.total)
        val toMove = askPieces(pieces, num)
        println()
        movePieces(toMove, name, destName)
        nextHumanMove(numRemaining - num)
      }
    }

    loggingControlChanges {
      if (game.isHuman(faction)) {
        val maxNum = (game.spaces filter hasPieces map (_.pieces.totalOf(pieceTypes))).sum
        val num = if (mandatory)
          numToMove
        else
          askInt(s"\nHow many $desc do you wish to move", 0, maxNum min numToMove)
        nextHumanMove(num)
      }
      else
        Bot.doEventMoveTo(destName, faction, numToMove, mandatory, pieceTypes.toSet, onlyFrom)
    }
  }

}


