
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
// I Corps Commander: Place 3 ARVN pieces within 3 spaces of Hue.
// Shift receiving spaces each 1 level toward Active Support.
//
// Shaded Text
// Popular general relieved: Replace any 2 ARVN with any 2 VC pieces
// within 2 spaces of Hue. Patronage +4 or -4.
//
// Tips
// A LoC or Province with 0 Population cannot shift (1.6).



object Card_087 extends EventCard(87, "Nguyen Chanh Thi",
  DualEvent,
  List(ARVN, VC, NVA, US),
  ListMap(US   -> (Critical  -> Unshaded),
          ARVN -> (Critical  -> Shaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Performed -> Shaded))) {

  val unshadedSpaces = spaces(spacesWithin(3, Hue)) filterNot (_.isNorthVietnam)
  val shiftCandidate = (sp: Space) =>
    sp.canHaveSupport &&
    sp.support != ActiveSupport

  // US will only execute if at least one space can be shifted
  // toward Active Support
  def unshadedEffective(faction: Faction): Boolean = faction match {
    case ARVN => game.availablePieces.has(ARVNPieces)
    case _    => game.availablePieces.has(ARVNPieces) &&
                 (unshadedSpaces exists shiftCandidate)
  }

  def executeUnshaded(faction: Faction): Unit = {
    var placedSpaces = Set.empty[String]

    def placeArvnPieces(): Unit = {
      if (game.isHuman(faction)) {
        val maxToPlace = game.piecesToPlace.totalOf(ARVNPieces) min 3
  
        def nextPlacement(count: Int): Unit = if (count <= maxToPlace) {
          val arvnAvail = game.availablePieces.only(ARVNPieces)
          val arvnPieces = game.piecesToPlace.only(ARVNPieces)
          def askVolRemoval: Boolean = {
            println(s"\nPlacing ${ordinal(count)} ARVN piece:")
            askYorN("There a no ARVN pieces in the Available box, do you wish to continue? (y/n) ")
          }
          
          if (arvnAvail.nonEmpty || askVolRemoval) {
            val name = askSimpleMenu(spaceNames(unshadedSpaces), s"\nChoose space for ${ordinal(count)} ARVN piece:").head
            val typeChoices = {
              val types = if (game.getSpace(name).totalBases < 2)
                arvnPieces.getTypes.distinct
                else
                  arvnPieces.getTypes.distinct filterNot (_ == ARVNBase)
              types map (t => t -> t.genericSingular)
            }
            val arvnType    = askMenu(typeChoices, "\nChoose type of piece to place:").head
            if (!arvnAvail.has(arvnType))
              voluntaryRemoval(1, arvnType)
            placePieces(name, Pieces().set(1, arvnType))
            placedSpaces += name
            nextPlacement(count + 1)
          }
        }
        nextPlacement(1)
      }
      else {
        def nextPlacement(numRemaining: Int, availArvn: Pieces): Unit = if (numRemaining > 0) {
          val ignoreBases = (unshadedSpaces forall (_.totalBases == 2))
          val considered = if (ignoreBases) availArvn.except(ARVNBase) else availArvn
          val piece      = Bot.selectFriendlyToPlaceOrMove(considered, 1)
          val isBase     = piece.has(ARVNBase)
          val candidates = if (isBase)
            unshadedSpaces filter (_.totalBases < 2)
          else
            unshadedSpaces
          val allActive  = candidates forall (_.support == ActiveSupport)
          val sp = if (allActive) {
            if (isBase)
              US_Bot.pickSpacePlaceBases(candidates)
            else
              US_Bot.pickSpacePlaceCubesSpecialForces(false)(candidates)
          }
          else
              US_Bot.pickSpaceTowardActiveSupport(candidates)

          if (piece.nonEmpty) {
            placePieces(sp.name, piece)
            placedSpaces += sp.name
          }
          nextPlacement(numRemaining - 1, availArvn - piece)
        }

        val availArvn  = game.availablePieces.only(ARVNPieces)
        val maxToPlace = availArvn.total min 3
        nextPlacement(maxToPlace, availArvn)
      }
    }

    loggingControlChanges {
      placeArvnPieces()
      log("\nShift spaces 1 level toward Active Support")
      log(separator())
      for (name <- placedSpaces)
        increaseSupport(name, 1)
    }
  }

  val shadedSpaces = spaces(spacesWithin(2, Hue))
  val shadedCandidate = (sp: Space) => sp.pieces.has(ARVNPieces)

  def shadedEffective(faction: Faction): Boolean = faction match {
    case ARVN => game.patronage < EdgeTrackMax
    case NVA  => game.patronage > 0
    case _    => game.patronage > 0 ||
                 ((shadedSpaces exists shadedCandidate) && game.availablePieces.has(VCPieces))
  }

  def executeShaded(faction: Faction): Unit = {

    def replaceARVNPieces(): Unit = {
      if (game.isHuman(faction)) {
        val maxReplace = game.piecesToPlace.totalOf(VCPieces) min 2

        def nextReplacement(count: Int): Unit = if (count <= maxReplace) {
          def haveReplacement: Boolean = game.availablePieces.has(VCPieces) || {
            println(s"\nReplacing ${ordinal(count)} ARVN piece:")
            askYorN("\nThere are no VC pieces in the Available box, do you wish to continue? (y/n) ")
          }
          val candidates = spaceNames(shadedSpaces filter shadedCandidate)
          if (candidates.nonEmpty && haveReplacement) {
            val name        = askSimpleMenu(candidates, "\nReplace ARVN piece in which space:").head
            val sp          = game.getSpace(name)
            val arvnPiece   = askPieces(sp.pieces.only(ARVNPieces), 1)
            val desc        = arvnPiece.getTypes.head.singular
            val ignoreBases = sp.totalBases == 2 && !arvnPiece.has(ARVNBase)
            val vcType     = if (ignoreBases)
              VCGuerrillas_U
            else
              askPieceType(s"\nReplace $desc with what:", List(VCGuerrillas_U, VCBase))
            if (!game.availablePieces.has(vcType))
              voluntaryRemoval(1, vcType)

            log()
            removePieces(name, arvnPiece)
            placePieces(name, Pieces().set(1, vcType))
            nextReplacement(count + 1)
          }
        }

        nextReplacement(1)
      }
      else {
        def nextReplacement(numRemaining: Int, availVC: Pieces): Unit = if (numRemaining > 0  && availVC.nonEmpty) {
          val candidates = shadedSpaces filter shadedCandidate
          if (candidates.nonEmpty) {
            val sp = if (faction == ARVN)
              Bot.pickSpaceRemoveFriendlyPieces(candidates, ARVNPieces)
            else
              VC_Bot.pickSpaceRemoveReplace(candidates)
            val arvnPieces = sp.pieces.only(ARVNPieces)
            val arvnPiece = if (faction == ARVN)
              Bot.selectFriendlyRemoval(arvnPieces, 1)
            else
              Bot.selectEnemyRemoveReplaceActivate(arvnPieces, 1)

            val vcPieces = if (sp.totalBases == 2 && !arvnPiece.has(ARVNBase))
              availVC.except(VCBase)
            else
              availVC

            val vcPiece = if (faction == ARVN)
              Bot.selectEnemyPlacement(availVC, 1)
            else
              Bot.selectFriendlyToPlaceOrMove(availVC, 1)

            log()
            removePieces(sp.name, arvnPiece)
            placePieces(sp.name, vcPiece)
            nextReplacement(numRemaining - 1, availVC - vcPiece)
          }
        }

        nextReplacement(2, game.availablePieces.only(VCPieces))
      }
    }

    
    val patChoices = List(true -> "Increase Patronage +4", false -> "Decrease Patronage -4")

    loggingControlChanges {
      replaceARVNPieces()
      val incPatronage = if (game.isHuman(faction))
        askMenu(patChoices, "\nChoose one:").head
      else
        (faction == ARVN)  // Bot choice

      log()
      if (incPatronage)
        increasePatronage(4)
      else
        decreasePatronage(4)
    }
  }
}
