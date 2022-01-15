
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
// US LT convicted: 2 Available US Troops out of play. Patronage +2.
//
// Shaded Text
// Massacre: Set a Province with US Troops to Active Opposition.
// VC place a Base and Guerrilla there. Aid –6.
//
// Tips
// Provinces with 0 Population do not shift from Neutral.

object Card_119 extends EventCard(119, "My Lai",
  DualEvent,
  List(VC, ARVN, NVA, US),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(USTroops) ||
    game.patronage < EdgeTrackMax

  def executeUnshaded(faction: Faction): Unit = {
    val numTroops = game.availablePieces.totalOf(USTroops) min 2

    loggingPointsChanges {
      if (numTroops == 0)
        log("There are no available US Troops")
      else
        moveAvailableToOutOfPlay(Pieces(usTroops = numTroops))
      increasePatronage(2)
    }
  }

  val provinceWithUSTroops = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(USTroops)

  val notActiveOppositionCandidate = (sp: Space) =>
      provinceWithUSTroops(sp) &&
      sp.canHaveSupport &&
      sp.support != ActiveOpposition

  def shadedEffective(faction: Faction): Boolean =
    (game.nonLocSpaces exists notActiveOppositionCandidate) ||
    ((game.nonLocSpaces exists provinceWithUSTroops) && game.availablePieces.has(VCPieces)) ||
    (game.trackResources(ARVN) && game.usAid > 0)


  def executeShaded(faction: Faction): Unit = {
    val candidates          = game.nonLocSpaces filter provinceWithUSTroops
    val activeOppCandidates = game.nonLocSpaces filter notActiveOppositionCandidate

    val name = if (candidates.isEmpty)
      ""
    else if (game.isHuman(faction))
      askSimpleMenu(spaceNames(candidates), "\nChoose a Province with US Troops:").head
    else if (activeOppCandidates.nonEmpty)
      VC_Bot.pickSpaceTowardActiveOpposition(activeOppCandidates).name
    else if (game.availablePieces.has(VCBase))
      VC_Bot.pickSpacePlaceBases(candidates).name
    else
      VC_Bot.pickSpacePlaceGuerrillas(candidates).name

      
    def piecesToPlace(canPlaceBase: Boolean): Pieces = {
      val canVolRemove = faction == VC && game.isHuman(VC)
      val placeBase = canPlaceBase &&
                      game.availablePieces.has(VCBase) || (canVolRemove && Human.numToPlace(VCBase, 1) == 1)
      val placeG    = game.availablePieces.has(VCGuerrillas_U) || (canVolRemove && Human.numToPlace(VCGuerrillas_U, 1) == 1)

      Pieces(
        vcBases        = if (placeBase) 1 else 0,
        vcGuerrillas_U = if (placeG)    1 else 0
      )
    }

    loggingControlChanges {
      if (name == "")
        log("There are no Provinces with US Troops")
      else {
        val sp = game.getSpace(name)
        println()
        setSupport(name, ActiveOpposition)
        placePieces(name, piecesToPlace(sp.canTakeBase))
      }
      decreaseUsAid(6)
    }
  }
}
