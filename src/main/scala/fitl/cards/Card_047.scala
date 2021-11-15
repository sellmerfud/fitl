
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
// Southerners resist invasion: Add ARVN Troops to double the ARVN
// pieces in a space with NVA. All ARVN free Assault NVA.
//
// Shaded Text
// NVA professional soldiers: Place up to 10 NVA Troops anywhere
// within 1 space of North Vietnam.
//
// Tips
// For the unshaded Event, in the selected space, count the number of
// ARVN pieces (including Bases) and place that number of ARVN Troops
// there from Available. Then ARVN Troops and Police Assault to remove
// NVA (only) in each map space with both ARVN and Active NVA. For the
// shaded Event, place the Troops anywhere among North Vietnam, Central Laos,
// Quang Tri, and the LoC from Khe Sanh to Hue (Highway 9)—even where Support
// or COIN Control.

object Card_047 extends EventCard(47, "Chu Luc",
  DualEvent,
  List(NVA, ARVN, VC, US),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  val doubleCandidate = (sp: Space) =>
    sp.pieces.has(NVAPieces) && sp.pieces.has(ARVNPieces)

  val nvaAssaultEffective = assaultEffective(ARVN, false, Set(NVA)) _

  def unshadedEffective(faction: Faction): Boolean =
    (game.availablePieces.has(ARVNTroops) && (game.spaces exists doubleCandidate)) ||
    (game.spaces exists nvaAssaultEffective)

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter doubleCandidate

    if (candidates.isEmpty)
      log("There are no spaces with both NVA and ARVN pieces")
    else {
      val name = if (game.isHuman(faction))
        askCandidate("Add ARVN Troops to which space: ", spaceNames(candidates))
      else
        Bot.pickSpacePlaceForces(faction, troops = true)(candidates).name

      val sp = game.getSpace(name)
      val num = sp.pieces.totalOf(ARVNPieces) min game.availablePieces.totalOf(ARVNTroops)
      placePieces(name, Pieces(arvnTroops = num))
    }

    log()
    val assaultSpaces = game.spaces filter nvaAssaultEffective
    if (assaultSpaces.isEmpty)
      log("There are no spaces where an ARVN Assault on NVA would be effective")
    else {
      val params = Params(
        event = true,
        free  = true,
        assaultParams = AssaultParams(
             specificSpaces = spaceNames(assaultSpaces).toSet,
             onlyTarget     = Some(NVA))
      )

      if (game.isHuman(faction))
        Human.executeAssault(ARVN, params)
      else
        ARVN_Bot.assaultOp(params, 0)
    }
  }

  def shadedEffective(faction: Faction): Boolean = false  // Bot never executes

  def executeShaded(faction: Faction): Unit = {
    placePiecesOnMap(faction, 10, Set(NVATroops), getAdjacent(NorthVietnam) + NorthVietnam)
  }
}
