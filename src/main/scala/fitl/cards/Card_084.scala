
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
// Fear of Northern reprisal: Place 1 ARVN Troop and 1 Police in each
// South Vietnam space with NVA.
//
// Shaded Text
// Thoroughly penetrated: ARVN remove 1 in 3 cubes (round down) each space.
// Place a VC Guerrilla in 3 spaces where ARVN removed.
//
// Tips
// For the shaded version, ARVN selects and must remove 1 in 3 of their own cubes
// (ARVN Troops or Police), not US or NVA. The executing Faction chooses which candidate
// spaces to place the VC Guerrillas.

object Card_084 extends EventCard(84, "To Quoc",
  DualEvent,
  List(ARVN, VC, US, NVA),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    isInSouthVietnam(sp.name) &&
    sp.pieces.has(NVAPieces)

  def unshadedEffective(faction: Faction): Boolean = false  // Bots never execute unshaded event

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate
    if (candidates.isEmpty)
      log("There are no NVA pieces in South Vietnam")
    else 
      loggingControlChanges {
        for (sp <- candidates) {
          if (!game.availablePieces.has(ARVNTroops))
            println(s"\nPlacing an ARVN Troop in ${sp.name}")
          val numTroops = Human.numToPlace(ARVNTroops, 1)
          if (!game.availablePieces.has(ARVNPolice))
            println(s"\nPlacing an ARVN Police in ${sp.name}")
          val numPolice = Human.numToPlace(ARVNPolice, 1)
          val cubes     = Pieces(arvnTroops = numTroops, arvnPolice = numPolice)
          placePieces(sp.name, cubes)
        }
      }
  }

  val shadedCandidate = (sp: Space) => sp.pieces.totalOf(ARVNCubes) >= 3

  def removeArvnCubes(candiates: List[Space]): Unit = candiates match {
    case Nil =>
    case sp::rest =>
      val cubes = sp.pieces.only(ARVNCubes)
      val num   = cubes.total / 3
      val toRemove = if (game.isHuman(ARVN))
        askPieces(cubes, num, prompt = Some(s"\nSelect cubes to remove from ${sp.name}"))
      else
        Bot.selectFriendlyRemoval(cubes, num)

      if (game.isHuman(ARVN))
        println()
      removePieces(sp.name, toRemove)
      removeArvnCubes(candiates filterNot (_.name == sp.name))
  }

  def placeVCGuerrillas(faction: Faction, candidates: List[Space]): Unit = {
    val numAvailVC = game.availablePieces.totalOf(VCGuerrillas_U)
    val selectedSpaces = if (candidates.size <= 3)
      spaceNames(candidates)
    else if (game.isHuman(faction)) {
      val prompt = "\nSelect 3 spaces to place a VC Guerrilla:"
      askSimpleMenu(spaceNames(candidates), prompt, numChoices = 3)
    }
    else if (faction == NVA)
      spaceNames(Bot.pickSpaces(3 min numAvailVC, candidates)(Bot.pickSpaceWithMostSupport))
    else // VC
      spaceNames(Bot.pickSpaces(3 min numAvailVC, candidates)(VC_Bot.pickSpacePlaceGuerrillas))

    for (name <- selectedSpaces) {
      val num = if (game.isHuman(faction))
        Human.numToPlace(VCGuerrillas_U, 1)
      else
        1
      placePieces(name, Pieces(vcGuerrillas_U = num))
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.spaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.spaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces with 3 or more ARVN cubes")
    else
      loggingControlChanges {
        removeArvnCubes(candidates)
        placeVCGuerrillas(faction, candidates)
      }
  }
}
