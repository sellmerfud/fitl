
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
// Riverines hunt Charlie: Remove all VC or all non-Troop NVA from
// a Lowland with US Troops.
//
// Shaded Text
// VC river ambush: In a Lowland with any VC, remove a die roll of US/ARVN
// cubes (US to Casualties). Place 1 VC piece.
// 
// Tips
// "A Lowland" means 1 Lowland Province. A "piece" can include a Base.
// "US to Casualties" means that any US pieces removed go to the Casualties box,
// while ARVN pieces removed go to ARVN Available Forces as usual (1.4.1).

object Card_100 extends EventCard(100, "Rach Ba Rai",
  DualEvent,
  List(VC, US, ARVN, NVA),
  ListMap(US   -> (Performed -> Unshaded),
          ARVN -> (Performed -> Unshaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Critical  -> Shaded))) {

  val NVANonTroops = NVAPieces.toSet - NVATroops

  val unshadedCandidate = (sp: Space) =>
    sp.isLowland &&
    sp.pieces.has(USTroops) &&
    sp.pieces.has(VCPieces.toSet ++ NVANonTroops)

  def unshadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nChoose space to carry out the event:").head
      else
        Bot.pickSpaceRemoveReplace(faction)(candidates).name

      val sp     = game.getSpace(name)
      val numVC  = sp.pieces.totalOf(VCPieces)
      val numNVA = sp.pieces.totalOf(NVANonTroops)
      val removeTypes = if (numVC > 0 && numNVA > 0) {
        if (game.isHuman(faction)) {
          val f = askFaction("\nRemove pieces of which faction:", Set(NVA, VC))
          if (f == NVA) NVANonTroops else VCPieces.toSet  
        }
        else if (numNVA >= numVC)
          NVANonTroops
        else
          VCPieces.toSet
      }
      else if (numVC > 0)
        VCPieces.toSet
      else
        NVANonTroops

      val toRemove = sp.pieces.only(removeTypes)
      println()
      removePieces(name, toRemove)
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isLowland &&
    sp.pieces.has(VCPieces)

  val withCubes = (sp: Space) =>
    shadedCandidate(sp) &&
    sp.pieces.has(CoinCubes)

  def shadedEffective(faction: Faction): Boolean =
    (game.availablePieces.has(VCPieces) && (game.nonLocSpaces exists shadedCandidate)) ||
    (game.nonLocSpaces exists withCubes)

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val canPlaceG = game.availablePieces.has(VCGuerrillas_U)
      val canPlaceB = game.availablePieces.has(VCBase)
      val canPlace  = canPlaceG || canPlaceB       

      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nChoose space to carry out the event:").head
      else {
        val removeCandidates = game.nonLocSpaces filter withCubes
  
        if (removeCandidates.nonEmpty && (faction == NVA || !canPlace))
          Bot.pickSpaceRemoveReplace(faction)(removeCandidates).name
        else if (canPlaceG)
          VC_Bot.pickSpacePlaceGuerrillas(candidates).name
        else
          VC_Bot.pickSpacePlaceBases(candidates).name
      }

      val die = d6
      val cubes = game.getSpace(name).pieces.only(CoinCubes)

      if (cubes.nonEmpty)
        log(s"\nRolling d6 to determine number of losses: $die")

      val (toRemove, toPlace) = if (game.isHuman(faction)) {
        val toRemove = askPieces(cubes, die, prompt = Some("\nSelect cubes to remove"))
        val toPlace  = if (canPlace) {
          val choices = List(
            choice(canPlaceG, Pieces(vcGuerrillas_U = 1), "VC Guerrilla"),
            choice(canPlaceB, Pieces(vcBases = 1),        "VC Base")).flatten
          askMenu(choices, "\nChoose piece to place").head
        }
        else
          Pieces()
        (toRemove, toPlace)
      }
      else {
        val toRemove = Bot.selectEnemyRemoveReplaceActivate(cubes, die)
        val toPlace = if (canPlaceG)      Pieces(vcGuerrillas_U = 1)
                      else if (canPlaceB) Pieces(vcBases = 1)
                      else                Pieces()
        (toRemove, toPlace)
      }

      println()
      loggingControlChanges {
        removePieces(name, toRemove)
        placePieces(name, toPlace)
      }
    }
  }
}
