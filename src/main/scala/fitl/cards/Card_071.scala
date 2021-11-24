
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
// ARVN stand firm: In a space in the South with ARVN,
// remove all NVA Troops and place 3 ARVN Troops.
//
// Shaded Text
// Conventional thrust: NVA free Marches Troops into a City
// and free Attacks there twice.
//
// Tips
// "The South" means any South Vietnam spaces, including all LoCs.
//  The NVA March could occur even during Monsoon and costs 0 but
//  otherwise follows all March rules, including repeated moves through
//  Laos/Cambodia if the Trail is at greater than 0 and including possibly
//  Activating moving Guerrillas (3.3.3). The Attacks also follow the usual
//  rule, including either using NVA Troops or Activating Attacking Guerrillas.

object Card_071 extends EventCard(71, "An Loc",
  DualEvent,
  List(ARVN, NVA, US, VC),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    isInSouthVietnam(sp.name) &&
    sp.pieces.has(ARVNPieces)
    
  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate

    val name = if (game.isHuman(faction))
      askCandidate("\nChoose a space in South Vietname with ARVN pieces: ", spaceNames(candidates))
    else
      Bot.pickSpaceWithMostPieces(Set(NVATroops))(candidates).name

    val sp = game.getSpace(name)
    val nvaTroops = sp.pieces.only(NVATroops)
    val numArvnAvail  = game.availablePieces.totalOf(ARVNTroops)
    loggingControlChanges {
      val numArvnTroops = if (game.isHuman(faction) && isCoin(faction))
        Human.numToPlace(ARVNTroops, 3)  // Allow voluntary removal if necessary
      else
        numArvnAvail min 3
  
      println()
      removePieces(name, nvaTroops)
      placePieces(name, Pieces(arvnTroops = numArvnTroops))
    }
  }

  def botCandidate = (sp: Space) =>
    sp.isCity &&
    (spaces(NVA_Bot.getNVAAdjacent(sp.name, free = true)) exists (_.pieces.has(NVATroops)))

  def botPickCity(candiates: List[Space]): String = {
    val sorted = candiates.sortBy { sp =>
      (spaces(NVA_Bot.getNVAAdjacent(sp.name, free = true)) map (_.pieces.totalOf(NVATroops))).sum
    }
    sorted.last.name
  }
  // Make sure there is at least one city that can reached by NVA Troops
  def shadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists botCandidate

  def nvaMarchToCity(name: String): Unit = {
    val params = Params(event = true, free = true)
    var marched = false
    Bot.movePiecesToDestinations(NVA, March, Set(NVATroops), false, params) {
      (_, _, _) => if (marched)
        None
      else {
        marched = true
        Some(name)
      }
    }
  }


  def executeShaded(faction: Faction): Unit = {
    if (game.isHuman(NVA)) {
      val choices = spaceNames(game.nonLocSpaces filter (_.isCity))
      val name = askSimpleMenu(choices, "\nChoose a city: ").head
      val dests = if (name == AnLoc && game.trail > TrailMin)
        (AnLoc::LaosCambodia).toSet
      else
        Set(name)
      val params = Params(
        event       = true,
        free        = true,
        onlyIn      = Some(dests),
        marchParams = MarchParams(onlyTypes = Set(NVATroops))
      )

      Human.executeMarch(NVA, params)
      if (game.getSpace(name).pieces.has(NVAForces)) {
        Human.performAttack(name, NVA, free = true)
        Human.performAttack(name, NVA, free = true)
      }
    }
    else {
      val candidates = game.nonLocSpaces filter botCandidate
      val name       = botPickCity(candidates)

      nvaMarchToCity(name)

      val attackParams = Params(
        event       = true,
        free        = true,
        onlyIn      = Some(Set(name)),
        maxSpaces   = Some(1)
      )

      NVA_Bot.attackOp(attackParams, actNum = 6, addAmbush = false)
      NVA_Bot.attackOp(attackParams, actNum = 6, addAmbush = false)
    }
  }
}
