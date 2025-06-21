
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
// Premature conventional buildup: In each of any 3 spaces,
// replace any 2 Guerrillas with 1 NVA Troop.
//
// Shaded Text
// Military strategist: NVA free Marches into up to 3 spaces then executes
// any 1 free Op or Special Activity within each, if desired.
//
// Tips
// The 3 spaces must be 3 different spaces, not the same space more than
// once. The NVA would March even during Monsoon.

object Card_056 extends EventCard(56, "Vo Nguyen Giap",
  DualEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Ignored -> Shaded))) {

  val unshadedCandidate = (sp: Space) => sp.pieces.has(Guerrillas)

  def unshadedEffective(faction: Faction): Boolean = false // Not executed by Bots

  def executeUnshaded(faction: Faction): Unit = {
    // Set the order so that we ask for VC pieces first!
    val GTypes = List(VCGuerrillas_A, VCGuerrillas_U, NVAGuerrillas_A, NVAGuerrillas_U)
    val candidates = spaceNames(game.spaces filter unshadedCandidate)
    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 3)
      candidates
    else {
      val choices = candidates map (n => n -> n)
      askMenu(choices, "\nSelect 3 spaces to replace guerrillas with an NVA Troop", numChoices = 3)
    }

    loggingControlChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces that qualify for the event")
      else
        for (name <- selectedSpaces) {
          println()
          val pieces    = game.getSpace(name).pieces
          val toReplace = askPieces(pieces, 2, GTypes, Some(s"Select guerrillas to replace in $name"))
          val numTroops = Human.numToPlace(NVATroops, 1)

          removeToAvailable(name, toReplace)
          placePieces(name, Pieces(nvaTroops = numTroops))
        }
    }
  }

  def shadedEffective(faction: Faction): Boolean = false // Not executed by Bots

  def executeShaded(faction: Faction): Unit = {

    def doAction(name: String): Unit = {
      val params = Params(
        event     = true,
        free      = true,
        onlyIn    = Some(Set(name)),
        maxSpaces = Some(1)
      )
      val bombardParams = Params(
        event     = true,
        free      = true,
        onlyIn    = Some(Set(name)),
        maxSpaces = Some(1)
      )
      val sp = game.getSpace(name)
      val canRally =
        !sp.isLoC &&
        (sp.population == 0 || sp.support <= Neutral)
      val canAttack =
        (sp.pieces.has(NVAGuerrillas) || sp.pieces.has(NVATroops)) &&
        sp.pieces.has(CoinPieces)
      val canTerror =
        (sp.pieces.has(NVAGuerrillas_U) || sp.pieces.has(NVATroops))
      val canInfiltrate =
        sp.pieces.has(NVABases) ||
        (sp.pieces.totalOf(NVAPieces) > sp.pieces.totalOf(VCPieces))
      val canBombard = {
        val haveBombers = (name :: getAdjacent(name).toList)
          .map(game.getSpace)
          .exists(_.pieces.totalOf(NVATroops) >= 3)
        haveBombers &&
        (sp.pieces.has(CoinBases) || sp.pieces.totalOf(CoinTroops) >= 3)
      }
      val canAmbush = sp.pieces.has(NVAGuerrillas_U)



      Human.initTurnVariables(false)
      val opChoices = List(
        choice(canRally, Rally, "Rally Operation"),
        choice(true, March, "March Operation"),
        choice(canAttack, Attack, "Attack Operation"),
        choice(canTerror, Terror, "Terror Operation"),
        choice(canInfiltrate, Infiltrate, "Infiltrate Activity"),
        choice(canBombard, Bombard, "Bombard Activity"),
        choice(canAmbush, Ambush, "Ambush Activity"),
        choice(true, "cancel", "None"),
      ).flatten
      askMenu(opChoices, s"\nChoose Operation/Activity to perform in $name").head match {
        case Rally      => Human.executeRally(NVA, params)
        case March      => Human.executeMarch(NVA, params)
        case Attack     => Human.executeAttack(NVA, params)
        case Terror     => Human.executeTerror(NVA, params)
        case Infiltrate => Human.doInfiltrate(params)
        case Bombard    => Human.doBombard(bombardParams)
        case Ambush     => Human.performAmbush(name, NVA, March, free = true)
        case _          => // cancel
      }
    }


    def nextAction(candidates: List[String]): Unit = if (candidates.nonEmpty) {
      val choices = (candidates map (n => n -> n)) :+ ("finished" -> "Finished performing Ops/Activities")
      askMenu(choices, "\nPeform an Operation or Activity in which space:").head match {
        case "finished" =>
        case name =>
          doAction(name)
          nextAction(candidates filterNot (_ == name))
      }
    }

      val marchParams = Params(
        event     = true,
        free      = true,
        maxSpaces = Some(3)
      )

      val destinations = Human.executeMarch(NVA, marchParams)
      nextAction(destinations.toList.sorted(SpaceNameOrdering))
  }
}
