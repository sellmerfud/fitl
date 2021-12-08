
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
// CIDG interdict NVA: Remove any 3 NVA pieces from a space
// with or adjacent to a COIN Base.
//
// Shaded Text
// Tay Nguyen offensive: NVA free March from any spaces outside
// South Vietnam, then free Attack or Ambush any 1 space.
//
// Tips
// "COIN" means US or ARVN. For the unshaded text, "a space" means the
// pieces must be removed from 1 single space. "Pieces" can include Bases.
//
// For shaded, the NVA can March repeatedly from Laos/Cambodia spaces—at cost 0—If
// the Trail is at 1 or more and would March even during Monsoon. Any Ambushes must
// occur in where there is at least one NVA Guerrilla to Activate (4.4.3), but need
// not be in March destinations nor involve Guerrillas that Marched and could occur
// even if all NVA Guerrillas there were already Active.

object Card_059 extends EventCard(59, "Plei Mei",
  DualEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  def adjacentToCoinBase(name: String) =
    spaces(getAdjacentNonLOCs(name)) exists (_.pieces.has(CoinBases))

  val unshadedCandidate = (sp: Space) =>
    sp.pieces.has(NVAPieces) &&
    (sp.pieces.has(CoinBases) || adjacentToCoinBase(sp.name))

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else if (game.isHuman(faction)) {
      val name = askCandidate("\nRemove NVA pieces from which space: ", spaceNames(candidates))
      val sp = game.getSpace(name)
      val toRemove = askPieces(sp.pieces.only(NVAPieces), 3, prompt = Some("Select NVA pieces to remove:"))
      removePieces(name, toRemove)
    }
    else {  // Bots
      val priorities = List(new Bot.HighestScore[Space]("Most NVA pieces", _.pieces.totalOf(NVAPieces)))
      val sp = Bot.pickSpaceRemoveReplace(faction)(Bot.narrowCandidates(candidates, priorities))
      val toRemove = Bot.selectEnemyRemoveReplaceActivate(sp.pieces.only(NVAPieces), 3)
      removePieces(sp.name, toRemove)
    }
  }

  private val isAttackSpace = (sp: Space) =>
      sp.pieces.has(NVAForces) &&
      sp.pieces.has(CoinPieces)

  // Note: The ambush for this event does not require an
  //       underground guerrilla.  So any NVA guerrilla will do.
  private val isAmbushSpace = (sp: Space) => {
    sp.pieces.has(NVAGuerrillas) &&
    ambushTargets(sp.name).nonEmpty
  }


  def shadedEffective(faction: Faction): Boolean = false // Not executed by Bots

  private def doAttack(): Unit = {
    val candidates = spaceNames(game.spaces filter isAttackSpace)
    if (candidates.isEmpty)
      log("\nThere are no spaces where NVA can successfully attack")
    else {
      val name = askCandidate("\nAttack in which space: ", candidates)
      Human.performAttack(name, NVA, free = true)
    }
  }

  private def doAmbush(): Unit = {
    val candidates = spaceNames(game.spaces filter isAmbushSpace)
    if (candidates.isEmpty)
      log("\nThere are no spaces where NVA can successfully ambush")
    else {
      val name = askCandidate("\nAmbush using which space: ", candidates)
      Human.performAmbush(name, NVA, March, free = true)
    }
  }


  def executeShaded(faction: Faction): Unit = {
    val params = Params(
      event = true,
      free  = true,
      march = MarchParams(onlyFrom = (NorthVietnam::LaosCambodia).toSet)
    )
    val choices = List(
      "attack" -> "Attack any one space",
      "ambush" -> "Ambush any one space"
    )

    log("NVA free marches from outside South Vietnam")
    Human.executeMarch(NVA, params)
    askMenu(choices, "\nChoose one:").head match {
      case "attack" => doAttack()
      case _        => doAmbush()
    }
  }
}
