
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
// Lon Nol deposes Sihanouk: US free Air Lifts into and US or ARVN free Sweeps
// within Cambodia. Remove 2 NVA/VC Bases from Cambodia.
//
// NVA invades Cambodia: NVA places a total of 12 NVA Troops and Guerrillas in Cambodia.
//
// Tips
// For the unshaded effect, US would decide the details of the Air Lift and whichever Faction
// was Sweeping would decide the Sweep’s details, including whether to move among Cambodia spaces.
// The Sweep could occur even during Monsoon. Tunneled Bases could not be removed (1.4.4, 5.1.1).

object Card_062 extends EventCard(62, "Cambodian Civil War",
  DualEvent,
  List(ARVN, US, NVA, VC),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Ignored -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    spaces(Cambodia) exists (_.pieces.has(InsurgentNonTunnels))

  val mostTroops = List(new Bot.HighestScore[Space]("Most NVA Troops", _.pieces.totalOf(NVATroops)))
  val hasUsTroops    = (sp: Space) => sp.pieces.has(USTroops)
  val hasArvnTroops  = (sp: Space) => sp.pieces.has(ARVNTroops)

  case class Origin(name: String, forces: Pieces)

  // Returns the Ranger Origins and Irregular Origins
  def getOrigins(dest: String): (List[Origin], List[Origin]) = {
    val (rangers, irregs) = (game.spaces flatMap { sp =>
      val keep = Bot.selectPiecesToKeep(sp.name, dest, US, AirLift, SpecialForces.toSet, Params())
      val movers = if (isInCambodia(sp.name) && sp.pieces.has(NVATroops))
        Pieces()
      else
      (sp.pieces - keep).only(SpecialForces)

      if (movers.nonEmpty)
        Some((Origin(sp.name, movers.only(Rangers)), Origin(sp.name, movers.only(Irregulars))))
      else
        None
    }).unzip

    val rangerOrigins = (rangers filter (_.forces.nonEmpty)).sortBy(-_.forces.total)
    val irregOrigins  = (irregs  filter (_.forces.nonEmpty)).sortBy(-_.forces.total)
    (rangerOrigins, irregOrigins)
  }

  // The Bot doesn't do a normal Air lift because there are special
  // instructions to only lift Special Forces to the space with
  // the most NVA Troops (Rangers before Irregulars)
  def botAirLift(): Unit = {
      val destCandidates = spaces(Cambodia) filter (_.pieces.has(NVATroops))
      if (destCandidates.isEmpty) {
        log("US chooses to not Air Lift Special Forces")
        log("because there are no NVA Troops in Cambodia")
      }
      else {
        val dest = Bot.bestCandidate(destCandidates, mostTroops)
        val (rangerOrigins, irreqOrigins) = getOrigins(dest.name)
        val maxOrigins  = if (game.inMonsoon) 1 else 3
        var usedOrigins = Set.empty[String]
        def originOK(origin: Origin) = usedOrigins(origin.name) || usedOrigins.size < maxOrigins
        
        def nextLift(remaining: Int, origins: List[Origin]): Int = {
          origins match {
            case origin::rest if remaining > 0 && originOK(origin) =>
              val num = origin.forces.total min remaining
              val movers = Bot.selectFriendlyToPlaceOrMove(origin.forces, num)

              movePieces(movers, origin.name, dest.name)
              usedOrigins += origin.name
              num + nextLift(remaining - num, rest)
            case _ => 0
          }
        }

        val numRangers = nextLift(4, rangerOrigins)
        nextLift(4 - numRangers, irreqOrigins)
      }
  }

  def botSweepInPlace(faction: Faction, candidates: List[Space]): Unit = {
    for (sp <- candidates)
      sweepInPlace(sp.name, faction, NormalTroops)
  }

  def executeUnshaded(faction: Faction): Unit = {
    // US Air lift

    log("\nUS Air Lift into Cambodia")
    log(separator(char = '='))
    loggingControlChanges {
      if (game.isHuman(US)) {
        val params = Params(event = true, free  = true,
          airlift = AirLiftParams(onlyTo = Cambodia.toSet))
  
        Human.doAirLift(params)
      }
      else
        botAirLift()
    }

    log("\nUS or ARVN Sweep within Cambodia")
    log(separator(char = '='))

    // Sweep only within Cambodia
    // Bot will only activate guerrillas in place
    if (game.isHuman(faction)) {
      val canSweepUS   = (spaces(Cambodia) exists hasUsTroops) ||
                         (spaces(Cambodia) exists (_.sweepActivations(US, NormalTroops) > 0))
      val canSweepARVN = (spaces(Cambodia) exists hasArvnTroops) ||
                         (spaces(Cambodia) exists (_.sweepActivations(ARVN, NormalTroops) > 0))
      if (canSweepUS || canSweepARVN) {
        val choices = List(
          choice(canSweepUS,   US,   "US"),
          choice(canSweepARVN, ARVN, "ARVN")
        ).flatten
        askMenu(choices, "Choose faction to peform a Sweep within Cambodia:").head match {
          case f if game.isHuman(f) =>
            val params = Params(
              event  = true,
              free   = true,
              onlyIn = Some(Cambodia.toSet)
            )
            loggingControlChanges {
              Human.executeSweep(f, params)
            }

          case f =>
            val candidates = spaces(Cambodia) filter (_.sweepActivations(f, NormalTroops) > 0)
            if (candidates.nonEmpty)
              botSweepInPlace(f, candidates)
            else
              log(s"\n$f chooses to not sweep troops within Cambodia")
        }
      }
      else
        log("\nNeither US nor ARVN can effectively Sweep within Cambodia")
    }
    else {
      val usCandidates   = spaces(Cambodia) filter (_.sweepActivations(US, NormalTroops) > 0)
      val arvnCandidates = spaces(Cambodia) filter (_.sweepActivations(ARVN, NormalTroops) > 0)


      if (usCandidates.nonEmpty)
        botSweepInPlace(US, usCandidates)
      else if (game.isBot(ARVN) && arvnCandidates.nonEmpty)
        botSweepInPlace(ARVN, arvnCandidates)
      else
        log("\nUS chooses to not move troops within Cambodia")
    }

    log("\nRemove 2 NVA/VC (non-tunneled) bases in Cambodia")
    log(separator(char = '='))
    loggingControlChanges {
      def baseCandidates = spaces(Cambodia) filter (_.pieces.has(InsurgentNonTunnels))
      if (baseCandidates.isEmpty)
        log("There are no NVA or VC bases in Cambodia")
      else if ((baseCandidates map (_.pieces.totalOf(InsurgentNonTunnels))).sum <= 2) {
        for (sp <- baseCandidates)
          removePieces(sp.name, sp.pieces.only(InsurgentNonTunnels))
      }
      else if (game.isHuman(faction)) {
        def removeNextBase(numRemaining: Int): Unit = if (numRemaining > 0 && baseCandidates.nonEmpty) {
          val name = askCandidate("Remove a NVA/VC base from which space: ", spaceNames(baseCandidates))
          val base = askPieces(game.getSpace(name).pieces.only(InsurgentNonTunnels), 1)
          println()
          removePieces(name, base)
          removeNextBase(numRemaining - 1)
        }
        removeNextBase(2)
      }
      else {
        def removeNextBase(numRemaining: Int): Unit = if (numRemaining > 0 && baseCandidates.nonEmpty) {
          val sp = Bot.pickSpaceRemoveReplace(faction)(baseCandidates)
          val base = Bot.selectEnemyRemoveReplaceActivate(sp.pieces.only(InsurgentNonTunnels), 1)
          removePieces(sp.name, base)
          removeNextBase(numRemaining - 1)
        }
        removeNextBase(2)
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(NVAForces)

  def executeShaded(faction: Faction): Unit = placePiecesOnMap(NVA, 12, NVAForces, Cambodia)
}
