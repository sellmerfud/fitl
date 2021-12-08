
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
// Sweep flushes enemy into kill zone: US or ARVN free Sweeps 1 non-Jungle space
// with US and ARVN Troops. They free Assault as US.
//
// Shaded Text
// Poor OPSEC: VC or NVA free March Guerrillas to any 3 spaces then free Ambush
// in each (even if Active).
//
// Tips
// The Sweep or Marches could occur even during Monsoon. For the unshaded text, the
// executing Faction picks US or ARVN and that Faction decides the details of its
// Sweep and Assault. US and ARVN Troops both can move and Activate Guerrillas.
// ARVN Troops would contribute as if US Troops, including double enemy losses for
// any US Base in a space, and so on. For shaded, the executing Faction picks VC or
// NVA and that Faction decides the details of the March and Ambush. NVA March could
// include multiple moves from Laos/Cambodia spaces if the Trail is at greater than 0.
// The Ambushes must occur in March destinations, but need not involve Guerrillas that
// Marched and could occur even if all that Faction’s Guerrillas there were already Active.

object Card_099 extends EventCard(99, "Masher/White Wing",
  DualEvent,
  List(VC, US, ARVN, NVA),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  
  val unshadedCandidate = (sp: Space) => !(sp.isLoC || sp.isJungle)
  def unshadedCandidates = spaceNames(game.nonLocSpaces filter unshadedCandidate)

  def unshadedEffective(faction: Faction): Boolean = {
    Bot.bestSweepAssaultTarget(faction, AllTroopsAsUS, unshadedCandidates) match {
      case Some((_, num)) => faction == ARVN || num >= 4  // US only if it can remove 4+
      case _ => false
    }
  }

  def executeUnshaded(faction: Faction): Unit = {
    val actor = if (game.isHuman(faction))
      askFaction("\nChoose faction to execute the event:", Set(US, ARVN))
    else
      faction
      
      if (game.isHuman(actor)) {
        val params = Params(event = true, free = true, cubeTreatment = AllTroopsAsUS, maxSpaces = Some(1))
        Human.initTurnVariables(specialActivity = false)
        val sweepSpaces = Human.executeSweep(actor, params)
        if (sweepSpaces.nonEmpty) {
          val name          = sweepSpaces.head
          val assaultParams = params.copy(assault = AssaultParams(removeTwoExtra = canUseM48PattonUnshaded(name)))
          Human.performAssault(actor, sweepSpaces.head, assaultParams)
        }
      }
    else {
      val name = Bot.bestSweepAssaultTarget(actor, AllTroopsAsUS, unshadedCandidates).get._1
      // M48Unshaded is checked by the Bot assault code
      Bot.sweepAndAssaultSpace(name, actor, AllTroopsAsUS)
    }
  }

  def shadedEffective(faction: Faction): Boolean = {
    val params = Params(event = true, free = true, maxSpaces = Some(3))
    // Effective if the NVA Bot can sucessfully march
    suspendLogging {
      NVA_Bot.marchOp(params,  6, true, true).nonEmpty
    }
  }

  def executeShaded(faction: Faction): Unit = {
    val params = Params(
      event     = true,
      free      = true,
      maxSpaces = Some(3),
      ambush    = AmbushParams(needUnderground = false, maxAmbush = Some(3))
    )

    val actor = if (game.isHuman(faction))
      askFaction("\nChoose faction to execute the event:", Set(NVA, VC))
    else
      faction  // NVA

    if (game.isHuman(actor)) {
      def canAmbush(name: String): Boolean = {
        val GTypes = ambushGuerrillaTypes(actor, needUnderground = false)
        val sp = game.getSpace(name)
        sp.pieces.has(GTypes) && ambushTargets(name).nonEmpty
      }

      Human.initTurnVariables(specialActivity = false)
      val dests = Human.executeMarch(actor, params)
      for (name <- dests; if canAmbush(name))
        Human.performAmbush(name, actor, March, free = true, needUnderground = false)
    }
    else {
      Bot.initTurnVariables()
      if (actor == NVA)
        NVA_Bot.marchOp(params,  6, true, true).nonEmpty
        else
        VC_Bot.marchOp(params,  6).nonEmpty
      Bot.ambushActivity(actor, Bot.getMoveDestinations.toList, March, 6, params, false)
    }
  }
}
