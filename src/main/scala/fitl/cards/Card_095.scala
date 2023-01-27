
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
// Root 'em out: US free Air Lifts, then Sweeps (no moves) or Assaults
// (no ARVN) in 2 spaces, then Air Strikes.
//
// Shaded Text
// Big-unit war bypasses population: Shift 3 Provinces with no Police each
// 2 levels toward Active Opposition.
//
// Tips
// For the unshaded effect, US decides the details of the unshaded actions,
// which follow the usual restrictions, except that the Sweep Op could occur
// even during Monsoon, Sweep is in place only because of "no moves", and the US
// Assault may not add and ARVN Assault (because of "no ARVN"). Air Strike can
// include Degrading the Trail as usual (4.2.3).
// For shaded, 0 Population Provinces cannot shift from Neutral.

object Card_095 extends EventCard(95, "Westmoreland",
  DualEvent,
  List(VC, US, NVA, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  def baseFirstOK = if (game.isBot(US))
    Bot.canUseAbramsUnshaded(US)
  else
    Human.canUseAbramsUnshaded(US)

  val canSweep = (sp: Space) =>
    !sp.isLoC &&
    sp.sweepActivations(US, NormalTroops) > 0

  def pattonCount = if (game.isBot(US))
      Bot.m48PattonCount
    else
      Human.m48PattonCount

  def canAssault = (sp: Space) =>
    assaultEffective(US, NormalTroops, baseFirstOK, false, pattonCount)(sp)
  
  

  val canAssultOrSweep = (sp: Space) => canAssault(sp) || canSweep(sp)
  
  def assaultKillsAllActive(faction: Faction) = (sp: Space) => {
      canAssault(sp) &&
      assaultKillsAllVulnerable(US, NormalTroops, baseFirstOK, vulnerableTunnels = false, pattonCount)(sp)
    
  }

  def botCanAssultOrSweep(faction: Faction) = (sp: Space) => assaultKillsAllActive(faction)(sp) || canSweep(sp)

  def humanSweepAssault(): Unit = {
    var spacesSelected = Set.empty[String]

    def nextSpace(): Unit = {
      val candidates = spaceNames(game.spaces filter (sp => !spacesSelected(sp.name) && canAssultOrSweep(sp)))

      if (spacesSelected.size < 2 && candidates.nonEmpty) {
        val count = spacesSelected.size + 1
        val name = askSimpleMenu(candidates, s"\nChoose ${ordinal(count)} space to Sweep/Assault").head
        val sp   = game.getSpace(name)
        val choices = List(
          choice(canAssault(sp), "assault", "Assault"),
          choice(canSweep(sp),   "sweep",   "Sweep in place")
        ).flatten
        
        askMenu(choices, s"\nChoose operation for $name").head match {
          case "assault" => Human.performAssault(US, name, Params(event = true, free = true))
          case _         => sweepInPlace(name, US, NormalTroops)
        }
        spacesSelected += name
        nextSpace()
      }
    }

    nextSpace()
  }

  // Bot Instructions:
  // In each space, Assault if it would remove all Active enemies; otherwise Sweep.
  def botSweepAssault(faction: Faction): Unit = {
    var spacesSelected = Set.empty[String]
    def validSpaces = game.spaces filterNot (sp => spacesSelected(sp.name))

    def nextAssault(): Unit = {
      val candidates = validSpaces filter assaultKillsAllActive(faction)

      if (spacesSelected.size < 2 && candidates.nonEmpty) {
        val sp = Bot.pickSpaceWithMostVulnerableInsurgents(faction, candidates)
        Bot.performAssault(US, sp.name, Params(event = true, free = true))
        spacesSelected += sp.name
        nextAssault()
      }
    }

    def nextSweep(): Unit = {
      val candidates = validSpaces filter canSweep

      if (spacesSelected.size < 2 && candidates.nonEmpty) {
        val sp = Bot.pickSpaceWithMostSweepActivations(US, NormalTroops)(candidates)
        sweepInPlace(sp.name, US, NormalTroops)
        spacesSelected += sp.name
        nextSweep()
      }
    }

    nextAssault()
    val numAssaulted = spacesSelected.size
    if (numAssaulted > 0)
      pause()

    nextSweep()
    if (spacesSelected.size > numAssaulted)
      pause()
  }

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists botCanAssultOrSweep(faction)

  def executeUnshaded(faction: Faction): Unit = {
    val human  = game.isHuman(US)
    val params = Params(event = true, free = true)
    if (human) {
      Human.doAirLift(params)
      humanSweepAssault() 
      Human.doAirStrike(params)
    }
    else {
      US_Bot.airLiftActivity(params)
      pause()
      botSweepAssault(faction)
      US_Bot.airStrikeActivity(params)
      pause()
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.canHaveSupport &&
    !sp.pieces.has(ARVNPolice) &&
    sp.support != ActiveOpposition

  def shadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate
    val human      = game.isHuman(faction)
    val prompt     = "\nChoose three Provinces to shift toward Active Opposition:"
    val provinces  = candidates.size match {
      case 0          => Nil
      case 1|2|3      => spaceNames(candidates)
      case _ if human => askSimpleMenu(spaceNames(candidates), prompt, 3)
      case _          => spaceNames(Bot.pickSpaces(3, candidates)(VC_Bot.pickSpaceTowardActiveOpposition))
    }

    println()
    if (provinces.isEmpty)
      log("There are no Provinces without Police that can be shifted toward Active Oppostion")
    else
      loggingPointsChanges {
        for (name <- provinces)
          decreaseSupport(name, 2)
      }
  }
}
