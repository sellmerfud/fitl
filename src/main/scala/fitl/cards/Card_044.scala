
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

// Unshaded Text
// Silver Bayonet: US free Air Lifts into 1 space with any NVA piece,
// then free Sweeps and Assaults there.
//
// Shaded Text
// Dong Xuan campaign—hot LZs: Select a Province with NVA Troops—remove a
// die roll of US Troops within 1 space of it to Casualties.
//
// Tips For the unshaded Event, the US decides the details of the free actions
// but must Air Lift, Sweep, and Assault with something. The Sweep could occur
// even during Monsoon. The free US Assault can add an ARVN Assault at cost 0 (3.2.4).
// For shaded, the US Troops can be any either in the same space as or adjacent to
// the Province with NVA Troops.

package fitl.cards

import scala.collection.immutable.ListMap
import fitl.FireInTheLake._
import fitl.EventHelpers._
import fitl.Bot
import fitl.Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }
import fitl.Human

object Card_044 extends EventCard(44, "la Drang",
  DualEvent,
  List(NVA, ARVN, US, VC),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    !sp.isNorthVietnam &&
    sp.pieces.has(NVAPieces)

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else if (game.isHuman(faction)) {
      val name = askCandidate("Select a space with NVA pieces: ", spaceNames(candidates))
      val params = Params(event = true, free = true, singleTarget = Some(name))

      loggingControlChanges {
        log(s"\nUS performs free Air Lift into $name")
        log(separator(char = '='))
        Human.doAirLift(params)
        log(s"\nUS performs free Sweep into $name")
        log(separator(char = '='))
        Human.executeSweep(US, params)
        log(s"\nUS performs free Assault in $name")
        log(separator(char = '='))
        Human.performAssault(US, name, params)

        val arvnEffective = assaultEffective(ARVN, true)(game.getSpace(name))
        if (arvnEffective && askYorN(s"\nFollow up with ARVN assault in $name? (y/n) ")) {
          log(s"\nUS adds a free follow up ARVN asault in $name")
          Human.performAssault(ARVN, name, params)
        }
      }
    }
    else {  // Bot
      val sp = US_Bot.pickSpaceRemoveReplace(candidates)
      val params = Params(event = true, free = true, singleTarget = Some(sp.name))

      loggingControlChanges {
        log(s"US performs free Air Lift into ${sp.name}")
        log(separator(char = '='))
        US_Bot.airLiftActivity(params)
        log(s"\nUS performs free Sweep into ${sp.name}")
        log(separator(char = '='))
        US_Bot.sweepOp(params)
        log(s"\nUS performs free Assault in ${sp.name}")
        log(separator(char = '='))
        Bot.performAssault(faction, sp.name, params)

        if (assaultEffective(ARVN, true)(game.getSpace(sp.name))) {
          log(s"\nUS adds a free follow up ARVN asault in $name")
          Bot.performAssault(ARVN, sp.name, params)
        }
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(NVATroops) &&
    withOrAdjacentExists(sp.name)(x => x.pieces.has(USTroops))

  val mostUSTroops = List(
    new Bot.HighestScore[Space](
      "Most US Troops in/adjacent",
      sp => withOrAdjacentTotal(sp.name)(_.pieces.totalOf(USTroops))
    )
  )

  def shadedEffective(faction: Faction): Boolean = game.spaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.spaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askCandidate("Select a Province with NVA Troops: ", spaceNames(candidates))
      else
        Bot.bestCandidate(candidates, mostUSTroops).name

      val die = d6
      log(s"\nRolling d6 to determine number of US Troops removed: $die")
      log(separator())
      log(s"$faction selects $name\n")

      val validSpaces = spaceNames(spaces(getAdjacent(name) + name) filter (_.pieces.has(USTroops)))
      removePiecesFromMap(faction, die, Set(USTroops), false, validSpaces)
    }
  }
}
