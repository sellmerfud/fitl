
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
// Stab at Iron Triangle: US free Air Lifts into, Sweeps in, then
// Assaults a space with a Tunnel—removing Tunneled Bases as if no Tunnel.
//
// Shaded Text
// Heavy casualties, few results: Select a Tunnel space—remove a die roll of
// US Troops within 1 space of it to Casualties.
//
// Tips
// This is one of just a few Events that can force removal of Tunneled Bases (5.1.1).
// The Sweep could occur even during Monsoon. Free US Assault can add an ARVN Assault
// at cost 0 (3.2.4).

object Card_023 extends EventCard(23, "Operation Attleboro", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded))) {

  // COIN Bots do not execute this event
  def unshadedEffective(faction: Faction): Boolean = false

  def executeUnshaded(faction: Faction): Unit = {
    val tunnelSpaces = game.nonLocSpaces filter (_.pieces.has(InsurgentTunnels))

    if (tunnelSpaces.nonEmpty) {
      val name = askCandidate("\nExecute event in which space: ", spaceNames(tunnelSpaces))
      val params = Params(event = true,
                          free  = true,
                          vulnerableTunnels = true,
                          airlift = AirLiftParams(onlyTo = Set(name)))
      loggingControlChanges {
        log(s"\nUS performs free Air Lift into $name")
        log(separator(char = '='))
        Human.doAirLift(params)
        log(s"\nUS performs free Sweep into $name")
        log(separator(char = '='))
        Human.executeSweep(US, params.copy(onlyIn = Some(Set(name))))
        log(s"\nUS performs free Assault in $name")
        log(separator(char = '='))
        Human.performAssault(US, name, params)

        val arvnEffective = assaultEffective(ARVN, NormalTroops, vulnerableTunnels = true)(game.getSpace(name))
        if (arvnEffective && askYorN(s"\nFollow up with ARVN assault in $name? (y/n) ")) {
          log(s"\nUS adds a free follow up ARVN assault in $name")
          Human.performAssault(ARVN, name, params)
        }
      }
    }
    else
      log("There are no spaces with a Tunneled base.")
  }

  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists { sp =>
      sp.pieces.has(InsurgentTunnels) &&
      withOrAdjacentExists(sp.name)(_.pieces.has(USTroops))
    }

  def executeShaded(faction: Faction): Unit = {
    val tunnelSpaces = game.nonLocSpaces filter (_.pieces.has(InsurgentTunnels))

    val name = if (game.isHuman(faction))
      askCandidate("\nExecute event in which space: ", spaceNames(tunnelSpaces))
    else {
      val priorities = List(
        new Bot.HighestScore[Space]("Most US troops in/adjacent", sp => {
          withOrAdjacentFold(sp.name, 0)((sum, sp) => sp.pieces.totalOf(USTroops))
        })
      )
      Bot.bestCandidate(tunnelSpaces, priorities).name
    }
    
    val num = d6
    log(s"\nRolling d6 to determine number of US Troops to remove: $num")
    removePiecesFromMap(faction, num, Set(USTroops), false, getAdjacent(name) + name)
  }
}
