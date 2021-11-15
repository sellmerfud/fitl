
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

// Single Event Text
// Tet holiday gesture: Set any 2 spaces to Passive Support.
// Patronage +2. No Air Strike until Coup.
// MOMENTUM
//
// Tips
// The Support and Patronage take effect at once; the ban on 
// Air Strikes lasts until the next Coup Round.

object Card_041 extends EventCard(41, "Bombing Pause",
  SingleEvent,
  List(NVA, ARVN, US, VC),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (NotExecuted -> Unshaded),
          VC   -> (NotExecuted -> Unshaded))) {


  def unshadedEffective(faction: Faction): Boolean = true

  def humanSelectTwo(numRemaining: Int, candidates: List[String]): List[String] = {
    if (numRemaining > 0 && candidates.nonEmpty) {
      val count = ordinal(3 - numRemaining)
      val name = askCandidate(s"Select the $count space to set to Passive Support: ", candidates)
      name::humanSelectTwo(numRemaining - 1, candidates filterNot (_ == name))
    }
    else
      Nil
  }

  def botSelectTwo(faction: Faction, numRemaining: Int, candidates: List[Space]): List[Space] = {
    if (numRemaining > 0 && candidates.nonEmpty) {

      val sp = faction match {
        case US => US_Bot.pickSpaceTowardActiveSupport(candidates)
        case _  => ARVN_Bot.pickSpaceTowardPassiveSupport(candidates)
      }
      sp::botSelectTwo(faction, numRemaining - 1, candidates filterNot (_.name == sp.name))
    }
    else
      Nil
  }

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter { sp =>
      sp.population > 0 && sp.support != PassiveSupport
    }

    if (candidates.isEmpty)
      log("There are no spaces to shift to Passive Support")
    else {
      val supportSpaces = if (game.isHuman(faction))
        humanSelectTwo(2, spaceNames(candidates)).reverse
      else
        botSelectTwo(faction, 2, candidates).reverse map (_.name)

      log()
      loggingPointsChanges {
        for (name <- supportSpaces)
          setSupport(name, PassiveSupport)
        increasePatronage(2)
      }
      playMomentum(Mo_BombingPause)
    }
  }

  // Single event - These functions are not used
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
