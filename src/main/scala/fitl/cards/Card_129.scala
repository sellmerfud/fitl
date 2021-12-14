
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

object Card_129 extends EventCard(129, "Coup! Failed Attempt", SingleEvent, List.empty, ListMap.empty) {

  // This function is not used for Coup Cards
  def unshadedEffective(faction: Faction): Boolean = false

  val isCandidate = (sp: Space) => sp.pieces.totalOf(ARVNCubes) > 2

  // Coup round event
  // ARVN removes 1 in 3 of its cubes per space (rounded down)
  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter isCandidate

    log("ARVN must remove 1 in 3 of its Cubes per space")
    log()
    if (candidates.isEmpty)
      log("There are no spaces with 3 or more ARVN Cubes")
    else {
      val removals = for (sp <- candidates) yield {
        val cubes = sp.pieces.only(ARVNCubes)
        val num   = cubes.total / 3
        val pieces = if (game.isHuman(ARVN))
          askPieces(cubes, num, prompt = Some(s"Select cubes to remove from ${sp.name}"))
        else
          Bot.selectFriendlyRemoval(cubes, num)

        (sp.name, pieces)
        }

      loggingControlChanges {
        for ((name, pieces) <- removals)
          removePieces(name, pieces)
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
