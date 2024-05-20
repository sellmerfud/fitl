
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
import scala.util.Random.shuffle
import fitl.FireInTheLake._
import fitl.EventHelpers._
import fitl.Bot
import fitl.Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }
import fitl.Human

// Unshaded Text
// Whiz-kid corporation: Flip 1 shaded US Capability to unshaded.
//
// Unshaded Text
// Systems analysis in ignorance of local conditions:
// Flip 1 unshaded US Capability to shaded.
//
// Tips
// A player may execute either version (5.2), so ARVN may
// flip a US unshaded Capability to shaded.

object Card_052 extends EventCard(52, "RAND",
  DualEvent,
  List(NVA, VC, US, ARVN),
  ListMap(US   -> (Critical -> Unshaded),
          ARVN -> (Critical -> Unshaded),
          NVA  -> (Critical -> Shaded),
          VC   -> (Critical -> Shaded))) {

  def isCandidate(shaded: Boolean)(cap: Capability) =
    cap.faction == US && cap.shaded == shaded

  def unshadedEffective(faction: Faction): Boolean =
    game.capabilities exists isCandidate(shaded = true)

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.capabilities filter isCandidate(shaded = true)

    if (candidates.isEmpty)
      log("There are no shaded US capabilities in play", Color.Event)
    else {
      val cap = if (game.isHuman(faction))
        askSimpleMenu(candidates, "Choose shaded US capability to flip:").head
      else
        shuffle(candidates).head

      flipCapability(cap)
    }
  }

  def shadedEffective(faction: Faction): Boolean =
        game.capabilities exists isCandidate(shaded = false)

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.capabilities filter isCandidate(shaded = false)

    if (candidates.isEmpty)
      log("There are no unshaded US capabilities in play", Color.Event)
    else {
      val cap = if (game.isHuman(faction))
        askSimpleMenu(candidates, "Choose unshaded US capability to flip:").head
      else
        shuffle(candidates).head

      flipCapability(cap)
    }
  }
}
