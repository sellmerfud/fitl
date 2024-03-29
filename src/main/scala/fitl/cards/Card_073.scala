
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
// LBJ advances social agenda: Conduct a Commitment Phase.
//
// Shaded Text
// War wrecks economy: US moves 3 pieces from Available to out of play.
//
// Tips
// For the unshaded text, the US as usual decides the details of the
// Commitment Phase. If unshaded "Great Society" occurs while unshaded
// Event 15 "Medevac" is in effect, "Medevac" affects the immediate
// Commitment Phase and then stays in effect to affect the coming Coup
// Round’s Commitment Phase as well. For shaded "Great Society", the US
// decides which pieces; "pieces" can include Bases.

object Card_073 extends EventCard(73, "Great Society",
  DualEvent,
  List(ARVN, NVA, US, VC),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = false  // Bots do not execute this event.
  
  // Mo_Medevac_Unshaded    - In Commitment Phase (immediately move all US TROOPS in CASUALTIES to AVAILABLE,
  //                          no TROOPS go out of play.  See note: For effect when #73 Great Society is played.
  def executeUnshaded(faction: Faction): Unit = coupCommitmentPhase()

  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(USPieces)

  def executeShaded(faction: Faction): Unit = {
    val availUS = game.availablePieces.only(USPieces)

    if (availUS.isEmpty)
      log("There are no available US Pieces")
    else {
      val toRemove =  if (game.isHuman(US))
        askPieces(availUS, 3, prompt = Some("Remove US Pieces to Out Of Play"))
      else
        Bot.selectFriendlyRemoval(availUS, 3)

      moveAvailableToOutOfPlay(toRemove)
    }
  }
}
