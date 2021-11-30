
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
// Unconventional counterinsurgent: Set a space outside Saigon with US or ARVN
// to Active Support. Add a Terror marker there. Patronage +1.
//
// Shaded Text
// Bureaucratic infighter: Patronage +3. No US Assault until Coup. MOMENTUM
//
// Tips
// A Province with 0 Population cannot be set to Support (1.6).
// The Terror marker would be added even if one is already in the space.

object Card_078 extends EventCard(78, "General Lansdale",
  DualEvent,
  List(ARVN, NVA, VC, US),
  ListMap(US   -> (Critical  -> Unshaded),
          ARVN -> (Critical  -> Shaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Performed -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.name != Saigon &&
    sp.canHaveSupport &&
    sp.pieces.has(CoinPieces)
    

  val unshadedBotCandidate = (sp: Space) => unshadedCandidate(sp) && sp.support != ActiveSupport

  def unshadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists unshadedBotCandidate

  def executeUnshaded(faction: Faction): Unit = {
    if (!(game.nonLocSpaces exists unshadedCandidate))
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction)) {
        val choices = spaceNames(game.nonLocSpaces filter unshadedCandidate)
        askSimpleMenu(choices, "\nChoose a space to set to Active Support:").head
      }
      else {
        val candidates = game.nonLocSpaces filter unshadedBotCandidate
        US_Bot.pickSpaceTowardActiveSupport(candidates).name
      }
      
      println()
      loggingPointsChanges {
        setSupport(name, ActiveSupport)
        if (game.terrorMarkersAvailable > 0)
          addTerror(name, 1)
        increasePatronage(1)
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    increasePatronage(3)
    playMomentum(Mo_GeneralLansdale)
  }
}
