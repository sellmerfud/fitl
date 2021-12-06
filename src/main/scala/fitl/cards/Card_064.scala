
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
// Uneasy allies: Aid +10 or –10. Patronage +3 or -5.
// If US or ARVN executing, that Faction Pacifies as if Support Phase.
// If Insurgent executing, that Faction remains Eligible.
//
// Tips
// Either the US or the ARVN only—whichever is executing the Event—Pacifies,
// not one after the other as during the Support Phase (6.3.1).

object Card_064 extends EventCard(64, "Honolulu Conference",
  SingleEvent,
  List(ARVN, US, NVA, VC),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Unshaded),
          VC   -> (Performed   -> Unshaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    if (isCoin(faction)) {
      (game.arvnResources > pacifyCost && (game.nonLocSpaces exists pacifyCandidate(faction))) ||
      game.usAid     < EdgeTrackMax ||
      game.patronage < EdgeTrackMax
    }
    else
      game.usAid > 0 || game.patronage > 0

  def executeUnshaded(faction: Faction): Unit = {
    val (addAid, addPat) = if (game.isHuman(faction)) {
      val aidChoices = List(true -> "Increase US Aid +10", false -> "Decrease US Aid -10")
      val patChoices = List(true -> "Increase Patronage Aid +3", false -> "Decrease Patronage -5")
      (askMenu(aidChoices, "\nChoose one:").head, askMenu(patChoices, "\nChoose one:").head)
    }
    else if (isCoin(faction))
      (true, false)  // Only US Bot uses this event, so we decrease Patronage
    else
      (false, false)

    println()
    if (addAid)
      increaseUsAid(10)
    else
      decreaseUsAid(10)
    
    if (addPat)
      increasePatronage(3)
    else
      decreasePatronage(5)

    if (isCoin(faction))
      coupSupportPhase(SupportParams().copy(forEvent = true, factions = Set(faction)))
    else
      remainEligibleNextTurn(faction)
  }

  // Single event - These functions are not used
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
