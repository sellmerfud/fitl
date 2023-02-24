
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
// Counter-sanctuary chemical: All Insurgents in Jungle go Active.
// US free Air Strikes among up to any 2 Jungle spaces (no effect on Trail).
//
// Shaded Text
// Industrial defoliation: Shift each Jungle and Highland with Insurgents 1 level
// toward Active Opposition.
//
// Tips
// For the unshaded text, the US decides the details of the Air Strike but may not
// Degrade the Trail. For shaded, a Province with 0 Population cannot be shifted
// from Neutral (1.6).

object Card_111 extends EventCard(111, "Agent Orange",
  DualEvent,
  List(VC, ARVN, US, NVA),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  val unshadedCandidate = (sp: Space) =>
    sp.isJungle &&
    sp.pieces.has(UndergroundGuerrillas)

  def unshadedEffective(faction: Faction): Boolean =
    (game.nonLocSpaces exists unshadedCandidate) ||
    airStrikeEffective

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    for (sp <- candidates)
      revealPieces(sp.name, sp.pieces.only(UndergroundGuerrillas))
    
    val jungles = spaceNames(game.nonLocSpaces filter (_.isJungle)).toSet
    val params = Params(
      event     = true,
      free      = true,
      maxSpaces = Some(2),
      airstrike = AirStrikeParams(canDegradeTrail = false, designated = Some(jungles)))

      if (game.isHuman(US))
        Human.doAirStrike(params)
      else
        US_Bot.airStrikeActivity(params)
  }

  val shadedCandidate = (sp: Space) =>
    (sp.isJungle || sp.isHighland) &&
    sp.canHaveSupport &&
    sp.support > ActiveOpposition &&
    sp.pieces.has(InsurgentPieces)

  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else
      loggingPointsChanges {
        for (sp <- candidates)
          decreaseSupport(sp.name, 1)
      }
  }
}
