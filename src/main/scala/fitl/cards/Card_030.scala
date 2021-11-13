
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
// Fire support: US or ARVN free Air Strikes any 1-3 coastal spaces,
// removing up to 2 pieces per space (no die roll and no effect on Trail).
//
// Shaded Text
// Frightening free fire: Shift 2 coastal Provinces with US Troops each
// 2 levels toward Active Opposition.
//
// Tips
// "Coastal" means Provinces, Cities, and LoCs touching ocean, including
// North Vietnam, as well as Provinces across a Highway from ocean (1.3.7).
// For the unshaded text, "any 1-3 spaces" means that the spaces do not need
// to have US or ARVN pieces in them.  The executing Faction chooses US or ARVN
// to strike, and that Faction decides the details;  it must strike at least 1 space.
// The Air Strikes have all other usual restrictions and effects  in the spaces,
// including removing Active pieces only and Bases last, and shifting Support/ Opposition (4.2.3).

object Card_030 extends EventCard(30, "USS New Jersey",
  DualEvent,
  List(US, VC, ARVN, NVA),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {
  
  val unshadedCandidate = (sp: Space) =>
    sp.coastal && sp.pieces.hasExposedInsurgents

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.spaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no coastal spaces with exposed Insurgent pieces")
    else if (game.isHuman(faction)) {
      val num = askInt("Strike in how many coastal spaces", 1, candidates.size min 3)
      val choices = candidates map (sp => sp.name -> sp.name)
      val names = askMenu(choices, "Select coastal spaces to air strike: ", num).toSet
      val params = Params(
        event        = true,
        free         = true,
        strikeParams = AirStrikeParams(
          canDegradeTrail = false,
          noCoin          = true,
          designated      = Some(names),
          maxHits         = Some(6),
          maxHitsPerSpace = Some(2)))
      loggingControlChanges {
        Human.doAirStrike(params)
      }
    }
    else {  // Bot
      def nextSpace(numRemaining: Int, candidates: List[Space]): List[String] = {
        if (numRemaining > 0 && candidates.nonEmpty) {
          val sp = Bot.pickSpaceRemoveReplace(faction)(candidates)
          sp.name::nextSpace(numRemaining - 1, candidates filterNot (_.name == sp.name))
        }
        else
          Nil
      }
      val names = nextSpace(3, candidates).toSet
      val params = Params(
        event        = true,
        free         = true,
        strikeParams = AirStrikeParams(
          canDegradeTrail = false,
          noCoin          = true,
          designated      = Some(names),
          maxHits         = Some(6),
          maxHitsPerSpace = Some(2)))
      loggingControlChanges  {
        US_Bot.airStrikeActivity(params)
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.coastal    &&
    sp.pieces.has(USTroops) &&
    sp.population > 0 &&
    sp.support != ActiveOpposition
    
  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that meet the event criteria")
    else {
      val names = if (game.isHuman(faction)) {
        val choices = candidates map (sp => sp.name -> sp.name)
        askMenu(choices, "Select coastal Provinces:", 2 min choices.size)
      }
      else {
        def nextSpace(numRemaining: Int, candidates: List[Space]): List[String] = {
          if  (numRemaining >  0 && candidates.nonEmpty) {
            val sp = VC_Bot.pickSpaceTowardActiveOpposition(candidates)
            sp.name::nextSpace(numRemaining - 1, candidates filterNot (_.name == sp.name))
          }
          else
            Nil
        }
        nextSpace(2, candidates)
      }

      loggingPointsChanges {
        for (name <- names) 
          decreaseSupport(name, 2)
      }
    }
  }
}
