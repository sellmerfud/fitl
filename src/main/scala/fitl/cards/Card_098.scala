
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
// Royal Australians: Place 2 out of play US Troops into a Province
// or remove all Guerrillas from all Jungle with US Troops.
//
// Shaded Text
// VC strike newly arrived troops: 1 US Base and 1 US Troop in
// a Jungle with 2+ VC Guerrillas to Casualties.
//
// Tips
// For the unshaded effect, the Troops could go into any 1 Province
// except North Vietnam, not necessarily into Jungle.

object Card_098 extends EventCard(98, "Long Tan",
  DualEvent,
  List(VC, US, ARVN, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {


  val unshadedCandidate = (sp: Space) =>
    sp.isJungle &&
    sp.pieces.has(USTroops) &&
    sp.pieces.has(Guerrillas)

  def unshadedEffective(faction: Faction): Boolean = if (faction == US)
    game.outOfPlay.has(USTroops) || (game.nonLocSpaces exists unshadedCandidate)
  else
    (game.nonLocSpaces exists unshadedCandidate)
  

  def executeUnshaded(faction: Faction): Unit = {
    val guerrillaCandidates = game.nonLocSpaces filter unshadedCandidate
    val oopTroops = game.outOfPlay.totalOf(USTroops)
    val numRemoved = guerrillaCandidates.map(_.pieces.totalOf(Guerrillas)).sum
    val choices = List(
      choice(oopTroops > 0,  "troops",     "Place Out of Play US Troops in a Province"),
      choice(numRemoved > 0, "guerrillas", "Remove all Guerrillas from a Jungle spaces with US Troops")
    ).flatten

    if (choices.isEmpty)
      log("The event cannot be carried out.")
    else {
      val action = if (game.isHuman(faction))
        askMenu(choices, "\nChoose one:").head
      else if (faction == ARVN)
        "guerrillas"
      else 
        if (oopTroops == 0 || numRemoved >= 4) "guerrillas" else "troops"

      loggingControlChanges {
        action match {
          case "troops" =>
            val provinces = game.nonLocSpaces filter (_.isProvince)
            val name = if (game.isHuman(faction))
              askSimpleMenu(spaceNames(provinces), "\nSelect a Province for the Out of Play troops:").head
            else
              US_Bot.pickSpacePlaceCubesSpecialForces(troops = true)(provinces).name
            val troops = Pieces(usTroops = oopTroops min 2)
            placePiecesFromOutOfPlay(name, troops)

          case _ =>
            for (sp <- guerrillaCandidates)
              removePieces(sp.name, sp.pieces.only(Guerrillas))
        }      
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isJungle &&
    sp.pieces.totalOf(VCGuerrillas) >= 2 &&
    sp.pieces.has(USBase::USTroops::Nil)

  def shadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces the qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nChoose Jungle space to remove US Base and US Troop:").head
      else
        NVA_Bot.pickSpaceRemoveReplace(candidates).name

      println()
      val sp = game.getSpace(name)
      val pieces = Pieces(
        usBases  = sp.pieces.totalOf(USBase) min 1,
        usTroops = sp.pieces.totalOf(USTroops) min 1)
      removeToCasualties(name, pieces)
    }
  }
}
