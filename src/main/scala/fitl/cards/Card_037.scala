
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
// Northern casualties: Select a US Base with US Troops.
// Remove 10 NVA Troops within 1 space of it.
//
// Shaded Text
// US Marines pinned: Up to 3 US Troops in 1 space with NVA to Casualties.
// US Ineligible through next card.
//
// Tips
// The 10 total NVA Troops removed may come from more than 1 adjacent space,
// including the same space as the US pieces.

object Card_037 extends EventCard(37, "Khe Sanh",
  DualEvent,
  List(NVA, US, VC, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.pieces.has(USBase)   &&
    sp.pieces.has(USTroops) &&
    withOrAdjacentExists(sp.name)(_.pieces.has(NVATroops))


  def unshadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists unshadedCandidate
    
  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askCandidate("Choose a space with US Base and Troops: ", spaceNames(candidates))
      else {
        val priorites = List(new Bot.HighestScore[Space](
          "Most NVA Troops within one space",
          sp => withOrAdjacentTotal(sp.name)(_.pieces.totalOf(NVATroops)) 
        ))
        Bot.bestCandidate(candidates, priorites).name
      }

      val validSpaces = spaceNames(withOrAdjacent(name)(_.pieces.has(NVATroops)))
      log(s"\n$faction selects $name")
      removePiecesFromMap(faction, 10, Set(NVATroops), false, validSpaces)
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.pieces.has(USTroops) &&
    sp.pieces.has(NVAPieces)

  def shadedEffective(faction: Faction): Boolean =
    (game.spaces exists shadedCandidate) ||
    game.sequence.willBeEligibeNextTurn(US)

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.spaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces with US Troops and NVA pieces")
    else {
      val name = if (game.isHuman(faction))
        askCandidate("Choose a space with US Troops and NVA pieces", spaceNames(candidates))
      else
        Bot.pickSpaceRemoveReplace(faction)(candidates).name

      val sp = game.getSpace(name)
      val num = sp.pieces.totalOf(USTroops) min 3
      removePieces(name, Pieces(usTroops = num))
    }

    makeIneligibleThroughNextTurn(US, faction == US)
  }
}
