
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
// Perimeter: Stay Eligible. Until Coup, no Ambush;
// remove 1 Guerrilla each Marching group that Activates.
// MOMENTUM
//
// Shaded Text
// Infiltrators turn mines around: Remove 1 COIN Base and 1 Underground
// Insurgent from a space with both (US to Casualties).
//
// Tips
// Eligibility for the executing Faction applies instantly and once;
// the effects on Ambush and March last until the next Coup Round (5.4).

object Card_017 extends EventCard(17, "Claymores",
  DualEvent,
  List(US, ARVN, VC, NVA),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val isClaymoresSpace = (sp: Space) =>
    sp.pieces.has(CoinBases) &&
    sp.pieces.has(UndergroundGuerrillas)


  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = {
    remainEligibleNextTurn(faction)
    playMomentum(Mo_Claymores)
  }

  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists isClaymoresSpace

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter isClaymoresSpace
    if (candidates.nonEmpty) {
      val name = if (game.isHuman(faction))
        askCandidate("\nRemove pieces from which space: ", spaceNames(candidates))
      else
        Bot.pickSpaceRemoveReplace(faction)(candidates).name
      val pieces = game.getSpace(name).pieces
      val (base, guerrilla) = if (game.isHuman(faction))
        (askPieces(pieces.only(CoinBases), 1, prompt = Some("Remove which base")),
        askPieces(pieces.only(UndergroundGuerrillas), 1, prompt = Some("Remove which guerrilla")))
      else
        (Bot.selectEnemyRemovePlaceActivate(pieces.only(CoinBases), 1),
          Bot.selectEnemyRemovePlaceActivate(pieces.only(UndergroundGuerrillas), 1))
      removePieces(name, base + guerrilla)
    }
  }
}
