
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
// Initial support: Move US pieces from out of play to map:
// - 4 if 0-2 cards in RVN Leader box
// - 2 if 3-5 cards in RVN Leader box
//
// Shaded Text
// Building skepticism: US Troop Casualties up to cards in RVN Leader
// box plus all US Base Casualties go out of play.
//
// Tips
// "Pieces" can include Bases. For the shaded text, count the number of Coup cards
// (including any "Failed Attempt" cards) in the RVN Leader box: the executing Faction
// selects that number of non-Base pieces in the Casualties box—in addition to all
// Bases there—to move to the Out of Play box.

object Card_120 extends EventCard(120, "US Press Corps",
  DualEvent,
  List(VC, ARVN, NVA, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Ignored -> Shaded))) {

  def unshadedEffective(faction: Faction): Boolean = game.outOfPlay.has(USPieces)

  def executeUnshaded(faction: Faction): Unit = {
    val num = if (game.numCardsInLeaderBox < 3) 4 else 2
    val validSpaces = spaceNames(game.spaces filterNot (_.isNorthVietnam)) 
    
    if (!game.outOfPlay.has(USPieces))
      log("There are no Out of Play US Pieces")
    else
      placeOutOfPlayPiecesOnMap(faction, num, USPieces, validSpaces)
  }

  def shadedEffective(faction: Faction): Boolean =
    game.casualties.has(USBase) ||
    (game.numCardsInLeaderBox > 0 && game.casualties.has(USTroops))

  def executeShaded(faction: Faction): Unit = {
    val numTroops = game.numCardsInLeaderBox min game.casualties.totalOf(USTroops)
    val numBases  = game.casualties.totalOf(USBase)
    val pieces    = Pieces(usTroops = numTroops, usBases = numBases)

    moveCasualtiesToOutOfPlay(pieces)
  }
}
