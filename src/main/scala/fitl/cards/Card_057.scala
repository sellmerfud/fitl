
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
// Protests ignored: Any 2 US Casualties to Available.
//
// Shaded Text
// US accused of neocolonialist war: 2 Available US Troops out of play.
// NVA add a die roll of Resources.
//
// Tips
// "US Casualties" may include Troops, Bases, and Irregulars.

object Card_057 extends EventCard(57, "International Unrest",
  DualEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.casualties.totalOf(USPieces) > 0

  def executeUnshaded(faction: Faction): Unit = {
    val num = game.casualties.totalOf(USPieces) min 2

    if (num == 0)
      log("There a no US casualties")
    else {
      val usPieces = if (game.isHuman(faction))
        askPieces(game.casualties.only(USPieces), num, prompt = Some("Select US casualties"))
      else
        Bot.selectFriendlyToPlaceOrMove(game.casualties.only(USPieces), num)

      moveCasualtiesToAvailable(usPieces)
    }
  }

  def shadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(USTroops)

  def executeShaded(faction: Faction): Unit = {
    val num = game.availablePieces.totalOf(USTroops) min 2

    if (num == 0)
      log("There are no available US Troops")
    else 
      moveAvailableToOutOfPlay(Pieces(usTroops = num))

    if (game.trackResources(NVA)) {
      val die = d6
      log(s"\nRolling d6 to increase NVA resources: $die")
      log(separator())
      increaseResources(NVA, die)
    }
  }
}
