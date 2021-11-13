
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
// Operation Menu: Remove a die roll of Insurgent pieces total from Cambodia and Laos.
//
// Shaded Text
//  "Secret bombing" revealed: NVA places 2 pieces in Cambodia.
//  US moves any 2 US Troops to out of play. Aid –6.
//
// Tips
// "Pieces" include unTunneled Bases (1.4, 5.1.1).
// "Any Troops" here can include Casualties, Available, or from the map.

object Card_002 extends EventCard(2, "Kissinger",
  DualEvent,
  List(US, ARVN, NVA, VC),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Critical    -> Shaded),
          VC   -> (Performed   -> Shaded))) {
    
  def unshadedEffective(faction: Faction): Boolean =
    spaces(LaosCambodia).exists(_.pieces.except(InsurgentTunnels).has(InsurgentPieces))

  def executeUnshaded(faction: Faction): Unit = {
    val pieceTypes = InsurgentPieces.toSet -- InsurgentTunnels.toSet
    val num = d6
    log(s"\nRolling d6 to determine number of pieces to remove: $num")
    removePiecesFromMap(faction, num, pieceTypes, false, LaosCambodia)
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    val cambodia = Set(NortheastCambodia, TheFishhook, TheParrotsBeak, Sihanoukville)
    loggingControlChanges {
      placePiecesOnMap(NVA, 2, NVAPieces, cambodia)
      removePiecesToOutOfPlay(US, 2, Set(USTroops), false, spaceNames(game.spaces))
      decreaseUsAid(6)
    }
  }
}
