
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
// Catholic backlash: Shift Saigon 1 level toward Passive Support.
// Patronage +6.
//
// Shaded Text
// Saigon Buddhists find leader: Place a VC piece in and shift Saigon 1 level
// toward Passive Opposition. Patronage –6.
//
// Tip. A Base is a "piece".

object Card_089 extends EventCard(89, "Tam Chau",
  DualEvent,
  List(ARVN, VC, NVA, US),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Critical    -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.patronage < EdgeTrackMax

  def executeUnshaded(faction: Faction): Unit = {
    val saigon_support = game.getSpace(Saigon).support

    loggingControlChanges {
      if (saigon_support < PassiveSupport)
        increaseSupport(Saigon, 1)
      else if (saigon_support > PassiveSupport)
        decreaseSupport(Saigon, 1)
      increasePatronage(6)
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.getSpace(Saigon).support > PassiveOpposition

  def executeShaded(faction: Faction): Unit = {
    val sp = game.getSpace(Saigon)
    val saigon_support = sp.support
    val piece = if (game.isHuman(faction)) {
      val pieceType = if (sp.totalBases == 2)
        VCGuerrillas_U
      else
        askPieceType(s"\nChoose piece to place in Saigon:", List(VCGuerrillas_U, VCBase))
      val num = Human.numToPlace(pieceType, 1)  // Check for voluntary removal
      Pieces().set(num, pieceType)
    }
    else
      Bot.selectFriendlyToPlaceOrMove(game.availablePieces.only(VCPieces), 1)

    loggingControlChanges {
      placePieces(Saigon, piece)
      if (saigon_support < PassiveOpposition)
        increaseSupport(Saigon, 1)
      else if (saigon_support > PassiveOpposition)
        decreaseSupport(Saigon, 1)
      decreasePatronage(6)
    }
  }
}
