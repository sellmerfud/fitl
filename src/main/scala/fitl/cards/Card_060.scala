
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
// Pulitzer photo inspires: 3 out-of-play US pieces to Available.
//
// Shaded Text
// Photos galvanize home front: NVA place 6 Troops outside South Vietnam,
// add +6 Resources, and, if executing, stay Eligible.
//
// Tips
// For the unshaded Event, Troops and Bases in US Available both affect
// the US Support + Available score (7.2). If NVA is the Faction executing
// the shaded Event, it stays Eligible.

object Card_060 extends EventCard(60, "War Photographer",
  DualEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Ignored -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = game.outOfPlay.has(USPieces)

  def executeUnshaded(faction: Faction): Unit = {
    val usPieces = game.outOfPlay.only(USPieces)

    val toAvail = if (usPieces.isEmpty) {
      log("There are no Out of Play US Pieces")
      Pieces()
    }
    else if (game.isHuman(faction))
      askPieces(usPieces, 3, prompt = Some("Move Out of Play US pieces to Available:"))
    else
      Bot.selectFriendlyToPlaceOrMove(usPieces, 3)

    println()
    moveOutOfPlayToAvailable(toAvail)
    
  }

  def shadedEffective(faction: Faction): Boolean = false  // Not executed by Bots

  def executeShaded(faction: Faction): Unit = {
    val num = if (game.isHuman(NVA))
      6
    else
      game.availablePieces.totalOf(NVATroops) min 6  // Bot only places from available

    placePiecesOnMap(NVA, num, Set(NVATroops), NorthVietnam::LaosCambodia)
    println()
    increaseResources(NVA, 6)
    if (faction == NVA)
      remainEligibleNextTurn(NVA)
  }
}
