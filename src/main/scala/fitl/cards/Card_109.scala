
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
// Party control of NLF draws anti-communist reaction: 
// Shift each City with VC 1 level toward Active Support.
//
// Shaded Text
// National Liberation Front leader: Place a VC Base and
// a VC Guerrilla in Saigon. Stay Eligible.
//
// Tips
// The VC Base cannot be placed if there are already 2 of any
//  Factions’ Bases in Saigon (1.4.2, 5.1.1).

object Card_109 extends EventCard(109, "Nguyen Huu Tho",
  DualEvent,
  List(VC, NVA, ARVN, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.isCity &&
    sp.pieces.has(VCPieces) &&
    sp.support < ActiveSupport

  def unshadedEffective(faction: Faction): Boolean = game.citySpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val canidates = game.citySpaces filter unshadedCandidate

    if (canidates.isEmpty)
      log("There are no city spaces that qualify for the event")
    else
      loggingPointsChanges {
        for (sp <- canidates)
          increaseSupport(sp.name, 1)
      }
  }

  def askRemove(pieceType: PieceType): Boolean = {
    Human.numToPlace(pieceType: PieceType, 1) == 1
  }

  def shadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(VCGuerrillas_U) ||
    (game.availablePieces.has(VCBase) && game.getSpace(Saigon).canTakeBase)

  def executeShaded(faction: Faction): Unit = {
    val canRemove     = faction == VC && game.isHuman(VC)
    val canTakeBase   = game.getSpace(Saigon).canTakeBase
    val haveBase      = game.availablePieces.has(VCBase)
    val haveGuerrilla = game.availablePieces.has(VCGuerrillas_U)
    var toPlace       = Pieces()

    if (canTakeBase && (haveBase || (canRemove && askRemove(VCBase))))
        toPlace = toPlace.add(1, VCBase)

    if (haveGuerrilla || (canRemove && askRemove(VCGuerrillas_U)))
      toPlace = toPlace.add(1, VCGuerrillas_U)

    placePieces(Saigon, toPlace)

    println()
    remainEligibleNextTurn(faction)
  }
}
