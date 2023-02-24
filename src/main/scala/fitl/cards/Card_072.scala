
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
// Crossover point: Until Coup, Assault and Patrol add +3 Aid per
// Guerrilla removed and cost 0.
// MOMENTUM
//
// Shaded Text
// "If it’s dead, it’s VC": Place 1 VC Guerrilla in each Active Opposition space,
// 2 NVA Troops in each Laos/Cambodia space.
//
// Tips
// "Cost 0" means ARVN will pay 0 Resources for these Operations, and US pays 0
// even if an ARVN Assault is added as part of US Assault (3.2.4).

object Card_072 extends EventCard(72, "Body Count",
  DualEvent,
  List(ARVN, NVA, US, VC),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = playMomentum(Mo_BodyCount)

  def activeOppSpaces = game.nonLocSpaces filter (_.support == ActiveOpposition)

  def shadedEffective(faction: Faction): Boolean =
    (activeOppSpaces.nonEmpty && game.availablePieces.has(VCGuerrillas_U)) ||
    game.availablePieces.has(NVATroops)

  def botPlaceVC(candidates: List[Space]): Unit = {
    if (candidates.nonEmpty && game.availablePieces.has(VCGuerrillas_U)) {
      val sp = VC_Bot.pickSpacePlaceGuerrillas(candidates)
      placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
      botPlaceVC(candidates filterNot (_.name == sp.name))
    }
  }

  def botPlaceNVA(candidates: List[Space]): Unit = {
    if (candidates.nonEmpty && game.availablePieces.has(NVATroops)) {
      val sp = NVA_Bot.pickSpacePlaceTroops(candidates)
      val num = game.availablePieces.totalOf(NVATroops) min 2
      placePieces(sp.name, Pieces(nvaTroops = num))
      botPlaceNVA(candidates filterNot (_.name == sp.name))
    }
  }

  def humanPlaceVC(candidates: List[String]): Unit = {
    val numSpaces = candidates.size
    def numAvail  = game.availablePieces.totalOf(VCGuerrillas_U)

    val num = if (numAvail >= numSpaces)
      numSpaces
    else {
      if (numSpaces == 1)
        println(s"\nThere is 1 space at Active Opposition")
      else
        println(s"\nThere are $numSpaces spaces at Active Opposition")
      Human.numToPlace(VCGuerrillas_U, numSpaces)
    }

    if (numAvail >= numSpaces) {
      for (name <- candidates)
          placePieces(name, Pieces(vcGuerrillas_U = 1))
    }
    else {
      def nextPlacement(candidates: List[String]): Unit = if (numAvail > 0) {
        println(s"\nNumber of available VC Guerrillas: $numAvail")
        val name = askSimpleMenu(candidates, "Place a VC Guerrilla in which space:").head
        placePieces(name, Pieces(vcGuerrillas_U = 1))
        nextPlacement(candidates filterNot (_ == name))
      }
      nextPlacement(candidates)
    }
  }

  def humanPlaceNVA(candidates: List[String]): Unit = {
    val numSpaces = candidates.size
    val maxNum    = numSpaces * 2
    def numAvail  = game.availablePieces.totalOf(NVATroops)

    val num = if (numAvail >= maxNum)
      maxNum
    else {
      println("\nThere are not enough available NVA Troops to place 2 in each space")
      Human.numToPlace(NVATroops, maxNum)
    }

    if (numAvail >= maxNum) {
      for (name <- candidates)
        placePieces(name, Pieces(nvaTroops = 2))
    }
    else {
      def nextPlacement(candidates: List[String]): Unit = if (numAvail > 0) {
        val (name, num) = if (numAvail == candidates.size * 2)
          (candidates.head, 2)
        else {
          println(s"\nNumber of available NVA Troops: $numAvail")
          val name = askSimpleMenu(candidates, "Place NVA Troops in which space:").head
          val num = askInt(s"Place how many Troops in $name", 1, 2 min numAvail)
          (name, num)
        }
        placePieces(name, Pieces(nvaTroops = num))
        nextPlacement(candidates filterNot (_ == name))
      }
      nextPlacement(candidates)
    }
  }


  def executeShaded(faction: Faction): Unit = {

    val vcSpaces = activeOppSpaces
    val nvaSpaces = game.nonLocSpaces filter (sp => isInLaosCambodia(sp.name))

    if (vcSpaces.isEmpty)
      log("There are no spaces at Active Opposition")
      
    loggingControlChanges {
      if (game.isHuman(faction)) {
        humanPlaceVC(spaceNames(vcSpaces))
        humanPlaceNVA(spaceNames(nvaSpaces))
      }
      else {
        botPlaceVC(vcSpaces)
        botPlaceNVA(nvaSpaces)
      }
    }
  }
}
