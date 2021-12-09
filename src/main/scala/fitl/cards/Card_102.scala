
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
// Clear and secure: Remove all Guerrillas from 1 space with
// a Tunnel and COIN Control.
//
// Shaded Text
// Iron Triangle: Place Tunnel markers on each Insurgent Base in
// 1 Province. Place 1 NVA and 1 VC Guerrilla there.
//
// Tips
// "A Tunnel" means a Tunneled Base.

object Card_102 extends EventCard(102, "Cu Chi",
  DualEvent,
  List(VC, NVA, US, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.coinControlled &&
    sp.pieces.has(InsurgentTunnels) &&
    sp.pieces.has(Guerrillas)

  def unshadedEffective(faction: Faction): Boolean = game.nonLocSpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter unshadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nSelect space to carry out the event:").head
      else
        Bot.pickSpaceRemoveReplace(faction)(candidates).name

      val sp = game.getSpace(name)
      removePieces(name, sp.pieces.only(Guerrillas))
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(InsurgentNonTunnels)

  def shadedEffective(faction: Faction): Boolean =
    (game.nonLocSpaces exists shadedCandidate) &&
    (game.tunnelMarkersAvailable > 0 || game.availablePieces.has(Guerrillas))

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nSelect space to carry out the event:").head
      else
        NVA_Bot.pickSpacePlaceBases(candidates).name

      println()
      val sp    = game.getSpace(name)
      val bases = sp.pieces.only(InsurgentNonTunnels)
      if (game.tunnelMarkersAvailable == 0)
        log("There are no available Tunnel markers")
      else {
        val toTunnel = if (game.tunnelMarkersAvailable >= bases.total)
          bases
        else if (bases.has(NVABase) && bases.has(VCBases)) {
          if (game.isHuman(faction)) {
            val choices = List(
              Pieces(vcBases  = 1) -> "VC Base",
              Pieces(nvaBases = 1) -> "NVA Base"
            )
            println("\nThere is only one Tunnel marker available")
            askMenu(choices, "Place Tunnel marker on which base:").head
          }
          else
            Pieces(nvaBases = 1)  // NVA Bot picks its own base
        }
        else {
          // Two of same type of base
          val baseType = bases.explode().head
          Pieces().set(1, baseType)
        }

        loggingControlChanges {
          addTunnelMarker(name, toTunnel)
          val guerrillas = Pieces(
           nvaGuerrillas_U =  game.availablePieces.totalOf(NVAGuerrillas_U) min 1,
           vcGuerrillas_U  =  game.availablePieces.totalOf(VCGuerrillas_U)  min 1)
          placePieces(name, guerrillas)
        }
      }
    }
  }
}
