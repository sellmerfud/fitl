
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

// Single Event Text
// Subterranean specialists: Place a Tunnel marker on an Insurgent Base in
// each of 2 Provinces, or remove 1 Tunneled Base from a space with US Troops.
//
// Tips
// This is one of just a few Events that can force removal of Tunneled Bases (5.1.1).

object Card_094 extends EventCard(94, "Tunnel Rats",
  SingleEvent,
  List(VC, US, NVA, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Unshaded),
          VC   -> (Ignored -> Unshaded))) {

  val tunnelWithUSTroops = (sp: Space) =>
    sp.pieces.has(InsurgentTunnels) &&
    sp.pieces.has(USTroops)

  val provinceWithNVANonTunnel = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(NVABases)

  val provinceWithNonTunnel = (sp: Space) =>
    sp.isProvince &&
    sp.pieces.has(InsurgentNonTunnels)

  def humanAdd(count: Int): Unit = {
    val candidates = spaceNames(game.nonLocSpaces filter provinceWithNonTunnel)
    if (count <= 2 && candidates.nonEmpty) {
      if (game.tunnelMarkersAvailable > 0) {
        val name = askSimpleMenu(candidates, s"\nSelect space to place ${ordinal(count)} Tunnel marker:").head
        val bases = game.getSpace(name).pieces.only(InsurgentNonTunnels)
        val base  = if (bases.vcBases > 0 && bases.nvaBases > 0) {
          val choices = List(Pieces(vcBases = 1) -> "VC Base", Pieces(nvaBases = 1) -> "NVA Base")
          askMenu(choices, s"\nSelect base in $name to receive Tunnel marker:").head
        }
        else if (bases.vcBases > 0)
          Pieces(vcBases = 1)
        else
          Pieces(nvaBases = 1)
  
        println()
        addTunnelMarker(name, base)
        humanAdd(count + 1)
      }
      else
        log("\nThere are no more Tunnel markers available")
    }
  }

  def humanRemove(): Unit = {
    val candidates = spaceNames(game.nonLocSpaces filter tunnelWithUSTroops)
    if (candidates.isEmpty)
      log("\nThere are no Tunneled bases in a space with US Troops")
    else {
      val name = askSimpleMenu(candidates, "\nRemove Tunneled base from which space:").head
      val tunnels = game.getSpace(name).pieces.only(InsurgentTunnels)
      val tunnel =  if (tunnels.vcTunnels > 0 && tunnels.nvaTunnels > 0) {
        val choices = List(Pieces(vcTunnels = 1) -> "VC Tunneled Base", Pieces(nvaTunnels = 1) -> "NVA Tunneled Base")
        askMenu(choices, s"\nSelect Tunneled Base to remove in $name:").head
      }
      else if (tunnels.vcTunnels > 0)
        Pieces(vcTunnels = 1)
      else
        Pieces(nvaTunnels = 1)
  
      println()
      removePieces(name, tunnel)
    }
  }

  def botAdd(numRemaining: Int): Unit = {
    val candidates = game.nonLocSpaces filter provinceWithNVANonTunnel

    if (numRemaining > 0 && candidates.nonEmpty) {
      if (game.tunnelMarkersAvailable > 0) {
        val sp = NVA_Bot.pickSpacePlaceBases(candidates)
        addTunnelMarker(sp.name, Pieces(nvaBases = 1))
      }
      else
        log("\nThere are no more Tunnel markers available")
      botAdd(numRemaining - 1)
    }
  }

  def botRemove(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter tunnelWithUSTroops
    val sp         = Bot.pickSpaceRemoveReplace(faction)(candidates)
    val tunnel     = Bot.selectEnemyRemoveReplaceActivate(sp.pieces.only(InsurgentTunnels), 1)

    removePieces(sp.name, tunnel)
  }

  def unshadedEffective(faction: Faction): Boolean = faction match {
    case US | ARVN => game.nonLocSpaces exists tunnelWithUSTroops
    case NVA       => (game.nonLocSpaces exists provinceWithNVANonTunnel) &&
                      game.tunnelMarkersAvailable > 0
    case VC        => false
  }

  def executeUnshaded(faction: Faction): Unit = {
    val action = if (game.isHuman(faction)) {
      val choices = List(
      "add"    -> "Place Tunnel markers on Insurgent bases",
      "remove" -> "Remove a Tunneled Base")
      askMenu(choices, "\nChoose one:").head
    }
    else if (faction == NVA)
      "add"
    else
      "remove"

    loggingControlChanges {
      (game.isHuman(faction), action) match {
        case (true ,  "add") => humanAdd(1)
        case (true , _     ) => humanRemove()
        case (false,  "add") => botAdd(2)
        case (false, _     ) => botRemove(faction)
      }
    }
  }

  // Single event - These functions are not used
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
