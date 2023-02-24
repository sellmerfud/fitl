
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
// Cadres assassinated: Remove any 3 VC pieces total from
// any COIN Control spaces.
//
// Shaded Text
// Misdirected: Add a Terror marker to any 2 spaces outside Saigon
// with COIN Control and VC. Set them to Active Opposition.
//
// Tips
// "Pieces" include unTunneled Bases (1.4, 5.1.1).

object Card_027 extends EventCard(27, "Phoenix Program",
  DualEvent,
  List(US, VC, ARVN, NVA),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val isPhoenixUnshadedSpace = (sp: Space) =>
    !sp.isLoC         &&
    sp.coinControlled &&
    sp.pieces.except(VCTunnel).has(VCPieces)

  def unshadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists isPhoenixUnshadedSpace

  def executeUnshaded(faction: Faction): Unit = {
    val VCTypes = VCPieces.toSet - VCTunnel
    val validSpaces = game.nonLocSpaces filter isPhoenixUnshadedSpace

    loggingControlChanges {
      if (validSpaces.isEmpty)
        log("There are no spaces that meet the event criteria")
      else if (game.isHuman(faction))
        removePiecesFromMap(faction, 3, VCTypes, false, spaceNames(validSpaces))
      else {
        // ARNV bot will target VC bases first
        val baseSpaces = validSpaces filter (_.pieces.has(VCBase))
        val numBases   = (baseSpaces map (_.pieces.totalOf(VCBases))).sum min 3
        val numOthers  = 3 - numBases
        removePiecesFromMap(faction, numBases, Set(VCBase), false, spaceNames(baseSpaces))
        removePiecesFromMap(faction, numOthers, VCTypes, false, spaceNames(validSpaces))
      }
    }
  }

  val isPhoenixShadedSpace = (sp: Space) =>
    !sp.isLoC               &&
    sp.name != Saigon       &&
    sp.coinControlled       &&
    sp.pieces.has(VCPieces) &&
    (game.terrorMarkersAvailable > 0 || sp.support != ActiveOpposition)

  def shadedEffective(faction: Faction): Boolean =
    game.nonLocSpaces exists isPhoenixShadedSpace

  def executeShaded(faction: Faction): Unit = {
    val validSpaces = game.nonLocSpaces filter isPhoenixShadedSpace

    val selectedSpaces = if (validSpaces.isEmpty)
      Nil
    else if (validSpaces.size <= 2)
      spaceNames(validSpaces)
    else if (game.isHuman(faction)) {
      val choices = spaceNames(validSpaces) map (n => n ->n )
      askMenu(choices, s"\nChoose 2 spaces to set to Active Opposition:", numChoices = 2)
    }
    else  // Bot
      spaceNames(Bot.pickSpaces(2, validSpaces)(VC_Bot.pickSpaceTowardActiveOpposition))


    loggingPointsChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces that meet the event criteria")
      else
        for (name <- selectedSpaces) {
          log()
          setSupport(name, ActiveOpposition)
          if (game.terrorMarkersAvailable > 0)
            addTerror(name, 1)
          else
            log(s"There are no available terror markers to add to $name")
        }
    }
  }
}
