
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
// Hearings stoke debate: US moves 4 US pieces from map to Available.
//
// Shaded Text
// War skeptic: 1 Available US Base out of play. Aid –9.
//
// Tips
// A Base is a "piece".

object Card_093 extends EventCard(93, "Senator Fulbright",
  DualEvent,
  List(VC, US, NVA, ARVN),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = false  // Bots never execute

  def executeUnshaded(faction: Faction): Unit = {

    def humanRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter (_.pieces.has(USPieces)))
      println(s"\nNumber of US piece removed to available: ${4 - numRemaining} of 4")
      println(separator())
      val name     = askSimpleMenu(candidates, "\nSelect space to remove US pieces:").head
      val usPieces = game.getSpace(name).pieces.only(USPieces)
      val num      = askInt(s"\nRemove how many pieces from $name", 0, usPieces.total min numRemaining)
      val pieces = askPieces(usPieces, num)

      removeToAvailable(name, pieces)
      humanRemoval(numRemaining - num)
    }

    // In the unlikely change that a Human player will select the unshaded
    // event when the US is a Bot player.
    def botRemoval(numRemaining: Int): Unit = {
      val candidates = game.spaces filter (_.pieces.has(USPieces))
      if (numRemaining > 0 && candidates.nonEmpty) {
        val sp = Bot.pickSpaceRemoveFriendlyPieces(candidates, USPieces)
        val piece = Bot.selectFriendlyRemoval(sp.pieces.only(USPieces), 1)
        removeToAvailable(sp.name, piece)
        botRemoval(numRemaining - 1)
      }
    }

    val numOnMap   = game.totalOnMap(_.pieces.totalOf(USPieces))
    
    loggingControlChanges {
      if (numOnMap == 0)
        log("There are no US pieces on the map")
      else if (numOnMap <= 4) {
        for (sp <- (game.spaces filter (_.pieces.has(USPieces))))
          removeToAvailable(sp.name, sp.pieces.only(USPieces))
      }
      else if (game.isHuman(US))
        humanRemoval(4)
      else
        botRemoval(4)
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(USBase)

  def executeShaded(faction: Faction): Unit = {

    loggingPointsChanges {
      if (game.availablePieces.has(USBase))
        moveAvailableToOutOfPlay(Pieces(usBases = 1))
      else
        log("There are no available US Bases")
      decreaseUsAid(9)
    }
  }
}
