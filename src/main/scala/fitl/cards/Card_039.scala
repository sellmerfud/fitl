
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
// "Alpha" strikes on North Vietnam: Remove any 4 pieces from North Vietnam or,
// once none, Laos. Degrade Trail 2 boxes.
//
// Shaded Text
// Explosion on CV-34: 1 Available US Troop out of play.
// Through next Coup, no Degrade of Trail.
// MOMENTUM
//
// Tips
// "Pieces" include unTunneled Bases as well as Troops and Guerrillas (1.4, 5.1.1).
// The US Troop goes out of play immediately; the ban on Degrade of Trail lasts through
// the end of the next Coup Round (so blocks effects of COIN Control in Laos/Cambodia
// and would leave "4" at "4", 6.2.2 & 6.6).

object Card_039 extends EventCard(39, "Oriskany",
  DualEvent,
  List(NVA, US, VC, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  
  val Laos = CentralLaos::SouthernLaos::Nil
  val removeTypes = InsurgentNonTunnels:::InsurgentForces:::CoinForces:::CoinBases

  def targetPieces(sp: Space) = sp.pieces.only(removeTypes)

  def hasTargets(name: String) = targetPieces(game.getSpace(name)).nonEmpty

  def unshadedEffective(faction: Faction): Boolean = 
    hasTargets(NorthVietnam) ||
    (Laos exists hasTargets) ||
    game.trail > TrailMin

  def executeUnshaded(faction: Faction): Unit = {
    loggingControlChanges {
      val laosSpaces = Laos filter hasTargets
      val numNVietnam = if (hasTargets(NorthVietnam)) {
        val targets = targetPieces(game.getSpace(NorthVietnam))
        val num     = targets.total min 4
  
        println("")
        removePiecesFromMap(faction, num, removeTypes, false, Set(NorthVietnam))
        num
      }
      else
        0
  
      if (numNVietnam < 4 && laosSpaces.nonEmpty)
        removePiecesFromMap(faction, 4 - numNVietnam, removeTypes, false, laosSpaces)
    }

    if (game.trail > TrailMin) {
      log()
      degradeTrail(2)
    }
  }

  def shadedEffective(faction: Faction): Boolean = true

  def executeShaded(faction: Faction): Unit = {
    if (game.availablePieces.has(USTroops))
      moveAvailableToOutOfPlay(Pieces(usTroops = 1))
    playMomentum(Mo_Oriskany)
  }
}
