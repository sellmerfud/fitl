
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
// Fortification mentality: Redeploy all COIN forces outside Vietnam to
// COIN-Controlled Cities. ARVN Resources –12. No Infiltrate or Trail
// Improvement by Rally until Coup.
// MOMENTUM
//
// Tips
// "Outside Vietnam" means Laos and Cambodia Provinces. The executing Faction
// gets to choose which redeploying pieces go to which COIN-Controlled Cities (5.1).
// Redeploy and Resource drop occur instantly and once; the effects on Infiltrate and
// Rally last until the next Coup Round (5.4).

object Card_038 extends EventCard(38, "McNamara Line",
  SingleEvent,
  List(NVA, US, VC, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Unshaded),
          VC   -> (Ignored -> Unshaded))) {


  def unshadedEffective(faction: Faction): Boolean = true

  def executeUnshaded(faction: Faction): Unit = {
    val withForces = game.spaces filter { sp =>
      isInLaosCambodia(sp.name) &&
      sp.pieces.has(CoinForces)
    }

    // Function so we get fresh spaces each time.
    // The bot code needs this to determine where to
    // place each piece.
    def coinControlledCities = game.citySpaces filter (_.coinControlled)

    if (withForces.isEmpty)
      log("\nThere are no COIN forces in Laos/Cambodia to redeploy")
    else if (coinControlledCities.isEmpty)
      log("\nCannot redeploy COIN forces because there are no COIN controlled cities")
    else if (game.isHuman(faction)) {
      for (origin <- withForces) {
        var forces = origin.pieces.only(CoinForces)
        while (forces.nonEmpty) {
          println(s"\nRedeploying COIN forces out of ${origin.name}")
          println(separator())
          println(andList(forces.descriptions))
          val city   = askCandidate("Redeploy to which city: ", spaceNames(coinControlledCities))
          val num    = askInt(s"Move how many of these pieces to $city", 0, forces.total)
          val movers = askPieces(forces, num)
          log()
          movePieces(movers, origin.name, city)
          forces = forces - movers
        }
      }
    }
    else {  // Bot
      for (origin <- withForces) {
        var forces = origin.pieces.only(CoinForces)
        while (forces.nonEmpty) {
          val city  = Bot.pickSpacePlaceForces(faction, forces.has(USTroops))(coinControlledCities)
          val mover = Bot.selectFriendlyToPlaceOrMove(forces, 1)
          movePieces(mover, origin.name, city.name)
          forces = forces - mover
        }
      }
    }

    log()
    decreaseResources(ARVN, 12)
    playMomentum(Mo_McNamaraLine)
  }

  // Single event - These functions are not used
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
