
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
// US sends study teams: 2 US pieces from out-of-play to South Vietnam,
// or transfer a die roll from Patronage to ARVN Resources. Aid +6.
//
// Shaded Text
// Duped: Remove Support from a COIN-Controlled City outside Saigon. Patronage +4 or VC Resources +4.
//
// Tips
// "Remove Support" means set either Active or Passive Support to Neutral (1.6.2).
// Note that either unshaded or shaded versions of the Event may be useful to the ARVN Faction.

object Card_063 extends EventCard(63, "Fact Finding",
  DualEvent,
  List(ARVN, US, NVA, VC),
  ListMap(US   -> (Performed -> Unshaded),
          ARVN -> (Critical  -> Shaded),
          NVA  -> (Performed -> Shaded),
          VC   -> (Performed -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.arvnPoints >= 42 || game.outOfPlay.has(USPieces) || game.usAid < EdgeTrackMax

  def executeUnshaded(faction: Faction): Unit = {
    val numOopUS = game.outOfPlay.totalOf(USPieces)
    val action = if (game.isHuman(faction)) {
      val choices = List(
        "oopUS"     -> "Move 2 US Pieces from Out of Play to South Vietnam",
        "patronage" -> "Transfer a die roll from Patronage to ARVN Resources"
      )
      askMenu(choices, "\nChoose one:").head
    }
    else {
      // Note: US is only Bot that does the unshaded event.
      if (game.arvnPoints >= 42) "patronage" else "oopUS"
    }

    action match {
      case "oopUS" if numOopUS == 0 => log("\nThere are no Out of Play US pieces")
      case "oopUS" => placeOutOfPlayPiecesOnMap(faction, 2 min numOopUS, USPieces, SouthVietnam)
      case _       =>
        val die = d6
        val num = d6 min game.patronage
        log(s"\nRolling d6 to determine amount of transfer: $die")
        log(separator())
        decreasePatronage(num)
        increaseResources(ARVN, num)
    }

    log()
    increaseUsAid(6)
  }

  val shadedCandidate = (sp: Space) =>
    sp.isCity          &&
    sp.name != Saigon  &&
    sp.coinControlled  &&
    sp.support > Neutral

  def shadedEffective(faction: Faction): Boolean = game.spaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    sealed trait Action
    case object Patronage extends Action
    case object Resources extends Action 
    val candidates = game.spaces filter shadedCandidate
    def setCity(name: => String) = if (candidates.nonEmpty) Some(name) else None

    val (city, action) = if (game.isHuman(faction)) {
      val choices = List(Patronage -> "+4 Patronage", Resources -> "+4 VC Resources")
      val city    = setCity(askCandidate("\nRemove support from which City: ", spaceNames(candidates)))
      val action  = askMenu(choices, "\nChoose one:").head
      (city, action)
    }
    else if (faction == ARVN) {
      val priorities = List(new Bot.LowestScore[Space]("Pop", _.population))
      val city = setCity(Bot.bestCandidate(candidates, priorities).name)
      (city, Patronage)
    }
    else {
      // NVA or VC
      val city = setCity(Bot.pickSpaceWithMostSupport(candidates).name)
      (city, Resources)
    }

    println()
    loggingPointsChanges {
      city match {
        case Some(name) => setSupport(name, Neutral)
        case None => log("There are no COIN controlled Cities with Support")
      }
  
      action match {
        case Patronage => increasePatronage(4)
        case Resources => increaseResources(VC, 4)
      }
    }
  }
}
