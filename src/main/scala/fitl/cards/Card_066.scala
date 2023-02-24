
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
// Interventionist: Aid and ARVN Resources each +9.
// Up to 2 US pieces from out-of-play to South Vietnam or,
// if desired, Patronage -3.
//
// Shaded Text
// Saigon seen as US puppet: Remove Support from 3 spaces outside Saigon.
// Patronage –3.
//
// Tips
// The unshaded "if desired" means that the executing Faction may decline
// both the US pieces and the drop in Patronage. "Pieces" includes Bases.
// The shaded "remove Support" means remove any Active or Passive Support
// marker from the space, leaving it Neutral (1.6.2).

object Card_066 extends EventCard(66, "Ambassador Taylor",
  DualEvent,
  List(ARVN, US, VC, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.usAid < EdgeTrackMax ||
    (game.trackResources(ARVN) && game.arvnResources < EdgeTrackMax) ||
    game.outOfPlay.totalOf(USPieces) > 0 ||
    game.arvnPoints >= 42

  def executeUnshaded(faction: Faction): Unit = {
    sealed trait EventAction
    case object USOOP     extends EventAction
    case object PATRONAGE extends EventAction
    case object NOTHING   extends EventAction

    val action: EventAction = if (game.isHuman(faction)) {
      val choices = List(
        USOOP     -> "Up to 2 US Pieces from Out of Play to South Vietnam",
        PATRONAGE -> "Decrease Patronage -3",
        NOTHING   -> "None of the above"
      )
      askMenu(choices, "\nChoose one:").head
    }
    else if (game.arvnPoints >= 42)
      PATRONAGE
    else
      USOOP

    println()
    loggingControlChanges {
      increaseUsAid(9)
      increaseResources(ARVN, 9)
      action match {
        case USOOP =>
          val maxUS = game.outOfPlay.totalOf(USPieces) min 2
          val num = if (game.isHuman(faction))
            askInt("Place how many Out of Play US Pieces", 0, maxUS)
          else
            maxUS
          placeOutOfPlayPiecesOnMap(faction, num, USPieces, SouthVietnam)
        case PATRONAGE =>
          decreasePatronage(3)
        case NOTHING => 
      }
    }
  }

  def shadedCandidate = (sp: Space) =>
    sp.name != Saigon &&
    sp.canHaveSupport &&
    sp.support > Neutral

  def shadedEffective(faction: Faction): Boolean =
    (game.nonLocSpaces exists shadedCandidate) ||
    game.arvnPoints >= 42

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    val selectedSpaces = if (candidates.isEmpty)
      Nil
    else if (candidates.size <= 3)
      spaceNames(candidates)
    else if (game.isHuman(faction)) {
      val choices = spaceNames(candidates) map (n => n -> n)
      askMenu(choices, "\nRemove Support from 3 spaces outside Saigon:", numChoices = 3)
    }
    else
      spaceNames(Bot.pickSpaces(3, candidates)(Bot.pickSpaceWithMostSupport))

    println()
    loggingPointsChanges {
      if (selectedSpaces.isEmpty)
        log("There are no spaces outside of Saigon with Support")
      else
        for (name <- selectedSpaces)
          setSupport(name, Neutral)
      decreasePatronage(3)
    }
  }
}
