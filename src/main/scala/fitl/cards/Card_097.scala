
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
// NLF terror reconciles GVN-US: Aid +10, or 4 Patronage to ARVN Resources.
// Flip any current RVN leader card—its text is ignored.
//
// Shaded Text
// US billet car bombed: Shift a City that has VC by 2 levels toward
// Active Opposition and add a Terror marker there.
//
// Tips. Unshaded text has no effect on “Duong Van Minh” or “Failed Attempt”;
// a flipped RVN leader card still counts as a card in the RVN leader box, such
// as for Pivotal Event pre-conditions. For shaded, any VC piece in the City
// (Guerrilla or Base) makes the space a candidate; the Terror marker would
// be added even if one is already there.

object Card_097 extends EventCard(97, "Brinks Hotel",
  DualEvent,
  List(VC, US, ARVN, NVA),
  ListMap(US   -> (Performed -> Unshaded),
          ARVN -> (Performed -> Unshaded),
          NVA  -> (Performed -> Unshaded),
          VC   -> (Critical  -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean = faction match {
    case ARVN => isRVNLeader(RVN_Leader_NguyenKhanh)
    case _    => game.patronage > 0  // US/NVA
  }
  def executeUnshaded(faction: Faction): Unit = {
    val choices = List(
      "aid"       -> "Increase US Aid +10",
      "patronage" -> "Transfer 4 Patronage to ARVN resources"
    )
    val action = if (game.isHuman(faction))
      askMenu(choices, "\nChoose one:").head
    else if (faction == ARVN)
      "aid"
    else
      "patronage"

    println()
    loggingPointsChanges {
      action match {
        case "aid" =>
          increaseUsAid(10)
        case _     =>
          val num = game.patronage min 4
            decreasePatronage(num)
            increaseResources(ARVN, num)
      }
          
      game.currentRvnLeader match {
        case RVN_Leader_DuongVanMinh|RVN_Leader_FailedCoup129|RVN_Leader_FailedCoup130 => // No effect
        case leader =>
          log(s"\nFlip RVN Leader face down [$leader]")         
          game = game.copy(rvnLeaderFlipped = true)
      }
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isCity &&
    sp.pieces.has(VCPieces) &&
    sp.support > ActiveOpposition

  def shadedEffective(faction: Faction): Boolean = game.citySpaces exists shadedCandidate

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.citySpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no Cities with VC pieces that can be shifted toward Active Opposition")
    else {
      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nChoose a City to shift toward Active Opposition:").head
      else
        VC_Bot.pickSpaceTowardActiveOpposition(candidates).name

      println()
      loggingPointsChanges {
        decreaseSupport(name, 2)
        if (game.terrorMarkersAvailable > 0)
          addTerror(name, 1)
      }
    }
  }
}
