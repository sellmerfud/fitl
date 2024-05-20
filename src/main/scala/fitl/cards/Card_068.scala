
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
// Elite trainers: Place 3 Irregulars or 3 Rangers in a Province
// without NVA Control. Set it to Active Support.
//
// Shaded Text
// Reluctant trainees: Remove any 3 Irregulars to Available and
// set 1 of their Provinces to Active Opposition.
//
// Tips
// A Province with 0 Population cannot be set to Support or Opposition (1.6).

object Card_068 extends EventCard(68, "Green Berets",
  DualEvent,
  List(ARVN, US, VC, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.isProvince &&
    !sp.nvaControlled

  def unshadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(SpecialForces) &&
    (game.nonLocSpaces exists unshadedCandidate)

  def executeUnshaded(faction: Faction): Unit = {
    val candidates     = game.nonLocSpaces filter unshadedCandidate
    val irregsToPlace  = game.piecesToPlace.totalOf(Irregulars)
    val rangersToPlace = game.piecesToPlace.totalOf(Rangers)
    val irregsAvail    = game.availablePieces.totalOf(Irregulars)
    val rangersAvail   = game.availablePieces.totalOf(Rangers)

    if (candidates.isEmpty)
      log("There are no Provinces that meet the event requirements")
    else if (irregsToPlace + rangersToPlace == 0)
      log("There are no Irregulars or Rangers in the Available box or on the map")
    else {
      if (game.isHuman(faction)) {
        val choices = List(
          choice(irregsToPlace  > 0, Irregulars_U, "Irregulars"),
          choice(rangersToPlace > 0, Rangers_U,    "Rangers")
        ).flatten
        val forceType = askMenu(choices, "\nPlace Irregulars or Rangers?").head
        val name = askCandidate(s"\nPlace ${forceType.genericPlural} in which Province: ", spaceNames(candidates))
        println()
        loggingControlChanges {
          val num = Human.numToPlace(forceType, 3)
          val pieces = Pieces().set(num, forceType)
          placePieces(name, pieces)
          setSupport(name, ActiveSupport)
        }
      }
      else {  // Bot
        val pieces = if (irregsAvail > rangersAvail)
          Pieces(irregulars_U = irregsAvail min 3)
        else
          Pieces(rangers_U = rangersAvail min 3)
        val name = US_Bot.pickSpaceTowardActiveSupport(candidates).name
        loggingControlChanges {
          placePieces(name, pieces)
          setSupport(name, ActiveSupport)
        }
      }
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.totalOnMap(_.pieces.totalOf(SpecialForces)) > 0

  def executeShaded(faction: Faction): Unit = {
    if (game.totalOnMap(_.pieces.totalOf(SpecialForces)) > 0) {
      val removedFrom = removePiecesFromMap(faction, 3, Irregulars, false, spaceNames(game.spaces), usToAvailable = true)
      val candidates = spaces(removedFrom) filter { sp =>
        sp.isProvince &&
        sp.canHaveSupport &&
        sp.support != ActiveOpposition
      }
  
      if (candidates.isEmpty)
        log("\nNone of the provinces where Irregulars were removed can be set to Active Opposition")
      else {
        val name = if (game.isHuman(faction))
          askSimpleMenu(spaceNames(candidates), "\nSet which province to Active Opposition:").head
        else
          VC_Bot.pickSpaceTowardActiveOpposition(candidates).name

        log()
        setSupport(name, ActiveOpposition)
      }
    }
    else
      log("There are no Irregulars on the map")
  }
}
