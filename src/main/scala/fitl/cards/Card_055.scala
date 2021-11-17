
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
// Bottlenecks: Degrade Trail 2 boxes. NVA selects and removes 4 of
// its pieces each from Laos and Cambodia.
//
// Shaded Text
// Convoys: Add twice Trail value to each NVA and VC Resources.
// NVA moves its unTunneled Bases anywhere within Laos/Cambodia.
//
// Tips
// For the unshaded Event, NVA may have to remove as many as 8 pieces
// total—4 in Laos and 4 in Cambodia; "pieces" include Bases. For shaded,
// NVA may pick up all its unTunneled Bases from Laos and Cambodia spaces,
// then replace them into any Laos and Cambodia spaces desired, within
// stacking (1.4.2).

object Card_055 extends EventCard(55, "Trucks",
  DualEvent,
  List(NVA, VC, US, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {


  def unshadedEffective(faction: Faction): Boolean =
    game.trail > TrailMin ||
    (game.spaces exists (sp => sp.pieces.has(NVAPieces) && isInLaosCambodia(sp.name)))

  def executeUnshaded(faction: Faction): Unit = {
    degradeTrail(2)
    removePiecesFromMap(NVA, 4, NVAPieces, friendly = true, validSpaces = LaosCambodia)
  }

  def shadedEffective(faction: Faction): Boolean = false // Not executed by Bots

  def shadedCandidates = spaceNames(spaces(LaosCambodia) filter (_.pieces.has(NVABase)))

  def executeShaded(faction: Faction): Unit = {
    increaseResources(NVA, game.trail * 2)
    increaseResources(VC, game.trail * 2)

    def nextHumanMove(): Unit = {
      val candidates = shadedCandidates
      val choices = (candidates map (n => n -> n)) :+
                    ("finished" -> "Finished moving untunneled bases")
      askMenu(choices, "\nMove an untunneled Base in which space:").head match {
        case "finished" =>
        case name =>
          val targets = spaceNames(spaces(LaosCambodia) filter (sp => sp.name != name && sp.totalBases < 2))
          if (targets.isEmpty)
            println("\nThere is no other space in Laos/Cambodia that can take a base")
          else {
            val choices = (targets map (n => n -> n)) :+ ("cancel" -> s"Do not move the base from $name")
            askMenu(choices, "\nMove the base to which space:").head match {
              case "cancel" =>
              case target =>
                log()
                movePieces(Pieces(nvaBases = 1), name, target)
            }
          }
          nextHumanMove()
      }
    }

    val nakedBase = (sp: Space) => sp.pieces.has(NVABase) && !sp.pieces.has(NVAForces)

    // Only move bases that are not accompanied by NVA Troops/Guerrillas
    def nextBotMove(): Unit = {
      (spaces(LaosCambodia) filter nakedBase).headOption match {
        case None =>
        case Some(sp) =>
          val targets = (spaces(LaosCambodia) filter { t =>
            t.name != sp.name &&
            t.totalBases < 2  &&
            t.pieces.has(NVAForces)
          })
          if (targets.nonEmpty) {
            val target = NVA_Bot.pickSpacePlaceBases(targets)
            movePieces(Pieces(nvaBases = 1),  sp.name, target.name)
            nextBotMove();
          }
      }
    }

    if (shadedCandidates.isEmpty)
      log("\nThere are no untunneled NVA Bases in Laos/Cambodia")
    else if (game.isHuman(NVA))
      nextHumanMove()
    else
      nextBotMove()
  }
}
