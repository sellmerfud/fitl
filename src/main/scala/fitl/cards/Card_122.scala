
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

// NVA Pivotal event
//
// Play if 2+ cards in RVN Leader box and more NVA Troops than US Troops on map.
//
// Invasion: NVA free Marches. Then NVA Troops on LoCs with no US/ARVN may move 1 space.
// Then all NVA Troops free Attack.
//
// Tips
// Do not count Minh as a card for the precondition. “On map” means in South Vietnam,
// North Vietnam, Laos, and Cambodia combined. NVA March may include the usual multiple
// moves in or out of Laos/Cambodia spaces (unless the Trail is at “0”, 3.3.2). NVA Troops
// must Attack wherever there are enemies; NVA Guerrillas may March but do not Attack.

object Card_122 extends EventCard(122, "Easter Offensive",
  SingleEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Critical    -> Unshaded),
          VC   -> (Ignored -> Unshaded))) {


  def humanMoveTroops(locsWithTroops: List[String]): Unit = {
    val moved = new MovingGroups()

    def numCanMoveFrom(name: String): Int =
      (game.getSpace(name).pieces.only(NVATroops) - moved(name)).total

    val canMoveFrom = (name: String) => numCanMoveFrom(name) > 0

    def moveTroopsFrom(origin: String): Unit = if (canMoveFrom(origin)) {
      val dests = getAdjacent(origin).toList.sorted(SpaceNameOrdering)
      val choices = (dests map (n => n ->n )) :+ "finished" -> s"Finished moving NVA Troops from $origin"

      println(s"\nMoving NVA Troops off of $origin")
      println(separator())
      askMenu(choices, "Select destination:").head match {
        case "finished" =>
        case dest =>
          val num = askInt(s"\nMove how many Troops to $dest", 0, numCanMoveFrom(origin))
          if (num > 0) {
            val troops = Pieces(nvaTroops = num)
            println()
            movePieces(troops, origin, dest)
            moved.add(dest, troops)
          }
          moveTroopsFrom(origin)
      }
    }

    def nextAction(): Unit = {
      val origins = locsWithTroops filter canMoveFrom
      val choices = (origins map (n => n -> n)) :+ ("finished" -> "Finished moving NVA Troops from LoCs")

      askMenu(choices, "\nSelect LoC from which to move NVA Troops:").head match {
        case "finished" =>
        case origin     => moveTroopsFrom(origin)
      }
    }

    nextAction()
  }

  def botMoveTroops(locsWithTroops: List[String]): Unit = {

    def nextMove(locs: List[String]): Unit = locs match {
      case Nil =>

      case origin::rest if !game.getSpace(origin).pieces.has(NVATroops) =>
        nextMove(rest)

      case origin::rest =>
        Bot.initTurnVariables()
        val dest   = NVA_Bot.pickSpaceMarchDest(spaces(getAdjacent(origin))).name
        val troops = Bot.movePiecesFromOneOrigin(origin, dest, NVA, EventMove(None), Set(NVATroops), Bot.NO_LIMIT, Params())

        if (troops.isEmpty)
          nextMove(rest)  // No more troops that the Bot wants to move from here
        else {
          movePieces(troops, origin, dest)
          nextMove(locs)  // Retry with same origin in case there are more troops to move elsewhere
        }
    }

    nextMove(locsWithTroops)
  }

  // Is NVA pivotal event playable?
  def unshadedEffective(faction: Faction): Boolean =
    game.numCardsInLeaderBox >= 2 &&
    game.totalOnMap(_.pieces.totalOf(NVATroops)) > game.totalOnMap(_.pieces.totalOf(USTroops))

  def executeUnshaded(faction: Faction): Unit = {
    val params = Params(event = true, free = true)

    log("\nNVA free Marches")
    log(separator(char = '='))

    if (game.isHuman(NVA))
      Human.executeMarch(NVA, params)
    else
      NVA_Bot.marchOp(params, 6, withLoC = false, withLaosCambodia = false, easterOffensive = true)

    val locsWithTroops = spaceNames(game.locSpaces filter (sp => sp.pieces.has(NVATroops) && !sp.pieces.has(CoinPieces)))
    log("\nNVA Troops on LoCs with no US/ARVN forces may move 1 space")
    log(separator(char = '='))
    if (locsWithTroops.isEmpty)
      log("There are no NVA Troops on LoCs without US/ARVN forces")
    else if (game.isHuman(NVA))
      humanMoveTroops(locsWithTroops)
    else
      botMoveTroops(locsWithTroops)

    val attackSpaces = spaceNames(game.spaces filter { sp => sp.pieces.has(NVATroops) && sp.pieces.has(CoinPieces) })

    log("\nAll NVA Troops free Attack")
    log(separator(char = '='))
    if (attackSpaces.isEmpty)
      log("There are no spaces where NVA Troops can attack")
    else if (game.isHuman(NVA))
      Human.easterOffensiveAttack(attackSpaces)
    else
      NVA_Bot.easterOffensiveAttack(attackSpaces)
  }

  // Shaded functions not used for Pivotal Event  
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
