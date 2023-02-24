
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
// Civilian Irregular Defense Groups: Replace a die roll of
// VC Guerrillas in South Vietnam with Rangers, Irregulars, or Police.
//
// Shaded Text
// Desertions and defections: Replace all Rangers, Police, and 
// Irregulars in a Highland space with 2 VC Guerrillas total.
//
// Tips
// For the shaded text, the Irregulars (like the Rangers and Police) go
// to their Available box, not to Casualties (1.4.1).

object Card_081 extends EventCard(81, "CIDG",
  DualEvent,
  List(ARVN, VC, US, NVA),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val CoinTypes = ARVNPolice::Rangers_U::Irregulars_U::Nil

  def unshadedEffective(faction: Faction): Boolean =
    game.totalOnMap(_.pieces.totalOf(VCGuerrillas)) > 0 &&
    game.availablePieces.has(CoinTypes)

  def humanReplacements(numRemaining: Int): Unit = if (numRemaining > 0) {
    val avail   = game.availablePieces.only(CoinTypes)
    val toPlace = game.piecesToPlace.only(CoinTypes)
    val replacements = if (avail.nonEmpty)
      List(
      choice(avail.has(Irregulars_U), Irregulars_U, "US Irregular"),
      choice(avail.has(Rangers_U),    Rangers_U,    "ARVN Ranger"),
      choice(avail.has(ARVNPolice),   ARVNPolice,   "ARVN Police"),
    ).flatten
    else
      List(
      choice(toPlace.has(Irregulars_U), Irregulars_U, "US Irregular"),
      choice(toPlace.has(Rangers_U),    Rangers_U,    "ARVN Ranger"),
      choice(toPlace.has(ARVNPolice),   ARVNPolice,   "ARVN Police"),
    ).flatten
    val choices     = spaceNames(game.spaces filter (_.pieces.has(VCGuerrillas)))
    val name        = askSimpleMenu(choices, "\nReplace a VC Guerrilla in which space:").head
    val sp          = game.getSpace(name)
    val prompt      = "\nSelect Guerrilla to replace"
    val guerrilla   = askPieces(sp.pieces, 1, VCGuerrillas_U::VCGuerrillas_A::Nil, Some(prompt))
    val replacement = if (replacements.nonEmpty) {
      val replacementType = askMenu(replacements, "\nChoose replacemnt:").head
      val num             = Human.numToPlace(replacementType, 1)
      Pieces().set(num, replacementType)
    }
    else
      Pieces()
      
    removePieces(name, guerrilla)
    placePieces(name, replacement)
    humanReplacements(numRemaining - 1)
  }

  def botReplacements(faction: Faction, numRemaining: Int): Unit = if (numRemaining > 0) {
    val candidates   = game.spaces filter (_.pieces.has(VCGuerrillas))
    val sp           = Bot.pickSpaceRemoveReplace(faction)(candidates)
    val guerrilla    = Bot.selectEnemyRemoveReplaceActivate(sp.pieces.only(VCGuerrillas), 1)
    val avail        = game.availablePieces.only(CoinTypes)
    val replacement  = Bot.selectFriendlyToPlaceOrMove(avail, 1)

    removePieces(sp.name, guerrilla)
    placePieces(sp.name, replacement)
    botReplacements(faction, numRemaining - 1)
  }


  def executeUnshaded(faction: Faction): Unit = {
    val die = d6

    log(s"Rolling d6 to determine number of VC Guerrillas to replace: $die")
    log()
    val num = die min game.totalOnMap(_.pieces.totalOf(VCGuerrillas))

    loggingControlChanges {
      if (game.isHuman(faction))
        humanReplacements(num)
      else
        botReplacements(faction, num)
    }
  }

  val shadedCandidate = (sp: Space) =>
    sp.isHighland &&
    sp.pieces.has(CoinTypes)

  def shadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(VCGuerrillas) &&
    (game.nonLocSpaces exists shadedCandidate)

  def executeShaded(faction: Faction): Unit = {
    val candidates = game.nonLocSpaces filter shadedCandidate

    if (candidates.isEmpty)
      log("There are no spaces that qualify for the event")
    else {
      val name = if (game.isHuman(faction))
        askSimpleMenu(spaceNames(candidates), "\nReplace Rangers, Police, and Irregulars in which space:").head
      else
        VC_Bot.pickSpaceRemoveReplace(candidates).name

      val numAvail = game.availablePieces.totalOf(VCGuerrillas_U)
      val numGuerrillas = if (numAvail >= 2)
        2
      else if (game.isBot(faction))
        numAvail
      else {
        println("There are not two available VC Guerrillas")
        Human.numToPlace(VCGuerrillas_U, 2)
      }
  
      loggingControlChanges {
        val sp = game.getSpace(name)
        val toRemove = sp.pieces.only(CoinTypes)
        removeToAvailable(name, toRemove)  // Even Irregulars go to available
        placePieces(name, Pieces().set(numGuerrillas, VCGuerrillas_U))
      }
    }
  }
}
