
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
// Revolutionary land reform seeks traction in prosperous districts:
// In each of 2 Provinces adjacent to Saigon, shift Support/Opposition
// 1 level either direction and place a VC Guerrilla or Police.
//
// Tips
// Shifting toward Opposition and placing Police would be a legal (if unusual) move.

object Card_106 extends EventCard(106, "Binh Duong",
  SingleEvent,
  List(VC, NVA, ARVN, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Unshaded),
          VC   -> (Critical    -> Unshaded))) {

  def candidates     = spaces(getAdjacent(Saigon)) filter (_.isProvince)
  def coinCandidates = candidates filter (_.support < ActiveSupport)
  def vcCandidates   = candidates filter (_.support > ActiveOpposition)

  def unshadedEffective(faction: Faction): Boolean = faction match {
    case US|ARVN => coinCandidates.nonEmpty
    case _       => vcCandidates.nonEmpty
  }


  // Return true if shift toward Support or false if shift toward Opposition
  def askShift(name: String, faction: Faction): Boolean = {
    val sp = game.getSpace(name)
    val choices = List(
      choice(sp.support < ActiveSupport,    true,  "Shift 1 level toward Active Support"),
      choice(sp.support > ActiveOpposition, false, "Shift 1 level toward Active Opposition")
    ).flatten
    val sorted = if (isCoin(faction)) choices else choices.reverse
    askMenu(sorted, s"\nChoose shift for $name").head
  }

  // Ask if the user wishes to place a VC Guerrilla or Police in the space.
  // If the piece is not available then:
  // VC Guerrilla - VC has the option of voluntary removal
  // Police       - Coin factions have the option of voluntary removal
  // Otherwise    - If the other piece type is available it is selected
  def askPiece(name: String, faction: Faction): Pieces = {
    val haveGuerrilla = game.availablePieces.has(VCGuerrillas_U)
    val havePolice    = game.availablePieces.has(ARVNPolice)
    val canRemoveG    = !haveGuerrilla && faction == VC
    val canRemoveP    = !havePolice && isCoin(faction)

    val choices = List(
      choice(havePolice,    ARVNPolice,     "ARVN Police"),
      choice(canRemoveP,    ARVNPolice,     "ARVN Police (requires volunatary removal)"),
      choice(haveGuerrilla, VCGuerrillas_U, "VC Guerrilla"),
      choice(canRemoveG,    VCGuerrillas_U, "VC Guerrilla (requires volunatary removal)")
    ).flatten
    val sorted = if (isCoin(faction)) choices else choices.reverse

    if (sorted.isEmpty)
      Pieces()
    else {
      val pieceType = askMenu(sorted, s"\nChoose type of piece to place in $name").head
      if (!game.availablePieces.has(pieceType))
        voluntaryRemoval(1, pieceType, Set(name))
      Pieces().set(1, pieceType)
    }
  }

  def botPiece(faction: Faction): Pieces = {
    val haveGuerrilla = game.availablePieces.has(VCGuerrillas_U)
    val havePolice    = game.availablePieces.has(ARVNPolice)
    faction match {
      case VC if haveGuerrilla   => Pieces(vcGuerrillas_U = 1)
      case US|ARVN if havePolice => Pieces(arvnPolice = 1)
      case _ if haveGuerrilla    => Pieces(vcGuerrillas_U = 1)
      case _ if havePolice       => Pieces(arvnPolice = 1)
      case _                     => Pieces()
    }
  }

  def executeUnshaded(faction: Faction): Unit = {
    val selectedSpaces = if (game.isHuman(faction))
      askSimpleMenu(spaceNames(candidates), "\nSelect 2 Provinces adjacent to Saigon:", numChoices = 2)
    else
      faction match {
        case US   => Bot.pickSpaces(2, coinCandidates)(US_Bot.pickSpaceTowardActiveSupport) map (_.name)
        case ARVN => Bot.pickSpaces(2, coinCandidates)(ARVN_Bot.pickSpaceTowardPassiveSupport) map (_.name)
        case _    => Bot.pickSpaces(2, coinCandidates)(VC_Bot.pickSpaceTowardActiveOpposition) map (_.name)
      }

    loggingControlChanges {
      for (name <- selectedSpaces) {
        val sp = game.getSpace(name)
        val (towardSupport, piece) = if (game.isHuman(faction))
          (askShift(name, faction), askPiece(name, faction))
        else
          (isCoin(faction), botPiece(faction))
        
        println()
        if (towardSupport)
          increaseSupport(name, 1)
        else
          decreaseSupport(name, 1)
        placePieces(name, piece)
      }
    }
  }

  // Single event - These functions are not used
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
