
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
// Buddhists counter Communists: Set up to 3 Neutral or Opposition Cities
// to Passive Support.
//
// Shaded Text
// People’s Revolutionary Committee: Shift Hue, Da Nang, and Saigon 1 level
// toward Active Opposition. Place a VC piece in Saigon.
//
// Tips
// A "piece" may be a Base.

object Card_114 extends EventCard(114, "Tri Quang",
  DualEvent,
  List(VC, ARVN, US, NVA),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.isCity &&
    sp.support < PassiveSupport

  def unshadedEffective(faction: Faction): Boolean = game.citySpaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candidates = game.citySpaces filter unshadedCandidate
    
    if (candidates.isEmpty)
      log("There are no cities that qualify for the event")
    else {
      val maxNum         = candidates.size min 3
      val selectedCities = if (game.isHuman(faction)) {
        val num = askInt("Set how many cities to Passive Support", 0, maxNum)
        if (num == 0)
          Nil
        else
          askSimpleMenu(spaceNames(candidates), s"\nSelect ${amountOf(num, "City", Some("Cities"))}:", numChoices = num)
      }
      else if (faction == US)
        Bot.pickSpaces(maxNum, candidates)(US_Bot.pickSpaceTowardActiveSupport) map (_.name)
      else
        Bot.pickSpaces(maxNum, candidates)(ARVN_Bot.pickSpaceTowardPassiveSupport) map (_.name)

      println()
      loggingPointsChanges {
        for (name <- selectedCities)
          setSupport(name, PassiveSupport)
      }
    }
  }

  def shadedCandidates = spaces(Hue::DaNang::Saigon::Nil) filter (_.support > ActiveOpposition)

  def shadedEffective(faction: Faction): Boolean =
    shadedCandidates.nonEmpty ||
    game.availablePieces.has(VCGuerrillas_U) ||
    (game.availablePieces.has(VCBase) && game.getSpace(Saigon).canTakeBase)

  def executeShaded(faction: Faction): Unit = {
    val saigon   = game.getSpace(Saigon)
    val pool     = if (game.isHuman(faction) && faction == VC) game.piecesToPlace else game.availablePieces
    val haveBase = pool.has(VCBase) && saigon.canTakeBase
    val haveG    = pool.has(VCGuerrillas_U)

    val toPlace = if (game.isHuman(faction)) {
      (haveBase, haveG) match {
        case (true, true) =>
          val choices = List(Pieces(vcBases = 1) -> "VC Base", Pieces(vcGuerrillas_U = 1) -> "VC Guerrilla")
          askMenu(choices, "\nChoose piece to place in Saigon").head
        case (true, false)  => Pieces(vcBases = 1)
        case (false, true)  => Pieces(vcGuerrillas_U = 1)
        case (false, false) => Pieces()
      }
    }
    else if (haveBase && (saigon.support < PassiveSupport || !haveG))
      Pieces(vcBases = 1)
    else if (haveG)
      Pieces(vcGuerrillas_U = 1)
    else
      Pieces()
    
    println()
    loggingControlChanges {
      for (sp <- shadedCandidates)
        decreaseSupport(sp.name, 1)
  
      log()
      placePieces(Saigon, toPlace)
    }
  }
}
