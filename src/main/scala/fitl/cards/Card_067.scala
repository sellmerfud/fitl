
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
// Sea power: US or ARVN relocates any of its Troops among coastal
// spaces, then free Sweeps and Assaults in 1 coastal space.
//
// Shaded Text
// Enemy vanished: VC relocate up to 3 pieces from any coastal space.
// US and ARVN Ineligible through next card.
//
// Tips
// "Coastal" spaces are those touching or across a Highway from ocean (1.3.7).
// The Sweep could occur even during Monsoon. The named Faction decides where
// to relocate its pieces (VC to any map spaces). "Pieces" includes Bases.

object Card_067 extends EventCard(67, "Amphib Landing",
  DualEvent,
  List(ARVN, US, VC, NVA),
  ListMap(US   -> (NotExecuted -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (NotExecuted -> Shaded))) {


  def coastalSpaces = game.spaces filter (sp => sp.coastal && !sp.isNorthVietnam)

  def unshadedEffective(faction: Faction): Boolean = false  // Not executed by Bots

  def executeUnshaded(faction: Faction): Unit = {
    val factionChoices = List(US -> "Use US Troops", ARVN -> "Use ARVN Troops")
    val eventFaction   = askMenu(factionChoices, "Choose one:").head
    val troops         = if (eventFaction == US) USTroops else ARVNTroops

    def relocateFrom(originName: String): Unit = {
      val destChoices = spaceNames(coastalSpaces) filterNot (_ == originName)
      val destName    = askSimpleMenu(destChoices, s"Relocate $troops from $originName to which space:").head
      val origin = game.getSpace(originName)
      val maxNum = origin.pieces.totalOf(troops)
      val num    = askInt(s"Relocate how many $troops from $originName to $destName", 0, maxNum)
      movePieces(Pieces().set(num, troops), originName, destName)
    }

    def nextRelocate(): Unit = {
      val withTroops = spaceNames(coastalSpaces filter (_.pieces.has(troops)))
      val choices = (withTroops map (n => n -> n)) :+ ("finished" -> s"Finished relocating $troops")
      askMenu(choices, s"\nRelocate $troops out of which space:").head match {
        case "finished" =>
        case name =>
          relocateFrom(name)
          nextRelocate()
      }
    }

    if (coastalSpaces exists (_.pieces.has(troops)))
      nextRelocate()
    else
      log(s"There are no $troops in coastal spaces")

    val target = askSimpleMenu(spaceNames(coastalSpaces), "Sweep and Assault in which space:").head
    val sp = game.getSpace(target)
    val params = Params(
      event        = true,
      free         = true,
      singleTarget = Some(target)
    )

    // Cannot sweep into a LoC
    if (!sp.isLoC)
      Human.executeSweep(eventFaction, params)
      
    Human.performAssault(eventFaction, target, params)
  }

  def shadedEffective(faction: Faction): Boolean = false  // Not executed by Bots

  def executeShaded(faction: Faction): Unit = {
    val originCandidates = coastalSpaces filter (_.pieces.has(VCPieces))

    if (originCandidates.isEmpty)
      log("There are no coastal spaces with VC pieces")
    else if (game.isBot(VC))
      log("VC chooses to not relocate any pieces")
    else {
      val originChoices = (spaceNames(originCandidates) map (n => n -> n)) :+
                          ("none" -> "Do not relocate any VC pieces")
      log("VC relocates up to 3 pieces out of a coastal space")
      val originName = if (game.isHuman(VC))
      askMenu(originChoices, "\nRelocate VC pieces from which space:").head match {
        case "none" =>
        case name   =>
          val dests = spaceNames(game.spaces filterNot (_.name == name)).toSet
          val sp = game.getSpace(name)
          val num = askInt(s"Relocate how many pieces out of $name", 0, sp.pieces.totalOf(VCPieces) min 3)
          val movers = askPieces(sp.pieces, num, VCPieces, Some(s"Select VC ${pluralize(num, "piece")} to relocate"))
          movePiecesFromSpace(VC, name, movers, dests)
      }
    }

    makeIneligibleThroughNextTurn(US)
    makeIneligibleThroughNextTurn(ARVN)
  }
}
