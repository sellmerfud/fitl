
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
// Respite: Place 2 Casualties onto the map.
// All Rangers and Irregulars Underground.
//
// Shaded Text
// Charlie bugs out: Flip all VC and NVA Guerrillas Underground.
//
// Tips
// The unshaded text allows selection of any 2 pieces from the Casualties box—Troops,
// Irregulars, or Bases—and their placement into any map spaces where they may stack (1.4.2).

object Card_110 extends EventCard(110, "No Contact",
  DualEvent,
  List(VC, NVA, ARVN, US),
  ListMap(US   -> (Critical    -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Ignored -> Shaded))) {

  val ActiveSpecials = Set(Irregulars_A, Rangers_A)

  def unshadedEffective(faction: Faction): Boolean =
    game.casualties.nonEmpty ||
    (game.spaces exists (_.pieces.has(ActiveSpecials)))

  def executeUnshaded(faction: Faction): Unit = {
    val num = game.casualties.total min 2
    val validSpaces = spaceNames(game.spaces filterNot (_.isNorthVietnam))

    loggingControlChanges {
      if (num > 0)
        placeCasualtyPiecesOnMap(faction, num, Set(USTroops, USBase, Irregulars_U), validSpaces)

      for (sp <- game.spaces)
        hidePieces(sp.name, sp.pieces.only(ActiveSpecials))
    }
  }

  def shadedEffective(faction: Faction): Boolean = game.spaces exists (_.pieces.has(ActiveGuerrillas))

  def executeShaded(faction: Faction): Unit = {
      for (sp <- game.spaces)
        hidePieces(sp.name, sp.pieces.only(ActiveGuerrillas))
  }
}
