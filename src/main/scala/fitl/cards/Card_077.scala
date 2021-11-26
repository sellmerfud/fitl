
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
// Communist Bloc eases off of war: Cut NVA and VC Resources each to half their
// total (round down). 5 Available NVA Troops out of play.
//
// Shaded Text
// Nixon disappointed: NVA add +9 Resources or free Infiltrate.
// Then VC free Rally in up to 6 spaces.
//
// Tips
// This unshaded event is the only way that pieces other than US or ARVN (NVA) will occupy
// the Out of Play box (1.4.1).
// For the shaded effect, the named Faction in each case decides its details.

object Card_077 extends EventCard(77, "Detente",
  DualEvent,
  List(ARVN, NVA, VC, US),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  def canCutRes = List(NVA, VC) exists { f => game.trackResources(f) && game.resources(f) > 1 }
            

  def unshadedEffective(faction: Faction): Boolean =
    game.availablePieces.has(NVATroops) ||
    canCutRes

  def executeUnshaded(faction: Faction): Unit = {
    val numTroops = game.availablePieces.totalOf(NVATroops) min 5
    decreaseResources(NVA, game.nvaResources / 2)
    decreaseResources(VC, game.vcResources / 2)
    moveAvailableToOutOfPlay(Pieces().set(numTroops, NVATroops))
  }

  val canInfiltrateBase = (sp: Space) =>
    sp.pieces.totalOf(NVAPieces) > sp.pieces.totalOf(VCPieces) &&
    sp.pieces.has(VCBases)

  val canInfiltrateTroops = (sp: Space) => sp.pieces.has(NVABases)

  def canInfiltrate =
    (game.availablePieces.has(NVABase) && (game.nonLocSpaces exists canInfiltrateBase)) ||
    (game.availablePieces.has(NVATroops) && (game.nonLocSpaces exists canInfiltrateTroops))

  val canRallyBase = (sp: Space) =>
    sp.support < PassiveSupport &&
    sp.totalBases < 2           &&
    sp.pieces.totalOf(VCGuerrillas) > 2

  val canRallyGuerrillas = (sp: Space) => sp.support < PassiveSupport

  val canFlipGuerrillas = (sp: Space) =>
    sp.support < PassiveSupport   &&
    sp.pieces.has(VCBases)        &&
    sp.pieces.has(VCGuerrillas_A) &&
    !sp.pieces.has(VCGuerrillas_U)

  def canRally = 
    (game.availablePieces.has(VCBase) && (game.nonLocSpaces exists canRallyBase)) ||
    (game.availablePieces.has(VCGuerrillas_U) && (game.nonLocSpaces exists canRallyGuerrillas)) ||
    (game.nonLocSpaces exists canFlipGuerrillas)

  def shadedEffective(faction: Faction): Boolean = faction match {
    case VC => canRally
    case _  => canInfiltrate
  }

  def executeShaded(faction: Faction): Unit = {
    val nvaAction = if (game.isHuman(faction)) {
      val choices = List("res" -> "Add +9 NVA resources", "infiltrate" -> "NVA free Infiltrate")
      askMenu(choices, "\nChoose one:")
    }
    else
      "infiltrate"  // Bot always chooses infiltrate

    println()
    if (nvaAction == "res")
      increaseResources(NVA, 9)
    else if (game.isHuman(NVA))
      Human.doInfiltrate(Params(event = true, free = true))
    else
      NVA_Bot.infiltrateActivity(Params(event = true, free = true), needDiceRoll = false, replaceVCBase = true)

    val rallyParams = Params(event = true, free = true, maxSpaces = Some(6))
    if (game.isHuman(VC))
      Human.executeRally(VC, rallyParams)
    else
      VC_Bot.rallyOp(rallyParams, actNum = 2)
  }
}
