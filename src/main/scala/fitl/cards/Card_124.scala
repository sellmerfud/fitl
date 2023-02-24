
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

// VC Pivotal event
//
// Play if 2+ cards in RVN Leader box and >20 VC Guerrillas in South.
//
// General uprising: Free Terror with 1 Underground VC per space.
// Place 6 VC pieces in any Cities. VC+NVA Guerrillas free Attack
// where enemies (remove VC first).
//
// Tips
// Do not count Minh as a card for the precondition. "In South" means among
// all South Vietnam spaces, including all LoCs. Carry out the steps in order.
// Terror is mandatory wherever Underground VC Guerrillas—even if no shift will
// benefit the VC—Activating 1 Underground VC Guerrilla each such space. If Terror
// markers will run out, it is up to the VC (random if Non-player) to decide the
// order of Terror spaces. Attacks are mandatory where VC or NVA Guerrillas are
/// located with any enemies, Activating all those Guerrillas, and include VC who
// just executed Terror. If both VC and NVA Guerrillas occupy such a space, add their
// numbers together to determine the die roll needed for success, removing VC before
// NVA if possible when US Troop or Base losses result in Attrition (3.3.3).

object Card_124 extends EventCard(124, "Tet Offensive",
  SingleEvent,
  List(VC, NVA, US, ARVN),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Ignored -> Unshaded),
          NVA  -> (Ignored -> Unshaded),
          VC   -> (Critical    -> Unshaded))) {

  def numVCGuerrillasInSouth: Int =
    (game.spaces filter (sp => isInSouthVietnam(sp.name)) map (_.pieces.totalOf(VCGuerrillas))).sum


  def unshadedEffective(faction: Faction): Boolean =
    game.numCardsInLeaderBox >= 2 &&
    numVCGuerrillasInSouth > 20

  val terrorCandidate = (sp: Space) => sp.pieces.has(VCGuerrillas_U)

  def terrorizeSpace(name: String): Unit = {
    log(s"\nVC selects $name for Terror")
    log(separator())
    revealPieces(name, Pieces(vcGuerrillas_U = 1))
    val sp = game.getSpace(name)
    if (capabilityInPlay(Cadres_Unshaded)) {
      val guerrillas = sp.pieces.only(VCGuerrillas)
      val num = guerrillas.total min 2
      val toRemove = if (game.isHuman(VC))
        askPieces(guerrillas, num, VCGuerrillas, Some(s"Remove guerrillas for [$Cadres_Unshaded]"))
      else
        Bot.selectFriendlyRemoval(guerrillas, num)
      removeToAvailable(name, toRemove)
    }

    if (sp.terror == 0 && game.terrorMarkersAvailable > 0)
      addTerror(name, 1) // Terror/Sabotage marker

      if (sp.canHaveSupport && sp.support != ActiveOpposition)
        decreaseSupport(name, 1)
  }

  def humanTerror(terrorSpaces: List[String]): Unit = {
    def nextTerror(candiates: List[String]): Unit = {
      val name = askSimpleMenu(candiates, "\nChoose next space to terrorize:").head
      terrorizeSpace(name)
      pause()
      nextTerror(candiates filterNot (_ == name))
    }
    
    println("\nThere are not enough Terror/Sabatoge markers for all spaces.")
    nextTerror(terrorSpaces)
  }

  // When doing Terror prioritize spaces that can actually have
  // support first, then LocS.  These will get done first
  // to use up the available terror markers
  val terrorPreferences = List(
    new Bot.BooleanPriority[Space](
      "City or Province can have Support", _.canHaveSupport
    ),
    new Bot.BooleanPriority[Space](
      "LoC", _.isLoC
    )
  )

  def botTerror(terrorSpaces: List[Space]): Unit = {
    def nextTerror(candidates: List[Space]): Unit = if (candidates.nonEmpty) {
      // Have Bot place terror markers in spaces that can have support first
      val narrowed = Bot.narrowCandidates(candidates, terrorPreferences)
      val sp = VC_Bot.pickSpaceTowardActiveOpposition(narrowed)

      terrorizeSpace(sp.name)
      pause()
      nextTerror(candidates filterNot(_.name == sp.name))
    }

    nextTerror(terrorSpaces)
  }

  // Attack space with VC+NVA Guerrillas
  def attackSpace(name: String): Unit = {
    def sp      = game.getSpace(name)
    val numGs   = sp.pieces.totalOf(Guerrillas)
    val die     = d6
    val success = die <= numGs

    log(s"\nVC + NVA Attack in $name")
    log(separator())
    revealPieces(name, sp.pieces.only(UndergroundGuerrillas))

    log(s"\nDie roll (${amountOf(numGs, "Guerrilla")}): $die [${if (success) "Success!" else "Failure"}]")
    if (success) {
      val coinPieces = sp.pieces.only(CoinPieces)
      val maxNum     = coinPieces.total min 2
      val deadPieces = if (game.isHuman(VC)) {
        val num = askInt("\nRemove how many enemy pieces", 1, maxNum, Some(maxNum))
        askEnemyCoin(coinPieces, num, prompt = Some(s"Remove ${amountOf(num, "enemy piece")} in $name"))
      }
      else
        Bot.selectRemoveEnemyCoinBasesLast(coinPieces, maxNum)
      val attrition      = deadPieces.only(USTroops::USBase::Nil).total min sp.pieces.totalOf(Guerrillas)
      val vcAttrition    = sp.pieces.totalOf(VCGuerrillas) min attrition
      val nvaAttrition   = sp.pieces.totalOf(NVAGuerrillas) min (attrition - vcAttrition)
      // All Guerrillas are now active
      val deadGuerrillas = Pieces(vcGuerrillas_A = vcAttrition, nvaGuerrillas_A = nvaAttrition)

      removePieces(name, deadPieces)
      removeToAvailable(name, deadGuerrillas, Some("Attrition:"))
    }
  }

  // Event Instructions
  // Select spaces using Place Bases.
  // Get 1 VC Base and 2 VC Guerrillas to each selected space.
  def botPlacement(): Unit = {

    def availVC = game.availablePieces.only(VCPieces)
    if (availVC.isEmpty)
      log("VC Bot places no pieces because there are none available")
    else {
      
      def nextBotPlacement(basesPlaced: Int, numRemaining: Int): Unit = if (numRemaining > 0 && availVC.nonEmpty) {
        val baseCandidates = game.citySpaces filter (_.canTakeBase)
        if (availVC.has(VCBase) && baseCandidates.nonEmpty && (basesPlaced < 2 || !availVC.has(VCGuerrillas))) {
          val sp = VC_Bot.pickSpacePlaceBases(baseCandidates)
          val numGs = (numRemaining - 1) min availVC.totalOf(VCGuerrillas_U) min 2
          val pieces = Pieces(vcBases = 1, vcGuerrillas_U = numGs)
          placePieces(sp.name, pieces)
          nextBotPlacement(basesPlaced + 1, numRemaining - pieces.total)
        }
        else if (availVC.has(VCGuerrillas_U)) {
          val sp = VC_Bot.pickSpacePlaceGuerrillas(game.citySpaces)
          placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
          nextBotPlacement(basesPlaced, numRemaining - 1)
        }
      }

      nextBotPlacement(0, 6)
    }
  }

  def executeUnshaded(faction: Faction): Unit = {

    val terrorSpaces = game.spaces filter terrorCandidate

    log("\nFree Terror with 1 Underground VC per space")
    log(separator(char = '='))
    if (terrorSpaces.isEmpty)
      log("There are no Underground VC Guerrillas that can terrorize")
    else {
      val markersNeeded = terrorSpaces count (_.terror == 0) 
      // If there are enough terror/sabatoge markers then just execute
      // all of the spaces, otherwise allow the user to select the
      // space order.
      loggingControlChanges {
        if (terrorSpaces.size == 1 || game.terrorMarkersAvailable >= markersNeeded) {
          for (sp <- terrorSpaces) {
            terrorizeSpace(sp.name)
            pause()
          }
        }
        else if (game.isHuman(VC))
          humanTerror(spaceNames(terrorSpaces))
        else
          botTerror(terrorSpaces)
      }
      pause()
    }

    log("\nPlace 6 VC Pieces in any Cities")
    log(separator(char = '='))
    loggingControlChanges {
      if (game.isHuman(VC))
        placePiecesOnMap(VC, 6, VCPieces, spaceNames(game.citySpaces))
      else
        botPlacement()
    }
    pause()

    val attackSpaces = game.spaces filter { sp => sp.pieces.has(Guerrillas) && sp.pieces.has(CoinPieces) }

    log("\nVC + NVA Guerrillas free Attack where there are enemies")
    log(separator(char = '='))
    loggingControlChanges {
      for (sp <- attackSpaces) {
        attackSpace(sp.name)
        pause()
      }
    }
  }

  // Shaded functions not used for Pivotal Event  
  def shadedEffective(faction: Faction): Boolean = false
  def executeShaded(faction: Faction): Unit = ()
}
