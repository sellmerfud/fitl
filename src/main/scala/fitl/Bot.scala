
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

package fitl

import java.io.IOException
import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.language.implicitConversions
import FUtil.Pathname
import FireInTheLake._
import java.nio.file.AtomicMoveNotSupportedException
import scala.annotation.meta.param

object Bot {

  // Value to indicate that space selection is unlimited.
  val NO_LIMIT = 1000;

  // These values are used with HighestScore and LowestScore filters
  // when a candidate is not qualified to produce a score.
  // For example if it is a LoC and we are only considering
  // Citys and Provinces.
  val NO_SCORE  = -1000


  // Bot debug logs even when logging is suspended!
  def botDebug(msg: => String) = if (game.botDebug) log(msg, force = true)

  def msgResult(result: Any, msg: String): String = {
    val resultStr = result match {
      case true  => "yes"
      case false => "no"
      case other => result.toString
    }

    s"$msg: $resultStr"
  }

  //  We log each choice in the list stopping after we reach the
  //  first condition that is true.
  def botDebugChoices(choices: => List[(Boolean, String)]): Unit = {

    def logNext(remaining: List[(Boolean, String)]): Unit = remaining match {
      case Nil =>
      case (cond, msg)::xs if cond =>
        log(msgResult(cond, msg))
      case (cond, msg)::xs =>
        log(msgResult(cond, msg))
        logNext(xs)
    }

    if (game.botDebug)
      logNext(choices)
  }

  // Variable Global within the Bot object
  // ---------------------------------------
  // Used to keep track of moveDestinations during and operation
  // The Bots will never move pieces out of a space that has been
  // selected as a move destination.
  // We use a vector to keep the spaces in priority order
  private var moveDestinations      = Vector.empty[String]
  private var transportDestinations = Vector.empty[String]
  private var transportOrigin       = Option.empty[String]
  // Used to enforce Govern/Advise restrictions
  private var trainingSpaces        = Set.empty[String]
  private var adviseSpaces          = Set.empty[String]
  private var m48PattonSpaces       = Set.empty[String]
  // Keeps track of all pieces that actually moved
  // during March/Sweep/Patrol/Transport/Air Lift..
  private var movedPieces      = new MovingGroups()


  def resetM48PattonSpaces(): Unit = m48PattonSpaces = Set.empty
  def m48PattonCount = m48PattonSpaces.size
  def canAddM48Patton(faction: Faction, name: String) = m48PattonCount < 2 && canUseM48PattonUnshaded(faction, name)
  
  def getMoveDestinations = moveDestinations
  
  // Add the dest to our list of destinations, but
  // do not allow duplicates.
  def addMoveDestination(destName: String): Unit = {
    if (!moveDestinations.contains(destName))
      moveDestinations = moveDestinations :+ destName
  }

  //  Return the number of moveable pieces in the given space
  def notYetMoved(space: Space, moveTypes: Set[PieceType]): Int = {
    space.pieces.totalOf(moveTypes) - movedPieces(space.name).totalOf(moveTypes)    
  }

  // Used to implement the Eligibility Tables
  case class ActionEntry(val action: Action, desc: String, test: (Faction) => Boolean)

  // Possible results when attempting to execute the instructions
  // on a Trung Card.  Flipping a card is handled by the card
  // instance itself.
  sealed trait TrungResult
  case class  TrungComplete(specialActivity: Boolean) extends TrungResult
  case object TrungDraw     extends TrungResult
  case object TrungNoOp     extends TrungResult


  // Trung Card defintion
  // The front and back of each card are treated a seperate
  // entities internally.

  abstract class TrungCard(val faction: Faction, val id: String, val actNum: Int) {

    def display(ident: String) = s"Trung: $faction - $ident"
    override def toString() = display(id)

    def flipCard(params: Params, specialDone: Boolean = false): TrungResult = {
      if (game.logTrung || game.botDebug) {
        log("\nTrung card flipped to its back side")
        log(separator())
        log(display(id * 2))        
      }
      executeBack(params, specialDone)
    }
    def executeFront(params: Params): TrungResult
    def executeBack(params: Params, specialDone: Boolean): TrungResult
  }

  // Add a march actiation number function to all NVA Trung cards
  // to override the cards activation number when the trail is a 4
  trait NVATrung extends TrungCard {
    def marchActNum = if (game.trail == TrailMax) 1 else actNum
  }


  sealed trait CompareOp {
    def compare(diceValue: Int, targetValue: Int): Boolean
  }

  object CMP_EQ extends CompareOp {
    override def toString() = "=="
    def compare(diceValue: Int, targetValue: Int): Boolean = diceValue == targetValue
  }
  
  object CMP_NE extends CompareOp {
    override def toString() = "!="
    def compare(diceValue: Int, targetValue: Int): Boolean = diceValue != targetValue
  }
  
  object CMP_LT extends CompareOp {
    override def toString() = "<"
    def compare(diceValue: Int, targetValue: Int): Boolean = diceValue < targetValue
  }
  
  object CMP_LE extends CompareOp {
    override def toString() = "<="
    def compare(diceValue: Int, targetValue: Int): Boolean = diceValue <= targetValue
  }
  
  object CMP_GT extends CompareOp {
    override def toString() = ">"
    def compare(diceValue: Int, targetValue: Int): Boolean = diceValue > targetValue
  }
  
  object CMP_GE extends CompareOp {
    override def toString() = ">="
    def compare(diceValue: Int, targetValue: Int): Boolean = diceValue >= targetValue
  }
  
  def logCheck(result: Boolean, display: String): Boolean = {
    if (game.logTrung || game.botDebug) {
      val yesNo = if (result) "[Yes]" else "[No]"
      log(s"Trung check: $display $yesNo")
    }
    result
  }
  
  //  Function to make a comparison against the dice roll of a number of d6.
  //  This is used so we can log the dice roll
  def trungDiceCheck(numd6: Int, targetValue: Int, compareOp: CompareOp, desc: String): Boolean = {
    val diceValue = rollDice(3)
    val result    = compareOp.compare(diceValue, targetValue)

    logCheck(result, s"${numd6}d6 $compareOp $desc ($targetValue): $diceValue")
  }

  def trungD3Check(targetValue: Int, compareOp: CompareOp, desc: String): Boolean = {
    val dieValue = d3
    val result   = compareOp.compare(dieValue, targetValue)

    logCheck(result, s"d3 $compareOp $desc ($targetValue): $dieValue")
  }

  def usPoints_GE_42: Boolean = {
    val result = game.usPoints >= 42
    logCheck(result, "Support + Available >= 42?")
  }

  def logOpChoice(faction: Faction, op: Operation, notes: Iterable[String] = Nil): Unit = {
    log(s"\n$faction chooses $op operation")
    log(separator(char = '='))
    for (note <- notes)
      log(note)
  }

  def logNoOp(faction: Faction, op: Operation): Unit = {
      log(s"\nNo spaces found for $faction $op")
  }

  def logSAChoice(faction: Faction, sa: SpecialActivity, notes: Iterable[String] = Nil): Unit = {
    log(s"\n$faction chooses $sa special activity")
    log(separator(char = '='))
    for (note <- notes)
      log(note)
  }

  def logNoActivity(faction: Faction, sa: SpecialActivity): Unit = {
      log(s"\nNo spaces found for $faction $sa")
  }

  def logEndSA(faction: Faction, sa: SpecialActivity): Unit = {
    log()
    log(separator())
    log(s"End of $faction $sa activity")
  }

  // Make an activation roll
  // and return true if it is a success
  def makeActivationRoll(faction: Faction, actNum: Int): Boolean ={
    val die     = d6
    val success = die > actNum

    log(s"\n$faction activation roll against activation number of $actNum")
    log(separator())
    log(s"Die roll: $die [${if (success) "Success!" else "Failure"}]")
    success
  }

      //  Used by US/ARVN Patrol operations
  def activateGuerrillasOnLOCs(faction: Faction): Boolean = {
    case class Activation(sp: Space, num: Int)
    val PatrolCubes = if (faction == US) List(USTroops) else ARVNCubes
    val underground = (sp: Space) => sp.pieces.totalOf(UndergroundGuerrillas)
    val cubes       = (sp: Space) => sp.pieces.totalOf(PatrolCubes)
    val numbers     = game.locSpaces map (sp => (sp, cubes(sp) min underground(sp))) filterNot (_._2 == 0)

    log(s"\n$faction Patrol - Activating guerrillas on LOCs")
    log(separator())
    if (numbers.isEmpty) {
      log("No guerrillas are activated")
      false
    }
    else {
      //  If a space has both NVA and VC underground guerrillas
      //  then ask user to pick the ones to activate.
      val activations = for ((sp, num) <- numbers) {
        val guerrillas = selectEnemyRemoveReplaceActivate(sp.pieces.only(UndergroundGuerrillas), num)
        revealPieces(sp.name, guerrillas)
        pause()
      }
      true
    }
  }

  // Convenience method
  def checkActivation(faction: Faction, needRoll: Boolean, actNum: Int): Boolean = {
    needRoll == false || makeActivationRoll(faction, actNum)
  }

  val CanTho_Hue_Route_Spaces = Set(
    Hue, DaNang, Kontum, QuiNhon, CamRahn, AnLoc, Saigon, CanTho, LOC_Hue_DaNang,
    LOC_DaNang_DakTo, LOC_DaNang_QuiNhon, LOC_Kontum_DakTo, LOC_Kontum_QuiNhon,
    LOC_Kontum_BanMeThuot, LOC_QuiNhon_CamRanh, LOC_CamRanh_DaLat, LOC_BanMeThuot_DaLat,
    LOC_Saigon_CamRanh, LOC_Saigon_DaLat, LOC_Saigon_AnLoc_BanMeThuot, LOC_Saigon_CanTho
  )

        // All routes from Can Tho to Hue blocked
  def allLocRoutesCanTho_HueBlocked: Boolean = !getPatrolDestinations(CanTho).contains(Hue)


  def sweepEffective(name: String, faction: Faction, cubeTreatment: CubeTreatment): Boolean = {
    game.getSpace(name).sweepActivations(faction, cubeTreatment) > 0
  }

  // Determine if the given faction can effectively Ambush
  // from the given space
  def canAmbushFrom(faction: Faction, needUnderground: Boolean)(sp: Space): Boolean = {
    val GTypes = ambushGuerrillaTypes(faction, needUnderground)
    sp.pieces.has(GTypes) &&
    (sp.pieces.has(CoinPieces) || (sp.isLoC && numAdjacentPieces(sp: Space, CoinPieces) > 0))
  }

  // When an ambush Special Activity follows a March Operation
  // The Bot may ambush only in spaces that were selected as
  // March destination.  And the Underground guerrilla used to
  // perform the Ambush must have marched into the space this turn.
  def marchAmbushCandidates(faction: Faction, needUnderground: Boolean): List[String] = {
    val GTypes = ambushGuerrillaTypes(faction, needUnderground)
    spaceNames(spaces(moveDestinations) filter { sp =>
      movedPieces(sp.name).has(GTypes) &&
      canAmbushFrom(faction, needUnderground)(sp)
    })
  }

    // Ask the user if they wish to use the Cobras capability
  // in the space.
  // (Part of a Sweep operation)
  def checkUnshadedCobras(name: String): Boolean = {
    val sp         = game.getSpace(name)
    val isPossible = capabilityInPlay(Cobras_Unshaded) &&
                     (sp.pieces.has(NVATroops::ActiveGuerrillas) ||
                     (!sp.pieces.has(UndergroundGuerrillas) && sp.pieces.has(InsurgentNonTunnels)))

    if (isPossible) {
      val (deadPiece, _) = selectRemoveEnemyInsurgentBasesLast(sp.pieces, 1)

      log(s"\nUsing $Cobras_Unshaded")
      log(separator())
      removeToAvailable(name, deadPiece)
      true
    }
    else
      false
  }

  // Used by events that allow a faction to execute a special activity.
  // We draw Trung Cards and execute the activity(s) on the cards until
  // one is effective.
  def eventSpecialActivity(faction: Faction, params: Params, only: Set[SpecialActivity] = AllSpecials): Unit = {
    val available = FactionSpecials(faction) filter only  // Not currently used!

    executeOp(faction, params.copy(specialActivityOnly = true)) match {
      case ER_NoOp =>
        log(s"\n$faction was unable to execute a Special Activity effectively")
      case _ =>
    }
  }


  // Perform Bot pacification in the given space.
  // Returns the number of terror removed + level shifted
  def pacifySpace(name: String, faction: Faction, coupRound: Boolean, coupPoints: Int = 0, free: Boolean = false, maxLevels: Int = 2): Int = {
    val nguyenCaoKy = isRVNLeader(RVN_Leader_NguyenCaoKy)
    val blowtorch   = momentumInPlay(Mo_BlowtorchKomer)
    // In Coup Round The leader takes precedence over Blowtorch because it was played
    // more recently.  Otherwise the Blowtorch momentum was played more recently and take precedence.
    val useResources = game.trackResources(ARVN) && !free
    val cost = if (useResources) {
      if (coupRound) (if (nguyenCaoKy) 4 else if (blowtorch) 1 else 3)
      else           (if (nguyenCaoKy) 4 else 3)  // blowtorch only applies in coup round
    }
    else 0

    // ARVN Bot will only pacify to Passive Support
    val maxSupport    = if (faction == ARVN || capabilityInPlay(CORDS_Shaded))
      PassiveSupport
    else
      ActiveSupport
    val sp             = game.getSpace(name)
    val maxShift       = ((maxSupport.value - sp.support.value) max 0) min maxLevels
    val maxInSpace     = maxShift + sp.terror
    val d3Limit        = if (coupRound) NO_LIMIT else d3
    val availResources = if (faction == ARVN) game.arvnResources else ((game.arvnResources - game.econ) max 0)
    val maxAfford      = (coupRound, useResources) match {
      case (_, true)  if cost == 0 => NO_LIMIT
      case (_, true)               => availResources / cost
      case (true, false)           => coupPoints
      case (false, false)          => NO_LIMIT
    }
    val maxPacify = ((sp.terror + maxShift) min d3Limit) min maxAfford
    val numTerror = maxPacify min sp.terror
    val numShift  = (maxPacify - numTerror) min maxShift

    log(s"\n$faction Pacifies ${amountOf(numShift, "level")} in ${sp.name}")
    log(separator())
    decreaseResources(ARVN, (numTerror + numShift) * cost)
    removeTerror(sp.name, numTerror)
    increaseSupport(sp.name, numShift)
    pause()
    numTerror + numShift
  }

  // Check for the General Lansdale momentum effect
  // and logs a message if it is in play.
  def generalLandsdale(faction: Faction): Boolean = {
    if (faction == US && momentumInPlay(Mo_GeneralLansdale)) {
      log(s"\n$faction cannot assault [Momentum: $Mo_GeneralLansdale]")
      true
    }
    else
      false
  }

  def noVulnerableInsurgents(sp: Space): Boolean = {
    !sp.pieces.has(NVATroops) &&
    !sp.pieces.has(ActiveGuerrillas) &&
    (sp.pieces.has(UndergroundGuerrillas) || !sp.pieces.has(InsurgentNonTunnels))
  }
  // Is space on the Can Tho - Hue route and would
  // an assault remove all insurgent pieces
  def assaultInWouldUnblockAlongCanTho_HueRoute(faction: Faction, cubeTreatment: CubeTreatment, vulnerableTunnels: Boolean)(sp: Space): Boolean = {
    CanTho_Hue_Route_Spaces(sp.name) &&
    sp.pieces.has(InsurgentForces)   &&
    assaultResult(faction, cubeTreatment, vulnerableTunnels)(sp).pieces.totalOf(InsurgentForces) == 0
  }
  
  // Is space on the Can Tho - Hue route and would
  // an assault remove all insurgent pieces
  def assaultInWouldRemoveInsurgentsAlongCanTho_HueRoute(faction: Faction, cubeTreatment: CubeTreatment, vulnerableTunnels: Boolean)(sp: Space): Boolean = {
    val numInsurgents = sp.pieces.totalOf(InsurgentPieces)
    CanTho_Hue_Route_Spaces(sp.name) &&
    numInsurgents > 0                &&
    assaultResult(faction, cubeTreatment, vulnerableTunnels)(sp).pieces.totalOf(InsurgentPieces) < numInsurgents
  }

  def pickSpaces(num: Int, candidates: List[Space])(picker: (List[Space]) => Space): List[Space] = {
    def nextSpace(numRemaining: Int, candidates: List[Space]): List[Space] = {
      if (numRemaining > 0 && candidates.nonEmpty) {
        val sp = picker(candidates)
        sp::nextSpace(numRemaining - 1, candidates filterNot (_.name == sp.name))
      }
      else
        Nil
    }

    nextSpace(num, candidates).reverse
  }


  def pickSpaceWithMostPieces(pieceTypes: Iterable[PieceType])(candidates: List[Space]) = {
    val priorities = List(new Bot.HighestScore[Space](s"Most ${andList(pieceTypes)}", _.pieces.totalOf(pieceTypes)))
    bestCandidate(candidates, priorities)
  }

  def pickSpaceWithMostVulnerableInsurgents(candidates: List[Space]) = {
    val priorities = List(new Bot.HighestScore[Space](
      s"Most vulnerable Insurgents",
      sp => vulnerableInsurgents(sp.pieces, vulnerableTunnels = false).total
    ))
    bestCandidate(candidates, priorities)
  }

  def pickSpaceWithMostSweepActivations(faction: Faction, cubeTreatment: CubeTreatment)(candidates: List[Space]) = {
    val priorities = List(
      new HighestScore[Space](s"$faction Activates most guerrillas", _.sweepActivations(faction, cubeTreatment))
    )
    bestCandidate(candidates, priorities)
  }

  def pickSpaceWithMostSupport(candidates: List[Space]) = {
    val priorities = List(new Bot.HighestScore[Space]("Most support", _.supportValue))
    bestCandidate(candidates, priorities)
  }
  //  Return the corresponding function for the given bot
  def pickSpaceRemoveReplace(faction: Faction) = faction match {
    case US   => US_Bot.pickSpaceRemoveReplace _
    case ARVN => ARVN_Bot.pickSpaceRemoveReplace _
    case NVA  => NVA_Bot.pickSpaceRemoveReplace _
    case VC   => VC_Bot.pickSpaceRemoveReplace _
  }

  //  Return the corresponding function for the given bot
  def pickSpacePlaceForces(faction: Faction, troops: Boolean = false) = faction match {
    case US   => US_Bot.pickSpacePlaceCubesSpecialForces(troops = troops) _
    case ARVN => ARVN_Bot.pickSpacePlaceCubesRangers _
    case NVA  if troops => NVA_Bot.pickSpacePlaceTroops _
    case NVA  => NVA_Bot.pickSpacePlaceGuerrillas _
    case VC   => VC_Bot.pickSpacePlaceGuerrillas _
  }

  //  Return the corresponding function for the given bot
  def pickSpacePlaceBases(faction: Faction) = faction match {
    case US   => US_Bot.pickSpacePlaceBases _
    case ARVN => ARVN_Bot.pickSpacePlaceBases _
    case NVA  => NVA_Bot.pickSpacePlaceBases _
    case VC   => VC_Bot.pickSpacePlaceBases _
  }

  // When removing friendly pieces the Bots will always remove from the space with the most
  // eligible pieces.
  def pickSpaceRemoveFriendlyPieces(candidates: List[Space], pieceTypes: Iterable[PieceType]): Space = {
    val priorities = List(new HighestScore[Space]("Most pieces", _.pieces.totalOf(pieceTypes)))

    bestCandidate(candidates, priorities)
  }
  
  //  Pick a target LoC to for an Assault as part of a Patrol Operation
  //  Be sure to only pass in candidate for which an assault would be effective!
  def pickPatrolAssaultLoc(faction: Faction, cubeTreatment: CubeTreatment, vulnerableTunnels: Boolean): Option[String] = {
    val priorities = List(
      new BooleanPriority[Space](
        "Would remove an Insurgent along a Can Tho to Hue route",
        assaultInWouldRemoveInsurgentsAlongCanTho_HueRoute(faction, cubeTreatment, vulnerableTunnels)
      ),
      new BooleanPriority[Space](
        "Would clear a all Insurgennts along a Can Tho to Hue route",
        assaultInWouldUnblockAlongCanTho_HueRoute(faction, cubeTreatment, vulnerableTunnels)
      ),
      new HighestScore[Space](
        "Remove the most insurgent pieces",
        sp => {
          val numBefore = sp.pieces.totalOf(InsurgentForces)
          val numAfter  = assaultResult(faction, cubeTreatment, vulnerableTunnels)(sp).pieces.totalOf(InsurgentForces)
        
          numBefore - numAfter
        }
      )
    )
    
    (game.locSpaces filter assaultEffective(faction, cubeTreatment, vulnerableTunnels)) match {
      case Nil        => None
      case candidates => Some(bestCandidate(candidates, priorities).name)
    }
  }
  

  //  -------------------------------------------------------------
  //  NVA and VC Ambush Activity
  //  This function should be called with a list of all spaces that
  //  can perform an ambush.
  //  It will select up to two spaces.
  //  For march, this is called after all marching has been completed so
  //  there is no need to make activation rolls.
  //  For Attack, this is called as part of the Attack and thus an activation
  //  roll may be necesasry.
  //  if `checkFirst` is true then we must make an activation roll before the
  //  first ambush (Attack only)
  //
  //  Return the names of the spaces that were ambushed and true if no
  //  actvation roll was failed.
  //  This lets the calling Attack know whether or not it can continue
  //  attacking spaces and which spaces have already been selected.
  //
  //  Mo_Claymores          - Prohibits Ambush
  //  Mo_TyphoonKate        - All SA's are limited to 1 space
  //  MainForceBns_Shaded   - 1 VC Ambush space may remove 2 enemy pieces
  //  BoobyTraps_Unshaded   - Ambush is max one space
  //  PT76_Unshaded         - If Attack, then NVA must first remove 1 Troop (if one is present)
  //  -------------------------------------------------------------
  def ambushActivity(faction: Faction,
                     ambushCandidates: List[String],
                     op: Operation,
                     actNum: Int,
                     params: Params,
                     checkFirst: Boolean): (Set[String], Boolean) = {
    var ambushSpaces     = Set.empty[String]
    var mainForceBnsUsed = false
    val limited          = momentumInPlay(Mo_TyphoonKate) || capabilityInPlay(BoobyTraps_Unshaded)
    val maxAmbush = params.ambush.maxAmbush getOrElse { if (limited) 1 else 2 }
    val notes = List(
      noteIf(momentumInPlay(Mo_TyphoonKate),s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
      noteIf(capabilityInPlay(BoobyTraps_Unshaded), s"You may ambush in only one space [$BoobyTraps_Unshaded]")
    ).flatten

    def activateOK() = params.free || {
      val needActivation = op == Attack && (checkFirst || ambushSpaces.nonEmpty)
      checkActivation(faction, needActivation, actNum)
    }

    // Return true if we can continue the attack (ie. No activation roll failure)
    def nextAmbush(candidates: List[String]): Boolean = {
      // Re-verify that we can ambush in the spaces because
      // a previous ambush may have removed a target piece
      val validSpaces = spaces(candidates) filter canAmbushFrom(faction, params.ambush.needUnderground)

      if (validSpaces.nonEmpty && ambushSpaces.size < maxAmbush) {

        if (activateOK()) {
          val (sp, target) = if (faction == NVA)
            NVA_Bot.pickSpaceAndTargetForAmbush(validSpaces)
          else
            VC_Bot.pickSpaceAndTargetForAmbush(validSpaces)
          val coinPieces = target.pieces.only(CoinPieces)
          val maxNum     = if (faction == VC && capabilityInPlay(MainForceBns_Shaded) && !mainForceBnsUsed) 2 else 1
          val num        = coinPieces.total min maxNum
          // Bases only removed if no other Coin forces (of either faction)
          val targetPieces = if (coinPieces.totalOf(CoinForces) >= num)
            coinPieces.only(CoinForces)
          else
            coinPieces
          val deadPieces = selectEnemyRemoveReplaceActivate(targetPieces, num)

          if (!params.event && ambushSpaces.isEmpty)
            logSAChoice(faction, Ambush, notes)

          if (sp.name == target.name)
            log(s"\n$faction Ambushes in ${sp.name}")
          else
            log(s"\n$faction Ambushes in ${sp.name}, targeting ${target.name}")
          log(separator())

          if (deadPieces.total == 2) {
            log(s"$faction elects to remove 2 enemy pieces [$MainForceBns_Shaded]")
            mainForceBnsUsed = true
          }

          if (faction == NVA && op == Attack && capabilityInPlay(PT76_Unshaded) && sp.pieces.has(NVATroops))
            removeToAvailable(sp.name, Pieces(nvaTroops = 1), Some(s"$PT76_Unshaded triggers:"))

          val underground = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
          // Some events do not require an underground guerrilla
          if (sp.pieces.has(underground))
          revealPieces(sp.name, Pieces().set(1, underground))
          removePieces(target.name, deadPieces)
          pause()
          ambushSpaces = ambushSpaces + sp.name
          nextAmbush(validSpaces map (_.name) filterNot (_ == sp.name))
        }
        else
          false // Failed activation
      }
      else
        true // ran out of candidates, No failed activation
    }

    //  Claymores momentum prevents ambush
    val canContinue = if (momentumInPlay(Mo_Claymores))
      true // True because we did nothing
    else
      nextAmbush(ambushCandidates)
    (ambushSpaces, canContinue)
  }

  //  Perform a US/AVRN assault in the given space.
  //  Important! This function assumes that the any activation roll has
  //             already been satisfied and that if this is an ARVN asasult
  //             that requires resources that the resources are available!
  //
  //  Abrams_Unshaded           - 1 US assault space may remove 1 non-Tunnel base first (not last)
  //  M48Patton_Unshaded        - In two non-Lowland US assault spaces, remove 2 extra enemy pieces
  //  Cobras_Shaded             - Each US assault space, 1 US Troop to Casualties on die roll of 1-3
  //  SearchAndDestroy_Unshaded - Each US assault space may remove 1 underground guerrilla
  //  SearchAndDestroy_Shaded   - Each US and ARVN assault Province shifts support one level toward Active Opposition
  //  Mo_BodyCount              - Cost=0 AND +3 Aid per guerrilla removed

  def performAssault(faction: Faction, name: String, params: Params): Unit = {
    def validEnemy(pieces: Pieces): Pieces =
      params.assault.onlyTarget match {
        case Some(NVA) => pieces.only(NVAPieces)
        case Some(VC)  => pieces.only(VCPieces)
        case _         => pieces
      }

    val asUS               = faction == US || params.cubeTreatment == AllCubesAsUS || params.cubeTreatment == AllTroopsAsUS
    val remove1BaseFirst   = asUS && capabilityInPlay(Abrams_Unshaded)
    val remove1Underground = asUS && capabilityInPlay(SearchAndDestroy_Unshaded)
    val m48Patton          = asUS && capabilityInPlay(M48Patton_Unshaded)
    val searchDestroy      = capabilityInPlay(SearchAndDestroy_Shaded)  // US and ARVN
    val baseTargets        = if (params.vulnerableTunnels) InsurgentBases else InsurgentNonTunnels
    val sp                 = game.getSpace(name)
    val totalLosses        = sp.assaultFirepower(faction, params.cubeTreatment)
    val enemyPieces        = validEnemy(sp.pieces)

    case class EnemyLosses(
      dead: Pieces          = Pieces(),
      baseFirst: Boolean    = false,
      exposedTunnel: Pieces = Pieces()) {
        val total = dead.total + exposedTunnel.total
      }

    def calculateLosses(numLosses: Int): EnemyLosses = {
      var killedPieces        = Pieces()
      def remainingLosses     = (numLosses - killedPieces.total) max 0
      def remainingEnemy      = enemyPieces - killedPieces
      val numVulnerableForces = enemyPieces.totalOf(NVATroops::ActiveGuerrillas)

      // Search And Destroy unshaded
      if (remove1Underground && enemyPieces.has(UndergroundGuerrillas)) {
        val removed = selectEnemyRemoveReplaceActivate(remainingEnemy.only(UndergroundGuerrillas), 1)
        killedPieces = killedPieces + removed  // Counts against total hits
      }

      // Abrams unshaded
      val baseFirst = if (remove1BaseFirst && remainingLosses <= numVulnerableForces && remainingEnemy.has(baseTargets)) {
        val removed = selectEnemyRemoveReplaceActivate(remainingEnemy.only(baseTargets), 1)

        killedPieces = killedPieces + removed
        true
      }
      else
        false

      // Remove exposed insurgent pieces, check for tunnel marker removal
      val (killedExposed, affectedTunnel) = selectRemoveEnemyInsurgentBasesLast(remainingEnemy, remainingLosses, params.vulnerableTunnels)
      killedPieces = killedPieces + killedExposed

      EnemyLosses(killedPieces, baseFirst, affectedTunnel)
    }
    
    val regularLosses = calculateLosses(totalLosses)
    // Trigger the M48 Patton capability if it is in play and would
    // be effective (max two non-Lowland spaces)
    val pattonLosses = if (canAddM48Patton(faction, name))
      calculateLosses(totalLosses + 2)
    else
      EnemyLosses()

    val (losses, usedPatton) = if (pattonLosses.total > regularLosses.total) {
      m48PattonSpaces += name
      (pattonLosses, true)
    }
    else
      (regularLosses, false)


    // Log all control changes at the end of the assault
    loggingControlChanges {
      log(s"\n$faction assaults in $name")
      log(separator())

      if (faction == ARVN && !params.free && game.trackResources(ARVN)) {
        if (momentumInPlay(Mo_BodyCount))
          log(s"ARVN Assault costs zero resources [Momentum: $Mo_BodyCount]")
        else
          decreaseResources(ARVN, 3)
      }

      log(s"The assault inflicts ${amountOf(totalLosses, "hit")}")
      if (usedPatton)
        log(s"$faction removes up to 2 extra enemy pieces [$M48Patton_Unshaded]")

      if (losses.baseFirst)
        log(s"$faction removes a base first [$Abrams_Unshaded]")

      if (losses.dead.has(UndergroundGuerrillas))
        log(s"$faction removes an underground guerrilla [$SearchAndDestroy_Unshaded]")

      if (losses.dead.isEmpty && losses.exposedTunnel.isEmpty)
        log("\nNo insurgemnt pieces were removed in the assault")
      else {
        log()
        removePieces(name, losses.dead)
        if (losses.exposedTunnel.nonEmpty) {
          val die = d6
          val success = die > 3
          log("\nNext piece to remove would be a tunneled base")
          log(separator())
          log(s"Die roll is: ${die} [${if (success) "Tunnel destroyed!" else "No effect"}]")
          if (success)
            removeTunnelMarker(name, losses.exposedTunnel)
        }
      }

      // Body Count momentum
      if (momentumInPlay(Mo_BodyCount) && losses.dead.has(Guerrillas)) {
        log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $Mo_BodyCount]")
        increaseUsAid(3 * losses.dead.totalOf(Guerrillas))
      }

      // Each removed base adds +6 aid
      if (faction == ARVN && losses.dead.has(baseTargets)) {
        log(s"\nEach insurgent base removed by ARVN Assault adds +6 Aid")
        increaseUsAid(6 * losses.dead.totalOf(baseTargets))
      }

      // Cobras_Shaded
      //    Eash US assault space, 1 US Troop to Casualties on die roll of 1-3
      if (asUS && sp.pieces.has(USTroops) && capabilityInPlay(Cobras_Shaded)) {
        val die = d6
        val success = die < 4
        log(s"\nCheck for loss of US Troop [$Cobras_Shaded]")
        log(s"Die roll is: ${die} [${if (success) "Troop eliminated" else "No effect"}]")
        if (success) {
          removeToCasualties(name, Pieces(usTroops = 1))
        }
      }

      // SearchAndDestroy_Shaded
      //    Each US and ARVN assault Province shifts support one level toward Active Opposition
      if (searchDestroy && sp.isProvince && sp.canHaveSupport && sp.support != ActiveOpposition) {
        log(s"\nEach assault shifts support toward Active Opposition [$SearchAndDestroy_Shaded]")
        decreaseSupport(name, 1)
      }
    }
  }


  // Type used for checking conditions in the
  // space selection and move priorities tables
  trait PriorityFilter[T] {
    val desc: String
    def filter(spaces: List[T]): List[T]
    override def toString() = desc
  }

  // A boolean priority filter
  // Results in all candidates for which the criteria is true
  class BooleanPriority[T](val desc: String, criteria: (T) => Boolean) extends PriorityFilter[T] {
    def filter(entries: List[T]) = {
      (entries filter criteria)
    }
  }

  // Highest integer score filter used with Priority Tables.
  // Applies the given score function to each entry in the input list and
  // takes the highest value.
  // Then returns the list of entries whose score matches that highest value.
  class HighestScore[T](val desc: String, score: (T) => Int) extends PriorityFilter[T] {
    def filter(entries: List[T]): List[T] = {
      val high = (entries map score).max
      // If none of the candidates qualified to produce a score
      if (high == NO_SCORE)
        Nil
      else {
        botDebug(s"$desc: score = $high")
        entries filter (score(_) == high)
      }
    }
  }

  // Lowest integer score filter used with Priority Tables.
  // Applies the given score function to each entry in the input list and
  // takes the lowest value.
  // Then returns the list of entries whose score matches that lowest value.
  class LowestScore[T](val desc: String, score: (T) => Int) extends PriorityFilter[T] {
    def filter(entries: List[T]): List[T] = {
      val low = (entries map score).min
      // If none of the candidates qualified to produce a score
      if (low == NO_SCORE)
        Nil
      else {
        botDebug(s"$desc): score = $low")
        entries filter (score(_) == low)
      }
    }
  }

  def assaultResult(faction: Faction, cubeTreatment: CubeTreatment, vulnerableTunnels: Boolean)(sp: Space): Space = {
    val pattonExtra     = if (canAddM48Patton(faction, sp.name)) 2 else 0
    val firepower       = assaultFirepower(faction, cubeTreatment)(sp) + pattonExtra
    val num             = firepower min vulnerableInsurgents(sp.pieces, vulnerableTunnels).total
    val (deadPieces, _) = selectRemoveEnemyInsurgentBasesLast(sp.pieces, num)
    sp.copy(pieces = sp.pieces - deadPieces)
  }

  def assaultWouldRemoveBase(faction: Faction, cubeTreatment: CubeTreatment, vulnerableTunnels: Boolean)(sp: Space): Boolean = {
    sp.pieces.totalOf(InsurgentNonTunnels) >
      assaultResult(faction, cubeTreatment, vulnerableTunnels)(sp).pieces.totalOf(InsurgentNonTunnels)
  }

  // Returns the number of the requested pieces types that are in
  // spaces adjecent to the given space.
  def numAdjacentPieces(sp: Space, pieceTypes: Iterable[PieceType]): Int = {
    spaces(getAdjacent(sp.name)).foldLeft(0) { (sum, sp) => sum + sp.pieces.totalOf(pieceTypes) }
  }

  // US space priority filters
  // -----------------------------------------------------------

  // The Trail is a speical case and does noth ave a filter

  val VulnerableBase = new BooleanPriority[Space](
    "Vulnerable Base",
    sp => sp.pieces.has(InsurgentBases) && !sp.pieces.has(UndergroundGuerrillas)
  )

  val ProvinceWithoutCOINBase = new BooleanPriority[Space](
    "Province without a COIN Base",
    sp => sp.isProvince && !sp.pieces.has(CoinBases)
  )

  val ProvinceWithUSBaseAndCubes = new BooleanPriority[Space](
    "Province with a US Base and 0-2 COIN cubes",
    sp => {
      val numCubes = sp.pieces.totalOf(CoinCubes)
      sp.isProvince && sp.pieces.has(USBase) && numCubes >= 0 && numCubes <= 2
    }
  )

  val USTroopsIrregLessUndergroundGuerrillasSupport = new BooleanPriority[Space](
    "US Troops + Irregulars < Undeground Guerrillas at Support",
    sp => {
      val numUS = sp.pieces.totalOf(USTroops::Irregulars)
      val numUnderground = sp.pieces.totalOf(UndergroundGuerrillas)
      !sp.isLoC && sp.canHaveSupport && sp.support > Neutral && numUS < numUnderground
    }
  )

  val HasCoinControl = new BooleanPriority[Space](
    "COIN Control",
    sp => !sp.isLoC && sp.coinControlled
  )

  val CityProvinceNoActiveSupport = new BooleanPriority[Space](
    "City or Province not at Active Support",
    sp => !sp.isLoC && sp.canHaveSupport && sp.support != ActiveSupport
  )

  val PoliceWithoutUSTroops = new BooleanPriority[Space](
    "Police without US Troops",
    sp => sp.pieces.has(ARVNPolice) && !sp.pieces.has(USTroops)
  )

  val COINFirepowerLessThanVulnerable = new BooleanPriority[Space](
    "COIN Firepower < vulnerable enemies where US pieces",
    sp => sp.pieces.has(USPieces) && coinFirepower(NormalTroops)(sp) < vulnerableInsurgents(sp.pieces, false).total
  )

  val HasUSBase = new BooleanPriority[Space](
    "US Base",
    _.pieces.has(USBase)
  )

  val MostPopulation = new HighestScore[Space](
    "Most Population",
    sp => if (sp.isLoC) NO_SCORE else sp.population
  )

  val HasEnemyBase = new BooleanPriority[Space](
    "Enemy Base",
    _.pieces.has(InsurgentBases)
  )

  val LocWithEnemyPieces = new BooleanPriority[Space](
    "LoC with enemy pieces",
    sp => sp.isLoC && sp.pieces.has(InsurgentPieces)
  )

  val MostTotalOpposition = new HighestScore[Space](
    "Most Total Opposition",
    sp => if (sp.isLoC) NO_SCORE else sp.oppositionValue
  )

  val IsHighlandProvince = new BooleanPriority[Space](
    "Highland Province",
    _.isHighland
  )


  // ARVN space priority filters
  // -----------------------------------------------------------

  // ProvinceWithoutCOINBase is defined in the US space priority filters

  // VulnerableBase is defined in the US space priority filters

  val RaidLaosCambodia = new BooleanPriority[Space](
    "Laos/Cambodia with 0-2 enemies without COIN Control",
    sp => isInLaosCambodia(sp.name) && sp.pieces.has(InsurgentPieces) && !sp.coinControlled
  )

  val SouthWithoutCoinControl = new BooleanPriority[Space](
    "City or Province in South Vietname without COIN Control",
    sp => !sp.isLoC && !sp.coinControlled && isInSouthVietnam(sp.name)
  )

  // MostPopulation is defined in the US space priority filters

  // LocWithEnemyPieces is defined in the US space priority filters

  // HasEnemyBase is defined in the US space priority filters

  val MostTotalSupport = new HighestScore[Space](
    "Most Total Support",
    sp => if (sp.isLoC) NO_SCORE else sp.supportValue
  )

  val MostArvnFirepower = new HighestScore[Space](
    "Most ARVN Firepower",
    sp => arvnFirepower(NormalTroops)(sp)
  )

  val CityProvinceFewestEnemyPieces = new LowestScore[Space](
    "City or Province with Fewest Entmy Pieces",
    sp => if (sp.isLoC) NO_SCORE else sp.pieces.totalOf(InsurgentPieces)
  )

  // MostTotalOpposition  is defined in the US space priority filters



  // NVA space priority filters
  // -----------------------------------------------------------

  // MostTotalSupport  is defined in the ARVN space priority filters

  val VulnerableNVABase = new BooleanPriority[Space](
    "Vulnerable NVA Base",
    sp => sp.pieces.has(NVABases) && !sp.pieces.has(UndergroundGuerrillas)
  )

  val CityProvinceMostNVAGuerrillas = new HighestScore[Space](
    "City or Province with Most NVA Guerrilllas",
    sp => if (sp.isLoC)
      NO_SCORE
    else
       sp.pieces.totalOf(NVAGuerrillas)
  )

  val MostNVABases = new HighestScore[Space](
    "Most NVA Bases",
    sp => if (sp.isLoC)
      NO_SCORE
    else
       sp.pieces.totalOf(NVABases)
  )

  val LaosCambodiaWithCoinControl = new BooleanPriority[Space](
    "Laos/Cambodia with COIN Control",
    sp => !sp.isLoC && sp.coinControlled && isInLaosCambodia(sp.name)
  )

  val AdjacentToMostNVATroopsYetToMarch = new HighestScore[Space](
    "Adjacent to most NVA Troops yet to March",
    sp => {
        // Get all adjacent spaces that have not yet been selected as march destinations
        // And return the number of unmoved troops adjacent
        
        val adjacent = NVA_Bot.getNVAAdjacent(sp.name) filterNot moveDestinations.contains
        // Note:  We were considering total troops yet to move in all spaces adjacent
        //        to the target space.  
        //        The designer stated (on BGG) that you should select based on the "one" space
        //        with the most unmoved troops that is adjacent to the target space
        //        Link: https://boardgamegeek.com/thread/2969461/article/41151414#41151414
        
        // adjacent.foldLeft(0) { (totalTroops, name) => totalTroops + notYetMoved(game.getSpace(name)) }
        
        val ajacentTroopNums = adjacent.map { name => notYetMoved(game.getSpace(name), Set(NVATroops)) }
        
        ajacentTroopNums.toList.sorted.lastOption getOrElse 0
    }
  )

  val CityProvinceInSouthWithoutNVAControl = new BooleanPriority[Space](
    "City or Province in South Vietnam without NVA Control",
    sp => !sp.isLoC && !sp.nvaControlled && isInSouthVietnam(sp.name)
  )

  // HasCoinControl is defined in the US space priority filters

  // MostPopulation is defined in the US space priority filters

  val CityProvinceWithoutUSBase = new BooleanPriority[Space](
    "City or Province without US Base",
    sp => !sp.isLoC && !sp.pieces.has(USBase)
  )

  val HasVCBase = new BooleanPriority[Space](
    "VC Base",
    sp => sp.pieces.has(NVABases)
  )

  val CityProvinceFewestNonNVAPieces = new LowestScore[Space](
    "City or Province with fewest non-NVA pieces",
    sp => {
      if (sp.isLoC)
        NO_SCORE
      else
        sp.pieces.except(NVAPieces).total
    }
  )

  val CityProvinceMostCoinCubes = new HighestScore[Space](
    "City or Province with most COIN cubes",
    sp => {
      if (sp.isLoC)
        NO_SCORE
      else
        sp.pieces.totalOf(CoinCubes)
    }
  )

  val CityProvinceHighestAjacentPopulation = new HighestScore[Space](
    "City or Province with highest total ajacent Pop",
    sp => {
      if (sp.isLoC)
        NO_SCORE
      else {
        // This is not used for March so we do not count all spaces in/adjacent to
        // Laos Cmbodia as adjacent when trail == 4
        getAdjacent(sp.name).foldLeft(0) { (sum, name) =>
          val adj_sp = game.getSpace(name)
          val pop = if (adj_sp.isLoC) 0 else adj_sp.population
          sum + pop
        }
      }
    }
  )


  // VC space priority filters
  // -----------------------------------------------------------

  val VulnerableVCBase = new BooleanPriority[Space](
    "Vulnerable VC Base",
    sp => sp.pieces.has(VCBases) && !sp.pieces.has(UndergroundGuerrillas)
  )

  val CityProvinceNoActiveOpposition = new BooleanPriority[Space](
    "City or Province not at Active Opposition",
    sp => !sp.isLoC && sp.canHaveSupport && sp.support != ActiveOpposition
  )

  val CityProvinceFewestVCPieces = new LowestScore[Space](
    "City or Province with fewest VC pieces",
    sp => {
      if (sp.isLoC)
        NO_SCORE
      else
        sp.pieces.totalOf(VCPieces)}
  )

  // MostPopulation is defined in the US space priority filters

  val MostVCBases = new HighestScore[Space](
    "Most VC Bases",
    sp => if (sp.isLoC)
      NO_SCORE
    else
       sp.pieces.totalOf(VCBases)
  )

  // HasCoinControl is defined in the US space priority filters

  // MostTotalSupport  is defined in the ARVN space priority filters

  val HasNVAControl = new BooleanPriority[Space](
    "NVA Control",
    sp => !sp.isLoC && sp.nvaControlled
  )

  val OneTwoVCGuerrillasRoomForBase = new BooleanPriority[Space](
    "1-2 VC Guerrillas and room for a Base",
    sp => {
      val numGuerrillas = sp.pieces.totalOf(VCGuerrillas)
      !sp.isLoC && sp.totalBases < 2 && (numGuerrillas == 1 || numGuerrillas == 2)
    }
  )

  // MostTotalOpposition  is defined in the US space priority filters

  val CityProvinceMostVCGuerrillas = new HighestScore[Space](
    "City or Province with most VC Guerrillas",
    sp => if (sp.isLoC)
      NO_SCORE
    else
       sp.pieces.totalOf(VCGuerrillas)
  )

  val CityProvinceFewestNonVCPieces = new LowestScore[Space](
    "City or Province with fewest non-VC pieces",
    sp => {
      if (sp.isLoC)
        NO_SCORE
      else
        sp.pieces.except(VCPieces).total
    }
  )

  val OnePlusEconLoc = new BooleanPriority[Space](
    "1+ Econ LoC",
    sp => sp.isLoC && sp.printedEconValue > 0
  )

  // Used during Resource Phase of the Coup Rounds
  val HighestEconValue = new HighestScore[Space](
    "Highest Econ",
    sp => if (sp.isLoC) sp.printedEconValue else NO_SCORE
  )

  // Narrow down the list of candidates to those that match best conform
  // to the given priorities.
  // Each filter in the list is used against the given candidates:
  // - If the result is a single candidate then a list with just that candidate
  //   is returned
  // - If the result is more than one candidate then the resulting list is
  //   then used with the next filter in the list.
  // - If the result is that none of the candidates match the filter then
  //   the samme list of candidates is used with the next filter in the list.
  //   Effectivlely skipping that priority filter.
  def narrowCandidates[T](candidates: List[T], priorities: List[PriorityFilter[T]]): List[T] = {
    @tailrec def nextPriority(candidates: List[T], priorities: List[PriorityFilter[T]]): List[T] = {
      (candidates, priorities) match {
        case (Nil, _) => Nil  // Only happens if the initial list of canidates was empty.
        case (sp :: Nil, _) =>
          // Narrowed to a single candidate
          List(sp)
        case (best, Nil) =>
          // No more priorities
          best
        case (list, f :: fs) =>
          (f filter list) match {
            case Nil =>
              botDebug(s"$f: matched nothing")
              nextPriority(list, fs) // Filter entire list by next priority
            case best  =>
              botDebug(s"$f: matched [${andList(best)}]")
              nextPriority(best, fs) // Filter matched list by next priority
          }
      }
    }
    nextPriority(candidates, priorities)
  }

  // Find the best candidate from the given list using the
  // prirority table represented by the list of PriorityFilters.
  //
  // The list of candidates MUST NOT be empty!
  //
  // We narrow the list of candiates to the ones that match most of
  // our priority filters.
  // Then if we still have more than one, we pick one at random.
  def bestCandidate[T](candidates: List[T], priorities: List[PriorityFilter[T]]): T = {
    assert(candidates.nonEmpty, "bestCandidate: called with empty list!")

    narrowCandidates(candidates, priorities) match {
      case best::Nil =>
        botDebug(s"Picked a winner [${best.toString}]")
        best

      case narrowed  =>
        val best = shuffle(narrowed).head        // Take one at random
        botDebug(s"Picked random winner [${best.toString}]")
        best
    }
  }

  def filterIf[T](cond: Boolean, filter: PriorityFilter[T]): Option[PriorityFilter[T]] = {
    if (cond) Some(filter) else None
  }

    // Calculate the max number of the factions pieces we could remove
    // without changing control
  def maxRemovalWithoutChangingControl(sp: Space, faction: Faction): Int = {
    import math.abs
    val totalCoin      = sp.pieces.totalOf(CoinPieces)
    val totalUS        = sp.pieces.totalOf(USPieces)
    val totalARVN      = sp.pieces.totalOf(ARVNPieces)
    val totalInsurgent = sp.pieces.totalOf(InsurgentPieces)
    val totalNVA       = sp.pieces.totalOf(NVAPieces)
    val totalNonNVA    = sp.pieces.totalOf(NonNVAPieces)
    val totalVC        = sp.pieces.totalOf(VCPieces)

    // Calculate the max number of the factions pieces we could remove
    // without changing control
    (faction, sp.control) match {
      case (US,   CoinControl)  => (totalCoin - totalInsurgent - 1) min totalUS
      case (US,   NvaControl)   => totalUS
      case (US,   Uncontrolled) => (totalCoin - ((totalNVA - totalVC) max 0)) min totalUS
      case (ARVN, CoinControl)  => (totalCoin - totalInsurgent - 1) min totalARVN
      case (ARVN, NvaControl)   => totalARVN
      case (ARVN, Uncontrolled) => (totalCoin - ((totalNVA - totalVC) max 0)) min totalARVN
      case (NVA,  CoinControl)  => totalNVA
      case (NVA,  NvaControl)   => (totalNVA - totalNonNVA - 1) max 0
      case (NVA,  Uncontrolled) => totalNVA - ((totalCoin - totalVC) max 0)
      case (VC,   CoinControl)  => totalVC
      case (VC,   NvaControl)   => totalVC
      case (VC,   Uncontrolled) => (totalVC - abs(totalCoin - totalNVA)) max 0
    }
  }

  def alternate[T](list1: List[T], list2: List[T]): List[T] = {
    (list1, list2) match {
      case (Nil, Nil)     => Nil
      case (x::xs, Nil)   => list1
      case (Nil, x::xs)   => list2
      case (x::xs, y::ys) => x::y::alternate(xs, ys)
    }
  }

  def selectFriendlyRemoval(pieces: Pieces, num: Int): Pieces = {
    // ARVN Troops/Police must be taken alternately.
    val beforeArvnCubes = pieces.explode(List(NVAGuerrillas_A, VCGuerrillas_A, NVAGuerrillas_U, VCGuerrillas_U,
                                                  Irregulars_A, Rangers_A, Irregulars_U, Rangers_U))
    val arvnCubes       = alternate(pieces.explode(ARVNTroops::Nil), pieces.explode(ARVNPolice::Nil))
    val afterArvnCubes  = pieces.explode(List(NVATroops, USTroops, VCBase, NVABase, ARVNBase, USBase, VCTunnel, NVATunnel))

    Pieces.fromTypes((beforeArvnCubes:::arvnCubes:::afterArvnCubes).take(num))
  }

  // When placing enemy pieces for an event.
  def selectEnemyPlacement(pieces: Pieces, num: Int): Pieces =
    selectFriendlyRemoval(pieces: Pieces, num: Int)

  def humanPiecesFirst(list: List[PieceType]): List[PieceType] = {
    val human = list filter (t => game.isHuman(owner(t)))
    val bot   = list filter (t => game.isBot(owner(t)))

    human:::bot
  }

  //  After creating this list of pieces, split the list into (Human, Bot) owned
  //  the concatenate those lists so that all of the Human owned pieces are removed first.
  def selectEnemyRemoveReplaceActivate(pieces: Pieces, num: Int): Pieces = {
    // ARVN Police/Troops must be taken alternately.
    val beforeArvnCubes = pieces.explode(List(NVATunnel, VCTunnel, USBase, ARVNBase, NVABase, VCBase, USTroops, NVATroops))
    val arvnCubes       = alternate(pieces.explode(ARVNPolice::Nil), pieces.explode(ARVNTroops::Nil))
    val afterArvnCubes  = pieces.explode(List(Rangers_U, Irregulars_U,Rangers_A, Irregulars_A,
                                              VCGuerrillas_U, NVAGuerrillas_U, VCGuerrillas_A, NVAGuerrillas_A))
    Pieces.fromTypes(humanPiecesFirst(beforeArvnCubes:::arvnCubes:::afterArvnCubes).take(num))
  }

  def selectRemoveEnemyCoinBasesLast(pieces: Pieces, num: Int): Pieces = {
    val coinBases  = pieces.only(CoinBases)
    val coinForces = pieces.only(CoinForces)
    val numForces  = num min coinForces.total
    val deadForces = selectEnemyRemoveReplaceActivate(coinForces, numForces)
    val numBases   = (num - deadForces.total) min coinBases.total
    val deadBases  = selectEnemyRemoveReplaceActivate(coinBases, numBases)

    (deadForces + deadBases)
  }

  //  Removes NVA Troops first, followed by active guerrillas
  //  then if no underground guerrillas remain, bases.
  //  If a Tunneled base is "vulnerable" is to be removed,
  //  we stop removing pieces.
  //  Roll a 1d6, 1-3 no effect, 4-6 remove tunnel marker
  //  Return all pieces that should be removed,
  //  also return a max of one Tunneled Base if its marker should be removed.
  def selectRemoveEnemyInsurgentBasesLast(pieces: Pieces, num: Int, vulnerableTunnels: Boolean = false): (Pieces, Pieces) = {
    val NonTunnelTypes   = if (vulnerableTunnels) InsurgentBases else InsurgentNonTunnels
    val insurgentTunnels = pieces.only(InsurgentTunnels)
    var deadPieces       = Pieces()
    var affectedTunnel   = Pieces()
    def remainingPieces  = pieces - deadPieces
    def numRemaining     = num - deadPieces.total

    def removeForces(types: Iterable[PieceType]): Unit = if (numRemaining > 0) {
      val candidates = pieces.only(types)
      val num = numRemaining min candidates.total
      deadPieces = deadPieces + selectEnemyRemoveReplaceActivate(candidates, num)
    }

    removeForces(NVATroops::Nil)
    removeForces(ActiveGuerrillas)

    // Check for base/tunnel marker removal
    if (!remainingPieces.has(InsurgentForces)) {
      val bases    = remainingPieces.only(NonTunnelTypes)
      val numBases = numRemaining min bases.total
      deadPieces = deadPieces + selectEnemyRemoveReplaceActivate(bases, numBases)

      if (!vulnerableTunnels && !remainingPieces.has(NonTunnelTypes)) {
        val tunnels    = remainingPieces.only(InsurgentTunnels)
        val numTunnels = numRemaining min tunnels.total
        affectedTunnel = selectEnemyRemoveReplaceActivate(tunnels, 1)
      }
    }

    (deadPieces, affectedTunnel)
  }
  // Function to make code clearer to read
  // just an alias for selectEnemyRemoveReplaceActivate()
  def selectFriendlyToPlaceOrMove(pieces: Pieces, num: Int): Pieces = {
        // ARVN Police/Troops must be taken alternately.
    val beforeArvnCubes = pieces.explode(List(NVATunnel, VCTunnel, USBase, ARVNBase, NVABase, VCBase, USTroops, NVATroops))
    val arvnCubes       = alternate(pieces.explode(ARVNPolice::Nil), pieces.explode(ARVNTroops::Nil))
    val afterArvnCubes  = pieces.explode(List(Rangers_U, Irregulars_U,Rangers_A, Irregulars_A,
                                              VCGuerrillas_U, NVAGuerrillas_U, VCGuerrillas_A, NVAGuerrillas_A))

    Pieces.fromTypes((beforeArvnCubes:::arvnCubes:::afterArvnCubes).take(num))
  }

  // Use when determining which moveable pieces should remain
  // in their origin space.
  // This is the same as when placing friendly pieces.
  def selectFriendlyToKeepInPlace(pieces: Pieces, num: Int): Pieces = selectFriendlyToPlaceOrMove(pieces, num)

  // Convenience classes so we don't have to pass too many params around
  case class MoveParams(
    origin: Space,   // Origin space when determining kept pieces, Dest space when determining movers
    dest: Space,     // Destination space
    faction: Faction,
    action: MoveAction,
    moveTypes: Set[PieceType],
    maxPieces: Int, // Transport limits to 6 pieces per destination
    origCandidates: Pieces,  // The original set of pieces that may be kept/moved
    selected: Pieces = Pieces()) {  // Kept pieces or moving pieces

    val candidates = origCandidates - selected
    val maxCanMove = candidates.total min (maxPieces - selected.total)
    def addPieces(newPieces: Pieces) = copy(selected = selected + newPieces)
  }

  def logKept(params: MoveParams, heading: String, result: String): Unit = if (game.botDebug) {
    import params._

    log()
    log(heading)
    log(separator())
    if (result.nonEmpty)
      log(s"result    : $result")
    log(s"candidates: ${andList(candidates.descriptions)}")
    log(s"kept      : ${andList(selected.descriptions)}")
  }

  def logMoved(params: MoveParams, heading: String, result: String): Unit = if (game.botDebug) {
    import params._

    val nowAtDest = dest.pieces.only(factionPieces(faction)) + selected
    log()
    log(heading)
    log(separator())
    if (result.nonEmpty)
      log(s"result     : $result")
    log(s"candidates : ${andList(candidates.descriptions)}")
    log(s"moving     : ${andList(selected.descriptions)}")
    log(s"now at dest: ${andList(nowAtDest.descriptions)}")
  }

  //  Used when selecting pieces to keep in an origin space and when
  //  selecting pieces to move to a destination space
  type MovePriority = (MoveParams) => MoveParams

  //  Move Priorities for selecting pieces to keep in a origin space
  //  ==============================================================

  //  Used by all factions
  val KP_ChangeNoControlSouthVietnam = (params: MoveParams) => {
    import params._
    val desc = "Change No Control in South Vietnam"
    if (origin.isLoC || !isInSouthVietnam(origin.name)) {
      logKept(params, desc, "ignore [Origin is Loc or is not in South Vietnam]")
      params
    }
    else {
      val numFaction = origin.pieces.only(factionPieces(faction)).total
      val numOthers  = origin.pieces.only(factionPieces(faction)).except(moveTypes).total
      val maxMovers  = candidates.total min maxRemovalWithoutChangingControl(origin, faction)
      val maxKeep    = numFaction - maxMovers
      val numKeep    = (maxKeep - numOthers) max 0
      val toKeep     = selectFriendlyToKeepInPlace(candidates, numKeep)
      val newParams  = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
      newParams
    }
  }

  //  Used by US and ARVN
  //  If the destination is not in South Vietnam,
  //  add all of the COIN cubes to the kept pieces.
  val KP_KeepAllCubesInSouthVietnam = (params: MoveParams) => {
    val desc = "Keep all cubes in South Vietnam"
    if (isInSouthVietnam(params.dest.name)) {
      logKept(params, desc, "ignore [Destination is in South Vietnam]")
      params
    }
    else {
      val toKeep    = params.candidates.only(CoinCubes)
      val newParams = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep - ${andList(toKeep.descriptions)}")
      newParams
    }
  }

  //  Used by US and ARVN
  //  If the origin is a LoC keep coin cubes equal to the
  //  number of enemy pieces.  If there a currently less
  //  COIN cubes that enemy pieces, this priority is ignored.
  //  add all of the COIN cubes to the kept pieces.
  val KP_KeepCoinCubesEqualToEnemyOnLoCs = (params: MoveParams) => {
    import params._
    val desc = "Keep COIN cubes equal enemy on LoCs"
    if (dest.isLoC) {
      val (factionCubes, otherCubes) = if (faction == US)
        (List(USTroops), ARVNCubes)
        else
        (ARVNCubes, List(USTroops))
      val numEnemy     = origin.pieces.totalOf(InsurgentForces)
      val numOtherCoin = origin.pieces.totalOf(otherCubes)
      val numNonMovers = (origin.pieces - candidates).totalOf(factionCubes)
      val numCubesNow  = numOtherCoin + numNonMovers
      val totalCubes   = numCubesNow + candidates.totalOf(factionCubes)
      // If the condition cannot be met then it is ignored
      if (totalCubes < numEnemy) {
        logKept(params, desc, "ignore [Less COIN cubes than enemy]")
        params
      }
      else if (numCubesNow >= numEnemy) {
        logKept(params, desc, "ignore [Already have enough COIN cubes]")
        params
      }
      else {
        val numKeep   = numEnemy - numCubesNow
        val toKeep    = selectFriendlyToKeepInPlace(candidates.only(factionCubes), numKeep)
        val newParams = params.addPieces(toKeep)

        logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
        newParams
      }
    }
    else {
      logKept(params, desc, "ignore [Origin is not a LoC]")
      params
    }
  }

  //  Used by US and ARVN
  val KP_Keep3CoinCubesWithCoinBase = (params: MoveParams) => {
    import params._
    val desc = "Keep 3 COIN cubes in space with a COIN base"
    if (origin.pieces.has(CoinBases)) {
      val (factionCubes, otherCubes) = if (faction == US)
        (List(USTroops), ARVNCubes)
      else
        (ARVNCubes, List(USTroops))
      val numOtherCoin = origin.pieces.totalOf(otherCubes)
      val numNonMovers = (origin.pieces - candidates).totalOf(factionCubes)
      val numCubesNow  = numOtherCoin + numNonMovers
      val totalCubes   = numCubesNow + candidates.totalOf(factionCubes)

      // If the condition cannot be met then it is ignored
      if (totalCubes < 3) {
        logKept(params, desc, "ignore [Not enough COIN cubes to satisfy the instruction]")
        params
      }
      else if (numCubesNow >= 3) {
        logKept(params, desc, "ignore [Already at least 3 COIN cubes in the space]")
        params
      }
      else {
        val numKeep   = 3 - numCubesNow
        val toKeep    = selectFriendlyToKeepInPlace(candidates.only(factionCubes), numKeep)
        val newParams = params.addPieces(toKeep)

        logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
        newParams
      }
    }
    else {
      logKept(params, desc, "ignore [Origin does not have a Coin base]")
      params
    }
  }

  //  Used by US and ARVN
  val KP_KeepCoinFirepowerGreaterOrEqualToVulnerable = (params: MoveParams) => {
    import params._
    val desc = "Keep COIN firepower >= vulnerable enemy"
    val firepower  = coinFirepower(NormalTroops)(origin)
    val vulnerable = vulnerableInsurgents(origin.pieces, false).total

    if (firepower >= vulnerable) {

      def keepPieces(num: Int): Pieces = {
        if (num == candidates.total)
          candidates  // Must keep all of them in place
        else {
          val toKeep = selectFriendlyToKeepInPlace(candidates, num)
          // If our firepower is still sufficent then we are done
          val updated = origin.pieces.except(moveTypes) + selected + toKeep
          if (coinFirepower(NormalTroops)(origin.setPieces(updated)) >= vulnerable)
            toKeep
          else  // Try again keeping one more
            keepPieces(num + 1)
        }
      }

      // Start by see what happens if we don't keep any
      val toKeep    = keepPieces(0)
      val newParams = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
      newParams
    }
    else {
      logKept(params, desc, "ignore [COIN firepower < vulnerable enemy pieces]")
      params
    }
  }

  // Used by ARVN
  val KP_ArvnCubesGreaterThanUsCubesAtSupport = (params: MoveParams) => {
    import params._
    val desc = "Keep ARVN cubes > US Troops at Support"

    val numUS      = origin.pieces.totalOf(USTroops)
    val numARVNNow = (origin.pieces - candidates).totalOf(ARVNCubes)
    val totalARVN  = numARVNNow + candidates.totalOf(ARVNCubes)

    val numARVN = origin.pieces.totalOf(ARVNCubes)
    if (origin.isLoC || (origin.population != 1 && origin.population != 2) || origin.support <= Neutral) {
      logKept(params, desc, "ignore [Not 1-2 pop space with Support]")
      params
    }
    else if (totalARVN <= numUS) {
      logKept(params, desc, "ignore [ARVN cubes <= US Troops]")
      params
    }
    else if (numARVNNow > numUS) {
      logKept(params, desc, "ignore [Already have ARVN cubes > US Troops]")
      params
    }
    else {
        val numKeep   = numUS + 1 - numARVNNow
        val toKeep    = selectFriendlyToKeepInPlace(candidates.only(ARVNCubes), numKeep)
        val newParams = params.addPieces(toKeep)

        logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
        newParams
    }
  }

  // Used by US
  val KP_USForcesGreaterOrEqualUndergroundGuerrilas = (params: MoveParams) => {
    import params._
    val desc = if (origin.isJungle)
      "Keep US Troops + Irregulars >= 2 x Underground Guerrillas in Jungle"
    else
      "Keep US Troops + Irregulars >= Underground Guerrillas"

    val numUSNow = (origin.pieces - candidates).totalOf(USForces)
    val totalUS  = numUSNow + candidates.totalOf(USForces)
    val numUG    = origin.pieces.totalOf(UndergroundGuerrillas)
    val needed   = if (origin.isJungle) numUG * 2 else numUG

    if (totalUS < needed) {
      logKept(params, desc, "ignore [Not enough US Troops + Irregulars to meet requirement]")
      params
    }
    else if (numUSNow >= needed) {
      logKept(params, desc, "ignore [Already have enough US Troops + Irregulars]")
      params
    }
    else {
      val numKeep   = needed - numUSNow
      val toKeep    = selectFriendlyToKeepInPlace(candidates.only(USForces), numKeep)
      val newParams = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
      newParams
    }
  }

  // Used by US
  val KP_1USTroopSaigonAnd2PopSpacesWithoutActiveSupport = (params: MoveParams) => {
    import params._
    val desc = "Keep 1 US Troop in Saigon and all 2-Pop not at Active Support"

    if (origin.name == Saigon || (!origin.isLoC && origin.population == 2 && origin.support != ActiveSupport)) {
      val numTroopsNow = (origin.pieces - candidates).totalOf(USTroops)
      val totalTroops  = numTroopsNow + candidates.totalOf(USTroops)

      if (totalTroops == 0) {
        logKept(params, desc, "ignore [No US Troops to meet requirement]")
        params
      }
      else if (numTroopsNow > 0) {
        logKept(params, desc, "ignore [Already has a US Troop]")
        params
      }
      else {
        val toKeep    = selectFriendlyToKeepInPlace(candidates.only(USTroops), 1)
        val newParams = params.addPieces(toKeep)

        logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
        newParams
      }
    }
    else {
      logKept(params, desc, "ignore [Not Saigon or 2-Pop not at Active Support]")
      params
    }
  }

  // Used by US
  val KP_USTroopsGreaterOrEqualArvnCubesAtSupport = (params: MoveParams) => {
    import params._
    val desc = "Keep US Troops >= ARVN cubes in 2-Pop with Suport"
    val numARVN      = origin.pieces.totalOf(ARVNCubes)
    val numUSNow = (origin.pieces - candidates).totalOf(USTroops)
    val totalUS  = numUSNow + candidates.totalOf(USTroops)

    if (origin.isLoC || origin.population != 2 || origin.support <= Neutral) {
      logKept(params, desc, "ignore [Not 2-Pop space with Support]")
      params
    }
    else if (numUSNow >= numARVN) {
      logKept(params, desc, "ignore [Already has US Troops >= ARVN cubes]")
      params
    }
    else if (totalUS < numARVN) {
      logKept(params, desc, "ignore [US Troops < ARVN cubes]")
      params
    }
    else {
        val numKeep   = numARVN - numUSNow
        val toKeep    = selectFriendlyToKeepInPlace(candidates.only(USTroops), numKeep)
        val newParams = params.addPieces(toKeep)

        logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
        newParams
    }
  }

  // Used by NVA and VC
  val KP_1ActingGuerrillaWithActingBase = (params: MoveParams) => {
    import params._
    val desc = "Keep 1 acting faction Guerrilla in space with acting faction base"
    val FactionGuerrillas = if (faction == NVA) NVAGuerrillas else VCGuerrillas

    if (!origin.pieces.has(factionBases(faction))) {
      logKept(params, desc, s"ignore [No $faction base in the space]")
      params
    }
    else if ((origin.pieces - candidates) has FactionGuerrillas) {
      logKept(params, desc, s"ignore [Already a $faction Guerrilla in the space]")
      params
    }
    else {
      val toKeep    = selectFriendlyToKeepInPlace(candidates.only(FactionGuerrillas), 1)
      val newParams = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
      newParams
    }
  }

  // Used by NVA and VC
  // VC will ignore this in Laos/Cambodia
  val KP_3ActingGuerrillasWithRoomForAvailableBase = (params: MoveParams) => {
    import params._
    val desc = "Keep 3 acting faction Guerrillas in space with room for Available Base"
    val FactionGuerrillas = if (faction == NVA) NVAGuerrillas else VCGuerrillas

    val numGNow = (origin.pieces - candidates).totalOf(FactionGuerrillas)
    val totalG  = numGNow + candidates.totalOf(FactionGuerrillas)

    if (origin.totalBases == 2) {
      logKept(params, desc, "ignore [No room for a base in the origin space]")
      params
    }
    else if (!game.availablePieces.has(factionBases(faction))) {
      logKept(params, desc, s"ignore [No available $faction bases]")
      params
    }
    else if (numGNow >= 3) {
      logKept(params, desc, s"ignore [Already 3 or more $faction Guerrillas in the space]")
      params
    }
    else if (totalG < 3) {
      logKept(params, desc, s"ignore [Not enough $faction Guerrillas to meet requirement]")
      params
    }
    else {
      val numKeep   = 3 - numGNow
      val toKeep    = selectFriendlyToKeepInPlace(candidates.only(FactionGuerrillas), numKeep)
      val newParams = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
      newParams
    }
  }

  // Used by NVA and VC
  val KP_FriendlyGuerrillasGreaterThanEnemiesOnLoCs = (params: MoveParams) => {
    import params._
    val desc = "Keep friendly Guerrillas > enemy on LoCs"
    val (ourTypes, theirTypes) = if (faction == NVA)
      (NVAGuerrillas, VCGuerrillas)
    else
      (VCGuerrillas, NVAGuerrillas)
    val numEnemy    = origin.pieces.totalOf(CoinForces)
    val numTheirs   = origin.pieces.totalOf(theirTypes)
    val numOursNow  = (origin.pieces - candidates).totalOf(ourTypes)
    val friendsNow  = numTheirs + numOursNow
    val totalFriends= friendsNow + candidates.totalOf(ourTypes)

    val totalOurs   = numOursNow + candidates.totalOf(ourTypes)
    val numFriendly = origin.pieces.totalOf(Guerrillas) // Include other faction's guerrillas

    if (!origin.isLoC) {
      logKept(params, desc, "ignore [Not a LoC]")
      params
    }
    else if (friendsNow > numEnemy) {
      logKept(params, desc, "ignore [Already more Friendly Guerrillas than enemy]")
      params
    }
    else if (totalFriends <= numEnemy) {
      logKept(params, desc, "ignore [Not enough Friendly Guerrillas to meet requirement]")
      params
    }
    else {
      val numKeep   = numEnemy + 1 - numOursNow
      val toKeep    = selectFriendlyToKeepInPlace(candidates.only(ourTypes), numKeep)
      val newParams = params.addPieces(toKeep)

      logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
      newParams
    }
  }

  // Used by VC
  val KP_1UndergroundIn1PopNotAtActiveSupport = (params: MoveParams) => {
    import params._
    val desc = "Keep 1 Undeground VC Guerrilla in 1+ Pop space not at Active Opposition"

    if (!origin.isLoC && origin.canHaveSupport && origin.support != ActiveOpposition) {
      val numNow  = (origin.pieces - candidates).totalOf(VCGuerrillas_U)

      if (numNow > 0) {
        logKept(params, desc, "ignore [Already have VC Underground Guerrilla in space]")
        params
      }
      else if (!candidates.has(VCGuerrillas_U)) {
        logKept(params, desc, "ignore [Do not have an Underground Guerrrilla to keep in the space]")
        params
      }
      else {
        val toKeep    = selectFriendlyToKeepInPlace(candidates.only(VCGuerrillas_U), 1)
        val newParams = params.addPieces(toKeep)

        logKept(newParams, desc, s"keep [${andList(toKeep.descriptions)}]")
        newParams
      }
    }
    else {
      logKept(params, desc, "ignore [Space does not meet criteria]")
      params
    }
  }


  //  Move Priorities for selecting pieces to move to a destination
  //  ==============================================================

  // Used by NVA for Easter Offensive (on LoC adjacent to Saigon)
  val MP_MoveMaxTroopsToLocAdjacentToSaigon = (params: MoveParams) => {
    import params._
    val desc    = "Move Max Troops onto LoCs adjacent to Saigon with no COIN"
    val hasCoin = dest.pieces.has(CoinPieces)

    // If no Coin the move all of the Troops
    val toMove    = if (hasCoin) Pieces() else candidates.only(NVATroops)
    val newParams = params.addPieces(toMove)

    logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
    newParams
  }

  // Used by US (on LoC)
  val MP_GetCoinFirepowerEqualEnemies = (params: MoveParams) => {
    import params._
    val desc = "Get COIN Firepower equal to enemies"
    val numEnemy = dest.pieces.totalOf(InsurgentPieces)

    def movePieces(num: Int): Pieces = {
      val toMove = selectFriendlyToPlaceOrMove(candidates, num)
      // If our firepower is sufficent then we are done
      val updated = dest.pieces + selected + toMove

      if (coinFirepower(NormalTroops)(dest.setPieces(updated)) >= numEnemy || num == maxCanMove)
        toMove
      else  // Try again moving one more
        movePieces(num + 1)
    }

    // Start by see what happens if we don't move any
    val toMove    = movePieces(0)
    val newParams = params.addPieces(toMove)

    logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
    newParams
  }

  // Used by ARVN (on LoC)
  val MP_GetArvnFirepowerEqualEnemies = (params: MoveParams) => {
    import params._
    val desc = "Get ARVN Firepower equal to enemies"
    val numEnemy = dest.pieces.totalOf(InsurgentPieces)

    def movePieces(num: Int): Pieces = {
      val toMove = selectFriendlyToPlaceOrMove(candidates, num)
      // If our firepower is sufficent then we are done
      val updated = dest.pieces + selected + toMove

      if (arvnFirepower(NormalTroops)(dest.setPieces(updated)) >= numEnemy || num == maxCanMove)
        toMove
      else  // Try again moving one more
        movePieces(num + 1)
    }


    // Start by see what happens if we don't move any
    val toMove    = movePieces(0)
    val newParams = params.addPieces(toMove)

    logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
    newParams
  }

  // Used by ARVN (on LoC)
  val MP_GetGuerrillasGreaterThanCoinForces = (params: MoveParams) => {
    import params._
    val desc = "Get Guerrillas > COIN forces"
    val numCoin = dest.pieces.totalOf(CoinForces)

    val numGuerrillas = dest.pieces.totalOf(Guerrillas) + selected.totalOf(Guerrillas)

    if (numGuerrillas > numCoin) {
      logMoved(params, desc, "ignore [Guerrillas already > COIN forces]")
      params
    }
    else {
      val numMove   = (numCoin + 1 - numGuerrillas) min maxCanMove
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(Guerrillas), numMove)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by US, ARVN, VC in (City/Province)
  val MP_Get1PieceToDestination = (params: MoveParams) => {
    import params._
    val desc = "Get 1 acting faction piece to destination"
    val nowAtDest = dest.pieces + selected

    if (nowAtDest.has(factionPieces(faction))) {
      logMoved(params, desc, s"ignore [Already a $faction piece in destination]")
      params
    }
    else {
      val toMove    = selectFriendlyToPlaceOrMove(candidates, 1)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by US, ARVN in (City/Province)
  val MP_Get3CoinCubesToDestination = (params: MoveParams) => {
    import params._
    val desc = "Get 3 COIN cubes to destination with US Base"

    val nowAtDest    = dest.pieces + selected
    val numCoinCubes = nowAtDest.totalOf(CoinCubes)

    if (!nowAtDest.has(USBase)) {
      logMoved(params, desc, "ignore [No US base in destination]")
      params
    }
    else if (numCoinCubes >= 3) {
      logMoved(params, desc, "ignore [Already at least 3 COIN cubes in destination]")
      params
    }
    else {
      val num       = (3 - numCoinCubes) min maxCanMove
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(CoinCubes), num)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by US in (City/Province)
  val MP_GetCoinFirepowerEqualToVulnerable = (params: MoveParams) => {
    import params._
    val desc = "Get COIN Firepower equal to vulnerable enemies"

    val numVulnerable = vulnerableInsurgents(dest.pieces, false).total

    def movePieces(num: Int): Pieces = {
      val toMove = selectFriendlyToPlaceOrMove(candidates, num)
      // If our firepower is sufficent then we are done
      val nowAtDest    = dest.pieces + selected + toMove

      if (coinFirepower(NormalTroops)(dest.setPieces(nowAtDest)) >= numVulnerable || num == maxCanMove)
        toMove
      else  // Try again moving one more
        movePieces(num + 1)
    }

    // Start by see what happens if we don't move any
    val toMove    = movePieces(0)
    val newParams = params.addPieces(toMove)

    logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
    newParams
  }

  // Used by US in (City/Province)
  val MP_GetUSForcesEqualToUnderground = (params: MoveParams) => {
    import params._
    val desc = if (origin.isJungle)
      "Get US Troops + Irregulars equal to 2 x Underground Guerrillas in Jungle"
    else
      "Get US Troops + Irregulars equal to Underground Guerrillas"

    val needed = if (origin.isJungle)
      2 * dest.pieces.totalOf(UndergroundGuerrillas)
    else
      dest.pieces.totalOf(UndergroundGuerrillas)

    val nowAtDest   = dest.pieces + selected
    val numUsForces = nowAtDest.totalOf(USForces)

    if (needed == 0) {
      logMoved(params, desc, "ignore [No Underground Guerrillas in destination]")
      params
    }
    else if (numUsForces >= needed) {
      logMoved(params, desc, "ignore [Conditon already met in destination]")
      params
    }
    else {
      val num       = (needed - numUsForces) min maxCanMove
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(USForces), num)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }

  }

  // Used by US Patrol to Saigon, ARVN (Sweep|Patrol|Transport) in (City/Province)
  val MP_GetCoinControl = (params: MoveParams) => {
    import params._
    val desc = "Get Coin Control"

    val nowAtDest = dest.pieces + selected

    if (nowAtDest.totalOf(CoinPieces) > nowAtDest.totalOf(InsurgentPieces)) {
      logMoved(params, desc, "ignore [Destination already COIN controlled]")
      params
    }
    else {
      val numMove   = (nowAtDest.totalOf(InsurgentPieces) + 1 - nowAtDest.totalOf(CoinPieces)) min maxCanMove
      val toMove    = selectFriendlyToPlaceOrMove(candidates, numMove)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by NVA in (City/Province)
  val MP_GetNvaControlWithCoinForces = (params: MoveParams) => {
    import params._
    val desc = "Get NVA Control (In South Vietnam with COIN forces)"

    val totalNvaTroops = origin.pieces.totalOf(NVATroops) + dest.pieces.totalOf(NVATroops)
    val dieRoll        = d6 + d6
    val nowAtDest      = dest.pieces + selected

    if (!isInSouthVietnam(dest.name)) {
      logMoved(params, desc, "ignore [Destination not in South Vietnam]")
      params
    }
    else if (nowAtDest.totalOf(NVAPieces) > nowAtDest.totalOf(NonNVAPieces)) {
      logMoved(params, desc, "ignore [Destination already NVA Controlled]")
      params
    }
    else if (!dest.pieces.has(CoinForces)) {
      logMoved(params, desc, "ignore [No COIN forces in destination]")
      params
    }
    else if (dieRoll > totalNvaTroops) {
      logMoved(params, desc, s"ignore [2d6 ($dieRoll) > NVA Troops in origin + destination ($totalNvaTroops)]")
      params
    }
    else {
      val numMove   = (nowAtDest.totalOf(NonNVAPieces) + 1 - nowAtDest.totalOf(NVAPieces)) min maxCanMove
      val toMove    = selectFriendlyToPlaceOrMove(candidates, numMove)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by NVA, VC in (City/Province)
  val MP_Get3FactionGuerrillasForAvailableBase = (params: MoveParams) => {
    import params._
    val desc = "Get 3 acting faction Guerrillas to destination for Available Base"

    val FactionGuerrillas = if (faction == NVA) NVAGuerrillas else VCGuerrillas
    val nowAtDest         = dest.pieces + selected

    if (dest.totalBases == 2) {
      logKept(params, desc, "ignore [No room for a base in the destination]")
      params
    }
    else if (!game.availablePieces.has(factionBases(faction))) {
      logKept(params, desc, s"ignore [$faction does not have an available base]")
      params
    }
    else if (nowAtDest.totalOf(FactionGuerrillas) >= 3) {
      logKept(params, desc, s"ignore [$faction already has at least 3 Guerrillas in destination]")
      params
    }
    else {
      val numMove   = (3 - nowAtDest.totalOf(FactionGuerrillas)) min maxCanMove
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(FactionGuerrillas), numMove)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by NVA, VC in (City/Province)
  val MP_Get1FactionGuerrillasToBase = (params: MoveParams) => {
    import params._
    val desc = "Get 1 acting faction Guerrilla to friendly Base"

    val FactionGuerrillas = if (faction == NVA) NVAGuerrillas else VCGuerrillas
    val nowAtDest         = dest.pieces + selected

    if (!nowAtDest.has(InsurgentBases)) {
      logKept(params, desc, "ignore [No Insurgent (friendly) base in destination]")
      params
    }
    else if (nowAtDest.has(FactionGuerrillas)) {
      logKept(params, desc, s"ignore [Already at least 1 $faction Guerrilla in destination]")
      params
    }
    else {
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(FactionGuerrillas), 1)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by NVA in (City/Province)
  val MP_GetNvaControlWithoutCoinForcesOrLaosCambodia = (params: MoveParams) => {
    import params._
    val desc = "Get NVA Control (South Vietname without COIN forces or Laos/Cambodia)"

    val nowAtDest      = dest.pieces + selected
    val numCoinForces  = nowAtDest.totalOf(CoinForces)

    if ((isInSouthVietnam(dest.name) && numCoinForces == 0) || isInLaosCambodia(dest.name)) {
      if (nowAtDest.totalOf(NVAPieces) > nowAtDest.totalOf(NonNVAPieces)) {
        logMoved(params, desc, "ignore [Destination already NVA Controlled]")
        params
      }
      else {
        val numMove   = (nowAtDest.totalOf(NonNVAPieces) + 1 - nowAtDest.totalOf(NVAPieces)) min maxCanMove
        val toMove    = selectFriendlyToPlaceOrMove(candidates, numMove)
        val newParams = params.addPieces(toMove)

        logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
        newParams
      }
    }
    else {
      logMoved(params, desc, "ignore [Not South Viename without forces or Laos/Cambodia]")
      params
    }
  }

  def movePriIf(condition: Boolean, priority: MovePriority): Option[MovePriority] = if (condition) Some(priority) else None

  //  When moving pieces, this is used to determine which of the factions pieces must be
  //  kept in the origin space.
  def selectPiecesToKeep(originName: String,
                         destName: String,
                         faction: Faction,
                         action: MoveAction,
                         moveTypes: Set[PieceType],
                         opParams: Params): Pieces = {
    val us      = faction == US
    val arvn    = faction == ARVN
    val nva     = faction == NVA
    val vc      = faction == VC
    val usSweep = us && action == Sweep
    val planBase = (vc || nva) && opParams.march.amassForBase

    def nextKeepPriority(priorities: List[MovePriority], params: MoveParams): Pieces = {
      priorities match {
        case _ if params.candidates.isEmpty => params.selected
        case Nil                            => params.selected
        case priority::rest                 => nextKeepPriority(rest, priority(params))
      }
    }

    val keepPriorities = List(
      movePriIf(true,       KP_ChangeNoControlSouthVietnam),
      movePriIf(us || arvn, KP_KeepAllCubesInSouthVietnam),
      movePriIf(us || arvn, KP_KeepCoinCubesEqualToEnemyOnLoCs),
      movePriIf(us || arvn, KP_Keep3CoinCubesWithCoinBase),
      movePriIf(us || arvn, KP_KeepCoinFirepowerGreaterOrEqualToVulnerable),
      movePriIf(arvn,       KP_ArvnCubesGreaterThanUsCubesAtSupport),
      movePriIf(usSweep,    KP_USForcesGreaterOrEqualUndergroundGuerrilas),
      movePriIf(us,         KP_1USTroopSaigonAnd2PopSpacesWithoutActiveSupport),
      movePriIf(us,         KP_USTroopsGreaterOrEqualArvnCubesAtSupport),
      movePriIf(nva || vc,  KP_1ActingGuerrillaWithActingBase),
      movePriIf(planBase,   KP_3ActingGuerrillasWithRoomForAvailableBase),
      movePriIf(nva || vc,  KP_FriendlyGuerrillasGreaterThanEnemiesOnLoCs),
      movePriIf(vc,         KP_1UndergroundIn1PopNotAtActiveSupport)
    ).flatten

    val origin     = game.getSpace(originName)
    val dest       = game.getSpace(destName)
    val candidates = origin.pieces.only(moveTypes)
    val params     = MoveParams(origin, dest, faction, action, moveTypes, NO_LIMIT, candidates)

    logKept(params, s"$faction is selecting pieces to keep in $originName", "")
    nextKeepPriority(keepPriorities, params)
  }

  //  When moving pieces, this is used to determine which of the factions pieces should be moved
  //  from the origin to the destination.
  def selectPiecesToMove(originName: String,
                         destName: String,
                         faction: Faction,
                         action: MoveAction,
                         moveTypes: Set[PieceType],
                         maxPieces: Int,
                         mustKeep: Pieces,
                         opParams: Params): Pieces = {
    val us        = faction == US
    val arvn      = faction == ARVN
    val nva       = faction == NVA
    val easter    = faction == NVA && action == EasterTroops
    val vc        = faction == VC
    val usPSaigon = us && action == Patrol && destName == Saigon
    val arvnSPT   = arvn && (action == Sweep || action == Patrol || action == Transport)
    val origin    = game.getSpace(originName)
    val dest      = game.getSpace(destName)

    def nextMovePriority(priorities: List[MovePriority], params: MoveParams): Pieces = {
      val exhausted = params.maxCanMove == 0 || params.candidates.isEmpty
      priorities match {
        case Nil            => params.selected
        case _ if exhausted => params.selected
        case priority::rest => nextMovePriority(rest, priority(params))
      }
    }

    val movePriorities = if (dest.isLoC)
      List(
        movePriIf(easter,    MP_MoveMaxTroopsToLocAdjacentToSaigon),
        movePriIf(us,        MP_GetCoinFirepowerEqualEnemies),
        movePriIf(arvn,      MP_GetArvnFirepowerEqualEnemies),
        movePriIf(nva || vc, MP_GetGuerrillasGreaterThanCoinForces)
      ).flatten
    else  // City or Province
      List(
        movePriIf(us || arvn || vc,     MP_Get1PieceToDestination),
        movePriIf(us || arvn,           MP_Get3CoinCubesToDestination),
        movePriIf(us,                   MP_GetCoinFirepowerEqualToVulnerable),
        movePriIf(us,                   MP_GetUSForcesEqualToUnderground),
        movePriIf(usPSaigon || arvnSPT, MP_GetCoinControl),
        movePriIf(nva,                  MP_GetNvaControlWithCoinForces),
        movePriIf(nva || vc,            MP_Get3FactionGuerrillasForAvailableBase),
        movePriIf(nva || vc,            MP_Get1FactionGuerrillasToBase),
        movePriIf(nva,                  MP_GetNvaControlWithoutCoinForcesOrLaosCambodia)
      ).flatten

    val candidates = origin.pieces.only(moveTypes) - mustKeep
    if (candidates.isEmpty)
      candidates  // No pieces were found that can move
    else {
      val params     = MoveParams(origin, dest, faction, action, moveTypes, maxPieces, candidates)

      logMoved(params, s"$faction is selecting pieces to move from $originName to $destName", "")
      nextMovePriority(movePriorities, params)
    }
  }

  // Determine which pieces will move to the destination from the
  // given origin space using the Move Priorities Table.
  //
  // The caller is responsible for ensuring that the origin is not
  // a previous destination, and that the origin has the most moveable
  // pieces that can reach the destination.
  // This function does not verify any of that.
  //
  // If no pieces can be found to move to the destination then an empty
  // Pieces instance is returned.
  def movePiecesFromOneOrigin(originName: String,
                              destName: String,
                              faction: Faction,
                              action: MoveAction,
                              moveTypes: Set[PieceType],
                              maxPieces: Int,
                              params: Params): Pieces = {

    // First we determine which pieces to keep in the origin space
    val toKeep = selectPiecesToKeep(originName, destName, faction, action, moveTypes, params)
    // If this is an Air Lift we must limit the number of non USTroops moved to 4
    // Higher priority others come first in the list.
    val airLiftKeep = if (action == AirLift) {
      val otherTypes = List(ARVNTroops, Rangers_U, Irregulars_U, Rangers_A, Irregulars_A)
      val canMove    = (game.getSpace(originName).pieces - toKeep).explode(otherTypes)
      val numAllowd  = 4 - movedPieces.allPieces.totalOf(otherTypes)
      Pieces.fromTypes(canMove drop numAllowd)
    }
    else
      Pieces()
    selectPiecesToMove(originName, destName, faction, action, moveTypes, maxPieces, toKeep + airLiftKeep, params)
  }


  // Find the best space from which to move pieces to the given destination.
  // If no pieces can reach the destination, the return None.

  def selectMoveOrigin(
    faction: Faction,
    destName: String,
    action: MoveAction,
    moveTypes: Set[PieceType],
    params: Params,
    previousOrigins: Set[String]): Option[String] = {
    
    val candidateNames: Set[String] = action match {
      case EventMove(Some(origins)) => origins - destName
      case EventMove(None)          => spaceNames(game.spaces filterNot (_.name == destName)).toSet // Any space
      case Sweep                    => getSweepOrigins(destName)
      case Patrol                   => getPatrolOrigins(destName)
      case Transport                => getTransportOrigins(destName)
      case AirLift                  => Set.empty // See selectAirLiftOrigin
      case EasterTroops             => NVA_Bot.getNVAAdjacent(destName)
      case March if faction == NVA  => NVA_Bot.getNVAAdjacent(destName) filter params.march.canMarchFrom
      case March                    => getAdjacent(destName) filter params.march.canMarchFrom
    }

    val isOrigin = (sp: Space) => {
      notYetMoved(sp, moveTypes) > 0 &&
      !(moveDestinations.contains(sp.name) || previousOrigins(sp.name))
    }
    
    val candidates = spaces(candidateNames) filter isOrigin
    val priorities = List(
      new HighestScore[Space]( "Most Moveable Pieces", sp => notYetMoved(sp, moveTypes))
    )

    botDebug(s"\nSelect origin space with moveable: [${andList(moveTypes.toList)}]")
    botDebug(separator(char = '#'))
    if (candidates.nonEmpty) {
      Some(bestCandidate(candidates, priorities).name)
    }
    else {
      botDebug(s"No reachable spaces with moveable pieces")
      None
    }
  }

  // Use by some events to determine if it is possible to move any of the requested
  // piece types to the given destination.
  def canMoveTo(faction: Faction, destName: String, action: MoveAction, moveTypes: Set[PieceType], params: Params): Boolean = {

    def nextOrigin(previousOrigins: Set[String]): Boolean = {
      Bot.selectMoveOrigin(faction, destName, action, moveTypes, params, previousOrigins) match {
        case None => false  // We didn't find any origins with moveable pieces
        case Some(originName) =>
          if (movePiecesFromOneOrigin(originName, destName, faction, action, moveTypes, 1, params).nonEmpty)
            true
          else
            nextOrigin(previousOrigins + originName)
      }
    }
    nextOrigin(Set.empty)
  }

  // Called by events to move a given number of pieces to a specific destination
  // using the Bot Move Priorities.
  // If mandatory is false, then it will move up to the given number but will not move
  // pieces away from spaces as determined by the Move Priorities.
  // If madatory is true, then once it has moved pieces using the Move Priorities if we have
  // not moved the mandatory number of pieces, then we will select pieces from the space(s)
  // with the most pieces.
  //
  // Returns the pieces that moved.
  def doEventMoveTo(
    destName: String,
    faction: Faction,
    num: Int,
    mandatory: Boolean,
    moveTypes: Set[PieceType],
    onlyFrom: Option[Set[String]] = None): Pieces = {
    val params = Params()
    initTurnVariables()
    var destProduced = false
    
    movePiecesToDestinations(faction, EventMove(onlyFrom), moveTypes, false, params, maxPieces = num, maxDests = Some(1)) {
      
      (_, _, prohibited) => {
        if (destProduced || prohibited(destName))
          None
        else {
          destProduced = true
          Some(destName)
        }
      }
    }

    if (mandatory && movedPieces.allPieces.total < num) {
      def nextMandatoryMove(numRemaning: Int): Unit = if (numRemaning > 0) {
        val priorities = List(new HighestScore[Space]("Most moveable pieces", _.pieces.totalOf(moveTypes)))
        val origins = onlyFrom match {
          case Some(names) => spaces(names - destName) filter (sp => sp.pieces.has(moveTypes))
          case None        => game.spaces filter { sp => sp.name != destName && sp.pieces.has(moveTypes) }
        }

        if (origins.nonEmpty) {
          val sp = bestCandidate(origins, priorities)
          val piece = selectFriendlyToPlaceOrMove(sp.pieces.only(moveTypes), 1)
          movePieces(piece, sp.name, destName)
          movedPieces.add(destName, piece)
          nextMandatoryMove(numRemaning - 1)
        }
      }

      nextMandatoryMove(num - movedPieces.allPieces.total)
    }
    movedPieces.allPieces
  }

  def selectAirLiftOrigin(destName: String, currentOrigins: Set[String], alreadyTried: Set[String], addNewOrigin: Boolean, params: Params): Option[String] = {
    import params.airlift.allowedType
    // Can only move up to 4 ARVNTroop, Irregulars, Rangers
    val otherTypes = (ARVNTroops::Irregulars:::Rangers).toSet filter allowedType
    val usTroops  = Set(USTroops) filter allowedType
    val numOthers = 4 - movedPieces.allPieces.totalOf(otherTypes)
    val moveTypes = if (numOthers < 4)
      otherTypes ++ usTroops
    else
      usTroops

    val newcandidates = game.spaces filter { sp =>
      sp.name != destName                 &&
      !moveDestinations.contains(sp.name) &&
      !currentOrigins(sp.name)            &&
      sp.pieces.has(moveTypes)
    }
    val existingCandidates = spaces(currentOrigins) filter { sp =>
      sp.name != destName    &&
      !alreadyTried(sp.name) &&
      sp.pieces.has(moveTypes)
    }
    val candidates = if (addNewOrigin)
      newcandidates:::existingCandidates
    else
      existingCandidates

    val priorities = List(
      new HighestScore[Space]("Most Moveable Pieces",
      sp => {
        val others = sp.pieces.totalOf(otherTypes) min numOthers
        sp.pieces.totalOf(usTroops) + others
      })
    )

    botDebug(s"\nSelect Air Lift origin space with moveable: [${andList(moveTypes.toList)}]")
    botDebug(separator(char = '#'))
    if (candidates.nonEmpty) {
      Some(bestCandidate(candidates, priorities).name)
    }
    else {
      botDebug(s"No spaces with moveable pieces for AirLift")
      None
    }
  }

  def sweepAndAssaultSpace(name: String, faction: Faction, cubeTreatment: CubeTreatment): Unit = {
    initTurnVariables() // Always start with fresh turn state
    val params = Params(event = true, free = true, cubeTreatment = cubeTreatment, onlyIn = Some(Set(name)))
    if (faction == US)
      US_Bot.sweepOp(params)
    else
      ARVN_Bot.sweepOp(params, 6)
    performAssault(faction, name, params)
  }

  // This is used by Event #99 Masher/White Wing
  // We select the best target space for the event.
  // We return the name of the space along with the number
  // of insurgents that would be eliminated in the space.
  def bestSweepAssaultTarget(faction: Faction, cubeTreatment: CubeTreatment, candidates: List[String]): Option[(String, Int)] = {
    var targets = Vector.empty[(String, Int)]

    suspendLogging {
      for (destName <- candidates) {
        val saved = game
        initTurnVariables() // Always start with fresh turn state
        sweepAndAssaultSpace(destName, faction, cubeTreatment)
        val before = saved.getSpace(destName).pieces.totalOf(InsurgentPieces)
        val after  = game.getSpace(destName).pieces.totalOf(InsurgentPieces)
        if (before > after)
          targets = targets :+ (destName -> (before - after))
        game = saved
      }
    }

    targets.sortBy(-_._2).headOption
  }

  //  This function determines if we should attempt to find another
  //  origin space for the given move destination.
  //  The rules are faction specific and correspond to the
  //  'B' conditions at the bottom of the Move Priorities Table.
  def canSelectAnotherOrigin(faction: Faction, destName: String, cubeTreatment: CubeTreatment): Boolean = {
    val dest         = game.getSpace(destName)
    val numCoin      = dest.pieces.totalOf(CoinPieces)
    val numInsurgent = dest.pieces.totalOf(InsurgentPieces)

    (faction, dest.isLoC) match {
      case (US|ARVN, true)  => numCoin < numInsurgent
      case (NVA|VC,  true)  => numInsurgent <= numCoin
      case (US|ARVN, false) => !dest.coinControlled &&
                               coinFirepower(cubeTreatment)(dest) < vulnerableInsurgents(dest.pieces, false).total
      case (NVA,     false) => !dest.nvaControlled
      case (VC,      false) => dest.coinControlled || dest.nvaControlled
    }
  }

  // Type of function supplied to the movePiecesToDestinations() function that
  // is called when a new destination space is needed.
  //  (needActivation: Boolean, prohibited: Set[String]) => Option[String]
  // - The first parameter is true of the last candidate tried was successfully
  //   selected as a destination
  // - The second parameter is true if an activation roll is required
  // - The third paramter is the set of space names that cannot be used
  //   (because they have already been selected or were considered but
  //   not pieces were able to move there)
  type MoveDestGetter = (Boolean, Boolean, Set[String]) => Option[String]


  //  This function implements a move operation for the given faction.
  //  This list of destCandidates should be sorted such the the higher priority
  //  spaces come first.
  //  Note:  During Air Lift we must use the maxDests to also limit origin spaces
  //         During Air Lift we must ensure no more than 4 non US Troops are moved
  //  Returns true if any following moves would need an activation roll.
  def movePiecesToDestinations(
    faction: Faction,
    action: MoveAction,
    moveTypes: Set[PieceType],
    checkFirst: Boolean,   // True if we need to check activate before the first dest
    params: Params,
    maxPieces: Int = NO_LIMIT,  // Transport limits the number of pieces to 6 per destination
    maxDests: Option[Int] = None)(getNextDestination: MoveDestGetter): Boolean = {

    val maxDest = maxDests getOrElse NO_LIMIT
    var allOrigins = Set.empty[String] // For air lift we must limit origins+destinations
    var totalMoved = 0
    // ----------------------------------------
    def tryOrigin(destName: String, previousOrigins: Set[String]): Unit = if (totalMoved < maxPieces) {
      val canAddAirLiftOrigin = if (moveDestinations contains destName)
        allOrigins.size + moveDestinations.size < maxDest
      else
        allOrigins.size + moveDestinations.size < maxDest - 1 // This dest has not yet been added
      val origin = action match {
        // Only one origin allowed for Transport
        case Transport if previousOrigins.nonEmpty => None
        // Use the same (single) origin from previous Transport destinations
        case Transport if transportOrigin.nonEmpty => transportOrigin

        case AirLift => selectAirLiftOrigin(destName, allOrigins, previousOrigins, canAddAirLiftOrigin, params)

        // Select the top priority origin
        case _ => selectMoveOrigin(faction, destName, action, moveTypes, params, previousOrigins)
      }

      origin match {
        case None =>  // No more origin spaces, so we are finished

        case Some(originName) =>
          val toMove = movePiecesFromOneOrigin(originName, destName, faction, action, moveTypes, maxPieces - totalMoved, params)
          if (toMove.nonEmpty) {
            //  First time we move pieces to a dest log
            //  the selection of the destination space.
            if (!moveDestinations.contains(destName)) {
              log(s"\n$faction selects $destName as $action destination")
              log(separator())
            }
            movePieces(toMove, originName, destName)
            movedPieces.add(destName, toMove)
            totalMoved += toMove.total

            // Marching Guerrillas may have to activate
            if (action == March) {
              val dest         = game.getSpace(destName)
              val underground  = toMove.only(UndergroundGuerrillas)
              val numForces    = dest.pieces.totalOf(CoinForces)
              val mainForceBns = capabilityInPlay(MainForceBns_Unshaded)
              val tolerance    = if (mainForceBns) 1 else 3
              val activate     = underground.nonEmpty &&
                                 (dest.isLoC || dest.support > Neutral) &&
                                 (toMove.total + numForces) > tolerance

              if (activate) {
                val suffix = if (mainForceBns) s" [$MainForceBns_Unshaded]" else ""
                log(s"\nThe moving Guerrillas must activate$suffix")
                log(separator())
                revealPieces(destName, underground)

                if (momentumInPlay(Mo_Claymores)) {
                  log(s"\nMust remove Guerrilla that activated [Momentum: $Mo_Claymores")
                  log(separator())
                  removeToAvailable(destName, Pieces().set(1, underground.getTypes.head))
                }
                log()  // Add blank line separator
              }
            }
            //  The dest space can no longer be considered for
            //  a destination or for an origin
            addMoveDestination(destName)
            
            if (action == Transport)
              transportOrigin = Some(originName)
            allOrigins += originName
          }
          else
            botDebug(s"\nNo pieces moved from $originName to $destName")
          // Check to see if we should try another origin
          // for this destination space
          // For the EasterTroops action we are marching max Troops onto LoCs
          // adjacent to Saigon so we always select another Origin.
          if (action == EasterTroops || canSelectAnotherOrigin(faction, destName, params.cubeTreatment)) {
            botDebug(s"\n$faction Bot will select another origin space for destination: $destName")
            tryOrigin(destName, previousOrigins + originName)
          }
      }
    }

    // ----------------------------------------
    def tryDestination(lastWasSuccess: Boolean, needActivationRoll: Boolean, notReachable: Set[String]): Boolean = {
      val exhausted = if (action == AirLift)
        allOrigins.size + moveDestinations.size >= maxDest
      else
        moveDestinations.size >= maxDest
      
      if (!exhausted) {
        // For Air Lift don't allow moving into any origin space
        val prohibited = if (action == AirLift)
          moveDestinations.toSet ++ notReachable ++ allOrigins
        else
          moveDestinations.toSet ++ notReachable
        getNextDestination(lastWasSuccess, needActivationRoll, prohibited) match {
          case None => // No more destinations
            needActivationRoll  // Return so subsequent calls will know if activation is needed

          case Some(destName) =>
            botDebug(s"\n$faction Bot will attempt $action to $destName")
            loggingControlChanges {
              tryOrigin(destName, Set.empty)
            }
            val dest = game.getSpace(destName)

            // If no pieces could be found to move into the destination then
            // the destination will not have been added to the moveDestinations vector.
            // If we are doing a Sweep, check to see if we can sweep in place.
            if (action == Sweep && !moveDestinations.contains(destName) && sweepEffective(destName, faction, params.cubeTreatment)) {
              // If this is a sweep operation and no cubes were
              // able to move into the destination, but there are
              // sufficient existing cubes in the destination to
              // activate at least 1 underground guerrilla, then
              // add the destName to the moveDestinations
              // to allow the Sweep operation to perform a
              // Sweep in Place.
              addMoveDestination(destName)
              log(s"\n$faction selects $destName to Sweep in Place")
            }

            if (moveDestinations.contains(destName)) {
              pause()  // Pause after each successful move
              // Since the last move was successful we must determine if an
              // activation roll is necessary to continue.
              val needActivationRoll = faction match {
                case US     => false
                case ARVN   => action != Patrol
                case NVA|VC => !game.getSpace(destName).isLoC
              }
              tryDestination(true, needActivationRoll, notReachable)
            }
            else {
              // This destination was a no-op so no activation
              // roll is necesary before trying again.
              tryDestination(false, false, notReachable + destName)
            }
          }
        }
        else
          needActivationRoll   // Return so subsequent calls will know if activation is needed
    }

    //  Start by trying the first destination.
    tryDestination(false, false, Set.empty)
  }

  // ================================================================
  // US Specific code
  // ================================================================
  object US_Bot {

    def any2PlusPopNotAtActiveSupportWithCoinControlUsPieces: Boolean = {
      val result = game.nonLocSpaces exists { sp =>
        sp.population >= 2          &&
        sp.support != ActiveSupport &&
        sp.coinControlled           &&
        sp.pieces.has(USPieces)
      }
      logCheck(result, "Any 2+ Pop space not at Active Support has COIN Control and US pieces?")
    }

    def any2PopNotAtActiveSupportWithCoinControlUsTroops: Boolean = {
      val result = game.nonLocSpaces exists { sp =>
        sp.population == 2          &&
        sp.support != ActiveSupport &&
        sp.coinControlled           &&
        sp.pieces.has(USTroops)
      }
      logCheck(result, "Any 2-Pop space not at Active Support has COIN Control and US Troops?")
    }

    def twoOrMoreSpacesWithSupportAndUndergroundGuerrillas: Boolean = {
      val result = game.nonLocSpaces exists { sp =>
        sp.canHaveSupport           &&
        sp.support > Neutral        &&
        sp.pieces.has(UndergroundGuerrillas)
      }
      logCheck(result, "2 or more spaces with Support and Underground Guerrillas?")
    }

    // This condition is used by the US Bot to determine if it
    // would select an Assault Operation.
    // I have made a small adjustment by checking it the
    // General Lansdale Momentum event is in effect.
    // Otherwise, when General Lansdale is in effect the US
    // Bot tends to pass on every turn.
    def usPiecesWith4PlusNVATroopsOrVulnerableBase: Boolean = {
     val result = !momentumInPlay(Mo_GeneralLansdale) &&
      (game.spaces exists { sp =>
        sp.pieces.has(USPieces) &&
        (sp.pieces.totalOf(NVATroops) > 3 ||
         (sp.pieces.has(InsurgentBases) && !sp.pieces.has(UndergroundGuerrillas)))
      })
      logCheck(result, "US pieces in space with 4+ NVA Troops or vulnerable Base?")
    }

    def spaceWhereCoinFPLessThanNVATroopsAndUSBaseOrTroops: Boolean = {
      val result = game.spaces exists { sp =>
        coinFirepower(NormalTroops)(sp) < sp.pieces.totalOf(NVATroops) &&
        sp.pieces.has(USTroops::USBase::Nil)
      }
      logCheck(result, "Any space were COIN FP < NVA Troops where US Troops or US Base?")
    }
      
    def usPolicyIsLBJ: Boolean = {
      logCheck(game.usPolicy == USPolicy_LBJ, "US Policy is LBJ?")
    }
    
    // All routes from Can Tho to Hue blocked and Shaded M-48 not in effect
    def allLocRoutesCanTho_HueBlockedAndNoShadedM48: Boolean = {
      val result = !capabilityInPlay(M48Patton_Shaded) && !getPatrolDestinations(CanTho).contains(Hue)
      logCheck(result, "All LOC routes from Can Tho to Hue blocked and Shaded M48 not in effect?")
    }

    def usPiecesWithVulnerableEnemies: Boolean = {
      val result = game.spaces exists { sp =>
        sp.pieces.has(USPieces) && vulnerableInsurgents(sp.pieces, false).nonEmpty
      }
      logCheck(result, "US pieces in any space with vulnerable enemies?")
    }

    def allUSBasesinSouthWithUSTroopsNoNVATroops: Boolean = {
      val candidates = game.nonLocSpaces filter { sp =>
        isInSouthVietnam(sp.name) && sp.pieces.has(USBase)
      }

      val result = candidates forall (sp => sp.pieces.has(USTroops) && !sp.pieces.has(NVATroops))
      logCheck(result, "All US Bases in South Vietnam in spaces with US Troops and no NVA Troops?")
    }

    // US Spaces Priorities: Shift Toward Active Support
    def pickSpaceTowardActiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             CityProvinceNoActiveSupport),
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botDebug(s"\nUS Select space (Shift Toward Active Support): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }


    // US Spaces Priorities: Place Bases
    def pickSpacePlaceBases(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,               ProvinceWithoutCOINBase),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(true,               MostPopulation),
        filterIf(true,              IsHighlandProvince)
      ).flatten

      botDebug(s"\nUS Select space (Place Bases): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // US Spaces Priorities: Place Cubes or Special Forces
    def pickSpacePlaceCubesSpecialForces(troops: Boolean)(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,   ProvinceWithUSBaseAndCubes),
        filterIf(true,   CityProvinceNoActiveSupport),
        filterIf(troops, PoliceWithoutUSTroops),
        filterIf(true,   COINFirepowerLessThanVulnerable),
        filterIf(true,   MostPopulation),
        filterIf(true,   HasEnemyBase)
      ).flatten

      botDebug(s"\nUS Select space (Place Cubes or Special Forces): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // US Spaces Priorities: Sweep Destinations
    // useUSTroops should be false when performing actions with ARVN pieces
    def pickSpaceSweepDest(candidates: List[Space], usingUSTroops: Boolean = true): Space = {

      val priorities = List(
        filterIf(true,               ProvinceWithUSBaseAndCubes),
        filterIf(true,               USTroopsIrregLessUndergroundGuerrillasSupport),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(true,               CityProvinceNoActiveSupport),
        filterIf(usingUSTroops,      PoliceWithoutUSTroops),
        filterIf(true,               MostPopulation),
        filterIf(true,               HasEnemyBase)
      ).flatten

      botDebug(s"\nUS Select space (Sweep Destinations): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // US Spaces Priorities: Air Lift Destinations
    def pickSpaceAirLiftDest(candidates: List[Space]): Space = {

      val priorities = List(
        COINFirepowerLessThanVulnerable,
        HasUSBase,
        MostPopulation,
        HasEnemyBase
      )

      botDebug(s"\nUS Select space (Air Lift Destinations): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // US Pick space for: Air Strike
    def pickSpaceAirStrike(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,                  VulnerableBase),
        filterIf(true,                  ProvinceWithoutCOINBase),
        filterIf(true,                  COINFirepowerLessThanVulnerable),
        filterIf(game.isHuman(VC),      MostTotalOpposition),
        filterIf(true,                  IsHighlandProvince)
      ).flatten

      botDebug(s"\nUS Select space (Air Strike): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // US Pick space for: Remove or Replace
    def pickSpaceRemoveReplace(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,                  VulnerableBase),
        filterIf(true,                  USTroopsIrregLessUndergroundGuerrillasSupport),
        filterIf(true,                  COINFirepowerLessThanVulnerable),
        filterIf(true,                  HasUSBase),
        filterIf(true,                  MostPopulation),
        filterIf(true,                  LocWithEnemyPieces)
      ).flatten

      botDebug(s"\nUS Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // Part of the Commitment phase of a Coup Round.
    // All US Troops currently in the casualites box
    // are place on the map in either:
    // COIN controlled spaces, LoCs, or Saigon.
    // We place each Troop cube one at a time using
    // the "Place Cubes or Special Forces" priorities.
    def placeUSCasualtyTroopsOnMap(coinControlled: Set[String]): Unit = {
      def placeNextTroop(numRemaining: Int): Unit = if (numRemaining > 0 ) {
        val candidates = game.spaces filter { sp =>
          isInSouthVietnam(sp.name) &&
          (sp.name == Saigon ||
           sp.isLoC          ||
           coinControlled(sp.name))
        }
        val sp = pickSpacePlaceCubesSpecialForces(troops = true)(candidates)
        moveCasualtiesToMap(Pieces(usTroops = 1), sp.name)
        placeNextTroop(numRemaining - 1)
      }

      placeNextTroop(game.casualties.totalOf(USTroops))
    }

    // Part of the Commitment phase of a Coup Round.
    // 1. Move bases from Cities leaving 1 base in Saigon.
    // 2. Place or withdraw US pieces per current US Policy
    //    o JFK   - Place bases in S. Vietnam, the place/withdraw troops until 15 are available
    //    o LBJ   - Place bases in S. Vietnam, the place/withdraw troops until 5 are available
    //    o NIxon - Withdraw bases from S. Vietnam leaving 1 base in Saigon, then
    //              place/withdraw troops until 25 are available
    // * Withdraw/move bases from spaces with the fewest enemy pieces
    // * Withdraw troops from spaces outside Saigon:
    //   1. Fewest troops and no COIN base
    //   2. Spaces with least population
    def moveUSCommitmentPieces(maxTroopsAllowed: Int, coinControlled: Set[String]): Unit = {
      val AVAILABLE = "available"
      val movedPieces = new MovingGroups()
      def basesMoved  = movedPieces.allPieces.totalOf(USBase)
      def troopsMoved = movedPieces.allPieces.totalOf(USTroops)
      def avail       = game.availablePieces
      val validSpace = (sp: Space) =>
        sp.name == Saigon ||
        sp.isLoC          ||
        coinControlled(sp.name)
      val canMovePieceAway = (sp: Space) => {
        val numCoin  = sp.pieces.totalOf(CoinPieces)
        val numVC    = sp.pieces.totalOf(VCPieces)
        val numNVA   = sp.pieces.totalOf(NVAPieces)
        val numEnemy = numVC + numNVA

        if (numCoin > numEnemy)
          numCoin > numEnemy + 1
        else if (numCoin + numVC >= numNVA)
          numCoin + numVC > numNVA
        else
          true
      }
      val baseLeavePriorities = List(
        new LowestScore[Space]("Fewest enemy pieces", _.pieces.totalOf(InsurgentPieces))
      )
      val fewestTroopsPriorities = List(
        new LowestScore[Space]("Fewest US Troops", _.pieces.totalOf(USTroops))
      )
      val leastPopPriorities = List(
        new BooleanPriority[Space]("LoC", _.isLoC),  // LoCs have no population so pick them first.
        new LowestScore[Space]("Least Population", sp => if (sp.isLoC) NO_SCORE else sp.population)
      )
      val numBases  = (sp: Space) => (sp.pieces - movedPieces(sp.name)).totalOf(USBase)
      val numTroops = (sp: Space) => (sp.pieces - movedPieces(sp.name)).totalOf(USTroops)

      def moveBases(): Unit = if (basesMoved < 2) {
        val originCandidates = game.spaces filter { sp =>
          val baseNeeded = if (sp.name == Saigon) 2 else 1
          validSpace(sp)       &&
          sp.isCity            &&
          canMovePieceAway(sp) &&
          numBases(sp) >= baseNeeded
        }

        if (originCandidates.nonEmpty) {
          val origin = bestCandidate(originCandidates, baseLeavePriorities)
          val destCandidates = game.spaces filter { sp =>
            validSpace(sp)            &&
            sp.name != origin.name    &&
            sp.isProvince             &&
            sp.totalBases < 2         &&
            isInSouthVietnam(sp.name) &&
            numTroops(sp) > 0
          }

          if (destCandidates.nonEmpty) {
            val dest = pickSpacePlaceBases(destCandidates)
            val base = Pieces(usBases = 1)
            movePieces(base, origin.name, dest.name)
            movedPieces.add(dest.name, base)
            moveBases()
          }
        }
      }

      def placeBases(): Unit = if (basesMoved < 2 && avail.has(USBase)) {
        val destCandidates = game.spaces filter { sp =>
          validSpace(sp)     &&
          sp.totalBases < 2  &&
          isInSouthVietnam(sp.name)
        }

        if (destCandidates.nonEmpty) {
          val dest = pickSpacePlaceBases(destCandidates)
          val base = Pieces(usBases = 1)
          placePieces(dest.name, base)
          movedPieces.add(dest.name, base)
          placeBases()
        }
      }

      def withdrawBases(): Unit = if (basesMoved < 2) {
        val originCandidates = game.spaces filter { sp =>
          val baseNeeded = if (sp.name == Saigon) 2 else 1
          validSpace(sp)       &&
          numBases(sp) >= baseNeeded
        }

        if (originCandidates.nonEmpty) {
          val origin = bestCandidate(originCandidates, baseLeavePriorities)
          val base = Pieces(usBases = 1)
          removeToAvailable(origin.name, base)
          movedPieces.add(AVAILABLE, base)
          withdrawBases()
        }
      }

      def placeTroops(targetNum: Int): Unit = {
        if (troopsMoved < maxTroopsAllowed && avail.totalOf(USTroops) > targetNum) {
          val destCandidates = game.spaces filter validSpace
          val dest = pickSpacePlaceCubesSpecialForces(troops = true)(destCandidates)
          val troop = Pieces(usTroops = 1)
          placePieces(dest.name, troop)
          movedPieces.add(dest.name, troop)
          placeTroops(targetNum)
        }
      }

      def withdrawTroops(targetNum: Int): Unit = {
        if (troopsMoved < maxTroopsAllowed && avail.totalOf(USTroops) < targetNum) {
          val originCandidates1 = game.spaces filter { sp =>
            validSpace(sp)            &&
            sp.name != Saigon         &&
            !sp.pieces.has(CoinBases) &&
            numTroops(sp) > 0
          }
          val originCandidates2 = game.spaces filter { sp => validSpace(sp) && numTroops(sp) > 0 }

          if (originCandidates1.nonEmpty || originCandidates2.nonEmpty) {
            val origin = if (originCandidates1.nonEmpty)
              bestCandidate(originCandidates1, fewestTroopsPriorities)
            else
              bestCandidate(originCandidates2, leastPopPriorities)
            val troop = Pieces(usTroops = 1)
            removeToAvailable(origin.name, troop)
            movedPieces.add(AVAILABLE, troop)
            withdrawTroops(targetNum)
          }
        }
      }

      def placeOrWithdrawTroops(targetNum: Int): Unit = {
        val availTroops = avail.totalOf(USTroops)
        if (availTroops > targetNum)
          placeTroops(targetNum)
        else if (availTroops < targetNum)
          withdrawTroops(targetNum)
      }

      val troopTarget = game.usPolicy match {
        case USPolicy_JFK => 15
        case USPolicy_LBJ =>  5
        case _ /* Nixon*/ => 25
      }

      moveBases()
      if (game.usPolicy == USPolicy_Nixon)
        withdrawBases()
      else
        placeBases()

      placeOrWithdrawTroops(troopTarget)
    }

    def moveOutOfPlayToCities(maxNum: Int): Unit = {

      // Place one piece at a time using the
      // "Place Base"/"Place cubes or Special Forces/" priorities
      def placeNext(toPlace: Pieces): Unit = if (toPlace.nonEmpty) {
        val piece = selectFriendlyToPlaceOrMove(toPlace, 1)
        val sp = if (piece.has(USBase))
          pickSpacePlaceBases(spaces(Cities))
        else
          pickSpacePlaceCubesSpecialForces(troops = piece.has(USTroops))(spaces(Cities))

        moveOutOfPlayToMap(piece, sp.name)
        placeNext(toPlace - piece)
      }

      val outOfPlay = game.outOfPlay.only(USPieces)
      val num = outOfPlay.total min maxNum
      val toPlace = selectFriendlyToPlaceOrMove(outOfPlay, num)

      placeNext(toPlace)
    }

    //  -------------------------------------------------------------
    //  Implement the US Train instructions from the US Trung cards.
    //  1. IF (JFK/LBJ policy) Place ARVN cubes in 1 space
    //  2. Place all available special forces
    //  3. Place ARVN Cubes
    //  - If unshaded Combined Action Platoons, get 1 ARVN Police to a space
    //    with US Troops.  If none available, take on from the space with the
    //    most Police that would not lose COIN control.
    //  - IF Patronage >= 17, Select Saigon and transfer Patronage to ARVN resources.
    //    Otherwise
    //    Remove terror and shift toward Active Suport for max 1d3
    //
    //  CombActionPlatoons_Unshaded
    //    US Training places (relocates) 1 added ARVN Police cube
    //    in one of the Training spaces with US Troops.
    //    Does not cost any added resources (if no Base/cubes placed in that space)
    //  CORDS_Unshaded
    //    US Training may pacify in 2 Training spaces (cost is unchanged)
    //  CORDS_Shaded
    //    US Training may pacify only to PassiveSupport (Not ActiveSupport)
    //  RVN_Leader_NguyenCaoKy - US/ARVN pacification costs 4 resources per Terror/Level
    def trainOp(params: Params, actNum: Int): Option[CoinOp] = {
      val maxTrain      = params.maxSpaces getOrElse NO_LIMIT
      var needArvnCheck = false
      var arvnExhausted = false   // Until we fail an activation or run out of resources

      def checkARVNActivation: Boolean = if (needArvnCheck == false)
        true
      else if (arvnExhausted)
        false
      else {
        val passed = makeActivationRoll(ARVN, actNum)
        arvnExhausted = !passed
        passed
      }
      def canAffordARVN = params.free || !game.trackResources(ARVN) || (game.arvnResources - 3) >= game.econ
      def canPlaceARVN = canAffordARVN && (!needArvnCheck || checkARVNActivation)
      def canTrain(arvn: Boolean) = trainingSpaces.size < maxTrain && (!arvn || canPlaceARVN)
      def prohibited(sp: Space) = !params.spaceAllowed(sp.name) || trainingSpaces(sp.name) || adviseSpaces(sp.name)
      val irregCandidate = (sp: Space) => !prohibited(sp) && sp.pieces.has(USPieces)
      val arvnCandidate  = (sp: Space) => !prohibited(sp) && sp.pieces.has(USBase)

      def trainToPlaceCubes(once: Boolean): Unit = {
        val candidates = game.nonLocSpaces filter arvnCandidate
        if (game.availablePieces.has(ARVNCubes) && candidates.nonEmpty && canTrain(true)) {
          val sp      = pickSpacePlaceCubesSpecialForces(troops = true)(candidates)
          val avail   = game.availablePieces.only(ARVNCubes)
          val num     = 6 min avail.total
          val toPlace = selectFriendlyToPlaceOrMove(avail, num)

          log(s"\n$US selects ${sp.name} for Train")
          log(separator())
          if (game.trackResources(ARVN))
            decreaseResources(ARVN, 3)
          placePieces(sp.name, toPlace)
          needArvnCheck = true
          trainingSpaces += sp.name
          pause()
          if (!once)
            trainToPlaceCubes(once = false)
        }
      }

      def trainToPlaceRangers(): Unit = {
        val candidates = game.nonLocSpaces filter arvnCandidate
        if (game.availablePieces.has(Rangers) && candidates.nonEmpty && canTrain(true)) {
          val sp      = pickSpacePlaceCubesSpecialForces(troops = false)(candidates)
          val avail   = game.availablePieces.only(Rangers)
          val num     = 2 min avail.total
          val toPlace = selectFriendlyToPlaceOrMove(avail, num)

          log(s"\n$US selects ${sp.name} for Train")
          log(separator())
          if (game.trackResources(ARVN))
            decreaseResources(ARVN, 3)
          placePieces(sp.name, toPlace)
          needArvnCheck = true
          trainingSpaces += sp.name
          pause()
          trainToPlaceRangers()
        }
      }

      def trainToPlaceIrregulars(): Unit = {
        val candidates = game.nonLocSpaces filter irregCandidate
        if (canTrain(false) && game.availablePieces.has(Irregulars) && candidates.nonEmpty) {
          val sp      = pickSpacePlaceCubesSpecialForces(troops = false)(candidates)
          val avail   = game.availablePieces.only(Irregulars)
          val num     = 2 min avail.total
          val toPlace = selectFriendlyToPlaceOrMove(avail, num)

          log(s"\n$US selects ${sp.name} for Train")
          log(separator())
          placePieces(sp.name, toPlace)
          trainingSpaces += sp.name
          pause()
          trainToPlaceIrregulars()
        }
      }

      def checkCombinesActionPlatoons(): Unit = {
        if (capabilityInPlay(CombActionPlatoons_Unshaded)) {
          val placePriorities = List( new HighestScore[Space]("Most Population", _.population) )
          val placeCandidates = game.nonLocSpaces filter { sp =>
            !sp.pieces.has(ARVNPolice) &&
            sp.pieces.has(USTroops)    &&
            sp.support < PassiveSupport
          }
          val dest = if (placeCandidates.nonEmpty)
            Some(bestCandidate(placeCandidates, placePriorities))
          else
            None
          val (placePolice, toAvail) = if (game.availablePieces.has(ARVNPolice))
            (true, None)
          else {
            // Spaces with ARVN Police that would not lose control
            // by removing one police cube
            val removePriorities = List( new HighestScore[Space]("Most Police", _.pieces.totalOf(ARVNPolice)) )
            val removeCandidates = game.spaces filter { sp =>
              sp.pieces.has(ARVNPolice) &&
              (!sp.coinControlled || sp.pieces.totalOf(CoinPieces) - 1 > sp.pieces.totalOf(InsurgentPieces))
            }
            if (removeCandidates.isEmpty)
              (false, None)
            else
              (true, Some(bestCandidate(removeCandidates, removePriorities)))
          }

          if (dest.nonEmpty && placePolice) {
            log(s"US places 1 ARVN Police with US Troops [$CombActionPlatoons_Unshaded]")
            log(separator())
            toAvail foreach { sp =>
              removeToAvailable(sp.name, Pieces(arvnPolice = 1))
            }
            placePieces(dest.get.name, Pieces().set(1, ARVNPolice))
          }
        }
      }

      def pacifyOneSpace(): Unit = {
        val baseRate   = if (isRVNLeader(RVN_Leader_NguyenCaoKy)) 4 else 3
        val costEach   = if (game.trackResources(ARVN)) baseRate else 0
        val candidates = game.nonLocSpaces filter { sp =>
          !adviseSpaces(sp.name)  &&
          (trainingSpaces(sp.name) || canTrain(false)) &&
          sp.coinControlled       &&
          sp.pieces.has(USPieces) &&
          sp.support < ActiveSupport
        }

        if (candidates.nonEmpty && game.arvnResources >= costEach) {
          val sp = pickSpaceTowardActiveSupport(candidates)
          // val numCanPay = if (costEach == 0) NO_LIMIT else game.arvnResources / costEach
          // val maxShift  = (ActiveSupport.value - sp.support.value) min 2
          // val maxPacify = ((sp.terror + maxShift) min d3) min numCanPay
          // val numTerror = maxPacify min sp.terror
          // val numShift  = (maxPacify - numTerror) min maxShift

          if (!trainingSpaces(sp.name)) {
            log(s"\n$US selects ${sp.name} for Train")
            log(separator())
            trainingSpaces += sp.name
          }
          pacifySpace(sp.name, US, coupRound = false)
        }
      }

      if (!params.event)
        logOpChoice(US, Train)
      if (game.usPolicy == USPolicy_JFK || game.usPolicy == USPolicy_LBJ)
        trainToPlaceCubes(once = true)

      trainToPlaceRangers()
      trainToPlaceIrregulars()
      trainToPlaceCubes(once = false)
      checkCombinesActionPlatoons()
      if (game.patronage >= 17 && !adviseSpaces(Saigon) && (trainingSpaces(Saigon) || canTrain(false))) {
        log(s"\nUS selects Saigon to transfer Patronage to ARVN resources")
        log(separator())
        decreasePatronage(3)
        increaseResources(ARVN, 3)
        trainingSpaces += Saigon
      }
      else
        pacifyOneSpace()

      if (trainingSpaces.nonEmpty) {
        Some(Train)
      }
      else {
        logNoOp(US, Train)
        None
      }
    }

    //  -------------------------------------------------------------
    //  Implement the US Patrol instructions from the US Trung cards.
    //  1. Get COIN control in Saigon.
    //  2. Select all LoCs with enemy pieces.
    //  *  Assault using Remove Priorities.  First to unblock the route
    //     from Can Tho to Hue if possible.  Otherwise at random.
    //  -> US will not Patrol if M48Patton_Shaded is in effect.
    //
    //  M48Patton_Shaded - After US/ARVN patrol NVA removes up to 2 cubes that moved (US to casualties)
    //  Mo_BodyCount     - Cost=0 AND +3 Aid per guerrilla removed
    def patrolOp(params: Params): Option[CoinOp] = {
      val maxPatrol = params.maxSpaces getOrElse NO_LIMIT
      if (!capabilityInPlay(M48Patton_Shaded)) {
        // Select a LoC Patrol destination candidate
        // ARVN Patrol never needs an activation roll
        val saigonCandidate = (_: Boolean, _: Boolean, prohibited: Set[String]) => {
          if (!game.getSpace(Saigon).coinControlled) Some(Saigon) else None
        }

        val locPatrolCandidate = (_: Boolean, _: Boolean, prohibited: Set[String]) => {
          val candidates = game.locSpaces filter { sp =>
            !prohibited(sp.name) &&
            sp.pieces.has(InsurgentForces)
          }

          if (candidates.nonEmpty)
            Some(shuffle(candidates).head.name)
          else
            None
        }

        if (!params.event)
          logOpChoice(US, Patrol)
        movePiecesToDestinations(US, Patrol, Set(USTroops), false, params, maxDests = Some(1))(saigonCandidate)
        if (moveDestinations.size < maxPatrol)
          movePiecesToDestinations(US, Patrol, Set(USTroops), false, params)(locPatrolCandidate)
        val activatedSomething = activateGuerrillasOnLOCs(US)

        // Add an assault on one LoC.  If this was a LimOp then the LoC must
        // be the selected destination.  If none was selected then we can pick any one.
        val assaultLoC = if (params.limOpOnly) {
          moveDestinations.headOption filter { name =>
            val sp = game.getSpace(name)
            
            sp.isLoC &&
            assaultEffective(US, params.cubeTreatment, params.vulnerableTunnels)(sp)
          }          
        }
        else
          pickPatrolAssaultLoc(US, params.cubeTreatment, params.vulnerableTunnels)

        assaultLoC foreach { name =>
          performAssault(US, name, Params(free = true))
        }
        
        if (moveDestinations.nonEmpty || activatedSomething || assaultLoC.nonEmpty)
          Some(Patrol)
        else {
          log("\nSkipping US Patrol as it would not be effective")
          log(separator())
          None
        }
      }
      else
        None
    }

    //  -------------------------------------------------------------
    //  Implement the US Sweep instructions from the US Trung cards.
    //  1. Use Move Priorities
    //  2. Sweep in place.
    //  *  If Shaded Booby Traps then sweep in max 2 spaces
    //
    //  Cobras_Unshaded - 2 US/ARVN sweep spaces each remove 1 Active untunneled enemy
    //                               (troops then  guerrillas then bases)
    //  CombActionPlatoons_Shaded - US may select max 2 spaces per sweep
    //  BoobyTraps_Shaded - Each sweep space, VC afterward removes 1 sweeping troop on
    //                      a roll 1-3 (US to casualties)
    //
    //  Sweep not allowed in Monsoon.
    def sweepOp(params: Params): Option[CoinOp] = {
      val cubeTypes = sweepCubeTypes(US, params.cubeTreatment)

      if (!params.event && game.inMonsoon)
        None
      else {
        // If Shaded Booby Traps then limit spaces to 2 unless this is a limOp
        val maxTraps: Option[Int]  = if (capabilityInPlay(BoobyTraps_Shaded)) Some(2) else None
        val maxSweep = (maxTraps.toList ::: params.maxSpaces.toList).sorted.headOption

        val nextSweepCandidate = (_: Boolean, _: Boolean, prohibited: Set[String]) => {
          val candidates = game.nonLocSpaces filter { sp =>
            params.spaceAllowed(sp.name) &&
            !sp.isNorthVietnam &&
            !prohibited(sp.name)
          }

          if (candidates.nonEmpty)
            Some(pickSpaceSweepDest(candidates).name)
          else
            None
        }

        if (!params.event)
          logOpChoice(US, Sweep)

        movePiecesToDestinations(US, Sweep, cubeTypes , false, params, maxDests = maxSweep)(nextSweepCandidate)
        val maxCobras = 2  // Unshaded cobras can be used in up to 2 spaces
        var numCobras = 0
        if (moveDestinations.nonEmpty) {
          // Activate guerrillas in each sweep destination
          for (name <- moveDestinations) {
            val activated = activateGuerrillasForSweep(name, US, params.cubeTreatment)
            val traps     = checkShadedBoobyTraps(name, US)
            val cobras    = numCobras < maxCobras && checkUnshadedCobras(name)
            if (cobras)
              numCobras += 1
            if (activated || traps || cobras)
              pause()
          }

          // Finally pay for the operation of applicable
          Some(Sweep)
        }
        else {
          logNoOp(US, Sweep)
          None
        }
      }
    }

    //  -------------------------------------------------------------
    //  Implement the US Assault instructions from the US Trung cards.
    //  Each Trung card has two Assault sections.
    //  After the top section, we call the supplied function to perform
    //  a special activity if allowed.
    //  Top section:
    //  1. Assault with US Troops in all spaces where ALL vulnerable enemies
    //     would be removed.
    //
    //  Bottom section:
    //  1. Assault adding ARVN to remove mosts enemies. (1 space)
    //  2. Assault in all spaces with US Troops
    //
    //  Mo_GeneralLansdale        - Assault prohibited
    //  Abrams_Unshaded           - 1 US assault space may remove 1 non-Tunnel base first (not last)
    //  Abrams_Shaded             - US may select max 2 spaces per Assault
    //  M48Patton_Unshaded        - In two non-Lowland US assault spaces, remove 2 extra enemy pieces
    //  Cobras_Shaded             - Each US assault space, 1 US Troop to Casualties on die roll of 1-3
    //  SearchAndDestroy_Unshaded - Each US assault space may remove 1 underground guerrilla
    //  SearchAndDestroy_Shaded   - Each US and ARVN assault Province shifts support one level toward Active Opposition
    //  Mo_BodyCount              - Cost=0 AND +3 Aid per guerrilla removed
    def assaultOp(params: Params)(doSpecialActivity: () => Boolean): Option[(CoinOp, Boolean)] = {
      val NVATargets = Set(NVATroops, NVABase)
      if (generalLandsdale(US))
        None
      else {
        var assaultSpaces = Set.empty[String]
        val paramsMax     = params.maxSpaces getOrElse NO_LIMIT
        val capMax        = if (capabilityInPlay(Abrams_Shaded)) 2
                       else if (capabilityInPlay(Cobras_Shaded)) 2
                       else NO_LIMIT
        val maxAssault    = paramsMax min capMax

        val wouldKillNVATroopsOrBase = (sp: Space) => {
          !assaultSpaces(sp.name)  && {
            val result = assaultResult(US, params.cubeTreatment, params.vulnerableTunnels)(sp)
            sp.pieces.totalOf(NVATargets) > result.pieces.totalOf(NVATargets)
          }
        }


        val assaultRemovesAllVulnerable = (sp: Space) =>
          !assaultSpaces(sp.name)  &&
          assaultEffective(US, params.cubeTreatment, params.vulnerableTunnels)(sp) &&
          noVulnerableInsurgents(assaultResult(US, params.cubeTreatment, params.vulnerableTunnels)(sp)) &&
          (!capabilityInPlay(SearchAndDestroy_Shaded) || wouldKillNVATroopsOrBase(sp))


        val assaultWithArvn = (sp: Space) => {
          if (!assaultSpaces(sp.name) && assaultEffective(US, params.cubeTreatment, params.vulnerableTunnels, m48PattonCount)(sp)) {
            val afterUS = assaultResult(US, params.cubeTreatment, params.vulnerableTunnels)(sp)
            if (assaultEffective(ARVN, params.cubeTreatment, params.vulnerableTunnels)(afterUS)) {
              val afterARVN = assaultResult(ARVN, params.cubeTreatment, params.vulnerableTunnels)(afterUS)
              val deadNVA   = sp.pieces.totalOf(NVATargets) > afterARVN.pieces.totalOf(NVATargets)
              capabilityInPlay(SearchAndDestroy_Shaded) == false || deadNVA
            }
            else
              false
          }
          else
            false
        }

        val arvnAssaultPriorities = List(
          new HighestScore[Space](
            "ARVN Assault removes most enemy",
            (sp: Space) => {
              val afterUS   = assaultResult(US, params.cubeTreatment, params.vulnerableTunnels)(sp)
              val afterARVN = assaultResult(ARVN, params.cubeTreatment, params.vulnerableTunnels)(afterUS)
              // Return number of pieces removed by ARVN assault
              afterUS.pieces.totalOf(InsurgentPieces) - afterARVN.pieces.totalOf(InsurgentPieces)
          })
        )

        val assaultWithTroops = (sp: Space) =>
          !assaultSpaces(sp.name)  &&
          assaultEffective(US, params.cubeTreatment, params.vulnerableTunnels, m48PattonCount)(sp) &&
          (!capabilityInPlay(SearchAndDestroy_Shaded) || wouldKillNVATroopsOrBase(sp))


        def nextAssault(selector: (Space) => Boolean, addARVN: Boolean): Unit = {
          //  We must reassess candidates each time because the m48PattonCount can change
          val candidates = game.spaces filter selector
          
          if (assaultSpaces.size < maxAssault && candidates.nonEmpty) {
            // Different priorities when adding an ARVN assault
            val sp = if (addARVN)
              bestCandidate(candidates, arvnAssaultPriorities)
            else
              pickSpaceRemoveReplace(candidates)

            if (!params.event && assaultSpaces.isEmpty)
              logOpChoice(US, Assault)
            
            performAssault(US, sp.name, params)
            pause()
            
            // performAssault will reduce the ARVN resources to pay for the Assault
            if (addARVN) {
              log(s"\nUS adds a follow up ARVN assault in ${sp.name}")
              performAssault(ARVN, sp.name, params)
              pause()
            }
            assaultSpaces += sp.name
            
            //  The Bot will only assault one space with an added ARVN assault
            if (!addARVN)
              nextAssault(selector, false)
          }
        }

        //  First candidates where all vulnerable enemies would be removed
        nextAssault(assaultRemovesAllVulnerable, false)

        val didSpecial = params.addSpecialActivity && doSpecialActivity()

        // Add an attack with add ARVN assault
        val canAffordARVN = momentumInPlay(Mo_BodyCount) ||  // Make ARVN Assault free
                            params.free                  ||
                            !game.trackResources(ARVN)   ||
                            (game.arvnResources - 3) >= game.econ
        if (params.cubeTreatment == NormalTroops && canAffordARVN)
          nextAssault(assaultWithArvn, true)

        // Assault in all remaining spaces with US Troops where effective
        nextAssault(assaultWithTroops, false)

        if (assaultSpaces.nonEmpty || didSpecial)
          Some(Assault -> didSpecial)
        else {
          logNoOp(US, Assault)
          None
        }
      }
    }

    //  -------------------------------------------------------------
    //  Implement the US Advise instructions from the US Trung cards.
    //  1. Remove an enemy base
    //  2. If Underground Guerrillas at Support Sweep in Place with ARVN
    //     to activate the most.
    //  3. Remove enemies with special forces
    //  4. Assault with ARVN using Remove
    //  *  If ARVN is human, Add +6 Aid
    //
    // Mo_TyphoonKate - prohibits air lift, transport, and bombard, all other special activities are max 1 space
    def adviseActivity(params: Params): Boolean = {
      val maxAdvise = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                      else if (momentumInPlay(Mo_TyphoonKate)) 1 else 2
      val UndergroundSpecialForces = Set(Irregulars_U, Rangers_U)
      def canAdvise = adviseSpaces.size < maxAdvise
      def prohibited(sp: Space) = !params.spaceAllowed(sp.name) || adviseSpaces(sp.name) || trainingSpaces(sp.name)
      def hasUndergroundForces(sp: Space) = sp.pieces.has(UndergroundSpecialForces)
      def specialForcesWouldRemoveBase(sp: Space) =
        hasUndergroundForces(sp) &&
        sp.pieces.has(InsurgentNonTunnels) &&
        sp.pieces.totalOf(InsurgentForces) < 2

      def useSpecialForces(sp: Space): Unit = {
        log(s"\nAdvise in ${sp.name} using Special Forces")
        log(separator())
        val forceType    = if (sp.pieces.has(Rangers_U)) Rangers_U else Irregulars_U
        val specialForce = Pieces().set(1, forceType)
        revealPieces(sp.name, specialForce)
        val deadForces = selectEnemyRemoveReplaceActivate(sp.pieces.only(InsurgentForces), 2)
        val deadBases  = selectEnemyRemoveReplaceActivate(sp.pieces.only(InsurgentNonTunnels), (2 - deadForces.total) max 0)
        removeToAvailable(sp.name, deadForces + deadBases)
      }

      def arvnAssault(sp: Space): Unit = {
        log(s"\nAdvise in ${sp.name} using ARVN Assault")
        log(separator())
        performAssault(ARVN, sp.name, Params(free = true))
      }

      val canRemoveBaseWithARVN = (sp: Space) =>
        !prohibited(sp) &&
        assaultWouldRemoveBase(ARVN, params.cubeTreatment, params.vulnerableTunnels)(sp)

      val canRemoveBaseWithSpecialForces = (sp: Space) => !prohibited(sp) && specialForcesWouldRemoveBase(sp)

      val canArvnSweep = (sp: Space) =>
        !prohibited(sp) &&
        sp.support > Neutral &&
        sp.sweepActivations(ARVN, params.cubeTreatment) > 0

      val canRemoveEnemies = (sp: Space) =>
        hasUndergroundForces(sp) &&
        sp.pieces.has(InsurgentForces:::InsurgentNonTunnels)

      def removeBases(): Unit =  {
        val arvnCandidates = game.nonLocSpaces filter canRemoveBaseWithARVN
        val sfCandidates   = game.nonLocSpaces filter canRemoveBaseWithSpecialForces
        if (canAdvise && (arvnCandidates.nonEmpty || sfCandidates.nonEmpty)) {
          // Favor ARVN over Special Forces to get the +6 Aid
          val candidates = if (arvnCandidates.nonEmpty) arvnCandidates else sfCandidates
          val sp = pickSpaceRemoveReplace(candidates)

          if (!params.event && adviseSpaces.isEmpty)
            logSAChoice(US, Advise)
          if (assaultWouldRemoveBase(ARVN, params.cubeTreatment, params.vulnerableTunnels)(sp))
            arvnAssault(sp)
          else
            useSpecialForces(sp)
          adviseSpaces += sp.name
          removeBases()
        }
      }

      def doArvnSweeps(): Unit = if (params.event || !game.inMonsoon) {
        val candidates = game.nonLocSpaces filter canArvnSweep
        if (canAdvise && candidates.nonEmpty) {
          val sp = pickSpaceWithMostSweepActivations(ARVN, params.cubeTreatment)(candidates)

          if (!params.event && adviseSpaces.isEmpty)
            logSAChoice(US, Advise)
          log(s"\nAdvise in ${sp.name} using ARVN Sweep in place")
          log(separator())
          activateGuerrillasForSweep(sp.name, ARVN, params.cubeTreatment, logHeading = false)
          pause()
          adviseSpaces += sp.name
          doArvnSweeps()
        }
      }

      def removeEnemies(): Unit = {
        val candidates = game.spaces filter canRemoveEnemies
        if (canAdvise && candidates.nonEmpty) {
          val sp = pickSpaceRemoveReplace(candidates)

          if (!params.event && adviseSpaces.isEmpty)
            logSAChoice(US, Advise)
          useSpecialForces(sp)
          pause()
          adviseSpaces += sp.name
          removeEnemies()
        }
      }

      def doArvnAssaults(): Unit = {
        val candidates = game.spaces filter assaultEffective(ARVN, params.cubeTreatment, params.vulnerableTunnels)
        if (canAdvise && candidates.nonEmpty) {
          val sp = pickSpaceRemoveReplace(candidates)

          if (!params.event && adviseSpaces.isEmpty)
            logSAChoice(US, Advise)
          arvnAssault(sp)
          pause()
          adviseSpaces += sp.name
          doArvnAssaults()
        }
      }

      // -----   Start of adviseActivity() -----------------------
      removeBases()
      doArvnSweeps()
      removeEnemies()
      doArvnAssaults()
      // US Bot only increases US Aid if the ARVN faction is Human
      if (game.isHuman(ARVN))
        increaseUsAid(6)

      if (adviseSpaces.nonEmpty)
        logEndSA(US, Advise)
      adviseSpaces.nonEmpty
    }

    //  -------------------------------------------------------------
    //  Implement the US Air Lift instructions from the US Trung cards.
    //  1. Use move priorities
    //  -  Max of four total origins/destinations
    //  -  Max of two in Monsoon
    //
    // Mo_TyphoonKate - prohibits air lift, transport, and bombard, all other special activities are max 1 space
    // Mo_Medevac_Shaded - prohibits air lift
    def airLiftActivity(params: Params): Boolean = {
      if (!params.event && (momentumInPlay(Mo_TyphoonKate) || momentumInPlay(Mo_Medevac_Shaded)))
        false
      else {
        // If Shaded Booby Traps then limit spaces to 2 unless this is a limOp
        val maxSpaces: Option[Int]  = Some(
          if (params.maxSpaces.nonEmpty) params.maxSpaces.get
          else if (game.inMonsoon) 2 else 4
        )

        val nextAirLiftCandidate = (_: Boolean, _: Boolean, prohibited: Set[String]) => {
          val candidates = game.spaces filter { sp =>
            params.spaceAllowed(sp.name) &&
            params.airlift.canLiftTo(sp.name) &&
            !sp.isNorthVietnam &&
            !prohibited(sp.name)
          }

          if (candidates.nonEmpty)
            Some(pickSpaceAirLiftDest(candidates).name)
          else
            None
        }


        logSAChoice(US, AirLift)
        //  Since the air lift can be use during a sweep we must preserve the moveDestinations
        val savedMovedDestinations = moveDestinations
        val savedMovedPieces       = movedPieces
        val moveTypes = (USTroops::ARVNTroops::Irregulars:::Rangers).toSet[PieceType]

        moveDestinations = Vector.empty
        movedPieces.reset()
        movePiecesToDestinations(US, AirLift, moveTypes, false, params, maxDests = maxSpaces)(nextAirLiftCandidate)
        val effective = moveDestinations.nonEmpty
        moveDestinations = savedMovedDestinations
        movedPieces      = savedMovedPieces

        if (effective)
          logEndSA(US, AirLift)
        effective
      }
    }

    //  -------------------------------------------------------------
    //  Implement the US Air Strike instructions from the US Trung cards.
    //  1.  Degrade trail if at least 2 hits and trail not at 1
    //  2.  Use Air Strike column of Space Selection table to select spaces.
    //      In each space remove max pieces possible (unless Laser Guided Bombs in effect)
    //
    //  Destroy exposed insurent units and degrade trail
    //  Up to 6 spaces (2 in Monsoon) each with any US or ARVN piece
    //  Roll d6 to determine number of hits
    //  2 hits may be used to degrade trail by one box (only once)
    //  1 hit per enemy piece removed.  (NVATroops, then active guerrillas, then exposed bases)
    //  Shift each City/Province with >= 1 Population one level toward Active Opposition
    //
    //  Mo_RollingThunder  - prohibits air strike
    //  Mo_DaNang          - prohibits air strike
    //  Mo_BombingPause    - prohibits air strike
    //  Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
    //  Mo_WildWeasels - Either degrade trail OR remove only 1 piece (not 1-6)
    //  Mo_ADSID       - -6 NVA resrouces at any trail change
    //  TopGun_Unshaded - degrade trail by 2 boxes instead of 1
    //  TopGun_Shaded   - degrade of trail requires d6 = 4-6 (after expending 2 hits)
    //  ArcLight_Unshaded - 1 space may be Province without COIN pieces (including N. Vietnam)
    //  ArcLight_Shaded - spaces removing >1 piece shift two levels toward Active Opposition
    //  LaserGuidedBombs_Unshaded - In space were only 1 piece removed, no shift toward Active Opposition
    //  LaserGuidedBombs_Shaded - Remove no more than 2 pieces total
    //  AAA_Shaded - Cannot degrade the trail below 2
    //  MiGs_Shaded - If trail degraded, US remove 1 Available Troop to Casualties
    //  SA2s_Unshaded - When trail degraded, US removes 1 NVA piece (including untunneled base) outside the South (includes N. Vietnam)
    //  Mo_Oriskany   - Shaded (prohibits degrade of trail) (includes air strike, coup round, NOT evnts!)

    def airStrikeActivity(params: Params): Boolean = {
      val prohibited = momentumInPlay(Mo_RollingThunder) ||
                       momentumInPlay(Mo_DaNang)         ||
                       momentumInPlay(Mo_BombingPause)
      if (!params.event && prohibited)
        false
      else {
        val adsid    = game.isHuman(NVA) && momentumInPlay(Mo_ADSID)
        val oriskany = momentumInPlay(Mo_Oriskany)
        val migs_shaded = capabilityInPlay(MiGs_Shaded) && !capabilityInPlay(TopGun_Unshaded)
        val aaa_shaded = capabilityInPlay(AAA_Shaded)
        val arclight_unshaded = capabilityInPlay(ArcLight_Unshaded)
        val minTrail   = if (aaa_shaded) 2 else TrailMin
        val maxHits       = params.airstrike.maxHits getOrElse d6
        var hitsRemaining = maxHits
        val wildWeasels = momentumInPlay(Mo_WildWeasels) &&
                        (!capabilityInPlay(LaserGuidedBombs_Shaded) ||
                         isEventMoreRecentThan(Mo_WildWeasels, LaserGuidedBombs_Shaded.name))
        val laserUnshaded = capabilityInPlay(LaserGuidedBombs_Unshaded)
        val laserShaded = capabilityInPlay(LaserGuidedBombs_Shaded) &&
                          (!momentumInPlay(Mo_WildWeasels) ||
                          isEventMoreRecentThan(LaserGuidedBombs_Shaded.name, Mo_WildWeasels))
        val arcLightShaded = capabilityInPlay(ArcLight_Shaded)
        val maxSpaces = if      (params.maxSpaces.nonEmpty) params.maxSpaces.get
                        else if (momentumInPlay(Mo_TyphoonKate)) 1
                        else if (game.inMonsoon) 2
                        else 6
        val maxPieces = if (wildWeasels) 1 else if (laserShaded) 2 else maxHits
        var strikeSpaces = Set.empty[String]
        var spaceKills: Map[String, Int] = Map.empty.withDefaultValue(0)
        var arclight_unshaded_used = false
        var totalRemoved = 0

        def logSelectAirStrike(): Unit = if (hitsRemaining == maxHits) {
          logSAChoice(US, AirStrike)
          if (params.airstrike.maxHits.nonEmpty)
            log(s"Number of hits = $maxHits")
          else
            log(s"Die roll to determine the number of hits = $maxHits")
        }
        // Degrade the trail if possible
        def degradeTheTrail(): Unit = {
          val canDegrade = params.airstrike.canDegradeTrail &&
                           !oriskany &&
                           hitsRemaining > 1 &&
                           game.trail > minTrail
          if (canDegrade) {
            val die    = d6
            val success = !capabilityInPlay(TopGun_Shaded) || die > 3
            val numBoxes = {
              val num = if (capabilityInPlay(TopGun_Unshaded)) 2 else 1
              val newTrail = (game.trail - num) max minTrail
              game.trail - newTrail
            }

            logSelectAirStrike()
            log(s"\nUS Air Strikes The Trail")
            log(separator())

            if (capabilityInPlay(TopGun_Shaded))
              log(s"Die roll ($TopGun_Shaded): $die [${if (success) "Success!" else "Failure"}]")


            // Hits applied even if Top Gun die roll failed
            hitsRemaining -= 2
            
            if (success) {
              degradeTrail(numBoxes)

              if (adsid) {
                log(s"Momentum: $Mo_ADSID reduces NVA resources at trail change")
                decreaseResources(NVA, 6)
              }

              if (migs_shaded && game.availablePieces.has(USTroops))
                  moveAvailableToCasualties(Pieces(usTroops = 1), Some(s"$MiGs_Shaded triggers"))

              if (capabilityInPlay(SA2s_Unshaded)) {
                val CanRemove = List(NVABase, NVATroops, NVAGuerrillas_U, NVAGuerrillas_A)
                log(s"\n$SA2s_Unshaded triggers")
                val sa2Candidates = spaces(OutsideSouth) filter (_.pieces.has(CanRemove))
                if (sa2Candidates.isEmpty)
                  log("There are no NVA outside the south that can be removed") // Very unlikely!
                else {
                  val priorities = List(
                    new BooleanPriority[Space]("NVA Base", _.pieces.has(NVABase)),
                    new BooleanPriority[Space]("NVA Troops", _.pieces.has(NVATroops)),
                    new BooleanPriority[Space]("NVA Guerrillas", _.pieces.has(NVAGuerrillas))
                  )
                  val sp = pickSpaceRemoveReplace(narrowCandidates(sa2Candidates, priorities))
                  val toRemove = selectEnemyRemoveReplaceActivate(sp.pieces.only(CanRemove), 1)
                  removeToAvailable(sp.name, toRemove)
                }
              }
            }
            pause()
          }
        }

        def strikeASpace(): Unit = {
          val isCandidate = (sp: Space) => {
            val canTarget = (sp.pieces.has(CoinPieces) ||
                            params.airstrike.noCoin ||
                            (sp.isProvince && arclight_unshaded && !arclight_unshaded_used))
            params.spaceAllowed(sp.name)   &&
            params.airstrike.spaceAllowed(sp.name) &&
            !strikeSpaces(sp.name)         &&
            sp.pieces.hasExposedInsurgents &&
            canTarget
          }

          val candidates = game.spaces filter isCandidate

          if (hitsRemaining > 0 && strikeSpaces.size < maxSpaces && totalRemoved < maxPieces && candidates.nonEmpty) {
            val sp  = pickSpaceAirStrike(candidates)
            val num = if (params.airstrike.maxHitsPerSpace.nonEmpty)
                        params.airstrike.maxHitsPerSpace.get min hitsRemaining
                 else if (laserUnshaded && sp.support != ActiveOpposition)  1
                 else if (arcLightShaded && sp.support > PassiveOpposition) 1
                 else hitsRemaining
            val (killedPieces, _) = selectRemoveEnemyInsurgentBasesLast(sp.pieces, hitsRemaining)

            logSelectAirStrike()
            log(s"\nUS Air Strikes ${sp.name}")
            log(separator())
            removeToAvailable(sp.name, killedPieces)
            if (!sp.pieces.has(CoinPieces) && arclight_unshaded)
              arclight_unshaded_used = true

            hitsRemaining -= killedPieces.total
            totalRemoved += killedPieces.total
            strikeSpaces += sp.name
            spaceKills += sp.name -> killedPieces.total
            pause()
            strikeASpace()
          }
        }

        def shiftSupportInStrikeSpaces(): Unit = {
          var shiftedOnce = false
          for {
            name <- strikeSpaces
            sp = game.getSpace(name)
            if !(sp.isLoC || sp.population == 0)
          } {
            if (!shiftedOnce) {
              log("\nShift support in strike spaces toward Active Opposition")
              log(separator(char = '='))
              shiftedOnce = true
            }
            val numKills = spaceKills(name)

            if (sp.support == ActiveOpposition)
              log(s"\n$name: No shift [Already at Active Opposition]")
            else if (numKills == 0)
              log(s"\n$name: No shift [No pieces removed]")
            else if (numKills > 1 && capabilityInPlay(ArcLight_Shaded) && sp.support > PassiveOpposition) {
                log(s"\n$name: Shift 2 levels [$ArcLight_Shaded]")
                log(separator())
                decreaseSupport(name, 2)
            }
            else if (numKills == 1 && capabilityInPlay(LaserGuidedBombs_Unshaded))
              log(s"\n$name: No shift [$LaserGuidedBombs_Unshaded]")
            else {
              log(s"\n$name: Shift 1 level toward Active Opposition")
              log(separator())
              decreaseSupport(name, 1)
            }
          }
          if (shiftedOnce)
            pause()
        }

        // Start of Air Strike Special Activity
        // ------------------------------------
        degradeTheTrail()
        loggingControlChanges {
          if (!momentumInPlay(Mo_WildWeasels) || hitsRemaining == maxHits)
            strikeASpace()

          if (hitsRemaining < maxHits) {
            shiftSupportInStrikeSpaces()
            logEndSA(US, AirStrike)
          }
        }
        hitsRemaining < maxHits  // True if we did an airstrike action
      }
    }
  }


  // ================================================================
  // ARVN Specific code
  // ================================================================
  object ARVN_Bot {

    // 2-pop Province with COIN Base and NO Supoprt
    val prov2PopWithCoinBaseNoSupport = (sp: Space) =>
      sp.isProvince &&
      sp.population == 2 &&
      sp.pieces.has(CoinBases) &&
      sp.support < PassiveSupport

    // 2-pop Province WITHOUT Control and NO Supoprt
    val prov2PopWithCoinControlNoSupport = (sp: Space) =>
      sp.isProvince &&
      sp.population == 2 &&
      sp.coinControlled &&
      sp.support < PassiveSupport

    // 2-pop Province with COIN Base, US Troops, and NO Support
    val pop2PopCoinBaseUsTroopsNoSupport = (sp: Space) =>
      sp.isProvince &&
      sp.population == 2 &&
      sp.pieces.has(CoinBases) &&
      sp.pieces.has(USTroops) &&
      sp.support < PassiveSupport

    // 2-pop City/Province with US Troops and support
    val pop2SpaceWithUsTroopsAndSupport = (sp: Space) =>
      (sp.isCity || sp.isProvince)  &&
      sp.population == 2 &&
      sp.support > Neutral &&
      sp.pieces.has(USTroops)

    def locOrProvinceNoBase(ignoreCoinBases: Boolean) = (sp: Space) =>
      sp.isLoC || (sp.isProvince && (ignoreCoinBases || !sp.pieces.has(CoinBases)))

    val provinceWithCoinControlNoCoinBase = (sp: Space) =>
      sp.isProvince &&
      sp.coinControlled &&
      !sp.pieces.has(CoinBases)

    val mostOpposition = List(
      new LowestScore[Space]("Most Opposition", _.support.value)
    )
    
    // Determine which ARVN troops/police are eligible to move
    // The Trung rules have instructions for which cubes
    // should remain in place
    // -----------------------------------------------
    // o  Unless Troops are leaving a Province without a COIN base,
    //    keep ARVN cubes > US Troops in 2-Pop spaces with Support
    // o  Keep both 1 ARVN Troop and 1 Police in each space without support.
    // AND:
    // o  Keep COIN Pieces > Enemies, first
    //    then keep COIN+VC pieces >= NVA pieces
    def determineEligibleMovingCubes(cubeType: PieceType, ignoreCoinBases: Boolean): MovingGroups = {
      val eligibleCubes = new MovingGroups()
      // Add the eligible cubes for each space
      for (sp <- game.spaces; if sp.pieces.has(cubeType)) {
        val numCubeType    = sp.pieces.totalOf(cubeType)
        val numARVNTroops  = sp.pieces.totalOf(ARVNTroops)
        val numPolice      = sp.pieces.totalOf(ARVNPolice)
        val numARVNCubes   = numARVNTroops + numPolice
        val numUSTroops    = sp.pieces.totalOf(USTroops)
        val numNVA         = sp.pieces.totalOf(NVAPieces)
        val numVC          = sp.pieces.totalOf(VCPieces)
        val numCOIN        = sp.pieces.totalOf(CoinPieces)
        val numOtherARVN   = numARVNCubes - numCubeType
        val numOtherCOIN   = numCOIN - numCubeType

        // If we are moving mandatory Troops out of LoCs or Provinces
        // without a base, then we don't keep any behind
        val numToKeep = if (cubeType == ARVNTroops && locOrProvinceNoBase(ignoreCoinBases)(sp))
          0
        else {
          val keepForSupport = if (cubeType == ARVNTroops && locOrProvinceNoBase(ignoreCoinBases)(sp))
            0
          else if (sp.population == 2 && sp.support > Neutral && numARVNCubes > numUSTroops)
            (numUSTroops + 1 - numOtherARVN) min numCubeType
          else if (sp.support < PassiveSupport && numARVNTroops > 0 && numPolice > 0)
            1
          else
            0

          val keepForControl = if (numCOIN > numNVA + numVC)
            (numNVA + numVC + 1 - numOtherCOIN) min numCubeType
          else if (numCOIN + numVC >= numNVA)
            (numNVA - numOtherCOIN - numVC) min numCubeType
          else
            0

          keepForSupport max keepForControl
        }


        // To ensure that both conditions are met we must use
        // the smaller value of the two, making sure that we
        // don't exceed the number that are actually in the space.
        val movingCubes = Pieces().set(numCubeType - numToKeep, cubeType)
        if (movingCubes.nonEmpty)
          eligibleCubes.set(sp.name, movingCubes)
      }
      eligibleCubes
    }

    // =========================================================================
    // Carry out the Troop Redeployment part of the
    // Coup Round Redeploy phase.
    // This is called by redeployARVNForces(), see the comment for
    // that function for a summary of the Trung Troop redeployment
    // instructions.
    // =========================================================================
    private def redeployARVNTroops(troopDestinations: List[String], ignoreCoinBases: Boolean): Unit = {
      var destsUsed       = Set.empty[String]  // Dests that we moved troops to
      var destsConsidered = Set.empty[String]  // Dests that were selected but did not need troops
      val eligibleTroops = determineEligibleMovingCubes(ARVNTroops, ignoreCoinBases)

      // o  Move ARVN Troops from LoCs and Provinces without a COIN base first,
      //    then if the `mandatoryOnly` flag is not set from any spaces with
      //    troops that are eligible to move.
      def getTroopOrigin(mandatoryOnly: Boolean): Option[String] = {
        val candidates1 = game.spaces filter { sp =>
          !destsUsed(sp.name) &&
          locOrProvinceNoBase(ignoreCoinBases)(sp)  &&
          eligibleTroops(sp.name).nonEmpty
        }

        if (candidates1.nonEmpty)
          Some(shuffle(candidates1).head.name)
        else if (!mandatoryOnly) {
          val candidates2 = game.spaces filter { sp =>
            !destsUsed(sp.name) &&
            eligibleTroops(sp.name).nonEmpty
          }
          if (candidates2.nonEmpty) {
            val priorities = List(new HighestScore[Space]("Most ARVN Troops", _.pieces.totalOf(ARVNTroops)))
            Some(bestCandidate(candidates2, priorities).name)
          }
          else
            None
        }
        else
          None
      }

      // Move troops to the given destination using as many origin spaces
      // as need to fullfil the number needed.  Or until we run out of
      // eligible troops.
      // Returns true if we have exhausted the eligible moveable troops
      def moveTroopsTo(dest: String, numNeeded: Int, mandatoryOnly: Boolean): Boolean = {
        def nextOrigin(remaining: Int): Boolean = if (remaining > 0) {
          getTroopOrigin(mandatoryOnly) match {
            case None => true  // No more origins with eligible troops

            case Some(origin) =>
              val numToMove = remaining min eligibleTroops(origin).total
              val troops    = Pieces(arvnTroops = numToMove)
              movePieces(troops, origin, dest)
              eligibleTroops.remove(origin, troops)
              nextOrigin(remaining - numToMove)
          }
        }
        else
          false

        // Record that this destination was considered so it is
        // not selected as a destination again.
        destsConsidered += dest
        if (numNeeded > 0) {
          // Record that we moved troops to this destination
          // so we do not subsequently select it as an origin.
          destsUsed += dest
          nextOrigin(numNeeded)
        }
        else
          false
      }

      // Find the next destination for troop deployment
      // using the Trung instructions, then determine how
      // many troops should be moved there and move as many
      // as possible up to that number.
      // Then if we have not exhausted all of the eligible troops
      // call recursively to do it again.
      //
      // Move Troop Priorities
      // 1. Move Troops to get COIN pieces to exceed enemies in Saigon and in all
      //    Provinces with a COIN Base
      // 2. Get 1 Troop to Hue and to each 2-Pop Province with a COIN base AND no support,
      //    most Opposition first
      // 3. Move Troops to get ARNV cubes to exceed US Troops in each 2-Pop Province
      //    with a COIN base, US Troops, and NO Support
      // 4. Move any Troops still in LoCs and Provinces without a COIN base
      def nextTroopDestination(): Unit = {
        lazy val candidates1 = spaces(troopDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          sp.name == Saigon || (sp.isProvince && sp.pieces.has(CoinBases)) &&
          sp.pieces.totalOf(CoinPieces) <= sp.pieces.totalOf(InsurgentPieces)
        }

        lazy val candidates2 = spaces(troopDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          (sp.name == Hue || prov2PopWithCoinBaseNoSupport(sp)) &&
          sp.pieces.totalOf(ARVNTroops) == 0
        }

        lazy val candidates3 = spaces(troopDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          pop2PopCoinBaseUsTroopsNoSupport(sp) &&
          sp.pieces.totalOf(ARVNCubes) <= sp.pieces.totalOf(USTroops)
        }

        val exhausted = if (candidates1.nonEmpty) {
          // 1. Move Troops to get COIN pieces to exceed enemies in Saigon and in all
          //    Provinces with a COIN Base
          val sp = pickSpacePlaceCubesRangers(candidates1)
          val numNeeded = sp.pieces.totalOf(InsurgentPieces) + 1 - sp.pieces.totalOf(CoinPieces)

          moveTroopsTo(sp.name, numNeeded, mandatoryOnly = false)
        }
        else if (candidates2.nonEmpty) {
          // 2. Get 1 Troop to Hue and to each 2-Pop Province with a COIN base AND no support,
          //    most Opposition first
          // ActiveOpposition has the LOWEST support value.
          val sp = pickSpacePlaceCubesRangers(narrowCandidates(candidates2, mostOpposition))

          moveTroopsTo(sp.name, 1, mandatoryOnly = false)
        }
        else if (candidates3.nonEmpty) {
          // 3. Move Troops to get ARNV cubes to exceed US Troops in each 2-Pop Province
          //    with a COIN base, US Troops, and NO Support
          val sp = pickSpacePlaceCubesRangers(candidates3)
          val numNeeded = sp.pieces.totalOf(USTroops) + 1 - sp.pieces.totalOf(ARVNCubes)

          moveTroopsTo(sp.name, 1, mandatoryOnly = false)
        }
        else {
          // 4. Move any Troops still in LoCs and Provinces without a COIN base
          //    (ie. mandatory redeployments)
          // -  Not sure the best way to handle the remaining manadatory troop redeployments?
          //    I will dole them out one at a time using place cubes priorities
          //    (May want to try to consolodate the movements to make the log cleaner?)
          val sp = pickSpacePlaceCubesRangers(spaces(troopDestinations))
          moveTroopsTo(sp.name, 1, mandatoryOnly = true)
        }

        if (!exhausted)
          nextTroopDestination()
      }

      // Start moving troops to the first destination
      nextTroopDestination()
    }

    // =========================================================================
    // Carry out the Police Redeployment part of the
    // Coup Round Redeploy phase.
    // This is called by redeployARVNForces(), see the comment for
    // that function for a summary of the Trung Police redeployment
    // instructions.
    // =========================================================================
    private def redeployARVNPolice(policeDestinations: List[String]): Unit = {
      var destsUsed       = Set.empty[String]  // Dests that we moved troops to
      var destsConsidered = Set.empty[String]  // Dests that were selected but did not need troops
      val eligiblePolice = determineEligibleMovingCubes(ARVNPolice, false)

      // o  Move Police from spaces with the most Police
      def getPoliceOrigin(): Option[String] = {
        val priorities = List(
          new HighestScore[Space]("Most ARVN Police", _.pieces.totalOf(ARVNPolice))
        )
        val candidates = game.spaces filter { sp =>
          !destsUsed(sp.name) &&
          eligiblePolice(sp.name).nonEmpty
        }

        if (candidates.nonEmpty)
          Some(bestCandidate(candidates, priorities).name)
        else
          None
      }

      // Move troops to the given destination using as many origin spaces
      // as need to fullfil the number needed.  Or until we run out of
      // eligible troops.
      // Returns true if we have exhausted the eligible moveable troops
      def movePoliceTo(dest: String, numNeeded: Int): Boolean = {

        def nextOrigin(remaining: Int): Boolean = if (remaining > 0) {
          getPoliceOrigin() match {
            case None => true  // No more origins with eligible police

            case Some(origin) =>
              val numToMove = remaining min eligiblePolice(origin).total
              val police    = Pieces(arvnPolice = numToMove)
              movePieces(police, origin, dest)
              eligiblePolice.remove(origin, police)
              nextOrigin(remaining - numToMove)
          }
        }
        else
          false

        // Record that this destination was considered so it is
        // not selected as a destination again.
        destsConsidered += dest
        if (numNeeded > 0) {
          // Record that we moved troops to this destination
          // so we do not subsequently select it as an origin.
          destsUsed += dest
          nextOrigin(numNeeded)
        }
        else
          false
      }


      // Find the next destination for police deployment
      // using the Trung instructions, then determine how
      // many police should be moved there and move as many
      // as possible up to that number.
      // Then if we have not exhausted all of the eligible police
      // call recursively to do it again.
      //
      // Move Police Priorities
      // 1. Get Police to exceed enemies in all Provinces with COIN Control and
      //    NO Coin base
      // 2. Get 1 Police to Hue and each 2-Pop Province with COIN control and NO support,
      //    most Opposition first.
      // 3. Move Police to get ARVN cubes to exceed US Troops in each 2-Pop space with
      //    both US Troops AND support.
      // 4. Get Police to equal Guerrillas on all LoCs, highest Econ first.
      def nextPoliceDestination(): Unit = {
        lazy val candidates1 = spaces(policeDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          provinceWithCoinControlNoCoinBase(sp) &&
          sp.pieces.totalOf(ARVNPolice) <= sp.pieces.totalOf(InsurgentPieces)
        }

        lazy val candidates2 = spaces(policeDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          (sp.name == Hue || prov2PopWithCoinControlNoSupport(sp)) &&
          sp.pieces.totalOf(ARVNPolice) == 0
        }

        lazy val candidates3 = spaces(policeDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          pop2SpaceWithUsTroopsAndSupport(sp) &&
          sp.pieces.totalOf(ARVNCubes) <= sp.pieces.totalOf(USTroops)
        }

        lazy val candidates4 = spaces(policeDestinations) filter { sp =>
          !destsConsidered(sp.name) &&
          sp.isLoC &&
          sp.pieces.totalOf(ARVNPolice) < sp.pieces.totalOf(Guerrillas)
        }

        val exhausted = if (candidates1.nonEmpty) {
          // 1. Get Police to exceed enemies in all Provinces with
          //     COIN Control and NO Coin base
          val sp = pickSpacePlaceCubesRangers(candidates1)
          val numNeeded = sp.pieces.totalOf(InsurgentPieces) + 1 - sp.pieces.totalOf(ARVNPolice)

          movePoliceTo(sp.name, numNeeded)
        }
        else if (candidates2.nonEmpty) {
          // 2. Get 1 Police to Hue and each 2-Pop Province with
          //    COIN control and NO support, most Opposition first.
          val sp = pickSpacePlaceCubesRangers(narrowCandidates(candidates2, mostOpposition))

          movePoliceTo(sp.name, 1)
        }
        else if (candidates3.nonEmpty) {
          // 3. Move Police to get ARVN cubes to exceed US Troops in
          //    each 2-Pop space with both US Troops AND support.
          val sp = pickSpacePlaceCubesRangers(candidates3)
          val numNeeded = sp.pieces.totalOf(USTroops) + 1 - sp.pieces.totalOf(ARVNCubes)

          movePoliceTo(sp.name, numNeeded)
        }
        else if (candidates4.nonEmpty) {
          // 4. Get Police to equal Guerrillas on all LoCs,
          //    highest Econ first.
          val sp = pickSpacePlaceCubesRangers(candidates4)
          val numNeeded = sp.pieces.totalOf(Guerrillas) - sp.pieces.totalOf(ARVNPolice)

          movePoliceTo(sp.name, numNeeded)
        }
        else
          true  // We've exhausted destinations

        if (!exhausted)
          nextPoliceDestination()
      }

      // Start moving police to the first destination
      nextPoliceDestination()
    }

    // Coup Round ARVN Redeployment
    //
    // Summary of Trung Instructions
    // ===============================================================================
    // Keep in place
    // -------------------------------------------------------------------------------
    // o  Unless Troops are leaving a Province without a COIN base,
    //    keep ARVN cubes > US Troops in 2-Pop spaces with Support
    // o  Keep both 1 ARVN Troop and 1 Police in each space without support.
    // AND:
    // o  Keep COIN Pieces > Enemies, first
    //    then keep COIN+VC pieces >= NVA pieces
    //
    // Move cubes
    // -------------------------------------------------------------------------------
    // o  Within each instruction move cubes using "Place Cubes or Rangers" to
    //    select destination
    // o  Move Police from spaces with the most Police
    // o  Move ARVN Troops from LoCs and Provinces without a COIN base first,
    //    then spaces with the most ARVN Troops
    //
    // Move Troop Priorities
    // 1. Move Troops to get COIN pieces to exceed enemies in Saigon and in all
    //    Provinces with a COIN Base
    // 2. Get 1 Troop to Hue and to each 2-Pop Province with a COIN base AND no support,
    //    most Opposition first
    // 3. Move Troops to get ARNV cubes to exceed US Troops in each 2-Pop Province
    //    with a COIN base, US Troops, and NO Support
    // 4. Move any Troops still in LoCs and Provinces without a COIN base
    //
    // Move Police Priorities
    // 1. Get Police to exceed enemies in all Provinces with COIN Control and
    //    NO Coin base
    // 2. Get 1 Police to Hue and each 2-Pop Province with COIN control and NO support,
    //    most Opposition first.
    // 3. Move Police to get ARVN cubes to exceed US Troops in each 2-Pop space with
    //    both US Troops AND support.
    // 4. Get Police to equal Guerrillas on all LoCs, highest Econ first.
    // ===============================================================================
    def redeployARVNForces(troops: Boolean, police: Boolean, ignoreCoinBases: Boolean): Unit = {
      // Get the destinations up front because they should not be affected
      // by changes to control as pieces are moved.
      // Control is not adjusted until the end of the Redeploy phase.
      val troopDestinations  = arvnRedeployTroopDestinations(ignoreCoinBases)
      val policeDestinations = arvnRedeployPoliceDestinations()

      if (troops) {
        if (arvnCanRedeployTroops(ignoreCoinBases))
          redeployARVNTroops(troopDestinations, ignoreCoinBases)
        else
          log("There are no ARVN Troops that can Redeploy")
      }

      if (police) {
        if (arvnCanRedeployPolice)
          redeployARVNPolice(policeDestinations)
        else
          log("There are no ARVN Police that can Redeploy")
      }
    }

    def threeD6_LE_AvailARVN = trungDiceCheck(3, game.availablePieces.totalOf(ARVNPieces), CMP_LE, s"Available ARVN pieces")

    //  Used during ARVN operations
    //  If we are tracking ARVN resources, then after passing an activation
    //  roll we must also make sure that there are sufficient ARVN resources
    //  to pay for the operation.
    def checkARVNActivation(needRoll: Boolean, actNum: Int, free: Boolean) =
      free ||
      (checkActivation(ARVN, needRoll, actNum) && (!game.trackResources(ARVN) || game.arvnResources >= 3))

    val arvnAssaultWouldAddCoinControl = (sp: Space) =>
      !sp.coinControlled && assaultResult(ARVN, NormalTroops, vulnerableTunnels = false)(sp).coinControlled

    def arvnAssaultWouldAddCoinControlToASpace: Boolean = {
      val result = game.nonLocSpaces exists arvnAssaultWouldAddCoinControl
      logCheck(result, "ARVN Assault would add COIN Control?")
    }

    // Return true if an assault on one of the spaces
    def arvnAssaultWouldUnblockCanTo_HueRouteSpace(candidates: Iterable[Space]): Boolean = {
      val result = candidates exists assaultInWouldUnblockAlongCanTho_HueRoute(ARVN, NormalTroops, vulnerableTunnels = false)
      logCheck(result, "ARVN Assault would unblock LoC route between Can Tho and Hue?")
    }

    def arvnAssaultWouldAddControlOrUnblockCanTo_Hue: Boolean = {
      arvnAssaultWouldAddCoinControlToASpace ||
      arvnAssaultWouldUnblockCanTo_HueRouteSpace(game.patrolSpaces)
    }

    def fiveArvnTroopsPlusRangersInAnySpace: Boolean = {
      val result = game.spaces exists { sp => sp.pieces.totalOf(ARVNTroops::Rangers) >= 5 }
      logCheck(result, "5+ ARVN Troops + Rangers in any one space?")
    }

    def any2PopSpaceWithSupportAndMoreArvnCubesThanUsCubes: Boolean = {
      val result = game.nonLocSpaces exists { sp =>
        sp.population == 2 &&
        sp.pieces.totalOf(ARVNCubes) > sp.pieces.totalOf(USTroops)
      }
      logCheck(result, "Any 2-Pop space with Support where ARVN cubes exceed US cubes?")
    }

    def nvaBaseOrNvaControlAt2PlusPop: Boolean = {
      val result = game.nonLocSpaces exists { sp =>
        sp.population >= 2 &&
        (sp.pieces.has(NVABases) || sp.nvaControlled)
      }
      logCheck(result, "NVA Base or NVA Control in any 2+ Pop space?")
    }
    
    // If Transport special activity was used,  then Armored Cavalry (unshaded)
    // allows ARVN to free assault in one Transport destination
    // Will only trigger if an transport special activity has take place.
    def armoredCavalryAssault(): Unit = {
      val candidates = spaces(transportDestinations) filter assaultEffective(ARVN, NormalTroops, vulnerableTunnels = false)
      // General Landsdale prohibits assault
      if (capabilityInPlay(ArmoredCavalry_Unshaded) && !momentumInPlay(Mo_GeneralLansdale) && candidates.nonEmpty) {
        val sp = pickSpaceRemoveReplace(candidates)
        log(s"\n$ArmoredCavalry_Unshaded triggers a free Assault")
        performAssault(ARVN, sp.name, Params(free = true))
      }
    }

    // ARVN Spaces Priorities: Shift Toward Passive Support
    def pickSpaceTowardPassiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botDebug(s"\nARVN Select space (Shift Toward Passive Support): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Place Bases
    def pickSpacePlaceBases(candidates: List[Space]): Space = {

      val priorities = List(
        ProvinceWithoutCOINBase,
        MostPopulation,
        MostTotalSupport,
        CityProvinceFewestEnemyPieces
      )

      botDebug(s"\nARVN Select space (Place Bases): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }


    // ARVN Spaces Priorities: Place Cubes or Rangers
    def pickSpacePlaceCubesRangers(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             SouthWithoutCoinControl),
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(true,             CityProvinceFewestEnemyPieces),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botDebug(s"\nARVN Select space (Place Cubes or Rangers): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Sweep or Transport Destinations
    def pickSpaceSweepTransportDest(candidates: List[Space]): Space = {

      val priorities = List(
        SouthWithoutCoinControl,
        MostPopulation,
        HasEnemyBase,
        CityProvinceFewestEnemyPieces
      )

      botDebug(s"\nARVN Select space (Sweep or Transport Destinations): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Patrol Destinations
    def pickSpacePatrolDest(candidates: List[Space]): Space = {

      val priorities = List(
        SouthWithoutCoinControl,
        MostPopulation,
        LocWithEnemyPieces
      )

      botDebug(s"\nARVN Select space (Patrol Destinations): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    case class GovernSpace(sp: Space, patronageOK: Boolean)

    // ARVN Spaces Priorities: Govern
    def pickSpaceGovern(candidates: List[GovernSpace]): GovernSpace = {

      val MostPopulation = new HighestScore[GovernSpace](
        "Most Population",
        governSpace => if (governSpace.sp.isLoC) NO_SCORE else governSpace.sp.population
      )

      val MostTotalSupport = new HighestScore[GovernSpace](
        "Most Total Support",
        governSpace => if (governSpace.sp.isLoC || !governSpace.patronageOK) NO_SCORE else governSpace.sp.supportValue
      )

      val priorities = List(
        MostPopulation,
        MostTotalSupport
      )

      botDebug(s"\nARVN Select space (Govern): [${andList(candidates)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    def pickSpaceRemoveReplace(candidates: List[Space]): Space =
      pickSpaceRemoveReplaceSpecific(candidates, false, false)

    // ARVN Spaces Priorities: Remove or Replace
    def pickSpaceRemoveReplaceSpecific(candidates: List[Space], assault: Boolean = false, raid: Boolean = false): Space = {

      val priorities = List(
        filterIf(true,     VulnerableBase),
        filterIf(raid,     RaidLaosCambodia),
        filterIf(true,     SouthWithoutCoinControl),
        filterIf(true,     MostPopulation),
        filterIf(true,     LocWithEnemyPieces),
        filterIf(assault,  MostArvnFirepower),
        filterIf(true,     CityProvinceFewestEnemyPieces)
      ).flatten

      botDebug(s"\nARVN Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    //  -------------------------------------------------------------
    //  Implement the ARVN Train instructions from the ARVN Trung cards.
    //  1. Place all available Rangers
    //  2. Place cubes
    //  3. Place 1 Base in a 2-Pop Province with no COIN Base
    //     (losing no COIN control)
    //     OR
    //     Remove terror and shift toward Passive Suport for max 1d3
    //
    // RVN_Leader_DuongVanMinh - Each ARVN Train operation adds +5 bonus Aid
    // RVN_Leader_NguyenCaoKy  - US/ARVN pacification costs 4 resources per Terror/Level
    def trainOp(params: Params, actNum: Int): Option[CoinOp] = {
      val maxTrain     = params.maxSpaces getOrElse NO_LIMIT
      def trained      = trainingSpaces.nonEmpty
      def canTrain     = trainingSpaces.size < maxTrain &&
                         checkARVNActivation(trained, actNum, params.free)
      def prohibited(sp: Space) = !params.spaceAllowed(sp.name) || trainingSpaces(sp.name)
      val canTrainRangers = (sp: Space) =>
        !(prohibited(sp) || sp.nvaControlled) &&
        (sp.isCity || sp.pieces.has(CoinBases))
      val canTrainCubes   = (sp: Space) =>
        !(prohibited(sp) || sp.nvaControlled) &&
        (sp.isCity || sp.pieces.has(CoinBases))

      // Return true if we can continue training
      // ie.  Did not fail activation or run out of resources
      def trainToPlaceRangers(candidates: List[Space]): Boolean = {
        if (game.availablePieces.has(Rangers) && candidates.nonEmpty) {
          if (canTrain) {
            val sp      = pickSpacePlaceCubesRangers(candidates)
            val num     = game.availablePieces.totalOf(Rangers) min 2
            val toPlace = Pieces(rangers_U = num)

            log(s"\n$ARVN selects ${sp.name} for Train")
            log(separator())
            if (game.trackResources(ARVN))
              decreaseResources(ARVN, 3)

            placePieces(sp.name, toPlace)
            trainingSpaces += sp.name
            pause()
            trainToPlaceRangers(candidates filterNot (_.name == sp.name))
          }
          else
            false  // Failed activation or out of ARVN resources
        }
        else
          true  // No failed activattion, just ran out of candidates
      }

      // Return true if we can continue training
      // ie.  Did not fail activation or run out of resources
      def trainToPlaceCubes(candidates: List[Space]): Boolean = {
        if (game.availablePieces.has(ARVNCubes) && candidates.nonEmpty) {
          if (canTrain) {
            val sp      = pickSpacePlaceCubesRangers(candidates)
            val cubes   = game.availablePieces.only(ARVNCubes)
            val toPlace = selectFriendlyToPlaceOrMove(cubes, cubes.total min 6)

            log(s"\n$ARVN selects ${sp.name} for Train")
            log(separator())
            if (game.trackResources(ARVN))
              decreaseResources(ARVN, 3)
            placePieces(sp.name, toPlace)
            trainingSpaces += sp.name
            pause()
            trainToPlaceCubes(candidates filterNot (_.name == sp.name))
          }
          else
            false  // Failed activation or out of ARVN resources
        }
        else
          true  // No failed activattion, just ran out of candidates
      }

      def placeARVNBase(): Boolean = {
        val baseCandidates = game.nonLocSpaces filter { sp =>
          !sp.nvaControlled                &&
          sp.isProvince                    &&
          sp.population == 2               &&
          sp.totalBases < 2                &&
          !sp.pieces.has(CoinBases)        &&
          sp.pieces.totalOf(ARVNCubes) > 2 &&
          (!sp.coinControlled ||    // Cannot lose COIN control
          sp.pieces.totalOf(CoinPieces) - 3 > sp.pieces.totalOf(InsurgentPieces))
        }

        var placedBase = false
        if (game.availablePieces.has(ARVNBase) && baseCandidates.nonEmpty) {
          val sp = pickSpacePlaceBases(baseCandidates)
          val mustPay = game.trackResources(ARVN) && !trainingSpaces(sp.name)
          // If we have not already trained in this space and we are
          // tracking ARVN resources, make sure there is cash in the bank.
          if (!mustPay || game.arvnResources >= 3) {
            val toRemove = selectFriendlyRemoval(sp.pieces.only(ARVNCubes), 3)

            if (trainingSpaces(sp.name))
              log(s"\n$ARVN selects ${sp.name} for Train")
            else
              log(s"\n$ARVN will place a base in ${sp.name}")
            log(separator())

            if (mustPay)
              decreaseResources(ARVN, 3)

            loggingControlChanges {
              removeToAvailable(sp.name, toRemove)
              placePieces(sp.name, Pieces(arvnBases = 1))
            }
            trainingSpaces += sp.name  // In case we did not train there previously
            placedBase = true
          }
        }
        placedBase
      }

      def pacifyOneSpace(): Unit = {
        val baseRate   = if (isRVNLeader(RVN_Leader_NguyenCaoKy)) 4 else 3
        val costEach   = if (game.trackResources(ARVN)) baseRate else 0
        val candidates = spaces(trainingSpaces) filter { sp =>
          sp.coinControlled &&
          sp.pieces.has(ARVNTroops)   &&
          sp.pieces.has(ARVNPolice)   &&
          sp.support < PassiveSupport
        }

        if (candidates.nonEmpty && game.arvnResources >= costEach) {
          val sp = pickSpaceTowardPassiveSupport(candidates)
          pacifySpace(sp.name, ARVN, coupRound = false)
        }
      }

      if (!params.event)
        logOpChoice(ARVN, Train)
      trainToPlaceRangers(game.nonLocSpaces filter canTrainRangers) &&
      trainToPlaceCubes(game.nonLocSpaces filter canTrainCubes)
      if (!placeARVNBase())
        pacifyOneSpace()

      if (trained) {
        if (isRVNLeader(RVN_Leader_DuongVanMinh)) {
          log(s"\nLeader: $RVN_Leader_DuongVanMinh effect triggers")
          log(separator())
          increaseUsAid(5)
          pause()
        }
        Some(Train)
      }
      else {
        logNoOp(ARVN, Train)
        None
      }
    }

    //  -------------------------------------------------------------
    //  Implement the ARVN Patrol instructions from the ARVN Trung cards.
    //  1. Use Move Priorities
    //  *  Assault using Remove Priorities.  First to unblock the route
    //     from Can Tho to Hue if possible.  Otherwise at random.
    //
    //  M48Patton_Shaded - After US/ARVN patrol NVA removes up to 2 cubes that moved (US to casualties)
    //  Mo_BodyCount     - Cost=0 AND +3 Aid per guerrilla removed
    def patrolOp(params: Params): Option[CoinOp] = {
      if (momentumInPlay(Mo_BodyCount) || !game.trackResources(ARVN) || game.arvnResources >= 3 ) {
        // Select a LoC Patrol destination candidate
        // ARVN Patrol never needs an activation roll
        val nextPatrolCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          val candidates = game.patrolSpaces filter (sp => !prohibited(sp.name))

          // No need to check activation since we are only trying LoCs
          if (candidates.nonEmpty)
            Some(ARVN_Bot.pickSpacePatrolDest(candidates).name)
          else
            None
        }
        if (!params.event)
          logOpChoice(ARVN, Patrol)
        if (!momentumInPlay(Mo_BodyCount) && game.trackResources(ARVN))
          decreaseResources(ARVN, 3)
        movePiecesToDestinations(ARVN, Patrol, ARVNCubes.toSet, false, params, maxDests = params.maxSpaces)(nextPatrolCandidate)
        val activatedSomething = activateGuerrillasOnLOCs(ARVN)
        // Add an assault on one LoC.  If this was a LimOp then the LoC must
        // be the selected destination.  If none was selected then we can pick any one.
        val assaultLoC = if (params.limOpOnly) {
          moveDestinations.headOption filter { name =>
            val sp = game.getSpace(name)
            sp.isLoC && assaultEffective(ARVN, params.cubeTreatment, params.vulnerableTunnels)(sp)
          }
        }
        else
          pickPatrolAssaultLoc(ARVN, params.cubeTreatment, params.vulnerableTunnels)

        assaultLoC foreach { name =>
          performAssault(ARVN, name, Params(free = true))
        }

        if (capabilityInPlay(M48Patton_Shaded) && movedPieces.size > 0)
          performM48Patton_Shaded(movedPieces)
        
        if (movedPieces.size > 0 || activatedSomething || assaultLoC.nonEmpty)
          Some(Patrol)
        else {
          log("\nSkipping ARVN Patrol as it would not be effective")
          log(separator())
          if (!momentumInPlay(Mo_BodyCount) && game.trackResources(ARVN))
            increaseResources(ARVN, 3)
          None
        }
      }
      else
        None
    }

    //  -------------------------------------------------------------
    //  Implement the ARVN Sweep instructions from the ARVN Trung cards.
    //  1. Use Move Priorities
    //     If Shaded Booby Traps then sweep in max 2 spaces
    //  Cobras_Unshaded   - 2 US/ARVN sweep spaces each remove 1 Active untunneled enemy
    //                               (troops then  guerrillas then bases)
    //  BoobyTraps_Shaded - Each sweep space, VC afterward removes 1 sweeping troop on
    //                      a roll 1-3 (US to casualties)
    //
    //  Sweep not allowed in Monsoon.
    def sweepOp(params: Params, actNum: Int): Option[CoinOp] = {
      val cubeTypes = sweepCubeTypes(ARVN, params.cubeTreatment)
      if (!params.event && game.inMonsoon)
        None
      else {
        // If Shaded Booby Traps then limit spaces to 2 unless this is a limOp
        // If we are tracking ARVN resources then we must also ensure
        // that we don't select more spaces that we can pay for.
        val mustPay = game.trackResources(ARVN) && !params.free
        val maxAfford: Option[Int] = if (mustPay) Some(game.arvnResources / 3) else None
        val maxTraps: Option[Int]  = if (capabilityInPlay(BoobyTraps_Shaded)) Some(2) else None
        val maxSweep = (maxAfford.toList ::: maxTraps.toList ::: params.maxSpaces.toList).sorted.headOption

        if (maxSweep.nonEmpty && maxSweep.get == 0)
          None  // Cannot afford to do any sweeping!
        else {
          val nextSweepCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
            val candidates = game.nonLocSpaces filter { sp =>
              params.spaceAllowed(sp.name) && !sp.isNorthVietnam && !prohibited(sp.name)
            }

            // Since a destination may get tossed out if there are not troops that can reach it
            // we don't use checkARVNActivation() because we don't want to pay for a space that
            // does not get used.  Instead we will pay for all of the selected destinations at the end
            // of the operation.
            lazy val activationOK = params.free || checkActivation(ARVN, needActivation, actNum)

            if (candidates.nonEmpty && activationOK)
              Some(pickSpaceSweepTransportDest(candidates).name)
            else
              None
          }

          if (!params.event)
            logOpChoice(ARVN, Sweep)

          movePiecesToDestinations(ARVN, Sweep, cubeTypes, false, params, maxDests = maxSweep)(nextSweepCandidate)
          val maxCobras = 2  // Unshaded cobras can be used in up to 2 spaces
          var numCobras = 0
          if (moveDestinations.nonEmpty) {
            // Activate guerrillas in each sweep destination
            for (name <- moveDestinations) {
              val activated = activateGuerrillasForSweep(name, ARVN, params.cubeTreatment)
              val traps     = checkShadedBoobyTraps(name, ARVN)
              val cobras    = numCobras < maxCobras && checkUnshadedCobras(name)
              if (cobras)
                numCobras += 1
              if (activated || traps || cobras)
                pause()
            }

            // Finally pay for the operation of applicable
            if (mustPay) {
              log(s"\nARVN pays for ${amountOf(moveDestinations.size, "sweep destination")}")
              log(separator())
              decreaseResources(ARVN, moveDestinations.size * 3)
            }
            Some(Sweep)
          }
          else {
            logNoOp(ARVN, Sweep)
            None
          }
        }
      }
    }


    //  Always called first, so no activation needed.
    //  Mo_BodyCount      - Cost=0 AND +3 Aid per guerrilla removed
    //  Mo_GeneralLansdale - Assault prohibited
    //  Returns the name of the space where assault occurred
    def assaultOneSpaceOpMostFirepower(params: Params): Option[String] = {
      // We do not check for activation because this will alwasy
      // be called for the first assault.  But we still need to check
      // resource availability.
      val hasCash = (params.free || !game.trackResources(ARVN) || game.arvnResources >= 3)
      if (generalLandsdale(ARVN) || !hasCash)
        None
      else {
        val candidates = game.spaces filter { sp =>
          assaultEffective(ARVN, params.cubeTreatment, params.vulnerableTunnels)(sp)
        }
        if (candidates.nonEmpty) {
          val MostFirepower = List(
            new HighestScore[Space](
            "Most ARVN Firepower",
            arvnFirepower(params.cubeTreatment)
          ))
          val sp = bestCandidate(candidates, MostFirepower)

          if (!params.event)
            logOpChoice(ARVN, Assault)
          performAssault(ARVN, sp.name, params)
          Some(sp.name)
        }
        else
          None
      }
    }

    //  -------------------------------------------------------------
    //  Implement the ARVN Assault instructions from the ARVN Trung cards.
    //  1. Assault in 1 space with most ARVN Firepower (optional)
    //  2. Assault using Remove Priorities.
    //
    //  Mo_BodyCount      - Cost=0 AND +3 Aid per guerrilla removed
    //  Mo_GeneralLansdale - Assault prohibited
    def assaultOp(params: Params, actNum: Int, previousAssaults: Set[String] = Set.empty): Option[CoinOp] = {
      if (generalLandsdale(ARVN))
        None
      else {
        var assaultSpaces = Set.empty[String]
        val maxAssault    = params.maxSpaces getOrElse NO_LIMIT

        def assaultIn(name: String): Unit = {
          performAssault(ARVN, name, params)
          assaultSpaces += name
          pause()
        }


        def nextAssault(): Unit = {
          val free           = params.free || momentumInPlay(Mo_BodyCount)
          val check          = previousAssaults.nonEmpty || assaultSpaces.nonEmpty
          lazy val activated = checkARVNActivation(check, actNum, free)
          lazy val candidates = game.spaces filter { sp =>
            !previousAssaults(sp.name) &&
            !assaultSpaces(sp.name)    &&
            assaultEffective(ARVN, params.cubeTreatment, params.vulnerableTunnels)(sp)
          }

          if (previousAssaults.size + assaultSpaces.size < maxAssault && candidates.nonEmpty && activated) {
            val sp = pickSpaceRemoveReplace(candidates)

            if (!params.event && previousAssaults.isEmpty && assaultSpaces.isEmpty)
              logOpChoice(ARVN, Assault)
            assaultIn(sp.name)
            nextAssault()
          }
        }

        // Some events dictate assaulting only
        // in specific spaces.
        if (params.assault.specificSpaces.nonEmpty) {
          for (name <- params.assault.specificSpaces)
            assaultIn(name)
        }
        else
          nextAssault()

        if (assaultSpaces.nonEmpty)
          Some(Assault)
        else {
          logNoOp(ARVN, Assault)
          None
        }
      }
    }

    //  -------------------------------------------------------------
    //  Implement the ARVN Govern instructions from the ARVN Trung cards.
    //  In each space Add Patronage unless US Aid is zero and the space would
    //  not add at least 2 Patronage.
    //
    //  Mo_TyphoonKate           - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
    //  MandateOfHeaven_Shaded   - ARVN Govern is maximum 1 space
    //  MandateOfHeaven_Unshaded - 1 Govern space may transfer Aid to Patronage without shifting support
    //  RVN_Leader_YoungTurks    - Each ARVN Govern adds +2 Patronage
    def governActivity(params: Params): Boolean = {
      val kate        = momentumInPlay(Mo_TyphoonKate)
      val mandates1   = capabilityInPlay(MandateOfHeaven_Shaded)
      val maxGovern   = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                        else if (kate || mandates1) 1 else 2
      var governSpaces = List.empty[GovernSpace]
                        
      def alreadyPicked(sp: Space) = governSpaces exists (_.sp.name == sp.name)
      
      val prohibited = (sp: Space) =>
        !params.spaceAllowed(sp.name) ||
        trainingSpaces(sp.name) ||
        alreadyPicked(sp) ||
        sp.name == Saigon
        
      
      val isAidCandidate = (sp: Space) =>
        !prohibited(sp)   &&
        sp.canHaveSupport &&
        sp.coinControlled &&
        sp.support > Neutral
        
      val isPatronageCandidate = (sp: Space) =>
        isAidCandidate(sp) &&
        sp.pieces.totalOf(ARVNCubes) > sp.pieces.totalOf(USTroops)

      val isCandidate = (sp: Space) => isAidCandidate(sp) || isPatronageCandidate(sp)
      
      def getCandidates = (game.nonLocSpaces filter isCandidate) map (sp => GovernSpace(sp, isPatronageCandidate(sp)))
      
      // In order to implement the Mandate of Heaven Unshaded capability
      // we must first pick our spaces, then determine which space will
      // use the capability

      // First pick our spaces
      for (i <- 1 to maxGovern) {
        val candidates = getCandidates
        if (candidates.nonEmpty) {
          governSpaces = governSpaces :+ pickSpaceGovern(candidates)
        }
      }

      case class GovernAction(name: String, addPatronage: Boolean)

      // Bot will only increase US Aid if aid is currently at zero and
      // the space will not add at least 2 patronage.  So there will only
      // be a maximum of one Aid action UNLESS all of the canidate spaces
      // are not valid spaces for inceasing patronage.
      def getGovernActions(): List[GovernAction] = {
        if ((governSpaces exists (_.patronageOK)) == false) {
          //  No spaces have more ARNV cubes than US troops so just add aid
          governSpaces map (entry => GovernAction(entry.sp.name, addPatronage = false))
        }
        else if (game.usAid > 0) {
          //  Add patronage in each space unless there less ARVN cubes than US cubes
          governSpaces map (entry => GovernAction(entry.sp.name, addPatronage = entry.patronageOK))
        }
        else {
          //  US Aid is at zero so we try to Add Aid but do not cannaballize a space that would
          //  add 2 or more patronage.
          val forcePatronage = (entry: GovernSpace) => entry.patronageOK && entry.sp.population > 1
          
          val patronageOnly = governSpaces forall forcePatronage
          
          if (governSpaces forall forcePatronage)
            governSpaces map (entry => GovernAction(entry.sp.name, addPatronage = true))
          else {
            // For adding Aid, first pick a space that cannot add patronage.
            // If that does not exist then pick one that will only add 1
            val noPatronage  = governSpaces filterNot (_.patronageOK)
            val aidSpace     =  (noPatronage ::: (governSpaces filter (_.sp.population == 1))).head
            val otherSpaces  = governSpaces filter (entry => entry.sp.name != aidSpace.sp.name)
            val aidAction    = GovernAction(aidSpace.sp.name, addPatronage = false)
            val otherActions = otherSpaces map (entry => GovernAction(entry.sp.name, addPatronage = entry.patronageOK))
            aidAction :: otherActions
          }
        }
      }

      if (governSpaces.nonEmpty) {
        val governActions = getGovernActions()
        val mandateSpace  = if (capabilityInPlay(MandateOfHeaven_Unshaded)) {
          val patronageSpaces = governActions filter (_.addPatronage)
          patronageSpaces.sortBy(x => game.getSpace(x.name).support.value).map(_.name).headOption
        }
        else
          None

        if (!params.event)
          logSAChoice(ARVN, Govern)

        for (GovernAction(name, addPatronage) <- governActions) {
          val sp = game.getSpace(name)

          log(s"\nARVN Governs in ${sp.name} (population: ${sp.population})")
          log(separator())

          loggingPointsChanges {
            if (addPatronage) {
              val num = (game.usAid min sp.population)

              log("Transfer population value from Aid to Patronage")
              decreaseUsAid(num)
              increasePatronage(num)
              mandateSpace match {
                case Some(mandate) if mandate == name =>
                  log(s"No shift in support. Using [$MandateOfHeaven_Unshaded]")
                case _ =>
                  decreaseSupport(sp.name, 1)
              }
            }
            else {
              log("Add 3 times population value to Aid")
              increaseUsAid(sp.population * 3)
            }

            //  Young turks always applies if in play
            if (isRVNLeader(RVN_Leader_YoungTurks)) {
              log(s"$RVN_Leader_YoungTurks leader effect triggers")
              increasePatronage(2)
            }
          }
          pause()
        }
        logEndSA(ARVN, Govern)
        true
      }
      else
        false
    }

    //  -------------------------------------------------------------
    //  Implement the ARVN Transport instructions from the ARVN Trung cards.
    //  1. Use Move Priorities
    //  Shaded Armored Cavalry is not handle here because it must happen
    //  after the operation if complete.  It is called in the Trung card
    //  execute functions.
    //
    // Mo_TyphoonKate          - prohibits air lift, transport, and bombard, all other special activities are max 1 space
    // ArmoredCavalry_Shaded   - Transport Rangers only (NO TROOPS)
    // RVN_Leader_NguyenKhanh  - Transport uses max 1 LOC space - Enforced by getTransportOrigins()
    def transportActivity(params: Params): Boolean = {

      if (!params.event && momentumInPlay(Mo_TyphoonKate))
        false  // Typhoon Kate prohibits transport
      else {
        val savedMovedDestinations = moveDestinations
        val savedMovedPieces       = movedPieces
        var flippedARanger         = false

        val moveTypes: Set[PieceType] =
          if (capabilityInPlay(ArmoredCavalry_Shaded)) Rangers.toSet else (ARVNTroops::Rangers).toSet

        val nextTransportCandidate = (_: Boolean, _: Boolean, prohibited: Set[String]) => {
          val candidates = game.spaces filter { sp =>
            params.spaceAllowed(sp.name) &&
            !sp.isNorthVietnam &&
            !prohibited(sp.name)
          }

          // No activation check for Transport
          if (candidates.nonEmpty)
            Some(pickSpaceSweepTransportDest(candidates).name)
          else
            None
        }

        moveDestinations = Vector.empty
        movedPieces.reset()
        if (!params.event)
          logSAChoice(ARVN, Transport)
        movePiecesToDestinations(ARVN, Transport, moveTypes, false, params, maxPieces = 6)(nextTransportCandidate)

        transportDestinations = moveDestinations
        moveDestinations      = savedMovedDestinations
        if (transportDestinations.isEmpty)
          log(s"\nNo spaces found for $ARVN $Transport")

        log("\nFlip all Rangers underground")
        log(separator())
        // Flip all rangers underground
        for (sp <- game.spaces; if sp.pieces.has(Rangers_A)) {
          hidePieces(sp.name, sp.pieces.only(Rangers_A))
          flippedARanger = true
        }
        if (!flippedARanger)
          log("There are no Active Rangers on the map")

        if (transportDestinations.nonEmpty || flippedARanger) {
          logEndSA(ARVN, Transport)
          true
        }
        else
          false
      }
    }


    // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
    def raidActivity(params: Params): Boolean = {
      val maxRaid = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (momentumInPlay(Mo_TyphoonKate)) 1 else 2
      // Underground guerrillas can be targeted, Tunnels cannot and
      // base only if no other Insurgent pieces (forces?) exist
      val hasRaidTarget = (sp: Space) =>
        sp.pieces.has(InsurgentForces) || sp.pieces.has(InsurgentNonTunnels)

      val canRemoveBase = (sp: Space) =>
        sp.pieces.totalOf(InsurgentForces) < 2 &&
        sp.pieces.has(InsurgentNonTunnels)

      val hasAdjacentUndergroundRangers = (sp: Space) =>
        spaces(getAdjacent(sp.name)) exists (_.pieces.has(Rangers_U))

      val MoveRangerPriorities = List(
        new BooleanPriority[Space]("Not with vulnerable base", (!canRemoveBase(_))),
        new BooleanPriority[Space]("Not with vulnerable force", (!hasRaidTarget(_)))
      )
      var raidSpaces = Set.empty[String]

      def nextRaid(getCandidates: () => List[Space]): Unit = {
        val candidates = getCandidates() filterNot (sp => raidSpaces.contains(sp.name))
        if (raidSpaces.size < maxRaid && candidates.nonEmpty) {
          val baseCandidates = candidates filter canRemoveBase
          val sp = if (baseCandidates.nonEmpty)
            pickSpaceRemoveReplaceSpecific(baseCandidates, raid = true)
          else
            pickSpaceRemoveReplaceSpecific(candidates, raid = true)

          val forcesPresent = sp.pieces.only(InsurgentForces)
          val basesPresent  = sp.pieces.only(InsurgentNonTunnels)
          val numForces     = 2 min forcesPresent.total
          val numBases      = (2 - numForces) min basesPresent.total
          val deadForces    = selectEnemyRemoveReplaceActivate(forcesPresent, numForces)
          val deadBases     = selectEnemyRemoveReplaceActivate(basesPresent, numBases)
          val uRanger       = Pieces(rangers_U = 1)

          if (!params.event && raidSpaces.isEmpty)
            logSAChoice(ARVN, Raid)

          log(s"\nARVN conducts raid in ${sp.name}")
          log(separator())
          // Move ranger in if necessary
          if (!sp.pieces.has(Rangers_U)) {
            val adjacent = spaces(getAdjacent(sp.name)) filter (_.pieces.has(Rangers_U))
            if (adjacent.nonEmpty) {
              val source = bestCandidate(adjacent, MoveRangerPriorities)
              movePieces(uRanger, source.name, sp.name)
            }
          }
          revealPieces(sp.name, uRanger)
          removeToAvailable(sp.name, deadForces + deadBases)
          raidSpaces += sp.name
          pause()
          nextRaid(getCandidates)
        }
      }

      def raidInPlaceCandidates(): List[Space] =
        game.spaces filter { sp =>
          params.spaceAllowed(sp.name) &&
          !sp.isNorthVietnam &&
          hasRaidTarget(sp) &&
          sp.pieces.has(Rangers_U)
        }

      def raidAdjacentCandidates(): List[Space] =
        game.spaces filter { sp =>
          params.spaceAllowed(sp.name) &&
          !sp.isNorthVietnam &&
          hasRaidTarget(sp) &&
          hasAdjacentUndergroundRangers(sp)
        }

      nextRaid(raidInPlaceCandidates _)
      nextRaid(raidAdjacentCandidates _)
      if (raidSpaces.nonEmpty)
        logEndSA(ARVN, Raid)
      raidSpaces.nonEmpty
    }
  }


  // ================================================================
  // NVA Specific code
  // ================================================================
  object NVA_Bot {

    def threeD6_LE_AvailNVATroops = trungDiceCheck(3, game.availablePieces.totalOf(NVATroops), CMP_LE, s"Available NVA Troops")
    def twoD6_LE_AvailNVAGuerrillas = trungDiceCheck(2, game.availablePieces.totalOf(NVAGuerrillas), CMP_LE, s"Available NVA Guerrillas")
    def d3_GE_Trail = trungD3Check(game.trail, CMP_GE, "the Trail")
    
    // Coup Round Redeployment
    // o Keep NVA pieces to exceed COIN+VC if possible
    //   otherwise keep keep Insurgent pieces to equal or exceed COIN pieces
    // o Then move Troops as follows:
    //   - Each cube may only move once
    //   - Select destination spaces outside N. Vietnam with a base using Place Troops priorities
    //   - Select origin spaces:
    //     1.  In South Vietnam with with no NVA base and no NVA control
    //     2.  In N. Vietnam/Laos/Cambodia with most Troops and not already
    //         selected as a destination
    //   - Roll 3d6.  Get this many troops to each destination using one or
    //                more origins.
    def redeployNVATroops(): Unit = {
      val destinationNames = nvaRedeployTroopDestinations()
      var destsUsed      = Set.empty[String]
      val eligibleTroops = new MovingGroups()
      val targetNumber   = rollDice(3)

      val destCandidate = (sp: Space) =>
        !destsUsed(sp.name)     &&
        !sp.isNorthVietnam      &&
        sp.pieces.has(NVABases) &&
        sp.pieces.totalOf(NVATroops) < targetNumber

      val isSouthOrigin = (sp: Space) =>
        isInSouthVietnam(sp.name)        &&
        eligibleTroops(sp.name).nonEmpty &&
        !(sp.pieces.has(NVABases) || sp.nvaControlled)

      val isOtherOrigin = (sp: Space) =>
        !destsUsed(sp.name)   &&
        (sp.isNorthVietnam || isInLaosCambodia(sp.name)) &&
        eligibleTroops(sp.name).nonEmpty

      val otherPriorities = List(
        new HighestScore[Space]("Most Troops", sp => sp.pieces.totalOf(NVATroops))
      )

      def getOrigin: Option[String] = {
        val southOrigins = game.spaces filter isSouthOrigin
        val otherOrigns  = game.spaces filter isOtherOrigin
        if (southOrigins.nonEmpty || otherOrigns.nonEmpty) {
          val sp = if (southOrigins.nonEmpty)
            shuffle(southOrigins).head
          else
            bestCandidate(otherOrigns, otherPriorities)
          Some(sp.name)
        }
        else
          None
      }

      def nextDestination(): Unit = {
        val candidates = game.spaces filter destCandidate

        if (candidates.nonEmpty) {
          val dest       = pickSpacePlaceTroops(candidates).name
          def troopsAtDest = game.getSpace(dest).pieces.totalOf(NVATroops)

          def moveFromOrigins(): Boolean = if (troopsAtDest < targetNumber) {
            val troopsNeeded = targetNumber - troopsAtDest
            getOrigin match {
              case None => true  // No more origins
              case Some(origin) =>
                val num = eligibleTroops(origin).total min troopsNeeded
                val troops = Pieces(nvaTroops = num)
                movePieces(troops, origin, dest)
                destsUsed += dest
                eligibleTroops.remove(origin, troops)
                moveFromOrigins()
            }
          }
          else
            false // Origins not exhausted, just moved max troops

          // Update redepoyDests first so that our destination
          // will not be selected as an origin!
          destsUsed += dest
          val originsExhausted = moveFromOrigins()

          if (!originsExhausted)
            nextDestination()
        }
      }

      if (nvaCanRedeployTroops) {
        // First determine which Troops are eligible to move
        // o Keep NVA pieces to exceed COIN+VC if possible
        //   otherwise keep keep Insurgent pieces to equal or exceed COIN pieces
        for (sp <- game.spaces; if sp.pieces.has(NVATroops)) {
          val numTroops = sp.pieces.totalOf(NVATroops)
          val numNVA    = sp.pieces.totalOf(NVAPieces)
          val numVC     = sp.pieces.totalOf(VCPieces)
          val numCOIN   = sp.pieces.totalOf(CoinPieces)

          val numEligible = if (numNVA > numCOIN + numVC)
            (numNVA - (numCOIN + numVC) - 1) min numTroops
          else if (numNVA + numVC >= numCOIN)
            (numNVA + numVC - numCOIN) min numTroops
          else
            numTroops

          if (numEligible > 0)
            eligibleTroops.set(sp.name, Pieces(nvaTroops = numEligible))
        }

        // Now start choosing destinations and moving troops.
        log(s"Rolling 3d6 for number of troops per destination: $targetNumber")
        nextDestination()
      }
      else
        log("There are no NVA Troops that can Redeploy")
    }

    val inOrAdjacentToLaosCambodia = Set(
      AnLoc,
      CentralLaos,
      SouthernLaos,
      NortheastCambodia,
      TheFishhook,
      TheParrotsBeak,
      Sihanoukville,
      NorthVietnam,
      QuangTri_ThuaThien,
      QuangNam,
      QuangTin_QuangNgai,
      BinhDinh,
      Pleiku_Darlac,
      PhuocLong,
      QuangDuc_LongKhanh,
      TayNinh,
      KienPhong,
      KienGiang_AnXuyen,
      LOC_Hue_KheSanh,
      LOC_DaNang_DakTo,
      LOC_Kontum_DakTo,
      LOC_Saigon_AnLoc_BanMeThuot,
      LOC_CanTho_ChauDoc
    )

    // When the trail is at 4
    // NVA march considers all spaces that are in or adjacent
    // to Laos/Cambodia to be adjacent.

    def getNVAAdjacent(name: String): Set[String] = {

      if (game.trail == 4 && inOrAdjacentToLaosCambodia(name))
        getAdjacent(name) ++ inOrAdjacentToLaosCambodia - name
      else
        getAdjacent(name)
    }

    def sixTroopsWithCOINTroopsOrBase: Boolean = {
      val result = game.spaces exists { sp =>
        sp.pieces.totalOf(NVATroops) >= 6 &&
        (sp.pieces.has(CoinTroops) || sp.pieces.has(CoinBases))
      }
      logCheck(result, "6+ NVA Troops in any space with COIN Troops or COIN Base?")
    }

    def sixTroopsWithCOINPieces: Boolean = {
      val result = game.spaces exists { sp =>
        sp.pieces.totalOf(NVATroops) >= 6 &&
        sp.pieces.has(CoinPieces)
      }
      logCheck(result, "6+ NVA Troops in any space with COIN Pieces?")
    }

    def eightTroopsOutsideSouth: Boolean = {
      val result = game.spaces exists { sp =>
        sp.pieces.totalOf(NVATroops) >= 8 &&
        isOutsideSouth(sp.name)
      }
      logCheck(result, "8+ NVA Troops in a space outside the South?")
    }

    def pop2WithoutCoinControl: Boolean = {
      val result = game.spaces exists (sp => sp.population == 2 && !sp.coinControlled)
      logCheck(result, "Any 2-Pop space without COIN Control?")
    }

    def atleastTwentyNVATroopsOnMap: Boolean = {
      val result = game.totalOnMap(_.pieces.totalOf(NVATroops)) >= 20
      logCheck(result, "20+ NVA Troops on the map?")
    }

    def undergroundGuerrillasWithSupport: Boolean = {
      val result = game.nonLocSpaces exists (sp => sp.support > Neutral && sp.pieces.has(NVAGuerrillas_U))
      logCheck(result, "Underground NVA Guerrillas in space with Support?")
    }
    
    def nvaMarchAmbushCandidates: List[String] = marchAmbushCandidates(NVA, needUnderground = true)
    
    def canFollowMarchWithAmbush: Boolean = { 
      val result = nvaMarchAmbushCandidates.nonEmpty && !momentumInPlay(Mo_Claymores)
      logCheck(result, "Can Ambush in March destinations?")
    }
    
    val MostUndergroundGuerrillas = List(
      new HighestScore[Space](
        "Most Underground NVA Guerrillas",
        sp => sp.pieces.totalOf(NVAGuerrillas_U)
      )
    )

    // NVA Spaces Priorities: Place Bases or Tunnels
    def pickSpacePlaceBases(candidates: List[Space]): Space = {

      val priorities = List(
        CityProvinceMostNVAGuerrillas,
        LaosCambodiaWithCoinControl,
        MostPopulation,
        CityProvinceWithoutUSBase,
        CityProvinceHighestAjacentPopulation
      )

      botDebug(s"\nNVA Select space (Place Bases or Tunnels): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place NVA Troops
    def pickSpacePlaceTroops(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,               CityProvinceMostNVAGuerrillas),
        filterIf(true,               MostNVABases),
        filterIf(true,               LaosCambodiaWithCoinControl),
        filterIf(true,               CityProvinceInSouthWithoutNVAControl),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(true,               MostPopulation),
        filterIf(true,               CityProvinceWithoutUSBase),
        filterIf(game.isHuman(VC),   HasVCBase),
        filterIf(true,               CityProvinceFewestNonNVAPieces),
        filterIf(true,               CityProvinceHighestAjacentPopulation)
      ).flatten

      botDebug(s"\nNVA Select space (Place NVA Troops): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place NVA Guerrilllas
    def pickSpacePlaceGuerrillas(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,               VulnerableNVABase),
        filterIf(true,               MostNVABases),
        filterIf(true,               LaosCambodiaWithCoinControl),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(true,               MostPopulation),
        filterIf(game.isHuman(VC),   HasVCBase),
        filterIf(true,               CityProvinceFewestNonNVAPieces),
        filterIf(true,               CityProvinceHighestAjacentPopulation)
      ).flatten

      botDebug(s"\nNVA Select space (Place NVA Guerrilllas): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: March Destinations
    def pickSpaceMarchDest(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,               LaosCambodiaWithCoinControl),
        filterIf(game.trail < 4,     AdjacentToMostNVATroopsYetToMarch),
        filterIf(true,               CityProvinceInSouthWithoutNVAControl),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(true,               MostPopulation),
        filterIf(true,               CityProvinceWithoutUSBase),
        filterIf(game.isHuman(VC),   HasVCBase),
        filterIf(true,               CityProvinceFewestNonNVAPieces)
      ).flatten

      botDebug(s"\nNVA Select space (March Destinations): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place Terror
    def pickSpacePlaceTerror(candidates: List[Space]): Space = {

      val priorities = List(
        MostTotalSupport,
        VulnerableNVABase,
        CityProvinceMostNVAGuerrillas
      )

      botDebug(s"\nNVA Select space (Place Terror): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Remove or Replace
    def pickSpaceRemoveReplace(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,               LaosCambodiaWithCoinControl),
        filterIf(true,               CityProvinceInSouthWithoutNVAControl),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(true,               MostPopulation),
        filterIf(true,               CityProvinceFewestNonNVAPieces),
        filterIf(true,               CityProvinceMostCoinCubes)
      ).flatten

      botDebug(s"\nNVA Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

        //  Returns the space selected for Ambush and the space being targeted by the Ambush
    //  The targeted space can be the same as the ambushing space or of the ambush space
    //  is a LoC the targeted space can be an adjacent space.
    def pickSpaceAndTargetForAmbush(candidates: List[Space]): (Space, Space) = {
      // First find all possible target spaces and pick the best one.
      val targetCandidates = spaces(candidates flatMap (sp => ambushTargets(sp.name)))
      val targetSpace      = pickSpaceRemoveReplace(targetCandidates)

      // Now find all of the original candidate spaces that can target the
      // target space and pick the one with the must Undeground Guerrillas.
      val reachableCandidates = candidates filter (sp => ambushTargets(sp.name) contains targetSpace.name)
      val ambushSpace = bestCandidate(reachableCandidates, MostUndergroundGuerrillas)

      (ambushSpace, targetSpace)
    }

    //  -------------------------------------------------------------
    //  Implement the NVA Rally Instructions from the NVA Trung cards.
    //  1. Place Bases where 3+ NVA Guerrillas
    //  2. Select spaces using Place Guerrillas
    //  3. Improve the Trail for free (even if already failed an activation roll)
    //  Mo_McNamaraLine - prohibits trail improvement by rally
    //  AAA_Unshaded    - Rally that Improves Trail may select 1 space only
    //  SA2s_Shaded     - NVA Rally improves Trail 2 boxes instead of 1
    def rallyOp(params: Params, actNum: Int): Option[InsurgentOp] = {
      var rallySpaces      = Set.empty[String]
      val cadres       = capabilityInPlay(Cadres_Shaded)
      val aaa          = capabilityInPlay(AAA_Unshaded)
      val maxRally     = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (aaa && game.trail <= 2) 1 // Nva priorites trail improvement if it is low
                    else NO_LIMIT
      def rallied      = rallySpaces.nonEmpty
      def canRally     = rallySpaces.size < maxRally && (params.free || checkActivation(NVA, rallied, actNum))
      val canRallyBase = (sp: Space) =>
        !rallySpaces(sp.name)       &&
        params.spaceAllowed(sp.name) &&
        !params.rally.guerrillasOnly &&
        sp.support < PassiveSupport &&
        sp.totalBases < 2           &&
        sp.pieces.totalOf(NVAGuerrillas) > 2

      val canRallyGuerrillas = (sp: Space) =>
        !rallySpaces(sp.name) &&
        params.spaceAllowed(sp.name) &&
        sp.support < PassiveSupport

      //  Return true if we can continue (no failed activation roll)
      def rallyToPlaceBase(candidates: List[Space]): Boolean = {
        if (game.availablePieces.has(NVABase) && candidates.nonEmpty) {
          if (canRally) {
            val sp       = pickSpacePlaceBases(candidates)
            val toRemove = selectFriendlyRemoval(sp.pieces.only(NVAGuerrillas), 2)
            log(s"\n$NVA selects ${sp.name} for Rally")
            log(separator())
            loggingControlChanges {
              removeToAvailable(sp.name, toRemove)
              placePieces(sp.name, Pieces(nvaBases = 1))
            }
            rallySpaces = rallySpaces + sp.name
            pause()
            rallyToPlaceBase(candidates filterNot (_.name == sp.name))
          }
          false // Failed activation
        }
        else
          true  // No failed activation, just ran out of candidates
      }

      //  Return true if we can continue (no failed activation roll)
      def rallyToPlaceGuerrillas(candidates: List[Space]): Boolean = {
        if (game.availablePieces.has(NVAGuerrillas_U) && candidates.nonEmpty) {
          if (canRally) {
            val sp         = pickSpacePlaceGuerrillas(candidates)
            val numToPlace = if (sp.pieces.has(NVABases))
              (sp.pieces.totalOf(NVABases) + game.trail) min game.availablePieces.totalOf(NVAGuerrillas_U)
            else
              1
            log(s"\n$NVA selects ${sp.name} for Rally")
            log(separator())
            placePieces(sp.name, Pieces(nvaGuerrillas_U = numToPlace))
            rallySpaces = rallySpaces + sp.name
            pause()
            rallyToPlaceGuerrillas(candidates filterNot (_.name == sp.name))
          }
          false // Failed activation
        }
        else
          true  // No failed activation, just ran out of candidates

      }

      def improveTrailForFree():  Unit = {
        if (momentumInPlay(Mo_McNamaraLine))
          log(s"\nNo trail improvement. Momentum: $Mo_McNamaraLine")
        else if (game.trail == TrailMax)
          log("\nNo trail improvement. The Trail is at 4.")
        else if (aaa && rallySpaces.size > 1)
            log(s"\nNo trail improvement. Rallied in more than one space. [$AAA_Unshaded]")
        else {
          val maxNum = if (capabilityInPlay(SA2s_Shaded)) 2 else 1
          val num    = maxNum min (TrailMax - game.trail)
          val msg = if (num == 2) s" [$SA2s_Shaded]" else ""

          log(s"\nNVA improves the trail$msg")
          log(separator())
          improveTrail(num)
        }
      }


      if (!params.event)
        logOpChoice(NVA, Rally)
      rallyToPlaceBase(game.nonLocSpaces filter canRallyBase) &&
      rallyToPlaceGuerrillas(game.nonLocSpaces filter canRallyGuerrillas)
      improveTrailForFree()

      if (rallied)
        Some(Rally)
      else {
        logNoOp(NVA, Rally)
        None
      }
    }

    //  -------------------------------------------------------------
    //  Implement the NVA March Instructions from the NVA Trung cards.
    //    March using Move Priorities
    //    1. Select 1 LoC adjacent adjacent to US without NVA Guerrillas (optional)
    //    2. Select 1 space in Laos/Cambodia without NVA Base adjacent
    //       to the most NVA Guerrillas (Optional)
    //    3. Select spaces using March Destinations
    //
    //    March not allowed in Monsoon.
    //  -------------------------------------------------------------
    def marchOp(params: Params, actNum: Int, withLoC: Boolean, withLaosCambodia: Boolean, easterOffensive: Boolean = false): Option[InsurgentOp] = {
      if (!params.event && game.inMonsoon)
        None
      else {
        // This is only used for easterOffensive
        val nextLocAdjacentToSaigonCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          val qualifies = (sp: Space) =>
            params.spaceAllowed(sp.name)  &&
            !prohibited(sp.name)          &&
            sp.isLoC                      &&
            !sp.pieces.has(CoinPieces)    &&
            areAdjacent(sp.name, Saigon)
          val candidates = game.locSpaces filter qualifies

          if (candidates.nonEmpty)
            Some(NVA_Bot.pickSpaceMarchDest(candidates).name)
          else
            None
        }

        // Select a LoC march destination candidate
        // Once we have marched to one LoC successfully we are done.
        val nextLoCCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          val qualifies = (sp: Space) =>
            params.spaceAllowed(sp.name)  &&
            !prohibited(sp.name)          &&
            !sp.pieces.has(NVAGuerrillas) &&
            numAdjacentPieces(sp, USForces) > 0
          val candidates = game.locSpaces filter qualifies

          // No need to check activation since we are only trying LoCs
          if (!lastWasSuccess && candidates.nonEmpty)
            Some(NVA_Bot.pickSpaceMarchDest(candidates).name)
          else
            None
        }

        // Select a Laos/Cambodia march destination candidate
        // Once we have marched to one Laos/Cambodia space successfully we are done.
        val nextLaosCambodiaCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          val LaosCambodiaPriorities = List(
            new HighestScore[Space](
              "Most Adjacent NVA Guerrillas",
              sp => numAdjacentPieces(sp, NVAGuerrillas)
            )
          )
          val qualifies = (sp: Space) =>
            params.spaceAllowed(sp.name) &&
            !prohibited(sp.name)      &&
            sp.totalBases < 2         &&
            !sp.pieces.has(NVABases)  &&
            isInLaosCambodia(sp.name) &&
            (spaces(getNVAAdjacent(sp.name)) exists (_.pieces.has(NVAGuerrillas)))
          val candidates = game.locSpaces filter qualifies
          lazy val activationOK = params.free || checkActivation(NVA, needActivation, actNum)

          if (!lastWasSuccess && candidates.nonEmpty && activationOK)
            Some(bestCandidate(candidates, LaosCambodiaPriorities).name)
          else
            None
        }

        val nextGenericCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          val candidates = game.spaces filter { sp =>
            params.spaceAllowed(sp.name)  &&
            !prohibited.contains(sp.name)
          }
          lazy val activationOK = params.free || checkActivation(NVA, needActivation, actNum)

          if (candidates.nonEmpty && activationOK)
            Some(NVA_Bot.pickSpaceMarchDest(candidates).name)
          else
            None
        }

        val marchers: Set[PieceType] = if (params.march.onlyTypes.nonEmpty)
          params.march.onlyTypes
        else
          NVAForces.toSet

        if (!params.event)
          logOpChoice(NVA, March)

        // If Easter Offensive pivotal event then we start by marching max troops to
        // LoCs adjacent to Saigon.
        if (easterOffensive)
          movePiecesToDestinations(NVA, EasterTroops, Set(NVATroops), false, params)(nextLocAdjacentToSaigonCandidate)

        // Never first need activation for LoCs
        if (withLoC)
          movePiecesToDestinations(NVA, March, marchers, false, params, maxDests = params.maxSpaces)(nextLoCCandidate)

        // No need for first activation after LoCs
        // If we march to Laos/Cambodia then we will need first activate check
        // for the generic destinations
        val needActivate = if (withLaosCambodia)
          movePiecesToDestinations(NVA, March, marchers, false, params, maxDests = params.maxSpaces)(nextLaosCambodiaCandidate)
        else
          false

        movePiecesToDestinations(NVA, March, marchers, needActivate, params, maxDests = params.maxSpaces)(nextGenericCandidate)

        if (moveDestinations.nonEmpty)
          Some(March)
        else {
          logNoOp(NVA, March)
          None
        }
      }
    }

    // Used to determine which Space (if any) will use the
    // PT76 Shaded Capability during attacks
    //  The delta of pieces killed if pt76_shaded used in the space
    case class PT76_Space(name: String, nvaTroops: Int, delta: Int)
    var pt76_space: Option[PT76_Space] = None

    def clearPT76Shaded(): Unit = {
      pt76_space = None
    }
    // If PT76 shaded is in effect and was found to be useful
    // then apply it now.
    def applyPt76Shaded(): Unit = {
      pt76_space foreach { pt76 =>
        val sp           = game.getSpace(pt76.name)
        val coinPieces   = sp.pieces.only(CoinPieces)
        val deadPieces   = selectRemoveEnemyCoinBasesLast(coinPieces, pt76.delta)
        val attritionNum = deadPieces.totalOf(USTroops::USBase::Nil) min pt76.delta
        val attrition    = Pieces(nvaTroops = attritionNum)
        val disp         = pluralize(pt76.delta, "piece")

        log(s"\nNVA elects to use [$PT76_Shaded] in ${pt76.name}")
        log(separator())
        log(s"This eliminates ${pt76.delta} more COIN $disp")
        loggingControlChanges {
          removePieces(pt76.name, deadPieces)
          removeToAvailable(pt76.name, attrition, Some("Attrition:"))
        }
      }
    }


    def performAttack(name: String, useTroops: Boolean): Unit = {
      val pt76_unshaded = capabilityInPlay(PT76_Unshaded)
      val sp            = game.getSpace(name)
      val guerrillas    = sp.pieces.only(NVAGuerrillas)
      val numTroops     = (if (pt76_unshaded) sp.pieces.totalOf(NVATroops) - 1 else sp.pieces.totalOf(NVATroops)) max 0
      val coinPieces    = sp.pieces.only(CoinPieces)
      val toActivate    = if (useTroops) Pieces() else guerrillas.only(NVAGuerrillas_U)
      val num           = (if (useTroops) (numTroops / 2) else 2) min coinPieces.total

      // If PT76 shaded in effect check to see if it
      // would be effective in this space and this space
      // has more NVA Troops than any previous effective space.
      if (useTroops && capabilityInPlay(PT76_Shaded)) {
        val prevTroops = pt76_space map (_.nvaTroops) getOrElse 0
        val prevDelta  = pt76_space map (_.delta) getOrElse 0
        val delta = (numTroops min coinPieces.total) - num

        if (delta > 0 && (numTroops > prevTroops || (numTroops == prevTroops && delta > prevDelta)))
          pt76_space = Some(PT76_Space(sp.name, numTroops, delta))
      }

      val die        = d6
      val numGs      = guerrillas.total
      val success    = useTroops || die <= numGs
      val forceDisplay = if (useTroops) "Troops" else "Guerrillas"

      log(s"\n$NVA Attacks in ${sp.name} using $forceDisplay")
      log(separator())
      revealPieces(sp.name, toActivate)
      if (!useTroops)
        log(s"Die roll (${amountOf(numGs, "Guerrilla")}): $die [${if (success) "Success!" else "Failure"}]")

      if (pt76_unshaded && sp.pieces.has(NVATroops))
        removeToAvailable(sp.name, Pieces(nvaTroops = 1), Some(s"$PT76_Unshaded triggers:"))

      if (success) {
        // Bases only removed if no other Coin forces (of either faction)
        val deadPieces = selectRemoveEnemyCoinBasesLast(coinPieces, num)
        val attrition  = if (useTroops) {
          val attritionNum  = deadPieces.totalOf(USTroops::USBase::Nil) min numTroops
          Pieces(nvaTroops = attritionNum)
        }
        else {
          val attritionNum  = deadPieces.totalOf(USTroops::USBase::Nil) min guerrillas.total
          Pieces(nvaGuerrillas_A = attritionNum) // All NVA guerrillas will have been activated
        }

        loggingControlChanges {
          removePieces(sp.name, deadPieces)
          removeToAvailable(sp.name, attrition, Some("Attrition:"))
        }
      }
    }


    //  -------------------------------------------------------------
    //  Implement the NVA Attack Instructions from the NVA Trung cards.
    //  Select spaces using the Remove Space Priority
    //  1. Select spaces with 2+ NVA Troops
    //  2. Ambush in 2 spaces (if able)
    //  3. Select spaces with 4+ NVA Guerrillas
    //
    //  Return Some(Attack) if we attack/ambush at least one space
    //         true if we included the ambush special activity
    //
    //  PT76_Unshaded  - If Attack, then NVA must first remove 1 Troop (if one is present)
    //  PT76_Shaded    - In one attack space remove 1 enemy per NVA Troop
    //  -------------------------------------------------------------
    def attackOp(params: Params, actNum: Int, addAmbush: Boolean): Option[(InsurgentOp, Boolean)] = {
      val threePlusGuerrillasNoVCBase = (sp: Space) => {
        sp.pieces.totalOf(VCGuerrillas) > 2 &&
        !sp.pieces.has(VCBases)
      }
      val maxAttacks = params.maxSpaces getOrElse NO_LIMIT
      var attackSpaces   = Set.empty[String]

      clearPT76Shaded()

      // Return true if we can continue attacking (ie. did not fail and activation roll)
      def nextAttack(candidates: List[Space], useTroops: Boolean, needActivation: Boolean): Boolean = {
        if (attackSpaces.size < maxAttacks && candidates.nonEmpty) {
          if (params.free || checkActivation(NVA, needActivation, actNum)) {
            val name = pickSpaceRemoveReplace(candidates).name

            performAttack(name, useTroops)
            attackSpaces += name
            pause()
            nextAttack(candidates filterNot (_.name == name), useTroops, needActivation = true)
          }
          else
            false  // Failed activation roll
        }
        else
          true  // Did not fail activation roll, so continue attacking
      }

      if (!params.event)
        logOpChoice(NVA, Attack)
      var keepAttacking   = true
      val troopCandidates = game.spaces filter { sp =>
        params.spaceAllowed(sp.name)     &&
        sp.pieces.totalOf(NVATroops) > 1 &&
        sp.pieces.has(CoinPieces)
      }

      if (!params.specialActivityOnly) {
        // Attack where 2+ troops
        keepAttacking = nextAttack(troopCandidates, useTroops = true, false)
      }

      val ambushed = if (addAmbush && keepAttacking && params.addSpecialActivity) {
        // Ambush in 1 or 2 spaces (if special activity allowed)
        lazy val ambushCandidates = spaceNames(game.spaces filter { sp =>
          params.spaceAllowed(sp.name) &&
          !attackSpaces(sp.name) &&
          canAmbushFrom(NVA, params.ambush.needUnderground)(sp)
        })
        val (ambushSpaces: Set[String], canContinue) = {
          if (ambushCandidates.nonEmpty)
            ambushActivity(NVA, ambushCandidates, Attack, actNum, params, checkFirst = attackSpaces.nonEmpty)
          else
            (Set.empty, true)
        }

        attackSpaces ++= ambushSpaces
        keepAttacking = canContinue
        ambushSpaces.nonEmpty
      }
      else
        false

      lazy val guerrillaCandidates = game.spaces filter { sp =>
        params.spaceAllowed(sp.name) &&
        !attackSpaces(sp.name) &&
        sp.pieces.totalOf(NVAGuerrillas) > 3 &&
        sp.pieces.has(CoinPieces)
      }

      // spaces the 4+ NVA Guerrillas
      if (keepAttacking && !params.specialActivityOnly)
        nextAttack(guerrillaCandidates, useTroops = false, needActivation = attackSpaces.nonEmpty)

      if (attackSpaces.nonEmpty) {
        applyPt76Shaded()  // If it is in effect
        Some(Attack, ambushed)
      }
      else {
        logNoOp(NVA, Attack)
        None
      }
    }

    def easterOffensiveAttack(attackSpaces: List[String]): Unit = {
      clearPT76Shaded()
      for (name <- attackSpaces) {
        performAttack(name, useTroops = true)
        pause()
      }
      applyPt76Shaded()  // If it is in effect
}


    //  -------------------------------------------------------------
    //  Implement the NVA Terror Instructions from the NVA Trung cards.
    //  Select spaces using Place Terror
    //  Note:
    //  Bruce Mansfield clarified that NVA Bot should not make
    //  an activation roll after conducting Terror on a LoC.
    //  -------------------------------------------------------------
    def terrorOp(params: Params, actNum: Int): Option[InsurgentOp] = {
      val maxTerror = params.maxSpaces getOrElse NO_LIMIT
      val isCandidate = (sp: Space) => {
        (sp.pieces.has(NVAGuerrillas_U) || sp.pieces.has(NVATroops)) &&
        ((sp.isLoC && sp.terror == 0 && sp.printedEconValue > 0) ||
         (!sp.isLoC && sp.canHaveSupport && (sp.terror == 0 || sp.support > Neutral)))
      }

      def nextTerror(numRemaining: Int, candidates: List[Space], needActivation: Boolean): Unit = {
        lazy val activateOK = params.free || checkActivation(VC, needActivation, actNum)
        if (numRemaining > 0 && candidates.nonEmpty && activateOK) {
          val sp = pickSpacePlaceTerror(candidates)

          log(s"\n$NVA selects ${sp.name} for Terror")
          if (sp.pieces.has(NVAGuerrillas_U))
            revealPieces(sp.name, Pieces(nvaGuerrillas_U = 1))

          if (sp.terror == 0 && game.terrorMarkersAvailable > 0)
            addTerror(sp.name, 1) // Terror/Sabotage marker

          if (sp.canHaveSupport && sp.support > Neutral)
            decreaseSupport(sp.name, 1)
          pause()
          nextTerror(numRemaining - 1, candidates filterNot (_.name == sp.name), needActivation = !sp.isLoC)
        }
      }

      if (!params.event)
        logOpChoice(NVA, Terror)
      val candidates = game.spaces filter isCandidate
      if (candidates.nonEmpty) {
        nextTerror(maxTerror, candidates, false)
        Some(Terror)
      }
      else {
        logNoOp(NVA, Terror)
        None
      }
    }

    //  -------------------------------------------------------------
    //  NVA Infiltrate Activity
    //
    //  Mo_TyphoonKate     - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
    //  Mo_McNamaraLine    - prohibits infiltrate
    //  Mo_559TransportGrp - Infiltrate is max 1 space
    //  -------------------------------------------------------------
    def infiltrateActivity(params: Params, needDiceRoll: Boolean, replaceVCBase: Boolean): Boolean = {
      if (!params.event && momentumInPlay(Mo_McNamaraLine))
        false
      else {
        var infiltrateSpaces = Set.empty[String]
        val limited          = momentumInPlay(Mo_TyphoonKate) || momentumInPlay(Mo_559TransportGrp)
        val maxInfiltrate    = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                               else if (limited) 1 else 2

        val notes = List(
          noteIf(momentumInPlay(Mo_TyphoonKate),s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
          noteIf(momentumInPlay(Mo_559TransportGrp), s"Infiltrate is max 1 space [Momentum: $Mo_559TransportGrp]")
        ).flatten

        val canReplaceBase = (sp: Space) =>
          params.spaceAllowed(sp.name) &&
          !infiltrateSpaces(sp.name) &&
          sp.pieces.totalOf(NVAPieces) > sp.pieces.totalOf(VCPieces) &&
          sp.pieces.has(VCBases)
        val canPlaceTroops = (sp: Space) =>
          params.spaceAllowed(sp.name) &&
          !infiltrateSpaces(sp.name) &&
          sp.pieces.has(NVABases)

        def placeTroops(): Unit = {
          val troopCandidates = game.nonLocSpaces filter canPlaceTroops
          val canPlace        = infiltrateSpaces.size < maxInfiltrate &&
                                game.availablePieces.has(NVATroops)   &&
                                troopCandidates.nonEmpty
          if (canPlace) {
            val sp        = pickSpacePlaceTroops(troopCandidates)
            val numGs     = (sp.pieces.totalOf(NVAGuerrillas) - 2) max 0
            val numTroops = (game.trail + sp.pieces.totalOf(NVABases) + numGs) min game.availablePieces.totalOf(NVATroops)
            val toRemove  = selectFriendlyRemoval(sp.pieces.only(NVAGuerrillas), numGs)
            val toPlace   = Pieces(nvaTroops = numTroops)
            if (!params.event && infiltrateSpaces.isEmpty)
              logSAChoice(NVA, Infiltrate, notes)

            log(s"\nNVA Infiltrates in ${sp.name}")
              log(separator())

            loggingControlChanges {
              removeToAvailable(sp.name, toRemove)
              placePieces(sp.name, toPlace)
            }
            pause()
            infiltrateSpaces += sp.name
            placeTroops()
          }
        }

        if (!needDiceRoll || NVA_Bot.threeD6_LE_AvailNVATroops) {
          val baseCandidates = game.nonLocSpaces filter canReplaceBase

          // First try to replace a VC base if requested
          if (replaceVCBase && game.availablePieces.has(NVABase) && baseCandidates.nonEmpty) {
            val sp      = pickSpaceRemoveReplace(baseCandidates)
            val vcPiece = selectEnemyRemoveReplaceActivate(sp.pieces.only(VCBases), 1)
            val nvaType = getInsurgentCounterPart(vcPiece.explode().head)

            if (!params.event)
              logSAChoice(NVA, Infiltrate, notes)
            log(s"\nNVA Infiltrates in ${sp.name}")
              log(separator())


            loggingControlChanges {
              if (sp.support < Neutral)
                increaseSupport(sp.name, 1)

              removeToAvailable(sp.name, vcPiece)
              placePieces(sp.name, Pieces(nvaBases = 1))
              if (nvaType == NVATunnel)
                  addTunnelMarker(sp.name, Pieces(nvaBases = 1))
              infiltrateSpaces += sp.name
            }
            pause()
          }

          // Next attempt to place troops at NVA bases
          placeTroops()
        }

        if (infiltrateSpaces.nonEmpty)
          logEndSA(NVA, Infiltrate)

        infiltrateSpaces.nonEmpty
      }
    }

    //  -------------------------------------------------------------
    //  NVA Bombard Activity
    //
    // Mo_TyphoonKate         - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
    // LongRangeGuns_Unshaded - NVA Bombard is max 1 space
    // LongRangeGuns_Shaded   - NVA Bombard is max 3 spaces
    //  -------------------------------------------------------------
    def bombardActivity(params: Params): Boolean = {
      if (!params.event && momentumInPlay(Mo_TyphoonKate))
        false   // Typooon Kate prohibits Bombard
      else {
        var bombardSpaces = Set.empty[String]
        val maxBombard = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (capabilityInPlay(LongRangeGuns_Unshaded)) 1
                    else if (capabilityInPlay(LongRangeGuns_Shaded))   3
                    else                                               2
        val isCandidate = (sp: Space) => {
          val enemyCondition = sp.pieces.totalOf(CoinTroops) > 2 ||
                               (sp.pieces.has(CoinTroops) && sp.pieces.has(CoinBases))
          lazy val hasAdjacentTroops = getAdjacent(sp.name) exists { name =>
            game.getSpace(name).pieces.totalOf(NVATroops) > 2
          }

          params.spaceAllowed(sp.name) &&
          !bombardSpaces(sp.name) &&
          enemyCondition          &&
          (sp.pieces.totalOf(NVATroops) > 2 || hasAdjacentTroops)
        }
        val notes = List(
          noteIf(capabilityInPlay(LongRangeGuns_Unshaded),s"NVA Bombard is max 1 space [$LongRangeGuns_Unshaded]"),
          noteIf(capabilityInPlay(LongRangeGuns_Shaded), s"NVA Bombard is max 3 spaces [$LongRangeGuns_Shaded]")
        ).flatten

        def nextBombard(numBombarded: Int): Unit = {
          val candidates = game.spaces filter isCandidate

          if (candidates.nonEmpty && numBombarded < maxBombard) {
            val sp       = pickSpaceRemoveReplace(candidates)
            val troops   = sp.pieces.only(CoinTroops)
            val toRemove = selectEnemyRemoveReplaceActivate(troops, troops.total min 1)

            if (!params.event && numBombarded == 0)
              logSAChoice(NVA, Bombard, notes)

            log(s"\nNVA Bombards ${sp.name}")
            log(separator())
            removePieces(sp.name, toRemove)
            bombardSpaces += sp.name
            pause()
            nextBombard(numBombarded + 1)
          }
        }

        nextBombard(0)

        // Return true if we bombarded at least one space
        if (bombardSpaces.nonEmpty)
          logEndSA(NVA, Bombard)

        bombardSpaces.nonEmpty
      }
    }
  }

  // ================================================================
  // VC Specific code
  // ================================================================
  object VC_Bot {

    def threeD6_LE_AvailVCPieces = trungDiceCheck(3, game.availablePieces.totalOf(VCPieces), CMP_LE, s"Available VC pieces")

    val canSubvertSpace = (sp: Space) => sp.pieces.has(VCGuerrillas_U) && sp.pieces.has(ARVNCubes)
    val canTaxSpace = (sp: Space) => {
      // Bot will only tax spaces with a VC Base if at least 2 underground guerrillas
      val needed = if (sp.pieces.has(VCBases)) 2 else 1
      val hasG = sp.pieces.totalOf(VCGuerrillas_U) >= needed
      hasG && ((sp.isLoC && sp.printedEconValue > 0) || (!sp.coinControlled && sp.population > 0))
    }

    def patronage_GE_17: Boolean = {
      logCheck(game.patronage >= 17, "Patronage >= 17?")
    }
    
    def fifteenPlusVCGuerrilasOnMap: Boolean = {
      val numVCGuerrillas = (sp: Space) => sp.pieces.totalOf(VCGuerrillas)
      val vcGuerrillasOnMap = game totalOnMap numVCGuerrillas
      
      logCheck(vcGuerrillasOnMap >= 15, "15+ VC Guerrillas on the map?")
    }

    def threePlusGuerrillasInSpace: Boolean = {
      val result = game.spaces exists (_.pieces.totalOf(VCGuerrillas) > 2)
      logCheck(result, "3+ VC Guerrillas in any space?")
    } 
    
    def undergroundAtNoActiveOpposition: Boolean = {
      val result = game.spaces exists { sp =>
        (sp.isLoC || sp.support != ActiveOpposition) && sp.pieces.has(VCGuerrillas_U)
      }
      logCheck(result, "Underground VC Guerrillas in space not at Active Opposition?")
    }

    def pop2SpaceWithoutGuerrillas: Boolean = {
      val result = game.spaces exists { sp =>
          !sp.isLoC && sp.population >= 2 && sp.pieces.totalOf(VCGuerrillas) == 0
      }
      logCheck(result, "Any 2+ Pop space without VC Guerrillas?")
    }

    def undergroundWithUSTroops: Boolean = {
      val result = game.spaces exists { sp=>
        sp.pieces.has(VCGuerrillas_U) && sp.pieces.has(USTroops)
      }
      logCheck(result, "Underground VC Guerrillas in space with US Troops?")
    }

    def vcMarchAmbushCandidates: List[String] = marchAmbushCandidates(VC, needUnderground = true)
    
    def canFollowMarchWithAmbush: Boolean = { 
      val result = vcMarchAmbushCandidates.nonEmpty && !momentumInPlay(Mo_Claymores)
      logCheck(result, "Can Ambush in March destinations?")
    }

    //  Can Tax if candidate spaces exist and 2d6 > agitate totak
    def twoD6GTAgitateTotal: Boolean = {
      logCheck(game.spaces exists VC_Bot.canTaxSpace, "Spaces exist that can be Taxed?") &&
      trungDiceCheck(2, game.agitateTotal, CMP_GT, s"Agitate Total")
    }

    def threePlusGuerrillasWithUSTroopsNoVCBase: Boolean = {
      val test = (sp: Space) => {
        sp.pieces.totalOf(VCGuerrillas) > 2 &&
        sp.pieces.has(USTroops)             &&
        !sp.pieces.has(VCBases)
       }
       logCheck(game.spaces exists test, "3+ VC Guerrillas in any spaces with US Troops and no VC Base?")
    }

    val MostUndergroundGuerrillas = List(
      new HighestScore[Space](
        "Most Underground VC Guerrillas",
        sp => sp.pieces.totalOf(VCGuerrillas_U)
      )
    )

    val MostActiveGuerrillas = List(
      new HighestScore[Space](
        "Most Active VC Guerrillas",
        sp => sp.pieces.totalOf(VCGuerrillas_A)
      )
    )

    // VC Spaces Priorities: Shift Toward Active Opposition
    def pickSpaceTowardActiveOpposition(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             CityProvinceNoActiveOpposition),
        filterIf(true,             MostPopulation),
        filterIf(game.isHuman(US), MostTotalSupport)
      ).flatten

      botDebug(s"\nVC Select space (Shift Toward Active Opposition): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // VC Spaces Priorities: Place Terror
    def pickSpacePlaceTerror(candidates: List[Space]): Space = {
      val coinPlayer = game.isHuman(US) || game.isHuman(ARVN)
      val priorities = List(
        filterIf(true,             CityProvinceNoActiveOpposition),
        filterIf(true,             MostPopulation),
        filterIf(game.isHuman(US), MostTotalSupport),
        filterIf(true,             CityProvinceMostVCGuerrillas),
        filterIf(true,             CityProvinceFewestNonVCPieces),
        filterIf(coinPlayer,       OnePlusEconLoc)
      ).flatten

      botDebug(s"\nVC Select space (Place Terror): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // VC Spaces Priorities: Place Bases or Tunnels
    def pickSpacePlaceBases(candidates: List[Space]): Space = {
      val priorities = List(
        MostPopulation,
        CityProvinceMostVCGuerrillas,
        CityProvinceFewestNonVCPieces
      )

      botDebug(s"\nVC Select space (Place Bases or Tunnels): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // VC Spaces Priorities: Place Guerrilllas
    def pickSpacePlaceGuerrillas(candidates: List[Space]): Space = {
      val priorities = List(
        filterIf(true,               VulnerableVCBase),
        filterIf(true,               MostPopulation),
        filterIf(true,               MostVCBases),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(game.isHuman(US),   MostTotalSupport),
        filterIf(game.isHuman(NVA),  HasNVAControl),
        filterIf(true,               OneTwoVCGuerrillasRoomForBase),
        filterIf(true,               CityProvinceFewestNonVCPieces)
      ).flatten

      botDebug(s"\nVC Select space (Place Guerrilllas): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // VC Spaces Priorities: March Destinations
    def pickSpaceMarchDest(candidates: List[Space]): Space = {
      val priorities = List(
        filterIf(true,               CityProvinceNoActiveOpposition),
        filterIf(true,               CityProvinceFewestVCPieces),
        filterIf(true,               MostPopulation),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(game.isHuman(US),   MostTotalSupport),
        filterIf(game.isHuman(NVA),  HasNVAControl),
        filterIf(true,               OneTwoVCGuerrillasRoomForBase),
        filterIf(true,               CityProvinceFewestNonVCPieces)
      ).flatten

      botDebug(s"\nVC Select space (March Destinations): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    // VC Spaces Priorities: Tax
    def pickSpaceTax(candidates: List[Space]): Space = {
      val coinPlayer = game.isHuman(US) || game.isHuman(ARVN)

      val priorities = List(
        filterIf(true,        MostTotalOpposition),
        filterIf(true,        CityProvinceMostVCGuerrillas),
        filterIf(coinPlayer,  OnePlusEconLoc)
      ).flatten

      botDebug(s"\nVC Select space (Tax): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }


    // VC Spaces Priorities: Remove or Replace
    def pickSpaceRemoveReplace(candidates: List[Space]): Space = {
      val priorities = List(
        filterIf(true,               VulnerableVCBase),
        filterIf(true,               MostPopulation),
        filterIf(true,               MostVCBases),
        filterIf(game.isHuman(ARVN), HasCoinControl),
        filterIf(game.isHuman(NVA),  HasNVAControl),
        filterIf(true,               CityProvinceMostVCGuerrillas),
        filterIf(true,               CityProvinceFewestNonVCPieces)
      ).flatten

      botDebug(s"\nVC Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botDebug(separator(char = '#'))
      bestCandidate(candidates, priorities)
    }

    //  Returns the space selected for Ambush and the space being targeted by the Ambush
    //  The targeted space can be the same as the ambushing space or of the ambush space
    //  is a LoC the targeted space can be an adjacent space.
    def pickSpaceAndTargetForAmbush(candidates: List[Space]): (Space, Space) = {
      // First find all possible target spaces and pick the best one.
      val targetCandidates = spaces(candidates flatMap (sp => ambushTargets(sp.name)))
      val targetSpace = pickSpaceRemoveReplace(targetCandidates)

      // Now find all of the original candidate spaces that can target the
      // target space and pick the one with the must Undeground Guerrillas.
      val reachableCandidates = candidates filter (sp => ambushTargets(sp.name) contains targetSpace.name)
      val ambushSpace = bestCandidate(reachableCandidates, MostUndergroundGuerrillas)

      (ambushSpace, targetSpace)
    }

    // Final part of the Commitment Phase of a Coup Round.
    // 1. Shift spaces with a VC Base at Passive Support first,
    //    using "Shift Toward Opposition" priorities.
    def usWithdrawalShifts(numPopShifts: Int): Unit = {
      var shiftSpaces = Set.empty[String]

      def nextShift(numRemaining: Int): Unit = if (numRemaining > 0) {
        val candidates1 = game.nonLocSpaces filter { sp =>
          !shiftSpaces(sp.name)         &&
          sp.canHaveSupport             &&
          sp.population <= numRemaining &&
          sp.support == PassiveSupport  &&
          sp.pieces.has(VCBases)
        }
        val candidates2 = game.nonLocSpaces filter { sp =>
          !shiftSpaces(sp.name)         &&
          sp.canHaveSupport             &&
          sp.population <= numRemaining &&
          sp.support > ActiveOpposition
        }

        if (candidates1.nonEmpty || candidates2.nonEmpty) {
          val sp = pickSpaceTowardActiveOpposition(if (candidates1.nonEmpty) candidates1 else candidates2)

          decreaseSupport(sp.name, 1)
          shiftSpaces += sp.name
          nextShift(numRemaining - sp.population)
        }
      }

      nextShift(numPopShifts)
    }

    // Cadres_Unshaded - VC must remove two guerrillas in the space
    // Cadres_Shaded   - Allows VC to agitate in a rally space.
    def agitateSpace(name: String, coupRound: Boolean, maxLevels: Int = 2): Unit = {
      val cadres_unshaded = capabilityInPlay(Cadres_Unshaded)
      val sp = game.getSpace(name)
      val cadres_check = !cadres_unshaded || sp.pieces.totalOf(VCGuerrillas) >= 2

      if (game.agitateTotal > 0 && (coupRound || capabilityInPlay(Cadres_Shaded)) && cadres_check) {
        val cadres_msg = if (coupRound) "" else s" [$Cadres_Shaded]"
        val numTerror = sp.terror min game.agitateTotal
        val numShift  = (sp.support.value - ActiveOpposition.value) min (game.agitateTotal - numTerror) min maxLevels
        log(s"\nVC Agitates ${amountOf(numShift, "level")} in ${name}$cadres_msg")
        log(separator())
        loggingPointsChanges {
          removeTerror(sp.name, numTerror)
          decreaseSupport(sp.name, numShift)
          decreaseAgitateTotal(numTerror + numShift)
        }

        if (cadres_unshaded) {
          // Get fresh copy of space to include the guerrilla that was just flipped
          val toRemove = selectFriendlyRemoval(sp.pieces.only(VCGuerrillas), 2)
          removeToAvailable(sp.name, toRemove, Some(s"$Cadres_Unshaded triggers"))
        }
      }
    }

    //  -------------------------------------------------------------
    //  Implement the VC Rally Instructions from the VC Trung cards.
    //  1. Place Bases where 3+ VC Guerrillas
    //  2. Select spaces using Place Guerrillas priority
    //  3. Flip most Guerrillas where ALL active
    //  Cadres_Shaded - VC Rally in 1 space that already had a base
    //                  may Agitage (even if COIN control)
    //                - Remove Terror then Shift upt to two levels
    //                  toward Active Opposition up to Agitate Total
    //  -------------------------------------------------------------
    def rallyOp(params: Params, actNum: Int): Option[InsurgentOp] = {
      var rallySpaces  = Set.empty[String]
      var agitated     = false  // Using Cadres
      val cadres       = capabilityInPlay(Cadres_Shaded)
      val maxRally     = params.maxSpaces getOrElse NO_LIMIT
      def rallied      = rallySpaces.nonEmpty
      def canRally     = rallySpaces.size < maxRally && (params.free || checkActivation(VC, rallied, actNum))
      val canRallyBase = (sp: Space) =>
        !rallySpaces(sp.name)       &&
        params.spaceAllowed(sp.name) &&
        !params.rally.guerrillasOnly &&
        sp.support < PassiveSupport &&
        sp.totalBases < 2           &&
        sp.pieces.totalOf(VCGuerrillas) > 2

      val canRallyGuerrillas = (sp: Space) =>
        !rallySpaces(sp.name) &&
        params.spaceAllowed(sp.name) &&
        sp.support < PassiveSupport

      val canFlipGuerrillas = (sp: Space) =>
        !rallySpaces(sp.name)         &&
        params.spaceAllowed(sp.name)  &&
        sp.support < PassiveSupport   &&
        sp.pieces.has(VCBases)        &&
        sp.pieces.has(VCGuerrillas_A) &&
        !sp.pieces.has(VCGuerrillas_U)

      def tryCadresAgitate(sp: Space): Unit = {
        if (!agitated && cadres && sp.support > ActiveOpposition && game.agitateTotal > 0)
          agitateSpace(sp.name, coupRound = false)
      }

      // Return true if we can continue (no failed activation)
      def rallyToPlaceBase(candidates: List[Space]): Boolean = {
        if (game.availablePieces.has(VCBase) && candidates.nonEmpty) {
          if (canRally) {
            val sp       = pickSpacePlaceBases(candidates)
            val hadBase  = sp.pieces.has(VCBases)
            val toRemove = selectFriendlyRemoval(sp.pieces.only(VCGuerrillas), 2)
            log(s"\n$VC selects ${sp.name} for Rally")
            log(separator())
            loggingControlChanges {
              removeToAvailable(sp.name, toRemove)
              placePieces(sp.name, Pieces(vcBases = 1))
            }
            rallySpaces = rallySpaces + sp.name
            if (hadBase)
              tryCadresAgitate(sp)
            pause()
            rallyToPlaceBase(candidates filterNot (_.name == sp.name))
          }
          else
            false // Failed activation or max spaces rallied
        }
        else
          true  // No failed activation, ran out of candidates
      }

      // Return true if we can continue (no failed activation)
      def rallyToPlaceGuerrillas(candidates: List[Space]): Boolean = {
        if (game.availablePieces.has(VCGuerrillas_U) && candidates.nonEmpty) {
          if (canRally) {
            val sp         = pickSpacePlaceGuerrillas(candidates)
            val hadBase    = sp.pieces.has(VCBases)
            val numToPlace = if (hadBase)
              (sp.pieces.totalOf(VCBases) + sp.population) min game.availablePieces.totalOf(VCGuerrillas_U)
            else
              1
            log(s"\n$VC selects ${sp.name} for Rally")
            log(separator())
            placePieces(sp.name, Pieces(vcGuerrillas_U = numToPlace))
            rallySpaces = rallySpaces + sp.name
            if (hadBase)
              tryCadresAgitate(sp)
            pause()
            rallyToPlaceGuerrillas(candidates filterNot (_.name == sp.name))
          }
          else
            false // Failed activation or max spaces rallied
        }
        else
          true  // No failed activation, ran out of candidates
      }

      // Return true if we can continue (no failed activation)
      def rallyToFlipGuerrillas(candidates: List[Space]): Boolean = {
        if (candidates.nonEmpty) {
          if (canRally) {
            val sp = bestCandidate(candidates, MostActiveGuerrillas)

            log(s"\n$VC selects ${sp.name} for Rally")
            log(separator())
            hidePieces(sp.name, sp.pieces.only(VCGuerrillas_A))
            rallySpaces = rallySpaces + sp.name
            // We know the space already had a base
            tryCadresAgitate(sp)
            pause()
            rallyToFlipGuerrillas(candidates filterNot (_.name == sp.name))
          }
          else
            false // Failed activation or max spaces rallied
        }
        else
          true  // No failed activation, ran out of candidates
      }

      if (!params.event)
        logOpChoice(VC, Rally)
      rallyToPlaceBase(game.nonLocSpaces filter canRallyBase)             &&
      rallyToPlaceGuerrillas(game.nonLocSpaces filter canRallyGuerrillas) &&
      rallyToFlipGuerrillas(game.nonLocSpaces filter canFlipGuerrillas)

      if (rallied)
        Some(Rally)
      else {
        logNoOp(VC, Rally)
        None
      }
    }

    //  -------------------------------------------------------------
    //  Implement the VC March Instructions from the VC Trung cards.
    //    March using Move Priorities
    //    1. Select 2 LoCs adjacent to most underground VC guerrillas
    //    2. Select spaces using March Destinations
    //
    //    March not allowed in Monsoon.
    //  -------------------------------------------------------------
    def marchOp(params: Params, actNum: Int): Option[InsurgentOp] = {
      if (!params.event && game.inMonsoon)
        None
      else {
        val nextLoCCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          val LocPriorities = List(
            new HighestScore[Space](
              "Most Adjacent Underground VC Guerrillas",
              sp => numAdjacentPieces(sp, Set(VCGuerrillas_U))
            )
          )
          val qualifies = (sp: Space) =>
            params.spaceAllowed(sp.name) &&
            !prohibited(sp.name) &&
            numAdjacentPieces(sp, Set(VCGuerrillas_U)) > 0

          lazy val locCandidates = game.locSpaces filter qualifies

          // No need to check activation since these are LoCs
          if (moveDestinations.size < 2 && locCandidates.nonEmpty) {
            Some(bestCandidate(locCandidates, LocPriorities).name)
          }
          else
            None
        }

        // Select the next march candidate
        // prohibited contains all previous march destinations plus failed
        // destinations.
        val nextGenericCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
          lazy val candidates = game.spaces filter {sp =>
            params.spaceAllowed(sp.name) &&
            !prohibited.contains(sp.name)
          }

          if (candidates.nonEmpty && (params.free || checkActivation(VC, needActivation, actNum)))
            Some(VC_Bot.pickSpaceMarchDest(candidates).name)
          else
            None
        }

        if (!params.event)
          logOpChoice(VC, March)

        movePiecesToDestinations(VC, March, VCGuerrillas.toSet, false, params, maxDests = params.maxSpaces)(nextLoCCandidate)
        // First activation check is always false since previos 0-2 spaces were LoCs
        movePiecesToDestinations(VC, March, VCGuerrillas.toSet, false, params, maxDests = params.maxSpaces)(nextGenericCandidate)

        if (moveDestinations.nonEmpty)
          Some(March)
        else {
          logNoOp(VC, March)
          None
        }
      }
    }

    //  -------------------------------------------------------------
    //  Implement the VC Attack Instructions from the VC Trung cards.
    //  Select spaces using the Remove Space Priority
    //  1. Ambush in 2 spaces (if able)
    //  2. Select spaces with 3+ VC Guerrillas and no VC base
    //
    //  Return Some(Attack) if we attack/ambush at least one space
    //         true if we included the ambush special activity
    //  -------------------------------------------------------------
    def attackOp(params: Params, actNum: Int): (Option[InsurgentOp], Boolean) = {
      val threePlusGuerrillasNoVCBase = (sp: Space) => {
        sp.pieces.totalOf(VCGuerrillas) > 2 &&
        !sp.pieces.has(VCBases)
      }
      val maxAttacks = params.maxSpaces getOrElse NO_LIMIT
      var attackSpaces = Set.empty[String]

      def nextAttack(candidates: List[Space], needActivation: Boolean): Unit = {
        lazy val activateOK = params.free || checkActivation(VC, needActivation, actNum)
        if (attackSpaces.size < maxAttacks && candidates.nonEmpty && activateOK) {
          val sp         = pickSpaceRemoveReplace(candidates)
          val guerrillas = sp.pieces.only(VCGuerrillas)
          val coinPieces = sp.pieces.only(CoinPieces)
          val toActivate = guerrillas.only(VCGuerrillas_U)
          val num        = 2 min coinPieces.total
          val die        = d6
          val numGs      = guerrillas.total
          val success    = die <= numGs

          log(s"\n$VC Attacks in ${sp.name}")
          log(separator())
          log(s"Die roll ${amountOf(numGs, "Guerrilla")}): $die [${if (success) "Success!" else "Failure"}]")
          attackSpaces += sp.name

          if (success) {
            val deadPieces    = selectRemoveEnemyCoinBasesLast(coinPieces, num)
            val attritionNum  = deadPieces.totalOf(USTroops::USBase::Nil) min guerrillas.total
            val attrition     = Pieces(vcGuerrillas_A = attritionNum) // All VC guerrillas are now active

            revealPieces(sp.name, toActivate)
            loggingControlChanges {
              removePieces(sp.name, deadPieces)
              removeToAvailable(sp.name, attrition, Some("Attrition:"))
              pause()
            }
          }
          nextAttack(candidates filterNot (_.name == sp.name), needActivation = true)
        }
      }

      if (!params.event)
        logOpChoice(VC, Attack)
      val ambushCandidates = spaceNames(game.spaces filter canAmbushFrom(VC, params.ambush.needUnderground))
      val (ambushSpaces: Set[String], canContinue) = if (params.addSpecialActivity && ambushCandidates.nonEmpty)
        ambushActivity(VC, ambushCandidates, Attack, actNum, params, checkFirst = false)
      else
        (Set.empty, true)

      attackSpaces ++= ambushSpaces
      val ambushed = ambushSpaces.nonEmpty
      // If we have not failed an activation roll then continue attacking in
      // spaces the 3+ VC Guerrillas and not VC Base
      val attackCandidates = game.spaces filterNot (sp => ambushSpaces(sp.name)) filter threePlusGuerrillasNoVCBase
      val attacked = if (canContinue && attackCandidates.nonEmpty) {
        nextAttack(attackCandidates, needActivation = ambushed)
        true
      }
      else
        false

      if (ambushed || attacked)
        (Some(Attack), ambushed)
      else {
        logNoOp(VC, Attack)
        (None, false)
      }
    }

    //  -------------------------------------------------------------
    //  Implement the VC Terror Instructions from the VC Trung cards.
    //    Select spaces using Place Terror
    //    VC Base - where 2+ Undergound Guerrillas
    //    Loc     - no activation number roll
    //  -------------------------------------------------------------
    def terrorOp(params: Params, actNum: Int): Option[InsurgentOp] = {
      val maxTerror   = params.maxSpaces getOrElse NO_LIMIT
      val isCandidate = (sp: Space) => {
        val needed = if (sp.pieces.has(VCBases)) 2 else 1
        sp.pieces.totalOf(VCGuerrillas_U) >= needed &&
        ((sp.isLoC && sp.terror == 0 && sp.printedEconValue > 0) ||
         (!sp.isLoC && sp.canHaveSupport && (sp.terror == 0 || sp.support != ActiveOpposition)))
      }

      def nextTerror(numRemaining: Int, candidates: List[Space], needActivation: Boolean): Unit = {
        lazy val activateOK = params.free || checkActivation(VC, needActivation, actNum)
        if (numRemaining > 0 && candidates.nonEmpty && activateOK) {
          val sp = pickSpacePlaceTerror(candidates)

          log(s"\n$VC selects ${sp.name} for Terror")
          revealPieces(sp.name, Pieces(vcGuerrillas_U = 1))
          if (capabilityInPlay(Cadres_Unshaded)) {
            // Get fresh copy of space to include the guerrilla that was just flipped
            val toRemove = selectFriendlyRemoval(game.getSpace(sp.name).pieces.only(VCGuerrillas), 2)
            removeToAvailable(sp.name, toRemove, Some(s"$Cadres_Unshaded triggers"))
          }

          if (sp.terror == 0 && game.terrorMarkersAvailable > 0)
            addTerror(sp.name, 1) // Terror/Sabotage marker

          if (sp.canHaveSupport)
            decreaseSupport(sp.name, 1)

          pause()
          nextTerror(numRemaining - 1, candidates filterNot (_.name == sp.name), !sp.isLoC)
        }
      }

      if (!params.event)
        logOpChoice(VC, Terror)
      val candidates = game.spaces filter isCandidate
      if (candidates.nonEmpty) {
        nextTerror(maxTerror, candidates, false)
        Some(Terror)
      }
      else {
        logNoOp(VC, Terror)
        None
      }
    }

    //  -------------------------------------------------------------
    //  VC Tax Activity
    //  -------------------------------------------------------------
    def taxActivity(params: Params): Boolean = {
      var taxSpaces = Set.empty[String]
      val maxTax = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                   else if (momentumInPlay(Mo_TyphoonKate)) 1 else d3

      def nextTax(): Unit = {
        val candidates = game.spaces filter { sp =>
          params.spaceAllowed(sp.name) &&
          !taxSpaces(sp.name)          &&
          canTaxSpace(sp)
        }
        if (taxSpaces.size < maxTax && candidates.nonEmpty) {
          val sp    = pickSpaceTax(candidates)
          val num  = if (sp.isLoC) sp.printedEconValue else sp.population

          if (!params.event && taxSpaces.isEmpty)
            logSAChoice(VC, Tax)

          log(s"\nVC Taxes in ${sp.name}")
          log(separator())
          revealPieces(sp.name, Pieces(vcGuerrillas_U = 1))
          loggingPointsChanges {
            if (!sp.isLoC && sp.support != ActiveSupport)
              increaseSupport(sp.name, 1)
            increaseAgitateTotal(num)
          }
          taxSpaces += sp.name
          pause()
          nextTax()
        }
      }

      nextTax()
      if (taxSpaces.nonEmpty)
        logEndSA(VC, Tax)
      taxSpaces.nonEmpty
    }

    //  -------------------------------------------------------------
    //  VC Subert Activity
    //  -------------------------------------------------------------
    def subvertActivity(params: Params): Boolean = {
      val maxSubvert = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                       else if (momentumInPlay(Mo_TyphoonKate)) 1 else 2
      var totalRemoved = 0

      def nextSubvert(candidates: List[Space], numSubverted: Int): Unit = {
        if (candidates.nonEmpty && numSubverted < maxSubvert) {
          val sp       = pickSpaceRemoveReplace(candidates)
          val cubes    = sp.pieces.only(ARVNCubes)
          val toRemove = selectEnemyRemoveReplaceActivate(cubes, cubes.total min 2)

          if (!params.event && numSubverted == 0)
            logSAChoice(VC, Subvert)

          log(s"\nVC Subverts in ${sp.name}")
          log(separator())
          removeToAvailable(sp.name, toRemove)
          totalRemoved += toRemove.total
          if (toRemove.total == 1 && game.availablePieces.has(VCGuerrillas_U))
            placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
          pause()
          nextSubvert(candidates filterNot (_.name == sp.name), numSubverted + 1)
        }
      }

      val candidates = game.spaces filter { sp =>
        params.spaceAllowed(sp.name) && VC_Bot.canSubvertSpace(sp)
      }
      nextSubvert(game.spaces filter VC_Bot.canSubvertSpace, 0)
      if (totalRemoved > 0) {
        if (totalRemoved / 2 > 0) {
          log()
          decreasePatronage(totalRemoved / 2)
          pause()
        }
        logEndSA(VC, Subvert)
        true
      }
      else
        false
    }
  }


  def isFirstOnNextCard(faction: Faction) = {
    val nextCard = eventDeck(game.onDeckCard)
    !nextCard.isCoup && faction == nextCard.factionOrder.head
  }

  val firstEligibileTable = List(
    // Event is Critical and Effective?
    ActionEntry(Event, "Current Event is Critical and Effective?",
    (faction) => {
      val card = eventDeck(game.currentCard)
      card.botPriority(faction) == Critical && card.eventEffective(faction)
    }),

    // Event is critical for Next eligible faction?
    ActionEntry(OpOnly, "Next Eligible could choose Critical Event?",
    (faction) => {
      val card = eventDeck(game.currentCard)
      game.followingFaction map (next => card.botPriority(next) == Critical) getOrElse false
    }),

    // Is first to act on next csrd and the event is Critical?
    ActionEntry(Pass, "First choice on upcoming Critical Event?",
      (faction) => {
      val nextCard = eventDeck(game.onDeckCard)

      isFirstOnNextCard(faction) && nextCard.botPriority(faction) == Critical
    }),

    // Otherwise...
    ActionEntry(OpPlusSpecial, "Otherwise...", (_) => true)
  )

  val secondEligibileTable = List(
    // Active fAction will be 1st Eligible on upcoming Critical Event and
    // cannot execute current Critical event?
    ActionEntry(Pass, "Will be 1st Eligible on upcoming Critical Event and cannot execute current Critical Event?",
      (faction) => {
      val card     = eventDeck(game.currentCard)
      val nextCard = eventDeck(game.onDeckCard)

      isFirstOnNextCard(faction) &&
      nextCard.botPriority(faction) == Critical &&
      !(game.sequence.canDo(Event) &&
        card.botPriority(faction) == Critical &&
        card.eventEffective(faction))
    }),

    // Event is Critical or Performed and it is Effective
    ActionEntry(Event, "1st Eligibile chose Op+SA and Event is Performed/Critical and Effective?",
      (faction) => {
      val card     = eventDeck(game.currentCard)
      val priority = card.botPriority(faction)

      game.sequence.canDo(Event) &&
      (priority == Critical || priority == Performed) &&
      card.eventEffective(faction)
    }),

    // Will be 1st Eligibile on upcoming card
    ActionEntry(Pass, "Will be 1st Eligibile on upcoming card?", (faction) => isFirstOnNextCard(faction)),

    // 1st Eligible chose Op Only or Op + SA?
    ActionEntry(LimitedOp, "1st Eligible chose Op Only or Op+SA?", (_) => { !game.sequence.canDo(OpPlusSpecial) }),

    // Otherwise...
    ActionEntry(OpPlusSpecial, "Otherwise...", (_) => true)
  )

  //  Choose an action for a Bot faction using the NP Elgibility Table
  //  A `prevEntry` is supplied if we previously chose an action but it
  //  was not able to be carried out effectively.  So we continue with the
  //  next row of the table.
  //  Note: The logic for determing if the Bot will choose its Pivotal Event
  //        is assumed to have already been done.
  def chooseAction(faction: Faction): Option[ActionEntry] = {
    val isFirstEligible = game.sequence.numActors == 0

    botDebug {
      val which = if (isFirstEligible) "1st" else "2nd"
      s"\n$faction Choosing Action using NP Eligiblity Table ($which eligible)"
    }
    botDebug(separator(char = '#'))

    val table = if (isFirstEligible)
      firstEligibileTable
    else
      secondEligibileTable

    table find { entry =>
      val result = entry.test(faction)
      botDebug(msgResult(result, entry.desc))
      result
    }
  }

  def initTurnVariables(): Unit = {
    moveDestinations      = Vector.empty
    transportDestinations = Vector.empty
    transportOrigin       = None
    trainingSpaces        = Set.empty
    adviseSpaces          = Set.empty
    m48PattonSpaces       = Set.empty
    movedPieces.reset()
  }

  def executeEvent(faction: Faction, card: EventCard): Unit = {
    card.eventType match {
      case DualEvent =>
        card.eventPart(faction) match {
          case Unshaded =>
            log(s"\n$faction executes the Unshaded Event: ${card.name}")
            log(separator())
            card.executeUnshaded(faction)
          case Shaded =>
            log(s"\n$faction executes the Shaded Event: ${card.name}")
            log(separator())
            card.executeShaded(faction)
        }

      case SingleEvent =>
        log(s"\n$faction executes the Event: ${card.name}")
        log(separator())
        card.executeUnshaded(faction)
    }
  }

  //  A bot is the next eligible faction
  //  Decide what type of action the Bot will take.
  //  We use the NP Elgibility Table.
  //  If an Op/Op+Sa/Lim-Op is selected and upon trying the operation we
  //  determine that it could not be carried out, then we continue down the
  //  table.
  def act(): Unit = {
    val faction = game.actingFaction.get
    val card = eventDeck(game.currentCard)

    initTurnVariables()

    val action = if (game.executingPivotalEvent) {
      //  The Pivotal events are single events which are always in the executeUnshaded() function.
      log(s"\n$faction executes its Pivotal Event: ${card.name}")
      card.executeUnshaded(faction)
      Event
    }
    else {
      chooseAction(faction) match {
        case None =>  // Could not carry out an action so Pass
          factionPasses(faction)
          Pass

        case Some(ActionEntry(Pass, _, _)) => // Table resolve to Pass
          factionPasses(faction)
          Pass

        case Some(ActionEntry(Event, _, _)) =>
          executeEvent(faction, card)
          Event

        case entry @ Some(ActionEntry(action, _, _)) =>
          // LimitedOp, OpOnly, or OpPlusSpecial
          val first = game.sequence.numActors == 0
          val sa    = action == OpPlusSpecial
          val maxsp = if (action == LimitedOp) Some(1) else None
          val params = Params(addSpecialActivity = sa, maxSpaces = maxsp)

          // If the first eligible was allowd to do a special
          // activity but was not able to do so then switch
          // it action to OpOnly
          // If the Bot could not do any operation at all then pass
          val actualAction = executeOp(faction, params) match {
            case ER_NoOp                  => Pass    // Could not do any action!
            case ER_OpOnly if first && sa => OpOnly  // First eligible could not do special activity
            case _                        => action
          }

          if (actualAction == Pass)
            factionPasses(faction)
          actualAction
      }
    }

    log(s"\nMove the $faction cylinder to the ${actorBoxName(action)} box")
    game = game.copy(sequence = game.sequence.addActor(faction, action))
}

  sealed trait ExecuteResult
  case object ER_OpPlusSpecial extends ExecuteResult
  case object ER_OpOnly        extends ExecuteResult
  case object ER_NoOp          extends ExecuteResult

  // Return TRUE if the operation was carried out
  // It may not be able to be done if:
  // ARVN resources == 0 and one of the COIN factions is Human
  // There are not eligible spaces to operate on, etc.
  // We keep track of the first card that we drew for the
  // off chance that we cycle through all of them and cannot perform
  // and operation.  Probably won't happend but this will prevent and
  // endless loop.
  def executeOp(faction: Faction, params: Params): ExecuteResult = {
    initTurnVariables()

    val firstCard = drawTrungCard(faction)

    def logTrungDraw(card: TrungCard): Unit = if (game.logTrung || game.botDebug) {
      log(s"\nDrawing Trung Card for $faction")
      log(separator())
      log(card.toString)
    }

    def executeCard(trungCard: TrungCard): ExecuteResult = {
      trungCard.executeFront(params) match {
        case  TrungComplete(true)  => ER_OpPlusSpecial
        case  TrungComplete(false) => ER_OpOnly

        case  TrungDraw | TrungNoOp =>
          val nextCard = drawTrungCard(faction)
          if (nextCard == firstCard)
            ER_NoOp
          else {
            logTrungDraw(nextCard)
            executeCard(nextCard)
          }
      }
    }

    logTrungDraw(firstCard)
    executeCard(firstCard)
  }

  // The Trung Deck contains only the face up cards.
  // The face sides are implmented within each card
  // object.
  val TrungDeck = List(
    Trung_US_A,  Trung_ARVN_G,  Trung_NVA_N,  Trung_VC_U,
    Trung_US_B,  Trung_ARVN_H,  Trung_NVA_P,  Trung_VC_V,
    Trung_US_C,  Trung_ARVN_J,  Trung_NVA_Q,  Trung_VC_W,
    Trung_US_D,  Trung_ARVN_K,  Trung_NVA_R,  Trung_VC_X,
    Trung_US_E,  Trung_ARVN_L,  Trung_NVA_S,  Trung_VC_Y,
    Trung_US_F,  Trung_ARVN_M,  Trung_NVA_T,  Trung_VC_Z
  )

  def trungFromId(id: String): TrungCard = TrungDeck.find(_.id == id) getOrElse {
    throw new IllegalArgumentException(s"Invalid Trung Card id: $id")
  }
  // ================================================================
  // US Trung Cards
  // ================================================================

  object Trung_US_A extends TrungCard(US, "A", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.spaceWhereCoinFPLessThanNVATroopsAndUSBaseOrTroops && US_Bot.airLiftActivity(params)) ||
        US_Bot.airStrikeActivity(params)
      }

      if (!US_Bot.any2PlusPopNotAtActiveSupportWithCoinControlUsPieces)
        TrungDraw
      else if (!US_Bot.usPiecesWith4PlusNVATroopsOrVulnerableBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.assaultOp(params)(doSpecialActivity) match {
          case Some((_, didSpecial)) => TrungComplete(didSpecial)
          case None                   => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.usPolicyIsLBJ && US_Bot.airStrikeActivity(params)) ||
        US_Bot.adviseActivity(params) ||
        US_Bot.airLiftActivity(params)
      }


      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        // Special activity comes first!
        val didSpecial = params.addSpecialActivity && doSpecialActivity()

        // If the special activty was done we return TrungComplete
        // even if the opeation was ineffective
        if (US_Bot.trainOp(params, actNum).nonEmpty || didSpecial)
          TrungComplete(didSpecial)
        else
          TrungNoOp
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_B extends TrungCard(US, "B", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.spaceWhereCoinFPLessThanNVATroopsAndUSBaseOrTroops && US_Bot.airLiftActivity(params)) ||
        US_Bot.airStrikeActivity(params)
      }

      if (!US_Bot.any2PlusPopNotAtActiveSupportWithCoinControlUsPieces)
        TrungDraw
      else if (!US_Bot.usPiecesWith4PlusNVATroopsOrVulnerableBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.assaultOp(params)(doSpecialActivity) match {
          case Some((op, didSpecial)) => TrungComplete(didSpecial)
          case None                   => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecial(): Boolean = {
        (US_Bot.usPolicyIsLBJ && US_Bot.airStrikeActivity(params)) ||
        US_Bot.airLiftActivity(params)
      }

      if (params.specialActivityOnly) {
        if (doSpecial())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val operation = if (US_Bot.allLocRoutesCanTho_HueBlockedAndNoShadedM48)
          US_Bot.patrolOp(params) orElse US_Bot.sweepOp(params)
        else
          US_Bot.sweepOp(params)

        operation match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecial())
          case None    => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_C extends TrungCard(US, "C", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.spaceWhereCoinFPLessThanNVATroopsAndUSBaseOrTroops && US_Bot.airLiftActivity(params)) ||
        US_Bot.airStrikeActivity(params)
      }

      if (!usPoints_GE_42)
        TrungDraw
      else if (!US_Bot.usPiecesWith4PlusNVATroopsOrVulnerableBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.assaultOp(params)(doSpecialActivity) match {
          case Some((op, didSpecial)) => TrungComplete(didSpecial)
          case None                   => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.usPolicyIsLBJ && US_Bot.airStrikeActivity(params)) ||
        US_Bot.adviseActivity(params) ||
        US_Bot.airLiftActivity(params)
      }

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        // Special activity comes first!
        val didSpecial = params.addSpecialActivity && doSpecialActivity()

        // If the special activty was done we return TrungComplete
        // even if the opeation was ineffective
        if (US_Bot.trainOp(params, actNum).nonEmpty || didSpecial)
          TrungComplete(didSpecial)
        else
          TrungNoOp
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_D extends TrungCard(US, "D", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.spaceWhereCoinFPLessThanNVATroopsAndUSBaseOrTroops && US_Bot.airLiftActivity(params)) ||
        US_Bot.airStrikeActivity(params)
      }

      if (!US_Bot.twoOrMoreSpacesWithSupportAndUndergroundGuerrillas)
        TrungDraw
      else if (!US_Bot.usPiecesWith4PlusNVATroopsOrVulnerableBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.assaultOp(params)(doSpecialActivity) match {
          case Some((op, didSpecial)) => TrungComplete(didSpecial)
          case None                   => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.usPolicyIsLBJ && US_Bot.airStrikeActivity(params)) ||
        US_Bot.airLiftActivity(params)
      }

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.sweepOp(params) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_E extends TrungCard(US, "E", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.spaceWhereCoinFPLessThanNVATroopsAndUSBaseOrTroops && US_Bot.airLiftActivity(params)) ||
        US_Bot.airStrikeActivity(params)
      }

      if (!US_Bot.usPiecesWithVulnerableEnemies)
        TrungDraw
      else if (!US_Bot.usPiecesWith4PlusNVATroopsOrVulnerableBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.assaultOp(params)(doSpecialActivity) match {
          case Some((op, didSpecial)) => TrungComplete(didSpecial)
          case None                   => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        US_Bot.adviseActivity(params) ||
        (US_Bot.usPolicyIsLBJ && US_Bot.airStrikeActivity(params)) ||
        US_Bot.airLiftActivity(params)
      }

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        US_Bot.trainOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_F extends TrungCard(US, "F", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (US_Bot.usPolicyIsLBJ && US_Bot.airStrikeActivity(params)) ||
        US_Bot.airLiftActivity(params)
      }

      if (!US_Bot.allUSBasesinSouthWithUSTroopsNoNVATroops)
        TrungDraw
      else if (params.specialActivityOnly) {
        // No need to check the flip card condition
        // for special only.
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val didSpecial = params.addSpecialActivity && doSpecialActivity()
        if (!US_Bot.allLocRoutesCanTho_HueBlockedAndNoShadedM48)
          flipCard(params, didSpecial)
        else
          US_Bot.patrolOp(params) match {
            case Some(_) => TrungComplete(didSpecial)
            case None    => TrungNoOp
          }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      val operation = if (US_Bot.any2PopNotAtActiveSupportWithCoinControlUsTroops)
        US_Bot.trainOp(params, actNum)
      else
        US_Bot.sweepOp(params)

      // The special activity will have been done
      // on the front side of the card.
      if (specialDone || operation.nonEmpty)
        TrungComplete(specialDone)
      else
        TrungNoOp
    }
  }


  // ================================================================
  // ARVN Trung Cards
  // ================================================================

  object Trung_ARVN_G extends TrungCard(ARVN, "G", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        ARVN_Bot.transportActivity(params) ||
        ARVN_Bot.raidActivity(params)
      }


      if (!ARVN_Bot.threeD6_LE_AvailARVN)
        TrungDraw
      else if (!ARVN_Bot.arvnAssaultWouldAddControlOrUnblockCanTo_Hue)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        ARVN_Bot.assaultOneSpaceOpMostFirepower(params) match {
          case Some(firstName) =>
            val didSpecial = params.addSpecialActivity && doSpecialActivity()
            // Now try other assaults
            ARVN_Bot.assaultOp(params, actNum, Set(firstName))
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None =>
            TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecial(): Boolean =
        ARVN_Bot.governActivity(params) ||
        ARVN_Bot.transportActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecial())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        ARVN_Bot.trainOp(params, actNum) match {
          case Some(_) =>
            val didSpecial = params.addSpecialActivity && doSpecial()
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_H extends TrungCard(ARVN, "H", actNum = 3) {


    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = ARVN_Bot.transportActivity(params)

      if (!ARVN_Bot.fiveArvnTroopsPlusRangersInAnySpace)
        TrungDraw
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val didSpecial = params.addSpecialActivity && doSpecialActivity()

        if (ARVN_Bot.arvnAssaultWouldAddControlOrUnblockCanTo_Hue) {
          ARVN_Bot.assaultOp(params, actNum)
          ARVN_Bot.armoredCavalryAssault()
          TrungComplete(didSpecial)
        }
        else
          flipCard(params, didSpecial) // Let back of card know if we did Transport activity
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      // Special Activity (Transport) is selected on the Front of the card!
      val operation = if (ARVN_Bot.threeD6_LE_AvailARVN)
        ARVN_Bot.trainOp(params, actNum)
      else
        ARVN_Bot.assaultOp(params, actNum)

      // If the special activity was done, return TrungComplete
      // even if the operation was ineffective
      if (operation.nonEmpty || specialDone) {
        ARVN_Bot.armoredCavalryAssault()
        TrungComplete(specialDone)
      }
      else
        TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_J extends TrungCard(ARVN, "J", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean =
        ARVN_Bot.governActivity(params) ||
        ARVN_Bot.transportActivity(params)

      if (!ARVN_Bot.any2PopSpaceWithSupportAndMoreArvnCubesThanUsCubes)
        TrungDraw
      else if (!ARVN_Bot.threeD6_LE_AvailARVN)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        ARVN_Bot.trainOp(params, actNum) match {
          case Some(_) if params.addSpecialActivity =>
            val didSpecial = doSpecialActivity()
            ARVN_Bot.armoredCavalryAssault()  // In case we transported
            TrungComplete(didSpecial)

          case Some(_) => TrungComplete(false)
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecial(op: CoinOp): Boolean =
        (op == Patrol && ARVN_Bot.governActivity(params)) ||
        ARVN_Bot.raidActivity(params)                     ||
        ARVN_Bot.transportActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecial(Patrol))
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val operation = if (allLocRoutesCanTho_HueBlocked)
          ARVN_Bot.patrolOp(params) orElse ARVN_Bot.sweepOp(params, actNum)
        else
          ARVN_Bot.sweepOp(params, actNum)

        operation match {
          case Some(op) =>
            val didSpecial = params.addSpecialActivity && doSpecial(op)
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_K extends TrungCard(ARVN, "K", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean =
        ARVN_Bot.governActivity(params) ||
        ARVN_Bot.raidActivity(params)   ||
        ARVN_Bot.transportActivity(params)

      if (!usPoints_GE_42)
        TrungDraw
      else if (!allLocRoutesCanTho_HueBlocked)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val didSpecial = params.addSpecialActivity && doSpecialActivity()

        ARVN_Bot.patrolOp(params) match {
          case Some(_) =>
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecial(op: CoinOp): Boolean =
        (op == Train && ARVN_Bot.governActivity(params)) ||
        ARVN_Bot.transportActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecial(Train))
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val operation = if (ARVN_Bot.arvnAssaultWouldAddCoinControlToASpace)
          ARVN_Bot.assaultOp(params, actNum)
        else
          ARVN_Bot.trainOp(params, actNum)

        operation match {
          case Some(op) =>
            val didSpecial = params.addSpecialActivity && doSpecial(op)
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_L extends TrungCard(ARVN, "L", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean =
        ARVN_Bot.governActivity(params) ||
        ARVN_Bot.transportActivity(params)

      if (!ARVN_Bot.nvaBaseOrNvaControlAt2PlusPop)
        TrungDraw
      else if (!ARVN_Bot.threeD6_LE_AvailARVN)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        ARVN_Bot.trainOp(params, actNum) match {
          case Some(_) =>
            val didSpecial = params.addSpecialActivity && doSpecialActivity()
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean =
        ARVN_Bot.raidActivity(params) ||
        ARVN_Bot.transportActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val operation = if (ARVN_Bot.arvnAssaultWouldAddControlOrUnblockCanTo_Hue)
          ARVN_Bot.assaultOp(params, actNum)
        else
          ARVN_Bot.sweepOp(params, actNum)

        operation match {
          case Some(_) =>
            val didSpecial = params.addSpecialActivity && doSpecialActivity()
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_M extends TrungCard(ARVN, "M", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean =
        ARVN_Bot.transportActivity(params) ||
        ARVN_Bot.governActivity(params)

      if (game.nvaPoints < 14)
        TrungDraw
      else if (!ARVN_Bot.threeD6_LE_AvailARVN)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        ARVN_Bot.trainOp(params, actNum) match {
          case Some(_) =>
            val didSpecial = params.addSpecialActivity && doSpecialActivity()
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean =
        (allLocRoutesCanTho_HueBlocked && ARVN_Bot.governActivity(params)) ||
        ARVN_Bot.raidActivity(params) ||
        ARVN_Bot.transportActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val operation = if (allLocRoutesCanTho_HueBlocked)
          ARVN_Bot.patrolOp(params) orElse ARVN_Bot.sweepOp(params, actNum)
        else
          ARVN_Bot.sweepOp(params, actNum)

        operation match {
          case Some(_) =>
            val didSpecial = params.addSpecialActivity && doSpecialActivity()
            ARVN_Bot.armoredCavalryAssault()
            TrungComplete(didSpecial)

          case None => TrungNoOp
        }
      }
    }
  }


  // ================================================================
  // NVA Trung Cards
  // ================================================================

  object Trung_NVA_N extends TrungCard(NVA, "N", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      if (!NVA_Bot.threeD6_LE_AvailNVATroops)
        TrungDraw
      else if (!NVA_Bot.sixTroopsWithCOINTroopsOrBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        val success = NVA_Bot.attackOp(params, actNum, addAmbush = true).nonEmpty ||
                      NVA_Bot.bombardActivity(params)
        if (success)
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val result = NVA_Bot.attackOp(params, actNum, addAmbush = params.addSpecialActivity)

        result match {
          case Some((_, true)) => TrungComplete(true)
          case Some(_)         => TrungComplete(params.addSpecialActivity && NVA_Bot.bombardActivity(params))
          case _               => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean =
        NVA_Bot.infiltrateActivity(params, needDiceRoll = false, replaceVCBase = false)

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        if (NVA_Bot.eightTroopsOutsideSouth) {
          val infiltrated = params.addSpecialActivity && doSpecialActivity()

          NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = false) match {
            case Some(_)             => TrungComplete(infiltrated)
            case None if infiltrated => TrungComplete(infiltrated) // SA only
            case None                => TrungNoOp
          }
        }
        else {
          NVA_Bot.rallyOp(params, actNum) match {
            case Some(_) =>
              val infiltrated = params.addSpecialActivity && doSpecialActivity()
              TrungComplete(infiltrated)

            case None =>
              TrungNoOp
          }
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_NVA_P extends TrungCard(NVA, "P", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = NVA_Bot.bombardActivity(params)

      if (!usPoints_GE_42)
        TrungDraw
      else if (!NVA_Bot.atleastTwentyNVATroopsOnMap)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val bombarded = params.addSpecialActivity && doSpecialActivity()

        val operation = if (NVA_Bot.sixTroopsWithCOINTroopsOrBase)
          NVA_Bot.attackOp(params, actNum, addAmbush = false) map (_._1)
        else
          NVA_Bot.marchOp(params, marchActNum, withLoC = false, withLaosCambodia = false)

        if (bombarded || operation.nonEmpty)
          TrungComplete(bombarded)
        else
          TrungNoOp
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean =
        NVA_Bot.infiltrateActivity(params, needDiceRoll = false, replaceVCBase = false)

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        if (NVA_Bot.twoD6_LE_AvailNVAGuerrillas) {
          NVA_Bot.rallyOp(params, actNum) match {
            case Some(_) =>
              val infiltrated = params.addSpecialActivity && doSpecialActivity()
              TrungComplete(infiltrated)

            case None =>
              TrungNoOp
          }
        }
        else {
          val infiltrated = params.addSpecialActivity && doSpecialActivity()
          val operation = NVA_Bot.marchOp(params, marchActNum, withLoC = false, withLaosCambodia = false)
          if (infiltrated || operation.nonEmpty)
            TrungComplete(infiltrated)
          else
            TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_NVA_Q extends TrungCard(NVA, "Q", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      if (!NVA_Bot.twoD6_LE_AvailNVAGuerrillas && !NVA_Bot.d3_GE_Trail)
        TrungDraw
      else if (!NVA_Bot.sixTroopsWithCOINTroopsOrBase)
        flipCard(params)
      else {
        val result = NVA_Bot.attackOp(params, actNum, addAmbush = params.addSpecialActivity)
        result match {
          case Some((_, true))  => TrungComplete(true)
          case Some((_, false)) => TrungComplete(params.addSpecialActivity && NVA_Bot.bombardActivity(params))
          case None             => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        NVA_Bot.infiltrateActivity(params, needDiceRoll = true, replaceVCBase = true) ||
        NVA_Bot.bombardActivity(params)        
      }

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        NVA_Bot.rallyOp(params, actNum) match {
          case Some(_) if params.addSpecialActivity =>
            val didSpecial = NVA_Bot.infiltrateActivity(params, needDiceRoll = true, replaceVCBase = true) ||
                             NVA_Bot.bombardActivity(params)
            TrungComplete(didSpecial)

          case Some(_) =>
            TrungComplete(false)

          case None =>
            TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_NVA_R extends TrungCard(NVA, "R", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean =
        NVA_Bot.infiltrateActivity(params, needDiceRoll = false, replaceVCBase = true)

      if (!usPoints_GE_42)  // Support + available US Troops and Bases
        TrungDraw
      else if (!NVA_Bot.threeD6_LE_AvailNVATroops)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val infiltrated = params.addSpecialActivity && doSpecialActivity()
        val operation = NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = true)

        if (infiltrated || operation.nonEmpty)
          TrungComplete(infiltrated)
        else
          TrungNoOp
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      if (params.specialActivityOnly) {
        // Ambush is not possible since we are not actually marching!
        if (NVA_Bot.bombardActivity(params))
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        if (NVA_Bot.undergroundGuerrillasWithSupport) {
          NVA_Bot.terrorOp(params, actNum) match {
            case Some(_) =>
              val bombarded = params.addSpecialActivity && NVA_Bot.bombardActivity(params)
              TrungComplete(bombarded)

            case None =>
              TrungNoOp
          }
        }
        else {
          def doSpecialActivity(): Boolean = {
            if (NVA_Bot.canFollowMarchWithAmbush) {
              ambushActivity(NVA, NVA_Bot.nvaMarchAmbushCandidates, March, actNum, params, false)
              true
            }
            else
              NVA_Bot.bombardActivity(params)
          }

          NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = true) match {
            case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
            case None    => TrungNoOp
          }
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_NVA_S extends TrungCard(NVA, "S", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean =
        NVA_Bot.infiltrateActivity(params, needDiceRoll = true, replaceVCBase = true) ||
        NVA_Bot.bombardActivity(params)

      if (!NVA_Bot.pop2WithoutCoinControl)
        TrungDraw
      else if (!NVA_Bot.twoD6_LE_AvailNVAGuerrillas && !NVA_Bot.d3_GE_Trail)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        NVA_Bot.rallyOp(params, actNum) match {
          case Some(_) if (params.addSpecialActivity) =>
            TrungComplete(doSpecialActivity())

          case Some(_) =>
            TrungComplete(false)

          case None =>
            TrungNoOp
        }
      }

    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      if (params.specialActivityOnly) {
        val success = NVA_Bot.attackOp(params, actNum, addAmbush = true).nonEmpty ||
                      NVA_Bot.bombardActivity(params)
        if (success)
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        def doSpecialActivity(op: InsurgentOp): Boolean = {
          if (op == March && NVA_Bot.canFollowMarchWithAmbush) {
            ambushActivity(NVA, NVA_Bot.nvaMarchAmbushCandidates, op, actNum, params, false)
            true
          }
          else
            NVA_Bot.bombardActivity(params)
        }


        val result = if (NVA_Bot.sixTroopsWithCOINPieces)
          NVA_Bot.attackOp(params, actNum, addAmbush = params.addSpecialActivity)
        else
          NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = true) map (_ -> false)

        result match {
          case Some((Attack, true)) => TrungComplete(true)
          case Some((op, _))        => TrungComplete(params.addSpecialActivity && doSpecialActivity(op))
          case None                 => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_NVA_T extends TrungCard(NVA, "T", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      if (!NVA_Bot.atleastTwentyNVATroopsOnMap)
        TrungDraw
      else if (!NVA_Bot.sixTroopsWithCOINTroopsOrBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        val success = NVA_Bot.attackOp(params, actNum, addAmbush = true).nonEmpty ||
                      NVA_Bot.bombardActivity(params)
        if (success)
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val result = NVA_Bot.attackOp(params, actNum, addAmbush = params.addSpecialActivity)

        result match {
          case Some((_, true)) => TrungComplete(true)
          case Some(_)         => TrungComplete(params.addSpecialActivity && NVA_Bot.bombardActivity(params))
          case None            => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(tryInfiltrate: Boolean) =
        (tryInfiltrate && NVA_Bot.infiltrateActivity(params, needDiceRoll = true, replaceVCBase = true)) ||
         NVA_Bot.bombardActivity(params)

      val underGroundWithSupport = (sp: Space) =>
        sp.pieces.has(NVAGuerrillas_U) && sp.support > Neutral
      val undergroundIn2PlusSpacesWithSupport: Boolean =
        (game.nonLocSpaces count underGroundWithSupport) > 1

      if (params.specialActivityOnly) {
        if (doSpecialActivity(undergroundIn2PlusSpacesWithSupport == false))
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        if (undergroundIn2PlusSpacesWithSupport) {
          NVA_Bot.terrorOp(params, actNum) match {
            case Some(_) =>
              val didSpecial = params.addSpecialActivity && doSpecialActivity(false)
              TrungComplete(didSpecial)
            case None    =>
              TrungNoOp
          }
        }
        else {
          val didSpecial = params.addSpecialActivity && doSpecialActivity(true)
          val operation = NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = false)

          if (didSpecial || operation.nonEmpty)
            TrungComplete(didSpecial)
          else
            TrungNoOp
        }
      }
    }
  }


  // ================================================================
  // VC Trung Cards
  // ================================================================

  object Trung_VC_U extends TrungCard(VC, "U", actNum = 2) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {

      def doSpecialActivity(): Boolean = {
        (VC_Bot.patronage_GE_17 && VC_Bot.subvertActivity(params)) || VC_Bot.taxActivity(params)
      }

      if (!VC_Bot.threePlusGuerrillasInSpace)
        TrungDraw
      else if (!VC_Bot.undergroundAtNoActiveOpposition)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        VC_Bot.terrorOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {

      def doSpecialActivity(op: InsurgentOp): Boolean = {
        if (op == March && VC_Bot.canFollowMarchWithAmbush) {
          ambushActivity(VC, VC_Bot.vcMarchAmbushCandidates, op, actNum, params, false)
          true
        }
        else
          (VC_Bot.twoD6GTAgitateTotal && VC_Bot.taxActivity(params)) ||
          VC_Bot.subvertActivity(params)
      }

      // ------------------------------------------------------------
      // Back Operation
      // ------------------------------------------------------------
      if (params.specialActivityOnly) {
        if (doSpecialActivity(Rally))
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val operation = if (VC_Bot.threeD6_LE_AvailVCPieces)
          VC_Bot.rallyOp(params, actNum)
        else
          VC_Bot.marchOp(params, actNum)

        operation match {
          case Some(op) => TrungComplete(params.addSpecialActivity && doSpecialActivity(op))
          case None     => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_VC_V extends TrungCard(VC, "V", actNum = 2) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (VC_Bot.twoD6GTAgitateTotal && VC_Bot.taxActivity(params)) ||
        VC_Bot.subvertActivity(params)
      }

      if (!VC_Bot.undergroundAtNoActiveOpposition)
        TrungDraw
      else if (!VC_Bot.threeD6_LE_AvailVCPieces)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        VC_Bot.rallyOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean =
        VC_Bot.subvertActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val (operation, ambushed) = if (VC_Bot.threePlusGuerrillasWithUSTroopsNoVCBase)
          VC_Bot.attackOp(params, actNum)
        else
          (VC_Bot.terrorOp(params, actNum), false)

        operation match {
          case Some(Attack) => TrungComplete(ambushed)
          case Some(Terror) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case _            => TrungNoOp
        }
      }
    }
  }


  // ---------------------------------------------------------------
  object Trung_VC_W extends TrungCard(VC, "W", actNum = 2) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (VC_Bot.patronage_GE_17 && VC_Bot.subvertActivity(params)) || VC_Bot.taxActivity(params)
      }

      if (!VC_Bot.undergroundWithUSTroops)
        TrungDraw
      else if (!VC_Bot.threeD6_LE_AvailVCPieces)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        VC_Bot.rallyOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean =
        VC_Bot.subvertActivity(params)

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val (operation, ambushed) = if (VC_Bot.undergroundAtNoActiveOpposition)
          (VC_Bot.terrorOp(params, actNum), false)
        else
          VC_Bot.attackOp(params, actNum)

        operation match {
          case Some(Attack) => TrungComplete(ambushed)
          case Some(Terror) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case _            => TrungNoOp
        }
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_VC_X extends TrungCard(VC, "X", actNum = 2) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (VC_Bot.twoD6GTAgitateTotal && VC_Bot.taxActivity(params)) ||
        VC_Bot.subvertActivity(params)
      }

      // ------------------------------------------------------------
      // Front Operation
      // ------------------------------------------------------------
      if (!VC_Bot.pop2SpaceWithoutGuerrillas)
        TrungDraw
      else if (!VC_Bot.threeD6_LE_AvailVCPieces)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        VC_Bot.rallyOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        if (VC_Bot.canFollowMarchWithAmbush) {
          ambushActivity(VC, VC_Bot.vcMarchAmbushCandidates, March, actNum, params, false)
          true
        }
        else
          (VC_Bot.patronage_GE_17 && VC_Bot.subvertActivity(params)) ||
          VC_Bot.taxActivity(params)
      }

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else
        VC_Bot.marchOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
    }

  }

  // ---------------------------------------------------------------
  object Trung_VC_Y extends TrungCard(VC, "Y", actNum = 2) {

    // ------------------------------------------------------------
    //Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = VC_Bot.taxActivity(params)

      if (!VC_Bot.threeD6_LE_AvailVCPieces)
        TrungDraw
      else if (!VC_Bot.threePlusGuerrillasWithUSTroopsNoVCBase)
        flipCard(params)
      else if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val (operation, ambushed) = VC_Bot.attackOp(params, actNum)

        operation match {
          case Some(_) if ambushed => TrungComplete(true)
          case Some(_)             => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case _                   => TrungNoOp
        }
      }
    }


    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (VC_Bot.patronage_GE_17 && VC_Bot.subvertActivity(params)) || VC_Bot.taxActivity(params)
      }

      if (params.specialActivityOnly) {
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else
        VC_Bot.rallyOp(params, actNum) match {
          case Some(_) => TrungComplete(params.addSpecialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
    }
  }

  // ---------------------------------------------------------------
  object Trung_VC_Z extends TrungCard(VC, "Z", actNum = 2) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      def doSpecialActivity(): Boolean = {
        (VC_Bot.patronage_GE_17 && VC_Bot.subvertActivity(params)) || VC_Bot.taxActivity(params)
      }

      if (!VC_Bot.fifteenPlusVCGuerrilasOnMap)
        TrungDraw
      else if (params.specialActivityOnly) {
        // No need to check the flip card condition
        // for special activity only
        if (doSpecialActivity())
          TrungComplete(true)
        else
          TrungNoOp
      }
      else {
        val specialDone = params.addSpecialActivity && doSpecialActivity()

        if (VC_Bot.threePlusGuerrillasInSpace) {
          val operation = VC_Bot.marchOp(params, actNum)
          if (specialDone || operation.nonEmpty)
            TrungComplete(specialDone)
          else
            TrungNoOp
        }
        else
          flipCard(params, specialDone)
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    //  NOTE:  This flip side is only executed if the Special Activity
    //         from the front side has already been attempted.
    //         If the Special Activity was done and we cannot find
    //         any valid spaces for the Operation we must still
    //         return TrungComplete(true)
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      val operation = if (VC_Bot.undergroundAtNoActiveOpposition)
        VC_Bot.terrorOp(params, actNum)
      else
        VC_Bot.marchOp(params, actNum)

      // This special activity will have been done on the front
      // of the card
      if (operation.nonEmpty || specialDone)
        TrungComplete(specialDone)
      else
        TrungNoOp
    }
  }
}
