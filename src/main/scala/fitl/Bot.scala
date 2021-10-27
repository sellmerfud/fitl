
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

object Bot {

  // Value to indicate that space selection is unlimited.
  private val NO_LIMIT = 1000;

  // These values are used with HighestScore and LowestScore filters
  // when a candidate is not qualified to produce a score.
  // For example if it is a LoC and we are only considering
  // Citys and Provinces.
  private val NO_SCORE  = -1000

  def botLog(msg: => String) = if (game.botLogging) log(msg)
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
  def botLogChoices(choices: => List[(Boolean, String)]): Unit = {

    def logNext(remaining: List[(Boolean, String)]): Unit = remaining match {
      case Nil =>
      case (cond, msg)::xs if cond =>
        log(msgResult(cond, msg))
      case (cond, msg)::xs =>
        log(msgResult(cond, msg))
        logNext(xs)
    }

    if (game.botLogging)
      logNext(choices)
  }

  case class Params(
    specialActivity: Boolean        = false, // May select a Special Activity
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    assaultRemovesTwoExtra: Boolean = false, // M48 Patton (unshaded)
    onlyIn: Option[Set[String]]     = None   // Limit command to the given spaces
  ) {
    val limOpOnly = maxSpaces == Some(1)

    def spaceAllowed(name: String) = {
      (onlyIn map (allowed =>  allowed.contains(name)) getOrElse true)
    }
  }

  class MovingGroups() {
    // Map Space Name, to Pieces in that space that cannot move.
    var groups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())

    def reset(): Unit = groups = Map.empty.withDefaultValue(Pieces())
    def apply(name: String): Pieces = groups(name)
    def add(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) + pieces)
    def remove(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) - pieces)

    def spaces = groups.keys.toSet
    def toList = groups.toList.sortBy(_._1)
    def allPieces = toList.foldLeft(Pieces()) { (all, group) => all + group._2 }
    def size   = groups.size
  }

  // Variable Global within the Bot object
  // ---------------------------------------
  // Used to keep track of moveDestinations during and operation
  // The Bots will never move pieces out of a space that has been
  // selected as a move destination.
  private var moveDestinations = Set.empty[String]

  // Keeps track of all pieces that actually moved
  // during March/Sweep/Patrol/Transport/Air Lift..
  private var movedPieces      = new MovingGroups()

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
      log("\nTrung card flipped to its back side")
      log(separator())
      log(display(id * 2))
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

  def logOpChoice(faction: Faction, op: Operation, notes: TraversableOnce[String] = Nil): Unit = {
    log(s"\n$faction chooses $op operation")
    log(separator())
    for (note <- notes)
      log(note)
  }

  def logNoOp(faction: Faction, op: Operation): Unit = {
      log(s"\nNo spaces found for $faction $op")
  }

  def logSAChoice(faction: Faction, sa: SpecialActivity, notes: TraversableOnce[String] = Nil): Unit = {
    log(s"\n$faction chooses $sa special activity")
    log(separator())
    for (note <- notes)
      log(note)
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

  // Convenience method
  def checkActivation(faction: Faction, needRoll: Boolean, actNum: Int): Boolean = {
    needRoll == false || makeActivationRoll(faction, actNum)
  }

  def sweepEffective(faction: Faction, name: String): Boolean = {
    val sp = game.getSpace(name)
    sp.sweepActivations(faction) > 0 && sp.pieces.totalOf(UndergroundGuerrillas) > 0
  }

  // Determine if the given faction can effectively Ambush
  // from the given space
  def canAmbushFrom(faction: Faction)(sp: Space): Boolean = {
    val GType = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U

    sp.pieces.has(GType) && 
    (sp.pieces.has(CoinPieces) || 
     sp.isLoC && numAdjacentPieces(sp: Space, CoinPieces) > 0)      
  }

  // When an ambush Special Activity follows a March Operation
  // The Bot may ambush only in spaces that were selected as 
  // March destination.  And the Underground guerrilla used to 
  // perform the Ambush must have marched into the space this turn.
  def marchAmbushCandidates(faction: Faction): List[String] = {
    val GType = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
    spaceNames(spaces(moveDestinations) filter { sp =>
      movedPieces(sp.name).has(GType) &&
      canAmbushFrom(faction)(sp)
    })
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
                     checkFirst: Boolean): (Set[String], Boolean) = {
    var ambushSpaces     = Set.empty[String]
    var mainForceBnsUsed = false
    val limited          = momentumInPlay(Mo_TyphoonKate) || capabilityInPlay(BoobyTraps_Unshaded)
    val maxAmbush = if (limited) 1 else 2
    val notes = List(
      noteIf(momentumInPlay(Mo_TyphoonKate),s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
      noteIf(capabilityInPlay(BoobyTraps_Unshaded), s"You may ambush in only one space [$BoobyTraps_Unshaded]")
    ).flatten

    def activateOK() = {
      val needActivation = op == Attack && (checkFirst || ambushSpaces.nonEmpty)
      checkActivation(faction, needActivation, actNum)
    }

    // Return true if we can continue the attack (ie. No activation roll failure)
    def nextAmbush(candidates: List[String]): Boolean = {
      // Re-verify that we can ambush in the spaces because
      // a previous ambush may have removed a target piece
      val validSpaces = spaces(candidates) filter canAmbushFrom(faction)
      
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
          val deadPieces = selectEnemyRemovePlaceActivate(targetPieces, num)

          if (ambushSpaces.isEmpty)
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

          revealPieces(sp.name, Pieces(vcGuerrillas_U = 1))
          removePieces(target.name, deadPieces)
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
    if (momentumInPlay(Mo_Claymores))
      (ambushSpaces, true) 
    else {
      val canContinue = nextAmbush(ambushCandidates)
      (ambushSpaces, canContinue)
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
        botLog(s"$desc: score = $high")
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
        botLog(s"$desc): score = $low")
        entries filter (score(_) == low)
      }
    }
  }

  def vulnerableBases(pieces: Pieces) = {
    if (pieces.has(UndergroundGuerrillas))
      Pieces()
    else
      pieces.only(InsurgentBases)
  }

  def vulnerableInsurgents(pieces: Pieces) = {
    val forces = pieces.only(NVATroops::ActiveGuerrillas)
    forces + vulnerableBases(pieces)
  }

  //  Assault firepower of the space plus any modifiers for
  //  capabilities, momentum, etc.
  def usFirepower(sp: Space) = {
    val firepower = sp.assaultFirepower(US)
    // Account for unshaded Search and Destroy when there would otherwise be zero firepower
    val canSearchDestroy = capabilityInPlay(SearchAndDestroy_Unshaded) &&
                           sp.pieces.has(USTroops) &&
                           sp.pieces.has(UndergroundGuerrillas)
    if (firepower == 0 && canSearchDestroy)
      1
    else
      firepower
  }

  def arvnFirepower(sp: Space) = sp.assaultFirepower(ARVN)

  def coinFirepower(sp: Space) = usFirepower(sp) + arvnFirepower(sp)

  // Returns the number of the requested pieces types that are in
  // spaces adjecent to the given space.
  def numAdjacentPieces(sp: Space, pieceTypes: TraversableOnce[PieceType]): Int = {
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
      !sp.isLoC && sp.population > 0 && sp.support > Neutral && numUS < numUnderground
    }
  )

  val HasCoinControl = new BooleanPriority[Space](
    "COIN Control",
    sp => !sp.isLoC && sp.coinControlled
  )

  val CityProvinceNoActiveSupport = new BooleanPriority[Space](
    "City or Province not at Active Support",
    sp => !sp.isLoC && sp.population > 0 && sp.support != ActiveSupport
  )

  val PoliceWithoutUSTroops = new BooleanPriority[Space](
    "Police without US Troops",
    sp => sp.pieces.has(ARVNPolice) && !sp.pieces.has(USTroops)
  )

  val COINFirepowerLessThanVulnerable = new BooleanPriority[Space](
    "COIN Firepower < vulnerable enemies where US pieces",
    sp => sp.pieces.has(USPieces) && coinFirepower(sp) < vulnerableInsurgents(sp.pieces).total
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
    sp => arvnFirepower(sp)
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
        // Get all adjacent spaces that have not yet been selecte as march destinations
        val adjacent = NVA_Bot.getNVAAdjacent(sp.name) filterNot moveDestinations.contains
        adjacent.foldLeft(0) { (totalTroops, name) =>
          totalTroops + game.getSpace(name).pieces.totalOf(NVATroops)
        }
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
    sp => !sp.isLoC && sp.population > 0 && sp.support != ActiveOpposition
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

  // Find the best candidate from the given list using the
  // prirority table represented by the list of PriorityFilters.
  //
  // The list of candidates MUST NOT be empty!
  //
  // Each filter in the list is used against the given candidates:
  // - If the result is a single candidate then that candidate is the winner
  // - If the result is more than one candidate then the resulting list is
  //   then used with the next filter in the list.
  // - If the result is that none of the candidates match the filter then
  //   the samme list of candidates is used with the next filter in the list.
  // - If we get through the entire list of filters and we still have not
  //   resolved the best candidate, then we pick randomly from those that remain.

  def bestCandidate[T](candidates: List[T], priorities: List[PriorityFilter[T]]): T = {
    assert(candidates.nonEmpty, "bestCandidate: called with empty list!")

    @tailrec def nextPriority(candidates: List[T], priorities: List[PriorityFilter[T]]): T = {
      (candidates, priorities) match {
        case (Nil, _)    => throw new IllegalArgumentException("nextPriority: empty list")
        case (sp :: Nil, _) =>
          botLog(s"Picked a winner [${sp.toString}]")
          sp
        case (best, Nil)   =>
          val sp = shuffle(best).head        // Take one at random
          botLog(s"Picked random winner [${sp.toString}]")
          sp
        case (list, f :: fs) =>
          (f filter list) match {
            case Nil =>
              botLog(s"$f: matched nothing")
              nextPriority(list, fs) // Filter entire list by next priority
            case best  =>
              botLog(s"$f: matched [${andList(best)}]")
              nextPriority(best, fs) // Filter matched list by next priority
          }
      }
    }
    nextPriority(candidates, priorities)
  }


  // // As soon as a filter finds at least one matching candidate, then the procees stops and the
  // // results from that filter are returned.
  // // If none of the filters finds at least one matching candidate we return Nil.
  // @tailrec final def selectCandidates(candidates: List[Space], filters: SpacePriorities): List[Space] = {
  //   botLog(s"selectCandidates: [${andList(candidates)}]")
  //   (candidates, filters) match {
  //     case (Nil, _) =>
  //       botLog("selectCandidates: no candidates to consider")
  //       Nil    // No candidates to consider
  //     case (_, Nil) =>
  //       botLog("selectCandidates: no candidates found")
  //       Nil    // No filter found any candidates
  //     case (xs, f::fs) =>
  //       (f filter xs) match {
  //         case Nil =>            // Filter did not match anything, try the next filter
  //           botLog(s"selectCandidates ($f): failed")
  //           selectCandidates(xs, fs)
  //         case results =>        // We got some results…
  //           botLog(s"selectCandidates ($f): [${(results map (_.name) mkString ", ")}]")
  //           results
  //       }
  //   }
  // }


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

  def humanPiecesFirst(list: List[PieceType]): List[PieceType] = {
    val human = list filter (t => game.isHuman(owner(t)))
    val bot   = list filter (t => game.isBot(owner(t)))

    human:::bot
  }

  //  After creating this list of pieces, split the list into (Human, Bot) owned
  //  the concatenate those lists so that all of the Human owned pieces are removed first.
  def selectEnemyRemovePlaceActivate(pieces: Pieces, num: Int): Pieces = {
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
    val deadForces = selectEnemyRemovePlaceActivate(coinForces, numForces)
    val numBases   = ((num - deadForces.total) max 0) min coinBases.total
    val deadBases  = selectEnemyRemovePlaceActivate(coinBases, numBases)

    (deadForces + deadBases)
  }

  //  If a Tunneled base is "vulnerable" is to be removed,
  //  we stop removing pieces.
  //  Roll a 1d6, 1-3 no effect, 4-6 remove tunnel marker
  //  Return all pieces that should be removed, true if we need to
  //  roll a die for Tunnel marker removal.
  def selectRemoveEnemyInsurgentBasesLast(pieces: Pieces, num: Int): (Pieces, Boolean) = {
    val insurgentTunnels = pieces.only(InsurgentTunnels)
    val insurgentBases  = pieces.only(InsurgentNonTunnels)
    val insurgentForces = vulnerableInsurgents(pieces)
    val numForces       = num min insurgentForces.total
    val deadForces      = selectEnemyRemovePlaceActivate(insurgentForces, numForces)
    val numBases        = ((num - deadForces.total) max 0) min insurgentBases.total
    val deadBases       = selectEnemyRemovePlaceActivate(insurgentBases, numBases)
    val checkTunnel     = num > numForces + numBases && insurgentTunnels.nonEmpty

    (deadForces + deadBases, checkTunnel)
  }
  // Function to make code clearer to read
  // just an alias for selectEnemyRemovePlaceActivate()
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
    origCandidates: Pieces,  // The original set of pieces that may be kept/moved
    selected: Pieces = Pieces()) {  // Kept pieces or moving pieces

    def addPieces(newPieces: Pieces) = copy(selected = selected + newPieces)
    def candidates = origCandidates - selected
  }

  def logKept(params: MoveParams, heading: String, result: String): Unit = if (game.botLogging) {
    import params._

    log()
    log(heading)
    log(separator())
    if (result.nonEmpty)
      log(s"result    : $result")
    log(s"candidates: ${andList(candidates.descriptions)}")
    log(s"kept      : ${andList(selected.descriptions)}")
  }

  def logMoved(params: MoveParams, heading: String, result: String): Unit = if (game.botLogging) {
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
    val firepower  = coinFirepower(origin)
    val vulnerable = vulnerableInsurgents(origin.pieces).total

    if (firepower >= vulnerable) {
      
      def keepPieces(num: Int): Pieces = {
        if (num == candidates.total)
          candidates  // Must keep all of them in place
        else {
          val toKeep = selectFriendlyToKeepInPlace(candidates, num)
          // If our firepower is still sufficent then we are done
          val updated = origin.pieces.except(moveTypes) + selected + toKeep
          if (coinFirepower(origin.setPieces(updated)) >= vulnerable)
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

    if (!origin.isLoC && origin.population > 0 && origin.support != ActiveOpposition) {
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

  // Used by US (on LoC)
  val MP_GetCoinFirepowerEqualEnemies = (params: MoveParams) => {
    import params._
    val desc = "Get COIN Firepower equal to enemies"
    val numEnemy = dest.pieces.totalOf(InsurgentPieces)
      
    def movePieces(num: Int): Pieces = {
      if (num == candidates.total)
        candidates  // Move all that we have
      else {
        val toMove = selectFriendlyToPlaceOrMove(candidates, num)
        // If our firepower is sufficent then we are done
        val updated = dest.pieces + selected + toMove

        if (coinFirepower(dest.setPieces(updated)) >= numEnemy)
          toMove
        else  // Try again moving one more
          movePieces(num + 1)
      }
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
      if (num == candidates.total)
        candidates  // Move all that we have
      else {
        val toMove = selectFriendlyToPlaceOrMove(candidates, num)
        // If our firepower is sufficent then we are done
        val updated = dest.pieces + selected + toMove

        if (arvnFirepower(dest.setPieces(updated)) >= numEnemy)
          toMove
        else  // Try again moving one more
          movePieces(num + 1)
      }
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
      val numMove   = numCoin + 1 - numGuerrillas
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
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(CoinCubes), 3 - numCoinCubes)
      val newParams = params.addPieces(toMove)

      logMoved(newParams, desc, s"move [${andList(toMove.descriptions)}]")
      newParams
    }
  }

  // Used by US in (City/Province)
  val MP_GetCoinFirepowerEqualToVulnerable = (params: MoveParams) => {
    import params._
    val desc = "Get COIN Firepower equal to vulnerable enemies"

    val numVulnerable = vulnerableInsurgents(dest.pieces).total
      
    def movePieces(num: Int): Pieces = {
      if (num == candidates.total)
        candidates  // Move all that we have
      else {
        val toMove = selectFriendlyToPlaceOrMove(candidates, num)
        // If our firepower is sufficent then we are done
        val nowAtDest    = dest.pieces + selected + toMove

        if (coinFirepower(dest.setPieces(nowAtDest)) >= numVulnerable)
          toMove
        else  // Try again moving one more
          movePieces(num + 1)
      }
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
      val toMove    = selectFriendlyToPlaceOrMove(candidates.only(USForces), needed - numUsForces)
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
      val numMove   = nowAtDest.totalOf(InsurgentPieces) + 1 - nowAtDest.totalOf(CoinPieces) 
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
      val numMove   = nowAtDest.totalOf(NonNVAPieces) + 1 - nowAtDest.totalOf(NVAPieces)
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
      val numMove   = 3 - nowAtDest.totalOf(FactionGuerrillas)
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
        val numMove   = nowAtDest.totalOf(NonNVAPieces) + 1 - nowAtDest.totalOf(NVAPieces)
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
                         moveTypes: Set[PieceType]): Pieces = {
    val us      = faction == US
    val arvn    = faction == ARVN
    val nva     = faction == NVA
    val vc      = faction == VC
    val usSweep = us && action == Sweep
    
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
      movePriIf(nva || vc,  KP_3ActingGuerrillasWithRoomForAvailableBase),
      movePriIf(nva || vc,  KP_FriendlyGuerrillasGreaterThanEnemiesOnLoCs),
      movePriIf(vc,         KP_1UndergroundIn1PopNotAtActiveSupport)
    ).flatten

    val origin     = game.getSpace(originName)
    val dest       = game.getSpace(destName)
    val candidates = origin.pieces.only(moveTypes)
    val params     = MoveParams(origin, dest, faction, action, moveTypes, candidates)
    
    logKept(params, s"$faction is selecting pieces to keep in $originName", "")
    nextKeepPriority(keepPriorities, params)
  }

  //  When moving pieces, this is used to determine which of the factions pieces should be moved
  //  form the origin to the destination.
  def selectPiecesToMove(originName: String,
                         destName: String,
                         faction: Faction,
                         action: MoveAction,
                         moveTypes: Set[PieceType],
                         mustKeep: Pieces): Pieces = {
    val us        = faction == US
    val arvn      = faction == ARVN
    val nva       = faction == NVA
    val vc        = faction == VC
    val usPSaigon = us && action == Patrol && destName == Saigon
    val arvnSPT   = arvn && (action == Sweep || action == Patrol || action == Transport)
    val origin    = game.getSpace(originName)
    val dest      = game.getSpace(destName)

    def nextMovePriority(priorities: List[MovePriority], params: MoveParams): Pieces = {
      priorities match {
        case _ if params.candidates.isEmpty => params.selected
        case Nil                            => params.selected
        case priority::rest                 => nextMovePriority(rest, priority(params))
      }      
    }
    
    val movePriorities = if (dest.isLoC)
      List(
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
      val params     = MoveParams(origin, dest, faction, action, moveTypes, candidates)
      
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
                 moveTypes: Set[PieceType]): Pieces = {
  
    // First we determine which piece to keep in the origin space
    val toKeep = selectPiecesToKeep(originName, destName, faction, action, moveTypes)
    selectPiecesToMove(originName, destName, faction, action, moveTypes, toKeep)
  }


  // Find the best space from which to move pieces to the given destination.
  // If not pieces can reach the destination, the return None.
  
  def selectMoveOrigin(
    faction: Faction,
    destName: String,
    action: MoveAction,
    moveTypes: Set[PieceType],
    previousOrigins: Set[String]): Option[String] = {

    val prohibited = (name: String) => moveDestinations(name) || previousOrigins(name)
    val adjacentNames = if (faction == NVA)
      NVA_Bot.getNVAAdjacent(destName) filterNot prohibited
    else
      getAdjacent(destName) filterNot prohibited

    val adjacentSpaces = spaces(adjacentNames)
    val priorities = List(
      new HighestScore[Space]( "Most Moveable Pieces", _.pieces.totalOf(moveTypes))
    )

    botLog(s"\nSelect origin space with moveable: [${andList(moveTypes.toList)}]")
    botLog(separator())
    if (adjacentSpaces exists (_.pieces.has(moveTypes))) {
      Some(bestCandidate(adjacentSpaces, priorities).name)
    }
    else {
      botLog(s"No adjacent spaces with moveable pieces")
      None
    }
  }

  //  This function determines if we should attempt to find another 
  //  origin space for the given move destination.
  //  The rules are faction specific and correspond to the
  //  'B' conditions at the bottom of the Move Priorities Table.
  def canSelectAnotherOrigin(faction: Faction, destName: String): Boolean = {
    val dest         = game.getSpace(destName)
    val numCoin      = dest.pieces.totalOf(CoinPieces)
    val numInsurgent = dest.pieces.totalOf(InsurgentPieces)

    (faction, dest.isLoC) match {
      case (US|ARVN, true)  => numCoin < numInsurgent
      case (NVA|VC,  true)  => numInsurgent <= numCoin
      case (US|ARVN, false) => !dest.coinControlled && 
                               coinFirepower(dest) < vulnerableInsurgents(dest.pieces).total
      case (NVA,     false) => !dest.nvaControlled
      case (VC,      false) => dest.coinControlled || dest.nvaControlled
    }
  }



  def testMove(): Unit = {

    val faction: Faction = VC
    val action: MoveAction = March

    initTurnVariables()

    log(s"\n$faction selects $action operation")
    log(separator())

    faction match {
      case US =>
        movePiecesToDestinations(US, Sweep, Set(USTroops), false) {
          (_, _, prohibited) => {
            val candidates = game.spaces filterNot (sp => prohibited.contains(sp.name))
            if (candidates.nonEmpty)
              Some(US_Bot.pickSpaceSweepDest(candidates, true).name)
            else
              None
          }
        }

      case ARVN =>
        movePiecesToDestinations(ARVN, Sweep, Set(ARVNTroops), false) {
          (_, activationRoll, prohibited) => {
            val candidates = game.spaces filterNot (sp => prohibited.contains(sp.name))
            if (candidates.nonEmpty && checkActivation(ARVN, activationRoll, 3))
              Some(ARVN_Bot.pickSpaceSweepTransportDest(candidates).name)
            else
              None
          }
        }

      case NVA =>
        // 1 LoC adjacent to US without NVA
        // Then spaces using March Priorities 
        movePiecesToDestinations(NVA, March, NVAForces.toSet, false) {
          (_, needActivation, prohibited) => {
            val locQualifies = (sp: Space) => !prohibited(sp.name) && !sp.pieces.has(NVAPieces) && numAdjacentPieces(sp, USPieces) > 0
            lazy val locCandidates = game.locSpaces filter locQualifies
            lazy val candidates = game.spaces filterNot (sp => prohibited.contains(sp.name))

            if (moveDestinations.size < 1 && locCandidates.nonEmpty)
              shuffle(locCandidates).headOption map (_.name)
            else if (candidates.nonEmpty && checkActivation(NVA, needActivation, 2))
              Some(NVA_Bot.pickSpaceMarchDest(candidates).name)
            else
              None
          }
        }
        
        case VC =>
          // 2 LoCs adjacent most underground VC guerrillas
          // Then spaces using March Priorities 
          movePiecesToDestinations(VC, March, VCGuerrillas.toSet, false) {
            (_, needActivation, prohibited) => {
              val locQualifies = (sp: Space) => !prohibited(sp.name) && numAdjacentPieces(sp, Set(VCGuerrillas_U)) > 0
              lazy val locCandidates = game.locSpaces filter locQualifies
              lazy val candidates = game.spaces filterNot (sp => prohibited.contains(sp.name))

              // No need to check activation since these are LoCs
              if (moveDestinations.size < 2 && locCandidates.nonEmpty) {
                val priorities = List(
                  new HighestScore[Space]("Most Underground VC Guerrillas", sp => numAdjacentPieces(sp, Set(VCGuerrillas_U)))
                )
                Some(bestCandidate(locCandidates, priorities).name)
              }
              else if (candidates.nonEmpty && checkActivation(VC, needActivation, 2)) {
                // Spaces using March Destinations column of Space Selection
                Some(VC_Bot.pickSpaceMarchDest(candidates).name)
              }
              else
                None
            }
          }
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
  //  Note:  This function is NOT used for ARVN Transport
  //  Returns true if any following moves would need an activation roll.
  def movePiecesToDestinations(
    faction: Faction,
    action: MoveAction,
    moveTypes: Set[PieceType],
    checkFirst: Boolean,   // True if we need to check activate before the first dest
    maxDestinations: Option[Int] = None)(getNextDestination: MoveDestGetter): Boolean = {

    val maxDest = maxDestinations getOrElse NO_LIMIT
    // ----------------------------------------
    def tryOrigin(destName: String, previousOrigins: Set[String]): Unit = {
      selectMoveOrigin(faction, destName, action, moveTypes, previousOrigins) match {
        case None =>  // No more origin spaces, so we are finished

        case Some(originName) =>
          val toMove = movePiecesFromOneOrigin(originName, destName, faction, action, moveTypes)
          if (toMove.nonEmpty) {
            //  First time we move pieces to a dest log
            //  the selection of the destination space.
            if (!moveDestinations.contains(destName)) {
              log(s"\n$faction selects $destName as $action destination")
              log(separator())
            }
            movePieces(toMove, originName, destName)
            movedPieces.add(destName, toMove)

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
              }
            }
            //  The dest space can no longer be considered for
            //  a destination or for an origin
            moveDestinations = moveDestinations + destName
          }
          else
            botLog(s"\nNo pieces moved from $originName to $destName")
          // Check to see if we should try another origin
          // for this destination space
          if (canSelectAnotherOrigin(faction, destName)) {
            botLog(s"\n$faction Bot will select another origin space for destination: $destName")
            tryOrigin(destName, previousOrigins + originName)
          }
      }
    }

    // ----------------------------------------
    def tryDestination(lastWasSuccess: Boolean, needActivationRoll: Boolean, notReachable: Set[String]): Boolean = {
      if (moveDestinations.size < maxDest) {
        getNextDestination(lastWasSuccess, needActivationRoll, moveDestinations ++ notReachable) match {
          case None => // No more destinations
            needActivationRoll  // Return so subsequent calls will know if activation is needed

          case Some(destName) => 
            botLog(s"\n$faction Bot will attempt $action to $destName")
            tryOrigin(destName, Set.empty)
            // pause()
            val dest = game.getSpace(destName)

            if (action == Sweep && !moveDestinations(destName) && sweepEffective(faction, destName)) {
              // If this is a sweep operation and no cubes were
              // able to move into the destination, but there are
              // sufficient existing cubes in the destination to
              // activate at least 1 underground guerrilla, then
              // add the destName to the moveDestinations
              // to allow the Sweep operation to perform a
              // Sweep in Place.
              moveDestinations = moveDestinations + destName
              log(s"\n$faction selects $destName to Sweep in Place")
            }

            if (moveDestinations(destName)) {
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

    def any2PlusPopNotAtActiveSupportWithCoinControlUsPieces: Boolean =
      game.nonLocSpaces exists { sp =>
        sp.population >= 2          &&
        sp.support != ActiveSupport &&
        sp.coinControlled           &&
        sp.pieces.has(USPieces)
      }

    def twoOrMoreSpacesWithSupportAndUndergroundGuerrillas: Boolean =
      game.nonLocSpaces exists { sp =>
        sp.population > 0           &&
        sp.support > Neutral        &&
        sp.pieces.has(UndergroundGuerrillas)
      }

    def usPiecesWith4PlusNVATroopsOrVulnerableBase: Boolean =
      game.spaces exists { sp =>
        sp.pieces.has(USPieces) &&
        (sp.pieces.totalOf(NVATroops) > 3 ||
         (sp.pieces.has(InsurgentBases) && !sp.pieces.has(UndergroundGuerrillas)))
      }

    // All routes from Can Tho to Hue blocked and Shaded M-48 not in effect
    def allLocRoutesCanTho_HueBlockedAndNoShadedM48: Boolean =
      !capabilityInPlay(M48Patton_Shaded) && !getPatrolDestinations(CanTho).contains(Hue)

    def usPiecesWithVulnerableEnemies: Boolean =
      game.spaces exists { sp =>
        sp.pieces.has(USPieces) && vulnerableInsurgents(sp.pieces).nonEmpty
      }

    def allUSBasesinSouthWithUSTroopsNoNVATroops: Boolean = {
      val candidates = game.nonLocSpaces filter { sp =>
        isInSouthVietnam(sp.name) && sp.pieces.has(USBase)
      }

      candidates forall (sp => sp.pieces.has(USTroops) && !sp.pieces.has(NVATroops))
    }

    // US Spaces Priorities: Shift Toward Active Support
    def pickSpaceTowardActiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             CityProvinceNoActiveSupport),
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botLog(s"\nUS Select space (Shift Toward Active Support): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nUS Select space (Place Bases): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

    // US Spaces Priorities: Place Cubes or Special Forces
    def pickSpacePlaceCubes(candidates: List[Space], placingMovingUSTroops: Boolean = false): Space = {

      val priorities = List(
        filterIf(true,                  ProvinceWithUSBaseAndCubes),
        filterIf(true,                  CityProvinceNoActiveSupport),
        filterIf(placingMovingUSTroops, PoliceWithoutUSTroops),
        filterIf(true,                  COINFirepowerLessThanVulnerable),
        filterIf(true,                  MostPopulation),
        filterIf(true,                  HasEnemyBase)
      ).flatten

      botLog(s"\nUS Select space (Place Cubes or Special Forces): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nUS Select space (Sweep Destinations): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nUS Select space (Air Lift Destinations): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

    // US Pick space for: Air Strike
    // TODO:  How do we handle the "trail" ?
    //        It is the first conditon in the table so perhaps
    //        the caller can sort this out before calling this function.
    def pickSpaceAirStrike(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,                  VulnerableBase),
        filterIf(true,                  ProvinceWithoutCOINBase),
        filterIf(true,                  COINFirepowerLessThanVulnerable),
        filterIf(game.isHuman(VC),      MostTotalOpposition),
        filterIf(true,                  IsHighlandProvince)
      ).flatten

      botLog(s"\nUS Select space (Air Strike): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nUS Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

  }


  // ================================================================
  // ARVN Specific code
  // ================================================================
  object ARVN_Bot {

    //  If we are tracking ARVN resources, then after passing an activation
    //  roll we must also make sure that there are sufficient ARVN resources
    //  to pay for the operation.
    def checkARVNActivation(needRoll: Boolean, actNum: Int, free: Boolean) = 
      checkActivation(ARVN, needRoll, actNum) &&
      (free || !game.trackResources(ARVN) || game.arvnResources >= 3)

    def arvnAssaultResult(sp: Space): Space = {
      val num            = arvnFirepower(sp) min vulnerableInsurgents(sp.pieces).total
      val (newPieces, _) = selectRemoveEnemyInsurgentBasesLast(sp.pieces, num)
      sp.copy(pieces = newPieces)
    }

    val arvnAssaultWouldAddCoinControl = (sp: Space) =>
      !sp.coinControlled && arvnAssaultResult(sp).coinControlled

    def arvnAssaultWouldAddCoinControlToASpace: Boolean =
      game.nonLocSpaces exists arvnAssaultWouldAddCoinControl

    def arvnAssaultWouldUnblockCanTo_Hue: Boolean = {      
      allLocRoutesCanTho_HueBlocked && {
        val blockedLocs         = game.locSpaces filter (sp => sp.pieces.has(InsurgentForces))
        val assaultWouldUnblock = (sp: Space) => {
          // The allLocRoutesCanTho_HueBlocked function works on
          // on the current game state, so make safe copy
          val savedGameState = game        
          game               = game.updateSpace(arvnAssaultResult(sp))
          val unblocked      = !allLocRoutesCanTho_HueBlocked
          game               = savedGameState
          unblocked
        }

        blockedLocs exists assaultWouldUnblock
      }
    }

    def arvnAssaultWouldAddControlOrUnblockCanTo_Hue: Boolean = {
      arvnAssaultWouldAddCoinControlToASpace ||
      arvnAssaultWouldUnblockCanTo_Hue
    }

    def fiveArvnTroopPlusRangesInAnySpace: Boolean =
      game.spaces exists { sp => sp.pieces.totalOf(ARVNTroops::Rangers) >= 5 }

    def any2PopSpaceWithMoreArvnCubesThanUsCubes: Boolean =
      game.nonLocSpaces exists { sp =>
        sp.population == 2 &&
        sp.pieces.totalOf(ARVNCubes) > sp.pieces.totalOf(USTroops)
      }

        // All routes from Can Tho to Hue blocked
    def allLocRoutesCanTho_HueBlocked: Boolean =
       !getPatrolDestinations(CanTho).contains(Hue)
  
    def nvaBaseOrNvaControlAt2PlusPop: Boolean =
      game.nonLocSpaces exists { sp =>
        sp.population >= 2 &&
        (sp.pieces.has(NVABases) || sp.nvaControlled)
      }

    // ARVN Spaces Priorities: Shift Toward Passive Support
    def pickSpaceTowardPassiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botLog(s"\nARVN Select space (Shift Toward Passive Support): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nARVN Select space (Place Bases): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nARVN Select space (Place Cubes or Rangers): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nARVN Select space (Sweep or Transport Destinations): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Patrol Destinations
    def pickSpacePatrolDest(candidates: List[Space]): Space = {

      val priorities = List(
        SouthWithoutCoinControl,
        MostPopulation,
        LocWithEnemyPieces
      )

      botLog(s"\nARVN Select space (Patrol Destinations): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Govern
    def pickSpaceGovern(candidates: List[Space]): Space = {

      val priorities = List(
        MostPopulation,
        MostTotalSupport
      )

      botLog(s"\nARVN Select space (Govern): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Remove or Replace
    def pickSpaceRemoveReplace(candidates: List[Space], assault: Boolean = false, raid: Boolean = false): Space = {

      val priorities = List(
        filterIf(true,     VulnerableBase),
        filterIf(raid,     RaidLaosCambodia),
        filterIf(true,     SouthWithoutCoinControl),
        filterIf(true,     MostPopulation),
        filterIf(true,     LocWithEnemyPieces),
        filterIf(assault,  MostArvnFirepower),
        filterIf(true,     CityProvinceFewestEnemyPieces)
      ).flatten

      botLog(s"\nARVN Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botLog(separator())
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
      val nguyenCaoKy         = isRVNLeader(RVN_Leader_NguyenCaoKy)
      var trainSpaces  = Set.empty[String]
      val maxTrain     = params.maxSpaces getOrElse NO_LIMIT
      def trained      = trainSpaces.nonEmpty
      def canTrain     = trainSpaces.size < maxTrain &&
                         checkARVNActivation(trained, actNum, params.free)
      val canTrainRangers = (sp: Space) => !(trainSpaces(sp.name) || sp.nvaControlled)
      val canTrainCubes   = (sp: Space) => 
        !(trainSpaces(sp.name) || sp.nvaControlled) &&
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
            placePieces(sp.name, toPlace)
            trainSpaces += sp.name
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
            placePieces(sp.name, toPlace)
            trainSpaces += sp.name
            trainToPlaceRangers(candidates filterNot (_.name == sp.name))
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
          val mustPay = game.trackResources(ARVN) && !trainSpaces(sp.name)
          // If we have not already trained in this space and we are
          // tracking ARVN resources, make sure there is cash in the bank.
          if (!mustPay || game.arvnResources >= 3) {
            val toRemove = selectFriendlyRemoval(sp.pieces.only(ARVNCubes), 3)

            if (trainSpaces(sp.name))
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
            trainSpaces += sp.name  // In case we did not train there previously
            placedBase = true
          }
        }
        placedBase
      }

      def pacifyOneSpace(): Unit = {
        val baseRate   = if (isRVNLeader(RVN_Leader_NguyenCaoKy)) 4 else 3
        val costEach   = if (game.trackResources(ARVN)) baseRate else 0
        val candidates = trainSpaces.toList map game.getSpace filter { sp =>
          sp.coinControlled &&
          sp.pieces.has(ARVNTroops)   &&
          sp.pieces.has(ARVNPolice)   &&
          sp.support < PassiveSupport
        }

        if (candidates.nonEmpty && game.arvnResources >= costEach) {
          val sp = pickSpaceTowardPassiveSupport(candidates)
          val numCanPay = if (costEach == 0) NO_LIMIT else game.arvnResources / costEach
          val maxShift  = (PassiveSupport.value - sp.support.value) min 2
          val maxPacify = ((sp.terror + maxShift) min d3) min numCanPay
          val numTerror = maxPacify min sp.terror
          val numShift  = (maxPacify - numTerror) min maxShift

          log(s"\nPacifying in ${sp.name}")
          log(separator())
          decreaseResources(ARVN, (numTerror + numShift) * costEach)
          removeTerror(sp.name, numTerror)
          increaseSupport(sp.name, numShift)
        }
      }

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
        }
        Some(Train)
      }
      else {
        logNoOp(ARVN, Train)
        None
      }
    }
  }


  // ================================================================
  // NVA Specific code
  // ================================================================
  object NVA_Bot {

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

    // When the trail is at 4, NVA march considers all spaces that are
    // in or adjacent to Laos/Cambodia to be adjacent.
    def areNVAAdjacent(name1: String, name2: String) = if (game.trail == 4)
      areAdjacent(name1, name2) || (inOrAdjacentToLaosCambodia(name1) && inOrAdjacentToLaosCambodia(name2))
    else
      areAdjacent(name1, name2)

    def getNVAAdjacent(name: String): Set[String] = if (game.trail == 4 && inOrAdjacentToLaosCambodia(name))
      getAdjacent(name) ++ inOrAdjacentToLaosCambodia - name
    else
      getAdjacent(name)

    def sixTroopsWithCOINTroopsOrBase: Boolean = {
      game.spaces exists { sp =>
        sp.pieces.totalOf(NVATroops) >= 6 &&
        (sp.pieces.has(CoinTroops) || sp.pieces.has(CoinBases))
      }
    }

    def sixTroopsWithCOINPieces: Boolean = {
      game.spaces exists { sp =>
        sp.pieces.totalOf(NVATroops) >= 6 &&
        sp.pieces.has(CoinPieces)
      }
    }

    def eightTroopsOutsideSouth: Boolean = {
      game.spaces exists { sp => 
        sp.pieces.totalOf(NVATroops) >= 8 &&
        isOutsideSouth(sp.name)
      }
    }

    def numSupportWithUndergroundGuerrillas: Int = {
      game.spaces count { sp => 
        sp.support > Neutral &&
        sp.pieces.has(NVAGuerrillas_U)
      }
    }

    def pop2WithoutCoinControl: Boolean = {
      game.spaces exists (sp => sp.isLoC || !sp.coinControlled)
    }

    def atleastTwentyNVATroopsOnMap: Boolean = {
      game.totalOnMap(_.pieces.totalOf(NVATroops)) >= 20
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

      botLog(s"\nNVA Select space (Place Bases or Tunnels): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nNVA Select space (Place NVA Troops): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nNVA Select space (Place NVA Guerrilllas): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nNVA Select space (March Destinations): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place Terror
    def pickSpacePlaceTerror(candidates: List[Space]): Space = {

      val priorities = List(
        MostTotalSupport,
        VulnerableNVABase,
        CityProvinceMostNVAGuerrillas
      )

      botLog(s"\nNVA Select space (Place Terror): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nNVA Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botLog(separator())
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
      val maxRally     = params.maxSpaces getOrElse NO_LIMIT
      def rallied      = rallySpaces.nonEmpty
      def canRally     = rallySpaces.size < maxRally && checkActivation(NVA, rallied, actNum)
      val canRallyBase = (sp: Space) => {
        !rallySpaces(sp.name)       &&
        sp.support < PassiveSupport &&
        sp.totalBases < 2           &&
        sp.pieces.totalOf(NVAGuerrillas) > 2
      }
      val canRallyGuerrillas = (sp: Space) => { !rallySpaces(sp.name) && sp.support < PassiveSupport }

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
        else if (capabilityInPlay(AAA_Unshaded) && rallySpaces.size > 1)
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
    //  -------------------------------------------------------------
    def marchOp(params: Params, actNum: Int, withLoC: Boolean, withLaosCambodia: Boolean): Option[InsurgentOp] = {
      // Select a LoC march destination candidate
      // Once we have marched to one LoC successfully we are done.
      val nextLoCCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
        val qualifies = (sp: Space) =>
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
          !prohibited(sp.name)      &&
          sp.totalBases < 2         &&
          !sp.pieces.has(NVABases)  &&
          isInLaosCambodia(sp.name) &&
          (spaces(getNVAAdjacent(sp.name)) exists (_.pieces.has(NVAGuerrillas)))
        val candidates = game.locSpaces filter qualifies
        lazy val activationOK = checkActivation(NVA, needActivation, actNum)
        
        if (!lastWasSuccess && candidates.nonEmpty && activationOK)
          Some(bestCandidate(candidates, LaosCambodiaPriorities).name)
        else
          None
      }

      val nextGenericCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
        val candidates        = game.spaces filterNot (sp => prohibited.contains(sp.name))
        lazy val activationOK = checkActivation(NVA, needActivation, actNum)

        if (candidates.nonEmpty && activationOK)
          Some(NVA_Bot.pickSpaceMarchDest(candidates).name)
        else
          None
      }


      logOpChoice(NVA, March)
      // Never first need activation for LoCs
      if (withLoC)
        movePiecesToDestinations(NVA, March, NVAForces.toSet, false, params.maxSpaces)(nextLoCCandidate)

      // No need for first activation after LoCs
      // If we march to Laos/Cambodia then we will need first activate check
      // for the generic destinations
      val needActivate = if (withLaosCambodia)
        movePiecesToDestinations(NVA, March, NVAForces.toSet, false, params.maxSpaces)(nextLaosCambodiaCandidate)
      else
        false

      movePiecesToDestinations(NVA, March, NVAForces.toSet, needActivate, params.maxSpaces)(nextGenericCandidate)

      if (moveDestinations.nonEmpty)
        Some(March)
      else {
        logNoOp(NVA, March)
        None
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
    def attackOp(params: Params, actNum: Int, addAmbush: Boolean): (Option[InsurgentOp], Boolean) = {
      val threePlusGuerrillasNoVCBase = (sp: Space) => {
        sp.pieces.totalOf(VCGuerrillas) > 2 &&
        !sp.pieces.has(VCBases)
      }
        
      val pt76_unshaded   = capabilityInPlay(PT76_Unshaded)
      var attackSpaces   = Set.empty[String]
      var usedPT76Shaded = false

      // Return true if we can continue attacking (ie. did not fail and activation roll)
      def nextAttack(candidates: List[Space], useTroops: Boolean, needActivation: Boolean): Boolean = {
        if (candidates.nonEmpty) {
          if (checkActivation(NVA, needActivation, actNum)) {
            val sp         = pickSpaceRemoveReplace(candidates)

            val guerrillas = sp.pieces.only(NVAGuerrillas)
            val numTroops  = (if (pt76_unshaded) sp.pieces.totalOf(NVATroops) - 1 else sp.pieces.totalOf(NVATroops)) max 0
            val coinPieces = sp.pieces.only(CoinPieces)
            val toActivate = if (useTroops) Pieces() else guerrillas.only(NVAGuerrillas_U)
            val num        = if (useTroops)
              (numTroops / 2) min coinPieces.total
            else
              2 min coinPieces.total
            val pt76Num = if (useTroops && capabilityInPlay(PT76_Shaded) && !usedPT76Shaded)
            {
              val n = numTroops min coinPieces.total
              if (n != 0 && n > num) Some(n) else None
            }
            else
              None
            val die        = d6
            val success    = useTroops || die <= guerrillas.total
            val forceDisplay = if (useTroops) "Troops" else "Guerrillas"

            attackSpaces += sp.name
            log(s"\n$NVA Attacks in ${sp.name} using $forceDisplay")
            log(separator())
            if (!useTroops)
              log(s"Die roll: $die [${if (success) "Success!" else "Failure"}]")

            if (pt76_unshaded && sp.pieces.has(NVATroops))
              removeToAvailable(sp.name, Pieces(nvaTroops = 1), Some(s"$PT76_Unshaded triggers:"))

            if (success) {
              // Bases only removed if no other Coin forces (of either faction)
              val numToKill = pt76Num match {
                case Some(n) =>
                  log(s"NVA elects to use [$PT76_Shaded]")
                  n
                case None => num
              }

              val deadPieces = selectRemoveEnemyCoinBasesLast(coinPieces, num)
              val attritionPieces = if (useTroops) {
                val attritionNum  = deadPieces.only(USTroops::USBase::Nil).total min numTroops
                Pieces(nvaTroops = attritionNum)
              }
              else {
                val attritionNum  = deadPieces.only(USTroops::USBase::Nil).total min guerrillas.total
                Pieces(nvaGuerrillas_A = attritionNum) // All NVA guerrillas will have been activated
              }

              revealPieces(sp.name, toActivate)
              loggingControlChanges {
                removePieces(sp.name, deadPieces)
                removeToAvailable(sp.name, attritionPieces, Some("Attrition:"))
              }
            }
            nextAttack(candidates filterNot (_.name == sp.name), useTroops, needActivation = true)
            true  // Did not fail activation roll
          }
          else
            false  // Failed activation roll
        }
        else
          true  // Did not fail activation roll, so continue attacking

      }

      logOpChoice(NVA, Attack)
      var keepAttacking   = true
      val troopCandidates = game.spaces filter { sp =>
        sp.pieces.totalOf(NVATroops) > 1 && sp.pieces.has(CoinPieces)
      }
      
      // Attack where 2+ troops
      keepAttacking = nextAttack(troopCandidates, useTroops = true, false)
      
      val ambushed = if (addAmbush && keepAttacking && params.specialActivity) {
        // Ambush in 1 or 2 spaces (if special activity allowed)
        lazy val ambushCandidates = spaceNames(game.spaces filter { sp =>
          !attackSpaces(sp.name) &&
          canAmbushFrom(NVA)(sp)
        })
        val (ambushSpaces: Set[String], canContinue) = {
          if (ambushCandidates.nonEmpty)
            ambushActivity(NVA, ambushCandidates, Attack, actNum, checkFirst = attackSpaces.nonEmpty)
          else
            (Set.empty, true)
        }
  
        attackSpaces ++= ambushSpaces
        keepAttacking  = canContinue
        ambushSpaces.nonEmpty
      }
      else
        false

      lazy val guerrillaCandidates = game.spaces filter { sp =>
        !attackSpaces(sp.name) &&
        sp.pieces.totalOf(NVAGuerrillas) > 3 &&
        sp.pieces.has(CoinPieces)
      }

      // spaces the 4+ NVA Guerrillas
      if (keepAttacking)
        nextAttack(guerrillaCandidates, useTroops = false, needActivation = attackSpaces.nonEmpty)
      
      if (attackSpaces.nonEmpty)
        (Some(Attack), ambushed)
      else {
        logNoOp(NVA, Attack)
        (None, false)
      }
    }

    //  -------------------------------------------------------------
    //  Implement the NVA Terror Instructions from the NVA Trung cards.
    //  Select spaces using Place Terror
    //  Note:
    //  Bruce Mansfield clarified that NVA Bot should not make
    //  an activation roll after conducting Terror on a LoC.
    //  -------------------------------------------------------------
    def terrorOp(params: Params, actNum: Int): Option[InsurgentOp] = {
      val isCandidate = (sp: Space) => {
        (sp.pieces.has(NVAGuerrillas_U) || sp.pieces.has(NVATroops)) &&
        ((sp.isLoC && sp.terror == 0 && sp.printedEconValue > 0) ||
         (!sp.isLoC && sp.population > 0 && (sp.terror == 0 || sp.support > Neutral)))
      }

      def nextTerror(candidates: List[Space], needActivation: Boolean): Unit = {
        if (candidates.nonEmpty && checkActivation(VC, needActivation, actNum)) {
          val sp = pickSpacePlaceTerror(candidates)

            log(s"\n$NVA selects ${sp.name} for Terror")
            if (sp.pieces.has(NVAGuerrillas_U))
              revealPieces(sp.name, Pieces(nvaGuerrillas_U = 1))

            if (sp.terror == 0)
              addTerror(sp.name, 1) // Terror/Sabotage marker

            if (!sp.isLoC && sp.support > Neutral)
              decreaseSupport(sp.name, 1)

          nextTerror(candidates filterNot (_.name == sp.name), needActivation = !sp.isLoC)
        }
      }

      logOpChoice(NVA, Terror)
      val candidates = game.spaces filter isCandidate
      if (candidates.nonEmpty) {
        nextTerror(candidates, false)
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
    def infiltrateActivity(needDiceRoll: Boolean, replaceVCBase: Boolean): Boolean = {
      var infiltrateSpaces = Set.empty[String]
      val limited          = momentumInPlay(Mo_TyphoonKate) || momentumInPlay(Mo_559TransportGrp)
      val maxInfiltrate    = if (limited) 1 else 2

      val notes = List(
        noteIf(momentumInPlay(Mo_TyphoonKate),s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
        noteIf(momentumInPlay(Mo_559TransportGrp), s"Infiltrate is max 1 space [Momentum: $Mo_559TransportGrp]")
      ).flatten

      val canReplaceBase = (sp: Space) =>
        !infiltrateSpaces(sp.name) &&
        sp.pieces.totalOf(NVAPieces) > sp.pieces.totalOf(VCPieces) &&
        sp.pieces.has(VCBases)
      val canPlaceTroops = (sp: Space) =>
        !infiltrateSpaces(sp.name) &&
        sp.pieces.has(NVABases)
      val diceSuccess = rollDice(3) <= game.availablePieces.totalOf(NVATroops)
      
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
          if (infiltrateSpaces.isEmpty)
            logSAChoice(NVA, Infiltrate, notes)

          log(s"\nNVA Infiltrates in ${sp.name}")
            log(separator())

          loggingControlChanges {
            removeToAvailable(sp.name, toRemove)
            placePieces(sp.name, toPlace)
          }
          infiltrateSpaces += sp.name
          placeTroops()
        }
      }

      if (!needDiceRoll || diceSuccess) {
        val baseCandidates = game.nonLocSpaces filter canReplaceBase

        // First try to replace a VC base if requested
        if (replaceVCBase && game.availablePieces.has(NVABase) && baseCandidates.nonEmpty) {
          val sp      = pickSpaceRemoveReplace(baseCandidates)
          val vcPiece = selectEnemyRemovePlaceActivate(sp.pieces.only(VCBases), 1)
          val nvaType = getInsurgentCounterPart(vcPiece.explode().head)

          logSAChoice(NVA, Infiltrate, notes)
          log(s"\nNVA Infiltrates in ${sp.name}")
            log(separator())

          if (sp.support < Neutral)
            increaseSupport(sp.name, 1)

          loggingControlChanges {
            removeToAvailable(sp.name, vcPiece)
            placePieces(sp.name, Pieces(nvaBases = 1))
            if (nvaType == NVATunnel)
                addTunnelMarker(sp.name, NVABase)
            infiltrateSpaces += sp.name
          }
        }

        // Next attempt to place troops at NVA bases
        placeTroops()
      }

      infiltrateSpaces.nonEmpty
    }

    //  -------------------------------------------------------------
    //  NVA Bombard Activity
    //
    // Mo_TyphoonKate         - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
    // LongRangeGuns_Unshaded - NVA Bombard is max 1 space
    // LongRangeGuns_Shaded   - NVA Bombard is max 3 spaces
    //  -------------------------------------------------------------
    def bombardActivity(): Boolean = {
      if (momentumInPlay(Mo_TyphoonKate))
        false   // Typooon Kate prohibits Bombard
      else {
        var bombardSpaces = Set.empty[String]
        val maxBombard = if (capabilityInPlay(LongRangeGuns_Unshaded)) 1
                    else if (capabilityInPlay(LongRangeGuns_Shaded))   3
                    else                                               2
        val isCandidate = (sp: Space) => {
          val enemyCondition = sp.pieces.totalOf(CoinTroops) > 2 || 
                               (sp.pieces.has(CoinTroops) && sp.pieces.has(CoinBases))
          lazy val hasAdjacentTroops = getAdjacent(sp.name) exists { name =>
            game.getSpace(name).pieces.totalOf(NVATroops) > 2
          }
          
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
            val toRemove = selectEnemyRemovePlaceActivate(troops, troops.total min 1)
    
            if (numBombarded == 0)
              logSAChoice(NVA, Bombard, notes)
    
            log(s"\nNVA Bombards ${sp.name}")
            log(separator())
            removePieces(sp.name, toRemove)
            nextBombard(numBombarded + 1)
          }
        }

        nextBombard(0)

        // Return true if we bombarded at least one space
        bombardSpaces.nonEmpty
      }
    }

  }

  // ================================================================
  // VC Specific code
  // ================================================================
  object VC_Bot {
    
    val canSubvertSpace = (sp: Space) => sp.pieces.has(VCGuerrillas_U) && sp.pieces.has(ARVNCubes)
    val canTaxSpace = (sp: Space) => {
      // Bot will only tax spaces with a VC Base if at least 2 underground guerrillas
      val needed = if (sp.pieces.has(VCBases)) 2 else 1 
      val hasG = sp.pieces.totalOf(VCGuerrillas_U) >= needed
      hasG && ((sp.isLoC && sp.printedEconValue > 0) || (!sp.coinControlled && sp.population > 0))
    }

    def undergroundAtNoActiveOpposition = game.spaces exists { sp =>
      (sp.isLoC || sp.support != ActiveOpposition) && sp.pieces.has(VCGuerrillas_U)
    }

    def pop2SpaceWithoutGuerrillas = game.spaces exists { sp =>
        !sp.isLoC && sp.population >= 2 && sp.pieces.totalOf(VCGuerrillas) == 0
    }

    def threePlusGuerrillasWithUSTroopsNoVCBase = {
      val test = (sp: Space) => {
        sp.pieces.totalOf(VCGuerrillas) > 2 &&
        sp.pieces.has(USTroops)             &&
        !sp.pieces.has(VCBases)
       }
       game.spaces exists test
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
    def pickSpaceTowardActiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             CityProvinceNoActiveOpposition),
        filterIf(true,             MostPopulation),
        filterIf(game.isHuman(US), MostTotalSupport)
      ).flatten

      botLog(s"\nVC Select space (Shift Toward Active Opposition): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nVC Select space (Place Terror): [${andList(candidates.sorted)}]")
      botLog(separator())
      bestCandidate(candidates, priorities)
    }
    
    // VC Spaces Priorities: Place Bases or Tunnels
    def pickSpacePlaceBases(candidates: List[Space]): Space = {
      val priorities = List(
        MostPopulation,
        CityProvinceMostVCGuerrillas,
        CityProvinceFewestNonVCPieces
      )

      botLog(s"\nVC Select space (Place Bases or Tunnels): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nVC Select space (Place Guerrilllas): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nVC Select space (March Destinations): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nVC Select space (Tax): [${andList(candidates.sorted)}]")
      botLog(separator())
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

      botLog(s"\nVC Select space (Remove or Replace): [${andList(candidates.sorted)}]")
      botLog(separator())
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
      def canRally     = rallySpaces.size < maxRally && checkActivation(VC, rallied, actNum)
      val canRallyBase = (sp: Space) => {
        !rallySpaces(sp.name)       &&
        sp.support < PassiveSupport &&
        sp.totalBases < 2           &&
        sp.pieces.totalOf(VCGuerrillas) > 2
      }
      val canRallyGuerrillas = (sp: Space) => { !rallySpaces(sp.name) && sp.support < PassiveSupport }
      val canFlipGuerrillas = (sp: Space) => {
        !rallySpaces(sp.name)         &&
        sp.support < PassiveSupport   &&
        sp.pieces.has(VCBases)        &&
        sp.pieces.has(VCGuerrillas_A) &&
        !sp.pieces.has(VCGuerrillas_U)
      }

      def tryCadresAgitate(sp: Space): Unit = {
        if (!agitated && cadres && sp.support > ActiveOpposition && game.agitateTotal > 0) {
          log(s"\nVC Agitates in ${sp.name} [$Cadres_Shaded]")
          log(separator())
          val numTerror = sp.terror min game.agitateTotal
          val numShift  = (sp.support.value - ActiveOpposition.value) min (game.agitateTotal - numTerror) min 2
          removeTerror(sp.name, numTerror)
          decreaseSupport(sp.name, numShift)
          decreaseAgitateTotal(numTerror + numShift)
        }
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
            rallyToFlipGuerrillas(candidates filterNot (_.name == sp.name))
          }
          else
            false // Failed activation or max spaces rallied
        }
        else
          true  // No failed activation, ran out of candidates
      }

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
    //  -------------------------------------------------------------
    def marchOp(params: Params, actNum: Int): Option[InsurgentOp] = {

      val nextLoCCandidate = (lastWasSuccess: Boolean, needActivation: Boolean, prohibited: Set[String]) => {
        val LocPriorities = List(
          new HighestScore[Space](
            "Most Adjacent Underground VC Guerrillas",
            sp => numAdjacentPieces(sp, Set(VCGuerrillas_U))
          )
        )
        val qualifies = (sp: Space) => !prohibited(sp.name) &&
                                          numAdjacentPieces(sp, Set(VCGuerrillas_U)) > 0
        lazy val locCandidates = game.locSpaces filter qualifies

        // No need to check activation since these are LoCs
        // 
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
        lazy val candidates    = game.spaces filterNot (sp => prohibited.contains(sp.name))

        if (candidates.nonEmpty && checkActivation(VC, needActivation, actNum)) 
          Some(VC_Bot.pickSpaceMarchDest(candidates).name)
        else
          None
      }

      logOpChoice(VC, March)
      movePiecesToDestinations(VC, March, VCGuerrillas.toSet, false, params.maxSpaces)(nextLoCCandidate)
      // First activation check is always false since previos 0-2 spaces were LoCs
      movePiecesToDestinations(VC, March, VCGuerrillas.toSet, false, params.maxSpaces)(nextGenericCandidate)

      if (moveDestinations.nonEmpty)
        Some(March)
      else {
        logNoOp(VC, March)
        None
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
        
      def nextAttack(candidates: List[Space], needActivation: Boolean): Unit = {
        if (candidates.nonEmpty && checkActivation(VC, needActivation, actNum)) {
          val sp         = pickSpaceRemoveReplace(candidates)
          val guerrillas = sp.pieces.only(VCGuerrillas)
          val coinPieces = sp.pieces.only(CoinPieces)
          val toActivate = guerrillas.only(VCGuerrillas_U)
          val num        = 2 min coinPieces.total
          val die        = d6
          val success    = die <= guerrillas.total

          log(s"\n$VC Attacks in ${sp.name}")
          log(separator())
          log(s"Die roll: $die [${if (success) "Success!" else "Failure"}]")

          if (success) {
            val deadPieces = selectRemoveEnemyCoinBasesLast(coinPieces, num)
            val attrition  = deadPieces.only(USTroops::USBase::Nil).total min guerrillas.total
            val attritionPieces = Pieces(vcGuerrillas_A = attrition) // All VC guerrillas are now active

            revealPieces(sp.name, toActivate)
            loggingControlChanges {
              removePieces(sp.name, deadPieces)
              removeToAvailable(sp.name, attritionPieces, Some("Attrition:"))
            }
          }
          nextAttack(candidates filterNot (_.name == sp.name), needActivation = true)
        }
      }

      logOpChoice(VC, Attack)
      val ambushCandidates = spaceNames(game.spaces filter canAmbushFrom(VC))
      val (ambushSpaces: Set[String], canContinue) = if (params.specialActivity && ambushCandidates.nonEmpty)
        ambushActivity(VC, ambushCandidates, Attack, actNum, checkFirst = false)
      else
        (Set.empty, true)

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

      val isCandidate = (sp: Space) => {
        val needed = if (sp.pieces.has(VCBases)) 2 else 1
        sp.pieces.totalOf(VCGuerrillas_U) >= needed &&
        ((sp.isLoC && sp.terror == 0 && sp.printedEconValue > 0) ||
         (!sp.isLoC && sp.population > 0 && (sp.terror == 0 || sp.support != ActiveOpposition)))
      }

      def nextTerror(candidates: List[Space], needActivation: Boolean): Unit = {
        if (candidates.nonEmpty && checkActivation(VC, needActivation, actNum)) {
          val sp = pickSpacePlaceTerror(candidates)

            log(s"\n$VC selects ${sp.name} for Terror")
            revealPieces(sp.name, Pieces(vcGuerrillas_U = 1))
            if (capabilityInPlay(Cadres_Unshaded)) {
              // Get fresh copy of space to include the guerrilla that was just flipped
              val toRemove = selectFriendlyRemoval(game.getSpace(sp.name).pieces.only(VCGuerrillas), 2)
              removeToAvailable(sp.name, toRemove, Some(s"$Cadres_Unshaded triggers"))
            }

            if (sp.terror == 0)
              addTerror(sp.name, 1) // Terror/Sabotage marker

            if (!sp.isLoC && sp.support != ActiveOpposition)
              decreaseSupport(sp.name, 1)

          nextTerror(candidates filterNot (_.name == sp.name), !sp.isLoC)
        }
      }

      logOpChoice(VC, Terror)
      val candidates = game.spaces filter isCandidate
      if (candidates.nonEmpty) {
        nextTerror(candidates, false)
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
    def taxActivity(): Boolean = {
      val maxTax = if (momentumInPlay(Mo_TyphoonKate)) 1 else d3
      def nextTax(candidates: List[Space], numTaxed: Int): Boolean = {
        if (candidates.nonEmpty && numTaxed < maxTax) {
          val sp    = pickSpaceTax(candidates)
          val num  = if (sp.isLoC) sp.printedEconValue else sp.population

          if (numTaxed == 0)
            logSAChoice(VC, Tax)

          log(s"\nVC Taxes in ${sp.name}")
          log(separator())
          revealPieces(sp.name, Pieces(vcGuerrillas_U = 1))
          if (!sp.isLoC && sp.support != ActiveSupport)
            increaseSupport(sp.name, 1)
          increaseAgitateTotal(num)
          nextTax(candidates filterNot (_.name == sp.name), numTaxed + 1)
        }
        else
          numTaxed > 0
      }

      nextTax(game.spaces filter canTaxSpace, 0)
    }

    //  -------------------------------------------------------------
    //  VC Subert Activity
    //  -------------------------------------------------------------
    def subvertActivity(): Boolean = {
      val maxSubvert = if (momentumInPlay(Mo_TyphoonKate)) 1 else 2
      var totalRemoved = 0

      def nextSubvert(candidates: List[Space], numSubverted: Int): Unit = {
        if (candidates.nonEmpty && numSubverted < maxSubvert) {
          val sp       = pickSpaceRemoveReplace(candidates)
          val cubes    = sp.pieces.only(ARVNCubes)
          val toRemove = selectEnemyRemovePlaceActivate(cubes, cubes.total min 2)
  
          if (numSubverted == 0)
            logSAChoice(VC, Subvert)
  
          log(s"\nVC Subverts in ${sp.name}")
          log(separator())
          removeToAvailable(sp.name, toRemove)
          totalRemoved += toRemove.total
          if (toRemove.total == 1 && game.availablePieces.has(VCGuerrillas_U))
            placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
          nextSubvert(candidates filterNot (_.name == sp.name), numSubverted + 1)
        }
      }

      nextSubvert(game.spaces filter VC_Bot.canSubvertSpace, 0)
      if (totalRemoved > 0) {
        if (totalRemoved / 2 > 0) {
          log()
          decreasePatronage(totalRemoved / 2)
        }
        true
      }
      else
        false
    }
  }


  def isFirstOnNextCard(faction: Faction) = {
    val nextCard = deck(game.onDeckCard)
    !nextCard.isCoup && faction == nextCard.factionOrder.head
  }

  val firstEligibileTable = List(
    // Event is Critical and Effective?
    ActionEntry(Event, "Current Event is Critical and Effective?",
    (faction) => {
      val card = deck(game.currentCard)
      card.eventPriority(faction) == Critical && card.eventEffective(faction)
    }),

    // Event is critical for Next eligible faction?
    ActionEntry(OpOnly, "Next Eligible could choose Critical Event?",
    (faction) => {
      val card = deck(game.currentCard)
      game.followingFaction map (next => card.eventPriority(next) == Critical) getOrElse false
    }),

    // Is first to act on next csrd and the event is Critical?
    ActionEntry(Pass, "First choice on upcoming Critical Event?",
      (faction) => {
      val nextCard = deck(game.onDeckCard)

      isFirstOnNextCard(faction) && nextCard.eventPriority(faction) == Critical
    }),

    // Otherwise...
    ActionEntry(OpPlusSpecial, "Otherwise...", (_) => true)
  )

  val secondEligibileTable = List(
    // Active fAction will be 1st Eligible on upcoming Critical Event and
    // cannot execute current Critical event?
    ActionEntry(Pass, "Will be 1st Eligible on upcoming Critical Event and cannot execute current Critical Event?",
      (faction) => {
      val card     = deck(game.currentCard)
      val nextCard = deck(game.onDeckCard)

      isFirstOnNextCard(faction) &&
      nextCard.eventPriority(faction) == Critical &&
      !(game.sequence.canDo(Event) &&
        card.eventPriority(faction) == Critical &&
        card.eventEffective(faction))
    }),

    // Event is Critical or Performed and it is Effective
    ActionEntry(Event, "1st Eligibile chose Op+SA and Event is Performed/Critical and Effective?",
      (faction) => {
      val card     = deck(game.currentCard)
      val priority = card.eventPriority(faction)

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

    botLog {
      val which = if (isFirstEligible) "1st" else "2nd"
      s"\n$faction Choosing Action using NP Eligiblity Table ($which eligible)"
    }
    botLog(separator())

    val table = if (isFirstEligible)
      firstEligibileTable
    else
      secondEligibileTable

    table find { entry =>
      val result = entry.test(faction)
      botLog(msgResult(result, entry.desc))
      result
    }
  }

  def initTurnVariables(): Unit = {
    moveDestinations = Set.empty
    movedPieces.reset()
  }

  //  A bot is the next eligible faction
  //  Decide what type of action the Bot will take.
  //  We use the NP Elgibility Table.
  //  If an Op/Op+Sa/Lim-Op is selected and upon trying the operation we
  //  determine that it could not be carried out, then we continue down the
  //  table.
  def act(): Unit = {
    val faction = game.actingFaction.get
    val card = deck(game.currentCard)

    initTurnVariables()

    if (game.executingPivotalEvent) {
      //  The Pivotal events are single events which are always in the executeUnshaded() function.
      card.executeEvent(faction)
    }
    else {
      chooseAction(faction) match {
        case None =>  // Could not carry out an action so Pass
          factionPasses(faction)

        case Some(ActionEntry(Pass, _, _)) => // Table resolve to Pass
          factionPasses(faction)

        case Some(ActionEntry(Event, _, _)) =>
          card.executeEvent(faction)

        case entry @ Some(ActionEntry(action, _, _)) =>
          // LimitedOp, OpOnly, or OpPlusSpecial
          val first = game.sequence.numActors == 0
          val sa    = action == OpPlusSpecial
          val maxsp = if (action == LimitedOp) Some(1) else None
          val params = Params(specialActivity = sa, maxSpaces = maxsp)
          
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
          else {
            game = game.copy(sequence = game.sequence.addActor(faction, actualAction))
            log()
            log(s"Move the $faction cylinder to the $actualAction box")
          }
      }
    }
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
    val firstCard = drawTrungCard(faction)

    def logTrungDraw(card: TrungCard): Unit = {
      log(s"\nDrawing Trung Card for $faction")
      log(separator())
      log(card.toString)
    }

    def executeCard(trungCard: TrungCard): ExecuteResult = {
      trungCard.executeFront(params) match {
        case  TrungComplete(true)  => ER_OpPlusSpecial
        case  TrungComplete(false) => ER_OpOnly

        case  TrungDraw =>
          val nextCard = drawTrungCard(faction)
          if (nextCard == firstCard)
            ER_NoOp
          else {
            logTrungDraw(nextCard)
            executeCard(nextCard)
          }

        case  TrungNoOp =>
          ER_NoOp
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
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_B extends TrungCard(US, "B", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_C extends TrungCard(US, "C", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_D extends TrungCard(US, "D", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_E extends TrungCard(US, "E", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_US_F extends TrungCard(US, "F", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
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
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_H extends TrungCard(ARVN, "H", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_J extends TrungCard(ARVN, "J", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_K extends TrungCard(ARVN, "K", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_L extends TrungCard(ARVN, "L", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }
  }

  // ---------------------------------------------------------------
  object Trung_ARVN_M extends TrungCard(ARVN, "M", actNum = 3) {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      log(s"\n$this not yet implemented!")
      TrungNoOp
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
      if (rollDice(3) > game.availablePieces.totalOf(NVATroops))
        TrungDraw
      else if (!NVA_Bot.sixTroopsWithCOINTroopsOrBase)
        flipCard(params)
      else {
        val (operation, ambushed) = NVA_Bot.attackOp(params, actNum, addAmbush = params.specialActivity)

        operation match {
          case Some(_) if ambushed => TrungComplete(true)
          case Some(_) => TrungComplete(params.specialActivity && NVA_Bot.bombardActivity())
          case _       => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      if (NVA_Bot.eightTroopsOutsideSouth) {
        val infiltrated = params.specialActivity && NVA_Bot.infiltrateActivity(needDiceRoll = false, replaceVCBase = false)

        NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = false) match {
          case Some(_) => TrungComplete(infiltrated)
          case None    => TrungNoOp
        }
      }
      else {
        NVA_Bot.rallyOp(params, actNum) match {
          case Some(_) => 
            val infiltrated = params.specialActivity &&
                              NVA_Bot.infiltrateActivity(needDiceRoll = false, replaceVCBase = false)
            TrungComplete(infiltrated)

          case None =>
            TrungNoOp
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
      if (game.totalCoinControl < 42)
        TrungDraw
      else if (!NVA_Bot.atleastTwentyNVATroopsOnMap)
        flipCard(params)
      else {
        val bombarded = params.specialActivity && NVA_Bot.bombardActivity()

        val operation = if (NVA_Bot.sixTroopsWithCOINTroopsOrBase)
          NVA_Bot.attackOp(params, actNum, addAmbush = false)
        else
          NVA_Bot.marchOp(params, marchActNum, withLoC = false, withLaosCambodia = false)

        operation match {
          case Some(_) => TrungComplete(bombarded)
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      if (rollDice(2) <= game.availablePieces.totalOf(NVAGuerrillas)) {
        NVA_Bot.rallyOp(params, actNum) match {
          case Some(_) =>
            val infiltrated = params.specialActivity && 
                              NVA_Bot.infiltrateActivity(needDiceRoll = false, replaceVCBase = true)
            TrungComplete(infiltrated)

          case None =>
            TrungNoOp
        }
      }
      else {
        val infiltrated = params.specialActivity &&
                          NVA_Bot.infiltrateActivity(needDiceRoll = false, replaceVCBase = false)
        NVA_Bot.marchOp(params, marchActNum, withLoC = false, withLaosCambodia = false) match {
          case Some(_) => TrungComplete(infiltrated)
          case None    => TrungNoOp
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
      val guerrilasCheck = rollDice(2) <= game.availablePieces.totalOf(NVAGuerrillas)
      val trailCheck     = d3 >= game.trail

      if (!guerrilasCheck && !trailCheck)
        TrungDraw
      else if (!NVA_Bot.sixTroopsWithCOINTroopsOrBase)
        flipCard(params)
      else {
        val (operation, ambushed) = NVA_Bot.attackOp(params, actNum, addAmbush = params.specialActivity)
        val bombarded = operation.nonEmpty && !ambushed && NVA_Bot.bombardActivity()
        operation match {
          case Some(_) if ambushed => TrungComplete(true)
          case Some(_) => TrungComplete(params.specialActivity && NVA_Bot.bombardActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      NVA_Bot.rallyOp(params, actNum) match {
        case Some(_) if params.specialActivity =>
          val didSpecial = NVA_Bot.infiltrateActivity(needDiceRoll = true, replaceVCBase = true) ||
                           NVA_Bot.bombardActivity()
          TrungComplete(didSpecial)

        case Some(_) =>
          TrungComplete(false)

        case None =>
          TrungNoOp
      }
    }
  }

  // ---------------------------------------------------------------
  object Trung_NVA_R extends TrungCard(NVA, "R", actNum = 2) with NVATrung {

    // ------------------------------------------------------------
    // Front Operation
    // ------------------------------------------------------------
    def executeFront(params: Params): TrungResult = {
      if (game.usPoints < 42)  // Support + available US Troops and Bases
        TrungDraw
      else if (rollDice(3) > game.availablePieces.totalOf(NVATroops))
        flipCard(params)
      else {
        val infiltrated = params.specialActivity &&
                          NVA_Bot.infiltrateActivity(needDiceRoll = false, replaceVCBase = true)

        NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = true) match {
          case Some(_) => TrungComplete(infiltrated)
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      if (game.nonLocSpaces exists (sp => sp.support > Neutral && sp.pieces.has(NVAGuerrillas_U))) {
        NVA_Bot.terrorOp(params, actNum) match {
          case Some(_) =>
            val bombarded = params.specialActivity && NVA_Bot.bombardActivity()
            TrungComplete(bombarded)

          case None =>
            TrungNoOp
        }
      }
      else {
        def doSpecialActivity(): Boolean = {
          val ambushCandidates = marchAmbushCandidates(NVA)
          val canAmbush        = ambushCandidates.nonEmpty && !momentumInPlay(Mo_Claymores)
        
          if (canAmbush)
            ambushActivity(NVA, ambushCandidates, March, actNum, false)
          
          canAmbush || NVA_Bot.bombardActivity()
        }

        NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = true) match {
          case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity())
          case None    => TrungNoOp
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
      val guerrillaCheck = rollDice(2) <= game.availablePieces.totalOf(NVAGuerrillas)
      val trailCheck     = d3 >= game.trail

      if (!NVA_Bot.pop2WithoutCoinControl)
        TrungDraw
      else if (!guerrillaCheck && !trailCheck)
        flipCard(params)
      else {
        NVA_Bot.rallyOp(params, actNum) match {
          case Some(_) if (params.specialActivity) =>
            val didSpecial = NVA_Bot.infiltrateActivity(needDiceRoll = true, replaceVCBase = true) ||
                             NVA_Bot.bombardActivity()
            TrungComplete(didSpecial)
            
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
      def doSpecialActivity(op: InsurgentOp): Boolean = {
        val ambushCandidates = marchAmbushCandidates(NVA)
        val canAmbush        = op == March && ambushCandidates.nonEmpty && !momentumInPlay(Mo_Claymores)
        
        if (canAmbush)
          ambushActivity(VC, ambushCandidates, op, actNum, false)

        canAmbush || NVA_Bot.bombardActivity()
      }


      val (operation, ambushed) = if (NVA_Bot.sixTroopsWithCOINPieces)
        NVA_Bot.attackOp(params, actNum, addAmbush = params.specialActivity)
      else
        (NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = true), false)

      operation match {
        case Some(Attack) if ambushed => TrungComplete(true)
        case Some(op)                 => TrungComplete(params.specialActivity && doSpecialActivity(op))
        case None                     => TrungNoOp
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
      else {
        val (operation, ambushed) = NVA_Bot.attackOp(params, actNum, addAmbush = params.specialActivity)

        operation match {
          case Some(_) if ambushed => TrungComplete(true)
          case Some(_)             => TrungComplete(params.specialActivity && NVA_Bot.bombardActivity())
          case None                => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      val underGroundWithSupport = (sp: Space) =>
        sp.pieces.has(NVAGuerrillas_U) && sp.support > Neutral
      val undergroundIn2PlusSpacesWithSupport: Boolean =
        (game.nonLocSpaces count underGroundWithSupport) > 1
      
      if (undergroundIn2PlusSpacesWithSupport) {
        NVA_Bot.terrorOp(params, actNum) match {
          case Some(_) => TrungComplete(params.specialActivity && NVA_Bot.bombardActivity())
          case None    => TrungNoOp
        }
      }
      else {
        val didSpecial =
          params.specialActivity &&
          (NVA_Bot.infiltrateActivity(needDiceRoll = true, replaceVCBase = true) ||
           NVA_Bot.bombardActivity())

        NVA_Bot.marchOp(params, marchActNum, withLoC = true, withLaosCambodia = false) match {
          case Some(_) => TrungComplete(didSpecial)
          case None    => TrungNoOp
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
        val canSubvert = game.patronage >= 17 && (game.spaces exists VC_Bot.canSubvertSpace)

        (canSubvert && VC_Bot.subvertActivity()) || VC_Bot.taxActivity()
      }
      
      val threePlusGuerrillas = game.spaces exists (_.pieces.totalOf(VCGuerrillas) > 2)

      if (threePlusGuerrillas) {

        if (VC_Bot.undergroundAtNoActiveOpposition) {

          VC_Bot.terrorOp(params, actNum) match {
            case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity)
            case None    => TrungNoOp
          }
        }
        else
          flipCard(params)
      }
      else
        TrungDraw
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {

      def doSpecialActivity(op: InsurgentOp): Boolean = {
        val ambushCandidates = marchAmbushCandidates(VC)
        val canAmbush        = op == March && ambushCandidates.nonEmpty && !momentumInPlay(Mo_Claymores)
        val agitateRoll      = rollDice(2)
        val canTax           = (agitateRoll > game.agitateTotal) && (game.spaces exists VC_Bot.canTaxSpace)
        
        if (canAmbush)
          ambushActivity(VC, ambushCandidates, op, actNum, false)

        canAmbush || (canTax && VC_Bot.taxActivity()) || VC_Bot.subvertActivity()
      }

      // ------------------------------------------------------------
      // Back Operation
      // ------------------------------------------------------------
      val dice = rollDice(3)
      botLog(s"Dice roll: $dice, available VC pieces: ${game.availablePieces.totalOf(VCPieces)}")

      val operation = if (dice <= game.availablePieces.totalOf(VCPieces))
        VC_Bot.rallyOp(params, actNum)
      else
        VC_Bot.marchOp(params, actNum)

      operation match {
        case Some(op) => TrungComplete(params.specialActivity && doSpecialActivity(op))
        case None     => TrungNoOp
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
        val agitateRoll      = rollDice(2)
        val canTax           = (agitateRoll > game.agitateTotal) && (game.spaces exists VC_Bot.canTaxSpace)
        val canSubvert       = game.spaces exists VC_Bot.canSubvertSpace
        
        (canTax && VC_Bot.taxActivity()) || VC_Bot.subvertActivity()

      }

      if (VC_Bot.undergroundAtNoActiveOpposition) {
        if (rollDice(3) <= game.availablePieces.totalOf(VCPieces)) {

          VC_Bot.rallyOp(params, actNum) match {
            case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity)
            case None    => TrungNoOp
          }
        }
        else
          flipCard(params)
      }
      else
        TrungDraw
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      val (operation, ambushed) = if (VC_Bot.threePlusGuerrillasWithUSTroopsNoVCBase)
        VC_Bot.attackOp(params, actNum)
      else
        (VC_Bot.terrorOp(params, actNum), false)

      operation match {
        case Some(Attack) => TrungComplete(ambushed)
        case Some(Terror) => TrungComplete(params.specialActivity && VC_Bot.subvertActivity())
        case _            => TrungNoOp
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
        (game.patronage >= 17 && VC_Bot.subvertActivity()) || VC_Bot.taxActivity()
      }

      val undergroundWithUSTroops = game.spaces exists { sp=>
        sp.pieces.has(VCGuerrillas_U) && sp.pieces.has(USTroops)
      }
      
      if (!undergroundWithUSTroops)
        TrungDraw
      else if (rollDice(3) > game.availablePieces.totalOf(VCPieces))
        flipCard(params)
      else {
        VC_Bot.rallyOp(params, actNum) match {
          case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      val (operation, ambushed) = if (VC_Bot.undergroundAtNoActiveOpposition)
        (VC_Bot.terrorOp(params, actNum), false)
      else
        VC_Bot.attackOp(params, actNum)

      operation match {
        case Some(Attack) => TrungComplete(ambushed)
        case Some(Terror) => TrungComplete(params.specialActivity && VC_Bot.subvertActivity())
        case _            => TrungNoOp
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
        (rollDice(2) > game.agitateTotal && VC_Bot.taxActivity()) || VC_Bot.subvertActivity()
      }

      // ------------------------------------------------------------
      // Front Operation
      // ------------------------------------------------------------
      if (!VC_Bot.pop2SpaceWithoutGuerrillas)
        TrungDraw
      else if (rollDice(3) > game.availablePieces.totalOf(VCPieces))
        flipCard(params)
      else {
        VC_Bot.rallyOp(params, actNum) match {
          case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity())
          case None    => TrungNoOp
        }
      }
    }

    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        val ambushCandidates = marchAmbushCandidates(VC)
        val canAmbush        = ambushCandidates.nonEmpty && !momentumInPlay(Mo_Claymores)
        val canSubvert       = game.patronage >= 17

        if (canAmbush)
          ambushActivity(VC, ambushCandidates, March, actNum, false)

        canAmbush || (canSubvert && VC_Bot.subvertActivity()) || VC_Bot.taxActivity()
      }

      VC_Bot.marchOp(params, actNum) match {
        case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity())
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
      if (rollDice(3) > game.availablePieces.totalOf(VCPieces))
        TrungDraw
      else if (!VC_Bot.threePlusGuerrillasWithUSTroopsNoVCBase)
        flipCard(params)
      else {
        val (operation, ambushed) = VC_Bot.attackOp(params, actNum)

        operation match {
          case Some(_) if ambushed => TrungComplete(true)
          case Some(_)             => TrungComplete(params.specialActivity && VC_Bot.taxActivity())
          case _                   => TrungNoOp
        }
      }
    }


    // ------------------------------------------------------------
    // Back Operation
    // ------------------------------------------------------------
    def executeBack(params: Params, specialDone: Boolean): TrungResult = {
      def doSpecialActivity(): Boolean = {
        val canSubvert       = game.patronage >= 17

        (canSubvert && VC_Bot.subvertActivity()) || VC_Bot.taxActivity()
      }

      VC_Bot.rallyOp(params, actNum) match {
        case Some(_) => TrungComplete(params.specialActivity && doSpecialActivity())
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
        val canSubvert       = game.patronage >= 17

        (canSubvert && VC_Bot.subvertActivity()) || VC_Bot.taxActivity()
      }

      val numVCGuerrillas = (sp: Space) => sp.pieces.totalOf(VCGuerrillas)
      val vcGuerrillasOnMap = game totalOnMap numVCGuerrillas

      if (vcGuerrillasOnMap < 15)
        TrungDraw
      else {
        val specialDone = params.specialActivity && doSpecialActivity()

        if (game.spaces exists (sp => numVCGuerrillas(sp) >= 3))
          VC_Bot.marchOp(params, actNum) match {
            case Some(_) => TrungComplete(true)
            case None    => TrungNoOp
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

      if (operation.nonEmpty || specialDone)
        TrungComplete(specialDone)
      else
        TrungNoOp
    }

  }

}
