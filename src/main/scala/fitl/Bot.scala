
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

object Bot {

  // These values are used with HighestScore and LowestScore filters
  // when a candidate is not qualified to produce a score.
  // For example if it is a LoC and we are only considering
  // Citys and Provinces.
  val NO_SCORE  = -1000

  // Used to keep track of moveDestinations during and operation
  // The Bots will never move pieces out of a space that has been
  // selected as a move destination.
  private var moveDestinations = Set.empty[String]

  def botLog(msg: => String) = if (game.botLogging) log(msg)
  def msgResult(msg: String, result: Any): String = {
    val resultStr = result match {
      case true  => "yes"
      case false => "no"
      case other => result.toString
    }

    s"$msg $resultStr"
  }


  case class Params(
    includeSpecial: Boolean         = false,
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

  // Used to implement the Eligibility Tables
  case class ActionEntry(val action: Action, desc: String, test: (Faction) => Boolean)

  // Possible results when attempting to execute the instructions
  // on a Trung Card
  sealed trait TrungResult
  case object TrungComplete extends TrungResult
  case object TrungDraw     extends TrungResult
  case object TrungFlip     extends TrungResult
  case object TrungNoOp     extends TrungResult


  // Trung Card defintion
  // The front and back of each card are treated a seperate
  // entities internally.

  trait TrungCard {
    val faction: Faction
    val id: String
    val flipSide: TrungCard

    def isFront = id.length == 1

    override def toString() = s"Trung: $faction - $id"

    def execute(faction: Faction, params: Params): TrungResult
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

  def vulnerableInsurgents(sp: Space) = {
    val forces = sp.pieces.totalOf(NVATroops::ActiveGuerrillas)
    val bases  = if (sp.pieces.has(UndergroundGuerrillas))
      0
    else
      sp.pieces.totalOf(InsurgentBases)
    forces + bases
  }

  //  Assault firepower of the sapce plus any modifiers for
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
      !sp.isLOC && sp.population > 0 && sp.support > Neutral && numUS < numUnderground
    }
  )

  val HasCoinControl = new BooleanPriority[Space](
    "COIN Control",
    sp => !sp.isLOC && sp.coinControlled
  )

  val CityProvinceNoActiveSupport = new BooleanPriority[Space](
    "City or Province not at Active Support",
    sp => !sp.isLOC && sp.population > 0 && sp.support != ActiveSupport
  )

  val PoliceWithoutUSTroops = new BooleanPriority[Space](
    "Police without US Troops",
    sp => sp.pieces.has(ARVNPolice) && !sp.pieces.has(USTroops)
  )

  val COINFirepowerLessThanVulnerable = new BooleanPriority[Space](
    "COIN Firepower < vulnerable enemies where US pieces",
    sp => sp.pieces.has(USPieces) && coinFirepower(sp) < vulnerableInsurgents(sp)
  )

  val HasUSBase = new BooleanPriority[Space](
    "US Base",
    _.pieces.has(USBase)
  )

  val MostPopulation = new HighestScore[Space](
    "Most Population",
    sp => if (sp.isLOC) NO_SCORE else sp.population
  )

  val HasEnemyBase = new BooleanPriority[Space](
    "Enemy Base",
    _.pieces.has(InsurgentBases)
  )

  val LocWithEnemyPieces = new BooleanPriority[Space](
    "LoC with enemy pieces",
    sp => sp.isLOC && sp.pieces.has(InsurgentPieces)
  )

  val MostTotalOpposition = new HighestScore[Space](
    "Most Total Opposition",
    sp => if (sp.isLOC) NO_SCORE else sp.oppositionValue
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
    sp => !sp.isLOC && !sp.coinControlled && isInSouthVietnam(sp.name)
  )

  // MostPopulation is defined in the US space priority filters

  // LocWithEnemyPieces is defined in the US space priority filters

  // HasEnemyBase is defined in the US space priority filters

  val MostTotalSupport = new HighestScore[Space](
    "Most Total Support",
    sp => if (sp.isLOC) NO_SCORE else sp.supportValue
  )

  val MostArvnFirepower = new HighestScore[Space](
    "Most ARVN Firepower",
    sp => arvnFirepower(sp)
  )

  val CityProvinceFewestEnemyPieces = new LowestScore[Space](
    "City or Province with Fewest Entmy Pieces",
    sp => if (sp.isLOC) NO_SCORE else sp.pieces.totalOf(InsurgentPieces)
  )

  // MostTotalOpposition  is defined in the US space priority filters



  // NVA space priority filters
  // -----------------------------------------------------------

  // MostTotalSupport  is defined in the ARVN space priority filters

  val VulnerableNVABase = new BooleanPriority[Space](
    "Vulnerable NVA Base",
    sp => sp.pieces.has(NVABase::NVATunnel::Nil) && !sp.pieces.has(UndergroundGuerrillas)
  )

  val CityProvinceMostNVAGuerrillas = new HighestScore[Space](
    "City or Province with Most NVA Guerrilllas",
    sp => if (sp.isLOC)
      NO_SCORE
    else
       sp.pieces.totalOf(NVAGuerrillas)
  )

  val MostNVABases = new HighestScore[Space](
    "Most NVA Bases",
    sp => if (sp.isLOC)
      NO_SCORE
    else
       sp.pieces.totalOf(NVABase::NVATunnel::Nil)
  )

  val LaosCambodiaWithCoinControl = new BooleanPriority[Space](
    "Laos/Cambodia with COIN Control",
    sp => !sp.isLOC && sp.coinControlled && isInLaosCambodia(sp.name)
  )

  val AdjacentToMostNVATroopsYetToMarch = new HighestScore[Space](
    "Adjacent to most NVA Troops yet to March",
    sp => {
        // Get all adjacent spaces that have not yet been selecte as march destinations
        val adjacent = NVA_Bot.getNVAAdjacent(sp.name) filterNot moveDestinations.contains
        adjacent.foldLeft(0) { (totalTroops, name) =>
          totalTroops + game.getSpace(name).pieces.numOf(NVATroops)
        }
    }
  )

  val CityProvinceInSouthWithoutNVAControl = new BooleanPriority[Space](
    "City or Province in South Vietnam without NVA Control",
    sp => !sp.isLOC && !sp.nvaControlled && isInSouthVietnam(sp.name)
  )

  // HasCoinControl is defined in the US space priority filters

  // MostPopulation is defined in the US space priority filters

  val CityProvinceWithoutUSBase = new BooleanPriority[Space](
    "City or Province without US Base",
    sp => !sp.isLOC && !sp.pieces.has(USBase)
  )

  val HasVCBase = new BooleanPriority[Space](
    "VC Base",
    sp => sp.pieces.has(VCBase::VCTunnel::Nil)
  )

  val CityProvinceFewestNonNVAPieces = new LowestScore[Space](
    "City or Province with fewest non-NVA pieces",
    sp => {
      if (sp.isLOC)
        NO_SCORE
      else
        sp.pieces.except(NVAPieces).total
    }
  )

  val CityProvinceMostCoinCubes = new HighestScore[Space](
    "City or Province with most COIN cubes",
    sp => {
      if (sp.isLOC)
        NO_SCORE
      else
        sp.pieces.totalOf(CoinCubes)
    }
  )

  val CityProvinceHighestAjacentPopulation = new HighestScore[Space](
    "City or Province with highest total ajacent Pop",
    sp => {
      if (sp.isLOC)
        NO_SCORE
      else {
        // This is not used for March so we do not count all spaces in/adjacent to
        // Laos Cmbodia as adjacent when trail == 4
        getAdjacent(sp.name).foldLeft(0) { (sum, name) =>
          val adj_sp = game.getSpace(name)
          val pop = if (adj_sp.isLOC) 0 else adj_sp.population
          sum + pop
        }
      }
    }
  )


  // VC space priority filters
  // -----------------------------------------------------------
  
  val VulnerableVCBase = new BooleanPriority[Space](
    "Vulnerable VC Base",
    sp => sp.pieces.has(VCBase::VCTunnel::Nil) && !sp.pieces.has(UndergroundGuerrillas)
  )

  val CityProvinceNoActiveOpposition = new BooleanPriority[Space](
    "City or Province not at Active Support",
    sp => !sp.isLOC && sp.population > 0 && sp.support != ActiveOpposition
  )

  val CityProvinceFewestVCPieces = new LowestScore[Space](
    "City or Province with fewest VC pieces",
    sp => {
      if (sp.isLOC)
        NO_SCORE
      else
        sp.pieces.totalOf(VCPieces)}
  )

  // MostPopulation is defined in the US space priority filters

  val MostVCBases = new HighestScore[Space](
    "Most VC Bases",
    sp => if (sp.isLOC)
      NO_SCORE
    else
       sp.pieces.totalOf(VCBase::VCTunnel::Nil)
  )

  // HasCoinControl is defined in the US space priority filters

  // MostTotalSupport  is defined in the ARVN space priority filters

  val HasNVAControl = new BooleanPriority[Space](
    "NVA Control",
    sp => !sp.isLOC && sp.nvaControlled
  )

  val OneTwoVCGuerrillasRoomForBase = new BooleanPriority[Space](
    "1-2 VC Guerrillas adn room for a Base",
    sp => {
      val numGuerrillas = sp.pieces.totalOf(VCGuerrillas)
      !sp.isLOC && sp.totalBases < 2 && (numGuerrillas == 1 || numGuerrillas == 2)
    }
  )

  // MostTotalOpposition  is defined in the US space priority filters

  val CityProvinceMostVCGuerrillas = new HighestScore[Space](
    "City or Province with most VC Guerrillas",
    sp => if (sp.isLOC)
      NO_SCORE
    else
       sp.pieces.totalOf(VCGuerrillas)
  )

  val CityProvinceFewestNonVCPieces = new LowestScore[Space](
    "City or Province with fewest non-VC pieces",
    sp => {
      if (sp.isLOC)
        NO_SCORE
      else
        sp.pieces.except(VCPieces).total
    }
  )
  
  val OnePlusEconLoc = new BooleanPriority[Space](
    "1+ Econ LoC",
    sp => sp.isLOC && sp.printedEconValue > 0
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

  // ================================================================
  // US Specific code
  // ================================================================
  object US_Bot {

    // US Spaces Priorities: Shift Toward Active Support
    def pickSpaceTowardActiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             CityProvinceNoActiveSupport),
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botLog(s"US Select space (Shift Toward Active Support): [${andList(candidates)}]")
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

      botLog(s"US Select space (Place Bases): [${andList(candidates)}]")
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

      botLog(s"US Select space (Place Cubes or Special Forces): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

    // US Spaces Priorities: Sweep Destinations
    def pickSpaceSweepDest(candidates: List[Space], placingMovingUSTroops: Boolean = false): Space = {

      val priorities = List(
        filterIf(true,                  ProvinceWithUSBaseAndCubes),
        filterIf(true,                  USTroopsIrregLessUndergroundGuerrillasSupport),
        filterIf(game.isHuman(ARVN),    HasCoinControl),
        filterIf(true,                  CityProvinceNoActiveSupport),
        filterIf(placingMovingUSTroops, PoliceWithoutUSTroops),
        filterIf(true,                  MostPopulation),
        filterIf(true,                  HasEnemyBase)
      ).flatten

      botLog(s"US Select space (Sweep Destinations): [${andList(candidates)}]")
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

      botLog(s"US Select space (Air Lift Destinations): [${andList(candidates)}]")
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

      botLog(s"US Select space (Air Strike): [${andList(candidates)}]")
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

      botLog(s"US Select space (Remove or Replace): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

  }


  // ================================================================
  // ARVN Specific code
  // ================================================================
  object ARVN_Bot {

    // ARVN Spaces Priorities: Shift Toward Passive Support
    def pickSpaceTowardPassiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             MostPopulation),
        filterIf(true,             HasEnemyBase),
        filterIf(game.isHuman(VC), MostTotalOpposition)
      ).flatten

      botLog(s"ARVN Select space (Shift Toward Passive Support): [${andList(candidates)}]")
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

      botLog(s"ARVN Select space (Place Bases): [${andList(candidates)}]")
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

      botLog(s"ARVN Select space (Place Cubes or Rangers): [${andList(candidates)}]")
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

      botLog(s"ARVN Select space (Sweep or Transport Destinations): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Patrol Destinations
    def pickSpacePatrolDest(candidates: List[Space]): Space = {

      val priorities = List(
        SouthWithoutCoinControl,
        MostPopulation,
        LocWithEnemyPieces
      )

      botLog(s"ARVN Select space (Patrol Destinations): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

    // ARVN Spaces Priorities: Govern
    def pickSpaceGovern(candidates: List[Space]): Space = {

      val priorities = List(
        MostPopulation,
        MostTotalSupport
      )

      botLog(s"ARVN Select space (Govern): [${andList(candidates)}]")
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

      botLog(s"ARVN Select space (Remove or Replace): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
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


    // NVA Spaces Priorities: Place Bases or Tunnels
    def pickSpacePlaceBases(candidates: List[Space]): Space = {

      val priorities = List(
        CityProvinceMostNVAGuerrillas,
        LaosCambodiaWithCoinControl,
        MostPopulation,
        CityProvinceWithoutUSBase,
        CityProvinceHighestAjacentPopulation
      )

      botLog(s"NVA Select space (Place Bases or Tunnels): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place NVA Troops
    def pickSpacePlaceNVATroops(candidates: List[Space]): Space = {

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

      botLog(s"NVA Select space (Place NVA Troops): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place NVA Guerrilllas
    def pickSpacePlaceNVAGuerrillas(candidates: List[Space]): Space = {

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

      botLog(s"NVA Select space (Place NVA Guerrilllas): [${andList(candidates)}]")
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

      botLog(s"NVA Select space (March Destinations): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

    // NVA Spaces Priorities: Place Terror
    def pickSpacePlaceTerror(candidates: List[Space]): Space = {

      val priorities = List(
        MostTotalSupport,
        VulnerableNVABase,
        CityProvinceMostNVAGuerrillas
      )

      botLog(s"NVA Select space (Place Terror): [${andList(candidates)}]")
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

      botLog(s"NVA Select space (Remove or Replace): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }

  }

  // ================================================================
  // VC Specific code
  // ================================================================
  object VC_Bot {
    
    // VC Spaces Priorities: Shift Toward Active Opposition
    def pickSpaceTowardActiveSupport(candidates: List[Space]): Space = {

      val priorities = List(
        filterIf(true,             CityProvinceNoActiveOpposition),
        filterIf(true,             MostPopulation),
        filterIf(game.isHuman(US), MostTotalSupport)
      ).flatten

      botLog(s"VC Select space (Shift Toward Active Opposition): [${andList(candidates)}]")
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

      botLog(s"VC Select space (Place Terror): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
    }
    
    // VC Spaces Priorities: Place Bases or Tunnels
    def pickSpacePlaceBase(candidates: List[Space]): Space = {
      val priorities = List(
        MostPopulation,
        CityProvinceMostVCGuerrillas,
        CityProvinceFewestNonVCPieces
      )

      botLog(s"VC Select space (Place Bases or Tunnels): [${andList(candidates)}]")
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

      botLog(s"VC Select space (Place Guerrilllas): [${andList(candidates)}]")
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

      botLog(s"VC Select space (March Destinations): [${andList(candidates)}]")
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

      botLog(s"VC Select space (Tax): [${andList(candidates)}]")
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

      botLog(s"VC Select space (Remove or Replace): [${andList(candidates)}]")
      bestCandidate(candidates, priorities)
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
  def chooseAction(faction: Faction, prevEntry: Option[ActionEntry]): Option[ActionEntry] = {
    val isFirstEligible = game.sequence.numActors == 0

    botLog {
      val which = if (isFirstEligible) "1st" else "2nd"
      if (prevEntry.isEmpty)
        s"\nChoose Action using NP Eligiblity Table ($which eligible)"
      else
        s"\nChoose Action using NP Eligiblity Table ($which eligible) after ${prevEntry.get.action} was not possible"
    }
    botLog(separator())

    val table = {
      val t = if (isFirstEligible)
        firstEligibileTable
      else
        secondEligibileTable

      prevEntry match {
        case None       => t
        case Some(prev) => t drop (t.indexOf(prev) + 1)
      }
    }

    //  In case we previously used the last entry in the table and it was
    //  not possible to carry it out.
    if (table.isEmpty)
      None
    else
      table find { entry =>
        val result = entry.test(faction)
        botLog(msgResult(entry.desc, result))
        result
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
    val card = deck(game.currentCard)

    //  Init global variables
    moveDestinations = Set.empty


    if (game.executingPivotalEvent) {
      //  The Pivotal events are single events which are always in the executeUnshaded() function.
      card.executeEvent(faction)
    }
    else {


      def selectAction(prevEntry: Option[ActionEntry]): Unit = {
        chooseAction(faction, prevEntry) match {
          case None =>  // Could not carry out an action so Pass
            factionPasses(faction)

          case Some(ActionEntry(Pass, _, _)) => // TAble resolve to Pass
            factionPasses(faction)

            case Some(ActionEntry(Event, _, _)) =>
              card.executeEvent(faction)

            case entry @ Some(ActionEntry(action, _, _)) =>  // LimitedOp, OpOnly, or OpPlusSpecial
              val sa    = action == OpPlusSpecial
              val maxsp = if (action == LimitedOp) Some(1) else None
              val params = Params(includeSpecial = sa, maxSpaces = maxsp)

              if (executeOp(faction, params)) {
                game = game.copy(sequence = game.sequence.addActor(faction, action))
                log()
                log(s"Move the $faction cylinder to the $action box")
              }
              else
                selectAction(entry)  // The operation could not be carried out so try again
        }
      }

      selectAction(None)

    }
  }


  // Return TRUE if the operation was carried out
  // It may not be able to be done if:
  // ARVN resources == 0 and one of the COIN factions is Human
  // There are not eligible spaces to operate on, etc.
  // We keep track of the first card that we drew for the
  // off chance that we cycle through all of them and cannot perform
  // and operation.  Probably won't happend but this will prevent and
  // endless loop.
  def executeOp(faction: Faction, params: Params): Boolean = {
    val firstCard = drawTrungCard(faction)

    def executeCard(trungCard: TrungCard): Boolean = {
       trungCard.execute(faction, params) match {
         case  TrungComplete =>
           true

         case  TrungDraw =>
           val nextCard = drawTrungCard(faction)
           if (nextCard == firstCard)
             false
           else
             executeCard(nextCard)

         case  TrungFlip =>
           executeCard(trungCard.flipSide)

         case  TrungNoOp =>
           false
       }
    }

    executeCard(firstCard)
  }



  // The Trung Deck contains only the face up cards.
  // The face down are accessed from the flipSide of the face up cards.
  val TrungDeck = List(
    Trung_US_A,  Trung_ARVN_G,  Trung_NVA_N,  Trung_VC_U,
    Trung_US_B,  Trung_ARVN_H,  Trung_NVA_P,  Trung_VC_V,
    Trung_US_C,  Trung_ARVN_J,  Trung_NVA_Q,  Trung_VC_W,
    Trung_US_D,  Trung_ARVN_K,  Trung_NVA_R,  Trung_VC_X,
    Trung_US_E,  Trung_ARVN_L,  Trung_NVA_S,  Trung_VC_Y,
    Trung_US_F,  Trung_ARVN_M,  Trung_NVA_T,  Trung_VC_Z
  )


  // ================================================================
  // US Trung Cards
  // ================================================================

  object Trung_US_A extends TrungCard {
    val faction = US
    val id = "A"
    lazy val flipSide = Trung_US_AA


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_AA extends TrungCard {
    val faction = US
    val id = "AA"
    lazy val flipSide = Trung_US_A


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_B extends TrungCard {
    val faction = US
    val id = "B"
    lazy val flipSide = Trung_US_BB


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_BB extends TrungCard {
    val faction = US
    val id = "BB"
    lazy val flipSide = Trung_US_B


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_C extends TrungCard {
    val faction = US
    val id = "C"
    lazy val flipSide = Trung_US_CC


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_CC extends TrungCard {
    val faction = US
    val id = "CC"
    lazy val flipSide = Trung_US_C


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_D extends TrungCard {
    val faction = US
    val id = "D"
    lazy val flipSide = Trung_US_DD


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_DD extends TrungCard {
    val faction = US
    val id = "DD"
    lazy val flipSide = Trung_US_D


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_E extends TrungCard {
    val faction = US
    val id = "E"
    lazy val flipSide = Trung_US_EE


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_EE extends TrungCard {
    val faction = US
    val id = "EE"
    lazy val flipSide = Trung_US_E


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_F extends TrungCard {
    val faction = US
    val id = "F"
    lazy val flipSide = Trung_US_FF


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_US_FF extends TrungCard {
    val faction = US
    val id = "FF"
    lazy val flipSide = Trung_US_F


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }


  // ================================================================
  // ARVN Trung Cards
  // ================================================================

  object Trung_ARVN_G extends TrungCard {
    val faction = ARVN
    val id = "G"
    lazy val flipSide = Trung_ARVN_GG


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_GG extends TrungCard {
    val faction = ARVN
    val id = "GG"
    lazy val flipSide = Trung_ARVN_G


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_H extends TrungCard {
    val faction = ARVN
    val id = "H"
    lazy val flipSide = Trung_ARVN_HH


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_HH extends TrungCard {
    val faction = ARVN
    val id = "HH"
    lazy val flipSide = Trung_ARVN_H


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_J extends TrungCard {
    val faction = ARVN
    val id = "J"
    lazy val flipSide = Trung_ARVN_JJ


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_JJ extends TrungCard {
    val faction = ARVN
    val id = "JJ"
    lazy val flipSide = Trung_ARVN_J


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_K extends TrungCard {
    val faction = ARVN
    val id = "K"
    lazy val flipSide = Trung_ARVN_KK


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_KK extends TrungCard {
    val faction = ARVN
    val id = "KK"
    lazy val flipSide = Trung_ARVN_K


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_L extends TrungCard {
    val faction = ARVN
    val id = "L"
    lazy val flipSide = Trung_ARVN_LL


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_LL extends TrungCard {
    val faction = ARVN
    val id = "LL"
    lazy val flipSide = Trung_ARVN_L


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_M extends TrungCard {
    val faction = ARVN
    val id = "M"
    lazy val flipSide = Trung_ARVN_MM


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_ARVN_MM extends TrungCard {
    val faction = ARVN
    val id = "MM"
    lazy val flipSide = Trung_ARVN_M


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }


  // ================================================================
  // NVA Trung Cards
  // ================================================================

  object Trung_NVA_N extends TrungCard {
    val faction = NVA
    val id = "N"
    lazy val flipSide = Trung_NVA_NN


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_NN extends TrungCard {
    val faction = NVA
    val id = "NN"
    lazy val flipSide = Trung_NVA_N


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_P extends TrungCard {
    val faction = NVA
    val id = "P"
    lazy val flipSide = Trung_NVA_PP


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_PP extends TrungCard {
    val faction = NVA
    val id = "PP"
    lazy val flipSide = Trung_NVA_P


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_Q   extends TrungCard {
    val faction = NVA
    val id = "Q"
    lazy val flipSide = Trung_NVA_QQ


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_QQ   extends TrungCard {
    val faction = NVA
    val id = "QQ"
    lazy val flipSide = Trung_NVA_Q


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_R   extends TrungCard {
    val faction = NVA
    val id = "R"
    lazy val flipSide = Trung_NVA_RR


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_RR   extends TrungCard {
    val faction = NVA
    val id = "RR"
    lazy val flipSide = Trung_NVA_R


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_S   extends TrungCard {
    val faction = NVA
    val id = "S"
    lazy val flipSide = Trung_NVA_SS


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_SS   extends TrungCard {
    val faction = NVA
    val id = "SS"
    lazy val flipSide = Trung_NVA_S


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_T   extends TrungCard {
    val faction = NVA
    val id = "T"
    lazy val flipSide = Trung_NVA_TT


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_NVA_TT   extends TrungCard {
    val faction = NVA
    val id = "TT"
    lazy val flipSide = Trung_NVA_T


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }


  // ================================================================
  // VC Trung Cards
  // ================================================================

  object Trung_VC_U extends TrungCard {
    val faction = VC
    val id = "U"
    lazy val flipSide = Trung_VC_UU


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_UU extends TrungCard {
    val faction = VC
    val id = "UU"
    lazy val flipSide = Trung_VC_U


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_V extends TrungCard {
    val faction = VC
    val id = "V"
    lazy val flipSide = Trung_VC_VV


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_VV extends TrungCard {
    val faction = VC
    val id = "VV"
    lazy val flipSide = Trung_VC_V


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_W extends TrungCard {
    val faction = VC
    val id = "W"
    lazy val flipSide = Trung_VC_WW


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_WW extends TrungCard {
    val faction = VC
    val id = "WW"
    lazy val flipSide = Trung_VC_W


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_X extends TrungCard {
    val faction = VC
    val id = "X"
    lazy val flipSide = Trung_VC_XX


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_XX extends TrungCard {
    val faction = VC
    val id = "XX"
    lazy val flipSide = Trung_VC_X


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_Y extends TrungCard {
    val faction = VC
    val id = "Y"
    lazy val flipSide = Trung_VC_YY


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_YY extends TrungCard {
    val faction = VC
    val id = "YY"
    lazy val flipSide = Trung_VC_Y


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_Z extends TrungCard {
    val faction = VC
    val id = "Z"
    lazy val flipSide = Trung_VC_ZZ


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }

  object Trung_VC_ZZ extends TrungCard {
    val faction = VC
    val id = "ZZ"
    lazy val flipSide = Trung_VC_Z


    def execute(faction: Faction, params: Params): TrungResult = {
      TrungComplete
    }
  }
}
