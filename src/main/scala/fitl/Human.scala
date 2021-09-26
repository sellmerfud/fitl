
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

// Functions to handle human commands/special activities
object Human {

  private var trainingSpaces = Set.empty[String]

  case class Params(
    includeSpecial: Boolean         = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    assaultRemovesTwoExtra: Boolean = false, // M48 Patton (unshaded)
    onlyIn: Option[Set[String]]     = None   // Limit command to the given spaces
  ) {
    val limOpOnly = maxSpaces == Some(1)

    def spaceAllowed(name: String) = onlyIn.isEmpty || onlyIn.contains(name)
  }


  // Aid in keeping track of when a special activity can be taken
  object Special {
    private var allowSpecial = false
    private var specialTaken = false

    var selectedSpaces = Set.empty[String]

    def init(params: Params): Unit = {
      allowSpecial   = params.includeSpecial
      specialTaken   = false
      selectedSpaces = Set.empty  // For Advise/Govern activities
    }

    def allowed = allowSpecial && !specialTaken
    def taken   = specialTaken

    def completed() = specialTaken = true
    def cancelled() = specialTaken = false
  }

  // Used during a turn to keep track of pieces that have already moved
  // in each space.
  // For patrol,  we add pieces to a moving group if they have terminated
  // movement in a space with Insurgent pieces.  This lets us know that
  // those pieces cannot continue moving.
  class MovingGroups() {
    // Map Space Name, to Pieces in that space that cannot move.
    var groups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())

    def apply(name: String): Pieces = groups(name)
    def add(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) + pieces)
    def remove(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) - pieces)

    def toList = groups.toList.sortBy(_._1)
    def size = groups.size
  }
  
  def noteIf(cond: Boolean, note: String): Option[String] = if (cond) Some(note) else None

  // TODO:  Perhaps this can be shared with the Bot code
  //        Would have to also put MovingGroups class in shared trait
  def sweepSources(destName:String, faction: Faction, alreadyMoved: MovingGroups): List[String] = {
    val troopType = if (faction == US) USTroops else ARVNTroops
    val srcSpaces = (game.spaces 
                     filter { sp =>
                       !sp.isNorthVietnam &&
                       sp.name != destName &&
                       (sp.pieces - alreadyMoved(sp.name)).has(troopType) &&
                       adjacentForSweep(sp.name, destName)
                     })
    spaceNames(srcSpaces)
  }
  
  // Returns false if user decides not to pacify in space
  def pacifySpace(name: String, faction: Faction): Boolean = {
    val cost        = if (momentumInPlay(Mo_BlowtorchKomer)) 1 else 3
    val maxLevel    = if (faction == US && capabilityInPlay(CORDS_Shaded)) PassiveSupport else ActiveSupport
    val sp          = game.getSpace(name)
    val maxShift    = ((maxLevel.value - sp.support.value) max 0) min 2
    val maxInSpace  = maxShift + sp.terror
    val maxPossible = maxInSpace min (game.arvnResources / cost)

    log(s"\nPacifying in $name")
    log(separator())
    if (momentumInPlay(Mo_BlowtorchKomer))
      log(s"Each terror/shift cost only 1 resource [Momentum: $Mo_BlowtorchKomer]")
    if (maxLevel == PassiveSupport)
      log(s"Cannot shift to Active Suport [$CORDS_Shaded]")
    
    if (maxPossible == 0) {
      println(s"\nIt is not possible to pacify in $name")
      false
    }
    else {
      val choices = List.range(maxPossible, -1, -1) map {
        case 0                    => (0 -> s"Do not pacify in $name")
        case n if sp.terror == 0  => (n -> s"Shift ${amountOf(n, "level")} to ${SupportType(sp.support.value + n)}")
        case n if n <= sp.terror  => (n -> s"Remove ${amountOf(n, "terror marker")}")
        case n                    => (n -> s"Remove ${amountOf(sp.terror, "terror marker")} and shift ${amountOf(n - sp.terror, "level")} to ${SupportType(sp.support.value + (n - sp.terror))}")
      }
      
      val num = askMenu(choices, "\nChoose one:", allowAbort = false).head
      if (num == 0)
        false
      else {
        val shift = (num - sp.terror) max 0
        log()
        decreaseResources(ARVN, num * cost)
        removeTerror(name, num min sp.terror)
        increaseSupport(name, shift)
        true
      }
    }
  }

  // Returns false if user decides not to agitate in space
  def agitateSpace(name: String): Boolean = {
    val sp          = game.getSpace(name)
    val maxShift    = ((sp.support.value - ActiveOpposition.value) max 0) min 2
    val maxInSpace  = maxShift + sp.terror
    val cadres      = capabilityInPlay(Cadres_Unshaded)
    val maxPossible = maxInSpace min game.arvnResources

    log(s"\nAgitating in $name")
    log(separator())
    if (cadres)
      log(s"VC must remove 2 guerrillas per agitate space [$Cadres_Unshaded]")
    
    if (cadres && sp.pieces.totalOf(VCGuerrillas) < 2) {
      log(s"\nThere are not two VC guerrillas in $name")
      false
    }
    else if (maxPossible == 0) {
      log(s"\nIt is not possible to agitate in $name")
      false
    }
    else {
      val choices = List.range(maxPossible, -1, -1) map {
        case 0                    => (0 -> s"Do not agitate in $name")
        case n if sp.terror == 0  => (n -> s"Shift ${amountOf(n, "level")} to ${SupportType(sp.support.value - n)}")
        case n if n <= sp.terror  => (n -> s"Remove ${amountOf(n, "terror marker")}")
        case n                    => (n -> s"Remove ${amountOf(sp.terror, "terror marker")} and shift ${amountOf(n - sp.terror, "level")} to ${SupportType(sp.support.value - (n - sp.terror))}")
      }
      
      val num = askMenu(choices, "\nChoose one:", allowAbort = false).head
      if (num == 0)
        false
      else {
        val shift = (num - sp.terror) max 0
        log()
        if (cadres) {
          val toRemove = askPieces(sp.pieces, 2, VCGuerrillas, Some(s"Remove guerrillas for [$Cadres_Unshaded]"))
          removeToAvailable(name, toRemove)
        }
        decreaseResources(VC, num)
        removeTerror(name, num min sp.terror)
        decreaseSupport(name, shift)
        true
      }
    }
  }

  
  
  // A human player has opted to take an action on the current card.
  def act(): Unit = {
    object Pass extends Exception
    val faction = game.nextUp.get

    try {
      val action = if (game.executingPivotalEvent)
        Event
      else {
        val choices: List[(Option[Action], String)] =
          (game.sequence.availableActions map (a => Some(a) -> a.toString)) :+ (None -> "Pass")

        askMenu(choices, "\nChoose one:").head getOrElse { throw Pass }
      }

      game = game.copy(sequence = game.sequence.addActor(faction, action))
      log()
      log(s"Move the $faction cylinder to the $action box")

      action match {
        case Event         => executeEvent(faction)
        case OpPlusSpecial => executeOp(faction, Params(includeSpecial = true))
        case OpOnly        => executeOp(faction)
        case LimitedOp     => executeOp(faction, Params(maxSpaces = Some(1)))
      }

    }
    catch {
      case Pass =>
        factionPasses(faction)
    }
  }

  // US special activity
  def doAdvise(params: Params): Unit = {
    log("\nUS Advise special activity not yet implemented.")
  }

  // US special activity
  def doAirLift(params: Params): Unit = {
    log("\nUS Air Lift special activity not yet implemented.")
  }

  // US special activity
  def doAirStrike(params: Params): Unit = {
    log("\nUS Air Strike special activity not yet implemented.")
  }

  // ARVN special activity
  def doGovern(params: Params): Unit = {
    log("\nARVN Govern special activity not yet implemented.")
  }

  // ARVN special activity
  def doTransport(params: Params): Unit = {
    log("\nARVN Transport special activity not yet implemented.")
  }

  // ARVN special activity
  def doRaid(params: Params): Unit = {
    log("\nARVN Raid special activity not yet implemented.")
  }

  // NVA special activity
  def doInfiltrate(params: Params): Unit = {
    log("\nNVA Infiltrate special activity not yet implemented.")
  }

  // NVA special activity
  def doBombard(params: Params): Unit = {
    log("\nNVA Bombard special activity not yet implemented.")
  }

  // NVA/VC special activity
  def doAmbush(faction: Faction, params: Params): Unit = {
    log(s"\n${faction} Ambush special activity not yet implemented.")
  }

  // VC special activity
  def doTax(params: Params): Unit = {
    log("\nVC Tax special activity not yet implemented.")
  }

  // VC special activity
  def doSubvert(params: Params): Unit = {
    log("\nVC Subvert special activity not yet implemented.")
  }


  def executeSpecialActivity(faction: Faction, params: Params, activities: List[SpecialActivity]): Unit = {
    val choices: List[(Option[SpecialActivity], String)] =
      (activities map (a => Some(a) -> a.toString)) :+
      (None -> "Do not perform a Speical Activitiy now")

    askMenu(choices, "\nChoose special activity:").head foreach { activity =>
      val savedState = game
      try {
        activity match {
          case Advise     => doAdvise(params)
          case AirLift    => doAirLift(params)
          case AirStrike  => doAirStrike(params)
          case Govern     => doGovern(params)
          case Transport  => doTransport(params)
          case Raid       => doRaid(params)
          case Infiltrate => doInfiltrate(params)
          case Bombard    => doBombard(params)
          case Ambush     => doAmbush(faction, params)
          case Tax        => doTax(params)
          case Subvert    => doSubvert(params)
        }
        Special.completed()
      }
      catch {
        case AbortAction =>
          println(s"\n>>>> Aborting $activity special activity <<<<")
          println(separator())
          displayGameStateDifferences(game, savedState)
          game = savedState
      }
    }
  }


  def executeEvent(faction: Faction): Unit = {
    println("executeEvent() not implemented")

  }

  def executeOp(faction: Faction, params: Params = Params()): Unit = {
    Special.init(params)
    trainingSpaces = Set.empty

    faction match {
      case US  | ARVN => executeCoinOp(faction, params)
      case NVA | VC   => executeInsurgentOp(faction, params)
    }
  }

  def executeCoinOp(faction: Faction, params: Params): Unit = {

    val landsdale = faction == US && momentumInPlay(Mo_GeneralLansdale)
    val notes = List(
      noteIf(game.inMonsoon, s"Sweep is prohibited [Not allowed in Monsoon]"),
      noteIf(landsdale,      s"US Assault is prohibited [Momentum: ${Mo_GeneralLansdale}]")
    ).flatten

    val availOps = CoinOp.ALL filter {
      case Train   => true
      case Patrol  => true
      case Sweep   => !game.inMonsoon
      case Assault => !landsdale
    }
    val choices = availOps map (op => op -> op.toString)

    if (notes.nonEmpty) {
      println("Notes:")
      println(separator())
      notes foreach println
    }

    askMenu(choices, "\nChoose operation:").head match {
      case Train   => executeTrain(faction, params)
      case Patrol  => executePatrol(faction, params)
      case Sweep   => executeSweep(faction, params)
      case Assault => executeAssault(faction, params)
    }
  }

  def executeInsurgentOp(faction: Faction, params: Params): Unit = {
    val notes = List(
      noteIf(game.inMonsoon, s"March is prohibited [Not allowed in Monsoon]")
    ).flatten

    val availOps = InsurgentOp.ALL filter {
      case Rally  => true
      case March  => !game.inMonsoon
      case Attack => true
      case Terror => true
    }
    val choices = availOps map (op => op -> op.toString)
    
    askMenu(choices, "\nChoose operation:").head match {
      case Rally  => executeRally(faction, params)
      case March  => executeMarch(faction, params)
      case Attack => executeAttack(faction, params)
      case Terror => executeTerror(faction, params)
    }
  }



  // ====================================================================
  // == Coin Operations =================================================
  // ====================================================================

  // CombActionPlatoons_Unshaded
  //    US Training places (relocates) 1 added ARVN Police cube
  //    in one of the Training spaces with US Troops.
  //    Does not cost any added resources (if no Base/cubes placed in that space)
  // CORDS_Unshaded
  //    US Training may pacify in 2 Training spaces (cost is unchanged)
  // CORDS_Shaded
  //    US Training may pacify only to PassiveSupport (Not ActiveSupport)
  def executeTrain(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == US)
      Advise::AirLift::AirStrike::Nil
    else
      Transport::Govern::Nil
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    val canPlaceExtraPolice = faction == US && capabilityInPlay(CombActionPlatoons_Unshaded)
    var placedExtraPolice   = false // only if CombActionPlatoons_Unshaded in play
    val maxTrainSpaces      = params.maxSpaces getOrElse 1000
    val maxPacifySpaces     = if (faction == US && capabilityInPlay(CORDS_Unshaded)) (2 min maxTrainSpaces) else 1
    val maxPacifyLevel      = if (faction == US && capabilityInPlay(CORDS_Shaded)) PassiveSupport else ActiveSupport
    var selectedSpaces      = List.empty[String]
    var arvnPlacedIn        = List.empty[String]
    var pacifySpaces        = List.empty[String]
    val isCandidate = (sp: Space) => {
      val valid = if (faction == ARVN) !sp.isLOC && !sp.nvaControlled
                  else                 !sp.isLOC && sp.pieces.has(USPieces)

      valid &&                                   // Valid for faction
      !sp.isNorthVietnam &&                      // Never North Vietnam
      params.spaceAllowed(sp.name) &&            // If event limits command to certain spaces
      !selectedSpaces.contains(sp.name) &&       // Not already selected
      !Special.selectedSpaces.contains(sp.name)  // Not selected for Advise/Govern Special Activity
    }

    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate,                      s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]"),
      noteIf(canPlaceExtraPolice,              s"May place 1 ARVN Police in 1 training space with US Troops [$CombActionPlatoons_Unshaded]"),
      noteIf(maxPacifySpaces == 2,             s"May pactify in 2 spaces [$CORDS_Unshaded]"),
      noteIf(maxPacifyLevel == PassiveSupport, s"May not Pacify to Active Support [$CORDS_Shaded]")
    ).flatten


    def promptToAddForces(name: String): Unit = {
      val sp = game.getSpace(name)
      val hasTheCash = params.free || (faction match {
        case ARVN => game.arvnResources >= 3
        case _    => (game.arvnResources - game.econ) >= 3  // US for Rangers

      })
      val canPlaceArvn = hasTheCash && (if (faction == US)
        sp.pieces.has(USBase)
      else
        sp.isCity || sp.pieces.has(CoinBases))
      val irregularsOK  = faction == US && game.piecesToPlace.has(Irregulars)
      val extraPoliceOK = canPlaceExtraPolice && !placedExtraPolice && sp.pieces.has(USTroops)
      val rangersOK     = canPlaceArvn && game.piecesToPlace.has(Rangers)
      val cubesOK       = canPlaceArvn && game.piecesToPlace.has(ARVNCubes)

      val choices = List(
        choice(extraPoliceOK, "extra",      s"Place extra Police [$CombActionPlatoons_Unshaded]"),
        choice(irregularsOK,  "irregulars", "Place Irregulars"),
        choice(rangersOK,     "rangers",    "Place Rangers"),
        choice(cubesOK,       "cubes",      "Place ARVN Troops/Police"),
        choice(true,          "finished",   "Do not place forces")
        ).flatten

      askMenu(choices, s"\nTraining in $name:").head match {
        case "extra" =>
          val toPlace = askPiecesToPlace(name, ARVNPolice::Nil, maxToPlace = 1)
          placePieces(name, toPlace)

        case "irregulars" =>
          val toPlace = askPiecesToPlace(name, Irregulars_U::Nil, maxToPlace = 2)
          placePieces(name, toPlace)

        case "rangers" =>
          val toPlace = askPiecesToPlace(name, Rangers_U::Nil, maxToPlace = 2)
          if (toPlace.total > 0) {
            log()
            if (!params.free)
              decreaseResources(ARVN, 3)
            arvnPlacedIn = name :: arvnPlacedIn
            placePieces(name, toPlace)
          }

        case "cubes" =>
          val toPlace = askPiecesToPlace(name, ARVNTroops::ARVNPolice::Nil, maxToPlace = 6)
          if (toPlace.total > 0) {
            log()
            if (!params.free)
              decreaseResources(ARVN, 3)
            arvnPlacedIn = name :: arvnPlacedIn
            placePieces(name, toPlace)
          }

        case _ =>
      }
    }
    
    def selectTrainSpace(): Unit = {
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect = candidates.nonEmpty && selectedSpaces.size < maxTrainSpaces
      val choices = List(
        choice(canSelect,       "select",   "Select a space to Train"),
        choice(Special.allowed, "special",  "Perform a Special Activity"),
        choice(true,            "finished", "Finished selecting spaces")
      ).flatten

      println(s"\nSpaces selected for Training")
      println(separator())
      wrap("", selectedSpaces) foreach println

      if (candidates.isEmpty) {
        val more = if (selectedSpaces.nonEmpty) " more" else ""
        println(s"\nThere are no${more} spaces eligible for Training")
      }

      askMenu(choices, "\nChoose one:").head match {
        case "select" =>
          askCandidateOrBlank("\nTrain in which space: ", candidates) foreach { name =>
            log(s"\n$faction selects $name for Training")
            promptToAddForces(name)
            selectedSpaces = selectedSpaces :+ name
          }
          selectTrainSpace()

        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectTrainSpace()

        case _ => // finished
      }
    }

    // Prompt for possible pacification, placing ARVN base or
    // transfer of ARVN resources to patronage.
    def promptFinalAction(): Unit = {

      val canPacify = (sp: Space) =>
        !(pacifySpaces contains sp.name) &&
        (sp.terror > 0 || sp.support < maxPacifyLevel) &&
        sp.coinControlled &&
        (faction == US || sp.pieces.has(ARVNTroops) && sp.pieces.has(ARVNPolice))

      val pacifyCandidates = if (pacifySpaces.size < maxPacifySpaces && game.arvnResources >= 3)
        spaces(selectedSpaces) filter canPacify map (_.name)
      else
        Nil

      val baseCandidates = if (pacifySpaces.isEmpty && game.piecesToPlace.has(ARVNBase)) {
        val canPlaceBase = (sp: Space) => sp.totalBases < 2 && sp.pieces.totalOf(ARVNCubes) >= 3
        faction match {
          case ARVN if params.free || game.arvnResources >= 3 => spaces(selectedSpaces) filter canPlaceBase map (_.name)
          case ARVN  => spaces(arvnPlacedIn) filter canPlaceBase map (_.name) // Spaces already paid for
          case _ => Nil  // US cannot place ARVN base
        }
      }
      else
        Nil

      val canXferPatronage = pacifySpaces.isEmpty && faction == US && selectedSpaces.contains(Saigon) && game.patronage > 0

      if (pacifyCandidates.nonEmpty ||
          baseCandidates.nonEmpty   ||
          canXferPatronage) {
        val pacifyMsg = if (pacifySpaces.isEmpty) "Pacify" else s"Pacify second space [$CORDS_Unshaded]"
        val choices = List(
           choice(pacifyCandidates.nonEmpty, "pacify",  pacifyMsg),
           choice(baseCandidates.nonEmpty,   "base",    "Place an ARVN base"),
           choice(canXferPatronage,          "xfer",    "Transfer patronage to ARVN resources"),
           choice(Special.allowed,           "special", "Perform a Special Activity"),
           choice(true,                      "none",    "Finished with Train operation")
        ).flatten

        askMenu(choices, "\nChoose final Train action:").head match {
          case "pacify" =>
            askCandidateOrBlank("\nPacify in which space: ", pacifyCandidates) foreach { name =>
            if (pacifySpace(name, faction))
              pacifySpaces = name :: pacifySpaces
            }
            
            if (pacifySpaces.size < maxPacifySpaces)
              promptFinalAction()

          case "base" =>
            loggingControlChanges {
              askCandidateOrBlank("\nPlace an ARVN base in which space: ", baseCandidates) match {
                case None       => promptFinalAction()
                case Some(name) =>
                  // askToPlace will ask the user to vountarily remove a base from the map
                  // if necessary.  If the user chooses not to, then it will return an
                  // empty Pieces instance.
                  val toPlace = askToPlaceBase(name, ARVNBase);
                  if (toPlace.isEmpty)
                    promptFinalAction()
                  else {
                    val sp    = game.getSpace(name)
                    val cubes = askPieces(sp.pieces, 3, ARVNCubes,  Some("Removing ARVN cubes to replace with base"))
                    removeToAvailable(name, cubes)
                    placePieces(name, toPlace)
                  }
              }
            }

          case "xfer" =>
            val amount = askInt(s"Transfer how much patronage to ARVN resources", 0, game.patronage min 3)
            if (amount > 0) {
              log()
              decreasePatronage(amount)
              increaseResources(ARVN, amount)
            }

          case "special" =>
            executeSpecialActivity(faction, params, availableActivities)
            promptFinalAction()

          case _ =>
        }
      }
    }


    log(s"\n$faction chooses Train operation")
    log(separator())
    if (notes.nonEmpty)
      notes foreach println

    selectTrainSpace()
    if (selectedSpaces.nonEmpty)
      promptFinalAction() // Pacify, Place base, Xfer Patronage to ARVN resources

    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }

  // Cap_M48Patton (shaded)
  //    After US/ARVN patrol NVA removes up to 2 cubes that moved
  //    (US to casualties)
  // Mo_BodyCount         = "#72 Body Count"               // Unshaded (affects asasult and patrol)
  //    Cost=0 AND +3 Aid per guerrilla removed
  
  def executePatrol(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == US)
      Advise::AirLift::AirStrike::Nil
    else
      Govern::Transport::Raid::Nil
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    val pattonUshaded = faction == US && capabilityInPlay(M48Patton_Unshaded)
    val pattonShaded  = capabilityInPlay(M48Patton_Shaded)
    val bodyCount     = momentumInPlay(Mo_BodyCount)
    val movedCubes  = new MovingGroups()  // Used to support M48 Patton (unshaded)
    val frozen      = new MovingGroups()  // Cubes that have moved into a space with Insurgent pieces
    var limOpDest: Option[String] = None
    val hasTheCash  = faction == US || params.free || bodyCount || game.arvnResources >= 3
    val PatrolCubes = if (faction == US) List(USTroops) else ARVNCubes
        
    // Faction cubes in the space that are not frozen in place
    val patrolCubes = (sp: Space) => sp.pieces.only(PatrolCubes) - frozen(sp.name)
    //  Spaces with movable cubes on or adjacent to LOCs/Cities
    //  If a limited Op, then the cubes must be able to reach the one selected destination.
    val isPatrolSource = (sp: Space) => {
      lazy val reachesLimOpDest = limOpDest map getPatrolDestinations(sp.name).contains getOrElse true
      val onNetwork = sp.isLOC || sp.isCity || getAdjacentLOCs(sp.name).nonEmpty || getAdjacentCities(sp.name).nonEmpty
      onNetwork && patrolCubes(sp).nonEmpty && reachesLimOpDest
    }

    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate,   s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]"),
      noteIf(bodyCount,     s"Cost is 0 and +3 Aid per guerrilla removed [Momentum: $Mo_BodyCount]"),
      noteIf(pattonUshaded, s"In follow up assault, remove 2 extra enemy pieces [$M48Patton_Unshaded]"),
      noteIf(pattonShaded,  s"After Patrol NVA removes up to 2 cubes that moved [$M48Patton_Shaded]")
    ).flatten

    def selectCubesToMove(): Unit = {
      val srcCandidates = spaceNames(game.spaces filter isPatrolSource)

      srcCandidates.nonEmpty
      val choices = List(
        choice(srcCandidates.nonEmpty,  "move",     "Move cubes"),
        choice(Special.allowed,         "special",  "Perform a Special Activity"),
        choice(true,                    "finished", "Finished moving cubes")
      ).flatten

      if (srcCandidates.isEmpty)
        println(s"\nThere are no spaces with cube eligible to move on patrol")

      askMenu(choices, "\nChoose one:").head match {
        case "move" =>
          askCandidateOrBlank("\nMove cubes out of which space: ", srcCandidates) foreach { src =>
            moveCubesFrom(src)
          }
          selectCubesToMove()
        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectCubesToMove()

        case _ => // finished
      }
    }    

    def moveCubesFrom(srcName: String): Unit = {
        val src            = game.getSpace(srcName)
        val destCandidates = getPatrolDestinations(srcName).sorted(LocLastOrdering)
        val eligible       = patrolCubes(src)

        println(s"\nMoving cubes out of $srcName")
        println(separator())
        wrap("These cubes can move   : ", eligible.descriptions) foreach println
        if (frozen(srcName).nonEmpty)
          wrap("These cubes cannot move: ", frozen(srcName).descriptions) foreach println
        
        val num = askInt(s"\nMove how many cubes out of $srcName", 0, eligible.total)
        if (num > 0) {
          val movers   = askPieces(eligible, num, PatrolCubes)
          val destinationName = limOpDest orElse {
            askCandidateOrBlank("\nSelect destination: ", destCandidates) map { name =>
              // remember dest if lim op
              if (params.limOpOnly)
                limOpDest = Some(name)
              name
            }
            
          }
          
          destinationName foreach { destName =>
            val dest   = game.getSpace(destName)
            movedCubes.remove(srcName, movers)
            movedCubes.add(destName, movers)
            if (dest.pieces.has(InsurgentPieces))
              frozen.add(destName, movers)
            movePieces(movers, srcName, destName)
          }
        }
    }
    
    def activateGuerrillasOnLOCs(): Unit = {
      case class Activation(sp: Space, num: Int)
      val underground = (sp: Space) => sp.pieces.totalOf(UndergroundGuerrillas)
      val cubes       = (sp: Space) => sp.pieces.totalOf(PatrolCubes)    
      val numbers     = game.locSpaces map (sp => (sp, cubes(sp) min underground(sp))) filterNot (_._2 == 0)
      
      log(s"\n$faction Patrol - Activating guerrillas on LOCs")
      log(separator())
      if (numbers.isEmpty)
        log("No guerrillas are activated")
      else {
        //  If a space has both NVA and VC underground guerrillas
        //  then ask user to pick the ones to activate.
        val activations = for ((sp, num) <- numbers) yield {
          val prompt     = s"\nChoose ${amountOf(num, "guerrilla")} to activate at: ${sp.name}"
          val guerrillas = askPieces(sp.pieces, num, UndergroundGuerrillas, Some(prompt))
          
          (sp, guerrillas)
        }
        
        for ((sp, guerrillas) <- activations)
          revealPieces(sp.name, guerrillas)
      }
    }
    
    //  If this is a limited Op, then the assault may only
    //  take place in the one selected destination.
    def assaultOneLOC(): Unit = {
      val canAssault = (sp: Space) => sp.pieces.has(PatrolCubes)
      val locs = if (params.limOpOnly)
        limOpDest.toList map game.getSpace filter canAssault
      else
        game.locSpaces filter canAssault
      val candidates = spaceNames(locs)
      if (candidates.nonEmpty || Special.allowed) {
        val choices = List(
          choice(candidates.nonEmpty, "assault",  "Assault at one LOC"),
          choice(Special.allowed,     "special",  "Perform a Special Activity"),
          choice(true,                "finished", "Do not Assault at one LOC")
        ).flatten
        
        askMenu(choices, "\nChoose one:").head match {
          case "assault" =>
            val assaultParams = Params(
              assaultRemovesTwoExtra = pattonUshaded,
              free                   = true)
            val name = askSimpleMenu(candidates, "\nAssault in which LOC:").head
            performAssault(name, faction, assaultParams)
          
          case "special" =>
            executeSpecialActivity(faction, params, availableActivities)
            assaultOneLOC()

          case _ => // finished
        }
      }
    }
    
              
    log(s"\n$faction chooses Patrol operation")
    log(separator())
    if (hasTheCash) {
      if (faction == ARVN && !params.free) {
        if (momentumInPlay(Mo_BodyCount))
          log(s"ARVN Assault costs zero resources [Momentum: $Mo_BodyCount]")
        else
          decreaseResources(ARVN, 3)
      }
      if (notes.nonEmpty) {
        println(separator())
        notes foreach println
      }
      
      selectCubesToMove()
      activateGuerrillasOnLOCs()
      assaultOneLOC()
      if (pattonShaded) {
        // TODO:
        // NVABot.removeEnemyPieces(2, ...)
        println()
        println(separator(char = '='))
        println(s"$M48Patton_Shaded for NVA Bot has not been implemented!")
        println(separator(char = '='))
      }
    }
    else
      log(s"There are not enough ARVN resources (${game.arvnResources}) to Patrol")

    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }


  //  Cobras_Unshaded            - 2 US/ARVN sweep spaces each remove 1 Active untunneled enemy
  //                              (troops then  guerrillas then bases)
  //  CombActionPlatoons_Shaded - US may select max 2 spaces per sweep
  //  BoobyTraps_Shaded         = Each sweep space, VC afterward removes 1 sweeping troop on
  //                              a roll 1-3 (US to casualties)
  def executeSweep(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == US)
      AirLift::AirStrike::Nil
    else
      Transport::Raid::Nil
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    var sweepSpaces     = Set.empty[String]
    var activatedSpaces = Set.empty[String]
    var cobrasSpaces    = Set.empty[String]
    val alreadyMoved    = new MovingGroups()
    val platoonsShaded  = faction == US && !params.limOpOnly && capabilityInPlay(CombActionPlatoons_Shaded)
    val cobrasUnshaded  = capabilityInPlay(Cobras_Unshaded)
    val boobyTraps      = capabilityInPlay(BoobyTraps_Shaded)
    val defaultMax      = if (platoonsShaded) 2 else 1000
    val maxSpaces       = params.maxSpaces getOrElse defaultMax
    
    if (params.maxSpaces.isEmpty && faction == US  && capabilityInPlay(CombActionPlatoons_Shaded))
      log(s"US can select a maximum of 2 spaces [$CombActionPlatoons_Shaded]")
      
    
    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate,    s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]"),
      noteIf(cobrasUnshaded, s"In 2 spaces, you may remove 1 active (untunneled) enemy [$Cobras_Unshaded]"),
      noteIf(platoonsShaded, s"US can select a maximum of 2 spaces [$CombActionPlatoons_Shaded]"),
      noteIf(boobyTraps,     s"Each space, VC afterward remove 1 troop on die roll 1-3 [$BoobyTraps_Shaded]")
    ).flatten

    // Ask the user if they wish to use the Cobras capability
    // in the space.
    def doCobras(name: String): Unit = {
      val Targets   = NVATroops::ActiveGuerrillas:::InsurgentNonTunnels
      val sp        = game.getSpace(name)
      val isPossible = sp.pieces.has(NVATroops::ActiveGuerrillas) ||
                       (!sp.pieces.has(UndergroundGuerrillas) && sp.pieces.has(InsurgentNonTunnels))
      
      if (isPossible && askYorN(s"Do you wish to use the Cobras capability in $name? (y/n) ")) {
        cobrasSpaces = cobrasSpaces + name
        log(s"\nUsing $Cobras_Unshaded")
        
        val deadPiece = if (sp.pieces.has(NVATroops))
          Pieces(nvaTroops = 1)
        else if (sp.pieces.has(ActiveGuerrillas))
          askPieces(sp.pieces, 1, ActiveGuerrillas)
        else
          askPieces(sp.pieces, 1, InsurgentNonTunnels)
        
        removeToAvailable(name, deadPiece)
      }
    }
    
    
    def moveCubesTo(destName: String): Unit = {
      val candidates = sweepSources(destName, faction, alreadyMoved)
      val choices = List(
        choice(candidates.nonEmpty, "move",      "Select a space from which to move troops"),
        choice(Special.allowed,     "special",   "Perform a Special Activity"),
        choice(true,                "finished", s"Finished moving troops to $destName")
      ).flatten

      log(s"\nMoving cubes into $destName")
      log(separator())
      askMenu(choices, "Choose one:").head match {
        case "move" =>
          val TroopType = if (faction == US) USTroops else ARVNTroops
          askCandidateOrBlank(s"\nMove $TroopType from which space: ", candidates) foreach { srcName =>
            val troops = game.getSpace(srcName).pieces.only(TroopType) - alreadyMoved(srcName)
            val num    = askInt(s"Move how many $TroopType", 0, troops.total)
          
            if (num > 0) {
              val movers = Pieces().set(num, TroopType)
              alreadyMoved.add(destName, movers)
              movePieces(movers, srcName, destName)
            }
          }
          moveCubesTo(destName)
          
        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          moveCubesTo(destName)

        case _ => // finished
      }
      
    }
    
    // Select provinces/cities (not N. Vietnam)
    def selectSweepSpace(): Unit = {
      val candidates = if (sweepSpaces.size < maxSpaces)
        spaceNames(game.nonLocSpaces filterNot (sp => sp.isNorthVietnam || sweepSpaces(sp.name)))
      else
        Nil

      val choices = List(
        choice(candidates.nonEmpty, "sweep",     "Select a Sweep space"),
        choice(Special.allowed,     "special",   "Perform a Special Activity"),
        choice(true,                "finished", s"Finished selecting Sweep spaces")
      ).flatten
      
      println(s"\nSweep spaces selected")
      println(separator())
      wrap("", sweepSpaces.toList) foreach println
      
      askMenu(choices, "\nChoose one:").head match {
        case "sweep" =>
          askCandidateOrBlank("\nSweep in which space: ", candidates) foreach { name =>
            sweepSpaces = sweepSpaces + name
            moveCubesTo(name)
            if (cobrasUnshaded && cobrasSpaces.size < 2)
              doCobras(name)
          }
          selectSweepSpace()
          
        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectSweepSpace()

        case _ => // finished
      }
    }
    
    def activateGuerrillasIn(name: String): Unit = {
      val sp = game.getSpace(name)
      val num = sp.sweepActivations(faction) min sp.pieces.totalOf(UndergroundGuerrillas)
      if (num > 0) {
        log(s"\nActivating guerrillas in $name")
        log(separator())
        val guerrillas = askPieces(sp.pieces, num, UndergroundGuerrillas)
        revealPieces(name, guerrillas)
      }
    }
    
    def activateGuerrillas(): Unit = {
      val canActivate = (name: String) => {
        val sp = game.getSpace(name)
        sp.sweepActivations(faction) > 0 && sp.pieces.has(UndergroundGuerrillas)
      }
      val candidates = ((sweepSpaces -- activatedSpaces) filter canActivate).toList.sorted
      if (candidates.nonEmpty) {
        val topChoices = List(
          choice(activatedSpaces.isEmpty,  "all",      "Activate guerrillas in all sweep spaces"),
          choice(activatedSpaces.nonEmpty, "rest",     "Activate guerrillas in the rest of the sweep spaces"),
          choice(Special.allowed,          "special",  "Perform a Special Activity")
        ).flatten
        val spaceChoices = candidates map (n => n -> s"Activate guerrillas in $n")
        val choices = topChoices ::: spaceChoices
        
        askMenu(choices, "\nChoose one:").head match {
          case "all" | "rest" =>
            for (name <- candidates) {
              activateGuerrillasIn(name)
              activatedSpaces = activatedSpaces + name
            }
              
          case "special" =>
            executeSpecialActivity(faction, params, availableActivities)
            activateGuerrillas()

          case name =>
            activateGuerrillasIn(name)
            activatedSpaces = activatedSpaces + name
            activateGuerrillas()
        }
      }
      else if (activatedSpaces.isEmpty)
        log(s"\nNo guerrillas can be activated in the ${amountOf(sweepSpaces.size, "sweep space")}")
    }
    
    //  In each sweep space roll a d6.
    //  On a 1-3, remove one of the sweeping factions troops.
    //  ARVN to available, US to casualties
    def resolveBoobyTraps(): Unit = {
      val troopType = if (faction == US) USTroops else ARVNTroops
      for (name <- sweepSpaces.toList.sorted) {
        val sp = game.getSpace(name)
        log(s"\nResolving $BoobyTraps_Shaded in $name")
        log(separator())
        if (sp.pieces.has(troopType)) {
          val die = d6
          val success = die < 4
          val status = if (success) "Success" else "Failure"
          log(s"Die roll: $die  [$status]")
          (die < 4) match {
            case true if faction == US => removeToCasualties(name, Pieces(usTroops = 1))
            case true                  => removeToAvailable(name, Pieces(nvaTroops = 1))
            case false                 => log("No troop is removed")
          }
        }
        else
          log(s"There is no $faction troop present")
      }
    }
    
    log(s"\n$faction chooses Sweep operation")
    log(separator())
    if (notes.nonEmpty)
      notes foreach println
    
    selectSweepSpace()
    if (sweepSpaces.nonEmpty) {
      activateGuerrillas()
      if (boobyTraps)
        resolveBoobyTraps()
    }
    
    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }


  //  Perform an assault in the given space.
  //  
  //  If US add an ARVN assault.
  def performAssault(name: String, faction: Faction, params: Params): Unit = {
    val remove1BaseFirst   = faction == US && capabilityInPlay(Abrams_Unshaded)
    val remove1Underground = faction == US && capabilityInPlay(SearchAndDestroy_Unshaded)
    val searchDestroy      = capabilityInPlay(SearchAndDestroy_Shaded)  // US and ARVN
    
    val sp          = game.getSpace(name)
    def pieces      = game.getSpace(name).pieces  // Always get fresh instance
    val baseFirst   = remove1BaseFirst && pieces.has(InsurgentNonTunnels)
    val underground = remove1Underground && pieces.has(UndergroundGuerrillas)
    val totalLosses = sp.assaultLosses(faction) + (if (params.assaultRemovesTwoExtra) 2 else 0)
    var remaining   = totalLosses

    // Log all control changes at the end of the assault
    loggingControlChanges {
      log(s"\n$faction assaults in $name")
      log(separator())
      
      if (faction == ARVN && !params.free) {
        if (momentumInPlay(Mo_BodyCount))
          log(s"\nARVN Assault costs zero resources [Momentum: $Mo_BodyCount]")
        else
          decreaseResources(ARVN, 3)
      }
      
      log(s"The assault inflicts ${amountOf(totalLosses, "hit")}")

      if (remaining > 0 && baseFirst) {
        val prompt = s"\nRemove a base first [$Abrams_Unshaded]"
        val removed = askPieces(pieces, 1, InsurgentNonTunnels, Some(prompt))
        removeToAvailable(name, removed)
        remaining -= removed.total
      }

      if (remaining > 0 && underground) {
        val prompt = s"\nRemove an underground guerrilla [$SearchAndDestroy_Unshaded]"
        val removed = askPieces(pieces, 1, UndergroundGuerrillas, Some(prompt))
        removeToAvailable(name, removed)
        if (momentumInPlay(Mo_BodyCount)) {
          log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $Mo_BodyCount]")
          increaseUsAid(3)
        }
        remaining -= removed.total
      }

      (remaining min pieces.numOf(NVATroops)) match {
        case 0 =>
        case num =>
          removeToAvailable(name, Pieces(nvaTroops = num))
          remaining -= num
      }

      (remaining min pieces.totalOf(ActiveGuerrillas)) match {
        case 0 =>
        case num =>
          val prompt = s"\nRemove active guerrillas"
          val removed = askPieces(pieces, num, ActiveGuerrillas, Some(prompt))
          removeToAvailable(name, removed)
          if (momentumInPlay(Mo_BodyCount)) {
            log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $Mo_BodyCount]")
            increaseUsAid(3 * removed.total)
          }
          remaining -= removed.total
      }

      if (!pieces.has(UndergroundGuerrillas)) {
        (remaining min pieces.totalOf(InsurgentNonTunnels)) match {
          case 0 =>
          case num =>
            val prompt = s"\nRemove untunneled bases"
            val removed = askPieces(pieces, num, InsurgentNonTunnels, Some(prompt))
            removeToAvailable(name, removed)
            increaseUsAid(removed.total * 6)
            remaining -= removed.total
        }

        if (remaining > 0 && pieces.has(InsurgentTunnels)) {
          val die = d6
          val success = die > 3
          log("\nNext piece to remove would be a tunneled base")
          log(s"Die roll is: ${die} [${if (success) "Tunnel destroyed!" else "No effect"}]")
          if (success) {
            val prompt = "\nRemove tunnel marker"
            val tunnel = askPieces(pieces, 1, InsurgentTunnels, Some(prompt))
            removeTunnelMarker(name, tunnel.explode().head)
          }
        }
      }
      
      if (remaining == totalLosses)
        log("\nNo insurgemnt pieces were removed in the assault")

      // Cobras_Shaded
      //    Eash US assault space, 1 US Troop to Casualties on die roll of 1-3
      if (faction == US && capabilityInPlay(Cobras_Shaded)) {
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
      if (searchDestroy && sp.isProvince && sp.population > 0 && sp.support != ActiveOpposition) {
        log(s"\nEach assault shifts support toward Active Opposition [$SearchAndDestroy_Shaded]")
        decreaseSupport(name, 1)
      }
    }
  }

  // Abrams_Unshaded
  //    1 US asault space may remove 1 non-Tunnel base first (not last)
  // Abrams_Shaded
  //    US may select max 2 spaces per Assault
  // M48Patton_Unshaded
  //  In two non-Lowland US assault spaces, remove 2 extra enemy pieces
  // Cobras_Shaded
  //    Eash US assault space, 1 US Troop to Casualties on die roll of 1-3
  //    (Does NOT apply to added ARVN assault)
  // SearchAndDestroy_Unshaded
  //    Eash US assault space may remove 1 underground guerrilla
  // SearchAndDestroy_Shaded
  //    Each US and ARVN assault Province shifts support one level toward Active Opposition
  // Mo_BodyCount
  //    Cost=0 AND +3 Aid per guerrilla removed
  // Mo_GeneralLansdale
  //    Assault prohibited
  //    This is handled in executeCoinOp()

  def executeAssault(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == US)
      AirLift::AirStrike::Nil
    else
      Transport::Raid::Nil
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    val bodyCount           = momentumInPlay(Mo_BodyCount)
    val abramsUnshaded      = faction == US && capabilityInPlay(Abrams_Unshaded)
    val abramsShaded        = faction == US && capabilityInPlay(Abrams_Shaded) && !params.limOpOnly
    val m48Patton           = faction == US && capabilityInPlay(M48Patton_Unshaded)
    val cobras              = faction == US && capabilityInPlay(Cobras_Shaded)
    val sdUnshaded          = faction == US && capabilityInPlay(SearchAndDestroy_Unshaded)
    val sdShaded            = faction == US && capabilityInPlay(SearchAndDestroy_Shaded)
    var assaultSpaces       = List.empty[String]
    var m48PattonSpaces     = List.empty[String]
    var addedARVNAssault    = false
    val isCandidate = (sp: Space) => {
      val cubes = if (faction == ARVN) ARVNCubes else List(USTroops)

      params.spaceAllowed(sp.name) &&           // If event limits command to certain spaces
      !assaultSpaces.contains(sp.name) &&       // Not already selected
      sp.pieces.has(InsurgentPieces) &&
      sp.pieces.has(cubes)
    }

    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate,    s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]"),
      noteIf(abramsUnshaded, s"In 1 space you may remove 1 (untunneled) base first [$Abrams_Unshaded]"),
      noteIf(abramsShaded,   s"Select a maximum of 2 spaces [$Abrams_Shaded]"),
      noteIf(m48Patton,      s"In 2 non-Lowland spaces remove 2 extra enemy pieces [$M48Patton_Unshaded]"),
      noteIf(cobras,         s"Each space, remove 1 troop on die roll 1-3 [$Cobras_Shaded]"),
      noteIf(sdUnshaded,     s"Each space, may remove 1 underground guerrilla [$SearchAndDestroy_Unshaded]"),
      noteIf(sdShaded,       s"Each province, shift support toward Active Opposition [$SearchAndDestroy_Shaded]"),
      noteIf(bodyCount,      s"Cost is 0 and +3 Aid per guerrilla removed [Momentum: $Mo_BodyCount]")
    ).flatten

    def selectAssaultSpace(): Unit = {

      val limitOK    = params.maxSpaces map (n => assaultSpaces.size < n) getOrElse true
      val abramsOK   = abramsShaded == false || assaultSpaces.size < 2
      val hasTheCash = faction == US || params.free || bodyCount || game.arvnResources >= 3
      val candidates = if (limitOK && abramsOK && hasTheCash)
        spaceNames(game.spaces filter isCandidate)
      else
        Nil

      val canSelect = candidates.nonEmpty && (params.maxSpaces map (x => assaultSpaces.size < x) getOrElse true)
      val choices = List(
        choice(canSelect,       "select",   "Select a space to Assault"),
        choice(Special.allowed, "special",  "Perform a Special Activity"),
        choice(true,            "finished", "Finished selecting spaces")
      ).flatten

      println(s"\nSpaces Assaulted")
      println(separator())
      wrap("", assaultSpaces) foreach println

      if (candidates.isEmpty) {
        val more = if (assaultSpaces.nonEmpty) " more" else ""
        if (faction == ARVN && !hasTheCash)
          println(s"\nThere are not enough ARVN resources (${game.arvnResources}) to conduct an Assault")
        else
          println(s"\nThere are no${more} spaces eligible for Assault")
      }

      askMenu(choices, "\nChoose one:").head match {
        case "select" =>
          askCandidateOrBlank("\nAssault in which space: ", candidates) foreach { name =>
            val assaultParams = if (m48Patton &&
                                    m48PattonSpaces.size < 2 &&
                                    !game.getSpace(name).isLowland &&
                                    askYorN(s"Remove 2 extra pieces in this Assault [$M48Patton_Unshaded]? (y/n) ")) {
              m48PattonSpaces = name :: m48PattonSpaces
              params.copy(assaultRemovesTwoExtra = true)
            }
            else
              params
            performAssault(name, faction, assaultParams)
          
            val canFollowup = faction == US &&
                              addedARVNAssault == false &&
                              game.getSpace(name).pieces.hasExposedInsurgents &&
                              game.getSpace(name).pieces.has(ARVNCubes) &&
                              (bodyCount || game.arvnResources >= 3)
                            
            if (canFollowup && askYorN(s"Follow up with ARVN assault in $name? (y/n) ")) {
              log(s"\nUS adds a follow up ARVN asault in $name")
              performAssault(name, ARVN, params)
            }
            assaultSpaces = assaultSpaces :+ name
          }
          selectAssaultSpace()

        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectAssaultSpace()

        case _ => // finished
      }
    }

    log(s"\n$faction chooses Assault operation")
    log(separator())
    if (notes.nonEmpty)
      notes foreach println

    selectAssaultSpace()

    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }

  // ====================================================================
  // == Insurgent Operations ===========================================
  // ====================================================================

  // AAA_Unshaded    - Rally that Improves Trail may select 1 space only
  // SA2s_Shaded     - NVA Rally improves Trail 2 boxes instead of 1
  // Cadres_Shaded   - VC Rally in 1 space that already had a base may Agitage (even if COIN control)
  // Mo_McNamaraLine - prohibits trail improvement by rally
  def executeRally(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == NVA)
      Infiltrate::Bombard::Nil
    else
      Tax::Subvert::Nil
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    val mcnamara    = faction == NVA && momentumInPlay(Mo_McNamaraLine)
    val sa2s        = faction == NVA && !mcnamara && capabilityInPlay(SA2s_Shaded)
    val aaa         = faction == NVA && !mcnamara && !params.limOpOnly && capabilityInPlay(AAA_Unshaded)
    val cadres      = faction == VC  && capabilityInPlay(Cadres_Shaded)
    var rallySpaces = List.empty[String]
    var didCadres   = false
    
    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate, s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]"),
      noteIf(mcnamara,    s"Trail improvemnet is prohibited [Momentum: $Mo_McNamaraLine]"),
      noteIf(sa2s,        s"Trail improvement is 2 boxes instead of 1 [$SA2s_Shaded]"),
      noteIf(aaa,         s"Rally that improves the Trail may select 1 space only [$AAA_Unshaded]"),
      noteIf(cadres,      s"May Agitate in one space with an existing base [$Cadres_Shaded]")
    ).flatten


    def promptToAddPieces(name: String): Unit = {
      val sp            = game.getSpace(name)
      val guerrillas    = if (faction == NVA) NVAGuerrillas else VCGuerrillas
      val underground   = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
      val active        = if (faction == NVA) NVAGuerrillas_A else VCGuerrillas_A
      val base          = if (faction == NVA) NVABase else VCBase
      val canPlaceBase  = sp.totalBases < 2 && sp.pieces.totalOf(guerrillas) >= 2
      val canHide       = faction == VC && sp.pieces.hasBase(VC) && sp.pieces.has(VCGuerrillas_A)
      val maxGuerrillas = if (sp.pieces.hasBase(faction))
        faction match {
          case NVA => sp.pieces.totalNVABases + game.trail 
          case _   => sp.pieces.totalVCBases + sp.population
        }
        
      else
        1

      val noneMsg = if (canHide)
        "Do not place or flip any pieces"
      else
        "Do not place any pieces"
      val choices = List(
        choice(true,              "place-one",   "Place a single guerrilla"),
        choice(maxGuerrillas > 1, "place-many", s"Place up to $maxGuerrillas guerrillas at base"),
        choice(canPlaceBase,      "place-base", s"Replace two guerrillas with a base"),
        choice(canHide,           "hide",        "Flip all active guerrillas underground"),
        choice(true,              "finished",    noneMsg)
        ).flatten

      askMenu(choices, s"\nRallying in $name:").head match {
        case "place-one" =>
          val toPlace = askPiecesToPlace(name, underground::Nil, maxToPlace = 1)
          placePieces(name, toPlace)

        case "place-many" =>
          val toPlace = askPiecesToPlace(name, underground::Nil, maxToPlace = maxGuerrillas)
          placePieces(name, toPlace)

        case "place-base" =>
          val toRemove = askPieces(sp.pieces, 2, guerrillas,  Some("Removing two guerrillas"))
          val toPlace = askToPlaceBase(name, base)
          loggingControlChanges {
            removeToAvailable(name, toRemove)
            placePieces(name, toPlace)
          }

        case "hide" =>
          val toHide = sp.pieces.only(active)
          hidePieces(name, toHide)

        case _ =>
      }
    }
    
    def selectRallySpace(): Unit = {
      val candidates = spaceNames(game.nonLocSpaces filter (sp => sp.support <= Neutral && !rallySpaces.contains(sp.name)))
      val hasTheCash = params.free || game.resources(faction) > 0
      val atMax      = params.maxSpaces map (_ >= rallySpaces.size) getOrElse false
      val canSelect  = !atMax && candidates.nonEmpty && hasTheCash
      
      val choices = List(
        choice(canSelect,       "select",  s"Select a rally space"),
        choice(Special.allowed, "special",  "Perform a Special Activity"),
        choice(true,            "finished", "Finished selecting spaces")
      ).flatten
      
      println(s"\nSpaces selected for Rally")
      println(separator())
      wrap("", rallySpaces) foreach println
        
      askMenu(choices, "\nChoose one:").head match {
        case "select" =>
          askCandidateOrBlank("\nRally in which space: ", candidates) foreach { name =>
            //  Note: COIN control does not prevent agitation here
            val canAgitate = cadres && game.resources(VC) > 0 && ({
              val sp = game.getSpace(name)
              sp.pieces.hasBase(VC) && (sp.support != ActiveOpposition || sp.terror > 0)
            })
                
            log(s"\n$faction selects $name for Rally")
            if (!params.free)
              decreaseResources(faction, 1)
            promptToAddPieces(name)
            rallySpaces = rallySpaces :+ name
          
            if (canAgitate && askYorN(s"Do you wish to Agitate in $name? (y/n) ") && agitateSpace(name))
              didCadres = true
          }
          selectRallySpace()
        
        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectRallySpace()
        
        case _ =>
      }
    }

    log(s"\n$faction chooses Rally operation")
    log(separator())
    if (notes.nonEmpty)
      notes foreach println
    
    selectRallySpace()
    
    if (faction == NVA && game.resources(NVA) >= 2 && game.trail < TrailMax &&
         !mcnamara && (!aaa || rallySpaces.size == 1)) {
      val num = if (sa2s) 2 else 1

      if (askYorN(s"Do you wish to spend 2 resources to improve the trail by ${amountOf(num, "box", Some("boxes"))}? (y/n) ")) {
        log("\nNVA improves the trail")
        log(separator())
        decreaseResources(NVA, 2)
        improveTrail(num)
      }
    }
      
    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }

  def executeMarch(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == NVA)
      Infiltrate::Bombard::Ambush::Nil
    else
      Tax::Subvert::Ambush::Nil
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    
    val moveableTypes   = if (faction == NVA) NVATroops::NVAGuerrillas else VCGuerrillas
    val maxDestinations = params.maxSpaces getOrElse 1000
    var destinations    = Map.empty[String, Boolean]  // True if space has been paid for
    val alreadyMoved    = new MovingGroups()
    
    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate, s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]")
    ).flatten
    
    def moveablePieces(name: String) = game.getSpace(name).pieces.only(moveableTypes) - alreadyMoved(name)

    def moveToDestination(destName: String): Unit = {
      val srcCandidates = getAdjacent(destName).toList.sorted filter (moveablePieces(_).total > 0)
      if (srcCandidates.isEmpty)
        println(s"\nThere are no pieces that can reach $destName")
      else {
        val choices = (srcCandidates map (name => name -> name)) :+ ("none" -> "Do not move any pieces")

        val srcName = askMenu(choices, "Move pieces from:").head
        if (srcName != "none") {
          val src         = game.getSpace(srcName)
          val dest        = game.getSpace(destName)
          val canContinue = !params.limOpOnly && faction == NVA && game.trail > TrailMin && isInLaosCambodia(destName)
          val trailMove   = faction == NVA && game.trail == TrailMax && (isInLaosCambodia(srcName) || isInLaosCambodia(destName))
          val free        = params.free || dest.isLOC || trailMove || destinations(destName)
          val moveable    = src.pieces.only(moveableTypes)
          val coinForces  = dest.pieces.totalOf(CoinForces)
          
          if (free || game.resources(faction) > 0) {
            if (!free) {
              log(s"\n$faction pays to move pieces into $destName")
              log(separator())
              decreaseResources(faction, 1)
            }
            
            val num      = askInt(s"Move how many pieces from $srcName", 1, moveable.total)
            val movers   = askPieces(moveable, num)
            val revealed = if ((dest.isLOC || dest.support > Neutral) && (coinForces + movers.total) > 3)
              movers.only(UndergroundGuerrillas)
            else
              Pieces()
            
            movePieces(movers, srcName, destName)
            revealPieces(destName, revealed)
            if (canContinue) {
              if (movers.total == 1)
                println("This piece can continue moving [Trail > 0]")
              else
                println("These pieces can continue moving [Trail > 0]")
            }
            else
              alreadyMoved.add(destName, movers)
          }
          else
            println(s"\n$faction does not have a resource to pay for the move into $destName")
        }
      }
    }

    def nextMarchAction(): Unit = {
      val candidates = spaceNames(game.spaces filter (sp => !destinations.contains(sp.name)))
      val canSelect  = candidates.nonEmpty && destinations.size < maxDestinations
      val moveCandidates = destinations.keys.toList.sorted filter { destName =>
        (getAdjacent(destName) exists (moveablePieces(_).total > 0))
      }
      
      val topChoices = List(
        choice(canSelect,       "select",  s"Add a march destination space"),
        choice(Special.allowed, "special",  "Perform a Special Activity")
      ).flatten
      val moveChoices = moveCandidates map (name => name -> s"Move pieces into $name")
      val lastChoice = List("finished" -> "Finished with March operation")
              
      askMenu(topChoices:::moveChoices:::lastChoice, "\nChoose one:").head match {
        case "select" =>
          askCandidateOrBlank("\nAdd which space as a march destination: ", candidates) foreach { name =>
            destinations = destinations + (name -> false)
            log(s"\n$faction selects $name as a March destination")
          }
          nextMarchAction()
        
        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          nextMarchAction()
        
        case "finished" =>
        
        case destName =>
          moveToDestination(destName)
          nextMarchAction()
      }
    }

    log(s"\n$faction chooses March operation")
    log(separator())
    if (notes.nonEmpty)
      notes foreach println
    
    nextMarchAction()
          
    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }

  def executeAttack(faction: Faction, params: Params): Unit = {
  }

  // Cadres_Unshaded - VC terror must remove two guerrillas per space
  def executeTerror(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == NVA)
      Bombard::Nil
    else
      Tax::Subvert::Nil
    val cadres              = faction == VC && capabilityInPlay(Cadres_Unshaded)
    val availableActivities = typhoonKateFilter(specialActivities)
    val prohibitedActivity  = (specialActivities filterNot availableActivities.contains).headOption
    var selectedSpaces      = List.empty[String]
    val maxSpaces           = params.maxSpaces getOrElse 1000
    val underground         = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
    
    def canSpecial = Special.allowed && availableActivities.nonEmpty
     
    inspect("specialActivities", specialActivities)
    inspect("availableActivities", availableActivities) 
    inspect("prohibitedActivity", prohibitedActivity)
    val typhoonKate = Special.allowed && prohibitedActivity.nonEmpty
    val notes = List(
      noteIf(typhoonKate, s"${prohibitedActivity.getOrElse("")} speical activity prohibited [Momentum: $Mo_TyphoonKate]"),
      noteIf(cadres,      s"$faction must remove 2 guerrillas in each terror space [$Cadres_Unshaded]")
    ).flatten
    
    
    // terrorMarkersAvailable
    def nextTerrorAction(): Unit = {
      val hasTheCash   = params.free || game.resources(faction) > 0
      var canTerrorize = (sp: Space) => {
        val canPay   = sp.isLOC || hasTheCash
        val hasPiece = faction match {
          case VC if cadres => sp.pieces.has(VCGuerrillas_U) && sp.pieces.totalOf(VCGuerrillas) > 1
          case VC           => sp.pieces.has(VCGuerrillas_U)
          case _            => sp.pieces.has(NVAGuerrillas_U) || sp.pieces.has(NVATroops)
        }
        !selectedSpaces.contains(sp.name) && canPay && hasPiece
      }
      val candidates = spaceNames(game.spaces filter canTerrorize)
      val canSelect  = candidates.nonEmpty && selectedSpaces.size < maxSpaces
      val choices = List(
        choice(canSelect,  "select",   "Select a space to Terrorize"),
        choice(canSpecial, "special",  "Perform a Special Activity"),
        choice(true,       "finished", "Finished selecting spaces")
      ).flatten

      println(s"\nSpaces selected for Terror")
      println(separator())
      wrap("", selectedSpaces) foreach println

      if (candidates.isEmpty) {
        val more = if (selectedSpaces.nonEmpty) " more" else ""
        println(s"\nThere are no${more} spaces eligible for a Terror operation")
      }

      askMenu(choices, "\nChoose one:").head match {
        case "select" =>
          askCandidateOrBlank("\nTerrorize which space: ", candidates) foreach { name =>
            def sp = game.getSpace(name)
            // NVA may terror with only troops, in which case no guerrilla is revealed
            val toReveal = if (sp.pieces.has(underground)) Pieces().set(1, underground) else Pieces()
          
            log(s"\n$faction selects $name for Terror")
            log(separator())
            if (!params.free && !sp.isLOC)
              decreaseResources(faction, 1)
            revealPieces(name, toReveal)
            if (cadres) {
              val toRemove = askPieces(sp.pieces, 2, VCGuerrillas, Some(s"Remove guerrillas for [$Cadres_Unshaded]"))
              removeToAvailable(name, toRemove)
            }
          
            if (sp.terror == 0)
              addTerror(name, 1) // Terror/Sabotage marker
          
            faction match {
              case NVA if !sp.isLOC && sp.support > Neutral           => decreaseSupport(name, 1)
              case NVA if !sp.isLOC && sp.support < Neutral           => increaseSupport(name, 1)
              case VC  if !sp.isLOC && sp.support != ActiveOpposition => decreaseSupport(name, 1)
              case _ => // No effect
            }
            selectedSpaces = selectedSpaces :+ name
          }
          nextTerrorAction()

        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          nextTerrorAction()

        case _ => // finished
      }
        
    }
    
    
    log(s"\n$faction chooses Terror operation")
    log(separator())
    if (notes.nonEmpty)
      notes foreach println
    
    nextTerrorAction()
          
    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
    
  }
}
