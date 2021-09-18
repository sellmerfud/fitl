
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
    includeSpecial: Boolean = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
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

  // Use during a turn to keep track of pieces that have already moved
  // in each space.
  object MovingGroups {
    var groups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())

    def init(): Unit = { groups = Map.empty.withDefaultValue(Pieces()) }
    def apply(name: String): Pieces = groups(name)
    def add(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) + pieces)
    def remove(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) - pieces)

    def toList = groups.toList.sortBy(_._1)
    def size = groups.size
  }

  def noteIf(cond: Boolean, note: String): Option[String] = if (cond) Some(note) else None

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
        case OpPlusSpecial => executeCmd(faction, Params(includeSpecial = true))
        case OpOnly        => executeCmd(faction)
        case LimitedOp     => executeCmd(faction, Params(maxSpaces = Some(1)))
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

  def executeCmd(faction: Faction, params: Params = Params()): Unit = {
    Special.init(params)
    MovingGroups.init()
    trainingSpaces = Set.empty

    faction match {
      case US  | ARVN => executeCoinCmd(faction, params)
      case NVA | VC   => executeInsurgentCmd(faction, params)
    }
  }

  def executeCoinCmd(faction: Faction, params: Params): Unit = {

    val landsdale = faction == US && momentumInPlay(Mo_GeneralLansdale)
    val notes = List(
      noteIf(landsdale, s"US Assault is prohibited [Momentum: ${Mo_GeneralLansdale}]")
    ).flatten

    val availOps = CoinOp.ALL filter {
      case Train => true
      case Patrol     => true
      case Sweep      => true
      case Assault    => !landsdale
    }
    val choices = availOps map (op => op -> op.toString)

    if (notes.nonEmpty) {
      println()
      notes foreach println
    }

    askMenu(choices, "\nChoose operation:").head match {
      case Train   => executeTrain(faction, params)
      case Patrol  => executePatrol(faction, params)
      case Sweep   => executeSweep(faction, params)
      case Assault => executeAssault(faction, params)
    }
  }

  def executeInsurgentCmd(faction: Faction, params: Params): Unit = {
    val choices: List[(Option[InsurgentOp], String)] =
      (InsurgentOp.ALL map (op => Some(op) -> op.toString)) :+ (None -> "Abort current action")

    val op = askMenu(choices, "\nChoose one:").head getOrElse { throw AbortAction }

    op match {
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
    val availableActivities = if (faction == US)
      Advise::AirLift::AirStrike::Nil
    else
      Transport::Govern::Nil
    val canPlaceExtraPolice = faction == US && capabilityInPlay(CombActionPlatoons_Unshaded)
    var placedExtraPolice   = false // only if CombActionPlatoons_Unshaded in play
    val maxPacifySpaces     = if (faction == US && capabilityInPlay(CORDS_Unshaded)) 2 else 1
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


    def selectTrainSpaces(): Unit = {

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

      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect = candidates.nonEmpty && (params.maxSpaces map (x => selectedSpaces.size < x) getOrElse true)
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
          val name = askCandidate("\nTrain in which space: ", candidates)
          promptToAddForces(name)

          selectedSpaces = selectedSpaces :+ name
          selectTrainSpaces()

        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectTrainSpaces()

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

      def pacifySpace(): Unit = {
        val name        = askCandidate("\nPacify in which space: ", pacifyCandidates)
        val sp          = game.getSpace(name)
        val maxShift    = ((maxPacifyLevel.value - sp.support.value) max 0) min 2
        val maxInSpace  = maxShift + sp.terror
        val maxPossible = maxInSpace min (game.arvnResources / 3)

        val choices = List.range(maxPossible, -1, -1) map {
          case 0                    => (0 -> s"Do not pacify in $name")
          case n if sp.terror == 0  => (n -> s"Shift ${amountOf(n, "level")} to ${SupportType(sp.support.value + n)}")
          case n if n <= sp.terror  => (n -> s"Remove ${amountOf(n, "terror marker")}")
          case n                    => (n -> s"Remove ${amountOf(sp.terror, "terror marker")} and shift ${amountOf(n - sp.terror, "level")} to ${SupportType(sp.support.value + n - sp.terror)}")
        }
        val p = if (maxPacifyLevel == PassiveSupport)
          s"\nYou cannot shift to Active Suport [$CORDS_Shaded]\nChoose one:"
        else
          "\nChoose one:"
        printSummary(spaceSummary(name))
        val num = askMenu(choices, p, allowAbort = false).head
        if (num > 0) {
          val shift = (num - sp.terror) max 0
          log()
          decreaseResources(ARVN, num * 3)
          removeTerror(name, num min sp.terror)
          increaseSupport(name, shift)
          pacifySpaces = name :: pacifySpaces
        }
      }

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
            pacifySpace()
            if (pacifySpaces.size < maxPacifySpaces)
            promptFinalAction()

          case "base" =>
            loggingControlChanges {
              val name  = askCandidate("\nPlace an ARVN base in which space: ", baseCandidates)
              // askToPlace will ask the user to vountarily remove a base from the map
              // if necessary.  If the user chooses not to, then it will return an
              // empty Pieces instance.
              val toPlace = askToPlaceBase(name, ARVNBase);
              if (toPlace.nonEmpty) {
                val sp    = game.getSpace(name)
                val cubes = askPieces(sp.pieces, 3, ARVNCubes,  Some("Removing ARVN cubes to replace with base"))
                removeToAvailable(name, cubes)
                placePieces(name, toPlace)
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


    val multiSpaces = params.maxSpaces map (_ > 1) getOrElse true
    val notes = List(
      noteIf(canPlaceExtraPolice, s"Capability [$CombActionPlatoons_Unshaded]: Place 1 extra ARVN Police in one space with US Troops"),
      noteIf(multiSpaces && maxPacifySpaces == 2, s"Capability [$CORDS_Unshaded]: May pacify in up to two spaces"),
      noteIf(maxPacifyLevel == PassiveSupport, s"Capability [$CORDS_Shaded]: May only pacify up to Passive Support")
    ).flatten

    log(s"\n$faction chooses Train operation")
    log(separator())
    notes foreach println

    selectTrainSpaces()
    if (selectedSpaces.nonEmpty)
      promptFinalAction() // Pacify, Place base, Xfer Patronage to ARVN resources

    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)
  }

  // Cap_M48Patton (shaded)
  //    After US/ARVN patrol NVA removes up to 2 cubes that moved
  //    (US to casualties)
  def executePatrol(faction: Faction, params: Params): Unit = {

  }

  def executeSweep(faction: Faction, params: Params): Unit = {
  }

  // Abrams_Unshaded
  //    1 US asault space may remove 1 non-Tunnel base first (not last)
  // Abrams_Shaded
  //    US may select max 2 spaces per Assault
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
  //    This is handled in executeCoinCmd()

  def executeAssault(faction: Faction, params: Params): Unit = {
    val availableActivities = if (faction == US)
      AirLift::AirStrike::Nil
    else
      Transport::Raid::Nil
    val remove1BaseFirst   = faction == US && capabilityInPlay(Abrams_Unshaded)
    val maxTwoSpaces       = faction == US && capabilityInPlay(Abrams_Shaded)
    val shadedCobras       = faction == US && capabilityInPlay(Cobras_Shaded)
    val remove1Underground = faction == US && capabilityInPlay(SearchAndDestroy_Unshaded)
    val searchDestroy      = capabilityInPlay(SearchAndDestroy_Shaded)  // US and ARVN
    val bodyCount          = momentumInPlay(Mo_BodyCount)
    var assaultSpaces      = List.empty[String]
    var addedARVNAssault   = false
    val isCandidate = (sp: Space) => {
      val cubes = if (faction == ARVN) ARVNCubes else List(USTroops)

      params.spaceAllowed(sp.name) &&           // If event limits command to certain spaces
      !assaultSpaces.contains(sp.name) &&       // Not already selected
      sp.pieces.has(InsurgentPieces) &&
      sp.pieces.has(cubes)
    }

    //  assaultFaction may be different from faction performing the operation
    //  If US add an ARVN assault.
    def performAssault(name: String, assaultFaction: Faction): Unit = {
      val sp          = game.getSpace(name)
      def pieces      = game.getSpace(name).pieces  // Always get fresh instance
      val baseFirst   = remove1BaseFirst && pieces.has(InsurgentNonTunnels)
      val underground = remove1Underground && pieces.has(UndergroundGuerrillas)
      val totalLosses = sp.assaultLosses(assaultFaction)
      var remaining   = totalLosses

      // Log all control changes at the end of the assault
      loggingControlChanges {
        log(s"\n$faction assaults in $name")
        log(separator())
        
        if (assaultFaction == ARVN && !params.free) {
          if (bodyCount)
            log(s"\nARVN Assault costs zero resources [Momentum: $bodyCount]")
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
          if (bodyCount) {
            log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $bodyCount]")
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
            if (bodyCount) {
              log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $bodyCount]")
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
        if (assaultFaction == US && capabilityInPlay(Cobras_Shaded)) {
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

    def selectAssaultSpaces(): Unit = {

      val limitOK    = params.maxSpaces map (n => assaultSpaces.size < n) getOrElse true
      val abramsOK   = maxTwoSpaces == false || assaultSpaces.size < 2
      val hasTheCash = faction == US || bodyCount || game.arvnResources >= 3
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
        println(s"\nThere are no${more} spaces eligible for Assault")
      }

      askMenu(choices, "\nChoose one:").head match {
        case "select" =>
          val name = askCandidate("\nAssault in which space: ", candidates)
          performAssault(name, faction)
          val canFollowup = faction == US &&
                            addedARVNAssault == false &&
                            game.getSpace(name).pieces.hasExposedInsurgents &&
                            game.getSpace(name).pieces.has(ARVNCubes) &&
                            (bodyCount || game.arvnResources >= 3)
          if (canFollowup && askYorN(s"Follow up with ARVN assault in $name? (y/n) ")) {
            log(s"\nUS adds a follow up ARVN asault in $name")
            performAssault(name, ARVN)
          }
          assaultSpaces = assaultSpaces :+ name
          selectAssaultSpaces()

        case "special" =>
          executeSpecialActivity(faction, params, availableActivities)
          selectAssaultSpaces()

        case _ => // finished
      }
    }

    val moreThanTwo = params.maxSpaces map (_ > 2) getOrElse true
    val notes = List(
      noteIf(remove1BaseFirst,    s"Capability [$Abrams_Unshaded]: May remove 1 untunneled base first in each space"),
      noteIf(moreThanTwo && shadedCobras , s"Capability [$Cobras_Shaded]: May assault in up to only 2 spaces"),
      noteIf(remove1Underground, s"Capability [$SearchAndDestroy_Unshaded]: May remove 1 underground guerrilla"),
      noteIf(searchDestroy, s"Capability [$SearchAndDestroy_Shaded]: Each assault shifts toward Active Opposition"),
      noteIf(bodyCount, s"Momentum [$Mo_BodyCount]: +3 Aid per guerrilla removed, ARVN assault is free")
    ).flatten

    log(s"\n$faction chooses Assault operation")
    log(separator())
    notes foreach println

    selectAssaultSpaces()



    //  Last chance to perform special activity
    if (Special.allowed && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, availableActivities)

  }

  // ====================================================================
  // == Insurgent Operations ===========================================
  // ====================================================================

  def executeRally(faction: Faction, params: Params): Unit = {
  }

  def executeMarch(faction: Faction, params: Params): Unit = {
  }

  def executeAttack(faction: Faction, params: Params): Unit = {
  }

  def executeTerror(faction: Faction, params: Params): Unit = {
  }

}
