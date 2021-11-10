
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

  private val NO_LIMIT = 1000;          // When space selection is unlimited.
  private var pt76_shaded_used = false  // NVA attack in one space

  def logOpChoice(faction: Faction, op: Operation, notes: TraversableOnce[String] = Nil): Unit = {
    log(s"\n$faction chooses $op operation")
    log(separator(char = '='))
    for (note <- notes)
      log(note)
  }

  def logSAChoice(faction: Faction, sa: SpecialActivity, notes: TraversableOnce[String] = Nil): Unit = {
    log(s"\n$faction chooses $sa special activity")
    log(separator(char = '='))
    for (note <- notes)
      log(note)
  }

  // Aid in keeping track of when a special activity can be taken
  object Special {
    private var allowSpecial = false
    private var specialTaken = false

    var ambushing = false
    var usedMainForceBns = false   // VC may remove 2 pieces in one space
    var trainingSpaces: Set[String] = Set.empty  // Spaces where Advise/Govern cannot be done (Training)

    var selectedSpaces = Set.empty[String]  // Spaces selected for the Special Activity
    var transportDestinations = Set.empty[String]

    def init(params: Params): Unit = {
      allowSpecial     = params.specialActivity
      specialTaken     = false
      ambushing        = false
      usedMainForceBns = false
      selectedSpaces   = Set.empty  // For Advise/Govern activities
      trainingSpaces   = Set.empty
      transportDestinations = Set.empty
    }

    def allowed = allowSpecial && !specialTaken
    def taken   = specialTaken

    def maxAmbushSpaces = if (capabilityInPlay(BoobyTraps_Unshaded)) 1 else 2
    def canAmbush       = ambushing && selectedSpaces.size < maxAmbushSpaces

    def completed() = specialTaken = true
    def cancelled() = specialTaken = false
  }

  //  Kill exposed insurgent pieces up to the max allowd.
  //  NVA Troops first
  //  Then active guerrillas (prompt user for which to remove)
  //  Then if no troops/guerrillas remain bases can be removed
  //  If `canRevealTunnel` is true, and no other Insurgent pieces
  //  remain, then the tunnel marker is removed.
  //  Return the number of pieces removed.
  def killExposedInsurgents(name: String, maxRemoval: Int, canRevealTunnel: Boolean, vulnerableTunnels: Boolean = false): Pieces = {
    val baseTargets = if (vulnerableTunnels) InsurgentBases else InsurgentNonTunnels
    loggingControlChanges {
      var removed    = Pieces()
      def pieces     = game.getSpace(name).pieces
      def remaining  = maxRemoval - removed.total

      (remaining min pieces.totalOf(NVATroops)) match {
        case 0 =>
        case num =>
          val toRemove = Pieces(nvaTroops = num)
          removeToAvailable(name, toRemove)
          removed = removed + toRemove
      }

      (remaining min pieces.totalOf(ActiveGuerrillas)) match {
        case 0 =>
        case num =>
          val prompt = s"\nRemove active guerrillas"
          val toRemove = askPieces(pieces, num, ActiveGuerrillas, Some(prompt))
          removeToAvailable(name, toRemove)
          removed = removed + toRemove
      }

      if (!pieces.has(NVATroops::Guerrillas)) {
        (remaining min pieces.totalOf(baseTargets)) match {
          case 0 =>
          case num =>
            val prompt = s"\nRemove bases"
            val toRemove = askPieces(pieces, num, baseTargets, Some(prompt))
            removeToAvailable(name, toRemove)
            removed = removed + toRemove
        }

        if (!vulnerableTunnels && canRevealTunnel && remaining > 0 && pieces.has(InsurgentTunnels)) {
          val die = d6
          val success = die > 3
          log("\nNext piece to remove would be a tunneled base")
          log(separator())
          log(s"Die roll is: ${die} [${if (success) "Tunnel destroyed!" else "No effect"}]")
          if (success) {
            val prompt = "\nRemove tunnel marker"
            val tunnel = askPieces(pieces, 1, InsurgentTunnels, Some(prompt))
            removeTunnelMarker(name, tunnel)
          }
        }
      }
      removed
    }
  }


  //  Perform ARNV redeployment during a Coup Round.
  //  1.  Prompt the use to move all Troops on LoCs and Provinces without COIN bases
  //      to either Cities without NVA control, spaces with a COIN base or Saigon.
  //  2.  Allow user to move any other Troops
  //      to either Cities without NVA control, spaces with a COIN base or Saigon.
  //  3.  Allow user to move Police to LoCs or COIN controlled spaces in South Vietnam.
  def redeployARVNForces(): Unit = {
    // Get the destinations up front because the should not be affected
    // by changes to control as pieces are moved.
    // Control is not adjusted until the end of the Redeploy phase.
    val troopDestinations     = arvnRedeployTroopDestinations()
    val policeDestinations    = arvnRedeployPoliceDestinations()
    val mandatoryTroopOrigins = arvnRedeployMandatoryTroopOrigins()

    def numTroopsStr(name: String) = {
      val num = game.getSpace(name).pieces.totalOf(ARVNTroops)
      amountOf(num, "troop")
    }
    def numPoliceStr(name: String) = {
      val num = game.getSpace(name).pieces.totalOf(ARVNPolice)
      s"$num police"
    }

    def moveTroopsFrom(origin: String, mandatory: Boolean): Unit = {
      val numTroops = game.getSpace(origin).pieces.totalOf(ARVNTroops)

      if (numTroops > 0) {
        val dests = troopDestinations filterNot (_ == origin)
        val finished = if (mandatory) None else Some("finished" -> s"Finished moving troops out of $origin")
        val choices  = dests.map(name => name -> name):::finished.toList

        println(s"\n${numTroopsStr(origin)} remaining in $origin")
        askMenu(choices, "Choose destination:", allowAbort = false).head match {
          case "finished" =>
          case dest =>
            val num = askInt("Move how many troops", 0, numTroops,
                            default = Some(numTroops), allowAbort = false)
            if (num > 0) {
              movePieces(Pieces(arvnTroops = num), origin, dest)
            }
        }
        // Keep trying until no more troops or user is finished
        moveTroopsFrom(origin, mandatory)
      }
    }

    def movePoliceFrom(origin: String): Unit = {
      val numPolice = game.getSpace(origin).pieces.totalOf(ARVNPolice)

      if (numPolice > 0) {
        val dests = policeDestinations filterNot (_ == origin)
        val choices  = dests.map(name => name -> name) :+
                       ("finished" -> s"Finished moving police out of $origin")

        println(s"\n${numPoliceStr(origin)} remaining in $origin")
        askMenu(choices, "Choose destination:", allowAbort = false).head match {
          case "finished" =>
          case dest =>
            val num = askInt("Move how many police", 0, numPolice,
                            default = Some(numPolice), allowAbort = false)
            if (num > 0) {
              movePieces(Pieces(arvnPolice = num), origin, dest)
            }
        }
        // Keep trying until no more troops or user is finished
        movePoliceFrom(origin)
      }
    }

    def doMandatoryTroopMoves(origins: List[String]): Unit = {
      if (origins.nonEmpty) {
        val choices = origins map (name => name -> s"$name [${numTroopsStr(name)}]")

        println("\nMandatory Troop Redeployment")
        val origin = askMenu(choices, "Move Troops out of which space:", allowAbort = false).head
        moveTroopsFrom(origin, mandatory = true)
        doMandatoryTroopMoves(origins filterNot (_ == origin))
      }
    }

    def doVoluntaryTroopMoves(): Unit = {
      val origins = spaceNames(game.spaces filter (_.pieces.has(ARVNTroops)))
      if (origins.nonEmpty) {
        val choices = origins.map(name => name -> s"$name [${numTroopsStr(name)}]") :+
                      ("finished" -> "Finished with voluntary Troop Redeployment")

        println("\nVoluntary Troop Redeployment")
        askMenu(choices, "Move Troops out of which space:", allowAbort = false).head match {
          case "finished" =>
          case origin =>
            moveTroopsFrom(origin, mandatory = false)
            doVoluntaryTroopMoves()
        }
      }
    }

    def doVoluntaryPoliceMoves(): Unit = {
      val origins = spaceNames(game.spaces filter (_.pieces.has(ARVNPolice)))
      if (origins.nonEmpty) {
        val choices = origins.map(name => name -> s"$name [${numPoliceStr(name)}]") :+
                      ("finished" -> "Finished with voluntary Police Redeployment")

        println("\nVoluntary Police Redeployment")
        askMenu(choices, "Move Police out of which space:", allowAbort = false).head match {
          case "finished" =>
          case origin =>
            movePoliceFrom(origin)
            doVoluntaryPoliceMoves()
        }
      }
    }

    if (arvnCanRedeployTroops) {
      doMandatoryTroopMoves(mandatoryTroopOrigins)
      doVoluntaryTroopMoves()
    }
    else
      log("There are no ARVN Troops that can Redeploy")

    if (arvnCanRedeployPolice)
      doVoluntaryPoliceMoves()
    else
      log("There are no ARVN Police that can Redeploy")
  }

  //  Perform NVA redeployment during a Coup Round.
  //  1.  NVA may voluntarily move NVA Troops from anywhere to any space
  //      with an NVA base.
  def redeployNVATroops(): Unit = {
    val troopDestinations = arvnRedeployTroopDestinations()

    def numTroopsStr(name: String) = {
      val num = game.getSpace(name).pieces.totalOf(NVATroops)
      amountOf(num, "troop")
    }

    def moveTroopsFrom(origin: String): Unit = {
      val numTroops = game.getSpace(origin).pieces.totalOf(NVATroops)

      if (numTroops > 0) {
        val dests = troopDestinations filterNot (_ == origin)
        val choices  = dests.map(name => name -> name) :+
                       ("finished" -> s"Finished moving troops out of $origin")

        println(s"\n${numTroopsStr(origin)} remaining in $origin")
        askMenu(choices, "Choose destination:", allowAbort = false).head match {
          case "finished" =>
          case dest =>
            val num = askInt("Move how many troops", 0, numTroops,
                            default = Some(numTroops), allowAbort = false)
            if (num > 0) {
              movePieces(Pieces(nvaTroops = num), origin, dest)
            }
        }
        // Keep trying until no more troops or user is finished
        moveTroopsFrom(origin)
      }
    }

    def doVoluntaryTroopMoves(): Unit = {
      val origins = spaceNames(game.spaces filter (_.pieces.has(NVATroops)))
      if (origins.nonEmpty) {
        val choices = origins.map(name => name -> s"$name [${numTroopsStr(name)}]") :+
                      ("finished" -> "Finished with voluntary Troop Redeployment")

        println("\nVoluntary Troop Redeployment")
        askMenu(choices, "Move Troops out of which space:", allowAbort = false).head match {
          case "finished" =>
          case origin =>
            moveTroopsFrom(origin)
            doVoluntaryTroopMoves()
        }
      }
    }

    if (nvaCanRedeployTroops)
      doVoluntaryTroopMoves()
    else
      log("There are no NVA Troops that can Redeploy")
  }

  // Part of the Commitment phase of a Coup Round.
  // All US Troops currently in the casualites box
  // are place on the map in either:
  // COIN controlled spaces, LoCs, or Saigon.
  def placeUSCasualtyTroopsOnMap(coinControlled: Set[String]): Unit = {
    val validSpace = (sp: Space) =>
      sp.name == Saigon ||
      sp.isLoC          ||
      coinControlled(sp.name)

    def placeTroops(): Unit = {
      val maxTroops = game.casualties.totalOf(USTroops)
      if (maxTroops > 0) {
        val candidates = spaceNames(game.spaces filter validSpace)
        val numLeft = amountOf(maxTroops, "Troop casualty", Some("Troop casualties"))
        val prompt = s"\n$numLeft remaining to place on the map\nChoose space to place Troops"
        val name = askSimpleMenu(candidates, prompt, allowAbort = false).head
        val num  = askInt(s"Move how many Troops to $name", 0, maxTroops, allowAbort = false)
        moveCasualtiesToMap(Pieces(usTroops = num), name)
        placeTroops()
      }
    }

    placeTroops()
  }

  // Part of the Commitment phase of a Coup Round.
  // US may move up to `maxTroopsAllowed` Troops and up to 2 bases
  // Available box, COIN controlled spaces, LoCs and Saigon
  def moveUSCommitmentPieces(maxTroopsAllowed: Int, coinControlled: Set[String]): Unit = {
    val AVAILABLE = "Available box"
    var somethingMoved = false
    val movedPieces = new MovingGroups()
    val validSpace = (sp: Space) =>
      sp.name == Saigon ||
      sp.isLoC          ||
      coinControlled(sp.name)
    val validNames = spaceNames(game.spaces filter validSpace).toSet
    val sortedNames = validNames.toList.sorted(SpaceNameOrdering)
    val sortedBaseNames = sortedNames filterNot (n => game.getSpace(n).isLoC)
    val totalTroops = game.availablePieces.totalOf(USTroops) +
                      game.totalOnMap(sp => if (validNames(sp.name)) sp.pieces.totalOf(USTroops) else 0)
    val totalBases  = game.availablePieces.totalOf(USBase) +
                      game.totalOnMap(sp => if (validNames(sp.name)) sp.pieces.totalOf(USBase) else 0)
    val maxTroops   = maxTroopsAllowed min totalTroops
    val maxBases    = 2 min totalBases
    def numTroopsMoved = movedPieces.allPieces.totalOf(USTroops)
    def numBasesMoved = movedPieces.allPieces.totalOf(USBase)

    def getPieces(name: String) = name match {
      case AVAILABLE => game.availablePieces
      case _         => game.getSpace(name).pieces
    }

    def moveEm(pieces: Pieces, origin: String, dest: String): Unit = if (pieces.nonEmpty) {
      (origin, dest) match {
        case (AVAILABLE, to)   => placePieces(to, pieces)
        case (from, AVAILABLE) => removeToAvailable(from, pieces)
        case (from, to)        => movePieces(pieces, from, to)
      }
      movedPieces.add(dest, pieces)
      somethingMoved = true
    }

    def moveNewTroops(): Unit = {
      val candidates  = sortedNames filter { name => (getPieces(name) - movedPieces(name)).has(USTroops) }
      val choices     = if (game.availablePieces.has(USTroops)) AVAILABLE +: candidates else candidates
      val origin      = askSimpleMenu(choices, "\nMove Troops in which space:", allowAbort = false).head
      val destChoices = (AVAILABLE::sortedNames) filterNot (_ == origin)
      val dest        = askSimpleMenu(destChoices, s"\nMove Troops in $origin to which space:", allowAbort = false).head
      val maxNum      = (getPieces(origin) - movedPieces(origin)).totalOf(USTroops) min (maxTroops - numTroopsMoved)
      val num         = askInt("Move how many Troops", 0, maxNum, allowAbort = false)
      moveEm(Pieces(usTroops = num), origin, dest)
    }

    def moveNewBase(): Unit = {
      val candidates  = sortedBaseNames filter { name => (getPieces(name) - movedPieces(name)).has(USBase) }
      val choices     = if (game.availablePieces.has(USBase)) AVAILABLE +: candidates else candidates
      val origin      = askSimpleMenu(choices, "\nMove Base from which space:", allowAbort = false).head
      val destChoices = (AVAILABLE::sortedBaseNames) filterNot (_ == origin)
      val dest        = askSimpleMenu(destChoices, "\nMove the Base to which space:", allowAbort = false).head
      moveEm(Pieces(usBases = 1), origin, dest)
    }

    def moveOldTroops(): Unit = {
      val avail = if (movedPieces(AVAILABLE).has(USTroops)) Some(AVAILABLE) else None
      val candidates = (movedPieces.toList
                           filter { case (name, pieces) => name != AVAILABLE && pieces.has(USTroops) }
                           map    { case (name, _) => name})
      val choices     = avail.toList ::: candidates
      val origin      = askSimpleMenu(choices, "\nMove Troops from which space:", allowAbort = false).head
      val destChoices = (AVAILABLE::sortedBaseNames) filterNot (_ == origin)
      val dest        = askSimpleMenu(destChoices, "\nMove Troops to which space:", allowAbort = false).head
      val maxNum      = movedPieces(origin).totalOf(USTroops)
      val num         = askInt("Move how many Troops", 0, maxNum, allowAbort = false)
      val pieces      = Pieces(usTroops = num)
      movedPieces.remove(origin, pieces)
      moveEm(pieces, origin, dest)
    }

    def moveOldBase(): Unit = {
      val avail = if (movedPieces(AVAILABLE).has(USBase)) Some(AVAILABLE) else None
      val candidates = (movedPieces.toList
                           filter { case (name, pieces) => name != AVAILABLE && pieces.has(USBase) }
                           map    { case (name, _) => name})
      val choices     = avail.toList ::: candidates
      val origin      = askSimpleMenu(choices, "\nMove Base from which space:", allowAbort = false).head
      val destChoices = (AVAILABLE::sortedNames) filterNot (_ == origin)
      val dest        = askSimpleMenu(destChoices, "\nMove the Base to which space:", allowAbort = false).head
      val pieces      = Pieces(usBases = 1)
      movedPieces.remove(origin, pieces)
      moveEm(pieces, origin, dest)
    }

    def nextAction(): Unit = {
      val canMoveNewTroops = maxTroops > 0 && numTroopsMoved < maxTroops
      val canMoveOldTroops = numTroopsMoved > 0
      val canMoveNewBases  = maxBases > 0 && numBasesMoved < maxBases
      val canMoveOldBases  = numBasesMoved > 0
      val choices = List(
        choice(canMoveNewTroops, "new-troop", "Move troops"),
        choice(canMoveNewBases,  "new-base",  "Move a base"),
        choice(canMoveOldTroops, "old-troop", "Re-move troops that have already moved"),
        choice(canMoveOldBases,  "old-base",  "Re-move a base that has already moved"),
        choice(true,             "finished",  "Finished moving pieces")
      ).flatten

      println(s"\nMoved so far: $numTroopsMoved/$maxTroops troops and $numBasesMoved/$maxBases bases")
      askMenu(choices, "Choose one:", allowAbort = false).head match {
        case "new-troop" =>
          moveNewTroops()
          nextAction()
        case "new-base"  =>
          moveNewBase()
          nextAction()
        case "old-troop" =>
          moveOldTroops()
          nextAction()
        case "old-base"  =>
          moveOldBase()
          nextAction()
        case _ =>
      }
    }

    if (totalBases + totalTroops > 0)
      nextAction()

    if (!somethingMoved)
      log("No pieces were moved")
  }

    // Final part of the Commitment Phase of a Coup Round.
    def usWithdrawalShifts(numPopShifts: Int): Unit = {
      var shiftSpaces = Set.empty[String]

      def pop(name: String) = game.getSpace(name).population

      def nextShift(numRemaining: Int): Unit = if (numRemaining > 0) {
        val candidates = spaceNames(game.nonLocSpaces filter { sp =>
          !shiftSpaces(sp.name)         &&
          sp.population >  0            &&
          sp.population <= numRemaining &&
          sp.support > ActiveOpposition
        })

        if (candidates.nonEmpty) {
          val choices = candidates.map(name => name -> s"$name [pop: ${pop(name)}]") :+
                        "finished" -> "Finished shifting spaces toward Opposition"

          val remaining = s"${amountOf(numRemaining, "population shift")} remaining"
          val prompt = s"\nChoose space to shift: ($remaining)"
          askMenu(choices, prompt, allowAbort = false).head match {
            case "finished" =>
            case name =>
              decreaseSupport(name, 1)
              shiftSpaces += name
              nextShift(numRemaining - pop(name))
          }
        }
      }

      nextShift(numPopShifts)
    }
  // Returns false if user decides not to pacify in space
  def pacifySpace(name: String, faction: Faction, coupRound: Boolean): Boolean = {

    val nguyenCaoKy = isRVNLeader(RVN_Leader_NguyenCaoKy)
    val blowtorch   = momentumInPlay(Mo_BlowtorchKomer)
    // In Coup Round The leader takes precedence over Blowtorch because it was played
    // more recently.  Otherwise the Blowtorch momentum was played more recently and take precedence.
    val cost = if (coupRound)
        (if (nguyenCaoKy) 4 else if (blowtorch) 1 else 3)
    else
        (if (nguyenCaoKy) 4 else 3)
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

      val num = askMenu(choices, "\nPacify:", allowAbort = false).head
      if (num == 0)
        false
      else {
        val shift = (num - sp.terror) max 0
        println()
        decreaseResources(ARVN, num * cost)
        removeTerror(name, num min sp.terror)
        increaseSupport(name, shift)
        true
      }
    }
  }

  // Returns false if user decides not to agitate in space
  def agitateSpace(name: String, coupRound: Boolean): Boolean = {
    val sp          = game.getSpace(name)
    val maxShift    = ((sp.support.value - ActiveOpposition.value) max 0) min 2
    val maxInSpace  = maxShift + sp.terror
    val cadres      = capabilityInPlay(Cadres_Unshaded)
    val maxPossible = maxInSpace min game.arvnResources
    val cadres_msg = if (coupRound) "" else s" [$Cadres_Shaded]"

    log(s"\nVC Agitates in ${name}$cadres_msg")
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

      val num = askMenu(choices, "\nAgitate:", allowAbort = false).head
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

  // Ask the user if they wish to use the Cobras capability
  // in the space.
  // (Part of a Sweep operation)
  def doCobrasUnshaded(name: String): Boolean = {
    val sp        = game.getSpace(name)
    val isPossible = capabilityInPlay(Cobras_Unshaded) &&
                     (sp.pieces.has(NVATroops::ActiveGuerrillas) ||
                     (!sp.pieces.has(UndergroundGuerrillas) && sp.pieces.has(InsurgentNonTunnels)))

    if (isPossible && askYorN(s"Do you wish to use the Cobras capability in $name? (y/n) ")) {
      log(s"\nUsing $Cobras_Unshaded")
      log(separator())
      val deadPiece = if (sp.pieces.has(NVATroops))
        Pieces(nvaTroops = 1)
      else if (sp.pieces.has(ActiveGuerrillas))
        askPieces(sp.pieces, 1, ActiveGuerrillas)
      else
        askPieces(sp.pieces, 1, InsurgentNonTunnels)

      removeToAvailable(name, deadPiece)
      true
    }
    else
      false
  }

  // If Transport special activity was used,  then Armored Cavalry (unshaded)
  // allows ARVN to free assault in one Transport destination
  def armoredCavalryAssault(): Unit = {
    // General Landsdale prohibits assault
    if (capabilityInPlay(ArmoredCavalry_Unshaded) && !momentumInPlay(Mo_GeneralLansdale)) {
      val isCandidate = (sp: Space) => sp.pieces.has(InsurgentPieces) &&sp.pieces.has(ARVNCubes)
      val candidates = spaceNames(Special.transportDestinations map game.getSpace filter isCandidate)

      if (candidates.nonEmpty && askYorN(s"\nDo you wish to perform a free assault in 1 transport destinaion via [$ArmoredCavalry_Unshaded]? (y/n) ")) {
          val name = askCandidate("Free Assault in which space: ", candidates)

          log(s"\n$ArmoredCavalry_Unshaded triggers a free Assault")
          performAssault(ARVN, name, Params(free = true))
      }
    }
  }

  def moveUSOutOfPlayToCities(maxNum: Int): Unit = {

    def placeInCity(pieces: Pieces): Unit = if (pieces.nonEmpty) {

      val name = askSimpleMenu(Cities, "Place pieces in which City:").head
      val num  = askInt(s"Place how many pieces in $name", 0, pieces.total)
      val toPlace = askPieces(pieces, num)
      moveOutOfPlayToMap(toPlace, name)
      placeInCity(pieces - toPlace)
    }

    val numPieces = maxNum min game.casualties.totalOf(USPieces)
    val prompt = "Choose US Pieces to remove from Out of Play"
    val pieces = askPieces(game.casualties, numPieces, USPieces, Some(prompt))

    placeInCity(pieces)
  }


  // Carry out an ambush in the given space
  // MainForceBns_Shaded   - 1 VC Ambush space may remove 2 enemy pieces
  // PT76_Unshaded - Each NVA attack space, first remove 1 NVA troop cube (also ambush)
  def performAmbush(name: String, faction: Faction, op: Operation, free: Boolean): Boolean = {
    val sp = game.getSpace(name)
    val ambusher = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
    val candidates = ambushTargets(name)
    assert(sp.pieces.has(ambusher), s"performAmbush(): $faction has no underground guerrilla in $name")

    if (candidates.nonEmpty) {
      askCandidateOrBlank("Kill target piece in which space: ", candidates) match {
        case Some(targetName) =>
          val target = game.getSpace(targetName)
          val coinPieces = target.pieces.only(CoinPieces)
          val num    = if (coinPieces.total > 1 &&
                           capabilityInPlay(MainForceBns_Shaded) &&
                           faction == VC && !Special.usedMainForceBns &&
                           askYorN(s"\nDo you wish to remove two enemy pieces in $name? (y/n) ")) {
            Special.usedMainForceBns = true
            2
          }
          else
            1
          val deadPieces = askEnemyCoin(coinPieces, num, prompt = Some(s"Ambushing in $name"))

          if (name == targetName)
            log(s"\n$faction Ambushes in $name")
          else
            log(s"\n$faction Ambushes in $name, targeting $targetName")
          log(separator())
          if (!free)
            decreaseResources(faction, 1)
          if (faction == NVA && op == Attack && capabilityInPlay(PT76_Unshaded) && sp.pieces.has(NVATroops))
            removeToAvailable(name, Pieces(nvaTroops = 1), Some(s"$PT76_Unshaded triggers:"))
          revealPieces(name, Pieces().set(1, ambusher))
          removePieces(targetName, deadPieces)
          Special.selectedSpaces = Special.selectedSpaces + name
          true

        case None =>
          false
      }
    }
    else {
      println(s"\nThere are no COIN pieces that can be killed by ambushing in $name")
      false
    }
  }


  def performAttack(name: String, faction: Faction, free: Boolean): Unit = {
    val sp = game.getSpace(name)
    val guerrillas = sp.pieces.only(if (faction == NVA) NVAGuerrillas else VCGuerrillas)
    val troops     = if (faction == NVA) sp.pieces.only(NVATroops) else Pieces()
    val coinPieces = sp.pieces.only(CoinPieces)

    if (faction == VC)
      assert(guerrillas.nonEmpty, s"performAttack(): $faction has no guerrilla in $name")
    else
      assert(guerrillas.nonEmpty || troops.nonEmpty, s"performAttack(): $faction has no guerrillas or troops in $name")
    assert(coinPieces.nonEmpty, s"performAttack(): There are no COIN pieces to attack in $name")

    val guerrillaAttack = if (faction == NVA) {
      if (guerrillas.nonEmpty && troops.nonEmpty) {
        val choices = List("guerrillas" -> "Guerrillas", "troops" -> "Troops")
        askMenu(choices, "Attack using which units:").head == "guerrillas"
      }
      else
        guerrillas.nonEmpty
    }
    else
      true


    if (guerrillaAttack) {
      val (underground, active) = if (faction == NVA)
        (NVAGuerrillas_U, NVAGuerrillas_A)
      else
        (VCGuerrillas_U, VCGuerrillas_A)
      val guerrilla_types = underground :: active :: Nil
      val toActivate = sp.pieces.only(underground)
      val maxNum     = 2 min coinPieces.total
      val die        = d6
      val success    = die <= guerrillas.total

      log(s"\n$faction Attacks in $name")
      log(separator())
      if (!free)
        decreaseResources(faction, 1)

      if (faction == NVA && capabilityInPlay(PT76_Unshaded) && troops.nonEmpty)
        removeToAvailable(name, Pieces(nvaTroops = 1), Some(s"$PT76_Unshaded triggers:"))
      log(s"\nDie roll: $die [${if (success) "Success!" else "Failure"}]")
      if (success) {
        val num        = askInt("\nRemove how many pieces", 1, maxNum, Some(maxNum))
        val deadPieces = askEnemyCoin(coinPieces, num, prompt = Some(s"Attacking in $name"))
        val attrition  = deadPieces.only(USTroops::USBase::Nil).total min sp.pieces.only(guerrilla_types).total

        revealPieces(name, toActivate)
        removePieces(name, deadPieces)
        removeToAvailable(name, Pieces().set(attrition, active), Some("Attrition:"))  // All guerrillas are now active
      }
    }
    else {  // NVA Troops attack
      val use_pt76_shaded = faction == NVA && capabilityInPlay(PT76_Shaded) && !pt76_shaded_used &&
                            askYorN(s"Do you which to use [$PT76_Shaded] in this attack? (y/n) ")
      val pt76_unshaded   = capabilityInPlay(PT76_Unshaded)


      val ratio      = if (use_pt76_shaded) 1.0 else 0.5
      val numTroops  = (if (pt76_unshaded) troops.total - 1 else troops.total) max 0
      val num        = (numTroops * ratio).toInt min coinPieces.total
      val deadPieces = if (num > 0)
        askEnemyCoin(coinPieces, num, prompt = Some(s"Attacking in $name"))
      else
        Pieces()
      val attrition  = deadPieces.only(USTroops::USBase::Nil).total min numTroops

      log(s"\n$faction Attacks in $name")
      log(separator())
      if (!free)
        decreaseResources(faction, 1)

      if (pt76_unshaded)
        removeToAvailable(name, Pieces(nvaTroops = 1), Some(s"$PT76_Unshaded triggers:"))
      if (num == 0)
        log("No hits are inflicted")
      if (use_pt76_shaded)
        log(s"NVA elects to use [$PT76_Shaded]")
      removePieces(name, deadPieces)
      removeToAvailable(name, Pieces(nvaTroops = attrition), Some("Attrition:"))

      if (use_pt76_shaded)
        pt76_shaded_used = true  // Can only be use once per attack operation
    }
  }



  // A human player has opted to take an action on the current card.
  def act(): Unit = {
    val faction = game.actingFaction.get
    val action = if (game.executingPivotalEvent)
      Event
    else {
      val choices: List[(Action, String)] =
        game.sequence.availableActions map (a => a -> a.toString)

      askMenu(choices, "\nChoose one:").head
    }

    game = game.copy(sequence = game.sequence.addActor(faction, action))
    log()
    log(s"Move the $faction cylinder to the $action box")

    action match {
      case Event         => executeEvent(faction)
      case OpPlusSpecial => executeOp(faction, Params(specialActivity = true))
      case OpOnly        => executeOp(faction)
      case LimitedOp     => executeOp(faction, Params(maxSpaces = Some(1)))
      case Pass          => factionPasses(faction)
    }
  }


  // US Advise special activity
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  def doAdvise(params: Params): Unit = {
    val arvnForces = ARVNCubes:::Rangers
    val cobras = capabilityInPlay(Cobras_Unshaded)
    // Can remove tunnel marker
    // Underground guerrillas shield bases/tunnels
    def hasAssaultTarget(sp: Space): Boolean = {
      val underground = sp.pieces.has(UndergroundGuerrillas)
      val activeForces = sp.pieces.has(NVATroops::ActiveGuerrillas)
      val enemyBases = sp.pieces.has(InsurgentNonTunnels:::InsurgentTunnels)

      activeForces || (!underground && enemyBases)
    }

    // No tunnels
    // Can remove underground guerrillas
    def hasSpecialOpsTarget(sp: Space): Boolean = {
      sp.pieces.has(NVATroops::UndergroundGuerrillas:::ActiveGuerrillas:::InsurgentNonTunnels)
    }

    // Part of Advise special activity
    // Activate 1 underground Irregular/Ranger to remove
    // 2 enemy pieces.  Bases last (Cannot target Tunnels at all)
    def useSpecialForces(name: String): Unit = {
      val enemyForceTypes = NVATroops::UndergroundGuerrillas:::ActiveGuerrillas
      val sp = game.getSpace(name)
      val choices = List(
        choice(sp.pieces.has(Irregulars_U), Irregulars_U, "US Irregular"),
        choice(sp.pieces.has(Rangers_U),    Rangers_U,    "ARVN Ranger")
      ).flatten

      val forceType   = askMenu(choices, "\nActivate which special force:").head
      val enemyForces = sp.pieces.only(enemyForceTypes)
      val enemyBases  = sp.pieces.only(InsurgentNonTunnels)
      val numForces   = 2 min enemyForces.total
      val numBases    = (2 - numForces) min enemyBases.total

      log(s"\nAdvise in $name using ${forceType.singular}")
      log(separator())
      revealPieces(name, Pieces().set(1, forceType))
      val deadForces = askPieces(enemyForces, numForces, prompt = Some("Remove enemy forces"))
      val deadBases  = askPieces(enemyBases, numBases, prompt = Some("Remove enemy bases"))
      removeToAvailable(name, deadForces + deadBases)
    }

    def nextAdviseSpace(): Unit = {
      val sweepCandidates = spaceNames(game.nonLocSpaces filter { sp =>
        !sp.isNorthVietnam &&
        params.spaceAllowed(sp.name) &&
        !Special.trainingSpaces(sp.name) &&
        !Special.selectedSpaces(sp.name) &&
        sp.pieces.has(arvnForces) &&
        (sp.pieces.has(UndergroundGuerrillas) || (cobras && hasAssaultTarget(sp)))
      })
      val assaultCandidates = spaceNames(game.spaces filter { sp =>
        params.spaceAllowed(sp.name) &&
        !Special.trainingSpaces(sp.name) &&
        !Special.selectedSpaces(sp.name) &&
        sp.pieces.has(arvnForces) &&
        hasAssaultTarget(sp)
      })
      val specialForcesCandidates = spaceNames(game.spaces filter { sp =>
        params.spaceAllowed(sp.name) &&
        !Special.trainingSpaces(sp.name) &&
        !Special.selectedSpaces(sp.name) &&
        sp.pieces.has(Irregulars_U::Rangers_U::Nil) &&
        hasSpecialOpsTarget(sp)
      })

      val maxAdvise = if (params.maxSpaces.nonEmpty)
        params.maxSpaces.get
      else if (momentumInPlay(Mo_TyphoonKate)) 1 else 2
      val canAdvise = Special.selectedSpaces.size < maxAdvise

      val canSweep   = canAdvise && !game.inMonsoon && sweepCandidates.nonEmpty
      val canAssault = canAdvise && !momentumInPlay(Mo_GeneralLansdale) &&
                                    assaultCandidates.nonEmpty
      val canSpecial = canAdvise && specialForcesCandidates.nonEmpty
      val choices = List(
        choice(canSweep,   "sweep",   "Sweep a space with ARVN forces"),
        choice(canAssault, "assault", "Assault a space with ARVN forces"),
        choice(canSpecial, "special", "Use Irregular/Ranger to remove enemy pieces"),
        choice(true,       "finished", "Finished selecting Advise spaces")
      ).flatten

      println(s"\n${amountOf(Special.selectedSpaces.size, "space")} of $maxAdvise selected for Advise")
      println(separator())
      wrap("", Special.selectedSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nChoose Advise option:").head match {
        case "sweep" =>
          askCandidateOrBlank("ARVN Sweep in which space: ", sweepCandidates) foreach { name =>
            activateGuerrillasForSweep(name, ARVN)
            checkShadedBoobyTraps(name, ARVN)
            doCobrasUnshaded(name)
            Special.selectedSpaces = Special.selectedSpaces + name
          }
          nextAdviseSpace()

        case "assault" =>
          askCandidateOrBlank("ARVN Assault in which space: ", assaultCandidates) foreach { name =>
            performAssault(ARVN, name, params.copy(free = true))
            Special.selectedSpaces = Special.selectedSpaces + name
          }
          nextAdviseSpace()

        case "special" =>
          askCandidateOrBlank("Use Irregular/Ranger in which space: ", specialForcesCandidates) foreach { name =>
            useSpecialForces(name)
            Special.selectedSpaces = Special.selectedSpaces + name
          }
          nextAdviseSpace()

        case _ =>
      }
    }

    logSAChoice(US, Advise)

    nextAdviseSpace();
    if (askYorN("\nDo you wish to add +6 Aid? (y/n) "))
      increaseUsAid(6)
  }

  // US special activity
  // Any US Troops plus up to 4 (Irregulars, Rangers, ARVN Troops)
  // among 4 selected spaces (2 in Monsoon, Never N. Vietnam)
  def doAirLift(params: Params): Unit = {
    val Others: List[PieceType] = ARVNTroops::Rangers:::Irregulars
    val maxAirLiftSpaces = if (params.maxSpaces.nonEmpty)
      params.maxSpaces.get
    else if (game.inMonsoon) 2 else 4
    var airLiftSpaces    = params.singleTarget.toList  // Some events specify air lift into ...
    val liftedOthers     = new MovingGroups()  // ARVN troops, Rangers, Irregulars (max 4)

    def totalLiftedOthers = liftedOthers.allPieces.total

    // All US Troops and Others that have moved are always included.
    // Others that have not moved only if total others < 4
    def moveablePieces(sp: Space) = {
      val usTroopsAndLiftedOthers = sp.pieces.only(USTroops) + liftedOthers(sp.name)
      val unmovedOthers = sp.pieces.only(Others) - liftedOthers(sp.name)

      if (totalLiftedOthers < 4)
        usTroopsAndLiftedOthers + unmovedOthers
      else
        usTroopsAndLiftedOthers
    }

    def airLiftCandidates = spaceNames(game.spaces filter { sp =>
      !sp.isNorthVietnam && 
      params.spaceAllowed(sp.name) &&
      !airLiftSpaces.contains(sp.name)
    })

    def liftOutCandidates = if (airLiftSpaces.size > 1)
      airLiftSpaces.filter{ name => 
        Some(name) != params.singleTarget &&
        moveablePieces(game.getSpace(name)).nonEmpty
      }
    else
      Nil


    // Lift forces out and place them in a destination space
    def liftForcesOutOf(srcName: String): Unit = {
      val ForceTypes = List(USTroops, Irregulars_U, Irregulars_A, ARVNTroops, Rangers_U, Rangers_A)
      val destCandidates = if (params.singleTarget.nonEmpty)
        params.singleTarget.toList
      else
        airLiftSpaces filter (_ != srcName)
      val choices = (destCandidates map (name => name -> name)) :+ ("none" -> "Do not move forces now")

      def moveForces(destName: String): Unit = {
        val src           = game.getSpace(srcName)
        val dest          = game.getSpace(destName)
        val eligible      = moveablePieces(src)
        val others        = eligible.only(Others)
        val unmovedOthers = eligible.only(Others) - liftedOthers(srcName)
        val maxNewOthers  = (4 - totalLiftedOthers) min unmovedOthers.total
        def maxForce(t: PieceType): Int = {
          if (t == USTroops)
            eligible.totalOf(USTroops)
          else
            liftedOthers(srcName).totalOf(t) + (maxNewOthers min unmovedOthers.totalOf(t))
        }
        val forceChoices = ForceTypes flatMap { t =>
          val num = maxForce(t)
          if (num > 0) Some(Some(t) -> s"Air Lift ${t.plural}") else None
        }
        val choices = forceChoices :+ (None -> s"Finished moving forces out of $srcName")

        println(s"\nAir Lifting forces from $srcName to $destName")
        askMenu(choices, "Choose one:").head match {
          case None =>
          case Some(t) =>
            val num = askInt(s"Move how many $t", 0, maxForce(t))
            movePieces(Pieces().set(num, t), srcName, destName)
            // Update liftedOthers
            if (t != USTroops) {
              val numMovedBefore = liftedOthers(srcName).totalOf(t)
              val numFirstMove   = (num - numMovedBefore) max 0
              val numMovingAgain = num - numFirstMove
              liftedOthers.remove(srcName, Pieces().set(numMovingAgain, t))
              liftedOthers.add(destName, Pieces().set(num, t))
            }
            moveForces(destName)
        }
      }

      askMenu(choices, "\nLift forces to which space:").head match {
        case "none"   =>
        case destName =>
          moveForces(destName)
      }
    }

    def nextAirLiftAction(): Unit = {
      val selectChoice = choice(airLiftSpaces.size < maxAirLiftSpaces, "select", "Select an Air Lift space").toList
      val moveChoices  = liftOutCandidates map (name => name -> s"Lift forces out of $name")
      val choices      = selectChoice:::moveChoices:::List("finished" -> "Finished with Air Lift")


      println(s"\n${amountOf(airLiftSpaces.size, "space")} of $maxAirLiftSpaces selected for Air Lift")
      println(separator())
      wrap("", airLiftSpaces, showNone = false) foreach println

      askMenu(choices, "\nSelect one:").head match {
        case "select" =>
          askCandidateOrBlank("\nAir Lift in which space: ", airLiftCandidates) foreach { name =>
            airLiftSpaces = airLiftSpaces :+ name
          }
          nextAirLiftAction()

        case "finished" =>

        case name =>
          liftForcesOutOf(name)
          nextAirLiftAction()
      }
    }

    val notes = List(
      noteIf(game.inMonsoon, "May only choose 2 spaces in Monsoon")
    ).flatten
    // Start of Air Lift Special Activity
    // ------------------------------------
    logSAChoice(US, AirLift, notes)

    nextAirLiftAction()
  }

  // US special activity
  // Destroy exposed insurent units and degrade trail
  // Up to 6 spaces (2 in Monsoon) each with any US or ARVN piece
  // Roll d6 to determine number of hits
  // 2 hits may be used to degrade trail by one box (only once)
  // 1 hit per enemy piece removed.  (NVATroops, then active guerrillas, then exposed bases)
  // Shift each City/Province with >= 1 Population one level toward Active Opposition
  //
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // Mo_WildWeasels - Either degrade trail OR remove only 1 piece (not 1-6)
  // Mo_ADSID       - -6 NVA resrouces at any trail change
  // TopGun_Unshaded - degrade trail by 2 boxes instead of 1
  // TopGun_Shaded   - degrade of trail requires d6 = 4-6 (after expending 2 hits)
  // ArcLight_Unshaded - 1 space may be Province without COIN pieces (including N. Vietnam)
  // ArcLight_Shaded - spaces removing >1 piece shift two levels toward Active Opposition
  // LaserGuidedBombs_Unshaded - In space were only 1 piece removed, no shift toward Active Opposition
  // LaserGuidedBombs_Shaded - Remove no more than 2 pieces total
  // AAA_Shaded - Cannot degrade the trail below 2
  // MiGs_Shaded - If trail degraded, US remove 1 Available Troop to Casualties
  // SA2s_Unshaded - When trail degraded, US removes 1 NVA piece (including untunneled base) outside the South (includes N. Vietnam)
  // Mo_Oriskany   - Shaded (prohibits degrade of trail) (includes air strike, coup round, NOT evnts!)
  def doAirStrike(params: Params): Unit = {
    val adsid = game.isHuman(NVA) && momentumInPlay(Mo_ADSID)
    val oriskany = momentumInPlay(Mo_Oriskany)
    val migs_shaded = capabilityInPlay(MiGs_Shaded) && !capabilityInPlay(TopGun_Unshaded)
    val aaa_shaded = capabilityInPlay(AAA_Shaded)
    val arclight_unshaded = capabilityInPlay(ArcLight_Unshaded)
    val maxHits     = params.strikeHits getOrElse d6
    val maxSpaces = if      (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (momentumInPlay(Mo_TyphoonKate)) 1
                    else if (game.inMonsoon) 2
                    else 6
    val wildWeasels = momentumInPlay(Mo_WildWeasels) &&
                    (!capabilityInPlay(LaserGuidedBombs_Shaded) ||
                      isEventMoreRecentThan(Mo_WildWeasels, LaserGuidedBombs_Shaded.name))
    val laserShaded = capabilityInPlay(LaserGuidedBombs_Shaded) &&
                      (!momentumInPlay(Mo_WildWeasels) ||
                      isEventMoreRecentThan(LaserGuidedBombs_Shaded.name, Mo_WildWeasels))
    val maxPieces = if (wildWeasels) 1 else if (laserShaded) 2 else maxHits
    var strikeSpaces = Set.empty[String]
    var removedSpaces = Set.empty[String] // strike spaces where pieces have been removed
    var totalRemoved = 0
    var arclight_unshaded_used = false
    def isCandidate(sp: Space) = {
      params.spaceAllowed(sp.name)   &&   // Event may limit to certain spaces
      !strikeSpaces(sp.name)         &&
      sp.pieces.hasExposedInsurgents &&
      (sp.pieces.has(CoinPieces) || params.strikeNoCoin || (sp.isProvince && arclight_unshaded && !arclight_unshaded_used))
    }

    def nextAirStrikeAction(hitsRemaining: Int, hasDegraded: Boolean): Unit = {
      if (momentumInPlay(Mo_WildWeasels) && (hasDegraded || totalRemoved > 0)) {
        println(s"\nNo more Air Strike actions allowed [Momentum: $Mo_WildWeasels")
        pause()
      }
      else {
        val StrikeOpt  = "strike:(.*)".r
        val selectCandidates = spaceNames(game.spaces filter isCandidate)
        val strikeCandidates = (strikeSpaces -- removedSpaces).toList filter (name => game.getSpace(name).pieces.hasExposedInsurgents)
        val canSelect  = hitsRemaining > 0 && strikeSpaces.size < maxSpaces && selectCandidates.nonEmpty
        val minTrail   = if (aaa_shaded) 2 else 1
        val canDegrade = game.trail > minTrail && hitsRemaining > 1 && !hasDegraded && !oriskany
        val topChoices = List(
          choice(canSelect,  "select",  "Select a space to Strike"),
          choice(canDegrade, "degrade", "Degrade the trail")
        ).flatten

        def performDegrade(): Unit = {
          val die    = d6
          val success = !capabilityInPlay(TopGun_Shaded) || die > 3
          val numBoxes = {
            val num = if (capabilityInPlay(TopGun_Unshaded)) 2 else 1
            val newTrail = (game.trail - num) max minTrail
            game.trail - newTrail
          }

          log(s"\nUS Air Strikes The Trail")
          log(separator())
          if (capabilityInPlay(TopGun_Shaded))
            log(s"Die roll ($TopGun_Shaded): $die [${if (success) "Success!" else "Failure"}]")

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
              val sa2Candidates = spaceNames(spaces(OutsideSouth) filter (_.pieces.has(CanRemove)))
              if (sa2Candidates.isEmpty)
                log("There are no NVA outside the south that can be removed") // Very unlikely!
              else {
                val name = askCandidate("Remove NVA piece from which space: ", sa2Candidates)
                val sp = game.getSpace(name)
                val toRemove = askPieces(sp.pieces, 1, CanRemove)
                removeToAvailable(name, toRemove)
              }
            }
          }
        }

        // Returns number of pieces removed
        def performStrike(name: String): Int = {
          val pieces       = game.getSpace(name).pieces
          val maxNumber    = numExposedInsurgents(pieces) min hitsRemaining
          val num          = askInt(s"\nRemove how many pieces from $name", 1, maxNumber)
          val killedPieces = killExposedInsurgents(name, num, canRevealTunnel = false, params.vulnerableTunnels)
          val sp           = game.getSpace(name)
          
          log(s"\nUS Air Strikes $name")
          log(separator())
          removeToAvailable(name, killedPieces)
          if (!sp.pieces.has(CoinPieces) && arclight_unshaded)
            arclight_unshaded_used = true

          if (num > 0) {
            val numShift = if (sp.isLoC || sp.population == 0 || sp.support == ActiveOpposition)
              0
            else if (num > 1 && capabilityInPlay(ArcLight_Shaded) && sp.support > PassiveOpposition) {
              log(s"Shift 2 levels toward Active Opposition [$ArcLight_Shaded]")
              2
            }
            else if (num == 1 && capabilityInPlay(LaserGuidedBombs_Unshaded)) {
              log(s"No shift toward Active Opposition [$LaserGuidedBombs_Unshaded]")
              0
            }
            else {
              log(s"Shift 1 level toward Active Opposition")
              1
            }
            decreaseSupport(name, numShift)
          }
          removedSpaces += name
          num
        }

        // Show nextAirStrikeAction menu

        val strikeChoices = if (hitsRemaining > 0 && totalRemoved < maxPieces)
          strikeCandidates map (name => s"strike:$name" -> s"Remove insurgents in $name")
        else
          Nil

        val choices = (topChoices:::strikeChoices) :+ ("finished" -> "Finished with Air Strike activity")

        println(s"\n${amountOf(strikeSpaces.size, "space")} of $maxSpaces selected for Air Strike")
        println(separator())
        wrap("", strikeSpaces.toList, showNone = false) foreach println

        if (!oriskany) {
          if (hasDegraded)
            println("\nYou have already degraded the trail")
          else
            println("\nYou have not yet degraded the trail")
        }

        val canRemove = (maxPieces - totalRemoved) min hitsRemaining
        if (choices.size == 1) {
          val degradeMsg = if (hasDegraded) "" else " and you cannot degrade the trail"
          println(s"\n ${amountOf(hitsRemaining, "hit")} remaining")
          println(s"There are no strike targets $degradeMsg")
        }
        else
          askMenu(choices, s"\nAir Strike: (${amountOf(hitsRemaining, "hit")} remaining, remove up to ${amountOf(canRemove, "piece")})").head match {
            case "select" =>
              askCandidateOrBlank("\nAir Strike in which space:", selectCandidates) foreach { name =>
                strikeSpaces += name
              }
              nextAirStrikeAction(hitsRemaining, hasDegraded)

            case "degrade" =>
              performDegrade()
              nextAirStrikeAction(hitsRemaining - 2, true)

            case StrikeOpt(name) =>
              val numRemoved = performStrike(name)
              totalRemoved += numRemoved

              nextAirStrikeAction(hitsRemaining - numRemoved, hasDegraded)

            case _ => // finished
          }
      }
    }

    val notes = List(
      noteIf(game.inMonsoon, "Max 2 spaces in Monsoon"),
      noteIf(momentumInPlay(Mo_TyphoonKate), s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
      noteIf(momentumInPlay(Mo_WildWeasels), s"Air Strike my EITHER degrade the trail or remove only 1 piece [Momentum: $Mo_WildWeasels]"),
      noteIf(oriskany, s"Trail change is prohibited [Momentum: $Mo_Oriskany]"),
      noteIf(adsid, s"-6 NVA resources at any trail change [Momentum: $Mo_ADSID]"),
      noteIf(capabilityInPlay(TopGun_Unshaded),s"Degrading the trail shifts two boxes [$TopGun_Unshaded]"),
      noteIf(capabilityInPlay(TopGun_Shaded), s"Degrading the requires a die roll of 4-6 [$TopGun_Shaded]"),
      noteIf(capabilityInPlay(ArcLight_Unshaded), s"May Air Strike in one Province without COIN pieces [$ArcLight_Unshaded]"),
      noteIf(capabilityInPlay(ArcLight_Shaded), s"Spaces removing more than 1 piece shift 2 levels toward Active Opposition [$ArcLight_Shaded]"),
      noteIf(capabilityInPlay(LaserGuidedBombs_Unshaded), s"Spaces removing only 1 piece do not shift toward Active Opposition [$LaserGuidedBombs_Unshaded]"),
      noteIf(capabilityInPlay(LaserGuidedBombs_Shaded), s"Remove no more than 2 pieces total [$LaserGuidedBombs_Shaded]"),
      noteIf(capabilityInPlay(AAA_Shaded), s"Cannot degrade the trail below 2 [$AAA_Shaded]"),
      noteIf(migs_shaded, s"If trail degraded, US removes 1 Available Troop to Casualties [$MiGs_Shaded]"),
      noteIf(capabilityInPlay(SA2s_Unshaded), s"When trail degraded, US removes 1 NVA piece outside the South [$SA2s_Unshaded]")
    ).flatten

    // Start of Air Lift Special Activity
    // ------------------------------------
    logSAChoice(US, AirStrike, notes)
    if (params.strikeHits.nonEmpty)
      log(s"Number of hits = $maxHits")
    else
      log(s"Die roll to determine the number of hits = $maxHits")

    nextAirStrikeAction(maxHits, false)
  }

  // ARVN special activity
  // Mo_TyphoonKate           - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // MandateOfHeaven_Unshaded - 1 Govern space may transfer Aid to Patronage without shifting support
  // MandateOfHeaven_Shaded   - ARVN Govern is maximum 1 space
  // RVN_Leader_YoungTurks    - Each ARVN Govern adds +2 Patronage
  def doGovern(params: Params): Unit = {
    val young_turks     = isRVNLeader(RVN_Leader_YoungTurks)
    val kate            = momentumInPlay(Mo_TyphoonKate)
    var heaven_unshaded_used = false
    val heaven_unshaded = capabilityInPlay(MandateOfHeaven_Unshaded)
    val heaven_shaded   = capabilityInPlay(MandateOfHeaven_Shaded)
    val maxSpaces       = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                          else if (kate || heaven_shaded) 1 else 2

    def isCandidate(sp: Space) = {
        params.spaceAllowed(sp.name) &&
        !sp.isLoC            &&
        sp.coinControlled    &&
        sp.population > 0    &&
        sp.support > Neutral &&
        sp.name != Saigon    &&
        !Special.trainingSpaces(sp.name)
    }

    def nextGovernAction(): Unit = {
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect  = Special.selectedSpaces.size < maxSpaces && candidates.nonEmpty
      val choices = List(
        choice(canSelect, "select",   "Select a Govern space"),
        choice(true,      "finished", "Finished with Govern activity")
      ).flatten

      println(s"\n${amountOf(Special.selectedSpaces.size, "space")} of $maxSpaces selected for Govern")
      println(separator())
      wrap("", Special.selectedSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nGovern:").head match {
        case "select" =>
          askCandidateOrBlank("\nGovern in which space: ", candidates) foreach { name =>
            val sp           = game.getSpace(name)
            val canPatronage = sp.population > 0 && sp.pieces.totalOf(ARVNCubes) > sp.pieces.totalOf(USTroops)

            val action = if (!canPatronage)
              "gain_aid"
            else {
              val choices = List(
                "xfer_patronage" -> s"Transfer population value from Aid to Patronage (${sp.population})",
                "gain_aid"       -> s"Add 3 times population value to Aid (${3 * sp.population})"
              )

              askMenu(choices, s"\nSelect Govern action for $name:").head
            }

            Special.selectedSpaces = Special.selectedSpaces + name

            log(s"\nARVN Governs in $name (population: ${sp.population})")
            log(separator())
            loggingPointsChanges {
              if (action == "xfer_patronage") {
                val num = (game.usAid min sp.population)
                log("Transfer population value from Aid to Patronage")
                decreaseUsAid(num)
                increasePatronage(num)

                val skipShift = heaven_unshaded && !heaven_unshaded_used &&
                      askYorN(s"Do you wish to use [$MandateOfHeaven_Unshaded] to avoid shifting support? (y/n) ")

                if (skipShift) {
                  heaven_unshaded_used = true
                  log(s"No shift in support. Used [$MandateOfHeaven_Unshaded]")
                }
                else
                    decreaseSupport(name, 1)
              }
              else if (action == "gain_aid") {
                log("Add 3 times population value to Aid")
                increaseUsAid(sp.population * 3)
              }

              //  Young turks always applies if in play
              if (young_turks) {
                log(s"$RVN_Leader_YoungTurks leader effect triggers")
                increasePatronage(2)
              }
            }
          }
          nextGovernAction()

        case _ =>
      }
    }

    val notes = List(
      noteIf(young_turks,     s"Each ARVN Govern add +2 Patronage [Leader $RVN_Leader_YoungTurks]"),
      noteIf(kate,            s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
      noteIf(heaven_unshaded, s"1 Govern space may transfer Aid to Patronage with no shift in support [$MandateOfHeaven_Unshaded]"),
      noteIf(heaven_shaded,   s"ARVN Govern is maximum 1 space [$MandateOfHeaven_Shaded]")
    ).flatten

    // Start of Govern Special Activity
    // ------------------------------------
    logSAChoice(ARVN, Govern, notes)

    nextGovernAction()
  }

  // ARVN special activity
  // Mo_TyphoonKate          - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // ArmoredCavalry_Unshaded - ARVN in 1 Transport destination (AFTER OPS) may free assault
  // ArmoredCavalry_Shaded   - Transport Rangers only (NO TROOPS)
  // RVN_Leader_NguyenKhanh  - Transport uses max 1 LOC space
  def doTransport(params: Params): Unit = {
    val nguyen_khanh     = isRVNLeader(RVN_Leader_NguyenKhanh)
    val armored_unshaded = capabilityInPlay(ArmoredCavalry_Unshaded)
    val armored_shaded   = capabilityInPlay(ArmoredCavalry_Shaded)
    val pieceTypes       = if (armored_shaded) Rangers else ARVNTroops::Rangers
    val piecesName       = if (armored_shaded) "Rangers" else "Troops/Rangers"
    val srcPrompt        = s"\nMove $piecesName out of which space: "
    val srcCandidates    = spaceNames(game.spaces filter (_.pieces.has(pieceTypes)))

    val notes = List(
      noteIf(nguyen_khanh,     s"Transport used max 1 LOC space [Leader $RVN_Leader_NguyenKhanh]"),
      noteIf(armored_unshaded, s"In one Transport destination may Free Assault (after Ops) [$ArmoredCavalry_Unshaded]"),
      noteIf(armored_shaded,   s"Transport Rangers only (NO Troops) [$ArmoredCavalry_Shaded]")
    ).flatten

    // Start of Transport Special Activity
    // ------------------------------------

    logSAChoice(ARVN, Transport, notes)

    val srcName        = askCandidate(srcPrompt, srcCandidates)
    val eligible       = game.getSpace(srcName).pieces.only(pieceTypes)
    val maxMovers      = eligible.total min 6
    val numMovers      = askInt(s"\nMove how many $piecesName out of $srcName", 0, maxMovers)
    if (numMovers > 0) {
      var movingPieces   = askPieces(eligible, numMovers, prompt = Some(s"Selecting $piecesName to Transport"))
      val destCandidates = getTransportDestinations(srcName).toList.sorted

      def nextDestination(movers: Pieces, candidates: Seq[String]): Unit = {
        if (movers.nonEmpty) {
          println(s"\n${amountOf(movers.total, "piece")} remaining to move")
          val destName = askCandidate(s"\nSelect a Transport destination space: ", candidates)
          val minNum   = if (candidates.size == 1) movers.total else 1
          val maxNum   = movers.total
          val num      = askInt(s"Move how many $piecesName to $destName", minNum, maxNum)
          val moveNow  = askPieces(movers, num)
          Special.transportDestinations = Special.transportDestinations + destName
          movePieces(moveNow, srcName, destName)
          nextDestination(movers - moveNow, candidates filterNot (_ == destName))
        }
      }

      nextDestination(movingPieces, destCandidates)
    }

    // Flip all rangers underground
    for (sp <- game.spaces; if sp.pieces.has(Rangers_A))
      hidePieces(sp.name, sp.pieces.only(Rangers_A))
  }



  // ARVN special activity
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  def doRaid(params: Params): Unit = {
    val kate      = momentumInPlay(Mo_TyphoonKate)
    val frozen    = new MovingGroups()
    val maxSpaces = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (kate) 1 else 2
    var raidSpaces = Set.empty[String]

    def canMoveFrom(name: String) = {
      val sp = game.getSpace(name)
      sp.pieces.only(Rangers) - frozen(name)
    }

    def adjWithRangers(name: String) = (getAdjacent(name) filter (n => canMoveFrom(n).nonEmpty)).toList.sorted

    def moveRangersInto(name: String): Unit = {
      val choices = (adjWithRangers(name) map (n => n -> n)) :+ ("finished" -> s"Finished moving Rangers into $name")

      askMenu(choices, s"\nMove Rangers into $name from:").head match {
        case "finished" =>
        case fromName =>
          val eligible = canMoveFrom(fromName)
          val num      = askInt(s"Move how many Rangers from $fromName", 0, eligible.total)
          val movers   = askPieces(eligible, num, prompt = Some("Moving Rangers to Raid"))
          movePieces(movers, fromName, name)
          frozen.add(name, movers)
          moveRangersInto(name)
      }
    }

    def nextSelectAction(): Unit = {
      val isCandidate = (sp: Space) => {
        params.spaceAllowed(sp.name) &&
        !raidSpaces(sp.name) &&
        !sp.isNorthVietnam &&
        (sp.pieces.has(Rangers) || adjWithRangers(sp.name).nonEmpty)
      }
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect  = raidSpaces.size < maxSpaces && candidates.nonEmpty
      val choices    = List(
        choice(canSelect, "select",   "Select a Raid space"),
        choice(true,      "finished", "Finished selecting Raid spaces")
      ).flatten

      println(s"\n${amountOf(raidSpaces.size, "space")} of $maxSpaces selected for Raid")
      println(separator())
      wrap("", raidSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nRaid:").head match {
        case "select" =>
          askCandidateOrBlank("Raid in which space: ", candidates) foreach { name =>
            raidSpaces = raidSpaces + name
            log(s"\nARVN Raids $name")
            if (adjWithRangers(name).nonEmpty && askYorN(s"Do you wish to move adjacent Rangers into $name? (y/n) "))
              moveRangersInto(name)
          }
          nextSelectAction()

        case _ =>
      }
    }

    // For each raid space with at least one underground Ranger and any enemy pieces
    // that can be killed, ask if the user wishes to do so
    def nextRaidAction(raided: Set[String]): Unit = {
      val candidates = (raidSpaces -- raided).toList filter { name =>
        val sp = game.getSpace(name)
        sp.pieces.has(Rangers_U) && (sp.pieces.has(InsurgentForces) || sp.pieces.has(InsurgentNonTunnels))
      }
      val choices = (candidates map (n => n -> s"Remove enemy pieces in $n")) :+ ("finished" -> "Finished with Raid Special Activity")
      askMenu(choices, "\nRaid: ").head match {
        case "finished" =>
        case name =>
          val sp = game.getSpace(name)
          val eligibleForces = sp.pieces.only(InsurgentForces)
          val eligibleBases  = sp.pieces.only(InsurgentNonTunnels)
          val numForces = 2 min eligibleForces.total
          val killedForces = askPieces(eligibleForces, numForces, prompt = Some("Removing enemy forces"))
          val killedBases = if (numForces < 2  && eligibleBases.nonEmpty) {
            val numBases = (2 - numForces) min eligibleBases.total
            askPieces(eligibleBases, numBases, prompt = Some("Removing enemy bases"))
          }
          else
            Pieces()

          log(s"\nARVN uses an underground ranger to remove enemy pieces in $name")
          log(separator())
          revealPieces(name, Pieces(rangers_U = 1))
          removeToAvailable(name, killedForces + killedBases)
          nextRaidAction(raided + name)
      }
    }

    // Start of Transport Special Activity
    // ------------------------------------

    val notes = List(
      noteIf(kate, s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]")
    ).flatten

    logSAChoice(ARVN, Raid, notes)

    nextSelectAction()
    nextRaidAction(Set.empty)
  }


  // NVA special activity
  // Mo_TyphoonKate     - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // Mo_McNamaraLine    - prohibits infiltrate
  // Mo_559TransportGrp - Infiltrate is max 1 space
  def doInfiltrate(params: Params): Unit = {
    val NVABases         = NVABase::NVATunnel::Nil
    val kate             = momentumInPlay(Mo_TyphoonKate)
    val transportGrp     = momentumInPlay(Mo_559TransportGrp)
    val maxSpaces        = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                           else if (kate || transportGrp) 1 else 2
    var infiltrateSpaces = Set.empty[String]

    val canSuborn = (sp: Space) =>
      sp.pieces.totalOf(NVAPieces) > sp.pieces.totalOf(VCPieces) &&
      (sp.pieces.has(VCPieces) || sp.support < Neutral)

    val isCandidate = (sp: Space) =>
      params.spaceAllowed(sp.name) &&
      !infiltrateSpaces(sp.name) &&
      sp.pieces.has(NVABases) || canSuborn(sp)


    def placeTroops(name: String): Unit = {
      def sp            = game.getSpace(name)
      val toPlaceTroops = game.piecesToPlace.totalOf(NVATroops)
      val trailNum      = game.trail + sp.pieces.totalOf(NVABases)
      val maxGuerrillas = sp.pieces.totalOf(NVAGuerrillas)

      log(s"\nNVA Infiltrates in $name")
      log(separator())
      loggingControlChanges {
        val trailTroops = askInt("Place how many troops for Trail + Bases", 0, trailNum min toPlaceTroops)
        ensurePieceTypeAvailable(NVATroops, trailTroops)
        placePieces(name, Pieces(nvaTroops = trailTroops))

        val numGuerrillas = askInt("\nReplace how many NVA guerrillas with troops", 0, maxGuerrillas)
        val guerrillas    = askPieces(sp.pieces, numGuerrillas,  NVAGuerrillas, prompt = Some("Selecting guerrilas to replace with troops"))
        removeToAvailable(name, guerrillas)
        ensurePieceTypeAvailable(NVATroops, guerrillas.total)
        placePieces(name, Pieces(nvaTroops = guerrillas.total))
      }
    }

    def subornVC(name: String): Unit = {
      val sp = game.getSpace(name)
      log(s"\nNVA Infiltrates in $name")
      log(separator())
      if (sp.support < Neutral)
        increaseSupport(name, 1)

      if (askYorN("\nDo you wish to replace one VC piece with its NVA counterpart? (y/n) "))
        loggingControlChanges {
          val vcPiece = askPieces(sp.pieces, 1, VCPieces,  Some("Selecting a VC piece to replace"))
          val nvaType = getInsurgentCounterPart(vcPiece.explode().head)

          if (vcPiece.has(VCBase) || vcPiece.has(VCTunnel)) {
            removeToAvailable(name, vcPiece)
            ensurePieceTypeAvailable(NVABase, 1)
            placePieces(name, Pieces(nvaBases = 1))
            if (nvaType == NVATunnel)
              addTunnelMarker(name, Pieces(nvaBases = 1))
          }
          else {
            removeToAvailable(name, vcPiece)
            ensurePieceTypeAvailable(nvaType, 1)
            placePieces(name, Pieces().set(1, nvaType))
          }
        }

    }

    def infiltrateSpace(name: String): Unit = {
      val sp        = game.getSpace(name)
      val canPlace  = sp.pieces.has(NVABases)
      val choices   = List(
        choice(canPlace,      "place",  "Place NVA Troops"),
        choice(canSuborn(sp), "suborn", "Shift Opposition toward Neutral/Replace VC piece")
      ).flatten

      askMenu(choices, s"\nInfiltrate in $name").head match {
        case "place" => placeTroops(name)
        case _       => subornVC(name)
      }
    }

    def nextInfiltrateAction(): Unit = {
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect  = infiltrateSpaces.size < maxSpaces && candidates.nonEmpty
      val choices    = List(
        choice(canSelect, "select",   "Select a space to Infiltrate"),
        choice(true,      "finished", "Finished with Infiltrate special activity")
      ).flatten

      println(s"\n${amountOf(infiltrateSpaces.size, "space")} of $maxSpaces selected for Infiltrate")
      println(separator())
      wrap("", infiltrateSpaces.toList, showNone = false) foreach println


      askMenu(choices, "\nInfiltrate:").head match {
        case "select" =>
          askCandidateOrBlank("\nInfiltrate in which space: ", candidates) foreach { name =>
            infiltrateSpace(name)
            infiltrateSpaces = infiltrateSpaces + name
          }
          nextInfiltrateAction()

        case _ =>
      }
    }


    // Start of Infiltrate Special Activity
    // ------------------------------------

    val notes = List(
      noteIf(kate,         s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
      noteIf(transportGrp, s"Infiltrate is max 1 space [Momentum: $Mo_559TransportGrp]")
    ).flatten

    logSAChoice(NVA, Infiltrate, notes)

    nextInfiltrateAction()
  }

  // NVA special activity
  // Mo_TyphoonKate         - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // LongRangeGuns_Unshaded - NVA Bombard is max 1 space
  // LongRangeGuns_Shaded   - NVA Bombard is max 3 spaces
  def doBombard(params: Params): Unit = {
    val longRange_unshaded = capabilityInPlay(LongRangeGuns_Unshaded)
    val longRange_shaded   = capabilityInPlay(LongRangeGuns_Shaded)
    var bombardSpaces      = Set.empty[String]
    val maxSpaces = if      (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (longRange_unshaded) 1
                    else if (longRange_shaded)   3
                    else                         2

    val isCandidate = (sp: Space) => {
      val enemyCondition = sp.pieces.totalOf(CoinTroops) > 2 ||
                           (sp.pieces.has(CoinTroops) && sp.pieces.has(CoinBases))
      lazy val hasAdjacentTroops = getAdjacent(sp.name) exists { name =>
        game.getSpace(name).pieces.totalOf(NVATroops) > 2
      }

      params.spaceAllowed(sp.name) &&
      !bombardSpaces(sp.name) &&
      enemyCondition && 
      (sp.pieces.totalOf(NVATroops) > 2 || hasAdjacentTroops)
    }

    def bombardSpace(name: String): Unit = {
      val sp = game.getSpace(name)

      log(s"\nNVA Bombards in $name")
      log(separator())

      val toRemove  = askPieces(sp.pieces, 1, CoinTroops, Some("Bombarding a Coin Troop"))
      removePieces(name, toRemove)
    }

    def nextBombardAction(): Unit = {
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect  = bombardSpaces.size < maxSpaces && candidates.nonEmpty
      val choices    = List(
        choice(canSelect, "select",   "Select a space to Bombard"),
        choice(true,      "finished", "Finished with Bombard special activity")
      ).flatten

      println(s"\n${amountOf(bombardSpaces.size, "space")} of $maxSpaces selected for Bombard")
      println(separator())
      wrap("", bombardSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nBombard:").head match {
        case "select" =>
          askCandidateOrBlank("\nBombard in which space: ", candidates) foreach { name =>
            bombardSpace(name)
            bombardSpaces = bombardSpaces + name
          }
        nextBombardAction()

        case _ =>
      }
    }

    // Start of Bombard Special Activity
    // ------------------------------------

    val notes = List(
      noteIf(longRange_unshaded, s"NVA Bombard is max 1 space [$LongRangeGuns_Unshaded]"),
      noteIf(longRange_shaded,   s"NVA Bombard is max 3 space [$LongRangeGuns_Unshaded]")
    ).flatten

    logSAChoice(NVA, Bombard, notes)

    nextBombardAction()
  }


  // NVA/VC special activity
  // The ambush activity is noted in the Special object.
  // See the performAmbush() function for carrying out an ambush
  // in a given space
  //
  // BoobyTraps_Unshaded - Max 1 ambush space (instead of 2)
  // Mo_Claymores prohibits ambush
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  def doAmbush(faction: Faction, params: Params): Unit = {
    logSAChoice(faction, Ambush)

    val notes = List(
      noteIf(momentumInPlay(Mo_TyphoonKate),s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]"),
      noteIf(capabilityInPlay(BoobyTraps_Unshaded), s"You may ambush in only one space [$BoobyTraps_Unshaded]"),
      noteIf(faction == VC && capabilityInPlay(MainForceBns_Shaded),s"In one ambush space VC may remove 2 pieces [$MainForceBns_Shaded]"),
      noteIf(faction == NVA && capabilityInPlay(PT76_Unshaded),s"Each NVA ambush space first remove 1 NVA troop [$PT76_Unshaded]"),
      noteIf(true, "You will be prompted to use ambush at the appropriate time(s) during your operation.")
    ).flatten

    for (note <- notes)
      log(note)

    // Set the ambushing flag.  The user will be prompted to ambush in the current
    // action: Attack, Patrol
    Special.ambushing = true
  }

  // VC special activity
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  def doTax(params: Params): Unit = {
    val kate      = momentumInPlay(Mo_TyphoonKate)
    var taxSpaces = Set.empty[String]
    val maxSpaces = if (params.maxSpaces.nonEmpty) params.maxSpaces.get
                    else if (kate) 1 else 4

    val isCandidate = (sp: Space) => if (sp.isLoC)
      params.spaceAllowed(sp.name) &&
      !taxSpaces(sp.name) && sp.printedEconValue > 0 &&
      sp.pieces.has(VCGuerrillas_U)
    else
      params.spaceAllowed(sp.name) &&
      !taxSpaces(sp.name) &&
      sp.population > 0 &&
      sp.pieces.has(VCGuerrillas_U) &&
      !sp.coinControlled

    def taxSpace(name: String): Unit = {
      val sp  = game.getSpace(name)
      val num = if (sp.isLoC) sp.printedEconValue else 2 * sp.population
      log(s"\nVC Taxes in $name")
      log(separator())
      revealPieces(name, Pieces(vcGuerrillas_U = 1))
      increaseResources(VC, num)
      if (!sp.isLoC && sp.support != ActiveSupport)
        increaseSupport(name, 1)
    }

    def nextTaxAction(): Unit = {
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect  = taxSpaces.size < maxSpaces && candidates.nonEmpty
      val choices    = List(
        choice(canSelect, "select",   "Select a space to Tax"),
        choice(true,      "finished", "Finished with Tax special activity")
      ).flatten

      println(s"\n${amountOf(taxSpaces.size, "space")} of $maxSpaces selected for Tax")
      println(separator())
      wrap("", taxSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nTax:").head match {
        case "select" =>
          askCandidateOrBlank("\nTax in which space: ", candidates) foreach { name =>
            taxSpace(name)
            taxSpaces = taxSpaces + name
          }
        nextTaxAction()

        case _ =>
      }
    }

    // Start of Tax Special Activity
    // ------------------------------------

    val notes = List(
      noteIf(kate, s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]")
    ).flatten

    logSAChoice(VC, Tax, notes)

    nextTaxAction()
  }

  // VC special activity
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  def doSubvert(params: Params): Unit = {
    val kate          = momentumInPlay(Mo_TyphoonKate)
    val maxSpaces     = if (kate) 1 else 2
    var subvertSpaces = Set.empty[String]
    var cubesRemoved  = 0

    val isCandidate = (sp: Space) =>
      params.spaceAllowed(sp.name) &&
      !subvertSpaces(sp.name) &&
      sp.pieces.has(VCGuerrillas_U) &&
      sp.pieces.has(ARVNCubes)

    def removeCubes(name: String): Unit = {
      val sp       =  game.getSpace(name)
      val eligible = sp.pieces.only(ARVNCubes)
      val num      = eligible.total min 2
      val toRemove = askPieces(eligible, num)

      log(s"\nVC Subverts in $name")
      log(separator())
      removeToAvailable(name, toRemove)
      cubesRemoved += toRemove.total
    }

    def replaceCube(name: String): Unit = {
      val sp       =  game.getSpace(name)
      val eligible = sp.pieces.only(ARVNCubes)
      val toRemove = askPieces(eligible, 1, prompt = Some("Select ARVN cube to replace"))

      log(s"\nVC Subverts in $name")
      log(separator())
      removeToAvailable(name, toRemove)
      ensurePieceTypeAvailable(VCGuerrillas_U, 1)
      placePieces(name, Pieces().set(1, VCGuerrillas_U))
      cubesRemoved += toRemove.total
    }

    def subvertSpace(name: String): Unit = {
        val sp = game.getSpace(name)
        val choices = List(
          "remove"  -> "Remove ARVN Cubes",
          "replace" -> "Replace 1 ARVN Cube with a VC Guerrilla"
        )

        askMenu(choices, s"\nSubvert in $name").head match {
          case "remove" => removeCubes(name)
          case _        => replaceCube(name)
        }
    }

    def nextSubvertAction(): Unit = {
      val candidates = spaceNames(game.spaces filter isCandidate)
      val canSelect  = subvertSpaces.size < maxSpaces && candidates.nonEmpty
      val choices    = List(
        choice(canSelect, "select",   "Select a space to Subvert"),
        choice(true,      "finished", "Finished with Subvert special activity")
      ).flatten

      println(s"\n${amountOf(subvertSpaces.size, "space")} of $maxSpaces selected for Subvert")
      println(separator())
      wrap("", subvertSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nSubvert:").head match {
        case "select" =>
          askCandidateOrBlank("\nSubvert in which space: ", candidates) foreach { name =>
            subvertSpace(name)
            subvertSpaces = subvertSpaces + name
          }
        nextSubvertAction()

        case _ =>
          log(s"\nTotal ARVN pieces removed by the Subvert activity: $cubesRemoved")
          decreasePatronage(cubesRemoved / 2)
      }
    }


    // Start of Subvert Special Activity
    // ------------------------------------

    val notes = List(
      noteIf(kate, s"All special activities are max 1 space [Momentum: $Mo_TyphoonKate]")
    ).flatten

    logSAChoice(VC, Subvert, notes)

    nextSubvertAction()

  }

  // val Mo_TyphoonKate       = "#115 Typhoon Kate"            // Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // val Mo_Claymores         = "#17 Claymores"                // Unshaded (prohibits ambush, affect guerrilla march)
  // val Mo_Medevac_Shaded    = s"$Medevac_prefix (shaded)"    // (prohibits air lift)
  // val Mo_RollingThunder    = "#10 Rolling Thunder"          // Shaded   (prohibits air strike)
  // val Mo_DaNang            = "#22 Da Nang"                  // Shaded (prohibits air strike)
  // val Mo_McNamaraLine      = "#38 McNamara Line"            // Single event (prohibits infiltrate, prohibits trail improvement by rally)  val Mo_BombingPause      = "#41 Bombing Pause"            // Single event (prohibits air strike)
  def executeSpecialActivity(faction: Faction, params: Params, activities: List[SpecialActivity]): Unit = {
    val momentumRestrictions = prohibitedSpecialActivities(activities)
    val prohibited           = momentumRestrictions.map(_._2).foldLeft(Set.empty[SpecialActivity]) { (set, list) => set ++ list }

    val notes = for ((mo, a) <- momentumRestrictions)
      yield s"Momentum: $mo prohibits ${andList(a)}"

    if (notes.nonEmpty) {
      println("\nNotes:")
      println(separator())
      for (note <- notes)
        log(note)
    }

    val choices: List[(Option[SpecialActivity], String)] =
      (activities filterNot prohibited.apply map (a => Some(a) -> a.toString)) :+
      (None -> "Do not perform a Speical Activitiy now")

    if (choices.size == 1) {
      // If all activities have been prohibited
      // mark the specal as completed so we do not
      // continue to ask if the user want to do a special
      // activity.
      println("\nThere are no Special Activities available")
      Special.completed()
    }
    else {
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
  }


  def executeEvent(faction: Faction): Unit = {

    val card = deck(game.currentCard)

    if (card.dual) {
      val choices = List("Unshaded", "Shaded")
      askSimpleMenu(choices, "\nExecute which part of the event:").head match {
        case "Unshaded" =>
          log(s"\n$faction executes the unshaded Event: ${card.name}")
          log(separator())
          card.executeUnshaded(faction)
          case _ =>
          log(s"\n$faction executes the shaded Event: ${card.name}")
          log(separator())
          card.executeShaded(faction)
      }
    }
    else { // Non dual events are always use the unshaded function
      log(s"\n$faction executes the Event: ${card.name}")
      log(separator())
      card.executeUnshaded(faction)
    }
  }

  def executeOp(faction: Faction, params: Params = Params()): Unit = {
    Special.init(params)
    pt76_shaded_used = false

    faction match {
      case US  | ARVN => executeCoinOp(faction, params)
      case NVA | VC   => executeInsurgentOp(faction, params)
    }
  }

  def executeCoinOp(faction: Faction, params: Params): Unit = {

    val landsdale = faction == US && momentumInPlay(Mo_GeneralLansdale)
    val availOps = CoinOp.ALL filter {
      case Train   => true
      case Patrol  => true
      case Sweep   => !game.inMonsoon
      case Assault => !landsdale
    }
    val choices = availOps map (op => op -> op.toString)

    val notes = List(
      noteIf(game.inMonsoon, s"Sweep is prohibited [Not allowed in Monsoon]"),
      noteIf(landsdale,      s"US Assault is prohibited [Momentum: ${Mo_GeneralLansdale}]")
    ).flatten

    if (notes.nonEmpty) {
      println("Notes:")
      println(separator())
      for (note <- notes)
        log(note)
    }

    askMenu(choices, "\nChoose operation:").head match {
      case Train   => executeTrain(faction, params)
      case Patrol  => executePatrol(faction, params)
      case Sweep   => executeSweep(faction, params)
      case Assault => executeAssault(faction, params)
    }
  }

  def executeInsurgentOp(faction: Faction, params: Params): Unit = {
    val availOps = InsurgentOp.ALL filter {
      case Rally  => true
      case March  => !game.inMonsoon
      case Attack => true
      case Terror => true
    }
    val choices = availOps map (op => op -> op.toString)

    val notes = List(
      noteIf(game.inMonsoon, s"March is prohibited [Not allowed in Monsoon]")
    ).flatten

    if (notes.nonEmpty) {
      println("Notes:")
      println(separator())
      for (note <- notes)
        log(note)
    }

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
  // RVN_Leader_DuongVanMinh - Each ARVN Train operation adds +5 bonus Aid
  // RVN_Leader_NguyenCaoKy - US/ARVN pacification costs 4 resources per Terror/Level
  def executeTrain(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == US)
      Advise::AirLift::AirStrike::Nil
    else
      Govern::Transport::Nil
    val canPlaceExtraPolice = faction == US && capabilityInPlay(CombActionPlatoons_Unshaded)
    var placedExtraPolice   = false // only if CombActionPlatoons_Unshaded in play
    val maxTrainSpaces      = params.maxSpaces getOrElse NO_LIMIT
    val maxPacifySpaces     = if (faction == US && capabilityInPlay(CORDS_Unshaded)) (2 min maxTrainSpaces) else 1
    val maxPacifyLevel      = if (faction == US && capabilityInPlay(CORDS_Shaded)) PassiveSupport else ActiveSupport
    var selectedSpaces      = List.empty[String]
    var arvnPlacedIn        = List.empty[String]
    var pacifySpaces        = List.empty[String]
    val duongVanMinh        = faction == ARVN && isRVNLeader(RVN_Leader_DuongVanMinh)
    val nguyenCaoKy         = isRVNLeader(RVN_Leader_NguyenCaoKy)
    val isCandidate = (sp: Space) => {
      val valid = if (faction == ARVN) !sp.isLoC && !sp.nvaControlled
                  else                 !sp.isLoC && sp.pieces.has(USPieces)

      valid &&                                   // Valid for faction
      !sp.isNorthVietnam &&                      // Never North Vietnam
      params.spaceAllowed(sp.name) &&            // If event limits command to certain spaces
      !selectedSpaces.contains(sp.name) &&       // Not already selected
      !Special.selectedSpaces.contains(sp.name)  // Not selected for Advise/Govern Special Activity
    }

    def canSpecial = Special.allowed

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
          log(s"US places 1 ARVN Police with US Troops [$CombActionPlatoons_Unshaded]")
          ensurePieceTypeAvailable(ARVNPolice, 1)
          placePieces(name, Pieces().set(1, ARVNPolice))

        case "irregulars" =>
          val num = askInt("\nPlace how many Irregulars", 0, 2)
          ensurePieceTypeAvailable(Irregulars_U, num)
          placePieces(name, Pieces().set(num, Irregulars_U))

        case "rangers" =>
          val num = askInt("\nPlace how many Rangers", 0, 2)
          if (num > 0) {
            log()
            if (!params.free)
              decreaseResources(ARVN, 3)
            ensurePieceTypeAvailable(Rangers_U, num)
            arvnPlacedIn = name :: arvnPlacedIn
            placePieces(name, Pieces().set(num, Rangers_U))
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
        choice(canSelect,  "select",   "Select a space to Train"),
        choice(canSpecial, "special",  "Perform a Special Activity"),
        choice(true,       "finished", "Finished selecting spaces")
      ).flatten

      println(s"\n${amountOf(selectedSpaces.size, "space")} selected for Training")
      println(separator())
      wrap("", selectedSpaces, showNone = false) foreach println

      if (candidates.isEmpty) {
        val more = if (selectedSpaces.nonEmpty) " more" else ""
        println(s"\nThere are no${more} spaces eligible for Training")
      }

      askMenu(choices, "\nTraining:").head match {
        case "select" =>
          askCandidateOrBlank("\nTrain in which space: ", candidates) foreach { name =>
            log(s"\n$faction selects $name for Training")
            promptToAddForces(name)
            selectedSpaces = selectedSpaces :+ name
            Special.trainingSpaces = selectedSpaces.toSet
          }
          selectTrainSpace()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
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
           choice(canSpecial,                "special", "Perform a Special Activity"),
           choice(true,                      "none",    "Finished with Train operation")
        ).flatten

        askMenu(choices, "\nChoose final Train action:").head match {
          case "pacify" =>
            askCandidateOrBlank("\nPacify in which space: ", pacifyCandidates) foreach { name =>
            if (pacifySpace(name, faction, coupRound = false))
              pacifySpaces = name :: pacifySpaces
            }

            if (pacifySpaces.size < maxPacifySpaces)
              promptFinalAction()

          case "base" =>
            loggingControlChanges {
              askCandidateOrBlank("\nPlace an ARVN base in which space: ", baseCandidates) match {
                case None       => promptFinalAction()
                case Some(name) =>
                  val sp    = game.getSpace(name)
                  val cubes = askPieces(sp.pieces, 3, ARVNCubes,  Some("Removing ARVN cubes to replace with base"))
                  removeToAvailable(name, cubes)
                  ensurePieceTypeAvailable(ARVNBase, 1)
                  placePieces(name, Pieces().set(1, ARVNBase))
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
            executeSpecialActivity(faction, params, specialActivities)
            promptFinalAction()

          case _ =>
        }
      }
    }


    val notes = List(
      noteIf(nguyenCaoKy,                      s"Pacification costs 4 resources per terror/level [Leader: $RVN_Leader_NguyenCaoKy]"),
      noteIf(canPlaceExtraPolice,              s"May place 1 ARVN Police in 1 training space with US Troops [$CombActionPlatoons_Unshaded]"),
      noteIf(maxPacifySpaces == 2,             s"May pactify in 2 spaces [$CORDS_Unshaded]"),
      noteIf(maxPacifyLevel == PassiveSupport, s"May not Pacify to Active Support [$CORDS_Shaded]")
    ).flatten

    logOpChoice(faction, Train, notes)

    if (duongVanMinh) {
      if (isRVNLeader(RVN_Leader_DuongVanMinh)) {
        log(s"\nLeader: $RVN_Leader_DuongVanMinh effect triggers")
        log(separator())
        increaseUsAid(5)
      }
    }

    selectTrainSpace()
    if (selectedSpaces.nonEmpty)
      promptFinalAction() // Pacify, Place base, Xfer Patronage to ARVN resources


    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)

    // If Transport special activity was used,  then Armored Cavalry (unshaded)
    // allows ARVN to free assault in one Transport destination
    armoredCavalryAssault()
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
      val onNetwork = sp.isLoC || sp.isCity || getAdjacentLOCs(sp.name).nonEmpty || getAdjacentCities(sp.name).nonEmpty
      onNetwork && patrolCubes(sp).nonEmpty && reachesLimOpDest
    }

    def canSpecial = Special.allowed

    def selectCubesToMove(): Unit = {
      val srcCandidates = spaceNames(game.spaces filter isPatrolSource)
      srcCandidates.nonEmpty
      val choices = List(
        choice(srcCandidates.nonEmpty,  "move",     "Move cubes"),
        choice(canSpecial,              "special",  "Perform a Special Activity"),
        choice(true,                    "finished", "Finished moving cubes")
      ).flatten

      if (srcCandidates.isEmpty)
        println(s"\nThere are no spaces with cube eligible to move on patrol")

      askMenu(choices, "\nMoving Patrol cubes:").head match {
        case "move" =>
          askCandidateOrBlank("\nMove cubes out of which space: ", srcCandidates) foreach { src =>
            moveCubesFrom(src)
          }
          selectCubesToMove()
        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          selectCubesToMove()

        case _ => // finished
      }
    }

    def moveCubesFrom(srcName: String): Unit = {
        val src            = game.getSpace(srcName)
        val destCandidates = getPatrolDestinations(srcName).toList.sorted
        val eligible       = patrolCubes(src)

        println(s"\nMoving Patrol cubes out of $srcName")
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

      if (candidates.nonEmpty || canSpecial) {
        val choices = List(
          choice(candidates.nonEmpty, "assault",  "Assault at one LOC"),
          choice(canSpecial,          "special",  "Perform a Special Activity"),
          choice(true,                "finished", "Do not Assault at one LOC")
        ).flatten

        askMenu(choices, "\nChoose one:").head match {
          case "assault" =>
            val assaultParams = Params(
              assaultRemovesTwoExtra = pattonUshaded,
              free                   = true)
            val name = askSimpleMenu(candidates, "\nAssault in which LOC:").head
            performAssault(faction, name, assaultParams)

          case "special" =>
            executeSpecialActivity(faction, params, specialActivities)
            assaultOneLOC()

          case _ => // finished
        }
      }
    }


    val notes = List(
      noteIf(bodyCount,     s"Cost is 0 and +3 Aid per guerrilla removed [Momentum: $Mo_BodyCount]"),
      noteIf(pattonUshaded, s"In follow up assault, remove 2 extra enemy pieces [$M48Patton_Unshaded]"),
      noteIf(pattonShaded,  s"After Patrol NVA removes up to 2 cubes that moved [$M48Patton_Shaded]")
    ).flatten

    logOpChoice(faction, Patrol, notes)

    if (hasTheCash) {
      if (faction == ARVN && !params.free) {
        if (momentumInPlay(Mo_BodyCount))
          log(s"ARVN Assault costs zero resources [Momentum: $Mo_BodyCount]")
        else
          decreaseResources(ARVN, 3)
      }
      if (notes.nonEmpty) {
        println(separator())
        for (note <- notes)
          log(note)
      }

      selectCubesToMove()
      activateGuerrillasOnLOCs()
      if (!momentumInPlay(Mo_GeneralLansdale))
        assaultOneLOC()
      if (pattonShaded && movedCubes.size > 0)
        performM48Patton_Shaded(movedCubes)
    }
    else
      log(s"There are not enough ARVN resources (${game.arvnResources}) to Patrol")

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)

    // If Transport special activity was used,  then Armored Cavalry (unshaded)
    // allows ARVN to free assault in one Transport destination
    armoredCavalryAssault()
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
    var sweepSpaces     = Set.empty[String]
    var activatedSpaces = Set.empty[String]
    var cobrasSpaces    = Set.empty[String]
    val alreadyMoved    = new MovingGroups()
    val platoonsShaded  = faction == US && !params.limOpOnly && capabilityInPlay(CombActionPlatoons_Shaded)
    val cobrasUnshaded  = capabilityInPlay(Cobras_Unshaded)
    val boobyTraps      = capabilityInPlay(BoobyTraps_Shaded)
    val defaultMax      = if (platoonsShaded) 2 else NO_LIMIT
    val maxSpaces       = params.maxSpaces getOrElse defaultMax

    def canSpecial = Special.allowed

    def moveTroopsTo(destName: String): Unit = {
      val candidates = spaceNames(sweepSources(destName, faction, alreadyMoved))
      val choices = (candidates map (name => name -> name)) :+ ("finished" -> s"Finished moving troops to $destName")

      askMenu(choices, s"\nMove troops to $destName from:").head match {
        case "finished" =>

        case srcName =>
          val TroopType = if (faction == US) USTroops else ARVNTroops
          val troops    = game.getSpace(srcName).pieces.only(TroopType) - alreadyMoved(srcName)
          val num       = askInt(s"Move how many $TroopType", 0, troops.total)

          if (num > 0) {
            val movers = Pieces().set(num, TroopType)
            alreadyMoved.add(destName, movers)
            movePieces(movers, srcName, destName)
          }
          moveTroopsTo(destName)
      }
    }

    // Per the rules, cubes are moved into sweep spaces "simultaneously"
    // so we do not allow a "special activity" to be selected while cubes
    // are being moved.
    def moveTroops(): Unit = {
      val destCandidates = sweepSpaces.toList.sorted filter { name =>
        sweepSources(name, faction, alreadyMoved).nonEmpty
      }

      val choices = (destCandidates map (name => name -> name)) :+ ("finished" -> "Finished moving troops")
      askMenu(choices, "\nMove cubes to:").head match {
        case "finished" =>
        case name =>
          moveTroopsTo(name)
          moveTroops()
      }
    }

    // Select provinces/cities (not N. Vietnam)
    def selectSweepSpace(): Unit = {
      // Note: we allow selecting spaces without cubes or adjacent cubes because
      // the user may air lift cube in...
      val candidates = if (sweepSpaces.size < maxSpaces)
        spaceNames(game.nonLocSpaces filterNot (sp => sp.isNorthVietnam || sweepSpaces(sp.name)))
      else
        Nil
      val canSweep = candidates.nonEmpty && (faction == US || params.free || game.arvnResources >= 3)
      val choices = List(
        choice(canSweep,   "sweep",     "Select a Sweep space"),
        choice(canSpecial, "special",   "Perform a Special Activity"),
        choice(true,       "finished", s"Finished selecting Sweep spaces")
      ).flatten

      println(s"\n${amountOf(sweepSpaces.size, "space")} selected for Sweep")
      println(separator())
      wrap("", sweepSpaces.toList, showNone = false) foreach println

      askMenu(choices, "\nSweep spaces:").head match {
        case "sweep" =>
          askCandidateOrBlank("\nSweep in which space: ", candidates) foreach { name =>
            sweepSpaces = sweepSpaces + name
            if (faction == ARVN && !params.free)
              decreaseResources(ARVN, 3)

            if (cobrasUnshaded && cobrasSpaces.size < 2 && doCobrasUnshaded(name))
              cobrasSpaces = cobrasSpaces + name
          }
          selectSweepSpace()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          selectSweepSpace()

        case _ => // finished
      }
    }

    def activateGuerrillas(): Unit = {
      val canActivate = (name: String) => {
        val sp = game.getSpace(name)
        sp.sweepActivations(faction) > 0
      }
      val candidates = ((sweepSpaces -- activatedSpaces) filter canActivate).toList.sorted
      val canAll  = candidates.size > 1 && activatedSpaces.isEmpty
      val canRest = candidates.size > 1 && activatedSpaces.nonEmpty

      if (candidates.nonEmpty) {
        val topChoices = List(
          choice(canAll,     "all",      "Activate guerrillas in all sweep spaces"),
          choice(canRest,    "rest",     "Activate guerrillas in the rest of the sweep spaces"),
          choice(canSpecial, "special",  "Perform a Special Activity")
        ).flatten
        val spaceChoices = candidates map (n => n -> s"Activate guerrillas in $n")
        val choices = topChoices ::: spaceChoices

        askMenu(choices, "\nSweep activation:").head match {
          case "all" | "rest" =>
            for (name <- candidates) {
              activateGuerrillasForSweep(name, faction)
              checkShadedBoobyTraps(name, faction)
              activatedSpaces = activatedSpaces + name
            }

          case "special" =>
            executeSpecialActivity(faction, params, specialActivities)
            activateGuerrillas()

          case name =>
            activateGuerrillasForSweep(name, faction)
            checkShadedBoobyTraps(name, faction)
            activatedSpaces = activatedSpaces + name
            activateGuerrillas()
        }
      }
      else if (activatedSpaces.isEmpty)
        log(s"\nNo guerrillas can be activated in the ${amountOf(sweepSpaces.size, "sweep space")}")
    }


    val notes = List(
      noteIf(cobrasUnshaded, s"In 2 spaces, you may remove 1 active (untunneled) enemy [$Cobras_Unshaded]"),
      noteIf(platoonsShaded, s"US can select a maximum of 2 spaces [$CombActionPlatoons_Shaded]"),
      noteIf(boobyTraps,     s"Each space, VC afterward remove 1 troop on die roll 1-3 [$BoobyTraps_Shaded]")
    ).flatten

    logOpChoice(faction, Sweep, notes)


    if (params.singleTarget.nonEmpty)  
      sweepSpaces = Set(params.singleTarget.get)
    else
      selectSweepSpace()
    moveTroops()
    if (sweepSpaces.nonEmpty) {
      activateGuerrillas()
    }

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)
  }


  //  Perform an assault in the given space.
  def performAssault(faction: Faction, name: String, params: Params): Unit = {
    val remove1BaseFirst   = faction == US && capabilityInPlay(Abrams_Unshaded)
    val remove1Underground = faction == US && capabilityInPlay(SearchAndDestroy_Unshaded)
    val searchDestroy      = capabilityInPlay(SearchAndDestroy_Shaded)  // US and ARVN
    val baseTargets = if (params.vulnerableTunnels) InsurgentBases else InsurgentNonTunnels
    val sp          = game.getSpace(name)
    def pieces      = game.getSpace(name).pieces  // Always get fresh instance
    val baseFirst   = remove1BaseFirst && pieces.has(baseTargets) && !pieces.has(UndergroundGuerrillas)
    val underground = remove1Underground && pieces.has(UndergroundGuerrillas)
    val totalLosses = sp.assaultFirepower(faction) + (if (params.assaultRemovesTwoExtra) 2 else 0)
    var killedPieces = Pieces()
    def remaining   = totalLosses - killedPieces.total

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

      // Abrams unshaded
      if (remaining > 0 && baseFirst) {
        log(s"\nRemove a base first [$Abrams_Unshaded]")
        val removed = askPieces(pieces, 1, baseTargets)
        removeToAvailable(name, removed)
        killedPieces = killedPieces + removed
      }

      // Search and Destory unshaded
      // Even if the remaining hits is zero, we will remove an underground
      // guerilla.  But if there are hits remainin, the it counts toward
      // those hits.
      val killedUnderground = if (underground) {
        log(s"\nRemove an underground guerrilla [$SearchAndDestroy_Unshaded]")
        val removed = askPieces(pieces, 1, UndergroundGuerrillas)
        removeToAvailable(name, removed)
        if (remaining > 0) {
          killedPieces = killedPieces + removed  // Counts against total hits
          Pieces()
        }
        else
          removed  // Will be added to killedPieces later
      }
      else
        Pieces()

      // Remove exposed insurgent pieces
      killedPieces = killedPieces + killExposedInsurgents(name, remaining,
                                         canRevealTunnel = true, params.vulnerableTunnels)
      // Add any removed underground guerrilla that did NOT count toward remaining hits above
      killedPieces = killedPieces + killedUnderground

      // Body Count momentum
      if (momentumInPlay(Mo_BodyCount) && killedPieces.totalOf(Guerrillas) > 0) {
        log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $Mo_BodyCount]")
        increaseUsAid(3 * killedPieces.totalOf(Guerrillas))
      }

      // Each removed base adds +6 aid
      if (faction == ARVN && killedPieces.totalOf(baseTargets) > 0) {
        log(s"\nEach insurgent base removed adds +6 Aid")
        increaseUsAid(6 * killedPieces.totalOf(baseTargets))

      }

      if (killedPieces.isEmpty)
        log("\nNo insurgemnt pieces were removed in the assault")

      // Cobras_Shaded
      //    Eash US assault space, 1 US Troop to Casualties on die roll of 1-3
      if (faction == US && pieces.has(USTroops) && capabilityInPlay(Cobras_Shaded)) {
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

    def canSpecial = Special.allowed

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
        choice(canSelect,  "select",   "Select a space to Assault"),
        choice(canSpecial, "special",  "Perform a Special Activity"),
        choice(true,       "finished", "Finished selecting spaces")
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

      askMenu(choices, "\nAssault:").head match {
        case "select" =>
          askCandidateOrBlank("\nAssault in which space: ", candidates) foreach { name =>
            val assaultParams = if (m48Patton &&
                                    m48PattonSpaces.size < 2 &&
                                    !game.getSpace(name).isLowland &&
                                    askYorN(s"Remove 2 extra pieces in this Assault [$M48Patton_Unshaded]? (y/n) ")) {
              log(s"\nUS removes up to 2 extra enemy pieces [$M48Patton_Unshaded]")
              m48PattonSpaces = name :: m48PattonSpaces
              params.copy(assaultRemovesTwoExtra = true)
            }
            else
              params
            performAssault(faction, name, assaultParams)

            val canFollowup = faction == US &&
                              addedARVNAssault == false &&
                              game.getSpace(name).pieces.hasExposedInsurgents &&
                              game.getSpace(name).pieces.has(ARVNCubes) &&
                              (bodyCount || game.arvnResources >= 3)

            if (canFollowup && askYorN(s"Follow up with ARVN assault in $name? (y/n) ")) {
              log(s"\nUS adds a follow up ARVN asault in $name")
              performAssault(ARVN, name, params)
            }
            assaultSpaces = assaultSpaces :+ name
          }
          selectAssaultSpace()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          selectAssaultSpace()

        case _ => // finished
      }
    }

    val notes = List(
      noteIf(abramsUnshaded, s"In 1 space you may remove 1 (untunneled) base first [$Abrams_Unshaded]"),
      noteIf(abramsShaded,   s"Select a maximum of 2 spaces [$Abrams_Shaded]"),
      noteIf(m48Patton,      s"In 2 non-Lowland spaces remove 2 extra enemy pieces [$M48Patton_Unshaded]"),
      noteIf(cobras,         s"Each space, remove 1 troop on die roll 1-3 [$Cobras_Shaded]"),
      noteIf(sdUnshaded,     s"Each space, may remove 1 underground guerrilla [$SearchAndDestroy_Unshaded]"),
      noteIf(sdShaded,       s"Each province, shift support toward Active Opposition [$SearchAndDestroy_Shaded]"),
      noteIf(bodyCount,      s"Cost is 0 and +3 Aid per guerrilla removed [Momentum: $Mo_BodyCount]")
    ).flatten

    logOpChoice(faction, Assault, notes)

    selectAssaultSpace()

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)
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
    val adsid       = faction == NVA && momentumInPlay(Mo_ADSID)
    val mcnamara    = faction == NVA && momentumInPlay(Mo_McNamaraLine)
    val sa2s        = faction == NVA && !mcnamara && capabilityInPlay(SA2s_Shaded)
    val aaa         = faction == NVA && !mcnamara && !params.limOpOnly && capabilityInPlay(AAA_Unshaded)
    val cadres      = faction == VC  && capabilityInPlay(Cadres_Shaded)
    var rallySpaces = List.empty[String]
    var didCadres   = false
    def canSpecial  = Special.allowed

    def promptToAddPieces(name: String): Unit = {
      val sp            = game.getSpace(name)
      val guerrillas    = if (faction == NVA) NVAGuerrillas else VCGuerrillas
      val underground   = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
      val active        = if (faction == NVA) NVAGuerrillas_A else VCGuerrillas_A
      val baseType      = if (faction == NVA) NVABase else VCBase
      val guerrillaType = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
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
          ensurePieceTypeAvailable(guerrillaType, 1)
          placePieces(name, Pieces().set(1, guerrillaType))

        case "place-many" =>
          val num = askInt("\nPlace how many guerrillas", 0, maxGuerrillas)
          ensurePieceTypeAvailable(guerrillaType, num)
          placePieces(name, Pieces().set(num, guerrillaType))

        case "place-base" =>
          val toRemove = askPieces(sp.pieces, 2, guerrillas,  Some("Removing two guerrillas"))
          ensurePieceTypeAvailable(baseType, 1)
          loggingControlChanges {
            removeToAvailable(name, toRemove)
            placePieces(name, Pieces().set(1, baseType))
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
        choice(canSelect,  "select",  s"Select a rally space"),
        choice(canSpecial, "special",  "Perform a Special Activity"),
        choice(true,       "finished", "Finished selecting spaces")
      ).flatten

      println(s"\n${amountOf(rallySpaces.size, "space")} selected for Rally")
      println(separator())
      wrap("", rallySpaces, showNone = false) foreach println

      askMenu(choices, "\nRally:").head match {
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

            if (canAgitate && askYorN(s"Do you wish to Agitate in $name? (y/n) ") && agitateSpace(name, coupRound = false))
              didCadres = true
          }
          selectRallySpace()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          selectRallySpace()

        case _ =>
      }
    }

    val notes = List(
      noteIf(mcnamara, s"Trail improvemnet is prohibited [Momentum: $Mo_McNamaraLine]"),
      noteIf(sa2s,     s"Trail improvement is 2 boxes instead of 1 [$SA2s_Shaded]"),
      noteIf(aaa,      s"Rally that improves the Trail may select 1 space only [$AAA_Unshaded]"),
      noteIf(adsid,    s"-6 NVA resources at any trail change [Momentum: $Mo_ADSID]"),
      noteIf(cadres,   s"May Agitate in one space with an existing base [$Cadres_Shaded]")
    ).flatten

    logOpChoice(faction, Rally, notes)

    selectRallySpace()

    if (faction == NVA && game.resources(NVA) >= 2 && game.trail < TrailMax &&
         !mcnamara && (!aaa || rallySpaces.size == 1)) {
      val num = if (sa2s) 2 else 1

      if (askYorN(s"Do you wish to spend 2 resources to improve the trail by ${amountOf(num, "box", Some("boxes"))}? (y/n) ")) {
        log("\nNVA improves the trail")
        log(separator())
        decreaseResources(NVA, 2)
        improveTrail(num)

        if (adsid) {
          log(s"Momentum: $Mo_ADSID reduces NVA resources at trail change")
          decreaseResources(NVA, 6)
        }
      }
    }

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)
  }

  // MainForceBns_Unshaded - March into LOC/Support activate on moving+COIN > 1 (vice >3)
  // Mo_Claymores - Remove 1 guerrillas in each marching groop that activates
  def executeMarch(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == NVA)
      Infiltrate::Bombard::Ambush::Nil
    else
      Tax::Subvert::Ambush::Nil
    val moveableTypes   = if (faction == NVA) NVATroops::NVAGuerrillas else VCGuerrillas
    val maxDestinations = params.maxSpaces getOrElse NO_LIMIT
    var destinations    = Set.empty[String]
    val mainForceBns    = capabilityInPlay(MainForceBns_Unshaded)
    val claymores       = momentumInPlay(Mo_Claymores)
    // Normally movedInto(name) == frozen(name), however for NVA pieces moving along the trail
    // they can be in the movedInto bucket but not the frozen bucket
    val movedInto       = new MovingGroups()  // All pieces that have moved into each dest space

    def moveablePieces(name: String) = {
      val moveable = game.getSpace(name).pieces.only(moveableTypes)
      // NVA pieces that moved in Laos/Cambodia can keep moving if the trail > 0
      if (faction == NVA && game.trail > TrailMin && isInLaosCambodia(name))
        moveable
      else
        moveable - movedInto(name)
    }

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
          val free        = params.free || dest.isLoC || trailMove || movedInto(destName).nonEmpty
          val moveable    = src.pieces.only(moveableTypes)
          val notYetMoved = moveable - movedInto(srcName)
          val numCoin     = dest.pieces.totalOf(CoinForces)

          if (free || game.resources(faction) > 0) {
            if (!free) {
              log(s"\n$faction pays to move pieces into $destName")
              log(separator())
              decreaseResources(faction, 1)
            }

            val num         = askInt(s"Move how many pieces from $srcName", 1, moveable.total)
            val movers      = askPieces(moveable, num)

            // If moving out of a trail space we must update the movedInto group
            // to reflect pieces that are making a subsequent move
            val trailMove = faction == NVA && game.trail > TrailMin && isInLaosCambodia(srcName)
            if (trailMove && !notYetMoved.contains(movers)) {
              val movedAgain = movers - notYetMoved
              movedInto.remove(srcName, movedAgain)
            }

            movePieces(movers, srcName, destName)

            val tolearance = if (mainForceBns) 1 else 3
            val activate   = (dest.isLoC || dest.support > Neutral) && (numCoin + movers.total) > tolearance
            if (activate) {
              val activeType = if (faction == NVA) NVAGuerrillas_A else VCGuerrillas_A
              val hidden     = movers.only(UndergroundGuerrillas)
              // Fix up the moving group to account for guerrilla activateion and possible
              // removal for Claymores
              val activeMovers = movers.except(UndergroundGuerrillas) + Pieces().set(hidden.total, activeType)
              val finalGroup   = if (claymores && activeMovers.has(activeType))
                activeMovers.remove(1, activeType)
              else
                activeMovers

              val suffix = if (mainForceBns) s" [$MainForceBns_Unshaded]" else ""
              log(s"\nThe moving Guerrillas must activate$suffix")
              log(separator())
              revealPieces(destName, hidden)
              if (claymores && activeMovers.has(activeType))
                removeToAvailable(destName, Pieces().set(1, activeType))
              movedInto.add(destName, finalGroup)
            }
            else
              movedInto.add(destName, movers)
          }
          else
            println(s"\n$faction does not have a resource to pay for the move into $destName")
        }
      }
    }

    def canSpecial = Special.allowed

    val maxAmbush = if (momentumInPlay(Mo_TyphoonKate)) 1 else 2

    def ambushCandidates = if (Special.canAmbush && Special.selectedSpaces.size < maxAmbush) {
      val underground = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
      destinations.toList.sorted filter { name =>
        !(Special.selectedSpaces contains name) &&   // Can't ambush same space twice
        movedInto(name).has(underground) &&          // Must have underground guerrillas that move into the space
        ambushTargets(name).nonEmpty                 // Must have a COIN target to kill
      }
    }
    else
      Nil

    def nextMarchAction(): Unit = {
      val AmbushOpt  = "ambush:(.*)".r
      val MoveOpt    = "move:(.*)".r
      val candidates = spaceNames(game.spaces filter (sp => !destinations.contains(sp.name)))
      val canSelect  = candidates.nonEmpty && destinations.size < maxDestinations
      val moveCandidates = destinations.toList.sorted filter { destName =>
        (getAdjacent(destName) exists (moveablePieces(_).total > 0))
      }
      val topChoices = List(
        choice(canSelect,  "select",  s"Add a march destination space"),
        choice(canSpecial, "special",  "Perform a Special Activity")
      ).flatten
      val ambushChoices = ambushCandidates map (name => s"ambush:$name" -> s"Ambush in $name")
      val moveChoices   = moveCandidates map (name => s"move:$name" -> s"Move pieces into $name")
      val lastChoice    = List("finished" -> "Finished with March operation")

      println(s"\n${amountOf(destinations.size, "destination")} selected for March")
      println(separator())
      wrap("", destinations.toList) foreach println

      askMenu(topChoices:::ambushChoices:::moveChoices:::lastChoice, "\nMarch:").head match {
        case "select" =>
          askCandidateOrBlank("\nAdd which space as a march destination: ", candidates) foreach { name =>
            destinations = destinations + name
            log(s"\n$faction selects $name as a March destination")
          }
          nextMarchAction()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          nextMarchAction()



        case AmbushOpt(name) =>
          if (performAmbush(name, faction, March, free = true)) {
            // An uderground member of the the movedInto group was just flipped
            // to active.  We must update the movedInto group to reflect that.
            val (underground, active) = if (faction == NVA)
              (Pieces(nvaGuerrillas_U = 1), Pieces(nvaGuerrillas_A = 1))
            else
              (Pieces(vcGuerrillas_U = 1), Pieces(vcGuerrillas_A = 1))

            movedInto.remove(name, underground)
            movedInto.add(name, active)
          }
          nextMarchAction()

        case MoveOpt(name) =>
          moveToDestination(name)
          nextMarchAction()

        case _ =>  // finished
      }
    }

    val notes = List(
      noteIf(mainForceBns, s"March into LOC/Support activates if moving+COIN > 1 [$MainForceBns_Unshaded]"),
      noteIf(claymores,    s"Remove 1 guerrilla in each marching group that activates [$Mo_Claymores]")
    ).flatten

    logOpChoice(faction, March, notes)

    nextMarchAction()

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)
  }

  // PT76_Unshaded - Each NVA attack space, first remove 1 NVA troop cube (also ambush)
  // PT76_Shaded   - In 1 NVA attack space, remove 1 enemy per NVA Troop
  def executeAttack(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == NVA)
      Bombard::Ambush::Nil
    else
      Tax::Ambush::Nil
    val pt76_unshaded      = faction == NVA && capabilityInPlay(PT76_Unshaded)
    val pt76_shaded        = faction == NVA && capabilityInPlay(PT76_Shaded)

    val maxAttacks   = params.maxSpaces getOrElse NO_LIMIT
    var attackSpaces = Set.empty[String]
    val guerrillas   = if (faction == NVA) NVAGuerrillas else VCGuerrillas
    def canSpecial   = Special.allowed

    val isAttackSpace = (sp: Space) => {
      !attackSpaces.contains(sp.name) &&
      (sp.pieces.has(guerrillas) || (faction == NVA && sp.pieces.has(NVATroops))) &&
      sp.pieces.has(CoinPieces)
    }

    val isAmbushSpace = (sp: Space) => {
      !attackSpaces.contains(sp.name) &&
      sp.pieces.has(guerrillas)       &&
      ambushTargets(sp.name).nonEmpty
    }

    val maxAmbush = if (momentumInPlay(Mo_TyphoonKate)) 1 else 2

    def nextAttackAction(): Unit = {
      val hasTheCash       = params.free || game.resources(faction) > 0
      val attackCandidates = spaceNames(game.spaces filter isAttackSpace)
      val ambushCandidates = spaceNames(game.spaces filter isAmbushSpace)
      val canAttack        = hasTheCash && attackCandidates.nonEmpty && attackSpaces.size < maxAttacks
      val canAmbush        = hasTheCash && Special.canAmbush && Special.selectedSpaces.size < maxAmbush &&
                             ambushCandidates.nonEmpty && attackSpaces.size < maxAttacks
      val choices = List(
        choice(canAttack,  "attack",  s"Select a space to Attack"),
        choice(canAmbush,  "ambush",  s"Select a space to Ambush"),
        choice(canSpecial, "special",  "Perform a Special Activity"),
        choice(true,       "finished", "Finished with Attack operation")
      ).flatten

      println(s"\n${amountOf(attackSpaces.size, "space")} selected for Attack")
      println(separator())
      wrap("", attackSpaces.toList) foreach println

      askMenu(choices, "\nAttack:").head match {
        case "attack" =>
          askCandidateOrBlank("\nAttack which space: ", attackCandidates) foreach { name =>
            performAttack(name, faction, free = params.free)
            attackSpaces = attackSpaces + name
          }
          nextAttackAction()

        case "ambush" =>
          askCandidateOrBlank("\nAmbush which space: ", ambushCandidates) foreach { name =>
            performAmbush(name, faction, Attack, free = params.free)
            attackSpaces = attackSpaces + name
          }
          nextAttackAction()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          nextAttackAction()

        case _ =>  // finished
      }
    }

    val notes = List(
      noteIf(pt76_unshaded, s"Each NVA attack space first remove 1 NVA troop [$PT76_Unshaded]"),
      noteIf(pt76_shaded,   s"In 1 NVA attack space, remove 1 enemy per Troop [$PT76_Shaded]")
    ).flatten

    logOpChoice(faction, Attack, notes)

    nextAttackAction()

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)
  }

  // Cadres_Unshaded - VC terror must remove two guerrillas per space
  def executeTerror(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == NVA)
      Bombard::Nil
    else
      Tax::Subvert::Nil
    val cadres         = faction == VC && capabilityInPlay(Cadres_Unshaded)
    var selectedSpaces = List.empty[String]
    val maxSpaces      = params.maxSpaces getOrElse NO_LIMIT
    val underground    = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
    def canSpecial     = Special.allowed

    // terrorMarkersAvailable
    def nextTerrorAction(): Unit = {
      val hasTheCash   = params.free || game.resources(faction) > 0
      var canTerrorize = (sp: Space) => {
        val canPay   = sp.isLoC || hasTheCash
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

      println(s"\n${amountOf(selectedSpaces.size, "space")} selected for Terror")
      println(separator())
      wrap("", selectedSpaces) foreach println

      if (candidates.isEmpty) {
        val more = if (selectedSpaces.nonEmpty) " more" else ""
        println(s"\nThere are no${more} spaces eligible for a Terror operation")
      }

      askMenu(choices, "\nTerror:").head match {
        case "select" =>
          askCandidateOrBlank("\nTerrorize which space: ", candidates) foreach { name =>
            def sp = game.getSpace(name)  // function so we get fresh copy each time
            // NVA may terror with only troops, in which case no guerrilla is revealed
            val toReveal = if (sp.pieces.has(underground)) Pieces().set(1, underground) else Pieces()

            log(s"\n$faction selects $name for Terror")
            log(separator())
            if (!params.free && !sp.isLoC)
              decreaseResources(faction, 1)
            revealPieces(name, toReveal)
            if (cadres) {
              val toRemove = askPieces(sp.pieces, 2, VCGuerrillas, Some(s"Remove guerrillas for [$Cadres_Unshaded]"))
              removeToAvailable(name, toRemove)
            }

            if (sp.terror == 0)
              addTerror(name, 1) // Terror/Sabotage marker

            faction match {
              case NVA if !sp.isLoC && sp.support > Neutral           => decreaseSupport(name, 1)
              case VC  if !sp.isLoC && sp.support != ActiveOpposition => decreaseSupport(name, 1)
              case _ => // No effect
            }
            selectedSpaces = selectedSpaces :+ name
          }
          nextTerrorAction()

        case "special" =>
          executeSpecialActivity(faction, params, specialActivities)
          nextTerrorAction()

        case _ => // finished
      }

    }

    val notes = List(
      noteIf(cadres, s"$faction must remove 2 guerrillas in each terror space [$Cadres_Unshaded]")
    ).flatten

    logOpChoice(faction, Terror, notes)

    nextTerrorAction()

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)

  }
}
