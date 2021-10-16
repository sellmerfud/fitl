
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

  private var pt76_shaded_used = false  // NVA attack in one space
  
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
      allowSpecial     = params.includeSpecial
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

    def spaces = groups.keys.toSet
    def toList = groups.toList.sortBy(_._1)
    def allPieces = toList.foldLeft(Pieces()) { (all, group) => all + group._2 }
    def size   = groups.size
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

  // Return the number of exposed insurgent pieces
  // Bases are only included if there are not hidden guerrillas
  def numExposedInsurgents(pieces: Pieces): Int = {
    val exposedForces = pieces.totalOf(NVATroops::ActiveGuerrillas)
    val exposedBases  = if (pieces.has(UndergroundGuerrillas)) 0
                        else pieces.totalOf(NVABase::VCBase::Nil)
    exposedForces + exposedBases
  }

  //  Kill exposed insurgent pieces up to the max allowd.
  //  NVA Troops first
  //  Then active guerrillas (prompt user for which to remove)
  //  Then if no troops/guerrillas remain bases can be removed
  //  If `canRevealTunnel` is true, and no other Insurgent pieces
  //  remain, then the tunnel marker is removed.
  //  Return the number of pieces removed.
  def killExposedInsurgents(name: String, maxRemoval: Int, canRevealTunnel: Boolean): Pieces = {
    loggingControlChanges {
      var removed    = Pieces()
      def pieces     = game.getSpace(name).pieces
      def remaining  = maxRemoval - removed.total

      (remaining min pieces.numOf(NVATroops)) match {
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
        (remaining min pieces.totalOf(InsurgentNonTunnels)) match {
          case 0 =>
          case num =>
            val prompt = s"\nRemove untunneled bases"
            val toRemove = askPieces(pieces, num, InsurgentNonTunnels, Some(prompt))
            removeToAvailable(name, toRemove)
            removed = removed + toRemove
        }

        if (canRevealTunnel && remaining > 0 && pieces.has(InsurgentTunnels)) {
          val die = d6
          val success = die > 3
          log("\nNext piece to remove would be a tunneled base")
          log(separator())
          log(s"Die roll is: ${die} [${if (success) "Tunnel destroyed!" else "No effect"}]")
          if (success) {
            val prompt = "\nRemove tunnel marker"
            val tunnel = askPieces(pieces, 1, InsurgentTunnels, Some(prompt))
            removeTunnelMarker(name, tunnel.explode().head)
          }
        }
      }
      removed
    }
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
        (if (blowtorch) 1 else if (nguyenCaoKy) 4 else 3)
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


  def activateGuerrillasForSweep(name: String, faction: Faction): Unit = {
    val sp = game.getSpace(name)
    val num = sp.sweepActivations(faction) min sp.pieces.totalOf(UndergroundGuerrillas)
    val troopType = if (faction == US) USTroops else ARVNTroops
    if (num > 0) {
      log(s"\nActivating guerrillas in $name")
      log(separator())
      val guerrillas = askPieces(sp.pieces, num, UndergroundGuerrillas)
      revealPieces(name, guerrillas)

      if (capabilityInPlay(BoobyTraps_Shaded) && sp.pieces.has(troopType)) {
        //  Roll a d6.  On a 1-3, remove one of the sweeping factions troops.
        //  ARVN to available, US to casualties
        log(s"\nResolving $BoobyTraps_Shaded in $name")
        log(separator())
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
    val isCandidate = (sp: Space) => sp.pieces.has(InsurgentPieces) &&sp.pieces.has(ARVNCubes)
    val candidates = spaceNames(Special.transportDestinations map game.getSpace filter isCandidate)

    if (candidates.nonEmpty && askYorN(s"\nDo you wish to perform a free assault in 1 transport destinaion via [$ArmoredCavalry_Unshaded]? (y/n) ")) {
        val name = askCandidate("Free Assault in which space: ", candidates)

        log(s"\n$ArmoredCavalry_Unshaded triggers a free Assault")
        performAssault(name, ARVN, Params(free = true))
    }
  }

  // Carry out an ambush in the given space
  // MainForceBns_Shaded   - 1 VC Ambush space may remove 2 enemy pieces
  // PT76_Unshaded - Each NVA attack space, first remove 1 NVA troop cube (also ambush)
  def performAmbush(name: String, faction: Faction, free: Boolean): Boolean = {
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
          if (faction == NVA && capabilityInPlay(PT76_Unshaded) && sp.pieces.has(NVATroops))
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
      val numTroops  = if (pt76_unshaded) troops.total - 1 else troops.total
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
      case OpPlusSpecial => executeOp(faction, Params(includeSpecial = true))
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
        !Special.trainingSpaces(sp.name) &&
        !Special.selectedSpaces(sp.name) &&
        sp.pieces.has(arvnForces) &&
        (sp.pieces.has(UndergroundGuerrillas) || (cobras && hasAssaultTarget(sp)))
      })
      val assaultCandidates = spaceNames(game.spaces filter { sp =>
        !Special.trainingSpaces(sp.name) &&
        !Special.selectedSpaces(sp.name) &&
        sp.pieces.has(arvnForces) &&
        hasAssaultTarget(sp)
      })
      val specialForcesCandidates = spaceNames(game.spaces filter { sp =>
        !Special.trainingSpaces(sp.name) &&
        !Special.selectedSpaces(sp.name) &&
        sp.pieces.has(Irregulars_U::Rangers_U::Nil) &&
        hasSpecialOpsTarget(sp)
      })

      val maxAdvise = if (momentumInPlay(Mo_TyphoonKate)) 1 else 2

      val canSweep   = Special.selectedSpaces.size < maxAdvise && sweepCandidates.nonEmpty
      val canAssault = Special.selectedSpaces.size < maxAdvise && assaultCandidates.nonEmpty
      val canSpecial = Special.selectedSpaces.size < maxAdvise && specialForcesCandidates.nonEmpty
      val choices = List(
        choice(canSweep,   "sweep",   "Sweep a space with ARVN forces"),
        choice(canAssault, "assault", "Assault a space with ARVN forces"),
        choice(canSpecial, "special", "Use Irregular/Ranger to remove enemy pieces"),
        choice(true,       "finished", "Finished selecting Advise spaces")
      ).flatten

      println(s"\n${amountOf(Special.selectedSpaces.size, "space")} of $maxAdvise selected for Advise")
      println(separator())
      wrap("", Special.selectedSpaces.toList) foreach println

      askMenu(choices, "\nChoose Advise option:").head match {
        case "sweep" =>
          askCandidateOrBlank("ARVN Sweep in which space: ", sweepCandidates) foreach { name =>
            activateGuerrillasForSweep(name, ARVN)
            doCobrasUnshaded(name)
            Special.selectedSpaces = Special.selectedSpaces + name
          }
          nextAdviseSpace()

        case "assault" =>
          askCandidateOrBlank("ARVN Assault in which space: ", assaultCandidates) foreach { name =>
            performAssault(name, ARVN, params.copy(free = true))
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

    log("\nUS chooses Advise special activity")
    log(separator())

    nextAdviseSpace();
    if (askYorN("\nDo you wish to add +6 Aid? (y/n) "))
      increaseUsAid(6)
  }

  // US special activity
  // Any US Troops plus up to 4 (Irregulars, Rangers, ARVN Troops)
  // among 4 selected spaces (2 in Monsoon, Never N. Vietnam)
  def doAirLift(params: Params): Unit = {
    val Others: List[PieceType] = ARVNTroops::Rangers:::Irregulars
    val maxAirLiftSpaces = if (game.inMonsoon) 2 else 4
    var airLiftSpaces    = List.empty[String]
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
      !sp.isNorthVietnam && !airLiftSpaces.contains(sp.name)
    })

    def liftOutCandidates = if (airLiftSpaces.size > 1)
      airLiftSpaces.filter(name => moveablePieces(game.getSpace(name)).nonEmpty)
    else
      Nil


    // Lift forces out and place them in a destination space
    def liftForcesOutOf(srcName: String): Unit = {
      val ForceTypes = List(USTroops, Irregulars_U, Irregulars_A, ARVNTroops, Rangers_U, Rangers_A)
      val destCandidates = airLiftSpaces filter (_ != srcName)
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
            eligible.numOf(USTroops)
          else
            liftedOthers(srcName).numOf(t) + (maxNewOthers min unmovedOthers.numOf(t))
        }
        val forceChoices = ForceTypes flatMap { t =>
          val num = maxForce(t)
          val name = if (num == 1) t.singular else t.plural
          if (num > 0) Some(Some(t) -> s"Air Lift $name") else None
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
              val numMovedBefore = liftedOthers(srcName).numOf(t)
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
      wrap("", airLiftSpaces) foreach println
      
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

    // Start of Air Lift Special Activity
    // ------------------------------------
    log("\nUS chooses Air Lift special activity")
    log(separator())
    if (game.inMonsoon)
      log("May only choose 2 spaces in Monsoon")

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
    val maxHits     = d6
    val maxSpaces = if      (momentumInPlay(Mo_TyphoonKate)) 1
                    else if (game.inMonsoon) 2
                    else 6
    val maxPieces = if (momentumInPlay(Mo_WildWeasels)) 1 else if (capabilityInPlay(LaserGuidedBombs_Shaded)) 2 else maxHits
    var strikeSpaces = Set.empty[String]
    var removedSpaces = Set.empty[String] // strike spaces where pieces have been removed
    var totalRemoved = 0
    var arclight_unshaded_used = false
    def isCandidate(sp: Space) = {
      params.spaceAllowed(sp.name)   &&   // Event may limit to certain spaces
      !strikeSpaces(sp.name)         &&
      sp.pieces.hasExposedInsurgents &&
      (sp.pieces.has(CoinPieces) || (sp.isProvince && arclight_unshaded && !arclight_unshaded_used))
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

          if (capabilityInPlay(TopGun_Shaded))
            log(s"\nDie roll ($TopGun_Shaded): %die [${if (success) "Success!" else "Failure"}]")

          if (success) {
            degradeTrail(numBoxes)

            if (migs_shaded && game.availablePieces.has(USTroops))
                removeAvailableToCasualties(Pieces(usTroops = 1), Some(s"$MiGs_Shaded triggers"))

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
          val killedPieces = killExposedInsurgents(name, num, canRevealTunnel = false)


          if (num > 0) {
            val sp = game.getSpace(name)
            val numShift = if (sp.isLOC || sp.population == 0 || sp.support == ActiveOpposition)
              0
            else if (num > 1 && capabilityInPlay(ArcLight_Shaded) && sp.support > PassiveOpposition) {
              log(s"\n$ArcLight_Shaded triggers")
              log(s"Shift 2 levels toward Active Opposition")
              2
            }
            else if (num == 1 && capabilityInPlay(LaserGuidedBombs_Unshaded)) {
              log(s"\n$LaserGuidedBombs_Unshaded triggers")
              log(s"No shift toward Active Opposition")
              0
            }
            else {
              log(s"\nShift 1 level toward Active Opposition")
              1
            }
            decreaseSupport(name, numShift)
          }
          removedSpaces = removedSpaces + name
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
        wrap("", strikeSpaces.toList) foreach println
        
        if (!oriskany) {
          if (hasDegraded)
            println("\nYou have already degraded the trail")
          else
            println("\nYou have not yet degraded the trail")
        }

        val canRemove = (maxPieces - totalRemoved) min hitsRemaining
        if (choices.size == 1) {
          val degradeMsg = if (hasDegraded) "" else " and you cannot degrade the trail"
          println(s"\n ${amountOf(hitsRemaining, "hit")} remaining}")
          println(s"There are no strike targets $degradeMsg")
        }
        else
          askMenu(choices, s"\nAir Strike: (${amountOf(hitsRemaining, "hit")} remaining, remove up to ${amountOf(canRemove, "piece")})").head match {
            case "select" =>
              askCandidateOrBlank("\nAir Strike in which space:", selectCandidates) foreach { name =>
                strikeSpaces = strikeSpaces + name
              }
              nextAirStrikeAction(hitsRemaining, hasDegraded)

            case "degrade" =>
              performDegrade()
              nextAirStrikeAction(hitsRemaining - 2, true)

            case StrikeOpt(name) =>
              val numRemoved = performStrike(name)
              totalRemoved -= numRemoved

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
    log("\nUS chooses Air Strike special activity")
    log(separator())
    for (note <- notes)
      log(note)

    log("\nRolling a die to determine the number of hits")
    log(separator())
    log(s"Number of hits: $maxHits")

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
    val maxSpaces       = if (kate || heaven_shaded) 1 else 2

    // Allow zero pop spaces if Young Turks is leader because
    // player may want the +2 Patronage
    def isCandidate(sp: Space) = {
        !sp.isLOC         &&
        sp.coinControlled &&
        sp.name != Saigon &&
        (sp.population > 0 || young_turks) &&
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
      wrap("", Special.selectedSpaces.toList) foreach println

      askMenu(choices, "\nGovern:").head match {
        case "select" =>
          askCandidateOrBlank("\nGovern in which space: ", candidates) foreach { name =>
            val sp           = game.getSpace(name)
            val canPatronage = sp.population > 0 && sp.pieces.totalOf(ARVNCubes) > sp.pieces.numOf(USTroops)

            val action = if (sp.population == 0)  // Special case for Young Turks
              "turks_only"
            else if (!canPatronage)
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
                val num = (game.patronage min sp.population)
                log("Transfer population value from Aid to Patronage")
                decreaseUsAid(num)
                increasePatronage(num)

                if (sp.population > 0 && sp.support != Neutral) {
                  val skipShift = heaven_unshaded && !heaven_unshaded_used &&
                       askYorN(s"Do you wish to use [$MandateOfHeaven_Unshaded] to avoid shifting support? (y/n) ")

                  if (skipShift)
                    heaven_unshaded_used = true
                  else {
                    if (sp.support > Neutral)
                      decreaseSupport(name, 1)
                    else
                      increaseSupport(name, 1)
                  }
                }
              }
              else if (action == "gain_aid") {
                log("Add 3 times population value to Aid")
                increaseUsAid(sp.population * 3)
              }
              else
                log(s"$name has zero population.  Cannot gain Aid or transfer Aid to Patronage")

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
    log("\nARVN chooses Govern special activity")
    log(separator())
    for (note <- notes)
      log(note)

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

    log("\nARVN chooses Transport special activity")
    log(separator())
    for (note <- notes)
      log(note)


    val srcName        = askCandidate(srcPrompt, srcCandidates)
    val eligible       = game.getSpace(srcName).pieces.only(pieceTypes)
    val maxMovers      = eligible.total min 6
    val numMovers      = askInt(s"\nMove how many $piecesName out of $srcName", 0, maxMovers)
    if (numMovers > 0) {
      var movingPieces   = askPieces(eligible, numMovers, prompt = Some(s"Selecting $piecesName to Transport"))
      val destCandidates = getTransportDestinations(srcName)

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
    val maxSpaces = if (kate) 1 else 2
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
      wrap("", raidSpaces.toList) foreach println

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

    

    log("\nARVN chooses Raid special activity")
    log(separator())
    for (note <- notes)
      log(note)

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
    val maxSpaces        = if (kate || transportGrp) 1 else 2
    var infiltrateSpaces = Set.empty[String]
    
    val canSuborn = (sp: Space) =>
      sp.pieces.totalOf(NVAPieces) > sp.pieces.totalOf(VCPieces) &&
      (sp.pieces.has(VCPieces) || sp.support < Neutral)
      
    val isCandidate = (sp: Space) =>
      sp.pieces.has(NVABases) || canSuborn(sp)
    
    
    def placeTroops(name: String): Unit = {
      def sp            = game.getSpace(name)
      val toPlaceTroops = game.piecesToPlace.numOf(NVATroops)
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
          val vcType  = vcPiece.explode().head
          val nvaType = getInsurgentCounterPart(vcType)
          
          if (vcPiece.has(VCBase) || vcPiece.has(VCTunnel)) {
            removeToAvailable(name, vcPiece)
            ensurePieceTypeAvailable(NVABase, 1)
            placePieces(name, Pieces().set(1, NVABase))
            if (nvaType == NVATunnel)
              addTunnelMarker(name, NVABase)
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
      wrap("", infiltrateSpaces.toList) foreach println
      
      
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


    log("\nNVA chooses Infiltrate special activity")
    log(separator())
    for (note <- notes)
      log(note)

    nextInfiltrateAction()
  }

  // NVA special activity
  // Mo_TyphoonKate         - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  // LongRangeGuns_Unshaded - NVA Bombard is max 1 space
  // LongRangeGuns_Shaded   - NVA Bombard is max 3 spaces
  def doBombard(params: Params): Unit = {
    val CoinTroops = USTroops::ARVNTroops::Nil
    val longRange_unshaded = capabilityInPlay(LongRangeGuns_Unshaded)
    val longRange_shaded   = capabilityInPlay(LongRangeGuns_Shaded)
    var bombardSpaces      = Set.empty[String]
    val maxSpaces = if      (longRange_unshaded) 1
                    else if (longRange_shaded)   3
                    else                         2
                      
    val isCandidate = (sp: Space) => {
      val enemyCondition = sp.pieces.totalOf(CoinTroops) > 2 || (sp.pieces.has(CoinTroops) && sp.pieces.has(CoinBases))
      lazy val hasAdjacentTroops = getAdjacent(sp.name) exists { name =>
        game.getSpace(name).pieces.numOf(NVATroops) > 2
      }
      
      enemyCondition && (sp.pieces.numOf(NVATroops) > 2 || hasAdjacentTroops)
    }
    
    def bombardSpace(name: String): Unit = {
      val sp = game.getSpace(name)
      
      log(s"\nNVA Bombards in $name")
      log(separator())
      
      val toRemove  = askPieces(sp.pieces, 1, CoinTroops, Some("Bombarding a Coin Troop"))
      val pieceType = toRemove.getTypes.head
      if (pieceType == USTroops)
        removeToCasualties(name, Pieces(usTroops = 1))
      else
        removeToAvailable(name, Pieces(arvnTroops = 1))
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
      wrap("", bombardSpaces.toList) foreach println
      
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


    log("\nNVA chooses Bombard special activity")
    log(separator())
    for (note <- notes)
      log(note)
      
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
    log(s"\n$faction chooses the Ambush special activity")
    log(separator())

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
    val maxSpaces = if (kate) 1 else 4
    
    val isCandidate = (sp: Space) => if (sp.isLOC)
      sp.printedEconValue > 0 && sp.pieces.has(VCGuerrillas_U)
    else
      sp.population > 0 && sp.pieces.has(VCGuerrillas_U) && !sp.coinControlled
    
    def taxSpace(name: String): Unit = {
      val sp  = game.getSpace(name)
      val num = if (sp.isLOC) sp.printedEconValue else 2 * sp.population
      log(s"\nVC Taxes in $name")
      log(separator())
      revealPieces(name, Pieces(vcGuerrillas_U = 1))
      increaseResources(VC, num)
      if (sp.support != ActiveSupport)
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
      wrap("", taxSpaces.toList) foreach println
      
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


    log("\nVC chooses Tax special activity")
    log(separator())
    for (note <- notes)
      log(note)
      
    nextTaxAction()
  }

  // VC special activity
  // Mo_TyphoonKate - Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)
  def doSubvert(params: Params): Unit = {
    val kate          = momentumInPlay(Mo_TyphoonKate)
    val maxSpaces     = if (kate) 1 else 2
    var subvertSpaces = Set.empty[String]
    var cubesRemoved  = 0
    
    val isCandidate = (sp: Space) => sp.pieces.has(VCGuerrillas_U) && sp.pieces.has(ARVNCubes)

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
      wrap("", subvertSpaces.toList) foreach println
      
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


    log("\nVC chooses Subvert special activity")
    log(separator())
    for (note <- notes)
      log(note)
      
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
    println("executeEvent() not implemented")

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
  // RVN_Leader_NguyenCaoKy - US/ARVN pacification costs 4 resources per Terror/Level
  def executeTrain(faction: Faction, params: Params): Unit = {
    val specialActivities = if (faction == US)
      Advise::AirLift::AirStrike::Nil
    else
      Govern::Transport::Nil
    val canPlaceExtraPolice = faction == US && capabilityInPlay(CombActionPlatoons_Unshaded)
    var placedExtraPolice   = false // only if CombActionPlatoons_Unshaded in play
    val maxTrainSpaces      = params.maxSpaces getOrElse 1000
    val maxPacifySpaces     = if (faction == US && capabilityInPlay(CORDS_Unshaded)) (2 min maxTrainSpaces) else 1
    val maxPacifyLevel      = if (faction == US && capabilityInPlay(CORDS_Shaded)) PassiveSupport else ActiveSupport
    var selectedSpaces      = List.empty[String]
    var arvnPlacedIn        = List.empty[String]
    var pacifySpaces        = List.empty[String]
    val nguyenCaoKy         = isRVNLeader(RVN_Leader_NguyenCaoKy)
    val isCandidate = (sp: Space) => {
      val valid = if (faction == ARVN) !sp.isLOC && !sp.nvaControlled
                  else                 !sp.isLOC && sp.pieces.has(USPieces)

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
      wrap("", selectedSpaces) foreach println

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

    log(s"\n$faction chooses Train operation")
    log(separator())
    for (note <- notes)
      log(note)

    selectTrainSpace()
    if (selectedSpaces.nonEmpty)
      promptFinalAction() // Pacify, Place base, Xfer Patronage to ARVN resources


    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)

    // If Transport special activity was used,  then Armored Cavalry (unshaded)
    // allows ARVN to free assault in one Transport destination
    if (capabilityInPlay(ArmoredCavalry_Unshaded) && Special.transportDestinations.nonEmpty)
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
      val onNetwork = sp.isLOC || sp.isCity || getAdjacentLOCs(sp.name).nonEmpty || getAdjacentCities(sp.name).nonEmpty
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
        val destCandidates = getPatrolDestinations(srcName).sorted(LocLastOrdering)
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
            performAssault(name, faction, assaultParams)

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
        for (note <- notes)
          log(note)
      }

      selectCubesToMove()
      activateGuerrillasOnLOCs()
      assaultOneLOC()
      if (pattonShaded && movedCubes.size > 0) {
        // TODO:
        // NVABot.removeEnemyPieces(2, movedCubes.toList)
        println()
        println(separator(char = '='))
        println(s"$M48Patton_Shaded for NVA Bot has not been implemented!")
        println(separator(char = '='))
      }
    }
    else
      log(s"There are not enough ARVN resources (${game.arvnResources}) to Patrol")

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)

    // If Transport special activity was used,  then Armored Cavalry (unshaded)
    // allows ARVN to free assault in one Transport destination
    if (capabilityInPlay(ArmoredCavalry_Unshaded) && Special.transportDestinations.nonEmpty)
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
    val defaultMax      = if (platoonsShaded) 2 else 1000
    val maxSpaces       = params.maxSpaces getOrElse defaultMax

    def canSpecial = Special.allowed

    def moveTroopsTo(destName: String): Unit = {
      val candidates = sweepSources(destName, faction, alreadyMoved)
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
      wrap("", sweepSpaces.toList) foreach println

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
        sp.sweepActivations(faction) > 0 && sp.pieces.has(UndergroundGuerrillas)
      }
      val candidates = ((sweepSpaces -- activatedSpaces) filter canActivate).toList.sorted

      if (candidates.nonEmpty) {
        val topChoices = List(
          choice(activatedSpaces.isEmpty,  "all",      "Activate guerrillas in all sweep spaces"),
          choice(activatedSpaces.nonEmpty, "rest",     "Activate guerrillas in the rest of the sweep spaces"),
          choice(canSpecial,               "special",  "Perform a Special Activity")
        ).flatten
        val spaceChoices = candidates map (n => n -> s"Activate guerrillas in $n")
        val choices = topChoices ::: spaceChoices

        askMenu(choices, "\nSweep activation:").head match {
          case "all" | "rest" =>
            for (name <- candidates) {
              activateGuerrillasForSweep(name, faction)
              activatedSpaces = activatedSpaces + name
            }

          case "special" =>
            executeSpecialActivity(faction, params, specialActivities)
            activateGuerrillas()

          case name =>
            activateGuerrillasForSweep(name, faction)
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

    log(s"\n$faction chooses Sweep operation")
    log(separator())
    for (note <- notes)
      log(note)

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
  //
  //  If US add an ARVN assault.
  def performAssault(name: String, faction: Faction, params: Params): Unit = {
    val remove1BaseFirst   = faction == US && capabilityInPlay(Abrams_Unshaded)
    val remove1Underground = faction == US && capabilityInPlay(SearchAndDestroy_Unshaded)
    val searchDestroy      = capabilityInPlay(SearchAndDestroy_Shaded)  // US and ARVN

    val sp          = game.getSpace(name)
    def pieces      = game.getSpace(name).pieces  // Always get fresh instance
    val baseFirst   = remove1BaseFirst && pieces.has(InsurgentNonTunnels) && !pieces.has(UndergroundGuerrillas)
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
        val prompt = s"\nRemove a base first [$Abrams_Unshaded]"
        val removed = askPieces(pieces, 1, InsurgentNonTunnels, Some(prompt))
        removeToAvailable(name, removed)
        killedPieces = killedPieces + removed
      }

      // Search and Destory unshaded
      // Even if the remaining hits is zero, we will remove an underground
      // guerilla.  But if there are hits remainin, the it counts toward
      // those hits.
      val killedUnderground = if (underground) {
        val prompt = s"\nRemove an underground guerrilla [$SearchAndDestroy_Unshaded]"
        val removed = askPieces(pieces, 1, UndergroundGuerrillas, Some(prompt))
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
      killedPieces = killedPieces + killExposedInsurgents(name, remaining, canRevealTunnel = true)
      // Add any removed underground guerrilla that did NOT count toward remaining hits above
      killedPieces = killedPieces + killedUnderground
      
      // Body Count momentum
      if (momentumInPlay(Mo_BodyCount) && killedPieces.totalOf(Guerrillas) > 0) {
        log(s"\nEach guerrilla removed adds +3 Aid [Momentum: $Mo_BodyCount]")
        increaseUsAid(3 * killedPieces.totalOf(Guerrillas))
      }

      // Each removed base adds +6 aid
      if (killedPieces.totalOf(InsurgentNonTunnels) > 0) {
        log(s"\nEach insurgent base removed adds +6 Aid")
        increaseUsAid(6 * killedPieces.totalOf(InsurgentNonTunnels))

      }

      if (killedPieces.isEmpty)
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

    log(s"\n$faction chooses Assault operation")
    log(separator())
    for (note <- notes)
      log(note)

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
      wrap("", rallySpaces) foreach println

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

            if (canAgitate && askYorN(s"Do you wish to Agitate in $name? (y/n) ") && agitateSpace(name))
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

    log(s"\n$faction chooses Rally operation")
    log(separator())
    for (note <- notes)
      log(note)

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
    val maxDestinations = params.maxSpaces getOrElse 1000
    var destinations    = Set.empty[String]
    val mainForceBins   = capabilityInPlay(MainForceBns_Unshaded)
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
          val free        = params.free || dest.isLOC || trailMove || movedInto(destName).nonEmpty
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

            val activateNum = if (mainForceBins) 1 else 3
            val activate    = (dest.isLOC || dest.support > Neutral) && (numCoin + movers.total) > activateNum
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
          if (performAmbush(name, faction, free = true)) {
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
      noteIf(mainForceBins, s"March into LOC/Support activates if moving+COIN > 1 [$MainForceBns_Unshaded]"),
      noteIf(claymores,     s"Remove 1 guerrilla in each marching group that activates [$Mo_Claymores]")
    ).flatten

    log(s"\n$faction chooses March operation")
    log(separator())
    for (note <- notes)
      log(note)

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

    val maxAttacks   = params.maxSpaces getOrElse 1000
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
          askCandidateOrBlank("\nAmbush which space: ", attackCandidates) foreach { name =>
            performAmbush(name, faction, free = params.free)
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

    log(s"\n$faction chooses Attack operation")
    log(separator())
    for (note <- notes)
      log(note)

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
    val maxSpaces      = params.maxSpaces getOrElse 1000
    val underground    = if (faction == NVA) NVAGuerrillas_U else VCGuerrillas_U
    def canSpecial     = Special.allowed

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
          executeSpecialActivity(faction, params, specialActivities)
          nextTerrorAction()

        case _ => // finished
      }

    }

    val notes = List(
      noteIf(cadres, s"$faction must remove 2 guerrillas in each terror space [$Cadres_Unshaded]")
    ).flatten

    log(s"\n$faction chooses Terror operation")
    log(separator())
    for (note <- notes)
      log(note)

    nextTerrorAction()

    //  Last chance to perform special activity
    if (canSpecial && askYorN("\nDo you wish to perform a special activity? (y/n) "))
      executeSpecialActivity(faction, params, specialActivities)

  }
}
