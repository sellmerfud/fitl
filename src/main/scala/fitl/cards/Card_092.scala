
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
// Delta strategy: ARVN then US free Sweep in place or Assault in
// each space adjacent to Can Tho.
//
// Shaded Text
// Sampans: NVA or VC moves any of its pieces (including unTunneled Bases)
// from Cambodia/Tay Ninh to spaces adjacent to Can Tho.
//
// Tips
// ARVN and US each decide the details of their respective Sweeps or Assaults.
// "Spaces adjacent to Can Tho" include the 3 Mekong river LoC spaces.

object Card_092 extends EventCard(92, "SEALORDS",
  DualEvent,
  List(VC, US, NVA, ARVN),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Ignored -> Shaded))) {

  val adjacentToCanTho = spaces(getAdjacent(CanTho))
  val SweepOp   = "Sweep"
  val AssaultOp = "Assault"
  val Ops       = SweepOp::AssaultOp::Nil

  def canSweep(faction: Faction)(sp: Space) =
    !sp.isLoC &&
    (sp.sweepActivations(faction, NormalTroops) > 0)

  def canAssault(faction: Faction)(sp: Space) = {
    val baseFirstOK = if (game.isBot(faction))
      Bot.canUseAbramsUnshaded(faction)
    else
      Human.canUseAbramsUnshaded(faction)
      
    val numPatton = if (game.isBot(faction))
      Bot.m48PattonCount
    else
      Human.m48PattonCount
    assaultEffective(faction, NormalTroops, baseFirstOK, false, numPatton)(sp)
  }

  def canAssultOrSweep(faction: Faction)(sp: Space) =
    canAssault(faction)(sp) || canSweep(faction)(sp)

  val unshadedCandidate = (sp: Space) =>
    canAssultOrSweep(ARVN)(sp) ||
    canAssultOrSweep(US)(sp)

  def humanUnshaded(actor: Faction, name: String): Unit = {
    def askOp(actor: Faction, name: String): String = {
      val sp = game.getSpace(name)
      val choices = List(
        choice(canSweep(actor)(sp), SweepOp, SweepOp),
        choice(canAssault(actor)(sp), AssaultOp, AssaultOp)
      ).flatten
      askMenu(choices, f"\nChoose $actor operation in $name:").head
    }

    askOp(actor, name) match {
      case SweepOp => sweepInPlace(name, actor, NormalTroops)
      case _       => Human.performAssault(actor, name, Params(event = true, free = true))
    }
  }

  def botUnshaded(actor: Faction, name: String): Unit = {
    // Bot will assault if:
    // - There are no underground guerrillas, or
    // - The assault will remove ALL active enemies.
    def willAssault(actor: Faction, name: String): Boolean = {
      val sp = game.getSpace(name)
      val baseFirstOK = Bot.canUseAbramsUnshaded(actor)
      
      canAssault(actor)(sp) &&
      (!sp.pieces.has(UndergroundGuerrillas) || 
       assaultKillsAllVulnerable(actor, NormalTroops, baseFirstOK, vulnerableTunnels = false, Bot.m48PattonCount)(sp))

    }

    if (willAssault(actor, name))
      Bot.performAssault(actor, name, Params(event = true, free = true))
    else
      sweepInPlace(name, actor, NormalTroops)
  }

  def unshadedEffective(faction: Faction): Boolean =
    adjacentToCanTho exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {

    def nextCandidate(candiates: List[String]): Unit = candiates match {
      case Nil =>
      case name::rest =>
        println(s"\nSEALORDS target: $name")
        println(separator(char = '='))
        for (actor <- List(ARVN, US)) {
          if (canAssultOrSweep(actor)(game.getSpace(name))) {
            if (game.isHuman(actor))
              humanUnshaded(actor, name)
            else
              botUnshaded(actor, name)
          }
        }
        nextCandidate(rest)
    }

    Bot.resetM48PattonSpaces()
    Human.resetM48PattonSpaces()
    loggingControlChanges {
      nextCandidate(spaceNames(adjacentToCanTho filter unshadedCandidate))
    }
  }

  def shadedSources = spaces(TayNinh::Cambodia)

  val botSpaceNeedsBase = (sp: Space) =>
    !sp.pieces.has(NVABases) &&
    sp.totalBases < 2

  val botSpaceNeedsTroops = (sp: Space) =>
    sp.pieces.has(NVABases) &&
    sp.pieces.totalOf(NVATroops) < 6

  val botSpaceNeedsGuerrillas = (sp: Space) =>
    sp.pieces.has(NVABases) &&
    sp.pieces.totalOf(NVAGuerrillas) < 2


  //  NVA Bot will attempt to move bases to these 2 population spaces
  def shadedBotTargets = spaces(KienPhong::KienHoa_VinhBinh::KienGiang_AnXuyen::Nil)


  def shadedBotBaseTargets = shadedBotTargets filter { sp =>
    !sp.pieces.has(NVABases) &&
    sp.totalBases < 2
  }

  def humanShaded(): Unit = {
    val actor = askSimpleMenu(NVA::VC::Nil, "\nChoose faction to execute the event:").head
    if (game.isBot(actor)) {
      if (actor == VC)
        log(s"\nVC Bot elects to not move any pieces")
      else
        botShaded()
    }
    else {
      val pieceTypes = if (actor == NVA)
        NVAPieces filterNot (_ == NVATunnel)
      else
        VCPieces filterNot (_ == VCTunnel)
      def nextMove(): Unit = {
        val origins = spaceNames(shadedSources filter (_.pieces.has(pieceTypes)))
        val choices = (origins map (n => n -> n)) :+ ("finished" -> "Finished moving pieces")
        askMenu(choices, s"\nMove $actor pieces out of which space:").head match {
          case "finished" =>
          case originName =>
            val origin = game.getSpace(originName)
            val onlyBases = origin.pieces.only(pieceTypes).except(BasePieces).isEmpty
            val dests   = if (onlyBases)
              spaceNames(adjacentToCanTho filter (_.canTakeBase))
            else
              spaceNames(adjacentToCanTho)
            if (dests.isEmpty)
              println("\nThere are no spaces adjacent to Can Tho with room for a base")
            else {
              val destChoices = (dests map (n => n -> n)) :+ ("cancel" -> s"Do not move pieces from $originName")
              askMenu(destChoices, s"\nMove pieces from $originName to which space:").head match {
                case "cancel" =>
                case destName =>
                  val dest          = game.getSpace(destName)
                  val originPieces  = origin.pieces.only(pieceTypes)
                  val originBases   = if (dest.canTakeBase) originPieces.only(BasePieces) else Pieces()
                  val originForces  = originPieces.except(BasePieces)
                  val maxBases      = originBases.total min (2 - dest.totalBases)
                  val maxTroops     = originForces.totalOf(NVATroops)
                  val maxGuerrillas = originForces.totalOf(Guerrillas)
                  val numBases      = if (maxBases == 0) 0 else askInt(s"\nMove how many bases to $destName", 0, maxBases)
                  val bases         = if (actor == NVA) Pieces(nvaBases = numBases) else Pieces(vcBases = numBases)
                  val numForces     = (maxTroops > 0, maxGuerrillas > 0) match {
                    case (true, true)   => askInt(s"\nMove how many Troops/Guerrillas to $destName", 0, maxTroops + maxGuerrillas)
                    case (true, false)  => askInt(s"\nMove how many Troops to $destName", 0, maxTroops)
                    case (false, true)  => askInt(s"\nMove how many Guerrillas to $destName", 0, maxGuerrillas)
                    case (false, false) => 0
                  }
                  val forces = askPieces(originForces, numForces, prompt = Some(s"\nMoving forces to $destName"))

                  movePieces(bases + forces, originName, destName)
              }
            }
            nextMove()
        }
      }
      
      val baseType: PieceType = (pieceTypes find BasePieces.contains).get
      val moveableForce = shadedSources exists (_.pieces.except(baseType).has(pieceTypes))
      val moveableBase  = shadedSources exists (_.pieces.has(baseType))
      val baseTarget    = adjacentToCanTho exists (_.canTakeBase)

      if (moveableForce || (moveableBase && baseTarget))
        nextMove()
      else
        println(s"\nThere are no $actor pieces that can be moved")
    }
  }

  // NVA Bot instructions
  // Get 1 NVA Base to each 2-Pop space, then get 6 NVA Troops
  // and 2 NVA Guerrillas to each space with an NVA Base.
  def botShaded(): Unit = {

    def moveBases(): Unit = {
      val origins = shadedSources filter (_.pieces.has(NVABase))
      val dests   = shadedBotTargets filter botSpaceNeedsBase

      if (origins.nonEmpty && dests.nonEmpty) {
        val origin = Bot.pickSpaceWithMostPieces(Set(NVABase))(origins)
        val dest   = NVA_Bot.pickSpacePlaceBases(dests)
        movePieces(Pieces(nvaBases = 1), origin.name, dest.name)
        moveBases()
      }
    }

    def moveTroops(): Unit = {
      val origins = shadedSources filter (_.pieces.has(NVATroops))
      val dests   = shadedBotTargets filter botSpaceNeedsTroops

      if (origins.nonEmpty && dests.nonEmpty) {
        val origin = Bot.pickSpaceWithMostPieces(Set(NVATroops))(origins)
        val dest   = NVA_Bot.pickSpacePlaceTroops(dests)
        movePieces(Pieces(nvaTroops = 1), origin.name, dest.name)
        moveTroops()
      }
    }

    def moveGuerrillas(): Unit = {
      val origins = shadedSources filter (_.pieces.has(NVAGuerrillas))
      val dests   = shadedBotTargets filter botSpaceNeedsGuerrillas

      if (origins.nonEmpty && dests.nonEmpty) {
        val origin = Bot.pickSpaceWithMostPieces(NVAGuerrillas)(origins)
        val dest   = NVA_Bot.pickSpacePlaceGuerrillas(dests)
        val guerrilla = Bot.selectFriendlyToPlaceOrMove(origin.pieces.only(NVAGuerrillas), 1)
        movePieces(guerrilla, origin.name, dest.name)
        moveGuerrillas()
      }
    }

    moveBases()
    moveTroops()
    moveGuerrillas()
  }


  // NVA Bot instructions
  // Get 1 NVA Base to each 2-Pop space, then get 6 NVA Troops
  // and 2 NVA Guerrillas to each space with an NVA Base.
  def shadedEffective(faction: Faction): Boolean = {
    val hasBasesToMove  = shadedSources exists (_.pieces.has(NVABase))
    val hasForcesToMove = shadedSources exists (_.pieces.has(NVAForces))
    val hasBaseDest     = shadedBotTargets exists botSpaceNeedsBase
    val hasForcesDest   = shadedBotTargets exists (sp => botSpaceNeedsTroops(sp) || botSpaceNeedsGuerrillas(sp))

    (hasBasesToMove && hasBaseDest) || (hasForcesToMove && hasForcesDest)
  }

  def executeShaded(faction: Faction): Unit = {
    loggingControlChanges {
      if (game.isHuman(faction))
        humanShaded()
      else
        botShaded()
    }
  }
}
