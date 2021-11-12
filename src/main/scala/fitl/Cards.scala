
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

import scala.collection.immutable.ListMap
import scala.util.Random.shuffle
import FireInTheLake._
import Bot.{ US_Bot, ARVN_Bot, NVA_Bot, VC_Bot }

object Cards {
  val SingleEvent = false
  val DualEvent   = true

  // Temporary Functions
  private def unshadedNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Unshaded event not yet implemented")
  }

  private def shadedNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Shaded event not yet implemented")
  }
  private def singleNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Event not yet implemented")
  }
  private def coupNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Coup event not yet implemented")
  }

  private def pivotalNotYet(faction: Faction): Unit = {
    log(s"\n${deck(game.currentCard)}: $faction pivotal event not yet implemented")
  }

  // ------------------------------------------------------------------------
  // Helper functions

  val LowlandTouchingMekong = List(KienPhong, KienHoa_VinhBinh, BaXuyen, KienGiang_AnXuyen)
  
  // Returns true if an air strike would be effective in the space.
  val canAirStrike = (sp: Space) =>
      sp.pieces.has(CoinPieces) &&
      numExposedInsurgents(sp.pieces) > 0

  def airStrikeEffective: Boolean = {
    val prohibited = momentumInPlay(Mo_RollingThunder) ||
                     momentumInPlay(Mo_DaNang)         ||
                     momentumInPlay(Mo_BombingPause)

    if (prohibited)
      false
    else if (capabilityInPlay(ArcLight_Unshaded))
      game.spaces exists (sp => numExposedInsurgents(sp.pieces) > 0)
    else
      game.spaces exists canAirStrike
  }

  val isBlowtorchKomerSpace = (sp: Space) =>
    sp.support > ActiveOpposition &&
    sp.pieces.has(USTroops::ARVNTroops::Nil) &&
    sp.pieces.has(ARVNPolice)

  val isClaymoresSpace = (sp: Space) =>
    sp.pieces.has(CoinBases) &&
    sp.pieces.has(UndergroundGuerrillas)

  val isAmericalVCSpace = (sp: Space) =>
    sp.isProvince &&
    sp.population > 0 &&
    sp.pieces.has(USTroops) &&
    sp.pieces.has(VCBase::VCGuerrillas)  // Not Tunneled bases

  val isStarliteUnshadedSpace = (sp: Space) =>
    sp.isProvince &&
    sp.coastal    &&
    sp.pieces.has(VCPieces) &&
    withOrAdjacentExists(sp.name)(_.pieces.has(USTroops))

  val isPhoenixUnshadedSpace = (sp: Space) =>
    !sp.isLoC         &&
    sp.coinControlled &&
    sp.pieces.except(VCTunnel).has(VCPieces)

  val isPhoenixShadedSpace = (sp: Space) =>
    !sp.isLoC               &&
    sp.name != Saigon       &&
    sp.coinControlled       &&
    sp.pieces.has(VCPieces) &&
    (game.terrorMarkersAvailable > 0 || sp.support != ActiveOpposition)

  val isTribesmenUnshadedSpace = (sp: Space) =>
    sp.pieces.has(Irregulars) &&
    sp.pieces.has(NVABase::VCBase::InsurgentForces)

  val isUSSNewJerseyShadedSpace = (sp: Space) =>
    sp.isProvince &&
    sp.coastal    &&
    sp.pieces.has(USTroops) &&
    sp.population > 0 &&
    sp.support != ActiveOpposition

  // Remove the given number pieces from the map.
  // US pieces are removed to casualties unless available is specified
  // all other pieces are removed to available.
  // Returns the set of spaces where pieces were removed.
  def removePiecesFromMap(
    faction: Faction,
    numToRemove: Int,
    pieceTypes: TraversableOnce[PieceType],
    friendly: Boolean,
    validSpaces: TraversableOnce[String],
    usToAvailable: Boolean = false): Set[String] = {
    val validNames = validSpaces.toSet
    val hasPieces = (sp: Space) => validNames(sp.name) && sp.pieces.has(pieceTypes)
    val desc = if (pieceTypes.size > 3)
      "pieces"
    else
      andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter hasPieces)
      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc removed: ${numToRemove - numRemaining} of ${numToRemove}")
        val name     = askCandidate(s"Remove $desc from which space: ", candidates)
        val sp       = game.getSpace(name)
        val pieces   = sp.pieces.only(pieceTypes)
        val num      = askInt(s"Remove how many pieces from $name", 0, numRemaining min pieces.total)
        val toRemove = askPieces(pieces, num)
        if (num > 0) {
          if (usToAvailable)
            removeToAvailable(sp.name, toRemove)
          else
            removePieces(name, toRemove)
          spacesUsed += name
        }
        nextHumanRemoval(numRemaining - num)
      }
    }

    def nextBotRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = game.spaces filter hasPieces
      if (candidates.nonEmpty) {
        val sp = if (friendly)
          Bot.pickSpaceRemoveFriendlyPieces(candidates, pieceTypes)
        else
          Bot.pickSpaceRemoveReplace(faction)(candidates)
        val pieces   = sp.pieces.only(pieceTypes)
        val toRemove = if (friendly)
          Bot.selectFriendlyRemoval(pieces, 1)
        else
          Bot.selectEnemyRemovePlaceActivate(pieces, 1)

        if (usToAvailable)
          removeToAvailable(sp.name, toRemove)
        else
          removePieces(sp.name, toRemove)
        spacesUsed += sp.name
        nextBotRemoval(numRemaining - 1)
      }
    }

    def removeAll(): Unit = {
      for {
        name <- validSpaces
        sp   = game.getSpace(name)
        dead = sp.pieces.only(pieceTypes)
        if dead.nonEmpty
      } {
        if (usToAvailable)
          removeToAvailable(name, dead)
        else
          removePieces(name, dead)
        spacesUsed += name
      }
    }

    val totalPieces = spaces(validSpaces).foldLeft(0)((sum, sp) => sum + sp.pieces.totalOf(pieceTypes))

    if (totalPieces <= numToRemove)
      removeAll()
    else if (game.isHuman(faction))
      nextHumanRemoval(numToRemove)
    else
      nextBotRemoval(numToRemove)
    spacesUsed
  }

  def removePiecesToOutOfPlay(
    faction: Faction,
    numToRemove: Int,
    pieceTypes: TraversableOnce[PieceType],
    friendly: Boolean,
    validSpaces: TraversableOnce[String]): Set[String] = {
    val validNames = validSpaces.toSet
    val hasPieces = (sp: Space) => validNames(sp.name) && sp.pieces.has(pieceTypes)
    val desc = andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter hasPieces)
      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc removed to Out of Play: ${numToRemove - numRemaining} of ${numToRemove}")
        val name     = askCandidate(s"Remove $desc from which space: ", candidates)
        val sp       = game.getSpace(name)
        val pieces   = sp.pieces.only(pieceTypes)
        val num      = askInt(s"Remove how many pieces from $name", 0, numRemaining min pieces.total)
        val toRemove = askPieces(pieces, num)
        if (num > 0) {
          removeToOutOfPlay(name, toRemove)
          spacesUsed += name
        }
        nextHumanRemoval(numRemaining - num)
      }
    }

    def nextBotRemoval(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = game.spaces filter hasPieces
      if (candidates.nonEmpty) {
        val sp = if (friendly)
          Bot.pickSpaceRemoveFriendlyPieces(candidates, pieceTypes)
        else
          Bot.pickSpaceRemoveReplace(faction)(candidates)
        val pieces   = sp.pieces.only(pieceTypes)
        val toRemove = if (friendly)
          Bot.selectFriendlyRemoval(pieces, 1)
        else
          Bot.selectEnemyRemovePlaceActivate(pieces, 1)
        removeToOutOfPlay(sp.name, toRemove)
        spacesUsed += sp.name
        nextBotRemoval(numRemaining - 1)
      }
    }

    def removeAll(): Unit = {
      for {
        name <- validSpaces
        sp   = game.getSpace(name)
        dead = sp.pieces.only(pieceTypes)
        if dead.nonEmpty
      } {
        removeToOutOfPlay(name, dead)
        spacesUsed += name
      }
    }

    val totalPieces = spaces(validSpaces).foldLeft(0)((sum, sp) => sum + sp.pieces.totalOf(pieceTypes))

    if (totalPieces <= numToRemove)
      removeAll()
    else if (game.isHuman(faction))
      nextHumanRemoval(numToRemove)
    else
      nextBotRemoval(numToRemove)
    spacesUsed
  }

  // Place pieces from available
  // Returns the set of spaces where pieces were placed
  def placePiecesOnMap(faction: Faction, numToPlace: Int, pieceTypes: TraversableOnce[PieceType],
                          validSpaces: TraversableOnce[String]): Set[String] = {
    val validNames = validSpaces.toSet
    val isValid = (sp: Space) => validNames(sp.name)
    val canTakeBase  = (sp: Space) => isValid(sp) && sp.totalBases < 2
    val desc = andList(pieceTypes map (_.genericPlural))
    var spacesUsed = Set.empty[String]

    def nextHumanPlacement(numRemaining: Int): Unit = if (numRemaining > 0) {
      val bases      = game.piecesToPlace.only(pieceTypes).only(BasePieces)
      val forces     = game.piecesToPlace.only(pieceTypes).except(BasePieces)
      val candidates = if (forces.nonEmpty)
        spaceNames(game.spaces filter isValid)
      else
        spaceNames(game.spaces filter canTakeBase)

      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc placed: ${numToPlace - numRemaining} of ${numToPlace}")
        val name      = askCandidate(s"Place $desc in which space: ", candidates)
        val sp        = game.getSpace(name)
        val placeBase = bases.nonEmpty && canTakeBase(sp) &&
                        askYorN(s"Do you wish to place a base in $name? (y/n) ")
        val pieces    = if (placeBase)
          askToPlaceBase(name, bases.explode().head)
        else
          askPiecesToPlace(name, forces.getTypes, numRemaining)
        if (pieces.nonEmpty) {
          placePieces(name, pieces)
          spacesUsed += name
        }
        nextHumanPlacement(numRemaining - pieces.total)
      }
    }

    def nextBotPlacement(numRemaining: Int, availPieces: Pieces): Unit = if (numRemaining > 0 && availPieces.nonEmpty) {
      val piece = Bot.selectFriendlyToPlaceOrMove(availPieces, 1)
      val optSpace = if (piece.has(BasePieces)) {
        val candidates = game.spaces filter canTakeBase
        if (candidates.nonEmpty)
          Some(Bot.pickSpacePlaceBases(faction)(candidates))
        else
          None
      }
      else {
        val candiates = game.spaces filter isValid
        if (candiates.nonEmpty) {
          val isTroop = piece.has(USTroops::NVATroops::ARVNTroops::Nil)
          Some(Bot.pickSpacePlaceForces(faction, isTroop)(candiates))
        }
        else
          None
      }
      optSpace match {
        case Some(sp) =>
          placePieces(sp.name, piece)
          spacesUsed += sp.name
          nextBotPlacement(numRemaining - 1, availPieces - piece)
        case None =>
          // It is possible that there are base available but no
          // space can accomodate a base, so remove the piece from
          // consideration and continue
          nextBotPlacement(numRemaining, availPieces - piece)
      }
    }

    if (game.isHuman(faction))
      nextHumanPlacement(numToPlace min game.piecesToPlace.only(pieceTypes).total)
    else
      nextBotPlacement(numToPlace, game.availablePieces.only(pieceTypes))
    spacesUsed
  }

  // Move the given number pieces one or more spaces on the map to
  // the given destination space.
  def moveMapPiecesToSpace(
    faction: Faction,
    numToMove: Int,
    destName: String,
    pieceTypes: TraversableOnce[PieceType],
    validSpaces: TraversableOnce[String]): Unit = {
    val validNames = validSpaces.toSet - destName
    val hasPieces = (sp: Space) => validNames(sp.name) && sp.pieces.has(pieceTypes)
    val desc = andList(pieceTypes)

    def nextHumanMove(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = spaceNames(game.spaces filter hasPieces)
      if (candidates.nonEmpty) {
        println(s"\nNumber of $desc moved to $destName: ${numToMove - numRemaining} of ${numToMove}")
        val name   = askCandidate("Move pieces from which space: ", candidates)
        val sp     = game.getSpace(name)
        val pieces = sp.pieces.only(pieceTypes)
        val num    = askInt(s"Move how many pieces from $name", 0, numRemaining min pieces.total)
        val toMove = askPieces(pieces, num)
        movePieces(toMove, name, destName)
        nextHumanMove(numRemaining - num)
      }
    }

    def nextBotMove(numRemaining: Int): Unit = if (numRemaining > 0) {
      val candidates = game.spaces filter hasPieces
      if (candidates.nonEmpty) {
        val sp = Bot.pickSpacePlaceForces(faction)(candidates)
        val pieces   = sp.pieces.only(pieceTypes)
        val toMove = Bot.selectFriendlyToPlaceOrMove(pieces, 1)
        movePieces(toMove, sp.name, destName)
        nextBotMove(numRemaining - 1)
      }
    }

    if (game.isHuman(faction))
      nextHumanMove(numToMove)
    else
      nextBotMove(numToMove)
  }


  // Convenience method for adding a card to the deck.
  private def entry(card: EventCard) = (card.number -> card)

  val deckMap: Map[Int, EventCard] = Map(
    // ------------------------------------------------------------------------
    entry(new EventCard(1, "Gulf of Tonkin", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        airStrikeEffective || game.outOfPlay.has(USPieces)
      },
      // executeUnshaded()
      (faction: Faction) => {
        val numCasualties = game.casualties.totalOf(USPieces) min 6
        if (game.isHuman(US)) {
          Human.doAirStrike(Params(event = true))
          Human.moveUSOutOfPlayToCities(6)
        }
        else {
          US_Bot.airStrikeActivity(Params(event = true))
          US_Bot.moveOutOfPlayToCities(6)
        }
      },
      // shadedEffective()
      (faction: Faction) => {
        isInsurgent(faction) && game.casualties.nonEmpty
      },
      // executeShaded()
      (faction: Faction) => {
        // Aid -1 per Casualty.  All Casualties out of play.
        decreaseUsAid(game.casualties.total)
        moveAllCasualtiesToOutOfPlay(game.casualties)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(2, "Kissinger", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        spaces(LaosCambodia).exists(_.pieces.except(InsurgentTunnels).has(InsurgentPieces))
      },
      // executeUnshaded()
      (faction: Faction) => {
        val pieceTypes = InsurgentPieces.toSet -- InsurgentTunnels.toSet
        val num = d6
        log(s"\nRolling d6 to determine number of pieces to remove: $num")
        removePiecesFromMap(faction, num, pieceTypes, false, LaosCambodia)
      },
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => {
        val cambodia = Set(NortheastCambodia, TheFishhook, TheParrotsBeak, Sihanoukville)
        placePiecesOnMap(NVA, 2, NVAPieces, cambodia)
        removePiecesToOutOfPlay(US, 2, Set(USTroops), false, spaceNames(game.spaces))
        decreaseUsAid(6)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(3, "Peace Talks", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.pivotCardsAvailable(US),
      // executeUnshaded()
      (faction: Faction) => {
        decreaseResources(NVA, 9)
        if (game.pivotCardsAvailable(US)) {
          log("\nPlace the \"Peace Talks\" marker on the Linebacker II pivotal event card")
          game = game.copy(peaceTalks = true)
        }
      },
      // shadedEffective()
      (faction: Faction) => {
        // Bot does not play to increment resources since they are not used.
        game.trail <= 2
      },
      // executeShaded()
      (faction: Faction) => {
        increaseResources(NVA, 9)
        if (game.trail <= 2)
          improveTrail(3 - game.trail)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(4, "Top Gun", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => {
        playCapability(TopGun_Unshaded)
        if (capabilityInPlay(MiGs_Shaded))
          removeCapabilityFromPlay(MiGs_Shaded)
      },
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(TopGun_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(5, "Wild Weasels", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        capabilityInPlay(SA2s_Shaded) ||
        game.trail > TrailMin         ||
        (game.trackResources(NVA) && game.nvaResources > 0)
      },
      // executeUnshaded()
      (faction: Faction) => {
        if (capabilityInPlay(SA2s_Shaded))
          removeCapabilityFromPlay(SA2s_Shaded)
        else {
          degradeTrail(2)
          decreaseResources(NVA, 9)
        }
      },
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playMomentum(Mo_WildWeasels)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(6, "Aces", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        val validSpaces = spaces(NorthVietnam::LaosCambodia)
        val canStrike = validSpaces exists (sp => vulnerableInsurgents(sp.pieces, false).nonEmpty)
        canStrike || game.trail > TrailMin
      },
      // executeUnshaded()
      (faction: Faction) => {
        val validNames = NorthVietnam::LaosCambodia
        val params = Params(
          event        = true,
          maxSpaces    = Some(1),
          strikeParams = AirStrikeParams(maxHits = Some(6), noCoin = true),
          onlyIn       = Some(validNames.toSet)
        )

        if (game.isHuman(US))
          Human.doAirStrike(params)
        else
          US_Bot.airStrikeActivity(params)
        degradeTrail(2)
      },
      // shadedEffective()
      (faction: Faction) => {
        game.availablePieces.has(USTroops) || game.trail < TrailMax
      },
      // executeShaded()
      (faction: Faction) => {
        val numTroops = game.availablePieces.totalOf(USTroops) min 2
        moveAvailableToCasualties(Pieces(usTroops = numTroops))
        improveTrail(2)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(7, "ADSID", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.trackResources(NVA),
      // executeUnshaded()
      (faction: Faction) => playMomentum(Mo_ADSID),
      // shadedEffective()
      (faction: Faction) => {
        game.trail < TrailMax || (game.trackResources(ARVN) && game.arvnResources > 0)
      },
      // executeShaded()
      (faction: Faction) => {
        if (game.trail < TrailMax) {
          val num = if (game.trail < 2) 2 - game.trail else 1
          improveTrail(num)
          decreaseResources(ARVN, 9)
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(8, "Arc Light", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(ArcLight_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(ArcLight_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(9, "Psychedelic Cookie", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        // For Nixon policy, US bot will move troops to available (from out of play first)
        // For LBJ and JFK it will move troops to S. Vietnam
        if (game.usPolicy == USPolicy_Nixon)
          game.outOfPlay.has(USTroops) || game.totalOnMap(_.pieces.totalOf(USTroops)) > 0
        else
          game.outOfPlay.has(USTroops)
      },
      // executeUnshaded()
      (faction: Faction) => {
        val numOutOfPlay = game.outOfPlay.totalOf(USTroops) min 3
        val numOnMap     = game.totalOnMap(_.pieces.totalOf(USTroops)) min 3
        if (game.isHuman(US)) {
          def outOfPlayToAvailableOrMap(numRemaining: Int): Unit = if (numRemaining > 0) {
            val choices = List(
              "available" -> "Move troops from Out of play to Available",
              "map"       -> "Move troops form Out of play to South Vietnam",
              "finished"  -> "Finished moving troops from Out of play"
            )
            println(s"\n${numOnMap - numRemaining} of $numOnMap troops moved from Out of play")
            askMenu(choices, "\nChoose one:").head match {
              case "available" =>
                val num = askInt("Move how many troops to Available", 0, numRemaining)
                moveOutOfPlayToAvailable(Pieces(usTroops = num))
                outOfPlayToAvailableOrMap(numRemaining - num)

              case "map" =>
                val candidates = spaceNames(game.spaces filter (sp => isInSouthVietnam(sp.name)))
                val name = askCandidate("\nMove troops to which space: ", candidates)
                val num = askInt(s"Move how many troops to $name", 0, numRemaining)
                moveOutOfPlayToMap(Pieces(usTroops = num), name)
                outOfPlayToAvailableOrMap(numRemaining - num)
                
              case _ =>
            }
          }

          def mapToAvailable(numRemaining: Int): Unit = if (numRemaining > 0) {
            val candidates = spaceNames(game.spaces filter (_.pieces.has(USTroops)))
            if (candidates.nonEmpty) {
              println(s"\n${numOnMap - numRemaining} of $numOnMap troops removed to Available")
              if (askYorN("\nDo you wish to remove troops to Available? (y/n) ")) {
                val name   = askCandidate("\nRemove troops from which space: ", candidates)
                val sp     = game.getSpace(name)
                val maxNum = sp.pieces.totalOf(USTroops) min numRemaining
                val num    = askInt(s"Remove how many troops from $name", 0, maxNum)
                removeToAvailable(name, Pieces(usTroops = num))
                mapToAvailable(numRemaining - num)
              }
            }
          }

          val choices = List(
            "from-oop" -> "Move US Troops from Out of Play to Available or South Vietname",
            "from-map" -> "Move US Troops from the map to Available"
          )
          askMenu(choices, "\nChoose one:").head match {
            case "from-oop" => loggingControlChanges(outOfPlayToAvailableOrMap(numOutOfPlay))
            case _          => loggingControlChanges(mapToAvailable(numOnMap))
          }
        }
        else if (game.usPolicy == USPolicy_Nixon) {
          if (numOutOfPlay > 0)
            moveOutOfPlayToAvailable(Pieces(usTroops = numOutOfPlay))
          else {
            loggingControlChanges {
              for (i <- 1 to numOnMap) {
                val candidates = game.spaces filter (_.pieces.has(USTroops))
                val sp = Bot.pickSpaceRemoveFriendlyPieces(candidates, Set(USTroops))
                removeToAvailable(sp.name, Pieces(usTroops = 1))
              }
            }
          }
        }
        else { // JFK or LBJ
          loggingControlChanges{
            for (i <- 1 to numOutOfPlay) {
                val candidates = game.spaces filter (sp => isInSouthVietnam(sp.name))
                val sp = Bot.pickSpacePlaceForces(US, troops = true)(candidates)
                moveOutOfPlayToMap(Pieces(usTroops = 1), sp.name)
            }
          }
        }
      },
      // shadedEffective()
      (faction: Faction) => game.totalOnMap(_.pieces.totalOf(USTroops)) > 0,
      // executeShaded()
      (faction: Faction) => {
        val totalTroops = game.totalOnMap(_.pieces.totalOf(USTroops)) min 3

        if (game.isHuman(US)) {

          def removeFromMap(numRemaining: Int): Unit = if (numRemaining > 0) {
              val candidates = spaceNames(game.spaces filter (_.pieces.has(USTroops)))
              println(s"\n${totalTroops - numRemaining} of $totalTroops troops removed to Out of Play")
              val name = askCandidate("\nRemove troops from which space: ", candidates)
                val num = askInt(s"Remove how many troops from $name", 0, numRemaining)
                removeToOutOfPlay(name, Pieces(usTroops = num))
                removeFromMap(numRemaining - num)

          }

          loggingControlChanges(removeFromMap(totalTroops))
        }
        else {
          loggingControlChanges {
            for (i <- 1 to totalTroops) {
              val candidates = game.spaces filter (_.pieces.has(USTroops))
              val sp = Bot.pickSpaceRemoveFriendlyPieces(candidates, Set(USTroops))
              removeToOutOfPlay(sp.name, Pieces(usTroops = 1))
            }
          }
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(10, "Rolling Thunder", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => {
        degradeTrail(2)
        decreaseResources(NVA, 9)
        makeIneligibleThroughNextTurn(NVA)
      },
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => {
        decreaseResources(ARVN, 5)
        playMomentum(Mo_RollingThunder)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(11, "Abrams", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(Abrams_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(Abrams_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(12, "Capt Buck Adams", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        game.spaces exists { sp =>
          isOutsideSouth(sp.name) &&
          sp.pieces.has(NVABase::UndergroundGuerrillas)
        }
      },
      // executeUnshaded()
      (faction: Faction) => {
        for (sp <- game.spaces; if isOutsideSouth(sp.name) && sp.pieces.has(UndergroundGuerrillas)) {
          val underground = sp.pieces.only(UndergroundGuerrillas)
          revealPieces(sp.name, underground)
        }
        
        val baseSpaces = game.spaces filter { sp =>
          isOutsideSouth(sp.name) && sp.pieces.has(NVABase)
        }
        if (baseSpaces.nonEmpty)
          removePiecesFromMap(faction, 1, Set(NVABase), false, spaceNames(baseSpaces))
      },
      // shadedEffective()
      (faction: Faction) => {
        val canPlaceBase = 
          game.availablePieces.has(NVABase) &&
          game.spaces.exists { sp =>
            isOutsideSouth(sp.name) &&
            sp.totalBases < 2 &&
            sp.nvaControlled
        }

        canPlaceBase || game.totalOnMap(_.pieces.totalOf(NVAGuerrillas_A)) > 0
      },
      // executeShaded()
      (faction: Faction) => {
        val baseSpaces = game.spaces filter { sp =>
          isOutsideSouth(sp.name) && sp.totalBases < 2
        }
        def guerrillaSpaces = game.spaces filter (sp => sp.pieces.has(NVAGuerrillas_A))

        def humanFlip(numRemaining: Int): Unit = if (numRemaining > 0) {
          val totalActive = baseSpaces.map(_.pieces.totalOf(NVAGuerrillas_A)).sum
          if (totalActive == numRemaining) {
            for (sp <- baseSpaces)
              hidePieces(sp.name, sp.pieces.only(NVAGuerrillas_A))
          }
          else {
            val candidates = spaceNames(guerrillaSpaces)
            if (candidates.nonEmpty) {
              val name   = askCandidate("Flip guerrillas in which space: ", candidates)
              val maxNum = game.getSpace(name).pieces.totalOf(NVAGuerrillas_A) min numRemaining
              val num  = askInt("Flip how many guerrillas in $name", 0, maxNum)
              hidePieces(name, Pieces(nvaGuerrillas_A = num))
              humanFlip(numRemaining - num)
            }
          }
        }

        def botFlip(numRemaining: Int): Unit = if (numRemaining > 0) {
          val candidates = guerrillaSpaces
          if (candidates.nonEmpty) {
            // Event instructions say to use Place Guerrillas priority
            val sp = NVA_Bot.pickSpacePlaceGuerrillas(candidates)
            hidePieces(sp.name, Pieces(nvaGuerrillas_A = 1))
            botFlip(numRemaining - 1)
          }
        }

        placePiecesOnMap(faction, 1, Set(NVABase), spaceNames(baseSpaces))
        if (game.isHuman(faction))
          humanFlip(3)
        else
          botFlip(3)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(13, "Cobras", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical -> Unshaded),
              ARVN -> (Critical -> Unshaded),
              NVA  -> (Critical -> Shaded),
              VC   -> (Critical -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(Cobras_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(Cobras_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(14, "M-48 Patton", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(M48Patton_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(M48Patton_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(15, "Medevac", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playMomentum(Mo_Medevac_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playMomentum(Mo_Medevac_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(16, "Blowtorch Komer", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => {
        playMomentum(Mo_BlowtorchKomer)
        increaseUsAid(10)
      },
        
      // shadedEffective()
      (faction: Faction) => {
        game.usAid > 0 || (game.spaces exists isBlowtorchKomerSpace)
      },
      // executeShaded()
      (faction: Faction) => {
        decreaseUsAid(10)
        val candidates = game.spaces filter isBlowtorchKomerSpace
        if (candidates.nonEmpty) {
          val name = if (game.isHuman(faction))
            askCandidate("\nShift which space toward Active Opposition: ", spaceNames(candidates))
          else 
            VC_Bot.pickSpaceTowardActiveOpposition(candidates).name
          decreaseSupport(name, 1)
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(17, "Claymores", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => {
        remainEligibleNextTurn(faction)
        playMomentum(Mo_Claymores)
      },
      // shadedEffective()
      (faction: Faction) => game.nonLocSpaces exists isClaymoresSpace,
      // executeShaded()
      (faction: Faction) => {
        val candidates = game.nonLocSpaces filter isClaymoresSpace
        if (candidates.nonEmpty) {
          val name = if (game.isHuman(faction))
            askCandidate("\nRemove pieces from which space: ", spaceNames(candidates))
          else
            Bot.pickSpaceRemoveReplace(faction)(candidates).name
          val pieces = game.getSpace(name).pieces
          val (base, guerrilla) = if (game.isHuman(faction))
            (askPieces(pieces.only(CoinBases), 1, prompt = Some("Remove which base")),
            askPieces(pieces.only(UndergroundGuerrillas), 1, prompt = Some("Remove which guerrilla")))
          else
            (Bot.selectEnemyRemovePlaceActivate(pieces.only(CoinBases), 1),
             Bot.selectEnemyRemovePlaceActivate(pieces.only(UndergroundGuerrillas), 1))
          removePieces(name, base + guerrilla)
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(18, "Combined Action Platoons", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(CombActionPlatoons_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(CombActionPlatoons_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(19, "CORDS", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(CORDS_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(CORDS_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(20, "Laser Guided Bombs", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(LaserGuidedBombs_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(LaserGuidedBombs_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(21, "Americal", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        game.outOfPlay.totalOf(USTroops) > 0 ||
        game.totalOnMap(_.pieces.totalOf(USTroops)) > 0
      },
      // executeUnshaded()
      (faction: Faction) => {
        // Move up to 2 US Troops EACH from the map and Out of Play
        // All Troops moved must go to either Available or to a single space on the map.
        // US alwasy does the removal regardless of who is executing the event.
        val (destName: Option[String], oopTroops: Pieces, numMapTroops: Int) = if (game.isHuman(US)) {
          val destChoices = List("available" -> "Available", "map" -> "A space on the map")
          val destName = askMenu(destChoices, "\nChoose destination for US Troops:").head match {
            case "available" => None // Destination is the available Box
            case _ =>
              val candidates = spaceNames(game.spaces filterNot (_.isNorthVietnam))
              Some(askCandidate("Move US Troops to which space: ", candidates))
          }

          val maxOOP    = game.outOfPlay.totalOf(USTroops) min 2
          val numOOP    = askInt("\nMove how many US Troops from Out Of Play", 0, maxOOP)
          val oopTroops = Pieces(usTroops = numOOP)
          val origins   = game.spaces filter { sp =>
            Some(sp.name) != destName &&
            sp.pieces.has(USTroops)
          }
          val numOnMap  = (origins map (_.pieces.totalOf(USTroops))).sum
          val maxMap    = numOnMap min 2
          val numMap    = askInt("Move how many US Troops from the map", 0, maxMap)
          (destName, oopTroops, numMap)
        }
        else {
          // JFK/LBJ: move Troops to South Vietnam
          // Nixon:   move Troops to Available
          val destName = if (game.usPolicy == USPolicy_Nixon)
            None  // To available
          else
            Some(Bot.pickSpacePlaceForces(US)(game.spaces filter (sp => isInSouthVietnam(sp.name))).name)
          val oopTroops  = Pieces(usTroops = game.outOfPlay.totalOf(USTroops) min 2)
          val totalOnMap = game.totalOnMap(_.pieces.totalOf(USTroops))
          val numMap     = destName match {
            case None => totalOnMap min 2
            case Some(dest) => (totalOnMap - game.getSpace(dest).pieces.totalOf(USTroops)) min 2
          }
          (destName, oopTroops, numMap)
        }

        val originNames = spaceNames(game.spaces)
        loggingControlChanges {
          destName match {
            case None =>
              moveOutOfPlayToAvailable(oopTroops)
              removePiecesFromMap(US, numMapTroops, Set(USTroops), true, originNames, usToAvailable = true)
            case Some(dest) =>
              placePiecesFromOutOfPlay(dest, oopTroops)
              moveMapPiecesToSpace(US, numMapTroops, dest, Set(USTroops), originNames)
          }
        }
      },
      // shadedEffective()
      (faction: Faction) => {
        game.nonLocSpaces exists (sp => isAmericalVCSpace(sp) && sp.support != ActiveOpposition)
      },
      // executeShaded()
      (faction: Faction) => {
        if (game.isHuman(faction)) {
          def nextProvince(spaceNum: Int): Unit = if (spaceNum <= 2) {
            val candidates = spaceNames(game.nonLocSpaces filter (sp => isAmericalVCSpace(sp)))
            if (candidates.nonEmpty) {
              val choices = candidates.map(n => n -> n) :+ "finished" -> "Finished selecting spaces for the Americal Event"
              askMenu(choices, s"\nSelect ${ordinal(spaceNum)} Americal space:").head match {
                case "finished" => 
                case name =>
                  val sp      = game.getSpace(name)
                  val allowed = if (faction == VC) VCGuerrillas :+ VCBase else VCBase::VCGuerrillas
                  val piece   = askPieces(sp.pieces, 1, allowed, Some(s"\nSelecting piece to remove from $name"))
                  loggingControlChanges {
                    removePieces(name, piece)
                    setSupport(name, ActiveOpposition)
                  }
                  nextProvince(spaceNum + 1)
              }
            }
          }

          nextProvince(1)          
        }
        else {
          // VC Bot
          def nextProvince(numRemaining: Int): Unit = if (numRemaining > 0) {
            val candidates = game.nonLocSpaces filter (sp => isAmericalVCSpace(sp) && sp.support != ActiveOpposition)
            if (candidates.nonEmpty) {
              val sp    = Bot.pickSpaceRemoveFriendlyPieces(candidates, VCBase::VCGuerrillas)
              val piece = Bot.selectFriendlyRemoval(sp.pieces.only(VCBase::VCGuerrillas), 1)
              removePieces(sp.name, piece)
              setSupport(sp.name, ActiveOpposition)
              nextProvince(numRemaining - 1)
            }
          }
          nextProvince(2)
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(22, "Da Nang", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.outOfPlay.has(USTroops),
      // executeUnshaded()
      (faction: Faction) => {
        val maxOop   = game.outOfPlay.totalOf(USTroops)
        val maxAvail = game.availablePieces.totalOf(USTroops)
        val (numOop, numAvail) = if (game.isHuman(US)) {
          val numOop   = askInt("Place how many OUT OF PLAY US Troops in Da Nang", 0, 3 min maxOop)
          val numAvail = askInt("Place how many AVAILABLE US Troops in Da Nang", 0, 6 - numOop min maxAvail)
          (numOop, numAvail)
        }
        else
          (maxOop min 3, 0) // US Bot will only place troops from Out of Play
        
        placePiecesFromOutOfPlay(DaNang, Pieces(usTroops = numOop))
        placePieces(DaNang, Pieces(usTroops = numAvail))
      },
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => {
        loggingPointsChanges {
          for (sp <- withOrAdjacent(DaNang)(sp => !sp.isLoC && sp.support > Neutral))
            setSupport(sp.name, Neutral)
        }
        log()
        playMomentum(Mo_DaNang)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(23, "Operation Attleboro", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,  // Bot does not execute
      // executeUnshaded()
      (faction: Faction) => {
        val tunnelSpaces = game.nonLocSpaces filter (_.pieces.has(InsurgentTunnels))

        if (tunnelSpaces.nonEmpty) {
          val name = askCandidate("\nExecute event in which space: ", spaceNames(tunnelSpaces))
          val params = Params(event = true, singleTarget = Some(name), vulnerableTunnels = true)
          Human.doAirLift(params)
          Human.executeSweep(US, params)
          Human.performAssault(US, name, params)

          val arvnEffective = assaultEffective(ARVN, true)(game.getSpace(name))
          if (arvnEffective && askYorN(s"\nFollow up with ARVN assault in $name? (y/n) ")) {
            log(s"\nUS adds a follow up ARVN asault in $name")
            Human.performAssault(ARVN, name, params)
          }
        }
        else
          log("There are no spaces with a Tunneled base.")
      },
      // shadedEffective()
      (faction: Faction) => {
        game.nonLocSpaces exists { sp =>
          sp.pieces.has(InsurgentTunnels) &&
          withOrAdjacentExists(sp.name)(_.pieces.has(USTroops))
        }
      },
      // executeShaded()
      (faction: Faction) => {
        val tunnelSpaces = game.nonLocSpaces filter (_.pieces.has(InsurgentTunnels))

        val name = if (game.isHuman(faction))
          askCandidate("\nExecute event in which space: ", spaceNames(tunnelSpaces))
        else {
          val priorities = List(
            new Bot.HighestScore[Space]("Most US troops in/adjacent", sp => {
              withOrAdjacentFold(sp.name, 0)((sum, sp) => sp.pieces.totalOf(USTroops))
            })
          )
          Bot.bestCandidate(tunnelSpaces, priorities).name
        }
        
        val num = d6
        log(s"\nRolling d6 to determine number of US Troops to remove: $num")
        removePiecesFromMap(faction, num, Set(USTroops), false, getAdjacent(name) + name)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(24, "Operation Starlite", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.spaces exists isStarliteUnshadedSpace,
      // executeUnshaded()
      (faction: Faction) => {
        val candidates = game.spaces filter isStarliteUnshadedSpace
        val name = if (game.isHuman(faction)) {
          if (candidates.nonEmpty)
            Some(askCandidate("\nExecute event in which space: ", spaceNames(candidates)))
          else
            None
        }
        else {
          //  I am narrowing the selection to spaces that contain
          //  a VC base (if any) because that makes more sense.
          val withVCBase = List(new Bot.BooleanPriority[Space]("With VC Base", _.pieces.has(VCBase)))
          val narrowed = Bot.narrowCandidates(candidates, withVCBase)
          Some(Bot.pickSpaceRemoveReplace(faction)(narrowed).name)
        }

        name match {
          case Some(name) =>
            val sp = game.getSpace(name)
            removePieces(name, sp.pieces.only(VCPieces))
          case None =>
            log("There are no coastal spaces that qualify.")
        }
      },
      // shadedEffective()
      (faction: Faction) => {
        game.spaces exists { sp => sp.isProvince && sp.pieces.has(VCGuerrillas_A) }
      },
      // executeShaded()
      (faction: Faction) => {
        def nextSpace(count: Int, total: Int, candidates: List[Space]): List[String] = {
          if (count > total || candidates.isEmpty)
            Nil
          else {
            val name = if (game.isHuman(faction))
              askCandidate(s"Select ${ordinal(count)} space: ", spaceNames(candidates))
            else
              VC_Bot.pickSpacePlaceGuerrillas(candidates).name
            name::nextSpace(count + 1, total, candidates filterNot (_.name == name))
          }
        }

        val candidates = game.spaces filter { sp => sp.isProvince && sp.pieces.has(VCGuerrillas_A) }
        val maxNum = candidates.size min 3
        if (candidates.nonEmpty) {
          val num = if (game.isHuman(faction))
            askInt("\nExecute event in how many Provinces", 0, maxNum)
          else
            maxNum
          if (num == candidates.size) {
            for (sp <- candidates)
              hidePieces(sp.name, sp.pieces.only(VCGuerrillas_A))
          }
          else {
            for (name <- nextSpace(1, num, candidates).reverse) {
              val sp = game.getSpace(name)
              hidePieces(name, sp.pieces.only(VCGuerrillas_A))
            }
          }
        }
        else
          log("There are no Provinces with underground VC Guerrillas.")
        remainEligibleNextTurn(faction)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(25, "TF-116 Riverines", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => {
        val emptyGroups = new MovingGroups()
        
        (spaces(MekongLoCs) exists (_.pieces.has(InsurgentPieces))) ||
        (LowlandTouchingMekong exists { name =>
          val sp = game.getSpace(name)
          sp.pieces.has(InsurgentPieces) &&
          (sp.pieces.has(ARVNForces) || sweepSources(name, ARVN, emptyGroups).nonEmpty)
        })
      },
      // executeUnshaded()
      (faction: Faction) => {
          var removedSome = false
          for (name <- MekongLoCs) {
            val sp       = game.getSpace(name)
            val toRemove = sp.pieces.only(InsurgentPieces)
            if (toRemove.nonEmpty && !removedSome) {
              removedSome = true
              log("Remove all NVA/VC from Mekong LoCs")
            }
            removePieces(name, toRemove)
          }
          if (!removedSome)
            log("There are no NVA/VC pieces on Mekong LoCs")

        if (game.isHuman(faction)) {
          def nextSpace(assaulter: Faction, candidates: List[String]): Unit = if (candidates.nonEmpty) {
            val choices = (candidates map (n => n -> n)) :+
                 ("finished" -> "Do not Sweep/Assault any more spaces")
            askMenu(choices, "\nSelect next space to Sweep/Assault:").head match {
              case "finished" =>
              case name =>
                val params = Params(event = true, free = true, singleTarget = Some(name))
                Human.initTurnVariables(params)
                Human.executeSweep(assaulter, params)
                Human.initTurnVariables(params)
                Human.performAssault(assaulter, name, params)
                val addArvnAssault =
                  assaulter == US &&
                  assaultEffective(ARVN, false)(game.getSpace(name)) &&
                  askYorN(s"\nAdd a free ARVN assault in $name? (y/n) ")
                if (addArvnAssault) {
                  Human.initTurnVariables(params)
                  Human.performAssault(ARVN, name, params)
                }
                nextSpace(assaulter, candidates filterNot (_ == name))
            }
          }
          log("\nSweep/Assault each Lowland Province touch Mekong")
          log(separator())
          val choices = List(US, ARVN).map(f => f -> f.toString)
          val assaulter = askMenu(choices, "Sweep/Assault using which faction:").head
          nextSpace(assaulter, LowlandTouchingMekong)
        }
        else {
          // Only ARVN Bot will execute this command
          log("\nSweep/Assault each Lowland Province touch Mekong")
          log(separator())
          for (name <- LowlandTouchingMekong if game.getSpace(name).pieces.has(InsurgentPieces)) {
            val params = Params(event = true, free = true, singleTarget = Some(name))
            Bot.initTurnVariables()  // Treat each space as a fresh turn
            ARVN_Bot.sweepOp(params, 1)
            Bot.initTurnVariables()  // Treat each space as a fresh turn
            Bot.performAssault(ARVN, name, params)
          }
        }
      },
      // shadedEffective()
      (faction: Faction) => game.availablePieces.has(VCGuerrillas_U),
      // executeShaded()
      (faction: Faction) => {
        val maxGs = MekongLoCs.size * 2
        val numAvailGs = game.availablePieces.totalOf(VCGuerrillas_U) min maxGs

        if (numAvailGs == maxGs) {
          for (name <- MekongLoCs)
            placePieces(name, Pieces(vcGuerrillas_U = 2))
        }
        else if (game.isHuman(faction)) {
          def nextLoc(candidates: List[String]): Unit = if (candidates.nonEmpty) {
            val name = askSimpleMenu(candidates, "\nPlace guerrillas on which Mekong LoC:").head
            val pieces = askPiecesToPlace(name, Set(VCGuerrillas_U), 2)
            placePieces(name, pieces)
            nextLoc(candidates filterNot (_ == name))
          }
          println("There are less than 6 available VC Guerrillas")
          println("You may move guerrillas from other spaces to make up the difference")
          loggingControlChanges {
            nextLoc(MekongLoCs)
          }
        }
        else {
          def nextLoC(numRemaining: Int, candidates: List[Space]): Unit = if (numRemaining > 0) {
            val sp = VC_Bot.pickSpacePlaceGuerrillas(candidates)
            val num = numRemaining min 2
            placePieces(sp.name, Pieces(vcGuerrillas_U = num))
            nextLoC(numRemaining - num, candidates filterNot (_.name == sp.name))
          }
          nextLoC(numAvailGs, spaces(MekongLoCs))
        }

        // Place sabotage markers where VC > COIN
        if (game.terrorMarkersAvailable > 0) {
          log()
          for (name <- MekongLoCs) {
            if (game.terrorMarkersAvailable > 0) {
              val sp = game.getSpace(name)
              if (sp.pieces.totalOf(VCPieces) > sp.pieces.totalOf(CoinPieces))
                addTerror(name, 1)
            }
          }
        }
        else
          log("\nThere are no available sabotage markers")
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(26, "LRRP", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false, // COIN Bots never play this event.
      // executeUnshaded()
      (faction: Faction) => {
        //  Human only
        if (game.piecesToPlace.has(Irregulars_U))
          placePiecesOnMap(US, 3, Set(Irregulars_U), LaosCambodia)
        else
          log("There are no Irregulars that can be placed.")
        Human.doAirStrike(Params(event = true, free = true))
      },
      // shadedEffective()
      (faction: Faction) => game.totalOnMap(_.pieces.totalOf(Irregulars)) > 0,
      // executeShaded()
      (faction: Faction) => {
        val numOnMap = game.totalOnMap(_.pieces.totalOf(Irregulars))
        def irregSpaces = game.spaces filter (_.pieces.has(Irregulars))

        // Save all control/scoring messages until the end.
        loggingControlChanges {
          val spacesUsed = if (numOnMap == 0) {
            log("There are no Irregulars on the map")
            Set.empty
          }
          else 
            removePiecesFromMap(faction, 3, Irregulars, false, spaceNames(game.spaces))

          for (name <- spacesUsed) {
            val sp = game.getSpace(name)
            if (!sp.isLoC && sp.support > ActiveOpposition)
              decreaseSupport(name, 1)
          }
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(27, "Phoenix Program", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.nonLocSpaces exists isPhoenixUnshadedSpace,
      // executeUnshaded()
      (faction: Faction) => {
        val VCTypes = VCPieces.toSet - VCTunnel
        val validSpaces = game.nonLocSpaces filter isPhoenixUnshadedSpace

        loggingControlChanges {
          if (validSpaces.isEmpty)
            log("There are no spaces that meet the event criteria")
          else if (game.isHuman(faction))
            removePiecesFromMap(faction, 3, VCTypes, false, spaceNames(validSpaces))
          else {
            // ARNV bot will target VC bases first
            val baseSpaces = validSpaces filter (_.pieces.has(VCBase))
            val numBases   = (baseSpaces map (_.pieces.totalOf(VCBases))).sum min 3
            val numOthers  = 3 - numBases
            removePiecesFromMap(faction, numBases, Set(VCBase), false, spaceNames(baseSpaces))
            removePiecesFromMap(faction, numOthers, VCTypes, false, spaceNames(validSpaces))
          }
        }
      },
      // shadedEffective()
      (faction: Faction) => game.nonLocSpaces exists isPhoenixShadedSpace,
      // executeShaded()
      (faction: Faction) => {
        val validSpaces = game.nonLocSpaces filter isPhoenixShadedSpace

        loggingPointsChanges {
          if (validSpaces.isEmpty)
            log("There are no spaces that meet the event criteria")
          else if (game.isHuman(faction)) {
            def nextSpace(count: Int, candidates: List[String]): Unit = if (count <= 2 && candidates.nonEmpty) {
              val name = askCandidate(s"Select ${ordinal(count)} space:", candidates)
              log(s"$faction selects $name")
              if (game.terrorMarkersAvailable > 0)
                addTerror(name, 1)
              else
                log("There are no available terror markers")
              setSupport(name, ActiveOpposition)
              nextSpace(count + 1, candidates filterNot (_ == name))
            }
            nextSpace(1, spaceNames(validSpaces))
          }
          else {
            def nextSpace(numRemaining: Int, candidates: List[Space]): Unit = if (numRemaining > 0 && candidates.nonEmpty) {
              val sp = VC_Bot.pickSpaceTowardActiveOpposition(candidates)
              log(s"$faction selects ${sp.name}")
              if (game.terrorMarkersAvailable > 0)
                addTerror(sp.name, 1)
              else
                log("There are no available terror markers")
              setSupport(sp.name, ActiveOpposition)
              nextSpace(numRemaining - 1, candidates filterNot (_.name == sp.name))
            }
            nextSpace(2, validSpaces)
          }
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(28, "Search and Destroy", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => playCapability(SearchAndDestroy_Unshaded),
      // shadedEffective()
      (faction: Faction) => true,
      // executeShaded()
      (faction: Faction) => playCapability(SearchAndDestroy_Shaded)
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(29, "Tribesmen", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.spaces exists isTribesmenUnshadedSpace,
      // executeUnshaded()
      (faction: Faction) => {
        val validSpaces = spaceNames(game.spaces filter isTribesmenUnshadedSpace)
        if (validSpaces.isEmpty)
          log("There are no spaces with Irregulars and Insurgent pieces")
        else
          loggingControlChanges {
            removePiecesFromMap(faction, 4, NVABase::VCBase::InsurgentForces, false, validSpaces)
          }
      },
      // shadedEffective()
      (faction: Faction) => {
        (game.availablePieces.has(VCGuerrillas_U) && game.totalOnMap(_.pieces.totalOf(Irregulars)) > 0) ||
        game.nonLocSpaces.exists(sp => sp.population > 0 && sp.isHighland && sp.support == Neutral) ||
        game.patronage > 0
      },
      // executeShaded()
      (faction: Faction) => {
        val totalIrreg = game.totalOnMap(_.pieces.totalOf(Irregulars))
        val numAvailGs = game.availablePieces.totalOf(VCGuerrillas_U)

        def replaceAll(): Unit = {
          for (sp <- game.spaces if sp.pieces.has(Irregulars)) {
            val irreg = sp.pieces.only(Irregulars)
            removeToAvailable(sp.name, irreg)
            placePieces(sp.name, Pieces(vcGuerrillas_U = irreg.total))
          }
        }

        def humanNextIrreg(numRemaining: Int): Unit = if (numRemaining > 0) {
          val candidates = spaceNames(game.spaces filter (_.pieces.has(Irregulars)))
          if (candidates.nonEmpty) {
            val name  = askCandidate("Replace Irregulars in which space: ", candidates)
            val sp    = game.getSpace(name)
            val irreg = sp.pieces.only(Irregulars)
            val num   = askInt(s"Replace how many Irregulars in $name", 0, irreg.total)
            val dead  = askPieces(irreg, num)
            removeToAvailable(name, dead)
            placePieces(name, Pieces(vcGuerrillas_U = num))
            humanNextIrreg(numRemaining - num)
          }
        }

        def botNextIrreg(): Unit = {
          val candidates = game.spaces filter (_.pieces.has(Irregulars))
          if (candidates.nonEmpty && game.availablePieces.has(VCGuerrillas_U)) {
            val sp = VC_Bot.pickSpaceRemoveReplace(candidates)
            val irreg = Bot.selectEnemyRemovePlaceActivate(sp.pieces.only(Irregulars), 1)
            removeToAvailable(sp.name, irreg)
            placePieces(sp.name, Pieces(vcGuerrillas_U = 1))
            botNextIrreg()
          }
        }

        loggingControlChanges {
          if (numAvailGs >= totalIrreg)
              replaceAll()
          else if (game.isHuman(faction)) {
            val gdisp = thereAre(numAvailGs, "available VC Guerrilla")
            println(s"There ${gdisp} and ${amountOf(totalIrreg, "Irregulars")} on the map")
            val numRemove = askInt("How many VC Guerrillas do you wish to voluntarily remove", 0, totalIrreg - numAvailGs)
            if (numRemove > 0)
              voluntaryRemoval(numRemove, VCGuerrillas_U)
            val num = numAvailGs + numRemove
            if (num == totalIrreg)
              replaceAll()
            else
              humanNextIrreg(num)
          }
          else { // Bot
            botNextIrreg()
          }

          val highland = game.nonLocSpaces filter (sp => sp.population > 0 && sp.isHighland && sp.support == Neutral)
          if (highland.isEmpty)
            log(s"\nThere are no Neutral Highland provinces")
          else {
            val name = if (game.isHuman(faction))
              askCandidate("\nSet which Neutral highland province to Active Opposition: ", spaceNames(highland))
            else
              VC_Bot.pickSpaceTowardActiveOpposition(highland).name
            setSupport(name, ActiveOpposition)
          }
          decreasePatronage(3)
        }

      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(30, "USS New Jersey", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => game.spaces exists { sp => sp.coastal && sp.pieces.hasExposedInsurgents },
      // executeUnshaded()
      (faction: Faction) => {
        val candidates = game.spaces filter { sp => sp.coastal && sp.pieces.hasExposedInsurgents }

        if (candidates.isEmpty)
          log("There are no coastal spaces with exposed Insurgent pieces")
        else if (game.isHuman(faction)) {
          val num = askInt("Strike in how many coastal spaces", 1, candidates.size min 3)
          val choices = candidates map (sp => sp.name -> sp.name)
          val names = askMenu(choices, "Select coastal spaces to air strike: ", num).toSet
          val params = Params(
            event        = true,
            free         = true,
            strikeParams = AirStrikeParams(
              canDegradeTrail = false,
              noCoin          = true,
              designated      = Some(names),
              maxHits         = Some(6),
              maxHitsPerSpace = Some(2)))
          Human.doAirStrike(params)
        }
        else {  // Bot
          def nextSpace(numRemaining: Int, candidates: List[Space]): List[String] = {
            if (numRemaining > 0 && candidates.nonEmpty) {
              val sp = Bot.pickSpaceRemoveReplace(faction)(candidates)
              sp.name::nextSpace(numRemaining - 1, candidates filterNot (_.name == sp.name))
            }
            else
              Nil
          }
          val names = nextSpace(3, candidates).toSet
          val params = Params(
            event        = true,
            free         = true,
            strikeParams = AirStrikeParams(
              canDegradeTrail = false,
              noCoin          = true,
              designated      = Some(names),
              maxHits         = Some(6),
              maxHitsPerSpace = Some(2)))
          US_Bot.airStrikeActivity(params)
        }
      },
      // shadedEffective()
      (faction: Faction) => game.nonLocSpaces exists isUSSNewJerseyShadedSpace,
      // executeShaded()
      (faction: Faction) => {
        val candidates = game.nonLocSpaces filter isUSSNewJerseyShadedSpace

        if (candidates.isEmpty)
          log("There are no spaces that meet the event criteria")
        else {
          val names = if (game.isHuman(faction)) {
            val choices = candidates map (sp => sp.name -> sp.name)
            askMenu(choices, "Select coastal Provinces:", 2 min choices.size)
          }
          else {
            def nextSpace(numRemaining: Int, candidates: List[Space]): List[String] = {
              if  (numRemaining >  0 && candidates.nonEmpty) {
                val sp = VC_Bot.pickSpaceTowardActiveOpposition(candidates)
                sp.name::nextSpace(numRemaining - 1, candidates filterNot (_.name == sp.name))
              }
              else
                Nil
            }
            nextSpace(2, candidates)
          }

          loggingPointsChanges {
            for (name <- names) 
              decreaseSupport(name, 2)
          }
        }
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(31, "AAA", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(32, "Long Range Guns", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(33, "MiGs", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => true,
      // executeUnshaded()
      (faction: Faction) => {
        playCapability(MiGs_Unshaded)
      },
      // shadedEffective()
      (faction: Faction) => !capabilityInPlay(TopGun_Unshaded),
      // executeShaded()
      (faction: Faction) => {
        if (capabilityInPlay(TopGun_Unshaded))
          log(s"\nThe shaded MiGS capability has been blocked [$TopGun_Unshaded]")
        else
          playCapability(MiGs_Shaded)
      }
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(34, "SA-2s", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(35, "Thanh Hoa", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(36, "Hamburger Hill", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(37, "Khe Sanh", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(38, "McNamara Line", SingleEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(39, "Oriskany", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(40, "PoWs", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(41, "Bombing Pause", SingleEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(42, "Chou En Lai", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new EventCard(43, "Economic Aid", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(44, "la Drang", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new EventCard(45, "PT-76", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(46, "559th Transport Grp", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(47, "Chu Luc", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(48, "Nam Dong", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(49, "Russian Arms", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(50, "Uncle Ho", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(51, "301st Supply Bn", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(52, "RAND", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Critical -> Unshaded),
              ARVN -> (Critical -> Unshaded),
              NVA  -> (Critical -> Shaded),
              VC   -> (Critical -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(53, "Sappers", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(54, "Son Tay", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(55, "Trucks", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(56, "Vo Nguyen Giap", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(57, "International Unrest", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(58, "Pathet Lao", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(59, "Plei Mei", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(60, "War Photographer", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(61, "Armored Cavalry", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(62, "Cambodian Civil War", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(63, "Fact Finding", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(64, "Honolulu Conference", SingleEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (Performed   -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(65, "International Forces", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(66, "Ambassador Taylor", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(67, "Amphib Landing", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new EventCard(68, "Green Berets", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(69, "MACV", SingleEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (Performed   -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(70, "ROKs", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(71, "An Loc", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(72, "Body Count", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    // Mo_Medevac_Unshaded    - In Commitment Phase (immediately move all US TROOPS in CASUALTIES to AVAILABLE,
    //                          no TROOPS go out of play.  See note: For effect when #73 Great Society is played.
    entry(new EventCard(73, "Great Society", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(74, "Lam Son 719", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(75, "Sihanouk", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(76, "Annam", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(77, "Detente", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(78, "General Lansdale", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(79, "Henry Cabot Lodge", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Shaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(80, "Light at the End of the Tunnel", SingleEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (Critical    -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(81, "CIDG", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(82, "Domino Theory", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(83, "Election", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(84, "To Quoc", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(85, "USAID", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(86, "Mandate of Heaven", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(87, "Nguyen Chanh Thi", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(88, "Phan Quang Dan", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(89, "Tam Chau", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(90, "WAlt Rostow", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(91, "Bob Hope", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(92, "SEALORDS", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(93, "Senator Fulbright", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(94, "Tunnel Rats", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(95, "Westmoreland", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(96, "APC", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(97, "Brinks Hotel", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Performed -> Unshaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(98, "Long Tan", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(99, "Masher/White Wing", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(100, "Rach Ba Rai", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(101, "Booby Traps", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(102, "Cu Chi", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(103, "Kent State", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(104, "Main Force Bns", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(105, "Rural Pressure", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Critical    -> Shaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(106, "Binh Duong", SingleEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (Critical    -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(107, "Burning Bonze", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(108, "Draft Dodgers", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(109, "Nguyen Huu Tho", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(110, "No Contact", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(111, "Agent Orange", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(112, "Colonel Chau", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(113, "Ruff Puff", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(114, "Tri Quang", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(115, "Typhoon Kate", SingleEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => singleNotYet(),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for single event
      // executeShaded()
      (faction: Faction) => ()      // Not used for single event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(116, "Cadres", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(117, "Corps Commanders", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(118, "Korean War Arms", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(119, "My Lai", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(120, "US Press Corps", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      // unshadedEffective()
      (faction: Faction) => false,
      // executeUnshaded()
      (faction: Faction) => unshadedNotYet(),
      // shadedEffective()
      (faction: Faction) => false,
      // executeShaded()
      (faction: Faction) => shadedNotYet()
    )),


    // ------------------------------------------------------------------------
    // Pivotal Events
    // ------------------------------------------------------------------------
    entry(new EventCard(121, "Linebacker II", SingleEvent, // US Pivotal event
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      // unshadedEffective()
      // Is US pivotal event playable?
      (faction: Faction) => {
        val threshold = if (game.peaceTalks) 25 else 40
        game.numCardsInLeaderBox >= 2 && game.usPoints > threshold
      },
      // executeUnshaded()
      // Carries out US pivotal event
      (faction: Faction) => pivotalNotYet(US),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for pivotal event
      // executeShaded()
      (faction: Faction) => ()      // Not used for pivotal event
    )),

    // With the Pivotal events only the Unshaded functions are used.
    // The unshadedEffective() function is used to determine if the
    // faction can play the Pivotal event.
    // ------------------------------------------------------------------------
    entry(new EventCard(122, "Easter Offensive", SingleEvent, // NVA Pivotal event
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      // unshadedEffective()
      // Is NVA pivotal event playable?
      (faction: Faction) => {
        game.numCardsInLeaderBox >= 2 &&
        game.totalOnMap(_.pieces.totalOf(NVATroops)) > game.totalOnMap(_.pieces.totalOf(USTroops))
      },
      // executeUnshaded()    
      // Carries out NVA pivotal event
      (faction: Faction) => pivotalNotYet(NVA),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for pivotal event
      // executeShaded()
      (faction: Faction) => ()      // Not used for pivotal event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(123, "Vietnamization", SingleEvent, // ARVN Pivotal event
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      // unshadedEffective()
      // Is VC pivotal event playable?
      (faction: Faction) => {
        game.numCardsInLeaderBox >= 2 && game.totalOnMap(_.pieces.totalOf(USTroops)) < 20
      },
      // executeUnshaded()
      // Carries out VC pivotal event
      (faction: Faction) => pivotalNotYet(VC),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for pivotal event
      // executeShaded()
      (faction: Faction) => ()      // Not used for pivotal event
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(124, "Tet Offensive", SingleEvent, // VC Pivotal event
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (Critical    -> Unshaded)),
      // unshadedEffective()
      // Is ARVN pivotal event playable?
      (faction: Faction) => {
        game.numCardsInLeaderBox >= 2 && numVCGuerrillasInSouth > 20
      },
      // executeUnshaded()
      // Carries out ARVN pivotal event
      (faction: Faction) => pivotalNotYet(ARVN),
      // shadedEffective()
      (faction: Faction) => false,  // Not used for pivotal event
      // executeShaded()
      (faction: Faction) => ()      // Not used for pivotal event
    )),

    // ------------------------------------------------------------------------
    // Coup Cards
    // ------------------------------------------------------------------------
    entry(new EventCard(125, "Coup! Nguyen Khanh", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,  // unshadedEffective() not used for Coup cards
      (faction: Faction) => (),     // executeUnshaded() no event for this Coup Card
      (faction: Faction) => false,  // shadedEffective() not used for Coup cards
      (faction: Faction) => ()      // executeShaded() not used for Coup cards
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(126, "Coup! Young Turks", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,  // unshadedEffective() not used for Coup cards
      (faction: Faction) => (),     // executeUnshaded() no event for this Coup Card
      (faction: Faction) => false,  // shadedEffective() not used for Coup cards
      (faction: Faction) => ()      // executeShaded() not used for Coup cards
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(127, "Coup! Nguyen Cao Ky", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,  // unshadedEffective() not used for Coup cards
      (faction: Faction) => (),     // executeUnshaded() no event for this Coup Card
      (faction: Faction) => false,  // shadedEffective() not used for Coup cards
      (faction: Faction) => ()      // executeShaded() not used for Coup cards
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(128, "Coup! Nguyen Van Thieu", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,  // unshadedEffective() not used for Coup cards
      (faction: Faction) => (),     // executeUnshaded() no event for this Coup Card
      (faction: Faction) => false,  // shadedEffective() not used for Coup cards
      (faction: Faction) => ()      // executeShaded() not used for Coup cards
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(129, "Coup! Failed Attempt",SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,  // unshadedEffective() not used for Coup cards
      // executeUnshaded() -- Coup round event
      // ARVN removes 1 in 3 of its cubes per space (rounded down)
      (faction: Faction) => coupNotYet(),
      (faction: Faction) => false,  // shadedEffective() not used for Coup cards
      (faction: Faction) => ()  // executeShaded() not used for Coup cards
    )),

    // ------------------------------------------------------------------------
    entry(new EventCard(130, "Coup! Failed Attempt", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,  // unshadedEffective() not used for Coup cards
      (faction: Faction) => deck(129).executeUnshaded(faction),
      (faction: Faction) => false,  // shadedEffective() not used for Coup cards
      (faction: Faction) => ()      // executeShaded() not used for Coup cards
    ))
  )
}