
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
import FUtil.Pathname
import FireInTheLake.{ GameState, SequenceOfPlay, Space, Pieces, Faction, Action, SpaceType,
                       Actor, Capability, GameSegment, PieceType, SupportType, SOFTWARE_VERSION,
                       BotIntents, BotIntentsVerbose }
import Bot.{ TrungCard, trungFromId }

object SavedGame {
  val CurrentFileVersion = 1

  def save(filepath: Pathname, gameState: GameState): Unit = {
    try {
      filepath.writeFile(toJson(gameState))
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error writing saved game ($filepath)$suffix")
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error writing saved game ($filepath)$suffix")
    }
  }
  
  private def toJson(gameState: GameState): String = {
    val top = Map(
      "file-version"     -> CurrentFileVersion,
      "software-version" -> SOFTWARE_VERSION,
      "game-state"       -> gameStateToMap(gameState)
    )
    Json.build(top)
  }

  // The path should be the full path to the file to load.
  // Will set the game global variable
  def load(filepath: Pathname): GameState = {
    try fromJson(filepath.readFile())
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error reading saved game ($filepath)$suffix")
        sys.exit(1)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error reading saved game ($filepath)$suffix")
        sys.exit(1)
    }
  }

  private def fromJson(jsonValue: String): GameState = {
    val top = asMap(Json.parse(jsonValue))
    if (!top.contains("file-version"))
      throw new IllegalArgumentException(s"Invalid save file - No file version number")
    
    if (!top.contains("game-state"))
      throw new IllegalArgumentException(s"Invalid save file - No game-state")

    asInt(top("file-version")) match {
      case 1 => gameFromVersion1(asMap(top("game-state")))
      case v => throw new IllegalArgumentException(s"Invalid save file version: $v")
    }
  }
  
  private def asString(x: Any): String = x.toString
  private def asBoolean(x: Any): Boolean = x match {
    case b: Boolean => b
    case _          => throw new Exception(s"Not a valid Boolean value: $x")
  }  
  private def asInt(x: Any): Int = x match {
    case i: Int => i
    case _      => throw new Exception(s"Not a valid Boolean value: $x")
  }  
  private def asMap(x: Any): Map[String, Any] = x match {
    case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
    case _            => throw new Exception(s"Not a valid Map value!")
  }  
  private def asList(x: Any): List[Any] = x match {
    case l: List[_] => l.asInstanceOf[List[Any]]
    case _          => throw new Exception(s"Not a valid List value!")
  }  
  private def asSet(x: Any): Set[Any] = x match {
    case l: List[_] => l.asInstanceOf[List[Any]].toSet
    case _          => throw new Exception(s"Not a valid Set value!")
  }  
    
  private def actorToMap(actor: Actor): Map[String, Any] =
    Map(
      "faction" -> actor.faction.name,
      "action"  -> actor.action.name
    )

  private def actorFromMap(data: Map[String, Any]): Actor = {
    Actor(
      Faction(asString(data("faction"))),
      Action(asString(data("action")))
    )
  }

  private def capabilityToMap(cap: Capability): Map[String, Any] =
    Map(
      "name"    -> cap.name,
      "shaded"  -> cap.shaded,
      "faction" -> cap.faction.toString
    )

  private def capabilityFromMap(data: Map[String, Any]): Capability = {
    Capability(
      asString(data("name")),
      asBoolean(data("shaded")),
      Faction(asString(data("faction")))
    )
  }

  private def gameSegmentToMap(seg: GameSegment): Map[String, Any] =
    Map(
      "save_number" -> seg.save_number,
      "card"        -> seg.card,
      "summary"     -> seg.summary
    )

  private def gameSegmentFromMap(data: Map[String, Any]): GameSegment = {
    GameSegment(
      asInt(data("save_number")),
      asString(data("card")),
      asList(data("summary")) map (_.toString)
    )
  }

  private def sequenceOfPlayToMap(seq: SequenceOfPlay): Map[String, Any] =
    Map(
      "eligibleThisTurn"   -> seq.eligibleThisTurn,
      "actors"             -> (seq.actors map (a => Map("faction" -> a.faction.name, "action" -> a.action.name))),
      "passed"             -> seq.passed,
      "eligibleNextTurn"   -> seq.eligibleNextTurn,
      "ineligibleNextTurn" -> seq.ineligibleNextTurn
    )
  
  private def sequenceOfPlayFromMap(data: Map[String, Any]): SequenceOfPlay = {
    SequenceOfPlay(
      asSet(data("eligibleThisTurn"))   map (name => Faction(name.toString)),
      asList(data("actors"))            map (data => actorFromMap(asMap(data))),
      asSet(data("passed"))             map (name => Faction(name.toString)),
      asSet(data("eligibleNextTurn"))   map (name => Faction(name.toString)),
      asSet(data("ineligibleNextTurn")) map (name => Faction(name.toString))
    )
  }
    
  private def spaceToMap(sp: Space): Map[String, Any] =
    Map(
      "name"       -> sp.name,
      "spaceType"  -> sp.spaceType,
      "population" -> sp.population,
      "coastal"    -> sp.coastal,
      "support"    -> sp.support,
      "support"    -> sp.support,
      "pieces"     -> (sp.pieces.explode() map (_.name)),
      "terror"     -> sp.terror
    )
  
  
  private def spaceFromMap(data: Map[String, Any]): Space = {
    //  Some space names have been fixed for typos etc.
    //  This will allow us to load game files that were saved
    //  with the obsolete names
    def spaceNameFixup(name: String): String = name match {
      case "Quang Tin Quang Ngai" => FireInTheLake.QuangTin_QuangNgai  // Added a hypen between the two Names
      case other                  => other
    }
    
    Space(
      spaceNameFixup(asString(data("name"))),
      SpaceType(asString(data("spaceType"))),
      asInt(data("population")),
      asBoolean(data("coastal")),
      SupportType(asString(data("support"))),
      Pieces.fromTypes(asList(data("pieces")) map (name => PieceType(name.toString))),
      asInt(data("terror"))
    )
  }
  
  
  private def gameStateToMap(gameState: GameState): Map[String, Any] = {
    Map(
      "scenarioName"           -> gameState.scenarioName,
      "humanFactions"          -> gameState.humanFactions,
      "cardsPerCampaign"       -> gameState.cardsPerCampaign,
      "totalCoupCards"         -> gameState.totalCoupCards,   // Total number in the current scenario
      "humanWinInVictoryPhase" -> gameState.humanWinInVictoryPhase,
      "spaces"                 -> (gameState.spaces map spaceToMap),
      "arvnResources"          -> gameState.arvnResources,
      "nvaResources"           -> gameState.nvaResources,
      "vcResources"            -> gameState.vcResources,
      "usAid"                  -> gameState.usAid,
      "patronage"              -> gameState.patronage,
      "econ"                   -> gameState.econ,
      "trail"                  -> gameState.trail,
      "usPolicy"               -> gameState.usPolicy,
      "casualties"             -> (gameState.casualties.explode() map (_.name)),
      "outOfPlay"              -> (gameState.outOfPlay.explode() map (_.name)),
      "pivotCardsAvailable"    -> gameState.pivotCardsAvailable,
      "capabilities"           -> (gameState.capabilities map capabilityToMap),
      "ongoingEvents"          -> gameState.ongoingEvents,
      "rvnLeaders"             -> gameState.rvnLeaders,
      "rvnLeaderFlipped"       -> gameState.rvnLeaderFlipped,
      "trungDeck"              -> (gameState.trungDeck map (_.id)),
      "momentum"               -> gameState.momentum,
      "sequence"               -> sequenceOfPlayToMap(gameState.sequence),
      "currentCard"            -> gameState.currentCard,
      "onDeckCard"             -> gameState.onDeckCard,
      "prevCardWasCoup"        -> gameState.prevCardWasCoup,
      "coupCardsPlayed"        -> gameState.coupCardsPlayed,
      "cardsSeen"              -> gameState.cardsSeen,
      "gameOver"               -> gameState.gameOver,
      "peaceTalks"             -> gameState.peaceTalks,
      "botDebug"               -> gameState.botDebug,
      "botTest"                -> gameState.botTest,
      "logTrung"               -> gameState.logTrung,
      "botIntents"             -> gameState.botIntents.toString,
      "history"                -> (gameState.history map gameSegmentToMap)
    )
  }
    
  private def gameFromVersion1(data: Map[String, Any]): GameState = {
    GameState(
      asString(data("scenarioName")),
      (asSet(data("humanFactions")) map (name => Faction(name.toString))),
      asInt(data("cardsPerCampaign")),
      asInt(data("totalCoupCards")),
      asBoolean(data("humanWinInVictoryPhase")),
      asList(data("spaces")) map (s => spaceFromMap(asMap(s))),
      asInt(data("arvnResources")),
      asInt(data("nvaResources")),
      asInt(data("vcResources")),
      asInt(data("usAid")),
      asInt(data("patronage")),
      asInt(data("econ")),
      asInt(data("trail")),
      asString(data("usPolicy")),
      Pieces.fromTypes(asList(data("casualties")) map (name => PieceType(name.toString))),
      Pieces.fromTypes(asList(data("outOfPlay")) map (name => PieceType(name.toString))),
      (asSet(data("pivotCardsAvailable")) map (name => Faction(name.toString))),
      asList(data("capabilities")) map (c => capabilityFromMap(asMap(c))),
      asList(data("ongoingEvents")) map (_.toString),
      asList(data("rvnLeaders")) map (_.toString),
      asBoolean(data("rvnLeaderFlipped")),
      asList(data("trungDeck")) map (id => trungFromId(id.toString)),
      asList(data("momentum")) map (_.toString),
      sequenceOfPlayFromMap(asMap(data("sequence"))),
      asInt(data("currentCard")),
      asInt(data("onDeckCard")),
      asBoolean(data("prevCardWasCoup")),
      asInt(data("coupCardsPlayed")),
      asList(data("cardsSeen")) map (_.toString.toInt),
      asBoolean(data("gameOver")),
      asBoolean(data("peaceTalks")),
      asBoolean(data("botDebug")),
      asBoolean(data.get("botTest") getOrElse false),
      asBoolean(data.get("logTrung") getOrElse true),
      BotIntents(asString(data.get("botIntents") getOrElse BotIntentsVerbose.toString)),
      (asList(data("history")) map (s => gameSegmentFromMap(asMap(s)))).toVector
    )
  }
}

