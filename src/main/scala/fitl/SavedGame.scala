
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
                       BotIntents, BotIntentsVerbose, LogEntry, Color }
import Bot.{ TrungCard, trungFromId }

object SavedGame {
  val CurrentFileVersion = 1
  val CurrentLogVersion  = 1

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
    val top = Json.Obj(
      "file-version"     -> CurrentFileVersion,
      "software-version" -> SOFTWARE_VERSION,
      "game-state"       -> gameStateToObj(gameState)
    )
    Json.output(top, indent = Some(2))
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
    val top = Json.parse(jsonValue).obj
    if (!top.contains("file-version"))
      throw new IllegalArgumentException(s"Invalid save file - No file version number")

    if (!top.contains("game-state"))
      throw new IllegalArgumentException(s"Invalid save file - No game-state")

    top("file-version").int match {
      case 1 => gameFromVersion1(top("game-state"))
      case v => throw new IllegalArgumentException(s"Invalid save file version: $v")
    }
  }

  private def actorToObj(actor: Actor) =
    Json.Obj(
      "faction" -> actor.faction.name,
      "action"  -> actor.action.name
    )

  private def actorFromObj(data: Json.Value): Actor = {
    Actor(
      Faction(data.obj("faction").str),
      Action(data.obj("action").str)
    )
  }

  private def capabilityToObj(cap: Capability) =
    Json.Obj(
      "name"    -> cap.name,
      "shaded"  -> cap.shaded,
      "faction" -> cap.faction.name
    )

  private def capabilityFromObj(data: Json.Value): Capability = {
    val params = data.obj
    Capability(
      params("name").str,
      params("shaded").bool,
      Faction(params("faction").str)
    )
  }

  private def gameSegmentToObj(seg: GameSegment) =
    Json.Obj(
      "save_number" -> seg.save_number,
      "card"        -> seg.card,
      "summary"     -> Json.Arr.from(seg.summary)
    )

  private def gameSegmentFromObj(data: Json.Value): GameSegment = {
    val params = data.obj
    GameSegment(
      params("save_number").int,
      params("card").str,
      params("summary").arr.map(_.str).toList
    )
  }

  private def sequenceOfPlayToObj(seq: SequenceOfPlay) =
    Json.Obj(
      "eligibleThisTurn"   -> Json.Arr.from(seq.eligibleThisTurn.map(_.name)),
      "actors"             -> Json.Arr.from(
        seq.actors.map(a => Json.Obj("faction" -> a.faction.name, "action" -> a.action.name))
      ),
      "passed"             -> Json.Arr.from(seq.passed.map(_.name)),
      "eligibleNextTurn"   -> Json.Arr.from(seq.eligibleNextTurn.map(_.name)),
      "ineligibleNextTurn" -> Json.Arr.from(seq.ineligibleNextTurn.map(_.name))
    )

  private def sequenceOfPlayFromObj(data: Json.Value): SequenceOfPlay = {
    val params = data.obj
    SequenceOfPlay(
      params("eligibleThisTurn").arr.map(name => Faction(name.str)).toSet,
      params("actors").arr.map(actorFromObj).toList,
      params("passed").arr.map(name => Faction(name.str)).toSet,
      params("eligibleNextTurn").arr.map(name => Faction(name.str)).toSet,
      params("ineligibleNextTurn").arr.map(name => Faction(name.str)).toSet
    )
  }

  private def spaceToObj(sp: Space) =
    Json.Obj(
      "name"       -> sp.name,
      "spaceType"  -> sp.spaceType.name,
      "population" -> sp.population,
      "coastal"    -> sp.coastal,
      "support"    -> sp.support.name,
      "pieces"     -> Json.Arr.from(sp.pieces.explode().map(_.name)),
      "terror"     -> sp.terror
    )


  private def spaceFromObj(data: Json.Value): Space = {
    val params = data.obj
    //  Some space names have been fixed for typos etc.
    //  This will allow us to load game files that were saved
    //  with the obsolete names
    def spaceNameFixup(name: String): String = name match {
      case "Quang Tin Quang Ngai" => FireInTheLake.QuangTin_QuangNgai  // Added a hypen between the two Names
      case other                  => other
    }

    Space(
      spaceNameFixup(params("name").str),
      SpaceType(params("spaceType").str),
      params("population").int,
      params("coastal").bool,
      SupportType(params("support").str),
      Pieces.fromTypes(params("pieces").arr.map(name => PieceType(name.str))),
      params("terror").int
    )
  }


  private def gameStateToObj(gameState: GameState) = {
    Json.Obj(
      "scenarioName"           -> gameState.scenarioName,
      "humanFactions"          -> Json.Arr.from(gameState.humanFactions.map(_.name)),
      "cardsPerCampaign"       -> gameState.cardsPerCampaign,
      "totalCoupCards"         -> gameState.totalCoupCards,   // Total number in the current scenario
      "humanWinInVictoryPhase" -> gameState.humanWinInVictoryPhase,
      "spaces"                 -> Json.Arr.from(gameState.spaces.map(spaceToObj)),
      "arvnResources"          -> gameState.arvnResources,
      "nvaResources"           -> gameState.nvaResources,
      "vcResources"            -> gameState.vcResources,
      "usAid"                  -> gameState.usAid,
      "patronage"              -> gameState.patronage,
      "econ"                   -> gameState.econ,
      "trail"                  -> gameState.trail,
      "usPolicy"               -> gameState.usPolicy,
      "casualties"             -> Json.Arr.from(gameState.casualties.explode().map(_.name)),
      "outOfPlay"              -> Json.Arr.from(gameState.outOfPlay.explode().map(_.name)),
      "pivotCardsAvailable"    -> Json.Arr.from(gameState.pivotCardsAvailable.map(_.name)),
      "capabilities"           -> Json.Arr.from(gameState.capabilities.map(capabilityToObj)),
      "ongoingEvents"          -> Json.Arr.from(gameState.ongoingEvents),
      "rvnLeaders"             -> Json.Arr.from(gameState.rvnLeaders),
      "rvnLeaderFlipped"       -> gameState.rvnLeaderFlipped,
      "trungDeck"              -> Json.Arr.from(gameState.trungDeck.map(_.id)),
      "momentum"               -> Json.Arr.from(gameState.momentum),
      "sequence"               -> sequenceOfPlayToObj(gameState.sequence),
      "currentCard"            -> gameState.currentCard,
      "onDeckCard"             -> gameState.onDeckCard,
      "prevCardWasCoup"        -> gameState.prevCardWasCoup,
      "coupCardsPlayed"        -> gameState.coupCardsPlayed,
      "cardsSeen"              -> Json.Arr.from(gameState.cardsSeen),
      "gameOver"               -> gameState.gameOver,
      "peaceTalks"             -> gameState.peaceTalks,
      "botDebug"               -> gameState.botDebug,
      "botTest"                -> gameState.botTest,
      "logTrung"               -> gameState.logTrung,
      "botIntents"             -> gameState.botIntents.description,
      "history"                -> Json.Arr.from(gameState.history.map(gameSegmentToObj)),
      "showColor"              -> gameState.showColor
    )
  }

  private def gameFromVersion1(data: Json.Value): GameState = {
    val params = data.obj
    GameState(
      params("scenarioName").str,
      params("humanFactions").arr.map(name => Faction(name.str)).toSet,
      params("cardsPerCampaign").int,
      params("totalCoupCards").int,
      params("humanWinInVictoryPhase").bool,
      params("spaces").arr.map(spaceFromObj).toList,
      params("arvnResources").int,
      params("nvaResources").int,
      params("vcResources").int,
      params("usAid").int,
      params("patronage").int,
      params("econ").int,
      params("trail").int,
      params("usPolicy").str,
      Pieces.fromTypes(params("casualties").arr.map(name => PieceType(name.str))),
      Pieces.fromTypes(params("outOfPlay").arr.map(name => PieceType(name.str))),
      params("pivotCardsAvailable").arr.map(name => Faction(name.str)).toSet,
      params("capabilities").arr.map(capabilityFromObj).toList,
      params("ongoingEvents").arr.map(_.str).toList,
      params("rvnLeaders").arr.map(_.str).toList,
      params("rvnLeaderFlipped").bool,
      params("trungDeck").arr.map(id => trungFromId(id.str)).toList,
      params("momentum").arr.map(_.str).toList,
      sequenceOfPlayFromObj(params("sequence")),
      params("currentCard").int,
      params("onDeckCard").int,
      params("prevCardWasCoup").bool,
      params("coupCardsPlayed").int,
      params("cardsSeen").arr.map(_.int).toList,
      params("gameOver").bool,
      params("peaceTalks").bool,
      params("botDebug").bool,
      params.get("botTest").map(_.bool).getOrElse(false),
      params.get("logTrung").map(_.bool).getOrElse(true),
      BotIntents(params.get("botIntents").map(_.str).getOrElse(BotIntentsVerbose.description)),
      params("history").arr.map(gameSegmentFromObj).toVector,
      params.get("showColor").map(_.bool).getOrElse(true)
    )
  }


  // Methods to save and load the log files
  private def logEntryToObj(entry: LogEntry) =
    Json.Obj(
      "text" -> entry.text,
      "color" -> entry.color.map(c => Json.Str(c.name)).getOrElse(Json.Null)
    )

  private def logEntryFromObj(data: Json.Value): LogEntry = {
    val color = if (data.obj("color").isNull)
      None
    else
      Some(Color.fromName(data.obj("color").str))

    LogEntry(data.obj("text").str, color)
  }

  private def logToJson(entries: Vector[LogEntry]): String = {
    val top = Json.Obj(
      "file-version"     -> CurrentLogVersion,
      "software-version" -> SOFTWARE_VERSION,
      "log"              -> Json.Arr.from(entries.map(logEntryToObj))
    )
    Json.output(top, indent = Some(2))
  }

  private def logFromVersion1(entries: List[Json.Value]): Vector[LogEntry] = {
    entries.map(logEntryFromObj).toVector
  }

  def saveLog(filepath: Pathname, entries: Vector[LogEntry]): Unit = {
    try {
      filepath.writeFile(logToJson(entries))
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error writing log file ($filepath)$suffix")
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error writing log file ($filepath)$suffix")
    }
  }

  private def logFromJson(jsonValue: String): Vector[LogEntry] = {
      val top = Json.parse(jsonValue).obj
      if (!top.contains("file-version"))
        throw new IllegalArgumentException(s"Invalid save file - missing file version number")

      if (!top.contains("log"))
        throw new IllegalArgumentException(s"Invalid save file - missing log entries")

      top("file-version").int match {
        case 1 => logFromVersion1(top("log").arr.toList)
        case v => throw new IllegalArgumentException(s"Invalid log file version: $v")
      }
  }


   // The path should be the full path to the file to load.
  // Will set the game global variable
  def loadLog(filepath: Pathname): Vector[LogEntry] = {
    try logFromJson(filepath.readFile())
    catch {
      case e: Json.JsonException =>
          // Older versions did not store the log as json
        // If we cannot parse the file then treat it as a regular
        // text file.
        filepath.readLines.toVector map { line =>
          LogEntry(line, None)
        }
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error reading log file ($filepath)$suffix")
        sys.exit(1)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error reading log file ($filepath)$suffix")
        sys.exit(1)
    }
  }
}
