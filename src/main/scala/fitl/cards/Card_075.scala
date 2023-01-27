
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
// Pursuit operations: US or ARVN free Sweep into or in any
// Cambodia spaces, then free Assaults in one.
//
// Shaded Text
// Sea supply and sanctuary: VC free Rally in any Cambodia spaces then
// free March from any Rally spaces. Then NVA do the same.
//
// Tips
// Whomever the executing Faction chose to Sweep would decide the details of
// the Sweep and Assault. Free US Assault can add an ARVN Assault at cost 0 (3.2.4).
// The Sweep or March could occur even during Monsoon. The NVA March could include
// repeated moves from Cambodia spaces if each such space just hosted NVA Rally and
// if the Trail is at greater than 0 (3.3.3).

object Card_075 extends EventCard(75, "Sihanouk",
  DualEvent,
  List(ARVN, NVA, US, VC),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  def baseFirstOK(faction: Faction) =
    if (game.isBot(faction))
      Bot.canUseAbramsUnshaded(faction)
    else
      Human.canUseAbramsUnshaded(faction)
    
  def canSweep(faction: Faction)(sp: Space) = sp.sweepActivations(faction, NormalTroops) > 0

  def canAssault(faction: Faction)(sp: Space) = {
    val pattonCount = if (game.isBot(faction))
      Bot.m48PattonCount
    else
      Human.m48PattonCount
    assaultEffective(faction, NormalTroops, baseFirstOK(faction), false, pattonCount)(sp)
  }


  // The move priorities for US/ARVN state to keep all cubes
  // in S. Vietnam so the Bot would only sweep in place if
  // happend to have Special Forces (or by some previous event)
  // cubes already in a space
  def unshadedEffective(faction: Faction): Boolean =
    (spaces(Cambodia) exists canSweep(faction)) ||
    (spaces(Cambodia) exists canAssault(faction))
    
  def executeUnshaded(faction: Faction): Unit = {
    val coinFaction = if (game.isHuman(faction))
      askSimpleMenu(US::ARVN::Nil, "\nChoose faction to Sweep/Assault:").head
    else
      faction

    if (game.isHuman(coinFaction)) {
      val params = Params(event = true, free = true, onlyIn = Some(Cambodia.toSet))
      Human.executeSweep(coinFaction, params)

      val name = askSimpleMenu(Cambodia, "\nChoose Cambodia space to assault:").head
      Human.performAssault(coinFaction, name, params)
    }
    else {
      val forceTypes = if (coinFaction == US) Irregulars else Rangers
      // Since Bots will not move cubes out of S. Vietnam the only thing
      // a bot can do is sweep in place using special forces.
      val sweepCandidates = spaces(Cambodia) filter canSweep(coinFaction)

      if (sweepCandidates.isEmpty) {
        log(s"\n$faction does not Sweep/Assault any Cambodia spaces")
        log("The Bot move priorities prohibit COIN cubes leaving S. Vietnam")
      }
      else {
        for (sp <- sweepCandidates)
          sweepInPlace(sp.name, coinFaction, NormalTroops)
      }

      // If for some reason (other event) the faction has cubes in Cambodia
      // then it will assault...
      val assaultCandidates = spaces(Cambodia) filter canAssault(coinFaction)
      for (sp <- assaultCandidates)
        Bot.performAssault(coinFaction, sp.name, Params(event = true, free = true))
    }
  }

  def piecesInSpace(faction: Faction, name: String) = game.getSpace(name).pieces.only(factionPieces(faction))
  def saveSpaces(faction: Faction): List[(String, Pieces)] = (Cambodia map (n => n -> piecesInSpace(faction, n)))
  def rallied(faction: Faction, saved: List[(String, Pieces)]): Set[String] =
    (saved filter { case (name, pieces) => pieces != piecesInSpace(faction, name) } map (_._1)).toSet

  def shadedEffective(faction: Faction): Boolean = game.availablePieces.has(VCGuerrillas:::NVAPieces)

  def humanRallyMarch(faction: Faction): Unit = {
    val saved = saveSpaces(faction)
    val rallyParams = Params(event = true, free = true,  onlyIn = Some(Cambodia.toSet))

    Human.executeRally(faction, rallyParams)
    val rallySpaces = rallied(faction, saved)
    if (rallySpaces.nonEmpty) {
      val marchParams = Params(
        event = true,
        free  = true,
        march = MarchParams(onlyFrom = rallySpaces))
      Human.executeMarch(faction, marchParams)
    }
  }

  def botRallyMarch(faction: Faction): Unit = {
    val rallyTypes = if (faction == VC) VCGuerrillas_U::Nil else NVAPieces
    if (game.availablePieces.has(rallyTypes)) {
      val saved = saveSpaces(faction)
      val rallyParams = Params(
        event = true,
        free  = true,
        onlyIn = Some(Cambodia.toSet),
        rally  = RallyParams(guerrillasOnly = faction == VC))
  
      if (faction == VC)
        VC_Bot.rallyOp(rallyParams, actNum = 2)
      else
        NVA_Bot.rallyOp(rallyParams, actNum = 2)

      val rallySpaces = rallied(faction, saved)
      if (rallySpaces.nonEmpty) {
        
        val marchParams = Params(
          event  = true,
          free   = true,
          onlyIn = if (faction == VC) Some(SouthVietnam.toSet) else None, // Force VC guerrillas to leave Cambodia
          march  = MarchParams(onlyFrom = rallySpaces,
                                    amassForBase = faction == NVA)) // Force VC guerrillas to leave Cambodia
        val nvaActNum = if (game.trail == TrailMax) 1 else 2
        if (faction == VC)
          VC_Bot.marchOp(marchParams, actNum = 2)
        else
          NVA_Bot.marchOp(marchParams, nvaActNum, true, true)
      }
    }
    else if (faction == VC)
      log("VC does not have any available Guerrillas to rally")
    else
      log("NVA does not have any available Pieces to rally")
  }
  
  def executeShaded(faction: Faction): Unit = {
    if (game.isHuman(VC))
      humanRallyMarch(VC)
    else
      botRallyMarch(VC)

    if (game.isHuman(NVA))
      humanRallyMarch(NVA)
    else
      botRallyMarch(NVA)
  }
}
