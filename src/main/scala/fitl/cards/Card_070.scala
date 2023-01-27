
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
// Tough Koreans: US or ARVN free Sweep into/in then free Assault Phu Bon
// and adjacent spaces as if US and as if all ARVN cubes are US Troops.
//
// Shaded Text
// UN troops abuse locals: Shift Qui Nhon, Phu Bon, and Khanh Hoa each 1 level
// toward Active Opposition.
//
// Tips
// For the unshaded text, the executing Faction picks US or ARVN and that faction
// decides the details of its Sweep and Assault. The Sweep could occur even during Monsoon.
// “Phu Bon and ad- jacent spaces” include Phu Bon, Qui Nhon, Binh Dinh, Kontum, Pleiku,
// Khanh Hoa, and—for Assault—the 3 LoCs touching PhuBon. “As if US and as if all ARVN
// cubes are US Troops” means that whichever selected Faction—US or ARVN—would move and fight
// with all US Troops, ARVN Troops, and Police as if all those cubes were US Troops, including
// double enemy losses for any US Base in a space, the effects of the “Abrams” Capability if in
// effect, and so on. Non-player ARVN will still use Non-player ARVN priorities (8.4.4).

object Card_070 extends EventCard(70, "ROKs",
  DualEvent,
  List(ARVN, US, VC, NVA),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (NotExecuted -> Unshaded),
          NVA  -> (NotExecuted -> Shaded),
          VC   -> (Critical    -> Shaded))) {

  val unshadedNames = getAdjacent(PhuBon_PhuYen) + PhuBon_PhuYen

  def unshadedEffective(faction: Faction): Boolean = true
  def executeUnshaded(faction: Faction): Unit = {
    val sweepDestinations = spaceNames(spaces(unshadedNames) filter (!_.isLoC)).toSet
    val params = Params(
      event        = true,
      free         = true,
      cubeTreatment = AllCubesAsUS,
      onlyIn = Some(sweepDestinations)
    )

    val actor: Faction = if (game.isHuman(faction)) {
      val choices: List[Faction] = List(US, ARVN)
      askSimpleMenu(choices, "\nChoose a faction to Sweep/Assault:").head
    }
    else
      US

    log(s"\n$faction chooses $actor to carry out the Sweeps and Assaults")

    if (game.isHuman(actor)) {
      val baseFirstOK = Human.canUseAbramsUnshaded(actor)
      Human.resetM48PattonSpaces()
      println()
      Human.executeSweep(actor, params)

      loggingControlChanges {
        for {
          name <- unshadedNames
          sp   =  game.getSpace(name)
               if assaultEffective(actor, AllCubesAsUS, baseFirstOK, false, Human.m48PattonCount)(sp)
        } {
          Human.performAssault(actor, name, params)
          pause()
        }
      }
    }
    else {
      val baseFirstOK = Bot.canUseAbramsUnshaded(actor)
      Bot.resetM48PattonSpaces()
      US_Bot.sweepOp(params)
      pause()
      loggingControlChanges {
        for {
          name <- unshadedNames
          sp   =  game.getSpace(name)
               if assaultEffective(actor, AllCubesAsUS, baseFirstOK, false, Bot.m48PattonCount)(sp)
        } {
          Bot.performAssault(actor, name, params)
          pause()
        }
      }
    }
    
  }

  val ShadedNames = List(QuiNhon, PhuBon_PhuYen, KhanhHoa)
  def shadedCandidates = spaces(ShadedNames) filter (_.support != ActiveOpposition)

  def shadedEffective(faction: Faction): Boolean = shadedCandidates.nonEmpty

  def executeShaded(faction: Faction): Unit = {
    loggingPointsChanges {
      for (sp <- shadedCandidates)
        decreaseSupport(sp.name, 1)
    }
  }
}
