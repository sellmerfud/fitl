
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
// North-South rivalry lingers: NVA and VC –1 Resource each per
// space with both. Patronage +2.
//
// Shaded Text
// Saigon regime seen as colonial retread: Remove Support from Hue,
// Da Nang, and an adjacent Province.
//
// Tips
// "Space with both" means a space with both NVA and VC pieces. "Remove Support" means
// set either Active or Passive Support to Neutral (1.6.2).

object Card_076 extends EventCard(76, "Annam",
  DualEvent,
  List(ARVN, NVA, VC, US),
  ListMap(US   -> (Ignored -> Unshaded),
          ARVN -> (Critical    -> Unshaded),
          NVA  -> (Performed   -> Shaded),
          VC   -> (Performed   -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    sp.pieces.has(NVAPieces) &&
    sp.pieces.has(VCPieces)

  def canReduceRes(faction: Faction) = game.trackResources(faction) && game.resources(faction) < EdgeTrackMax

  def unshadedEffective(faction: Faction): Boolean =
    game.patronage < EdgeTrackMax ||
    ((game.spaces exists unshadedCandidate) && (canReduceRes(NVA) || canReduceRes(VC)))

  def executeUnshaded(faction: Faction): Unit = {
    val num = game.spaces count unshadedCandidate
    decreaseResources(NVA, num)
    decreaseResources(VC, num)
    increasePatronage(2)
  }

  val shadedNames = List(Hue, DaNang, QuangTri_ThuaThien, QuangNam, QuangTin_QuangNgai).sorted(SpaceNameOrdering)

  val shadedCandididate = (sp: Space) => sp.support > Neutral

  def shadedEffective(faction: Faction): Boolean = spaces(shadedNames) exists shadedCandididate

  def executeShaded(faction: Faction): Unit = {
    val provinceCandidates = spaces(shadedNames) filter { sp => sp.isProvince && shadedCandididate(sp) }

    val province = if (provinceCandidates.isEmpty)
      None
    else if (game.isHuman(faction))
      Some(askSimpleMenu(spaceNames(provinceCandidates), "\nSelect province adjacent to Hue/Da Nang:").head)
    else
      Some(VC_Bot.pickSpaceTowardActiveOpposition(provinceCandidates).name)

    println()
    loggingPointsChanges {
      setSupport(Hue, Neutral)
      setSupport(DaNang, Neutral)
      province foreach { name => setSupport(name, Neutral) }
    }
  }
}
