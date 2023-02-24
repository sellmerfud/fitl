
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
// Drive on Vientiane: NVA removes 6 of its pieces total
// from North Vietnam and Laos.
//
// Shaded Text
// Trail security: If no COIN cubes in Laos, Improve Trail 2 boxes.
// If there are, US and ARVN Redeploy them to Vietnam.
//
// Tips
// "Pieces" can include Bases. "COIN" means US or ARVN.
// ARVN Redeploys ARVN Troops to Cities without NVA Control, any US or 
// ARVN Bases in South Vietnam, or Saigon, and
// Police to LoCs or COIN Control in South Vietnam (6.4.2).
// US Redeploys US Troops to anywhere in South Vietnam (including LoCs, if desired).

object Card_058 extends EventCard(58, "Pathet Lao",
  DualEvent,
  List(NVA, VC, ARVN, US),
  ListMap(US   -> (Performed   -> Unshaded),
          ARVN -> (Performed   -> Unshaded),
          NVA  -> (Ignored -> Shaded),
          VC   -> (Ignored -> Shaded))) {

  val unshadedCandidate = (sp: Space) =>
    (sp.isNorthVietnam || Laos.contains(sp.name)) &&
    sp.pieces.has(NVAPieces)

  def unshadedEffective(faction: Faction): Boolean = game.spaces exists unshadedCandidate

  def executeUnshaded(faction: Faction): Unit = {
    val candiates = game.spaces filter unshadedCandidate

    if (candiates.isEmpty)
      log("There are no NVA pieces in North Vietnam or Laos")
    else
      removePiecesFromMap(NVA, 6, NVAPieces, friendly = true, validSpaces = NorthVietnam::Laos);
  }

  def shadedEffective(faction: Faction): Boolean = false  // Not use by the Bots
  def executeShaded(faction: Faction): Unit = {
    val candidates = spaceNames(spaces(Laos) filter (_.pieces.has(CoinCubes)))

    if (candidates.isEmpty)
      improveTrail(2)
    else {
      val southVietnam   = (sp: Space) => isInSouthVietnam(sp.name)
      val arvnTroopDest  = (sp: Space) =>
        southVietnam(sp) && (
          sp.name == Saigon ||
          sp.pieces.has(CoinBases) ||
          (sp.isCity && !sp.nvaControlled)
        )
      val policeDest = (sp: Space) =>
        sp.isLoC ||
        (southVietnam(sp) && sp.coinControlled)

      val arvnTroopDests  = spaceNames(game.spaces filter arvnTroopDest).toSet
      val arvnPoliceDests = spaceNames(game.spaces filter policeDest).toSet
      val usTroopsDests   = spaceNames(game.spaces filter southVietnam).toSet

      loggingControlChanges {
        for (name <- candidates) {
          val sp = game.getSpace(name)
          val arvnTroops = sp.pieces.only(ARVNTroops)
          val arvnPolice = sp.pieces.only(ARVNPolice)
          val usTroops   = sp.pieces.only(USTroops)
  
          if (arvnTroops.nonEmpty) {
            log(s"\nRedeploying ARVN Troops out of $name")
            log(separator())
            movePiecesFromSpace(ARVN, name, arvnTroops, arvnTroopDests)
          }
  
          if (arvnPolice.nonEmpty) {
            log(s"\nRedeploying ARVN Police out of $name")
            log(separator())
            movePiecesFromSpace(ARVN, name, arvnPolice, arvnPoliceDests)
          }
  
          if (usTroops.nonEmpty) {
            log(s"\nRedeploying US Troops out of $name")
            log(separator())
            movePiecesFromSpace(US, name, usTroops, usTroopsDests)
          }
        }
      }
    }
  }
}
