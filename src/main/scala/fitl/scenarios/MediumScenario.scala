
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

package fitl.scenarios

import fitl.FireInTheLake._

class MediumScenario extends Scenario {
  val name                = "Medium: 1968-1972"
  val cardsPerCampaign    = 13 // 12 plus Coup
  val totalCoupCards      = 3
  val pivotCardsAvailable = Faction.ALL
  val usAid               = 30
  val econ                = 15
  val patronage           = 15
  val vcResources         = 15
  val nvaResources        = 20
  val arvnResources       = 30
  val trail               = 3
  val usPolicy            = USPolicy_LBJ
  val rvnLeadersInPlay    = List(RVN_Leader_NguyenCaoKy, RVN_Leader_YoungTurks, RVN_Leader_NguyenKhanh, RVN_Leader_DuongVanMinh)
  val outOfPlay           = Pieces(usTroops = 5, arvnTroops = 10, rangers_U = 3)
  val periodCapabilities  = List(AAA_Shaded, MainForceBns_Shaded, SA2s_Shaded, SearchAndDestroy_Shaded, ArcLight_Unshaded, M48Patton_Unshaded)

  val spaces = List(
    Default_Hue.copy(support = PassiveSupport).addPieces(Pieces(
      usTroops = 1, arvnPolice = 2)),
    Default_DaNang.copy(support = PassiveSupport).addPieces(Pieces(
      usTroops = 1, arvnPolice = 2)),
    Default_Kontum.copy(support = PassiveSupport).setPieces(Pieces(
      usBases = 1, usTroops = 1, irregulars_U = 1)),
    Default_QuiNhon.copy(support = PassiveSupport).addPieces(Pieces(
      usTroops = 1, arvnPolice = 2)),
    Default_CamRahn.copy(support = PassiveSupport).addPieces(Pieces(
      usTroops = 1, arvnPolice = 2)),
    Default_AnLoc.setPieces(Pieces(
      arvnTroops = 1, arvnPolice = 2)),
    Default_Saigon.copy(support = ActiveSupport).setPieces(Pieces(
      usBases    = 1, usTroops       = 2,
      arvnTroops = 1, arvnPolice     = 4, rangers_U = 1,
      vcBases    = 1, vcGuerrillas_U = 1)),
    Default_CanTho.copy(support = PassiveSupport).setPieces(Pieces(
        usTroops   = 3, irregulars_U = 1,
        arvnTroops = 2, arvnPolice   = 1)),
    Default_CentralLaos.setPieces(Pieces(
      nvaBases = 1, nvaGuerrillas_U = 1, nvaTroops = 9)),
    Default_SouthernLaos.setPieces(Pieces(
      nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_NortheastCambodia.setPieces(Pieces(
      nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_TheFishhook.setPieces(Pieces(
      nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_TheParrotsBeak.setPieces(Pieces(
      nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_Sihanoukville.setPieces(Pieces(
      nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_NorthVietnam.setPieces(Pieces(nvaBases = 1, nvaGuerrillas_U = 1, nvaTroops = 9)),
    Default_QuangTri_ThuaThien.copy(support = PassiveSupport).setPieces(Pieces(
      usBases    = 1, usTroops        = 4, irregulars_U = 1,
      arvnTroops = 3,
      nvaBases   = 1, nvaGuerrillas_U = 3)),
    Default_QuangNam.copy(support = ActiveOpposition).setPieces(Pieces(vcBases = 1, vcGuerrillas_U = 2)),
    Default_QuangTin_QuangNgai.copy(support = PassiveSupport).addPieces(Pieces(
      usBases    = 1, usTroops   = 2,
      arvnTroops = 2, arvnPolice = 1)),
    Default_BinhDinh.copy(support = ActiveSupport).setPieces(Pieces(
      usTroops   = 2, irregulars_U   = 1,
      arvnPolice = 1,
      vcBases    = 1, vcGuerrillas_U = 2)),
    Default_Pleiku_Darlac.copy(support = ActiveSupport).setPieces(Pieces(
      usTroops   = 2, irregulars_U   = 1,
      arvnPolice = 1,
      vcBases    = 1, vcGuerrillas_U = 2)),
    Default_PhuBon_PhuYen.copy(support = PassiveSupport).setPieces(Pieces(
      usTroops       = 3,
      arvnTroops     = 2, arvnPolice = 2,
      vcGuerrillas_U = 2)),
    Default_KhanhHoa.copy(support = ActiveSupport).setPieces(Pieces(
      usTroops   = 2, irregulars_U   = 1,
      arvnPolice = 1,
      vcBases    = 1, vcGuerrillas_U = 2)),
    Default_PhuocLong.setPieces(Pieces(
      vcBases         = 1, vcGuerrillas_U = 2,
      nvaGuerrillas_U = 1)),
    Default_QuangDuc_LongKhanh.setPieces(Pieces(
      arvnTroops     = 2, arvnPolice = 1,
      vcGuerrillas_U = 1)),
    Default_BinhTuy_BinhThuan.setPieces(Pieces(
      usBases    = 1, usTroops       = 2,
      arvnTroops = 3, arvnPolice     = 1,
      vcBases    = 1, vcGuerrillas_U = 2)),
    Default_TayNinh.copy(support = ActiveOpposition).setPieces(Pieces(
      usBases         = 1, usTroops       = 3,
      arvnTroops      = 2, rangers_U      = 1,
      vcTunnels       = 1, vcGuerrillas_U = 3,
      nvaGuerrillas_U = 2)),
    Default_KienPhong.copy(support = PassiveOpposition).setPieces(Pieces(
      arvnPolice     = 1,
      vcGuerrillas_U = 1)),
    Default_KienHoa_VinhBinh.copy(support = PassiveOpposition).setPieces(Pieces(
      arvnPolice     = 1,
      vcGuerrillas_U = 1)),
    Default_BaXuyen.copy(support = PassiveOpposition).setPieces(Pieces(
      arvnPolice     = 1,
      vcGuerrillas_U = 1)),
    Default_KienGiang_AnXuyen.copy(support = ActiveOpposition).setPieces(Pieces(
      arvnBases      = 1, arvnTroops = 2, rangers_U = 1,
      vcGuerrillas_U = 1))
  )
}

