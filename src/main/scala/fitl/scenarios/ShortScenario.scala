
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

class ShortScenario extends Scenario {
  val name                = "Short: 1965-1967"
  val cardsPerCampaign    = 9 // 8 plus Coup
  val totalCoupCards      = 3
  val pivotCardsAvailable = Set.empty[Faction]
  val usAid               = 15
  val econ                = 15
  val patronage           = 18
  val vcResources         = 10
  val nvaResources        = 15
  val arvnResources       = 30
  val trail               = 2
  val usPolicy            = USPolicy_LBJ
  val rvnLeadersInPlay    = List(RVN_Leader_YoungTurks, RVN_Leader_NguyenKhanh, RVN_Leader_DuongVanMinh)
  val outOfPlay           = Pieces(usTroops = 6, arvnTroops = 10, rangers_U = 3)
  val periodCapabilities  = List(AAA_Shaded)

  val spaces = List(
    Default_Hue.addPieces(Pieces(arvnPolice = 2)),
    Default_DaNang.copy(support = ActiveSupport).setPieces(Pieces(usTroops = 3, arvnPolice = 1)),
    Default_Kontum.copy(support = ActiveSupport).setPieces(Pieces(usTroops = 3, arvnPolice = 1)),
    Default_QuiNhon.copy(support = PassiveSupport).setPieces(Pieces(arvnPolice = 1)),
    Default_CamRahn.copy(support = PassiveSupport).setPieces(Pieces(arvnPolice = 1)),
    Default_AnLoc.copy(support = PassiveSupport).setPieces(Pieces(arvnPolice = 1)),
    Default_Saigon.copy(support = ActiveSupport).setPieces(Pieces(usBases = 1, usTroops = 3,
        arvnTroops = 4, arvnPolice = 2, rangers_U = 1)),
    Default_CanTho.copy(support = ActiveSupport).setPieces(Pieces(usBases = 1, usTroops = 3,
        arvnTroops = 4, arvnPolice = 2, rangers_U = 1)),
    Default_CentralLaos.setPieces(Pieces(nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_SouthernLaos.setPieces(Pieces(nvaBases = 2, nvaGuerrillas_U = 1, nvaTroops = 6)),
    Default_NortheastCambodia,
    Default_TheFishhook.setPieces(Pieces(nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_TheParrotsBeak.setPieces(Pieces(nvaBases = 1, nvaGuerrillas_U = 2)),
    Default_Sihanoukville,
    Default_NorthVietnam.setPieces(Pieces(nvaBases = 2, nvaGuerrillas_U = 1, nvaTroops = 6)),
    Default_QuangTri_ThuaThien.copy(support = ActiveOpposition).setPieces(Pieces(arvnBases = 1, arvnTroops = 2,
        nvaBases = 1, nvaGuerrillas_U = 4)),
    Default_QuangNam.setPieces(Pieces(rangers_U = 1, arvnPolice = 1)),
    Default_QuangTin_QuangNgai.setPieces(Pieces(usTroops = 2, arvnPolice = 1)),
    Default_BinhDinh.copy(support = PassiveSupport).setPieces(Pieces(usBases = 1, irregulars_U = 1, usTroops = 4,
        arvnTroops = 2, arvnPolice = 1, vcBases = 1, vcGuerrillas_U = 2)),
    Default_Pleiku_Darlac.setPieces(Pieces(usBases = 1, irregulars_U = 1, usTroops = 1, vcBases = 1, vcGuerrillas_U = 2)),
    Default_PhuBon_PhuYen,
    Default_KhanhHoa.setPieces(Pieces(irregulars_U = 1, usTroops = 1)),
    Default_PhuocLong,
    Default_QuangDuc_LongKhanh.copy(support = ActiveOpposition).setPieces(Pieces(vcBases = 1, vcGuerrillas_U = 2, nvaGuerrillas_U = 1)),
    Default_BinhTuy_BinhThuan.copy(support = PassiveSupport).setPieces(Pieces(usTroops = 2, arvnPolice = 1, vcBases = 1, vcGuerrillas_U = 2)),
    Default_TayNinh.copy(support = ActiveOpposition).setPieces(Pieces(vcTunnels = 1, vcGuerrillas_U = 2, nvaGuerrillas_U = 1)),
    Default_KienPhong.copy(support = ActiveOpposition).setPieces(Pieces(vcGuerrillas_U = 2)),
    Default_KienHoa_VinhBinh.addPieces(Pieces(arvnPolice = 2)),
    Default_BaXuyen.addPieces(Pieces(arvnPolice = 2)),
    Default_KienGiang_AnXuyen.copy(support = ActiveOpposition).setPieces(Pieces(vcGuerrillas_U = 2))
  )
}

