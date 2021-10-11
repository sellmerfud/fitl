
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

class FullScenario extends Scenario {
  val name                = "Full: 1964-1972"
  val cardsPerCampaign    = 13 // 12 plus Coup
  val totalCoupCards      = 6
  val pivotCardsAvailable = Faction.ALL
  val usAid               = 15
  val econ                = 15
  val patronage           = 15
  val vcResources         = 5
  val nvaResources        = 10
  val arvnResources       = 30
  val trail               = 1
  val usPolicy            = USPolicy_JFK
  val rvnLeadersInPlay    = List(RVN_Leader_DuongVanMinh)
  val outOfPlay           = Pieces(usBases = 2, usTroops = 10, arvnBases = 2, arvnTroops = 10, rangers_U = 3)
  val periodCapabilities  = List.empty

  val spaces = List(
    Default_Saigon.copy(support = PassiveSupport).setPieces(Pieces(
                usBases    = 1, usTroops   = 2,
                arvnTroops = 2, arvnPolice = 3)),
    Default_Hue.addPieces(Pieces(
                arvnTroops = 2, arvnPolice = 2)),
    Default_QuiNhon.copy(support = PassiveSupport).addPieces(Pieces(
                arvnTroops = 2, arvnPolice = 2)),
    Default_CamRahn.copy(support = PassiveSupport).addPieces(Pieces(
                arvnTroops = 2, arvnPolice = 2)),
    Default_AnLoc.copy(support = PassiveSupport).setPieces(Pieces(
                arvnTroops = 2, arvnPolice = 2)),
    Default_CanTho.copy(support = PassiveSupport).setPieces(Pieces(
                arvnTroops = 2, arvnPolice = 2)),
    Default_DaNang.addPieces(Pieces(
                usTroops = 2, arvnPolice = 1)),
    Default_Kontum.setPieces(Pieces(
                usTroops = 2, arvnPolice = 1)),
    Default_QuangTri_ThuaThien.setPieces(Pieces(
                usTroops = 1, irregulars_U   = 1,
                vcBases  = 1, vcGuerrillas_U = 2)),
    Default_BinhDinh.setPieces(Pieces(
                usTroops = 1, irregulars_U   = 1,
                vcBases  = 1, vcGuerrillas_U = 2)),
    Default_QuangNam.setPieces(Pieces(
                rangers_U = 1, arvnPolice = 1)),
    Default_Pleiku_Darlac.setPieces(Pieces(
                usBases = 1, irregulars_U   = 1, usTroops = 1,
                vcBases = 1, vcGuerrillas_U = 2)),
    Default_QuangTin_QuangNgai.copy(support = ActiveOpposition).addPieces(Pieces(
                vcBases = 1, vcGuerrillas_U = 2)),
    Default_QuangDuc_LongKhanh.copy(support = ActiveOpposition).addPieces(Pieces(
                vcBases = 1, vcGuerrillas_U = 2)),
    Default_BinhTuy_BinhThuan.copy(support = ActiveOpposition).addPieces(Pieces(
                vcBases = 1, vcGuerrillas_U = 2)),
    Default_TayNinh.copy(support = ActiveOpposition).setPieces(Pieces(
                vcTunnels = 1, vcGuerrillas_U = 2)),
    Default_PhuBon_PhuYen.copy(support = PassiveSupport).setPieces(Pieces(
                arvnPolice = 1)),
    Default_KhanhHoa.copy(support = PassiveSupport).setPieces(Pieces(
                arvnPolice = 1)),
    Default_KienHoa_VinhBinh.copy(support = PassiveSupport).setPieces(Pieces(
                arvnPolice = 1)),
    Default_BaXuyen.copy(support = PassiveSupport).setPieces(Pieces(
                arvnPolice = 1)),
    Default_KienPhong.copy(support = ActiveOpposition).setPieces(Pieces(
                vcGuerrillas_U = 1)),
    Default_KienGiang_AnXuyen.copy(support = ActiveOpposition).setPieces(Pieces(
                vcGuerrillas_U = 1)),
    Default_NorthVietnam.setPieces(Pieces(
                nvaBases = 1, nvaGuerrillas_U = 3)),      
    Default_CentralLaos.setPieces(Pieces(
                nvaBases = 1, nvaGuerrillas_U = 3)),
    Default_SouthernLaos.setPieces(Pieces(
                nvaBases = 1, nvaGuerrillas_U = 3)),
    Default_TheParrotsBeak.setPieces(Pieces(
                nvaBases = 1, nvaGuerrillas_U = 3))
  )
}

