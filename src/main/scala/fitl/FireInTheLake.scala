
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
import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import Ordering.Implicits._
import scala.language.implicitConversions
import FUtil.Pathname
import scenarios._

object FireInTheLake {

  val INTEGER = """(\d+)""".r

  def d6 = nextInt(6) + 1
  def d3 = nextInt(3) + 1

  val EdgeTrackMax = 75
  val TrailMin     = 0
  val TrailMax     = 4

  val TerrorMarkerManifest = 15

  val USPolicy_JFK   = "JFK"
  val USPolicy_LBJ   = "LBJ"
  val USPolicy_Nixon = "Nixon"

  sealed trait Faction {
    val name: String
    val sortOrder: Int
    val pivotCard: Int
    override def toString() = name
  }

  case object US extends Faction {
    val name = "US"
    val isBarbarian = false
    val sortOrder = 1
    val pivotCard = PivotalUS
  }

  case object ARVN extends Faction {
    val name = "ARVN"
    val sortOrder = 2
    val pivotCard = PivotalARVN
  }

  case object VC  extends Faction {
    val name = "VC"
    val sortOrder = 3
    val pivotCard = PivotalVC
  }

  case object NVA  extends Faction {
    val name = "NVA"
    val sortOrder = 4
    val pivotCard = PivotalNVA
  }

  object Faction {
    val ALL       = Set[Faction](US, ARVN, VC, NVA)
    val INSURGENT = Set[Faction](VC, NVA)
    val COIN      = ALL -- INSURGENT
    implicit val FactionOrdering = Ordering.by { faction: Faction => faction.sortOrder }
    val names = ALL.toSeq map (_.name)
    val maxNameLen = (names map (_.length)).max
    def apply(name: String): Faction = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid faction name: $name")
    }
    def others(faction: Faction): Set[Faction] = ALL - faction
  }

  type BotEventPriority = Int
  val NotExecuted: BotEventPriority = 0
  val Critical:    BotEventPriority = 1<<0
  val Performed:   BotEventPriority = 1<<1
  val PlaceHolder: BotEventPriority = 1<<2 // Remove once we know the actual values from the table


  sealed trait EventSelection
  case object NoEvent  extends EventSelection
  case object Unshaded extends EventSelection
  case object Shaded   extends EventSelection

  type BotEventSelector = Faction => EventSelection  // Used to see if Bot will execute the event.
  type CardEvent        = Faction => Unit

  // For cards that have only one event:
  //  dual == false and the event condtions and execution use the `unshaded` fields
  class Card(
    val number: Int,
    val name: String,
    val factions: ListMap[Faction, BotEventPriority],  // Nil for Coup cards
    val dual: Boolean,
    val unshadedSelection: BotEventSelector,
    val executeUnshaded: CardEvent,
    val shadedSelection: BotEventSelector,
    val executeShaded: CardEvent) {


    val isCoup = factions.isEmpty

    def numAndName = s"#$number - $name"
    override def toString() = numAndName

    def factionOrder = factions.keys.toList
    def orderString  = factionOrder map (_.name) mkString ", "
    def fullString = if (isCoup) s"$numAndName (Coup)" else s"$numAndName ($orderString)"
  }

  // Sort by card number
  implicit val CardOrdering = new Ordering[Card] {
    def compare(x: Card, y: Card) = x.number compare y.number
  }

  val RVN_Leader_DuongVanMinh   = "Duong Van Minh"     // Printed on map
  val RVN_Leader_NguyenKhanh    = "Nguyen Khanh"       // Coup card #125
  val RVN_Leader_YoungTurks     = "Young Turks"        // Coup card #126
  val RVN_Leader_NguyenCaoKy    = "Nguyen Cao Ky"      // Coup card #127
  val RVN_Leader_NguyenVanThieu = "Nguyen Van Thieu"   // Coup card #128
  val RVN_Leader_FailedCoup129  = "Failed Coup (#129)" // Coup card #129
  val RVN_Leader_FailedCoup130  = "Failed Coup (#130)" // Coup card #130

  val RVN_Leaders = List(
    RVN_Leader_DuongVanMinh,
    RVN_Leader_NguyenKhanh,
    RVN_Leader_YoungTurks,
    RVN_Leader_NguyenCaoKy,
    RVN_Leader_NguyenVanThieu,
    RVN_Leader_FailedCoup129,
    RVN_Leader_FailedCoup130
  )

  // Pivotal card numbers
  val PivotalUS   = 121
  val PivotalNVA  = 122
  val PivotalVC   = 123
  val PivotalARVN = 124

  object deck {
    val deckMap   = Cards.deckMap
    val CoupCards = Range.inclusive(125, 130).toSet
    val PivotalCards = Set(PivotalUS, PivotalNVA, PivotalVC, PivotalARVN)

    def isValidNumber(num: Int) = deckMap contains num
    def isPivotalCard(num: Int) = PivotalCards contains num
    def apply(num: Int): Card   = deckMap(num)
    def cards: List[Card]       = deckMap.valuesIterator.toList.sorted
  }


  // Cities
  val Hue     = "Hue"
  val DaNang  = "Da Nang"
  val Kontum  = "Kontum"
  val QuiNhon = "Qui Nhon"
  val CamRahn = "Cam Ranh"
  val AnLoc   = "An Loc"
  val Saigon  = "Saigon"
  val CanTho  = "Can Tho"

  // Provinces
  val CentralLaos        = "Central Laos"
  val SouthernLaos       = "Southern Laos"
  val NortheastCambodia  = "Northeast Cambodia"
  val TheFishhook        = "The Fishhook"
  val TheParrotsBeak     = "The Parrot's Beak"
  val Sihanoukville      = "Sihanoukville"
  val NorthVietnam       = "North Vietnam"
  val QuangTri_ThuaThien = "Quang Tri-Thua Thien"
  val QuangNam           = "Quang Nam"
  val QuangTin_QuangNgai = "Quang Tin Quang Ngai"
  val BinhDinh           = "Binh Dinh"
  val Pleiku_Darlac      = "Pleiku-Darlac"
  val PhuBon_PhuYen      = "Phu Bon-Phu Yen"
  val KhanhHoa           = "Khanh Hoa"
  val PhuocLong          = "Phuoc Long"
  val QuangDuc_LongKhanh = "Quang Duc-Long Khanh"
  val BinhTuy_BinhThuan  = "Binh Tuy-Binh Thuan"
  val TayNinh            = "Tay Ninh"
  val KienPhong          = "Kien Phong"
  val KienHoa_VinhBinh   = "Kien Hoa-Vinh Binh"
  val BaXuyen            = "Ba Xuyen"
  val KienGiang_AnXuyen  = "Kien Giang-An Xuyen"

  // LOCs
  val LOC_Hue_KheSanh             = "LOC Hue -- Khe Sanh"
  val LOC_Hue_DaNang              = "LOC Hue -- Da Nang"
  val LOC_DaNang_DakTo            = "LOC DaNang -- Dak To"
  val LOC_DaNang_QuiNhon          = "LOC DaNang -- Qui Nhon"
  val LOC_Kontum_DakTo            = "LOC Kontum -- Dak To"
  val LOC_Kontum_QuiNhon          = "LOC Kontum -- Qui Nhon"
  val LOC_Kontum_BanMeThuot       = "LOC Kontum -- Ban Me Thuot"
  val LOC_QuiNhon_CamRanh         = "LOC Qui Nhon -- Cam Ranh"
  val LOC_CamRanh_DaLat           = "LOC Cam Ranh -- Da Lat"
  val LOC_BanMeThuot_DaLat        = "LOC Ban Me Thuot -- Da Lat"
  val LOC_Saigon_CamRanh          = "LOC Saigon -- Cam Ranh"
  val LOC_Saigon_DaLat            = "LOC Saigon -- Da Lat"
  val LOC_Saigon_AnLoc_BanMeThuot = "LOC Saigon -- An Loc -- Ban Me Thuot"
  val LOC_Saigon_CanTho           = "LOC Saigon -- Can Tho"
  val LOC_CanTho_ChauDoc          = "LOC Can Tho -- Chau Doc"
  val LOC_CanTho_BacLieu          = "LOC Can Tho -- Bac Lieu"
  val LOC_CanTho_LongPhu          = "LOC Can Tho -- Long Phu"

  val LocLastOrdering = new Ordering[String] {
    def compare(x: String, y: String) = {
      (x startsWith "LOC", y startsWith "LOC") match {
        case (false, true) => -1
        case (true, false) => 1
        case _             => x compare y
      }
    }
  }

  // A space name and a list of adjacent space names
  val adjacencyMap: Map[String, Set[String]] = Map(
    // Cities
    Hue                         -> Set(QuangTri_ThuaThien, LOC_Hue_KheSanh, LOC_Hue_DaNang),
    DaNang                      -> Set(QuangNam, QuangTin_QuangNgai, LOC_Hue_DaNang, LOC_DaNang_QuiNhon,
                                       LOC_DaNang_DakTo),
    Kontum                      -> Set(BinhDinh, Pleiku_Darlac, PhuBon_PhuYen, LOC_Kontum_DakTo,
                                       LOC_Kontum_BanMeThuot, LOC_Kontum_QuiNhon),
    QuiNhon                     -> Set(BinhDinh, PhuBon_PhuYen, LOC_DaNang_QuiNhon, LOC_Kontum_QuiNhon,
                                       LOC_QuiNhon_CamRanh),
    CamRahn                     -> Set(KhanhHoa, BinhTuy_BinhThuan, LOC_QuiNhon_CamRanh, LOC_Saigon_CamRanh,
                                       LOC_CamRanh_DaLat),
    AnLoc                       -> Set(PhuocLong, TayNinh, TheFishhook, LOC_Saigon_AnLoc_BanMeThuot),
    Saigon                      -> Set(BinhTuy_BinhThuan, QuangDuc_LongKhanh, TayNinh, KienPhong,
                                       KienHoa_VinhBinh, LOC_Saigon_CamRanh, LOC_Saigon_DaLat,
                                       LOC_Saigon_AnLoc_BanMeThuot,
                                       LOC_Saigon_CanTho),
    CanTho                      -> Set(KienPhong, KienHoa_VinhBinh, BaXuyen, KienGiang_AnXuyen,
                                       LOC_Saigon_CanTho, LOC_CanTho_ChauDoc, LOC_CanTho_BacLieu,
                                       LOC_CanTho_LongPhu),

    // Provinces
    CentralLaos                 -> Set(NorthVietnam, QuangTri_ThuaThien, QuangNam, SouthernLaos,
                                       LOC_Hue_KheSanh),
    SouthernLaos                -> Set(CentralLaos, QuangNam, QuangTin_QuangNgai, BinhDinh, Pleiku_Darlac,
                                       LOC_DaNang_DakTo, LOC_Kontum_DakTo),
    NortheastCambodia           -> Set(SouthernLaos, TheFishhook, Pleiku_Darlac),
    TheFishhook                 -> Set(NortheastCambodia, TheParrotsBeak, AnLoc, Pleiku_Darlac,
                                       QuangDuc_LongKhanh, PhuocLong, TayNinh, LOC_Saigon_AnLoc_BanMeThuot),
    TheParrotsBeak              -> Set(TheFishhook, Sihanoukville, TayNinh, KienPhong, KienGiang_AnXuyen,
                                       LOC_CanTho_ChauDoc),
    Sihanoukville               -> Set(TheParrotsBeak, KienGiang_AnXuyen),
    NorthVietnam                -> Set(CentralLaos, QuangTri_ThuaThien, LOC_Hue_KheSanh),
    QuangTri_ThuaThien          -> Set(NorthVietnam, Hue, CentralLaos, QuangNam, LOC_Hue_KheSanh,
                                       LOC_Hue_DaNang),
    QuangNam                    -> Set(CentralLaos, SouthernLaos, QuangTri_ThuaThien, DaNang,
                                       QuangTin_QuangNgai, LOC_Hue_DaNang, LOC_DaNang_DakTo),
    QuangTin_QuangNgai          -> Set(SouthernLaos, DaNang, QuangNam, BinhDinh, LOC_DaNang_DakTo,
                                       LOC_DaNang_QuiNhon),
    BinhDinh                    -> Set(SouthernLaos, QuangTin_QuangNgai, QuiNhon, PhuBon_PhuYen, Kontum,
                                       Pleiku_Darlac, LOC_DaNang_DakTo, LOC_DaNang_QuiNhon, LOC_Kontum_DakTo,
                                       LOC_Kontum_QuiNhon),
    Pleiku_Darlac               -> Set(SouthernLaos, NortheastCambodia, TheFishhook, BinhDinh, Kontum,
                                       PhuBon_PhuYen, KhanhHoa, QuangDuc_LongKhanh, LOC_Kontum_DakTo,
                                       LOC_Kontum_BanMeThuot, LOC_DaNang_DakTo, LOC_BanMeThuot_DaLat,
                                       LOC_Saigon_AnLoc_BanMeThuot),
    PhuBon_PhuYen               -> Set(Kontum, BinhDinh, QuiNhon, KhanhHoa, Pleiku_Darlac,
                                       LOC_Kontum_QuiNhon, LOC_QuiNhon_CamRanh, LOC_Kontum_BanMeThuot),
    KhanhHoa                    -> Set(PhuBon_PhuYen, CamRahn, BinhTuy_BinhThuan, QuangDuc_LongKhanh,
                                       Pleiku_Darlac, LOC_QuiNhon_CamRanh, LOC_CamRanh_DaLat,
                                       LOC_BanMeThuot_DaLat, LOC_Kontum_BanMeThuot, LOC_Saigon_DaLat),
    PhuocLong                   -> Set(TheFishhook, AnLoc, QuangDuc_LongKhanh, TayNinh,
                                       LOC_Saigon_AnLoc_BanMeThuot),
    QuangDuc_LongKhanh          -> Set(TheFishhook, Pleiku_Darlac, KhanhHoa, BinhTuy_BinhThuan, Saigon,
                                       TayNinh, PhuocLong, LOC_Kontum_BanMeThuot,
                                       LOC_Saigon_AnLoc_BanMeThuot, LOC_BanMeThuot_DaLat, LOC_Saigon_DaLat),
    BinhTuy_BinhThuan           -> Set(Saigon, QuangDuc_LongKhanh, KhanhHoa, CamRahn, LOC_BanMeThuot_DaLat,
                                       LOC_CamRanh_DaLat, LOC_Saigon_DaLat, LOC_Saigon_CamRanh),
    TayNinh                     -> Set(TheParrotsBeak, TheFishhook, AnLoc, PhuocLong, QuangDuc_LongKhanh,
                                       Saigon, KienPhong, LOC_Saigon_AnLoc_BanMeThuot),
    KienPhong                   -> Set(TheParrotsBeak, TayNinh, Saigon, KienHoa_VinhBinh, CanTho,
                                       KienGiang_AnXuyen, LOC_CanTho_ChauDoc, LOC_Saigon_CanTho),
    KienHoa_VinhBinh            -> Set(Saigon, KienPhong, CanTho, BaXuyen, LOC_Saigon_CanTho,
                                       LOC_CanTho_LongPhu),
    BaXuyen                     -> Set(KienGiang_AnXuyen, CanTho, KienHoa_VinhBinh, LOC_CanTho_BacLieu,
                                       LOC_CanTho_LongPhu),
    KienGiang_AnXuyen           -> Set(Sihanoukville, TheParrotsBeak, KienPhong, CanTho, BaXuyen,
                                       LOC_CanTho_ChauDoc, LOC_CanTho_BacLieu),
    // LOCs
    LOC_Hue_KheSanh             -> Set(CentralLaos, NorthVietnam, Hue, QuangTri_ThuaThien),
    LOC_Hue_DaNang              -> Set(Hue, QuangTri_ThuaThien, QuangNam, DaNang),
    LOC_DaNang_DakTo            -> Set(DaNang, QuangNam, QuangTin_QuangNgai, SouthernLaos, BinhDinh,
                                       Pleiku_Darlac, LOC_Kontum_DakTo),
    LOC_DaNang_QuiNhon          -> Set(DaNang, QuangTin_QuangNgai, BinhDinh, QuiNhon),
    LOC_Kontum_DakTo            -> Set(Kontum, Pleiku_Darlac, SouthernLaos, BinhDinh),
    LOC_Kontum_QuiNhon          -> Set(Kontum, BinhDinh, QuiNhon, PhuBon_PhuYen),
    LOC_Kontum_BanMeThuot       -> Set(Kontum, Pleiku_Darlac, PhuBon_PhuYen, KhanhHoa, QuangDuc_LongKhanh,
                                       LOC_Saigon_AnLoc_BanMeThuot, LOC_BanMeThuot_DaLat),
    LOC_QuiNhon_CamRanh         -> Set(QuiNhon, PhuBon_PhuYen, KhanhHoa, CamRahn),
    LOC_CamRanh_DaLat           -> Set(CamRahn, KhanhHoa, BinhTuy_BinhThuan, QuangDuc_LongKhanh,
                                       LOC_Saigon_DaLat, LOC_BanMeThuot_DaLat),
    LOC_BanMeThuot_DaLat        -> Set(Pleiku_Darlac, KhanhHoa, BinhTuy_BinhThuan, QuangDuc_LongKhanh,
                                       LOC_Kontum_BanMeThuot, LOC_Saigon_AnLoc_BanMeThuot, LOC_Saigon_DaLat,
                                       LOC_CamRanh_DaLat),
    LOC_Saigon_CamRanh          -> Set(Saigon, CamRahn, BinhTuy_BinhThuan),
    LOC_Saigon_DaLat            -> Set(Saigon, BinhTuy_BinhThuan, QuangDuc_LongKhanh, KhanhHoa,
                                       LOC_BanMeThuot_DaLat, LOC_CamRanh_DaLat),
    LOC_Saigon_AnLoc_BanMeThuot -> Set(Saigon, TayNinh, QuangDuc_LongKhanh, PhuocLong, AnLoc, TheFishhook,
                                       Pleiku_Darlac, KhanhHoa, LOC_Kontum_BanMeThuot, LOC_BanMeThuot_DaLat),
    LOC_Saigon_CanTho           -> Set(Saigon, CanTho, KienPhong, KienHoa_VinhBinh),
    LOC_CanTho_ChauDoc          -> Set(CanTho, KienPhong, KienGiang_AnXuyen, TheParrotsBeak),
    LOC_CanTho_BacLieu          -> Set(CanTho, KienGiang_AnXuyen, BaXuyen),
    LOC_CanTho_LongPhu          -> Set(CanTho, BaXuyen, KienHoa_VinhBinh)
  )

  def getAdjacent(name: String): Set[String] = adjacencyMap(name)
  def areAdjacent(name1: String, name2: String) = getAdjacent(name1) contains name2
  def getAdjacentLOCs(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isLOC }
  def getAdjacentCities(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isCity }
  def getAdjacentLOCsAndCities(name: String) = getAdjacentLOCs(name) ++ getAdjacentCities(name)
  
  //  All spaces in Laos and Cambodia
  val LaosCambodia = List(CentralLaos, SouthernLaos, NortheastCambodia, TheFishhook, TheParrotsBeak, Sihanoukville)
  
  def isInLaosCambodia(name: String) = LaosCambodia contains name
  
  //  Return a sequence of all spaces that can be reached from the given space by patrolling cubes.
  //  A cube can move on to adjacent LOCs/Cities and keep moving via adjacent LOCs/Cities until
  //  it reaches a space with an insurgent piece.
  def getPatrolDestinations(srcName: String): Seq[String] = {
    @tailrec def getDests(candidates: Set[String], destinations: Set[String]): Set[String] = {
      if (candidates.isEmpty)
        destinations
      else {
        val name      = candidates.head
        val sp        = game.getSpace(name)
        val isDest    = name != srcName && (sp.isLOC || sp.isCity)
        val endOfPath = name != srcName && sp.pieces.has(InsurgentPieces)
        val adjacent  = if (endOfPath) Set.empty else (getAdjacentLOCsAndCities(name) - srcName)
        val newDests  = if (isDest) destinations + name else destinations
        val newCandidates = candidates.tail ++ adjacent -- destinations
        getDests(newCandidates, newDests)
      }          
    }
    
    getDests(Set(srcName), Set.empty).toList.sorted
  }

  // A space is adjacent for sweep movement if it is truly adjacent to the destination
  // or if it is adjacent to a LOC that is adjacent to the destination AND the LOC
  // free of Insurgent pieces
  def adjacentForSweep(srcName: String, destName: String): Boolean = {
    val locAccess = (name: String) => {
      val sp = game.getSpace(name)
      !sp.pieces.has(InsurgentPieces) && areAdjacent(name, destName)
    }
    
    areAdjacent(srcName, destName) || (getAdjacentLOCs(srcName) exists locAccess)
  }

  sealed trait PieceType {
    val name: String

    def singular        = name
    def plural          = s"${name}s"
    def genericSingular = singular
    def genericPlural   = s"${genericSingular}s"

    override def toString() = plural
  }

  type PieceTypeSet = Set[PieceType]

  case object USTroops       extends PieceType { val name = "US Troop" }
  case object Irregulars_U   extends PieceType { val name = "US Underground Irregular"; override def genericSingular = "US Irregular" }
  case object Irregulars_A   extends PieceType { val name = "US Active Irregular"; override def genericSingular = "US Irregular" }
  case object USBase         extends PieceType { val name = "US Base" }

  case object ARVNTroops     extends PieceType { val name = "ARVN Troop" }
  case object ARVNPolice     extends PieceType { val name = "ARVN Police"; override def plural = name; override def genericPlural = name }
  case object Rangers_U      extends PieceType { val name = "ARVN Underground Ranger"; override def genericSingular = "ARVN Ranger" }
  case object Rangers_A      extends PieceType { val name = "ARVN Active Ranger"; override def genericSingular = "ARVN Ranger" }
  case object ARVNBase       extends PieceType { val name = "ARVN Base" }

  case object NVATroops       extends PieceType { val name = "NVA Troop" }
  case object NVAGuerrillas_U extends PieceType { val name = "NVA Underground Guerrilla"; override def genericSingular = "NVA Guerrilla" }
  case object NVAGuerrillas_A extends PieceType { val name = "NVA Active Guerrilla"; override def genericSingular = "NVA Guerrilla" }
  case object NVABase         extends PieceType { val name = "NVA Base" }
  case object NVATunnel       extends PieceType { val name = "NVA Tunneled Base" }

  case object VCGuerrillas_U  extends PieceType { val name = "VC Underground Guerrilla"; override def genericSingular = "VC Guerrilla" }
  case object VCGuerrillas_A  extends PieceType { val name = "VC Active Guerrilla"; override def genericSingular = "VC Guerrilla" }
  case object VCBase          extends PieceType { val name = "VC Base" }
  case object VCTunnel        extends PieceType { val name = "VC Tunneled Base" }

  val USPieces        = List(USTroops, Irregulars_U, Irregulars_A, USBase)
  val ARVNPieces      = List(ARVNTroops, ARVNPolice, Rangers_U, Rangers_A, ARVNBase)
  val NVAPieces       = List(NVATroops, NVAGuerrillas_U, NVAGuerrillas_A, NVABase, NVATunnel)
  val VCPieces        = List(VCGuerrillas_U, VCGuerrillas_A, VCBase, VCTunnel)
  val CoinPieces      = USPieces:::ARVNPieces
  val InsurgentPieces = NVAPieces:::VCPieces
  val NonNVAPieces    = USPieces:::ARVNPieces:::VCPieces
  val CoinBases       = List(USBase, ARVNBase)
  val InsurgentBases  = List(NVATunnel, NVABase, VCTunnel, VCBase)
  val InsurgentNonTunnels = List(NVABase, VCBase)
  val InsurgentTunnels    = List(NVATunnel, VCTunnel)
  val Irregulars      = List(Irregulars_A, Irregulars_U)
  val Rangers         = List(Rangers_A, Rangers_U)
  val NVAGuerrillas   = List(NVAGuerrillas_A, NVAGuerrillas_U)
  val VCGuerrillas    = List(VCGuerrillas_A, VCGuerrillas_U)
  val ARVNCubes       = List(ARVNPolice, ARVNTroops)
  val FlippablePieces = List(Irregulars_U, Irregulars_A, Rangers_U, Rangers_A,
                             NVAGuerrillas_U, NVAGuerrillas_A, VCGuerrillas_U, VCGuerrillas_A)
  val Guerrillas      = List(NVAGuerrillas_A, VCGuerrillas_A, NVAGuerrillas_U, VCGuerrillas_U)
  val ActiveGuerrillas      = List(NVAGuerrillas_A, VCGuerrillas_A)
  val UndergroundGuerrillas = List(NVAGuerrillas_U, VCGuerrillas_U)
  val CoinForces      = List(USTroops, Irregulars_A, Irregulars_U, ARVNTroops, ARVNPolice, Rangers_A, Rangers_U)

  val factionPieces: Map[Faction, List[PieceType]] = Map(
    US   -> USPieces,
    ARVN -> ARVNPieces,
    NVA  -> NVAPieces,
    VC   -> VCPieces)

  val BasePieces = List(USBase, ARVNBase, NVABase, NVATunnel, VCBase, VCTunnel)
  val AllPieceTypes = USPieces:::ARVNPieces:::NVAPieces:::VCPieces

  val Forces = AllPieceTypes filterNot isBase

  def isBase(pieceType: PieceType)        = BasePieces contains pieceType
  def isForce(pieceType: PieceType)       = Forces contains pieceType
  def isFlippable(pieceType: PieceType)   = FlippablePieces contains pieceType

  //  Normalize active types to their undeground counterparts,
  //  and tunneled bases to their non-tunnel counterparts
  def normalizedType(pieceType: PieceType): PieceType = pieceType match {
    case Irregulars_A | Irregulars_U       => Irregulars_U
    case Rangers_A | Rangers_U             => Rangers_U
    case VCGuerrillas_A | VCGuerrillas_U   => VCGuerrillas_U
    case NVAGuerrillas_A | NVAGuerrillas_U => NVAGuerrillas_U
    case VCTunnel | VCBase                 => VCBase
    case NVATunnel | NVABase               => NVABase
    case _                                 => pieceType
  }

  def simiarTypes(pieceType: PieceType): Seq[PieceType] = pieceType match {
    case Irregulars_A | Irregulars_U       => Seq(Irregulars_A, Irregulars_U)
    case Rangers_A | Rangers_U             => Seq(Rangers_A, Rangers_U)
    case VCGuerrillas_A | VCGuerrillas_U   => Seq(VCGuerrillas_A, VCGuerrillas_U)
    case NVAGuerrillas_A | NVAGuerrillas_U => Seq(NVAGuerrillas_A, NVAGuerrillas_U)
    case NVABase | NVATunnel               => Seq(NVABase, NVATunnel)
    case VCBase | VCTunnel                 => Seq(VCBase, VCTunnel)
    case _                                 => Seq(pieceType)
  }


  def owner(pieceType: PieceType): Faction = pieceType match {
    case t if USPieces contains t   => US
    case t if ARVNPieces contains t => ARVN
    case t if NVAPieces contains t  => NVA
    case _                          => VC
  }


  // Class used to keep track of the pieces in a particular space
  case class Pieces(
    usTroops: Int        = 0,
    irregulars_U: Int    = 0,
    irregulars_A: Int    = 0,
    usBases: Int         = 0,
    arvnTroops: Int      = 0,
    arvnPolice: Int      = 0,
    rangers_U: Int       = 0,
    rangers_A: Int       = 0,
    arvnBases: Int       = 0,
    nvaTroops: Int       = 0,
    nvaGuerrillas_U: Int = 0,
    nvaGuerrillas_A: Int = 0,
    nvaBases: Int        = 0,
    nvaTunnels: Int      = 0,
    vcGuerrillas_U: Int  = 0,
    vcGuerrillas_A: Int  = 0,
    vcBases: Int         = 0,
    vcTunnels: Int       = 0) {

    val totalNVABases       = nvaBases + nvaTunnels
    val totalVCBases        = vcBases  + vcTunnels

    def totalFaction(faction: Faction)  = totalOf(factionPieces(faction))
    def hasForce(faction: Faction): Boolean = only(Forces).totalFaction(faction) > 0
    def hasBase(faction: Faction) = only(BasePieces).totalFaction(faction) > 0
    def hasExposedInsurgents = has(NVATroops) ||
                               has(ActiveGuerrillas) ||
                               (!has(Guerrillas) && has(InsurgentBases))

    def total = totalOf(AllPieceTypes)

    // For some force calculations we want to know simply the total pieces
    // regardless of active/underground and regarless of tunneled bases
    def normalized: Pieces = copy(
      irregulars_U    = irregulars_U + irregulars_A,
      irregulars_A    = 0,
      rangers_U       = rangers_U + rangers_A,
      rangers_A       = 0,
      vcGuerrillas_U  = vcGuerrillas_U + vcGuerrillas_A,
      vcGuerrillas_A  = 0,
      nvaGuerrillas_U = nvaGuerrillas_U + nvaGuerrillas_A,
      nvaGuerrillas_A = 0,
      vcBases         = vcBases + vcTunnels,
      vcTunnels       = 0,
      nvaBases        = nvaBases + nvaTunnels,
      nvaTunnels      = 0)

    def isEmpty = total == 0
    def nonEmpty = !isEmpty

    def totalBases = totalOf(BasePieces)

    def descriptions = AllPieceTypes filter (numOf(_) > 0) map (t => amtPiece(numOf(t), t))

    override def toString() = if (isEmpty) "none" else descriptions.mkString(", ")

    def totalOf(pieceTypes: Seq[PieceType]) = pieceTypes.foldLeft(0) { (num, piece) => num + numOf(piece) }

    // Return true of this Pieces instance contains at least all of the specified pieces.
    def contains(query: Pieces): Boolean = AllPieceTypes forall (t => numOf(t) >= query.numOf(t))

    // Return true if this Pieces instance has at least one of the given piece type
    def has(pt: PieceType): Boolean = numOf(pt) > 0

    // Return true if this Pieces instance has at least one of any of the given piece types
    def has(pts: Seq[PieceType]): Boolean = totalOf(pts) > 0

    def numOf(pieceType: PieceType): Int = pieceType match {
      case USTroops        => usTroops
      case Irregulars_U    => irregulars_U
      case Irregulars_A    => irregulars_A
      case USBase          => usBases
      case ARVNTroops      => arvnTroops
      case ARVNPolice      => arvnPolice
      case Rangers_U       => rangers_U
      case Rangers_A       => rangers_A
      case ARVNBase        => arvnBases
      case NVATroops       => nvaTroops
      case NVAGuerrillas_U => nvaGuerrillas_U
      case NVAGuerrillas_A => nvaGuerrillas_A
      case NVABase         => nvaBases
      case NVATunnel       => nvaTunnels
      case VCGuerrillas_U  => vcGuerrillas_U
      case VCGuerrillas_A  => vcGuerrillas_A
      case VCBase          => vcBases
      case VCTunnel        => vcTunnels
    }

    def set(num: Int, pieceType: PieceType): Pieces = pieceType match {
      case USTroops        => copy(usTroops        = num)
      case Irregulars_U    => copy(irregulars_U    = num)
      case Irregulars_A    => copy(irregulars_A    = num)
      case USBase          => copy(usBases         = num)
      case ARVNTroops      => copy(arvnTroops      = num)
      case ARVNPolice      => copy(arvnPolice      = num)
      case Rangers_U       => copy(rangers_U       = num)
      case Rangers_A       => copy(rangers_A       = num)
      case ARVNBase        => copy(arvnBases       = num)
      case NVATroops       => copy(nvaTroops       = num)
      case NVAGuerrillas_U => copy(nvaGuerrillas_U = num)
      case NVAGuerrillas_A => copy(nvaGuerrillas_A = num)
      case NVABase         => copy(nvaBases        = num)
      case NVATunnel       => copy(nvaTunnels      = num)
      case VCGuerrillas_U  => copy(vcGuerrillas_U  = num)
      case VCGuerrillas_A  => copy(vcGuerrillas_A  = num)
      case VCBase          => copy(vcBases         = num)
      case VCTunnel        => copy(vcTunnels       = num)
    }

    def add(num: Int, pieceType: PieceType): Pieces = pieceType match {
      case USTroops        => copy(usTroops        = usTroops + num)
      case Irregulars_U    => copy(irregulars_U    = irregulars_U + num)
      case Irregulars_A    => copy(irregulars_A    = irregulars_A + num)
      case USBase          => copy(usBases         = usBases + num)
      case ARVNTroops      => copy(arvnTroops      = arvnTroops + num)
      case ARVNPolice      => copy(arvnPolice      = arvnPolice + num)
      case Rangers_U       => copy(rangers_U       = rangers_U + num)
      case Rangers_A       => copy(rangers_A       = rangers_A + num)
      case ARVNBase        => copy(arvnBases       = arvnBases + num)
      case NVATroops       => copy(nvaTroops       = nvaTroops + num)
      case NVAGuerrillas_U => copy(nvaGuerrillas_U = nvaGuerrillas_U + num)
      case NVAGuerrillas_A => copy(nvaGuerrillas_A = nvaGuerrillas_A + num)
      case NVABase         => copy(nvaBases        = nvaBases + num)
      case NVATunnel       => copy(nvaTunnels      = nvaTunnels + num)
      case VCGuerrillas_U  => copy(vcGuerrillas_U  = vcGuerrillas_U + num)
      case VCGuerrillas_A  => copy(vcGuerrillas_A  = vcGuerrillas_A + num)
      case VCBase          => copy(vcBases         = vcBases + num)
      case VCTunnel        => copy(vcTunnels       = vcTunnels + num)
    }

    def remove(num: Int, pieceType: PieceType): Pieces = pieceType match {
      case USTroops        => copy(usTroops        = (usTroops - num) max 0)
      case Irregulars_U    => copy(irregulars_U    = (irregulars_U - num) max 0)
      case Irregulars_A    => copy(irregulars_A    = (irregulars_A - num) max 0)
      case USBase          => copy(usBases         = (usBases - num) max 0)
      case ARVNTroops      => copy(arvnTroops      = (arvnTroops - num) max 0)
      case ARVNPolice      => copy(arvnPolice      = (arvnPolice - num) max 0)
      case Rangers_U       => copy(rangers_U       = (rangers_U - num) max 0)
      case Rangers_A       => copy(rangers_A       = (rangers_A - num) max 0)
      case ARVNBase        => copy(arvnBases       = (arvnBases - num) max 0)
      case NVATroops       => copy(nvaTroops       = (nvaTroops - num) max 0)
      case NVAGuerrillas_U => copy(nvaGuerrillas_U = (nvaGuerrillas_U - num) max 0)
      case NVAGuerrillas_A => copy(nvaGuerrillas_A = (nvaGuerrillas_A - num) max 0)
      case NVABase         => copy(nvaBases        = (nvaBases - num) max 0)
      case NVATunnel       => copy(nvaTunnels      = (nvaTunnels - num) max 0)
      case VCGuerrillas_U  => copy(vcGuerrillas_U  = (vcGuerrillas_U - num) max 0)
      case VCGuerrillas_A  => copy(vcGuerrillas_A  = (vcGuerrillas_A - num) max 0)
      case VCBase          => copy(vcBases         = (vcBases - num) max 0)
      case VCTunnel        => copy(vcTunnels       = (vcTunnels - num) max 0)
    }

    def only(pieceTypes: Seq[PieceType]): Pieces =
      pieceTypes.foldLeft(Pieces()) { (pieces, t) => pieces.add(numOf(t), t) }

    def only(pieceType: PieceType): Pieces = Pieces().add(numOf(pieceType), pieceType)

    def except(pieceTypes: Seq[PieceType]): Pieces =
      pieceTypes.foldLeft(this) { (pieces, t) => pieces.set(0, t) }

    def except(pieceType: PieceType): Pieces = except(Seq(pieceType))

    def + (added: Pieces): Pieces = Pieces(
      usTroops        = usTroops        + added.usTroops,
      irregulars_U    = irregulars_U    + added.irregulars_U,
      irregulars_A    = irregulars_A    + added.irregulars_A,
      usBases         = usBases         + added.usBases,
      arvnTroops      = arvnTroops      + added.arvnTroops,
      arvnPolice      = arvnPolice      + added.arvnPolice,
      rangers_U       = rangers_U       + added.rangers_U,
      rangers_A       = rangers_A       + added.rangers_A,
      arvnBases       = arvnBases       + added.arvnBases,
      nvaTroops       = nvaTroops       + added.nvaTroops,
      nvaGuerrillas_U = nvaGuerrillas_U + added.nvaGuerrillas_U,
      nvaGuerrillas_A = nvaGuerrillas_A + added.nvaGuerrillas_A,
      nvaBases        = nvaBases        + added.nvaBases,
      nvaTunnels      = nvaTunnels      + added.nvaTunnels,
      vcGuerrillas_U  = vcGuerrillas_U  + added.vcGuerrillas_U,
      vcGuerrillas_A  = vcGuerrillas_A  + added.vcGuerrillas_A,
      vcBases         = vcBases         + added.vcBases,
      vcTunnels       = vcTunnels       + added.vcTunnels)

    def - (removed: Pieces): Pieces = Pieces(
      usTroops        = (usTroops        - removed.usTroops) max 0,
      irregulars_U    = (irregulars_U    - removed.irregulars_U) max 0,
      irregulars_A    = (irregulars_A    - removed.irregulars_A) max 0,
      usBases         = (usBases         - removed.usBases) max 0,
      arvnTroops      = (arvnTroops      - removed.arvnTroops) max 0,
      arvnPolice      = (arvnPolice      - removed.arvnPolice) max 0,
      rangers_U       = (rangers_U       - removed.rangers_U) max 0,
      rangers_A       = (rangers_A       - removed.rangers_A) max 0,
      arvnBases       = (arvnBases       - removed.arvnBases) max 0,
      nvaTroops       = (nvaTroops       - removed.nvaTroops) max 0,
      nvaGuerrillas_U = (nvaGuerrillas_U - removed.nvaGuerrillas_U) max 0,
      nvaGuerrillas_A = (nvaGuerrillas_A - removed.nvaGuerrillas_A) max 0,
      nvaBases        = (nvaBases        - removed.nvaBases) max 0,
      nvaTunnels      = (nvaTunnels      - removed.nvaTunnels) max 0,
      vcGuerrillas_U  = (vcGuerrillas_U  - removed.vcGuerrillas_U) max 0,
      vcGuerrillas_A  = (vcGuerrillas_A  - removed.vcGuerrillas_A) max 0,
      vcBases         = (vcBases         - removed.vcBases) max 0,
      vcTunnels       = (vcTunnels       - removed.vcTunnels) max 0)


    def getTypes: List[PieceType] = AllPieceTypes filter has

    // Convert to a list where each the piece type of each piece occupies
    // and entry in the list.
    def explode(order: List[PieceType] = AllPieceTypes): List[PieceType] = {
      order flatMap { pieceType => List.fill(numOf(pieceType))(pieceType) }
    }
  }

  object Pieces {
    def fromTypes(collection: Seq[PieceType]) =
      collection.foldLeft(Pieces()) { (pieces, t) => pieces.add(1, t) }
  }

  // Convenience function
  // Allows us to use `space.usTroops` in place of `space.pieces.usTroops`
  // implicit def spacePieces(sp: Space): Pieces = sp.pieces

  sealed trait SpaceType
  {
    val name: String
    override def toString() = name
  }

  case object City             extends SpaceType { val name = "City"}
  case object HighlandProvince extends SpaceType { val name = "Highland Province"}
  case object LowlandProvince  extends SpaceType { val name = "Lowland Province"}
  case object JungleProvince   extends SpaceType { val name = "Jungle Province"}
  case object LOC              extends SpaceType { val name = "LOC"}

  object SpaceType {
    val ALL       = Set[SpaceType](City, HighlandProvince, LowlandProvince, JungleProvince, LOC)
    def apply(name: String): SpaceType = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SpaceType name: $name")
    }
  }

  sealed trait SupportType extends Ordered[SupportType] {
    val name: String
    val value: Int
    def compare(that: SupportType) = this.value - that.value

    override def toString() = name
  }
  case object ActiveSupport     extends SupportType { val name = "Active Support" ;    val value = 4 }
  case object PassiveSupport    extends SupportType { val name = "Passive Support";    val value = 3 }
  case object Neutral           extends SupportType { val name = "Neutral";            val value = 2 }
  case object PassiveOpposition extends SupportType { val name = "Passive Opposition"; val value = 1 }
  case object ActiveOpposition  extends SupportType { val name = "Active Opposition";  val value = 0 }

  object SupportType {
    val ALL = Set[SupportType](Neutral, PassiveSupport, ActiveSupport, PassiveOpposition, ActiveOpposition)
    def apply(name: String): SupportType = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SupportType name: $name")
    }

    def apply(value: Int): SupportType = ALL.find(_.value == value) getOrElse {
      throw new IllegalArgumentException(s"Invalid SupportType value: $value")
    }
  }

  sealed trait Control {
    val name: String

    override def toString() = name
  }
  case object Uncontrolled extends Control { val name = "Uncontrolled"    }
  case object CoinControl  extends Control { val name = "COIN Control" }
  case object NvaControl   extends Control { val name = "NVA Control"  }

  //  Definition of a map space: City, Province, LOC

  case class Space(
    name:       String,
    spaceType:  SpaceType,
    population: Int         = 0,   // Also used for econ value on LOCs
    coastal:    Boolean     = false,
    support:    SupportType = Neutral,
    pieces:     Pieces      = Pieces(),
    terror:     Int         = 0) {   // Sabatoge on LOCs

    override def toString() = name

    def printedEconValue = population  // overloaded field
    def currentEconValue = if (isLOC && terror == 0) printedEconValue else 0
    
    def isCity     = spaceType == City
    def isProvince = spaceType == HighlandProvince || spaceType == LowlandProvince || spaceType == JungleProvince
    def isLOC      = spaceType == LOC

    def isHighland = spaceType == HighlandProvince
    def isLowland  = spaceType == LowlandProvince
    def isJungle   = spaceType == JungleProvince

    def isNorthVietnam = name == NorthVietnam

    def coinControlled: Boolean = pieces.totalOf(CoinPieces) > pieces.totalOf(InsurgentPieces)
    def nvaControlled: Boolean  = pieces.totalOf(NVAPieces) > pieces.totalOf(NonNVAPieces)
    def control = if      (!isLOC && coinControlled) CoinControl
                  else if (!isLOC && nvaControlled)  NvaControl
                  else                               Uncontrolled

    def supportValue: Int = support match {
      case PassiveSupport => population
      case ActiveSupport  => 2 * population
      case _              => 0
    }
    def oppositionValue: Int = support match {
      case PassiveOpposition => population
      case ActiveOpposition  => 2 * population
      case _                 => 0
    }

    def coinControlValue: Int = if (!isLOC && coinControlled) population else 0
    def nvaControlValue: Int  = if (!isLOC && nvaControlled)  population else 0

    def totalBases = pieces.totalOf(BasePieces)
    def addPieces(newPieces: Pieces): Space = copy(pieces = pieces + newPieces)
    def removePieces(removedPieces: Pieces): Space = copy(pieces = pieces - removedPieces)
    def setPieces(newPieces: Pieces): Space = copy(pieces = newPieces)

    def assaultCubes(faction: Faction): Int = faction match {
      case US                   => pieces.numOf(USTroops)
      case _ if isCity || isLOC => pieces.totalOf(ARVNCubes)
      case _                    => pieces.numOf(ARVNTroops)
    }
    
    def sweepForces(faction: Faction): Int = if (faction == US)
      pieces.totalOf(USTroops::Irregulars_U::Irregulars_A::Nil)
    else
      pieces.totalOf(ARVNTroops::ARVNPolice::Rangers_U::Rangers_A::Nil)

    def assaultMultiplier(faction: Faction): Double = faction match {
      case US if pieces.has(USBase) => 2.0
      case US if isHighland         => 1.0/2.0
      case US                       => 1.0
      case _ if isHighland          => 1.0/3.0
      case _                        => 1.0/2.0
    }

    def assaultLosses(faction: Faction): Int = (assaultCubes(faction) * assaultMultiplier(faction)).toInt
    
    def sweepActivations(faction: Faction): Int = {
      if (isJungle)
        sweepForces(faction) / 2
      else
        sweepForces(faction)
    }

  }

  //  Order spaces:
  //    Cities first
  //    Provinces next
  //    LOCs last
  //    Alphabetic within each category
  implicit object SpaceOrdering extends Ordering[Space] {
    private def categoryValue(sp: Space) = sp.spaceType match {
      case City => 0
      case LOC  => 2
      case _    => 1  // All provinces
    }

    def compare(a: Space, b: Space) = {
      val catDiff = categoryValue(a) - categoryValue(b)
      if (catDiff == 0)
          a.name compareTo b.name
      else
        catDiff
    }
  }


  // Default space definitions
  val Default_Hue     = Space(Hue,    City, 2, coastal = true)
  val Default_DaNang  = Space(DaNang, City, 1, coastal = true)
  val Default_Kontum  = Space(Kontum, City, 1)
  val Default_QuiNhon = Space(QuiNhon, City, 1, coastal = true)
  val Default_CamRahn = Space(CamRahn, City, 1, coastal = true)
  val Default_AnLoc   = Space(AnLoc , City, 1)
  val Default_Saigon  = Space(Saigon, City, 6, coastal = true)
  val Default_CanTho  = Space(CanTho, City, 1)

  val Default_CentralLaos        = Space(CentralLaos,        JungleProvince,   0)
  val Default_SouthernLaos       = Space(SouthernLaos,       JungleProvince,   0)
  val Default_NortheastCambodia  = Space(NortheastCambodia,  JungleProvince,   0)
  val Default_TheFishhook        = Space(TheFishhook,        JungleProvince,   0)
  val Default_TheParrotsBeak     = Space(TheParrotsBeak,     JungleProvince,   0)
  val Default_Sihanoukville      = Space(Sihanoukville,      JungleProvince,   0, coastal = true)
  val Default_NorthVietnam       = Space(NorthVietnam,       HighlandProvince, 0, coastal = true)
  val Default_QuangTri_ThuaThien = Space(QuangTri_ThuaThien, HighlandProvince, 2, coastal = true)
  val Default_QuangNam           = Space(QuangNam,           HighlandProvince, 1, coastal = true)
  val Default_QuangTin_QuangNgai = Space(QuangTin_QuangNgai, LowlandProvince,  2, coastal = true)
  val Default_BinhDinh           = Space(BinhDinh,           HighlandProvince, 2, coastal = true)
  val Default_Pleiku_Darlac      = Space(Pleiku_Darlac,      HighlandProvince, 1)
  val Default_PhuBon_PhuYen      = Space(PhuBon_PhuYen,      LowlandProvince,  1, coastal = true)
  val Default_KhanhHoa           = Space(KhanhHoa,           HighlandProvince, 1, coastal = true)
  val Default_PhuocLong          = Space(PhuocLong,          JungleProvince,   0)
  val Default_QuangDuc_LongKhanh = Space(QuangDuc_LongKhanh, JungleProvince,   1)
  val Default_BinhTuy_BinhThuan  = Space(BinhTuy_BinhThuan,  JungleProvince,   1, coastal = true)
  val Default_TayNinh            = Space(TayNinh,            JungleProvince,   2)
  val Default_KienPhong          = Space(KienPhong,          LowlandProvince,  2)
  val Default_KienHoa_VinhBinh   = Space(KienHoa_VinhBinh,   LowlandProvince,  2, coastal = true)
  val Default_BaXuyen            = Space(BaXuyen,            LowlandProvince,  1, coastal = true)
  val Default_KienGiang_AnXuyen  = Space(KienGiang_AnXuyen,  LowlandProvince,  2, coastal = true)

  val Default_LOC_Hue_KheSanh             = Space(LOC_Hue_KheSanh,             LOC, 1, coastal = true)
  val Default_LOC_Hue_DaNang              = Space(LOC_Hue_DaNang,              LOC, 1, coastal = true)
  val Default_LOC_DaNang_DakTo            = Space(LOC_DaNang_DakTo,            LOC, 0)
  val Default_LOC_DaNang_QuiNhon          = Space(LOC_DaNang_QuiNhon,          LOC, 1, coastal = true)
  val Default_LOC_Kontum_DakTo            = Space(LOC_Kontum_DakTo,            LOC, 1)
  val Default_LOC_Kontum_QuiNhon          = Space(LOC_Kontum_QuiNhon,          LOC, 1)
  val Default_LOC_Kontum_BanMeThuot       = Space(LOC_Kontum_BanMeThuot,       LOC, 1)
  val Default_LOC_QuiNhon_CamRanh         = Space(LOC_QuiNhon_CamRanh,         LOC, 1, coastal = true)
  val Default_LOC_CamRanh_DaLat           = Space(LOC_CamRanh_DaLat,           LOC, 1)
  val Default_LOC_BanMeThuot_DaLat        = Space(LOC_BanMeThuot_DaLat,        LOC, 0)
  val Default_LOC_Saigon_CamRanh          = Space(LOC_Saigon_CamRanh,          LOC, 1, coastal = true)
  val Default_LOC_Saigon_DaLat            = Space(LOC_Saigon_DaLat,            LOC, 1)
  val Default_LOC_Saigon_AnLoc_BanMeThuot = Space(LOC_Saigon_AnLoc_BanMeThuot, LOC, 1)
  val Default_LOC_Saigon_CanTho           = Space(LOC_Saigon_CanTho,           LOC, 2)
  val Default_LOC_CanTho_ChauDoc          = Space(LOC_CanTho_ChauDoc,          LOC, 1)
  val Default_LOC_CanTho_BacLieu          = Space(LOC_CanTho_BacLieu,          LOC, 0, coastal = true)
  val Default_LOC_CanTho_LongPhu          = Space(LOC_CanTho_LongPhu,          LOC, 1, coastal = true)

  val DefaultSpaces = List(
    Default_Hue,
    Default_DaNang,
    Default_Kontum,
    Default_QuiNhon,
    Default_CamRahn,
    Default_AnLoc,
    Default_Saigon,
    Default_CanTho,
    Default_CentralLaos,
    Default_SouthernLaos,
    Default_NortheastCambodia,
    Default_TheFishhook,
    Default_TheParrotsBeak,
    Default_Sihanoukville,
    Default_NorthVietnam,
    Default_QuangTri_ThuaThien,
    Default_QuangNam,
    Default_QuangTin_QuangNgai,
    Default_BinhDinh,
    Default_Pleiku_Darlac,
    Default_PhuBon_PhuYen,
    Default_KhanhHoa,
    Default_PhuocLong,
    Default_QuangDuc_LongKhanh,
    Default_BinhTuy_BinhThuan,
    Default_TayNinh,
    Default_KienPhong,
    Default_KienHoa_VinhBinh,
    Default_BaXuyen,
    Default_KienGiang_AnXuyen,
    Default_LOC_Hue_KheSanh,
    Default_LOC_Hue_DaNang,
    Default_LOC_DaNang_DakTo,
    Default_LOC_DaNang_QuiNhon,
    Default_LOC_Kontum_DakTo,
    Default_LOC_Kontum_QuiNhon,
    Default_LOC_Kontum_BanMeThuot,
    Default_LOC_QuiNhon_CamRanh,
    Default_LOC_CamRanh_DaLat,
    Default_LOC_BanMeThuot_DaLat,
    Default_LOC_Saigon_CamRanh,
    Default_LOC_Saigon_DaLat,
    Default_LOC_Saigon_AnLoc_BanMeThuot,
    Default_LOC_Saigon_CanTho,
    Default_LOC_CanTho_ChauDoc,
    Default_LOC_CanTho_BacLieu,
    Default_LOC_CanTho_LongPhu)

  val SpaceNames = DefaultSpaces.sorted map (_.name)

  // Markers
  val Marker_PeaceTalks = "Peace Talks"

  // Capabilities
  val Cap_TopGun             = "#4 Top Gun"                  // affects air strike
  val Cap_ArcLight           = "#8 Arc Light"                // affects air strike
  val Cap_Abrams             = "#11 Abrams"                  // affects assault
  val Cap_Cobras             = "#13 Cobras"                  // affects sweep / assault
  val Cap_M48Patton          = "#14 M-48 Patton"             // affects assault / patrol
  val Cap_CombActionPlatoons = "#18 Combined Acton Platoons" // affects training / sweep
  val Cap_CORDS              = "#19 CORDS"                   // affects training
  val Cap_LaserGuidedBombs   = "#20 Laser Guided Bombs"      // affects air strike
  val Cap_SearchAndDestroy   = "#28 Search And Destroy"      // affects assault
  val Cap_AAA                = "#31 AAA"                     // affects rally / air strike
  val Cap_LongRangeGuns      = "#32 Long Range Guns"         // affects bombard
  val Cap_MiGs               = "#33 MiGs"                    // affects NVA resoures during reset / air strike
  val Cap_SA2s               = "#34 SA-2s"                   // affects air strike degrading trail / NVA rally improving trail
  val Cap_PT76               = "#45 PT-76"                   // affects NVA attack
  val Cap_ArmoredCavalry     = "#61 Armored Cavalry"         // affects ARVN transport
  val Cap_MandateOfHeaver    = "#86 Mandate of Heaven"       // affects ARVN govern
  val Cap_BoobyTraps         = "#101 Booby Traps"            // affects ambush / sweep
  val Cap_MainForceBns       = "#104 Main Force Bns"         // affects insurgent march / VC ambush
  val Cap_Cadres             = "#116 Cadres"                 // affects VC terror and agitate / VC rally agitate


  val AllCapabilityNames = List(
    Cap_TopGun, Cap_ArcLight, Cap_Abrams, Cap_Cobras, Cap_M48Patton,
    Cap_CombActionPlatoons, Cap_CORDS, Cap_LaserGuidedBombs, Cap_SearchAndDestroy,
    Cap_AAA, Cap_LongRangeGuns, Cap_MiGs, Cap_SA2s, Cap_PT76, Cap_ArmoredCavalry,
    Cap_MandateOfHeaver, Cap_BoobyTraps, Cap_MainForceBns, Cap_Cadres
  )

  //  Records a capability that is currently in play
  case class Capability(name: String, shaded: Boolean) {
    lazy val flavor = if (shaded) "shaded" else "unshaded"
    override def toString() = s"$name ($flavor)"
  }

  def shadedCapability(name: String)   = Capability(name, true)
  def unshadedCapability(name: String) = Capability(name, false)

  val TopGun_Unshaded             = unshadedCapability(Cap_TopGun)
  val ArcLight_Unshaded           = unshadedCapability(Cap_ArcLight)
  val Abrams_Unshaded             = unshadedCapability(Cap_Abrams)
  val Cobras_Unshaded             = unshadedCapability(Cap_Cobras)
  val M48Patton_Unshaded          = unshadedCapability(Cap_M48Patton)
  val CombActionPlatoons_Unshaded = unshadedCapability(Cap_CombActionPlatoons)
  val CORDS_Unshaded              = unshadedCapability(Cap_CORDS)
  val LaserGuidedBombs_Unshaded   = unshadedCapability(Cap_LaserGuidedBombs)
  val SearchAndDestroy_Unshaded   = unshadedCapability(Cap_SearchAndDestroy)
  val AAA_Unshaded                = unshadedCapability(Cap_AAA)
  val LongRangeGuns_Unshaded      = unshadedCapability(Cap_LongRangeGuns)
  val MiGs_Unshaded               = unshadedCapability(Cap_MiGs)
  val SA2s_Unshaded               = unshadedCapability(Cap_SA2s)
  val PT76_Unshaded               = unshadedCapability(Cap_PT76)
  val ArmoredCavalry_Unshaded     = unshadedCapability(Cap_ArmoredCavalry)
  val MandateOfHeaver_Unshaded    = unshadedCapability(Cap_MandateOfHeaver)
  val BoobyTraps_Unshaded         = unshadedCapability(Cap_BoobyTraps)
  val MainForceBns_Unshaded       = unshadedCapability(Cap_MainForceBns)
  val Cadres_Unshaded             = unshadedCapability(Cap_Cadres)

  val TopGun_Shaded               = shadedCapability(Cap_TopGun)
  val ArcLight_Shaded             = shadedCapability(Cap_ArcLight)
  val Abrams_Shaded               = shadedCapability(Cap_Abrams)
  val Cobras_Shaded               = shadedCapability(Cap_Cobras)
  val M48Patton_Shaded            = shadedCapability(Cap_M48Patton)
  val CombActionPlatoons_Shaded   = shadedCapability(Cap_CombActionPlatoons)
  val CORDS_Shaded                = shadedCapability(Cap_CORDS)
  val LaserGuidedBombs_Shaded     = shadedCapability(Cap_LaserGuidedBombs)
  val SearchAndDestroy_Shaded     = shadedCapability(Cap_SearchAndDestroy)
  val AAA_Shaded                  = shadedCapability(Cap_AAA)
  val LongRangeGuns_Shaded        = shadedCapability(Cap_LongRangeGuns)
  val MiGs_Shaded                 = shadedCapability(Cap_MiGs)
  val SA2s_Shaded                 = shadedCapability(Cap_SA2s)
  val PT76_Shaded                 = shadedCapability(Cap_PT76)
  val ArmoredCavalry_Shaded       = shadedCapability(Cap_ArmoredCavalry)
  val MandateOfHeaver_Shaded      = shadedCapability(Cap_MandateOfHeaver)
  val BoobyTraps_Shaded           = shadedCapability(Cap_BoobyTraps)
  val MainForceBns_Shaded         = shadedCapability(Cap_MainForceBns)
  val Cadres_Shaded               = shadedCapability(Cap_Cadres)


  val Medevac_prefix = "#15 Medevac"

  // Momentum markers
  val Mo_WildWeasels       = "#5 Wild Weasels"              // Shaded   (affects Air Strike)
  val Mo_ADSID             = "#7 ADSID"                     // Unshaded (affects change to trail value)
  val Mo_RollingThunder    = "#10 Rolling Thunder"          // Shaded   (prohibits air strike)
  val Mo_Medevac_Unshaded  = s"$Medevac_prefix (unshaded)"  // (affects commitment phase during coup round)
  val Mo_Medevac_Shaded    = s"$Medevac_prefix (shaded)"    // (prohibits air lift)
  val Mo_BlowtorchKomer    = "#16 Blowtorch Komer"          // Unshaded (Pacify costs 1 resource per step/terror, during Support phase)
  val Mo_Claymores         = "#17 Claymores"                // Unshaded (prohibits ambush, affect guerrilla march)
  val Mo_DaNang            = "#22 Da Nang"                  // Shaded (prohibits air strike)
  val Mo_McNamaraLine      = "#38 McNamara Line"            // Single event (prohibits infiltrate, prohibits trail improvement by rally)
  val Mo_Oriskany          = "#39 Oriskany"                 // Shaded (prohibits degrade of trail) (includes air strike, coup round, NOT evnts!)
  val Mo_BombingPause      = "#41 Bombing Pause"            // Single event (prohibits air strike)
  val Mo_559TransportGrp   = "#46 559th Transport Grp"      // Unshaded (Infiltrate is max 1 space)
  val Mo_BodyCount         = "#72 Body Count"               // Unshaded (affects asasult and patrol)
  val Mo_GeneralLansdale   = "#78 General Lansdale"         // Shaded (prohibits assault)
  val Mo_TyphoonKate       = "#115 Typhoon Kate"            // Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)

  val AllMomentum = List(
    Mo_WildWeasels, Mo_ADSID, Mo_RollingThunder, Mo_Medevac_Unshaded, Mo_Medevac_Shaded,
    Mo_BlowtorchKomer, Mo_Claymores, Mo_DaNang, Mo_McNamaraLine, Mo_Oriskany,
    Mo_BombingPause, Mo_559TransportGrp, Mo_BodyCount, Mo_GeneralLansdale, Mo_TyphoonKate
  )


  trait Scenario {
    val name: String
    val cardsPerCampaign: Int
    val totalCoupCards: Int
    val pivotCardsAvailable: Set[Faction]
    val usAid: Int
    val econ: Int
    val patronage: Int
    val vcResources: Int
    val nvaResources: Int
    val arvnResources: Int
    val trail: Int
    val usPolicy: String
    val rvnLeadersInPlay: List[String]
    val outOfPlay: Pieces
    val periodCapabilities: List[Capability]
    val spaces: List[Space]

    // Override this if the scenario requires any special setup
    val additionalSetup: () => Unit = () => ()
  }

  def scenarioEntry(s: Scenario): (String, Scenario) = s.name -> s

  val scenarios = ListMap[String, Scenario](
    scenarioEntry(new ShortScenario),
    scenarioEntry(new MediumScenario)
  )
  val scenarioChoices = scenarios.toList map { case (key, scenario) => key -> scenario.name }

  // Case sensitive
  def isValidScenario(name: String) = scenarios contains name

  sealed trait CoinOp {
    val name: String
    override def toString() = name
  }
  case object  Train   extends CoinOp { val name = "Train"   }
  case object  Patrol  extends CoinOp { val name = "Patrol"  }
  case object  Sweep   extends CoinOp { val name = "Sweep"   }
  case object  Assault extends CoinOp { val name = "Assault" }

  object CoinOp {
    val ALL = List(Train, Patrol, Sweep, Assault)
  }

  sealed trait InsurgentOp {
    val name: String
    override def toString() = name
  }
  case object  Rally  extends InsurgentOp { val name = "Rally"   }
  case object  March  extends InsurgentOp { val name = "March"  }
  case object  Attack extends InsurgentOp { val name = "Attack"   }
  case object  Terror extends InsurgentOp { val name = "Terror" }

  object InsurgentOp {
    val ALL = List(Rally, March, Attack, Terror)
  }

  sealed trait Action {
    val name: String
    override def toString() = name
  }
  case object  Event         extends Action { val name = "Event"                    }
  case object  OpPlusSpecial extends Action { val name = "Op/Special Activity" }
  case object  OpOnly        extends Action { val name = "Op Only"             }
  case object  LimitedOp     extends Action { val name = "Limited Op"          }

  case class Actor(faction: Faction, action: Action)

  sealed trait SpecialActivity {
    val name: String
    override def toString() = name
  }
  case object Advise     extends SpecialActivity { val name = "Advise" }
  case object AirLift    extends SpecialActivity { val name = "Air Lift" }
  case object AirStrike  extends SpecialActivity { val name = "Air Strike" }
  case object Govern     extends SpecialActivity { val name = "Govern" }
  case object Transport  extends SpecialActivity { val name = "Transport" }
  case object Raid       extends SpecialActivity { val name = "Raid" }
  case object Infiltrate extends SpecialActivity { val name = "Infiltrate" }
  case object Bombard    extends SpecialActivity { val name = "Bombard" }
  case object Ambush     extends SpecialActivity { val name = "Ambush" }
  case object Tax        extends SpecialActivity { val name = "Tax" }
  case object Subvert    extends SpecialActivity { val name = "Subvert" }

  def typhoonKateProhibited(activities: List[SpecialActivity]): List[SpecialActivity] = {
    val Prohibited: Set[SpecialActivity] = Set(AirLift, Transport, Bombard)
    if (momentumInPlay(Mo_TyphoonKate))
      activities filter Prohibited.contains
    else
      Nil
  }
  
  def claymoresProhibited(activities: List[SpecialActivity]): List[SpecialActivity] = {
    val Prohibited: Set[SpecialActivity] = Set(Ambush)
    if (momentumInPlay(Mo_Claymores))
      activities filter Prohibited.contains
    else
      Nil
  }

  case class SequenceOfPlay(
    eligibleThisTurn:   Set[Faction] = Faction.ALL,
    actors:             List[Actor]  = Nil,
    passed:             Set[Faction] = Set.empty,
    eligibleNextTurn:   Set[Faction] = Set.empty,
    ineligibleNextTurn: Set[Faction] = Set.empty) {

    lazy val acted = (actors map (_.faction)).toSet
    lazy val ineligibleThisTurn = Faction.ALL.toSet -- eligibleThisTurn -- acted -- passed
    lazy val numEligible = eligibleThisTurn.size
    lazy val numActors   = actors.size
    lazy val numPassed   = passed.size
    lazy val exhausted   = numActors == 2 || eligibleThisTurn.isEmpty

    def availableActions: List[Action] = {
      actors match {
        case Nil        => List(Event, OpOnly, OpPlusSpecial)
        case first::Nil => first.action match {
          case OpOnly        => List(LimitedOp)
          case OpPlusSpecial => List(Event, LimitedOp)
          case Event         => List(OpPlusSpecial)
          case _ => throw new IllegalStateException("availableActions illegal first action detected")
        }
        case _ => throw new IllegalStateException("availableActions called after two actions taken")
      }
    }

    def addActor(faction: Faction, action: Action): SequenceOfPlay =
      if (actors.size < 2)
        copy(actors = actors :+ Actor(faction, action), eligibleThisTurn = eligibleThisTurn - faction)
      else
        throw new IllegalStateException("addActor(): Two actors have aready been added")

    def addPasser(faction: Faction): SequenceOfPlay =
      copy(passed = passed + faction, eligibleThisTurn = eligibleThisTurn - faction)

    def remainEligible(faction: Faction): SequenceOfPlay =
      copy(eligibleNextTurn = eligibleNextTurn + faction, ineligibleNextTurn = ineligibleNextTurn - faction)

    def remainIneligible(faction: Faction): SequenceOfPlay =
      copy(eligibleThisTurn   = eligibleThisTurn - faction,   // Ineligible now and through next turn
           ineligibleNextTurn = ineligibleNextTurn + faction,
           eligibleNextTurn   = eligibleNextTurn - faction)

    def makeIneligible(faction: Faction): SequenceOfPlay =
        copy(eligibleThisTurn = eligibleThisTurn - faction)

    // Called when two factions have acted, or all have passed
    // Adjusts eligibility for the following turn
    def adjustEligibility(): SequenceOfPlay = {
      SequenceOfPlay(eligibleThisTurn = eligibleThisTurn ++ ineligibleThisTurn ++ passed ++ eligibleNextTurn -- ineligibleNextTurn)
    }
  }

  //  In force pool, casualties, and out of play box:
  //  - all bases are non-tunnels
  //  - all guerrillas, rangers, irregulars are underground
  val ForcePool = Pieces(
    usTroops        = 40,
    irregulars_U    = 6,
    irregulars_A    = 0,
    usBases         = 6,
    arvnTroops      = 30,
    arvnPolice      = 30,
    rangers_U       = 6,
    rangers_A       = 0,
    arvnBases       = 3,
    nvaTroops       = 40,
    nvaGuerrillas_U = 20,
    nvaGuerrillas_A = 0,
    nvaBases        = 9,
    nvaTunnels      = 0,
    vcGuerrillas_U  = 30,
    vcGuerrillas_A  = 0,
    vcBases         = 9,
    vcTunnels       = 0)

  // A game segment containing a file name for the segment,
  // a short description and the log messages that were generated
  // during the game segment.
  case class GameSegment(filename: String, description: String, log: Vector[String])

  //  Note: we calculate the 'Available' pieces by starting with the Force Pool totals
  //        and subtracting pieces on the map, casualties, and out of play.
  case class GameState(
    scenarioName: String,
    humanFaction: Faction,
    cardsPerCampaign: Int,    // Including the Coup! card
    totalCoupCards: Int,   // Total number in the current scenario
    spaces: List[Space],
    arvnResources: Int                = 0,  // 0 - 75
    nvaResources: Int                 = 0,
    vcResources: Int                  = 0,
    usAid: Int                        = 0,
    patronage: Int                    = 0,
    econ: Int                         = 0,
    trail: Int                        = 0, // 0 - 4
    usPolicy: String                  = USPolicy_JFK,
    casualties: Pieces                = Pieces(),  // US pieces only
    outOfPlay: Pieces                 = Pieces(),  // US and ARVN
    pivotCardsAvailable: Set[Faction] = Set.empty,
    capabilities: List[Capability]    = Nil,
    rvnLeaders: List[String]          = List(RVN_Leader_DuongVanMinh),  // Head of list is current leader
    momentum: List[String]            = Nil,
    sequence: SequenceOfPlay          = SequenceOfPlay(),
    cardsDrawn: Int                   = 0,
    currentCard: Int                  = 0,
    onDeckCard: Option[Int]           = None,
    prevCardWasCoup: Boolean          = false,
    coupCardsPlayed: Int              = 0,  // Number of Coup cards played/ignored thus far
    turn: Int                         = 0,  // turn zero indicates the start of the game
    botDebug: Boolean                 = false,
    history: Vector[GameSegment]      = Vector.empty,
    log: Vector[String]               = Vector.empty) {  // Log of the cuurent game segment


    lazy val allPiecesOnMap = spaces.foldLeft(Pieces()) { (total, space) => total + space.pieces }
    lazy val availablePieces = ForcePool - allPiecesOnMap.normalized - casualties.normalized - outOfPlay.normalized
    // piecesToPlace are either on the map or available (not casualties or out of play)
    // Does not include US troops or bases
    lazy val piecesToPlace = (allPiecesOnMap.normalized + availablePieces).except(USTroops::USBase::Nil)
    lazy val currentRvnLeader = rvnLeaders.head
    lazy val locSpaces = spaces filter (_.isLOC)
    lazy val nonLocSpaces = spaces filterNot (_.isLOC)

    // Create a one line turn description for the current game state.
    // This is used to mark the current game segment, etc.
    // "#10 Rolling Thunder - 0 acted, US is up"
    def description: String = {
      val b = new StringBuilder
      if (cardsDrawn == 0) {
        b.append("Start of game, no cards have been drawn")
      }
      else {
        val card = deck(currentCard)
        b.append(s"$card - ")
        if (isCoupRound)
          b.append("Coup! round")
        else {
          b.append(s"${sequence.numActors} acted")
          if (sequence.numPassed > 0)
            b.append(s", ${sequence.numPassed} passed")
          nextUp map (_.name) foreach { up =>
            b.append(s", $up is up")
          }
        }
      }
      b.toString
    }

    def isHuman(faction: Faction) = faction == humanFaction
    def isBot(faction: Faction) = !isHuman(faction)

    lazy val useArvnResources = isHuman(US) || isHuman(ARVN)
    lazy val useEcon          = useArvnResources

    // Return next eligible faction or None
    def nextUp: Option[Faction] = if (sequence.numActors < 2)
      deck(currentCard).factionOrder find sequence.eligibleThisTurn
    else
      None
    def executingPivotalEvent = deck.isPivotalCard(currentCard) && sequence.numActors == 0

    def isFinalCampaign = coupCardsPlayed == (totalCoupCards - 1)
    def isCoupRound = cardsDrawn > 0 && deck(currentCard).isCoup
    def onDeckIsCoup = onDeckCard map (deck(_).isCoup) getOrElse false
    def inMonsoon = !isCoupRound && onDeckIsCoup
    def resources(faction: Faction) = faction match {
      case US   => throw new IllegalArgumentException("resources called for US faction!")
      case ARVN => arvnResources
      case NVA  => nvaResources
      case VC   => vcResources
    }
    
    // Count the total number of something in each space on the map
    def totalOnMap(numberPerSpace: Space => Int): Int =
      spaces.foldLeft(0) { (total, space) => total + numberPerSpace(space) }

    // Count the number of a type of piece that is on the map, casualties, or out or play
    def numPiecesInUse(numberPer: Pieces => Int): Int =
      totalOnMap(space => numberPer(space.pieces)) + numberPer(casualties) + numberPer(outOfPlay)

    def totalCoinControl            = totalOnMap(_.coinControlValue)
    def totalNvaControl             = totalOnMap(_.nvaControlValue)
    def availablelUSTroopsAndBases  = availablePieces.totalOf(USTroops::USBase::Nil)
    def totalSupport                = totalOnMap(_.supportValue)
    def totalOpposition             = totalOnMap(_.oppositionValue)
    def nvaBasesOnMap               = totalOnMap(_.pieces.totalNVABases)
    def vcBasesOnMap                = totalOnMap(_.pieces.totalVCBases)
    def terrorMarkersAvailable      = TerrorMarkerManifest - totalOnMap(_.terror)
    def totalLOCEcon                = 15 - totalOnMap(_.currentEconValue)

    def usPoints   = totalSupport + availablelUSTroopsAndBases
    def nvaPoints  = totalNvaControl + nvaBasesOnMap
    def arvnPoints = totalCoinControl + patronage
    def vcPoints   = totalOpposition + vcBasesOnMap
    def usScore    = usPoints   - 50
    def nvaScore   = nvaPoints  - 18
    def arvnScore  = arvnPoints - 50
    def vcScore    = vcPoints   - 35

    def numPieceAvailable(pieceType: PieceType): Int = {
      availablePieces.only(normalizedType(pieceType)).total
    }
    def isPieceAvailable(pieceType: PieceType): Boolean = numPieceAvailable(pieceType) > 0

    def hasSpace(test: (Space) => Boolean) = spaces exists test
    def getSpaces(test: (Space) => Boolean): List[Space] = spaces filter test
    def getSpace(name: String) = (spaces find (_.name == name)).get
    def withSpace[T](name: String)(fn: (Space) => T): T = fn(getSpace(name))
    def updateSpace(changed: Space): GameState =
      this.copy(spaces = changed :: (spaces filterNot (_.name == changed.name)))
  }

  var game: GameState = _           // Global variable that holds the current game state.

  def autoVictory(faction: Faction): Boolean = game.isBot(faction) &&
    (faction match {
      case US   => game.usScore   > 0
      case NVA  => game.nvaScore  > 0
      case ARVN => game.arvnScore > 0
      case VC   => game.vcScore   > 0
      })


  def scenarioSummary: Seq[String] = {
    val b = new ListBuffer[String]
    val len = Faction.maxNameLen
    b += s"Scenario: ${game.scenarioName}"
    b += separator()
    for (f <- Faction.ALL.toList.sorted)
      b += s"${padLeft(f, len)}: ${if (game.isHuman(f)) "Human" else "Bot" }"
    b.toList
  }

  // Calculate percentage chance that the next card drawn will
  // be a Coup card.
  // Returns: percentage
  //          number of cards remaining in current campaign
  def chanceOfDrawingACoupCard: (Double, Int) = {
    def countCoup(cardNum: Int) = if (deck(cardNum).isCoup) 1 else 0
    val coupCardShowing = game.isCoupRound || game.onDeckIsCoup

    if (game.cardsDrawn < 3) {
      val cardsRemaining = game.cardsPerCampaign - game.cardsDrawn
      val chance = if (coupCardShowing) 0.0 else 1.0 / cardsRemaining
      (chance, cardsRemaining)
    }
    else {
      val cardsPlayed     = game.cardsDrawn - 2
      val lastPlayedPile  = (cardsPlayed - 1) / game.cardsPerCampaign + 1
      val currentCardPile = (cardsPlayed / game.cardsPerCampaign) + 1
      val nextCardPile    = (game.cardsDrawn / game.cardsPerCampaign) + 1

      if (nextCardPile == lastPlayedPile) {
        val cardsInCurrent = game.cardsDrawn % game.cardsPerCampaign
        val cardsRemaining = game.cardsPerCampaign - cardsInCurrent
        val chance         = if (game.coupCardsPlayed >= nextCardPile || coupCardShowing)
          0.0
        else
          1.0 / cardsRemaining
        (chance, cardsRemaining)
      }
      else if (nextCardPile == currentCardPile) {
        val cardsRemaining = game.cardsPerCampaign - 2
        val chance = if (coupCardShowing) 0.0 else 1.0 / cardsRemaining
        (chance, cardsRemaining)
      }
      else if (game.onDeckCard.isEmpty)
          (1.0 / game.cardsPerCampaign, game.cardsPerCampaign)
      else {
        val cardsRemaining = game.cardsPerCampaign - 1
        val chance = if (game.onDeckIsCoup) 0.0 else 1.0 / cardsRemaining
        (chance, cardsRemaining)
      }
    }
  }

  def statusSummary: Seq[String] = {
    val b = new ListBuffer[String]
    def score(faction: Faction): String = {
      val (points, score, auto)= faction match {
      case US   => (game.usPoints,   game.usScore,   autoVictory(US))
      case NVA  => (game.nvaPoints,  game.nvaScore,  autoVictory(NVA))
      case ARVN => (game.arvnPoints, game.arvnScore, autoVictory(ARVN))
      case VC   => (game.vcPoints,   game.vcScore,   autoVictory(VC))
      }

      val auto_display = if (auto) "** Auto victory **" else ""
      f"$points%2d   ($score%3d)  $auto_display"
    }
    val coupCardChance = chanceOfDrawingACoupCard match {
      case (chance, remaining) =>  f"${chance}%.3f  (${remaining} cards remaining in campaign)"
    }

    b += "Game Summary"
    b += separator()
    b += s"Total Support + Avail US : ${score(US)}"
    b += s"Coin Control + Patronage : ${score(NVA)}"
    b += s"NVA Control + NVA Bases  : ${score(NVA)}"
    b += s"Total Opp. + VC Bases    : ${score(NVA)}"

    b += separator()
    if (game.useArvnResources)
      b += f"ARVN resources : ${game.arvnResources}%2d"
    if (game.isHuman(NVA))
      b += f"NVA resources  : ${game.nvaResources}%2d"
    val agitate = if (game.isBot(VC)) " (Agitate Total)" else ""
    b += f"VC resources   : ${game.vcResources}%2d${agitate}"
    if (game.useEcon)
      b += f"Econ           : ${game.econ}%2d"
    b += f"US Aid         : ${game.usAid}%2d"
    b += f"Patronage      : ${game.patronage}%2d"
    b += f"Trail          : ${game.trail}%2d"
    if (game.isBot(US))
      b += s"US Policy      : ${game.usPolicy}"

    if (game.cardsDrawn > 0) {
      b += separator()
      b += s"Current card   : ${deck(game.currentCard).fullString}"
      game.onDeckCard foreach { num =>
        b += s"On deck card   : ${deck(num).fullString}"
      }
    }

    b += separator()
    b += s"Campaign       : ${ordinal(game.coupCardsPlayed+1)} of ${game.totalCoupCards} ${if (game.isFinalCampaign) " -- Final campaign" else ""}"
    b += s"Cards/Campaign : ${game.cardsPerCampaign}"
    b += f"Coup chance    : $coupCardChance"

    b.toList
  }

  def availablePiecesSummary: Seq[String] = {
    val b = new ListBuffer[String]
    val avail = game.availablePieces

    def addPieces(types: Seq[PieceType]): Unit = {
      for (t <- types; name = t.genericPlural; count = avail.numOf(t))
        b += f"${name}%-15s: ${count}%2d"
    }

    b += "Available Pieces"
    b += separator()
    addPieces(USTroops::Irregulars_U::USBase::Nil)
    b += separator()
    addPieces(ARVNTroops::ARVNPolice::Rangers_U::ARVNBase::Nil)
    b += separator()
    addPieces(NVATroops::NVAGuerrillas_U::NVABase::Nil)
    b += separator()
    addPieces(VCGuerrillas_U::VCBase::Nil)
    b.toList
  }

  def casualtiesSummary: Seq[String] = {
    val b = new ListBuffer[String]

    def addPieces(types: Seq[PieceType]): Unit = {
      for (t <- types; name = t.genericPlural; count = game.casualties.numOf(t) if count > 0)
        b += f"${name}%-15s: ${count}%2d"
    }
    b += "Casualties"
    b += separator()
    if (game.casualties.total == 0)
      b += "None"
    else
      addPieces(USPieces)
    b.toList
  }

  def outOfPlaySummary: Seq[String] = {
    val b = new ListBuffer[String]

    def addPieces(types: Seq[PieceType]): Unit = {
      for (t <- types; name = t.genericPlural; count = game.outOfPlay.numOf(t) if count > 0)
        b += f"${name}%-15s: ${count}%2d"
    }
    b += "Out of Play"
    b += separator()
    if (game.outOfPlay.total == 0)
      b += "None"
    else {
      addPieces(USPieces)
      addPieces(ARVNPieces)
    }
    b.toList
  }

  def eventSummary: Seq[String] = {
    val b = new ListBuffer[String]
    val pivotal = Faction.ALL.toList.sorted map {
      faction =>
      if (game.pivotCardsAvailable(faction))
        s"$faction (available)"
      else
        s"$faction (not available)"
    }
    b += "Active Events"
    b += separator()
    wrap("Capabilities: ", game.capabilities map (_.toString)) foreach (b += _)
    wrap("Momentum    : ", game.momentum)     foreach (b += _)
    wrap("Pivotal     : ", pivotal) foreach (b += _)
    b += ""
    b.toList
  }

  def sequenceList: Seq[String] = {
    val b = new ListBuffer[String]
    if (game.cardsDrawn > 0) {
      val sequence = game.sequence
      val card = deck(game.currentCard)
      b += s"Current card  : ${card.fullString}"
      if (!card.isCoup) {
        val actors     = sequence.actors map (a => s"${a.faction} => ${a.action.name}")
        val eligible   = card.factionOrder filter sequence.eligibleThisTurn   map (_.name)
        val ineligible = card.factionOrder filter sequence.ineligibleThisTurn map (_.name)
        val passed     = card.factionOrder filter sequence.passed     map (_.name)
        wrap("Eligible      : ", eligible) foreach (b += _)
        wrap("Acted         : ", actors) foreach (b += _)
        wrap("Passed        : ", passed) foreach (b += _)
        wrap("Ineligible    : ", ineligible) foreach (b += _)
      }
    }
    b.toList
  }

  def sequenceSummary: Seq[String] = {
    val b = new ListBuffer[String]
    if (game.cardsDrawn > 0) {
      b += "Sequence of Play"
      b += separator()
      b ++= sequenceList
    }
    b.toList
  }


  def spaceSummary(name: String, prefix: String = ""): Seq[String] = {
    val b = new ListBuffer[String]
    val sp = game.getSpace(name)
    b += ""
    b += s"${prefix}${sp.name} (${sp.spaceType})"
    b += separator()
    if (sp.isLOC)
      b += s"Sabotage  : ${sp.terror}"
    else {
      b += s"Population: ${sp.population}"
      b += s"Control   : ${sp.control}"
      b += s"Support   : ${sp.support}"
      b += s"Terror    : ${sp.terror}"
    }
    wrap("Pieces    : ", sp.pieces.descriptions) foreach (b += _)
    b.toList
  }

  def spaceNames(spaces: Traversable[Space]): List[String] = (spaces map (_.name)).toList.sorted
  def spaces(names: Traversable[String]): List[Space] = (names map game.getSpace).toList.sortBy(_.name)

  // We assume that the current working directory
  // set as the installed directory and thus the game directory
  // is in ./games.  The script used to invoke the program will
  // ensure that is the case.
  val gamesDir = Pathname("./games")
  var gameName: Option[String] = None // The name of sub-directory containing the game files

  case object ExitGame    extends Exception
  case object AbortAction extends Exception
  case object Rollback    extends Exception

  def main(args: Array[String]): Unit = {
    try {
      gamesDir.mkpath()

      askWhichGame() match {
        case Some(name) =>
          // loadMostRecent(name)

        case None => // Start a new game
          println()
          val scenarioName = {
            // prompt for scenario
            val choices = scenarioChoices :+ ("quit" -> "Quit")
            askMenu(choices, "Choose a scenario:", allowAbort = false).head match {
              case "quit"   => throw ExitGame
              case scenario => scenario
            }
          }
          val scenario = scenarios(scenarioName)
          val usePeriodEvents = askYorN("\nAre you using period events? (y/n) ")
          val humanFaction = askFaction("Which faction do you wish to play:", allowAbort = false)

          // println()
          // gameName = Some(askGameName("Enter a name for your new game: "))

          game = initialGameState(scenario, humanFaction, usePeriodEvents)

          logSummary(scenarioSummary)
          log()
          scenario.additionalSetup()

          //  If VC is a Bot then we use the vcResources as the Agitate Total
          //  This is initialized by rolling a d3
          if (game.isBot(VC)) {
            val agitateTotal = d3
            log(s"\nRolling d3 to set the Agitate Total (VC resources cylinder)")
            log(separator())
            setAgitateTotal(d3)
          }


      }

      mainLoop()
    }
    catch {
      case ExitGame =>
    }
  }


  def initialGameState(scenario: Scenario, humanFaction: Faction, usePeriodCapabilities: Boolean) = {
    var spaces = DefaultSpaces
    // Apply scenario overrides to countries.
    for (sp <- scenario.spaces)
      spaces = sp :: (spaces filterNot (_.name == sp.name))

    GameState(
      scenario.name,
      humanFaction,
      scenario.cardsPerCampaign,
      scenario.totalCoupCards,
      spaces,
      scenario.arvnResources,
      scenario.nvaResources,
      scenario.vcResources,
      scenario.usAid,
      scenario.patronage,
      scenario.econ,
      scenario.trail,
      scenario.usPolicy,
      Pieces(), // casualties
      scenario.outOfPlay,
      scenario.pivotCardsAvailable,
      if (usePeriodCapabilities) scenario.periodCapabilities else Nil,
      scenario.rvnLeadersInPlay)
  }


  def saveGameState(desc: String): Unit = {
    val filename = s"save-${game.history.size}"
    game = game.copy(log = Vector.empty, history = game.history :+ GameSegment(filename, desc, game.log))
    // TODO: save JSON to disk
  }


  trait Command {
    val name: String
    val desc: String
  }

  object EndTurnCmd extends Command {
    val name = "end turn"
    val desc = "End the turn for the current event card"
  }

  object ActCmd extends Command {
    val name = "act"
    val desc = "Take an action on the current card"
  }

  object PassCmd extends Command {
    val name = "pass"
    val desc = "Pass on the current card and remain eligible"
  }

  object BotCmd extends Command {
    val name = "bot"
    val desc = s"The Bot acts on the current card"
  }

  object DrawCardCmd extends Command {
    val name = "card"
    val desc = """Draw the next event card
                 |  card #    - Enter the number of the event card
                 |  card      - You will be prompted for the card number
               """.stripMargin
    val action = (param: Option[String]) => ()
  }

  object ShowCmd extends Command {
    val name = "show"

    val desc = """Display the current game state
                 |  show scenario  - name of the current scenario
                 |  show summary   - current score, resources, etc.
                 |  show pieces    - available pieces, casualties, out of play pieces
                 |  show events    - capabilities and momentum events in play
                 |  show sequence  - current sequence of play
                 |  show all       - entire game state
                 |  show <space>   - state of a single space""".stripMargin

  }

  object HistoryCmd extends Command {
    val name = "history"
    val desc = """Display game history
                 |  history            - Shows the log from the most recent game segment
                 |  history n          - Shows the log from n the most recent game segments
                 |  history all        - Shows the entire log
                 |  history  >file     - Saves the log from the most recent game segment to file
                 |  history n >file    - Saves the log from n the most recent game segments to file
                 |  history all >file  - Saves the entire log to file""".stripMargin

  }

  object AdjustCmd extends Command {
    val name = "adjust"
    val desc = """Adjust game settings  (Minimal rule checking is applied)
                              |  adjust imperium        - Imperium track
                              |  adjust roads           - Road maintenance
                              |  adjust sea             - Toggle patrolled value of a sea
                              |  adjust prestige        - Dux prestige
                              |  adjust wealth          - Civatates wealth
                              |  adjust dux res         - Dux resources
                              |  adjust civ res         - Civatates resources
                              |  adjust saxon renown    - Saxon renown
                              |  adjust scotti renown   - Scotti renown
                              |  adjust cavalry         - Cavalry in the casualties/out of play boxes
                              |  adjust comitates       - Off board Comitates (not yet in play)
                              |  adjust refugees        - Available refugee markers
                              |  adjust capabilities    - Capabilities currently in play
                              |  adjust momentum        - Momentum events currently in play
                              |  adjust niall           - Number of Saxon raider on Niall Noigiallach card
                              |  adjust bot debug       - Toggle debug output of bot logic
                              |  adjust <space>         - Space specific settings""".stripMargin

  }

  object RollbackCmd extends Command {
    val name = "rollback"
    val desc = "Roll back to the start of any turn"
  }

  object QuitCmd extends Command {
    val name = "quit"
    val desc = "Quit the game.  All plays for the current turn will be saved."
  }

  object HelpCmd extends Command {
    val name = "help"
    val desc = "List available commands"
  }

  val CommonCmds = List(ShowCmd, HistoryCmd, RollbackCmd, AdjustCmd, HelpCmd, QuitCmd)


  def doCommonCommand(cmd: Command, param: Option[String]): Unit = {
    cmd match {
      case ShowCmd     => showCommand(param)
      case HistoryCmd  => showHistory(param)
      case RollbackCmd => rollback(param)
      case AdjustCmd   => adjustSettings(param)
      case QuitCmd     => if (askYorN("Really quit (y/n)? ")) throw ExitGame
      case HelpCmd     => // Handled in the askCommand() function
      case _           => throw new IllegalArgumentException(s"${cmd.name} is not a common command")
    }
  }



  def safeToInt(str: String): Option[Int] = try Some(str.toInt) catch { case e: NumberFormatException => None }
  // Cannot draw pivotal event cards!
  def isValidCardDraw(cardNum: Int): Boolean = deck.isValidNumber(cardNum) && !deck.isPivotalCard(cardNum)

  // Prompt the user for one or two cards if necessary.
  // Then update the game state with the new card numbers.
  def drawNextCard(param: Option[String]): Unit = {

    def paramValue(p: Option[String]): Option[Int] =
      p match {
        case Some(num) if checkCardNum(num) => Some(num.toInt)
        case _ => None
      }

    if (game.cardsDrawn == 0) {
      val card1 = paramValue(param) getOrElse askCardNumber("Enter the # of the first event card: ")
      val card2 = askCardNumber("\nEnter the # of the second event card: ")
      game = game.copy(currentCard = card1, onDeckCard = Some(card2), cardsDrawn = 2)
    }
    else {
      val nextCard = paramValue(param) getOrElse askCardNumber("Enter the # of the next event card: ")
      game = game.copy(onDeckCard      = Some(nextCard),
                       cardsDrawn      = game.cardsDrawn + 1)
    }

    log()
    log(s"Current card: ${deck(game.currentCard).fullString}")
    log(s"On deck card: ${deck(game.onDeckCard.get).fullString}")
  }


  def askCardDrawCommand(): Unit = {
    val opts = orList(DrawCardCmd.name :: "?" :: Nil)
    val promptLines = new ListBuffer[String]
    val seqList = sequenceList
    val msg = if (game.cardsDrawn == 0)
      "(First event card not yet drawn)"
    else
      "(On deck event card not yet drawn)"
    promptLines += ""
    promptLines += s">>> Turn ${game.turn}  $msg"
    if (seqList.nonEmpty) {
      promptLines += separator()
      promptLines ++= seqList
    }
    promptLines += separator()
    promptLines += s"($opts): "

    val prompt = promptLines.mkString("\n", "\n", "")

    val (cmd, param) = askCommand(prompt, DrawCardCmd :: CommonCmds)
    cmd match {
      case DrawCardCmd =>
        drawNextCard(param)

      case _ =>
        // Handle history, show, etc.
        doCommonCommand(cmd, param)
        askCardDrawCommand()
    }
  }

  // ---------------------------------------------
  // Process all top level user commands.
  @tailrec def mainLoop(): Unit = {
    game = game.copy(turn = game.turn + 1)

    try {
      askCardDrawCommand()

      if (game.isCoupRound)
        resolveCoupCard()
      else
        resolveNextActor()
    }
    catch {
      // The rollback command will have restored the game state from a previosly
      // saved turn.
      case Rollback =>
    }

    // Loop infinitely
    // We will terminate when we get an ExitGame exception (caught in main())
    mainLoop()
  }


  // Resolve the Coup phase, then reset the sequence of play and draw the next card.
  def resolveCoupCard(): Unit = {
    game = game.copy(coupCardsPlayed = game.coupCardsPlayed + 1)
    log("Epoch resolution has not been implemented.")
    // NOTE: Check game.prevCardWasCoup
    //       IF so resolve the RVN Leader and any immediate effects
    //       but do not conduct Coup round.
    //       If it is the final Coup card determine victory
    // All factions are eligible after a Coup round
    game = game.copy(sequence = SequenceOfPlay())
    
    // ....
    
    saveGameState(game.description)
    
  }

  def adjustFactionEligibility(): Unit = {
    val oldSequence = game.sequence
    val newSequence = oldSequence.adjustEligibility
    val eligible    = (newSequence.eligibleThisTurn filterNot oldSequence.eligibleThisTurn).toList.sorted
    val ineligible  = (newSequence.ineligibleThisTurn filterNot oldSequence.ineligibleThisTurn).toList.sorted

    game = game.copy(sequence = newSequence)

    log("\nAdjust eligiblity")
    log(separator())
    if (eligible.nonEmpty)
      log(s"Move the ${andList(eligible)} ${pluralize(eligible.size, "cylinder")} to the Eligible box")
    if (ineligible.nonEmpty)
      log(s"Move the ${andList(ineligible)} ${pluralize(ineligible.size, "cylinder")} to the Ineligible box")
  }

  // Resolve the action for the next eligible faction.
  // Then if two actions have occurred or if there are no more
  // eligible factions, adjust the sequence of play and draw the next card.
  def resolveNextActor(): Unit = {
    val promptLines = new ListBuffer[String]
    val next = game.nextUp map (f => s"  ($f is up next)") getOrElse ""
    promptLines += ""
    promptLines += s">>> Turn ${game.turn}${next}"
    promptLines += separator()
    promptLines ++= sequenceList
    promptLines += separator()

    if (game.sequence.exhausted) {
      val opts = orList(EndTurnCmd.name::"?"::Nil)

      promptLines += s"($opts): "

      val prompt = promptLines.mkString("\n", "\n", "")

      @tailrec def nextEndCommand(): Unit = {
        val (cmd, param) = askCommand(prompt.toString, EndTurnCmd::CommonCmds)
        cmd match {
          case EndTurnCmd =>
            adjustFactionEligibility()
            game.copy(
              prevCardWasCoup = deck(game.currentCard).isCoup,
              currentCard     = game.onDeckCard.get)

            saveGameState(game.description)
          case _ =>
            doCommonCommand(cmd, param)
            nextEndCommand()
        }
      }

      nextEndCommand()
    }
    else {
      val faction   = game.nextUp.get
      val actorCmds = if (game.isBot(faction))
        List(BotCmd)
      else if (game.executingPivotalEvent)
        List(ActCmd) // 1st Faction cannot pass on pivotal event
      else
        List(ActCmd, PassCmd)

      val opts = orList((actorCmds map (_.name)) :+ "?")

      promptLines += s"[$faction] ($opts): "

      val prompt = promptLines.mkString("\n", "\n", "")

      @tailrec def nextActorCommand(): Unit = {
        val (cmd, param) = askCommand(prompt.toString, actorCmds ::: CommonCmds)
        cmd match {
          case PassCmd =>
            factionPasses(faction)
            saveGameState(game.description)
            resolveNextActor()
          case ActCmd  =>
            val savedState = game
            try {
              Human.act()
              log(s"\nFinished with $faction turn")
              saveGameState(game.description)
            }
            catch {
              case AbortAction =>
                println("\n>>>> Aborting the current action <<<<")
                println(separator())
                displayGameStateDifferences(game, savedState)
                game = savedState
            }
            resolveNextActor()
          case BotCmd  =>
            val action = shuffle(game.sequence.availableActions).head

            game = game.copy(sequence = game.sequence.addActor(faction, action))
            log()
            log(s"Move the $faction cylinder to the $action box")

            // Bot.act()
            log(s"\nFinished with $faction turn")
            saveGameState(game.description)
            resolveNextActor()
          case _ =>
            doCommonCommand(cmd, param)
            nextActorCommand()
        }
      }

      // Ask for initial command
      nextActorCommand()
    }
  }


  // def canPlayPivotCard(faction: Faction): Boolean = {
  //   val factionCondition = faction match {
  //     case Scotti    => true
  //     case Saxon     => game.saxonRenown >= 15
  //     case Dux       => game.romanRule == false && game.prestige >= 5
  //     case Civitates => game.romanRule == false
  //   }
  //   game.sequence.eligible(faction) && factionCondition
  // }

  def ??? : Nothing = throw new Exception("Not yet implemented")


  // Check to see if any faction wishes to play their pivotal event.
  // If so, update the cards and save the game state.
  def checkForPivotalEvent(): Unit = {
    // if (game.sequence.numActors == 0 && !deck.isPivotalCard(game.currentCard)) {
    //   val trumpPriority = List(Scotti, Saxon, Dux, Civitates)
    //   // TODO: Need to implement logic to determine if Bots will play their pivotal event.
    //   val botPivoter: Option[Faction] = None
    //   // val botPivoter = trumpPriority find (f => game.isBot(f) && canPlayPivotCard(f))
    //
    //   // List of human players who can pivot if they wish.
    //   // If a bot wishes to pivot, the list will only include those
    //   // that can trump the bot.
    //   val canTrumpBot = (trumpPriority.reverse dropWhile (f => Some(f) != botPivoter)).tail.reverse
    //   val humanPivots = canTrumpBot filter (f => game.isHuman(f) && canPlayPivotCard(f))
    //   val pivoter = (botPivoter, humanPivots) match {
    //     case (bot, Nil) => bot // May be None
    //     case (Some(bot), humans) =>
    //       val choices = ("none" -> s"Allow $bot bot to play its pivotal event") ::
    //                     (humans map (f => f.name -> s"$f will trump with their pivotal event"))
    //       println(s"\nThe ${bot} bot wishes to play its pivotal event")
    //       askMenu(choices, allowAbort = false).head match {
    //         case "none" => Some(bot)
    //         case name   => Some(Faction(name))
    //       }
    //     case (None, humans) =>
    //       val choices = ("none" -> "No faction will play a pivotal event") ::
    //                     (humans map (f => f.name -> s"$f will play their pivotal event"))
    //       println(s"\nDoes any faction wish to play a pivotal event?")
    //       askMenu(choices, allowAbort = false).head match {
    //         case "none" => None
    //         case name   => Some(Faction(name))
    //       }
    //   }
    //   pivoter foreach { faction =>
    //     game = game.copy(nextCards = game.currentCard :: game.nextCards,
    //                      currentCard = faction.pivotCard)
    //     log()
    //     log(s"The $faction play their pivotal event")
    //     saveGameState(game.description)
    //   }
    // }
  }


  def factionPasses(faction: Faction): Unit = {
    game = game.copy(sequence = game.sequence.addPasser(faction))
    val amount = if (faction == US || faction == ARVN) 3 else 1
    log()
    log(s"$faction faction passes")
    log(separator())
    log(s"Move the $faction cylinder to the pass box")
    increaseResources(faction, amount)
  }

  // Will not increase resrouces if faction is NP bot.
  def increaseResources(faction: Faction, amount: Int): Unit = if (amount > 0) {
    faction match {
      case US | ARVN if game.useArvnResources =>
        game = game.copy(arvnResources = (game.arvnResources + amount) min EdgeTrackMax)
        log(s"Increase $ARVN resources by +$amount to ${game.arvnResources}")

      case NVA  if game.isHuman(NVA) =>
        game = game.copy(nvaResources = (game.nvaResources + amount) min EdgeTrackMax)
        log(s"Increase $NVA resources by +$amount to ${game.nvaResources}")

      case VC if game.isHuman(VC) =>
        game = game.copy(vcResources = (game.vcResources + amount) min EdgeTrackMax)
        log(s"Increase $VC resources by +$amount to ${game.vcResources}")

      case _ =>
    }
  }

  // Will not decrease resrouces if faction is NP bot.
  def decreaseResources(faction: Faction, amount: Int): Unit = if (amount > 0) {
    faction match {
      case US | ARVN if game.useArvnResources =>
        game = game.copy(arvnResources = (game.arvnResources - amount) max 0)
        log(s"Decrease $ARVN resources by -$amount to ${game.arvnResources}")

      case NVA if game.isHuman(NVA) =>
        game = game.copy(nvaResources = (game.nvaResources - amount) max 0)
        log(s"Decrease $NVA resources by -$amount to ${game.nvaResources}")

      case VC if game.isHuman(VC) =>
        game = game.copy(vcResources = (game.vcResources - amount) max 0)
        log(s"Decrease $VC resources by -$amount to ${game.vcResources}")

      case _ =>
    }
  }
  
  def improveTrail(num: Int): Unit = if (num > 0) {
    val newTrail = (game.trail + num) min TrailMax
    if (newTrail != game.trail) {
      game = game.copy(trail = newTrail)
      log(s"Improve the trail by ${amountOf(num, "box", Some("boxes"))} to $newTrail")
    }
  }
  
  def degradeTrail(num: Int): Unit = if (num > 0) {
    val newTrail = (game.trail - num) max TrailMin
    if (newTrail != game.trail) {
      game = game.copy(trail = newTrail)
      log(s"Degrade the trail by ${amountOf(num, "box", Some("boxes"))} to $newTrail")
    }
  }

  def addTerror(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    assert(game.terrorMarkersAvailable >= num, s"addTerror($name): not enough available markers")
    game = game.updateSpace(sp.copy(terror = sp.terror + num))
    if (sp.isLOC)
      log(s"Add ${amountOf(num, "sabotage marker")} to $name")
    else
      log(s"Add ${amountOf(num, "terror marker")} to $name")
  }

  def removeTerror(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    assert(sp.terror >= num, s"removeTerror($name): not enough markers in space")
    game = game.updateSpace(sp.copy(terror = sp.terror - num))
    if (sp.isLOC)
      log(s"Remove ${amountOf(num, "sabotage marker")} from $name")
    else
      log(s"Remove ${amountOf(num, "terror marker")} from $name")
  }

  def increasePatronage(amount: Int): Unit = if (amount > 0) {
    loggingPointsChanges {
      game = game.copy(patronage = (game.patronage + amount) min EdgeTrackMax)
      log(s"Increase Patronage by +$amount to ${game.patronage}")
    }
  }

  def decreasePatronage(amount: Int): Unit = if (amount > 0) {
    loggingPointsChanges {
      game = game.copy(patronage = (game.patronage - amount) max 0)
      log(s"Decrease Patronage by -$amount to ${game.patronage}")
    }
  }

  def increaseUsAid(amount: Int): Unit = if (amount > 0) {
    game = game.copy(usAid = (game.usAid + amount) min EdgeTrackMax)
    log(s"Increase US Aid by +$amount to ${game.usAid}")
  }

  def decreaseUsAid(amount: Int): Unit = if (amount > 0) {
    game = game.copy(usAid = (game.usAid - amount) max 0)
    log(s"Decrease US Aid by -$amount to ${game.usAid}")
  }

  // If VC is a Bot, the we store the agitate total in vcResources
  def setAgitateTotal(amount: Int): Unit = if (amount > 0) {
    game = game.copy(vcResources = amount)
    log(s"Set Agitate Total to ${game.vcResources}")
  }

  // If VC is a Bot, the we store the agitate total in vcResources
  def increaseAgitateTotal(amount: Int): Unit = if (amount > 0) {
    game = game.copy(vcResources = (game.vcResources + amount) min EdgeTrackMax)
    log(s"Increase Agitate Total by +$amount to ${game.vcResources}")
  }

  // If VC is a Bot, the we store the agitate total in vcResources
  def decreaseAgitateTotal(faction: Faction, amount: Int): Unit = if (amount > 0) {
    game = game.copy(vcResources = (game.vcResources - amount) max 0)
    log(s"Decrease Agitate Total by -$amount to ${game.vcResources}")
  }


  def increaseSupport(name: String, num: Int): Unit = if (num > 0) {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      val newSupport = try SupportType(sp.support.value + num)
      catch {
        case _: IllegalArgumentException =>
          throw new IllegalStateException(s"Cannot increase support from ${sp.support} by $num levels")
      }

      val updated = sp.copy(support = newSupport)
      game = game.updateSpace(updated)
      logSupportChange(sp, updated)
    }
  }

  def decreaseSupport(name: String, num: Int): Unit = if (num > 0) {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      val newSupport = try SupportType(sp.support.value - num)
      catch {
        case _: IllegalArgumentException =>
          throw new IllegalStateException(s"Cannot decrease support from ${sp.support} by $num levels")
      }

      val updated = sp.copy(support = newSupport)
      game = game.updateSpace(updated)
      logSupportChange(sp, updated)
    }
  }

  def capabilityInPlay(cap: Capability) = game.capabilities contains cap

  def playCapability(cap: Capability): Unit = {
    game = game.copy(capabilities = cap :: game.capabilities)
    log(s"Capability is now in play: $cap")
  }

  def removeCapabilityFromPlay(cap: Capability): Unit = {
    if (game.capabilities contains cap) {
      game = game.copy(capabilities = game.capabilities filterNot (_ == cap))
      log(s"Remove capability '$cap' from play")
    }
  }

  def momentumInPlay(mo: String) = game.momentum contains mo

  def playMomentum(mo: String): Unit = {
    game = game.copy(momentum = mo :: game.momentum)
    log(s"Momentum event is now in play: $mo")
  }

  // Place pieces from the AVAILABLE box in the given map space.
  // There must be enough pieces in the available box or an exception is thrown.
  def placePieces(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.availablePieces contains pieces, "Insufficent pieces in the available box")

    loggingControlChanges {
      val sp      = game.getSpace(spaceName)

      assert(pieces.totalBases + sp.pieces.totalBases <= 2, s"Cannot place more than 2 bases in $spaceName")
      game = game.updateSpace(sp.copy(pieces = sp.pieces + pieces))
      log(s"\nPlace the following pieces from AVAILABLE into $spaceName:")
      wrap("  ", pieces.descriptions) foreach (log(_))
    }
  }

  def removeToAvailable(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp = game.getSpace(spaceName)
      assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
      val updated = sp.copy(pieces = sp.pieces - pieces)
      game = game.updateSpace(updated)
      log(s"\nRemove the following pieces from $spaceName to AVAILABLE:")
      wrap("  ", pieces.descriptions) foreach (log(_))
    }
  }

  def removeToCasualties(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp = game.getSpace(spaceName)
      assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
      val updated = sp.copy(pieces = sp.pieces - pieces)
      // Pieces in casualties are always normalized.
      game = game.updateSpace(updated).copy(casualties = game.casualties + pieces.normalized)
      log(s"\nRemove the following pieces from $spaceName to CASUALTIES:")
      wrap("  ", pieces.descriptions) foreach (log(_))
    }
  }

  // Remove pieces from the given space.
  // US to casualties, all other to available
  
  def removePieces(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val deadUS    = pieces.only(USPieces)
      val deadOther = pieces.except(USPieces)
    
      removeToCasualties(spaceName, deadUS)
      removeToAvailable(spaceName, deadOther)
    }
  }

  //  Reveal guerrillas/rangers/irregulars in a space
  def revealPieces(spaceName: String, hidden: Pieces): Unit = if (hidden.total > 0) {
    val Valid = List(Irregulars_U, Rangers_U, NVAGuerrillas_U, VCGuerrillas_U)
    val sp = game.getSpace(spaceName)
    assert(hidden.only(Valid) == hidden, s"revealPieces() called with non-undeground pieces: $hidden")
    assert(sp.pieces contains hidden, s"revealPieces() $spaceName does not contain all requested pieces: $hidden")
    
    val visible = Pieces(
      irregulars_A    = hidden.irregulars_U,
      rangers_A       = hidden.rangers_U,
      nvaGuerrillas_A = hidden.nvaGuerrillas_U,
      vcGuerrillas_A  = hidden.vcGuerrillas_U)
    val updated = sp.copy(pieces = sp.pieces - hidden + visible)
    game = game.updateSpace(updated)
    
    if (visible.total == 1)
      log(s"\nIn $spaceName, flip the following piece to its active side")
    else
      log(s"\nIn $spaceName, flip the following pieces to their active sides")
    wrap("  ", hidden.descriptions) foreach (log(_))
  }
  
  //  Hide guerrillas/rangers/irregulars in a space
  def hidePieces(spaceName: String, visible: Pieces): Unit = if (visible.total > 0) {
    val Valid = List(Irregulars_A, Rangers_A, NVAGuerrillas_A, VCGuerrillas_A)
    val sp = game.getSpace(spaceName)
    assert(visible.only(Valid) != visible, s"hidePieces() called with non-active pieces: $visible")
    assert(sp.pieces contains visible, s"hidePieces() $spaceName does not contain all requested pieces: $visible")
    
    val hidden = Pieces(
      irregulars_A    = visible.irregulars_U,
      rangers_A       = visible.rangers_U,
      nvaGuerrillas_A = visible.nvaGuerrillas_U,
      vcGuerrillas_A  = visible.vcGuerrillas_U)
    val updated = sp.copy(pieces = sp.pieces - visible + hidden)
    game = game.updateSpace(updated)
    
    if (visible.total == 1)
      log(s"\nIn $spaceName, flip the following piece to its underground side")
    else
      log(s"\nIn $spaceName, flip the following pieces to their underground sides")
    wrap("  ", visible.descriptions) foreach (log(_))
  }
  
  
  
  // Move the given pieces from the source space to the destination space
  // and log the activity.
  def movePieces(pieces: Pieces, source: String, dest: String): Unit = {
    val srcSpace = game.getSpace(source)
    val dstSpace = game.getSpace(dest)
    assert(srcSpace.pieces contains pieces, s"$source does not contain all requested pieces: $pieces")
    
    val updatedSrc = srcSpace.copy(pieces = srcSpace.pieces - pieces)
    val updatedDst = dstSpace.copy(pieces = dstSpace.pieces + pieces)
    loggingControlChanges {
      game = game.updateSpace(updatedSrc).updateSpace(updatedDst)
      log(s"\nMove the following pieces from $source to $dest:")
      log(separator())
      wrap("", pieces.descriptions) foreach (log(_))
    }
  }

  def addTunnelMarker(spaceName: String, base: PieceType): Unit = {
    val sp = game.getSpace(spaceName)
    assert(base == VCBase || base == NVABase, s"addTunnelMarker() called with invalid type: $base")
    assert(sp.pieces.has(base), s"addTunnelMarker() $spaceName does not contain a $base")
    val tunnel = if (base == VCBase) VCTunnel else NVATunnel
    val updated = sp.copy(pieces = sp.pieces.remove(1, base).add(1, tunnel))
    game = game.updateSpace(updated)
    log(s"\nAdd a tunnel marker to a $base in $spaceName")
  }

  def removeTunnelMarker(spaceName: String, tunnel: PieceType): Unit = {
    val sp = game.getSpace(spaceName)
    assert(tunnel == VCTunnel || tunnel == NVATunnel, s"removeTunnelMarker() called with invalid type: $tunnel")
    assert(sp.pieces.has(tunnel), s"removeTunnelMarker() $spaceName does not contain a $tunnel")
    val base = if (tunnel == VCTunnel) VCBase else NVABase
    val updated = sp.copy(pieces = sp.pieces.remove(1, tunnel).add(1, base))
    game = game.updateSpace(updated)
    log(s"\nRemove a tunnel marker from a $base in $spaceName")
  }


  // Return list of spaces that have COIN pieces that can be reached
  // from the given ambush space.
  // Returns empty if none found
  def ambushTargets(name: String): List[String] = {
    val ambushSpace = game.getSpace(name)
    
    val inPlace = if (ambushSpace.pieces.has(CoinPieces)) Some(name) else None
    val adjacent = if (ambushSpace.isLOC)
      getAdjacent(name).toList filter (adj => game.getSpace(adj).pieces.has(CoinPieces))
    else
      Nil
    
    (inPlace.toList ::: adjacent).sorted
  }

  // Ask the user to select a number of pieces.
  // The type of pieces allowed for selection may be limited by passing a list of those
  // that are allowed.  An empty list indicates that all types of pieces may be selected.
  def askPieces(pieces: Pieces, num: Int, allowed: Seq[PieceType] = AllPieceTypes,
                prompt: Option[String] = None, allowAbort: Boolean = true): Pieces = {
    val pieceTypes = allowed filter pieces.has
    var selected   = Pieces()
    val numPieces  = num min pieces.totalOf(pieceTypes)
    if (numPieces > 0) {
      if (pieceTypes.size == 1)
        selected = selected.set(numPieces, pieceTypes.head)
      else {
        val available = pieces.only(pieceTypes)
        if (numPieces == available.total)
          selected = available
        else {
          println()
          prompt foreach println
          println(s"Select ${amountOf(numPieces, "piece")} among the following:")
          wrap("  ", available.descriptions) foreach println
          println()

          def nextType(types: Seq[PieceType]): Unit = {
            val numRemaining = numPieces - selected.total
            if (numRemaining != 0) {
              // If we have to include all remainig pieces, don't bother asking
              if (pieces.totalOf(types) == numRemaining) {
                for (pieceType <- types)
                  selected = selected.add(pieces.numOf(pieceType), pieceType)
              }
              else {
                val (pieceType :: rest) = types
                val totalOfRest = pieces.totalOf(rest)
                val minimum = if (totalOfRest < numRemaining) numRemaining - totalOfRest else 0
                val maximum = numRemaining min pieces.numOf(pieceType)
                val n = askInt(s"How many ${pieceType}", minimum, maximum, allowAbort = allowAbort)
                selected = selected.add(n, pieceType)
                nextType(rest)
              }
            }
          }
          nextType(pieceTypes)
        }
      }
    }
    selected
  }

  // Ask the user to select enemy coin pieces to remove.
  // This function ensures that bases are not selected if there
  // are any other coin pieces present.
  def askEnemyCoin(pieces: Pieces, num: Int, prompt: Option[String] = None, allowAbort: Boolean = true): Pieces = {
    val forces = pieces.only(CoinForces)
    val bases  = pieces.only(CoinBases)
    
    val deadForces = askPieces(forces, num min forces.total, prompt = prompt, allowAbort = allowAbort)
    val deadBases  = if (deadForces.total < num && (forces - deadForces).isEmpty && bases.nonEmpty)
      askPieces(bases, num - deadForces.total, prompt = prompt, allowAbort = allowAbort)
    else
      Pieces()
    
    deadForces + deadBases
  }

  // Ask the user to remove the given number of pieces of the requested type from the map.
  def voluntaryRemoval(num: Int, pieceType: PieceType, prohibitedSpaces: Set[String] = Set.empty): Unit = if (num > 0) {
    val types = simiarTypes(pieceType)  // Account for Active/Underground if necessary
    val candidateNames = spaceNames(game.spaces filterNot (sp => prohibitedSpaces(sp.name)) filter (_.pieces.has(types)))
    def availPieces(names: List[String]) = names.foldLeft(0)((sum, n) => sum + game.getSpace(n).pieces.totalOf(types))
    assert(availPieces(candidateNames) >= num, "voluntaryRemoval: Not enough pieces on map!")

    def nextSpace(removed: Vector[(String, Pieces)], candidates: List[String]): Vector[(String, Pieces)] = {
      val removedSoFar = removed.foldLeft(0) { case (sum, (_, n)) => sum + n.total }
      val numLeft      = num - removedSoFar
      val avail        = availPieces(candidates)
      if (numLeft == 0)
        removed
      else if (avail == numLeft) {
        // Remove all remaining pieces
        removed ++ (candidates map (n => (n -> game.getSpace(n).pieces.only(types))))
      }
      else {
        val name = askCandidate(s"\nSelect space to remove ${pieceType.genericPlural}: ", candidates)
        val sp = game.getSpace(name)
        val pieces = sp.pieces.only(types)
        val numInSpace = pieces.total min numLeft
        val minFromSpace = 1 max (numLeft - (avail - numInSpace))
        val num = if (minFromSpace == numInSpace) numInSpace
                 else askInt(s"Remove how many ${pieceType.genericPlural}", minFromSpace, numInSpace)
        val toRemove = askPieces(pieces, num, types)

        nextSpace(removed :+ (name -> toRemove), candidates filterNot (_ == name))
      }
    }

    val removed = nextSpace(Vector.empty, candidateNames)

    loggingControlChanges {
      log()
      for ((name, removedPieces) <- removed) {
        val sp = game.getSpace(name)
        val updated = sp.copy(pieces = sp.pieces - removedPieces)
        game = game.updateSpace(updated)
        log(s"Remove ${andList(removedPieces.descriptions)} from $name to AVAILABLE")
      }
    }
  }

  // Ask the number of each type of pieces to place in the given space up to the
  // given maximum.
  // If there are not sufficient pieces in the available box, then the user is
  // asked to remove some from the map.
  // If there are not enough in available or in other spaces on the map, then the
  // piece type is skipped.
  // IMPORTANT:
  //  -  Assumes that Active pieces are never included in the list of types!!
  //  -  Do not use this function for USTroops
  //  -  See askToPlaceBase() for placing bases
  def askPiecesToPlace(spaceName: String, types: List[PieceType], maxToPlace: Int): Pieces = {
    // Get number of each type in available box plus on map
    val piecesToPlace = game.piecesToPlace.only(types)
    val maxPieces     = maxToPlace min piecesToPlace.total
    val availMap      = (types map (t => t -> piecesToPlace.total)).toMap
    val availTypes    = types filter (availMap(_) > 0)
    if (availTypes.isEmpty) {
      println(s"\nThere are no ${orList(types)} available to be placed.")
      Pieces()
    }
    else {
      availTypes match {
        case pieceType::Nil => println(s"\nPlace up to ${amountOf(maxPieces, pieceType.singular)} in $spaceName")
        case _              => println(s"\nPlace up to ${amountOf(maxPieces, "piece")} in $spaceName (${andList(availTypes)})")
      }

      def nextType(placed: Pieces, remainingTypes: List[PieceType]): Pieces = {
        val maxRemaining = maxPieces - placed.total
        if (maxRemaining == 0 || remainingTypes.isEmpty)
          placed
        else {
          val pieceType = remainingTypes.head
          val maxOfType = availMap(pieceType) min maxRemaining
          if (maxOfType == 0) {
            println(s"\nThere are no ${pieceType.genericPlural} available to be placed.")
            nextType(placed, remainingTypes.tail)
          }
          else {
            val num = askInt(s"\nPlace how many ${pieceType.genericPlural}? ", 0, maxOfType)
            val numAvail = game.availablePieces.numOf(pieceType)
            val finalNum = if (num <= numAvail)
               num
            else {
              // Ask if they want to voluntarily remove pieces to make up the difference.
              numAvail match {
                case 0 => println(s"\nThere are no ${pieceType.genericPlural} in the available box")
                case 1 => println(s"\nThere is only 1 ${pieceType.genericSingular} in the available box")
                case n => println(s"\nThere are only ${amountOf(n, pieceType.genericSingular)} in the available box")
              }
              println
              if (askYorN("Do you wish to voluntarily remove pieces to make up the difference? (y/n) ")) {
                val numToRemove = askInt("How many pieces do you wish to remove from the map", 0, num - numAvail)
                voluntaryRemoval(numToRemove, pieceType)
                numAvail + numToRemove
              }
              else
                numAvail
            }
            nextType(placed.add(finalNum, pieceType), remainingTypes.tail)
          }
        }
      }

      nextType(Pieces(), availTypes)
    }
  }

  // If there are not sufficient bases in the available box, then the user is
  // asked to remove one from the map.
  // IMPORTANT:
  //  -  Do not use this function for  USBase
  //  -  Do not call with Tunneled base types!
  def askToPlaceBase(spaceName: String, baseType: PieceType): Pieces = {
    // Get number of each type in available box plus on map
    val piecesToPlace = game.piecesToPlace.only(baseType)
    if (piecesToPlace.isEmpty) {
      println(s"\nThere are no ${baseType.plural} available to be placed.")
      Pieces()
    }
    else {
      // If there is one available, then no need to ask.
      if (game.availablePieces.has(baseType))
        Pieces().set(1, baseType)
      else {
        // None available, so ask where to remove one voluntarily
        println(s"\nThere are no ${baseType.genericPlural} in the available box")
        if (askYorN("Do you wish to voluntarily remove a base from the map? (y/n) ")) {
          voluntaryRemoval(1, baseType)
          Pieces().set(1, baseType)
        }
        else
          Pieces()
      }
    }
  }

  def logSupportChange(orig: Space, updated: Space): Unit = {
    assert(orig.name == updated.name, "logSupportChange: not the same space!")
    if (orig.support != updated.support) {
      val name = orig.name
      (orig.support, updated.support) match {
          case (s, Neutral) => log(s"Remove $s marker from $name")
          case (Neutral, s) => log(s"Place $s marker in $name")
          case (old, s)     => log(s"Replace $old marker in $name with $s marker")
      }
    }
  }


  def logPointsChanges(origGame: GameState, newGame: GameState): Unit = {
    var loggedHeader = false;
    def logChange(message: String): Unit = {
      if (!loggedHeader) {
        log("\nScore Marker changes")
        log(separator())
        loggedHeader = true
      }
      log(message)
    }

    if (origGame.usPoints != newGame.usPoints)
      logChange(s"Move the 'Support + Avail US' marker to ${newGame.usPoints}")

    if (origGame.arvnPoints != newGame.arvnPoints)
      logChange(s"Move the 'COIN Control + Patronage' marker to ${newGame.arvnPoints}")

    if (origGame.nvaPoints != newGame.nvaPoints)
      logChange(s"Move the 'NVA Control + NVA Bases' marker to ${newGame.nvaPoints}")

    if (origGame.vcPoints != newGame.vcPoints)
      logChange(s"Move the 'Total Opposition + VC Bases' marker to ${newGame.vcPoints}")
  }

  private var loggingPointsChangesActive = false
  def loggingPointsChanges[T](code: => T): T = {
    if (loggingPointsChangesActive)
      code
    else {
      loggingPointsChangesActive = true
      try {
        val savedGame = game
        val result    = code

        logPointsChanges(savedGame, game)
        result
      }
      finally {
        loggingPointsChangesActive = false
      }
    }
  }

  // Performs the given code, then logs all spaces that have changed control
  // and logs the updates to edge track markers that are base on control changes.
  // Nested calls to this funciton will not perform any logging.  The outermost
  // call, only, will do the logging.
  private var loggingControlChangesActive = false

  def loggingControlChanges[T](code: => T): T = {
    if (loggingControlChangesActive)
      code
    else {
      loggingControlChangesActive = true
      try {
        loggingPointsChanges {
          val savedGame  = game
          val result     = code

          val changed = for {
            space <- game.spaces.sortBy(_.name)
            orig = savedGame.getSpace(space.name)
            if !orig.isLOC && orig.control != space.control
          } yield (space.name, orig.control, space.control)

          if (changed.nonEmpty) {
            log(s"\nControl changes")
            log(separator())
            for ((name, origControl, newControl) <- changed) {
              (origControl, newControl) match {
                case (Uncontrolled, _)  => log(s"Place ${newControl} marker in ${name}")
                case (_, Uncontrolled)  => log(s"Remove ${origControl} marker from ${name}")
                case _                  => log(s"Flip control marker in ${name} to ${newControl}")
              }
            }
          }
          result
        }
      }
      finally {
        loggingControlChangesActive = false
      }
    }
  }



  // Returns comma separated string with last choice separated by "and"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples and oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges and grapes"
  def andList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} and ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + ", and " + x.last.toString
  }

  // Returns comma separated string with last choice separated by "or"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples or oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges or grapes"
  def orList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} or ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + ", or " + x.last.toString
  }

  def pluralize(num: Int, name: String, optPlural: Option[String] = None): String = {
    val plural = optPlural getOrElse { if (name endsWith "s") s"${name}es" else s"${name}s" }
    num match {
      case 1 => name
      case _ => plural
    }
  }

  def pluralizeD(num: Double, name: String, optPlural: Option[String] = None): String = {
    val plural = optPlural getOrElse { if (name endsWith "s") s"${name}es" else s"${name}s" }
    num match {
      case x if x > 0.0 && x <= 1.0 => name
      case _                        => plural
    }
  }

  def inspect[T](name: String, value: T): T = {
    val str = if (value == null) "NULL" else value.toString
    println(s"DEBUG: $name == ${str}")
    value
  }

  // Format the given sequence of strings in a comma separated list
  // such that we do not exceed the given number of columns.
  def wrap(prefix: String, values: Seq[String], columns: Int = 78): Seq[String] = {
    val b = new ListBuffer[String]
    val s = new StringBuilder(prefix)
    var first = true
    if (values.isEmpty)
      s.append("none")
    else {
      val margin = " " * prefix.length
      s.append(values.head)
      for (v <- values.tail) {
        s.append(", ")
        if (s.length + v.length < columns)
          s.append(v)
        else {
          b += s.toString
          s.clear
          s.append(margin).append(v)
        }
      }
    }
    b += s.toString
    b.toList
  }

  def pause() {
    import scala.util.Properties.isWin
    if (isWin)
      readLine("Press Enter to continue... ")
    else
      readLine("Continue ↩︎ ")
  }

  var echoLogging = true
  // Print the line to the console and save it in the game's history.
  def log(line: String = "", echo: Boolean = true): Unit = {
    if (echo && echoLogging)
      println(line)
    game = game.copy(log = game.log :+ line)
  }

  def separator(length: Int = 52, char: Char = '-'): String = char.toString * length


  def printSummary(summary: Seq[String]): Unit = if (summary.nonEmpty) {
    println()
    summary foreach println
  }

  def logSummary(summary: Seq[String]): Unit = {
    log()
    summary foreach (log(_))
  }

  def padLeft(x: Any, width: Int) = "%%-%ds".format(width).format(x.toString)

  // Get ordinal number.  Good for 1 to 20.
  def ordinal(i: Int): String = i match {
    case 1 => "1st"
    case 2 => "2nd"
    case 3 => "3rd"
    case x if x > 20 => throw new IllegalArgumentException("ordinal() only good for numbers <= 20")
    case x => s"${x}th"
  }


  // If num is 1 use the name as is
  // otherwise either use the plural if given or add an 's' to the name.
  def amountOf(num: Int, name: String, plural: Option[String] = None) = s"$num ${pluralize(num, name, plural)}"
  def amtPiece(num: Int, pieceType: PieceType) = amountOf(num, pieceType.singular, Some(pieceType.plural))

  def displayGameStateDifferences(from: GameState, to: GameState): Unit = {
    println("\ndisplayGameStateDifferences() not implemented")
  }

  // Find a match for the given string in the list of options.
  // Any unique prefix of the given options will succeed.
  def matchOne(input: String, options: Seq[String], allowAbort: Boolean = false): Option[String] = {
    if (input == "?") {
      println(s"Enter one of:\n${orList(options)}")
      None
    }
    else {
      val lowerInput   = input.toLowerCase
      val pairdOptions = options.distinct map (o => o -> o.toLowerCase)
      val matches      = pairdOptions filter { case (_, lower) => lower startsWith lowerInput}
      matches match {
        case Seq() =>
          println(s"'$input' is not valid. Must be one of:\n${orList(options)}")
          None
        case Seq((value, _))  =>
          Some(value)

        case multiple =>
          // See if the prefix happens to be an exact match for one option
          val single = multiple find { case (value, lower) => lower == lowerInput }
          single match {
            case Some((value, _)) => Some(value)
            case None =>
              val choices = (multiple.toList map (x => Some(x._1) -> x._1)) :+ (None -> "None of the above")
              val prompt = s"\n'$input' is ambiguous.  Choose one:"
              askMenu(choices, prompt, allowAbort = allowAbort).head
          }
      }
    }
  }

  def askOneOf(prompt: String,
               options: Seq[Any],
               initial: Option[String] = None,
               allowNone: Boolean = false,
               allowAbort: Boolean = true): Option[String] = {
    val AbortActionString = "abort"
    val choices = if (allowAbort) options ++ List(AbortActionString) else options
    
    def testResponse(response: Option[String]): Option[String] = {
      response flatMap (s => matchOne(s.trim, choices map (_.toString))) match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | ""              => testResponse(None)
            case input                  => testResponse(Some(input))
          }
        case Some(AbortActionString) if allowAbort =>
          if (askYorN("Really abort (y/n)? ")) throw AbortAction else testResponse(None)
        case s => s
      }
    }
    testResponse(initial)
  }

  def askYorN(prompt: String): Boolean = {
    def testResponse(r: String): Option[Boolean] = {
      if (r == null)
        None
      else
        r.trim.toLowerCase match {
          case "n" | "no"  => Some(false)
          case "y" | "yes" => Some(true)
          case _           => None
        }
    }

    testResponse(readLine(prompt)) match {
      case Some(result) => result
      case None         => askYorN(prompt)
    }
  }

  def askInt(prompt: String, low: Int, high: Int, default: Option[Int] = None, allowAbort: Boolean = true): Int = {
    assert(low <= high, "askInt() low cannot be greater than high")
    if (low == high) {
      println(s"$prompt: $low")
      low
    }
    else {
      val choices = (low to high).toList
      default match {
        case Some(d) =>
          val p = "%s (%d - %d) Default = %d: ".format(prompt, choices.head, choices.last, d)
          askOneOf(p, choices, allowNone = true, allowAbort = allowAbort) map (_.toInt) match {
            case None    => d
            case Some(x) => x
          }
        case None =>
          val p = "%s (%d - %d): ".format(prompt, choices.head, choices.last)
          (askOneOf(p, choices, None, allowAbort = allowAbort) map (_.toInt)).get
      }
    }
  }

  // Convenience method for createing choices for the askMenu() function.
  def choice[T](condition: Boolean, value: T, desc: String): Option[(T, String)] =
    if (condition) Some(value -> desc) else None

  def askSimpleMenu[T](items: List[T],
                       prompt: String = "",
                       numChoices: Int = 1,
                       repeatsOK: Boolean = false,
                       allowAbort: Boolean = true): List[T] = {
    askMenu(items map (i => i -> i.toString), prompt, numChoices, repeatsOK, allowAbort)
  }

  // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenu[T](items: List[(T, String)],
                 menuPrompt: String = "",
                 numChoices: Int = 1,
                 repeatsOK: Boolean = false,
                 allowAbort: Boolean = true): List[T] = {
    def nextChoice(num: Int, itemsRemaining: ListMap[T, String]): List[T] = {
      if (itemsRemaining.isEmpty || num > numChoices)
        Nil
      else if (itemsRemaining.size == 1)
        itemsRemaining.keys.head :: Nil
      else {
        val width = itemsRemaining.size.toString.size
        println(menuPrompt)
        println(separator(char = '='))
        val indexMap = (itemsRemaining.keys.zipWithIndex map (_.swap)).toMap
        for ((key, i) <- itemsRemaining.keysIterator.zipWithIndex) {
          val prefix = String.format(s"%${width}d) ", new Integer(i+1))
          println(s"${prefix}${itemsRemaining(key)}")
        }
        val prompt = if (numChoices > 1) s"${ordinal(num)} Selection: "
        else "Selection: "
        println(separator())
        val choice = askOneOf(prompt, 1 to itemsRemaining.size, allowAbort = allowAbort).get.toInt
        val index  = choice - 1
        val key    = indexMap(index)
        val remain = if (repeatsOK) itemsRemaining else itemsRemaining - key
        indexMap(index) :: nextChoice(num + 1, remain)
      }
    }
    nextChoice(1, ListMap(items:_*))
  }

  def askCandidate(prompt: String, candidates: Seq[String], allowAbort: Boolean = true): String = {
    assert(candidates.nonEmpty, s"askCandidate(): list of candidates cannot be empty")
    // If only one candidate then don't bother to ask
    if (candidates.size == 1) {
      println(s"$prompt ${candidates.head}")
      candidates.head
    }
    else
      askOneOf(prompt, candidates, allowAbort = allowAbort).get
  }
  
  //  Returns None if the user enters an empty response
  def askCandidateOrBlank(prompt: String, candidates: Seq[String], allowAbort: Boolean = true): Option[String] = {
    assert(candidates.nonEmpty, s"askCandidateOrBlank(): list of candidates cannot be empty")
    // If only one candidate then don't bother to ask
    if (candidates.size == 1) {
      println(s"$prompt ${candidates.head}")
      candidates.headOption
    }
    else
      askOneOf(prompt, candidates, allowAbort = allowAbort, allowNone = true)
  }
    
  // Check card number input and print a message
  // if the number is not valid
  def checkCardNum(cardNum: String): Boolean = {
    cardNum match {
      case INTEGER(num) if isValidCardDraw(num.toInt) => true
      case INTEGER(num) if deck.isPivotalCard(num.toInt) =>
        println(s"'${deck(num.toInt)}' is a pivotal event card")
        false
      case input =>
        println(s"'$input' is not a valid card number")
        false
    }
  }

  def askCardNumber(prompt: String): Int = {
    readLine(prompt).trim match {
      case null | ""                 => askCardNumber(prompt)
      case num if !checkCardNum(num) => askCardNumber(prompt)
      case num                       => num.toInt
    }
  }


  // TODO: Implement askWhichGame()
  def askWhichGame(): Option[String] = None


  def askFaction(prompt: String, factions: Set[Faction] = Faction.ALL, allowAbort: Boolean = true): Faction = {
      assert(factions.nonEmpty, "askFaction called with empty set")
      println()
      askSimpleMenu(factions.toList.sorted, prompt, allowAbort = allowAbort).head
  }


  // Returns the selected command and any extra user input that was given.
  @tailrec def askCommand(prompt: String, cmds: Seq[Command]): (Command, Option[String]) = {
    readLine(prompt) match {
      case null =>  // EOF, user pressed ctrl-d (ctrl-Z on Windoze)
        println()
        askCommand(prompt, cmds)
      case input =>
        val tokens = input.split("\\s+").toList.dropWhile(_ == "")
        tokens match {
          case Nil => askCommand(prompt, cmds)  // User did not enter a command
          case verb :: rest =>
            val param = if (rest.nonEmpty) Some(rest.mkString(" ")) else None
            matchOne(verb, cmds map (_.name)) match {
              case None => askCommand(prompt, cmds)  // Invalid command entered
              case Some(name) =>
                cmds find (_.name == name) match {
                  case None => throw new IllegalStateException(s"Internal error: Command '$name' is not valid")
                  case Some(cmd) if cmd == HelpCmd =>
                    showHelp(cmds, param)
                    askCommand(prompt, cmds)
                  case Some(cmd) => (cmd, param)
                }
            }
        }
    }
  }

  def showHelp(cmds: Seq[Command], param: Option[String]): Unit = {
    val names = cmds map (_.name)
    param match {
      case None =>
        println("Available commands: (type help <command> for more detail)")
        println(orList(names))
      case Some(p) =>
        for (name <- matchOne(p, names); cmd <- cmds find (_.name == name))
          println(cmd.desc)
    }
  }


  def showCommand(param: Option[String]): Unit = {
    val options =  "scenario" :: "summary" :: "pieces" :: "events" ::
                    "sequence" :: "all" :: SpaceNames

    askOneOf("Show: ", options, param, allowNone = true, allowAbort = false) foreach {
      case "scenario"  => printSummary(scenarioSummary)
      case "summary"   => printSummary(statusSummary)
      case "pieces"    => printPiecesSummary()
      case "events"    => printSummary(eventSummary)
      case "sequence"  => printSummary(sequenceSummary)  // card, 1st eligible, etc.
      case "all"       => printGameState()
      case name        => printSummary(spaceSummary(name))
    }
  }

  def printPiecesSummary(): Unit = {
    printSummary(availablePiecesSummary)
    printSummary(casualtiesSummary)
    printSummary(outOfPlaySummary)
  }

  def printGameState(): Unit = {
    printSummary(scenarioSummary)
    printSummary(statusSummary)
    printPiecesSummary()
    printSummary(eventSummary)
    printSummary(sequenceSummary)
    println
    for (name <- SpaceNames; line <- spaceSummary(name))
      println(line)
  }

  // Display some or all of the game log.
  // usage:
  //   history            ##  Shows the log from the most recent game segment
  //   history 1          ##  Shows the log from the most recent game segment
  //   history n          ##  Shows the log from n the most recent game segments
  //   history all        ##  Shows the entire log
  //   history  >file     ##  Saves the log from the most recent game segment to file
  //   history 1 >file    ##  Saves the log from the most recent game segment to file
  //   history n >file    ##  Saves the log from n the most recent game segments to file
  //   history all >file  ##  Saves the entire log to file
  def showHistory(input: Option[String]): Unit = {
    case class Error(msg: String) extends Exception
    try {
      def redirect(tokens: List[String]): Option[String] = {
        tokens match {
          case Nil => None
          case x::xs  if !(x startsWith ">") => None
          case ">":: Nil => throw Error("No filename specified after '>'")
          case ">"::file::xs => Some(file)
          case file::xs => Some(file drop 1)
        }
      }

      val tokens = (input getOrElse "" split "\\s+").toList map (_.toLowerCase) dropWhile (_ == "")
      val (param, file) = if (tokens.isEmpty)
        (None, None)
      else if (!(tokens.head startsWith ">"))
          (tokens.headOption, redirect(tokens.tail))
      else
        (None, redirect(tokens))

      val NUM = """(\d+)""".r
      val segments = param match {
        case None                     => game.history.takeRight(1)
        case Some(NUM(n))             => game.history.takeRight(n.toInt)
        case Some("all" | "al" | "a") => game.history
        case Some(p)                  => throw Error(s"Invalid parameter: $p")
      }

      val msgs = for (s <- segments; msg <- s.log)
        yield msg

      file match {
        case None =>
          for (s <- segments; msg <- s.log)
            println(msg)
        case Some(fname) =>
          Pathname(fname).writer { w =>
            for (s <- segments; msg <- s.log) {
              w.write(msg)
              w.write(lineSeparator)
            }
          }
      }
    }
    catch {
      case e: IOException => println(s"IOException: ${e.getMessage}")
      case Error(msg) => println(msg)
    }
  }

  def rollback(param: Option[String]): Unit = {
    for ((segment, i) <- game.history.zipWithIndex)
      println(f"$i%3d: ${segment.description}")
  }

  def adjustSettings(param: Option[String]): Unit = {
    val agitate = if (game.isBot(VC)) List("agitate") else Nil
    val options = (
      List("resources", "aid", "patronage", "econ", "trail", "uspolicy", "casualties", "out of play",
      "capabilities", "momentum", "rvnLeaders", "pivot cards", "bot debug") ::: agitate
    ).sorted ::: SpaceNames

    val choice = askOneOf("[Adjust] (? for list): ", options, param, allowNone = true, allowAbort = false)
    choice foreach {
      case "resources"    => adjustResources()
      case "agitate"      => adjustAgitate()
      case "aid"          => adjustAid()
      case "patronage"    => adjustPatronage()
      case "econ"         => adjustEcon()
      case "trail"        => adjustTrail()
      case "uspolicy"     => adjustUSPolicy()
      case "casualties"   => adjustCasualties()
      case "out of play"  => adjustOutOfPlay()
      case "capabilities" => adjustCapabilities()
      case "momentum"     => adjustMomentum()
      case "rvnLeaders"   => adjustRvnLeaders()
      case "pivot cards"  => adjustPivotalCards()
      case "bot debug"    => adjustBotDebug()
      case name           => adjustSpace(name)
    }
  }


  def adjustmentDesc(name: String, oldValue: Any, newValue: Any): String = {
    def normalize(value: Any) = value match {
      case None                       => "none"
      case Some(x)                    => x.toString.trim
      case true                       => "yes"
      case false                      => "no"
      case s: Seq[_] if s.isEmpty     => "none"
      case s: Seq[_]                  => s map (_.toString) mkString ", "
      case x if x.toString.trim == "" => "none"
      case x                          => x.toString.trim
    }
    s"$name adjusted from [${normalize(oldValue)}] to [${normalize(newValue)}]"
  }

  def spaceAdjustmentDesc(spaceName: String, attributeName: String, oldValue: Any, newValue: Any): String =
    adjustmentDesc(s"$spaceName: $attributeName", oldValue, newValue)

  def adjustInt(name: String, current: Int, range: Range): Option[Int] = {
    val prompt = s"$name is $current.  Enter new value (${range.min} - ${range.max}) "
    @tailrec def getResponse(): Option[Int] =
      readLine(prompt) match {
        case null | "" => None
        case INTEGER(x) if range contains x.toInt => Some(x.toInt)
        case input =>
          println(s"$input is not valid")
          getResponse()
      }
    getResponse()
  }

  def adjustResources(): Unit = {
    if (game.isHuman(NVA)) {
        adjustInt("NVA resources", game.nvaResources, 0 to EdgeTrackMax) foreach { value =>
          val desc = adjustmentDesc("NVA resources", game.nvaResources, value)
          game = game.copy(nvaResources = value)
          log(desc)
          saveGameState(desc)
        }
    }
    else if (game.isHuman(VC)) {
      adjustInt("VC resources", game.vcResources, 0 to EdgeTrackMax) foreach { value =>
        val desc = adjustmentDesc("VC resources", game.vcResources, value)
        game = game.copy(vcResources = value)
        log(desc)
        saveGameState(desc)
      }
    }
    else {
      adjustInt("ARVN resources", game.arvnResources, 0 to EdgeTrackMax) foreach { value =>
        val desc = adjustmentDesc("ARVN resources", game.arvnResources, value)
        game = game.copy(arvnResources = value)
        log(desc)
        saveGameState(desc)
      }
    }
  }

  def adjustAgitate(): Unit = {
    adjustInt("VC Agitate total", game.vcResources, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("VC Agitate total (resources cylinder)", game.vcResources, value)
      game = game.copy(vcResources = value)
      log(desc)
      saveGameState(desc)
    }
  }

  def adjustAid(): Unit = {
    adjustInt("US Aid", game.usAid, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("US Aid", game.usAid, value)
      game = game.copy(usAid = value)
      log(desc)
      saveGameState(desc)
    }
  }

  def adjustPatronage(): Unit = {
    val origGame = game
    adjustInt("Patronage", game.patronage, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("Patronage", game.patronage, value)
      game = game.copy(patronage = value)
      log(desc)
      logPointsChanges(origGame, game)
      saveGameState(desc)
    }


  }

  def adjustEcon(): Unit = {
    adjustInt("Econ marker", game.econ, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("Econ marker", game.econ, value)
      game = game.copy(econ = value)
      log(desc)
      saveGameState(desc)
    }
  }

  def adjustTrail(): Unit = {
    adjustInt("Trail marker", game.trail, TrailMin to TrailMax) foreach { value =>
      val desc = adjustmentDesc("Trail Marker", game.trail, value)
      game = game.copy(trail = value)
      log(desc)
      saveGameState(desc)
    }
  }

  def adjustUSPolicy(): Unit = {
    val oldPolicy = game.usPolicy
    val options = List(USPolicy_JFK, USPolicy_LBJ, USPolicy_Nixon)
    val choices = options map (opt => opt -> opt)

    println(s"\nCurrent US Policy is: ${game.usPolicy}")
    val newPolicy = askMenu(choices, "\nChoose US Policy:", allowAbort = false).head
    if (newPolicy != oldPolicy) {
      game = game.copy(usPolicy = newPolicy)
      val desc = adjustmentDesc("US Policy", oldPolicy, newPolicy)
      log(desc)
      saveGameState(desc)
    }
  }

  // Move US pieces between the casualties box and the available box
  def adjustCasualties(): Unit = {

    def nextAdjustment(): Unit = {
      val casualties = game.casualties
      val available  = game.availablePieces.only(USPieces)
      val pieceChoices: List[(Option[PieceType], String)] = USPieces flatMap { t =>
        if (casualties.has(t) || available.has(t))
          Some(Some(t) -> t.plural)
        else
          None
      }
      val choices = pieceChoices :+ (None -> "Finished adjusting casualties")


      println(s"\nCasualties Pieces")
      println(separator())
      wrap("", casualties.descriptions) foreach println

      println(s"\nAvailable Pieces")
      println(separator())
      wrap("", available.descriptions) foreach println

      askMenu(choices, "\nSelect the type of casualties piece to adjust:", allowAbort = false).head foreach {
        pieceType =>
          val origNum = casualties.numOf(pieceType)
          val maxNum  = available.numOf(pieceType) + casualties.numOf(pieceType)

          adjustInt(pieceType.plural, origNum, 0 to maxNum) foreach { value =>
            if (value != origNum) {
              val newCasualties = game.casualties.set(value, pieceType)
              game = game.copy(casualties = newCasualties)
              log(adjustmentDesc(s"Casualties: ${pieceType.plural}", origNum, value))
            }
          }
          nextAdjustment()
      }
    }

    if (game.casualties.total + game.availablePieces.only(USPieces).total == 0)
      println("\nThere are no US forces in either the available or casualties box.")
    else {
      val savedGame = game
      nextAdjustment()
      if (savedGame.casualties != game.casualties) {
        // Number of available US pieces affects US score
        logPointsChanges(savedGame, game)
        saveGameState("Adjustments to US casualties")
      }
    }
  }

  // Move US/ARVN pieces between the out of play box and the available box
  def adjustOutOfPlay(): Unit = {
    val validTypes = USPieces ::: ARVNPieces
    def nextAdjustment(): Unit = {
      val outOfPlay = game.outOfPlay
      val available = game.availablePieces.only(validTypes)
      val pieceChoices: List[(Option[PieceType], String)] = validTypes flatMap { t =>
        if (outOfPlay.has(t) || available.has(t))
          Some(Some(t) -> t.plural)
        else
          None
      }
      val choices = pieceChoices :+ (None -> "Finished adjusting out of play pieces")


      println(s"\nOut of Play Pieces")
      println(separator())
      wrap("", outOfPlay.descriptions) foreach println

      println(s"\nAvailable Pieces")
      println(separator())
      wrap("", available.descriptions) foreach println

      askMenu(choices, "\nSelect the type of 'Out of Play' piece to adjust:", allowAbort = false).head foreach {
        pieceType =>
          val origNum = outOfPlay.numOf(pieceType)
          val maxNum  = available.numOf(pieceType) + outOfPlay.numOf(pieceType)

          adjustInt(pieceType.plural, origNum, 0 to maxNum) foreach { value =>
            if (value != origNum) {
              val newOutOfPlay = game.outOfPlay.set(value, pieceType)
              game = game.copy(outOfPlay = newOutOfPlay)
              log(adjustmentDesc(s"Out of Play: ${pieceType.plural}", origNum, value))
            }
          }
          nextAdjustment()
      }
    }

    if (game.outOfPlay.total + game.availablePieces.only(validTypes).total == 0)
      println("\nThere are no US or ARVN forces in either the available or out of play box.")
    else {
      val savedGame = game
      nextAdjustment()
      if (savedGame.outOfPlay != game.outOfPlay) {
        // Number of available US pieces affects US score
        logPointsChanges(savedGame, game)
        saveGameState("Adjustments to out of play pieces")
      }
    }
  }

  def adjustRvnLeaders(): Unit = {
    val LeaderCards = RVN_Leaders filterNot (_ == RVN_Leader_DuongVanMinh)

    def nextAdjustment(): Unit = {
      val available = LeaderCards filterNot game.rvnLeaders.contains map (l => l -> l)
      val choices = List(
        choice(available.nonEmpty,       "add",      "Add RVN Leader to the stack"),
        choice(game.rvnLeaders.size > 1, "remove",   "Remove top RVN leader from the stack"),
        choice(true,                     "finished", "Finished adjusting RVN Leaders")
      ).flatten

      println(s"\nRVN Leader Stack (top to bottom)")
      println(separator())
      wrap("", game.rvnLeaders) foreach println

      askMenu(choices, "\nChoose one:", allowAbort = false).head match {
        case "add" =>
          val leaderChoices = available :+ ("none" -> "Do not add an RVN leader")
          askMenu(leaderChoices, "Choose an RVN Leader:", allowAbort = false).head match {
            case "none" =>
            case leader =>
              game = game.copy(rvnLeaders = leader :: game.rvnLeaders)
              log(s"Adjusted RVN leaders: [added ${leader}]")
          }
          nextAdjustment()

        case "remove" =>
          val leader = game.rvnLeaders.head
          game = game.copy(rvnLeaders = game.rvnLeaders.tail)
          log(s"Adjusted RVN leaders: [removed ${leader}]")
          nextAdjustment()

        case _ =>
      }

    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.rvnLeaders != game.rvnLeaders)
      saveGameState("Adjustments to RVN Leader stack")
  }

  def adjustCapabilities(): Unit = {

    def askCapability(shaded: Boolean, names: List[String]): Option[Capability] = {
        val capChoices: List[(Option[Capability], String)] = names map { name =>
          val cap = Capability(name, shaded)
          Some(cap) -> s"Add ${cap}"
        }
        val choices = capChoices :+ (None -> "Do not add a capability")
        askMenu(choices, "\nChoose one:", allowAbort = false).head
    }

    def nextAdjustment(): Unit = {
      val included = game.capabilities map (_.name)
      val excluded = AllCapabilityNames filterNot included.contains

      val addChoices = List(
        choice(excluded.nonEmpty, unshadedCapability("add"), "Add unshaded capability"),
        choice(excluded.nonEmpty, shadedCapability("add"),   "Add shaded capability")
      ).flatten

      val removeChoices = game.capabilities map (cap => cap -> s"Remove ${cap}")
      val choices = addChoices ::: removeChoices ::: List(shadedCapability("finished") -> "Finished adjusting capabilities")

      println("\nCapabilities in play")
      println(separator())
      wrap("", game.capabilities map (_.toString)) foreach println
      askMenu(choices, "\nChoose one:", allowAbort = false).head match {
        case Capability("finished", _) =>

        case Capability("add", shaded) =>
          askCapability(shaded, excluded) foreach { cap =>
            game = game.copy(capabilities = cap :: game.capabilities)
            log(s"Adjusted capabilities: [added ${cap}]")
          }
          nextAdjustment()

        case cap =>
          game = game.copy(capabilities = game.capabilities filterNot (_ == cap))
          log(s"Adjusted capabilities: [removed ${cap}]")
          nextAdjustment()
      }
    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.capabilities != game.capabilities)
      saveGameState("Adjusted capabilities in play")
  }

  def adjustMomentum(): Unit = {

    def askMomentum(names: List[String]): Option[String] = {
        val moChoices: List[(Option[String], String)] = names map { name =>
          Some(name) -> s"Add ${name}"
        }
        val choices = moChoices :+ (None -> "Do not add a momentum card")
        askMenu(choices, "\nChoose one:", allowAbort = false).head
    }

    def nextAdjustment(): Unit = {
      var excluded = AllMomentum filterNot game.momentum.contains
      if (game.momentum exists (_ startsWith Medevac_prefix))
        excluded = excluded filterNot (_ startsWith Medevac_prefix)

      val addChoice = if (excluded.nonEmpty)
        List("add" -> "Add Momentum card")
      else
        Nil
      val choices = addChoice :::
                    (game.momentum map (c => c -> s"Remove $c")) :::
                    List("finished" -> "Finished adjusting Momentum cards")

      println("\nMomentum cards in play")
      println(separator())
      wrap("", game.momentum) foreach println
      askMenu(choices, "\nChoose one:", allowAbort = false).head match {
        case "finished" =>

        case "add" =>
          askMomentum(excluded) foreach { mo =>
            game = game.copy(momentum = mo :: game.momentum)
            log(s"Adjusted momentum cards: [added ${mo}]")
          }
          nextAdjustment()

        case mo =>
          game = game.copy(momentum = game.momentum filterNot (_ == mo))
          log(s"Adjusted momentum cards: [removed ${mo}]")
          nextAdjustment()
      }
    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.momentum != game.momentum)
      saveGameState("Adjusted momentum cards in play")
  }

  def adjustPivotalCards(): Unit = {

    def nextAdjustment(): Unit = {
      val pivotal = Faction.ALL.toList.sorted map {
        faction =>
        if (game.pivotCardsAvailable(faction))
          s"$faction (available)"
        else
          s"$faction (not available)"
      }

      val choices: List[(Option[Faction], String)] = (Faction.ALL.toList.sorted map {
        faction =>
        Some(faction) -> s"Toggle $faction pivotal event"
      }) :+ (None -> "Finished adjusting pivotal event availability")

      println("\nPivotal Event Availablity")
      println(separator())
      wrap("Pivotal     : ", pivotal) foreach println
      askMenu(choices, "\nChoose one:").head match {
        case None =>
        case Some(faction) if game.pivotCardsAvailable(faction) =>
          game = game.copy(pivotCardsAvailable = game.pivotCardsAvailable - faction)
          log(s"Adjusted $faction pivotal card: [now unavailable]")
          nextAdjustment()

        case Some(faction) =>
          game = game.copy(pivotCardsAvailable = game.pivotCardsAvailable + faction)
          log(s"Adjusted $faction pivotal card: [now available]")
          nextAdjustment()
      }
    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.pivotCardsAvailable != game.pivotCardsAvailable)
      saveGameState("Adjusted pivotal card availability")
  }

  def adjustBotDebug(): Unit = {
    val newValue = !game.botDebug
    val desc = adjustmentDesc("Bot debug", game.botDebug, newValue)
    game = game.copy(botDebug = newValue)
    log(desc)
    saveGameState(desc)
  }


  // case class Space(
  // name:       String,
  // spaceType:  SpaceType,
  // population: Int         = 0,   // Also used for econ value on LOCs
  // coastal:    Boolean     = false,
  // support:    SupportType = Neutral,
  // pieces:     Pieces      = Pieces(),
  // terror:     Int         = 0) {   // Sabatoge on LOCs

  def adjustSpace(name: String): Unit = {
    val origSpace = game.getSpace(name)
    def nextAction(): Unit = {
      val sp        = game.getSpace(name)
      val adjPieces = game.availablePieces.nonEmpty || sp.pieces.nonEmpty
      val marker    = if (sp.isLOC) "Sabotage" else "Terror"
      val choices   = List(
        choice(true,      "support", "Support level"),
        choice(true,      "terror",  s"Number of $marker markers"),
        choice(adjPieces, "pieces",  "Faction pieces"),
        choice(true,      "done",   s"Finished adjusting $name")
      ).flatten

      printSummary(spaceSummary(name, "Adjusting: "))
      askMenu(choices, "\nChoose one:", allowAbort = false).head match {
        case "support" => adjustSupport(name); nextAction()
        case "terror"  => adjustTerror(name);  nextAction()
        case "pieces"  => adjustPieces(name);  nextAction()
        case "done"    =>
      }
    }
    nextAction()
    if (game.getSpace(name) != origSpace)
      saveGameState(s"Adjusted space: $name")
  }


  def adjustSupport(name: String): Unit = {
    val origGame = game
    val sp = game.getSpace(name)
    val options = List(ActiveSupport, PassiveSupport, Neutral, PassiveOpposition, ActiveOpposition)
    val choices = options map (opt => opt -> opt.name)

    println()
    println(s"Choose support level for ${name}:")
    val newSupport = askMenu(choices, allowAbort = false).head
    game = game.updateSpace(sp.copy(support = newSupport))
    log(spaceAdjustmentDesc(name, "support", sp.support, newSupport))
    logPointsChanges(origGame, game)
  }

  def adjustTerror(name: String): Unit = {
    val sp     = game.getSpace(name)
    val marker = if (sp.isLOC) "Sabotage" else "Terror"
    val maxVal = game.terrorMarkersAvailable + sp.terror
    println()
    adjustInt(s"Number of $marker markers", sp.terror, 0 to maxVal) foreach { value =>
      game = game.updateSpace(sp.copy(terror = value))
      log(spaceAdjustmentDesc(name, marker, sp.terror, value))
    }
  }


  def adjustPieces(name: String): Unit = {

    def nextAdjustment(): Unit = {
      val sp        = game.getSpace(name)
      val pieces    = sp.pieces
      val available = game.availablePieces
      val pieceChoices: List[(Option[PieceType], String)] = AllPieceTypes flatMap { t =>
        if (pieces.has(t) || available.has(normalizedType(t)))
          Some(Some(t) -> t.plural)
        else
          None
      }
      val choices = pieceChoices :+ (None -> s"Finished adjusting pieces in ${name}")


      println(s"\nPieces in $name")
      println(separator())
      wrap("", pieces.descriptions) foreach println

      println(s"\nAvailable Pieces")
      println(separator())
      wrap("", game.availablePieces.descriptions) foreach println

      askMenu(choices, "\nSelect type of piece to adjust:", allowAbort = false).head foreach {
        pieceType =>
          val origNum = pieces.numOf(pieceType)
          val maxNum  = {
            val n = available.numOf(normalizedType(pieceType)) + pieces.numOf(pieceType)
            if (isBase(pieceType)) {
              val maxBase = 2 + pieces.numOf(pieceType) - pieces.totalOf(BasePieces)
               n min maxBase
            }
            else n
          }

          adjustInt(pieceType.plural, origNum, 0 to maxNum) foreach { value =>
            if (value != origNum) {
              loggingControlChanges {
                game = game.updateSpace(sp.setPieces(pieces.set(value, pieceType)))
                log(spaceAdjustmentDesc(name, pieceType.plural, origNum, value))
              }
            }
          }
          nextAdjustment()
      }
    }

    loggingControlChanges {
      nextAdjustment()      
    }
  }

}