
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

  val RVN_Leader_DuongVanMinh   = "Duong Van Minh"   // Printed on map
  val RVN_Leader_NguyenKhanh    = "Nguyen Khanh"     // Coup card #125
  val RVN_Leader_YoungTurks     = "Young Turks"      // Coup card #126
  val RVN_Leader_NguyenCaoKy    = "Nguyen Cao Ky"    // Coup card #127
  val RVN_Leader_NguyenVanThieu = "Nguyen Van Thieu" // Coup card #128

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
  val SouthernLaos       = "Souther Laos"
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
  val LOC_Hue_KheSanh             = "LOC: Hue -- Khe Sanh"
  val LOC_Hue_DaNang              = "LOC: Hue -- Da Nang"
  val LOC_DaNang_DakTo            = "LOC: DaNang -- DakTo"
  val LOC_DaNang_QuiNhon          = "LOC: DaNang -- Qui Nhon"
  val LOC_Kontum_DakTo            = "LOC: Kontum -- Dak To"
  val LOC_Kontum_QuiNhon          = "LOC: Kontum -- Qui Nhon"
  val LOC_Kontum_BanMeThuot       = "LOC: Kontum -- Ban Me Thuot"
  val LOC_QuiNhon_CamRanh         = "LOC: Qui Nhon -- Cam Ranh"
  val LOC_CamRanh_DaLat           = "LOC: Cam Ranh -- Da Lat"
  val LOC_BanMeThuot_DaLat        = "LOC: Ban Me Thuot -- Da Lat"
  val LOC_Saigon_CamRanh          = "LOC: Saigon -- Cam Ranh"
  val LOC_Saigon_DaLat            = "LOC: Saigon -- Da Lat"
  val LOC_Saigon_AnLoc_BanMeThuot = "LOC: Saigon -- An Loc -- Ban Me Thuot"
  val LOC_Saigon_CanTho           = "LOC: Saigon -- Can Tho"
  val LOC_CanTho_ChauDoc          = "LOC: Can Tho -- Chau Doc"
  val LOC_CanTho_BacLieu          = "LOC: Can Tho -- Bac Lieu"
  val LOC_CanTho_LongPhu          = "LOC: Can Tho -- Long Phu"

  // A space name and a list of adjacent space names
  val adjacencyMap: Map[String, Seq[String]] = Map(
    // Cities
    Hue                         -> Seq(QuangTri_ThuaThien, LOC_Hue_KheSanh, LOC_Hue_DaNang),
    DaNang                      -> Seq(QuangNam, QuangTin_QuangNgai, LOC_Hue_DaNang, LOC_DaNang_QuiNhon,
                                       LOC_DaNang_DakTo),
    Kontum                      -> Seq(BinhDinh, Pleiku_Darlac, PhuBon_PhuYen, LOC_Kontum_DakTo,
                                       LOC_Kontum_BanMeThuot, LOC_Kontum_QuiNhon),
    QuiNhon                     -> Seq(BinhDinh, PhuBon_PhuYen, LOC_DaNang_QuiNhon, LOC_Kontum_QuiNhon,
                                       LOC_QuiNhon_CamRanh),
    CamRahn                     -> Seq(KhanhHoa, BinhTuy_BinhThuan, LOC_QuiNhon_CamRanh, LOC_Saigon_CamRanh,
                                       LOC_CamRanh_DaLat),
    AnLoc                       -> Seq(PhuocLong, TayNinh, TheFishhook, LOC_Saigon_AnLoc_BanMeThuot),
    Saigon                      -> Seq(BinhTuy_BinhThuan, QuangDuc_LongKhanh, TayNinh, KienPhong,
                                       KienHoa_VinhBinh, LOC_Saigon_CamRanh, LOC_Saigon_DaLat,
                                       LOC_Saigon_AnLoc_BanMeThuot,
                                       LOC_Saigon_CanTho),
    CanTho                      -> Seq(KienPhong, KienHoa_VinhBinh, BaXuyen, KienGiang_AnXuyen,
                                       LOC_Saigon_CanTho, LOC_CanTho_ChauDoc, LOC_CanTho_BacLieu,
                                       LOC_CanTho_LongPhu),

    // Provinces
    CentralLaos                 -> Seq(NorthVietnam, QuangTri_ThuaThien, QuangNam, SouthernLaos,
                                       LOC_Hue_KheSanh),
    SouthernLaos                -> Seq(CentralLaos, QuangNam, QuangTin_QuangNgai, BinhDinh, Pleiku_Darlac,
                                       LOC_DaNang_DakTo, LOC_Kontum_DakTo),
    NortheastCambodia           -> Seq(SouthernLaos, TheFishhook, Pleiku_Darlac),
    TheFishhook                 -> Seq(NortheastCambodia, TheParrotsBeak, AnLoc, Pleiku_Darlac,
                                       QuangDuc_LongKhanh, PhuocLong, TayNinh, LOC_Saigon_AnLoc_BanMeThuot),
    TheParrotsBeak              -> Seq(TheFishhook, Sihanoukville, TayNinh, KienPhong, KienGiang_AnXuyen,
                                       LOC_CanTho_ChauDoc),
    Sihanoukville               -> Seq(TheParrotsBeak, KienGiang_AnXuyen),
    NorthVietnam                -> Seq(CentralLaos, QuangTri_ThuaThien, LOC_Hue_KheSanh),
    QuangTri_ThuaThien          -> Seq(NorthVietnam, Hue, CentralLaos, QuangNam, LOC_Hue_KheSanh,
                                       LOC_Hue_DaNang),
    QuangNam                    -> Seq(CentralLaos, SouthernLaos, QuangTri_ThuaThien, DaNang,
                                       QuangTin_QuangNgai, LOC_Hue_DaNang, LOC_DaNang_DakTo),
    QuangTin_QuangNgai          -> Seq(SouthernLaos, DaNang, QuangNam, BinhDinh, LOC_DaNang_DakTo,
                                       LOC_DaNang_QuiNhon),
    BinhDinh                    -> Seq(SouthernLaos, QuangTin_QuangNgai, QuiNhon, PhuBon_PhuYen, Kontum,
                                       Pleiku_Darlac, LOC_DaNang_DakTo, LOC_DaNang_QuiNhon, LOC_Kontum_DakTo,
                                       LOC_Kontum_QuiNhon),
    Pleiku_Darlac               -> Seq(SouthernLaos, NortheastCambodia, TheFishhook, BinhDinh, Kontum,
                                       PhuBon_PhuYen, KhanhHoa, QuangDuc_LongKhanh, LOC_Kontum_DakTo,
                                       LOC_Kontum_BanMeThuot, LOC_DaNang_DakTo, LOC_BanMeThuot_DaLat,
                                       LOC_Saigon_AnLoc_BanMeThuot),
    PhuBon_PhuYen               -> Seq(Kontum, BinhDinh, QuiNhon, KhanhHoa, Pleiku_Darlac,
                                       LOC_Kontum_QuiNhon, LOC_QuiNhon_CamRanh, LOC_Kontum_BanMeThuot),
    KhanhHoa                    -> Seq(PhuBon_PhuYen, CamRahn, BinhTuy_BinhThuan, QuangDuc_LongKhanh,
                                       Pleiku_Darlac, LOC_QuiNhon_CamRanh, LOC_CamRanh_DaLat,
                                       LOC_BanMeThuot_DaLat, LOC_Kontum_BanMeThuot, LOC_Saigon_DaLat),
    PhuocLong                   -> Seq(TheFishhook, AnLoc, QuangDuc_LongKhanh, TayNinh,
                                       LOC_Saigon_AnLoc_BanMeThuot),
    QuangDuc_LongKhanh          -> Seq(TheFishhook, Pleiku_Darlac, KhanhHoa, BinhTuy_BinhThuan, Saigon,
                                       TayNinh, PhuocLong, LOC_Kontum_BanMeThuot,
                                       LOC_Saigon_AnLoc_BanMeThuot, LOC_BanMeThuot_DaLat, LOC_Saigon_DaLat),
    BinhTuy_BinhThuan           -> Seq(Saigon, QuangDuc_LongKhanh, KhanhHoa, CamRahn, LOC_BanMeThuot_DaLat,
                                       LOC_CamRanh_DaLat, LOC_Saigon_DaLat, LOC_Saigon_CamRanh),
    TayNinh                     -> Seq(TheParrotsBeak, TheFishhook, AnLoc, PhuocLong, QuangDuc_LongKhanh,
                                       Saigon, KienPhong, LOC_Saigon_AnLoc_BanMeThuot),
    KienPhong                   -> Seq(TheParrotsBeak, TayNinh, Saigon, KienHoa_VinhBinh, CanTho,
                                       KienGiang_AnXuyen, LOC_CanTho_ChauDoc, LOC_Saigon_CanTho),
    KienHoa_VinhBinh            -> Seq(Saigon, KienPhong, CanTho, BaXuyen, LOC_Saigon_CanTho,
                                       LOC_CanTho_LongPhu),
    BaXuyen                     -> Seq(KienGiang_AnXuyen, CanTho, KienHoa_VinhBinh, LOC_CanTho_BacLieu,
                                       LOC_CanTho_LongPhu),
    KienGiang_AnXuyen           -> Seq(Sihanoukville, TheParrotsBeak, KienPhong, CanTho, BaXuyen,
                                       LOC_CanTho_ChauDoc, LOC_CanTho_BacLieu),
    // LOCs
    LOC_Hue_KheSanh             -> Seq(CentralLaos, NorthVietnam, Hue, QuangTri_ThuaThien),
    LOC_Hue_DaNang              -> Seq(Hue, QuangTri_ThuaThien, QuangNam, DaNang),
    LOC_DaNang_DakTo            -> Seq(DaNang, QuangNam, QuangTin_QuangNgai, SouthernLaos, BinhDinh,
                                       Pleiku_Darlac, LOC_Kontum_DakTo),
    LOC_DaNang_QuiNhon          -> Seq(DaNang, QuangTin_QuangNgai, BinhDinh, QuiNhon),
    LOC_Kontum_DakTo            -> Seq(Kontum, Pleiku_Darlac, SouthernLaos, BinhDinh),
    LOC_Kontum_QuiNhon          -> Seq(Kontum, BinhDinh, QuiNhon, PhuBon_PhuYen),
    LOC_Kontum_BanMeThuot       -> Seq(Kontum, Pleiku_Darlac, PhuBon_PhuYen, KhanhHoa, QuangDuc_LongKhanh,
                                       LOC_Saigon_AnLoc_BanMeThuot, LOC_BanMeThuot_DaLat),
    LOC_QuiNhon_CamRanh         -> Seq(QuiNhon, PhuBon_PhuYen, KhanhHoa, CamRahn),
    LOC_CamRanh_DaLat           -> Seq(CamRahn, KhanhHoa, BinhTuy_BinhThuan, QuangDuc_LongKhanh,
                                       LOC_Saigon_DaLat, LOC_BanMeThuot_DaLat),
    LOC_BanMeThuot_DaLat        -> Seq(Pleiku_Darlac, KhanhHoa, BinhTuy_BinhThuan, QuangDuc_LongKhanh,
                                       LOC_Kontum_BanMeThuot, LOC_Saigon_AnLoc_BanMeThuot, LOC_Saigon_DaLat,
                                       LOC_CamRanh_DaLat),
    LOC_Saigon_CamRanh          -> Seq(Saigon, CamRahn, BinhTuy_BinhThuan),
    LOC_Saigon_DaLat            -> Seq(Saigon, BinhTuy_BinhThuan, QuangDuc_LongKhanh, KhanhHoa,
                                       LOC_BanMeThuot_DaLat, LOC_CamRanh_DaLat),
    LOC_Saigon_AnLoc_BanMeThuot -> Seq(Saigon, TayNinh, QuangDuc_LongKhanh, PhuocLong, AnLoc, TheFishhook,
                                       Pleiku_Darlac, KhanhHoa, LOC_Kontum_BanMeThuot, LOC_BanMeThuot_DaLat),
    LOC_Saigon_CanTho           -> Seq(Saigon, CanTho, KienPhong, KienHoa_VinhBinh),
    LOC_CanTho_ChauDoc          -> Seq(CanTho, KienPhong, KienGiang_AnXuyen, TheParrotsBeak),
    LOC_CanTho_BacLieu          -> Seq(CanTho, KienGiang_AnXuyen, BaXuyen),
    LOC_CanTho_LongPhu          -> Seq(CanTho, BaXuyen, KienHoa_VinhBinh)
  )

  def getAdjacent(name: String): Seq[String] = adjacencyMap(name)
  def areAdjacent(name1: String, name2: String) = getAdjacent(name1) contains name2


  sealed trait PieceType {
    val name: String

    def singular = name
    def plural   = s"${name}s"
    def generic  = plural

    override def toString() = plural
  }

  type PieceTypeSet = Set[PieceType]

  case object USTroops       extends PieceType { val name = "US Troop" }
  case object Irregulars_U   extends PieceType { val name = "US Underground Irregular"; override def generic = "US Irregulars"}
  case object Irregulars_A   extends PieceType { val name = "US Active Irregular" }
  case object USBase         extends PieceType { val name = "US Base" }

  case object ARVNTroops     extends PieceType { val name = "ARVN Troop" }
  case object ARVNPolice     extends PieceType { val name = "ARVN Police"; override def plural = name }
  case object Rangers_U      extends PieceType { val name = "ARVN Underground Ranger"; override def generic = "ARVN Rangers" }
  case object Rangers_A      extends PieceType { val name = "ARVN Active Ranger" }
  case object ARVNBase       extends PieceType { val name = "ARVN Base" }

  case object NVATroops       extends PieceType { val name = "NVA Troop" }
  case object NVAGuerrillas_U extends PieceType { val name = "NVA Underground Guerrilla"; override def generic = "NVA Guerrillas" }
  case object NVAGuerrillas_A extends PieceType { val name = "NVA Active Guerrilla" }
  case object NVABase         extends PieceType { val name = "NVA Base" }
  case object NVATunnel       extends PieceType { val name = "NVA Tunneled Base" }

  case object VCGuerrillas_U  extends PieceType { val name = "VC Underground Guerrilla"; override def generic = "VC Guerrillas" }
  case object VCGuerrillas_A  extends PieceType { val name = "VC Active Guerrilla" }
  case object VCBase          extends PieceType { val name = "VC Base" }
  case object VCTunnel        extends PieceType { val name = "VC Tunneled Base" }

  val USPieces        = List(USTroops, Irregulars_U, Irregulars_A, USBase)
  val ARVNPieces      = List(ARVNTroops, ARVNPolice, Rangers_U, Rangers_A, ARVNBase)
  val NVAPieces       = List(NVATroops, NVAGuerrillas_U, NVAGuerrillas_A, NVABase, NVATunnel)
  val VCPieces        = List(VCGuerrillas_U, VCGuerrillas_A, VCBase, VCTunnel)
  val CoinPieces      = USPieces:::ARVNPieces
  val InsurgentPieces = NVAPieces:::VCPieces
  val NonNVAPieces    = USPieces:::ARVNPieces:::VCPieces

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

    def total = Faction.ALL.foldLeft(0) { (sum, faction) => sum + totalFaction(faction) }

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
  implicit def spacePieces(sp: Space): Pieces = sp.pieces

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

  sealed trait SupportType {
    val name: String
    
    override def toString() = name
  }
  case object Neutral           extends SupportType { val name = "Neutral" }
  case object PassiveSupport    extends SupportType { val name = "Passive Support"}
  case object ActiveSupport     extends SupportType { val name = "Active Support"}
  case object PassiveOpposition extends SupportType { val name = "Passive Opposition" }
  case object ActiveOpposition  extends SupportType { val name = "Active Opposition" }

  object SupportType {
    val ALL = Set[SupportType](Neutral, PassiveSupport, ActiveSupport, PassiveOpposition, ActiveOpposition)
    def apply(name: String): SupportType = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SupportType name: $name")
    }
  }
  
  sealed trait Control {
    val name: String
    
    override def toString() = name
  }
  case object Uncontrolled   extends Control { val name = "Uncontrolled"    }
  case object CoinControlled extends Control { val name = "COIN Controlled" }
  case object NvaControlled  extends Control { val name = "NVA Controlled"  }

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

    def econValue  = population  // overloaded field
    def isCity     = spaceType == City
    def isProvince = spaceType == HighlandProvince || spaceType == LowlandProvince || spaceType == JungleProvince
    def isLOC      = spaceType == LOC

    def isHighland = spaceType == HighlandProvince
    def isLowland  = spaceType == LowlandProvince
    def isJungle   = spaceType == JungleProvince

    
    def coinControlled: Boolean = pieces.totalOf(CoinPieces) > pieces.totalOf(InsurgentPieces)
    def nvaControlled: Boolean  = pieces.totalOf(NVAPieces) > pieces.totalOf(NonNVAPieces)
    def control = if (coinControlled)
      CoinControlled
    else if (nvaControlled)
      NvaControlled
    else
      Uncontrolled

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


    def addPieces(newPieces: Pieces): Space = copy(pieces = pieces + newPieces)
    def removePieces(removedPieces: Pieces): Space = copy(pieces = pieces - removedPieces)
    def setPieces(newPieces: Pieces): Space = copy(pieces = newPieces)
    

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
  val Cap_CombActionPlatoons = "#18 Combined Acton Platoons" // affects traiing / sweep
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


  val AllCapabilities = List(
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

  // Momentum markers
  val Mo_WildWeasels       = "#5 Wild Weasels"          // Shaded   (affects Air Strike)
  val Mo_ADSID             = "#7 ADSID"                 // Unshaded (affects change to trail value)
  val Mo_RollingThunder    = "#10 Rolling Thunder"      // Shaded   (prohibits air strike)
  val Mo_Medevac_Unshaded  = "#15 Medevac (unshaded)"   // (affects commitment phase during coup round)
  val Mo_Medevac_Shaded    = "#15 Medevac (shaded)"     // (prohibits air lift)
  val Mo_BlowtorchKomer    = "#16 Blowtorch Komer"      // Unshaded (Pacity costs 1 resource per step/teror)
  val Mo_Claymores         = "#17 Claymores"            // Unshaded (prohibits ambush, affect guerrilla march)
  val Mo_DaNang            = "#22 Da Nang"              // Shaded (prohibits air strike)
  val Mo_McNamaraLine      = "#38 McNamara Line"        // Single event (prohibits infiltrate, prohibits trail improvement by rally)
  val Mo_Oriskany          = "#39 Oriskany"             // Shaded (prohibits degrade of trail)
  val Mo_BombingPause      = "#41 Bombing Pause"        // Single event (prohibits air strike)
  val Mo_559TransportGrp   = "#46 559th Transport Grp"  // Unshaded (Infiltrate is max 1 space)
  val Mo_BodyCount         = "#72 Body Count"           // Unshaded (affects asasult and patrol)
  val Mo_GeneralLansdale   = "#78 General Lansdale"     // Shaded (prohibits assault)
  val Mo_TyphoonKate       = "#115 Typhoon Kate"        // Single event (prohibits air lift, transport, and bombard, all other special activities are max 1 space)

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


  sealed trait Action {
    val name: String
    override def toString() = name
  }
  case object  Event         extends Action { val name = "Event"                    }
  case object  OpPlusSpecial extends Action { val name = "Op/Special Activity" }
  case object  OpOnly        extends Action { val name = "Op Only"             }
  case object  LimitedOp     extends Action { val name = "Limited Op"          }

  case class Actor(faction: Faction, action: Action)

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
        case Nil        => List(OpOnly, OpPlusSpecial, Event)
        case first::Nil => first.action match {
          case OpOnly        => List(LimitedOp)
          case OpPlusSpecial => List(LimitedOp, Event)
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
    capabilities: List[Capability]    = Nil,
    rvnLeaders: List[String]          = List(RVN_Leader_DuongVanMinh),
    momentum: List[String]            = Nil,
    sequence: SequenceOfPlay          = SequenceOfPlay(),
    cardsDrawn: Int                   = 0,
    currentCard: Int                  = 0,
    onDeckCard: Option[Int]           = None,
    prevCardWasCoup: Boolean          = false,
    coupCardsPlayed: Int              = 0,  // Number of Coup cards played/ignored thus far
    pivotCardsAvailable: Set[Faction] = Set.empty,
    turn: Int                         = 0,  // turn zero indicates the start of the game
    botDebug: Boolean                 = false,
    history: Vector[GameSegment]      = Vector.empty,
    log: Vector[String]               = Vector.empty) {  // Log of the cuurent game segment


    lazy val allPiecesOnMap = spaces.foldLeft(Pieces()) { (total, space) => total + space.pieces }
    lazy val availablePieces = ForcePool - allPiecesOnMap.normalized - casualties.normalized - outOfPlay.normalized
    lazy val currentRvnLeader = rvnLeaders.head
    lazy val locSpaces = spaces filter (_.isLOC)
    lazy val nonLocSpaces = spaces filterNot (_.isLOC)

    // Create a one line turn description for the current game state.
    // This is used to mark the current game segment, etc.
    // "#10 Rolling Thunder - 0 acted, US is up"
    def description: String = {
      val card = deck(currentCard)
      val b = new StringBuilder(s"$card - ")
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
    def isMonsoon = !isCoupRound && onDeckIsCoup

    // Count the total number of something in each space on the map
    def totalOnMap(numberPerSpace: Space => Int): Int =
      spaces.foldLeft(0) { (total, space) => total + numberPerSpace(space) }

    // Count the number of a type of piece that is on the map, casualties, or out or play
    def piecesInUse(numberPer: Pieces => Int): Int =
      totalOnMap(space => numberPer(space.pieces)) + numberPer(casualties) + numberPer(outOfPlay)

    def totalCoinControl            = totalOnMap(sp => if (sp.coinControlled) sp.population else 0)
    def totalNvaControl             = totalOnMap(sp => if (sp.nvaControlled) sp.population else 0)
    def availablelUSTroopsAndBases  = availablePieces.totalOf(USTroops::USBase::Nil)
    def totalSupport                = totalOnMap(_.supportValue)
    def totalOpposition             = totalOnMap(_.oppositionValue)
    def nvaBasesOnMap               = totalOnMap(_.pieces.totalNVABases)
    def vcBasesOnMap                = totalOnMap(_.pieces.totalVCBases)

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
      for (t <- types; name = t.generic; count = avail.numOf(t))
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
      for (t <- types; name = t.generic; count = game.casualties.numOf(t) if count > 0)
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
    val USPieces        = List(USTroops, Irregulars_U, Irregulars_A, USBase)
    val ARVNPieces      = List(ARVNTroops, ARVNPolice, Rangers_U, Rangers_A, ARVNBase)
    
    val b = new ListBuffer[String]
    
    def addPieces(types: Seq[PieceType]): Unit = {
      for (t <- types; name = t.generic; count = game.outOfPlay.numOf(t) if count > 0)
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
    b += "Active Events"
    b += separator()
    wrap("Capabilities: ", game.capabilities map (_.toString)) foreach (b += _)
    wrap("Momentum    : ", game.momentum)     foreach (b += _)
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
            println("Choose a scenario:")
            val choices = scenarioChoices :+ ("quit" -> "Quit")
            askMenu(choices, allowAbort = false).head match {
              case "quit"   => throw ExitGame
              case scenario => scenario
            }
          }
          val scenario = scenarios(scenarioName)
          val usePeriodEvents = askYorN("\nAre you using period events? (y/n) ")
          val humanFaction = askFaction("Which faction do you wish to play", allowAbort = false)
          
          // println()
          // gameName = Some(askGameName("Enter a name for your new game: "))

          game = initialGameState(scenario, humanFaction, usePeriodEvents)
          
          logSummary(scenarioSummary)
          log()
          scenario.additionalSetup()
          
          //  If VC is a Bot then we use the vcResources as the Agitate Total
          //  This is initialized by rolling a d3
          val agitateTotal = d3
          log(s"\nRolling d3 to set the Agitate Total (VC resources cylinder)")
          if (game.isBot(VC))
            setAgitateTotal(d3)
          
          
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
      
      // saveGameState(game.description)  // Save the current game state
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
            resolveNextActor()
          case ActCmd  =>
            Human.act()
            log(s"\nFinished with $faction turn")
            resolveNextActor()
          case BotCmd  =>
            val action = shuffle(game.sequence.availableActions).head
            
            game = game.copy(sequence = game.sequence.addActor(faction, action))
            log()
            log(s"Move the $faction cylinder to the $action box")
            
            // Bot.act()
            log(s"\nFinished with $faction turn")
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
    //   // TODO: Need to implement logic to determine if Bots will play
    //   // their pivotal event.
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
        val savedState = game
        val result     = code

        // Check each space for a control change and log the results
        for (sp <- game.spaces.sortBy(_.name); orig = savedState.getSpace(sp.name))
          logControlChange(orig, sp)

          def nvaPoints  = game.totalNvaControl + game.nvaBasesOnMap
          def arvnPoints = game.totalCoinControl + game.patronage

        // Log any edge track control markers that have changed
        if (game.arvnPoints != savedState.arvnPoints)
          log(s"Move the 'COIN + Patronage' marker to ${game.arvnPoints}")
        
        if (game.nvaPoints != savedState.nvaPoints)
          log(s"Move the 'NVA + Bases' marker to ${game.nvaPoints}")
        result
      }
      finally {
        loggingControlChangesActive = false
      }
    }
  }

  def logControlChange(orig: Space, updated: Space): Unit = {
    assert(orig.name == updated.name, "logControlChange: not the same space!")
    // LOCs do not have control
    if (!orig.isLOC) {
      (orig.control, updated.control) match {
        case (x, y) if (x == y) => // No change to log
        case (Uncontrolled, _)  => log(s"Place ${updated.control} marker in ${orig.name}")
        case (_, Uncontrolled)  => log(s"Remove ${orig.control} marker from ${orig.name}")
        case _                  => log(s"Flip control marker in ${orig.name} to: ${updated.control}")
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
  def matchOne(s: String, options: Seq[String]): Option[String] = {
    if (s == "?") {
      println(s"Enter one of:\n${orList(options)}")
      None
    }
    else {
      val normalized = options map (_.toLowerCase)
      (normalized.distinct filter (_ startsWith s.toLowerCase)) match {
        case Seq() =>
          println(s"'$s' is not valid. Must be one of:\n${orList(options)}")
          None
        case Seq(v)  =>
          Some(options(normalized.indexOf(v)))

        case many if many exists (_ == s) =>
          Some(options(normalized.indexOf(s)))

        case ambiguous =>
          println(s"'$s' is ambiguous. (${orList(ambiguous)})")
          None
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

  def askOddInt(prompt: String, low: Int, high: Int, default: Option[Int] = None, allowAbort: Boolean = true): Int = {
    def nextTry(): Int = askInt(prompt, low, high, default, allowAbort) match {
      case n if n % 2 == 1 => n
      case _ =>
        println("Please enter an odd number")
        nextTry()
    }
    nextTry()
  }

  def askEvenInt(prompt: String, low: Int, high: Int, default: Option[Int] = None, allowAbort: Boolean = true): Int = {
    def nextTry(): Int = askInt(prompt, low, high, default, allowAbort) match {
      case n if n % 2 == 0 => n
      case _ =>
        println("Please enter an even number")
        nextTry()
    }
    nextTry()
  }

  // Convenience method for createing choices for the askMenu() function.
  def choice[T](condition: Boolean, value: T, desc: String): Option[(T, String)] =
    if (condition) Some(value -> desc) else None

  def askSimpleMenu[T](items: List[T],
                       heading: String = "",
                       numChoices: Int = 1,
                       repeatsOK: Boolean = false,
                       allowAbort: Boolean = true): List[T] = {
    askMenu(items map (i => i -> i.toString), heading, numChoices, repeatsOK, allowAbort)
  }

  // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenu[T](items: List[(T, String)],
                 heading: String = "",
                 numChoices: Int = 1,
                 repeatsOK: Boolean = false,
                 allowAbort: Boolean = true): List[T] = {
    def nextChoice(num: Int, itemsRemaining: ListMap[T, String]): List[T] = {
      if (itemsRemaining.isEmpty || num > numChoices)
        Nil
      else if (itemsRemaining.size == 1)
        itemsRemaining.keys.head :: Nil
      else {
        println(heading)
        println(separator(char = '='))
        val indexMap = (itemsRemaining.keys.zipWithIndex map (_.swap)).toMap
        for ((key, i) <- itemsRemaining.keysIterator.zipWithIndex)
          println(s"${i+1}) ${itemsRemaining(key)}")
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
  

  // TODO: ...
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
      case "uspolicy"     => adjustUspolicy()
      case "casualties"   => adjustCasualties()
      case "out of play"  => adjustOutOfPlay()
      case "capabilities" => adjustCapabilities()
      case "momentum"     => adjustMomentum()
      case "rvnLeaders"   => adjustRvnLeaders()
      case "pivot cards"  => adjustPivotCards()
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
  }
  
  def adjustPatronage(): Unit = {
  }
  
  def adjustEcon(): Unit = {
  }
  
  def adjustTrail(): Unit = {
  }
  
  def adjustUspolicy(): Unit = {
  }
  
  def adjustCasualties(): Unit = {
  }
  
  def adjustOutOfPlay(): Unit = {
  }
  
  def adjustCapabilities(): Unit = {
  }
  
  def adjustMomentum(): Unit = {
  }
  
  def adjustRvnLeaders(): Unit = {
  }
  
  def adjustPivotCards(): Unit = {
  }
  
  

  //
  //
  // def adjustCapabilities(): Unit = {
  //   var included = game.capabilities
  //   var excluded = AllCapabilities filterNot included.contains
  //
  //   def nextAdjustment(): Unit = {
  //     val choices = (included.sorted map (c => c -> s"Remove $c")) ++
  //                   (excluded.sorted map (c => c -> s"Add $c")) :+
  //                   ("done" -> "Finished")
  //     askMenu(choices, "\nAdjusting capabilities:", allowAbort = false).head match {
  //       case "done" =>
  //       case cap    =>
  //         if (included contains cap) {
  //           included = included filterNot (_ == cap)
  //           excluded = cap :: excluded
  //           cap match {
  //             case CapShadedNiallNoigiallach =>
  //               game = game.copy(eventData = game.eventData.copy(niallNoigiallachRaiders = 0))
  //             case _ =>
  //           }
  //         }
  //         else {
  //           included = cap :: included
  //           excluded = excluded filterNot (_ == cap)
  //         }
  //         nextAdjustment()
  //     }
  //   }
  //
  //   nextAdjustment()
  //   if (included != game.capabilities) {
  //     log(adjustmentDesc("capabilities", game.capabilities.sorted, included.sorted))
  //     game = game.copy(capabilities = included)
  //     saveGameState("Adjusted capabilities in play")
  //
  //   }
  // }
  //
  //
  // def adjustMomentum(): Unit = {
  //   var included = game.momentum
  //   var excluded = AllMomentum filterNot included.contains
  //
  //   def nextAdjustment(): Unit = {
  //     val choices = (included.sorted map (c => c -> s"Remove $c")) ++
  //                   (excluded.sorted map (c => c -> s"Add $c")) :+
  //                   ("done" -> "Finished")
  //     askMenu(choices, "\nAdjusting Momentum events:", allowAbort = false).head match {
  //       case "done" =>
  //       case event    =>
  //         if (included contains event) {
  //           included = included filterNot (_ == event)
  //           excluded = event :: excluded
  //         }
  //         else {
  //           included = event :: included
  //           excluded = excluded filterNot (_ == event)
  //           event match {
  //             case MoUnshadedNiallsRaid =>
  //               val sea = askSimpleMenu(Seas, "Select sea for Niall's Raid:", allowAbort = false).head
  //               game = game.copy(eventData = game.eventData.copy(niallsRaidSea = sea))
  //
  //             case MoShadedRuinOfTheVillas =>
  //               val faction = askSimpleMenu(List(Saxon, Scotti), "Select faction for Ruin of the Villas:", allowAbort = false).head
  //               game = game.copy(eventData = game.eventData.copy(ruinOfTheVillasFaction = faction))
  //
  //             case MoShadedFeedingTheRavens =>
  //               val faction = askSimpleMenu(Faction.ALL.toList.sorted, "Select faction for Feeding the Ravens:", allowAbort = false).head
  //               game = game.copy(eventData = game.eventData.copy(feedingTheRavensFaction = faction))
  //             case _ =>
  //           }
  //         }
  //         nextAdjustment()
  //     }
  //   }
  //
  //   nextAdjustment()
  //   if (included != game.capabilities) {
  //     log(adjustmentDesc("momentum", game.momentum.sorted, included.sorted))
  //     game = game.copy(momentum = included)
  //     saveGameState("Adjusted momentum events in play")
  //   }
  // }
  //
  // def adjustNiallRaiders(): Unit = {
  //   val numAvail = game.scottiRaidersAvailable + game.eventData.niallNoigiallachRaiders
  //   adjustInt("Scotti raiders on Niall Noigiallach", game.eventData.niallNoigiallachRaiders, 0 to numAvail) foreach { value =>
  //     val desc = adjustmentDesc("Scotti raiders on Niall Noigiallach", game.eventData.niallNoigiallachRaiders, value)
  //     game = game.copy(eventData = game.eventData.copy(niallNoigiallachRaiders = value))
  //     log(desc)
  //     saveGameState(desc)
  //   }
  // }
  //
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
      val choices   = List(
        choice(true,      "support", "Support level"),
        choice(true,      "terror",  "Number of terror markers"),
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
    val sp = game.getSpace(name)
    val options = List(ActiveSupport, PassiveSupport, Neutral, PassiveOpposition, ActiveOpposition)
    val choices = options map (opt => opt -> opt.name)

    println()
    println(s"Choose support level for ${name}:")
    val newSupport = askMenu(choices, allowAbort = false).head
    game = game.updateSpace(sp.copy(support = newSupport))
    log(spaceAdjustmentDesc(name, "support", sp.support, newSupport))
  }
  
  def adjustTerror(name: String): Unit = {
    val sp = game.getSpace(name)
    println()
    adjustInt("Number of terror markers", sp.terror, 0 to 10) foreach { value =>
    game = game.updateSpace(sp.copy(terror = value))
    log(spaceAdjustmentDesc(name, "terror", sp.terror, value))
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
          val maxNum  = available.numOf(normalizedType(pieceType)) + pieces.numOf(pieceType)
        
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
    
    nextAdjustment()
  }

}