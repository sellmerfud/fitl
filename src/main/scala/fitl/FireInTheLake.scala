
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
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.language.implicitConversions
import scala.util.Properties.isWin
import scala.util.Properties.lineSeparator
import scala.util.Random.nextInt
import scala.util.Random.shuffle

import Ordering.Implicits._
import FUtil.Pathname
import scenarios._
import Bot.{ TrungCard, TrungDeck }

object FireInTheLake {

  val SOFTWARE_VERSION = "0.15"
  val INTEGER = """(\d+)""".r

  def d6 = nextInt(6) + 1
  def d3 = nextInt(3) + 1
  def rollDice(numDice: Int) = List.fill(numDice)(d6).sum

  val EdgeTrackMax = 75
  val TrailMin     = 0
  val TrailMax     = 4

  val TerrorMarkerManifest = 15
  val TunnelMarkerManifest = 6

  val USPolicy_JFK   = "JFK"
  val USPolicy_LBJ   = "LBJ"
  val USPolicy_Nixon = "Nixon"

  sealed abstract class Faction(val name: String, val sortOrder: Int, val pivotCard: Int, val tieBreak: Int) {
    override def toString() = name
  }

  case object US   extends Faction("US",   1, PivotalUS,   4)
  case object ARVN extends Faction("ARVN", 2, PivotalARVN, 2)
  case object VC   extends Faction("VC",   3, PivotalVC,   1)
  case object NVA  extends Faction("NVA",  4, PivotalNVA,  3)

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
  def isCoin(faction: Faction) = faction == US || faction == ARVN
  def isInsurgent(faction: Faction) = faction == NVA || faction == VC

  case class Score(faction: Faction, points: Int, score: Int)
  // Order scores from highest to lowest
  // With ties, Bot beat Humans
  // Then ties are resolved VC, ARVN, NVA, then US
  implicit val ScoreOrdering = new Ordering[Score] {
    def compare(x: Score, y: Score) = {
      if (x.score != y.score)
        y.score - x.score  // Highest comes first
      else if (game.isBot(x.faction) && !game.isBot(y.faction)) -1
      else if (!game.isBot(x.faction) && game.isBot(y.faction)) 1
      else
        x.faction.tieBreak - y.faction.tieBreak
    }
  }

  sealed trait BotEventPriority
  case object NotExecuted extends BotEventPriority
  case object Critical    extends BotEventPriority
  case object Performed   extends BotEventPriority

  sealed trait EventPart
  case object Unshaded extends EventPart
  case object Shaded   extends EventPart

  sealed trait EventType
  case object SingleEvent extends EventType
  case object DualEvent   extends EventType

  // For cards that have only one event:
  //  dual == false and the event condtions and execution use the `unshaded` fields
  abstract class EventCard(
    val number: Int,
    val name: String,
    val eventType: EventType,  // single/dual
    val factionOrder: List[Faction],
    val botPriorities: ListMap[Faction, (BotEventPriority, EventPart)]) {

    def unshadedEffective(faction: Faction): Boolean
    def executeUnshaded(faction: Faction): Unit

    def shadedEffective(faction: Faction): Boolean
    def executeShaded(faction: Faction): Unit


    val isCoup = factionOrder.isEmpty

    def numAndName = s"#$number - $name"
    override def toString() = numAndName

    def botPriority(faction: Faction) = botPriorities(faction)._1
    // EventPart: Unshaded/Shaded
    def eventPart(faction: Faction)     = botPriorities(faction)._2

    def orderString  = factionOrder map (_.name) mkString ", "
    def fullString = if (isCoup) numAndName else s"$numAndName ($orderString)"

    def eventEffective(faction: Faction): Boolean = (eventType, eventPart(faction)) match {
      case (SingleEvent, _)      => unshadedEffective(faction)
      case (DualEvent, Unshaded) => unshadedEffective(faction)
      case (DualEvent, Shaded)   => shadedEffective(faction)
    }

    def isCritical(faction: Faction)  = botPriority(faction) == Critical
    def isPerformed(faction: Faction) = botPriority(faction) == Performed
    def isIgnored(faction: Faction)   = botPriority(faction) == NotExecuted
  }

  // Sort by card number
  implicit val CardOrdering = new Ordering[EventCard] {
    def compare(x: EventCard, y: EventCard) = x.number compare y.number
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

  def rvnLeaderForCard(cardNum: Int): String = cardNum match {
    case 125 => RVN_Leader_NguyenKhanh
    case 126 => RVN_Leader_YoungTurks
    case 127 => RVN_Leader_NguyenCaoKy
    case 128 => RVN_Leader_NguyenVanThieu
    case 129 => RVN_Leader_FailedCoup129
    case 130 => RVN_Leader_FailedCoup130
    case x   => throw new IllegalArgumentException("Bad card number for RVN Leader")
  }

  // Pivotal card numbers
  val PivotalUS   = 121
  val PivotalNVA  = 122
  val PivotalARVN = 123
  val PivotalVC   = 124

  object eventDeck {
    import fitl.cards._
    private def entry(card: EventCard) = (card.number -> card)

    val deckMap: Map[Int, EventCard] = Map(
    entry(Card_001), entry(Card_002), entry(Card_003), entry(Card_004), entry(Card_005),
    entry(Card_006), entry(Card_007), entry(Card_008), entry(Card_009), entry(Card_010),
    entry(Card_011), entry(Card_012), entry(Card_013), entry(Card_014), entry(Card_015),
    entry(Card_016), entry(Card_017), entry(Card_018), entry(Card_019), entry(Card_020),
    entry(Card_021), entry(Card_022), entry(Card_023), entry(Card_024), entry(Card_025),
    entry(Card_026), entry(Card_027), entry(Card_028), entry(Card_029), entry(Card_030),
    entry(Card_031), entry(Card_032), entry(Card_033), entry(Card_034), entry(Card_035),
    entry(Card_036), entry(Card_037), entry(Card_038), entry(Card_039), entry(Card_040),
    entry(Card_041), entry(Card_042), entry(Card_043), entry(Card_044), entry(Card_045),
    entry(Card_046), entry(Card_047), entry(Card_048), entry(Card_049), entry(Card_050),
    entry(Card_051), entry(Card_052), entry(Card_053), entry(Card_054), entry(Card_055),
    entry(Card_056), entry(Card_057), entry(Card_058), entry(Card_059), entry(Card_060),
    entry(Card_061), entry(Card_062), entry(Card_063), entry(Card_064), entry(Card_065),
    entry(Card_066), entry(Card_067), entry(Card_068), entry(Card_069), entry(Card_070),
    entry(Card_071), entry(Card_072), entry(Card_073), entry(Card_074), entry(Card_075),
    entry(Card_076), entry(Card_077), entry(Card_078), entry(Card_079), entry(Card_080),
    entry(Card_081), entry(Card_082), entry(Card_083), entry(Card_084), entry(Card_085),
    entry(Card_086), entry(Card_087), entry(Card_088), entry(Card_089), entry(Card_090),
    entry(Card_091), entry(Card_092), entry(Card_093), entry(Card_094), entry(Card_095),
    entry(Card_096), entry(Card_097), entry(Card_098), entry(Card_099), entry(Card_100),
    entry(Card_101), entry(Card_102), entry(Card_103), entry(Card_104), entry(Card_105),
    entry(Card_106), entry(Card_107), entry(Card_108), entry(Card_109), entry(Card_110),
    entry(Card_111), entry(Card_112), entry(Card_113), entry(Card_114), entry(Card_115),
    entry(Card_116), entry(Card_117), entry(Card_118), entry(Card_119), entry(Card_120),
    entry(Card_121), entry(Card_122), entry(Card_123), entry(Card_124), entry(Card_125),
    entry(Card_126), entry(Card_127), entry(Card_128), entry(Card_129), entry(Card_130))

    val CoupCards = Range.inclusive(125, 130).toSet
    val PivotalCards = Set(PivotalUS, PivotalNVA, PivotalVC, PivotalARVN)

    def isValidNumber(num: Int) = deckMap contains num
    def isPivotalCard(num: Int) = PivotalCards contains num

    def apply(num: Int): EventCard = deckMap(num)
    def cards: List[EventCard]     = deckMap.valuesIterator.toList.sorted
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
  val LOC_DaNang_DakTo            = "LOC Da Nang -- Dak To"
  val LOC_DaNang_QuiNhon          = "LOC Da Nang -- Qui Nhon"
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

  val Cities       = List(Hue, DaNang, Kontum, QuiNhon, CamRahn, AnLoc, Saigon, CanTho)
  val OutsideSouth = List(NorthVietnam, CentralLaos, SouthernLaos, NortheastCambodia,
                          TheFishhook, TheParrotsBeak, Sihanoukville)

  val Laos         = List(CentralLaos, SouthernLaos)
  val Cambodia     = List(NortheastCambodia, TheFishhook, TheParrotsBeak, Sihanoukville)
  val LaosCambodia = Laos:::Cambodia

  val MekongLoCs = List(LOC_Saigon_CanTho, LOC_CanTho_ChauDoc, LOC_CanTho_LongPhu)

  // Order space name Cities first then Provinces the LoCs
  // Within each category sort alphabetically
  val SpaceNameOrdering = new Ordering[String] {
    private def typeScore(name: String) =
      if      (name startsWith "LOC") 3
      else if (Cities contains name)  1
      else                            2

    def compare(x: String, y: String) = {
      (typeScore(x), typeScore(y)) match {
        case (sx, sy) if sx == sy => x compare y  // Alphabetical within same type
        case (sx, sy)             => sx - sy
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
  def areAdjacent(name1: String, name2: String): Boolean = getAdjacent(name1) contains name2
  def getAdjacentLOCs(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isLoC }
  def getAdjacentCities(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isCity }
  def getAdjacentProvinces(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isProvince }
  def getAdjacentLOCsAndCities(name: String): Set[String] = getAdjacentLOCs(name) ++ getAdjacentCities(name)
  def getAdjacentNonLOCs(name: String): Set[String] = getAdjacentProvinces(name) ++ getAdjacentCities(name)

  def isInLaos(name: String)         = Laos contains name
  def isInCambodia(name: String)     = Cambodia contains name
  def isInLaosCambodia(name: String) = LaosCambodia contains name
  def isOutsideSouth(name: String)   = OutsideSouth contains name
  def isInSouthVietnam(name: String) = !isOutsideSouth(name)

  // Shortest distance between spaces
  def distanceBetween(origin: String, target: String): Int = {
    def measure(current: String, visited: Set[String]): Option[Int] = {
      if (current == target)
        Some(0)
      else {
        (getAdjacent(current) filterNot visited) match {
          case x if x.isEmpty => None
          case adjacents =>
            val paths = adjacents.toList.map(a => measure(a, visited ++ adjacents)).flatten.sorted
            paths match {
              case Nil    => None
              case x :: _ => Some(1 + x)
            }
        }
      }
    }
    measure(origin, Set.empty).get
  }

  def spacesWithin(numSpaces: Int, origin: String): Set[String] = {

    def adjacentSpaces(spacesLeft: Int, visited: Set[String]): Set[String] = {
      if (spacesLeft > 0) {
        val newVisited = visited.foldLeft(visited) { (v, name) => v ++ getAdjacent(name) }
        adjacentSpaces(spacesLeft - 1, newVisited)
      }
      else
        visited
    }
    adjacentSpaces(numSpaces, Set(origin))
  }

  // Returns the list of spaces with or adjacent to
  // the given space that satisfy the given test.
  def withOrAdjacent(name: String)(test: (Space) => Boolean): List[Space] = {
    spaces(getAdjacent(name) + name) filter test
  }

  def withOrAdjacentExists(name: String)(test: (Space) => Boolean): Boolean = {
    spaces(getAdjacent(name) + name) exists test
  }

  def withOrAdjacentFold[T](name: String, init: T)(func: (T, Space) => T): T = {
    spaces(getAdjacent(name) + name).foldLeft(init)(func)
  }

  def withOrAdjacentTotal(name: String)(func: Space => Int): Int = {
    withOrAdjacentFold(name, 0) { (sum, sp) => sum + func(sp) }
  }

  //  A cube can move onto adjacent LOCs/Cities and keep moving via adjacent LOCs/Cities until
  //  it reaches a space with an insurgent piece.
  //  Return a sequence of all spaces that can be reached from the given space by patrolling cubes.
  def getPatrolDestinations(srcName: String): Set[String] = {

    @tailrec def getDests(candidates: Set[String], destinations: Set[String]): Set[String] = {
      if (candidates.isEmpty)
        destinations
      else {
        val name      = candidates.head
        val sp        = game.getSpace(name)
        val isDest    = name != srcName && (sp.isLoC || sp.isCity)
        val endOfPath = name != srcName && sp.pieces.has(InsurgentPieces)
        val adjacent  = if (endOfPath) Set.empty[String] else (getAdjacentLOCsAndCities(name) - srcName)
        val newDests  = if (isDest) destinations + name else destinations
        val newCandidates = candidates.tail ++ (adjacent -- destinations)
        getDests(newCandidates, newDests)
      }
    }

    getDests(Set(srcName), Set.empty)
  }

    // Used by the Bot code,  we do not include the destName as
  // an origin only spaces where something could move.
  def getPatrolOrigins(destName: String): Set[String] = {

    @tailrec def getTravelOrigins(travelSpaces: Set[String], origins: Set[String]): Set[String] = {
      if (travelSpaces.isEmpty)
        origins
      else {
        val name = travelSpaces.head
        val sp   = game.getSpace(name)

        // Enemy pieces stop movement and no movement through a province
        if (sp.pieces.has(InsurgentPieces) || sp.isProvince)
          getTravelOrigins(travelSpaces.tail, origins + name)
        else {
          val adjacent = getAdjacent(name) - NorthVietnam - destName
          val newTravelSpaces = (travelSpaces.tail ++ adjacent) -- origins
          getTravelOrigins(newTravelSpaces, origins + name)
        }
      }
    }

    // Can always move from any adjacent space
    getTravelOrigins(getAdjacent(destName) - NorthVietnam - destName, Set.empty)
  }


  //  Return a sequence of all spaces that can be reached from the given space by Transported pieces.
  //  A piece MAY move onto an adjacent LOC.  Then can continue moving through adjacent LOC/Cities.
  //  Finally MAY then move to an adjacent space (Never N. Vietnam)
  //  If it ever enters a spcace with any Insurgent piece, then it must stop.
  //  if RVN_Leader_NguyenKhanh is in play then only 1 LOC can be used
  //  Use by the Human code
  def getTransportDestinations(srcName: String): Set[String] = {
    val nguyen_khanh = isRVNLeader(RVN_Leader_NguyenKhanh)

    @tailrec def getLocDests(locsCities: Set[String], destinations: Set[String]): Set[String] = {
      if (locsCities.isEmpty)
        destinations
      else {
        val name = locsCities.head
        val sp   = game.getSpace(name)

        if (sp.pieces.has(InsurgentPieces))  // Enemy pieces stop movement
          getLocDests(locsCities.tail, destinations + name)
        else {
          val adjacentMove = if (nguyen_khanh)
            getAdjacentCities(name)
          else
            getAdjacentLOCsAndCities(name)

          val adjacentDest = if (nguyen_khanh)
            (getAdjacentNonLOCs(name) - NorthVietnam)
          else
            (getAdjacent(name) - NorthVietnam)

            val newLocsCities = locsCities.tail ++ (adjacentMove -- destinations)
            val newDests  = destinations ++ adjacentDest + name
            getLocDests(newLocsCities, newDests)
        }
      }
    }

    // The piece can always stay put or can move to any adjacent Province/City.
    val immediateDests = Set(srcName) ++ (getAdjacentNonLOCs(srcName) - NorthVietnam)
    // The first move must be onto a LOC (not City)
    val locDests = getLocDests(getAdjacentLOCs(srcName), Set.empty)

    immediateDests ++ locDests
  }

  // Used by the Bot code,  we do not include the destName as
  // an origin only spaces where something could move.
  def getTransportOrigins(destName: String): Set[String] = {
    val nguyen_khanh = isRVNLeader(RVN_Leader_NguyenKhanh)

    @tailrec def getTravelOrigins(travelSpaces: Set[String], usedLoC: Boolean, origins: Set[String]): Set[String] = {
      if (travelSpaces.isEmpty)
        origins
      else {
        val name = travelSpaces.head
        val sp   = game.getSpace(name)

        // Enemy pieces stop movement and no movement through a province
        if (sp.pieces.has(InsurgentPieces) || sp.isProvince)
          getTravelOrigins(travelSpaces.tail, usedLoC, origins + name)
        else {
          val adjacentSpaces = if (nguyen_khanh && (usedLoC || sp.isLoC))
            getAdjacentNonLOCs(name) - NorthVietnam - destName
          else if (sp.isLoC)
            getAdjacent(name) - NorthVietnam - destName
          else // it is a city, can only travel here from a LoC
            getAdjacentLOCs(name)

          val newTravelSpaces = (travelSpaces.tail ++ adjacentSpaces) -- origins
          getTravelOrigins(newTravelSpaces, usedLoC || sp.isLoC, origins + name)
        }
      }
    }

    // Can always move from any adjacent space
    getTravelOrigins(getAdjacent(destName) - NorthVietnam, false, Set.empty)
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

  // Used by Bot code
  // Spaces that can reach the destination for Sweeping
  def getSweepOrigins(destName: String): Set[String] = {
    (game.spaces filter (sp => sp.name != destName && adjacentForSweep(sp.name, destName)) map (_.name)).toSet
  }

  // Used by Human code
  def sweepSources(destName: String, faction: Faction, alreadyMoved: MovingGroups): List[Space] = {
    val troopType = if (faction == US) USTroops else ARVNTroops
    game.spaces filter { sp =>
      val movableTroops = sp.pieces - alreadyMoved(sp.name)
      !sp.isNorthVietnam           &&
      sp.name != destName          &&
      movableTroops.has(troopType) &&
      adjacentForSweep(sp.name, destName)
    }
  }

  // `cubeTreatment` is used by events
  //  Return TRUE if any guerrillas were activated
  def activateGuerrillasForSweep(name: String, faction: Faction, cubeTreatment: CubeTreatment, logHeading: Boolean = true): Boolean = {
    val sp = game.getSpace(name)
    val num = sp.sweepActivations(faction, cubeTreatment)
    if (num > 0) {
      if (logHeading) {
        log(s"\nActivating guerrillas in $name")
        log(separator())
      }
      val guerrillas = if (game.isHuman(faction))
        askPieces(sp.pieces, num, UndergroundGuerrillas)
      else
        Bot.selectEnemyRemoveReplaceActivate(sp.pieces.only(UndergroundGuerrillas), num)
      revealPieces(name, guerrillas)
    }
    num > 0
  }

  // Used by events
  def sweepInPlace(name: String, faction: Faction, cubeTreatment: CubeTreatment): Boolean = {
    log()
    log(s"$faction Sweeps in $name")
    log(separator())
    activateGuerrillasForSweep(name, faction, cubeTreatment, logHeading = false)
  }

  // During a sweep operation, if Shaded Booby Traps is in effect
  // VC removes one sweeping troop on a d6 roll of 1-3
  def checkShadedBoobyTraps(name: String, faction: Faction): Boolean = {
    val troopType = if (faction == US) USTroops else ARVNTroops
    val sp        = game.getSpace(name)

    if (capabilityInPlay(BoobyTraps_Shaded) && sp.pieces.has(troopType)) {
      //  Roll a d6.  On a 1-3, remove one of the sweeping factions troops.
      //  ARVN to available, US to casualties
      log(s"\nResolving $BoobyTraps_Shaded in $name")
      log(separator())
      val die = d6
      val success = die < 4
      val status = if (success) "Success" else "Failure"
      log(s"Die roll: $die  [$status]")
      (die < 4) match {
        case true if faction == US => removeToCasualties(name, Pieces(usTroops = 1))
        case true                  => removeToAvailable(name, Pieces(nvaTroops = 1))
        case false                 => log("No troop is removed")
      }
      true
    }
    else
      false
  }

  sealed abstract class PieceType(val name: String) {
    def singular        = name
    def plural          = s"${name}s"
    def genericSingular = singular
    def genericPlural   = s"${genericSingular}s"

    override def toString() = plural
  }

  object PieceType {
    def apply(name: String): PieceType = AllPieceTypes find (_.name == name) getOrElse {
      throw new IllegalArgumentException(s"Invalid name for PieceType: $name")
    }
  }

  case object USTroops       extends PieceType("US Troop")
  case object Irregulars_U   extends PieceType("US Underground Irregular") { override def genericSingular = "US Irregular" }
  case object Irregulars_A   extends PieceType("US Active Irregular") { override def genericSingular = "US Irregular" }
  case object USBase         extends PieceType("US Base")

  case object ARVNTroops     extends PieceType("ARVN Troop")
  case object ARVNPolice     extends PieceType("ARVN Police") { override def plural = name; override def genericPlural = name }
  case object Rangers_U      extends PieceType("ARVN Underground Ranger") { override def genericSingular = "ARVN Ranger" }
  case object Rangers_A      extends PieceType("ARVN Active Ranger") { override def genericSingular = "ARVN Ranger" }
  case object ARVNBase       extends PieceType("ARVN Base")

  case object NVATroops       extends PieceType("NVA Troop")
  case object NVAGuerrillas_U extends PieceType("NVA Underground Guerrilla") { override def genericSingular = "NVA Guerrilla" }
  case object NVAGuerrillas_A extends PieceType("NVA Active Guerrilla") { override def genericSingular = "NVA Guerrilla" }
  case object NVABase         extends PieceType("NVA Base")
  case object NVATunnel       extends PieceType("NVA Tunneled Base")

  case object VCGuerrillas_U  extends PieceType("VC Underground Guerrilla") { override def genericSingular = "VC Guerrilla" }
  case object VCGuerrillas_A  extends PieceType("VC Active Guerrilla") { override def genericSingular = "VC Guerrilla" }
  case object VCBase          extends PieceType("VC Base")
  case object VCTunnel        extends PieceType("VC Tunneled Base")

  val USPieces            = List(USTroops, Irregulars_U, Irregulars_A, USBase)
  val ARVNPieces          = List(ARVNTroops, ARVNPolice, Rangers_U, Rangers_A, ARVNBase)
  val NVAPieces           = List(NVATroops, NVAGuerrillas_U, NVAGuerrillas_A, NVABase, NVATunnel)
  val VCPieces            = List(VCGuerrillas_U, VCGuerrillas_A, VCBase, VCTunnel)
  val CoinPieces          = USPieces:::ARVNPieces
  val InsurgentPieces     = NVAPieces:::VCPieces
  val NonNVAPieces        = USPieces:::ARVNPieces:::VCPieces
  val CoinBases           = List(USBase, ARVNBase)
  val NVABases            = List(NVABase, NVATunnel)
  val VCBases             = List(VCBase, VCTunnel)
  val InsurgentBases      = List(NVATunnel, NVABase, VCTunnel, VCBase)
  val InsurgentNonTunnels = List(NVABase, VCBase)
  val InsurgentTunnels    = List(NVATunnel, VCTunnel)
  val Irregulars          = List(Irregulars_A, Irregulars_U)
  val Rangers             = List(Rangers_A, Rangers_U)
  val NVAGuerrillas       = List(NVAGuerrillas_A, NVAGuerrillas_U)
  val VCGuerrillas        = List(VCGuerrillas_A, VCGuerrillas_U)
  val ARVNCubes           = List(ARVNPolice, ARVNTroops)
  val FlippablePieces     = List(Irregulars_U, Irregulars_A, Rangers_U, Rangers_A,
                                                  NVAGuerrillas_U, NVAGuerrillas_A, VCGuerrillas_U, VCGuerrillas_A)
  val SpecialForces       = Rangers:::Irregulars
  val Guerrillas          = List(NVAGuerrillas_A, VCGuerrillas_A, NVAGuerrillas_U, VCGuerrillas_U)
  val ActiveGuerrillas    = List(NVAGuerrillas_A, VCGuerrillas_A)
  val UndergroundGuerrillas = List(NVAGuerrillas_U, VCGuerrillas_U)
  val CoinForces          = List(USTroops, Irregulars_A, Irregulars_U, ARVNTroops, ARVNPolice, Rangers_A, Rangers_U)
  val USForces            = List(USTroops, Irregulars_A, Irregulars_U)
  val ARVNForces          = List(ARVNTroops, ARVNPolice, Rangers_A, Rangers_U)
  val CoinCubes           = List(USTroops, ARVNTroops, ARVNPolice)
  val CoinTroops          = List(USTroops, ARVNTroops)
  val NVAForces           = List(NVATroops, NVAGuerrillas_A, NVAGuerrillas_U)
  val InsurgentForces     = List(NVATroops, NVAGuerrillas_A, NVAGuerrillas_U, VCGuerrillas_A, VCGuerrillas_U)

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

  def areEnemies(us: Faction, them: Faction) = isCoin(us) != isCoin(them)

  def includesEnemyBase(faction: Faction, pieceTypes: Iterable[PieceType]) = {
    pieceTypes exists { ptype => isBase(ptype) && areEnemies(faction, owner(ptype)) }
  }

  val factionBases: Map[Faction, List[PieceType]] = Map(
    US   -> List(USBase),
    ARVN -> List(ARVNBase),
    NVA  -> NVABases,
    VC   -> VCBases)

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
    case NVABase | NVATunnel               => NVABases
    case VCBase | VCTunnel                 => VCBases
    case _                                 => Seq(pieceType)
  }


  def owner(pieceType: PieceType): Faction = pieceType match {
    case t if USPieces contains t   => US
    case t if ARVNPieces contains t => ARVN
    case t if NVAPieces contains t  => NVA
    case _                          => VC
  }


  def getInsurgentCounterPart(pieceType: PieceType): PieceType = {
    pieceType match {
      case NVAGuerrillas_U => VCGuerrillas_U
      case NVAGuerrillas_A => VCGuerrillas_A
      case NVABase         => VCBase
      case NVATunnel       => VCTunnel
      case VCGuerrillas_U  => NVAGuerrillas_U
      case VCGuerrillas_A  => NVAGuerrillas_A
      case VCBase          => NVABase
      case VCTunnel        => NVATunnel
      case _ => throw new IllegalArgumentException(s"getInsurgentCounterPart called with $pieceType")
    }
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
                               (!has(Guerrillas) && has(InsurgentNonTunnels))

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

    def descriptions = AllPieceTypes filter (totalOf(_) > 0) map (t => amtPiece(totalOf(t), t))

    override def toString() = if (isEmpty) "none" else descriptions.mkString(", ")

    def totalOf(pieceTypes: Iterable[PieceType]): Int =
      pieceTypes.foldLeft(0) { (num, piece) => num + totalOf(piece) }

    // Return true of this Pieces instance contains at least all of the specified pieces.
    def contains(query: Pieces): Boolean = AllPieceTypes forall (t => totalOf(t) >= query.totalOf(t))

    // Return true if this Pieces instance has at least one of the given piece type
    def has(pt: PieceType): Boolean = totalOf(pt) > 0

    // Return true if this Pieces instance has at least one of any of the given piece types
    def has(pts: Iterable[PieceType]): Boolean = totalOf(pts) > 0

    def totalOf(pieceType: PieceType): Int = pieceType match {
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

    def only(pieceTypes: Iterable[PieceType]): Pieces =
      pieceTypes.foldLeft(Pieces()) { (pieces, t) => pieces.add(totalOf(t), t) }

    def only(pieceType: PieceType): Pieces = Pieces().add(totalOf(pieceType), pieceType)

    def except(pieceTypes: Iterable[PieceType]): Pieces =
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
      order flatMap { pieceType => List.fill(totalOf(pieceType))(pieceType) }
    }
  }

  object Pieces {
    def fromTypes(collection: Iterable[PieceType]) =
      collection.foldLeft(Pieces()) { (pieces, t) => pieces.add(1, t) }
  }

  def vulnerableBases(pieces: Pieces, vulnerableTunnels: Boolean) = {
    if (pieces.has(UndergroundGuerrillas))
      Pieces()
    else if (vulnerableTunnels)
      pieces.only(InsurgentBases)
    else
      pieces.only(InsurgentNonTunnels)
  }

  def vulnerableInsurgents(pieces: Pieces, vulnerableTunnels: Boolean) = {
    val forces = pieces.only(NVATroops::ActiveGuerrillas)
    forces + vulnerableBases(pieces, vulnerableTunnels)
  }

  //  Assault firepower of the space plus any modifiers for
  //  capabilities, momentum, etc.
  def usFirepower(cubeTreatment: CubeTreatment)(sp: Space) = {
    val firepower = sp.assaultFirepower(US, cubeTreatment)
    // Account for unshaded Search and Destroy when there would otherwise be zero firepower
    val canSearchDestroy = capabilityInPlay(SearchAndDestroy_Unshaded) &&
                           sp.pieces.has(USTroops) &&
                           sp.pieces.has(UndergroundGuerrillas)
    if (firepower == 0 && canSearchDestroy)
      1
    else
      firepower
  }

  def arvnFirepower(cubeTreatment: CubeTreatment)(sp: Space) =
    sp.assaultFirepower(ARVN, cubeTreatment)

  // If there are no US Troops present then COIN firepower
  // is zero.  This is because COIN firepower is used to determine
  // the number of pieces removed during a US Assault (with added ARVN assault)
  // If no US Troops are present, then there can be no US Assault.
  def coinFirepower(cubeTreatment: CubeTreatment)(sp: Space) = {
    val usPower = usFirepower(cubeTreatment)(sp)
    if (cubeTreatment == AllCubesAsUS || cubeTreatment == AllTroopsAsUS)
      usPower
    else if (usPower > 0)
      usPower + arvnFirepower(NormalTroops)(sp)
    else
      0
  }

  def assaultFirepower(faction: Faction, cubeTreatment: CubeTreatment)(sp: Space): Int = {
    faction match {
      case US => usFirepower(cubeTreatment)(sp)
      case _  => arvnFirepower(cubeTreatment)(sp)
    }
  }

  def canUseM48PattonUnshaded(faction: Faction, name: String): Boolean =
    faction == US && capabilityInPlay(M48Patton_Unshaded) && !game.getSpace(name).isLowland

  // TRUE if any underground guerrillas and all active guerrillas and troops would be killed
  // TRUE if no underground guerrillas and all active guerrillas, troops and bases would be killed
  // FALSE if no active pieces or not all active pieces would be killed
  def assaultKillsAllVulnerable(faction: Faction, cubeTreatment: CubeTreatment, vulnerableTunnels: Boolean, enemies: Set[Faction] = Set(NVA, VC))(sp: Space): Boolean = {
    val enemyPieces = (sp: Space) =>
      enemies.foldLeft(Pieces()) {
        case (pieces, NVA) => pieces + sp.pieces.only(NVAPieces)
        case (pieces, VC)  => pieces + sp.pieces.only(VCPieces)
        case (pieces, _)   => pieces
      }
    val firepower  = assaultFirepower(faction, cubeTreatment)(sp)

    val vulnerable = vulnerableInsurgents(enemyPieces(sp), vulnerableTunnels).total
    (vulnerable > 0) && (firepower >= vulnerable)
  }

  def assaultEffective(faction: Faction,
                       cubeTreatment: CubeTreatment,
                       vulnerableTunnels: Boolean,
                       pattonSpaces: Int = 0,
                       enemies: Set[Faction] = Set(NVA, VC))(sp: Space): Boolean = {
    val enemyPieces = (sp: Space) =>
      enemies.foldLeft(Pieces()) {
        case (pieces, NVA) => pieces + sp.pieces.only(NVAPieces)
        case (pieces, VC)  => pieces + sp.pieces.only(VCPieces)
        case (pieces, _)   => pieces
      }
    val pattonExtra     = if (pattonSpaces < 2 && canUseM48PattonUnshaded(faction, sp.name)) 2 else 0
    val firepower       = assaultFirepower(faction, cubeTreatment)(sp) + pattonExtra
    val enemy           = enemyPieces(sp)
    val asUSAssault     = faction == US || cubeTreatment == AllCubesAsUS || cubeTreatment == AllTroopsAsUS
    val killUnderground = asUSAssault && capabilityInPlay(SearchAndDestroy_Unshaded) && enemy.has(UndergroundGuerrillas)
    val numUnderground  = if (killUnderground) 1 else 0
    val vulnerable      = vulnerableInsurgents(enemy, vulnerableTunnels).total + numUnderground

    (firepower min vulnerable) > 0
  }


  // Used during a turn to keep track of pieces that have already moved
  // in each space.
  // For patrol,  we add pieces to a moving group if they have terminated
  // movement in a space with Insurgent pieces.  This lets us know that
  // those pieces cannot continue moving.
  class MovingGroups() {
    // Map Space Name, to Pieces in that space that cannot move.
    var groups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())

    def reset(): Unit = groups = Map.empty.withDefaultValue(Pieces())
    def apply(name: String): Pieces = groups(name)
    def set(name: String, pieces: Pieces): Unit = groups += name -> pieces
    def add(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) + pieces)
    def remove(name: String, pieces: Pieces): Unit = {
      // If the result removes all pieces then remove the entry
      // from the groups map so it will not be include when toList
      // is called.
      val newPieces = groups(name) - pieces
      if (newPieces.isEmpty)
        groups -= name
      else
        groups += name -> newPieces
    }

    def spaces = groups.keys.toSet
    def toList = groups.toList.sortBy(_._1)
    def allPieces = toList.foldLeft(Pieces()) { (all, group) => all + group._2 }
    def size   = groups.size
  }

  // Convenience function
  // Allows us to use `space.usTroops` in place of `space.pieces.usTroops`
  // implicit def spacePieces(sp: Space): Pieces = sp.pieces

  sealed abstract class SpaceType(val name: String) {
    override def toString() = name
  }

  case object City             extends SpaceType("City")
  case object HighlandProvince extends SpaceType("Highland Province")
  case object LowlandProvince  extends SpaceType("Lowland Province")
  case object JungleProvince   extends SpaceType("Jungle Province")
  case object LoC              extends SpaceType("LOC")

  object SpaceType {
    val ALL       = Set[SpaceType](City, HighlandProvince, LowlandProvince, JungleProvince, LoC)
    def apply(name: String): SpaceType = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SpaceType name: $name")
    }
  }

  sealed abstract class SupportType(val name: String, val value: Int) extends Ordered[SupportType] {
    def compare(that: SupportType) = this.value - that.value
    override def toString() = name
  }
  case object ActiveSupport     extends SupportType("Active Support",     4)
  case object PassiveSupport    extends SupportType("Passive Support",    3)
  case object Neutral           extends SupportType("Neutral",            2)
  case object PassiveOpposition extends SupportType("Passive Opposition", 1)
  case object ActiveOpposition  extends SupportType("Active Opposition",  0)

  object SupportType {
    val ALL = Set[SupportType](Neutral, PassiveSupport, ActiveSupport, PassiveOpposition, ActiveOpposition)
    def apply(name: String): SupportType = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SupportType name: $name")
    }

    def apply(value: Int): SupportType = ALL.find(_.value == value) getOrElse {
      throw new IllegalArgumentException(s"Invalid SupportType value: $value")
    }
  }

  sealed abstract class Control(val name: String) {
    override def toString() = name
  }
  case object Uncontrolled extends Control("Uncontrolled")
  case object CoinControl  extends Control("COIN Control")
  case object NvaControl   extends Control("NVA Control")

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
    def currentEconValue = if (isLoC && terror == 0) printedEconValue else 0

    def isCity     = spaceType == City
    def isProvince = spaceType == HighlandProvince || spaceType == LowlandProvince || spaceType == JungleProvince
    def isLoC      = spaceType == LoC

    def isHighland = spaceType == HighlandProvince
    def isLowland  = spaceType == LowlandProvince
    def isJungle   = spaceType == JungleProvince

    def isNorthVietnam = name == NorthVietnam

    def coinControlled: Boolean = pieces.totalOf(CoinPieces) > pieces.totalOf(InsurgentPieces)
    def nvaControlled: Boolean  = pieces.totalOf(NVAPieces) > pieces.totalOf(NonNVAPieces)
    def control = if      (!isLoC && coinControlled) CoinControl
                  else if (!isLoC && nvaControlled)  NvaControl
                  else                               Uncontrolled

    // Can have support/opposition
    def canHaveSupport = !isLoC && population > 0

    def supportValue: Int =
      support match {
        case PassiveSupport if canHaveSupport => population
        case ActiveSupport  if canHaveSupport => 2 * population
        case _                                => 0
      }

    def oppositionValue: Int =
      support match {
        case PassiveOpposition if canHaveSupport => population
        case ActiveOpposition  if canHaveSupport => 2 * population
        case _                                   => 0
      }

    def coinControlValue: Int = if (!isLoC && coinControlled) population else 0
    def nvaControlValue: Int  = if (!isLoC && nvaControlled)  population else 0

    def totalBases = pieces.totalOf(BasePieces)
    def canTakeBase = !isLoC && totalBases < 2
    def addPieces(newPieces: Pieces): Space = copy(pieces = pieces + newPieces)
    def removePieces(removedPieces: Pieces): Space = copy(pieces = pieces - removedPieces)
    def setPieces(newPieces: Pieces): Space = copy(pieces = newPieces)

    def assaultCubes(faction: Faction, cubeTreatment: CubeTreatment): Int = {
      cubeTreatment match {
        case AllCubesAsUS                     => pieces.totalOf(CoinCubes)
        case AllTroopsAsUS if isCity || isLoC => pieces.totalOf(CoinCubes)
        case AllTroopsAsUS                    => pieces.totalOf(CoinTroops)
        case NormalTroops =>
          faction match {
            case US                      => pieces.totalOf(USTroops)
            case ARVN if isCity || isLoC => pieces.totalOf(ARVNCubes)
            case ARVN                    => pieces.totalOf(ARVNTroops)
            case _                       => 0
         }
      }
    }

    def sweepForces(faction: Faction, cubeTreatment: CubeTreatment): Pieces =
      pieces.only(sweepForceTypes(faction, cubeTreatment))

    def assaultMultiplier(faction: Faction): Double = faction match {
      case US if pieces.has(USBase) => 2.0
      case US if isHighland         => 1.0/2.0
      case US                       => 1.0
      case ARVN if isHighland       => 1.0/3.0
      case ARVN                     => 1.0/2.0
      case _                        => 0.0
    }

    def assaultFirepower(faction: Faction, cubeTreatment: CubeTreatment): Int = {
      val totalCubes = assaultCubes(faction, cubeTreatment)
      val allUS      = cubeTreatment == AllCubesAsUS || cubeTreatment == AllTroopsAsUS
      val multiplier = assaultMultiplier(if (allUS) US else faction)
      (totalCubes * multiplier).toInt
    }

    // The number of underground guerrillas that would
    // be activated by the given faction
    def sweepActivations(faction: Faction, cubeTreatment: CubeTreatment): Int = {
      val numForces   = sweepForces(faction, cubeTreatment).total
      val numActivate = if (isJungle) numForces / 2 else numForces

      numActivate min pieces.totalOf(UndergroundGuerrillas)
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
      case LoC  => 2
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

  val Default_LOC_Hue_KheSanh             = Space(LOC_Hue_KheSanh,             LoC, 1, coastal = true)
  val Default_LOC_Hue_DaNang              = Space(LOC_Hue_DaNang,              LoC, 1, coastal = true)
  val Default_LOC_DaNang_DakTo            = Space(LOC_DaNang_DakTo,            LoC, 0)
  val Default_LOC_DaNang_QuiNhon          = Space(LOC_DaNang_QuiNhon,          LoC, 1, coastal = true)
  val Default_LOC_Kontum_DakTo            = Space(LOC_Kontum_DakTo,            LoC, 1)
  val Default_LOC_Kontum_QuiNhon          = Space(LOC_Kontum_QuiNhon,          LoC, 1)
  val Default_LOC_Kontum_BanMeThuot       = Space(LOC_Kontum_BanMeThuot,       LoC, 1)
  val Default_LOC_QuiNhon_CamRanh         = Space(LOC_QuiNhon_CamRanh,         LoC, 1, coastal = true)
  val Default_LOC_CamRanh_DaLat           = Space(LOC_CamRanh_DaLat,           LoC, 1)
  val Default_LOC_BanMeThuot_DaLat        = Space(LOC_BanMeThuot_DaLat,        LoC, 0)
  val Default_LOC_Saigon_CamRanh          = Space(LOC_Saigon_CamRanh,          LoC, 1, coastal = true)
  val Default_LOC_Saigon_DaLat            = Space(LOC_Saigon_DaLat,            LoC, 1)
  val Default_LOC_Saigon_AnLoc_BanMeThuot = Space(LOC_Saigon_AnLoc_BanMeThuot, LoC, 1)
  val Default_LOC_Saigon_CanTho           = Space(LOC_Saigon_CanTho,           LoC, 2)
  val Default_LOC_CanTho_ChauDoc          = Space(LOC_CanTho_ChauDoc,          LoC, 1)
  val Default_LOC_CanTho_BacLieu          = Space(LOC_CanTho_BacLieu,          LoC, 0, coastal = true)
  val Default_LOC_CanTho_LongPhu          = Space(LOC_CanTho_LongPhu,          LoC, 1, coastal = true)

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

  val SouthVietnam = SpaceNames filter isInSouthVietnam

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
  val Cap_MandateOfHeaven    = "#86 Mandate of Heaven"       // affects ARVN govern
  val Cap_BoobyTraps         = "#101 Booby Traps"            // affects ambush / sweep
  val Cap_MainForceBns       = "#104 Main Force Bns"         // affects insurgent march / VC ambush
  val Cap_Cadres             = "#116 Cadres"                 // affects VC terror and agitate / VC rally agitate


  //  Records a capability that is currently in play
  //  Faction is used by the Bots when makeing event decisions
  case class Capability(name: String, shaded: Boolean, faction: Faction) {
    lazy val flavor = if (shaded) "shaded" else "unshaded"
    override def toString() = s"$name ($flavor)"
    def flip = copy(shaded = !shaded)
  }

  def shadedCapability(name: String, faction: Faction)   = Capability(name, true, faction)
  def unshadedCapability(name: String, faction: Faction) = Capability(name, false, faction)

  val TopGun_Unshaded             = unshadedCapability(Cap_TopGun, US)
  val ArcLight_Unshaded           = unshadedCapability(Cap_ArcLight, US)
  val Abrams_Unshaded             = unshadedCapability(Cap_Abrams, US)
  val Cobras_Unshaded             = unshadedCapability(Cap_Cobras, US)
  val M48Patton_Unshaded          = unshadedCapability(Cap_M48Patton, US)
  val CombActionPlatoons_Unshaded = unshadedCapability(Cap_CombActionPlatoons, US)
  val CORDS_Unshaded              = unshadedCapability(Cap_CORDS, US)
  val LaserGuidedBombs_Unshaded   = unshadedCapability(Cap_LaserGuidedBombs, US)
  val SearchAndDestroy_Unshaded   = unshadedCapability(Cap_SearchAndDestroy, US)
  val AAA_Unshaded                = unshadedCapability(Cap_AAA, NVA)
  val LongRangeGuns_Unshaded      = unshadedCapability(Cap_LongRangeGuns, NVA)
  val MiGs_Unshaded               = unshadedCapability(Cap_MiGs, NVA)
  val SA2s_Unshaded               = unshadedCapability(Cap_SA2s, NVA)
  val PT76_Unshaded               = unshadedCapability(Cap_PT76, NVA)
  val ArmoredCavalry_Unshaded     = unshadedCapability(Cap_ArmoredCavalry, ARVN)
  val MandateOfHeaven_Unshaded    = unshadedCapability(Cap_MandateOfHeaven, ARVN)
  val BoobyTraps_Unshaded         = unshadedCapability(Cap_BoobyTraps, VC)
  val MainForceBns_Unshaded       = unshadedCapability(Cap_MainForceBns, VC)
  val Cadres_Unshaded             = unshadedCapability(Cap_Cadres, VC)

  val AllUnshadedCaps = List(
    TopGun_Unshaded,
    ArcLight_Unshaded,
    Abrams_Unshaded,
    Cobras_Unshaded,
    M48Patton_Unshaded,
    CombActionPlatoons_Unshaded,
    CORDS_Unshaded,
    LaserGuidedBombs_Unshaded,
    SearchAndDestroy_Unshaded,
    AAA_Unshaded,
    LongRangeGuns_Unshaded,
    MiGs_Unshaded,
    SA2s_Unshaded,
    PT76_Unshaded,
    ArmoredCavalry_Unshaded,
    MandateOfHeaven_Unshaded,
    BoobyTraps_Unshaded,
    MainForceBns_Unshaded,
    Cadres_Unshaded
  )


  val TopGun_Shaded               = shadedCapability(Cap_TopGun, US)
  val ArcLight_Shaded             = shadedCapability(Cap_ArcLight, US)
  val Abrams_Shaded               = shadedCapability(Cap_Abrams, US)
  val Cobras_Shaded               = shadedCapability(Cap_Cobras, US)
  val M48Patton_Shaded            = shadedCapability(Cap_M48Patton, US)
  val CombActionPlatoons_Shaded   = shadedCapability(Cap_CombActionPlatoons, US)
  val CORDS_Shaded                = shadedCapability(Cap_CORDS, US)
  val LaserGuidedBombs_Shaded     = shadedCapability(Cap_LaserGuidedBombs, US)
  val SearchAndDestroy_Shaded     = shadedCapability(Cap_SearchAndDestroy, US)
  val AAA_Shaded                  = shadedCapability(Cap_AAA, NVA)
  val LongRangeGuns_Shaded        = shadedCapability(Cap_LongRangeGuns, NVA)
  val MiGs_Shaded                 = shadedCapability(Cap_MiGs, NVA)
  val SA2s_Shaded                 = shadedCapability(Cap_SA2s, NVA)
  val PT76_Shaded                 = shadedCapability(Cap_PT76, NVA)
  val ArmoredCavalry_Shaded       = shadedCapability(Cap_ArmoredCavalry, ARVN)
  val MandateOfHeaven_Shaded      = shadedCapability(Cap_MandateOfHeaven, ARVN)
  val BoobyTraps_Shaded           = shadedCapability(Cap_BoobyTraps, VC)
  val MainForceBns_Shaded         = shadedCapability(Cap_MainForceBns, VC)
  val Cadres_Shaded               = shadedCapability(Cap_Cadres, VC)

  val AllShadedCaps = List(
    TopGun_Shaded,
    ArcLight_Shaded,
    Abrams_Shaded,
    Cobras_Shaded,
    M48Patton_Shaded,
    CombActionPlatoons_Shaded,
    CORDS_Shaded,
    LaserGuidedBombs_Shaded,
    SearchAndDestroy_Shaded,
    AAA_Shaded,
    LongRangeGuns_Shaded,
    MiGs_Shaded,
    SA2s_Shaded,
    PT76_Shaded,
    ArmoredCavalry_Shaded,
    MandateOfHeaven_Shaded,
    BoobyTraps_Shaded,
    MainForceBns_Shaded,
    Cadres_Shaded
  )


  val Medevac_prefix = "#15 Medevac"

  // Momentum markers
  val Mo_WildWeasels       = "#5 Wild Weasels"              // Shaded   (affects Air Strike)
  val Mo_ADSID             = "#7 ADSID"                     // Unshaded (-6 NVA resources at any trail change)
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
    scenarioEntry(new MediumScenario),
    scenarioEntry(new FullScenario)
  )
  val scenarioChoices = scenarios.toList map { case (key, scenario) => key -> scenario.name }

  // Case sensitive
  def isValidScenario(name: String) = scenarios contains name

  sealed trait Operation

  sealed abstract class CoinOp(val name: String) extends Operation {
    override def toString() = name
  }

  // This trait is shared by all Ops and Special Activities
  // that can move pieces and is used by the Bot Movement
  // priorities.
  sealed trait MoveAction

  // This is the move action used when using the Bot's
  // Movement Priorities as part of an event execution
  // that does not specify taking a "free" operation or
  // special activity.
  // `onlyFrom` can be used to limit the origins for
  // the moved pieces.
  case class EventMove(onlyFrom: Option[Set[String]]) extends MoveAction {
    override def toString() = "Event Move"
  }

  // Special MoveAction use by NVA PivotalEvent
  // Allows us to tweak the Move Priorities
  // to Move Max Troops onto LoCs adjacent to Saigon.
  case object EasterTroops extends MoveAction

  case object  Train   extends CoinOp("Train")
  case object  Patrol  extends CoinOp("Patrol") with MoveAction
  case object  Sweep   extends CoinOp("Sweep")  with MoveAction
  case object  Assault extends CoinOp("Assault")


  object CoinOp {
    val ALL = List(Train, Patrol, Sweep, Assault)
  }

  sealed abstract class InsurgentOp(val name: String) extends Operation {
    override def toString() = name
  }
  case object  Rally  extends InsurgentOp("Rally")
  case object  March  extends InsurgentOp("March") with MoveAction
  case object  Attack extends InsurgentOp("Attack")
  case object  Terror extends InsurgentOp("Terror")

  object InsurgentOp {
    val ALL = List(Rally, March, Attack, Terror)
  }

  sealed abstract class Action(val name: String) {
    override def toString() = name
  }
  case object  Event         extends Action("Event")
  case object  OpPlusSpecial extends Action("Op/Special Activity")
  case object  OpOnly        extends Action("Op Only")
  case object  LimitedOp     extends Action("Limited Op")
  case object  Pass          extends Action("Pass")

  object Action {
    val ALL = Set[Action](Event, OpPlusSpecial, OpOnly, LimitedOp, Pass)
    def apply(name: String): Action = ALL find (_.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid action name: $name")
    }
  }

  case class Actor(faction: Faction, action: Action) {
    override def toString() = s"$faction performed $action"
  }

  sealed abstract class SpecialActivity(val name: String) {
    override def toString() = name
  }
  case object Advise     extends SpecialActivity("Advise")
  case object AirLift    extends SpecialActivity("Air Lift") with MoveAction
  case object AirStrike  extends SpecialActivity("Air Strike")
  case object Govern     extends SpecialActivity("Govern")
  case object Transport  extends SpecialActivity("Transport") with MoveAction
  case object Raid       extends SpecialActivity("Raid")
  case object Infiltrate extends SpecialActivity("Infiltrate")
  case object Bombard    extends SpecialActivity("Bombard")
  case object Ambush     extends SpecialActivity("Ambush")
  case object Tax        extends SpecialActivity("Tax")
  case object Subvert    extends SpecialActivity("Subvert")

  val AllSpecials: Set[SpecialActivity] =
    Set(Advise, AirLift, AirStrike, Govern, Transport, Raid,
        Infiltrate, Bombard, Ambush, Tax, Subvert)

  val FactionSpecials: Map[Faction, List[SpecialActivity]] = Map(
    US   -> List(Advise, AirLift, AirStrike),
    ARVN -> List(Govern, Transport, Raid),
    NVA  -> List(Infiltrate, Bombard, Ambush),
    VC   -> List(Tax, Subvert, Ambush))


  // Air Strike params used by Events that carry out
  // air strikes with special rules.
  case class AirStrikeParams(
    maxHits: Option[Int]            = None,  // Overrides d6 roll for hits
    maxHitsPerSpace: Option[Int]    = None,
    canDegradeTrail: Boolean        = true,
    noCoin: Boolean                 = false,     // No Coin pieces needed in space
    designated: Option[Set[String]] = None  // Strike in these spaces
  ) {
    def spaceAllowed(name: String) = {
      (designated map (allowed =>  allowed.contains(name)) getOrElse true)
    }
  }

  case class AirLiftParams(
    onlyTo: Set[String] = Set.empty,
    allowedTypes: Set[PieceType] = Set.empty
  ) {
    def canLiftTo(name: String) = onlyTo.isEmpty || onlyTo(name)
    def allowedType(t: PieceType) = allowedTypes.isEmpty || allowedTypes(t)
    def allowedPieces(pieces: Pieces) = if (allowedTypes.isEmpty)
      pieces
    else
      pieces.only(allowedTypes)
  }

  case class AssaultParams(
    onlyTarget: Option[Faction] = None,      // Chu Luc (unshaded)
    specificSpaces: Set[String] = Set.empty, // Chu Luc (unshaded)
    removeTwoExtra: Boolean     = false      // M48 Patton (unshaded)
  )

  case class MarchParams(
    onlyFrom: Set[String]     = Set.empty,  // If not empty, restricts where pieces can march from.
    onlyTypes: Set[PieceType] = Set.empty,  // Some events specify marching only NVA Troops for example
    amassForBase: Boolean     = true        // Normally the Bots will try to accumulate guerrillas for a
                                            // base, but no with some events.
  ) {
    def canMarchFrom(name: String) = onlyFrom.isEmpty || onlyFrom(name)
  }

  // For some events the Bot will only rally guerrillas (not bases)
  case class RallyParams(guerrillasOnly: Boolean = false)

  case class AmbushParams(
    needUnderground: Boolean = true,
    maxAmbush: Option[Int ]  = None
  )

  case class SweepParams(
    explicitSpaces: Set[String] = Set.empty  // Used by Human only, for bot use Params.onlyIn
  )

  // Some events treat all cubes as US Troops
  // Some event treat all troops as US Troops
  sealed trait CubeTreatment
  case object AllCubesAsUS  extends CubeTreatment
  case object AllTroopsAsUS extends CubeTreatment
  case object NormalTroops  extends CubeTreatment

  def sweepCubeTypes(faction: Faction, cubeTreatment: CubeTreatment): Set[PieceType] = {
    cubeTreatment match {
      case NormalTroops if faction == US => Set(USTroops)
      case NormalTroops                  => Set(ARVNTroops)
      case AllTroopsAsUS                 => Set(USTroops, ARVNTroops)
      case AllCubesAsUS                  => Set(USTroops, ARVNTroops, ARVNPolice)
    }
  }

  def sweepForceTypes(faction: Faction, cubeTreatment: CubeTreatment): Set[PieceType] = {
    val cubeTypes = sweepCubeTypes(faction, cubeTreatment)
    faction match {
        case US   => cubeTypes ++ Irregulars.toSet
        case ARVN => cubeTypes ++ Rangers.toSet
        case _    => Set.empty
    }
  }

  // Parameters used when executing operations and special activities
  // This is used by both the Humand and Bot objects.
  case class Params(
    addSpecialActivity: Boolean  = false, // May add a Special Activity
    specialActivityOnly: Boolean = false, // Used by Bot code when executing events
    maxSpaces: Option[Int]       = None,
    free: Boolean                = false, // Events grant free commands
    onlyIn: Option[Set[String]]  = None,  // Limit command to the given spaces
    event: Boolean               = false,
    airstrike: AirStrikeParams   = AirStrikeParams(),
    airlift: AirLiftParams       = AirLiftParams(),
    sweep: SweepParams           = SweepParams(),
    assault: AssaultParams       = AssaultParams(),
    march:   MarchParams         = MarchParams(),
    rally:   RallyParams         = RallyParams(),
    ambush:  AmbushParams        = AmbushParams(),
    vulnerableTunnels: Boolean   = false,  // Used by events assault/air strike
    cubeTreatment: CubeTreatment = NormalTroops
  ) {
    val limOpOnly = maxSpaces == Some(1)

    def spaceAllowed(name: String) = {
      (onlyIn map (allowed =>  allowed.contains(name)) getOrElse true)
    }
  }

  // The following momentum events prohibit special activities
  // Mo_TyphoonKate     - prohibits air lift, transport, and bombard
  // Mo_Claymores       - prohibits ambush
  // Mo_McNamaraLine    - prohibits infiltrate
  // Mo_Medevac_Shaded  - prohibits air lift
  // Mo_RollingThunder  - prohibits air strike
  // Mo_DaNang          - prohibits air strike
  // Mo_BombingPause    - prohibits air strike

  def prohibitedSpecialActivities(activities: List[SpecialActivity]): List[(String, List[SpecialActivity])] = {
    val events: List[(String, Set[SpecialActivity])] = List(
      Mo_TyphoonKate     -> Set(AirLift, Transport, Bombard),
      Mo_Claymores       -> Set(Ambush),
      Mo_McNamaraLine    -> Set(Infiltrate),
      Mo_Medevac_Shaded  -> Set(AirLift),
      Mo_RollingThunder  -> Set(AirStrike),
      Mo_DaNang          -> Set(AirStrike),
      Mo_BombingPause    -> Set(AirStrike)
    )
    var result: Map[String, List[SpecialActivity]] = Map.empty.withDefaultValue(Nil)

    for (a <- activities; (mo, prohibits) <- events if momentumInPlay(mo) && prohibits(a))
      result += mo -> (a::result(mo))

    result.toList
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
        case Nil        => List(Event, OpOnly, OpPlusSpecial, Pass)
        case first::Nil => first.action match {
          case OpOnly        => List(LimitedOp, Pass)
          case OpPlusSpecial => List(Event, LimitedOp, Pass)
          case Event         => List(OpPlusSpecial, Pass)
          case _ => throw new IllegalStateException("availableActions illegal first action detected")
        }
        case _ => throw new IllegalStateException("availableActions called after two actions taken")
      }
    }

    def location(faction: Faction): String = {
      val actor = actors find (_.faction == faction)

      if (actor.nonEmpty)
        s"${actor.get.action} box"
      else if (eligibleThisTurn(faction))
        "Eligible Factions box"
      else if (passed(faction))
        "Pass box"
      else
        "Ineligible Factions box"
    }

    def canDo(action: Action) = availableActions contains action

    def willBeEligibeNextTurn(faction: Faction) =
      !ineligibleNextTurn(faction) &&
      !(actors exists (_.faction == faction))

    def addActor(faction: Faction, action: Action): SequenceOfPlay = {
      if (actors.size < 2) {
        action match {
          case Pass =>
            copy(passed = passed + faction, eligibleThisTurn = eligibleThisTurn - faction)
          case _ =>
            copy(actors = actors :+ Actor(faction, action), eligibleThisTurn = eligibleThisTurn - faction)
        }
      }
      else
        throw new IllegalStateException("addActor(): Two actors have aready been added")
    }

    def remainEligible(faction: Faction): SequenceOfPlay =
      copy(eligibleNextTurn = eligibleNextTurn + faction, ineligibleNextTurn = ineligibleNextTurn - faction)

    def ineligibleThroughNextTurn(faction: Faction): SequenceOfPlay =
      copy(eligibleThisTurn   = eligibleThisTurn - faction,   // Ineligible now and through next turn
           ineligibleNextTurn = ineligibleNextTurn + faction,
           eligibleNextTurn   = eligibleNextTurn - faction)

    def makeEligible(faction: Faction): SequenceOfPlay =
      copy(eligibleThisTurn  = eligibleThisTurn + faction,
          actors             = actors filterNot (_.faction == faction),
          passed             = passed - faction,
          eligibleNextTurn   = eligibleNextTurn - faction,
          ineligibleNextTurn = ineligibleNextTurn - faction)

    def makeIneligible(faction: Faction): SequenceOfPlay =
        copy(eligibleThisTurn = eligibleThisTurn - faction)

    // Called when two factions have acted, or all have passed
    // Adjusts eligibility for the following turn
    def updateEligibility(): SequenceOfPlay = {
      SequenceOfPlay(eligibleThisTurn = eligibleThisTurn   ++
                                        ineligibleThisTurn ++
                                        passed             ++
                                        eligibleNextTurn   --
                                        ineligibleNextTurn)
    }
  }

  def actorBoxName(action: Action): String = {
    if (action == Pass || game.sequence.numActors == 0)
      action.toString
    else
      game.sequence.actors.head.action match {
        case OpOnly        => "LimOp"
        case OpPlusSpecial => "LimOp or Event"
        case _             => "Op - May add Special Activity"
      }
  }

  //  Move the faction to the eligible box.
  def makeEligible(faction: Faction): Unit = {
    if (!game.sequence.eligibleThisTurn(faction)) {
      log(s"Move the $faction cylinder to the Eligible Factions box")
      game = game.copy(sequence = game.sequence.makeEligible(faction))
    }
  }

  def makeIneligibleThroughNextTurn(faction: Faction, currentlyActing: Boolean): Unit = {
    log(s"\n$faction is ineligible through the next card")
    if (game.sequence.eligibleThisTurn(faction) && !currentlyActing)
      log(s"Move the $faction cylinder to the Ineligible Factions box")

    game = game.copy(sequence = game.sequence.ineligibleThroughNextTurn(faction))
  }

  def remainEligibleNextTurn(faction: Faction): Unit = {
    log(s"\n$faction will remain eligible for the next card")
    game = game.copy(sequence = game.sequence.remainEligible(faction))
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
  case class GameSegment(save_number: Int, card: String, summary: Seq[String])

  //  Note: we calculate the 'Available' pieces by starting with the Force Pool totals
  //        and subtracting pieces on the map, casualties, and out of play.
  case class GameState(
    scenarioName: String,
    humanFactions: Set[Faction],
    cardsPerCampaign: Int,    // Including the Coup! card
    totalCoupCards: Int,   // Total number in the current scenario
    humanWinInVictoryPhase: Boolean,
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
    ongoingEvents: List[String]       = Nil,  // Used to determine which capability/momentum was play most recently
    rvnLeaders: List[String]          = List(RVN_Leader_DuongVanMinh),  // Head of list is current leader
    rvnLeaderFlipped: Boolean         = false,  // Can happen by event #97 Brinks Hotel
    trungDeck: List[TrungCard]        = Nil,  // The head of the list is the top of the deck
    momentum: List[String]            = Nil,
    sequence: SequenceOfPlay          = SequenceOfPlay(),
    currentCard: Int                  = 0,
    onDeckCard: Int                   = 0,
    prevCardWasCoup: Boolean          = false,
    coupCardsPlayed: Int              = 0,    // Number of Coup cards played/ignored thus far
    cardsSeen: List[Int]              = Nil,  // Numbers of cards that have been drawn from the deck
    gameOver: Boolean                 = false,
    peaceTalks: Boolean               = false,
    botDebug: Boolean                 = false,
    logTrung: Boolean                 = true, // Log Trung decisions
    history: Vector[GameSegment]      = Vector.empty,
    log: Vector[String]               = Vector.empty) {  // Log of the cuurent game segment


    lazy val botFactions = Faction.ALL -- humanFactions
    lazy val allPiecesOnMap = spaces.foldLeft(Pieces()) { (total, space) => total + space.pieces }
    lazy val availablePieces = ForcePool - allPiecesOnMap.normalized - casualties.normalized - outOfPlay.normalized
    // piecesToPlace are either on the map or available (not casualties or out of play)
    // Does not include US troops or bases
    lazy val piecesToPlace    = (allPiecesOnMap.normalized + availablePieces).except(USTroops::USBase::Nil)
    lazy val currentRvnLeader = rvnLeaders.head
    lazy val locSpaces        = spaces filter (_.isLoC)
    lazy val nonLocSpaces     = spaces filterNot (_.isLoC)
    lazy val citySpaces       = (spaces filter (_.isCity))
    lazy val patrolSpaces     = locSpaces ::: citySpaces

    lazy val numCardsDrawn    = cardsSeen.size
    // rvnLeaders always contains "Duong Van Minh" which is
    // not a card, hence size - 1
    lazy val numCardsInLeaderBox = rvnLeaders.size - 1

    val agitateTotal = vcResources

    def actionSummary: Seq[String] = {
      val b = new ListBuffer[String]

      if (gameOver) {
        val winner = scores.head.faction
        val player = if (game.isBot(winner)) "Bot" else "Player"
        s"The game has ended with a $winner $player victory"
      }
      else if (isCoupRound) {
        s"${ordinal(coupCardsPlayed + 1)} Coup! round"
      }
      else {
        actingFaction foreach { faction =>
          b += s"${faction} is up"
        }
                
        for (f <- eventDeck(currentCard).factionOrder) {
          sequence.actors find (_.faction == f) foreach { actor =>
            b += actor.toString
          }
          sequence.passed.find(_ == f) foreach { faction =>
            b += s"$faction passed"
          }
        }
      }
      b.toList
    }

    def isHuman(faction: Faction) = humanFactions(faction)
    def isBot(faction: Faction) = !isHuman(faction)

    def trackResources(faction: Faction) = faction match {
      case US    => throw new IllegalArgumentException("trackResources() does not accept US faction as argument")
      case ARVN  => isHuman(US) || isHuman(ARVN)
      case other => isHuman(other)
    }

    // Return faction that is currently the next available or None
    def actingFaction: Option[Faction] = if (sequence.exhausted)
      None
    else
      eventDeck(currentCard).factionOrder find sequence.eligibleThisTurn

    def isActingFaction(faction: Faction) = actingFaction exists (_ == faction)

    // Return faction that would be following the acting faction
    // on the current card.
    def followingFaction: Option[Faction] = actingFaction match {
      case Some(active) if (sequence.numActors == 0) =>
        eventDeck(currentCard).factionOrder filter (_ != active) find sequence.eligibleThisTurn
      case _ => None
    }


    def executingPivotalEvent = eventDeck.isPivotalCard(currentCard) && sequence.numActors == 0

    def isFinalCampaign = coupCardsPlayed == (totalCoupCards - 1)
    def isCoupRound = numCardsDrawn > 0 && eventDeck(currentCard).isCoup
    def onDeckIsCoup = onDeckCard > 0 &&  eventDeck(onDeckCard).isCoup
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
    def tunnelMarkersAvailable      = TunnelMarkerManifest - totalOnMap(_.pieces.totalOf(InsurgentTunnels))
    def totalLOCEcon                = 15 - totalOnMap(_.currentEconValue)

    def usPoints   = totalSupport + availablelUSTroopsAndBases
    def nvaPoints  = totalNvaControl + nvaBasesOnMap
    def arvnPoints = totalCoinControl + patronage
    def vcPoints   = totalOpposition + vcBasesOnMap
    def usScore    = usPoints   - 50
    def nvaScore   = nvaPoints  - 18
    def arvnScore  = arvnPoints - 50
    def vcScore    = vcPoints   - 35

    lazy val scores = List(
      Score(VC,   game.vcPoints, game.vcScore),
      Score(ARVN, game.arvnPoints, game.arvnScore),
      Score(NVA,  game.nvaPoints, game.nvaScore),
      Score(US,   game.usPoints, game.usScore)).sorted

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

  def autoVictory(faction: Faction): Boolean = (game.isBot(faction) || game.humanWinInVictoryPhase) &&
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
  //
  // Returns
  // ( chance of next card being Coup!,
  //   campaign pile that next card belongs to,
  //   number of remaining cards in that campaign
  //   Coup! card status of current campaign (resolved, current, on deck, unresolved)
  // )
  
  def chanceOfDrawingACoupCard: (Double, Int, Int, String) = {
    val coupCardShowing = game.isCoupRound || game.onDeckIsCoup
    
    //  Special case for the start of a game when less than three
    //  cards have been drawn.
    if (game.numCardsDrawn < 3) {
      val cardsRemaining = game.cardsPerCampaign - game.numCardsDrawn
      val chance = if (coupCardShowing) 0.0 else 1.0 / cardsRemaining
      val coupStatus = (game.isCoupRound, game.onDeckIsCoup) match {
        case (true, _)     => "Is current card"
        case (false, true) => "Is upcoming card"
        case _             => "Has not yet been seen"
      }
      
      (chance, 1, cardsRemaining, coupStatus)
    }
    else {
      //  A 'Pile' relates to each pile of cards containing a Coup!
      //  card as defined by the current scenario.
      val cardsPlayed         = game.numCardsDrawn - 2
      val lastPlayedPile      = (cardsPlayed - 1) / game.cardsPerCampaign + 1
      val currentCardPile     = cardsPlayed / game.cardsPerCampaign + 1
      val upcomingCardPile    = (cardsPlayed + 1) / game.cardsPerCampaign + 1
      val topOfDeckPile       = (game.numCardsDrawn / game.cardsPerCampaign) + 1
      val numCoupSeen         = game.coupCardsPlayed +
                                (if (game.isCoupRound)  1 else 0) +
                                (if (game.onDeckIsCoup) 1 else 0)
      val currentCoupResolved = lastPlayedPile == game.coupCardsPlayed && topOfDeckPile == lastPlayedPile
      val cardsRemainingPile  = game.cardsPerCampaign - (game.numCardsDrawn % game.cardsPerCampaign)
      val chance              = if (topOfDeckPile > numCoupSeen) 1.0 / cardsRemainingPile else 0.0
    
      val coupStatus = (game.isCoupRound, game.onDeckIsCoup, currentCoupResolved) match {
        case (true,   _,    _)    => s"${ordinal(currentCardPile)} Coup! is the current card"
        case (false, true,  _)    => s"${ordinal(upcomingCardPile)} Coup! is the upcoming card"
        case (false, false, true) => s"${ordinal(topOfDeckPile)} Coup! has been resolved"
        case _                    => s"${ordinal(topOfDeckPile)} Coup! has not yet been seen"
      }
    
      (chance, topOfDeckPile, cardsRemainingPile, coupStatus)
    }
  }

  def statusSummary: Seq[String] = {
    val scoreLabels: Map[Faction, String] = Map(
      US   -> "US   - Total Support + Avail US",
      ARVN -> "ARVN - COIN Control  + Patronage",
      NVA  -> "NVA  - NVA Control   + NVA Bases",
      VC   -> "VC   - Total Opp.    + VC Bases"
    )
    val padLen = scoreLabels.values.map(_.length).max
    val b = new ListBuffer[String]
    def displayScore(score: Score): String = {
      val label = s"${padLeft(scoreLabels(score.faction), padLen)} : "
      val auto_display = if (autoVictory(score.faction)) "** Auto victory **" else ""
      f"${label}%s${score.points}%2d   (${score.score}%3d)  $auto_display"
    }

    val (coupChance, nextCardCampaign, cardsRemaining, coupStatus) = chanceOfDrawingACoupCard
       
    val coupCardChance = if (game.isFinalCampaign && game.isCoupRound)
      s"0.00%  (Final Coup! round)"
    else
      f"${coupChance*100}%.2f%%  (${amountOf(cardsRemaining, "card")} remaining in ${ordinal(nextCardCampaign)} pile)"

    b += "Game Summary"
    b += separator()
    game.scores foreach { s => b += displayScore(s) }
    b += separator()
    if (game.trackResources(ARVN))
      b += f"ARVN Resources : ${game.arvnResources}%2d"
    if (game.trackResources(NVA))
      b += f"NVA  Resources : ${game.nvaResources}%2d"
    if (game.trackResources(VC))
      b += f"VC   Resources : ${game.vcResources}%2d"
    else
      b += f"VC   Agitate   : ${game.vcResources}%2d"
    b += separator()
    if (game.trackResources(ARVN)) {
      b += f"Econ           : ${game.econ}%2d"
    }
    b += f"US Aid         : ${game.usAid}%2d"
    b += f"Patronage      : ${game.patronage}%2d"
    b += separator()
    b += f"Trail          : ${game.trail}%2d"
    b += f"Terror (avail) : ${game.terrorMarkersAvailable}%2d"
    if (game.isBot(US))
      b += s"US Policy      : ${game.usPolicy}"

    b += separator()
    val leaderFlip = if (game.rvnLeaderFlipped) " (flipped face down)" else ""
    b += s"RVN Leader     : ${game.currentRvnLeader}$leaderFlip"
    if (game.rvnLeaders.size > 1) {
      wrap("Past Leaders   : ", game.rvnLeaders.tail) foreach (b += _)
      b += separator()
    }
    if (game.numCardsDrawn > 0) {
      b += s"Current card   : ${eventDeck(game.currentCard).fullString}"
      if (game.onDeckCard > 0)
        b += s"On Deck card   : ${eventDeck(game.onDeckCard).fullString}"
      else
        b += "On Deck card   : None (Last Coup round)"
    }

    b += separator()
    b += s"Campaign       : ${ordinal(game.coupCardsPlayed+1)} of ${game.totalCoupCards} ${if (game.isFinalCampaign) " -- Final campaign" else ""}"
    b += f"Cards/Pile     : ${game.cardsPerCampaign}%2d"
    b += f"Cards drawn    : ${game.numCardsDrawn}%2d"
    b += s"Coup card      : $coupStatus"
    b += f"Coup chance    : $coupCardChance"

    if (game.humanFactions.nonEmpty) {
      val desc = if (game.humanWinInVictoryPhase)
        "Allowed during Victory phase of any Coup! round"
      else
        "Not allowed until final Coup! round"
      b += separator()
      b += s"Human Victory  : $desc"      
    }

    b.toList
  }

  def availablePiecesSummary: Seq[String] = {
    val b = new ListBuffer[String]
    val avail = game.availablePieces

    def addPieces(types: Iterable[PieceType]): Unit = {
      if (avail.has(types))
        b += separator()
      for (t <- types; name = t.genericPlural; count = avail.totalOf(t))
        b += f"${name}%-15s: ${count}%2d"
    }

    b += "Available Pieces"
    if (game.availablePieces.isEmpty) {
      b += separator()
      b += "None"
    }
    else {
      addPieces(USTroops::Irregulars_U::USBase::Nil)
      addPieces(ARVNTroops::ARVNPolice::Rangers_U::ARVNBase::Nil)
      addPieces(NVATroops::NVAGuerrillas_U::NVABase::Nil)
      addPieces(VCGuerrillas_U::VCBase::Nil)
    }
    b.toList
  }

  def casualtiesSummary: Seq[String] = {
    val b = new ListBuffer[String]

    def addPieces(types: Iterable[PieceType]): Unit = {
      if (game.casualties.has(types))
        b += separator()
      for (t <- types; name = t.genericPlural; count = game.casualties.totalOf(t) if count > 0)
        b += f"${name}%-15s: ${count}%2d"
    }
    b += "Casualties"
    if (game.casualties.isEmpty) {
      b += separator()
      b += "None"
    }
    else
      addPieces(USPieces)
      addPieces(ARVNPieces)
    b.toList
  }

  def outOfPlaySummary: Seq[String] = {
    val b = new ListBuffer[String]

    def addPieces(types: Iterable[PieceType]): Unit = {
      if (game.outOfPlay.has(types))
        b += separator()
      for (t <- types; name = t.genericPlural; count = game.outOfPlay.totalOf(t) if count > 0)
        b += f"${name}%-15s: ${count}%2d"
    }
    b += "Out of Play"
    if (game.outOfPlay.isEmpty) {
      b += separator()
      b += "None"
    }
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
      val pt = if (game.peaceTalks && faction == US) ", Peace Talks" else ""
      if (game.pivotCardsAvailable(faction))
        s"$faction (available$pt)"
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

  def sequenceList(card: EventCard, sequence: SequenceOfPlay): Seq[String] = {
    def actorDisp(faction: Faction) = if (sequence.eligibleNextTurn(faction))
      s"${faction}(+)"
      else if (sequence.ineligibleNextTurn(faction))
      s"${faction}(-)"
    else
      faction.toString
    val b = new ListBuffer[String]
    b += s"Current card  : ${card.fullString}"
    if (!card.isCoup) {
      val actors     = sequence.actors map (a => s"${actorDisp(a.faction)} -> ${a.action.name}")
      val eligible   = card.factionOrder filter sequence.eligibleThisTurn   map (_.name)
      val ineligible = card.factionOrder filter sequence.ineligibleThisTurn map (_.name)
      val passed     = card.factionOrder filter sequence.passed     map (_.name)
      wrap("Eligible      : ", eligible) foreach (b += _)
      wrap("Acted         : ", actors) foreach (b += _)
      wrap("Passed        : ", passed) foreach (b += _)
      wrap("Ineligible    : ", ineligible) foreach (b += _)
    }
    b.toList
  }

  def sequenceSummary: Seq[String] = {
    val b = new ListBuffer[String]
    if (game.numCardsDrawn > 0) {
      b += "Sequence of Play"
      b += separator()
      b ++= sequenceList(eventDeck(game.currentCard), game.sequence)
    }
    b.toList
  }


  def spaceSummary(name: String, prefix: String = ""): Seq[String] = {
    val b = new ListBuffer[String]
    val sp = game.getSpace(name)
    b += ""
    b += s"${prefix}${sp.name} (${sp.spaceType})"
    b += separator()
    if (sp.isLoC)
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

  def spaceNames(spaces: Iterable[Space]): List[String] = (spaces map (_.name)).to(List).sorted(SpaceNameOrdering)
  def spaces(names: Iterable[String]): List[Space] = (names map game.getSpace).to(List).sortBy(_.name)(SpaceNameOrdering)

  // We assume that the current working directory
  // set as the installed directory and thus the game directory
  // is in ./games.  The script used to invoke the program will
  // ensure that is the case.
  val gamesDir = Pathname("./games")
  var gameName: Option[String] = None // The name of sub-directory containing the game files

  // Ask the user for a name for their new game.
  def askGameName(prompt: String): String = {
    val VALID_NAME = """([-A-Za-z0-9_ ]+)""".r
    def getName: String = {
      readLine(prompt) match {
        case null => getName
        case VALID_NAME(name) =>
          if ((gamesDir/name).exists) {
            println(s"\nA game called '$name' already exists:")
            println(separator())
            println(loadGameDescription(name))
            if (askYorN(s"\nDo you want to overwrite the existing game (y/n)? ")) {
              (gamesDir/name).rmtree()
              name
            }
            else
              getName
          }
          else
            name
        case name =>
          println("The name must consist of one or more letters, numbers, spaces, dashes or undercores")
          getName
      }
    }
    getName
  }

  case object ExitGame    extends Exception
  case object AbortAction extends Exception
  case object Rollback    extends Exception
  case object Adjustment  extends Exception

  def main(args: Array[String]): Unit = {
    val versionSuffix = if (SOFTWARE_VERSION.startsWith("0")) " - BETA" else ""
    val versionDisplay = s"Fire in the Lake: Trung Bot Software (version $SOFTWARE_VERSION$versionSuffix)"
    val versionFlags = Set("-v", "-version", "--version", "version")
    
    if (args.nonEmpty && versionFlags.contains(args(0).toLowerCase)) {
      println(versionDisplay)
      System.exit(0)      
    }
    
    println()
    println(versionDisplay)
    
    println(separator())
    try {
      gamesDir.mkpath()

      askWhichGame() match {
        case Some(name) =>
          val save_number = mostRecentSaveNumber(name) getOrElse {
            throw new IllegalStateException(s"No saved file found for game '$name'")
          }
          loadGameState(name, save_number)

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
          val usePeriodEvents = scenario.periodCapabilities.nonEmpty && askYorN("\nAre you using period events? (y/n) ")
          val humanFactions: Set[Faction] = {
            val num = askInt("\nHow many factions will be played by human players", 0, 4, Some(1), allowAbort = false)
            def nextHuman(humans: Set[Faction]): Set[Faction] = {
              if (humans.size == num)
                humans
              else
              {
                val remaining = Faction.ALL -- humans
                val x = humans.size + 1
                val faction   = askFaction(s"Select the ${ordinal(x)} human faction:", remaining, allowAbort = false)
                nextHuman(humans + faction)
              }
            }

            num match {
              case 4 => Faction.ALL
              case 0 => Set.empty
              case 1 => Set(askFaction("Select the human faction:", allowAbort = false))
              case n => nextHuman(Set.empty)
            }
          }

          if (humanFactions.size == 1) {
            println()
            println("When playing solitaire the human faction normally may only win during the final Coup! round.")
            println("For an easier game you can allow the human faction to win during any Coup! round.")
          }
          else if (humanFactions.size > 1) {
            println()
            println("When playing with multiple human factions, normally any faction may win during")
            println("the Victory phase of any Coup! round.")
            println("For a harder game you can force the human factions to win only during the final Coup! round.")
          }
          
          val humanCanWinEarly = if (humanFactions.nonEmpty)
            askYorN(s"\nAllow human ${pluralize(humanFactions.size, "faction")} to win in Victory phase of any Coup round? (y/n) ");
          else
            false

          println()
          gameName = Some(askGameName("Enter a name for your new game: "))

          game = initialGameState(scenario, humanFactions, usePeriodEvents, humanCanWinEarly)

          log()
          log("Start of Game")
          log(separator(char = '='))
          logSummary(scenarioSummary)
          log()
          scenario.additionalSetup()

          //  If VC is a Bot then we use the vcResources as the Agitate Total
          //  This is initialized by rolling a d3
          if (game.isBot(VC))
            initAgitateTotal()

          // Ask user for the first two event cards
          // and create the initial save point
          drawNextCard()
          saveGameState()
      }

      mainLoop()
    }
    catch {
      case ExitGame =>
    }
  }


  def initialGameState(scenario: Scenario, humanFactions: Set[Faction], usePeriodCapabilities: Boolean, humanCanWinEarly: Boolean) = {
    val trungDeck = shuffle(TrungDeck filterNot (card => humanFactions(card.faction)))
    var spaces    = DefaultSpaces

    // Apply scenario overrides to countries.
    for (sp <- scenario.spaces)
      spaces = sp :: (spaces filterNot (_.name == sp.name))

    val (capabilities, ongoingEvents) = if (usePeriodCapabilities)
      (scenario.periodCapabilities, scenario.periodCapabilities map (_.name))
    else
      (Nil, Nil)

    GameState(
      scenario.name,
      humanFactions,
      scenario.cardsPerCampaign,
      scenario.totalCoupCards,
      humanCanWinEarly,
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
      capabilities,
      ongoingEvents,
      scenario.rvnLeadersInPlay,
      false,
      trungDeck
    )
  }


    // Save a brief description of the game.
  // The descriptions are used by the askWhichGame() function.
  def saveGameDescription(): Unit = {
    assert(gameName.nonEmpty, "saveGameDescription(): called with gameName not set!")
    val summary = game.actionSummary.headOption getOrElse ""
    val desc = s"${game.scenarioName}  [${eventDeck(game.currentCard)}] $summary"
    val path = gamesDir/gameName.get/"description"

    path.writeFile(desc)
  }

  def loadGameDescription(name: String): String = {
    val path = gamesDir/name/"description"
    if (path.exists)
      path.readFile().trim
    else
      ""
  }

  def saveGameState(desc: String = ""): Unit = {
    assert(gameName.nonEmpty, "saveGameState(): called with gameName not set!")

    val save_number = game.history.size
    val save_path   = gamesDir/gameName.get/getSaveName(save_number)
    val log_path    = gamesDir/gameName.get/getLogName(save_number)
    val summary = if (desc.isEmpty)
      game.actionSummary
    else
      game.actionSummary :+ desc
    val segment = GameSegment(save_number, eventDeck(game.currentCard).toString, summary)

    // Make sure that the game directory exists
    save_path.dirname.mkpath()
    log_path.writeFile(game.log.mkString("", lineSeparator, lineSeparator))
    game = game.copy(log = Vector.empty, history = game.history :+ segment)
    SavedGame.save(save_path, game)
    saveGameDescription()
  }

    // Load the most recent game file for the given game.
  def loadGameState(name: String, save_number: Int): Unit = {
    val save_path = gamesDir/name/getSaveName(save_number)
    gameName = Some(name)
    game = SavedGame.load(save_path).copy(log = Vector.empty)
  }


  def getSaveFileNumber(filename: Pathname): Option[Int] = {
    val SAVE_FILE = """save-(\d+)""".r
    filename.basename.toString match {
      case SAVE_FILE(n) => Some(n.toInt)
      case _            => None
    }
  }

  def getSaveName(save_number: Int) = f"save-$save_number%03d"
  def getLogName(save_number: Int)  = f"log-$save_number%03d"

  // Given a directory for a saved game finds the most recent save file.
  def mostRecentSaveNumber(name: String): Option[Int] = {
    val dir = gamesDir/name
    if (dir.isDirectory) {
      val entries = dir.children(withDirectory = false) flatMap { child =>
        getSaveFileNumber(child)
      }
      entries.sortBy(num => -num).headOption
    }
    else
      None
  }

    // Return the list of saved games
  def savedGames: List[String] = {
    gamesDir.children(withDirectory = false).toList map (_.toString) filter { name =>
      mostRecentSaveNumber(name).nonEmpty
    }
  }

  def debug(msg: String) =  println(s"DEBUG - $msg")

    // Ask which saved game the user wants to load.
  // Return None if they wish to start a new game.
  def askWhichGame(): Option[String] = {
    val games = savedGames
    if (games.isEmpty)
      None
    else {
      val gameChoices = games map { name =>
        val desc = loadGameDescription(name)
        val suffix = if (desc == "") "" else s", $desc"
        name -> s"Resume '$name'$suffix"
      }
      val choices = ("--new-game--" -> "Start a new game") :: gameChoices ::: List("--quit-game--" -> "Quit")
      println()
      askMenu(choices, "Which game would you like to play:", allowAbort = false).head match {
        case "--new-game--"  => None
        case "--quit-game--" => throw ExitGame
        case name            => Some(name)
      }
    }
  }


  trait Command {
    val name: String
    val desc: String
  }

  object ActCmd extends Command {
    val name = "act"
    val desc = "Take an action on the current card"
  }

  object BotCmd extends Command {
    val name = "bot"
    val desc = s"The Bot acts on the current card"
  }

  object CoupCmd extends Command {
    val name = "coup"
    val desc = s"Resolve the Coup! Round"
  }

  object ShowCmd extends Command {
    val name = "show"

    val desc = """|Display the current game state
                  |  show scenario  - name of the current scenario
                  |  show summary   - current score, resources, etc.
                  |  show pieces    - available pieces, casualties, out of play pieces
                  |  show events    - capabilities, momentum, pivotal events
                  |  show all       - entire game state
                  |  show <space>   - state of a single space""".stripMargin

  }

  object HistoryCmd extends Command {
    val name = "history"
    val desc = """|Display game history
                  |  history            - Shows the log starting from the most recent save point (Same as history -1)
                  |  history -n         - Shows the log starting from the nth most recent save point
                  |  history n          - Shows the log starting from the nth save point
                  |  history -n num     - Shows num log entries starting from the nth most recent save point
                  |  history n num      - Shows num log entries starting from the nth save point
                  |  history all        - Shows the entire log (Same as history 0)
                  |  You may add >file to the end of any history command to write the history to disk.""".stripMargin

  }

  object AdjustCmd extends Command {
    val name = "adjust"
    val desc = """|Adjust game settings  (Minimal rule checking is applied)
                  |  adjust aid             - US Aid level
                  |  adjust patronage       - ARVN Patronage
                  |  adjust resources       - Faction resources
                  |  adjust econ            - Econ marker value
                  |  adjust trail           - Trail value
                  |  adjust uspolicy        - Current US Policy
                  |  adjust casualties      - Pieces in the Casualties box
                  |  adjust out of play     - Pieces in the Out of Play box
                  |  adjust on deck card    - Change the on deck card
                  |  adjust capabilities    - Capabilities currently in play
                  |  adjust momentum        - Momentum events currently in play
                  |  adjust rvnLeaders      - Stack of RVN Leaders
                  |  adjust pivotal         - Adjust available Pivotal event cards
                  |  adjust trung           - Adjust Trung deck
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

  // Prompt the user for one or two cards if necessary.
  // Then update the game state with the new card numbers.
  def drawNextCard(): Unit = {
    if (game.numCardsDrawn == 0) {
      println("\nDraw event cards")
      println(separator())
      val card1 = askCardNumber("Enter the number of the 1st Event card: ")
      game = game.copy(cardsSeen = game.cardsSeen :+ card1)
      val card2 = askCardNumber("Enter the number of the 2nd Event card: ")
      game = game.copy(currentCard  = card1,
                       onDeckCard   = card2,
                       cardsSeen    = game.cardsSeen :+ card2)
    }
    else {
      // If Coup round then all faction cylinders have already been
      // moved to the eligible box
      if (!game.isCoupRound)
        updateFactionEligibility(false)
      
      if ((game.coupCardsPlayed == game.totalCoupCards - 1 && game.onDeckCard > 0 && eventDeck(game.onDeckCard).isCoup)) {
        //  Last Coup card is on deck, so no need to draw a card
        game = game.copy(currentCard     = game.onDeckCard,
                         onDeckCard      = 0,
                         prevCardWasCoup = game.isCoupRound)
      }
      else {        
        println("\nDraw event card")
        println(separator())
        val nextCard = askCardNumber("Enter the number of the next On Deck Event card: ")
        game = game.copy(currentCard     = game.onDeckCard,
                         onDeckCard      = nextCard,
                         prevCardWasCoup = game.isCoupRound,
                         cardsSeen       = game.cardsSeen :+ nextCard)
      }
    }

    log()
    log(s"Current card: ${eventDeck(game.currentCard)}")
    if (game.onDeckCard > 0)
      log(s"On Deck card: ${eventDeck(game.onDeckCard)}")
    else
      log("On Deck card: None (Last Coup round)")
  }


  def showScores(header: String): Unit = {

    val winner = game.scores.head.faction

    log()
    log(separator(char = '='))
    log(header)
    log(s"$winner wins!")
    log(separator(char = '='))
    log()
    log("Victory Margins")
    log(separator())
    for (Score(faction, _, score) <- game.scores)
      log(f"${padLeft(faction, 4)}  ${score}%+3d")
  }

  // Resolve Coup Round
  def resolveCoupCard(): Unit = {
    val coupCard         = eventDeck(game.currentCard)
    val isFailedCoup     = game.currentCard == 129 || game.currentCard == 130
    val skipCoupRound    = game.prevCardWasCoup
    val newLeader        = rvnLeaderForCard(game.currentCard)
    val isFinalCoupRound = game.coupCardsPlayed == game.totalCoupCards - 1

    def placeLeader(): Unit = {
      if (game.rvnLeaders.size == 1) {
        log(s"\nPlace $coupCard in the RVN Leader Box")
        game = game.copy(rvnLeaders = newLeader::game.rvnLeaders, rvnLeaderFlipped = false)
      }
      else if (isFailedCoup) {
        log(s"\nPlace $coupCard beneath the other cards in the RVN Leader Box")
        // Place above RVN_Leader_DuongVanMinh which is printed on the map
        val newLeaders = game.rvnLeaders.init:::List(newLeader, game.rvnLeaders.last)
        game = game.copy(rvnLeaders = newLeaders, rvnLeaderFlipped = false)
      }
      else {
        log(s"\nPlace $coupCard in the RVN Leader Box on top of the stack")
        game = game.copy(rvnLeaders = newLeader::game.rvnLeaders, rvnLeaderFlipped = false)
      }
    }

    log(s"Coup! Card: $coupCard")
    log(separator())

    if (isFailedCoup)
      coupCard.executeUnshaded(ARVN)  // ARVN Desertion

    // Place the leader card in the Leader Box
    placeLeader()

    val gameOver = if (game.prevCardWasCoup) {
      //  We just drew a Coup! card immediately after a Coup round.
      //  We resolve the RVN Leader and any immediate effects from
      //  the Coup event,  but we do not conduct a Coup Round.
      log("\nSkipping this Coup! round (two Coup! cards in a row)")
      log(separator())

      if (isFinalCoupRound) {
        showScores("Game over after final Coup! Card")
        true
      }
      else
        false // Game has not ended
    }
    else {
      var gameEnded = false
      log(s"\nConduct a Coup! Round")
      log(separator())

      log(s"\nVictory Phase")
      log(separator(char = '='))
      val leader = game.scores.head
      //  During Victory phase a Bot can always win
      //  Human can win only if not a solataire game
      if (leader.score > 0 && (game.isBot(leader.faction) || game.humanWinInVictoryPhase)) {
        val coupRound = game.coupCardsPlayed + 1
        showScores(s"Game over in the ${ordinal(coupRound)} Coup! round")

        //  Allow the user to continue even though a Bot achieved victory
        //  Many people like to play it out until the end of the final Coup round.
        gameEnded = !askYorN("\nDo you want to continue playing this game? (y/n)")
      }
      else
        log("None of the Bots has achieved its victory condition")

      if (!gameEnded) {
        coupResourcesPhase()
        coupSupportPhase()
        coupRedeployPhase()

        if (isFinalCoupRound) {
          showScores("Game over in final Coup! Round")
          gameEnded = true
        }
        else {
          coupCommitmentPhase()
          coupResetPhase()
        }
      }
      gameEnded
    }

    game = game.copy(coupCardsPlayed = game.coupCardsPlayed + 1, gameOver = gameOver)
  }

  // Resources Phase of Coup Round
  // Mo_Oriskany - Shaded (prohibits degrade of trail) (includes air strike, coup round, NOT evnts!)
  def coupResourcesPhase(): Unit = {
    val sabotagePriorities = List(Bot.HighestEconValue)
    val canSabotage = (sp: Space) =>
      sp.terror == 0 &&
      (sp.pieces.totalOf(Guerrillas) > sp.pieces.totalOf(CoinPieces) ||
       (spaces(getAdjacentCities(sp.name)) exists (!_.coinControlled)))

    def sabotageCandidates = game.locSpaces filter canSabotage

    def nextBotSabotage(): Unit = {
      val candidates = sabotageCandidates
      if (game.terrorMarkersAvailable > 0 && candidates.nonEmpty) {
        val sp = Bot.bestCandidate(candidates, sabotagePriorities)
        addTerror(sp.name, 1)
        nextBotSabotage()
      }
    }

    def nextHumanSabotage(): Unit = {
      val candidates = sabotageCandidates
      val numMarkers = game.terrorMarkersAvailable
      if (numMarkers > 0 && candidates.nonEmpty) {
        val choices = candidates.map(sp => sp.name -> sp.name)
        val prompt  = s"\nChoose LoC to sabotage (${amountOf(numMarkers, "sabotage marker")} available):"
        val name = askMenu(choices, prompt).head
          addTerror(name, 1)
          nextHumanSabotage()
      }
    }

    log("\nResources Phase")
    log(separator(char = '='))

    // Sabotage Locs --------------------------------------
    log("\nSabotage LoCs")
    log(separator())
    if (sabotageCandidates.isEmpty)
      log("There are no LoCs that can be sabotaged")
    else if (game.terrorMarkersAvailable == 0)
      log("There are no sabotage markers available")
    else {
      // If there are enough terror markers to sabotage all candidates
      // then we just place them all.
      // Otherwise we allow the VC faction to choose where they go.
      if (game.terrorMarkersAvailable >= sabotageCandidates.size)
        nextBotSabotage()
      else {
        log("\nThere are not enough sabotage markers for all LoCs")
        log("VC will choose which LoCs are sabotaged")
        if (game.isBot(VC))
          nextBotSabotage()
        else
          nextHumanSabotage()
      }
    }
    pause()

    // Degrade Trail --------------------------------------
    log("\nDegrade the Trail")
    log(separator())
    if (!spaces(LaosCambodia).exists(_.coinControlled))
      log("There are no COIN controlled spaces in Laos or Cambodia")
    else if (game.trail == TrailMin)
      log("The trail is already at the 1 box.")
    else if (momentumInPlay(Mo_Oriskany))
      log(s"Degrading the Trail is prohibity by Momentum: $Mo_Oriskany")
    else
      degradeTrail(1)
    pause()

    // ARNV EArnings  -------------------------------------
    log("\nARVN Earnings")
    log(separator())
    val newEcon = (game.locSpaces map (_.currentEconValue)).sum
    setEconValue(newEcon)
    if (game.trackResources(ARVN))
      increaseResources(ARVN, game.usAid + newEcon)
    else
      log("ARVN resources are not being tracked")
    pause()

    val vcBasesOnMap  = game.totalOnMap(_.pieces.totalOf(VCBases))
    val nvaBasesLaosCambodia = (game.nonLocSpaces filter (sp => isInLaosCambodia(sp.name)) map (_.pieces.totalOf(NVABases))).sum
    log("\nInsurgent Earnings")
    log(separator())
    if (game.trackResources(VC) && vcBasesOnMap > 0)
      increaseResources(VC, vcBasesOnMap)
    else
      log("VC resources are not being tracked")

    if (game.trackResources(NVA))
      increaseResources(NVA, nvaBasesLaosCambodia + (2 * game.trail))
    else
      log("NVA resources are not being tracked")
    pause()

    log("\nCasualties and Aid")
    log(separator())
    if (game.casualties.isEmpty)
      log("There are no pieces in the Casualties Box")
    else
      decreaseUsAid(game.casualties.total * 3)
    pause()
  }

  def pacifyCandidate(faction: Faction)(sp: Space) = {
    val maxSupport = if (game.isHuman(faction) || faction == US) ActiveSupport else PassiveSupport
    val troops     = if (faction == US) USTroops else ARVNTroops
    sp.canHaveSupport         &&
    sp.coinControlled         &&
    sp.support < maxSupport   &&
    sp.pieces.has(ARVNPolice) &&
    sp.pieces.has(troops)
  }

  def pacifyCost = if (game.trackResources(ARVN)) {
    if (isRVNLeader(RVN_Leader_NguyenCaoKy))    4
    else if (momentumInPlay(Mo_BlowtorchKomer)) 1
    else                                        3
  }
  else 0

  case class SupportParams(
    forEvent: Boolean      = false,
    free: Boolean          = false,
    maxLevels: Int         = 2,
    factions: Set[Faction] = Set(US, ARVN, VC)
  )
  // Support Phase of Coup Round
  // RVN_Leader_NguyenCaoKy - US/ARVN pacification costs 4 resources per Terror/Level
  // Mo_BlowtorchKomer      - Pacify costs 1 resource per step/terror, during Support phase
  // MandateOfHeaven_Shaded - ARVN Pacify is maximum 1 space (instead of 4)
  // Cadres_Unshaded        - VC to Agigate must remove 2 VC guerrillas per space (or not possible there)
  def coupSupportPhase(params: SupportParams = SupportParams()): Unit = {
    // The leader would have just been played and thus take precedence
    val nguyenCaoKy = game.trackResources(ARVN) && isRVNLeader(RVN_Leader_NguyenCaoKy)
    val blowtorch   = game.trackResources(ARVN) && momentumInPlay(Mo_BlowtorchKomer) && !nguyenCaoKy
    val mandate     = capabilityInPlay(MandateOfHeaven_Shaded)
    val cadres      = capabilityInPlay(Cadres_Unshaded)
    val pacifyNotes = List(
      noteIf(nguyenCaoKy, "US/ARVN pacification cost 4 resources per Terror/Level"),
      noteIf(blowtorch, "Pacification costs 1 resource per step/terror"),
      noteIf(mandate, "ARVN Pacify is maximum 1 space (instead of 4)")
    ).flatten
    val agitateNotes = List(
      noteIf(cadres, "VC to Agigate must remove 2 VC guerrillas per space")
    ).flatten

    val agitateCost    = if (params.free) 0 else if (nguyenCaoKy) 4 else 1
    var botPoints      = rollDice(2)  // When both US and ARVN are Bots
    var agitateSpaces  = Set.empty[String]
    val cadresUnshaded = capabilityInPlay(Cadres_Unshaded)
    var usSpaces       = Set.empty[String]
    var arvnSpaces     = Set.empty[String]

    def pacifySpaces   = usSpaces ++ arvnSpaces
    def recordSpace(faction: Faction, name: String): Unit =
      if (faction == US)
        usSpaces += name
      else
        arvnSpaces += name

    def isCandidate(faction: Faction)(sp: Space) = !pacifySpaces(sp.name) && pacifyCandidate(faction)(sp)

    def canPacify(faction: Faction) = {      
      val haveResources = if (game.trackResources(ARVN)) {
        val lowerLimit = if (faction == US) game.econ else 0
        (game.arvnResources - pacifyCost) >= lowerLimit
      }
      else
        botPoints > 0
      
      pacifySpaces.size < 4 &&
      haveResources &&
      (faction == US || arvnSpaces.isEmpty || mandate == false )
    }
    
    def pacify(faction: Faction): Unit = {
      val candidates = game.nonLocSpaces filter isCandidate(faction)

      if (canPacify(faction) && candidates.nonEmpty) {
        if (game.isBot(faction)) {
          val sp = if (faction == US)
            Bot.US_Bot.pickSpaceTowardActiveSupport(candidates)
          else
            Bot.ARVN_Bot.pickSpaceTowardPassiveSupport(candidates)
          botPoints -= Bot.pacifySpace(sp.name, faction, coupRound = true, coupPoints = botPoints, params.free, params.maxLevels)
          recordSpace(faction, sp.name)
          pacify(faction)
        }
        else {
          val choices = spaceNames(candidates).map(n => n -> n) :+
             ("finished" -> "Finished pacifying spaces")

          val spaceMsg = s"${amountOf(4 - pacifySpaces.size, "space")} remaining"
          val cashMsg  = amountOf(game.arvnResources, "ARVN resource")
          val econMsg  = if (faction == US) s", Econ is ${game.econ}" else ""
          println(s"\n$faction Pacification (${spaceMsg}, ${cashMsg}${econMsg})")
          askMenu(choices, "Choose space to pacify:").head match {
            case "finished" =>
            case name       =>
              Human.pacifySpace(name, faction, coupRound = true, params.free, params.maxLevels)
              recordSpace(faction, name)
              pacify(faction)
          }
        }
      }
    }

    def agitateCandidate(sp: Space) = {
      sp.canHaveSupport             &&
      !agitateSpaces(sp.name)       &&
      sp.support > ActiveOpposition &&
      sp.pieces.has(VCPieces)       &&
      !sp.coinControlled            &&
      (!cadresUnshaded || sp.pieces.totalOf(VCGuerrillas) >= 2)
    }
    def canAgitate =
      agitateSpaces.size < 4 &&
      (if (game.trackResources(VC)) game.vcResources > agitateCost else game.agitateTotal > 0)

    def agitate(): Unit = {
      val candidates = game.nonLocSpaces filter agitateCandidate

      if (canAgitate && candidates.nonEmpty) {
        if (game.isBot(VC)) {
          val sp = Bot.VC_Bot.pickSpaceTowardActiveOpposition(candidates)
          Bot.VC_Bot.agitateSpace(sp.name, coupRound = true, params.maxLevels)
          pause()
          agitateSpaces += sp.name
          agitate()
        }
        else {
          val choices = spaceNames(candidates).map(n => n -> n) :+
             ("finished" -> "Finished agitating spaces")

          val spaceMsg = s"${amountOf(4 - agitateSpaces.size, "space")} remaining"
          val cashMsg  = amountOf(game.vcResources, "VC resource")
          println(s"\nVC Agitation ($spaceMsg, $cashMsg)")
          askMenu(choices, "Choose space to agitate:").head match {
            case "finished" =>
            case name       =>
              Human.agitateSpace(name, coupRound = true, params.free, params.maxLevels)
              agitateSpaces += name
              agitate()
          }
        }
      }

    }

    if (!params.forEvent) {
      log(s"\nSupport Phase")
      log(separator(char = '='))
    }

    loggingPointsChanges {
      if (params.factions(US) || params.factions(ARVN)) {
        log("\nPacification")
        log(separator())
        for (note <- pacifyNotes)
          log(note)
      }

      if (!game.trackResources(ARVN))
        log(s"Rolling 2d6 to determine max pacification: $botPoints")

      if (params.factions(US)) {
        pacify(US)
        if (usSpaces.isEmpty) {
          log("\nUS does not pacify any spaces")
          pause()
        }
      }

      if (params.factions(ARVN)) {
        pacify(ARVN)
        if (arvnSpaces.isEmpty) {
          log("\nARVN does not pacify any spaces")
          pause()
        }
      }

      if (params.factions(VC)) {
        log("\nAgitation")
        log(separator())
        for (note <- agitateNotes)
          log(note)

        agitate()
        if (agitateSpaces.isEmpty) {
          log("\nVC does not agitate any spaces")
          pause()
        }
      }
    }
  }

  // Used by both Human/ARV_Bot
  def arvnRedeployTroopDestinations(ignoreCoinBases: Boolean): List[String] = {
    val troopDest = (sp: Space) =>
      sp.name == Saigon        ||
      (ignoreCoinBases == false && sp.pieces.has(CoinBases)) ||
      (sp.isCity && !sp.nvaControlled)
    spaceNames(game.spaces filter troopDest)
  }

  // Used by both Human/ARV_Bot
  def arvnRedeployPoliceDestinations(): List[String] = {
    val policeDest = (sp: Space) => sp.isLoC || (sp.coinControlled && isInSouthVietnam(sp.name))
    spaceNames(game.spaces filter policeDest)
  }



  // Used by both Human/ARV_Bot
  // Returns spaces with ARVN Troops that must redeploy
  def arvnRedeployMandatoryTroopOrigins(ignoreCoinBases: Boolean): List[String] = {
    val isOrigin = (sp: Space) =>
      sp.pieces.has(ARVNTroops) &&
      (sp.isLoC || (sp.isProvince && (ignoreCoinBases || !sp.pieces.has(CoinBases))))
    spaceNames(game.spaces filter isOrigin)
  }

  def arvnCanRedeployTroops(ignoreCoinBases: Boolean): Boolean = {
    val arvnTroopsOnMap        = game.spaces exists (_.pieces.has(ARVNTroops))
    val troopDestOutsideSaigon = arvnRedeployTroopDestinations(ignoreCoinBases) exists (_ != Saigon)
    val troopsOutsideSaigon    = game.spaces exists (sp => sp.name != Saigon && sp.pieces.has(ARVNTroops))

    arvnTroopsOnMap &&
    (troopsOutsideSaigon || troopDestOutsideSaigon)
  }

  // As long as there is at least on police cube on the map
  // it can be redeployed
  def arvnCanRedeployPolice: Boolean = game.spaces exists (_.pieces.has(ARVNPolice))

    // Used by both Human/ARV_Bot
  def nvaRedeployTroopDestinations(): List[String] = {
    val troopDest = (sp: Space) => sp.pieces.has(NVABases)
    spaceNames(game.spaces filter troopDest)
  }

  def nvaCanRedeployTroops: Boolean = {
    val nvaDests = nvaRedeployTroopDestinations()
    val nvaTroopsOnMap = game.spaces exists (_.pieces.has(NVATroops))
    val nvaTroopsNotAtBase = game.spaces exists (sp => sp.pieces.has(NVATroops) && !sp.pieces.has(NVABases))

    nvaTroopsOnMap    &&
    nvaDests.nonEmpty &&
    (nvaDests.size > 1 || nvaTroopsNotAtBase)
  }

  // Redeploy Phase of Coup Round
  def coupRedeployPhase(): Unit = {

    def redepoyCOINOutOfLaosCambodia(): Unit = {
      var nothingMoved = true
      for (sp <- spaces(LaosCambodia)) {
        val usTroops = sp.pieces.only(USTroops)
        val others   = sp.pieces.only(ARVNTroops::ARVNPolice::Irregulars:::Rangers)
        if (usTroops.nonEmpty || others.nonEmpty)
          nothingMoved = false
        removeToCasualties(sp.name, usTroops)
        removeToAvailable(sp.name, others)
      }
      if (nothingMoved)
        log("No COIN forces in Laos or Cambodia")
    }

    log(s"\nRedeploy Phase")
    log(separator(char = '='))
    // Don't log any control changes until we are finished with the entire process.
    loggingControlChanges {

      log("\nLaos and Cambodia")
      log(separator())
      redepoyCOINOutOfLaosCambodia()
      pause()

      log("\nARVN Redeploy")
      log(separator())
      if (game.isBot(ARVN))
        Bot.ARVN_Bot.redeployARVNForces(troops = true, police = true, ignoreCoinBases = false)
      else
        Human.redeployARVNForces(troops = true, police = true, ignoreCoinBases = false)
      pause()

      log("\nNVA Redeploy")
      log(separator())
      if (game.isBot(NVA)) {
        Bot.NVA_Bot.redeployNVATroops()
        pause()
      }
      else
        Human.redeployNVATroops()
    }
  }

  // Commitment Phase of Coup Round
  // Mo_Medevac_Unshaded    - In Commitment Phase (immediately move all US TROOPS in CASUALTIES to AVAILABLE,
  //                          no TROOPS go out of play, bases still do.
  //                          See note: For effect when #73 Great Society is played.
  def coupCommitmentPhase(): Unit = {
    def troopsAndBasesAvailable = game.availablePieces.totalOf(USTroops) +
                                  game.availablePieces.totalOf(USBase)

    loggingControlChanges {
      //  Save the set of coin controlled spaces
      //  This is used during the commitment phase while
      //  pieces are placed/moved.  The new control values are
      //  not adjusted until the end of the phase.
      val coinControlledSpaces = spaceNames(game.spaces filter (sp => !sp.isLoC && sp.coinControlled)).toSet

      log(s"\nCommitment Phase")
      log(separator(char = '='))
      if (game.casualties.has(USTroops) && momentumInPlay(Mo_Medevac_Unshaded)) {
        log(s"US Troops in the Casualties box move to available: [Momentum: $Mo_Medevac_Unshaded]")
        moveCasualtiesToAvailable(game.casualties.only(USTroops))
      }

      // Determine the US Bot's new US Policy
      if (game.isBot(US)) {
        log("\nSet US Policy")
        log(separator())
        //  The Coup Card for the current Coup round has not yet
        //  been acccounted for hence the -2 below.
        val isPenultimateCoupRound = game.coupCardsPlayed == game.totalCoupCards - 2
        val die = d6
        val newPolicy = if (isPenultimateCoupRound) {
          log("Upcoming campaign is the final campaign")
          USPolicy_Nixon
        }
        else {
          log(s"Rolling d6 to determine new US Policy: $die")
          die match {
            case _ if die == game.numCardsInLeaderBox => USPolicy_JFK
            case _ if die >  game.numCardsInLeaderBox => USPolicy_LBJ
            case _                                    => USPolicy_Nixon
          }
        }
        setUSPolicy(newPolicy)
      }


      // Take 1 in 3 US Troop casualties and all US base casualties out of play.
      val numTroopsOutOfPlay = game.casualties.totalOf(USTroops) / 3
      val toOutOfPlay = game.casualties.only(USBase).add(numTroopsOutOfPlay, USTroops)
      log("\nROTATION: Move US base Casualties and 1/3 US Troop Casualties to Out of Play")
      log(separator())
      if (toOutOfPlay.nonEmpty)
        moveCasualtiesToOutOfPlay(toOutOfPlay)
      else
        log("There are no US bases or Troops to move Out of Play")

      // Move US Irregular casualties to available
      log("\nROTATION: Move US Irregular Casualties to Available")
      log(separator())
      val irregulars = game.casualties.only(Irregulars_U)
      if (irregulars.nonEmpty)
        moveCasualtiesToAvailable(game.casualties.only(Irregulars_U))
      else
        log("There are no US Irregulars in the Casualties box")
      pause()

      // Then US places all remaining US Troop casualties into any COIN controlled spaces,
      // LoCs, or Saigon.
      val numUSPlacedOnMap = game.casualties.totalOf(USTroops)

      if (numUSPlacedOnMap > 0) {
        log("\nROTATION: Place the remaining US Troop Casualties on the map")
        log(separator())
        if (game.isBot(US))
          Bot.US_Bot.placeUSCasualtyTroopsOnMap(coinControlledSpaces)
        else
          Human.placeUSCasualtyTroopsOnMap(coinControlledSpaces)
        pauseIfBot(US)
      }

      // The US may then move US Troops totaling up to 10 minus the number of casualties
      // placed on the map, plus up to 2 US Bases among the US available box, any COIN
      // controlled spaces, LoCs and Saigon.
      val maxUSTroopsToMove = (10 - numUSPlacedOnMap) max 0
      log(s"\nROTATION: Move up to ${amountOf(maxUSTroopsToMove, "US Troop")} and")
      log("up to 2 bases among Available box, COIN controlled spaces, LoCs and Saigon")
      log(separator())

      val numAvailableBefore = troopsAndBasesAvailable
      if (game.isBot(US))
        Bot.US_Bot.moveUSCommitmentPieces(maxUSTroopsToMove, coinControlledSpaces)
      else
        Human.moveUSCommitmentPieces(maxUSTroopsToMove, coinControlledSpaces)
      pauseIfBot(US)
      val numToAvailable = (troopsAndBasesAvailable - numAvailableBefore) max 0
      // Withdrawal: For every 2 US pieces just moved from the map to available, the
      // VC may shift 1 Population by 1 level toward Opposition.  No space may be shifted
      // by more than 1 level.
      val numVCPopShifts = numToAvailable / 2  // Number of Withdrawal shifts for VC
      log(s"\nWITHDRAWAL: For every 2 US pieces just removed to Available, VC shifts")
      log("1 population of support by 1 level toward Active Opposition")
      val was = if (numToAvailable == 1) "was" else "were"
      log()
      log(s"${amountOf(numToAvailable, "piece")} $was removed to Available")
      log(separator())
      if (numVCPopShifts > 0) {
        if (game.isBot(VC))
          Bot.VC_Bot.usWithdrawalShifts(numVCPopShifts)
        else
          Human.usWithdrawalShifts(numVCPopShifts)
      }
      else
        log("No shifts in support possible")
      if (numVCPopShifts == 0 || game.isBot(VC))
        pause()
    }
    pause() // Pause after showing control/scoring changes
  }

  // Reset Phase of Coup Round
  def coupResetPhase(): Unit = {
    log(s"\nReset Phase")
    log(separator(char = '='))
    if (game.trail == TrailMin) {
      log("The Trail is in 0 box")
      improveTrail(1)
    }
    else if (game.trail == TrailMax) {
      log("The Trail is in 4 box")
      if (momentumInPlay(Mo_Oriskany))
        log(s"The trail cannot be degraded [Momentum: $Mo_Oriskany]")
      else
        degradeTrail(1)
    }

    val sortedNames = spaceNames(game.spaces)
    // Remove all Terror and Sabotage markers
    for (name <- sortedNames; sp = game.getSpace(name); num = sp.terror; if num > 0)
      removeTerror(name, num)

    for (name <- sortedNames) {
      val sp = game.getSpace(name)
      val active = sp.pieces.only(Irregulars_A::Rangers_A::ActiveGuerrillas)
      hidePieces(name, active)
    }

    // Remove any momentum cards from play
    if (game.momentum.nonEmpty)  {
      log()
      game.momentum foreach removeMomentumFromPlay
    }

    // All factions are eligible after a Coup round
    log("\nMove all faction cylinders to the Eligible Factions box.")
    game = game.copy(sequence = SequenceOfPlay())

    if (game.isBot(VC) && game.agitateTotal == 0)
      initAgitateTotal()

    if (game.botFactions.nonEmpty) {
      game = game.copy(trungDeck = shuffle(game.trungDeck))
      log("\nThe Trung deck has been reshuffled")
  }
  pause()
}


  def updateFactionEligibility(makeAllEligible: Boolean): Unit = {
    val oldSequence = game.sequence
    val newSequence = if (makeAllEligible) SequenceOfPlay() else oldSequence.updateEligibility()
    val eligible    = (newSequence.eligibleThisTurn filterNot oldSequence.eligibleThisTurn).toList.sorted
    val ineligible  = (newSequence.ineligibleThisTurn filterNot oldSequence.ineligibleThisTurn).toList.sorted

    game = game.copy(sequence = newSequence)

    if (eligible.nonEmpty || ineligible.nonEmpty) {
      log("\nAdjust eligiblity")
      log(separator())
      if (eligible.nonEmpty)
        log(s"Move the ${andList(eligible)} ${pluralize(eligible.size, "cylinder")} to the Eligible box")
      if (ineligible.nonEmpty)
        log(s"Move the ${andList(ineligible)} ${pluralize(ineligible.size, "cylinder")} to the Ineligible box")
    }
  }

  // Allow user to do common commands and to resolve the
  // Coup card.
  @tailrec def processCoupCommand(): Unit = {
    val opts      = orList(List(CoupCmd.name, "?"))
    val coupNum   = game.coupCardsPlayed + 1
    val card      = eventDeck(game.currentCard)
    val prompt = {
      val promptLines = new ListBuffer[String]
      promptLines += ""
      promptLines += s">>> ${ordinal(coupNum)} Coup Round <<<"
      promptLines += separator(char = '=')
      promptLines += s"Current card: ${card.fullString}"
      promptLines += s"($opts): "
      promptLines.mkString("\n", "\n", "")
    }

    val (cmd, param) = askCommand(prompt, CoupCmd :: CommonCmds)

    cmd match {
      case CoupCmd  =>
        resolveCoupCard()

      case _ =>
        doCommonCommand(cmd, param)
        processCoupCommand()
    }

  }

  // Resolve the action for the next eligible faction.
  @tailrec def processActorCommand(): Unit = {
    val faction   = game.actingFaction.get
    val upNext    = s"  ($faction is up next)"
    val actorCmds = if (game.isBot(faction)) List(BotCmd) else List(ActCmd)
    val opts      = orList((actorCmds map (_.name)) :+ "?")
    val extra     = if (game.inMonsoon) " [MONSOON]"
                    else if (game.executingPivotalEvent) " [PIVOTAL EVENT]"
                    else ""
    val prompt = {
      val promptLines = new ListBuffer[String]
      promptLines += ""
      promptLines += s">>> $faction turn$extra <<<"
      promptLines += separator(char = '=')
      promptLines ++= sequenceList(eventDeck(game.currentCard), game.sequence)
      promptLines += s"($opts): "
      promptLines.mkString("\n", "\n", "")
    }

    val (cmd, param) = askCommand(prompt, actorCmds ::: CommonCmds)

    cmd match {
      case ActCmd  =>
        logSummary(sequenceSummary, echo = false)
        Human.act()
        log(s"\nFinished with $faction turn")

      case BotCmd  =>
        logSummary(sequenceSummary, echo = false)
        Bot.act()
        log(s"\nFinished with $faction turn")

      case _ =>
        doCommonCommand(cmd, param)
        processActorCommand()
    }
  }

  // Return the list of factions that may play their pivotal event.
  // The factions are returned in the order: VC, ARVN, NVA, US
  // (Factions in this order can Trump those that follow)
  def getPlayablePivotalEvents: List[Faction] = {

    def canPlayPivotal(faction: Faction): Boolean = {
      val pivotCard = eventDeck(faction.pivotCard)

      game.pivotCardsAvailable(faction)       &&
      game.sequence.eligibleThisTurn(faction) &&
      pivotCard.eventEffective(faction)
    }

    // Pivotal events are only available if no
    // faction has acted on the current card and
    // the next card showing is not a Coup! card.
    // For pivot cards, the eventEffection() function
    // returns true if the condition for playing the pivotal
    // event has been met.
    if (game.sequence.numActors == 0 && !game.isCoupRound && !game.onDeckIsCoup)
      List(VC, ARVN, NVA, US) filter canPlayPivotal
    else
      Nil
  }

  //  Determine if a faction will play their Pivotal
  //  Event, cancelling the current event card.
  //  The Bots will always play their pivotal event
  //  as soon as they can and a 1d6 roll is less than
  //  the number of cards in the RVN Leader Box.
  //  If the Bot is 1st Elibigle on the current card,
  //  it will not play its pivotal event if the current
  //  event is critical for that Bot.
  def getPivotalFaction: Option[Faction] = {
    def currentEventIsCritical(faction: Faction) =
      game.isActingFaction(faction) &&
      eventDeck(game.currentCard).isCritical(faction)

    def askHumanPivot(factions: List[Faction], pivotBot: Option[Faction]): Option[Faction] = {
      factions match {
        case Nil   =>
          None

        case f::fs =>
          pivotBot foreach { bot =>
            println()
            println(separator(char = '='))
            println(s"$bot Bot plans to play its pivotal event")
            println(separator(char = '='))
          }
          val prompt = pivotBot match {
            case None      => s"\nDoes $f wish to play their Pivotal Event? (y/n) "
            case Some(bot) => s"\nDoes $f wish to trump $bot and play their Pivotal Event? (y/n) "
          }
          if (askYorN(prompt))
            Some(f)
          else
            askHumanPivot(fs, pivotBot)
      }
    }

    val eligible = getPlayablePivotalEvents
    // Find the first Bot that will play their card.
    val pivotBot = eligible find { faction =>
      game.isBot(faction)           &&
      d6 < game.numCardsInLeaderBox &&
      !currentEventIsCritical(faction)
    }

    // Next find all eligible human factions
    // that could play their pivotal event without
    // being trumped by the Bot.

    val pivotHumans = pivotBot match {
      case None      => eligible filter game.isHuman
      case Some(bot) => eligible takeWhile (_ != bot) filter game.isHuman
    }

    val pivotPlayer = askHumanPivot(pivotHumans, pivotBot)

    pivotPlayer orElse pivotBot
  }


  @tailrec def mainLoop(): Unit = {
    // Does any faction wish to play their pivotal event?
    // Will alwasy return None if Coup round or Coup card on deck
    // or any faction has already acted.
    // If the current card is already a pivotal card then the game
    // was saved and restored so don't allow it to be trumped!
    val pivotFaction = if (eventDeck.isPivotalCard(game.currentCard))
      None
    else
      getPivotalFaction

    if (!game.gameOver && pivotFaction.nonEmpty) {
      val faction = pivotFaction.get

      log()
      log(separator(char = '='))
      log(s"$faction elects to play their Pivotal Event")
      log(separator(char = '='))
      log(s"Replace the current event card with ${eventDeck(faction.pivotCard)}")

      // Replace the current card with the faction's Pivotal event
      game = game.copy(currentCard = faction.pivotCard, pivotCardsAvailable = game.pivotCardsAvailable - faction)
      saveGameState()
    }

    try {
      // This can happend if the user loads a game that has already ended
      // We call this here because the user may want to rollback to
      // an earlier turn.
      if (game.gameOver)
        endgameCommand() // Does not return, throws ExitGame or Rollback

      val savedState = game
      try {
        if (game.isCoupRound) {
          processCoupCommand()

          if (game.gameOver) {
            // The game has ended allow the user to
            // look around, rollback, and quit
            saveGameState()
            endgameCommand() // Does not return, throws ExitGame or Rollback
          }
          else {
            drawNextCard()
            saveGameState()
          }
        }
        else {
          processActorCommand()

          // If the final Coup card is on deck then move it
          // to the current card.
          // Otherwise if no more factions can act on the current card
          // prompt prompt for a new Event card.
          if (game.sequence.exhausted)
            drawNextCard()

          // Now create a save point to capture the action
          saveGameState()
        }
      }
      catch {
        case AbortAction =>
          println("\n>>>> Aborting the current action <<<<")
          println(separator(char = '='))
          displayGameStateDifferences(game, savedState)
          game = savedState

        // After an adjustment we comback to the main loop
        // so we can check to see if a pivotal event should
        // now be played.
        case Adjustment =>
      }
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

  //  Called when the game has ended.
  //  Allow the user to do common commands.
  //  Show, History, Rollback, Quit
  // This function does not return!
  // It can throw either ExitGame, or Rollback exceptions.
  def endgameCommand(): Unit = {
    val Commands = List(ShowCmd, HistoryCmd, RollbackCmd, HelpCmd, QuitCmd)

    val opts      = orList(List("quit", "?"))
    val prompt = {
      val promptLines = new ListBuffer[String]
      promptLines += ""
      promptLines += s">>> The game has ended <<<"
      promptLines += separator(char = '=')
      promptLines += s"($opts): "
      promptLines.mkString("\n", "\n", "")
    }
    val (cmd, param) = askCommand(prompt, Commands)

    doCommonCommand(cmd, param)
    endgameCommand()
  }

  //  Draw the next Trung Card for the given faction.
  //  The game state is updated and the topmost card
  //  is returned.
  def drawTrungCard(faction: Faction): TrungCard = {
    var trungDeck = game.trungDeck

    // First the topmost card is place on the bottom
    // Then continue drawing until we get a card for the
    // given faction.
    do {
      trungDeck = trungDeck.tail :+ trungDeck.head
    } while (trungDeck.head.faction != faction)

    game = game.copy(trungDeck = trungDeck)
    trungDeck.head
  }

  def factionPasses(faction: Faction): Unit = {
    log(s"\n$faction faction passes")
    log(separator())
    faction match {
      case US|ARVN => increaseResources(ARVN, 3)
      case _       => increaseResources(faction, 1)
    }
  }

  // Will not increase resrouces if faction is NP bot.
  def increaseResources(faction: Faction, amount: Int): Unit = if (amount > 0) {
    faction match {

      case ARVN if game.trackResources(ARVN) =>
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
      case US =>
        throw new IllegalArgumentException("decreaseResources passed US faction")

      case ARVN if game.trackResources(ARVN) =>
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
      if (momentumInPlay(Mo_ADSID) && game.isHuman(NVA)) {
        log(s"Momentum $Mo_ADSID triggers")
        decreaseResources(NVA, 6)
      }
    }
  }

  def degradeTrail(num: Int): Unit = if (num > 0) {
    val newTrail = (game.trail - num) max TrailMin
    if (newTrail != game.trail) {
      game = game.copy(trail = newTrail)
      log(s"Degrade the trail by ${amountOf(num, "box", Some("boxes"))} to $newTrail")
      if (momentumInPlay(Mo_ADSID) && game.isHuman(NVA)) {
        log(s"Momentum $Mo_ADSID triggers")
        decreaseResources(NVA, 6)
      }
    }
  }

  //  Add terror/sabotage markers
  def addTerror(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    assert(game.terrorMarkersAvailable >= num, s"addTerror($name): not enough available markers")
    game = game.updateSpace(sp.copy(terror = sp.terror + num))
    if (sp.isLoC)
      log(s"Add ${amountOf(num, "sabotage marker")} to $name")
    else
      log(s"Add ${amountOf(num, "terror marker")} to $name")
  }

  //  Remove terror/sabotage markers
  def removeTerror(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    assert(sp.terror >= num, s"removeTerror($name): not enough markers in space")
    game = game.updateSpace(sp.copy(terror = sp.terror - num))
    if (sp.isLoC)
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

  def setEconValue(amount: Int): Unit = if (game.trackResources(ARVN)) {
    game = game.copy(econ = amount)
    log(s"Set Econ marker to ${game.econ}")
  }

  def setUSPolicy(policy: String): Unit = if (game.isBot(US)) {
    game = game.copy(usPolicy = policy)
    log(s"Set US Policy to ${game.usPolicy}")
  }

  // If VC is a Bot, the we store the agitate total in vcResources
  def setAgitateTotal(amount: Int): Unit = if (game.isBot(VC)) {
    game = game.copy(vcResources = amount)
    log(s"Set Agitate Total (VC resources cylinder) to ${game.vcResources}")
  }

  def initAgitateTotal(): Unit = {
    val agitateTotal = d3
    log(s"\nRolling d3 to set the Agitate Total: $agitateTotal")
    log(separator())
    setAgitateTotal(agitateTotal)
    pause()
  }

  // If VC is a Bot, the we store the agitate total in vcResources
  def increaseAgitateTotal(amount: Int): Unit = if (game.isBot(VC) && amount > 0) {
    game = game.copy(vcResources = (game.vcResources + amount) min EdgeTrackMax)
    log(s"Increase Agitate Total by +$amount to ${game.vcResources}")
  }

  // If VC is a Bot, the we store the agitate total in vcResources
  def decreaseAgitateTotal(amount: Int): Unit = if (game.isBot(VC) && amount > 0) {
    game = game.copy(vcResources = (game.vcResources - amount) max 0)
    log(s"Decrease Agitate Total by -$amount to ${game.vcResources}")
  }

  def setSupport(name: String, newSupport: SupportType): Unit = {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      if (sp.canHaveSupport && sp.support != newSupport) {
        val updated = sp.copy(support = newSupport)
        game = game.updateSpace(updated)
        logSupportChange(sp, updated)
      }
    }
  }

  def increaseSupport(name: String, num: Int): Unit = if (num > 0) {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      if (sp.canHaveSupport) {
        val newValue = (sp.support.value + num) min ActiveSupport.value
        val updated = sp.copy(support = SupportType(newValue))
        game = game.updateSpace(updated)
        logSupportChange(sp, updated)
      }
    }
  }

  def decreaseSupport(name: String, num: Int): Unit = if (num > 0) {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      if (sp.canHaveSupport) {
        val newValue = (sp.support.value - num) max ActiveOpposition.value
        val updated = sp.copy(support = SupportType(newValue))

        game = game.updateSpace(updated)
        logSupportChange(sp, updated)
      }
    }
  }

  def isRVNLeader(name: String) = game.currentRvnLeader == name && !game.rvnLeaderFlipped

  def capabilityInPlay(cap: Capability) = game.capabilities contains cap

  //  Caller must ensure that both events are currently in play
  //  before calling this function.
  def isEventMoreRecentThan(event: String, otherEvent: String): Boolean = {
    val eventIndex = game.ongoingEvents.indexOf(event)
    val otherIndex = game.ongoingEvents.indexOf(otherEvent)
    assert(eventIndex != -1, s"isEventMoreRecentThan: $event is not in ongoingEvents")
    assert(otherIndex != -1, s"isEventMoreRecentThan: $otherEvent is not in ongoingEvents")

    eventIndex < otherIndex
  }

  // We add all capabilities and momentum cards to the ongoingEvent list.
  // The entries closer to the head of the list have been played more
  // recently and will therefore override event played later.
  def addOngoingEvent(name: String): Unit = {
    game = game.copy(ongoingEvents = name::game.ongoingEvents)
  }

  def removeOngoingEvent(name: String): Unit = {
    game = game.copy(ongoingEvents = game.ongoingEvents filterNot (_ == name))
  }

  def playCapability(cap: Capability): Unit = {
    game = game.copy(capabilities = cap :: game.capabilities)
    addOngoingEvent(cap.name)
    log(s"\nCapability: '$cap' is now in effect")
  }

  def removeCapabilityFromPlay(cap: Capability): Unit = {
    if (game.capabilities contains cap) {
      game = game.copy(capabilities = game.capabilities filterNot (_ == cap))
      removeOngoingEvent(cap.name)
      log(s"\nRemove capability: '$cap' from play")
    }
  }

  def flipCapability(cap: Capability): Unit = {
    if (game.capabilities contains cap) {
      game = game.copy(capabilities = cap.flip :: (game.capabilities filterNot (_ == cap)))
      val desc = if (cap.shaded) "unshaded" else "shaded"
      log(s"\nFlip capability: '$cap' to its $desc side")
    }
  }


  def momentumInPlay(mo: String) = game.momentum contains mo

  def playMomentum(mo: String): Unit = {
    game = game.copy(momentum = mo :: game.momentum)
    addOngoingEvent(mo)
    log(s"\nMomentum: '$mo' is now in play")
  }

  def removeMomentumFromPlay(mo: String): Unit = {
    game = game.copy(momentum = game.momentum filterNot (_ == mo))
    removeOngoingEvent(mo)
    log(s"Remove Momentum: '$mo' from play")

  }

  // Place pieces from the AVAILABLE box in the given map space.
  // There must be enough pieces in the available box or an exception is thrown.
  def placePieces(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.availablePieces contains pieces, "Insufficent pieces in the available box")

    loggingControlChanges {
      val sp      = game.getSpace(spaceName)

      assert(pieces.totalBases + sp.pieces.totalBases <= 2, s"Cannot place more than 2 bases in $spaceName")
      game = game.updateSpace(sp.copy(pieces = sp.pieces + pieces))
      for (desc <- pieces.descriptions)
        log(s"Place $desc from AVAILABLE into $spaceName")
    }
  }

  // Place pieces from the OUT OF PLAY box in the given map space.
  // There must be enough pieces in the out of play box or an exception is thrown.
  def placePiecesFromOutOfPlay(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.outOfPlay contains pieces, "Insufficent pieces in the out of play box")

    loggingControlChanges {
      val sp      = game.getSpace(spaceName)

      assert(pieces.totalBases + sp.pieces.totalBases <= 2, s"Cannot place more than 2 bases in $spaceName")
      game = game.copy(outOfPlay = game.outOfPlay - pieces).updateSpace(sp.copy(pieces = sp.pieces + pieces))
      for (desc <- pieces.descriptions)
        log(s"Place $desc from OUT OF PLAY into $spaceName")
    }
  }

  def removeToAvailable(spaceName: String, pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp = game.getSpace(spaceName)
      assert(sp.pieces contains pieces, s"removeToAvailable() $spaceName does not contain all requested pieces: $pieces")
      val updated = sp.copy(pieces = sp.pieces - pieces)
      game = game.updateSpace(updated)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Remove $desc from $spaceName to AVAILABLE")
    }
  }

  def removeToCasualties(spaceName: String, pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp = game.getSpace(spaceName)
      assert(sp.pieces contains pieces, s"removeToCasualties() $spaceName does not contain all requested pieces: $pieces")
      val updated = sp.copy(pieces = sp.pieces - pieces)
      // Pieces in casualties are always normalized.
      game = game.updateSpace(updated).copy(casualties = game.casualties + pieces.normalized)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Remove $desc from $spaceName to CASUALTIES")
    }
  }

  // Remove pieces from a map space directly to the Out Of Play box.
  def removeToOutOfPlay(spaceName: String, pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp = game.getSpace(spaceName)
      assert(sp.pieces contains pieces, s"removeToOutOfPlay() $spaceName does not contain all requested pieces: $pieces")
      val updated = sp.copy(pieces = sp.pieces - pieces)
      // Pieces in casualties are always normalized.
      game = game.updateSpace(updated).copy(outOfPlay = game.outOfPlay + pieces.normalized)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Remove $desc from $spaceName to OUT OF PLAY")
    }
  }

  def moveAvailableToCasualties(pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingPointsChanges {
      val available = game.availablePieces
      assert(available contains pieces, s"All requested pieces are not available: $pieces")
      // Pieces in casualties are always normalized.
      game = game.copy(casualties = game.casualties + pieces.normalized)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Move $desc from AVAILABLE to CASUALTIES")
    }
  }

  def moveAvailableToOutOfPlay(pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingPointsChanges {
      val available = game.availablePieces
      assert(available contains pieces, s"All requested pieces are not available: $pieces")
      // Pieces in out of play are always normalized.
      game = game.copy(outOfPlay = game.outOfPlay + pieces.normalized)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Move $desc from AVAILABLE to OUT OF PLAY")
    }
  }

  def moveCasualtiesToAvailable(pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingPointsChanges {
      val casualties = game.casualties
      assert(casualties contains pieces, s"All requested pieces are not in casualties: $pieces")
      game = game.copy(casualties = game.casualties - pieces.normalized)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Move $desc from CASUALTIES to AVAILABLE")
    }
  }

  def moveCasualtiesToMap(pieces: Pieces, name: String, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val casualties = game.casualties
      assert(casualties contains pieces, s"All requested pieces are not in casualties: $pieces")
      val sp = game.getSpace(name)
      val newSp = sp.copy(pieces = sp.pieces +  pieces)
      game = game.copy(casualties = game.casualties - pieces.normalized).updateSpace(newSp)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Move $desc from CASUALTIES to $name")
    }
  }

  def moveCasualtiesToOutOfPlay(pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingPointsChanges {
      assert(game.casualties contains pieces, s"All requested pieces are not in casualties: $pieces")
      game = game.copy(casualties = game.casualties - pieces.normalized,
                       outOfPlay  = game.outOfPlay + pieces.normalized)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      for (desc <- pieces.descriptions)
        log(s"Move $desc from CASUALTIES to OUT OF PLAY")
    }
  }

  def moveOutOfPlayToMap(pieces: Pieces, name: String): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp    = game.getSpace(name)
      val newSp = sp.copy(pieces = sp.pieces + pieces)

      assert(game.outOfPlay contains pieces, s"All requested pieces are not in out of play: $pieces")
      game = game.copy(outOfPlay = game.outOfPlay - pieces.normalized).updateSpace(newSp)

      for (desc <- pieces.descriptions)
        log(s"Move $desc from OUT OF PLAY to $name")
    }
  }

  def moveOutOfPlayToAvailable(pieces: Pieces): Unit = if (pieces.total > 0) {
    loggingPointsChanges {
      assert(game.outOfPlay contains pieces, s"All requested pieces are not in out of play: $pieces")
      game = game.copy(outOfPlay = game.outOfPlay - pieces.normalized)

      for (desc <- pieces.descriptions)
        log(s"Move $desc from OUT OF PLAY to AVAILABLE")
    }
  }

  // Remove pieces from the given space.
  // US to casualties, all other to available

  def removePieces(spaceName: String, pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val deadUS    = pieces.only(USPieces)
      val deadOther = pieces.except(USPieces)

      reason foreach { msg =>
        log(s"\n$msg")
        log(separator())
      }
      removeToCasualties(spaceName, deadUS)
      removeToAvailable(spaceName, deadOther)
    }
  }

  // Return the number of exposed insurgent pieces
  // Bases are only included if there are no hidden guerrillas
  def numExposedInsurgents(pieces: Pieces): Int = {
    val exposedForces = pieces.totalOf(NVATroops::ActiveGuerrillas)
    val exposedBases  = if (pieces.has(UndergroundGuerrillas)) 0
                        else pieces.totalOf(NVABase::VCBase::Nil)
    exposedForces + exposedBases
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

    for (desc <- hidden.descriptions)
      log(s"Flip $desc in $spaceName to ACTIVE")
  }

  //  Hide guerrillas/rangers/irregulars in a space
  def hidePieces(spaceName: String, visible: Pieces): Unit = if (visible.total > 0) {
    val Valid = List(Irregulars_A, Rangers_A, NVAGuerrillas_A, VCGuerrillas_A)
    val sp = game.getSpace(spaceName)
    assert(visible.only(Valid) == visible, s"hidePieces() called with non-active pieces: $visible")
    assert(sp.pieces contains visible, s"hidePieces() $spaceName does not contain all requested pieces: $visible")

    val hidden = Pieces(
      irregulars_U    = visible.irregulars_A,
      rangers_U       = visible.rangers_A,
      nvaGuerrillas_U = visible.nvaGuerrillas_A,
      vcGuerrillas_U  = visible.vcGuerrillas_A)
    val updated = sp.copy(pieces = sp.pieces - visible + hidden)
    game = game.updateSpace(updated)

    for (desc <- visible.descriptions)
      log(s"Flip $desc in $spaceName to UNDERGROUND")
  }



  // Move the given pieces from the source space to the destination space
  // and log the activity.
  def movePieces(pieces: Pieces, source: String, dest: String): Unit = {
    val srcSpace = game.getSpace(source)
    val dstSpace = game.getSpace(dest)
    assert(srcSpace.pieces contains pieces, s"movePieces() $source does not contain all requested pieces: $pieces")

    val updatedSrc = srcSpace.copy(pieces = srcSpace.pieces - pieces)
    val updatedDst = dstSpace.copy(pieces = dstSpace.pieces + pieces)
    loggingControlChanges {
      game = game.updateSpace(updatedSrc).updateSpace(updatedDst)

      for (desc <- pieces.descriptions)
        log(s"Move $desc from $source to $dest")
    }
  }

  def addTunnelMarker(spaceName: String, bases: Pieces): Unit = {
    val sp = game.getSpace(spaceName)
    val onlyBases = bases.explode() forall InsurgentNonTunnels.contains
    assert(game.tunnelMarkersAvailable > 0, "addTunnelMarker() called but no tunnel markers available")
    assert(onlyBases, s"addTunnelMarker() called with non insurgent bases")
    assert(sp.pieces.contains(bases), s"addTunnelMarker() $spaceName does not contain $bases")

    val tunnels = bases.explode().foldLeft(Pieces()) { (p, b) =>
      val tunnelType = if (b == NVABase) NVATunnel else VCTunnel
      p.add(1, tunnelType)
    }

    val newPieces = sp.pieces - bases + tunnels
    game = game.updateSpace(sp.copy(pieces = newPieces))
    for (desc <- bases.descriptions)
      log(s"Add a tunnel marker to ${desc} in $spaceName")
  }

  def removeTunnelMarker(spaceName: String, tunnels: Pieces): Unit = {
    val sp = game.getSpace(spaceName)
    val onlyTunnels = tunnels.explode() forall InsurgentTunnels.contains
    assert(onlyTunnels, s"removeTunnelMarker() called with non tunnels")
    assert(sp.pieces.contains(tunnels), s"removeTunnelMarker() $spaceName does not contain $tunnels")

    val bases = tunnels.explode().foldLeft(Pieces()) { (p, t) =>
      val baseType = if (t == NVATunnel) NVABase else VCBase
      p.add(1, baseType)
    }

    val newPieces = sp.pieces - tunnels + bases
    game = game.updateSpace(sp.copy(pieces = newPieces))
    for (desc <- tunnels.descriptions)
      log(s"Remove a tunnel marker from ${desc} in $spaceName")
  }

  //  Used by both Human and Bot Patrol commands
  //  to carry out the effect of the shaded M48 Patton capability
  def performM48Patton_Shaded(movedCubes: MovingGroups): Unit = {
    if (capabilityInPlay(M48Patton_Shaded) && movedCubes.size > 0) {
      val (name, toRemove) = if (game.isHuman(NVA)) {
        println(s"\nNVA removes two cubes that moved [$M48Patton_Shaded]")
        val choices = movedCubes.toList map (x => x._1 -> x._1)

        val name = askMenu(choices, "Remove cubes from which space:", allowAbort = false).head
        val sp   = game.getSpace(name)
        val num  = movedCubes(name).total min 2
        val toRemove = askPieces(movedCubes(name), num, prompt = Some("Select cubes to remove"), allowAbort = false)
        (name, toRemove)
      }
      else {
        val candidates = spaces(movedCubes.toList map (_._1))
        val sp         = Bot.NVA_Bot.pickSpaceRemoveReplace(candidates)
        val toRemove   = Bot.selectEnemyRemoveReplaceActivate(movedCubes(sp.name), 2)
        (sp.name, toRemove)
      }
      removePieces(name, toRemove, Some(s"NVA removes two cubes that moved [$M48Patton_Shaded]"))
    }
  }

  def ambushGuerrillaTypes(faction: Faction, needUnderground: Boolean) = (faction, needUnderground) match {
    case (NVA, true)  => Set(NVAGuerrillas_U)
    case (NVA, false) => Set(NVAGuerrillas_U, NVAGuerrillas_A)
    case (_,   true)  => Set(VCGuerrillas_U)
    case (_,   false) => Set(VCGuerrillas_U, VCGuerrillas_A)
  }

  // Return list of spaces that have COIN pieces that can be reached
  // from the given ambush space.
  // Returns empty if none found
  def ambushTargets(name: String): List[String] = {
    val ambushSpace = game.getSpace(name)

    val inPlace = if (ambushSpace.pieces.has(CoinPieces)) Some(name) else None
    val adjacent = if (ambushSpace.isLoC)
      getAdjacent(name).toList filter (adj => game.getSpace(adj).pieces.has(CoinPieces))
    else
      Nil

    (inPlace.toList ::: adjacent).sorted
  }

  def askPieceType(prompt: String, pieceTypes: Iterable[PieceType], generic: Boolean = true): PieceType = {
    val choices = if (generic) pieceTypes map (t => t -> t.genericSingular)
                  else         pieceTypes map (t => t -> t.singular)

      askMenu(choices.to(List), prompt).head
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
          prompt foreach { p =>
            println(p)
            println(separator())
          }
          println(s"Select ${amountOf(numPieces, "piece")} among the following:")
          wrap("  ", available.descriptions) foreach println
          println()

          def nextType(types: Iterable[PieceType]): Unit = {
            val numRemaining = numPieces - selected.total
            if (numRemaining != 0) {
              // If we have to include all remainig pieces, don't bother asking
              if (pieces.totalOf(types) == numRemaining) {
                for (pieceType <- types)
                  selected = selected.add(pieces.totalOf(pieceType), pieceType)
              }
              else {
                val (pieceType :: rest) = types
                val totalOfRest = pieces.totalOf(rest)
                val minimum = if (totalOfRest < numRemaining) numRemaining - totalOfRest else 0
                val maximum = numRemaining min pieces.totalOf(pieceType)
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
  def voluntaryRemoval(num: Int, pieceType: PieceType, prohibited: Set[String] = Set.empty): Unit = if (num > 0) {
    val types = simiarTypes(pieceType)  // Account for Active/Underground if necessary
    val numOnMap = game.totalOnMap(_.pieces.totalOf(types))
    var removed = Map.empty[String, Pieces].withDefaultValue(Pieces())
    def candidates = spaceNames(
      game.spaces filter { sp =>
        val numInSpace = sp.pieces.totalOf(types) - removed(sp.name).total
        !prohibited(sp.name) &&
         numInSpace > 0
      }
    )

    def nextSpace(numRemaining: Int): Unit = if (numRemaining > 0) {
      val desc = amountOf(numRemaining, pieceType.genericSingular, Some(pieceType.genericPlural))
      println(s"\nYou must remove $desc from the map")
      val name         = askCandidate(s"\nSelect space to remove ${pieceType.genericPlural}: ", candidates)
      val sp           = game.getSpace(name)
      val pieces       = sp.pieces.only(types)
      val numInSpace   = pieces.total - removed(name).total
      val maxInSpace   = numInSpace min numRemaining
      val numFromSpace = askInt(s"Remove how many ${pieceType.genericPlural}", 0, maxInSpace)
      val toRemove     = askPieces(pieces, numFromSpace, types)
      if (toRemove.nonEmpty)
        removed += (name -> (removed(name) + toRemove))
      nextSpace(numRemaining - numFromSpace)
    }

    assert(numOnMap >= num, s"voluntaryRemoval: Not enough ${pieceType.genericPlural} on map!")

    if (numOnMap == num) {
      for (name <- candidates; sp = game.getSpace(name))
        removed += (name -> sp.pieces.only(types))
    }
    else
      nextSpace(num)

    if (removed.nonEmpty)
      loggingControlChanges {
        println()
        for ((name, pieces) <- removed) {
          val sp = game.getSpace(name)
          val updated = sp.copy(pieces = sp.pieces - pieces)
          game = game.updateSpace(updated)
          log(s"Remove ${andList(pieces.descriptions)} from $name to AVAILABLE")
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
  def askPiecesToPlace(spaceName: String, types: Iterable[PieceType], maxToPlace: Int): Pieces = {
    // Get number of each type in available box plus on map
    val piecesToPlace = game.piecesToPlace.only(types)
    val maxPieces     = maxToPlace min piecesToPlace.total
    val availMap      = (types map (t => t -> piecesToPlace.total)).to(Map)
    val availTypes    = types.to(List) filter (availMap(_) > 0)
    if (availTypes.isEmpty) {
      println(s"\nThere are no ${orList(types.to(List))} available to be placed.")
      Pieces()
    }
    else {

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
            val num = askInt(s"\nPlace how many ${pieceType.genericPlural} in $spaceName? ", 0, maxOfType)
            val numAvail = game.availablePieces.totalOf(pieceType)
            val finalNum = if (num <= numAvail)
               num
            else {
              // Ask if they want to voluntarily remove pieces to make up the difference.
              numAvail match {
                case 0 => println(s"\nThere are no ${pieceType.genericPlural} in the available box")
                case 1 => println(s"\nThere is only 1 ${pieceType.genericSingular} in the available box")
                case n => println(s"\nThere are only ${amountOf(n, pieceType.genericSingular)} in the available box")
              }
              println()
              if (askYorN("Do you wish to voluntarily remove pieces to make up the difference? (y/n) ")) {
                val numToRemove = askInt("How many pieces do you wish to remove from the map", 0, num - numAvail)
                voluntaryRemoval(numToRemove, pieceType, prohibited = Set(spaceName))
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

  // The User has already determined how many of a given piece type to place.
  // If there are not enough of the pieces in the available box, then we must
  // ask for vountariy removal.
  def ensurePieceTypeAvailable(pieceType: PieceType, num: Int): Unit = {
    val normalized = normalizedType(pieceType)
    assert(game.piecesToPlace.totalOf(normalized) >= num, s"askPieceTypeToPlace: Not enought pieces can be placed ($pieceType)")

    if (game.availablePieces.totalOf(normalized) <  num) {
      val mustRemove = num - game.availablePieces.totalOf(normalized)

      println(s"\nThere are not enough ${normalized.genericPlural} in the available box")
      voluntaryRemoval(mustRemove, normalized)
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

    if (piecesToPlace.isEmpty || (piecesToPlace.total == 1 && game.getSpace(spaceName).pieces.has(baseType))) {
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
          voluntaryRemoval(1, baseType, prohibited = Set(spaceName))
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
          case (s, Neutral)                             => log(s"Remove $s marker from $name")
          case (Neutral, s)                             => log(s"Place $s marker in $name")
          case (old, s) if old < Neutral && s < Neutral => log(s"Flip $old marker in $name to $s")
          case (old, s) if old > Neutral && s > Neutral => log(s"Flip $old marker in $name to $s")
          case (old, s)                                 => log(s"Replace $old marker in $name with $s marker")
      }
    }
  }


  def logPointsChanges(origGame: GameState, newGame: GameState): Unit = {
    var loggedHeader = false;
    def logChange(message: String): Unit = {
      if (!loggedHeader) {
        log("\nScore Marker Changes")
        log(separator())
        loggedHeader = true
      }
      log(message)
    }

    if (origGame.usPoints != newGame.usPoints)
      logChange(s"Move the 'Support + Avail US' marker from ${origGame.usPoints} to ${newGame.usPoints}")

    if (origGame.arvnPoints != newGame.arvnPoints)
      logChange(s"Move the 'COIN Control + Patronage' marker from ${origGame.arvnPoints} to ${newGame.arvnPoints}")

    if (origGame.nvaPoints != newGame.nvaPoints)
      logChange(s"Move the 'NVA Control + NVA Bases' marker from ${origGame.nvaPoints} to ${newGame.nvaPoints}")

    if (origGame.vcPoints != newGame.vcPoints)
      logChange(s"Move the 'Total Opposition + VC Bases' marker from ${origGame.vcPoints} to ${newGame.vcPoints}")
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
            if !orig.isLoC && orig.control != space.control
          } yield (space.name, orig.control, space.control)

          if (changed.nonEmpty) {
            log(s"\nControl Changes")
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
  def andList(x: Iterable[Any]) = x.to(Seq) match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} and ${b.toString}"
    case s         => s.dropRight(1).mkString(", ") + ", and " + s.last.toString
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

  def thereAre(num: Int, singular: String, plural: Option[String] = None): String = {
    val pluralName = pluralize(num, singular, plural)
    num match {
      case 0 => s"are no $pluralName"
      case 1 => s"is 1 $singular"
      case n => s"are $n $pluralName"
    }
  }

  def inspect[T](name: String, value: T): T = {
    val str = if (value == null) "NULL" else value.toString
    println(s"DEBUG: $name == ${str}")
    value
  }

  // Format the given sequence of strings in a comma separated list
  // such that we do not exceed the given number of columns.
  def wrap[T](prefix: String, values: Seq[T], columns: Int = 78, showNone: Boolean = true): Seq[String] = {
    val stringValues = values map (_.toString)
    val b = new ListBuffer[String]
    val s = new StringBuilder(prefix)
    var first = true
    if (stringValues.isEmpty) {
      if (showNone)
        s.append("none")
    }
    else {
      val margin = " " * prefix.length
      s.append(stringValues.head)
      for (v <- stringValues.tail) {
        s.append(", ")
        if (s.length + v.length < columns)
          s.append(v)
        else {
          b += s.toString
          s.clear()
          s.append(margin).append(v)
        }
      }
    }
    b += s.toString
    b.toList
  }

  def pause(): Unit = {
    if (!loggingSuspended)
      readLine("\n>>>>> [ Press Enter to continue... ] <<<<<")
  }

  def pauseIfBot(faction: Faction): Unit = if (game.isBot(faction)) pause()

  // We sometimes want to run some Bot code to
  // check a what-if situation.  In these cases
  // we do not want anything to be logged to the
  // terminal.

  private var loggingSuspended = false

  def suspendLogging[T](code: => T): T = {
    if (loggingSuspended)
      code
    else {
      loggingSuspended = true
      try     code
      finally loggingSuspended = false
    }
  }


  // Print the line to the console and save it in the game's history.
  def log(line: String = "", force: Boolean = false, echo: Boolean = true): Unit = if (!loggingSuspended || force) {
    if (echo)
      println(line)
    game = game.copy(log = game.log :+ line)
  }

  def separator(length: Int = 52, char: Char = '-'): String = char.toString * length


  def printSummary(summary: Seq[String]): Unit = if (summary.nonEmpty) {
    println()
    summary foreach println
  }

  def logSummary(summary: Seq[String], echo: Boolean = true): Unit = {
    log(echo = echo)
    summary foreach (msg => log(msg, echo = echo))
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

  def displayGameStateDifferences(from: GameState, to: GameState): Unit = if (from != to) {
    val b = new ListBuffer[String]

    def showMarker(marker: String, oldValue: Any, newValue: Any, displayValue: Any = null): Unit = {
      if (oldValue != newValue) {
        val display = if (displayValue == null) newValue.toString else displayValue.toString
        b += s"Set $marker to: $display"
      }
    }

    def showList[T](label: String, oldList: Seq[T], newList: Seq[T]): Unit = {
      if (oldList != newList) {
        wrap(label, newList) foreach (x => b += x)
      }
    }

    def showPieces(label: String, oldPieces: Pieces, newPieces: Pieces): Unit = {
      if (oldPieces != newPieces) {
        b += ""
        wrap(label, newPieces.descriptions) foreach (x => b += x)
      }
    }

    val heading = "The following changes should be made to the game board"
    println()
    println(separator(length = heading.length, char = '='))
    println("The following changes should be made to the game board")
    println(separator(length = heading.length, char = '='))

    // Show changes to spaces first
    for (fromSp <- from.spaces.sorted) {
      val toSp = to.getSpace(fromSp.name)
      val terrorName = if (fromSp.isLoC) "Sabotage markers" else "Terror markers"
      b.clear()
      showMarker("Support", fromSp.support, toSp.support)
      showMarker("Control",  fromSp.control, toSp.control)
      showMarker(terrorName, fromSp.terror, toSp.terror)
      showPieces("Pieces: ", fromSp.pieces, toSp.pieces)

      if (b.nonEmpty) {
        println(s"\nChanges to ${fromSp.name}:")
        println(separator())
        b foreach println
      }
    }

    b.clear()
    b += ""
    for (f <- List(ARVN, NVA, VC))
      if (game.trackResources(f))
        showMarker("ARVN resources", from.resources(f), to.resources(f))

    if (from.isBot(VC))
      showMarker("VC Agitate total", from.agitateTotal, to.agitateTotal)

    showMarker("US Aid", from.usAid, to.usAid)
    showMarker("Patronage", from.patronage, to.patronage)
    showMarker("Econ marker", from.econ, to.econ)
    showMarker("Trail marker", from.trail, to.trail)
    if (game.isBot(US))
      showMarker("US Policy", from.usPolicy, to.usPolicy)

    showPieces("Casualties: ", from.casualties, to.casualties)
    showPieces("Out of play: ", from.outOfPlay, to.outOfPlay)
    showPieces("Available: ", from.availablePieces, to.availablePieces)
    showList("Capabilities: ", from.capabilities, to.capabilities)
    showList("Momentum: ", from.momentum, to.momentum)
    showList("RVN Leaders: ", from.rvnLeaders, to.rvnLeaders)
    if (from.rvnLeaderFlipped != to.rvnLeaderFlipped) {
      if (to.rvnLeaderFlipped)
        println(s"Flip RVN Leader face down [${to.currentRvnLeader}]")
      else
        println(s"Flip RVN Leader face up [${to.currentRvnLeader}]")
    }

    // Score markers
    if (from.usPoints   != to.usPoints ||
        from.arvnPoints != to.arvnPoints ||
        from.nvaPoints  != to.nvaPoints ||
        from.vcPoints   != to.vcPoints) {
          
      b += ""
    }
    
    showMarker("Support + Avail US", from.usPoints, to.usPoints)
    showMarker("COIN Control + Patronage", from.arvnPoints, to.arvnPoints)
    showMarker("NVA Control + NVA Bases", from.nvaPoints, to.nvaPoints)
    showMarker("Total Opposition + VC Bases", from.vcPoints, to.vcPoints)

    if (from.sequence != to.sequence) {
      b += ""
      for {
        f <- List(US, ARVN, NVA, VC)
        if from.sequence.location(f) != to.sequence.location(f)
      } {
        b += s"Place $f cylinder in the ${to.sequence.location(f)}"
      }
    }

    b foreach println
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

  def noteIf(cond: Boolean, note: String): Option[String] = if (cond) Some(note) else None

  // Convenience method for createing choices for the askMenu() function.
  def choice[T](condition: Boolean, value: T, desc: String): Option[(T, String)] =
    if (condition) Some(value -> desc) else None

  def choice[T](condition: Boolean, value: T, desc: String, detail: Seq[String]): Option[(T, (String, Seq[String]))] =
    if (condition) Some(value -> (desc, detail)) else None

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
    if (items.size <= numChoices)
      items map (_._1)
    else
      nextChoice(1, ListMap(items:_*))
  }

  // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenuWithWrap[T](
    items: List[(T, (String, Seq[String]))],
    menuPrompt: String = "",
    numChoices: Int = 1,
    repeatsOK: Boolean = false,
    allowAbort: Boolean = true): List[T] = {

    def nextChoice(num: Int, itemsRemaining: ListMap[T, (String, Seq[String])]): List[T] = {
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
          val number = String.format(s"%${width}d) ", new Integer(i+1))
          val (desc, detail) = itemsRemaining(key)
          val prefix = s"${number}${desc} "
          wrap(prefix, detail, showNone = false) foreach println
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
  // if the number is not valid or if the card has already been played.
  def checkCardNum(cardNum: String): Boolean = {
    cardNum match {
      case INTEGER(num) if !eventDeck.isValidNumber(num.toInt) =>
        println(s"'$cardNum' is not a valid card number")
        false
        
      case INTEGER(num) if eventDeck.isPivotalCard(num.toInt) =>
        println(s"'${eventDeck(num.toInt)}' is a pivotal event card")
        false
        
      case INTEGER(num) if game.cardsSeen.contains(num.toInt) =>
        println(s"'${eventDeck(num.toInt)}' has already been drawn during this game")
        false
        
      case INTEGER(_) =>
        true
        
      case _ =>
        println(s"'$cardNum' is not a card number")
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
        println(s"Available commands: ${orList(names)}")
        println("Type help <command> for more detail")
      case Some(p) =>
        for (name <- matchOne(p, names); cmd <- cmds find (_.name == name))
          println(cmd.desc)
    }
  }


  def showCommand(param: Option[String]): Unit = {
    val options =  "scenario" :: "summary" :: "pieces" :: "events" :: "all" :: SpaceNames

    askOneOf("Show: ", options, param, allowNone = true, allowAbort = false) foreach {
      case "scenario" => printSummary(scenarioSummary)
      case "summary"  => printSummary(statusSummary)
      case "pieces"   => printPiecesSummary()
      case "events"   => printSummary(eventSummary)
      case "all"      => printGameState()
      case name       => printSummary(spaceSummary(name))
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
    println()
    for (name <- SpaceNames; line <- spaceSummary(name))
      println(line)
  }

  // Display some or all of the game log.
  // usage:
  // history            - Shows the log starting from the most recent save point (Same as history -1)
  // history -n         - Shows the log starting from the nth most recent save point
  // history n          - Shows the log starting from the nth save point
  // history n num      - Shows num log entries starting from the nth save point
  // history -n num     - Shows num log entries starting from the nth most recent save point
  // history all        - Shows the entire log (Same as history 0)
  // You may add >file to the end of any history command to write the history to disk.""".stripMargin
  def showHistory(input: Option[String]): Unit = {
    case class Error(msg: String) extends Exception
    try {
      def redirect(tokens: List[String]): Option[Pathname] = {
        tokens match {
          case Nil => None
          case x::_  if !(x startsWith ">") => None
          case ">":: Nil => throw Error("No filename specified after '>'")
          case ">"::file::_ => Some(Pathname(file))
          case file::_ => Some(Pathname(file drop 1))
        }
      }

      def printSegment(save_number: Int, last_save_number: Int, path: Option[Pathname]): Unit = {
        if (save_number <= last_save_number) {
          val header   = s"\n>>> History of save point $save_number <<<"
          val log_path = gamesDir/gameName.get/getLogName(save_number)

          val msgs = if (log_path.exists)
            log_path.readLines.toVector
          else
            Vector.empty

          path match {
            case None =>
              println(header)
              println(separator(char = '='))
              for (msg <- msgs)
                println(msg)

            case Some(path) =>
              path.appender { stream =>
                stream.write(header + lineSeparator)
                stream.write(separator() + lineSeparator)
                for (msg <- msgs)
                  stream.write(msg + lineSeparator)
              }
          }
          printSegment(save_number + 1, last_save_number, path)
        }
      }

      val maxIndex = game.history.size - 1
      val NUM      = """(-?\d+)""".r
      val ALL      = """al{0,2}""".r
      val REDIR    = """>.*""".r
      val tokens   = (input getOrElse "" split "\\s+").toList map (_.toLowerCase) dropWhile (_ == "")
      
      def indexVal(str: String): Int = str.toInt match {
        case x if x < 0 => (game.history.size + x) max 0
        case x          => x min maxIndex 
      }
      
      val (startIndex, count, redirect_path) = tokens match {
        case Nil                => (maxIndex, 0, None)
        case NUM(x)::NUM(y)::xs => (indexVal(x), y.toInt, redirect(xs))
        case NUM(x)::xs         => (indexVal(x), 0, redirect(xs))
        case ALL()::xs          => (0, 0, redirect(xs))
        case REDIR()::_         => (maxIndex, 0, redirect(tokens))
        case p::_               => throw Error(s"Invalid parameter: $p")
      }
      val endIndex = count match {
        case c if c < 1 => maxIndex
        case c          => (startIndex + (c - 1)) min maxIndex
      }
            
      // Delete any previous file before we start appending to it.
      redirect_path foreach { p =>
        if (p.isDirectory)
          throw new IllegalArgumentException(s"Cannot redirect to a directory ($p)!")
        p.delete()
      }

      printSegment(startIndex, endIndex, redirect_path)

      redirect_path foreach { p =>
        println(s"\nHistory was written to file: $p")
      }
    }
    catch {
      case e: IOException => println(s"IOException: ${e.getMessage}")
      case Error(msg) => println(msg)
    }


  }

  // Allows the user to roll back to the beginning of any turn.
  def rollback(input: Option[String]): Unit = {

    try {
      val pages = game.history.reverse.drop(1).sliding(25, 25).toList
      val firstPage = 0
      val lastPage  = pages.size -1
      val PAGE_UP   = -1
      val PAGE_DOWN = -2
      val CANCEL    = -3

      def showPage(pageNum: Int): Unit = {
        val saveChoices: List[(Int, (String, Seq[String]))] = pages(pageNum).toList map {
          case GameSegment(save_number, card, summary) => save_number -> (s"Save point ${save_number + 1} [$card]", summary)
        }
          val otherChoices: List[(Int, (String, Seq[String]))] = List(
            choice(pageNum > firstPage, PAGE_UP,   "Page up, show newer save points ", Seq.empty),
            choice(pageNum < lastPage,  PAGE_DOWN, "Page down, show older save points ", Seq.empty),
            choice(true,                CANCEL,    "Cancel, do not roll back ", Seq.empty)
          ).flatten

        println("\nRollback to the beginning of a previous save point.")
        println("The save points are displayed with the most recent first.")

        askMenuWithWrap(saveChoices:::otherChoices, "Choose a save point:").head match {
          case CANCEL      =>
          case PAGE_UP     => showPage(pageNum - 1)
          case PAGE_DOWN   => showPage(pageNum + 1)
          case save_number =>
            if (askYorN(s"Are you sure you want to rollback to this save point? (y/n) ")) {
              // Games are saved at the end of the turn, so we actually want
              // to load the file with turnNumber -1.
              val name         = gameName.get
              val oldGameState = game
              loadGameState(name, save_number)
              saveGameDescription()  // Update the description file

              // Remove all safe files that succeed this one.
              // We are exploring anew
              removeSaveFiles(name, save_number + 1)
              displayGameStateDifferences(oldGameState, game)
              throw Rollback
            }
            else
              showPage(pageNum)
        }
      }

      if (game.history.size > 1)
        showPage(0)
      else
        println("\nThere are no previous save points")
    }
    catch {
      case AbortAction =>
    }
  }

  // Remove turn files starting with the given save file number and all
  // those that follow that number.
  def removeSaveFiles(name: String, num: Int): Unit = {
    import Pathname.glob
    for {
      path    <- glob(gamesDir/name/"save-*")
      saveNum <- getSaveFileNumber(path)
              if saveNum >= num
    } path.delete()
  }


  def adjustSettings(param: Option[String]): Unit = {
    val agitate = if (game.isBot(VC)) List("agitate") else Nil
    val options = (
      List("resources", "aid", "patronage", "econ", "trail", "uspolicy", "casualties",
      "on deck card", "out of play", "capabilities", "momentum", "rvnLeaders", "pivotal",
      "eligibility", "trung", "bot log", "trung log", "human win") ::: agitate
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
      case "on deck card" => adjustOnDeckCard()
      case "capabilities" => adjustCapabilities()
      case "momentum"     => adjustMomentum()
      case "rvnLeaders"   => adjustRvnLeaders()
      case "pivotal"      => adjustPivotalCards()
      case "eligibility"  => adjustEligibility()
      case "trung"        => adjustTrungDeck()
      case "bot log"      => adjustBotDebug()
      case "trung log"    => adjustLogTrung()
      case "human win"    => adjustHumanWinInVictoryPhase()
      case name           => adjustSpace(name)
    }

    // We throw an Adjustment exception to return to
    // the mainLoop().
    // This allows us to check if the adjustment allows a
    // faction to play their pivotal event.
    throw Adjustment
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
        saveGameState(desc)}
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
      saveGameState(desc)
      logPointsChanges(origGame, game)
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
          val origNum = casualties.totalOf(pieceType)
          val maxNum  = available.totalOf(pieceType) + casualties.totalOf(pieceType)

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
        saveGameState("Adjusted Casualties")
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
          val origNum = outOfPlay.totalOf(pieceType)
          val maxNum  = available.totalOf(pieceType) + outOfPlay.totalOf(pieceType)

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
        saveGameState("Adjusted Out of Play pieces")
      }
    }
  }

  def adjustOnDeckCard(): Unit = {
    if (game.numCardsDrawn <= 1)
      println("\nNo on deck card has been drawn")
    else {
      println(s"\nThe on deck card is: ${eventDeck(game.onDeckCard)}")
      val oldOnDeckNum = game.onDeckCard
      val newOnDeckNum = askCardNumber("\nEnter the number of the on deck card: ")
      if (newOnDeckNum != oldOnDeckNum) {
        game = game.copy(onDeckCard = newOnDeckNum, cardsSeen = game.cardsSeen.dropRight(1) :+ newOnDeckNum)
        log(adjustmentDesc(s"On Deck Card", eventDeck(oldOnDeckNum).toString, eventDeck(newOnDeckNum).toString))
        saveGameState("Adjusted On Deck Card")
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
      saveGameState("Adjusted RVN Leaders")

  }

  def adjustCapabilities(): Unit = {
    val AllCapNames = AllUnshadedCaps map (_.name)
    val RemoveCap = """remove-(.*)""".r
    def askCapability(candidates: List[Capability]): Option[Capability] = {
        val capChoices: List[(Option[Capability], String)] = candidates map { cap =>
          Some(cap) -> s"Add $cap"
        }
        val choices = capChoices :+ (None -> "Do not add a capability")
        askMenu(choices, "\nChoose one:", allowAbort = false).head
    }

    def nextAdjustment(): Unit = {
      val included = game.capabilities map (_.name)
      val excluded = AllCapNames filterNot included.contains

      val addChoices = List(
        choice(excluded.nonEmpty, "add-unshaded", "Add unshaded capability"),
        choice(excluded.nonEmpty, "add-shaded",   "Add shaded capability")
      ).flatten

      val removeChoices = game.capabilities map (cap => s"remove-${cap.name}" -> s"Remove ${cap}")
      val choices = addChoices ::: removeChoices ::: List("finished" -> "Finished adjusting capabilities")

      println("\nCapabilities in play")
      println(separator())
      wrap("", game.capabilities map (_.toString)) foreach println
      askMenu(choices, "\nChoose one:", allowAbort = false).head match {

        case "add-unshaded" =>
          val caps = excluded flatMap (name => AllUnshadedCaps find (_.name == name))
          askCapability(caps) foreach { cap =>
            game = game.copy(capabilities = cap :: game.capabilities)
            addOngoingEvent(cap.name)
            log(s"Adjusted capabilities: [added ${cap}]")
          }
          nextAdjustment()

        case "add-shaded" =>
          val caps = excluded flatMap (name => AllShadedCaps find (_.name == name))
          askCapability(caps) foreach { cap =>
            game = game.copy(capabilities = cap :: game.capabilities)
            addOngoingEvent(cap.name)
            log(s"Adjusted capabilities: [added ${cap}]")
          }
          nextAdjustment()

        case RemoveCap(name) =>
          game.capabilities find (_.name == name) foreach { cap =>
            game = game.copy(capabilities = game.capabilities filterNot (_ == cap))
            removeOngoingEvent(name)
            log(s"Adjusted capabilities: [removed ${cap}]")
          }
          nextAdjustment()

        case _ =>
      }
    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.capabilities != game.capabilities)
      saveGameState("Adjusted Capabilities in play")
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
            addOngoingEvent(mo)
            log(s"Adjusted momentum cards: [added ${mo}]")
          }
          nextAdjustment()

        case mo =>
          game = game.copy(momentum = game.momentum filterNot (_ == mo))
          log(s"Adjusted momentum cards: [removed ${mo}]")
          removeOngoingEvent(mo)
          nextAdjustment()
      }
    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.momentum != game.momentum)
      saveGameState("Adjusted Momentum cards in play")
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
      saveGameState("Adjusted Pivotal cards availablity")
  }

  def adjustEligibility(): Unit = {
    var seq = game.sequence

    def removeFaction(faction: Faction, seq: SequenceOfPlay): SequenceOfPlay = {
      seq.copy(
        eligibleThisTurn = seq.eligibleThisTurn - faction,
        actors           = seq.actors filterNot (_.faction == faction),
        passed           = seq.passed - faction
      )
    }

    def nextAdjustment(): Unit = {
      val choices = Faction.ALL.toList.map(f => Some(f) -> f.toString()) :+
                    (None -> "Finished adjusting eligibility")

      println("\nAdjusting Faction Eligibility")
      println(separator())
      sequenceList(eventDeck(game.currentCard), seq) foreach println

      askMenu(choices, "\nSelect faction:", allowAbort = false).head match {
        case None =>
        case Some(faction) =>
          val canEvent  = !seq.actors.exists(_.action == Event)
          val canOpSa   = !seq.actors.exists(_.action == OpPlusSpecial)
          val canOpOnly = !seq.actors.exists(_.action == OpOnly)
          val canLimOp  = !seq.actors.exists(_.action == LimitedOp)
          val canPass   = !seq.passed(faction)
          val canElig   = !seq.eligibleThisTurn(faction)
          val canInelig = !seq.ineligibleThisTurn(faction)
          val options = List(
            choice(canEvent,  "event",   s"Event box"),
            choice(canOpSa,   "op-sa",   s"Op+Special Activity box"),
            choice(canOpOnly, "on-only", s"Op Only box"),
            choice(canLimOp,  "lim-op",  s"Limited Op box"),
            choice(canPass,   "pass",    s"Passed box"),
            choice(canElig,   "elig",    s"Eligible box"),
            choice(canInelig, "inelig",  s"Ineligible box"),
            choice(true,      "no-op",   s"Do not move the $faction cylinder")
          ).flatten

          val opt = askMenu(options, s"\nChoose location for $faction cylinder:").head
          opt match {
            case "event"   => seq = removeFaction(faction, seq).addActor(faction, Event)
            case "op-sa"   => seq = removeFaction(faction, seq).addActor(faction, OpPlusSpecial)
            case "on-only" => seq = removeFaction(faction, seq).addActor(faction, OpOnly)
            case "lim-op"  => seq = removeFaction(faction, seq).addActor(faction, LimitedOp)
            case "pass"    => seq = removeFaction(faction, seq).addActor(faction, Pass)
            case "elig"    => seq = removeFaction(faction, seq).copy(eligibleThisTurn = seq.eligibleThisTurn + faction)
            case "inelig"  => seq = removeFaction(faction, seq)
            case _  =>
          }
          if (opt != "no-op")
            nextAdjustment()
      }
    }

    nextAdjustment()

    if (seq != game.sequence) {
      game = game.copy(sequence = seq)
      val desc = "Adjusted faction eligibility"
      log(desc)
      saveGameState(desc)
    }
  }


  def adjustTrungDeck(): Unit = {

    def nextAdjustment(): Unit = {
      val current = game.trungDeck(1)

      val choices: List[(Option[TrungCard], String)] =
          (game.trungDeck map (tc =>  Some(tc) -> tc.toString)) :+
          (None -> "Finished adjusting the Trung deck")

      println(s"\n$current is second from the top")
      askMenu(choices, "\nChoose which Trung card should second from the top:").head match {
        case None =>
        case Some(tc) =>
          //  The top card is alwasy put on the bottom of the deck
          //  So the next card drawn is the second card in the stack.
          val others = game.trungDeck filterNot (_ == tc)
          val newDeck = others.head::tc::others.tail
          game = game.copy(trungDeck = newDeck)
          log(s"Adjusted Trung deck: [$tc now second from the top]")
          nextAdjustment()
      }
    }

    val savedGame = game
    nextAdjustment()
    if (savedGame.trungDeck != game.trungDeck)
      saveGameState("Adjusted Trung Deck")
  }


  def adjustBotDebug(): Unit = {
    val newValue = !game.botDebug
    val desc = adjustmentDesc("Bot Debug Logging", game.botDebug, newValue)
    game = game.copy(botDebug = newValue)
    log(desc)
    saveGameState("Bot Debug Logging")
  }
  
  //  Whether or not to log dice rolls in Trung decisions
  def adjustLogTrung(): Unit = {
    val newValue = !game.logTrung
    val desc = adjustmentDesc("Log Trung checks", game.logTrung, newValue)
    game = game.copy(logTrung = newValue)
    log(desc)
    saveGameState("Log Trung checks")
  }

  def adjustHumanWinInVictoryPhase(): Unit = {
    val newValue = !game.humanWinInVictoryPhase
    val desc = adjustmentDesc("Allow Human Win in Victory Phase", game.humanWinInVictoryPhase, newValue)
    game = game.copy(humanWinInVictoryPhase = newValue)
    log(desc)
    saveGameState("Adjust Human Win in Victory Phase")
    
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
      val sp         = game.getSpace(name)
      val adjSupport = sp.canHaveSupport
      val adjPieces  = game.availablePieces.nonEmpty || sp.pieces.nonEmpty
      val marker     = if (sp.isLoC) "Sabotage" else "Terror"
      val choices    = List(
        choice(adjSupport, "support", "Support level"),
        choice(true,       "terror",  s"Number of $marker markers"),
        choice(adjPieces,  "pieces",  "Faction pieces"),
        choice(true,       "done",   s"Finished adjusting $name")
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
  }


  def adjustSupport(name: String): Unit = {
    val sp = game.getSpace(name)
    val origGame = game
    val origSupport = sp.support
    val options = List(ActiveSupport, PassiveSupport, Neutral, PassiveOpposition, ActiveOpposition)
    val choices = options map (opt => opt -> opt.name)

    println()
    println(s"Choose support level for ${name}:")
    val newSupport = askMenu(choices, allowAbort = false).head
    if (newSupport != origSupport) {
      game = game.updateSpace(sp.copy(support = newSupport))
      val desc = spaceAdjustmentDesc(name, "support", sp.support, newSupport)
      log(desc)
      saveGameState(desc)
      logPointsChanges(origGame, game)
    }
  }

  def adjustTerror(name: String): Unit = {
    val sp     = game.getSpace(name)
    val orig   = sp.terror
    val marker = if (sp.isLoC) "Sabotage" else "Terror"
    val maxVal = game.terrorMarkersAvailable + sp.terror
    println()
    adjustInt(s"Number of $marker markers", sp.terror, 0 to maxVal) foreach { value =>
      if (value != orig) {
        val desc = spaceAdjustmentDesc(name, marker, sp.terror, value)
        game = game.updateSpace(sp.copy(terror = value))
        log(desc)
        saveGameState(desc)
      }
    }
  }


  def adjustPieces(name: String): Unit = {
    val origPieces = game.getSpace(name).pieces
    def nextAdjustment(): Unit = {
      val sp        = game.getSpace(name)
      val forbidden: Set[PieceType] = if (sp.isLoC) (CoinBases:::InsurgentBases).toSet else Set.empty
      val pieces    = sp.pieces.except(forbidden)
      val available = game.availablePieces.except(forbidden)
      val pieceChoices: List[(Option[PieceType], String)] = AllPieceTypes flatMap { t =>
        if (!forbidden(t) &&
           pieces.has(t) || available.has(normalizedType(t)) &&
           (!isBase(t) || pieces.only(BasePieces).except(t).total < 2))
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
          val origNum = pieces.totalOf(pieceType)
          val maxNum  = {
            val n = available.totalOf(normalizedType(pieceType)) + pieces.totalOf(pieceType)
            if (isBase(pieceType)) {
              val maxBase = 2 - pieces.only(BasePieces).except(pieceType).total
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

    if (game.getSpace(name).pieces != origPieces)
      saveGameState(s"Adjusted pieces in $name")
  }
}