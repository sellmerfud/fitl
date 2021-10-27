
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
import Bot.{ TrungCard, TrungDeck }

object FireInTheLake {

  val INTEGER = """(\d+)""".r

  def d6 = nextInt(6) + 1
  def d3 = nextInt(3) + 1
  def rollDice(numDice: Int) = List.fill(numDice)(d6).sum

  val EdgeTrackMax = 75
  val TrailMin     = 0
  val TrailMax     = 4

  val TerrorMarkerManifest = 15

  val USPolicy_JFK   = "JFK"
  val USPolicy_LBJ   = "LBJ"
  val USPolicy_Nixon = "Nixon"

  sealed abstract class Faction(val name: String, val sortOrder: Int, val pivotCard: Int) {
    override def toString() = name
  }

  case object US   extends Faction("US",   1, PivotalUS)
  case object ARVN extends Faction("ARVN", 2, PivotalARVN)
  case object VC   extends Faction("VC",   3, PivotalVC)
  case object NVA  extends Faction("NVA",  4, PivotalNVA)

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

  sealed trait BotEventPriority
  case object NotExecuted extends BotEventPriority
  case object Critical    extends BotEventPriority
  case object Performed   extends BotEventPriority


  sealed trait EventType
  case object Unshaded extends EventType
  case object Shaded   extends EventType

  type EventEffective = Faction => Boolean  // Used to see if event would be effective for Bot
  type EventExecution = Faction => Unit     // Execute the event for Human and Bot

  // For cards that have only one event:
  //  dual == false and the event condtions and execution use the `unshaded` fields
  class EventCard(
    val number: Int,
    val name: String,
    val dual: Boolean,
    val factionOrder: List[Faction],
    val eventData: ListMap[Faction, (BotEventPriority, EventType)],  // Empty
    val unshadedEffective: EventEffective,
    val shadedEffective: EventEffective,
    val executeUnshaded: EventExecution,
    val executeShaded: EventExecution) {


    val isCoup = factionOrder.isEmpty

    def numAndName = s"#$number - $name"
    override def toString() = numAndName
    
    def eventPriority(faction: Faction) = eventData(faction)._1
    def eventType(faction: Faction)     = eventData(faction)._2

    def orderString  = factionOrder map (_.name) mkString ", "
    def fullString = if (isCoup) s"$numAndName (Coup)" else s"$numAndName ($orderString)"
    
    def eventEffective(faction: Faction) = eventType(faction) match {
      case Unshaded => unshadedEffective(faction)
      case Shaded   => shadedEffective(faction)
    }

    def executeEvent(faction: Faction) = eventType(faction) match {
      case Unshaded => executeUnshaded(faction)
      case Shaded   => executeShaded(faction)
    }
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

  val OutsideSouth =  List(NorthVietnam, CentralLaos, SouthernLaos, NortheastCambodia,
                           TheFishhook, TheParrotsBeak, Sihanoukville)
  
  val LaosCambodia =  List(CentralLaos, SouthernLaos, NortheastCambodia,
                           TheFishhook, TheParrotsBeak, Sihanoukville)
  
  // Order space name Cities first then Provinces the LoCs
  // Within each category sort alphabetically
  val SpaceNameOrdering = new Ordering[String] {
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
  def areAdjacent(name1: String, name2: String): Boolean = getAdjacent(name1) contains name2
  def getAdjacentLOCs(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isLoC }
  def getAdjacentCities(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isCity }
  def getAdjacentProvinces(name: String): Set[String] = getAdjacent(name) filter { x => game.getSpace(x).isProvince }
  def getAdjacentLOCsAndCities(name: String): Set[String] = getAdjacentLOCs(name) ++ getAdjacentCities(name)
  def getAdjacentNonLOCs(name: String): Set[String] = getAdjacentProvinces(name) ++ getAdjacentCities(name)
  
  def isInLaosCambodia(name: String) = LaosCambodia contains name
  def isOutsideSouth(name: String)   = OutsideSouth contains name
  def isInSouthVietnam(name: String) = !isOutsideSouth(name)
  
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
  
  
  //  Return a sequence of all spaces that can be reached from the given space by Transported pieces.
  //  A piece MAY move onto an adjacent LOC.  Then can continue moving through adjacent LOC/Cities.
  //  Finally MAY then move to an adjacent space (Never N. Vietnam)
  //  It it ever enters a spcace with any Insurgent piece, then it must stop.
  //  if RVN_Leader_NguyenKhanh is in play then only 1 LOC can be used
  def getTransportDestinations(srcName: String): Seq[String] = {
    val nguyen_khanh     = isRVNLeader(RVN_Leader_NguyenKhanh)  
    
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
    
    (immediateDests ++ locDests).toList.sorted
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

    def totalOf(pieceTypes: TraversableOnce[PieceType]): Int =
      pieceTypes.foldLeft(0) { (num, piece) => num + totalOf(piece) }

    // Return true of this Pieces instance contains at least all of the specified pieces.
    def contains(query: Pieces): Boolean = AllPieceTypes forall (t => totalOf(t) >= query.totalOf(t))

    // Return true if this Pieces instance has at least one of the given piece type
    def has(pt: PieceType): Boolean = totalOf(pt) > 0

    // Return true if this Pieces instance has at least one of any of the given piece types
    def has(pts: TraversableOnce[PieceType]): Boolean = totalOf(pts) > 0

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

    def only(pieceTypes: TraversableOnce[PieceType]): Pieces =
      pieceTypes.foldLeft(Pieces()) { (pieces, t) => pieces.add(totalOf(t), t) }

    def only(pieceType: PieceType): Pieces = Pieces().add(totalOf(pieceType), pieceType)

    def except(pieceTypes: TraversableOnce[PieceType]): Pieces =
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
    def fromTypes(collection: TraversableOnce[PieceType]) =
      collection.foldLeft(Pieces()) { (pieces, t) => pieces.add(1, t) }
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

    def supportValue: Int = if (isLoC)
      0
    else 
      support match {
        case PassiveSupport => population
        case ActiveSupport  => 2 * population
        case _              => 0
      }
      
    def oppositionValue: Int = if (isLoC)
      0
    else
      support match {
        case PassiveOpposition => population
        case ActiveOpposition  => 2 * population
        case _                 => 0
      }

    def coinControlValue: Int = if (!isLoC && coinControlled) population else 0
    def nvaControlValue: Int  = if (!isLoC && nvaControlled)  population else 0

    def totalBases = pieces.totalOf(BasePieces)
    def addPieces(newPieces: Pieces): Space = copy(pieces = pieces + newPieces)
    def removePieces(removedPieces: Pieces): Space = copy(pieces = pieces - removedPieces)
    def setPieces(newPieces: Pieces): Space = copy(pieces = newPieces)

    def assaultCubes(faction: Faction): Int = faction match {
      case US                      => pieces.totalOf(USTroops)
      case ARVN if isCity || isLoC => pieces.totalOf(ARVNCubes)
      case ARVN                    => pieces.totalOf(ARVNTroops)
      case _                       => 0
    }
    
    def sweepForces(faction: Faction): Int = faction match {
      case US   => pieces.totalOf(USTroops::Irregulars_U::Irregulars_A::Nil)
      case ARVN => pieces.totalOf(ARVNTroops::ARVNPolice::Rangers_U::Rangers_A::Nil)
      case _    => 0
    }

    def assaultMultiplier(faction: Faction): Double = faction match {
      case US if pieces.has(USBase) => 2.0
      case US if isHighland         => 1.0/2.0
      case US                       => 1.0
      case ARVN if isHighland       => 1.0/3.0
      case ARVN                     => 1.0/2.0
      case _                        => 0.0
    }

    def assaultFirepower(faction: Faction): Int = (assaultCubes(faction) * assaultMultiplier(faction)).toInt
    
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

    def remainIneligible(faction: Faction): SequenceOfPlay =
      copy(eligibleThisTurn   = eligibleThisTurn - faction,   // Ineligible now and through next turn
           ineligibleNextTurn = ineligibleNextTurn + faction,
           eligibleNextTurn   = eligibleNextTurn - faction)

    def makeIneligible(faction: Faction): SequenceOfPlay =
        copy(eligibleThisTurn = eligibleThisTurn - faction)

    // Called when two factions have acted, or all have passed
    // Adjusts eligibility for the following turn
    def adjustEligibility(): SequenceOfPlay = {
      SequenceOfPlay(eligibleThisTurn = eligibleThisTurn   ++ 
                                        ineligibleThisTurn ++
                                        passed             ++ 
                                        eligibleNextTurn   --
                                        ineligibleNextTurn)
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
  case class GameSegment(save_number: Int, card: String, summary: Seq[String])

  //  Note: we calculate the 'Available' pieces by starting with the Force Pool totals
  //        and subtracting pieces on the map, casualties, and out of play.
  case class GameState(
    scenarioName: String,
    humanFactions: Set[Faction],
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
    trungDeck: List[TrungCard]        = Nil,  // The head of the list is the top of the deck
    momentum: List[String]            = Nil,
    sequence: SequenceOfPlay          = SequenceOfPlay(),
    cardsDrawn: Int                   = 0,
    currentCard: Int                  = 0,
    onDeckCard: Int                   = 0,
    prevCardWasCoup: Boolean          = false,
    coupCardsPlayed: Int              = 0,  // Number of Coup cards played/ignored thus far
    botLogging: Boolean               = false,
    history: Vector[GameSegment]      = Vector.empty,
    log: Vector[String]               = Vector.empty) {  // Log of the cuurent game segment


    lazy val allPiecesOnMap = spaces.foldLeft(Pieces()) { (total, space) => total + space.pieces }
    lazy val availablePieces = ForcePool - allPiecesOnMap.normalized - casualties.normalized - outOfPlay.normalized
    // piecesToPlace are either on the map or available (not casualties or out of play)
    // Does not include US troops or bases
    lazy val piecesToPlace = (allPiecesOnMap.normalized + availablePieces).except(USTroops::USBase::Nil)
    lazy val currentRvnLeader = rvnLeaders.head
    lazy val locSpaces = spaces filter (_.isLoC)
    lazy val nonLocSpaces = spaces filterNot (_.isLoC)

    val agitateTotal = vcResources

    def actionSummary: Seq[String] = {
      val b = new ListBuffer[String]

      if (isCoupRound) {
        s"${ordinal(coupCardsPlayed + 1)} Coup! round"
      }
      else {
        actingFaction foreach { faction =>
          b += s"${faction} is up"
        }
        if (sequence.actors.nonEmpty)
          b ++= sequence.actors map (_.toString)
        if (sequence.passed.nonEmpty)
          b += s"${andList(sequence.passed)} passed"
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
    
    lazy val useEcon = trackResources(ARVN)

    // Return faction that is currently the next available or None
    def actingFaction: Option[Faction] = if (sequence.exhausted)
      None
    else
      deck(currentCard).factionOrder find sequence.eligibleThisTurn
    
    // Return faction that would be following the acting faction 
    // on the current card.
    def followingFaction: Option[Faction] = actingFaction match {
      case Some(active) if (sequence.numActors == 0) =>
        deck(currentCard).factionOrder filter (_ != active) find sequence.eligibleThisTurn
      case _ => None
    }
    
    
    def executingPivotalEvent = deck.isPivotalCard(currentCard) && sequence.numActors == 0

    def isFinalCampaign = coupCardsPlayed == (totalCoupCards - 1)
    def isCoupRound = cardsDrawn > 0 && deck(currentCard).isCoup
    def onDeckIsCoup = onDeckCard > 0 &&  deck(onDeckCard).isCoup
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
      else if (game.onDeckCard == 0)
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
    if (game.trackResources(ARVN))
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
    
    b += separator()
    b += s"RVN Leader     : ${game.currentRvnLeader}"
    if (game.cardsDrawn > 0) {
      b += s"Current card   : ${deck(game.currentCard).fullString}"
      if (game.onDeckCard > 0) {
        b += s"On deck card   : ${deck(game.onDeckCard).fullString}"
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

    def addPieces(types: TraversableOnce[PieceType]): Unit = {
      for (t <- types; name = t.genericPlural; count = avail.totalOf(t))
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

    def addPieces(types: TraversableOnce[PieceType]): Unit = {
      for (t <- types; name = t.genericPlural; count = game.casualties.totalOf(t) if count > 0)
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

    def addPieces(types: TraversableOnce[PieceType]): Unit = {
      for (t <- types; name = t.genericPlural; count = game.outOfPlay.totalOf(t) if count > 0)
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

  def spaceNames(spaces: Traversable[Space]): List[String] = (spaces map (_.name)).toList.sorted
  def spaces(names: Traversable[String]): List[Space] = (names map game.getSpace).toList.sortBy(_.name)

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
            println(s"A game called '$name' already exists.")
            if (askYorN(s"Do you want to overwrite the existing game (y/n)? ")) {
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

  def main(args: Array[String]): Unit = {
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
            val num = askInt("How many factions will be played by human players", 0, 4, Some(1), allowAbort = false)
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

          println()
          gameName = Some(askGameName("Enter a name for your new game: "))

          game = initialGameState(scenario, humanFactions, usePeriodEvents)

          log("Start of Game")
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


  def initialGameState(scenario: Scenario, humanFactions: Set[Faction], usePeriodCapabilities: Boolean) = {
    val trungDeck = shuffle(TrungDeck filterNot (card => humanFactions(card.faction)))
    var spaces    = DefaultSpaces
    
    // Apply scenario overrides to countries.
    for (sp <- scenario.spaces)
      spaces = sp :: (spaces filterNot (_.name == sp.name))

    GameState(
      scenario.name,
      humanFactions,
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
      scenario.rvnLeadersInPlay,
      trungDeck
    )
  }


    // Save a brief description of the game.
  // The descriptions are used by the askWhichGame() function.
  def saveGameDescription(): Unit = {
    assert(gameName.nonEmpty, "saveGameDescription(): called with gameName not set!")
    val summary = game.actionSummary.headOption getOrElse ""
    val desc = s"${game.scenarioName}  [${deck(game.currentCard)}] $summary"
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

  def saveGameState(): Unit = {
    assert(gameName.nonEmpty, "saveGameState(): called with gameName not set!")

    val save_number = game.history.size
    val save_path   = gamesDir/gameName.get/getSaveName(save_number)
    val log_path    = gamesDir/gameName.get/getLogName(save_number)
    val segment = GameSegment(save_number, deck(game.currentCard).toString, game.actionSummary)

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

  
  def getSaveFileNumber(filename: String): Option[Int] = {
    val SAVE_FILE = """save-(\d+)""".r
    filename match {
      case SAVE_FILE(n) => Some(n.toInt)
      case _            => None
    }
  }

  def getSaveName(save_number: Int) = s"save-$save_number"
  def getLogName(save_number: Int)  = s"log-$save_number"

  // Given a directory for a saved game finds the most recent save file.
  def mostRecentSaveNumber(name: String): Option[Int] = {
    val dir = gamesDir/name
    if (dir.isDirectory) {
      val entries = dir.children(withDirectory = false) flatMap { child =>
        val filename = child.toString
        getSaveFileNumber(filename) 
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
      println("Which game would you like to play:")
      askMenu(choices, allowAbort = false).head match {
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

  object ShowCmd extends Command {
    val name = "show"

    val desc = """|Display the current game state
                  |  show scenario  - name of the current scenario
                  |  show summary   - current score, resources, etc.
                  |  show pieces    - available pieces, casualties, out of play pieces
                  |  show events    - capabilities, momentum, pivotal events
                  |  show sequence  - current sequence of play
                  |  show all       - entire game state
                  |  show <space>   - state of a single space""".stripMargin

  }

  object HistoryCmd extends Command {
    val name = "history"
    val desc = """|Display game history
                  |  history            - Shows the log from the beginning of the most recent save point
                  |  history n          - Shows the log from the beginning of the nth most recent save point
                  |  history all        - Shows the entire log
                  |  history  >file     - Same as above but writes the history to a file
                  |  history n >file    - Same as above but writes the history to a file
                  |  history all >file  - Same as above but writes the history to a file""".stripMargin

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
  // Cannot draw pivotal event cards!
  def isValidCardDraw(cardNum: Int): Boolean = deck.isValidNumber(cardNum) && !deck.isPivotalCard(cardNum)

  // Prompt the user for one or two cards if necessary.
  // Then update the game state with the new card numbers.
  def drawNextCard(): Unit = {
    val newSequence = game.sequence.adjustEligibility()
    if (game.cardsDrawn == 0) {
      val card1 = askCardNumber("\nEnter the number of the first Event card: ")
      val card2 = askCardNumber("Enter the number of the second Event card: ")
      game = game.copy(currentCard  = card1,
                       onDeckCard   = card2,
                       cardsDrawn   = 2,
                       sequence     = newSequence)
    }
    else {
      val nextCard = askCardNumber("\nEnter the number of the next Event card: ")
      game = game.copy(currentCard  = game.onDeckCard,
                       onDeckCard   = nextCard,
                       cardsDrawn   = game.cardsDrawn + 1,
                       sequence     = newSequence)
    }

    log()
    log(s"Current card: ${deck(game.currentCard)}")
    log(s"On deck card: ${deck(game.onDeckCard)}")
  }

  // Resolve the Coup phase, then reset the sequence of play and draw the next card.
  // ------------------
  // Mo_Oriskany            - Shaded (prohibits degrade of trail) (includes air strike, coup round, NOT evnts!)
  // Mo_Medevac_Unshaded    - In Commitment Phase (immediately move all US TROOPS in CASUALTIES to AVAILABLE,
  //                          no TROOPS go out of play.  See note: For effect when #73 Great Society is played.
  // Mo_BlowtorchKomer      - Pacify costs 1 resource per step/terror, during Support phase
  // MandateOfHeaven_Shaded - ARVN Pacify is maximum 1 space (instead of 4)
  // Cadres_Unshaded        - VC to Agigate must remove 2 VC guerrillas per space (or not possible there)
  // RVN_Leader_NguyenCaoKy - US/ARVN pacification costs 4 resources per Terror/Level
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
  @tailrec def processActorCommand(faction: Faction): Unit = {
    val upNext    = s"  ($faction is up next)"
    val actorCmds = if (game.isBot(faction)) List(BotCmd) else List(ActCmd)
    val opts      = orList((actorCmds map (_.name)) :+ "?")
    val prompt = {
      val promptLines = new ListBuffer[String]
      promptLines += ""
      promptLines += s">>> $faction turn <<<"
      promptLines += separator()
      promptLines ++= sequenceList
      promptLines += s"($opts): "
      promptLines.mkString("\n", "\n", "")
    }

    val (cmd, param) = askCommand(prompt, actorCmds ::: CommonCmds)

    cmd match {
      case ActCmd  =>
        Human.act()
        log(s"\nFinished with $faction turn")

      case BotCmd  =>
        Bot.act()
        log(s"\nFinished with $faction turn")

      case _ =>
        doCommonCommand(cmd, param)
        processActorCommand(faction)
    }
  }

  @tailrec def mainLoop(): Unit = {
    try {
      val savedState = game
      try {
        if (game.isCoupRound) {
          resolveCoupCard()
          // TODO:  Need to check if we have just played the
          //        final Coup! card.  If so call endGameCommand()
          //        which should let the user do common commands 
          //        including rollback.
          drawNextCard()
          saveGameState()
        }
        else {
        
          // TODO:  We must check to see if nay faction wishes to play it's pivotal event.
          //        If so we replace the current card with the approprate event card.
          // ....

          processActorCommand(game.actingFaction.get)
          
          // If no more factions can act on the current card
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
          println(separator())
          displayGameStateDifferences(game, savedState)
          game = savedState
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
    //   }
    // }
  }

  //  Draw the next Trung Card for the given faction.
  //  The game state is updated and the topmost card
  //  is returned.
  def drawTrungCard(faction: Faction): TrungCard = {
    var deck = game.trungDeck
        
    // First the topmost card is place on the bottom
    // Then continue drawing until we get a card for the 
    // given faction.    
    do {
      deck = deck.tail :+ deck.head  
    } while (deck.head.faction != faction)
    
    game = game.copy(trungDeck = deck)
    deck.head
  }

  def factionPasses(faction: Faction): Unit = {
    game = game.copy(sequence = game.sequence.addActor(faction, Pass))
    val amount = if (faction == US || faction == ARVN) 3 else 1
    log()
    log(s"$faction faction passes")
    log(separator())
    increaseResources(faction, amount)
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
  
  def setEconValue(amount: Int): Unit = if (game.useEcon) {
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
    log(s"Set Agitate Total to ${game.vcResources}")
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


  def increaseSupport(name: String, num: Int): Unit = if (num > 0) {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      if (!sp.isLoC && sp.population > 0) {
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
  }

  def decreaseSupport(name: String, num: Int): Unit = if (num > 0) {
    loggingPointsChanges {
      val sp = game.getSpace(name)
      if (sp.population > 0) {
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
  }

  def isRVNLeader(name: String) = game.currentRvnLeader == name

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
      for (desc <- pieces.descriptions)        
        log(s"Place $desc from AVAILABLE into $spaceName")
    }
  }

  def removeToAvailable(spaceName: String, pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
    loggingControlChanges {
      val sp = game.getSpace(spaceName)
      assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
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
      assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
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
  
  def removeAvailableToCasualties(pieces: Pieces, reason: Option[String] = None): Unit = if (pieces.total > 0) {
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
        log(s"Remove $desc from AVAILABLE to CASUALTIES")
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
      irregulars_A    = visible.irregulars_U,
      rangers_A       = visible.rangers_U,
      nvaGuerrillas_A = visible.nvaGuerrillas_U,
      vcGuerrillas_A  = visible.vcGuerrillas_U)
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
    assert(srcSpace.pieces contains pieces, s"$source does not contain all requested pieces: $pieces")
    
    val updatedSrc = srcSpace.copy(pieces = srcSpace.pieces - pieces)
    val updatedDst = dstSpace.copy(pieces = dstSpace.pieces + pieces)
    loggingControlChanges {
      game = game.updateSpace(updatedSrc).updateSpace(updatedDst)
      
      for (desc <- pieces.descriptions)
        log(s"Move $desc from $source to $dest")      
    }
  }

  def addTunnelMarker(spaceName: String, base: PieceType): Unit = {
    val sp = game.getSpace(spaceName)
    assert(base == VCBase || base == NVABase, s"addTunnelMarker() called with invalid type: $base")
    assert(sp.pieces.has(base), s"addTunnelMarker() $spaceName does not contain a $base")
    val tunnel = if (base == VCBase) VCTunnel else NVATunnel
    val updated = sp.copy(pieces = sp.pieces.remove(1, base).add(1, tunnel))
    game = game.updateSpace(updated)
    log(s"\nAdd a tunnel marker to a ${base.singular} in $spaceName")
  }

  def removeTunnelMarker(spaceName: String, tunnel: PieceType): Unit = {
    val sp = game.getSpace(spaceName)
    assert(tunnel == VCTunnel || tunnel == NVATunnel, s"removeTunnelMarker() called with invalid type: $tunnel")
    assert(sp.pieces.has(tunnel), s"removeTunnelMarker() $spaceName does not contain a $tunnel")
    val base = if (tunnel == VCTunnel) VCBase else NVABase
    val updated = sp.copy(pieces = sp.pieces.remove(1, tunnel).add(1, base))
    game = game.updateSpace(updated)
    log(s"\nRemove a tunnel marker from a ${base.singular} in $spaceName")
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

          def nextType(types: TraversableOnce[PieceType]): Unit = {
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
        val num = askInt(s"Remove how many ${pieceType.genericPlural}", minFromSpace, numInSpace)
        val toRemove = askPieces(pieces, num, types)

        nextSpace(removed :+ (name -> toRemove), candidates filterNot (_ == name))
      }
    }

    val removed = nextSpace(Vector.empty, candidateNames)

    if (removed.nonEmpty)
      loggingControlChanges {
        log()
        for ((name, removedPieces) <- removed) {
          val sp = game.getSpace(name)
          val updated = sp.copy(pieces = sp.pieces - removedPieces)
          game = game.updateSpace(updated)
          for (desc <- removedPieces.descriptions)        
            log(s"Remove $desc from $name to AVAILABLE")
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

  // The User has already determined how many of a given piece type to place.
  // If there are not enough of the pieces in the available box, then we must
  // ask for vountariy removal.
  def ensurePieceTypeAvailable(pieceType: PieceType, num: Int): Unit = {
    assert(game.piecesToPlace.totalOf(pieceType) >= num, "askPieceTypeToPlace: Not enought pieces can be placed")
    
    if (game.availablePieces.totalOf(pieceType) <  num) {
      val mustRemove = num - game.availablePieces.totalOf(pieceType)
      
      println(s"\nThere are not enough ${pieceType.genericPlural} in the available box")
      voluntaryRemoval(mustRemove, pieceType)
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
            if !orig.isLoC && orig.control != space.control
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
  def andList(x: TraversableOnce[Any]) = x.toSeq match {
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
      b.clear
      showMarker("Support", fromSp.support, toSp.support)
      showMarker(terrorName, fromSp.terror, toSp.terror)
      showPieces("Pieces: ", fromSp.pieces, toSp.pieces)

      if (b.nonEmpty) {
        println(s"\nChanges to ${fromSp.name}:")
        println(separator())
        b foreach println
      }
    }

    b.clear
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
  //   history            ##  Shows the log from the beginning of most recent save point
  //   history n          ##  Shows the log from the beginning of the nth the most recent save point
  //   history all        ##  Shows the entire log
  //   history  >file     ##  Same as above but log to a file instead of the terminal
  //   history n >file
  //   history all >file
  def showHistory(input: Option[String]): Unit = {
    case class Error(msg: String) extends Exception
    try {
      def redirect(tokens: List[String]): Option[Pathname] = {
        tokens match {
          case Nil => None
          case x::xs  if !(x startsWith ">") => None
          case ">":: Nil => throw Error("No filename specified after '>'")
          case ">"::file::xs => Some(Pathname(file))
          case file::xs => Some(Pathname(file drop 1))
        }
      }

      def printSegment(save_number: Int, path: Option[Pathname]): Unit = {
        if (save_number < game.history.size) {
          val header   = s"\n>>> History of save point $save_number <<<"
          val log_path = gamesDir/gameName.get/getLogName(save_number)

          val msgs = if (log_path.exists)
            log_path.readLines.toVector
          else
            Vector.empty

          path match {
            case None =>
              println(header)
              println(separator())
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
          printSegment(save_number + 1, path)
        }
      }

      val tokens = (input getOrElse "" split "\\s+").toList map (_.toLowerCase) dropWhile (_ == "")
      val (param, redirect_path) = if (tokens.isEmpty)
        (None, None)
      else if (tokens.head startsWith ">")
        (None, redirect(tokens))
      else
        (tokens.headOption, redirect(tokens.tail))

      // The messages for history since the last save point is in game.log
      // The messages for history for previous save points are in log-n files
      val NUM = """(\d+)""".r
      val start_num = param match {
        case None                     => game.history.size - 1
        case Some(NUM(n))             => (game.history.size - n.toInt) max 0
        case Some("all" | "al" | "a") => 0
        case Some(p)                  => throw Error(s"Invalid parameter: $p")
      }

      // Delete any previous file before we start appending to it.
      redirect_path foreach { p =>
        if (p.isDirectory)
          throw new IllegalArgumentException(s"Cannot redirect to a directory ($p)!")
        p.delete()
      }

      printSegment(start_num, redirect_path)
      
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
          case GameSegment(save_number, card, summary) => save_number -> (s"[$card]", summary)
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

      showPage(0)
    }
    catch {
      case AbortAction =>
    }
  }

  // Remove turn files starting with the given save file number and all 
  // those that follow that number.
  def removeSaveFiles(name: String, num: Int): Unit = {
    import Pathname.glob    
    val turnFiles = glob(gamesDir/name/"save-*")
    turnFiles filter (getSaveFileNumber(_) exists (_ >= num)) foreach (_.delete())
  }


  def adjustSettings(param: Option[String]): Unit = {
    val agitate = if (game.isBot(VC)) List("agitate") else Nil
    val options = (
      List("resources", "aid", "patronage", "econ", "trail", "uspolicy", "casualties", "out of play",
      "capabilities", "momentum", "rvnLeaders", "pivotal", "trung", "bot log") ::: agitate
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
      case "pivotal"      => adjustPivotalCards()
      case "trung"        => adjustTrungDeck()
      case "bot log"      => adjustBotDebug()
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
        }
    }
    else if (game.isHuman(VC)) {
      adjustInt("VC resources", game.vcResources, 0 to EdgeTrackMax) foreach { value =>
        val desc = adjustmentDesc("VC resources", game.vcResources, value)
        game = game.copy(vcResources = value)
        log(desc)
      }
    }
    else {
      adjustInt("ARVN resources", game.arvnResources, 0 to EdgeTrackMax) foreach { value =>
        val desc = adjustmentDesc("ARVN resources", game.arvnResources, value)
        game = game.copy(arvnResources = value)
        log(desc)
      }
    }
  }

  def adjustAgitate(): Unit = {
    adjustInt("VC Agitate total", game.vcResources, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("VC Agitate total (resources cylinder)", game.vcResources, value)
      game = game.copy(vcResources = value)
      log(desc)
    }
  }

  def adjustAid(): Unit = {
    adjustInt("US Aid", game.usAid, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("US Aid", game.usAid, value)
      game = game.copy(usAid = value)
      log(desc)
    }
  }

  def adjustPatronage(): Unit = {
    val origGame = game
    adjustInt("Patronage", game.patronage, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("Patronage", game.patronage, value)
      game = game.copy(patronage = value)
      log(desc)
      logPointsChanges(origGame, game)
    }


  }

  def adjustEcon(): Unit = {
    adjustInt("Econ marker", game.econ, 0 to EdgeTrackMax) foreach { value =>
      val desc = adjustmentDesc("Econ marker", game.econ, value)
      game = game.copy(econ = value)
      log(desc)
    }
  }

  def adjustTrail(): Unit = {
    adjustInt("Trail marker", game.trail, TrailMin to TrailMax) foreach { value =>
      val desc = adjustmentDesc("Trail Marker", game.trail, value)
      game = game.copy(trail = value)
      log(desc)
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
            log(s"Adjusted capabilities: [added ${cap}]")
          }
          nextAdjustment()
          
        case "add-shaded" =>
          val caps = excluded flatMap (name => AllShadedCaps find (_.name == name))
          askCapability(caps) foreach { cap =>
            game = game.copy(capabilities = cap :: game.capabilities)
            log(s"Adjusted capabilities: [added ${cap}]")
          }
          nextAdjustment()

        case RemoveCap(name) =>
          game.capabilities find (_.name == name) foreach { cap =>
            game = game.copy(capabilities = game.capabilities filterNot (_ == cap))
            log(s"Adjusted capabilities: [removed ${cap}]")
          }
          nextAdjustment()
          
        case _ =>
      }
    }

    val savedGame = game
    nextAdjustment()
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
  }


  def adjustBotDebug(): Unit = {
    val newValue = !game.botLogging
    val desc = adjustmentDesc("Bot log", game.botLogging, newValue)
    game = game.copy(botLogging = newValue)
    log(desc)
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
      val marker    = if (sp.isLoC) "Sabotage" else "Terror"
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
    val marker = if (sp.isLoC) "Sabotage" else "Terror"
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
      val forbidden = if (sp.isLoC) CoinBases:::InsurgentBases else Nil
      val pieces    = sp.pieces.except(forbidden)
      val available = game.availablePieces.except(forbidden)
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
          val origNum = pieces.totalOf(pieceType)
          val maxNum  = {
            val n = available.totalOf(normalizedType(pieceType)) + pieces.totalOf(pieceType)
            if (isBase(pieceType)) {
              val maxBase = 2 + pieces.totalOf(pieceType) - pieces.totalOf(BasePieces)
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