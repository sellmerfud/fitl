
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

import scala.collection.immutable.ListMap
import scala.util.Random.shuffle
import FireInTheLake._

object Cards {
  val SingleEvent = false
  val DualEvent   = true

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)

  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Gulf of Tonkin", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(2, "Kissinger", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(3, "Peace Talks", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(4, "Top Gun", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(5, "Wild Weasels", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(6, "Aces", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(7, "ADSID", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(8, "Arc Light", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(9, "Psychedelic Cookie", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(10, "Rolling Thunder", 
      ListMap(US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(11, "Abrams", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(12, "Capt Buck Adams", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(13, "Cobras", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(14, "M-48 Patton", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(15, "Medevac", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(16, "Blowtorch Komer", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(17, "Claymores", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(18, "Combined Action Platoons", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(19, "CORDS", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(20, "Laser Guided Bombs", 
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(21, "Americal", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(22, "Da Nang", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(23, "Operation Attleboro", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(24, "Operation Starlite", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(25, "TF-116 Riverines", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(26, "LRRP", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(27, "Phoenix Program", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(28, "Search and Destroy", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(29, "Tribesmen", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(30, "USS New Jersey", 
      ListMap(US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(31, "AAA", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(32, "Long Range Guns", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(33, "MiGs", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(34, "SA-2s", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(35, "Thanh Hoa", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(36, "Hamburger Hill", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(37, "Khe Sanh", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(38, "McNamara Line", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(39, "Oriskany", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(40, "PoWs", 
      ListMap(NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(41, "Bombing Pause", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(42, "Chou En Lai", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new Card(43, "Economic Aid", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(44, "la Drang", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new Card(45, "PT-76", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(46, "559th Transport Grp", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(47, "Chu Luc", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(48, "Nam Dong", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(49, "Russian Arms", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(50, "Uncle Ho", 
      ListMap(NVA -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(51, "301st Supply Bn", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(52, "RAND", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(53, "Sappers", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(54, "Son Tay", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(55, "Trucks", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(56, "Vo Nguyen Giap", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(57, "International Unrest", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(58, "Pathet Lao", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(59, "Plei Mei", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(60, "War Photographer", 
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(61, "Armored Cavalry", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(62, "Cambodian Civil War", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(63, "Fact Finding", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(64, "Honolulu Conference", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(65, "International Forces", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(66, "Ambassador Taylor", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(67, "Amphib Landing", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new Card(68, "Green Berets", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(69, "MACV", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(70, "ROKs", 
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(71, "An Loc", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(72, "Body Count", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(73, "Great Society", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(74, "Lam Son 719", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(75, "Sihanouk", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, VC -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(76, "Annam", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(77, "Detente", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(78, "General Lansdale", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(79, "Henry Cabot Lodge", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(80, "Light at the End of the Tunnel", 
      ListMap(ARVN -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(81, "CIDG", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(82, "Domino Theory", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(83, "Election", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(84, "To Quoc", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(85, "USAID", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(86, "Mandate of Heaven", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(87, "Nguyen Chanh Thi", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(88, "Phan Quang Dan", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(89, "Tam Chau", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(90, "WAlt Rostow", 
      ListMap(ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(91, "Bob Hope", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(92, "SEALORDS", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(93, "Senator Fulbright", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(94, "Tunnel Rats", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(95, "Westmoreland", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(96, "APC", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(97, "Brinks Hotel", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(98, "Long Tan", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(99, "Masher/White Wing", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(100, "Rach Ba Rai", 
      ListMap(VC -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(101, "Booby Traps", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(102, "Cu Chi", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(103, "Kent State", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(104, "Main Force Bns", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(105, "Rural Pressure", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(106, "Binh Duong", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(107, "Burning Bonze", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(108, "Draft Dodgers", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(109, "Nguyen Huu Tho", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(110, "No Contact", 
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(111, "Agent Orange", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(112, "Colonel Chau", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(113, "Ruff Puff", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(114, "Tri Quang", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(115, "Typhoon Kate", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(116, "Cadres", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(117, "Corps Commanders", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(118, "Korean War Arms", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(119, "My Lai", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(120, "US Press Corps", 
      ListMap(VC -> PlaceHolder, ARVN-> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder),
      DualEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    

    // ------------------------------------------------------------------------
    // Pivotal Events
    // ------------------------------------------------------------------------
    entry(new Card(121, "Linebacker II", // US Pivotal event
      ListMap(US -> PlaceHolder, ARVN -> PlaceHolder, VC -> PlaceHolder, NVA -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(122, "Easter Offensive", // NVA Pivotal event
      ListMap(NVA -> PlaceHolder, VC -> PlaceHolder, ARVN -> PlaceHolder, US -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(123, "Vietnamization", // ARVN Pivotal event
      ListMap(ARVN -> PlaceHolder, US -> PlaceHolder, NVA -> PlaceHolder, VC -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(124, "Tet Offensive", // VC Pivotal event
      ListMap(VC -> PlaceHolder, NVA -> PlaceHolder, US -> PlaceHolder, ARVN -> PlaceHolder),
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    // Coup Cards
    // ------------------------------------------------------------------------
    entry(new Card(125, "Coup! Nguyen Khanh",
      ListMap.empty,
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(126, "Coup! Young Turks",
      ListMap.empty,
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(127, "Coup! Nguyen Cao Ky",
      ListMap.empty,
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(128, "Coup! Nguyen Van Thieu",
      ListMap.empty,
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(129, "Coup! Failed Attempt",
      ListMap.empty,
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => (),  // ARVN removed 1 in 3 of its cubes per space
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(130, "Coup! Failed Attempt",
      ListMap.empty,
      SingleEvent,
      (faction: Faction) => NoEvent,
      (faction: Faction) => deck(129).executeUnshaded(faction),
      (faction: Faction) => NoEvent,
      (faction: Faction) => ()
    ))
  )
}