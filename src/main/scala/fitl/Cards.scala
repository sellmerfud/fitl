
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

  // Temporary Functions
  private def unshadedNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Unshaded event not yet implemented")
  }
  
  private def shadedNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Shaded event not yet implemented")
  }
  private def singleNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Event not yet implemented")
  }
  private def coupNotYet(): Unit = {
    log(s"\n${deck(game.currentCard)}: Coup event not yet implemented")
  }

  // Convenience method for adding a card to the deck.
  private def entry(card: EventCard) = (card.number -> card)

  val deckMap: Map[Int, EventCard] = Map(
    // ------------------------------------------------------------------------
    entry(new EventCard(1, "Gulf of Tonkin", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(2, "Kissinger", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(3, "Peace Talks", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(4, "Top Gun", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(5, "Wild Weasels", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(6, "Aces", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(7, "ADSID", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(8, "Arc Light", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(9, "Psychedelic Cookie", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(10, "Rolling Thunder", DualEvent,
      List(US, NVA, VC, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(11, "Abrams", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(12, "Capt Buck Adams", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(13, "Cobras", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical -> Unshaded),
              ARVN -> (Critical -> Unshaded),
              NVA  -> (Critical -> Shaded),
              VC   -> (Critical -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(14, "M-48 Patton", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(15, "Medevac", DualEvent,
      List(US, ARVN, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(16, "Blowtorch Komer", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(17, "Claymores", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(18, "Combined Action Platoons", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(19, "CORDS", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(20, "Laser Guided Bombs", DualEvent,
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(21, "Americal", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(22, "Da Nang", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(23, "Operation Attleboro", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(24, "Operation Starlite", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(25, "TF-116 Riverines", DualEvent,
      List(US, VC, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(26, "LRRP", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(27, "Phoenix Program", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(28, "Search and Destroy", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(29, "Tribesmen", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(30, "USS New Jersey", DualEvent,
      List(US, VC, ARVN, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(31, "AAA", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(32, "Long Range Guns", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(33, "MiGs", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(34, "SA-2s", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(35, "Thanh Hoa", DualEvent,
      List(NVA, US, ARVN, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(36, "Hamburger Hill", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(37, "Khe Sanh", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(38, "McNamara Line", SingleEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(39, "Oriskany", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(40, "PoWs", DualEvent,
      List(NVA, US, VC, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(41, "Bombing Pause", SingleEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(42, "Chou En Lai", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new EventCard(43, "Economic Aid", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(44, "la Drang", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new EventCard(45, "PT-76", DualEvent,
      List(NVA, ARVN, US, VC),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(46, "559th Transport Grp", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(47, "Chu Luc", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(48, "Nam Dong", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(49, "Russian Arms", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(50, "Uncle Ho", DualEvent,
      List(NVA, ARVN, VC, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(51, "301st Supply Bn", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(52, "RAND", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Critical -> Unshaded),
              ARVN -> (Critical -> Unshaded),
              NVA  -> (Critical -> Shaded),
              VC   -> (Critical -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(53, "Sappers", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(54, "Son Tay", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(55, "Trucks", DualEvent,
      List(NVA, VC, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(56, "Vo Nguyen Giap", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(57, "International Unrest", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(58, "Pathet Lao", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(59, "Plei Mei", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(60, "War Photographer", DualEvent,
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(61, "Armored Cavalry", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(62, "Cambodian Civil War", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(63, "Fact Finding", DualEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(64, "Honolulu Conference", SingleEvent,
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (Performed   -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(65, "International Forces", DualEvent, 
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(66, "Ambassador Taylor", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(67, "Amphib Landing", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    // See node in the Errata!
    entry(new EventCard(68, "Green Berets", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(69, "MACV", SingleEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (Performed   -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(70, "ROKs", DualEvent,
      List(ARVN, US, VC, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(71, "An Loc", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(72, "Body Count", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    // Mo_Medevac_Unshaded    - In Commitment Phase (immediately move all US TROOPS in CASUALTIES to AVAILABLE,
    //                          no TROOPS go out of play.  See note: For effect when #73 Great Society is played.
    entry(new EventCard(73, "Great Society", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(74, "Lam Son 719", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(75, "Sihanouk", DualEvent,
      List(ARVN, NVA, US, VC),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(76, "Annam", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(77, "Detente", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(78, "General Lansdale", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(79, "Henry Cabot Lodge", DualEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Shaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(80, "Light at the End of the Tunnel", SingleEvent,
      List(ARVN, NVA, VC, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (Critical    -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(81, "CIDG", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(82, "Domino Theory", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(83, "Election", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(84, "To Quoc", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(85, "USAID", DualEvent,
      List(ARVN, VC, US, NVA),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(86, "Mandate of Heaven", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(87, "Nguyen Chanh Thi", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Shaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(88, "Phan Quang Dan", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Performed -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(89, "Tam Chau", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(90, "WAlt Rostow", DualEvent,
      List(ARVN, VC, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (Performed   -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(91, "Bob Hope", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(92, "SEALORDS", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(93, "Senator Fulbright", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(94, "Tunnel Rats", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(95, "Westmoreland", DualEvent,
      List(VC, US, NVA, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(96, "APC", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(97, "Brinks Hotel", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Performed -> Unshaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(98, "Long Tan", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(99, "Masher/White Wing", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(100, "Rach Ba Rai", DualEvent,
      List(VC, US, ARVN, NVA),
      ListMap(US   -> (Performed -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(101, "Booby Traps", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Performed -> Unshaded),
              NVA  -> (Critical  -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(102, "Cu Chi", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(103, "Kent State", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(104, "Main Force Bns", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Critical  -> Unshaded),
              ARVN -> (Critical  -> Unshaded),
              NVA  -> (Performed -> Shaded),
              VC   -> (Critical  -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(105, "Rural Pressure", DualEvent,
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Critical    -> Shaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(106, "Binh Duong", SingleEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (Critical    -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(107, "Burning Bonze", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(108, "Draft Dodgers", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(109, "Nguyen Huu Tho", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(110, "No Contact", DualEvent,
      List(VC, NVA, ARVN, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(111, "Agent Orange", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(112, "Colonel Chau", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(113, "Ruff Puff", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(114, "Tri Quang", DualEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(115, "Typhoon Kate", SingleEvent,
      List(VC, ARVN, US, NVA),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(116, "Cadres", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(117, "Corps Commanders", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(118, "Korean War Arms", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (Performed   -> Unshaded),
              ARVN -> (Performed   -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(119, "My Lai", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Shaded),
              VC   -> (Critical    -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(120, "US Press Corps", DualEvent,
      List(VC, ARVN, NVA, US),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Performed   -> Shaded),
              VC   -> (NotExecuted -> Shaded)),
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => unshadedNotYet(),
      (faction: Faction) => shadedNotYet()
    )),
    

    // ------------------------------------------------------------------------
    // Pivotal Events
    // ------------------------------------------------------------------------
    entry(new EventCard(121, "Linebacker II", SingleEvent, // US Pivotal event
      List(US, ARVN, VC, NVA),
      ListMap(US   -> (Critical    -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      (faction: Faction) => game.numCardsInLeaderBox >= 2 &&
                            game.usPoints > 40,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // With the Pivotal events only the Unshaded functions are used.
    // The unshadedEffective() function is used to determine if the
    // faction can play the Pivotal event.
    // ------------------------------------------------------------------------
    entry(new EventCard(122, "Easter Offensive", SingleEvent, // NVA Pivotal event
      List(NVA, VC, ARVN, US),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (Critical    -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      (faction: Faction) => game.numCardsInLeaderBox >= 2 &&
                            game.totalOnMap(_.pieces.totalOf(NVATroops)) > game.totalOnMap(_.pieces.totalOf(USTroops)),
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(123, "Vietnamization", SingleEvent, // ARVN Pivotal event
      List(ARVN, US, NVA, VC),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (Critical    -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (NotExecuted -> Unshaded)),
      (faction: Faction) => game.numCardsInLeaderBox >= 2 &&
                            game.totalOnMap(_.pieces.totalOf(USTroops)) < 20,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(124, "Tet Offensive", SingleEvent, // VC Pivotal event
      List(VC, NVA, US, ARVN),
      ListMap(US   -> (NotExecuted -> Unshaded),
              ARVN -> (NotExecuted -> Unshaded),
              NVA  -> (NotExecuted -> Unshaded),
              VC   -> (Critical    -> Unshaded)),
      (faction: Faction) => game.numCardsInLeaderBox >= 2 &&
                            numVCGuerrillasInSouth > 20,
      (faction: Faction) => false,
      (faction: Faction) => singleNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    // Coup Cards
    // ------------------------------------------------------------------------
    entry(new EventCard(125, "Coup! Nguyen Khanh", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => coupNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(126, "Coup! Young Turks", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => coupNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(127, "Coup! Nguyen Cao Ky", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => coupNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(128, "Coup! Nguyen Van Thieu", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => coupNotYet(),
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(129, "Coup! Failed Attempt",SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => coupNotYet(),  // ARVN removed 1 in 3 of its cubes per space
      (faction: Faction) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new EventCard(130, "Coup! Failed Attempt", SingleEvent,
      List.empty,
      ListMap.empty,
      (faction: Faction) => false,
      (faction: Faction) => false,
      (faction: Faction) => deck(129).executeUnshaded(faction),
      (faction: Faction) => ()
    ))
  )
}