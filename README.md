## Overview
This is a console application that implements the Tru'ng AI Bots for the board game
*Fire In The Lake*, designed by Mark Herman and Volko Ruhnke and published by GMT Games.
The Tru'ng Bots were designed by Bruce Mansfield.

You can play as any faction and the bots will play the other three factions.

## Downloading the executable package

You can use this link to download the latest release: [Version 0.29][1]

If you are upgrading to a newer version and the **major** version numbers of the two versions
are the same, then any saved games in progress will be compatible.

Simply copy the *games* folder from the older *fitl-x.x* directory to the 
new *fitl-x.x* directory.

[1]: https://www.dropbox.com/s/vuath4w6a3nutwf/fitl-0.29.zip?dl=0

## Running the program

This is a Scala program, so you will need to have the Java JVM installed and make sure that
the `java` command is on your `PATH`

There are two scripts provided that will start the program.

* `filt` -- As bash script used on Mac and Linux
* `filt.cmd` -- A Windows command file

## Using the program

When you first run the program, it will prompt you to enter:

1. The scenario that you wish to play
2. Whether or not you will be using period events.
3. Which faction or factions will be played by human players.
4. A name for your game so that your progress can be saved.

Your game is saved after each turn or Coup! round is completed.

The next time you run the program, it will ask if you want to resume a saved game.


## Entering commands

The program does not maintain the deck of event cards.  You must setup and shuffle the deck and then
tell the program which card has been drawn.  You will be prompted to enter a card number each time
a new event card must be drawn.

The Trung deck is maintained by the program.  You will see messages indicating when ad Trung card
is drawn but you do not need to use the physical Trung cards.

Use the `help` command to see all of the the available commands.  You can get further help for 
a specific command by typing its name after `help`. For example for help on the `show` 
command type `help show`.

The `show` command allows you to inspect the current state of the board, game status, etc.

The `history` command allow you to review the log for the previous game turns.  Each time the
game is saved, a new entry is added to the history.  With no parameters the `history` command
will show the log for the most recent save point.

The `rollback` command will let you restart the game from the beginning of any turn.

You can also abort the current action that you are playing at most prompts by entering `abort`.
This will take you back to the start of your turn.  And changes needed to the physical game
board are displayed so you can keep your board in sync with the software.

## Command Abbreviations

All commands can be shorted to the prefix of the command name that is unique.  The `h` 
command is actually shorthand for `history`.

In fact this use of abbreviated prefixes works at every prompt in the game.  For example, if you are
executing an operation and the software prompts you for the space in which to perform that operation
you could enter `sai` to indicate `Saigon`.

If you were to enter an ambiguous prefix such as `quang`, the the software will display a menu to
allow you to clarify which space you wish to select.

    'quang' is ambiguous.  Choose one:
    ====================================================
    1) Quang Duc-Long Khanh
    2) Quang Nam
    3) Quang Tin-Quang Ngai
    4) Quang Tri-Thua Thien
    5) None of the above
    ----------------------------------------------------
    Selection:

Also, when at a prompt, you can see all of choices that are valid for the current operation by entering a question mark `?`.

### Selecting LOC's

The names of LOCs are quite long as they include all endpoints of the LOC.
In the software the name of all LOCS begin with the prefix `LOC`
> For example: `LOC Ban Me Thuot -- Da Lat`

Remembering and typing the entire unique prefix for an LOC would not be very
easy since there are many LOCs coming out of the same endpont.
So when selecting an LOC you can simply enter "LOC" as the prefix and the
software will provide a menu of all LOCs that can be selected for the current game function.
You can then simply enter the number next to the LOC that you wish to select.


## License

    Copyright (c) 2023 Curt Sellmer
    
    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:
    
    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
