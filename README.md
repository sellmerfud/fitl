## Overview
This is a console application that implements the Tru'ng AI Bots for the board game
*Fire In The Lake*, designed by Mark Herman and Volko Ruhnke and published by GMT Games.
The Tru'ng Bots were designed by Bruce Mansfield.

## Downloading the executable package

You can use this link to download the latest release: [Version 1.35][1]

This will download a zip file called `fitl-x.x.zip` (where *x.x* is the version number).
Extracting the zip file will create a folder called `fitl-x.x`.

If you are upgrading to a newer version you can continue any games that you started with
the previous version.  To do so, simply copy the *games* folder below the older *fitl-x.x* directory to the 
new *fitl-x.x* directory.

[1]: https://github.com/sellmerfud/fitl/releases/download/v1.35/fitl-1.35.zip

## Running the program

This is a Scala program, so you will need to have the Java JVM installed and make sure that
the `java` command is on your `PATH`.

There are two scripts provided that will start the program.

<dl>
  <dt><pre>fitl</pre></dt>
  <dd>A bash script used on Mac and Linux</dd>

  <dt><pre>fitl.cmd</pre></dt>
  <dd>A Windows command file</dd>
</dl>

## Using the program

When you first run the program, it will prompt you to enter:

1. The scenario that you wish to play
2. Whether or not you will be using period events.
3. Which faction or factions will be played by human players.
4. A name for your game so that your progress can be saved.

Your game is saved after each turn or Coup! round is completed.

The next time you run the program, it will ask if you want to resume a saved game and will list all
of your saved games.  You can pick one or start a new game.


## Entering commands

The program does not maintain the deck of event cards.  You must setup and shuffle the deck and then
tell the program which card has been drawn.  You will be prompted to enter a card number each time
a new event card must be drawn.

The Trung deck is maintained by the program.  You will see messages indicating when a Trung card
is drawn but you do not need to use the physical Trung cards.

Use the `help` command to see all of the the available commands.  You can get further help for 
a specific command by typing its name after `help`. For example for help on the `show` 
command type `help show`.

The `show` command allows you to inspect the current state of the board, game status, etc.

The `history` command allow you to review the log for the previous game turns.  Each time the
game is saved, a new entry is added to the history.  With no parameters the `history` command
will show the log for the most recent save point.

The `rollback` command will let you restart the game from the beginning of any turn.

When it is a Bot faction's turn to play you will use the `bot` command.  The results of the Bot turn
are displayed so that you can follow along and update your physical game board.

When it is a human faction's turn to play you will use the `act` command to take your turn.  The program
will then ask you how you wish to proceed.

During your turn you can enter `abort` at most prompts if you wish to cancel what you have done
and start your turn over. This will take you back to the start of your turn.  And any changes 
needed to the physical game board will be displayed so you can keep your physical board in sync
with the software. 

If you enter `abort` while you are conducting a *Special Activity* only the
special activity is aborted.  This allows you to change your mind and select another special activity,
or to not perform a special activity at the current moment.  Once you have backed out of the
special activity, you can enter `abort` again to abort the entire turn.

## Command Abbreviations

All commands can be shortened to the prefix of the command name that is unique.  For example `hi` 
is shorthand for `history`.

In fact this use of abbreviated prefixes works at every prompt in the game.  For example, if you are
executing an operation and the software prompts you for the space in which to perform that operation
you could enter `sai` to indicate `Saigon`. (You do not need to distinguish between uppercase and lowercase
letters)

If you were to enter an ambiguous prefix such as `quang`, then the software will display a menu to
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

For example if you are playing the VC and are prompted to select a space to Terrorize, you could enter `?` and you
will see a list of all valid spaces that can be terrorized by the VC faction.  Then you will be prompted
again to enter a space.

    Terrorize which space: ?
    Enter one of:
    Binh Dinh, Binh Tuy-Binh Thuan, Kien Giang-An Xuyen, Kien Phong, Pleiku-Darlac, Quang Duc-Long Khanh, Tay Ninh, or abort

    Terrorize which space: 


### Selecting LOC's

The names of LOCs are quite long as they include all endpoints of the LOC.
In the software the name of all LOCS begin with the prefix `LOC`
> For example: `LOC Ban Me Thuot -- Da Lat`

Remembering and typing the entire unique prefix for an LOC would not be very
easy since there are many LOCs coming out of the same endpont.
So when selecting an LOC you can simply enter "LOC" as the prefix and the
software will provide a menu of all LOCs that can be selected for the current game function.
You can then simply enter the number next to the LOC that you wish to select.

    Terrorize which space: loc
    
    'loc' is ambiguous.  Choose one:
    ====================================================
    1) LOC Hue -- Da Nang
    2) LOC Saigon -- An Loc -- Ban Me Thuot
    3) None of the above
    ----------------------------------------------------
    Selection:
    
In the above example, only the LOCs with underground guerrillas are included in the list.
    
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
