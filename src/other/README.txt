

README file for the Fire in the Lake console application.
-----------------------------------------------------------------------
The fitl program is an implementation of the Trung solo AI for
the board game Fire in the Lake (2nd edition)
designed by Mark Herman and Volko Ruhnke and published by GMT Games.

The program is implemented in the Scala programming language
and therefore requires the Java JVM.  It is a text based 
console application and should run on Mac OSX, Windows and
Linux.

To run the program
-----------------------------------------------------------------------
1. Ensure that Java is installed on your system and that the java
   executable can be found on your PATH.
   
2. Run the fitl script located in the same directory
   as this README file. (On Windows, use the fitl.cmd script)

3. If all is well, the game will prompt you for a scenario and you
   will be off and running.
   
   
More Details
-----------------------------------------------------------------------
1. You will be presented with a Command: prompt.  You can type
   'help' to get a list of valid commands.  You can type 
   'help <command>' to get more detailed help on a particular command.
   
2. Any time the game prompts you for a value (a command, a space name, etc.),  
   you only need to enter the first few characters of the value.  For example,
   if you want to enter "Saigon" as the target of an event, you can simply
   type 'Sai'

3. The names of LoCs are quite long and inconvenient to type.
There for the name of each loc starts with "LOC".  When you need
to specify the name of an LoC you can simply type "loc" and you will
be presented with a menu of the LoCs allowing you to choose the 
appropriate one.
   
3. Your game will automatically be saved after each factions turn.
   When you resume a game in progress, the game will load the most
   recent save file.
   The games are saved in a 'games' subdirectory that is created
   Each game will have its own subdirectory within 'games'.
   To delete a saved game, simply delete its corresponding subdirectory.
      
4. You can use the 'rollback' command to roll back to the beginning
   of any previous turn.
   
5. Look at the help for the 'show' and 'history' commands.  They are
   very useful for making sure your board is in sync with the game.
   If you find any inconsistencies, you can use the 'adjust' command.
   

Copyright (c) 2021 Curt Sellmer
-----------------------------------------------------------------------
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
