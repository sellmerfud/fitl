@ECHO off

SetLocal EnableDelayedExpansion

rem  _____ _            _         _   _            _          _
rem |  ___(_)_ __ ___  (_)_ __   | |_| |__   ___  | |    __ _| | _____
rem | |_  | | '__/ _ \ | | '_ \  | __| '_ \ / _ \ | |   / _` | |/ / _ \
rem |  _| | | | |  __/ | | | | | | |_| | | |  __/ | |__| (_| |   <  __/
rem |_|   |_|_|  \___| |_|_| |_|  \__|_| |_|\___| |_____\__,_|_|\_\___|
rem
rem
rem This script is used to run the scala implementation of the
rem solo Tru'ng bots for Fire in the Lake

rem Set the current working directory to the directory where this script is running.
rem This is important so that we can find the jar files in the lib subdirectory.


pushd %~dp0
java -cp lib\loader.jar loader.Loader %*
popd
EndLocal
