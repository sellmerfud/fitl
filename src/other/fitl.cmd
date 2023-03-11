@ECHO off

SetLocal EnableDelayedExpansion

rem This script is used to run the scala implementation of the
rem solo Tru'ng bots for Fire in the Lake

rem Set the current working directory to the directory where this script is running.
rem This is important so that we can find the jar files in the lib subdirectory.


pushd %~dp0
java -cp lib/loader.jar loader.Loader %*
popd
EndLocal
