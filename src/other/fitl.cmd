@ECHO off

SetLocal EnableDelayedExpansion

rem This script is used to run the scala implementation of the
rem solo Tru'ng bots for Fire in the Lake

rem Set the current working directory to the directory where this script is running.
rem This is important so that all of our relative path references work correctly.
pushd %~dp0

set CLASSPATH=lib\fire-in-the-lake_2.13-0.13.jar;^
lib\scala-library-2.13.10.jar;^
lib\scala-parser-combinators_2.13-2.1.1.jar;

java -cp %CLASSPATH%  fitl.FireInTheLake %*

popd
EndLocal
