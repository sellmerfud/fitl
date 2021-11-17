@ECHO off

SetLocal EnableDelayedExpansion

rem This script is used to run the scala implementation of the
rem solo Tru'ng bots for Fire in the Lake

rem Set the current working directory to the directory where this script is running.
rem This is important so that all of our relative path references work correctly.
pushd %~dp0

set CLASSPATH=lib\fire-in-the-lake_2.12-0.1.jar;lib\optparse_2.12-2.2.jar;lib\scala-library-2.12.15.jar;^
lib\scala-parser-combinators_2.12-1.1.2.jar;lib\scala-reflect-2.12.15.jar;lib\scalactic_2.12-3.0.1.jar
java -cp %CLASSPATH%  fitl.FireInTheLake %*

popd
EndLocal
