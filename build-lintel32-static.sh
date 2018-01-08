#!/bin/bash

## PLEASE change these values to reflect your system
## I'm using a fpcupdeluxe instalation with these values
FREEPASCAL=~/FreePascal
LAZARUS=/lazarus
LAZBUILD=/lazbuild
## END edit

echo BUILD: Clearing the binaries
echo
rm -f UberPDFExample UberPDFExample-lintel32-static

echo BUILD: Compiling with Build Option lintel32-static
echo
$FREEPASCAL$LAZARUS$LAZBUILD --bm=lintel32-static UberPDFExample.lpi

if [ $? = 0 ]; then
  echo BUILD: Renaming the binary accordingly
  echo
  mv UberPDFExample UberPDFExample-lintel32-static
else
  echo BUILD: Cannot find binary to move
fi
