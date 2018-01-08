#!/bin/bash

## PLEASE change these values to reflect your system
## I'm using a fpcupdeluxe instalation with these values
FREEPASCAL=~/FreePascal
LAZARUS=/lazarus
LAZBUILD=/lazbuild
## END edit

echo BUILD: Clearing the binaries
echo
rm -f UberPDFExample UberPDFExample-lintel64-static

echo BUILD: Compiling with Build Option lintel64-static
echo
$FREEPASCAL$LAZARUS$LAZBUILD --bm=lintel64-static UberPDFExample.lpi

if [ $? = 0 ]; then
  echo BUILD: Renaming the binary accordingly
  echo
  cp UberPDFExample UberPDFExample-lintel64-static
else
  echo BUILD: Cannot find binary to copy
fi
