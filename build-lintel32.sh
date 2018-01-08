#!/bin/bash

FREEPASCAL=~/FreePascal
LAZARUS=/lazarus
LAZBUILD=/lazbuild

echo Clearing the binaries
rm UberPDFExample UberPDFExample-lintel32

echo Compiling with Build Option lintel32
$FREEPASCAL$LAZARUS$LAZBUILD --bm=lintel32 UberPDFExample.lpi

echo Renaming the binary accordingly
cp UberPDFExample UberPDFExample-lintel32
