#!/bin/bash

FREEPASCAL=~/FreePascal
LAZARUS=/lazarus
LAZBUILD=/lazbuild

echo Clearing the binaries
rm UberPDFExample UberPDFExample-lintel64

echo Compiling with Build Option lintel64
$FREEPASCAL$LAZARUS$LAZBUILD --bm=lintel64 UberPDFExample.lpi

echo Renaming the binary accordingly
cp UberPDFExample UberPDFExample-lintel64
