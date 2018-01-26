#!/bin/bash

## PLEASE change these values to reflect your system
## I'm using a fpcupdeluxe instalation with these values
FREEPASCAL_DIR=$HOME/FreePascal
LAZARUS_DIR=$FREEPASCAL_DIR/lazarus
LAZBUILD_BIN=$LAZARUS_DIR/lazbuild
## END edit

## UberBuild
## This script assumes that the UberBuild is in the following folders
## under the current path
UBER_SDK=./uber
UBER_PDF_LIB_DIR=$UBER_SDK/uberbaselibs/uberpdfsdk/lib/lintel64/gcc/librtl
UBER_PDF_LIB=$UBER_PDF_LIB_DIR/libuberpdfsdkdyn.so
##

## Build variables
EXAMPLE_BIN=UberPDFExample
EXAMPLE_NEW_BIN=UberPDFExample-lintel64
EXAMPLE_BUILD=lintel64
EXAMPLE_FILE=UberPDFExample.lpi
##

DEPS_OK=0

function clean() {
  echo "BUILD: Clearing the binaries ( $EXAMPLE_BIN; $EXAMPLE_NEW_BIN )"
  echo
  rm -f ./$EXAMPLE_BIN ./$EXAMPLE_NEW_BIN
}

function check_dependencies() {
  if [ -x $LAZBUILD_BIN ]; then
    DEPS_OK=1
  else
    echo BUILD ERROR: Cannot find lazbuild binary
    echo SERACH: $LAZBUILD_BIN
    echo
    exit 1
  fi
  if [ -f $UBER_PDF_LIB ]; then
    DEPS_OK=1
  else
    echo BUILD ERROR: Cannot find UberPDF .so library
    echo SERACH: $UBER_PDF_LIB
    echo
    exit 1
  fi
}

function build() {
  echo BUILD: Compiling with Build Option lintel64
  echo
  $LAZBUILD_BIN --bm=$EXAMPLE_BUILD $EXAMPLE_FILE

  if [ $? = 0 ]; then
    echo
    rename
  else
    echo
    echo BUILD ERROR: Compiling failed, will not move binary
  fi
}

function rename() {
  echo BUILD: Renaming $EXAMPLE_BIN to $EXAMPLE_NEW_BIN
  echo
  mv ./$EXAMPLE_BIN ./$EXAMPLE_NEW_BIN
}

## MAIN
##
check_dependencies

if [ "x$DEPS_OK" = "x1" ]; then
  clean
  build
fi
##
