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
UBER_PLATFORM=lintel64
UBER_PDF_LIB_DIR=$UBER_SDK/uberbaselibs/uberpdfsdk/lib/$UBER_PLATFORM/gcc/librtl
UBER_PDF_LIB=$UBER_PDF_LIB_DIR/libuberpdfsdkdyn.so
##

## Build variables
EXAMPLE_BIN=UberPDFExample
EXAMPLE_NEW_BIN=$EXAMPLE_BIN-$UBER_PLATFORM
EXAMPLE_BUILD=$UBER_PLATFORM
EXAMPLE_FILE=$EXAMPLE_BIN.lpi
##

## Dependencies
DEPS_OK=0

function check_dependencies() {
    echo BUILD: Dependencies ...
    echo BUILD: Checking lazbuild binary
    if [ -x $LAZBUILD_BIN ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find lazbuild binary
        echo SEARCH: $LAZBUILD_BIN
        echo
        exit 1
    fi
    echo BUILD: Checking for the UberPDF .so library
    if [ -f $UBER_PDF_LIB ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find UberPDF .so library
        echo SEARCH: $UBER_PDF_LIB
        echo Without this library the binary will not run with success
        echo
        exit 1
    fi
    echo BUILD: Done
    echo
}

function clean() {
    echo "BUILD: Clearing the binaries ( $EXAMPLE_BIN; $EXAMPLE_NEW_BIN )"
    echo
    rm -f ./$EXAMPLE_BIN ./$EXAMPLE_NEW_BIN
}

function build() {
    echo BUILD: Compiling with Build Option $EXAMPLE_BUILD
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
echo UberPDF Example build script for $UBER_PLATFORM
echo
check_dependencies

if [ "x$DEPS_OK" = "x1" ]; then
    clean
    build
fi
##
