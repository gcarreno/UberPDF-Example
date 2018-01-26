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
UBER_PDF_LIB=$UBER_PDF_LIB_DIR/libuberpdfsdk.a

UBER_3RD=$UBER_SDK/3rdpartylibs

UBER_3RD_QPDF_DIR=$UBER_3RD/qpdf/lib/$UBER_PLATFORM/gcc/librtl
UBER_3RD_QPDF=$UBER_3RD_QPDF_DIR/libqpdf.a

UBER_3RD_JPEG_DIR=$UBER_3RD/libjpeg/lib/$UBER_PLATFORM/gcc/librtl
UBER_3RD_JPEG=$UBER_3RD_JPEG_DIR/libjpeg.a

UBER_3RD_PNG_DIR=$UBER_3RD/libpng/lib/$UBER_PLATFORM/gcc/librtl
UBER_3RD_PNG=$UBER_3RD_PNG_DIR/libpng.a

UBER_3RD_ZIP_DIR=$UBER_3RD/zlib/lib/$UBER_PLATFORM/gcc/librtl
UBER_3RD_ZIP=$UBER_3RD_ZIP_DIR/libz.a
##

## Build variables
EXAMPLE_BIN=UberPDFExample
EXAMPLE_NEW_BIN=$EXAMPLE_BIN-$UBER_PLATFORM-static
EXAMPLE_BUILD=$UBER_PLATFORM-static
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

    echo BUILD: Checking for the UberPDF .a library
    if [ -f $UBER_PDF_LIB ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find UberPDF .a library
        echo SEARCH: $UBER_PDF_LIB
        echo
        exit 1
    fi

    echo BUILD: Checking for QPDF
    if [ -f $UBER_3RD_QPDF ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find QPDF .a library
        echo SEARCH: $UBER_3RD_QPDF
        echo
        exit 1
    fi

    echo BUILD: Checking for libJPEG
    if [ -f $UBER_3RD_JPEG ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find libJPEG .a library
        echo SEARCH: $UBER_3RD_JPEG
        echo
        exit 1
    fi

    echo BUILD: Checking for libPNG
    if [ -f $UBER_3RD_PNG ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find libPNG .a library
        echo SEARCH: $UBER_3RD_PNG
        echo
        exit 1
    fi

    echo BUILD: Checking for libZ
    if [ -f $UBER_3RD_ZIP ]; then
        DEPS_OK=1
    else
        echo BUILD ERROR: Cannot find libZ .a library
        echo SEARCH: $UBER_3RD_ZIP
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
