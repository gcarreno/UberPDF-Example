#!/bin/bash

###
# Free Pascal and Lazarus variables
#
# PLEASE change these values to reflect your system
# I'm using a fpcupdeluxe instalation with these values
#
FREEPASCAL_DIR="$HOME/FreePascal"
LAZARUS_DIR="$FREEPASCAL_DIR/lazarus"
LAZBUILD_BIN="$LAZARUS_DIR/lazbuild"
# END edit

###
# Script variables
#
BUILD_VERSION="0.3"
BUILD_BIN="UberPDFExample"
BUILD_LPI="$BUILD_BIN.lpi"
BUILD_MODE=""
BUILD_OK=0

###
# Uber related variables
#
UBER_OS="$(uname -s)"
UBER_ARCH="$(uname -m)"
UBER_PLATFORM=""
UBER_DEPS_OK=0
UBER_SDK_DIR="./uber"
UBER_PDF_LIB_DIR=""
UBER_PDF_LIB=""
###


function check_platform() {
    case "$UBER_OS" in
        "Linux")
            case "$UBER_ARCH" in
                "i[3456]86")
                    UBER_PLATFORM='lintel32'
                    ;;
                "x86_64")
                    UBER_PLATFORM='lintel64'
                    ;;
                # Official Raspbian distribution is 32 bit only
                # Will only ever compile for 32 bits
                # NOTE: Needs to be re-assessed
                # armv6l - RaspberryPi 1 B+
                # armv7l - RaspberryPi 3
                "armv6l"|"armv7l")
                    UBER_PLATFORM='linarm32'
                    ;;
            esac
            ;;
    esac
}

function check_dependencies() {
    UBER_PDF_LIB_DIR=$UBER_SDK_DIR/uberbaselibs/uberpdfsdk/lib/$UBER_PLATFORM/gcc/librtl
    case "$BUILD_MODE" in
        *-static)
            UBER_PDF_LIB=$UBER_PDF_LIB_DIR/libuberpdfsdk.a
            ;;
        *)
            UBER_PDF_LIB=$UBER_PDF_LIB_DIR/libuberpdfsdkdyn.so
            ;;
    esac

    echo -e "BUILD: Dependencies ..."

    echo -n "BUILD:   Checking lazbuild binary"
    if [ -x $LAZBUILD_BIN ]; then
        echo -e " FOUND"
        UBER_DEPS_OK=1
    else
        echo -e "\nBUILD ERROR: Cannot find lazbuild binary"
        echo -e "SEARCH: $LAZBUILD_BIN\n"
        UBER_DEPS_OK=0
        return
    fi

    echo -n "BUILD:   Checking UberBuild top folder"
    if [ -d $UBER_SDK_DIR ]; then
        echo -e " FOUND"
        UBER_DEPS_OK=1
    else
        echo -e "\nBUILD ERROR: Cannot find UberBuild top folder"
        echo -e "SEARCH: $UBER_SDK"
        echo -e "        This example expects that UberBuild/SDK reside under the searched folder\n"
        UBER_DEPS_OK=0
        return
    fi

    echo -n "BUILD:   Checking for the UberPDF .so/.a library"
    if [ -f $UBER_PDF_LIB ]; then
        echo -e " FOUND"
        UBER_DEPS_OK=1
    else
        echo -e "\nBUILD ERROR: Cannot find UberPDF .so/.a library"
        echo -e "SEARCH: $UBER_PDF_LIB"
        echo -e "        Without this library the binary will not run with success\n"
        UBER_DEPS_OK=0
        return
    fi

    echo -e "BUILD: Dependencies DONE"
}

function clean_examples() {
    echo -e "BUILD: Removing old binaries..."
    #echo -e "BUILD:   executing: \$ rm -f $BUILD_BIN"
    #rm -f "$BUILD_BIN"
    echo -e "BUILD:   executing: \$ rm -f $BUILD_BIN-$BUILD_MODE"
    rm -f "$BUILD_BIN-$BUILD_MODE"
    echo -e "BUILD: Removing old binaries DONE"
}

function build_example() {
    echo -e "BUILD: Building..."

    check_dependencies

    if [ "x$UBER_DEPS_OK" = "x1" ]; then
        clean_examples
        echo -e "BUILD: Compiling..."
        echo -e "BUILD:   executing: \$ $LAZBUILD_BIN -r -B --bm=$UBER_PLATFORM $BUILD_LPI"
        $LAZBUILD_BIN -r -B --bm=$BUILD_MODE $BUILD_LPI > /tmp/temp_$BUILD_MODE.log 2>&1

        if [ $? = 0 ]; then
            echo -e "BUILD: Compiling DONE"
            BUILD_OK=1
            rm -f /tmp/temp_$BUILD_MODE.log > /dev/null 2>&1
            #copy_examples
        else
            echo -e "BUILD ERROR: Compile did not succeed!"
            echo -e "BUILD ERORR: LOG '/tmp/temp_$BUILD_MODE.log'"
            echo -e "---- LOG -- (last 20 lines) -----------------------"
            tail -20 /tmp/temp_$BUILD_MODE.log
            echo -e "---- LOG ------------------------------------------\n"
        fi
    else
        echo -e "BUILD: Dependency check failed, exiting.\n"
        exit 1
    fi
    echo -e "BUILD: Building DONE\n"
}

function usage() {
    echo -e "This script is aware of the folowing build modes:\n"
    echo -e "\tlintel64:        Linux 64 bits, Intel."
    echo -e "\tlintel64-static: Linux 64 bits, Intel, static linking."
    echo -e "\tlintel32:        Linux 32 bits, Intel."
    echo -e "\tlintel32-static: Linux 32 bits, Intel, static linking."
    #echo -e "\tlinarm64:        Linux 64 bits, ARM."
    #echo -e "\tlinarm64-static: Linux 64 bits, ARM, static linking."
    echo -e "\tlinarm32:        Linux 32 bits, ARM."
    echo -e "\tlinarm32-static: Linux 32 bits, ARM, static linking.\n"
    echo -e "Options:\n"
    echo -e "\t-h, --help:      Prints this help message."
    echo -e "\t-v, --version:   Prints this build script version.\n"
    echo -e "Note: If no parameters are provided, script will"
    echo -e "      auto-detect the build mode."
    echo
}

###
# MAIN
#
echo -e "UberPDF Lazarus Examples build script v$BUILD_VERSION\n"

check_platform

case "$1" in
    "lintel64")
        echo -e "BUILD: Will build for 'lintel64'."
        UBER_PLATFORM="$1"
        BUILD_MODE="$1"
        build_example
        ;;
    "lintel64-static")
        echo -e "BUILD: Will build for 'lintel64', static linking."
        UBER_PLATFORM="lintel64"
        BUILD_MODE="$1"
        set_platform
        build_example
        ;;
    "lintel32")
        echo -e "BUILD: Will build for 'lintel32'."
        UBER_PLATFORM="$1"
        BUILD_MODE="$1"
        build_example
        ;;
    "lintel32-static")
        echo -e "BUILD: Will build for 'lintel32', static linking."
        UBER_PLATFORM="lintel32"
        BUILD_MODE="$1"
        build_example
        ;;
    "linarm32")
        echo -e "BUILD: Will build for 'linarm32'."
        UBER_PLATFORM="$1"
        BUILD_MODE="$1"
        build_example
        ;;
    "linarm32-static")
        echo -e "BUILD: Will build for 'linarm32'."
        UBER_PLATFORM="lintel32"
        BUILD_MODE="$1"
        build_example
        ;;
    "-h"|"--help")
        usage
        ;;
    "-v"|"--version")
        echo -e "BUILD: Version $BUILD_VERSION\n"
        ;;
    "")
        echo -e "BUILD: No known platform suggested, will build for '$UBER_PLATFORM'."
        BUILD_MODE="$UBER_PLATFORM"
        build_example
        ;;
    *)
        echo -e "Unknown build mode \"$1\"."
        echo
        usage
        ;;
esac
###
