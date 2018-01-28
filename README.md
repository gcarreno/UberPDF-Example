# ÜberPDF SDK Example for Lazarus

## My environment

 * Ubuntu 17.10 64 bits
 * Lazarus 1.9.0
 * FPC 3.1.1
 * UberBuild under folder `uber`

## Examples

I've split all the examples into their own units.

### Hello World

This example will produce the PDF file `hello-world.pdf`.

### Add JPEG

This example will produce the PDF file `add-jpeg.pdf`.

It needs the image file at `uber/3rdpartylibs/_testfiles/testfiles/jpg/uberdude-uberairshare-stage-board-01.jpg`.

### Split Document

This example will produce the PDF file `split-document.pdf`.

It needs the PDF file at `PDF_files/NASA_Solar_Dynamics_Observatory_PressKit.pdf`.

### Stamp Document

This example will produce the PDF file `stamp-document.pdf`.

It needs these PDF files at `PDF_files/DocumentToStamp.pdf` and `PDF_files/PlatypusStamp.pdf`.

## Project Build Modes

The only Build Mode I'm currently using and testing is the `lintel64`.

I would apreciate some help in testing `lintel32*`, `wintel32*` and `wintel64*` build modes.

I'm attempting to make this as complete as possible under my current test environment in terms of the dynamic libraries paths in all relevant places.

I have Lazarus configured with cross-compiling for win32 and win64 but I didn't test the ÜberBuild™ with any Windows versions.

I have 2 Raspberry Pi's and will, sometime in the future, have a gander at compiling this on them. I will add the apropriate Build Options once I do get around it.

### Build Script

The build script, `build.sh`, builds the various binaries.

At the moment it knows the following build modes:

 1. lintel64        (Linux, Intel, 64 bits)
 2. lintel64-static (Linux, Intel, 64 bits, static linking)
 3. lintel32        (Linux, Intel, 32 bits)
 4. lintel32-static (Linux, Intel, 32 bits, static linking)
 5. -h, --help      (Prints the help message)
 6. -v, --version   (Prints the script version)

### `lintel64`

This is the Linux x68_64 (64 bits) build mode with dynamic linking.

This is my main playground, alongside the static linking below.

Both are Linux 64 bits, just the type of linking differs.

### `lintel64-static`

This is the Linux x68_64 (64 bits) build mode with static linking.

This and `lintel64` have been tested on my environment and work accordingly.

### `lintel32`

This is the Linux i386 (32 bits) build mode with dynamic linking.

### `lintel32-static`

This is the Linux i386 (32 bits) build mode with static linking.

### `wintel64`

This is the Windows 64 bits build mode with dynamic linking.

**Status**: Fiddling with Microsoft Visual C (MSVC) Community to get UberSDK built.

### `wintel64-static`

This is the Windows 64 bits build mode with static linking.

### `wintel32`

This is the Windows 32 bits build mode with dynamic linking.

### `wintel32-static`

This is the Windows 32 bits build mode with static linking.

## Future Build Modes

### `linarm64`

This is the Linux ARM (64 bits) build mode with dynamic linking.

**Status**: Need to re-install my Raspberry Pi on a bigger SD Card.

### `linarm64-static`

This is the Linux ARM (64 bits) build mode with static linking.

### `linarm32`

This is the Linux ARM (32 bits) build mode with dynamic linking.

### `linarm32-static`

This is the Linux ARM (32 bits) build mode with static linking.
