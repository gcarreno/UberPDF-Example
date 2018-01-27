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

### Split Document

This example will produce the PDF file `split-document.pdf`.

### Stamp Document

This example will produce the PDF file `stamp-document.pdf`.

## Project Build Modes

The only Build Mode I'm currently using and testing is the `lintel64`.

I would apreciate some help in testing `lintel32*`, `wintel32*` and `wintel64*` build modes.

I'm attempting to make this as complete as possible under my current test environment in terms of the dynamic libraries paths in all relevant places.

I have Lazarus configured with cross-compiling for win32 and win64 but I didn't test the ÜberBuild™ with any Windows versions.

I have 2 Raspberry Pi's and will, sometime in the future, have a gander at compiling this on them. I will add the apropriate Build Options once I do get around it.

### `lintel64`

This is the Linux x68_64 (64 bits) configuration with dynamic linking.

There's a build script: `build-lintel64.sh`, that triggers the build mode for Linux-x86_64

This is my main playground.

### `lintel64-static`

This is the Linux x68_64 (64 bits) configuration with static linking.

There's a build script: `build-lintel64-static.sh`, that triggers the build mode for Linux-x86_64

This and `lintel64` have been tested on my environment and work accordingly.

### `lintel32`

This is the Linux i386 (32 bits) configuration with dynamic linking.

### `lintel32-static`

This is the Linux i386 (32 bits) configuration with static linking.

### `wintel64`

This is the Windows 64 bits configuration with dynamic linking.

### `wintel64-static`

This is the Windows 64 bits configuration with static linking.

### `wintel32`

This is the Windows 32 bits configuration with dynamic linking.

### `wintel32-static`

This is the Windows 32 bits configuration with static linking.

