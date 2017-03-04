# Shuffler

## Introduction

This is my first Haskell program. It takes an input string from either a file or stdin and randomly shuffles it. The characters in the input string can be labeled with their positions in the original string in either decimal or hexadecimal. Output strings that have been numbered in this way can then be unshuffled with a different command line option.

## Usage

The command line usage is
```bash
shuffler [OPTION] [FILE]
```
The `FILE` is the name of file to shuffle or unshuffle. If no `FILE` is specified, then stdin is used as the input source. The program output is sent to stdout. The options are
* `-d`: shuffle with decimal numbering (default).
* `-x`: shuffle with hexadecimal numbering.
* `-n`: shuffle, but do not number the output.
* `-u`: unshuffle.
* `-h`: display help.

For example, if the file `test.txt` has contents
```text
Here is some text to shuffle.
```
then the command
```bash
./shuffler -x test.txt
```
produces the output
```text
m-xa o-x13 u-x17 t-x10 f-x18 e-x1 ( )-xc s-x8 t-xd e-xe ( )-x7 ( )-x4 h-x16 r-x2 e-x3 ( )-x14 .-x1c s-x6 f-x19 o-x9 x-xf t-x12 i-x5 H-x0 l-x1a s-x15 ( )-x11 e-x1b e-xb
```
The `( )` strings denote spaces in the original string. New lines are removed and replaced with spaces. Likewise, running
```bash
./shuffler -x test.txt | ./shuffler -u
```
just returns the original string (with new lines replaced with spaces).

## Installation and removal

If you have the [Glasgow Haskell Compiler](https://www.haskell.org/ghc) installed, you can compile the source from the repository parent directory by running the script
```bash
sh utl/build.sh
```
This will put the compiled binary `shuffler` in the parent repository directory and create a `build/` directory where all the `*.hi` and `*.o` files will be placed. Compilation uses the `-O2` optimization and is equivalent to running
```bash
ghc -O2 -outputdir /build -o shuffler -icode/src --make code/src/main.hs
```
This script does not do anything else other than compile the source and organize the output within the repository directory. So, to uninstall, just delete the repository.

## License

This program is licensed under a [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause) (see the `LICENSE.txt` file).
