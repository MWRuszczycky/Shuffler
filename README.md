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
 ( )-d17   m-d10   h-d22    H-d0   e-d11   x-d15
    s-d8  ( )-d7   f-d25    e-d3   s-d21  ( )-d4
   f-d24   e-d27   u-d23    i-d5 ( )-d20   e-d14
    o-d9    e-d1   t-d13   .-d28   o-d19   t-d16
    r-d2    s-d6   l-d26   t-d18 ( )-d12
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
