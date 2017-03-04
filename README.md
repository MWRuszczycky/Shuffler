# Shuffler

## Introduction

This is a really simple program I wrote while trying to learn Haskell. It takes an input string from either a file or stdin and shuffles it. The characters in the input string can be labeled with their positions in the original string in either decimal or hexadecimal. Output strings that have been numbered in this way can then be unshuffled with a different command line option.

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

## Installation

## Removal

## License

This program is licensed under a [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause) (see the `LICENSE.txt` file).
