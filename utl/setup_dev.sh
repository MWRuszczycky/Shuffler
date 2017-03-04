# Setup command line aliases for development.
# Run from the repository directory with

# $ source utl/setup_dev.sh

# The aliased commands should be run from the repository directory.
# The commands are:

# comp  : compile
# run   : run the binary from the repository directory
# clean : delete all binaries and output files

alias comp='ghc -outputdir build -o bin/shuffler -icode/src --make code/src/main.hs'
alias run='bin/shuffler'
alias clean='rm -I bin/* build/*'
