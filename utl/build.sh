# Script for automating compilation of the source

# When run from the repository directory, the script will compile the
# source with -O2 ghc optimization placing all output files
# (.hi and .o) in a build/ directory and the binary in the repository
# directory. If the build/ directory already exists and is not empty,
# then the user is prompted to delete its contents.

ODIR="build"
PROG="shuffler"
RUNGHC="ghc -O2 -outputdir $ODIR -o $PROG -icode/src --make code/src/main.hs"

# Check if the compiled program already exists.
if [ -e "$PROG" ]; then
    echo "$PROG already exists."
    echo -n "Do you want to delete it and recompile? (y/n): "
    read RESPONSE
    case $RESPONSE in
        "y" ) rm -i $PROG;;
        "n" ) echo "aborting"; exit 0;;
        * ) echo "Unknown command, aborting"; exit 1;;
    esac
fi

# Check if the build directory already exists and is empty.
if [ -d "$ODIR" ]; then
    if [ ! -z "$(ls -A $ODIR)" ]; then
        echo "The $ODIR directory already exists and is not empty."
        echo -n "Do you want to delete the contents of $ODIR? (y/n/abort): "
        read RESPONSE
        case $RESPONSE in
            "y" ) rm -rI $ODIR/*.*;;
            "n" ) ;; # Do nothing.
            "abort" ) echo "aborting"; exit 0;;
            *) echo "Unknown command, aborting"; exit 1;;
        esac
    fi
else
    echo "Making output directory $ODIR ..."
    mkdir $ODIR
fi

echo "Compling with -O2 optimization ..."
echo "Running: $RUNGHC"
$RUNGHC
