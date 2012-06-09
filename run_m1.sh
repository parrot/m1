#! /bin/sh

[ -e 'm1' ] || { echo 'm1 does not exist'; exit 1; } 

if [ -z "$PARROT_M0_ASSEMBLER" ]; then
    if [ -e 'm0_assembler.pl' ]; then
        PARROT_M0_ASSEMBLER='./m0_assembler.pl'
    else
        echo 'm0_assembler.pl does not exist in $PWD and $PARROT_M0_ASSEMBLER not defined';
        exit 1;
    fi
fi

if [ -z "$PARROT_M0_INTERP" ]; then
    if [ -e 'm0' ]; then
        PARROT_M0_INTERP='./m0'
    else
        echo 'm0 does not exist in $PWD and $PARROT_M0_INTERP not defined';
        exit 1;
    fi
fi

filename=${1%.*}
file_suffixe=${1##*.}
[ "$file_suffixe" = 'm1' ] || { echo "file suffixe is not 'm1'"; exit 1; }

echo "# Running ./m1 $1 2>/dev/null $filename.m0"
./m1 $1 2>/dev/null > $filename.m0
[ -s $filename.m0 ] || { echo "nok..outputs a empty file $filename.m0 when compiling $1"; exit 1; }

echo "# Running $PARROT_M0_ASSEMBLER $filename.m0 >/dev/null"
$PARROT_M0_ASSEMBLER $filename.m0 >/dev/null

echo "# Running $PARROT_M0_INTERP $filename.m0b"
$PARROT_M0_INTERP    $filename.m0b || { echo "nok..$1 exits unexpectedly"; exit 1; }
echo "ok..$1 exits normally"
exit 0
