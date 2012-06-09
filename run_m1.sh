#! /bin/sh

[ -e 'm1' ] || { echo 'm1 does not exist'; exit 1; } 
[ -e 'm0_assembler.pl' ] || { echo 'm0_assembler.pl does not exist'; exit 1; } 
[ -e 'm0' ] || { echo 'm0 does not exist'; exit 1; } 

filename=${1%.*}
file_suffixe=${1##*.}
[ "$file_suffixe" = 'm1' ] || { echo "file suffixe is not 'm1'"; exit 1; }

echo "running $1..."
./m1 $1 2>/dev/null > $filename.m0
[ -s $filename.m0 ] || { echo "nok..outputs a empty file $filename.m0 when compiling $1"; exit 1; }
./m0_assembler.pl $filename.m0 >/dev/null
./m0 $filename.m0b || { echo "nok..$1 exits unexpectedly"; exit 1; }
echo "ok..$1 exits normally"
exit 0
