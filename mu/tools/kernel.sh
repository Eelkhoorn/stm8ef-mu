#!/bin/bash
# Compile the stm8ef kernel words for muforth
# a sym link forth.rst must exist in the same directory as this script, 
# pointing to stm8ef/out/BOARD/forth.rst

# The link below must point to your muforth/mu/target/STM8/words.mu4
symlink=/home/pi/build/muforth/mu/target/STM8/words.mu4

file=$(readlink -f forth.rst)
dir=${file%forth.rst}
map=$dir*.map
trap=$(awk '$2=="_TRAP_Handler" {print $1}' $map)
TRAP="0x${trap#0000}"
echo "_TRAP_Handler: "$TRAP
export TRAP
words="$(pwd)"/words.mu4
SOURCE=${BASH_SOURCE[0]}
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
dir=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

"$dir"/header.awk --non-decimal-data forth.rst > $words
"$dir"/words-imm.awk forth.rst >> $words
"$dir"/words.awk --non-decimal-data forth.rst >> $words
"$dir"/constants.awk forth.rst >> $words
[ -h $symlink ] && rm $symlink
[ -e $words ] && ln -s $words $symlink
