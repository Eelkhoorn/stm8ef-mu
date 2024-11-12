#!/bin/bash
# Compile the stm8ef kernel words for muforth
# a sym link forth.rst must exist in the same directory as this script, 
# pointing to stm8ef/out/BOARD/forth.rst

file=$(readlink -f forth.rst)
dir=${file%forth.rst}
words="$(pwd)"/kernel.mu4

workdir=$(pwd)
target=${workdir##*/}

#words="$(pwd)"/words.mu4
SOURCE=${BASH_SOURCE[0]}
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
dir=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

"$dir"/header.awk -v target="$target" forth.rst > $words
"$dir"/words-imm.awk forth.rst >> $words
"$dir"/words.awk forth.rst >> $words
"$dir"/constants.awk forth.rst >> $words
echo -e "\nld work/words+.mu4" >>$words

# disassembling
target_dir=${workdir%work/*}target/STM8
awk '
  FNR==NR && $0~/"goto"/ {next}
  FNR==NR && $2=="constant" {print "$"$1"  "$3;next}
  FNR<28 {next}
  $3~/ITC_IX/ {next}
  $0~/.*_.*_/ {next}
  $2=="equ" {print "$"$1"  "$3}
  ' $words "$target_dir/$target.efr" > "$target_dir/$target.con"

