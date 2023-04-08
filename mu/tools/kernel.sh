#!/bin/bash
# check the sym link below!!
symlink=/home/USER/muforth/mu/target/STM8/words.mu4

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
