#!/bin/bash
current_dir=$(pwd)
echo -e "\n Updating STM8 kernel in ../../stm8ef/out/$1.\n"
dir=$(pwd)

echo "updating $1"
cd ../../stm8ef
make BOARD="$1" #2>/dev/null >/dev/null
cd "$dir"/work/"$1"
./kernel.sh

cd "$current_dir"
