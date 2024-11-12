#!/bin/bash
echo -e "\n Loads image in muforth, saves it to /tmp/image.bin,\n and flashes image to target via STlink device.\n"
BOARD=("MINDEV" "W1209-FD" "MINDEV-DOLIT" "L051F3-DOLIT" "STM8L051F3" "L151K4-DOLIT" "STM8L151K4")
MCU=("stm8s103f3"  "stm8s103f3" "stm8s103f3" "stm8l051f3" "stm8l051f3"  "stm8l151k4" "stm8l151k4")
declare -i ind
file="$1"
base_dir=${file#*work/}
target=${base_dir%%/*}
echo "target: $target"
for i in "${BOARD[@]}";do
#echo "$i"
[[ "$target" == "$i" ]] && mcu="${MCU[$ind]}"
ind+=1
done
echo "mcu: $mcu"

./muforth -f $1  save-image /tmp/image.bin bye # 2> /dev/null

stm8flash -c stlinkv2 -p "$mcu" -w /tmp/image.bin
