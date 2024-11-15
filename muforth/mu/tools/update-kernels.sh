#!/bin/bash
current_dir=$(pwd)
echo -e "\n Updating STM8 kernels in ../../stm8ef/out.\n"
BOARD=("MINDEV" "W1209-FD" "STM8L051F3" "STM8L151K4")
for i in "${BOARD[@]}" ; do
	echo "updating $i"
	cd ../../stm8ef
	make BOARD="$i" >/dev/null # 2>/dev/null
	cd "$current_dir"/work/"$i"
	./kernel.sh
cd "$current_dir"
done
