#!/usr/bin/awk -f

BEGIN{ printf "meta\nhex\n\n"}
$0~/= RAMPOOL$/&&$1~/^00/ {print gensub(/^0000/,"",1,$1" constant "$3)}
$0~/LED7LAST =/&&$1~/^00/ {print gensub(/^0000/,"",1,$1" constant "$3)}

