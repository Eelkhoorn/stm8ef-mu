#!/usr/bin/awk -f


BEGIN{ print "meta\n"}
$0~/= RAMPOOL$/&&$1~/^00/ {print gensub(/^0000/,"",1,$1" constant "$3)}
$0~/LED7LAST =/&&$1~/^00/ {print gensub(/^0000/,"",1,$1" constant "$3)}

END {print "\nld werk/words+\n"}