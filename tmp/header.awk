#!/usr/bin/awk -f 

$0~/END_SDCC_FLASH/ {adr=gensub(/^00/,"0x",1,$1)}
END { print "loading words\n\nhex\n\n\
70  constant @ram\n\
400 @ram - constant #ram\n\
@ram image-region r-region\n\
r-region h @ ram-pointer ! "
printf "%x",0x808B+adr
print " constant @flash\n\
a000 @flash - constant #flash\n\
@flash  image-region f-region\n\
f-region h @ flash-pointer !\n\
\n__meta"}

