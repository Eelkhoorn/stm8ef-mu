#!/usr/bin/awk -f 
BEGIN { offset=0x808b; print "\ntarget\n"}
$3=="END_SDCC_FLASH" {FLASH_END=gensub(/^0+/,"",1,$1)}
$5~/wName/ {next}
$0 !~ /COMPO/ && ($3 == "HEADER" || $3 == "HEADFLG") {
s=gensub(/^\^\//,"",1,$5)
r=gensub(/\/$/,"",1,s)
v=gensub(/^"/,"",1,r)
o=gensub(/"$/,"",1,v)
if(o~/"/) o=gensub(/'/,"","g",o)
adr=gensub(/^00/,"0x",1,$1)
adr=adr+0   # cast string to number
if(adr<0x8000) adr=adr+offset

printf "%x%s%s%s", adr, " goto  : ", o, " ;\n"
}
END {print "\n"FLASH_END" 808B \\f + goto\n"}
