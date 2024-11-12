#!/usr/bin/awk -f 
BEGIN {print "\ntarget\n"}
$3=="RXA:" {flag=-1}
flag==-1 && $1==000001 {flag=7;next}
flag==7 {flag=0;offset=strtonum("0x"$1);printf "%s%x\n", "\n-- offset: ",offset}
$5~/wName/ {next}
$0 !~ /COMPO/ && ($3 == "HEADER" || $3 == "HEADFLG") {
s=gensub(/^\^\//,"",1,$5)
r=gensub(/\/$/,"",1,s)
v=gensub(/^"/,"",1,r)
o=gensub(/"$/,"",1,v)
if(o~/"/) o=gensub(/'/,"","g",o)
adr=strtonum("0x"$1)
if(adr<0x8000) adr=adr+offset

printf "%x%s%s%s", adr, " goto  : ", o, " ;\n"
}
$3=="RESTC:" {flag=-1}
flag==-1 && $0~/IRET/ { flag=0;kernel_end=strtonum("0x"$1);end=kernel_end+1}
END {printf "\n%x%s\n",end," goto\n"}
