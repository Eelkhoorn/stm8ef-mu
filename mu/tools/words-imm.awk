#!/usr/bin/awk -f 
BEGIN { print "\ntarget-compiler\n"}
$5~/wName/ {next}
$1~/0000/ {next}
$1~/^...$/ {next}
$1~/^....$/ {next}
$0~/COMPO/ {
s=gensub(/^\^\//,"",1,$5)
r=gensub(/\/$/,"",1,s)
v=gensub(/^"/,"",1,r)
o=gensub(/"$/,"",1,v)
if(o~/"/) o=gensub(/'/,"","g",o)
adr=gensub(/^00/,"",1,$1)
print adr " goto  : " o " ; [r]"
}

