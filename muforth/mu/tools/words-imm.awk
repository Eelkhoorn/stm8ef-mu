#!/usr/bin/awk -f 
BEGIN { print "\ntarget-compiler\n"}
$6=="COMPO" {
label=$4
s=gensub(/^\^\//,"",1,$5)
r=gensub(/\/$/,"",1,s)
v=gensub(/^"/,"",1,r)
o=gensub(/"$/,"",1,v)
if(o~/"/) o=gensub(/'/,"","g",o)
}
$3==label":" {flag=1;next}
flag==1 {
flag=0
adr=gensub(/^00/,"",1,$1)
print adr " goto  : " o " ; [r]"
}

