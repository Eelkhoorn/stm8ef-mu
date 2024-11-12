#!/usr/bin/awk -f 

$0~/USERRAM/ {userram = gensub(/^0000/,"",1,$1)}

$3=="RESTC:" {flag=-1}
flag==-1 && $0~/IRET/ { flag=0;kernel_end=strtonum("0x"$1);end=kernel_end+1}
END { print "loading work/"target"/kernel.mu4\n\nhex\n\n"
print userram "  constant @ram\n\
ram @ram dup dp 2!\n\
eeprom @eeprom dup dp 2!"
printf "%x%s",end,\
" constant @flash\n\
flash @flash dup dp 2!\n\
\n__meta"
}
