#!/bin/sed -rf

/\#require WIPE/ d
/\#require mark/ d
/\#require MARK/ d
/^mark / d
/^MARK / d
/\\res / d
/^WIPE/ d
s*#require *ld work/lib/*
s*#include *ld work/lib/*
s/\\\\/comment ===/
s/\\/-- /g
s/CONSTANT/constant/g
s/LITERAL/literal/g
s/\&([0-9])/\#\1/g
s/ALLOT/allotr/g
s/NVM/flash/
s/RAM/ram/
s/HEX/hex/
s/DECIMAL/decimal/
s/BEGIN/begin/g
s/UNTIL/until/g
s/WHILE/while/g
s/REPEAT/repeat/g
s/DO/do/g
s/LOOP/loop/g
s/IF/if/g
s/ELSE/else/g
s/THEN/then/g
s/FOR/for/g
s/NEXT/next/g
s/C,/c,/g
s/ i / I /g
