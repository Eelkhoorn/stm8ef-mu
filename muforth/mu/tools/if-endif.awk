#!/usr/bin/awk -f
# script to check .if/.endif balance in assembler files
# prints linenumber, balance, line
$0~/\.if/{a++1; print NR"\t",a"\t",$0}$0~/.endif/{a-=1;print NR"\t",a"\t",$0}
