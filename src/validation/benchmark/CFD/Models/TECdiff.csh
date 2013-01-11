#!/bin/csh
paste $1 $2 > TECtemp.dat
echo " ----------------------------------------------  "
echo "  "
echo "Checking files: "$1" & "$2
echo "  "
echo "Relative differences:"
awk '(NF==20) {print abs(rd($1,$11)), abs(rd($2,$12)), abs(rd($3,$13)), \
         abs(rd($4,$14)), abs(rd($5,$15)), abs(rd($6,$16)), abs(rd($7,$17)), \
         abs(rd($8,$18)), abs(rd($9,$19)), abs(rd($10,$20)) }\
    function rd(a,b) {if (a!=0) {return (a-b)/a} else {return (a-b)}} \
    function abs(a) {if (a>0) {return a} else {return a*(-1)} }' \
    TECtemp.dat | awk -f maxval.awk
echo "  "
echo "Absolute differences:"
awk '(NF==20) {print abs(rd($1,$11)), abs(rd($2,$12)), abs(rd($3,$13)), \
         abs(rd($4,$14)), abs(rd($5,$15)), abs(rd($6,$16)), abs(rd($7,$17)), \
         abs(rd($8,$18)), abs(rd($9,$19)), abs(rd($10,$20)) }\
    function rd(a,b) {return (a-b)} \
    function abs(a) {if (a>=0) {return a} else {return a*(-1)} }' \
    TECtemp.dat | awk -f maxval.awk
echo "  "
rm -f TECtemp.dat
