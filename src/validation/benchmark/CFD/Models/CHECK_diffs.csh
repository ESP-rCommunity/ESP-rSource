#!/bin/csh
foreach i (*.TEC)
./TECdiff.csh $i $1/$i
end
