#!/bin/bash
bps -file cellular_mctl.cfg -mode text << XXX

c
annual.res
annual.mfr
6 8
6 8
3
6
n
s
y
single day run
y
y
-
-
XXX


res -file annual.res -mode text << XXY
 
d
f
-
c
g
f   # zone flux
b   # ventilation
f   # zone flux
a   # infiltration
b   # temperatures
a   # zone db
!
-
-
-
XXY
