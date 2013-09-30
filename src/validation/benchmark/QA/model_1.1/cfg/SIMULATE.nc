#!/bin/csh -fb
set CONFIG=$1
set VERSION=$2
$VERSION/bps -file $CONFIG -mode text <<XXX

c
$CONFIG.nc_res
9 1
15 1
3
1
s
n
ESRU Standard test: $CONFIG
y
y
-
-
XXX
$VERSION/bps -file $CONFIG -mode text <<XXX

c
$CONFIG.ncs_res
9 7
15 7
3
1
s
n
ESRU Standard summer test: $CONFIG
y
y
-
-
XXX
