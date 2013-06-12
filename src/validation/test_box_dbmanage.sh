#!/bin/sh
# This takes model in the folder test_box and scans it
# to establish databases and uses a local copy of materials
# and constructions.
# the command line needs to include path to esp-r distribution
VERSION=$1
cd test_box/cfg
pwd
$VERSION/prj -mode text -file test_box.cfg<<XXX
b # database management
- 
s # save as
test_box_a.cfg
test_box_a
b
c # materials
d # copy default

-
e # MLC
d # copy default

-
-
-
r
-
XXX
