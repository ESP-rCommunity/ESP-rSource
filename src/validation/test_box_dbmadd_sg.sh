#!/bin/sh
# This takes model in the folder test_box after the scripts
# test_tox_register.sh and test_box_dbmanage.sh have been run
# and adds MLC:
#  single_glz (for use between buffer & room)
VERSION=$1
cd test_box/dbs
cp test_box_a.constrdb test_box_a.constrdb-
cd ../cfg
pwd
$VERSION/prj -mode text -file test_box_a.cfg<<XXX
b # database management
e # MLC
a # browse
1 # add
a # at end of list
single_glz
i # select the layer
y # confirm
m # glass cat
c # clear float
y # use it
6 # thickness
b # type
b # tmc
c # optics
c # pick SCF8783_06nb
-
-
> # save
y
y
a # browse edit
1 # add
a # at end of list
conc_pla_ptn
i # change to plaster
y
f # plaster
a # dense plaster
y
20
!
b # append
y
0 # page
r # gaps
a # air gap
y
a # accept
0.17
0.17
0.17
10 # thickness
!
b # append
y
b # concrete
l # heavy mix
y
150
!
b # append
y
0 # page
r # gaps
a # air gap
y
a # accept
0.17
0.17
0.17
10 # thickness
!
b # append
y
f # plaster
a # dense plaster
y
20
-
>
y
y
-
-
y
-
XXX
