#!/bin/sh
# This takes model in the folder test_box after the scripts
# test_tox_register.sh and test_box_dbmanage.sh have been run
# and adds MLC:
#  single_glz (for use between buffer & room)
#  conc_pla_pt (plastered concrete structure partition)
#  acu_gyp_ptn (acoustic gypboard partition)
#  apt_conc_cl (ceiling with void & structure)
#  apt_conc_fl (matched floor with struc and void)
# note: if initial MLC file changes then the position
# of pt_conc_cl might change (in line 163)
# the command line needs to include path to esp-r distribution
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
1
a # at end of list
acu_gyp_ptn
i # change layer
y
f # plaster
h # white gyp
y
24
!
b # append
y
o # insul
a # glass fibre
y
50
!
b # append
y
f # plaster
h # white gyp
y
24
-
1 # add
a # end of list
apt_conc_cl
i # change layer
y
d # wood
u # parquet
y
20
!
b # append
y
g # screeds
b # cast screed
y
30
!
b # append
y
b # concrete
c # aerated
y
50
!
b # append
y
b # concrete
l # heavy
y
150
!
b # append
y
r # gaps
a
y
a
0.1
0.1
0.1
200
!
b # append
y
h # tiles
j # ceiling mine
y
12.5
-
1 # add
e # copy invert
0 # page
0 # page
f # last page
a # apt_conc_cl
-
apt_conc_fl
-
> # save
y
-
-
-
y
-
XXX
