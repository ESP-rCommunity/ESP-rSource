#!/bin/csh 
#
# Run dfs in default mode.
#
#  USAGE: dfs_basic.csh dfs_executable  model.dfd
#    assumes model file is local and called model.dfd
#
set DFS=$1
set MODEL=$2
#
rm -f CFD_mon TECp_1.dat
time $DFS -mode text -file $MODEL << XYZ 
d
a
e
e
-
-
n
y
XYZ
mv CFD_mon $MODEL".mon"
mv TECp_1.dat $MODEL".TEC"
rm -f tmp_DFS_1 CFD_new_phi
