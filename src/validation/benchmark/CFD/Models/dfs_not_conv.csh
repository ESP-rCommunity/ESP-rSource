#!/bin/csh 
#
# Run dfs in default mode.
#
#  USAGE: dfs_not_conv.csh dfs_executable model.dfs
#
set DFS=$1
set MODEL=$2
#
rm -f CFD_mon TECp_1.dat
# 
time $DFS -mode text -file $MODEL << XYZ 
d
a
y
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
