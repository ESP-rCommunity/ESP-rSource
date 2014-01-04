#!/bin/csh 
#
# Run dfs in default mode.
#
#  USAGE: dfs_bi-cg.csh dfs_executable model.dfs
#
set DFS=$1
set MODEL=$2
#
rm -f CFD_mon TECp_1.dat
# 
time $DFS -mode text -file $MODEL << XYZ 
c
a
-
d
a
e
e
-
-
n
y
XYZ
mv CFD_mon $MODEL"_bi-cg.mon"
mv TECp_1.dat $MODEL"_bi-cg.TEC"
rm -f tmp_DFS_1 CFD_new_phi
