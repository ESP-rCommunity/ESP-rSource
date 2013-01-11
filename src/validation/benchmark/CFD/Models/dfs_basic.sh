#!/bin/sh 
#
# Run dfs in default mode.
#
#  USAGE: dfs_basic.csh dfs_executable model
#    assumes model file is local and called model.dfd
#
DFS=$1
model=$2
#
rm -f CFD_mon TECend.DAT
# 
time $DFS -mode text -file $model".dfd" 1> $model".out" 2> $model".time" << XYZ 
d
a
e
d

-
-
n
y
y
XYZ
mv CFD_mon $model".mon"
mv TECend.DAT $model".TEC"
rm -f tmp_DFS_1 CFD_new_phi
