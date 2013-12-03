#!/bin/sh 
#
# Run dfs in default mode.
#
#  USAGE: dfs_ggdh_not_conv.sh dfs_executable model.dfs
#
DFS=$1
model=$2
#
rm -f CFD_mon TECp_1.dat
# 
time $DFS -mode text -file $model".dfd" 1> $model"_ggdh.out" 2> $model"_ggdh.time" << XYZ 
c
b
-
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
mv CFD_mon $model"_ggdh.mon"
mv TECp_1.dat $model"_ggdh.TEC"
rm -f tmp_DFS_1 CFD_new_phi
