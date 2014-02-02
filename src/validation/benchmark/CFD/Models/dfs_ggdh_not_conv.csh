#!/bin/csh 
#
# Run dfs in default mode.
#
#  USAGE: dfs_ggdh_not_conv.csh dfs_executable model.dfs
#
set DFS=$1
set MODEL=$2
#
rm -f CFD_mon TECp_1.dat
# 
time $DFS -mode text -file $MODEL << XYZ 
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
mv CFD_mon $MODEL"_ggdh.mon"
mv TECp_1.dat $MODEL"_ggdh.TEC"
rm -f tmp_DFS_1 CFD_new_phi
