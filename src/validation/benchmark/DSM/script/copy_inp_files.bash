#!/bin/bash

mkdir Case1 Case2 Case3 Case4 Case5 Case6 win_exe_inputs/
cp ../Case1_natvent/cfg/*inp Case1
cp ../Case2_mechvent/cfg/*inp Case2
cp ../test3_deep_plan/cfg/*inp Case3
cp ../test4_hotel/cfg/*inp Case4
cp ../test5_school/cfg/*inp Case5
cp ../test6_retail_unit_1/cfg/*inp Case6

mv Case1 win_exe_inputs/
mv Case2 win_exe_inputs/
mv Case3 win_exe_inputs/
mv Case4 win_exe_inputs/
mv Case5 win_exe_inputs/
mv Case6 win_exe_inputs/

zip win_exe_inputs.zip win_exe_inputs/
zip win_exe_inputs.zip win_exe_inputs/*
zip win_exe_inputs.zip win_exe_inputs/*/*
