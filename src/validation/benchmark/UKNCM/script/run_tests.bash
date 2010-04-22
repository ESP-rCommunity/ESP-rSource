#!/bin/bash
echo 'Script to run tests for DSM required for accreditation to '
echo 'with the UK NCM in order to issue EPCs'
echo 'Script runs one or more of the 10 DSM in ../'
echo ' '
echo 'Which tests to run?'
echo ' > 1-10 for individual tests'
echo ' > 0 to run all tests'
read N

#Set directory names
dir[1]=../Case1_natvent/cfg/
dir[2]=../Case2_mechvent/cfg/
dir[3]=../test3_deep_plan/cfg/
dir[4]=../test4_hotel/cfg/
dir[5]=../test5_school/cfg/
dir[6]=../test6_retail_unit_1/cfg/
dir[7]=../Case7_renewables/cfg/
dir[8]=../Case8/cfg/
dir[9]=../Case9_RefBldg/cfg/
dir[10]=../test10_retail_unit_1/cfg/

#Set cfg file names
file[1]=${dir[1]}"shal_plan_office_nv.cfg"
file[2]=${dir[2]}"shal_plan_office_mv.cfg"
file[3]=${dir[3]}"deep_plan.cfg"
file[4]=${dir[4]}"hotel.cfg"
file[5]=${dir[5]}"school_bc.cfg"
file[6]=${dir[6]}"retail_unit.cfg"
file[7]=${dir[7]}"shal_plan_office_pv.cfg"
file[8]=${dir[8]}"hotel.cfg"
file[9]=${dir[9]}"shal_plan_office_nv.cfg"
file[10]=${dir[10]}"retail_unit.cfg"

# Run simulations
for i in 1 2 3 4 5 6 7 8 9 10
do
  if [ $N = $i ] || [ $N = "0" ]; then
    ./remove_ncm_stuff.bash ${dir[$i]}
    echo ${dir[$i]}
    echo ${file[$i]}
    ./run.sh ${file[$i]}
    mv fort.92 res_det.$i
    mv fort.91 res_sum.$i
    mv fort.98 res_98.$i # Not used in document preparation
    mv fort.89 res_89.$i # Not used in document preparation
    rm -f fort.*
  fi

# touch results files
  touch res_det.$i res_sum.$i
done
echo 'Simulations complete.'
./write_report.bash

echo 'Remove results scratch (fort) files [y/n] ? n'
read A
# A=y
if [ $A = "y" ]; then
  rm -rf *det* *sum*
fi

echo 'Remove stripped, notional etc. model files [y/n] ? n'
read A
# A=y
if [ $A = "y" ]; then
  for i in 1 2 3 4 5 6 7 8 9 10
  do
    ./remove_ncm_stuff.bash ${dir[$i]}
  done
fi

# Delete dummy results files for models not simulated if any
echo 'Remove dummy results files [y/n] ? n'
read A
if [ $A = "y" ]; then
  for i in 1 2 3 4 5 6 7 8 9 10
  do
  if [ $N = $i ] || [ $N = 0 ];then
    dummy=0
  else
    rm -rf res_det.$i res_sum.$i res_98.$i
  fi
  done
fi
