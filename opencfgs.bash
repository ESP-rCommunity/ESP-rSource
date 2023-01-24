#!/bin/bash
#
# This script sequentially opens the training models of ESP-r.
# It uses the same logic as in the Install script.
# Source and target directories. It should be run from the top
# level of the ESP-r distribution.
HOMEdir=`pwd`			# Current directory.
MODdir="${HOMEdir}/models"	# Source models directory.
TRAdir="${MODdir}/training"	# Source training models.
cd ${MODdir}/

# Get list of cfg files in the training folder.
cfgnames=(`find ./training ./validation -name "*.cfg" -print`)
cfgcount=`find ./training ./validation -name "*.cfg" -print | wc -l`
cfgnum=0
for file in "${cfgnames[@]}"; do
  cfgnum=$((${cfgnum} + 1))
  printf "       -> ${cfgnum}/${cfgcount} (${file})"
  echo "Proceed with ${file:2} ?";
  YN=none;
  while [ "$YN" != "y" ] && [ "$YN" != "n" ] && [ "$YN" != "" ]
  do
    if [ "$YN" != "none" ]; then
      echo " ";
      echo "Please answer yes or no  (y/n) [y].";
    fi
    read YN
  done

  if [ "$YN" = "y" ]; then
    prj -file ${file:2}
    nedit ${file:2}
  fi
  echo "Continue?";
  YN=none;
  while [ "$YN" != "y" ] && [ "$YN" != "n" ] && [ "$YN" != "" ]
  do
    if [ "$YN" != "none" ]; then
      echo " ";
      echo "Please answer yes or no  (y/n) [y].";
    fi
    read YN
  done

  if [ "$YN" = "n" ]; then
    echo "Exiting the script.";
    exit;
  fi
done
