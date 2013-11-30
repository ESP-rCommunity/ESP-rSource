#!/bin/bash

echo "Script to generate results for the top-tail project models"
echo " "

for resfile in `ls *.res`
  do
    echo -ne "Recovering from $resfile"
    ./extract_det $resfile
    echo " "
    ./extract_det_csv $resfile
  done
  printf "\r Recovering $file ... done"
  echo " "
