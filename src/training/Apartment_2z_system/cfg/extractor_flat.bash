#!/bin/bash

echo "Script to generate results for the top-tail project models"
echo " "

for resfile in `ls *.res`
  do
    echo -ne "Recovering from $resfile"
    ./extract_flat $resfile
    echo " "
    ./extract_flat_csv $resfile
  done
  printf "\r Recovering $file ... done"
  echo " "
