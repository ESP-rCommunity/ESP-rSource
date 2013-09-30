#!/bin/sh

# This script searches fro csv files created by open office,
# and converts them into bcd-friendly .csv format

ls -1 *.csv | while read csv_file; do

  rootname=`echo $csv_file | sed "s/\.csv$//"`
  bcd_file="${rootname}.bcd"
  # Nuke `"'s, extra commas and trailing commas.
  echo "converting ${csv_file} > ${bcd_file}"
  cat $csv_file | sed "s/\"//g" | sed "s/,,//g" | sed "s/, *$//g" > $bcd_file

done;
