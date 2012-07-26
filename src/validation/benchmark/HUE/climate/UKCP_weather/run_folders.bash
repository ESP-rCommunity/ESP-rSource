#!/bin/bash

for i in `ls -d /media/WeatherGen_??/*`
do
  echo $i

  unzip $i"/output_6.zip" -d ./
  unzip $i"/output_5.zip" -d ./
  unzip $i"/output_4.zip" -d ./
  unzip $i"/output_3.zip" -d ./
  unzip $i"/output_2.zip" -d ./
  unzip $i"/output_1.zip" -d ./

  ./ESP-r_weather_gen

  k=${#i}-2
  j=${i:21:$k}
  zip $j.ascii.zip *.clm.a
  mv r_0001_cntr_hly.yr001.clm $j.bin
  rm -rf output_?.zip *.csv *.xml *.txt *.clm.a *.clm
done
