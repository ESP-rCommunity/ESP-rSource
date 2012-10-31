#!/bin/bash

echo "script to run mechanical simulation and report back plant usage"
rm -f annual.res annual.mfr out.results
./win_run.sh > out.results
clear
more out.results
