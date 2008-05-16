#!/bin/bash
#
./tester.pl -vv /home/esru/esp-r/bin_11.5_52Z/bps.exe /home/esru/esp-r/bin/bps.exe --no_data \
                       -p ../test_suite/esru_benchmark_model/cfg/bld_basic_summer.cfg > output.txt 2>&1

exit 0
