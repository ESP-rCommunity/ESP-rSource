Notes about the application stats

This utility scans the csv files (typically in the folder
databases/UK_NCM of the esp-r distribution) and produces
an ascii text file which summarizes the patterns found in
the csv file (see example 42.text in the folder which was
generated fro the 42.csv file in the folder).

The use of stats is:
  stats -file filename.csv

this will produce a file filename.text

Requirements:

In the folder where filename.csv is located you must copy the file 
DHWandAuxE.cvs from this source folder (typically DHWandAuxE.cvs
is the same as TM33SummationsForDHWandAuxE.csv located in databases
folder of the ESP-r distribution).

The script do_all would process all of the csv files in
a folder.
