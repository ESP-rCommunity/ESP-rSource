#! /bin/gawk -f
#
# Author: Achim Geissler
# Created: 2012/04/10
# Status: Usable
#
# This script takes a .dictionary file from ESP-r and converts
# the records for each variable to a format which can directly be
# copied to input.xml for time step data output.
#
# If you are interested in summary data, change
#  <step_variable> and </step_variable> in line 39 to
#  <log_variable> and </log_variable>.
# 
BEGIN { 
  if (ARGC<2) {
    printf("  *** ConvertDictionary ***\n")
    printf("  Call: \n")
    printf("    $ ConvertDictionary.awk  <file> \n")
    exit 1
  }
  if (!match(ARGV[1],".dictionary")) {
    printf("  *** wrong input file extension! ****\n")
    printf("      File(s) must be of type .dictionary!\n")
    exit 1
  }
# Set field separator 
  FS = "[" 
}
#
{
  if (FNR == 1) {
      # set output file name
      split(FILENAME,trunk,".")  # split at "." ... trunk[2] now contains "dictionary"
      FILENAME=(trunk[2] "_input.xml")
  }
  if (FNR >= 1) {
    gsub(/ $/, "", $1); # remove trailing whitespace from field
    printf("  <step_variable>%s</step_variable>\n",$1) > FILENAME
  } # end if FNR >= 1
}
#
END {
# 

#
}
