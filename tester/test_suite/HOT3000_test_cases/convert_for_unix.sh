#!/bin/bash

# Del results files
rm -fr  ./*/cfg/out.*
rm -fr  ./*/cfg/*.html
rm -fr  ./*/cfg/DEC
rm -fr  ./*/cfg/*.res


# Convert paths for binary databases:
ls -1 ./*/cfg/*.cfg | while read cfgfile; do

  echo " - editing $cfgfile ";
  

done 