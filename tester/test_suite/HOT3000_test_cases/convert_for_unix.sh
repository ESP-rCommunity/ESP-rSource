#!/bin/bash

# Del results files
rm -fr  ./*/cfg/out.*
rm -fr  ./*/cfg/*.html
rm -fr  ./*/cfg/DEC
rm -fr  ./*/cfg/*.res


# Convert paths for binary databases:
ls -1 ./*/cfg/*.cfg | while read cfgfile; do

  echo " - editing $cfgfile ";
  perl -pi -e "s/\*pdb(\s+)\.\.\/\.\.\/databases\/plantc\.db1/*pdb\1\/usr\/esru\/esp-r\/databases\/plantc.db1/g" $cfgfile
  perl -pi -e "s/\*evn(\s+)\.\.\/\.\.\/databases\/profiles\.db1/*evn\1\/usr\/esru\/esp-r\/databases\/profiles.db1/g" $cfgfile
  perl -pi -e "s/\*clm(\s+).+$/*clm\1\/usr\/esru\/esp-r\/climate\/CAN_ON_Toronto_CWEC/g" $cfgfile

done 