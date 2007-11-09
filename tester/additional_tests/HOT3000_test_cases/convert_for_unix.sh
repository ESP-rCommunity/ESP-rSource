#!/bin/bash

# Del results files
rm -fr  ./*/cfg/out.*
rm -fr  ./*/cfg/*.html
rm -fr  ./*/cfg/DEC
rm -fr  ./*/cfg/*.res


# Convert to unix line ending, and remove x permissions

ls -1 ./*/*/* | while read myfile; do


#   svn revert $myfile;

  dos2unix $myfile;
  chmod a-x $myfile;

done 


# Convert paths for binary databases:
ls -1 ./*/cfg/*.cfg | while read cfgfile; do

  echo " - editing $cfgfile ";
  
  # Edit sim preset
  perl -pi -e "s/\s*[0-9]+\s+[0-9]+\s+[0-9]+\s+[0-9]+\s+def\s*# period & name/  1   1   7   1  test # period & name/g" $cfgfile


  # Edit databases
  perl -pi -e "s/\*prm(\s+)\.\.\/\.\.\/databases\/constr\.db1/*prm\1\/usr\/esru\/esp-r\/databases\/constr.db1/g" $cfgfile
  perl -pi -e "s/\*pdb(\s+)\.\.\/\.\.\/databases\/plantc\.db1/*pdb\1\/usr\/esru\/esp-r\/databases\/plantc.db1/g" $cfgfile
  perl -pi -e "s/\*evn(\s+)\.\.\/\.\.\/databases\/profiles\.db1/*evn\1\/usr\/esru\/esp-r\/databases\/profiles.db1/g" $cfgfile
  perl -pi -e "s/\*clm(\s+).+$/*clm\1\/usr\/esru\/esp-r\/climate\/CAN_ON_Toronto_CWEC/g" $cfgfile



done 