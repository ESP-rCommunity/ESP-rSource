#!/bin/csh
# this opens all of the cfg files in the CEN folder

echo "Uses prj to open each tester model for checking. It reads a"
echo "file named which_cfg created via the commands:"
echo "cd test_suite"
echo "find . -name *.cfg -print >which_cfg"
echo "then run:./ckeck_suite.sh "
echo " "
echo currently working on $PWD

foreach i ( `cat which_cfg` )
  echo looking at $i
  prj -file $i
  echo " "
  set x="y"
  echo "Proceed (y/n) ?"
  set x = {$<}
  if ( $x == "n") then
    echo " "
    echo "Ok, stopping further checks."
    echo " "
    exit
  endif
end
