#!/bin/bash
#
# Date of last change: 15/05/2008
# Status: in work
#
# Call automated_tests.pl with own branch against development branch
#
./automated_tests.pl -v --branch development_branch@2886 --branch Achim_Geissler@2887 --skip-forcheck

exit 0
