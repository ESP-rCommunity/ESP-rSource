#!/bin/sh
# this registers a model in the folder test_box and sets
# the site parameters
VERSION=$1
$VERSION/prj -mode text<<XXX
e # create new
test_box
a test model via script input
../doc/test_box.log
n # no edit
n # no images
52.0
1.0
2008  # year
-
XXX
