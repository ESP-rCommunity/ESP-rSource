#!/bin/bash

# 


# Arg 1: 'all', '' or a specific module name 

# Get doc sumbmodule and name it 'doc'.

echo "run co" 
svn co https://github.com/ESP-rCommunity/ESP-rDoc/trunk ./doc
echo " ok"
svn propset svn:ignore "doc" .


