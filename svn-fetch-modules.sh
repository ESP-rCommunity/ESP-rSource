#!/bin/bash

# 


# Arg 1: 'all', '' or a specific module name 

# Get doc sumbmodule and name it 'doc'.


if [ ! -d ./doc ]; then
  echo "Checking out documentation via svn...-> doc.." 
  svn co https://github.com/ESP-rCommunity/ESP-rDoc/trunk ./doc
  
.
else 
  echo "Refreshing doc..."
  cd ./doc 
  svn update 
  cd .. 
fi 


if [ ! -d ./databases ]; then
  echo "Checking out data and climate files via svn...-> databases.." 
  svn co https://github.com/ESP-rCommunity/ESP-rDatabases/trunk ./databases

  .
else 
echo "Refreshing databases..."
  cd ./databases
  svn update 
  cd .. 
fi 

if [ ! -d ./models ]; then
  echo "Checking out models via svn...-> models .." 
  svn co https://github.com/ESP-rCommunity/ESP-rModels/trunk ./models

  
  
else 
  echo "Refeshing models ..."
  cd ./models
  svn update 
  cd .. 
fi 

echo "Setting svn ignore properities for databasesm, doc, models..."
svn propset svn:ignore "databases
doc
models" .


