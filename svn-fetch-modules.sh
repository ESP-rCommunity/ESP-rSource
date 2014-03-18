#!/bin/bash

# 

echo " svn-fetch-modules.pl: "
Path=`pwd`
echo " path: ${Path}"

# Arg 1: 'all', '' or a specific module name 

# Get doc sumbmodule and name it 'doc'.

# figure out if this is an SVN repository.



SVNstatus=`svn info  2>&1`



NotAnSvnWorkingCopy=`svn info 2>&1 | grep "is not a working copy"`
NotAGitRepo=`git status 2>&1 | grep "fatal: Not a git repository"`


if [ "${NotAnSvnWorkingCopy}" != "" ] ; then
 echo 
 echo "  -> ERROR: "
 echo "     ${Path} doesn't appear to be versioned via svn."
 
 if [ "${NotAGitRepo}" == "" ] ; then 
 
   echo "     ${Path} appears to be a git repository!" 
   echo "     Run: "
   echo "       git submodule init "
   echo "       git submodule update "
   echo 
   
 fi 
 
 echo "  -> svn-fetch-modules.pl: Quitting.  "
 echo 
 exit  
 
fi




  if [ ! -d ./doc ]; then
    echo "Checking out documentation via svn...-> doc.." 
    svn co https://github.com/ESP-rCommunity/ESP-rDoc/trunk ./doc
     else 
    echo "Refreshing doc..."
    cd ./doc 
    svn update 
    cd .. 
  fi 


  if [ ! -d ./databases ]; then
    echo "Checking out data and climate files via svn...-> databases.." 
    svn co https://github.com/ESP-rCommunity/ESP-rDatabases/trunk ./data

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

  echo "Setting svn ignore properties for databasesm, doc, models..."
  svn propset svn:ignore "data
  doc
  models" .

