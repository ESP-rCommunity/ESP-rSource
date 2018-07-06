#!/bin/bash
# wrapper for creating files and folders for deb distribution
# 1st parameter is version number e.g. 13.1.1
# 2nd parameter is source folder name
echo "for version $1 from distribution in $2"
echo "mkdir espr_$1"
mkdir espr_$1
echo "rsync --exclude .svn --exclude .git -av ESP-r_Src_V$1/ espr_$1/"
rsync --exclude .svn --exclude .git -av $2/ espr_$1/
echo "tar cf espr_$1.tar espr_$1"
tar cf espr_$1.tar espr_$1
echo "gzip -9 espr_$1.tar"
gzip -9 espr_$1.tar
echo "mkdir to_build_$1"
mkdir to_build_$1
echo "cp espr_$1.tar.gz to_build_$1"
cp espr_$1.tar.gz to_build_$1
echo "cd to_build_$1"
cd to_build_$1
echo "tar xfz espr_$1.tar.gz"
tar xfz espr_$1.tar.gz
echo "ln -s espr_$1.tar.gz espr_$1.orig.tar.gz"
ln -s espr_$1.tar.gz espr_$1.orig.tar.gz
echo "cd espr_$1"
cd espr_$1
echo "debuild"
debuild 
