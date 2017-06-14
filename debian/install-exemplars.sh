#!/bin/bash
# This script complete the installation process:
# It copies the training and validation models but does
# not convert the ascii shading files (catch-22 with db)

#-----------------------------------
# Define variables
#-----------------------------------
#PATH
# see export in rules file
DEBdir=$SRCdir/debian/espr-exemplars$ESPdir
#local binary (not yet used)
CLM=$SRCdir/src/esruclm/clm
PDB=$SRCdir/src/esrupdb/pdb
BPS=$SRCdir/src/esrubps/bps
ISH=$SRCdir/src/esruish/ish
PRJ=$SRCdir/src/esruprj/prj
PDB=$SRCdir/src/esrupdb/pdb
cd ${SRCdir}/models/
printf "Copy exemplar models..."
cp -r training $DEBdir
printf "Copy validation models..."
cp -r validation $DEBdir
find training -name "\.svn" | xargs rm -fr
find training -name "\.git" | xargs rm -fr
find validation -name "\.svn" | xargs rm -fr
find validation -name "\.git" | xargs rm -fr
cfgnames=(`find training validation -name "*.cfg" -print`)
cfgcount=`find training validation -name "*.cfg" -print | wc -l`
printf "  "
printf "Found ${cfgcount} model cfg files."
printf "  "
