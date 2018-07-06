#!/bin/bash
# This script complete the installation process:
# It copies the training and validation models but does
# not yet convert the ascii shading files (catch-22 with db
# which causes ish to fault. If ish can be revised to ignore
# databases then the logic below should work ok.

#-----------------------------------
# Define variables
#-----------------------------------
#PATH
# see export in rules file
DEBdir=$SRCdir/debian/espr-exemplars$ESPdir
#local binary
CLM=$SRCdir/src/eclm/clm
PDB=$SRCdir/src/epdb/pdb
BPS=$SRCdir/src/ebps/bps
ISH=$SRCdir/src/eish/ish
PRJ=$SRCdir/src/eprj/prj
PDB=$SRCdir/src/epdb/pdb
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
printf "...."
printf "Found ${cfgcount} model cfg files."
printf "...."
cfgnum=0
for file in "${cfgnames[@]}"; do
  cfgnum=$((${cfgnum} + 1))
  printf "      -> model ${cfgnum}/${cfgcount} (${file})"
  isi=(`egrep '(\*isi\ |\*zon\ )' $file | cut -d# -f1`)
  ish_tag_count=(`egrep '(\*isi\ )' $file | cut -d# -f1 | wc -l `)
  hits=${#isi[@]}
  i=0
  ish_processed_count=0
  while [ $((i)) -lt $((hits-2)) ]; do
  if [ ${isi[$((i))]} == "*zon" ]; then
    if [ ${isi[$((i+2))]} == "*isi" ]; then
      ish_processed_count=$((${ish_processed_count}+1))
      printf "\r      -> model ${cfgnum}/${cfgcount} (${file}); zone ${ish_processed_count}/${ish_tag_count}"
      printf "\r $ISH -mode text -file ${file} -zone ${isi[$((i+1))]} -act useupdate_silent"
      $ISH -mode text -file ${file} -zone ${isi[$((i+1))]} -act useupdate_silent > /dev/null
      shdfile=${file%/*}/${isi[$((i+3))]}
      mv -f ${shdfile} $DEBdir/${shdfile}
    fi
  fi
  i=2+$(($i))
  done
done
