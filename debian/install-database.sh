#!/bin/bash
# Some files are install(copy) by dh_install tool(see rules and espr.install files)
#This script complete the installation process:
#	3-Create binary files for databases

#-----------------------------------
# Define variables
#-----------------------------------
#PATH
# see export in rules file
DEBdir=$SRCdir/debian/espr-database$ESPdir
DATdir="${SRCdir}/data"	# Source data directory.
#local binary
CLM=$SRCdir/src/eclm/clm
PDB=$SRCdir/src/epdb/pdb

# Weather.
# Define a function to convert the climate database format from ascii to binary.
clim_convert()  # converts climate dbs from iwec.a to binary formats
{
TargetName=`echo $2 |  sed 's/\.a$//g'`
printf "\r    - installing weather databases [${TargetName}] ..."
rm -f $1
# $CLM -mode text -file $1 -act asci2bin silent $2  >/dev/null 2>/dev/null
$CLM -mode text -file $1 -act asci2bin silent $2
}
echo -n "    - installing weather databases ..."
cd ${DATdir}/climate
cp climatelist $DEBdir/climate
  sed -e 's|/opt/esp-r|'"${ESPdir}"'|' ${DATdir}/climate/climatelist > $DEBdir/climate/climatelist   
cp README $DEBdir/climate/Readme
cp README_IWEC $DEBdir/climate/Readme_iwec
for file in *.a
do
  CLIM_a=${file}
  CLIM_b=`echo $file |  sed 's/\.a$//g'`
  clim_convert $DEBdir/climate/${CLIM_b} ${CLIM_a}
  mv ${DATdir}/climate/${CLIM_b} $DEBdir/climate/
done
#=====================================================================
pdb_convert()  # converts plant dbs from iwec.a to binary formats
#=====================================================================
{
SourceName=$1
TargetName=$2
TargetFolder=$3
printf "\r    - installing plant databases [${TargetName}] ..."
$PDB -mode text -file $TargetName -act asci2bin ${SourceName}
mv $TargetName $TargetFolder
}
cd ${DATdir}/databases
echo -n "   - Creating binary plant components databases..."
  # pdb convert: [ source file ] [ dest. file ] [ dest folder ]
  pdb_convert plantc.db1.a plantc.db1 $DEBdir/databases
  pdb_convert plantc.db2.a plantc.db2 $DEBdir/databases
  pdb_convert plantc.tt.a  plantc.tt  $DEBdir/databases
  

# Sample result set.
echo -n "   - Creating example binary results database..."
rm -f ${ESPdir}/databases/test.res
$BPS -mode text >/dev/null 2>&1 <<zzz
a
d
y
c
${ESPdir}/databases/test.res
11 7
17 7
1
1
s

example_results
y
y
-
-
zzz
    echo " Done."
