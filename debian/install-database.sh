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
#local binary
CLM=$SRCdir/src/esruclm/clm
PDB=$SRCdir/src/esrupdb/pdb

#=====================================================================
clim_convert()  # converts climate dbs from iwec.a to binary formats
#=====================================================================
# Arg_1 = target file 
# Arg_2 = source file
{


# short=`echo $1 | sed 's/^.*\///g'`
# city=$short
# country=$short
# country=`echo $short | sed 's/_.*$//g;'`
# city=`echo $short | sed 's/_iwec//g;'`
# city=`echo $city | sed 's/.iwec//g;'`
# city=`echo $city | sed 's/_cwec//g;'`
# city=`echo $city | sed 's/.cwec//g;'`
# city=`echo $city | sed 's/^.*_//g;'`

# printf "\r   - Creating binary climate databases [$city ($country)]..."

# sed-less version

TargetName=`echo $2 |  sed 's/\.a$//g'`
printf "\r                                                                            "
printf "\r   - Creating binary climate databases [${TargetName}]..."

rm -f $1
$CLM -mode text -file $1 -act asci2bin silent $2  >/dev/null 2>/dev/null


}
#=====================================================================
pdb_convert()  # converts plant dbs from iwec.a to binary formats
#=====================================================================
{
SourceName=$1
TargetName=$2
TargetFolder=$3
printf "\r                                                                            "
printf "\r   - Creating binary plant databases [${TargetName}]..."

$PDB -mode text -file $TargetName -act asci2bin ${SourceName}
mv $TargetName $TargetFolder

}



#-----------------------------------
# Create binary files for database
#-----------------------------------

cd ${SRCdir}/data/databases
echo -n "   - Creating binary plant components databases..."
  # pdb convert: [ source file ] [ dest. file ] [ dest folder ]
  pdb_convert plantc.db1.a plantc.db1 $DEBdir/databases
  pdb_convert plantc.db2.a plantc.db2 $DEBdir/databases
  pdb_convert plantc.tt.a  plantc.tt  $DEBdir/databases
echo -n "   - Creating binary climate databases..."
cd ${SRCdir}/data/climate
sed -e 's|/usr/esru/esp-r|'"${ESPdir}"'|' $SRCdir/data/climate/climatelist > $DEBdir/climate/climatelist   
cp README $DEBdir/climate/Readme
cp README_IWEC $DEBdir/climate/Readme_iwec
#Find all assci files (we use the convention of .a)
for file in *.a
do
  #Assign asscii file to variable.
  CLIM_a=${file}
  #Create binary file name by cutting off the .a suffix.
  CLIM_b=`echo $file |  sed 's/\.a$//g'`
  #Convert.
  clim_convert $DEBdir/climate/${CLIM_b} ${CLIM_a}
done

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
