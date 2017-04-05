#!/bin/csh
echo "Script to be run after import_geometryAndAttribution_fromEplusModel.py"
echo "and before associate_geometryAndDatabases_fromEplusModel.py. It"
echo "creates ESP-r folder structure as well as boilerplate cfg file. It"
echo "is passed the name of the IDF file and the root name of the ESP-r model."
echo "first importing databases from the idf file $1"
echo "  "
echo "The method has been tested on a variety of EPP 8.5 models and the"
echo " following caviats apply: "
echo "a) only the initial 30 characters of material & construction names used"
echo "b) zone & surface names are truncated to 12 characters "
echo "c) only material, construction and geometry tokens are parsed."
echo "d) glazing optics are not parsed - you have to set these."
echo "e) an ESP-r folder structure is created. You will have to re-name"
echo "   the mode cfg & cnn files, set location year etc."
echo "f) zone offsets in IDF files are not yet implemented."
echo "  "
set x="y"
echo "Proceed (y/n) ?"
set x = {$<}
if ( $x == "n") then
  echo " "
  echo "Exiting with no conversion."
  echo " "
  exit
endif
echo "starting conversion..." 
./import_materialsAndConstructions_fromEplusModel.py $1
echo "now importing geometric information from the idf file."
./import_geometryAndAttribution_fromEplusModel.py $1
echo "Now creating the ESP-r model folders..."
mkdir $2
foreach j ( cfg dbs ctl doc images nets rad zones )
  echo "creating $2/$j"
  mkdir $2/$j
end
foreach j ( `ls *.geo ` )
  echo "$j"
  echo "moving $j"
  mv "$j" $2/zones/$j
end
foreach j ( `ls *.constrdb ` )
  echo "moving constructions $j"
  mv "$j" $2/dbs/$j
end
foreach j ( `ls *.materialdb ` )
  echo "moving $j"
  mv "$j" $2/dbs/$j
end
echo "moving connections file..."
mv Eplus_imported.cnn $2/cfg/Eplus_imported.cnn
echo " "
echo "creating dummy cfg file..."
echo "* CONFIGURATION4.0" > $2.cfg
echo "*date Fri Feb 24 16:20:23 2017" >> $2.cfg
echo "*root $2" >> $2.cfg
echo "*zonpth  ../zones   # path to zones" >> $2.cfg
echo "*netpth  ../nets   # path to networks" >> $2.cfg
echo "*ctlpth  ../ctl   # path to controls" >> $2.cfg
echo "*aimpth ../aim2   # path to aim2 files" >> $2.cfg
echo "*radpth ../rad   # path to radiance files" >> $2.cfg
echo "*imgpth  ../images    # path to project images" >> $2.cfg
echo "*docpth  ../doc    # path to project documents" >> $2.cfg
echo "*dbspth  ../dbs    # path to local databases" >> $2.cfg
echo "*hvacpth ../hvac    # path to hvac files" >> $2.cfg
echo "*bsmpth ../bsm   # path to BASESIMP files" >> $2.cfg
echo "*radcore  1  # number of cores available to Radiance" >> $2.cfg
echo "*indx    0 # Project registration" >> $2.cfg
echo " 55.900   -4.100   # Latitude & Longitude (diff from time meridian)" >> $2.cfg
echo "      1   0.200   # Site exposure & ground reflectivity" >> $2.cfg
echo "* DATABASES" >> $2.cfg
echo "*stdmat  material.db4.a" >> $2.cfg
echo "*stdcfcdb  CFClayers.db1.a" >> $2.cfg
echo "*stdmlc  multicon.db5" >> $2.cfg
echo "*stdopt  optics.db2" >> $2.cfg
echo "*stdprs  pressc.db1" >> $2.cfg
echo "*stdevn  profiles.db2.a" >> $2.cfg
echo "*stdclm  clm67" >> $2.cfg
echo "*stdmscldb  mscomp.db1" >> $2.cfg
echo "*stdmould  mould.db1" >> $2.cfg
echo "*stdpdb  plantc.db1" >> $2.cfg
echo "*stdsbem  SBEM.db1" >> $2.cfg
echo "*stdpredef  predefined.db1" >> $2.cfg
echo "*slr_half_hr   0  # solar timing hour centred" >> $2.cfg
echo "*quick_run   0  # no" >> $2.cfg
echo "*calename standard weekday Sat Sun hol" >> $2.cfg
echo "*calentag weekdays, weekdays (all year), 260" >> $2.cfg
echo "*calentag saturday, Saturdays (all year),  52" >> $2.cfg
echo "*calentag sunday, Sundays (all year),  52" >> $2.cfg
echo "*calentag holiday, holiday,   1" >> $2.cfg
echo "*list    4" >> $2.cfg
echo " 4,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1," >> $2.cfg
echo " 2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1," >> $2.cfg
echo " 1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1," >> $2.cfg
echo " 1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2," >> $2.cfg
echo " 3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1," >> $2.cfg
echo " 1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1, " >> $2.cfg
echo " 1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3," >> $2.cfg
echo " 1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1" >> $2.cfg
echo "*end_list" >> $2.cfg
echo "*year  2007 # assessment year" >> $2.cfg
echo "*water_in_zones 0.6  998.2 4190.0    0.0 # water cond density spec ht shortwave abs" >> $2.cfg
echo "* PROJ LOG" >> $2.cfg
echo "../doc/$2.log" >> $2.cfg
echo "auto conversion from EPP IDF file" >> $2.cfg
mv $2.cfg $2/cfg/$2.cfg
echo "adding geometry files and databases into the model cfg file."
cd $2/cfg/
../../associate_geometryAndDatabases_fromEplusModel.py $2.cfg
cd ../..
echo "done."

