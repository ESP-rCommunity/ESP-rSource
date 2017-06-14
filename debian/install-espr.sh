#!/bin/bash
# Some files are install(copy) by dh_install tool(see rules and espr.install files)
#This script complete the installation process:
#	1-Create config files for the application
#	2-Create an configured application launcher


#-----------------------------------
# Define variables
#-----------------------------------
#PATH
# see export in rules file

#=====================================================================
# Main script
#=====================================================================
#-----------------------------------
# Write out defaut configuration file
#-----------------------------------
echo -n " Installing esprc and default files"
# Header tag for default file 
echo "*ESP-r Defaults" > $DEBdir/default

# Installation path?
echo "*ipth $ESPdir" >> $DEBdir/default

# Default names for model files (why do they point to explicit paths
# in the training directory, and what about all the files that aren't
# included in this list/
echo "*cfg $ESPdir/training/basic/cfg/bld_basic.cfg" >> $DEBdir/default
echo "*ctl $ESPdir/training/basic/ctl/bld_basic.ctl" >> $DEBdir/default
echo "*mfn ${ESPdir}/training/basic/nets/bld_basic_af1.afn" >> $DEBdir/default
echo "*dfd $ESPdir/training/cfd/template.dfd" >> $DEBdir/default
echo "*pnf $ESPdir/training/plant/vent_simple/cfg/vent.cfg" >> $DEBdir/default

# Are these default names for results files, or pointers
# to example database files?
echo "*res $ESPdir/databases/test.res" >> $DEBdir/default
echo "*mfr $ESPdir/databases/test.mfr" >> $DEBdir/default

# Default database file locations?
echo "*clm $ESPdir/climate/clm67" >> $DEBdir/default
echo "*prs $ESPdir/databases/pressc.db1" >> $DEBdir/default
echo "*prm $ESPdir/databases/material.db4.a" >> $DEBdir/default
echo "*mlc $ESPdir/databases/multicon.db5" >> $DEBdir/default
echo "*opt $ESPdir/databases/optics.db2" >> $DEBdir/default
echo "*evn $ESPdir/databases/profiles.db2.a" >> $DEBdir/default
echo "*pdb $ESPdir/databases/plantc.db1" >> $DEBdir/default
echo "*ecdb $ESPdir/databases/elcomp.db1" >> $DEBdir/default
echo "*mcdb $ESPdir/databases/mscomp.db2" >> $DEBdir/default
echo "*icdb $ESPdir/databases/icons.db1" >> $DEBdir/default
echo "*mldb $ESPdir/databases/mould.db1" >> $DEBdir/default
echo "*sbem $ESPdir/databases/SBEM.db1" >> $DEBdir/default
echo "*cfcdb $$ESPdir/databases/CFClayers.db1.a" >> $DEBdir/default
echo "*end" >> $DEBdir/default
# End of default file.


#-----------------------------------
# Write out esprc configuration file
#-----------------------------------

echo "*ESPRC" > $DEBdir/esprc

echo "*gprn,rectangular dump,import" >> $DEBdir/esprc
echo "*tprn,Text dump,/tmp/tx_dump" >> $DEBdir/esprc
echo "*gxwd,screen dump,import -window root" >> $DEBdir/esprc
echo "*cad,CAD package,xzip,ZIP" >> $DEBdir/esprc
# Image viewer program is platform specific:
# Use display as image editor (and what if it's not available?)
echo "*image_display,TIF,display" >> $DEBdir/esprc
echo "*image_display,XBMP,display" >> $DEBdir/esprc
echo "*image_display,GIF,display" >> $DEBdir/esprc
echo "*image_display,XWD,display" >> $DEBdir/esprc

echo "*journal,OFF" >> $DEBdir/esprc
# Text editor is platform specific:
# Use nedit --- but what if it's not available?
# We could use the value of the EDITOR environment
# variable, but it might point to a command-line
# editor such as vi/nano that wouldn't initialize
# correctly outside a terminal.
echo "*editor,editor,gedit" >> $DEBdir/esprc

echo "*report_gen,Reporting tool,xfs" >> $DEBdir/esprc

# Pointer to exemplar files. Will we install exemplar and validation
# standards?
# Use current installation folder for default databses
TVInstDir="$ESPdir"
echo "*exemplars,Exemplars,${TVInstDir}/training/exemplars" >> $DEBdir/esprc
echo "*validation_stds,Validation standards,${TVInstDir}/validation/stds_list" >> $DEBdir/esprc
# Pointer to defaults file.
echo "*db_defaults,Defaults,$ESPdir/default" >> $DEBdir/esprc

# Pointer to climate list. Depends on whether databases were installed/found.
echo "*db_climates,climatelist,$ESPdir/climate/climatelist" >> $DEBdir/esprc
echo "*end" >> $DEBdir/esprc
# End of file.

#-----------------------------------
# Create an application launcher file
#-----------------------------------
# Makeup an appropriate esp-r script.
echo "#!/bin/sh" > $DEBdir/bin/esp-r
echo "if [ X$EFONT_0 = X ]; then" >> $DEBdir/bin/esp-r
echo "      EFONT_0=6x12" >> $DEBdir/bin/esp-r
echo "      EFONT_1=6x13" >> $DEBdir/bin/esp-r
echo "      EFONT_2=8x13" >> $DEBdir/bin/esp-r
echo "      EFONT_3=9x15" >> $DEBdir/bin/esp-r
echo "fi" >> $DEBdir/bin/esp-r
echo "export PATH=\"$ESPdir/bin:\$PATH\"">> $DEBdir/bin/esp-r
echo "prj \$@ &" >> $DEBdir/bin/esp-r
chmod a+x $DEBdir/bin/esp-r
