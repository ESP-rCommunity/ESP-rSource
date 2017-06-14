#!/bin/bash
#This script generate 2 header files necessary for compilation 
#If your terminal doesn't speak english, this line is necessary
LANG=C
#-----------------------------------
# Define variables
#-----------------------------------
# see export in rules file
#-----------------------------------
# Retreive informations about version
#-----------------------------------
echo " Try to collect svn information"
# Get time of build 
time=`date`

# Get system information. (host not used at present)
architecture=`uname -m`
system=`uname -s`
os_flavour=`uname -r`
host=`uname -n`


# Digest version information in Version text file
release=`cat Version | grep "ESP-r release " | sed "s/ESP-r release //g"`


# Try to collect svn information from debian archive
svn_info_command="svn info"
svn_status_command="svn status"
#svn info are in a file text
#svn_info_command="cat svn_info.txt"
#svn_status_command="cat svn_status.txt"

if [ "$unversioned" == "no" ]; then
   svnrevision=`$svn_info_command 2>&1 | grep "^Revision:"`
   svnnotfound=`$svn_info_command 2>&1 | grep "command not found"`
   notworkingcopy=`$svn_info_command 2>&1 | grep "is not a working copy"`
   URL=`$svn_info_command 2>&1 | grep "^URL" | sed "s/URL: //g"`
   URLcvsdude=` echo $URL | grep "https*://espr.svn[0-9]*.cvsdude.com/esp-r/"`
   URLesprcentral=` echo $URL | grep "https*://[a-zA-Z0-9]*@*esp-r.net/esp-r/"`
 else
   svnnotfound="yes"
 fi

 
 # Ensure that the URL is from ESP-r central. Code built from
 # other repositories is to be designated 'unversioned'

 if [ "$unversioned"    == "yes" ]
    [ "$svnnotfound"    != "" ]    ||
    [ "$notworkingcopy" != "" ] ||
    [ "$svnrevision"    ==  "" ] ;   then
   versioned_repository="no";
 else
   if [ "$URLcvsdude" != "" ] || [ "$URLesprcentral"  != "" ]; then
     versioned_repository="yes"
   else
     versioned_repository="no"
   fi
 fi
 

 # If svn is available and repository is under versioning,
 # get misc svn information
 if [ "$versioned_repository" = "yes" ]; then
   revision_number=`$svn_info_command 2>&1 | grep "^Revision" | sed "s/Revision: //g"`

   # Get branch name: Strip ESP-r.net/espr/esp-r portion of url
   branch_name=`echo $URL | sed "s/^.*\/esp-r\///g"`

   # Strip leading 'branches', if any
   branch_name=`echo $branch_name | sed "s/branches\///g"`
   # Strip trailing 'src'
   branch_name=`echo $branch_name | sed "s/\/src//g"`

   # Now determine if URL describes development branch, a sub-branch,
   # or the trunk

   # Default version to sub-branch...
   source="Sub-branch"

   if [ "$branch_name" = "trunk" ]; then
     source="Trunk"
   fi

   if [ "$branch_name" = "development_branch" ]; then
     source="Development branch"
   fi

   # Use svn status to deterine if repository has
   # been locally modified (denoted by M/G/C status)
   #
   svn_status=`$svn_status_command 2>&1 | grep "^[MGC]" `
   if [ "$svn_status" != "" ]; then
    state="Locally modified"
   else
    state="Vanilla"
   fi

   echo " Done."
 else
   echo " Unavailable!"
 fi
#-----------------------------------
# Write build_info.h
#-----------------------------------
  echo "C      build_info.h"                                              >  ${SRCdir}/src/include/build_info.h
  echo "C      "                                                          >> ${SRCdir}/src/include/build_info.h
  echo "C      This file contains versioning and build information"       >> ${SRCdir}/src/include/build_info.h
  echo "C      for the ESP-r system. It is generated automatically"       >> ${SRCdir}/src/include/build_info.h
  echo "C      by the Install script each time ESP-r is recompiled---"    >> ${SRCdir}/src/include/build_info.h
  echo "C      any changes should be made in the Install scipt, and "     >> ${SRCdir}/src/include/build_info.h
  echo "C      not in this file"                                          >> ${SRCdir}/src/include/build_info.h
  echo "C      "                                                          >> ${SRCdir}/src/include/build_info.h
  echo "C      The following fortran variable assignments are "           >> ${SRCdir}/src/include/build_info.h
  echo "C      digested by procedure ESPrVersion() in esrucom/Startup.F"  >> ${SRCdir}/src/include/build_info.h
  echo "       "                                                          >> ${SRCdir}/src/include/build_info.h
  echo "       logical  bRelease,bVersioned,bModified"                    >> ${SRCdir}/src/include/build_info.h
  echo "       character cBuilder*32       ! User who compiled system"    >> ${SRCdir}/src/include/build_info.h
  echo "       character cArchitecture*32  ! Machine archictecture"       >> ${SRCdir}/src/include/build_info.h
  echo "       character cOperSystem*32    ! Operating system"            >> ${SRCdir}/src/include/build_info.h
  echo "       character cOSversion*32     ! OS flavour/version"          >> ${SRCdir}/src/include/build_info.h
  echo "       character cHost*256         ! Host name"                   >> ${SRCdir}/src/include/build_info.h
  echo "       character cRelease_Num*8    ! Release version"             >> ${SRCdir}/src/include/build_info.h
  echo "       character cSource*32        ! Repository version"          >> ${SRCdir}/src/include/build_info.h
  echo "       character cBranch*48        ! Branch name"                 >> ${SRCdir}/src/include/build_info.h
  echo "       character cRevision*32      ! Revision number"             >> ${SRCdir}/src/include/build_info.h
  echo "       character cState*32         ! Flag for repository state."  >> ${SRCdir}/src/include/build_info.h
  echo "       character cCC*32            ! Flags for compilers."        >> ${SRCdir}/src/include/build_info.h
  echo "       character cFC*32            ! fortran"                     >> ${SRCdir}/src/include/build_info.h
  echo "       character cCPL*32           ! C++"                         >> ${SRCdir}/src/include/build_info.h
  echo "       character cXML_support*32   ! Compile-time options"        >> ${SRCdir}/src/include/build_info.h
  echo "       character cSQLite_support*32! Compile-time options"        >> ${SRCdir}/src/include/build_info.h
  echo "       character cXlibrary*32      ! where which"                 >> ${SRCdir}/src/include/build_info.h
  echo "       "                                                          >> ${SRCdir}/src/include/build_info.h
  echo "       cBuilder          = \"${USER}\" "                          >> ${SRCdir}/src/include/build_info.h
  echo "       cHost             = \"${host}\""                           >> ${SRCdir}/src/include/build_info.h
  echo "       cArchitecture     = \"${architecture}\""                   >> ${SRCdir}/src/include/build_info.h
  echo "       cOperSystem       = \"${system}\""                         >> ${SRCdir}/src/include/build_info.h
  echo "       cOSversion        = \"${os_flavour}\""                     >> ${SRCdir}/src/include/build_info.h

  # Set flag indicating if this is a 'release' build
  # A release MUST be built from a versioned copy
  # of TRUNK in a Vanilla state.
  if [ "$versioned_repository" = "yes" ] &&
     [ "$source" = "Trunk"             ] &&
     [ "$state"  = "Vanilla"           ];  then
    echo "       bRelease          = .TRUE."                              >> ${SRCdir}/src/include/build_info.h
  else
    echo "       bRelease          = .FALSE."                             >> ${SRCdir}/src/include/build_info.h
  fi

  echo "       cRelease_num      = \"${release}\""                        >> ${SRCdir}/src/include/build_info.h

  if [ "${versioned_repository}" = "yes" ]; then
    echo "       bVersioned        = .TRUE. "                             >> ${SRCdir}/src/include/build_info.h
  else
    echo "       bVersioned        = .FALSE. "                            >> ${SRCdir}/src/include/build_info.h
  fi

  if [ "$state" = "Vanilla" ]; then
    echo "       bModified         = .FALSE."                             >> ${SRCdir}/src/include/build_info.h
  else
    echo "       bModified         = .TRUE."                              >> ${SRCdir}/src/include/build_info.h
  fi

  echo "C      URL commented out, as it likely exceeds g77's  "           >> ${SRCdir}/src/include/build_info.h
  echo "C      72 character-per-line limit."                              >> ${SRCdir}/src/include/build_info.h

  echo "C      cURL              = \"${URL}\""                            >> ${SRCdir}/src/include/build_info.h
  echo "       cSource           = \"$source\""                           >> ${SRCdir}/src/include/build_info.h
  echo "       cBranch           = \"$branch_name\" "                     >> ${SRCdir}/src/include/build_info.h
  echo "       cRevision         = \"$revision_number\""                  >> ${SRCdir}/src/include/build_info.h
  echo "       cState            = \"${state}\""                          >> ${SRCdir}/src/include/build_info.h
  # Add compilers, x-libraries & xml/xsl status
  echo "       cCC               = \"$MCC\""                               >> ${SRCdir}/src/include/build_info.h
  echo "       cFC               = \"$MFC\""                               >> ${SRCdir}/src/include/build_info.h
  echo "       cCPL              = \"$MCPL\""                              >> ${SRCdir}/src/include/build_info.h
  if [ "$xml_support" = "yes" ]; then
    echo "       cXML_support      = \"supported\""                       >> ${SRCdir}/src/include/build_info.h
  else
    echo "       cXML_support      = \"unsupported\""                     >> ${SRCdir}/src/include/build_info.h
  fi
  if [ "$SQLite_support" = "yes" ]; then
    echo "       cSQlite_support   = \"supported\""                       >> ${SRCdir}/src/include/build_info.h
  else
    echo "       cSQLite_support   = \"unsupported\""                     >> ${SRCdir}/src/include/build_info.h
  fi
  echo "       cXlibrary         = \"$xLibs\" "                           >> ${SRCdir}/src/include/build_info.h
#-----------------------------------
# Write installdir.h
#-----------------------------------
####### create an installdir.h file so startup.F knows where to look
  echo "c this is where esp-r is installed" > ${SRCdir}/src/include/espinstalldir.h
  echo "       character instpath*60" >> ${SRCdir}/src/include/espinstalldir.h
  echo "       instpath = " >> ${SRCdir}/src/include/espinstalldir.h
  echo "     & '${ESPdir}'" >> ${SRCdir}/src/include/espinstalldir.h
  exit 0
