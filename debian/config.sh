#!/bin/bash
#This script generate 2 header files necessary for compilation 
#If your terminal doesn't speak english, this line is necessary
LANG=C

# Establish if this is a copy from svn or git.  
NotAnSvnWorkingCopy=`svn info 2>&1 | grep "is not a working copy"`
NotAGitRepo=`git status 2>&1 | grep "fatal: Not a git repository"`
 
#echo "> SVN query > ${NotAnSvnWorkingCopy} <" 
#echo "> GIT query > ${NotAGitRepo} <"
 
if [ "${NotAnSvnWorkingCopy}" != "" ] && 
   [ "${NotAGitRepo}"         != "" ] ; then 
   VersioningScheme="none" 
else
  if [ "${NotAnSvnWorkingCopy}" == "" ]; then 
    VersioningScheme="svn"
  else 
    VersioningScheme="git"
  fi 
fi    
 
# Ensure that build information is always recompiled into
# every binary - touch common file version.F.
touch ${SRCdir}/lib/version.F >/dev/null 2>&1

# Time of build
time=`date`

# System information.
architecture=`uname -m`
system=`uname -s`
os_flavour=`uname -r`
host=`uname -n`

# Copy version information to Version text file.
release=`cat ./Version | grep "ESP-r Release " | sed "s/ESP-r Release //g"`
echo " "
echo " Installing targets for ESP-r version ${release}"
echo " "
if [ "${VersioningScheme}" == "svn" ]; then 
    versioned_repository="yes"
    revision_number=`svn info 2>&1 | grep "Revision:"` 
fi
if [ "${VersioningScheme}" == "git" ]; then
    versioned_repository="yes"
fi 

# Write build information to header file.
echo "C      build_info.h"                                              >  ${SRCdir}/src/include/build_info.h
echo "C      "                                                          >> ${SRCdir}/src/include/build_info.h
echo "C      This file contains versioning and build information"       >> ${SRCdir}/src/include/build_info.h
echo "C      for the ESP-r system. It is generated automatically"       >> ${SRCdir}/src/include/build_info.h
echo "C      by the Install script each time ESP-r is recompiled."      >> ${SRCdir}/src/include/build_info.h
echo "C      Changes should be made in the Install scipt and not"       >> ${SRCdir}/src/include/build_info.h
echo "C      not in this file"                                          >> ${SRCdir}/src/include/build_info.h
echo "C      "                                                          >> ${SRCdir}/src/include/build_info.h
echo "C      The following Fortran variable assignments are used"       >> ${SRCdir}/src/include/build_info.h
echo "C      by ESPrVersion() in common/Startup.F"                      >> ${SRCdir}/src/include/build_info.h
echo "       "                                                          >> ${SRCdir}/src/include/build_info.h
echo "       logical  bRelease,bVersioned,bModified"                    >> ${SRCdir}/src/include/build_info.h
echo "       character cBuilder*32       ! User who compiled system"    >> ${SRCdir}/src/include/build_info.h
echo "       character cArchitecture*32  ! Machine architecture"        >> ${SRCdir}/src/include/build_info.h
echo "       character cOperSystem*32    ! Operating system"            >> ${SRCdir}/src/include/build_info.h
echo "       character cOSversion*32     ! Operating system version"    >> ${SRCdir}/src/include/build_info.h
echo "       character cHost*256         ! Host name"                   >> ${SRCdir}/src/include/build_info.h
echo "       character cRelease_Num*24   ! Release version"             >> ${SRCdir}/src/include/build_info.h
echo "       character cSource*32        ! Repository version"          >> ${SRCdir}/src/include/build_info.h
echo "       character cBranch*48        ! Branch name"                 >> ${SRCdir}/src/include/build_info.h
echo "       character cRevision*32      ! Revision number"             >> ${SRCdir}/src/include/build_info.h
echo "       character cState*32         ! Flag for repository state"   >> ${SRCdir}/src/include/build_info.h
echo "       character cCC*32            ! Flags for C compiler"        >> ${SRCdir}/src/include/build_info.h
echo "       character cFC*32            ! Flags for Fortran compiler"  >> ${SRCdir}/src/include/build_info.h
echo "       character cCPL*32           ! Flags for C++ compiler"      >> ${SRCdir}/src/include/build_info.h
echo "       character cXML_support*32   ! XML compile-time options"    >> ${SRCdir}/src/include/build_info.h
echo "       character cSQLite_support*32! SQLite compile-time options" >> ${SRCdir}/src/include/build_info.h
echo "       character cXlibrary*32      ! X11 library location"        >> ${SRCdir}/src/include/build_info.h
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
