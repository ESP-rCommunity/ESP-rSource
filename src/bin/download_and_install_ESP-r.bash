#!/bin/bash
#-----------------------------------------------------------
# Detect architecture. In the case of OSX force it to x86_64
# otherwise let uname -m do it.
#-----------------------------------------------------------
architecture=`uname -s`      # computer type e.g. Linux Darwin SunOS
if [ "$architecture" = "Darwin" ]; then
  machine="x86_64"
else
  machine=`uname -m`           # CPU  e.g. i386 x86_64  sun4u
fi
platform="auto-detect"     # default platform

#-----------------------------------------------------------
# If auto-detection is active, interpret architecture
#-----------------------------------------------------------
if [ "$platform" = "auto-detect" ]; then
   # Add more architectures here as they become available.
   # Note: $architecture has been error-trapped above.
   case $architecture in
      Linux)                           platform="linux";;
      CYGWIN_NT-5.2  | CYGWIN_NT-5.1  | CYGWIN_NT-5.0  | CYGWIN_NT-6.0 | CYGWIN_NT-6.1 | CYGWIN_NT-6.1-WOW64 | CYGWIN_NT-10.0) platform="cygwin";;
      MINGW64_NT-10.0 |MINGW64_NT-6.1 |MINGW32_NT-6.1 | MINGW32_NT-5.2 | MINGW32_NT-5.1 | MSYS_NT-6.1 ) platform="mingw";;
      Darwin)                          platform="mac";;
   esac
fi

echo "  ";
echo "This script grabs the cutting-edge ESP-r repositories from Github, assembles";
echo "them for compiling and then, optionally, invokes the ESP-r Install script";
echo "to build ESP-r, its databases, exemplar models and places the distribution";
if [ "$platform" = "linux" ]  ||
   [ "$platform" = "cygwin" ] ; then
   echo "in a standard location on Linux based computers (/opt/esru).";
fi
if [ "$platform" = "mac" ] ; then
   echo "in a standard location on OSX based computers (/opt/esru).";
fi
if [ "$platform" = "mingw" ] ; then
   echo "in a standard location on Windows 7/10 computers (C:/ESRU).";
   echo "when building within MSYS2 command window."; 
fi
echo "  ";
echo "The script will create esru and an esru/tmp folders within the folder";
echo "where it is currently located. The assembled sources will be found in";
echo "a folder ESP-rMaster and the ESP-r distribution (executables, databases,";
echo "exemplars, and documentation) will be placed in /opt/esru and links";
if [ "$platform" = "linux" ]  ||
   [ "$platform" = "cygwin" ] ; then
  echo "to ESP-r executables will be placed in /usr/bin so that";
  echo "they will be easily found by the operating system.";
fi
if [ "$platform" = "mac" ] ; then
  echo "to ESP-r executables will be placed in $HOME/bin so that";
  echo "they will be easily found by the operating system.";
fi
if [ "$platform" = "mingw" ] ; then
   echo "The $PATH environment variable will be updated.";
fi

echo "  ";
echo "The script begins by checking for dependencies (e.g. gcc g++ gfortran";
echo "compilers and a git client). If they are not found you will need to";
echo "install these. You also need to have root permissions to run the script";
echo "(use sudo).";
echo "  ";
echo "The script will replace any existing ESP-r distribution so backup or";
echo "move anything in /opt/esru before you proceed. It will use typical";
echo "assumptions when invoking the ESP-r Install script (you have the";
echo "option to run Install manually). ";
echo "  ";

echo "Proceed with these tasks?";
YN=none;
while [ "$YN" != "y" ] && [ "$YN" != "n" ] && [ "$YN" != "" ]
do
  if [ "$YN" != "none" ]; then
     echo " ";
     echo "Please answer yes or no  (y/n) [y].";
  fi
  read YN
done

if [ "$YN" = "n" ]; then
   # tell user to run Install manually.
   echo "Exiting the script.";
   exit;
fi

if [ "$platform" = "linux" ]  ||
   [ "$platform" = "mac" ] ; then
  echo "Step 0 Are you root?";
  if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root, use sudo ./download_and_install_ESP-r.bash";
    echo "... aborting";
    exit;
  fi
  echo "Step 0 Are you root? ... DONE";
fi
echo "  ";
echo "Step 1 Check for dependencies";

# Check for dependencies
for SWARE in git gcc g++ gfortran rsync make
do
  SWARE_PRESENT=`which $SWARE`;
  if [ -z "$SWARE_PRESENT" ]; then # check if string is not empty
    echo "ERROR: Install $SWARE before continuing ... aborting";
    exit;
  else
    echo "    Using $SWARE present at $SWARE_PRESENT";
  fi
done

if [ "$platform" = "mac" ] ; then
  if [ -d "/opt/local/bin" ]; then
    echo "  ";
    echo "Looks like you are using the MacPorts package manager so you will need to";
    echo "use the --compiler_version option and run the Install script manually.";
    echo "looks like the available GNU compilers are:";
    ls -l /opt/local/bin/gcc-m*
    ls -l /opt/local/bin/g++-m*
    ls -l /opt/local/bin/gfortran-m*
    echo "to find out more about Install command line options do a ./Install -h";
  else
    if [ -d "/usr/local/Homebrew" ]; then
      echo "  ";
      echo "Looks like you are using the Homebrew package manager. If the compilers";
      echo "were found then you will need to use the --compiler_version -6 option when";
      echo "invoking the Install command line.";
    else
      echo "Did not find MacPorts or Homebrew. If you are using something else";
      echo "proceed with caution.";
    fi
  fi
  if [ ! -d "/Applications/Xcode.app" ]; then
    SWARE_PRESENT=`which make`;
    if [ -z "$SWARE_PRESENT" ]; then # check if string is not empty
      echo "ERROR: Install command line tools for XCode before continuing ... aborting";
      exit;
    else
      echo "  Using $SWARE present at $SWARE_PRESENT";
      echo "  So assuming a development tool-chain is present.";
    fi
  fi
fi

echo "Step 1 Check for dependencies ... DONE";
echo "  ";

echo "Step 2 Downloading source and checking out latest...";
# clone git repositories into esru/tmp
if [ ! -d "esru" ]; then
  mkdir esru
fi
if [ ! -d "esru/tmp" ]; then
  mkdir esru/tmp
else
  rm -rf esru/tmp/ESP-r*
fi
echo "  ";
git clone --recursive https://github.com/ESP-rCommunity/ESP-rSource.git esru/tmp/ESP-rSource
echo "  ";
git clone --recursive https://github.com/ESP-rCommunity/ESP-rDatabases.git esru/tmp/ESP-rDatabases
echo "  ";
git clone --recursive https://github.com/ESP-rCommunity/ESP-rModels.git esru/tmp/ESP-rModels
echo "  ";
git clone --recursive https://github.com/ESP-rCommunity/ESP-rDoc.git esru/tmp/ESP-rDoc
echo "Step 2 Downloading source...PASSED";
echo "  ";

# Copy the individual repositories to standard locations for Install script to work
echo "Step 3 Assembling repositories into ESP-rMaster folders...";
if [ ! -d "ESP-rMaster" ]; then
  mkdir ESP-rMaster
else
  rm -rf ESP-rMaster/*
fi
rsync -a esru/tmp/ESP-rSource/ ESP-rMaster/
rsync -a esru/tmp/ESP-rDatabases/ ESP-rMaster/data/
rsync -a esru/tmp/ESP-rDoc/ ESP-rMaster/doc/
rsync -a esru/tmp/ESP-rModels/ ESP-rMaster/models/
rm -rf ESP-rMaster/*/.git*
echo "Step 3 Assembling repositories into ESP-rMaster folders... DONE";
echo "  ";

echo "Step 4 Install ESP-r distribution...";
if [ "$platform" = "linux" ]  ||
   [ "$platform" = "cygwin" ] ||
   [ "$platform" = "mac" ] ; then
  if [ ! -d "/opt" ]; then
    mkdir /opt
  fi
  if [ ! -d "/opt/esru" ]; then
    mkdir /opt/esru
  else
    rm -rf /opt/esru/*
  fi
fi
if [ "$platform" = "mingw" ] ; then
  if [ ! -d "C:/ESRU" ]; then
    mkdir C:/ESRU
    echo "The $PATH environment variable will be updated.";
  else
    rm -rf C:/ESRU/*
  fi
fi

cd ESP-rMaster
# Confirm running Install script.
echo "  ";
echo "The default Install script parameters will be:";
if [ "$platform" = "linux" ]  ||
   [ "$platform" = "cygwin" ] ; then
  echo "./Install_o2 -d /opt/esru --debug --X11 --gcc4 --silent --force";
fi
if [ "$platform" = "mac" ] ; then
  if [ -d "/usr/local/Homebrew" ]; then
    echo "Suggest invoking Install with --compiler_version -6 ";
    echo "./Install_o2 -d /opt/esru --debug --X11 --gcc4 --silent --force --compiler_version -6";
  else
    echo "Suggest invoking Install with --compile_version -mp-5 ";
    echo "./Install_o2 -d /opt/esru --debug --X11 --gcc4 --silent --force --compiler_version -mp-5";
  fi
fi
if [ "$platform" = "mingw" ] ; then
  echo "./Install_o2 -d C:/ESRU --debug --GTK --gcc4";
fi
echo "and all databases and exemplars will be installed.";
echo "To find out more about Install command line options do a ./Install -h";
if [ "$platform" = "mac" ] ; then
  echo "For OSX we suggest manual invocation of the Install script.";
fi
echo "  ";
echo "Proceed with running the ESP-r Install script? A no allows you";
echo "to invoke Install script manually.";
YN=none;
while [ "$YN" != "y" ] && [ "$YN" != "n" ] && [ "$YN" != "" ]
do
  if [ "$YN" != "none" ]; then
     echo "  "
     echo "Please answer yes or no  (y/n) [y]."
  fi
  read YN
done

if [ "$YN" = "n" ]; then
   # tell user to run Install manually.
   echo "Manually run the Install script with your own options e.g.";
   echo "cd ESP-rMaster";
   echo "sudo ./Install -d /opt/esru --debug --gcc4";
   echo "and then manually remove the esru and esru/tmp folders.";
   if [ "$platform" = "mingw" ] ; then
     echo "After compiling you should also run the copy_dll_to_c_esru_esp-r_bin.bash";
     echo "command in the ESPrMaster folder to place dependencies in the";
     echo "C:/ESRU/esp-r/bin folder.";
   fi
   exit;
fi

if [ "$platform" = "linux" ]  ||
   [ "$platform" = "cygwin" ] ; then
  ./Install_o1 -d /opt/esru --debug --X11 --gcc4 --silent --force
fi
if [ "$platform" = "mac" ] ; then
  if [ -d "/usr/local/Homebrew" ]; then
    ./Install_o1 -d /opt/esru --debug --X11 --gcc4 --silent --force --compiler_version -6
  else
    ./Install_o1 -d /opt/esru --debug --X11 --gcc4 --silent --force --compiler_version -mp-5
  fi
fi
if [ "$platform" = "mingw" ] ; then
  echo "For MSYS2 say no to XML output, choose GTK, say yes to databases and models.";
  ./Install_o2 -d C:/ESRU --debug --GTK --gcc4
  echo " ";
  echo "After compiling you should also run the copy_dll_to_c_esru_esp-r_bin.bash";
  echo "command in the ESPrMaster folder to place dependencies in the";
  echo "C:/ESRU/esp-r/bin folder.";
  echo "Do this BEFORE you clean up the folders!";
  echo " ";
fi
echo "  ";
echo "Step 4 Install ESP-r distribution... DONE";
echo "  ";

echo "Step 5 Add executable links in /usr/bin so bash shell can find stuff";
if [ "$platform" = "mac" ] ; then
  if [ ! -d "$HOME/bin" ]; then
    mkdir $HOME/bin
  fi
fi
for executable in aco b2e bps dbm dfs cdb c2e clm e2r ecnv eco enet grd ish mfs mld mrt pdb prj res stats vew
do
  if [ "$platform" = "linux" ]  ||
     [ "$platform" = "cygwin" ] ; then
    ln -s  /opt/esru/esp-r/bin/$executable /usr/bin/$executable
  fi
  if [ "$platform" = "mac" ] ; then
    ln -s  /opt/esru/esp-r/bin/$executable /$HOME/bin/$executable
  fi
done
echo "  ";
echo "Step 6 Cleaning object files from the compile in ESPrMaster ...";
echo "  ";
cd src
make clean

echo "  ";
echo "Step 7 Cleaning downloaded repositories (in esru/tmp) ...";
echo "[say n if you want to do this yourself]";
echo "Proceed with these tasks?";
YN=none;
while [ "$YN" != "y" ] && [ "$YN" != "n" ] && [ "$YN" != "" ]
do
  if [ "$YN" != "none" ]; then
     echo " ";
     echo "Please answer yes or no  (y/n) [y].";
  fi
  read YN
done

if [ "$YN" = "n" ]; then
   # tell user to cleanup manually.
   echo "Manually cleanup the folders esru and esru/tmp";
   exit;
else
# At the end remove temporary files and folders
  cd ..
  rm -rf esru/tmp/*
  rm -rf esru/*
fi


# Check that prj can be found.
PRJ_PRESENT=`which prj`;
if [ -z "$PRJ_PRESENT" ]; then # check if string is not empty
  echo "ERROR: ESP-r Project Manager not yet found - check paths.";
else
  echo "Checking ESP-r Project Manager is present at $PRJ_PRESENT";
  echo "looks like you are good to go!";
fi
