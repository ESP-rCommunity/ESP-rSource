#!/bin/bash
echo "Starting apg-get commands needed to setup packages for"
echo "ESP-r compile and use on Ubuntu 11.04."
echo "  "
echo "You will be asked for your password. You may be asked"
echo "to confirm some details during the install."
echo "When scripts finished check if there were any package-not-found "
echo "warnings. If so use apt-get manually for those packages. "
echo "  "
echo "Doing install of ...gcc g++ gfortran gdb gettext libtool..."
sudo apt-get install gcc g++ gfortran gdb gettext libtool
echo "doing... build-essential console-tools-dev expect-dev..."
sudo apt-get install build-essential console-tools-dev expect-dev
echo "doing ... subversion subversion-tools..."
sudo apt-get install subversion subversion-tools
echo "doing... libxml2 libxml2-dev libxml2-utils..."
sudo apt-get install libxml2 libxml2-dev libxml2-utils
echo "doing ... libxslt1-dev libxslt1-dbg..."
sudo apt-get install libxslt1-dev libxslt1-dbg
echo "doing ... netpbm imagemagick gimp gimp-data..."
sudo apt-get install netpbm imagemagick gimp gimp-data
echo "doing ... libx11-6-dbg libx11-dev..."
sudo apt-get install libx11-6-dbg libx11-dev
echo "doing ... openssh-server ssh csh..."
sudo apt-get install openssh-server ssh csh
echo "doing ... enscript psutils nedit..."
sudo apt-get install enscript psutils nedit
echo "doing ... libgtk2.0 libgtk2.0-0-dbg libgtk2.0-dev libfontconfig1-dbg libcairo2-dbg libpixman-1-0-dbg lixslt1-dbg libgtk2.0-0-dbg..."
sudo apt-get install libgtk2.0 libgtk2.0-0-dbg libgtk2.0-dev libgtk2.0-0-dbg
sudo apt-get install libfontconfig1-dbg libcairo2-dbg libpixman-1-0-dbg lixslt1-dbg
echo "doing ... xfonts-75dpi xfs font-manager..."
sudo apt-get install xfonts-75dpi xfs font-manager
echo "doing ... xfonts-terminus-dos xfonts-terminus xfonts-mplus ttf-dejavu..."
sudo apt-get install xfonts-terminus-dos xfonts-terminus xfonts-mplus ttf-dejavu
sudo apt-get install gsfonts-x11 t1-xfree86-nonfree ttf-xfree86-nonfree
echo "doing ... xfig transfig..."
sudo apt-get install xfig transfig
echo "  "
echo "You might need some additional fonts...or log out and in to ensure fonts are known."
