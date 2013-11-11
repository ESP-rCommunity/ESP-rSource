PLEASE READ THIS. It provides information that will help you get ESP-r running.

This is a Installer for ESP-r running in the Cygwin emulation environment on a Windows XP, Vista, W7 machine. It assumes that you have already installed Cygwin on your computer. If you have NOT follow the instructions below and then restart this installer!

If you have already installed Cygwin the you can skip the next section (look for Other checks to make...).

Installing Cygwin (if you have not alreay done it):

Have a look at the Cygwin web site at <http://www.cygwin.com>. It contains useful information in addition to the setup.exe application!
 
BEFORE downloading and installing cygwin:
a) Ensure there is an administrative and at least one normal user on your PC. Typically the Administrator user would maintains the cygwin and ESP-r applications. If you work as a different user for normal simulation tasks then it is harder to corrupt databases and test models.
b) Avoid spaces or unprintable characters in user login names.
c) Ensure you are using a LOCALLE that uses a dot for decimal numbers e.g. USA or UK. 
d) If the computer is part of a domain then there are additional commands needed to create entries in the /etc/passwd file and the /etc/group file.
 
If you are going to take the entire cygwin environment you will need ~3.0GB of disk space. If you restrict cygwin to packages needed to develop ESP-r then you will need ~2.0GB of disk space. If you take a minimum cygwin that is sufficient to run ESP-r then you will need ~1.5GB of free space for cygwin. You will need ~700 MB for ESP-r and Radiance.

Point a web browser at <http://www.cygwin.com> and click look for the install options for 32bit or 64bit editions (c:\TEMP is a good place to put it) which will be used to download and install cygwin components. This same tool can also be used to update cygwin.
 
Once downloaded run the setup-x86.exe or setup- x86_64.exeand it will ask the following questions:
a) Where to get cygwin? Choose the option "internet" if this if you have a fast connection and will be installing cygwin on one machine. Choose the "download" option if you want to install cygwin on more than one machine. Choose the "Install from local directory" if you have already downloaded a cygwin distribution or are doing it from a CD.
 
b) Where to put it? Take the suggestion c:\cygwin and also the default option "all users" and "Unix file type". When asked about a "local package directory accept "c:\TEMP" if internet or download, or point to a folder with previously downloaded files if that is the approach you are taking.
  
c) For an internet or download install you may have to set an appropriate proxy.
 
d) Select a download site. 
 
Once the download site has been reached the next task is to select the packages to install. There are essentially three levels depending on how much disk space you want to allocate to cygwin and ESP-r. If disk space is not a problem follow the instructions in section 1.3a. If you want cygwin to support compilation of ESP-r the skip section 1.3b. If you just want to run ESP-r skip to the section 1.3c.
 
Consider what type of window manager you would like to use. It is possible to replicate the GNOME or KDE window managers found on Linux computers if you have lots of disk space and a fast computer. There is  also a minimal approach that allows for ESP-r to run in graphic mode without a window manager e.g. it just pops-up on the normal screen as an application.
 
1.3a If disk space is not a problem - Select all by toggling the "All -> default" to "All -> Install". This could take a few moments for this change to take affect.  It can take 30 minutes to download All and another 40 minutes to install.

1.3b Packages for ESP-r development - If you want to be selective and only download what is required to compile and run ESP-r (and save download time and disk space) the following is a useful guide however also look at the files cygwin32bit_instld_packages.txt or cygwin64bit_packages_installed.txt:
 
  Admin:        tick all (i.e. toggle Install)
  Archive:      default plus p7zip zip unzip xz
  Base:         tick all
  Audio:        default
  Database:     default
  Devel:        If you have space toggle to Install.
                If you want to save some space toggle to default and add gcc4 versions of gcc g++ and gfortran, binutils, file, DDD, gdb, make, subversion
		
  Doc:          optional but at least get the man pages and xpdf
  Editors:      default plus nedit, vim & midnight commander
  Games:        optional
  Gnome:        default plus development versions of atk, gdk-pixbuf, glib2, gtk2-x11-engines, libgnome2, libxslt, libxml2, pango
		 
  Graphics:     default plus ImageMagic, ghostscript, jpeg, libpng, opengl, tiff, netpbm
  KDE:          optional
  Intepretors:  tick expect, gawk, m4, perl 
  Libs:         default plus 
  Mail:         optional
  Math:         optional
  Net:          default plus openssh, ping, inetutils, rsync, rsh, rsh-server.
  Perl:         default
  Publishing:   default plus psutils
  Shells:       pick all
  System:       Install, 
  Text:         default plus enscript, groff, gv,less, more, textinfo
  Utils:        default plus bzip2, clear, cygutils, file, gnupg, keychain, ncurses, patch, time, units
  Web:          default plus wget, cadaver
  X11:          default plus fonts, cygutils-x11, xinit, libX11-dev xorg-server xorg-server-devel xorg-server-extra, xterm, Xaw3D, grace, lesstif, transfig and xfig.
 
 1.3c Packages needed just to run ESP-r: If you want to be selective and only download what is required to RUN ESP-r (and save even more download time and disk space) the following is a useful guide:
 
  Admin:        tick all
  Archive:      tick all
  Base:         toggle to default
  Database:     toggle to default
  Devel:        toggle to default
  Doc:          toggle to default + xpdf
  Editors:      at least nedit, vim & midnight commander
  Games:        optional
  Gnome:        optional
  Graphics:     default plus ImageMagic
  Intepretors:  default plus gawk and expect 
  Libs:         default (double check if ESP-r module complains)
  Mail:         toggle to none
  Math:         default
  Net:          default
  Publishing:   default
  Shells:       Install
  System:       Install 
  Text:         default plus enscript, less, more, textinfo
  Utils:        default
  Web:          default plus wget
  X11:          use the minimal install list from above.

1.4 Post-install patches: 

After completing the list the cygwin installer will download the various packages and put an icon on the desktop. The installer takes some time sorting out the post-install so do not give up!
  
After login into the PC as Administrator (suggested), click on the Cygwin icon on your desktop to start up a cygwin window. This emulates a Linux machine within your PC. Use Linux commands to go to the /tmp folder or create such a folder if it does not exist.
 
Then log in as each user on your Windows computer and start Cygwin. Cygwin will create some `dot` (environment settings) files in your (Cygwin) home folder. If you log in as Ralph you should be located in /home/Ralph after you log in.

Test whether non-administrative users can run graphic applications in Cygwin - as a normal user give the command 'startxwin' in the Cygwin command window. A new window should start in which you can run graphic applications (such as ESP-r). The initial Cygwin command window is only able to invoke text applications and commnands.

If Cygwin is working log out of Cygwin and run this ESP-r Installer again.


Other checks to make before running this installer:

If there is an existing Native Windows version of ESP-r on the computer PLEASE uninstall it using the Windows add/remove applications tool. Then restart this installer.

What this Installer does:

This installer will place a Cygwin version of ESP-r in a standard location (C:\cygwin\opt\esru) on your PC. If this folder does not exist it will be created. 

You should have power user privileges when you run this Installer.  

The following folders will be created:
C:\cygwin\opt\esru
        - esp-r
          - bin (holds two useful scripts link_to link_to_bash)
          - binX11 (ESP-r executables for X11 interface)
          - binGTK (ESP-r executables for GTK interface)
          - databases (corporate databases)
          - electric_loads (measured electrical data)
          - lib  (holds help text for applications)
          - manual (background reading)
          - training (dozens of example models)
          - validation (models used in validation work)
          - xsl (support for XML reporting)
                    
The binX11 or binGTK folders are populated with the two graphic versions of ESP-r.  If you want to run in text mode you need to start the modules with a -mode text in the command line.

A note about the ESP-r model files - use NotePad++ or a native Cygwin text editor to edit (NOT Word or NotePad). 


Adapting your Cygwin environment:

You will need to set an environment variable PATH so that the ESP-r executables are found. There are scripts named link_to and link_to_bash which needs to be run within Cygwin to ensure the executables of the version of ESP-r you want to use are found.
In the Cygwin command window go back to your home folder and create a bin folder (if one does not exist):

  cd
  mkdir bin

Copy the file /opt/esru/bin/link_to into your new bin folder:

  cd
  cd bin
  cp /opt/esru/bin/link_to .

Check and if there is a .cshrc or a .profile file in your home folder:

  cd
  ls -al

Some users of ESP-r prefer to use the "C" shell. The entry in the .cshrc file in your home folder might look something like:

set path = ( $path /usr/local/bin /usr/local/bin/ray /usr/local/lib/ray /home/fred/bin )

If you are running the bash shell look for an entry in the .profile file something like:

export PATH=/usr/local/bin:/home/fred/bin:$PATH

There is an example file /opt/esru/bash_profile

Once you have updated either the .cshrc file or the .profile file then you will need to log out and then back in again.
Next run the link_to script to enable either the X11 or GTK version of ESP-r (you can swap versions by re-running the script.

  cd
  cd bin
  ./link_to /opt/esru/esp-r/binGTK

or

  cd 
  cd bin
  ./link_to /opt/esru/esp-r/binX11

To see if the environment variables are correct give the following command:

  which prj

If the answer back is '~/bin/prj' or '/home/fred/bin/prj' then the environment is set correctly.

Basic machine requirements:

Windows XP or W7 with 900MHz 32-bit processor as a minimum and a 1.8GHz 32-bit processor recommended. 

Memory should be at least 1GB (2GB is better). On some machines increases in RAM will allow for results files to be scanned faster.  

ESP-r is disk-intensive so a faster disk is a good investment. The ESP-r distribution takes up about 500MB of disk space. Depending on the complexity of your models you need to allows for at least another Gb of working space.

Dependencies:

If ESP-r executables do not run it may be because dll (library) files are missing or are not the correct version. There might also be some fonts (lucidia) that you will need to install. Here is a list of what you need for the X11 version of the simulator:
CygX11-6.dll, Cygwin1.dll, Cygxml2-2.dll, Cygxslt-1.dll, Cyggcc-S-1.dll, Cyggfortran-3.dll, Cygstdc++6.dll

Here is a list of what is required for the GTK version of the simulator:
Cygwin1.dll, Cyggdk-X11-2.0.dll, Cyggdk_Pixbuf-2.0-0.dll, Cygglib-2.0-0.dll, Cyggobject-2.0-0.dll, Cyggtk-X11-2.0-0.dll, Cygpango-1.0-0.dll, Cygxml2-2.dll, Cygxslt-1.dll, Cyggcc-S-1.dll, Cyggfortran-3.dll, Cygstdc++6.dll


Building from scratch

If you cannot get ESP-r to run in Cygwin then consider grabbling the source code (see below) and compiling it on your machine. Below is the usual sequence:

  cd
  mkdir Src
  cd Src
  svn checkout https://espr.svn.cvsdude.com/esp-r/branches/development_branch
  cd development_branch
  cd src
  ./Install -d /opt/esru --gcc4 --debug
  (say yes to debug and databases and example models)
  
What else does ESP-r run on? 

There are versions of ESP-r which run Native on a Windows PC as well as OSX (v10.7 & v10.8) and various Linux computers.  

NOTE: ESP-r is less stable on 64-bit computers and operating systems.  Currently ESP-r requires the GNU compiler collection 4.1 or newer.
