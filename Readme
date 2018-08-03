## ===========================================================
**_Note_: Code is maintained here for legacy support**
**_Please see item 12 below regarding code changes and development_**
Official ESP-r releases are published at: 
http://www.esru.strath.ac.uk/Downloads/downloads.htm#ESP-r
## ===========================================================

#                         ESP-r

##   An integrated building performance simulation program


### 1. ACKNOWLEDGMENTS

ESP-r is developed by an international consortium as listed
at http://www.esru.strath.ac.uk/ and distributed by the
Energy Systems Research Unit at the University of Strathclyde.
Enquiries about the system should be sent to esru@strath.ac.uk.

System development and application is supported by a community
mailing list. To subscribe, please register at
http://lists.strath.ac.uk/mailman/listinfo/esp-r.

### 2. LICENSE

ESP-r is free software made available in the hope that it will
be useful but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

It can be redistributed under the terms of the GNU General Public
License as published by the Free Software Foundation (version 2
or later) and available as part of the ESP-r distribution.

### 3. OBTAINING ESP-r.

The system source files and, separately, a pre-compiled installation
(for Ubuntu 18.04 LTS) are available as downloads from the applications
page at http//www.esru.strath.ac.uk.

Prior to an installation from source code, you should ensure
that a writable directory /opt/esp-r exists (possibly by creating
a link named /opt/esp-r pointing to a directory esp-r elsewhere
that you own.
 
### 4. SYSTEM REQUIREMENTS

ESP-r has been successfully installed on Linux, Cygwin, MSYS2
and MAC OS X systems using different compiler and graphics
library combinations. The native (and preferred) environment
is Linux. Taking the example of a Ubuntu 18.04 LTS computer
the following packages will be required for the 'tool-chain'
and application dependencies needed to build and run ESP-r.

sudo apt-get install build-essential gcc g++ gfortran gdb libtool
sudo apt-get install libx11-dev imagemagick xfonts-100dpi
sudo apt-get install nedit nano xfig transfig xterm csh

### 5. OPTIONAL FEATURES

By default, the ESP-r simulation engine (bps) creates a binary results
database that is post-processed using a separate tool (res). Bps
can optionally export results in XML and CSV formats provided
that the GNU libxml2 library is available. If the GNU libxslt library
is also available, bps can be configured to translate XML results
to a user-specified ASCII format.

### 6. INSTALLATION

Installation of ESP-r from source code is managed by an
'Install' script, which is located in the top level directory
of the source download. A distribution can be installed into any
user-writable directory as defined above. Thereafter, follow
the instructions in the 'Readme' file.

The Install script will guide you through the installation
and can be controlled via command line arguments. For more
information run:

  $ sudo ./Install --help

A complete installation based on the embedded defaults will
result from:

  $ sudo ./Install

while the following command will change the default installation
directory and compiler family:

  $ sudo ./Install -d <installation path> --gcc4

### 7. POST INSTALLATION CONFIGURATION

On most Linux and Cygwin systems, the only post-installation
configuration is to update the PATH environment variable
to include the location of the installed ESP-r binaries.
Run ESP-r by issuing the command 'ESP-r' in a terminal window.

### 8. DOCUMENTATION

Publications describing the theory and operation of ESP-r are
available on the publications page at the above web site, while
other resources (such as coding standards and learning materials)
are available from the downloads page.

### 9. VALIDATION

ESP-r has participated in a large number of validation projects
and includes the ability (activated via the 'validation' option
in prj) to automatically perform the IEA BESTEST/ASHRAE 140
and CEN validation tests.

### 10. QUALITY ASSURANCE AND RELEASE TESTING

Development of ESP-r continues, and its predictions are
subject to change as new features are added and existing
facilities improved. To guarantee the reliability of ESP-r
results, a quality assurance procedure is performed prior
to each release. 

### 11. SUPPORT

The quickest route to a remedy for an ESP-r installation or
application problem is to ask for help on the ESP-r mailing
list as described above.

### 12. MODIFYING THE SOURCE CODE

Under the terms of the GPL lcence you are free to modify the
source code in any way you require for your own use. Should you
wish to make the modified version available to a third party,
you must pass the changes to the central repository. To
help with the quality assurance of updates, please register
your intention to change the source with the ESP-r archivist,
who will work with you to prevent problems arrising from simultaneous
code changes.
