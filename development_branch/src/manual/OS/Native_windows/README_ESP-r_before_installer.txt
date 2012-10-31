What is in this installer...

This installer will place a native Windows version of ESP-r in a standard location (C:\Esru) on your PC.  You should have power user privileges when you run setup.  

The setup places ESP-r in a folder C:\Esru (created if it does not exist). You should also create a C:\TEMP folder if one does not exist.

Setup attempts to update the system 'path' environment variables so that the various ESP-r modules can be found.

The following folders will be created:
C:\Esru
        - esp-r
          - bin (ESP-r executables)
          - bin_graphic (graphic version of ESP-r)
          - bin_text (dos command line version of ESP-r)
          - climate (climate files from various locations)
          - databases (corporate databases)
          - electric_loads (measured electrical data)
          - lib  (currently empty)
          - manual (background reading)
          - training (dozens of example models)
          - validation (models used in validation work)
          - xsl (support for XML reporting)
                    
The bin folder initially is populated with the graphic version of ESP-r. If you want to use the text based version (for automation) then copy the *.exe files in bin_text into bin.

A note about the ESP-r model files - use WordPad or NotePad++ to edit (NOT Word or NotePad). 
 
The text editors gvim62.exe or notepad++ are compatible with ESP-r which you might find useful if you are likely to want to move models from Windows to Linux or OSX machines.

Basic machine requirements:

Windows XP or Vista with 900MHz 32-bit processor as a minimum and a 1.8GHz 32-bit processor recommended. 

Memory should be at least 512Mb (1GB is better). On some machines increases in RAM will allow for results files to be scanned faster.  

ESP-r is disk-intensive so a faster disk is a good investment. The ESP-r distribution takes up about 350MB of disk space. Depending on the complexity of your models you need to allows for at least another Gb of working space. 

What else does ESP-r run on? 

There are versions of ESP-r which run on a Windows PC under the Cygwin emulation environment (some people prefer this option as it give access to an extended feature set as well (as the ability to invoke Radiance for visual simulations).  

ESP-r compiles and runs on just about every Linux and Unix distribution.  NOTE: ESP-r is less stable on 64-bit computers and operating systems.  Currently ESP-r requires the GNU compiler collection 4.1 or newer.

ESP-r compiles and runs on Intel based Mac OSX computers (v10.5 or newer).
