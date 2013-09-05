C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2007.

C espriou.h holds common block for file units and file I/O.

C currentfile (144 char) remembers the name of the file currently being read.
      common/curfile/currentfile
      character currentfile*144

C Where list of climate files is held.
      common/clmlst/cdblbl,cdblfil
      character cdblbl*20,cdblfil*144

C Where default db list (as setup by Install script are kept.
      common/defdb/dfdblbl,defdbfl
      character dfdblbl*20,defdbfl*144

C Where exemplars list is kept.
      common/exemlst/exemlbl,exemfl
      character exemlbl*20,exemfl*144

C Where esp-r environment variables at initial scan are held.
      common/udot/esprc
      character esprc*144

C For external help text facility these variables give focus
C to the file and the section of the help text blocks.
      character helpinapp*24  ! application name set once in MAIN
      common/helpin/helpinapp

C parameters for browsing files (e.g. as used in esrucom/filelist.F)
C MFFOLD - (integer) number of files remembered in a folder. Changes
C to this parameter should also be reflected in the size of the
C buffers and loop limits in lib/esru_nox.c and lib/esru_x.c functions
C getfilelist_.
      integer MFFOLD
      parameter (MFFOLD=100)

C end of espriou.h
