C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains a dummy subroutine which is used by the
C Makefile of plt without taking on board the complete counterpart of:
C *** CFMNGE      SUBROUTINE CFMNGE(IC,icfd)
C plt utilizes MFLWCA (esrumfs/mfsbps.F).  MFLWCA now includes a
C call to CFMNGE to accomplish the BSim-CFD momentum conflation.
C However, CFD conflation is not available on the plant side.

C *** CFMNGE
      SUBROUTINE CFMNGE(IC,icfd)
C This CFMNGE version is a dummy for incorporation in plt's Makefile.
C In its complete version, CFMNGE is the manager of the dfs calculations.

      WRITE(*,*) ' CFMNGE: CFD calculation is not supported in this',
     &                   ' plt version.'
 
      STOP ' CFMNGE '

      END
