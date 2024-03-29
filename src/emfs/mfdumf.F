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

C This file contains dummy subroutines which may be used by the
C Makefile of mfs, in order to meet its needs
C without taking on board the complete counterparts of the simulator:

C Contains: MZMIXT

C *** MZMIXT ***********************************************************
C This MZMIXT version is a dummy for incorporation in eg. mfs's Makefile.
C In its complete version, MZMIXT calculate the zone weighted zone 
C temperature.

      SUBROUTINE MZMIXT(IFS2,TMRT,TMIX)

      WRITE(*,*) ' MZMIXT: sensor location index -2 NOT supported',
     &                   ' in the current configuration.'

      STOP ' MZMIXT: it cannot sense a mix of zone air temperature'
    
      END
