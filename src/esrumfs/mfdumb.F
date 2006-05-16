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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C MFDUMB of ESRUmfs #####################################################

C This file contains dummy subroutines which may be used by the
C Makefile's of mfs &| bld, in order to meet their needs
C without taking on board the complete counterparts of:
C
C     MFLW2P MFCNTL

C *** MFLW2P ***********************************************************
C This MFLW2P version is a dummy for incorporation in eg. bld's Makefile.
C In its complete version, MFLW2P transfers mass flow calculation results
C to the plant side of the overall problem configuration.

      SUBROUTINE MFLW2P

      WRITE(*,*) ' MFLW2P NOT supported in current configuration'

      STOP ' MFLW2P: unresolvable error !'

      END

C *** MFCNTL ***********************************************************
C This MFCNTL version is a dummy for incorporation in eg. mfs's Makefile.
C In its complete version, MFCNTL finds the sensed value as defined by
C the sensor location etc. in a plant control loop.

      SUBROUTINE MFCNTL(SVCTL)

      WRITE(*,*) ' MFCNTL: sensed property index 0 NOT supported',
     &                   ' in stand-alone version of ESRUmfs !'

      STOP ' MFCNTL: unresolvable error !'

      END
