
C MFLW2B

C This file contains dummy subroutines which may be used by the
C Makefile of mfs &| plt, in order to meet its needs
C without taking on board the complete counterparts of:
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

C This MFLW2B version is a dummy for incorporation
C in eg. plt's Makefile.
C In its complete version, MFLW2B transfers mass
C flow calculation results to the building side
C of the overall problem configuration.

      SUBROUTINE MFLW2B

      WRITE(*,*) ' MFLW2B NOT supported in current configuration'

      STOP ' MFLW2B: unresolvable error !'

      END
