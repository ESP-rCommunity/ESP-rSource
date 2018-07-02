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

C This file contains dummy subroutines which are used by the
C Makefile of dfs in order to meet its needs
C without taking on board the complete counterparts of:
C  MFBNDR

C ********************* MFBNDR *********************
C In its complete version, MFBNDR evaluate inlet and outlet conditions
C calculated by Mass Flow Network model.

      SUBROUTINE MFBNDR(intl)

      common/outin/iuout,iuin
      character OUTS*124

      WRITE(OUTS,'(2a)')'MFBNDR: mass flow network calculation is not',
     &                  'supported in CFD only simulations.'
      call edisp(iuout,OUTS)
      call USRMSG(' Please use the integrated simulator.',' ','F')
      STOP
      END
