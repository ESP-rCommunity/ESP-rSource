C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

c *********************************************************************
c *********************************************************************
c HVAC_OUTPUT_FILE

c Created by: Kamel Haddad
c Initial Creation Date: May 1st 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to open and write heading for HVAC hourly output data file
c *********************************************************************
      SUBROUTINE HVAC_OUTPUT_FILE

C Modified by Ian Beausoleil-Morrison 2001-10-05.
C Presence of hvac.res file interupting operation in numerous scenarios
C (e.g. simulating from a directory in which user does not have write
C privileges; performing multiple simulations within single bpsh3k session).
C Therefore, current approach defeated to prevent creation of hvac.res file.
C     open(unit=202,file='hvac.res',status='unknown')

c Heading for the hvac data file
C      write(202,101) 'Month','Day','Hour','System',
C     &'Heating Load','Cooling Load',
C     &'Primary System','Backup System','Fan Energy','Draft Fan',
C     &'Pilot Energy'
C      write(202,102) '(W)','(W)','Energy (J)','Energy (J)','(J)',
C     &'Energy (J)','(J)'

  101 format(a5,1x,a3,2x,a4,1x,a6,1x,a12,6x,a12,6x,a14,4x,
     &a13,7x,a10,9x,a9,10x,a12)
  102 format(23x,a3,15x,a3,15x,a10,8x,a10,10x,a3,16x,a10,9x,a3)

      return
      end
