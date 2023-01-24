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

C ******
C This file contains the following subroutines:
C   MValid

C ****** MValid
C The main menu for self-testing.
      subroutine MValid
#include "help.h"

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/GR1D04/GAM,RGAM
      common/Vld04/DTime,NTime,MonNod,NRoot,NIter
      common/Vld10/VlDirR,VlDirL
      common/FILEP/IFIL

      character*25 item(8)
      character VlDirR*96,VlDirL*72
      integer MITEM,INO ! max items and current menu item

C  VlDirR='/usr/esru/esp-r/validation'
      DTime=3600.0
      NTime=24
      MonNod=1
      NRoot=500
      NIter=1000
      Gam=0.5

   10 INO=-3
      ITEM(1)='a conduction'
      ITEM(2)='b BESTEST'
      ITEM(3)='? help'
      ITEM(4)='- exit menu'
      MITEM=4

      H(1)='This menu gives access to self-testing corresponding to:'
      H(2)='  a) an analytical solution of the transient conduction'
      H(3)='     within a multi-layered construction, and'
      H(4)='  b) BESTEST (ASHRAE 140 and CEN/ISO standards).'
      CALL EMENU('Self testing',ITEM,MITEM,INO)

C Help.
      IF(INO.EQ.MITEM-1)THEN
        CALL PHELPD('vld opening',4,'-',0,0,IER)

C Analytical solution for the transient conduction within a
C multi-layered construction.
      ELSEIF(INO.EQ.1)THEN
        CALL ANLCND

C BESTEST tests.
      ELSEIF(INO.EQ.2)THEN
        call STNDRDS
          
C Exit menu.
      ELSEIF(INO.EQ.MITEM)THEN
        return
 
      ENDIF
      
      GOTO 10
      END


      
