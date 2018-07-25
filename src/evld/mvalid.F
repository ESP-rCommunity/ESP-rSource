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

C *********************************************************************
C This file contains the following subroutines:
C   MValid
C   ANLYTC
C   NUMERC
C   EMIPRC

C *********************************************************************
C MValid is the main validation controller.
C *********************************************************************
      subroutine MValid

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GR1D04/GAM,RGAM

      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter
      COMMON/Vld10/VlDirR,VlDirL

      common/FILEP/IFIL

C Path to model and command line file (if any). Browse
C is a logical flag, .true. restricts update/save options.
C      common/rpath/path
           
C Indicator of possible focus zone.
C      common/user/browse

      character*25 item(8)
      character h*72,VlDirR*96,VlDirL*72
      integer MITEM,INO ! max items and current menu item

C      VlDirR='/usr/esru/esp-r/validation'
      DTime=3600.0
      NTime=24
      MonNod=1
      NRoot=500
      NIter=1000
      Gam=0.5

   10 INO=-3
      ITEM(1)='a analytical'
      ITEM(2)='b numerical (unavailable)'
      ITEM(3)='c empirical (unavailable)'
      ITEM(4)='  ---------------'
      ITEM(5)='d standards      '
      ITEM(6)='  ---------------'
      ITEM(7)='? help           '
      ITEM(8)='- exit this menu'
      MITEM=8

      H(1)='This is the main menu for the validation facility. '
      H(2)='The various tests are as follows.'
      H(3)='  a. analytical solution of the temperature distribution'
      H(4)='     within a multi-layered construction.'
      H(5)='  b. approved numerical solutions for specific cases'
      H(6)='     (NOT YET AVAILABLE).'
      H(7)='  c. empirical datasets for specific cases (NOT YET'
      H(8)='     AVAILABLE).'
      H(9)='  d. ASHRAE 140 and CEN/ISO standards.'
      CALL USRMSG(' ',' ','-')
      CALL EMENU('Validation Testing',ITEM,MITEM,INO)

C Analytical.
      IF(INO.EQ.1)THEN
        call tstamp('>','PRJ: analytical validation models enter')
        CALL ANLYTC
        call tstamp('>','PRJ: analytical validation models finish')

C Numerical.
      ELSEIF(INO.EQ.2)THEN
        call tstamp('>','PRJ: numerical validation models enter')
        CALL NUMERC
        call tstamp('>','PRJ: numerical validation models finish')

C Empirical.
      ELSEIF(INO.EQ.3)THEN
        call tstamp('>','PRJ: empirical validation models enter')
        CALL EMIPRC
        call tstamp('>','PRJ: empirical validation models finish')

C BESTEST.
      ELSEIF(INO.EQ.5)THEN
        call tstamp('>','PRJ: standards validation models enter')
        call STNDRDS
        call tstamp('>','PRJ: standards validation models finish')
          
C Help.
      ELSEIF(INO.EQ.MITEM-1)THEN
        CALL PHELPD('vld opening',9,'-',0,0,IER)

C Exit validation.
      ELSEIF(INO.EQ.MITEM)THEN
        return
      ENDIF
      
      GOTO 10
      END

C *********************************************************************
C ANLYTC
C *********************************************************************
      SUBROUTINE ANLYTC

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN

      character h*72
      character*35 item(7)
      integer MITEM,INO ! max items and current menu item
      
   10 INO=-3
      ITEM(1)='a conduction'
      ITEM(2)='b convection (NOT YET AVAILABLE)'
      ITEM(3)='c radiation (NOT YET AVAILABLE)'
      ITEM(4)='d moisture flow (NOT YET AVAILABLE)'
      ITEM(5)='  ----------------------'
      ITEM(6)='? help                 '
      ITEM(7)='- exit this menu'
      MITEM=7

      H(1)='This is the main menu for the analytical verification'
      H(2)='facility. The available options are as follows.'
      H(4)='  a. transient heat conduction through a multi-layered'
      H(5)='     construction.'
      H(6)='  b. surface convection test (NOT YET AVAILABLE).'
      H(7)='  c. radiative heat flow test (NOT YET AVAILABLE).'
      H(8)='  d. moisture diffusion through a multi-layered'
      H(9)='     construction(NOT YET AVAILABLE).'
      CALL USRMSG(' ',' ','-')
      CALL EMENU('Analytical Tests',ITEM,MITEM,INO)

C Conduction.
      IF(INO.EQ.1)THEN
        CALL ANLCND

C Convection.
      ELSEIF(INO.EQ.2)THEN
        CALL ANLCNV

C Radiaion.
      ELSEIF(INO.EQ.3)THEN
        CALL ANLRAD

C Moisture Transport.
      ELSEIF(INO.EQ.4)THEN
        CALL ANLMFL

C Help.
      ELSEIF(INO.EQ.MITEM-1)THEN
        CALL PHELPD('vld opening',9,'-',0,0,IER)

C Return to the main vld menu.
      ELSEIF(INO.EQ.MITEM)THEN
        RETURN
      ENDIF
      GOTO 10
      END

C *********************************************************************
C NUMERC
C *********************************************************************
      SUBROUTINE NUMERC

      RETURN
      END

C *********************************************************************
C EMIPRC
C *********************************************************************
      SUBROUTINE EMIPRC

      RETURN
      END
      
