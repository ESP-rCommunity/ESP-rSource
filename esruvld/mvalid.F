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

C      common/udot/esprc
           
C Indicator of possible focus zone.
C      common/user/browse

      character*18 item(8)
      character h*72,VlDirR*72,VlDirL*72

C      VlDirR='/usr/esru/esp-r/validation'
      DTime=3600.0
      NTime=24
      MonNod=1
      NRoot=500
      NIter=1000
      Gam=0.5

   10 INO=-3
      ITEM(1)='a Analytical'
      ITEM(2)='b Comparative'
      ITEM(3)='c Empirical'
      ITEM(4)='  ---------------'
      ITEM(5)='d Standards'
      ITEM(6)='  ---------------'
      ITEM(7)='? help           '
      ITEM(8)='- Exit validation'
      MITEM=8

      H(1)='    This is the main controller for the validation. '
      H(2)='Four types of validation can be performed:'
      H(3)='1 analytical testing of temperature distribution'
      H(4)='  within multi-layered building construction.'
      H(5)='2 numerical comparision (NOT AVAILABLE).'
      H(6)='3 empirical experimentation (NOT AVAILABLE).'
      H(7)='4 international standards - ASHRAE 140, CEN etc.'
      CALL USRMSG(' ',' ','-')
      CALL EMENU('Validation',ITEM,MITEM,INO)

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
        CALL PHELPD('vld opening',7,'-',0,0,IER)

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
      character*25 item(7)
      
   10 INO=-3
      ITEM(1)='a Conduction'
      ITEM(2)='b Convection'
      ITEM(3)='c Radiation'
      ITEM(4)='d Moisture flow'
      ITEM(5)='  ----------------------'
      ITEM(6)='? help                 '
      ITEM(7)='- Exit '
      MITEM=7

      H(1)='    This is the main controller for the analytical.'
      H(2)='validation.  Four types of analytical validation can be'
      H(3)='performed:'
      H(4)='1. transient heat conduction through multi-layered'
      H(5)='   construction.'
      H(6)='2. convective heat flow. (NOT AVAILABLE)'
      H(7)='3. radiative heat flow. (NOT AVAILABLE)'
      H(8)='4. moisture diffusion through multi-layered construction.'
      H(9)='   (NOT AVAILABLE)'
      CALL USRMSG(' ',' ','-')
      CALL EMENU('  Analytical Validation',ITEM,MITEM,INO)

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
      
