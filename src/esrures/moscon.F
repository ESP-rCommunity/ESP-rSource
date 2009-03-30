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

C This file contains the following routine:
C      MOSCON
C      MOSCONX

C ******************** MOSCON ********************

C MOSCON computes the opaque surface convection load at any
C specified internal or external surface. 

      SUBROUTINE MOSCON(IEX,IC,KK,IDAY,IT,T,XD,YDUM,QC,HC,JREC)

C IEX  = 1 for an inside surface.
C IEX  = 2 for an 'outside' surface.
C IC   = zone number.
C KK   = surface number.
C IDAY = recovery day.
C IT   = time step within recovery day.
C T    = internal air temperature.
C XD   = inside surface temperatures.
C YDUM = 'outside' surface temperatures.
C QC   = surface convection load.
C JREC = current results database record position.

C Inside and 'outside' surfaces are handled differently.

#include "building.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/RECV5/SNA(MCOM,MS),ZBASEA(MCOM),ZTRANA(MCOM)
      COMMON/RECV9/CHARDM(MCOM,MS)
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/RECV12/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HCTHRY/IHCT,ICOREXHCT

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      CHARACTER*25 CSTR
      character outs*124

      DIMENSION XD(MS),YDUM(MS)

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      IUNIT=IFIL
      IFIX=0
      TIMEC=FLOAT(IT)/(FLOAT(NTS)*24.0)
      NPER=NHCFP(IC)
      IF(NPER.EQ.0)goto  52
      DO 20 I=1,NPER
      II=I
      IF(TIMEC.LT.HCFPFT(IC,II).AND.TIMEC.GE.HCFPFT(IC,II))goto 53
   20 CONTINUE
      goto 52
   53 IFIX=II
   52 CONTINUE

      IF(IEX.EQ.2)goto 1

C Surface internal: convection coefficients depend on
C direction of heat flow, air temperature, surface
C temperature and surface geometry.
      T1=T
      T2=XD(KK)
   15 DT=ABS(T1-T2)

C Determine direction of heat flow.
      IF(PELV(IC,KK).LE.45..AND.PELV(IC,KK).GE.-45.)goto 2
      IF(PELV(IC,KK).LT.-45.)goto 3

C Surface is (predominately) horizontal facing down.
      IF(T1.GE.T2)goto 4

C Downward heat flow.
C   6 HC=0.267*((DT**0.25)/(CHARDM(IC,KK)**0.24))
    6 IF(IHCT.EQ.1)THEN

C Theory 1: Cardiff, highest correlation.
         HC=3.10*(DT*0.17)
      ELSEIF(IHCT.EQ.2)THEN

C Theory 2: Halcrow, time invariant low.
         HC=1.0
      ELSEIF(IHCT.EQ.3)THEN

C Theory 3: Halcrow, time invariant high.
         HC=7.0
      ELSE

C Default: Alamdari and Hammond.
         HC=0.60*((DT/(CHARDM(IC,KK)**2.0))**0.2)
      ENDIF

      goto 51

C Upward flow of heat.
C   4 HC=2.03*((DT**0.31)/(CHARDM(IC,KK)**0.08))
    4 IF(IHCT.EQ.1)THEN

C Theory 1: Cardiff, highest correlation.
         HC=2.27*(DT**0.24)
      ELSEIF(IHCT.EQ.2)THEN

C Theory 2: Halcrow, time invariant low.
         HC=1.0
      ELSEIF(IHCT.EQ.3)THEN

C Theory 3: Halcrow, time invariant high.
         HC=7.0
      ELSE

C Default: Alamdari and Hammond.
         HC=((1.4*((DT/CHARDM(IC,KK))**0.25))**6.0
     &   +(1.63*(DT**(1.0/3.0)))**6.0)**(1.0/6.0)
      ENDIF

      goto 51

C Surface is (predominately) vertical.
C   2 HC=1.448*((DT**0.32)/(CHARDM(IC,KK)**0.05))
C   2 HC=0.134*(CHARDM(IC,KK)**(-0.5))+1.11*(DT**(1.0/6.0))
C     HC=HC*HC
    2 IF(IHCT.EQ.1)THEN

C Theory 1: Cardiff, highest correlation.
         HC=2.30*(DT**0.24)
      ELSEIF(IHCT.EQ.2)THEN

C Theory 2: Halcrow, time invariant low.
         HC=1.0
      ELSEIF(IHCT.EQ.3)THEN

C Theory 3: Halcrow, time invariant high.
         HC=7.0
      ELSE

C Default: Alamdari and Hammond.
         HC=((1.50*((DT/CHARDM(IC,KK))**0.25))**6.0
     &   +(1.23*((DT)**(1.0/3.0)))**6.0)**(1.0/6.0)
      ENDIF

      goto 51

C Surface is (predominately) horizontal facing up.
    3 IF(T1.GE.T2)goto 6
      goto 4
   51 IF(IFIX.EQ.0)goto 5
      IF(IEX.EQ.1.AND.HCFVI(IC,KK,IFIX).GE.0.0)HC=HCFVI(IC,KK,IFIX)
      IF(IEX.EQ.2.AND.HCFVE(IC,KK,IFIX).GE.0.0)HC=HCFVE(IC,KK,IFIX)
      goto 5

C Determine the 'other' zone and connection information.
   1  CALL SURADJ(IC,KK,IE,TMP,IZC,ISC,ICN,CSTR)

      IF(IE.EQ.1)TMP=T
      IF(IE.NE.3)goto 7

C Determine adjacent zone air temperature from
C results database.
      IREC=JREC
      J1=IC
      J2=IZC-1
      IF(IZC.LT.IC)J1=IZC
      IF(IZC.LT.IC)J2=IC-1
      IF(J1.EQ.0.OR.J2.EQ.0)goto 1001
      DO 10 I=J1,J2
      NN=NCONST(I)
      IF(IZC.LT.IC)goto 8
      IREC=IREC+3+NN
      goto 10
    8 IREC=IREC-3-NN
   10 CONTINUE
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)TMP

    7 IF(IE.NE.0)goto 9

C Surface external: determine parallel wind component.
      IF(PELV(IC,KK).LT.-45..OR.PELV(IC,KK).GT.45.)goto 11
      WSA=ABS(PAZI(IC,KK)-VAL3(5,IT))
      IF(WSA.GT.180.)WSA=360.0-WSA
      IF(WSA.GT.90.0)goto 12

C Surface 'facing' wind direction.
      IF(WSA.LT.10.)goto 13
      V=VAL3(4,IT)*SIN(WSA*R)
      goto 14

   13 V=0.5*VAL3(4,IT)
      IF(V.GT.0.5)V=0.5
      IF(VAL3(4,IT).GT.2.)V=0.25*VAL3(4,IT)
      goto 14

C Surface 'shaded' from wind.
   12 V=VAL3(4,IT)*0.25*ABS(SIN(WSA*R))
      goto 14
   11 V=VAL3(4,IT)

C Updated to match subsys.f.
   14 HC=2.8+3.0*V
      IF(IFIX.EQ.0)goto 96
      HC=HCFVE(IC,KK,IFIX)

C Corrected to use proper air and surface temperatures for the case
C of surfaces with a real 'outside' face.
   96 DT=(VAL3(2,IT)-YDUM(KK))
      QC=SNA(IC,KK)*DT*HC
      goto 17

C 'Outside' surface is internal.
    9 T1=TMP
      T2=YDUM(KK)
      goto 15

C Determine surface convection load.
    5 QC=SNA(IC,KK)*DT*HC
   17 RETURN

 1000 WRITE(outs,16)IREC
   16 FORMAT(' MOSCON: library error at record',I5)
      call edisp(iuout,outs)
      goto 17

 1001 call edisp(iuout,
     & ' MOSCON: error in source or adjacent zone number.')
      goto 17
      END

C ******************** MOSCONX ********************

C MOSCONX "computes" the opaque surface convection load at any
C specified internal or external surface.  Other facilities
C are available to recover QC from db. Returns selected
C data in QC,HC.

      SUBROUTINE MOSCONX(IEX,IZ,IDAY,IT,QC,HC)

C IEX  = 1 for an inside surface QC.
C IEX  = 2 for an 'outside' surface QC.
C IZ   = zone number.
C IS   = surface number.
C IDAY = recovery day.
C IT   = recovery timestep.
C TAIR = internal air temperature.
C TS   = inside surface temperatures.
C TSO = 'outside' surface temperatures.
C QC   = surface convection load.
C HC   = heat transfer coef.

C Inside and 'outside' surfaces are handled differently.

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/RECV5/SNA(MCOM,MS),ZBASEA(MCOM),ZTRANA(MCOM)
      COMMON/RECV9/CHARDM(MCOM,MS)
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/RECV12/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/hcthry/ihct,icorexhct

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      CHARACTER*25 CSTR

      DIMENSION TS(MS),TSO(MS),OTS(MS),OTSO(MS),QC(MS),HC(MS)

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      IFIX=0

      TIMEC=FLOAT(IT)/(FLOAT(NTS)*24.0)
      NPER=NHCFP(IZ)
      IF(NPER.EQ.0)goto  52
      DO 20 I=1,NPER
        II=I
        IF(TIMEC.LT.HCFPFT(IZ,II).AND.TIMEC.GE.HCFPFT(IZ,II))goto 53
   20 CONTINUE
      goto 52
   53 IFIX=II
   52 CONTINUE

C Get zone temperatures and then loop through each surface.
C Set chosen result set.
      ISET=ISIM
      call GZTMS(IDAY,IZ,ISET,IT,TS,TSO,TAIR,TMCON,A1,A2,A5,RH) 
      NN=NCONST(IZ)
      do 42, IS=1,NN

      IF(IEX.EQ.2)goto 1

C Surface internal: convection coefficients depend on
C direction of heat flow, air temperature, surface
C temperature and surface geometry.
      T1=TAIR
      T2=TS(IS)
   15 DT=ABS(T1-T2)

C Determine direction of heat flow.
      IF(PELV(IZ,IS).LE.45..AND.PELV(IZ,IS).GE.-45.)goto 2
      IF(PELV(IZ,IS).LT.-45.)goto 3

C Surface is (predominately) horizontal facing down.
      IF(T1.GE.T2)goto 4

C Downward heat flow.
    6 IF(IHCT.EQ.1)THEN

C Theory 1: Cardiff, highest correlation.
         HC(IS)=3.10*(DT*0.17)
      ELSEIF(IHCT.EQ.2)THEN

C Theory 2: Halcrow, time invariant low.
         HC(IS)=1.0
      ELSEIF(IHCT.EQ.3)THEN

C Theory 3: Halcrow, time invariant high.
         HC(IS)=7.0
      ELSE

C Default: Alamdari and Hammond.
         HC(IS)=0.60*((DT/(CHARDM(IZ,IS)**2.0))**0.2)
      ENDIF

      goto 51

C Upward flow of heat.
    4 IF(IHCT.EQ.1)THEN

C Theory 1: Cardiff, highest correlation.
         HC(IS)=2.27*(DT**0.24)
      ELSEIF(IHCT.EQ.2)THEN

C Theory 2: Halcrow, time invariant low.
         HC(IS)=1.0
      ELSEIF(IHCT.EQ.3)THEN

C Theory 3: Halcrow, time invariant high.
         HC(IS)=7.0
      ELSE

C Default: Alamdari and Hammond.
         HC(IS)=((1.4*((DT/CHARDM(IZ,IS))**0.25))**6.0
     &   +(1.63*(DT**(1.0/3.0)))**6.0)**(1.0/6.0)
      ENDIF

      goto 51

C Surface is (predominately) vertical.
    2 IF(IHCT.EQ.1)THEN

C Theory 1: Cardiff, highest correlation.
         HC(IS)=2.30*(DT**0.24)
      ELSEIF(IHCT.EQ.2)THEN

C Theory 2: Halcrow, time invariant low.
         HC(IS)=1.0
      ELSEIF(IHCT.EQ.3)THEN

C Theory 3: Halcrow, time invariant high.
         HC(IS)=7.0
      ELSE

C Default: Alamdari and Hammond.
         HC(IS)=((1.50*((DT/CHARDM(IZ,IS))**0.25))**6.0
     &   +(1.23*((DT)**(1.0/3.0)))**6.0)**(1.0/6.0)
      ENDIF

      goto 51

C Surface is (predominately) horizontal facing up.
    3 IF(T1.GE.T2)goto 6
      goto 4
   51 IF(IFIX.EQ.0)goto 5
      IF(IEX.EQ.1.AND.HCFVI(IZ,IS,IFIX).GE.0.0)HC(IS)=HCFVI(IZ,IS,IFIX)
      IF(IEX.EQ.2.AND.HCFVE(IZ,IS,IFIX).GE.0.0)HC(IS)=HCFVE(IZ,IS,IFIX)
      goto 5

C Determine the 'other' zone and connection information.
   1  CALL SURADJ(IZ,IS,IE,TMP,IZC,ISC,ICN,CSTR)

      IF(IE.EQ.1)TMP=TAIR
      IF(IE.eq.3)then

C Determine adjacent zone air temperature from results database.
        call GZTMS(IDAY,IZC,ISET,IT,OTS,OTSO,OTAIR,OTMCON,A1,A2,A5,ORH) 
        TMP=OTAIR
      endif

      IF(IE.NE.0)goto 9

C Surface external: determine parallel wind component.
      IF(PELV(IZ,IS).LT.-45..OR.PELV(IZ,IS).GT.45.)goto 11
      WSA=ABS(PAZI(IZ,IS)-VAL3(5,IT))
      IF(WSA.GT.180.)WSA=360.0-WSA
      IF(WSA.GT.90.0)goto 12

C Surface 'facing' wind direction.
      IF(WSA.LT.10.)goto 13
      V=VAL3(4,IT)*SIN(WSA*R)
      goto 14

   13 V=0.5*VAL3(4,IT)
      IF(V.GT.0.5)V=0.5
      IF(VAL3(4,IT).GT.2.)V=0.25*VAL3(4,IT)
      goto 14

C Surface 'shaded' from wind.
   12 V=VAL3(4,IT)*0.25*ABS(SIN(WSA*R))
      goto 14
   11 V=VAL3(4,IT)

C Updated to match subsys.f.
   14 HC(IS)=2.8+3.0*V
      IF(IFIX.EQ.0)goto 96
      HC(IS)=HCFVE(IZ,IS,IFIX)

C Corrected to use proper air and surface temperatures for the case
C of surfaces with a real 'outside' face.
   96 DT=(VAL3(2,IT)-TSO(IS))
      QC(IS)=SNA(IZ,IS)*DT*HC(IS)
      goto 42

C 'Outside' surface is internal.
    9 T1=TMP
      T2=TSO(IS)
      goto 15

C Determine surface convection load.
    5 QC(IS)=SNA(IZ,IS)*DT*HC(IS)

   42 continue
      RETURN

      END
