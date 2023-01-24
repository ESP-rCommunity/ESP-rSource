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

C This file contains the following routines:
C MODEWP - dew point temperature
C MOMNRD - area weighted MRT
C MOMRTS - area weighted MRT
C MORESL - area weighted resultant temperature
C SENMRT - MRT at a sensor (using explicit view factors)
C SENOPT - operative temperature at a sensor

C ******************** MODEWP ********************
C MODEWP computes the dew point temperature given dry bulb
C temperature and % saturation.

      SUBROUTINE MODEWP(TAI,PSAT,DPT)
#include "building.h"
#include "site.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      character outs*124
      IF(TAI.LT.11.)then
        FS=-0.0000073*TAI+1.00444
      elseif(TAI.GE.11.0.AND.TAI.LT.26.0)then
        FS=0.0000132*TAI+1.004205
      elseif(TAI.GE.26.0.AND.TAI.LE.60.0)then
        FS=0.0000405*TAI+1.003497
      elseif(TAI.GT.60.0.OR.TAI.LT.0.1)then
        write(outs,'(a,F7.2,a)') 'MODEWP: ',TAI,
     &    ' out of range: 60 > dbt > 0, setting dp = 0.0 '
        call edisp(iuout,outs)
        DPT=0.0
        return
      endif
      if(TAI.LT.0.)then
        PSS=10**(10.5380997-(2663.91/(273.16+TAI)))
      elseif(TAI.GE.0.)then
        X=XLHSPS(TAI)
        PSS=10**(X)
      endif
      if(PSAT.gt.100.0)then
        write(outs,'(a,F7.2,a)') 'MODEWP: ',PSAT,
     &    ' out of range: 100 > psat > 0, setting psat = 100.0 '
        call edisp(iuout,outs)
        PSAT=100.0
      elseif(PSAT.le.0.0)then
        write(outs,'(a,F7.2,a)') ' MODEWP: ',PSAT,
     &    ' out of range: 100 > psat > 0, setting psat = 1.0 '
        call edisp(iuout,outs)
        PSAT=1.0
      endif
      GSS=(0.62197*FS*PSS)/(patmos-(FS*PSS))
      G=PSAT*GSS/100.
      PS=(patmos*G)/(FS*(0.62197+G))
      IF(TAI.LT.0.)then
        DPT=(2663.91/(10.5380997-ALOG10(PS)))-273.16
        RETURN
      endif
    1 RHS=ALOG10(PS)
      DPT=TAI
      XLHS=XLHSPS(DPT)
      DIF1=ABS(RHS-XLHS)
      IF(DIF1.LT..01)RETURN
    3 DPT=DPT-1.
      XLHS=XLHSPS(DPT)
      DIF2=ABS(RHS-XLHS)
      IF(DIF2.GT.DIF1)goto 2
      DIF1=DIF2
      goto 3
    2 DPT=DPT+1.
      XLHS=XLHSPS(DPT)
      DIF1=ABS(RHS-XLHS)
    5 DPT=DPT-.1
      XLHS=XLHSPS(DPT)
      DIF2=ABS(RHS-XLHS)
      IF(DIF2.GT.DIF1)goto 4
      DIF1=DIF2
      goto 5
    4 DPT=DPT+.1
      RETURN
      END

      FUNCTION XLHSPS(DPT)
      X=31.59051-8.2*ALOG10(DPT+273.16)+0.0024804*(DPT+273.16)-
     &3142.31/(DPT+273.16)
      XLHSPS=X
      RETURN
      END

C ******************** MOMNRD ********************
C MOMNRD computes the mean radiant temperature from opaque
C surface temperature information. Note
C << TA QM no longer used, but ordering not changed. >>
      SUBROUTINE MOMNRD(ICOMP,TA,TS,QM,NS,TMNRD)
#include "building.h"
#include "geometry.h"

      DIMENSION TS(MS),QM(MS)

C Sum product of opaque surface temperatures and areas.
      ZPROD=0.
      allsurf=0.
      DO 10 I=1,NS
        ZPROD=ZPROD+SNA(ICOMP,I)*TS(I)
        allsurf=allsurf+SNA(ICOMP,I)
   10 CONTINUE

      TMNRD=ZPROD/allsurf
      RETURN
      END

C ******************** MOMRTS ********************
C MOMRTS computes the mean radiant temperature from opaque
C surface temperature information.
C << GEP, GIP, QM, TA not used >>
      SUBROUTINE MOMRTS(ICOMP,TA,TS,QM,NS,OP,GEP,GIP,TMRTS)
#include "building.h"
#include "geometry.h"

      DIMENSION TS(MS),QM(MS),OP(MS)

C Sum product of surface temperatures and areas.
      ZPROD=0.
      tsna = 0.
      DO 10 I=1,NS
        ZPROD=ZPROD+SNA(ICOMP,I)*OP(I)*TS(I)
        tsna = tsna + SNA(ICOMP,I)*OP(I)
   10 CONTINUE

      tarea = tsna
      TMRTS=ZPROD/tarea

      RETURN
      END

C ******************** MORESL ********************
C MORESL computes the resultant temperature from opaque
C surface temperature information.
C << QM not used >>
      SUBROUTINE MORESL(ICOMP,TA,TS,QM,NS,TRESL)
#include "building.h"
#include "geometry.h"

      DIMENSION TS(MS),QM(MS)

C Sum product of opaque surface temperatures and areas.
      ZPROD=0.
      allsurf=0.
      DO 10 I=1,NS
        ZPROD=ZPROD+SNA(ICOMP,I)*TS(I)
        allsurf=allsurf+SNA(ICOMP,I)
   10 CONTINUE

      X1=TA*.5
      X2=.5*(ZPROD/allsurf)
      TRESL=X1+X2
      RETURN
      END

C ******************** SENMRT ********************
C SENMRT computes 1 days mean radiant temperature data at a sensor.
C IOPT - specific temperature differences:
C        1: -Y (back)   - +Y (front)
C        2: +X (right)  - -X (left)
C        3: +Y (front)  - -Y (back)
C        4: -X (left)   - +X (right)
C        5: +Z (top)    - -Z (bottom)
C        6: -Z (bottom) - +Z (top)

      SUBROUTINE SENMRT(IDAY,IZONE,ISENS,ISET,IOPT)
#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      common/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION XD(MZS),surarea(6),stemp(6)

      character outs*124,LMRT*72

C Recover sensor vievfactors.
      IUF=IFIL+1
      if(LVIEW(IZONE)(1:7).eq.'UNKNOWN')then
        call edisp(iuout,'skipping SENMRT (no viewfactors)')
        return
      endif
      LMRT=LVIEW(IZONE)
      call ERMRT(0,IUOUT,IUF,LMRT,IZONE,IER)

C Find surface areas of sensor cube.
      surarea(1)=DXC(ISENS)*DZC(ISENS)
      surarea(2)=DYC(ISENS)*DZC(ISENS)
      surarea(3)=surarea(1)
      surarea(4)=surarea(2)
      surarea(5)=DXC(ISENS)*DYC(ISENS)
      surarea(6)=surarea(5)
      totarea=(surarea(1)+surarea(2)+surarea(5))*2.0
      do 5 i=1,6
        surarea(i)=surarea(i)/totarea
5     continue

C Check view factors sum to one (if not then scale).
      do 10 I=1,6
        sum=0.000
        do 15 J=1,NZSUR(IZONE)
          sum=sum+FFL(ISENS,I,J)
 15     continue
        if (sum.gt.0.0) then
          do 25 J=1,NZSUR(IZONE)
            FFL(ISENS,I,J)=FFL(ISENS,I,J)/sum
 25       continue
        else
          call edisp(iuout,'error in sensor view factors.')
        endif
 10   continue

C Recover all surface temperatures.
      IUNIT=IFIL
      ICJ=2+NZSUR(IZONE)
      N=24*NTS
      do 20 I=1,N
        GVAL(I)=0.

C Return the record number using hash.
        IREC=IHASHD(IDAY)+IHASHT(I)+IHASHZ(IZONE)+IHASHS(ISET)
        read(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=99)(XD(K),K=1,ICJ)

C Calculate MRT.
        XD(1)=0.0
        do 30 K=1,6
          stemp(K)=0.0
          do 40 J=3,ICJ
            stemp(K)=stemp(K)+FFL(ISENS,K,(J-2))*((XD(J)+273.19)**4)
 40       continue
          stemp(K)=stemp(K)**0.25
 30     continue

C Treat data according to IOPT.
        if (IOPT.eq.0) then

C Average surface radiant temperatures.
          XD(1)=0.0
          do 50 K=1,6
            XD(1)=XD(1)+(stemp(K)*surarea(K))
C            stemp(K)=stemp(K)-273.19
 50       continue
          GVAL(I)=XD(1)-273.19

C Calculate delta T between opposite surfaces.  The value of IOPT is the 
C surface representing the front surface.
        elseif (IOPT.eq.1) then
          GVAL(I)=stemp(1)-stemp(3)
        elseif (IOPT.eq.2) then
          GVAL(I)=stemp(2)-stemp(4)
        elseif (IOPT.eq.3) then
          GVAL(I)=stemp(3)-stemp(1)
        elseif (IOPT.eq.4) then
          GVAL(I)=stemp(4)-stemp(2)
        elseif (IOPT.eq.5) then
          GVAL(I)=stemp(5)-stemp(6)
        elseif (IOPT.eq.6) then
          GVAL(I)=stemp(6)-stemp(5)
        endif
 20   continue

C Debug.
C      write(6,*) (GVAL(I),I=1,NTS)

      return

 99   call edisp(iuout,' reading error in SENMRT')

      return
      end

C ******************** SENOPT ********************

C SENOPT computes 1 days operative temperature data at an MRT sensor. If
C there is a CFD domain in the zone, local air temperature and air
C velocity will be used. Returns values via GVAL in common GET2. 

      SUBROUTINE SENOPT(IDAY,IZONE,IMRT,ISET)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "cfd.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      CHARACTER LCFD*72

C Directives for GOCFDGET (more efficient than RDCFDAT).
      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,8)
      COMMON/MFGET/FLOWVALS(MFRS),MFRECVR(MCNN+2+MCONTM)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)
      DIMENSION ZTMS(MS),YDUM(MS),TSO(MS)

      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/ICFNOD/ICFD,ICP

      logical ok,perok,rdok
      character outs*124

      IF(ISAVE.LT.2 .OR. ISAVE.GT.4)THEN
        call edisp(iuout,' SENOPT: Save option does not allow access.')
        goto 999
      ENDIF

C Check if there is CFD in this zone.
      if (IFCFD(IZONE).gt.0) then
        call OPCFD('c',ok,IER)
        if (.not.ok.or.IER.ne.0) goto 924
      
C Read header data and set common block data.
        call RCFDLIBH(IZONE,IER)
        if (IER.ne.0) goto 924


C If old CFD results version, read CFD file to get axis vertices to
C initialise CFD - building domain transformation matrices.
        if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
          CALL RCFDVRTS(IZONE,IER)
          if (IER.ne.0) goto 924
        endif
        CALL INIT_CFDTRANS(IER)
        if (IER.ne.0) goto 924
C Find CFD point closest to MRT sensor.              
        CALL GRIDGEO
        PI = 4.0 * ATAN(1.0)
        R=PI/180.
        SA=SIN(CANG(IMRT)*R)
        CA=COS(CANG(IMRT)*R)
        DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
        DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA
        CALL CFDTRANS(2,XOC(IMRT)+DX,YOC(IMRT)+DY,
     &                  ZOC(IMRT)+DZC(IMRT)/2,XC,YC,ZC,IER)
        CALL FDCFDPT(1,XC,YC,ZC,IIX,IIY,IIZ,IER)
        if (IER.ne.0) goto 924
        IXCEL=IIX-1
        IYCEL=IIY-1
        IZCEL=IIZ-1
        ok=.true.
        goto 925
  924   write(outs,'(a,a,i1,a,i2,a)')' SENOPT: Error setting up CFD ',
     &  'results for operative temperature calculations at MRT sensor ',
     &  IMRT,' in zone ',IZONE,'.'
        call edisp(iuout,outs)
        call edisp(iuout,' CFD will not be used.')
        ok=.false.
  925   CONTINUE
      endif

C Get MRT in GVAL.
      CALL SENMRT(IDAY,IZONE,IMRT,ISET,0)

      N=24*NTS
      do JC1=1,N

C Check if there are CFD results available for this time step.
        if (ok) then
          CALL GETIFRM(IDAY,JC1,IFRAME,IER)
          if (IER.ne.0) then
            write(outs,'(a,i3.3,a,i4.4,a,i2.2)')
     &        ' SENOPT: CFD results not avialable for day ',IDAY,
     &        ' timestep ',JC1,' in zone ',IZONE
            CALL EDISP(IUOUT,outs)
            IER=0
            perok=.false.
          else
            perok=.true.
          endif
        else
          perok=.false.
        endif

C Set up directives for GOCFDGET - retrieve velocity and temperature.
        if (perok) then
          CALL MOFLOWSU() ! clear commons
          NMFGET=2
          IMFGETNO(1,1)=18
          IMFGETNO(1,3)=ICFD
          IMFGETNO(1,4)=1
          IMFGETNO(2,1)=19
          IMFGETNO(2,3)=ICFD
          IMFGETNO(2,4)=1
          IMFLIST(1,1)=IXCEL
          IMFLIST(1,2)=IYCEL
          IMFLIST(1,3)=IZCEL
          CALL GOCFDGET(IFRAME,0,0,0,IER)
          if (IER.ne.0) then
            IER=0
            rdok=.false.
          else
            rdok=.true.
          endif
        else
          rdok=.false.          
        endif

        if (rdok) then ! use CFD results
          WWVEL=FLOWVALS(1)
          TAIR=FLOWVALS(2)          
        else ! assume well mixed zone air and air velocoty < 0.1 m/s
          WWVEL=0.09
          CALL GZTMS
     &      (IDAY,IZONE,ISET,JC1,ZTMS,TSO,TAIR,TMCON,A1,A2,A5,RH)
        endif

C Calculate operative temperature.
        if (WWVEL.le.0.1) then
          OpT=0.5*GVAL(JC1)+0.5*TAIR
        else
          OpT=(TAIR*SQRT(10.0*WWVEL)+GVAL(JC1))/(1+SQRT(10.0*WWVEL))
        endif
        GVAL(JC1)=OpT
      enddo

  999 RETURN
      END
