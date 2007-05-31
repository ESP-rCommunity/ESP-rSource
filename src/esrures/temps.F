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

C This file contains the following routines:
C MODEWP - dew point temperature
C MOMNRD - area weighted MRT
C MOMRTS - area weighted MRT
C MORESL - area weighted resultant temperature
C SENMRT - MRT at a sensor (using explicit view factors)

C ******************** MODEWP ********************
C MODEWP computes the dew point temperature given dry bulb
C temperature and % saturation.

      SUBROUTINE MODEWP(TAI,PSAT,DPT)
C      character outs*124
      IF(TAI.LT.11.)then
        FS=-0.0000073*TAI+1.00444
      elseif(TAI.GE.11.0.AND.TAI.LT.26.0)then
        FS=0.0000132*TAI+1.004205
      elseif(TAI.GE.26.0.AND.TAI.LE.60.0)then
        FS=0.0000405*TAI+1.003497
      elseif(TAI.GT.60.0.OR.TAI.LT.0.1)then
        write(6,'(a,F7.2,a)') 'MODEWP: ',TAI,
     &    ' out of range: 60 > dbt > 0, setting dp = 0.0 '
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
        write(6,'(a,F7.2,a)') 'MODEWP: ',PSAT,
     &    ' out of range: 100 > psat > 0, setting psat = 100.0 '
        PSAT=100.0
      elseif(PSAT.le.0.0)then
        write(6,'(a,F7.2,a)') ' MODEWP: ',PSAT,
     &    ' out of range: 100 > psat > 0, setting psat = 1.0 '
        PSAT=1.0
      endif
      GSS=(0.62197*FS*PSS)/(1013.25-(FS*PSS))
      G=PSAT*GSS/100.
      PS=(1013.25*G)/(FS*(0.62197+G))
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

      DIMENSION TS(MS),QM(MS)

      COMMON/RECV5/SNA(MCOM,MS),ZBASEA(MCOM),ZTRANA(MCOM)

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

      DIMENSION TS(MS),QM(MS),OP(MS)

      COMMON/RECV5/SNA(MCOM,MS),ZBASEA(MCOM),ZTRANA(MCOM)

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

      DIMENSION TS(MS),QM(MS)

      COMMON/RECV5/SNA(MCOM,MS),ZBASEA(MCOM),ZTRANA(MCOM)

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
C SENMRT computes the resultant temperature from opaque
C surface, window temperature information.
C IOPT - specific temperature differences.

      SUBROUTINE SENMRT(IDAY,IZONE,ISENS,ISET,IOPT)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      common/MRTF/FFL(MCUB,6,MSM),VWF(MS,MS)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION XD(MZS),surarea(6),stemp(6)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character outs*124,LMRT*72,CUBN*6

C Recover sensor vievfactors.
      IUF=IFIL+1
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
          write (6,*) 'error in sensor view factors.'
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

C Debug...
      write (6,*) (GVAL(I),I=1,NTS)

      return

 99   write (6,*) ' reading error in SENMRT'

      return
      end
