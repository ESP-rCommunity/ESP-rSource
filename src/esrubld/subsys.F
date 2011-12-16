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
C      MZCFGG
C      MZELWE
C      MZVENT
C      MZWCON
C      MZVAPC

C ******************** MZCFGG ********************
C MZCFGG computes the grey body radiation coefficient
C between each pair of internal opaque surfaces. This
C is given by:

C HR(1,2)=(E1*(T1**4)*F(1,2)-E2*(T2**4)*F(1,2)+R1*
C         E2*(T2**4)*(F(1,2)**2)-R2*E1*(T1**4)*
C         (F(1,2)**2)*A1/A2)/((T1-T2)*(1-R1*R2*
C         (F(1,2)**2)*A1/A2))   *   SBC*A1/A2

C where:

C    HR(1,2)   = grey body radiation coefficient between
C                some opaque surface 1 and surface 2 and
C                allowing for multiple diffuse reflections.
C    E1 & E2   = emissivities of surfaces 1 and 2
C    R1 & R2   = (1-E1) & (1-E2) respectively
C    T1 & T2   = surface temperatures (K)
C    A1 & A2   = surface areas
C    F(1,2)    = the black body geometric configuration
C                factor between surfaces 1 and 2 (as computed
C                in MZCFGB)
C    SBC       = Stefan-Boltzmann constant.

      SUBROUTINE MZCFGG(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON /PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &              NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),
     &              AE(MCOM,MS)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/V2/CFB(MCOM,MST,MST)

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      COMMON/COE31/HRP(MS,MS),ZHRP(MS),HRF(MS,MS),ZHRF(MS)

C The following common saves the computed future time-row
C values for use as present values at the next time-step.
      COMMON/COE31S/HRS(MCOM,MS,MS),ZHRS(MCOM,MS)
      COMMON/COE31PS/HRPS(MCOM,MS,MS),ZHRPS(MCOM,MS)
      common/ts4/itsitr(msch),itrpas

      character outs*144

      DIMENSION QX(MS,MS)

C Function comparing two reals:
      logical bSPEqual


C Set Stefan-Boltzmann constant.
      SBC=56.7E-9
      NC=NCONST(ICOMP)

C Set present time values and initialise future values.
C If iterating save present value on first pass through and then
C use this value on subsequent iterations.
      DO 10 I=1,NC
        if (ITRPAS.eq.0) then
          ZHRP(I)=ZHRS(ICOMP,I)
          ZHRPS(ICOMP,I)=ZHRS(ICOMP,I)
        else
          ZHRP(I)=ZHRPS(ICOMP,I)
        endif
        ZHRF(I)=0.
        DO 20 J=1,NC
          if (ITRPAS.eq.0) then
            HRP(I,J)=HRS(ICOMP,I,J)
            HRPS(ICOMP,I,J)=HRS(ICOMP,I,J)
          else
            HRP(I,J)=HRPS(ICOMP,I,J)
          endif
          HRF(I,J)=0.
   20   CONTINUE
   10 CONTINUE

C Set future time values.
      DO 50 I=1,NC
      J=I+1
      IF(J.GT.NC)goto 50
      DO 60 K=J,NC

C From I to K (/m2 of K).
      T1=TFS(ICOMP,I)+273.16
      T2=TFS(ICOMP,K)+273.16
!       DT=ABS(T1-T2)
      IF(bSPEqual(T1,T2))goto 3
      E1=EI(ICOMP,I)
      E2=EI(ICOMP,K)
      F12=CFB(ICOMP,I,K)
      R1=1.-E1
      R2=1.-E2
      A1=SNA(ICOMP,I)
      A2=SNA(ICOMP,K)

      X1=E1*E2*(T2**4)
      X3=E1*E2*(T1**4)
      X5=T2-T1
      X6=1.-(R1*R2*(F12**2)*A1/A2)
      X7=((X1-X3)*SBC*F12*A1)/(X5*X6*A2)
      X8=0.
      DO 601 LL=1,NC
      IF(LL.EQ.I.OR.LL.EQ.K)goto 601
      X9=(1.0-EI(ICOMP,LL))*CFB(ICOMP,I,LL)*
     &CFB(ICOMP,K,LL)
      X10=1.-(R1*R2*(1.-EI(ICOMP,LL))*CFB(ICOMP,I,LL)*
     &CFB(ICOMP,LL,K)*CFB(ICOMP,K,I))
      X8=X8+(SBC*A1*X9*(X1-X3))/(SNA(ICOMP,LL)*
     &X10*X5)
  601 CONTINUE
      HRF(I,K)=ABS(X7+X8)

C From K to I (/m2 of I).
C ***********************
C     F21=CFB(ICOMP,K,I)

C     X1=E2*(T2**4)*F21
C     X2=E1*(T1**4)*F21
C     X5=T2-T1
C     X6=1.-(R2*R1*(F21**2)*A2/A1)
C     X7=(X1-X2)/(X5*X6)
C     HRF(K,I)=ABS(X7)*SBC*A2/A1
C ***********************

      HRF(K,I)=HRF(I,K)*A2/A1
      goto 60
    3 HRF(I,K)=0.
      HRF(K,I)=0.
      IF(NSINC.GT.1)goto 60
      HRF(I,K)=5.7*CFB(ICOMP,I,K)
      HRF(K,I)=5.7*CFB(ICOMP,K,I)
   60 CONTINUE
   50 CONTINUE

C Summate for each surface.
      DO 70 I=1,NC
        SUM=0.
        DO 80 J=1,NC
          SUM=SUM+HRF(J,I)
   80   CONTINUE

C For I receiving.
        ZHRF(I)=SUM
   70 CONTINUE

C Save future time-row values for use as present
C values at next time-step.
      DO 90 I=1,NC
        ZHRS(ICOMP,I)=ZHRF(I)
        DO 100 J=1,NC
          HRS(ICOMP,I,J)=HRF(I,J)
  100   CONTINUE
   90 CONTINUE

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(4).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 9999
      call edisp(itu,' ')
      write(outs,'(A,I4)')' Subroutine MZCFGG     Trace output',ICNT
      call edisp(itu,outs)
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Output computed values.
      write(outs,'(3A,I2,a)')' Zone ',zname(ICOMP),' (',ICOMP,
     &  ') Inter-surface radiation coefficients'
      call edisp(itu,' ')
      call edisp(itu,outs)
      call edisp(itu,' ')

C MS sensitive, print first 22 surfaces.
      nss=min0(22,nc)
      write(outs,'(5X,A,22I5)')' Summation',(I,I=1,nss)
      call edisp(itu,outs)
      call edisp(itu,' ')
      DO 9992 I=1,NC
        write(outs,9991)I,ZHRF(I),(HRF(I,J),J=1,nss)
 9991   format(I3,F12.2,22F5.1)
        call edisp(itu,outs)
 9992 CONTINUE
      DO 9000 I=1,NC
        DO 9010 J=1,NC
          QX(I,J)=HRF(I,J)*SNA(ICOMP,J)*(TFS(ICOMP,I)-TFS(ICOMP,J))
 9010   CONTINUE
 9000 CONTINUE

C Write out flux for first 22 surfaces in the zone.
      call edisp(itu,' Radiative flux')
      call edisp(itu,' ')
      nss=min0(22,nc)
      write(outs,'(3X,22(I6))')(I,I=1,nss)
      call edisp(itu,outs)
      DO 9020 I=1,NC
        write(outs,'(I3,22F6.0)')I,(QX(I,J),J=1,nss)
        call edisp(itu,outs)
 9020 CONTINUE
 9999 RETURN
      END


C ******************** MZELWE ********************
C  MZELWE computes the longwave radiation exchange between an
C  external opaque surface and its surrounding hemispherical envelope,
C  where the surrounding hemispherical envelope consists of some portion
c  of the sky vault, surrounding buildings and the ground as seen by
c  each surface. The ratio and temperature of each component of the
C  surfaces envelope will be predicted such that an 'Equivalent
C  Temperature' of the surrounds may be established. When the Equivalent
C  temperature has been estimated it is then possible to
C  predict the net longwave radiation exchange between the
C  surface under consideration with its surrounds. An external
C  longwave radiation coefficient is then evaluated, based on the
C  difference between the external ambient and surface temperatures.

      SUBROUTINE MZELWE(ICOMP)
#include "building.h"
#include "site.h"
#include "geometry.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/DNORGH/IRTYPE

      COMMON/CLIMWB/TWBP,TWBF
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &            ARP(MCOM,MS),ARF(MCOM,MS)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/COE33/QELWP(MS),QELWF(MS)
      COMMON/COE33Z/QELWS(MCOM,MS)
      COMMON/COE33ZS/QELWSPS(MCOM,MS)
      COMMON/COE33S/QELWPB(MS),QELWFB(MS),QELWPK(MS),QELWFK(MS),
     &QELWPG(MS),QELWFG(MS)
      COMMON/COE33P/QELWB(MCOM,MS),QELWK(MCOM,MS),QELWG(MCOM,MS)
      COMMON/COE33PS/QELWBPS(MCOM,MS),QELWKPS(MCOM,MS),QELWGPS(MCOM,MS)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      COMMON/ELWE1/A1(MCOM,MS),B1(MCOM,MS),C1(MCOM,MS)
      COMMON/ELWE2/CA(6),CB(6),CC(6),CD(6),CE(6)
      COMMON/ELWE3/GTP(6),GTF(6),QGLP,QGLF,QGGP,QGGF,GC(6,4),
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST

C During setup scan for inbuilt and user defined ground temperatures.
C igttype: if > 1 then index points to most often used standard profile
C         if < 1 then index points to most often used user profile
C         if = 0 then use compiled pattern.
      common/C25/igttype,GTMP(12)

      common/ts4/itsitr(msch),itrpas

C Ground temperature profile to be used in calculation is now a user input.
C If no profile is entered by the user, default temperature profile is
C used.
      COMMON/LongRad/iExtLgRadFlag, eGrdTp
      INTEGER  iExtLgRadFlag
      INTEGER  jMon
      REAL     eGrdTp(12)
      REAL     GTMP
      
c This common block is used to define the sky temperature calculation method
      COMMON/EXLONGWV/iExt_longwave_flg
      integer iExt_longwave_flg    ! 0 = Martin & Berdahl (1984);
                                   ! 1 = Clarke ($7.5.4 JC book, 2nd edition, 2001);
                                   ! 2 = Cole (?);
                                   ! 3 = Czeplak & Kasten (1987);               
                                   ! 4 = Swinbank (clear - uses dry bulb T);                  
                                   ! 5 = Swinbank (clear - uses wet bulb T); 


      character outs*124,ZSDES*28,ZSDESC*20,ZSDESS*16
      DIMENSION PROPB(8),PROPS(8),PROPG(8)
      DIMENSION TBLD(MS),HROF(MS)
      DIMENSION QELWFC(MS)
      logical closer,close

      DATA ZEROK/273.15/,SBC/5.6697E-08/

C  The following data statements carry the proportions of
C  surrounding buildings, sky vault and ground assumed
C  to be seen by a vertical surface for a particular site
C  defined by the Index of Exposure, IEXP.

      DATA PROPB/0.28,0.18,0.1,0.34,0.52,0.0,1.0,0.0/
      DATA PROPS/0.36,0.41,0.45,0.33,0.15,0.5,0.0,0.0/
      DATA PROPG/0.36,0.41,0.45,0.33,0.33,0.5,0.0,0.0/

C External Longwave Radiation
C If ground temperature profile entered, values defined
C in input.F are overwritten.
      IF(iExtLgRadFlag.EQ.1)THEN
        DO 23 jMon=1,12
         GTMP(jMon)=eGrdTp(jMon)
   23   CONTINUE
      ENDIF

      NC=NCONST(ICOMP)
      DO 10 I=1,NC
      
C Update present values, or recover saved present vals if iterating.
        if (ITRPAS.eq.0) then
          QELWP(I) =QELWS(ICOMP,I)
          QELWPB(I)=QELWB(ICOMP,I)
          QELWPK(I)=QELWK(ICOMP,I)
          QELWPG(I)=QELWG(ICOMP,I)
          
C Save present values for possible iterations.
          QELWSPS(ICOMP,I)=QELWS(ICOMP,I)
          QELWBPS(ICOMP,I)=QELWB(ICOMP,I)
          QELWKPS(ICOMP,I)=QELWK(ICOMP,I)
          QELWGPS(ICOMP,I)=QELWG(ICOMP,I)
        else
          QELWP(I) =QELWSPS(ICOMP,I)
          QELWPB(I)=QELWBPS(ICOMP,I)
          QELWPK(I)=QELWKPS(ICOMP,I)
          QELWPG(I)=QELWGPS(ICOMP,I)
        endif
        IF(NSINC.EQ.1)THEN
          QELWP(I)=0.0
          QELWPB(I)=0.0
          QELWPK(I)=0.0
          QELWPG(I)=0.0
        ENDIF
        QELWF(I)=0.0
        HROF(I)=0.0
   10 CONTINUE

C The following section need only be calculated once.
      IF(NSINC.GT.1)goto 1

C Set initial cloud cover = 0.4
      CLOUDC=0.4
      CLOUD1=0.4
      CLOUD2=0.4
      CLOUD3=0.4

C << place to use connection based viewfactors >>
C << props/propg/propb would be replaced by vfgr vfsk vfbl vfob >>
      IF(siteexposureindex.NE.8)goto 101
      PROPS(8)=skyview
      PROPG(8)=groundview
      PROPB(8)=buildingview

C Depending on the Index of Exposure the proportions of the
C surroundings are assigned new variable names.
  101 AV=PROPB(siteexposureindex)
      BV=PROPS(siteexposureindex)
      CV=PROPG(siteexposureindex)

C For each surface calculate the ratio of sky vault,
C surrounding building(s) and ground associated with the
C surface's hemispherical envelope.
      NC=NCONST(ICOMP)
      DO 20 I=1,NC
        ADDCH1=0.0
        IF(IE(ICOMP,I).EQ.0)goto 2
        A1(ICOMP,I)=0.0
        B1(ICOMP,I)=0.0
        C1(ICOMP,I)=0.0
        goto 20
    2   TILT=90.0-PELV(ICOMP,I)
        IF(TILT.GT.180.0)TILT=180.0
        IF(TILT.LT.0.0)TILT=0.0

C Because the values of Av, Bv and Cv are for vertical
C surfaces only, then the values of A, B and C must
C be found, where A, B and C are the actual proportions
C for a surface at any angle of inclination.
        IF(TILT.LT.89.8.OR.TILT.GT.90.2)goto 3

C The following expressions are used for a vertical surface.

C << use vfgr for c1, vfsk for b1, vfbl for a1 >>
C << if obstructions were assumed to be at ambient air T >>
C << where would that fit into the equations? >>
        A1(ICOMP,I)=AV
        B1(ICOMP,I)=BV
        C1(ICOMP,I)=CV
        ADDCH1=AV+BV+CV
        IF(ADDCH1.LT.0.98.OR.ADDCH1.GT.1.02)goto 1001
        goto 20

C Non-vertical surface considered.
C << vfbl for av in following calculation >>
    3   CT=ECOSD(TILT)
        A1(ICOMP,I)=(1.0-ABS(CT))*AV
        DELTAA=AV-A1(ICOMP,I)

C Different equations are used to solve B and C depending
C on the angle of inclination.
C << vfgr for CV and vfsk for BV
        IF(TILT.GT.90.0)goto 4
        C1(ICOMP,I)=CV*(1.0-ECOSD(TILT))
        B1(ICOMP,I)=BV+(CV-C1(ICOMP,I))+DELTAA
        ADDCH1=A1(ICOMP,I)+B1(ICOMP,I)+C1(ICOMP,I)
        IF(ADDCH1.LT.0.98.OR.ADDCH1.GT.1.02)goto 1001
        goto 20
    4   B1(ICOMP,I)=BV*(1.0+ECOSD(TILT))
        C1(ICOMP,I)=CV+(BV-B1(ICOMP,I))+DELTAA
        ADDCH1=A1(ICOMP,I)+B1(ICOMP,I)+C1(ICOMP,I)
        IF(ADDCH1.LT.0.98.OR.ADDCH1.GT.1.02)goto 1001
   20 CONTINUE
      IF(ICOMP.GT.1)goto 1

C Calculate time independent coefficients of the ground model matrix.
C A number of assumptions are involved in this ground model:
C   a) The ground is assumed to be homogeneous common earth with
C      the thermophysical properties Density=1460,Specific Heat=880,
C      and conductivity=1.28.
C   b) A three element model is assumed sufficient. The uppermost
C      element is 0.2m thick and the second and third elements
C      are 0.5m thick.
C   c) Monthly ground temperature assumed constant at a
C      depth of 1.2m.
C   d) Ground surface convective heat transfer coefficient
C      is set at 10 W/m^2K.
      DEN=1460.0
      SHT=880.0
      CON=1.28
      GHTC=10.0
      TIMSTP=3600.0/real(NTSTEP)

C Consider surface node (ie node 1).
      CAP=DEN*SHT*0.1
      CA(1)=2.0*GHTC*TIMSTP/CAP
      CB(1)=2.0*CON*TIMSTP/(CAP*0.1)
      CC(1)=2.0+CA(1)+CB(1)
      CD(1)=2.0-CA(1)-CB(1)
      CE(1)=2.0*TIMSTP/CAP

C Consider nodes 2-6.
      DO 300 I=2,6
        IF(I.EQ.3)goto 301
        IF(I.EQ.5)goto 302
        DXI=0.25
        IF(I.EQ.2)DXI=0.1
        CAP=(CON*TIMSTP)/(DEN*SHT*(DXI**2.0))
        CA(I)=CAP
        CB(I)=CAP
        CC(I)=2.0+(2.0*CAP)
        CD(I)=2.0-(2.0*CAP)
        CE(I)=TIMSTP/(DEN*SHT*DXI)
        goto 300
  301   DX1=0.1
        DX2=0.25
        goto 303
  302   DX1=0.25
        DX2=0.25
  303   CAP=TIMSTP/((DEN*SHT*DX1/2.0)+(DEN*SHT*DX2/2.0))
        CA(I)=CON*CAP/DX1
        CB(I)=CON*CAP/DX2
        CC(I)=2.0+CA(I)+CB(I)
        CD(I)=2.0-CA(I)-CB(I)
        CE(I)=CAP
  300 CONTINUE
    1 IF(ICOMP.GT.1)goto 15

      CALL EDAYR(IDYP,ID,IM)

C If the first timestep of the simulation initialise
C all of the nodes to the ground temperature, otherwise
C only reset the lowest node.
      if(nsinc.gt.1)then
        gtf(6)=GTMP(IM)
      else
        DO 310 J=1,6
          GTF(J)=GTMP(IM)
  310   CONTINUE
      endif
      QGLF=0.0
      QGGF=0.0

C Determine the mean sky vault temperature to which the surface
C radiates. Note that the sky and ground temperatures are assumed
C identical for all zone surfaces at a particlar time-increment.
C Sky temperature (TSKY) algorithm after Berdahl and Martin.

C Evaluate the quantity of cloud cover.
      QT=0.0
      IF(ISUNUP.EQ.0)goto 6
      IF(QFF.LE.0.0.OR.QDF.LE.0.0)goto 6
      CLOUDC=(CLOUD1+CLOUD2+CLOUD3)/3.0
      CLOUD2=CLOUD1
      CLOUD3=CLOUD2

      QT=QFF+QDF*ESIND(SALT)
      IF(IRTYPE.EQ.1)QT=QDF
      RADR=QFF/QT
      IF(RADR.LE.0.0)goto 8
      IF(RADR.GT.1.0)goto 7
      CLOUDC=RADR*RADR
      goto 6
    7 CLOUDC=1.0
      goto 6
    8 CLOUDC=0.0
      RCLOUD=0.0
      goto 12
    6 CLOUD1=CLOUDC
   12 CONTINUE

C Atmospheric pressure (fixed, but should depend on
C site elevation).
      PATM=965.0

C Compute the dew point temperature TDP.
      PSAT=6.1078*EXP(17.08085*TF/(234.175+TF))
      IF(ABS(PSAT).LT.1.0E-6)PSAT=1.0E-6
      PREAL=HF/100.0*PSAT

C For zero RF set dew point temperature to 293.
      IF(ABS(PREAL).GT.1.0E-6)THEN
         TDP=234.175*LOG(PREAL/6.1078)/(17.08085-
     &   LOG(PREAL/6.1078))
      ELSE
         TDP=293.0
      ENDIF

C Here provide the different sky temperature options
      IF(iExt_longwave_flg.EQ.0)THEN

C Use default Martin & Berdahl calculation
C Clear sky condition:
        PI = 4.0 * ATAN(1.0)
        TIM=2.0*PI*real(IHRF)/24.0
        E0=0.711+0.56*(TDP/100.0)+0.73*((TDP/100.0)**2)+
     &   0.013*COS(TIM)+(12.0E-05)*(PATM-1000.)

C Cloudy condition: assume cloud base is at 3km.
        CBH=3.0
        GAMMA=EXP(CBH/(-8.2))

C Radiation from the atmosphere:
        RSKY=SBC*(E0+(1.0-E0)*CLOUDC*GAMMA)*(TF+ZEROK)**4

C Assume the atmosphere is a black body emitter:
        TSKY=(RSKY/SBC)**0.25
      ELSEIF(iExt_longwave_flg.EQ.1)THEN

C Use Clarke's calculation
C Clear sky temperature as in bld #5.4 (after Swinbank).
        TCSKY=0.05532*((TWBF+ZEROK)**1.5)

C Cloudy sky temperature as JC Book.
        TSKYJC=((1.0-CLOUDC)*TCSKY**4+CLOUDC*(1.0-0.84*CLOUDC)*(0.527+
     &  (0.161*(EXP(8.45*(1.0-(273.0/(TWBF+ZEROK))))))+0.84*CLOUDC)*
     &  (TWBF+ZEROK)**4)**0.25
         TJC=TSKYJC-ZEROK 
         TSKY=TSKYJC   
      ELSEIF(iExt_longwave_flg.EQ.2)THEN

C Use Cole's calculation
        EAIR=0.527+0.161*(EXP(8.45*(1.0-(273.0/(TWBF+ZEROK)))))
        ECLOUD=0.16*EAIR+0.84

C Assume cloud base temperature equal to wet bulb.
        TCLOUD=TWBF+ZEROK      

C Swinbank model with ambient air temperature.
        TSKYSW=0.05532*((TF+ZEROK)**1.5)

C Sky temperature after Cole.
        TSKYNEU=
     &  ((1.-CLOUDC)*(TSKYSW**4)+CLOUDC*ECLOUD*(TCLOUD**4))**0.25
        TSNEU=TSKYNEU-ZEROK
        TSKY=TSKYNEU
      ELSEIF(iExt_longwave_flg.EQ.3)THEN

C Use Czeplak & Kasten calculation
        ECZ=(9.365E-06)*(TF+ZEROK)**2
        RWOLKE=(ECZ+(1.0-ECZ)*(0.9-0.065*CBH))*SBC*((TF+ZEROK)**4)
        RKLAR=(1.174*SBC*ECZ*(TF+ZEROK)**4)-33.0
        RGES=(1.0-CLOUDC)*RKLAR+CLOUDC*RWOLKE
        TSKYCZ=(RGES/SBC)**0.25      
        TSCZ=TSKYCZ-ZEROK
        TSKY=TSKYCZ
      ELSEIF(iExt_longwave_flg.EQ.4)THEN

C Use Swinbank (clear - uses dry bulb T) calculation
C Swinbank model with ambient air temperature.
        TSKYSW=0.05532*((TF+ZEROK)**1.5)       
        TSW=TSKYSW-ZEROK
        TSKY=TSKYSW
      ELSEIF(iExt_longwave_flg.EQ.5)THEN

C Use Swinbank (clear - uses wet bulb T) calculation
C Clear sky temperature as in bld #5.4 (after Swinbank).
        TCSKY=0.05532*((TWBF+ZEROK)**1.5)        
        TCS=TCSKY-ZEROK
        TSKY=TCSKY
      ENDIF
      IF(TSKY.LT.200.0.OR.TSKY.GT.325.0)then
       write(outs,'(a,F7.1,a)')'MZELWE: a dubious sky temperature(',
     &   TSKY,' (degK)) has been calculated.'
        call edisp(iuout,outs)
        call edisp(iuout,
     & ' The computed flux is ok unless another warning is given.')
      endif
      CONTINUE

C Calculate ground temperature.
      DO 320 J=1,6
        GTP(J)=GTF(J)
  320 CONTINUE
      QGLP=QGLF
      QGGP=QGGF

C Assume ground is horizontal and totally exposed to the sky
C with no radiative exchange between ground and surrounding
C buildings.

C Longwave loss from ground: GRLW is the longwave reflectance.
      GRLW=0.05
      QGLF=(1.0-GRLW)*SBC*((TSKY**4)-((GTF(1)+ZEROK)**4))

C For NT: required because QT is being corrupted!!!!
C Re-initialise QT.
C      QT=QFF+QDF*ESIND(SALT)
C      IF(IRTYPE.EQ.1)QT=QDF
C End For NT

C Ground solar gain. 
      QGGF=0.0
      IF(ISUNUP.EQ.0)goto 305
      QGGF=QT*(1.0-groundrefl)
  305 CONTINUE

C Completing ground model matrix.
      DO 330 I=1,6
        IF(I.NE.1)goto 306
        GC(I,1)=0.0
        GC(I,2)=CC(I)
        GC(I,3)=-CB(I)
        GC(I,4)=CA(I)*TP+CA(I)*TF+CB(I)*GTP(I+1)+
     &          CD(I)*GTP(I)+CE(I)*(QGLP+QGLF+QGGP+QGGF)
        goto 330
  306   IF(I.EQ.6)goto 309
        GC(I,1)=-CA(I)
        GC(I,2)=CC(I)
        GC(I,3)=-CB(I)
        GC(I,4)=CA(I)*GTP(I-1)+CD(I)*GTP(I)+CB(I)*GTP(I+1)
        goto 330
  309   GC(I,1)=-CA(I)
        GC(I,2)=CC(I)
        GC(I,3)=0.0
        GC(I,4)=CA(I)*GTP(I-1)+CD(I)*GTP(I)+CB(I)*GTMP(IM)+
     &          CB(I)*GTMP(IM)
  330 CONTINUE

C Conduct forward reduction.
      DO 340 I=2,6
        X1=GC(I,1)
        X2=GC(I-1,2)
        X3=X1/X2
        GC(I,2)=GC(I,2)-GC(I-1,3)*X3
        GC(I,4)=GC(I,4)-GC(I-1,4)*X3
  340 CONTINUE

C Conduct backward substitution.
      GTF(6)=GC(6,4)/GC(6,2)
      IC=6
  308 IC=IC-1
      GTF(IC)=(GC(IC,4)-GC(IC,3)*GTF(IC+1))/GC(IC,2)
      IF(IC.NE.1)goto 308

C The average temperature of the surrounding external buildings
c for a surface is estimated by using the temperature of the zone
c surface which has an azimuthal difference of 180 Deg.
c This method is assumed to be applicable for surfaces with
c elevations > -30 and < 30 deg. For all other surfaces
c (ie elevations < -30 and > 30 deg.) use a mean weighted
c average of all external surfaces in all zones under simulation.

c Evaluate mean weighted temperature for surfaces which
c are non-vertical.
      IROOF=0
      INORTH=0
      ISOUTH=0
      IWEST=0
      IEAST=0
      IAVE=0
      TROOF=0.0
      TNORTH=0.0
      TSOUTH=0.0
      TWEST=0.0
      TEAST=0.0
      TAVE=0.0
      DO 30 K=1,NCOMP
      NSUR=NCONST(K)
        DO 40 L=1,NSUR
          IF(IE(K,L).NE.0)goto 40
          TAVE=TAVE+TFC(K,L,1)
          IAVE=IAVE+1
          IF(PELV(K,L).GE.-30.0.AND.PELV(K,L).LE.30.0)goto 41
          TROOF=TROOF+TFC(K,L,1)
          IROOF=IROOF+1
          goto 40
   41     IF(PAZI(K,L).GE.45.0.AND.PAZI(K,L).LT.135.0)THEN
            TEAST=TEAST+TFC(K,L,1)
            IEAST=IEAST+1
          ENDIF
          IF(PAZI(K,L).GE.135.0.AND.PAZI(K,L).LT.225.0)THEN
            TSOUTH=TSOUTH+TFC(K,L,1)
            ISOUTH=ISOUTH+1
          ENDIF
          IF(PAZI(K,L).GE.225.0.AND.PAZI(K,L).LT.315.0)THEN
            TWEST=TWEST+TFC(K,L,1)
            IWEST=IWEST+1
          ENDIF
          IF(PAZI(K,L).GE.315.0.OR.PAZI(K,L).LT.45.0)THEN
            TNORTH=TNORTH+TFC(K,L,1)
            INORTH=INORTH+1
          ENDIF
   40     CONTINUE
   30   CONTINUE
      IF(IAVE.EQ.0)THEN
        IAVE=1
        TAVE=TF
      ENDIF
      TBAVE=(TAVE/real(IAVE))+ZEROK
      IF(IROOF.EQ.0)TBROOF=TBAVE
      IF(IROOF.GT.0)TBROOF=(TROOF/real(IROOF))+ZEROK
      IF(INORTH.EQ.0)TBNRTH=TBAVE
      IF(INORTH.GT.0)TBNRTH=(TNORTH/real(INORTH))+ZEROK
      IF(IEAST.EQ.0)TBEAST=TBAVE
      IF(IEAST.GT.0)TBEAST=(TEAST/real(IEAST))+ZEROK
      IF(ISOUTH.EQ.0)TBSUTH=TBAVE
      IF(ISOUTH.GT.0)TBSUTH=(TSOUTH/real(ISOUTH))+ZEROK
      IF(IWEST.EQ.0)TBWEST=TBAVE
      IF(IWEST.GT.0)TBWEST=(TWEST/real(IWEST))+ZEROK

C Calculate surrounding building temperature for each
C vertical surface.
   15 NC=NCONST(ICOMP)
      DO 60 I=1,NC

      IF(IE(ICOMP,I).NE.0)goto 60
      ELV=ABS(PELV(ICOMP,I))
      IF(ELV.LE.30.0)goto 16
      TBLD(I)=TBROOF
      goto 60
   16 AZI=PAZI(ICOMP,I)+180.0
      IF(AZI.GT.360.0)AZI=AZI-360.0

c Establish zone surface which has an azimuth 180.0 different from
c the surface considered. However, in a REG or GEN body the surface
c may not be exactly 180 deg. therefore some allowance is made until
c the most appropriate surface is found.
      IF(AZI.GE.45.0.AND.AZI.LT.135.0)THEN
        TBLD(I)=TBEAST
      ENDIF
      IF(AZI.GE.135.0.AND.AZI.LT.225.0)THEN
        TBLD(I)=TBSUTH
      ENDIF
      IF(AZI.GE.225.0.AND.AZI.LT.315.0)THEN
        TBLD(I)=TBWEST
      ENDIF
      IF(AZI.GE.315.0.OR.AZI.LT.45.0)THEN
        TBLD(I)=TBNRTH
      ENDIF
      ETF=TF+ZEROK+50.0
      DTF=TF+ZEROK-50.0
      IF(TBLD(I).LT.DTF)TBLD(I)=DTF
      IF(TBLD(I).GT.ETF)TBLD(I)=ETF
   60 CONTINUE

C  Compute present time step future time row values.
      TGRD=GTF(1)+ZEROK
      IF(TGRD.LT.200.0.OR.TGRD.GT.350.0)then
       write(outs,'(a,F8.2,a)')'MZELWE: a dubious ground temperature(',
     &   TGRD,' ) has been calculated.'
        call edisp(iuout,outs)
        call edisp(iuout,
     & ' The computed flux is ok unless another warning is given.')
      endif
      CONTINUE

C Radiation balance for each zone surface.
      DO 50 I=1,NC
        QELWFB(I)=0.
        QELWFK(I)=0.
        QELWFC(I)=0.
        QELWFG(I)=0.
        IF(IE(ICOMP,I).NE.0)goto 49

C If external surface emissivity small, then assume no
C longwave flux exchange.
        IF(EE(ICOMP,I).LT.1.0E-5)goto 49

C Surrounding buildings component.
C      QELWFB(I)=A1(ICOMP,I)*SBC*(TBLD(I)**4-(TFC(ICOMP,I,1)
C     &+ZEROK)**4)/(2/EE(ICOMP,I)-1.0)

C (Paul Strachan) Temporary: remove next 9 lines and
C re-enable previous 2.

C Surrounding buildings component.
        IF(EE(ICOMP,I).LE.1.0)THEN
          QELWFB(I)=A1(ICOMP,I)*SBC*(TBLD(I)**4-(TFC(ICOMP,I,1)+
     &              ZEROK)**4)/(2.0/EE(ICOMP,I)-1.0)
        ELSE
C Modify equation if emissivity is artificially high (>1).
          QELWFB(I)=A1(ICOMP,I)*SBC*(TBLD(I)**4-(TFC(ICOMP,I,1)+
     &              ZEROK)**4)*EE(ICOMP,I)
        ENDIF

C Sky component.
        QELWFK(I)=B1(ICOMP,I)*SBC*EE(ICOMP,I)*(TSKY**4-
     &            (TFC(ICOMP,I,1)+ZEROK)**4)

C Sky component (original equation of Berdahl and Martin).
        QELWFC(I)=B1(ICOMP,I)*4.0*EE(ICOMP,I)*SBC*((TF+ZEROK)**3)*
     &            (TFC(ICOMP,I,1)+ZEROK-TSKY)*(-1.0)

C Angle of surface inclination
C      RNEIGS(ICOMP,I)=90.0-PELV(ICOMP,I)
        RNEIG=90.0-PELV(ICOMP,I)

C Ground component.
C The following special case is used if the bottom of the
C simulated building is parallel to the earth's surface.

C         5.67*10**(-8)
C   q12 = ------------- (T1**4 - T2**4)
C          1    1
C          -- + -- - 1
C          e1   e2

C For NT: required because GRLW is being corrupted!!!!
C        GRLW=0.05
C End For NT

        call eclose(RNEIG,180.0,1.0,closer)
        IF(C1(ICOMP,I).GT.0.95.AND.closer)THEN
          EGRD=(1.0-GRLW)
          QELWFG(I)=(SBC*((TGRD)**4-(TFC(ICOMP,I,1)+ZEROK)**4))/
     &              (1.0/EGRD+1.0/EE(ICOMP,I)-1.0)
        ELSE
          QELWFG(I)=C1(ICOMP,I)*SBC*EE(ICOMP,I)*(1.0-GRLW)*
     &             ((TGRD)**4-(TFC(ICOMP,I,1)+ZEROK)**4)
        ENDIF

C Sum all components.
        QELWF(I)=QELWFB(I)+QELWFK(I)+QELWFG(I)

C Use equation of Berdahl and Martin, if INDEX OF EXPOSURE is 8
C      IF(siteexposureindex.EQ.8)THEN
C        QELWF(I)=QELWFB(I)+QELWFC(I)+QELWFG(I)
C      ENDIF

C Calculate equivalent temperature of surroundings 
C     - used in CFC longwave exchange for outdoor blinds
        TEQV(I)=(A1(ICOMP,I)*(TBLD(I)**4)+B1(ICOMP,I)*(TSKY**4)+
     &          C1(ICOMP,I)*(TGRD**4))**0.25

C Estimate contribution to external longwave flux (for CFC output)
        Bld_fraction(icomp,i)=(A1(ICOMP,I)*(TBLD(I)**4))/(TEQV(I)**4)
        Sky_fraction(icomp,i)=(B1(ICOMP,I)*(TSKY**4))/(TEQV(I)**4)
        Grd_fraction(icomp,i)=(C1(ICOMP,I)*(TGRD**4))/(TEQV(I)**4)

        DT=((TP+TF)/2.0)-TFC(ICOMP,I,1)
        IF(DT.GE.0.0.AND.DT.LT.0.01)DT=0.01
        IF(DT.GT.-0.01.AND.DT.LT.0.0)DT=-0.01
        HROF(I)=QELWF(I)/DT

C High longwave flux ****** WARNING MESSAGE ******
C      IF(ABS(QELWF(I)).GT.400.0)THEN

C (Paul Strachan) Temporary: remove next 2 lines and
C restore last one.

C Don't use correlation if artificial emissivity used (>1)
        IF(ABS(QELWF(I)).GT.495.0.AND.EE(ICOMP,I).LE.1.)THEN
          call zsid(icomp,I,ZSDES,ZSDESC,ZSDESS)
          write(outs,'(A,F9.1,A,A,A,I6)')
     &    ' MZELWE: a longwave flux of',QELWF(I),
     &    ' Watts has been calculated for ',ZSDESC(1:lnblnk(ZSDESC)),
     &    ' @ increment',NSINC
          call edisp(iuout,outs)
          write(outs,'(A)')
     &    ' This is considered high and a correlation will be used.'
          call edisp(iuout,outs)
          write(outs,'(A,F7.1,A,F7.1,A,F7.1)')
     &    ' <sky = ',QELWFK(I),' <buildings = ',QELWFB(I),
     &    ' <ground = ',QELWFG(I)
          call edisp(iuout,outs)
          QELWF(I)=(3.0+1.7*VF)*(TF-TFC(ICOMP,I,1))
        ENDIF
   49   QELWS(ICOMP,I)=QELWF(I)
        QELWB(ICOMP,I)=QELWFB(I)
        QELWK(ICOMP,I)=QELWFK(I)
        QELWG(ICOMP,I)=QELWFG(I)
   50 CONTINUE
      goto 1004

C Error messages.
 1001 call edisp(iuout,
     & ' MZELWE: external view factors do not sum to unity. (stopping)')
      call epwait
      call epagend
      STOP

C Trace output.
 1004 IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ITRACE(4).EQ.0)goto 9999
      IF(NSINC.GT.ITCF.OR.IZNTRC(ICOMP).NE.1)goto 9999
      write(outs,'(A,I4,A,A)')' Subroutine MZELWE Trace output ',ICNT,
     &  ' Zone ',ZNAME(ICOMP)
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Trace output includes sky temperature as determined
C from several different methods.

C << how about allowing user to select the sky method to use? >>

      EAIR=0.527+0.161*(EXP(8.45*(1.0-(273.0/(TWBF+ZEROK)))))
      ECLOUD=0.16*EAIR+0.84
      ETOTAL=(1.0-(0.84*CLOUDC))*EAIR+(0.84*CLOUDC)

C Assume cloud base temperature equal to wet bulb.
      TCLOUD=TWBF+ZEROK
      RCLOUD=CLOUDC*ECLOUD*SBC*(TCLOUD**4)
      RESP=(1.0-CLOUDC)*(5.5*(TWBF+ZEROK)-1289.0)

C Effective sky temperature with cloud reflected, ground emitted
C longwave flux.
      TSKESP=((RESP+RCLOUD)/(ETOTAL*SBC))**0.25

C After Czeplak.
      ECZ=(9.365E-06)*(TF+ZEROK)**2
      RWOLKE=(ECZ+(1.0-ECZ)*(0.9-0.065*CBH))*SBC*((TF+ZEROK)**4)
      RKLAR=(1.174*SBC*ECZ*(TF+ZEROK)**4)-33.0
      RGES=(1.0-CLOUDC)*RKLAR+CLOUDC*RWOLKE
      TSKYCZ=(RGES/SBC)**0.25

C Swinbank model with ambient air temperature.
      TSKYSW=0.05532*((TF+ZEROK)**1.5)

C Sky temperature after Cole.
      TSKYNEU=((1.-CLOUDC)*(TSKYSW**4)+CLOUDC*ECLOUD*(TCLOUD**4))**0.25

C Clear sky temperature as in bld #5.4 (after Swinbank).
      TCSKY=0.05532*((TWBF+ZEROK)**1.5)

C Cloudy sky temperature as JC Book.
      TSKYJC=((1.0-CLOUDC)*TCSKY**4+CLOUDC*(1.0-0.84*CLOUDC)*(0.527+
     &(0.161*(EXP(8.45*(1.0-(273.0/(TWBF+ZEROK))))))+0.84*CLOUDC)*
     &(TWBF+ZEROK)**4)**0.25

      TS=TSKY-ZEROK
      TCS=TCSKY-ZEROK
      TJC=TSKYJC-ZEROK
      TSNEU=TSKYNEU-ZEROK
      TSW=TSKYSW-ZEROK
      TSESP=TSKESP-ZEROK
      TSCZ=TSKYCZ-ZEROK
      write(outs,9997)TS,GTF(1),TF,TWBF
 9997 format(2X,' Tsky = ',F6.2,'  Tgrd = ',F6.2,'  Tamb = ',
     &       F6.2,'  Twet = ',F6.2)
      call edisp(itu,outs)
      write(outs,'(a,f4.2,a,f5.3,a,4f6.1)')'  Cloudcover = ',CLOUDC,
     &  ' GRLW =',GRLW,' SAZI SALT QFF QDF ISUNUP=',SAZI,SALT,QFF,QDF
      call edisp(itu,outs)
      write(outs,'(2X,A,3F7.3)')' Tdewpoint PSAT E0= ',TDP,PSAT,E0
      call edisp(itu,outs)
      write(outs,9973)TSESP,ECLOUD,ETOTAL
 9973 format(2X,' Tsky(bld 5.4a)  = ',F6.2,'   Ecloud = ',F6.4,
     &'  Etotal = ',F6.4)
      call edisp(itu,outs)
      write(outs,'(A,F6.2)') '   Tsky(Clarke)              = ',TJC
      call edisp(itu,outs)
      write(outs,'(A,F6.2)') '   Tsky(Cole)                = ',TSNEU
      call edisp(itu,outs)
      write(outs,'(A,F6.2)') '   Tsky(Czeplak)             = ',TSCZ
      call edisp(itu,outs)
      write(outs,'(A,F6.2)') '   Tclearsky(Swinbank    )   = ',TSW
      call edisp(itu,outs)
      write(outs,'(A,F6.2)') '   Tclearsky(Swinbank,bld)   = ',TCS
      call edisp(itu,outs)
      write(outs,'(A,F6.1)') '   Irradiance,glob (Eglob,h) = ',QT
      call edisp(itu,outs)
      call edisp(itu,
     &' Surface   Tbld   Q(ext.lwr)   Tsur  Coeff.  Tblack Angle Delta')
      NC=NCONST(ICOMP)
      DO 9996 I=1,NC
        TBUILD=TBLD(I)-ZEROK
        RNEIG=90.0-PELV(ICOMP,I)
C Debug.
        DT=((TP+TF)/2.0)-TFC(ICOMP,I,1)
        IF(DT.GE.0.0.AND.DT.LT.0.01)DT=0.01
        IF(DT.GT.-0.01.AND.DT.LT.0.0)DT=-0.01
        xx=QELWF(I)/DT
        call eclose(HROF(I),xx,0.01,close)
        if(.NOT.close)write(itu,*)'corrupted hrof',I,HROF(I),xx,
     &  TFC(ICOMP,I,1)

        IF(IE(ICOMP,I).EQ.0)THEN

C Equivalent black body temperature TBLACK.
          IF(EE(ICOMP,I).GT.1.0E-5)THEN
            TBLACK=((((TFC(ICOMP,I,1)+ZEROK)**4)+(QELWF(I)/
     &      (SBC*EE(ICOMP,I))))**0.25)-ZEROK
            write(outs,9995)I,TBUILD,QELWF(I),TFC(ICOMP,I,1),
     &                     HROF(I),TBLACK,RNEIG,DT
 9995       format(I4,2X,F7.2,2X,F10.2,2X,F6.2,
     &             F10.2,2X,F7.2,2X,2F6.2)
            call edisp(itu,outs)
          ELSE
            write(outs,9990)I,TBUILD,QELWF(I),TFC(ICOMP,I,1),
     &                     HROF(I),RNEIG,DT
 9990       format(I4,2X,F7.2,2X,F10.2,2X,F6.2,
     &             F10.2,2X,'E. Zero',2X,2F6.2)
            call edisp(itu,outs)
          ENDIF
        ELSE
          write(outs,9994)I,QELWF(I),TFC(ICOMP,I,1),HROF(I),RNEIG
 9994     format(I4,2X,'Internal ',F10.2,2X,F6.2,F10.2,
     &    2X,'Internal ',F6.2)
          call edisp(itu,outs)
        ENDIF
 9996 CONTINUE

      call edisp(itu,' ')
      call edisp(itu,
     &' Surface  Qbld    Qclsky    Qsky     Qground     A1  B1  C1 ')
      call edisp(itu,' ')

      DO 9992 I=1,NC
      write(outs,9991)I,QELWFB(I),QELWFC(I),QELWFK(I),
     &                QELWFG(I),A1(ICOMP,I),B1(ICOMP,I),C1(ICOMP,I)
 9991 format(I4,F10.2,F10.2,F10.2,F10.2,3X,F4.2,1X,
     &       F4.2,1X,F4.2)
      call edisp(itu,outs)
 9992 CONTINUE

 9999 RETURN
      END

C ******************** MZVENT ********************
C MZVENT computes:
C CVVP & CVVF - the conductance associated with ventilation
C               air at the present and future time-row.
C CVIP & CVIF - the conductance associated with infiltration
C               air at the present and future time-row.
C QVNP        - the total air load (infiltration and zone-
C               coupled ventilation) at the present time row.
C XX3         - the sum of the product of conductances and
C               'outside' temperatures at the future time-row.
C CVVPM & CVVFM - as CVVP & CVVF but normalised to the
C                 external temperature (for use in MZLS3).

      SUBROUTINE MZVENT(ICOMP)
      
      USE AIM2_InputData, ONLY:iAIM2
      USE AIM2_TIMESTEP, ONLY:AIM2_CONTROL
      
      IMPLICIT NONE

      include "building.h"
      include "geometry.h"
      include "tdf2.h"
      include "sbem.h"
      include "hvac_parameters.h"
      include "blc25_open_windows.h"   
      include "hvac_common.h"
 
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER :: ITCF,ITRACE,IZNTRC,ITU
      COMMON/TC/ITC,ICNT
      INTEGER :: ITC,ICNT
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

      COMMON/C1/NCOMP,NCON
      integer ncomp,ncon
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      INTEGER :: ITCTL,IVL,IVU,IVH
      REAL :: TLO,TUP,THI,ACIL,ACVL,TAL,ACIU,ACVU,TAU,ACIH,ACVH,TAH


      COMMON/PREC5N/CVIN(MDTY,MCOM,MT),CVVN(MDTY,MCOM,MT),
     &ICCN(MDTY,MCOM,MT),TIAN(MDTY,MCOM,MT)

      REAL :: CVIN,CVVN,TIAN
      INTEGER :: ICCN

C Variables used to transport equiv. conductances for infiltration, 
C ventilation from air-flow networks. 
      COMMON/AfnToBldData/fAfnCondInfil,fAfnCondVent,iAfnIndex,
     &                    fAfnCondNorm, bAfnZoneActive,fZoneMassBalance
     
      real fAfnCondInfil(MCOM)   
      real fAfnCondVent(MCOM)
      real fAfnCondNorm(MCOM)
      integer iAfnIndex(MCOM)
      logical bAfnZoneActive(MCOM)
      real fZoneMassBalance(MCOM,4)

      COMMON/BTIME/BTIMEP,BTIMEF
      REAL :: btimep,btimef
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER :: IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL :: QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      common/pvala/tpa(mcom),qpa(mcom)
      REAL :: tpa,qpa
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      REAL :: TFA,QFA

      COMMON/COE39/CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM
      REAL :: CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM

C The following common block saves the future time values
C (held in /COE39/) for use at the next time-step.
C Save present values for possible iterations at this time step.
      COMMON/COE39S/CIF(MCOM),CVF(MCOM),X3(MCOM),CVM(MCOM)
      REAL :: CIF,CVF,X3,CVM
      COMMON/COE39PS/CIP(MCOM),CVP(MCOM),QVP(MCOM),CVPM(MCOM)
      REAL :: CIP,CVP,QVP,CVPM

      COMMON/FVALG/GFA(MCOM)
      REAL :: GFA
      COMMON/MBINFO/ZMBI(MCOM,4)
      REAL :: ZMBI
      COMMON/ZCVNT/ZCVENT(MCOM)
      REAL :: ZCVENT
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

      COMMON/MFSINC/MFBSNC
      INTEGER :: MFBSNC

      common/ts4/itsitr(msch),itrpas
      INTEGER :: itsitr,itrpas

      common/TAFtoH3K/TAF
      real TAF

C Declare local logical variable to check if the zone is actually modelled by AIM2
C (currently not used)
C      logical AIM2_infil_zone_indicator

c Mechanical ventilation systems.
      Common /mechvdef/ iMech, LMech
      Integer iMech
      Character LMech*72

C Declare local variables used to store the conductances caused by
C mechanical ventilation and infiltration.
      REAL ZONE_mechvent_cond,ZONE_infil_cond

      REAL, PARAMETER :: SMALL=1.0E-15

      DIMENSION VAL(MBITS+2)

      CHARACTER LAPROB*72
      character outs*124
      logical close

      INTEGER IDAY1,IDAY2,ID1,ID2
      DIMENSION ACC(MCOM),CCVVFM(MCOM),CCVVF(MCOM)
      logical closeto0
      real rTempnotioOverheat
      common/ncmOperHrSystem/iOperTimestep(MCOM),
     &       iMonthOperTimestep(12,MCOM)
      integer iOperTimestep,iMonthOperTimestep       
      
      common/CountTimesteps/i_countsteps(mcom),numbStartTimesteps,
     &numTotTimstps      
      integer i_countsteps
      integer numbStartTimesteps  !total number of start-up time steps
      integer numTotTimstps !total number of time steps including the start-up days      

      integer imonth !current month at simulation time (to accumulate monthly results)
      integer idayn
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT,grad

      REAL :: acc,acc1,acc2,aci,acr,amh,amn
      REAL :: amx,ccvvf,ccvvfm,cf,cv1,cv2
      INTEGER :: icomp,i,iacc1,iacc2,ic2,ier,ifoc,isd,it1,it2,itdi
      INTEGER :: itrc,ifunit,iv,ival
      REAL :: sum,t1,ta1,ta2,tafl,tafu,trat,tx,val,xx
      REAL :: time

      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)
      logical znotair
      real zfldK,zfldD,zfldC,zfldA,zSWAp,zSWAf,cvair,cvother,xmod,
     &     flowi,flowv

C      CHARACTER*2   cZone_Chars ! Padded zone number (two digits)
      
C Logical flags for indicating which air flow model is in use.      
      logical :: bOPR, bAFN, bAIM2, bNoFlow

C  Initialise.
      IF(NSINC.EQ.1)X3(ICOMP)=0.0

C First compute the present time-row values of CVIP, CVVP
C & QVNP by equating the current simulation time-step
C present values to the future values computed at the
C previous time-step.
C If iterating at this time step then do not update present 
C coefficients after the first pass through this routine.
      FirstPass: if (ITRPAS.eq.0) then
        CVVPM=CVM(ICOMP)
        CVIP=CIF(ICOMP)
        CVVP=CVF(ICOMP)
        QVNP=X3(ICOMP)-(CIF(ICOMP)+CVF(ICOMP))*TFA(ICOMP)

C Remember present coefficients for possible iterations.
        CVPM(ICOMP)=CVM(ICOMP)
        CIP(ICOMP)=CIF(ICOMP)
        CVP(ICOMP)=CVF(ICOMP)
        QVP(ICOMP)=X3(ICOMP)-(CIF(ICOMP)+CVF(ICOMP))*TFA(ICOMP)
        TAF=TF ! initialise ventilation air temp.
      else
        CVVPM=CVPM(ICOMP)
        CVIP=CIP(ICOMP)
        CVVP=CVP(ICOMP)
        QVNP=QVP(ICOMP)

      endif FirstPass

C Compute future time-row values: first set current future time.
      TIME=BTIMEF

C Set 'pre-TIME' and 'post-TIME' on-the-hour values.
      IT1=IHRP
      IT2=IHRF
      IF(IT2.GT.24)IT2=1
      T1=FLOAT(IT1)

      TRAT=TIME-T1
      IF(T1.GT.TIME)TRAT=TIME-(T1-24.0)

      ! This legacy line appears to be part of a convoluted method 
      ! of transporting data from the flow network into the scheduled
      ! air-flow arrays used by the OPR file. It's no longer necessary,
      ! but is referenced in a comment pretaining to a potential bug, 
      ! below. 
      !
      ! IF(IAIRN.ge.1.AND.MFBSNC.EQ.1)TRAT=1.

C If fluid flow network present, and this is the 1st building
C zone, and fluid flow simulation was not already invoked for the
C plant, then invoke fluid flow simulation, but only if time
C is exactly on the hour or MFBSNC=1 (mass flow every building
C time step).
C Data is stored in the dedicated AfnToBldData common variables. 
C (N.B. the COE39S variables CIF, CVF and CVM are not changed;
C neither are the PREC5 variables, which were previously used
C to export the flow simulation results.)
      IF(IAIRN.ge.1.AND.ICOMP.EQ.1) THEN
        IF(MFBSNC.EQ.1.OR.ABS(BTIMEF-FLOAT(IHRF)).LT..0001) THEN
          CALL MFLWCA(1,BTIMEF)
        END IF
      END IF

C If current zone not represented by a mass flow network node,
C but there is temporal data for this zone process it.
      TdfFlow: IF(ICAAS(ICOMP).EQ.0.AND.IZIVENT(ICOMP).ne.0)then

C Use tdf ventilation data, where IZIVENT() points to the position in
C the tdf file. Note: set trace to verbose for testing.
        ICF=0
        itrc=0
        itdi=IZIVENT(ICOMP)
        IFOC=itdi

C This temporal entry holds three static variables:
C ifunit is 0 for ac/h or 1 for m^3/sec
C IACC1 is the 1st source zone for ventilation (0 means skip).
C IACC2 is the 2nd source zone for ventilation (0 means skip).
        ifunit=ITMAR(itdi,IATDF(itdi,1))
        IACC1=ITMAR(itdi,IATDF(itdi,2))
        IACC2=ITMAR(itdi,IATDF(itdi,3))

        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)

C ACI is the infiltration value, ACC1 & ACC2 are 1st & 2nd ventilation
C depending on the unit of flow (ifunit).
        if(ifunit.eq.0)then
          ACI=VAL(ISD)
          ACC1=VAL(ISD+1)
          ACC2=VAL(ISD+2)
        else
          ACI=VAL(ISD)*3600.0/VOL(ICOMP)
          ACC1=VAL(ISD+1)*3600.0/VOL(ICOMP)
          ACC2=VAL(ISD+2)*3600.0/VOL(ICOMP)
        endif

C Debug.
c        CALL DAYCLK(IDYP,BTIMEF,6)
C        write(6,*)'tdf infil data @',btimef,' is item',itdi,
C     &   ' values ',ifunit,aci,acc1,acc2,VAL(ISD),VAL(ISD+1),VAL(ISD+2)

        CVVF=0.
        SUM=0.
        CVIF=0.33*VOL(ICOMP)*ACI
        
        IF(ACI.LE.0.)CVIF=0.

C Fill ZMBI(*,2) = zone coupling mass flow times moisture content
C and  ZMBI(*,4) = total mass flow rate lost from the zone.
C ZCVENT = the zone coupling mass flow of dry air. ZMBI(*,4)=
C ZCVENT(*)+ZMBI(*,1). ZCVENT is purely a trace device.
        ZMBI(ICOMP,2)=0.0
        ZMBI(ICOMP,4)=0.0
        ZCVENT(ICOMP)=0.0
        XX=TF
        IF(ABS(XX).LT.SMALL)XX=1.
        DO 602 I=1,NCOMP
          if(I.eq.IACC1)then
            CCVVF(I)=0.33*VOL(ICOMP)*ACC1
            CVVF=CVVF+CCVVF(I)
            ZCVENT(ICOMP)=ZCVENT(ICOMP)+1.2*VOL(ICOMP)*ACC1/3600.
            ZMBI(ICOMP,2)=ZMBI(ICOMP,2)+1.2*VOL(ICOMP)*ACC1*GFA(I)/3600.
            ZMBI(ICOMP,4)=ZMBI(ICOMP,4)+1.2*VOL(ICOMP)*ACC1/3600.
          elseif(I.eq.IACC2)then
            CCVVF(I)=0.33*VOL(ICOMP)*ACC2
            CVVF=CVVF+CCVVF(I)
            ZCVENT(ICOMP)=ZCVENT(ICOMP)+1.2*VOL(ICOMP)*ACC2/3600.
            ZMBI(ICOMP,2)=ZMBI(ICOMP,2)+1.2*VOL(ICOMP)*ACC2*GFA(I)/3600.
            ZMBI(ICOMP,4)=ZMBI(ICOMP,4)+1.2*VOL(ICOMP)*ACC2/3600.
          else
            CCVVF(I)=0.
          endif
          CCVVFM(I)=CCVVF(I)*TFA(I)/XX
          SUM=SUM+CCVVFM(I)
  602   CONTINUE
        XX3=CVIF*TF+SUM*XX
        CVVFM=SUM

C If UK ncm calculations then use fresh air requirements from
C file (tdf) plus user defined infiltration values. Care should be
C taken when IZIVENT is also greater than 0. 
      elseif(IACTIV(ICOMP).ne.0)then 

C Note: set trace to verbose for testing.
        ICF=0
        itrc=0
        itdi=IACTIV(ICOMP)
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)       

        call eclose(val(isd),0.0,0.001,closeto0) 
        if(.not.closeto0)then

C Multiply: outdoor air value from tdf (based on fresh air requirements
C in l/s/pers)*Occupants density (pers/m2)*zone area
C Removed the occcupant fraction val(isd) from the calculation of ACI 
C because the default ncm calculation assumes that there is no "demand-ventilation".
          ACI=TMAR(itdi,IATDF(itdi,9))*TMAR(itdi,IATDF(itdi,1))
     &       *ZBASEA(ICOMP)
        else
          ACI=0.0
        endif

C <<<Check what would happen if there was also a ventilation value in the>>>>
C <<<original model - variables ACC1 & ACC2>>>>

C Convert l/s to ach
        ACI=ACI*(3600.0/1000.)/VOL(ICOMP)

C Need the number of timesteps of operation - be based on the heating
C and cooling flux
        call eclose(qfut,0.0,0.01,closeto0)  
        if(.not.closeto0)then
          if(i_countsteps(icomp).gt.numbStartTimesteps)then
            call edayr(idyp,idayn,imonth)               
            iOperTimestep(ICOMP)=1+iOperTimestep(ICOMP)
            iMonthOperTimestep(imonth,ICOMP)=
     &        iMonthOperTimestep(imonth,ICOMP)+1  
          endif      
        endif
        
C Set UK NCM notional and reference infiltration/air leakage
        if(i_countsteps(icomp).eq.1)then
          CALL RSBEM !this is needed for obtaining iDsmTestingFlag
        endif
        INOTIcheck: if(INOTI.eq.1.OR.INOTI.eq.2.OR.INOTI.eq.3.OR.
     &    (INOTI.eq.4.AND.iDsmTestingFlag.gt.0))then
          countsteps: if(i_countsteps(icomp).eq.1)then

C If there is no system linked to the current zone then set SFPHS to zero.
            if(IHLZ(ICOMP).eq.0)then
              closeto0=.true.
            else
              call eclose(SFPHS(IHLZ(ICOMP)),0.0,0.01,closeto0)
            endif
            iNatVentilatedFlag=0
            if(IHLZ(ICOMP).eq.0)then
              iNatVentilatedFlag=1
            elseif(IBRUKC(IHLZ(ICOMP)).eq.-1111)then
              if(closeto0)then !heated only space (nat. ventilated)
                iNatVentilatedFlag=1
              endif
            endif       

C Reference building is always naturally ventilated, has radiators and
C DX cooling so set ventilation flag.
            if(inoti.eq.2.or.idsmtestingflag.eq.2)iNatVentilatedFlag=1
          endif countsteps
          
C If zone temperature for naturally ventilated building is greater
C than setpoint+1degC, add 5 air changes per hour (according to UK NCM
C guidance).          
C
C Naturally ventilated buildings with mechanical supply/extract are
C treated as mechanically ventilated in SBEM and so they are not
C included in this overheating rule (as per NCM modelling guide).
C This NCM modelling guide rule has now been implemented as a control
C law because that avoids overcooling to some extent. Uncomment the
C following code to re-implement the 5 air changes per hour rule.
c          rTempnotioOverheat=TMAR(itdi,IATDF(itdi,15))+1.
c          ICOMPP=-1*ICOMP  !!!cmix
c          call MZMIXT(ICOMPP,TMRTT,TMIXX) !!!cmix
c          if(tfa(icomp).gt.rTempnotioOverheat)then
c          if(TMIXX.gt.rTempnotioOverheat)then !!!cmix
c            if(iNatVentilatedFlag.eq.1)then          
c              ACH(ICOMP)=5.
c            endif
c         else     

C Trap 0 volumes in case notional building is not properly generated.
          call eclose(VOL(ICOMP),0.0,0.001,closeto0)
          closetozero: if(closeto0)then	      
            write(outs,'(2A)')' Volume of 0 found in zone: ',
     &      zname(ICOMP)
            call edisp(itu,outs)
            write(outs,'(A)')' Check the notional model and try again'
            call edisp(itu,outs)
          else
            
C Notional building has 10 m^3/h/m^2 according to UK's NCM instructions.
C Multiply 10 by floor area and divide by volume and by 20.
C Use 15 in place of 10 m^3/h/m^2 if typical building.
C << EXP_AREA is currently zero and needs to be calculated.
              IF(INOTI.eq.1.OR.INOTI.eq.2.OR.
     &          (INOTI.eq.4.AND.iDsmTestingFlag.gt.0))THEN
c                ACH(ICOMP)=(10.*EXP_AREA/VOL(ICOMP))/20.
              ELSEIF(INOTI.EQ.3)THEN
c                ACH(ICOMP)=(15.*EXP_AREA/VOL(ICOMP))/20.
              ENDIF

C Alternatively use 0.167 ach - this will help to avoid high ach in cases
C where sloped roofs are used.
          endif closetozero
          call NOTVENTAUX(ICOMP)
        else      

C Using AIR PERMEABILITY values (ACH(ICOMP)) specified in the NCM file.
          if(i_countsteps(icomp).eq.1)then
            CALL RSBEM
          endif
        endif INOTIcheck

        ACI=ACI+ACH(ICOMP)

        CVVF=0.
        SUM=0.
        CVIF=0.33*VOL(ICOMP)*ACI        
        IF(ACI.LE.0.)CVIF=0.

C Set xx=0 for now. Not used at the moment but attempting
C to follow similar pattern as the classic ventilation
C entry in tdf.
        XX=0. 
        XX3=CVIF*TF+SUM*XX
        CVVFM=0.

C Neither the tdf or UK FRESH AIR model were used above.
C So default to the infiltration specified in the common block
C for the day time, zone, and time type (CVIN)
C These values were either specified in the *.opr file. 
      else

C Check for thermostatic controls from .opr file. 
        ChkOprCtl: if ( ITCTL(ICOMP) /= 0 ) then 
C It is controlled. Establish sensed variable.        
            IV=ITCTL(ICOMP)
            IVAL=ABS(ITCTL(ICOMP))
            IF(IVAL.GT.4)IVAL=IVAL-10
            TX=TFA(ICOMP)
            IF(IVAL.EQ.2)TX=TFA(ICF)
            IF(IVAL.EQ.3)TX=TF ! infil only: ext db temp
            IF(IVAL.EQ.4)TX=VF      
            IF(IVAL.EQ.5)TX=TF ! infil, ext db t, factors
        endif ChkOprCtl
      
C Compute parameters needed to access infiltration/ventilation
C schedules. Determine day, time parameters needed to access scheduled infiltration 
C parameters from the OPR file. 
        IDAY1=IDYP
        IF(IT2.EQ.1) then
          IDAY2=IDYP+1
        ELSE
          IDAY2 = IDYP
        ENDIF
        IF(IDAY2.GT.365)IDAY2=IDAY2-365
        ID1=ICALENDER(IDAY1)
        ID2=ICALENDER(IDAY2)
        DtypeLt3: IF(NBDAYTYPE.LE.3)THEN
          ID1=1
          ID2=1
          IF(IDWP.EQ.6)ID1=2
          IF(IDWP.EQ.7)ID1=3
          IF(IT2.EQ.1)then
            IF(IDWF.EQ.6)ID2=2
            IF(IDWF.EQ.7)ID2=3
          else
            IF(IDWP.EQ.6)ID2=2
            IF(IDWP.EQ.7)ID2=3
          endif
        ENDIF DtypeLt3

	  
C Air flow infiltration is specified via the .opr file, 
C the air-flow network, or the AIM-2 infiltration model. Check 
C the contents of the iAirFlowModel array.  
C
C iAirFlowModel is initialized according to the following priority
C table, which preserves support for standard ESP-r conventions. 
C However, controllers such as blc25_open_windows can also modify 
C iAirFlowModel, allowing bps to switch between different air 
C infiltration schemes. The most likely application for such control
C is to switch between the AIM-2 model (infiltration) and the air-flow
C network (natural ventilation), taking advantage of the comparative 
C strengths of these two models.  
C
C   -------------------------------------------------------------------
C   Case      OPR-schedule    Air-flow netork      AIM-2 file   Model
C   -------------------------------------------------------------------
C   0            inactive         inactive          inactive    no-flow
C   1            active           inactive          inactive     OPR
C   2            active           active            inactive     AFN
C   3            active           active            active       AIM-2
C   -------------------------------------------------------------------
C   (note that ESP-r currently won't run without an opr file, making 
C    case 0 moot). 

C It's possible that the controls or user error have specified a model
C that is not configured, or is not computing infiltration for a 
C given zone. If this is the case, default to the next method according
C to the above priority table.
        bAIM2   = .false. 
        bAFN    = .false. 
        bOPR    = .false. 
        bNoFlow = .false. 
        
        CheckAirflowMethod: select case (iAirFlowModel(iComp)) 
        
          case ( iAIM2model )
            ! Make sure AIM2 model is defined: otherwise revert to 
            ! afn/opr
            if ( iAIM2 == 1 ) then 
              bAIM2 = .true. 
            elseif ( bAfnZoneActive(iComp) ) then
              bAFN = .true. 
            else
              bOPR = .true. 
            endif 
            
          case ( iFlowNetwork ) 
            ! Make sure air-flow network is active for this zone, 
            ! otherwise revert to aim2/opr.
            if ( bAfnZoneActive(iComp) ) then 
              bAfn = .true. 
            elseif ( iAIM2 == 1 ) then 
              bAIM2 = .true. 
            else 
              bOPR = .true.
            endif 
          
          case ( iOPRschedule )
            ! Can't revert to no-flow, as it isn't possible. 
            bOPR = .true. 
          
          case default 
            ! Can't happen (there must be an .opr file). 
            bNoFlow = .true. 
        end select CheckAirflowMethod

        
        
C========================================================================          
        AssignInfil: if ( bNoFlow ) then
C User has not specified any air flow (Can't really happen!)          
          CVIF = 0.

C========================================================================         
        elseif (  bOPR ) then
C User has provided scheduled air flows.     

C Gather infiltration values from schedule.
        CV1=CVIN(ID1,ICOMP,IT1)
        CV2=CVIN(ID2,ICOMP,IT2)
C Interpolate between scheduled flows.
        CVIF=CV1+(TRAT*(CV2-CV1))
      
C Check if zone is thermostatically controlled, and possibly assign 
C new infiltration values according to control scheme. 
            
          OprInfCtl: IF(ITCTL(ICOMP) /= 0 .and. 
     &                  ITCTL(ICOMP) /=-1 .and.
     &                  ITCTL(ICOMP) /=-2 .and.
     &                  ITCTL(ICOMP) /=-3 .and.
     &                  ITCTL(ICOMP) /=-17 )then

C Check if infiltration is thermostatically controlled, according to
C wind speed.
C (How can infiltration be thermostatically controlled? Should
C  this code infact be operating on CVVF?)
            OprInfWindCtl: if( ITCTL(ICOMP) == 4 ) then 
              AMH=ACIH(ICOMP)
              AMX=ACIU(ICOMP)
              AMN=ACIL(ICOMP)
              IF(TX.LT.TLO(ICOMP))ACR=AMN
              IF(TX.GT.TUP(ICOMP))ACR=AMX
              IF(TX.GT.THI(ICOMP))ACR=AMH
              IF(TX.GE.TLO(ICOMP).AND.TX.LE.TUP(ICOMP))THEN
                ACR=AMN+(AMX-AMN)*((TX-TLO(ICOMP))/
     &              (TUP(ICOMP)-TLO(ICOMP)))
              ENDIF
              CVIF=ACR*0.33*VOL(ICOMP)
            
C ITCTL = -15, factors for CVIF which respect day types and periods defined
              elseif (ITCTL(ICOMP).EQ.-15) then
                if (TX.LE.TLO(icomp)) then
                  ! do nothing, i.e. use values from .opr schedule
                elseif (TX.GT.TLO(ICOMP).AND.TX.LE.TUP(ICOMP)) then
                  ! multiply .opr-value by ACIL
                  CVIF = CVIF*ACIL(ICOMP)
                elseif (TX.GT.TUP(ICOMP).AND.TX.LE.THI(ICOMP)) then
                  ! multiply .opr-value by ACIU
                  CVIF = CVIF*ACIU(ICOMP)
                else
                  ! multiply .opr-value by ACIH
                  CVIF = CVIF*ACIH(ICOMP)
                endif
C ITCTL = -16, factors for CVIF which respect day types and periods defined
C              This logic looks at both TF and TFA(ICF) and is designed for
C              "summer ventilation"
C << to do: ignore weekends (or better: unoccupied intervals), 
C           add hysteresis for "ACIH"-case (?) >>
              elseif (ITCTL(ICOMP).EQ.-16) then
                if (TF.LE.TLO(icomp)) then
                  ! do nothing, i.e. use values from .opr schedule
                elseif (TF.GT.TLO(ICOMP).and.
     &                   TF.LE.THI(ICOMP)) then
                  if (TFA(ICOMP).LE.TUP(ICOMP)) then
                    ! multiply .opr-value by ACIL
                    CVIF = CVIF*ACIL(ICOMP)
                  elseif (TFA(ICOMP).GT.TUP(ICOMP).and.
     &                     TFA(ICOMP).LE.THI(ICOMP)) then
                    ! multiply .opr-value by ACIU
                    CVIF = CVIF*ACIU(ICOMP)
                  else ! TFA(ICOMP) .gt. thi
                    ! multiply .opr-value by ACIH
                    CVIF = CVIF*ACIH(ICOMP)
                  endif
                else ! TF .gt. THI
                  ! do nothing, i.e. use values from .opr schedule
                endif
C -----END new code (18.08.2010)

            endif OprInfWindCtl
            
            OprDiffCooling: if ( TX.LE.TLO(ICOMP) .or. 
     &                           TX.GT.TUP(ICOMP)     ) then 
              if ( TX .GE. TF ) then 
                CVIF=ACIL(ICOMP)*0.33*VOL(ICOMP)
                IF(TX.GT.TUP(ICOMP))CVIF=ACIU(ICOMP)*0.33*VOL(ICOMP)
                IF(TX.GT.THI(ICOMP))CVIF=ACIH(ICOMP)*0.33*VOL(ICOMP)
              endif 
            endif OprDiffCooling
          
          endif OprInfCtl
C========================================================================          
        elseif ( bAFN ) then 
C User has defined an air-flow network. Use conductance reported from there.        

          CVIF = fAfnCondInfil(iComp)        

C========================================================================      
        elseif ( bAIM2) then 
       
          ! Conductance of infiltration returned for that zone
          ! If the zone was not specified in the AIM-2 file then it will return a zero
          CALL AIM2_CONTROL(ICOMP,ZONE_infil_cond)
        
          ! Assign this to ESP-r's standard infiltration conductance variable `CVIF' so that these
          ! loads are reflected in ESP-r's zonal energy balances.      
           CVIF = ZONE_infil_cond 
        endif AssignInfil
        
C If the mechanical ventilation model is active
C then add this infiltration conductance to the CVIF.
C Note that this mechanical ventilation is added to the CVIF regardless of 
C whether it is from the *.opr, air flow network, or AIM-2.
C This is because the mechanical ventilation continues to run in all cases,
C even when the windows are open.
C The mechanical ventilation model has its own logic to determine its rates (e.g. HRV)
C or whether it is on/off

        ZONE_mechvent_cond = 0.                    ! Reset mech vent conductance.
        IF( iMech.eq.1) THEN      ! f77 syntax since this file also used by bld.
          CALL MECH_VENT_CONTROL(ICOMP,ZONE_mechvent_cond) ! Conductance of mech vent returned.
          CVIF = CVIF + ZONE_mechvent_cond
        ENDIF

C When there is an outdoor air flow through the HVAC  (air source heat pump) system, this is accounted for 
c through its effect on space infiltration. Initially the conductance for outdoor air flow
c is set to zero, then when this value is greater than 0., it's effect on the infiltration of the
c space is accounted for here.
        CVIF = CVIF + outdoor_air_inf_cond(ICOMP)

C Now zone-coupling advection.
C N.B. TIA? holds the normalised (to external air temperature)
C ventilation conductance in case a fluid flow network exists
C (for use in zone-coupled flow calculation only).

C At this stage the ventilation conductance as determined in
C subroutine mzvenc of precal.F assumes that zones are air
C filled. These conductances need to be modified for zones
C that contain another fluid.
        cvair=0.33
        cvother=1000.0*zfldC/3600.0
        xmod=1.
        IF(znotair(ICOMP))xmod=cvother/cvair	 ! zone is not air filled

C Computation of ventilation depends on wether the air flow network is active.      
        AssignVent: if ( bAFN ) then 

C Collect data from air flow module.         
          CF   = fAfnCondVent(iComp) * xmod
          CVVF = CF
          ICF = iAfnIndex(iComp)
          TAF = fAfnCondNorm(iComp)

        elseif ( bOPR .or. bAIM2 ) then 

C Impose computed conductance for scheduled ventilation for .opr or 
C .aim2 case. 
          CV1=CVVN(ID1,ICOMP,IT1)*xmod
          CV2=CVVN(ID2,ICOMP,IT2)*xmod
          IC2=ICCN(ID2,ICOMP,IT2)
          TA1=TIAN(ID1,ICOMP,IT1)
          TA2=TIAN(ID2,ICOMP,IT2)
          CF=CV1+(TRAT*(CV2-CV1))
          ICF=IC2
          TAF=TA2
          ! This is a piece of legacy code that is superfluous here
          ! (and possibly superfluous all together as TRAT is 
          ! set to 1 when the air flow network is active [IARN=1]). 
          ! But it may also be a bug: it's possible the code was 
          ! intended to be 'IF(IARN.ne.1)...', which would make 
          ! more sense given ESP-r's other conventions regarding
          ! .opr file schedules.
          ! 
          ! IF(IAIRN.ge.1)TAF=TA1+(TRAT*(TA2-TA1))
          CVVF=CF
 
C Check if zone is thermostatically controlled, and optionally 
C overwrite the computed ventilation conductance by that determined 
C by the appropriate control scheme. 
          OprVentCtl: if ( ITCTL(ICOMP) .ne.0 ) then 
          
C Check for control on wind 
            OprVentWndCtl: if ( ITCTL(ICOMP).ne.  -4 .and.
     &                          ITCTL(ICOMP).ne. -11 .and.
     &                          ITCTL(ICOMP).ne. -12 .and.
     &                          ITCTL(ICOMP).ne. -13 .and.
     &                          ITCTL(ICOMP).ne. -15 .and.
     &                          ITCTL(ICOMP).ne. -16       ) then 

C Control on wind.
              if( ITCTL(ICOMP) == 4 ) then 
                AMH=ACVH(ICOMP)
                AMX=ACVU(ICOMP)
                AMN=ACVL(ICOMP)
                TAFL=TAL(ICOMP)
                TAFU=TAU(ICOMP)
                IF(TX.LT.TLO(ICOMP))THEN
                  ACR=AMN
                  ICF=IVL(ICOMP)
                  TAF=TAL(ICOMP)
                ELSE IF(TX.GT.TUP(ICOMP).and.TX.LE.THI(ICOMP))THEN
                  ACR=AMX
                  ICF=IVU(ICOMP)
                  TAF=TAU(ICOMP)
                ELSE IF(TX.GT.THI(ICOMP))THEN
                  ACR=AMH
                  ICF=IVH(ICOMP)
                  TAF=TAH(ICOMP)
                ELSE IF(TX.GE.TLO(ICOMP).AND.TX.LE.TUP(ICOMP))THEN
                  ACR=AMN+(AMX-AMN)*((TX-TLO(ICOMP))/
     &                (TUP(ICOMP)-TLO(ICOMP)))
                  TAF=TAFL+(TAFU-TAFL)*((TX-TAFL)/(TAFU-TAFL))
                  ICF=IVL(ICOMP)
                  IF(TX.GT.((TAFL+TAFU)/2.0))ICF=IVU(ICOMP)
                ENDIF
                CVVF=ACR*0.33*VOL(ICOMP)
                CF=CVVF
            
C Add own ventilation control, here
C ITCTL = -17, CVVF is controlled, day types and periods defined are respected.
C    .opr entries are used in a slightly non-conform way:
C       ITCTL   TLO   TUP   THI
C
C low:    ACIL    ACVL   ICF   TAL
C mid:    ACIU    ACVU   ICF   TAU
C high:   ACIH    ACVH   ICF   TAH
C              << functionality ??>>
C << to do: ?? >>
              elseif (ITCTL(ICOMP).EQ.-17) then
                if (TF.LE.TLO(icomp)) then ! TF is ambient temp.
                  TAF  = TAL(icomp)
C   ** "winter control" **
C
C   1.0 ---        --------
C          |     /
C          |   /
C   0.4    | /
C          
C        ACVL   ACVU
                  if (TFA(ICOMP).LE.ACVL(icomp)) then
                    ! ventilation with 100% of scheduled ACH at TAL(icomp),
                    ! support heating ...
                  elseif (TFA(ICOMP).LE.ACVU(icomp)) then
                    ! ventilation rate acc. to ramp between ACVL and ACVU, also at TAL(icomp)
                    ! Lower end of ramp is 40% of scheduled ACH
                    call eclose(ACVL(icomp),ACVU(icomp),0.01,closeto0)
                    if (closeto0) then
                      grad=1.0
                    else
                      grad=0.6/(ACVU(icomp)-ACVL(icomp))
                    endif
                    CVVF = CVVF*(grad*TFA(icomp)+0.4-grad*ACVL(icomp))
                  else
                    ! ventilation with 100% of scheduled ACH at TAL(icomp),
                    ! support cooling ...
!                  CVVF = CVVF*0.4
                  endif
                else ! TF gt. TLO
C   ** transitional / summer season control **
C << add drawing of logic >>
                  ! Set TAF according to ambient temp.
                  if (TF.lt.TUP(ICOMP)) then
                    TAF = TAU(icomp)
                  else
                    TAF = TAH(icomp)
                  endif
                  if (TFA(ICOMP).LE.ACVU(icomp)) then
                    ! ventilation with 40% of scheduled ACH at TAF acc. to ambient temp.
                    CVVF = CVVF*0.4
                  elseif (TFA(ICOMP).lt.ACVH(icomp)) then
                    ! ventilation rate acc. to ramp between ACVU and ACVH, at TAF acc.
                    ! to ambient temp. Lower end of ramp is 40% of scheduled ACH
                    call eclose(ACVH(icomp),ACVU(icomp),0.01,closeto0)
                    if (closeto0) then
                      grad=1.0
                    else
                      grad=0.6/(ACVH(icomp)-ACVU(icomp))
                    endif
                    CVVF = CVVF*(grad*TFA(icomp)+0.4-grad*ACVU(icomp))
                  else
                    ! ventilation with 100% of scheduled ACH at TAF
                  endif
                endif
C -----END new code (30.08.2010)
              endif ! if ICTL(ICOMP) .eq. XX
            endif OprVentWndCtl

            OprVentOthCtl: if ( TX .LE. TLO(ICOMP) .or. 
     &                          TX .GT. TUP(ICOMP)      ) then         
C Some other control.
              
              CVVF=ACVL(ICOMP)*0.33*VOL(ICOMP)
              ICF=IVL(ICOMP)
              TAF=TAL(ICOMP)
              IF(TX.GT.TUP(ICOMP))THEN
                CVVF=ACVU(ICOMP)*0.33*VOL(ICOMP)
                ICF=IVU(ICOMP)
                TAF=TAU(ICOMP)
              END IF
              IF(TX.GT.THI(ICOMP))THEN
                CVVF=ACVH(ICOMP)*0.33*VOL(ICOMP)
                ICF=IVH(ICOMP)
                TAF=TAH(ICOMP)
              END IF
              CF=CVVF
            endif OprVentOthCtl
          endif OprVentCtl
        endif AssignVent
      endif TdfFlow

C Set up zone moisture balance information for use in MZVAPC, unless
C current zone is mapped to mass flow network node, in which case ZMBI
C is filled by MFLF2B. 
C ZMBI,1= infiltration mass flow rate; 2= product of zone coupling
C         mass flow rate and moisture content; 3= product of
C         mechanical system mass flow rate and moisture content;
C         4= total mass flow rate lost from the zone (no moisture
C         included).
      MoistureByMethod: IF( bAFN ) THEN
C........Read moisture balance data from air-flow network.
         ZMBI(ICOMP,1) = fZoneMassBalance(ICOMP,1)
         ZMBI(ICOMP,2) = fZoneMassBalance(ICOMP,2)
         ZMBI(ICOMP,3) = fZoneMassBalance(ICOMP,3)
         ZMBI(ICOMP,4) = fZoneMassBalance(ICOMP,4)    
      else 
C No air-flow network. Establish air and moisture balance based
C on infiltration.  First initialize if there is no flow network or 
C no node in the current zone.

         ZMBI(ICOMP,1)=0.0
         ZMBI(ICOMP,2)=0.0
         ZMBI(ICOMP,4)=0.0

C Check infiltration.
         IF(CVIF.GT.0.0) THEN
           ZMBI(ICOMP,1)=CVIF/1006.
           ZMBI(ICOMP,4)=ZMBI(ICOMP,4)+ZMBI(ICOMP,1)
         END IF

C Check zone coupled ventilation, If no zone coupled ventilation
C reset ZCVENT.
         IF(ICF.GT.0) THEN
           ZMBI(ICOMP,2)=CVVF/1006.
           ZCVENT(ICOMP)=ZMBI(ICOMP,2)
           ZMBI(ICOMP,4)=ZMBI(ICOMP,4)+ZMBI(ICOMP,2)
           ZMBI(ICOMP,2)=ZMBI(ICOMP,2)*GFA(ICF)
         else
           ZCVENT(ICOMP)=0.0
         END IF
      ENDIF MoistureByMethod

C Modify to relate to outside temperature.
      IF( bAFN )THEN

C In the case of a flow network.
        XX=TF
        IF(ABS(XX).LT.SMALL)XX=1.
        CVVFM=TAF
      ELSE

C No flow network.
        XX=TF
        IF(ABS(XX).LT.SMALL)XX=1.
        if(ICF.EQ.0)then
          CVVFM=CF*TAF/XX
        else
          CVVFM=CF*TFA(ICF)/XX
        endif
      ENDIF

C Determine XX3.
      XX3=CVIF*TF+CVVFM*XX

C Save future-time values for use at next time-step.
  171 CVM(ICOMP)=CVVFM
      CIF(ICOMP)=CVIF
      CVF(ICOMP)=CVVF
      X3(ICOMP)=XX3

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(11).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 9999
      write(outs,'(A,I4,A,A)')' Subroutine MZVENT Trace output',ICNT,
     &  ' Zone ',zname(ICOMP)
      call edisp(itu,' ')
      call edisp(itu,outs)
      ICNT=ICNT+1
      CALL DAYCLK(IDYP,BTIMEF,ITU)

C Output computed results.
      write(outs,'(4(a,f12.4))') ' CVIP   = ',CVIP,
     &                           ' CVIF   = ',CVIF,
     &                           ' CVVP   = ',CVVP,
     &                           ' CVVF   = ',CVVF
      call edisp(itu,' ')
      call edisp(itu,outs)
      write(outs,'(3(a,f12.4))') ' QVNP   = ',QVNP,
     &                           ' CVVFM  = ',CVVFM,
     &                           ' XX3    = ',XX3
      call edisp(itu,outs)
      write(outs,'(4(a,f12.4))') ' ZMBI1  = ',ZMBI(ICOMP,1),
     &                           ' ZMBI2  = ',ZMBI(ICOMP,2),
     &                           ' ZMBI3  = ',ZMBI(ICOMP,3),
     &                           ' ZMBI4  = ',ZMBI(ICOMP,4)
      call edisp(itu,outs)
      write(outs,'(2(a,f12.4))') ' ZCVENT = ',ZCVENT(ICOMP),
     &                           ' ACH    = ',ACH(ICOMP)
      call edisp(itu,outs)

      flowi=CVIF/1006.0		! infiltration mass flow rate
      flowv=CVVF/1006.0		! ventilation mass flow rate
      if(znotair(ICOMP)) then
         flowi=flowi*1006.0/zfldc
         flowv=flowv*1006.0/zfldc
      endif
      write(outs,'(2(a,f12.4))') ' flowi  = ',flowi,
     &                           ' flowv  = ',flowv
      call edisp(itu,outs)
 9999 RETURN
      END

C ******************** MZWCON ********************
C MZWCON computes:
C ZAUGEP & ZAUGEF - the sum of the product of area and U-value
C                   for external windows at the present and
C                   future time row.
C ZAUGIP & ZAUGIF - the sum of the product of area and U-value
C                   for internal windows at the present and
C                   future time row.
C QWCP            - the total window conduction load (for internal
C                   + external windows) at the present time row.
C XX1             - the sum of the product of window areas, U-values
C                   and 'Outside' temperature at the future time-row.
C ZGIPM & ZGIFM   - as ZAUGIP & ZAUGIF but normalised to the
C                   external temperature (for use in MZLS3).

      SUBROUTINE MZWCON(ICOMP)

#include "building.h"

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/COE35/ZAUGEP,ZAUGIP,ZAUGEF,ZAUGIF,QWCP,XX1,ZGIPM,ZGIFM

C The following common blocks save the future time values
C (held in /COE35/) for use at the next time step.
      COMMON/COE35S/ZGEF(MCOM),ZGIF(MCOM),X1(MCOM),ZGIMF(MCOM)
      COMMON/COE35PS/ZGEPS(MCOM),ZGIPS(MCOM),QWCPS(MCOM),ZGIMPS(MCOM)

      common/ts4/itsitr(msch),itrpas

C Initialise.
      IF(NSINC.EQ.1)X1(ICOMP)=0.0

C First compute the present time-row values of ZAUGEP,
C ZAUGIP, QWCP by equating the current simulation
C time-step present values to the future values computed
C at the previous time-step.
C If iterating save present value on first pass through and then
C use this value on subsequent iterations.
      if (ITRPAS.eq.0) then
        ZGIPM =ZGIMF(ICOMP)
        ZAUGEP=ZGEF(ICOMP)
        ZAUGIP=ZGIF(ICOMP)
        QWCP  =X1(ICOMP)-(ZGEF(ICOMP)+ZGIF(ICOMP))*TFA(ICOMP)
        
C Save present values.
        ZGIMPS(ICOMP)=ZGIPM
        ZGEPS(ICOMP) =ZAUGEP
        ZGIPS(ICOMP) =ZAUGIP
        QWCPS(ICOMP) =QWCP
      else
        ZGIPM =ZGIMPS(ICOMP)
        ZAUGEP=ZGEPS(ICOMP)
        ZAUGIP=ZGIPS(ICOMP)
        QWCP  =QWCPS(ICOMP)
      endif

C Compute future time row values.
      ZGIFM=0.
      ZAUGEF=0.
      ZAUGIF=0.
      XX1=0.

C Save future time values for use at next time-step.
      ZGIMF(ICOMP)=ZGIFM
      ZGEF(ICOMP)=ZAUGEF
      ZGIF(ICOMP)=ZAUGIF
      X1(ICOMP)=XX1

      RETURN
      END


C ******************** MZVAPC ********************

C This routine calculates zone relative humidity by performing
C a moisture balance at the air point.

      SUBROUTINE MZVAPC(ICOMP,ZRH,CNDS)
#include "building.h"
#include "geometry.h"

#include "hvac_parameters.h"
#include "hvac_common.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF

      COMMON/PREC6L/TQL1(MCOM,MT),TQL2(MCOM,MT),TQL3(MCOM,MT)
      COMMON/COE34L/QLAT(MCOM)
      COMMON/FVALG/GFA(MCOM)
      COMMON/PVALG/GPA(MCOM)
      COMMON/WBULBO/IOPT

      common/evapsur/vapsur(mcom)
      COMMON/EVPSRC/VSRCF(MCOM),VSRCP(MCOM)

      COMMON/VAPC1/VCIP(MCOM),VCIF(MCOM),VCCP(MCOM),VCCF(MCOM),
     &VCLP(MCOM),VCLF(MCOM),VCMP(MCOM),VCMF(MCOM),VCLGP(MCOM),
     &VCLGF(MCOM)
      COMMON/VAPC2/ZMDAP(MCOM),ZMDAF(MCOM)
      COMMON/ATPRES/PATMOS

      COMMON/MBINFO/ZMBI(MCOM,4)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/AVRAGE/IDAVER

C Common for relative humidity control (ideal)
      COMMON/IDRHCTL1/RHSPTU(MCOM),RHSPTL(MCOM),RHHC(MCOM),
     &RHDC(MCOM),IRHCT(MCOM)
      COMMON/RHCTLDAT/ZLLDF(MCOM),ZLLDP(MCOM)

      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST17/RHmax
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)

c Common for time step computed parameters of HVAC system
c in common/hvac_timestep_parameters is included via hvac_parameters.h.
      character outs*124
      LOGICAL MSTROK,MSTRZN
      real ADDMOIST

C Moisture modelling is invoked for the current zone.
      IF(MSTROK.AND.MSTRZN(ICOMP))THEN
        TCfair=TFA(ICOMP)
        ZRH=Pfair(ICOMP)/Psat01(TCfair)*100.0
        GFA(ICOMP)=HUMRT1(TCfair,ZRH,PATMOS,IOPT)
        IF(IDAVER.NE.1.AND.ZRH.LT.RHmax)THEN
          TCpair=TPA(ICOMP)
          ZRHp=Ppair(ICOMP)/Psat01(TCpair)*100.0
          ZRH=(ZRH+ZRHp)/2.0
        ENDIF
        CNDS=Ffair(ICOMP)-Fpair(ICOMP)
        IF(CNDS.LT.0.0)CNDS=0.0
        RETURN
      ENDIF

C Initialise.
      IF(NSINC.GT.1)goto 5
      VCIF(ICOMP)=0.0
      VCCF(ICOMP)=0.0
      VCLF(ICOMP)=0.0
      VCMF(ICOMP)=0.0
      VCLGF(ICOMP)=0.0
      VSRCF(ICOMP)=0.0
      ZMDAF(ICOMP)=1.2*VOL(ICOMP)
      ZLLDP(ICOMP)=0.0
      GFA(ICOMP)=0.0032
      ENTHCT=0.0
      ENTHUC=0.0

    5 VCIP(ICOMP)=VCIF(ICOMP)
      VCCP(ICOMP)=VCCF(ICOMP)
      VCLP(ICOMP)=VCLF(ICOMP)
      VCMP(ICOMP)=VCMF(ICOMP)
      VCLGP(ICOMP)=VCLGF(ICOMP)
      ZMDAP(ICOMP)=ZMDAF(ICOMP)
      VSRCP(ICOMP)=VSRCF(ICOMP)
      ZLLDP(ICOMP)=ZLLDF(ICOMP)
      GPA(ICOMP)=GFA(ICOMP)

C Infiltration mass flow rate only.
      VCIF(ICOMP)=ZMBI(ICOMP,1)

C Zone coupling mass flow rate * moisture content.
      VCCF(ICOMP)=ZMBI(ICOMP,2)

C Get the gain from evaporative  surface and re set the value for the next time-step
      VSRCF(ICOMP)=vapsur(ICOMP)
      vapsur(ICOMP)=0.0

C Enter gain due to mechanical system here
C mechanical system mass flow rate * moisture content.
      VCMF(ICOMP)=ZMBI(ICOMP,3)

C Set latent load from casual gains (value set in casual.F, mzcasi).      
      QL=QLAT(ICOMP)

c HVAC
c Originally, the latent load QL of the space was divided by 2454000 
c to get the mass of saturated steam injected inside the space. This 
c is updated so that the enthalpy of saturated steam is allowed to change 
c with the zone temperature.
      enthalpy_sat_steam = 2501000. + 1860. * 0.5 * (TFA(ICOMP) +
     &                     TPA(ICOMP))
      VCLGF(ICOMP)=QL/enthalpy_sat_steam

C Total mass flow rate lost from the zone (no moisture included).
      VCLF(ICOMP)=ZMBI(ICOMP,4)

C Zonal mass of dry air (relies on DENSIT in MFUTIL of ESRUmfs).
      ZMDAF(ICOMP)=DENSIT(1,TFA(ICOMP))*VOL(ICOMP)

C Perform a free float air node moisture balance.
C Mass flow into the zone is averaged over present and future timesteps, this mirrors 
C the treatment of exfiltration. (Previously only future inlet flows were considered 
C leading to instability with timesteps over 3 minutes).
C RHS contains all the known terms:
C    present value + all in (present and future) + all out (present).
C Last line of equation takes into account the effect of a
C cooling coil on zone moisture balance.
      RHS=GPA(ICOMP)*ZMDAP(ICOMP)+
     &    (VCIF(ICOMP)*GEXTF+VCCF(ICOMP)+VCMF(ICOMP)+VCLGF(ICOMP)+
     &    VSRCF(ICOMP))*3600./real(2*NTSTEP)+
     &    (VCIP(ICOMP)*GEXTP+VCCP(ICOMP)+VCMP(ICOMP)+VCLGP(ICOMP)+
     &    VSRCP(ICOMP))*3600./real(2*NTSTEP)-
     &    GPA(ICOMP)*VCLP(ICOMP)*3600./real(2*NTSTEP)
     &    + coil_moist_in(ICOMP) - coil_moist_out1(ICOMP)  

C Equation modified to account for cooling coil operation effect on 
C zone moisture balance.
C Calculate the future zone air moisture content. This is only correct if there
C is no humidity control. Mass balance altered later if there is RH control.
C      GFA(ICOMP)=RHS/(ZMDAF(ICOMP)+VCLF(ICOMP)*3600./(2.*NTSTEP))
      GFA(ICOMP)=RHS/(ZMDAF(ICOMP)+VCLF(ICOMP)*3600./real(2*NTSTEP)+ 
     &                coil_moist_out2(ICOMP))

C Set coil moisture parameters to zero for the current zone
      coil_moist_in(ICOMP) = 0.
      coil_moist_out1(ICOMP) = 0.
      coil_moist_out2(ICOMP) = 0.

C Trace ... does the moisture equation balance?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)THEN
        CONTINUE
      ELSE

        IF(ITRACE(10).EQ.0.OR.NSINC.GT.ITCF.OR.
     &  IZNTRC(ICOMP).NE.1) THEN
          CONTINUE        
        ELSE

C Write header of the trace message.
          write(outs,'(A,I4)')' Subroutine MZVAPC   Trace output',ICNT
          call edisp(itu,outs)
          write(outs,'(23X,A,I4)')' Zone',ICOMP
          ICNT=ICNT+1
          CALL DAYCLK(IDYP,BTIMEF,ITU)
          call edisp(itu,outs)

C Left hand side
          WRITE(OUTS,'(A)')' Left hand side of mass balance'
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Future mass (kg)', 
     &      GFA(ICOMP)*ZMDAF(ICOMP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Present mass (kg)', 
     &      GPA(ICOMP)*ZMDAP(ICOMP)
          CALL EDISP(ITU,OUTS)

          AHS=GFA(ICOMP)*ZMDAF(ICOMP) - GPA(ICOMP)*ZMDAP(ICOMP)
          WRITE(OUTS,'(A,F10.5)')' Future - present mass (kg):',AHS
          CALL EDISP(ITU,OUTS)

C Right hand side
          WRITE(OUTS,'(A)') ' '
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A)') ' Right hand side 
     &            (* indicates present timerow) '
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Infiltration in (kg) ',
     &      VCIF(ICOMP)*GEXTF*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Infiltration* in (kg) ',
     &      VCIP(ICOMP)*GEXTP*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Ventilation in (kg) ',
     &      VCCF(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Ventilation* in (kg) ',
     &      VCCP(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Mechanical in (kg) ',
     &      VCMF(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Mechanical* in (kg) ',
     &      VCMP(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Gains in (kg) ',
     &      VCLGF(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Gains* in (kg) ',
     &      VCLGP(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Evaporative gain in (kg) ',
     &      VSRCF(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Evaporative gain* in (kg) ',
     &      VSRCP(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Losses out (kg) ',
     &      VCLF(ICOMP)*GFA(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)
          WRITE(OUTS,'(A,F10.5)') ' Losses*out (kg) ',
     &      VCLP(ICOMP)*GPA(ICOMP)*3600./real(2*NTSTEP)
          CALL EDISP(ITU,OUTS)

C Sum of the RHS including outflow at future timerow, assuming no RH control.
          RHS2=(VCIF(ICOMP)*GEXTF+VCCF(ICOMP)+VCMF(ICOMP)+VCLGF(ICOMP)
     &    +VSRCF(ICOMP))*3600./real(2*NTSTEP)+
     &    (VCIP(ICOMP)*GEXTP+VCCP(ICOMP)+VCMP(ICOMP)+VCLGP(ICOMP)
     &    +VSRCP(ICOMP))*3600./real(2*NTSTEP) -GPA(ICOMP)*VCLP(ICOMP)
     &    *3600./real(2*NTSTEP)
     &    -GFA(ICOMP)*VCLF(ICOMP)*3600./real(2*NTSTEP)
          WRITE(OUTS,'(A,F10.5)') 'Total in - total out (kg)',RHS2
          CALL EDISP(ITU,OUTS)

C Difference between RHS and LHS.
          WRITE(OUTS,'(A,F10.5)') 'LHS-RHS assuming no humidity control'
     &       ,AHS-RHS2
          CALL EDISP(ITU,OUTS)
        ENDIF     
      ENDIF

C If the relative humidity control is active then apply control, the
C above value of moisture content is essentially the free floating value
C if no plant is connected to the system and there is no RH control.
C Only control type 1 dealt with here.
      IF(IRHCT(ICOMP).EQ.1) THEN

C Calculate zone moisture content at the upper and lower RH set points. 
        GCTU=HUMRT1(TFA(ICOMP),RHSPTU(ICOMP),PATMOS,0) 
        GCTL=HUMRT1(TFA(ICOMP),RHSPTL(ICOMP),PATMOS,0)

        IF(GFA(ICOMP).GT.GCTU.OR.GFA(ICOMP).LT.GCTL)THEN

C Calculate amount of moisture (kg) to remove if dehumidification required.
          IF(GFA(ICOMP).GT.GCTU) THEN         
            ADDMOIST=
     &        GCTU*(ZMDAF(ICOMP)+VCLF(ICOMP)*3600./real(2*NTSTEP))
     &        -RHS
            GFA(ICOMP)=GCTU
C Check that the maximum dehumidification is not exceeded (kg).
C ADDMOIST will be negative for dehumidification.   
            IF(ADDMOIST.LT.RHDC(ICOMP)*3600./real(NTSTEP)) THEN

C At the maximum dehumidification, so undertake moisture mass balance with this quantity of moisture removed.
              GFA(ICOMP)=(RHDC(ICOMP)*3600./real(NTSTEP)+RHS)/
     &                 (ZMDAF(ICOMP)+VCLF(ICOMP)*3600./real(2*NTSTEP))
              ADDMOIST=RHDC(ICOMP)*3600./real(NTSTEP)
            ENDIF
C Enthalpy for final moisture content (J/kg). 
            ENTHCT=ENTHP2(TFA(ICOMP),GFA(ICOMP))*1000.
C Enthalpy if  humidification had not been applied.
            ENTHUC=ENTHP2(TFA(ICOMP),GFA(ICOMP)
     &             -ADDMOIST/ZMDAF(ICOMP))*1000.
C Calculate the latent addition/extraction (W).
            ZLLDF(ICOMP)=(ENTHCT-ENTHUC)*ZMDAF(ICOMP)
     &                         /(3600./real(NTSTEP))

          ELSEIF(GFA(ICOMP).LT.GCTL)THEN  
            ADDMOIST=
     &           GCTL*(ZMDAF(ICOMP)+VCLF(ICOMP)*3600./real(2*NTSTEP))
     &           -RHS
            GFA(ICOMP)=GCTL

C Check that the maximum humidification is not exceeded (kg).
C ADDMOIST will be positive for humidification.   
            IF(ADDMOIST.GT.RHHC(ICOMP)*3600./real(NTSTEP)) THEN

C At the maximum humidification, so undertake moisture mass balance with this quantity of moisture added.
              GFA(ICOMP)=(RHHC(ICOMP)*3600./real(NTSTEP)+RHS)/
     &                 (ZMDAF(ICOMP)+VCLF(ICOMP)*3600./real(2.*NTSTEP))
              ADDMOIST=RHHC(ICOMP)*3600./real(NTSTEP)
            ENDIF
C Enthalpy for final moisture content (J/kg) 
            ENTHCT=ENTHP2(TFA(ICOMP),GFA(ICOMP))*1000.
C Enthalpy if  humidification had not been applied
            ENTHUC=ENTHP2(TFA(ICOMP),GFA(ICOMP)
     &             -ADDMOIST/ZMDAF(ICOMP))*1000.
C Calculate the latent addition/extraction (W)
            ZLLDF(ICOMP)=(ENTHCT-ENTHUC)*ZMDAF(ICOMP)
     &                                  /(3600/real(NTSTEP))
          ENDIF
        ELSE
          ZLLDF(ICOMP)=0.0
        ENDIF
      ELSE
        ZLLDF(ICOMP)=0.0
      ENDIF

C Reset the relative humidity control flag.
      IRHCT(ICOMP)=0

C Calculate moisture content and temperature.
      GMA=(GPA(ICOMP)+GFA(ICOMP))*0.5
      IF(IDAVER.EQ.1)GMA=GFA(ICOMP)
      TMA=(TPA(ICOMP)+TFA(ICOMP))*.5
      IF(IDAVER.EQ.1)TMA=TFA(ICOMP)
      ZMA=(ZMDAP(ICOMP)+ZMDAF(ICOMP))*.5
      IF(IDAVER.EQ.1)ZMA=ZMDAF(ICOMP)

C Evaluate zone relative humidity and test for condensation.
      ZRH=PCRH2(TMA,GMA,PATMOS)
      CNDS=0.0
      XVAL=0.0
      IF(ZRH.GE.0.0.AND.ZRH.LE.100.0)goto 661

C Next check not really necessary.
      IF(ZRH.LT.0.0)THEN
        write(outs,'(A,I3,A,F7.2,a)')
     &  ' MZVAPC: for zone',ICOMP,' the computed relative humidity of',
     &  ZRH,' has been reset to zero.'
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        ZRH=0.0
        GOTO 661
      ENDIF

C Also check whether the computed moisture content needs to be reset.
       IF(GFA(ICOMP).LT.0.0) THEN
         WRITE(OUTS,'(A,I5,A,F7.2,a)')
     &   ' MZVAPC: for zone',ICOMP,' the computed moisture content of',
     &  GFA(ICOMP),' has been reset to zero.'
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        GFA(ICOMP)=0.0 
      ENDIF

C Check if condensation will occur if so then calculate the maximum possible
C moisture content and re-set the zone moisture content.
      ZRH=100.0
      G=HUMRT1(TMA,ZRH,PATMOS,IOPT)
      XVAL=GMA-G
      CNDS=XVAL*ZMA
      GFA(ICOMP)=HUMRT1(TFA(ICOMP),ZRH,PATMOS,IOPT)
  661 CONTINUE

C Determine the air node parial vapor pressure (to be used for moisture 
C modelling).
      IF(MSTROK)THEN
        Ppair(ICOMP)=Pfair(ICOMP)
        TCfair=TFA(ICOMP)
        Pfair(ICOMP)=ZRH/100.0*Psat01(TCfair)
      ENDIF

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999

      IF(ITRACE(10).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 9999

C Append computed results to early trace statements.
      write(outs,9997)ZMDAP(ICOMP),ZMDAF(ICOMP)
 9997 format(' ZMDAP = ',F14.6,' ZMDAF = ',F14.6)
      call edisp(itu,outs)
      write(outs,99971)GEXTP,GEXTF
99971 format(' GEXTP = ',F14.6,' GEXTF = ',F14.6)
      call edisp(itu,outs)
      write(outs,99972)VCIP(ICOMP),VCIF(ICOMP)
99972 format(' VCIP  = ',F14.6,' VCIF  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99973)VCCP(ICOMP),VCCF(ICOMP)
99973 format(' VCCP  = ',F14.6,' VCCF  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99974)VCMP(ICOMP),VCMF(ICOMP)
99974 format(' VCMP  = ',F14.6,' VCMF  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99975)VCLGP(ICOMP),VCLGF(ICOMP)
99975 format(' VCLGP = ',F14.6,' VCLGF = ',F14.6)
      call edisp(itu,outs)
      write(outs,99976)VCLP(ICOMP),VCLF(ICOMP)
99976 format(' VCLP  = ',F14.6,' VCLF  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99977)XVAL,CNDS
99977 format(' XVAL  = ',F14.6,' CNDS  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99978)TPA(ICOMP),TFA(ICOMP)
99978 format(' TPA   = ',F14.6,' TFA   = ',F14.6)
      call edisp(itu,outs)
      write(outs,99979)GPA(ICOMP),GFA(ICOMP),ZRH
99979 format(' GPA   = ',F14.6,' GFA   = ',F14.6,' ZRH   = ',F14.6)
      call edisp(itu,outs)
      write(outs,99980) RHSPTU(ICOMP)
99980 format(' RH set point upper: ',F14.6)
      call edisp(itu,outs)
      write(outs,99981) RHSPTL(ICOMP)
99981 format(' RH set point lower: ',F14.6)
      call edisp(itu,outs)
      write(outs,99982) RHHC(ICOMP)
99982 format(' Max humidification rate: ',F14.6)
      call edisp(itu,outs)
      write(outs,99983) RHDC(ICOMP)
99983 format(' Max dehumidification rate: ',F14.6)
      call edisp(itu,outs)
      write(outs,99984) ENTHUC,ENTHCT
99984 format(' ENTHUC  = ',F14.6,' ENTHCT  = ',F14.6)
      call edisp(itu,outs)
      write(outs,99985) ZLLDF(ICOMP)
99985 format( 'Zone latent load to maintain RH setpoint = ',F14.6)
      call edisp(itu,outs)

 9999 RETURN
      END

C ******************** NOTVENTAUX ********************
C NOTVENTAUX models the Auxiliary energy for the notional, reference and 
C typical building based on rules in the UK NCM guide (v4d March 2008)

      SUBROUTINE NOTVENTAUX(ICOMP)
      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "tdf2.h"
#include "sbem.h"

      logical closeto0
      integer itdi,icomp

      common/btime/btimep,btimef
      real btimep,btimef
      real VAL
      dimension VAL(MBITS+2)
      common/trc/itrc
      integer itrc
      integer ISD,IFOC,IER

C Auxiliary energy:
      common/ncmAuxilEnergy/zonAuxil(12,MCOM),m2zonAuxil(12,MCOM),
     &      fmonthAuxil_ncm_Energy(12,MCOM)
      real zonAuxil,m2zonAuxil,fmonthAuxil_ncm_Energy

      common/CountTimesteps/i_countsteps(mcom),numbStartTimesteps,
     &numTotTimstps
      integer i_countsteps
      integer numbStartTimesteps  !total number of start-up timesteps
      integer numTotTimstps !total number of timesteps including the start-up days

C Common to count for the number of timesteps that systems are in operatrion
C Used to calculate an NCM Auxiliary energy (post-processed in save level 6)
C It excludes start-up timesteps
      common/ncmOperHrSystem/iOperTimestep(MCOM),
     &       iMonthOperTimestep(12,MCOM)
      integer iOperTimestep,iMonthOperTimestep !iMonthOperTimestep is the total monthly timesteps
                                               !that heating is operational

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer imonth !current month at simulation time (to accumulate monthly results)
      integer idayn,im  
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      real ValueToCalc
      INTEGER iactvalue
      real SFP ! Specific fan power for notional and reference building for this system
      real MinAuxE ! Minimum Auxiliary energy W/m2 to reference 

      if(i_countsteps(icomp).ne.1)return
      do 1 im=1,12
        zonAuxil(im,icomp)=0.
        m2zonAuxil(im,icomp)=0.
  	fmonthAuxil_ncm_Energy(im,icomp)=0.
 1    continue
      iactvalue=theactivityindex(ICOMP)

C Initialise specific fan power
C If there is zero SFP in the stripped model there will be no SFP in the
C not/ref/typ models either. Otherwise use default of 2 and 3 respectively
      closeto0=.false.
      call eclose(SFPHS(IHLZ(ICOMP)),0.0,0.01,closeto0)
      if(closeto0)then
        SFP=0.
      else
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
          SFP=2.
        elseif(inoti.eq.3)then ! Typical model
          SFP=3.
        endif
      endif

C Set specific fan power based on ventilation system associated with
C each system
      if(IHLZ(ICOMP).eq.0)then ! No system linked to this zone
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
          SFP=0.
        elseif(inoti.eq.3)then ! Typical model
          SFP=0.
        endif
      ELSE
        if(IVENT(IHLZ(ICOMP)).EQ.0.OR.IVENT(IHLZ(ICOMP)).EQ.10)THEN
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
            SFP=0.
          elseif(inoti.eq.3)then ! Typical model
            SFP=0.
          endif
        elseif(IVENT(IHLZ(ICOMP)).EQ.1.OR.IVENT(IHLZ(ICOMP)).EQ.11)THEN
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
            SFP=2.
          elseif(inoti.eq.3)then ! Typical model
            SFP=3.
          endif
        elseif(IVENT(IHLZ(ICOMP)).EQ.2.OR.IVENT(IHLZ(ICOMP)).EQ.12)THEN
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
            SFP=1.2
          elseif(inoti.eq.3)then ! Typical model
            SFP=1.8
          endif
        elseif(IVENT(IHLZ(ICOMP)).EQ.3.OR.IVENT(IHLZ(ICOMP)).EQ.13)THEN
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
            SFP=0.8
          elseif(inoti.eq.3)then ! Typical model
            SFP=1.1
          endif
        ENDIF
        IF(IVENT(IHLZ(ICOMP)).GE.10)THEN ! If there are local ventilation units present
        if(inoti.eq.1.or. !notional
     &  (inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
            SFP=SFP+0.5
          elseif(inoti.eq.3)then ! Typical model
            SFP=SFP+0.75
          endif
        ENDIF
      endif

C Set minimum auxiliary energy for ac and mech vent systems
      if(inoti.eq.1.or. !notional
     &(inoti.eq.4.and.iDsmTestingFlag.eq.1))then ! or stripped with notional DSM testing flag
        MinAuxE=8.5
      elseif(inoti.eq.3)then ! Typical model
        MinAuxE=14.0
      endif

C Read TDF to get the fresh air requirements
      itrc=0
      itdi=IACTIV(ICOMP)
      IFOC=itdi
      CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
      ValueToCalc=TMAR(itdi,IATDF(itdi,1))*TMAR(itdi,IATDF(itdi,9))*SFP

C If no system associated with this zone then skip this code.
      if(IHLZ(ICOMP).eq.0)return

C If space is not heated assume no auxililary energy
      if(IBRUKC(IHLZ(ICOMP)).eq.0)then
        iOperTimestep(ICOMP)=0
        do 3 im=1,12
          zonAuxil(im,icomp)=0.
          m2zonAuxil(im,icomp)=0.
          fmonthAuxil_ncm_Energy(im,icomp)=0.
          iMonthOperTimestep(im,ICOMP)=0
 3      continue
      endif

      do 5000 imonth=1,12

C Calculate Auxiliary energy for notional/reference/typical buildings
C If it is not mechanically ventilated or this is the UK NCM reference 
C model (in which case cooling auxiliary energy is not to be calculated)
C or stripped model that has been assigned a reference model flag.
C This means heating operates and hour/timestep should be accounted
C 0.61 W/m^2 * area (and integrate over time) for notional and reference
C and 1.23 for typical model
C The calculated values will be in Watts
        if(IBRUKC(IHLZ(ICOMP)).eq.-1111)then         ! no cooling system for zone
          IF(INOTI.EQ.1.OR.INOTI.EQ.2.OR.            ! notional and reference
     &    (INOTI.eq.4.AND.iDsmTestingFlag.gt.0))THEN ! or stripped with DSM testing flag
            m2zonAuxil(imonth,ICOMP)=0.61*
     &      fMonthEstSysHrs(iactvalue,imonth)+m2zonAuxil(imonth,ICOMP)

            fmonthAuxil_ncm_Energy(imonth,icomp)=0.61*ZBASEA(ICOMP)*
     &      fMonthEstSysHrs(iactvalue,imonth)
          ELSEIF(INOTI.EQ.3)THEN ! Typical model
            m2zonAuxil(imonth,ICOMP)=1.23*
     &      fMonthEstSysHrs(iactvalue,imonth)+m2zonAuxil(imonth,ICOMP)

            fmonthAuxil_ncm_Energy(imonth,icomp)=1.23*ZBASEA(ICOMP)*
     &      fMonthEstSysHrs(iactvalue,imonth)
          ENDIF 
        else

C This will apply only for air-conditioned spaces. According to the 
C NCM guide (paragr. 42 & 88): we need to determine the greatest between
C fresh air requirement * SFP and 8.5 W/m2 (notional/reference buildings)
C AND fresh air requirement * SFP and 14.0 W/m2 (typical building)
          if(ValueToCalc.lt.MinAuxE)ValueToCalc=MinAuxE
        endif

C For systems with any active ventilation at all (mechanical ventilation
C without cooling or full air conditioning systems)
C Auxiliary energy is calculated in addition to the 0.61W/m2
C calculated above but not for the reference building because it always
C has radiators for heating and direct expansion cooling but fan power
C associated with cooling is taken into account in cooling load value
C (NCM Modelling Guide October 2008 version)
        IF(INOTI.EQ.2.OR.iDsmTestingFlag.EQ.2)THEN
          m2zonAuxil(imonth,ICOMP)=0.61*
     &    fMonthEstSysHrs(iactvalue,imonth)+m2zonAuxil(imonth,ICOMP)

          fmonthAuxil_ncm_Energy(imonth,icomp)=0.61*ZBASEA(ICOMP)*
     &    fMonthEstSysHrs(iactvalue,imonth)
        ELSE
          m2zonAuxil(imonth,ICOMP)=
     &    ValueToCalc*nonzerohours(iactvalue,1)/12.+
     &    m2zonAuxil(imonth,ICOMP)

          fmonthAuxil_ncm_Energy(imonth,icomp)=ValueToCalc*
     &    ZBASEA(ICOMP)*nonzerohours(iactvalue,1)/12.+
     &    fmonthAuxil_ncm_Energy(imonth,icomp)
        ENDIF

C zonAuxil=Occupants density (pers/m2)*fresh air requirements l/s/pers*
C         SpecificFanPower* zone Area
        zonAuxil(imonth,ICOMP)=m2zonAuxil(imonth,ICOMP)*ZBASEA(ICOMP) 
 5000 continue
      return
      end
