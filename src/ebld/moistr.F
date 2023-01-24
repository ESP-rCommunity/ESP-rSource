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


C This file contains the following subroutines.
C MZMSTR - Controller for combined heat and moisture transfer.
C INITMS - Initiates the whole system moisture variables.
C INITMZ - Initiates the zone moisture variables.
C MZMVPP - Sets the present vapor pressure distribution.
C MZMTMP - Sets the moisture nodes present temperatures by linear 
C          interpolation.
C MZMTMF - Sets the moisture nodes future temperatures by linear 
C          interpolation.
C MZMCFP - Sets the present moisture parameters.
C SETNEF - Updates the new future variables value.
C MSLINK - Sets the parameters for linking the moisture simulation with  
C          other modules (mfs, plt etc.).
C MZMCFF - Calculates the future moisture parameters.
C MZMSPF - Saves the future pressure distribution for convergence test.
C MZMSTU - Sets up the zone moisture transfer matrix.
C MZMITR - Solves the zone moisture matrix by Gauss Siedal method.
C MZMCHP - Checks for convergence in pressure values.
C MZMCHF - Checks for convergence in fluid values.
C SETFEN - Updates the future variables from n'th variables.
C MZCOE5 - Modifies the zone heat matrix to count for the heat of 
C          transformation (moisture transfer).
C HEATRF - Calculates the heat of transformation for pre-defined control
C          volume which is the summation of the heat of transformation of 
C          several moisture control volumes in whole or partion.
C HTRANF - Calculates the heat of transformation for a complete moisture
C          control volume.
C LYRPRP - Gets the thermophysical properties for a layer.
C WRITRH - Writes the relative humidity results.
C MSRTND
C CHKMST

C ******************** MZMSTR ********************
C Controller for combined heat and moisture transfer.

      SUBROUTINE MZMSTR(ICOMP)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      LOGICAL INTHPS,INTHPZ
      COMMON/MOIST11/MITRM,RITRMP,RITRMF
      COMMON/MOIST99/ICNVITRM(MCOM)

      LOGICAL CONVP,CONVF
      LOGICAL ILTHPS,ILTHPZ
      CHARACTER MSG*72

C Set the present vapour pressure distribution equal to the previuos time
C step future distribution.
      CALL MZMVPP(ICOMP)

C Estimate the present temperatures for moisture nodes by linear 
C interpolation.
      CALL MZMTMP(ICOMP)

C Set the present value for moisture parameters equal to the saved values.
      CALL MZMCFP(ICOMP)

C Set the parameters for linking the moisture simulation with other 
C modules in ESP-r (mfs, plt, ...etc).
      CALL MSLINK(ICOMP)

C Set the new variables values equal to the current future values.
      CALL SETNEF(ICOMP)

C Estimate the future temperatures for moisture nodes by linear 
C interpolation.
      CALL MZMTMF2(ICOMP)

      DO 10 ITRM=1,MITRM

C Estimate the new values for moisture parameters.
        CALL MZMCFF(ICOMP)

C Set up the moisture and fluid matrix.
        CALL MZMSTU(ICOMP)

C Solve the moisture and fluid balance matrix equation iteratively.
        CALL MZMITR(ICOMP)

C Check for convergence in pressure values.
        CALL MZMCHP(ICOMP,CONVP)

C Check for convergence in fluid values.
        IF(CONVP)CALL MZMCHF(ICOMP,CONVF)

C Set the future pressures equal to the new pressures.
        CALL SETFEN(ICOMP)

        IF(CONVP.AND.CONVF)THEN

C Convergence achieved.
          CALL WRITRH(ICOMP)
          IF(ITRM.EQ.1)THEN
            CALL EDAYR(IDYP,IDAYN,IMTHN)
            WRITE(MSG,'(1X,A)')
     &      'MZMSTR: moisture convergence after'
            CALL EDISP(IUOUT,MSG)
            WRITE(MSG,'(9X,A,I3,A,3I3)')
     &      '1 iteration in zone',ICOMP,
     &      ' at Day, Month, Hour: ',IDAYN,IMTHN,IHRF
            CALL EDISP(IUOUT,MSG)
          ENDIF

          ICNVITRM(ICOMP)=ITRM

          RETURN

C Re-establish and solve the energy matrix.
        ELSE

C Set the present values of thermal properties equal to the saved values 
C from the first iteration (since it is based on one time step lagging).
          IF(ITRM.EQ.1) CALL MTHPES(ICOMP)

C Estimate the future values for thermophysical properties.
          CALL MTHFEI(ICOMP)
          IF(ILTHPZ(ICOMP))CALL MZKOND(ICOMP)
          IF(INTHPZ(ICOMP))CALL MZNTHP(ICOMP)
          CALL MZTHSFP(ICOMP)

C Set up the energy matrix coefficients.
          CALL MZCOE4M(ICOMP)
          CALL MZSETU(ICOMP)
          CALL MZCOE5(ICOMP)

C Solve the energy matrix.
          CALL MTXCTL(ICOMP)
          CALL MZMTMF2(ICOMP)
        ENDIF   

   10 CONTINUE
      CALL WRITRH(ICOMP)
      CALL EDAYR(IDYP,IDAYN,IMTHN)
      WRITE(MSG,'(1X,A)')
     &     'MZMSTR: moisture '
      CALL EDISP(IUOUT,MSG)
      WRITE(MSG,'(9X,A,I3,A,3I3)')
     &      'convergence failed in zone',ICOMP,
     &      ' at Day, Month, Hour: ',IDAYN,IMTHN,IHRF
      CALL EDISP(IUOUT,MSG)
      ICNVITRM(ICOMP)=ITRM
      RETURN
      END

C ******************** INITMS ********************
C Initiates the whole system moisture variables.

      SUBROUTINE INITMS

      COMMON/MOIST11/MITRM,RITRMp,RITRMf
      COMMON/MOIST12/GAMm,RGAMm
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST14/DTfalse,TRMfls,TRMflsp1
      COMMON/MOIST17/RHmax

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      DTfalse=3600.0

      GAMm=0.5
      RLXln=0.4    !0.1  RLX p_new + (1-RLX) p_old ; FACTOR=1/RLX
      RLXlnF=0.2   !0.01 RLX F_new + (1-RLX) F_old ; FACTOR=1/RLX
      RLXlnT=0.4   !0.1  RLX T_new + (1-RLX) T_old

      MITRM=500

      RITRMP=0.075 ! (Pf-Pn) FACTOR < RITRMP ? => O.K. (both for zone air and layers!)
      RITRMF=0.001 ! (Ff-Fn) FACTOR < RITRMF ? => O.K. (both for zone air and layers!)

      RHmax=0.99
      RETURN
      END

C ******************** INITMZ ********************
C Initiates the zone moisture variables.

      SUBROUTINE INITMZ(ICOMP)
#include "building.h"
#include "site.h"
#include "moist.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST05/IPERM(MCOM,MS,ME),PERM(MCOM,MS,ME,3)
      COMMON/MOIST07/ITVAPD(MCOM,MS,ME),TVAPD(MCOM,MS,ME)
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST31/CPsair(MCOM),RHOsair(MCOM),Usair(MCOM)
      COMMON/MOIST25/BETAif(MCOM,MS),BETAof(MCOM,MS)
      COMMON/MOIST37/DELTs(MCOM,MS,MNM)
      COMMON/MOIST38/DTPs(MCOM,MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      character outs*124
      integer iMM

      TC=15.0
      RH=0.5
      Pinit=Pvpr02(TC,RH)
      Ppair(ICOMP)=Pinit
      Fpair(ICOMP)=0.0
      Spair(ICOMP)=0.0
      Pfair(ICOMP)=Pinit
      Ffair(ICOMP)=0.0
      Sfair(ICOMP)=0.0
      Xair=X03(TC,RH,atmpres)
      Usair(ICOMP)=Xair
      CPsair(ICOMP)=CPmst01(TC,Xair)
      RHOsair(ICOMP)=RHOdry01(TC)
      hc=10.0
      CP=CPsair(ICOMP)
      BETA=BETA01(hc,CP,TC)
      Sf(ICOMP)=0.0
      DO 10 I=1,NCONST(ICOMP)
        APp(ICOMP,I)=Ppair(ICOMP)
        APf(ICOMP,I)=Pfair(ICOMP)
        BETAif(ICOMP,I)=BETA
        BETAof(ICOMP,I)=BETA
        ITM1=0
        DO 20 IL=1,NELTS(ICOMP,I)
          NM=MNDL(ICOMP,I,IL)
          NMP1=NM+1
          DO IM=1,NMP1
            IF(IM.LT.NMP1)THEN
              ITM1=ITM1+1
              Pp(ICOMP,I,ITM1)=Pinit
              Fp(ICOMP,I,ITM1)=0.0
              Pf(ICOMP,I,ITM1)=Pinit
              Ff(ICOMP,I,ITM1)=0.0
              UM0=PERM(ICOMP,I,IL,1)
              UM100=PERM(ICOMP,I,IL,2)
              B=PERM(ICOMP,I,IL,3)
              iMM=iMoistModel(ICOMP)
              DX=DELT01(iMM,RH,UM0,UM100,B)

C If small value, report problem.
              if(DX.lt.0.99E-15)then
                write(outs,'(a,3i3)') 
     &           'INITMZ: small vapour perm. at zone/surf/layer: ',
     &           icomp,i,IL
                call edisp(iuout,outs)
                DELTs(ICOMP,I,ITM1)=DX
              else
                DELTs(ICOMP,I,ITM1)=DX
              endif
              DTPs(ICOMP,I,ITM1)=TVAPD(ICOMP,I,IL)
            ENDIF
          enddo
   20   CONTINUE
        ITM1=ITM1+1
        Pp(ICOMP,I,ITM1)=Pinit
        Fp(ICOMP,I,ITM1)=0.0
        Pf(ICOMP,I,ITM1)=Pinit
        Ff(ICOMP,I,ITM1)=0.0
   10 CONTINUE
      RETURN
      END

C ******************** MZMVPP ********************
C Set the present vapor pressure distribution.

      SUBROUTINE MZMVPP(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)

      Ppair(ICOMP)=Pfair(ICOMP)
      Fpair(ICOMP)=Ffair(ICOMP)
      Spair(ICOMP)=Sfair(ICOMP)
      Sp(ICOMP)=Sf(ICOMP)
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NM=MNDC(ICOMP,I)
      DO 10 IM=1,NM
        Pp(ICOMP,I,IM)=Pf(ICOMP,I,IM)
        Fp(ICOMP,I,IM)=Ff(ICOMP,I,IM)
   10 CONTINUE
      RETURN
      END

C ******************** MZMTMP ********************
C Sets the moisture nodes present temperatures by linear interpolation.

      SUBROUTINE MZMTMP(ICOMP)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST50/TMNp(MCOM,MS,MNM)

      character outs*124

      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NE=NELTS(ICOMP,I)
        NHT=NNDC(ICOMP,I)
        INMT=0

C For lumped constructions.
        IF(ILUMP(ICOMP,I).GE.1)THEN
          THICK=0.
          DO IL=1,NE
            THICK=THICK+THRMLI(ICOMP,I,IL,4)
          ENDDO
          DELXh=THICK/2.0
          XSUMM=0.
          DO 30 IL=1,NE

C Trap divition by zero (1.0 is a guessed alternative).
            THICK=THRMLI(ICOMP,I,IL,4)
            if(MNDL(ICOMP,I,IL).ne.0)then
              DELXm=THICK/FLOAT(MNDL(ICOMP,I,IL))
            else
              write(outs,*) 'MNDL out of range @',ICOMP,I,IL
              call edisp(iuout,outs)
              DELXm=THICK/1.0
            endif
            DO INM=1,MNDL(ICOMP,I,IL)
              INMT=INMT+1

C Trap for division by zero.
              if(DELXh.gt.0.0)then
                INH1=INT(XSUMM/DELXh)
              else
                write(outs,*) 'DELXh out of range @',ICOMP,I,IL
                call edisp(iuout,outs)
                INH1=INT(XSUMM/1.0)
              endif
              XINTP=XSUMM-DELXh*INH1
              INH1=INH1+1
              T1P=TPC(ICOMP,I,INH1)
              INH2=INH1+1
              IF(INH2.EQ.NHT)THEN
                T2P=TPS(ICOMP,I)
              ELSE
                T2P=TPC(ICOMP,I,INH2)
              ENDIF
              RATIO=XINTP/DELXh
              TMPp(I,INMT)=RATIO*(T2P-T1P)+T1P
              TMNp(ICOMP,I,INMT)=TMPp(I,INMT) ! save with zone info
              XSUMM=XSUMM+DELXm
            enddo
   30     CONTINUE
        ELSE
          IRN=1
          DO 40 IL=1,NE
            INMT=INMT+1
            TMPp(I,INMT)=TPC(ICOMP,I,IRN)
            TMNp(ICOMP,I,INMT)=TMPp(I,INMT) ! save with zone info
            THICK=THRMLI(ICOMP,I,IL,4)
            NNT=NNDL(ICOMP,I,IL)

C Trap zero division.
            if(NNT.gt.0)then
              DELXh=THICK/FLOAT(NNT)
            else
              write(outs,*) 'NNT out of range @',ICOMP,I,IL
              call edisp(iuout,outs)
              DELXh=THICK/FLOAT(1)
            endif
            NMT=MNDL(ICOMP,I,IL)
            if(NMT.gt.0)then
              DELXm=THICK/FLOAT(NMT)
            else
              write(outs,*) 'NMT out of range @',ICOMP,I,IL
              call edisp(iuout,outs)
              DELXm=THICK/FLOAT(1)
            endif
            XSUMM=0.
            DO 50 INM=2,MNDL(ICOMP,I,IL)
              INMT=INMT+1
              XSUMM=XSUMM+DELXm
              if(DELXh.gt.0.0)then
                INTRVL=INT(XSUMM/DELXh)
              else
                write(outs,*) 'DELXh out of range @',ICOMP,I,IL
                call edisp(iuout,outs)
                INTRVL=INT(XSUMM/1.0)
              endif
              XINTP=XSUMM-DELXh*INTRVL
              INH1=IRN+INTRVL
              T1P=TPC(ICOMP,I,INH1)
              INH2=INH1+1
              IF(INH2.EQ.NHT)THEN
                T2P=TPS(ICOMP,I)
              ELSE
                T2P=TPC(ICOMP,I,INH2)
              ENDIF
              RATIO=XINTP/DELXh
              TMPp(I,INMT)=RATIO*(T2P-T1P)+T1P
              TMNp(ICOMP,I,INMT)=TMPp(I,INMT) ! save with zone info
   50       CONTINUE
            IRN=IRN+NNDL(ICOMP,I,IL)
   40     CONTINUE
        ENDIF
        INMT=INMT+1
        TMPp(I,INMT)=TPS(ICOMP,I)
        TMNp(ICOMP,I,INMT)=TMPp(I,INMT) ! save with zone info
   10 CONTINUE
      RETURN
      END

C ******************** MZMTMF ********************
C Sets the moisture nodes future temperatures by linear interpolation.

      SUBROUTINE MZMTMF(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)

      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NE=NELTS(ICOMP,I)
        NHT=NNDC(ICOMP,I)
        INMT=0

C For lumped constructions.
        IF(ILUMP(ICOMP,I).GE.1)THEN
          THICK=0.
          DO 20 IL=1,NE
            THICK=THICK+THRMLI(ICOMP,I,IL,4)
   20     CONTINUE
          DELXh=THICK/2.0
          XSUMM=0.
          DO 30 IL=1,NE
            THICK=THRMLI(ICOMP,I,IL,4)
            DELXm=THICK/FLOAT(MNDL(ICOMP,I,IL))
          DO 30 INM=1,MNDL(ICOMP,I,IL)
            INMT=INMT+1
            INH1=INT(XSUMM/DELXh)
            XINTP=XSUMM-DELXh*INH1
            INH1=INH1+1
            T1F=TFC(ICOMP,I,INH1)
            INH2=INH1+1
            IF(INH2.EQ.NHT)THEN
              T2F=TFS(ICOMP,I)
            ELSE
              T2F=TFC(ICOMP,I,INH2)
            ENDIF
            RATIO=XINTP/DELXh
            TMPf(I,INMT)=RATIO*(T2F-T1F)+T1F
            XSUMM=XSUMM+DELXm
   30     CONTINUE
        ELSE
          IRN=1
          DO 40 IL=1,NE
            INMT=INMT+1
            TMPf(I,INMT)=TFC(ICOMP,I,IRN)
            THICK=THRMLI(ICOMP,I,IL,4)
            DELXh=THICK/FLOAT(NNDL(ICOMP,I,IL))
            DELXm=THICK/FLOAT(MNDL(ICOMP,I,IL))
            XSUMM=0.
            DO 50 INM=2,MNDL(ICOMP,I,IL)
              INMT=INMT+1
              XSUMM=XSUMM+DELXm
              INTRVL=INT(XSUMM/DELXh)
              XINTP=XSUMM-DELXh*INTRVL
              INH1=IRN+INTRVL
              T1F=TFC(ICOMP,I,INH1)
              INH2=INH1+1
              IF(INH2.EQ.NHT)THEN
                T2F=TFS(ICOMP,I)
              ELSE
                T2F=TFC(ICOMP,I,INH2)
              ENDIF
              RATIO=XINTP/DELXh
              TMPf(I,INMT)=RATIO*(T2F-T1F)+T1F
   50       CONTINUE
            IRN=IRN+NNDL(ICOMP,I,IL)
   40     CONTINUE
        ENDIF
        INMT=INMT+1
        TMPf(I,INMT)=TFS(ICOMP,I)
   10 CONTINUE
      RETURN
      END

C ******************** MZMTMF2 ********************
C Sets the moisture nodes future temperatures by linear interpolation.

      SUBROUTINE MZMTMF2(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)

      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NE=NELTS(ICOMP,I)
        NHT=NNDC(ICOMP,I)
        INMT=0

C For lumped constructions.
        IF(ILUMP(ICOMP,I).GE.1)THEN
          THICK=0.
          DO IL=1,NE
            THICK=THICK+THRMLI(ICOMP,I,IL,4)
          ENDDO
          DELXh=THICK/2.0
          XSUMM=0.
          DO 30 IL=1,NE
            THICK=THRMLI(ICOMP,I,IL,4)
            DELXm=THICK/FLOAT(MNDL(ICOMP,I,IL))
            DO INM=1,MNDL(ICOMP,I,IL)
              INMT=INMT+1
              INH1=INT(XSUMM/DELXh)
              XINTP=XSUMM-DELXh*INH1
              INH1=INH1+1
              T1F=TFC(ICOMP,I,INH1)
              INH2=INH1+1
              IF(INH2.EQ.NHT)THEN
                T2F=TFS(ICOMP,I)
              ELSE
                T2F=TFC(ICOMP,I,INH2)
              ENDIF
              RATIO=XINTP/DELXh
              TMPnew=RATIO*(T2F-T1F)+T1F
              TMPf(I,INMT)=RLXlnT*TMPnew+RRLXlnT*TMPf(I,INMT)
              XSUMM=XSUMM+DELXm
            ENDDO
   30     CONTINUE
        ELSE
          IRN=1
          DO 40 IL=1,NE
            INMT=INMT+1
            TMPnew=TFC(ICOMP,I,IRN)
            TMPf(I,INMT)=RLXlnT*TMPnew+RRLXlnT*TMPf(I,INMT)
            THICK=THRMLI(ICOMP,I,IL,4)
            DELXh=THICK/FLOAT(NNDL(ICOMP,I,IL))
            DELXm=THICK/FLOAT(MNDL(ICOMP,I,IL))
            XSUMM=0.
            DO 50 INM=2,MNDL(ICOMP,I,IL)
              INMT=INMT+1
              XSUMM=XSUMM+DELXm
              INTRVL=INT(XSUMM/DELXh)
              XINTP=XSUMM-DELXh*INTRVL
              INH1=IRN+INTRVL
              T1F=TFC(ICOMP,I,INH1)
              INH2=INH1+1
              IF(INH2.EQ.NHT)THEN
                T2F=TFS(ICOMP,I)
              ELSE
                T2F=TFC(ICOMP,I,INH2)
              ENDIF
              RATIO=XINTP/DELXh
              TMPnew=RATIO*(T2F-T1F)+T1F
              TMPf(I,INMT)=RLXlnT*TMPnew+RRLXlnT*TMPf(I,INMT)
   50       CONTINUE
            IRN=IRN+NNDL(ICOMP,I,IL)
   40     CONTINUE
        ENDIF
        INMT=INMT+1
        TMPnew=TFS(ICOMP,I)
        TMPf(I,INMT)=RLXlnT*TMPnew+RRLXlnT*TMPf(I,INMT)
   10 CONTINUE
      RETURN
      END

C ******************** MZMCFP ********************
C Sets the present moisture parameters.

      SUBROUTINE MZMCFP(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST21/CPpair,CPfair,RHOpair,RHOfair,Upair,Ufair
      COMMON/MOIST24/BETAip(MCOM,MS),BETAop(MCOM,MS)
      COMMON/MOIST25/BETAif(MCOM,MS),BETAof(MCOM,MS)
      COMMON/MOIST27/DELTp(MS,MNM),DELTf(MS,MNM)
      COMMON/MOIST28/DTPp(MS,MNM),DTPf(MS,MNM)
      COMMON/MOIST31/CPsair(MCOM),RHOsair(MCOM),Usair(MCOM)
      COMMON/MOIST37/DELTs(MCOM,MS,MNM)
      COMMON/MOIST38/DTPs(MCOM,MS,MNM)

C Set the present time heat capacity, density, moisture content, and
C moisture storage term for air.
      CPpair=CPsair(ICOMP)
      RHOpair=RHOsair(ICOMP)
      Upair=Usair(ICOMP)

C Set the present time convective mass transfer coefficients.
      NC=NCONST(ICOMP)
      DO I=1,NC
        BETAip(ICOMP,I)=BETAif(ICOMP,I)
        BETAop(ICOMP,I)=BETAof(ICOMP,I)
      ENDDO

C For construction moisture nodes.
      DO 20 I=1,NC
        NE=NELTS(ICOMP,I)
        ITM=0
        DO 30 IL=1,NE
          NM=MNDL(ICOMP,I,IL)
          DO IM=1,NM
            ITM=ITM+1
            DELTp(I,ITM)=DELTs(ICOMP,I,ITM)
            DTPp(I,ITM)=DTPs(ICOMP,I,ITM)
          ENDDO
   30   CONTINUE
   20 CONTINUE
      RETURN
      END

C ******************** SETNEF ********************
C Update the new future variables value.

      SUBROUTINE SETNEF(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

C Update the values for moisture new future parameters.
      Pnair=Pfair(ICOMP)
      Snair=Sfair(ICOMP)
      Fnair=Ffair(ICOMP)
      Sn=Sf(ICOMP)
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        DO IM=1,MNDC(ICOMP,I)
          Pn(I,IM)=Pf(ICOMP,I,IM)
          Fn(I,IM)=Ff(ICOMP,I,IM)
        ENDDO
   10 CONTINUE
      RETURN
      END

C ******************** MSLINK ********************
C Set the parameters for linking the moisture simulation with  
C other modules (mfs, plt etc.).

      SUBROUTINE MSLINK(ICOMP)
#include "building.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow

      COMMON/MBINFO/ZMBI(MCOM,4)
      COMMON/PREC6L/TQL1(MCOM,MT),TQL2(MCOM,MT),TQL3(MCOM,MT)
      COMMON/COE34L/QLAT(MCOM)
      COMMON/VAPC1/VCIP(MCOM),VCIF(MCOM),VCCP(MCOM),VCCF(MCOM),
     &VCLP(MCOM),VCLF(MCOM),VCMP(MCOM),VCMF(MCOM),VCLGP(MCOM),
     &VCLGF(MCOM)
      COMMON/MOIST66/VPINF,VPCPL,VPLST,VPMCH,VPLTN
      COMMON/MOIST67/VFINF,VFCPL,VFLST,VFMCH,VFLTN


C Link the building lumped moisture flow network.
C Modify the boundary vector to account for:
C VPINF*GEXTP infiltration
C VPCPL       zone coupling
C VPMCH       mechanical system gain
C VPLTN       latent gains
C VPLST       lost moisture from the zone
      VCIP(ICOMP)=VCIF(ICOMP)
      VPINF=VCIP(ICOMP)
      VCCP(ICOMP)=VCCF(ICOMP)
      VPCPL=VCCP(ICOMP)
      VCLP(ICOMP)=VCLF(ICOMP)
      VPLST=VCLP(ICOMP)
      VCMP(ICOMP)=VCMF(ICOMP)
      VPMCH=VCMF(ICOMP)
      VCLGP(ICOMP)=VCLGF(ICOMP)
      VPLTN=VCLGP(ICOMP)

C Infiltration mass flow rate only.
      VFINF=ZMBI(ICOMP,1)
      VCIF(ICOMP)=VFINF

C Zone coupling mass flow rate * moisture content.
      VFCPL=ZMBI(ICOMP,2)
      VCCF(ICOMP)=VFCPL

C Enter gain due to mechanical system here
C mechanical system mass flow rate * moisture content.
      VFMCH=ZMBI(ICOMP,3)
      VCMF(ICOMP)=VFMCH

C Total mass flow rate lost from the zone (no moisture included).
C The future moisture flow rate is dependent on the future air node
C moisture content (vapor pressure).
      VFLST=ZMBI(ICOMP,4)
      VCLF(ICOMP)=VFLST

C << this might be a place where latent gains from temporal
C << casual gains need to be taken into account?
C Latent gains expressed as a moisture (assuming 20 C) addition.
      IF(IDWF.EQ.6)THEN
        QL=TQL2(ICOMP,IHRF)
      ELSEIF(IDWF.EQ.7)THEN
        QL=TQL3(ICOMP,IHRF)
      ELSE
        QL=TQL1(ICOMP,IHRF)
      ENDIF
      VFLTN=QL/2454000.0
      VCLGF(ICOMP)=VFLTN
      RETURN
      END

C ******************** MZMCFF ********************
C Calculates the future moisture parameters.

      SUBROUTINE MZMCFF(ICOMP)
#include "building.h"
#include "site.h"
#include "geometry.h"
#include "moist.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &HCOF(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST05/IPERM(MCOM,MS,ME),PERM(MCOM,MS,ME,3)
      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      COMMON/MOIST07/ITVAPD(MCOM,MS,ME),TVAPD(MCOM,MS,ME)
      COMMON/MOIST17/RHmax
      COMMON/MOIST21/CPpair,CPfair,RHOpair,RHOfair,Upair,Ufair
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST25/BETAif(MCOM,MS),BETAof(MCOM,MS)
      COMMON/MOIST26/XIpfair,RXIVpf(MS,MNM)
      COMMON/MOIST27/DELTp(MS,MNM),DELTf(MS,MNM)
      COMMON/MOIST28/DTPp(MS,MNM),DTPf(MS,MNM)
      COMMON/MOIST31/CPsair(MCOM),RHOsair(MCOM),Usair(MCOM)
      COMMON/MOIST37/DELTs(MCOM,MS,MNM)
      COMMON/MOIST38/DTPs(MCOM,MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

      integer iMM
      DOUBLE PRECISION XI03
      character outs*124

C Set moisture model for this zone.
      iMM=iMoistModel(ICOMP)

C Calculate the future time vapour ratio for zone air.
      TCfair=TFA(ICOMP)
      if(Psat01(TCfair).ne.0.0)then
        RH=Pnair/Psat01(TCfair)
      else
        write(outs,*) 'Psat01(TCfair) returning zero',TCfair
        call edisp(iuout,outs)
      endif
      Ufair=X03(TCfair,RH,atmpres)
      Usair(ICOMP)=Ufair

C Calculate the future time density for dry zone air.
      RHOfair=RHOdry01(TCfair)
      RHOsair(ICOMP)=RHOfair

C Calculate the future time heat capacity for zone air.
      CPfair=CPmst01(TCfair,Ufair)
      CPsair(ICOMP)=CPfair

C Calculate the moisture capacity for the zone air.
      Pvp=Ppair(ICOMP)
      Pvf=Pfair(ICOMP)
      TCp=TPA(ICOMP)
      TCf=TFA(ICOMP)
      XIpfair=XIair01(Pvp,Pvf,TCp,TCf,atmpres)

C Estimate the future time convective mass transfer coefficients.
      NC=NCONST(ICOMP)
      DO 20 I=1,NC

C Internal construction surface, TCfair is future zone air temp.
        ALPHA=HCIF(ICOMP,I)
        BETAif(ICOMP,I)=BETA01(ALPHA,CPfair,TCfair)

C External construction surface.
        ALPHA=HCOF(ICOMP,I)
        TCf=ATF(ICOMP,I) ! future adjacent temperature
        if(Psat01(TCf).ne.0.0)then
          RH=APf(ICOMP,I)/Psat01(TCf)
        else
          write(outs,*)
     &      'Psat01(TCf) for zone ',ICOMP,
     &      ', surface ',I,' returning zero ',TCf
          call edisp(iuout,outs)
        endif
        AUfair=X03(TCf,RH,atmpres)
        ACPfair=CPmst01(TCf,AUfair)
        BETAof(ICOMP,I)=BETA01(ALPHA,ACPfair,TCf)
   20 CONTINUE

C For construction moisture nodes.
      DO 30 I=1,NC
        Asrf=SNA(ICOMP,I)
        NE=NELTS(ICOMP,I)
        ITM1=0
        DELX1=0.
        RXI1=0.
        DO 40 IL=1,NE
          NM=MNDL(ICOMP,I,IL)
          THCK=THRMLI(ICOMP,I,IL,4)
          if(NM.gt.0)then
            DELXm=THCK/NM
          else
            DELXm=THCK/1.0
          endif
          NMP1=NM+1
          DO 50 IM=1,NMP1

C Estimate the future time water vapor permeability.
            IF(IM.LT.NMP1)THEN
              ITM1=ITM1+1
              ITM2=ITM1+1
              TCf1=TMPf(I,ITM1)
              if(Psat01(TCf1).ne.0.0)then
                RH1=Pn(I,ITM1)/Psat01(TCf1)
              else
                write(outs,*) 'Psat01(TCf1) returning zero ',TCf1
                call edisp(iuout,outs)
              endif
              IF(RH1.GT.RHmax)THEN
                RH1=RHmax
                Pn(I,ITM1)=RH1*Psat01(TCf1)
              ENDIF
              TCp1=TMPp(I,ITM1)
              if(Psat01(TCp1).ne.0.0)then
                RH1p=Pp(ICOMP,I,ITM1)/Psat01(TCp1)
              else
                write(outs,*) 'Psat01(TCp1) returning zero.',TCp1
                call edisp(iuout,outs)
              endif
              UM0=PERM(ICOMP,I,IL,1)
              UM100=PERM(ICOMP,I,IL,2)
              B=PERM(ICOMP,I,IL,3)

C Check for small return values and identify source. If permeability
C values are small then probably glass so just set dELTf1.
              if((UM0.lt.1.0e-12).or.UM100.lt.1.0e-12)then
                DELTf1=1.0e-12
              else
                DELTf1=DELT01(iMM,RH1,UM0,UM100,B)
                if(DELTf1.lt.1.0E-14)then
                  write(outs,'(a,3i3)') 
     &            'MZMCFF DELTf1 small at zone surf layer: ',icomp,i,IL
                  call edisp(iuout,outs)
                endif
              endif
              TCf2=TMPf(I,ITM2)
              if(Psat01(TCf2).ne.0.0)then
                RH2=Pn(I,ITM2)/Psat01(TCf2)
              else
                write(outs,*) 'Psat01(TCf2) returning zero.',TCf2
                call edisp(iuout,outs)
              endif
              IF(RH2.GT.RHmax)THEN
                RH2=RHmax
                Pn(I,ITM2)=RH2*Psat01(TCf2)
              ENDIF

C Check for small return values and identify source.
              if((UM0.lt.1.0e-12).or.UM100.lt.1.0e-12)then
                DELTf2=1.0e-12
              else
                DELTf2=DELT01(iMM,RH2,UM0,UM100,B)
                if(DELTf2.lt.1.0E-14)then
                  write(outs,'(a,3i3)') 
     &            'MZMCFF DELTf2 small at zone surf layer: ',icomp,i,IL
                  call edisp(iuout,outs)
                endif
              endif

C Step-wise profile for water vapour permeability (DELX's cancel out).
              if((DELTf1+DELTf2).ne.0.0)then
                DELTf(I,ITM1)=2.*DELTf1*DELTf2/(DELTf1+DELTf2)
              else
                write(outs,*) 'DELTf1+DELTf2 is zero.',DELTf1,DELTf2
                call edisp(iuout,outs)
              endif
              DELTs(ICOMP,I,ITM1)=DELTf(I,ITM1)

C Estimate the future time thermal vapour diffusion coefficient.
              DTPf(I,ITM1)=TVAPD(ICOMP,I,IL)
              DTPs(ICOMP,I,ITM1)=DTPf(I,ITM1)
            ELSE
              RH1=RH2
              TCp1=TMPp(I,ITM1)
              if(Psat01(TCp1).ne.0.0)then
                RH1p=Pp(ICOMP,I,ITM1)/Psat01(TCp1)
              else
                write(outs,*) 'Psat01(TCp1) returning zero.',TCp1
                call edisp(iuout,outs)
              endif
            ENDIF

C Estimate the future time moisture storage capacity term.
            Uh=SORP(ICOMP,I,IL,1)
            CFF=SORP(ICOMP,I,IL,2)
            EXPT=SORP(ICOMP,I,IL,3)
            RXI=SNGL(XI03(iMM,RH1p,RH1,Uh,CFF,EXPT)) ! kg/kg in current moisture node
            IF(IM.EQ.1)THEN    ! outermost moisture node of material layer IL
              DELX2=DELXm/2.0  ! m (thickness of moisture node "layer")
              RXI2=RXI
              RXIVpf(I,ITM1)=Asrf*(DELX1*RXI1+DELX2*RXI2)
            ELSEIF(IM.EQ.NMP1)THEN ! innermost moisture node of material layer IL
              DELX1=DELXm/2.0
              RXI1=RXI
            ELSE ! all intermediate nodes in material layer IL
              RXIVpf(I,ITM1)=Asrf*DELXm*RXI ! kg in surf I and moisture node ITM1
            ENDIF
   50     CONTINUE  ! loop IM (moisture nodes)
   40   CONTINUE    ! loop IL (material layer nodes)

C Estimate the future time moisture capacity for surface node.
        ITM1=ITM1+1
        RXIVpf(I,ITM1)=Asrf*DELX1*RXI1
   30 CONTINUE ! loop I (surface in zone)
      RETURN
      END

C ******************** MZMSPF ********************
C Save the future pressure distribution for convergence test.

      SUBROUTINE MZMSPF(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

      Pnair=Pfair(ICOMP)
      Fnair=Ffair(ICOMP)
      Snair=Sfair(ICOMP)
      Sn=Sf(ICOMP)
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NM=MNDC(ICOMP,I)
        DO IM=1,NM
          Pn(I,IM)=Pf(ICOMP,I,IM)
          Fn(I,IM)=Ff(ICOMP,I,IM)
        ENDDO
   10 CONTINUE
      RETURN
      END

C ******************** MZMSTU ********************
C Sets up the zone moisture transfer matrix including effect of
C node temperatured distribution. Implicitness degrees between 0 and 1
C inclusive are allowed.

      SUBROUTINE MZMSTU(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/GR3D150/INDX3,DTIME
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST12/GAMm,RGAMm
      COMMON/MOIST14/DTfalse,TRMfls,TRMflsp1
      COMMON/MOIST21/CPpair,CPfair,RHOpair,RHOfair,Upair,Ufair
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST24/BETAip(MCOM,MS),BETAop(MCOM,MS)
      COMMON/MOIST25/BETAif(MCOM,MS),BETAof(MCOM,MS)
      COMMON/MOIST26/XIpfair,RXIVpf(MS,MNM)
      COMMON/MOIST27/DELTp(MS,MNM),DELTf(MS,MNM)
      COMMON/MOIST28/DTPp(MS,MNM),DTPf(MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair
      COMMON/MOIST66/VPINF,VPCPL,VPLST,VPMCH,VPLTN
      COMMON/MOIST67/VFINF,VFCPL,VFLST,VFMCH,VFLTN

      common/trcm01/ep(MS),ef(MS),TERMcpl
      character outs*124

C Consider each multilayered construction within current zone.
      SUMA1=0.
      SUMA2=0.
      SUMA3=0.
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        Asrf=SNA(ICOMP,I)
        IL=1
        IM=1
        IM2=IM+1
        if(MNDL(ICOMP,I,IL).ne.0)then
          DELXm=THRMLI(ICOMP,I,IL,4)/FLOAT(MNDL(ICOMP,I,IL))
        else
          DELXm=THRMLI(ICOMP,I,IL,4)/1.0
        endif

C Connection with outside.
        PP1=APp(ICOMP,I)
        PP2=APf(ICOMP,I)

C Connection with outside, another zone, or similar.
        CRSp1=RGAMm*BETAop(ICOMP,I)*Asrf*DTIME
        CRSf1= GAMm*BETAof(ICOMP,I)*Asrf*DTIME

C Connection with the inner node.
        TERM=Asrf*DTIME/DELXm
        CRSp2=RGAMm*TERM*DELTp(I,IM)
        CRSf2= GAMm*TERM*DELTf(I,IM)

C Estimate the present and future saturated water vapour pressure.
        TC=TMPp(I,IM)
        Psp=Psat01(TC)
        TC=TMPf(I,IM)
        Psf=Psat01(TC)
        if(Psp.ne.0.0)then
          SLFp=RXIVpf(I,IM)/Psp
        else
          write(outs,*) 'Psat01(TC) is zero ',TC
          call edisp(iuout,outs)
        endif
        if(Psf.ne.0.0)then
          SLFf=RXIVpf(I,IM)/Psf
        else
          write(outs,*) 'Psat01(TC) is zero ',TC
          call edisp(iuout,outs)
        endif
        DTPp2=RGAMm*DTPp(I,IM)*TERM
        DTPf2= GAMm*DTPf(I,IM)*TERM
        W(I,IM,1)=0.0
        W(I,IM,2)=TRMflsp1*SLFf+CRSf1+CRSf2
        W(I,IM,3)=-CRSf2
        W(I,IM,4)=-GAMm*DTIME
        W(I,IM,5)=(SLFp-CRSp1-CRSp2)*Pp(ICOMP,I,IM)+
     &  CRSp1*PP1+CRSp2*Pp(ICOMP,I,IM2)+CRSf1*PP2+Fp(ICOMP,I,IM)+
     &  DTPp2*(TMPp(I,IM2)-TMPp(I,IM))+DTPf2*(TMPf(I,IM2)-TMPf(I,IM))+
     &  TRMfls*SLFf*Pn(I,IM)

C Continue with other nodes (homogeneous) in the first layer.
        DO 100 ID=1,MNDL(ICOMP,I,IL)-1
          IM=IM+1
          IM1=IM-1
          IM2=IM+1
          TERM=Asrf*DTIME/DELXm
          CRSp1=RGAMm*TERM*DELTp(I,IM1)
          CRSf1= GAMm*TERM*DELTf(I,IM1)
          CRSp2=RGAMm*TERM*DELTp(I,IM)
          CRSf2= GAMm*TERM*DELTf(I,IM)
          TC=TMPp(I,IM)
          Psp=Psat01(TC)
          TC=TMPf(I,IM)
          Psf=Psat01(TC)
          if(Psp.ne.0.0)then
            SLFp=RXIVpf(I,IM)/Psp
          else
            write(outs,*) ' Psat01(TC) returns zero ',TC
            call edisp(iuout,outs)
          endif
          if(Psf.ne.0.0)then
            SLFf=RXIVpf(I,IM)/Psf
          else
            write(outs,*) ' Psat01(TC) returns zero ',TC
            call edisp(iuout,outs)
          endif
          DTPp1=RGAMm*DTPp(I,IM1)*TERM
          DTPf1= GAMm*DTPf(I,IM1)*TERM
          DTPp2=RGAMm*DTPp(I,IM)*TERM
          DTPf2= GAMm*DTPf(I,IM)*TERM
          W(I,IM,1)=-CRSf1
          W(I,IM,2)=TRMflsp1*SLFf+CRSf1+CRSf2
          W(I,IM,3)=-CRSf2
          W(I,IM,4)=-GAMm*DTIME
          W(I,IM,5)=(SLFp-CRSp1-CRSp2)*Pp(ICOMP,I,IM)+
     &    CRSp1*Pp(ICOMP,I,IM1)+CRSp2*Pp(ICOMP,I,IM2)+Fp(ICOMP,I,IM)+
     &    DTPp1*(TMPp(I,IM1)-TMPp(I,IM))+DTPf1*(TMPf(I,IM1)-TMPf(I,IM))+
     &    DTPp2*(TMPp(I,IM2)-TMPp(I,IM))+DTPf2*(TMPf(I,IM2)-TMPf(I,IM))+
     &    TRMfls*SLFf*Pn(I,IM)
  100   CONTINUE

C Continue with other layers in the construction.
        DO 110 IL=2,NELTS(ICOMP,I)
          IM=IM+1
          IM1=IM-1
          IM2=IM+1

C IL: current layer, IP: previous layer.
          IP=IL-1
          DELXl=THRMLI(ICOMP,I,IL,4)/FLOAT(MNDL(ICOMP,I,IL))
          DELXp=THRMLI(ICOMP,I,IP,4)/FLOAT(MNDL(ICOMP,I,IP))
          TERMP=Asrf*DTIME/DELXp
          CRSp1=RGAMm*TERMP*DELTp(I,IM1)
          CRSf1= GAMm*TERMP*DELTf(I,IM1)
          TERML=Asrf*DTIME/DELXl
          CRSp2=RGAMm*TERML*DELTp(I,IM)
          CRSf2= GAMm*TERML*DELTf(I,IM)
          TC=TMPp(I,IM)
          Psp=Psat01(TC)
          TC=TMPf(I,IM)
          Psf=Psat01(TC)
          SLFp=RXIVpf(I,IM)/Psp
          SLFf=RXIVpf(I,IM)/Psf
          DTPp1=RGAMm*DTPp(I,IM1)*TERMP
          DTPf1= GAMm*DTPf(I,IM1)*TERMP
          DTPp2=RGAMm*DTPp(I,IM)*TERML
          DTPf2= GAMm*DTPf(I,IM)*TERML
          W(I,IM,1)=-CRSf1
          W(I,IM,2)=TRMflsp1*SLFf+CRSf1+CRSf2
          W(I,IM,3)=-CRSf2
          W(I,IM,4)=-GAMm*DTIME
          W(I,IM,5)=(SLFp-CRSp1-CRSp2)*Pp(ICOMP,I,IM)+
     &    CRSp1*Pp(ICOMP,I,IM1)+CRSp2*Pp(ICOMP,I,IM2)+Fp(ICOMP,I,IM)+
     &    DTPp1*(TMPp(I,IM1)-TMPp(I,IM))+DTPf1*(TMPf(I,IM1)-TMPf(I,IM))+
     &    DTPp2*(TMPp(I,IM2)-TMPp(I,IM))+DTPf2*(TMPf(I,IM2)-TMPf(I,IM))+
     &    TRMfls*SLFf*Pn(I,IM)

C Continue with other (homogeneous) nodes in the layer.
          DELXm=DELXl
        DO 110 ID=1,MNDL(ICOMP,I,IL)-1
          IM=IM+1
          IM1=IM-1
          IM2=IM+1
          TERM=Asrf*DTIME/DELXm
          CRSp1=RGAMm*TERM*DELTp(I,IM1)
          CRSf1= GAMm*TERM*DELTf(I,IM1)
          CRSp2=RGAMm*TERM*DELTp(I,IM)
          CRSf2= GAMm*TERM*DELTf(I,IM)
          TC=TMPp(I,IM)
          Psp=Psat01(TC)
          TC=TMPf(I,IM)
          Psf=Psat01(TC)
          SLFp=RXIVpf(I,IM)/Psp
          SLFf=RXIVpf(I,IM)/Psf
          DTPp1=RGAMm*DTPp(I,IM1)*TERM
          DTPf1= GAMm*DTPf(I,IM1)*TERM
          DTPp2=RGAMm*DTPp(I,IM)*TERM
          DTPf2= GAMm*DTPf(I,IM)*TERM
          W(I,IM,1)=-CRSf1
          W(I,IM,2)=TRMflsp1*SLFf+CRSf1+CRSf2
          W(I,IM,3)=-CRSf2
          W(I,IM,4)=-GAMm*DTIME
          W(I,IM,5)=(SLFp-CRSp1-CRSp2)*Pp(ICOMP,I,IM)+
     &    CRSp1*Pp(ICOMP,I,IM1)+CRSp2*Pp(ICOMP,I,IM2)+Fp(ICOMP,I,IM)+
     &    DTPp1*(TMPp(I,IM1)-TMPp(I,IM))+DTPf1*(TMPf(I,IM1)-TMPf(I,IM))+
     &    DTPp2*(TMPp(I,IM2)-TMPp(I,IM))+DTPf2*(TMPf(I,IM2)-TMPf(I,IM))+
     &    TRMfls*SLFf*Pn(I,IM)
  110   CONTINUE

C Internal surface node
        IM=IM+1
        IM1=IM-1
        IL=NELTS(ICOMP,I)
        TERM=Asrf*DTIME/DELXm
        CRSp1=RGAMm*TERM*DELTp(I,IM1)
        CRSf1= GAMm*TERM*DELTf(I,IM1)
        CRSp2=RGAMm*Asrf*DTIME*BETAip(ICOMP,I)
        CRSf2= GAMm*Asrf*DTIME*BETAif(ICOMP,I)
        TC=TMPp(I,IM)
        Psp=Psat01(TC)
        TC=TMPf(I,IM)
        Psf=Psat01(TC)
        SLFp=RXIVpf(I,IM)/Psp
        SLFf=RXIVpf(I,IM)/Psf
        DTPp1=RGAMm*DTPp(I,IM1)*TERM
        DTPf1= GAMm*DTPf(I,IM1)*TERM
        W(I,IM,1)=-CRSf1
        W(I,IM,2)=TRMflsp1*SLFf+CRSf1+CRSf2
        W(I,IM,3)=-CRSf2
        W(I,IM,4)=-GAMm*DTIME
        W(I,IM,5)=(SLFp-CRSp1-CRSp2)*Pp(ICOMP,I,IM)+
     &  CRSp1*Pp(ICOMP,I,IM1)+CRSp2*Ppair(ICOMP)+Fp(ICOMP,I,IM)+
     &  DTPp1*(TMPp(I,IM1)-TMPp(I,IM))+DTPf1*(TMPf(I,IM1)-TMPf(I,IM))+
     &  TRMfls*SLFf*Pn(I,IM)

C Convective term for the air node equation.
        E(1,I)=-CRSf2
        SUMA1=SUMA1+CRSf2
        SUMA2=SUMA2-CRSp2
        SUMA3=SUMA3+CRSp2*Pp(ICOMP,I,IM)

        ef(I)=CRSf2
        ep(I)=CRSp2
   10 CONTINUE

C Complete the air node equation. 1 - NC: convective terms,
C INA:air furture self coupling, IQA:plant term, IPA:present term
      INA=NC+1
      IQA=INA+1
      IPA=IQA+1
      TCpair=TPA(ICOMP)
      Psp=Psat01(TCpair)
      TCfair=TFA(ICOMP)
      Psf=Psat01(TCfair)
      SLFp=RHOpair*XIpfair*VOL(ICOMP)/Psp
      SLFf=RHOfair*XIpfair*VOL(ICOMP)/Psf
      E(1,INA)=TRMflsp1*SLFf+SUMA1
      E(1,IQA)=-GAMm*DTIME
      E(1,IPA)=(SLFp+SUMA2)*Ppair(ICOMP)+SUMA3+Fpair(ICOMP)+
     &TRMfls*SLFf*Pnair

C Link the building lumped moisture flow network.
C Modify the boundary vector to count for;
      TERMcpl=(RGAMm*(VPINF*GEXTP+VPCPL+VPMCH+VPLTN-Upair*VPLST)+
     &        GAMm*(VFINF*GEXTF+VFCPL+VFMCH+VFLTN-Ufair*VFLST))*DTIME
      E(1,IPA)=E(1,IPA)+TERMcpl
      RETURN
      END

C ******************** MZMITR ********************
C Solves the zone moisture matrix by Gauss Seidel method.

      SUBROUTINE MZMITR(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST17/RHmax
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      CHARACTER*72 MSG

      DOUBLE PRECISION SUM,Pnew

      NC=NCONST(ICOMP)
      INA=NC+1
      IQA=INA+1
      IPA=IQA+1

C Construction moisture nodes.
      DO 20 I=1,NC
        MNC=MNDC(ICOMP,I)
        DO IM=1,MNC
          Pnsave=Pn(I,IM)
          Fnsave=Fn(I,IM)
          IF(IM.EQ.1)THEN
            Pn1=0.
          ELSE
            IM1=IM-1
            Pn1=Pn(I,IM1)
          ENDIF
          IF(IM.EQ.MNC)THEN
            Pn2=Pnair
          ELSE
            IM2=IM+1
            Pn2=Pn(I,IM2)
          ENDIF
          TC=TMPf(I,IM)
          Ps=Psat01(TC)
          DOMAIN=W(I,IM,2)
          IF(ABS(DOMAIN).LT.1.0e-28)THEN
          WRITE(MSG,'(A,3I4)')'ERROR: very small domain for',ICOMP,I,IM
            CALL EDISP(IUOUT,MSG)
            WRITE(MSG,'(A,I5)')'at time step',NSINC
            CALL EDISP(IUOUT,MSG)
            WRITE(MSG,'(A,G14.4)')' DOMAIN: ',DOMAIN
            CALL EDISP(IUOUT,MSG)
            stop
          ENDIF
          SUM=W(I,IM,5)-W(I,IM,1)*Pn1-W(I,IM,3)*Pn2-W(I,IM,4)*Sn
          Fn(I,IM)=0.0
C        Pnew=(SUM-Fn(I,IM))/DOMAIN
          Pnew=SUM/DOMAIN
          Pn(I,IM)=RLXln*SNGL(Pnew)+RRLXln*Pnsave
          IF(Pn(I,IM).LT.0.1)Pn(I,IM)=0.1
          RH=Pn(I,IM)/Ps
          IF(RH.GT.RHmax)THEN
            RH=RHmax
            Pn(I,IM)=RH*Ps
          ENDIF
          Fnew=SNGL(SUM)-Pn(I,IM)*DOMAIN
          Fn(I,IM)=RLXlnF*Fnew+RRLXlnF*Fnsave
          IF(Fn(I,IM).LT.0.0)Fn(I,IM)=0.0
C        ENDIF
        ENDDO
   20 CONTINUE

C Air node.
      Pnsave=Pnair
      Fnsave=Fnair
      TC=TFA(ICOMP)
      Ps=Psat01(TC)
      DOMAIN=E(1,INA)
      IF(ABS(DOMAIN).LT.1.0e-28)THEN
        WRITE(MSG,'(A)')'ERROR: very small air node domain'
        CALL EDISP(IUOUT,MSG)
        WRITE(MSG,'(7x,2(A,I5))')'for zone',ICOMP,'at time step',NSINC
        CALL EDISP(IUOUT,MSG)
        WRITE(MSG,'(A,G14.4)')' DOMAIN: ',DOMAIN
        CALL EDISP(IUOUT,MSG)
        stop
      ENDIF
      SUM=E(1,IPA)-E(1,IQA)*Snair
      DO I=1,NC
        IMsurf=MNDC(ICOMP,I)
        SUM=SUM-E(1,I)*Pn(I,IMsurf)
      ENDDO
      Fnair=0.0
C      Pnew=(SUM-Fnair)/DOMAIN
      Pnew=SUM/DOMAIN
      Pnair=RLXln*SNGL(Pnew)+RRLXln*Pnsave
      IF(Pnair.LT.0.1)Pnair=0.1
      RH=Pnair/Ps
      IF(RH.GT.RHmax)THEN
        RH=RHmax
        Pnair=RH*Ps
      ENDIF
      Fnew=SNGL(SUM)-Pnair*DOMAIN
      Fnair=RLXlnF*Fnew+RRLXlnF*Fnsave
      IF(Fnair.LT.0.0)Fnair=0.0
C      ENDIF
      RETURN
      END

C ******************** MZMCHP ********************
C Check for convergence in pressure values.

      SUBROUTINE MZMCHP(ICOMP,CONVP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST11/MITRM,RITRMP,RITRMF
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      character outs*124
 
      LOGICAL CONVP

      RESXP=0.
      IF(RLXln.LT.1.0)THEN
        
      if(RLXln.lt.0.99E-10)then
        write(outs,'(a,i3)') 
     &  'MZMCHP: RLXln too small in zone',icomp
        call edisp(iuout,outs)
      endif

        FACTOR=1.0/RLXln
      ELSE
        FACTOR=1.0
      ENDIF
      RESP=(Pfair(ICOMP)-Pnair)*FACTOR
      ARESP=ABS(RESP)
      RESXP=AMAX1(RESXP,ARESP)
      IF(RESXP.GT.RITRMP)THEN
        CONVP=.FALSE.
        RETURN
      ENDIF
      DO 10 I=1,NCONST(ICOMP)
        DO IM=1,MNDC(ICOMP,I)
          RESP=(Pf(ICOMP,I,IM)-Pn(I,IM))*FACTOR
          ARESP=ABS(RESP)
          RESXP=AMAX1(RESXP,ARESP)
          IF(RESXP.GT.RITRMP)THEN
            CONVP=.FALSE.
            RETURN
          ENDIF
        ENDDO
   10 CONTINUE
      CONVP=.TRUE.
      RETURN
      END

C ******************** MZMCHF ********************
C Check for convergence in fluid values.

      SUBROUTINE MZMCHF(ICOMP,CONVF)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST11/MITRM,RITRMP,RITRMF
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      character outs*124
 
      LOGICAL CONVF

      RESXF=0.
      IF(RLXlnF.LT.1.0)THEN
        if(RLXlnF.lt.0.99E-10)then
          write(outs,'(a,3i3)') 
     &    'MZMCHF: RLXlnF too small in zone',icomp
          call edisp(iuout,outs)
        endif
        FACTOR=1.0/RLXlnF
      ELSE
        FACTOR=1.0
      ENDIF
      RESF=(Ffair(ICOMP)-Fnair)*FACTOR
      ARESF=ABS(RESF)
      RESXF=AMAX1(RESXF,ARESF)
      IF(RESXF.GT.RITRMF)THEN
        CONVF=.FALSE.
        RETURN
      ENDIF
      DO 10 I=1,NCONST(ICOMP)
        DO IM=1,MNDC(ICOMP,I)
          RESF=(Ff(ICOMP,I,IM)-Fn(I,IM))*FACTOR
          ARESF=ABS(RESF)
          RESXF=AMAX1(RESXF,ARESF)
          IF(RESXF.GT.RITRMF)THEN
            CONVF=.FALSE.
            RETURN
          ENDIF
        ENDDO
   10 CONTINUE
      CONVF=.TRUE.
      RETURN
      END

C ******************** SETFEN ********************
C Updates the future variables from n'th variables.

      SUBROUTINE SETFEN(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

      Pfair(ICOMP)=Pnair
      Ffair(ICOMP)=Fnair
      Sfair(ICOMP)=Snair
      Sf(ICOMP)=Sn
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NM=MNDC(ICOMP,I)
        DO IM=1,NM
          Pf(ICOMP,I,IM)=Pn(I,IM)
          Ff(ICOMP,I,IM)=Fn(I,IM)
        ENDDO
   10 CONTINUE
      RETURN
      END

C ******************** LYRPRP ********************
C Gets the thermophysical properties for a layer.

      SUBROUTINE LYRPRP(ICOMP,I,IL,CNDCT,DNSTY,HTCAP,THCK)
#include "building.h"

      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      CALL LYRTYP(ICOMP,I,IL,IAIR)

C Air gap with fixed thermal resistance .
      IF(IAIR.GT.0)THEN
        THCK=THRMLI(ICOMP,I,IL,4)
        RESIST=RGAPS(ICOMP,I,IAIR)
        CNDCT=THCK/RESIST
        DNSTY=1.3
        HTCAP=1005.5

C Ordinary solid layer.
      ELSE
        CNDCT=THRMLI(ICOMP,I,IL,1)
        DNSTY=THRMLI(ICOMP,I,IL,2)
        HTCAP=THRMLI(ICOMP,I,IL,3)
        THCK=THRMLI(ICOMP,I,IL,4)
      ENDIF
      RETURN
      END

C ******************** LYRTYP ********************
C Defines the layer type; air gap (IAIR > 0) or solid (IAIR = 0).

      SUBROUTINE LYRTYP(ICOMP,I,IL,IAIR)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)

      IAIR=0
      NG=NGAPS(ICOMP,I)
      DO 10 IG=1,NG
        IF(IL.EQ.NPGAP(ICOMP,I,IG))IAIR=IG
   10 CONTINUE
      RETURN
      END

C ******************** CHKMST ********************

      SUBROUTINE CHKMST(IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

      CHARACTER MSG*72

      IER=0
      DO 10 ICOMP=1,NCOMP
        IF(MSTRZN(ICOMP))THEN
          DO 20 I=1,NCONST(ICOMP)

C If this construction is lumped.
            IF(ILUMP(ICOMP,I).EQ.1)THEN
              WRITE(MSG,'(2A)')'Moisture transport cannot be ',
     &                         'modelled through lumped constructions.'
              CALL USRMSG(' ',MSG,'W')
              IER=1
              RETURN
            ENDIF
            DO IL=1,NELTS(ICOMP,I)
              SUM=FLOAT(MNDL(ICOMP,I,IL)/NNDL(ICOMP,I,IL))+1.0
              RMNDR=AMOD(SUM,2.0)
              IF(RMNDR.GT.1.0E-4)THEN
                WRITE(MSG,'(1a,3(i3,a))')
     &          'heat node numbers at layer (',ICOMP,',',I,',',IL,').'
                CALL USRMSG('unacceptable ratio of moisture to',MSG,'W')
                IER=1
                RETURN
              ENDIF
            ENDDO
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
      END

C ******************** MSRTND ********************

      SUBROUTINE MSRTND(ICOMP,I,IL,INH,IMS,IME)
#include "building.h"

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

C Define the first moisture node in the current layer.
      IMS=1
      IH=1
      DO 10 ILYR=1,IL-1
        IMS=IMS+MNDL(ICOMP,I,ILYR)
        IH=IH+NNDL(ICOMP,I,ILYR)
   10 CONTINUE
      NM=MNDL(ICOMP,I,IL)
      NH=NNDL(ICOMP,I,IL)
      NMNHR=NM/NH
      N1=(NMNHR-1)/2
      IME=IMS+N1

C The current heat node is the first node in the layer.
      IF(IH.EQ.INH)RETURN
      DO 20 IND=2,NH
        IH=IH+1
        IMS=IME+1
        IME=IME+NMNHR
        IF(IH.EQ.INH)RETURN
   20 CONTINUE

C The current heat node is the last node in the layer.
      IH=IH+1
      IMS=IME+1

C IME=MNDC(ICOMP,I).
      IME=IME+(NMNHR-1)/2+1
      IF(IH.EQ.INH)RETURN
      STOP 'Error (156): in MSRTND'
      END

C ******************** WRITRH ********************
C Writes the relative humidity results.

      SUBROUTINE WRITRH(ICOMP)
#include "building.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PREC7/ITCNST
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST09/IMSTR
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST26/XIpfair,RXIVpf(MS,MNM)
      COMMON/GR3D150/INDX3,DTIME
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

      dimension RHsrf(MS),TCsrf(MS)

      NDAY=INT(NSINC/(24*NTSTEP))
      IF(NDAY.GE.ITCNST)THEN
        IF(IHRF.EQ.1)THEN
          IDAY=IDYF
        ELSE
          IDAY=IDYP
        ENDIF
        TIMES=IDAY+(IHRF-1+ITS*DTIME/3600.0)/24.0
        TCair=TFA(ICOMP)
        Ps=Psat01(TCair)
        RHair=Pnair/Ps*100.0
        NC=NCONST(ICOMP)
        DO 10 I=1,NC
          NM=MNDC(ICOMP,I)
          TCsrf(I)=TMPf(I,NM)
          Tdb=TCsrf(I)
          Ps=Psat01(Tdb)
          RHsrf(I)=Pn(I,NM)/Ps*100.0
   10   CONTINUE

C Note: adjust format if MS>48.
        WRITE(IMSTR,'(F10.5,48F5.1)')TIMES,RHair,(RHsrf(I),I=1,NC)
        WRITE(IMSTR,'(10X,48F5.1)')TCair,(TCsrf(I),I=1,NC)
      ENDIF
      RETURN
      END

C ******************** H3K_transport_mould_data ********************
C Writes the detailed moisture results to h3k output.

      SUBROUTINE H3K_transport_mould_data(ICOMP)
      use h3kmodule
      implicit none
#include "building.h"
#include "geometry.h"
#include "moist.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      integer       IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      integer NCONST,NELTS,NGAPS,NPGAP

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      real THRMLI

      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      integer MNDS,MNDZ,MNDC,MNDL

      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      integer ISORP
      real SORP

      COMMON/MOIST09/IMSTR
      integer IMSTR

      COMMON/MOIST26/XIpfair,RXIVpf(MS,MNM)
      real XIpfair,RXIVpf

      COMMON/MOIST50/TMNp(MCOM,MS,MNM)
      real TMNp

      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      real Pf,Ff,Sf

      COMMON/MOIST99/ICNVITRM(MCOM)
      integer ICNVITRM

      integer ICOMP

C Local variables
      integer NC,iSurf,NM,iMNode,NE,iLay,IL

      real Ps,RH,Uh,CFF,EXPT,DNSTY

C Declarations for H3Kreporting object.
      CHARACTER*12 cZone_Chars   ! zname(MCOM)
      Character*12 cSurf_Chars   ! sname(MCOM,MS)
      CHARACTER*8  cNode_Chars   !

C FUNCTIONS
      real Psat01,U01,U02

C Switch for zone name output.
      if (ReportBoolConfig("use_zonenames")) then
        write (cZone_Chars,'(A)') zname(icomp)(1:lnzname(icomp))
      else

C Pad zone index to 'XX'.
        if ( ICOMP .gt. 9 ) then
           write (cZone_Chars,'(A,I2)') 'zone_',ICOMP
        else
           write (cZone_Chars,'(A,I1)') 'zone_0',ICOMP
        endif
      endif    ! use_zonenames

      Call AddToReport(
     &        rvMstItCnt%Identifier,
     &        real(ICNVITRM(ICOMP)),
     &        cZone_Chars)

        NC=NCONST(ICOMP)
        DO 10 iSurf=1,NC

C Switch for surface name output.
          if (ReportBoolConfig ("use_surfacenames")) then
            write (cSurf_Chars,'(A)')
     &          sname(ICOMP,isurf)(1:lnsname(ICOMP,iSurf))
          else

C Pad surface index to 'XX'.
            if ( iSurf .gt. 9 ) then
              write (cSurf_Chars,'(A,I2)') 'surface_',iSurf
            else
              write (cSurf_Chars,'(A,I1)') 'surface_0',iSurf
            endif
          endif   ! use_surfacenames

          NE=NELTS(ICOMP,iSurf)    ! number of layers in surface
          IL=1                     ! set layer to first (outermost)
          NM=MNDL(ICOMP,iSurf,IL)  ! number of moisture nodes in layer

C XML output for each moisture node of surface iSurf.
          do iMNode = 1,mndc(ICOMP,iSurf)
            if ( iMNode .gt. 9 ) then
              write(cNode_Chars, '(A,I2)') 'mnode_', iMNode
            else
              write(cNode_Chars, '(A,I1)') 'mnode_0', iMNode
            endif

C Find iLay for each iMNode (neccessary due to layer material
C properties SORP() relating to construction layers and not
C moisture nodes!).
            if (iMNode.le.NM) then
              iLay=IL
            else
              if (IL+1 .le. NE) then
                IL=IL+1
                NM=NM+MNDL(ICOMP,iSurf,IL)
                iLay=IL
              endif
            endif

C Vapour pressure at "moisture node". (Nodes *not* necessarily identical
C to construction nodes!).
            Call AddToReport(
     &             rvBldMstVapPressNode%Identifier,
Cx     &             Pn(iSurf,iMNode),
     &             Pf(ICOMP,iSurf,iMNode),
     &             cZone_Chars,
     &             cSurf_Chars,
     &             cNode_Chars)

C Relative humidity at "moisture node"
            Ps=Psat01(TMNp(ICOMP,iSurf,iMNode))
            RH=Pf(ICOMP,iSurf,iMNode)/Ps

            Call AddToReport(
     &             rvBldMstRHnode%Identifier,
Cx     &             Pn(iSurf,iMNode)/Ps*100.0,
     &             100.*RH,
     &             cZone_Chars,
     &             cSurf_Chars,
     &             cNode_Chars)

C Moisture storage capacity at "moisture node"
            Call AddToReport(
     &             rvBldMstStoreCap%Identifier,
     &             RXIVpf(iSurf,iMNode)/SNA(ICOMP,iSurf),
     &             cZone_Chars,
     &             cSurf_Chars,
     &             cNode_Chars)

C Moisture content at "moisture node", kg/m3
            Uh=SORP(ICOMP,iSurf,iLay,1)
            CFF=SORP(ICOMP,iSurf,iLay,2)
            EXPT=SORP(ICOMP,iSurf,iLay,3)

            Call AddToReport(
     &             rvBldMstStorage%Identifier,
Cx     &             Ff(ICOMP,iSurf,iMNode),
     &             U01(iMoistModel(ICOMP),RH,Uh,CFF,EXPT),
     &             cZone_Chars,
     &             cSurf_Chars,
     &             cNode_Chars)

C Moisture content at "moisture node" (kg/kg).
            DNSTY=THRMLI(ICOMP,iSurf,iLay,2)

            Call AddToReport(
     &             rvBldMstStorageMass%Identifier,
cx     &             Ff(ICOMP,iSurf,iMNode),
     &             U02(iMoistModel(ICOMP),RH,Uh,CFF,EXPT,DNSTY),
     &             cZone_Chars,
     &             cSurf_Chars,
     &             cNode_Chars)

C Temperature at "moisture node".
            Call AddToReport(
     &             rvBldMstTNode%Identifier,
     &             TMNp(ICOMP,iSurf,iMNode),
     &             cZone_Chars,
     &             cSurf_Chars,
     &             cNode_Chars)

          enddo  ! End iSurf 1,MN of ICOMP.

   10   CONTINUE

      RETURN
      END
