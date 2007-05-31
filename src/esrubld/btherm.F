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

C   This file contains the following variable thermophysical
C   properties subroutines:

C   1) modifying the coefficients of the difference equations:
C        thzero
C        inthrm
C        mzkond
C        mzthrm
C        mzlayr
C        mzcoe4

C *************************  MZVTHP  **************************
C MZVTHP controls the variable thermophysical properties 
C simulation.
C *************************************************************
      SUBROUTINE MZVTHP(ICOMP)
      INCLUDE 'building.h'

      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      COMMON/VTHP32/IVTHPS,IVTHPZ(MCOM)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)

      LOGICAL ILTHPS,ILTHPZ,INTHPS,INTHPZ,IVTHPS,IVTHPZ,MSTROK,MSTRZN

      IF(MSTRZN(ICOMP).OR.IVTHPZ(ICOMP))THEN

C Set the present values of thermal properties equal to the saved values.
        CALL MTHPES(ICOMP)

C Set the future values equal to the initial values.
        CALL MTHFEI(ICOMP)

C Calculate the new values for thermal conductivity based on linear 
C temperature and/or moisture content dependence.
        IF(ILTHPZ(ICOMP))CALL MZKOND(ICOMP)

C Calculate the new values for thermal properties based on nonlinear 
C temperature dependence.
        IF(INTHPZ(ICOMP))CALL MZNTHP(ICOMP)

C Save the future thermal properties.
        CALL MZTHSFP(ICOMP)

C Calculate the corection ratios due to variable thermophysical 
C properties.
        IF(MSTRZN(ICOMP))THEN
          CALL MZMTMP(ICOMP)
          CALL MZMTMF(ICOMP)
          CALL MZCOE4M(ICOMP)
        ELSE
          CALL MZCOE4(ICOMP)
        ENDIF
      ELSE
        CALL MZITCF(ICOMP)
      ENDIF
      RETURN
      END

C *************************  THZERO  **************************
C THZERO initializes all variables associated with the nonlinear
C thermal properties.
C *************************************************************
      SUBROUTINE THZERO
      INCLUDE 'building.h'

      COMMON/VTHP11/NTHF,IBTHAL(MTHF,3)
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)

C Initialize all variables of the nonlinear thermal properties 
      NTHF=0
      DO 10 ITHF=0,MTHF
        NTHEQ(ITHF)=0
        IBTHAL(ITHF,1)=0
        IBTHAL(ITHF,2)=0
        IBTHAL(ITHF,3)=0
      DO 10 IEQ=1,MTHEQ
      DO 10 IDATA=1,MDATA
        BTHDT(ITHF,IEQ,IDATA)=0.0
   10 CONTINUE
      RETURN
      END

C *************************  MZITHP  **********************************
C MZITHP initializes the variable thermophysical properties variables.
C *********************************************************************
      SUBROUTINE MZITHP
      INCLUDE 'building.h'

      common/c1/ncomp,ncon
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP23/THCONs(MCOM,MS,MN)
      COMMON/VTHP24/THDNSs(MCOM,MS,MHCV),THCAPs(MCOM,MS,MHCV)
      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      COMMON/VTHP32/IVTHPS,IVTHPZ(MCOM)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      LOGICAL ILTHPS,ILTHPZ,INTHPS,INTHPZ,IVTHPS,IVTHPZ

C Set the variable thermophysical properties flags.
      IF(ILTHPS.OR.INTHPS)THEN
        IVTHPS=.TRUE.
      ELSE
        IVTHPS=.FALSE.
      ENDIF
      DO 10 ICOMP=1,NCOMP
        IF(ILTHPZ(ICOMP).OR.INTHPZ(ICOMP))THEN
          IVTHPZ(ICOMP)=.TRUE.
        ELSE
          IVTHPZ(ICOMP)=.FALSE.
        ENDIF
        NC=NCONST(ICOMP)
      DO 10 I=1,NC

C Only if the condtruction is not lumped.
        IF(ILUMP(ICOMP,I).NE.1)THEN
          IDC=0
          IHCV=0
          NE=NELTS(ICOMP,I)
          DO 20 IL=1,NE

C Check if the current layer is an air gap for which fixed properties are
C assigned.
            CALL LYRTYP(ICOMP,I,IL,IAIR)
            NDL=NNDL(ICOMP,I,IL)
          DO 20 IDL=1,NDL+1
            IF(IDL.LE.NDL)THEN
              IDC=IDC+1
              IF(IAIR.GT.0)THEN
                THCONs(ICOMP,I,IDC)=THRMLI(ICOMP,I,IL,4)/
     &                              RGAPS(ICOMP,I,IAIR)
              ELSE
                THCONs(ICOMP,I,IDC)=THRMLI(ICOMP,I,IL,1)
              ENDIF
            ENDIF
            IHCV=IHCV+1
            IF(IAIR.GT.0)THEN
              THDNSs(ICOMP,I,IHCV)=1.3
              THCAPs(ICOMP,I,IHCV)=1005.5
            ELSE
              THDNSs(ICOMP,I,IHCV)=THRMLI(ICOMP,I,IL,2)
              THCAPs(ICOMP,I,IHCV)=THRMLI(ICOMP,I,IL,3)
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
      END

C *************************  MZITCF  **********************************
C MZITCF sets the thermal coupling terms.
C *********************************************************************
      SUBROUTINE MZITCF(ICOMP)
      INCLUDE 'building.h'

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/PREC13P/CPs(MCOM,MS,MN,2),QCPs(MCOM,MS,MN)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)
      COMMON/VTHP17/SLF(MS,MN)

      common/ts4/itsitr(msch),itrpas
      
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        NDC=NNDC(ICOMP,I)
        DO 10 IDC=1,NDC

C Update present coefficients - using saved present values - and
C future using standard values from mzcoe1 (or mzcoe2).
          VCP(I,IDC,1)=CPs(ICOMP,I,IDC,1)
          VCP(I,IDC,2)=CPs(ICOMP,I,IDC,2)
          VCF(I,IDC,1)=C(ICOMP,I,IDC,1)
          VCF(I,IDC,2)=C(ICOMP,I,IDC,2)
          QCP(I,IDC)=QCPs(ICOMP,I,IDC)
          QCF(I,IDC)=QC(ICOMP,I,IDC)
          SLF(I,IDC)=1.0
          
C Save future coefficient values (allowing for time step 4 control).
          if (ITRPAS.eq.0) then
            CPs(ICOMP,I,IDC,1)=C(ICOMP,I,IDC,1)
            CPs(ICOMP,I,IDC,2)=C(ICOMP,I,IDC,2)
            QCPs(ICOMP,I,IDC)=QC(ICOMP,I,IDC)
          endif
   10 CONTINUE
      RETURN
      END

C *****************************  MTHPES  ******************************
C MTHPES sets the present values of thermal properties equal to the
C saved values from the last time step.
C *********************************************************************
      SUBROUTINE MTHPES(ICOMP)
      INCLUDE 'building.h'

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/VTHP21/THCONp(MS,MN),THDNSp(MS,MHCV),THCAPp(MS,MHCV)
      COMMON/VTHP23/THCONs(MCOM,MS,MN)
      COMMON/VTHP24/THDNSs(MCOM,MS,MHCV),THCAPs(MCOM,MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C Only if the condtruction is not lumped.
        IF(ILUMP(ICOMP,I).NE.1)THEN

C Set the present variables.
          NDC=NNDC(ICOMP,I)
          DO 20 IDC=1,NDC
            THCONp(I,IDC)=THCONs(ICOMP,I,IDC)
   20     CONTINUE
          NE=NELTS(ICOMP,I)
          NHCV=NDC+NE-1
          DO 30 IHCV=1,NHCV
            THDNSp(I,IHCV)=THDNSs(ICOMP,I,IHCV)
            THCAPp(I,IHCV)=THCAPs(ICOMP,I,IHCV)
   30     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
      END

C *****************************  MTHFEI  ******************************
C MTHFEI sets the future thermophysical properties values equal to the 
C initial values.
C *********************************************************************
      SUBROUTINE MTHFEI(ICOMP)
      INCLUDE 'building.h'

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C Only if the condtruction is not lumped.
        IF(ILUMP(ICOMP,I).NE.1)THEN
          IDC=0
          IHCV=0
          NE=NELTS(ICOMP,I)
          DO 20 IL=1,NE

C Check if the current layer is an air gap for which fixed properties are
C assigned.
            CALL LYRTYP(ICOMP,I,IL,IAIR)
            NDL=NNDL(ICOMP,I,IL)
          DO 20 IDL=1,NDL+1
            IF(IDL.LE.NDL)THEN
              IDC=IDC+1
              IF(IAIR.GT.0)THEN
                THCONf(I,IDC)=THRMLI(ICOMP,I,IL,4)/RGAPS(ICOMP,I,IAIR)
              ELSE
                THCONf(I,IDC)=THRMLI(ICOMP,I,IL,1)
              ENDIF
            ENDIF
            IHCV=IHCV+1
            IF(IAIR.GT.0)THEN
              THDNSf(I,IHCV)=1.3
              THCAPf(I,IHCV)=1005.5
            ELSE
              THDNSf(I,IHCV)=THRMLI(ICOMP,I,IL,2)
              THCAPf(I,IHCV)=THRMLI(ICOMP,I,IL,3)
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
      END

C ***********************  MZKOND (ICOMP) *********************
C MZKOND estimates the actual value of the thermal conductivity
C depending on the type of the layer. If the user defined the
C thermal conductivity of the layer to be :
C       1- constant: then Ka  = Ks
C       2- temperature dependent: then 
C               Ka = Ks + A * [ Ta - Ts ] 
C       3- moisture content dependent : then
C               Ka = ks + B * Mc
C       4- temperature and moisture dependent : then
C               Ka = Ks + A * [ Ta - Ts ] + B * Mc
C       where ;
C           Ka : actual thermal conductivity 
C           Ks : standard thermal conductivity(DRY)
C           A  : linear temp. dependency factor
C           B  : linear moisture dependency factor
C           Ta : actual temperature of the layer node
C           Ts : standard temperature for Ks
C           Mc : actual moisture content
 
C     NB :-These equations assume that the temperature and/or
C          moisture dependency of the thermal conductivity are
C          linear.
C         -This subroutine can be easily improved to involve higher 
C          orders of dependency, by including it in a bigger IF
C          statements structure. 
C *************************************************************
      SUBROUTINE MZKOND(ICOMP)
      INCLUDE 'building.h'

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP02/IVKON(MCOM,MS,ME)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)

      LOGICAL MSTROK,MSTRZN

C Determine the moisture node temperatures if moisture modelling is invoked.
      IF(MSTRZN(ICOMP))CALL MZMTMF(ICOMP)
      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C If the construction is lumped, check the next construction.
        IF(ILUMP(ICOMP,I).NE.1)THEN
          NDC=NNDC(ICOMP,I)
          NE=NELTS(ICOMP,I)

C INDS: first heat node; INDE: last heat node.
          INDE=1
          DO 20 J=1,NE

C Define the layer type.
            CALL LYRTYP(ICOMP,I,J,IAIR)
            IF(IAIR.GT.0)THEN
              CNDCT0=THRMLI(ICOMP,I,J,4)/RGAPS(ICOMP,I,IAIR)
            ELSE
              CNDCT0=THRMLI(ICOMP,I,J,1)
            ENDIF
            NHT=NNDL(ICOMP,I,J)

C DELH: space step for heat nodes
            DELH=THRMLI(ICOMP,I,J,4)/FLOAT(NHT)
            INDS=INDE
            INDE=INDE+NHT
            INDX=IVKON(ICOMP,I,J)

C If k_value of this layer is constant, check next layer.
            IF(INDX.NE.0)THEN
              CNDCT1=0.0
              CNDCT2=0.0
              X2=0.0
              DO 30 INDL=INDS,INDE-1
                K1=INDL
                K2=INDL+1

C If k_value of this layer is temperature dependent, or temp. & moisture
C dependent, then
                IF(INDX.EQ.1.OR.INDX.EQ.3)THEN 
                  TT1=TFC(ICOMP,I,K1)
                  IF(INDL.EQ.NDC-1)THEN
                    TT2=TFS(ICOMP,I)
                  ELSE
                    TT2=TFC(ICOMP,I,K2)
                  ENDIF 
                  TK=(TT1+TT2)/2.-THRMLI(ICOMP,I,J,5)
                  CNDCT1=THRMLI(ICOMP,I,J,6)*TK

C If k_value of this layer is moisture dependent, or temp. & moisture
C dependent, then 
                ELSEIF(INDX.EQ.2.OR.INDX.EQ.3)THEN
                  IF(MSTROK.AND.MSTRZN(ICOMP))THEN

C Define heat node boundaries.
                    X1=X2
                    X2=X2+DELH

C Calculate the total thermal resistance with accounting for moisture 
C content dependnece.
                    CALL RESMST(ICOMP,I,J,X1,X2,TOTRES)

C Estimate the change in thermal conductivity due to moisture content 
C dependnece.
                    CNDCT2=DELH/TOTRES-THRMLI(ICOMP,I,J,1)
                  ENDIF
                ELSE
                  STOP 'MZKOND: error in the linear conductivity type.'
                ENDIF
                THCONf(I,INDL)=CNDCT0+CNDCT1+CNDCT2
   30         CONTINUE
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
      END

C *********************************************************************
C RESMST estimates the thermal resistance for a sigment of a layer 
C including the effect of moisture content.
C *********************************************************************
      SUBROUTINE RESMST(ICOMP,I,IL,X1,X2,TOTRES)
      INCLUDE 'building.h'

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

      SMLL=1.E-6
      TOTRES=0.

C Define the first moisture node in the current layer.
      INM=0
      DO 10 ILYR=1,IL-1
        INM=INM+MNDL(ICOMP,I,ILYR)
   10 CONTINUE
      NM=MNDL(ICOMP,I,IL)

C DELM: moisture node space step.
      DELM=THRMLI(ICOMP,I,IL,4)/FLOAT(NM)
      DO 20 IM=1,NM+1
        INM=INM+1
        IF(IM.EQ.1)THEN
          XX1=0.0
          XX2=DELM/2.
        ELSEIF(IM.GT.NM)THEN
          XX1=XX2
          XX2=XX2+DELM/2.
        ELSE
          XX1=XX2
          XX2=XX2+DELM
        ENDIF
        IF(X1.GT.(XX2-SMLL))THEN
        ELSEIF(X2.LT.(XX1+SMLL))THEN
          RETURN
        ELSE
          XMAX=AMIN1(X2,XX2)
          XMIN=AMAX1(X1,XX1)

C Thermal resistance = delta x / thermal conductivity.
          TOTRES=TOTRES+(XMAX-XMIN)/CONMST(ICOMP,I,INM,IL)
        ENDIF
   20 CONTINUE
      RETURN
      END

C **********************************************************************
C CONMST calculates the inter-moisture nodal thermal conductivity as
C a function of moisture content.
C **********************************************************************
      FUNCTION CONMST(ICOMP,I,INM,IL)
      INCLUDE 'building.h'

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)

      TC=TMPf(I,INM)
      RH=Pf(ICOMP,I,INM)/Psat01(TC)
      Uh=SORP(ICOMP,I,IL,1)
      CFF=SORP(ICOMP,I,IL,2)
      EXPT=SORP(ICOMP,I,IL,3)
      DNSTY=THRMLI(ICOMP,I,IL,2)
      U=U02(RH,Uh,CFF,EXPT,DNSTY)

C Thermal conductivity = 
C       dry conductivity * moisture dependence factor * moisture content
      CONMST=THRMLI(ICOMP,I,IL,1)+THRMLI(ICOMP,I,IL,7)*U
      RETURN
      END

C *****************************  MZNTHP  ******************************
C For each function controlling the  thermal properties of the building
C materials; and according to the actuator location; MZTHRM computes
C the new values of the layers thermal  properties by using the 
C equation defined by the user.
C *********************************************************************
      SUBROUTINE MZNTHP(ICOMP)
      INCLUDE 'building.h'

      COMMON/VTHP11/NTHF,IBTHAL(MTHF,3)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      NC=NCONST(ICOMP)

C for each thermal control function
      DO 10 I=1,NTHF
       
C First, define the location of the actuator;
C IBTHAL(I,1) = 0  : the complete building materials have 
C                    the same temperature dependence.
C IBTHAL(I,1) = Z 
C    IBTHAL(I,2)=0 : zone Z  materials have the same
C                     temperature dependence.
C IBTHAL(I,1) = Z 
C    IBTHAL(I,2)=C  
C       IBTHAL(I,3)=0 : construction C in zone Z materials
C                       have the same temperature dependence.
C IBTHAL(I,1) = Z 
C    IBTHAL(I,2)=C  
C       IBTHAL(I,3)=L : layer L of construction C in zone Z has
C                       temperature dependent thermal properties.
             
        IB1=IBTHAL(I,1)
        IB2=IBTHAL(I,2)
        IB3=IBTHAL(I,3)

C Complete system or zone case.
        IF(IB1.EQ.0.OR.(IB1.EQ.ICOMP.AND.IB2.EQ.0))THEN
          DO 20 J=1,NC
            IF(ILUMP(ICOMP,J).NE.1)THEN
              NE=NELTS(ICOMP,J)
              INDE=1
              DO 30 K=1,NE
                 INDS=INDE
                 INDE=INDE+NNDL(ICOMP,J,K)
                 CALL MZLAYR(I,ICOMP,J,K,INDS,INDE)
   30         CONTINUE
            ENDIF
   20     CONTINUE

C Complete construction case.
        ELSEIF(IB1.EQ.ICOMP.AND.IB2.GT.0.AND.IB2.LE.NC.AND.IB3.EQ.0)THEN
          J=IB2
          IF(ILUMP(ICOMP,J).EQ.1)GOTO 10
          NE=NELTS(ICOMP,J)
          INDE=1
          DO 60 K=1,NE
             INDS=INDE
             INDE=INDE+NNDL(ICOMP,J,K)
             CALL MZLAYR(I,ICOMP,J,K,INDS,INDE)
   60     CONTINUE

C Single layer case.
        ELSEIF(IB1.EQ.ICOMP.AND.IB2.GT.0.AND.IB2.LE.NC.AND.
     &         IB3.GT.0.AND.IB3.LE.NELTS(IB1,IB2))THEN
          J=IB2
          K=IB3
          IF(ILUMP(ICOMP,J).EQ.1)GOTO 10
          INDS=1
          DO 100 JL=1,K-1
             INDS=INDS+NNDL(ICOMP,J,JL)
  100     CONTINUE
          INDE=INDS+NNDL(ICOMP,J,K)
          CALL MZLAYR(I,ICOMP,J,K,INDS,INDE)
        ENDIF
   10 CONTINUE
      RETURN
      END

C **************************    MZLAYR    *****************************
C MZLAYR calculates thermal properties values for a layer based on the 
C activated thermal control function.
C *********************************************************************
      SUBROUTINE MZLAYR(IEQ,ICOMP,IS,IL,INDS,INDE)
      INCLUDE 'building.h'

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      NC=NNDC(ICOMP,IS)
      DO 10 INDL=INDS,INDE
        TT1=TFC(ICOMP,IS,INDL)
        DO 20 L=1,NTHEQ(IEQ)
          ITYP=INT(BTHDT(IEQ,L,1))
          COLD=BTHDT(IEQ,L,2)
          HOT=BTHDT(IEQ,L,3)

C Thermal conductivity.
          IF(ITYP.EQ.1)THEN
            IF(INDL.LT.INDE)THEN
              IF(INDL.GE.NC-1)THEN
                TT2=TFS(ICOMP,IS)
              ELSE
                TT2=TFC(ICOMP,IS,INDL+1)
              ENDIF
              TEMP=(TT1+TT2)/2.
              IF(TEMP.GE.COLD.AND.TEMP.LT.HOT)THEN
                PVALU=BTHDT(IEQ,L,4)+
     &                BTHDT(IEQ,L,5)*TEMP**BTHDT(IEQ,L,6)+
     &                BTHDT(IEQ,L,7)*TEMP**BTHDT(IEQ,L,8)+   
     &                BTHDT(IEQ,L,9)*TEMP**BTHDT(IEQ,L,10)+   
     &                BTHDT(IEQ,L,11)*TEMP**BTHDT(IEQ,L,12)
                THCONf(IS,INDL)=PVALU
              ENDIF
            ENDIF

C Density or heat capacity.
          ELSEIF(ITYP.EQ.2.OR.ITYP.EQ.3)THEN
            IHCV=INDL+IL-1
            TEMP=TT1
            IF(TEMP.GE.COLD.AND.TEMP.LT.HOT)THEN
              PVALU=BTHDT(IEQ,L,4)+BTHDT(IEQ,L,5)*TEMP**BTHDT(IEQ,L,6)+
     &              BTHDT(IEQ,L,7)*TEMP**BTHDT(IEQ,L,8)+   
     &              BTHDT(IEQ,L,9)*TEMP**BTHDT(IEQ,L,10)+   
     &              BTHDT(IEQ,L,11)*TEMP**BTHDT(IEQ,L,12)
              IF(ITYP.EQ.2)THEN
                THDNSf(IS,IHCV)=PVALU
              ELSE
                THCAPf(IS,IHCV)=PVALU
              ENDIF
            ENDIF
          ELSE
            STOP 'Unknown thermal property type.'
          ENDIF
   20   CONTINUE
   10 CONTINUE
      RETURN
      END

C ***************************** MZTHSFP ********************************
C MZTHSFP saves the future thermal property values.
C *********************************************************************
      SUBROUTINE MZTHSFP(ICOMP)
      INCLUDE 'building.h'
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/VTHP23/THCONs(MCOM,MS,MN)
      COMMON/VTHP24/THDNSs(MCOM,MS,MHCV),THCAPs(MCOM,MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C Only if the condtruction is not lumped.
        IF(ILUMP(ICOMP,I).NE.1)THEN
          NDC=NNDC(ICOMP,I)
          DO 20 IDC=1,NDC
            THCONs(ICOMP,I,IDC)=THCONf(I,IDC)
   20     CONTINUE
          NE=NELTS(ICOMP,I)
          NHCV=NDC+NE-1
          DO 30 IHCV=1,NHCV
            THDNSs(ICOMP,I,IHCV)=THDNSf(I,IHCV)
            THCAPs(ICOMP,I,IHCV)=THCAPf(I,IHCV)
   30     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
      END

C ***************************** MZCOE4 ********************************
C MZCOE4 modifies the coefficients computed by MZCOE1 (or MZCOE2 if the
C time step was less than one hour)
C *********************************************************************
      SUBROUTINE MZCOE4(ICOMP)
      INCLUDE 'building.h'

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)
      COMMON/VTHP21/THCONp(MS,MN),THDNSp(MS,MHCV),THCAPp(MS,MHCV)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/GR3D150/INDX3,DTIME

      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C If this construction is lumped.
        IF(ILUMP(ICOMP,I).EQ.1)THEN
          DO 100 IND=1,3
            VCP(I,IND,1)=C(ICOMP,I,IND,1)
            VCP(I,IND,2)=C(ICOMP,I,IND,2)
            QCP(I,IND)=QC(ICOMP,I,IND)
            VCF(I,IND,1)=C(ICOMP,I,IND,1)
            VCF(I,IND,2)=C(ICOMP,I,IND,2)
            QCF(I,IND)=QC(ICOMP,I,IND)
  100      CONTINUE
          GOTO 10
        ENDIF

C First node in the construction.
        IN=1

C First layer in the construction.
        IL=1

C First homogeneous control volume.
        IHCV=1

C Node is situated at 'outside' surface.
        NDL=NNDL(ICOMP,I,IL)
        DELXl=THRMLI(ICOMP,I,IL,4)/FLOAT(NDL)
        CMMN=2.0*DTIME/(DELXl*THDNSp(I,IHCV)*THCAPp(I,IHCV))
        VCP(I,IN,1)=CMMN
        VCP(I,IN,2)=CMMN*THCONp(I,IN)/DELXl
        QCP(I,IN)=CMMN
        VCF(I,IN,1)=CMMN
        VCF(I,IN,2)=CMMN*THCONf(I,IN)/DELXl
        QCF(I,IN)=CMMN

C Continue with other nodes (homogeneous) in the first layer.
        DO 20 INL=1,NDL-1
          IN1=IN
          IN=IN+1
          IN2=IN
          IHCV=IHCV+1
          CMMN=DTIME/(DELXl*THDNSp(I,IHCV)*THCAPp(I,IHCV))
          VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DELXl
          VCP(I,IN,2)=CMMN*THCONp(I,IN2)/DELXl
          QCP(I,IN)=CMMN
          VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DELXl
          VCF(I,IN,2)=CMMN*THCONf(I,IN2)/DELXl
          QCF(I,IN)=CMMN
   20   CONTINUE

C Continue with other layers in the construction.
        NE=NELTS(ICOMP,I)
        DO 30 IL=2,NE
          IN1=IN
          IN=IN+1
          IN2=IN
          IHCV=IHCV+2
          IHCV1=IHCV-1
          IHCV2=IHCV

C DELXp and DELXl are previous and current layers' space steps.
          DELXp=DELXl
          NDL=NNDL(ICOMP,I,IL)
          DELXl=THRMLI(ICOMP,I,IL,4)/FLOAT(NDL)
          CAP=(THDNSp(I,IHCV1)*THCAPp(I,IHCV1)*DELXp+
     &         THDNSp(I,IHCV2)*THCAPp(I,IHCV2)*DELXl)/2.0
          CMMN=DTIME/CAP
          VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DELXp
          VCP(I,IN,2)=CMMN*THCONp(I,IN2)/DELXl
          QCP(I,IN)=CMMN
          VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DELXp
          VCF(I,IN,2)=CMMN*THCONf(I,IN2)/DELXl
          QCF(I,IN)=CMMN

C Continue with other nodes (homogeneous) in the layer.
          DO 50 ID=1,NDL-1
            IN1=IN
            IN=IN+1
            IN2=IN
            IHCV=IHCV+1
            CMMN=DTIME/(DELXl*THDNSp(I,IHCV)*THCAPp(I,IHCV))
            VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DELXl
            VCP(I,IN,2)=CMMN*THCONp(I,IN2)/DELXl
            QCP(I,IN)=CMMN
            VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DELXl
            VCF(I,IN,2)=CMMN*THCONf(I,IN2)/DELXl
            QCF(I,IN)=CMMN
   50     CONTINUE
   30   CONTINUE

C Internal surface node
        IN1=IN
        IN=IN+1
        IL=NE
        IHCV=IHCV+1
        CMMN=2.0*DTIME/(DELXl*THDNSp(I,IHCV)*THCAPp(I,IHCV))
        VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DELXl
        VCP(I,IN,2)=CMMN
        QCP(I,IN)=CMMN
        VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DELXl
        VCF(I,IN,2)=CMMN
        QCF(I,IN)=CMMN
   10 CONTINUE
      RETURN
      END
