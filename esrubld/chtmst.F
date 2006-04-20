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

C ***************************** MZCOE4M ********************************
C MZCOE4M 
C *********************************************************************
      SUBROUTINE MZCOE4M(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)
      COMMON/VTHP21/THCONp(MS,MN),THDNSp(MS,MHCV),THCAPp(MS,MHCV)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR3D150/INDX3,DTIME

      NC=NCONST(ICOMP)
      DO 10 I=1,NC

C First node in the construction.
        IN=1

C First layer in the construction.
        IL=1

C First homogeneous control volume.
        IHCV=1

C Node is situated at 'outside' surface.
        NH=NNDL(ICOMP,I,IL)
        DXHl=THRMLI(ICOMP,I,IL,4)/FLOAT(NH)
        CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
        CAP=(DXHl/2.0)*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
        CMMN=DTIME/CAP
        VCP(I,IN,1)=CMMN
        VCP(I,IN,2)=CMMN*THCONp(I,IN)/DXHl
        QCP(I,IN)=CMMN
        VCF(I,IN,1)=CMMN
        VCF(I,IN,2)=CMMN*THCONf(I,IN)/DXHl
        QCF(I,IN)=CMMN

C Continue with other nodes (homogeneous) in the first layer.
        DO 20 INL=1,NH-1
          IN1=IN
          IN=IN+1
          IN2=IN
          IHCV=IHCV+1
          CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
          CAP=DXHl*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
          CMMN=DTIME/CAP
          VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DXHl
          VCP(I,IN,2)=CMMN*THCONp(I,IN2)/DXHl
          QCP(I,IN)=CMMN
          VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DXHl
          VCF(I,IN,2)=CMMN*THCONf(I,IN2)/DXHl
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

C DXHp and DXHl are previous and current layers' space steps.
          IP=IL-1
          DXHp=DXHl
          CALL MSTCAP(ICOMP,I,IP,IN,CAPUv,CAPMl)
          CAP1=(DXHp/2.)*THDNSp(I,IHCV1)*THCAPp(I,IHCV1)+CAPUv+CAPMl
          NH=NNDL(ICOMP,I,IL)
          DXHl=THRMLI(ICOMP,I,IL,4)/FLOAT(NH)
          CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
          CAP2=(DXHl/2.)*THDNSp(I,IHCV2)*THCAPp(I,IHCV2)+CAPUv+CAPMl
          CAP=CAP1+CAP2
          CMMN=DTIME/CAP
          VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DXHp
          VCP(I,IN,2)=CMMN*THCONp(I,IN2)/DXHl
          QCP(I,IN)=CMMN
          VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DXHp
          VCF(I,IN,2)=CMMN*THCONf(I,IN2)/DXHl
          QCF(I,IN)=CMMN

C Continue with other nodes (homogeneous) in the layer.
          DO 50 ID=1,NH-1
            IN1=IN
            IN=IN+1
            IN2=IN
            IHCV=IHCV+1
            CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
            CAP=DXHl*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
            CMMN=DTIME/CAP
            VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DXHl
            VCP(I,IN,2)=CMMN*THCONp(I,IN2)/DXHl
            QCP(I,IN)=CMMN
            VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DXHl
            VCF(I,IN,2)=CMMN*THCONf(I,IN2)/DXHl
            QCF(I,IN)=CMMN
   50     CONTINUE
   30   CONTINUE

C Internal surface node
        IN1=IN
        IN=IN+1
        IL=NE
        IHCV=IHCV+1
        CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
        CAP=(DXHl/2.0)*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
        CMMN=DTIME/CAP
        VCP(I,IN,1)=CMMN*THCONp(I,IN1)/DXHl
        VCP(I,IN,2)=CMMN
        QCP(I,IN)=CMMN
        VCF(I,IN,1)=CMMN*THCONf(I,IN1)/DXHl
        VCF(I,IN,2)=CMMN
        QCF(I,IN)=CMMN
   10 CONTINUE
      RETURN
      END

C ***************************** MZCOE5 ********************************
C MZCOE5 
C *********************************************************************
      SUBROUTINE MZCOE5(ICOMP)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP21/THCONp(MS,MN),THDNSp(MS,MHCV),THCAPp(MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D04/GAM,RGAM
      COMMON/GR3D150/INDX3,DTIME
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST24/BETAip(MCOM,MS),BETAop(MCOM,MS)
      COMMON/MOIST25/BETAif(MCOM,MS),BETAof(MCOM,MS)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST68/Ud(MS,MNM),Udair
      COMMON/MOIST70/ACAP

C First, calculate the moisture content variation (considering the 
C effect of relatation factors).
      CALL DELUAF(ICOMP)
      NC=NCONST(ICOMP)
      N1=NC+1
      N3=NC+3
      SUM=0.0
      DO 10 I=1,NC
        Asrf=SNA(ICOMP,I)

C First node in the construction.
        IN=1

C First layer in the construction.
        IL=1

C First homogeneous control volume.
        IHCV=1
        NH=NNDL(ICOMP,I,IL)
        NM=MNDL(ICOMP,I,IL)
        DXHl=THRMLI(ICOMP,I,IL,4)/FLOAT(NH)
        DXMl=THRMLI(ICOMP,I,IL,4)/FLOAT(NM)

C The first and last moisture nodes in the current heat control volume.
        CALL MSRTND(ICOMP,I,IL,IN,IMS,IME)

C Convection.
C Connection with outside.
        II=IMS
        TIp=TMPp(I,II)
        TIf=TMPf(I,II)
        PIp=Pp(ICOMP,I,II)
        PIf=Pf(ICOMP,I,II)
        TJp=ATP(ICOMP,I)
        TJf=ATF(ICOMP,I)
        PJp=APp(ICOMP,I)
        PJf=APf(ICOMP,I)
        CRSp1=BETAop(ICOMP,I)
        CRSf1=BETAof(ICOMP,I)

C Calculate the vapour flow in (kgv/m2 s).
        FLWp1=CRSp1*(PJp-PIp)
        IF(FLWp1.GT.0.0)THEN

C Calculate heat flux in (W/m2).
          FLXp1=RGAM*FLWp1*Hv01(TJp)
        ELSE
          FLXp1=RGAM*FLWp1*Hv01(TIp)
        ENDIF
        FLWf1=CRSf1*(PJf-PIf)
        IF(FLWf1.GT.0.0)THEN
          FLXf1=GAM*FLWf1*Hv01(TJf)
        ELSE
          FLXf1=GAM*FLWf1*Hv01(TIf)
        ENDIF

C Connection with the inner node.
        CALL HTFLX2(ICOMP,I,IME,DXMl,FLXp2,FLXf2)

C Change in stored energy due to change in stored moisture mass (W/m2).
        HT=0.0
        DO 20 IM=IMS,IME
          TCf=TMPf(I,IM)
          HTJv=Hv01(TCf)*Ud(I,IM)
          HTJl=Hl01(TCf)*(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
          HT=HT+(HTJv+HTJl)/(DTIME*Asrf)
   20   CONTINUE

C Modify the present term for the current heat node.
        CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
        CAP=(DXHl/2.0)*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
        CMMN=DTIME/CAP
        W(I,IN,5)=W(I,IN,5)+(FLXp1+FLXf1+FLXp2+FLXf2-HT)*CMMN

C Continue with other nodes (homogeneous) in the first layer.
        DO 30 INL=1,NH-1
          IN=IN+1
          IHCV=IHCV+1

C Connection with the outer node.
          FLXp1=-FLXp2
          FLXf1=-FLXf2

C Connection with the inner node.
          CALL MSRTND(ICOMP,I,IL,IN,IMS,IME)
          CALL HTFLX2(ICOMP,I,IME,DXMl,FLXp2,FLXf2)


C Change in stored energy due to change in stored moisture mass (W/m2).
          HT=0.0
          DO 40 IM=IMS,IME
            TCf=TMPf(I,IM)
            HTJv=Hv01(TCf)*Ud(I,IM)
            HTJl=Hl01(TCf)*(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
            HT=HT+(HTJv+HTJl)/(DTIME*Asrf)
   40     CONTINUE

C Modify the present term for the current heat node.
          CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
          CAP=DXHl*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
          CMMN=DTIME/CAP
          W(I,IN,5)=W(I,IN,5)+(FLXp1+FLXf1+FLXp2+FLXf2-HT)*CMMN
   30   CONTINUE

C Continue with other layers in the construction.
        NE=NELTS(ICOMP,I)
        DO 50 IL=2,NE
          IN=IN+1
          IHCV=IHCV+2
          IHCV1=IHCV-1
          IHCV2=IHCV

C DXHp and DXHl are previous and current layers' space steps.
          IP=IL-1
          DXHp=DXHl
          NH=NNDL(ICOMP,I,IL)
          NM=MNDL(ICOMP,I,IL)
          DXHl=THRMLI(ICOMP,I,IL,4)/FLOAT(NH)
          DXMl=THRMLI(ICOMP,I,IL,4)/FLOAT(NM)

C Connection with the outer node.
          FLXp1=-FLXp2
          FLXf1=-FLXf2

C Connection with the inner node.
          CALL MSRTND(ICOMP,I,IP,IN,IMS,IME2)
          CALL MSRTND(ICOMP,I,IL,IN,IMS2,IME)
          CALL HTFLX2(ICOMP,I,IME,DXMl,FLXp2,FLXf2)

C Change in stored energy due to change in stored moisture mass (W/m2).
          HT=0.0
          DO 60 IM=IMS,IME
            TCf=TMPf(I,IM)
            HTJv=Hv01(TCf)*Ud(I,IM)
            HTJl=Hl01(TCf)*(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
            HT=HT+(HTJv+HTJl)/(DTIME*Asrf)
   60     CONTINUE

C Modify the present term for the current heat node.
          CALL MSTCAP(ICOMP,I,IP,IN,CAPUv,CAPMl)
          CAP1=(DXHp/2.)*THDNSp(I,IHCV1)*THCAPp(I,IHCV1)+CAPUv+CAPMl
          CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
          CAP2=(DXHl/2.)*THDNSp(I,IHCV2)*THCAPp(I,IHCV2)+CAPUv+CAPMl
          CAP=CAP1+CAP2
          CMMN=DTIME/CAP
          W(I,IN,5)=W(I,IN,5)+(FLXp1+FLXf1+FLXp2+FLXf2-HT)*CMMN

C Continue with other nodes (homogeneous) in the layer.
          DO 70 ID=1,NH-1
           IN=IN+1
           IHCV=IHCV+1

C Connection with the outer node.
           FLXp1=-FLXp2
           FLXf1=-FLXf2

C Connection with the inner node.
           CALL MSRTND(ICOMP,I,IL,IN,IMS,IME)
           CALL HTFLX2(ICOMP,I,IME,DXMl,FLXp2,FLXf2)

C Change in stored energy due to change in stored moisture mass (W/m2).
           HT=0.0
           DO 80 IM=IMS,IME
             TCf=TMPf(I,IM)
             HTJv=Hv01(TCf)*Ud(I,IM)
             HTJl=Hl01(TCf)*(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
             HT=HT+(HTJv+HTJl)/(DTIME*Asrf)
   80      CONTINUE

C Modify the present term for the current heat node.
           CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
           CAP=DXHl*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
           CMMN=DTIME/CAP
           W(I,IN,5)=W(I,IN,5)+(FLXp1+FLXf1+FLXp2+FLXf2-HT)*CMMN
   70     CONTINUE
   50   CONTINUE

C Internal surface node
        IN=IN+1
        IL=NE
        IHCV=IHCV+1

C Connection with the outer node.
        FLXp1=-FLXp2
        FLXf1=-FLXf2

C Connection with the air node.
        CALL MSRTND(ICOMP,I,IL,IN,IMS,IME)
        II=IME
        TIp=TMPp(I,II)
        TIf=TMPf(I,II)
        PIp=Pp(ICOMP,I,II)
        PIf=Pf(ICOMP,I,II)
        TJp=TPA(ICOMP)
        TJf=TFA(ICOMP)
        PJp=Ppair(ICOMP)
        PJf=Pfair(ICOMP)
        CRSp2=BETAip(ICOMP,I)
        CRSf2=BETAif(ICOMP,I)
        FLWp2=CRSp2*(PJp-PIp)
        IF(FLWp2.GT.0.0)THEN
          FLXp2=RGAM*FLWp2*Hv01(TJp)
        ELSE
          FLXp2=RGAM*FLWp2*Hv01(TIp)
        ENDIF
        FLWf2=CRSf2*(PJf-PIf)
        IF(FLWf2.GT.0.0)THEN
          FLXf2=GAM*FLWf2*Hv01(TJf)
        ELSE
          FLXf2=GAM*FLWf2*Hv01(TIf)
        ENDIF

C Change in stored energy due to change in stored moisture mass (W/m2).
        HT=0.0
        DO 90 IM=IMS,IME
          TCf=TMPf(I,IM)
          HTJv=Hv01(TCf)*Ud(I,IM)
          HTJl=Hl01(TCf)*(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
          HT=HT+(HTJv+HTJl)/(DTIME*Asrf)
   90   CONTINUE

C Modify the present term for the current heat node.
        CALL MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
        CAP=(DXHl/2.0)*THDNSp(I,IHCV)*THCAPp(I,IHCV)+CAPUv+CAPMl
        CMMN=DTIME/CAP
        E(I,N3)=E(I,N3)+(FLXp1+FLXf1+FLXp2+FLXf2-HT)*CMMN

C Save the flux for air node in (Watts).
        SUM=SUM-(FLXp2+FLXf2)*Asrf
   10 CONTINUE

C Air node.
C Phase change in (Watts).
C Xair in (kgv/Kgair), and Uair in (Kgv)
      TCf=TFA(ICOMP)
      HTJv=Hv01(TCf)*Udair
      HTJl=Hl01(TCf)*(Ffair(ICOMP)-Fpair(ICOMP))
      HT=(HTJv+HTJl)/DTIME
      E(N1,N3)=E(N1,N3)+(SUM-HT)*ACAP
      RETURN
      END

C ********************************************************************
      SUBROUTINE HTFLX2(ICOMP,I,IME,DXl,FLXp2,FLXf2)
#include "building.h"

      COMMON/GR1D04/GAM,RGAM
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST27/DELTp(MS,MNM),DELTf(MS,MNM)
      COMMON/MOIST28/DTPp(MS,MNM),DTPf(MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)

      II=IME
      IJ=IME+1
      TIp=TMPp(I,II)
      TIf=TMPf(I,II)
      PIp=Pp(ICOMP,I,II)
      PIf=Pf(ICOMP,I,II)
      TJp=TMPp(I,IJ)
      TJf=TMPf(I,IJ)
      PJp=Pp(ICOMP,I,IJ)
      PJf=Pf(ICOMP,I,IJ)
      CRSp2=DELTp(I,II)/DXl
      CRSf2=DELTf(I,II)/DXl
      DTPp2=DTPp(I,II)/DXl
      DTPf2=DTPf(I,II)/DXl

C Calculate the vapour flow in (kgv/m2 s).
      FLWp2=CRSp2*(PJp-PIp)+DTPp2*(TJp-TIp)
      IF(FLWp2.GT.0.0)THEN

C Calculate heat flux in (W/m2).
        FLXp2=RGAM*FLWp2*Hv01(TJp)
      ELSE
        FLXp2=RGAM*FLWp2*Hv01(TIp)
      ENDIF
      FLWf2=CRSf2*(PJf-PIf)+DTPf2*(TJf-TIf)
      IF(FLWf2.GT.0.0)THEN
        FLXf2=GAM*FLWf2*Hv01(TJf)
      ELSE
        FLXf2=GAM*FLWf2*Hv01(TIf)
      ENDIF
      RETURN
      END

C **********************************************************************
      SUBROUTINE MSTCAP(ICOMP,I,IL,IN,CAPUv,CAPMl)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)

      CAPUv=0.0
      CAPMl=0.0
      Asrf=SNA(ICOMP,I)

C The first moisture node in the current layer.
      IMF=1
      DO 10 ILYR=1,IL-1
        IMF=IMF+MNDL(ICOMP,I,ILYR)
   10 CONTINUE
      NM=MNDL(ICOMP,I,IL)

C The last moisture node in the current layer.
      IML=IMF+NM
      DELX=THRMLI(ICOMP,I,IL,4)/FLOAT(NM)
      Uh=SORP(ICOMP,I,IL,1)
      CFF=SORP(ICOMP,I,IL,2)
      EXPT=SORP(ICOMP,I,IL,3)
      CALL MSRTND(ICOMP,I,IL,IN,IMS,IME)
      DO 20 IM=IMS,IME
        IF(IM.EQ.IMF.OR.IM.EQ.IML)THEN
          DX=DELX/2.0
        ELSE
          DX=DELX
        ENDIF
        TC=TMPp(I,IM)

C CPv in (J/kg K).
        CPv=CPvpr01(TC)
        CPl=SHTH2O(TC)*1000.0
        Psp=Psat01(TC)
        RHp=Pp(ICOMP,I,IM)/Psp

C U in (kg/m3).
        U=U01(RHp,Uh,CFF,EXPT)

C CAPUv in (J/m2 K).
        CAPUv=CAPUv+DX*CPv*U
        CAPMl=CAPMl+CPl*Fp(ICOMP,I,IM)/Asrf
   20 CONTINUE
      RETURN
      END

C **********************************************************************
      SUBROUTINE SHTNEF(ICOMP)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/MOIST81/TNC(MS,MN),QNcnst
      COMMON/MOIST82/TNS(MS),QNsrf
      COMMON/MOIST83/TNair,QNair
      common/ts4/itsitr(msch),itrpas

C Now, for the next time-step, set the present temperature
C and plant injection values equal to the future values.
C If iterating then use present values on subsequent calls.
      if (ITRPAS.eq.0) then
        QNair= QFA(ICOMP)
        QNsrf=QFS(ICOMP)
        QNcnst=QFC(ICOMP)
        TNair=TFA(ICOMP)
        NSUR=NCONST(ICOMP)
        DO 10 I=1,NSUR
          TNS(I)=TFS(ICOMP,I)
          NN=NNDC(ICOMP,I)-1
          DO 10 J=1,NN
            TNC(I,J)=TFC(ICOMP,I,J)
   10   CONTINUE
      else
        QNair= QPA(ICOMP)
        QNsrf=QPS(ICOMP)
        QNcnst=QPC(ICOMP)
        TNair=TPA(ICOMP)
        NSUR=NCONST(ICOMP)
        DO 20 I=1,NSUR
          TNS(I)=TPS(ICOMP,I)
          NN=NNDC(ICOMP,I)-1
          DO 20 J=1,NN
            TNC(I,J)=TPC(ICOMP,I,J)
   20   CONTINUE
      endif
      
      RETURN
      END

C *********************************************************************
C MZMSTU sets up the zone moisture transfer matrix. This is done with
C counting for the possible different temperature node distribution.
C Implicitness degrees between 0 and 1 inclusive are allowed.
C *********************************************************************
      SUBROUTINE DELUAF(ICOMP)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/GR3D150/INDX3,DTIME
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST12/GAMm,RGAMm
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST24/BETAip(MCOM,MS),BETAop(MCOM,MS)
      COMMON/MOIST25/BETAif(MCOM,MS),BETAof(MCOM,MS)
      COMMON/MOIST27/DELTp(MS,MNM),DELTf(MS,MNM)
      COMMON/MOIST28/DTPp(MS,MNM),DTPf(MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST52/Pf(MCOM,MS,MNM),Ff(MCOM,MS,MNM),Sf(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      COMMON/MOIST68/Ud(MS,MNM),Udair

C Consider each multilayered construction within current  zone.
      SUM=0.0
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        Asrf=SNA(ICOMP,I)
        IL=1
        IM=1
        IM2=IM+1
        DELXm=THRMLI(ICOMP,I,IL,4)/FLOAT(MNDL(ICOMP,I,IL))
        PIp=Pp(ICOMP,I,IM)
        PIf=Pf(ICOMP,I,IM)
        TIp=TMPp(I,IM)
        TIf=TMPf(I,IM)

C Connection with outside.
        P1p=APp(ICOMP,I)
        P1f=APf(ICOMP,I)

C Connection with outside, another zone, or similar.
        CRSp1=RGAMm*BETAop(ICOMP,I)*Asrf*DTIME
        CRSf1= GAMm*BETAof(ICOMP,I)*Asrf*DTIME

C Connection with the inner node.
        P2p=Pp(ICOMP,I,IM2)
        P2f=Pf(ICOMP,I,IM2)
        T2p=TMPp(I,IM2)
        T2f=TMPf(I,IM2)
        TERM=Asrf*DTIME/DELXm
        CRSp2=RGAMm*TERM*DELTp(I,IM)
        CRSf2= GAMm*TERM*DELTf(I,IM)
        DTPp2=RGAMm*TERM*DTPp(I,IM)
        DTPf2= GAMm*TERM*DTPf(I,IM)
        FLW1=CRSp1*(P1p-PIp)+CRSf1*(P1f-PIf)
        FLW2=CRSp2*(P2p-PIp)+CRSf2*(P2f-PIf)+
     &       DTPp2*(T2p-TIp)+DTPf2*(T2f-TIf)
        Ud(I,IM)=FLW1+FLW2-(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))

C Continue with other nodes (homogeneous) in the first layer.
        DO 100 ID=1,MNDL(ICOMP,I,IL)-1
          IM=IM+1
          IM2=IM+1
          PIp=Pp(ICOMP,I,IM)
          PIf=Pf(ICOMP,I,IM)
          TIp=TMPp(I,IM)
          TIf=TMPf(I,IM)

C Connection with outer node.
          FLW1=-FLW2

C Connection with the inner node.
          P2p=Pp(ICOMP,I,IM2)
          P2f=Pf(ICOMP,I,IM2)
          T2p=TMPp(I,IM2)
          T2f=TMPf(I,IM2)
          TERM=Asrf*DTIME/DELXm
          CRSp2=RGAMm*TERM*DELTp(I,IM)
          CRSf2= GAMm*TERM*DELTf(I,IM)
          DTPp2=RGAMm*TERM*DTPp(I,IM)
          DTPf2= GAMm*TERM*DTPf(I,IM)
          FLW2=CRSp2*(P2p-PIp)+CRSf2*(P2f-PIf)+
     &         DTPp2*(T2p-TIp)+DTPf2*(T2f-TIf)
          Ud(I,IM)=FLW1+FLW2-(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
  100   CONTINUE

C Continue with other layers in the construction.
        DO 110 IL=2,NELTS(ICOMP,I)
          IM=IM+1
          IM2=IM+1
          PIp=Pp(ICOMP,I,IM)
          PIf=Pf(ICOMP,I,IM)
          TIp=TMPp(I,IM)
          TIf=TMPf(I,IM)

C Connection with outer node.
          FLW1=-FLW2

C Connection with the inner node.
          P2p=Pp(ICOMP,I,IM2)
          P2f=Pf(ICOMP,I,IM2)
          T2p=TMPp(I,IM2)
          T2f=TMPf(I,IM2)
          DELXm=THRMLI(ICOMP,I,IL,4)/FLOAT(MNDL(ICOMP,I,IL))
          TERM=Asrf*DTIME/DELXm
          CRSp2=RGAMm*TERM*DELTp(I,IM)
          CRSf2= GAMm*TERM*DELTf(I,IM)
          DTPp2=RGAMm*TERM*DTPp(I,IM)
          DTPf2= GAMm*TERM*DTPf(I,IM)
          FLW2=CRSp2*(P2p-PIp)+CRSf2*(P2f-PIf)+
     &         DTPp2*(T2p-TIp)+DTPf2*(T2f-TIf)
          Ud(I,IM)=FLW1+FLW2-(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))

C Continue with other (homogeneous) nodes in the layer.
        DO 110 ID=1,MNDL(ICOMP,I,IL)-1
          IM=IM+1
          IM2=IM+1
          PIp=Pp(ICOMP,I,IM)
          PIf=Pf(ICOMP,I,IM)
          TIp=TMPp(I,IM)
          TIf=TMPf(I,IM)

C Connection with outer node.
          FLW1=-FLW2

C Connection with the inner node.
          P2p=Pp(ICOMP,I,IM2)
          P2f=Pf(ICOMP,I,IM2)
          T2p=TMPp(I,IM2)
          T2f=TMPf(I,IM2)
          TERM=Asrf*DTIME/DELXm
          CRSp2=RGAMm*TERM*DELTp(I,IM)
          CRSf2= GAMm*TERM*DELTf(I,IM)
          DTPp2=RGAMm*TERM*DTPp(I,IM)
          DTPf2= GAMm*TERM*DTPf(I,IM)
          FLW2=CRSp2*(P2p-PIp)+CRSf2*(P2f-PIf)+
     &         DTPp2*(T2p-TIp)+DTPf2*(T2f-TIf)
          Ud(I,IM)=FLW1+FLW2-(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))
  110   CONTINUE

C Internal surface node
        IM=IM+1
        PIp=Pp(ICOMP,I,IM)
        PIf=Pf(ICOMP,I,IM)
        TIp=TMPp(I,IM)
        TIf=TMPf(I,IM)

C Connection with outer node.
        FLW1=-FLW2

C Connection with the inner node.
        P2p=Ppair(ICOMP)
        P2f=Pfair(ICOMP)
        T2p=TPA(ICOMP)
        T2f=TFA(ICOMP)
        IL=NELTS(ICOMP,I)
        TERM=Asrf*DTIME/DELXm
        CRSp2=RGAMm*Asrf*DTIME*BETAip(ICOMP,I)
        CRSf2= GAMm*Asrf*DTIME*BETAif(ICOMP,I)
        FLW2=CRSp2*(P2p-PIp)+CRSf2*(P2f-PIf)
        Ud(I,IM)=FLW1+FLW2-(Ff(ICOMP,I,IM)-Fp(ICOMP,I,IM))

C Save moisture flow into air space (kgv).
        SUM=SUM-FLW2
   10 CONTINUE

C Air node.
        Udair=SUM-(Ffair(ICOMP)-Fpair(ICOMP))
      RETURN
      END
