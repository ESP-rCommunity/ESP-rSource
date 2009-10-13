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

C This file contains additional routines developed within 
C the framework of COPERNICUS project "Integrated Design
C Optimization of Building Energy Performance and 
C Indoor Environment".  
C Contents:
C  CALCHUM  - Calculates the equation for the mass concentration 
C             of a chemical, e.g. carbon dioxide. 
C  BNDARHUM - Imposes BCs for the equation of concentration   
C  BLKBNDH  - Imposes BC's on blockages 

C ********************* CALCHUM *********************
C CALCHUM - Calculates the coefficients of the matrix of the 
C humidity (water vapour)-equation and calls the matrix-solver 
C to obtain the distribution - dimensions are in [kg_H2O/kg_humid air] .   
C The air is assumed to be a mixture of "dry air" and H2O. 
C H2O is a gas of a lower density than the air. The user can specify 
C According to Recknagel, Sprenger und Schrameck 97/98, p. 49. 
C persons at light, sedentary activity release water vapour: 
C 20C - 35g/h; 22C - 40g/h; 23C - 50g/h; 24and25C-60/h - local variable 
C "PRODH".  Summer conditions are assumed and therefore the production  
C coded is 60 g/h. A source term is specified in the CV where a person 
C is breathing.  
C 
      SUBROUTINE CALCHUM  
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/BNDCND/FLWIN,XMONIN
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/FLUPRp/DENp(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)      
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/CONCEN/CONf(ntcelx,ntcely,ntcelz),RESORC,SCHMIL,PRODR
      COMMON/HUMIDT/HUMf(ntcelx,ntcely,ntcelz) 
      COMMON/HUMIDp/HUMp(ntcelx,ntcely,ntcelz) 
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/INCAL2/INCALC,INCALH 
      COMMON/CONST/GREAT,small,GRAV
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME
      COMMON/NORMCW/RENORMC,RENORMWV 
      COMMON/ITERAT/NITER
      common/SOLVER/ILISOL

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C Boundary condition commons.  
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

C Common blocks for blockages and small supply openings
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ)

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ
      LOGICAL INCALC,INCALH 
      LOGICAL OK,LINVOL

C Keep for testing:      REAL test1 

C << For equations as concentration and humidity we need writing the 
C << results in results-file to make runs with continuation >> 

      SCHMIH=0.59 
      PRODH=0.0000166666 

      DO 50 I=2,NIM1
        DO 50 J=2,NJM1
          DO 50 K=2,NKM1

C First "GAMH" values are calculated and stored temporarily in AP
C Note that INCALK is a test for the presence of turbulence 
C <<should be rewritten to include the case of MIT-zero-equation model>> 
C <<using ITURB>0 assumes all turbulence is this correct?>>
        IF(ITURB(ICFD).gt.0) THEN 
          AP(I,J,K)=VISCOS/SCHMIH+(VIS(I,J,K)-VISCOS)/PRANDT
        ELSE 
          AP(I,J,K)=VISCOS/SCHMIH
        ENDIF 

C If there are people in the room their source term is 
C calculated and added here. 
C PRODH is the production rate in [kg_H20/s]. 
C Care sould be taken not to multiply later the so calculated here 
C SU_value by the volume of the cv 

C Clear SU term here
        SU(I,J,K)=0.0
 50   CONTINUE

C Should loop through humidity sources here.
      do 60 IV=1,NVOL(ICFD)
        if (IVTYPE(IV,ICFD).eq.20) then
          do 70 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 71 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 72 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)

C VOLHum is in g/h => divide by 1000*3600 to get to required units of kg/s
                SU(I,J,K)=VOLHum(IV,ICFD)/3600000.
 72           continue
 71         continue
 70       continue
        endif
 60   continue

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then
C Loop through whole blockage, set humidity to zero 
        do 10 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          do 20 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 21 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 22 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)   
                HUMf(I,J,K)=0.0
 22           continue
 21         continue
 20       continue
 10     continue
      endif
C 
      DO 100 I=2,NIM1
        DO 1001 J=2,NJM1
          DO 1002 K=2,NKM1

C Compute areas and volume.
            AREAEW=AreaEWP(J,K)
            AREANS=AreaNSP(I,K)
            AREAHL=AreaHLP(I,J)
            VOL=VolP(I,J,K)
            
C Calculate convection coefficients.
            GE=(SIFE(I)*DENf(I,J,K)+SIFW(I+1)*DENf(I+1,J,K))*Uf(I+1,J,K)
            GW=(SIFE(I-1)*DENf(I-1,J,K)+SIFW(I)*DENf(I,J,K))*Uf(I,J,K)
            GN=(SIFN(J)*DENf(I,J,K)+SIFS(J+1)*DENf(I,J+1,K))*Vf(I,J+1,K)
            GS=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))*Vf(I,J,K)
            GH=(SIFH(K)*DENf(I,J,K)+SIFL(K+1)*DENf(I,J,K+1))*Wf(I,J,K+1)
            GL=(SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K))*Wf(I,J,K)

            CE=GE*AREAEW
            CW=GW*AREAEW
            CN=GN*AREANS
            CS=GS*AREANS
            CH=GH*AREAHL
            CL=GL*AREAHL

C Dynamic (transient) term.
            AP0=DENp(I,J,K)*VOL/DT
            APF=DENf(I,J,K)*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C False dynamic term, use te value for temperature: fsDTT 
            APfals=DENf(i,j,k)*VOL/fsDTT

C Calculate diffusion coefficients  
            GAMAE=SIFE(I)*AP(I,J,K)+SIFW(I+1)*AP(I+1,J,K)
            GAMAW=SIFE(I-1)*AP(I-1,J,K)+SIFW(I)*AP(I,J,K)
            GAMAN=SIFN(J)*AP(I,J,K)+SIFS(J+1)*AP(I,J+1,K)
            GAMAS=SIFN(J-1)*AP(I,J-1,K)+SIFS(J)*AP(I,J,K)
            GAMAH=SIFH(K)*AP(I,J,K)+SIFL(K+1)*AP(I,J,K+1)
            GAMAL=SIFH(K-1)*AP(I,J,K-1)+SIFL(K)*AP(I,J,K)

            DFE=GAMAE*AREAEW/DXEP(I)
            DFW=GAMAW*AREAEW/DXPW(I)
            DFN=GAMAN*AREANS/DYNP(J)
            DFS=GAMAS*AREANS/DYPS(J)
            DFH=GAMAH*AREAHL/DZHP(K)
            DFL=GAMAL*AREAHL/DZPL(K)

C Main coefficients:
C Hybrid scheme.
C            AE(I,J,K)=AMAX1(ABS(0.5*CE),DFE)-0.5*CE
C            AW(I,J,K)=AMAX1(ABS(0.5*CW),DFW)+0.5*CW
C            AN(I,J,K)=AMAX1(ABS(0.5*CN),DFN)-0.5*CN
C            AS(I,J,K)=AMAX1(ABS(0.5*CS),DFS)+0.5*CS
C            AH(I,J,K)=AMAX1(ABS(0.5*CH),DFH)-0.5*CH
C            AL(I,J,K)=AMAX1(ABS(0.5*CL),DFL)+0.5*CL

C Power law scheme.
            AE(I,J,K)=APLAWCF(DFE,-CE)
            AW(I,J,K)=APLAWCF(DFW,CW)
            AN(I,J,K)=APLAWCF(DFN,-CN)
            AS(I,J,K)=APLAWCF(DFS,CS)
            AH(I,J,K)=APLAWCF(DFH,-CH)
            AL(I,J,K)=APLAWCF(DFL,CL)

C Source coefficients.
            SU(I,J,K)=SU(I,J,K)+AP0*HUMp(I,J,K)+
     &                APfals*HUMf(I,J,K)+CP*HUMf(I,J,K)
            SP(I,J,K)=-AP0-APfals-CP 
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE
C Apply boundary conditions.
C No difference between turbulent and laminar treatments, because 
C only zero-gradient type BC's  
        CALL BNDARHUM  

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then
C Go and set the boundary conditions  
        do 2016 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          call BLKBNDH(IV)
 2016   continue
      endif 

C Remaining coefficients and residual source calculation.
      RESORC=0.0
      RENORMC=0.0
C Keep for testing:      test1=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &              AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            RESOR=AE(I,J,K)*HUMf(i+1,j,K)+AW(I,J,K)*HUMf(i-1,j,K)+
     &            AN(I,J,K)*HUMf(i,j+1,K)+AS(I,J,K)*HUMf(i,j-1,K)+
     &            AH(I,J,K)*HUMf(i,j,K+1)+AL(I,J,K)*HUMf(i,j,K-1)-
     &            AP(I,J,K)*HUMf(i,j,K)+SU(I,J,K)
            RENORM=AP(I,J,K)*HUMf(I,J,K)
C Check for blockages. 
      if(NBLK(ICFD).ne.0) then
C If inside the blockage set resor to zero.
        do 2006 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          OK=LINVOL(I,J,K,
     &            IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &            JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &            KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
          if(OK) then
            resor=0.0
          endif
 2006   continue
      endif
C 
            VOL=VolP(I,J,K)
            SORVOL=GREAT*VOL
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=RENORM/SORVOL
            ENDIF
C keep for testing:            test1=test1+SU(I,J,K)
            RESORC=RESORC+ABS(RESOR)
            RENORMC=RENORMC+ABS(RENORM)

C Under-relaxation. Using always values from temperature equation 
            AP(I,J,K)=AP(I,J,K)/URFT 
            SU(I,J,K)=SU(I,J,K)+(1.0-URFT)*AP(I,J,K)*HUMf(I,J,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Keep for testing:        write(*,*)'all internal humidity sources=',test1 

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then
C If inside the blockage force concentration to zero. 
        do 2007 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          OK=LINVOL(I,J,K,
     &            IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &            JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &            KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
          if(OK) then

C Set the humidity inside the blockage equal to zero. 
            AE(I,J,K)=0.0
            AW(I,J,K)=0.0
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
            AH(I,J,K)=0.0
            AL(I,J,K)=0.0
            SP(I,J,K)=0.0
            SU(I,J,K)=0.0
            AP(I,J,K)=1.0
          endif

 2007   continue
      endif 
C 
C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,2,NI,NJ,NK,HUMf,NSWPT)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,HUMf,NSWPT)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,HUMf,NSWPT)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,HUMf,NSWPT)
      endif

      RETURN
      END

C ********************* BNDARHUM *********************
C BNDARHUM - Establishes the boundary conditions for the 
C concentration equation. There is no difference between 
C laminar or turbulent cases - at solid walls the BC's are 
C always of the zero-gradient type. 

      SUBROUTINE BNDARHUM                                                      
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),ICNACT(MCFND,MNZ)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/CONST/GREAT,small,GRAV
      COMMON/HUMIDT/HUMf(ntcelx,ntcely,ntcelz) 
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/MFS/IMFACT,NOSTOP,MITMFS
      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)

      CHARACTER*72 HSID
      character outs*124

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ
      LOGICAL BHS

C <<probably missing logical IMITZ in conc.F and locmage.F>>

C If flow is 1D or 2D make coefficients in other directions zero. 
      IF(.NOT.INCALU)THEN
        I=2
        DO 100 J=1,NJ
          DO 1001 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 1001     CONTINUE
 100    CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 105 I=1,NI
          DO 1051 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1051     CONTINUE
 105    CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 110 I=1,NI
          DO 1101 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 1101     CONTINUE
 110    CONTINUE
      ENDIF

C Examine each opening in turn.
      DO 300 L=1,NOPEN(ICFD)
C Inlets 
      IF(FIXM(L,ICFD).GE.0.0)THEN

C Iterate over cells covering opening.
        DO 101 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
          DO 1011 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 1012 K=KOPENi(L,ICFD),KOPENf(L,ICFD)

C `Prescribed velocity' type openings (inlets only): 
C To keep the correct balance of the water contents in the room 
C so that in the outlet its average value corresponds to what is produced 
C in the room, the diffusion in the inlet openings is set to zero; 
C UPWIND scheme is applied there. The relative humidity is set to 50%  
C which is calculated with respect to the temperature in the inlet. 
C The corresponding coefficients A_nb are recalculated.  
C <<other relative humidities could be prescribed by the user in dfd-file>>
C <<we should make the software engineering for obstacles and 
C <<small supply orifices (real area, real velocity or flowrate, 
C <<the location of the cv - which wall and indexes>> 
              IF(IWOPEN(L,ICFD).EQ.1)THEN

C Opening in west.
C first test the temperature and prescribe a relative humidity of 50% 
                TSAT=Tf(1,J,K)

C The following equation is taken from Recknagel 97/98, p. 113
        PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
      HUMf(I-1,J,K)=0.5*PSAT*18.02/(0.5*PSAT*18.02+100000.0*28.96)
                GW=(SIFE(1)*DENf(1,J,K)+SIFW(2)*DENf(2,J,K))*Uf(2,J,K)
                CW=GW*AreaEWP(J,K)
                AW(2,J,K)=AMAX1(CW,0.0)
c  
c << here is an example how to do in the future
c the bc for a prescribed humidity, e.g. as 0.123 kg/kg >> 
c               HUMf(I-1,J,K)=0.123 

                SU(2,J,K)=SU(2,J,K)+AW(2,J,K)*HUMf(I-1,J,K)
                SP(2,J,K)=SP(2,J,K)-AW(2,J,K)
                AW(2,J,K)=0.0

C Keep for testing:        write(*,*)'Hum_west=',HUMf(I-1,J,K) 
C Keep for testing:        write(*,*)'Flux _west=',SU(2,J,K)
              ELSEIF(IWOPEN(L,ICFD).EQ.2)THEN

C Opening in east.
C first test the temperature and prescribe a relative humidity of 50% 
                TSAT=Tf(I+1,J,K)

C The following equation is taken from Recknagel 97/98, p. 113
        PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]        
      HUMf(I+1,J,K)=0.5*PSAT*18.02/(0.5*PSAT*18.02+100000.0*28.96)
                GE=(SIFE(NIM1)*DENf(NIM1,J,K)+
     &              SIFW(NIM1+1)*DENf(NIM1+1,J,K))*Uf(NIM1+1,J,K)
                CE=GE*AreaEWP(J,K)
                AE(NIM1,J,K)=AMAX1(-CE,0.0)
                SU(NIM1,J,K)=SU(NIM1,J,K)+AE(NIM1,J,K)*HUMf(I+1,J,K)
                SP(NIM1,J,K)=SP(NIM1,J,K)-AE(NIM1,J,K)
                AE(NIM1,J,K)=0.0
              ELSEIF(IWOPEN(L,ICFD).EQ.3)THEN

C Opening in south.
C first test the temperature and prescribe a relative humidity of 50% 
                TSAT=Tf(I,1,K)

C The following equation is taken from Recknagel 97/98, p. 113
        PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]        
      HUMf(I,J-1,K)=0.5*PSAT*18.02/(0.5*PSAT*18.02+100000.0*28.96)
                GS=(SIFN(1)*DENf(I,1,K)+SIFS(2)*DENf(I,2,K))*Vf(I,2,K)
                CS=GS*AreaNSP(I,K)
                AS(I,2,K)=AMAX1(CS,0.0)
                SU(I,2,K)=SU(I,2,K)+AS(I,2,K)*HUMf(I,J-1,K)
                SP(I,2,K)=SP(I,2,K)-AS(I,2,K)
                AS(I,2,K)=0.0
              ELSEIF(IWOPEN(L,ICFD).EQ.4)THEN

C Opening in north.
C first test the temperature and prescribe a relative humidity of 50% 
                TSAT=Tf(I,J+1,K)

C The following equation is taken from Recknagel 97/98, p. 113
        PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]        
      HUMf(I,J+1,K)=0.5*PSAT*18.02/(0.5*PSAT*18.02+100000.0*28.96)
                GN=(SIFN(NJM1)*DENf(I,NJM1,K)+
     &          SIFS(NJM1+1)*DENf(I,NJM1+1,K))*Vf(I,NJM1+1,K)
                CN=GN*AreaNSP(I,K)
                AN(I,NJM1,K)=AMAX1(-CN,0.0)
                SU(I,NJM1,K)=SU(I,NJM1,K)+AN(I,NJM1,K)*HUMf(I,J+1,K)
                SP(I,NJM1,K)=SP(I,NJM1,K)-AN(I,NJM1,K)
                AN(I,NJM1,K)=0.0
              ELSEIF(IWOPEN(L,ICFD).EQ.5)THEN

C Opening in low.
C first test the temperature and prescribe a relative humidity of 50% 
                TSAT=Tf(I,J,1)

C The following equation is taken from Recknagel 97/98, p. 113
        PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]        
      HUMf(I,J,K-1)=0.5*PSAT*18.02/(0.5*PSAT*18.02+100000.0*28.96)
                GL=(SIFH(1)*DENf(I,J,1)+SIFL(2)*DENf(I,J,2))*Wf(I,J,2)
                CL=GL*AreaHLP(I,J)
                AL(I,J,2)=AMAX1(CL,0.0)
                SU(I,J,2)=SU(I,J,2)+AL(I,J,2)*HUMf(I,J,K-1)
                SP(I,J,2)=SP(I,J,2)-AL(I,J,2)
                AL(I,J,2)=0.0
              ELSEIF(IWOPEN(L,ICFD).EQ.6)THEN

C Opening in high.
C first test the temperature and prescribe a relative humidity of 50% 
                TSAT=Tf(I,J,K+1)

C The following equation is taken from Recknagel 97/98, p. 113
        PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]        
      HUMf(I,J,K+1)=0.5*PSAT*18.02/(0.5*PSAT*18.02+100000.0*28.96)
                GH=(SIFH(NKM1)*DENf(I,J,NKM1)+
     &              SIFL(NKM1+1)*DENf(I,J,NKM1+1))*Wf(I,J,NKM1+1)
                CH=GH*AreaHLP(I,J)
                AH(I,J,NKM1)=AMAX1(-CH,0.0)
                SU(I,J,NKM1)=SU(I,J,NKM1)+AH(I,J,NKM1)*HUMf(I,J,K+1)
                SP(I,J,NKM1)=SP(I,J,NKM1)-AH(I,J,NKM1)
                AH(I,J,NKM1)=0.0
              ENDIF
 1012       CONTINUE
 1011     CONTINUE
 101    CONTINUE
       ENDIF 
C Examine next opening.
 300  CONTINUE

C Outlets.
C For all type of outlets - `zero-gradient type' boundary conditions. 
C <<The average concentration in outlets is also calculated (summed in AVEHUM)>>
C <<Summation of the massflow is made in SUMFLU  >>
C <<Trace is hardwired to off - switch on by changing ITRC=1 >>
      ITRC=0 
      AVEHUM=0.0 
      SUMFLU=0.0 
      DO 415 M=1,NOPEN(ICFD)
        IF(FIXM(M,ICFD).LT.0.) THEN

C West wall.
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN
            I=IOPENi(M,ICFD)
            DO 900 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9001 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
                HUMf(I-1,J,K)=HUMf(I,J,K)
                AREAEW=AreaEWP(J,K)
                if (ITRC.eq.1) then
                  AVEHUM=AVEHUM+
     &                     DENf(I,J,K)*AREAEW*Uf(I,J,K)*HUMf(I-1,J,K) 
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREAEW*Uf(I,J,K)
                endif
 9001         CONTINUE
 900        CONTINUE

C East wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.2) THEN
            I=IOPENi(M,ICFD)
            DO 902 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9021 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AE(I,J,K)=0.0
                HUMf(I+1,J,K)=HUMf(I,J,K)
                AREAEW=AreaEWP(J,K)
                if (ITRC.eq.1) then
                  AVEHUM=AVEHUM+
     &                     DENf(I,J,K)*AREAEW*Uf(I+1,J,K)*HUMf(I+1,J,K) 
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREAEW*Uf(I+1,J,K)
                endif
 9021         CONTINUE
 902        CONTINUE

C South wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.3) THEN
            J=JOPENi(M,ICFD)
            DO 904 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9041 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AS(I,J,K)=0.0
                HUMf(I,J-1,K)=HUMf(I,J,K)
                AREANS=AreaNSP(I,K)
                if (ITRC.eq.1) then
                  AVEHUM=AVEHUM+
     &                     DENf(I,J,K)*AREANS*Vf(I,J,K)*HUMf(I,J-1,K)
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREANS*Vf(I,J,K)
                endif
 9041         CONTINUE
 904        CONTINUE

C North wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.4) THEN
            J=JOPENi(M,ICFD)
            DO 906 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9061 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AN(I,J,K)=0.0
                HUMf(I,J+1,K)=HUMf(I,J,K)
                AREANS=AreaNSP(I,K)
                if (ITRC.eq.1) then
                  AVEHUM=AVEHUM+
     &                     DENf(I,J,K)*AREANS*Vf(I,J+1,K)*HUMf(I,J+1,K)
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREANS*Vf(I,J+1,K)
                endif
 9061         CONTINUE
 906        CONTINUE

C Low wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.5) THEN
            K=KOPENi(M,ICFD)
            DO 908 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9081 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AL(I,J,K)=0.0
                HUMf(I,J,K-1)=HUMf(I,J,K)
                AREAHL=AreaHLP(I,J)
                if (ITRC.eq.1) then
                  AVEHUM=AVEHUM+
     &                     DENf(I,J,K)*AREAHL*Wf(I,J,K)*HUMf(I,J,K-1)
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREAHL*Wf(I,J,K)
                endif
 9081         CONTINUE
 908        CONTINUE

C High wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.6) THEN
            K=KOPENi(M,ICFD)
            DO 910 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9101 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AH(I,J,K)=0.0
                HUMf(I,J,K+1)=HUMf(I,J,K)
                AREAHL=AreaHLP(I,J)
                if (ITRC.eq.1) then
                  AVEHUM=AVEHUM+
     &                     DENf(I,J,K)*AREAHL*Wf(I,J,K+1)*HUMf(I,J,K+1)
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREAHL*Wf(I,J,K+1)
                endif
 9101         CONTINUE
 910        CONTINUE
          ENDIF
        ENDIF
 415  CONTINUE

C Here te average value of concentration in the outlets is calculated 
C <<Here we need to make an output of the average outlet-concentration >>
C <<on screen and make it available during testing (ltest-variable)>>
      if (ITRC.eq.1) then
        write(outs,*)'Average humidity=',AVEHUM/(anotzero(SUMFLU)),
     &            ' Humidity flux out=',AVEHUM
        call edisp(iuout,outs)
      endif

C Apply zero-gradient-type BC's on each solid boundary in turn.
      DO 425 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C West wall.
        IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)
          DO 430 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4301 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0 
              HUMf(I-1,J,K)=HUMf(I,J,K)
 4301       CONTINUE
 430      CONTINUE

C East wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DO 435 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4351 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0 
              HUMf(I+1,J,K)=HUMf(I,J,K)
 4351       CONTINUE
 435      CONTINUE

C South wall.
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DO 440 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0 
              HUMf(I,J-1,K)=HUMf(I,J,K)
 4401       CONTINUE
 440      CONTINUE

C North wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DO 445 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4451 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0 
              HUMf(I,J+1,K)=HUMf(I,J,K)
 4451       CONTINUE
 445      CONTINUE

C Low wall.
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DO 450 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4501 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0 
              HUMf(I,J,K-1)=HUMf(I,J,K)
 4501       CONTINUE
 450      CONTINUE

C High wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DO 455 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4551 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0 
              HUMf(I,J,K+1)=HUMf(I,J,K)
 4551       CONTINUE
 455      CONTINUE

        ENDIF
 425  CONTINUE

      RETURN
      END

C ********************* BLKBNDH *********************
C BLKBNDH - set boundary conditions for the H2O due to solid 
C face for blockages. 
C Zero-gradient type of BC's are applied 
C
      SUBROUTINE BLKBNDH(IBLK)
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C Set values for all faces.
      do 40 IFACE=1,6
        IST=IVCELLS(IBLK,ICFD,1)
        IFN=IVCELLS(IBLK,ICFD,2)
        JST=JVCELLS(IBLK,ICFD,1)
        JFN=JVCELLS(IBLK,ICFD,2)
        KST=KVCELLS(IBLK,ICFD,1)
        KFN=KVCELLS(IBLK,ICFD,2)

c      write(*,*)'UP_istfn,jstfn,kstfn=',IST,IFN,JST,JFN,KST,KFN

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
        if (IFACE.eq.1) then
          IST=IVCELLS(IBLK,ICFD,2)+1
          IFN=IVCELLS(IBLK,ICFD,2)+1
        elseif (IFACE.eq.2) then
          IST=IVCELLS(IBLK,ICFD,1)-1
          IFN=IVCELLS(IBLK,ICFD,1)-1
        elseif (IFACE.eq.3) then
          JST=JVCELLS(IBLK,ICFD,2)+1
          JFN=JVCELLS(IBLK,ICFD,2)+1
        elseif (IFACE.eq.4) then
          JST=JVCELLS(IBLK,ICFD,1)-1
          JFN=JVCELLS(IBLK,ICFD,1)-1
        elseif (IFACE.eq.5) then
          KST=KVCELLS(IBLK,ICFD,2)+1
          KFN=KVCELLS(IBLK,ICFD,2)+1
        elseif (IFACE.eq.6) then
          KST=KVCELLS(IBLK,ICFD,1)-1
          KFN=KVCELLS(IBLK,ICFD,1)-1
        endif

c      write(*,*)'LO_istfn,jstfn,kstfn=',IST,IFN,JST,JFN,KST,KFN

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through whole blockage.
        do 10 II=IST,IFN
          do 20 JJ=JST,JFN
            do 30 KK=KST,KFN

c      write(*,*)'L2=i.,j.,k.,IFACE',IST,IFN,JST,JFN,KST,KFN,IFACE 

C Disconnect cell from blockage (i.e. set A?=0.)
C Blockage on West face of adjacent cell.
              if (IFACE.eq.1) then
                AW(II,JJ,KK)=0.

C Blockage on East face of adjacent cell.
              elseif (IFACE.eq.2) then
                AE(II,JJ,KK)=0.

C Blockage on South face of adjacent cell.
              elseif (IFACE.eq.3) then
                AS(II,JJ,KK)=0.

C Blockage on North face of adjacent cell.
              elseif (IFACE.eq.4) then
                AN(II,JJ,KK)=0.

C Blockage on Low face of adjacent cell.
              elseif (IFACE.eq.5) then
                AL(II,JJ,KK)=0.

C Blockage on High face of adjacent cell.
              elseif (IFACE.eq.6) then
                AH(II,JJ,KK)=0.
              endif

 30         continue
 20       continue
 10     continue
 40   continue

      RETURN
      END





