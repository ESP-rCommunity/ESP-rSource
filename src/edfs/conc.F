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
C Indoor Environment"  
C
C  CALCCONC - Calculates the equation for the mass concentration 
C             of a chemical, e.g. carbon dioxide. 
C  BNDARCON - Imposes BCs for the equation of concentration 
C  BLKBNDC  - Imposes BC's on blockages 
C  SCHMIDTT - Calculates turbulent schmidt number for a fluid

C ********************* CALCCONC *********************
C CALCCONC - Calculates the coefficients of the matrix of the 
C Concentration-equation (e.g. CO2)and calls the matrix-solver to obtain 
C its distribution. The mass concentration obtained is in [kg/kg].  
C The air is assumed to be a mixture of "normal air" and e.g. CO2. 
C Therefore the CO2 concentration calculated here presents 
C the "excess value" above outdoor concentration, i.e. we assume 
C zero-concentration of CO2 in the air-supply openings. 
C CO2 is a gas of a higher density than the air.  
C There are two basic ways of using CO2 concentration in practice: 
C 1. For indoor air quality as a contamination source from people;
c 2. During measurements of the local age of air in buildings. 
C The first utilization requires that a source term is specified 
C in the breathing area of the person and the second - a similar 
C concentration-source in the area of generating the CO2 gas. 
C Both sources are added in the right-hand-side of the equation - 
C i.e. in the source term of the appropriate control volume.  
C All equation terms have a dimension of [kg_CO2/s] after integration 
C within the CV. 
C According to ASHRAE 62-1989, Appendix D, for office activities 
C the CO2 generation is 0.30 [L/min], or 16.5*10-6 [kg_CO2/s]. 
C Any furter concentration equation could be implemented in a 
C similar way - the only difference is in the diffusion term, where 
C the multiplication sould be done by the appropriate Schmidt number. 
C Therefore we will make the implementation universal - the Schmidt 
C number is given in the input "dfd"-file. 
C 
      SUBROUTINE CALCCONC(ICTMNO) 
#include "cfd.h"
#include "net_flow.h"
#include "net_flow_data.h"

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

      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      character VOLNAME*12,VCsurf*12
      COMMON/CONST/GREAT,small,GRAV
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORMCW/RENORMC,RENORMWV 
      common/SOLVER/ILISOL
      COMMON/SCHMTT/SCHMT(MCTM),GFM(MCTM),VCRIT(MCTM),TBOIL(MCTM),
     &       TCRIT(MCTM),ISCHMT(MCTM)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      COMMON/CFDMFS2/MFNEWN(MNVLS,MNZ),NFNDN,NCFDOPNO(MNVLS,MNZ),
     &               NFNPDN(MNZ)
      COMMON/ACT/ACTCNN(MCNN)
      logical ACTCNN

C Source / Sink model commons
      COMMON/CTDFAF/ICTDFAF,SRCE(MNVLS,MCTM,MNZ),ICCSRC
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,SSLINK1(MSPMNO,MCONTM)
      COMMON/CONTM9/SSSTR(MSPMNO,MCONTM)
      COMMON/CONTM2/CNCAJAM(MPRODI),ZMCPAM(MPRODI)

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
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      common/ocflux/COflux,H2Oexp,H2Oswt

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      INTEGER SPMTYP,SSLINK1,SSLINK2
      LOGICAL OK,LINVOL,CALLMA,CALPOL
      CHARACTER*12 POLNAM,SRCE,SSNAME
      REAL POL3D(ntcelx,ntcely,ntcelz),SRCSTR(MNVLS,MNZ)
      logical unixok,NOTFOUND

C Set itrc to zero for no debugging and 1 to turn on trace.
      itrc=0

      ICTM=ICTMNO
      DO 785 IV=1,NVOL(ICFD)
        DO 787 II=1,ICFD
          SRCSTR(IV,II)=VOLPOL(ICTM,IV,II)
 787    CONTINUE
 785  CONTINUE

C <<what sould be done with resorc and renormc ?>> 
C Here the value of the Schmidt number (SCHMIL) for CO2 is coded. 
C <<1.2MET=30L/h CO2 production = 0.0000153737  kg/s ; rho=1.98 at 0C>> 
C <<the indexes of the cells with the people sould be specified in dfd>>
C PRODR=0.0000153737 - one breathing person during office work  
C V_CO2=17*M, where M is the metabolic rate and V_CO2 is in [L/h]. 

C If a contaminants flow network has been defined and linked with the
C CFD domain take source strength from the source model linked with this
C source type boundary condition and this contaminant
C Contaminant linking is independant of CFD to mass flow conflation but 
C if a CFD to mass flow conflation is defined (recommended) contaminant
C mass inflows and outflows from opening type boundary conditions are 
C also considered.
      IF(ICTDFAF.EQ.1)THEN

C Get source model and strength associated with this boundary condition
        DO 698 IV=1,NVOL(ICFD)
          IF(IVTYPE(IV,ICFD).EQ.20)THEN

C First get any explicitly defined source type boundary conditions
            DO 699 ISPMNO=1,NSPMNO
              IF(SRCE(IV,ICTM,ICFD)(1:12).EQ.
     &        SSNAME(SSLINK1(ISPMNO,ICTM))(1:12))THEN
                IF(SSLINK2(ISPMNO,ICFDNOD(ICFD)).NE.0)THEN
                  SRCSTR(IV,ICFD)=SSSTR(ISPMNO,ICTM)
                ENDIF
              ENDIF
 699        CONTINUE

C Now get any sourcing/sinking due to air flows from dfs-mfs conflation
C Flow rate is -ve for flow going out from CFD domain.
C TODO - Doing this with the volume name is very yuck, should find a
C        better way.
            IF(VOLNAME(IV,ICFD)(1:4).EQ.'SRC_')THEN
              DO 701 IX=1,NVOL(ICFD)
                IF(VOLNAME(IV,ICFD)(5:12).EQ.VOLNAME(IX,ICFD)(1:8))THEN

C First, look for an extra connection added for the CFD-MFS conflation
C process.
                  NOTFOUND=.TRUE.
                  do IOPNO=1,NFNDN
                    if (IX.eq.NCFDOPNO(IOPNO,ICFD)) then
                      INOD=MFNEWN(IOPNO,ICFD)
                      ICNN=1
                      DO WHILE(ICNN.LE.NCNN.AND.NOTFOUND)
                        IF(NODPS(ICNN).EQ.INOD.OR.NODNE(ICNN).EQ.INOD)
     &                    THEN
                          NOTFOUND=.FALSE.   
                        ELSE
                          ICNN=ICNN+1
                        ENDIF
                      END DO
                    endif
                  enddo

C If this is not found, use the original connection.
                  if (NOTFOUND) then
                    ICNN=ICFDCNN(IX,ICFD)
                  endif

                  FLRATE=REAL(FLW2(ICNN)+FLW1(ICNN))
                  IF(FLRATE.GT.0.0)THEN
                    INOD=NODNE(ICNN)
                  ELSE
                    INOD=NODPS(ICNN)
                  ENDIF
                  IROWNO=NCTM(ICFD)*(INOD-1)+ICTM
                  SRCSTR(IV,ICFD)=CNCAJAM(IROWNO)*FLRATE
                ENDIF
 701          CONTINUE
            ENDIF
          ENDIF
698     CONTINUE
      ENDIF

C Debug.
C      write(6,*)'srcstr ',srcstr

C Get turbulent Schmidt number for this contaminant
C SCHMIL=0.94 was previously used for CO2 and 0.59 for moisture also
C PRODR=0.0000153737 for CO2 and 0.0000166666 for moisture were used
      IF(ISCHMT(ICTM).EQ.1)THEN
        VL1=GFM(ICTM)
        VL2=VCRIT(ICTM)
        VL3=TBOIL(ICTM)
        VL4=TCRIT(ICTM)
        CALL SCHMIDTT(VL1,VL2,VL3,VL4,TURSCH)
      ELSEIF(ISCHMT(ICTM).EQ.0)THEN
        TURSCH=SCHMT(ICTM)
      ENDIF
      SCHMIL=TURSCH
      DO 50 I=2,NIM1
        DO 50 J=2,NJM1
          DO 50 K=2,NKM1

C First "GAMH" values are calculated and stored temporarily in AP
C Note that INCALK is a test for the presence of turbulence 
C <<should be completed to include the case of MIT-zero-equation model>> 
C <<using ITURB>0 assumes all turbulence is this correct?>>
        IF(ITURB(ICFD).gt.0) THEN 
          AP(I,J,K)=VISCOS/SCHMIL+(VIS(I,J,K)-VISCOS)/PRANDT
        ELSE 
          AP(I,J,K)=VISCOS/SCHMIL
        ENDIF 
C If there are CO2 sources from people in the room their source 
C term is calculated and added here. Implemented universally, i.e. 
C other contaminants are treated in a similar way using common fields. 
C PRODR is the production rate in [kg_constituent/s]. 
C Care sould be taken not to multiply later the so calculated here 
C SU_value by the volume of the cv 

C Clear SU term here
        SU(I,J,K)=0.0
 50   CONTINUE

C Should loop through conc sources here.
      do 60 IV=1,NVOL(ICFD)
        if (IVTYPE(IV,ICFD).eq.20) then
          do 70 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 71 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 72 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
               if (POLNAM(ICTM,ICFD) .eq. 'Sweat') then
!                call DynamicOcc(1,1.5,1)
                VOLPOL(ICTM,IV,ICFD)=H2Oswt+H2Oexp
                SRCSTR(IV,ICFD)=VOLPOL(ICTM,IV,ICFD)
!               if (ITRC.eq.1) then
!                print *, SRCSTR(IV,ICFD)
!               endif
               endif
                SU(I,J,K)=SRCSTR(IV,ICFD)
 72           continue
 71         continue
 70       continue
        endif
 60   continue

C Debug.
      call isunix(unixok)
      if(itrc.ge.1.and.unixok)then
        write(6,*)'srcstr ',(srcstr(iv,icfd),iv=13,nvol(icfd))
        write(6,*)'volpol ',(VOLPOL(ICTM,IV,ICFD),iv=13,nvol(icfd))
      endif

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then

C Loop through whole blockage, set concentration to zero 
        do 10 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          do 20 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 21 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 22 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)   
                POLCONCf(ICTM,I,J,K)=0.0
 22           continue
 21         continue
 20       continue
 10     continue
      endif
 
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
            SU(I,J,K)=SU(I,J,K)+AP0*POLCONCp(ICTM,I,J,K)+
     &               APfals*POLCONCf(ICTM,I,J,K)+CP*POLCONCf(ICTM,I,J,K)
            SP(I,J,K)=-AP0-APfals-CP 
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Apply boundary conditions.
C No difference between turbulent and laminar treatments, because 
C only zero-gradient type BC's  
      CALL BNDARCON(ICTMNO)  

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then

C Go and set the boundary conditions  
        do 2016 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          call BLKBNDC(IV)
 2016   continue
      endif 

C Remaining coefficients and residual source calculation.
      RESORC=0.0
      RENORMC=0.0
      test1=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &              AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            RESOR=AE(I,J,K)*POLCONCf(ICTM,i+1,j,K)+
     &            AW(I,J,K)*POLCONCf(ICTM,i-1,j,K)+
     &            AN(I,J,K)*POLCONCf(ICTM,i,j+1,K)+
     &            AS(I,J,K)*POLCONCf(ICTM,i,j-1,K)+
     &            AH(I,J,K)*POLCONCf(ICTM,i,j,K+1)+
     &            AL(I,J,K)*POLCONCf(ICTM,i,j,K-1)-
     &            AP(I,J,K)*POLCONCf(ICTM,i,j,K)+SU(I,J,K)
            RENORM=AP(I,J,K)*POLCONCf(ICTM,I,J,K)

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
 
            VOL=VolP(I,J,K)
            SORVOL=GREAT*VOL
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=RENORM/SORVOL
            ENDIF
            test1=test1+SU(I,J,K)
            RESORC=RESORC+ABS(RESOR)
            RENORMC=RENORMC+ABS(RENORM)

C Under-relaxation. 
            AP(I,J,K)=AP(I,J,K)/URFC(ICTM) 
            SU(I,J,K)=SU(I,J,K)+(1.0-URFC(ICTM))*AP(I,J,K)*
     &                POLCONCf(ICTM,I,J,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Keep for testing:        write(97,*)'all internal sources=',test1 

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

C Set the temperature inside the blockage equal to the initial one. 
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

C Set up pollutant concentration arrays in a format solvers can use
      DO 131 I=1,NIM1
        DO 132 J=1,NJM1
          DO 133 K=1,NKM1
            POL3D(I,J,K)=POLCONCf(ICTM,I,J,K)

C Trace...
             if(itrc.ge.1)write(96,*)'polconcf 99',polconcf(ictm,i,j,k)

 133      CONTINUE
 132    CONTINUE
 131  CONTINUE

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,2,NI,NJ,NK,POL3D,NSWPT)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,POL3D,NSWPT)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,POL3D,NSWPT)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,POL3D,NSWPT)
      endif

C Store concentrations back in common block variables
      DO 1331 I=1,NIM1
        DO 1332 J=1,NJM1
          DO 1333 K=1,NKM1
            POLCONCf(ICTM,I,J,K)=POL3D(I,J,K)
1333      CONTINUE
1332    CONTINUE
1331  CONTINUE

      RETURN
      END

C ********************* BNDARCON *********************
C BNDARCON - Establishes the boundary conditions for the 
C concentration equation. There is no difference between 
C laminar or turbulent cases - at solid walls the BC's are 
C always of the zero-gradient type. 

      SUBROUTINE BNDARCON(ICTMNO)                                                      
#include "cfd.h"

      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLN(MCFND,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
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
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD,IMITZ
      LOGICAL IZEROT,IZanKE,CALLMA,CALPOL
      logical unixok
      
      CHARACTER POLNAM*12
      ICTM=ICTMNO

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
C To keep the correct balance of the concentration equation in the room 
C so that in the outlet its average value corresponds to what is produced 
C in the room, the diffusion in the inlet openings is set to zero; 
C UPWIND scheme is applied there. The concentration is also set to zero 
C and the corresponding coefficients A_nb are recalculated 
C <<we should make the software engineering for obstacles and 
C <<small supply orifices (real area, real velocity or flowrate, 
C <<the location of the cv - which wall and indexes>> 
              IF(IWOPEN(L,ICFD).EQ.1)THEN

C Opening in west.
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN

C first test the temperature and prescribe a relative humidity of 50% 
                  TSAT=Tf(1,J,K)

C The following equation is taken from Recknagel 97/98, p. 113
                  PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &            *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
                  POLCONCf(ICTM,I-1,J,K)=0.5*PSAT*18.02/
     &            (0.5*PSAT*18.02+100000.0*28.96) 
                ELSE
                  POLCONCf(ICTM,I-1,J,K)=0.0 
                ENDIF
                GW=(SIFE(1)*DENf(1,J,K)+SIFW(2)*DENf(2,J,K))*Uf(2,J,K)
                CW=GW*AreaEWP(J,K)
                AW(2,J,K)=AMAX1(CW,0.0)
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN
                  SU(2,J,K)=SU(2,J,K)+AW(2,J,K)*POLCONCf(ICTM,I-1,J,K)
                  SP(2,J,K)=SP(2,J,K)-AW(2,J,K)
                  AW(2,J,K)=0.0
                ENDIF
              ELSEIF(IWOPEN(L,ICFD).EQ.2)THEN

C Opening in east.
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN

C first test the temperature and prescribe a relative humidity of 50% 
                  TSAT=Tf(I+1,J,K)

C The following equation is taken from Recknagel 97/98, p. 113
                  PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &            *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
                  POLCONCf(ICTM,I+1,J,K)=0.5*PSAT*18.02/
     &            (0.5*PSAT*18.02+100000.0*28.96) 
                ELSE
                  POLCONCf(ICTM,I+1,J,K)=0.0 
                ENDIF
                GE=(SIFE(NIM1)*DENf(NIM1,J,K)+
     &              SIFW(NIM1+1)*DENf(NIM1+1,J,K))*Uf(NIM1+1,J,K)
                CE=GE*AreaEWP(J,K)
                AE(NIM1,J,K)=AMAX1(-CE,0.0)
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN
                  SU(NIM1,J,K)=SU(NIM1,J,K)+AE(NIM1,J,K)*
     &                         POLCONCf(ICTM,I+1,J,K)
                  SP(NIM1,J,K)=SP(NIM1,J,K)-AE(NIM1,J,K)
                  AE(NIM1,J,K)=0.0
                ENDIF
              ELSEIF(IWOPEN(L,ICFD).EQ.3)THEN

C Opening in south.
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN

C first test the temperature and prescribe a relative humidity of 50% 
                  TSAT=Tf(I,1,K)

C The following equation is taken from Recknagel 97/98, p. 113
                  PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &            *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
                  POLCONCf(ICTM,I,J-1,K)=0.5*PSAT*18.02/
     &            (0.5*PSAT*18.02+100000.0*28.96) 
                ELSE
                  POLCONCf(ICTM,I,J-1,K)=0.0 
                ENDIF
                GS=(SIFN(1)*DENf(I,1,K)+SIFS(2)*DENf(I,2,K))*Vf(I,2,K)
                CS=GS*AreaNSP(I,K)
                AS(I,2,K)=AMAX1(CS,0.0)
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN
                  SU(I,2,K)=SU(I,2,K)+AS(I,2,K)*POLCONCf(ICTM,I,J-1,K)
                  SP(I,2,K)=SP(I,2,K)-AS(I,2,K)
                  AS(I,2,K)=0.0
                ENDIF
              ELSEIF(IWOPEN(L,ICFD).EQ.4)THEN

C Opening in north.
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN

C first test the temperature and prescribe a relative humidity of 50% 
                  TSAT=Tf(I,J+1,K)

C The following equation is taken from Recknagel 97/98, p. 113
                  PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &            *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
                  POLCONCf(ICTM,I,J+1,K)=0.5*PSAT*18.02/
     &            (0.5*PSAT*18.02+100000.0*28.96) 
                ELSE
                  POLCONCf(ICTM,I,J+1,K)=0.0 
                ENDIF
                GN=(SIFN(NJM1)*DENf(I,NJM1,K)+
     &          SIFS(NJM1+1)*DENf(I,NJM1+1,K))*Vf(I,NJM1+1,K)
                CN=GN*AreaNSP(I,K)
                AN(I,NJM1,K)=AMAX1(-CN,0.0)
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN
                  SU(I,NJM1,K)=SU(I,NJM1,K)+AN(I,NJM1,K)
     &                         *POLCONCf(ICTM,I,J+1,K)
                  SP(I,NJM1,K)=SP(I,NJM1,K)-AN(I,NJM1,K)
                  AN(I,NJM1,K)=0.0
                ENDIF
              ELSEIF(IWOPEN(L,ICFD).EQ.5)THEN

C Opening in low.
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN

C first test the temperature and prescribe a relative humidity of 50% 
                  TSAT=Tf(I,J,1)

C The following equation is taken from Recknagel 97/98, p. 113
                  PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &            *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
                  POLCONCf(ICTM,I,J,K-1)=0.5*PSAT*18.02/
     &            (0.5*PSAT*18.02+100000.0*28.96) 
                ELSE
                  POLCONCf(ICTM,I,J,K-1)=0.0 
                ENDIF
                GL=(SIFH(1)*DENf(I,J,1)+SIFL(2)*DENf(I,J,2))*Wf(I,J,2)
                CL=GL*AreaHLP(I,J)
                AL(I,J,2)=AMAX1(CL,0.0)
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN
                  SU(I,J,2)=SU(I,J,2)+AL(I,J,2)*POLCONCf(ICTM,I,J,K-1)
                  SP(I,J,2)=SP(I,J,2)-AL(I,J,2)
                  AL(I,J,2)=0.0
                ENDIF
              ELSEIF(IWOPEN(L,ICFD).EQ.6)THEN

C Opening in high.
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN

C first test the temperature and prescribe a relative humidity of 50% 
                  TSAT=Tf(I,J,K+1)

C The following equation is taken from Recknagel 97/98, p. 113
                  PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &            *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg]   
C <<In the following 50% relative humidity is hardcoded in every inlet
C opening. To change this we need to read the value from the dfd-file>>      
                  POLCONCf(ICTM,I,J,K+1)=0.5*PSAT*18.02/
     &            (0.5*PSAT*18.02+100000.0*28.96) 
                ELSE
                  POLCONCf(ICTM,I,J,K+1)=0.0 
                ENDIF
                GH=(SIFH(NKM1)*DENf(I,J,NKM1)+
     &          SIFL(NKM1+1)*DENf(I,J,NKM1+1))*Wf(I,J,NKM1+1)
                CH=GH*AreaHLP(I,J)
                AH(I,J,NKM1)=AMAX1(-CH,0.0)
                IF(ICTM.EQ.JHUMINDX(ICFD))THEN
                  SU(I,J,NKM1)=SU(I,J,NKM1)+AH(I,J,NKM1)
     &                         *POLCONCf(ICTM,I,J,K+1)
                  SP(I,J,NKM1)=SP(I,J,NKM1)-AH(I,J,NKM1)
                  AH(I,J,NKM1)=0.0
                ENDIF
              ENDIF
 1012       CONTINUE
 1011     CONTINUE
 101    CONTINUE
       ENDIF 
C Examine next opening.
 300  CONTINUE

C Outlets 
C For all type of outlets - `zero-gradient type' boundary conditions. >>
C <<The average concentration in outlets is also calculated (summed in AVECON) >>
C <<Summation of the massflow is made in SUMFLU >>
C <<Trace is hardwired to off - switch on by changing ITRC=1 >>
      ITRC=0
      AVECON=0.0 
      SUMFLU=0.0 
      DO 415 M=1,NOPEN(ICFD)
        IF(FIXM(M,ICFD).LT.0.) THEN

C West wall.
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN
            I=IOPENi(M,ICFD)
            DO 900 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9001 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
                POLCONCf(ICTM,I-1,J,K)=POLCONCf(ICTM,I,J,K)
                AREAEW=AreaEWP(J,K)
                if (ITRC.eq.1) then
                  AVECON=AVECON+
     &              DENf(I,J,K)*AREAEW*Uf(I,J,K)*POLCONCf(ICTM,I-1,J,K) 
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
                POLCONCf(ICTM,I+1,J,K)=POLCONCf(ICTM,I,J,K)
                AREAEW=AreaEWP(J,K)
                if (ITRC.eq.1) then
                  AVECON=AVECON+
     &             DENf(I,J,K)*AREAEW*Uf(I+1,J,K)*POLCONCf(ICTM,I+1,J,K) 
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
                POLCONCf(ICTM,I,J-1,K)=POLCONCf(ICTM,I,J,K)
                AREANS=AreaNSP(I,K)
                if (ITRC.eq.1) then
                  AVECON=AVECON+
     &              DENf(I,J,K)*AREANS*Vf(I,J,K)*POLCONCf(ICTM,I,J-1,K)
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
                POLCONCf(ICTM,I,J+1,K)=POLCONCf(ICTM,I,J,K)
                AREANS=AreaNSP(I,K)
                if (ITRC.eq.1) then
                  AVECON=AVECON+
     &             DENf(I,J,K)*AREANS*Vf(I,J+1,K)*POLCONCf(ICTM,I,J+1,K)
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
                POLCONCf(ICTM,I,J,K-1)=POLCONCf(ICTM,I,J,K)
                AREAHL=AreaHLP(I,J)
                if (ITRC.eq.1) then
                  AVECON=AVECON+
     &              DENf(I,J,K)*AREAHL*Wf(I,J,K)*POLCONCf(ICTM,I,J,K-1)
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
                POLCONCf(ICTM,I,J,K+1)=POLCONCf(ICTM,I,J,K)
                AREAHL=AreaHLP(I,J)
                if (ITRC.eq.1) then
                  AVECON=AVECON+
     &             DENf(I,J,K)*AREAHL*Wf(I,J,K+1)*POLCONCf(ICTM,I,J,K+1)
                  SUMFLU=SUMFLU+DENf(I,J,K)*AREAHL*Wf(I,J,K+1)
                endif
 9101         CONTINUE
 910        CONTINUE
          ENDIF
        ENDIF
 415  CONTINUE

C Here the average value of concentration in the outlets is calculated. 
C << Need to make an output of the average outlet-concentration >>
C << on screen and make it available during testing (ltest-variable) >>

C Debug.
      call isunix(unixok)
      if (ITRC.eq.1.and.unixok) then
        write(6,*)'Average concentration =',AVECON/anotzero(SUMFLU)
      endif
C 
C Apply zero-gradient-type BC's on each solid boundary in turn.
C 
C <<Also a boundary condition with a mass flux proportional to the 
C concentration is programmed here, waiting for appropriate interface.  
C At present the coeficients A&BCOEF1 are set to zero>>. Needed for dust 
C concentration and droplets. ACOEF1 is for horizontal walls and BCOEF1 
C is the transfer coefficient (analogical to hc) for vertical walls. 
C
      ACOEF1=-0.0
      BCOEF1=-0.0
C 
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
              POLCONCf(ICTM,I-1,J,K)=POLCONCf(ICTM,I,J,K)
C Sink of concentration - needed for modelling droplets or dust 
C The sink is distributed either to SU or SP to avoide 
C negative concentrations during the iterative process 
        IF(ICTM.NE.JHUMINDX(ICFD))THEN
        HSINK1=BCOEF1*POLCONCf(ICTM,I,J,K)*AreaEWP(J,K)
        SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,HSINK1)
        SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,HSINK1)/
     &            anotzero(POLCONCf(ICTM,I,J,K))
        ENDIF
 4301       CONTINUE
 430      CONTINUE

C East wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DO 435 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4351 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0 
              POLCONCf(ICTM,I+1,J,K)=POLCONCf(ICTM,I,J,K)
C see comments west wall
        IF(ICTM.NE.JHUMINDX(ICFD))THEN
        HSINK1=BCOEF1*POLCONCf(ICTM,I,J,K)*AreaEWP(J,K)
        SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,HSINK1)
        SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,HSINK1)/
     &            anotzero(POLCONCf(ICTM,I,J,K))
        ENDIF
 4351       CONTINUE
 435      CONTINUE

C South wall.
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DO 440 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0 
              POLCONCf(ICTM,I,J-1,K)=POLCONCf(ICTM,I,J,K)
C see comments west wall
        IF(ICTM.NE.JHUMINDX(ICFD))THEN
        HSINK1=BCOEF1*POLCONCf(ICTM,I,J,K)*AreaNSP(I,K)
        SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,HSINK1)
        SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,HSINK1)/
     &            anotzero(POLCONCf(ICTM,I,J,K))
        ENDIF
 4401       CONTINUE
 440      CONTINUE

C North wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DO 445 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4451 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0 
              POLCONCf(ICTM,I,J+1,K)=POLCONCf(ICTM,I,J,K)
C see comments west wall
        IF(ICTM.NE.JHUMINDX(ICFD))THEN
        HSINK1=BCOEF1*POLCONCf(ICTM,I,J,K)*AreaNSP(I,K)
        SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,HSINK1)
        SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,HSINK1)/
     &            anotzero(POLCONCf(ICTM,I,J,K))
        ENDIF
 4451       CONTINUE
 445      CONTINUE

C Low wall.
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DO 450 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4501 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0 
              POLCONCf(ICTM,I,J,K-1)=POLCONCf(ICTM,I,J,K)
C see comments west wall 
        IF(ICTM.NE.JHUMINDX(ICFD))THEN
        HSINK1=ACOEF1*POLCONCf(ICTM,I,J,K)*AreaHLP(I,J)
        SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,HSINK1)
        SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,HSINK1)/
     &            anotzero(POLCONCf(ICTM,I,J,K))
        ENDIF
 4501       CONTINUE
 450      CONTINUE

C High wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DO 455 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4551 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0 
              POLCONCf(ICTM,I,J,K+1)=POLCONCf(ICTM,I,J,K)
C see comments west wall 
        IF(ICTM.NE.JHUMINDX(ICFD))THEN
        HSINK1=ACOEF1*POLCONCf(ICTM,I,J,K)*AreaHLP(I,J)
        SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,HSINK1)
        SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,HSINK1)/
     &            anotzero(POLCONCf(ICTM,I,J,K))
        ENDIF
 4551       CONTINUE
 455      CONTINUE

        ENDIF
 425     CONTINUE

      RETURN
      END

C ********************* BLKBNDC *********************
C BLKBNDC - set boundary conditions for the Contaminant and 
C humidity due to solid face for blockages. 
C Zero-gradient type of BC's are applied 
C
      SUBROUTINE BLKBNDC(IBLK)
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

C ********************* SCHMIDTT *********************
C Calculates turbulent Schmidt Number for contaminant gases
C following procedure given in PhD Thesis by Aizaz Samuel

      SUBROUTINE SCHMIDTT(GFM,VCRIT,TBOIL,TCRIT,TRSHMT)
#include "building.h"
#include "net_flow.h"
#include "cfd.h"
      COMMON/ICFNOD/ICFD,ICP
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

C First get temperature in CFD domain
      TEMP_K=0.5*(TFA(ICP)+TPA(ICP))+273.

C Now get pressure in CFD domain first 
      PRESS_BAR=1.01325+REAL(PRES(ICFDNOD(ICFD)))/100000.0

C Now calculate viscosity VAL_MU and diffusivity D_AB
      SIGMA=0.809*VCRIT**(1./3.)
      EPSILON=TCRIT/1.2593
      TSTAR=TEMP_K/EPSILON
      OMEGA_MU=1.16145*TSTAR**(-0.14874)+0.52487*EXP(-0.7732*TSTAR)+
     &         2.16178*EXP(-2.43787*TSTAR)
      VAL_MU=0.00002669*(GFM*TEMP_K)**0.5/(OMEGA_MU*SIGMA**2.)
      VAL_M_AB=2./(1./GFM+1./14.)
      SIGMA_A=0.777*VCRIT**0.349
      SIGMA_AB=0.5*(SIGMA_A+3.62)
      EPSILON_A=1.15*TBOIL
      EPSILON_AB=(97.*EPSILON_A)**0.5
      TSTAR=TEMP_K/EPSILON_AB
      OMEGA_D=1.06036*TSTAR**(-0.1561)+0.193*EXP(-0.47635*TSTAR)+
     &        1.03587*EXP(-1.52996*TSTAR)+1.76474*EXP(-3.89411*TSTAR)
      D_AB=(3.03-(0.98/VAL_M_AB**0.5))*TEMP_K**(3./2.)/
     &      (PRESS_BAR*1000.*VAL_M_AB**0.5*SIGMA_AB**2.*OMEGA_D)
      RHO=PRESS_BAR*GFM*100./(8.314*TEMP_K)

C Calculate turbulent Schmidt number = two times Schmidt number
      TRSHMT=2.*(VAL_MU/10.)/(D_AB*RHO*10000.)
      RETURN
      END
