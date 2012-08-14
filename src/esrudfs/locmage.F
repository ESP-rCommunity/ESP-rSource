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
C Indoor Environment. 
C Contents:
C  CALCLMA  - Calculates the equation for the "Local Mean Age of Air"
C             variable. 
C  BNDARLMA - Imposes BCs for the "Local Mean Age of Air" variable  

C ********************* CALCLMA *********************
C CALCLMA - Calculates the coefficients of the matrix of the 
C "passive scalar" equation and solves the matrix to obtain the 
C "Local Mean Age of Air" variable. The variable has dimension 
C of time [s], but here it is made dimensionless by the average 
C time the air stays in the room (room_volume/volumetric_flowrate).
C Therefore the "Local Mean Age of Air" is equal to 1.0 in the 
C outlet openings of the room. Theoretically the variable uses 
C the mathematical model of a stationary equation for concentration. 
C However, for simplicity, instead of a concentration equation, we 
C use in the following analogy to the energy equation. As the "Local Mean 
C Age of Air" is a "passive scalar" quantity, it does not affect 
C the velocity field and should be calculated after convergence 
C for all other variables is achieved. 
      SUBROUTINE CALCLMA(ITIMST)
#include "cfd.h"

      common/outin/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB

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
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/CONST/GREAT,small,GRAV
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     1             IPPHI(MNZ),SRMAX(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME 
      COMMON/ITERAT/NITER
      common/SOLVER/ILISOL
C Parameter "Nostop" gt. 0 overrides "non-convergence" control. Use with care.
      COMMON/MFS/IMFACT,NOSTOP,MITMFS

C Boundary condition commons.  
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
C Common blocks for blockages and small supply openings
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ)

      character LRES(4)*24,LMON(4)*24

      DIMENSION CRES(4),CMON(4)

      LOGICAL OK,DOK,LINVOL

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CALCLMA'  ! set for subroutine

      call edisp(iuout,
     &  'Initiating calculation of local mean age of air')
     
C If there is no mass flow coming in the room (e.g. only natural 
C convection), there is no reason to calculate LMA: 
      IF(FLWIN.lt.0.0000001) THEN 
         call edisp(iuout,'There are no flows into the domain!')
         call edisp(iuout,'Cannot proceed with calculation.')
C <<Initialize LMA - subroutine INIT in cfcalc.F, add common there>> 
C <<change dfd-file and start this subroutine after finising calculations>> 
        RETURN
      ENDIF

C It is usefull to have a stronger criterion for LMA: 
      SORMAX=SRMAX(ICFD)/10.0

C Write column headings (if not in graphics mode.)
      if (MMOD.NE.8) write (IUOUT,410) 
      write (ICFMON,410) 
 410  FORMAT(/,' DFS - Monitoring of residuals & variables (LMA)',/,/,
     &'   ITER',2X,'1-aver/aver',2X,'resid/LMA_mon',/)

C VOLTOT is the total volume of the fluid in the room. Needed for the 
C source_term. Calculated before the main loop with regard to blockages.   
      VOLTOT=0.0 
      DO 50 I=2,NIM1
        DO 51 J=2,NJM1
          DO 52 K=2,NKM1
            VOL=VolP(I,J,K)
            VOLTOT=VOLTOT+VOL 
 52       CONTINUE
 51     CONTINUE
 50   CONTINUE

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then
C Loop through whole blockage, decrease voltot by its volume.  
        do 10 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          do 20 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 21 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 22 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)   
                VOL=VolP(I,J,K)                
                VOLTOT=VOLTOT-VOL 
 22           continue
 21         continue
 20       continue
 10     continue
      endif
 
C LOOP HERE 
C Using the same number of iterations as main solver 
      MAXIT=MAXITR(ICFD)
 999  IPLOT=0

      do 9999 NITER=1,MAXIT 
      NPLT=MAXIT/50
      IF (mod(MAXIT,50).NE.0) NPLT=NPLT+1

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

C Calculate diffusion coefficients.
C GAMH are always calculated in PROPS (even if isothermal flow!). The laminar 
C or the corresponding turbulent values are already accounted for in GAMH.
            GAMAE=SIFE(I)*GAMH(I,J,K)+SIFW(I+1)*GAMH(I+1,J,K)
            GAMAW=SIFE(I-1)*GAMH(I-1,J,K)+SIFW(I)*GAMH(I,J,K)
            GAMAN=SIFN(J)*GAMH(I,J,K)+SIFS(J+1)*GAMH(I,J+1,K)
            GAMAS=SIFN(J-1)*GAMH(I,J-1,K)+SIFS(J)*GAMH(I,J,K)
            GAMAH=SIFH(K)*GAMH(I,J,K)+SIFL(K+1)*GAMH(I,J,K+1)
            GAMAL=SIFH(K-1)*GAMH(I,J,K-1)+SIFL(K)*GAMH(I,J,K)

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
C    1. The dimension of the terms of the energy equation is: 
C       (e.g. convection) den*vel*area - [kg/s]. After that each convective 
C       or diffusion term is multiplied by T, so all terms Anb*Tnb and 
C       the source term Su have a dimension of[kg.K/s]. According to 
C       the theory the magnitude of the source term should be 1. This 
C       would result in a temperature difference in the outlet opening 
C       which is as many Kelvins as many seconds the LMA there is. 
C    2. However, we wish additionally that the temperature 
C       in the outlet is equal to 1, which would correspond to a 
C       dimensionless value of LMA (local mean age) of 1 in this opening. 
C       Therefore the value of the source_term will differ from 1. 
C    3. The heat balance for the whole room is: 
C       mass_flow_rate*(cp_out*temperature_out-cp_in*temperature_in)=
C       vol_heat_source*volume_fluid_room 
C       It follows from the theory that temperature_in (LMA_in) is 0 
C       and we wish that the temperature_out (LMA_out) is equal to 1. 
C       Therefore we obtain (LHS and RHS in [W/m3]): 
C       vol_heat_source= mass_flow_rate*cp_out/volume_fluid_room  (*1K!)  
C    4. In the ESP-r cp (Specific Heat, SH) is equal to 1006.0 [J/{kg.K)]. 
C    5. Source_term_per_unit_volume=vol_heat_source/cp in [kg.K/(s.m3)] or: 
C       Source_term_per_unit_volume=mass_flow_rate/volume_fluid_room. 
C    6. Source_term=Source_term_per_unit_volume*volume_CV or: 
C       Source_term=(mass_flow_rate/volume_fluid_room)*volume_CV. 
C    7. mass_flow_rate is stored in the variable FLWIN. 
            SU(I,J,K)=ABS(FLWIN)/VOLTOT*VOL  
            SP(I,J,K)=0.0 
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Apply boundary conditions.
C No difference between turbulent and laminar treatments, because 
C only zero-gradient type BC's. 
      CALL BNDARLMA(AVELMA,SUMFLU)

C Check for blockages. 
      if(NBLK(ICFD).ne.0) then
C Go and set the boundary conditions  
        do 2016 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          call BLKBNDL(IV)
 2016   continue
      endif 

C Remaining coefficients and residual source calculation.
      RESORL=0.0
      RENORML=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &              AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            RESOR=AE(I,J,K)*AGEf(i+1,j,K)+AW(I,J,K)*AGEf(i-1,j,K)+
     &            AN(I,J,K)*AGEf(i,j+1,K)+AS(I,J,K)*AGEf(i,j-1,K)+
     &            AH(I,J,K)*AGEf(i,j,K+1)+AL(I,J,K)*AGEf(i,j,K-1)-
     &            AP(I,J,K)*AGEf(i,j,K)+SU(I,J,K)
            RENORM=AP(I,J,K)*AGEf(I,J,K)

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
            RESORL=RESORL+ABS(RESOR)
            RENORML=RENORML+ABS(RENORM)

C Under-relaxation. Using always a constant value of 0.90 is fast enough 
C and guarantees always a good convergence. Therefore local variable.  
            URFLMA=0.90 
            AP(I,J,K)=AP(I,J,K)/URFLMA
            SU(I,J,K)=SU(I,J,K)+(1.0-URFLMA)*AP(I,J,K)*AGEf(i,j,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

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

C Set the LMA inside the blockage equal to zero. 
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
        CALL LISOLV1(2,2,2,NI,NJ,NK,AGEf,NSWPT)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,AGEf,NSWPT)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,AGEf,NSWPT)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,AGEf,NSWPT)
      endif

C End of iteration loop.
C Plot values on screen.
      if (mod(NITER,NPLT).eq.0) then

C Write residuals and monitoring values in a ASCII format file.
        WRITE(ICFMON,312)NITER,1.-AVELMA,RESORL,AVELMA,
     &                  AGEf(IMONT(ICFD),JMONT(ICFD),KMONT(ICFD))
 312    FORMAT(I7,2X,2E11.3,/,9X,2E11.3)

      IF(MMOD.eq.8)THEN
          NCRV=2
          CRES(1)=abs(1.-AVELMA)
          CRES(2)=RESORL
          CMON(1)=AVELMA
          CMON(2)=AGEf(IMONT(ICFD),JMONT(ICFD),KMONT(ICFD))
          LRES(1)='Outlet difference'
          LRES(2)='Overall residual'
          LMON(1)='Average LMA at outlets'
          LMON(2)='Local mean age'
          CALL PLOT(IPLOT,NPLT,ITIMST,NITER,SORMAX,NCRV,CRES,CMON,
     &                  LRES,LMON)

        else

C Not in graphics mode => list data.
          WRITE(IUOUT,312)NITER,1.-AVELMA,RESORL,AVELMA,
     &                  AGEf(IMONT(ICFD),JMONT(ICFD),KMONT(ICFD))
        endif
      endif
C Check for convergence, making sure that we have done at least 5 iterations.

C Debug.
C        write(6,*) 'Residual and max resid ',RESORL,SRMAX(ICFD)

C      if (RESORL.lt.SRMAX(ICFD).and.NITER.gt.5) goto 99
      if (RESORL.lt.SRMAX(ICFD) .and. NOSTOP .ne.0 
     &     .and. NITER.gt.5) goto 99
 9999 continue

C Check max iters.
      dok=.true.
      helptopic='dfs_convergence_accept'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK('Maximum iterations reached,','accept solution?',
     &   OK,dok,nbhelp)
      if (.not.OK) goto 999

  99  RETURN
      END

C ********************* BNDARLMA *********************
C BNDARLMA - Establishes the boundary conditions for the "Local Mean 
C Age of Air" variable. There is no difference between laminar or turbulent 
C cases - at solid walls the BC's are always of the zero-gradient type. 

      SUBROUTINE BNDARLMA(AVELMA,SUMFLU)
#include "cfd.h"

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
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/CONST/GREAT,small,GRAV
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/MFS/IMFACT,NOSTOP,MITMFS
      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)

      CHARACTER HSID*72

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD,BHS,IMITZ
      LOGICAL IZEROT,IZanKE

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
C To keep the correct balance of the "Local Mean Age of Air" in the room
C so that in the outlet its average value is 1.0, the diffusion in the 
C inlet openings should be set to zero; UPWIND scheme is applied. The LMA  
C is set to zero and the corresponding coefficients A_nb are recalculated 
              IF(IWOPEN(L,ICFD).EQ.1)THEN

C Opening in west.
                AGEf(I-1,J,K)=0.0 
                GW=(SIFE(1)*DENf(1,J,K)+SIFW(2)*DENf(2,J,K))*Uf(2,J,K)
                CW=GW*AreaEWP(J,K)
                AW(2,J,K)=AMAX1(CW,0.0)
              ELSEIF(IWOPEN(L,ICFD).EQ.2)THEN

C Opening in east.
                AGEf(I+1,J,K)=0.0 
                GE=(SIFE(NIM1)*DENf(NIM1,J,K)+
     &          SIFW(NIM1+1)*DENf(NIM1+1,J,K))*Uf(NIM1+1,J,K)
                CE=GE*AreaEWP(J,K)
                AE(NIM1,J,K)=AMAX1(-CE,0.0)
              ELSEIF(IWOPEN(L,ICFD).EQ.3)THEN

C Opening in south.
                AGEf(I,J-1,K)=0.0 
                GS=(SIFN(1)*DENf(I,1,K)+SIFS(2)*DENf(I,2,K))*Vf(I,2,K)
                CS=GS*AreaNSP(I,K)
                AS(I,2,K)=AMAX1(CS,0.0)
              ELSEIF(IWOPEN(L,ICFD).EQ.4)THEN

C Opening in north.
                AGEf(I,J+1,K)=0.0 
                GN=(SIFN(NJM1)*DENf(I,NJM1,K)+
     &          SIFS(NJM1+1)*DENf(I,NJM1+1,K))*Vf(I,NJM1+1,K)
                CN=GN*AreaNSP(I,K)
                AN(I,NJM1,K)=AMAX1(-CN,0.0)
              ELSEIF(IWOPEN(L,ICFD).EQ.5)THEN

C Opening in low.
                AGEf(I,J,K-1)=0.0 
                GL=(SIFH(1)*DENf(I,J,1)+SIFL(2)*DENf(I,J,2))*Wf(I,J,2)
                CL=GL*AreaHLP(I,J)
                AL(I,J,2)=AMAX1(CL,0.0)
              ELSEIF(IWOPEN(L,ICFD).EQ.6)THEN

C Opening in high.
                AGEf(I,J,K+1)=0.0 
                GH=(SIFH(NKM1)*DENf(I,J,NKM1)+
     &          SIFL(NKM1+1)*DENf(I,J,NKM1+1))*Wf(I,J,NKM1+1)
                CH=GH*AreaHLP(I,J)
                AH(I,J,NKM1)=AMAX1(-CH,0.0)
              ENDIF
 1012       CONTINUE
 1011     CONTINUE
 101    CONTINUE
       ENDIF 

C Examine next opening.
 300  CONTINUE

C Outlets 
C For all type of outlets - `zero-gradient type' for the LMA
C The average LMA in outlets is also calculated (summed in AVELMA)
C Summation of the massflow needed for averaging is made in SUMFLU   
      AVELMA=0.0
      SUMFLU=0.0
      DO 415 M=1,NOPEN(ICFD)
        IF(FIXM(M,ICFD).LT.0.) THEN

C West wall.
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN
            I=IOPENi(M,ICFD)
            DO 900 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9001 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
                AGEf(I-1,J,K)=AGEf(I,J,K)
                AREAEW=AreaEWP(J,K)
                SUMFLU=SUMFLU+ABS(FIXM(M,ICFD))/AREAEW
                AVELMA=AVELMA+ABS(FIXM(M,ICFD))/AREAEW*AGEf(I-1,J,K)
 9001         CONTINUE
 900        CONTINUE

C East wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.2) THEN
            I=IOPENi(M,ICFD)
            DO 902 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9021 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AE(I,J,K)=0.0
                AGEf(I+1,J,K)=AGEf(I,J,K)
                AREAEW=AreaEWP(J,K)
                SUMFLU=SUMFLU+ABS(FIXM(M,ICFD))/AREAEW
                AVELMA=AVELMA+ABS(FIXM(M,ICFD))/AREAEW*AGEf(I+1,J,K)
 9021         CONTINUE
 902        CONTINUE

C South wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.3) THEN
            J=JOPENi(M,ICFD)
            DO 904 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9041 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AS(I,J,K)=0.0
                AGEf(I,J-1,K)=AGEf(I,J,K)
                AREANS=AreaNSP(I,K)
                SUMFLU=SUMFLU+ABS(FIXM(M,ICFD))/AREANS
                AVELMA=AVELMA+ABS(FIXM(M,ICFD))/AREANS*AGEf(I,J-1,K)
 9041         CONTINUE
 904        CONTINUE

C North wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.4) THEN
            J=JOPENi(M,ICFD)
            DO 906 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9061 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AN(I,J,K)=0.0
                AGEf(I,J+1,K)=AGEf(I,J,K)
                AREANS=AreaNSP(I,K)
                SUMFLU=SUMFLU+ABS(FIXM(M,ICFD))/AREANS
                AVELMA=AVELMA+ABS(FIXM(M,ICFD))/AREANS*AGEf(I,J+1,K)
 9061         CONTINUE
 906        CONTINUE

C Low wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.5) THEN
            K=KOPENi(M,ICFD)
            DO 908 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9081 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AL(I,J,K)=0.0
                AGEf(I,J,K-1)=AGEf(I,J,K)
                AREAHL=AreaHLP(I,J)
                SUMFLU=SUMFLU+ABS(FIXM(M,ICFD))/AREAHL
                AVELMA=AVELMA+ABS(FIXM(M,ICFD))/AREAHL*AGEf(I,J,K-1)
 9081         CONTINUE
 908        CONTINUE

C High wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.6) THEN
            K=KOPENi(M,ICFD)
            DO 910 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9101 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AH(I,J,K)=0.0
                AGEf(I,J,K+1)=AGEf(I,J,K)
                AREAHL=AreaHLP(I,J)
                SUMFLU=SUMFLU+ABS(FIXM(M,ICFD))/AREAHL
                AVELMA=AVELMA+ABS(FIXM(M,ICFD))/AREAHL*AGEf(I,J,K+1)
 9101         CONTINUE
 910        CONTINUE
          ENDIF
        ENDIF
 415  CONTINUE

C Here te average value of LMA in the outlets is calculated 

C Debug.
C      write(6,*)'AVELMA,sumflu=',AVELMA,SUMFLU

      AVELMA=AVELMA/(anotzero(SUMFLU))

C Debug.
C      write(6,*)'average LMA in outlets=',AVELMA

C <<Here we need to make an output of the average outlet-LMA on 
C screen - probably the best is to do it with the monitoring values>>

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
              AGEf(I-1,J,K)=AGEf(I,J,K)
 4301       CONTINUE
 430      CONTINUE

C East wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DO 435 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4351 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              AGEf(I+1,J,K)=AGEf(I,J,K)
 4351       CONTINUE
 435      CONTINUE

C South wall.
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DO 440 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              AGEf(I,J-1,K)=AGEf(I,J,K)
 4401       CONTINUE
 440      CONTINUE

C North wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DO 445 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4451 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              AGEf(I,J+1,K)=AGEf(I,J,K)
 4451       CONTINUE
 445      CONTINUE

C Low wall.
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DO 450 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4501 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              AGEf(I,J,K-1)=AGEf(I,J,K)
 4501       CONTINUE
 450      CONTINUE

C High wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DO 455 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4551 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              AGEf(I,J,K+1)=AGEf(I,J,K)
 4551       CONTINUE
 455      CONTINUE

        ENDIF
 425     CONTINUE

      RETURN
      END

C ********************* BLKBNDL *********************
C BLKBNDL - set boundary conditions for LMA on the solid 
C face for blockages. 
C Zero-gradient type of BC's are applied 
C
      SUBROUTINE BLKBNDL(IBLK)
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












