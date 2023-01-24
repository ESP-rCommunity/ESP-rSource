C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains subroutines comprising the 'engine'
C of the CFD algorithm. 
C  CFCALC    - CFD manager.
C  CALC?     - Establish the coefficients for the linear algebraic 
C              equation ? and solve it (where ? is U, V, W, p, T,
C              TE and ED).
C  DEFINESSO - Sets area ratios for small supply openings.  
C  INIT      - Initialise all flow variables.
C  UNIFORMT  - Set an uniform temperature everywhere if temperature not
C              being calculated.
C  PROPS     - Calculate flow dependent properties.
C  INDBND    - Initialise some boundary conditions.
C  INOUT     - Establish all inlet and outlet boundary conditions.
C  RECRES    - Modify mass flow residuals if pressure is fixed in 
C              certain cells.
C  EXCFDBC   - Export CFD boundary conditions to file.
C  RFNCFDGRD - Refine CFD grid in areas of poor convergence.


C ********************* CFCALC *********************
C CFD solution manager.

      SUBROUTINE CFCALC(ITIMST,TSTEP,IMFSAC,IBLACT,CONVER)
#include "building.h"
#include "cfd.h"
#include "help.h"

      common/outin/iuout,iuin,ieout
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      COMMON/BNDCND/FLWIN,XMONIN
      COMMON/FDTRFC/FLSDTU(MNZ),FLSDTV(MNZ),FLSDTW(MNZ),FLSDTP(MNZ),
     &              FLSDTT(MNZ),FLSDTK(MNZ),FLSDTE(MNZ)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/PRSREF/IPRESF(MNZ),JPRESF(MNZ),KPRESF(MNZ)
      COMMON/UVEL/RESORU,NSWPU,URFU,FSDTU,DXEPU(ntcelx),
     &            DXPWU(ntcelx),SEWU(ntcelx)
      COMMON/VVEL/RESORV,NSWPV,URFV,FSDTV,DYNPV(ntcely),
     &            DYPSV(ntcely),SNSV(ntcely)
      COMMON/WVEL/RESORW,NSWPW,URFW,FSDTW,DZHPW(ntcelz),
     &            DZPLW(ntcelz),SHLW(ntcelz)
      COMMON/PCOR/RESORM,NSWPP,URFP,FSDTP,IPREF,JPREF,KPREF
      COMMON/TEN/RESORK,NSWPK,URFK,FSDTK
      COMMON/TDIS/RESORE,NSWPD,URFE,FSDTE
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     &            Wf(ntcelx,ntcely,ntcelz),
     &            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     &            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     &            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     &            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     &             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/CONST/GREAT,small,GRAV
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)
      COMMON/TIMSTP/DT
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     &              IZEROT,IZanKE,IMITZ
      COMMON/INCALP/INCALPOL(MCTM)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      COMMON/MFS/IMFACT
      COMMON/ICFNOD/ICFD,ICP
      COMMON/LINRFC1/URFCD(MNZ)
      COMMON/URFDEN/URFDEN
      COMMON/NSSWP/NSSWPU(MNZ),NSSWPV(MNZ),NSSWPW(MNZ),NSSWPP(MNZ),
     &             NSSWPT(MNZ),NSSWPK(MNZ),NSSWPE(MNZ)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME
      COMMON/RENORM/LRENORM
      COMMON/ZTURB/rMOOT(MNZ),nZtoKE(MNZ)
      COMMON/ITERAT/NITER
      COMMON/YUANcm/Uqstor(ntcelx,ntcely,ntcelz),Uqinit 
      COMMON/ACCrec/IACC(MNZ)
      common/fvisgo/gophrun

      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)

C NBLK is the number of blockages in each domain.
C INBLK points to the volume that defines the blockage shape.
C NSSO is the number of small supply openings in each domain.
C INSSO points to the volume that defines the small supply shape.
C BLKTEMP is used to transfer surface temperatures from the building
C side to CFD.
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
     
C BCOFF is used to disable occupant-linked BCs if the occupant is not
C present.
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      common/SSOinit/areaSSO
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      common/btime/btimep,btimef
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
C      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      CHARACTER LRES(4)*24,LMON(4)*24,POLNAM*12
      character VOLNAME*12,VCsurf*12,BLKSURF*12,wkd1*10,wkd2*10,OUT*96

      DIMENSION CRES(4),CMON(4)

      LOGICAL LRENORM,OK,closer,MKURFL
      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,CONVER,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero,Uqinit
      LOGICAL CALPOL,CALLMA
      LOGICAL areaSSO,gophrun,INCALPOL
C      logical unixok

      real sec_pre,sec_now,sec_tkn
      integer NITER_pre
      character outs*124

      helpinsub='cfcalc'  ! set for subroutine

      NITER=0
      DT=TSTEP
      CONVER=.FALSE.
      IMFACT=IMFSAC
      NMFSRUN=0
      NSPITR=1
      MKURFL=.FALSE.
      NRFND=0
C      NRFN=(NI*NJ*NK)/1000
      NRFN=1  ! Number of cells to refine

C Set up small openings if not done.
      IF(.NOT.areaSSO) THEN
        CALL DEFINESSO
      ENDIF

C Set under relaxation factors using data in zone .dfd file. If
C buoyancy is active, use lower factors.
      if (BUOY(ICFD)) then
        URFU = URFCU2(ICFD)
        URFV = URFCV2(ICFD)
        URFW = URFCW2(ICFD)
        URFP = URFCP2(ICFD)
        URFT = URFCT2(ICFD)
        URFK = URFCK2(ICFD)
        URFE = URFCE2(ICFD)
        URFVIS = URFCVS2(ICFD)
      else
        URFU=URFCU(ICFD)
        URFV=URFCV(ICFD)
        URFW=URFCW(ICFD)
        URFP=URFCP(ICFD)
        URFT=URFCT(ICFD)
        URFK=URFCK(ICFD)
        URFE=URFCE(ICFD)
        URFVIS=URFCVS(ICFD)
      endif
      URFDEN=URFCD(ICFD)
      DO 123 ICTM=1,NCTM(ICFD)
        URFC(ICTM)=URFCC(ICFD,ICTM)
  123 CONTINUE

C Fictitious time-step.
      FSDTU=FLSDTU(ICFD); FSDTV=FLSDTV(ICFD)
      FSDTW=FLSDTW(ICFD); FSDTP=FLSDTP(ICFD)
      FSDTT=FLSDTT(ICFD); FSDTK=FLSDTK(ICFD)
      FSDTE=FLSDTE(ICFD)

C Maximum number of iterations.
      MAXIT=MAXITR(ICFD)

C Monitored position (as defined in zone .dfd file).
      IMON=IMONT(ICFD)
      JMON=JMONT(ICFD)
      KMON=KMONT(ICFD)

C Maximum source of residuals.
      SORMAX=SRMAX(ICFD)

C Indicate which equations are active.
      INCALU=CALLU(ICFD); INCALV=CALLV(ICFD)
      INCALW=CALLW(ICFD); INCALT=CALLT(ICFD)
      INCALK=KEMDL(ICFD); INCALD=KEMDL(ICFD)
      DO 121 ICTM=1,NCTM(ICFD)
        INCALPOL(ICTM)=CALPOL(ICTM,ICFD)
 121  CONTINUE

      BUOYA=BUOY(ICFD)
      BOUSSA=BOUSSI(ICFD)
      IZEROT=ZEROT(ICFD)
      IZanKE=ZandKE(ICFD)
      IMITZ=MITzero(ICFD)

C If the zero-equation model is to be used for the preliminary run
C before activating the k-epsilon model then activate the zero-equation
C model for the first iteration.
      if(IZanKE) IZEROT=.TRUE.

C Set the flag to indicate that the Yuan wall functions have not been
C invoked this time-step. This will trigger the initialisation of the
C velocity scale based on the heat flux (Uq) prior to applying the
C momentum wall functions the first time through.
      Uqinit = .false.

C Pressure reference position (determined in subroutine NEW2OLD).
      IPREF=IPRESF(ICFD)
      JPREF=JPRESF(ICFD)
      KPREF=KPRESF(ICFD)
      
C Set sweeping control.
      NSWPU=NSSWPU(ICFD); NSWPV=NSSWPV(ICFD)
      NSWPW=NSSWPW(ICFD); NSWPP=NSSWPP(ICFD)
      NSWPK=NSSWPK(ICFD); NSWPD=NSSWPE(ICFD)
      NSWPT=NSSWPT(ICFD)

C Initialise renormalisation factors.
      RENORM2=1.0; RENORMU=1.0; RENORMV=1.0
      RENORMW=1.0; RENORMT=1.0; RENORMK=1.0
      RENORME=1.0

C Initialise variables for flatline detection and automatic grid
C refinement.
      resorma=0.; resorua=0.; resorva=0.; resorwa=0.;
      xpmona=0.; xumona=0.; xvmona=0.; xwmona=0.
      npstuc=0; nustuc=0; nvstuc=0; nwstuc=0
      IFNDWT(ICFD)=0
      do i=1,MRFN
        IRFNCLS(ICFD,i,1)=0
      enddo
      nitrav=100    ! iterations to average over
      xstucrin=0.02 ! minimum negative change in averaged residuals, as proportion of convergence criterion
      xstucrip=0.1  ! minimum positive change in averaged residuals, as proportion of convergence criterion
      nstucriall=2  ! number of times in a row for all residuals
      nstucrione=5  ! number of times in a row for one residual

C Set up heat injections at sources.
C Check for links between sources and operations file.
C Recover definitions for current day type and timeF.
      if (IHRF.eq.1) then 
        ID=IDWF
      else
        ID=IDWP
      endif
      if (btimef.gt.24.) then
        btimef=btimef-24.
        ID=IDWF
      endif
      if(ID.EQ.IDWE1)then
        IDAY=2
      elseif(ID.EQ.IDWE2)then
        IDAY=3
      else
        IDAY=1
      endif
      
      do 10 IV=1,NVOL(ICFD)
        if (IDcasgn(IV,ICFD).gt.0) then

C Link defined. Set heat gain (convective fraction).  
C Calculate the current period id for identified casual gain.
          IGN=IDcasgn(IV,ICFD)
          IPER=NCGPER(ICP,IDAY,IGN)
          do 30 I=1,NCGPER(ICP,IDAY,IGN)
            if (BTIMEF.le.TCGS(ICP,IDAY,IGN,I+1).and.
     &          BTIMEF.gt.TCGS(ICP,IDAY,IGN,I)) then
              IPER=I
            endif
 30       continue
          VOLHeat(IV,ICFD)=CGSENC(ICP,IDAY,IGN,IPER)*
     &                     CGCTL(ICP,2,IGN)*Fcasgn(IV,ICFD)
        endif
 10   continue

C Set up contaminant injections at sources.
      IF(NCTM(ICFD).gt.0)CALL CONCSRC

C Set up momentum boundary conditions at openings.
      IF(ICFMFS(ICFD).EQ.1)CALL INOUT
  
      if(.not.INCALT) call UNIFORMT  

      CALL PROPS(1)
      WRITE(ICFMON(ICFD),1030)ITIMST

C Record Adaptive Conflation Control (ACC) actions (begin).
      IF(abs(IBLACT).eq.4.or.abs(IBLACT).eq.5)THEN
        write(IACC(icfd),*)
        write(IACC(icfd),*)
     &    'Initiating CFD simulation with parameters:'
        if(INCALU) write(IACC(icfd),*) '  U-momentum active'
        if(INCALV) write(IACC(icfd),*) '  V-momentum active'
        if(INCALW) write(IACC(icfd),*) '  W-momentum active'
        if(INCALT) write(IACC(icfd),*) '  Energy equation active'
        if(INCALK) write(IACC(icfd),*) '  k-epsilon turb model active'
        if(IZanKE) write(IACC(icfd),*) '  k-epsilon + fixed mu-t active'
        if(IMITZ)  write(IACC(icfd),*) '  Chen & Xu turb model active'
        if(BUOYA)  write(IACC(icfd),*) '  Buoyancy turned on'
        if(BOUSSA) write(IACC(icfd),*)
     &  '  Boussinesq approx. active with reference temp. ',TBAR(ICFD)
        do 20 IV=1,NVOL(ICFD)
          if (IDcasgn(IV,ICFD).gt.0) then
            write(IACC(icfd),*) 'Heat gain set from casual gain ',
     &         IDcasgn(IV,ICFD),' to ',VOLHeat(IV,ICFD),'W in volume ',
     &         VOLNAME(IV,ICFD)
          endif
 20     continue
        write(IACC(icfd),*) '  Convergence criteria: ',MAXIT,SORMAX
        write(IACC(icfd),*) '  Relaxation factors: ',URFU,URFV,URFW,
     &                             URFP,URFT,URFK,URFE,URFVIS,URFDEN
      else

C Debug.
C        call isunix(unixok)
C        if(unixok)then
C          write(6,*) ' '
C          write(6,*) 'Initiating CFD simulation with parameters:'
C          if(INCALU) write(6,*) '  U-momentum active'
C          if(INCALV) write(6,*) '  V-momentum active'
C          if(INCALW) write(6,*) '  W-momentum active'
C          if(INCALT) write(6,*) '  Energy equation active'
C          if(INCALK) write(6,*) '  k-epsilon turb model active'
C          if(IZanKE) write(6,*) '  k-epsilon + fixed mu-t active'
C          if(IMITZ)  write(6,*) '  Chen & Xu turb model active'
C          if(BUOYA)  write(6,*) '  Buoyancy turned on'
C          if(BOUSSA) write(6,*) '  Boussinesq approx. active',
C     &                          ' with reference temp. ',TBAR(ICFD)
C          do 21 IV=1,NVOL(ICFD)
C            if (IDcasgn(IV,ICFD).gt.0) then
C              write(6,*) 'Heat gain set from casual gain ',
C     &         IDcasgn(IV,ICFD),' to ',VOLHeat(IV,ICFD),'W in volume ',
c     &         VOLNAME(IV,ICFD)
C            endif
C 21       continue
C          write(6,*) '  Convergence criteria: ',MAXIT,SORMAX
C          write(6,*) '  Relaxation factors: ',URFU,URFV,URFW,URFP,URFT,
C     &                                        URFK,URFE,URFVIS,URFDEN
C        endif
       ENDIF
C Record ACC actions (end).

C Export boundary conditions.
      write(ICFDBC(ICFD),'(a)')' '
      write(ICFDBC(ICFD),'(a)')'###################################'
      write(ICFDBC(ICFD),'(a,i2.2)')
     &  'boundary conditions for domain ',ICFD
      write(ICFDBC(ICFD),'(a,i3.3,a,f5.2)')
     &  'on day ',IDYP,' at time ',BTIMEF
      write(ICFDBC(ICFD),'(a)')'###################################'
      CALL EXCFDBC(ICFDBC(ICFD),IER)

C Initialise plot counter.
      IPLOT=0

C Iteration loop.
 320  NPLT=MAXIT/MFRP
      IF(mod(MAXIT,MFRP).NE.0) NPLT=NPLT+1
      ICALL=0

C If in text mode, print monitoring headlines.
      IF(LRENORM) THEN
        IF(MMOD.NE.8) WRITE(IUOUT,420)
        WRITE(ICFMON(ICFD),420)
      ELSE
        IF(MMOD.NE.8) WRITE(IUOUT,410)
        WRITE(ICFMON(ICFD),410)
      ENDIF

C Iterate until solution converges.
 300  NITER=NITER+1

C If both the zero-equation and k-epsilon models are being used,
C determine which to activate at this iteration.
      if(IZanKE) then
        if(NITER.ge.nZtoKE(ICFD)) then
          INCALK=.TRUE.
          INCALD=.TRUE.
          IZEROT=.FALSE.
        else
          IZEROT=.TRUE.
          INCALK=.FALSE.
          INCALD=.FALSE.
        endif
      endif

C Check for blockages. If found then set velocities to zero.
      if(NBLK(ICFD).gt.0) then 
        do 60 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD) 
          if (BCOFF(iv)) cycle
          do 70 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 71 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 72 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                Uf(I,J,K)=0.0
                Uf(I+1,J,K)=0.0
                Vf(I,J,K)=0.0
                Vf(I,J+1,K)=0.0
                Wf(I,J,K)=0.0
                Wf(I,J,K+1)=0.0
 72           continue
 71         continue
 70       continue
 60     continue
      endif

C Call equation solvers.
      IF(INCALU) CALL CALCU
      IF(INCALV) CALL CALCV
      IF(INCALW) CALL CALCW
      CALL CALCP
      IF(INCALK) CALL CALCTE
      IF(INCALD) CALL CALCED
      IF(INCALT) CALL CALCT
      DO 122 ICTM=1,NCTM(ICFD)
        IF(INCALPOL(ICTM))CALL CALCCONC(ICTM)
 122  CONTINUE
      CALL PROPS(2)
      
      ICALL=ICALL+1
      IF(ABS(IBLACT).EQ.2)THEN

C Integrated thermal conflation.
        IF(ICALL.EQ.10.OR.NITER.EQ.MAXIT)THEN

C Determine wall temperatures using ESP-r back substitution procedure.
          CALL CFMZBK

C Re-establish solid boundary temperatures computed by back
C substitution procedure.
          CALL BS2CFDSB
          ICALL=0
        ENDIF
      ENDIF

C Re-establish the inlet and outlet boundary conditions.
      CALL INOUT

C If enabled, evaluate the normalised residuals for each variable.
      IF(LRENORM) THEN
        IF(NITER.EQ.2) RENORM2=RESORM
        RESORM=RESORM/RENORM2
        RESORU=RESORU/RENORMU
        RESORV=RESORV/RENORMV
        RESORW=RESORW/RENORMW
        RESORT=RESORT/RENORMT
        RESORK=RESORK/RENORMK
        RESORE=RESORE/RENORME

C Find the maximum residual, SORCE.
        SORCE=AMAX1(RESORM,RESORU,RESORV,RESORW,RESORT,RESORK,RESORE)
      ELSE

C Make the residuals relative to the inlet mass flow if one exist.
        if (flwin.gt.small) RESORM=RESORM/flwin

        if (XMONIN.gt.small) then
          RESORU=RESORU/XMONIN
          RESORV=RESORV/XMONIN
          RESORW=RESORW/XMONIN
        endif

C Find the maximum source of residual.
        SORCE=AMAX1(RESORM,RESORU,RESORV,RESORW)
      ENDIF

C Write/plot residuals and monitoring values.
C First check if output required for this iteration.
C If in text or script mode, report less frequently,
C especially if the domain is large.
      if (mod(NITER,NPLT).eq.0.or.NITER.le.1) then

C Check time taken to run since last check.
C        write(6,*)'checking time'
        if(NITER.eq.1)then
          call cpu_time(sec_now)
          NITER_pre=1
C          write(6,*)'NITER=1, sec_now=',sec_now
        else
C          write(6,*)'NITER=',NITER,NITER_pre
          sec_pre=sec_now
          call cpu_time(sec_now)
          sec_tkn=(sec_now-sec_pre)/real((NITER-NITER_pre))
C          write(6,*)'sec_pre=',sec_pre,'sec_now=',sec_now,
C     &                                        'sec_tkn=',sec_tkn
          NITER_pre=NITER

C In graphics mode, re-display target is 2 seconds.
          IF(MMOD.eq.8)THEN
            NPLT=50  ! Initial assumption
            if ((sec_tkn*50.).lt.2.0) then
              NPLT=100  ! Maximum NPLT
            else

C Output ~ every 2 seconds, constrained to increments of 25 iterations (rounded up).
              NPLT=int(2.0/sec_tkn)
              sec_tkn=real(NPLT)/25.  ! sec_tkn no longer needed, use as temporary variable.
              NPLT=(int(sec_tkn)+1)*25
            endif
          ELSE  ! we have text mode
            if ((sec_tkn*50.).lt.10.0) then
              NPLT=200  ! Maximum NPLT of 200.
            else

C Output ~ every 15 seconds, constrained to increments of 25 iterations (rounded up).
              NPLT=int(15.0/sec_tkn)
              sec_tkn=real(NPLT)/25.  ! sec_tkn no longer needed, use as temporary variable.
              NPLT=(int(sec_tkn)+1)*25
            endif
          ENDIF
        endif
C        write(6,*)'NPLOT is now=',NPLT

C If in graphical mode, maximum NPLT of 50.
        if ((MMOD.eq.8).and.(NPLT.gt.50)) then
          NPLT=50
        endif

C Write residuals and monitoring values to file.
        WRITE(ICFMON(ICFD),312)NITER,RESORU,RESORV,RESORW,RESORM,
     &         RESORT,RESORK,RESORE,Uf(IMON,JMON,KMON),
     &         Vf(IMON,JMON,KMON),Wf(IMON,JMON,KMON),P(IMON,JMON,KMON),
     &         Tf(IMON,JMON,KMON),TEf(IMON,JMON,KMON),
     &         EDf(IMON,JMON,KMON)

C Do not display anything for first 2 iterations.
        if(NITER.GE.3) then

C In graphics mode => plot data.
          IF(MMOD.eq.8)THEN
            NCRV=1
            CRES(1)=RESORM
            CMON(1)=P(IMON,JMON,KMON)
            LRES(1)='Mass                '
            LMON(1)='Pressure            '

C Check which momentum equations are solved.
            if (INCALU) then
              NCRV=NCRV+1
              CRES(NCRV)=RESORU
              CMON(NCRV)=Uf(IMON,JMON,KMON)
              LRES(NCRV)='Momentum X-direction'
              LMON(NCRV)='Velocity X-direction'
            endif
            if (INCALV) then
              NCRV=NCRV+1
              CRES(NCRV)=RESORV
              CMON(NCRV)=Vf(IMON,JMON,KMON)
              LRES(NCRV)='Momentum Y-direction'
              LMON(NCRV)='Velocity Y-direction'
            endif
            if (INCALW) then
              NCRV=NCRV+1
              CRES(NCRV)=RESORW
              CMON(NCRV)=Wf(IMON,JMON,KMON)
              LRES(NCRV)='Momentum Z-direction'
              LMON(NCRV)='Velocity Z-direction'
            endif
            CALL PLOT(ICFD,IPLOT,NPLT,ITIMST,NITER,SORMAX,NCRV,CRES,
     &                CMON,LRES,LMON)
          else

C Not in graphics mode => list data.
            WRITE(IUOUT,312)NITER,RESORU,RESORV,RESORW,RESORM,RESORT,
     &         RESORK,RESORE,Uf(IMON,JMON,KMON),Vf(IMON,JMON,KMON),
     &         Wf(IMON,JMON,KMON),P(IMON,JMON,KMON),Tf(IMON,JMON,KMON),
     &         TEf(IMON,JMON,KMON),EDf(IMON,JMON,KMON)
          endif
        endif
      else
        if(NITER.eq.1.and.IBLACT.eq.0) then
          CALL EDISP(IUOUT,'Monitoring will commence momentarily.')
          IF(MMOD.eq.8)THEN
            CALL EDISP(IUOUT,
     &             '`<` on plot represents convergence criterion.')
          endif
        endif
      endif

C Write solution to a temporary file every 100 iterations. This is useful
C if a simulation must be halted prior to convergence: the saved data can
C be used to set initial values when the simulation is resumed.
C      IF(mod(NITER,100).EQ.0) THEN
C        IER=0
C        call INTSTR(ICFD,nbchar,ISWD,IER)
C        write(outfil,'(a8,a)')'tmp_DFS_',nbchar(1:ISWD)
C        CLOSE(icftmp)
C        CALL FPOPEN(icftmp,IER,6,3,outfil)
C        REWIND(icftmp)
C        CALL PRNNEW(icftmp)
C      ENDIF

C Check for maximum iterations or acceptable maximum residual.
      IF(NITER.EQ.MAXIT)GOTO 302
      IF(SORCE.lt.SORMAX.and.NITER.ge.20)GOTO 302

C At regular intervals after 1000 interations, check for flatlined
C residuals characterised by: 
C 1. All averaged residuals changing < xstucri*100% over the last
C    nitrav iterations, nstucriall times in a row.
C 2. Any one averaged residual changing < xstucri*100% over the last
C    nitrav interations, nstucrione times in a row.  Only check
C    equations that have not already converged for this condition.
      if (DOFLT .and. NITER.gt.1000) then
        resorma=resorma+RESORM/real(nitrav)
        if (INCALU) resorua=resorua+RESORU/real(nitrav)
        if (INCALV) resorva=resorva+RESORV/real(nitrav)
        if (INCALW) resorwa=resorwa+RESORW/real(nitrav)
        if (NITER.eq.nitrav) then
          resormap=resorma
          resorma=0.
          if (INCALU) then
            resoruap=resorua
            resorua=0.
          endif
          if (INCALV) then
            resorvap=resorva
            resorva=0.
          endif
          if (INCALW) then
            resorwap=resorwa
            resorwa=0.
          endif
        elseif ((NITER/nitrav)*nitrav.eq.NITER) then
          xdif=resormap-resorma
          ok=.false.
          if (xdif.gt.0. .and. xdif.lt.SORMAX*xstucrin) then
            ok=.true.
          elseif (xdif.lt.0. .and. xdif.gt.-SORMAX*xstucrip) then
            ok=.true.
          endif
          if (ok) then
            npstuc=npstuc+1
          else
            npstuc=0
          endif
C         write(6,*)'Pres',(resormap-resorma)/resormap,npstuc
          if (INCALU) then
            xdif=resoruap-resorua
            ok=.false.
            if (xdif.gt.0. .and. xdif.lt.resoruap*xstucrin) then
              ok=.true.
            elseif (xdif.lt.0. .and. xdif.gt.-resoruap*xstucrip) then
              ok=.true.
            endif
            if (ok) then
              nustuc=nustuc+1
            else
              nustuc=0
            endif
C           write(6,*)'Ures',(resoruap-resorua)/resoruap,nustuc
          endif
          if (INCALV) then
            xdif=resorvap-resorva
            ok=.false.
            if (xdif.gt.0. .and. xdif.lt.resorvap*xstucrin) then
              ok=.true.
            elseif (xdif.lt.0. .and. xdif.gt.resorvap*xstucrip) then
              ok=.true.
            endif
            if (ok) then
              nvstuc=nvstuc+1
            else
              nvstuc=0
            endif
            ! write(6,*)'Vres',(resorvap-resorva)/resorvap,nvstuc
          endif
          if (INCALW) then
            xdif=resorwap-resorwa
            ok=.false.
            if (xdif.gt.0. .and. xdif.lt.resorwap*xstucrin) then
              ok=.true.
            elseif (xdif.lt.0. .and. xdif.gt.-resorwap*xstucrip) then
              ok=.true.
            endif
            if (ok) then
              nwstuc=nwstuc+1
            else
              nwstuc=0
            endif
C           write(6,*)'Wres',(resorwap-resorwa)/resorwap,nwstuc
          endif
          if ((npstuc.ge.nstucriall
     &         .and.(.not.INCALU.or.nustuc.ge.nstucriall)
     &         .and.(.not.INCALV.or.nvstuc.ge.nstucriall)
     &         .and.(.not.INCALW.or.nwstuc.ge.nstucriall))
     &        .or.(npstuc.ge.nstucrione.and.RESORM.gt.SORMAX)
     &        .or.(nustuc.ge.nstucrione.and.RESORU.gt.SORMAX)
     &        .or.(nvstuc.ge.nstucrione.and.RESORV.gt.SORMAX)
     &        .or.(nwstuc.ge.nstucrione.and.RESORW.gt.SORMAX)) then

            write(outs,'(a,i5,a)') 
     &        '   Flatlined residual detected at iteration ',NITER,'.'
            CALL EDISP(IUOUT,outs)
            
C If grid refinement is active try refining grid resolution in areas of
C poor convergence. Set flag to find worst cells in terms of worst
C equation on the next iteration.
            if (DORFN) then
         
              if (RESORM.lt.SORCE+0.001.and.RESORM.gt.
     &                                           sorce-0.001) then
                IFNDWT(ICFD)=1
              elseif (RESORU.lt.SORCE+0.001.and.RESORU.gt.
     &                                           sorce-0.001) then
                IFNDWT(ICFD)=2
              elseif (RESORV.lt.SORCE+0.001.and.RESORV.gt.
     &                                           sorce-0.001) then
                IFNDWT(ICFD)=3
              elseif (RESORW.lt.SORCE+0.001.and.RESORW.gt.
     &                                           sorce-0.001) then
                IFNDWT(ICFD)=4
              elseif (LRENORM) then
                if (RESORT.lt.SORCE+0.001.and.RESORT.gt.
     &                                           sorce-0.001) then
                  IFNDWT(ICFD)=5
                elseif (RESORK.lt.SORCE+0.001.and.RESORK.gt.
     &                                           sorce-0.001) then  
                  IFNDWT(ICFD)=6
                elseif (RESORE.lt.SORCE+0.001.and.RESORE.gt.
     &                                           sorce-0.001) then    
                  IFNDWT(ICFD)=7
                endif  
             endif
                  
C              if (RESORM.eq.SORCE) then
C                IFNDWT(ICFD)=1
C              elseif (RESORU.eq.SORCE) then
C                IFNDWT(ICFD)=2
C              elseif (RESORV.eq.SORCE) then
C                IFNDWT(ICFD)=3
C              elseif (RESORW.eq.SORCE) then
C                IFNDWT(ICFD)=4
C              elseif (LRENORM) then
C                if (RESORT.eq.SORCE) then
C                  IFNDWT(ICFD)=5
C                elseif (RESORK.eq.SORCE) then
C                  IFNDWT(ICFD)=6
C                elseif (RESORE.eq.SORCE) then
C                  IFNDWT(ICFD)=7
C                endif
C              endif
            
C Otherwise, stop the simulation.
            else
              CALL EDISP(IUOUT,'   Stopping CFD simulation.')
              goto 302
            endif
          endif   
          
          resormap=resorma
          resorma=0.
          if (INCALU) then
            resoruap=resorua
            resorua=0.
          endif
          if (INCALV) then
            resorvap=resorva
            resorva=0.
          endif
          if (INCALW) then
            resorwap=resorwa
            resorwa=0.
          endif

C This code should only be used if grid refinement is active.
        elseif (IFNDWT(ICFD).gt.0) then

C Found worst cells: try to refine the grid in these areas.
C If this does not improve the situation, stop the simulation.
          CALL EDISP(IUOUT,'   Attempting to refine the grid.')
          CALL RFNCFDGRD(ok,IER)
          IFNDWT(ICFD)=0
          do i=1,NRFN
            IRFNCLS(ICFD,i,1)=0
          enddo
          if (ok .and. IER.eq.0) CALL GRID(IER)
          if (.not.ok .or. IER.ne.0) then
            CALL EDISP(IUOUT,
     &            '   ... failed, stopping the CFD simulation.')
            goto 302
          else           
            CALL EDISP(IUOUT,
     &            '   ... done, resuming the CFD simulation.')
          endif     
        endif
      endif
      goto 300

  302 continue   ! maximum iterations or acceptable residual
      
C Check convergence.
      IF(SORCE.GT.SORMAX)THEN

C Not converged.
        WRITE(OUT,'(A,I5,A)')
     &       '   Solution did not converge after ',NITER,' iterations.'
        call edisp(iuout,OUT)

C Case of mfs active, the number of dfs-mfs iterations less than 10
C and not running in script mode.
        IF(ICFMFS(ICFD).EQ.1.AND.NMFSRUN.LE.10.AND.MMOD.NE.-6)THEN

C Help message for next two dialogs.
          helptopic='dfs_mass_residual'
          call gethelptext(helpinsub,helptopic,nbhelp)

C Check if mass residual is the maximum residual.
          CALL ECLOSE(resorm,sorce,0.01,closer)

C Force continue if closer is false.
          IW=4

C Mass residual is maximum residual.
          IF(CLOSER)THEN
            call EASKABCD('Solution did not converge.',
     &        'Options:','reduce vel. URF +100 iter','+10 mfs iter',
     &        'mark converged','continue',IW,nbhelp)
          ENDIF

C Reduce velocity URFs and add 100 further iterations.
          if (IW.eq.1) then
            call edisp(IUOUT,
     &        '   ... reducing velocity URFs + 100 iterations.')
            URFU=URFU*0.75
            URFV=URFV*0.75
            URFW=URFW*0.75
            NOMAXIT=MAXIT
            MAXIT=MAXIT+100

            NMFSRUN=NMFSRUN+1
            IF (NITER.LT.MAXIT) THEN
              CALL REPLOT (NOMAXIT,MAXIT,IPLOT)
              GOTO 320
            ENDIF

C Add 10 further mfs iterations. 
          elseif (IW.eq.2) then
            CALL EDISP(IUOUT,'   ... + 10 cfd-mfs iterations.')
            NOMAXIT=MAXIT
            MAXIT=MAXIT+10
            NMFSRUN=NMFSRUN+1
            IF (NITER.LT.MAXIT) THEN
              CALL REPLOT (NOMAXIT,MAXIT,IPLOT)
              GOTO 320
            ENDIF

C Mark converged.
          elseif (IW.eq.3)then
            CONVER=.TRUE.
            WRITE(OUT,'(A,I5,A)')'   Marked converged after ',
     &                            NITER,' iterations.'
            CALL EDISP(IUOUT,OUT)

            IF(abs(IBLACT).eq.4 .or. abs(IBLACT).eq.5)THEN
              write(IACC(icfd),'(a)') OUT(1:lnblnk(out))
            ENDIF
            goto 42

C Continue to following code.
          elseif (IW.eq.4)then
            continue
          ENDIF

C Momentum residual is the maximum residual. 
          call EASKABCD('Solution did not converge.',
     &      'Options:','reduce velocity URF +500 iter',
     &      '+500 iter','mark converged','continue',IW,nbhelp)

C Reduce velocity under-relaxation factors and add 500 further iterations.
          if (IW.eq.1) then
            NOMAXIT=MAXIT
            MAXIT=MAXIT+500
            URFU=URFU*0.75
            URFV=URFV*0.75
            URFW=URFW*0.75
            IF (NITER.LT.MAXIT) THEN
              GOTO 320
            ENDIF

C Add 500 further iterations.
          elseif (IW.eq.2) then
            NOMAXIT=MAXIT
            MAXIT=MAXIT+500
            IF (NITER.LT.MAXIT) THEN
              GOTO 320
            ENDIF

C Marked converged.
          elseif (IW.eq.3) then
            CONVER=.TRUE.
            WRITE(OUT,'(A,I5,A)')'   Marked converged after ',
     &                            NITER,' iterations.'
            CALL EDISP(IUOUT,OUT)
            IF( abs(IBLACT).eq.4 .or. abs(IBLACT).eq.5)THEN
              write(IACC(icfd),'(a)') OUT(1:lnblnk(out))
            ENDIF
            goto 42

          elseif (IW.eq.4) then
            continue
          endif

        ENDIF

C Case of mfs inactive or the number of dfs-mfs iterations greater than 10
C or not running in script mode (or fall through from above code).
        NOMAXIT=MAXIT
        CALL CHECK(MAXIT,CONVER)
        if(.not.conver)then
          if(NITER.lt.MAXIT)then
            call replot(NOMAXIT,MAXIT,IPLOT)
            GOTO 320  ! next iteration
          endif
        endif

C Converged within iteration limit.
      ELSE
        CONVER=.TRUE.
        call cpu_time(sec_conv)
        WRITE(OUT,'(A,I5,A,f8.1,A)')'   Converged in ',NITER,
     &           ' iterations (elapsed time ',sec_conv,' s).'
        CALL EDISP(IUOUT,OUT)

        if(abs(IBLACT).eq.4 .or. abs(IBLACT).eq.5)THEN
          write(IACC(icfd),'(a)') OUT(1:lnblnk(out))
        endif

      ENDIF   ! end convergence test

C Set blockage temperatures.
      call BLKSETT
 
C Write final monitoring/residual information if not done above.
  42  if(mod(NITER,NPLT).ne.0)then
        WRITE(ICFMON(ICFD),312)NITER,RESORU,RESORV,RESORW,RESORM,
     &     RESORT,RESORK,RESORE,Uf(IMON,JMON,KMON),Vf(IMON,JMON,KMON),
     &     Wf(IMON,JMON,KMON),P(IMON,JMON,KMON),Tf(IMON,JMON,KMON),
     &     TEf(IMON,JMON,KMON),EDf(IMON,JMON,KMON)

C Write final solutions to screen if not in graphic mode.
        if (MMOD.ne.8) then
          WRITE(IUOUT,312)NITER,RESORU,RESORV,RESORW,RESORM,RESORT,
     &         RESORK,RESORE,Uf(IMON,JMON,KMON),Vf(IMON,JMON,KMON),
     &         Wf(IMON,JMON,KMON),P(IMON,JMON,KMON),Tf(IMON,JMON,KMON),
     &         TEf(IMON,JMON,KMON),EDf(IMON,JMON,KMON)
        endif

C Append to the ACC file.
        IF( abs(IBLACT).eq.4 .or. abs(IBLACT).eq.5)THEN
          WRITE(IACC(icfd),312)NITER,RESORU,RESORV,RESORW,RESORM,RESORT,
     &         RESORK,RESORE,Uf(IMON,JMON,KMON),Vf(IMON,JMON,KMON),
     &         Wf(IMON,JMON,KMON),P(IMON,JMON,KMON),Tf(IMON,JMON,KMON),
     &         TEf(IMON,JMON,KMON),EDf(IMON,JMON,KMON)
        endif
      endif

C Calculate mean age of air, if required and not preliminary CFD 
C simulation.

C Debug.
C      write(6,*) 'LMA',CALLMA(ICFD),'  gopher',gophrun

      if (CALLMA(ICFD).and.(.not.gophrun)) call calclma(ITIMST)

      RETURN
      
C Format statements.
 312  FORMAT(I7,2X,7E11.3,/,9X,7E11.3)
 410  FORMAT(/,' DFS - Monitoring of residuals & variables',/,/,
     &'   ITER',8X,'U',10X,'V',10X,'W',10X,'P', 10X,'T',10X,'K',
     &7X,'Eps.',/)
 420  FORMAT(/,' DFS - Monitoring of renormalised residuals & ',
     &' variables',/,/,'   ITER',8X,'U',10X,'V',10X,'W',10X,'P',
     &10X,'T',10X,'K',7X,'Eps.',/)
 1030 FORMAT(/,'ITIME =',I3)
      END


C ********************* INIT *********************
C Initialise CFD simulation parameters.

      SUBROUTINE INIT
#include "building.h"
#include "cfd.h"

      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)

      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRp/DENp(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPp/Tp(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      CHARACTER POLNAM*12
      LOGICAL CALLMA,CALPOL
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      COMMON/ICFNOD/ICFD,ICP
     
C Initialise variables.
      DO 200 I=1,NI
        DO 2001 J=1,NJ
          DO 2002 K=1,NK
            if (I.eq.1) then
              Up(I,J,K)=0.
              Uf(I,J,K)=0.
            else
              Up(I,J,K)=UINIT(ICFD)
              Uf(I,J,K)=UINIT(ICFD)
            endif
            if (J.eq.1) then
              Vp(I,J,K)=0.
              Vf(I,J,K)=0.
            else
              Vp(I,J,K)=VINIT(ICFD)
              Vf(I,J,K)=VINIT(ICFD)
            endif
            if (K.eq.1) then
              Wp(I,J,K)=0.
              Wf(I,J,K)=0.
            else
              Wp(I,J,K)=WINIT(ICFD)
              Wf(I,J,K)=WINIT(ICFD)
            endif
            DENp(I,J,K)=AIRDEN(TINIT(ICFD))
            DENf(I,J,K)=AIRDEN(TINIT(ICFD))
C            P(I,J,K)=GRAV*DENf(I,J,K)*(ZP(KPREF)-ZP(K))
            P(I,J,K)=PINIT(ICFD)
            PP(I,J,K)=0.0
            Tp(I,J,K)=TINIT(ICFD)
            Tf(I,J,K)=TINIT(ICFD)
            DO 2010 ICTM=1,NCTM(ICFD)
              POLCONCp(ICTM,I,J,K)=POLINIT(ICFD,ICTM)
              POLCONCf(ICTM,I,J,K)=POLINIT(ICFD,ICTM)
 2010       CONTINUE
            AGEf(I,J,K)=0.0 
            TEp(I,J,K)=TEINIT(ICFD)
            TEf(I,J,K)=TEINIT(ICFD)
            EDp(I,J,K)=EDINIT(ICFD)
            EDf(I,J,K)=EDINIT(ICFD)
            VIS(I,J,K)=VISCOS
            GAMH(I,J,K)=VISCOS/PRANDL
            BETA(I,J,K)=AIRBET(TINIT(ICFD))

C Concentration of H2O corresponding to RH of 50% (equation
C from Recknagel 97/98, p.113).
            TSAT=TINIT(ICFD)
            PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04
     &               *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4)

C Use thermodynamic relations to convert partial pressure to [kg/kg].       
C Uncomment the following line to initialise moisture as contaminant
C number 2.
C            JHUMINDX(ICFD)=2
            IF(JHUMINDX(ICFD).NE.0)THEN
              POLCONCf(JHUMINDX(ICFD),I,J,K)=0.5*PSAT*18.02/
     &                                 (0.5*PSAT*18.02+100000.0*28.96)
              POLCONCp(JHUMINDX(ICFD),I,J,K)=
     &          POLCONCf(JHUMINDX(ICFD),I,J,K)
            ENDIF
 2002     CONTINUE
 2001   CONTINUE
 200  CONTINUE

      RETURN
      END

C ******************** UNIFORMT *********************
C Set a uniform temperature everywhere if temperature is not
C being calculated (needed for mass balance and convergence).
C Checks temperature in all inflows, averages temperature according
C to the mass flowrate and sets it uniform in the CFD domain.

      SUBROUTINE UNIFORMT
#include "building.h"
#include "cfd.h"

      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)

      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      COMMON/TEMPp/Tp(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     &             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN

      HELPTM=0.0
      HELPM=0.0
      HELPT=0.0 

C Examine each opening in turn.
      DO 100 L=1,NOPEN(ICFD)

C Iterate over cells covering opening.
        DO 101 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
          DO 101 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 101 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(FIXM(L,ICFD).GT.0.0)THEN
      
C It is an inflow. 
                HELPTM=HELPTM+FIXT(L,ICFD)*FIXM(L,ICFD)
                HELPM=HELPM+FIXM(L,ICFD)
              ENDIF
 101    CONTINUE
 100  CONTINUE 
      HELPT=HELPTM/HELPM 
      TINIT(ICFD)=HELPT 

C Initialise temperature.
      DO 200 I=1,NI
        DO 200 J=1,NJ
          DO 200 K=1,NK
            Tp(I,J,K)=TINIT(ICFD)
            Tf(I,J,K)=TINIT(ICFD)
 200  CONTINUE

      RETURN
      END

C ********************* PROPS *********************
C Calculate fluid properties that are dependent on the solution.
C ICALL stops the initisation of blockage data until after the
C first iteration.

      SUBROUTINE PROPS(ICALL)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     1            TURBIN,ALAMDA,PRTE,PRED
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/CONST/GREAT,small,GRAV
      COMMON/URFDEN/URFDEN
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/INCALP/INCALPOL(MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/ZTURB/rMOOT(MNZ),nZtoKE(MNZ)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)     
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      CHARACTER POLNAM*12
      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ
      LOGICAL CALLMA,CALPOL,OK,LINVOL,INCALPOL

      DO 100 I=1,NI
        DO 1001 J=1,NJ
          DO 1002 K=1,NK

C Update eddy viscosity based on current iteration's k and epsilon solutions.
C VIS is the eddy viscosity, not the kinematic eddy viscosity (units are Pa*s).
            if(IZEROT) then

C Zero-equation fixed-eddy-viscosity turbulence model.
              VIS(I,J,K) = (rMOOT(ICFD)+1.)*VISCOS
            elseif(IMITZ) then

C MIT zero-eqn turbulence model. Refer to eq.7 of Chen & Xu 1998 E&B paper.
              CALL MITlenV(I,J,K,rleng,Vmean)
              VISOLD=VIS(I,J,K)
              VIS(I,J,K) = VISCOS + 0.03874*DENf(I,J,K)*Vmean*rleng
              VIS(I,J,K)=URFVIS*VIS(I,J,K)+(1.-URFVIS)*VISOLD
            else

C k-epsilon turbulence model.
              VISOLD=VIS(I,J,K)
              if (ABS(EDf(I,J,K)).lt.small) then
                VIS(I,J,K)=VISCOS
              else
                VIS(I,J,K)=VISCOS+  
     &                   DENf(I,J,K)*TEf(I,J,K)**2*CMU/ABS(EDf(I,J,K))
              endif
              VIS(I,J,K)=URFVIS*VIS(I,J,K)+(1.-URFVIS)*VISOLD
            endif

C Diffusion coefficient of the energy equation. Note that the energy
C equation has been normalised by the heat capacity, SH.
C VISCOS/PRANDL = conductivity/SH.
C            GAMH(I,J,K)=VISCOS/PRANDL + (VIS(I,J,K)-VISCOS)/PRANDT
C The above is equivalent to:
            GAMH(I,J,K)=VIS(I,J,K)/PRANDT

            IF(BOUSSA) THEN

C Apply the Boussinesq approximation whereby density is a constant,
C evaluated at the reference temperature specified by the user.
              DENf(I,J,K)=AIRDEN(TBAR(ICFD))

            ELSE

C Boussinesq approximation not applied, calculate density as
C a function of temperature.
              DENf(I,J,K)=URFDEN*AIRDEN(Tf(I,J,K))+(1.-URFDEN)*
     &                                                   DENf(I,J,K)

C In case of a gas mixture (concentration and/or humidity),
C the density should be calculated according to the thermodynamic rules 
C No need of underrelaxation as in rooms we have lower dependence of 
C density from concentration than from temperature.

C Reserved space for cases when pollutant/ contaminant presence affects
C density (see next set of comments if pollutant is CO2 or moisture).
              DO 123 ICTM=1,NCTM(ICFD)
               IF(INCALPOL(ICTM))THEN
               ENDIF
 123          CONTINUE

C             IF(INCALC) THEN 
C Density for CO2 at 20C is 1.842 
C <<any other constitute in the future should be given by 
C the user at 20C and supplied through a common field here>> 
C A consequent approach would be: first to calculate the branch here and then 
C underrelax the density due to temperature-dependence. Requires a lot of
C new if-statements 

C Currently it is assumed that concentration does not affects density.
C In cases where it does, the next two lines of code should be enabled.   
C              DENCON=1.842 
C              DENf(I,J,K)=1./(CONf(I,J,K)/DENCON+
C    &                   (1.-CONf(I,J,K))/DENf(I,J,K))
C             ENDIF
C
C             IF(INCALH) THEN 
C
C Reserved space if humidity affects density - similar actions as in the 
C case of concentration calculation are required. 
C             ENDIF
            ENDIF

C Thermal expansion coefficient.
            BETA(I,J,K)=AIRBET(Tf(I,J,K))

C In case of blockages, set the value of viscosity equal to zero.
C This forces the diffusion toward the wall to zero. It is a better 
C approach than applied to the walls of the room - at walls the 
C viscosity is set equal to the physical one.
C Only do this after the first iteration is complete.
            if (ICALL.eq.2.and.NBLK(ICFD).gt.0) then
              do 2006 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD)
                if (BCOFF(iv)) cycle

C If inside the blockage then set VIS and GAMH to zero.
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if(OK) then
                  VIS(I,J,K) = small 
                  GAMH(I,J,K) = small
                endif
 2006         continue
            endif
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE
 
      RETURN
      END


C ********************* CALCU *********************
C Calculate coefficients of the matrix of the momentum 
C conservation equation in X direction and solve the matrix to 
C obtain velocity component in X direction.

      SUBROUTINE CALCU
#include "building.h"
#include "cfd.h"
      
      COMMON/ICFNOD/ICFD,ICP
      COMMON/UVEL/RESORU,NSWPU,URFU,FSDTU,DXEPU(ntcelx),DXPWU(ntcelx),
     1            SEWU(ntcelx)
      COMMON/DUDVDW/DU(ntcelx,ntcely,ntcelz),DV(ntcelx,ntcely,ntcelz),
     1              DW(ntcelx,ntcely,ntcelz)
      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)  
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
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/CONST/GREAT,small,GRAV
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME
      common/SOLVER/ILISOL

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,IZEROT,IZanKE
      LOGICAL IMITZ
      LOGICAL LINVOL,OK

      double precision RESOR, SORVOL, resorfn(MRFN)

      DO 100 I=3,NIM1
        DO 1001 J=2,NJM1
          DO 1002 K=2,NKM1

C Compute areas and volume.
            AREANS=AreaNSU(I,K)
            AREAEW=AreaEWP(J,K)
            AREAHL=AreaHLU(I,J)
            VOL=VolU(I,J,K)

C Calculate convection coefficients.
            GN=(SIFS(J+1)*DENf(I,J+1,K)+SIFN(J)*DENf(I,J,K))*Vf(I,J+1,K)
            GNW=(SIFS(J+1)*DENf(I-1,J+1,K)+SIFN(J)*DENf(I-1,J,K))
     &          *Vf(I-1,J+1,K)
            GS=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))*Vf(I,J,K)
            GSW=(SIFS(J)*DENf(I-1,J,K)+SIFN(J-1)*DENf(I-1,J-1,K))
     &          *Vf(I-1,J,K)
            GE=(SIFW(I+1)*DENf(I+1,J,K)+SIFE(I)*DENf(I,J,K))*Uf(I+1,J,K)
            GP=(SIFW(I)*DENf(I,J,K)+SIFE(I-1)*DENf(I-1,J,K))*Uf(I,J,K)
            GW=(SIFW(I-1)*DENf(I-1,J,K)+SIFE(I-2)*DENf(I-2,J,K))
     &          *Uf(I-1,J,K)
            GH=(SIFH(K)*DENf(I,J,K)+SIFL(K+1)*DENf(I,J,K+1))*Wf(I,J,K+1)
            GHW=(SIFH(K)*DENf(I-1,J,K)+SIFL(K+1)*DENf(I-1,J,K+1))
     &          *Wf(I-1,J,K+1)
            GL=(SIFL(K)*DENf(I,J,K)+SIFH(K-1)*DENf(I,J,K-1))*Wf(I,J,K)
            GLW=(SIFL(K)*DENf(I-1,J,K)+SIFH(K-1)*DENf(I-1,J,K-1))
     &          *Wf(I-1,J,K)

            CN=(SIFE(I-1)*GNW+SIFW(I)*GN)*AREANS
            CS=(SIFE(I-1)*GSW+SIFW(I)*GS)*AREANS
            CE=0.5*(GE+GP)*AREAEW
            CW=0.5*(GP+GW)*AREAEW
            CH=(SIFE(I-1)*GHW+SIFW(I)*GH)*AREAHL
            CL=(SIFE(I-1)*GLW+SIFW(I)*GL)*AREAHL

C Calculate diffusion coefficients.
            VISN=(SIFN(J)*VIS(I,J,K)+SIFS(J+1)*VIS(I,J+1,K))*SIFW(I)
     &        +(SIFN(J)*VIS(I-1,J,K)+SIFS(J+1)*VIS(I-1,J+1,K))*SIFE(I-1)
            VISS=(SIFN(J-1)*VIS(I,J-1,K)+SIFS(J)*VIS(I,J,K))*SIFW(I)
     &        +(SIFN(J-1)*VIS(I-1,J-1,K)+SIFS(J)*VIS(I-1,J,K))*SIFE(I-1)
            VISH=(SIFH(K)*VIS(I,J,K)+SIFL(K+1)*VIS(I,J,K+1))*SIFW(I)
     &        +(SIFH(K)*VIS(I-1,J,K)+SIFL(K+1)*VIS(I-1,J,K+1))*SIFE(I-1)
            VISL=(SIFH(K-1)*VIS(I,J,K-1)+SIFL(K)*VIS(I,J,K))*SIFW(I)
     &        +(SIFH(K-1)*VIS(I-1,J,K-1)+SIFL(K)*VIS(I-1,J,K))*SIFE(I-1)

            DFN=VISN*AREANS/DYNP(J)
            DFS=VISS*AREANS/DYPS(J)
            DFE=VIS(I,J,K)*AREAEW/DXEPU(I)
            DFW=VIS(I-1,J,K)*AREAEW/DXPWU(I)
            DFH=VISH*AREAHL/DZHP(K)
            DFL=VISL*AREAHL/DZPL(K)

C Main coefficients for:
C Hybrid scheme.
C           AE(I,J,K)=AMAX1(ABS(0.5*CE),DFE)-0.5*CE
C           AW(I,J,K)=AMAX1(ABS(0.5*CW),DFW)+0.5*CW
C           AN(I,J,K)=AMAX1(ABS(0.5*CN),DFN)-0.5*CN
C           AS(I,J,K)=AMAX1(ABS(0.5*CS),DFS)+0.5*CS
C           AH(I,J,K)=AMAX1(ABS(0.5*CH),DFH)-0.5*CH
C           AL(I,J,K)=AMAX1(ABS(0.5*CL),DFL)+0.5*CL

C Power law scheme.
            AE(I,J,K)=APLAWCF(DFE,-CE)
            AW(I,J,K)=APLAWCF(DFW,CW)
            AN(I,J,K)=APLAWCF(DFN,-CN)
            AS(I,J,K)=APLAWCF(DFS,CS)
            AH(I,J,K)=APLAWCF(DFH,-CH)
            AL(I,J,K)=APLAWCF(DFL,CL)

            DU(I,J,K)=AREAEW

C Dynamic term.
            AP0=(SIFW(I)*DENp(I,J,K)+SIFE(I-1)*DENp(I-1,J,K))*VOL/DT
            APF=(SIFW(I)*DENf(I,J,K)+SIFE(I-1)*DENf(I-1,J,K))*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C Fictitious time-step.
            APfals=(SIFW(I)*DENf(I,J,K)+SIFE(I-1)*DENf(I-1,J,K))*
     &              VOL/fsDTU

C Source coefficients.
            SU(I,J,K)=AP0*Up(I,J,K)+DU(I,J,K)*(P(I-1,J,K)-P(I,J,K))+
     &                APfals*Uf(I,J,K)+CP*Uf(I,J,K)
            SP(I,J,K)=-AP0-APfals-CP
            DUDXP  =(Uf(i+1,j,K)-Uf(I,J,K))/SEW(I)
            DUDXW  =(Uf(I,J,K)-Uf(i-1,j,K))/SEW(I-1)
            SU(I,J,K) =(VIS(I,J,K)*DUDXP-VIS(I-1,J,K)*DUDXW)*AREAEW+
     &                   SU(I,J,K)

            GAMWN=VISN
            DVDXWN=(Vf(i,j+1,K)-Vf(i-1,j+1,K))/DXPW(I)
            GAMWS=VISS
            DVDXWS=(Vf(i,j,K)-Vf(i-1,j,K))/DXPW(I)
            SU(I,J,K) =SU(I,J,K)+(GAMWN*DVDXWN-GAMWS*DVDXWS)*AREANS

            GAMHW=VISH
            DWDXHW=(Wf(i,j,K+1)-Wf(i-1,j,K+1))/DXPW(I)
            GAMLW=VISL
            DWDXLW=(Wf(i,j,K)-Wf(i-1,j,K))/DXPW(I)
            SU(I,J,K) =SU(I,J,K)+(GAMHW*DWDXHW-GAMLW*DWDXLW)*AREAHL

 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Apply boundary conditions.
      IF(INCALK)THEN

C Log-law or Yuan wall functions.
        CALL MODUT

C Check for blockages.
        if(NBLK(ICFD).gt.0) then 
          do iblk=1,NBLK(ICFD)
            iv=INBLK(iblk,ICFD)
            if (BCOFF(iv)) cycle
            call BLKBNDU(iv)
          enddo
        endif
      ELSEIF(IMITZ)THEN

C MIT zero-equation model: no slip condition.
        CALL MODUMIT
      ELSE

C Laminar.
        CALL MODUL
      ENDIF

C Remaining coefficients and residual source calculation.
      RESORU=0.0
      RENORMU=0.0
      SAnb=0.0
      U=0.0
      V=0.0
      W=0.0
      DO 300 I=3,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AN(I,J,K)+AS(I,J,K)+AE(I,J,K)+AW(I,J,K)+
     &                AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            SAnb=AN(I,J,K)+AS(I,J,K)+AE(I,J,K)+AW(I,J,K)+
     &                AH(I,J,K)+AL(I,J,K)
            RESOR=AE(I,J,K)*Uf(i+1,j,K)+AW(I,J,K)*Uf(i-1,j,K)+
     &            AN(I,J,K)*Uf(i,j+1,K)+AS(I,J,K)*Uf(i,j-1,K)+
     &            AH(I,J,K)*Uf(i,j,K+1)+AL(I,J,K)*Uf(i,j,K-1)-
     &            AP(I,J,K)*Uf(I,J,K)+SU(I,J,K)

C Check for small supply openings and set resor to zero.  This is 
C done for the next-to-opening cell (into domain) as it is this 
C cell that has the corrected momentum.
            if(NSSO(ICFD).gt.0) then 
               do 2006 IVO=1,NSSO(ICFD)
                  IV=INSSO(IVO,ICFD)
                  OK=.false.
                  if (IVOLF(IV,ICFD).eq.1) then
                    OK=LINVOL(I-1,J,K,
     &                    IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                    JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                    KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  endif
                  if (IVOLF(IV,ICFD).eq.2) then
                    OK=LINVOL(I,J,K,
     &                    IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                    JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                    KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  endif
                  if (OK) resor=0.0
 2006          continue
            endif

C Check for blockages. 
            if(NBLK(ICFD).gt.0) then
              do 2007 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD)
                if (BCOFF(iv)) cycle

C If inside the blockage or blockage to the west then set resor to zero.
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),(IVCELLS(IV,ICFD,2)+1),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if (OK) resor=0.0
 2007         continue
            endif

            U=Uf(I,J,K)
            IF(INCALV) V=SIFE(I-1)*(Vf(I-1,J,K)+Vf(I-1,J+1,K))/2.+
     &                               SIFW(I)*(Vf(I,J,K)+Vf(I,J+1,K))/2.
            IF(INCALW) W=SIFE(I-1)*(Wf(I-1,J,K)+Wf(I-1,J,K+1))/2.+
     &                               SIFW(I)*(Wf(I,J,K)+Wf(I,J,K+1))/2.
            RENORM=AP(I,J,K)*SQRT(U*U + V*V + W*W)

            SORVOL=GREAT*VolU(I,J,K)
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=real(RENORM/SORVOL)
            ENDIF
            RESORU=real(RESORU+ABS(RESOR))
            RENORMU=RENORMU+ABS(RENORM)

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.2) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                elseif (ABS(RESOR).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                endif
              enddo
            endif

C Linear under-relaxation.
            AP(I,J,K)=AP(I,J,K)/URFU
            SU(I,J,K)=SU(I,J,K)+(1.-URFU)*AP(I,J,K)*Uf(I,J,K)

C SIMPLE scheme.
C            DU(I,J,K)=DU(I,J,K)/AP(I,J,K)

C SIMPLEC scheme.
            DU(I,J,K)=DU(I,J,K)/anotzero(AP(I,J,K)-SAnb)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE


C Check for small supply openings 
      if(NSSO(ICFD).gt.0) then

C Loop over opening and adjust momentum in small openings.
         do 2000 IVO=1,NSSO(ICFD)
            IV=INSSO(IVO,ICFD) 
            if (IVCELLS(IV,ICFD,1).eq.IVCELLS(IV,ICFD,2)) then
              I=IVCELLS(IV,ICFD,1)
              do 509 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                do 509 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)

C Set Ia=I of adjacent cell (depends on if east or west boundary).
                  OK=.false.
                  if (IVOLF(IV,ICFD).eq.1) then
                    Ia=I+1
                    Ib=I
                    OK=.true.
                  elseif (IVOLF(IV,ICFD).eq.2) then
                    Ia=I
                    Ib=I+1
                    OK=.true.
                  endif
                  if (OK) then
                    AE(Ia,J,K)=0.0; AW(Ia,J,K)=0.0
                    AN(Ia,J,K)=0.0; AS(Ia,J,K)=0.0
                    AH(Ia,J,K)=0.0; AL(Ia,J,K)=0.0

C AP does not change. Calculate the value for 
C Uf which results in the true momentum and put it in SU. 
                    SU(Ia,J,K)=AP(Ia,J,K)*Uf(Ib,J,K)/
     &                                     SQRT(VOLArea(IV,ICFD))
                  endif
 509          continue
            endif
 2000    continue
      endif

C Check for blockages.
      if(NBLK(ICFD).gt.0) then

C Loop through whole blockage,set U_velocity to zero.
        do 2002 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD) 
          if (BCOFF(iv)) cycle
          IST=max(3,IVCELLS(IV,ICFD,1))
          do 2001 I=IST,IVCELLS(IV,ICFD,2)
            do 2001 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 2001 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)             
C                SU(I,J,K)=0.0
C                AP(I,J,K)=GREAT
C                SU(I+1,J,K)=0.0
C                AP(I+1,J,K)=GREAT

                AE(I,J,K)=0.0; AW(I,J,K)=0.0; AN(I,J,K)=0.0
                AS(I,J,K)=0.0; AH(I,J,K)=0.0; AL(I,J,K)=0.0
                SP(I,J,K)=0.0; SU(I,J,K)=0.0; AP(I,J,K)=1.0

                AE(I+1,J,K)=0.0; AW(I+1,J,K)=0.0
                AN(I+1,J,K)=0.0; AS(I+1,J,K)=0.0
                AH(I+1,J,K)=0.0; AL(I+1,J,K)=0.0
                SP(I+1,J,K)=0.0; SU(I+1,J,K)=0.0
                AP(I+1,J,K)=1.0

 2001     continue
 2002   continue
      endif

C Example of calculation of momentum-sources - see also 
C the correspondent v and w equations. 
C << Keep this example as comments and make later 
C the appropriate interface developments>>. Of course, also 
C in the particular cells of momentum sources 
C <<we should set resor=0.0 - similar to sso and blockages >> 
C Allow by the interface max 5 of the 6 walls of a scalar cell 
C to be defined as momentum-sources (to allow the 6th wall 
C to be calculated so that continuity equation is satisfied). 
C
C udesir - the desired u-velocity to be achieved (user-supplied) [m/s].  
c      udesir=-1.0
C idesir, jdesir, kdesir - the desired u-cell indexes (position). 
c      idesir=7 
c      jdesir=4
c      kdesir=4
c      i=idesir
c      j=jdesir
c      k=kdesir
C update all coeficients in the momentum_source cell: 
c                    AE(I,J,K)=0.0 
c                    AW(I,J,K)=0.0
c                    AN(I,J,K)=0.0 
c                    AS(I,J,K)=0.0
c                    AH(I,J,K)=0.0 
c                    AL(I,J,K)=0.0
C SP and AP should not be changed. 
c    SU(I,J,K)=udesir*AP(I,J,K) 
C go and do the same for next momentum source 

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(3,2,2,NI,NJ,NK,Uf,NSWPU)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(3,2,2,NI,NJ,NK,Uf,NSWPU)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(3,2,2,NI,NJ,NK,Uf,NSWPU)
      elseif (ILISOL.eq.4) then
        call lisolv4(3,2,2,NIM1,NJM1,NKM1,Uf,NSWPU)
      endif

      RETURN
      END


C ********************* CALCV *********************
C Calculate coefficients of the matrix of the momentum 
C conservation equation in y direction and solve the matrix 
C to obtain velocity component in y direction.

      SUBROUTINE CALCV
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/VVEL/RESORV,NSWPV,URFV,FSDTV,DYNPV(ntcely),DYPSV(ntcely),
     1            SNSV(ntcely)
      COMMON/DUDVDW/DU(ntcelx,ntcely,ntcelz),DV(ntcelx,ntcely,ntcelz),
     1              DW(ntcelx,ntcely,ntcelz)
      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)  
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
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/CONST/GREAT,small,GRAV
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME 
      common/SOLVER/ILISOL

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,IZEROT,IZanKE
      LOGICAL IMITZ
      LOGICAL OK,LINVOL

      double precision RESOR, SORVOL, resorfn(MRFN)

      DO 100 I=2,NIM1
        DO 1001 J=3,NJM1
          DO 1002 K=2,NKM1

C Compute areas and volume.
            AREANS=AreaNSP(I,K)
            AREAEW=AreaEWV(J,K)
            AREAHL=AreaHLV(I,J)
            VOL=VolV(I,J,K)

C Calculate convection coefficients.
            GP=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))*Vf(I,J,K)
            GN=(SIFN(J)*DENf(I,J,K)+SIFS(J+1)*DENf(I,J+1,K))*Vf(I,J+1,K)
            GS=(SIFN(J-2)*DENf(I,J-2,K)+SIFS(J-1)*DENf(I,J-1,K))
     &        *Vf(I,J-1,K)
            GE=(SIFE(I)*DENf(I,J,K)+SIFW(I+1)*DENf(I+1,J,K))*Uf(I+1,J,K)
            GES=(SIFE(I)*DENf(I,J-1,K)+SIFW(I+1)*DENf(I+1,J-1,K))
     &         *Uf(I+1,J-1,K)
            GW=(SIFE(I-1)*DENf(I-1,J,K)+SIFW(I)*DENf(I,J,K))*Uf(I,J,K)
            GWS=(SIFE(I-1)*DENf(I-1,J-1,K)+SIFW(I)*DENf(I,J-1,K))
     &         *Uf(I,J-1,K)
            GH=(SIFH(K)*DENf(I,J,K)+SIFL(K+1)*DENf(I,J,K+1))*Wf(I,J,K+1)
            GHS=(SIFH(K)*DENf(I,J-1,K)+SIFL(K+1)*DENf(I,J-1,K+1))
     &         *Wf(I,J-1,K+1)
            GL=(SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K))*Wf(I,J,K)
            GLS=(SIFH(K-1)*DENf(I,J-1,K-1)+SIFL(K)*DENf(I,J-1,K))
     &         *Wf(I,J-1,K)

            CN=0.5*(GN+GP)*AREANS
            CS=0.5*(GP+GS)*AREANS
            CE=(SIFN(J-1)*GES+SIFS(J)*GE)*AREAEW
            CW=(SIFN(J-1)*GWS+SIFS(J)*GW)*AREAEW
            CH=(SIFN(J-1)*GHS+SIFS(J)*GH)*AREAHL
            CL=(SIFN(J-1)*GLS+SIFS(J)*GL)*AREAHL

C Calculate diffusion coefficients.
            VISE=(SIFW(I+1)*VIS(I+1,J,K)+SIFE(I)*VIS(I,J,K))*SIFS(J)
     &        +(SIFW(I+1)*VIS(I+1,J-1,K)+SIFE(I)*VIS(I,J-1,K))*SIFN(J-1)
            VISW=(SIFW(I)*VIS(I,J,K)+SIFE(I-1)*VIS(I-1,J,K))*SIFS(J)
     &        +(SIFW(I)*VIS(I,J-1,K)+SIFE(I-1)*VIS(I-1,J-1,K))*SIFN(J-1)
            VISH=(SIFL(K+1)*VIS(I,J,K+1)+SIFH(K)*VIS(I,J,K))*SIFS(J)
     &        +(SIFL(K+1)*VIS(I,J-1,K+1)+SIFH(K)*VIS(I,J-1,K))*SIFN(J-1)
            VISL=(SIFL(K)*VIS(I,J,K)+SIFH(K-1)*VIS(I,J,K-1))*SIFS(J)
     &        +(SIFL(K)*VIS(I,J-1,K)+SIFH(K-1)*VIS(I,J-1,K-1))*SIFN(J-1)

            DFN=VIS(I,J,K)*AREANS/DYNPV(J)
            DFS=VIS(I,J-1,K)*AREANS/DYPSV(J)
            DFE=VISE*AREAEW/DXEP(I)
            DFW=VISW*AREAEW/DXPW(I)
            DFH=VISH*AREAHL/DZHP(K)
            DFL=VISL*AREAHL/DZPL(K)

C Main coefficients for:
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

            DV(I,J,K)=AREANS

C Dynamic term.
            AP0=(SIFN(J-1)*DENp(I,J-1,K)+SIFS(J)*DENp(I,J,K))*VOL/DT
            APF=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C False dynamic term.
            APfals=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))*
     &             VOL/fsDTV

C Source coefficients.
            SU(I,J,K)=AP0*Vp(i,j,K)+DV(I,J,K)*(P(I,J-1,K)-P(I,J,K))+
     &                APfals*Vf(i,j,K)+CP*Vf(i,j,K)
            SP(I,J,K)=-AP0-APfals-CP

            GAMes=VISE
            DUDYes =(Uf(i+1,j,K)-Uf(i+1,j-1,K))/DYPS(J)
            GAMws=VISW
            DUDYWS =(Uf(I,J,K)-Uf(i,j-1,K))/DYPS(J)
            SU(I,J,K)=SU(I,J,K)+(GAMes*DUDYes-GAMws*DUDYws)*AREAEW

            DVDYP  =(Vf(i,j+1,K)-Vf(i,j,K))/SNS(J)
            DVDYS  =(Vf(i,j,K)-Vf(i,j-1,K))/SNS(J-1)
            SU(I,J,K) =SU(I,J,K)+
     &               (VIS(I,J,K)*DVDYP-VIS(I,J-1,K)*DVDYS)*AREANS

            GAMhs=VISH
            DWDYhs =(Wf(i,j,K+1)-Wf(i,j-1,K+1))/DYPS(J)
            GAMls=VISL
            DWDYls =(Wf(i,j,K)-Wf(i,j-1,K))/DYPS(J)
            SU(I,J,K)=SU(I,J,K)+(GAMhs*DWDYhs-GAMls*DWDYls)*AREAHL
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Apply boundary conditions.
      IF(INCALK)THEN

C Log-law or Yuan wall functions.
        CALL MODVT

C Check for blockages.
        if(NBLK(ICFD).gt.0) then 
          do 200 iblk=1,NBLK(ICFD)
            iv=INBLK(iblk,ICFD)
            if (BCOFF(iv)) cycle
            call BLKBNDV(iv)
 200      continue
        endif
      ELSEIF(IMITZ)THEN

C MIT zero-equation model: no slip condition.
        CALL MODVMIT
      ELSE

C Laminar.
        CALL MODVL
      ENDIF

C Remaining coefficients and residual source calculation.
      RESORV=0.0
      RENORMV=0.0
      SAnb=0.
      U=0.0
      V=0.0
      W=0.0

      DO 300 I=2,NIM1
        DO 3001 J=3,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+AH(I,J,K)+
     &                AL(I,J,K)-SP(I,J,K)
            SAnb=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+AH(I,J,K)+
     &                AL(I,J,K)
            RESOR=AE(I,J,K)*Vf(i+1,j,K)+AW(I,J,K)*Vf(i-1,j,K)+
     &            AN(I,J,K)*Vf(i,j+1,K)+AS(I,J,K)*Vf(i,j-1,K)+
     &            AH(I,J,K)*Vf(i,j,K+1)+AL(I,J,K)*Vf(i,j,K-1)-
     &            AP(I,J,K)*Vf(i,j,K)+SU(I,J,K)

C Check for small supply openings and set resor to zero. This is 
C done for the next to opening cell (into domain) as it is this 
C cell that has the corrected momentum.
            if(NSSO(ICFD).gt.0) then 
               do 2006 IVO=1,NSSO(ICFD)
                  IV=INSSO(IVO,ICFD)
                  OK=.false.
                  if (IVOLF(IV,ICFD).eq.3) then
                    OK=LINVOL(I,J-1,K,
     &                    IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                    JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                    KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  elseif (IVOLF(IV,ICFD).eq.4) then
                    OK=LINVOL(I,J,K,
     &                    IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                    JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                    KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  endif
                  if (OK) resor=0.0
 2006          continue
            endif

C Check for blockages. 
            if(NBLK(ICFD).gt.0) then

C If inside the blockage or blockage to the south then set resor to zero.
              do 2007 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD)  
                if (BCOFF(iv)) cycle
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),(JVCELLS(IV,ICFD,2)+1),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if (OK) resor=0.0
 2007         continue
            endif
 
            IF(INCALU) U=SIFN(J-1)*(Uf(I,J-1,K)+Uf(I+1,J-1,K))/2.+
     &         SIFS(J)*(Uf(I,J,K)+Uf(I+1,J,K))/2.
            V=Vf(I,J,K)
            IF(INCALW) W=SIFN(J-1)*(Wf(I,J-1,K)+Wf(I,J-1,K+1))/2.+
     &         SIFS(J)*(Wf(I,J,K)+Wf(I,J,K+1))/2.
            RENORM=AP(I,J,K)*SQRT(U*U + V*V + W*W)

            SORVOL=GREAT*VolV(I,J,K)
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=real(RENORM/SORVOL)
            ENDIF
            RESORV=real(RESORV+ABS(RESOR))
            RENORMV=RENORMV+ABS(RENORM)

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.3) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                elseif (ABS(RESOR).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                endif
              enddo
            endif

C Linear under-relaxation.
            AP(I,J,K)=AP(I,J,K)/URFV
            SU(I,J,K)=SU(I,J,K)+(1.-URFV)*AP(I,J,K)*Vf(i,j,K)

C SIMPLE scheme.
C            DV(I,J,K)=DV(I,J,K)/AP(I,J,K)

C SIMPLEC scheme.
            DV(I,J,K)=DV(I,J,K)/anotzero(AP(I,J,K)-SAnb)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Check for small supply openings 
      if(NSSO(ICFD).gt.0) then

C Loop over opening and adjust momentum in small openings.
         do 2000 IVO=1,NSSO(ICFD)
            IV=INSSO(IVO,ICFD) 
            if (JVCELLS(IV,ICFD,1).eq.JVCELLS(IV,ICFD,2)) then
              J=JVCELLS(IV,ICFD,1)
              do 509 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
                do 509 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)

C Set Ja=J of adjacent cell (depends on if south(=3) or north(=4) boundary).
                  OK=.false.
                  if (IVOLF(IV,ICFD).eq.3) then
                    Ja=J+1
                    Jb=J
                    OK=.true.
                  elseif (IVOLF(IV,ICFD).eq.4) then
                    Ja=J
                    Jb=J+1
                    OK=.true.
                  endif
                  if (OK) then
                    AE(I,Ja,K)=0.0; AW(I,Ja,K)=0.0
                    AN(I,Ja,K)=0.0; AS(I,Ja,K)=0.0
                    AH(I,Ja,K)=0.0; AL(I,Ja,K)=0.0

C AP does not change.  Calculate the value for 
C Vf which results in the true momentum and put it in SU. 
                    SU(I,Ja,K)=AP(I,Ja,K)*Vf(I,Jb,K)*
     &                               SQRT(1./VOLArea(IV,ICFD))
                  endif
 509          continue    
            endif
 2000    continue
      endif

C Check for blockages.
C Loop through whole blockage,set V_velocity to zero 
      if(NBLK(ICFD).gt.0) then 
        do 2002 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD) 
          if (BCOFF(iv)) cycle
          JST=max(3,JVCELLS(IV,ICFD,1))
          do 2001 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 2001 J=JST,JVCELLS(IV,ICFD,2)
              do 2001 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
C                SU(I,J,K)=0.0
C                AP(I,J,K)=GREAT
C                SU(I,J+1,K)=0.0
C                AP(I,J+1,K)=GREAT

                AE(I,J,K)=0.0; AW(I,J,K)=0.0; AN(I,J,K)=0.0
                AS(I,J,K)=0.0; AH(I,J,K)=0.0; AL(I,J,K)=0.0
                SP(I,J,K)=0.0; SU(I,J,K)=0.0; AP(I,J,K)=1.0

                AE(I,J+1,K)=0.0; AW(I,J+1,K)=0.0
                AN(I,J+1,K)=0.0; AS(I,J+1,K)=0.0
                AH(I,J+1,K)=0.0; AL(I,J+1,K)=0.0
                SP(I,J+1,K)=0.0; SU(I,J+1,K)=0.0
                AP(I,J+1,K)=1.0

 2001     continue
 2002   continue
      endif

C Example of calculation of v-momentum-sources /see u-momentum also/ 
C << Keep current comments >>
C vdesir - the desired v-velocity to be achieved (user-supplied), [m/s].  
c      vdesir=-1.0   
C idesir, jdesir, kdesir - the desired w-cell indexes (position). 
c      idesir=7 
c      jdesir=4
c      kdesir=4
c      i=idesir
c      j=jdesir
c      k=kdesir
C update all coeficients in the momentum_source cell: 
c                    AE(I,J,K)=0.0 
c                    AW(I,J,K)=0.0
c                    AN(I,J,K)=0.0 
c                    AS(I,J,K)=0.0
c                    AH(I,J,K)=0.0 
c                    AL(I,J,K)=0.0
C SP and AP should not be changed. 
c    SU(I,J,K)=vdesir*AP(I,J,K) 
C go and do the same for next momentum source 
C
C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,3,2,NI,NJ,NK,Vf,NSWPV)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,3,2,NI,NJ,NK,Vf,NSWPV)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,3,2,NI,NJ,NK,Vf,NSWPV)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,3,2,NIM1,NJM1,NKM1,Vf,NSWPV)
      endif

      RETURN
      END


C ********************* CALCW *********************
C Calculate coefficients of the matrix of the momentum 
C conservation equation in z direction and solve the matrix 
C to obtain velocity component in z direction.

      SUBROUTINE CALCW 
#include "building.h"
#include "cfd.h"

      COMMON/PCOR/RESORM,NSWPP,URFP,FSDTP,IPREF,JPREF,KPREF
      COMMON/ICFNOD/ICFD,ICP
      COMMON/WVEL/RESORW,NSWPW,URFW,FSDTW,DZHPW(ntcelz),DZPLW(ntcelz),
     1            SHLW(ntcelz)
      COMMON/DUDVDW/DU(ntcelx,ntcely,ntcelz),DV(ntcelx,ntcely,ntcelz),
     1              DW(ntcelx,ntcely,ntcelz)
      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)  
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
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/CONST/GREAT,small,GRAV
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/SOLVER/ILISOL

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,IZEROT,IZanKE
      LOGICAL IMITZ
      LOGICAL OK,LINVOL

      double precision RESOR, SORVOL, resorfn(MRFN)

      DO 100 I=2,NIM1
        DO 1001 J=2,NJM1
          DO 1002 K=3,NKM1

C Compute areas and volume.
            AREANS=AreaNSW(I,K)
            AREAEW=AreaEWW(J,K)
            AREAHL=AreaHLP(I,J)
            VOL=VolW(I,J,K)

C Calculate convection coefficients.
            GP=(SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K))*Wf(I,J,K)
            GH=(SIFH(K)*DENf(I,J,K)+SIFL(K+1)*DENf(I,J,K+1))*Wf(I,J,K+1)
            GL=(SIFH(K-2)*DENf(I,J,K-2)+SIFL(K-1)*DENf(I,J,K-1))
     &        *Wf(I,J,K-1)
            GN=(SIFN(J)*DENf(I,J,K)+SIFS(J+1)*DENf(I,J+1,K))*Vf(I,J+1,K)
            GNL=(SIFN(J)*DENf(I,J,K-1)+SIFS(J+1)*DENf(I,J+1,K-1))
     &         *Vf(I,J+1,K-1)
            GS=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))*Vf(I,J,K)
            GSL=(SIFN(J-1)*DENf(I,J-1,K-1)+SIFS(J)*DENf(I,J,K-1))
     &         *Vf(I,J,K-1)
            GE=(SIFE(I)*DENf(I,J,K)+SIFW(I+1)*DENf(I+1,J,K))*Uf(I+1,J,K)
            GEL=(SIFE(I)*DENf(I,J,K-1)+SIFW(I+1)*DENf(I+1,J,K-1))
     &         *Uf(I+1,J,K-1)
            GW=(SIFE(I-1)*DENf(I-1,J,K)+SIFW(I)*DENf(I,J,K))*Uf(I,J,K)
            GWL=(SIFE(I-1)*DENf(I-1,J,K-1)+SIFW(I)*DENf(I,J,K-1))
     &         *Uf(I,J,K-1)

            CN=(SIFH(K-1)*GNL+SIFL(K)*GN)*AREANS
            CS=(SIFH(K-1)*GSL+SIFL(K)*GS)*AREANS
            CE=(SIFH(K-1)*GEL+SIFL(K)*GE)*AREAEW
            CW=(SIFH(K-1)*GWL+SIFL(K)*GW)*AREAEW
            CH=0.5*(GH+GP)*AREAHL
            CL=0.5*(GP+GL)*AREAHL

C Calculate diffusion coefficients.
            VISLE=(SIFW(I+1)*VIS(I+1,J,K)+SIFE(I)*VIS(I,J,K))*SIFL(K)
     &        +(SIFW(I+1)*VIS(I+1,J,K-1)+SIFE(I)*VIS(I,J,K-1))*SIFH(K-1)
            VISLW=(SIFW(I)*VIS(I,J,K)+SIFE(I-1)*VIS(I-1,J,K))*SIFL(K)
     &        +(SIFW(I)*VIS(I,J,K-1)+SIFE(I-1)*VIS(I-1,J,K-1))*SIFH(K-1)
            VISLN=(SIFS(J+1)*VIS(I,J+1,K)+SIFN(J)*VIS(I,J,K))*SIFL(K)
     &        +(SIFS(J+1)*VIS(I,J+1,K-1)+SIFN(J)*VIS(I,J,K-1))*SIFH(K-1)
            VISLS=(SIFS(J)*VIS(I,J,K)+SIFN(J-1)*VIS(I,J-1,K))*SIFL(K)
     &        +(SIFS(J)*VIS(I,J,K-1)+SIFN(J-1)*VIS(I,J-1,K-1))*SIFH(K-1)

            DFN=VISLN*AREANS/DYNP(J)
            DFS=VISLS*AREANS/DYPS(J)
            DFE=VISLE*AREAEW/DXEP(I)
            DFW=VISLW*AREAEW/DXPW(I)
            DFH=VIS(I,J,K)*AREAHL/DZHPW(K)
            DFL=VIS(I,J,K-1)*AREAHL/DZPLW(K)

C Main coefficients for:
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

            DW(I,J,K)=AREAHL

C Dynamic term.
            AP0=(SIFH(K-1)*DENp(I,J,K-1)+SIFL(K)*DENp(I,J,K))*VOL/DT
            APF=(SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K))*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C False dynamic term.
            APfals=(SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K))*
     &              VOL/fsDTW

C Source coefficients.
            SU(I,J,K)=AP0*Wp(i,j,K)+DW(I,J,K)*(P(I,J,K-1)-P(I,J,K))+
     &                APfals*Wf(i,j,K)+CP*Wf(i,j,K)

C Buoyancy effect.
            IF(BUOYA) THEN
              IF(BOUSSA) THEN

C Apply the Boussinesq approximation.  Only if the temperature difference
C is significant (>=0.00001degC) modify the source term.
                Tmod=SIFL(K)*Tf(i,j,K)+SIFH(K-1)*Tf(i,j,K-1)-TBAR(ICFD)
                if (abs(Tmod).gt.0.00001) then
                  SU(I,J,K)=SU(i,j,K)+
     &                         GRAV*BETA(i,j,K)*VOL*DENf(i,j,K)*Tmod
                endif
              ELSE

C Don't apply the Boussinesq approximation. Only if the density difference
C is significant (>=1e-7kg/m^3) modify the source term.
                Dmod=SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K)-
     &                             AIRDEN(Tf(IPREF,JPREF,KPREF))
                if (abs(Dmod).gt.1e-7) then
                  SU(I,J,K)=SU(I,J,K)-GRAV*VOL*Dmod
                endif
              ENDIF
            ENDIF

C Source coefficients.
            SP(I,J,K)=-AP0-APfals-CP
            GAMle=VISLE
            DUDZle =(Uf(i+1,j,K)-Uf(i+1,j,K-1))/DZPL(K)
            GAMlw=VISLW
            DUDZlw =(Uf(I,J,K)-Uf(i,j,K-1))/DZPL(K)
            SU(I,J,K)=SU(I,J,K)+(GAMle*DUDZle-GAMlw*DUDZlw)*AREAEW

            GAMln=VISLN
            DVDZln  =(Vf(i,j+1,K)-Vf(i,j+1,K-1))/DZPL(K)
            GAMls=VISLS
            DVDZls  =(Vf(i,j,K)-Vf(i,j,K-1))/DZPL(K)
            SU(I,J,K) =SU(I,J,K)+(GAMln*DVDZln-GAMls*DVDZls)*AREANS

            GAMP  =VIS(I,J,K)
            DWDZP =(Wf(i,j,K+1)-Wf(i,j,K))/SHL(K)
            GAML  =VIS(I,J,K-1)
            DWDZL =(Wf(i,j,K)-Wf(i,j,K-1))/SHL(K-1)
            SU(I,J,K)=SU(I,J,K)+(GAMP*DWDZP-GAML*DWDZL)*AREAHL

 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE
 
C Apply boundary conditions.
      IF(INCALK)THEN

C Log-law or Yuan wall functions.
        CALL MODWT

C Check for blockages.
        if(NBLK(ICFD).gt.0) then 
          do 200 iblk=1,NBLK(ICFD)
            iv=INBLK(iblk,ICFD)
            if (BCOFF(iv)) cycle
            call BLKBNDW(iv)
 200      continue
        endif
      ELSEIF(IMITZ)THEN

C MIT zero-equation model: no slip condition.
        CALL MODWMIT
      ELSE

C Laminar.
        CALL MODWL
      ENDIF

C Remaining coefficients and residual source calculation.
      RESORW=0.0
      RENORMW=0.0
      SAnb=0.0
      U=0.0
      V=0.0
      W=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=3,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+AH(I,J,K)+
     &                AL(I,J,K)-SP(I,J,K)
            SAnb=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+AH(I,J,K)+
     &                AL(I,J,K)

            RESOR=AE(I,J,K)*Wf(i+1,j,K)+AW(I,J,K)*Wf(i-1,j,K)+
     &            AN(I,J,K)*Wf(i,j+1,K)+AS(I,J,K)*Wf(i,j-1,K)+
     &            AH(I,J,K)*Wf(i,j,K+1)+AL(I,J,K)*Wf(i,j,K-1)-
     &            AP(I,J,K)*Wf(i,j,K)+SU(I,J,K)

C Check for small supply openings and set resor to zero.  This is 
C done for the next to opening cell (into domain) as it is this 
C cell that has the corrected momentum.
            if(NSSO(ICFD).gt.0) then 
               do 2006 IVO=1,NSSO(ICFD)
                  IV=INSSO(IVO,ICFD)
                  OK=.false.
                  if (IVOLF(IV,ICFD).eq.5) then
                  
C The SSO is on the low wall 
                    OK=LINVOL(I,J,K-1,
     &                    IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                    JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                    KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  elseif (IVOLF(IV,ICFD).eq.6) then
                  
C The SSO is on the high wall 
                    OK=LINVOL(I,J,K,
     &                    IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                    JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                    KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  endif
                  if (OK) resor=0.0
 2006          continue
            endif

C Check for blockages. 
            if(NBLK(ICFD).gt.0) then
    
C If inside the blockage or blockage to the low then set resor to zero.
              do 2007 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD)  
                if (BCOFF(iv)) cycle
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),(KVCELLS(IV,ICFD,2)+1))
                if (OK) resor=0.0
 2007         continue
            endif
            IF(INCALU) U=SIFH(K-1)*(Uf(I,J,K-1)+Uf(I+1,J,K-1))/2.+
     &        SIFL(K)*(Uf(I,J,K)+Uf(I+1,J,K))/2.
            IF(INCALV) V=SIFH(K-1)*(Vf(I,J,K-1)+Vf(I,J+1,K-1))/2.+
     &        SIFL(K)*(Vf(I,J,K)+Vf(I,J+1,K))/2.
            W=Wf(I,J,K)
            RENORM=AP(I,J,K)*SQRT(U*U + V*V + W*W)

            SORVOL=GREAT*VolW(I,J,K)
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=real(RENORM/SORVOL)
            ENDIF
            RESORW=real(RESORW+ABS(RESOR))
            RENORMW=RENORMW+ABS(RENORM)

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.4) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                elseif (ABS(RESOR).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                endif
              enddo
            endif

C Linear under-relaxation.
            AP(I,J,K)=AP(I,J,K)/URFW
            SU(I,J,K)=SU(I,J,K)+(1.-URFW)*AP(I,J,K)*Wf(i,j,K)

C SIMPLE scheme.
C            DW(I,J,K)=DW(I,J,K)/AP(I,J,K)
C SIMPLEC scheme.
            DW(I,J,K)=DW(I,J,K)/anotzero(AP(I,J,K)-SAnb)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Check for small supply openings 
      if(NSSO(ICFD).gt.0) then

C Loop over opening and adjust momentum in small openings.
         do 2000 IVO=1,NSSO(ICFD)
            IV=INSSO(IVO,ICFD) 
            if (KVCELLS(IV,ICFD,1).eq.KVCELLS(IV,ICFD,2)) then
              K=KVCELLS(IV,ICFD,1)
              do 509 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                do 509 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)

C Set Ka=K of adjacent cell (depends on if low(5) or high(6) boundary).
                  OK=.false.
                  if (IVOLF(IV,ICFD).eq.5) then
                    Ka=K+1
                    Kb=K
                    OK=.true.
                  elseif (IVOLF(IV,ICFD).eq.6) then
                    Ka=K
                    Kb=K+1
                    OK=.true.
                  endif
                  if (OK) then
                    AE(I,J,Ka)=0.0; AW(I,J,Ka)=0.0
                    AN(I,J,Ka)=0.0; AS(I,J,Ka)=0.0
                    AH(I,J,Ka)=0.0; AL(I,J,Ka)=0.0

C AP does not change. Calculate the value for Wf that
C results in the true momentum and put it in SU. 
                    SU(I,J,Ka)=AP(I,J,Ka)*Wf(I,J,Kb)/
     &                                       SQRT(VOLArea(IV,ICFD)) 

                  endif
 509          continue    
            endif
 2000    continue
      endif

C Check for blockages.
      if(NBLK(ICFD).gt.0) then

C Loop through whole blockage,set W_velocity to zero 
        do 2002 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD) 
          if (BCOFF(iv)) cycle
          KST=max(3,KVCELLS(IV,ICFD,1))
          do 2001 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 2001 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 2001 K=KST,KVCELLS(IV,ICFD,2)             
C               SU(I,J,K)=0.0
C               AP(I,J,K)=GREAT
C               SU(I,J,K+1)=0.0
C               AP(I,J,K+1)=GREAT

                AE(I,J,K)=0.0; AW(I,J,K)=0.0
                AN(I,J,K)=0.0; AS(I,J,K)=0.0
                AH(I,J,K)=0.0; AL(I,J,K)=0.0
                SP(I,J,K)=0.0; SU(I,J,K)=0.0
                AP(I,J,K)=1.0

                AE(I,J,K+1)=0.0; AW(I,J,K+1)=0.0
                AN(I,J,K+1)=0.0; AS(I,J,K+1)=0.0
                AH(I,J,K+1)=0.0; AL(I,J,K+1)=0.0
                SP(I,J,K+1)=0.0; SU(I,J,K+1)=0.0
                AP(I,J,K+1)=1.0
 2001     continue
 2002   continue
      endif

C Example of calculation of w-momentum-sources /see u-momentum also/
C wdesir - the desired w-velocity to be achieved (user-supplied) [m/s].  
C      wdesir=0.4 
C idesir, jdesir, kdesir - the desired w-cell indexes (position). 
C      idesir=7 
C      jdesir=4
C      kdesir=5
C      i=idesir
C      j=jdesir
C      k=kdesir
C update all coeficients in the momentum_source cell.
C                AE(I,J,K)=0.0 
C                AW(I,J,K)=0.0
C                AN(I,J,K)=0.0 
C                AS(I,J,K)=0.0
C                AH(I,J,K)=0.0 
C                AL(I,J,K)=0.0
C SP and AP should not be changed. 
C     SU(I,J,K)=wdesir*AP(I,J,K) 
C Do the same for next momentum source.

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,3,NI,NJ,NK,Wf,NSWPW)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,3,NI,NJ,NK,Wf,NSWPW)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,3,NI,NJ,NK,Wf,NSWPW)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,3,NIM1,NJM1,NKM1,Wf,NSWPW)
      endif

      RETURN
      END


C ********************* CALCP *********************
C Calculate coefficients of the matrix of the mass conservation 
C equation and solve the matrix to obtain pressure corrections.

      SUBROUTINE CALCP
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/PCOR/RESORM,NSWPP,URFP,FSDTP,IPREF,JPREF,KPREF
      COMMON/DUDVDW/DU(ntcelx,ntcely,ntcelz),DV(ntcelx,ntcely,ntcelz),
     &              DW(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     &            Wf(ntcelx,ntcely,ntcelz),
     &            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     &            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
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
     &            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     &            BETA(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     &            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     &            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     &            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     &            SP(ntcelx,ntcely,ntcelz)
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz) 
      common/SOLVER/ILISOL

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      COMMON/CONST/GREAT,small,GRAV
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN

      LOGICAL OK,LINVOL,BLOCKS
      real resorfn(MRFN)

      RESORM=0.0
      X1=0.0
      X2=0.0
      X3=0.0
      DO 100 I=2,NIM1
        DO 1001 J=2,NJM1
          DO 1002 K=2,NKM1

C Compute areas and volume.
            AREAEW=AreaEWP(J,K)
            AREANS=AreaNSP(I,K)
            AREAHL=AreaHLP(I,J)
            VOL=VolP(I,J,K)
 
C Calculate coefficients.
            DENE=(SIFE(I)*DENf(I,J,K)+SIFW(I+1)*DENf(I+1,J,K))
            DENW=(SIFE(I-1)*DENf(I-1,J,K)+SIFW(I)*DENf(I,J,K))
            DENN=(SIFN(J)*DENf(I,J,K)+SIFS(J+1)*DENf(I,J+1,K))
            DENS=(SIFN(J-1)*DENf(I,J-1,K)+SIFS(J)*DENf(I,J,K))
            DENH=(SIFH(K)*DENf(I,J,K)+SIFL(K+1)*DENf(I,J,K+1))
            DENL=(SIFH(K-1)*DENf(I,J,K-1)+SIFL(K)*DENf(I,J,K))
            AE(I,J,K)=DENE*AREAEW*DU(I+1,J,K)
            AW(I,J,K)=DENW*AREAEW*DU(I,J,K)
            AN(I,J,K)=DENN*AREANS*DV(I,J+1,K)
            AS(I,J,K)=DENS*AREANS*DV(I,J,K)
            AH(I,J,K)=DENH*AREAHL*DW(I,J,K+1)
            AL(I,J,K)=DENL*AREAHL*DW(I,J,K)

C Calculate source terms.
            CE=DENE*Uf(i+1,j,K)*AREAEW
            CW=DENW*Uf(I,J,K)*AREAEW
            CN=DENN*Vf(i,j+1,K)*AREANS
            CS=DENS*Vf(i,j,K)*AREANS
            CH=DENH*Wf(i,j,K+1)*AREAHL
            CL=DENL*Wf(I,J,K)*AREAHL

C Next part of algorithm works well but is inefficient. 
C <<all testings should be replaced with an array of integers>>
C
C Check for blockages and cells adjacent to blockages. 
C In case of blockages, all convective fluxes and coefficients
C are set to zero.
            if(NBLK(ICFD).gt.0) then 
              do 2006 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD) 
                if (BCOFF(iv)) cycle
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if(OK) then 

C Cell is within the blockage.      
                  AE(I,J,K)=0.0; AW(I,J,K)=0.0
                  AN(I,J,K)=0.0; AS(I,J,K)=0.0
                  AH(I,J,K)=0.0; AL(I,J,K)=0.0
                  CE=0.0; CW=0.0; CN=0.0
                  CS=0.0; CH=0.0; CL=0.0
                else 

C Cell is outside blockage. 
C Check cells to side of current cell if not blocked. 
C If blocked, set coefficients and fluxes toward blockages to zero. 
                  OK=LINVOL(I+1,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if(OK) then  
                    AE(I,J,K)=0.0
                    CE=0.0  
                  endif
                  OK=LINVOL(I-1,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if(OK) then 
                    AW(I,J,K)=0.0
                    CW=0.0              
                  endif
                  OK=LINVOL(I,J+1,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if(OK) then  
                    AN(I,J,K)=0.0
                    CN=0.0
                  endif
                  OK=LINVOL(I,J-1,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if(OK) then  
                    AS(I,J,K)=0.0
                    CS=0.0
                  endif
                  OK=LINVOL(I,J,K+1,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if(OK) then  
                    AH(I,J,K)=0.0
                    CH=0.0
                  endif
                  OK=LINVOL(I,J,K-1,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if(OK) then  
                    AL(I,J,K)=0.0
                    CL=0.0
                  endif
                endif
 2006         continue
            endif 
            SMP=CE-CW+CN-CS+CH-CL
            if (ABS(SMP).lt.small) SMP=0.

C Dynamic source term.
            SDT=(DENf(I,J,K)-DENp(I,J,K))*VOL/DT
            if (ABS(SDT).lt.small) SDT=0.

            SP(I,J,K)=0.0
            SU(I,J,K)=-SMP-SDT

C Compute sum of absolute mass sources.
            RESORM=RESORM+ABS(SU(I,J,K))

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.1) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(SU(I,J,K))
                  exit
                elseif (ABS(SU(I,J,K)).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(SU(I,J,K))
                  exit
                endif
              enddo
            endif
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

      RESORM=RECRES(RESORM)

C Modify problem.
      CALL MODPP1

C Remaining coefficients and residual source calculation.
      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &                                AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,2,NI,NJ,NK,PP,NSWPP)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,PP,NSWPP)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,PP,NSWPP)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,PP,NSWPP)
      endif

C Check for blockages. 
      BLOCKS=.false.
      if(NBLK(ICFD).gt.0) BLOCKS=.true.

C Correct velocities.
      DO 500 I=2,NIM1
        DO 5001 J=2,NJM1
          DO 5002 K=2,NKM1
            if (BLOCKS) then

C Check for blockages and exclude them from calculation.
              if (I.gt.2) then

C Check scalar cells on both boundaries in U direction for blockages.
                IUcor=0
                do 5101 IVO=1,NBLK(ICFD)
                  IV=INBLK(IVO,ICFD) 
                  if (BCOFF(iv)) cycle
                  OK=LINVOL(I,J,K,
C     &                    (IVCELLS(IV,ICFD,1)+1),(IVCELLS(IV,ICFD,2)+1),
     &                     IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                     JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                     KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if (OK) IUcor=IUcor+1 
                  OK=LINVOL(I-1,J,K,
C     &                    (IVCELLS(IV,ICFD,1)+1),(IVCELLS(IV,ICFD,2)+1),
     &                     IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                     JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                     KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if (OK) IUcor=IUcor+1
 5101           continue

C If both bounding scalar cells unblocked, correct velocity.
                if (IUcor.eq.0) Uf(I,J,K)=Uf(I,J,K)+
     &                        DU(I,J,K)*(PP(I-1,J,K)-PP(I,J,K))
              endif
              if (J.gt.2) then

C Check scalar cells on both boundaries in V direction for blockages.
                IVcor=0
                do 5102 IVO=1,NBLK(ICFD)
                  IV=INBLK(IVO,ICFD) 
                  if (BCOFF(iv)) cycle
                  OK=LINVOL(I,J,K,
     &                     IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
C     &                    (JVCELLS(IV,ICFD,1)+1),(JVCELLS(IV,ICFD,2)+1),
     &                     JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                     KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if (OK) IVcor=IVcor+1
                  OK=LINVOL(I,J-1,K,
     &                     IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
C     &                    (JVCELLS(IV,ICFD,1)+1),(JVCELLS(IV,ICFD,2)+1),
     &                     JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                     KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if (OK) IVcor=IVcor+1
 5102           continue

C If both bounding scalar cells unblocked correct velocity.
                if (IVcor.eq.0) Vf(i,j,K)=Vf(i,j,K)+
     &                        DV(I,J,K)*(PP(I,J-1,K)-PP(I,J,K))
              endif
              if (K.gt.2) then

C Check scalar cells on both boundaries in W direction for blockages.
                IWcor=0
                do 5103 IVO=1,NBLK(ICFD)
                  IV=INBLK(IVO,ICFD) 
                  if (BCOFF(iv)) cycle
                  OK=LINVOL(I,J,K,
     &                     IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                     JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
c     &                    (KVCELLS(IV,ICFD,1)+1),(KVCELLS(IV,ICFD,2)+1))
     &                     KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if (OK) IWcor=IWcor+1
                  OK=LINVOL(I,J,K-1,
     &                     IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                     JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
c     &                    (KVCELLS(IV,ICFD,1)+1),(KVCELLS(IV,ICFD,2)+1))
     &                     KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                  if (OK) IWcor=IWcor+1
 5103           continue

C If both bounding scalar cells unblocked correct velocity.
                if (IWcor.eq.0) Wf(i,j,K)=Wf(i,j,K)+
     &                        DW(I,J,K)*(PP(I,J,K-1)-PP(I,J,K))
              endif
            else

C No blockages.
              IF(I.gt.2)Uf(I,J,K)=Uf(I,J,K)+
     &                             DU(I,J,K)*(PP(I-1,J,K)-PP(I,J,K))
              IF(J.gt.2)Vf(i,j,K)=Vf(i,j,K)+
     &                             DV(I,J,K)*(PP(I,J-1,K)-PP(I,J,K))
              IF(K.gt.2)Wf(i,j,K)=Wf(i,j,K)+
     &                             DW(I,J,K)*(PP(I,J,K-1)-PP(I,J,K))
            endif
 5002     CONTINUE
 5001   CONTINUE
 500  CONTINUE

C Correct pressures (with provision for under-relaxation).
      PPREF=PP(IPREF,JPREF,KPREF)    ! reference volume pressure

      DO 600 I=2,NIM1
        DO 6001 J=2,NJM1
          DO 6002 K=2,NKM1
            if (BLOCKS) then

C Check for blockages and exclude them from calculation.
              IPcor=0
              do 5007 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD) 
                if (BCOFF(iv)) cycle

C If outside the blockage correct pressure.
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if (OK) IPcor=IPcor+1
 5007         continue
              if(IPcor.eq.0) then

C Pressure correction.
                P(I,J,K)=P(I,J,K)+URFP*(PP(I,J,K)-PPREF)
                PP(I,J,K)=0.0
              endif

C No blockages, correct pressure.
            else
              P(I,J,K)=P(I,J,K)+URFP*(PP(I,J,K)-PPREF)
              PP(I,J,K)=0.0
            endif
 6002     CONTINUE
 6001   CONTINUE
 600  CONTINUE

      RETURN
      END


C ********************* CALCT *********************
C Calculate the coefficients of the matrix of the energy 
C equation and solve the matrix to obtain temperatures.

      SUBROUTINE CALCT
#include "building.h"
#include "cfd.h"

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
      COMMON/TEMPp/Tp(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/CONST/GREAT,small,GRAV
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME 
      common/SOLVER/ILISOL
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,IZEROT,IZanKE
      LOGICAL IMITZ
      LOGICAL OK,LINVOL

      double precision RESOR, SORVOL, resorfn(MRFN)

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

C Dynamic term.
            AP0=DENp(I,J,K)*VOL/DT
            APF=DENf(I,J,K)*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C False dynamic term.
            APfals=DENf(i,j,k)*VOL/fsDTT

C Calculate diffusion coefficients.
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
            SU(I,J,K)=AP0*Tp(i,j,K)+APfals*Tf(i,j,k)+CP*Tf(i,j,k)
            SP(I,J,K)=-AP0-APfals-CP
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Apply boundary conditions.
      IF(INCALK.or.IMITZ)THEN

C Use one of the turbulent boundary condition treatments.
        CALL BNDARTT
      ELSE

C Use laminar boundary conditions.
        CALL MODTL
      ENDIF
 
C Check for blockages.
      if(NBLK(ICFD).gt.0) then 
        do IVO=1,NBLK(ICFD)
          iv=INBLK(IVO,ICFD)
          if (BCOFF(iv)) cycle
          call BLKBNDT(iv)
        enddo
      endif

C Remaining coefficients and residual source calculation.
      RESORT=0.0
      RENORMT=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &              AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            RESOR=AE(I,J,K)*Tf(i+1,j,K)+AW(I,J,K)*Tf(i-1,j,K)+
     &            AN(I,J,K)*Tf(i,j+1,K)+AS(I,J,K)*Tf(i,j-1,K)+
     &            AH(I,J,K)*Tf(i,j,K+1)+AL(I,J,K)*Tf(i,j,K-1)-
     &            AP(I,J,K)*Tf(i,j,K)+SU(I,J,K)
            RENORM=AP(I,J,K)*Tf(I,J,K)

C Check for blockages.
C If inside the blockage or to the west then set resor to zero.
            if(NBLK(ICFD).gt.0) then 
              do 2006 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD) 
                if (BCOFF(iv)) cycle
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if(OK) then
                  resor=0.0
                endif
 2006         continue
            endif

            SORVOL=GREAT*VolP(I,J,K)
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=real(RENORM/SORVOL)
            ENDIF
            RESORT=real(RESORT+ABS(RESOR))
            RENORMT=RENORMT+ABS(RENORM)

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.5) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                elseif (ABS(RESOR).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                endif
              enddo
            endif

C Under-relaxation.
            AP(I,J,K)=AP(I,J,K)/URFT
            SU(I,J,K)=SU(I,J,K)+(1.0-URFT)*AP(I,J,K)*Tf(i,j,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Check for blockages.
C Ensure blockages do not participate in the solution. 
      if(NBLK(ICFD).gt.0) then 
        do 2007 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)  
          if (BCOFF(iv)) cycle
          do 2017 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 2107 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 2207 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                AE(I,J,K)=0.0; AW(I,J,K)=0.0
                AN(I,J,K)=0.0; AS(I,J,K)=0.0
                AH(I,J,K)=0.0; AL(I,J,K)=0.0
                SP(I,J,K)=0.0
                SU(I,J,K)=TINIT(ICFD)
                AP(I,J,K)=1.0
 2207         continue
 2107       continue
 2017     continue
 2007   continue
      endif

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,2,NI,NJ,NK,Tf,NSWPT)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,Tf,NSWPT)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,Tf,NSWPT)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,Tf,NSWPT)
      endif

      RETURN
      END


C ********************* CALCTE *********************
C Calculate coefficients of the matrix of the turbulent 
C energy equation and solve the matrix.

      SUBROUTINE CALCTE
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/TEN/RESORK,NSWPK,URFK,FSDTK
      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     &            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     &            EDp(ntcelx,ntcely,ntcelz)  
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     &            Wf(ntcelx,ntcely,ntcelz),
     &            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     &            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     &            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     &            DZHP(ntcelz),DZPL(ntcelz),
     &            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     &            XU(ntcelx),YV(ntcely),ZW(ntcelz)
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
     &            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     &            BETA(ntcelx,ntcely,ntcelz)                      
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     &            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     &            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     &            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     &            SP(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     &             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      COMMON/CONST/GREAT,small,GRAV
      COMMON/SUSP/SUKD(ntcelx,ntcely,ntcelz),SPKD(ntcelx,ntcely,ntcelz)
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME 
      common/SOLVER/ILISOL
      COMMON/GGDH/ GENB1(ntcelx,ntcely,ntcelz),GGDH

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      LOGICAL GGDH
      LOGICAL OK, LINVOL

      double precision RESOR, SORVOL, resorfn(MRFN)

C Check for blockages and set TEf.
      if(NBLK(ICFD).gt.0) then 
        do 10 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)
          if (BCOFF(iv)) cycle

C Loop through whole blockage.
          do 20 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 21 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 22 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                TEf(I,J,K)=0.0
 22           continue
 21         continue
 20       continue
 10    continue
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

C Dynamic term.
            AP0=DENp(I,J,K)*VOL/DT
            APF=DENf(I,J,K)*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C False dynamic term.
            APfals=DENf(i,j,k)*VOL/fsDTK

C Calculate diffusion coefficients.
            GAMAE=(SIFE(I)*VIS(I,J,K)+SIFW(I+1)*VIS(I+1,J,K))/PRTE
            GAMAW=(SIFE(I-1)*VIS(I-1,J,K)+SIFW(I)*VIS(I,J,K))/PRTE
            GAMAN=(SIFN(J)*VIS(I,J,K)+SIFS(J+1)*VIS(I,J+1,K))/PRTE
            GAMAS=(SIFN(J-1)*VIS(I,J-1,K)+SIFS(J)*VIS(I,J,K))/PRTE
            GAMAH=(SIFH(K)*VIS(I,J,K)+SIFL(K+1)*VIS(I,J,K+1))/PRTE
            GAMAL=(SIFH(K-1)*VIS(I,J,K-1)+SIFL(K)*VIS(I,J,K))/PRTE

            DFE=GAMAE*AREAEW/DXEP(I)
            DFW=GAMAW*AREAEW/DXPW(I)
            DFN=GAMAN*AREANS/DYNP(J)
            DFS=GAMAS*AREANS/DYPS(J)
            DFH=GAMAH*AREAHL/DZHP(K)
            DFL=GAMAL*AREAHL/DZPL(K)

C Main coefficients for:
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
            SU(I,J,K)=AP0*TEp(i,j,K)+APfals*TEf(i,j,K)+CP*TEf(i,j,K)
            SP(I,J,K)=-AP0-APfals-CP

            Tvis=VIS(I,J,K)-VISCOS
            if (abs(Tvis).lt.small) Tvis=0.
            DUDX=(Uf(i+1,j,K)-Uf(I,J,K))/SEW(I)
            DVDY=(Vf(i,j+1,K)-Vf(i,j,K))/SNS(J)
            DWDZ=(Wf(i,j,K+1)-Wf(i,j,K))/SHL(K)

C Get remaining velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GEN2=(2.*(DUDX**2+DVDY**2+DWDZ**2)+(DUDY+DVDX)**2+
     &           (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)*Tvis

C Under-relaxation of the generation term.
C The underrelaxation factor is connected to the one of k-equation, 
C but damping should be less stronger (made using sqrt). 
C           URFKG=sqrt(URFK)
            URFKG=1.0   ! Needs testing, currently set to 1.0 (i.e. disabled). 
            GEN(I,J,K)=(1.0-URFKG)*GEN(I,J,K)+URFKG*GEN2 

C Buoyancy effect.
            IF(BUOYA)THEN
              IF(.not.GGDH) THEN

C Apply standard k-eps buoyant term.
                Th=SIFH(K)*Tf(I,J,K)+SIFL(K+1)*Tf(I,J,K+1)
                Tl=SIFH(K-1)*Tf(I,J,K-1)+SIFL(K)*Tf(I,J,K)

C Add the contribution to the source terms according to the sign
C either to SU or to SP - otherwise a negative turbulent kinetic 
C energy could result. Note: (SP is always negative, SU - positive):
                GENB=GRAV*BETA(i,j,K)*Tvis/PRANDT*(Th-Tl)/SHL(K)
                SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,GENB)*VOL
                SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,GENB)*VOL/
     &                                      anotzero(TEf(I,J,K))
              ELSE

C Apply General Gradient Diffusion Hypothesys (GGDH).
C When using GGDH it is reasonable to set the under-relaxation
C factors lower, e.g. U,V,W-0.2; k-eps-0.05; T-0.1 or lower.
C Some programming here is commented for future developments when
C the gravity will act also on axes x and y.

C Calculate turbulent stresses from velocity derivatives.
C Assume Z direction is vertical.
C                 ROUU = 2.0/3.0*DENf(I,J,K)*TEf(i,j,K)-2.0*Tvis*DUDX
C                 ROVV = 2.0/3.0*DENf(I,J,K)*TEf(i,j,K)-2.0*Tvis*DVDY
                 ROWW = 2.0/3.0*DENf(I,J,K)*TEf(i,j,K)-2.0*Tvis*DWDZ
C                 ROUV = -Tvis*(DUDY+DVDX)
                 ROUW = -Tvis*(DUDZ+DWDX)
                 ROVW = -Tvis*(DVDZ+DWDY)

C Calculate temperature derivatives.
                 Teee = SIFE(I)*Tf(I,J,K)+SIFW(I+1)*Tf(I+1,J,K)
                 Twww = SIFE(I-1)*Tf(I-1,J,K)+SIFW(I)*Tf(I,J,K)
                 DTDX = (Teee-Twww)/SEW(I)

                 Tnnn = SIFN(J)*Tf(I,J,K)+SIFS(J+1)*Tf(I,J+1,K)
                 Tsss = SIFN(J-1)*Tf(I,J-1,K)+SIFS(J)*Tf(I,J,K)
                 DTDY = (Tnnn-Tsss)/SNS(J)

                 Thhh = SIFH(K)*Tf(I,J,K)+SIFL(K+1)*Tf(I,J,K+1)
                 Tlll = SIFH(K-1)*Tf(I,J,K-1)+SIFL(K)*Tf(I,J,K)
                 DTDZ = (Thhh-Tlll)/SHL(K)

C Calculate turbulent heat flux.
C UTITA and VTITA not used (for future development).
                 CTITA=0.15
C                 UTITA = -CTITA*TEf(i,j,K)/anotzero(EDf(I,J,K))
C     &               *(ROUU*DTDX+ROUV*DTDY+ROUW*DTDZ)
C                 VTITA = -CTITA*TEf(i,j,K)/anotzero(EDf(I,J,K))
C     &               *(ROUV*DTDX+ROVV*DTDY+ROVW*DTDZ)
                 WTITA = -CTITA*TEf(i,j,K)/anotzero(EDf(I,J,K))
     &               *(ROUW*DTDX+ROVW*DTDY+ROWW*DTDZ)

C Assign gravities and add the buoyancy part to the generation term.
C                 GRAVX = 0.0
C                 GRAVY = 0.0
                 GRAVZ = GRAV
                 GENB = -BETA(I,J,K)*WTITA*(-GRAVZ)
                 GENB1(I,J,K)=GENB
C                 GENB = -BETA(I,J,K)*(UTITA*(-GRAVX)
C     &              +VTITA*(-GRAVY)+WTITA*(-GRAVZ))

C Add the contribution to the source terms (SP is always negative).
                 SU(I,J,K)=SU(I,J,K)+AMAX1(0.0,GENB)*VOL
                 SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,GENB)*VOL/
     &                                      anotzero(TEf(I,J,K))
              ENDIF
            ENDIF

            SUKD(I,J,K)=SU(I,J,K)
            SU(I,J,K)=SU(I,J,K)+GEN(I,J,K)*VOL
            SPKD(I,J,K)=SP(I,J,K)
            if (abs(Tvis).gt.small) then
              SP(I,J,K)=SP(I,J,K)-
     &                    CD*CMU*DENf(i,j,K)**2*ABS(TEf(i,j,K))*VOL/Tvis
            else
              SP(I,J,K)=SP(I,J,K)-SIGN(GREAT,Tvis)
            endif
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Modify problem.
      CALL MODTE

C Check for blockages.
      if(NBLK(ICFD).gt.0) then 
        do 200 IVO=1,NBLK(ICFD)
          iv=INBLK(IVO,ICFD)
          if (BCOFF(iv)) cycle
          call BLKBNDKE(iv)
 200    continue
      endif

C Remaining coefficients and residual source calculation.
      RESORK=0.0
      RENORMK=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &                AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            RESOR=AE(I,J,K)*TEf(i+1,j,K)+AW(I,J,K)*TEf(i-1,j,K)+
     &            AN(I,J,K)*TEf(i,j+1,K)+AS(I,J,K)*TEf(i,j-1,K)+
     &            AH(I,J,K)*TEf(i,j,K+1)+AL(I,J,K)*TEf(i,j,K-1)-
     &            AP(I,J,K)*TEf(i,j,K)+SU(I,J,K)
            RENORM=AP(I,J,K)*TEf(I,J,K)

C Check for blockages.
            if(NBLK(ICFD).gt.0) then 
              do 2006 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD)
                if (BCOFF(iv)) cycle

C If inside the blockage set resor to zero.
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if(OK) then
                  resor=0.0
                endif
 2006         continue
            endif

            SORVOL=GREAT*VolP(I,J,K)
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=real(RENORM/SORVOL)
            ENDIF
            RESORK=real(RESORK+ABS(RESOR))
            RENORMK=RENORMK+ABS(RENORM)

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.6) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                elseif (ABS(RESOR).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                endif
              enddo
            endif

C Linear under-relaxation.
            AP(I,J,K)=AP(I,J,K)/URFK
            SU(I,J,K)=SU(I,J,K)+(1.0-URFK)*AP(I,J,K)*TEf(i,j,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Check for blockages.
C Loop through whole blockage, set the turbulent kinetic energy to zero.    
      if(NBLK(ICFD).gt.0) then 
        do 2000 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD) 
          if (BCOFF(iv)) cycle
          do 2001 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 2001 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 2001 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2) 
C                SU(I,J,K)=0.0
C                AP(I,J,K)=GREAT
                AE(I,J,K)=0.0; AW(I,J,K)=0.0
                AN(I,J,K)=0.0; AS(I,J,K)=0.0
                AH(I,J,K)=0.0; AL(I,J,K)=0.0
                SP(I,J,K)=0.0; SU(I,J,K)=0.0
                AP(I,J,K)=1.0
 2001     continue
 2000   continue
      endif

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,2,NI,NJ,NK,TEf,NSWPK)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,TEf,NSWPK)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,TEf,NSWPK)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,TEf,NSWPK)
      endif

      RETURN
      END


C ********************* CALCED *********************
C Calculate coefficients of the matrix of the energy 
C dissipation equation and solve the matrix.

      SUBROUTINE CALCED
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/TDIS/RESORE,NSWPD,URFE,FSDTE
      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)  
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
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     1            TURBIN,ALAMDA,PRTE,PRED
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)   
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      COMMON/CONST/GREAT,small,GRAV
      COMMON/SUSP/SUKD(ntcelx,ntcely,ntcelz),SPKD(ntcelx,ntcely,ntcelz)
      COMMON/TIMSTP/DT
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/NORM/RENORMU,RENORMV,RENORMW,RENORMT,RENORMK,RENORME
      common/SOLVER/ILISOL
      COMMON/GGDH/ GENB1(ntcelx,ntcely,ntcelz),GGDH 

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      LOGICAL GGDH
      LOGICAL OK, LINVOL

      double precision RESOR, SORVOL, resorfn(MRFN)

C Set solution method to default.
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

C Dynamic term.
            AP0=DENp(I,J,K)*VOL/DT
            APF=DENf(I,J,K)*VOL/DT

            SMP=CN-CS+CE-CW+CH-CL-(AP0-APF)
            CP=AMAX1(0.0,SMP)

C False dynamic term.
            APfals=DENf(i,j,k)*VOL/FSDTE

C Calculate diffusion coefficients.
            GAMAE=(SIFE(I)*VIS(I,J,K)+SIFW(I+1)*VIS(I+1,J,K))/PRED
            GAMAW=(SIFE(I-1)*VIS(I-1,J,K)+SIFW(I)*VIS(I,J,K))/PRED
            GAMAN=(SIFN(J)*VIS(I,J,K)+SIFS(J+1)*VIS(I,J+1,K))/PRED
            GAMAS=(SIFN(J-1)*VIS(I,J-1,K)+SIFS(J)*VIS(I,J,K))/PRED
            GAMAH=(SIFH(K)*VIS(I,J,K)+SIFL(K+1)*VIS(I,J,K+1))/PRED
            GAMAL=(SIFH(K-1)*VIS(I,J,K-1)+SIFL(K)*VIS(I,J,K))/PRED

            DFE=GAMAE*AREAEW/DXEP(I)
            DFW=GAMAW*AREAEW/DXPW(I)
            DFN=GAMAN*AREANS/DYNP(J)
            DFS=GAMAS*AREANS/DYPS(J)
            DFH=GAMAH*AREAHL/DZHP(K)
            DFL=GAMAL*AREAHL/DZPL(K)

C Main coefficients.
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
            SU(I,J,K)=AP0*EDp(i,j,K)+APfals*EDf(i,j,K)+CP*EDf(i,j,K)
            SP(I,J,K)=-AP0-APfals-CP

            SUKD(I,J,K)=SU(I,J,K)

            Tvis=VIS(I,J,K)-VISCOS
            if (abs(Tvis).lt.small) then
              SU(I,J,K)=SU(I,J,K)+SIGN(GREAT,Tvis)
              Tvis=0.
            else
              SU(I,J,K)=SU(I,J,K)+
     &           C1*CMU*GEN(I,J,K)*VOL*DENf(i,j,K)*ABS(TEf(i,j,K))/Tvis
            endif

C Buoyancy effect.
            IF(BUOYA)THEN
              IF(.not.GGDH) THEN

C APPLY Standard K-EPS Buoyant Term
                Th=SIFH(K)*Tf(I,J,K)+SIFL(K+1)*Tf(I,J,K+1)
                Tl=SIFH(K-1)*Tf(I,J,K-1)+SIFL(K)*Tf(I,J,K)

C Add the contribution to the source terms according to the sign
C either to SU or to SP - otherwise a negative dissipation 
C could result. Note: SP is always negative, SU positive.
                GENB=GRAV*BETA(i,j,K)*Tvis/PRANDT*(Th-Tl)/SHL(K)
                SU(I,J,K)=SU(I,J,K)+AMAX1(GENB,0.0)*VOL*C3*
     &                   EDf(i,j,K)/anotzero(TEf(i,j,K))
                SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,GENB)*VOL*C3/
     &                   anotzero(TEf(i,j,K))
              ELSE
C Apply General Gradient Diffusion Hypothesys (GGDH).
C Add the contribution to the source terms (SP is always negative):
                SU(I,J,K)=SU(I,J,K)+AMAX1(GENB1(I,J,K),0.0)*VOL*C3*
     &                 EDf(i,j,K)/anotzero(TEf(i,j,K))
                SP(I,J,K)=SP(I,J,K)+AMIN1(0.0,GENB1(I,J,K))*VOL*C3/
     &                 anotzero(TEf(i,j,K))
              ENDIF
            ENDIF

            SPKD(I,J,K)=SP(I,J,K)
            if (abs(Tvis).gt.small) then
              SP(I,J,K)=SP(I,J,K)-
     &          C2*CMU*DENf(i,j,K)*DENf(i,j,K)*ABS(TEf(i,j,K))*VOL/Tvis
            else
              SP(I,J,K)=SP(I,J,K)-SIGN(GREAT,Tvis)
            endif
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Modify problem.
      CALL MODED

C Check for blockages.
      if(NBLK(ICFD).gt.0) then 
        do 2106 IVO=1,NBLK(ICFD)
          iv=INBLK(IVO,ICFD)
          if (BCOFF(iv)) cycle
          call BLKBNDED(iv)
 2106   continue
      endif

C Remaining coefficients and residual source calculation.
      RESORE=0.0
      RENORME=0.0

      DO 300 I=2,NIM1
        DO 3001 J=2,NJM1
          DO 3002 K=2,NKM1
            AP(I,J,K)=AE(I,J,K)+AW(I,J,K)+AN(I,J,K)+AS(I,J,K)+
     &              AH(I,J,K)+AL(I,J,K)-SP(I,J,K)
            RESOR=AE(I,J,K)*EDf(i+1,j,K)+AW(I,J,K)*EDf(i-1,j,K)+
     &            AN(I,J,K)*EDf(i,j+1,K)+AS(I,J,K)*EDf(i,j-1,K)+
     &            AH(I,J,K)*EDf(i,j,K+1)+AL(I,J,K)*EDf(i,j,K-1)-
     &            AP(I,J,K)*EDf(i,j,K)+SU(I,J,K)
            RENORM=AP(I,J,K)*EDf(I,J,K)

C Check for blockages.
C If inside the blockage then set resor to zero.
            if(NBLK(ICFD).gt.0) then 
              do 2006 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD)
                if (BCOFF(iv)) cycle 
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if(OK) then
                  resor=0.0
                endif
 2006         continue
            endif

            SORVOL=GREAT*VolP(I,J,K)
            IF(-SP(I,J,K).GT.0.5*SORVOL) THEN
              RESOR=RESOR/SORVOL
              RENORM=real(RENORM/SORVOL)
            ENDIF
            RESORE=real(RESORE+ABS(RESOR))
            RENORME=RENORME+ABS(RENORM)

C Search for the cells with the highest residuals, if needed.
            if (IFNDWT(ICFD).eq.7) then
              do irfn=1,NRFN
                if (IRFNCLS(ICFD,irfn,1).eq.0) then
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                elseif (ABS(RESOR).gt.resorfn(irfn)) then
                  if (irfn.lt.NRFN) then
                    do iirfn=NRFN,irfn+1,-1
                      if (IRFNCLS(ICFD,iirfn-1,1).eq.0) cycle
                      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn-1,1)
                      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn-1,2)
                      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn-1,3)
                      resorfn(iirfn)=resorfn(iirfn-1)
                    enddo
                  endif
                  IRFNCLS(ICFD,irfn,1)=I
                  IRFNCLS(ICFD,irfn,2)=J
                  IRFNCLS(ICFD,irfn,3)=K
                  resorfn(irfn)=ABS(RESOR)
                  exit
                endif
              enddo
            endif

C Linear under-relaxation.
            AP(I,J,K)=AP(I,J,K)/URFE
            SU(I,J,K)=SU(I,J,K)+(1.0-URFE)*AP(I,J,K)*EDf(i,j,K)
 3002     CONTINUE
 3001   CONTINUE
 300  CONTINUE

C Check for blockages.
      if(NBLK(ICFD).gt.0) then 
        do 2000 IVO=1,NBLK(ICFD)
          IV=INBLK(IVO,ICFD)
          if (BCOFF(iv)) cycle

C Loop through whole blockage, set the dissipation to a small value.
          do 2001 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            do 2001 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              do 2001 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2) 
                SU(I,J,K)=100.0*small
                AP(I,J,K)=1.0; AE(I,J,K)=0.0; AW(I,J,K)=0.0
                AN(I,J,K)=0.0; AS(I,J,K)=0.0; AH(I,J,K)=0.0
                AL(I,J,K)=0.0; SP(I,J,K)=0.0
 2001     continue
 2000   continue
      endif

C Solution of difference equation.
      if (ILISOL.eq.1) then
        CALL LISOLV1(2,2,2,NI,NJ,NK,EDf,NSWPD)
      elseif (ILISOL.eq.2) then
        CALL LISOLV2(2,2,2,NI,NJ,NK,EDf,NSWPD)
      elseif (ILISOL.eq.3) then
        CALL LISOLV3(2,2,2,NI,NJ,NK,EDf,NSWPD)
      elseif (ILISOL.eq.4) then
        call lisolv4(2,2,2,NIM1,NJM1,NKM1,EDf,NSWPD)
      endif

      RETURN
      END


C ********************* INDBND *********************
C Initialise some boundary conditions.

      SUBROUTINE INDBND
#include "building.h"
#include "cfd.h"

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/BCTYPCEL/IBCTPC(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

C Initialise IBCTPC.
      do i=1,NI
        do j=1,NJ
          do k=1,NK
            IBCTPC(i,j,k)=0
          enddo
        enddo
      enddo

C Examine each CFD boundary condition in turn.
      DO 20 IV=1,NVOL(ICFD)        

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' has the following meanings: 1 for west; 2 for east;
C 3 for south; 4 for north; 5 for low; 6 for high.
        location = IVOLF(IV,ICFD)

C Determine BC type.
        ityp=IVTYPE(IV,ICFD)

C Solid and symmetrical BCs.
        if (ityp.lt.10) then

C West wall.
          IF(location.EQ.1)THEN
            I=IVCELLS(IV,ICFD,1)
            DO 40 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              DO 401 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                Uf(I,J,K)=0.0

C Set up array of BC type per cell.
C This is used for velocity cells which straddle BC divides.
                IBCTPC(I,J,K)=ityp
  401        CONTINUE
  40       CONTINUE

C East wall.
          ELSEIF(location.EQ.2)THEN
            I=IVCELLS(IV,ICFD,1)
            DO 80 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              DO 801 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                Uf(I+1,J,K)=0.0
                IBCTPC(I,J,K)=ityp
  801        CONTINUE
  80       CONTINUE

C South wall.
          ELSEIF(location.EQ.3)THEN
            J=JVCELLS(IV,ICFD,1)
            DO 120 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO 1201 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                Vf(I,J,K)=0.0
                IBCTPC(I,J,K)=ityp
 1201         CONTINUE
 120        CONTINUE

C North wall.
C Previously, there was a bug in this section caused by
C iterating K from JSBi to KSBf instead of KSBi to KSBf.
C This had the effect that solid boundary cells on the north face may
C have had a momentum in the V direction equal to the starting V
C momentum specfied in the domain file.
          ELSEIF(location.EQ.4)THEN
            J=JVCELLS(IV,ICFD,1)
            DO 160 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO 1601 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                Vf(I,J+1,K)=0.0
                IBCTPC(I,J,K)=ityp
 1601         CONTINUE
 160        CONTINUE

C Low wall.
          ELSEIF(location.EQ.5)THEN
            K=KVCELLS(IV,ICFD,1)
            DO 200 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO 2001 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                Wf(I,J,K)=0.0
                IBCTPC(I,J,K)=ityp
 2001         CONTINUE
 200        CONTINUE

C High wall.
          ELSEIF(location.EQ.6)THEN
            K=KVCELLS(IV,ICFD,1)
            DO 240 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO 2401 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                Wf(I,J,K+1)=0.0
                IBCTPC(I,J,K)=ityp
 2401         CONTINUE
 240        CONTINUE
          ENDIF

C Set pressure if pressure BC.
        elseif (ityp.eq.10) then
          DO 3101 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            DO 3102 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              DO 3103 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                P(I,J,K)=VOLPres(IV,ICFD)
                IBCTPC(I,J,K)=ityp
 3103         CONTINUE
 3102       CONTINUE
 3101     CONTINUE

C For other BC types, just set up IBCTPC.
        else
          DO 3201 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            DO 3202 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              DO 3203 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                IBCTPC(I,J,K)=ityp
 3203         CONTINUE
 3202       CONTINUE
 3201     CONTINUE
        endif
 20   CONTINUE

      RETURN
      END

C ********************* DEFINESSO *********************
C Definition of Small Supply Openings.
C Called once in the beginning of the computations. 
C If an area for an opening is defined in the dfd file the 
C variable VOLArea is used to store the ratio of real to actual 
C opening area. If no area is specified then VOLArea is set to 1.0.

      SUBROUTINE DEFINESSO 
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/SSOinit/areaSSO

      LOGICAL areaSSO

C Search for velocity type openings and see if an opening area 
C has been defined.
C After this loop VOLArea will represent the ratio between the real 
C opening size and the actual opening size.
      do 10 IV=1,NVOL(ICFD)
        if (IVTYPE(IV,ICFD).eq.11) then
          if (VOLArea(IV,ICFD).gt.0.0) then

C Find area of opening as defined by cell geometry.
            TOTAREA=0.
            if (IVOLF(IV,ICFD).eq.1.or.IVOLF(IV,ICFD).eq.2) then

C Opening in west or east face.
              do 20 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                do 21 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  TOTAREA=TOTAREA+AreaEWP(J,K)
 21             continue 
 20           continue
            elseif (IVOLF(IV,ICFD).eq.3.or.IVOLF(IV,ICFD).eq.4) then

C Opening in south or north face.
              do 30 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
                do 31 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  TOTAREA=TOTAREA+AreaNSP(I,K)
 31             continue 
 30           continue
            elseif (IVOLF(IV,ICFD).eq.5.or.IVOLF(IV,ICFD).eq.6) then

C Opening in low or high face.
              do 40 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
                do 41 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                  TOTAREA=TOTAREA+AreaHLP(I,J)
 41             continue 
 40           continue
            else
            endif

C Calculate ratio of real to cell area and store in VOLArea.
            VOLArea(IV,ICFD)=VOLArea(IV,ICFD)/TOTAREA
            if (VOLArea(IV,ICFD).gt.1.) then
              VOLArea(IV,ICFD)=1.
            endif
          else
            VOLArea(IV,ICFD)=1.
          endif
        else
          VOLArea(IV,ICFD)=1.
        endif
 10   continue

C Set logical variable to indicate that the VOLArea's have been
C calculated.  This will prevent this subroutine from being invoked
C again during the same simulation (as will happen in conflated runs).
      areaSSO = .TRUE.

      return
      end 

C ********************* INOUT *********************
C Establish domain inlet and outlet conditions.

      SUBROUTINE INOUT
#include "building.h"
#include "cfd.h"

      COMMON/BNDCND/FLWIN,XMONIN
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     &            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     &            DZHP(ntcelz),DZPL(ntcelz),
     &            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     &            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     &            Wf(ntcelx,ntcely,ntcelz),
     &            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     &            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     &             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     &            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     &            BETA(ntcelx,ntcely,ntcelz)
C      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      COMMON/BCTYPCEL/IBCTPC(ntcelx,ntcely,ntcelz)
C      COMMON/ITERAT/NITER

      real Utmp,Vtmp,Wtmp

      XMONIN=0.0
      FLWIN=0.0
      FLWOUT=0.0

C Examine each BC.
      DO 100 IV=1,NVOL(ICFD)

        ityp=IVTYPE(IV,ICFD)
        location=IVOLF(IV,ICFD)

C For velocity type openings, set boundary conditions.
        if (ityp.eq.11) then

C West face.
          if (location.eq.1) then

C Calculate opening area.
            AREA=(YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))*
     &           (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))

C Iterate over cells covering opening.
            DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)

C Important for satisfying mass balance and convergency - updating 
C temperature when flow leaving domain leads to correct density 
C and correct source term in pressure correction equation. 
                  if (VOLVel(IV,ICFD).gt.0.0) then
                    Tf(I-1,J,K)=VOLTemp(IV,ICFD)
                  else
                    Tf(I-1,J,K)=Tf(I,J,K)
                  endif
                  if (BOUSSA) then
                    TEMP=TBAR(ICFD)
                  else
                    TEMP=Tf(I-1,J,K)
                  endif

C Set inlet velocity and turbulence scalars.
                  Uf(I,J,K)=VOLVel(IV,ICFD)/AIRDEN(TEMP)/AREA
                  VRES=Uf(I,J,K)*SQRT(1.+ETAND(VOLDir(IV,ICFD,1))**2+
     &                                   ETAND(VOLDir(IV,ICFD,2))**2)
                  TEf(I-1,J,K)=TURBIN*VRES**2
                  EDf(I-1,J,K)=TEf(I-1,J,K)**1.5/ALAMDA/SQRT(AREA)

C Set lateral velocities.
                  Vtmp=Uf(I,J,K)*ETAND(VOLDir(IV,ICFD,1))

C Halve laterial velocities in cells that straddle the BC divide, 
C maintaining overall momentum. In cases where the opening is
C 1 cell wide, this does not allow the full lateral momentum to
C develop, so instead use the full value. Implement this by doubling
C Vtmp.
                  if (JVCELLS(IV,ICFD,1).eq.JVCELLS(IV,ICFD,2))
     &              Vtmp=Vtmp*2       
                  if (J.eq.JVCELLS(IV,ICFD,1)) then
                        
C Check if the BC on the other side is another velocity BC (IBCTPC). Set
C it to -ve the first time we set the velocity, then aggregate the
C second time to avoid losing the velocity contribution from the first
C time.
                    if (IBCTPC(I,J-1,K).eq.11) then
                      Vf(I-1,J,K)=Vtmp/2.
                      IBCTPC(I,J-1,K)=-11
                    elseif (IBCTPC(I,J-1,K).eq.-11) then
                      Vf(I-1,J,K)=Vf(I-1,J,K)+Vtmp/2.
                      IBCTPC(I,J-1,K)=11

C Otherwise, set lateral velocity as normal.                      
                    else
                      Vf(I-1,J,K)=Vtmp/2.
                    endif

C Cell not on a BC divide; use full lateral velocity.
                  else
                    Vf(I-1,J,K)=Vtmp
                  endif

C Need to check separately for the end BC divide because this will be
C the same scalar cell as the start if the opening is only 1 cell wide.
                  if (J.eq.JVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J+1,K).eq.11) then
                      Vf(I-1,J+1,K)=Vtmp/2.
                      IBCTPC(I,J+1,K)=-11
                    elseif (IBCTPC(I,J+1,K).eq.-11) then
                      Vf(I-1,J+1,K)=Vf(I-1,J+1,K)+Vtmp/2.
                      IBCTPC(I,J+1,K)=11
                    else
                      Vf(I-1,J+1,K)=Vtmp/2.
                    endif
                  endif

C Now the other lateral direction.
                  Wtmp=Uf(I,J,K)*ETAND(VOLDir(IV,ICFD,2))
                  if (KVCELLS(IV,ICFD,1).eq.KVCELLS(IV,ICFD,2))
     &              Wtmp=Wtmp*2
                  if (K.eq.KVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J,K-1).eq.11) then
                      Wf(I-1,J,K)=Wtmp/2.
                      IBCTPC(I,J,K-1)=-11
                    elseif (IBCTPC(I,J,K-1).eq.-11) then
                      Wf(I-1,J,K)=Wf(I-1,J,K)+Wtmp/2.
                      IBCTPC(I,J,K-1)=11
                    else
                      Wf(I-1,J,K)=Wtmp/2.
                    endif
                  else
                    Wf(I-1,J,K)=Wtmp
                  endif
                  if (K.eq.KVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J,K+1).eq.11) then
                      Wf(I-1,J,K+1)=Wtmp/2.
                      IBCTPC(I,J,K+1)=-11
                    elseif (IBCTPC(I,J,K+1).eq.-11) then
                      Wf(I-1,J,K+1)=Wf(I-1,J,K+1)+Wtmp/2.
                      IBCTPC(I,J,K+1)=11
                    else
                      Wf(I-1,J,K+1)=Wtmp/2.
                    endif
                  endif
                enddo
              enddo
            enddo

C East face.
C See west face above for comments.
          elseif (location.eq.2) then
            AREA=(YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))*
     &           (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))
            DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  if (VOLVel(IV,ICFD).gt.0.0) then
                    Tf(I+1,J,K)=VOLTemp(IV,ICFD)
                  else
                    Tf(I+1,J,K)=Tf(I,J,K)
                  endif
                  if (BOUSSA) then
                    TEMP=TBAR(ICFD)
                  else
                    TEMP=Tf(I+1,J,K)
                  endif
                  Uf(I+1,J,K)=-VOLVel(IV,ICFD)/AIRDEN(TEMP)/AREA
                  VRES=abs(Uf(I+1,J,K))*
     &              SQRT(1.+ETAND(VOLDir(IV,ICFD,1))**2+
     &                      ETAND(VOLDir(IV,ICFD,2))**2)
                  TEf(I+1,J,K)=TURBIN*VRES**2
                  EDf(I+1,J,K)=TEf(I+1,J,K)**1.5/ALAMDA/SQRT(AREA)
                  Vtmp=-Uf(I+1,J,K)*ETAND(VOLDir(IV,ICFD,1))
                  if (JVCELLS(IV,ICFD,1).eq.JVCELLS(IV,ICFD,2))
     &              Vtmp=Vtmp*2
                  if (J.eq.JVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J-1,K).eq.11) then
                      Vf(I+1,J,K)=Vtmp/2.
                      IBCTPC(I,J-1,K)=-11
                    elseif (IBCTPC(I,J-1,K).eq.-11) then
                      Vf(I+1,J,K)=Vf(I+1,J,K)+Vtmp/2.
                      IBCTPC(I,J-1,K)=11
                    else
                      Vf(I+1,J,K)=Vtmp/2.
                    endif
                  else
                    Vf(I+1,J,K)=Vtmp
                  endif
                  if (J.eq.JVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J+1,K).eq.11) then
                      Vf(I+1,J+1,K)=Vtmp/2.
                      IBCTPC(I,J+1,K)=-11
                    elseif (IBCTPC(I,J+1,K).eq.-11) then
                      Vf(I+1,J+1,K)=Vf(I+1,J+1,K)+Vtmp/2.
                      IBCTPC(I,J+1,K)=11
                    else
                      Vf(I+1,J+1,K)=Vtmp/2.
                    endif
                  endif
                  Wtmp=-Uf(I+1,J,K)*ETAND(VOLDir(IV,ICFD,2))
                  if (KVCELLS(IV,ICFD,1).eq.KVCELLS(IV,ICFD,2))
     &              Wtmp=Wtmp*2
                  if (K.eq.KVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J,K-1).eq.11) then
                      Wf(I+1,J,K)=Wtmp/2.
                      IBCTPC(I,J,K-1)=-11
                    elseif (IBCTPC(I,J,K-1).eq.-11) then
                      Wf(I+1,J,K)=Wf(I+1,J,K)+Wtmp/2.
                      IBCTPC(I,J,K-1)=11
                    else
                      Wf(I+1,J,K)=Wtmp/2.
                    endif
                  else
                    Wf(I+1,J,K)=Wtmp
                  endif
                  if (K.eq.KVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J,K+1).eq.11) then
                      Wf(I+1,J,K+1)=Wtmp/2.
                      IBCTPC(I,J,K+1)=-11
                    elseif (IBCTPC(I,J,K+1).eq.-11) then
                      Wf(I+1,J,K+1)=Wf(I+1,J,K+1)+Wtmp/2.
                      IBCTPC(I,J,K+1)=11
                    else
                      Wf(I+1,J,K+1)=Wtmp/2.
                    endif
                  endif
                enddo
              enddo
            enddo

C South face.
C See west face above for comments.
          ELSEIF (location.EQ.3) THEN
            AREA=(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &           (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))
            DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  if (VOLVel(IV,ICFD).gt.0.0) then
                    Tf(I,J-1,K)=VOLTemp(IV,ICFD)
                  else
                    Tf(I,J-1,K)=Tf(I,J,K)
                  endif
                  if (BOUSSA) then
                    TEMP=TBAR(ICFD)
                  else
                    TEMP=Tf(I,J-1,K)
                  endif
                  Vf(I,J,K)=VOLVel(IV,ICFD)/AIRDEN(TEMP)/AREA
                  VRES=Vf(I,J,K)*SQRT(1.+ETAND(VOLDir(IV,ICFD,1))**2+
     &                                   ETAND(VOLDir(IV,ICFD,2))**2)
                  TEf(I,J-1,K)=TURBIN*VRES**2
                  EDf(I,J-1,K)=TEf(I,J-1,K)**1.5/ALAMDA/SQRT(AREA)
                  Utmp=Vf(I,J,K)*ETAND(VOLDir(IV,ICFD,1))
                  if (IVCELLS(IV,ICFD,1).eq.IVCELLS(IV,ICFD,2))
     &              Utmp=Utmp*2
                  if (I.eq.IVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I-1,J,K).eq.11) then
                      Uf(I,J-1,K)=Utmp/2.
                      IBCTPC(I-1,J,K)=-11
                    elseif (IBCTPC(I-1,J,K).eq.-11) then
                      Uf(I,J-1,K)=Uf(I,J-1,K)+Utmp/2.
                      IBCTPC(I-1,J,K)=11
                    else
                      Uf(I,J-1,K)=Utmp/2.
                    endif
                  else
                    Uf(I,J-1,K)=Utmp
                  endif
                  if (I.eq.IVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I+1,J,K).eq.11) then
                      Uf(I+1,J-1,K)=Utmp/2.
                      IBCTPC(I+1,J,K)=-11
                    elseif (IBCTPC(I+1,J,K).eq.-11) then
                      Uf(I+1,J-1,K)=Uf(I+1,J-1,K)+Utmp/2.
                      IBCTPC(I+1,J,K)=11
                    else
                      Uf(I+1,J-1,K)=Utmp/2.
                    endif
                  endif
                  Wtmp=Vf(I,J,K)*ETAND(VOLDir(IV,ICFD,2))
                  if (KVCELLS(IV,ICFD,1).eq.KVCELLS(IV,ICFD,2))
     &              Wtmp=Wtmp*2
                  if (K.eq.KVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J,K-1).eq.11) then
                      Wf(I,J-1,K)=Wtmp/2.
                      IBCTPC(I,J,K-1)=-11
                    elseif (IBCTPC(I,J,K-1).eq.-11) then
                      Wf(I,J-1,K)=Wf(I,J-1,K)+Wtmp/2.
                      IBCTPC(I,J,K-1)=11
                    else
                      Wf(I,J-1,K)=Wtmp/2.
                    endif
                  else
                    Wf(I,J-1,K)=Wtmp
                  endif
                  if (K.eq.KVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J,K+1).eq.11) then
                      Wf(I,J-1,K+1)=Wtmp/2.
                      IBCTPC(I,J,K+1)=-11
                    elseif (IBCTPC(I,J,K+1).eq.-11) then
                      Wf(I,J-1,K+1)=Wf(I,J-1,K+1)+Wtmp/2.
                      IBCTPC(I,J,K+1)=11
                    else
                      Wf(I,J-1,K+1)=Wtmp/2.
                    endif
                  endif
                enddo
              enddo
            enddo

C North face.
C See west face above for comments.
          ELSEIF (location.EQ.4) THEN
            AREA=(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &           (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))
            DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  if (VOLVel(IV,ICFD).gt.0.0) then
                    Tf(I,J+1,K)=VOLTemp(IV,ICFD)
                  else
                    Tf(I,J+1,K)=Tf(I,J,K)
                  endif
                  if (BOUSSA) then
                    TEMP=TBAR(ICFD)
                  else
                    TEMP=Tf(I,J+1,K)
                  endif
                  Vf(I,J+1,K)=-VOLVel(IV,ICFD)/AIRDEN(TEMP)/AREA
                  VRES=abs(Vf(I,J+1,K))*
     &              SQRT(1.+ETAND(VOLDir(IV,ICFD,1))**2+
     &                      ETAND(VOLDir(IV,ICFD,2))**2)
                  TEf(I,J+1,K)=TURBIN*VRES**2
                  EDf(I,J+1,K)=TEf(I,J+1,K)**1.5/ALAMDA/SQRT(AREA)
                  Utmp=-Vf(I,J+1,K)*ETAND(VOLDir(IV,ICFD,1))
                  if (IVCELLS(IV,ICFD,1).eq.IVCELLS(IV,ICFD,2))
     &              Utmp=Utmp*2
                  if (I.eq.IVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I-1,J,K).eq.11) then
                      Uf(I,J+1,K)=Utmp/2.
                      IBCTPC(I-1,J,K)=-11
                    elseif (IBCTPC(I-1,J,K).eq.-11) then
                      Uf(I,J+1,K)=Uf(I,J+1,K)+Utmp/2.
                      IBCTPC(I-1,J,K)=11
                    else
                      Uf(I,J+1,K)=Utmp/2.
                    endif
                  else
                    Uf(I,J+1,K)=Utmp
                  endif
                  if (I.eq.IVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I+1,J,K).eq.11) then
                      Uf(I+1,J+1,K)=Utmp/2.
                      IBCTPC(I+1,J,K)=-11
                    elseif (IBCTPC(I+1,J,K).eq.-11) then
                      Uf(I+1,J+1,K)=Uf(I+1,J+1,K)+Utmp/2.
                      IBCTPC(I+1,J,K)=11
                    else
                      Uf(I+1,J+1,K)=Utmp/2.
                    endif
                  endif
                  Wtmp=-Vf(I,J+1,K)*ETAND(VOLDir(IV,ICFD,2))
                  if (KVCELLS(IV,ICFD,1).eq.KVCELLS(IV,ICFD,2))
     &              Wtmp=Wtmp*2
                  if (K.eq.KVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J,K-1).eq.11) then
                      Wf(I,J+1,K)=Wtmp/2.
                      IBCTPC(I,J,K-1)=-11
                    elseif (IBCTPC(I,J,K-1).eq.-11) then
                      Wf(I,J+1,K)=Wf(I,J+1,K)+Wtmp/2.
                      IBCTPC(I,J,K-1)=11
                    else
                      Wf(I,J+1,K)=Wtmp/2.
                    endif
                  else
                    Wf(I,J+1,K)=Wtmp
                  endif
                  if (K.eq.KVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J,K+1).eq.11) then
                      Wf(I,J+1,K+1)=Wtmp/2.
                      IBCTPC(I,J,K+1)=-11
                    elseif (IBCTPC(I,J,K+1).eq.-11) then
                      Wf(I,J+1,K+1)=Wf(I,J+1,K+1)+Wtmp/2.
                      IBCTPC(I,J,K+1)=11
                    else
                      Wf(I,J+1,K+1)=Wtmp/2.
                    endif
                  endif
                enddo
              enddo
            enddo

C Low face.
C See west face above for comments.
          ELSEIF (location.EQ.5) THEN
            AREA=(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &           (YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))
            DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  if (VOLVel(IV,ICFD).gt.0.0) then
                    Tf(I,J,K-1)=VOLTemp(IV,ICFD)
                  else
                    Tf(I,J,K-1)=Tf(I,J,K)
                  endif
                  if (BOUSSA) then
                    TEMP=TBAR(ICFD)
                  else
                    TEMP=Tf(I,J,K-1)
                  endif
                  Wf(I,J,K)=VOLVel(IV,ICFD)/AIRDEN(TEMP)/AREA
                  VRES=Wf(I,J,K)*SQRT(1.+ETAND(VOLDir(IV,ICFD,1))**2+
     &                                   ETAND(VOLDir(IV,ICFD,2))**2)
                  TEf(I,J,K-1)=TURBIN*VRES**2
                  EDf(I,J,K-1)=TEf(I,J,K-1)**1.5/ALAMDA/SQRT(AREA)
                  Utmp=Wf(I,J,K)*ETAND(VOLDir(IV,ICFD,1))
                  if (IVCELLS(IV,ICFD,1).eq.IVCELLS(IV,ICFD,2))
     &              Utmp=Utmp*2
                  if (I.eq.IVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I-1,J,K).eq.11) then
                      Uf(I,J,K-1)=Utmp/2.
                      IBCTPC(I-1,J,K)=-11
                    elseif (IBCTPC(I-1,J,K).eq.-11) then
                      Uf(I,J,K-1)=Uf(I,J,K-1)+Utmp/2.
                      IBCTPC(I-1,J,K)=11
                    else
                      Uf(I,J,K-1)=Utmp/2.
                    endif
                  else
                    Uf(I,J,K-1)=Utmp
                  endif
                  if (I.eq.IVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I+1,J,K).eq.11) then
                      Uf(I+1,J,K-1)=Utmp/2.
                      IBCTPC(I+1,J,K)=-11
                    elseif (IBCTPC(I+1,J,K).eq.-11) then
                      Uf(I+1,J,K-1)=Uf(I+1,J,K-1)+Utmp/2.
                      IBCTPC(I+1,J,K)=11
                    else
                      Uf(I+1,J,K-1)=Utmp/2.
                    endif
                  endif
                  Vtmp=Wf(I,J,K)*ETAND(VOLDir(IV,ICFD,2))
                  if (JVCELLS(IV,ICFD,1).eq.JVCELLS(IV,ICFD,2))
     &              Vtmp=Vtmp*2
                  if (J.eq.JVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J-1,K).eq.11) then
                      Vf(I,J,K-1)=Vtmp/2.
                      IBCTPC(I,J-1,K)=-11
                    elseif (IBCTPC(I,J-1,K).eq.-11) then
                      Vf(I,J,K-1)=Vf(I,J,K-1)+Vtmp/2.
                      IBCTPC(I,J-1,K)=11
                    else
                      Vf(I,J,K-1)=Vtmp/2.
                    endif
                  else
                    Vf(I,J,K-1)=Vtmp
                  endif
                  if (J.eq.JVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J+1,K).eq.11) then
                      Vf(I,J+1,K-1)=Vtmp/2.
                      IBCTPC(I,J+1,K)=-11
                    elseif (IBCTPC(I,J+1,K).eq.-11) then
                      Vf(I,J+1,K-1)=Vf(I,J+1,K-1)+Vtmp/2.
                      IBCTPC(I,J+1,K)=11
                    else
                      Vf(I,J+1,K-1)=Vtmp/2.
                    endif
                  endif
                enddo
              enddo
            enddo

C High face.
C See west face above for comments.
          ELSEIF (location.EQ.6) THEN              
            AREA=(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &           (YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))
            DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  if (VOLVel(IV,ICFD).gt.0.0) then
                    Tf(I,J,K+1)=VOLTemp(IV,ICFD)
                  else
                    Tf(I,J,K+1)=Tf(I,J,K)
                  endif
                  if (BOUSSA) then
                    TEMP=TBAR(ICFD)
                  else
                    TEMP=Tf(I,J,K+1)
                  endif
                  Wf(I,J,K+1)=-VOLVel(IV,ICFD)/AIRDEN(TEMP)/AREA
                  VRES=abs(Wf(I,J,K+1))*
     &              SQRT(1.+ETAND(VOLDir(IV,ICFD,1))**2+
     &                      ETAND(VOLDir(IV,ICFD,2))**2)
                  TEf(I,J,K+1)=TURBIN*VRES**2
                  EDf(I,J,K+1)=TEf(I,J,K+1)**1.5/ALAMDA/SQRT(AREA)
                  Utmp=-Wf(I,J,K+1)*ETAND(VOLDir(IV,ICFD,1))
                  if (IVCELLS(IV,ICFD,1).eq.IVCELLS(IV,ICFD,2))
     &              Utmp=Utmp*2
                  if (I.eq.IVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I-1,J,K).eq.11) then
                      Uf(I,J,K+1)=Utmp/2.
                      IBCTPC(I-1,J,K)=-11
                    elseif (IBCTPC(I-1,J,K).eq.-11) then
                      Uf(I,J,K+1)=Uf(I,J,K+1)+Utmp/2.
                      IBCTPC(I-1,J,K)=11
                    else
                      Uf(I,J,K+1)=Utmp/2.
                    endif
                  else
                    Uf(I,J,K+1)=Utmp
                  endif
                  if (I.eq.IVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I+1,J,K).eq.11) then
                      Uf(I+1,J,K+1)=Utmp/2.
                      IBCTPC(I+1,J,K)=-11
                    elseif (IBCTPC(I+1,J,K).eq.-11) then
                      Uf(I+1,J,K+1)=Uf(I+1,J,K+1)+Utmp/2.
                      IBCTPC(I+1,J,K)=11
                    else
                      Uf(I+1,J,K+1)=Utmp/2.
                    endif
                  endif
                  Vtmp=-Wf(I,J,K+1)*ETAND(VOLDir(IV,ICFD,2))
                  if (JVCELLS(IV,ICFD,1).eq.JVCELLS(IV,ICFD,2))
     &              Vtmp=Vtmp*2
                  if (J.eq.JVCELLS(IV,ICFD,1)) then
                    if (IBCTPC(I,J-1,K).eq.11) then
                      Vf(I,J,K+1)=Vtmp/2.
                      IBCTPC(I,J-1,K)=-11
                    elseif (IBCTPC(I,J-1,K).eq.-11) then
                      Vf(I,J,K+1)=Vf(I,J,K+1)+Vtmp/2.
                      IBCTPC(I,J-1,K)=11
                    else
                      Vf(I,J,K+1)=Vtmp/2.
                    endif
                  else
                    Vf(I,J,K+1)=Vtmp
                  endif
                  if (J.eq.JVCELLS(IV,ICFD,2)) then
                    if (IBCTPC(I,J+1,K).eq.11) then
                      Vf(I,J+1,K+1)=Vtmp/2.
                      IBCTPC(I,J+1,K)=-11
                    elseif (IBCTPC(I,J+1,K).eq.-11) then
                      Vf(I,J+1,K+1)=Vf(I,J+1,K+1)+Vtmp/2.
                      IBCTPC(I,J+1,K)=11
                    else
                      Vf(I,J+1,K+1)=Vtmp/2.
                    endif
                  endif
                enddo
              enddo
            enddo
          endif

C Aggregate total flow in and out.
          if (VOLVel(IV,ICFD).gt.0.0) then
            FLWIN=FLWIN+VOLVel(IV,ICFD)
            XMONIN=XMONIN+VOLVel(IV,ICFD)
          elseif (VOLVel(IV,ICFD).lt.0.0) then
            FLWOUT=FLWOUT-VOLVel(IV,ICFD)
          endif

C Calculate mass flow exiting at `zero-gradient type' openings.
        elseif (ityp.eq.12) then

C Iterate over cells of opening.
          DO I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            DO J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              DO K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)

C West face.
                IF (location.eq.1) THEN
                  FLWOUT=FLWOUT-Uf(I+1,J,K)*DENf(I,J,K)*AreaEWP(J,K)

C East face.
                ELSEIF (location.eq.2) THEN
                  FLWOUT=FLWOUT+Uf(I,J,K)*DENf(I,J,K)*AreaEWP(J,K)

C South face.
                ELSEIF(location.eq.3) THEN
                  FLWOUT=FLWOUT-Vf(I,J+1,K)*DENf(I,J,K)*AreaNSP(I,K)

C North face.
                ELSEIF(location.eq.4) THEN
                  FLWOUT=FLWOUT+Vf(I,J,K)*DENf(I,J,K)*AreaNSP(I,K)

C Low face.
                ELSEIF(location.eq.5) THEN
                  FLWOUT=FLWOUT-Wf(I,J,K+1)*DENf(I,J,K)*AreaHLP(I,J)

C High face.
                ELSEIF(location.eq.6) THEN
                  FLWOUT=FLWOUT+Wf(I,J,K)*DENf(I,J,K)*AreaHLP(I,J)
                endif
              enddo
            enddo
          enddo
        endif

C Examine next opening.
 100  CONTINUE

C Now fix velocities at `zero-gradient type' openings. Examine each opening
C in turn.
      DO 200 IV=1,NVOL(ICFD)
        IF (IVTYPE(IV,ICFD).eq.12.and.VOLVel(IV,ICFD).LE.0.0) THEN

C Calculate FF ratio of mass flow into domain to ratio of mass flow exiting.
C This is used to adjust the flows at the zero-gradient exits to balance
C mass over the domain. Refer to Versteeg and Malalasekera (1995), p198.
          FF=ABS(FLWIN/anotzero(FLWOUT))

C Iterate over cells covering opening.
          DO 201 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
            DO 2011 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
              DO 2012 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)

C Opening in west face.
                IF(IVOLF(IV,ICFD).EQ.1) THEN
                  IF(Uf(I+1,J,K).GT.0.) THEN
                    Uf(I,J,K)=-FF*Uf(I+1,J,K)
                  ELSE
                    Uf(I,J,K)=FF*Uf(I+1,J,K)
                  ENDIF

C Opening in east face.
                ELSEIF(IVOLF(IV,ICFD).EQ.2) THEN
                  IF(Uf(I,J,K).LT.0.) THEN
                    Uf(I+1,J,K)=-FF*Uf(I,J,K)
                  ELSE
                    Uf(I+1,J,K)=FF*Uf(I,J,K)
                  ENDIF

C Opening in south face.
                ELSEIF(IVOLF(IV,ICFD).EQ.3) THEN
                  IF(Vf(I,J+1,K).GT.0.) THEN
                    Vf(I,J,K)=-FF*Vf(I,J+1,K)
                  ELSE
                    Vf(I,J,K)=FF*Vf(I,J+1,K)
                  ENDIF

C Opening in north face.
                ELSEIF(IVOLF(IV,ICFD).EQ.4) THEN
                  IF(Vf(I,J,K).LT.0.) THEN
                    Vf(I,J+1,K)=-FF*Vf(I,J,K)
                  ELSE
                    Vf(I,J+1,K)=FF*Vf(I,J,K)
                  ENDIF

C Opening in low face.
                ELSEIF(IVOLF(IV,ICFD).EQ.5) THEN
                  IF(Wf(I,J,K+1).GT.0.) THEN
                    Wf(I,J,K)=-FF*Wf(I,J,K+1)
                  ELSE
                    Wf(I,J,K)=FF*Wf(I,J,K+1)
                  ENDIF
                  
C Opening in high face.
                ELSEIF(IVOLF(IV,ICFD).EQ.6) THEN
                  IF(Wf(I,J,K).LT.0.) THEN
                    Wf(I,J,K+1)=-FF*Wf(I,J,K)
                  ELSE
                    Wf(I,J,K+1)=FF*Wf(I,J,K)
                  ENDIF
                ENDIF
 2012         CONTINUE
 2011       CONTINUE
 201      CONTINUE
        ENDIF
 200  CONTINUE

      RETURN
      END


C ********************* RECRES *********************
C Modify residuals for all scalar variables if pressure is 
C fixed in a cell.

      FUNCTION RECRES(RESRM)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      RECRES=RESRM
      DO 110 L=1,NOPEN(ICFD)
        DO 1101 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
          DO 1102 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 1103 K=KOPENi(L,ICFD),KOPENf(L,ICFD)

C Diminish sum of absolute mass sources on fixed pressure nodes.
              IF(IWOPEN(L,ICFD).EQ.0) RECRES=RECRES-ABS(SU(I,J,K))
 1103       CONTINUE
 1102     CONTINUE
 1101   CONTINUE
 110  CONTINUE

      RETURN
      END

C ********************* VELDERIV *********************
C Calculate the velocity derivitives for use in turbulence equations.

      subroutine VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
#include "building.h"
#include "cfd.h"

      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)

C Calculate derivitives.
      DUDY=((SIFN(J)*(Uf(I,J,K)+Uf(I+1,J,K))/2.+
     &       SIFS(J+1)*(Uf(I,J+1,K)+Uf(I+1,J+1,K))/2.)-
     &         (SIFN(J-1)*(Uf(I,J-1,K)+Uf(I+1,J-1,K))/2.+
     &          SIFS(J)*(Uf(I,J,K)+Uf(I+1,J,K))/2.))/SNS(J)

      DUDZ=((SIFH(K)*(Uf(I,J,K)+Uf(I+1,J,K))/2.+
     &       SIFL(K+1)*(Uf(I,J,K+1)+Uf(I+1,J,K+1))/2.)-
     &         (SIFH(K-1)*(Uf(I,J,K-1)+Uf(I+1,J,K-1))/2.+
     &          SIFL(K)*(Uf(I,J,K)+Uf(I+1,J,K))/2.))/SHL(K)

      DVDX=((SIFW(I+1)*(Vf(I+1,J,K)+Vf(I+1,J+1,K))/2.+
     &       SIFE(I)*(Vf(I,J,K)+Vf(I,J+1,K))/2.)-
     &         (SIFW(I)*(Vf(I,J,K)+Vf(I,J+1,K))/2.+
     &          SIFE(I-1)*(Vf(I-1,J,K)+Vf(I-1,J+1,K))/2.))/SEW(I)

      DVDZ=((SIFH(K)*(Vf(I,J,K)+Vf(I,J+1,K))/2.+
     &       SIFL(K+1)*(Vf(I,J,K+1)+Vf(I,J+1,K+1))/2.)-
     &         (SIFH(K-1)*(Vf(I,J,K-1)+Vf(I,J+1,K-1))/2.+
     &          SIFL(K)*(Vf(I,J,K)+Vf(I,J+1,K))/2.))/SHL(K)

      DWDX=((SIFE(I)*(Wf(I,J,K)+Wf(I,J,K+1))/2.+
     &       SIFW(I+1)*(Wf(I+1,J,K)+Wf(I+1,J,K+1))/2.)-
     &         (SIFE(I-1)*(Wf(I-1,J,K)+Wf(I-1,J,K+1))/2.+
     &          SIFW(I)*(Wf(I,J,K)+Wf(I,J,K+1))/2.))/SEW(I)

      DWDY=((SIFN(J)*(Wf(I,J,K)+Wf(I,J,K+1))/2.+
     &       SIFS(J+1)*(Wf(I,J+1,K)+Wf(I,J+1,K+1))/2.)-
     &         (SIFN(J-1)*(Wf(I,J-1,K)+Wf(I,J-1,K+1))/2.+
     &          SIFS(J)*(Wf(I,J,K)+Wf(I,J,K+1))/2.))/SNS(J)

      RETURN
      END


C ********************* APLAWCF *********************
C Calculate A? coefficients based on power law scheme.
C Note that the value of C should be -ve for E,N and H faces, 
C e.g. AE()=APLAWCF(DFE,-CE) and for other faces AW()=APLAWCF(DFW,CW).

      FUNCTION APLAWCF(DF,C)

      if (abs(C/DF).gt.9.99) then  ! floating point error if close to 10.0.
        APLAWCF=AMAX1(C,0.)
      else
        APLAWCF=DF*AMAX1(0.,(1.-0.1*ABS(C/DF))**5)+AMAX1(C,0.)
      endif

      RETURN
      END


C ********************* EXCFDBC *********************
C Export current CFD boundary conditions to a file.
C Assumes output file on iunit is already open.

      SUBROUTINE EXCFDBC(iunit,ier)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALLMA,CALPOL
      CHARACTER POLNAM*12
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

      do iv=1,NVOL(ICFD)
        if (BCOFF(iv)) cycle
        ityp=IVTYPE(iv,ICFD)
        write(iunit,'(a)')' '
        write(iunit,'(a,i3.3)')'volume: ',iv
        if (ityp.eq.1) then
          write(iunit,'(a)')'type: solid, temperature'
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
        elseif (ityp.eq.2) then
          write(iunit,'(a)')'type: solid, heat flux'
          write(iunit,'(a,f8.2)')'heat flux (W): ',VOLHeat(iv,ICFD)
        elseif (ityp.eq.3) then
          write(iunit,'(a)')'type: symmetrical'
        elseif (ityp.eq.4) then
          write(iunit,'(a)')'type: solid, temperature'
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
        elseif (ityp.eq.5) then
          write(iunit,'(a)')'type: solid, heat flux'
          write(iunit,'(a,f8.2)')'heat flux (W): ',VOLHeat(iv,ICFD)
        elseif (ityp.eq.6) then
          write(iunit,'(a)')'type: symmetrical'
        elseif (ityp.eq.10) then
          write(iunit,'(a)')'type: opening, pressure'
          write(iunit,'(a,f8.2)')'pressure (Pa): ',VOLPres(iv,ICFD)
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
        elseif (ityp.eq.11) then
          write(iunit,'(a)')'type: opening, directed flow'
          write(iunit,'(a,f8.5)')'mass flow (kg/s): ',VOLVel(iv,ICFD)
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
          write(iunit,'(a,f8.2)')'direction 1 (deg): ',VOLDir(iv,ICFD,1)
          write(iunit,'(a,f8.2)')'direction 2 (deg): ',VOLDir(iv,ICFD,2)
        elseif (ityp.eq.12) then
          write(iunit,'(a)')'type: opening, zero gradient'
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
        elseif (ityp.eq.13) then
          write(iunit,'(a)')'type: opening, mass'
          write(iunit,'(a,f8.5)')'mass flow (kg/s): ',VOLVel(iv,ICFD)
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
        elseif (ityp.eq.20) then
          write(iunit,'(a)')'type: source'
          write(iunit,'(a,f8.2)')'heat flux (W): ',VOLHeat(iv,ICFD)
          do ictm=1,NCTM(ICFD)
            write(iunit,'(a,i2.2,a,f8.5)')'contaminant ',ictm,
     &        ' rate (kg/s): ',VOLPOL(ictm,iv,ICFD)
          enddo
        elseif (ityp.eq.30) then
          write(iunit,'(a)')'type: blockage, heat flux'
          write(iunit,'(a,f8.2)')'heat flux (W): ',VOLHeat(iv,ICFD)
        elseif (ityp.eq.31 .or. ityp.eq.32) then
          write(iunit,'(a)')'type: blockage, temperature'
          write(iunit,'(a,f8.2)')'temperature (C): ',VOLTemp(iv,ICFD)
        elseif (ityp.eq.33) then
          write(iunit,'(a)')'type: blockage, face temperatures'
          write(iunit,'(a,f8.2)')'east temperature (C): ',
     &      BLKTEMP(iv,1)
          write(iunit,'(a,f8.2)')'west temperature (C): ',
     &      BLKTEMP(iv,2)
          write(iunit,'(a,f8.2)')'north temperature (C): ',
     &      BLKTEMP(iv,3)
          write(iunit,'(a,f8.2)')'south temperature (C): ',
     &      BLKTEMP(iv,4)
          write(iunit,'(a,f8.2)')'high temperature (C): ',
     &      BLKTEMP(iv,5)
          write(iunit,'(a,f8.2)')'low temperature (C): ',
     &      BLKTEMP(iv,6)
        elseif (ityp.eq.34) then
          write(iunit,'(a)')'type: blockage, heat flux (from gain)'
          write(iunit,'(a,f8.2)')'heat flux (W): ',VOLHeat(iv,ICFD)
        elseif (ityp.eq.35) then
          write(iunit,'(a)')'type: blockage, heat flux (from person)'
          write(iunit,'(a,f8.2)')'heat flux (W): ',VOLHeat(iv,ICFD)
        endif
      enddo

      RETURN
      END


C ********************* RFNCFDGRD *********************
C Try to refine CFD grid in areas of poor convergence.
C We don't store per cell residuals in memory so this subroutine uses
C common RFNGRD, populated elsewhere, to determine which cells to
C refine. Returns logical 'refined' indicating whether any refinements
C have been made.

      SUBROUTINE RFNCFDGRD(refined,ier)
#include "building.h"
#include "cfd.h"

      logical refined

      COMMON/ICFNOD/ICFD,ICP
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &              NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/VARp/Up(ntcelx,ntcely,ntcelz),Vp(ntcelx,ntcely,ntcelz),
     1            Wp(ntcelx,ntcely,ntcelz),TEp(ntcelx,ntcely,ntcelz),
     2            EDp(ntcelx,ntcely,ntcelz)  
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPp/Tp(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
C      COMMON/FLUPRp/DENp(ntcelx,ntcely,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALLMA,CALPOL
      CHARACTER POLNAM*12
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz)
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      logical INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,IZEROT,IZanKE,
     &        IMITZ
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)

      logical ok

      refined=.false.
      ier=0
        
C Refine grid by splitting cells in half in all 3 dimensions (into 8).
C Currently, we can't have a cell bounded by more than one cell on a
C face, se we must split all cells on the row in each dimension.

      do irfn=1,NRFN

C First, check we won't be exceeding array limits.
        if (NI+1.gt.NTCELX .or. NJ+1.gt.NTCELY .or. NK+1.gt.NTCELZ) then
          call EDISP(IUOUT,'Maximum number of cells reached.')
          return
        endif
        if (NREG(1,ICFD)+2.gt.MNREG .or. NREG(2,ICFD)+2.gt.MNREG .or. 
     &      NREG(3,ICFD)+2.gt.MNREG .or. NREG(4,ICFD)+2.gt.MNREG) then
          call EDISP(IUOUT,'Maximum number of regions reached.')
          return
        endif

C Split in I dimension.
        icel=IRFNCLS(ICFD,irfn,1)
C First, check that we havn't already split this I plane in this run of
C refinements.
        if (irfn.gt.1) then
          do iirfn=1,irfn-1
            if (icel.eq.IRFNCLS(ICFD,iirfn,1)) then
              icel=0
              exit
            endif
          enddo
        endif

        if (icel.gt.0) then
C Find which region the cell is in.
          itot=1
          do irgn=1,NREG(1,ICFD)
            itot=itot+ABS(NCELX(irgn,ICFD))
            if (icel.le.itot) exit
          enddo

          if (ABS(NCELX(irgn,ICFD)).eq.1) then
C Region is only 1 cell; this is straightforward to split.
            NCELX(irgn,ICFD)=2
            
C Otherwise, we need to create a new region for the cell being split.
          elseif (icel.eq.itot) then
C Cell to split is at the end of the region.
            NCELX(irgn,ICFD)=ABS(NCELX(irgn,ICFD))-1
            xwth=XU(icel+1)-XU(icel)
            XREG(irgn,ICFD)=XREG(irgn,ICFD)-xwth
            NREG(1,ICFD)=NREG(1,ICFD)+1
            do iirgn=NREG(1,ICFD),irgn+2,-1
              NCELX(iirgn,ICFD)=NCELX(iirgn-1,ICFD)
              XREG(iirgn,ICFD)=XREG(iirgn-1,ICFD)
              XPLAW(iirgn,ICFD)=XPLAW(iirgn-1,ICFD)
            enddo
            NCELX(irgn+1,ICFD)=2
            XREG(irgn+1,ICFD)=xwth
            XPLAW(irgn+1,ICFD)=1.
          elseif (icel.eq.itot-ABS(NCELX(irgn,ICFD))+1) then
C Cell to split is at the beginning of the region.
            NCELX(irgn,ICFD)=ABS(NCELX(irgn,ICFD))-1
            xwth=XU(icel+1)-XU(icel)
            XREG(irgn,ICFD)=XREG(irgn,ICFD)-xwth
            NREG(1,ICFD)=NREG(1,ICFD)+1
            do iirgn=NREG(1,ICFD),irgn+1,-1
              NCELX(iirgn,ICFD)=NCELX(iirgn-1,ICFD)
              XREG(iirgn,ICFD)=XREG(iirgn-1,ICFD)
              XPLAW(iirgn,ICFD)=XPLAW(iirgn-1,ICFD)
            enddo
            NCELX(irgn,ICFD)=2
            XREG(irgn,ICFD)=xwth
            XPLAW(irgn,ICFD)=1.
          else
C Cell is in the middle of the region.
            NCELX(irgn,ICFD)=ABS(NCELX(irgn,ICFD))-(itot-icel+1)
            XREG(irgn,ICFD)=XREG(irgn,ICFD)-(XU(itot+1)-XU(icel))
            NREG(1,ICFD)=NREG(1,ICFD)+2
            do iirgn=NREG(1,ICFD),irgn+3,-1
              NCELX(iirgn,ICFD)=NCELX(iirgn-2,ICFD)
              XREG(iirgn,ICFD)=XREG(iirgn-2,ICFD)
              XPLAW(iirgn,ICFD)=XPLAW(iirgn-2,ICFD)
            enddo
            NCELX(irgn+1,ICFD)=2
            XREG(irgn+1,ICFD)=XU(icel+1)-XU(icel)
            XPLAW(irgn+1,ICFD)=1.
            NCELX(irgn+2,ICFD)=itot-icel
            XREG(irgn+2,ICFD)=XU(itot+1)-XU(icel+1)
            XPLAW(irgn+2,ICFD)=XPLAW(irgn,ICFD)
          endif
          NI=NI+1; NIM1=NI-1; NIM2=NI-2
        endif

C Split in J dimension.
        jcel=IRFNCLS(ICFD,irfn,2)

C First, check that we havn't already split this J plane in this run of
C refinements.
        if (irfn.gt.1) then
          do iirfn=1,irfn-1
            if (jcel.eq.IRFNCLS(ICFD,iirfn,2)) then
              jcel=0
              exit
            endif
          enddo
        endif

        if (jcel.gt.0) then
C Find which region the cell is in.
          itot=1
          do irgn=1,NREG(2,ICFD)
            itot=itot+ABS(NCELY(irgn,ICFD))
            if (jcel.le.itot) exit
          enddo

          if (ABS(NCELY(irgn,ICFD)).eq.1) then
C Region is only 1 cell; this is straightforward to split.
            NCELY(irgn,ICFD)=2
            
C Otherwise, we need to create a new region for the cell being split.
          elseif (jcel.eq.itot) then
C Cell to split is at the end of the region.
            NCELY(irgn,ICFD)=ABS(NCELY(irgn,ICFD))-1
            ywth=YV(jcel+1)-YV(jcel)
            YREG(irgn,ICFD)=YREG(irgn,ICFD)-ywth
            NREG(2,ICFD)=NREG(2,ICFD)+1
            do iirgn=NREG(2,ICFD),irgn+2,-1
              NCELY(iirgn,ICFD)=NCELY(iirgn-1,ICFD)
              YREG(iirgn,ICFD)=YREG(iirgn-1,ICFD)
              YPLAW(iirgn,ICFD)=YPLAW(iirgn-1,ICFD)
            enddo
            NCELY(irgn+1,ICFD)=2
            YREG(irgn+1,ICFD)=ywth
            YPLAW(irgn+1,ICFD)=1.
          elseif (jcel.eq.itot-ABS(NCELY(irgn,ICFD))+1) then
C Cell to split is at the beginning of the region.
            NCELY(irgn,ICFD)=ABS(NCELY(irgn,ICFD))-1
            ywth=YV(jcel+1)-YV(jcel)
            YREG(irgn,ICFD)=YREG(irgn,ICFD)-ywth
            NREG(2,ICFD)=NREG(2,ICFD)+1
            do iirgn=NREG(2,ICFD),irgn+1,-1
              NCELY(iirgn,ICFD)=NCELY(iirgn-1,ICFD)
              YREG(iirgn,ICFD)=YREG(iirgn-1,ICFD)
              YPLAW(iirgn,ICFD)=YPLAW(iirgn-1,ICFD)
            enddo
            NCELY(irgn,ICFD)=2
            YREG(irgn,ICFD)=ywth
            YPLAW(irgn,ICFD)=1.
          else
C Cell is in the middle of the region.
            NCELY(irgn,ICFD)=ABS(NCELY(irgn,ICFD))-(itot-jcel+1)
            YREG(irgn,ICFD)=YREG(irgn,ICFD)-(YV(itot+1)-YV(jcel))
            NREG(2,ICFD)=NREG(2,ICFD)+2
            do iirgn=NREG(2,ICFD),irgn+3,-1
              NCELY(iirgn,ICFD)=NCELY(iirgn-2,ICFD)
              YREG(iirgn,ICFD)=YREG(iirgn-2,ICFD)
              YPLAW(iirgn,ICFD)=YPLAW(iirgn-2,ICFD)
            enddo
            NCELY(irgn+1,ICFD)=2
            YREG(irgn+1,ICFD)=YV(jcel+1)-YV(jcel)
            YPLAW(irgn+1,ICFD)=1.
            NCELY(irgn+2,ICFD)=itot-jcel
            YREG(irgn+2,ICFD)=YV(itot+1)-YV(jcel+1)
            YPLAW(irgn+2,ICFD)=YPLAW(irgn,ICFD)
          endif
          NJ=NJ+1; NJM1=NJ-1; NJM2=NJ-2
        endif

C Split in K dimension.
        kcel=IRFNCLS(ICFD,irfn,3)

C First, check that we havn't already split this K plane in this run of
C refinements.
        if (irfn.gt.1) then
          do iirfn=1,irfn-1
            if (kcel.eq.IRFNCLS(ICFD,iirfn,3)) then
              kcel=0
              exit
            endif
          enddo
        endif

        if (kcel.gt.0) then
C Find which region the cell is in.
          itot=1
          do irgn=1,NREG(3,ICFD)
            itot=itot+ABS(NCELZ(irgn,ICFD))
            if (kcel.le.itot) exit
          enddo

          if (ABS(NCELZ(irgn,ICFD)).eq.1) then
C Region is only 1 cell; this is straightforward to split.
            NCELZ(irgn,ICFD)=2

C Otherwise, we need to create a new region for the cell being split.
          elseif (kcel.eq.itot) then
C Cell to split is at the end of the region.
            NCELZ(irgn,ICFD)=ABS(NCELZ(irgn,ICFD))-1
            Zwth=ZW(kcel+1)-ZW(kcel)
            ZREG(irgn,ICFD)=ZREG(irgn,ICFD)-Zwth
            NREG(3,ICFD)=NREG(3,ICFD)+1
            do iirgn=NREG(3,ICFD),irgn+2,-1
              NCELZ(iirgn,ICFD)=NCELZ(iirgn-1,ICFD)
              ZREG(iirgn,ICFD)=ZREG(iirgn-1,ICFD)
              ZPLAW(iirgn,ICFD)=ZPLAW(iirgn-1,ICFD)
            enddo
            NCELZ(irgn+1,ICFD)=2
            ZREG(irgn+1,ICFD)=Zwth
            ZPLAW(irgn+1,ICFD)=1.
          elseif (kcel.eq.itot-ABS(NCELZ(irgn,ICFD))+1) then
C Cell to split is at the beginning of the region.
            NCELZ(irgn,ICFD)=ABS(NCELZ(irgn,ICFD))-1
            Zwth=ZW(kcel+1)-ZW(kcel)
            ZREG(irgn,ICFD)=ZREG(irgn,ICFD)-Zwth
            NREG(3,ICFD)=NREG(3,ICFD)+1
            do iirgn=NREG(3,ICFD),irgn+1,-1
              NCELZ(iirgn,ICFD)=NCELZ(iirgn-1,ICFD)
              ZREG(iirgn,ICFD)=ZREG(iirgn-1,ICFD)
              ZPLAW(iirgn,ICFD)=ZPLAW(iirgn-1,ICFD)
            enddo
            NCELZ(irgn,ICFD)=2
            ZREG(irgn,ICFD)=Zwth
            ZPLAW(irgn,ICFD)=1.
          else
C Cell is in the middle of the region.
            NCELZ(irgn,ICFD)=ABS(NCELZ(irgn,ICFD))-(itot-kcel+1)
            ZREG(irgn,ICFD)=ZREG(irgn,ICFD)-(ZW(itot+1)-ZW(kcel))
            NREG(3,ICFD)=NREG(3,ICFD)+2
            do iirgn=NREG(3,ICFD),irgn+3,-1
              NCELZ(iirgn,ICFD)=NCELZ(iirgn-2,ICFD)
              ZREG(iirgn,ICFD)=ZREG(iirgn-2,ICFD)
              ZPLAW(iirgn,ICFD)=ZPLAW(iirgn-2,ICFD)
            enddo
            NCELZ(irgn+1,ICFD)=2
            ZREG(irgn+1,ICFD)=ZW(kcel+1)-ZW(kcel)
            ZPLAW(irgn+1,ICFD)=1.
            NCELZ(irgn+2,ICFD)=itot-kcel
            ZREG(irgn+2,ICFD)=ZW(itot+1)-ZW(kcel+1)
            ZPLAW(irgn+2,ICFD)=ZPLAW(irgn,ICFD)
          endif
          NK=NK+1; NKM1=NK-1; NKM2=NK-2
        endif

        if (icel.eq.0 .and. jcel.eq.0 .and. kcel.eq.0) cycle

C Adjust state variable arrays.
        do 150 k=NK,2,-1
          do 150 j=NJ,2,-1
            do 150 i=NI,2,-1
              ok=.false.
              if (i.gt.icel .and. icel.gt.0) then
                ok=.true.
                ii=i-1
              else
                ii=i
              endif
              if (j.gt.jcel .and. jcel.gt.0) then
                ok=.true.
                jj=j-1
              else
                jj=j
              endif
              if (k.gt.kcel .and. kcel.gt.0) then
                ok=.true.
                kk=k-1
              else
                kk=k
              endif
              if (ok) then
                if (INCALU.and.ii.ge.2) then
                  Up(i,j,k)=Up(ii,jj,kk)
                  Uf(i,j,k)=Uf(ii,jj,kk)
                endif
                if (INCALV.and.jj.ge.2) then
                  Vp(i,j,k)=Vp(ii,jj,kk)
                  Vf(i,j,k)=Vf(ii,jj,kk)
                endif
                if (INCALW.and.kk.ge.2) then
                  Wp(i,j,k)=Wp(ii,jj,kk)
                  Wf(i,j,k)=Wf(ii,jj,kk)
                endif
                P(i,j,k)=P(ii,jj,kk)
                if (INCALK) then
                  TEp(i,j,k)=TEp(ii,jj,kk)
                  TEf(i,j,k)=TEf(ii,jj,kk)
                endif
                if (INCALD) then
                  EDp(i,j,k)=EDp(ii,jj,kk)
                  EDf(i,j,k)=EDf(ii,jj,kk)
                endif
                if (INCALT) then
                  Tp(i,j,k)=Tp(ii,jj,kk)
                  Tf(i,j,k)=Tf(ii,jj,kk)
                endif
                VIS(i,j,k)=VIS(ii,jj,kk)
                BETA(i,j,k)=BETA(ii,jj,kk)
                GAMH(i,j,k)=GAMH(ii,jj,kk)
                if (CALLMA(ICFD)) AGEf(i,j,k)=AGEf(ii,jj,kk)
                do ipol=1,NCTM(ICFD)
                  if (CALPOL(ipol,ICFD)) then
                    POLCONCp(ipol,i,j,k)=POLCONCp(ipol,ii,jj,kk)
                    POLCONCf(ipol,i,j,k)=POLCONCf(ipol,ii,jj,kk)
                  endif
                enddo
              endif
  150   continue

C Adjust boundary condition extents.
        do ivol=i,NVOL(ICFD)
          if (IVOLF(ivol,ICFD).eq.1) then
            IVCELLS(ivol,ICFD,1)=2
            IVCELLS(ivol,ICFD,2)=2
          elseif (IVOLF(ivol,ICFD).eq.2) then            
            IVCELLS(ivol,ICFD,1)=NIM1
            IVCELLS(ivol,ICFD,2)=NIM1
          else
            if (IVCELLS(ivol,ICFD,1).gt.icel .and. icel.gt.0)
     &        IVCELLS(ivol,ICFD,1)=IVCELLS(ivol,ICFD,1)+1
            if (IVCELLS(ivol,ICFD,2).ge.icel .and. icel.gt.0)
     &        IVCELLS(ivol,ICFD,2)=IVCELLS(ivol,ICFD,2)+1
          endif
          if (IVOLF(ivol,ICFD).eq.3) then
            JVCELLS(ivol,ICFD,1)=2
            JVCELLS(ivol,ICFD,2)=2
          elseif (IVOLF(ivol,ICFD).eq.4) then            
            JVCELLS(ivol,ICFD,1)=NJM1
            JVCELLS(ivol,ICFD,2)=NJM1
          else
            if (JVCELLS(ivol,ICFD,1).gt.jcel .and. jcel.gt.0)
     &        JVCELLS(ivol,ICFD,1)=JVCELLS(ivol,ICFD,1)+1
            if (JVCELLS(ivol,ICFD,2).ge.jcel .and. jcel.gt.0)
     &        JVCELLS(ivol,ICFD,2)=JVCELLS(ivol,ICFD,2)+1
          endif
          if (IVOLF(ivol,ICFD).eq.5) then
            KVCELLS(ivol,ICFD,1)=2
            KVCELLS(ivol,ICFD,2)=2
          elseif (IVOLF(ivol,ICFD).eq.6) then            
            KVCELLS(ivol,ICFD,1)=NKM1
            KVCELLS(ivol,ICFD,2)=NKM1
          else
            if (KVCELLS(ivol,ICFD,1).gt.kcel .and. kcel.gt.0)
     &        KVCELLS(ivol,ICFD,1)=KVCELLS(ivol,ICFD,1)+1
            if (KVCELLS(ivol,ICFD,2).ge.kcel .and. kcel.gt.0)
     &        KVCELLS(ivol,ICFD,2)=KVCELLS(ivol,ICFD,2)+1
          endif
        enddo

C Adjust monitoring position.
        if (IMONT(ICFD).gt.icel .and. icel.gt.0)
     &    IMONT(ICFD)=IMONT(ICFD)+1
        if (JMONT(ICFD).gt.jcel .and. jcel.gt.0)
     &    JMONT(ICFD)=JMONT(ICFD)+1
        if (KMONT(ICFD).gt.kcel .and. kcel.gt.0)
     &    KMONT(ICFD)=KMONT(ICFD)+1

C Adjust further refinement positions.
        do iirfn=NRFN,irfn+1,-1
          if (IRFNCLS(ICFD,iirfn,1).gt.icel .and. icel.gt.0)
     &      IRFNCLS(ICFD,iirfn,1)=IRFNCLS(ICFD,iirfn,1)+1
          if (IRFNCLS(ICFD,iirfn,2).gt.jcel .and. jcel.gt.0)
     &      IRFNCLS(ICFD,iirfn,2)=IRFNCLS(ICFD,iirfn,2)+1
          if (IRFNCLS(ICFD,iirfn,3).gt.kcel .and. kcel.gt.0)
     &      IRFNCLS(ICFD,iirfn,3)=IRFNCLS(ICFD,iirfn,3)+1
        enddo

C Record refinement positions so we can average results later.
        NRFND=NRFND+1
        IRFND(ICFD,NRFND,1)=icel
        IRFND(ICFD,NRFND,2)=jcel
        IRFND(ICFD,NRFND,3)=kcel

C Set flag to say that we have refined some cells.
        refined=.true.
      enddo

C Recalculate allowable number of refinements per run.
      ! NRFN=(NI*NJ*NK)/1000
      NRFN=1

      CALL NEW2OLD
      ! CALL PRNNEW(274)
      ! CALL INDBND
      ! CALL PRNNEW(275)

      return
      end
