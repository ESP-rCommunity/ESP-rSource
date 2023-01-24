C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C CFMNGE         - Controls CFD calcs and results output when BSim and CFD are conflated.
C SURFCONF       - Calls CFMNGE each time step there is surface conflation.
C CFDLIBHEAD     - Write header information to cfd results file.
C cfdlibdom      - Write information describing the CFD domain model to results file.
C cfdlib         - Write results of CFD simulation to results file.
C readcfd3dascii - read ascii information from one timestep.
C RFNAVG         - average results if the grid has been refined mid-simulation.
C CFDOCHK        - Check if occupants are present and deactivate BCs if needed.


C ********************* CFMNGE *********************
C Manages CFD calculations and results output when BSim and CFD
C are conflated.

      SUBROUTINE CFMNGE(ICOMP,IND)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      common/outin/iuout,iuin,ieout
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfcall/icfcal(MNZ)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)
      COMMON/ICFNOD/ICFD,ICP
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      common/cfdini/INITFL(MNZ)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      CHARACTER*72 TITLE,CFTRFL,LPHI
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      common/htccfd/HCICFD(MCOM,MS)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/SOLVER/ILISOL
      common/CFDTOGS/ACCON,CFD2BLD,CFD2MFS
      common/OCCCFD/TaCFD(MCOM),TTRACK(MCOM)

C Commons for trace.
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      common/fvisgo/gophrun
      logical gophrun

      DIMENSION HC(MNSBZ),SFNAM(MNSBZ)

      CHARACTER NBCHAR*10,SFNAM*12
      CHARACTER*72 outfil,LCFD,outs
      LOGICAL INITFL,CONVER
      integer ACCON,CFD2BLD,CFD2MFS
      real TaCFD

C MITMFS is the maximum number of times cfd will be invoked using mfs 
C determined boundary conditions at one time step to attain mutual
C cfd-mfs convergence.
      PARAMETER(MITMFS=10)

      ICFD=IND
      ICP=ICOMP
      IMFACT=0
      IBLACT=IFCFD(ICP)
      TSTEP=3600.0/float(ntstep)
      LKTR=1

C Check if CFD-MFS trace output is requested.
C ITRC: If 0, no trace. If >0, unit number.
      ITRC=0
      if (ITRACE(44).gt.0) then
      
C Check if within trace period.
        if (NSINC.ge.ITC.and.NSINC.le.ITCF) then
        
C If ACC is active, only print header and increment trace counter on
C gopher run.
          if((abs(IBLACT).ge.4).and.(abs(IBLACT).le.5))then
            if (gophrun) then
              call edisp(ITU,' ')
              write(outs,'(a,i5)')'CFMNGE/MFSCORR: trace output ',ICNT
              call edisp(ITU,outs)
              ICNT=ICNT+1
            endif
          else
            call edisp(ITU,' ')
            write(outs,'(a,i5)')'CFMNGE/MFSCORR: trace output ',ICNT
            call edisp(ITU,outs)
            ICNT=ICNT+1
          endif
          ITRC=ITU
        endif
      endif

C Hardwire version of LISOLV to use.
      ILISOL=3

C Increment counter that keeps track of how many times CFD has been invoked
C for this zone, but only on the gopher run if the ACC is active.
      if((abs(IBLACT).ge.4).and.(abs(IBLACT).le.5))then
        if(gophrun)icfcal(ICFD)=icfcal(ICFD)+1
      else
        icfcal(ICFD)=icfcal(ICFD)+1
      endif

      TTRACK(ICP)=icfcal(ICFD)

C Debug.
C       write(6,*) 'CFMNGE (ICP,ICOMP,IND[icfd],icfcal)',ICP,ICOMP,
C    &                                                   IND,icfcal

C If this is the first time CFD has been called, initialise surface
C convection to zero. If mfs conflation is active then the following
C code needs to run every time this subroutine is called.
      IF(icfcal(ICFD).le.1)then
        DO 10 I=1,NCONST(ICP)
          QCFDF(ICP,I)=0.0
  10    CONTINUE
        TINIT(ICFD)=TFA(ICP)

C Initialise variable fields. Either read initial values from file saved
C from previous solution (if user specified this option), or initialise
C all field variables to values specified in CFD input file.
        IF(INITFL(ICFD))then
          ifl=ICFTMP
          ier=0
          call INTSTR(ICFD,nbchar,ISWD,IER)
          write(outfil,'(a15,a)')'../tmp/tmp_DFS_',nbchar(1:ISWD)
          CLOSE(ifl)
          CALL FPOPEN(IFL,IER,5,1,outfil)
          REWIND(ifl)
          CALL REAOLD(ifl)
        ELSE
          CALL INIT
        ENDIF
      ELSE
      
C CFD has been called at a previous time step. Initial values
C for this time step taken as final solution achieved at previous
C time step. Read previous solution.
        ifl=ICFTMP
        ier=0
        call INTSTR(ICFD,nbchar,ISWD,IER)
        write(outfil,'(a15,a)')'../tmp/tmp_DFS_',nbchar(1:ISWD)
        ier=0
        CALL FPOPEN(IFL,IER,5,1,outfil)
        REWIND(ifl)
        CALL REAOLD(ifl)
      ENDIF

C Calculate geometrical quantities.
      CALL GRID(IER)

C Save domain geometry data to results library.
      if (icfcal(ICFD).eq.1.and..not.gophrun) call cfdlibdom(ICOMP)

C Set up boundary conditions that are independent of airflow.
      CALL INDBND

C Map zone thermal conditions to CFD solid boundaries.
      CALL BS2CFDSB

C Map zone thermal conditions to CFD blockages.
      CALL BS2CFDBLK

C Calculate areas of solid boundaries.
      CALL CFAREA(NCONST(ICP))

C *** Invoke CFD simulation.

C This line, if uncommented, will force preliminary simulations to
C use one way CFD-MFS conflation.
C      if(((abs(IBLACT).ge.4).and.(abs(IBLACT).le.5)).and.gophrun)
C     &                                                      goto 255

C Activate mfs-cfd coupling via MFBNDR if airflow network is defined.
      IF(ICFMFS(ICFD).EQ.1)THEN
        IF(NCONF.GT.1)THEN
          if(CFD2MFS.eq.2)then

C Current model limitation: if there is more than one CFD domain,
C impose 'dry' MFS calculated flow rates directly on CFD domain
C (i.e. one way conflation). In case the user has specified two-way
C conflation, indicate this change.
            write(outs,'(a,a)')'   Multiple CFD domains detected, ',
     &                      'deactivating CFD-MFS iteration.'
            call edisp(iuout,outs)

            CFD2MFS=1
          endif
          GOTO 255

C Only one CFD domain.
        ELSEIF(NCONF.EQ.1)THEN
          IF(CFD2MFS.EQ.2)THEN 

C Use iterative process to attain mutual convergence (two way conflation).
            GOTO 254
          ELSE

C One way conflation.
            GOTO 255
          endif
        ELSE

C This code should never execute.
          CALL EDISP(IUOUT,
     &      'CFMNGE: Error configuring mfs-cfd conflation!')
          GOTO 258
        ENDIF
      ELSE

C No CFD-MFS conflation.
        CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)
        GOTO 258
      ENDIF

 254  IF(LKTR.EQ.1)CALL MFBNDR(1)

C Trace output.
      if (ITRC.gt.0) then
        write(ITRC,'(a)')' '
        write(ITRC,'(a)')'*** DRY MFS results (before iteration) ***'
        CALL MFOUTP(ITRC)
      endif
 256  CALL MFBNDR(2)      
      CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)
      IF(CONVER)THEN
        IPOK=3
        
C Trace output.
        if (ITRC.gt.0) then
          write(ITRC,'(a)')' '
          write(ITRC,'(a,i2.2,a)')
     &      '*** Conflated MFS results (at iteration ',LKTR,') ***'
        endif
        CALL MFBNDR(IPOK)
        IF(IPOK.EQ.0)THEN
          write(outs,'(A,I4,A)')'   Got mfs-cfd mutual convergence in '
     &                         ,lktr,' iterations.'
          call edisp(iuout,outs)
          CALL MFBNDR(0)
          GOTO 257
        ELSE
          IF(LKTR.GT.MITMFS)THEN
            write(outs,'(A,I4,A)')
     &        '   Could not get mfs-cfd convergence after '
     &        ,lktr,' iterations.'
            call edisp(iuout,outs)
            CALL MFBNDR(0)
            GOTO 257
          ENDIF
          LKTR=LKTR+1
          GOTO 256
        ENDIF
      ELSE
        outs='standalone did not converge, going to next timestep'
        call edisp(iuout,outs)
        CALL MFBNDR(0)
        GOTO 257
      ENDIF
      GOTO 258

 255  CALL MFBNDR(1)  ! Initialise MFS connections representing CFD connections.
      CALL MFBNDR(2)  ! Impose mfs mass flows on CFD domain.

C Trace output.
      if (ITRC.gt.0) then
        write(ITRC,'(a)')'*** MFS results (imposed on CFD) ***'
        write(ITRC,'(a)')' '
        CALL MFOUTP(ITRC)
      endif

C CFD solution manager.
      CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)
      CALL MFBNDR(0)  ! decouple flow network and CFD domain
      GOTO 258

C CFD-MFS trace output delimeters.
 257  if (ITRC.gt.0) then
        write(ITRC,'(a)')' '
        if (gophrun) then
          write(ITRC,'(a)')'*** Exploratory simulation done ***'
        else
          write(ITRC,'(a)')'*** Time step done ***'
        endif
        write(ITRC,'(a)')' '
      endif

C Convection coefficients.
 258  DO 100 I=1,NCONST(ICP)
        icon=IZSTOCN(icp,i)
        SFNAM(I)=SNAME(icp,i)
 100  CONTINUE

C Surface conflation.
      IF( ABS(IBLACT).EQ.1 .OR.
     &    (ABS(IBLACT).GE.4.and.ABS(IBLACT).LE.7) )THEN
        TAIR = TFA(ICP)
      ENDIF

C Integrated thermal conflation.
      IF(ABS(IBLACT).EQ.2) THEN
        icTREF(1,ICFD) = 2
        CALL TREFBC(1,TAIR)
        TFA(ICP) = TAIR
      ENDIF

      IF( ABS(IBLACT).EQ.1 .OR.
     &    (ABS(IBLACT).GE.4.and.ABS(IBLACT).LE.7) )THEN
        TFA(ICP) = TAIR  
      ENDIF

C Calculate convection coefficients and assign to temporary
C array. Adaptive conflation routine will decide whether to
C use these in the zone energy balance.
      CALL CFCONV(TAIR,HC,SFNAM,NCONST(ICP))
      DO 200 I=1,NCONST(ICP)
        HCICFD(ICP,I)=HC(I)
 200  CONTINUE

C Save calculated variables for use at the next time-step.
      if(.not.gophrun)then
        ifl=icftmp
        call INTSTR(ICFD,nbchar,ISWD,IER)
        write(outfil,'(a15,a)')'../tmp/tmp_DFS_',nbchar(1:ISWD)
        CLOSE(ifl)
        CALL FPOPEN(IFL,IER,6,3,outfil)
        REWIND(ifl)
        CALL PRNNEW(ifl)
      endif

      RETURN
      END


C ********************* SURFCONF *********************
C Initiates a CFD analysis if (for the given time-step) there
C is surface conflation between the thermal and CFD domains.
C It is used for both `one-way' and `two-way' surface conflation.
C If 'adaptive conflation control' is active it performes a
C preliminary flow analysis and adjusts CFD simulation parameters
C prior to performing the final CFD analysis for the given time-step.
C Although CFD will calculate convection coefficients when there is
C 'one-way' surface conflation, these values will not be used
C by the thermal domain. If there is 'two-way' surface conflation,
C CFD-calculated convection coefficients are passed back, but the
C convection coding may or may not accept them: this decision is
C made by subroutine ASSESS. Calls CFMNGE each time step there
C is surface conflation.

      SUBROUTINE SURFCONF(ICOMP)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/BTIME/BTIMEP,BTIMEF
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ACCrec/IACC(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/calcdCFD/mICORcfd(MCOM),mHCcfd(MCOM)
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALPOL,CALLMA
      CHARACTER POLNAM*12
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
C       real headflux1,trunkflux1,larmflux1,rarmflux1,llegflux1
C       real rlegflux1,H2Oexp1,H2Oswt1,Qoccsens1,Qocclat1
C C       real COflux,Tsko_av1,Tco_av1,Tsk_av1
C       COMMON/ocflux1/headflux1(MNZ),trunkflux1(MNZ),larmflux1(MNZ),
C      &  rarmflux1(MNZ),llegflux1(MNZ),rlegflux1(MNZ),H2Oexp1(MNZ),
C      &  H2Oswt1(MNZ),Qoccsens1(MCOM),Qocclat1(MCOM)
C C         COflux,Tsko_av1,Tco_av1,Tsk_av1
C       REAL tavhead1,tavtrunk1,tavarml1,tavarmr1,tavlegl1,tavlegr1
C       REAL hrhead,hrtrunk,hrarml,hrarmr,hrlegl,hrlegr,HMETA2
C       integer cfdcheck, msegflag
C       COMMON/segtemp1/tavhead1(MNZ),tavtrunk1(MNZ),tavarml1(MNZ),
C      &  tavarmr1(MNZ),tavlegl1(MNZ),tavlegr1(MNZ),hrhead(MNZ),
C      &  hrtrunk(MNZ),hrarml(MNZ),hrarmr(MNZ),hrlegl(MNZ),hrlegr(MNZ),
C      &  cfdcheck(MNZ),msegflag(MNZ),HMETA2(MNZ)
C      REAL ic(31,4)
      CHARACTER LCFD*72
      character cfd3dascii*72
      character doit*248
      CHARACTER*72 outs
      character ICFDSTR*10,ICPSTR*10
      character louts*1500,loutsd*1500 

C Do not invoke CFD at first time step.
      IF(NSINC.gt.1) THEN

C Determine which CFD domain number (ie. icfd) the current building zone
C corresponds to, set common variable ICFD, then invoke CFD. Following the CFD 
C run, set the flag indicating that CFD has been run in `surface conflation' 
C mode and that there are CFD-calculated HCs for this zone for the current
C time step.
        ICP=ICOMP
        do 30 iconf=1,NCONF            
          if( icfdnd(iconf).eq.ICOMP )then
            ICFD=iconf
          endif
   30   continue
C Debug.
C        write(6,*) 'SURFCONF (ICP,ICOMP,icfd)',ICP,ICOMP,icfd

C Indicate CFD domain being processed.
C        if(NCONF.gt.1)then
          call INTSTR(ICFD,ICFDSTR,ICFDLN,IER)
          if(IER.ne.0)goto 123
          call INTSTR(ICP,ICPSTR,ICPLN,IER)
          if(IER.ne.0)goto 123   
          if(IHRF.eq.1) then 
            IDY=IDYF
          else
            IDY=IDYP
          endif
          call EDAYR(IDY,ID,IM)
          write(outs,'(5a,I2,a,I2,A,F5.2,A)')'*** CFD DOMAIN ',
     &        ICFDSTR(1:ICFDLN),' (zone ',ICPSTR(1:ICPLN),
     &        ') for day ',id,'/',im,' at time ',btimef,' ***'
          call edisp(IUOUT,outs)

C Initialise the thermophysical variables and some gridding data.
  123   CALL INICNT

C Check occupancy-linked blockages.
        call CFDOCHK()

C If the user has requested adaptive control over the conflation, then
C invoke the controller to assess and prepare for the CFD run (i.e. adaptive
C conflation control). Otherwise, just call CFD with the user's input settings
C (i.e. uncontrolled).
        if(abs(IFCFD(ICOMP)).ge.4.and.abs(IFCFD(ICOMP)).le.7)then
          CALL ADAPCONF
        endif

C Record ACC actions.
        IF(abs(IFCFD(ICOMP)).eq.4.or.abs(IFCFD(ICOMP)).eq.5)THEN
          write(IACC(icfd),*)
          write(IACC(icfd),*) 'Commencing adapted CFD simulation.'
          call edisp(IUOUT,'Commencing adapted CFD simulation.')

        ELSE
          CALL edisp(IUOUT,'Commencing CFD simulation.')

        ENDIF

        CALL CFMNGE(ICOMP,icfd)

C Examine the spacing of the next-to-wall grid points in terms of the
C dimensionless distance (y+). These results are used when conditional
C two-way adaptive conflation is active.
        CALL CHKSPC

C Set the flag to indicate that there are CFD calculated convection
C coefficients for the current time-step (thermal domain's convection routines
C decide what to do with the coefficients).
        mHCcfd(ICOMP) = 1
      
C Save solution information (IFCFD wil be -ve if past start-up period).
        if(IFCFD(ICP).lt.0)then
          call cfdlib

C Write out an ASCII file with CFD domain information as well as the current
C data as would have been written in cfdlib. Intended to be read by utility
C dfv that displays a 3D rendering of the current solution.
          write(cfd3dascii,'(a,i1,a,i4.4)') '../tmp/cfd3dascii_',
     &                                               icfd,'_',NSINC
          call efopseq(38,cfd3dascii,3,ier)
          write(38,'(a)') 'cfd3dascii'
          write(38,'(a,16i4)') 'head_1:',MCEL1D,ICFDSV,ICFD,NCOMP,
     &      (IEQSV(J),J=1,5+NCTM(ICFD))
          write(38,'(a,150i4)') 'head_2:',(abs(ICFAUX(I)),I=1,NCOMP)
          write(38,'(a,2i4,2f9.5)')'head_3:',ICFDYS,ICFDYF,CFTIMS,CFTIMF
          write(38,'(a,i4)') 'head_4:',NCTM(ICFD)
          write(38,'(a,i3,a,F7.4)') 'head_5: day ',IDYP,' at ',BTIMEF
          DO ICTM=1,NCTM(ICFD)
            write(38,'(2a)') 'POL: ',POLNAM(ICTM,ICFD)
          ENDDO
          write(38,'(a,i3)') 'zone:',ICOMP
          write(38,'(a,9i4)') 
     &      'all:',NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
          write(38,'(a,150f9.5)') 'XU:',(XU(I),I=1,NI)
          write(38,'(a,150f9.5)') 'YV:',(YV(I),I=1,NJ)
          write(38,'(a,150f9.5)') 'ZW:',(ZW(I),I=1,NK)
          do 10 K=2,NK
            do 101 J=2,NJ
              write(38,'(a,i2.2,a,i2.2)') 'Cell_KJ: ',K,' ',J
              if (IEQSV(1).eq.1) then
                write(38,'(a,150f9.5)') 'Uf:',(Uf(I,J,K),I=2,NI)
              endif
              if (IEQSV(2).eq.1) then
                write(38,'(a,150f9.5)') 'Vf:',(Vf(I,J,K),I=2,NI)
              endif
              if (IEQSV(3).eq.1) then
                write(38,'(a,150f9.5)') 'Wf:',(Wf(I,J,K),I=2,NI)
              endif
              if (IEQSV(4).eq.1) then
C                write(38,'(a,150f7.2)') 'Tf:',(Tf(I,J,K),I=2,NI)
                write(louts,'(150f7.2)') (Tf(I,J,K),I=2,NI)
                call SDELIM(louts,loutsd,'C',IW)
                write(38,'(2a)') 'Tf: ',loutsd(1:lnblnk(loutsd))
              endif
              if (IEQSV(5).eq.1) then  ! to allow occassional 1K seconds.
C                write(38,'(a,150f8.1)') 'AGEf: ',(AGEf(I,J,K),I=2,NI)
                write(louts,'(150f8.1)') (AGEf(I,J,K),I=2,NI)
                call SDELIM(louts,loutsd,'C',IW)
                write(38,'(2a)') 'AGEf: ',loutsd(1:lnblnk(loutsd))
              endif
              DO ICTM=1,NCTM(ICFD)
                if (IEQSV(5+ICTM).eq.1) then
                  write(38,'(a,150f9.5)') 'POL:',
     &              (POLCONCf(ICTM,I,J,K),I=2,NI)
                endif
              ENDDO      
 101        continue
 10       continue

C C Write the body and skin temperature for current time step.
C          if (cfdcheck(ICFD).GT.1.and.HMETA2(ICFD).LT.1) then
C            open(unit=92,file="initial_condition1.txt",status="old")
C            do i=1,25
C              read (92,*) (ic(i,j),j=1,4)
C              write(38,'( 6f11.5)') (ic(i,j),j=1,4)
C            end do
C            CLOSE ( UNIT=92 )
C            write(38,*) tavhead1(ICFD),tavtrunk1(ICFD),tavarml1(ICFD),
C     &        tavarmr1(ICFD),tavlegl1(ICFD),tavlegr1(ICFD),
C     &        Qoccsens1(ICP),Qocclat1(ICP)
C     &        ,Tsko_av1, Tco_av1, Tsk_av1
C          endif

C          write(38,*) tavhead2,tavtrunk2,tavarml2,tavarmr2, 
C    &       tavlegl2,tavlegr2,Qoccsens2,Qocclat2,
C    &       Tsko_av2, Tco_av2, Tsk_av2
C          endif

          write(38,'(a)') 'end_cfd3dascii'
          CLOSE ( UNIT=38 )

C At this point invoke the CFD domain viewer:
C dfv -file zone_dfs_file -actf ../tmp/cfd3dasii_file -range <min> <max>.
C Only need to call this once and thereafter inform the user to load
C the next timestep.
          if(ICFVIEW.eq.1)then
            if(DFVinvoked)then
              call usrmsg('Select the next timestep in the CFD viewer.'
     &                    ,' ','-')
            else
              write(doit,'(5a,2i4,a)')'dfv -file ',
     &          LCFD(icp)(1:lnblnk(LCFD(icp))),' -actf ',
     &          cfd3dascii(1:lnblnk(cfd3dascii)),' -range',ICFVIEWMINT,
     &          ICFVIEWMAXT,' & '
C              call lusrmsg('Visualising CFD results via',doit,'-')
              call runit(doit,'-')
              DFVinvoked=.true.  ! so not invoked in subsequent timesteps.
            endif
          endif
        endif
      ENDIF

      RETURN
      END


C ********************* readcfd3dascii *********************
C Read ascii information for one timestep with simulation
C day and timestep returned via ISIMDAY SIMTIMEF.

      SUBROUTINE readcfd3dascii(ICOMP,cfd3dascii,ISIMDAY,SIMTIMEF)
#include "building.h"
#include "cfd.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      CHARACTER POLNAM*12
      LOGICAL CALPOL,CALLMA
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      character cfd3dascii*72
      character loutstr*1000,WORD*32
      character outs*124
      real VAL

C Set unit number and initial values of ISIMDAY & SIMTIMEF.
      IER=0; ICTM=0; ISIMDAY=0; SIMTIMEF=0.0
      iunit=38
      CALL EFOPSEQ(iunit,cfd3dascii,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') '   cfd3dascii file ../tmp/',
     &    cfd3dascii(1:lnblnk(cfd3dascii)),
     &    ' could not be opened.'
        call edisp(IUOUT,outs)
        IER=1
        RETURN
      endif
      write(currentfile,'(a)') cfd3dascii(1:lnblnk(cfd3dascii))
      
  42  CALL STRIPC1K(iunit,LOUTSTR,0,ND,1,'cfd3dlines',IER)
      IF(IER.NE.0) goto 1002
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
      IF(IER.NE.0) goto 1002
      if(WORD(1:10).eq.'cfd3dascii')then
        continue
      elseif(WORD(1:14).eq.'end_cfd3dascii')then
        CLOSE ( UNIT=iunit )
        return
      elseif(WORD(1:7).eq.'head_1:')then

C Read head_1: MCEL1D,ICFDSV,(IEQSV(J),J=1,5+NCTM(ICFD)).
        CALL EGETWI(LOUTSTR,K,MCEL1D,0,0,'-','MCEL1D',IER)
        CALL EGETWI(LOUTSTR,K,ICFDSV,0,0,'-','ICFDSV',IER)
        CALL EGETWI(LOUTSTR,K,ICFD,0,0,'-','ICFD',IER)
        CALL EGETWI(LOUTSTR,K,NCOMP,0,0,'-','NCOMP',IER)
        DO J=1,5+NCTM(ICFD)
          CALL EGETWI(LOUTSTR,K,IEQSV(J),0,0,'-','IEQSV',IER)
        ENDDO
      elseif(WORD(1:7).eq.'head_2:')then

C Read head_2: (abs(ICFAUX(I)),I=1,NCOMP).
        DO I=1,NCOMP   ! where do we get NCOMP?
          CALL EGETWI(LOUTSTR,K,ICFAUX(I),0,0,'-','ICFAUX',IER)
        ENDDO

      elseif(WORD(1:7).eq.'head_3:')then

C Read head_3: ICFDYS,ICFDYF,CFTIMS,CFTIMF.
        CALL EGETWI(LOUTSTR,K,ICFDYS,0,0,'-','ICFDYS',IER)
        CALL EGETWI(LOUTSTR,K,ICFDYF,0,0,'-','ICFDYF',IER)
        CALL EGETWR(LOUTSTR,K,CFTIMS,0.,23.,'W','CFTIMS',IER)
        CALL EGETWR(LOUTSTR,K,CFTIMF,0.,24.,'W','CFTIMF',IER)

      elseif(WORD(1:7).eq.'head_4:')then

C Read head_4: NCTM(ICFD).
        CALL EGETWI(LOUTSTR,K,NCTM(ICFD),0,0,'-','NCTM(ICFD)',IER)

      elseif(WORD(1:7).eq.'head_5:')then

C Read head_5: ISIMDAY SIMTIMEF.
        K=12
        CALL EGETWI(LOUTSTR,K,ISIMDAY,0,0,'-','IDYP',IER)
        CALL EGETW(LOUTSTR,K,WORD,'W',' at ',IER)
        CALL EGETWR(LOUTSTR,K,SIMTIMEF,0.,24.,'W','CFTIMF',IER)

C Debug.
        write(6,*) 'for simulation day ',ISIMDAY,' & time ',SIMTIMEF

      elseif(WORD(1:4).eq.'POL:')then

C Read POLNAM: loop POLNAM(ICTM,ICFD).
        CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
        ICTM=ICTM+1
        write(POLNAM(ICTM,ICFD),'(a)') WORD(1:lnblnk(WORD))

      elseif(WORD(1:5).eq.'zone:')then

C Read zone.
        CALL EGETWI(LOUTSTR,K,ICOMP,0,0,'-','ICOMP',IER)

      elseif(WORD(1:4).eq.'all:')then

C Read all.
        CALL EGETWI(LOUTSTR,K,NI,0,0,'-','NI',IER)
        CALL EGETWI(LOUTSTR,K,NJ,0,0,'-','NJ',IER)
        CALL EGETWI(LOUTSTR,K,NK,0,0,'-','NK',IER)
        CALL EGETWI(LOUTSTR,K,NIM1,0,0,'-','NIM1',IER)
        CALL EGETWI(LOUTSTR,K,NJM1,0,0,'-','NJM1',IER)
        CALL EGETWI(LOUTSTR,K,NKM1,0,0,'-','NKM1',IER)
        CALL EGETWI(LOUTSTR,K,NIM2,0,0,'-','NIM2',IER)
        CALL EGETWI(LOUTSTR,K,NJM2,0,0,'-','NJM2',IER)
        CALL EGETWI(LOUTSTR,K,NKM2,0,0,'-','NKM2',IER)

      elseif(WORD(1:3).eq.'XU:')then

C Read XU.
        DO I=1,NI
          CALL EGETWR(LOUTSTR,K,XU(I),0.,24.,'W','XU(I)',IER)
        ENDDO

      elseif(WORD(1:3).eq.'YV:')then

C Read YV.
        DO I=1,NJ
          CALL EGETWR(LOUTSTR,K,YV(I),0.,24.,'W','YV(I)',IER)
        ENDDO

      elseif(WORD(1:3).eq.'ZW:')then

C Read ZW.
        DO I=1,NK
          CALL EGETWR(LOUTSTR,K,ZW(I),0.,24.,'W','ZW(I)',IER)
        ENDDO

      elseif(WORD(1:8).eq.'Cell_KJ:')then

C Read cells: get current index for K & J.
        ICTM=0  ! reset
        CALL EGETWI(LOUTSTR,K,KK,0,0,'-','KK',IER)
        CALL EGETWI(LOUTSTR,K,JJ,0,0,'-','JJ',IER)
        goto 42
      elseif(WORD(1:3).eq.'Uf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,24.,'W','Uf',IER)
          Uf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:3).eq.'Vf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,24.,'W','Vf',IER)
          Vf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:3).eq.'Wf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,24.,'W','Wf',IER)
          Wf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:3).eq.'Tf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,24.,'W','Tf',IER)
          Tf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:5).eq.'AGEf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,24.,'W','AGEf',IER)
          AGEf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:5).eq.'POL:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,24.,'W','POLCONCf',IER)
          ICTM=ICTM+1
          POLCONCf(ICTM,I,JJ,KK)=VAL
        ENDDO
      endif
      goto 42

C Errors for loutstr reads.
 1002 write(outs,'(3a)') 'readcfd3dascii: conversion error in...',
     &  LOUTSTR(1:50),'...'
      
      end

C ********************* cfdlibhead *********************
C Write header information to cfd results library.

      subroutine cfdlibhead
#include "building.h"
#include "cfd.h"

      common/outin/iuout,iuin,ieout
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked
      COMMON/ICFNOD/ICFD,ICP

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)

      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF

      CHARACTER outs*124, POLNAM*12

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALLMA,CALPOL

      integer, dimension(MCOM) :: domarr

C Depending on CFD results version, execute appropriate code.
C Version 1 has only momentum and temperature (deprecated).
C Version 2 adds local mean age of air and contaminants (deprecated).
C Version 3 reorganises the header structure to fully enable multiple
C CFD domains, and potentially reduces library size.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        goto 102
      elseif (ICFDSV.eq.3) then
        goto 103
      else
        call edisp(iuout,
     &    ' CFDLIBHEAD: unrecognised CFD results version.')
        goto 999
      endif

C *** VERSIONS 1 & 2 ***      

C Save record width as first data item and save level.
C Save level 1 - velocity and temperature information saved.
C Save level 2 - all active solutions saved (indexed in following fields).
C IEQSV - which equations have results saved:
C  IEQSV(1)=U momentum 
C  IEQSV(2)=V momentum 
C  IEQSV(3)=W momentum 
C  IEQSV(4)=Temperature
C  IEQSV(5)=Local mean age of air
C  IEQSV(5+MCTM)=contaminant concentration
C Indicate which equations are active=>results to save.

 102  do 10 I=1,7
        IEQSV(I)=0
 10   continue
      if (CALLU(ICFD)) IEQSV(1)=1
      if (CALLV(ICFD)) IEQSV(2)=1
      if (CALLW(ICFD)) IEQSV(3)=1
      if (CALLT(ICFD)) IEQSV(4)=1
      if (CALLMA(ICFD)) IEQSV(5)=1
      DO 111 ICTM=1,NCTM(ICFD)
        IF(CALPOL(ICTM,ICFD)) IEQSV(5+ICTM)=1
 111  CONTINUE
      IREC=1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) MCEL1D,ICFDSV,
     &                                      (IEQSV(J),J=1,5+NCTM(ICFD))

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',MCEL1D,ICFDSV,(IEQSV(J),J=1,5+NCTM(ICFD))

C Which zone(s) is CFD active in.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                (abs(ICFAUX(I)),I=1,NCOMP)

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',(abs(ICFAUX(I)),I=1,NCOMP)

C Write start and finish time: start day; finish day; start hour; finish hour.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) ICFDYS,ICFDYF,
     &           CFTIMS,CFTIMF

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',ICFDYS,ICFDYF,CFTIMS,CFTIMF

C Write number of contaminants and names
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCTM(ICFD)

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',NCTM(ICFD)

      DO 333 ICTM=1,NCTM(ICFD)
        IREC=IREC+1
        WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)POLNAM(ICTM,ICFD)

C Debug.
C        WRITE(6,*)POLNAM(ICTM,ICFD)

 333  CONTINUE

      goto 998

C *** VERSION 3 ***

C We no longer store the equations saved on the first line of the
C header for two reasons:
C 1. it depends on the number of contaminants, which makes the
C    read order of the library weird; and
C 2. it might be different for each domain.
C Record 1: record width, save level.
 103  IREC=1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)MCEL1D,ICFDSV

C Record 2: array of domain numbers for each zone.
      IREC=IREC+1
      do I=1,NCOMP
        domarr(I)=0
        do J=1,NCONF
          if (I.eq.icfdnd(J)) domarr(I)=J
        enddo
      enddo
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                (domarr(I),I=1,NCOMP)

C Record 3: CFD simulation period (start day, finish day, start
C               time, finish time).
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) ICFDYS,ICFDYF,
     &  CFTIMS,CFTIMF

      goto 998

C << ADD FURTHER CFD RESULT LIBRARY VERSIONS HERE >>      
  
C Remember current CFD results library record number.
 998  IRECPC=IREC
      goto 999

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIBHEAD: error at record',IREC,'.'
      call edisp(iuout,outs)
 999  return
      end


C ********************* cfdlibdom *********************
C Write domain information to cfd results library for each 
C CFD domain.

      subroutine cfdlibdom(ICOMP)
#include "building.h"
#include "cfd.h"

      common/outin/iuout,iuin,ieout
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      CHARACTER POLNAM*12
      LOGICAL CALPOL,CALLMA
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ICFNOD/ICFD,ICP
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)

      CHARACTER outs*124

C Depending on CFD results version, execute appropriate code.
C Version 1 has only momentum and temperature (deprecated).
C Version 2 adds local mean age of air and contaminants (deprecated).
C Version 3 reorganises the header structure to fully enable multiple
C CFD domains, and potentially reduces library size.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        goto 102
      elseif (ICFDSV.eq.3) then
        goto 103
      else
        call edisp(iuout,
     &    ' CFDLIBDOM: unrecognised CFD results version.')
        goto 999
      endif

C *** VERSION 1 & 2 ***

C Calculate record number to write to.
 102  IREC=5+NCTM(ICFD)+(4*(ICOMP-1))

C Debug.
C      write(6,*) 'saving to IREC ',IREC,' for zone ',ICOMP

C Calculate number of records for each domain time step (strictly this 
C could always be recalculated from the domain size but there is room 
C to store the data).
C IMULT is the number of metrics being saved (1 per record).
      if (ICFDSV.eq.1) then
        IMULT=4
      else
        IMULT=0
        do 10 I=1,5+NCTM(ICFD)
          IMULT=IMULT+IEQSV(I)
 10     continue
      endif
      IDRSZE=NJM1*NKM1*IMULT

C Write domain size.
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) IDRSZE,NIM2,NJM2,NKM2

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',IDRSZE,NIM2,NJM2,NKM2

C Write grid sizing information.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (XU(I),I=1,NI)

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',(XU(I),I=1,NI)

      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (YV(I),I=1,NJ)

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',(YV(I),I=1,NJ)

      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZW(I),I=1,NK)

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',(ZW(I),I=1,NK)

      GOTO 999

C *** VERSION 3 ***

C Calculate record number to write to.
 103  IREC=3+(8*(ICFD-1))

C Write number of contaminants.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCTM(ICFD)

C Write names of contaminants.
C If there are no contaminants, just write an "x".
C NOTE: It is theoretically possible for this to exceed the record width
C of the library, though this would require an extremely specialised use
C of the CFD with many contaminants. The risk is considered so minimal
C as to be negligible for the time being.
      IREC=IREC+1
      if (NCTM(ICFD).gt.0) then
        WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &    (POLNAM(I,ICFD),I=1,NCTM(ICFD))
      else
        WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)'x'
      endif
        
C Write equations saved.
      IMULT=0
      if (CALLU(ICFD)) then
        IEQSV(1)=1
        IMULT=IMULT+1
      else
        IEQSV(1)=0
      endif        
      if (CALLV(ICFD)) then
        IEQSV(2)=1
        IMULT=IMULT+1
      else
        IEQSV(2)=0
      endif       
      if (CALLW(ICFD)) then
        IEQSV(3)=1
        IMULT=IMULT+1
      else
        IEQSV(3)=0
      endif       
      if (CALLT(ICFD)) then
        IEQSV(4)=1
        IMULT=IMULT+1
      else
        IEQSV(4)=0
      endif       
      if (CALLMA(ICFD)) then
        IEQSV(5)=1
        IMULT=IMULT+1
      else
        IEQSV(5)=0
      endif       
      do ICTM=1,NCTM(ICFD)
        if(CALPOL(ICTM,ICFD)) then
          IEQSV(5+ICTM)=1
          IMULT=IMULT+1
        else
          IEQSV(5+ICTM)=0
        endif       
      enddo
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &  (IEQSV(I),I=1,5+NCTM(ICFD))

C Write domain size.
      IDRSZE=NJM1*NKM1*IMULT
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) IDRSZE,NIM2,NJM2,NKM2

C Write grid sizing information.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (XU(I),I=1,NI)

      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (YV(I),I=1,NJ)

      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZW(I),I=1,NK)

C Write grid origin vertices.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)IORG(ICFD),
     &  ixend(ICFD),iyend(ICFD),izend(ICFD)

      GOTO 999

 999  return

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIBDOM: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      end


C ********************* cfdlib *********************
C Write data to cfd results library.

      subroutine cfdlib
#include "building.h"
#include "cfd.h"

      common/outin/iuout,iuin,ieout
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/ICFNOD/ICFD,ICP
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      CHARACTER POLNAM*12
      LOGICAL CALPOL,CALLMA
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &  NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN

      CHARACTER outs*124

      real xU(ntcelx,ntcely,ntcelz),xV(ntcelx,ntcely,ntcelz),
     &     xW(ntcelx,ntcely,ntcelz),xT(ntcelx,ntcely,ntcelz),
     &     xAGE(ntcelx,ntcely,ntcelz),xPOL(MCTM,ntcelx,ntcely,ntcelz)
      integer xNI,xNJ,xNK

C If grid has been refined, average results to match original grid.
      if (NRFND.gt.0) then
        call RFNAVG(xU,xV,xW,xT,xAGE,xPOL,xNI,xNJ,xNK,ier)
        if (ier.ne.0) then
          call edisp(iuout,
     &      ' CFDLIB: cannot average results for refined grid.')
          goto 999
        endif
      else
        xU=Uf; xV=Vf; xW=Wf; xT=Tf; xAGE=AGEf; xPOL=POLCONCf
        xNI=NI; xNJ=NJ; xNK=NK
      endif

C Set current record (IRECPC).
C If the first CFD domain then IRECPC will be a header record number. If 
C this is the case then set to the first record past the header, otherwise 
C use the saves record.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        IST=5+NCTM(ICFD)+(4*(MCOM-1))
      elseif (ICFDSV.eq.3) then
        IST=4+(8*NCONF)
      else
        call edisp(iuout,
     &    ' CFDLIB: unrecognised CFD results version.')
        goto 999
      endif
      if (IRECPC.lt.IST) IRECPC=IST
      IREC=IRECPC

C Loop through all control volumes.
      do 10 K=2,xNK
        do 101 J=2,xNJ

C Write data to library. Adjust trace to file unit 6 if ntcelz>48.
          if (CALLU(ICFD)) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (xU(I,J,K),I=2,xNI)
          endif

          if (CALLV(ICFD)) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (xV(I,J,K),I=2,xNI)
          endif

          if (CALLW(ICFD)) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (xW(I,J,K),I=2,xNI)
          endif

          if (CALLT(ICFD)) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (xT(I,J,K),I=2,xNI)
          endif
                
          if (CALLMA(ICFD)) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (xAGE(I,J,K),I=2,xNI)
          endif
                
          DO 121 ICTM=1,NCTM(ICFD)
            if (CALPOL(ICTM,ICFD)) then
              IREC=IREC+1
              WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &          (xPOL(ICTM,I,J,K),I=2,xNI)
            endif
 121      CONTINUE      
 101    continue
 10   continue

C Remember current CFD results library record number.
      IRECPC=IREC

C Debug.
C      write(6,*) 'IRECPCend ',IRECPC

 999  return

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIB: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      end

C ********************* RFNAVG *********************
C Average results over grid cells that have been refined mid-simulation.
C This is done so that results saved to the results library match the
C grid geometry saved in the domain header.

C Uses commons as input.
C Returns averaged future time step arrays and array sizes.

      SUBROUTINE RFNAVG(avU,avV,avW,avT,avAGE,avPOL,avNI,avNJ,avNK,ier)
#include "building.h"
#include "cfd.h"

      real avU(ntcelx,ntcely,ntcelz),avV(ntcelx,ntcely,ntcelz),
     &     avW(ntcelx,ntcely,ntcelz),avT(ntcelx,ntcely,ntcelz),
     &     avAGE(ntcelx,ntcely,ntcelz),avPOL(MCTM,ntcelx,ntcely,ntcelz)
      integer avNI,avNJ,avNK,ier

      COMMON/ICFNOD/ICFD,ICP
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &  NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
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

      ier=0
      nnim1=NIM1; nnjm1=NJM1; nnkm1=NKM1

      do irfn=NRFND,1,-1

        icel=IRFND(ICFD,irfn,1)
        jcel=IRFND(ICFD,irfn,2)
        kcel=IRFND(ICFD,irfn,3)
        
        if (icel.gt.0) nnim1=nnim1-1
        if (jcel.gt.0) nnjm1=nnjm1-1
        if (kcel.gt.0) nnkm1=nnkm1-1

C Adjust state variable arrays.
        do 150 k=2,nnkm1
          do 150 j=2,nnjm1
            do 150 i=2,nnim1
              if (i.gt.icel .and. icel.gt.0) then
                ii=i+1
              else
                ii=i
              endif
              if (j.gt.jcel .and. jcel.gt.0) then
                jj=j+1
              else
                jj=j
              endif
              if (k.gt.kcel .and. kcel.gt.0) then
                kk=k+1
              else
                kk=k
              endif
              if (INCALU.and.i.ge.3) then
                if (i.eq.icel.and.j.eq.jcel.and.k.eq.kcel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i+1,j,k)+Uf(i,j+1,k)+
     &              Uf(i,j,k+1)+Uf(i+1,j+1,k)+Uf(i+1,j,k+1)+
     &              Uf(i,j+1,k+1)+Uf(i+1,j+1,k+1))/8.
                elseif (i.eq.icel.and.j.eq.jcel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i+1,j,k)+Uf(i,j+1,k)+
     &              Uf(i+1,j+1,k))/4.
                elseif (i.eq.icel.and.k.eq.kcel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i+1,j,k)+Uf(i,j,k+1)+
     &              Uf(i+1,j,k+1))/4.
                elseif (j.eq.jcel.and.k.eq.kcel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i,j+1,k)+Uf(i,j,k+1)+
     &              Uf(i,j+1,k+1))/4.
                elseif (i.eq.icel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i+1,j,k))/2.
                elseif (j.eq.jcel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i,j+1,k))/2.
                elseif (k.eq.kcel) then
                  avU(i,j,k)=(Uf(i,j,k)+Uf(i,j,k+1))/2.
                else
                  avU(i,j,k)=Uf(ii,jj,kk)
                endif
              endif  
              if (INCALV.and.j.ge.3) then
                if (i.eq.icel.and.j.eq.jcel.and.k.eq.kcel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i+1,j,k)+Vf(i,j+1,k)+
     &              Vf(i,j,k+1)+Vf(i+1,j+1,k)+Vf(i+1,j,k+1)+
     &              Vf(i,j+1,k+1)+Vf(i+1,j+1,k+1))/8.
                elseif (i.eq.icel.and.j.eq.jcel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i+1,j,k)+Vf(i,j+1,k)+
     &              Vf(i+1,j+1,k))/4.
                elseif (i.eq.icel.and.k.eq.kcel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i+1,j,k)+Vf(i,j,k+1)+
     &              Vf(i+1,j,k+1))/4.
                elseif (j.eq.jcel.and.k.eq.kcel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i,j+1,k)+Vf(i,j,k+1)+
     &              Vf(i,j+1,k+1))/4.
                elseif (i.eq.icel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i+1,j,k))/2.
                elseif (j.eq.jcel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i,j+1,k))/2.
                elseif (k.eq.kcel) then
                  avV(i,j,k)=(Vf(i,j,k)+Vf(i,j,k+1))/2.
                else
                  avV(i,j,k)=Vf(ii,jj,kk)
                endif
              endif
              if (INCALW.and.k.ge.3) then
                if (i.eq.icel.and.j.eq.jcel.and.k.eq.kcel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i+1,j,k)+Wf(i,j+1,k)+
     &              Wf(i,j,k+1)+Wf(i+1,j+1,k)+Wf(i+1,j,k+1)+
     &              Wf(i,j+1,k+1)+Wf(i+1,j+1,k+1))/8.
                elseif (i.eq.icel.and.j.eq.jcel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i+1,j,k)+Wf(i,j+1,k)+
     &              Wf(i+1,j+1,k))/4.
                elseif (i.eq.icel.and.k.eq.kcel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i+1,j,k)+Wf(i,j,k+1)+
     &              Wf(i+1,j,k+1))/4.
                elseif (j.eq.jcel.and.k.eq.kcel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i,j+1,k)+Wf(i,j,k+1)+
     &              Wf(i,j+1,k+1))/4.
                elseif (i.eq.icel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i+1,j,k))/2.
                elseif (j.eq.jcel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i,j+1,k))/2.
                elseif (k.eq.kcel) then
                  avW(i,j,k)=(Wf(i,j,k)+Wf(i,j,k+1))/2.
                else
                  avW(i,j,k)=Wf(ii,jj,kk)
                endif
              endif
              if (INCALT) then
                if (i.eq.icel.and.j.eq.jcel.and.k.eq.kcel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i+1,j,k)+Tf(i,j+1,k)+
     &              Tf(i,j,k+1)+Tf(i+1,j+1,k)+Tf(i+1,j,k+1)+
     &              Tf(i,j+1,k+1)+Tf(i+1,j+1,k+1))/8.
                elseif (i.eq.icel.and.j.eq.jcel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i+1,j,k)+Tf(i,j+1,k)+
     &              Tf(i+1,j+1,k))/4.
                elseif (i.eq.icel.and.k.eq.kcel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i+1,j,k)+Tf(i,j,k+1)+
     &              Tf(i+1,j,k+1))/4.
                elseif (j.eq.jcel.and.k.eq.kcel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i,j+1,k)+Tf(i,j,k+1)+
     &              Tf(i,j+1,k+1))/4.
                elseif (i.eq.icel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i+1,j,k))/2.
                elseif (j.eq.jcel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i,j+1,k))/2.
                elseif (k.eq.kcel) then
                  avT(i,j,k)=(Tf(i,j,k)+Tf(i,j,k+1))/2.
                else
                  avT(i,j,k)=Tf(ii,jj,kk)
                endif
              endif
              if (CALLMA(ICFD)) then
                if (i.eq.icel.and.j.eq.jcel.and.k.eq.kcel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i+1,j,k)+AGEf(i,j+1,k)+
     &              AGEf(i,j,k+1)+AGEf(i+1,j+1,k)+AGEf(i+1,j,k+1)+
     &              AGEf(i,j+1,k+1)+AGEf(i+1,j+1,k+1))/8.
                elseif (i.eq.icel.and.j.eq.jcel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i+1,j,k)+AGEf(i,j+1,k)+
     &              AGEf(i+1,j+1,k))/4.
                elseif (i.eq.icel.and.k.eq.kcel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i+1,j,k)+AGEf(i,j,k+1)+
     &              AGEf(i+1,j,k+1))/4.
                elseif (j.eq.jcel.and.k.eq.kcel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i,j+1,k)+AGEf(i,j,k+1)+
     &              AGEf(i,j+1,k+1))/4.
                elseif (i.eq.icel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i+1,j,k))/2.
                elseif (j.eq.jcel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i,j+1,k))/2.
                elseif (k.eq.kcel) then
                  avAGE(i,j,k)=(AGEf(i,j,k)+AGEf(i,j,k+1))/2.
                else
                  avAGE(i,j,k)=AGEf(ii,jj,kk)
                endif
              endif
              do ipol=1,NCTM(ICFD)
                if (CALPOL(ipol,ICFD)) then
                  if (i.eq.icel.and.j.eq.jcel.and.k.eq.kcel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i+1,j,k)+POLCONCf(ipol,i,j+1,k)+
     &                POLCONCf(ipol,i,j,k+1)+POLCONCf(ipol,i+1,j+1,k)+
     &                POLCONCf(ipol,i+1,j,k+1)+POLCONCf(ipol,i,j+1,k+1)+
     &                POLCONCf(ipol,i+1,j+1,k+1))/8.
                  elseif (i.eq.icel.and.j.eq.jcel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i+1,j,k)+POLCONCf(ipol,i,j+1,k)+
     &                POLCONCf(ipol,i+1,j+1,k))/4.
                  elseif (i.eq.icel.and.k.eq.kcel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i+1,j,k)+POLCONCf(ipol,i,j,k+1)+
     &                POLCONCf(ipol,i+1,j,k+1))/4.
                  elseif (j.eq.jcel.and.k.eq.kcel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i,j+1,k)+POLCONCf(ipol,i,j,k+1)+
     &                POLCONCf(ipol,i,j+1,k+1))/4.
                  elseif (i.eq.icel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i+1,j,k))/2.
                  elseif (j.eq.jcel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i,j+1,k))/2.
                  elseif (k.eq.kcel) then
                    avPOL(ipol,i,j,k)=(POLCONCf(ipol,i,j,k)+
     &                POLCONCf(ipol,i,j,k+1))/2.
                  else
                    avPOL(ipol,i,j,k)=POLCONCf(ipol,ii,jj,kk)
                  endif
                endif
              enddo
  150   continue
      enddo

      avNI=nnim1+1; avNJ=nnjm1+1; avNK=nnkm1+1

      return
      end


C ********************* CFDOCHK *********************
C Checks occupancy at the current time step.
C If occupants are not present, associated BCs are disabled.

      SUBROUTINE CFDOCHK

#include "building.h"
#include "cfd.h"

      common/dynamico/isdynamicocup(MCOM)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CALENI/NBDAYTYPE,NBCALDAYS(MDTY),ICALENDER(365)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,IDYNOW
      COMMON/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &  CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &  CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/CASGNSO/META1(MCOM,MDTY,MGTY,MGPER),
     &  NBMEN(MCOM,MDTY,MGTY,MGPER),NBWOM(MCOM,MDTY,MGTY,MGPER),
     &  NBCHI(MCOM,MDTY,MGTY,MGPER),CONCC(MCOM,MDTY,MGTY,MGPER),
     &  RADCC(MCOM,MDTY,MGTY,MGPER),CLOTH(MCOM,MDTY,MGTY,MGPER)
      REAL META1,NBMEN,NBWOM,NBCHI
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &  JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &  VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &  VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &  VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),VOLPol(MCTM,MNVLS,MNZ)
      COMMON/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &  BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF

C If there are no dynamic occupants, just activate all BCs.
      if (isdynamicocup(ICP).eq.0) then
        do iv=1,NVOL(ICFD)
          BCOFF(iv)=.false.
        enddo
        RETURN
      endif

C Get current day type.
      idaytyp = ICALENDER(IDYNOW)

C Assume casual gain type 1 is occupants.
      ign = 1

C Determine the current period.
      iper = NCGPER(ICP,idaytyp,ign)
      do i=1,NCGPER(ICP,idaytyp,ign)
        if (BTIMEF.le.TCGS(ICP,idaytyp,ign,i+1).and.
     &      BTIMEF.gt.TCGS(ICP,idaytyp,ign,i)) then
          iper = i
          exit
        endif
      enddo

C Check number of men, women and children.
      inbmen=NINT(NBMEN(ICP,idaytyp,ign,iper))
      inbwom=NINT(NBWOM(ICP,idaytyp,ign,iper))
      inbchi=NINT(NBCHI(ICP,idaytyp,ign,iper))

C Loop through occupant-linked BCs, and disable any that are not
C present.
      ibmen=0; ibwom=0; ibchi=0
      ismen=0; iswom=0; ischi=0
      do iv=1,NVOL(ICFD)
        BCOFF(iv)=.false.
        if (IVTYPE(iv,ICFD).eq.35) then
          if (BLKSURF(iv,ICFD,2)(1:10).eq.'whole_body' .or. 
     &        BLKSURF(iv,ICFD,2)(1:5).eq.'trunk') then
            if (BLKSURF(iv,ICFD,1)(1:3).eq.'man') then
              ibmen=ibmen+1
              if (ibmen.gt.inbmen) BCOFF(iv)=.true.
            elseif (BLKSURF(iv,ICFD,1)(1:5).eq.'woman') then
              ibwom=ibwom+1
              if (ibwom.gt.inbwom) BCOFF(iv)=.true.
            elseif (BLKSURF(iv,ICFD,1)(1:5).eq.'child') then
              ibchi=ibchi+1
              if (ibchi.gt.inbchi) BCOFF(iv)=.true.
            endif
          endif
        elseif (IVTYPE(iv,ICFD).eq.20) then
          if (VCSURF(iv,ICFD)(1:3).eq.'man') then
            ismen=ismen+1
            if (ismen.gt.inbmen) BCOFF(iv)=.true.
          elseif (VCSURF(iv,ICFD)(1:5).eq.'woman') then
            iswom=iswom+1
            if (iswom.gt.inbwom) BCOFF(iv)=.true.
          elseif (VCSURF(iv,ICFD)(1:5).eq.'child') then
            ischi=ischi+1
            if (ischi.gt.inbchi) BCOFF(iv)=.true.
          endif
        endif

C Debug.
        if (BCOFF(iv)) write(6,*)'BC ',VOLNAME(iv,ICFD),
     &    ' disabled @ ',IDYNOW,BTIMEF
      enddo

      RETURN
      END




