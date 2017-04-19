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

C Contains:
C  CFMNGE - Controls CFD calcs and results output when BSim and CFD are conflated.
C  SURFCONF - Calls CFMNGE each time step there is surface conflation.
C  CFDLIBHEAD - Write header information to cfd results file.
C  cfdlibdom - Write information describing the CFD domain model to results file
C  cfdlib - Write results of CFD simulation to results file.

C ********************* CFMNGE *********************
C CFMNGE - Manages CFD calcs and results output when BSim and CFD are conflated.
      SUBROUTINE CFMNGE(ICOMP,IND)
#include "cfd.h"
#include "building.h"
#include "geometry.h"

      common/outin/iuout,iuin
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfcall/icfcal(MNZ)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/ICFCHN/ICFMON(MNZ),ICFTMP,ICFLIB

      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)
      COMMON/ICFNOD/ICFD,ICP
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      common/cfdini/INITFL(MNZ)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ)
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
      CHARACTER*72 TITLE,outfil,CFTRFL,LPHI,LCFD,outs
      LOGICAL INITFL,CONVER
      integer ACCON,CFD2BLD,CFD2MFS
      real TaCFD
C MITMFS is the max number of times cfd will be invoked using mfs 
C determined boundary conditions at one time step to attain mutual
C cfd-mfs convergence.
      PARAMETER(MITMFS=20)

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
C Check if we are within trace period.
        if (NSINC.ge.ITC.and.NSINC.le.ITCF) then
C If ACC is active, only print header and increment trace counter on
C gopher run.
          if((abs(IBLACT).ge.4).and.(abs(IBLACT).le.5))then
            if (gophrun) then
              call edisp(ITU,' ')
              write(outs,'(a,i5)')' CFMNGE/MFSCORR: Trace output ',ICNT
              call edisp(ITU,outs)
              ICNT=ICNT+1
            endif
          else
            call edisp(ITU,' ')
            write(outs,'(a,i5)')' CFMNGE/MFSCORR: Trace output ',ICNT
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
C Debug
c      write(6,*)'icfcal=',icfcal
        TTRACK(ICP)=icfcal(ICFD)
C Debug
C      write(6,*) 'CFMNGE (ICP,ICOMP,IND[icfd])',ICP,ICOMP,IND

C If this is the first time CFD has been called, initialize surface convection
C to zero.
C If mfs conflation is active then the following code needs to run every
C time this subroutine is called
      IF(icfcal(ICFD).le.1)then
        DO 10 I=1,NCONST(ICP)
          QCFDF(ICP,I)=0.0
  10    CONTINUE
        TINIT(ICFD)=TFA(ICP)

C Initialize variable fields. Either read initial values from file saved
C from previous solution (if user specifies this option), or initialize all
C field variables to values specified in CFD input file.
        IF(INITFL(ICFD))then
          ifl=ICFTMP
          ier=0
          call INTSTR(ICFD,nbchar,ISWD,IER)
          write(outfil,'(a8,a)')'tmp_DFS_',nbchar(1:ISWD)
          CLOSE(ifl)
          CALL FPOPEN(IFL,IER,5,1,outfil)
          REWIND(ifl)
          CALL REAOLD(ifl)
        ELSE
          CALL INIT
        ENDIF
      ELSE
      
C CFD has been called a previous time step. Initial values for this time step
C taken as final solution achieved at previous time step. Read previous solution.
        ifl=ICFTMP
        ier=0
        call INTSTR(ICFD,nbchar,ISWD,IER)
        write(outfil,'(a8,a)')'tmp_DFS_',nbchar(1:ISWD)
        ier=0
        CALL FPOPEN(IFL,IER,5,1,outfil)
        REWIND(ifl)
        CALL REAOLD(ifl)
      ENDIF

C Calculate geometrical quantities.
      CALL GRID(IER)

C Save domain geometry data to results library.
      if (icfcal(ICFD).eq.1) call cfdlibdom(ICOMP)

C Set up boundary conditions which are independent of the airflow.
      CALL INDBND

C Map BSim thermal conditions onto CFD solid boundaries.
      CALL BS2CFDSB

C Map BSim thermal conditions to CFD blockages.
      CALL BS2CFDBLK

C Calculate areas of solid boundaries.
      CALL CFAREA(NCONST(ICP))

C Invoke CFD simulation.

C This line, if uncommented, will force preliminary simulations to
C use one way CFD-MFS conflation.
c      if(((abs(IBLACT).ge.4).and.(abs(IBLACT).le.5)).and.gophrun)goto 255

C Activate mfs-cfd coupling via MFBNDR if airflow network is defined.
      IF(ICFMFS(ICFD).EQ.1)THEN
        IF(NCONF.GT.1)THEN
          if(CFD2MFS.eq.2)then
C If there is more than one CFD domain, 
C impose dry MFS calculated flow rates directly onto CFD (one way conflation).
C Unless the user has already specified one way conflation, inform them.
            write(outs,'(a,a)')'Multiple CFD domains detected, ',
     &                      'deactivating CFD-MFS iterative solution.'
            call edisp(iuout,outs)
            CFD2MFS=1
          endif
          GOTO 255
        ELSEIF(NCONF.EQ.1)THEN
C Only one CFD domain.
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
     &      'CFMNGE: Error configuring mfs-cfd conflation')
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
        write(ITRC,'(a)')'*** DRY MFS RESULTS (before iteration) ***'
        write(ITRC,'(a)')' '
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
     &      '*** CONFLATED MFS RESULTS (at iteration ',LKTR,') ***'
          write(ITRC,'(a)')' '
        endif
        CALL MFBNDR(IPOK)
        IF(IPOK.EQ.0)THEN
          write(outs,'(A,I4,A)')'Got mfs-cfd mutual convergence in '
     &                         ,lktr,' runs'
          call edisp(iuout,outs)
          CALL MFBNDR(0)
          GOTO 257
        ELSE
          IF(LKTR.GT.MITMFS)THEN
            write(outs,'(A,I4,A)')
     &        'Could not get mfs-cfd convergence after '
     &        ,lktr,' runs'
            call edisp(iuout,outs)
            CALL MFBNDR(0)
            GOTO 257
          ENDIF
          LKTR=LKTR+1
          GOTO 256
        ENDIF
      ELSE
        outs='standalone did not converge,going to next timestep'
        call edisp(iuout,outs)
        CALL MFBNDR(0)
        GOTO 257
      ENDIF
      GOTO 258

 255  CALL MFBNDR(1)
C Trace output.
      if (ITRC.gt.0) then
        write(ITRC,'(a)')'*** MFS RESULTS (imposed on CFD) ***'
        write(ITRC,'(a)')' '
        CALL MFOUTP(ITRC)
      endif
      CALL MFBNDR(2)
      CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)
      CALL MFBNDR(0)

      GOTO 258
C CFD-MFS trace output delimeters.
 257  if (ITRC.gt.0) then
        write(ITRC,'(a)')' '
        if (gophrun) then
          write(ITRC,'(a)')'*** PRELIMINARY SIMULATION DONE ***'
        else
          write(ITRC,'(a)')'*** TIME STEP DONE ***'
        endif
        write(ITRC,'(a)')' '
      endif

C Convection coefficients.
 258  DO 100 I=1,NCONST(ICP)
        icon=IZSTOCN(icp,i)
        SFNAM(I)=SSNAME(icon)
 100  CONTINUE

C Surface conflation:
      IF( ABS(IBLACT).EQ.1 .OR.
     &    (ABS(IBLACT).GE.4.and.ABS(IBLACT).LE.7) )THEN
        TAIR = TFA(ICP)
      ENDIF

C Integrated thermal conflation:
      IF(ABS(IBLACT).EQ.2) THEN
        icTREF(1,ICFD) = 2
        CALL TREFBC(1,TAIR)
        TFA(ICP) = TAIR
      ENDIF

      IF( ABS(IBLACT).EQ.1 .OR.
     &    (ABS(IBLACT).GE.4.and.ABS(IBLACT).LE.7) )THEN
        TFA(ICP) = TAIR  
      ENDIF
C Calculate convection coefficients and assign to temporary array. Adaptive
C conflation routine will decide whether to use these in zone thermal matrix.
      CALL CFCONV(TAIR,HC,SFNAM,NCONST(ICP))
      DO 200 I=1,NCONST(ICP)
        HCICFD(ICP,I)=HC(I)
 200  CONTINUE

C Write calculated variable fields to be used in the next time-step.
      if(.not.gophrun)then
        ifl=icftmp
        call INTSTR(ICFD,nbchar,ISWD,IER)
        write(outfil,'(a8,a)')'tmp_DFS_',nbchar(1:ISWD)
        CLOSE(ifl)
        CALL FPOPEN(IFL,IER,6,3,outfil)
        REWIND(ifl)
        CALL PRNNEW(ifl)
      endif

      RETURN
      END


C ********************* SURFCONF *********************
C SURFCONF - Calls CFMNGE each time step there is surface conflation.

      SUBROUTINE SURFCONF(ICOMP)
#include "cfd.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/ACCrec/IACC(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/SSOinit/areaSSO
      COMMON/calcdCFD/mICORcfd(MCOM),mHCcfd(MCOM)

C To support writing ASCII file.
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX,URFC(MCTM)
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

      CHARACTER POLNAM*12
      character cfd3dascii*72

      LOGICAL areaSSO
      CHARACTER*72 LCFD,outs
      character ICFDSTR*10,ICPSTR*10

C This subroutine initiates a CFD analysis if (for the given time-step) there
C is surface conflation between the thermal and CFD domains.
C It is used for both `one-way' and `two-way' surface conflation. If `adaptive
C conflation control' is active it performes a preliminary flow analysis and
C adjusts CFD simulation parameters prior to performing the final CFD analysis
C for the given time-step.
C Although CFD will calculate convection coefficients when there is `one-way'
C surface conflation, these values will not be used by the thermal domain.
C If there is `two-way' surface conflation, CFD-calculated convection
C coefficients are passed back, but the convection coding may or may not accept
C them: this decision is made by subroutine ASSESS.

C Don't invoke CFD if this is the first time step.
      IF(NSINC.gt.1) THEN

C Determine which CFD domain number (ie. icfd) the current building zone
C corresponds to, set common variable ICFD, then invoke CFD. Following the CFD 
C run, set the flag indicating that CFD has been run in `surface conflation' 
C mode and that there are CFD-calculated HCs for this zone for the current time 
C step.
        ICP=ICOMP
        do 30 iconf=1,NCONF            
          if( icfdnd(iconf).eq.ICOMP )then
            ICFD=iconf
          endif
   30   continue
C Debug.
C        write(6,*) 'SURFCONF (ICP,ICOMP,icfd)',ICP,ICOMP,icfd

C If more than one CFD domain, inform the user which one it is simulating.
        if(NCONF.gt.1)then
          call INTSTR(ICFD,ICFDSTR,ICFDLN,IER)
          if(IER.ne.0)goto 123
          call INTSTR(ICP,ICPSTR,ICPLN,IER)
          if(IER.ne.0)goto 123       
          write(outs,'(5a)')'*** CFD DOMAIN ',ICFDSTR(1:ICFDLN),
     &        ' (representing building zone ',ICPSTR(1:ICPLN),') ***'
          call edisp(IUOUT,outs)
        endif

C Initialize the thermophysical variables and some gridding data.
  123   CALL INICNT     

C Flag indicating that CFD `small opening' areas have not yet been calculated
C for this domain.
        areaSSO = .FALSE.

C If the user has requested adaptive control over the conflation, then
C invoke the controller to assess and prepare for the CFD run (i.e. adaptive
C conflation control). Otherwise, just call CFD with the user's input settings
C (i.e. uncontrolled).
        if(abs(IFCFD(ICOMP)).ge.4.and.abs(IFCFD(ICOMP)).le.7)then
          CALL ADAPCONF
        endif

C Record ACC actions (begin).
        IF(abs(IFCFD(ICOMP)).eq.4.or.abs(IFCFD(ICOMP)).eq.5)THEN
          write(IACC(icfd),*)
          write(IACC(icfd),*) 'Commencing final CFD simulation.'
C Record ACC actions (end).
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
      
C Save solution information (IFCFD wil be negative if past start-up period).
C Calculate current simulation timestep. - possibly use to synchornise results.
        write(6,*) 'about to call cfdlib with ifcfd is',IFCFD(ICP)
        if(IFCFD(ICP).lt.0)then
          call cfdlib

C Write out an ASCII file with CFD domain information as well as the current
C data as would have been written in cfdlib. Intended to be read by utility
C that displays a 3D rendering of the current solution.
C << Will need to be controlled in some way so only invoked as required. >>
          write(cfd3dascii,'(a,i4.4)') 'cfd3dascii_',NSINC
          write(6,'(2a)') 'the file is ',cfd3dascii
          call efopseq(38,cfd3dascii,4,ier)
          write(38,'(a)') 'cfd3dascii'
          write(38,'(a,16i4)') 'head_1:',MCEL1D,ICFDSV,ICFD,NCOMP,
     &      (IEQSV(J),J=1,5+NCTM)
          write(38,'(a,50i4)') 'head_2:',(abs(ICFAUX(I)),I=1,NCOMP)
          write(38,'(a,2i4,2f9.5)')'head_3:',ICFDYS,ICFDYF,CFTIMS,CFTIMF
          write(38,'(a,i4)') 'head_4:',NCTM
          DO ICTM=1,NCTM
            write(38,'(2a)') 'POL: ',POLNAM(ICTM,ICFD)
          ENDDO
          write(38,'(a,i3)') 'zone:',ICOMP
          write(38,'(a,9i4)') 
     &      'all:',NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
          write(38,'(a,50f9.5)') 'XU:',(XU(I),I=1,NI)
          write(38,'(a,50f9.5)') 'YV:',(YV(I),I=1,NJ)
          write(38,'(a,50f9.5)') 'ZW:',(ZW(I),I=1,NK)
          do 10 K=2,NK
            do 101 J=2,NJ
              write(38,'(a,i2.2,a,i2.2)') 'Cell_KJ: ',K,' ',J
              if (IEQSV(1).eq.1) then
                write(38,'(a,50f9.5)') 'Uf:',(Uf(I,J,K),I=2,NI)
              endif
              if (IEQSV(2).eq.1) then
                write(38,'(a,50f9.5)') 'Vf:',(Vf(I,J,K),I=2,NI)
              endif
              if (IEQSV(3).eq.1) then
                write(38,'(a,50f9.5)') 'Wf:',(Wf(I,J,K),I=2,NI)
              endif
              if (IEQSV(4).eq.1) then
                write(38,'(a,50f9.5)') 'Tf:',(Tf(I,J,K),I=2,NI)
              endif
              if (IEQSV(5).eq.1) then
                write(38,'(a,50f9.5)') 'AGEf:',(AGEf(I,J,K),I=2,NI)
              endif
              DO ICTM=1,NCTM
                if (IEQSV(5+ICTM).eq.1) then
                  write(38,'(a,50f9.5)') 'POL:',
     &              (POLCONCf(ICTM,I,J,K),I=2,NI)
                endif
              ENDDO      
 101        continue
 10       continue
          write(38,'(a)') 'end_cfd3dascii'
          CLOSE ( UNIT=38 )
        endif

      ENDIF

      RETURN
      END

C ********************* readcfd3dascii *********************
C readcfd3dascii - read ascii information from one timestep.
      SUBROUTINE readcfd3dascii(ICOMP,cfd3dascii)
#include "building.h"
#include "cfd.h"

C espriou.h provides currentfile.
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/ICFNOD/ICFD,ICP

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX,URFC(MCTM)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM)
      
C NCDOM - number of CFD domains
C ICDOM - CFD domain selected for output
C ICFDZ - thermal zone associated with each CFD domain
      COMMON/cfddoms/NCDOM,ICDOM,ICFDZ(MNZ)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124

      LOGICAL CALPOL,CALLMA,dll
      CHARACTER LCFD*72,POLNAM*12
      character cfd3dascii*72
      character loutstr*1000,WORD*32
      character outs*124
      real VAL

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='readcfd3dascii'  ! set for cfiles

C Set unit number.
      IER=0; ICTM=0
      iunit=38
      CALL EFOPSEQ(iunit,cfd3dascii,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'cfd3dascii file ',
     &    cfd3dascii(1:lnblnk(cfd3dascii)),
     &    ' could not be opened.'
        call isadll(dll)
        if(dll)then
          dllsubr='readcfd3dascii'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(ITRU,outs)
          IER=1
          RETURN
        endif
      ENDIF
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

C Read head_1: MCEL1D,ICFDSV,(IEQSV(J),J=1,5+NCTM)
        CALL EGETWI(LOUTSTR,K,MCEL1D,0,0,'-','MCEL1D',IER)
        CALL EGETWI(LOUTSTR,K,ICFDSV,0,0,'-','ICFDSV',IER)
        CALL EGETWI(LOUTSTR,K,ICFD,0,0,'-','ICFD',IER)
        CALL EGETWI(LOUTSTR,K,NCOMP,0,0,'-','NCOMP',IER)
        DO J=1,5+NCTM
          CALL EGETWI(LOUTSTR,K,IEQSV(J),0,0,'-','IEQSV',IER)
        ENDDO
      elseif(WORD(1:7).eq.'head_2:')then
	
C Read head_2: (abs(ICFAUX(I)),I=1,NCOMP)
        DO I=1,NCOMP   ! where do we get NCOMP?
          CALL EGETWI(LOUTSTR,K,ICFAUX(I),0,0,'-','ICFAUX',IER)
        ENDDO

      elseif(WORD(1:7).eq.'head_3:')then

C Read head_3: ICFDYS,ICFDYF,CFTIMS,CFTIMF
        CALL EGETWI(LOUTSTR,K,ICFDYS,0,0,'-','ICFDYS',IER)
        CALL EGETWI(LOUTSTR,K,ICFDYF,0,0,'-','ICFDYF',IER)
        CALL EGETWR(LOUTSTR,K,CFTIMS,0.,23.,'W','CFTIMS',IER)
        CALL EGETWR(LOUTSTR,K,CFTIMF,0.,24.,'W','CFTIMF',IER)

      elseif(WORD(1:7).eq.'head_4:')then

C Read head_4: NCTM
        CALL EGETWI(LOUTSTR,K,NCTM,0,0,'-','NCTM',IER)

      elseif(WORD(1:4).eq.'POL:')then

C Read POLNAM: loop POLNAM(ICTM,ICFD)
        CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
        ICTM=ICTM+1
        write(POLNAM(ICTM,ICFD),'(a)') WORD(1:lnblnk(WORD))

      elseif(WORD(1:5).eq.'zone:')then

C Read zone:
        CALL EGETWI(LOUTSTR,K,ICOMP,0,0,'-','ICOMP',IER)

      elseif(WORD(1:4).eq.'all:')then

C Read all:
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

C Read XU:
        DO I=1,NI
          CALL EGETWR(LOUTSTR,K,XU(I),0.,24.,'W','XU(I)',IER)
        ENDDO

      elseif(WORD(1:3).eq.'YV:')then

C Read YV:
        DO I=1,NJ
          CALL EGETWR(LOUTSTR,K,YV(I),0.,24.,'W','YV(I)',IER)
        ENDDO

      elseif(WORD(1:3).eq.'ZW:')then

C Read ZW:
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
C cfdlibhead - write header information to cfd results library.
      subroutine cfdlibhead
#include "cfd.h"
#include "building.h"

      common/outin/iuout,iuin
      COMMON/ICFCHN/ICFMON(MNZ),ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM)
      COMMON/ICFNOD/ICFD,ICP

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX,URFC(MCTM)

      CHARACTER outs*124, POLNAM*12

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALLMA,CALPOL

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
      do 10 I=1,7
        IEQSV(I)=0
 10   continue
      if (CALLU(ICFD)) IEQSV(1)=1
      if (CALLV(ICFD)) IEQSV(2)=1
      if (CALLW(ICFD)) IEQSV(3)=1
      if (CALLT(ICFD)) IEQSV(4)=1
      if (CALLMA(ICFD)) IEQSV(5)=1
      DO 111 ICTM=1,NCTM
        IF(CALPOL(ICTM,ICFD)) IEQSV(5+ICTM)=1
 111  CONTINUE
      IREC=1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) MCEL1D,ICFDSV,
     &                                             (IEQSV(J),J=1,5+NCTM)

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',MCEL1D,ICFDSV,(IEQSV(J),J=1,5+NCTM)

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
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCTM

C Debug.
C      WRITE(6,*) ICFLIB,IREC,':',NCTM

      DO 333 ICTM=1,NCTM
        IREC=IREC+1
        WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)POLNAM(ICTM,ICFD)

C Debug.
C        WRITE(6,*)POLNAM(ICTM,ICFD)

 333  CONTINUE
  
C Remember current CFD res lib record number.
      IRECPC=IREC
      return

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIBHEAD: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      end


C ********************* cfdlibdom *********************
C cfdlibdom - write domain information to cfd results library for each 
C CFD domain.
C This routine will work for multiple CFD domains in a problem.

      subroutine cfdlibdom(ICOMP)
#include "cfd.h"

      common/outin/iuout,iuin
      COMMON/ICFCHN/ICFMON(MNZ),ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX,URFC(MCTM)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

      CHARACTER outs*124, POLNAM*12
      LOGICAL CALPOL,CALLMA

C Calculate record number to write to.
      IREC=5+NCTM+(4*(ICOMP-1))

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
        do 10 I=1,5+NCTM
          IMULT=IMULT+IEQSV(I)
 10     continue
      endif
      IDRSZE=NJM1*NKM1*IMULT

C Write domain size and start and finish time step increment.
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

      return

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIBDOM: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      end


C ********************* cfdlib *********************
C cfdlib - write data to cfd results library.

      subroutine cfdlib
#include "building.h"
#include "cfd.h"

      common/outin/iuout,iuin
      COMMON/ICFCHN/ICFMON(MNZ),ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX,URFC(MCTM)
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

      CHARACTER outs*124, POLNAM*12
      LOGICAL CALPOL,CALLMA

C Set current record (IRECPC).
C If the first CFD domain then IRECPC will be a header record number.  If 
C this is the case then set to the first recore past the header, otherwise 
C use the saves record.
      IST=5+NCTM+(4*(MCOM-1))
      if (IRECPC.lt.IST) IRECPC=IST
      IREC=IRECPC

C Debug.
C      write(6,*) 'IRECPCstart ',IRECPC

C Loop through all control volumes.
      do 10 K=2,NK
        do 101 J=2,NJ

C Write data to library. Adjust trace to file unit 6 if ntcelz>48
          if (IEQSV(1).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Uf(I,J,K),I=2,NI)

C Debug.
C            write(6,'(i3,48f9.5)') IREC,(Uf(I,J,K),I=2,NI)

          endif

          if (IEQSV(2).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Vf(I,J,K),I=2,NI)

C Debug.
C          write(6,'(i3,48f9.5)') IREC,(Vf(I,J,K),I=2,NI)

          endif

          if (IEQSV(3).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Wf(I,J,K),I=2,NI)

C Debug.
C          write(6,'(i3,48f9.5)') IREC,(Wf(I,J,K),I=2,NI)

          endif

          if (IEQSV(4).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Tf(I,J,K),I=2,NI)

C Debug.
C          write(6,'(i3,48f9.5)') IREC,(Tf(I,J,K),I=2,NI)

          endif
                
          if (IEQSV(5).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (AGEf(I,J,K),I=2,NI)

C Debug
C          write(6,'(i3,48f9.5)') IREC,(AGEf(I,J,K),I=2,NI)

          endif
                
          DO 121 ICTM=1,NCTM
            if (IEQSV(5+ICTM).eq.1) then
              IREC=IREC+1
              WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                     (POLCONCf(ICTM,I,J,K),I=2,NI)

C Debug
C              write(6,'(i3,48f9.5)') IREC,(POLCONCf(ICTM,I,J,K),I=2,NI)

            endif
 121      CONTINUE      
 101    continue
 10   continue

C Remember current CFD res lib record number.
      IRECPC=IREC

C Debug.
C      write(6,*) 'IRECPCend ',IRECPC

      return

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIB: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      end

