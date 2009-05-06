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

      common/outin/iuout,iuin
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfcall/icfcal(MNZ)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB

      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)
      COMMON/ICFNOD/ICFD,ICP
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
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/SOLVER/ILISOL
      COMMON/GGDH/ GENB1(ntcelx,ntcely,ntcelz),GGDH
      
C Parameter "Nostop" gt. 0 overrides "non-convergence" control. Use with care.
C MITMFS is the max number of times cfd will be invoked using mfs 
C determined boundary conditions at one time step to attain mutual
C cfd-mfs convergence.
      COMMON/MFS/IMFACT,NOSTOP,MITMFS

      DIMENSION HC(MNSBZ),SFNAM(MNSBZ)

      CHARACTER NBCHAR*10,SFNAM*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER*72 TITLE,outfil,CFTRFL,LPHI,LCFD,out
      LOGICAL INITFL,CONVER,GGDH

C      PARAMETER(MITMFS=20)

      ICFD=IND
      ICP=ICOMP
      IMFACT=0
      IBLACT=IFCFD(ICP)
      TSTEP=3600.0/float(ntstep)
      LKTR=1

C Hardwire version of LISOLV to use.
      ILISOL=3
      GGDH=.false.

C Increment counter that keeps track of how many times CFD has been invoked
C for this zone.
      icfcal(ICFD)=icfcal(ICFD)+1

C If this is the first time CFD has been called, initialize surface convection
C to zero.
C If mfs conflation is active then the following code needs to run every
C time this subroutine is called
      IF(icfcal(ICFD).eq.1)then
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
          call INTSTR(ICP,nbchar,ISWD,IER)
          write(outfil,'(a,a)')'tmp_DFS_',nbchar(1:ISWD)
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
        call INTSTR(ICP,nbchar,ISWD,IER)
        write(outfil,'(a,a)') 'tmp_DFS_',nbchar(1:ISWD)
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
      ifl=ICFMON
      if (icfcal(ICFD).eq.1) call efopseq(ifl,CFTRFL(ICFD),3,ier)

C Activate mfs-cfd coupling via MFBNDR if airflow network is defined
      IF(ICFMFS(ICFD).EQ.1)THEN
        IF(LKTR.EQ.1)CALL MFBNDR(1)
 256    CALL MFBNDR(2)      
        CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)
        IF(CONVER)THEN
          IPOK=3
          CALL MFBNDR(IPOK)
          IF(IPOK.EQ.0)THEN
            write(out,'(A,I4,A)')'Got mfs-cfd mutual convergence in '
     &                           ,lktr,' runs'
            call edisp(iuout,out)
            CALL MFBNDR(0)
            GOTO 257
          ELSE
            IF(LKTR.GT.MITMFS)THEN
              write(out,'(A,I4,A)')
     &          'Could not get mfs-cfd convergence in '
     &                           ,lktr,' runs'
              call edisp(iuout,out)
              CALL MFBNDR(0)
              GOTO 257
            ENDIF
            LKTR=LKTR+1
            GOTO 256
          ENDIF
        ELSE
          out='standalone did not converge,going to next timestep'
          call edisp(iuout,out)
          CALL MFBNDR(0)
          GOTO 257
        ENDIF
      ELSE
        CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)
      ENDIF

C Convection coefficients.
 257  DO 100 I=1,NCONST(ICP)
        icon=IZSTOCN(icp,i)
        SFNAM(I)=SSNAME(icon)
 100  CONTINUE

C Surface conflation:
      IF( ABS(IFCFD(ICP)).EQ.1 .OR.
     &    (ABS(IFCFD(ICP)).GE.4.and.ABS(IFCFD(ICP)).LE.7) )THEN
        TAIR = TFA(ICP)
      ENDIF

C Integrated thermal conflation:
      IF(ABS(IFCFD(ICP)).EQ.2) THEN
        icTREF(1,ICFD) = 2
        CALL TREFBC(1,TAIR)
        TFA(ICP) = TAIR
      ENDIF

C Calculate convection coefficients and assign to temporary array. Adaptive
C conflation routine will decide whether to use these in zone thermal matrix.
      CALL CFCONV(TAIR,HC,SFNAM,NCONST(ICP))
      DO 200 I=1,NCONST(ICP)
        HCICFD(ICP,I)=HC(I)
 200  CONTINUE

C Write calculated variable fields to be used in the next time-step.
      ifl=icftmp
      call INTSTR(ICP,nbchar,ISWD,IER)
      write(outfil,'(a8,a)')'tmp_DFS_',nbchar(1:ISWD)
      CLOSE(ifl)
      CALL FPOPEN(IFL,IER,6,3,outfil)
      REWIND(ifl)
      CALL PRNNEW(ifl)

      RETURN
      END


C ********************* SURFCONF *********************
C SURFCONF - Calls CFMNGE each time step there is surface conflation.

      SUBROUTINE SURFCONF(ICOMP,mHCcfd)
#include "cfd.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/ACCrec/IACC
      COMMON/ICFNOD/ICFD,ICP

      CHARACTER*72 LCFD

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

C Only invoke CFD during the start-up period if requested by user.
        IF(.NOT.( IFCFD(ICOMP).LE.-1.AND.IDYP.LT.ISDS) ) THEN

C Determine which CFD domain number (ie. icfd) the current building zone
C corresponds to, then invoke CFD. Following the CFD run, set the flag
C indicating that CFD has been run in `surface conflation' mode and that
C there are CFD-calculated HCs for this zone for the current time step.
C The loop should not be necessary - keep debug statement as check for 
C multiple CFD domains
C          do 30 icfd=1,NCONF
            if( icfdnd(icfd).eq.ICOMP )then
c              write (6,*) 'SURFCONF (ICP,ICOMP,icfd)',ICP,ICOMP,icfd

C Initialize the thermophysical variables and some gridding data.
              CALL INICNT     

C If the user has requested adaptive control over the conflation, then
C invoke the controller to assess and prepare for the CFD run (i.e. adaptive
C conflation control). Otherwise, just call CFD with the user's input settings
C (i.e. uncontrolled).
              if(abs(IFCFD(ICOMP)).ge.4.and.abs(IFCFD(ICOMP)).le.7)then
                CALL ADAPCONF
              endif

C Record ACC actions (begin).
              IF(abs(IFCFD(ICOMP)).eq.4.or.abs(IFCFD(ICOMP)).eq.5)THEN
                write(IACC,*)
                write(IACC,*) 'Commencing final CFD simulation.'
              ENDIF

C Record ACC actions (end).
              call edisp(IUOUT,'Commencing adapted CFD simulation.')
              CALL CFMNGE(ICOMP,icfd)

C Examine the spacing of the next-to-wall grid points in terms of the
C dimensionless distance (y+). These results are used when conditional
C two-way adaptive conflation is active.
              CALL CHKSPC

C Set the flag to indicate that there are CFD calculated convection
C coefficients for the current time-step (thermal domain's convection routines
C decide what to do with the coefficients).
              mHCcfd = 1
      
C Save solution information (if past start-up period).
C Calculate current simulation timestep. - possibly use to synchornise results.
              if(IDYP.GE.ISDS) call cfdlib
            endif
C   30     continue
        ENDIF
      ENDIF

      RETURN
      END

C ********************* cfdlibhead *********************
C cfdlibhead - write header information to cfd results library.

      subroutine cfdlibhead
#include "cfd.h"
#include "building.h"

      common/outin/iuout,iuin
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM)
      COMMON/ICFNOD/ICFD,ICP

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX

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
C      WRITE(6,*) ICFLIB,IREC,':',MCEL1D,ICFDSV,(IEQSV(J),J=1,5+NCTM)

C Which zone(s) is CFD active in.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                (abs(ICFAUX(I)),I=1,NCOMP)
C      WRITE(6,*) ICFLIB,IREC,':',(abs(ICFAUX(I)),I=1,NCOMP)

C Write start and finish time: start day; finish day; start hour; finish hour.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) ICFDYS,ICFDYF,
     &           CFTIMS,CFTIMF
C      WRITE(6,*) ICFLIB,IREC,':',ICFDYS,ICFDYF,CFTIMS,CFTIMF

C Write number of contaminants and names
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) NCTM
C      WRITE(6,*) ICFLIB,IREC,':',NCTM
      DO 333 ICTM=1,NCTM
        IREC=IREC+1
        WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)POLNAM(ICTM,ICFD)
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
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX
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
C      write (6,*) 'saving to IREC ',IREC,' for zone ',ICOMP

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
C      WRITE(6,*) ICFLIB,IREC,':',IDRSZE,NIM2,NJM2,NKM2

C Write grid sizing information.
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (XU(I),I=1,NI)
C      WRITE(6,*) ICFLIB,IREC,':',(XU(I),I=1,NI)
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (YV(I),I=1,NJ)
C      WRITE(6,*) ICFLIB,IREC,':',(YV(I),I=1,NJ)
      IREC=IREC+1
      WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZW(I),I=1,NK)
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
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX
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
C      write (6,*) 'IRECPCstart ',IRECPC

C Loop through all control volumes.
      do 10 K=2,NK
        do 101 J=2,NJ

C Write data to library. Adjust trace to file unit 6 if ntcelz>48
          if (IEQSV(1).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Uf(I,J,K),I=2,NI)
C            write (6,'(i3,48f9.5)') IREC,(Uf(I,J,K),I=2,NI)
          endif

          if (IEQSV(2).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Vf(I,J,K),I=2,NI)
C          write (6,'(i3,48f9.5)') IREC,(Vf(I,J,K),I=2,NI)
          endif

          if (IEQSV(3).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Wf(I,J,K),I=2,NI)
C          write (6,'(i3,48f9.5)') IREC,(Wf(I,J,K),I=2,NI)
          endif

          if (IEQSV(4).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Tf(I,J,K),I=2,NI)
C          write (6,'(i3,48f9.5)') IREC,(Tf(I,J,K),I=2,NI)
          endif
                
          if (IEQSV(5).eq.1) then
            IREC=IREC+1
            WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (AGEf(I,J,K),I=2,NI)
C          write (6,'(i3,48f9.5)') IREC,(AGEf(I,J,K),I=2,NI)
          endif
                
          DO 121 ICTM=1,NCTM
            if (IEQSV(5+ICTM).eq.1) then
              IREC=IREC+1
              WRITE(ICFLIB,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                     (POLCONCf(ICTM,I,J,K),I=2,NI)
C              write(6,'(i3,48f9.5)') IREC,(POLCONCf(ICTM,I,J,K),I=2,NI)
            endif
 121      CONTINUE      
 101    continue
 10   continue

C Remember current CFD res lib record number.
      IRECPC=IREC
C      write (6,*) 'IRECPCend ',IRECPC
      return

C Error writing data.
 1000 write(outs,'(A,I5,A)')' CFDLIB: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      end

