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

C This file contains the following routines:
C      MZLIBF
C      dealremote: deal with local and remote file names.
C      MZDEL
C      MZSAVE

C ******************** MZLIBF 

C MZLIBF assigns any user-specified library in which
C the simulation results are stored.
C If any specified library already contains simulation
C results then a check is performed to ensure that any
C further simulation results entered relate to the same
C design information as those results already stored.
C This avoids confusion during output since information
C about each simulation is obtained from a header block
C containing all the relevant design details.
C If the simulations to be entered relate to different design
C information than simulations already stored then another
C library must be specified to which any subsequent
C simulation results will be transferred.
C If there is a mass flow simulation then request it's
C results library as well.
      SUBROUTINE MZLIBF
#include "building.h"
#include "cfd.h"
#include "plant.h"
#include "power.h"
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      common/reclen/nzrl,nprl,nerl

C Versions of results libraries.
      common/recver/izver,ipver,iever
      common/rpath/path
      common/uhome/upath
      common/user/browse
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/ffn/iflwn,icffs(mpcon)
      COMMON/FFC/IMFRES
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST09/IMSTR

C Electrical results library name. 
      COMMON/ENETRES/ERESLIB
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C CFD unit nos.
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB

C Active domain common.
      common/ACTDOM/CFDOK

      COMMON/save/isave

      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok

      character outs*124,ltmp*72,h*72,path*72,upath*72,longtfile*144
      character cfgroot*24,LCFGF*72
      character*72 LAPROB,ENTFLNAM,ERESLIB,CFRESF
      character RFILE*72,PFILE*72,MSTRFILE*72,LAFRES*72
      dimension iflg(MCOM)

      logical OK,dok,MSTROK,MSTRZN,remote,unixok,browse
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical CFDOK,concat

C Check and see if in a remote folder.
C Debug.
C      write(6,*) ' path is ',path
C      write(6,*) ' users home is ',upath
C      write(6,*) ichar(path(1:1)),' ',ichar(path(2:2))

C If simulator was commissioned with browsing yes then by definition
C we are running remotely and the results files should be saved
C to the users home folder. Note: this should be the same logic as
C is used in esruprj/edcfg.F subroutine edcfg.F (near line 993).
      remote=.false.
      if(browse)then
        remote=.true.
      else

C Check the configuration file name and if concat is true then
C bps was started up somewhere other than the cfg folder so
C results files should be saved to the users home folder.
        ltmp = LCFGF
        call addpath(ltmp,longtfile,concat)
        if(concat)then
          remote=.true.
        endif
      endif

C Debug.
C      write(6,*) 'mzlibf browse and remote ',browse,remote

C Skip building library if plant only configuration
      IF(INDCFG.EQ.2) GOTO 121

    1 IUNIT=IFIL+2
      CALL ERPFREE(IUNIT,ISTAT)

C Now assign user-specified building library. Use dealremote to confirm
C the file name and check if the file is being placed in a location the
C user can write to.
    4 LTMP=RFILE

C Debug.
C      write(6,*) 'reslib.F spfileok remote ',spfileok,remote
      if(.not.spfileok) then
        iw=0
        call dealremote('zi',remote,LTMP,IW)
        IF(LTMP(1:2).EQ.'  ')GOTO 4
      else

C If the simulator is being run in autoexec mode then the results 
C will be placed in the location specified if remote is false and
C with the users home folder prepended if remote is true. Otherwise
C the user will be asked to confirm the results file name (and path)
        if(autook) then
          iw=2
          if(remote)then
            call dealremote('zs',remote,LTMP,IW)
          endif
        else
          iw=0
          call dealremote('zi',remote,LTMP,IW)
          IF(LTMP(1:2).EQ.'  ')GOTO 4
        ENDIF
      ENDIF

C If remote path then use EFOPRAN otherwise FPRAND.
      RFILE=LTMP
      if(iw.eq.1)then
        call EFOPRAN(IUNIT,RFILE,nzrl,3,IER)
        if(ier.ne.0)goto 4
      else
        CALL FPRAND(IUNIT,ISTAT,nzrl,3,RFILE)
        if(ISTAT.lt.0) then
          if(autook) then
            write(outs,*)'MZLIBF Fatal error: unable to open',
     &        rfile(1:lnblnk(rfile)),' record width ',nzrl
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            call epagend
            stop
          else
            goto 4
          endif
        endif
      endif

C Tell the user (and any log file) that their library will be used.
      write(outs,'(2A)')' Library file: ',RFILE
      call edisp(iuout,outs)
      call tstamp('>','BPS: create zone results')
      call tstamp('>',RFILE)

C << the following code is for testing >>
C Create SQL results database.
C      write (doit,'(2a)') 'mysqladmin -u root CREATE ',RFILE
C      write (doit,'(a)') 'mysqladmin -u iain -pxx CREATE fred'
C      write (6,*) doit
C      call runit(doit,'-')

c Does this library contain results ?
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=7)NSIM

c Response according to number of existing results sets.
C If autook then do not preserve previous results sets.
      if(autook) then
        if(NSIM.gt.0) then
          OK=.FALSE.
        else
          goto 8
        endif
      else

C Autook not set so ask for confirmation.
        h(1)='A previous assessment or assessments are stored in'
        h(2)='this file. If the model is the same it should be safe'
        h(3)='to append another assessment. '
        h(4)=' '
        h(5)='If the model has changed the differences might cause'
        h(6)='eratic performance results recovery. Take care! '
        h(7)=' '
        h(8)='If you do not preserve the pervious assessments the'
        h(9)='new data will overwrite them. '
        if(NSIM.EQ.1)then
          dok=.true.
          CALL ASKOK(' Library contains one result set.',
     &               ' Preserve (see help)?',OK,dok,9)
        elseif(NSIM.gt.1)then
          write(outs,'(A,I3,A)')' Library contains',NSIM,
     &                           ' result sets.'
          dok=.true.
          CALL ASKOK(outs,' Preserve (see help)?',OK,dok,9)
        elseif(NSIM.eq.0)then
          goto 8
        endif
      endif

      IF(OK)then

C User wants to preserve data. Determine if design data has been changed.
        dok=.false.
        h(1)='If the model has changed the differences might cause'
        h(2)='eratic performance results recovery. Take care! '
        h(3)=' '
        h(4)='If in doubt, specify a different results file. Note'
        h(5)='that if the model is newer than the existing predictions'
        h(6)='you can also get eratic results reporting. '
        CALL ASKOK(' Is the current model different in any way from',
     &  ' the model of the stored results (see help)?',OK,dok,6)
        if(.NOT.OK) goto 121
        GOTO 1
      else

c Delete all simulations by setting NSIM=0.
        IREC=1
        NSIM=0

C izver is the version flag for building results: 
C   izver=1 tells res that the results files have associated descriptive strings.
C   izver=2 tells res that aggregate inside and outside face convective transfer 
C     is included in zone info. 
C   izver=3 total exterior transparent and floor areas saved for each zone
C   izver<4 does not include explicit conv/rad/latent gain fields for each casual gain type.
        izver=4
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver
      endif

C Write header block information to building results library.
    8 CALL MZLS1

C Now assign user-specified mass flow library.
  121 if(iairn.ge.1.or.iflwn.eq.1)then

C <<Should really get this name from the simulation parameters.>>
        if(LAFRES(1:7).eq.'UNKNOWN'.or.LAFRES(1:7).eq.'unknown')
     &     LAFRES='libf'
    9   LTMP=LAFRES
        if(.not.spfileok) then
          iw=0
          call dealremote('fi',remote,LTMP,IW)
          IF(LTMP.EQ.' ')GOTO 9
          LAFRES=LTMP
        else

C If the simulator is being run in autoexec mode then flow results 
C will be placed in the location specified if remote is false and
C with the users home folder prepended if remote is true. Otherwise
C the user will be asked to confirm the results file name (and path)
          if(autook) then 
            iw=2
            if(remote)then
              call dealremote('fs',remote,LTMP,IW)
            endif
          else
            iw=0
            call dealremote('fi',remote,LTMP,IW)
            IF(LTMP.EQ.' ')GOTO 9
            LAFRES=LTMP
          endif
        endif

C Open the mass flow results file on ifil+4, if it exists check with
C user before overwriting. In case of specifying a non-relative
C file path name use FPOPEN.
        IMFRES=IFIL+4
        if(iw.eq.1)then
          CALL EFOPRAN(IMFRES,LAFRES,NMRL,3,IER)
          if(ier.ne.0)goto 9
        else
          CALL ERPFREE(IMFRES,ISTAT)
          call FPRAND(IMFRES,ISTAT,NMRL,3,LAFRES)
          if(ISTAT.lt.0)then
            if(autook) then
              write(outs,*)'MZLIBF Fatal error: unable to open',
     &          LAFRES(1:lnblnk(LAFRES)),' record width ',NMRL
              call edisp(iuout,outs)
              call pauses(3)   ! pause for 3 seconds then abort program.
              call epagend
              stop
            else
              goto 9
            endif
          endif
        endif

        write(outs,'(2A)')' Flow results file: ',LAFRES
        call edisp(iuout,outs)
        call tstamp('>','BPS: create flow results')
        call tstamp('>',LAFRES)

C Write header information to mass flow library.
        call MFOUTPH(IMFRES)
      endif

C Test for plant. If plant is not found, or save-level 0 or 5
C are specified AND the simulatior is in silent running mode.

      IF(INDCFG.EQ.1 ) GOTO 17 
      IF( autook .AND. ( isave .EQ. 0 .OR. isave .EQ. 5 ) ) GOTO 17

C If a plant library has already been assigned
C within current run of program then 'free' it.
 100  IUNIT=IFIL+3
      CALL ERPFREE(IUNIT,ISTAT)

C Now assign user-specified plant library.
 103  LTMP=pfile
      if(.not.spfileok) then
        iw=0
        call dealremote('pi',remote,LTMP,IW)
        IF(ltmp.EQ.' ')GOTO 103
        pfile=ltmp
      else

C If the simulator is being run in autoexec mode then plant results 
C will be placed in the location specified if remote is false and
C with the users home folder prepended if remote is true. Otherwise
C the user will be asked to confirm the results file name (and path)
        if(autook) then 
          iw=2
          if(remote)then
            call dealremote('ps',remote,LTMP,IW)
          endif
        else
          iw=0
          call dealremote('pi',remote,LTMP,IW)
          IF(ltmp.EQ.' ')GOTO 103
          pfile=ltmp
        endif
      endif

C If remote path then use EFOPRAN otherwise FPRAND.
      if(iw.eq.1)then
        call EFOPRAN(IUNIT,PFILE,nprl,3,IER)
        if(ier.ne.0)goto 103
      else
        CALL FPRAND(IUNIT,ISTAT,nprl,3,PFILE)
        if(ISTAT.lt.0)then
          if(autook) then
            write(outs,*)'MZLIBF Fatal error: unable to open',
     &        pfile(1:lnblnk(pfile)),' record width ',nprl
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            call epagend
            stop
          else
            goto 103
          endif
        endif

        write(outs,'(2A)')' Plant results file: ',PFILE
        call edisp(iuout,outs)
        call tstamp('>','BPS: create plant results')
        call tstamp('>',PFILE)
      endif

c Does this library contain results ?
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=107)NSIM

c Response according to number of existing results sets.
      h(1)='A previous assessment or assessments are stored in'
      h(2)='this file. If the model is the same it should be safe'
      h(3)='to append another assessment. '
      h(4)=' '
      h(5)='If the model has changed the differences might cause'
      h(6)='eratic performance results recovery. Take care! '
      h(7)=' '
      h(8)='If you do not preserve the pervious assessments the'
      h(9)='new data will overwrite them. '
      IF ( autook ) then
         OK = .FALSE.
      elseIF(NSIM.EQ.1)then
        dok=.true.
        CALL ASKOK(' Library contains one result set.',
     &             ' Preserve (see help)?',OK,dok,9)
      elseif(NSIM.gt.1)then
        write(outs,'(A,I3,A)')' Library contains',NSIM,' result sets.'
        dok=.true.
        CALL ASKOK(outs,' Preserve (see help)?',OK,dok,9)
      elseif(NSIM.eq.0)then
        goto 108
      endif

      IF(OK)then

c Determine if design data has been changed.
        dok=.false.
        h(1)='If the model has changed the differences might cause'
        h(2)='eratic performance results recovery. Take care! '
        h(3)=' '
        h(4)='If in doubt, specify a different results file. Note'
        h(5)='that if the model is newer than the existing predictions'
        h(6)='you can also get eratic results reporting. '
        CALL ASKOK(' Is the current model different in any way from',
     &  ' the model of the stored results (see help)?',OK,dok,6)
        if(.NOT.OK)goto 17
        GOTO 100
      else

c Delete all simulations by setting NSIM=0.
        IREC=1
        NSIM=0
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl
      endif

c Write header block information to plant library.
  108 CALL MZPLS1

c Now assign user-specified moisture flow library.
   17 if(MSTROK)then
   59   LTMP=MSTRFILE
        if(.not.spfileok) then
          iw=0
          call dealremote('mi',remote,LTMP,IW)
          IF(LTMP.EQ.' ')GOTO 59
          MSTRFILE=LTMP
        else

C If the simulator is being run in autoexec mode then moisture results 
C will be placed in the location specified if remote is false and
C with the users home folder prepended if remote is true. Otherwise
C the user will be asked to confirm the results file name (and path)
          if(autook) then 
            iw=2
            if(remote)then
              call dealremote('ms',remote,LTMP,IW)
            endif
          else
            iw=0
            call dealremote('mi',remote,LTMP,IW)
            IF(LTMP.EQ.' ')GOTO 59
            MSTRFILE=LTMP
          endif
        endif

C Open the moisture flow results file on imstr, if it exists check with
C user before overwriting. In case of specifying a non-relative
C file path name use FPOPEN.
        if(iw.eq.1)then
          CALL EFOPSEQ(IMSTR,MSTRFILE,4,IER)
          if(ier.ne.0)GOTO 59
        else
          CALL ERPFREE(IMSTR,ISTAT)
          call FPOPEN(IMSTR,ISTAT,1,3,MSTRFILE)
          if(ISTAT.lt.0)then
            if(autook) then
              write(outs,*)'MZLIBF Fatal error: unable to open',
     &          MSTRFILE(1:lnblnk(MSTRFILE))
              call edisp(iuout,outs)
              call pauses(3)   ! pause for 3 seconds then abort program.
              call epagend
              stop
            else
              goto 59
            endif
          endif
        endif

C Echo to display and log file the moisture library file name.
        write(outs,'(2A)')' Moisture results file: ',MSTRFILE
        call edisp(iuout,outs)
        call tstamp('>','BPS: create moisture results')
        call tstamp('>',MSTRFILE)

C Write the moisture result library header.
        DO 58 ICOMP=1,NCOMP
          IF(MSTRZN(ICOMP))THEN
            iflg(ICOMP)=1
          ELSE
            iflg(ICOMP)=0
          ENDIF
   58   CONTINUE
        WRITE(IMSTR,'(30I2)')(iflg(ICOMP),ICOMP=1,NCOMP)
      endif

C Assign a library for the power flow results, and open on IFIL+21. 

  127 if( ientxist.gt.0 .and. 
     &     ( .not. autook .or. 
     &     (isave .ne. 0 .and. isave .ne. 5) ) )then
   11   LTMP=ERESLIB
        IF(.not.spfileok) THEN
          IW=0
          call dealremote('ei',remote,LTMP,IW)
          IF(LTMP.EQ.' ')GOTO 11
          ERESLIB=LTMP
        else
          if(autook) then 
            iw=2
            if(remote)then
              call dealremote('es',remote,LTMP,IW)
            endif
          else
            iw=0
            call dealremote('ei',remote,LTMP,IW)
            IF(LTMP.EQ.' ')GOTO 11
            ERESLIB=LTMP
          endif
        endif

C Open the electrical network results file on ifil+21(MADFIL), if it exists check with
C user before overwriting. In case of specifying a non-relative
C file path name use FPRAND.
        IELFIL=IFIL+MADFIL
        IELIBW=MELIBW
        if(iw.eq.1)then
          CALL EFOPRAN(IELFIL,ERESLIB,IELIBW,3,IER)
          if(ier.ne.0)goto 11
        else
          CALL ERPFREE(IELFIL,ISTAT)
          call FPRAND(IELFIL,ISTAT,IELIBW,3,ERESLIB)
          if(ISTAT.lt.0)then
            if(autook) then
              write(outs,*)'MZLIBF Fatal error: unable to open',
     &          ERESLIB(1:lnblnk(ERESLIB)),' record width ',IELIBW
              call edisp(iuout,outs)
              call pauses(3)   ! pause for 3 seconds then abort program.
              call epagend
              stop
            else
              goto 11
            endif
          endif
        endif

C Write out the electrical results header to the file. 
        CALL ENETLIBH
        write(outs,'(2A)')' Power results file: ',ERESLIB
        call edisp(iuout,outs)
        call tstamp('>','BPS: create power results')
        call tstamp('>',ERESLIB)
      endif

C CFD results library.
      if (CFDOK) then
 591    LTMP='libc'
        if(.not.spfileok) then
          iw=0
          call dealremote('di',remote,LTMP,IW)
          IF(LTMP.EQ.' ')GOTO 591
          CFRESF=LTMP
        else
          if(autook) then 
  
C <<Should really get this name from the simulation parameters.>>
            iw=2
            if(remote)then
              call dealremote('ds',remote,LTMP,IW)
            endif
            CFRESF=LTMP
          else
            iw=0
            call dealremote('di',remote,LTMP,IW)
            IF(LTMP.EQ.' ')GOTO 591
            CFRESF=LTMP
          endif
        endif

C Open the CFD results file on ICFLIB, if it exists check with
C user before overwriting. In case of specifying a non-relative
C file path name use FPOPEN.
        if(iw.eq.1)then
          call EFOPRAN(ICFLIB,CFRESF,MCEL1D,3,IER)
          if(ier.ne.0)GOTO 591
        else
          CALL ERPFREE(ICFLIB,ISTAT)
          CALL FPRAND(ICFLIB,ISTAT,MCEL1D,3,CFRESF)
          if(ISTAT.lt.0)then
            if(autook) then
              call epagend
              stop
            else
              goto 591
            endif
          endif
        endif
      endif

      RETURN

C Error handling
 1000 WRITE(outs,'(A,I5,A)')' MZLIBF: error at record',IREC,'.'
      call edisp(iuout,outs)
      goto 17

C Error on read of library - reset and initialize.
   7  IREC=1
      NSIM=0
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver
      goto 8

 107  IREC=1
      NSIM=0
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl
      goto 108

      END

C ********** dealremote
C Deal with names of various results files and checks if they will be
C put in a location other than where the user invoked the tool.
C Passed parameters:
C  topic (2 char) (z=zone, f=network flow, p=plant, m=moisture, e=electrical,
C         d=CFD ) with second character i=interactive s=silent
C  remote (logical) = true then calculate the full path
C  lfile (72 char) file name which may be edited by user (if interactive) 
C        and returned as an updated string
C Return parameter:
C  IW (integer) as 0 if no decision made, IW as 1 if in current path and IW
C        as 2 if in some other location.
      subroutine dealremote(topic,remote,LFILE,IW)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/uhome/upath
      common/pophelp/h(60)
      character topic*2,LFILE*72
      character h*72,upath*72,ltmp*72,ltpath*72,filen*72
      logical unixok,remote

      iw=1
      if(remote)then
        call fdroot(lfile,ltpath,filen)

C Is the path (ltpath) to the results file absolute or relative.
C If it is unix and starts with a / then absolute. If DOS and
C the 2nd character is a : then absolute.
        call isunix(unixok)
        if(unixok)then
          if (ICHAR(ltpath(1:1)).ne.47) then
            write(LFILE,'(3a)') upath(1:lnblnk(upath)),'/',
     &        filen(1:lnblnk(filen))
            iw=2
          endif
        else
          if (ltpath(2:2).ne.':') then
            write(LFILE,'(3a)') upath(1:lnblnk(upath)),'/',
     &        filen(1:lnblnk(filen))
            iw=2
          endif
        endif
      endif

C Depending on the domain of the file confirm its name.
      if(topic(1:2).eq.'zi')then
        H(1)='The zone results library contains one or more simulation'
        H(2)='result sets. Each result set is comprised of the'
        H(3)='simulation time-series for the same model but,'
        H(4)='potentially, for different periods, climate data and '
        H(5)='control. This file is passed to res for analysis. '
        H(6)=' '
        H(7)='It is normally placed in the model configuration folder,'
        H(8)='however another folder can be specified. '
        if(IW.eq.1)then
          CALL EASKS(LFILE,' Zone results library name ?',
     &      ' ',72,'libb','zone results lib',IER,8)
        elseif(IW.eq.2)then
          write(ltmp,'(2a)') upath(1:lnblnk(upath)),'/libb'
          CALL EASKS(LFILE,' Zone results library name ?',
     &      ' ',72,ltmp,'zone results lib',IER,9)
        endif
      elseif(topic(1:2).eq.'fi')then
        H(1)='The results of mass flow simulations are placed in an'
        H(2)='ASCII file for recovery and analysis by the simlator'
        H(3)='or the stand-alone mfs solver. '
        H(4)=' '
        H(5)='It is normally placed in the model configuration folder,'
        H(6)='however another folder can be specified. '
        if(IW.eq.1)then
          CALL EASKS(LFILE,' Mass flow results library name ?',
     &      ' ',72,'libf','mass results lib',IER,6)
        elseif(IW.eq.2)then
          write(ltmp,'(2a)') upath(1:lnblnk(upath)),'/libf'
          CALL EASKS(LFILE,' Mass flow results library name ?',
     &      ' ',72,ltmp,'mass results lib',IER,6)
        endif
      elseif(topic(1:2).eq.'pi')then
        H(1)='The plant (systems) results library contains one or more'
        H(2)='simulation result sets. Each set comprises the simulation'
        H(3)='time-series for the same model but, potentially, for'
        H(5)='different periods, climate data and control. This file'
        H(6)='is passed to the results analysis module for analysis.'
        H(7)=' '
        H(8)='It is normally placed in the model configuration folder,'
        H(9)='however another folder can be specified. '
        if(IW.eq.1)then
          CALL EASKS(LFILE,' Plant results library name ?',
     &      ' ',72,'libp','plnt results lib',IER,9)
        elseif(IW.eq.2)then
          write(ltmp,'(2a)') upath(1:lnblnk(upath)),'/libp'
          CALL EASKS(LFILE,' Plant results library name ?',
     &      ' ',72,ltmp,'plnt results lib',IER,9)
        endif
      elseif(topic(1:2).eq.'mi')then
        H(1)='The results of moisture flow simulations are placed in'
        H(2)='an ASCII file for recovery and analysis by the mld'
        H(3)='module. '
        H(4)=' '
        H(5)='It is normally placed in the model configuration folder,'
        H(6)='however another folder can be specified. '
        if(IW.eq.1)then
          CALL EASKS(LFILE,' Moisture flow results library name ?',
     &      ' ',72,'libmoist','Moisture results lib',IER,6)
        elseif(IW.eq.2)then
          write(ltmp,'(2a)') upath(1:lnblnk(upath)),'/libmoist'
          CALL EASKS(LFILE,' Moisture flow results library name ?',
     &      ' ',72,ltmp,'Moisture results lib',IER,6)
        endif
      elseif(topic(1:2).eq.'ei')then
        H(1)='The results of electrical network simulations are '
        H(2)='placed in an binary database for recovery and'
        H(3)='analysis by the results analysis module. '
        H(4)=' '
        H(5)='It is normally placed in the model configuration folder,'
        H(6)='however another folder can be specified. '
        if(IW.eq.1)then
          CALL EASKS(LFILE,'Electrical network results library name ?',
     &      ' ',72,'libe','electrical results lib',IER,6)
        elseif(IW.eq.2)then
          write(ltmp,'(2a)') upath(1:lnblnk(upath)),'/libe'
          CALL EASKS(LFILE,'Electrical network results library name ?',
     &      ' ',72,ltmp,'electrical results lib',IER,6)
        endif
      elseif(topic(1:2).eq.'di')then
        H(1)='The results of CFD simulations are placed in a binary'
        H(2)='database for recovery and analysis by the results'
        H(3)='analysis module. '
        H(4)=' '
        H(5)='It is normally placed in the model configuration folder,'
        H(6)='however another folder can be specified. '
        if(IW.eq.1)then
          CALL EASKS(LFILE,' CFD results library name ?',
     &      ' ',72,'CFD.res','CFD results lib',IER,6)
        elseif(IW.eq.2)then
          write(ltmp,'(2a)') upath(1:lnblnk(upath)),'/CFD.res'
          CALL EASKS(LFILE,' CFD results library name ?',
     &      ' ',72,ltmp,'CFD results lib',IER,6)
        endif
      endif
      return
      end

c ******************** MZDEL

c MZDEL DELETES THE LAST RESULT-SET SAVED IN THE SOLUTION FILE.

      SUBROUTINE MZDEL(IB,IP,ISIMLB,ISIMLP)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/reclen/nzrl,nprl,nerl
      common/recver/izver,ipver,iever

      character outs*124

      IF(IP.EQ.1)goto 4
      IUNIT=IFIL+2

C READ NUMBER OF SIMULATIONS CURRENTLY HELD WITHIN 
C building SOLUTION FILE.
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM
      IF(NSIM.EQ.0)goto 1

C DE-INCREMENT BY 1.
      NSIM=NSIM-1

C WRITE THIS TO building SOLUTION FILE.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver
      ISIMLB=NSIM
      goto 2

C NO SIMULATIONS IN SOLUTION FILE PRIOR TO ATTEMPTED DELETION.
    1 ISIMLB=-1
      goto 2
    4 IUNIT=IFIL+3

C READ NUMBER OF SIMULATIONS CURRENTLY HELD WITHIN 
C plant SOLUTION FILE.
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)NSIM
      IF(NSIM.EQ.0)goto 5

C DE-INCREMENT BY 1.
      NSIM=NSIM-1

C WRITE THIS TO plant SOLUTION FILE.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)NSIM,nzrl
      ISIMLP=NSIM
      goto 2

C NO SIMULATIONS IN SOLUTION FILE PRIOR TO ATTEMPTED DELETION.
    5 ISIMLP=-1
    2 RETURN

 1000 IF(IP.EQ.1)goto 1001
      write(outs,3)IREC
    3 format(' MZDEL: building results library error (record number',
     &I4,').')
      call edisp(iuout,outs)
      goto 2

 1001 write(outs,6)IREC
    6 FORMAT(' MZDEL: plant results library error (record number',
     &I4,').')
      call edisp(iuout,outs)
      goto 2
      END

c ******************** MZSAVE

c MZSAVE SAVES THE LAST SIMULATION RESULTS SET PERMANENTLY IN THE
c SOLUTION FILE. FAILURE TO SELECT THE '+1SAVE' COMMAND FROM THE
c 'SIMUL' MENU IMMEDIATELY AFTER ANY SIMULATION IS COMPLETE RESULTS
c IN THE OVERWRITING OF THESE RESULTS.

      SUBROUTINE MZSAVE(ISIMN1,ISIMN2)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/C6/INDCFG
      common/reclen/nzrl,nprl,nerl
      common/recver/izver,ipver,iever

      character outs*124

      IF(INDCFG.EQ.2)goto 3
      IUNIT=IFIL+2

C READ NUMBER OF SIMULATIONS CURRENTLY HELD WITHIN SOLUTION FILE.
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM

C INCREMENT BY 1.
      NSIM=NSIM+1

C WRITE THIS TO SOLUTION FILE.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver
      ISIMN1=NSIM
    3 IF(INDCFG.EQ.1)goto 2

      IUNIT=IFIL+3
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM
      NSIM=NSIM+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl
      ISIMN2=NSIM

    2 RETURN

 1000 IF(IUNIT.EQ.(IFIL+19))goto 1001
      write(outs,1)IREC
    1 format(' MZSAVE: error in building results file (record',I4,').')
      call edisp(iuout,outs)
      goto 2

 1001 write(outs,4)IREC
    4 format(' MZSAVE: error in plant results file (record',I4,').')
      call edisp(iuout,outs)
      goto 2
      END

