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

C This file (resdef.f) contains the following routines:
C  MORFIL
C  MORESS
C  MOOPER
C  MOZDFN
C  MOINIT
C  MOCHEK
C  MOFREE

C ******************** MORFIL ********************

C MORFIL assigns any user-specified results database
C for access by the various result recovery and display
C routines.

      SUBROUTINE MORFIL(iftype)
#include "building.h"
#include "plant.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/INITL/INIT1,INIT2
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/ABSRESLIB/RFILEABS,PFILEABS,MSTRFILEABS,LAFRESABS
      common/rpath/path
      common/rcmd/lcmdfl
      COMMON/LIBOK/IBLIB,IFLIB,ICLIB,IELIB,IPLIB

C Record lengths for zone (nzrl), plant (nprl), electrical(nerl) results libraries.
      common/reclen/nzrl,nprl,nerl
      common/recver/izver,ipver,iever

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
 
      character LTMP*72,H*72,outs*124,outs248*248
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character RFILEABS*144,PFILEABS*144,MSTRFILEABS*144,LAFRESABS*144
      character path*72,lcmdfl*144,ltmpc*144,lpath*72
      logical XST
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok

      INIT1=0
      INIT2=0
      IUNIT=IFIL
      INIT1=1

C Initialise result library type
      iftype=0

C Assign user-specified database (file must exist -
C unit number = IFIL) but first free any previously
C assigned database.
      IF(INIT2.EQ.1)CALL ERPFREE(IUNIT,ISTAT)

   6  if(.NOT.autook)then
        H(1)='A results library is created by the simulator. It will'
        H(2)='contain the time series results of one or more'
        H(3)='simulations. '
        H(4)='On computers running UNIX, the file: '
        write(H(5),'(5x,a60)') DEFRLB(1:60)
        H(6)='usually exists. This is a link to an example results'
        H(7)='library set up when ESP was first installed.'
        LTMP=' '
        ltmpc=LCMDFL   ! the file name that was passed in the command line
        llt=lnblnk(ltmpc)

C Use ifdefs because the X11 version will be returning only the
C name of the file, while the GTK version will be returning the
C name with the full path.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(ltmpc,' Library name ? ',' ',96,DEFRLB,
     &      'library file',IER,7)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(ltmpc,' Library name ? ',' ',124,DEFRLB,
     &      'library file',IER,7)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(ltmpc,' Library name ? ',' ',144,DEFRLB,
     &      'library file',IER,7)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(ltmpc,' Library name ? ',' ',144,DEFRLB,
     &      'library file',IER,7)
        else
          CALL EASKF(ltmpc,' Library name ? ',' ',96,DEFRLB,
     &    'library file',IER,7)
        endif

C If user cancel set iftype to -2 and return (assume calling code
C will act on this.
        if(ier.eq.-3)then
          iftype=-2
          return
        endif

        if(ltmpc(1:2).ne.'  '.and.ltmpc(1:4).ne.'UNKN')then

C Save back the command file name. While here also assume that any
C other results files will be found in the same location and make
C up full length strings that guess what those file names will be.
          LCMDFL=ltmpc
          lrf=lnblnk(ltmpc)
          lrf3=lrf-3
          lrf4=lrf-4
          if(ltmpc(lrf3:lrf).eq.'.res')then
            write(LAFRESABS,'(2a)') ltmpc(1:lrf4),'.mfr'
            write(PFILEABS,'(2a)') ltmpc(1:lrf4),'.plr'
          else
            continue
          endif
        else
          goto 6
        endif
        IER=0
      else
        LTMP=' '
        IER=0
        call usrmsg(' ',' ','-')
      endif

C Temporarily find the path and local file name for the results
C file supplied. Save the current path to lpath and restore this
C after the results file has been found and loaded so that subsequent
C scans of cfg and zone files can be accomplished.
C Debug.
C      write(outs248,*) 'MORFIL: current command file path is ',path
C      call edisp248(iuout,outs248,100)
      write(outs248,*) 'MORFIL: cmd line file is ',lcmdfl
      call edisp248(iuout,outs248,100)

      lpath=path
      call fdroot(LCMDFL,path,LTMP)

C Debug.
C      write(outs248,*) 'MORFIL: local file is now ',ltmp
C      call edisp248(iuout,outs248,100)
C      write(outs248,*) 'MORFIL: rev path is ',path
C      call edisp248(iuout,outs248,100)

      CALL ERPFREE(iunit,ISTAT)
      call FINDFIL(ltmp,xst)
      IF(XST)THEN

C See if this is a plant result file.
C That is if record 32=-1 or -2.
        nprl = nprecl
        call EFOPRAN(iunit,LTMP,nprl,1,IER)
        if(ier.eq.0) then
           irec=32
           read(iunit,rec=irec,iostat=ios,err=13)iftype
           if(iftype.eq.-1) then

C Debug.
C             write(6,*) 'MORFIL: older plant ',iftype

             IPLIB=1
             return
           elseif(iftype.eq.-2) then

C Debug.
C             write(6,*) 'MORFIL: newer plant ',iftype

	     IPLIB=2
             iftype=-1
             return
           endif
        endif

C Else assume this is a building result file. Use same width
C as the simulator uses.
        nzrl = MZRL
        close(iunit)
        iftype=0
        call EFOPRAN(iunit,LTMP,nzrl,1,IER)
        irec=1
        read(iunit,rec=irec,iostat=ios,err=13)nsim,nreclx
        if (nreclx.lt.nzrl) then
          nzrl = nreclx
          close(iunit)
          call EFOPRAN(iunit,LTMP,nzrl,1,IER)
          IBLIB=1
          write(outs,'(A,I3)')' Opening file with record length',nzrl
          call edisp(iuout,outs)
        elseif (nreclx.gt.nzrl) then
C          nzrl = nreclx
C          close(iunit)
C          call EFOPRAN(iunit,LTMP,nzrl,1,IER)
C          write(outs,'(A,I3)')' Opening file with record length',nzrl
C          call edisp(iuout,outs)
          call usrmsg(
     &      ' Unable to open library (record width to large).',LTMP,'W')
          CALL EPAGEND
          STOP
        else
          if(LTMP.eq.DEFRLB) then
            call edisp(iuout,' Default results library assigned.')
            IBLIB=1
          endif
        endif
        RFILE=LTMP   ! save the non-path part of the results file

C Restore the path after getting results file name so model files
C can be accessed.
        path=lpath
C        write(outs248,*) 'MORFIL: path reset to ',path
C        call edisp248(iuout,outs248,100)
      ELSE
        call usrmsg(' Could not find: ',LTMP,'W')
        goto  6
      ENDIF

C Establish if this database contains result-sets.
      INIT2=1
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=7)NSIM,nzrl,izver,IMET
      IF(NSIM.EQ.0)then
        H(1)='Take extreme caution in using incomplete or corrupt '
        H(2)='results libraries. '
        call easkab('Library is incomplete or corrupt...  Do you want',
     &    ' to:',' attempt to continue (DANGEROUS)','give up  ? ',IW,2)
        if(IW.eq.2)then
          CALL EPAGEND
          STOP
        else
          NSIM=1
        endif
      elseif(NSIM.gt.MNRS) then
        H(1)='Change the variable MNRS in the file building.h to 100'
        H(2)='as results analysis is curently configured to read a '
        H(3)='maximum of 30 result sets. '
        H(4)='You will then have to recompile the results analysis '
        H(5)='module - good luck ! '
        call easkab('Library has more than 30 result sets! Do you want',
     &    ' to:',' attempt to continue (DANGEROUS)','give up ? ',IW,5)
        if(IW.eq.2)then
          CALL EPAGEND
          STOP
        else
          NSIM=MNRS
        endif
      endif
      INIT2=1
      GOTO 8

    7 if(ios.eq.2)then
        call usrmsg('No permission to read number of simulation',
     &            'result sets from the library.','W')
      else
        call usrmsg('Unable to read number of simulation result',
     &            'sets from the library.','W')
      endif
      GOTO 6

C Read in all file names and miscellaneous data associated
C with the results held in this database and stored in the
C header block.
    8 ISTREC=MNRS
      CALL MOINIT(ISTREC,IER)

C If the user was asked to browse for the model configuration file
C and it was not found or there was a cancel then ier will be returned
C as -3 so set iftype to -2 to signal a problem.
      if(ier.eq.-3)then
        call usrmsg(
     &    'A cancel when browsing for the model configuration file',
     &    'will probably cause problems. Try loading results again.',
     &    'W')
        iftype=-2
      elseif(ier.eq.-4)then
        call usrmsg(
     &    'An issue with the climate file was encountered. This will',
     &    'cause problems. Try finding climate again respecifying.',
     &    'W')
      elseif(ier.eq.-5)then
        call usrmsg(
     &    'An issue with the results file header was encountered. This',
     &    'will cause problems. Try re-running the simulation.',
     &    'W')
      endif
      return

C Note: flow results are opened at call to mfoutp.

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG('No permission to read library header',' ','W')
      else
        CALL USRMSG('File read error in library header',' ','W')
      endif
      RETURN
      END

C ******************** MORESS ********************
C MORESS assigns the user-defined result-set from the
C (potentially) many held within the currently assigned
C database.
C If the results from a sensitivity analysis are detected 
C (all ISAVE's the same) then proceed as usual but note that 
C SA is possible by loading all result set aide memoire's into 
C thc common /SETPIK/ and /SETNAM/.

C Common block variables are:

C ISIM     - the selected result-set number.
C ISTADD   - the start address (record number) of the actual
C            results.
C ID1, IM1 - simulation start day and month numbers.
C ISDS     - simulation start year day number.
C ID2, IM2 - as ID1, IM1 & ISDS but for simulation finish
C & ISDF     day.
C NTS      - simulation computational time-step.
C ISAVE    - result save option where:
C            ISAVE = 1 ; minimum save
C              '   = 2 ; moderate save
C              '   = 3 ; total save
C              '   = 4 ; surface balance save
C CTLDOC    - Overall configuration control descriptor.
C          - 'None' indicates no control.
C
C IDAVER   - Dictates whether time-row results have been averaged
C            prior to transfer to the results library.
C LCTLF    - Holds the name of the configuration control file, if
C            one exists.
C RSNAME   - Results set aide memoir held in common /SETNAM/ local version is
C TNAME (temporary value) 

      SUBROUTINE MORESS
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/DEFLT/IDEFLT
      COMMON/FILEP/IFIL

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPKA/NSIM
      common/prec7/itcnst

      COMMON/CCTLNM/ctldoc,LCTLF
      COMMON/CCTL/ICASCF(MCOM)

C NS - number of selected result sets
C NSNO() - set numbers of selected sets
C ISETON() - set to 1 if set active (only in common so as values persist)
C IMET - uncertainty analysis method
C IFAFLG - factorial analysis peturbations
      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/ZONPIK/NZ,NZNO(MCOM)
 
      COMMON/SETNAM/RSNAME(MNRS)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/AVRAGE/IDAVER

C Versions of libraries (zone, plant, electrical).
      common/recver/izver,ipver,iever

C SCNAME is the first 12 characters of the overall control name.
      CHARACTER ctldoc*248,LCTLF*72,H*72,outs*124,SCNAME*12
      CHARACTER RSNAME*40,TNAME*40

      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg     !- flag indicating if solar radiation data in weather file
                                   !- is hour-centered (default) or half-hour centered.
                                   !- 0 = hour-centered; 1 = half-hour centered.

      DIMENSION ITMPSETS(MNRS)

C Set number of chosen sets to zero and clear array NSNO.
      IMET=0
      NS=0
      SCNAME=' '
      do 5, II=1,MNRS
        NSNO(II)=0
        ISETON(II)=0
 5    continue
      IUNIT=IFIL

C Determine how many result-sets are held in database. Note
C that izver = 3 or earlier does not include explicit separate
C casual gains data items.
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NSIM,nzrl,izver,IMET

C Debug.
C      write(6,*) 'uncertainty analysis method ',IMET

C If there is more than one result-set then check to see if ISAVE=const for 
C all sets -- if this is the case then res can calculate and display 
C differences between the sets. 
C If ISAVE is not constant for ALL of the sets then the user will be asked 
C to nominate a set for use.
      IF(NSIM.GT.1)THEN

C Display the header information from all the result sets.
        call edisp(iuout,'  ')
        write(outs,'(A,I2)') ' Number of result-sets held = ',NSIM
        call edisp(iuout,outs)
        call edisp(iuout,'  ')
C Set|Control    |Start |Finish| Time   | Save |Average|Pre|Aid memoire
C no.| name      | day  | day  |steps/hr|option| Flag  |sim|
        write(outs,'(A,A)')' Set|Control    |Start |Finish| Time   ',
     &        '| Save |Average|Pre|Aid memoire'
        call edisp(iuout,outs)
        write(outs,'(A,A)')' no.| name      | day  | day  |steps/hr',
     &        '|option| Flag  |sim|(for this set)'
        call edisp(iuout,outs)

C Read and display information on all result-sets.
        if (IMET.eq.2) NFA=nint(log10(real(NSIM-1))/log10(real(2)))
        DO 99 I=1,NSIM
          IREC=I+1

C Read result-set start address.
          TNAME=' '
          READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NST1,NST2,TNAME
          if (izver.eq.0) TNAME=' Undefined '
          write(RSNAME(I),'(a40)') TNAME(1:40)
  
C In the case of factorial uncertainty analysis decode result set name.
          if (IMET.eq.2) then
            K=3
            do 722 IFS=1,NFA
              call EGETWI(TNAME,K,IC,0,0,'-','factorial state',IER)
              IFAFLG(I,IFS)=IC
 722        continue
          endif   
          write (6,*) (IFAFLG(I,IXXX),IXXX=1,NFA)   
  
C Read simulation start and finish days and control name.
          IREC=NST1
          READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)ID1,IM1,ID2,
     &                IM2,ISDS,ISDF,NTS,ISAVE,SCNAME,IDAVER,itcnst,
     &                iSlr_half_hr_flg
          write(outs,711)I,SCNAME,ID1,IM1,ID2,IM2,NTS,
     &                                      ISAVE,IDAVER,itcnst,
     &                                      iSlr_half_hr_flg,TNAME
          call edisp(iuout,outs)
  711     format(I3,1X,A12,I3,',',I2,I4,',',I2,I8,I7,I8,I5,2X,I5,2X,A40)
   99   CONTINUE

C There is more than one result set therefore check to see if ISAVE is the 
C same for all sets.
        NS=NSIM
        ITMP=IREC
        DO 10 I=1,NSIM
          IREC=I+1
          TNAME=' '
          READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NST1,NST2,TNAME
          if (izver.eq.0) TNAME=' Undefined '
          write(RSNAME(I),'(a40)') TNAME(1:40)
          IREC=NST1
          READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)ID1,IM1,ID2,IM2,
     &              ISDS,ISDF,NTS,ISAVE,SCNAME,IDAVER,itcnst,
     &              iSlr_half_hr_flg
          if(I.eq.1) ISVO=ISAVE
          if(ISAVE.ne.ISVO) NS=0
   10   CONTINUE
        IREC=ITMP
        if(NS.ne.0) then
          H(1)='It is possible to examine differences between your'
          H(2)='result sets.  This is conventional when conducting'
          H(3)='an uncertainty analysis.'
          if (IMET.eq.1) then
            write (outs,'(2a)')'The current result library holds data ',
     &        'from a differential uncertainty analysis'
            call easkab(outs,'Do you wish to analyse:',
     &        'the uncertainties','a particular set',IW,3)
          elseif (IMET.eq.2) then
            write (outs,'(2a)')'The current result library holds data ',
     &        'from a factorial uncertainty analysis'
            call easkab(outs,'Do you wish to analyse:',
     &        'the uncertainties','a particular set',IW,3)
          elseif (IMET.eq.3) then
            write (outs,'(2a)')'The current result library holds data ',
     &        'from a monte carlo uncertainty analysis'
            call easkab(outs,'Do you wish to analyse:',
     &        'the uncertainties','a particular set',IW,3)
          else
            IW=2
          endif
          if (IW.eq.2) then

C Choose one of the sets, otherwise set one is the base case and all 
C other sets are chosen. 
            H(1)=' Pick a set for analysis.'
            ITMP=1
            ITEMS=NSIM
            CALL EPICKS(ITMP,ITMPSETS,' ',' Which set for analysis: ',
     &                         40,ITEMS,RSNAME,' result-set list',IER,1)
            ISIM=ITMPSETS(1)
            NS=0
          else
            write (outs,'(a,a)') 'Set one from the library will be ',
     &                 'used as the base-case result set, and will also'
            call EDISP(iuout,outs)
            call EDISP(iuout,'be the set used for detailed analysis.')
            ISIM=1
            NS=NSIM
            do 400 IXX=1,NS
              NSNO(IXX)=IXX
              ISETON(IXX)=1
 400        continue
          endif
        else
          H(1)='This results library contains more than one result'
          H(2)='set, each one corresponding to the same system '
          H(3)='configuration but, perhaps, a different period,'
          H(4)='climate or control.'
          H(5)='  '
          H(6)='You are asked to specify the result-set you wish '
          H(7)='to analyse. The default is result-set 1.'
          ITMP=1
          ITEMS=NSIM
          CALL EPICKS(ITMP,ITMPSETS,' ',' Which set for analysis: ',
     &                         40,ITEMS,RSNAME,' result-set list',IER,7)
          ISIM=ITMPSETS(1)

C Set NS to zero to prohibit access to sensetivity analysis section of res.
          NS=0
        endif
      ELSE
        ISIM=1
      ENDIF
      IDEFLT=1

C Read details of this result-set.
      IREC=ISIM+1
      TNAME = '  '
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NST1,NST2,TNAME
      if (izver.eq.0) TNAME=' Undefined '
      write(RSNAME(ISIM),'(a40)') TNAME(1:40)
      ISTADD=NST2
      IREC=NST1
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)ID1,IM1,ID2,IM2,ISDS,
     &ISDF,NTS,ISAVE,SCNAME,IDAVER,itcnst, iSlr_half_hr_flg

C Set|Control   |Start |Finish| Time   | Save |Average|Pre|Aid memoire
C no.| name     | day  | day  |steps/hr|option| Flag  |sim|
      call edisp(iuout,' ')
      write(outs,'(A,A)')' Set|Control    |Start |Finish| Time   ',
     &                  '| Save |Average|Pre|Slr_hr|Aid memoire'
      call edisp(iuout,outs)
      write(outs,'(A,A)')' no.| name      | day  | day  |steps/hr',
     &                  '|option| Flag  |sim| Flag |(for this set)'
      call edisp(iuout,outs)
      write(outs,7)ISIM,SCNAME,ID1,IM1,ID2,IM2,NTS,
     &  ISAVE,IDAVER,itcnst,iSlr_half_hr_flg,TNAME
      call edisp(iuout,outs)
    7 format(I3,1X,A12,I3,',',I2,I4,',',I2,I8,I7,I8,I5,2X,I5,2X,A40)
      IREC=IREC+1
      NCF=0
      IF(SCNAME(1:5).NE.'NONE ') THEN
        NCP=NCOMP
        IF(NCOMP.GT.20)NCP=20
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(ICASCF(I),I=1,NCP)
        IREC=IREC+1
        IF(NCOMP.GT.20) THEN
          READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)
     &                                         (ICASCF(I),I=21,NCOMP)
          IREC=IREC+1
        ENDIF
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)LCTLF
        IREC=IREC+1
      ENDIF

C Reset the output period to that of the whole result set chosen.
      IOH1=1
      IOD1=ID1
      IOM1=IM1
      IOH2=24
      IOD2=ID2
      IOM2=IM2
      IODS=ISDS
      IODF=ISDF
      NOUT=1
      IAV=0

C Reconstruct the results library hash variables before returning.
      CALL HASHL
      RETURN

 1000 if(IOS.eq.2)then
        write(outs,'(A,I5)')
     &    'MORESS: no permission to read library at record',IREC
      else
        write(outs,'(A,I5)')'MORESS: library error at record',IREC
      endif
      call edisp(iuout,outs)
      RETURN
      END

C ******************** MOOPER 
C MOOPER allows user-definition of the output recovery
C period, the output time-step increment and whether or
C not the results should be averaged across the output
C interval.

C Common block variables are:
C IOD1, IOM1  - start day, month and hour numbers.
C & IOH1
C IOD2, IOM2  - finish day, month and hour numbers.
C & IOH2
C IODS & IODF - start and finish year day numbers.
C NOUT        - output time-step increment.  For example,
C               for a 15 minute time-stepping scheme:
C               NOUT = 1 gives output at 15 minute intervals
C               NOUT = 2 gives output at 30 minute intervals
C               NOUT = 3 gives output at 45 minute intervals
C               and so on.
C IAV         - specifies whether results should be averaged over
C               the output interval if NOUT > 1 (IAV=1 yes; IAV=0 no)

      SUBROUTINE MOOPER
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/DEFLT/IDEFLT
      COMMON/C6/INDCFG

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)

      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

      character outs*124,H*72,PDESCR*60
      logical ok,dok

C Default set-up: output results for whole of simulation period at same 
C timestep as simulation, assume that any filtering will be on type 1 
C casual gains (usually occupancy).
      IF(IDEFLT.EQ.1)then
        IOH1=1
        IOD1=ID1
        IOM1=IM1
        IOH2=24
        IOD2=ID2
        IOM2=IM2
        IODS=ISDS
        IODF=ISDF
        NOUT=1
        IAV=0
        CALL HDDATE(PDESCR)
        write(outs,'(1x,a)') PDESCR
        call edisp(iuout,outs)
        call edisp(iuout,' Output time-step interval = 1')

C Only pre-scan library if there is a zone library involved.
        if(indcfg.eq.1.or.indcfg.eq.3)then

C Calculate max, mins and averages of main results parameters.
C Assume that occupancy filter will be set to cas type 1 for res lib scan.
          iocut=1
          IFILT=1
          call usrmsg('Pre-scanning database... ','  ','-')
          call scnocup(iier)
          call MMAPER

C Reset occupancy filter.
          iocut=0
          call scnocup(iier)
          call usrmsg('Pre-scanning database...done. ','  ','P')
          call usrmsg('  ','  ','-')
          return
        endif
      endif

      call edisp(iuout,' Output period and interval definition')

   9  call askper(IFDAY,IER)

C Start time must be before finish time.
      IF(IODS.LT.IODF)goto 18
      IF((IODS.EQ.IODF).AND.(IOH1.GE.IOH2))THEN
        call usrmsg(' On a single day the start time must come',
     &            ' before the finish.  Respecify output period.','W')
        GOTO 9
      endif
      IF(IODS.GT.IODF)THEN
        call usrmsg(' Start day must be equal to or come before',
     &              ' finish day.  Respecify output period.','W')
        GOTO 9
      endif

C Output time-step increment ?
   18 H(1) ='The output time-step increment is an integer greater '
      H(2) ='than 1. It determines the number of simulation '
      H(3) ='time-steps which constitute 1 recovery step.'
      H(4) =' '
      H(5) ='For example, if a one hour time-step simulation was'
      H(6) ='performed, then an output time-step increment of 1 '
      H(7) ='would mean that the recovery interval was also one '
      H(8) ='hour; 2 would mean every 2 hours; and so on.'
      H(9) ='Conversely a simulation increment of 15 minutes and'
      H(10)='output increment of 1 is every 15 minutes, 2 every 30'
      H(11)='minutes; and so on.'
      H(12)=' '
      H(13)='If the output increment is greater than 1, irrespective'
      H(14)='of the simulation time-step, you may opt to average '
      H(15)='across the intra-interval simulation time-step results.'
      H(16)='If the interval is greater than one hour only the'
      H(17)='relevant symbol will be plotted on any graphs.'
      CALL EASKI(NOUT,' ',' Output time-step increment ? ',
     &  1,'F',60,'F',1,'output time-step',IER,17)

      IAV=0
      if (NOUT.gt.1) then
        dok=.false.
        call askok(' ','Average results over each output interval?',
     &    ok,dok,17)
        if(ok)then
          IAV=1
        else
          IAV=0
        endif
      endif

C Calculate max, mins and averages of main results parameters if
C there is a zone library.
      if(indcfg.eq.1.or.indcfg.eq.3)then
        call usrmsg('Rescanning database... ',' ','-')
        call MMAPER
        call usrmsg('Rescanning database...done.',' ','P')
        call usrmsg('  ','  ','-')
      endif

      RETURN
      END

C ******************** MOZDFN ********************
C MOZDFN allows user-definition of the zones to be
C considered on output.
C Common block variables are:
C NZ   - number of zones for output.
C NZNO - zone number of each.

      SUBROUTINE MOZDFN
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/DEFLT/IDEFLT

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      DIMENSION IVALS(MCOM)
      character H*72

C If default setup then link in all zones in the model.
      IF(IDEFLT.EQ.1)then
        NZ=NCOMP
        DO 50 I=1,NZ
          NZNO(I)=I
   50   CONTINUE
        call edisp(iuout,' ')
        call edisp(iuout,' All zones considered for output.')
        RETURN
      endif

C If only one zone then set NZNO and NZ accordingly.
      IF(NCOMP.EQ.1)then
        NZNO(1)=1
        NZ=1
        RETURN
      endif

C Otherwise select the zones from a list of zone names.
      call edisp(iuout,' Output zone definition')
      call edisp(iuout,' ')

    7 if (NS.ne.0) then
        H(1)='  '
        H(2)='Pick one zone for analysis purposes.'
        H(3)='  '
        INPIC=1
      else
        H(1)='Pick one, several or All zones for inclusion in a'
        H(2)='graph or tabular listing from the list provided. '
        H(3)='You may choose them in any order. '
        INPIC=NCOMP
      endif
      CALL EPICKS(INPIC,IVALS,' ',' Which zones to include: ',
     &  12,NCOMP,zname,' zone list',IER,3)
      NZ=INPIC
      DO 40 I=1,INPIC
        NZNO(I)=IVALS(I)
   40 CONTINUE

C Two zones cannot have the same number.
      DO 20 I=1,NZ
        DO 30 J=1,NZ
          IF(I.EQ.J)goto 30
          J1=NZNO(I)
          J2=NZNO(J)
          IF(J1.EQ.J2)then
            call edisp(iuout,' Two zones cannot have the same number.')
            goto 7
          endif
   30   CONTINUE
   20 CONTINUE
      RETURN

      END

C ******************** MOINIT
C MOINIT recovers from the results database selected
C miscellaneous data stored in the header block.
C Common block variables are:
C NCOMP   - number of zones comprising the system to
C           be simulated.
C NCON    - total number of intra-zone connections.
C NFP     - defines the internal plane at
C           which solar energy is injected in the absence
C           of internal insolation information.
C LSNAM   - is the name of the entire system.
C NCCODE  - zone code.
C ZNAME   - zone name (from geometry.h).
C NCONST  - number of multilayered constructions in
C           each zone.
C NELTS   - number of homogeneous elements in each
C           construction.
C NGAPS   - number of air gaps in each construction.
C NPGAP   - the position of each gap (element number
C           counting from 'outside').
C SNA     - net opaque surface area for each surface.
C All other common variables are defined in the subroutine
C where they are first referenced.

C ISTREC is the start record.
C ier is zero if no problems and -3 if user issued a cancel.
      SUBROUTINE MOINIT(ISTREC,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL

      common/recver/izver,ipver,iever
      common/rpath/path
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF

      COMMON/DNORGH/IRTYPE
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/RECV5/SNA(MCOM,MS)
      common/PREC18/ZTRANA(MCOM)

C Nothing references ZGAE and ZGAI.
      COMMON/RECV8/ZGAE(MCOM),ZGAI(MCOM)
      COMMON/RECV9/CHARDM(MCOM,MS)
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/RECV12/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      COMMON/RECVBP/IBP
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      common/hcthry/ihct,icorexhct
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF

      CHARACTER LCFGF*72,CHDUM*72

C longtmp is the model configuration file with full path.
      CHARACTER NAME*6,longtmp*144,H*72,cfgroot*24
      character outs*124,znpad*15,path*72,MODE*4
      character louts*248
      
      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      IER=0
      IUNIT=IFIL
      IREC=ISTREC+2
      IBP=1
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NCOMP,NCON

C If an oder library the start record is probably 32 so if
C the data scan yields a silly number reset IREC and try and
C read again.
      if(NCOMP.gt.MCOM.or.NCON.gt.MCON)then
        IREC=30+2
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NCOMP,NCON
        if(NCOMP.gt.MCOM.or.NCON.gt.MCON)then
          write(outs,*) 'WARNING: ncomp at record was dubious',NCOMP,
     &       IREC
	  call edisp(iuout,outs)
	  call edisp(iuout,'this results file needs to be scanned')
	  call edisp(iuout,'with an older version of res.')
        endif
      endif
      IREC=IREC+1
      IF(NCOMP.GT.0)goto 3
      IBP=2
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)NCOMP,NSUR,NELT,
     &NWIN,NDOOR
      IREC=IREC+1
      DO 300 LL=1,NCOMP
        NCONST(LL)=NSUR
        NZSUR(LL)=NSUR
        DO 301 KK=1,NSUR
          NELTS(LL,KK)=NELT
  301   CONTINUE
  300 CONTINUE
      goto 4
    3 READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)LSNAM
      IREC=IREC+1
      NCP=NCOMP
      IF(NCP.GT.20)NCP=20
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NCCODE(I),I=1,NCP)
      IREC=IREC+1
      IF(NCOMP.LE.20)goto 4
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NCCODE(I),I=21,NCOMP)
      IREC=IREC+1

C Recover CLIMATE file name used at simulation time.
C << if there is an error it might be because of the version
C << of the database file

    4 READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)LCLIM
      IREC=IREC+1

C Jump over header block records containing data not
C required at present.
      IF(IBP.EQ.2)RETURN
      X=FLOAT(NCON)/20.
      IJ=INT(X)+1
      DO 1030 I=1,NCOMP
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)LPROJ(I)
      IREC=IREC+1

 1030 CONTINUE

C Added to save the name of each zone geometry file in LGEOM(MCOM).
      DO 1031 I=1,NCOMP
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)CHDUM
        LGEOM(I)=CHDUM
        IREC=IREC+1
 1031 CONTINUE

C Added to save the name of each zone LSHAD file.
      DO 1032 I=1,NCOMP
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)LSHAD(I)
        IREC=IREC+1
 1032 CONTINUE

C Added to save the name of each zone thermal properties file.
      DO 1033 I=1,NCOMP
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)LTHRM(I)
        IREC=IREC+1
 1033 CONTINUE

C Other file names read in so next line not required.
C     IREC=IREC+3*NCOMP
      DO 104 I=1,IJ
        J1=I*20-19
        J2=I*20
        IF(I.EQ.IJ)J2=NCON
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(IC1(J),J=J1,J2)
        IREC=IREC+1
  104 CONTINUE
      DO 105 I=1,IJ
        J1=I*20-19
        J2=I*20
        IF(I.EQ.IJ)J2=NCON
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(IE1(J),J=J1,J2)
        IREC=IREC+1
  105 CONTINUE
      DO 106 I=1,IJ
        J1=I*20-19
        J2=I*20
        IF(I.EQ.IJ)J2=NCON
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(ICT(J),J=J1,J2)
        IREC=IREC+1
  106 CONTINUE
      DO 107 I=1,IJ
        J1=I*20-19
        J2=I*20
        IF(I.EQ.IJ)J2=NCON
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(IC2(J),J=J1,J2)
        IREC=IREC+1
  107 CONTINUE
      DO 108 I=1,IJ
        J1=I*20-19
        J2=I*20
        IF(I.EQ.IJ)J=NCON
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(IE2(J),J=J1,J2)
        IREC=IREC+1
  108 CONTINUE
      DO 20 I=1,NCOMP
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)znpad
        write(ZNAME(I),'(a)') znpad(1:12)
	lnzname(I)=lnblnk(znpad)  ! update string length
        IREC=IREC+1
   20 CONTINUE
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NCONST(I),I=1,NCP)

C Also assing NZSUR common.
      do 1108 i=1,NCP
        NZSUR(i)=NCONST(i)
 1108 continue
      IREC=IREC+1

      IF(NCOMP.LE.20)goto 202
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NCONST(I),I=21,NCOMP)
      do 1109 i=21,NCOMP
        NZSUR(i)=NCONST(i)
 1109 continue

C Debug...
C      write(6,*) 'nconst=',nconst
C      write(6,*) 'nzsur=',nzsur

      IREC=IREC+1
  202 DO 30 I=1,NCOMP
        NN=NCONST(I)
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NELTS(I,J),J=1,NN)
        IREC=IREC+1
   30 CONTINUE
      DO 40 I=1,NCOMP
        NN=NCONST(I)
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NGAPS(I,J),J=1,NN)
        IREC=IREC+1
   40 CONTINUE
      DO 50 I=1,NCOMP
        NN=NCONST(I)
        DO 60 J=1,NN,5

C Read for all three possible air gaps.
          if(J.LE.(NN-5))then
            READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)
     &        (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP),
     &        (NPGAP(I,J+2,K),K=1,MGP),(NPGAP(I,J+3,K),K=1,MGP),
     &        (NPGAP(I,J+4,K),K=1,MGP)
          elseif(J.eq.(NN-4))then
            READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)
     &        (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP),
     &        (NPGAP(I,J+2,K),K=1,MGP),(NPGAP(I,J+3,K),K=1,MGP)
          elseif(J.eq.(NN-3))then
            READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)
     &        (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP),
     &        (NPGAP(I,J+2,K),K=1,MGP)
          elseif(J.eq.(NN-2))then
            READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)
     &        (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP)
          elseif(J.eq.(NN-1))then
            READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)
     &        (NPGAP(I,J,K),K=1,MGP)
          endif
          IREC=IREC+1
   60   CONTINUE
   50 CONTINUE

C If results library is version two or earlier then skip unused records.
C else read in transparent and floor areas as described.
      if (izver.lt.3) then
        IREC=IREC+NCOMP+NCOMP
      else
      
C Read floor area of each zone (this is actually already available
C in common block prec17 defined in geometry.h. Potentially we
C do not need to read this array here. But later if we decide not
C to scan geometry files this line would be needed.
C << this is a place where record width is tied to number of zones! >>
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(ZBASEA(I),I=1,NCOMP)
        IREC=IREC+1
C        write (6,*) 'FLOOR A:',IREC,(ZBASEA(I),I=1,NCOMP)
      
C Read area of transparent surfaces connected to external boundary.
C << this is a place where record width is tied to number of zones! >>
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(ZTRANA(I),I=1,NCOMP)
        IREC=IREC+1
C        write (6,*) 'TRAN A:',IREC,(ZTRANA(I),I=1,NCOMP)
      endif

C Read surface areas in each zone.
      DO 90 I=1,NCOMP
        NN=NCONST(I)
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(SNA(I,J),J=1,NN)
        IREC=IREC+1
   90 CONTINUE

C ZOA Not needed so skip ncomp records
C << could be location of other one-per-zone data >>.
      DO 100 I=1,NCOMP
        IREC=IREC+1
  100 CONTINUE
      DO 110 I=1,NCOMP
        NN=NCONST(I)
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(CHARDM(I,J),J=1,NN)
        IREC=IREC+1
  110 CONTINUE
      DO 120 I=1,NCOMP
        NN=NCONST(I)
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(PAZI(I,J),J=1,NN)
        IREC=IREC+1
  120 CONTINUE
      DO 130 I=1,NCOMP
        NN=NCONST(I)
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(PELV(I,J),J=1,NN)
        IREC=IREC+1
  130 CONTINUE
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NHCFP(I),I=1,NCP)
      IREC=IREC+1
      IF(NCOMP.LE.20)goto 203
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(NHCFP(I),I=21,NCOMP)
      IREC=IREC+1
  203 DO 150 I=1,NCOMP
       IF(NHCFP(I).EQ.0)goto 150
       NN=NCONST(I)
       NP=NHCFP(I)
       DO 160 K=1,NP
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)HCFPST(I,K),
     &      HCFPFT(I,K),(HCFVI(I,J,K),J=1,NN)
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)(HCFVE(I,J,K),J=1,NN)
        IREC=IREC+1
  160  CONTINUE
  150 CONTINUE

C Read inside and outside indices of heat transfer coef. (by default
C each is one)
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)IHCT,ICOREXHCT
      IREC=IREC+1

C Read the full path of the configuration file associated with
C this model.
      READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1001)NAME
      IF(NAME.EQ.'CONFIG')THEN
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1006)longtmp

C Debug.
C        write(6,*) longtmp

        IREC=IREC+1
      ELSE
 45     H(1)='The system configuration file name is being requested'
        H(2)='because this is an old library format which does not '
        H(3)='include this. If you cannot find this file then you'
        H(4)='cannot proceed.'
        longtmp=' '

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then we need the full length to avoid truncation.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',
     &      ' Corresponding system configuration file name ? ',
     &      96,' ','configuration file',IER,4)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',
     &      ' Corresponding system configuration file name ? ',
     &      144,' ','configuration file',IER,4)
        else
          CALL EASKF(longtmp,' ',
     &      ' Corresponding system configuration file name ? ',
     &      96,' ','configuration file',IER,4)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if (longtmp.eq.' ') goto 45
      ENDIF
      
C Extract path from cfg file name and check that we can find it.
 289  call fdroot(longtmp,path,LCFGF)
      IFCFG=IFIL+1
      IAPROB=IFIL+2
      CALL ERPFREE(IFCFG,ISTAT)
      call FINDFIL(LCFGF,XST)
      IF(XST)THEN
        MODE='ALL'
        CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,ITRC,IER)
        IF(IER.NE.0)THEN
          WRITE(OUTS,'(A,2X,A)')' Problem reading ',LCFGF
          call edisp(iuout,outs)
        ELSE
          CFGOK=.TRUE.
        ENDIF

C If izver is >=4 then the only information needed from the zone
C operation file is the user name for each casual gain type. Scan
C this information.
        DO 43 IZONE=1,NCOMP
          CALL EROPER(0,iuout,IAPROB,izone,IER)
  43    CONTINUE
      ELSE

C Not found, might be in a remote folder.  Warn user first.
        WRITE(OUTS,'(A,A)')' Could not find ',LCFGF
        call edisp(iuout,outs)
        WRITE(OUTS,'(A,A)')' in the folder ',path
        call edisp(iuout,outs)
        H(1)='The model configuration file holds the definition of'
        H(2)='the building/plant which was simulated.'
        H(3)='This file was not found.  Please supply the model '
        H(4)='which MATCHES that within the results library.'
        H(5)='IF IT DOES NOT MATCH THEN RESULTS RECOVERY '
        H(6)='WILL BE COMPROMISED! '
        CALL PHELPD('no system file',6,'-',0,0,IER)

        H(1)='The model configuration file holds the definition '
        H(2)='of the building/ plant to be simulated.  It should'
        H(3)='match the contents held within the results library.'
        write(longtmp,'(a)') LCFGF(1:lnblnk(LCFGF))

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then we need the full length to avoid truncation.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' Model configuration file ?',' ',96,
     &      DFCFG,'config file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' Model configuration file ?',' ',144,
     &      DFCFG,'config file name',IER,3)
        else
          CALL EASKF(longtmp,' Model configuration file ?',' ',96,
     &      DFCFG,'config file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if (longtmp(1:2).ne.'  ') GOTO 289
      ENDIF

C The following line reads ncomp records which will contain zeros
C (because there are no default doors and windows). ZGAE and ZGAI
C are not referenced. Could we use these slots for something else?
      DO 140 I=1,NCOMP
        READ(IUNIT,REC=IREC,IOSTAT=ios,ERR=1000)ZGAE(I),ZGAI(I),
     &  xdum,xdum
        IREC=IREC+1
  140 CONTINUE
  
C Finally check if the climate file read in above exists.
   44 IUNITC=IFIL+5
      call EFOPRAN(IUNITC,LCLIM,144,0,IER)
      IF(IER.LT.0)then

C Climate file not found - ask user for name.
        write(louts,'(3A)') ' Referenced climate file ',
     &     LCLIM(1:lnblnk(LCLIM)), ' not found...'
        call edisp248(iuout,louts,90)
        call erpfree(IUNITC,istat)
        H(1)=' A climate file defines the model boundary'
        H(2)=' condition. The one included in the results libary'
        H(3)=' was not found, type in the correct name.'
        longtmp=LCLIM

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then we need the full length to avoid truncation.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,3)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if(longtmp(1:2).ne.'  '.and.longtmp(1:4).ne.'UNKN')then
          LCLIM=longtmp
        endif
        goto 44
      endif

C Also recover the solar radiation flag from the climate file.

C << Note this sets the year from the climate file scan!  What
C << if the model uses a different year?  Which one is used?

      IRECC=366
      READ(IUNITC,REC=IRECC,IOSTAT=ios,ERR=102)IYEAR,ICDNGH
      IRTYPE=0
      IF(ICDNGH.EQ.123)IRTYPE=1

    2 RETURN

  102 if(IOS.eq.2)then
        call usrmsg(' ',
     &    'No permission to read year from climate file.','W')
      else
        call usrmsg(' ','Error reading year from climate file.','W')
      endif
      ier=-4
      goto 2
 1000 if(IOS.eq.2)then
        write(outs,'(A,I4)')'MOINIT: no permission to read @ rec ',IREC
      else
        write(outs,'(A,I4)')'MOINIT: read error @ rec ',IREC
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      ier=-5
      goto 2
 1001 if(IOS.eq.2)then
        write(outs,'(A,I4)')
     &    'MOINIT: no permission to read model name @ rec ',IREC
      else
        write(outs,'(A,I4)')
     &    'MOINIT: error getting model name @ rec ',IREC
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      ier=-5
      goto 2

 1006 call usrmsg(' Could not read name of configuration file',
     &            ' from the results library.','W')
      goto 1000

      END

C ******************** MOCHEK 

C MOCHEK ensures that the requested output period
C information is consistent with the simulation period
C information associated with the currently assigned
C result-set. The user-defined zones for output are
C also checked for consistency.

      SUBROUTINE MOCHEK(IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      dimension ITEMP(MCOM)

      IER=0

C Check period information.
      IF(IODS.LT.ISDS)then
        call edisp(iuout,' Output start day is before the simulation')
        call edisp(iuout,' start day.  Please respecify.')
        IER=1
      endif
      IF(IODF.GT.ISDF)then
        call edisp(iuout,' Output finish day is after the simulation')
        call edisp(iuout,' finish day.  Please respecify.')
        IER=1
      endif

C Output time-step increment cannot be set so that output
C interval is greater than 24 hours.
      TINT=FLOAT(NOUT/NTS)
      IF(TINT.GT.24.)then
        call edisp(iuout,' Given the simulation time-step associated')
        call edisp(iuout,' with the currently defined result set, this')
        call edisp(iuout,' output time-step increment will result in a')
        call edisp(iuout,' display time-increment greater than the')
        call edisp(iuout,' maximum allowed (24 hours). ')
        IER=1
      endif

C Check zone information.
      IF(NZ.GT.NCOMP)then
        call edisp(iuout,' Number of zones for output exceeds number')
        call edisp(iuout,' of zones in simulation.')
        IER=1
      endif

      DO 10 I=1,NZ
        IF(NZNO(I).GT.NCOMP)then
          call edisp(iuout,' A zone number is greater than the')
          call edisp(iuout,' number of zones considered in totals')
          call edisp(iuout,' in the previous simulation.')
          IER=1
        endif
   10 CONTINUE
      DO 20 I=1,NZ
        DO 30 J=1,NZ
          IF(J.EQ.I)goto 30
          IF(NZNO(I).EQ.NZNO(J))then
            call edisp(iuout,' Two zones cannot have the same number.')
            IER=1
          endif
   30   CONTINUE
   20 CONTINUE
      IF(IER.EQ.1.OR.NZ.EQ.1)RETURN

C Set zone numbers in ascending order.
      KFLAG=1
      call SORTI(NZNO,ITEMP,NZ,KFLAG)
      RETURN
      END

C ******************** MOFREE ********************

      SUBROUTINE MOFREE
      RETURN
      END
