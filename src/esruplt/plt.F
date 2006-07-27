C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

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
C      ESRUPLT
C      MZFREE

c ******************** ESRUPLT ******************

c The main program initialises the user terminal and the
c user-specified climate database and displays the main menu.

c Common block variables are:

c IER1    - input error index which ensures that those menu picks
c           which are dependent on input data cannot be selected
c           if any error was detected during the input sequence

c IUOUT & - write and read unit numbers set via the following data
c IUIN      statement

c ICOUT   - write unit for rule scripts.

C IFIL    - basic file unit number. Actual file unit numbers are:
C           IFIL+0  climate file
C           IFIL+1  any temporarily opened file
C           IFIL+2  building results library
C           IFIL+3  plant results library
C           IFIL+4  mass flow results file
C           IFIL+5  first NOCHAN (occupied utility file channel)

c MMOD    - index returned from the 'PAGES' routine
c           and determines the device type attached to current job.

c ESRUplt's operational details are given in section 4
c of the ESP user's manual available from ESRU, University
c of Strathclyde.

c Theoretical basis is explained, in detail, in the following
c reference:

c Clarke J A `Energy Simulation in Building Design'
c Adam Hilger Ltd, Bristol and Boston, June 1985.

      program plt
#include "building.h"
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
      COMMON/FILEP/IFIL
      COMMON/TC/ITC,ICNT
      common/trc/itrc
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/ER1/IER1
      COMMON/WRNOFF/IWMOFF
      COMMON/HELP/IHELPM
      COMMON/AVRAGE/IDAVER

C Record lengths for zone (nzrl), plant (nprl), electrical(nerl) results libraries.
      common/reclen/nzrl,nprl,nerl
      COMMON/C22/ICLIM,LCLIM

C Path to problem and command line file (if any).
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/rcmd/LCMDFL

C Where default db list is kept.
      common/defdb/dfdblbl,defdbfl

C Defaults.
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      character*72 DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF

      LOGICAL     OK,dok,unixok

      CHARACTER*27 ITEM(9)
      character H*72,ltmp*72,lclim*72,outs*144,inf*144,LCMDFL*144
      character path*72,upath*72,esprc*72,cupath*72,outs248*248
      character dfdblbl*20,defdbfl*72,fs*1

c Record length of binary results file
c Parameter nprecl defined in 'plant.h'.
      nprl = nprecl

C Some initial values, which may be changed later:
      call ezero
      call curmodule('plt ')
      IUIN=5
      IUOUT=6
      ICOUT=0
      IFIL=10
      ITC=0
      itrc=0
      ICNT=1
      ITCF=0
      ITU=IUOUT
      IWMOFF=1
      IHELPM=0
      IFS=1
      ITFS=1
      IMFS=1
      call partf(MODL,inf)

C No window size and postion are passed via partf so
C set these so that a standard size is used.
      iappw=0
      iappx=10
      iappy=10

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Initialise results averaging flag: 0 means
C results will be averaged.
      IDAVER=0

C No input achieved as yet.
      INP=0

C If your compiler does not support floating-point arithmetic
C according to ANSI/IEEE Std 754-1985 comment out the following code.

C Initialize mode, status, and signal handling for IEEE arithmetic.
C See also: f77_ieee_environment(3F) f77_floatingpoint(3F)
C           Note that in case of Sun FORTRAN, IEEEE exceptions arise
C           only if: Sun-3 f77 with -f68881 or -ffpa option
C                or: Sun-4 f77.
C      IEEER=IEEE_HANDLER('set','common',SIGFPE_ABORT)
C      IF(IEEER.NE.0) WRITE(IUOUT,*) ' IEEE_handler not set !'

C Initialise output device: -10 forces user selection.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=580
        iappx=75
        iappy=120
      else
        if(iappx.le.0)iappx=75
        if(iappy.le.0)iappy=120
        if(iappw.le.200)then
          iappwi=int(580*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(30*iappw*0.01),24)
        LIMTTY=20
        LIMIT =20
      else
        menuchw = 30
        LIMTTY=20
        LIMIT =20
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1
        IFS=1
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r Plant Simulator: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(30,7,7,2,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
        call opentutorial
        call opensetup
        call updcapt(1)
#endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF
      IF(MMOD.NE.-6)ICOUT=IUOUT
      call edisp(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Stand-alone plant: Version 2.22a of July 2006.' ,
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow, Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,' ')

C Find the users home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take command line file name as  initial file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs,80)
      else
        LCMDFL='UNKNOWN'
      endif

C Inital climate assumption (to default).
      LCLIM=DCLIM
      ICLIM=IFIL

      INO=-2

C Set climate data file (file must exist - unit
C number = ifil). If found, attempt to open and test that it is
C a climate file by reading records 366 and 367.
    8 H(1)=' A climate file defines the problem boundary'
      H(2)=' condition. Usually a number of files will'
      H(3)=' exist and these can be analysed using ESRU: clm.'
      H(4)=' '
      H(5)=' Any climate file can be entered at this point. '
      H(6)=' For example, on computers running UNIX, the file '
      H(7)=' [~esru/esp-r/default] holds the name of a '
      H(8)=' default climate file. This might define a TRY or '
      H(9)=' example year. '
   35 iclim=ifil
      call erpfree(iclim,istat)
      LTMP=LCLIM
      CALL EASKS(LTMP,' Climate file name ?',
     &    ' ',72,DCLIM,'climate file name',IER,9)
      IF(LTMP.EQ.' ')GOTO 35
      LCLIM=LTMP
      CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
      IF(ISTAT.GE.0)THEN
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)IYEAR,IRT
        IREC=367
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)CLMLOC
        WRITE(OUTS,1000)CLMLOC,IYEAR
 1000   FORMAT(' Climate data: ',A30,':',I5)
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,OUTS)

C << IRTYPE not used >>
        IRTYPE=0
        IF(IRT.EQ.123)IRTYPE=1
      ELSE
        dok=.false.
        h(1)='There should not be any problem with the climate data'
        h(2)='associated with this model. Please check this before '
        h(3)='attemtping to run the simulation again. '
        CALL ASKOK('Problem encountered with climate data...try again?',
     &             '(no will exit application)',OK,dok,3)
        IF(OK)THEN
          goto 35
        ELSE
          CALL EPAGEND
          CALL ERPFREE(ICLIM,ISTAT)
          STOP
        ENDIF
      ENDIF

c So far no input errors. List help
   14 IER1=0

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('plt ',1)

c And menu is to be displayed.
      INO=-5
      ITEM(1)='1 Input description '
      ITEM(2)='2 Initiate simulation'
      ITEM(3)='3 Assign climate file'
      ITEM(4)='   ............      '
      ITEM(5)='t Trace facilities'
      IF(IWMOFF.EQ.1)THEN
        ITEM(6) ='w Warnings >> OFF      '
      ELSEIF(IWMOFF.EQ.0)THEN
        ITEM(6) ='w Warnings >> ON       '
      ENDIF
      IF(ITRC.EQ.0)THEN
        ITEM(7)='r Reporting >> silent  '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(7)='r Reporting >> summary '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(7)='r Reporting >> detailed'
      ENDIF
      ITEM(8)='? Help facility'
      ITEM(9)='f Finish and exit         '
      nitms=9

C Help text for menu.

   15 CALL EMENU('ESRUplt',ITEM,NITMS,INO)

c Test for illegal menu pick.
      IF(INO.EQ.0.OR.INO.EQ.4)GOTO 11
      IF(INO.EQ.8)then

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('plt ',0)
      endif

c Do not allow those menu picks which are input data dependent
c unless input has been successfully completed.
      IF(INP.EQ.0.AND.(INO.EQ.2))GOTO 12
      IF(INO.EQ.1)CALL MZINPT

c Now increment input flag - but only if no input data errors
c have been detected.
      IF(INO.EQ.1.AND.IER1.EQ.0)INP=1

C The following statement is ignored in plant only version.
      IF(INO.EQ.2)CALL MZSIML
      IF(INO.EQ.3)GOTO 8
      IF(INO.EQ.5)CALL MZTRAC
      IF(INO.EQ.9)GOTO 13
      IF(INO.EQ.6)THEN

C Warning messages switch.
        dok=.false.
        h(1)='You have the option to display warning messages '
        h(2)='(issues which are not fatal, but might affect the '
        h(3)='simulation results). '
        CALL ASKOK(' ',' Warning messages required?',OK,dok,3)
        IF(OK)THEN
          IWMOFF=0
          IF(MMOD.NE.-6) ICOUT=99
        ELSE
          IWMOFF=1
        ENDIF
      ENDIF

      if(ino.eq.7)then

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      endif

C Re-display the menu.
      GOTO 14

  103 dok=.false.
      h(1)='There should not be any problems with the climate file'
      h(2)='when running an assessment. Check your model. A no will'
      h(3)='exit the application. '
      CALL ASKOK(' Problem in climate file, try again? ',
     &            ' (no exits program) ',OK,dok,3)
      IF(OK)THEN
        goto 14
      ELSE
        CALL EPAGEND
        CALL ERPFREE(ICLIM,ISTAT)
        STOP
      enDIF

c Output menu error signal and force re-selection from menu.
   11 INO=-1
      GOTO 15
   12 CONTINUE

C Do not allow simulation unless input has been successfully completed.
          call usrmsg(' ',' Configuration file not yet entered.','W')
      GOTO 14
   13 CONTINUE
        call usrmsg(' Closing plant module...',' ','P')
      if(itu.eq.33) call erpfree(itu,istat)
      CALL ERPFREE(ICLIM,ISTAT)
      CALL EPAGEND
      STOP
      END

C ******************** MZFREE ********************

C A routine to free all assigned file channels.

      SUBROUTINE MZFREE(IFIL)
#include "building.h"
C     LOGICAL OPN

      DO 10 I=IFIL-2,IFIL+5+(2*MCOM)
C     ICH=I
C     INQUIRE(ICH,OPENED=OPN)
C     IF(OPN)CALL ERPFREE(ICH,ISTAT)
   10 CONTINUE
      RETURN
      END


C Dummy routines (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(ITRC,IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
      return
      end

      subroutine SVDSOPT
      return
      end

      subroutine RCDSOPT
      return
      end

      SUBROUTINE DRAWOBS(IFOC,ier)
      return
      end

      SUBROUTINE DRWSEN(ier)
      return
      end

      SUBROUTINE EGRNDR(IER)
      return
      end

      SUBROUTINE DSGRID(RH,GD,LD,IER)
      return
      end

      SUBROUTINE EMKVIEW(ITRC,IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(IUF,direc,ier)
      CHARACTER direc*1
      ier=0
      return
      end

C ***** dummy routines for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end
