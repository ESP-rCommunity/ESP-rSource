C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C ******************** Program mfs ********************
C Simulates fluid flows within building & plant configurations.
C Solves a network representing one-dimensional fluid
C flow in building and/or plant configurations.
C This involves the calculation of fluid flow through branches and nodes
C in a network which represents a building and/or plant configuration.
C The nodes and branches describe boundary pressure points, internal
C node characteristics and flow restrictions.
C Mfs uses an iterative mass balance approach in which nodal
C pressures (of unknown-pressure nodes) are adjusted until the mass
C residual of each internal node satifies some criterion.

C Local variables are:
C     IUOUT  - write unit (standard output).
C     IUIN   - read  unit (standard input).
C     IMFTU  - write unit for fluid flow calculation trace information.
C     ICOUT  - write unit for (non-fatal) warning messages.
C     MMOD   - determines the device type attached to the current job.
C Global COMMON variables are described in emfnetw.F.

      program mfs
      USE START_UP
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C ihflag = 0 write 13h30, ihflag = 1 write 0.5625
C idhflg = 0 no day demarcations, idhflg = 1 write demarcation
C          between tabular reporting days.
C ilflag = 0 tabular labels on multi-lines, ilflag = 1 on one line.
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG

C Path to problem
      common/rpath/path

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Activation of connections
      COMMON/ACT/ACTCNN(MCNN)
      character*72 LAPROB
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFCALC/IPSMOD,MSLVTP

C Once per hour storage of results, averaged over that hour.
      common/c6/indcfg
      common/rdcgen/irdact
      common/rdcmfs/irdcm,sFLWTIM,sDRYB,sWDIR,sWSPD,sITER,sIOK,
     &              sTNOD(MNOD)

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

      LOGICAL LAST,OK,unixok,there,ACTCNN
      CHARACTER*24 ITEM(18)
      CHARACTER OUTSTR*124,inf*144,outs*124
      character path*72
      character tg*1,delim*1,xfile*144
      character journcmd*20,jfile*72,fs*1,cjfile*72
      character dstmp*24,uname*24,tfile*72

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*72 ! for banner title via epages call

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer IWM     ! for radio button
      integer NITMS,INO ! max items and current menu item
      integer lnafn   ! length of flow network file name

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture   ! to pass to updcapt
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture   ! to pass to updcapt
#endif

C Initialize global common data.
      call ezero
      call curmodule('mfs ')
      helpinapp='mfs'  ! set once for the application
      helpinsub='mfs'  ! set for MAIN
      IUOUT=6
      IUIN=5
      IFS=1
      ITFS=1
      IMFS=1
      LIMTTY=30
      LIMIT =30
      IDAYF=0
      IDAYL=0
      IAPRES=11
      LAPROB = '  '   ! clear name of flow network
      AUTOVR=.false.  ! by default, ask to overwrite files

      cAppName = 'mfs'

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Export (grtool) file information.
      xfile = 'data.grt'
      ixopen = 0
      ixunit = 16
      ixpunit = 17
      iuj = 20
      delim='-'
      call parpsf(MODL,iappw,iappx,iappy,inf)

C If the command line file name something other than UNKNOWN
C remember it.
      lnafn = lnblnk(inf)
      if(inf(1:4).ne.'UNKN')then
        if(lnafn.le.72)then
          write(LAPROB,'(a)')inf(1:lnafn)
        else
          write(LAPROB,'(a)')inf(1:72)
          call usrmsg('The file name passed in command line was',
     &      'greater than 72 characters. Might be a problem.','W')
        endif
      endif

C File unit numbers.
      IFPRB=10
      IAPRES=11
      IFCLM=12
      IFRES=13
      IUA=14

C Default value for GRTOOL file = 0 (off), day hash marks off and
C tabular labels on multiple lines.
      IHFLAG=0
      IDHFLG=0
      ILFLAG=0

C Assume most legible day and time formats.
      IFDAY=2
      IFTIME=0

C Stand-alone version of mfs. Set INDCFG accordingly.
      indcfg=0

C Initialize once per hour storage of results counter.
      irdact=0
      irdcm=1
      IPSMOD=1  ! initial assumption
      MSLVTP=2  ! LU solver as in mfsbps.F

C Activate all connections.
      DO 1 ICNN=1,MCNN
        ACTCNN(ICNN)=.TRUE.
   1  CONTINUE

C If your compiler does not support floating-point arithmetic
C according to ANSI/IEEE Std 754-1985 comment out the following code,
C which initializes mode, status, and signal for IEEE arithmetic.
C See also: f77_ieee_environment(3F) f77_floatingpoint(3F)
C           Note that in case of Sun FORTRAN, IEEEE exceptions arise
C           only if: Sun-3 f77 with -f68881 or -ffpa option
C                or: Sun-4 f77.
C      IEEER=IEEE_HANDLER('set','common',SIGFPE_ABORT)
C      IF(IEEER.NE.0)call edisp(iuout,
C     &  ' mfs: arithmetic exceptions are ignored !')

C Determine terminal type and set write unit to stderr for rule scripts.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=580
        iappx=85
        iappy=80
      else
        if(iappx.le.0)iappx=85
        if(iappy.le.0)iappy=80
        if(iappw.le.200)then
          iappwi=int(580*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/725.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(27*iappw*0.01),16)
        LIMTTY=8
        LIMIT =8
      else
        menuchw = 27
        LIMTTY=8
        LIMIT =8
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for menus
C and dialog.
        IMFS=5
        IFS=5
#ifdef OSX
        IFS=4  ! use a smaller dialog font
#endif
        ITFS=4
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        IMFS=5 ! Text mode set LIMTTY larger for paging menus.
        IFS=5
        ITFS=4
        LIMTTY=30
        LIMIT =30
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Network flow simulator of ESP-r ',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=8; iiw2=8; iiw3=7; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          iicapture=1
          call updcapt(iicapture)
        endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

C Set additional output units to stdout. Then redirect warning 
C messages to stderr in case of rule script program control.
      IMFTU=IUOUT
      ICOUT=IUOUT
      IF(MMOD.EQ.-6) ICOUT=0

C Show version
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get user's custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IUA,0,IIER)

C If there is a journal active set up to add to the journal. First
C get the root journal file (setup by prj) from the users home folder.
C All subsequent tstamp messages will be appended to root journal.
      if(journio.eq.1)then
        uname=' '
        call usrname(uname)
        cjfile=' '
        write(cjfile,'(a,a1,3a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),'cur_j'
        there=.false.
        INQUIRE(FILE=cjfile,EXIST=there)
        if(there)then
          close(iuj)
          open(iuj,file=cjfile,status='UNKNOWN',err=901)
          CALL STRIPC(iuj,OUTSTR,0,ND,1,'header',IER)
          if(OUTSTR(1:15).eq.'Current_Journal')then
            k=16
            CALL EGETRM(OUTSTR,K,jfile,'W','root journ',IER)
          endif
          close(iuj)
        endif

        write(outs,'(a,a)')'Journal (network flow analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=942          ! set to unused index 942
      open(ieout,file=iefile,status='UNKNOWN',err=904)
      write(ieout,'(a)')'Session log for mfs'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  904 continue

C Scan the defaults file for default pressure and weather.
C Make temporary use of file unit IUA.
 902  call escdef(IUA,'s',IER)

C Use defaults for pressure coef and weather if they are available.
      if(ier.eq.0)then
        write(LAPRES,'(a)') DAPRES(1:lnblnk(DAPRES))
        write(LCLIM,'(a)')  DCLIM(1:lnblnk(DCLIM))
      endif

C Fill valid component type arrays
C Set defaults for iteration parameters, Pstack calculation model,
C                  matrix solver type and trace output flag
C Assign and attach problem description (IFPRB), wind pressure
C distribution (IAPRES), climate (IFCLM) and results (IFRES) files
      CALL MFINIT(IFPRB,IFCLM,IFRES)

C Let the user pick a menu item.
   10 INO=-5
      ITEM(1) ='a define problem       '
      ITEM(2) ='b initiate simulation  '
      ITEM(3) ='c tabular output       '
      ITEM(4) ='d graphical output     '
      ITEM(5) ='  ____________________ '
      ITEM(6) ='i iteration parameters '
      ITEM(7) ='s stack assumptions    '
      ITEM(8) ='m solver type          '
      ITEM(9) ='t trace facilities     '
      ITEM(10)='w warning messages     '
      ITEM(11)='  ____________________ '
      ITEM(12)='  ... Formating ...    '
      if(ixopen.eq.1)then
        ITEM(13)='> output >> file     '
      elseif(ixopen.eq.0)then
        ITEM(13)='> output >> screen   '
      endif
      if(IHFLAG.eq.0)then
        ITEM(14)='* time >> 10h30      '
      elseif(IHFLAG.eq.1)then
        ITEM(14)='* time >> 0.4375     '
      endif
      if(delim.eq.'-')then
        ITEM(15)='^ delimiter >> normal'
      elseif(delim.eq.'T')then
        ITEM(15)='^ delimiter >> tab   '
      elseif(delim.eq.'C')then
        ITEM(15)='^ delimeter >> comma '
      elseif(delim.eq.'S')then
        ITEM(15)='^ delimiter >> space '
      endif
      ITEM(16)='* preferences          '
      ITEM(17)='? help                 '
      ITEM(18)='- exit module          '
      NITMS=18

C Use askabout to instantiate the initial help messages (2nd parameter is one).
   12 call askabout('mfs ',1)

      CALL EMENU('Network flow simulation',ITEM,NITMS,INO)

C Exit module.
      IF(INO.EQ.NITMS)THEN
        CALL ERPFREE(IFPRB,ISTAT)
        call tstamp('>','Finish network flow analysis')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        CALL EPAGEND
        STOP

C Menu help.
      elseif(INO.EQ.NITMS-1)then
        call askabout('mfs ',0)

C Preferences.        
      elseif(INO.EQ.NITMS-2)then
        CALL SETUP(ITRC,iuout,IER)

C Change problem description.
      elseif(INO.EQ.1)then
        CALL MFINIT(IFPRB,IFCLM,IFRES)
        call tstamp('>','MFS: change network file')

C Invoke simulation.
      elseif(INO.EQ.2)then
        GOTO 30

C Tabular output.
      ELSEIF(INO.EQ.3)THEN
        CALL MFLOAD(IFPRB,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG('Problem encountered while loading the',
     &                'mass flow network description.','W')
          GOTO 10
        ENDIF
        IF(NNOD.LT.2.OR.NCMP.LT.1.OR.NCNN.LT.1)THEN
          CALL USRMSG('Insufficient nodes and connections',
     &    'to create a network.','W')
          GOTO 10
        ENDIF
        CALL MFCRIM(IFRES,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG('Mismatch between the network description',
     &                'and results file.','W')
          GOTO 10
        ENDIF

        helptopic='tabular_format_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Tabular output:',
     &              'formatted','raw',IW,nbhelp)
        IF(IW.EQ.1) THEN
          call tstamp('>','MFS: enter pretty print reporting')
          CALL MFTABL(itrc,IFRES,ier)
          call tstamp('>','MFS: exit pretty print reporting')
        ELSEIF(IW.EQ.2) THEN
          call tstamp('>','MFS: enter raw reporting')
          CALL MFRTBL(itrc,IFRES,ier)
          call tstamp('>','MFS: exit raw reporting')
        END IF

C Graphical output of results. Disabled on nongraphic terminals.
C First read problem description file.
C If not OK, or if not network data available, or if there is a
C missmatch in the problem file and resuults file go back to menu.
      ELSEIF(INO.EQ.4)THEN
        IF(MMOD.LT.0.AND.MMOD.NE.-6)THEN
          CALL USRMSG(' ','You must be in a graphic mode!','W')
          GOTO 10
        ENDIF
        CALL MFLOAD(IFPRB,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG('Problem encountered while loading the',
     &                'mass flow network description.','W')
          GOTO 10
        ENDIF
        IF(NNOD.LT.2.OR.NCMP.LT.1.OR.NCNN.LT.1)THEN
          CALL USRMSG(' Insufficient nodes and connections',
     &    ' to create a network.','W')
          GOTO 10
        ENDIF
        CALL MFCRIM(IFRES,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG('Mismatch between the network',
     &                'description and results file.','W')
          GOTO 10
        ENDIF
        call tstamp('>','MFS: enter graphing')
        CALL MFPLOT(itrc,iuout,IFRES,ier)
        call tstamp('>','MFS: exit graphing')

C Alter iteration parameters.
      ELSEIF(INO.EQ.6)THEN
        CALL MFMODE(1)
        call tstamp('>','MFS: change iteration parameters')

C Alter stack model.
      ELSEIF(INO.EQ.7)THEN
        CALL MFMODE(2)
        call tstamp('>','MFS: change pstack parameters')

C Alter solver type.
      ELSEIF(INO.EQ.8)THEN
        CALL MFMODE(3)
        call tstamp('>','MFS: change solver type')

C Alter trace flag.
      ELSEIF(INO.EQ.9)THEN
        CALL MFMODE(4)
        call tstamp('>','MFS: change trace level')

C Enable/disable non-fatal warning message printing.
      ELSEIF(INO.EQ.10)THEN
        helptopic='mfs_review_warnings'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(OUTSTR,'Print warning messages?',OK,nbhelp)
        IF(OK)THEN
          ICOUT=IUOUT
        ELSE
          IF(MMOD.NE.-6) THEN
           ICOUT=99
           CALL EDISP(IUOUT,' ')
           CALL EDISP(IUOUT,'Non-fatal run-time warnings will be  ')
           CALL EDISP(IUOUT,'directed to file fort.99.')
          END IF
        ENDIF

C Output destination.
      ELSEIF(INO.EQ.13)THEN
        call ctlexpmfs(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
      
C Toggle time format and day separators.
      ELSEIF(INO.EQ.14)THEN
        IHFLAG=IHFLAG+1
        if(IHFLAG.GT.1)IHFLAG=0
C        if(IHFLAG.eq.0)then
C          call edisp(iuout,' ')
C          call edisp(iuout,'standard display time = 10h30 set ')
C        elseif(IHFLAG.eq.1)then
C          call edisp(iuout,' ')
C          call edisp(iuout,'3rd party graphing time = day fraction set')
C        endif
        helptopic='mfs_include_day_marker'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB('Include mark between days when',
     &    'displaying or writing data?','no','yes',IDH,nbhelp)
        IDHFLG=IDH-1

C Toggle delimeter.
      ELSEIF(INO.EQ.15)THEN
        helptopic='mfs_tabular_delimeters'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IWM=1
        CALL EASKATOG(' ','Columns delimeter options:',
     &    'multiple spaces','single space','tab','comma','cancel',
     &    ' ',' ',IWM,nbhelp)
        if(iwm.eq.1)then
          delim = '-'
        elseif(iwm.eq.2)then
          delim = 'S'
        elseif(iwm.eq.3)then
          delim = 'T'
        elseif(iwm.eq.4)then
          delim = 'C'
        endif
 
      ELSE
        INO=-1
        GOTO 12
      ENDIF
      GOTO 10

C Set up and do actual simulation; first read problem description file
C If not OK, or if not network data available go back to menu.
   30 CALL MFLOAD(IFPRB,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG('Problem encountered while loading the',
     &              'mass flow network description.','W')
        GOTO 10
      ENDIF
      IF(NNOD.LT.2.OR.NCMP.LT.1.OR.NCNN.LT.1)THEN
        CALL USRMSG('Insufficient nodes and connections',
     &  'to create a network.','W')
        GOTO 10
      ENDIF

c Call control subroutine.
      IER=0
      CALL MZCSTR(ier)
      IF(IER.EQ.1)GOTO 10

C Input start and stop dates; set simulation time step control data
C Read wind pressure coefficients.
C Set known constant pressure nodes pressures and reset other nodes.
C Set initial values for flow, derivative and pressure history variables.
C Rewind results file (will only hold results of most recent mfs run).
C Indicate start of simulation and show iteration parameters.
      CALL MFSETU(IFRES)

C Start simulation and continue until LAST hour.
      LAST=.FALSE.

C Update time counter
C Check if past LAST hour and if not:
C   Read/set weather data
C   Set boundary nodes temperature and surface pressure
C   Calculate fluid density for each node
C   Calculate stack effect difference for each connection
C   Show simulation progress
   40 CALL MFRSET(IFCLM,LAST)
      IF(LAST) GOTO 10

C Solve the fluid mass flow network
      call tstamp('>','MFS: enter network solver')
      CALL MFSOLV

C Transfer results to results file
      CALL MFOUTP(IFRES)
      call tstamp('>','MFS: exit network solver')
      GOTO 40

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      END

C **************** MFSETU
C Input start and finish dates and set simulation time step control data.
C Read wind pressure coefficients.
C Set known constant pressure nodes pressures and reset other nodes.
C Set initial values for flow, derivative and pressure history variables.
C Rewind results file (will only hold results of most recent simulation).
C Indicate start of simulation and show iteration parameters

      SUBROUTINE MFSETU(IFRES)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MFTRA/IMFTU
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW

      character LAPROB*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character outs*124,outs248*248

      PARAMETER (SMALL=1.0E-15)

C Assume most legible day and time formats as well as
C a default start and finish day.
      IFDAY=2
      IFTIME=0
      IFYD=1
      ILYD=31

C Input start and stop dates.
   10 CALL EASKPER('Simulation period:',IFYD,ILYD,IFDAY,IER)
      IF(IER.NE.0) GOTO 10
      CALL EDAYR(IFYD,IFD,IFM)
      CALL EDAYR(ILYD,ILD,ILM)

C Set simulation time step control variables and progress flag.
      IRM=IFM
      IRD=IFD
      IRH=0
      IHOUR=0
      IYD=IFYD
      IPROG=0

C Read pressure distribution file (coefficient for normal wind first).
      REWIND(IAPRES,ERR=999)
      READ(IAPRES,*,ERR=999) NPRE
      IF(NPRE.LE.0.OR.NPRE.GT.MPRD)then
        call usrmsg(' Number of pressure distributions out of',
     &              ' range. Could be bad pressure db.','W')
        GOTO 100
      endif
      DO 22 J=1,NPRE
      READ(IAPRES,*,ERR=999) (FPRE(I,J),I=1,MPOS)
   22 CONTINUE

C Set known constant pressure nodes pressures and reset other nodes.
      DO 32 INOD=1,NNOD
      IF(NDTYP(INOD).EQ.1.OR.NDTYP(INOD).EQ.2) THEN
         PRES(INOD)=SUPNOD(INOD,1)
      ELSE
         PRES(INOD)=0.0
      END IF
   32 CONTINUE

C Set initial values for flow, derivative and pressure history variables.
      DO 34 ICNN=1,NCNN
      HDP(ICNN)=DBLE(SMALL)
      HDV(ICNN)=1.0
      HFL(ICNN)=DBLE(SMALL)
   34 CONTINUE

C Rewind results file (by this action the results file will only
C hold the results of the most recent simulation).
      REWIND(IFRES,ERR=998)

C Indicate start of simulation and show main calculation parameters
      call edisp(IMFTU,'Network flow simulation commenced with:')
      WRITE(outs,'(A,A)') 'Problem: ',LAPROB(1:LNBLNK(LAPROB))
      call edisp(IMFTU,outs)
      WRITE(outs248,'(A,A)') 'Cp-wind: ',LAPRES(1:LNBLNK(LAPRES))
      call edisp248(IMFTU,outs248,100)
      WRITE(outs248,'(A,A)') 'Weather: ',LCLIM(1:LNBLNK(LCLIM))
      call edisp248(IMFTU,outs248,100)
      WRITE(outs,'(A,A)') 'Results: ',LAFRES(1:LNBLNK(LAFRES))
      call edisp(IMFTU,outs)
      call edisp(IMFTU,'Iteration control parameters:')
      WRITE(outs,'(A,I5)') '   MAXITF = ',MAXITF
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F9.3)') '   FERREL (%) = ',FERREL*100.0
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F11.5)') '   FERMFL (kg/s)= ',FERMFL
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F8.2)') '   PMAX (Pa) = ',PMAX
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F8.2)') '   STEFFR = ',STEFFR
      call edisp(IMFTU,outs)
      call edisp(IMFTU,'Calculation control parameters:')
      WRITE(outs,'(A,4X,I1)') '   IPSMOD = ',IPSMOD
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,4X,I1)') '   MSLVTP = ',MSLVTP
      call edisp(IMFTU,outs)

      RETURN

C SUnrecoverable error.
  100 STOP 'MFSETU: unresolvable error!'

C Database read error.
  999 CALL edisp(iuout,'MFSETU: pressure db read error!')
      goto 100

C Rewind error.
  998 CALL edisp(iuout,'MFSETU: unable to rewind results file!')
      goto 100

      END

C ********************* SETUP 
C Displays the preferences menu.

      SUBROUTINE SETUP(itrc,ITRU,IER)
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      DIMENSION SETUPM(5)
      CHARACTER SETUPM*36
      integer NITMS,ISETUP ! max items and current menu item

      helpinsub='mfs'  ! set for setup

C Present the setup menu, ISETUP is the menu index returned. Generate
C the menu text from current common block into via UPSET.
    2 IER=0
      ISETUP=-2

      IF(IFDAY.EQ.0)THEN
        SETUPM(1)='a date display >> DOY  10           '
      ELSEIF(IFDAY.EQ.1)THEN
        SETUPM(1)='a date display >> 10 Jan            '
      ELSEIF(IFDAY.EQ.2)THEN
        SETUPM(1)='a date display >> Fri 10 Jan        '
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        SETUPM(2)='b time display >> 10h30             '
      ELSEIF(IFTIME.EQ.1)THEN
        SETUPM(2)='b time display >> 10.50             '
      ELSEIF(IFTIME.EQ.2)THEN
        SETUPM(2)='b time display >> 0.4166 (day frac) '
      ENDIF
      SETUPM(3)=  '  ____________________________      '
      SETUPM(4)=  '? help                              '
      SETUPM(5)=  '- exit menu                         '

C Help text for this menu.
      helptopic='mfs_setup_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      NITMS=5
      CALL EMENU('Peferences',SETUPM,NITMS,ISETUP)
      IF(ISETUP.EQ.1)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0
      ELSEIF(ISETUP.EQ.2)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0
      ELSEIF(ISETUP.EQ.4)THEN

C Present help text for the menu.
        helptopic='mfs_setup_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('preference section',nbhelp,'-',0,0,IER)
      ELSEIF(ISETUP.EQ.5)THEN

C Return to main menu.
        RETURN
      ELSE

C Not one of the legal menu choices.
        ISETUP=-1
        GOTO 2
      ENDIF
      ISETUP=-4
      GOTO 2

      END

C Dummy routine for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end

C Dummy routines from common3dv.F needed to match calls in esru_lib.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine redraw(IER)
      ier=0
      return
      end

      subroutine chgazi(icazi,ifrlk)
      return
      end

      subroutine chgelev(icelev,ifrlk)
      return
      end

      subroutine chgpan(ix,iy)
      return
      end

      subroutine chgzoom(imode)
      return
      end

      subroutine optview
      return
      end

      subroutine chgsun(isunhour)
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed paramters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type set integer size of passed parameters.
#ifdef OSI
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD
      integer JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR
      integer*8 JITGRD,JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#endif     
      return
      end

      subroutine chgzonpik(jizgfoc,jnzg)
#ifdef OSI
      integer jizgfoc,jnzg  ! for use with viewtext
#else
      integer*8 jizgfoc,jnzg  ! for use with viewtext
#endif
      return
      end

      subroutine chgzonpikarray(jnznog,jnznogv)
#ifdef OSI
      integer jnznog,jnznogv
#else
      integer*8 jnznog,jnznogv
#endif
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end
      
C dintervalf: A fortran implementation of the c surboutine dinterval.
C Copied from esrucom/esru_misc.F
      subroutine dintervalf(v1,v2,dv,ndec,mode)
      real v1,v2,dv
      integer ndec,mode
C When 'mode'=1 the hour interval on the graphical time (x-axis) is
C set as follow:
C v=v2-v1 for v < 12 dv=1, v < 18 dv=2, v < 24 dv=3
C             v < 48 dv=6, v < 96 dv=12 else dv=24.
C Should be the same logic as in esru_x.c.
      real v,dvv,x,w
      integer ix
      if(mode.eq.0)then
        vv = v2 - v1
        v = abs(vv)
        x = log10(v)
        ix = nint(x)
        if (x.lt.0.0) ix=ix-2
        dx = real(ix)

        dz = 10.0**dx
        vr =  v / dz
        w = 10.0
        if (vr.lt.5.0) w = 5.0
        if (vr.lt.2.0) w = 2.0

        dvv = w * 0.1 * dz
        if (vv.lt.0.0) dvv = -dvv

        nd = 1 - ix
        if (w.eq.10.0)then
          nd=nd-1
        elseif (w.eq.5.0)then
          nd = 1
        elseif (w.eq.2.0)then
          nd = 2
        endif
        if (nd.lt.0) nd = 0
      else
        v = v2 - v1
        dvv = 168.0
        if (v.lt.4320.0) dvv = 48.0
        if (v.lt.1440.0) dvv = 24.0
        if (v.lt.338.0) dvv = 12.0
        if (v.lt.122.0) dvv = 8.0
        if (v.lt.50.0) dvv = 4.0
        if (v.lt.26.0) dvv = 3.0
        if (v.lt.20.0) dvv = 2.0
        if (v.lt.14.0) dvv = 1.0
        nd = 0
      endif
      dv = dvv
      ndec = nd
      return
      end

C And align_comment from esru_misc.F.

C ******************** align_comment ********************
C Add comment after a specific column and return formatted line.

      subroutine align_comment(icolumn,string,comment,aligned_str)

      integer lnblnk
      integer icolumn              ! Column to start comment.
      character*(*) string         ! Data or token string.
      character*(*) comment        ! Comment string.
      character*(*) aligned_str    ! Composed string.
      character*48  hash_str       ! Filler string to be created.
      integer lnstring,lncomment   ! Length of strings.
      integer lnprehash            ! Blanks before hash

C If lnstring is less than icolumn create hash_str to fill in the gap.
C Otherwise if lnstring is greater than icolumn the hash_str is '  # '
      lnstring=lnblnk(string)
      lncomment=lnblnk(comment)
      if(lnstring.lt.(icolumn-3))then
        lnprehash=(icolumn-lnstring)+1
        hash_str='                                               '
        write(hash_str(lnprehash:lnprehash),'(a)') '#' 
      else
        lnprehash=3
        write(hash_str,'(a)') '  # '
      endif
      write(aligned_str,'(3a)') string(1:lnstring),
     &  hash_str(1:lnprehash+1),comment(1:lncomment)
      return
      end

C The following from econstr.F
      SUBROUTINE ECONST(LCONS,IUNIT,ICOMP,ITRC,ITRU,IER)
      character LCONS*72
      integer IUNIT,ICOMP,ITRC,ITRU,IER
  
      return
      end

      SUBROUTINE ERTWIN(ITRC,ITRU,IUA,LUA,ICOMP,IER)
      character LUA*72
      integer ITRC,ITRU,IUA,ICOMP,IER
  
      return
      end
     
      

