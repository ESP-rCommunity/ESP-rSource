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

C ************* mfs

C Mfs simulates fluid flows in building & plant configurations.

C For details on mfs's theoretical basis see publications listed
C at the following Web site:

C http://www.esru.strath.ac.uk

C Mfs solves a network representing one-dimensional fluid
C flow in building and/or plant configurations.
C This involves the calculation of fluid flow through branches and nodes
C in a network which represents a building and/or plant configuration.
C The nodes and branches describe boundary pressure points, internal
C node characteristics and flow restrictions.
C Mfs uses an iterative mass balance approach in which nodal
C pressures (of unknown-pressure nodes) are adjusted until the mass
C residual of each internal node satifies some criterion.

C This is the main program which initializes the user terminal,
C initializes IEEE error handling
C controls common initialization
C displays the main menu and controls the main program flow.

C All global COMMON variables are described in emfnetw.F.

C Local variables in this module are:
C     IUOUT  - write unit (standard output).
C     IUIN   - read  unit (standard input).
C     IMFTU  - write unit for fluid flow calculation trace information.
C     ICOUT  - write unit for (non-fatal) warning messages.
C     MMOD   - determines the device type attached to the current job.

      program mfs
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
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
      common/uhome/upath
      common/udot/esprc

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl

C Defaults.
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Activation of connections
      COMMON/ACT/ACTCNN(MCNN)

C Once per hour storage of results, averaged over that hour.
      common/c6/indcfg
      common/rdcgen/irdact
      common/rdcmfs/irdcm,sFLWTIM,sDRYB,sWDIR,sWSPD,sITER,sIOK,
     &              sTNOD(MNOD)

      COMMON/MFLOW1/NNOD,NCMP,NCNN

      LOGICAL LAST,OK,DOK,unixok,there,ACTCNN
      CHARACTER*24 ITEM(18)
      CHARACTER H*72,OUTSTR*124,inf*144,outs*124
      character*72 DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character path*72,upath*72,esprc*72
      character dfdblbl*20,defdbfl*72
      character tg*1,delim*1,xfile*72
      character journcmd*20,jfile*72,uname*24,fs*1,cjfile*72

C Initialize global common data.
      call ezero
      call curmodule('mfs ')
      IUOUT=6
      IUIN=5
      IFS=1
      ITFS=1
      IMFS=1
      LIMTTY=8
      LIMIT =8
      IDAYF=0
      IDAYL=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Export (grtool) file information. Temporarily use 
C unit ifil+6 for output file.
      xfile = 'data.grt'
      ixopen = 0
      ixunit = 16
      ixpunit = 17
      iuj = 20
      delim='-'
      call parpsf(MODL,iappw,iappx,iappy,inf)

C File unit numbers.
      IFPRB=10
      IFPRE=11
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
        iappw=560
        iappx=85
        iappy=80
      else
        if(iappx.le.0)iappx=85
        if(iappy.le.0)iappy=80
        if(iappw.le.200)then
          iappwi=int(560*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

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
     &'ESP-r Network Flow Simulator: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,8,8,7,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
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

C Set additional output units to stdout. Then redirect warning 
C messages to stderr in case of rule script program control.
      IMFTU=IUOUT
      ICOUT=IUOUT
      IF(MMOD.EQ.-6) ICOUT=0

C Show version
      CALL EDISP(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Flow Solver: Version 6.21a of January 2006.',
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow, Scotland.'
      call edisp(IUOUT,outs)
      CALL EDISP(IUOUT,' ')

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

C Scan the defaults file for default configuration.
C Make temporary use of file unit IUA.
 902  call escdef(dfdblbl,defdbfl,IUA,IER)

C Fill valid component type arrays
C Set defaults for iteration parameters, Pstack calculation model,
C                  matrix solver type and trace output flag
C Assign and attach problem description (IFPRB), wind pressure
C distribution (IFPRE), climate (IFCLM) and results (IFRES) files
      CALL MFINIT(IFPRB,IFPRE,IFCLM,IFRES)

C Let the user pick a menu item.
   10 INO=-5
      ITEM(1) ='a Change problem       '
      ITEM(2) ='b Initiate simulation  '
      ITEM(3) ='c Tabular output       '
      ITEM(4) ='d Graphical output     '
      ITEM(5) ='  ____________________ '
      ITEM(6) ='i Iteration parameters '
      ITEM(7) ='s Stack assumptions    '
      ITEM(8) ='m Solver type          '
      ITEM(9) ='t Trace facilities     '
      ITEM(10)='w Warning messages     '
      ITEM(11)='  ____________________ '
      ITEM(12)='  Formating...         '
      if(ixopen.eq.1)then
        ITEM(13)='> Output >> file  '
      elseif(ixopen.eq.0)then
        ITEM(13)='> Output >> screen'
      endif
      if(IHFLAG.eq.0)then
        ITEM(14)='* Time >> 10h30   '
      elseif(IHFLAG.eq.1)then
        ITEM(14)='* Time >> 0.4375  '
      endif
      if(delim.eq.'-')then
        ITEM(15)='^ Delim >> normal '
      elseif(delim.eq.'T')then
        ITEM(15)='^ Delim >> TAB    '
      elseif(delim.eq.'C')then
        ITEM(15)='^ Delim >> comma  '
      elseif(delim.eq.'S')then
        ITEM(15)='^ Delim >> space  '
      endif
      ITEM(16)='* Preferences          '
      ITEM(17)='? Help                 '
      ITEM(18)='- Finish mfs           '
      NITMS=18

C Use askabout to instanciate the initial help messages (2nd parameter is one).
   12 call askabout('mfs ',1)

      CALL EMENU('network flow solver',ITEM,NITMS,INO)

      IF(INO.EQ.NITMS)THEN

C Finish mfs.
C Close fluid flow problem description file
        CALL ERPFREE(IFPRB,ISTAT)

        call usrmsg(' Closing mass flow analysis...',' ','P')
        call tstamp('>','Finish network flow analysis')
        CALL EPAGEND
        STOP
      elseif(INO.EQ.NITMS-1)then

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('mfs ',0)
      elseif(INO.EQ.NITMS-2)then
        CALL SETUP(ITRC,iuout,IER)
      elseif(INO.EQ.1)then

C Change to a different problem description.
        CALL MFINIT(IFPRB,IFPRE,IFCLM,IFRES)
        call tstamp('>','MFS: change network file')
      elseif(INO.EQ.2)then

C Invoke simulation.
        GOTO 30
      ELSEIF(INO.EQ.3)THEN

C Tabular output of results; first read problem description file.
        CALL MFLOAD(IFPRB,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' Problem encountered while loading the',
     &                ' mass flow network description. ','W')
          GOTO 10
        ENDIF
        IF(NNOD.LT.2.OR.NCMP.LT.1.OR.NCNN.LT.1)THEN
          CALL USRMSG(' There do not appear to be sufficient nodes and',
     &    ' connections available to create a network. ','W')
          GOTO 10
        ENDIF
        CALL MFCRIM(IFRES,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' There is a mismatch between the network',
     &                ' description and the results file.','W')
          GOTO 10
        ENDIF

        H(1)='Pretty print is restricted to prescribed forms while'
        H(2)='raw mode is more versatile and is often used for'
        H(3)='export to third party graphing utilities.'
        CALL EASKAB(' ','Do you want tabular output to be:',
     &              'pretty print','raw & versatile',IW,3)
        IF(IW.EQ.1) THEN
          call tstamp('>','MFS: enter pretty print reporting')
          CALL MFTABL(itrc,IFRES,ier)
          call tstamp('>','MFS: exit pretty print reporting')
        ELSE IF(IW.EQ.2) THEN
          call tstamp('>','MFS: enter raw reporting')
          CALL MFRTBL(itrc,IFRES,ier)
          call tstamp('>','MFS: exit raw reporting')
        END IF
      ELSEIF(INO.EQ.4)THEN

C Graphical output of results. Disabled on nongraphic terminals.
C First read problem description file.
C If not OK, or if not network data available, or if there is a
C missmatch in the problem file and resuults file go back to menu.
        IF(MMOD.LT.0.AND.MMOD.NE.-6)THEN
          CALL USRMSG(' ',' You must be in a graphic mode!','W')
          GOTO 10
        ENDIF
        CALL MFLOAD(IFPRB,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' Problem encountered while loading the',
     &                ' mass flow network description. ','W')
          GOTO 10
        ENDIF
        IF(NNOD.LT.2.OR.NCMP.LT.1.OR.NCNN.LT.1)THEN
          CALL USRMSG(' There do not appear to be sufficient nodes and',
     &    ' connections available to create a network. ','W')
          GOTO 10
        ENDIF
        CALL MFCRIM(IFRES,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' There is a mismatch between the network',
     &                ' description and the results file.','W')
          GOTO 10
        ENDIF
        call tstamp('>','MFS: enter graphing')
        CALL MFPLOT(itrc,iuout,IFRES,ier)
        call tstamp('>','MFS: exit graphing')
      ELSEIF(INO.EQ.6)THEN

C Alter iteration parameters.
        CALL MFMODE(1)
        call tstamp('>','MFS: change iteration parameters')
      ELSEIF(INO.EQ.7)THEN

C Alter Pstack model.
        CALL MFMODE(2)
        call tstamp('>','MFS: change pstack parameters')
      ELSEIF(INO.EQ.8)THEN

C Alter solver type.
        CALL MFMODE(3)
        call tstamp('>','MFS: change solver type')
      ELSEIF(INO.EQ.9)THEN

C Alter trace flag
        CALL MFMODE(4)
        call tstamp('>','MFS: change trace level')
      ELSEIF(INO.EQ.10)THEN

C Enable/disable non-fatal warning message printing.
        dok=.false.
        h(1)='Some models have parameters which, although not errors'
        h(2)='might be of concern. If you would like to view warnings'
        h(3)='answer yes. '
        CALL ASKOK(OUTSTR,' Print warning messages?',OK,dok,3)
        IF(OK)THEN
          ICOUT=IUOUT
        ELSE
          IF(MMOD.NE.-6) THEN
           ICOUT=99
           CALL EDISP(IUOUT,' ')
           CALL EDISP(IUOUT,' Non-fatal run-time warnings will be  ')
           CALL EDISP(IUOUT,' directed to a scratch file (fort.99).')
          END IF
        ENDIF
      ELSEIF(INO.EQ.13)THEN
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
      ELSEIF(INO.EQ.14)THEN

C Toggle time format and day separators.
        IHFLAG=IHFLAG+1
        if(IHFLAG.GT.1)IHFLAG=0
        if(IHFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'standard display time = 10h30 set ')
        elseif(IHFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'3rd party graphing time = day fraction set')
        endif
        H(1)='Places an # separator between days of tabular data.'
        CALL EASKAB('Include mark between days when ',
     &              'displaying or writing data :','no','yes',IDH,1)
        IDHFLG=IDH-1
      ELSEIF(INO.EQ.15)THEN

C Toggle delimeter.
        H(1) ='Tabular data can be sent to file with various '
        H(2) ='delimiters: '
        H(3) ='  spaces (format using spaces to lineup columns)'
        H(4) ='  single space between columns'
        H(5) ='  comma separator (for excel)'
        H(6) ='  tab separator (for excel)'
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','continue',' ',
     &    ' ',IWM,6)
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
        CALL USRMSG(' Problem encountered while loading the',
     &              ' mass flow network description. ','W')
        GOTO 10
      ENDIF
      IF(NNOD.LT.2.OR.NCMP.LT.1.OR.NCNN.LT.1)THEN
        CALL USRMSG(' There do not appear to be sufficient nodes and',
     &  ' connections available to create a network. ','W')
        GOTO 10
      ENDIF

c Call control subroutine MZCSTR
34    IER=0
      CALL MZCSTR(ier)
      IF(IER.EQ.1)GOTO 10

C Input start and stop dates; set simulation time step control data
C Read wind pressure coefficients.
C Set known constant pressure nodes pressures and reset other nodes.
C Set initial values for flow, derivative and pressure history variables.
C Rewind results file (will only hold results of most recent mfs run).
C Indicate start of simulation and show iteration parameters.
      CALL MFSETU(IFPRE,IFRES)

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
C Input start and stop dates; set simulation time step control data
C Read wind pressure coefficients
C Set known constant pressure nodes pressures and reset other nodes
C Set initial values for flow, derivative and pressure history variables
C Rewind results file (will only hold results of most recent ESRUmfs run)
C Indicate start of simulation and show iteration parameters

      SUBROUTINE MFSETU(IFPRE,IFRES)

#include "building.h"
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFTRA/IMFTU
      COMMON/C22/ICLIM,LCLIM
      common/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW

      CHARACTER*72 LCLIM
      character*72 LAPROB,LAPRES,LAFRES
      character outs*124

      PARAMETER (SMALL=1.0E-15)

C Assume most legible day and time formats as well as
C a default start and finish day.
      IFDAY=2
      IFTIME=0
      IFYD=1
      ILYD=31

C Input start and stop dates
   10 CALL EASKPER('Simulation period:',IFYD,ILYD,IFDAY,IER)
      IF(IER.NE.0) GOTO 10
      CALL EDAYR(IFYD,IFD,IFM)
      CALL EDAYR(ILYD,ILD,ILM)

C Set simulation time step control variables and progress flag
      IRM=IFM
      IRD=IFD
      IRH=0
      IHOUR=0
      IYD=IFYD
      IPROG=0

C Read pressure distribution file (coefficient for normal wind first)
      REWIND(IFPRE,ERR=999)
      READ(IFPRE,*,ERR=999) NPRE
      IF(NPRE.LE.0.OR.NPRE.GT.MPRD)then
        call usrmsg(' Number of pressure distributions out of',
     &              ' range. Could be bad pressure db.','W')
        GOTO 100
      endif
      DO 22 J=1,NPRE
      READ(IFPRE,*,ERR=999) (FPRE(I,J),I=1,MPOS)
   22 CONTINUE

C Set known constant pressure nodes pressures and reset other nodes
      DO 32 INOD=1,NNOD
      IF(NDTYP(INOD).EQ.1.OR.NDTYP(INOD).EQ.2) THEN
         PRES(INOD)=SUPNOD(INOD,1)
      ELSE
         PRES(INOD)=0.0
      END IF
   32 CONTINUE

C Set initial values for flow, derivative and pressure history variables
      DO 34 ICNN=1,NCNN
      HDP(ICNN)=DBLE(SMALL)
      HDV(ICNN)=1.0
      HFL(ICNN)=DBLE(SMALL)
   34 CONTINUE

C Rewind results file (by this action the results file will only
C hold the results of the most recent ESRUmfs run)
      REWIND(IFRES,ERR=998)

C Indicate start of simulation and show main calculation parameters
      call edisp(IMFTU,' ESRUmfs simulation commenced with:')
      WRITE(outs,'(A,A)') ' Problem : ',LAPROB(1:LNBLNK(LAPROB))
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,A)') ' Cp-wind : ',LAPRES(1:LNBLNK(LAPRES))
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,A)') ' Climate : ',LCLIM(1:LNBLNK(LCLIM))
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,A)') ' Results : ',LAFRES(1:LNBLNK(LAFRES))
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,I5)')
     &' Iteration control parameters:     MAXITF       = ',MAXITF
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F9.3)')
     &'                                   FERREL (%)   = ',FERREL*100.0
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F11.5)')
     &'                                   FERMFL (kg/s)= ',FERMFL
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F8.2)')
     &'                                   PMAX   (Pa)  = ',PMAX
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,F8.2)')
     &'                                   STEFFR       = ',STEFFR
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,4X,I1)')
     &' Calculation control parameters:   IPSMOD       = ',IPSMOD
      call edisp(IMFTU,outs)
      WRITE(outs,'(A,4X,I1)')
     &'                                   MSLVTP       = ',MSLVTP
      call edisp(IMFTU,outs)

      RETURN

C Stop due to 'unrecoverable' error
  100 STOP ' MFSETU: unresolvable error !'

C Error trap on read error or silly data
  999 CALL edisp(iuout,'  MFSETU: pressure db read error !')
      goto 100

  998 CALL edisp(iuout,'  MFSETU: unable to rewind results file!')
      goto 100

      END

C ********************* SETUP 
C SETUP provides the menu's and control logic for the mfs SETUP menu.
      SUBROUTINE SETUP(itrc,ITRU,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      DIMENSION SETUPM(5)
      CHARACTER SETUPM*36,H*72

C Present the setup menu, ISETUP is the menu index returned. Generate
C the menu text from current common block into via UPSET.
    2 IER=0
      ISETUP=-2

      IF(IFDAY.EQ.0)THEN
        SETUPM(1)='a Date display >> DOY  10           '
      ELSEIF(IFDAY.EQ.1)THEN
        SETUPM(1)='a Date display >> 10 Jan            '
      ELSEIF(IFDAY.EQ.2)THEN
        SETUPM(1)='a Date display >> Fri 10 Jan        '
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        SETUPM(2)='b Time display >> 10h30             '
      ELSEIF(IFTIME.EQ.1)THEN
        SETUPM(2)='b Time display >> 10.50             '
      ELSEIF(IFTIME.EQ.2)THEN
        SETUPM(2)='b Time display >> 0.4166 (day frac) '
      ENDIF
      SETUPM(3)=  '  ____________________________      '
      SETUPM(4)=  '? Help                              '
      SETUPM(5)=  '- Exit to main menu                 '

C Help text for this menu.
      H(1)='The preferences menu allows the fonts and display'
      H(2)='sizes within mfs to be modified by the user. '
      H(3)=' '
      H(4)='For ease of viewing several font sizes are available.'
      H(5)='Remember that the size of the window may be resized '
      H(6)='so that complex images may be viewed in greater '
      H(7)='detail.'

      CALL EMENU('  Mfs preferences',SETUPM,5,ISETUP)
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
        CALL PHELPD('preference section',7,'-',0,0,IER)
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

C CTLEXP *********** (copy of code from esrures/utils.F)
C CTLEXP: Control feedback to export file or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T, X, or G specifying text, tabular or graphic 
C info being saved.
      subroutine ctlexp(xfile,ixopen,ixunit,ixpunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rcmd/LCMDFL
      common/rpath/path

      character*(*) msg,xfile
      character H*72,path*72,outs*124,tg*1,tfile*72
      character ixpfile*72,ttext*42,LCMDFL*144,longtfile*144
      logical concat

C Initial name for export parameter file is ixfile+par.
      if(xfile(1:1).ne.' ')then
        write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif

C Each call is a toggle.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs,'(a,a)') 'closing export file: ',xfile
        call usrmsg(outs,' ','-')
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
          write(outs,'(a,a)') 'closing export parameter file: ',ixpfile
          call usrmsg(outs,'reporting >> to display ','-')
        elseif(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
        elseif(tg.eq.'G')then
          call wwcsetend
          call wwcclose(xfile)
        endif
      elseif(ixopen.eq.1)then
        iw=0
        if(path.ne.'./'.and.path.ne.' ')then
           write(outs,'(A,A)') ' The current path is: ',path
           call edisp(iuout,outs)
           h(1)='If using path then name appended to path. Otherwise'
           h(2)='file put in folder where application was started.'
           h(3)='If `local` choice you may also give an absolute'
           h(4)='addressed file name ie. /tmp/junk.exp '
           CALL EASKAB(' The problem is in a remote folder... ',
     &      ' place export file:','using current path',
     &      'in local (or absolute) folder',IW,4)
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        call easks(xfile,' export file name: ',' ',72,
     &    ' ','export file name',IER,2)

        write(outs,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                       ' for export.'
        call usrmsg(outs,' ','-')
        if(iw.eq.1)then
          if(tg.eq.'X')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
            write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
            call efopseq(ixpunit,ixpfile,4,IER)
            write(outs,'(3A)')' opened ',ixpfile(1:lnblnk(ixpfile)),
     &                       ' for export parameters.'
            call usrmsg(outs,' ','-')
          elseif(tg.eq.'T')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to wwlib.c
            tfile=xfile
            longtfile=' '
            call addpath(tfile,longtfile,concat)
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        else
          if(tg.eq.'X')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
            write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
            CALL ERPFREE(ixpunit,ISTAT)
            call FPOPEN(ixpunit,ISTAT,1,3,ixpfile)
            write(outs,'(3A)')' opened ',ixpfile(1:lnblnk(ixpfile)),
     &                       ' for export parameters.'
            call usrmsg(outs,' ','-')
          elseif(tg.eq.'T')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'G')then
            call wwcopen(xfile)
            call wwcsetstart
          endif
        endif
        write(outs,'(1x,3A)') msg(:lnblnk(msg)),
     &    ' reporting >> to ',xfile(1:lnblnk(xfile))
        call usrmsg(outs,' ','-')

C If tabular info then create the headder to the xvgr parameter file.
        if(tg.eq.'X')then
          write(ixpunit,'(a)') '# ACE/gr parameter file'
          write(ixpunit,'(a)') '# '
          write(ixpunit,'(a)') 'page 5'
          write(ixpunit,'(a)') 'page inout 5'
          write(ixpunit,'(a)') 'link page off'
          write(ixpunit,'(a)') 'with g0'
          write(ixpunit,'(a)') 'g0 on'
          write(ixpunit,'(a)') 'g0 type xy'
          write(ixpunit,'(a)') 'g0 autoscale type AUTO'
          write(ixpunit,'(a)') '  default linestyle 1'
          write(ixpunit,'(a)') '  default linewidth 1'
          write(ixpunit,'(a)') '  default color 1'
          write(ixpunit,'(a)') '  default char size 1.0000'
          write(ixpunit,'(a)') '  default font 2'
          write(ixpunit,'(a)') '  default font source 0'
          write(ixpunit,'(a)') '  default symbol size 0.5000'
          write(ixpunit,'(a)') '  view xmin 0.15'
          write(ixpunit,'(a)') '  view xmax 0.85'
          write(ixpunit,'(a)') '  view ymin 0.15'
          write(ixpunit,'(a)') '  view ymax 0.85'

          h(1)='3rd party graphing usually has a main title...'
          ttext = 'Simulation Results'
          call easks(ttext,'Title for 3rd party graph: ',' ',42,
     &      'Simulation Results ','3rd party title',IER,1)
          write(ixpunit,'(3a)') 'title "',
     &      ttext(1:lnblnk(ttext)),'"'
          write(ixpunit,'(a)') 'title font 4'
          write(ixpunit,'(a)') 'title size 1.25'

C Include the name of the results library as a subtitle.
          write(ixpunit,'(3a)') 'subtitle "',
     &       LCMDFL(1:lnblnk(LCMDFL)),'"'
          write(ixpunit,'(a)') 'subtitle font 4'
          write(ixpunit,'(a)') 'subtitle size 1.00'
          
C Typical xaxis stuff.
          write(ixpunit,'(a)') '  xaxis  label "Day of year"'
          write(ixpunit,'(a)') '  xaxis  label place auto'
          write(ixpunit,'(a)') '  xaxis  label char size 1.0000'
          write(ixpunit,'(a)') '  xaxis  label font 4'
          write(ixpunit,'(a)') '  xaxis  label color 1'
          write(ixpunit,'(a)') '  xaxis  label linewidth 1'
          write(ixpunit,'(a)') '  xaxis  ticklabel on'
          write(ixpunit,'(a)') '  xaxis  ticklabel type auto'
          write(ixpunit,'(a)') '  xaxis  ticklabel layout horizontal'
          write(ixpunit,'(a)') '  xaxis  ticklabel  op bottom'
          write(ixpunit,'(a)') '  xaxis  ticklabel char size 0.75000'
          
C Typical yaxis stuff.
          write(ixpunit,'(a)') '  yaxis  tick on'
          write(ixpunit,'(a)') '  yaxis  label "Data"'
          write(ixpunit,'(a)') '  yaxis  label place auto'
          write(ixpunit,'(a)') '  yaxis  label char size 1.0000'
          write(ixpunit,'(a)') '  yaxis  label font 4'
          write(ixpunit,'(a)') '  yaxis  label color 1'
          write(ixpunit,'(a)') '  yaxis  label linewidth 1'
          write(ixpunit,'(a)') '  yaxis  ticklabel on'
          write(ixpunit,'(a)') '  yaxis  ticklabel type auto'
          write(ixpunit,'(a)') '  yaxis  ticklabel layout horizontal'
          write(ixpunit,'(a)') '  yaxis  ticklabel  op left'
          write(ixpunit,'(a)') '  yaxis  ticklabel char size 0.75000'
          write(ixpunit,'(a)') '  legend on'
          write(ixpunit,'(a)') '  legend vgap 2'
          write(ixpunit,'(a)') '  legend hgap 1'
          write(ixpunit,'(a)') '  legend x1 0.2'
          write(ixpunit,'(a)') '  legend y1 0.8'
          write(ixpunit,'(a)') '  legend length 4'
          write(ixpunit,'(a)') '  legend color 1 '
          write(ixpunit,'(a)') '  legend linestyle 1'
          write(ixpunit,'(a)') '  legend linewidth 1'
          write(ixpunit,'(a)') '  legend font 4'
          write(ixpunit,'(a)') '  legend char size 0.75000'
        endif
      endif
      RETURN
      END

C ***** dummy routine for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

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
