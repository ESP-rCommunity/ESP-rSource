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

C This file contains the following routine:

C ******************** MAIN ********************

C The main program initialises the user terminal and
C the user-specified results database and displays the
C main menu.

C Common block variables are:

C IUOUT &   - write and read unit numbers set via the
C IUIN        following DATA statement.
C IFIL      - basic file unit number. Actual file unit numbers are:
C     IFIL +  0 : building results library
C     IFIL +  1 : any temporarily opened file
C     IFIL +  2 : any temporarily opened file (?)
C     IFIL +  3 : primitive construction database
C     IFIL +  4 : mass flow results library
C     IFIL +  5 : climate file
C     IFIL +  6 : output data file
C     IFIL +  7 : TDF file
C     IFIL +  8 : for external help text
C     IFIL +  9 : MLC database
C     IFIL + 10 : plant results library
C     IFIL + 11 : plant component database
C     IFIL + 12 : output data parameters file (for xvgr/ xmgr)
C     IFIL + 13 : recovery parameters file
C     IFIL + 14 : CFD results library
C     IFIL + 21 : power flow results library 

C ICOUT     - output channel number to which data is written.
C           - Principle use is for function button facility on workstations.

C For operational details refer to the ESP-r program user's manual.
      program res
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "material.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C default file names for databases
#include "espriou.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/OUTPCH/ICOUT
      COMMON/DEFLT/IDEFLT
      COMMON/PSY1/PATMOS,IOPT
      common/trc/itrc
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Flags identifying if results libries are available. =0 no lib, =1 lib exists.
      COMMON/LIBOK/IBLIB,IFLIB,ICLIB,IELIB,IPLIB

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Simulation period and save option.
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Simulator parameters and performance information requested data.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      common/SPFLN/spsname
      common/ract/paction
      common/recov01/pifltog,lpifl

C Path to model and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C CFD commons.
      common/flvimg/imgtyp,IMOPTS
      common/flvpar/Lflpar(MCOM)

C Flags for generic data retrieval routines:
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

C Optional title for graph and up to 5 other labels and their positions.
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      COMMON/pres/iunitp

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      
C Name of current application
      common/APPNAME/cAppName

C IEEE callback (Solaris)
#ifdef SUN
#ifdef F90
      external SIGFPE_bps      
      integer SIGFPE_bps      
#endif
#endif
      
      character cAppName*12      
      
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical     unixok,there

      CHARACTER ITEM(19)*23
      CHARACTER*72 LCFGF,actf,Lflpar
      character inf*144,LCMDFL*144,outs248*248,xfile*144
      character outs*124,MODE*4,tg*1,delim*1
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,LFCFG
      character path*72,spsname*8,paction*16
      character cfgroot*24,fs*1,act*16,aut*12
      character journcmd*20,jfile*72,uname*24,cjfile*72,OUTSTR*124
      character bact*4,pifltog*4,lpifl*72
      character graftitle*64,grlbl*24
      character firstbit*72,secondbit*72
      character longtmp*144

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Initialize global common data.
      call ezero
      call curmodule('res ')
      cAppName = 'res'
      helpinapp='res'  ! set once for the application
      helpinsub='res'  ! set for MAIN
      IUOUT=6
      IUIN=5
      ICOUT=0
      IFIL=10
      LIMTTY=24
      LIMIT =24
C     NSIGFIG=3
      NSIGFIG=4
      IFS=1
      ITFS=1
      IMFS=1
      IFDAY=2
      IFTIME=0
      IOPTDB=IFIL+6
      IUTDF=IFIL+7
      IUTDFA=IFIL+8
      matver=0.0   ! initial assumption of binary materials database

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs
      write(upath,'(a1,a1)')'.',fs
      call parres(MODL,iappw,iappx,iappy,inf,act,aut,actf)

C Flags identifying if results libries are available. 
C =0 no lib, =1 lib exists.  Set all = 0 initially
      IBLIB=0
      IFLIB=0
      ICLIB=0
      IELIB=0
      IPLIB=0

C Initial comfort parameters.
      WVEL=0.1
      ACTL=90.0
      CLO=0.7
      iocut = 0
      iocuset = 0

C Initial values for common/IGETFLG/ flags.
      IOCUPF=0
      ialstused=0
      IROC=0

C Assume that the configuration, mlc db, primitive db, control file
C have not been read in.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      OPTKOK=.FALSE.
      pifltog='OFF'
      lpifl='UNKNOWN'
      IDEFLT=1
      PATMOS=1013.25
      IOPT=2
      graftitle=' '
      ngrlbl=0
      grlbl(1)=' '
      grlbl(2)=' '
      grlbl(3)=' '
      grlbl(4)=' '
      grlbl(5)=' '

C Set CFD visualization parameter files to UNKNOWN
      do 8 I=1,MCOM
        Lflpar(I)='UNKNOWN'
 8    continue
      imgtyp=0
      IMOPTS=0

C Initialise coordinates for eye point, view point and angle of view.
C Needed for CFD 3-D views.
      EYEM(1)=-100.
      EYEM(2)=-100.
      EYEM(3)=100.
      VIEWM(1)=10.
      VIEWM(2)=10.
      VIEWM(3)=10.
      ANG=40.

C If your compiler does not support floating-point arithmetic
C according to ANSI/IEEE Std 754-1985 comment out the following code.

C Initialize mode, status, and signal handling for IEEE arithmetic.
C See also: f77_ieee_environment(3F) f77_floatingpoint(3F)
C           Note that in case of Sun FORTRAN, IEEEE exceptions arise
C           only if: Sun-3 f77 with -f68881 or -ffpa option
C                or: Sun-4 f77.
#ifdef SUN
#ifdef F90
      ieeer=ieee_handler('set','common',SIGFPE_ABORT)
      ieeer=ieee_handler('set','common',SIGFPE_bps)
      if(ieeer.ne.0) write(iuout,*) ' IEEE_handler not set !'
#endif
#endif

C Initialise output device.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=600
        iappx=60
        iappy=70
      else
        if(iappx.le.0)iappx=60
        if(iappy.le.0)iappy=70
        if(iappw.le.200)then
          iappwi=int(600*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(26*iappw*0.01),18)
        LIMTTY=9
        LIMIT =9
      else
        menuchw = 26
        LIMTTY=9
        LIMIT =9
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
     &'ESP-r Results Analysis: enquiries to esru@strath.ac.uk')
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call win3d(menuchw,15,15,5,3,igl,igr,igt,igb,igw,igwh)
          igw=igr-igl
          igwh=igb-igt
          call opencpw
          call opensetup
          call updcapt(1)
        elseif(iglib.eq.2)then
          CALL win3d(menuchw,9,20,5,3,igl,igr,igt,igb,igw,igwh)
          igw=igr-igl
          igwh=igb-igt
        endif
        call setzscale()
        call setgscale()
        call setcscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF
      ITRC=1
      IF(MMOD.NE.-6)ICOUT=IUOUT
      CALL ESPrVersion("summary",cAppName,IUOUT)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow, Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,' ')

C Find the users home folder then get users custom settings.
C Make temporary use of file unit ixunit.
      ixunit = ifil + 6
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      IUOUT=6  ! and make sure iuout is not zero
      call scesprc(esprc,ixunit,0,IIER)

C If there is a journal active set up to add to the journal. First
C get the root journal file (setup by prj) from the users home folder.
C All subsequent tstamp messages will be appended to root journal.
      iuj=IFIL+10
      if(journio.eq.1)then
        uname=' '
        call usrname(uname)
        cjfile=' '
        write(cjfile,'(a,a1,a,a,a)')upath(1:lnblnk(upath)),fs,'.',
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

        write(outs,'(a,a)')'Journal (results analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Scan the defaults file for default configuration.
 902  call escdef(ixunit,IER)

C Take command line file name as initial results file name.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs,80)
        call tstamp('>',outs)
      else
        LCMDFL='UNKNOWN'
      endif

C Check for command line actions.
      if(act(1:2).ne.'  '.and.act(1:4).ne.'UNKN')then
         paction=act
         write(outs,'(a,a)')' the action is: ',paction
         call edisp(iuout,outs)
         write(outs,'(a,a)')'RES: the input action is: ',paction
         call tstamp('>',outs)
         if(aut(1:6).eq.'silent')then
           autook=.true.
           call edisp(iuout,'Silent-running mode.')
           call tstamp('>','Silent-running mode')
         else
           autook=.false.
         endif
      endif

C Export (grtool) file information. Temporarily use 
C unit ifil+6 for output file.
      xfile = 'data.grt'
      ixopen = 0
      ixunit = ifil + 6
      ixpunit = ifil + 12
      delim = '-'

C Assume primitive constructions input from IFIL+3.
      IFMAT=IFIL+3
      write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))

C Assume multilayer input from IFIL+9.
      IFMUL=IFIL+9
      write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))

C Initialise results library.
      CALL MORFIL(iftype)
      if(iftype.eq.-2)then
        iftype=0
        goto 4    ! user canceled the file browse jump to menu.
      elseif(iftype.eq.-1) then

C Get the name of the configuration file name 
C Read the plant configuration file.
        if(IPLIB.eq.0.or.IPLIB.eq.1)then
          helptopic='res_dialog_for_cfg_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(longtmp,'(a)') LCFGF(1:lnblnk(LCFGF))

          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            CALL EASKF(longtmp,' ',
     &        ' Corresponding system configuration file name ?',
     &        96,' ','configuration file',IER,nbhelp)
          elseif(iglib.eq.2)then
            CALL EASKF(longtmp,' ',
     &        ' Corresponding system configuration file name ?',
     &        144,' ','configuration file',IER,nbhelp)
          else
            CALL EASKF(longtmp,' ',
     &        ' Corresponding system configuration file name ?',
     &        96,' ','configuration file',IER,nbhelp)
          endif
          if(ier.eq.-3) goto 902  ! cancel detected pass back -3 in ier.

	  call fdroot(longtmp,path,LCFGF)

C Read the model plant-only configuration file.
          IFCFG=IFIL+1
          IAPROB=IFIL+2  
          MODE='ALL '
          call ERSYS(LFCFG,IFCFG,IAPROB,MODE,ITRC,IER)
	endif

C If we have a plant only file go straight to the plant results menu.
        call sysoutp('-',0)

C Free all assigned files.
        call erpfree(iunitp,istat)
        call usrmsg('Closing results analysis...',' ','P')
        CALL MOFREE
        CALL EPAGEND
        STOP         
      endif

C Define result-set requires from this database.
      call tstamp('>','RES: select result set')
      CALL MORESS

C Specify the system configuration file.
      if(autook)then
        IW=1
      else
        IW=1
      endif
      ITRC=IW-1
      CALL MOGCFG

C Zones to be included.
      CALL MOZDFN
      
C Specify output period.
      call tstamp('>','RES: default output period set')
      CALL MOOPER
      IDEFLT=0

C If there are general results images to be shown (ie. with
C an image focus of FRES then show them now.
      call imgdisp(0,'FRES',ier)

C If a command line action passed, see if action to be done before
C presenting the main menu. If running silent then exit.
      if(paction(1:4).eq.'ipv_')then
        call edisp(iuout,'Generating IPV report...')
        call ipvrep(iier)
        call edisp(iuout,'Generating IPV report...done.')
        if(autook)then
          CALL MOFREE
          CALL EPAGEND
          STOP
        endif
      elseif(paction(1:5).eq.'BEMS_')then

C If running in BEMS mode, use the commands in actf to guide extraction.
        bact='OPSS'
        write(bact,'(a4)') paction(6:9)
        call edisp(iuout,'Generating BEMS report...')
        call bemsrep(actf,bact,iier)
        call edisp(iuout,'Generating BEMS report...done.')
        if(autook)then
          CALL MOFREE
          CALL EPAGEND
          STOP
        endif
      elseif(paction(1:7).eq.'recover')then

C If providing a performance information file then scan contents.
C Set lpifl to file passed in parameter and set pifltog to OFF so
C that current user selections do not overwrite these contents.
        call edisp(iuout,' ')
        call edisp(iuout,'Generating specified output(s)...')
        lpifl=actf
        pifltog='OFF '
        call READPIF(ierr)
        call edisp(iuout,'Generating specified output(s)...done.')
        if(autook)then
          CALL MOFREE
          CALL EPAGEND
          STOP
        endif
      endif

C The above database, simulation result-set, output period
C and zone definition will remain in force until explicitly
C changed via the appropriate menu command.

C Setup for menu text.
    4 ITEM(1)   ='1 Select result file  '
      ITEM(2)   ='2 Select result set   '
      ITEM(3)   ='3 Define output period'
      ITEM(4)   ='4 Select zones        '
      ITEM(5)   ='  ------------------- '
      ITEM(6)   ='a Graphs              '
      ITEM(7)   ='c Timestep reports    '
      ITEM(8)   ='d Enquire about       '
      ITEM(9)   ='e Plant results       '
      ITEM(10)  ='f Indoor env. quality '
      ITEM(11)  ='g Electrical results  '
      ITEM(12)  ='h CFD                 '
      ITEM(13)  ='i Sensitivity         '
      ITEM(14)  ='j IPV                 '
      ITEM(15)  ='  ------------------- '
      IF(ITRC.EQ.0)THEN
        ITEM(16)='r Report >> silent    '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(16)='r Report >> summary   '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(16)='r Report >> detailed  '
      ENDIF
      ITEM(17)=  '* Preferences         '
      ITEM(18)=  '? Help                '
      ITEM(19)=  '- Quit                '

      NITMS=19
      INO=-2

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('res ',1)

    5 CALL EMENU('results analysis',ITEM,NITMS,INO)

C Test for illegal menu pick.
    6 IF(INO.EQ.0.OR.INO.EQ.5.OR.INO.EQ.15)goto 2
      IF(INO.GT.5.AND.INO.LT.15)THEN
        CALL MOCHEK(IER)
        IF(IER.EQ.1)goto 2
      ENDIF
      IF(INO.EQ.1)THEN
        IDEFLT=1
        call tstamp('>','RES: open new results database')
        CALL MORFIL(itype)
        if(iftype.eq.-2)then
          iftype=0
          goto 4    ! user canceled the file browse jump to menu.
        endif
        CALL MORESS

        CALL MOGCFG

C Zones to be included.
        CALL MOZDFN

C Read mass flow library.
        IFRES=IFIL+4
        call MFREADH(IFRES,ITRC)

C Specify output period.
        call tstamp('>','RES: default output period set')
        CALL MOOPER

C If there are general results images to be shown (ie. with
C an image focus of FRES then show them now.
        call imgdisp(0,'FRES',ier)
        IDEFLT=0
      ELSEIF(INO.EQ.2)THEN
        call tstamp('>','RES: select result set')
        CALL MORESS
      ELSEIF(INO.EQ.3)THEN
        call tstamp('>','RES: select period')
        CALL MOOPER
      ELSEIF(INO.EQ.4)THEN
        call tstamp('>','RES: select zones')
        CALL MOZDFN
      ELSEIF(INO.EQ.6)THEN
        call tstamp('>','RES: enter graphic reporting')
        CALL MOGRAF
        call tstamp('>','RES: exit graphic reporting')
      ELSEIF(INO.EQ.7)THEN
        call tstamp('>','RES: enter tabular reporting')
        CALL MOTABL
        call tstamp('>','RES: exit tabular reporting')
      ELSEIF(INO.EQ.8)THEN
        call tstamp('>','RES: enter statics reporting')
        CALL MOSTAT
        call tstamp('>','RES: exit statics reporting')
      ELSEIF(INO.EQ.9)THEN
        call tstamp('>','RES: enter plant reporting')
        call sysoutp('-',1)
        call tstamp('>','RES: exit plant reporting')
      ELSEIF(INO.EQ.10)THEN
        call tstamp('>','RES: enter IAQ reporting')
        call iaqmenu
        call tstamp('>','RES: exit IAQ reporting')
      ELSEIF(INO.EQ.11)THEN
        call tstamp('>','RES: enter electrical reporting')
        call ENETRES
        call tstamp('>','RES: exit electrical reporting')
      ELSEIF(INO.EQ.12)THEN
        call tstamp('>','RES: enter CFD reporting')
        call MOCFD
        call tstamp('>','RES: exit CFD reporting')
      ELSEIF(INO.EQ.13)THEN
        call tstamp('>','RES: enter sensitivity reporting')
        call MOSA
        call tstamp('>','RES: exit sensitivity reporting')
      ELSEIF(INO.EQ.14)THEN
        call tstamp('>','RES: enter IPV reporting')
        call ipvrep(iier)
        call tstamp('>','RES: exit IPV reporting')
      ELSEIF(INO.EQ.(NITMS-3))THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
        INO=-4
        GOTO 4
      ELSEIF(INO.EQ.(NITMS-2))THEN

C Res preferences.
        CALL SETUP(iuout,IER)
      ELSEIF(INO.EQ.(NITMS-1))THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('res ',0)
      ELSEIF(INO.EQ.NITMS)THEN

C Program termination. Free all assigned files and temporal scrach.
        call usrmsg(' Closing results analysis...',' ','P')
        CALL MOFREE

C Delete any temporal scratch file if temporal data associated with
C the model.
        if(ITDFLG.ne.0)then
          CALL ERPFREE(IUTDF,ISTAT)
          lnts=lnblnk(LTDF)
          if(LTDF(lnts-15:lnts).eq.'.temporalscratch')then
            call usrmsg('Deleting scratch file...',LTDF,'P')
            call EFOPRAN(iutdf,LTDF,ITWPR,1,IER)
            CALL EFDELET(iutdf,ISTAT)
            call usrmsg('Deleting scratch file...done.',LTDF,'P')
          endif
        endif

        call tstamp('>','Finish results analysis')
        CALL EPAGEND
        STOP
      ELSE
        goto 4
      ENDIF
      goto  4

C Output menu error signal and allow re-selection from menu.
    2 INO=-1
      goto 5

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      END


C ********** imgdisp
C imgdisp displays images associated with start-up or at specific points
C and the image browser has not been invoked do this now. In the case
C of the GTK version use an in-built GTK function rather than an
C external tool.
      subroutine imgdisp(iforce,focus,ier)
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C .esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the .esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF, XBMP)
C   fmtexe (20 char) is the application name used to display
C          images of type fmttag.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C Images in the model.
C  imgfmt (4 char) gives the format of each image associated with
C         the model, it must match one of the known fmttag.
C  imgfoc (4 char) associates an image with a specific topic:
C         'FZON' is related to zone composition
C         'FNET' is related to network composition
C         'FCTL' is related to control composition
C         'FDFS' is related to CFD domains
C         'FPER' is related to predicted performance
C         '****' is a general image displayed at startup
C  limgfil (72 char) name of the image file << ?? longer string >>
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      character ilist*200,dolist*254
      character fmttag*4,fmtexe*20,tfile*72,focus*4
      character longtfile*144,longtfiledos*144
      character topic*248,head*136,act*1
      integer iglib  ! if one then X11 if 2 then GTK if 3 text only.

      logical concat,show,unixok

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='imgdisp'  ! set for cfiles

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Loop through the images and display those which match the 'focus'
C and can be displayed with a known utility. Iforce, if set to 1,
C forces display.
      if(noimg.eq.0)return
      if(imgtyp.eq.0)then
        call edisp(iuout,'* No image display application available.')
        call edisp(iuout,'Ask your administrator to define one via')
        call edisp(iuout,'your .esprc file.')
        return
      endif

      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1)then

C If X11 version then request an external agent to display the image.
        if((iton.eq.0.or.iforce.eq.1))then
          do 498 imgv=1,imgtyp
            ilist=' '
            ix=1
            do 49 img=1,noimg
              if(imgfmt(img)(1:3).eq.fmttag(imgv)(1:3))then

C If focus is `****` then show images marked with `****` , otherwise
C begins with and `S`, otherwise attempt to match the focus and the image. 
                show=.false.
                if(focus(1:4).eq.'****'.and.
     &             imgfoc(img)(1:1).eq.'S')show=.true.
                if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
                if(show)then
                  longtfile=' '
                  if(unixok)then
                    call addpath(limgfil(img),longtfile,concat)
                  else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.  If X11 then an external application is used
C an double quoting is useful. If GTK then internal function used and
C quotes are not required.
                    call addpath(limgfil(img),longtfile,concat)
                    if(iglib.eq.1)then
                      call cmdfiledos(longtfile,longtfiledos,ier)
                    elseif(iglib.eq.2)then
                      call imgfiledos(longtfile,longtfiledos,ier)
                    endif
                    longtfile=' '
                    longtfile=longtfiledos
                  endif
                  ixl=lnblnk(longtfile)
                  ixe=ix+ixl
                  if(ixe.le.200)then
                    WRITE(ilist(ix:ixe),'(a,2x)')longtfile(1:ixl)
                  endif
                  ix=ixe+1
                endif
              endif
 49         continue
            if(ix.gt.1)then
              dolist=' '
              if(fmtexe(imgv)(1:2).eq.'  ')then
                call usrmsg(
     &    'The image display tool has not be defined. Ask your',
     &    'administrator to update the .esprc file definition.','W')
              else

C If cfg recent version then display image documentation as well.
                if(icfgv.gt.3)then
                  call edisp248(iuout,imgdoc(img),100)
                endif
                write(dolist,'(a,2x,a,a)')
     &            fmtexe(imgv)(1:lnblnk(fmtexe(imgv))),
     &            ilist(1:lnblnk(ilist)),' &'
                call runit(dolist,'graph')
                iton=1
              endif
            endif
 498      continue
        endif
      elseif(iglib.eq.2)then

C If GTK version then use GTK calls to display images in popup.
        if((iton.eq.0.or.iforce.eq.1))then

C Setup help text for the popup.
          helptopic='images_associated_with'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPW(nbhelp,IHW,IER)

          do 99 img=1,noimg
            if(imgfmt(img)(1:3).eq.'GIF'.or.
     &         imgfmt(img)(1:4).eq.'XBMP'.or.
     &         imgfmt(img)(1:3).eq.'TIF'.or.
     &         imgfmt(img)(1:3).eq.'JPG')then

C If focus is `****` then show images marked with `****` , otherwise
C begins with and `S`, otherwise attempt to match the focus and the image. 
C Create a string topic to pass to the display routine along with
C the image file name.
C << tag for editing ?? >>
              show=.false.
              if(focus(1:4).eq.'****'.and.
     &           imgfoc(img)(1:1).eq.'S')show=.true.
              if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
              if(show)then
                longtfile=' '
                if(unixok)then
                  call addpath(limgfil(img),longtfile,concat)
                else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
                  call addpath(limgfil(img),longtfile,concat)
                  call cmdfiledos(longtfile,longtfiledos,ier)
                  longtfile=' '
                  longtfile=longtfiledos
                endif
                if(icfgv.gt.3)then
                  idl=lnblnk(imgdoc(img))
                  write(topic,'(a)') imgdoc(img)(1:idl)
                endif

C Provide some feedback, set the title of the popup and then call C
C function in lib/esp_draw.c
                call edisp(iuout,topic)
                write(head,'(2a)') 'Image: ',longtfile(1:ixl)
                act='-'
                call popupimage(head,topic,act,longtfile)
                iton=1
              endif
            endif
  99      continue
        endif
      endif

      return
      end


C Dummy routines from common3dv.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
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
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITSNR,JITGRD,JITORG
      integer JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITSNR,JITGRD,JITORG
      integer*8 JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#endif     
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

C In support of floating point exceptions. Comment out for NON SOLAIRS.
#ifdef SUN
#ifdef F90
      integer function SIGFPE_bps(sig,code,context)
      integer sig,code,context(5)
      character label*16
      if(loc(code).eq.208) label = 'invalid'
      if(loc(code).eq.200) label = 'division by zero'
      if(loc(code).eq.212) label = 'overflow'
      if(loc(code).eq.204) label = 'underflow'
      if(loc(code).eq.196) label = 'inexact'
      write(6,*) 'Exception code ',loc(code),label,'at ',context(4)
      SIGFPE_bps = loc(code)
      end
#endif
#endif

