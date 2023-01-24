C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file comprises the following subroutines.
C MAIN    - Initialises the user terminal and user-specified
C           results file, then displays the menu.
C imgdisp - Display images associated with a model either at application
C           start-up or at specific points thereafter.
C Various dummy subroutines.

C ******************** res ********************
C Initialises the user terminal and user-specified
C results file, then displays the menu.

      program res
      USE START_UP
#include "building.h"
#include "site.h"
#include "model.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

C IUOUT & IUIN - user write and read unit numbers.
      common/OUTIN/IUOUT,IUIN,IEOUT

C IFIL, the basic file unit number, is set to 10.
C   +  0: building results library
C   +  1: any temporarily opened file
C   +  2: any temporarily opened file (?)
C   +  3: primitive construction database
C   +  4: mass flow results library
C   +  5: weather file
C   +  6: output data file
C   +  7: TDF file
C   +  8: for external help text
C   +  9: MLC database
C   + 10: plant results file
C   + 11: plant component database
C   + 12: output data parameters file (for xvgr/ xmgr)
C   + 13: recovery parameters file
C   + 14: CFD results file
C   + 21: power flow results file
      common/FILEP/IFIL
      common/SPAD/MMOD,LIMIT,LIMTTY
      common/GFONT/IFS,ITFS,IMFS
      common/appw/iappw,iappx,iappy

      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/OUTPCH/ICOUT    ! output channel
      common/DEFLT/IDEFLT
      common/trc/itrc
      common/exporttg/xfile,tg,delim
      common/EXPORTI/ixopen,ixunit,ixpunit

C Flags identifying if results files are available (0 no, 1 yes)..
      common/LIBOK/IBLIB,IFLIB,ICLIB,IELIB,IPLIB

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Simulation period and save option.
      common/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Simulator parameters and performance information requested data.
      LOGICAL     spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      common/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok

      common/SPFLN/spsname
      common/ract/paction
      common/recov01/pifltog,lpifl

C Path to model and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

      integer ireportunit ! 0 is default, 1 is W, 2 is kW, 3 is J.
      common/repunit/ireportunit

C CFD commons.
      common/flvimg/imgtyp,IMOPTS
      common/flvpar/Lflpar(MCOM)

C Flags for generic data retrieval routines:
      common/IGETFLG/IOCUPF,ialstused,IROC

C Optional title for graph and up to 10 other labels and their positions.
      common/grextras/graftitle,grlbl(10),ngrlbl,lblpx(10),lblpy(10)
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      COMMON/pres/iunitp  ! file unit for plant

      LOGICAL     CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      
C Name of current application.
      common/APPNAME/cAppName

      logical libheading     ! have we printed report heading (library name etc.).
      logical headingcontext ! has set or period changed so heading needs reprinted.
      common/libhead/libheading,headingcontext
      integer ISTRW

C Ask to overwrite flag.
      common/OVRWT/AUTOVR
      logical AUTOVR

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture   ! to pass to updcapt
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture   ! to pass to updcapt
#endif
      
      character cAppName*12      
      
      logical     unixok,there

      character ITEM(19)*23
      character*96 actf
      character*72 Lflpar
      character inf*144,LCMDFL*144,outs248*248,xfile*144
      character outs*124,MODE*4,tg*1,delim*1
      character*72 LFCFG
      character path*72,spsname*30,paction*16
      character fs*1,act*16,aut*12
      character journcmd*20,jfile*72,cjfile*72,OUTSTR*124
      character bact*4,pifltog*4,lpifl*96
      character graftitle*64,grlbl*24
      character firstbit*72,secondbit*72
      character longtmp*144
      character dstmp*24,uname*24,tfile*72
      character hold32*32     ! for helper applications

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*68 ! for banner title via epages call

      integer iglib     ! if 1 then X11, 2 then GTK, 3 then text only
      integer NITMS,INO ! max items and current menu item

C Initialise global common data.
      call ezero
      call curmodule('res ')
      cAppName = 'res'
      helpinapp='res'   ! set once for the application
      helpinsub='res'   ! set for MAIN
      IUOUT=6
      IUIN=5
      ICOUT=0
      IFIL=10
      LIMTTY=30
      LIMIT =30
      NSIGFIG=4
      IFS=0
      ITFS=1
      IMFS=1
      IFDAY=3
      IFTIME=3
      ICLIM=IFIL+5
      IOPTDB=IFIL+6
      IUTDF=IFIL+7
      IUTDFA=IFIL+8
      iunitp=IFIL+10
      matver=0.0     ! initial assumption of binary materials database
      ireportunit=0  ! assume normal units for tabular listings
      libheading=.false.; headingcontext=.false. ! no heading yet printed
      AUTOVR=.false. ! by default, ask to overwrite files

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

C Flags identifying if results files are available. Initialise all to 0.
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

C Assume that the configuration, mlc db, primitive db and control file
C have not been read in.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      OPTKOK=.FALSE.
      pifltog='OFF'
      lpifl='UNKNOWN'
      IDEFLT=1
      IOPT=2
      graftitle=' '
      ngrlbl=0
      grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '; grlbl(4)=' '
      grlbl(5)=' '; grlbl(6)=' '; grlbl(7)=' '; grlbl(8)=' '
      grlbl(9)=' '; grlbl(10)=' '

C Set CFD visualisation parameter files to UNKNOWN.
      do 8 I=1,MCOM
        Lflpar(I)='UNKNOWN'
 8    continue
      imgtyp=0
      IMOPTS=0

C Initialise coordinates for eye point, view point and angle of view.
C (required for 3-D CFD views).
      EYEM(1)=-100.
      EYEM(2)=-100.
      EYEM(3)=100.
      VIEWM(1)=10.
      VIEWM(2)=10.
      VIEWM(3)=10.
      ANG=40.

C Initialise output device.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=660
        iappx=60
        iappy=70
      else
        if(iappx.le.0)iappx=60
        if(iappy.le.0)iappy=70
        if(iappw.le.200)then
          iappwi=int(660*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/725.0))

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

C Set initial font sizes (IMFS is for menus, IFS is for dialog and ITFS is for text
C feedback). Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for
C menus and dialog.
        IMFS=5
        IFS=4
        ITFS=1
#ifdef OSX
        IMFS=4
        IFS=4   ! use a smaller dialog font
        ITFS=0
#endif
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        IMFS=5  ! Text mode set LIMTTY larger for paging menus.
        IFS=4
        ITFS=1
        LIMTTY=30
        LIMIT =30
      ENDIF

C Find the current ESP-r version number and add it to the application window title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Results Analyser of ESP-r ',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11, GTK or text only.
        if(iglib.eq.1)then

C Setup and pass in parameters to win3d.
          iiw1=15; iiw2=15; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call opencpw
          call opensetup
          iicapture=1
          call updcapt(iicapture)
        elseif(iglib.eq.2)then
          iiw1=9; iiw2=20; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
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
      ELSE
        
C Set reasonable defaults for text mode.
        igw=418; igwh=268; igl=107; igb=341
      ENDIF
      ITRC=1
      IF(MMOD.NE.-6)ICOUT=IUOUT
      if(aut(1:6).eq.'silent')then
        CALL ESPrVersion("silent",cAppName,IUOUT)
      else
        CALL ESPrVersion("summary",cAppName,IUOUT)
      endif

C Find the user's home folder and get user's custom settings.
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

C If there is a journal active, prepare to add to the journal. First
C get the root journal file name from the user's home folder.
C All subsequent tstamp messages will be appended to this file.
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

C Confirm if there an an xterm available.
      call isunix(unixok)
      if(unixok)then
        found_xterm=.false.; hold32='xterm'
        call isinstalled(hold32,found_xterm)
      else
        found_xterm=.false.  ! not (yet) in Windows
      endif

C Open the session file.
  902 ieout=ifil+932          ! set to unused index 942
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for res'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  904 continue

C Scan the defaults file for default configuration.
      call escdef(ixunit,'s',IER)

C Take command line file name as initial results file name.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
      else
        LCMDFL='UNKNOWN'
      endif

C Check for command line actions.
      if(act(1:2).ne.'  '.and.act(1:4).ne.'UNKN')then
         paction=act
         write(outs,'(2a)') 'The action is: ',paction
         call edisp(iuout,outs)
         write(outs,'(2a)') 'RES: the input action is: ',paction
         call tstamp('>',outs)
         if(aut(1:6).eq.'silent')then
           autook=.true.
           call tstamp('>','Silent-running mode')
         else
           autook=.false.
         endif
      endif

C Export (grtool) file information. Temporarily use 
C unit ifil+6 for output file.
      xfile = 'data.csv'
      ixopen = 0
      ixunit = ifil + 6
      ixpunit = ifil + 12
      delim = '-'

C Assume materials input from IFIL+3.
      IFMAT=IFIL+3
      write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))

C Assume multilayer input from IFIL+9.
      IFMUL=IFIL+9
      write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))

C Assume CFClayers input from IFIL+10.
      icfcdb=ifil+10 
      write(LCFCDB,'(a)') DCFCDB(1:lnblnk(DCFCDB))

C Initialise results file.
      CALL MORFIL(iftype)
      if(iftype.eq.-2)then
        iftype=0
        goto 4    ! user cancelled the file browse jump to menu.
      elseif(iftype.eq.-1) then

C Get the name of the configuration file name and read the plant configuration file.
        if(IPLIB.eq.0.or.IPLIB.eq.1)then
          helptopic='res_dialog_for_cfg_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(longtmp,'(a)') LCFGF(1:lnblnk(LCFGF))

          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            ISTRW=96
          elseif(iglib.eq.2)then
            ISTRW=144
          else
            ISTRW=96
          endif
          CALL EASKF(longtmp,' ',
     &      'Corresponding system configuration file name?',
     &      ISTRW,' ','configuration file',IER,nbhelp)
          if(ier.eq.-3) goto 902  ! cancel detected, pass back -3 in ier.

          call fdroot(longtmp,path,LCFGF)

C Read the model plant-only configuration file. Also set the initial
C export file name to reflect the model root name.
          IFCFG=IFIL+1
          IAPROB=IFIL+2  
          MODE='ALL '
          call ERSYS(LFCFG,IFCFG,IAPROB,MODE,ITRC,IER)
          write(xfile,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.csv'
        endif

C If we have a plant-only file go straight to the plant results menu.
        call sysoutp('-',0)

C Free all assigned files.
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call erpfree(iunitp,istat)
        CALL MOFREE
        CALL EPAGEND
        STOP         
      endif

C Define result-set required from this file.
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

C PATMOS is atmospheric pressure in mbar; atmpres is in Pa.
        PATMOS=atmpres/100.0

C Get zones to be included.
      CALL MOZDFN
      
C Specify output period.
      call tstamp('>','RES: default output period set')
      CALL MOOPER
      IDEFLT=0

C If there are general results images to be shown (ie. with
C an image focus of FRES then show them now.
      call imgdisp(0,'FRES',ier)

C If a command line action passed, see if action is to be done before
C presenting the main menu. If running silent then exit.
      if(paction(1:4).eq.'ipv_')then
        call edisp(iuout,'Generating IPV report ...')
        call ipvrep(iier)
        call edisp(iuout,'Generating IPV report ... done.')
        if(autook)then
          CALL MOFREE
          close(ieout)
          CALL ERPFREE(ieout,ISTAT)
          CALL EPAGEND
          STOP
        endif
      elseif(paction(1:5).eq.'BEMS_')then

C If running in BEMS mode, use the commands in actf to guide extraction.
        bact='OPSS'
        write(bact,'(a4)') paction(6:9)
        call edisp(iuout,'Generating BEMS report ...')
        call bemsrep(actf,bact,iier)
        call edisp(iuout,'Generating BEMS report ... done.')
        if(autook)then
          CALL MOFREE
          close(ieout)
          CALL ERPFREE(ieout,ISTAT)
          CALL EPAGEND
          STOP
        endif
      elseif(paction(1:7).eq.'recover')then

C If providing a performance information file then scan contents.
C Set lpifl to file passed in parameter and set pifltog to OFF so
C that current user selections do not overwrite these contents.
        call edisp(iuout,' ')
        call edisp(iuout,'Generating specified output(s) ...')
        lpifl=actf
        pifltog='OFF '
        call READPIF(ierr)
        call edisp(iuout,'Generating specified output(s) ... done.')
        if(autook)then
          CALL MOFREE
          close(ieout)
          CALL ERPFREE(ieout,ISTAT)
          CALL EPAGEND
          STOP
        endif
      endif

C The above database, simulation result-set, output period
C and zone definition will remain in force until explicitly
C changed via the appropriate menu command.

C Setup for menu text.
    4 ITEM(1)   ='1 results file         '
      ITEM(2)   ='2 result set           '
      ITEM(3)   ='3 output period        '
      ITEM(4)   ='4 building zones       '
      ITEM(5)   ='  -------------------- '
      ITEM(6)   ='a graphs               '
      ITEM(7)   ='c reports              '
      ITEM(8)   ='d enquire about        '
      ITEM(9)   ='e plant results        '
      ITEM(10)  ='f indoor environment   '
      ITEM(11)  ='g electrical results   '
      ITEM(12)  ='h flow results (CFD)   '
      ITEM(13)  ='i sensitivity results  '
      ITEM(14)  ='j IPV                  '
      ITEM(15)  ='  -------------------- '
      IF(ITRC.EQ.0)THEN
        ITEM(16)='r report >> silent     '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(16)='r report >> summary    '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(16)='r report >> detailed   '
      ENDIF
      ITEM(17)=  '* preferences          '
      ITEM(18)=  '? help                 '
      ITEM(19)=  '- quit module          '

      NITMS=19
      INO=-2

C Use askabout to instantiate the initial help messages (2nd parameter is 1).
      call askabout('res ',1)

    5 CALL EMENU('Module options',ITEM,NITMS,INO)

C Test for illegal menu pick.
    6 IF(INO.EQ.0.OR.INO.EQ.5.OR.INO.EQ.15)goto 2
      IF(INO.GT.5.AND.INO.LT.15)THEN
        CALL MOCHEK(IER)
        IF(IER.EQ.1)goto 2
      ENDIF
      IF(INO.EQ.1)THEN
        IDEFLT=1
        call tstamp('>','RES: open new results file')
        CALL MORFIL(itype)
        if(iftype.eq.-2)then
          iftype=0
          goto 4    ! user cancelled the file browse, jump to menu.
        endif
        CALL MORESS

        CALL MOGCFG

C PATMOS is atmospheric pressure in mbar; atmpres is in Pa.
        PATMOS=atmpres/100.0

C Zones selection.
        CALL MOZDFN

C Read mass flow file.
        IFRES=IFIL+4
        call MFREADH(IFRES,ITRC)

C Output period selection.
        call tstamp('>','RES: default output period set')
        CALL MOOPER

C If there are general results images to be shown (ie. with
C an image focus of FRES) then show them now.
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
        call MOCFD('M')
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

C Help via askabout in esru_ask.F (the zero requests display).
        call askabout('res ',0)
      ELSEIF(INO.EQ.NITMS)THEN

C Program termination.
        CALL MOFREE

C Delete any scratch file if temporal data is associated with
C the model.
        if(ITDFLG.ne.0)then
          CALL ERPFREE(IUTDF,ISTAT)
          lnts=lnblnk(LTDF)
          if(LTDF(lnts-15:lnts).eq.'.temporalscratch')then
            call usrmsg('Deleting scratch file ...',LTDF,'P')
            call EFOPRAN(iutdf,LTDF,1,1,IER)
            CALL EFDELET(iutdf,ISTAT)
            call usrmsg('Deleting scratch file ... done.',LTDF,'P')
          endif
        endif

        call tstamp('>','Finish results analysis')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        CALL EPAGEND
        goto 999        
      ELSE
        goto 4
      ENDIF
      goto  4

C Output menu error signal and allow re-selection from menu.
    2 INO=-1
      goto 5

 901  call edisp(iuout,' ')
      call edisp(iuout,'Error opening journal file!')
      goto 902
 903  call edisp(iuout,' ')
      call edisp(iuout,'Error opening session log!')
      goto 904

 999  END PROGRAM


C ********** imgdisp
C Displays images associated with a model either at application start-up
C or at specific points thereafter.

      subroutine imgdisp(iforce,focus,ier)
#include "building.h"
#include "model.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Possible image formats are defined in the .esprc file. To add
C additional formats edit the ESP-r Install script at the point
C where it creates the .esprc file.
C   imgtyp           - the number of image formats supported.
C   fmttag (4 char)  - GIF, PNG, XBMP etc.
C   fmtexe (20 char) - application used to display each images type.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C Images associated with the model.
C   imgfmt (4 char)  - gives the image format.
C   imgfoc (4 char)  - associates an image with a specific topic:
C         'FZON' for zone composition
C         'FNET' for network composition
C         'FCTL' for control composition
C         'FDFS' for flow domains
C         'FPER' for performance
C         '****' for an image to be displayed at application start-up.
C   limgfil (72 char) - name of the image file.
      character imgfmt*4   ! GIF XBMP TIF JPG PNG
      character imgfoc*4   ! FZON FNET FCTL FDFS
      character limgfil*72 ! file name (extend to 144 char)
      character imgdoc*248 ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg        ! number of images.
      integer iton         ! 0 if images not yet shown, 1 if yes.
      common/imagfi/noimg,iton

      character ilist*200,dolist*254
      character fmttag*4,fmtexe*20,tfile*72,focus*4
      character longtfile*144,longtfiledos*144
      character topic*248,head*136,act*1
      integer iglib        ! if 1 then X11 if 2 then GTK, if 3 text only.

      logical concat,show,unixok

      helpinsub='res'  ! set for cfiles

C Check if Unix- or DOS-based.
      call isunix(unixok)

C Loop through images and display those that match the 'focus'.
C Iforce = 1 forces display.
      if(noimg.eq.0)return
      if(imgtyp.eq.0)then
        call edisp(iuout,' ')
        call edisp(iuout,'Image display application not found!')
        call edisp(iuout,'(Define one via the .esprc file.)')
        return
      endif

      iglib = igraphiclib()  ! find out if X11, GTK or text only.
      if(iglib.eq.1)then

C For X11 version request display via an external app.
        if((iton.eq.0.or.iforce.eq.1))then
          do 498 imgv=1,imgtyp
            ilist=' '
            ix=1
            do 49 img=1,noimg
              if(imgfmt(img)(1:3).eq.fmttag(imgv)(1:3))then

C If focus is `****` show all images marked with `****` or beginning with
C a `S`, otherwise attempt to match the focus and the image. 
                show=.false.
                if(focus(1:4).eq.'****'.and.
     &             imgfoc(img)(1:1).eq.'S')show=.true.
                if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
                if(show)then
                  longtfile=' '
                  if(unixok)then
                    call addpath(limgfil(img),longtfile,concat)
                  else

C If running on a non-linux machine, determie if there are spaces in the
C name and change any / to \. If X11 use external application. If GTK use
C an internal function.
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
     &    'An image display tool canot be located. Define',
     &    'one via your .esprc file.','W')
              else

C For most recent cfg file format, display image documentation as well.
                if(icfgv.gt.3)then
                  call edisp248(iuout,imgdoc(img),100)
                endif
                write(dolist,'(a,2x,a,a)')
     &            fmtexe(imgv)(1:lnblnk(fmtexe(imgv))),
     &            ilist(1:lnblnk(ilist)),' & '
                call runit(dolist,'-')
                iton=1
              endif
            endif
 498      continue
        endif
      elseif(iglib.eq.2)then

C If GTK version, use function call to display images in popup.
        if((iton.eq.0.or.iforce.eq.1))then

C Help text for the popup.
          helptopic='images_associated_with'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPW(nbhelp,IHW,IER)

          do 99 img=1,noimg
            if(imgfmt(img)(1:3).eq.'GIF'.or.
     &         imgfmt(img)(1:4).eq.'XBMP'.or.
     &         imgfmt(img)(1:3).eq.'TIF'.or.
     &         imgfmt(img)(1:3).eq.'PNG'.or.
     &         imgfmt(img)(1:3).eq.'JPG')then

C If focus is `****` then show images marked with `****` or beginning
C with a `S`, otherwise attempt to match the focus and the image. 
C Create a string topic to pass to the display routine along with
C the image file name.
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

C Provide some feedback, set the title of the popup and then call
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


C ********** Dummy subroutines (from common3dv.F)

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
     &   JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD,JITORG,GDIS,JITBND,JITDSP,
     &   JITHLS,JITHLZ,JITPPSW)

C Passed parameters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type, set integer size of passed parameters.
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
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
      ier=0
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

      subroutine Find_season_parameters()
      return
      end

      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)
      integer icomp,ier
      return
      end

