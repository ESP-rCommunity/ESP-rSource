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


C Clm.f is the main controlling code of the weather data display and 
C analysis facility of the ESRU simulation suite of programs.
C Clm allows the creation, modification and analysis of
C climatic data sets compatible with ESP-r.

      program clm
      USE START_UP
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "seasons.h"
#include "climate.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/FILEP/IFIL
      common/trc/itrc
      COMMON/ER/IER
      COMMON/INPER/INIT
      COMMON/OUTPCH/ICOUT
      integer iuout,iuin,ieout
      common/OUTIN/IUOUT,IUIN,IEOUT
      integer iecount    ! count of warning messages
      common/warncounter/iecount

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

      character CLMLOC*42
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF

C Indicator of possible command line action to take and the
C file associated with this action.
      common/ract/paction,pascif

C Path to problem and command line file (if any).  Note: within
C prj there is a concept of standard location for weather data or
C model located weather data. Clm might or might not know about
C this.
      common/rpath/path
      common/rcmd/LCMDFL

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Redirected text/graphics parameters.
      common/exporttg/xfile,tg,delim
      common/exporttgi/ixopen,ixloc,ixunit

C Weather list common.
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR      

      character ITEM*27
      dimension ITEM(22)
      character LASCI*96,outs*124,outs248*248
      character PERST1*14,PERST2*44,PERST3*44,LCMDFL*144
      character inf*144,ascif*96,pascif*96,llasci*144
      character pact*16,aut*12,paction*16
      character path*72,xfile*144,tg*1,delim*1
      character tab*1,fs*1,OUTSTR*124
      character journcmd*20,jfile*72,cjfile*72
      character blank*2   ! string for edisp of blank line
      character dstmp*24,uname*24,tfile*72
      character ltpath*72,filen*72
      character hold32*32     ! for helper applications

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*62 ! for banner title via epages call

      LOGICAL OK,unixok,there,silent

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ic,iwm  ! for radio buttons
      integer NITEM,INO  ! max items and current menu item
      integer ISTRW
      logical summary ! to confirm sucessful read
      character minu*1,second*1,longdms*14,latdms*14

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture   ! to pass to updcapt
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture   ! to pass to updcapt
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

C Initial assumptions. Pass module name to C code structure.
      call ezero
      call curmodule('clm ')
      cAppName = 'clm'
      helpinapp='clm'  ! set once for the application
      helpinsub='clm'  ! set for MAIN
      tab = CHAR(9)
      ITRC=1
      IUOUT=6
      IUIN=5
      LIMTTY=30
      LIMIT =30
      IFIL=10
      iuj=IFIL+10
      IFDAY=2
      IFTIME=0
      ID1=1
      IM1=1
      IT1=1
      ID2=31
      IM2=12
      IT2=24
      IDS=1     ! Initial assumption for a whole year.
      IDF=365
      KDS=1
      KDF=24
      IYEAR=2022
      ICYEAR=2022
      cfgroot='  '
      LCFGF='  '
      AUTOVR=.false.  ! by default, ask to overwrite files
      summary=.false. ! do not provide summary of weather

C Set default early winter, spring, summer, autumn, late winter periods.
      CALL EDAY(9,1,ia1wins)
      CALL EDAY(15,1,ia1winf)
      CALL EDAY(6,3,ia1sprs)
      CALL EDAY(12,3,ia1sprf)
      CALL EDAY(11,7,iasums)
      CALL EDAY(17,7,iasumf)
      CALL EDAY(2,10,ia2sprs)
      CALL EDAY(8,10,ia2sprf)
      CALL EDAY(20,11,ia2wins)
      CALL EDAY(26,11,ia2winf)

C Default season definitions.
      CALL EDAY(1,1,is1wins)
      CALL EDAY(28,2,is1winf)
      CALL EDAY(1,11,is2wins)
      CALL EDAY(31,12,is2winf)
      CALL EDAY(1,3,is1sprs)
      CALL EDAY(30,4,is1sprf)
      CALL EDAY(1,9,is2sprs)
      CALL EDAY(31,10,is2sprf)
      CALL EDAY(1,5,is1sums)
      CALL EDAY(31,8,is1sumf)

C Set default weatherlist text entries.
      clmlnam = 'not yet entered'
      clmlaid = 'not yet entered'
      clmloc  = 'not yet entered'
      clmlavail = 'OFFLINE'
      clmldbfile = ' '
      nbclmlhelp = 1
      clmlhelp(1) = 'not yet entered'
 
C Text/graphic feedback redirection on channel ifil +12 
      ixunit = ifil + 12
      delim = '-'
      ixopen = 0

C Get command line parameters.
      call parclm(MODL,iappw,iappx,iappy,inf,pact,aut,ascif)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

c Initialise output device, assume minimal trace, set reduced
C size of window display.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=620
        iappx=80
        iappy=80
      else
        if(iappx.le.0)iappx=80
        if(iappy.le.0)iappy=80
        if(iappw.le.200)then
          iappwi=int(620*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/725.0))

C Set initial menu width and text feedback number of lines.
      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(28*iappw*0.01),18)
        LIMTTY= MAX0(int(8*iappw*0.01),4)
        LIMIT = MAX0(int(8*iappw*0.01),4)
      else
        menuchw = 28
        LIMTTY=8
        LIMIT =8
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for menus
C and dialog.
        IMFS=5
        IFS=5
        ITFS=5
#ifdef OSX
        IMFS=4
        IFS=4   ! use a smaller dialog font
        ITFS=4
#endif
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        IMFS=5  ! Text mode set LIMTTY larger for paging menus.
        IFS=5
        ITFS=5
        LIMTTY=30
        LIMIT =30
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Weather module of ESP-r ',
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
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then

C Setup and pass in parameters to win3d.
          iiw1=10; iiw2=22; iiw3=4; iiw4=3; iimenu=menuchw
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
        igw=349; igwh=265; igl=77; igb=325
      ENDIF

      blank='  '
      call edisp(IUOUT,blank)
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get user's custom settings.
C Make temporary use of file unit IAF=IFIL+1.
      IAF=IFIL+1
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IAF,0,IIER)

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

        write(outs,'(a,a)')'Journal (weather analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif
  902 continue

C Confirm if there an an xterm available.
      call isunix(unixok)
      if(unixok)then
        found_xterm=.false.; hold32='xterm'
        call isinstalled(hold32,found_xterm)
      else
        found_xterm=.false.  ! not (yet) in Windows
      endif


C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=ifil+932          ! set to unused index 942
      iecount=0               ! clear count of errors
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for clm'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  904 continue

C Scan the defaults file for default configuration.
      call escdef(IAF,'-',IER)

C Weather file is on IFIL.
      ICLIM=IFIL
      LCLIM=DCLIM
      INIT =0

C Take command line file name to LCMDF and use as initial weather file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        LCMDFL=' '
      else
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
C        if(itrc.gt.1)call edisp248(iuout,outs248,80)
      endif

C Take command line action parameter:
C If paction = 'asci2bin' or 'ascii2bin' then fill binary file `inf` with data 
C from `ascif` and exit.
C If paction is `bin2asci` or `bin2ascii`file ascii file `ascif` with data from
C the binary file `inf` and exit.
C If paction = `epw2bin` take user to point of interaction for dealing
C with this conversion.
      if(pact(1:1).ne.' ')then
        paction=pact
        write(outs,'(a,a)')' the action is: ',paction
        if(itrc.gt.1)call edisp(iuout,outs)
        write(outs,'(a,a)')'clm: the input action is: ',paction
        call tstamp('>',outs)
        if(paction(1:8).eq.'asci2bin'.or.
     &     paction(1:9).eq.'ascii2bin')then
          silent=.true.
          pascif=ascif
          CALL CFILIN(silent)                        ! Create empty weather file.
          NTSPH=1                                    ! Import weather data from ASCII file.
          CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,  ! Confirm period
     &      IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
          IUNIT=IFIL+1
          CALL MKBCLM(iuout,1,pascif,IUNIT,IDS,IDF,silent,IER)
          call tstamp('>','Finished weather module conversion ASCI2BIN')
          write(outs248,'(5a)')'Climate_conversion',tab,
     &      pascif(1:lnblnk(pascif)),tab,LCMDFL(1:lnblnk(LCMDFL))

C Close the session log.
          close(ieout)
          if(iecount.eq.0)then     ! if there were no warnings delete
            open(ieout,file=iefile,status='UNKNOWN')
            call efdelet(ieout,istat)
          endif
          CALL EPAGEND
          STOP
        elseif(paction(1:8).eq.'bin2asci'.or.
     &         paction(1:9).eq.'bin2ascii')then
          silent=.true.
          pascif=ascif

          CALL CFILIN(silent)   ! Read in the supplied weather file (LCMDFL).
          NTSPH=1               ! Write data to an ascii file.
          CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
          IAF=IFIL+1
          CALL MKACLM('E',pascif,IAF,IDS,IDF,IER)
          call tstamp('>','Finished weather module conversion BIN2ASCI')
          write(outs248,'(5a)')'Climate_conversion',tab,
     &      LCMDFL(1:lnblnk(LCMDFL)),tab,pascif(1:lnblnk(pascif))

          close(ieout)          ! Close the session log.
          if(iecount.eq.0)then  ! if there were no warnings delete
            open(ieout,file=iefile,status='UNKNOWN')
            call efdelet(ieout,istat)
          endif

          CALL EPAGEND
          STOP
        elseif(paction(1:7).eq.'epw2bin')then
          paction='INTERACTIVE'
          silent=.true.
          pascif=ascif
          CALL CFILIN(silent)  ! Create empty weather file.
          NTSPH=1              ! Import weather data from EPW file.
          IUNIT=IFIL+1
          CALL EPWTOBCLM(pascif,IUNIT,IER)
          call tstamp('>','Finished weather module conversion EPW2BIN')
          write(outs248,'(5a)')'Climate_conversion',tab,
     &      pascif(1:lnblnk(pascif)),tab,LCMDFL(1:lnblnk(LCMDFL))

          close(ieout)         ! Close the session log.
          if(iecount.eq.0)then ! if there were no warnings delete
            open(ieout,file=iefile,status='UNKNOWN')
            call efdelet(ieout,istat)
          endif

          CALL EPAGEND
          STOP
        else

C In interactive mode so do splash-screen and then read
C in the weather file name via list
C selection, editing or accepting the LCMDFL.
          call edisp(IUOUT,blank)
          paction='INTERACTIVE'
          silent=.false.
          CALL CFILIN(silent)
        endif
      else

C In interactive mode so do splash-screen and then read
C in the weather file name via list
C selection, editing or accepting the LCMDFL.
        call edisp(IUOUT,blank)
        CALL ESPrVersion("summary",cAppName,IUOUT)
        call edisp(IUOUT,blank)
        paction='INTERACTIVE'
        silent=.false.
        CALL CFILIN(silent)
      endif

C Main clm menu.
    3 INO=-4

C fdroot accepts a full input file name of up to 72 characters and returns
C the path and file name separately.
      call fdroot(LCLIM(1:72),ltpath,filen)
      write(ITEM(1),'(2A)')  'a weather: ',filen(1:15)
 
C Derive string for focus period.
      NTSPH=1
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(ITEM(2),'(A,A24)')'b ',PERST3(9:32)
      ITEM(3)=                '  ______________________ '
      ITEM(4)=                'c synoptic               '
      ITEM(5)=                'd graphical              '
      ITEM(6)=                'e psychrometric          '
      ITEM(7)=                'f tabular                '
      ITEM(8)=                '  ______________________ '
      if(IDNGH.eq.0)then
        ITEM(9)=              'g radiation >> DN        '
      elseif(IDNGH.eq.123)then
        ITEM(9)=              'g radiation >> GH        '
      endif
      ITEM(10)=               '  ______________________ '
      ITEM(11)=               'h edit weather station   '
      ITEM(12)=               'i edit weather           '
      ITEM(13)=               'j export                 '
      ITEM(14)=               'k import                 '
      ITEM(15)=               'l manage files           '
      ITEM(16)=               'm summary of weather     '
      ITEM(17)=               's preferences            '
      IF(ITRC.EQ.0)THEN
        ITEM(18)=             'r report >> silent       '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(18)=             'r report >> summary      '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(18)=             'r report >> detailed     '
      ENDIF
      if(ixopen.eq.1)then
        ITEM(19)='> output >> file  '
      elseif(ixopen.eq.0)then
        ITEM(19)='> output >> screen'
      endif
      if(delim.eq.'-')then
        ITEM(20)='^ delim >> normal '
      elseif(delim.eq.'T')then
        ITEM(20)='^ delim >> TAB    '
      elseif(delim.eq.'C')then
        ITEM(20)='^ delim >> comma  '
      elseif(delim.eq.'S')then
        ITEM(20)='^ delim >> space  '
      endif
      ITEM(21)=               '? help                    '
      ITEM(22)=               '- exit weather analysis   '

C Use askabout to instantiate the initial help messages (2nd parameter is one).
      call askabout('clm ',1)

C Present the opening display and control menu.
      NITEM=22
      INO=-3
      CALL EMENU('Weather analysis',ITEM,NITEM,INO)

c Exit the weather analysis tool.
      IF(INO.EQ.NITEM)THEN
        write(outs248,'(3a)')'Climate_selected',tab,
     &    LCMDFL(1:lnblnk(LCMDFL))
        call tstamp('>','Finished weather module')

C Close the session log.
        close(ieout)
        if(iecount.eq.0)then     ! if there were no warnings delete
          open(ieout,file=iefile,status='UNKNOWN')
          call efdelet(ieout,istat)
        endif
        CALL EPAGEND
        STOP

C Manage the weather file.
      ELSEIF(INO.EQ.1)THEN
        helptopic='select_db_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','Weather file options:',
     &   'new','use','reselect','cancel',ICT,nbhelp)
        if(ICT.eq.1)then

C Confirm that this is a new file and clear memory if necessary.
          helptopic='ok_to_clear_data'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ',
     &      'Clear weather data held in memory?',OK,nbhelp)
          if(OK)then
            call clrclm
            LCMDFL='UNKNOWN'
            call tstamp('>','CLM: cleared weather data')
          endif
          silent=.false.
          CALL CFILIN(silent)
        elseif(ICT.eq.2)then
          call tstamp('>','CLM: use current weather data')
          write(LCMDFL,'(a)') LCLIM(1:lnblnk(lclim))
          silent=.false.
          CALL CFILIN(silent)
        elseif(ICT.eq.3)then
          call tstamp('>','CLM: select weather data')
          LCMDFL='UNKNOWN'
          silent=.false.
          CALL CFILIN(silent)
        endif

C Period definition.
      ELSEIF(INO.EQ.2)THEN
        call selper(IER)
        INIT=1
        INEW=1

C Synoptic analysis.
      ELSEIF(INO.EQ.4)THEN
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(ITFS.eq.4) ITFS=0
        if(ITFS.eq.5) ITFS=1
        if(ITFS.eq.6) ITFS=2
        if(ITFS.eq.7) ITFS=3
        call userfonts(IFS,ITFS,IMFS)
        IF(INIT.EQ.0)CALL selper(IER)
        INIT=1
        call tstamp('>','CLM: enter synoptic analysis')
        CALL CLMSYN
        call tstamp('>','CLM: exit synoptic analysis')
        IMFS=lastmenufont
        ITFS=lasttextfont    ! reset to proportional font in text feedback
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        call usrmsg(' ',' ','-')  ! refresh dialog 

C Graphic analysis. Enlarge graphics area if small.
      ELSEIF(INO.EQ.5)THEN
        IF(INIT.EQ.0)CALL selper(ier)
        INIT=1
        if(MMOD.EQ.8)then
          if(LIMTTY.gt.12)then
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1)then
              if(iappw.gt.0.and.iappw.lt.100)then
                menuchw = MAX0(int(28*iappw*0.01),10)
                LIMTTY= MAX0(int(6*iappw*0.01),4)
                LIMIT = MAX0(int(6*iappw*0.01),4)
              else
                menuchw = 28
                LIMTTY=6
                LIMIT =6
              endif

C Setup and pass in parameters to win3d.
              iiw1=10; iiw2=22; iiw3=7; iiw4=3; iimenu=menuchw
              iigl=igl; iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              ilf=2; ild=LIMTTY
              call winclr
              CALL feedbox(iimenu,ilf,igfw,igfh)
              CALL opengdisp(iimenu,ild,ilf,igdw,igdh)
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
              igl=int(iigl); igr=int(iigr);igt=int(iigt);igb=int(iigb)
              igw=int(iigw); igwh=int(iigwh)
              call opencpw
              call opensetup
            elseif(iglib.eq.2)then
              continue
            endif
          ENDIF
          call tstamp('>','CLM: enter graphic analysis.')
          CALL CLMGRF
          call tstamp('>','CLM: exit graphic analysis.')
        else
          call edisp(iuout,' Not in graphic window!')
        endif

C Psychrometric analysis. Enlarge graphics area if small.
      ELSEIF(INO.EQ.6)THEN
        IF(INIT.EQ.0)CALL selper(ier)
        INIT=1
        if(MMOD.EQ.8)then
          if(LIMTTY.gt.12)then
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1)then
              if(iappw.gt.0.and.iappw.lt.100)then
                menuchw = MAX0(int(28*iappw*0.01),10)
                LIMTTY= MAX0(int(6*iappw*0.01),4)
                LIMIT = MAX0(int(6*iappw*0.01),4)
              else
                menuchw = 28
                LIMTTY=6
                LIMIT =6
              endif

C Setup and pass in parameters to win3d.
              iiw1=10; iiw2=22; iiw3=7; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              ilf=2; ild=LIMTTY
              call winclr
              CALL feedbox(iimenu,ilf,igfw,igfh)
              CALL opengdisp(iimenu,ild,ilf,igdw,igdh)
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
              igl=int(iigl);igr=int(iigr);igt=int(iigt);igb=int(iigb)
              igw=int(iigw);igwh=int(iigwh)
              call opencpw
              call opensetup
            elseif(iglib.eq.2)then
              continue
            endif
          ENDIF
          call tstamp('>','CLM: enter psychrometric analysis')
          CALL PSYCHART
          call tstamp('>','CLM: exit psychrometric analysis')
        else
          call edisp(iuout,' Not in graphic window!')
        endif

C Tabular analysis. Switch to monospace small font in text feedback.
      ELSEIF(INO.EQ.7)THEN
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(ITFS.eq.4) ITFS=0
        if(ITFS.eq.5) ITFS=1
        if(ITFS.eq.6) ITFS=2
        if(ITFS.eq.7) ITFS=3
        call userfonts(IFS,ITFS,IMFS)
        IF(INIT.EQ.0)CALL selper(ier)
        INIT=1
        call tstamp('>','CLM: enter tabular analysis')
        CALL CLMTOT
        call tstamp('>','CLM: exit tabular analysis')
        IMFS=lastmenufont
        ITFS=lasttextfont    ! reset to proportional font in text feedback
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        call usrmsg(' ',' ','-')  ! refresh dialog 

C Change radiation type.
      ELSEIF(INO.EQ.9)THEN
        helptopic='rad_change_password'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IANS=0
        CALL EASKI(IANS,' ','Password? ',
     &    0,'-',0,'-',0,'password',IER,nbhelp)
        IF(IANS.NE.101)then
          call usrmsg(' ','Password incorrect!','-')
          goto 3
        endif

        helptopic='rad_type_toggle'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Solar radiation type:',
     &    'direct normal','global horizontal',IRADT,nbhelp)
        if (CFVER.eq.1) then
          IRTYP=0
          IF(IRADT.EQ.2)IRTYP=123
          IDNGH=IRTYP
          ICDNGH=IRTYP
          IREC=366
          READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR
          WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IRTYP
        else
          if (IRADT.eq.1 .and. CMXST(4)) then
            CMCOL(3)=CMCOL(4); CMCOL(4)=0
            CMXST(3)=.true.;   CMXST(4)=.false.
          elseif (IRADT.eq.2 .and. CMXST(3)) then
            CMCOL(4)=CMCOL(3); CMCOL(3)=0
            CMXST(4)=.true.;   CMXST(3)=.false.
          endif
          IREC=368
          WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)(CMCOL(i),i=1,CFMCM)
          CALL CLMMDN2O
        endif
        call tstamp('>','CLM: change radiation type')
        goto 3
 1000   call edisp(iuout,' CLM: weather file read/write error!')
        goto 3

C Edit site attributes.
      ELSEIF(INO.EQ.11)THEN
        call sitedit

C Creation of weather file from manual editing.
      ELSEIF(INO.EQ.12)THEN
        helptopic='clm_data_creation_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IC=1
        CALL EASKATOG(' ','Specify data via:',
     &    'keyboard','prediction','transforms','curve fit',
     &    'cancel',' ',' ',IC,nbhelp)

        if(ic.eq.1)then
          call tstamp('>','CLM: manual editing')
          CALL RRITE
        elseif(ic.eq.2)then
          call tstamp('>','CLM: predictive editing')
          ist=1
          CALL CLPRED(ist)
        elseif(ic.eq.3)then
          call tstamp('>','CLM: transform editing')
          ist=2
          CALL CLPRED(ist)
        elseif(ic.eq.3)then
          call tstamp('>','CLM: transform editing')
          ist=3
          CALL CLPRED(ist)
        elseif(ic.eq.4)then
          goto 3
        endif

C Export weather data to ASCII file (with similar name).
      ELSEIF(INO.EQ.13)THEN
        helptopic='clm_export_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkabc(' ','Otions:','ESP-r compatible',
     &    'SQL compatible','cancel',IICT,nbhelp)
        if(IICT.eq.3) goto 3

C Prepend a .a to the current binary name and allow for browsing.
        call tstamp('>','CLM: export weather data')
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        helptopic='clm_file_browse_options'
        call gethelptext(helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  290   llt=lnblnk(LLASCI)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            ISTRW=96
          elseif(llt.ge.96.and.llt.lt.124)then
            ISTRW=124
          elseif(llt.ge.124.and.llt.le.144)then
            ISTRW=144
          endif
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(LLASCI,' ','ASCII weather file?',
     &    ISTRW,DCLIM,'ASCII weather file name',IER,nbhelp)
        IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 290

C Cast back to 96 char name.
        llt=MIN0(lnblnk(llasci),96)
        write(LASCI,'(a)') LLASCI(1:llt)

        silent=.true.
        CALL CFILIN(silent) ! Read the supplied weather file (LCMDFL). 
 
C Confirm the period to transfer.
        NTSPH=1
        CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

        write(outs,'(3a)')'Export data inclusive of ',
     &           PERST3(1:lnblnk(PERST3)),'?'
        helptopic='clm_export_inclusive'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ',outs,OK,nbhelp)
        IF(.NOT.OK)THEN
          CALL selper(ier)
        ENDIF
        IAF=IFIL+1
        if(IICT.eq.1)then
          CALL MKACLM('E',LASCI,IAF,IDS,IDF,IER)
        elseif(IICT.eq.2)then
          CALL MKACLM('S',LASCI,IAF,IDS,IDF,IER)
        endif

C ESP-r or EPW weather file?
      ELSEIF(INO.EQ.14)THEN
        helptopic='clm_esp_or_epw'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKATOG(' ','Import options:',
     &   'EPW file','ASCII ESP-r','Korean MET',
     &   'ASCII column data','cancel',' ',' ',IICT,nbhelp)
        if(IICT.eq.1)then
          NTSPH=1
          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_epw_source'
          call gethelptext(helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  291     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              ISTRW=96
            elseif(llt.ge.96.and.llt.lt.124)then
              ISTRW=124
            elseif(llt.ge.124.and.llt.le.144)then
              ISTRW=144
            endif
          elseif(iglib.eq.2)then
            ISTRW=144
          else
            ISTRW=96
          endif
          CALL EASKF(LLASCI,'EPW file?',
     &      ' ',ISTRW,DCLIM,'EPW file name',IER,nbhelp)

C If user cancelled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 291

C Cast back to 96 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),96)
          write(LASCI,'(a)') LLASCI(1:llt)
          CALL EPWTOBCLM(LASCI,IUNIT,IER)
        elseif(IICT.eq.2)then

C Import weather data from ASCII file asking user whether
C to overwrite site information. Confirm the period to transfer.
          NTSPH=1
          CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &            IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

          write(outs,'(3a)') 'Import data inclusive of ',
     &                PERST3(1:lnblnk(PERST3)),'?'
          helptopic='clm_import_inclusive'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ',outs,OK,nbhelp)
          IF(.NOT.OK)THEN
            CALL selper(ier)
          ENDIF

          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_import_ascii_esp'
          call gethelptext(helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will return only the name of the
C file, while the GTK version will return the full path.
  292     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              ISTRW=96
            elseif(llt.ge.96.and.llt.lt.124)then
              ISTRW=124
            elseif(llt.ge.124.and.llt.le.144)then
              ISTRW=144
            endif
          elseif(iglib.eq.2)then
            ISTRW=144
          else
            ISTRW=96
          endif
          CALL EASKF(LLASCI,' ','ASCII weather file?',
     &      ISTRW,DCLIM,'ASCII weather file name',IER,nbhelp)

C If user cancelled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 292

C Cast back to 96 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),96)
          write(LASCI,'(a)') LLASCI(1:llt)

          helptopic='clm_also_update_site'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Update site info?',OK,nbhelp)
          silent=.false.
          WRITE(outs248,'(2A)')'Writing data to ',LCLIM(1:lnblnk(LCLIM))
          CALL EDISP248(IUOUT,outs248,100)
          if(ok)then
            CALL MKBCLM(iuout,1,LASCI,IUNIT,IDS,IDF,silent,IER)
          else
            CALL MKBCLM(iuout,0,LASCI,IUNIT,IDS,IDF,silent,IER)
          endif
          call tstamp('>','CLM: import weather data')
          CALL EDISP(IUOUT,'Modified weather file saved')
          CALL EDISP(IUOUT,blank)
        elseif(IICT.eq.3)then

C Import weather data from Korean Met Office XXXH.dat ASCII file
C asking user which year to transfer.
          NTSPH=1
          helptopic='clm_year_is_required'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKI(IYEAR,' ','Year to import?',
     &             1900,'W',2051,'W',2007,'year',IER,nbhelp)
          CLMLOC=' '
          helptopic='clm_site_name'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(CLMLOC,' ','Weather station name:',
     &      42,'North_Pole','weather site',IER,nbhelp)

          helpinsub='common'  ! get from common.help
          helptopic='clm_latitude_match'
          call gethelptext(helpinsub,helptopic,nbhelp)
          helpinsub='clm'     ! reset for MAIN
          CALL EASKR(CLAT,' ','Weather station latitude?',
     &      -89.9,'W',89.9,'W',30.0,'weather latitude',IER,nbhelp)

          CALL EASKR(CLONG,' ',
     &      'Weather station longitude diference?',
     &      -14.9,'W',14.9,'W',0.0,'weather long',IER,nbhelp)

          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_korea_met_file_name'
          call gethelptext(helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  293     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              ISTRW=96
            elseif(llt.ge.96.and.llt.lt.124)then
              ISTRW=124
            elseif(llt.ge.124.and.llt.le.144)then
              ISTRW=144
            endif
          elseif(iglib.eq.2)then
            ISTRW=144
          else
            ISTRW=96
          endif
          CALL EASKF(LLASCI,' ','Korean weather file?',
     &      ISTRW,DCLIM,'Korean weather file name',IER,nbhelp)

C If user cancelled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 293

C Cast back to 96 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),96)
          write(LASCI,'(a)') LLASCI(1:llt)

C Ask if older or newer file format.
          helptopic='korea_met_file_old_new'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' ','Korean weather file format options:',
     &     'old','new','cancel',ICT,nbhelp)
          if(ICT.eq.1)then
            call MKOREANCLM(LASCI,IUNIT,'old',IER)
          elseif(ICT.eq.2)then
            call MKOREANCLM(LASCI,IUNIT,'new',IER)
          endif

        elseif(IICT.eq.4)then

C Import weather data from ASCII file asking user whether
C to overwrite site information. Confirm the period to transfer.
          NTSPH=1
          CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &            IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

          write(outs,'(3a)') 'Import column data inclusive of ',
     &            PERST3(1:lnblnk(PERST3)),'?'
          helptopic='clm_import_inclusive'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ',outs,OK,nbhelp)
          IF(.NOT.OK)THEN
            CALL selper(ier)
          ENDIF

          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_import_ascii_esp'
          call gethelptext(helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will return only the name of the
C file, while the GTK version will return the full path.
  294     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              ISTRW=96
            elseif(llt.ge.96.and.llt.lt.124)then
              ISTRW=124
            elseif(llt.ge.124.and.llt.le.144)then
              ISTRW=144
            endif
          elseif(iglib.eq.2)then
            ISTRW=144
          else
            ISTRW=96
          endif
          CALL EASKF(LLASCI,' ','ASCII weather file?',
     &      ISTRW,DCLIM,'ASCII weather file name',IER,nbhelp)

C If user cancelled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 294

C Cast back to 96 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),96)
          write(LASCI,'(a)') LLASCI(1:llt)

          WRITE(outs248,'(2A)')'Writing data to ',LCLIM(1:lnblnk(LCLIM))
          CALL EDISP248(IUOUT,outs248,100)
          CALL COLTOBCLM(iuout,LASCI,IUNIT,IDS,IDF,IER)
          call tstamp('>','CLM: import column data')
          CALL EDISP(IUOUT,'Modified weather file saved')
          CALL EDISP(IUOUT,blank)
        endif

C Manage weather list.
      ELSEIF(INO.EQ.15)THEN
        call editemclimatelist()

C Summary of current weather.
      ELSEIF(INO.EQ.16)THEN
        minu = CHAR(39)
        second = CHAR(34)
        ILONG=INT(CLONG)                 ! integer portion of degrees
        FRAM=abs(clong)-abs(real(ilong)) ! fraction portion of degrees
        FRAD2=FRAM*60                    ! into minutes
        ILONGM=INT(FRAD2)                ! integer part of minutes
        FRAS=abs(FRAD2)-abs(real(ilongm))! fraction portion of minutes
        FRAS2=FRAS*60                    ! seconds
        if(clong.gt.0.0)then
          write(longdms,'(i2,a,i2,a,f4.1,a)')
     &    ILONG,'deg ',ILONGM,minu,FRAS2,second
        else
          write(longdms,'(i3,a,i2,a,f4.1,a)')
     &    ILONG,'deg',ILONGM,minu,FRAS2,second
        endif

C Get latitude in degrees minutes and seconds.
        ILAT=INT(CLAT)                  ! integer portion of degrees
        FRAM=abs(clat)-abs(real(ilat))  ! fraction portion of degrees
        FRAD2=FRAM*60                   ! into minutes
        ILATM=INT(FRAD2)                ! integer part of minutes
        FRAS=abs(FRAD2)-abs(real(ilatm))! fraction portion of minutes
        FRAS2=FRAS*60                   ! seconds
        if(clat.ge.0.0)then
          write(latdms,'(i2,a,i2,a,f4.1,a)')
     &    ILAT,'deg ',ILATM,minu,FRAS2,second
        else
          write(latdms,'(i3,a,i2,a,f4.1,a)')
     &    ILAT,'deg',ILATM,minu,FRAS2,second
        endif
        lnclmloc=lnblnk(clmloc)
        IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
          WRITE(outs,102)CLMLOC(1:lnclmloc),
     &      CLAT,latdms,CLONG,longdms,IYEAR
 102      FORMAT(1X,A,':',F6.2,' (',a,')N ',F6.2,' (',a,')W :',I5)
        elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
          WRITE(outs,103)CLMLOC(1:lnclmloc),
     &      CLAT,latdms,CLONG,longdms,IYEAR
 103      FORMAT(1X,A,':',F6.2,' (',a,')S ',F6.2,' (',a,')E :',I5)
        elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
          WRITE(outs,104)CLMLOC(1:lnclmloc),
     &      CLAT,latdms,CLONG,longdms,IYEAR
 104      FORMAT(1X,A,':',F6.2,' (',a,')S ',F6.2,' (',a,')W :',I5)
        else
          WRITE(outs,101)CLMLOC(1:lnclmloc),
     &      CLAT,latdms,CLONG,longdms,IYEAR
 101      FORMAT(1X,A,':',F6.2,' (',a,')N ',F6.2,' (',a,')E :',I5)
        endif
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        NTSPH=1
        CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,  ! confirm period
     &    IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        write(outs,'(1X,A44)')PERST2
        call edisp(iuout,outs)

C Preferences.
      ELSEIF(INO.EQ.17)THEN
        CALL SETUP(IUOUT,IER)

C Toggle trace level.
      ELSEIF(INO.EQ.18)THEN
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0

C Feedback channel. Suggest an initial name for the export file.
      ELSEIF(INO.EQ.19)THEN
        if(ixopen.eq.0)then
          llt=lnblnk(LCLIM)
          if(llt.lt.130)then
            write(xfile,'(2a)') LCLIM(1:llt),'.report'
          else
            write(xfile,'(2a)') LCLIM(1:130),'.report'
          endif
        endif
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T','Data',IER)
        if(ixopen.eq.1)then
          call usrmsg(
     &      'Any text based reports you request will now be sent to',
     &      'the export file. Return here and toggle off when done.',
     &      'W')
        endif

C Delimiter.
      ELSEIF(INO.EQ.20)THEN
        helptopic='clm_delimiter_choice'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IWM=1
        CALL EASKATOG(' ','Column delimeter:',
     &    'normal spaces','single space','tab','comma','cancel',' ',
     &    ' ',IWM,nbhelp)
        if(iwm.eq.1)then
          delim = '-'
        elseif(iwm.eq.2)then
          delim = 'S'
        elseif(iwm.eq.3)then
          delim = 'T'
        elseif(iwm.eq.4)then
          delim = 'C'
        endif

C Help requested.
      ELSEIF(INO.EQ.21)THEN
        helptopic='weather_analysis_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('analysis menu',nbhelp,'-',0,0,IER)
      ELSE
        INO=-1
        goto 3
      endif
      goto 3

 901  call edisp(iuout,'Error opening journal file ... continuing.')
      goto 902
 903  call edisp(iuout,'Error opening session log ... continuing.')
      goto 904

      END

C ********* CLMTOT
C Displays in tabular format the weather parameters for each
C hour in a specified day.

      SUBROUTINE CLMTOT
#include "climate.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT
      common/appw/iappw,iappx,iappy
      COMMON/RADTYP/IDNGH
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/exporttg/xfile,tg,delim
      common/exporttgi/ixopen,ixloc,ixunit

      character outs*124,outs2*124,xfile*144,tg*1,delim*1
      character blank*2  ! for edisp of blank line
      logical ok

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

      helpinsub='clm'  ! set for this subroutine

C If in type 8 terminal make the text display area larger before
C displaying the following data.
      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          if(iappw.gt.0.and.iappw.lt.100)then
            menuchw = MAX0(int(28*iappw*0.01),12)
            LIMTTY= MAX0(int(30*iappw*0.01),24)
            LIMIT = MAX0(int(30*iappw*0.01),24)
          else
            menuchw = 28
            LIMTTY=30
            LIMIT =30
          endif

C Setup and pass in parameters to win3d.
          iiw1=10; iiw2=22; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          ilf=2; ild=LIMTTY
          call winclr
          CALL feedbox(iimenu,ilf,igfw,igfh)
          CALL opengdisp(iimenu,ild,ilf,igdw,igdh)
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call opencpw
          call opensetup
       elseif(iglib.eq.2)then
          continue
        endif
      ENDIF
      blank='  '

C Determine day to be displayed.
    5 call edisp(iuout,blank)
      call edisp(iuout,' Day to be displayed...')
      IDD=ID1
      IMM=IM1
      IYD=IDS
      call oneday(IFDAY,IYD,IDD,IMM,IER)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file.',' ','-')
      endif

C Read values of the weather parameters for this day.
      CALL CLMGET(IYD,IER)

C Output all values in tabular form.
      CALL CSITEH
      WRITE(outs,6)IDD,IMM
    6 FORMAT('           Period: Day',I3,' of month',I3)
      call edisp(itru,outs)
      call edisp(itru,blank)
      write(outs,'(A)') ' Hr '
      write(outs2,'(A)')'    '
      k=4
      do i=1,MCM
        if (CMXST(i)) then
          write(outs ,'(2A,1X)')outs(1:k) ,CMNAMA(i)
          write(outs2,'(2A,1X)')outs2(1:k),CMUNIT(i)
          k=k+7
        endif
      enddo
      call edisp(itru,outs)
      call edisp(itru,outs2)
      call edisp(itru,blank)

      DO 10 J=1,24
        write(outs,'(I3,1X)')J
        k=4
        do i=1,MCM
          if (CMXST(i)) then
            if (i.eq.1 .or. i.eq.5) then
              write(outs,'(A,F6.1,1X)')outs(1:k),CMRVAL(i,J)
            else
              write(outs,'(A,I6,1X)')outs(1:k),int(CMRVAL(i,J))
            endif
            k=k+7
          endif
        enddo
        call eddisp(itru,outs)
   10 CONTINUE

C Consider another day?
      helptopic='confirm_another_period'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Continue with another period?',OK,nbhelp)
      IF(OK)GOTO 5

      RETURN
      END


C ********************* SETUP
C SETUP provides the menus and control logic for the preferences menu.
      SUBROUTINE SETUP(ITRU,IER)

#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      DIMENSION SETUPM(6)
      CHARACTER SETUPM*36
      integer nitms,ISETUP  ! max items and current menu item

C Present the preferences menu. ISETUP is the menu index returned.
      helpinsub='clm'  ! set for this subroutine
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

      SETUPM(3)=  ' ________________________________   '

      SETUPM(4)=  '? help                              '
      SETUPM(5)=  '- exit menu                         '
      nitms=5
      CALL EMENU('clm preferences',SETUPM,nitms,ISETUP)

      IF(ISETUP.EQ.1)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0
      ELSEIF(ISETUP.EQ.2)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY.
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0
      ELSEIF(ISETUP.EQ.4)THEN

C Produce help text for the  menu.
        helptopic='clm_setup_help'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('clm preferences',nbhelp,'-',0,0,IER)
      ELSEIF(ISETUP.EQ.5)THEN

C Return to main menu.
        RETURN
      ELSE

C Not one of the legal menu choices.
        ISETUP=-1
        goto 2
      ENDIF
      ISETUP=-4
      goto 2

      END

C ******** clrclm
C Clear common blocks for a new weather file.

      SUBROUTINE clrclm

      CHARACTER CLMLOC*42
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW


      clmloc='new site'
      ID1=1
      IM1=1
      IT1=1
      ID2=31
      IM2=12
      IT2=24
      IDS=1
      IDF=365
      IYEAR=2000
      ICYEAR=2000
      CLAT=50.
      CLONG=0.

      return
      end

C ***** dummy routines for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine redraw(ier)
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

C Passed parameters.
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

C A fortran implementation of the c surboutine dinterval.
C Copy of std version in esru_misc.F
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
