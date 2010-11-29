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

C Clm.f is the main controlling code of the climate display and 
C analysis facility of the ESRU simulation suite of programs.
C Clm allows the creation, modification and analysis of
C climatic data sets compatible with ESP-r.

      program clm
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "espriou.h"
      
#include "seasons.h"
C seasons.h provides typper and typsea
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
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
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF

C Indicator of possible command line action to take and the
C file associated with this action.
      common/ract/paction,pascif

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Redirected text/graphics parameters.
      common/exporttg/xfile,tg,delim
      common/exporttgi/ixopen,ixloc,ixunit

C Climate list common.
      character clmlnam*32,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12

      dimension ITEM(22)
      character CLMLOC*30
      character ITEM*26,LASCI*72,outs*124,outs248*248
      character PERST1*14,PERST2*40,PERST3*40,LCMDFL*144
      character inf*144,ascif*72,pascif*72,llasci*144
      character pact*16,aut*12,paction*16
      character path*72,xfile*144,tg*1,delim*1
      character tab*1,fs*1,OUTSTR*124
      character journcmd*20,jfile*72,uname*24,cjfile*72
      character cfgroot*24,LCFGF*72
      character blank*2   ! string for edisp of blank line
      LOGICAL OK,DOK,unixok,there,silent

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ic,iwm  ! for radio buttons
      integer NITEM,INO  ! max items and current menu item

C For help messages
      character helpinapp*24   ! application name
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Initial assumptions. Pass module name to C code structure.
      call ezero
      call curmodule('clm ')
      cAppName = 'clm'
      helpinapp='clm'; helpinsub='clm'  ! set for MAIN
      tab = CHAR(9)
      ITRC=1
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
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
      IDS=1
      IDF=365
      KDS=1
      KDF=24
      IYEAR=2000
      ICYEAR=2000
      cfgroot='  '
      LCFGF='  '

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

C Set default climatelist text entries
      clmlnam = 'not yet entered'
      clmlaid = 'not yet entered'
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
        iappw=570
        iappx=80
        iappy=80
      else
        if(iappx.le.0)iappx=80
        if(iappy.le.0)iappy=80
        if(iappw.le.200)then
          iappwi=int(570*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

C Set initial menu width and text feedback number of lines.
      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(29*iappw*0.01),18)
        LIMTTY= MAX0(int(8*iappw*0.01),4)
        LIMIT = MAX0(int(8*iappw*0.01),4)
      else
        menuchw = 29
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
     &  'ESP-r Climate Analysis: enquiries to esru@strath.ac.uk')
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          CALL win3d(menuchw,10,22,4,3,igl,igr,igt,igb,igw,igwh)
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

      blank='  '
      call edisp(IUOUT,blank)
      CALL ESPrVersion("summary",cAppName,IUOUT)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow, Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,blank)


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

        write(outs,'(a,a)')'Journal (climate analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Scan the defaults file for default configuration.
      call escdef(IAF,IER)

C Assume climate file is on IFIL.
      ICLIM=IFIL
      LCLIM=DCLIM
      INIT =0

C Take command line file name to LCMDF and use as initial climate file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        LCMDFL=' '
      else
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        if(itrc.gt.1)call edisp248(iuout,outs248,80)
      endif

C Take command line action parameter:
C If paction = 'asci2bin' then fill binary file `inf` with data 
C from `ascif` and exit.
C If paction is `bin2asci` file ascii file `ascif` with data from
C the binary file `inf` and exit.
C If paction = `epw2bin` take user to point of interaction for dealing
C with this conversion.
      if(pact(1:1).ne.' ')then
        paction=pact
        write(outs,'(a,a)')' the action is: ',paction
        if(itrc.gt.1)call edisp(iuout,outs)
        write(outs,'(a,a)')'clm: the input action is: ',paction
        call tstamp('>',outs)
        if(paction(1:8).eq.'asci2bin')then
          silent=.true.
          pascif=ascif

C Read in the supplied climate file name (LCMDFL).
          CALL CFILIN(silent)

C Import climate data from ASCII file.
          NTSPH=1
          CALL PERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &            IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
          IUNIT=IFIL+1
          CALL MKBCLM(iuout,1,pascif,IUNIT,IDS,IDF,silent,IER)
          call tstamp('>','Finished climate module conversion ASCI2BIN')
          write(outs248,'(5a)')'Climate_conversion',tab,
     &      pascif(1:lnblnk(pascif)),tab,LCMDFL(1:lnblnk(LCMDFL))

C Debug.
C          write(6,*)outs248(1:lnblnk(outs248))

          CALL EPAGEND
          STOP
        elseif(paction(1:8).eq.'bin2asci')then
          silent=.true.
          pascif=ascif

C Read in the supplied climate file (LCMDFL).
          CALL CFILIN(silent)

C Write out this data to an asci file.
          NTSPH=1
          CALL PERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
          IAF=IFIL+1
          CALL MKACLM('E',pascif,IAF,IDS,IDF,IER)
          call tstamp('>','Finished climate module conversion BIN2ASCI')
          write(outs248,'(5a)')'Climate_conversion',tab,
     &      LCMDFL(1:lnblnk(LCMDFL)),tab,pascif(1:lnblnk(pascif))

C Debug,
C          write(6,*)outs248(1:lnblnk(outs248))

          CALL EPAGEND
          STOP
        elseif(paction(1:7).eq.'epw2bin')then
          paction='INTERACTIVE'
          silent=.true.
          pascif=ascif

C Read in the climate file (LCMDFL)
          CALL CFILIN(silent)

C Import climate data from EPW file.
          NTSPH=1
          IUNIT=IFIL+1
          CALL EPWTOBCLM(pascif,IUNIT,IER)
          call tstamp('>','Finished climate module conversion EPW2BIN')
          write(outs248,'(5a)')'Climate_conversion',tab,
     &      pascif(1:lnblnk(pascif)),tab,LCMDFL(1:lnblnk(LCMDFL))

C Debug.
C          write(6,*)outs248(1:lnblnk(outs248))

          CALL EPAGEND
          STOP
        else

C In interactive mode so do splash-screen and then read
C in the climate file name via list
C selection, editing or accepting the LCMDFL.
          call edisp(IUOUT,blank)
          CALL ESPrVersion("summary",cAppName,IUOUT)
          call edisp(IUOUT,blank)
          paction='INTERACTIVE'
          silent=.false.
          CALL CFILIN(silent)
        endif
      else

C In interactive mode so do splash-screen and then read
C in the climate file name via list
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
      write(ITEM(1),'(2A)')  'a climate: ',LCLIM(1:15)
 
C Derive string for focus period.
      NTSPH=1
      CALL PERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(ITEM(2),'(A,A24)')'b ',PERST3(9:32)
      ITEM(3)=                '  _______________________ '
      ITEM(4)=                'c synoptic analysis       '
      ITEM(5)=                'd graphical analysis      '
      ITEM(6)=                'e psychrometric analysis  '
      ITEM(7)=                'f table (one day) analysis'
      ITEM(8)=                '  _______________________ '
      if(IDNGH.eq.0)then
        ITEM(9)=              'g radiation >> dir normal '
      elseif(IDNGH.eq.123)then
        ITEM(9)=              'g radiation >> global hor '
      endif
      ITEM(10)=               '  _______________________ '
      ITEM(11)=               'h edit site data          '
      ITEM(12)=               'i edit climate data       '
      ITEM(13)=               'j export to text file     '
      ITEM(14)=               'k import from text file   '
      ITEM(15)=               'l manage climatelist      '
      ITEM(16)=               '  _______________________ '
      ITEM(17)=               's preferences             '
      IF(ITRC.EQ.0)THEN
        ITEM(18)=             'r reporting >> silent     '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(18)=             'r reporting >> summary    '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(18)=             'r reporting >> detailed   '
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
      ITEM(22)=               '- end                     '

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('clm ',1)

C Present the opening display and control menu.
      NITEM=22
      INO=-3
      CALL EMENU('Climate Analysis',ITEM,NITEM,INO)
      IF(INO.EQ.NITEM)THEN
        call usrmsg(' Closing climate module...',' ','P')
        write(outs248,'(3a)')'Climate_selected',tab,
     &    LCMDFL(1:lnblnk(LCMDFL))

C Debug.
C        write(6,*)outs248(1:lnblnk(outs248))

        call tstamp('>','Finished climate module')
        CALL EPAGEND
        STOP
      ELSEIF(INO.EQ.1)THEN
        helptopic='select_db_opt'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        CALL EASKABCD('Climate database options:',' ',
     &   'new db','use current db','select db','cancel',ICT,nbhelp)
        if(ICT.eq.1)then

C Confirm that this is a new file and clear memory if necessary.
          dok=.true.
          helptopic='ok_to_clear_data'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL ASKOK('As part of changing climate file do you wish',
     &      'to clear any current data held in memory?',OK,dok,2)
          CALL ASKOK('As part of changing climate file do you wish',
     &      'to clear any current data held in memory?',OK,dok,nbhelp)
          if(OK)then
            call clrclm
            LCMDFL='UNKNOWN'
            call tstamp('>','CLM: cleared climate data')
          endif
          silent=.false.
          CALL CFILIN(silent)
        elseif(ICT.eq.2)then
          call tstamp('>','CLM: use current climate data')
          write(LCMDFL,'(a)') LCLIM(1:lnblnk(lclim))
          silent=.false.
          CALL CFILIN(silent)
        elseif(ICT.eq.3)then
          call tstamp('>','CLM: select climate data')
          LCMDFL='UNKNOWN'
          silent=.false.
          CALL CFILIN(silent)
        endif
      ELSEIF(INO.EQ.2)THEN

C Period definition.
        call selper(IER)
        INIT=1
        INEW=1
      ELSEIF(INO.EQ.4)THEN

C Synoptic analysis.
        IF(INIT.EQ.0)CALL selper(IER)
        INIT=1
        call tstamp('>','CLM: enter synoptic analysis')
        CALL CLMSYN
        call tstamp('>','CLM: exit synoptic analysis')
      ELSEIF(INO.EQ.5)THEN

C Graphic analysis. Enlarge graphics area if small.
        IF(INIT.EQ.0)CALL selper(ier)
        INIT=1
        if(MMOD.EQ.8)then
          if(LIMTTY.gt.12)then
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1)then
              if(iappw.gt.0.and.iappw.lt.100)then
                menuchw = MAX0(int(29*iappw*0.01),10)
                LIMTTY= MAX0(int(6*iappw*0.01),4)
                LIMIT = MAX0(int(6*iappw*0.01),4)
              else
                menuchw = 29
                LIMTTY=6
                LIMIT =6
              endif
              call winclr
              CALL feedbox(menuchw,2,igfw,igfh)
              CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
              CALL win3d(menuchw,10,22,7,3,igl,igr,igt,igb,igw,igwh)
              igw=igr-igl
              igwh=igb-igt
            elseif(iglib.eq.2)then
              continue
            endif
          ENDIF
          call tstamp('>','CLM: enter graphic analysis')
          CALL CLMGRF
          call tstamp('>','CLM: exit graphic analysis')
        else
          call edisp(iuout,' Not in graphic window !')
        endif
      ELSEIF(INO.EQ.6)THEN

C Psychrometric analysis. Enlarge graphics area if small.
        IF(INIT.EQ.0)CALL selper(ier)
        INIT=1
        if(MMOD.EQ.8)then
          if(LIMTTY.gt.12)then
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1)then
              if(iappw.gt.0.and.iappw.lt.100)then
                menuchw = MAX0(int(29*iappw*0.01),10)
                LIMTTY= MAX0(int(6*iappw*0.01),4)
                LIMIT = MAX0(int(6*iappw*0.01),4)
              else
                menuchw = 29
                LIMTTY=6	
                LIMIT =6
              endif
              call winclr
              CALL feedbox(menuchw,2,igfw,igfh)
              CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
              CALL win3d(menuchw,10,22,7,3,igl,igr,igt,igb,igw,igwh)
              igw=igr-igl
              igwh=igb-igt
            elseif(iglib.eq.2)then
              continue
            endif
          ENDIF
          call tstamp('>','CLM: enter psychrometric analysis')
          CALL PSYCHART
          call tstamp('>','CLM: exit psychrometric analysis')
        else
          call edisp(iuout,' Not in graphic window !')
        endif
      ELSEIF(INO.EQ.7)THEN

C Tabular analysis.
        IF(INIT.EQ.0)CALL selper(ier)
        INIT=1
        call tstamp('>','CLM: enter tabular analysis')
        CALL CLMTOT
        call tstamp('>','CLM: exit tabular analysis')
      ELSEIF(INO.EQ.9)THEN

C Change radiation type.
        helptopic='rad_change_password'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        IANS=0
        CALL EASKI(IANS,' ',' Password ? ',
     &    0,'-',0,'-',0,'passowrd',IER,nbhelp)
        IF(IANS.NE.101)then
          call usrmsg(' ',' Password not accepted! ','-')
          goto 3
        endif

        helptopic='rad_type_choice'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        CALL EASKAB(' Solar radiation type in file: ',' ',
     &    'direct normal','global horizontal',IRADT,nbhelp)
        IRTYP=0
        IF(IRADT.EQ.2)IRTYP=123
        IDNGH=IRTYP
        ICDNGH=IRTYP
        IREC=366
        READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR
        WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IRTYP
        call tstamp('>','CLM: change radiation type')
        goto 3
 1000   call edisp(iuout,' Data file error! ')
        goto 3
      ELSEIF(INO.EQ.11)THEN

C Edit site attributes.
        call sitedit
      ELSEIF(INO.EQ.12)THEN

C Creation of climate file from manual editing.
        helptopic='clm_data_creation_opt'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        IC=1
        CALL EASKATOG(' Creation of data via: ',' ',
     &    'keyboard entry','prediction','transforms','curve fitting',
     &    'continue',' ',' ',IC,nbhelp)

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
      ELSEIF(INO.EQ.13)THEN

C Export climate data to ASCII file (with similar name).
        helptopic='clm_export_options'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        call easkabc(' Climate export options:',' ','ASCII esp-r file',
     &    'SQL tab separated','continue',IICT,nbhelp)
        if(IICT.eq.3) goto 3

C Prepend a .a to the current binary name and allow for browsing.
        call tstamp('>','CLM: export climate data')
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        helptopic='clm_file_browse_options'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  290   llt=lnblnk(LLASCI)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(LLASCI,' Climate file (esp-r ascii format)?',
     &        ' ',96,DCLIM,'ASCII climate file name',IER,nbhelp)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(LLASCI,' Climate file (esp-r ascii format)?',
     &        ' ',124,DCLIM,'climate file name',IER,nbhelp)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(LLASCI,' Climate file (esp-r ascii format)?',
     &        ' ',144,DCLIM,'ASCII climate file name',IER,nbhelp)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' Climate file (esp-r ascii format)?',
     &      ' ',144,DCLIM,'ASCII climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,' Climate file (esp-r ascii format)?',
     &      ' ',96,DCLIM,'ASCII climate file name',IER,nbhelp)
        endif
        IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 290

C Cast back to 72 char name.
        llt=MIN0(lnblnk(llasci),72)
        write(LASCI,'(a)') LLASCI(1:llt)
 
C Confirm the period to transfer.
        NTSPH=1
        CALL PERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

        write(outs,'(A,A)') PERST3(1:lnblnk(PERST3)),' ?'
        dok=.true.
        helptopic='clm_export_inclusive'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        CALL ASKOK(' Export the data inclusive of',outs,OK,dok,nbhelp)
        IF(.NOT.OK)THEN
          CALL selper(ier)
        ENDIF
        IAF=IFIL+1
        if(IICT.eq.1)then
          CALL MKACLM('E',LASCI,IAF,IDS,IDF,IER)
        elseif(IICT.eq.2)then
          CALL MKACLM('S',LASCI,IAF,IDS,IDF,IER)
        endif
      ELSEIF(INO.EQ.14)THEN

C Check whether it is an esp-r climate file or an EPW file.
        helptopic='clm_esp_or_epw'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' Climate import options:',' ',
     &   'EPW climate file','ASCII esp-r file','Korean MET',
     &   'continue',IICT,nbhelp)
        if(IICT.eq.1)then
          NTSPH=1
          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_epw_source'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  291     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(LLASCI,' Climate EPW file?',
     &          ' ',96,DCLIM,'EPW climate file name',IER,nbhelp)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(LLASCI,' Climate EPW file?',
     &          ' ',124,DCLIM,'EPW climate file name',IER,nbhelp)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(LLASCI,' Climate EPW file?',
     &          ' ',144,DCLIM,'EPW climate file name',IER,nbhelp)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(LLASCI,' Climate EPW file?',
     &        ' ',144,DCLIM,'EPW climate file name',IER,nbhelp)
          else
            CALL EASKF(LLASCI,' Climate EPW file?',
     &        ' ',96,DCLIM,'EPWclimate file name',IER,nbhelp)
          endif

C If user canceled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 291

C Cast back to 72 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),72)
          write(LASCI,'(a)') LLASCI(1:llt)
          CALL EPWTOBCLM(LASCI,IUNIT,IER)
        elseif(IICT.eq.2)then

C Import climate data from ASCII file asking user whether
C to overwrite site information. Confirm the period to transfer.
          NTSPH=1
          CALL PERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &            IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

          write(outs,'(2A)') PERST3(1:lnblnk(PERST3)),' ?'
          dok=.true.
          helptopic='clm_import_inclusive'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL ASKOK('Import the data inclusive of',outs,OK,dok,nbhelp)
          IF(.NOT.OK)THEN
            CALL selper(ier)
          ENDIF

          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_import_ascii_esp'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  292     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(LLASCI,' Climate ASCII file?',
     &          ' ',96,DCLIM,'ASCII climate file name',IER,nbhelp)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(LLASCI,' Climate ASCII file?',
     &          ' ',124,DCLIM,'ASCII climate file name',IER,nbhelp)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(LLASCI,' Climate ASCII file?',
     &          ' ',144,DCLIM,'ASCII climate file name',IER,nbhelp)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(LLASCI,' Climate ASCII file?',
     &        ' ',144,DCLIM,'ASCII climate file name',IER,nbhelp)
          else
            CALL EASKF(LLASCI,' Climate ASCII file?',
     &        ' ',96,DCLIM,'ASCII climate file name',IER,nbhelp)
          endif

C If user canceled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 292

C Cast back to 72 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),72)
          write(LASCI,'(a)') LLASCI(1:llt)

          dok=.true.
          helptopic='clm_also_update_site'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL ASKOK(' ','Update the site info as well?',OK,dok,nbhelp)
          silent=.false.
          WRITE(outs248,'(2A)')'Writing data to ',LCLIM(1:lnblnk(LCLIM))
          CALL EDISP248(IUOUT,outs248,100)
          if(ok)then
            CALL MKBCLM(iuout,1,LASCI,IUNIT,IDS,IDF,silent,IER)
          else
            CALL MKBCLM(iuout,0,LASCI,IUNIT,IDS,IDF,silent,IER)
          endif
          call tstamp('>','CLM: import climate data')
          CALL EDISP(IUOUT,'Modified climate file saved')
          CALL EDISP(IUOUT,blank)
        elseif(IICT.eq.3)then

C Import climate data from Korean Met Office XXXH.dat ASCII file
C asking user which year to transfer.
          NTSPH=1
          helptopic='clm_year_is_required'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL EASKI(IYEAR,' ','Which year to import? ',
     &             1900,'W',2020,'W',2000,'year',IER,nbhelp)
          CLMLOC=' '
          helptopic='clm_site_name'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL EASKS(CLMLOC,' ','Climate site name ? ',
     &      30,'North_Pole','climate site. ',IER,nbhelp)

          helptopic='clm_latitude_match'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL EASKR(CLAT,' ','Climate latitude ? ',
     &      -89.9,'W',89.9,'W',30.0,'climate latitude',IER,nbhelp)

          CALL EASKR(CLONG,' ',
     &      'Longitude diference from local time meridian?',
     &      -14.9,'W',14.9,'W',0.0,'climate long',IER,nbhelp)

          LASCI=' '
          LLASCI=' '
          IUNIT=IFIL+1
          helptopic='clm_korea_met_file_name'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  293     llt=lnblnk(LLASCI)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(LLASCI,' Korean XXXH.dat file?',
     &          ' ',96,DCLIM,'Korean climate file name',IER,nbhelp)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(LLASCI,' Korean XXXH.dat file?',
     &          ' ',124,DCLIM,'Korean climate file name',IER,nbhelp)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(LLASCI,' Korean XXXH.dat file?',
     &          ' ',144,DCLIM,'Korean climate file name',IER,nbhelp)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(LLASCI,' Korean XXXH.dat file?',
     &        ' ',144,DCLIM,'Korean climate file name',IER,nbhelp)
          else
            CALL EASKF(LLASCI,' Korean XXXH.dat file?',
     &        ' ',96,DCLIM,'Korean climate file name',IER,nbhelp)
          endif

C If user canceled loop show menu again.
          if(ier.eq.-3) goto 3

          IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 293

C Cast back to 72 char name and do the conversion.
          llt=MIN0(lnblnk(llasci),72)
          write(LASCI,'(a)') LLASCI(1:llt)

C Ask if older or newer file format.
          helptopic='korea_met_file_old_new'
          call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
          CALL EASKABC('Korean met office file age options:',' ',
     &     'older file','newer file','cancel',ICT,nbhelp)
          if(ICT.eq.1)then
            call MKOREANCLM(LASCI,IUNIT,'old',IER)
          elseif(ICT.eq.2)then
            call MKOREANCLM(LASCI,IUNIT,'new',IER)
          endif
        endif
      ELSEIF(INO.EQ.15)THEN

C Manage climate list.
        call editemclimatelist()
      ELSEIF(INO.EQ.17)THEN

C Preferences.
        CALL SETUP(IUOUT,IER)
      ELSEIF(INO.EQ.18)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO.EQ.19)THEN

C Feedback channel. Suggest an initial name for the export file.
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
      ELSEIF(INO.EQ.20)THEN

C Delimiter.
        helptopic='clm_delimiter_choice'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        IWM=1
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','continue',' ',
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
      ELSEIF(INO.EQ.21)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('clm ',0)

C Get additional text to dislay.
        helptopic='clm_additional_notes'
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
        CALL PHELPD('climate import',nbhelp,'climate_import',0,0,IER)
      ELSE
        INO=-1
        goto 3
      endif
      goto 3

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 3

      END

C ********* CLMTOT
C 'CLMTOT' displays in tabular format the climatic parameters for each
C hour in any specified day.

      SUBROUTINE CLMTOT
      PARAMETER (MT=24)
      COMMON/OUTIN/IUOUT,IUIN
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

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      character outs*124,xfile*144,tg*1,delim*1
      character blank*2  ! for edisp of blank line
      logical ok,dok

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C For help messages
      character helpinapp*24   ! application name
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

      helpinapp='clm'; helpinsub='CLMTOT'  ! set for this subroutine

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
          call winclr
          CALL feedbox(menuchw,2,igfw,igfh)
          CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
          CALL win3d(menuchw,10,22,5,3,igl,igr,igt,igb,igw,igwh)
          igw=igr-igl
          igwh=igb-igt
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
        call usrmsg(' Output being directed to file...',' ','-')
      endif

C Read values of the climatic parameters for this day.
      CALL CLMGET(IYD)

C Output all values in tabular form.
      CALL CSITEH
      IF(IDNGH.EQ.0)THEN
         WRITE(outs,6)IDD,IMM
    6    FORMAT('           Period: Day',I3,' of month',I3)
         call edisp(itru,outs)
         call edisp(itru,blank)
         WRITE(outs,61)
   61    FORMAT(' Hr d.b. temp. dr.n. rad. df.h. rad.',
     &          ' wd. vel. wd. dir. rel. h.')
         call edisp(itru,outs)
         WRITE(outs,62)
   62    FORMAT(6X,'deg.C',6X,'W/m2 ',6X,'W/m2 ',6X,'m/s',
     &          4X,'deg.f.n.',3X,'pct')
         call edisp(itru,outs)
         call edisp(itru,blank)
      ELSEIF(IDNGH.EQ.123)THEN
         WRITE(outs,63)IDD,IMM
   63    FORMAT('           Period: Day',I3,' of month',I3)
         call edisp(itru,outs)
         WRITE(outs,64)
   64    FORMAT(' Hr d.b. temp. Gl.h. rad. df.h. rad.',
     &          ' wd. vel. wd. dir. rel. h.')
         call edisp(itru,outs)
         WRITE(outs,65)
   65    FORMAT(6X,'deg.C',6X,'w/m2 ',6X,'w/m2 ',6X,'m/s',
     &          4X,'deg.f.n.',3X,'pct')
         call edisp(itru,outs)
         call edisp(itru,blank)
      ENDIF
      DO 10 I=1,24
      C1=CDIF(I)
      C2=CTMP(I)
      C3=CDNR(I)
      C4=CVEL(I)
      C5=CDIR(I)
      C6=CHUM(I)
      WRITE(outs,7)I,C2,C3,C1,C4,C5,C6
    7 FORMAT(I3,F11.2,2F11.0,F9.2,F9.0,F8.2)
      call eddisp(itru,outs)
   10 CONTINUE

C Consider another day?
      helptopic='confirm_another_period'
      call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
      dok=.true.
      CALL ASKOK(' ','Continue with another period?',OK,dok,nbhelp)
      IF(OK)GOTO 5

      RETURN
      END


C ********************* SETUP
C SETUP provides the menus and control logic for the preferences menu.
      SUBROUTINE SETUP(ITRU,IER)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      DIMENSION SETUPM(6)
      CHARACTER SETUPM*36
      integer nitms,ISETUP  ! max items and current menu item

C For help messages
      character helpinapp*24   ! application name
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

C Present the preferences menu. ISETUP is the menu index returned.
      helpinapp='clm'; helpinsub='SETUP'  ! set for this subroutine
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

      SETUPM(3)=  ' ________________________________   '

      SETUPM(4)=  '? Help                              '
      SETUPM(5)=  '- Exit to main menu                 '
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
        call gethelptext(helpinapp,helpinsub,helptopic,nbhelp)
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
C Clear common blocks for a new climate file.
      SUBROUTINE clrclm

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW

      CHARACTER CLMLOC*30

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


C ************* PERSTR (compact variant of lib/esru_cut_lib.f EPERSTR
C PERSTR creates three strings representing the start and stop
C time of a diary period based on the preferred time & date
C display format.
C IFDAY  0 gives 'DOY 10', 1 gives '10 Jan', 2 gives 'Fri 10 Jan'
C IFTIME 0 gives '10h30', 1 gives '10.50', 2 gives '0.4375'
C PERST1 (14 char) is:' 10h00  15h30 ',' 10.00  15.50 ',' 0.4375 0.6458'
C PERST3 (44 char):
C if IFDAY=0 then it is: 'period: DOY 100 - DOY 112, 1990'
C if IFDAY=1 then it is: 'period: 10 Jan - 31 Jan, 1990'
C if IFDAY=2 then it is: 'period: Mon 10 Jan-Mon 17 Jan 1990'
C PERST2 (44 char) includes the time of day but not the year. 
C if IFDAY=2 then it is: 'period Mon 10 Jan@10h30-Mon 17 Jan@10h30'
C IER=0 OK, IER=1 problem.
C BTIM and PETIM are in terms of decimal fractions of a day.
      SUBROUTINE PERSTR(IYEAR,IBDOY,IBTIM,IEDOY,IETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER T1H*5, T1D*5, T2H*5, T2D*5
      CHARACTER PERST1*14,PERST2*40,PERST3*40,DS*7,DS1*10,DE*7,DE1*10
      character outs*124

      IER=0

C Generate view period string based on IBDOY,IEDOY,BTIM,ETIM
      CALL STDATE(IYEAR,IBDOY,DS,DS1)
      CALL STDATE(IYEAR,IEDOY,DE,DE1)
      CALL ESTIME(NTSPH,1,IBTIM,T1H,T1D,TIMER1)
      CALL ESTIME(NTSPH,1,IETIM,T2H,T2D,TIMER2)
      FD1=FLOAT(IBDOY)+TIMER1
      FD2=FLOAT(IEDOY)+TIMER2
      IF(IFTIME.EQ.0)THEN
        WRITE(PERST1,1,IOSTAT=ISTAT,ERR=10)T1H,T2H
    1   FORMAT(' ',A5,'  ',A5,' ')
      ELSEIF(IFTIME.EQ.1)THEN
        WRITE(PERST1,1,IOSTAT=ISTAT,ERR=10)T1D,T2D
      ELSEIF(IFTIME.EQ.2)THEN
        WRITE(PERST1,2,IOSTAT=ISTAT,ERR=10)TIMER1,TIMER2
    2   FORMAT(2F7.4)
      ENDIF

      IF(IFDAY.EQ.0)THEN
        WRITE(PERST3,3,IOSTAT=ISTAT,ERR=10)IBDOY,IEDOY,IYEAR
    3   FORMAT('period: DOY ',I3,' - DOY ',I3,' ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,4,IOSTAT=ISTAT,ERR=10)IBDOY,T1H,IEDOY,T2H
    4     FORMAT('period: DOY ',I3,' @',A5,' - DOY ',I3,' @',A5)
        ELSEIF(IFTIME.EQ.1)THEN
          WRITE(PERST2,4,IOSTAT=ISTAT,ERR=10)IBDOY,T1D,IEDOY,T2D
        ELSEIF(IFTIME.EQ.2)THEN
          WRITE(PERST2,5,IOSTAT=ISTAT,ERR=10)FD1,FD2
    5     FORMAT('period: DOY ',F10.6,' - DOY ',F10.6)
        ENDIF
      ELSEIF(IFDAY.EQ.1)THEN
        WRITE(PERST3,6,IOSTAT=ISTAT,ERR=10)DS,DE,IYEAR
    6   FORMAT('period: ',A7,'- ',A7,' ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,7,IOSTAT=ISTAT,ERR=10)DS,T1H,DE,T2H
    7     FORMAT('period: ',A7,' @',A5,'-',A7,' @',A5)
        ELSE
          WRITE(PERST2,7,IOSTAT=ISTAT,ERR=10)DS,T1D,DE,T2D
        ENDIF
      ELSEIF(IFDAY.EQ.2)THEN
        WRITE(PERST3,8,IOSTAT=ISTAT,ERR=10)DS1,DE1,IYEAR
    8   FORMAT('period: ',A10,'-',A10,' ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,9,IOSTAT=ISTAT,ERR=10)DS1,T1H,DE1,T2H
    9     FORMAT('period ',A10,'@',A5,'-',A10,'@',A5)
        ELSE
          WRITE(PERST2,9,IOSTAT=ISTAT,ERR=10)DS1,T1D,DE1,T2D
        ENDIF
      ENDIF
      RETURN

C Error trapped from I/O above.
  10  write(outs,*) 'EPERSTR: error writing warning or strings.'
      call edisp(iuout,outs)

      END


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

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

C test subroutinhe to capture help text from external file (to be
C found in the ESP-r distribution lib folder) with file ending
C of *.help
      subroutine gethelptext(app,sub,topic,nbhelp)
      
      integer lnblnk  ! function definition

C Parameters.
      character app*24   ! application name
      character sub*24   ! subroutine name
      character topic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Commons.
      integer ifil
      COMMON/FILEP/IFIL
      character h*72
      common/pophelp/h(60)
      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN

C Local variables.
      logical unixok
      integer ln,lna,lnf   ! length of install path, app name app folder
      integer lns,lnt,lnhl ! length of subroutine, topic, helpline
      integer lno          ! length of outstr
      integer K            ! position
      character helpfil*72 ! the help file to load
      character folder*8   ! the source folder for the application
      character WORD*32    ! a token to read
      character outstr*124 ! for help phrases
      character outs*124 ! for messages
      character helpline*72 ! line of help text
      character sq*1        ! single quote
      logical havesub       ! set true if subroutine matches
      logical havetopic     ! set true if topic matches
#include "espinstalldir.h"

C Get to work.
      call isunix(unixok)  ! which folder separator to use
      ln=lnblnk(instpath)  ! where is ESP-r installed
      lna=lnblnk(app)      ! length of app name
      if(app(1:3).eq.'clm')then
        folder='esruclm'
      elseif(app(1:3).eq.'bps')then
        folder='esrubps'
      endif
      lnf=lnblnk(folder)   ! length of folder name
      lns=lnblnk(sub)      ! length of subroutine
      lnt=lnblnk(topic)    ! length of topic
      sq=char(39)   ! assign single quote

      if(unixok)then
        write(helpfil,'(4a)') instpath(1:ln),'/lib/',folder(1:lnf),
     &    '.help'
      endif

      ihfil=ifil+2              ! set file unit number
      CALL ERPFREE(ihfil,ISTAT) ! free it
      CALL EFOPSEQ(ihfil,helpfil,1,IER)
      if(ier.ne.0)then
        call edisp(iuout,'Could not find help text file.')
        call edisp(iuout,helpfil)
        nbhelp=-1
        return
      endif

      nbhelp=0  ! set initial assumption
      havesub= .false.
      havetopic= .false.

C Check the header to see if this is a help text file.
      read(ihfil,'(a)',IOSTAT=IOS,ERR=1) outstr   ! scan header
      if(ios.ne.0) goto 1   ! jump if EOF
      K=0
      CALL EGETW(outstr,K,WORD,'W','*help_text',IER)
      if(WORD(1:10).ne.'*help_text')then
        call edisp(iuout,'Not a help text file.')
        call edisp(iuout,helpfil)
        goto 1
      endif
      CALL EGETW(outstr,K,WORD,'W','application',IER)
      if(WORD(1:lna).ne.app(1:lna))then
        write(outs,'(2a)') 'Not a help text file for ',app(1:lna)
        call edisp(iuout,outs)
        goto 1
      endif

C If we reached this point then we can go looking for a
C subroutine and topic match on each new line.
      scan: do
        read(ihfil,'(a)',IOSTAT=IOS,ERR=1) outstr   ! scan line
        if(ios.ne.0)  exit scan   ! jump if EOF
        lno=lnblnk(outstr)
        if(lno.le.1) cycle  ! jump if a blank line

        if(outstr(1:1).eq.'*')then  ! we had a key phrase
          K=0
          CALL EGETW(outstr,K,WORD,'W','*item',IER)
          if(WORD(1:5).eq.'*item')then
            CALL EGETW(outstr,K,WORD,'W','subroutine',IER)
            if(WORD(1:lns).eq.sub(1:lns))then

C We have a matching subroutine name remember this and if
C there are still more characters in the line then it
C probably is a topic.
              havesub = .true.  ! set true subroutine matches
              if(K.le.lno)then
                CALL EGETW(outstr,K,WORD,'W','topic',IER)
                if(WORD(1:lnt).eq.topic(1:lnt))then
                  havetopic = .true.     ! set true topic matches
                  cycle   ! keep scanning
                else
                  cycle   ! keep scanning
                endif
              else
                cycle   ! keep scanning
              endif
            else
              cycle   ! keep scanning
            endif
          elseif(WORD(1:8).eq.'*enditem')then

C Reached end of item exit scan, if this was the correct sub and topic then
C set number of help lines.
            if(havesub.and.havetopic)then
              CALL ERPFREE(ihfil,ISTAT) ! free it
              return
            else
              cycle  ! keep scanning
            endif
          elseif(WORD(1:6).eq.'*topic')then

C In case there are some old *topic lines in the help f
            if(lno.le.7) cycle  ! jump if no other token
            K=7
            CALL EGETW(outstr,K,WORD,'W','topic',IER)
            if(WORD(1:lnt).eq.topic(1:lnt))then
              havetopic = .true.     ! set true topic matches
              cycle   ! keep scanning
            else
              cycle   ! keep scanning
            endif
          elseif(WORD(1:11).eq.'*subroutine')then
            cycle   ! keep scanning
          endif

C        if(outstr(1:5).eq.'*item')then
C          if(lno.le.6) cycle  ! jump if no other token
C          K=6
C          CALL EGETW(outstr,K,WORD,'W','sub',IER)
C          if(WORD(1:3).eq.'sub')then
C            CALL EGETW(outstr,K,WORD,'W','subroutine',IER)
C            if(WORD(1:lns).eq.sub(1:lns))then

C We have a matching subroutine name
C              havesub = .true.  ! set true subroutine matches
C              cycle   ! keep scanning
C            else
C              cycle   ! keep scanning
C            endif
C          endif
C        elseif(outstr(1:6).eq.'*topic')then
C          if(lno.le.7) cycle  ! jump if no other token
C          K=7
C          CALL EGETW(outstr,K,WORD,'W','topic',IER)
C          if(WORD(1:lnt).eq.topic(1:lnt))then
C            havetopic = .true.     ! set true topic matches
C            cycle   ! keep scanning
C          else
C            cycle   ! keep scanning
C          endif
C        elseif(outstr(1:8).eq.'*enditem')then

C Reached end of item exit scan, if this was the correct sub and topic then
C set number of help lines.
C          if(havesub.and.havetopic)then
C            CALL ERPFREE(ihfil,ISTAT) ! free it
C            return
C          else
C            cycle  ! keep scanning
C          endif
C        elseif(outstr(1:11).eq.'*subroutine')then
C          cycle   ! keep scanning

        elseif(outstr(1:1).eq.sq)then

C We have a line that starts with a quote. If we have reached the
C correct topic then read it otherwise scan another line.
          if(havesub.and.havetopic)then
            K=0
            call EGETDQ(outstr,K,helpline,'W','phrase',IER)
            lnhl=lnblnk(helpline)
            call edisp(iuout,helpline)
            nbhelp=nbhelp+1
            if(lnhl.le.1)then
              h(nbhelp)='  '
            else
              write(h(nbhelp),'(a)') helpline(1:lnhl)
            endif
            cycle   ! keep scanning
          else
            cycle   ! keep scanning
          endif
        else
 
C Does not match expectations.
          cycle   ! keep scanning
        endif
      end do scan

C The end of file reached. If there has not been a match make up a
C standard message.
  1   close(ihfil)
      if(havesub.and.havetopic)then
        return
      else
        h(1)='No contextual help text was found for the topic'
        write(h(2),'(5a)') topic(1:lnt),' in ',sub(1:lns),
     &    ' of application ',app(1:lna)
        nbhelp=2
        return
      endif

      end
