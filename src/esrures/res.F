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
C     IFIL +  8 : unused
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
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFCON,IFCON,LFMUL,IFMUL (for materials and constructions database)
C default file names for databases

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appw/iappw,iappx,iappy
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/DEFLT/IDEFLT
      COMMON/PSY1/PATMOS,IOPT
      common/trc/itrc
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Flags identifying if results libries are available. =0 no lib, =1 lib exists.
      COMMON/LIBOK/IBLIB,IFLIB,ICLIB,IELIB,IPLIB

C Where default db list is kept.
      common/defdb/dfdblbl,defdbfl

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
      common/uhome/upath
      common/udot/esprc
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
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

C Optional title for graph and up to 5 other labels and their positions.
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      COMMON/pres/iunitp

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical     unixok,there,OK

      CHARACTER ITEM(19)*23
      CHARACTER*72 H,xfile,LTMP,LCFGF,actf,Lflpar
      character inf*144,LCMDFL*144,outs248*248
      character outs*124,MODE*4,tg*1,delim*1
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,LFCFG
      character path*72,upath*72,esprc*72,spsname*8,paction*16
      character dfdblbl*20,defdbfl*72,cfgroot*24,fs*1,act*16,aut*12
      character journcmd*20,jfile*72,uname*24,cjfile*72,OUTSTR*124
      character bact*4,pifltog*4,lpifl*72
      character graftitle*64,grlbl*24,LTDF*72,LTDFA*72

C Initialize global common data.
      call ezero
      call curmodule('res ')
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
      IUTDF=IFIL+7
      IUTDFA=IFIL+8

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
#ifdef X11
        call win3d(menuchw,15,15,5,3,igl,igr,igt,igb,igw,igwh)
        call opencpw
        call opentutorial
        call opensetup
        call updcapt(1)
#else
        CALL win3d(menuchw,9,20,5,3,igl,igr,igt,igb,igw,igwh)
#endif
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
      write(outs,'(2a)')
     & ' ESP-r Results Analysis Tool: Version 7.41 of November 2006.',
     & ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
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
 902  call escdef(dfdblbl,defdbfl,ixunit,IER)

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
      IFCON=IFIL+3
      LFCON=DFCON

C Assume multilayer input from IFIL+9.
      IFMUL=IFIL+9
      LFMUL=DFMUL

C Initialise results library.
      CALL MORFIL(iftype)
      if(iftype.eq.-1) then

C Get the name of the configuration file name 
C Read the plant configuration file.
        h(1)='In order to read the plant results'
        h(2)='you must supply the prj `plant only`'
        h(3)='model configuration file name. Note  '
        h(4)='that res CANNOT read the PLANT configuration '
        h(5)='file unless it has first read the model '
        h(6)='configuration file.'

        call easks(LTMP,'Configuration file name ?',
     &   ' ',72,' ','configuration file',IER,0)
        call fdroot(LTMP,path,LFCFG)

C Read the plant configuration file.
        IFCFG=IFIL+1
        IAPROB=IFIL+2  
        MODE='ALL '
        call ERSYS(LFCFG,IFCFG,IAPROB,MODE,ITRC,IER)

C If we have a plant only file go straight to the plant results menu.
        call sysoutp('-',0)

C Free all assigned files.
        call erpfree(iunitp,istat)
        call usrmsg(' Closing results analysis...',' ','P')
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


C ***** imgdisp
C If there are associated images for startup or at specific points
C and the image browser has not been invoked do this now.
      subroutine imgdisp(iforce,focus,ier)

      COMMON/OUTIN/IUOUT,IUIN
C Images.
      COMMON/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton
      common/showimg/imgtyp,fmttag(5),fmtexe(5)
      character imgfmt*4,imgfoc*4,limgfil*72,ilist*200,dolist*254
      character fmttag*4,fmtexe*20,focus*4,tfile*72
      character longtfile*144,longtfiledos*144
      logical concat,show,unixok

C Loop through the images and for those which match the "focus"
C can be displayed with a known utility can be brought up. Iforce,
C if set to 1 forces display.
      if(noimg.eq.0)return
      if(imgtyp.eq.0)then
        call edisp(iuout,' ')
        call edisp(iuout,'No image display apps available..')
        call edisp(iuout,'See tutorial to set up .esprc files.')
        return
      endif
      if((iton.eq.0.or.iforce.eq.1))then

C Check if Unix-based or DOS based.
        call isunix(unixok)
        do 498 imgv=1,imgtyp
          ilist=' '
          ix=1
          do 49 img=1,noimg
            if(imgfmt(img)(1:3).eq.fmttag(imgv)(1:3))then

C If focus is `****` then show images marked with `****` or whos mark
C begins with and `S`, otherwise attempt to match the focus and the image. 
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
                ixl=lnblnk(longtfile)
                ixe=ix+ixl
                if(ixe.le.200)then
                  WRITE(ilist(ix:ixe),'(a,2x)')longtfile(1:ixl)
                endif
                ix=ixe+1
              endif
            endif
 49       continue
          if(ix.gt.1)then
            dolist=' '
            write(dolist,'(a,2x,a,a)')
     &          fmtexe(imgv)(1:lnblnk(fmtexe(imgv))),
     &          ilist(1:lnblnk(ilist)),' &'
            call runit(dolist,'graph')
            iton=1
          endif
 498    continue
      endif
      return
      end

C Dummy routines from common3dv.F
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

      SUBROUTINE PLELEV(IUF,direc,ier)
      CHARACTER direc*1
      ier=0
      return
      end
