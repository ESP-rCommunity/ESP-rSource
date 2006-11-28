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

C ESP-r to Radiance driver/ translator utility.

C File naming convention:  UNKNOWN - required but not yet defined,
C blank or NONE not required and not yet defined.

      program e2r
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IFPRE (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFCON,IFCON,LFMUL,IFMUL (for materials and constructions database)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Path to model.
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/expath/runpath
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl
      common/rcmd/LCMDFL
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF

C Radiance based commons.
C rofil = `outside` objects, rzfil = is the zone (inside) composition.
C rskyfil is the sky definition file,  matfil,rmmfil contain
C materials and miscel geometries.
C rmfil is for miscel descriptions.
      common/e2rf/rambfil,optnfil
      common/e2rq/indrcb,itrack,imgqua,detlvl,llvar,penumb
      common/rad1/rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil
      common/rad1a/aglzfil
      common/rad1m/matfil,rmmfil
      common/raddn/skydone,outdone,indone,misdone,vewdone,glzdone
      common/rad2/mono,intext,ifocz
      common/radrp/ipicx
      common/radv/rvpx,rvpy,rvpz,vdx,vdy,vdz,angh,angv,azim,elev
      common/grdisk/grdd,grcx,grcy
      common/radgt/iglzty
      common/sky1/rgrfl,isky,irdoy,rtime,iryear
      common/radif/irofil,irzfil,imatfil,iglzfil
      common/e2rc/crenrp,coconv
      common/e2rs/indxscn,rscedes(10),rzoncmd,rupaxis

C Scene configuration commons.
      common/radcfg/LRADCF
      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               RIFDESC(MCOM+1),RIFPURP(MCOM+1)
      common/raddata2/NRIF,IRIFFOC
      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)
      common/cmddata/zone,aim,act
      common/radgrid/LDFGRID

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      CHARACTER H*72,outs*124,ITEM*30
      CHARACTER LDFGRID*72
      CHARACTER inf*144,LCMDFL*144,LCFGF*72,LBSRIF*72,L144*144
      character*72 rofil,rzfil,rskyfil,octfil,picfil,rmfil
      character*72 matfil,rmmfil,glzfil,aglzfil
      character rambfil*72,optnfil*72,tfile*72,runpath*72
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character path*72,upath*72,esprc*72,cfgroot*24
      character dfdblbl*20,defdbfl*72
      character crenrp*160,coconv*40
      character rscedes*72,rzoncmd*60,rupaxis*1
      character imgqua*6,detlvl*6,llvar*6,penumb*5
      character journcmd*20,jfile*72,uname*24,fs*1,cjfile*72
      character SCENE*28,RIFNAME*72,SCENERT*28,RIFDESC*72,RIFPURP*12
      character LRADCF*72,zone*16,aim*12,act*72,OUTSTR*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character ltmp72*72,dtmp72*72

      DIMENSION ITEM(13)

      LOGICAL KEEP,OK,DOK,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND,XST,unixok,there
      logical skydone,outdone,indone,misdone,vewdone,glzdone

C Initialize global common data.
      call ezero
      call curmodule('e2r ')
      IUIN = 5
      IUOUT = 6
      IFIL = 10
      NSIGFIG=3

C Assume configuration file is from IFIL+8, any leakage description
C is fom IFIL+9 and ASCII viewing and geometry file reading on IFILE+1. 
C The current outside description file is irofil = IFIL+3.
C The current room description file is IRRFIL = IFIL+4.
C Glazing (contains defaults and illums) iglzfil = IFIL+16.
C Materials file imatfil = IFIL+11.
C Scene configuration file IRCFG = IFIL+17.
      irofil = IFIL+3
      irzfil = IFIL+4
      imatfil = IFIL+11
      iglzfil = IFIL+16
      IRCFG = IFIL+17

C Primitives on IFIL+13, mlc on IFIL+14, optics on IFIL+15,
C check for root journal file on iuj.
      IFCON=IFIL+13
      IFMUL=IFIL+14
      IOPTDB=IFIL+15
      iuj=IFIL+1

C Temporary ascii files ITA1 = IFIL+6, ITA2 = IFIL+7, ITA3 = IFIL+10.
C      ITA1 = IFIL+6
      ITA2 = IFIL+7
      IFCFG=IFIL+8
      ITA3 = IFIL+10
      call parspza(MODL,iappw,iappx,iappy,inf,zone,aim,act)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Make case insensitive.
      if (aim(1:8).eq.'External') zone='  '
      if (aim(1:8).eq.'coupling') aim='Coupling'
      if (aim(1:8).eq.'day_coef') aim='Day_coef'
      if (act(1:6).eq.'create') act='Create'

      ITRC=1
      IFS=1
      ITFS=1
      IMFS=1
      IRIFFOC=1
      do 9 I=1,MCOM+1
        SCENE(I)='UNDEFINED'
        RIFNAME(I)='UNKNOWN'
        SCENERT(I)='UNKNOWN'
        RIFDESC(I)='UNKNOWN'
        RIFPURP(I)='UNKNOWN'
        LBSRIF(I)='NONE'
        NBSRIF(I)=-1
 9    continue

C Set defaults.
      OK=.false.
      KEEP=.false.
      CFGOK=.false.
      MLDBOK=.false.
      CONDBOK=.false.
      CTLOK=.false.
      OPTKOK=.false.
      MODIFY=.false.
      MODLEN=.false.
      MODBND=.false.
      XST=.false.
      LDFGRID='NONE'
      LRADCF='UNKNOWN'
      LCFGF='UNKNOWN'
      rofil='UNKNOWN'
      matfil='UNKNOWN'
      outdone=.false.
      rzfil='UNKNOWN'
      rmfil='UNKNOWN'
      rmmfil='UNKNOWN'
      glzfil='UNKNOWN'
      aglzfil='UNKNOWN'
      iglzty=1
      indone=.false.
      rskyfil='UNKNOWN'
      skydone=.false.
      misdone=.false.
      vewdone=.false.
      glzdone=.false.
      octfil='UNKNOWN'
      picfil='UNKNOWN'
      radpth='../rad'
      rambfil='NONE'
      optnfil='UNKNOWN'

C Assume no focous zone.
      ifocz = -1

C Default values for rif file variables.
      intext = 0
      imgqua = 'Low'
      detlvl = 'Medium'
      llvar  = 'High'
      indrcb = 0
      ipicx  = 258
      penumb = 'False'
      crenrp='  '
      coconv='  '

      indxscn=0
      do 4 i=1,10
        rscedes(i)=' '
  4   continue

C Assume monochrome monitor.
      mono=0
      ier=0

C Assume sunny, spring day, mid-morning angle of
C views and looking east.
      isky=3
      irdoy=92
      rtime=10.
      iryear=1998
      angh=60.
      angv=60.

      rvpx=-10.
      rvpy=-10.
      rvpz=10.
      vdx=1.
      vdy=0.
      vdz=0.
      azim=90.
      elev=0.

C Initial coords for eyepoint, viewing point, angle of view.
      EYEM(1)=-100.
      EYEM(2)=-100.
      EYEM(3)=100.
      VIEWM(1)=10.
      VIEWM(2)=10.
      VIEWM(3)=10.
      ANG=40.

C General image option flags.
      ITDSP=0
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=1
      ITVNO=1
      ITORG=1
      ITSNR=1
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0

C Initialise output device, assume minimal trace.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=520
        iappx=60
        iappy=10
      else
        if(iappx.le.0)iappx=60
        if(iappy.le.0)iappy=10
        if(iappw.le.200)then
          iappwi=int(520*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(32*iappw*0.01),12)
        LIMTTY=5
        LIMIT =5
      else
        menuchw = 32
        LIMTTY=5
        LIMIT =5
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
     &'Radiance desktop:  enquiries to esru@strath.ac.uk')
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,2,2,2,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
        call opensetup
        call updwire(0)
        call updazi(1)
#endif
        call setzscale()
        call setgscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

      write(outs,'(2a)')
     &  ' ESP-r - Radiance: Version 3.16a of January 2006.',
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow Scotland.'
      call edisp(IUOUT,outs)
      call edisp(iuout,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C If there is a journal active set up to add to the journal. First
C get the root journal file (setup by prj) from the users home folder.
C All subsequent tstamp messages will be appended to root journal.
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

        write(outs,'(a,a)')'Journal (visual analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.
 902  call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take `c' command line file name, convert to fortran string LCMDFL.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
      else
        LCMDFL='  '
      endif

C If an input file has been specified then load and display it
C and then present the main menu.
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then
          call MODLSU
        endif
      endif

C No command line configuration file supplied or cannot be found, 
C therfore get one.
      if(.NOT.XST)then
        H(1)='The model configuration file holds the definition '
        H(2)='of the building/ plant to be simulated, including the'
        H(3)='names of all of the files required.'
        l144=LCMDFL
        dtmp72=DFCFG
 1289   llt=lnblnk(L144)

C Use ifdefs because the X11 version will be returning only the
C name of the file, while the GTK version will be returning the
C name with the full path.
#ifdef X11
        if(llt.lt.96)then
          CALL EASKF(L144,' Model configuration file?',' ',96,dtmp72,
     &    'config file name',IER,3)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(L144,' Model configuration file?',' ',124,dtmp72,
     &    'config file name',IER,3)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(L144,' Model configuration file?',' ',144,dtmp72,
     &    'config file name',IER,3)
        endif
#else
        CALL EASKF(L144,' Model configuration file?',' ',144,dtmp72,
     &    'config file name',IER,3)
#endif

        IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 1289
        call st2file(L144,LCMDFL)
        call usrmsg('File selected is',l144,'-')
        XST=.false.
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if (XST) call MODLSU
      endif

C Confirm monitor type. Skip if purpose has been defined on the command 
C line as 'Coupling' or 'Day_coef' or 'Day_fact'.
      if (aim(1:8).eq.'Coupling'.or.aim(1:8).eq.'Day_coef'.or.
     &    aim(1:8).eq.'Day_fact') then
        mono=0
      else
        H(1)='This is required for several of the display utilities.'
        CALL EASKABC('Please confirm your monitor type:',' ',
     &                        'monochrome','greyscale','colour',imon,1)
        mono=imon-1
      endif

C Read RADIANCE configuration if it exists.
      XST=.FALSE.
      write(tfile,'(a,a)')path(1:lnblnk(path)),LRADCF(1:lnblnk(LRADCF))
      INQUIRE (FILE=tfile,EXIST=XST)
      if(XST)then
        call RADCFGIN

C Check for command line information.
        if (zone(1:4).ne.'NONE'.and.aim(1:4).ne.'NONE') then
          call checkrif('o')
          if (act(1:4).ne.'NONE') call autorad
        else
          if (NRIF.gt.1) then 
            call whichrif('m')
          elseif (NRIF.eq.1) then
            IRIFFOC=1
            call RRIF(ita2,ita3,RIFNAME(IRIFFOC),'r',ier)
          else
            call ADDSCENE(IRCFG)
          endif
        endif
      else

C Check for command line information.
        if (zone(1:4).ne.'NONE'.and.aim(1:4).ne.'NONE') then
          call checkrif('n')
          if (act(1:4).ne.'NONE') call autorad
        else

C Create a default name using the cfgroot.
          write (LRADCF,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'

C File does not exist, ask for another/ new file.
 2289     H(1)='A scene configuration file is required to aid'
          H(2)='maintenance of the visualisation process.'
          H(3)='This file contains details on where individual'
          H(4)='RIF (Radiance Input Format) files are kept and'
          H(5)='what there intended use is.'
          ltmp72=LRADCF
          dtmp72='scene.rcf'
          CALL EASKS(ltmp72,
     &        'Scene configuration file? (suggested name)',
     &        ' ',72,dtmp72,'scene file name',IER,5)
          IF(ltmp72(1:2).EQ.'  '.or.ltmp72(1:4).eq.'UNKN')GOTO 2289
          LRADCF=ltmp72
          XST=.FALSE.
          INQUIRE (FILE=LRADCF,EXIST=XST)
          if(XST)then
            call RADCFGIN
            if (NRIF.gt.1) then 
              call whichrif('m')
            elseif (NRIF.eq.1) then
              IRIFFOC=1
              call RRIF(ita2,ita3,RIFNAME(IRIFFOC),'r',ier)
            else
              call ADDSCENE(IRCFG)
            endif
          else
            call ADDSCENE(IRCFG)
          endif
        endif
      endif

10    INO = -4
      write(ITEM(1),'(A,A)')  'a model: ',LCFGF(1:21)
      write(ITEM(2),'(A,A)')  '   path: ',path(1:21)
      ITEM(3)=                '  ____________________________'
      write(ITEM(4),'(A,A)')  'b scenes  : ',LRADCF(1:18)
      write(ITEM(5),'(A,A)')  'c name    : ',SCENE(IRIFFOC)(1:18)
      write(ITEM(6),'(A,A)')  '   RIF    : ',RIFNAME(IRIFFOC)(1:18)
      write(ITEM(7),'(A,A)')  '   root   : ',SCENERT(IRIFFOC)(1:18)
      write(ITEM(8),'(A,A)')  '   purpose: ',RIFPURP(IRIFFOC)
      ITEM(9) =               'd create / edit scene details '
      ITEM(10) =              'e calculate / view scene      '
      ITEM(11) =              '  ____________________________'
      ITEM(12)=               '? Help                        '
      ITEM(13)=               '- Quit                        '
      NITEMS = 13

C If user has defined model and perhaps resized the display then
C redraw the model image.
      if(CFGOK.AND.MODIFY)then
        MODBND=.TRUE.
        MODLEN=.TRUE.
        nzg=NCOMP
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE

C (Re)Set all surfaces to std line width.
        CALL INLNST(1)
        izgfoc=0
        CALL ADJVIEW(ITRC,IER)

C At this point we know the new model bounds. Reset the ground disk
C diameter and centre.
        XD=(XMX-XMN)**2 + (YMX-YMN)**2
        grdd=SQRT(XD)
        grcx=XMN+((XMX-XMN)/2.)
        grcy=YMN+((YMX-YMN)/2.)
      endif

C Use askabout to instanciate the initial help messages (2nd parameter is one).
  20  call askabout('e2r ',1)

C Present the main menu.
      CALL EMENU('Radiance desktop',ITEM,NITEMS,INO)

      IF(INO.EQ.NITEMS)THEN

C Copy files back to radpth if necessary.
        dok=.false.
        h(1)='The e2r module has been working with files in '
        h(2)='a temporary folder. If you say `yes` then the '
        h(3)='files will be copied back into the rad folder '
        h(4)='of the ESP-r model. '
        h(5)=' '
        h(6)='If you say `no` the temporary folder will be '
        h(7)='removed and your files deleted. '
        h(8)=' '
        h(9)='If you are planning to alter the geometry and/'
        h(10)='or constructions in your model it is usually'
        h(11)='best not to keep the files (i.e. select `no`).'
        CALL ASKOK('Do you wish to keep the Radiance scene files?',
     &             '(see help)',KEEP,dok,11)
        if (KEEP) then
          write (outs,'(5a)')'mv -f ',runpath(1:lnblnk(runpath)),
     &               LRADCF(1:lnblnk(LRADCF)),' ',path(1:lnblnk(path))
          call runit(outs,'-')
          write (outs,'(5a)')'mv -f ',runpath(1:lnblnk(runpath)),'* ',
     &                                     path(1:lnblnk(path)),radpth
          call runit(outs,'-')
        endif
        write (outs,'(a,a)') 'rm -fr ',runpath(1:lnblnk(runpath))
        call runit(outs,'-')
        call tstamp('>','Finish visual analysis')
        CALL EPAGEND
        STOP
      elseif(INO.EQ.NITEMS-1)then

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('e2r ',0)
      elseif(INO.EQ.1)then

C ESP-r configuration file.
  289   H(1)='The model configuration file holds the definition '
        H(2)='of the building/ plant to be simulated, including the'
        H(3)='names of all of the files required.'
        l144=LCMDFL
        dtmp72=DFCFG
        llt=lnblnk(L144)
#ifdef X11
        if(llt.lt.96)then
          CALL EASKF(L144,' Model configuration file?',' ',96,dtmp72,
     &    'config file name',IER,3)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(L144,' Model configuration file?',' ',124,dtmp72,
     &    'config file name',IER,3)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(L144,' Model configuration file?',' ',144,dtmp72,
     &    'config file name',IER,3)
        endif
#else
        CALL EASKF(L144,' Model configuration file?',' ',144,dtmp72,
     &    'config file name',IER,3)
#endif
        IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 289
        call st2file(L144,LCMDFL)
        call usrmsg('File selected is',l144,'-')
        XST=.false.
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST) call MODLSU
      elseif(INO.EQ.4)THEN

C Define possibly different scene configuration file, process if existing 
C or create a new one with reasonable defaults.
 319    H(1)='A scene configuration file is required to aide'
        H(2)='maintenance of the visualisation process.'
        H(3)='This file contains details on where individual'
        H(4)='RIF (Radiance Input Format) files are kept and'
        H(5)='wheat there intended use is.'
        ltmp72=LRADCF
        dtmp72='scene.rcf'
        CALL EASKS(ltmp72,' Scene configuration file (suggested name)?',
     &    '  ',72,dtmp72,'scene file name',IER,5)
        if(ltmp72.eq.'  ')goto 319 
        LRADCF=ltmp72
        call tstamp('>','E2R: change scene configuration')
        call RADCFGIN

C Select scene from all available, or edit scene list.
      elseif(INO.EQ.5)then
        call tstamp('>','E2R: select scene configuration')
        call whichrif('m')
      elseif(INO.EQ.9)THEN

C Generate Radiance files.
        if(.NOT.CFGOK)then
          CALL USRMSG(' ','Please define the model first!','W')
        else
          call tstamp('>','E2R: generate radiance files')
          call EMKRAD
        endif

C Radiance visualisation/ calculation after reading in current rif file.
      ELSEIF(INO.EQ.10)THEN
        call rrif(ITA2,ITA3,RIFNAME(IRIFFOC),'r',IER)
        if (ier.eq.0) then
          call tstamp('>','E2R: enter visualisation')
          call MKVREND
          call tstamp('>','E2R: exit visualisation')
        else
          call tstamp('>','E2R: visualisation scene file missing')
          call usrmsg(' Could not find all the scene files.',
     &         '(Check that they have been created via option d.)','W')
        endif
      ELSE
        INO=-2
        goto 20
      ENDIF
      goto 10

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      END


C *************** MODLSU ***************
C MODLSU read in supplied configuration file and set up paths.
      subroutine MODLSU
#include "building.h"

      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

C Path to model.
      common/rpath/path
      common/uhome/upath
      common/expath/runpath
      common/rcmd/LCMDFL

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/grdisk/grdd,grcx,grcy
      common/sky1/rgrfl,isky,irdoy,rtime,iryear

      COMMON/C1/NCOMP,NCON
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/GTFIL/GTGEOM

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI

      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      character path*72,cfgroot*24,runpath*72,GTGEOM*72
      CHARACTER LCMDFL*144,LCFGF*72
      character outs*124,fs*1,tldir*72,upath*72

      LOGICAL MODIFY,MODLEN,MODBND,unixok
      LOGICAL        CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Configuration file and path are held in common/rcmd/LCMDFL
      call fdroot(LCMDFL,path,LCFGF)
      IAIRP = IFIL+9
      call ESCSYS(LCFGF,IFCFG,IAIRP,IER)
      if(IER.eq.0)then
        call inqu_tmc
        CFGOK=.TRUE.
        rgrfl=GREF
        NZONES=NCOMP
        ITRC=0
        CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.

C If there is a ground topology read it.
        if(GTGEOM(1:2).eq.'  '.or.GTGEOM(1:4).eq.'UNKN')then
          continue
        else
          iunit=IAIRP
          call egrnin(iunit,gtgeom,itrc,itru,ier)
        endif
        call tstamp('>','E2R: scanning model')
        call tstamp('>',LCFGF)
        nzg=NCOMP
        IF(MMOD.EQ.8)call updwire(ncomp)
        DO 444 I=1,nzg
          nznog(I)=I
 444    CONTINUE

C (Re)Set all surfaces to std line width.
        CALL INLNST(1)
        izgfoc=0
        if(initvt.eq.1)then
          EYEM(1)=EYEMI(1)
          EYEM(2)=EYEMI(2)
          EYEM(3)=EYEMI(3)
          VIEWM(1)=VIEWMI(1)
          VIEWM(2)=VIEWMI(2)
          VIEWM(3)=VIEWMI(3)
          ANG=ANGI
        endif
        CALL ADJVIEW(ITRC,IER)

C At this point we know the model bounds. Set the ground disk
C diameter and centre.
        XD=(XMX-XMN)**2 + (YMX-YMN)**2
        grdd=SQRT(XD)
        grcx=XMN+((XMX-XMN)/2.)
        grcy=YMN+((YMX-YMN)/2.)

C Get PID for use in /tmp directory structure.
        call esppid(ipid)
        write (tldir,'((5a,i7,a))') upath(1:lnblnk(upath)),fs,
     &                                       '.',fs,'.rad',ipid,fs
        call st2file(tldir,runpath)
        write (outs,'(a,a)') 'mkdir ',runpath(1:lnblnk(runpath))
        call runit(outs,'-')
      endif

      return
      end


C *************** VIFILE ***************
C VIFILE Browse or manually edit a text file.
C opt is b/B for browsing, e/E for editing.
C lfile contains path as well as file name.
      subroutine vifile(ifu,lfile,opt,ier)

      COMMON/OUTIN/IUOUT,IUIN
      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20,tmode*8
      character lfile*72,opt*1,doit*124

      logical XST

      ier = 0

      XST=.FALSE.
      if(lfile(1:2).ne.'  '.and.lfile(1:4).ne.'UNKN')then
        INQUIRE (FILE=lfile,EXIST=XST)
      endif
      if(opt.eq.'b'.or.opt.eq.'B')then
        if(XST)then
          call LISTAS(ifu,lfile,IER)
        else
          call edisp(iuout,'There is no file named... ')
          call edisp(iuout,lfile)
        endif
      elseif(opt.eq.'e'.or.opt.eq.'E')then
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &                                      lfile(1:lnblnk(lfile)),' &'
        call runit(doit,tmode)
      endif
      return
      end

C **** copy of opendb from esruprj/eddb.F ******
C *************** opendb ***************
C Open the common databases - primitive constructions, multilayer constructions,
C and optical db.
      subroutine opendb(ier)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      LOGICAL    XST,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      character SOPT*12,outs*124,GDESCR*36
      IER=0

C Open currently named primitive construction db.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by with changing
C this to 24 words wide.
      CALL ERPFREE(IFCON,ISTAT)
      call EFOPRAN(IFCON,LFCON,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(a,a,a)') ' No primitive constructions db ',
     &     LFCON(1:lnblnk(LFCON)),' found.'
        call edisp(iuout,outs)
        CONDBOK=.FALSE.
      else
C        CALL edisp(iuout,' opened primitive materials db.')
        CONDBOK=.TRUE.
      endif
        
C Read multilayer db information into common.
      CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,ITRU,IER)
        IF(IER.NE.0)then
          write(outs,'(a,a,a)') ' Problem with multilayer constr db ',
     &     LFCON(1:lnblnk(LFCON)),'... '
          call edisp(iuout,outs)
          MLDBOK=.FALSE.
        else
C          call edisp(iuout,' opened multilayer constructions db.')
          MLDBOK=.TRUE.
        endif
      else
        write(outs,'(a,a,a)') ' No multilayer constructions db ',
     &     LFCON(1:lnblnk(LFCON)),' found.'
        call edisp(iuout,outs)
      endif

C Open optical database and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg(' Optical db not found or there was a problem',
     &  ' reading it. Please check database management ASAP .','W')
        OPTKOK=.FALSE.
      else
C        call edisp(iuout,' opened optical properties db.')
        OPTKOK=.TRUE.
      endif

      return
      end


C ************* inqu_tmc *************
C inqu_tmc: Check if tmc control active.
      subroutine inqu_tmc
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      common/radabs/NABS

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character outs*124

      logical XST

      ITA1=IFIL+6

C NABS = number of alternative blind states.
      NABS=0

C Loop through zones checking if a tmc file has been referenced.
      do 10 ICOMP=1,NCOMP
        if (ITW(ICOMP).eq.1) then

C Rescan zone Construction and TMC file 
C (need to test blind/ shuter control flags).
          call FINDFIL(LTHRM(ICOMP),XST)
          if(XST)then
            call EGOMIN(ITA1,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
            CALL ECONST(LTHRM(ICOMP),ITA1,ICOMP,0,IUOUT,IER)
            CALL ERTWIN(0,IUOUT,ITA1,LTWIN(ICOMP),ICOMP,IER)
          else
            write (outs,'(a,a)') ' Could not find construction file: ',
     &                                                     LTHRM(ICOMP)
            call usrmsg(outs,'blind control tracking not possible!','W')
          endif

C Check if tmc control found.
          do 30 I=1,MTMC
            if (IBCMT(ICOMP,I).gt.NABS) NABS=IBCMT(ICOMP,I)
 30       continue
        endif
 10   continue


      return
      end

C ************* imgdisp *************
C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end
