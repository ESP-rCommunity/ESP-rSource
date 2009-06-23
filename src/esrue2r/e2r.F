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
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "espriou.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Path to model.
      common/rpath/path
      common/uhome/upath
      common/expath/runpath,pathtype
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/user/browse

      common/rcmd/LCMDFL
      
      integer ncomp,ncon
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
      common/radgrpts/NDFP,DFPTS(500,3),DFDIR(3),DFVALS(500)
      common/e2rv/vewcmds(20),rvewsh(20),indxvew,ipckvew
      character vewcmds*124,rvewsh*10

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12      

C Materials data array.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      CHARACTER H*72,outs*124,ITEM*30
      CHARACTER LDFGRID*72
      CHARACTER inf*144,LCMDFL*144,LCFGF*72,LBSRIF*72,L144*144
      character*72 rofil,rzfil,rskyfil,octfil,picfil,rmfil
      character*72 matfil,rmmfil,glzfil,aglzfil
      character rambfil*72,optnfil*72,tfile*72,runpath*72,pathtype*24
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character path*72,upath*72,cfgroot*24
      character crenrp*160,coconv*40
      character rscedes*72,rzoncmd*60,rupaxis*1
      character imgqua*6,detlvl*6,llvar*6,penumb*5
      character journcmd*20,jfile*72,uname*24,fs*1,cjfile*72
      character SCENE*28,RIFNAME*72,SCENERT*28,RIFDESC*72,RIFPURP*12
      character LRADCF*72,zone*16,aim*12,act*72,OUTSTR*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character ltmp72*72,dtmp72*72,ltmp*72
      character pf*72,gf*72,df*72,VALT*28
      character brw*4   ! to signal whether browsing model
      DIMENSION ITEM(24)
      DIMENSION IVALV(20),VALT(20),IPVALV(20)  ! for selecting view from list

      LOGICAL KEEP,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND,XST,unixok,there
      logical skydone,outdone,indone,misdone,vewdone,glzdone
      LOGICAL ok,dok
      logical browse  ! is true if parameter passed as yes

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer lnrp  ! length of run path
      integer numberofzones ! to pass to updwire rather than ncomp

C Initialize global common data.
      call ezero
      call curmodule('e2r ')
      cAppName = 'e2r'
      IUIN = 5
      IUOUT = 6
      IFIL = 10
      NSIGFIG=3
      matver=0.0   ! initial assumption of binary materials database

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
      IFMAT=IFIL+13
      IFMUL=IFIL+14
      IOPTDB=IFIL+15
      iuj=IFIL+1

C Temporary ascii files ITA1 = IFIL+6, ITA2 = IFIL+7, ITA3 = IFIL+10.
      ITA1 = IFIL+6
      ITA2 = IFIL+7
      IFCFG=IFIL+8
      ITA3 = IFIL+10

      call parse2r(MODL,iappw,iappx,iappy,inf,zone,aim,act,brw)

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
      KEEP=.false.
      CFGOK=.false.
      MLDBOK=.false.
      MATDBOK=.false.
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

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,2,2,2,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          numberofzones=0
          call updwire(numberofzones)  ! pass local integer
          call updazi(1)
        endif
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

      CALL ESPrVersion("summary",cAppName,IUOUT)
      
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow Scotland.'
      call edisp(IUOUT,outs)
      call edisp(iuout,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
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
 902  call escdef(IFIL+1,IER)

C Take `c' command line file name, convert to fortran string LCMDFL.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
      else
        LCMDFL='  '
      endif

C Check if the command line included a browse directive.
      if(brw(1:2).eq.'no')then
        browse= .false.
      elseif(brw(1:3).eq.'yes')then
        browse= .true.
      endif

C debug.
      write(outs,'(2a)') 'Passed browse parameter of ',brw
      call edisp(iuout,outs)

C If an input file has been specified then load and display it
C and then present the main menu.
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then
          call MODLSU
          lnrp=lnblnk(runpath)
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

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(L144,' Model configuration file?',' ',96,dtmp72,
     &        'config file name',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(L144,' Model configuration file?',' ',124,
     &        dtmp72,'config file name',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(L144,' Model configuration file?',' ',144,
     &        dtmp72,'config file name',IER,3)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(L144,' Model configuration file?',' ',144,dtmp72,
     &      'config file name',IER,3)
        else
          CALL EASKF(L144,' Model configuration file?',' ',96,dtmp72,
     &      'config file name',IER,3)
        endif

        IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 1289
        call st2file(L144,LCMDFL)
        call usrmsg('File selected is',l144,'-')
        XST=.false.
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if (XST)then
          call MODLSU
          lnrp=lnblnk(runpath)
        endif
      endif

C Confirm monitor type. Skip if purpose has been defined on the command 
C line as 'Coupling' or 'Day_coef' or 'Day_fact'.
      if (aim(1:8).eq.'Coupling'.or.aim(1:8).eq.'Day_coef'.or.
     &    aim(1:8).eq.'Day_fact') then
        mono=0
      else
        if(nzonec.ge.20)then
          mono=2   ! can display more than 20 zone colours assume colour display
        elseif(nzonec.lt.20.and.ngset.gt.20)then
          mono=1   ! not many colours but some greys so set to greyscale
        else
          H(1)='This is required for several of the display utilities.'
          CALL EASKABC('Please confirm your monitor type:',' ',
     &                        'monochrome','greyscale','colour',imon,1)
          mono=imon-1
        endif
      endif

C Read RADIANCE configuration if it exists. If we are returning to a
C model that was previously worked on then thescene file would be
C in the cfg folder. If we are not browsing then that file should be
C copied into the ../rad folder.
      XST=.FALSE.
      write(tfile,'(a,a)')path(1:lnblnk(path)),LRADCF(1:lnblnk(LRADCF))
      INQUIRE (FILE=tfile,EXIST=XST)
      if(XST)then
C debug   write(6,*) tfile
        continue
      else
        lnrp=lnblnk(runpath)
        write(tfile,'(a,a)')runpath(1:lnrp),LRADCF(1:lnblnk(LRADCF))
        INQUIRE (FILE=tfile,EXIST=XST)
        if(XST)then
C debug    write(6,*) tfile
          continue
        else

C Ask user about the scene file. Begin by checking for command line information.
          if (zone(1:4).ne.'NONE'.and.aim(1:4).ne.'NONE') then
            call checkrif('n')
            if (act(1:4).ne.'NONE') call autorad
          else

C Create a default scene file name using the cfgroot.
            write (LRADCF,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'

C File does not exist, ask for another/ new file.
 2289       H(1)='A scene configuration file is required to aid'
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
              continue
            else
              call ADDSCENE(IRCFG)
            endif
          endif
        endif
      endif

C Scan the scene file.
      call RADCFGIN

C Check for command line information.
      if (zone(1:4).ne.'NONE'.and.aim(1:4).ne.'NONE') then
        call checkrif('o')
        if (act(1:4).ne.'NONE') call autorad
      else
        if (NRIF.gt.1) then
          call edisp(iuout,
     &      'There are existing scenes to select from...')
          call whichrif('m')
        elseif (NRIF.eq.1) then
          IRIFFOC=1
          call RRIF(ita2,ita3,RIFNAME(IRIFFOC),'r',ier)
        else
          call edisp(iuout,
     &      'There are no existing scenes. Pleases create one.')
          call ADDSCENE(IRCFG)
        endif
      endif

10    INO = -4
      write(ITEM(1),'(2A)')   'a model: ',LCFGF(1:21)
      write(ITEM(2),'(2A)')   '   path: ',path(1:21)
      ITEM(3)=                '  ____________________________'
      write(ITEM(4),'(2A)')   'b scenes  : ',LRADCF(1:18)
      write(ITEM(5),'(2A)')   'c name    : ',SCENE(IRIFFOC)(1:18)
      write(ITEM(6),'(2A)')   '   RIF    : ',RIFNAME(IRIFFOC)(1:18)
      write(ITEM(7),'(2A)')   '   root   : ',SCENERT(IRIFFOC)(1:18)
      write(ITEM(8),'(2A)')   '   purpose: ',RIFPURP(IRIFFOC)
      ITEM(9)=                '   ___________________________'
      ITEM(10)=               'd  sky type, location & time  '
      ITEM(11)=               'e  zone & outside composition '
      ITEM(12)=               'f  other (furniture&fittings) '
      if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        ITEM(13)=             'g  edit/display grid          '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
        ITEM(13)=             'g                             '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
        ITEM(13)=             'g                             '
      else
        ITEM(13)=             'g  scene view points          '
      endif
      ITEM(14)=               '   ___________________________'
      ITEM(15)=               'h  scene parameter options    '
      if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        ITEM(16)=             'i  calculate daylight factors '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
        ITEM(16)=             'i  calculate sensor illum.    '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
        ITEM(16)=             'i  calculate daylight coeffs  '
      else
        ITEM(16)=             'i  render the scene           '
      endif
      if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        ITEM(17)=             'j  view daylight factor result'
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
        ITEM(17)=             'j  view calculated illuminance'
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
        ITEM(17)=             'j  view daylight coeffs       '
      else
        ITEM(17)=             'j  view visualisation results '
      endif
      ITEM(18)=               '>  save scene information     '
      ITEM(19)=               '   __________________________ '
      ITEM(20)=               '? help                        '
      ITEM(21)=               '- quit                        '
      NITEMS = 21

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
        CALL ADJVIEW(IER)

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

C Copy files back to radpth if necessary. If running native windows
C then there is no need to copy all files back.
        lnrp=lnblnk(runpath)
        call isunix(unixok)
        if(unixok)then
          if(browse)then
            dok=.false.
            h(1)='The e2r module has been working with files in'
            h(2)='a temporary folder. If you say `yes` then the'
            h(3)='files will be copied back into the rad folder'
            h(4)='of the ESP-r model.'
            h(5)=' '
            h(6)='If you say `no` the temporary folder will be'
            h(7)='removed and your files deleted.'
            h(8)=' '
            h(9)='If you are planning to alter the geometry and/'
            h(10)='or constructions in your model it is usually'
            h(11)='best not to keep the files (i.e. select `no`).'
            CALL ASKOK('Do you wish to keep the Radiance scene files?',
     &             '(see help)',KEEP,dok,11)
          else
	  
C User owns the model. Only copy the lradcf file back to the cfg
C folder (the other files can stay where they are).
            write (outs,'(5a)')'cp -f ',runpath(1:lnrp),
     &               LRADCF(1:lnblnk(LRADCF)),' ',path(1:lnblnk(path))
            call runit(outs,'-')
            call edisp(iuout,outs)
            KEEP=.false.
          endif
        else
	  
C User owns the model. Only copy the lradcf file back to the cfg
C folder (the other files can stay where they are).
          write (outs,'(5a)')'cp -f ',runpath(1:lnrp),
     &               LRADCF(1:lnblnk(LRADCF)),' ',path(1:lnblnk(path))
          call runit(outs,'-')
          call edisp(iuout,outs)
          KEEP=.false.
        endif
        lnrp=lnblnk(runpath)
        if (KEEP) then

C Move the radiance configuration file and the other files in the
C temporary folder and then remove the temporary folder.
          write (outs,'(5a)')'mv -f ',runpath(1:lnrp),
     &               LRADCF(1:lnblnk(LRADCF)),' ',path(1:lnblnk(path))
          call runit(outs,'-')
          write (outs,'(5a)')'mv -f ',runpath(1:lnrp),'* ',
     &                                     path(1:lnblnk(path)),radpth
          call runit(outs,'-')
          write (outs,'(a,a)') 'rm -fr ',runpath(1:lnrp)
          call runit(outs,'-')
        endif
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
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(L144,' Model configuration file?',' ',96,dtmp72,
     &        'config file name',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(L144,' Model configuration file?',' ',124,
     &        dtmp72,'config file name',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(L144,' Model configuration file?',' ',144,
     &        dtmp72,'config file name',IER,3)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(L144,' Model configuration file?',' ',144,dtmp72,
     &      'config file name',IER,3)
        else
          CALL EASKF(L144,' Model configuration file?',' ',96,dtmp72,
     &      'config file name',IER,3)
        endif

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

      elseif(INO.EQ.10)then

C Sky type.
        call SKYFORM(IER)
        call edisp(iuout,' updating RIF file to account for zones.')
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
        endif

      elseif(INO.EQ.11)then

C Zone and outside composition, updating rif file upon return.
        call E2RFORM('i',IER)
        call edisp(iuout,' updating RIF file to account for zones.')
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
        endif

      elseif(INO.EQ.12)then

C Other descriptions.
        H(1)='In order not to disturb files generated from a thermal'
        H(2)='description it is sometimes useful to place miscel '
        H(3)='furniture and fixtures in a separate file. '
        ltmp=rmfil
        CALL EASKS(ltmp,' Miscel (furniture & fittings file ?',
     &    '  ',72,rmfil,'misc fixt file name',IER,3)
        INQUIRE (FILE=ltmp,EXIST=XST)
        if(XST)rmfil = ltmp

        H(1)='Browsing lists contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Miscel (furniture & fittings description:',' ',
     &     'browse file','edit file','ignore',IW,2)
        if(IW.eq.1)then
          call vifile(ITA1,rmfil,'b',ier)
        elseif(IW.eq.2)then
          call vifile(ITA1,rmfil,'e',ier)
        endif

      elseif(INO.EQ.13)then

C Depending on value of rifpurp.
        if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
          call ERPFREE(ITA1,ISTAT)
          write(tfile,'(a,a)')runpath(1:lnrp),
     &                        LDFGRID(1:lnblnk(LDFGRID))
          call FPOPEN(ITA1,ISTAT,1,1,tfile)
          if(ISTAT.eq.0)then
            NDFP=0
  42        CALL STRIPC(ITA1,OUTSTR,0,ND,0,'grid points',IFER)
            if (IFER.eq.0) then
              NDFP=NDFP+1
              K=0
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid x',IER)
              DFPTS(NDFP,1)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid y',IER)
              DFPTS(NDFP,2)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid z',IER)
              DFPTS(NDFP,3)=XTMP
              CALL EGETWR(OUTSTR,K,DFDIR(1),0.,0.,'-','vec x',IER)
              CALL EGETWR(OUTSTR,K,DFDIR(2),0.,0.,'-','vec y',IER)
              CALL EGETWR(OUTSTR,K,DFDIR(3),0.,0.,'-','vec z',IER)
              goto 42
            endif
            CALL DFGRIDLBL('g')
          endif
          call ERPFREE(ITA1,ISTAT)
          dok=.false.
          h(1)='If you want to alter the grid points say yes.'
          CALL ASKOK('Edit grid points?','  ',OK,dok,1)
          izone=0
          if(OK)call setdfgrid('-',izone,ier)
        elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
          continue
        elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
          continue
        else

C Edit view points.  Save off current views.
          teye1=EYEM(1)
          tviewm1=VIEWM(1)
          teye2=EYEM(2)
          tviewm2=VIEWM(2)
          teye3=EYEM(3)
          tviewm3=VIEWM(3)
          tang=ANG
          thang=HANG
          itbndt=ITBND
          call VEWFORM(IER)
          call edisp(iuout,' updating RIF file to account for views.')
          IFC=3
          call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
          if (NBSRIF(IRIFFOC).eq.1) then
            call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
          endif

C Because of the site plan, recalculate perspective view.
C Recover previous view.
          EYEM(1)=teye1
          VIEWM(1)=tviewm1
          EYEM(2)=teye2
          VIEWM(2)=tviewm2
          EYEM(3)=teye3
          VIEWM(3)=tviewm3
          ANG=tang
          HANG=thang
          ITBND=itbndt
          MODIFY=.TRUE.
          MODLEN=.TRUE.
        endif

      elseif(INO.EQ.15)then

C Scene parameter options.
        call RRSKY(ITA2,rskyfil,ga,IER)
        if(IER.LT.0)then 
          CALL USRMSG(' ',' Please define the sky first!','W')
          GOTO 10
        endif
        call RIFFORM(ier)

      elseif(INO.eq.16)then

C Based on purpose calculate DF or sensor illum or daylight coeffs or render.

C Generate image/ visualisation data/ daylight factors.
C Check and see if there is an existing octree file.  Probably best
C to remove it before proceeding.
        CALL ERPFREE(ITA2,ISTAT)
        call FINDFIL(octfil,XST)
        if(XST)then
          H(1)='The octree is a intermediate step between the '
          H(2)='Radiance input files and the rendered image. '
          H(3)='Radiance should be able to decide if the octree '
          H(4)='needs recreated or not, although if you have a '
          H(5)='new sky file then you must force Radiance to  '
          H(6)='recreate the octree (ie choose option 1).'
          CALL EASKABC(' The octree exists, do you want to',' ',
     &      'force update','let Radiance decide (default)','cancel',
     &                                                        IOCTFL,6)
          if (IOCTFL.eq.3) then
            goto 10
          endif
        endif

C Pull up a list of views and ask user for which one.
        if (RIFPURP(IRIFFOC)(1:8).eq.'External'.or.
     &      RIFPURP(IRIFFOC)(1:8).eq.'Internal'.or.
     &      RIFPURP(IRIFFOC)(1:5).eq.'Glare') then
          if (indxvew.gt.0) then
            H(1)='Please select one view.'
            IX=1
            do 333 iv=1,indxvew
              write(valt(iv),'(a)') rvewsh(iv)
  333       continue
            CALL EPICKS(IX,IVALV,' ',' Select a view: ',
     &                  28,indxvew,VALT,'View options',IER,3)
            if(IX.ne.0)then
              ipckvew=IVALV(1)
            endif
          endif
        endif

        if (RIFPURP(IRIFFOC)(1:8).eq.'External'.or.
     &      RIFPURP(IRIFFOC)(1:8).eq.'Internal') then

C Generate image if external or internal.
          call genimage(1)
        elseif (RIFPURP(IRIFFOC)(1:5).eq.'Glare') then

C Extra calculations for glare.
          call genimage(2)
          call getglr
        elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then

C Daylight factors - calculate and display.
          call getdf
        endif

      elseif(INO.eq.17)then

C Based on purpose view DF or illuminance or daylight coef or image.
        if (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef'.or.
     &      RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
          call usrmsg('No results display for this option.','  ','W')
          goto 10
        elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        
C Check for daylight factor results. Note: the results could have been
C shifted to a *.df- file so check that as well.
          ITA1 = IFIL+6
          lnrp=lnblnk(runpath)
          write(df,'(2a)')SCENERT(NRIF)(1:lnblnk(SCENERT(NRIF))),'.df'
          write(ltmp,'(2a)')runpath(1:lnrp),df(1:lnblnk(df))
          call ERPFREE(ITA1,ISTAT)
          call FPOPEN(ITA1,ISTAT,1,1,ltmp)
          if(ISTAT.eq.0)then
C debug	    write(6,*) 'found  ',ltmp
            continue
          else
            write(df,'(2a)')SCENERT(NRIF)(1:lnblnk(SCENERT(NRIF))),
     &        '.df-'
            write(ltmp,'(2a)')runpath(1:lnrp),df(1:lnblnk(df))
            call ERPFREE(ITA1,ISTAT)
            call FPOPEN(ITA1,ISTAT,1,1,ltmp)
            if(ISTAT.eq.0)then
C debug	      write(6,*) 'found  ',ltmp
              continue
            else
              call edisp(iuout,
     &          'Could not find the daylight factors file.')
              call edisp(iuout,ltmp)
              goto 10
            endif
          endif

C Read the DF data and calculate averages.  
          NDFP=0
          AVEDF=0.
          DFMIN=10000.
          if(ISTAT.eq.0)then
  43        CALL STRIPC(ITA1,OUTSTR,0,ND,0,'DF results',IFER)
            if (IFER.eq.0) then
              NDFP=NDFP+1
              K=0
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid x',IER)
              DFPTS(NDFP,1)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid y',IER)
              DFPTS(NDFP,2)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid z',IER)
              DFPTS(NDFP,3)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','DF',IER)
              DFVALS(NDFP)=XTMP
              AVEDF=AVEDF+XTMP
              DFMIN=amin1(DFMIN,XTMP)
              goto 43
            endif
            call ERPFREE(ITA1,ISTAT)
            AVEDF=AVEDF/real(NDFP)
            write(outs,'(a,f5.2,a)')'Average daylight factor ',AVEDF,'%'
            call edisp (iuout,outs)
            write(outs,'(a,f5.2)')'Uniformity ratio (min/ave) ',
     &                                  DFMIN/AVEDF
            call edisp (iuout,outs)
            call DFGRIDLBL('d')
          endif
          goto 10
        endif

C Check for existing file names based on available view and picture info.
        IX=1
        npics=0
        do 3331 iv=1,indxvew
          XST=.false.
          write(pf,'(5a)') runpath(1:lnrp),
     &      picfil(1:lnblnk(picfil)),'_',
     &      rvewsh(iv)(1:lnblnk(rvewsh(iv))),'.pic'
          INQUIRE (FILE=pf,EXIST=XST)
          if (XST) then
            npics=npics+1
            write(valt(npics),'(a)') rvewsh(iv)
            IPVALV(npics)=iv
          else
            write(6,*) 'file ',pf(1:lnblnk(pf)),' not found...'
          endif
 3331   continue
        npics=npics+1
        write (valt(npics),'(a)') 'Other '

C Now have list of possible images, ask which one to display.
C If there are no images then re-display the menu.
        if (npics.gt.1) then
          H(1)='Please select one view.'
          CALL EPICKS(IX,IVALV,' ',' Select a view point: ',
     &                28,npics,VALT,'View options',IER,1)
          if(IX.eq.0) goto 10
        else
          goto 10
        endif
        if (IVALV(1).eq.(npics+1)) then
          write(pf,'(a)') 'UNKNOWN.pic'
          write(gf,'(a)') 'UNKNOWN.glr'
        else
          ipckvew=IPVALV(IVALV(1))

C Generate default file names.
          write(pf,'(5a)') runpath(1:lnrp),
     &      picfil(1:lnblnk(picfil)),'_',
     &      rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),'.pic'
          write(gf,'(5a)') runpath(1:lnrp),
     &      picfil(1:lnblnk(picfil)),'_',
     &      rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),'.glr'
        endif

C Check chosen image file name.
        CALL ERPFREE(ITA2,ISTAT)
        call FINDFIL(pf,XST)
        if (.not.XST) then
          call usrmsg('Cannot find picture file: ',pf(1:lnblnk(pf)),'W')
          goto 10
        endif

C Get glare info file name (if necessary) and display image.
        if (RIFPURP(IRIFFOC)(1:5).eq.'Glare') then
          CALL ERPFREE(ITA2,ISTAT)
          call FINDFIL(gf,XST)
          if (.not.XST) then
            call usrmsg('Cannot find glare file: ',gf(1:lnblnk(gf)),'W')
            goto 10
          endif
          call dispimage(2)
        else
          call dispimage(1)
        endif

      elseif(INO.eq.18)then

C Save scene information.
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
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
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

C Path to model.
      common/rpath/path
      common/uhome/upath
      common/expath/runpath,pathtype
      common/rcmd/LCMDFL
      common/user/browse

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/grdisk/grdd,grcx,grcy
      common/sky1/rgrfl,isky,irdoy,rtime,iryear

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/GTFIL/GTGEOM

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      character path*72,cfgroot*24,runpath*72,pathtype*24,GTGEOM*72
      CHARACTER LCMDFL*144,LCFGF*72
      character outs*124,fs*1,tldir*72,upath*72,MODE*4

      LOGICAL MODIFY,MODLEN,MODBND,unixok
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical browse  ! as set in e2r command line.
      integer lnrp  ! length of run path
      integer numberofzones ! to pass to updwire rather than ncomp

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
      MODE='NONE'
      call ERSYS(LCFGF,IFCFG,IAIRP,MODE,0,IER)
      if(IER.eq.0)then
        call inqu_tmc
        CFGOK=.TRUE.
        rgrfl=GREF
        NZONES=NCOMP
        ITRC=0
        ISFSUM=0
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
        IF(MMOD.EQ.8)then
          numberofzones=ncomp
          call updwire(numberofzones)
        endif
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
        CALL ADJVIEW(IER)

C At this point we know the model bounds. Set the ground disk
C diameter and centre.
        XD=(XMX-XMN)**2 + (YMX-YMN)**2
        grdd=SQRT(XD)
        grcx=XMN+((XMX-XMN)/2.)
        grcy=YMN+((YMX-YMN)/2.)

C Decide where the radiance model will be held while calculations
C are underway. There are several options:
C a) Use the traditional Unix/Linux approach and copy the files
C    to a temporary folder, work there and then copy it back.
C    This is necessary if the user is browsing (e.g. does not
C    own the model).
C b) Keep the radiance files in the models rad folder and work
C    there. If native windows then this option should be used.
C    If the user owns the model then they could also work in
C    the model rad folder.
C c) The model uses a single folder and there is no separate
C    rad folder to work with.
        call isunix(unixok)
        if(unixok)then

C If unix and browsing get PID for use in /tmp directory structure.
          if(browse)then
            call esppid(ipid)
            write (tldir,'((5a,i7,a))') upath(1:lnblnk(upath)),fs,
     &                                '.',fs,'.rad',ipid,fs
            call st2file(tldir,runpath)
            lnrp=lnblnk(runpath)
            write (outs,'(a,a)') 'mkdir ',runpath(1:lnrp)
            call runit(outs,'-')
            pathtype='radinhome'
            call edisp(iuout,' Currently using runpath in user home')
          else
	  
C If user owns the model then use the ../rad or ./cfg folder so no
C need to create a folder.
            if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
              pathtype='radincfg'
            elseif(radpth(1:6).eq.'./rad '.or.
     &             radpth(1:6).eq.'.\\rad ')then
              pathtype='radinrad'
            endif
            write(runpath,'(2a)') radpth(1:lnblnk(radpth)),fs
            lnrp=lnblnk(runpath)
          endif
        else
          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
            pathtype='radincfg'
          elseif(radpth(1:6).eq.'./rad '.or.
     &           radpth(1:6).eq.'.\\rad ')then
            pathtype='radinrad'
          endif
          write(runpath,'(2a)') radpth(1:lnblnk(radpth)),fs
          lnrp=lnblnk(runpath)
        endif
        write(outs,'(2a)') 'The place where rad will be invoked is ',
     &     runpath(1:lnrp)
        call edisp(iuout,outs)
      endif

      return
      end


C *************** VIFILE ***************
C VIFILE Browse or manually edit a text file.
C opt is b/B for browsing, e/E for editing.
C lfile contains path as well as file name.
      subroutine vifile(ifu,lfile,opt,ier)
      
      integer lnblnk  ! function definition

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
C Open the common databases - materials, multilayer constructions,
C and optical db.
      subroutine opendb(ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Material data arrays.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      LOGICAL    XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,close
      DIMENSION ICLN(30)
      character SOPT*12,outs*124,GDESCR*36

      IER=0

C Open currently named materials database.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by with changing
C this to 24 words wide.
      CALL ERPFREE(IFMAT,ISTAT)
      call EFOPRAN(IFMAT,LFMAT,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(a,a,a)') ' No materials database ',
     &     LFMAT(1:lnblnk(LFMAT)),' found.'
        call edisp(iuout,outs)
        MATDBOK=.FALSE.
      else

C Also check to see if the file has correct info. 
        IREC=601
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,
     &    (ICLN(I),I=1,30)
        if(NCLASS.LE.0)then
          goto 1000   ! no classifications so try alternatives
        else
          MATDBOK=.TRUE.
          matver = 0.0
          goto 139  !  a binary file found so carry on.
        endif
 1000   CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
        call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
          if(ier.ne.0)then
            call usrmsg('No readable materials database was found',
     &                  'or file was corrupt','W')
            MATDBOK=.FALSE.
          else
            call eclose(matver,1.1,0.001,close)
            if(close)then
              call edisp(iuout,' Filled material data arrays...')
              MATDBOK=.TRUE.
            else
              call usrmsg(
     &          'No readable material database was found. ',
     &          'check other warnings for advise.','W')
              MATDBOK=.FALSE.
            endif
          endif
        elseif(ier.eq.0)then
          call eclose(matver,1.1,0.001,close)
          if(close)then
            call edisp(iuout,' Filled material data arrays...')
            MATDBOK=.TRUE.
          endif
        endif
      endif
        
C Read multilayer db information into common.
 139  CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,ITRU,IER)
        IF(IER.NE.0)then
          write(outs,'(a,a,a)') ' Problem with multilayer constr db ',
     &     LFMAT(1:lnblnk(LFMAT)),'... '
          call edisp(iuout,outs)
          MLDBOK=.FALSE.
        else
C          call edisp(iuout,' opened multilayer constructions db.')
          MLDBOK=.TRUE.
        endif
      else
        write(outs,'(a,a,a)') ' No multilayer constructions db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found.'
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

C geometry.h provides commons G0 and G2.
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      common/radabs/NABS

      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character outs*124

      logical XST
      logical newgeo  ! to use for testing if new/old geometry file.

      ITA1=IFIL+6

C NABS = number of alternative blind states.
      NABS=0
      newgeo=.false.  ! assume older format geometry.

C Loop through zones checking if a tmc file has been referenced.
      do 10 ICOMP=1,NCOMP
        if (ITW(ICOMP).eq.1) then

C Rescan zone Construction and TMC file 
C (need to test blind/ shuter control flags).
          call FINDFIL(LTHRM(ICOMP),XST)
          if(XST)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(ITA1,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
            else
              call egomin(ITA1,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
            endif
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
