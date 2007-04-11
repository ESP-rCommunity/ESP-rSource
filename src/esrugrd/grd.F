C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C *********************************************************************
C This file contains the following subroutines:

C INITPF is the main program for multi-dimensional gridding of the  
C        solid domain (air volumes gridding is not included).
C INTGRD initializes the gridding process by reading in the required 
C        geometry and construction data.

C ***************************  initpsfz  ********************************
C INITPFZ is the main program for multi-dimensional gridding of the  
C solid domain (air volumes gridding is not included).
C << calling routine could focus - 1D/3D ground etc. so the necessary
C << file names can be in place.
      program grd
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)

      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

C Path to problem
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/rcmd/LCMDFL
      common/rzone/inzone

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl

      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      LOGICAL OK,MODIFY,MODBND,MODLEN,XST,GRDOK

      DIMENSION ITEM(12)
      CHARACTER H*72,LFILE*72,LCFGF*72
      CHARACTER OUTSTR*124,ITEM*24,path*72,inf*144,LCMDFL*144
      character upath*72,esprc*72,cfgroot*24,outs248*248,L144*144
      character dfdblbl*20,defdbfl*72
      character inz*16,inzone*16,outs*124,MODE*4

C Initialize global common data.
      call ezero
      call curmodule('grd ')
      ITRC=0
      IUOUT=6
      IUIN=5
      IFIL=11
      LIMTTY=8
      LIMIT =8
      IFS=1
      ITFS=0
      IMFS=1
      LIMTTY=20
      NSIGFIG=3
      path='./'
      IDRW1=0
      call parsfz(MODL,iappw,iappx,iappy,inf,inz)

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, updated geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1.
      IFMAT=IFIL+4
      IFCFG=IFIL+5
      IUF=IFIL+6
      ICVS=IFIL+8
      ICNC=IFIL+9
      INDC=IFIL+10
      ITAQ=IFIL+11

C Assume that the configuration and geometry files have not been read in.
      CFGOK=.FALSE.
      GRDOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      MODIFY=.FALSE.

C Initial coords for eyepoint, viewing point, angle of view.
      EYEM(1)=-100.
      EYEM(2)=-100.
      EYEM(3)=100.
      VIEWM(1)=10.
      VIEWM(2)=10.
      VIEWM(3)=10.
      ANG=40.

C General image option flags.
      ITDSP=1
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=1
      ITVNO=1
      ITORG=1
      ITSNR=0
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0

C Initialise output device, assume minimal trace, and a smallish
C display.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=510
        iappx=60
        iappy=60
      else
        if(iappx.le.0)iappx=60
        if(iappy.le.0)iappy=60
        if(iappw.le.200)then
          iappwi=int(510*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(28*iappw*0.01),12)
        LIMTTY=8
        LIMIT =8
      else
        menuchw = 28
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
     &'ESP-r Construction Gridding: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,3,2,2,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
        call opentutorial
        call opensetup
        call updwire(0)
        call updcapt(1)
        call updazi(1)
#endif
        call setzscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

      CALL EDISP(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Construction Gridding: Version 2.6a of January 2006.',
     &  ' Copyright '
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' 2001-6 Energy Systems Research Unit, University of',
     & ' Strathclyde,'
      call edisp(IUOUT,outs)
      CALL EDISP(IUOUT,' Glasgow Scotland. ')
      CALL EDISP(IUOUT,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),'/.','esprc'
      call scesprc(esprc,IFIL+1,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take command line file name as initial configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      else
        LCMDFL='  '
      endif

C Take command line zone name and
C determine if focus should be to a single zone.
      if(inz(1:1).ne.' ')then
        inzone=inz
        write(outstr,'(a,a)')' the input zone is: ',inzone
        call edisp(iuout,outstr)
      else
        inzone='ALL'
      endif

C If an input file has been specified then load it.
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then

C Find the path and local file name.
          CALL FDROOT(LCMDFL,PATH,LCFGF)
          CALL EDISP(IUOUT,' Scanning the problem description')
          MODE='NONE'
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,ITRC,IER)
          IF(IER.EQ.0)THEN
            NZONES=NCOMP
            CALL ZDATA (itrc,IER,NZONES,ISFSUM)
            CFGOK=.TRUE.
            call updwire(ncomp)

C Initialize the 1D gridding variables.
            CALL INTGRD(GRDOK,ITRC)
            IF(GRDOK)THEN

C Set flag so that bounds are checked.
              DO 30 ICOMP=1,NCOMP
                ZBFLG(ICOMP)=0
   30         CONTINUE
              CFGOK=.TRUE.
              MODIFY=.TRUE.
              MODLEN=.TRUE.
              MODBND=.TRUE.
            ENDIF
          ELSE
            CFGOK=.FALSE.
            IF(MMOD.EQ.8)CALL startbuffer()
          ENDIF
        endif
      endif

      ITRC=0
   10 INO=-3
      ITEM(1)='1 problem definition    '
      ITEM(2)='  --------------------- '
      ITEM(3)='a 1D gridding           '
      ITEM(4)='b Multi-gridding        '
      ITEM(5)='c ground-modelling      '
      ITEM(6)='d moisture 1D gridding  '
      ITEM(7)='  --------------------- '
      ITEM(8)='l look at ASCII file    '
      IF(ITRC.EQ.0)THEN
        ITEM(9)='r reporting >> silent  '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(9)='r reporting >> summary ' 
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(9)='r reporting >> verbose '
      ENDIF
      ITEM(10)='* image control         '
      ITEM(11)='? help                  '
      ITEM(12)='- Exit gridding module  '
      MITEM=12

C If user has defined problem and perhaps resized the display then
C redraw the problem image.
      IF(CFGOK.AND.(MODIFY.OR.IDRW1.NE.1))THEN
        ITZNM=0
        ITVNO=1
        IDRW1=1
        IDRW2=0
        CALL GRAAPH(IDRW1,IDRW2)
      ENDIF

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('grd ',1)

      CALL EMENU('   Building-Gridding',ITEM,MITEM,INO)

      IF(.NOT.CFGOK.AND.(INO.GE.3.AND.INO.LE.6))THEN

C Check if the problem is defined.
        CALL USRMSG(' ','the problem should be defined first.','W')
      ELSEIF(INO.EQ.1)THEN

C Define the problem.
        H(1)='The system configuration file holds the definition of'
        H(2)='the building/plant to be simulated, including the'
        H(3)='names of all of the files required. If the name given'
        H(4)='matches an existing file it will be read in.'
        H(5)=' '
        L144=LCMDFL

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  289   llt=lnblnk(L144)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(L144,' Model configuration file?',' ',96,' ',
     &        'config file name',IER,5)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(L144,' Model configuration file?',' ',124,' ',
     &        'config file name',IER,5)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(L144,' Model configuration file?',' ',144,' ',
     &        'config file name',IER,5)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(L144,' Model configuration file?',' ',144,' ',
     &      'config file name',IER,5)
        else
          CALL EASKF(L144,' Model configuration file?',' ',96,' ',
     &      'config file name',IER,5)
        endif

        IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 289
        call st2file(L144,LCMDFL)

C Find the path and local file name.
        CALL FDROOT(LCMDFL,PATH,LCFGF)
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,' Scanning the problem description')
        MODE='NONE'
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,ITRC,IER)
        IF(IER.EQ.0)THEN

C Initialize the 1D gridding variables.
          NZONES=NCOMP
          CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
          CALL INTGRD(GRDOK,ITRC)
          IF(GRDOK)THEN
            CFGOK=.TRUE.
            MODIFY=.TRUE.
            MODLEN=.TRUE.
            MODBND=.TRUE.

C Set flag so that bounds are checked.
          DO 20 ICOMP=1,NCOMP
            ZBFLG(ICOMP)=0
   20     CONTINUE
          ENDIF
        ELSE
          CFGOK=.FALSE.
          IF(MMOD.EQ.8)CALL startbuffer()
        ENDIF
      ELSEIF(INO.EQ.3)THEN

C Display 1D gridding menu.
        CALL MENU1D
      ELSEIF(INO.EQ.4)THEN

C Display Multi-gridding menu.
        CALL MENU3D(ITRC)
        MODIFY=.TRUE.
      ELSEIF(INO.EQ.5)THEN

C Display the ground modelling menu.
        CALL MENUGM(IDRW1)
      ELSEIF(INO.EQ.6)THEN

C Display the moisture 1D gridding menu.
        CALL MENU1M(IDRW1,IDRW2,ITRC)
      ELSEIF(INO.EQ.MITEM-4)THEN

C List out an ASCII file.
        CALL EASKS(LFILE,' File to look at ? ',
     &     ' ',72,' ','ascii file',IER,0)
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,LFILE,1,IER)
        IF(IER.EQ.0)THEN

C Actual read of ASCII file.
   40     READ(IUNIT,'(A124)',IOSTAT=ISTAT,END=50)OUTSTR
          CALL EDISP(IUOUT,OUTSTR)
          GOTO 40

C End of file encountered, close the file and return to menu.
   50     CALL USRMSG(' ',' End of file reached','-')
          CALL ERPFREE(IUNIT,ISTAT)
        ELSE
          CALL USRMSG(' ','Problem opening file... ','W')
        ENDIF
      ELSEIF(INO.EQ.MITEM-3)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO.EQ.MITEM-2)THEN

C Image control.
C Setting izgfoc=0 denotes that not in single zone edit mode.
        nzg=NCOMP
        DO 60 I=1,nzg
          nznog(I)=I
  60    CONTINUE

C (Re)Set all surfaces to std line width.
        CALL INLNST(1)
        izgfoc=0
        CALL EVSET(ITRC,'-',IER)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('grd ',0)
      ELSEIF(INO.EQ.MITEM)THEN

C Exit program.
        call usrmsg(' Closing building gridding...',' ','P')
        IF(MATDBOK)CALL ERPFREE(IFMAT,ISTAT)
        CALL EPAGEND
        STOP
      ENDIF
      GOTO 10
      END

C **************************     INTGRD    ****************************
C INTGRD initializes the gridding process by reading in the required 
C geometry and construction data. It also initialises ground 
C coefficients.
C *********************************************************************
      SUBROUTINE INTGRD(GRDOK,ITRC)
#include "building.h"
#include "esprdbfile.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D02/IADP,IVAR,FMIN,FREF,FMAX,NSTP
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)
      COMMON/GRND07/VERTX(6,4)

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL GRDOK
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LUTIL,LTHRM
      CHARACTER VERTX*2

      GRDOK=.true.
      IUNIT=IFIL+1
      FMIN=0.45
      FREF=0.5
      FMAX=0.55
      NSTP=1
      IADP=0
      IVAR=0
      BCNV=5.0
      NNDS=0
      DO 10 ICOMP=1,NCOMP
        CALL EGOMIN(IUNIT,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
        IF(IER.NE.0)THEN
          GRDOK=.FALSE.
          RETURN
        ENDIF
        CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,ITRC,IUOUT,IER)
        IF(IER.NE.0)THEN
          GRDOK=.FALSE.
          RETURN
        ENDIF
        CALL MZMISC(ICOMP)
        CALL NODTWO(ICOMP)
        NNDS=NNDS+NNDZ(ICOMP)
   10 CONTINUE

C If primitive database not opened, do it now.
      IF(.NOT.MATDBOK)THEN
        IER=0
        CALL EFOPRAN(IFMAT,LFMAT,40,1,IER)
        IF(IER.NE.0)RETURN
        MATDBOK=.TRUE.
      ENDIF

C Initialise ground coefficients.
      NDXG=1
      DXG(1)=50.
      NDYG=1
      DYG(1)=50.
      NDZG=1
      DZG(1)=20.
      DO 20 IL=1,MGXYZ
      DO 20 IY=1,MGXYZ
      DO 20 IX=1,MGXYZ
        ITPGND(IX,IY,IL)=263
   20 CONTINUE
      WRITE(VERTX(1,1),'(A1)')'1'
      WRITE(VERTX(1,2),'(A1)')'2'
      WRITE(VERTX(1,3),'(A1)')'6'
      WRITE(VERTX(1,4),'(A1)')'5'
      WRITE(VERTX(2,1),'(A1)')'2'
      WRITE(VERTX(2,2),'(A1)')'3'
      WRITE(VERTX(2,3),'(A1)')'7'
      WRITE(VERTX(2,4),'(A1)')'6'
      WRITE(VERTX(3,1),'(A1)')'3'
      WRITE(VERTX(3,2),'(A1)')'4'
      WRITE(VERTX(3,3),'(A1)')'8'
      WRITE(VERTX(3,4),'(A1)')'7'
      WRITE(VERTX(4,1),'(A1)')'4'
      WRITE(VERTX(4,2),'(A1)')'1'
      WRITE(VERTX(4,3),'(A1)')'5'
      WRITE(VERTX(4,4),'(A1)')'8'
      WRITE(VERTX(5,1),'(A1)')'5'
      WRITE(VERTX(5,2),'(A1)')'6'
      WRITE(VERTX(5,3),'(A1)')'7'
      WRITE(VERTX(5,4),'(A1)')'8'
      WRITE(VERTX(6,1),'(A1)')'4'
      WRITE(VERTX(6,2),'(A1)')'3'
      WRITE(VERTX(6,3),'(A1)')'2'
      WRITE(VERTX(6,4),'(A1)')'1'
      RETURN
      END

C ***** Dummy display of images.
      subroutine imgdisp(iforce,focus,ier)
      character focus*4

      return
      end

C CTLEXP *********** ( same as prj version )
C CTLEXP: Control feedback to export wireframe or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T, X, or G specifying text, tabular or graphic 
C   info being saved.
C xfile (width variable) is the initial name of the file to be exported
C msg (width variable) is prepended to dialog as export happens
C ixopen is a toggle reset each time ctlexp is called to signal open or close file.
C ixunit is the file unit number for the export file.
C ixloc signals whether path will be local or remote ??
      subroutine ctlexp(xfile,ixopen,ixloc,ixunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rpath/path

      character*(*) msg,xfile
      character H*72,path*72,outs*124,tg*1,tfile*72
      character ixpfile*72
      character longtfile*144,longtfiledos*144
      logical concat,remote,unixok
      integer lenxfile

C Initial name for export parameter file is xfile+par.
      lenxfile=lnblnk(xfile)
      if(xfile(1:1).ne.' '.and.lenxfile.gt.2)then
        write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Each call is a toggle. If open then close text or graphics command
C file. For the graphics ask if file should be further converted.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs,'(a,a)') 'closing export file: ',xfile
        call edisp(iuout,outs)
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
        elseif(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
        elseif(tg.eq.'G')then
          if(ixloc.eq.1)then
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
          else
            write(longtfile,'(a)') xfile(1:lnblnk(xfile))
          endif
          call wwcsetend
          call wwcclose(longtfile)
        endif
      elseif(ixopen.eq.1)then
        ixloc=0
        remote=.false.
        call isunix(unixok)
        if(unixok)then
          if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')remote=.true.
        else
          if(path(1:2).ne.'  ')remote=.true.
          if(ichar(path(1:1)).eq.46.and.
     &       ichar(path(2:2)).eq.92)remote=.false.
        endif
        if(remote)then
           write(outs,'(A,A)') ' The current path is: ',path
           call edisp(iuout,outs)
         h(1)='If using path then the export file will be placed in'
         h(2)='the model folder. Otherwise nominate a folder and file'
         h(3)='name for the export file (i.e. /tmp/junk.exp or'
         h(4)='/home/ralph/reports/junk.exp) '
         h(5)=' '
         h(6)='If you selected this option in error click continue.'
           write(outs,'(A,A)') ' The model is in folder ',path
           CALL EASKABC(outs,' place export file:',
     &       'in the model folder','user defined folder','continue',
     &       ixloc,6)
        else
          ixloc = 2   ! ixloc needs to be non-zero for subsequent code to work.
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        call easks(xfile,' export file name: ',' ',72,
     &    ' ','export file name',IER,2)

        write(outs,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                       ' for export.'
        call edisp(iuout,outs)
        if(ixloc.eq.1)then
          if(tg.eq.'X')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'T')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to wwlib.c
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            call edisp248(iuout,longtfile,100)
            call edisp248(iuout,xfile,100)
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        elseif(ixloc.eq.2)then
          if(tg.eq.'T')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'G')then
            tfile=xfile   ! << this needs to change if xfile not 72 char >>
            call wwcopen(tfile)
            call wwcsetstart
          endif
        elseif(ixloc.eq.3)then
          return
        endif
        write(outs,'(1x,3A)') msg(:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call edisp(iuout,outs)
      endif
      RETURN
      END

