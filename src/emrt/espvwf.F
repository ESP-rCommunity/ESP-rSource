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

C ESRU: espvwf

C Revised to take in configuration file and zone focus.
      program espvwf
      USE START_UP
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/c20/precz.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "prj3dv.h"

C IEEE callback (Solaris)
#ifdef SUN
#ifdef F90
      external SIGFPE_bps      
      integer SIGFPE_bps      
#endif
#endif
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appw/iappw,iappx,iappy
      COMMON/FILEP/IFIL
      COMMON/FIL/IOFIL,IFPOL
      COMMON/FATRIB/ATR

      COMMON/MRT2/GEOLOAD,MRTLOAD
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      COMMON /CELLS/ NCELL(MSM), NCPT(MSM), IGFACT
      COMMON /BUBXYZ1/ NPATCH, NBUB
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      common/tmrt1/tmrt,temperr
      common/tmrt2/temp(ms),tempmrt
      common /cube5/fstgrp,fstsur,lstsur,strtgr

C Path to model.
      common/user/browse
      common/rpath/path
      common/rcmd/LCMDFL
      character runpath*72  ! path for working files
      common/expath/runpath

      integer lnrp  ! length of runpath
      common/expathl/lnrp

C Indicator of possible focus zone.
      common/rzone/inzone
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12      
      
      LOGICAL     CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      integer fstgrp,fstsur,lstsur,strtgr
      logical browse

      DIMENSION ITEM(19)
      LOGICAL GEOLOAD,MRTLOAD,tmrt,temperr,ATR,context
      LOGICAL XST,unixok,there
      character outs*124,outs248*248,MODE*4
      CHARACTER OUTSTR*124,ITEM*29
      CHARACTER DFILE*72,inf*144,LCMDFL*144
      CHARACTER VFILE*72,ZN*12,LTMP*72,L144*144,LTMPFL*72
      character path*72
      character journcmd*20,jfile*72,uname*24
      character inz*16,inzone*16,fs*1,cjfile*72

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call
      character ppath*72,filen*72

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer numberofzones ! to pass to updwire rather than ncomp
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture,iiazi    ! to pass to updcapt updazi
#else
      integer*8 numberofzones
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture,iiazi    ! to pass to updcapt updazi
#endif
      integer NITEMS,INO ! max items and current menu item
      integer IGFACTold,NPATCHold ! local vars to remember grid/patch settings

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Common block variable meanings:
C     NSURMF  -  total number of surfaces
C     NGRID  -  number of grid cells
C     NTRPOL -  total number of records in polygon file

C     PEQN   -  plane equation of surface
C     PCG    -  centroid of surface

C     NCELL  -  number of grid cells in surface
C     NCPT   -  pointer to first cell in cell list

C     ISPNT(1,)  -  pointer to polygon in polygon file
C     ISPNT(2,)  -  pointer to transf. polygon in polygon file
C     ISPNT(3,)  -  pointer to transformation matrix
C     ISPNT(4,)  -  pointer to reverse transf. matrix
C     ISPNT(5,)  -  pointer to bound box polygon in polygon file

C     FF     -  surface form factor matrix
C     FFL    -  master surface form factor matrix for all sensors
C     FFS    -  solid angle of each cell

C     IVIS   -  surface visibility matrix
C     IARECT -  surface rectangle matrix (filled in checkrec.f)

C     IDG   -  surface identity ie. parent surface
C     XC    -  x co -ordinate of cell centre
C     YC    -  y co-ordinate of cell centre
C     ZC    -  z co-ordinate of cell centre

C     NPATCH  - Number of patches in one patch-ring
C               There are NPATCH/4 rings on a bubble
C     NBUB    - Number of patches on a bubble (=1/4(NPATCH**2)

C     surarea(i)        : Area of cube-surfaces [m**2]
C     fstgrp            : The first grid point on the cube.
C     tmrt   (logical)  : True if tmrt-calculations needed
C     temp(i)           : Temperature for surface i. [C]

C Initialize global common data.
      call ezero
      call curmodule('mrt ')
      cAppName = 'mrt'
      helpinapp='mrt'  ! set once for the application
      helpinsub='mrt'  ! set for MAIN
      IUIN=5
      IUOUT=6
      LIMTTY=24
      LIMIT =24
      IFIL = 10
      NSIGFIG=3
      matver=0.0   ! initial assumption of binary materials database

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, updated geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      IUF = IFIL+1
      IFPOL = IFIL+2
      IFCFG=IFIL+5
      IAIRP = IFIL+6
      iuj=IFIL+10
      ITRC=1
      IFS=1
      ITFS=0
      IMFS=1
      ATR=.TRUE.
      ZN='UNKNOWN'
      LCFGF='UNKNOWN'
      ICOMP=-1
      call parsfz(MODL,iappw,iappx,iappy,inf,inz)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

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
      ITSNM=0
      ITVNO=1
      ITORG=1
      ITSNR=0
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0

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

c Initialise output device, assume minimal trace.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=530
        iappx=75
        iappy=130
      else
        if(iappx.le.0)iappx=75
        if(iappy.le.0)iappy=130
        if(iappw.le.200)then
          iappwi=int(530*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(32*iappw*0.01),16)
        LIMTTY=10
        LIMIT =10
      else
        menuchw = 32
        LIMTTY=10
        LIMIT =10
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

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'ESP-r View Factors ',
     &  cVnum(1:lnblnk(cVnum))

      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle)

C Open the text display box equal to LIMTTY if MMOD = 8.
C If starting as a reduced percentage of default then widen the
C graphic display to compensate.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=4; iiw2=2; iiw3=2; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
C        write(6,*) 'igw igwh igl igb',igw,igwh,igl,igb
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          numberofzones=0
          call updwire(numberofzones)  ! pass local integer
          iicapture=1; iiazi=1
          call updcapt(iicapture)
          call updazi(iiazi)
        endif
        call setzscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()

C Debug.
C        write(6,*) 'mdispl,ifgrey,ncset,ngset,nzonec ',
C     &    mdispl,nifgrey,ncset,ngset,nzonec
      ELSE
        
C Set reasonable defaults for text mode.
        igw=434; igwh=223; igl=41; igb=257
      ENDIF
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Remember where vwf was started (pwdinitial). If within model
C cfg folder then set pwdtocfg to !
      pwdinitial='  '  ! clear common
      pwdtocfg='  '    ! and from pwd into cfg folder
      call usrdir(pwdinitial)
      lnpwdi=lnblnk(pwdinitial)
      write(6,*) 'vwf the pwd command is ',pwdinitial(1:lnpwdi)
C      write(6,*) lnpwdi,lnpwdi-2,' ',pwdinitial(lnpwdi-2:lnpwdi)
      if(pwdinitial(lnpwdi-2:lnpwdi).eq.'cfg')then
        pwdtocfg='!'             ! signal no need to append this
        lnpwdc=lnblnk(pwdtocfg)  ! remember its length
        write(6,*) 'vwf working in model cfg folder'
      endif

C Find the user's home folder then get user's custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IUF,0,IIER)

C If there is a journal active set up to add to the journal. First
C get the root journal file (setup by prj) from the users home folder.
C All subsequent tstamp messages will be appended to root journal.
      if(journio.eq.1)then
        uname=' '
        call usrname(uname)
        cjfile=' '
        write(cjfile,'(5a)')upath(1:lnblnk(upath)),fs,'.',
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

        write(outs,'(a,a)')'Journal (viewfactor analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Scan the defaults file silently for default configuration.
      call escdef(IUF,'s',IER)

C Take command line file name as configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      else
        LCMDFL='  '
      endif

C Take command line zone name and determine if the focus should
C be to a single zone or ALL zones (e.g. not yet specified).
      if(inz(1:1).ne.' ')then
        write(inzone,'(a)') inz
        call edisp(iuout,'  ') ! echo a blank line
        write(outs,'(a,a)')'The input zone is: ',inzone
        call edisp(iuout,outs)
        call tstamp('>',outs)
      else
        inzone='ALL'
      endif

C Clear VEWF array.
      DO 331 I = 1,MS
        DO 332 J=1,MS
          VEWF(I,J)=0.
  332   CONTINUE
  331 CONTINUE

C Initilize surface temperatures and emisivities.
C << Should get eimisivities from mlc file. >>
      DO 55 IT=1,MS
        temp(IT)=20.0
        EMISI(IT)=0.9
   55 CONTINUE

c Initial assumptions on griddivision and patch division.
      IGFACT = 10
      NPATCH=40

      nsurmf = 0
      NGRID = 0
      NTRPOL = 0
      NCUBF = 0   ! ?? when is geometry scanned ??
      MRTLOAD=.FALSE.
      GEOLOAD=.FALSE.

C If an input file has been specified then load it. If this is
C successful then scan all the zones via ZDATA.
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then

C See if there is a pwdtocfg which can be recovered from within
C the command line file.
          call fdpwdtocfg(LCMDFL,ppath,filen)
          write(6,*) 'pwdtocfg ',pwdtocfg(1:lnblnk(pwdtocfg))
C          write(6,*) 'pp ',ppath(1:lnblnk(ppath))
C          write(6,*) 'fi ',filen(1:lnblnk(filen))

          call fdroot(LCMDFL,path,LCFGF)
          call edisp(iuout,' ')
          call edisp(iuout,' Scanning the model description')
          MODE='ALL '
          CALL ERSYS(LCFGF,IFCFG,IAIRP,MODE,0,IER)
          if(IER.eq.0)then

C Where is zones folder in relation to where vwf was started?
            if(pwdtocfg(1:1).eq.'!')then
              write(runpath,'(2a)') zonepth(1:lnblnk(zonepth)),fs
              lnrp=lnblnk(runpath)  ! remember this
            else
              write(runpath,'(3a)') pwdtocfg(2:lnblnk(pwdtocfg)),
     &          zonepth(1:lnblnk(zonepth)),fs
              lnrp=lnblnk(runpath)  ! remember this
C              write(6,*) 'lnrp ',lnrp,pwdtocfg
              write(6,*) 'runpath ',runpath
            endif

            CFGOK=.TRUE.
            numberofzones=ncomp
            IF(MMOD.EQ.8)call updwire(numberofzones)
            CALL ZDATA (ITRC,IER,int(numberofzones),ISFSUM)

C Open scratch file now that path is known. Extend the name of the
C vwf file. If this file exists then remove it before opening a
C new scratch file.
            if(browse)then
              call usrmsg('You do not own the current model and so are',
     &                    'not allowed to calculate viewfactors.','W')
              goto 10
            else
              if(unixok)then
                fs = char(47)
                write(LTMPFL,'(3a)') runpath(1:lnrp),
     &            cfgroot(1:lnblnk(cfgroot)),'.mrt'
                INQUIRE (FILE=LTMPFL(1:lnblnk(LTMPFL)),EXIST=XST)
                if(XST)then
                  call edisp(iuout,'Removing existing scratch file. ')
                  call FPRAND(IFPOL,ISTAT,80,1,LTMPFL)
                  call EFDELET(IFPOL,ISTAT)
                endif
                call FPRAND(IFPOL,ISTAT,80,3,LTMPFL)
                if(ISTAT.lt.0)then
                  call edisp(iuout,' Error opening mrt.trace file;')
                  CALL EPAGEND
                  STOP
                endif
                call edisp(iuout,'Opening scratch file...')
                call edisp(iuout,LTMPFL)
              else
                fs = char(92)
C                write(LTMPFL,'(4a)') path(1:lnblnk(path)),fs,
C     &            cfgroot(1:lnblnk(cfgroot)),'.mrt'
                write(LTMPFL,'(4a)') runpath(1:lnrp),fs,
     &            cfgroot(1:lnblnk(cfgroot)),'.mrt'
                INQUIRE (FILE=LTMPFL(1:lnblnk(LTMPFL)),EXIST=XST)
                if(XST)then
                  call edisp(iuout,'Removing existing scratch file. ')
                  call FPRAND(IFPOL,ISTAT,80,1,LTMPFL)
                  call EFDELET(IFPOL,ISTAT)
                endif
                call FPRAND(IFPOL,ISTAT,80,3,LTMPFL)
                if(ISTAT.lt.0)then
                  call edisp(iuout,' Error opening mrt.trace file;')
                  CALL EPAGEND
                  STOP
                endif
                call edisp(iuout,'Opened scratch file...')
                call edisp(iuout,LTMPFL)
              endif
            endif
          endif

C If inzone = All display an image of the model. If inzone is either
C a string which matches a zone name or is an index then focus on
C that zone and update string ZN.  If inzone = UNKNOWN or the 
C index 0 present a list.
          call zindex(inzone,index)
          if(index.gt.0)then
            ic = index
            write(ZN,'(a)') zname(ic)
            goto 42
          endif
        endif
      endif

10    INO = -4
      write(ITEM(1),'(A,A20)')  'a model: ',LCFGF(1:20)
      write(ITEM(2),'(A,A21)')  '  path: ',path(1:21)
      write(ITEM(3),'(A,A12)')  'b zone: ',ZN(1:12)
      ITEM(4) =                 '  __________________________ '
      ITEM(5) =                 '  calculation Parameters  '
      write(ITEM(6),'(A,I4)')   'c   grid division : ',IGFACT
      write(ITEM(7),'(A,I4)')   'd   patch division: ',NPATCH/4
      ITEM(8) =                 '  __________________________ '
      ITEM(9)=                  'e zone view factor calcs.    '
      ITEM(10) =                'f MRT sensor attrib & calcs. '
      ITEM(11)=                 '  __________________________ '
      IF(ITRC.EQ.0)THEN
        ITEM(12)=               'r reporting >> silent        '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(12)=               'r reporting >> summary       '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(12)=               'r reporting >> detailed      '
      ENDIF
      ITEM(13)=                 '! display surface info       '
      ITEM(14)=                 '> update file                '
      ITEM(15)=                 '  __________________________ '
      ITEM(16)=                 '? help                       '
      ITEM(17)=                 '- exit vwf                   '
      NITEMS = 17

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('vwf ',1)

20    CALL EMENU('  Viewfactors & MRT',ITEM,NITEMS,INO)

      IF(INO.EQ.1)THEN
        helptopic='user_supplied_cfg'
        call gethelptext(helpinsub,helptopic,nbhelp)
        L144=LCMDFL

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  289   llt=lnblnk(L144)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(L144,' Model configuration file?',' ',96,DFCFG,
     &        'config file name',IER,nbhelp)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(L144,' Model configuration file?',' ',124,DFCFG,
     &        'config file name',IER,nbhelp)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(L144,' Model configuration file?',' ',144,DFCFG,
     &        'config file name',IER,nbhelp)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(L144,' Model configuration file?',' ',144,DFCFG,
     &      'config file name',IER,nbhelp)
        else
          CALL EASKF(L144,' Model configuration file?',' ',96,DFCFG,
     &      'config file name',IER,nbhelp)
        endif

        IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 289
        call st2file(L144,LCMDFL)

C See if there is a pwdtocfg which can be recovered from within
C the command line file.
        call fdpwdtocfg(LCMDFL,ppath,filen)
        write(6,*) 'pwdtocfg ',pwdtocfg(1:lnblnk(pwdtocfg))
C        write(6,*) 'pp ',ppath(1:lnblnk(ppath))
C        write(6,*) 'fi ',filen(1:lnblnk(filen))

C Find the path and local file name.
        call fdroot(LCMDFL,path,LCFGF)
        call edisp(iuout,' ')
        call edisp(iuout,' Scanning the model description')
        MODE='ALL '
        CALL ERSYS(LCFGF,IFCFG,IAIRP,MODE,0,IER)
        if(IER.eq.0)then

C Where is zones folder in relation to where vwf was started?
          if(pwdtocfg(1:1).eq.'!')then
            write(runpath,'(2a)') zonepth(1:lnblnk(zonepth)),fs
            lnrp=lnblnk(runpath)  ! remember this
          else
            write(runpath,'(3a)') pwdtocfg(2:lnblnk(pwdtocfg)),
     &        zonepth(1:lnblnk(zonepth)),fs
            lnrp=lnblnk(runpath)  ! remember this
C            write(6,*) 'lnrp ',lnrp,pwdtocfg
            write(6,*) 'runpath ',runpath
          endif

          CFGOK=.TRUE.
          numberofzones=ncomp
          IF(MMOD.EQ.8) call updwire(numberofzones)
          CALL ZDATA (ITRC,IER,int(numberofzones),ISFSUM)
          call tstamp('>','VWF: scanning new model')
          call tstamp('>',LCFGF)

C Open scratch file now that path is known.
          if(unixok)then
            fs = char(47)
C            write(LTMPFL,'(4a)') path(1:lnblnk(path)),fs,
C     &        cfgroot(1:lnblnk(cfgroot)),'.mrt'
            write(LTMPFL,'(3a)') runpath(1:lnrp),
     &        cfgroot(1:lnblnk(cfgroot)),'.mrt'
            INQUIRE (FILE=LTMPFL(1:lnblnk(LTMPFL)),EXIST=XST)
            if(XST)then
              call edisp(iuout,'Removing existing scratch file. ')
              call FPRAND(IFPOL,ISTAT,80,1,LTMPFL)
              call EFDELET(IFPOL,ISTAT)
            endif
            call FPRAND(IFPOL,ISTAT,80,3,LTMPFL)
            if(ISTAT.lt.0)then
              call edisp(iuout,' Error opening mrt.trace file;')
              CALL EPAGEND
              STOP
            endif
            call edisp(iuout,'Opening scratch file...')
            call edisp(iuout,LTMPFL)
          else
            fs = char(92)
C            write(LTMPFL,'(4a)') path(1:lnblnk(upath)),fs,
C     &        cfgroot(1:lnblnk(cfgroot)),'.mrt'
            write(LTMPFL,'(4a)') runpath(1:lnrp),fs,
     &        cfgroot(1:lnblnk(cfgroot)),'.mrt'
            INQUIRE (FILE=LTMPFL(1:lnblnk(LTMPFL)),EXIST=XST)
            if(XST)then
              call edisp(iuout,'Removing existing scratch file. ')
              call FPRAND(IFPOL,ISTAT,80,1,LTMPFL)
              call EFDELET(IFPOL,ISTAT)
            endif
            call FPRAND(IFPOL,ISTAT,80,3,LTMPFL)
            if(ISTAT.lt.0)then
              call edisp(iuout,' Error opening mrt.trace file;')
              CALL EPAGEND
              STOP
            endif
            call edisp(iuout,'Opened scratch file...')
            call edisp(iuout,LTMPFL)
          endif
        else
          goto 289
        endif
        goto 10
      elseif(INO.EQ.3)THEN

C Read geometry file for either calculation purpose
        if(.NOT.CFGOK)then
          CALL USRMSG(' ',' Please define the model first!','W')
          GOTO 10
        endif
        IC=-1
        CALL EASKGEOF('Select a zone to assess:',CFGOK,IC,'-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 10
        IER=0
        write(outs,'(a,a)')'VWF: shift zone focus to: ',zname(IC)
        ZN=zname(IC)
        call tstamp('>',outs)
        goto 42
      ELSEIF(INO.EQ.6)THEN

c Change parameter igfact.
        call edisp(iuout,' ')
        WRITE(outs,'(A,I3)') ' Current grid subdivision  = ', IGFACT
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,' Note that the maximum grid subdivision is')
        igx=int(sqrt(real(mgc/nsur)))
        WRITE(outs,'(i3,a,I5,a,i3,a)') igx,' [sqrt(',mgc,
     &    '/nsur)] and nsur is currently ',nsur,'.'
        call edisp(iuout,outs)
        helptopic='grid_subdivision_density'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(IGFACT,' ',' New value ? ',
     &    2,'W',igx,'W',6,'grid factor',IER,nbhelp)
        call tstamp('>','VWF: updated grid factor')
      ELSEIF(INO.EQ.7)THEN

c Change npatch.
        call edisp(iuout,' ')
        call edisp(iuout,' Variable np is the patch subdivision.')
        call edisp(iuout,' Every bubble is divided into np rings')
        WRITE(outs,'(A,I5)')' Current patch subdivision np = ',NPATCH/4
        call edisp(iuout,outs)
        helptopic='patch_subdivision'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(NP,' ',' New value ? ',
     &    1,'F',MPATCH/4,'F',10,'patch subdivision',IER,nbhelp)
        NPATCH=4*NP
        call tstamp('>','VWF: updated patch subdivision')
      ELSEIF(INO.EQ.9)THEN

c Calculate viewfactors for input geometry.
        if(.NOT.CFGOK)then
          CALL USRMSG(' ',' Please define the model first!','W')
          GOTO 10
        endif
        if(ICOMP.eq.-1)then
          CALL USRMSG(' ',' Please select a zone!','W')
          GOTO 10
        endif

        strtgr = 1
        fstsur = 1
        lstsur = nsur
        tmrt=.false.
        GEOLOAD=.TRUE.
        call tstamp('>','VWF: enter viewfactor calcs')
        CALL CALVF(ICOMP)
        call tstamp('>','VWF: exit viewfactor calcs')
      ELSEIF(INO.EQ.10)THEN

C Proceed with TMRT calculations after asking for the
C temperatures of all surfaces, dimensions and coordinates
C of rectangular body.
        if(.NOT.CFGOK)then
          CALL USRMSG(' ',' Please define the model first!','W')
          GOTO 10
        endif
        if(ICOMP.eq.-1)then
          CALL USRMSG(' ',' Please select a zone!','W')
          GOTO 10
        endif
        if(NCUB(ICOMP).eq.0)then
          call usrmsg(' No MRT sensors defined in this viewfactor ',
     &      ' file... Please select another zone.','W')
          goto 10
        endif
        nsurmf = 0
        NGRID = 0
        NTRPOL = 0
        IER=0
        IGFACTold=IGFACT
        NPATCHold=NPATCH
        call tstamp('>','VWF: enter mrt calcs')
        call EDMRTC(ITRC,IUOUT,ICOMP,IER)
        call tstamp('>','VWF: exit mrt calcs')

C Reset grid and patch after return from MRT sensor calculations.
        IGFACT=IGFACTold
        NPATCH=NPATCHold
      ELSEIF(INO.EQ.12)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO.EQ.13)THEN

c Printout of surfaces.
        call tstamp('>','VWF: list surface info')
        context=.false.
        CALL SURINFO(ICOMP,iuout,context)
      ELSEIF(INO.EQ.14)THEN

C Save view factor file.
  442   helptopic='viewfactor_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
        endif
        LTMP=LVIEW(icomp)
        CALL EASKS(LTMP,' Viewfactor & MRT file name ?',' ',
     &     72,DFILE,'vwf file name',IER,nbhelp)
        if (LTMP(1:2).eq.'  ')goto 442
        LVIEW(icomp)=LTMP
        CALL EMKMRT(LVIEW(icomp),LGEOM(ICOMP),NZSUR(icomp),IUF,
     &    ICOMP,'v',IER)
        call tstamp('>','VWF: update viewfactor file')
      ELSEIF(INO.EQ.16)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('vwf ',0)
      ELSEIF(INO.EQ.17)THEN

c End program, free and delete the scratch file.
        call usrmsg(' Closing viewfactor and MRT program...',' ','P')
        call tstamp('>','Finish viewfactor module')
        INQUIRE (FILE=LTMPFL(1:lnblnk(LTMPFL)),EXIST=XST)
        if(XST)then
          CALL ERPFREE(IFPOL,ISTST)
          call FPRAND(IFPOL,ISTAT,80,1,LTMPFL)
          CALL EFDELET(IFPOL,ISTAT)
        endif
        CALL EPAGEND
        STOP
      ELSE
        INO=-2
        goto 20
      ENDIF
      goto 10

C Process a zone....
  42  continue

C On entry refresh geometry, determine its bounds, force it
C drawn and remember number of surfaces to pass to sensor file.
      ICOMP=IC
      call georead(IUF,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)

C Initialize the default viewfactors and MRT sensor file name and the 
C matching geometry file.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
       WRITE(VFILE,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
      else
        WRITE(VFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
      endif

C Get mrt file name. If it exists read it, if not create one with 
C default (area weighted) viewfactors and no MRT sensors.
  43  if(IVF(ICOMP).eq.1)then
        call edisp(iuout,' Opened existing viewfactor file.')
        CALL ERMRT(ITRC,iuout,IUF,LVIEW(icomp),ICOMP,IER)
        if(ier.ne.0)then
          helptopic='problem_reading_existing'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('Problem reading existing viewfactor file :',
     &      ' ','read again','create new file','continue',irt,nbhelp)
          if(irt.eq.1)then
            goto 43
          elseif(irt.eq.2)then

C Clear VEWF array in preparation for editing.
            DO 231 I = 1,MS
              DO 232 J=1,MS
                VEWF(I,J)=0.
  232         CONTINUE
  231       CONTINUE

C If geometry file included mrt sensors use them when creating
C this new file.
C            NCUB(ICOMP)=0
            CALL EMKMRT(LVIEW(icomp),LGEOM(ICOMP),NZSUR(ICOMP),
     &        IUF,ICOMP,'v',IER)
          elseif(irt.eq.3)then
            goto 10
          endif
        else

C Having read in an existing viewfactor file (which might contain
C previously calculated data set the values for strtgr fstsur and
C lstsur as if a calculation had just been performed.
          strtgr = 1
          fstsur = 1
          lstsur = nsur
        endif
        MRTLOAD=.TRUE.
        GEOLOAD=.TRUE.
        CALL FILEIN(ITRC,IUOUT,IER)
      else

C There was not vwf file so create one filled with zeros.
        LVIEW(icomp)=VFILE
        IVF(ICOMP)=1

C If mrt sensors known from geometry file include them when creating
C this new file.
C        NCUB(ICOMP)=0

        CALL edisp(iuout,' ')
        CALL edisp(iuout,' Creating file full of zeros.....')
        CALL edisp(iuout,' REMEMBER to add this to the configuration')
        CALL edisp(iuout,' when you return to the project manager')
        CALL edisp(iuout,' ')
        CALL EMKMRT(LVIEW(icomp),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &    ICOMP,'v',IER)
        CALL FILEIN(ITRC,IUOUT,IER)
        MRTLOAD=.TRUE.
        GEOLOAD=.TRUE.
        call tstamp('>','VWF: created file full of zeros')
      endif

C Determine geometry bounds and force it to be drawn.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ESCZONE(ICOMP)
      CALL BNDOBJ(1,IER)
      CALL ERCZONE(ICOMP)
      MODIFYVIEW=.TRUE.
      MODLEN=.TRUE.
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      if(ncub(ICOMP).gt.0)then
        CALL ESCZONE(ICOMP)
        CALL DRWSEN(ICOMP,ier)
        CALL ERCZONE(ICOMP)
      endif
      goto 10

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 10

      END

C *********** CALVF 
C Calculate viewfactors for a zone.
      SUBROUTINE CALVF(ICOMP)

#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/MRT2/GEOLOAD,MRTLOAD
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON /CELLS/ NCELL(MSM), NCPT(MSM), IGFACT
      COMMON /FORMF/ FF(MSM,MSM), FFS
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)

c wrongrow   : Will contain the rownumber with largest error
c rowsum(i)  : will contain the sum over j  of  ff(i,j)
c np         : number of patch-rings on bubble
C outfile is the geometry file name for the zone with cube added.
      real drow,rowsum(MS+1)
      integer wrongrow
      character outs*124
      logical OK,DOK,GEOLOAD,MRTLOAD
      dimension iivis(MSM)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

      helpinsub='CALVF'  ! set for this subroutine

c Initialize FF(i,j) and IVIS(i,j).
      IUF = IFIL+1
      do 230 i=1,nsurm
        do 220 j=1,nsurm
          ff(i,j)=0.0
          ivis(i,j)=0
220     continue
230   continue

      CALL  GRID(ierror)

C Exit on error.
      if(ierror.ne.0)goto 1111

      CALL  SURVIS

      CALL edisp(iuout,' ')
      CALL edisp(iuout,' Surface-by surface visibility matrix: ')
      CALL edisp(iuout,' -1 other surf invisible [? same plane] ')
      CALL edisp(iuout,'  1 other surf fully visible ')
      CALL edisp(iuout,'  2 other surf partially obstructed by a 3rd ')
      CALL edisp(iuout,'  3 other surf perpendicular and obstructed. ')
      CALL edisp(iuout,' ')

C MS sensitive, change if MS>32.
      DO 240 I = 1,nsurmf
        do 241 jj=1,nsurmf
          iivis(jj)=IVIS(i,jj)
  241   continue
        ipos=1
        call ailist(ipos,nsurmf,iivis,MSM,'S',outs,loutln,itrunc)
        WRITE(outs,'(a)') outs(1:loutln)
        call edisp(iuout,outs)
240   CONTINUE

      call checkrec

      CALL USRMSG(' ',' Computation commences','-')

      CALL  BUBBLE

      PI = 4.0 * ATAN(1.0)
      DEN = PI* real(IGFACT*IGFACT)

      CALL USRMSG(' ',' View factor information','-')

      rowsum(MS+1)=0.0
      DO 252 I = 1,nsurmf
        rowsum(i)=0.0
        DO 250 J = 1,nsurmf
          if (ncell(i).ne.0)then

C Calculate viewfactor by dividing by the number of grid cells in
C the polygon.
            ff(i,j)=ff(i,j)/(ncell(i)*PI)
          else
            ff(i,j)=ff(i,j)/den
          endif

C If an individual ff is > 0.99999 reset it because it
C causes havoc when attempting to re-read the vwf file.
          if(ff(i,j).gt.0.9999)then
            write(outs,*) 'resetting ff(ij)',ff(i,j),' to 0.999'
            call edisp(iuout,outs)
            ff(i,j)=0.9999
          endif
          rowsum(i)=rowsum(i)+ff(i,j)

C If within zone portion of ff array save to zone viewfactor array.
          if(i.le.MS.and.j.le.MS)VEWF(i,j)=ff(i,j)
250     CONTINUE
        drow=((rowsum(i)-1.0)**2)**0.5
        if ((drow).gt.rowsum(MS+1)) then
          rowsum(MS+1)=drow
          wrongrow=i
        endif
252   CONTINUE

      WRITE(outs,'(1X,I5)')  nsurmf
      call edisp(iuout,outs)
      DO 460 I2 =  1,nsurmf
        if(nsurmf.le.10)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1, nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.10.and.nsurmf.le.20)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11, nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.20.and.nsurmf.le.30)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.30.and.nsurmf.le.40)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.40.and.nsurmf.le.50)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.50.and.nsurmf.le.60)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.60.and.nsurmf.le.70)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.70.and.nsurmf.le.80)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.80.and.nsurmf.le.90)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.90.and.nsurmf.le.100)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.100.and.nsurmf.le.110)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,100)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=101,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.110.and.nsurmf.le.120)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,100)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=101,110)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=111,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.120.and.nsurmf.le.130)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,100)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=101,110)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=111,120)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=121,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.130.and.nsurmf.le.140)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,100)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=101,110)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=111,120)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=121,130)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=131,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.140.and.nsurmf.le.150)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,100)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=101,110)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=111,120)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=121,130)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=131,140)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=141,nsurmf)
          call edisp(iuout,outs)
        elseif(nsurmf.gt.150.and.nsurmf.le.160)then
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=1,10)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=11,20)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=21,30)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=31,40)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=41,50)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=51,60)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=61,70)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=71,80)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=81,90)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=91,100)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=101,110)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=111,120)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=121,130)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=131,140)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=141,150)
          call edisp(iuout,outs)
          WRITE(outs,'(1X,10F7.4)')  (FF(I2,J),J=151,nsurmf)
          call edisp(iuout,outs)
        endif
460   CONTINUE

C  Print sum over a row of view factors. Commented
C  because it might influence use of view factors in bps.
      DO 462 I2 =  1,nsurmf
        fr=0.0
        DO 461 J =  1,nsurmf
          fr=fr+ff(i2,j)
461     CONTINUE
        WRITE(outs,4060)i2,fr
4060    FORMAT('  sum over row ',i3,' is : ',F15.10)
        call edisp(iuout,outs)
462   CONTINUE

C If summation is not close to unity then report.
      if (rowsum(MS+1).gt.0.01) then
        call edisp(iuout,' ')
        call edisp(iuout,
     &' The sum over one row in the viewfactor matrix should be 1.00.')
        call edisp(iuout,' A deviation > 0.01 was encountered')
        xxx=(1.0-rowsum(wrongrow))
        write(outs,2072)xxx,wrongrow
2072    format(' The largest deviation was : ',f8.4,' in row ',i3)
        call edisp(iuout,outs)
        if(xxx.gt.0.4)then
          call edisp(iuout,'  ')
          call edisp(iuout,
     &    ' This deviation might mean that the grid was not fine')
          call edisp(iuout,
     &    ' enough for the surface associated with that row. Try')
          call edisp(iuout,
     &    ' increasing the grid resolution and recalculate.')
        endif
      endif
C Save view factor file.
      dok=.true.
      helptopic='user_review_of_calcs'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' Ok to save these viewfactors?',' ',OK,dok,
     &  nbhelp)
      if(OK)then
        CALL EMKMRT(LVIEW(icomp),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &    ICOMP,'v',IER)
      endif
      RETURN

C Grid Error 
 1111 maxgrd=int(sqrt(real(mgc/nsurmf)))
      write(outs,'(a,i5,a,i5,a)') ' This zone needs ',ierror,
     &  ' cells and ',mgc,' are available.'
      call edisp(iuout,outs)
      write(outs,'(a,i3,a)') ' Reduce the grid parameter to',maxgrd,
     &  ' or recompile module.'
      call edisp(iuout,outs)
      RETURN

      END
      
C ***** Dummy display of images.
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
      return
      end

      subroutine Find_season_parameters()
      return
      end

      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)
      integer icomp,ier
      return
      end
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
      ier=0
      return
      end


C In support of floating point exceptions.
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

C Local copyt of EPKVERT (from edgeo.F)
C ************* EPKVERT 
C EPKVERT Select one or more vertices from information currently in
C common block G1.
C << zone index is not known but might be useful here >>
      SUBROUTINE EPKVERT(INPICK,IVLST,TITLE,PROMPT1,PROMPT2,NHELP,IER)
#include "building.h"
#include "geometry.h"
#include "epara.h"
#include "prj3dv.h"

C Parameters passed.
      integer inpick  ! passed in number of items that can be selected
                      ! and becomes actual number of items selected
      integer IVLST   ! array to hold items selected
      CHARACTER*(*) TITLE,PROMPT1,PROMPT2
      integer nhelp   ! number of context help lines
      integer ier     ! error state zero ok, one problem

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      DIMENSION  item(36)
      real COG1,COG2,COG3  ! position to check against
      DIMENSION  COG1(3),COG2(3),COG3(3),IVLST(MTV)
      CHARACTER item*33,outs*124,KEY*1
      logical greyok,found
      integer MVERT,IVERT ! max items and current menu item

C Trackview needs iixx,iiyy.
#ifdef OSI
      integer iix,iiy,ixd,iyd,iicol,iixx,iiyy,iik
#else
      integer*8 iix,iiy,ixd,iyd,iicol,iixx,iiyy,iik
#endif

C Initialise vertex list menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C Also clear tagged items list (IVLST).
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif
      IER=0
      MHEAD=1
      MCTL=4
      ILEN=NTV
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IALLOW=INPICK
      INPICK=0
      DO 40 I=1,NTV
        IVLST(I)=0
   40 CONTINUE

C Initial menu entry setup.
C << is there something better to use then NTV ? >>
      CALL USRMSG(PROMPT1,PROMPT2,'-')
   92 ILEN=NTV
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          item(m)=' '
          WRITE(item(M),'(A1,I3,3F9.3)')KEY,L,X(L),Y(L),Z(L)
          do 20 K=1,INPICK
            if(IVLST(K).eq.L) then
              WRITE(item(M),'(A1,I3,3F9.3,A)')KEY,L,X(L),Y(L),
     &          Z(L),' *'
            endif
 20       continue
        ENDIF
   10 CONTINUE

      item(1)    =' Vertex | X Y Z coordinates      '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        item(M+1)='  ______________________________ '
      ELSE
        WRITE(item(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      if(MMOD.lt.8)then
        write(item(M+2),'(2x,a,i3,a)') '(',IALLOW,' items)'
      else
        write(item(M+2),'(a,i3,a)')'* select via mouse (',IALLOW,
     &    ' items)'
      endif
      item(M+3)  ='? help                           '
      item(M+4)  ='- exit                           '

C Help text for this menu is passed from calling routine.
      CALL EMENU(TITLE,item,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return with updated IVLST().
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help strings setup in the calling function.
        CALL PHELPD('vertex pick',NHELP,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Use mouse to select existing vertices.
        if(MMOD.eq.8)then
          ijvn=0
          if(inpick.ne.2)then
            call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
          else
            call edisp(iuout,
     &        'Select points or edge via cursor...type `e` to finish.')
          endif

C Return pixel position of mouse click, check if key `e` or `E` was
C hit and then loop through each of the vertices for something close.
C In this code block trackview is correctly using iixx and iiyy.
  46      CALL trackview(iik,iixx,iiyy)
          if(iik.eq.69.or.iik.eq.101)goto 47
          found=.false.
          do 45 i=1,NTV
            COG1(1)=X(I); COG1(2)=Y(I); COG1(3)=Z(I)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            ixd=iix-iixx
            iyd=iiy-iiyy
C Debug
C            write(6,'(7i5)') i,iixx,iiyy,iix,iiy,ixd,iyd
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 46
              endif
              WRITE(outs,'(a,i3,a,3F9.3)')' The point matches vertex',
     &          i,' @ XYZ ',X(I),Y(I),Z(I)
              call edisp(iuout,outs)
              found=.true.
              INPICK=INPICK+1
              ijvn=ijvn+1
              IVLST(ijvn)=i

C Highight the matching vertex.
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
              goto 46
            endif
  45      continue
          if(.NOT.found)then

C If there were only two points to find, check if user clicked on edge.
            if(inpick.eq.2)then
              call edisp(iuout,'code for edge check in progress')
            endif
            goto 46
          endif
  47      continue
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Look through previous selections and see if IFOC is unique, if
C so update IVLST and loop back for another.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPICK.GT.0)THEN
          DO 44 J=1,INPICK
            IF(IVLST(J).EQ.IFOC.or.FOUND) then
              FOUND=.TRUE.
              if (J+1.gt.NTV) then
                IVLST(J)=0
              else
                IVLST(J)=IVLST(J+1)
              endif
            endif
  44      CONTINUE
          IF(.NOT.FOUND)THEN
            if (INPICK.lt.IALLOW) then
              INPICK=INPICK+1
              IVLST(INPICK)=IFOC
            endif
          ELSE
            INPICK=INPICK-1
          ENDIF
        ELSEIF(INPICK.EQ.0)THEN
          INPICK=1
          IVLST(INPICK)=IFOC
        ENDIF
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END


