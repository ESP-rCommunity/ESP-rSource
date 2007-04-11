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

C MAIN ish.F

C THE SHADING PROGRAM PREDICTS FOR ANY BODY SHAPE :
C      1.   THE SHADING ON OPAQUE SURFACES
C      2.   THE SHADING ON TRANSPARENT SURFACES

C The subroutines used by ish are found in the following files:
C comput.f - COMPUT: Sets up the shading computation parameters.
C graph.f  - GRAPH:  Controls display of surface &  shaded areas.
C          - FACDRW: Draws the elevation of a surface.
C          - SHDDRW: Plots point on a surface to represent shaded portion.
C          - SITPLN: Draws the site plan showing zone & obstructions.
C miscel.f - AREA:   Returns area for any polygon.
C          - MESH:   Computes centre point X&Z coord of grid squares.
C          - ERRORM: Error trapping.
C point.f  - POINT1: Decides if a point is within a face boundary.
C          - POINT2: Decides if a point is within a shadow boundary.
C files.f  - SFILE:  Open a new transitional shading file.
C          - SIFILE: Opens a shading/insolation file.
C          - SWRT:   Writes the shading data to the shading file.
C          - SSAVE:  Checks and writes a SHADING/INSOLATION FILE.
C          - RETRV1: Retrives info from header block of the trans file.
C shdcon.f - SHDCON: Controls shading computation.
C          - SHDTST: Tests grid centre points to see if within surface.
C          - DECOMP: Reduces 8 pt shadow 'box' to equivalent outline.
C silist.f - SILIST: Lists the contents of a shading/insolation file.
C synop.f  - SYNOP:  Displays table of % shading for opaque & transparent.
C tfiles.f - TFILE1: Write to transitional shading file.
C          - TFILE2: Transfers transformed coords of surfaces to trns shd file.
C          - TFILE3: Saves hourly grid shading index in the trns shd file.
C trnsf.f  - TRNSF1: Sets of the coefficients of the transformation equations.
C          - TRNSF2: Original site coordinate system > local face system.
C          - TRNSF3: Project 8 corner point of the obstruction block.
C          - CORNR:  Returns X & Y coords of 4 top corners of obstr block.
C          - PANGS:  Returns the azimuth, elevation and tilt of a surface.
C winshd.f - WINSHD: Defines ASHRAE single window shading prediction method.
C wshad.f  - WSHAD:  Performs ASHRAE window shading.

      program ish
#include "building.h"

      integer igraphiclib  ! external definition

      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/TC/ITC,ICNT
      common/pophelp/h(60)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/DATA2/ICAL
      COMMON/MTFILE/LTRNS,MULTC,MONS,MONF
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

C Path to model and command line file (if any).
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/rcmd/LCMDFL

C Indicator of possible focus zone and action to take.
      common/rzone/inzone
      common/ract/paction

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC8/SLAT,SLON

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DIMENSION MTHNAM(12),ITEMS(20),ISADD(12),ISHD(12)
      LOGICAL MODIFY,MODLEN,MODBND,XST
      LOGICAL    OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,tok
      CHARACTER*72 LNAME,LTRNS,TBASE,H
      CHARACTER*72 LCFGF,LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character items*26
      CHARACTER MTHNAM*3,outs*124,outs248*248,ETEXT*60,LTMP*144
      CHARACTER DESCRH*5,DESCRD*5,zname*12,zdesc*64,inf*144,LCMDFL*144
      character path*72,upath*72,esprc*72,cfgroot*24
      character dfdblbl*20,defdbfl*72
      character inz*16,inzone*16,ZN*12,pact*16,paction*16,OUTSTR*124
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character journcmd*20,jfile*72,uname*24,fs*1,cjfile*72
      logical silent,unixok,there,context,existing
      character xfile*72,tg*1,delim*1

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Initialize global common data.
      call ezero
      call curmodule('ish ')
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      NSIGFIG=3

C Notes on various file units within shd: IUNIT is the transitional
C shading file, IFILSI is the shading insolation db, IUF is for
C miscel files, IFCFG for the configuration file.
      IFIL=11
      IUF=IFIL+1
      IFCFG=IFIL+2
      CFGOK=.FALSE.
      IFILSI=IFIL+3
      IAIRP = IFIL+6
      iuj=IFIL+10
      IUNIT=IFIL
      LNAME='SHDTRN'
      ZN='UNKNOWN'
      call parsfza(MODL,iappw,iappx,iappy,inf,inz,pact)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

      IFDAY=2
      IFTIME=0
      IYEAR=2003
      IBDOY=1
      IEDOY=1
      ITC=0
      ICOUT=IUOUT
      tok=.false.

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
      ITSNM=0
      ITVNO=1
      ITORG=1
      ITSNR=0
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0
      IFS=1
      ITFS=1
      IMFS=1

C Initialise terminal and flags.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=530
        iappx=75
        iappy=170
      else
        if(iappx.le.0)iappx=75
        if(iappy.le.0)iappy=170
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
        menuchw = MAX0(int(28*iappw*0.01),16)
        LIMTTY=10
        LIMIT =10
      else
        menuchw = 28
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
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r Shading & Insolation: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,2,1,1,3,igl,igr,igt,igb,igw,igwh)
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
      IF(MMOD.NE.-6)ICOUT=IUOUT

      call edisp(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Shading & Insolation: Version 1.5a of January 2006.',
     &  ' Copyright 2001-6 '
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Energy Systems Research Unit, University of',
     & ' Strathclyde, Glasgow Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,' ')

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

        write(outs,'(a,a)')'Journal (shading analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif
  902 continue

C Scan the defaults file for default configuration.
      call escdef(dfdblbl,defdbfl,IUF,IER)

C Take command line file name as initial configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        if(pact(1:13).ne.'update_silent')then
          call edisp248(iuout,outs248,80)
        endif
      else
        LCMDFL='  '
      endif

C Take command line zone name and
C determine if focus should be to a single zone.
      if(inz(1:1).ne.' ')then
        inzone=inz
        write(outs,'(a,a)')' the input zone is: ',inzone
        if(pact(1:13).ne.'update_silent')then
          call edisp(iuout,outs)
        endif
        write(outs,'(a,a)')'ISH: the input zone is: ',inzone
        call tstamp('>',outs)
      else
        inzone='ALL'
        call tstamp('>','ISH: no focus zone')
      endif

C Take command line action parameter. If paction = 'recalculate' then
C after the configuration is read in, do a standard annual recalculation
C and pause so user can view results. If paction = 'update_silent' then
C do standard annual calculation but exit when completed.
      if(pact(1:1).ne.' ')then
        paction=pact
        write(outs,'(a,a)')'ISH: the input action is: ',paction
        call tstamp('>',outs)
        if(paction(1:11).eq.'recalculate')then
          write(outs,'(a,a)')' the action is: ',paction
          call edisp(iuout,outs)
          silent=.true.
        elseif(paction(1:13).eq.'update_silent')then
          silent=.true.
        else
          paction='INTERACTIVE'
          call tstamp('>','ISH: interactive mode')
          silent=.false.
        endif
      else
        paction='INTERACTIVE'
        silent=.false.
      endif

      ICAL=0
      MULTC=0
      ICOMP=1
      MONS=1
      MONF=12

C Export (grtool) file information. Temporarily use 
C unit ifil+6 for output file.
      xfile = 'shading.txt'
      ixopen = 0
      ixunit = ifil + 9
      delim = '-'

C Debug.
C      write(6,*) ixopen,ixunit,delim,xfile(1:lnblnk(xfile))

C If an input file has been specified then load it.
      LCFGF=' '
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then
          call fdroot(LCMDFL,path,LCFGF)
          if(paction(1:13).ne.'update_silent')then
            call usrmsg(' Scanning the model...',' ','-')
          endif
          call ESCSYS(LCFGF,IFCFG,IAIRP,IER)
          if(paction(1:13).ne.'update_silent')then
            call usrmsg(' Scanning the model...done.',' ','-')
          endif
          if(IER.eq.0)then
            CFGOK=.TRUE.
            NZONES=NCOMP
            CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
            IF(MMOD.EQ.8)call updwire(ncomp)

C If inzone = All display an image of the model. If inzone is either
C a string which matches a zone name or is an index then focus on
C that zone.  If inzone = UNKNOWN or the index 0 present a list.
            call zindex(inzone,index)
            if(index.le.0)goto 201
            ICOMP = index
            ZN=zname(ICOMP)

C Setup transitional shading file based on zones name.
            write(LNAME,'(A)')ZN(1:lnblnk(ZN))
            OK=.true.
            LTRNS=LNAME
            CALL SHDPRB(ITC,silent,ICOMP,IER)
          endif
        endif
      endif

C If in recalculation mode, proceed with actions with minimal interface.
C If an existing db is found, removed it and open a new one before
C starting calculations.
      if(silent)then
        IF(ISI(ICOMP).EQ.1)THEN
          maxrec=24
          if(NZSUR(icomp).gt.24)maxrec=NZSUR(icomp)
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)then
            CALL ERPFREE(IFILSI,ISTST)
            call EFOPRAN(IFILSI,LSHAD(ICOMP),maxrec,1,IER)
            write(outs,'(A,A)') ' Deleting previous ',LSHAD(ICOMP)
            if(paction(1:13).ne.'update_silent')then
              call edisp(iuout,outs)
            endif
            CALL EFDELET(IFILSI,ISTAT)
            call sifile(ICOMP)
          endif
          
C Proceed with shading and then insolaiton calculations.
          if(IOBS(ICOMP).EQ.1)then
            CALL EGOMST(IUF,ZOBS(icomp),0,0,iuout,IER)
            ZN=zname(ICOMP)
            write(outs,'(3a)') 'Shading calculations for ',
     &        zn(1:lnblnk(zn)),'...'
            call edisp(iuout,outs)
            call tstamp('>','ISH: re-calculate shading')
            CALL COMPUT(ICOMP,IER,'s')
            write(outs,'(3a)') 'Shading calculations for ',
     &        zn(1:lnblnk(zn)),'...done.'
            call edisp(iuout,outs)
          else
            call edisp(iuout,' No obstructions, doing insolation.')
          endif
          ICSTAT=0
          ZN=zname(ICOMP)
          write(outs,'(3a)') 'Insolation calculations for ',
     &      zn(1:lnblnk(zn)),'...'
          call edisp(iuout,outs)
          call tstamp('>','ISH: start insolation computation')
          call INSCOM(ICOMP,ICSTAT,'s')
          write(outs,'(3a)') 'Insolation calculations for ',
     &      zn(1:lnblnk(zn)),'...done.'
          call edisp(iuout,outs)
         else
          call usrmsg(' Unknown shading:insolation file.',
     &                ' Cannot recalculate...','W')
        endif
        if(paction(1:11).eq.'recalculate')then
          call usrmsg(' Recalculation complete. You may exit.',' ','-')
        elseif(paction(1:13).eq.'update_silent')then

C Finished recalculation so exit program, delete the 
C transitional files; if a single month calculation has been done,
C then offer to save or delete; if a multi-month calculation has
C been done, all user to delete them.
          call usrmsg(' Recalculation complete...exiting.',' ','-')
          IUNIT=IFIL
          IF(MULTC.EQ.0)THEN
            CALL EFDELET(IUNIT,ISTAT)
          ELSEIF(MULTC.EQ.2)THEN
            IRECW=MS+5
            do 155 ix=1,NCOMP
              do 156 im=1,12
                write(LNAME,'(A,A3)')zname(ix)(1:lnblnk(zname(ix))),
     &                MTHNAM(im)
                call EFOPRAN(IUNIT,LNAME,IRECW,0,IER)
                if(ier.eq.0) CALL EFDELET(IUNIT,ISTAT)
 156          continue
 155        continue
            call pauses(1)
          endif
        ENDIF
        CALL EPAGEND
        STOP
      endif
      
C Display opening menu.
  201 IER=0

      INOS=-3
      write(ITEMS(1),'(A,A17)') 'a model: ',LCFGF(1:17)
      write(ITEMS(2),'(A,A18)') '  path: ',path(1:18)
      write(ITEMS(3),'(A,A12)') 'b zone: ',ZN(1:12)
      ITEMS(4) =                '  _______________________ '
      ITEMS(5)=                 'c ASHRAE window calc      '
      ITEMS(6)=                 'd mercator Sun Path       '
      ITEMS(7)=                 'e views: sun/site plan    '
      ITEMS(8) =                '  _______________________ '
      ITEMS(9)=                 'f calculate shading       '
      ITEMS(10)=                'g edit:import:export data '
      ITEMS(11)=                'h view shading patterns   '
      ITEMS(12) =               '  _______________________ '
      ITEMS(13)=                'i predict insolation      '
      ITEMS(14)=                'j view insolation         '
      ITEMS(15) =               '  _______________________ '
      IF(ITC.EQ.0)THEN
        ITEMS(16)=              'r reporting >> silent     '
      ELSEIF(ITC.EQ.1)THEN
        ITEMS(16)=              'r reporting >> summary    '
      ELSEIF(ITC.EQ.2)THEN
        ITEMS(16)=              'r reporting >> detailed   '
      ENDIF
      if(ixopen.eq.1)then
        ITEMS(17)='> output >> file       '
      elseif(ixopen.eq.0)then
        ITEMS(17)='> output >> screen     '
      endif
      ITEMS(18)=                '! browse zone surfaces    '
      ITEMS(19)=                '? help                    '
      ITEMS(20)=                '- finish ISH              '

C If zone has not been selected disallow calculations and views.
      if(ZN(1:4).eq.'UNKN')then
        ITEMS(9) =              'f  ...                    '
        ITEMS(10)=              'g  ...                    '
        ITEMS(11)=              'h  ...                    '
        ITEMS(13)=              'i  ...                    '
        ITEMS(14)=              'j  ...                    '
      endif
      NITEMS = 20

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('ish ',1)

      CALL EMENU('  Shading & insolation',ITEMS,NITEMS,INOS)
      IF(INOS.EQ.1)THEN

C Request definition of model.
        H(1)='The system configuration file holds the definition '
        H(2)='of the building/plant to be simulated, including the'
        H(3)='names of all of the files required. If the name '
        H(4)='given matches an existing file it will be read and '
        H(5)='the data will be available for editing.'
        LTMP=LCMDFL

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the 
C file, while the GTK version will be returning the
C name with the full path.
  289   llt=lnblnk(ltmp)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(LTMP,' Model configuration file?',' ',96,DFCFG,
     &        'config file name',IER,5)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(LTMP,' Model configuration file?',' ',124,DFCFG,
     &        'config file name',IER,5)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(LTMP,' Model configuration file?',' ',144,DFCFG,
     &        'config file name',IER,5)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(LTMP,' Model configuration file?',' ',144,DFCFG,
     &      'config file name',IER,5)
        else
          CALL EASKF(LTMP,' Model configuration file?',' ',96,DFCFG,
     &      'config file name',IER,5)
        endif

        IF(LTMP(1:2).EQ.'  '.or.LTMP(1:4).eq.'UNKN')GOTO 289
        call st2file(LTMP,LCMDFL)
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then
          call fdroot(LCMDFL,path,LCFGF)
          call edisp(iuout,' ')
          call edisp(iuout,' Scanning the model description')
          call tstamp('>','ISH: scanning new model')
          call tstamp('>',LCFGF)
          call ESCSYS(LCFGF,IFCFG,IAIRP,IER)
          if(IER.eq.0)then
            CFGOK=.TRUE.
            NZONES=NCOMP
            CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
          endif
        endif
      elseif(INOS.EQ.3)THEN

C Read geometry file for either calculation purpose
        if(.NOT.CFGOK)then
          CALL USRMSG(' ',' Please define the model first!','W')
          GOTO 201
        endif
        IC=-1
        CALL EASKGEOF('Select a zone to assess:',CFGOK,IC,'-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 201
        IER=0
        ICOMP = IC
        ZN=zname(ICOMP)
        write(outs,'(a,a)')'ISH: shift zone focus to: ',ZN
        call tstamp('>',outs)

C See if there is an existing transitional shading file with, 
C the zones name as root ask whether to use it.
        write(LNAME,'(A)')zname(ICOMP)(1:lnblnk(zname(ICOMP)))
        LTRNS=LNAME
        CALL SHDPRB(ITC,silent,ICOMP,IER)
      ELSEIF(INOS.EQ.5)THEN

C Perform the ASHRAE calculations.
        call tstamp('>','ISH: enter ASHRAE calcs')
        CALL WINSHD
        call tstamp('>','ISH: exit ASHRAE calcs')
      ELSEIF(INOS.EQ.6)THEN

C Sun plot.
        call tstamp('>','ISH: start sunplot')
        call edisp(iuout,' ')
        call edisp(iuout,' Date for the Mercator sun plot... ')
        IMO=1
        IDO=1
        call ASKDAY(IFDAY,IMO,IDO,IJDAY,IER)
        call sunplt(IJDAY,IER)
      ELSEIF(INOS.EQ.7)THEN
        if(MMOD.lt.8)goto 201
        H(1)='The site plan shows zones in bold and obstructions '
        H(2)='as thin lines.  The sun view places the viewer at a '
        H(3)='distance of a kilometer and at the azimuth & elevation'
        H(4)='of the sun. '
        CALL EASKAB(' ','Current viewpoints:',
     &    'site plan','view model from sun position',IW,4)

C Draw the siteplan.  After drawing it the zone geometry file must 
C be read back in to reconstruct the common blocks G1,G2,G5 in 
C preparation for any calculations to be done.
        if(IW.eq.1)then
          call tstamp('>','ISH: look at siteplan')
          CALL SITPLN(ICOMP)
          CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,0,0,IUOUT,IER)
        else

C Draw the zone and any obstructions associated with it based on
C sun position. Confirm the lat, long, request day and time,
C compute positon and update the view.
          call tstamp('>','ISH: look at sun view')
          call edisp(iuout,' Date and time for the view... ')
          NTS=1
          IMO=1
          IDO=1
          STIME=1.
  67      CALL ASKTIM(IFDAY,NTS,IMO,IDO,IJDAY,STIME,IT,IER)
  68      CALL EAZALTS(STIME,IJDAY,SLAT,SLON,ISUNUP,SAZI,SALT)
          IF(SALT.GT.0.0)THEN
            CALL  ANGXYZ(SAZI,SALT,X1,Y1,Z1)
            EYEM(1)=X1
            EYEM(2)=Y1
            EYEM(3)=Z1
            ITSNM=1
            MODIFY=.TRUE.
            MODLEN=.TRUE.
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(ITC,IER)

C Generate a heading for the view.
            CALL EDTIME(STIME,DESCRH,DESCRD,TIMER)
            WRITE(ETEXT,'(A,I2,1X,A3,A,A5,A,F6.1,A,F5.1)')
     &        ' View: ',IDO,MTHNAM(IMO),' @',DESCRH,
     &        '  azim:',SAZI,' elev:',SALT
            CALL viewtext(ETEXT,2,1,IFS)
            CALL EASKAB(' ',' Next viewpoint:',
     &        'next hour','exit',IW,0)
            if(IW.eq.1)then
              STIME=STIME+((60.0/FLOAT(NTS))/60.0)
              goto 68
            else
              ITSNM=0
              goto 201
            endif
          else
            CALL EASKAB(' View point below horizon:',' ',
     &        'specify another time','exit',IW,0)
            if(IW.eq.1)goto 67
            ITSNM=0
          endif
        endif
      ELSEIF(INOS.EQ.9)THEN

C Calculate shading, first making sure that isi is non-zero.
        if(ZN(1:4).eq.'UNKN') goto 201
        IF(ISI(ICOMP).EQ.1)THEN

C If there is already data in the file ask if the user wants
          existing=.false.
          irec=1
          read(ifilsi,rec=irec,iostat=istat,err=30)(ishd(i),i=1,12),
     &        (isadd(i),i=1,12)
          do 10 i=1,12
           if(ishd(i).ne.0.or.isadd(i).ne.0)existing=.true.
   10     continue
          if(existing)then
            h(1)='Data already existing in this file. New computations'
            h(2)='can overwrite this (not recommended) or a fresh file'
            h(3)='can be created.'
            CALL EASKABC('Shading file has existing data.','Options:',
     &        'overwrite data','create a fresh file',
     &        'abort calculation',IW,3)
            if(iw.eq.1)then
              continue
            elseif(iw.eq.2)then
              maxrec=24
              if(NZSUR(icomp).gt.24)maxrec=NZSUR(icomp)
              CALL ERPFREE(IFILSI,ISTST)
              call EFOPRAN(IFILSI,LSHAD(ICOMP),maxrec,1,IER)
              write(outs,'(A,A)') ' Deleting previous ',LSHAD(ICOMP)
              if(paction(1:13).ne.'update_silent')then
                call edisp(iuout,outs)
              endif
              CALL EFDELET(IFILSI,ISTAT)
              call sifile(ICOMP)
            elseif(iw.eq.3)then
              goto 201
            endif
          endif

          call tstamp('>','ISH: start shading computation')
          CALL COMPUT(ICOMP,IER,'i')
        else
          call usrmsg(
     &    'Unknown shading:insolation file. Please choose another zone',
     &    'or use the Project Manager to specify for this zone.','W')
        endif
      ELSEIF(INOS.EQ.10)THEN

C Edit data for one month.
        if(ZN(1:4).eq.'UNKN')then
          call edisp(iuout,'Plese select a zone first!')
          goto 201
        else
          CALL SIEDIT(ICOMP)
        endif
      ELSEIF(INOS.EQ.11)THEN

C View results of simulation: current shading/insolation file contents,
C synopsis of shading or plot of shading on a surface.
        if(ZN(1:4).eq.'UNKN')then
          call edisp(iuout,'Plese select a zone first!')
          goto 201
        endif
  300   h(1)='Choose one of the options supplied. '
        CALL EASKABC('Shading options:',' ','List file content',
     &               'Shading synopsis','Draw surf shading ? ',IW,1)
        if(IW.eq.1)then
          call tstamp('>','ISH: list shading contents')
          CALL SILIST(ICOMP)
        ELSEIF(IW.EQ.2.OR.IW.EQ.3)THEN
          call tstamp('>','ISH: shading synopsis')
          IF(MULTC.EQ.0)THEN

C Flags for calculation not set or a previous set of transition files
C have probably been loaded.
            CALL USRMSG(
     &      ' You have probably loaded in a previous analysis, and it',
     &      ' is assumed that it was with the current version, ','-')
            MULTC=2
          ENDIF

          IF(MULTC.EQ.2)THEN

C A multi-month calculation was done, ask for which month, then open
C the relevant transitional shading file based on the base name.
   65       H(1)='Should be an integer Jan=1 etc. '
            MON=MONS
            CALL EASKI(MON,' ',' Computation month number? ',
     &        1,'F',12,'F',1,'month number',IER,1)
            IF(IER.NE.0)goto 65

C Allow for number of surfaces within the transitional file.
            IRECW=MS+5
            write(TBASE,'(A,A3)')LTRNS(:LNBLNK(LTRNS)),MTHNAM(MON)
            call EFOPRAN(IUNIT,TBASE,IRECW,1,IER)
            IF(ier.eq.0)THEN
              write(outs,'(A,A,A)')' Transitional file ',
     &                              TBASE(:LNBLNK(TBASE)),' opened...'
              CALL edisp(iuout,outs)
            ELSE
              CALL USRMSG(' The transitional shading file for',
     &                    ' this month does not exist. ','W')
C << at this point could do shading computation to create the necessary >>
C << transitional file to read. >>
              goto 300
            ENDIF
          endif
          CALL RETRV1
          if(IW.EQ.2)CALL SYNOP(ICOMP)
          if(IW.EQ.3.and.MMOD.eq.8)then
            call tstamp('>','ISH: view shading patterns')
            CALL GRAPH(ICOMP)
          endif
        endif
      ELSEIF(INOS.EQ.13)THEN

C Insolation computation.
        if(ZN(1:4).eq.'UNKN')then
          call edisp(iuout,'Plese select a zone first!')
          goto 201
        endif
        IF(ISI(ICOMP).EQ.1)THEN
          ICSTAT=0
          call tstamp('>','ISH: start insolation computation')
          call INSCOM(ICOMP,ICSTAT,'-')
        else
          call usrmsg(' Unknown shading:insolation db or utility',
     &                ' file. Please choose another zone...','W')
        endif
      ELSEIF(INOS.EQ.14)THEN

C Synopsis of insolation.
        if(ZN(1:4).eq.'UNKN')then
          call edisp(iuout,'Plese select a zone first!')
          goto 201
        else
          CALL EASKAB(' Insolation reporting:',' ',
     &      'list file contents','insolation synopsis',IW,0)
          if(IW.eq.1)then
            call tstamp('>','ISH: list insolation contents')
            CALL SILISTI(icomp)
          else
            call tstamp('>','ISH: list insolation synopsis')
            MON=MONS
            CALL EASKI(MON,' ',' Which month (number) ? ',
     &        1,'F',12,'F',1,'ins comp month',IER,0)
 202        CALL INSYNP(icomp)
            if(MON.lt.12)then
              h(1)='You can look at several months in sequence.'
              CALL EASKAB('Look at next month?:',' ','yes','no',IW,1)
              if(iw.eq.1)then
                MON=MON+1
                goto 202
              endif
            endif
          endif
        endif
      ELSEIF(INOS.EQ.16)THEN

C Set trace level displayed in opening menu and main menu.
        ITC=ITC+1
        if(ITC.GT.2)then
          ITC=0
          ICOUT=iuout
          tok=.false.
        elseif(ITC.eq.1)then
          h(1)='Look for a fort.33 file'
          CALL EASKAB(' Redirect trace messages to file?:',' ',
     &        'yes','no',IW,1)
          if(IW.eq.1)then
            ICOUT=33
            tok=.true.
          else
            ICOUT=iuout
            tok=.false.
          endif 
        elseif(ITC.eq.2)then
          h(1)='Look for a fort.33 file'
          CALL EASKAB(' Redirect verbose trace messages to file?:',' ',
     &        'yes','no',IW,1)
          if(IW.eq.1)then
            ICOUT=33
            tok=.true.
          else
            ICOUT=iuout
            tok=.false.
          endif 
        endif
      ELSEIF(INOS.EQ.17)THEN
        if(ixopen.eq.0)then
          write(xfile,'(2a)') zname(icomp)(1:lnblnk(zname(icomp))),
     &      '.txt'  ! initial file name
        endif
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T','Report',IER)
        if(ixopen.eq.1)then
          call usrmsg('Reports should be redirected to the file',
     &      'until you return and toggle this option.','W')
        endif
      ELSEIF(INOS.EQ.18)THEN

C Surface summary: print headder, followed by surface information.
        call tstamp('>','ISH: list surface attributes')
        context=.false.
        CALL SURINFO(ICOMP,iuout,context)
      ELSEIF(INOS.EQ.19)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('ish ',0)
      ELSEIF(INOS.EQ.20)THEN

C Gracefully exit program: if nothing has been done, delete the 
C transitional file; if a single month calculation has been done,
C then offer to save or delete; if a multi-month calculation has
C been done, all user to delete them.
        IUNIT=IFIL
        IF(MULTC.EQ.0)THEN
          CALL EFDELET(IUNIT,ISTAT)
        ELSEIF(MULTC.EQ.2)THEN
          dok=.true.
          h(1)='The transitional files were used during calculations'
          h(2)='and do not need to be kept. Probably you will want '
          h(3)='to delete these files. '
          CALL ASKOK(' ',' Delete the transitional files?',OK,dok,3)
          IRECW=MS+5
          if(OK)then
            do 55 ix=1,NCOMP
              do 56 im=1,12
                write(LNAME,'(A,A3)')zname(ix)(1:lnblnk(zname(ix))),
     &                MTHNAM(im)
                call EFOPRAN(IUNIT,LNAME,IRECW,0,IER)
                if(ier.eq.0)then
                  write(outs,'(A,A)') ' Deleting ',LNAME
                  call usrmsg(outs,' ','-')
                  CALL EFDELET(IUNIT,ISTAT)
                endif
  56          continue
  55        continue
          endif
        ENDIF
        call usrmsg(' Closing shading/insolation module...',' ','P')
        if(journio.eq.1)then
          call tstamp('>','Finish shading & insolation analysis')
          close(iuj)
        endif
        CALL EPAGEND
        STOP
      ELSE

C Not one of the legal menu choices.
        INOS=-1
        goto 201
      ENDIF
      goto  201

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902
  30  call edisp(iuout,'Error checking if file contained data.')
      goto 201

      END
      
C ********* SHDPRB
C Specify files - allow user to scan the configuration file or input
C file names manually.
      SUBROUTINE SHDPRB(ITC,silent,ICOMP,IER)

#include "building.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/FILEP/IFIL
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      LOGICAL MODIFY,MODLEN,MODBND,silent
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character h*72

      IUF=IFIL+1

      if(silent)then
        CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,1,0,IUOUT,IER)
      else
        CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,1,1,IUOUT,IER)
      endif
      IF(IER.NE.0)THEN
        CALL edisp(iuout,'Problem detected while trying to open/read')
        call edisp(iuout,'geom file... respecify in the main menu.')
        return
      endif
      NZSUR(ICOMP)=NSUR
      NZTV(ICOMP)=NTV

C Get name of obstruction file.
      IF(IOBS(ICOMP).EQ.1)THEN
        if(silent)then
          CALL EGOMST(IUF,ZOBS(icomp),0,0,iuout,IER)
        else
          CALL EGOMST(IUF,ZOBS(icomp),0,1,iuout,IER)
        endif
      ELSEIF(IOBS(ICOMP).NE.1)THEN
        if(silent)then
          call edisp(iuout,'No obstructions, only comput insolation.')
        else
          h(1)='When obstructions are not associated with a zone '
          h(2)='shading (on exterior surfaces) cannot be calculated. '
          h(3)='It is possible to calculate the distribution of '
          h(4)='(unshaded) insolation within the zone. '
          call easkab(' No obstruction file found:',' ',
     &      'return to project manager',
     &      'only look at insolation patterns',IW,4)
          if(IW.EQ.1)then
            CALL EPAGEND
            STOP
          endif
        endif
      ENDIF

C Get name of the shading insolation file.
      IF(ISI(ICOMP).EQ.1)THEN
        CALL SIFILE(ICOMP)
      ELSEIF(ISI(ICOMP).NE.1)THEN
        call usrmsg(' No shading:insolation db file found.',
     &    ' Please supply via the project manager...','W')
        call usrmsg('The shading and insolation module is',
     &              'now terminating...','-')
        call pauses(1)
        CALL EPAGEND
        STOP
      ENDIF

C Determine it's bounds for future comparison. Force it to be drawn.
      if(MMOD.eq.8)then
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ESCZONE(ICOMP)
        CALL BNDOBJ(0,IER)
        CALL ERCZONE(ICOMP)

        MODIFY=.TRUE.
        MODLEN=.TRUE.
        MODBND=.TRUE.
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(ITC,IER)
      endif

      return
      end

C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end
