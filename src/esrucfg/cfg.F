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

C cfg searches through a number of zones and attempts to derive the
C interconnections between them as a function of geometry.  This is a
C useful tool for proofing or making a first pass for building a
C connection list in a configuration file.

C Subroutines in cfg.F:
C  program cfg - the `main` of this application.
C  config - present close geometric matches to the user.
C  esacon - import surface attributes into connections list.
C  sfcrnr - returns number of `corners` in a surface.
C  lindir - returns alpha and theta angles of a line.
C  surbox - find bounding box and centre of a surface.

      program cfg
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "espriou.h"
C espriou.h provides currentfile.
      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C ITDSP labels toggle: all surf + obs = 0, all surf = 1, partn = 2,
C       similar = 4, surfs + obs+ ground = 5, ground only = 6
C ITBND bounds toggle: static = 0, optimum = 1, zone focus = 2
C ITEPT is
C ITZNM zone name toggle: display = 0, hidden = 1
C ITSNM surface name toggle: display = 0, hidden = 1
C ITORG origin toggle: display = 0, hidden = 1
C ITSNR surf normal toggle: display = 0, hidden = 1.
C ITOBS obstruction toggle: not yet enabled.
C ITHLS highlight toggle: normal 0, constr 1, trans/opaq 2, part atrib 3
C ITGRD grid toggle: display = 0, hidden = 1
C ITVNO vertex toggle: display = 0, hidden = 1
C ITPPSW current view - perspective/plan/south/west
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C3F/LCNN

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Path to problem.
      common/rpath/path
      common/uhome/upath
      common/udot/esprc

C Pause length during sequentail scan longpause (true=1 sec)
      common/scanpause/longpause

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12      
      
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND,OK,DOK,OTHEROK,XST
      logical M0SKP,M1SKP,M2SKP,M3SKP,M4SKP,M5SKP,M6SKP,M7SKP
      logical M1OK,M2OK,M4OK,M5OK,M6OK,M7OK
      logical unixok,there,prob,longpause

      DIMENSION ITEMS1(22)
      CHARACTER LCFGF*72,H*72,CFGFN*72,LTMP*144
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER ITEMS1*30,inf*144,CXSTR*78,outs*124,outs248*248
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character DCNN*72,LLCNN*72,LCNN*72
      character path*72,upath*72,esprc*72
      character dfdblbl*20,defdbfl*72
      character journcmd*20,jfile*72,cfgroot*24,cjfile*72,OUTSTR*124
      character uname*24,dstmp*24,fs*1,bl*2,MODE*4

C Initialize global common data.
      call ezero
      call curmodule('cfg ')
      cAppName = 'cfg'
      IUOUT=6
      IUIN=5
      ICOUT=0
      IFIL=11
      IFS=1
      ITFS=0
      IMFS=0
      NSIGFIG=3
      LLCNN=' '
      DCNN=' '
      bl='  '
      longpause=.true.
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, updated geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      IFCFG=IFIL+5
      IUF=IFIL+6
      IPRODB=IFIL+6
      iuj=IFIL+10

C Assume that all surfaces marked as identical, constant, ground and
C adiabatic require no confirmation (if no geometric match is found).
C Assume all existing partitions are re-checked and 
C failed searches brought to users attention.
      M0SKP=.FALSE.
      M1SKP=.TRUE.
      M2SKP=.TRUE.
      M3SKP=.FALSE.
      M4SKP=.TRUE.
      M5SKP=.TRUE.
      M6SKP=.TRUE.
      M7SKP=.TRUE.
      OTHEROK=.TRUE.

C Assume that the configuration, mlc db, primitive db, control file
C have not been read in.
      CFGOK=.FALSE.
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
      ITSNR=1
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0

C CACC is the tolerance between two vertices, IACC is the number
C of corners beyond tolerance which are allowed,
C DACC is the line degree tolerance for setting of corners.
      CACC=0.010
      IACC=0
      DACC=5.0

C Initialise output device, assume minimal trace, and a smallish
C display.  If passed zero size and offsets, use default. If size
C is <200 then take % of default, otherwise use passed width.
C If left & top offsets are 0 then use defaults.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=520
        iappx=80
        iappy=110
      else
        if(iappx.le.0)iappx=80
        if(iappy.le.0)iappy=110
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
        menuchw = MAX0(int(33*iappw*0.01),12)
        LIMTTY=8
        LIMIT =8
      else
        menuchw = 33
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
     &'ESP-r Topology Checker: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.  Problem
C not yet known so pass 0 to updwire [the wireframe control routine].
C If starting as a reduced percentage of default then widen the
C graphic display to compensate.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,2,1,1,2,igl,igr,igt,igb,igw,igwh)
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
C Debug
        write(6,*) 'mdispl,ifgrey,ncset,ngset,nzonec ',
     &    mdispl,nifgrey,ncset,ngset,nzonec
      ENDIF
      IF(MMOD.NE.-6)ICOUT=IUOUT

      call edisp(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Topology tool: Version 1.13a of January 2006.',
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow, Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,' ')

C Find the user's home folder then get user's custom settings.
      call edisp(IUOUT,' ')
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C If there is a journal active set this up (put journal file in the
C users home folcer as a 'dot' file in the form .user_name_pid_number.
C Use st2file to remove any blanks and wild cards.
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
        write(outs,'(a,a)')'Journal (topology analysis) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif
  902 continue

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take command line file name as initial configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LTMP=inf
        write(outs248,'(a,a)')' the input file is: ',LTMP
        call edisp248(iuout,outs248,80)
      else
        LTMP=' '
      endif

C If an input file has been specified then load it.
      XST=.false.
      if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
        INQUIRE (FILE=LTMP,EXIST=XST)
        if(XST)then

C Find the path and local file name.
          call fdroot(LTMP,path,LCFGF)
          MODE='NONE'
          IAPROB=IPRODB
          call ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
          call tstamp('>','CFG: scanning model')
          call tstamp('>',LCFGF)
          call pauses(1)
          if(IER.eq.0)then
            call ckcurmatch(prob,iprob,ier)
            CFGOK=.TRUE.
            if(MMOD.EQ.8)call updwire(ncomp)
            NZONES=NCOMP
            CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
            MODIFY=.TRUE.
          else
            call usrmsg(' The supplied configuration file had an',
     &       'error during scanning. Please check in project manager.',
     &       'W')
          endif
        endif
      endif

      ITRC=0
    3 INO=-4
      ITEMS1(1)=   'a problem definition         '
      ITEMS1(2)=   '  _____________________      '
      ITEMS1(3)=   'b set vertex tolerance       '
      ITEMS1(4)=   '  confirm if (already):      '
      if(M1SKP)then
        ITEMS1(5)= 'c   marked identical : No    '
      else
        ITEMS1(5)= 'c   marked identical : Yes   '
      endif
      if(M2SKP)then
        ITEMS1(6)= 'd   marked constant  : No    '
      else
        ITEMS1(6)= 'd   marked constant  : Yes   '
      endif
      if(M3SKP)then
        ITEMS1(7)= 'e   marked partition : No    '
      else
        ITEMS1(7)= 'e   marked partition : Yes   '
      endif
      if(M4SKP)then
        ITEMS1(8)= 'f   marked ground    : No    '
      else
        ITEMS1(8)= 'f   marked ground    : Yes   '
      endif
      if(M5SKP)then
        ITEMS1(9)= 'g   marked adiabatic : No    '
      else
        ITEMS1(9)= 'g   marked adiabatic : Yes   '
      endif
      if(M6SKP)then
        ITEMS1(10)='h   marked BASESIMP  : No    '
      else
        ITEMS1(10)='h   marked BASESIMP  : Yes   '
      endif
      if(M7SKP)then
        ITEMS1(11)='h   marked IDENT_CEN  : No   '
      else
        ITEMS1(11)='h   marked IDENT_CEN  : Yes  '
      endif
      if(M0SKP)then
        ITEMS1(12)='i   if no match found: No    '
      else
        ITEMS1(12)='i   if no match found: Yes   '
      endif
      ITEMS1(13)=  '  _____________________      '

      ITEMS1(14)=  'p edit individual connections'
      ITEMS1(15)=  'q clear some/all contiguity  '
      ITEMS1(16)=  'r check via vertex contiguity'
      ITEMS1(17)=  '  _____________________      '

      IF(ITRC.EQ.0)THEN
        ITEMS1(18)='s reporting >> silent        '
      ELSEIF(ITRC.EQ.1)THEN  
        ITEMS1(18)='s reporting >> summary       ' 
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS1(18)='s reporting >> verbose       '
      ENDIF
      ITEMS1(19)=  '! list connections           '
      if(longpause)then
        ITEMS1(20)='* pause during check 1 sec   '
      else
        ITEMS1(20)='* pause during check 0.5 sec '
      endif
      ITEMS1(21)=  '? help                       '
      ITEMS1(22)=  '- exit program               '
      MITEM=22

C If user has defined problem and perhaps resized the display then
C redraw the problem image.
      if(CFGOK.AND.MODIFY)then
        MODBND=.TRUE.
        MODLEN=.TRUE.
        nzg=NZONES
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE

C (Re)Set all surfaces to standard line width.
        izgfoc=0
        if(MMOD.eq.8)then
          CALL INLNST(1)
          CALL ADJVIEW(ITRC,IER)
        endif
      endif

C Use askabout to instanciate the initial help messages (2nd parameter is one).
  2   call askabout('cfg ',1)

      CALL EMENU('  Topology',ITEMS1,MITEM,INO)
      IF(INO.EQ.MITEM)THEN
        call usrmsg(' Closing topology tool...',' ','P')
        if(journio.eq.1)then
          call tstamp('>','Finish topology analysis')
        endif
        CALL EPAGEND
        STOP
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('cfg ',0)
      ELSEIF(INO.EQ.MITEM-2)THEN

C Swop the pause length.
        if(longpause)then
          longpause=.false.
        else
          longpause=.true.
        endif
      ELSEIF(INO.EQ.MITEM-3)THEN

C List connections.
        call tstamp('>','CFG: list topology')
        WRITE(outs,'(A,I3)')' Number of connections = ',NCON
        CALL EDISP(iuout,' ')
        CALL EDISP(iuout,outs)
        CALL CONXINF(1,0,CXSTR)
        CALL EDISP(iuout,CXSTR)
        DO 21 ICON=1,NCON
          CALL CONXINF(1,ICON,CXSTR)
          CALL EDISP(iuout,CXSTR)
   21   CONTINUE
      ELSEIF(INO.EQ.1)THEN
        H(1)='The system configuration file holds the definition of'
        H(2)='the building/plant to be simulated, including the'
        H(3)='names of all of the files required. If the name given'
        H(4)='matches an existing file it will be read in and the'
        H(5)='data will be available for editing.'
  289   llt=lnblnk(ltmp)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(ltmp,' Model configuration file ?',' ',96,
     &        DFCFG,'config file name',IER,5)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(ltmp,' Model configuration file ?',' ',124,
     &        DFCFG,'config file name',IER,5)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(ltmp,' Model configuration file ?',' ',144,
     &        DFCFG,'config file name',IER,5)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(ltmp,' Model configuration file ?',' ',144,
     &      DFCFG,'config file name',IER,5)
        else
          CALL EASKF(ltmp,' Model configuration file ?',' ',96,
     &      DFCFG,'config file name',IER,5)
        endif

        call usrmsg('File selected is',ltmp,'-')
        IF(LTMP.EQ.' ')GOTO 289

C Find the path and local file name, if ersys sucessful report to
C user on any mis-matches discovered.
        call fdroot(LTMP,path,LCFGF)
        call edisp(iuout,' ')
        call edisp(iuout,' Scanning the model description')
        MODE='NONE'
        IAPROB=IPRODB
        call ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
        if(IER.eq.0)then
          call ckcurmatch(prob,iprob,ier)
          if(prob)then
            call usrmsg('Problems detected, probably best to `clear`',
     &        'the current topology. ','W')
            call tstamp('>','CFG: found problem in contiguity')
          endif
          CFGOK=.TRUE.
          if(MMOD.eq.8)call updwire(ncomp)
        else
          goto 289
        endif

C Read all of the vertex information.
        IF(CFGOK)THEN
          NZONES=NCOMP
        ELSE
          call usrmsg('Configuration not defined or not correct',
     &      'so contiguity cannot be established.','W')
          goto 3
        ENDIF
        call edisp(iuout,' Loading the geometry')
        CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
        MODIFY=.TRUE.
      ELSEIF(INO.EQ.3)THEN

C Set tolerances for vertex comparison.
        call tstamp('>','CFG: enter tolerances')
        H(1)='The tolerance is expressed in metres and applies to'
        H(2)='the X, Y & Z of each vertex.'
        CALL EASKR(CACC,' ',' Tolerance (metres) ? ',
     &             0.001,'W',0.2,'W',0.001,'dist tolerance',IER,2)

        H(1)='The tolerance is expressed in degrees and allows the'
        H(2)='user to control how much of a change in direction'
        H(3)='along a line constitutes a surface corner.  If set '
        H(4)='too small, it will pick up minor vertices along a '
        H(5)='side as corners. '
        CALL EASKR(DACC,' ',' Corner tolerance (degrees) ?',
     &             0.1,'W',5.0,'W',1.0,'degree tolerance',IER,5)

        H(1)='Within cfg the shape of a surface is determined by'
        H(2)='the number of corners within the surface polygon.'
        H(3)='Vertices which lie along a line are not counted unless'
        H(4)='the angle of the line changes by several degrees.'
        H(5)='If the difference is 0 then both surfaces must have '
        H(6)='the same number of corners and they must be within '
        H(7)='the distance tolerance specified.  Increasing this'
        H(8)='number will allow for one or more of the corners to'
        H(9)='be out of tolerance and still be presented to the'
        H(10)='user for confirmation. '
        CALL EASKI(IACC,' Number of corners which are allowed outside',
     &    ' of the distance tolerance ? ',
     &     0,'F',2,'W',1,'corner tolerance',IER,10)

      ELSEIF(INO.EQ.5)THEN

C Flip logic for identical.
        call tstamp('>','CFG: toggle identical')
        if(M1SKP)then
          M1SKP=.false.
        else
          M1SKP=.true.
        endif
      ELSEIF(INO.EQ.6)THEN

C Flip logic for constant.
        call tstamp('>','CFG: toggle constant')
        if(M2SKP)then
          M2SKP=.false.
        else
          M2SKP=.true.
        endif
      ELSEIF(INO.EQ.7)THEN

C Flip logic for partitions.
        call tstamp('>','CFG: toggle partitions')
        if(M3SKP)then
          M3SKP=.false.
        else
          dok=.false.
          h(1)='Typically, you would want to include existing '
          h(2)='partitions in the list of surfaces to check.'
          h(3)='You are allowed to exclude them even though it '
          h(4)='is a dangerous option for expert users. '
          call askok('Are you sure you want to skip marked partitions?',
     &               '(see help)',OK,dok,4)
          if(OK)M3SKP=.true.
        endif
      ELSEIF(INO.EQ.8)THEN

C Flip logic for ground.
        call tstamp('>','CFG: toggle ground')
        if(M4SKP)then
          M4SKP=.false.
        else
          
          M4SKP=.true.
        endif
      ELSEIF(INO.EQ.9)THEN

C Flip logic for adiabatic.
        call tstamp('>','CFG: toggle adiabatic')
        if(M5SKP)then
          M5SKP=.false.
        else
          M5SKP=.true.
        endif
      ELSEIF(INO.EQ.10)THEN

C Flip logic for case of BASESIMP or no match found.
        call tstamp('>','CFG: toggle no match found')
        if(M6SKP)then
          M6SKP=.false.
        else
          M6SKP=.true.
        endif
      ELSEIF(INO.EQ.11)THEN

C Flip logic for case of CEN 13791 partition or no match found.
        call tstamp('>','CFG: toggle no match found')
        if(M7SKP)then
          M7SKP=.false.
        else
          M7SKP=.true.
        endif
      ELSEIF(INO.EQ.12)THEN

C Flip logic for case of UNKNOWN or no match found.
        call tstamp('>','CFG: toggle no match found')
        if(M0SKP)then
          M0SKP=.false.
        else
          H(1)='This option will result in any surface which does'
          H(2)='not conform to the geometric matching criteria to be'
          H(3)='assumed to have an EXTERNAL boundary condition.'
          H(4)='If you want to choose the boundary condition then'
          H(5)='you should toggle `no match found` to YES.'
          CALL PHELPD('cfg import atrib',5,'-',0,0,IER)
          M0SKP=.true.
        endif
      ELSEIF(INO.EQ.14)THEN

C Edit single connection.
        call edaconn(itrc,ier)
      ELSEIF(INO.EQ.15)THEN

C Clear contiguity.
        h(1)='In restructuring a model it is sometimes useful to '
        h(2)='redefine the topology.'
        h(3)=' '
        h(4)='The `clear subset` asks for each type of connection '
        h(5)='(partitions/identical etc.) to clear. '
        h(6)=' '
        h(7)='The `clear all` will rebuild the whole master list '
        h(8)='and this should be done if the list has been corrupted.'
        CALL EASKABC(' Topology Options:',' ','clear all topology',
     &   'clear subset of topology.','continue',IRT,9)
        if(IRT.eq.1)then
          dok=.false.
          call askok('About to clear ALL contiguity.','Is this ok',
     &      OK,dok,8)
          if(.NOT.OK)goto 3
          call tstamp('>','CFG: clearing contiguity')
          call edisp(iuout,'Clearing contiguity... ')
          ICC=0
          do 87 IZ=1,NCOMP
            CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
            nzsur(iz)=nsur
            do 88 IS=1,NSUR
              ICC=ICC+1
              IC1(ICC)=IZ
              IE1(ICC)=IS
              ICT(ICC)=0
              IC2(ICC)=0
              IE2(ICC)=0
   88       continue
   87     continue
          NCON=ICC
          write(outs,'(a,i4)')'No. of connections now = ',NCON
          call edisp(iuout,outs)

C Check if user wants to re-establish connection data from surface
C attributes.
          H(1)='After clearing contiguity you can re-establish some of'
          H(2)='the connections via surface attributes (adiabetic,'
          H(3)='similar, constant and ground connections). If these.'
          H(4)='attributes are YES in the list of boundary type'
          H(5)='toggles (choices c-h) then you will be asked for '
          H(6)='confirmation. '
          H(7)=' '
          H(8)='Answering yes to this question can save you lots of '
          H(9)='time. '
          dok=.true.
          call askok('Recover surface attribute data?',
     &      '(adiabetic,similar, constant etc. see help)',OK,dok,9)
          if(OK)then
            call tstamp('>','CFG: import surface attributes')
            call ESACON(ITRC,M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP,IER)
          endif
        elseif(IRT.eq.2)then
          call tstamp('>','CFG: clearing partial contiguity')
          h(1)='The `clear subset` asks for each type of connection '
          h(2)='(partitions/identical etc.) to clear. '
          h(3)=' '
          h(4)='Say yes to SIMILAR and/or CONSTANT and/or GROUND'
          h(5)='and/or ADIABETIC if you want to clear connections'
          h(6)='of surfaces matching these boundary types. '
          dok=.false.
          call askok(' ','Clear items marked SIMILAR?',M1OK,dok,6)
          call askok(' ','Clear items marked CONSTANT?',M2OK,dok,6)
          call askok(' ','Clear items marked GROUND?',M4OK,dok,6)
          call askok(' ','Clear items marked ADIABETIC?',M5OK,dok,6)
          call askok(' ','Clear items marked BASESIMP?',M6OK,dok,6)
          call askok(' ','Clear items marked IDENT_CEN?',M7OK,dok,6)
          call usrmsg('Clearing partial contiguity...',bl,'-')
          ICC=0
          do 187 IZ=1,NCOMP
            CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
            nzsur(iz)=nsur
            do 188 IS=1,NSUR
              if(ICT(ICC).eq.1.and.M1OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.2.and.M2OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.4.and.M4OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.5.and.M5OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.6.and.M6OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.7.and.M7OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              else
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              endif
  188       continue
  187     continue
          NCON=ICC
          write(outs,'(a,i4)')'No. of connections now = ',NCON
          call edisp(iuout,outs)

C Check if user wants to re-establish connection data from surface
C attributes.
          H(1)='After clearing contiguity you can re-establish some of'
          H(2)='the connections via surface attributes (adiabetic,'
          H(3)='similar, constant and ground connections). If these.'
          H(4)='attributes are YES in the list of boundary type'
          H(5)='toggles (choices c-h) then you will be asked for '
          H(6)='confirmation. '
          H(7)='This facility might save lots of time. '
          call askok('Recover surface attribute data?',
     &      '(adiabetic,similar, constant etc. see help)',OK,dok,7)
          if(OK)then
            call tstamp('>','CFG: import surface attributes')
            call ESACON(ITRC,M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP,IER)
          endif
        elseif(IRT.eq.3)then
          goto 3
        endif

C Save current common block information to file, jump back and
C update the display.  If icfgv = 3 then only need to update
C the connections file.  If it changes also update the configuration file.
        if(icfgv.eq.3)then
  286     H(1)='The surface topology of the problem is held'
          H(2)='in a connections file.'
          write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(LCNN(1:1).eq.' ')then
            LCNN=DCNN
            LLCNN=LCNN
          else
            LLCNN=LCNN
          endif
          CALL EASKS(LCNN,' Updated surface connections file ?',' ',
     &      72,DCNN,'system connx file name',IER,2)
          IF(LCNN.EQ.' ')GOTO 286

C If overwriting the same connections file then open it and write.
          if(LCNN.eq.LLCNN)then
            CALL EFOPSEQ(IUF,LCNN,3,IER)
            write(currentfile,'(a)') LCNN(1:lnblnk(LCNN))
            WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=2) '*connections  for ',
     &        cfgroot(1:lnblnk(cfgroot))
            call dstamp(dstmp)
            WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
            WRITE(IUF,'(I7,A)',IOSTAT=IOS,ERR=2)
     &        NCON,'   # number of connections'
            DO 931 I = 1,NCON
              CALL CONXINF(1,I,CXSTR)
              if(icfgv.eq.3)then
                WRITE(IUF,'(5I4,A,A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &           ICT(I),IC2(I),IE2(I),'  # ',CXSTR(:lnblnk(CXSTR))
              else
               WRITE(IFCFG,'(5I4,A,A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &           ICT(I),IC2(I),IE2(I),'  # ',CXSTR(:lnblnk(CXSTR))
              endif
  931       CONTINUE

C Re-build zone:surface to connection hash array.
            CALL ERPFREE(IUF,IOS)
            do 45 icc = 1, NCON
              IZSTOCN(IC1(icc),IE1(icc))=icc
 45         continue
          else
            CALL EMKCFG('-',IER)
          endif
        elseif(icfgv.lt.3)then

C Older configuration file format, ask for its name.
          H(1)='The current problem description will be placed'
          H(2)='into this file. It is suggested that you define'
          H(3)='a new file (ie. project.cfg > project_a.cfg).'
          CFGFN=LCFGF
          CALL EASKS(CFGFN,
     &     'Updated configuration file name ? (best if a new file) ',
     &     ' ',72,'xxx.cfg','revised config file',IER,3)
          if(CFGFN.ne.' ')LCFGF=CFGFN
          CALL EMKCFG('-',IER)
        endif
      ELSEIF(INO.EQ.16)THEN

C Check vertex matches and rebuild configuration file. If icfgv = 3 then
C only need to update the connections file. If it changes also update
C the configuration file.
        ITSNM=0
        if(icfgv.eq.3)then
  285     H(1)='The surface topology of the problem is held'
          H(2)='in a connections file.'
          write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(LCNN(1:1).eq.' ')then
            LCNN=DCNN
            LLCNN=LCNN
          else
            LLCNN=LCNN
          endif
          CALL EASKS(LCNN,' Updated surface connections file ?',' ',
     &      72,DCNN,'system connx file name',IER,2)
          IF(LCNN.EQ.' ')GOTO 285
        elseif(icfgv.lt.3)then

C Older configuration file format, ask for its name.
          H(1)='The current problem description will be placed'
          H(2)='into this file. It is suggested that you define'
          H(3)='a new file (ie. project.cfg > project_a.cfg).'
          CFGFN=LCFGF
          CALL EASKS(CFGFN,
     &     'Updated configuration file name ? (best if a new file) ',
     &     ' ',72,'xxx.cfg','revised config file',IER,3)
          if(CFGFN.ne.' ')LCFGF=CFGFN
        endif

C Look for connections.
        call tstamp('>','CFG: do graphic match of surfaces')
        CALL CONFIG(NZONES,CACC,IACC,DACC,M0SKP,M1SKP,M2SKP,M3SKP,
     &    M4SKP,M5SKP,M6SKP,M7SKP,OTHEROK)

C Save data to configuration file.
C If overwriting the same connections file then open it and write.
        if(icfgv.eq.3)then
          if(LCNN.eq.LLCNN)then
            CALL EFOPSEQ(IUF,LCNN,3,IER)
            write(currentfile,'(a)') LCNN(1:lnblnk(LCNN))
            WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=2) '*connections  for ',
     &        cfgroot(1:lnblnk(cfgroot))
            call dstamp(dstmp)
            WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
            WRITE(IUF,'(I7,A)',IOSTAT=IOS,ERR=2)
     &        NCON,'   # number of connections'
            DO 930 I = 1,NCON
              CALL CONXINF(1,I,CXSTR)
              if(icfgv.eq.3)then
                WRITE(IUF,'(5I4,A,A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &           ICT(I),IC2(I),IE2(I),'  # ',CXSTR(:lnblnk(CXSTR))
              else
                WRITE(IFCFG,'(5I4,A,A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &           ICT(I),IC2(I),IE2(I),'  # ',CXSTR(:lnblnk(CXSTR))
              endif
  930       CONTINUE

C Re-build zone:surface to connection hash array.
            CALL ERPFREE(IUF,IOS)
            do 43 icc = 1, NCON
              IZSTOCN(IC1(icc),IE1(icc))=icc
 43         continue
          else
            CALL EMKCFG('-',IER)
          endif
        else
          CALL EMKCFG('-',IER)
        endif
      ELSEIF(INO.EQ.18)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
        INO=-4
      ELSE
        INO=-1
        GOTO 2
      ENDIF
      GOTO 3

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      END

C ************** CONFIG
C CONFIG processes the vertex data and presents close matches for
C the user to confirm before writing to temporary configuration file.
      SUBROUTINE CONFIG(NZONES,CACC,IACC,DACC,M0SKP,M1SKP,M2SKP,M3SKP,
     &  M4SKP,M5SKP,M6SKP,M7SKP,OTHEROK)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6/LINSTY(MCON)

      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      common/ground/grdtmp(12,mgrdp)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)

C Pause length during sequentail scan longpause (true=1 sec)
      common/scanpause/longpause

      DIMENSION COORD1(MV,3), COORD2(MV,3)
      dimension ICVALS(MCOM),TOTHER(MS),ipmz(3),ipms(3)

      LOGICAL MODIFY,MODLEN,MODBND,OK,DOK,DOIT,OTHEROK,UPDGEO
      logical M0SKP,M1SKP,M2SKP,M3SKP,M4SKP,M5SKP,M6SKP,M7SKP
      logical closea,closee,close,longpause
      character outs*124,CXSTR*78,zname*12,SN*12,ZN*12,SO*12,TOTHER*12
      character SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,DESCRC*25
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,H
      character LCNN*72,DCNN*72,cfgroot*24,LCFGF*72,zdesc*64,bl*2

C CACC is the tolerance allowed between two vertices.
C TOTHER is array of temporary new topology attributes for current zone.

C ICNT holds the number of matching vertices between two surfaces.
      bl='  '
      ITSNM=0
      ICNT=0
      IC=0

C Set for redraw if image on first entry (if refresh set to after
C each edit).
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.

C Request which zones to include.
      H(1)=' Pick one, several or all zones for inclusion'
      ICPIC=NZONES
      CALL EPICKS(ICPIC,ICVALS,' ',' Which zones to include: ',
     &  12,NZONES,zname,' zone list',IER,1)

C Loop for each zone, only doing comparison for selected zones.
      DO 100 I=1,NZONES
        IZ=I
        DOIT=.FALSE.
        UPDGEO=.FALSE.
        do 215 ix=1,icpic
          if(IZ.eq.ICVALS(ix))DOIT=.TRUE.
  215   continue
        if(DOIT)then

C Get confirmation for continuing, if user wishes to skip a
C selected zone then updae the IC count to reflect the surfaces
C skiped.
          if(IZ.ne.1)then
            write(outs,'(3a)')' Continue with ',zname(IZ),' ?'
            dok=.true.
            h(1)='Normally you would check all of the zones. And '
            h(2)='if you need to exit this process (ran out of '
            h(3)='coffee) this is a place you can do this. NOTE '
            h(4)='that this can leave the model in an inconsistent'
            h(5)='state. '
            CALL ASKOK(outs,'(see help)',OK,dok,5)
            if(.NOT.OK)then
              IC=IC+nzsur(IZ)
              goto 100
            endif
          endif
          WRITE(outs,'(A,I2,A,A)') 'Processing (',IZ,') ',zname(IZ)
          call edisp(iuout,outs)
          call edisp(iuout,' corners|connx| description ')
          call edisp(iuout,' in surf|index| of existing topology')
        endif

C Display the reference zone (if type 3 then display both).
C Remember topology attributes for reference zone.
        IUO=IFIL+1
        CALL EGOMIN(IUO,LGEOM(IZ),IZ,0,0,iuout,IER)
        do 322 IJ=1,NSUR
          TOTHER(IJ)=SOTHER(IJ)(1:12)
  322   continue
        MODIFY=.TRUE.
        MODBND=.TRUE.

C Loop over each surface.
        DO 110 J=1,nzsur(IZ)
          JS=J
          IC=IC+1

C If zone is not to be included in the analysis, jump.
          if(.NOT.DOIT)goto 110

C Reset all surface lines to standard width and then the specific 
C surface(s) under consideration to wide lines. If other side
C is not a type 3 then don`t highlight.
          if(MMOD.eq.8)then
            CALL INLNST(1)
            LINSTY(IC)=2
          endif
          nzg=1
          nznog(1)=IZ
          izgfoc = IZ
          if(MMOD.eq.8)CALL CADJVIEW(0,IER)
          MODIFY=.TRUE.

C Find number of corners and their coordinates for this surface.
          CALL SFCRNR(DACC,IZ, JS, NAPIC1, COORD1)
          CALL CONXINF(1,IC,CXSTR)
          write(outs,'(I6,3X,A)') NAPIC1,CXSTR
          call edisp(iuout,outs)

C If user said skip marked partitions, check and update the
C surface attribute if it does not match and jump around.
          if(M3SKP.and.(ICT(IC).eq.3))then
            call edisp(iuout,'known connection skipped.')
            if(MMOD.eq.8)then
              if(longpause)then
                call pauses(1)
              else
                call pausems(400)
              endif
            endif
            ZN=zname(IC2(IC))
            WRITE(SO,'(A)')TOTHER(JS)
            if(SO(1:lnblnk(SO)).ne.ZN(1:lnblnk(ZN)))then
               TOTHER(JS)=ZN
               UPDGEO=.TRUE.
            endif
            goto 110
          endif

C Reset number of geometric matches. If more than one then ask.
          ipmatch=0

C Loop over other zones.
          DO 200 II=1, NZONES
            IF (II.EQ.IZ) GOTO 200

C Find if there is a surface connection.
            DO 210 JJ=1,nzsur(II)

C Find number of corners and their coordinates for the other surface.
              CALL SFCRNR(DACC,II, JJ, NAPIC2, COORD2)

C If the number of corners of the two surface are equal then
C check if their coordinates are within tolerance.
              ICNT=0
              IF (NAPIC1.EQ.NAPIC2) THEN
                DO 300 K=1, NAPIC1
                  DO 350 KK=1, NAPIC1
                    XDIFF=ABS(COORD2(KK,1)-COORD1(K,1))
                    YDIFF=ABS(COORD2(KK,2)-COORD1(K,2))
                    ZDIFF=ABS(COORD2(KK,3)-COORD1(K,3))
                    IF(XDIFF.LE.CACC.AND.YDIFF.LE.CACC.AND.
     &                 ZDIFF.LE.CACC) THEN
                      ICNT=ICNT+1
                      GOTO 300
                    ENDIF
  350             CONTINUE
  300           CONTINUE
              ENDIF

C If the number of matching corners is within the IACC limit, then
C present choice to the user.
              IF ((NAPIC1-ICNT).le.IACC) THEN
                ipmatch=ipmatch+1
                if(ipmatch.le.3)then
                  ipmz(ipmatch)=II
                  ipms(ipmatch)=JJ
                endif
              ENDIF

C Reset ICNT before commencing a new loop.
              ICNT=0
  210       CONTINUE
  200     CONTINUE

C Depending on how many matches were found, proceed.
          if(ipmatch.eq.0)then

C If user said skip marked IDENTICAL, check and update the
C surface attribute if it does not match and jump around.
            WRITE(SO,'(A)')TOTHER(JS)
            if(M1SKP.and.(ICT(IC).eq.1))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:7).ne.'SIMILAR')then
                TOTHER(JS)='SIMILAR'
                UPDGEO=.TRUE.
              endif
            goto 110
            endif

C If user said skip marked CONSTANT, check and update the
C surface attribute if it does not match and jump around.
            if(M2SKP.and.(ICT(IC).eq.2))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:8).ne.'CONSTANT')then
                TOTHER(JS)='CONSTANT'
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip marked GROUND, check and update the
C surface attribute if it does not match and jump around.
            if(M4SKP.and.(ICT(IC).eq.4))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:6).ne.'GROUND')then
                TOTHER(JS)='GROUND'
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip marked ADIABATIC, check and update the
C surface attribute if it does not match and jump around.
            if(M5SKP.and.(ICT(IC).eq.5))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:9).ne.'ADIABATIC')then
                TOTHER(JS)='ADIABATIC'
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip marked BASESIMP, check and update the
C surface attribute if it does not match and jump around. 
            if(M6SKP.and.(ICT(IC).eq.6))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:8).ne.'BASESIMP')then
                TOTHER(JS)='BASESIMP'
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip marked IDENT_CEN, check and update the
C surface attribute if it does not match and jump around. 
            if(M7SKP.and.(ICT(IC).eq.7))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:9).ne.'IDENT_CEN')then
                TOTHER(JS)='IDENT_CEN'
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C Ask user if some other type of connection. If M0SKP is true then
C set connection to exterior and proceed.
            if(OTHEROK)then
              if(M0SKP)then
                IC1(IC)=IZ
                IE1(IC)=JS
                ICT(IC)=0
                IE2(IC)=0
                IC2(IC)=0
                if(SO(1:8).ne.'EXTERIOR')then
                  TOTHER(JS)='EXTERIOR'
                  UPDGEO=.TRUE.
                endif
                call edisp(iuout,'no match found setting to EXTERIOR.')
                if(MMOD.eq.8)then
                  if(longpause)then
                    call pauses(1)
                  else
                    call pausems(400)
                  endif
                endif
              else

C Setup contextural help for boundary conditions.
        H(1) ='The boundary conditions at the other face of this sur-'
        H(2) ='face is specified via the selection of one of these'
        H(3) ='choices.  The default is OUTSIDE.'
        H(4) =' '
        H(5) =' Exterior - the other face of the surface is exposed to'
        H(6) ='  ambient air temperature, wind and solar radiation.'
        H(4) =' '
        H(5) =' Dynamic/similar - the other face of the surface is'
        H(6) ='  exposed to the same conditions as the inside face'
        H(7) ='  (except that when direct sunlight is falling on the'
        H(8) ='  inside face it cannot be falling on the other face'
        H(9) ='  only the diffuse component is used on the other face.'
        H(10)='  where both sides get the same radiation.'
        H(11)='  A dynamic/similar connection can include an offset'
        H(12)='  temperature (C) and radiation (W/m^2).'
        H(13)=' '
        H(14)=' Static - requires that a constant temperature and'
        H(15)='  radiation be supplied. You might use this to represent'
        H(16)='  a cold storage room.'
        H(17)=' '
        H(18)=' Surface in another zone - the other face of the surface'
        H(19)='  gets its boundary conditions from the last known state'
        H(20)='  of the other surface. There is a thermophysical con-'
        H(21)='  nection established between the two zones.'
        H(22)=' '
        H(23)=' Ground (standard profile) - the other face of the'
        H(24)='  surface is in direct contact with one of the standard'
        H(25)='  monthly temperature profiles embedded in ESP-r.'
        H(26)=' '
        H(27)=' Ground (user defined profile) - the other face of the'
        H(28)='  surface is in direct contact with a monthly profile'
        H(29)='  previously created by the user (in the model context)'
        H(30)=' '
        H(31)=' Ground (3D conduction) - the other face connects to a'
        H(32)='  3D ground representation. This is not often used.'
        H(33)=' '
        H(34)=' Adiabatic boundary - typically used for back-to-back'
        H(35)='  surfaces in a zone representing thermal mass. There'
        H(36)='  is no heat flux passed beyond the other-face. '
        H(37)=' '
        H(38)=' BASESIMP is used to represent one of several basement'
        H(39)='  configurations and precalculated patterns of heat'
        H(40)='  transfer. For this to be useful there must be a'
        H(41)='  basesimp file created for the zone giving a number'
        H(42)='  of dimensions and computational parameters.'
        H(43)=' '
        H(44)=' CEN 13791 partition which is like the SIMILAR'
        H(45)='  but for CEN 13791 test. The offset temperauture'
        H(46)='  and radiation (W/m^2) in SIMILAR is not available'
        H(47)='  for the CEN 13791 partition type. For details see'
        H(48)='  section 4.4.2 CEN 13791 standard. '
        H(49)=' '
        H(50)='There is a module of ESP-r which uses polygon adjacency'
        H(51)='to automatically link partitions together. '
        H(52)=' '
                if(ICT(IC).eq.3)then
                  write(outs,'(a,a)') 
     &              'Partition, but no geometric match found. Is ',
     &              CXSTR(1:lnblnk(CXSTR))
                elseif(ICT(IC).ne.3)then
                  write(outs,'(a,a)')'No geometric match found. Is ',
     &              CXSTR(1:lnblnk(CXSTR))
                  idno=ICT(IC)
                endif
                iart=0
                call MENUATOL(outs,' Connection options ',
     &  'a exterior                  ','b dynamic (similar) boundary',
     &  'c static boundary conditions','d ground (standard profile) ',
     &  'e ground (user def profile) ','f ground (3D conduction)    ',
     &  'g adiabatic (no heat flux)  ','h BASESIMP foundation       ',
     &  'i CEN 13791 partition       ',
     &  'j UNKNOWN (at this time)    ','k accept current            ',
     &   ' ',iart,idno,52)
                if(IART.eq.0)then
                  continue
                elseif(IART.eq.1)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=0
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:8).ne.'EXTERIOR')then
                    TOTHER(JS)='EXTERIOR'
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.2)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=1
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:7).ne.'SIMILAR')then
                    TOTHER(JS)='SIMILAR'
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.3)then
                  H(1)='A type 2 connection requires that a constant'
                  H(2)='temperature and radiation (Watts) be entered.'
                  CALL EASKR(VAL,' ',' Constant temperature ? ',
     &              -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
                  CALL EASKR(VAL2,' ',' Constant rad (W/m^2) ? ',
     &              -999.,'F',999.,'F',0.,'adjacent rad',IER,2)
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=2
                  IE2(IC)=INT(VAL2)
                  IC2(IC)=INT(VAL)
                  if(SO(1:8).ne.'CONSTANT')then
                    TOTHER(JS)='CONSTANT'
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.4)then

C List out standard profiles, assuming that if June is 0.0, then the
C profile has not been defined.
                  CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
                  CALL EDISP(iuout,
     &              'id  Jan, Feb, Mar, Apr, May, Jun, Jul...')
                  do 21 igrdp=1,mgrdp
                    CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
                    if(.NOT.close)then
                      WRITE(OUTS,'(I2,12F5.1)')igrdp,
     &                  (GRDTMP(JG,IGRDP),JG=1,12)
                      call edisp(iuout,outs)
                    endif
21                continue
                  H(1)='A ground connection allows either:'
                  H(2)='> 0 a connection to one of several standard'
                  H(3)='    profiles. Supply its index.'
                  H(4)='= 0 a connection to a user defined profile. '
                  H(5)='    supply 0 plus user profile index. '
                  H(6)='-3  a connection to a 3D ground model.'
                  iic2=ic2(IC)
  777             CALL EASKI(IIC2,' Monthly (standard) profile index?',
     &              ' ',1,'F',12,'F',1,'ground profile',IER,7)
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=4
                  IF(IIC2.GT.0)THEN
                    IC2(IC)=iic2
                    IE2(IC)=0
                    if(SO(1:6).ne.'GROUND')then
                      TOTHER(JS)='GROUND'
                      UPDGEO=.TRUE.
                    endif
                  ELSE
                    CALL USRMSG(' ',' Unacceptable ground profile...',
     &  'W')
                    GOTO 777
                  ENDIF
                elseif(IART.eq.5)then

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
                  if(NGRDP.gt.0)then
                    CALL EDISP(iuout,' ')
                    do 22 igrdp=1,NGRDP    
                      CALL EDISP(iuout,
     &                  'User defined ground profile Jan-Dec:')
                      WRITE(OUTS,'(12F6.1)')(UGRDTP(JG,IGRDP),JG=1,12)
                      call edisp(iuout,outs)
 22                 continue
                    H(1)='One of the user defined profile numbers is'
                    H(2)='required at this point.'
                    CALL EASKI(IIE2,
     &                'User defined ground profile index?',
     &                ' ',0,'F',9,'F',1,'user defined profile',IER,2)
                    IC1(IC)=IZ
                    IE1(IC)=JS
                    ICT(IC)=4
                    IC2(IC)=0
                    IE2(IC)=iie2
                    if(SO(1:6).ne.'GROUND')then
                      TOTHER(JS)='GROUND'
                      UPDGEO=.TRUE.
                    endif
                  else
                    call usrmsg(
     &               'No user defined ground profiles have been found.',
     &               'Go to `problem context` to define these.','W')
                  endif
                elseif(iart.eq.6)then 

C Link to a 3D conduction model..
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=4
                  IC2(IC)=-3
                  IE2(IC)=0
                  if(SO(1:6).ne.'GROUND')then
                    TOTHER(IE1(JS))='GROUND'
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.7)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=5
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:9).ne.'ADIABATIC')then
                    TOTHER(JS)='ADIABATIC'
                    UPDGEO=.TRUE.
                  endif
                elseif(iart.eq.8)then 

C BASESIMP start.
C Get BASESIMP configuration via call to bsimtype. Save state of current
C menu and recover after the call.
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=6
                  H(1)='Specify the BASESIMP configuration index.'
                  H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the '
                  H(3)='correspondence between iconfig and standard '
                  H(4)='BASESIMPdescriptors (e.g. BCIN_1).'
                  IBS=IC2(IC)
                  call bsimtype(ibs)
                  IC2(IC)=IBS

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration.
                  Ifrac=IE2(IC)
          H(1)='BASESIMP is a `whole-foundation model`. Therefore, when'
          H(2)='a foundation is represented by numerous ESP-r surfaces'
          H(3)='(e.g. four walls + floor to represent a basement), you'
          H(4)='must specify the percentage of the whole-foundation'
          H(5)='heat loss to attribute to each surface. Normally the'
          H(6)='sum of surfaces representing a founcation is 100%.'
                  CALL EASKI(Ifrac,
     &              ' Percent of BASESIMP loss to this surface?'
     &             ,' ',0,'F',100,'F',100,'BASESIMP heat loss percent',
     &              IER,6)
                  IE2(IC)=Ifrac
                  if(SO(1:8).ne.'BASESIMP')then
                    TOTHER(IE1(JS))='BASESIMP'
                    UPDGEO=.TRUE.
                  endif
C BASESIMP end.
                elseif(IART.eq.9)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=7
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:7).ne.'IDENT_CEN')then
                    TOTHER(JS)='IDENT_CEN'
                    UPDGEO=.TRUE.
                  endif

C CEN 13791 partition end.
                elseif(IART.eq.10)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=0
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:7).ne.'UNKNOWN')then
                    TOTHER(JS)='UNKNOWN'
                    UPDGEO=.TRUE.
                  endif
                endif
              endif
            endif
          elseif(ipmatch.eq.1)then

C Only one possible match, inform user and then proceed.
            II=ipmz(1)
            JJ=ipms(1)
            MODBND=.TRUE.
            if(MMOD.eq.8)then
              CALL INLNST(1)
              LINSTY(IC)=2
            endif
            nzg=2
            nznog(1)=IZ
            nznog(2)=II
            CALL SURADJ(II,JJ,IE,TMP,IZC,ISC,ICO,DESCRC)
            if(MMOD.eq.8)LINSTY(ICO)=2
            izgfoc = IZ
            if(MMOD.eq.8)CALL CADJVIEW(0,IER)
            MODIFY=.TRUE.
            MODBND=.TRUE.
            IC1(IC)=IZ
            IE1(IC)=JS
            ICT(IC)=3
            IC2(IC)=II
            IE2(IC)=JJ
            call usrmsg(' ',bl,'-')
            if(MMOD.eq.8)then
              if(longpause)then
                call pauses(1)
              else
                call pausems(400)
              endif
            endif

C Update surface attributes.
            SN=SNAME(II,JJ)
            ZN=zname(II)
            WRITE(SO,'(A)')TOTHER(JS)
            if(SO(1:lnblnk(SO)).ne.ZN(1:lnblnk(ZN)))then
              TOTHER(JS)=ZN
              UPDGEO=.TRUE.
            endif
            WRITE(outs,'(a,a,a,a)')' accepting match to surface ',
     &          SN(1:lnblnk(SN)),' in ',ZN(1:lnblnk(ZN))
            call edisp(iuout,outs)
          elseif(ipmatch.gt.1)then

C If there is more than ONE possible matching geometric connection
C then confirm with the user. First check if one of the matches
C is of a reasonable orientation.
            iipmatch=MIN0(ipmatch,3)
            write(outs,'(a,i2,a,a)') 'There are ',iipmatch,
     &        ' possible matches with ',CXSTR(1:lnblnk(CXSTR))
            call edisp(iuout,outs)
            do 841 ipml=1,iipmatch
              IIx=ipmz(ipml)
              JJx=ipms(ipml)
              CALL SURADJ(IIx,JJx,IE,TMP,IZC,ISC,ICO,DESCRC)
              SELV=SSPELV(ic)+SSPELV(ico)
              DAZI=SSPAZI(ic)-SSPAZI(ico)
              DAZI=ABS(DAZI)
              call eclose(DAZI,180.0,3.0,closea)
              call eclose(SELV,0.0,3.0,closee)
              SN=SNAME(IIx,JJx)
              ZN=zname(IIx)
              if(closea.and.closee)then
                WRITE(outs,'(a,a,a,a)')
     &            ' Probable (orientation close) match to surface ',
     &            SN(1:lnblnk(SN)),' in ',ZN(1:lnblnk(ZN))
              else
                WRITE(outs,'(a,a,a,a)')
     &            ' Possible (orientation not close) match to surface ',
     &            SN(1:lnblnk(SN)),' in ',ZN(1:lnblnk(ZN))
              endif
              call edisp(iuout,outs)
 841        continue
 840        continue

C With two surfaces close together it is difficult to see the connection.
C Zoom in on the surface in question - find its bounding box and centre
C the view on it and calculate the angle of view to that box.
            if(MMOD.eq.8)then
              call surbox(iz,js,1.,cx,cy,cz,sxmx,sxmn,symx,symn,szmx,
     &          szmn)
              call saveview
              call ang3vtx(sxmn,symn,sznb,EYEM(1),EYEM(2),EYEM(3),sxmx,
     &          symx,szmx,angn)
              ANG=angn
              HANG=ANG/2.0
            endif
            do 843 ipml=1,iipmatch
              ITBND=0
              VIEWM(1)=cx
              VIEWM(2)=cy
              VIEWM(3)=cz
              XMN=sxmn
              YMN=symn
              ZMN=szmn
              XMX=sxmx
              YMX=symx
              ZMX=szmx
              MODIFY=.TRUE.
              MODLEN=.TRUE.
              if(MMOD.eq.8)CALL INLNST(1)
              if(MMOD.eq.8)LINSTY(IC)=2
              nzg=2
              II=ipmz(ipml)
              JJ=ipms(ipml)
              nznog(1)=IZ
              nznog(2)=II
              if(MMOD.eq.8)LINSTY(ICO)=2
              izgfoc = IZ
              if(MMOD.eq.8)CALL CADJVIEW(0,IER)
              SN=SNAME(II,JJ)
              ZN=zname(II)
              WRITE(outs,'(a,a,a,a)')' Probable match to surface ',
     &            SN(1:lnblnk(SN)),' in ',ZN(1:lnblnk(ZN))
              dok=.true.
              h(1)='If there is more than ONE possible matching '
              h(2)='geometric connection you are asked to confirm'
              h(3)='which one. Exercise care - the interface does'
              h(4)='not tell you how many possible matches it will'
              h(5)='be showing you. '
              CALL ASKOK(outs,' Accept this? (see help)',OK,dok,5)
              IF(OK)then
                IC1(IC)=IZ
                IE1(IC)=JS
                ICT(IC)=3
                IC2(IC)=II
                IE2(IC)=JJ

C Update surface attributes and then restore the view points.
                WRITE(SO,'(A)')TOTHER(JS)
                if(SO(1:lnblnk(SO)).ne.ZN(1:lnblnk(ZN)))then
                  TOTHER(JS)=ZN
                  UPDGEO=.TRUE.
                endif
                if(MMOD.eq.8)call recoview
                ITBND=1
                MODLEN=.TRUE.
                MODIFY=.TRUE.
                MODBND=.TRUE.

C Move to the next surface.
                ICNT=0
                goto 110
              endif
 843        continue

C If reached this point then no surface selected. 
            if(MMOD.eq.8)call recoview
            ITBND=1
            MODBND=.TRUE.
            MODIFY=.TRUE.
            MODLEN=.TRUE.
            h(1)='If you continue, the current connection type '
            h(2)='will be retained.  You can edit this connection '
            h(3)='after all of the surfaces have been checked.'
            CALL EASKABCD(' None of the suggestions taken:',' ',
     &        'loop again','set to UNKNOWN','set to EXTERIOR',
     &        'continue',IRTA,3)
            if(irta.eq.1)then
              goto 840
            elseif(irta.eq.2)then
              IC1(IC)=IZ
              IE1(IC)=JS
              ICT(IC)=0
              IE2(IC)=0
              IC2(IC)=0
              if(SO(1:7).ne.'UNKNOWN')then
                TOTHER(JS)='UNKNOWN'
                UPDGEO=.TRUE.
              endif
            elseif(irta.eq.3)then
              IC1(IC)=IZ
              IE1(IC)=JS
              ICT(IC)=0
              IE2(IC)=0
              IC2(IC)=0
              if(SO(1:7).ne.'EXTERIOR')then
                TOTHER(JS)='EXTERIOR'
                UPDGEO=.TRUE.
              endif
            elseif(irta.eq.4)then
              ICNT=0
              goto 110
            endif

C Reset line style.
            if(MMOD.eq.8)CALL INLNST(1)
          ENDIF
  110   CONTINUE

C Update geometry file if required.
        if(UPDGEO)then
          call edisp(iuout,' Updating surface attributes...')
          CALL EGOMIN(IFIL+2,LGEOM(IZ),IZ,0,0,iuout,IER)
          do 323 IJ=1,NSUR
            SOTHER(IJ)=TOTHER(IJ)
  323     continue
          CALL EMKGEO(IFIL+2,LGEOM(IZ),IZ,iuout,3,IER)
          IF(IER.NE.0)THEN
            CALL USRMSG(bl,' Problem updating geometry file.','W')
          ENDIF
        endif
  100 CONTINUE

C Ask user if updated information should be saved to different file.
 289  h(1)='At this point the zone geometry files will have updated'
      h(2)='surface attributes. And the master connections list is '
      h(3)='still held in memory and needs to be written out to file'
      h(4)='or the model will be inconsistent.'
      h(5)=' '
      h(6)='This dialog allows you to save the master connections list'
      h(7)='to a different file name (in case you do not want to'
      h(8)='overwrite the existing file. '
      write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
      if(LCNN(1:1).eq.' ')LCNN=DCNN
      CALL EASKS(LCNN,' Surface connections file name?',' ',
     &  72,DCNN,'system connx file name',IER,2)
      IF(LCNN.EQ.' ')GOTO 289
      call EMKCFG('-',IER)

      RETURN
      END


C ************* ESACON 
C ESACON import surface connection attributes into the problem topology
C common block C3 while taking into account confirmation preferences.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE ESACON(ITRC,M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP,IER)
#include "building.h"
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      common/ground/grdtmp(12,mgrdp)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      common/scanpause/longpause

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      LOGICAL OK,DOK,MODIFY,MODLEN,MODBND,close,longpause
      logical M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP

      CHARACTER CXSTR*78,outs*124
      CHARACTER OUTSTR*124,SO*12,TMP*54,zname*12,zdesc*64,H*72
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,bl*2
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL

C General image option flags.
      bl='  '
      ITDSP=1
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=0
      ITVNO=1
      ITORG=1
      ITSNR=1
      ITGRD=1
      GRDIS=0.0

C Display connection header.
      TMP=' '
      CALL EDISP(iuout,' ')
      CALL CONXINF(1,0,CXSTR)
      CALL EDISP(iuout,CXSTR)

C Loop through each connection in turn. Read in the geometry
C attributes for each set of connections in turn.
C Map between the connection and surface. Only read in geometry
C and display zone when connection jumps to another zone.
      ICL=0
      DO 123 ICC=1,NCON
        CALL CONXINF(1,ICC,CXSTR)
        CALL EDISP(iuout,CXSTR)
        IF(ICL.NE.IC1(ICC))THEN
          CALL EGOMIN(IFIL+1,LGEOM(IC1(ICC)),IC1(ICC),1,0,ITRU,IER)
          MODIFY=.TRUE.
          MODBND=.TRUE.

C Set all surfaces to standard line width.
          if(MMOD.eq.8)CALL INLNST(1)
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          IF(IER.NE.0)THEN
            RETURN
          ELSE
            ICL=IC1(ICC)
            ISS=0
          ENDIF
        ENDIF
        ISS=ISS+1

C If surface attribute is something other than UNKNOWN then see about 
C upgrading the current connection. Display the connection and the
C surface attribute and ask user if attribute should be used. First
C check for easy matches.
        WRITE(SO,'(A)')SOTHER(ISS)(1:12)
        WRITE(TMP,'(A,I3,A,A15)')'Surface ',ISS,' attribute: ',SO
        WRITE(OUTSTR,'(A,A)')'Topology: ',CXSTR(1:lnblnk(CXSTR))
        IF(SO(1:8).EQ.'EXTERIOR'.AND.ICT(ICC).EQ.0)THEN
          CALL EDISP(iuout,'   topology & attribute EXTERIOR matches.')
          goto  123
        ELSEIF(SO(1:7).EQ.'SIMILAR'.AND.ICT(ICC).EQ.1)THEN
          CALL EDISP(iuout,'   topology & attribute SIMILAR matches.')
          GOTO 123
        ELSEIF(SO(1:8).EQ.'CONSTANT'.AND.ICT(ICC).EQ.2)THEN
          CALL EDISP(iuout,'   topology & attribute CONSTANT matches.')
          GOTO 123
        ELSEIF(SO(1:6).EQ.'GROUND'.AND.ICT(ICC).EQ.4)THEN
          CALL EDISP(iuout,'   topology & attribute GROUND match.')
          GOTO 123
        ELSEIF(SO(1:9).EQ.'ADIABATIC'.AND.ICT(ICC).EQ.5)THEN
          CALL EDISP(iuout,'   topology & attribute ADIABATIC match.')
          GOTO 123
        ELSEIF(SO(1:8).EQ.'BASESIMP'.AND.ICT(ICC).EQ.6)THEN
          CALL EDISP(iuout,'   topology & attribute BASESIMP match.')
          GOTO 123
        ELSEIF(SO(1:9).EQ.'IDENT_CEN'.AND.ICT(ICC).EQ.7)THEN
          CALL EDISP(iuout,'   topology & attribute IDENT_CEN match.')
          GOTO 123
        ELSEIF(SO(1:7).EQ.'UNKNOWN'.OR.SO(1:2).EQ.'  ')THEN
          CALL EDISP(iuout,' surface attribute was UNKNOWN: skipping.')
          GOTO 123
        ELSEIF(ICT(ICC).EQ.3.AND.(SO.EQ.zname(IC2(ICC))))then
          CALL EDISP(iuout,'   connected zone names match.')
          GOTO 123
        ELSEIF(SO(1:7).EQ.'SIMILAR')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          if(.NOT.M1SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << SIMILAR.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=1
          IC2(ICC)=0
          IE2(ICC)=0
          GOTO 123
        ELSEIF(SO(1:8).EQ.'CONSTANT')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          if(.NOT.M2SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << CONSTANT.')
          ICT(ICC)=2
          H(1)='A type 2 connection requires that a constant '
          H(2)='temperature and radiation (Watts) be entered.'
          VAL=IC2(ICC)
          CALL EASKR(VAL,' ',' Constant temperature ? ',
     &        -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
          VAL2=IE2(ICC)
          CALL EASKR(VAL2,' ',' Constant rad (W/m^2) ? ',
     &        -999.,'F',999.,'F',0.,'adjacent rad',IER,2)
          IC2(ICC)=INT(VAL)
          IE2(ICC)=INT(VAL2)
          GOTO 123
        ELSEIF(SO(1:6).EQ.'GROUND')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          if(.NOT.M4SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << GROUND.')
          ICT(ICC)=4
          CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
          CALL EDISP(iuout,'id  Jan, Feb, Mar, Apr, May, Jun, Jul...')
          do 21 igrdp=1,mgrdp
            CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
            if(.NOT.close)then
              WRITE(OUTS,'(I2,12F5.1)')igrdp,(GRDTMP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
            endif
21        continue

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
          if(NGRDP.gt.0)then
            CALL EDISP(iuout,' ')
            do 22 igrdp=1,NGRDP    
              CALL EDISP(iuout,'User defined ground profile Jan-Dec:')
              WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
 22         continue
          endif
          H(1)='A ground connection allows either:'
          H(2)='> 0 - a connection to one of several standard profiles'
          H(3)='  in which case the profile number is required.'
          H(4)='= 0 - a connection to a user defined profile:'
          H(5)='    a 0 followed by a profile number is required.'
          H(6)='-3  a connection to a 3D ground model.'
          IVAL=1
          CALL EASKI(IVAL,' Monthly profile (0 = user defined) ? ',
     &      ' ',-3,'F',12,'F',1,'ground profile',IER,5)
          IC2(ICC)=IVAL
          IF(IC2(ICC).GT.0)THEN
            IE2(ICC)=0
          ELSEIF(IC2(ICC).eq.0)THEN
            H(1)='One of the user defined profiles numbers is'
            H(2)='required at this point.'
            IVAL=IE2(ICC)
            CALL EASKI(IVAL,' User defined profile: ',
     &        ' ',0,'F',9,'F',1,'user defined profile',IER,2)
            IE2(ICC)=IVAL
          ELSEIF(IC2(ICC).eq.-3)THEN
            IE2(ICC)=0
          ENDIF
          GOTO 123
        ELSEIF(SO(1:9).EQ.'ADIABATIC')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          if(.NOT.M5SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << ADIABATIC.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=5
          IC2(ICC)=0
          IE2(ICC)=0
          GOTO 123
        ELSEIF(SO(1:8).EQ.'BASESIMP')THEN

C BASESIMP start.
C Get BASESIMP configuration via call to bsimtype. Save state of current
C menu and recover after the call.
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          if(.NOT.M6SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << BASESIMP.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=6

          H(1)='Specify the BASESIMP configuration index.'
          H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the '
          H(3)='correspondence between iconfig and standard '
          H(4)='BASESIMPdescriptors (e.g. BCIN_1).'
C          CALL EPMENSV
          IBS=IC2(ICC)
          call bsimtype(ibs)
C           CALL EPMENRC
          IC2(ICC)=IBS

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration.
          Ifrac=IE2(ICC)
          H(1)='BASESIMP is a `whole-foundation model`. Therefore, when'
          H(2)='a foundation is represented by numerous ESP-r surfaces'
          H(3)='(e.g. four walls + floor to represent a basement), you'
          H(4)='must specify the percentage of the whole-foundation'
          H(5)='heat loss to attribute to each surface. Normally the'
          H(6)='sum of surfaces representing a founcation is 100%.'
          CALL EASKI(Ifrac,
     &      ' Percent of BASESIMP loss to this surface?'
     &      ,' ',0,'F',100,'F',100,'BASESIMP heat loss percent',IER,6)
          IE2(ICC)=Ifrac
C BASESIMP end.
          GOTO 123
        ELSEIF(SO(1:9).EQ.'IDENT_CEN')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          if(.NOT.M7SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << IDENT_CEN.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=7
          IC2(ICC)=0
          IE2(ICC)=0
          GOTO 123
        ELSEIF(SO(1:8).EQ.'EXTERIOR')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          call edisp(iuout,OUTSTR)
          call edisp(iuout,TMP)
          dok=.true.
          h(1)='This pause allows you to correct errors. '
          CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
          IF(.NOT.OK)GOTO 123
          CALL EDISP(iuout,'   topology << EXTERIOR.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=0
          IC2(ICC)=0
          IE2(ICC)=0
        ELSE

C Not an easy match or mandated match so tell user.
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(ITRC,IER)
          WRITE(OUTSTR,'(A,A)')'Topology: ',CXSTR(1:lnblnk(CXSTR))
          call edisp(iuout,' ')
          call edisp(iuout,OUTSTR)
          call edisp(iuout,TMP)
          CALL usrmsg(bl,'Attribute cannot be imported.','-')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          GOTO 123
        ENDIF
  123 CONTINUE

C Ask user if updated info should be saved to file.
      dok=.true.
      h(1)='Assuming the last task was successful you need to '
      h(2)='save the topology to file to preserve any changes '
      h(3)='made to the model.  '
      CALL ASKOK(' ','Save updated topology?',OK,dok,3)
      if(OK)then
        call EMKCFG('-',IER)
      endif

      RETURN
      END

C ************ SFCRNR
C SFCRNR finds the total number of corners in a surface and
C stores their coordinates. A corner exists if two joined
C edges have different angles 'ALPHA' and 'THETA' where:
C         ALPHA is measured from the X axis on the XY plane.
C         THETA is measured from the XY plane.
      SUBROUTINE SFCRNR(DACC,IZN, JSURF, NAPICS, COORD)
#include "building.h"

      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)
      COMMON/C24/IZSTOCN(MCOM,MS)
      DIMENSION COORD(MV,3)

C ANACC is the tolerance in degrees before a corner is assumed.
      icc=IZSTOCN(IZN,JSURF)
      ANACC = DACC

C NAPICS counts number of corners located for surface.
      NAPICS=0

C NSV holds number of vertices for this surface.
      NSV=NZNVER(icc)

C Loop over each vertex to locate an apex.
      DO 10 I=1, NSV
         IND=I
         IVERT1= NZJVN(icc,IND)
         X1=VCOORD(IZN,IVERT1,1)
         Y1=VCOORD(IZN,IVERT1,2)
         Z1=VCOORD(IZN,IVERT1,3)

         IND=IND+1
         IF (IND.GT.NSV) IND=1
         IVERT2= NZJVN(icc,IND)
         X2=VCOORD(IZN,IVERT2,1)
         Y2=VCOORD(IZN,IVERT2,2)
         Z2=VCOORD(IZN,IVERT2,3)

         IND=IND+1
         IF (IND.GT.NSV) IND=1
         IVERT3= NZJVN(icc,IND)
         X3=VCOORD(IZN,IVERT3,1)
         Y3=VCOORD(IZN,IVERT3,2)
         Z3=VCOORD(IZN,IVERT3,3)

C Find angles of edge 1.
         CALL LINDIR(X1,Y1,Z1,X2,Y2,Z2,ALPHA1,THETA1)

C Find angles of edge 2.
         CALL LINDIR(X2,Y2,Z2,X3,Y3,Z3,ALPHA2,THETA2)

         ALDIFF=ABS(ALPHA2-ALPHA1)
         THDIFF=ABS(THETA2-THETA1)

C If angles difference greater than ANACC then apex located.
         IF (ALDIFF.GT.ANACC.OR.THDIFF.GT.ANACC) THEN
            NAPICS=NAPICS+1
            COORD(NAPICS,1)=X2
            COORD(NAPICS,2)=Y2
            COORD(NAPICS,3)=Z2
         ENDIF
   10 CONTINUE

      RETURN
      END


C ************ LINDIR
C This subroutine finds the angles associated with a line in
C space. The angles returned are the alpha and theta angles in degrees.
C Alpha lies in the XY plane and theta measured from the XY plane.
      subroutine lindir(X1,Y1,Z1,X2,Y2,Z2,ALPHA,THETA)

      logical close
      PI = 4.0 * ATAN(1.0)
      DIST=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
      THETA=ASIN((Z2-Z1)/DIST)
      call eclose(X1,X2,0.001,close)
      IF (close) THEN
         ALPHA=PI/2.
      ELSE
         ALPHA=ATAN((Y2-Y1)/(X2-X1))
      ENDIF
      THETA = THETA*180./PI
      ALPHA = ALPHA*180./PI

      RETURN
      END

C Find bounding box and center related to a surface. Dx is cushion
C to place around the box (m).
      subroutine surbox(iz,is,dx,cx,cy,cz,sxmx,sxmn,symx,symn,szmx,szmn)
#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      SXMX=-1.E+7
      SYMX=-1.E+7
      SXMN=1.E+7
      SYMN=1.E+7
      SZMX=-1.E+7
      SZMN=1.E+7
      N = NVER(IS)
      DO 150 J = 1,N
        SXMN=AMIN1(SXMN,X(JVN(IS,J)))
        SYMN=AMIN1(SYMN,Y(JVN(IS,J)))
        SZMN=AMIN1(SZMN,Z(JVN(IS,J)))
        SXMX=AMAX1(SXMX,X(JVN(IS,J)))
        SYMX=AMAX1(SYMX,Y(JVN(IS,J)))
        SZMX=AMAX1(SZMX,Z(JVN(IS,J)))
  150 CONTINUE
      SXMX=SXMX+dx
      SYMX=SYMX+dx
      SXMN=SXMN-dx
      SYMN=SYMN-dx
      SZMX=SZMX+dx
      SZMN=SZMN-dx
      cx=SXMN+((SXMX-SXMN)/2.0)
      cy=SYMN+((SYMX-SYMN)/2.0)
      cz=SZMN+((SZMX-SZMN)/2.0)
      return
      end

C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end
