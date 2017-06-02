C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-6.

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

C NETWORK is the stand-alone routine for ESP-r network definition.
C The file continains the following routines:
C   NETWORK      - main routine
C  
C   NETWDRW      - drawing routine for the network
C   NETWMIC      - add, delete and copy icons.
C   NETWIDW      - draws an icon on the screen
C   NETWMCO      - add, delete and copy connections
C   NETWIDW      - handles the drawing of icons on the display
C   NETWCDW      - draws a connection on the screen
C   NETGDATR     - handles global attributes of networks
C   selrelexttag - setup values based on external information *.summary
C   listexttag   - list external information in *.summary file
C   catie_int    - decodes min max current & default integer values
C   catie_r      - decodes min max current & default real values

C Main variables used in Network:

C NWKNAM           - network description (72 char)
C NWKTYPSTR        - network type descriptor
C INWKTYP          - network type 1 = 'HVAC', 2 = 'Flow', 3 = 'Electrical'
C                         4 ='Control', 5 = 'Hygroscopic', 6 = 'PrimitivePt'

C NICON            - no. of icons 
 
C ICONCS           - currently selected icon no
C ICONTP(MICN)     - icon type (determines what is drawn in screen)
C XYZICON(MICN,3)  - icon x,y,z-position
C NICONATR(MICN)   - number of attributes associated with this icon
C ATRICN(MICN,MIATRB,3) - icon attributes (array of attribute strings)
C ATRTAG(MICN,MIATRB,1) - icon attribute tags (group identity)
C ATRTAG(MICN,MIATRB,2) - icon attribute tags (data type intg/real/text)
C ATRTAG(MICN,MIATRB,3) - icon attribute tags (`external` or `-`)
C ATRTAG(MICN,MIATRB,4) - icon attribute tags (external tag or `-`)
C ATRTAG(MICN,MIATRB,5) - icon attribute tags (static or user editable)
C ATRMENU(MICN,MIATRB)  - icon attribute menu entry from icon database
C ATRPHRASE(MICN)       - phrase describing the icon
C NIVC(MICN)            - number of icon vertex co-ordinates
C VCICON(MICN,MICNV,3)  - icon vertex co-ordinates (used in drawing icon)
C NIVE(MICN)            - number of icon vertex edges
C IVEICN(MICN,MICNE,5)  - icon vertex edges (used in drawing icon)
C    (n,n,1) is start vertex index, (n,n,2) is end vertex index
C    (n,n,3) is icon line colour (i.e. zone or grey colour index),
C    (n,n,4) colour scale (0=interface, 1=greyscale, 2=zone scale),
C    (n,n,5) line type (1=solid, 2=dotted, 3=dashed, 4=double)
C ISEL(MICN)           - logical variable to determine if an
C                         icon has been selected
C NIVE(MICN) - number of dots in the icon (used for drawing)
C IVDOT(MICN,MICND,4) - icon dot structure where (n,n,1) is vertex index
C    (n,n,2) is colour (i.e. zone or grey colour index),
C    (n,n,3) colour scale (0=interface, 1=greyscale, 2=zone scale),
C    (n,n,4) size (0=small, 1=4x4pixel) 
C NIVA(MICN) - number of arcs/circles in icon
C IVARC(MICN,MICND,7) - icon arc structure where (n,n,1) is centre
C    vertex index, (n,n,2) is index of a vertex on the radius, (n,n,3) is
C    angle (integer degrees anticlockwise from 3o'clock position) to the
C    start of arc, (n,n,4) is angle (integer degrees anticlockwise from 
C    3o'clock position) to the end of the arc, 
C    (n,n,5) is colour (i.e. zone or grey colour index),
C    (n,n,6) colour scale (0=interface, 1=greyscale, 2=zone scale),
C    (n,n,7) fill index (0=no fill, 1=black fill).
C NIVL(MICN) - number of internal labels in the icon.
C NWICNLBL(MICN,MICND) - internal label text (4 character)
C IVLBL(MICN,MICND,3)  - icon internal label structure where (n,n,1) is vertex index
C    (n,n,2) is colour (i.e. zone or grey colour index),
C    (n,n,3) colour scale (0=interface, 1=greyscale, 2=zone scale),
C NWICNM(MICN)         - the icon name (from user),
C NWICNHIS(MICN) - icon origin tags (domain, category and name from icon db)
C CONCP(MICN,MCNP,2)    - x & y of attachment point connection <-> icon
C ICNCT(MICN,MCNP)      - attachment point type (to check against ICNNT(MNCNN))
C See gnetwk.h for definitions of common block NWKCON
C ICNNCS           - currently selected connection
C NCONP(MICN)      - number of attachment points for each icon

C The following relate to connection attributes of each domain (some
C have no attributes)
C   dtagatr(MNWKTYP,MIATRB,5): array of tags (12 char) where the first is the
C     group (thermal/flow/control/output/location/environment/electrical)
C     the 2nd signals data type, the 3rd is `external` or `-`, the 4th
C     is an external key word or `-`, and the 5th is key word 'static' or 'user'.
C     Note `external` signals that an external domain summary file should 
C     be consulted for information associated with the external key word.

C CSEL(MCNN)          - logical variable to determine if a 
C                       connection has been selected

C GRMAX(3)            - grid maximum dimensions (m) on x,y,z dimensions
C GRSPC(3)            - grid spacing in x,y,z dimensions
C GRLYRH(MLYRS)       - grid layer height (redundant!)

C SCALF               - current scaling factor (zoom)

C IVIEW               - view setting  1 - xy 2 - xz 3 - yz
C VIEWCEN(3)          - current view view centre (x,y,z)
C VIEWLIM(6)          - current view limits (x1,x2,y1,y2,z1,z2)
 
C GON                 - grid on/off
C SON                 - snap on/off


C ************************* NETWORK ********************************
C This is the main network management routine.
      PROGRAM NETWORK
      USE START_UP
#include "gnetwk.h"
#include "building.h"
#include "espriou.h"
#include "esprdbfile.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

C Graphics and ESP-r default commons
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
      COMMON/FILEP/IFIL
      COMMON/MOD/MODEL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/FUNIT/IUCFG,IUPDB

C To signal to MFWRIT that globals are available.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

C Simulation preferences as per simcon.F in esrubps.
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

C Path to problem and command line file (if any).
      COMMON/RPATH/PATH

C Legacy flow file commons.
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72

C If working with a flow network, keep track of how many icons are
C nodes and components.
      integer NBFNODES ! number of flow nodes
      integer NBFCOMP  ! number of flow components
      COMMON/NWKGFLOW/NBFNODES,NBFCOMP

C Icon commons are in gnetwk.h
C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

C Common for component/connection toggle
      COMMON/NWKGTOG/ITOG

C Grid commons
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12
      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf
      COMMON/ICONDBNAM/ICONDBFL
      character ICONDBFL*72
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL
      character DFCFD*72,DECMPDBFL*72,DICONDBFL*72

C Icon rotation angle and grid common blocks.
      COMMON/IROT/ROTA
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)

C Summary of external data.
      common/exsum/isexavail,iuex

C Signal that there is a new connection which might have attributes
C to edit.
      common/newconn/ihavenewcnn
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12

C Variables passed back as parameters in call to scanicondb.
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)

      character iconphrase*40,category*12
      character tagatr*12,menuatr*32,atrib*12,ICDNAM*12
      dimension tagatr(MIATRB,5),atrib(MIATRB,3),menuatr(MIATRB)

C WORDSS array of string tokens from each NWICHNIS.
      CHARACTER*32 WORDSS(12)

      CHARACTER*72 LTMP,DFILE
      CHARACTER exttag*12
      CHARACTER*34 ITEM(27)
      CHARACTER INF*144,PATH*72
      CHARACTER FS*1,STR*72,outs*124,outstr*124
      LOGICAL UNIXOK,OK,DOK,CLOSEM,CLOSEX,isexavail,XST
      dimension IVAL(MNWKTYP)
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      character ddgtagatr*12,ddgmenuatr*32,ddgatrib*12

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer NITEMS,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

C Initialize global common data. IUEX is file unit for the
C model external data summary.
      CALL EZERO
      call curmodule('net ')
      cAppName = 'net'
      helpinapp='net'  ! set once for the application
      helpinsub='NETWORK'  ! set for MAIN
      IUOUT=6
      IUIN=5
      IFIL=10
      IUEX=42
      IFS=1
      ITFS=1
      IMFS=1
      ITOG=0; NBFNODES=0; NBFCOMP=0
      NSIGFIG=3

C Scan the command line and copy the name of the argument into the network 
C filename string. 
C << this only takes first 72 char of a potentially longer inf >>
      call parpsf(MODL,iappw,iappx,iappy,inf)
      IF(LNBLNK(INF).GT.1)THEN
        WRITE(LAPROB,'(A72)')INF(1:72)
      ELSE
        LAPROB='UNKNOWN'
      ENDIF

C Set folder separator (fs) to \ or / as required.
      CALL ISUNIX(UNIXOK)
      IF(UNIXOK)THEN
        FS = CHAR(47)
      ELSE
        FS = CHAR(92)
      ENDIF
      WRITE(PATH,'(A1,A1)')'.',FS

C Initial use of default icons file. 
      ICONDBFL=DICONDBFL

C Set unit numbers for configuration file and plant data
C base files respectively.
      IUCFG = 7
      IUPDB = 8
      ICONFIL=IFIL+1

C Determine terminal type and set write unit to stderr for rule scripts.
      MMOD=MODL
      MODEL=MODL
      IF(IAPPW.EQ.0.AND.IAPPX.EQ.0.AND.IAPPY.EQ.0)THEN
        iappw=670
        iappx=30
        iappy=15
      ELSE
        if(iappx.le.0)iappx=30
        if(iappy.le.0)iappy=15
        if(iappw.le.200)then
          iappwi=int(670*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      ENDIF

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

C Set initial menu width and text feedback number of lines.
      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(37*iappw*0.01),18)
        LIMTTY= MAX0(int(5*iappw*0.01),5)
        LIMIT = MAX0(int(5*iappw*0.01),5)
      else
        menuchw = 37
        LIMTTY=5
        LIMIT =5
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IFS is for buttons & graphs, ITFS dialog & 
C text feedback and IMFS for menus). If GTK set IFS = 4 to get small
C courier font.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        IMFS=0
        if(iglib.eq.1)then
          IFS=1
        elseif(iglib.eq.2)then
          IFS=4
        endif
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'ESP-r Network Tool ',
     &  cVnum(1:lnblnk(cVnum))

      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle)

C Set additional output units to stdout. Then redirect warning
C messages to stderr in case of rule script program control.
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C Setup and pass in parameters to win3d.
        iiw1=8; iiw2=10; iiw3=2; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        if(iglib.eq.1)then
          call opencpw
          call opensetup
        endif
        call setzscale()
        call setgscale()
        mdispl=0; nifgrey=0; ncset=0; ngset=0; nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()

C Activate the network graphics flag (activates special drawing routines 
C in the c-interface code.
        INGOF=1
        CALL NWKGFLG(INGOF)
        ISE=1; ICO=0; IDA=0
        CALL NWKSMOD(ISE,ICO,IDA)
      ENDIF

      CALL ESPrVersion("summary",cAppName,IUOUT)

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('enet',1)

C Initialise the network on first entry and clear screen and draw grid.
      CALL NWKINIT
      if(iglib.eq.1.or.iglib.eq.2)then
        CALL NETWDRW
      endif

C Warnings to user if in text mode or using GTK library.
      if(iglib.eq.2)then
        call usrmsg(
     &    'This application is using a graphic library which does',
     &    'not support mouse tracking - selection lists used.','W')
      elseif(iglib.eq.3)then
        call usrmsg('This application is intended for use in graphic',
     &    'mode. Be careful in text mode.','W')
      endif
      call edisp(iuout,' ')
      call edisp(iuout,'HINT: Checkout the ? Help option for notes')
      call edisp(iuout,'on using mouse clicks to manipulate icons.')

C If there is a command line file read it in via NETREAD in silent mode.
C NETREAD figures out NBFNODES and NBFCOMP.
      IF(LAPROB(1:3).NE.'UNK'.AND.LAPROB(1:3).NE.'   ')THEN
        IER=0
        INUNIT=IFIL+1
        CALL NETREAD(INUNIT,'S',IER)
        if(IER.NE.0)then
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,'NWKLOAD: ERROR trying to read network!')
        endif

C If a flow network also build standard flow common blocks. If there
C are no global entries there should be at least one for wind pressure
C reductions - so set a standard entry. Include four additional attributes
C for the flow solution.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          if(idgatrdom.eq.0)then
            idgatrdom=5
            ddgtagatr(1,1)='*flow'; ddgtagatr(1,2)='real'
            ddgtagatr(1,3)='-'; ddgtagatr(1,4)='-'
            ddgtagatr(1,5)='user'
            ddgatrib(1,1)='1.0'; ddgatrib(1,2)='0.0'
            ddgatrib(1,3)='10.0'
            ddgmenuatr(1)='wind pressure reduction factor'
            ddgtagatr(2,1)='*flow'; ddgtagatr(2,2)='intg'
            ddgtagatr(2,3)='-'; ddgtagatr(2,4)='-'
            ddgtagatr(2,5)='user'
            ddgatrib(2,1)='1'; ddgatrib(2,2)='1'; ddgatrib(2,3)='2'
            ddgmenuatr(2)='stack pressure 1=sending 2=avg'
            ddgtagatr(3,1)='*flow'; ddgtagatr(3,2)='intg'
            ddgtagatr(3,3)='-'; ddgtagatr(3,4)='-'
            ddgtagatr(3,5)='user'
            ddgatrib(3,1)='2'; ddgatrib(3,2)='1'; ddgatrib(3,3)='2'
            ddgmenuatr(3)='solution 1=Gaussian 2=LU decomp'
            ddgtagatr(4,1)='*flow'; ddgtagatr(4,2)='intg'
            ddgtagatr(4,3)='-'; ddgtagatr(4,4)='-'
            ddgtagatr(4,5)='user'
            ddgatrib(4,1)='-1'; ddgatrib(4,2)='-1'; ddgatrib(4,3)='3'
            ddgmenuatr(4)='trace -1=off 1=itr 2=wor 3=all'
            ddgtagatr(5,1)='*flow'; ddgtagatr(5,2)='intg'
            ddgtagatr(5,3)='-'; ddgtagatr(5,4)='-'
            ddgtagatr(5,5)='user'
            ddgatrib(5,1)='1'; ddgatrib(5,2)='0'; ddgatrib(5,3)='1'
            ddgmenuatr(5)='timing 0=once/hr 1=per zone ts'

C Set standard values for simulation preferences to be written to the text-based 
C flow network file.
            IPSMOD=1  ! stack using sending
            MFBSNC=1  ! timing per zone ts
            MSLVTP=2  ! LU decompsition
            MAXITF=100 ! iteration limit as per bps
            FERREL=0.01 ! relative error as per bps
            FERMFL=0.0005 ! abs error as per bps
            PMAX=50.0     ! max pressure correction
            STEFFR= -0.5  ! ratio of successive pressure corrections
            MFTRAC= -1    ! no trace
            ITER=0
            IOK=0
            haveglobal=.TRUE. ! for use in MFWRIT
          endif
          CALL NETTOFLW(ier)
          call mflist(iuout)
        endif

C Also check for summary file. This is done each time a new graphic
C network file is read in. If the file exists and starts with *Synopsis
C do not 
        isexavail=.false.
        DFILE='network_flow.summary'
        lnf=lnblnk(LAPROB)
        write(LTMP,'(2a)') LAPROB(1:lnf-3),'summary'
        helptopic='net_summary_file'
        call gethelptext(helpinsub,helptopic,nbhelp)

C If the file exists read it, otherwise check with the user.
        call FINDFIL(LTMP,XST)
        if(.NOT.XST)then
          CALL EASKS(LTMP,'Model summary file? ',' ',72,
     &      DFILE,'summary file open',IER,nbhelp)
          CALL EFOPSEQ(iuex,ltmp,1,IER)
          if(ier.ne.0)then
            isexavail=.false.
          else 
            CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
            if(IER.NE.0)isexavail=.false.
            if(OUTSTR(1:9).ne.'*Synopsis')then
              call usrmsg('Attempted to scan non-synopsis file named',
     &          ltmp,'W')
              isexavail=.false.
            else

C Remember the current icon file in case of errors.
              write(currentfile,'(a)') LTMP(1:lnblnk(LTMP))

C Scan the model summary file and report on zone names, volumes and height.
C << probably only if network type is flow. >>
              isexavail=.true.
              exttag='*Zones'
              call listexttag(exttag,ier)
            endif
          endif
        else
          CALL EFOPSEQ(iuex,ltmp,1,IER)
          if(ier.ne.0)then
            isexavail=.false.
          else 
            CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
            if(IER.NE.0)isexavail=.false.
            if(OUTSTR(1:9).ne.'*Synopsis')then
              call usrmsg('Attempted to scan non-synopsis file named',
     &          ltmp,'W')
              isexavail=.false.
            else

C Remember the current icon file in case of errors.
              write(currentfile,'(a)') LTMP(1:lnblnk(LTMP))

C Scan the model summary file and report on zone names, volumes and height.
              isexavail=.true.
              exttag='*Zones'
              call listexttag(exttag,ier)
            endif
          endif
        endif

C The summary file might specify an icon database. Scan it.
        CALL ERPFREE(ICONFIL,ISTAT)
C        call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
C     &    catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
C     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
        call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)

      ELSE

C If no network file specified scan the default icon database.
        CALL ERPFREE(ICONFIL,ISTAT)
C        call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
C     &    catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
C     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
        call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)

      ENDIF                

C Loop through each of the icons and instanciate the NWICNME from
C the information in the icons database.
      IF(NNICN.GT.0)THEN
        DO 30 ICN=1,NNICN   

C Decode NWICNHIS() so that getanicon can be called to populate NWICNME.
          call getutokens(NWICNHIS(icn),':',IW,WORDSS)
          write(category,'(a)') WORDSS(2)(1:lnblnk(WORDSS(2)))
          write(ICDNAM,'(a)') WORDSS(3)(1:lnblnk(WORDSS(3)))
          call geticonindex(inwktyp,category,ICDNAM,idbcat,
     &     idbitem,IER)
 
C Get details of icon and then get its attributes (if any).
          if(idbitem.gt.0)then
            call geticonatr(inwktyp,category,ICDNAM,natrib,
     &        tagatr,atrib,menuatr,iconphrase,IER)
            write(NWICNME(ICN),'(a)') 
     &        iconphrase(1:lnblnk(iconphrase))
C Debug       write(6,*) icn,ICDNAM,' ',iconphrase
          endif
 30     CONTINUE
      ENDIF
 
C (Re)draw the network prior to building menu strings.
      if(iglib.eq.1.or.iglib.eq.2)then
        CALL NETWDRW
      endif

  5   INO=-1
      WRITE(ITEM(1),'(2A)')'a network: ',LAPROB(1:23)
      WRITE(ITEM(2),'(A)') 'b description:'
      IF(LNBLNK(NWKDSC).LE.39)THEN
        WRITE(ITEM(3),'(2X,A)')NWKDSC(1:32)
      ELSE
        WRITE(ITEM(3),'(2X,A,A)')NWKDSC(1:28),' ...'
      ENDIF
      WRITE(ITEM(4),'(A,A)')'c network type: ',NWKTYPSTR(INWKTYP)
      IF(LNBLNK(ICONDBFL).GT.21)THEN
        WRITE(ITEM(5),'(3A)')'d icons: ',ICONDBFL(1:21),
     &' ...'
      ELSE
        WRITE(ITEM(5),'(2A)')'d icons: ',ICONDBFL(1:25)
      ENDIF
      ITEM(6) =' ________________________________'
      if(idgatrdom.gt.0)then
        WRITE(ITEM(7),'(A,i2,A)')'e global attributes (',idgatrdom,')'
      else
        WRITE(ITEM(7),'(A)')     'e global attributes (None)'
      endif
      if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then

C A flow domain, toggle between nodes, components and connections.
        if(ITOG.EQ.0)then
          WRITE(ITEM(8),'(A)')'e toggle >> nodes  '
          WRITE(ITEM(9),'(A,A,I3,A)')
     &      '  No. of nodes ...     ','(',NBFNODES,')'
          WRITE(ITEM(10),'(A)')   'f  add node            '
          WRITE(ITEM(11),'(A)')   'g  delete node         '  
          WRITE(ITEM(12),'(A)')   'h  copy node           ' 
          WRITE(ITEM(13),'(A)')   'i  edit node data      '    
          WRITE(ITEM(14),'(A)')   'j  list node data      '    
        elseif(ITOG.EQ.1)then
          WRITE(ITEM(8),'(A)')'e toggle >> components  '
          WRITE(ITEM(9),'(A,A,I3,A)')
     &      '  No. of components ...     ','(',NBFCOMP,')'
          WRITE(ITEM(10),'(A)')   'f  add component       '
          WRITE(ITEM(11),'(A)')   'g  delete component    '  
          WRITE(ITEM(12),'(A)')   'h  copy component      ' 
          WRITE(ITEM(13),'(A)')   'i  edit component data '    
          WRITE(ITEM(14),'(A)')   'j  list component data '    
        else

C << but in flow terms two graphic connections equals one flow connection >>
          WRITE(ITEM(8),'(A)')'e toggle >> connections '
          WRITE(ITEM(9),'(A,A,I3,A)')
     &      '  No. of connections ..     ','(',NICNN,')'
          WRITE(ITEM(10),'(A)')   'f  add connection      '
          WRITE(ITEM(11),'(A)')   'g  delete connection   '  
          WRITE(ITEM(12),'(A)')   'h  copy <N/A>          ' 
          WRITE(ITEM(13),'(A)')   'i  edit connection data'   
          WRITE(ITEM(14),'(A)')   'j  list connection data'   
        endif
      else

C A domain other than flow. Toggle between components and connections.
        IF(ITOG.EQ.0)THEN
          WRITE(ITEM(8),'(A)')'e toggle >> components  '
          WRITE(ITEM(9),'(A,A,I3,A)')
     &      '  No. of components ...     ','(',NNICN,')'
          WRITE(ITEM(10),'(A)')   'f  add component       '
          WRITE(ITEM(11),'(A)')   'g  delete component    '  
          WRITE(ITEM(12),'(A)')   'h  copy component      ' 
          WRITE(ITEM(13),'(A)')   'i  edit component data '    
          WRITE(ITEM(14),'(A)')   'j  list component data '    
        ELSE
          WRITE(ITEM(8),'(A)')'e toggle >> connections '
          WRITE(ITEM(9),'(A,A,I3,A)')
     &      '  No. of connections ..     ','(',NICNN,')'
          WRITE(ITEM(10),'(A)')   'f  add connection      '
          WRITE(ITEM(11),'(A)')   'g  delete connection   '  
          WRITE(ITEM(12),'(A)')   'h  copy <N/A>          ' 
          WRITE(ITEM(13),'(A)')   'i  edit connection data'   
          WRITE(ITEM(14),'(A)')   'j  list connection data'   
        ENDIF
      endif   
         
      ITEM(15)='  ___drawing/grid controls_______'
      IF(IVIEW.EQ.1)THEN
        WRITE(ITEM(16),'(A)') 'j  current view >> X-Y '
      ELSEIF(IVIEW.EQ.2)THEN
        WRITE(ITEM(16),'(A)') 'j  current view >> X-Z '
      ELSE
        WRITE(ITEM(16),'(A)') 'j  current view >> Y-Z '     
      ENDIF
      CLOSEM=.FALSE.
      CLOSEX=.FALSE.
      CALL ECLOSE(GRSPC(1),0.125,0.001,CLOSEM)
      CALL ECLOSE(GRSPC(1),GRMAX(1),0.001,CLOSEX)  

      IF(CLOSEM)THEN
        WRITE(ITEM(17),'(A)')'k  grid spacing XY:(0.125 Minimum)'
      ELSEIF(CLOSEX)THEN
        WRITE(ITEM(17),'(A,F5.2,A)')
     &'k  grid spacing XY: (',GRMAX(1),'Maximum)'
      ELSE
        WRITE(ITEM(17),'(A,F5.2)')'k  grid spacing XY: ',GRSPC(1)
      ENDIF
      WRITE(ITEM(18),'(A,F5.2)')  'l  grid spacing Z: ',GRSPC(3)
      IF(SCALF.GT.VIEWMZ)THEN
        WRITE(ITEM(19),'(A,i4,A)')'m  zoom: ',int(VIEWMZ),' % (Maximum)'
      ELSEIF(SCALF.LT.VIEWMN)THEN
        WRITE(ITEM(19),'(A,i3,A)')'m  zoom: ',int(VIEWMN),' % (Minimum)'
      ELSE
        WRITE(ITEM(19),'(A,F6.2,A)')'m  zoom: ',SCALF*100.,' %'
      ENDIF
      WRITE(ITEM(20),'(A,F6.2)') 'n  icon rotaton angle: ',ROTA
      ITEM(21)='o  refresh screen                 '
      ITEM(22)=' ________________________________'
      ITEM(23)='s save                           '
      ITEM(24)='! save as ...                    '
      ITEM(25)='> export as ...                  '
      ITEM(26)='? help                           '
      ITEM(27)='- exit                           '
      NITEMS=27
      if(iglib.eq.1.or.iglib.eq.2)then
        CALL NETWDRW
      endif

C Generate overall help text blocks.
      helptopic='net_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Reset the have a new connection toggle if that was the previous interaction.
      if(ihavenewcnn.ne.0) ihavenewcnn=0

      CALL EMENU('Graphical network definition',ITEM,NITEMS,INO)

      if(INO.EQ.NITEMS)then

C Switch network graphics off and quit.
        INGOF=0
        CALL NWKGFLG(INGOF)
        if(isexavail)then
          CALL ERPFREE(iuex,ISTAT)
        endif
        CALL EPAGEND
        STOP

      elseif(INO.EQ.NITEMS-1)then
        helptopic='net_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('network main',nbhelp,'-',0,0,IER)

      ELSEIF(INO.EQ.2)THEN

C Provide a description of the network.
        STR='None'
        CALL EASKS(NWKDSC,
     &    'Please provide a short description of the network',
     &    '  ',72,STR,'netdesc',IER,nbhelp)
      ELSEIF(INO.EQ.4)THEN

C Set the network type
        IPCK=1
        CALL EPICKS(IPCK,IVAL,'Select a network type',' ',12,MNWKTYP,
     &    NWKTYPSTR,'Network select',IER,nbhelp)
        IF(IVAL(1).GT.0)THEN
          INWKTYP=IVAL(1)
        ELSE
          INWKTYP=2
          CALL EDISP(IUOUT,'Setting network type to default: FLOW')
        ENDIF

      ELSEIF(INO.EQ.5)THEN

C Option to read another icon file name. << hardcoded >>
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          DFILE='/usr/esru/esp-r/databases/icons.db1'
        else
          DFILE=DICONDBFL
        endif
        ltmp=ICONDBFL
  2     CALL EASKS(ltmp,'icon database filename ?',
     &    ' ',72,DFILE,'ecomp dbnam',IER,nbhelp)
        if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN') ICONDBFL=ltmp

        ICONFIL=IFIL+1
        CALL EFOPSEQ(ICONFIL,ICONDBFL,1,IER)
        IF(IER.NE.0) THEN
          dok=.false.
          CALL ASKOK('ERROR - could not find the icon database.',
     &      'Try again?',OK,dok,8)
          IF(.NOT.OK) THEN 
            IER=1
            CALL ERPFREE(ICONFIL,ISTAT)
          ELSE
            ICONDBFL='UNKNOWN'
            GOTO 2
          ENDIF
        ELSE

C Scan the header of the nominated icon database.
          CALL ERPFREE(ICONFIL,ISTAT)
C          call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
C     &      catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
C     &      datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
          call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &      datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
        ENDIF

      ELSEIF(INO.EQ.7)THEN

C If there are network global attributes edit them.
        if(idgatrdom.gt.0)then
          call netgdatr(ier)
        endif

      ELSEIF(INO.EQ.8)THEN

C Toggle between components and connections, or if flow nodes, components and
C connections. NWKSMOD is a C function.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          IF(ITOG.LE.1)THEN
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            ITOG=ITOG+1
          ELSE
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            ITOG=0
          ENDIF
        else
          IF(ITOG.EQ.0)THEN
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            ITOG=1
          ELSE
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            ITOG=0
          ENDIF
        endif

      ELSEIF(INO.EQ.10)THEN

C User asked to add an icon. If working with a flow network ITOG zero
C is for nodes, ITOG one is for components, ITOG two is connections.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          IF(ITOG.EQ.0.or.ITOG.eq.1)THEN

C Add a icon. Call to nwksmod and netwmic setup C functions
C which then calls NETWMIC. If mouse click not available
C ask the user X Y Z for icon.
            if(iglib.eq.1)then
              ISE=1; ICO=0; IDA=0
              CALL NWKSMOD(ISE,ICO,IDA)
            else

            endif
            CALL NETWMIC(0,1)
          ELSE

C Add a connection. Call to nwksmod and netwmco setup C functions
C which then calls GCONAD.
            lastnicnn = NICNN
            ISE=0; ICO=1; IDA=0
            ihavenewcnn=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,1)
          ENDIF
        else

C Non-flow domain. Work with components and connections.
          IF(ITOG.EQ.0)THEN

C Add a icon. Call to nwksmod and netwmic setup C functions
C which then calls NETWMIC.
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,1)
          ELSE

C Add a connection. Call to nwksmod and netwmco setup C functions
C which then calls GCONAD.
            lastnicnn = NICNN
            ISE=0; ICO=1; IDA=0
            ihavenewcnn=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,1)  ! 1 is add
          ENDIF
        endif

      ELSEIF(INO.EQ.11)THEN

C Delete a connection or icon
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          IF(ITOG.EQ.0.or.ITOG.eq.1)THEN
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,2)  ! 2 is delete
          ELSE
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,2)
          ENDIF
        else
          IF(ITOG.EQ.0)THEN
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,2)
          ELSE
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,2)
          ENDIF
        endif

      ELSEIF(INO.EQ.12)THEN

C Copy an icon (connection copy N/A).
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          IF(ITOG.EQ.0.or.ITOG.eq.1)THEN
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,3)
          else
            continue
          ENDIF
        else
          IF(ITOG.EQ.0)THEN
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,3)
          else
            continue
          ENDIF
        endif

      ELSEIF(INO.EQ.13)THEN 

C Component or connection editing data here.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          if(ITOG.EQ.0.or.ITOG.EQ.1)then
            ISE=1; ICO=0; IDA=1
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,4)           
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          else
            ISE=1; ICO=0; IDA=1
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,4)
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          endif
        else
          if(ITOG.EQ.0)then
            ISE=1; ICO=0; IDA=1
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,4)           
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          else
            ISE=1; ICO=0; IDA=1
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,4)
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          endif
        endif

      ELSEIF(INO.EQ.14)THEN

C Component or connection listing.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          if(ITOG.EQ.0.or.ITOG.EQ.1)then
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,5)           
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          else

C << this is new needs checking >>
            ISE=1; ICO=0; IDA=1
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,4)
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          endif
        else
          if(ITOG.EQ.0)then
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMIC(0,5)           
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          else

C << this is new needs checking >>
            ISE=1; ICO=0; IDA=1
            CALL NWKSMOD(ISE,ICO,IDA)
            CALL NETWMCO(0,4)
            ISE=1; ICO=0; IDA=0
            CALL NWKSMOD(ISE,ICO,IDA)
          endif
        endif

C If flow, update the flow commons and list them out.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          CALL NETTOFLW(ier)
          call mflist(iuout)
        endif

      ELSEIF(INO.EQ.16)THEN
 
C Drawing controls.
c        IF(IVIEW.LT.3)THEN
c          IVIEW=IVIEW+1
C Currently disable view switching
          IVIEW=1
c        ELSE
c          IVIEW=1
c        ENDIF

      ELSEIF(INO.EQ.17)THEN

C Specify XY grid spacing.
        helptopic='grid_spacing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(OUTS,'(A,F5.1)')'enter a value between 0.125 and ',
     &    GRMAX(1)
        CALL EASKR(GRSPC(1),'XY spacing?',OUTS,0.125,'F',GRMAX(1),'F',
     &    1.0,'gridspace',IER,nbhelp)
        GRSPC(2)=GRSPC(1)

      ELSEIF(INO.EQ.18)THEN

C Specify Z grid spacing.
        helptopic='grid_spacing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(OUTS,'(A,F5.1)')'enter a value between 0.125 and ',
     &    GRMAX(1)
        CALL EASKR(GRSPC(3),'Z spacing?',OUTS,0.125,'F',GRMAX(3),'F',
     &    1.0,'gridspace',IER,nbhelp)

      ELSEIF(INO.EQ.19)THEN

C Menu option for zooming in and out of the graph.
        helptopic='grid_spacing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(OUTS,'(A,F5.1)')
     &    'Enter a value between 0.5 and 8.0 currently ',SCALF
        CALL EASKR(SCALF,'zoom setting?',OUTS,VIEWMN,'F',VIEWMZ,'F',
     &  VIEWMZ,'zoom setting',IER,nbhelp)

      ELSEIF(INO.EQ.20)THEN

C Edit rotation angle. If user clicks middle button on an icon that
C has been highlighted this is the rotation angle to apply.
        helptopic='grid_spacing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(ROTA,
     &    'Icon rotation angle to apply if you middle mouse button',
     &    'click on a highlighted icon. ',0.0,'-',
     &    0.0,'-',0.0,'gridspace',IER,nbhelp)

      ELSEIF(INO.EQ.21)THEN

C Refresh icons and connections.
        if(iglib.eq.1.or.iglib.eq.2)then
          CALL NETWDRW
        endif

      ELSEIF(INO.EQ.23)THEN

C Save current network to (graphics) file.
        IM=1
        CALL NETWRITE(IM)

C Update the flow commons, list and over-write legacy flow file.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          CALL NETTOFLW(ier)
          call mflist(iuout)
   79     IUM=IFIL+6
          lnf=lnblnk(LAPROB)
          write(LTMP,'(2a)') LAPROB(1:lnf-3),'legacy'
          CALL EFOPSEQ(IUM,LTMP,3,IER)
          IF(IER.NE.0)goto 79
          haveglobal=.FALSE. ! do not yet include simulation preference
          CALL MFWRIT(IUM)
          CALL ERPFREE(IUM,ISTAT)
        endif

      ELSEIF(INO.EQ.24)THEN

C Save graphic network to a user specified file.
        IM=2
        CALL NETWRITE(IM)

C Update the flow commons, list them out and write legacy file.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          CALL NETTOFLW(ier)
          call mflist(iuout)
   78     IUM=IFIL+6
          lnf=lnblnk(LAPROB)
          write(LTMP,'(2a)') LAPROB(1:lnf-3),'legacy'
          helptopic='convert_to_text_network'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(LTMP,' Fluid flow network file (legacy)?',
     &      '(see help)',72,' ','mass flow file',IER,nbhelp)
          if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
            continue
          else
            goto 78
          endif
          CALL EFOPSEQ(IUM,LTMP,4,IER)
          IF(IER.NE.0)goto 78

          haveglobal=.TRUE. ! include simulation preference
          CALL MFWRIT(IUM)
          CALL ERPFREE(IUM,ISTAT)
        endif

      ELSEIF(INO.EQ.25)THEN

C Export. << not yet enabled >>.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          call usrmsg('Export to vector file for 3D display not yet',
     &      'enabled. Work in progress...','W')
        endif

      ELSEIF(INO.EQ.1)THEN

C Read in a network, clear exiting network definitions and refreshing
C the screen.  << ?? do we need without fill common blocks ?? >>
        helptopic='convert_to_text_network'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkabc('Options:',' ','standard network file read',
     &    'read & fill flow commons','continue',iwh,nbhelp)
        if(iwh.eq.3)then
          goto 5
        endif
        OK=.FALSE.
        dok=.false.
        CALL ASKOK('Reading in a network will clear the current one',
     &    'are you sure you want to do this?',OK,dok,nbhelp)
        IF(.NOT.OK) GOTO 5
        CALL NWKINIT
        if(iglib.eq.1.or.iglib.eq.2)then
          CALL startbuffer
          CALL NETWDRW
        endif
        LAPROB='UNKNOWN'
        IER=0
        INUNIT=IFIL+1
        if(iwh.eq.1)then
          CALL NETREAD(INUNIT,'N',IER)
        elseif(iwh.eq.2)then
          CALL NETREAD(INUNIT,'N',IER)
          CALL NETTOFLW(ier)
          call mflist(iuout)
        else
          continue
        endif
        if(iwh.eq.1.or.iwh.eq.2)then

C Scan any new .summary file.
          lnf=lnblnk(LAPROB)
          write(LTMP,'(2a)') LAPROB(1:lnf-3),'summary'
          helptopic='net_summary_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(LTMP,' Model summary file? ',' ',72,
     &      DFILE,'summary file open',IER,nbhelp)
          CALL EFOPSEQ(iuex,ltmp,1,IER)
          if(ier.ne.0)then
            isexavail=.false.
          else 
            CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
            if(IER.NE.0)isexavail=.false.
            if(OUTSTR(1:9).ne.'*Synopsis')then
              call usrmsg('Attempted to scan non-synopsis file named',
     &          ltmp,'W')
              isexavail=.false.
            else

C Scan the model summary file and report on zone names, volumes and height.
              isexavail=.true.
              exttag='*Zones'
              call listexttag(exttag,ier)
            endif
          endif
        endif

      ELSE
        GOTO 5   ! fall through
      ENDIF

C Re-draw the screen to impliment any changes before redrawing menu.
      if(iglib.eq.1.or.iglib.eq.2)then
        CALL NETWDRW
      endif

C If there has been a new connection added check with user.

C Debug     write(6,*) 'ihavenewcnn b',ihavenewcnn

      GOTO 5
      END

C ************************* NETWDRW **********************************
C NETWDRW is the main drawing routine for the network  and is called
C when the workspace needs to be resized or redrawn. 
      SUBROUTINE NETWDRW

# include "gnetwk.h"

C Icon commons are gnetwk.h
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKGRDL/GON,SON
      LOGICAL GON,SON
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12
      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      character OUTS1*26
      logical greyok
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iicol
      integer iigl,iigr,iigt,iigb
      integer iix1,iix2,iix3,iix4,iiy1,iiy2,iiy3,iiy4
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb
      integer*8 iix1,iix2,iix3,iix4,iiy1,iiy2,iiy3,iiy4
#endif

C Clear the screen
      CALL startbuffer

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb

C Set the new x y and z view limits. Currently do not scroll
C left further than X = -5m and down to Y = -5m. Start with 
C showing a bit of negative X and Y.
      VIEWLIM(1)=VIEWCEN(1)-((FLOAT(MGRXE)*0.5)/SCALF)
      IF(VIEWLIM(1).LT.-5.0) THEN
        VIEWLIM(1)=-5.0
        VIEWLIM(2)= VIEWLIM(1)+(FLOAT(MGRXE)/SCALF)
      ENDIF
      VIEWLIM(2)=VIEWCEN(1)+((FLOAT(MGRXE)*0.5)/SCALF)
      IF(VIEWLIM(2).GT.GRMAX(1)) THEN
        VIEWLIM(2)=GRMAX(1)
        VIEWLIM(1)=VIEWLIM(2)-MGRXE/SCALF 
      ENDIF
      VIEWLIM(3)=VIEWCEN(2)-((FLOAT(MGRYE)*0.5)/SCALF)
      IF(VIEWLIM(3).LT.-5.0) THEN
        VIEWLIM(3)=-5.0
        VIEWLIM(4)=VIEWLIM(3)+MGRYE/SCALF
      ENDIF
      VIEWLIM(4)=VIEWCEN(2)+((FLOAT(MGRYE)*0.5)/SCALF)
      IF(VIEWLIM(4).GT.GRMAX(2)) THEN
        VIEWLIM(4)=GRMAX(2)
        VIEWLIM(3)=VIEWLIM(4)-MGRYE/SCALF
      ENDIF
      VIEWLIM(5)=VIEWCEN(3)-((FLOAT(MGRZE)*0.5)/SCALF)
      IF(VIEWLIM(5).LT.0.0)THEN
        VIEWLIM(5)=0.0
        VIEWLIM(6)=VIEWLIM(5)+MGRZE/SCALF
      ENDIF
      VIEWLIM(6)=VIEWCEN(3)+((FLOAT(MGRZE)*0.5)/SCALF)
      IF(VIEWLIM(6).GT.GRMAX(3))THEN
        VIEWLIM(6)=GRMAX(3)
        VIEWLIM(5)=VIEWLIM(6)-MGRZE/SCALF
      ENDIF

C Debug. write(6,*) 'viewlim ',VIEWLIM,igw,igwh   

C Draw the grid with the current scaling parameters.
      IF(IVIEW.EQ.1)THEN
        XMIN=VIEWLIM(1); XMAX=VIEWLIM(2)
        YMIN=VIEWLIM(3); YMAX=VIEWLIM(4)
      ELSEIF(IVIEW.EQ.2)THEN
        XMIN=VIEWLIM(1); XMAX=VIEWLIM(2)
        YMIN=VIEWLIM(5); YMAX=VIEWLIM(6)
      ELSE
        XMIN=VIEWLIM(3); XMAX=VIEWLIM(4)
        YMIN=VIEWLIM(5); YMAX=VIEWLIM(6)
      ENDIF

      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,XSC,YSC,SCA,
     &       XADD,YADD)

C Draw the Lower and Left extents of the grid and labels.
      IF(IVIEW.EQ.1.OR.IVIEW.EQ.2)THEN
        call dintervalf(Xmin,Xmax,DY,NDEC,1)
        CALL horaxisdd(Xmin,Xmax,iigl,iigr,iigb,xadd,XSC,1,
     &    DY,NDEC,'X (all grid dimensions in m)')
      ELSE
        call dintervalf(Xmin,Xmax,DY,NDEC,1)
        CALL horaxisdd(Xmin,Xmax,iigl,iigr,iigb,xadd,XSC,1,
     &    DY,NDEC,'Y (all grid dimensions in m)')
      ENDIF

      IF(IVIEW.EQ.1)THEN      
        call dintervalf(Ymin,Ymax,DY,NDEC,1)
        CALL vrtaxisdd(Ymin,Ymax,iigl,iigb,iigt,yadd,YSC,1,
     &    DY,NDEC,0,'Y')
      ELSE
        call dintervalf(Ymin,Ymax,DY,NDEC,1)
        CALL vrtaxisdd(Ymin,Ymax,iigl,iigb,iigt,yadd,YSC,1,
     &    DY,NDEC,0,'Z')
      ENDIF

C Store the scaling parameters used in drawing the axes.
      CALL linescale(iigl,xadd,XSC,iigb,yadd,YSC)

C Draw the upper and right extents.
      call u2pixel(XMAX,YMIN,iix1,iiy1)
      call u2pixel(XMAX,YMAX,iix2,iiy2)
      call eswline(iix1,iiy1,iix2,iiy2)
      call u2pixel(XMIN,YMAX,iix3,iiy3)
      call u2pixel(XMAX,YMAX,iix4,iiy4)
      call eswline(iix3,iiy3,iix4,iiy4)

C Check that the grid is ON GRSPC(*)<>0.0
      IF(GON)THEN

C If libraries and monitor support greyscale then reset forground
C to 50% grey when drawing dots.
        greyok=.false.
        if(nifgrey.gt.4)then
          iicol=nifgrey-3
          call winscl('i',iicol)
          greyok=.true.
        endif

C Draw on the X grid lines.
        IF(IVIEW.EQ.1)THEN
          GSPCW=GRSPC(1)
        ELSEIF(IVIEW.EQ.2)THEN
          GSPCW=GRSPC(1)
        ELSE
          GSPCW=GRSPC(2)
        ENDIF
        GX=(INT(XMIN/GSPCW)*GSPCW)+GSPCW
        iupdown=0; isym=0
        CALL ETPLOT(GX,YMIN,iupdown,isym)    
 10     continue
        iupdown=-203; isym=0
        CALL ETPLOT(GX,YMAX,iupdown,isym)
        GX=GX+GSPCW; iupdown=0; isym=0
        CALL ETPLOT(GX,YMIN,iupdown,isym)
        IF(GX.LT.XMAX) GOTO 10

C Draw on the Y grid lines.
        IF(IVIEW.EQ.1)THEN
          GSPCH=GRSPC(2)
        ELSEIF(IVIEW.EQ.2)THEN
          GSPCH=GRSPC(3)
        ELSE
          GSPCH=GRSPC(3)
        ENDIF
        GY=(INT(YMIN/GSPCH)*GSPCH)+GSPCH
        iupdown=0; isym=0
        CALL ETPLOT(XMIN,GY,iupdown,isym)    
 20     continue
        iupdown=-203; isym=0
        CALL ETPLOT(XMAX,GY,iupdown,isym)
        GY=GY+GSPCH; iupdown=0; isym=0
        CALL ETPLOT(XMIN,GY,iupdown,isym)
        IF(GY.LT.YMAX) GOTO 20

C Return to black lines after drawing grid.
        if(greyok)then
          iicol=0
          call winscl('-',iicol)
        endif
      ENDIF

C Draw on the network description. 
      WRITE(OUTS1,'(A,A)')'Network Type: ',NWKTYPSTR(INWKTYP)(1:12)
      iside=1; isize=1; ifont=1
      call viewtext(outs1,iside,isize,ifont)

C Redraw the icons with the current scaling, limits and layer values
      IF(NNICN.GT.0)THEN
        DO 30 ICN=1,NNICN   
          CALL NETWIDW(ICN)
 30     CONTINUE
      ENDIF

C Redraw the connections with current scaling, limits and layer.
      IF(NICNN.GT.0)THEN
        DO 40 ICON=1,NICNN 
          CALL NETWCDW(ICON)
 40     CONTINUE

      ENDIF

      RETURN
      END

C ********************* NETWMIC *********************************
C NETWMIC manages the network icons, can be called from the C-side
C to handle deletion of icons.

C << ?? add an option to select an icon but do not increment count
C << rather overwrite an existing icons data.
      SUBROUTINE NETWMIC(IMODE,IMOPT)

#include "gnetwk.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

C Icon commons are in gnetwk.h       
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf 
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      logical ISEL,CSEL

C If working with a flow network, keep track of how many icons are
C nodes and components.
      integer NBFNODES ! number of flow nodes
      integer NBFCOMP  ! number of flow components
      COMMON/NWKGFLOW/NBFNODES,NBFCOMP

C Common for component/connection toggle
      COMMON/NWKGTOG/ITOG

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)
      dimension vert(MICNV,2),iedge(MICNE,5),idot(MICND,4),
     &  iarc(MICND,7),ilabel(MICND,4),labeltx(MICND),text(60),
     &  iatt(MCNP,2)

C ICITEM1 is ...
C ICITEM2 is ...
      DIMENSION ICITEM1(MICNCAT+3),ICITEM2(MICN+4)
      
      CHARACTER OUTS*124,name*12
      CHARACTER ICDNAM*12,ICMENU*36
      CHARACTER ICITEM1*44,ICITEM2*44, KEY*1
      character iconphrase*40,category*12
      character labeltx*4,text*72
      character tagatr*12,menuatr*32,atrib*12
      dimension tagatr(MIATRB,5),atrib(MIATRB,3),menuatr(MIATRB)
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      character hold*32

C WORDSS array of string tokens from each NWICHNIS.
      CHARACTER*32 WORDSS(12)
      real cgx,cgy

      LOGICAL CLOSEX,CLOSEY,OK,DOK,MREPT,found
      integer NICITEM,IMOUT,NICITEM2,IMOUT2 ! max items and current menu item
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

#ifdef OSI
      integer iii,iix,iiy  ! for use with trackview
#else
      integer*8 iii,iix,iiy  ! for use with trackview
#endif


C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='NETWMIC'  ! set for subroutine

C Set the component reporting flag.
      MREPT=.FALSE.
   
C Ask if the user wants to add/delete/copy/edit/list an icon.
      IOPT=IMOPT
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.2)then
        continue
      else
        call usrmsg('Current version does not support network views.',
     &    'Sorry about that...','W')
        return
      endif

      IF(IOPT.EQ.1.OR.IOPT.EQ.3)THEN

C Add or copy an icon. Find current mouse position and loop through
C all the icons looking for one within 0.3m and ask for confirmation.
C If no mouse need to provide alternative logic requesting position.
        ITCMP=NNICN+1
        IF(IOPT.EQ.3)THEN
          if(iglib.eq.1)then
            if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
             CALL USRMSG('Click on the component to copy',' ','-')
            elseif(INWKTYP.eq.2)then
             CALL USRMSG('Click on the node/component to copy',' ','-')
            elseif(INWKTYP.eq.4)then
             CALL USRMSG('Click on the control entity to copy',' ','-')
            endif
            CALL TRACKVIEW(iii,iix,iiy)
            CALL PIXEL2U(IIX,IIY,GX,GY)
            CGX=GX; CGY=GY
          else
            hold = ' 1.000  1.000  1.000    '
            CALL EASKS(HOLD,'Position @ component to copy (X Y Z m):',
     &        ' ',36,' 1. 1. 1.','position XYZ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,CGX,-99.,99.,'W','Xm',IER)
            CALL EGETWR(HOLD,K,CGY,-99.,99.,'W','Ym',IER)
            CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Zm',IER)
          endif
          CLOSEX=.FALSE.; CLOSEY=.FALSE.
          DO 15 I=1,NNICN
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,2),0.3,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.3,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,XYZICON(I,2),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.3,CLOSEY)
            ENDIF

            IF(CLOSEX.AND.CLOSEY)THEN
              DELTAX=0.0; DELTAY=0.0; DELTAZ=0.0

C Switch object selected status.
              WRITE(OUTS,'(A,A)')'Copying ',NWICNM(I)
              CALL USRMSG(OUTS,' ','-')

C Place it one the drawing area.                                        
              if(iglib.eq.1)then
                CALL USRMSG('Now position it on the screen',' ','-')
                CALL TRACKVIEW(iii,iix,iiy)
                CALL PIXEL2U(IIX,IIY,GX,GY)
                CGXC=GX
                CGYC=GY
              else
                hold = ' 1.000  1.000  1.000    '
                CALL EASKS(HOLD,'New position @ X Y Z m:',
     &            ' ',36,' 1. 1. 1.','position XYZ',IER,nbhelp)
                K=0
                CALL EGETWR(HOLD,K,CGX,-99.,99.,'W','Xm',IER)
                CALL EGETWR(HOLD,K,CGY,-99.,99.,'W','Ym',IER)
                CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Zm',IER)
              endif
              IF(IVIEW.EQ.1)THEN
                GRMAX1=GRMAX(1); GRMAX2=GRMAX(2)
              ELSEIF(IVIEW.EQ.2)THEN
                GRMAX1=GRMAX(1); GRMAX2=GRMAX(2)
              ELSE
                GRMAX1=GRMAX(2); GRMAX2=GRMAX(3)
              ENDIF
              IF((CGXC.LT.GRMAX1.AND.CGXC.GT.0.0).AND.
     &          (CGYC.LT.GRMAX2.AND.CGYC.GT.0.0))THEN

C Snap the component to the grid if this is active. 
                CALL NETWSNAP(CGXC,CGYC,CGXS,CGYS)
                CGXC=CGXS
                CGYC=CGYS

                IF(IVIEW.EQ.1)THEN
                  XYZICON(ITCMP,1)=CGXC      
                  XYZICON(ITCMP,2)=CGYC  
                  XYZICON(ITCMP,3)=VIEWCEN(3) 
                ELSEIF(IVIEW.EQ.2)THEN 
                  XYZICON(ITCMP,1)=CGXC   
                  XYZICON(ITCMP,2)=VIEWCEN(2) 
                  XYZICON(ITCMP,3)=CGYC  
                ELSE
                  XYZICON(ITCMP,1)=VIEWCEN(1)    
                  XYZICON(ITCMP,2)=CGXC  
                  XYZICON(ITCMP,3)=CGYC
                ENDIF

C Store old icon position
                DELTAX=XYZICON(ITCMP,1)-XYZICON(I,1)
                DELTAY=XYZICON(ITCMP,2)-XYZICON(I,2)
                DELTAZ=XYZICON(ITCMP,3)-XYZICON(I,3)

C Finally translate the component image to the selected centre.
                DO 107 IV=1,NIVC(I)
                   VCICON(ITCMP,IV,1)=VCICON(I,IV,1)+DELTAX
                   VCICON(ITCMP,IV,2)=VCICON(I,IV,2)+DELTAY
                   VCICON(ITCMP,IV,3)=VCICON(I,IV,2)+DELTAZ
 107            CONTINUE  

                DO 117 IC=1,NCONP(I)
                  CONCP(ITCMP,IC,1)=CONCP(I,IC,1)+DELTAX
                  CONCP(ITCMP,IC,2)=CONCP(I,IC,2)+DELTAY
                  ICNCT(ITCMP,IC)=ICNCT(I,IC)
 117            CONTINUE

C Copy the rest of the data.

C << check and see if this works for the case of flow network nodes and/or components. >>

                call copyicon(I,ITCMP,ier)
              ELSE
                RETURN
              ENDIF

C << Copy component parameters here .... >>
C <<            XXXXX >>

              CLOSEX=.FALSE.
              CLOSEY=.FALSE.
            ENDIF
 15       CONTINUE

          NNICN=ITCMP
          CALL NETWDRW

C Give the copied component a name 
          helptopic='unique_name_required'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(NWICNM(NNICN),'Give a (unique) name',
     &      '(with no blank spaces) ',12,ICDNAM,'newcmp',
     &      IER,nbhelp)

C Check for duplicate names ....
          icn=nnicn
          call dupicname(icn)
          RETURN
        ENDIF

C Add a component icon by asking the user to select from the
C icon database. First recover existing categories.
        CALL USRMSG('Please select from list:',' ','-')
C  8     call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
C     &    catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
C     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
  8     call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
        IF(IER.GT.0) GOTO 999
        found=.false.
        if(nbdomain.ge.1)then
          do 42 id=1,nbdomain
            if(idomain(id).eq.inwktyp)then
              found=.true.
              idf=id
            endif
  42      continue
        endif

        if(.NOT.found) goto 999
        id=idf
        NCAT=nbcat(id)  

C Set up a menu with the available categories.   
  717   ILEN=NCAT
        MHEAD=3
        MCTL=3   
        IPACT=CREATE
        CALL EKPAGE(IPACT)

        ILEN=NCAT
        WRITE(ICITEM1(1),'(A,A)') ' network domain: ',NWKTYPSTR(INWKTYP)
        WRITE(ICITEM1(2),'(A)') ' available categories: '
        WRITE(ICITEM1(3),'(A)') '  ______________________________ '

C If this is a flow domain and focused on nodes only accept the category
C with tags `inodes` or `bnodes`. If focused on components display all
C other categories.
        M=MHEAD
        DO 17 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            if(INWKTYP.eq.2.and.ITOG.eq.0)then
              if(cattag(id,L)(1:6).eq.'inodes'.or.
     &           cattag(id,L)(1:6).eq.'bnodes')then
                 M=M+1   ! found a node category
                 CALL EMKEY(L,KEY,IER)
                 WRITE(ICITEM1(M),'(A,1X,A)') key,catmenu(id,L)(1:31)
              endif
            elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
              if(cattag(id,L)(1:6).eq.'inodes'.or.
     &           cattag(id,L)(1:6).eq.'bnodes')then 
                 M=M+1     ! mark as not applicable
                 CALL EMKEY(L,KEY,IER)
                 WRITE(ICITEM1(M),'(A)') '  Not applicable'
              else
                 M=M+1     ! found a non-node category
                 CALL EMKEY(L,KEY,IER)
                 WRITE(ICITEM1(M),'(A,1X,A)') key,catmenu(id,L)(1:31)
              endif
            else
              M=M+1   ! not a flow domain so present all categories
              CALL EMKEY(L,KEY,IER)
              WRITE(ICITEM1(M),'(A,1X,A)') key,catmenu(id,L)(1:31)
            endif
          ENDIF
  17    CONTINUE

C If there is a long list then allow paging.
        IF(IPFLG.EQ.0)THEN  
          ICITEM1(M+1)='  ______________________________ '
        ELSE
          WRITE(ICITEM1(M+1),109)IPM,MPM 
  109     FORMAT   ('0 page: ',I2,' of ',I2,' --------')
        ENDIF
        ICITEM1(M+2)='? help'
        ICITEM1(M+3)='- exit this menu'
        NICITEM=M+MCTL
        IMOUT=-1
        CALL NETWDRW
  9     CALL EMENU('Choose a category:',ICITEM1,NICITEM,IMOUT)
        IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL+1) THEN
          CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Store the number of the selected icons category. If nothing selected
C then check again.
          NIT=ICHOSE
          if(NIT.eq.0)goto 9
          CALL EPMENSV
        ELSEIF(IMOUT.EQ.NICITEM-1) then
          helptopic='icon_categories'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Icon Category',nbhelp,'-',0,0,IER)
          GOTO 9
        ELSEIF(IMOUT.EQ.NICITEM) then
          RETURN
        ENDIF  

C Show components in the chosen category.
        MHEAD=3
        MCTL=4
        IMOUT2=-1
        IER=0
        ILEN=nbicons(id,NIT)
        IMOUT2=-1
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        WRITE(ICITEM2(1),'(A,1X,A)') ' category: ',catmenu(id,NIT)
        if(INWKTYP.eq.2.and.ITOG.eq.0)then
          WRITE(ICITEM2(2),'(A)')' select a node: '
        elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
          WRITE(ICITEM2(2),'(A)')' select a component: '
        else
          WRITE(ICITEM2(2),'(A)')' select a component: '
        endif
        WRITE(ICITEM2(3),'(A)')'  ______________________________ '
 11     M=MHEAD
        ILEN=nbicons(id,NIT)
        IMOUT2=-1
        DO 12 J=1,ILEN
          IF(J.GE.IST.AND.(J.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(J,KEY,IER)
            WRITE(ICITEM2(M),'(A,1X,A)')key,iconmenu(id,NIT,J)(1:40)
          ENDIF
 12     CONTINUE
        IF(IER.GT.0) GOTO 8
 14     IF(M.GT.0) THEN

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN  
            ICITEM2(M+1)='  ______________________________ '
          ELSE
            WRITE(ICITEM2(M+1),108)IPM,MPM 
 108        FORMAT   ('0 page: ',I2,' of ',I2,' --------')
          ENDIF
          IF(MREPT) THEN
            if(INWKTYP.eq.2.and.ITOG.eq.0)then
              ICITEM2(M+2)='>> node info ON         '
            elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
              ICITEM2(M+2)='>> component info ON    '
            else
              ICITEM2(M+2)='>> component info ON    '
            endif
          ELSE
            if(INWKTYP.eq.2.and.ITOG.eq.0)then
              ICITEM2(M+2)='>> node info OFF        '
            elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
              ICITEM2(M+2)='>> component info OFF   '
            else
              ICITEM2(M+2)='>> component info OFF   '
            endif
          ENDIF
          ICITEM2(M+3)  ='? help                  '
          ICITEM2(M+4)  ='- exit this menu        '
          NICITEM2=M+MCTL

          IMOUT2=-1   
          CALL NETWDRW   
 13       continue
          if(INWKTYP.eq.2.and.ITOG.eq.0)then
            CALL EMENU('Nodes in category',ICITEM2,NICITEM2,IMOUT2)
          elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
            CALL EMENU('Components in category',ICITEM2,NICITEM2,IMOUT2)
          else
            CALL EMENU('Components in category',ICITEM2,NICITEM2,IMOUT2)
          endif

          IF(IMOUT2.EQ.NICITEM2) THEN

C return to the previous menu ..
            GOTO 717
          ELSEIF(IMOUT2.EQ.NICITEM2-1) THEN
            helptopic='icon_cat_components'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('Component',nbhelp,'-',0,0,IER)
            GOTO 13
          ELSEIF(IMOUT2.EQ.NICITEM2-2) THEN
            IF(MREPT) THEN
              MREPT=.FALSE.
            ELSE
              MREPT=.TRUE.
            ENDIF
            GOTO 14
          ELSEIF(IMOUT2.EQ.NICITEM2-3) THEN

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
          ELSEIF(IMOUT2.GT.MHEAD.AND.IMOUT2.LT.NICITEM2-MCTL+1)THEN
            CALL KEYIND(NICITEM2,IMOUT2,ICHOSE2,IO)

C Store the number of the selected icons category.
            IVAL2=ICHOSE2

C Store the name of the selected string, compose NWICNHIS from the
C icon's original domain:catetory:name and get the data.
            category=cattag(id,NIT)
            ICDNAM=icontag(id,NIT,IVAL2)
            l2=lnblnk(NWKTYPSTR(INWKTYP))
            l3=lnblnk(category)
            l4=lnblnk(ICDNAM)
            write(NWICNHIS(ITCMP),'(5a)') NWKTYPSTR(INWKTYP)(1:l2),':',
     &        category(1:l3),':',ICDNAM(1:l4)

C Make up a name depending on whether it is a node or a flow component
C and use the counter for each in the name.
            if(INWKTYP.eq.2.and.ITOG.eq.0)then
              NBFNODES=NBFNODES+1   ! increment number of flow nodes
              write(NWICNM(ITCMP),'(a,i2.2)') 'Node_',NBFNODES
            elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
              NBFCOMP=NBFCOMP+1     ! increment number of flow components
              write(NWICNM(ITCMP),'(a,i2.2)') 'Comp_',NBFCOMP
            else
              NWICNM(ITCMP)='New_Comp'
            endif

C Check if there is a matching entity.
            call geticonindex(inwktyp,category,ICDNAM,idbcat,idbitem,
     &        IER)
 
C Get details of icon and then get its attributes (if any).
            if(idbitem.gt.0)then 
              call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &          nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &          nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)
            else
              call usrmsg('Could not locate a matching entity',
     &          'in the icon database.','W')
            endif

            write(NWICNME(ITCMP),'(a)') icmenu(1:36)
    
C Debug.
C            write(6,*) nbvert,nbedge,nbdot,nblabel,nbarc,nbatt,nbtext
C            write(6,*) iedge
C            write(6,*) idot
C            write(6,*) ilabel
C            write(6,*) inwktyp,category
   
            call geticonatr(inwktyp,category,ICDNAM,natrib,
     &        tagatr,atrib,menuatr,iconphrase,IER)
     
C Debug.
C           write(6,*) ICDNAM 
C           write(6,*) natrib 
C           write(6,*) tagatr 
C           write(6,*) atrib 
C           write(6,*) iconphrase 

C For (natrib times) extract attributes of the icon (tags, values,
C menu entries). Note atrib can have up to MIATRB attributes. Also
C remember (most) of the descriptive phrase from the selection list.
            NICONATR(ITCMP)=natrib
            write(NWICNME(ITCMP),'(a)')iconphrase(1:36)
            if(natrib.ge.1)then
              do 39 IV=1,NICONATR(ITCMP)
                ATRTAG(ITCMP,IV,1)=tagatr(IV,1)
                ATRTAG(ITCMP,IV,2)=tagatr(IV,2)
                ATRTAG(ITCMP,IV,3)=tagatr(IV,3)
                ATRTAG(ITCMP,IV,4)=tagatr(IV,4)
                ATRTAG(ITCMP,IV,5)=tagatr(IV,5)
                ATRICN(ITCMP,IV,1)=atrib(IV,1)
                ATRICN(ITCMP,IV,2)=atrib(IV,2)
                ATRICN(ITCMP,IV,3)=atrib(IV,3)
                ATRMENU(ITCMP,IV)=menuatr(IV)
  39          continue
            endif

C For (nbvert times) extract vert data.
            NIVC(ITCMP)=nbvert
            DO 40 IV=1,NIVC(ITCMP)
              VCICON(ITCMP,IV,1)=vert(IV,1)*XSIZIC
              VCICON(ITCMP,IV,2)=vert(IV,2)*YSIZIC 
  40        CONTINUE

C For (nbedge times) extract edge vertex indices colour index, colour
C type, line type.
            NIVE(ITCMP)=nbedge
            if(nbedge.ge.1)then
              DO 41 IE=1,NIVE(ITCMP)
                IVEICN(ITCMP,IE,1)=iedge(IE,1)
                IVEICN(ITCMP,IE,2)=iedge(IE,2)
                IVEICN(ITCMP,IE,3)=iedge(IE,3)
                IVEICN(ITCMP,IE,4)=iedge(IE,4)
                IVEICN(ITCMP,IE,5)=iedge(IE,5)
  41          CONTINUE
            endif
  
C For nbdot times extract dot vertex index, colour index and size
            NIVD(ITCMP)=nbdot
            if(nbdot.ge.1)then
              do 143 IE=1,NIVD(ITCMP)
                IVDOT(ITCMP,IE,1)=idot(IE,1)
                IVDOT(ITCMP,IE,2)=idot(IE,2)
                IVDOT(ITCMP,IE,3)=idot(IE,3)
                IVDOT(ITCMP,IE,4)=idot(IE,4)
 143          continue
            endif

C For nbarc times extract arc centre vertex index, radiaus vertex index,
C 1st and 2nd angles, colour index and fill type
            NIVA(ITCMP)=nbarc
            if(nbarc.ge.1)then
              do 145 IE=1,NIVA(ITCMP)
                IVARC(ITCMP,IE,1)=iarc(IE,1)
                IVARC(ITCMP,IE,2)=iarc(IE,2)
                IVARC(ITCMP,IE,3)=iarc(IE,3)
                IVARC(ITCMP,IE,4)=iarc(IE,4)
                IVARC(ITCMP,IE,5)=iarc(IE,5)
                IVARC(ITCMP,IE,6)=iarc(IE,6)
                IVARC(ITCMP,IE,7)=iarc(IE,7)
 145          continue
            endif
  
C For nblabel times extract internal label vertex index, colour index and text
            NIVL(ITCMP)=nblabel
            if(nblabel.ge.1)then
              do 144 IE=1,NIVL(ITCMP)
                IVLBL(ITCMP,IE,1)=ilabel(IE,1)
                IVLBL(ITCMP,IE,2)=ilabel(IE,2)
                IVLBL(ITCMP,IE,3)=ilabel(IE,3)
                NWICNLBL(ITCMP,IE)=labeltx(IE)
 144          continue
            endif

C For (nbatt times) extract attachment point vertex X and Y. Icc
C is the index of the vertex associated with the attachment point.
C Also set the type of connection (ICNCT)
C << find place to double check that both ends of connection are
C << of the same type.
            NCONP(ITCMP)=nbatt
            DO 142 IC=1,NCONP(ITCMP)
              iic=iatt(ic,1)
              CONCP(ITCMP,IC,1)=vert(iic,1)*XSIZIC
              CONCP(ITCMP,IC,2)=vert(iic,2)*YSIZIC
              ICNCT(ITCMP,IC)=iatt(ic,2)
 142        CONTINUE      
          ELSE
            IMOUT2=-1
            GOTO 11   
          ENDIF
        ELSE
          IMOUT=-1 
          GOTO 717
        ENDIF

C Place it one the drawing area. If graphic mode does not support
C mouse click then ask for X Y Z.                                       
        if(iglib.eq.1)then
          CALL USRMSG('Now position it on the screen',' ','-')
          CALL TRACKVIEW(iii,iix,iiy)
          CALL PIXEL2U(IIX,IIY,GX,GY)
          CGX=GX; CGY=GY
        else
          hold = ' 1.000  1.000  1.000    '
          CALL EASKS(HOLD,'Position (X Y Z metres):',' ',36,
     &      ' 1. 1. 1.','position XYZ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,CGX,-99.,99.,'W','Xm',IER)
          CALL EGETWR(HOLD,K,CGY,-99.,99.,'W','Ym',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Zm',IER)
        endif
        IF(IVIEW.EQ.1)THEN
          GRMAX1=GRMAX(1); GRMAX2=GRMAX(2)
        ELSEIF(IVIEW.EQ.2)THEN
          GRMAX1=GRMAX(1); GRMAX2=GRMAX(2)
        ELSE
          GRMAX1=GRMAX(2); GRMAX2=GRMAX(3)
        ENDIF
        IF((CGX.LT.GRMAX1.AND.CGX.GT.0.0).AND.
     &     (CGY.LT.GRMAX2.AND.CGY.GT.0.0))THEN

C Snap the component to the grid if this is active. 
          CALL NETWSNAP(CGX,CGY,CGXS,CGYS)
          CGX=CGXS; CGY=CGYS

C Finally translate the component image to the selected centre.
           DO 100 IV=1,NIVC(ITCMP)
             VCICON(ITCMP,IV,1)=CGX+VCICON(ITCMP,IV,1)-XSIZIC/2.
             VCICON(ITCMP,IV,2)=CGY+VCICON(ITCMP,IV,2)-YSIZIC/2.
 100       CONTINUE  

           DO 110 IC=1,NCONP(ITCMP)
             CONCP(ITCMP,IC,1)=CGX+CONCP(ITCMP,IC,1)-XSIZIC/2.
             CONCP(ITCMP,IC,2)=CGY+CONCP(ITCMP,IC,2)-YSIZIC/2.
 110       CONTINUE

           NNICN=ITCMP    
           CALL NETWDRW
        ELSE
          RETURN
        ENDIF

C Update the number of component icons.
        IF(IVIEW.EQ.1)THEN
          XYZICON(NNICN,1)=CGX      
          XYZICON(NNICN,2)=CGY  
          XYZICON(NNICN,3)=VIEWCEN(3) 
        ELSEIF(IVIEW.EQ.2)THEN 
          XYZICON(NNICN,1)=CGX      
          XYZICON(NNICN,2)=VIEWCEN(2) 
          XYZICON(NNICN,3)=CGY   
        ELSE
          XYZICON(NNICN,1)=VIEWCEN(1)    
          XYZICON(NNICN,2)=CGX  
          XYZICON(NNICN,3)=CGY
        ENDIF

        ICONTP(NNICN)=1
        CALL NETWDRW

C If user just defined a node use their name for it otherwise
C suggest the component name from the icon database.
        if(INWKTYP.eq.2.and.ITOG.eq.0)then
          continue
        else
          NWICNM(NNICN)=ICDNAM
        endif
        helptopic='unique_name_required'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(NWICNM(NNICN),'Give a (unique) name',
     &    '(with no blank spaces) ',12,ICDNAM,'newcmp',
     &    IER,nbhelp)

C Check for duplicate names .... 
        icn=nnicn
        call dupicname(icn) 

C Debug.  write(6,*) 'just after naming of icon ',icn

        write(outs,'(2a)')  'Loading attributes of ',NWICNM(NNICN) 
        call edisp(iuout,outs)
        icnpick=icn

        call NETICONDATR(icnpick,ier)

      ELSEIF(IOPT.EQ.2)THEN

C Delete selected components.
        IF(IMODE.NE.1) THEN

C Let the user select an icon to delete (all calls to netwmic have
C had imode set to zero).
          if(iglib.eq.1)then
            if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
             CALL USRMSG(' click on the component to delete',' ','-')
            elseif(INWKTYP.eq.2)then
             CALL USRMSG(' click on the node/component to delete',' ',
     &         '-')
            elseif(INWKTYP.eq.4)then
             CALL USRMSG(' click on the control entity to delete',' ',
     &         '-')
            endif
            CALL TRACKVIEW(iii,iix,iiy)
            CALL PIXEL2U(IIX,IIY,GX,GY)
            CGX=GX; CGY=GY
          else

C Non-X11: If a node then pull up list of nodes, if component pull up a list
C of components.  Figure out which icon and remember its name for confirmation.
            CALL NETTOFLW(ier) ! update commons

C Loop clearing current selections setting isel()=.false.
            do I=1,NNICN
              ISEL(I)=.FALSE.
            enddo
            if(itog.eq.0)then
              CALL EPMENSV
              call ASKRNOD('Select node to delete:','-',ISD,IER)
C Debug
              write(6,*) 'node to delete ',isd,icontoflow(isd),
     &          inodetoicon(isd)
              CALL EPMENRC
              I=inodetoicon(isd)
              ISEL(I)=.TRUE.
              name=NWICNM(I)

            elseif(itog.eq.1)then
              CALL EPMENSV
              call ASKRCMP('Select component to delete:','-',ICMP,IER)
              CALL EPMENRC
C Debug
              write(6,*)'comp to delete',icmp,icontoflow(icmp),
     &          icomptoicon(icmp)
              I=icomptoicon(icmp)
              ISEL(I)=.TRUE.
              name=NWICNM(I)

            endif
          endif

C If X11 search for close points.
          if(iglib.eq.1)then
            CLOSEX=.FALSE.; CLOSEY=.FALSE.

C Loop through all icons, clearing current selections and setting
C isel()=.true. to the one within 0.3m.
            DO 705 I=1,NNICN
              ISEL(I)=.FALSE.
              IF(IVIEW.EQ.1)THEN
                CALL ECLOSE(CGX,XYZICON(I,1),0.3,CLOSEX)
                CALL ECLOSE(CGY,XYZICON(I,2),0.3,CLOSEY)
              ELSEIF(IVIEW.EQ.2)THEN
                CALL ECLOSE(CGX,XYZICON(I,1),0.3,CLOSEX)
                CALL ECLOSE(CGY,XYZICON(I,3),0.3,CLOSEY)
              ELSE
                CALL ECLOSE(CGX,XYZICON(I,2),0.3,CLOSEX)
                CALL ECLOSE(CGY,XYZICON(I,3),0.3,CLOSEY)
              ENDIF

              IF(CLOSEX.AND.CLOSEY)THEN
                ISEL(I)=.TRUE.
                name=NWICNM(I)
              ENDIF
  705       CONTINUE
          endif
          CALL NETWDRW

C Confirm that this is the icon to be deleted.
          write(outs,'(3a)') 'Delete the icon ',name(1:lnblnk(name)),
     &      ' and associated connections?'
          dok=.false.
          helptopic='pause_to_consider'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(outs,' ',OK,dok,nbelp)
        ELSE
          OK=.TRUE.
        ENDIF
        IF(OK)THEN

C Delete chosen icon and compact the list of icons.
C << NOTE: IC has not been set and is not used >>
          call compacticon(IC,ier)
          CALL NETWDRW
        ENDIF

      ELSEIF(IOPT.EQ.4)THEN

C Edit the icon data attributes. NETIDATR is for icons with lots of
C attributes and NETICONDATR is for ones with few attributes. Get the
C current mouse position and loop through icons for one within 0.3m.
C        call NETIDATR(ier)
        icnpick=0   ! reset
        if(iglib.eq.1)then
          if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
           CALL USRMSG('Click on the component to edit',' ','-')
          elseif(INWKTYP.eq.2)then
           CALL USRMSG('Click on the node/component to edit',' ','-')
          elseif(INWKTYP.eq.4)then
           CALL USRMSG('Click on the control entity to edit',' ','-')
          endif
          CALL TRACKVIEW(iii,iix,iiy)
          CALL PIXEL2U(IIX,IIY,GX,GY)
          CGX=GX; CGY=GY
        else

C Non-X11: If a node then pull up list of nodes, if component pull up a list
C of components.  Figure out which icon and remember its name for confirmation.
          CALL NETTOFLW(ier) ! update commons

C Loop clearing current selections setting isel()=.false.
          do I=1,NNICN
            ISEL(I)=.FALSE.
          enddo
          if(itog.eq.0)then
            CALL EPMENSV
            call ASKRNOD('Select node to edit:','-',ISD,IER)
C Debug
            write(6,*) 'node to edit ',isd,icontoflow(isd),
     &        inodetoicon(isd)
            CALL EPMENRC
            ICN=inodetoicon(isd)
            ISEL(ICN)=.TRUE.
            icnpick=icn

          elseif(itog.eq.1)then
            CALL EPMENSV
            call ASKRCMP('Select component to edit:','-',ICMP,IER)
            CALL EPMENRC
C Debug
            write(6,*)'comp to edit',icmp,icontoflow(icmp),
     &        icomptoicon(icmp)
            ICN=icomptoicon(icmp)
            ISEL(ICN)=.TRUE.
            icnpick=icn

          endif
          CLOSEX=.TRUE.; CLOSEY=.TRUE.

        endif

C If X11 search for close points.
        if(iglib.eq.1)then
          CLOSEX=.FALSE.; CLOSEY=.FALSE.
          DO ICN=1,NNICN
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,XYZICON(ICN,1),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(ICN,2),0.3,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,XYZICON(ICN,1),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(ICN,3),0.3,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,XYZICON(ICN,2),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(ICN,3),0.3,CLOSEY)
            ENDIF
            IF(CLOSEX.AND.CLOSEY) icnpick=icn
          enddo
          if(icnpick.ne.0) icn=icnpick
        endif

        IF(icnpick.ne.0)THEN

C Decode NWICNHIS() so that getanicon can be called to populate NWICNME.
          call getutokens(NWICNHIS(icn),':',IW,WORDSS)
          write(category,'(a)') WORDSS(2)(1:lnblnk(WORDSS(2)))
          write(ICDNAM,'(a)') WORDSS(3)(1:lnblnk(WORDSS(3)))

C Check if there is a matching entity.
          call geticonindex(inwktyp,category,ICDNAM,idbcat,idbitem,
     &      IER)
 
C Get details of icon and then get its attributes (if any).
          if(idbitem.gt.0)then 
            call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &        nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &        nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)
            write(NWICNME(icn),'(a)') icmenu(1:36)
          else
            call usrmsg('Could not locate a matching entity',
     &        'in the icon database.','W')
          endif

          call NETICONDATR(icnpick,ier)
        else
          continue
        endif

      ELSEIF(IOPT.EQ.5)THEN

C List the icon details. Get current mouse position and then
C find the icon within 0.3m of this.
        if(iglib.eq.1)then
          if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
           CALL USRMSG('Click on the component to list',' ','-')
          elseif(INWKTYP.eq.2)then
           CALL USRMSG('Click on the node/component to list',' ','-')
          elseif(INWKTYP.eq.4)then
           CALL USRMSG('Click on the control entity to list',' ','-')
          endif
          CALL TRACKVIEW(iii,iix,iiy)
          CALL PIXEL2U(IIX,IIY,GX,GY)
          CGX=GX; CGY=GY
        else

C Non-X11: If a node then pull up list of nodes, if component pull up a list
C of components.  Figure out which icon and remember its name for confirmation.
          CALL NETTOFLW(ier) ! update commons

          icn=0   ! reset
C Loop clearing current selections setting isel()=.false.
          do I=1,NNICN
            ISEL(I)=.FALSE.
          enddo
          if(itog.eq.0)then
            CALL EPMENSV
            call ASKRNOD('Select node to list:','-',ISD,IER)
C Debug
            write(6,*) 'node to list ',isd,icontoflow(isd),
     &        inodetoicon(isd)
            CALL EPMENRC
            ICN=inodetoicon(isd)
            ISEL(ICN)=.TRUE.

          elseif(itog.eq.1)then
            CALL EPMENSV
            call ASKRCMP('Select component to list:','-',ICMP,IER)
            CALL EPMENRC
C Debug
            write(6,*)'comp to list',icmp,icontoflow(icmp),
     &        icomptoicon(icmp)
            ICN=icomptoicon(icmp)
            ISEL(ICN)=.TRUE.

          endif
          CLOSEX=.TRUE.; CLOSEY=.TRUE.

        endif
        if(iglib.eq.1)then
          CLOSEX=.FALSE.; CLOSEY=.FALSE.
          DO I=1,NNICN
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,2),0.3,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.3,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,XYZICON(I,2),0.3,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.3,CLOSEY)
            ENDIF
            IF(CLOSEX.AND.CLOSEY) icn=i
          enddo
        endif

        IF(icn.ne.0)THEN

C Decode NWICNHIS() so that getanicon can be called to populate NWICNME.
          call getutokens(NWICNHIS(icn),':',IW,WORDSS)
          write(category,'(a)') WORDSS(2)(1:lnblnk(WORDSS(2)))
          write(ICDNAM,'(a)') WORDSS(3)(1:lnblnk(WORDSS(3)))
          call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &      nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &      nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)

          WRITE(outs,'(2A)') 'Name: ', NWICNM(I)(1:lnblnk(NWICNM(I)))
          call edisp(iuout,outs)
          WRITE(outs,'(A)')
     &      '  domain      |category    |name          '
          call edisp(iuout,outs)
          WRITE(outs,'(A)') NWICNHIS(I)(1:40)
          call edisp(iuout,outs)
          if(nbtext.gt.0)then
            WRITE(outs,'(A)') 'Notes: '
            call edisp(iuout,outs)
            do 116 jh=1,nbtext
              WRITE(outs,'(A)') text(jh)
              call edisp(iuout,outs)
  116       continue
          endif
          if(NICONATR(I).gt.0)then
            WRITE(outs,'(A)') 'Attributes: '
            call edisp(iuout,outs)
            DO 520 jh=1,NICONATR(I)
              l3=lnblnk(ATRTAG(I,JH,1))
              l6=lnblnk(ATRMENU(I,JH))
              l7=MAX0(lnblnk(ATRICN(I,JH,1)),1)
              WRITE(outs,'(6A)') ' ',ATRTAG(I,jh,1)(1:l3),': ',
     &          ATRMENU(I,jh)(1:l6),' is ',ATRICN(I,jh,1)(1:l7)
              call edisp(iuout,outs)
 520        continue
          endif
          return
        endif
        RETURN
      ENDIF

      RETURN

999   CALL EDISP(IUOUT,'ERROR: problem opening icon database, aborting')
      RETURN
      
      END 

C ********************* NETWMCO ***********************************
C This routine manages the creation of connections, the routine can
C be called from the c-side to delete a connection (mode='c')

        SUBROUTINE NETWMCO(IMODE,IMOPT)     
#include "gnetwk.h"
#include "net_flow.h"
#include "epara.h"

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C Grid commons
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

      integer iwh  ! for selection
      character hold*32
      real cgx,cgy

      LOGICAL ISEL,CSEL,OK,DOK,CLOSEX,CLOSEY
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

#ifdef OSI
      integer iii,iix,iiy,iiwx,iiwy ! for use with trackview
#else
      integer*8 iii,iix,iiy,iiwx,iiwy  ! for use with trackview
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='NETWMCO'  ! set for subroutine

C Depending on if user wants to add/delete/copy/edit a component      
      IOPT=IMOPT
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1)then
        continue
      elseif(iglib.eq.2)then
        continue
      else
        call usrmsg('Current version does not support mouse tracking.',
     &    'Sorry about that...','W')
        return
      endif

      IF(IOPT.EQ.1)THEN
        if(iglib.eq.1)then

C Adding a connection, ask user preference for mouse input or menu input.
          helptopic='check_mouse_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkabc('Options:',' ','select via mouse',
     &      'select via menu','continue',iwh,nbhelp)
          if(iwh.eq.1)then
            call USRMSG(
     &'Connect: button1-start icon; button3-end icon (later highlight',
     &'via button1 & adjust way-points via button3)','-')
            return
          elseif(iwh.eq.2)then

C Use similar pattern to text interface to get index of
C the two nodes and the component via MCONAD.
            CALL NETTOFLW(ier) ! update commons
            CALL EPMENSV
            call ASKRNOD('Select first node:','-',IS,IER)
C Debug     write(6,*) 'start node',is,icontoflow(is),inodetoicon(is)
            CALL EPMENRC
            CALL EPMENSV
            call ASKRNOD('Select second node:','-',IE,IER)
C Debug     write(6,*) 'end node ',ie,icontoflow(ie),inodetoicon(ie)
            CALL EPMENRC
            CALL EPMENSV
            call ASKRCMP('Via component:','-',ICMP,IER)
            CALL EPMENRC
C Debug     write(6,*)'comp ',icmp,icontoflow(icmp),icomptoicon(icmp)
            call MCONAD(inodetoicon(is),icomptoicon(icmp))
            call MCONAD(icomptoicon(icmp),inodetoicon(ie))
          endif
        elseif(iglib.eq.2)then

C GTK interface, use similar pattern to text interface to get index of
C the two nodes and the component rather than GCONAD use MCONAD.
          CALL NETTOFLW(ier) ! update commons
          CALL EPMENSV
          call ASKRNOD('Select first node:','-',IS,IER)
C Debug   write(6,*) 'start node',is,icontoflow(is),inodetoicon(is)
          CALL EPMENRC
          CALL EPMENSV
          call ASKRNOD('Select second node:','-',IE,IER)
C Debug   write(6,*) 'end node ',ie,icontoflow(ie),inodetoicon(ie)
          CALL EPMENRC
          CALL EPMENSV
          call ASKRCMP('Via component:','-',ICMP,IER)
          CALL EPMENRC
C Debug   write(6,*)'component ',icmp,icontoflow(icmp),icomptoicon(icmp)
          call MCONAD(inodetoicon(is),icomptoicon(icmp))
          call MCONAD(icomptoicon(icmp),inodetoicon(ie))
        endif

      ELSEIF(IOPT.EQ.2)THEN

C Let the user select a connection to delete. Get current position
C and look through all of the way-points for one within 0.25m.
C << why 0.5m here and 0.2m below? >>       
        CALL USRMSG('Click on a connection way-point to delete',
     &  '(you will be asked to confirm this action).','-')
        if(iglib.eq.1)then
          CALL TRACKVIEW(iii,iix,iiy)
          CALL PIXEL2U(IIX,IIY,GX,GY)
          CGX=GX; CGY=GY
        else
          helptopic='position_manual'
          call gethelptext(helpinsub,helptopic,nbhelp)
          hold = ' 1.000  1.000  1.000    '
          CALL EASKS(HOLD,'Position @ waypoint to delete (X Y Z m):',
     &      ' ',36,' 1. 1. 1.','wp position XYZ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,CGX,-99.,99.,'W','Xm',IER)
          CALL EGETWR(HOLD,K,CGY,-99.,99.,'W','Ym',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Zm',IER)
        endif

        CLOSEX=.FALSE.; CLOSEY=.FALSE.
        DO 705 I=1,NICNN
          CSEL(I)=.FALSE.
          DO 706 J=1,NCONWP(I)
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,CNWNP(I,J,1),0.25,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(I,J,2),0.25,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,CNWNP(I,J,1),0.25,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(I,J,3),0.25,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,CNWNP(I,J,2),0.25,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(I,J,3),0.25,CLOSEY)
            ENDIF
            IF(CLOSEX.AND.CLOSEY)THEN
              CSEL(I)=.TRUE.
            ENDIF
  706     CONTINUE  
  705   CONTINUE

C Delete all selected connections.
        IF(IMODE.NE.1) THEN
          CALL NETWDRW
          dok=.true.
          helptopic='pause_to_consider'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK('Delete selected connection?',' ',
     &      OK,dok,nbhelp)
        ELSE
          OK=.TRUE.
        ENDIF
        IF(OK)THEN
          NWCNN=0
          DO 25 J=1,NICNN
            IF(.NOT.CSEL(J))THEN
              NWCNN=NWCNN+1
              ICNS(NWCNN)=ICNS(J)
              ICNE(NWCNN)=ICNE(J)
              ICNNT(NWCNN)=ICNNT(J)
              ICNSP(NWCNN)=ICNSP(J)
              ICNEP(NWCNN)=ICNEP(J)                  
              NCONWP(NWCNN)=NCONWP(J)
              DO 27 K=1,NCONWP(J)
                CNWNP(NWCNN,K,1)=CNWNP(J,K,1)
                CNWNP(NWCNN,K,2)=CNWNP(J,K,2)
                CNWNP(NWCNN,K,3)=CNWNP(J,K,3)
  27          CONTINUE
              idatrdom(NWCNN)=idatrdom(J)

              DO 28 K=1,MIATRB
                ddmenuatr(NWCNN,K)=ddmenuatr(J,K)
                DO 29 L=1,5
                  ddtagatr(NWCNN,K,L)=ddtagatr(J,K,L)
                  if(L.le.3)ddatrib(NWCNN,K,L)=ddatrib(J,K,L)
  29            CONTINUE
  28          CONTINUE
              CSEL(NWCNN)=CSEL(J)
            ENDIF
  25      CONTINUE
          NICNN=NWCNN
          CALL NETWDRW
        ENDIF
      ELSEIF(IOPT.EQ.4)THEN

C Edit the conection data. Get mouse position and then look through
C the list of way-points for one within 0.25m in the X and Y.
        CALL USRMSG('Click on a connection (way-point) to identify',
     &    'what to edit.','-')
        if(iglib.eq.1)then
          CALL TRACKVIEW(iii,iiwx,iiwy)
          CALL PIXEL2U(IIWX,IIWY,GX,GY)
          CGX=GX; CGY=GY
        else
          helptopic='position_manual'
          call gethelptext(helpinsub,helptopic,nbhelp)
          hold = ' 1.000  1.000  1.000    '
          CALL EASKS(HOLD,'Position @ waypoint to edit (X Y Z m):',
     &      ' ',36,' 1. 1. 1.','wp position XYZ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,CGX,-99.,99.,'W','Xm',IER)
          CALL EGETWR(HOLD,K,CGY,-99.,99.,'W','Ym',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Zm',IER)
        endif
        CLOSEX=.FALSE.; CLOSEY=.FALSE.
        IMATCH=0
        DO 515 JJ=1,NICNN
          DO 516 ICWP=1,NCONWP(JJ)
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,CNWNP(JJ,ICWP,1),0.25,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(JJ,ICWP,2),0.25,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,CNWNP(JJ,ICWP,1),0.25,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(JJ,ICWP,3),0.25,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,CNWNP(JJ,ICWP,2),0.25,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(JJ,ICWP,3),0.25,CLOSEY)
            ENDIF
            IF(CLOSEX.AND.CLOSEY)THEN
              IMATCH=JJ
              CLOSEX=.FALSE.
              CLOSEY=.FALSE.
            ENDIF
 516      CONTINUE
 515    continue

C If we have a match proceed otherwise return.
        if(imatch.eq.0)then
          return
        else
          J=imatch
          CALL NETCNNDATR(IER,J)
        endif
      ELSE
        RETURN
      ENDIF

      RETURN
      END

C ****************************** NETWIDW ******************************
C This routine handles the drawing of icons on the display. 

      SUBROUTINE NETWIDW(ICN)

# include "gnetwk.h"

C Grid commons
      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH

C Icon commons are in gnetwk.h
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      LOGICAL ISEL,CSEL
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

C connection points property array
C      COMMON/CNP/CPP(3,MNCNN),ICPP(MICN,4,3),III

C connection points parameter array
C      COMMON/CPPA/ENV(5),PARA(MNCNN,13),CPARA(MICN,4,13),
C     &  SUB_MATRIX_A(MICN,MNCNN,MNCNN),SUB_MATRIX_B(MICN,MNCNN),
C     &  MATRIX_A(MNCNN,MNCNN),MATRIX_B(MNCNN),ICOUNTER,IDONE

      CHARACTER LABL*12,LBLINT*4

C      CHARACTER CPP*30,ICPP*30
C      REAL ENV,PARA,CPARA,SUB_MATRIX_A,SUB_MATRIX_B
C      REAL MATRIX_A,MATRIX_B

      LOGICAL INBOUND,CLOSE
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
      integer iix1,iiy1,iix2,iiy2  ! passed to edline
      integer iixlc,isize
      integer iix,iiy,iixl,iiyl,iixr,iiyu
      integer iicol
      integer iiw4
#else
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iix1,iiy1,iix2,iiy2 ! passed to edline
      integer*8 iixlc,isize
      integer*8 iix,iiy,iixl,iiyl,iixr,iiyu
      integer*8 iicol
      integer*8 iiw4
#endif

      X=XYZICON(ICN,1); Y=XYZICON(ICN,2)

      INBOUND=.TRUE.

C Get view limits and check if the component should be plotted in this view. 
      IF(IVIEW.EQ.1)THEN
        X=XYZICON(ICN,1); Y=XYZICON(ICN,2)

C << do not check for Z differences in current version of net >>
C        CALL ECLOSE(XYZICON(ICN,3),VIEWCEN(3),0.01,CLOSE) 
C        IF(.NOT.CLOSE) RETURN
      ELSEIF(IVIEW.EQ.2)THEN
        X=XYZICON(ICN,1); Y=XYZICON(ICN,3)
        CALL ECLOSE(XYZICON(ICN,2),VIEWCEN(2),0.01,CLOSE) 
        IF(.NOT.CLOSE) RETURN
      ELSE
        X=XYZICON(ICN,2); Y=XYZICON(ICN,3)
        CALL ECLOSE(XYZICON(ICN,1),VIEWCEN(1),0.01,CLOSE) 
        IF(.NOT.CLOSE) RETURN
      ENDIF

      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C Get pixel position of the point and plot the icon and its label.
      CALL U2PIXEL(X,Y,iix,iiy)

C Get the pixel position of the lower and left edge of the component
      CALL U2PIXEL(X-XSIZIC,Y-YSIZIC,iixl,iiyl)
      CALL U2PIXEL(X+XSIZIC,Y+YSIZIC,iixr,iiyu)

C Check the component is on the graphics screen
      IF(IIXL.GT.IGL.AND.IIXR.LT.IGR.AND.IIYU.GT.IGT.AND.
     &   IIYL.LT.IGB)THEN

C Move to the first point and plot the icon in highlighted mode.
        if(NIVE(ICN).ge.1)then
          DO 10 I=1,NIVE(ICN)
            IS=IVEICN(ICN,I,1)
            IE=IVEICN(ICN,I,2)
            call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),iix1,iiy1)
            call u2pixel(VCICON(ICN,IE,1),VCICON(ICN,IE,2),iix2,iiy2)
            iicol=IVEICN(ICN,I,3)
            if(IVEICN(ICN,I,4).eq.0)then
              call winscl('i',iicol)
            elseif(IVEICN(ICN,I,4).eq.1)then
              call winscl('g',iicol)
            elseif(IVEICN(ICN,I,4).eq.2)then
              call winscl('z',iicol)
            endif

C IVEICN(n,n,5) line type (1=solid, 2=dotted, 3=dashed, 4=double)
C If select draw double thickness, otherwise use the line style of the icon.
            if(ISEL(ICN))then
              call edwline(iix1,iiy1,iix2,iiy2)
            else
              if(IVEICN(ICN,I,5).eq.0)then
                call eswline(iix1,iiy1,iix2,iiy2)
              elseif(IVEICN(ICN,I,5).eq.1)then
                call eswline(iix1,iiy1,iix2,iiy2)
              elseif(IVEICN(ICN,I,5).eq.2)then
                iiw4=3
                call edline(iix1,iiy1,iix2,iiy2,iiw4)
              elseif(IVEICN(ICN,I,5).eq.3)then
                iiw4=3
                call edash(iix1,iiy1,iix2,iiy2,iiw4)
              elseif(IVEICN(ICN,I,5).eq.4)then
                call edwline(iix1,iiy1,iix2,iiy2)
              endif
            endif
            iicol=0
            call winscl('-',iicol)
            call forceflush()
  10      CONTINUE
        endif

C Do any dots.
        if(NIVD(ICN).ge.1)then
          DO 11 I=1,NIVD(ICN)
            IS=IVDOT(ICN,I,1)
            call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),iix1,iiy1)
            iicol=IVDOT(ICN,I,2)
            if(IVDOT(ICN,I,3).eq.0)then
              call winscl('i',iicol)
            elseif(IVDOT(ICN,I,3).eq.1)then
              call winscl('g',iicol)
            elseif(IVDOT(ICN,I,3).eq.2)then
              call winscl('z',iicol)
            endif

C Draw small or large dot.
            if(IVDOT(ICN,I,4).eq.0)then
              call esymbol(iix1,iiy1,8,1)
            elseif(IVDOT(ICN,I,4).eq.1)then
              call esymbol(iix1,iiy1,33,1)
            else
              call esymbol(iix1,iiy1,33,1)
            endif
            iicol=0
            call winscl('-',iicol)
            call forceflush()
  11      CONTINUE
        endif

C Do any arcs. Get pixel coords of centre and radius, then find the
C pixel radius (ir1) via call to crowxyz. Other 3D point on line
C logic is in edgeo.F near line 5522.
        if(NIVA(ICN).ge.1)then
          DO 9 I=1,NIVA(ICN)
            IS=IVARC(ICN,I,1)
            IR=IVARC(ICN,I,2)
            z1=0.0
            z2=0.0
            call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),iix1,iiy1)
            call u2pixel(VCICON(ICN,IR,1),VCICON(ICN,IR,2),iix2,iiy2)
            ir1 = nint(crowxyz(real(iix1),real(iiy1),z1,real(iix2),
     &          real(iiy2),z2))
            iang1=IVARC(ICN,I,3)
            iang2=IVARC(ICN,I,4)
            iicol=IVARC(ICN,I,5)
            if(IVARC(ICN,I,6).eq.0)then
              call winscl('i',iicol)
            elseif(IVARC(ICN,I,6).eq.1)then
              call winscl('g',iicol)
            elseif(IVARC(ICN,I,6).eq.2)then
              call winscl('z',iicol)
            endif

C Debug     write(6,*) iix1,iiy1,iix2,iiy2,is,ir,ir1,iang1,iang2,iicol

C Draw unfilled or filled arc. If icon highlighted draw 2nd arc with
C radius one pixel larger in grey.
            if(IVARC(ICN,I,7).eq.0)then
              call earc(iix1,iiy1,ir1,iang1,iang2,0)
              if(ISEL(ICN))then
                iicol=1
                call winscl('z',iicol)
                call earc(iix1,iiy1,ir1+1,iang1,iang2,0)
              endif
            else
              call earc(iix1,iiy1,ir1,iang1,iang2,1)
            endif
            iicol=0
            call winscl('-',iicol)
            call forceflush()
   9      CONTINUE
        endif

C Do any internal labels in smallest font in the specified colour.
        DO 12 I=1,NIVL(ICN)
          IS=IVLBL(ICN,I,1)
          call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),iix1,iiy1)
          iicol=IVLBL(ICN,I,2)
          icol=IVLBL(ICN,I,2)
          isize=0
          if(iglib.eq.1)then
            IF(IFS.GT.0)CALL winfnt(0)
          elseif(iglib.eq.2)then
            IF(IFS.ne.4)CALL winfnt(4)
          endif
          line=1; icx=1  ! in case GTK set font context
          call findviewtext(icx,line,isize,ix,iy)
          write(LBLINT,'(a)') NWICNLBL(ICN,I) 
          if(IVLBL(ICN,I,3).eq.0)then
            call winscl('i',iicol)
            call textatxy(iix1,iiy1,LBLINT,'i',icol)
          elseif(IVLBL(ICN,I,3).eq.1)then
            call winscl('g',iicol)
            call textatxy(iix1,iiy1,LBLINT,'g',icol)
          elseif(IVLBL(ICN,I,3).eq.2)then
            call winscl('z',iicol)
            call textatxy(iix1,iiy1,LBLINT,'z',icol)
          endif
          iicol=0
          call winscl('-',iicol)
          if(iglib.eq.1)then
            IF(IFS.GT.0)CALL winfnt(IFS)
          elseif(iglib.eq.2)then
            CALL winfnt(IFS)
          endif
          call forceflush()
  12    CONTINUE
          
C Write on the label and recreate LABL after textpixwidth call.
        WRITE(LABL,'(A12)') NWICNM(ICN)

C Also check that the label is within the view limits, write
C it in black and use a smaller font if the scale is less than 1.
        CALL U2PIXEL(X,Y-OFFLAB,iixl,iiyl)
        if(iglib.eq.1)then
          if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(0)
        elseif(iglib.eq.2)then
          if(SCALF.lt.1.0)CALL winfnt(4)
        endif
        call textpixwidth(LABL,ipixw)
        iixlc = iixl - (ipixw/2)
        WRITE(LABL,'(A12)') NWICNM(ICN)
        if(iglib.eq.1)then
          if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(IFS)
        elseif(iglib.eq.2)then
          if(SCALF.lt.1.0)CALL winfnt(4)
        endif
        IF(IIXLC.GT.IGL.AND.IIXR.LT.IGR.AND.IIYU.GT.IGT.AND.
     &     IIYL.LT.IGB)THEN
          isize=0
          if(iglib.eq.1)then
            if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(0)
          elseif(iglib.eq.2)then
            IF(IFS.ne.4)CALL winfnt(4)
          endif
          line=1; icx=1  ! in case GTK set font context
          call findviewtext(icx,line,isize,ix,iy)
          call textatxy(iixlc,iiyl,LABL,'i',6)
          iicol=0
          call winscl('-',iicol)
          if(iglib.eq.1)then
            if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(IFS)
          elseif(iglib.eq.2)then
            if(SCALF.lt.1.0)CALL winfnt(4)
          endif
        ENDIF
      ELSE
        INBOUND=.FALSE.
      ENDIF

C Draw on the connection points for the component (applies to all components)
      IF(INBOUND)THEN
        DO 100 I=1,NCONP(ICN)

C Move to first position.
C Set the drawing colour depending on the attachment type.
          iicol=ICNCT(ICN,I)
          call winscl('z',iicol)

          if(INWKTYP.eq.6)then

C If domain is primitive part draw a circle and setup ICPP array.
            CALL U2PIXEL(CONCP(ICN,I,1),CONCP(ICN,I,2),IIX,IIY)
            CALL ECIRC(IIX,IIY,2,1)
            IF(ICN.EQ.1)THEN
              KKK=I
            ELSE
              KKK=0
              DO 119,JJJ=1,ICN-1
                KKK=KKK+NCONP(JJJ)
  119         CONTINUE
              KKK=KKK+I
            ENDIF
C            ICPP(ICN,I,1)=CPP(1,KKK)
C            ICPP(ICN,I,2)=CPP(2,KKK)
C            ICPP(ICN,I,3)=CPP(3,KKK)

C Debug.     WRITE(6,*),CPP(1,III),III

          else

C Otherwise draw a square. << patch to make square invisable >>
C            iupdown=0
C            isym=0
C            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
C     &                CONCP(ICN,I,2)-XYSIZCP/2.,iupdown,isym)
C            iupdown=1
C            CALL ETPLOT(CONCP(ICN,I,1)+XYSIZCP/2.,
C     &                CONCP(ICN,I,2)-XYSIZCP/2.,iupdown,isym)
C            CALL ETPLOT(CONCP(ICN,I,1)+XYSIZCP/2.,
C     &                CONCP(ICN,I,2)+XYSIZCP/2.,iupdown,isym)
C            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
C     &                CONCP(ICN,I,2)+XYSIZCP/2.,iupdown,isym)
C            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
C     &                CONCP(ICN,I,2)+XYSIZCP/2.,iupdown,isym)
C            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
C     &                CONCP(ICN,I,2)-XYSIZCP/2.,iupdown,isym)
          endif

C Reset the line colour to standard.
          iicol=0
          call winscl('-',iicol)
          call forceflush()
100     CONTINUE
      ENDIF

      RETURN
      END

C ****************************** NETWCDW **********************************
C This routine handles the drawing of connections and their intermediate
C points on the display
      SUBROUTINE NETWCDW(ICON)

# include "gnetwk.h"
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C Icon commons are in gnetwk.h
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH

      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C connection points property array
C      COMMON/CNP/CPP(3,MNCNN),ICPP(MICN,4,3),III

C connection points parameter array
C      COMMON/CPPA/ENV(5),PARA(MNCNN,13),CPARA(MICN,4,13),
C     &  SUB_MATRIX_A(MICN,MNCNN,MNCNN),SUB_MATRIX_B(MICN,MNCNN),
C     &  MATRIX_A(MNCNN,MNCNN),MATRIX_B(MNCNN),ICOUNTER,IDONE

      DIMENSION AL(2),AU(2),EP(3),SP(3),MP(3),PP(2),CP(2)
      REAL AL,AU,EP,SP,MP,PP,MIN1,MAX1,MIN2,MAX2

C      CHARACTER CPP*30,ICPP*30
C      REAL ENV,PARA,CPARA,SUB_MATRIX_A,SUB_MATRIX_B
C      REAL MATRIX_A,MATRIX_B

C Logical
      LOGICAL CLOSE,CLOSES,CLOSEE,CLOSEP,FOUND,ISEL,CSEL
      integer icontinue     ! to control while loops
      integer iloop         ! for loops

      REAL PI
      real xya,xyb  ! for u2pixel calls
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

#ifdef OSI
      integer iicol,iixp,iiyp,iix,iiy
      integer iisx1,iisy1,iisx2,iisy2,iisx3,iisy3,iisx4,iisy4
      integer iiax,iiay,iiux,iiuy
#else
      integer*8 iicol,iixp,iiyp,iix,iiy
      integer*8 iisx1,iisy1,iisx2,iisy2,iisx3,iisy3,iisx4,iisy4
      integer*8 iiax,iiay,iiux,iiuy
#endif
C Draw the connection, with a square at each intermediate point.

C Move to the start of the connection 
      CLOSES=.FALSE.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.2)then
        continue
      else
        call usrmsg('Current version does not support mouse tracking.',
     &    'Sorry about that...','W')
        return
      endif

C Check if the view 'layer' is correct for the drawing of the point.
      MIN1=0.0; MAX1=0.0
      MIN2=0.0; MAX2=0.0

      IF(IVIEW.EQ.1)THEN
        SP(1)=CONCP(ICNS(ICON),ICNSP(ICON),1)
        SP(2)=CONCP(ICNS(ICON),ICNSP(ICON),2)
        SP(3)=XYZICON(ICNS(ICON),3)

C << do not check for Z so all are seen >>
C        CALL ECLOSE(SP(3),VIEWCEN(3),0.01,CLOSES)
        CLOSES=.true.
        MIN1=VIEWLIM(1); MAX1=VIEWLIM(2)
        MIN2=VIEWLIM(3); MAX2=VIEWLIM(4)
      ELSEIF(IVIEW.EQ.2)THEN
        SP(1)=CONCP(ICNS(ICON),ICNSP(ICON),1)
        SP(2)=XYZICON(ICNS(ICON),2)
        SP(3)=CONCP(ICNS(ICON),ICNSP(ICON),2)
        CALL ECLOSE(SP(3),VIEWCEN(2),0.01,CLOSES)
        MIN1=VIEWLIM(1); MAX1=VIEWLIM(2)
        MIN2=VIEWLIM(5); MAX2=VIEWLIM(6)
      ELSE
        SP(1)=XYZICON(ICNS(ICON),2)
        SP(2)=CONCP(ICNS(ICON),ICNSP(ICON),1)
        SP(3)=CONCP(ICNS(ICON),ICNSP(ICON),2)
        CALL ECLOSE(SP(3),VIEWCEN(1),0.01,CLOSES)
        MIN1=VIEWLIM(3); MAX1=VIEWLIM(4)
        MIN2=VIEWLIM(5); MAX2=VIEWLIM(6)
      ENDIF
      CALL U2PIXEL(SP(1),SP(2),IIX,IIY)

C If the layer is correct then check if the connection is within screen limits.
      IF(CLOSES)THEN
        IF(IIX.GE.IGL.AND.IIX.LE.IGR.AND.IIY.GE.IGT.AND.IIY.LE.IGB)THEN
          CLOSES=.TRUE.
        ELSE
          CLOSES=.FALSE.           
        ENDIF      
      ENDIF

C Move to the start of the connection.
C Set the drawing colour depending on the connection type.
      iicol=ICNNT(ICON)
      call winscl('z',iicol)

C Store the start point as the previous point and start drawing the
C connection. 
      PP(1)=SP(1); PP(2)=SP(2)
      call u2pixel(pp(1),pp(2),iixp,iiyp)

      IF(CLOSES)THEN
        CLOSEP=.TRUE. 
      ELSE
        CLOSEP=.FALSE.
      ENDIF
      
C Use whle loop to traverse nconwp list.  
      icontinue=1
      iloop=0
      do while (icontinue.ne.0)
        iloop=iloop+1  ! increment iloop until = NCONWP then jump out
        if(iloop.eq.NCONWP(ICON)) icontinue=0

C Debug  write(6,*) 'at ',iloop,icon,NCONWP(ICON)

C Get the pixels for the initial points
        CLOSE=.FALSE.
        IF(IVIEW.EQ.1)THEN
          MP(1)=CNWNP(ICON,iloop,1); MP(2)=CNWNP(ICON,iloop,2)

C << do not test for Z of waypoints so all are seen >>
C          CALL ECLOSE(CNWNP(ICON,iloop,3),VIEWCEN(3),0.01,CLOSE)
          CLOSE=.TRUE.
        ELSEIF(IVIEW.EQ.2)THEN
          MP(1)=CNWNP(ICON,iloop,1); MP(2)=CNWNP(ICON,iloop,3) 
          CALL ECLOSE(CNWNP(ICON,iloop,2),VIEWCEN(2),0.01,CLOSE)
        ELSE
          MP(1)=CNWNP(ICON,iloop,2); MP(2)=CNWNP(ICON,iloop,3) 
          CALL ECLOSE(CNWNP(ICON,iloop,1),VIEWCEN(1),0.01,CLOSE)
        ENDIF

C record the pixel values of the way point.
        call u2pixel(MP(1),MP(2),iix,iiy)

        IF(IIX.GE.IGL.AND.IIX.LE.IGR.AND.IIY.GE.IGT.AND.
     &     IIY.LE.IGB.AND.CLOSE.AND.CLOSEP)THEN

C If within the view limits then draw a line from the last point 
C to the current point.
           if(csel(icon))then
             call edwline(iixp,iiyp,iix,iiy)
           else
             call eswline(iixp,iiyp,iix,iiy)
           endif

C Get the pixel values of the square around the connection point.
           xya=MP(1)+XYSIZCP/2.; xyb=MP(2)-XYSIZCP/2.
           call u2pixel(xya,xyb,iisx1,iisy1)
           xya=MP(1)+XYSIZCP/2.; xyb=MP(2)+XYSIZCP/2.
           call u2pixel(xya,xyb,iisx2,iisy2)
           xya=MP(1)-XYSIZCP/2.; xyb=MP(2)+XYSIZCP/2.
           call u2pixel(xya,xyb,iisx3,iisy3)
           xya=MP(1)-XYSIZCP/2.; xyb=MP(2)-XYSIZCP/2.
           call u2pixel(xya,xyb,iisx4,iisy4)

C Draw a square around the connection intermediate point.         
           IF(CSEL(ICON))THEN
             CALL EDWLINE(iisx1,iisy1,iisx2,iisy2)
             CALL EDWLINE(iisx2,iisy2,iisx3,iisy3)
             CALL EDWLINE(iisx3,iisy3,iisx4,iisy4)
             CALL EDWLINE(iisx4,iisy4,iisx1,iisy1)
           ELSE
             CALL ESWLINE(iisx1,iisy1,iisx2,iisy2)
             CALL ESWLINE(iisx2,iisy2,iisx3,iisy3)
             CALL ESWLINE(iisx3,iisy3,iisx4,iisy4)
             CALL ESWLINE(iisx4,iisy4,iisx1,iisy1)
           ENDIF
           call forceflush()

          CLOSEP=.TRUE.

C Get the intersection between the line between the two points and the
C boundary of the domain. Move to the intersection point. 
        ELSEIF(IIX.GT.IGL.AND.IIX.LT.IGR.AND.IIY.GT.IGT.AND.
     &         IIY.LT.IGB.AND.CLOSE.AND..NOT.CLOSEP)THEN
          CLOSEP=.TRUE.

C If the start of the connection was outwith screen limits then move to the
C intersection and draw the connection.
          CP(1)=MP(1); CP(2)=MP(2)
          CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
C Debug   write(6,*) 'at a ',cp,pp,xi,yi,found
          call u2pixel(xi,yi,iiax,iiay)

C Draw a square around the connection intermediate point.
          if(found)then
            if(csel(icon))then 
              call edwline(iiax,iiay,iix,iiy)
            else
              call eswline(iiax,iiay,iix,iiy)
            endif
          else
            if(csel(icon))then
              call edwline(iixp,iiyp,iix,iiy)
            else
              call eswline(iixp,iiyp,iix,iiy)
            endif
          endif

C Get the pixel values of the square around the connection point. 
           xya=MP(1)+XYSIZCP/2.; xyb=MP(2)-XYSIZCP/2.
           call u2pixel(xya,xyb,iisx1,iisy1)
           xya=MP(1)+XYSIZCP/2.; xyb=MP(2)+XYSIZCP/2.
           call u2pixel(xya,xyb,iisx2,iisy2)
           xya=MP(1)-XYSIZCP/2.; xyb=MP(2)+XYSIZCP/2.
           call u2pixel(xya,xyb,iisx3,iisy3)
           xya=MP(1)-XYSIZCP/2.; xyb=MP(2)-XYSIZCP/2.
           call u2pixel(xya,xyb,iisx4,iisy4)

C Draw a square around the connection intermediate point.
           IF(CSEL(ICON))THEN
             CALL EDWLINE(iisx1,iisy1,iisx2,iisy2)
             CALL EDWLINE(iisx2,iisy2,iisx3,iisy3)
             CALL EDWLINE(iisx3,iisy3,iisx4,iisy4)
             CALL EDWLINE(iisx4,iisy4,iisx1,iisy1)
           ELSE
             CALL ESWLINE(iisx1,iisy1,iisx2,iisy2)
             CALL ESWLINE(iisx2,iisy2,iisx3,iisy3)
             CALL ESWLINE(iisx3,iisy3,iisx4,iisy4)
             CALL ESWLINE(iisx4,iisy4,iisx1,iisy1)
           ENDIF
           call forceflush()

           CLOSEP=.TRUE.
        ELSEIF(CLOSE.AND.CLOSEP)THEN

C If the end of the connection to the intermediate point is outwith the
C screen limits then draw a line to the intersection point from the
C previous point. 
C << Needs an else case for if found is false. >>
          CP(1)=MP(1); CP(2)=MP(2)
          CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)

C Debug   write(6,*) 'at b ',cp,pp,xi,yi,found
          call u2pixel(xi,yi,iiax,iiay)
          if(found)then
            if(csel(icon))then 
              call edwline(iiax,iiay,iixp,iiyp)
            else
              call eswline(iiax,iiay,iixp,iiyp)
            endif  
            call forceflush()
          else
            if(csel(icon))then
              call edwline(iixp,iiyp,iix,iiy)
            else
              call eswline(iixp,iiyp,iix,iiy)
            endif
          endif        
          CLOSEP=.FALSE.
        ELSE
          CLOSEP=.FALSE.
        ENDIF

C Store this way point as the previous way point
        PP(1)=MP(1)
        PP(2)=MP(2)
C get the pixel coordinates of the previous point
        call u2pixel(pp(1),pp(2),iixp,iiyp) 

      enddo  ! of while loop
C  10  CONTINUE

C Finally draw the end point
      CLOSEE=.FALSE.

      IF(IVIEW.EQ.1)THEN
        EP(1)=CONCP(ICNE(ICON),ICNEP(ICON),1)
        EP(2)=CONCP(ICNE(ICON),ICNEP(ICON),2)
        EP(3)=XYZICON(ICNS(ICON),3)

C << do not check Z so all endpoints can be seen >>
C        CALL ECLOSE(EP(3),VIEWCEN(3),0.01,CLOSEE)
        CLOSEE=.true.
      ELSEIF(IVIEW.EQ.2)THEN
        EP(1)=CONCP(ICNE(ICON),ICNEP(ICON),1)
        EP(2)=XYZICON(ICNS(ICON),2)
        EP(3)=CONCP(ICNE(ICON),ICNEP(ICON),2)
        CALL ECLOSE(EP(3),VIEWCEN(2),0.01,CLOSEE)
      ELSE
        EP(1)=XYZICON(ICNS(ICON),1)
        EP(2)=CONCP(ICNE(ICON),ICNEP(ICON),1) 
        EP(3)=CONCP(ICNE(ICON),ICNEP(ICON),2)
        CALL ECLOSE(EP(3),VIEWCEN(1),0.01,CLOSEE)
      ENDIF

      call u2pixel(EP(1),EP(2),iix,iiy)

      IF(IIX.GT.IGL.AND.IIX.LT.IGR.AND.IIY.GT.IGT.AND.
     &   IIY.LT.IGB.AND.CLOSEE.AND.CLOSEP)THEN
        if(csel(icon))then 
          call edwline(iix,iiy,iixp,iiyp)
        else
          call eswline(iix,iiy,iixp,iiyp)
        endif 
        call forceflush()
        
        IF(NCONWP(ICON).GT.0)THEN
          IF(IVIEW.EQ.1)THEN
            X1=MP(1); Y1=MP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=MP(1); Y1=MP(3)
          ELSE
            X1=MP(2); Y1=MP(3)
          ENDIF
        ELSE 
          IF(IVIEW.EQ.1)THEN
            X1=SP(1); Y1=SP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=SP(1); Y1=SP(3) 
          ELSE
            X1=SP(2); Y1=SP(3)
          ENDIF          
        ENDIF 

        IF(IVIEW.EQ.1)THEN
          X2=EP(1); Y2=EP(2) 
        ELSEIF(IVIEW.EQ.2)THEN
          X2=EP(1); Y2=EP(3) 
        ELSE
          X2=EP(2); Y2=EP(3) 
        ENDIF  

C Draw the arrow.
        PI=22./7.
        DX=ABS(X2-X1); DY=ABS(Y2-Y1)
        DH=(DX**2.+DY**2.)**0.5
        ANG=ASIN(DY/DH) 
        ALENG=0.2 
        PHI=PI/6.  
        IF(X2.GT.X1.AND.Y2.GT.Y1)THEN

C Up right.
          AU(1)=EP(1)-ALENG*COS(ANG-PHI)
          AU(2)=EP(2)-ALENG*SIN(ANG-PHI)
          AL(1)=EP(1)-ALENG*SIN(0.5*PI-ANG-PHI)
          AL(2)=EP(2)-ALENG*COS(0.5*PI-ANG-PHI)
        ELSEIF(X2.LE.X1.AND.Y2.GT.Y1)THEN

C Up left.
          AU(1)=EP(1)+ALENG*COS(ANG-PHI)
          AU(2)=EP(2)-ALENG*SIN(ANG-PHI)
          AL(1)=EP(1)+ALENG*SIN(0.5*PI-ANG-PHI)
          AL(2)=EP(2)-ALENG*COS(0.5*PI-ANG-PHI)
        ELSEIF(X2.LE.X1.AND.Y2.LE.Y1)THEN

C Lower left.
          AL(1)=EP(1)+ALENG*COS(ANG-PHI)
          AL(2)=EP(2)+ALENG*SIN(ANG-PHI)
          AU(1)=EP(1)+ALENG*SIN(0.5*PI-ANG-PHI)
          AU(2)=EP(2)+ALENG*COS(0.5*PI-ANG-PHI)
        ELSE

C Lower right.
          AL(1)=EP(1)-ALENG*COS(ANG-PHI)
          AL(2)=EP(2)+ALENG*SIN(ANG-PHI)
          AU(1)=EP(1)-ALENG*SIN(0.5*PI-ANG-PHI)
          AU(2)=EP(2)+ALENG*COS(0.5*PI-ANG-PHI)
        ENDIF

C Draw the arrow (try using bold so it shows up better).
        call u2pixel(AL(1),AL(2),iiax,iiay)
        call u2pixel(AU(1),AU(2),iiux,iiuy)
        if(csel(icon))then 
          call edwline(iiax,iiay,iix,iiy)
          call edwline(iiux,iiuy,iix,iiy)
          call edwline(iiux,iiuy,iiax,iiay)
        else
          call edwline(iiax,iiay,iix,iiy)
          call edwline(iiux,iiuy,iix,iiy)
          call edwline(iiux,iiuy,iiax,iiay)
C          call eswline(iiax,iiay,iix,iiy)
C          call eswline(iiux,iiuy,iix,iiy)
        endif
        call forceflush()

      ELSEIF(IIX.GT.IGL.AND.IIX.LT.IGR.AND.IIY.GT.IGT.AND.
     &       IIY.LT.IGB.AND.CLOSEE.AND..NOT.CLOSEP)THEN
     
C Within the normal axis bounds. Check if at boundary via cintsct
C otherwise draw from mid-point.
        CP(1)=EP(1)
        CP(2)=EP(2)
        CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
        if(found)then
          call u2pixel(xi,yi,iiax,iiay)
          if(csel(icon))then 
            call edwline(iiax,iiay,iix,iiy)
          else
            call eswline(iiax,iiay,iix,iiy)
          endif
        else
          if(csel(icon))then
            call edwline(iixp,iiyp,iix,iiy)
          else
            call eswline(iixp,iiyp,iix,iiy)
          endif
        endif

        IF(NCONWP(ICON).GT.0)THEN
          IF(IVIEW.EQ.1)THEN
            X1=MP(1); Y1=MP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=MP(1); Y1=MP(3)
          ELSE
            X1=MP(2); Y1=MP(3)
          ENDIF
        ELSE 
          IF(IVIEW.EQ.1)THEN
            X1=SP(1); Y1=SP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=SP(1); Y1=SP(3) 
          ELSE
            X1=SP(2); Y1=SP(3)
          ENDIF          
        ENDIF 

        IF(IVIEW.EQ.1)THEN
          X2=EP(1); Y2=EP(2) 
        ELSEIF(IVIEW.EQ.2)THEN
          X2=EP(1); Y2=EP(3) 
        ELSE
          X2=EP(2); Y2=EP(3) 
        ENDIF  

C Draw the arrow.
        PI=22./7.
        DX=ABS(X2-X1); DY=ABS(Y2-Y1)
        DH=(DX**2.+DY**2.)**0.5
        ANG=ASIN(DY/DH) 
        ALENG=0.2 
        PHI=PI/6.  
        IF(X2.GT.X1.AND.Y2.GT.Y1)THEN

C Up right.
          AU(1)=EP(1)-ALENG*COS(ANG-PHI)
          AU(2)=EP(2)-ALENG*SIN(ANG-PHI)
          AL(1)=EP(1)-ALENG*SIN(0.5*PI-ANG-PHI)
          AL(2)=EP(2)-ALENG*COS(0.5*PI-ANG-PHI)
        ELSEIF(X2.LE.X1.AND.Y2.GT.Y1)THEN
 
C Up left.
          AU(1)=EP(1)+ALENG*COS(ANG-PHI)
          AU(2)=EP(2)-ALENG*SIN(ANG-PHI)
          AL(1)=EP(1)+ALENG*SIN(0.5*PI-ANG-PHI)
          AL(2)=EP(2)-ALENG*COS(0.5*PI-ANG-PHI)
        ELSEIF(X2.LE.X1.AND.Y2.LE.Y1)THEN

C Lower left.
          AL(1)=EP(1)+ALENG*COS(ANG-PHI)
          AL(2)=EP(2)+ALENG*SIN(ANG-PHI)
          AU(1)=EP(1)+ALENG*SIN(0.5*PI-ANG-PHI)
          AU(2)=EP(2)+ALENG*COS(0.5*PI-ANG-PHI)
        ELSE
C Lower right.
          AL(1)=EP(1)-ALENG*COS(ANG-PHI)
          AL(2)=EP(2)+ALENG*SIN(ANG-PHI)
          AU(1)=EP(1)-ALENG*SIN(0.5*PI-ANG-PHI)
          AU(2)=EP(2)+ALENG*COS(0.5*PI-ANG-PHI)
        ENDIF

C Draw the arrow (try using bold so it shows up better).
        call u2pixel(AL(1),AL(2),iiax,iiay)
        call u2pixel(AU(1),AU(2),iiux,iiuy)
        if(csel(icon))then 
          call edwline(iiax,iiay,iix,iiy)
          call edwline(iiux,iiuy,iix,iiy)
          call edwline(iiux,iiuy,iiax,iiay)
        else
C          call eswline(iiax,iiay,iix,iiy)
C          call eswline(iiux,iiuy,iix,iiy)
          call edwline(iiax,iiay,iix,iiy)
          call edwline(iiux,iiuy,iix,iiy)
          call edwline(iiux,iiuy,iiax,iiay)
        endif
        call forceflush()
             
      ELSEIF(CLOSEE.AND.CLOSEP)THEN
        CP(1)=EP(1)
        CP(2)=EP(2)
        CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
        if(found)then
          call u2pixel(xi,yi,iiax,iiay)
          if(csel(icon))then 
            call edwline(iix,iiy,iixp,iiyp)
          else
            call eswline(iix,iiy,iixp,iiyp)
          endif
        else
          if(csel(icon))then
            call edwline(iixp,iiyp,iix,iiy)
          else
            call eswline(iixp,iiyp,iix,iiy)
          endif
        endif
        call forceflush()
      ELSE
        continue
      ENDIF
      
C Reset the line colour to standard.
      iicol=0
      call winscl('-',iicol)
      RETURN
      END

C ************* imgdisp *************
C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end

C Dummy subroutine needed to compile.
      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1

      return
      end

C ********************* NETGDATR ************
C Each domain may contain global attributes such as the wind
C reduction factor of a flow network which relate to the network
C as a whole rather than to individual components. If there
C are such attributes a `global attributes` option is included
C in the main menu which calls this subroutine.

C Figure out how to manage these common blocks for inclusion
C in text-based flow network file. Set a logic variable if
C any of the simulation parameters have been touched.

      SUBROUTINE NETGDATR(IER)

# include "epara.h"
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf

C Global attributes commons (associated with igatrdom,dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)
      CHARACTER ddgtagatr*12,ddgmenuatr*32,ddgatrib*12

C Simulation preferences (from bps simcon.F)
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

C CATSI: list of attribute categories (presented for selection).
C NCATIE: number of data fields in each attribute category.
C ICATIE: pointer from DATACATIE bookkeepping back to the NWKCON arrays.
      DIMENSION CATSI(MIATRC),NCATIE(MIATRC),ICATIE(MIATRC,MIATRB)

C DATCATIE: attribute editing array for the current category. 
C TAGCATIE: ajucent for DATCATIE of the global attribute tags. 
C IECDATE is menu array for attribute categories.
C IECDATEs is menu arrary for attribute data fields.
C CATIES is menu entry for each data field
      DIMENSION DATCATIE(MIATRC,MIATRB,3),IECDATE(MIATRC+6),
     &CATIES(MIATRC,MIATRB),IECDATEs(30),TAGCATIE(MIATRC,MIATRB,5)

      LOGICAL MATCH

      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,key*1
      CHARACTER CATSI*12,IECDATE*34,CATIES*32,
     &IECDATEs*49,DATCATIE*12,TAGCATIE*12,t32*32
      integer NECDATA,ICOUT1,NECDATAs,ICOUT2 ! max items and current menu item

#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='NETGDATR'  ! set for subroutine

C Clear the menu structures.
      NCATI=0
      DO 507 JJ=1,MIATRC
        NCATIE(JJ)=0
        CATSI(JJ)=' '
        DO 508 K=1,MIATRB
          ICATIE(JJ,K)=0
          DO 509 L=1,5
            if(L.le.3)DATCATIE(JJ,K,L)='  '
            TAGCATIE(JJ,K,L)='  '
  509     CONTINUE
  508   CONTINUE
  507 CONTINUE

C Build the menu commands. First present a list of global attribute
C categories.  If a flow network then there should always be at least
C one global attribute for wind pressure reductions.
      if(idgatrdom.eq.0) goto 30
      do 43 i=1,idgatrdom
        MATCH=.FALSE.
        IF(I.NE.1)THEN
          DO 525 JJ=1,NCATI
            IF(CATSI(JJ).EQ.ddgtagatr(I,1))THEN
              MATCH=.TRUE.

C Put the data into an existing category and 
C Increment the number of entries for this category
              NCATIE(JJ)=NCATIE(JJ)+1
              CATIES(NCATI,NCATIE(JJ))=ddgmenuatr(I)

C Set the index for this category entry: relates category data -> global data
              ICATIE(JJ,NCATIE(JJ))=I
              DO 523 L=1,5
                if(L.le.3)DATCATIE(JJ,NCATIE(JJ),L)=ddgatrib(I,L)
                TAGCATIE(JJ,NCATIE(JJ),L)=ddgtagatr(I,L)
  523         CONTINUE
            ENDIF
  525     CONTINUE
        ENDIF

C Put the data into a new category
        IF(.NOT.MATCH)THEN
          NCATI=NCATI+1
          CATSI(NCATI)=ddgtagatr(I,1)
          NCATIE(NCATI)=NCATIE(NCATI)+1

C Set the index for this category entry: relates category data -> global data
          ICATIE(NCATI,NCATIE(NCATI))=I
          CATIES(NCATI,NCATIE(NCATI))=ddgmenuatr(I)
          DO 575 L=1,5
            if(L.le.3)DATCATIE(NCATI,NCATIE(NCATI),L)=ddgatrib(I,L)
            TAGCATIE(NCATI,NCATIE(NCATI),L)=ddgtagatr(I,L)
  575     CONTINUE           
        ENDIF
   43 CONTINUE

C Display the category and data menus
  705 NECDATA=0
      MHEAD=1
      MCTL=3
      ILEN=NCATI
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NCATI
      ICOUT1=-3
      WRITE(IECDATE(1),'(A)') ' global attribute categories:'          
      M=MHEAD
      DO 707 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(IECDATE(M),'(A,1x,A)') Key,CATSI(L)
        endif
  707 CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        IECDATE(M+1)='  ______________________________ '
      ELSE
        WRITE(IECDATE(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      WRITE(IECDATE(M+2),'(A)') '? help '
      WRITE(IECDATE(M+3),'(A)') '- exit this menu '
      NECDATA=M+MCTL  
      CALL NETWDRW
      helptopic='net_global_data'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EMENU('Global attribute categories',IECDATE,
     &  NECDATA,ICOUT1)
      IF(ICOUT1.EQ.NECDATA)THEN
        RETURN
      ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
        helptopic='net_global_data'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Data categories',nbhelp,'-',0,0,IER)
      ELSEIF(ICOUT1.EQ.NECDATA-2)THEN   

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(ICOUT1.GT.MHEAD.AND.
     &       ICOUT1.LT.(NECDATA-MCTL+1))THEN

C Record the selected data entry and set up sub-menu with data
C Note use EPMENSV to remember the category menu state.
        CALL KEYIND(NECDATA,ICOUT1,IAST,IO)
        CALL EPMENSV

  718   MHEAD=2
        MCTL=4
        ICOUT2=-1
        ILEN=NCATIE(IAST)
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Initial menu entry setup.
  792   IER=0
        ILEN=NCATIE(IAST)
        ICOUT2=-3

        WRITE(IECDATEs(1),'(A,A)')'  category: ',CATSI(IAST) 
        WRITE(IECDATEs(2),'(A)')'  _______________________________' 
        M=MHEAD

C Check for longest length to display. iwla is for caties,
C iwlb is for datcatie, iwa is both plus necessary spaces.
        iwla=0
        iwlb=0
        do 817 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            la=lnblnk(CATIES(IAST,L))
            if(la.gt.iwla) iwla = la
            lb=lnblnk(DATCATIE(IAST,L,1))
            if(lb.gt.iwlb) iwlb = lb
          endif
  817   continue
        iwla=MIN0(32,iwla)
        iwa=(iwla+iwlb+7)
        iw=MAX0(35,iwa)
        DO 717 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(L,KEY,IER)

C Depending on whether the data is static or user editable.
            IF(TAGCATIE(IAST,L,5)(1:4).eq.'user')THEN
              WRITE(IECDATEs(M),'(A,1x,3A)') Key,
     &         CATIES(IAST,L)(1:iwla),' ',
     &         DATCATIE(IAST,L,1)(1:iwlb)
            else
              WRITE(IECDATEs(M),'(2x,4A)') 
     &          CATIES(IAST,L)(1:iwla),' ',
     &          DATCATIE(IAST,L,1)(1:iwlb),' #'
            endif
          endif
  717   CONTINUE

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          IECDATEs(M+1)='  _______________________________'
        ELSE
          WRITE(IECDATEs(M+1),15)IPM,MPM 
        ENDIF
        WRITE(IECDATEs(M+2),'(A)')'  cannot edit values marked (#)'
        WRITE(IECDATEs(M+3),'(A)')'? help '
        WRITE(IECDATEs(M+4),'(A)')'- exit this menu '
        NECDATAs=M+MCTL
        ICOUT2=-1
        CALL NETWDRW

C Make menu only as wide as it needs to be.
        if(MMOD.eq.8)then
          impx=0
          impy=0
          CALL VWMENU('Global attributes',IECDATEs,NECDATAs,
     &      impx,impy,iw,irpx,irpy,ICOUT2)
        else
          CALL EMENU('Global attributes',IECDATEs,NECDATAs,
     &      ICOUT2)
        endif

        IF(ICOUT2.EQ.NECDATAs)THEN

C Restore category menu setup before returning to label 705.
          CALL EPMENRC
          GOTO 705

        ELSEIF(ICOUT2.EQ.NECDATAs-1)THEN
          helptopic='net_category_attrib'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Data values',nbhelp,'-',0,0,IER)

        ELSEIF(ICOUT2.EQ.NECDATAs-3)THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF

        ELSEIF(ICOUT2.GT.MHEAD.AND.
     &         ICOUT2.LT.(NECDATAs-MCTL+1))THEN
          CALL KEYIND(NECDATAs,ICOUT2,ISL,IO)
          IF(TAGCATIE(IAST,ISL,2)(1:4).eq.'intg')THEN

C First read DATCATIE strings into IVMIN/IVMAX/IVAL variables, then edit
C the variable and then write it back to the string ISTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
            call catie_int(datcatie,iast,isl,ivmin,ivmax,ival,ivald,ier)
            IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
              t32=CATIES(IAST,ISL)
              CALL EASKI(IVAL,t32,' ',IVMIN,'W',
     &          IVMAX,'W',IVALD,'glob atrib intg data',IER,nbhelp)
              if(ier.eq.0)then
                write(istr,'(I8)') IVAL
                call removepad(istr,inpstr,ilena,iflag)
                write(DATCATIE(IAST,ISL,1),'(A)') inpstr
                write(ddgatrib(ICATIE(IAST,ISL),1),'(A)') inpstr

C If flow domain update simulation preferences depending on which
C global attribute is currently active.
                if(INWKTYP.eq.2)then
C Debug           write(6,*)'iast,isl,inpstr,ival',iast,isl,inpstr,ival
                  if(ISL.eq.2)then
                    IPSMOD=IVAL ! stack pressure
                  elseif(ISL.eq.3)then
                    MSLVTP=IVAL ! solution
                  elseif(ISL.eq.4)then
                    MFTRAC=IVAL ! trace level
                  elseif(ISL.eq.5)then
                    MFBSNC=IVAL  ! timing
                  endif
               endif
             endif
            ENDIF
          ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'real')THEN

C First read DATCATIE strings into VMIN/VMAX/VAL real variables, then edit
C the variable and then write it back to the string VSTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
            call catie_r(datcatie,iast,isl,vmin,vmax,val,vald,ier)

            IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
              t32=CATIES(IAST,ISL)
              CALL EASKR(VAL,t32,' ',VMIN,'W',
     &          VMAX,'W',VALD,'atrib real data',IER,nbhelp)
              if(ier.eq.0)then
                call relstr(val,vnpstr,ilena,iflag)
                WRITE(DATCATIE(IAST,ISL,1),'(a)') vnpstr
                WRITE(ddgatrib(ICATIE(IAST,ISL),1),'(a)') vnpstr
              endif
            ENDIF
          ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'text')THEN
            VSTR=DATCATIE(IAST,ISL,1)
            IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
              t32=CATIES(IAST,ISL)
              CALL EASKS(VSTR,t32,' ',12,'nothing',
     &          'atrib text data',IER,nbhelp)
              if(ier.eq.0.and.VSTR(1:2).ne.'  ')then
                call removepad(vstr,vnpstr,ilena,iflag)
                DATCATIE(IAST,ISL,1)=VNPSTR
                ddgatrib(ICATIE(IAST,ISL),1)=VNPSTR
              endif
            ENDIF
          ENDIF
        ELSE
          GOTO 792
        ENDIF
        GOTO 718
      ELSE

C Restore category menu setup before returning to label 705.
        CALL EPMENRC
        GOTO 705
      ENDIF
      GOTO 92
  30  continue

      RETURN
      END

C ********** selrelexttag
C Select a real value based on external information held in 
C a .summary file for the current model. Up to 6 real values
C are returned. If ier=-1 then summary file was not available.
      subroutine selrelexttag(icn,exttag,AVAL,SVAL,ier)
#include "gnetwk.h"
#include "net_flow.h"
#include "building.h"
C #include "geometry.h"
#include "epara.h"
      
      integer lnblnk  ! function definition
      integer icn     ! index of current icon

      COMMON/OUTIN/IUOUT,IUIN
      common/MFLDOC/DEPRE(MPRD)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)

C Summary of external data.
      common/exsum/isexavail,iuex

      dimension AVAL(6),SVAL(6),zx(MCOM),zy(MCOM),zz(MCOM),zn(MCOM)
      dimension zv(MCOM)

C snacon is the surface area associated with a connection,
C surazi is the azimuth associated with a connection,
C surelv is the elevation associated with a connection,
C surzmin is the lowest Z associated with a connection,
C surzcog is the Z cog associated with a connection,
C surzmax is the highest Z associated with a connection
C snamecon is the surface name associated with inside face of a connection
C znamecon is the zone name associated with inside face a conneciton
      dimension snacon(MCON),surazi(MCON),surelv(MCON),surzmin(MCON)
      dimension surzcog(MCON),surzmax(MCON),conname(MCON)
      dimension snamecon(MCON),znamecon(MCON)
      CHARACTER outstr*124,DEPRE*40,t72*72
      CHARACTER PCITM(35)*42,KEY*1,exttag*12,WORD*20,SVAL*12,conname*43
      CHARACTER PSITM(35)*60
      character snamecon*12,znamecon*12,outs*124,tl*18,tc*18,tt*18
      character zn*12,hold*32
      LOGICAL OK,DOK,isexavail
      integer IRT  ! for radio button
      integer NITMS,IVERT,IDUM ! max items and current menu item and placeholder
      real xa,xh  ! for editing attributes

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='selrelexttag'  ! set for subroutine

C Rewind summary file if it exists (assumed to be open on unit iuex).
      if(isexavail)then
        ier=0
        REWIND(iuex,ERR=999)
        CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
      else
        ier=-1
        return
      endif

C Clear return array. This implies that the current values are
C not known within this subroutine. It may be useful for calling
C application to report this prior to user selection.
      AVAL(1)=0.00; AVAL(2)=0.00; AVAL(3)=0.00; AVAL(4)=0.00; 
      AVAL(5)=0.00; AVAL(6)=0.00
      SVAL(1)='  '; SVAL(2)='  '; SVAL(3)='  '; SVAL(4)='  '
      SVAL(5)='  '; SVAL(6)='  '
      
C Depending on what exttag is present different dialog and return
C one or more real values
      if(exttag(1:10).eq.'*Pressures')then

C Setup multi-page menu.
  42    CALL STRIPC(iuex,OUTSTR,99,ND,1,'tag & data',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','summary tags',IFLAG)
        IF(IFLAG.NE.0)goto 1
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          goto 42
        elseif(WORD(1:4).eq.'*cfg')then
          goto 42
        elseif(WORD(1:4).eq.'*afn')then
          goto 42
        elseif(WORD(1:10).eq.'*Pressures')then
          inpre=0
          CALL EGETWI(OUTSTR,K,NPRE,1,40,'W','nb of pressures',IER)
  43      CALL STRIPC(iuex,OUTSTR,99,ND,1,'pressure stuff',IER)
          IF(IER.NE.0)goto 1
          if(outstr(1:14).eq.'*End_Pressures')then
            continue
          else
            inpre=inpre+1
            K=0
            CALL EGETRM(OUTSTR,K,DEPRE(inpre),'W','pres phrase',IER)
            IF(IER.NE.0)goto 1
            goto 43
          endif

C Generate the menu.
          MHEAD=2
          MCTL=4
          ILEN=NPRE
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Initial menu entry setup.
 70       IER=0
          ILEN=NPRE
          IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
          PCITM(1)=' W:H  |  Exposure  |  Pitch (roof)'
          PCITM(2)='  (W:H = width to height ratio)   '
          M=MHEAD
          DO 201 IM=1,NPRE
            IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
              M=M+1
              CALL EMKEY(M,KEY,IER)   
              WRITE(PCITM(M),'(A1,1X,A)')KEY,DEPRE(IM)
            ENDIF
  201     CONTINUE

C Number of actual items displayed.
          NITMS=M+MCTL

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN
            PCITM(M+1)='  ________________________  '
          ELSE
            WRITE(PCITM(M+1),15)IPM,MPM 
 15       FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
          ENDIF
          PCITM(M+2)=' '
          PCITM(M+3)='? help                    '
          PCITM(M+4)='- exit (without selecting)'
          IVERT=-1

C Now display the menu.
          helptopic='net_pressure_coef'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call USRMSG('Select the pressure coefficient set which',
     &'best matches this surface. (see help) ','-') 
          CALL EMENU('Pressure Coefficient Sets:',PCITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN

C User exits without making a selection.
            ier= -1
          ELSEIF(IVERT.EQ.NITMS-1)THEN
            helptopic='net_pressure_coef'
            call gethelptext(helpinsub,helptopic,nbhelp)
           CALL PHELPD('databases overview',nbhelp,'-',0,0,IER)
            GOTO 70
          ELSEIF(IVERT.EQ.NITMS-3)THEN

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
            GOTO 70
          ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Let user select a pc set.
            CALL KEYIND(NITMS,IVERT,IFOC,IO)
          ELSE
            IVERT=-1
            GOTO 70
          ENDIF

C Ask whether choice is OK.
          write(outs,'(3a)')'Using ',DEPRE(IFOC),
     &      ' as the pressure coefficient set?'
          call edisp(iuout,outs)
          AVAL(1)=real(IFOC)
          return
        endif
      elseif(exttag(1:2).eq.'- ')then
        return
      elseif(exttag(1:6).eq.'*Zones')then

C Setup multi-page menu.
 142    CALL STRIPC(iuex,OUTSTR,99,ND,1,'tag & data',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','summary tags',IFLAG)
        IF(IFLAG.NE.0)goto 1
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          goto 142
        elseif(WORD(1:4).eq.'*cfg')then
          goto 142
        elseif(WORD(1:4).eq.'*afn')then
          goto 142
        elseif(WORD(1:10).eq.'*Pressures')then
          goto 142
        elseif(WORD(1:6).eq.'*Zones')then
          inz=0
          CALL EGETWI(OUTSTR,K,NZ,1,MCOM,'W','nb of zones',IER)
 143      CALL STRIPC(iuex,OUTSTR,99,ND,1,'zone stuff',IER)
          IF(IER.NE.0)goto 1
          if(outstr(1:10).eq.'*End_Zones')then
            continue
          else
            inz=inz+1
            K=0
            CALL EGETW(OUTSTR,K,zn(inz),'W','zone name',IFLAG)
            CALL EGETWR(OUTSTR,K,zv(inz),0.,0.,'-','zone vol',IER)
            CALL EGETWR(OUTSTR,K,zx(inz),0.,0.,'-','zone cog X',IER)
            CALL EGETWR(OUTSTR,K,zy(inz),0.,0.,'-','zone cog Y',IER)
            CALL EGETWR(OUTSTR,K,zz(inz),0.,0.,'-','zone cog Z',IER)
            IF(IER.NE.0)goto 1
            goto 143
          endif

C Generate the menu.
          MHEAD=2
          MCTL=4
          ILEN=nz
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Initial menu entry setup.
 71       IER=0
          ILEN=NZ
          IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
          PCITM(1)=' Zone name   | volume  | height'
          PCITM(2)='             |  (m^3)  |       '
          M=MHEAD
          DO 202 IM=1,NZ
            IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
              M=M+1
              CALL EMKEY(M,KEY,IER)   
              WRITE(PCITM(M),'(A1,1X,A,2F8.2)')KEY,zn(IM),zv(im),
     &          zz(im)
            ENDIF
  202     CONTINUE

C Number of actual items displayed.
          NITMS=M+MCTL

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN
            PCITM(M+1)='  ________________________  '
          ELSE
            WRITE(PCITM(M+1),15)IPM,MPM 
          ENDIF
          PCITM(M+2)=' '
          PCITM(M+3)='? help                    '
          PCITM(M+4)='- exit (without selecting manual edit)'
          IVERT=-1

C Now display the menu.
          helptopic='external_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call USRMSG('Select a zone from the model and its volume',
     &      'and height can be used. (see help)','-') 
          CALL EMENU('Available zones in model:',PCITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN

C User exits without making a selection. The user can type in
C a volume & location X Y Z based on current icon location.
            ier= -1
            helptopic='external_overview'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('external overview',nbhelp,'-',0,0,IER)
            AVAL(1)=1.0  ! initial guess
            write(hold,'(4f8.2)') AVAL(1),XYZICON(ICN,1),
     &        XYZICON(ICN,2),XYZICON(ICN,3)
            CALL EASKS(HOLD,'Volume & position @ node (X Y Z m):',
     &        ' ',36,' 1. 1. 1. 1.','node position XYZ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,VL,0.0,99.,'W','Vol',IER)
            CALL EGETWR(HOLD,K,CGX,-99.,99.,'W','Xm',IER)
            CALL EGETWR(HOLD,K,CGY,-99.,99.,'W','Ym',IER)
            CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Zm',IER)
            if(ier.eq.0)then
              AVAL(1)=VL; AVAL(2)=CGX; AVAL(3)=CGY; AVAL(4)=VALZ
              SVAL(1)='nozone'  ! signal not associated
              return
            endif

          ELSEIF(IVERT.EQ.NITMS-3)THEN

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
            GOTO 71
          ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Let user select a pc set.
            CALL KEYIND(NITMS,IVERT,IFOC,IO)
          ELSE
            IVERT=-1
            GOTO 71
          ENDIF

C Ask whether choice is OK.
          dok=.true.
          helptopic='external_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(t72,'(a,a)')'Use ',zn(ifoc)
          CALL ASKOK(t72,'attributes with this component?',
     &      OK,dok,nbhelp)
          IF(.NOT.OK)GOTO 71
          AVAL(1)=zv(ifoc); AVAL(2)=zx(ifoc); AVAL(3)=zy(ifoc)
          AVAL(4)=zz(ifoc); SVAL(1)=zn(ifoc)
          return
        else

C Word did not match, read another line.
          goto 142
        endif
      elseif(exttag(1:9).eq.'*Surfaces')then

C Setup multi-page menu.
 242    CALL STRIPC(iuex,OUTSTR,99,ND,1,'tag & data',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','summary tags',IFLAG)
        IF(IFLAG.NE.0)goto 1
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          goto 242
        elseif(WORD(1:4).eq.'*cfg')then
          goto 242
        elseif(WORD(1:10).eq.'*Pressures')then
          goto 242
        elseif(WORD(1:6).eq.'*Zones')then
          goto 242
        elseif(WORD(1:9).eq.'*Surfaces')then

C Work with data in *Surfaces portion of the summary file. The initial
C phrase might be `1 pt_general in manager >|< Identical,`
          inc=0
          CALL EGETWI(OUTSTR,K,NC,1,MCON,'W','nb of conn',IER)
 243      CALL STRIPC(iuex,OUTSTR,99,ND,1,'surface stuff',IER)
          if(outstr(1:13).eq.'*End_Surfaces')then
            continue
          else
            inc=inc+1
            K=0

C Debug     write(6,*) inc,' ',outstr(1:lnblnk(outstr))

C Decode tokens in the .summary file, discard initial index, use text description
C of the connection and then get the geometric attributes.
            CALL EGETWI(OUTSTR,K,IDUM,1,MCON,'W','current index',IER)
            CALL EGETP(OUTSTR,K,conname(inc),'W','conn descr',IFLAG)
            CALL EGETW(OUTSTR,K,znamecon(inc),'W','conn zn name',IFLAG)
            CALL EGETW(OUTSTR,K,snamecon(inc),'W','conn sf name',IFLAG)
            CALL EGETWR(OUTSTR,K,snacon(inc),0.,0.,'-','surf area',IER)
            CALL EGETWR(OUTSTR,K,surazi(inc),0.,0.,'-','surf azim',IER)
            CALL EGETWR(OUTSTR,K,surelv(inc),0.,0.,'-','surf elev',IER)
            CALL EGETWR(OUTSTR,K,surzmin(inc),0.,0.,'-','surf Zmin',IER)
            CALL EGETWR(OUTSTR,K,surzcog(inc),0.,0.,'-','surf Zcog',IER)
            CALL EGETWR(OUTSTR,K,surzmax(inc),0.,0.,'-','surf Zmax',IER)
C Debug     write(6,*) znamecon(inc),' ',snamecon(inc),' ',snacon(inc),
C     &                 ' ',surazi(inc)
            IF(IER.NE.0)goto 1
            goto 243
          endif

C Generate the menu.
          MHEAD=2
          MCTL=4
          ILEN=NC
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Initial menu entry setup.
 72       IER=0
          ILEN=NC
          IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
          PSITM(1)=
     &    ' Connection                   | area  | azim | elev | COG '
          PSITM(2)=
     &    '                              | (m^3) | (deg)| (deg)  Z(m)'
          M=MHEAD
          DO 302 IM=1,NC
            IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
              M=M+1
              CALL EMKEY(M,KEY,IER)   
              WRITE(PSITM(M),'(A1,1X,A,4F7.2)')KEY,conname(IM)(1:28),
     &          snacon(im),surazi(im),surelv(im),surzcog(im)
            ENDIF
  302     CONTINUE

C Number of actual items displayed.
          NITMS=M+MCTL

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN
            PSITM(M+1)='  ________________________  '
          ELSE
            WRITE(PSITM(M+1),15)IPM,MPM 
          ENDIF
          PSITM(M+2)=' '
          PSITM(M+3)='? help                    '
          PSITM(M+4)='- exit (without selecting)'
          IVERT=-1

C Now display the menu.
          helptopic='external_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call USRMSG('Select a surface from the model and its area',
     &'and orientation can be used. (see help) ','-') 
          CALL EMENU('Available surfaces in model:',PSITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN

C User exits without making a selection.
            ier= -1
          ELSEIF(IVERT.EQ.NITMS-1)THEN
            helptopic='external_overview'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('external overview',nbhelp,'-',0,0,IER)
            GOTO 72
          ELSEIF(IVERT.EQ.NITMS-3)THEN

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
            GOTO 72
          ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Let user select a pc set.
            CALL KEYIND(NITMS,IVERT,IFOC,IO)
          ELSE
            IVERT=-1
            GOTO 72
          ENDIF

C Ask whether choice is OK. If so return area, azim, elev, Z min
C Z cog, Z max, zone name and surface name.
          helptopic='place_at_lowest_point'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(tl,'(a,f7.3,a)') 'low (',surzmin(ifoc),'m)'
          write(tc,'(a,f7.3,a)') 'cog (',surzcog(ifoc),'m)'
          write(tt,'(a,f7.3,a)') 'top (',surzmax(ifoc),'m)'
          write(outs,'(3a)') 'Use ',
     &      conname(ifoc)(1:lnblnk(conname(ifoc))),' attributes with'
          IRT=1
          call easkatog(outs,'options:',tl,tc,tt,'other','continue',
     &      ' ',' ',IRT,nbhelp)
          IF(irt.eq.5)GOTO 72
          AVAL(1)=snacon(ifoc)

C Confirm azimuth from the .summary file.
          xa=surazi(ifoc)
          CALL EASKR(xa,'Component aimuth (deg):',' ',-360.0,'W',
     &      360.0,'W',0.0,'component azim',IER,nbhelp)
          if(ier.eq.0)AVAL(2)=xa
          AVAL(3)=surelv(ifoc)
          if(irt.eq.1)then
            AVAL(4)=surzmin(ifoc)
          elseif(irt.eq.2)then
            AVAL(4)=surzcog(ifoc)
          elseif(irt.eq.3)then
            AVAL(4)=surzmax(ifoc)
          elseif(irt.eq.4)then
            xh=surzcog(ifoc)
            CALL EASKR(xh,'Component height (m):',' ',-9.0,'W',99.0,'W',
     &        1.0,'component height',IER,nbhelp)
            if(ier.eq.0)AVAL(4)=xh
          endif
          SVAL(1)=znamecon(ifoc)
          SVAL(2)=snamecon(ifoc)
          return
        else

C Word did not match, read another line.
          goto 242
        endif
      endif

C Error handling
  1   CALL USRMSG('Problem with summary line:',OUTSTR,'W')
      call edisp(iuout,' Plese check the model summary file.')
      IER=1
      return

 999  call edisp(iuout,' Problem rewinding summary file.')
      IER=1
      return

      end


C ********** listexttag
C List a portion of external information held in a .summary
C file for the current model. File is assumed to be open on
C unit iuex.
      subroutine listexttag(exttag,ier)
#include "building.h"
CC#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN

C Summary of external data.
      common/exsum/isexavail,iuex

      dimension zx(MCOM),zy(MCOM),zz(MCOM),zn(MCOM),zv(MCOM)
      CHARACTER outstr*124
      CHARACTER exttag*12,WORD*20,outs*124,zn*12
      LOGICAL isexavail

C Rewind the .summary file and read in the first line.
      if(isexavail)then
        ier=0
        REWIND(iuex,ERR=999)
      else
        ier=-1
        return
      endif
      CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
      if(IER.NE.0)isexavail=.false.
      if(OUTSTR(1:9).ne.'*Synopsis')then
        call usrmsg('Attempted to scan non-synopsis file.',' ','W')
        isexavail=.false.
        return
      else

C Scan the model summary file and report on zone names, volumes and height.
        isexavail=.true.
  42    CALL STRIPC(iuex,OUTSTR,99,ND,1,'tag & data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','summary tags',IFLAG)
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          goto 42
        elseif(WORD(1:4).eq.'*cfg')then
          goto 42
        elseif(WORD(1:10).eq.'*Pressures')then
          CALL EGETWI(OUTSTR,K,NPRE,1,40,'W','nb of pressures',IER)
          write(outs,'(a,i3,a)') 'There are ',NPRE,' pressure sets.'
          call edisp(iuout,outs)
          goto 42
        elseif(WORD(1:6).eq.'*Zones')then
          inz=0
          call edisp(iuout,
     &      ' Model zone names, volume, height   centre')
  43      CALL STRIPC(iuex,OUTSTR,99,ND,1,'zone stuff',IER)
          if(outstr(1:10).eq.'*End_Zones')then
            continue
          else
            inz=inz+1
            K=0
            CALL EGETW(OUTSTR,K,zn(inz),'W','zone name',IFLAG)
            CALL EGETWR(OUTSTR,K,zv(inz),0.,0.,'-','zn vol',IER)
            CALL EGETWR(OUTSTR,K,zx(inz),0.,0.,'-','zn cog X',IER)
            CALL EGETWR(OUTSTR,K,zy(inz),0.,0.,'-','zn cog Y',IER)
            CALL EGETWR(OUTSTR,K,zz(inz),0.,0.,'-','zn cog Z',IER)
            write(outs,'(4X,A,4F8.2)') zn(inz),zv(inz),zz(inz),
     &        zx(inz),zy(inz)
            call edisp(iuout,outs)
            goto 43
          endif
        elseif(WORD(1:9).eq.'*Surfaces')then
          CALL EGETWI(OUTSTR,K,NC,1,MCON,'W','nb of conn',IER)
          write(outs,'(a,i3,a)') 'There are ',NC,' surfaces.'
          call edisp(iuout,outs)
          goto 42
        else
          goto 42
        endif
      endif
      return

 999  call edisp(iuout,' Problem rewinding summary file.')
      IER=1
      return
      end


C ********** catie_int
C catie_int decodes min max current & default integer values
C from datacatie array.
      subroutine catie_int(datcatie,iast,isl,ivmin,ivmax,ival,ivald,ier)
# include "gnetwk.h"
      COMMON/OUTIN/IUOUT,IUIN
      
C DATCATIE: attribute editing array for the current category to match
C the calling subroutine.
      DIMENSION DATCATIE(MIATRC,MIATRB,3)
      character DATCATIE*12
      integer iast,isl,ivmin,ivmax,ival,ivald,ier
      
      ier=0
      
      read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)IVMIN
      read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)IVMAX
      read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVAL
      read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVALD
C      write(6,*) 'catie_int ',ivmin,ivmax,ival,ivald
      RETURN
  99  CALL EDISP(IUOUT,'Error in string formatting catie_int')
      END


C ********** catie_r
C catie_r decodes min max current & default real values
C from datacatie array.
      subroutine catie_r(datcatie,iast,isl,vmin,vmax,val,vald,ier)
# include "gnetwk.h"
      COMMON/OUTIN/IUOUT,IUIN
      
C DATCATIE: attribute editing array for the current category to match
C the calling subroutine.
      DIMENSION DATCATIE(MIATRC,MIATRB,3)
      character DATCATIE*12
      integer iast,isl,ier
      real VMIN,VMAX,VAL,VALD
      
      ier=0
      
      read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)VMIN
      read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)VMAX
      read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VAL
      read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VALD
C      write(6,*) 'catie_r ',vmin,vmax,val,vald
      RETURN
  99  CALL EDISP(IUOUT,'Error in string formatting catie_r')
      END

C Dummy subroutines needed by libNonGTK:
C  ADJVIEW: Adjusts the viewing transforms & boundaries if necessary.
      SUBROUTINE ADJVIEW(IER)
      integer ier
      return
      end

C CHGAZI called from C code in esp-r.c or esru_x.c with an increment or 
C decrement to the viewing azimuth (icazi). This is a dummy. 
      subroutine chgazi(icazi)
      integer icazi
      return
      end

C CHGELEV is called from C code in esp-r.c or esru_x.c with an increment or 
C decrement to the viewing elevation (icelev). This is a dummy. 
      subroutine chgelev(icelev)
      integer icelev
      return
      end

C CHGSUN is called from C code in esru_x.ca and esp-r.c requesting solar view. 
      subroutine chgsun(isunhour)
      integer isunhour
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed paramters.
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

C EMKVIEW constructs a 'viewer' format file. This is a dummy subroutine.
      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      integer IUO,IER
      logical CFGOK
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
      integer ityp
      return
      end

C Other dummy subroutines needed for compile.
      SUBROUTINE EROPER(ITRC,ITRU,IUO,ICOMP,IER)
      integer itrc,itru,iuo,icomp,ier
      return
      end

      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)
      integer icomp,ier
      return
      end

C ***** Notice that an active description button has been pressed.
      subroutine cfgpk(act)
      character act*1
      return
      end
      
C dintervalf: A fortran implementation of the c surboutine dinterval.
C Copied from esrucom/esru_misc.F
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


