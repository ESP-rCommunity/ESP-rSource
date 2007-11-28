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
C   NETWCDW      - draws a connection on the screen

C Main variables used in Network:

C NWKNAM           	- network description (72 char)
C NWKFLNAM         	- network file name
C NWKTYPSTR  		- network type descriptor
C INWKTYP	        - network type 1 = 'HVAC', 2 = 'Flow', 3 = 'Electrical'
C                         4 ='Control', 5 = 'Hygroscopic', 6 = 'PrimitivePt'

C NICON            	- no. of icons 
 
C ICONCS            	- currently selected icon no
C ICONTP(MICN)     	- icon type (determines what is drawn in screen)
C XYZICON(MICN,3)	- icon x,y,z-position
C NICONATR(MICN)	- number of attributes associated with this icon
C ATRICN(MICN,MIATRB,3) - icon attributes (array of attribute strings)
C ATRTAG(MICN,MIATRB,1) - icon attribute tags (group identity)
C ATRTAG(MICN,MIATRB,2) - icon attribute tags (data type intg/real/text)
C ATRTAG(MICN,MIATRB,3) - icon attribute tags (`external` or `-`)
C ATRTAG(MICN,MIATRB,4) - icon attribute tags (external tag or `-`)
C ATRTAG(MICN,MIATRB,5) - icon attribute tags (static or user editable)
C ATRMENU(MICN,MIATRB)  - icon attribute menu entry
C NIVC(MICN)       	- number of icon vertex co-ordinates
C VCICON(MICN,MICNV,3) - icon vertex co-ordinates (used in drawing icon)
C NIVE(MICN)       	- number of icon vertex edges
C IVEICN(MICN,MICNE,5)	- icon vertex edges (used in drawing icon)
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
C IVLBL(MICN,MICND,3) - icon internal label structure where (n,n,1) is vertex index
C    (n,n,2) is colour (i.e. zone or grey colour index),
C    (n,n,3) colour scale (0=interface, 1=greyscale, 2=zone scale),
C NWICNM(MICN) - the icon name (from user),
C NWICNHIS(MICN) - icon origin tags (domain, category and name from icon db)
C CONCP(MICN,MCNP,2)	- x & y of attachment point connection <-> icon
C ICNCT(MICN,MCNP)	- attachment point type (to check against
C    ICNNT(MNCNN)); type 0=none, 1=air, 2=water, 3=steam, 
C    4=refrigerant, 5=fuel, 6=combustion product, 7=signal (0-10v)
C NICNN            	- connection number 
C ICNNCS           	- currently selected connection
C ICNS(MNCNN)      	- connection start component
C ICNE(MNCNN)      	- connection end component
C ICNNT(MNCNN)     	- connection type
C NCONP(MICN)		- number of attachment points for each icon
C NCONWP(MCNN)     	- number of connection way points

C The following relate to connection attributes of each domain (some
C have no attributes)
C idatrdom(MNCNN)       - number of connection attributes
C   dtagatr(MNWKTYP,MIATRB,5): array of tags (12 char) where the first is the
C     group (thermal/flow/control/output/location/environment/electrical)
C     the 2nd signals data type, the 3rd is `external` or `-`, the 4th
C     is an external key word or `-`, and the 5th is key word 'static' or 'user'.
C     Note `external` signals that an external domain summary file should 
C     be consulted for information associated with the external key word.
C ddatrib(MNCNN,MIATRB,3): array of strings holding attribute data
C     by type: for integers and real ,datrib(?,?,1) is a value
C     datrib(?,?,2) is minimum, atrib(?,?,3) is maximum, for text data 
C     datrib(?,?,1) is the string and datrib(?,?,2) & datrib(?,?,3) blank.
C ddmenuatr(MNCNN,MIATRB): array of menu entries (32 char)

C CNWNP(MNCNN,MCIP,3) 	- connection way points (x,y,z)
C CSEL(MCNN)       	- logical variable to determine if a 
C                         connection has been selected

C GRMAX(3)        	- grid maximum dimensions (m) on x,y,z dimensions
C GRSPC(3)        	- grid spacing in x,y,z dimensions
C GRLYRH(MLYRS)   	- grid layer height (redundant!)

C SCALF           	- current scaling factor (zoom)

C IVIEW           	- view setting  1 - xy 2 - xz 3 - yz
C VIEWCEN(3)      	- current view view centre (x,y,z)
C VIEWLIM(6)     	- current view limits (x1,x2,y1,y2,z1,z2)
 
C GON            	- grid on/off
C SON             	- snap on/off


C ************************* NETWORK ********************************
C This is the main network management routine.
      PROGRAM NETWORK

#include "gnetwk.h"
#include "building.h"
#include "espriou.h"

C Graphics and ESP-r default commons
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
      COMMON/FILEP/IFIL
      COMMON/MOD/MODEL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/POPHELP/H(60)
      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/FUNIT/IUCFG,IUPDB

C Path to problem and command line file (if any).
      COMMON/RPATH/PATH
      COMMON/UHOME/UPATH

C Legacy flow file commons.
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF,DSBEM

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

C Common for component/connection toggle
      COMMON/NWKGTOG/ITOG

C Grid commons
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/NWKTYP/INWKTYP,vergnf
      COMMON/ICONDBNAM/ICONDBFL
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam

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

      dimension idomain(MNWKTYP),nbcat(MNWKTYP),
     &  cattag(MNWKTYP,MICNCAT),catmenu(MNWKTYP,MICNCAT),
     &  nbicons(MNWKTYP,MICNCAT),icontag(MNWKTYP,MICNCAT,MICN),
     &  iconmenu(MNWKTYP,MICNCAT,MICN)
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)

      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM,ICONDBFL,LTMP,DFILE
      CHARACTER NWKTYPSTR*12,exttag*12
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      CHARACTER*34 ITEM(26)
      CHARACTER*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,DSBEM
      CHARACTER INF*144,H*72,PATH*72,UPATH*72
      CHARACTER FS*1,STR*72,outs*124,outstr*124
      LOGICAL UNIXOK,OK,DOK,CLOSEM,CLOSEX,isexavail
      dimension IVAL(MNWKTYP)
      character cattag*12,catmenu*32,icontag*12,iconmenu*40
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      character ddgtagatr*12,ddgmenuatr*32,ddgatrib*12
      CHARACTER*72 LAPROB*72
      character*72 DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam

C Initialize global common data. IUEX is file unit for the
C model external data summary.
      CALL EZERO
      call curmodule('net ')
      cAppName = 'net'
      IUOUT=6
      IUIN=5
      IFIL=10
      IUEX=42
      IFS=1
      ITFS=1
      IMFS=1
      ITOG=0
      NSIGFIG=3

C Scan the command line and copy the name of the argument into the network 
C filename string. 
C << this only takes first 72 char of a potentially longer inf >>
      CALL PARPSF(MODL,IAPPW,IAPPX,IAPPY,INF)
      IF(LNBLNK(INF).GT.1)THEN
        WRITE(NWKFLNAM,'(A72)')INF(1:72)
      ELSE
        NWKFLNAM='UNKNOWN'
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

C Determine terminal type and set write unit to stderr for rule scripts.
      MODL=8
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
        LIMTTY=4
        LIMIT =4
      else
        menuchw = 37
        LIMTTY=4
        LIMIT =4
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
     &  'ESP-r Network Tool: enquiries to esru@strath.ac.uk')

C Set additional output units to stdout. Then redirect warning
C messages to stderr in case of rule script program control.
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MODEL = 8.
      IF(MODEL.EQ.8)THEN
        CALL WIN3D(MENUCHW,8,10,2,3,IGL,IGR,IGT,IGB,IGW,IGWH)
#ifdef X11
        CALL OPENSETUP
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

C Activate the network graphics flag (activates special drawing routines 
C in the c-interface code.
        INGOF=1
        CALL NWKGFLG(INGOF)
        ISE=1
        ICO=0
        IDA=0
        CALL NWKSMOD(ISE,ICO,IDA)
      ENDIF

      CALL ESPrVersion("summary")

C Initialise the network on first entry and clear screen and draw grid.
      CALL NWKINIT
      CALL startbuffer()
      CALL NETWDRW

C If there is a command line file read it in. 
      IF(NWKFLNAM(1:3).NE.'UNK'.AND.NWKFLNAM(1:3).NE.'   ')THEN
        IER=0
        INUNIT=IFIL+1
        CALL NETREAD(INUNIT,'S',IER)
        if(IER.NE.0)then
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,'NWKLOAD: ERROR trying to read network!')
        endif

C If a flow network also build standard flow common blocks.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          CALL NETTOFLW(ier)
          call mflist(iuout)
        endif

C Also check for summary file. This is done each time a new graphic
C network file is read in. If the file exists and starts with *Synopsis
C do not 
        isexavail=.false.
        DFILE='network_flow.summary'
        lnf=lnblnk(NWKFLNAM)
        write(LTMP,'(2a)') NWKFLNAM(1:lnf-3),'summary'
        h(1)='This summary file is written by the project manager'
        h(2)='before invoking net. It is used by net to access'
        h(3)='model data.'
        CALL EASKS(LTMP,' Model summary file? ',' ',72,
     &    DFILE,'summary file open',IER,3)
        CALL EFOPSEQ(iuex,ltmp,1,IER)
        if(ier.ne.0)then
          isexavail=.false.
        else 
          CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
          if(IER.NE.0)isexavail=.false.
          if(OUTSTR(1:9).ne.'*Synopsis')then
            call usrmsg('Attempted to scan non-synopsis file named',
     &        ltmp,'W')
            isexavail=.false.
          else

C Scan the model summary file and report on zone names, volumes and height.
C << probably only if network type is flow. >>
            isexavail=.true.
            exttag='*Zones'
            call listexttag(exttag,ier)
          endif
        endif
      ENDIF                
 
C (Re)draw the network prior to building menu strings.
      CALL NETWDRW

  5   INO=-1
      WRITE(ITEM(1),'(2A)')'a network: ',NWKFLNAM(1:23)
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
        WRITE(ITEM(7),'(A,i2,A)')'e network attributes (',idgatrdom,')'
      else
        WRITE(ITEM(7),'(A)')     'e network attributes (None)'
      endif
      IF(ITOG.EQ.0)THEN
        WRITE(ITEM(8),'(A)')'e toggle >> components  '
        WRITE(ITEM(9),'(A,A,I3,A)')
     &'  No. of components ...     ','(',NNICN,')'
        WRITE(ITEM(10),'(A)')   'f  add component       '
        WRITE(ITEM(11),'(A)')   'g  delete component    '  
        WRITE(ITEM(12),'(A)')   'h  copy component      ' 
        WRITE(ITEM(13),'(A)')   'i  edit component data '    
        WRITE(ITEM(14),'(A)')   'j  list component data '    
      ELSE
        WRITE(ITEM(8),'(A)')'e toggle >> connections '
        WRITE(ITEM(9),'(A,A,I3,A)')
     &'  No. of connections ..     ','(',NICNN,')'
        WRITE(ITEM(10),'(A)')   'f  add connection      '
        WRITE(ITEM(11),'(A)')   'g  delete connection   '  
        WRITE(ITEM(12),'(A)')   'h  copy <N/A>          ' 
        WRITE(ITEM(13),'(A)')   'i  edit connection data'   
        WRITE(ITEM(14),'(A)')   'j  list connection data'   
      ENDIF    
         
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
      ITEM(23)='> save                           '
      ITEM(24)='! save as ...                    '
      ITEM(25)='? help                           '
      ITEM(26)='- exit                           '
      NITEMS=26
      CALL NETWDRW

C If there has been a new connection added check with user.
C Debug..
C      write(6,*) 'ihavenewcnn c',ihavenewcnn
      if(ihavenewcnn.ne.0)then
        dok=.true.
        h(1)='Recently a new connection was added to the network.'
        h(2)='Before doing anything else you should define its '
        h(3)='attributes. You can also edit attributes by clicking'
        h(4)='on the connecting line. '
        call askok('Edit this connections attributes?',
     &    ' ',ok,dok,4)
        if(ok)then
          J=ihavenewcnn
          CALL NETCNNDATR(IER,J)
          ihavenewcnn=0
        else
          ihavenewcnn=0
        endif
      endif
      CALL EMENU('Graphical network definition',ITEM,NITEMS,INO)

      IF(INO.EQ.NITEMS)THEN

C Switch network graphics off and quit.
        INGOF=0
        CALL NWKGFLG(INGOF)
        if(isexavail)then
          CALL ERPFREE(iuex,ISTAT)
        endif
        CALL EPAGEND
        STOP
      elseif(INO.EQ.NITEMS-1)THEN
        H(1)='The graphic network module produces graphic descriptions'
        H(2)='of networks related to simulation such as plant systems,'
        H(3)='flow networks and electrical networks. It accesses a'
        H(4)='database of icons and supports both the linking of icons'
        H(5)='and partial attribution of icons. '
        H(6)=' '
        H(7)='Network descriptions are written to a graphic network '
        H(8)='file which other computational agents scan for relevant '
        H(9)='information. '
        H(10)=' '
        H(11)='The current version of net should be considered work in'
        H(12)='progress and the format of the icons database and the '
        H(13)='graphic network file is subject to revision. '
        H(14)=' '
        H(15)='Use of the net module: '
        H(16)=' option a defines the `gnf` file which will be loaded if'
        H(17)='          it exists.'
        H(18)=' option b description of the file contents and/or use'
        H(19)=' option c the domain of the network (currently mixed'
        H(20)='          domains are not supported) which should be'
        H(21)='          defined before adding components.'
        H(22)=' option d the icons database used by the `gnf` file.'
        H(23)=' option e switches focus between components and'
        H(24)='          connections between components'
        H(25)=' If the focus is components then you have the option to'
        H(26)=' add a new component, delete components which have been'
        H(27)=' highlighted (ckick with left mouse button), copy a'
        H(28)=' component (left mouse button to select which icon to'
        H(29)=' copy and click again to place the copy) and edit the'
        H(30)=' attributes of the icon (left mouse button to select).'
        H(31)=' '
        H(32)=' If the focus is connections then you have the option'
        H(33)=' to add a connection (select menu option then use left'
        H(34)=' mouse button to click on one of the attachement boxes'
        H(35)=' then the middle mouse button for intermediate points'
        H(36)=' and the right button on another icon attachment box.'
        H(37)=' You can also delete a previously highlighted connection'
        H(38)=' (click on any intermediate point with left button). You'
        H(39)=' can also edit connection attributes of a previously'
        H(40)=' highlighted connection.'
        H(41)=' '
        H(42)='Changes to the gridding are accomplished via the middle'
        H(43)='mouse button (clicking on a point not close to an '
        H(44)='existing icon or connection. '
        CALL PHELPD('network main',44,'-',0,0,IER)
      ELSEIF(INO.EQ.2)THEN

C Provide a description of the network.
        h(1)='This phrase can describe the intent of this network,'
        h(2)='any assumptions or project references.'
        STR='None'
        CALL EASKS(NWKDSC,
     &    'Please provide a short description of the network',
     &    '  ',72,STR,'netdesc',IER,2)
      ELSEIF(INO.EQ.4)THEN

C Set the network type
        H(1)='Select the type of network you want to develop.'
        H(2)='Each domain includes a set of icons which can be'
        H(3)='associated with related components/entities.'
        IPCK=1
        CALL EPICKS(IPCK,IVAL,'Select a network type',' ',12,MNWKTYP,
     &    NWKTYPSTR,'Network select',IER,3)
        IF(IVAL(1).GT.0)THEN
          INWKTYP=IVAL(1)
        ELSE
          INWKTYP=1
          CALL EDISP(IUOUT,'Setting network type to default: HVAC')
        ENDIF
      ELSEIF(INO.EQ.5)THEN

C Option to read another icon file name. << hardcoded >>
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          DFILE='/usr/esru/esp-r/databases/flow_icons.db1'
        else
          DFILE=DICONDBFL
        endif
        h(1)='This is the database file which contains the icons'
        h(2)='for the network tool.'
        ltmp=ICONDBFL
  2     CALL EASKS(ltmp,'icon database filename ?',
     &    ' ',72,DFILE,'ecomp dbnam',IER,2)
        if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN') ICONDBFL=ltmp

        ICONFIL=IFIL+1
        CALL EFOPSEQ(ICONFIL,ICONDBFL,1,IER)
        IF(IER.NE.0) THEN
          dok=.false.
          h(1)='Icons for this application are held in a file which is'
          h(2)='typically /usr/esru/esp-r/databases/flow_icons.db1 for'
          h(3)='flow networks and /usr/esru/esp-r/databases/icons.db1'
          h(4)='for other networks. '
          h(5)=' '
          h(6)='The the folder /usr/esru/esp-r/databases '
          h(7)=' '
          h(8)='A no will set this file name to UNKNOWN. '
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
          call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
     &      catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
     &      datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
        ENDIF
      ELSEIF(INO.EQ.7)THEN

C If there are network global attributes edit them.
        if(idgatrdom.gt.0)then
          call netgdatr(ier)
        endif
      ELSEIF(INO.EQ.8)THEN

C Toggle between components and connections.
        IF(ITOG.EQ.0)THEN
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          ITOG=1
        ELSE
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          ITOG=0
        ENDIF
      ELSEIF(INO.EQ.10)THEN
        IF(ITOG.EQ.0)THEN

C Add a icon. Call to nwksmod and netwmic setup C functions
C which then calls NETWMIC.
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMIC(0,1)
        ELSE

C Add a connection. Call to nwksmod and netwmco setup C functions
C which then calls GCONADD.
          lastnicnn = NICNN
          ISE=0
          ICO=1
          IDA=0
          ihavenewcnn=0
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMCO(0,1)
        ENDIF
      ELSEIF(INO.EQ.11)THEN

C Delete a connection or icon
        IF(ITOG.EQ.0)THEN
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMIC(0,2)
        ELSE
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMCO(0,2)
        ENDIF
      ELSEIF(INO.EQ.12)THEN

C Copy an icon (connection copy N/A).
        IF(ITOG.EQ.0)THEN
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMIC(0,3)
        else
          continue
        ENDIF
      ELSEIF(INO.EQ.13)THEN 

C Component or connection editing data here.
        if(ITOG.EQ.0)then
          ISE=1
          ICO=0
          IDA=1
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMIC(0,4)           
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
        else
          ISE=1
          ICO=0
          IDA=1
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMCO(0,4)
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
        endif
      ELSEIF(INO.EQ.14)THEN

C Component or connection listing..
        if(ITOG.EQ.0)then
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMIC(0,5)           
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
        else

C << this is new needs checking >>
          ISE=1
          ICO=0
          IDA=1
          CALL NWKSMOD(ISE,ICO,IDA)
          CALL NETWMCO(0,4)
          ISE=1
          ICO=0
          IDA=0
          CALL NWKSMOD(ISE,ICO,IDA)
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
        WRITE(OUTS,'(A,F5.1)')'enter a value between 0.125 and ',
     &    GRMAX(1)
        CALL EASKR(GRSPC(1),'XY spacing?',OUTS,0.125,'F',GRMAX(1),'F',
     &    1.0,'gridspace',IER,0)
        GRSPC(2)=GRSPC(1)
      ELSEIF(INO.EQ.18)THEN

C Specify Z grid spacing.
        WRITE(OUTS,'(A,F5.1)')'enter a value between 0.125 and ',
     &    GRMAX(1)
        CALL EASKR(GRSPC(3),'Z spacing?',OUTS,0.125,'F',GRMAX(3),'F',
     &    1.0,'gridspace',IER,0)

      ELSEIF(INO.EQ.19)THEN 
        WRITE(OUTS,'(A,F5.1)')'enter a value between 0.5 and 8.0'
        CALL EASKR(SCALF,'zoom setting?',OUTS,VIEWMN,'F',VIEWMZ,'F',
     &  VIEWMZ,'zoom setting',IER,0)
      ELSEIF(INO.EQ.19)THEN

C Edit rotation angle.
        CALL EASKR(ROTA,'Icon rotation angle?',' ',0.0,'-',
     &    0.0,'-',0.0,'gridspace',IER,0)
      ELSEIF(INO.EQ.21)THEN

C Refresh icons and connections.
        CALL NETWDRW
      ELSEIF(INO.EQ.23)THEN

C Save current network to file.
        IM=1
        CALL NETWRITE(IM)

C Update the flow commons and list them out.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          CALL NETTOFLW(ier)
          call mflist(iuout)
        endif
      ELSEIF(INO.EQ.24)THEN

C Save network to a user specified file.
        IM=2
        CALL NETWRITE(IM)

C Update the flow commons and list them out. Ask user if they want
C to create a legacy network flow file.
        if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
          CALL NETTOFLW(ier)
          call mflist(iuout)
          OK=.FALSE.
          dok=.true.
          h(1)='The icon based network file is not used directly '
          h(2)='by the simulator. It uses a text-based flow network'
          h(3)='file and if you say yes to this the current network'
          h(4)='will be saved to this file format. '
          h(5)=' '
          h(6)='If the network is incomplete you might not want to'
          h(7)='not create this extra file.'
          CALL ASKOK('Do you want to save the nodes & components',
     &      'to a text-based flow network file?',OK,dok,7)
          if(OK)then

C Transfer data to fluid flow file and return to main menu.
   78       IUM=IFIL+6
            IF(NWKFLNAM(1:3).NE.'UNK'.AND.NWKFLNAM(1:2).NE.'  ')THEN
              write(LTMP,'(2a)') NWKFLNAM(1:lnblnk(NWKFLNAM)),'.afn'
            else
              ltmp='legacy.afn'
            endif
            h(1)='The network flow information within the graphic'
            h(2)='network file can be extracted into a legacy fluid'
            h(3)='flow file for use in simulations. '
            h(4)=' '
            h(5)='Note: wind pressures and pressure sets should be'
            h(6)='updated within the project manager. '
            CALL EASKS(LTMP,' Fluid flow network file ? ',
     &        '(legacy see help)',72,' ','mass flow file',IER,6)
            if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
              LAPROB=LTMP
            else
              goto 78
            endif
            CALL EFOPSEQ(IUM,LAPROB,4,IER)
            IF(IER.NE.0)goto 78
            CALL MFWRIT(IUM)
            CALL ERPFREE(IUM,ISTAT)
          endif
        endif
      ELSEIF(INO.EQ.1)THEN

C Read in a network, clear exiting network definitions and refreshing
C the screen.
        h(1)='to test reading functions... '
        call easkabc('Options:',' ','standard network file read',
     &    'read & fill flow commons','continue',iwh,1)
        if(iwh.eq.3)then
          goto 5
        endif
        OK=.FALSE.
        dok=.false.
        h(1)='The description of the current network will be replaced'
        h(2)='by any new network defined or read in. If you want to'
        h(3)='keep any recent changes, say no and save the file first.'
        CALL ASKOK('Reading in a network will clear the current one',
     &    'are you sure you want to do this?',OK,dok,3)
        IF(.NOT.OK) GOTO 5
        CALL NWKINIT
        CALL startbuffer
        CALL NETWDRW
        NWKFLNAM='UNKNOWN'
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
          lnf=lnblnk(NWKFLNAM)
          write(LTMP,'(2a)') NWKFLNAM(1:lnf-3),'summary'
          h(1)='This summary file is written by the project manager'
          h(2)='before invoking net. It is used by net to access'
          h(3)='model data.'
          CALL EASKS(LTMP,' Model summary file? ',' ',72,
     &      DFILE,'summary file open',IER,3)
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
        GOTO 5
      ENDIF

C Re-draw the screen to impliment any changes before redrawing menu.
      CALL NETWDRW

C If there has been a new connection added check with user.
C Debug..
C      write(6,*) 'ihavenewcnn b',ihavenewcnn

      GOTO 5
      END

C ************************* NETWDRW **********************************
C This is the main drawing routine for the network  and is called
C when the workspace needs to be resized or redrawn. 
      SUBROUTINE NETWDRW

# include "gnetwk.h"

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKGRDL/GON,SON
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/NWKTYP/INWKTYP,vergnf

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM
      CHARACTER NWKTYPSTR*12
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12,OUTS1*26
      LOGICAL GON,SON,greyok

C Clear the screen
      CALL startbuffer

C Set the new x y and z view limits. Currently do not scroll
C left further than X = -5m and down to Y = -5m.
      VIEWLIM(1)=VIEWCEN(1)-((FLOAT(MGRXE)*0.5)/SCALF)
      IF(VIEWLIM(1).LT.-5.0) THEN
        VIEWLIM(1)=-5.0
        VIEWLIM(2)= VIEWLIM(1)+MGRXE/SCALF 
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

C Draw the grid with the current scaling parameters.
      IF(IVIEW.EQ.1)THEN
        XMIN=VIEWLIM(1)
        XMAX=VIEWLIM(2)
        YMIN=VIEWLIM(3)
        YMAX=VIEWLIM(4)
      ELSEIF(IVIEW.EQ.2)THEN
        XMIN=VIEWLIM(1)
        XMAX=VIEWLIM(2)
        YMIN=VIEWLIM(5)
        YMAX=VIEWLIM(6)
      ELSE
        XMIN=VIEWLIM(3)
        XMAX=VIEWLIM(4)
        YMIN=VIEWLIM(5)
        YMAX=VIEWLIM(6)
      ENDIF

      CALL AXISCALE(IGW,IGWH,XMIN,XMAX,YMIN,YMAX,XSC,YSC,SCA,XADD,
     &       YADD)

C Draw the Lower and Left extents of the grid and labels.
      IF(IVIEW.EQ.1.OR.IVIEW.EQ.2)THEN
        CALL horaxis(Xmin,Xmax,igl,igr,igb,xadd,XSC,1,
     &    'X (all grid dimensions in m)')
      ELSE
        CALL horaxis(Xmin,Xmax,igl,igr,igb,xadd,XSC,1,
     &    'Y (all grid dimensions in m)')
      ENDIF

      IF(IVIEW.EQ.1)THEN      
        CALL vrtaxis(Ymin,Ymax,igl,igb,igt,yadd,YSC,1,0,'Y')
      ELSE
        CALL vrtaxis(Ymin,Ymax,igl,igb,igt,yadd,YSC,1,0,'Z')
      ENDIF

C Store the scaling parameters used in drawing the axes.
      CALL linescale(igl,xadd,XSC,igb,yadd,YSC)

C Draw the upper and right extents.
      call u2pixel(XMAX,YMIN,ix1,iy1)
      call u2pixel(XMAX,YMAX,ix2,iy2)
      call eswline(ix1,iy1,ix2,iy2)
      call u2pixel(XMIN,YMAX,ix3,iy3)
      call u2pixel(XMAX,YMAX,ix4,iy4)
      call eswline(ix3,iy3,ix4,iy4)

C Check that the grid is ON GRSPC(*)<>0.0
      IF(GON)THEN

C If libraries and monitor support greyscale then reset forground
C to 50% grey when drawing dots.
        greyok=.false.
        if(nifgrey.gt.4)then
          call winscl('i',nifgrey-3)
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
        CALL ETPLOT(GX,YMIN,0,0)    
10      CALL ETPLOT(GX,YMAX,-203,0)
        GX=GX+GSPCW 
        CALL ETPLOT(GX,YMIN,0,0)
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
        CALL ETPLOT(XMIN,GY,0,0)    
20      CALL ETPLOT(XMAX,GY,-203,0)
        GY=GY+GSPCH
        CALL ETPLOT(XMIN,GY,0,0)
        IF(GY.LT.YMAX) GOTO 20

C Return to black lines after drawing grid.
        if(greyok)call winscl('-',0)
      ENDIF

C Draw on the network description. 
      WRITE(OUTS1,'(A,A)')'Network Type: ',NWKTYPSTR(INWKTYP)(1:12)
      CALL viewtext(OUTS1,1,1,1)

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
C This routine manages the network icons, can be called from the C-side
C to handle deletion of icons.
      SUBROUTINE NETWMIC(IMODE,IMOPT)

# include "gnetwk.h"
#include "epara.h"

      COMMON/POPHELP/H(60)  

      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

C Icon commons       
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60) 

      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      COMMON/NWKTYP/INWKTYP,vergnf  
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

      dimension idomain(MNWKTYP),nbcat(MNWKTYP),
     &  cattag(MNWKTYP,MICNCAT),catmenu(MNWKTYP,MICNCAT),
     &  nbicons(MNWKTYP,MICNCAT),icontag(MNWKTYP,MICNCAT,MICN),
     &  iconmenu(MNWKTYP,MICNCAT,MICN)
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)
      dimension atrib(MIATRB,3),tagatr(MIATRB,5),menuatr(MIATRB)
      dimension vert(MICNV,2),iedge(MICNE,5),idot(MICND,4),
     &  iarc(MICND,7),ilabel(MICND,4),labeltx(MICND),text(60),
     &  iatt(MCNP,2)

C ICITEM1 is ...
C ICITEM2 is ...
      DIMENSION ICITEM1(MICNCAT+3),ICITEM2(MICN+4)
      
      CHARACTER H*72,OUTS*124,name*12
      CHARACTER NWICNM*12,ICDNAM*12,NWICNHIS*40,NWICNLBL*4,NWKTYPSTR*12
      character NWICNTXT*72
      CHARACTER A*1,ICITEM1*44,ICITEM2*44, KEY*1
      character cattag*12,catmenu*32,category*12
      character icontag*12,iconmenu*40,labeltx*4,text*72
      character tagatr*12,menuatr*32,atrib*12
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM

      LOGICAL CLOSEX,CLOSEY,OK,DOK,MREPT, ISEL,CSEL,found

C Set the component reporting flag.
      MREPT=.FALSE.
   
C Ask if the user wants to add/delete/copy/edit/list an icon.
      IOPT=IMOPT

      IF(IOPT.EQ.1.OR.IOPT.EQ.3)THEN

C Add or copy an icon.
        ITCMP=NNICN+1
        IF(IOPT.EQ.3)THEN
          if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
            CALL USRMSG('Click on the component to copy',' ','-')
          elseif(INWKTYP.eq.2)then
            CALL USRMSG('Click on the node/component to copy',' ','-')
          elseif(INWKTYP.eq.4)then
            CALL USRMSG('Click on the control entity to copy',' ','-')
          endif
          CALL TRACKVIEW(A,IX,IY)

          IPX=IX
          IPY=IY
          CALL PIXEL2U(IPX,IPY,GX,GY)
          CGX=GX
          CGY=GY
          CLOSEX=.FALSE.
          CLOSEY=.FALSE.
          DO 15 I=1,NNICN
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.5,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,2),0.5,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.5,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.5,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,XYZICON(I,2),0.5,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.5,CLOSEY)
            ENDIF

            IF(CLOSEX.AND.CLOSEY)THEN
              DELTAX=0.0
              DELTAY=0.0
              DELTAZ=0.0

C Switch object selected status.
              WRITE(OUTS,'(A,A)')'Copying ',NWICNM(I)
              CALL USRMSG(OUTS,' ','-')

C Place it one the drawing area.                                        
              CALL USRMSG('Now position it on the screen',' ','-')
              CALL TRACKVIEW(A,IX,IY)
              IPX=IX
              IPY=IY
              CALL PIXEL2U(IPX,IPY,GX,GY)
              CGXC=GX
              CGYC=GY
              IF(IVIEW.EQ.1)THEN
                GRMAX1=GRMAX(1)
                GRMAX2=GRMAX(2)
              ELSEIF(IVIEW.EQ.2)THEN
                GRMAX1=GRMAX(1)
                GRMAX2=GRMAX(2)
              ELSE
                GRMAX1=GRMAX(2)
                GRMAX2=GRMAX(3)
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
          H(1)='Requires a unique name, which contains'
          H(2)='no blank spaces.'
          CALL EASKS(NWICNM(NNICN),'Give a (unique) name',
     &    '(with no blank spaces) ',12,ICDNAM,'newcmp',IER,2)

C Check for duplicate names ....
          icn=nnicn
          call dupicname(icn)
          RETURN
        ENDIF

C Add a component icon by asking the user to select from the
C icon database. First recover existing categories.
        CALL USRMSG('Please select from list:',' ','-')
  8     call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
     &    catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
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

        M=MHEAD
        DO 17 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(L,KEY,IER)
            WRITE(ICITEM1(M),'(A,1X,A)')key,catmenu(id,L)(1:31)
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
          H(1)='The menu options relate to different categories'
          H(2)='of icons which may be selected for use with the'
          H(3)='current domain in the network tool.'
          CALL PHELPD('Icon Category',3,'-',0,0,IER)
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
        WRITE(ICITEM2(2),'(A)')' select a component: '
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
            ICITEM2(M+2)='>> component info ON    '
          ELSE
            ICITEM2(M+2)='>> component info OFF   '
          ENDIF
          ICITEM2(M+3)  ='? help                  '
          ICITEM2(M+4)  ='- exit this menu        '
          NICITEM2=M+MCTL

          IMOUT2=-1   
          CALL NETWDRW   
 13       CALL EMENU('Components in category',ICITEM2,NICITEM2,IMOUT2)
        
          IF(IMOUT2.EQ.NICITEM2) THEN

C return to the previous menu ..
            GOTO 717
          ELSEIF(IMOUT2.EQ.NICITEM2-1) THEN
            H(1)='Select a component from the list, which will '
            H(2)='then be drawn into the network. '
            H(3)='  '
            H(4)='Switch the "component info" toggle to ON to view'
            H(5)='any more information associated with the '
            H(6)='component; this appears in the text window.'
            CALL PHELPD('Component',6,'-',0,0,IER)
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
            NWICNM(ITCMP)='New_Comp'
		 
C Get details of icon and then get its attributes (if any).
            call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &        nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &        nbarc,iarc,nbatt,iatt,nbtext,text,IER)
     
C Debug...
C      write(6,*) nbvert,nbedge,nbdot,nblabel,nbarc,nbatt,nbtext
C      write(6,*) iedge
C      write(6,*) idot
C      write(6,*) ilabel   
            call geticonatr(inwktyp,category,ICDNAM,natrib,
     &        tagatr,atrib,menuatr,IER)
     
C Debug...
C      write(6,*) natrib		 
C      write(6,*) tagatr		 
C      write(6,*) atrib		 

C Get the component template data.
C For (nbtext times) extract text (documentation associated with icon)
            NIVT(ITCMP)=nbtext
            if(nbtext.ge.1)then
              do 38 IV=1,NIVT(ITCMP)
                NWICNTXT(ITCMP,IV)=text(IV)
  38          continue
            endif

C For (natrib times) extract attributes of the icon (tags, values,
C menu entries). Note atrib can have up to MIATRB attributes.
            NICONATR(ITCMP)=natrib
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

C Place it one the drawing area.                                        
        CALL USRMSG('Now position it on the screen',' ','-')
        CALL TRACKVIEW(A,IX,IY)
        IPX=IX
        IPY=IY
        CALL PIXEL2U(IPX,IPY,GX,GY)
        CGX=GX
        CGY=GY
        IF(IVIEW.EQ.1)THEN
          GRMAX1=GRMAX(1)
          GRMAX2=GRMAX(2)
        ELSEIF(IVIEW.EQ.2)THEN
          GRMAX1=GRMAX(1)
          GRMAX2=GRMAX(2)
        ELSE
          GRMAX1=GRMAX(2)
          GRMAX2=GRMAX(3)
        ENDIF
        IF((CGX.LT.GRMAX1.AND.CGX.GT.0.0).AND.
     &     (CGY.LT.GRMAX2.AND.CGY.GT.0.0))THEN

C Snap the component to the grid if this is active. 
          CALL NETWSNAP(CGX,CGY,CGXS,CGYS)
          CGX=CGXS
          CGY=CGYS

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

C << At this point, if `Flow` domain and an internal node the
C << interface offer the user an option to pick from
C << the `.summary` *Zones list for data. Not yet implemented.
        ICONTP(NNICN)=1
        CALL NETWDRW
        NWICNM(NNICN)=ICDNAM
        H(1)='Requires a unique name, which contains'
        H(2)='no blank spaces.'
        CALL EASKS(NWICNM(NNICN),'Give a (unique) name',
     &    '(with no blank spaces) ',12,ICDNAM,'newcmp',IER,2)

C Check for duplicate names .... 
        icn=nnicn
        call dupicname(icn) 

C Debug..
        write(6,*) 'just after naming of icon ',icn 
        write(outs,'(2a)')  'Loading attributes of ',NWICNM(NNICN) 
        call edisp(iuout,outs)
        icnpick=icn
        call NETICONDATR(icnpick,ier)
      ELSEIF(IOPT.EQ.2)THEN

C Delete all selected components.
        IF(IMODE.NE.1) THEN

C Let the user select an icon to delete (all calls to netwmic have
C had imode set to zero).
          if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
            CALL USRMSG(' click on the component to delete',' ','-')
          elseif(INWKTYP.eq.2)then
            CALL USRMSG(' click on the node/component to delete',' ',
     &      '-')
          elseif(INWKTYP.eq.4)then
            CALL USRMSG(' click on the control entity to delete',' ',
     &      '-')
          endif
          CALL TRACKVIEW(A,IX,IY)

          IPX=IX
          IPY=IY
          CALL PIXEL2U(IPX,IPY,GX,GY)
          CGX=GX
          CGY=GY
          CLOSEX=.FALSE.
          CLOSEY=.FALSE.

C Loop through all icons, clearing current selections and setting
C isel()=.true. to the one nearest.
          DO 705 I=1,NNICN
            ISEL(I)=.FALSE.
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.5,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,2),0.5,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,XYZICON(I,1),0.5,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.5,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,XYZICON(I,2),0.5,CLOSEX)
              CALL ECLOSE(CGY,XYZICON(I,3),0.5,CLOSEY)
            ENDIF

            IF(CLOSEX.AND.CLOSEY)THEN
              ISEL(I)=.TRUE.
              name=NWICNM(I)
            ENDIF
  705     CONTINUE
          CALL NETWDRW

C Confirm that this is the icon to be deleted.
          write(outs,'(3a)') 'Delete the icon ',name(1:lnblnk(name)),
     &      ' and associated connections?'
          dok=.false.
          h(1)='Pause in case you decide not to delete connections.'
          CALL ASKOK(outs,' ',OK,dok,1)
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
C attributes and NETICONDATR is for ones with few attributes.
C        call NETIDATR(ier)
        if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
          CALL USRMSG('Click on the component to edit',' ','-')
        elseif(INWKTYP.eq.2)then
          CALL USRMSG('Click on the node/component to edit',' ','-')
        elseif(INWKTYP.eq.4)then
          CALL USRMSG('Click on the control entity to edit',' ','-')
        endif
        CALL TRACKVIEW(A,IX,IY)
        IPX=IX
        IPY=IY
        CALL PIXEL2U(IPX,IPY,GX,GY)
        CGX=GX
        CGY=GY
        CLOSEX=.FALSE.
        CLOSEY=.FALSE.
        DO 515 ICN=1,NNICN
          IF(IVIEW.EQ.1)THEN
            CALL ECLOSE(CGX,XYZICON(ICN,1),0.5,CLOSEX)
            CALL ECLOSE(CGY,XYZICON(ICN,2),0.5,CLOSEY)
          ELSEIF(IVIEW.EQ.2)THEN
            CALL ECLOSE(CGX,XYZICON(ICN,1),0.5,CLOSEX)
            CALL ECLOSE(CGY,XYZICON(ICN,3),0.5,CLOSEY)
          ELSE
            CALL ECLOSE(CGX,XYZICON(ICN,2),0.5,CLOSEX)
            CALL ECLOSE(CGY,XYZICON(ICN,3),0.5,CLOSEY)
          ENDIF

          IF(CLOSEX.AND.CLOSEY)THEN
            icnpick=icn
            call NETICONDATR(icnpick,ier)
          else
            continue
          endif
  515   continue
      ELSEIF(IOPT.EQ.5)THEN

C List the icon details.
        if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
          CALL USRMSG('Click on the component to list',' ','-')
        elseif(INWKTYP.eq.2)then
          CALL USRMSG('Click on the node/component to list',' ','-')
        elseif(INWKTYP.eq.4)then
          CALL USRMSG('Click on the control entity to list',' ','-')
        endif
        CALL TRACKVIEW(A,IX,IY)

        IPX=IX
        IPY=IY
        CALL PIXEL2U(IPX,IPY,GX,GY)
        CGX=GX
        CGY=GY
        CLOSEX=.FALSE.
        CLOSEY=.FALSE.
        DO 115 I=1,NNICN
          IF(IVIEW.EQ.1)THEN
            CALL ECLOSE(CGX,XYZICON(I,1),0.5,CLOSEX)
            CALL ECLOSE(CGY,XYZICON(I,2),0.5,CLOSEY)
          ELSEIF(IVIEW.EQ.2)THEN
            CALL ECLOSE(CGX,XYZICON(I,1),0.5,CLOSEX)
            CALL ECLOSE(CGY,XYZICON(I,3),0.5,CLOSEY)
          ELSE
            CALL ECLOSE(CGX,XYZICON(I,2),0.5,CLOSEX)
            CALL ECLOSE(CGY,XYZICON(I,3),0.5,CLOSEY)
          ENDIF
          IF(CLOSEX.AND.CLOSEY)THEN
            WRITE(H(1),'(2A)') 'Name: ', NWICNM(I)(1:lnblnk(NWICNM(I)))
            WRITE(H(2),'(A)')
     &'  domain      |category    |name          '
            WRITE(H(3),'(A)') NWICNHIS(I)(1:40)
            if(NIVT(I).gt.0)then
              MM=4
              WRITE(H(MM),'(A)') 'Notes: '
              do 116 jh=1,NIVT(I)
                MM=MM+1
                if(MM.lt.58)then
                  WRITE(h(MM),'(A)') NWICNTXT(I,jh)
                else
                  MM=MM-1
                endif
  116         continue
            else
              MM=2
            endif
            if(NICONATR(I).gt.0)then
              MM=MM+1
              WRITE(H(MM),'(A)') 'Attributes: '
              DO 520 jh=1,NICONATR(I)
                MM=MM+1
                if(MM.lt.58)then
                  l3=lnblnk(ATRTAG(I,JH,1))
                  l6=lnblnk(ATRMENU(I,JH))
                  l7=MAX0(lnblnk(ATRICN(I,JH,1)),1)
                  WRITE(h(MM),'(6A)') ' ',ATRTAG(I,jh,1)(1:l3),': ',
     &              ATRMENU(I,jh)(1:l6),' is ',ATRICN(I,jh,1)(1:l7)
                else
                  MM=MM-1
                endif
 520          continue
            endif
            CALL PHELPD('icon list',MM,'-',0,0,IER)
            return
          endif
 115    continue
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

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

      COMMON/POPHELP/H(60)  

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C Grid commons
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      CHARACTER H*72,AW*1,ddtagatr*12,ddmenuatr*32,ddatrib*12,A*1

      LOGICAL ISEL,CSEL,OK,DOK,CLOSEX,CLOSEY

C Depending on if user wants to add/delete/copy/edit a component      
      IOPT=IMOPT

      IF(IOPT.EQ.1)THEN
        call USRMSG(
     &' connect: button1-start ; button2-waypoint ; button3-end',
     &' ','-')
        RETURN
      ELSEIF(IOPT.EQ.2)THEN

C Let the user select a connection        
        CALL USRMSG(' click on the connection to delete',' ','-')
        CALL TRACKVIEW(A,IX,IY)
        IPX=IX
        IPY=IY
        CALL PIXEL2U(IPX,IPY,GX,GY)
        CGX=GX
        CGY=GY
        CLOSEX=.FALSE.
        CLOSEY=.FALSE.
        DO 705 I=1,NICNN
          CSEL(I)=.FALSE.
          DO 706 J=1,NCONWP(I)
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,CNWNP(I,J,1),0.5,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(I,J,2),0.5,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,CNWNP(I,J,1),0.5,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(I,J,3),0.5,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,CNWNP(I,J,2),0.5,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(I,J,3),0.5,CLOSEY)
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
          h(1)='Pause, in case you made a selection error. '
          CALL ASKOK('Delete selected connection?',' ',OK,dok,1)
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

C Edit the conection data
        CALL USRMSG('Click on a connection (way-point) to edit',' ',
     &  '-')
        CALL TRACKVIEW(AW,IWX,IWY)
C        IX=IWX
C        IY=IWY
C        CALL NETCNNDATR(IER,IX,IY)
        IPX=IWX
        IPY=IWY
        CALL PIXEL2U(IPX,IPY,GX,GY)
        CGX=GX
        CGY=GY
        CLOSEX=.FALSE.
        CLOSEY=.FALSE.
        IMATCH=0
        DO 515 JJ=1,NICNN
          DO 516 ICWP=1,NCONWP(JJ)
            IF(IVIEW.EQ.1)THEN
              CALL ECLOSE(CGX,CNWNP(JJ,ICWP,1),0.2,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(JJ,ICWP,2),0.2,CLOSEY)
            ELSEIF(IVIEW.EQ.2)THEN
              CALL ECLOSE(CGX,CNWNP(JJ,ICWP,1),0.2,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(JJ,ICWP,3),0.2,CLOSEY)
            ELSE
              CALL ECLOSE(CGX,CNWNP(JJ,ICWP,2),0.2,CLOSEX)
              CALL ECLOSE(CGY,CNWNP(JJ,ICWP,3),0.2,CLOSEY)
            ENDIF
            IF(CLOSEX.AND.CLOSEY)THEN
              IMATCH=JJ
              CLOSEX=.FALSE.
              CLOSEY=.FALSE.
            ENDIF
 516      CONTINUE
 515    continue

C If we have a match proceed.
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
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60) 

      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/GFONT/IFS,ITFS,IMFS

C connection points property array
C      COMMON/CNP/CPP(3,MNCNN),ICPP(MICN,4,3),III

C connection points parameter array
C      COMMON/CPPA/ENV(5),PARA(MNCNN,13),CPARA(MICN,4,13),
C     &  SUB_MATRIX_A(MICN,MNCNN,MNCNN),SUB_MATRIX_B(MICN,MNCNN),
C     &  MATRIX_A(MNCNN,MNCNN),MATRIX_B(MNCNN),ICOUNTER,IDONE

      CHARACTER LABL*12
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNTXT*72
      character NWICNLBL*4,LBLINT*4
      character ATRTAG*12,ATRMENU*32,ATRICN*12

C      CHARACTER CPP*30,ICPP*30
C      REAL ENV,PARA,CPARA,SUB_MATRIX_A,SUB_MATRIX_B
C      REAL MATRIX_A,MATRIX_B

      LOGICAL ISEL,CSEL,INBOUND,CLOSE

      X=XYZICON(ICN,1)
      Y=XYZICON(ICN,2)

      INBOUND=.TRUE.

C Get view limits and check if the component should be plotted in this view. 
      IF(IVIEW.EQ.1)THEN
        X=XYZICON(ICN,1)
        Y=XYZICON(ICN,2)

C << do not check for Z differences in current version of net >>
C        CALL ECLOSE(XYZICON(ICN,3),VIEWCEN(3),0.01,CLOSE) 
C        IF(.NOT.CLOSE) RETURN
      ELSEIF(IVIEW.EQ.2)THEN
        X=XYZICON(ICN,1)
        Y=XYZICON(ICN,3)
        CALL ECLOSE(XYZICON(ICN,2),VIEWCEN(2),0.01,CLOSE) 
        IF(.NOT.CLOSE) RETURN
      ELSE
        X=XYZICON(ICN,2)
        Y=XYZICON(ICN,3)
        CALL ECLOSE(XYZICON(ICN,1),VIEWCEN(1),0.01,CLOSE) 
        IF(.NOT.CLOSE) RETURN
      ENDIF  

C Get pixel position of the point and plot the icon and its label.
      CALL U2PIXEL(X,Y,IX,IY)

C Get the pixel position of the lower and left edge of the component
      CALL U2PIXEL(X-XSIZIC,Y-YSIZIC,IXL,IYL)
      CALL U2PIXEL(X+XSIZIC,Y+YSIZIC,IXR,IYU)

C Check the component is on the graphics screen
      IF(IXL.GT.IGL.AND.IXR.LT.IGR.AND.IYU.GT.IGT.AND.IYL.LT.IGB)THEN

C Move to the first point and plot the icon in highlighted mode.
        if(NIVE(ICN).ge.1)then
          DO 10 I=1,NIVE(ICN)
            IS=IVEICN(ICN,I,1)
            IE=IVEICN(ICN,I,2)
            call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),ix1,iy1)
            call u2pixel(VCICON(ICN,IE,1),VCICON(ICN,IE,2),ix2,iy2)
            icol=IVEICN(ICN,I,3)
            if(IVEICN(ICN,I,4).eq.0)then
              call winscl('i',icol)
            elseif(IVEICN(ICN,I,4).eq.1)then
              call winscl('g',icol)
            elseif(IVEICN(ICN,I,4).eq.2)then
              call winscl('z',icol)
            endif

C IVEICN(n,n,5) line type (1=solid, 2=dotted, 3=dashed, 4=double)
C If select draw double thickness, otherwise use the line style of the icon.
            if(ISEL(ICN))then
              call edwline(ix1,iy1,ix2,iy2)
            else
              if(IVEICN(ICN,I,5).eq.0)then
                call eswline(ix1,iy1,ix2,iy2)
              elseif(IVEICN(ICN,I,5).eq.1)then
                call eswline(ix1,iy1,ix2,iy2)
              elseif(IVEICN(ICN,I,5).eq.2)then
                call edline(ix1,iy1,ix2,iy2,3)
              elseif(IVEICN(ICN,I,5).eq.3)then
                call edash(ix1,iy1,ix2,iy2,3)
              elseif(IVEICN(ICN,I,5).eq.4)then
                call edwline(ix1,iy1,ix2,iy2)
              endif
            endif
            call winscl('-',0)
            call forceflush()
  10      CONTINUE
        endif

C Do any dots.
        if(NIVD(ICN).ge.1)then
          DO 11 I=1,NIVD(ICN)
            IS=IVDOT(ICN,I,1)
            call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),ix1,iy1)
            icol=IVDOT(ICN,I,2)
            if(IVDOT(ICN,I,3).eq.0)then
              call winscl('i',icol)
            elseif(IVDOT(ICN,I,3).eq.1)then
              call winscl('g',icol)
            elseif(IVDOT(ICN,I,3).eq.2)then
              call winscl('z',icol)
            endif

C Draw small or large dot.
            if(IVDOT(ICN,I,4).eq.0)then
	      call esymbol(ix1,iy1,8,1)
            elseif(IVDOT(ICN,I,4).eq.1)then
	      call esymbol(ix1,iy1,32,1)
            else
	      call esymbol(ix1,iy1,32,1)
            endif
            call winscl('-',0)
            call forceflush()
  11      CONTINUE
        endif

C Do any arcs. Get pixel coords of centre and radius, then find the
C pixel radius (ir1) via call to crowxyz.
        if(NIVA(ICN).ge.1)then
          DO 9 I=1,NIVA(ICN)
            IS=IVARC(ICN,I,1)
            IR=IVARC(ICN,I,2)
            z1=0.0
            z2=0.0
            call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),ix1,iy1)
            call u2pixel(VCICON(ICN,IR,1),VCICON(ICN,IR,2),ix2,iy2)
            ir1 = nint(crowxyz(real(ix1),real(iy1),z1,real(ix2),
     &          real(iy2),z2))
            iang1=IVARC(ICN,I,3)
            iang2=IVARC(ICN,I,4)
            icol=IVARC(ICN,I,5)
            if(IVARC(ICN,I,6).eq.0)then
              call winscl('i',icol)
            elseif(IVARC(ICN,I,6).eq.1)then
              call winscl('g',icol)
            elseif(IVARC(ICN,I,6).eq.2)then
              call winscl('z',icol)
            endif
C debug...
C            write(6,*) ix1,iy1,is,ir,ir1,iang1,iang2,icol

C Draw unfilled or filled arc.
            if(IVARC(ICN,I,7).eq.0)then
	      call earc(ix1,iy1,ir1,iang1,iang2,0)
            else
	      call earc(ix1,iy1,ir1,iang1,iang2,1)
            endif
            call winscl('-',0)
            call forceflush()
   9      CONTINUE
        endif

C Do any internal labels in smallest font in the specified colour.
        DO 12 I=1,NIVL(ICN)
          IS=IVLBL(ICN,I,1)
          call u2pixel(VCICON(ICN,IS,1),VCICON(ICN,IS,2),ix1,iy1)
          icol=IVLBL(ICN,I,2)
          IF(IFS.GT.0)CALL winfnt(0)
          write(LBLINT,'(a)') NWICNLBL(ICN,I) 
          if(IVLBL(ICN,I,3).eq.0)then
            call winscl('i',icol)
            call textatxy(ix1,iy1,LBLINT,'i',icol)
          elseif(IVLBL(ICN,I,3).eq.1)then
            call winscl('g',icol)
            call textatxy(ix1,iy1,LBLINT,'g',icol)
          elseif(IVLBL(ICN,I,3).eq.2)then
            call winscl('z',icol)
            call textatxy(ix1,iy1,LBLINT,'z',icol)
          endif
          call winscl('-',0)
          IF(IFS.GT.0)CALL winfnt(IFS)
          call forceflush()
  12    CONTINUE
          
C Write on the label.
        WRITE(LABL,'(A12)') NWICNM(ICN)

C Also check that the label is within the view limits, write
C it in black and use a smaller font if the scale is less than 1.
        CALL U2PIXEL(X,Y-OFFLAB,IXL,IYL)
        if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(0)
        call textpixwidth(LABL,ipixw)
        ixlc = ixl - (ipixw/2)
        if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(IFS)
        IF(IXLC.GT.IGL.AND.IXR.LT.IGR.AND.IYU.GT.IGT
     &               .AND.IYL.LT.IGB)THEN
          if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(0)
          call textatxy(ixlc,iyl,LABL,'i',6)
          call winscl('-',0)
          if(SCALF.lt.1.0.and.IFS.GT.0)CALL winfnt(IFS)
        ENDIF
      ELSE
        INBOUND=.FALSE.
      ENDIF

C Draw on the connection points for the component (applies to all components)
      IF(INBOUND)THEN
        DO 100 I=1,NCONP(ICN)

C Move to first position.
C Set the drawing colour depending on the attachment type.
          icol=ICNCT(ICN,I)
          call winscl('z',icol)

          if(INWKTYP.eq.6)then

C If domain is primitive part draw a circle and setup ICPP array.
            CALL U2PIXEL(CONCP(ICN,I,1),CONCP(ICN,I,2),IX,IY)
            CALL ECIRC(IX,IY,2,1)
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
C            WRITE(6,*),CPP(1,III),III
          else

C Otherwise draw a square.
            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
     &                CONCP(ICN,I,2)-XYSIZCP/2.,0,0)
            CALL ETPLOT(CONCP(ICN,I,1)+XYSIZCP/2.,
     &                CONCP(ICN,I,2)-XYSIZCP/2.,1,0)
            CALL ETPLOT(CONCP(ICN,I,1)+XYSIZCP/2.,
     &                CONCP(ICN,I,2)+XYSIZCP/2.,1,0)
            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
     &                CONCP(ICN,I,2)+XYSIZCP/2.,1,0)
            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
     &                CONCP(ICN,I,2)+XYSIZCP/2.,1,0)
            CALL ETPLOT(CONCP(ICN,I,1)-XYSIZCP/2.,
     &                CONCP(ICN,I,2)-XYSIZCP/2.,1,0)
          endif

C Reset the line colour to standard.
          call winscl('-',0)
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
C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C Icon commons 
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH

      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C connection points property array
C      COMMON/CNP/CPP(3,MNCNN),ICPP(MICN,4,3),III

C connection points parameter array
C      COMMON/CPPA/ENV(5),PARA(MNCNN,13),CPARA(MICN,4,13),
C     &  SUB_MATRIX_A(MICN,MNCNN,MNCNN),SUB_MATRIX_B(MICN,MNCNN),
C     &  MATRIX_A(MNCNN,MNCNN),MATRIX_B(MNCNN),ICOUNTER,IDONE

      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      DIMENSION AL(2),AU(2),EP(3),SP(3),MP(3),PP(2),CP(2)
      REAL AL,AU,EP,SP,MP,PP,MIN1,MAX1,MIN2,MAX2

C      CHARACTER CPP*30,ICPP*30
C      REAL ENV,PARA,CPARA,SUB_MATRIX_A,SUB_MATRIX_B
C      REAL MATRIX_A,MATRIX_B

C Logical
      LOGICAL CLOSE,CLOSES,CLOSEE,CLOSEP,FOUND,ISEL,CSEL

      REAL PI

C Draw the connection, with a square at each intermediate point.

C Move to the start of the connection 
      CLOSES=.FALSE.

C Check if the view 'layer' is correct for the drawing of the point.
      MIN1=0.0
      MAX1=0.0
      MIN2=0.0
      MAX2=0.0

      IF(IVIEW.EQ.1)THEN
        SP(1)=CONCP(ICNS(ICON),ICNSP(ICON),1)
        SP(2)=CONCP(ICNS(ICON),ICNSP(ICON),2)
        SP(3)=XYZICON(ICNS(ICON),3)

C << do not check for Z so all are seen >>
C        CALL ECLOSE(SP(3),VIEWCEN(3),0.01,CLOSES)
        CLOSES=.true.
        MIN1=VIEWLIM(1)
        MAX1=VIEWLIM(2)
        MIN2=VIEWLIM(3)
        MAX2=VIEWLIM(4)
      ELSEIF(IVIEW.EQ.2)THEN
        SP(1)=CONCP(ICNS(ICON),ICNSP(ICON),1)
        SP(2)=XYZICON(ICNS(ICON),2)
        SP(3)=CONCP(ICNS(ICON),ICNSP(ICON),2)
        CALL ECLOSE(SP(3),VIEWCEN(2),0.01,CLOSES)
        MIN1=VIEWLIM(1)
        MAX1=VIEWLIM(2)
        MIN2=VIEWLIM(5)
        MAX2=VIEWLIM(6)
      ELSE
        SP(1)=XYZICON(ICNS(ICON),2)
        SP(2)=CONCP(ICNS(ICON),ICNSP(ICON),1)
        SP(3)=CONCP(ICNS(ICON),ICNSP(ICON),2)
        CALL ECLOSE(SP(3),VIEWCEN(1),0.01,CLOSES)
        MIN1=VIEWLIM(3)
        MAX1=VIEWLIM(4)
        MIN2=VIEWLIM(5)
        MAX2=VIEWLIM(6)
      ENDIF
      CALL U2PIXEL(SP(1),SP(2),IX,IY)

C If the layer is correct then check if the connection is within screen limits.
      IF(CLOSES)THEN
        IF(IX.GE.IGL.AND.IX.LE.IGR.AND.IY.GE.IGT.AND.IY.LE.IGB)THEN
          CLOSES=.TRUE.
        ELSE
          CLOSES=.FALSE.           
        ENDIF      
      ENDIF

C Move to the start of the connection.
C Set the drawing colour depending on the connection type.
      icol=ICNNT(ICON)
      call winscl('z',icol)

C Store the start point as the previous point and start drawing the
C connection. 
      PP(1)=SP(1)
      PP(2)=SP(2)
      call u2pixel(pp(1),pp(2),ixp,iyp)

      IF(CLOSES)THEN
        CLOSEP=.TRUE. 
      ELSE
        CLOSEP=.FALSE.
      ENDIF   
      DO 10 I=1,NCONWP(ICON)

C Get the pixels for the initial points
        CLOSE=.FALSE.
        IF(IVIEW.EQ.1)THEN
          MP(1)=CNWNP(ICON,I,1)
          MP(2)=CNWNP(ICON,I,2)

C << do not test for Z of waypoints so all are seen >>
C          CALL ECLOSE(CNWNP(ICON,I,3),VIEWCEN(3),0.01,CLOSE)
          CLOSE=.TRUE.
        ELSEIF(IVIEW.EQ.2)THEN
          MP(1)=CNWNP(ICON,I,1)
          MP(2)=CNWNP(ICON,I,3) 
          CALL ECLOSE(CNWNP(ICON,I,2),VIEWCEN(2),0.01,CLOSE)
        ELSE
          MP(1)=CNWNP(ICON,I,2)
          MP(2)=CNWNP(ICON,I,3) 
          CALL ECLOSE(CNWNP(ICON,I,1),VIEWCEN(1),0.01,CLOSE)
        ENDIF

C record the pixel values of the way point.
        call u2pixel(MP(1),MP(2),ix,iy)

        IF(IX.GE.IGL.AND.IX.LE.IGR.AND.IY.GE.IGT
     &.AND.IY.LE.IGB.AND.CLOSE.AND.CLOSEP)THEN

C If within the view limits then draw a line from the last point 
C to the current point.
           if(csel(icon))then
             call edwline(ixp,iyp,ix,iy)
           else
             call eswline(ixp,iyp,ix,iy)
           endif

C Get the pixel values of the square around the connection point. 
           call u2pixel(MP(1)+XYSIZCP/2.,MP(2)-XYSIZCP/2.,isx1,isy1)
           call u2pixel(MP(1)+XYSIZCP/2.,MP(2)+XYSIZCP/2.,isx2,isy2)
           call u2pixel(MP(1)-XYSIZCP/2.,MP(2)+XYSIZCP/2.,isx3,isy3)
           call u2pixel(MP(1)-XYSIZCP/2.,MP(2)-XYSIZCP/2.,isx4,isy4)
C Draw a square around the connection intermediate point.
           
           IF(CSEL(ICON))THEN
             CALL EDWLINE(isx1,isy1,isx2,isy2)
             CALL EDWLINE(isx2,isy2,isx3,isy3)
             CALL EDWLINE(isx3,isy3,isx4,isy4)
             CALL EDWLINE(isx4,isy4,isx1,isy1)
           ELSE
             CALL ESWLINE(isx1,isy1,isx2,isy2)
             CALL ESWLINE(isx2,isy2,isx3,isy3)
             CALL ESWLINE(isx3,isy3,isx4,isy4)
             CALL ESWLINE(isx4,isy4,isx1,isy1)
           ENDIF

          CLOSEP=.TRUE.

C Get the intersection between the line between the two points and the
C boundary of the domain. Move to the intersection point. 
        ELSEIF(IX.GT.IGL.AND.IX.LT.IGR.AND.IY.GT.IGT
     &.AND.IY.LT.IGB.AND.CLOSE.AND..NOT.CLOSEP)THEN
          CLOSEP=.TRUE.

C If the start of the connection was outwith screen limits then move to the
C intersection and draw the connection.
          CP(1)=MP(1)
          CP(2)=MP(2)
          CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
          call u2pixel(xi,yi,iix,iiy)

C Draw a square around the connection intermediate point.
c          if(found)then
            if(csel(icon))then 
              call edwline(iix,iiy,ix,iy)
            else
              call eswline(iix,iiy,ix,iy)
            endif
c          endif
C Get the pixel values of the square around the connection point. 
           call u2pixel(MP(1)+XYSIZCP/2.,MP(2)-XYSIZCP/2.,isx1,isy1)
           call u2pixel(MP(1)+XYSIZCP/2.,MP(2)+XYSIZCP/2.,isx2,isy2)
           call u2pixel(MP(1)-XYSIZCP/2.,MP(2)+XYSIZCP/2.,isx3,isy3)
           call u2pixel(MP(1)-XYSIZCP/2.,MP(2)-XYSIZCP/2.,isx4,isy4)
C Draw a square around the connection intermediate point.
           
           IF(CSEL(ICON))THEN
             CALL EDWLINE(isx1,isy1,isx2,isy2)
             CALL EDWLINE(isx2,isy2,isx3,isy3)
             CALL EDWLINE(isx3,isy3,isx4,isy4)
             CALL EDWLINE(isx4,isy4,isx1,isy1)
           ELSE
             CALL ESWLINE(isx1,isy1,isx2,isy2)
             CALL ESWLINE(isx2,isy2,isx3,isy3)
             CALL ESWLINE(isx3,isy3,isx4,isy4)
             CALL ESWLINE(isx4,isy4,isx1,isy1)
           ENDIF

           CLOSEP=.TRUE.
        ELSEIF(CLOSE.AND.CLOSEP)THEN

C If the end of the connection to the intermediate point is outwith the
C screen limits then draw a line to the intersection point from the
C previous point.
          CP(1)=MP(1)
          CP(2)=MP(2)
          CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
          call u2pixel(xi,yi,iix,iiy)
c          if(found)then
            if(csel(icon))then 
              call edwline(iix,iiy,ixp,iyp)
            else
              call eswline(iix,iiy,ixp,iyp)
            endif  
c          endif        
          CLOSEP=.FALSE.
        ELSE
          CLOSEP=.FALSE.
        ENDIF

C Store this way point as the previous way point
        PP(1)=MP(1)
        PP(2)=MP(2)
C get the pixel coordinates of the previous point
        call u2pixel(pp(1),pp(2),ixp,iyp) 

        
  10  CONTINUE

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

      call u2pixel(EP(1),EP(2),ix,iy)

      IF(IX.GT.IGL.AND.IX.LT.IGR.AND.IY.GT.IGT
     &.AND.IY.LT.IGB.AND.CLOSEE.AND.CLOSEP)THEN
        if(csel(icon))then 
          call edwline(ix,iy,ixp,iyp)
        else
          call eswline(ix,iy,ixp,iyp)
        endif 
        
        IF(NCONWP(ICON).GT.0)THEN
          IF(IVIEW.EQ.1)THEN
            X1=MP(1)
            Y1=MP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=MP(1)
            Y1=MP(3)
          ELSE
            X1=MP(2)
            Y1=MP(3)
          ENDIF
        ELSE 
          IF(IVIEW.EQ.1)THEN
            X1=SP(1)
            Y1=SP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=SP(1)
            Y1=SP(3) 
          ELSE
            X1=SP(2)
            Y1=SP(3)
          ENDIF          
        ENDIF 

        IF(IVIEW.EQ.1)THEN
          X2=EP(1)
          Y2=EP(2) 
        ELSEIF(IVIEW.EQ.2)THEN
          X2=EP(1)
          Y2=EP(3) 
        ELSE
          X2=EP(2)
          Y2=EP(3) 
        ENDIF  

C Draw the arrow.
        PI=22./7.
        DX=ABS(X2-X1)
        DY=ABS(Y2-Y1)
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

C Draw the arrow.
        call u2pixel(AL(1),AL(2),iax,iay)
        call u2pixel(AU(1),AU(2),iux,iuy)
        if(csel(icon))then 
          call edwline(iax,iay,ix,iy)
          call edwline(iux,iuy,ix,iy)
        else
          call eswline(iax,iay,ix,iy)
          call eswline(iux,iuy,ix,iy)
        endif
      ELSEIF(IX.GT.IGL.AND.IX.LT.IGR.AND.IY.GT.IGT
     &.AND.IY.LT.IGB.AND.CLOSEE.AND..NOT.CLOSEP)THEN
        CP(1)=EP(1)
        CP(2)=EP(2)
        CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
        call u2pixel(xi,yi,iix,iiy)
        if(csel(icon))then 
          call edwline(iix,iiy,ix,iy)
        else
          call eswline(iix,iiy,ix,iy)
        endif

        IF(NCONWP(ICON).GT.0)THEN
          IF(IVIEW.EQ.1)THEN
            X1=MP(1)
            Y1=MP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=MP(1)
            Y1=MP(3)
          ELSE
            X1=MP(2)
            Y1=MP(3)
          ENDIF
        ELSE 
          IF(IVIEW.EQ.1)THEN
            X1=SP(1)
            Y1=SP(2)
          ELSEIF(IVIEW.EQ.2)THEN
            X1=SP(1)
            Y1=SP(3) 
          ELSE
            X1=SP(2)
            Y1=SP(3)
          ENDIF          
        ENDIF 

        IF(IVIEW.EQ.1)THEN
          X2=EP(1)
          Y2=EP(2) 
        ELSEIF(IVIEW.EQ.2)THEN
          X2=EP(1)
          Y2=EP(3) 
        ELSE
          X2=EP(2)
          Y2=EP(3) 
        ENDIF  

C Draw the arrow.
        PI=22./7.
        DX=ABS(X2-X1)
        DY=ABS(Y2-Y1)
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

C Draw the arrow.
        call u2pixel(AL(1),AL(2),iax,iay)
        call u2pixel(AU(1),AU(2),iux,iuy)
        if(csel(icon))then 
          call edwline(iax,iay,ix,iy)
          call edwline(iux,iuy,ix,iy)
        else
          call eswline(iax,iay,ix,iy)
          call eswline(iux,iuy,ix,iy)
        endif
             
      ELSEIF(CLOSEE.AND.CLOSEP)THEN
        CP(1)=EP(1)
        CP(2)=EP(2)
        CALL CINTSCT(CP,PP,MIN1,MAX1,MIN2,MAX2,XI,YI,FOUND)
        call u2pixel(xi,yi,iix,iiy)
        if(csel(icon))then 
          call edwline(iix,iiy,ixp,iyp)
        else
          call eswline(iix,iiy,ixp,iyp)
        endif
      ELSE
        continue
      ENDIF
      
C Reset the line colour to standard.
      call winscl('-',0)
      RETURN
      END

C ************* imgdisp *************
C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end

C ********************* NETGDATR ************
C Each domain may contain global attributes such as the wind
C reduction factor of a flow network which relate to the network
C as a whole rather than to individual components. If there
C are such attributes a `global attributes` option is included
C in the main menu which calls this subroutine.
      SUBROUTINE NETGDATR(IER)

# include "epara.h"
# include "gnetwk.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

      COMMON/POPHELP/H(60)  

C CATSI: list of attribute categories (presented for selection).
C NCATIE: number of data fields in each attribute category.
C ICATIE: pointer from DATACATIE bookkeepping back to the 
C         NWKCON arrays.
      DIMENSION CATSI(MIATRC),NCATIE(MIATRC),ICATIE(MIATRC,MIATRB)

C DATCATIE: attribute editing array for the current category. 
C TAGCATIE: ajucent for DATCATIE of the global attribute tags. 
C IECDATE is menu array for attribute categories.
C IECDATEs is menu arrary for attribute data fields.
C CATIES is menu entry for each data field
      DIMENSION DATCATIE(MIATRC,MIATRB,3),IECDATE(MIATRC+6),
     &CATIES(MIATRC,MIATRB),IECDATEs(30),TAGCATIE(MIATRC,MIATRB,5)

      LOGICAL MATCH

      CHARACTER H*72,ddgtagatr*12,ddgmenuatr*32,ddgatrib*12

      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,key*1
      CHARACTER CATSI*12,IECDATE*34,CATIES*32,
     &IECDATEs*49,DATCATIE*12,TAGCATIE*12,t32*32


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

C Build the menu commands. First present a list of global
C attribute categories.
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
      CALL EMENU('Global attribute categories',IECDATE,
     &  NECDATA,ICOUT1)
      IF(ICOUT1.EQ.NECDATA)THEN
        RETURN
      ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
        H(1)='This menu displays the categoties of global data which'
        H(2)='can be edited for this domain/network.'
        H(3)='Please select a category to contine or'
        H(4)='exit to the previous menu.'
        CALL PHELPD('Data categories',4,'-',0,0,IER)
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

C << This could be reminder of which network type. >>
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
        iwla=MIN0(30,iwla)
        iwa=(iwla+iwlb+7)
        iw=MAX0(33,iwa)
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
        WRITE(IECDATEs(M+2),'(A)')'  cannot edit this value (#)'
        WRITE(IECDATEs(M+3),'(A)')'? help '
        WRITE(IECDATEs(M+4),'(A)')'- exit this menu '
        NECDATAs=M+MCTL
        ICOUT2=-1
        CALL NETWDRW

C Make menu only as wide as it needs to be.
        if(MMOD.eq.8)then
          CALL VWMENU('Global attributes',IECDATEs,NECDATAs,
     &      0,0,iw,irpx,irpy,ICOUT2)
        else
          CALL EMENU('Global attributes',IECDATEs,NECDATAs,
     &      ICOUT2)
        endif
        IF(ICOUT2.EQ.NECDATAs)THEN

C Restore category menu setup before returning to label 705.
          CALL EPMENRC
          GOTO 705
        ELSEIF(ICOUT2.EQ.NECDATAs-1)THEN
          h(1)='This list displays the attributes for this'
          h(2)='category; click on the data item to edit its  '
          h(3)='value.'
          CALL PHELPD('Data values',3,'-',0,0,IER)
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
            read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)IVMIN
            read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)IVMAX
            read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVAL
            read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVALD
            IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
              t32=CATIES(IAST,ISL)
              CALL EASKI(IVAL,t32,' ',IVMIN,'W',
     &          IVMAX,'W',IVALD,'glob atrib intg data',IER,0)
              if(ier.eq.0)then
                write(istr,'(I8)') IVAL
                call removepad(istr,inpstr,ilen,iflag)
                write(DATCATIE(IAST,ISL,1),'(A)') inpstr
                write(ddgatrib(ICATIE(IAST,ISL),1),'(A)') inpstr
              endif
            ENDIF
          ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'real')THEN

C First read DATCATIE strings into VMIN/VMAX/VAL real variables, then edit
C the variable and then write it back to the string VSTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
            read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)VMIN
            read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)VMAX
            read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VAL
            read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VALD

            IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
              h(1)='A real number is expected here.'
              t32=CATIES(IAST,ISL)
              CALL EASKR(VAL,t32,' ',VMIN,'W',
     &          VMAX,'W',VALD,'atrib real data',IER,1)
              if(ier.eq.0)then
                call relstr(val,vnpstr,ilen,iflag)
                WRITE(DATCATIE(IAST,ISL,1),'(a)') vnpstr
                WRITE(ddgatrib(ICATIE(IAST,ISL),1),'(a)') vnpstr
              endif
            ENDIF
          ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'text')THEN
            VSTR=DATCATIE(IAST,ISL,1)
            IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
              t32=CATIES(IAST,ISL)
              CALL EASKS(VSTR,t32,' ',12,'nothing',
     &          'atrib text data',IER,0)
              if(ier.eq.0.and.VSTR(1:2).ne.'  ')then
                call removepad(vstr,vnpstr,ilen,iflag)
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
  99  CALL EDISP(IUOUT,'Error in string formatting NETGDATR')
      END

C ********** selrelexttag
C Select a real value based on external information held in 
C a .summary file for the current model. Up to 6 real values
C are returned. If ier=-1 then summary file was not available.
      subroutine selrelexttag(exttag,AVAL,SVAL,ier)
#include "net_flow.h"
#include "building.h"
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/MFLDOC/DEPRE(MPRD)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC2/VOL(MCOM)

C Summary of external data.
      common/exsum/isexavail,iuex

      dimension AVAL(6),SVAL(6),zx(MCOM),zy(MCOM),zz(MCOM)

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
      CHARACTER H*72,outstr*124,DEPRE*40,t72*72,zname*12,zdesc*64
      CHARACTER PCITM(35)*42,KEY*1,exttag*12,WORD*20,SVAL*12,conname*43
      CHARACTER PSITM(35)*60
      character snamecon*12,znamecon*12,outs*124,tl*18,tc*18,tt*18
      LOGICAL OK,DOK,isexavail

C Rewind summary file if it exists (assumed to be open on unit iuex).
      if(isexavail)then
        ier=0
        REWIND(iuex,ERR=999)
        CALL STRIPC(iuex,OUTSTR,99,ND,1,'synopsis',IER)
      else
        ier=-1
        return
      endif

C Clear return array. 
      AVAL(1)=0.00
      AVAL(2)=0.00
      AVAL(3)=0.00
      AVAL(4)=0.00
      AVAL(5)=0.00
      AVAL(6)=0.00
      SVAL(1)='  '
      SVAL(2)='  '
      SVAL(3)='  '
      SVAL(4)='  '
      SVAL(5)='  '
      SVAL(6)='  '
      
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
          call USRMSG('Select the pressure coefficient set which',
     &'best matches this surface. (see help) ','-') 
          CALL EMENU('Pressure Coefficient Sets:',PCITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN

C User exits without making a selection.
            ier= -1
          ELSEIF(IVERT.EQ.NITMS-1)THEN
            H(1)='To enable the calculation of wind-induced surface'
            H(2)='pressures (as required by ESP-r`s fluid flow'
            H(3)='simulators), a database of angle dependent'
            H(4)='pressure coefficients is provided.'
            H(5)='The default ESP-r set contains wind pressure coeffi-'
            H(6)='cients for surfaces in typical positions and within '
            H(7)='several different exposure categories.'
            H(8)='They can be used (with care) for low-rise buildings.'
            H(9)='See the ESP manual for additional info.'
            h(10)=' '
            h(11)='It is also possible to calculate pressure coeffi-' 
            h(12)='cients using CpCalc (see database maintenance).'
            h(13)=' '
            h(14)='Select the pressure coefficient from this list, ' 
            h(15)='which is most appropriate for the location of the '
            h(16)='external node.'
            CALL PHELPD('databases overview',16,'-',0,0,IER)
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
            CALL EGETW(OUTSTR,K,zname(inz),'W','zone name',IFLAG)
            CALL EGETWR(OUTSTR,K,VOL(inz),0.,0.,'-','zone vol',IER)
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
              WRITE(PCITM(M),'(A1,1X,A,2F8.2)')KEY,zname(IM),vol(im),
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
          PCITM(M+4)='- exit (without selecting)'
          IVERT=-1

C Now display the menu.
          call USRMSG('Select a zone from the model and its volume',
     &'and height can be used. (see help) ','-') 
          CALL EMENU('Available zones in model:',PCITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN

C User exits without making a selection.
            ier= -1
          ELSEIF(IVERT.EQ.NITMS-1)THEN
            h(1)='Each thermal zone has attributes such as name,'
            h(2)='volume and a centre of gravity which can be used'
            h(3)='by a flow network component.'
            CALL PHELPD('external overview',3,'-',0,0,IER)
            GOTO 71
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
          h(1)='The zone attributes are volume and location (centre)'
          h(2)='and these can be taken from the model rather than '
          h(3)='typing them in manually. '
          write(t72,'(a,a)')'Use ',zname(ifoc)
          CALL ASKOK(t72,'attributes with this component?',OK,dok,3)
          IF(.NOT.OK)GOTO 71
          AVAL(1)=VOL(ifoc)
          AVAL(2)=zx(ifoc)
          AVAL(3)=zy(ifoc)
          AVAL(4)=zz(ifoc)
          SVAL(1)=zname(ifoc)
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
          inc=0
          CALL EGETWI(OUTSTR,K,NC,1,MCON,'W','nb of conn',IER)
 243      CALL STRIPC(iuex,OUTSTR,99,ND,1,'surface stuff',IER)
          if(outstr(1:13).eq.'*End_Surfaces')then
            continue
          else
            inc=inc+1
            K=0
            CALL EGETP(OUTSTR,K,conname(inc),'W','conn descr',IFLAG)
            CALL EGETW(OUTSTR,K,znamecon(inc),'W','conn zn name',IFLAG)
            CALL EGETW(OUTSTR,K,snamecon(inc),'W','conn sf name',IFLAG)
            CALL EGETWR(OUTSTR,K,snacon(inc),0.,0.,'-','surf area',IER)
            CALL EGETWR(OUTSTR,K,surazi(inc),0.,0.,'-','surf azim',IER)
            CALL EGETWR(OUTSTR,K,surelv(inc),0.,0.,'-','surf elev',IER)
            CALL EGETWR(OUTSTR,K,surzmin(inc),0.,0.,'-','surf Zmin',IER)
            CALL EGETWR(OUTSTR,K,surzcog(inc),0.,0.,'-','surf Zcog',IER)
            CALL EGETWR(OUTSTR,K,surzmax(inc),0.,0.,'-','surf Zmax',IER)
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
      PSITM(1)=' Connection                | area  | azim | elev | COG '
      PSITM(2)='                           |  (m^3)| (deg)| (deg)  Z(m)'
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
          call USRMSG('Select a surface from the model and its area',
     &'and orientation can be used. (see help) ','-') 
          CALL EMENU('Available surfaces in model:',PSITM,NITMS,IVERT)
          IF(IVERT.EQ.NITMS)THEN

C User exits without making a selection.
            ier= -1
          ELSEIF(IVERT.EQ.NITMS-1)THEN
            h(1)='Each surface has attributes such as name, area'
            h(2)='orientation and a centre of gravity which can be used'
            h(3)='by a flow network component.'
            CALL PHELPD('external overview',3,'-',0,0,IER)
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
          h(1)='The options listed would place the component at the'
          h(2)='lowest point of the surface, the COG or the top point'
          h(3)='of the surface.'
          write(tl,'(a,f7.3,a)') 'low (',surzmin(ifoc),'m)'
          write(tc,'(a,f7.3,a)') 'cog (',surzcog(ifoc),'m)'
          write(tt,'(a,f7.3,a)') 'top (',surzmax(ifoc),'m)'
          write(outs,'(3a)') 'Use ',
     &      conname(ifoc)(1:lnblnk(conname(ifoc))),' attributes with'
          call easkatog(outs,'options:',tl,tc,tt,'other','continue',
     &      ' ',' ',IRT,3)
          IF(irt.eq.5)GOTO 72
          AVAL(1)=snacon(ifoc)
          AVAL(2)=surazi(ifoc)
          AVAL(3)=surelv(ifoc)
          if(irt.eq.1)then
            AVAL(4)=surzmin(ifoc)
          elseif(irt.eq.2)then
            AVAL(4)=surzcog(ifoc)
          elseif(irt.eq.3)then
            AVAL(4)=surzmax(ifoc)
          elseif(irt.eq.4)then
            xh=surzcog(ifoc)
            h(1)='The component height is in model coordinates.'
            CALL EASKR(xh,'Component height (m):',' ',-9.0,'W',99.0,'W',
     &        1.0,'component height',IER,1)
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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC2/VOL(MCOM)

C Summary of external data.
      common/exsum/isexavail,iuex

      dimension zx(MCOM),zy(MCOM),zz(MCOM)
      CHARACTER outstr*124,zname*12,zdesc*64
      CHARACTER exttag*12,WORD*20,outs*124
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
          call edisp(iuout,' Model zone names, volume, height')
  43      CALL STRIPC(iuex,OUTSTR,99,ND,1,'zone stuff',IER)
          if(outstr(1:10).eq.'*End_Zones')then
            continue
          else
            inz=inz+1
            K=0
            CALL EGETW(OUTSTR,K,zname(inz),'W','zone name',IFLAG)
            CALL EGETWR(OUTSTR,K,VOL(inz),0.,0.,'-','zn vol',IER)
            CALL EGETWR(OUTSTR,K,zx(inz),0.,0.,'-','zn cog X',IER)
            CALL EGETWR(OUTSTR,K,zy(inz),0.,0.,'-','zn cog Y',IER)
            CALL EGETWR(OUTSTR,K,zz(inz),0.,0.,'-','zn cog Z',IER)
            write(outs,'(4X,A,2F8.2)') zname(inz),vol(inz),zz(inz)
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
