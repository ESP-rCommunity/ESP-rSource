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
C This file contains the following routines:

C GRDSRF is the main controller for surfaces discretization. It 
C        displays the main menu for surfaces.
C DRWSRF draws a surface with it's associated gridding lines.
C SRFBND defines the boundaries of a surface.
C DFTGRD creates the default gridding for a surface.
C SRFIND defines the surfaces indentation from inside.
C HRZGRD creates a horrizontal grid line in a surface.
C VRTGRD creates a vertical grid line in a surface.
C DELINE deletes last gridding line.
C VRTNUM defines the 3D vertex number from its X and Z coordinates.
C CKGRDL checks if the gridding line does exist.

C GRDEDG is the main controller for edges discretization. It 
C        displays the main menu for edges.
C DRWEDG draws the selected edge after checking it's existance. It 
C        also draws the gridding in the local z direction.
C CONEDG displays the edge construction menu.
C FHCEDG draws the free hand sketch for the cross section of a given 
C        edge with it's gridding lines.
C TOTEDG determines the total number of edges in the zone.
C INDEDG Initializes the edges gridding.
C EDGNUM defines the number of the edge in a zone from it's associated
C        two surfaces.
C EDGCRT creates new edges or appends new edge to existing one.

C GRDCRN is the main controller for corners discretization. It 
C        displays the main menu for corners.
C DRWCRN draws the selected corner after checking it's existance. It 
C        also draws the gridding in the local z direction.
C CONCRN displays the corner construction menu.
C FHCCRN draws the free hand sketch for the cross section of a given 
C        corner with it's gridding lines.
C INDCRN initializes the corners dimensions.
C CRNNUM defines the number of the corner in a zone from it's associated
C        two surfaces. If the corner does not exist a new one will be 
C        created.
C TOTCRN determines the total number of corners in the zone.
C *********************************************************************

C *********************************************************************
C GRDSRF is the main controller for surfaces discretization. It 
C displays the main menu for surfaces.
C *********************************************************************
      SUBROUTINE GRDSRF(IZ,IDRW1,IDRW2)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PREC8/SLAT,SLON

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D10/XMINS(MS),XMAXS(MS),ZMINS(MS),ZMAXS(MS)

      LOGICAL SRFOK

      DIMENSION ITEM(12),IVALS(MS)
      CHARACTER SSRF*12,ITEMS(MS)*17,CORDNT*24
      CHARACTER ITEM*28,SZN*12,STTS*5
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='GRDSRF'  ! set for subroutine

      SZN=zname(IZ)
      SSRF=' '
      SRFOK=.FALSE.
      IDRW1=2
      IDRW2=IZ
      IS=1
   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'  zone    : ',SZN
      WRITE(ITEM(2),'(A,A)')'  surface : ',SSRF
      IF(SRFOK.AND.INDXS(IS).EQ.1)THEN
        ITEM(3)='  status  : lumped'
      ELSEIF(SRFOK.AND.INDXS(IS).EQ.3)THEN
        ITEM(3)='  status  : discritized'
      ELSE
        ITEM(3)='  status  : '
      ENDIF
      ITEM(4)='  --------------------------'
      ITEM(5)='1 select the surface        '
      ITEM(6)='2 add H-gridding line       '
      ITEM(7)='3 add V-gridding line       '
      ITEM(8)='4 clear last gridding line  '
      ITEM(9)='5 change status             '
      ITEM(10)='  --------------------------'
      ITEM(11)='? help                      '
      ITEM(12)='- exit                      '
      MITEM=12

C Help text for this menu.
      helptopic='grd_surf_grid_ctl'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If user has defined problem and perhaps resized the display then
C redraw the problem image.
      IF(MODIFYVIEW)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('     Surface Gridding',ITEM,MITEM,INO)

      IF(.NOT.SRFOK.AND.INO.GE.6.AND.INO.LE.10)THEN
        CALL USRMSG(' ',' the surface should be selected first.','W')
      ELSEIF(SRFOK.AND.INDXS(IS).NE.3.AND.INO.GE.6.AND.INO.LE.8)THEN
        CALL USRMSG(' ',' the current surface is lumped..','W')
      ELSEIF(INO.EQ.5)THEN

C Select the surface.
        IF(IDRW1.NE.2.OR.IDRW2.NE.IZ.OR.MODIFYVIEW)THEN
          IDRW1=2
          IDRW2=IZ
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF

C Show the surface selection menu.
        DO 50 ISS=1,NSUR
          IF(INDXS(ISS).EQ.1)THEN
            STTS='( L )'
          ELSEIF(INDXS(ISS).EQ.3)THEN
            STTS='( D )'
          ENDIF
          icon=IZSTOCN(iz,iss)
          WRITE(ITEMS(ISS),'(A12,A5)')SSNAME(icon),STTS
   50   CONTINUE
        INPICK=1
   60   CALL EPICKS(INPICK,IVALS,' ',' ',17,
     &    NSUR,ITEMS,'SELECT SRF.',IER,nbhelp)
        IS=IVALS(1)
        IF(IS.EQ.0)THEN
          SSRF=' '
          SRFOK=.FALSE.
        ELSEIF(IS.GT.0.AND.IS.LE.NSUR)THEN
          icon=IZSTOCN(iz,is)
          SSRF=SSNAME(icon)
          SRFOK=.TRUE.
          CALL TRNSF4(IS)
          XMIN=XMINS(IS)+1.E-3
          XMAX=XMAXS(IS)-1.E-3
          ZMIN=ZMINS(IS)+1.E-3
          ZMAX=ZMAXS(IS)-1.E-3
          XVALU=(XMIN+XMAX)/2.
          ZVALU=(ZMIN+ZMAX)/2.

C Draw the elevation view for the selected surface.
          IDRW1=3
          IDRW2=IS
          CALL GRAAPH(IDRW1,IDRW2)
        ELSE
          GOTO 60
        ENDIF
      ELSEIF(INO.EQ.6)THEN

C Add horrizontal gridding line.
        WRITE(CORDNT,'(1X,2F9.2)')XVALU,ZVALU
        CALL EASKS(CORDNT,' ','Define the x- and z- coordinates:',
     &    24,' ',' coordinates ',IER,nbhelp)
        NCRD = iCountWords(CORDNT)
        IF(NCRD.EQ.2)THEN
          K=0
          CALL EGETWR(CORDNT,K,XVALU,XMIN+1.E-2,XMAX-1.E-2,
     &                'F','x-value',IER)
          IF(IER.NE.0)GOTO 10
          CALL EGETWR(CORDNT,K,ZVALU,ZMIN+1.E-2,ZMAX-1.E-2,
     &                'F','z-value',IER)
          IF(IER.NE.0)GOTO 10
          CALL HRZGRD(IS,XVALU,ZVALU)
        ELSE
          CALL USRMSG(' ',' two coordinates are required.','W')
        ENDIF
      ELSEIF(INO.EQ.7)THEN

C  Add vertical gridding line.
        WRITE(CORDNT,'(1X,2F9.2)')XVALU,ZVALU
        CALL EASKS(CORDNT,' ','Define the x- and z- coordinates:',
     &    24,' ',' coordinates ',IER,nbhelp)
        NCRD = iCountWords(CORDNT)
        IF(NCRD.EQ.2)THEN
          K=0
          CALL EGETWR(CORDNT,K,XVALU,XMIN+1.E-2,XMAX-1.E-2,
     &                'F','x-value',IER)
          IF(IER.NE.0)GOTO 10
          CALL EGETWR(CORDNT,K,ZVALU,ZMIN+1.E-2,ZMAX-1.E-2,
     &                'F','z-value',IER)
          IF(IER.NE.0)GOTO 10
          CALL VRTGRD(IS,XVALU,ZVALU)
        ELSE
          CALL USRMSG(' ',' two coordinates are required.','W')
        ENDIF
      ELSEIF(INO.EQ.8)THEN

C Clear last gridding line 
        IF(INDXS(IS).EQ.3)THEN
          IF(NLINA(IS).GT.NLIND(IS).OR.MODIFYVIEW)THEN
            CALL DELINE(IS)

C Draw the elevation view for the selected surface.
            IDRW1=3
            IDRW2=IS
            CALL GRAAPH(IDRW1,IDRW2)
          ELSE
            CALL USRMSG(' ',
     &          ' no additional gridding lines are available.','W')
          ENDIF
        ELSE
          CALL USRMSG(' ',' the current surface is lumped.','W')
        ENDIF
      ELSEIF(INO.EQ.9)THEN

C Change the surface status.
          IF(INDXS(IS).EQ.1)THEN
            INDXS(IS)=3
          ELSEIF(INDXS(IS).EQ.3)THEN
            INDXS(IS)=1
          ENDIF

C Draw the elevation view for the selected surface.
          IDRW1=3
          IDRW2=IS
          CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        helptopic='grd_surf_grid_ctl'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('surf grid',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Exit program.
        RETURN
      ENDIF
      GOTO 10
      END

C *****************************  DRWSRF  ******************************
C DRWSRF draws a surface with it's associated gridding lines.
C *********************************************************************
      SUBROUTINE DRWSRF(IS)
#include "building.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D10/XMINS(MS),XMAXS(MS),ZMINS(MS),ZMAXS(MS)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym,iix,iiy    ! passed to etplot u2pixel
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym,iix,iiy    ! passed to etplot u2pixel
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Define the  MAX & MIN surface coordinates.
      XMAX=XMAXS(IS)
      ZMAX=ZMAXS(IS)
      ZMIN=ZMINS(IS)
      XMIN=XMINS(IS)

C Clear the vieweing box, draw its border and return the offsets
C which define it.  The win3d parameters match those in FACDRW
C and SITPLN.
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=29; iiw2=7; iiw3=4; iiw4=3; iimenu=29
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Reopen the dialogue box.
      CALL usrmsg(' ',' ','-')

C Determine scaling ratios for the drawing and axis.
      call axiscale(igwid,igheight,XMIN,XMAX,ZMIN,ZMAX,xsc,ysc,sca,
     &              Xadd,Zadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(iigl,Xadd,sca,iigb,Zadd,sca)

C Display header information.
      iside=1
      isize=1
      ifont=2
      call viewtext('Elevation View',iside,isize,ifont)

C Draw a scale horizontal axis, move to beginning point.
      CALL horaxis(XMIN,XMAX,iigl,iigr,iigb,Xadd,sca,0,'X axis ->')

C Draw a scale vertical axis.
      CALL vrtaxis(ZMIN,ZMAX,iigl,iigb,iigt,Zadd,sca,0,0,'^ Z axis')
      DO 20 IL=1,NLINB(IS)
        IV1=ILINE(IS,IL,1)
        IV2=ILINE(IS,IL,2)
        X1=XVRT(IS,IV1)
        Z1=ZVRT(IS,IV1)
        X2=XVRT(IS,IV2)
        Z2=ZVRT(IS,IV2)
        IPEN=1
        iupdown=0
        isym=0
        CALL ETPLOT(X1,Z1,iupdown,isym)
        iupdown=IPEN
        CALL ETPLOT(X2,Z2,iupdown,isym)
        CALL U2PIXEL(X1,Z1,iix,iiy)
        CALL ECIRC(iix,iiy,3,1)
        IVRT1=ILINB(IS,IL,1)
        CALL VERTLBL(iix,iiy,IVRT1,IER)
   20 CONTINUE
      IF(INDXS(IS).EQ.3)THEN
        DO 30 IL=NLINB(IS)+1,NLINA(IS)
          IV1=ILINE(IS,IL,1)
          IV2=ILINE(IS,IL,2)
          X1=XVRT(IS,IV1)
          Z1=ZVRT(IS,IV1)
          X2=XVRT(IS,IV2)
          Z2=ZVRT(IS,IV2)
          IPEN=-204
          iupdown=0
          isym=0
          CALL ETPLOT(X1,Z1,iupdown,isym)
          iupdown=IPEN
          CALL ETPLOT(X2,Z2,iupdown,isym)
   30   CONTINUE
      ENDIF
      call forceflush()

      RETURN
      END

C *****************************  SRFBND  ******************************
C SRFBND defines the boundary of a surface.
C *********************************************************************
      SUBROUTINE SRFBND(IS)
#include "building.h"
#include "geometry.h"

      COMMON/G1T/XFT(MV),ZFT(MV)

      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)

      DIMENSION IBND(MLS,2)

      IN=0
      NVR=NVER(IS)

C Determine the boundary lines (excluding the the linking lines between 
C the outer and inner boundaries.
      DO 10 I=1,NVR
        IV1=JVN(IS,I)
        IF(I.LT.NVR)THEN
          J=I+1
        ELSE
          J=1
        ENDIF
        IV2=JVN(IS,J)
        DO 20 II=1,NVR
          IV3=JVN(IS,II)
          IF(II.LT.NVR)THEN
            IJ=II+1
          ELSE
            IJ=1
          ENDIF
          IV4=JVN(IS,IJ)

C Bypass repeated line segments.
          IF(IV4.EQ.IV1.AND.IV3.EQ.IV2)GOTO 10
   20   CONTINUE
        IN=IN+1
        IBND(IN,1)=I
        IBND(IN,2)=J
   10 CONTINUE
      NBND=IN

C Fill up the surface gridding variables.
      NLINB(IS)=NBND
      NLIND(IS)=NBND
      NLINA(IS)=NBND
      NVRTA(IS)=0
      DO 100 I=1,NBND
        XVALU=XFT(IBND(I,1))
        ZVALU=ZFT(IBND(I,1))
        ILINB(IS,I,1)=JVN(IS,IBND(I,1))
        CALL VRTNUM(IS,XVALU,ZVALU,NUM)
        ILINE(IS,I,1)=NUM
        XVALU=XFT(IBND(I,2))
        ZVALU=ZFT(IBND(I,2))
        ILINB(IS,I,2)=JVN(IS,IBND(I,2))
        CALL VRTNUM(IS,XVALU,ZVALU,NUM)
        ILINE(IS,I,2)=NUM
  100 CONTINUE
      NVRTB(IS)=NVRTA(IS)
      NVRTD(IS)=NVRTA(IS)
      RETURN
      END

C *******************************  DFTGRD  ****************************
C DFTGRD generates the default gridding for a surface.
C *********************************************************************
      SUBROUTINE DFTGRD(IS)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)

      DIMENSION TH2D1(MLS),TH2D2(MLS)

      NLIND(IS)=NLINB(IS)
      NLINA(IS)=NLINB(IS)

C For each corner define the two lines comprising it and determine
C there angle from the +ve X-axis (counter clockwise).
      DO 10 I=1,NLINB(IS)
        IV11=ILINE(IS,I,1)
        IV12=ILINE(IS,I,2)
        DO 20 J=1,NLINB(IS)
          IV21=ILINE(IS,J,1)
          IF(IV12.EQ.IV21)THEN
            IV22=ILINE(IS,J,2)
            CALL ANGL2P(IS,IV12,IV11,TH2D1(I))
            CALL ANGL2P(IS,IV21,IV22,TH2D2(I))
            GOTO 10
          ENDIF
   20   CONTINUE
   10 CONTINUE


C for each corner define the default gridding required.
      DO 30 I=1,NLINB(IS)

C Bypass if the angle is 180.
        IF(ABS(ABS(TH2D1(I)-TH2D2(I))-180.0).LT.10.)GOTO 30

C TH2D1 should be larger than TH2D2 for counter clockwise search.
        IF(TH2D1(I).LT.TH2D2(I))TH2D1(I)=TH2D1(I)+360.0

C Only vertical and horizontal gridding lines will be generated.
        THET=AINT((TH2D2(I)+1.)/90.0)*90.0

C Define the corner.
        IVCRN=ILINE(IS,I,2)
        XCRN=XVRT(IS,IVCRN)
        ZCRN=ZVRT(IS,IVCRN)

C Check if a gridding line is required at 90, 180, 270, and 360 
C degrees from +ve x-axis.
        DO 40 II=1,4
          THET=THET+90.0
          IF(ABS(TH2D1(I)-THET).LT.10.)GOTO 30

C Check for the 90 degrees case.
          IF(ABS(THET-90.0).LT.1.0.OR.ABS(THET-450.0).LT.1.0)THEN
            DZMIN=1.E+10

C Determine the intersection point between a gridding line from the
C current angle to all existing boundary lines.
            DO 50 I50=1,NLINB(IS)
              IB1=ILINE(IS,I50,1)
              IB2=ILINE(IS,I50,2)
              XB1=XVRT(IS,IB1)
              ZB1=ZVRT(IS,IB1)
              XB2=XVRT(IS,IB2)
              ZB2=ZVRT(IS,IB2)

C Bypass if the current boundary line is one of the two lines forming 
C the angle.
              IF(IB1.NE.IVCRN.AND.IB2.NE.IVCRN.AND.

C Bypass vertical boundary lines since they will not intersect with 90
C gridding lines.
     &          ABS(XB2-XB1).GT.1.E-2)THEN

C The x-coordinate for the intersection point is known.
C Determine the z-coordinate.
                ZINT=(XCRN-XB1)/(XB2-XB1)*(ZB2-ZB1)+ZB1
                XMX=AMAX1(XB1,XB2)+1.0E-6
                XMN=AMIN1(XB1,XB2)-1.0E-6
                ZMX=AMAX1(ZB1,ZB2)+1.0E-6
                ZMN=AMIN1(ZB1,ZB2)-1.0E-6

C Check if the intersection point is within the two lines (not their
C extensions).
                IF(XCRN.GT.XMN.AND.XCRN.LT.XMX.AND.ZINT.GT.ZMN.AND.
     &             ZINT.LT.ZMX)THEN
C The required gridding line is the shortest.
                  IF(ZINT.GT.ZCRN.AND.(ZINT-ZCRN).LT.DZMIN)THEN
                    ZMIN=ZINT
                    DZMIN=(ZINT-ZCRN)
                  ENDIF
                ENDIF
              ENDIF
  50        CONTINUE

C Check if this gridding line does not already exists
            XMIN=XCRN
            CALL CKGRDL(IS,XCRN,ZCRN,XMIN,ZMIN,NUM)
          ELSEIF(ABS(THET-180.0).LE.1.0.OR.ABS(THET-540.0).LE.1.0)THEN
            DXMIN=1.E+10
            DO 70 I70=1,NLINB(IS)
              IB1=ILINE(IS,I70,1)
              IB2=ILINE(IS,I70,2)
              XB1=XVRT(IS,IB1)
              ZB1=ZVRT(IS,IB1)
              XB2=XVRT(IS,IB2)
              ZB2=ZVRT(IS,IB2)
              IF(IB1.NE.IVCRN.AND.IB2.NE.IVCRN.AND.
     &          ABS(ZB2-ZB1).GT.1.E-2)THEN
                XINT=(ZCRN-ZB1)/(ZB2-ZB1)*(XB2-XB1)+XB1
                XMX=AMAX1(XB1,XB2)+1.0E-6
                XMN=AMIN1(XB1,XB2)-1.0E-6
                ZMX=AMAX1(ZB1,ZB2)+1.0E-6
                ZMN=AMIN1(ZB1,ZB2)-1.0E-6
                IF(XINT.GT.XMN.AND.XINT.LT.XMX.AND.ZCRN.GT.ZMN.AND.
     &             ZCRN.LT.ZMX)THEN
                  IF(XCRN.GT.XINT.AND.(XCRN-XINT).LT.DXMIN)THEN
                    XMIN=XINT
                    DXMIN=(XCRN-XINT)
                  ENDIF
                ENDIF
              ENDIF
  70        CONTINUE
            ZMIN=ZCRN
            CALL CKGRDL(IS,XCRN,ZCRN,XMIN,ZMIN,NUM)
          ELSEIF(ABS(THET-270.0).LE.1.0.OR.ABS(THET-630.0).LE.1.0)THEN
            DZMIN=1.E+10
            DO 90 I90=1,NLINB(IS)
              IB1=ILINE(IS,I90,1)
              IB2=ILINE(IS,I90,2)
              XB1=XVRT(IS,IB1)
              ZB1=ZVRT(IS,IB1)
              XB2=XVRT(IS,IB2)
              ZB2=ZVRT(IS,IB2)
              IF(IB1.NE.IVCRN.AND.IB2.NE.IVCRN.AND.
     &          ABS(XB2-XB1).GT.1.E-2)THEN
                ZINT=(XCRN-XB1)/(XB2-XB1)*(ZB2-ZB1)+ZB1
                XMX=AMAX1(XB1,XB2)+1.0E-6
                XMN=AMIN1(XB1,XB2)-1.0E-6
                ZMX=AMAX1(ZB1,ZB2)+1.0E-6
                ZMN=AMIN1(ZB1,ZB2)-1.0E-6
                IF(XCRN.GT.XMN.AND.XCRN.LT.XMX.AND.ZINT.GT.ZMN.AND.
     &             ZINT.LT.ZMX)THEN
                  IF(ZCRN.GT.ZINT.AND.(ZCRN-ZINT).LT.DZMIN)THEN
                    ZMIN=ZINT
                    DZMIN=(ZCRN-ZINT)
                  ENDIF
                ENDIF
              ENDIF
   90       CONTINUE
            XMIN=XCRN
            CALL CKGRDL(IS,XCRN,ZCRN,XMIN,ZMIN,NUM)
          ELSEIF(ABS(THET-360.0).LE.1.0.OR.ABS(THET-720.0).LE.1.0)THEN
            DXMIN=1.E+10
            DO 110 I110=1,NLINB(IS)
              IB1=ILINE(IS,I110,1)
              IB2=ILINE(IS,I110,2)
              XB1=XVRT(IS,IB1)
              ZB1=ZVRT(IS,IB1)
              XB2=XVRT(IS,IB2)
              ZB2=ZVRT(IS,IB2)
              IF(IB1.NE.IVCRN.AND.IB2.NE.IVCRN.AND.
     &          ABS(ZB2-ZB1).GT.1.E-2)THEN
                XINT=(ZCRN-ZB1)/(ZB2-ZB1)*(XB2-XB1)+XB1
                XMX=AMAX1(XB1,XB2)+1.0E-6
                XMN=AMIN1(XB1,XB2)-1.0E-6
                ZMX=AMAX1(ZB1,ZB2)+1.0E-6
                ZMN=AMIN1(ZB1,ZB2)-1.0E-6
                IF(XINT.GT.XMN.AND.XINT.LT.XMX.AND.ZCRN.GT.ZMN.AND.
     &             ZCRN.LT.ZMX)THEN
                  IF(XINT.GT.XCRN.AND.(XINT-XCRN).LT.DXMIN)THEN
                    XMIN=XINT
                    DXMIN=(XINT-XCRN)
                  ENDIF
                ENDIF
              ENDIF
  110       CONTINUE
            ZMIN=ZCRN
            CALL CKGRDL(IS,XCRN,ZCRN,XMIN,ZMIN,NUM)
          ENDIF
   40   CONTINUE
   30 CONTINUE
      NLIND(IS)=NLINA(IS)
      NVRTD(IS)=NVRTA(IS)
      RETURN
      END

C ************************     SRFIND     *****************************
C SRFIND defines the surfaces indentation from inside.
C *********************************************************************
      SUBROUTINE SRFIND
#include "building.h"
#include "geometry.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)

      DO 10 IS=1,NSUR
        THKS(IS)=0.
      DO 10 IL=1,NE(IS)
        THKS(IS)=THKS(IS)+THK(IS,IL)
   10 CONTINUE
      DO 20 IS1=1,NSUR
        Y1S(IS1)=0.
      DO 20 IS2=1,NSUR
        IF(IS1.NE.IS2)THEN
          DELTA=0.
          DO 30 I123=1,3
            DELTA=DELTA+ABS(DCOSS(IS1,2,I123)-DCOSS(IS2,2,I123))
   30     CONTINUE
          IF(DELTA.LT.0.01)THEN
            Y1S1=Y0S(IS2)+THKS(IS2)-(Y0S(IS1)+THKS(IS1))
            Y1S(IS1)=AMAX1(Y1S(IS1),Y1S1)
          ENDIF
        ENDIF
   20 CONTINUE
      RETURN
      END

C ***************************   HRZGRD   ******************************
C HRZGRD creates a horrizontal grid line in a surface.
C *********************************************************************
      SUBROUTINE HRZGRD(IS,XVALU,ZVALU)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

      SML=-1.E-4
      CALL POINT1(IS,XVALU,ZVALU,IANS)
      IF(IANS.EQ.1)THEN
        DO 10 I=1,NLINA(IS)
          IB1=ILINE(IS,I,1)
          IB2=ILINE(IS,I,2)
          XB1=XVRT(IS,IB1)
          ZB1=ZVRT(IS,IB1)
          XB2=XVRT(IS,IB2)
          ZB2=ZVRT(IS,IB2)
          DZ1=ABS(ZVALU-ZB1)
          DZ2=ABS(ZVALU-ZB2)
          XMX=AMAX1(XB1,XB2)
          XMN=AMIN1(XB1,XB2)
          IF(DZ1.LT.1.E-2.AND.DZ2.LT.1.E-2.AND.
     &      (XVALU-XMN).GT.1.E-4.AND.(XMX-XVALU).GT.1.E-4)THEN
            CALL USRMSG('Selected gridding line is',
     &                  'very near to existing one !','W')
            RETURN
          ENDIF
   10   CONTINUE
        DXMIN1=1.E+10
        DXMIN2=1.E+10
        DO 20 I2=1,NLINB(IS)
          IB1=ILINE(IS,I2,1)
          IB2=ILINE(IS,I2,2)
          XB1=XVRT(IS,IB1)
          ZB1=ZVRT(IS,IB1)
          XB2=XVRT(IS,IB2)
          ZB2=ZVRT(IS,IB2)
          IF(ABS(ZB2-ZB1).LT.1E-3)GOTO 20
          XINT=(ZVALU-ZB1)/(ZB2-ZB1)*(XB2-XB1)+XB1
          XMX=AMAX1(XB1,XB2)
          XMN=AMIN1(XB1,XB2)
          ZMX=AMAX1(ZB1,ZB2)
          ZMN=AMIN1(ZB1,ZB2)
          IF((XMX-XINT).GT.SML.AND.(XINT-XMN).GT.SML.AND.
     &      (ZMX-ZVALU).GT.SML.AND.(ZVALU-ZMN).GT.SML)THEN
            IF((XVALU-XINT).GT.0.0.AND.(XVALU-XINT).LT.DXMIN1)THEN
              XMIN1=XINT
              DXMIN1=(XVALU-XINT)
            ELSEIF((XINT-XVALU).GT.0.0.AND.(XINT-XVALU).LT.DXMIN2)THEN
              XMIN2=XINT
              DXMIN2=(XINT-XVALU)
            ENDIF
          ENDIF
   20   CONTINUE
        NLINA(IS)=NLINA(IS)+1
        CALL VRTNUM(IS,XMIN1,ZVALU,NUM)
        ILINE(IS,NLINA(IS),1)=NUM
        CALL VRTNUM(IS,XMIN2,ZVALU,NUM)
        ILINE(IS,NLINA(IS),2)=NUM
        IF(MMOD.EQ.8)THEN
          iupdown=0
          isym=0
          CALL ETPLOT(XMIN1,ZVALU,iupdown,isym)
          iupdown=-204
          CALL ETPLOT(XMIN2,ZVALU,iupdown,isym)
        ENDIF
      ELSE
        CALL USRMSG(' ','Selected point is not within the surface!','W')
      ENDIF
      call forceflush()

      RETURN
      END

C ****************************    VRTGRD   ****************************
C VRTGRD creates a vertical grid line in a surface.
C *********************************************************************
      SUBROUTINE VRTGRD(IS,XVALU,ZVALU)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

      SML=-1.E-4
      CALL POINT1(IS,XVALU,ZVALU,IANS)
      IF(IANS.EQ.1)THEN
        DO 10 I=1,NLINA(IS)
          IB1=ILINE(IS,I,1)
          IB2=ILINE(IS,I,2)
          XB1=XVRT(IS,IB1)
          ZB1=ZVRT(IS,IB1)
          XB2=XVRT(IS,IB2)
          ZB2=ZVRT(IS,IB2)
          DX1=ABS(XVALU-XB1)
          DX2=ABS(XVALU-XB2)
          ZMX=AMAX1(ZB1,ZB2)
          ZMN=AMIN1(ZB1,ZB2)
          IF(DX1.LT.1.E-2.AND.DX2.LT.1.E-2.AND.
     &      (ZVALU-ZMN).GT.1.E-4.AND.(ZMX-ZVALU).GT.1.E-4)THEN
            CALL USRMSG('Selected gridding line is',
     &                  'very near to existing one !','W')
            RETURN
          ENDIF
   10   CONTINUE
        DZMIN1=1.E+10
        DZMIN2=1.E+10
        DO 20 I2=1,NLINB(IS)
          IB1=ILINE(IS,I2,1)
          IB2=ILINE(IS,I2,2)
          XB1=XVRT(IS,IB1)
          ZB1=ZVRT(IS,IB1)
          XB2=XVRT(IS,IB2)
          ZB2=ZVRT(IS,IB2)
          IF(ABS(XB2-XB1).LT.1E-3)GOTO 20
          ZINT=(XVALU-XB1)/(XB2-XB1)*(ZB2-ZB1)+ZB1
          XMX=AMAX1(XB1,XB2)
          XMN=AMIN1(XB1,XB2)
          ZMX=AMAX1(ZB1,ZB2)
          ZMN=AMIN1(ZB1,ZB2)
          IF((XMX-XVALU).GT.SML.AND.(XVALU-XMN).GT.SML.AND.
     &      (ZMX-ZINT).GT.SML.AND.(ZINT-ZMN).GT.SML)THEN
            IF((ZVALU-ZINT).GT.0.0.AND.(ZVALU-ZINT).LT.DZMIN1)THEN
              ZMIN1=ZINT
              DZMIN1=(ZVALU-ZINT)
            ELSEIF((ZINT-ZVALU).GT.0.0.AND.(ZINT-ZVALU).LT.DZMIN2)THEN
              ZMIN2=ZINT
              DZMIN2=(ZINT-ZVALU)
            ENDIF
          ENDIF
   20   CONTINUE
        NLINA(IS)=NLINA(IS)+1
        CALL VRTNUM(IS,XVALU,ZMIN1,NUM)
        ILINE(IS,NLINA(IS),1)=NUM
        CALL VRTNUM(IS,XVALU,ZMIN2,NUM)
        ILINE(IS,NLINA(IS),2)=NUM
        IF(MMOD.EQ.8)THEN
          iupdown=0
          isym=0
          CALL ETPLOT(XVALU,ZMIN1,iupdown,isym)
          iupdown=-204
          CALL ETPLOT(XVALU,ZMIN2,iupdown,isym)
        ENDIF
      ELSE
        CALL USRMSG(' ','Selected point is not within the surface!','W')
      ENDIF
      RETURN
      END

C **************************    DELINE    *****************************
C DELINE deletes last gridding line.
C *********************************************************************
      SUBROUTINE DELINE(IS)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)

      DO 10 I=1,2
        IV1=ILINE(IS,NLINA(IS),I)
        DO 20 J=1,NLINA(IS)-1
          IV21=ILINE(IS,J,1)
          IV22=ILINE(IS,J,2)
          IF(IV1.EQ.IV21.OR.IV1.EQ.IV22)GOTO 10
   20   CONTINUE
        NVRTA(IS)=NVRTA(IS)-1
        IF(NVRTD(IS).GT.NVRTA(IS))NVRTD(IS)=NVRTA(IS)
        DO 30 K=IV1,NVRTA(IS)
          XVRT(IS,K)=XVRT(IS,K+1)
          ZVRT(IS,K)=ZVRT(IS,K+1)
   30   CONTINUE
        XVRT(IS,NVRTA(IS)+1)=0.
        ZVRT(IS,NVRTA(IS)+1)=0.
   10 CONTINUE
      NLINA(IS)=NLINA(IS)-1
      IF(NLIND(IS).GT.NLINA(IS))NLIND(IS)=NLINA(IS)
      RETURN
      END

C **************************    VRTNUM    *****************************
C VRTNUM defines the 3D vertex number from its X and Z coordinates.
C *********************************************************************
      SUBROUTINE VRTNUM(IS,X,Z,NUM)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)

      DO 10 I=1,NVRTA(IS)
        CHK1=ABS(X-XVRT(IS,I))
        CHK2=ABS(Z-ZVRT(IS,I))
        CHK=CHK1+CHK2
        IF(CHK.LT.1.E-3)THEN
          NUM=I
          RETURN
        ENDIF
   10 CONTINUE
      NVRTA(IS)=NVRTA(IS)+1
      XVRT(IS,NVRTA(IS))=X
      ZVRT(IS,NVRTA(IS))=Z
      NUM=NVRTA(IS)
      RETURN
      END

C ****************************   CKGRDL   *****************************
C CKGRDL checks if the gridding line does exist.
C *********************************************************************
      SUBROUTINE CKGRDL(IS,XCRN,ZCRN,XMIN,ZMIN,NUM)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)

C Check if this gridding line does not already exists
      DO 10 I=NLINB(IS)+1,NLINA(IS)
        CHK1=ABS(XMIN-XVRT(IS,ILINE(IS,I,1)))
        CHK2=ABS(ZMIN-ZVRT(IS,ILINE(IS,I,1)))
        CHK3=ABS(XCRN-XVRT(IS,ILINE(IS,I,2)))
        CHK4=ABS(ZCRN-ZVRT(IS,ILINE(IS,I,2)))
        TOL4=CHK1+CHK2+CHK3+CHK4
        CHK5=ABS(XMIN-XVRT(IS,ILINE(IS,I,2)))
        CHK6=ABS(ZMIN-ZVRT(IS,ILINE(IS,I,2)))
        CHK7=ABS(XCRN-XVRT(IS,ILINE(IS,I,1)))
        CHK8=ABS(ZCRN-ZVRT(IS,ILINE(IS,I,1)))
        TOL8=CHK5+CHK6+CHK7+CHK8
        IF(TOL4.LT.1.E-3.OR.TOL8.LT.1.E-3)THEN
          NUM=I
          RETURN
        ENDIF
  10  CONTINUE

C Create new gridding line.
      NLINA(IS)=NLINA(IS)+1
      NUM=NLINA(IS)
      CALL VRTNUM(IS,XMIN,ZMIN,NUM)
      ILINE(IS,NLINA(IS),1)=NUM
      CALL VRTNUM(IS,XCRN,ZCRN,NUM)
      ILINE(IS,NLINA(IS),2)=NUM
      RETURN
      END

C **************************  GRDEDG  *********************************
C GRDEDG is the main controller for edges discretization. It displays
C the main menu for edges.
C *********************************************************************
      SUBROUTINE GRDEDG(IZ,IDRW1,IDRW2)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC8/SLAT,SLON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)

      LOGICAL EDGOK

      DIMENSION ITEM(17),IVALS(MEZ)
      CHARACTER*72 LINDSC,DISCR
      CHARACTER SRF(2)*12,ITEMS(MEZ)*27
      CHARACTER ITEM*30,SZN*12,STTS*9,STATS*15,MSG*124,outs*124
      real DZX   ! To avoid variable name clash with DX from geometry.h
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='GRDEDG'  ! set for subroutine

      SZN=zname(IZ)
      SRF(1)=' '
      SRF(2)=' '
      EDGOK=.FALSE.
      IEG=1
   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'  zone       : ',SZN
      WRITE(ITEM(2),'(A,A)')'  surface (1): ',SRF(1)
      WRITE(ITEM(3),'(A,A)')'  surface (2): ',SRF(2)
      IF(EDGOK.AND.INDXE(IEG).EQ.0)THEN
       ITEM(4)='  status     : Not defined'
      ELSEIF(EDGOK.AND.INDXE(IEG).EQ.1)THEN
       ITEM(4)='  status     : Lumped'
      ELSEIF(EDGOK.AND.INDXE(IEG).EQ.3)THEN
       ITEM(4)='  status     : Descretized'
      ELSE
       ITEM(4)='  status     :'
      ENDIF
      ITEM(5)='  ----------------------------'
      ITEM(6)='1 define the edge            '
      ITEM(7)='2 define Z - gridding        '
      ITEM(8)='3 construction details       '
      ITEM(9)='4 change status              '
      ITEM(10)='  ----------------------------'
      ITEM(11)='5 all edges not defined      '
      ITEM(12)='6 all edges lumped           '
      ITEM(13)='7 all edges discritized      '
      ITEM(14)='  ----------------------------'
      ITEM(15)='l list edges                 '
      ITEM(16)='? help                       '
      ITEM(17)='- exit                       '
      MITEM=17

C Help text for this menu.
      helptopic='grd_edge_gridding'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Draw the appropriate graph.
      IF(MODIFYVIEW)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('       Edge Gridding',ITEM,MITEM,INO)
      IF(.NOT.EDGOK.AND.INO.GE.7.AND.INO.LE.9)THEN

C trap un-acceptable selections.
        CALL USRMSG(' ',' the edge should be defined first.','W')
      ELSEIF(INO.EQ.6)THEN

C Define the zone.
        IF(IDRW1.NE.2.OR.IDRW2.NE.IZ.OR.MODIFYVIEW)THEN
          IDRW1=2
          IDRW2=IZ
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF

C Show the edge selection menu.
        DO 20 IG=1,NUMEDG
          IF(INDXE(IG).EQ.1)THEN
            STTS='--- ( L )'
          ELSEIF(INDXE(IG).EQ.3)THEN
            STTS='--- ( D )'
          ELSE
            STTS='--- ( N )'
          ENDIF
          WRITE(ITEMS(IG),'(2(A,I2),A2,A9)')'edge at (',IVXEDG(IG,1),
     &                                ' - ',IVXEDG(IG,2),') ',STTS
   20   CONTINUE
   30   INPICK=1
        CALL EPICKS(INPICK,IVALS,' ',' ',27,
     &              NUMEDG,ITEMS,'  SELECT  EDGE',IER,nbhelp)
        IEG=IVALS(1)
        IF(IEG.GT.0.AND.IEG.LE.NUMEDG)THEN

C Draw the selected edge.
          IF(IDRW1.NE.4.OR.IDRW2.NE.IEG.OR.MODIFYVIEW)THEN
            IDRW1=4
            IDRW2=IEG
            CALL GRAAPH(IDRW1,IDRW2)
          ENDIF
          icon1=IZSTOCN(iz,ISFEDG(IEG,1))
          icon2=IZSTOCN(iz,ISFEDG(IEG,2))
          SRF(1)=SSNAME(icon1)
          SRF(2)=SSNAME(icon2)
          EDGOK=.TRUE.
        ELSEIF(IEG.EQ.0)THEN
          EDGOK=.FALSE.
          GOTO 10
        ELSE
          GOTO 30
        ENDIF
      ELSEIF(INO.EQ.7)THEN

C Define edge gridding in the local z-direction.
        ILNS=IVXEDG(IEG,1)
        ILNE=IVXEDG(IEG,2)
        THK3=SQRT((X(ILNE)-X(ILNS))**2+(Y(ILNE)-Y(ILNS))**2+
     &             (Z(ILNE)-Z(ILNS))**2)
        WRITE(outs,'(A,F12.5,A)')' The actual length which is (',
     &    THK3,') meters.'
        call edisp(iuout,outs)
        WRITE(LINDSC,'(A,2(I2,A))')
     &        'Define the gridding intervals (m) from vertex (',
     &        ILNS,') to (',ILNE,')'
   40   NDZ=NDZE(IEG)
        WRITE(DISCR,'(1X,10F7.3)')(DZE(IEG,J),J=1,NDZ)
        CALL EASKS(DISCR,LINDSC,' ',72,' ',
     &    ' gridding intervals ',IER,nbhelp)
        NDZ = iCountWords(DISCR)
        K=0
        TDZ=0.
        DO 50 I=1,NDZ
          CALL EGETWR(DISCR,K,DZX,0.,THK3,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 40
          DZE(IEG,I)=DZX
          TDZ=TDZ+DZX
          IF((TDZ-THK3).GT.1.E-3)THEN
            CALL USRMSG('the summation of entered intervals',
     &                  'is heigher than the actual length','W')
            GOTO 10
          ENDIF
   50   CONTINUE
        IF((THK3-TDZ).GT.1.E-3)THEN
          CALL USRMSG('the summation of entered intervals',
     &                'is less than the actual length','W')
          GOTO 10
        ENDIF
        NDZE(IEG)=NDZ

C Draw the intervals.
        IDRW1=4
        IDRW2=IEG
        CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.8)THEN

C Display the construction details menu.
        CALL CONEDG(IZ,IEG)
        IDRW1=4
        IDRW2=IEG
        CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.9)THEN

C Toggle the edge status.
        IF(INDXE(IEG).EQ.0)THEN
          INDXE(IEG)=1
        ELSEIF(INDXE(IEG).EQ.1)THEN
          INDXE(IEG)=3
        ELSEIF(INDXE(IEG).EQ.3)THEN
          INDXE(IEG)=0
        ENDIF
      ELSEIF(INO.EQ.11)THEN

C Set all the edges status to undefined.
        DO 60 III=1,NUMEDG
          INDXE(III)=0
   60   CONTINUE
      ELSEIF(INO.EQ.12)THEN

C Set all the edges status to lumped.
        DO 70 III=1,NUMEDG
          INDXE(III)=1
   70   CONTINUE
      ELSEIF(INO.EQ.13)THEN

C Set all the edges status to descretised.
        DO 80 III=1,NUMEDG
          INDXE(III)=3
   80   CONTINUE
      ELSEIF(INO.EQ.MITEM-2)THEN

C List edge informations.
        CALL EDISP(IUOUT,
     &    ' No.  IV1   IV2   surface-1      surface-2      status')
        CALL EDISP(IUOUT,
     &   '-----------------------------------------------------------')
        DO 90 IG=1,NUMEDG
          IF(INDXE(IG).EQ.1)THEN
            STATS='lumped'
          ELSEIF(INDXE(IG).EQ.3)THEN
            STATS='discretized'
          ELSE
            STATS='not defined'
          ENDIF
          ISF1=ISFEDG(IG,1)
          ISF2=ISFEDG(IG,2)
          icon1=IZSTOCN(iz,isf1)
          icon2=IZSTOCN(iz,isf2)
          WRITE(MSG,'(3(I3,3X),3(A12,3X))')IG,IVXEDG(IG,1),
     &      IVXEDG(IG,2),SSNAME(icon1),SSNAME(icon2),STATS
          CALL EDISP(IUOUT,MSG)
   90   CONTINUE
      ELSEIF(INO.EQ.MITEM-1)THEN

C Display the help message.
        helptopic='grd_edge_gridding'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(' edges gridding ',12,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Exit.
        RETURN
      ENDIF
      GOTO 10
      END

C *****************************  DRWEDG  ******************************
C DRWEDG draws the selected edge after checking it's existance. It 
C also draws the gridding in the local z direction.
C *********************************************************************
      SUBROUTINE DRWEDG(IEG)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)

      DIMENSION XEDG(8),YEDG(8),ZEDG(8)
      DIMENSION XS(8),YS(8),ZS(8)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym,iix,iiy    ! passed to etplot u2pixel
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym,iix,iiy    ! passed to etplot u2pixel
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      IVRT1=IVXEDG(IEG,1)
      IVRT2=IVXEDG(IEG,2)
      IS1=ISFEDG(IEG,1)
      IS2=ISFEDG(IEG,2)
      XEDG(1)=X(IVRT1)
      YEDG(1)=Y(IVRT1)
      ZEDG(1)=Z(IVRT1)
      XEDG(5)=X(IVRT2)
      YEDG(5)=Y(IVRT2)
      ZEDG(5)=Z(IVRT2)

C Determine the thickness of the first surfaces.
      THK1=0.0
      DO 70 I=1,NDXE(IEG)
        THK1=THK1+DXE(IEG,I)
   70 CONTINUE
      XEDG(4)=XEDG(1)+DCOSE(IEG,1,1)*THK1
      YEDG(4)=YEDG(1)+DCOSE(IEG,1,2)*THK1
      ZEDG(4)=ZEDG(1)+DCOSE(IEG,1,3)*THK1
      XEDG(8)=XEDG(5)+DCOSE(IEG,1,1)*THK1
      YEDG(8)=YEDG(5)+DCOSE(IEG,1,2)*THK1
      ZEDG(8)=ZEDG(5)+DCOSE(IEG,1,3)*THK1

C Determine the thickness of the second surfaces.
      THK2=0.0
      DO 100 I=1,NDYE(IEG)
        THK2=THK2+DYE(IEG,I)
  100 CONTINUE
      XEDG(2)=XEDG(1)+DCOSE(IEG,2,1)*THK2
      YEDG(2)=YEDG(1)+DCOSE(IEG,2,2)*THK2
      ZEDG(2)=ZEDG(1)+DCOSE(IEG,2,3)*THK2
      XEDG(3)=XEDG(4)+DCOSE(IEG,2,1)*THK2
      YEDG(3)=YEDG(4)+DCOSE(IEG,2,2)*THK2
      ZEDG(3)=ZEDG(4)+DCOSE(IEG,2,3)*THK2
      XEDG(6)=XEDG(5)+DCOSE(IEG,2,1)*THK2
      YEDG(6)=YEDG(5)+DCOSE(IEG,2,2)*THK2
      ZEDG(6)=ZEDG(5)+DCOSE(IEG,2,3)*THK2
      XEDG(7)=XEDG(8)+DCOSE(IEG,2,1)*THK2
      YEDG(7)=YEDG(8)+DCOSE(IEG,2,2)*THK2
      ZEDG(7)=ZEDG(8)+DCOSE(IEG,2,3)*THK2

C Find 2D extremes coordinates.
      DO 110 I=1,8
        CALL ORTTRN(XEDG(I),YEDG(I),ZEDG(I),TEMAT,AX,AY,AZ,IERR)
        CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
  110 CONTINUE
      XMN2=AMIN1(XS(1),XS(2),XS(3),XS(4),XS(5),XS(6),XS(7),XS(8))
      YMN2=AMIN1(YS(1),YS(2),YS(3),YS(4),YS(5),YS(6),YS(7),YS(8))
      XMX2=AMAX1(XS(1),XS(2),XS(3),XS(4),XS(5),XS(6),XS(7),XS(8))
      YMX2=AMAX1(YS(1),YS(2),YS(3),YS(4),YS(5),YS(6),YS(7),YS(8))

C Setup and pass in parameters to win3d.
      iiw1=20; iiw2=4; iiw3=3; iiw4=3; iimenu=29
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh
      CALL AXISCALE(igwid,igheight,XMN2,XMX2,YMN2,YMX2,XSC,YSC,
     &              SCA,XADD,YADD)
      CALL LINESCALE(iigl,XADD,SCA,iigb,YADD,SCA)

C Locate and label the two extreme vertices.
      CALL U2PIXEL(XS(1),YS(1),iix,iiy)
      CALL ECIRC(iix,iiy,3,1)
      CALL VERTLBL(iix,iiy,IVRT1,IER)
      CALL U2PIXEL(XS(5),YS(5),iix,iiy)
      CALL ECIRC(iix,iiy,3,1)
      CALL VERTLBL(iix,iiy,IVRT2,IER)

C Draw the edge.
      iupdown=0
      isym=0
      CALL ETPLOT(XS(1),YS(1),iupdown,isym)
      iupdown=1
      DO 120 I=2,4
        CALL ETPLOT(XS(I),YS(I),iupdown,isym)
  120 CONTINUE
      CALL ETPLOT(XS(1),YS(1),iupdown,isym)
      DO 130 I=5,8
        CALL ETPLOT(XS(I),YS(I),iupdown,isym)
  130 CONTINUE
      CALL ETPLOT(XS(5),YS(5),iupdown,isym)
      iupdown=0
      CALL ETPLOT(XS(2),YS(2),iupdown,isym)
      iupdown=1
      CALL ETPLOT(XS(6),YS(6),iupdown,isym)
      iupdown=0
      CALL ETPLOT(XS(3),YS(3),iupdown,isym)
      iupdown=1
      CALL ETPLOT(XS(7),YS(7),iupdown,isym)
      iupdown=0
      CALL ETPLOT(XS(4),YS(4),iupdown,isym)
      iupdown=1
      CALL ETPLOT(XS(8),YS(8),iupdown,isym)

C Draw the gridding lines.
      CALL EDGNUM(IS1,IS2,IVRT1,IVRT2,IEG)
      DO 140 IDZ=1,NDZE(IEG)-1
        I=5
        XEDG(I)=XEDG(I-4)+DZE(IEG,IDZ)*DCOSE(IEG,3,1)
        YEDG(I)=YEDG(I-4)+DZE(IEG,IDZ)*DCOSE(IEG,3,2)
        ZEDG(I)=ZEDG(I-4)+DZE(IEG,IDZ)*DCOSE(IEG,3,3)
        CALL ORTTRN(XEDG(I),YEDG(I),ZEDG(I),TEMAT,AX,AY,AZ,IERR)
        CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
        iupdown=0
        isym=0
        CALL ETPLOT(XS(I),YS(I),iupdown,isym)
        XEDG(I-4)=XEDG(I)
        YEDG(I-4)=YEDG(I)
        ZEDG(I-4)=ZEDG(I)
        DO 150 I=6,8
          XEDG(I)=XEDG(I-4)+DZE(IEG,IDZ)*DCOSE(IEG,3,1)
          YEDG(I)=YEDG(I-4)+DZE(IEG,IDZ)*DCOSE(IEG,3,2)
          ZEDG(I)=ZEDG(I-4)+DZE(IEG,IDZ)*DCOSE(IEG,3,3)
          CALL ORTTRN(XEDG(I),YEDG(I),ZEDG(I),TEMAT,AX,AY,AZ,IERR)
          CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
          iupdown=-204
          isym=0
          CALL ETPLOT(XS(I),YS(I),iupdown,isym)
          XEDG(I-4)=XEDG(I)
          YEDG(I-4)=YEDG(I)
          ZEDG(I-4)=ZEDG(I)
  150   CONTINUE
        iupdown=-204
        isym=0
        CALL ETPLOT(XS(5),YS(5),iupdown,isym)
  140 CONTINUE
      call forceflush()

      RETURN
      END

C **************************   CONEDG   *******************************
C CONEDG displays the edge construction menu.
C *********************************************************************
      SUBROUTINE CONEDG(IZ,IEG)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PREC8/SLAT,SLON

      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)

      LOGICAL OK,DOK,chdb

      DIMENSION ITEM(14),IUDLR(5)

      CHARACTER*72 PROPS
      CHARACTER ITEM*30,SZN*12,SRF(2)*12,LMTS*32,DISCR*94
      CHARACTER TEXT*124,BAR*1,outs*124
      real DXX,DYX   ! To avoid variable name clash with DX from geometry.h
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CONEDG'  ! set for subroutine

C Define the two surfaces.
      IS1=ISFEDG(IEG,1)
      IS2=ISFEDG(IEG,2)
      SZN=ZNAME(IZ)
      icon1=IZSTOCN(iz,is1)
      icon2=IZSTOCN(iz,is2)
      SRF(1)=SSNAME(icon1)
      SRF(2)=SSNAME(icon2)
      IL=1
      IF(MMOD.EQ.8)CALL FHCEDG(IZ,IEG,IL)
      IDRW=1
      INO=-3

   10 WRITE(ITEM(1),'(A,A)')'  zone       : ',SZN
      WRITE(ITEM(2),'(A,A)')'  surface (1): ',SRF(1)
      WRITE(ITEM(3),'(A,A)')'  surface (2): ',SRF(2)
      WRITE(ITEM(4),'(A,I2)')'  mesh level : ',IL
      ITEM(5)='  ----------------------------'
      ITEM(6)='1 show next mesh level      '
      ITEM(7)='2 define X - gridding       '
      ITEM(8)='3 define Y - gridding       '
      ITEM(9)='4 create material rectangle '
      ITEM(10)='5 copy level construction '
      ITEM(11)='  ----------------------------'
      ITEM(12)='l list construction details '
      ITEM(13)='? help                     '
      ITEM(14)='- exit                     '
      MITEM=14

C Help for this menu.
      helptopic='grd_display_edges'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Draw the appropriate graph.
      IF(MODIFYVIEW)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('    Edge  Construction',ITEM,MITEM,INO)

      IF(INO.LE.5)THEN
        INO=-1
        GOTO 10
      ELSEIF(INO.EQ.6)THEN

C Show the next mesh level.
        IL=IL+1
        IF(IL.GT.NDZE(IEG))IL=1
        IF(MMOD.EQ.8)CALL FHCEDG(IZ,IEG,IL)
      ELSEIF(INO.EQ.7)THEN

C Define X - gridding.
        THK1=0.
        DO 20 I=1,NDXE(IEG)
          THK1=THK1+DXE(IEG,I)
   20   CONTINUE
        WRITE(outs,'(A,F12.5,A)')' The actual length which is (',
     &    THK1,') meters.'
        call edisp(iuout,outs)
        NDX=NDXE(IEG)
        WRITE(DISCR,'(1X,10F9.3)')(DXE(IEG,J),J=1,NDX)
        CALL EASKS(DISCR,'Define the X - gridding intervals (m).',
     &    ' ',72,' ',' gridding intervals ',IER,nbhelp)
        NDX = iCountWords(DISCR)
        IF(NDX.GT.MZE)THEN
          CALL USRMSG(' ',' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        TDX=0.
        DO 30 I=1,NDX
          CALL EGETWR(DISCR,K,DXX,0.,THK1,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DXE(IEG,I)=DXX
          TDX=TDX+DXX
          IF((TDX-THK1).GT.1.E-4)THEN
            CALL USRMSG('the summation of entered intervals',
     &                  'is heigher than the actual length','W')
            GOTO 10
          ENDIF
   30   CONTINUE
        IF((THK1-TDX).GT.1.E-4)THEN
          CALL USRMSG('the summation of entered intervals',
     &                'is less than the actual length','W')
          GOTO 10
        ENDIF
        IF(NDXE(IEG).NE.NDX)THEN
          NDXE(IEG)=NDX
          IF(MMOD.EQ.8)CALL FHCEDG(IZ,IEG,IL)
        ENDIF
      ELSEIF(INO.EQ.8)THEN

C Define Y - gridding.
        THK2=0.
        DO 40 I=1,NDYE(IEG)
          THK2=THK2+DYE(IEG,I)
   40   CONTINUE
        WRITE(outs,'(A,F12.5,A)')' The actual length which is (',
     &    THK2,') meters.'
        call edisp(iuout,outs)
        NDY=NDYE(IEG)
        WRITE(DISCR,'(1X,10F9.3)')(DYE(IEG,J),J=1,NDY)
        CALL EASKS(DISCR,'Define the Y - gridding intervals (m).',
     &    ' ',72,' ',' gridding intervals ',IER,nbhelp)
        NDY = iCountWords(DISCR)
        IF(NDY.GT.MZE)THEN
          CALL USRMSG(' ',
     &              ' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        TDY=0.
        DO 50 I=1,NDY
          CALL EGETWR(DISCR,K,DYX,0.,THK2,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DYE(IEG,I)=DYX
          TDY=TDY+DYX
          IF((TDY-THK2).GT.1.E-4)THEN
            CALL USRMSG('the summation of entered intervals',
     &                  'is heigher than the actual length','W')
            GOTO 10
          ENDIF
   50   CONTINUE
        IF((THK2-TDY).GT.1.E-4)THEN
          CALL USRMSG('the summation of entered intervals',
     &                'is less than the actual length','W')
          GOTO 10
        ENDIF
        IF(NDYE(IEG).NE.NDY)THEN
          NDYE(IEG)=NDY
          IF(MMOD.EQ.8)CALL FHCEDG(IZ,IEG,IL)
        ENDIF

C Create a material rectangle.
      ELSEIF(INO.EQ.9)THEN
        IF(MMOD.EQ.8.AND.IDRW.EQ.1)CALL FHCEDG(IZ,IEG,IL)
        IDRW=2

C Ask if user wishes to browse through materials to find suitable
C reference. Use elistmat to select via materials array.
        CALL ASKOK('Browse the materials db for a new material',
     &    '(NO exits the process)?',OK,dok,nbhelp)
        IF(OK)then
          iwhich = 0
          CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
          if(iwhich.eq.0)then
            CALL EASKABC('Your selection is `0` i.e. air. Options:',
     &        ' ','accept','reselect material','abort',
     &        iwair,nbhelp)
            if(iwair.eq.2)then
              CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
            elseif(iwair.eq.3)then
              GOTO 10
            endif
          elseif(iwhich.eq.-99)then
            continue
          endif
        else
          goto 10  ! go back
        endif

        IUDLR(1)=IWHICH
        WRITE(LMTS,'(4(1X,I2))')(IUDLR(K),K=2,5)
        WRITE(PROPS,'(2A)')'enter the material rectangle borders',
     &                     ' (left, down, right, and up)'
        CALL EASKS(LMTS,PROPS,' ',32,' ',' rectangle limits ',
     &    IER,nbhelp)
        NDZ = iCountWords(LMTS)
        K=0
        DO 60 L=2,5
          CALL EGETWI(LMTS,K,IUDLR(L),1,100,'F',
     &                 'rectangle limits',IER)
          IF(IER.NE.0)GOTO 10
   60   CONTINUE
        DO 70 IY=IUDLR(3),IUDLR(5)
          DO 80 IX=IUDLR(2),IUDLR(4)
            ITPEDG(IEG,IX,IY,IL)=IUDLR(1)
   80     CONTINUE
   70   CONTINUE
        IF(MMOD.EQ.8)CALL FHCEDG(IZ,IEG,IL)
        IDRW=2

C Copy the construction data of existing edge level.
      ELSEIF(INO.EQ.10)THEN
        CALL EASKI(IWCH,' ','Copy form which level:',1,'F',NDZE(IEG),
     &    'F',1,'level number',IER,nbhelp)
        DO 90 IY=1,NDYE(IEG)
        DO 90 IX=1,NDXE(IEG)
          ITPEDG(IEG,IX,IY,IL)=ITPEDG(IEG,IX,IY,IWCH)
   90   CONTINUE
        CALL FHCEDG(IZ,IEG,IL)
        IDRW=2

C List construction details.
      ELSEIF(INO.EQ.(MITEM-2))THEN
        BAR='I'
        icon=IZSTOCN(iz,is2)
        WRITE(TEXT,'(A12,4X,A1,10A)')SSNAME(icon),
     &        '-',('--------',IX=1,NDXE(IEG))
        CALL EDISP(IUOUT,TEXT)
        WRITE(TEXT,'(14X,2X,A1,8(2X,I3,2X,A1))')
     &        BAR,(ITPEDG(IEG,IX,NDYE(IEG),IL),BAR,IX=1,NDXE(IEG))
        CALL EDISP(IUOUT,TEXT)
        WRITE(TEXT,'(16X,A1,8A8)')'-',('--------',IX=1,NDXE(IEG))
        CALL EDISP(IUOUT,TEXT)
        DO 100 IY=NDYE(IEG)-1,1,-1
          WRITE(TEXT,'(16X,A1,8(2X,I3,2X,A1))')
     &          BAR,(ITPEDG(IEG,IX,IY,IL),BAR,IX=1,NDXE(IEG))
          CALL EDISP(IUOUT,TEXT)
          WRITE(TEXT,'(16X,A1,8A8)')'-',('--------',IX=1,NDXE(IEG))
          CALL EDISP(IUOUT,TEXT)
  100   CONTINUE
        icon=IZSTOCN(iz,is1)
        WRITE(TEXT,'(18X,A12)')SSNAME(icon)
        CALL EDISP(IUOUT,TEXT)

C Display help statements.
      ELSEIF(INO.EQ.(MITEM-1))THEN

        helptopic='grd_display_edges'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(' edges construction ',8,'-',0,0,IER)

C Exit.
      ELSEIF(INO.EQ.MITEM)THEN
        RETURN
      ENDIF
      GOTO 10
      END

C ***************************   FHCEDG   ******************************
C FHCEDG draws the free hand sketch for the cross section of a given 
C edge with it's gridding lines.
C *********************************************************************
      SUBROUTINE FHCEDG(IZ,IEG,IL)
#include "building.h"
#include "geometry.h"

      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)

      CHARACTER LABELN*3,LABELS*16
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer iix,iiy,iix1,iiy1,iix2,iiy2
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont,ipos    ! passed to viewtext etlabel
      integer*8 iix,iiy,iix1,iiy1,iix2,iiy2
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Define the two surfaces.
      IS1=ISFEDG(IEG,1)
      IS2=ISFEDG(IEG,2)

      XMN2=0.
      YMN2=0.
      XMX2=FLOAT(NDXE(IEG))
      YMX2=FLOAT(NDYE(IEG))
      XYMX=AMAX1(XMX2,YMX2)

C Setup and pass in parameters to win3d.
      iiw1=30; iiw2=10; iiw3=5; iiw4=3; iimenu=29
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh
      CALL AXISCALE(igwid,igheight,XMN2,XMX2,YMN2,YMX2,XSC,YSC,SCA,
     &               XADD,YADD)
      CALL LINESCALE(iigl,XADD,SCA,iigb,YADD,SCA)
      CALL U2PIXEL(XMN2,YMN2,iix,iiy)
      CALL ELINE(iix,iiy,1)
      CALL ECIRC(iix,iiy,3,1)
      CALL U2PIXEL(XMX2,YMN2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL U2PIXEL(XMX2,YMX2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL U2PIXEL(XMN2,YMX2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL U2PIXEL(XMN2,YMN2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      DO 10 I=1,NDXE(IEG)-1
        CALL U2PIXEL(FLOAT(I),YMN2,iix1,iiy1)
        CALL U2PIXEL(FLOAT(I),YMX2,iix2,iiy2)
        CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
   10 CONTINUE
      DO 20 J=1,NDYE(IEG)-1
        CALL U2PIXEL(XMN2,FLOAT(J),iix1,iiy1)
        CALL U2PIXEL(XMX2,FLOAT(J),iix2,iiy2)
        CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
   20 CONTINUE
      XCF=-0.5
      DO 30 I=1,NDXE(IEG)
        XCF=XCF+1.
        YCF=-0.5
        DO 40 J=1,NDYE(IEG)
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,iix,iiy)
          WRITE(LABELS,'(I3)')ITPEDG(IEG,I,J,IL)
          ipos=0
          isize=itfs
          CALL ETLABEL(LABELS,XCF,YCF,ipos,isize)
   40   CONTINUE
   30 CONTINUE
      iside=1
      isize=1
      ifont=2
      call viewtext('Material Types',iside,isize,ifont)
      XLAB=XMN2-XYMX/20.
      YLAB=YMN2-XYMX/20.
      WRITE(LABELN,'(A,I2)')'V',IVXEDG(IEG,1)
      ipos=2
      isize=itfs
      CALL ETLABEL(LABELN,XLAB,YLAB,ipos,isize)
      icon=IZSTOCN(iz,is1)
      WRITE(LABELS,'(A12)') SSNAME(icon)
      XLAB=XMX2/2.
      YLAB=YMN2-XYMX/10.
      ipos=2
      isize=itfs+2
      CALL ETLABEL(LABELS,XLAB,YLAB,ipos,isize)
      icon=IZSTOCN(iz,is2)
      WRITE(LABELS,'(A12)') SSNAME(icon)
      XLAB=XMN2-XMX2/10.
      YLAB=YMX2/2.
      ipos=1
      isize=itfs+2
      CALL ETLABEL(LABELS,XLAB,YLAB,ipos,isize)
      call forceflush()

      RETURN
      END

C ***************************   TOTEDG   ******************************
C TOTEDG determines the total number of edges in the zone.
C *********************************************************************
      SUBROUTINE TOTEDG
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)

C Initialize the total number of edges in the current zone.
      NUMEDG=0

C For each surface.
      DO 10 IS1=1,NSUR-1
        DO 20 IS2=IS1+1,NSUR

C Determine the two planes angles.
          DELTA=0.
          DO 30 I123=1,3
            DELTA=DELTA+ABS(DCOSS(IS2,2,I123)-DCOSS(IS1,2,I123))
   30     CONTINUE

C The two surfaces are in the same plane (bypass).
          IF(DELTA.LT.0.01)GOTO 20

C Determine the common edges between the two surfaces.
C Searching for the first common vertex.

C For each vertex in the first surface.
          DO 40 I1=1,NVER(IS1)
            IF(I1.EQ.NVER(IS1))THEN
              J1=1
            ELSE
              J1=I1+1
            ENDIF
            IV11=JVN(IS1,I1)
            IV12=JVN(IS1,J1)
          DO 40 I2=1,NVER(IS2)
            IF(I2.EQ.NVER(IS2))THEN
              J2=1
            ELSE
              J2=I2+1
            ENDIF
            IV21=JVN(IS2,I2)
            IV22=JVN(IS2,J2)
            IF(IV11.EQ.IV22.AND.IV12.EQ.IV21)THEN
              CALL EDGCRT(IS1,IS2,IV11,IV12)
            ENDIF
   40     CONTINUE
   20   CONTINUE
   10 CONTINUE
      RETURN
      END


C *************************     INDEDG     ****************************
C INDEDG Initializes the edges gridding.
C *********************************************************************
      SUBROUTINE INDEDG
#include "building.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)

      DIMENSION ZNORM(3)

      DO 10 IEG=1,NUMEDG
        IS1=ISFEDG(IEG,1)
        ISE1=ISEFNE(IEG,IS1) ! find index srf-edge connection surface
        DELTX=Y1S(IS1)
        IF(DELTX.LT.1.E-6)THEN
          I=0
        ELSE
          I=1
          DXE(IEG,1)=DELTX
        ENDIF
        DO 20 IL1=NE(IS1),1,-1
          I=I+1
          DXE(IEG,I)=THK(IS1,IL1)
   20   CONTINUE
        DELTX=Y0SE(ISE1)-Y0S(IS1)
        IF(DELTX.GT.1.E-3)THEN
          I=I+1
          DXE(IEG,I)=DELTX
        ENDIF
        NDXE(IEG)=I
        IS2=ISFEDG(IEG,2)
        ISE2=ISEFNE(IEG,IS2) ! find index srf-edge connection surface
        DELTY=Y1S(IS2)
        IF(DELTY.LT.1.E-6)THEN
          I=0
        ELSE
          I=1
          DYE(IEG,1)=DELTY
        ENDIF
        DO 30 IL2=NE(IS2),1,-1
          I=I+1
          DYE(IEG,I)=THK(IS2,IL2)
   30   CONTINUE
        DELTY=Y0SE(ISE2)-Y0S(IS2)
        IF(DELTY.GT.1.E-3)THEN
          I=I+1
          DYE(IEG,I)=DELTY
        ENDIF
        NDYE(IEG)=I
        CALL LINORM(IVXEDG(IEG,1),IVXEDG(IEG,2),ZNORM)
        DO 40 I123=1,3
          DCOSE(IEG,1,I123)=DCOSS(IS1,2,I123)
          DCOSE(IEG,2,I123)=DCOSS(IS2,2,I123)
          DCOSE(IEG,3,I123)=ZNORM(I123)
   40   CONTINUE
   10 CONTINUE
      RETURN
      END

C ***************************   EDGNUM   ******************************
C EDGNUM defines the number of the edge in a zone from it's associated
C two surfaces.
C *******************************************************************
      SUBROUTINE EDGNUM(IS1,IS2,IV1,IV2,NUM)
#include "building.h"

      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)

      DO 10 I=1,NUMEDG
        ISRF1=ISFEDG(I,1)
        ISRF2=ISFEDG(I,2)
        IF((IS1.EQ.ISRF1.AND.IS2.EQ.ISRF2).OR.
     &     (IS1.EQ.ISRF2.AND.IS2.EQ.ISRF1))THEN
           IVRT1=IVXEDG(I,1)
           IVRT2=IVXEDG(I,2)
           IF((IV1.EQ.IVRT1.AND.(IV2.EQ.IVRT2.OR.IV2.EQ.IV1)).OR.
     &        (IV1.EQ.IVRT2.AND.(IV2.EQ.IVRT1.OR.IV2.EQ.IV1)))THEN
              NUM=I
              RETURN
           ENDIF
         ENDIF
   10 CONTINUE
      STOP "error (301): no edge defined at expected edge location."
      END

C ***************************    EDGCRT    ****************************
C EDGCRT creates new edges or appends new edge to existing one.
C *********************************************************************
      SUBROUTINE EDGCRT(IS1,IS2,IV1,IV2)
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)

      DIMENSION ZNORM(3)

      DO 10 I=1,NUMEDG
        ISRF1=ISFEDG(I,1)
        ISRF2=ISFEDG(I,2)
        IF((IS1.EQ.ISRF1.AND.IS2.EQ.ISRF2).OR.
     &     (IS1.EQ.ISRF2.AND.IS2.EQ.ISRF1))THEN
           IVRT1=IVXEDG(I,1)
           IVRT2=IVXEDG(I,2)
           IF(IV1.EQ.IVRT1)THEN
             IVXEDG(I,1)=IV2
             DZE(I,1)=SQRT((X(IVXEDG(I,2))-X(IVXEDG(I,1)))**2+
     &                     (Y(IVXEDG(I,2))-Y(IVXEDG(I,1)))**2+
     &                     (Z(IVXEDG(I,2))-Z(IVXEDG(I,1)))**2)
             RETURN
           ELSEIF(IV1.EQ.IVRT2)THEN
             IVXEDG(I,2)=IV2
             DZE(I,1)=SQRT((X(IVXEDG(I,2))-X(IVXEDG(I,1)))**2+
     &                     (Y(IVXEDG(I,2))-Y(IVXEDG(I,1)))**2+
     &                     (Z(IVXEDG(I,2))-Z(IVXEDG(I,1)))**2)
             RETURN
           ELSEIF(IV2.EQ.IVRT1)THEN
             IVXEDG(I,1)=IV1
             DZE(I,1)=SQRT((X(IVXEDG(I,2))-X(IVXEDG(I,1)))**2+
     &                     (Y(IVXEDG(I,2))-Y(IVXEDG(I,1)))**2+
     &                     (Z(IVXEDG(I,2))-Z(IVXEDG(I,1)))**2)
             RETURN
           ELSEIF(IV2.EQ.IVRT2)THEN
             IVXEDG(I,2)=IV1
             DZE(I,1)=SQRT((X(IVXEDG(I,2))-X(IVXEDG(I,1)))**2+
     &                     (Y(IVXEDG(I,2))-Y(IVXEDG(I,1)))**2+
     &                     (Z(IVXEDG(I,2))-Z(IVXEDG(I,1)))**2)
             RETURN
           ENDIF
         ENDIF
   10 CONTINUE
      NUMEDG=NUMEDG+1
      IF(NUMEDG.GT.MEZ)
     &STOP "error (302): maximum number of edges exceeded."
      ISFEDG(NUMEDG,1)=IS1
      ISFEDG(NUMEDG,2)=IS2
      IVXEDG(NUMEDG,1)=IV1
      IVXEDG(NUMEDG,2)=IV2
      CALL LINORM(IV1,IV2,ZNORM)
      DO 20 I123=1,3
        DCOSE(NUMEDG,1,I123)=DCOSS(IS1,2,I123)
        DCOSE(NUMEDG,2,I123)=DCOSS(IS2,2,I123)
        DCOSE(NUMEDG,3,I123)=ZNORM(I123)
   20 CONTINUE
      NDZE(NUMEDG)=1
      DZE(NUMEDG,1)=SQRT((X(IV1)-X(IV2))**2+
     &                   (Y(IV1)-Y(IV2))**2+(Z(IV1)-Z(IV2))**2)
      RETURN
      END

C **************************  GRDCRN  *********************************
C GRDCRN is the main controller for corners discretization. It displays
C the main menu for corners.
C *********************************************************************
      SUBROUTINE GRDCRN(IZ,IDRW1,IDRW2)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PREC8/SLAT,SLON

      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)

      LOGICAL CRNOK

      DIMENSION IVALS(MCZ)
      CHARACTER*72 DISCR
      CHARACTER SRF(3)*12,ITEMS(MCZ)*25
      CHARACTER ITEM(18)*30,SZN*12,STTS*9,STATS*15
      character MSG*124,outs*124
      real DZX   ! To avoid variable name clash with DX from geometry.h
      integer MITEM,INO  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='GRDCRN'  ! set for subroutine

      SZN=zname(IZ)
      SRF(1)=' '
      SRF(2)=' '
      SRF(3)=' '
      CRNOK=.FALSE.
      ICR=1
   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'  zone       : ',SZN
      WRITE(ITEM(2),'(A,A)')'  surface (1): ',SRF(1)
      WRITE(ITEM(3),'(A,A)')'  surface (2): ',SRF(2)
      WRITE(ITEM(4),'(A,A)')'  surface (3): ',SRF(3)
      IF(CRNOK.AND.INDXC(ICR).EQ.0)THEN
       ITEM(5)='  status     : Not defined'
      ELSEIF(CRNOK.AND.INDXC(ICR).EQ.1)THEN
       ITEM(5)='  status     : Lumped'
      ELSEIF(CRNOK.AND.INDXC(ICR).EQ.3)THEN
       ITEM(5)='  status     : Descretized'
      ELSE
       ITEM(5)='  status     :'
      ENDIF
      ITEM(6)='  ----------------------------'
      ITEM(7)='1 define the corner          '
      ITEM(8)='2 define Z - gridding        '
      ITEM(9)='3 construction details       '
      ITEM(10)='4 change status              '
      ITEM(11)='  ----------------------------'
      ITEM(12)='5 all corners not defined    '
      ITEM(13)='6 all corners lumped         '
      ITEM(14)='7 all corners descretized    '
      ITEM(15)='  ----------------------------'
      ITEM(16)='l list corners               '
      ITEM(17)='? help                       '
      ITEM(18)='- exit                       '
      MITEM=18

C Help text for this menu.
      helptopic='grd_corner_grdding'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Draw the appropriate graph.
      IF(MODIFYVIEW)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('     Corner  Gridding',ITEM,MITEM,INO)

C Trap the un-acceptable selections.
      IF(.NOT.CRNOK.AND.INO.GE.8.AND.INO.LE.10)THEN
        CALL USRMSG(' ','The corner should be defined first.','W')
      ELSEIF(INO.EQ.7)THEN

C Define the corner.
        IF(IDRW1.NE.2.OR.IDRW2.NE.IZ.OR.MODIFYVIEW)THEN
          IDRW1=2
          IDRW2=IZ
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF

C Show the surface selection menu.
        DO 20 ICRN=1,NUMCRN
          IF(INDXC(ICRN).EQ.1)THEN
            STTS='--- ( L )'
          ELSEIF(INDXC(ICRN).EQ.3)THEN
            STTS='--- ( D )'
          ELSE
            STTS='--- ( N )'
          ENDIF
          IV1=IVXCRN(ICRN)
          WRITE(ITEMS(ICRN),'(A,I2,2A)')'corner at (',IV1,') ',STTS
   20   CONTINUE
        INPICK=1
   30   CALL EPICKS(INPICK,IVALS,' ',' ',25,
     &              NUMCRN,ITEMS,'   SELECT CORNER',IER,nbhelp)
        ICR=IVALS(1)
        IF(ICR.GT.0.AND.ICR.LE.NUMCRN)THEN

C Draw the selected corner.
          IF(IDRW1.NE.5.OR.IDRW2.NE.ICR.OR.MODIFYVIEW)THEN
            IDRW1=5
            IDRW2=ICR
            CALL GRAAPH(IDRW1,IDRW2)
          ENDIF
          icon1=IZSTOCN(iz,ISFCRN(ICR,1))
          icon2=IZSTOCN(iz,ISFCRN(ICR,2))
          icon3=IZSTOCN(iz,ISFCRN(ICR,3))
          SRF(1)=SSNAME(icon1)
          SRF(2)=SSNAME(icon2)
          SRF(3)=SSNAME(icon3)
          CRNOK=.TRUE.
        ELSEIF(ICR.EQ.0)THEN
          CRNOK=.FALSE.
          GOTO 10
        ELSE
          GOTO 30
        ENDIF
      ELSEIF(INO.EQ.8)THEN

C Define edge gridding in the local z-direction.
        THK3=0.
        NDZ=NDZC(ICR)
        DO 100 IDZ=1,NDZ
          THK3=THK3+DZC(ICR,IDZ)
  100   CONTINUE
        WRITE(outs,'(A,F12.5,A)')' The actual length which is (',
     &    THK3,') meters.'
        call edisp(iuout,outs)
        WRITE(DISCR,'(1X,10F7.3)')(DZC(ICR,J),J=1,NDZ)
        CALL EASKS(DISCR,'Define the Z - gridding intervals (m).',
     &    ' ',72,' ',' gridding intervals ',IER,nbehlp)
        CALL USRMSG(' ',' ','-')
        NDZ = iCountWords(DISCR)
        IF(NDZ.GT.MZE)THEN
          CALL USRMSG(' ',' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        TDZ=0.
        DO 120 I=1,NDZ
          CALL EGETWR(DISCR,K,DZX,0.,THK3,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DZC(ICR,I)=DZX
          TDZ=TDZ+DZX
          IF((TDZ-THK3).GT.1.E-4)THEN
            CALL USRMSG('the summation of entered intervals',
     &                  'is greater than the actual length','W')
            CALL USRMSG(' ',' ','-')
            GOTO 10
          ENDIF
  120   CONTINUE
        IF((THK3-TDZ).GT.1.E-4)THEN
          CALL USRMSG('the summation of entered intervals',
     &                'is less than the actual length','W')
          CALL USRMSG(' ',' ','-')
          GOTO 10
        ENDIF
        IF(NDZC(ICR).NE.NDZ)THEN
          NDZC(ICR)=NDZ
          IF(MMOD.EQ.8)CALL DRWCRN(ICR)
        ENDIF
      ELSEIF(INO.EQ.9)THEN

C Display the corner construction details menu.
        CALL CONCRN(IZ,ICR)
         IDRW1=5
         IDRW2=ICR
         CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.10)THEN

C Toggle the corner status.
        IF(INDXC(ICR).EQ.0)THEN
          INDXC(ICR)=1
        ELSEIF(INDXC(ICR).EQ.1)THEN
          INDXC(ICR)=3
        ELSEIF(INDXC(ICR).EQ.3)THEN
          INDXC(ICR)=0
        ENDIF
      ELSEIF(INO.EQ.12)THEN

C Set all corners status to undefined.
        DO 40 III=1,NUMCRN
          INDXC(III)=0
   40   CONTINUE
      ELSEIF(INO.EQ.13)THEN

C Set all corners status to lumped.
         DO 50 III=1,NUMCRN
          INDXC(III)=1
   50   CONTINUE
      ELSEIF(INO.EQ.14)THEN

C Set all corners status to descretised.
        DO 60 III=1,NUMCRN
          INDXC(III)=3
   60   CONTINUE
      ELSEIF(INO.EQ.MITEM-2)THEN

C List the corners informations.
        WRITE(MSG,'(2A)')' No.  vrtx  surface-1      surface-2',
     &         '      surface-3      status'
        CALL EDISP(IUOUT,MSG)
        WRITE(MSG,'(2A)')'------------------------------------------',
     &              '--------------------------'
        CALL EDISP(IUOUT,MSG)
        DO 70 ICC=1,NUMCRN
          IF(INDXC(ICC).EQ.1)THEN
            STATS='lumped'
          ELSEIF(INDXC(ICC).EQ.3)THEN
            STATS='discretized'
          ELSE
            STATS='not defined'
          ENDIF
          ISF1=ISFCRN(ICC,1)
          ISF2=ISFCRN(ICC,2)
          ISF3=ISFCRN(ICC,3)
          icon1=IZSTOCN(iz,isf1)
          icon2=IZSTOCN(iz,isf2)
          icon3=IZSTOCN(iz,isf3)

          WRITE(MSG,'(2(I3,3X),4(A12,3X))')ICC,IVXCRN(ICC),
     &          SSNAME(icon1),SSNAME(icon2),SSNAME(icon3),STATS
          CALL EDISP(IUOUT,MSG)
   70   CONTINUE
      ELSEIF(INO.EQ.MITEM-1)THEN

C Display the help message.
        helptopic='grd_corner_grdding'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(' corners gridding ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Eixt.
        RETURN
      ENDIF
      GOTO 10
      END

C ************************     DRWCRN     *****************************
C DRWCRN draws the selected corner after checking it's existance. It 
C also draws the gridding in the local z direction.
C *********************************************************************
      SUBROUTINE DRWCRN(ICR)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)

      DIMENSION XCRN(8),YCRN(8),ZCRN(8)
      DIMENSION XS(8),YS(8),ZS(8)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym,iix,iiy    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale u2pixel
      integer*8 iupdown,isym,iix,iiy    ! passed to etplot u2pixel
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      IVRX=IVXCRN(ICR)

      XCRN(1)=X(IVRX)
      YCRN(1)=Y(IVRX)
      ZCRN(1)=Z(IVRX)

C Determine the thickness of the first surface.
      THK1=0.0
      DO 70 I=1,NDXC(ICR)
        THK1=THK1+DXC(ICR,I)
   70 CONTINUE
      XCRN(4)=XCRN(1)+DCOSC(ICR,1,1)*THK1
      YCRN(4)=YCRN(1)+DCOSC(ICR,1,2)*THK1
      ZCRN(4)=ZCRN(1)+DCOSC(ICR,1,3)*THK1

C Determine the thickness of the second surface.
      THK2=0.0
      DO 100 I=1,NDYC(ICR)
        THK2=THK2+DYC(ICR,I)
  100 CONTINUE
      XCRN(2)=XCRN(1)+DCOSC(ICR,2,1)*THK2
      YCRN(2)=YCRN(1)+DCOSC(ICR,2,2)*THK2
      ZCRN(2)=ZCRN(1)+DCOSC(ICR,2,3)*THK2
      XCRN(3)=XCRN(4)+DCOSC(ICR,2,1)*THK2
      YCRN(3)=YCRN(4)+DCOSC(ICR,2,2)*THK2
      ZCRN(3)=ZCRN(4)+DCOSC(ICR,2,3)*THK2

C Determine the thickness of the third surface.
      THK3=0.0
      DO 130 I=1,NDZC(ICR)
        THK3=THK3+DZC(ICR,I)
  130 CONTINUE
      DO 140 I=1,4
        XCRN(I+4)=XCRN(I)+DCOSC(ICR,3,1)*THK3
        YCRN(I+4)=YCRN(I)+DCOSC(ICR,3,2)*THK3
        ZCRN(I+4)=ZCRN(I)+DCOSC(ICR,3,3)*THK3
  140 CONTINUE

C Find 2D extremes coordinates.
      DO 150 I=1,8
        CALL ORTTRN(XCRN(I),YCRN(I),ZCRN(I),TEMAT,AX,AY,AZ,IERR)
        CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
  150 CONTINUE
      XMN2=AMIN1(XS(1),XS(2),XS(3),XS(4),XS(5),XS(6),XS(7),XS(8))
      YMN2=AMIN1(YS(1),YS(2),YS(3),YS(4),YS(5),YS(6),YS(7),YS(8))
      XMX2=AMAX1(XS(1),XS(2),XS(3),XS(4),XS(5),XS(6),XS(7),XS(8))
      YMX2=AMAX1(YS(1),YS(2),YS(3),YS(4),YS(5),YS(6),YS(7),YS(8))

C Setup and pass in parameters to win3d.
      iiw1=20; iiw2=4; iiw3=3; iiw4=3; iimenu=29
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh
      CALL AXISCALE(igwid,igheight,XMN2,XMX2,YMN2,YMX2,XSC,YSC,SCA,
     &              XADD,YADD)
      CALL LINESCALE(iigl,XADD,SCA,iigb,YADD,SCA)

C Locate and label the vertex.
      CALL U2PIXEL(XS(1),YS(1),iix,iiy)
      CALL ECIRC(iix,iiy,3,1)
      CALL VERTLBL(iix,iiy,IVRX,IER)

C Draw the corner.
      iupdown=0
      isym=0
      CALL ETPLOT(XS(1),YS(1),iupdown,isym)
      iupdown=1
      DO 160 I=2,4
        CALL ETPLOT(XS(I),YS(I),iupdown,isym)
  160 CONTINUE
      CALL ETPLOT(XS(1),YS(1),iupdown,isym)
      DO 170 I=5,8
        CALL ETPLOT(XS(I),YS(I),iupdown,isym)
  170 CONTINUE
      iupdown=1
      CALL ETPLOT(XS(5),YS(5),iupdown,isym)
      iupdown=0
      CALL ETPLOT(XS(2),YS(2),iupdown,isym)
      iupdown=1
      CALL ETPLOT(XS(6),YS(6),iupdown,isym)
      iupdown=0
      CALL ETPLOT(XS(3),YS(3),iupdown,isym)
      iupdown=1
      CALL ETPLOT(XS(7),YS(7),iupdown,isym)
      iupdown=0
      CALL ETPLOT(XS(4),YS(4),iupdown,isym)
      iupdown=1
      CALL ETPLOT(XS(8),YS(8),iupdown,isym)

C Draw the gridding lines.
      DO 180 IDZ=1,NDZC(ICR)-1
        I=5
        XCRN(I)=XCRN(I-4)+DZC(ICR,IDZ)*DCOSC(ICR,3,1)
        YCRN(I)=YCRN(I-4)+DZC(ICR,IDZ)*DCOSC(ICR,3,2)
        ZCRN(I)=ZCRN(I-4)+DZC(ICR,IDZ)*DCOSC(ICR,3,3)
        CALL ORTTRN(XCRN(I),YCRN(I),ZCRN(I),TEMAT,AX,AY,AZ,IERR)
        CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
        iupdown=0
        isym=0
        CALL ETPLOT(XS(I),YS(I),iupdown,isym)
        XCRN(I-4)=XCRN(I)
        YCRN(I-4)=YCRN(I)
        ZCRN(I-4)=ZCRN(I)
        DO 190 I=6,8
          XCRN(I)=XCRN(I-4)+DZC(ICR,IDZ)*DCOSC(ICR,3,1)
          YCRN(I)=YCRN(I-4)+DZC(ICR,IDZ)*DCOSC(ICR,3,2)
          ZCRN(I)=ZCRN(I-4)+DZC(ICR,IDZ)*DCOSC(ICR,3,3)
          CALL ORTTRN(XCRN(I),YCRN(I),ZCRN(I),TEMAT,AX,AY,AZ,IERR)
          CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
          iupdown=-204
          isym=0
          CALL ETPLOT(XS(I),YS(I),iupdown,isym)
          XCRN(I-4)=XCRN(I)
          YCRN(I-4)=YCRN(I)
          ZCRN(I-4)=ZCRN(I)
  190   CONTINUE
        iupdown=-204
        isym=0
        CALL ETPLOT(XS(5),YS(5),iupdown,isym)
  180 CONTINUE
      call forceflush()

      RETURN
      END

C **************************   CONCRN   *******************************
C CONCRN displays the corner construction menu.
C *********************************************************************
      SUBROUTINE CONCRN(IZ,ICR)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PREC8/SLAT,SLON

      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)

      LOGICAL OK,DOK,chdb

      DIMENSION ITEM(15),IUDLR(5)

      CHARACTER*72 PROPS,DISCR
      CHARACTER ITEM*30,SZN*12,SRF(3)*12,LMTS*32
      CHARACTER TEXT*124,BAR*1,outs*124
      real DXX,DYX   ! To avoid variable name clash with DX from geometry.h
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CONCRN'  ! set for subroutine

C Define the three surfaces.
      IS1=ISFCRN(ICR,1)
      IS2=ISFCRN(ICR,2)
      IS3=ISFCRN(ICR,3)
      icon1=IZSTOCN(iz,is1)
      icon2=IZSTOCN(iz,is2)
      icon3=IZSTOCN(iz,is3)
      SZN=zname(IZ)
      SRF(1)=SSNAME(icon1)
      SRF(2)=SSNAME(icon2)
      SRF(3)=SSNAME(icon3)
      IL=1
      IF(MMOD.EQ.8)CALL FHCCRN(IZ,ICR,IL)
      INO=-3

   10 WRITE(ITEM(1),'(A,A)')'  zone       : ',SZN
      WRITE(ITEM(2),'(A,A)')'  surface (1): ',SRF(1)
      WRITE(ITEM(3),'(A,A)')'  surface (2): ',SRF(2)
      WRITE(ITEM(4),'(A,A)')'  surface (3): ',SRF(3)
      WRITE(ITEM(5),'(A,I2)')'  mesh level : ',IL
      ITEM(6)='  ----------------------------'
      ITEM(7)='1 show next mesh level      '
      ITEM(8)='2 define X - gridding       '
      ITEM(9)='3 define Y - gridding       '
      ITEM(10)='4 create material rectangle '
      ITEM(11)='5 copy level construction   '
      ITEM(12)='  ----------------------------'
      ITEM(13)='l list construction details '
      ITEM(14)='? help                      '
      ITEM(15)='- exit                      '
      MITEM=15

C Help text for this menu.  << to here >>
      helptopic='grd_corner_constr'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Draw the appropriate graph.
      IF(MODIFYVIEW)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('    Corner  Construction',ITEM,MITEM,INO)
      IF(INO.EQ.7)THEN

C Show the next mesh level.
        IL=IL+1
        IF(IL.GT.NDZC(ICR))IL=1
        IF(MMOD.EQ.8)CALL FHCCRN(IZ,ICR,IL)
      ELSEIF(INO.EQ.8)THEN

C Define X - gridding.
        THK1=0.
        DO 20 I=1,NDXC(ICR)
          THK1=THK1+DXC(ICR,I)
   20   CONTINUE
        WRITE(outs,'(A,F12.5,A)')' The actual length which is (',
     &                           THK1,') meters.'
        call edisp(iuout,outs)
        NDX=NDXC(ICR)
        WRITE(DISCR,'(1X,10F9.3)')(DXC(ICR,J),J=1,NDX)
        CALL EASKS(DISCR,'Define the X - gridding intervals (m).',
     &    ' ',72,' ',' gridding intervals ',IER,nbhelp)
        CALL USRMSG(' ',' ','-')
        NDX = iCountWords(DISCR)
        IF(NDX.GT.MZE)THEN
          CALL USRMSG(' ',' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        TDX=0.
        DO 30 I=1,NDX
          CALL EGETWR(DISCR,K,DXX,0.,THK1,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DXC(ICR,I)=DXX
          TDX=TDX+DXX
          IF((TDX-THK1).GT.1.E-4)THEN
            CALL USRMSG('the summation of entered intervals',
     &                  'is heigher than the actual length','W')
            CALL USRMSG(' ',' ','-')
            GOTO 10
          ENDIF
   30   CONTINUE
        IF((THK1-TDX).GT.1.E-4)THEN
          CALL USRMSG('the summation of entered intervals',
     &                'is less than the actual length','W')
          CALL USRMSG(' ',' ','-')
          GOTO 10
        ENDIF
        IF(NDXC(ICR).NE.NDX)THEN
          NDXC(ICR)=NDX
          IF(MMOD.EQ.8)CALL FHCCRN(IZ,ICR,IL)
        ENDIF
      ELSEIF(INO.EQ.9)THEN

C Define Y - gridding.
        THK2=0.
        DO 40 I=1,NDYC(ICR)
          THK2=THK2+DYC(ICR,I)
   40   CONTINUE
        WRITE(outs,'(A,F12.5,A)')' The actual length which is (',
     &    THK2,') meters.'
        call edisp(iuout,outs)
        NDY=NDYC(ICR)
        WRITE(DISCR,'(1X,10F9.3)')(DYC(ICR,J),J=1,NDY)
        CALL EASKS(DISCR,'Define the X - gridding intervals (m).',
     &    ' ',72,' ',' gridding intervals ',IER,nbhelp)
        CALL USRMSG(' ',' ','-')
        NDY = iCountWords(DISCR)
        IF(NDY.GT.MZE)THEN
          CALL USRMSG(' ',
     &              ' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        TDY=0.
        DO 50 I=1,NDY
          CALL EGETWR(DISCR,K,DYX,0.,THK2,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DYC(ICR,I)=DYX
          TDY=TDY+DYX
          IF((TDY-THK2).GT.1.E-4)THEN
            CALL USRMSG('the summation of entered intervals',
     &                  'is heigher than the actual length','W')
            CALL USRMSG(' ',' ','-')
            GOTO 10
          ENDIF
   50   CONTINUE
        IF((THK2-TDY).GT.1.E-4)THEN
          CALL USRMSG('the summation of entered intervals',
     &                'is less than the actual length','W')
          CALL USRMSG(' ',' ','-')
          GOTO 10
        ENDIF
        IF(NDYC(ICR).NE.NDY)THEN
          NDYC(ICR)=NDY
          IF(MMOD.EQ.8)CALL FHCCRN(IZ,ICR,IL)
        ENDIF
      ELSEIF(INO.EQ.10)THEN

C Create a material rectangle.

C Ask if user wishes to browse through materials to find suitable
C reference. Use elistmat to select via materials array.
        CALL ASKOK('Browse the materials db for a new material',
     &    '(NO exits the process)?',OK,dok,nbhelp)
        IF(OK)then
          iwhich = 0
          CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
          if(iwhich.eq.0)then
            CALL EASKABC('Your selection is `0` i.e. air. Options:',
     &        ' ','accept','reselect material','abort',iwair,nbhelp)
            if(iwair.eq.2)then
              CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
            elseif(iwair.eq.3)then
              GOTO 10
            endif
          elseif(iwhich.eq.-99)then
            continue
          endif
        else
          goto 10  ! go back
        endif

        IUDLR(1)=IWHICH
        WRITE(LMTS,'(4(1X,I2))')(IUDLR(K),K=2,5)
        WRITE(PROPS,'(2A)')'enter the material rectangle borders',
     &                      ' (left, down, right, and up)'
        CALL EASKS(LMTS,PROPS,' ',32,' ',' rectangle limits ',
     &    IER,nbhelp)
        CALL USRMSG(' ',' ','-')
        NDZ = iCountWords(LMTS)
        K=0
        DO 60 L=2,5
          CALL EGETWI(LMTS,K,IUDLR(L),1,100,'F',
     &                 'rectangle limits',IER)
          IF(IER.NE.0)GOTO 10
   60   CONTINUE
        DO 70 IY=IUDLR(3),IUDLR(5)
          DO 80 IX=IUDLR(2),IUDLR(4)
            ITPCRN(ICR,IX,IY,IL)=IUDLR(1)
   80     CONTINUE
   70   CONTINUE
        IF(MMOD.EQ.8)CALL FHCCRN(IZ,ICR,IL)
      ELSEIF(INO.EQ.11)THEN

C Copy the construction data of existing corner level.
        CALL EASKI(IWCH,' ',' Copy form which level:',1,'F',NDZC(ICR),
     &    'F',1,'level number',IER,nbhelp)
        DO 90 IY=1,NDYC(ICR)
        DO 90 IX=1,NDXC(ICR)
          ITPCRN(ICR,IX,IY,IL)=ITPCRN(ICR,IX,IY,IWCH)
   90   CONTINUE
        CALL FHCCRN(IZ,ICR,IL)
      ELSEIF(INO.EQ.(MITEM-2))THEN

C List construction details.
        BAR='I'
        icon=IZSTOCN(iz,is2)
        WRITE(TEXT,'(A12,4X,A1,10A)') SSNAME(icon),
     &                    '-',('--------',IX=1,NDXC(ICR))
        CALL EDISP(IUOUT,TEXT)
        WRITE(TEXT,'(14X,2X,A1,8(2X,I3,2X,A1))')
     &        BAR,(ITPCRN(ICR,IX,NDYC(ICR),IL),BAR,IX=1,NDXC(ICR))
        CALL EDISP(IUOUT,TEXT)
        WRITE(TEXT,'(16X,A1,8A8)')'-',('--------',IX=1,NDXC(ICR))
        CALL EDISP(IUOUT,TEXT)
        DO 100 IY=NDYC(ICR)-1,1,-1
          WRITE(TEXT,'(16X,A1,8(2X,I3,2X,A1))')
     &          BAR,(ITPCRN(ICR,IX,IY,IL),BAR,IX=1,NDXC(ICR))
          CALL EDISP(IUOUT,TEXT)
          WRITE(TEXT,'(16X,A1,8A8)')'-',('--------',IX=1,NDXC(ICR))
          CALL EDISP(IUOUT,TEXT)
  100   CONTINUE
        icon=IZSTOCN(iz,is1)
        WRITE(TEXT,'(18X,A12)') SSNAME(icon)
        CALL EDISP(IUOUT,TEXT)
      ELSEIF(INO.EQ.(MITEM-1))THEN

C Display help statements.
        helptopic='grd_corner_constr'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(' corner construction ',8,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Exit.
        RETURN
      ENDIF
      GOTO 10
      END

C ***************************   FHCCRN   ******************************
C FHCCRN draws the free hand sketch for the cross section of a given 
C corner with it's gridding lines.
C *********************************************************************
      SUBROUTINE FHCCRN(IZ,ICR,IL)
#include "building.h"
#include "geometry.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)

      CHARACTER LABELN*3,LABELS*16
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer iix,iiy,iix1,iiy1,iix2,iiy2
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer*8 iix,iiy,iix1,iiy1,iix2,iiy2
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Return if not in graghical mode.
      IF(MMOD.LT.8)RETURN

C Define the two surfaces.
      IS1=ISFCRN(ICR,1)
      IS2=ISFCRN(ICR,2)

      XMN2=0.
      YMN2=0.
      XMX2=FLOAT(NDXC(ICR))
      YMX2=FLOAT(NDYC(ICR))
      XYMX=AMAX1(XMX2,YMX2)
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=30; iiw2=10; iiw3=5; iiw4=5; iimenu=29
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh
      CALL AXISCALE(igwid,igheight,XMN2,XMX2,YMN2,YMX2,XSC,YSC,SCA,
     &               XADD,YADD)
      CALL LINESCALE(iigl,XADD,SCA,iigb,YADD,SCA)
      CALL U2PIXEL(XMN2,YMN2,iix,iiy)
      CALL ELINE(iix,iiy,1)
      CALL ECIRC(iix,iiy,3,1)
      CALL U2PIXEL(XMX2,YMN2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL U2PIXEL(XMX2,YMX2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL U2PIXEL(XMN2,YMX2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL U2PIXEL(XMN2,YMN2,iix,iiy)
      CALL ELINE(iix,iiy,0)
      DO 10 I=1,NDXC(ICR)-1
        CALL U2PIXEL(FLOAT(I),YMN2,iix1,iiy1)
        CALL U2PIXEL(FLOAT(I),YMX2,iix2,iiy2)
        CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
   10 CONTINUE
      DO 20 J=1,NDYC(ICR)-1
        CALL U2PIXEL(XMN2,FLOAT(J),iix1,iiy1)
        CALL U2PIXEL(XMX2,FLOAT(J),iix2,iiy2)
        CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
   20 CONTINUE
      XCF=-0.5
      ipos=0
      isize=itfs
      DO 30 I=1,NDXC(ICR)
        XCF=XCF+1.
        YCF=-0.5
        DO 40 J=1,NDYC(ICR)
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,iix,iiy)
          WRITE(LABELS,'(I3)')ITPCRN(ICR,I,J,IL)
          CALL ETLABEL(LABELS,XCF,YCF,ipos,isize)
   40   CONTINUE
   30 CONTINUE
      iside=1
      isize=1
      ifont=2
      call viewtext('Material Types',iside,isize,ifont)
      XLAB=XMN2-XYMX/20.
      YLAB=YMN2-XYMX/20.
      WRITE(LABELN,'(A,I2)')'V',IVXCRN(ICR)
      ipos=2
      isize=itfs
      CALL ETLABEL(LABELN,XLAB,YLAB,ipos,isize)
      icon=IZSTOCN(iz,is1)
      WRITE(LABELS,'(A12)') SSNAME(icon)
      XLAB=XMX2/2.
      YLAB=YMN2-XYMX/10.
      ipos=2
      isize=itfs+2
      CALL ETLABEL(LABELS,XLAB,YLAB,ipos,isize)
      icon=IZSTOCN(iz,is2)
      WRITE(LABELS,'(A12)') SSNAME(icon)
      XLAB=XMN2-XMX2/10.
      YLAB=YMX2/2.
      ipos=1
      isize=itfs+2
      CALL ETLABEL(LABELS,XLAB,YLAB,ipos,isize)
      call forceflush()

      RETURN
      END

C **************************     INDCRN     ***************************
C INDCRN initializes the corners dimensions.
C *********************************************************************
      SUBROUTINE INDCRN
#include "building.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)

      LOGICAL OK1,OK2,OK3

      DO 100 ICR=1,NUMCRN
        IS1=ISFCRN(ICR,1)
        IS2=ISFCRN(ICR,2)
        IS3=ISFCRN(ICR,3)
        IV1=IVXCRN(ICR)
        DO 10 I123=1,3
          DCOSC(ICR,1,I123)=DCOSS(IS1,2,I123)
          DCOSC(ICR,2,I123)=DCOSS(IS2,2,I123)
          DCOSC(ICR,3,I123)=DCOSS(IS3,2,I123)
   10   CONTINUE
        OK1=.FALSE.
        OK2=.FALSE.
        OK3=.FALSE.

C First edge.
        CALL EDGNUM(IS1,IS2,IV1,IV1,IEG1)
        IF(ISFEDG(IEG1,1).EQ.IS1)THEN
          OK1=.TRUE.
          DO 110 IX=1,NDXE(IEG1)
            DXC(ICR,IX)=DXE(IEG1,IX)
  110     CONTINUE
          NDXC(ICR)=NDXE(IEG1)
        ELSEIF(ISFEDG(IEG1,1).EQ.IS2)THEN
          OK2=.TRUE.
          DO 120 IY=1,NDXE(IEG1)
            DYC(ICR,IY)=DXE(IEG1,IY)
  120     CONTINUE
          NDYC(ICR)=NDXE(IEG1)
        ELSEIF(ISFEDG(IEG1,1).EQ.IS3)THEN
          OK3=.TRUE.
          DO 130 IL=1,NDXE(IEG1)
            DZC(ICR,IL)=DXE(IEG1,IL)
  130     CONTINUE
          NDZC(ICR)=NDXE(IEG1)
        ENDIF
        IF(ISFEDG(IEG1,2).EQ.IS1)THEN
          OK1=.TRUE.
          DO 140 IX=1,NDYE(IEG1)
            DXC(ICR,IX)=DYE(IEG1,IX)
  140     CONTINUE
          NDXC(ICR)=NDYE(IEG1)
        ELSEIF(ISFEDG(IEG1,2).EQ.IS2)THEN
          OK2=.TRUE.
          DO 150 IY=1,NDYE(IEG1)
            DYC(ICR,IY)=DYE(IEG1,IY)
  150     CONTINUE
          NDYC(ICR)=NDYE(IEG1)
        ELSEIF(ISFEDG(IEG1,2).EQ.IS3)THEN
          OK3=.TRUE.
          DO 160 IL=1,NDYE(IEG1)
            DZC(ICR,IL)=DYE(IEG1,IL)
  160     CONTINUE
          NDZC(ICR)=NDYE(IEG1)
        ENDIF

C Second edge.
        CALL EDGNUM(IS2,IS3,IV1,IV1,IEG2)
        IF(.NOT.OK1.AND.ISFEDG(IEG2,1).EQ.IS1)THEN
          OK1=.TRUE.
          DO 210 IX=1,NDXE(IEG2)
            DXC(ICR,IX)=DXE(IEG2,IX)
  210     CONTINUE
          NDXC(ICR)=NDXE(IEG2)
        ELSEIF(.NOT.OK2.AND.ISFEDG(IEG2,1).EQ.IS2)THEN
          OK2=.TRUE.
          DO 220 IY=1,NDXE(IEG2)
            DYC(ICR,IY)=DXE(IEG2,IY)
  220     CONTINUE
          NDYC(ICR)=NDXE(IEG2)
        ELSEIF(.NOT.OK3.AND.ISFEDG(IEG2,1).EQ.IS3)THEN
          OK3=.TRUE.
          DO 230 IL=1,NDXE(IEG2)
            DZC(ICR,IL)=DXE(IEG2,IL)
  230     CONTINUE
          NDZC(ICR)=NDXE(IEG2)
        ENDIF
        IF(.NOT.OK1.AND.ISFEDG(IEG2,2).EQ.IS1)THEN
          OK1=.TRUE.
          DO 240 IX=1,NDYE(IEG2)
            DXC(ICR,IX)=DYE(IEG2,IX)
  240     CONTINUE
          NDXC(ICR)=NDYE(IEG2)
        ELSEIF(.NOT.OK2.AND.ISFEDG(IEG2,2).EQ.IS2)THEN
          OK2=.TRUE.
          DO 250 IY=1,NDYE(IEG2)
            DYC(ICR,IY)=DYE(IEG2,IY)
  250     CONTINUE
          NDYC(ICR)=NDYE(IEG2)
        ELSEIF(.NOT.OK3.AND.ISFEDG(IEG2,2).EQ.IS3)THEN
          OK3=.TRUE.
          DO 260 IL=1,NDYE(IEG2)
            DZC(ICR,IL)=DYE(IEG2,IL)
  260     CONTINUE
          NDZC(ICR)=NDYE(IEG2)
        ENDIF
        IF(.NOT.OK1.OR..NOT.OK2.OR..NOT.OK3)
     &  STOP "error (303): corner default gridding."
        THKX=0.
        DO 310 IX=1,NDXC(ICR)
          THKX=THKX+DXC(ICR,IX)
  310   CONTINUE
        THKY=0.
        DO 320 IY=1,NDYC(ICR)
          THKY=THKY+DYC(ICR,IY)
  320   CONTINUE
        THKZ=0.
        DO 330 IL=1,NDZC(ICR)
          THKZ=THKZ+DZC(ICR,IL)
  330   CONTINUE
  100 CONTINUE
      RETURN
      END

C ***************************   CRNNUM   ******************************
C CRNNUM defines the number of the corner in a zone from it's associated
C three surfaces. If the corner does not exist a new one will be created.
C *******************************************************************
      SUBROUTINE CRNNUM(IS1,IS2,IS3,IV1,NUM)
#include "building.h"

      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)

      IF(IS1.EQ.IS2.OR.IS1.EQ.IS3.OR.IS2.EQ.IS3)
     &STOP "error (304): a corner with two similar surfaces detected."
      DO 10 ICR=1,NUMCRN
        IVRT1=IVXCRN(ICR)
        IF(IV1.EQ.IVRT1)THEN
          ISRF1=ISFCRN(ICR,1)
          ISRF2=ISFCRN(ICR,2)
          ISRF3=ISFCRN(ICR,3)
          IF((IS1.EQ.ISRF1.OR.IS1.EQ.ISRF2.OR.IS1.EQ.ISRF3).AND.
     &       (IS2.EQ.ISRF1.OR.IS2.EQ.ISRF2.OR.IS2.EQ.ISRF3).AND.
     &       (IS3.EQ.ISRF1.OR.IS3.EQ.ISRF2.OR.IS3.EQ.ISRF3))THEN
             NUM=ICR
             IS1=ISFCRN(ICR,1)
             IS2=ISFCRN(ICR,2)
             IS3=ISFCRN(ICR,3)
             RETURN
          ENDIF
        ENDIF
   10 CONTINUE
      NUMCRN=NUMCRN+1
      IF(NUMCRN.GT.MCZ)
     &STOP "error (305): maximum corner numbers exceeded."
      NUM=NUMCRN
      IVXCRN(ICR)=IV1
      ISFCRN(ICR,1)=IS1
      ISFCRN(ICR,2)=IS2
      ISFCRN(ICR,3)=IS3
      RETURN
      END

C ***************************   TOTCRN   ******************************
C TOTCRN determines the total number of corners in the zone.
C *********************************************************************
      SUBROUTINE TOTCRN
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)

      LOGICAL YES

C Initialize the total number of cornerss in the current zone.
      NCRN=0

C For each vertex in the current zone.
      DO 10 IV=1,NTV
        ICNT=0

C For each surface in the current zone.
        DO 20 IS1=1,NSUR
          CALL CMVRTS(IV,IS1,YES)
          IF(YES)THEN
            DO 30 J=1,ICNT
              IS2=ISFCRN(NCRN+1,J)
              DELTA=0.
              DO 40 I123=1,3
                DELTA=DELTA+ABS(DCOSS(IS2,2,I123)-DCOSS(IS1,2,I123))
   40         CONTINUE

C The two surfaces are in the same plane (bypass).
              IF(DELTA.LT.0.01)GOTO 20
   30       CONTINUE
            ICNT=ICNT+1
            ISFCRN(NCRN+1,ICNT)=IS1
            IF(ICNT.EQ.3)THEN
              NCRN=NCRN+1
              IVXCRN(NCRN)=IV
              DO 50 IXYZ=1,3
              DO 50 I123=1,3
                DCOSC(NCRN,IXYZ,I123)=DCOSS(ISFCRN(NCRN,IXYZ),2,I123)
   50         CONTINUE
              ICNT=0
              GOTO 10
            ENDIF
          ENDIF
   20   CONTINUE
   10 CONTINUE
      NUMCRN=NCRN
      RETURN
      END
