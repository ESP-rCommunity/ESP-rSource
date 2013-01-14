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

C Contains: grid visualization routines.
C  CGD - domain grid visualization.
C  VGRID1D - draws the CFD domain gridding in 1d.
C  VGRID2D - draws the CFD domain gridding in 2d.
C  VGRID3D - draws the CFD domain gridding in 3d.
C  VSB3D - draws the solid boundaries - highlighting the chosen one.
C  VAO3D - draws the air openings - highlighting the chosen one.

C ********************* CGD *********************
C CGD - domain grid visualization.
C IC = zone number (if in dfs set to 1)
      subroutine cgd(IC,IER)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      CHARACTER ITEM(16)*32
      character outs*124,ETEXT*30

      logical GRMODE
      integer NITMS,INO  ! max items and current menu item

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

      helpinsub='cgd'  ! set for subroutine

C Set default values.
C GRMODE = graphics mode.
      GRMODE=.false.
      XZ=0.
      YZ=0.
      ZZ=0.

C Set additional output units to stdout. Then redirect warning 
C messages to stderr in case of rule script program control.
      IMFTU=IUOUT
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Reset the display bounds for grid display.
      IF(MMOD.EQ.8)THEN
        call startbuffer()

C Setup and pass in parameters to win3d.
        iiw1=6; iiw2=6; iiw3=2; iiw4=4; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        GRMODE=.true.
      ENDIF
      CALL INICNT
      CALL GRID(ier)

C Let the user pick a menu item.
   1  INO=-5
      ITEM(1) ='a Plot x-y plane (2D) grid     '
      ITEM(2) ='b Plot x-z plane (2D) grid     '
      ITEM(3) ='c Plot y-z plane (2D) grid     '
      ITEM(4) =' --3D grid options -------     '
      ITEM(5) ='d  geometry+grid               '
      ITEM(6) ='e  geometry+grid+solid         '
      ITEM(7) ='f  geometry+grid+solid+openings'
      ITEM(8) ='g  only grid                   '
      ITEM(9) ='h  only grid+solid             '
      ITEM(10)='i  only grid+solid+openings    '
      ITEM(11)='  ------------------------     '
      ITEM(12)='j List x plane coordinates     '
      ITEM(13)='k List y plane coordinates     '
      ITEM(14)='l List z plane coordinates     '
      ITEM(15)='? Help                         '
      ITEM(16)='- Exit                         '
      NITMS=16

C Help text for menu.
      helptopic='dfs_display_list_grid'
      call gethelptext(helpinsub,helptopic,nbhelp)

   12 CALL EMENU('Grid visualisation',ITEM,NITMS,INO)
      IF(INO.EQ.0)THEN

C Wrong pick.
        INO=-1
        GOTO 12
      ELSEIF(INO.EQ.1.and.GRMODE)THEN 

C x-y plane grid.
        WRITE(ETEXT,'(A,A15)') ' X-Y grid for ',ZNAME(ic)
        call VGRID2D(XZ,YZ,XU,YV,NI,NJ,ETEXT,'X axis (m)','Y')
      ELSEIF(INO.EQ.2.and.GRMODE)THEN

C x-z plane grid.
        WRITE(ETEXT,'(A,A15)') ' X-Z grid for ',ZNAME(ic)
        call VGRID2D(XZ,ZZ,XU,ZW,NI,NK,ETEXT,'X axis (m)','Z')
      ELSEIF(INO.EQ.3.and.GRMODE)THEN

C y-z plane grid.
        WRITE(ETEXT,'(A,A15)') ' Y-Z grid for ',ZNAME(ic)
        call VGRID2D(YZ,ZZ,YV,ZW,NJ,NK,ETEXT,'Y axis (m)','Z')
      ELSEIF(INO.EQ.5.and.GRMODE)THEN

C 3D gridding with zone geometry.
        CALL EASKABCD('Options for 3D gridding:',' ','all faces',
     &    'south->north','east->west','high->low',IWO,nbhelp)
        if(iwo.eq.1)then
          call VGRID3D('zn ','xyz',ic)
        elseif(iwo.eq.2)then
          call VGRID3D('zn ','xzg',ic)
        elseif(iwo.eq.3)then
          call VGRID3D('zn ','yzg',ic)
        elseif(iwo.eq.4)then
          call VGRID3D('zn ','xyg',ic)
        endif
      ELSEIF(INO.EQ.6.and.GRMODE)THEN

C 3D gridding with zone geometry and solids.
        CALL EASKABCD('Options for 3D gridding:',' ','all faces',
     &    'south->north','east->west','high->low',IWO,nbhelp)
        if(iwo.eq.1)then
          call VGRID3D('zn ','xyz',ic)
        elseif(iwo.eq.2)then
          call VGRID3D('zn ','xzg',ic)
        elseif(iwo.eq.3)then
          call VGRID3D('zn ','yzg',ic)
        elseif(iwo.eq.4)then
          call VGRID3D('zn ','xyg',ic)
        endif
        call VSB3D(0,ic)
      ELSEIF(INO.EQ.7.and.GRMODE)THEN

C 3D gridding with zone geometry and solids and openings.
        CALL EASKABCD('Options for 3D gridding:',' ','all faces',
     &    'south->north','east->west','high->low',IWO,nbhelp)
        if(iwo.eq.1)then
          call VGRID3D('zn ','xyz',ic)
        elseif(iwo.eq.2)then
          call VGRID3D('zn ','xzg',ic)
        elseif(iwo.eq.3)then
          call VGRID3D('zn ','yzg',ic)
        elseif(iwo.eq.4)then
          call VGRID3D('zn ','xyg',ic)
        endif
        call VSB3D(0,ic)
        call VAO3D(0)
      ELSEIF(INO.EQ.8.and.GRMODE)THEN

C 3D gridding without zone geometry.
        CALL EASKABCD('Options for 3D gridding:',' ','all faces',
     &    'south->north','east_.west','high->low',IWO,nbhelp)
        if(iwo.eq.1)then
          call VGRID3D('---','xyz',ic)
        elseif(iwo.eq.2)then
          call VGRID3D('---','xzg',ic)
        elseif(iwo.eq.3)then
          call VGRID3D('---','yzg',ic)
        elseif(iwo.eq.4)then
          call VGRID3D('---','xyg',ic)
        endif
      ELSEIF(INO.EQ.9.and.GRMODE)THEN

C 3D gridding without zone geometry and with solids.
        CALL EASKABCD('Options for 3D gridding:',' ','all faces',
     &    'south->north','east->west','high->low',IWO,nbhelp)
        if(iwo.eq.1)then
          call VGRID3D('---','xyz',ic)
        elseif(iwo.eq.2)then
          call VGRID3D('---','xzg',ic)
        elseif(iwo.eq.3)then
          call VGRID3D('---','yzg',ic)
        elseif(iwo.eq.4)then
          call VGRID3D('---','xyg',ic)
        endif
        call VSB3D(0,ic)
      ELSEIF(INO.EQ.10.and.GRMODE)THEN

C 3D gridding without zone geometry and with solids and openings.
        CALL EASKABCD('Options for 3D gridding:',' ','all faces',
     &    'south->north','east->west','high->low',IWO,nbhelp)
        if(iwo.eq.1)then
          call VGRID3D('---','xyz',ic)
        elseif(iwo.eq.2)then
          call VGRID3D('---','xzg',ic)
        elseif(iwo.eq.3)then
          call VGRID3D('---','yzg',ic)
        elseif(iwo.eq.4)then
          call VGRID3D('---','xyg',ic)
        endif
        call VSB3D(0,ic)
        call VAO3D(0)
      ELSEIF(INO.EQ.12)THEN

C Text listings
        call edisp(iuout,'Grid in the X axis... ')
        outs='Cell index, X start & end,   distance, aspect ratio'
        call edisp(iuout,outs)
        asprat=1.0 
        DO 101 I=2,(NI-1)
          if(i.ne.2)xdprev=xd
          xd=XU(I+1)-XU(I)
          if(i.ne.2)then 
            asprat=xd/(xdprev+1.0E-20)
            if(asprat.lt.1.0) asprat=1.0/asprat 
          endif 
          write(outs,'(i7,4f10.4)') I-1,XU(I)+XZ,XU(I+1)+XZ,xd, 
     &          asprat 
          call edisp(iuout,outs)
  101   CONTINUE

      ELSEIF(INO.EQ.13)THEN
        call edisp(iuout,'Grid in the Y axis... ')
        outs='Cell index, Y start & end,   distance, aspect ratio'
        call edisp(iuout,outs)
        asprat=1.0 
        DO 102 I=2,(NJ-1)
          if(i.ne.2)ydprev=yd 
          yd=YV(I+1)-YV(I)
          if(i.ne.2)then 
            asprat=yd/(ydprev+1.0E-20)
            if(asprat.lt.1.0) asprat=1.0/asprat 
          endif 
          write(outs,'(i7,4f10.4)') I-1,YV(I)+YZ,YV(I+1)+YZ,yd,
     &          asprat 
          call edisp(iuout,outs)
  102   CONTINUE
      ELSEIF(INO.EQ.14)THEN
        call edisp(iuout,'Grid in the Z axis... ')
        outs='Cell index, Z start & end,   distance, aspect ratio'
        call edisp(iuout,outs)
        asprat=1.0 
        DO 103 I=2,(NK-1)
          if(i.ne.2)zdprev=zd 
          zd=ZW(I+1)-ZW(I)
          if(i.ne.2)then 
            asprat=zd/(zdprev+1.0E-20)
            if(asprat.lt.1.0) asprat=1.0/asprat 
          endif 
          write(outs,'(i7,4f10.4)') I-1,ZW(I)+ZZ,ZW(I+1)+ZZ,zd,
     &          asprat 
          call edisp(iuout,outs)
  103   CONTINUE
      ELSEIF(INO.EQ.15)THEN
        helptopic='dfs_display_list_grid'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('grid visualization',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.16)THEN

C Finish grid visualization, returning display to initial state.
        IF(MMOD.EQ.8)THEN
          call startbuffer()

C Setup and pass in parameters to win3d.
          iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
C          CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        ENDIF
        return
      ELSE 
        INO=-1
        GOTO 12
      ENDIF
      GOTO 1

      end


C ********************* VGRID1D *********************
C VGRID1D - draws the CFD domain gridding in 1d for the chosen direction.
C xoff,yoff - offsets usually 0.0
C TOTDIS    - total domain length
C CTDIS     - defined length so far
C NRG       - no of regions
C IAX       - axis ID
C ICFD      - cfd domain id
C NCEL      - no of cells in each gridding region
C REG       - length of each gridding region
C plaw      - power law coeff of each gridding region
C title     - title of plot
C xax       - text for horiz axis.
      SUBROUTINE VGRID1D(xoff,yoff,TOTDIS,CTDIS,NRG,IAX,ICFD,NCEL,REG,
     &                   plaw,TITLE,xax)
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      character title*30,xax*10

      dimension NCEL(MNREG,MNZ),plaw(MNREG,MNZ)
      dimension NRG(4,MNZ),REG(MNREG,MNZ)
      dimension xgrd(MCEL1D)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1
#endif

C x-z plane grid.
      call startbuffer()

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
      Xmin=xoff
      Xmax=amax1(TOTDIS,CTDIS)
      Xmax=Xmax+xoff
      Ymin=yoff
      Ymax=yoff+1.0
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,
     &  Xadd,Yadd)

C Store the scaling parameters used in drawing the axes.
      CALL linescale(iigl,Xadd,sca,iigb,Yadd,sca)
      iiw1=iigb+10
      call horaxis(Xmin,Xmax,iigl,iigr,iiw1,Xadd,sca,0,xax)
      iside=1
      isize=0
      ifont=1
      call viewtext(title,iside,isize,ifont)

C Draw a bold box around domain.
      IPEN=-302
      iupdown=0
      isym=0
      CALL etplot(xoff,yoff,iupdown,isym)
      iupdown=IPEN
      CALL etplot(xoff,yoff+1.0,iupdown,isym)
      CALL etplot(xoff+TOTDIS,yoff+1.0,iupdown,isym)
      CALL etplot(xoff+TOTDIS,yoff,iupdown,isym)
      CALL etplot(xoff,yoff,iupdown,isym)

C Draw solid lines at end of each region.
      NCTOT=0
      xg=0.0
      do 10 I=1,NRG(IAX,ICFD)
        NCTOT=NCTOT+abs(NCEL(I,ICFD))
        IPEN=1
        iupdown=0
        isym=0
        CALL etplot(xg+xoff,yoff,iupdown,isym)
        iupdown=IPEN
        CALL etplot(xg+xoff,yoff+1.0,iupdown,isym)
        xg=xg+REG(I,ICFD)
 10   continue
      iupdown=0
      isym=0
      CALL etplot(xg+xoff,yoff,iupdown,isym)
      iupdown=IPEN
      CALL etplot(xg+xoff,yoff+1.0,iupdown,isym)

C For each region calculate gridding.
C Call griddist then plot.
      ICEL=2
      xgrd(ICEL)=0.0
      do 20 IREG=1,NRG(IAX,ICFD)
        call GRIDIST(IREG,ICFD,PLAW,NCEL,REG,xgrd,ICEL,MCEL1D)
        ICEL=ICEL+ABS(NCEL(IREG,ICFD))
 20   continue
      xgrd(1)=-xgrd(3)

C Now plot the grid lines.
      DO 30 I=2,NCTOT+2
        IPEN=-204
        iupdown=0
        isym=0
        CALL etplot(xgrd(I)+xoff,yoff,iupdown,isym)
        iupdown=IPEN
        CALL etplot(xgrd(I)+xoff,yoff+1.0,iupdown,isym)
   30 CONTINUE

      return
      end


C ********************* VGRID2D *********************
C VGRID2D - draws the CFD domain gridding in 2d for the chosen direction.
C xoff,yoff - offsets usually 0.0
C xgrd,ygrd - grid cell points
C nx,ny     - number of grid cell points
C title     - title of plot
C xax,yax   - text for horiz and vert axis.
      SUBROUTINE VGRID2D(xoff,yoff,xgrd,ygrd,nx,ny,title,xax,yax)
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      character title*30,xax*10,yax*1

      dimension xgrd(MCEL1D),ygrd(MCEL1D)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C x-z plane grid.
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=4; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

      Xmin=xoff
      Xmax=xgrd(nx)+xoff
      Ymin=yoff
      Ymax=ygrd(ny)+yoff
      call axiscale(igwid,igheight,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,
     &  Xadd,Yadd)

C Store the scaling parameters used in drawing the axes.
      CALL linescale(iigl,Xadd,sca,iigb,Yadd,sca)
      iiw1=iigb+10
      call horaxis(Xmin,Xmax,iigl,iigr,iigb,Xadd,sca,0,xax)
      if((Ymax-Ymin).lt.2.0)then
        iiw1=iigl-10
        call vrtaxis(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,0,0,yax)
        iiw1=iigr-15
        call vrtaxis(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,0,1,yax)
      else
        iiw1=iigl-10
        call vrtaxis(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,1,0,yax)
        iiw1=iigr-15
        call vrtaxis(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,1,1,yax)
      endif
      iside=1
      isize=0
      ifont=1
      call viewtext(title,iside,isize,ifont)
      DO 30 I=2,nx
        IPEN=-204
        if (I.eq.2.or.I.eq.nx) IPEN=1
        iupdown=0
        isym=0
        CALL etplot(xgrd(I)+xoff,yoff,iupdown,isym)
        iupdown=IPEN
        CALL etplot(xgrd(I)+xoff,ygrd(ny)+yoff,iupdown,isym)
   30 CONTINUE
      DO 40 K=2,ny
        IPEN=-204
        if (K.eq.2.or.K.eq.ny) IPEN=1
        iupdown=0
        isym=0
        CALL etplot(xoff,ygrd(K)+yoff,iupdown,isym)
        iupdown=IPEN
        CALL etplot(xgrd(nx)+xoff,ygrd(K)+yoff,iupdown,isym)
   40 CONTINUE

      return
      end


C ********************* VGRID3D *********************
C VGRID3D - draws the CFD domain gridding in 3d. The paremater
C act has the following meanings:
C   zact = zn then draw the zone geometry as well.
C   zact = - do not draw zone geometry.
C   gact = xyg then draw only top and bottom face grid
C   gact = yzg then draw only east and west face grid
C   gact = xzg then draw only north and south face grid
C   gact = xyz then draw all face grids
C izone is the index of the zone to be drawin (if zero do not bother`).
      SUBROUTINE VGRID3D(zact,gact,izone)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "cfd.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)

      logical greyok
      character gact*3,zact*3
      real XPP,YPP,ZPP  ! points taking into account zone minimum
      real X0,Y0,Z0

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iixs,iiys,iix,iiy,iixlast,iiylast
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iixs,iiys,iix,iiy,iixlast,iiylast
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Check in graphics mode.
      IF(MMOD.ne.8) return

C If libraries and monitor support greyscale then reset forground
C to 50% grey when drawing dots.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Set up grid information.
      call SETGEO

C Clear current viewing box and re-establish image bounds.
      CALL startbuffer

C Setup and pass in parameters to win3d.
      iiw1=4; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Get bounds for image.
      HANG=ANG/2.0
      HITH=1.0
      YON=1300.0
      CALL LENS(IER)

C Determine scaling ratios for the drawing and axis.
C Determine the 2D coords of the site extremes.
      CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
      CALL axiscale(igwid,igheight,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,
     &  Xadd,Yadd)
      call linescale(iigl,Xadd,sca,iigb,Yadd,sca)

C Loop through all X-Y plane grid lines.
      if(gact.eq.'xyz'.or.gact.eq.'xyg')then
        do 10 I=2,NI
          do 20 J=2,NJ

C Transform first point to screen coords.
            XPP=ZXMN(izone)+XU(i); YPP=ZYMN(izone)+YV(j)
            ZPP=ZZMN(izone)+0.
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixs,iiys)

C Transform first point to screen coords.
            XPP=ZXMN(izone)+XU(i); YPP=ZYMN(izone)+YV(j)
            ZPP=ZZMN(izone)+ZW(NK)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iix,iiy)

C Draw dotted line.
            if (I.eq.2.or.I.eq.NI.or.J.eq.2.or.J.eq.NJ) then
              if ((I.eq.2.and.J.eq.2).or.(I.eq.NI.and.J.eq.NJ).or.
     &            (I.eq.2.and.J.eq.NJ).or.(I.eq.NI.and.J.eq.2)) then
                call eline(iixs,iiys,1)
                call eline(iix,iiy,0)
              else
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif

C Draw the dotted line after casting to local variable.
                call edline(iixs,iiys,iix,iiy,5)
                if(greyok)then
                  iicol=0
                  call winscl('-',iicol)
                endif
                call forceflush()
              endif
            endif
 20       continue
 10     continue
      endif

C Loop through all Z-Y plane grid lines.
      if(gact.eq.'xyz'.or.gact.eq.'yzg')then
        do 30 K=2,NK
          do 40 J=2,NJ

C Transform first point to screen coords.
            XPP=ZXMN(izone)+0.; YPP=ZYMN(izone)+YV(j)
            ZPP=ZZMN(izone)+ZW(k)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixs,iiys)

C Transform first point to screen coords.
            XPP=ZXMN(izone)+XU(NI); YPP=ZYMN(izone)+YV(j)
            ZPP=ZZMN(izone)+ZW(k)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iix,iiy)

C Draw line to end point.
            if (K.eq.2.or.K.eq.NK.or.J.eq.2.or.J.eq.NJ) then
              if ((K.eq.2.and.J.eq.2).or.(K.eq.NK.and.J.eq.NJ).or.
     &            (K.eq.2.and.J.eq.NJ).or.(K.eq.NK.and.J.eq.2)) then
                call eline(iixs,iiys,1)
                call eline(iix,iiy,0)
              else
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                call edline(iixs,iiys,iix,iiy,4)
                if(greyok)then
                  iicol=0
                  call winscl('-',iicol)
                endif
                call forceflush()
              endif
            endif
 40       continue
 30     continue
      endif

C Loop through all Z-X plane grid lines.
      if(gact.eq.'xyz'.or.gact.eq.'xzg')then
        do 50 K=2,NK
          do 60 I=2,NI

C Transform first point to screen coords.
            XPP=ZXMN(izone)+XU(I); YPP=ZYMN(izone)+0.
            ZPP=ZZMN(izone)+ZW(k)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixs,iiys)

C Transform end point to screen coords.
            XPP=ZXMN(izone)+XU(I); YPP=ZYMN(izone)+YV(NJ)
            ZPP=ZZMN(izone)+ZW(k)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iix,iiy)

C Draw line to end point.
            if (K.eq.2.or.K.eq.NK.or.I.eq.2.or.I.eq.NI) then
              if ((K.eq.2.and.I.eq.2).or.(K.eq.NK.and.I.eq.NI).or.
     &            (K.eq.2.and.I.eq.NI).or.(K.eq.NK.and.I.eq.2)) then
                call eline(iixs,iiys,1)
                call eline(iix,iiy,0)
              else
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                call edline(iixs,iiys,iix,iiy,4)
                if(greyok)then
                  iicol=0
                  call winscl('-',iicol)
                endif
                call forceflush()
              endif
            endif
 60       continue
 50     continue
      endif

C If the zone geometry is requested then use some of the code from
C cread3dv.F to draw the surfaces of the zone in bold lines.
      if(zact(1:2).eq.'zn'.and.izone.ne.0)then

C Continue on with any surfaces in the zone.
  205   DO 1000 I=1,NZSUR(izone)
          icc=IZSTOCN(izone,I)

C Copy polygon for Surface I into structure A for compatibility with
C viewer format 'holes' in surfaces.
C NAP   = Number of vertex points on surface
C IANXT = Index of 'next' point
C IAPNT = Pointer to first vertex of polygon
          IAPNT = 1
          NAP = NZNVER(icc)
          DO 1100 J = 1,NZNVER(icc)
            K = NZJVN(icc,J)
            AX(J) = VCOORD(izone,K,1)
            AY(J) = VCOORD(izone,K,2)
            AZ(J) = VCOORD(izone,K,3)
            IANXT(J) = J + 1
 1100     CONTINUE

          IANXT(NZNVER(icc)) = IAPNT

C-----------------------------------------------------------------------
C Transform surface polygon to EYE co-ordinates
C Take structure A multiply by TEMAT return structure B
C TEMAT = Model to Eye Matrix
          CALL MATPOL(NAP,IAPNT,AX,AY,AZ,IANXT,TEMAT,
     &              SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)

C Generate clipping flags and clip geometry in eye coords.
C If ISTAT =  0 : totally inside frustrum
C If ISTAT =  1 : totally outside frustrum
C If ISTAT = -1 : straddles frustrum
          CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
          IF (ISTAT .EQ. 1) THEN
            GOTO  1000
          ELSEIF (ISTAT .EQ. -1) THEN
            CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
          ENDIF

C Debug.
C          write(6,*)'icc nap nbp nb',icc,nap,nbp,nb,
C     &      ISTOK,AX(1),AY(1),AZ(1),AX(2),AY(2),AZ(2)

C Count the edges in case enhanced edge drawing required.
          iedge=0
          DO 300 J = 1,NB
            IP = IABS(IBPNT)
            CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
            call u2pixel(AAX,AAY,iix,iiy)
            iixlast=iix
            iiylast=iiy

            IP1 = IBNXT(IP)

 351        CONTINUE
            iedge=iedge+1
            CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
            call u2pixel(AAX,AAY,iix,iiy)
            call edwline(iixlast,iiylast,iix,iiy)
            call forceflush()

C Repeat until next point is the first, remember position.
            IP1 = IBNXT(IP1)
            iixlast=iix
            iiylast=iiy
            IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
            iedge=iedge+1
            CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
            call u2pixel(AAX,AAY,iix,iiy)
            call edwline(iixlast,iiylast,iix,iiy)
            call forceflush()
  300     CONTINUE
          call forceflush()
 1000   CONTINUE
      endif

      return
      end


C ********************* VSB3D *********************
C VSB3D - draws the solid boundaries - highlighting the chosen one.
      SUBROUTINE VSB3D(ISBHL,izone)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)

      character LLOCAT*1,outs*124,temp*4
#ifdef OSI
      integer iixs,iiys,iix,iiy
#else
      integer*8 iixs,iiys,iix,iiy
#endif
      real XPP,YPP,ZPP  ! points taking into account zone minimum
      real XX,YY,ZZ

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NSB(ICFD).eq.0) return

C Draw round solid boundaries and last character of label.
      do 70 Isbn=1,NSB(ICFD)
        write (outs,'(i10)') abs(IWSB(Isbn,ICFD))
        write (LLOCAT,'(a)') outs(lnblnk(outs):lnblnk(outs))
        K=0
        call EGETWI(LLOCAT,K,ILOCAT,1,6,'F',
     &                                  'Illegal surface orient.',IER)
        if (ILOCAT.eq.1.or.ILOCAT.eq.2) then
          if (ILOCAT.eq.2) then
            MNOD=NI
          else
            MNOD=2
          endif

C In YZ plane.
C Transform local coords to screen coords and draw box around boundary.
          XPP=ZXMN(izone)+XU(MNOD); YPP=ZYMN(izone)+YV(JSBi(Isbn,ICFD))
          ZPP=ZZMN(izone)+ZW(KSBi(Isbn,ICFD))
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          XPP=ZXMN(izone)+XU(MNOD) 
          YPP=ZYMN(izone)+YV(JSBf(Isbn,ICFD)+1)
          ZPP=ZZMN(izone)+ZW(KSBi(Isbn,ICFD))
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(MNOD)
          YPP=ZYMN(izone)+YV(JSBf(Isbn,ICFD)+1)
          ZPP=ZZMN(izone)+ZW(KSBf(Isbn,ICFD)+1)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(MNOD); YPP=ZYMN(izone)+YV(JSBi(Isbn,ICFD))
          ZPP=ZZMN(izone)+ZW(KSBf(Isbn,ICFD)+1)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(MNOD); YPP=ZYMN(izone)+YV(JSBi(Isbn,ICFD))
          ZPP=ZZMN(izone)+ZW(KSBi(Isbn,ICFD))
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

C Label in center of boundary.
          YY=(YV(JSBi(Isbn,ICFD))+YV(JSBf(Isbn,ICFD)))/2.
          ZZ=(ZW(KSBi(Isbn,ICFD))+ZW(KSBf(Isbn,ICFD)))/2.
          CALL ORTTRN(XU(MNOD),YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a,i2)') 'SB',Isbn
          CALL wstxpt(iix,iiy,temp)
          call forceflush()

        elseif (ILOCAT.eq.3.or.ILOCAT.eq.4) then
          if (ILOCAT.eq.4) then
            MNOD=NJ
          else
            MNOD=2
          endif

C In XZ plane.
C Transform local coords to screen coords and draw box around boundary.
          XPP=ZXMN(izone)+XU(ISBi(Isbn,ICFD)); YPP=ZYMN(izone)+YV(MNOD)
          ZPP=ZZMN(izone)+ZW(KSBi(Isbn,ICFD))
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          XPP=ZXMN(izone)+XU(ISBf(Isbn,ICFD)+1)
          YPP=ZYMN(izone)+YV(MNOD)
          ZPP=ZZMN(izone)+ZW(KSBi(Isbn,ICFD))
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(ISBf(Isbn,ICFD)+1)
          YPP=ZYMN(izone)+YV(MNOD)
          ZPP=ZZMN(izone)+ZW(KSBf(Isbn,ICFD)+1)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(ISBi(Isbn,ICFD)); YPP=ZYMN(izone)+YV(MNOD)
          ZPP=ZZMN(izone)+ZW(KSBf(Isbn,ICFD)+1)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(ISBi(Isbn,ICFD)); YPP=ZYMN(izone)+YV(MNOD)
          ZPP=ZZMN(izone)+ZW(KSBi(Isbn,ICFD))
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

C Label in center of boundary.
          XX=(XU(ISBi(Isbn,ICFD))+XU(ISBf(Isbn,ICFD)))/2.
          ZZ=(ZW(KSBi(Isbn,ICFD))+ZW(KSBf(Isbn,ICFD)))/2.
          CALL ORTTRN(XX,YV(MNOD),ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a,i2)') 'SB',Isbn
          CALL wstxpt(iix,iiy,temp)
          call forceflush()
        elseif (ILOCAT.eq.5.or.ILOCAT.eq.6) then
          if (ILOCAT.eq.6) then
            MNOD=NK
          else
            MNOD=2
          endif

C In XY plane.
C Transform local coords to screen coords and draw box around boundary.
          XPP=ZXMN(izone)+XU(ISBi(Isbn,ICFD)) 
          YPP=ZYMN(izone)+YV(JSBi(Isbn,ICFD))
          ZPP=ZZMN(izone)+ZW(MNOD)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          XPP=ZXMN(izone)+XU(ISBf(Isbn,ICFD)+1)
          YPP=ZYMN(izone)+YV(JSBi(Isbn,ICFD))
          ZPP=ZZMN(izone)+ZW(MNOD)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(ISBf(Isbn,ICFD)+1)
          YPP=ZYMN(izone)+YV(JSBf(Isbn,ICFD)+1)
          ZPP=ZZMN(izone)+ZW(MNOD)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(ISBi(Isbn,ICFD))
          YPP=ZYMN(izone)+YV(JSBf(Isbn,ICFD)+1)
          ZPP=ZZMN(izone)+ZW(MNOD)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          XPP=ZXMN(izone)+XU(ISBi(Isbn,ICFD))
          YPP=ZYMN(izone)+YV(JSBi(Isbn,ICFD))
          ZPP=ZZMN(izone)+ZW(MNOD)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Isbn.eq.ISBHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

C Label in center of boundary.
          XX=(XU(ISBi(Isbn,ICFD))+XU(ISBf(Isbn,ICFD)))/2.
          YY=(YV(JSBi(Isbn,ICFD))+YV(JSBf(Isbn,ICFD)))/2.
          CALL ORTTRN(XX,YY,ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a,i2)') 'SB',Isbn
          CALL wstxpt(iix,iiy,temp)
          call forceflush()
        endif
 70   continue

      return
      end


C ********************* VAO3D *********************
C VAO3D - draws the air openings - highlighting the chosen one.
      SUBROUTINE VAO3D(IOPENHL)
#include "building.h"
#include "cfd.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),ICNACT(MCFND,MNZ)

      character LLOCAT*1,outs*124,temp*4
#ifdef OSI
      integer iixs,iiys,iix,iiy
#else
      integer*8 iixs,iiys,iix,iiy
#endif

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NOPEN(ICFD).eq.0)return

C Draw round solid boundaries and last character of label.
      do 70 Iaop=1,NOPEN(ICFD)
        write (outs,'(i10)') IWOPEN(Iaop,ICFD)
        write (LLOCAT,'(a)') outs(lnblnk(outs):lnblnk(outs))
        K=0
        call EGETWI(LLOCAT,K,ILOCAT,1,6,'F',
     &                                  'Illegal surface orient.',IER)
        if (ILOCAT.eq.1.or.ILOCAT.eq.2) then
          if (ILOCAT.eq.2) then
            MNOD=NI
          else
            MNOD=2
          endif

C In YZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL ORTTRN(XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)
          CALL ORTTRN(XU(MNOD),YV(JOPENf(Iaop,ICFD)+1),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(MNOD),YV(JOPENf(Iaop,ICFD)+1),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

C Label in center of boundary.
          Y=(YV(JOPENi(Iaop,ICFD))+YV(JOPENf(Iaop,ICFD)))/2.
          Z=(ZW(KOPENi(Iaop,ICFD))+ZW(KOPENf(Iaop,ICFD)))/2.
          CALL ORTTRN(XU(MNOD),Y,Z,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a,i2)') 'AO',Iaop
          CALL wstxpt(iix,iiy,temp)
          call forceflush()

        elseif (ILOCAT.eq.3.or.ILOCAT.eq.4) then
          if (ILOCAT.eq.4) then
            MNOD=NJ
          else
            MNOD=2
          endif

C In XZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(MNOD),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(MNOD),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

C Label in center of boundary.
          X=(XU(IOPENi(Iaop,ICFD))+XU(IOPENf(Iaop,ICFD)))/2.
          Z=(ZW(KOPENi(Iaop,ICFD))+ZW(KOPENf(Iaop,ICFD)))/2.
          CALL ORTTRN(X,YV(MNOD),Z,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a,i2)') 'AO',Iaop
          CALL wstxpt(iix,iiy,temp)
          call forceflush()
        elseif (ILOCAT.eq.5.or.ILOCAT.eq.6) then
          if (ILOCAT.eq.6) then
            MNOD=NK
          else
            MNOD=2
          endif

C In XY plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(JOPENi(Iaop,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(JOPENi(Iaop,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(JOPENf(Iaop,ICFD)+1),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(JOPENf(Iaop,ICFD)+1),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()
          iixs=iix
          iiys=iiy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(JOPENi(Iaop,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if (Iaop.eq.IOPENHL) then
            call edwline(iixs,iiys,iix,iiy)
          else
            call eswline(iixs,iiys,iix,iiy)
          endif
          call forceflush()

C Label in center of boundary.
          X=(XU(IOPENi(Iaop,ICFD))+XU(IOPENf(Iaop,ICFD)))/2.
          Y=(YV(JOPENi(Iaop,ICFD))+YV(JOPENf(Iaop,ICFD)))/2.
          CALL ORTTRN(X,Y,ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a,i2)') 'AO',Iaop
          CALL wstxpt(iix,iiy,temp)
          call forceflush()
        endif
 70   continue

      return
      end



