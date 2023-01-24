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

C Contains: grid visualization routines.
C  CGD - domain grid visualization.
C  VGRID1D - draws the CFD domain gridding in 1d.
C  VGRID2D - draws the CFD domain gridding in 2d.
C  VGRID3D - draws the CFD domain gridding in 3d.
C  VSB3D - draws the solid boundaries - highlighting the chosen one.
C  VAO3D - draws the air openings - highlighting the chosen one.
C  INIT_CFDTRANS - initialises data structures for CFD <-> geometry
C                  domain transformations.
C  INIT_CFDTRANS_NOGEOM - as above, but works without gemoetry.
C  CFDTRANS - does CFD <-> geometry domain transformations.
C  ESMANYZON - populates G1M (used for point containment checks) with
C              geometry for any zone, converted into CFD coordinates.
C  V1CEL3D - highlights 1 CFD grid cell with bold lines, and its
C            projections on all 6 faces with normal lines.
C  CFDVIEW - essentially the equivalent of ADJVIEW (common3dv.F), but
C            for CFD domains.

C ********************* CGD *********************
C CGD - domain grid visualization.
C IC = zone number (if in dfs set to 1)
      subroutine cgd(IC,IER)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "prj3dv.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      COMMON/MODVIS/IVISMOD

      CHARACTER ITEM(16)*31
      character outs*124,ETEXT*30

      logical GRMODE
      integer NITMS,INO  ! max items and current menu item

      helpinsub='cgd'  ! set for subroutine

C Set default values.
C GRMODE = graphics mode.
      GRMODE=.false.
      XZ=0.; YZ=0.; ZZ=0.

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
        GRMODE=.true.

C Set wire-frame display options so that we see only surfaces,
C we are focused on this zone, no origin no grid, no surface normals
        itorg=1; itgrd=1; itsnr=1; 
      ENDIF
      IVISMOD=2

C Let the user pick a menu item.
   1  INO=-5
      ITEM(1) =  'a Plot x-y plane (2D) grid     '
      ITEM(2) =  'b Plot x-z plane (2D) grid     '
      ITEM(3) =  'c Plot y-z plane (2D) grid     '
      ITEM(4) =  ' --3D grid options -------     '
      if (.not.HAS_GEOM) then
        ITEM(5)= '  no zone geometry available   '
      elseif (ISHGEO.eq.0) then
        ITEM(5)= 'd zone geometry >> off         '
      elseif (ISHGEO.gt.0) then
        ITEM(5)= 'd zone geometry >> bold lines  '
      else
        ITEM(5)= 'd zone geometry >> normal lines'
      endif
      if (IFACES.eq.0) then
        ITEM(6)= 'e CFD grid >> off              '
      elseif (IFACES.eq.1) then
        ITEM(6)= 'e CFD grid >> all              '
      elseif (IFACES.eq.2) then
        ITEM(6)= 'e CFD grid >> X direction only '
      elseif (IFACES.eq.3) then
        ITEM(6)= 'e CFD grid >> Y direction only '
      elseif (IFACES.eq.4) then
        ITEM(6)= 'e CFD grid >> Z direction only '
      endif
      if (ISHSB.eq.0) then
        ITEM(7)= 'f solid boundaries >> off      '
      else
        ITEM(7)= 'f solid boundaries >> on       '
      endif
      if (ISHAO.eq.0) then
        ITEM(8)= 'g air flow openings >> off     '
      else
        ITEM(8)= 'g air flow openings >> on      '
      endif
      if (ISHBLK.eq.0) then
        ITEM(9)= 'h blockages >> off             '
      else
        ITEM(9)= 'h blockages >> on              '
      endif
      if (ISHSRC.eq.0) then
        ITEM(10)='i sources >> off               '
      else
        ITEM(10)='i sources >> on                '
      endif
      ITEM(11)=  '  ------------------------     '
      ITEM(12)=  'j List x plane coordinates     '
      ITEM(13)=  'k List y plane coordinates     '
      ITEM(14)=  'l List z plane coordinates     '
      ITEM(15)=  '? Help                         '
      ITEM(16)=  '- Exit                         '
      NITMS=16

C Help text for menu.
      helptopic='dfs_display_list_grid'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if (MODIFYVIEW) call redraw(IER)

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

C Toggle geometry.
        if (ISHGEO.eq.0) then ! off -> bold lines
          ISHGEO=1
        elseif (ISHGEO.gt.0) then ! bold -> normal lines
          ISHGEO=-MS-1
        else ! -> off
          ISHGEO=0
        endif
        MODIFYVIEW=.TRUE.
      ELSEIF(INO.EQ.6.and.GRMODE)THEN

C Toggle CFD grid.
        IFACES=IFACES+1 ! 0 -> 1 -> 2 -> 3 -> 4 -> 0
        if (IFACES.gt.4) IFACES=0
        MODIFYVIEW=.TRUE.
      ELSEIF(INO.EQ.7.and.GRMODE)THEN

C Toggle solid boundaries
        if (ISHSB.eq.0) then ! off -> on
          ISHSB=-1
        else ! -> off
          ISHSB=0
        endif
        MODIFYVIEW=.TRUE.
      ELSEIF(INO.EQ.8.and.GRMODE)THEN

C Toggle air flow openings.
        if (ISHAO.eq.0) then ! off -> on
          ISHAO=-1
        else ! -> off
          ISHAO=0
        endif
        MODIFYVIEW=.TRUE.
      ELSEIF(INO.EQ.9.and.GRMODE)THEN

C Toggle blockages.
        if (ISHBLK.eq.0) then ! off -> on
          ISHBLK=-1
        else ! -> off
          ISHBLK=0
        endif
        MODIFYVIEW=.TRUE.
      ELSEIF(INO.EQ.10.and.GRMODE)THEN

C Toggle sources.
        if (ISHSRC.eq.0) then ! off -> on
          ISHSRC=-1
        else ! -> off
          ISHSRC=0
        endif
        MODIFYVIEW=.TRUE.
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
#include "building.h"
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/SPAD/MMOD,LIMIT,LIMTTY

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

      if(mmod.ne.8) return

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
      call dintervalf(Xmin,Xmax,ddy1,ny,0)
      call horaxisdd(Xmin,Xmax,iigl,iigr,iiw1,Xadd,sca,0,ddy1,ny,xax)
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
#include "building.h"
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/SPAD/MMOD,LIMIT,LIMTTY

      character title*30,xax*10,yax*1

      dimension xgrd(NTCELX),ygrd(NTCELY)
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

      if(mmod.ne.8) return

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
      call dintervalf(Xmin,Xmax,ddy1,NDEC,0)
      call horaxisdd(Xmin,Xmax,iigl,iigr,iigb,Xadd,sca,0,ddy1,NDEC,xax)
      if((Ymax-Ymin).lt.2.0)then
        iiw1=iigl-10
        call dintervalf(Ymin,Ymax,DY,NDEC,0)
        call vrtaxisdd(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,0,
     &    DY,NDEC,0,yax)
        iiw1=iigr-15
        call vrtaxisdd(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,0,
     &    DY,NDEC,1,yax)
      else
        iiw1=iigl-10
        call dintervalf(Ymin,Ymax,DY,NDEC,1)
        call vrtaxisdd(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,1,
     &    DY,NDEC,0,yax)
        iiw1=iigr-15
        call vrtaxisdd(Ymin,Ymax,iiw1,iigb,iigt,Yadd,sca,1,
     &    DY,NDEC,1,yax)
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
C VGRID3D - draws the CFD domain gridding in 3d.
C   izin = 0 then do not draw zone geometry.
C   izin > 0 then draw geometry for zone izin in bold lines
C   izin < 0 then draw geometry for zone ICP in normal lines,
C            highlighting surface izin in bold lines.
C Note: izin can be set to a large negative number (greater than number
C of surfaces) to draw geometry in normal lines only.
C   ig = 0 then do not graw any grid.
C   ig = 1 then draw all face grids.
C   ig = 2 then draw only east and west face grid.
C   ig = 3 then draw only north and south face grid.
C   ig = 4 then draw only top and bottom face grid.
      SUBROUTINE VGRID3D(izin,ig)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "cfd.h"

      integer iz,izin,ig

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

C Only used if iz<0
      COMMON/ICFNOD/ICFD,ICP

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)

      logical greyok,odd
      integer im
      character btemp*6
      real XPP,YPP,ZPP  ! points taking into account zone minimum
      real X0,Y0,Z0
      real DX,DY,DHX,DHY ! for intermediate label points 

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iixs,iiys,iix,iiy,iixlast,iiylast
      integer iixhs,iiyhs,iixxhs,iixyhs
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iixs,iiys,iix,iiy,iixlast,iiylast
      integer*8 iixhs,iiyhs,iixxhs,iixyhs
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

C Clear current viewing box and re-establish image bounds.
      CALL startbuffer

C Setup and pass in parameters to win3d.
      if (nzg.gt.0) then
        iiw1=4; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
      else
        iiw1=2; iiw2=2; iiw3=3; iiw4=1; iimenu=menuchw
      endif
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw; igheight=igwh

C Get bounds for image.
      if (nzg.gt.0) then
        call bndobj(0,IER)
      endif
      HITH=1.0; YON=1300.0
      CALL LENS(IER)

C Set scaling ratios for the drawing and axis.
      CALL axiscale(igwid,igheight,-1000.,1000.,-1000.,1000.,xsc,ysc,
     &  sca,Xadd,Yadd)
      call linescale(iigl,Xadd,sca,iigb,Yadd,sca)

      if (izin.lt.0) then
        iz=ICP
        isurhlt=-izin
      else
        iz=izin
      endif

C Debug.
C      write(6,*) 'iz ig ',iz,ig

C Reset to smallest font for drawing i j k labels.
      IF(IFS.GT.0)CALL winfnt(0)

C Loop through all X-Y plane grid lines.
      if(ig.eq.1.or.ig.eq.4)then
        do 10 I=2,NI    ! I traverses along south X axis cells
          do 20 J=2,NJ  ! J traverses along west Y axis cells

C Transform first point to screen coords.
            CALL CFDTRANS(1,XU(i),YV(j),0.,XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixs,iiys)

C Get half way point for west face cells.
            DY=YV(j+1)-YV(j); DHY= DY*0.5
            CALL CFDTRANS(1,XU(i),YV(j)+DHY,0.,XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixhs,iiyhs)

C Get half way point for south face cells.
            DX=XU(i+1)-XU(i); DHX= DX*0.5
            CALL CFDTRANS(1,XU(i)+DHX,YV(j),0.,XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixxhs,iixyhs)

C Transform end point to screen coords.
            CALL CFDTRANS(1,XU(i),YV(j),ZW(NK),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iix,iiy)

            if(J.eq.2.and.I.eq.2)then  ! label the lower left corner
              btemp='   '; iicol=0
              write(btemp,'(a,i2.2)') 'i',i-1
              call textatxy(iixxhs,iixyhs+10,btemp,'-',iicol)
              btemp='   '
              write(btemp,'(a,i2.2)') 'j',j-1
              call textatxy(iixhs-20,iiyhs+10,btemp,'-',iicol)
            elseif(I.eq.2.and.J.lt.NJ)then

C If more than 20 cells only do every other one.
              btemp='   '; iicol=0  ! along lower Y
              write(btemp,'(a,i2.2)') 'j',j-1
              if(NJ.le.20)then
                call textatxy(iixhs-20,iiyhs+10,btemp,'-',iicol)
              else
                im=MOD(j,2)
                if(im.eq.1)then
                  continue
                else
                  call textatxy(iixhs-20,iiyhs+10,btemp,'-',iicol)
                endif
              endif
            elseif(J.eq.2.and.I.lt.NI)then
              btemp='   '; iicol=0
              write(btemp,'(a,i2.2)') 'i',i-1
              if(NI.le.20)then
                call textatxy(iixxhs,iixyhs+10,btemp,'-',iicol)
              else
                im=MOD(i,2)
                if(im.eq.1)then
                  continue
                else
                  call textatxy(iixxhs,iixyhs+10,btemp,'-',iicol)
                endif
              endif
            endif

C Draw dotted or solid line.
            if (I.eq.2.or.I.eq.NI.or.J.eq.2.or.J.eq.NJ) then
              if ((I.eq.2.and.J.eq.2).or.(I.eq.NI.and.J.eq.NJ).or.
     &            (I.eq.2.and.J.eq.NJ).or.(I.eq.NI.and.J.eq.2)) then
                call eline(iixs,iiys,1)  ! solid line
                call eline(iix,iiy,0)
                call forceflush()
              else
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif

C Draw the dotted line after casting to local variable.
                if(mmod.eq.8)then
                  call edline(iixs,iiys,iix,iiy,5)
                else
                  call edlinewwc(iixs,iiys,iix,iiy,5)
                endif
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
      if(ig.eq.1.or.ig.eq.2)then
        do 30 K=2,NK    ! is along the West Z face
          do 40 J=2,NJ  ! is along the West Y face

C Transform first point to screen coords.
            CALL CFDTRANS(1,0.,YV(j),ZW(k),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixs,iiys)

C Get half way point for west face cells.
            DY=YV(j+1)-YV(j); DHY= DY*0.5
            CALL CFDTRANS(1,0.,YV(j)+DHY,ZW(k),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixhs,iiyhs)

            if(J.eq.NJ.and.K.lt.NK)then
              btemp='   '; iicol=0  ! along the left vertical
              write(btemp,'(a,i2.2)') 'k',k-1
              if(NK.le.20)then
                call textatxy(iixs-20,iiys-5,btemp,'-',iicol)
              else
                im=MOD(k,2)
                if(im.eq.1)then
                  continue
                else
                  call textatxy(iixs-20,iiys-5,btemp,'-',iicol)
                endif
              endif
            endif
            if(ig.eq.3)then
              if(K.eq.2.and.J.lt.NJ)then
                btemp='   '; iicol=0  ! along the left lower Y
                write(btemp,'(a,i2.2)') 'j',j-1
                if(NJ.le.20)then
                  call textatxy(iixhs-20,iiyhs+10,btemp,'-',iicol)
                else
                  im=MOD(j,2)
                  if(im.eq.1)then
                    continue
                  else
                    call textatxy(iixhs-20,iiyhs+10,btemp,'-',iicol)
                  endif
                endif
              endif
            endif

C Transform 2nd point to screen coords.
            CALL CFDTRANS(1,XU(NI),YV(j),ZW(k),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iix,iiy)

C Draw line to end point.
            if (K.eq.2.or.K.eq.NK.or.J.eq.2.or.J.eq.NJ) then
              if ((K.eq.2.and.J.eq.2).or.(K.eq.NK.and.J.eq.NJ).or.
     &            (K.eq.2.and.J.eq.NJ).or.(K.eq.NK.and.J.eq.2)) then
                call eline(iixs,iiys,1)
                call eline(iix,iiy,0)
                call forceflush()
              else
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                if(mmod.eq.8)then
                  call edline(iixs,iiys,iix,iiy,4)
                else
                  call edlinewwc(iixs,iiys,iix,iiy,4)
                endif
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
      if(ig.eq.1.or.ig.eq.3)then
        do 50 K=2,NK    ! along South face X axis
          do 60 I=2,NI  ! along South face Z axis

C Transform first point to screen coords.
            CALL CFDTRANS(1,XU(i),0.,ZW(k),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixs,iiys)

C Get half way point for south face cells.
            DX=XU(i+1)-XU(i); DHX= DX*0.5
            CALL CFDTRANS(1,XU(i)+DHX,0.,ZW(k),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iixxhs,iixyhs)

            if(ig.eq.2)then
              if(k.eq.2.and.I.lt.NI)then
                btemp='   '; iicol=0   ! labels along lower X axis
                write(btemp,'(a,i2.2)') 'i',i-1
                if(NI.le.20)then
                call textatxy(iixxhs,iixyhs+10,btemp,'-',iicol)
                else
                  im=MOD(i,2)
                  if(im.eq.1)then
                    continue
                  else
                    call textatxy(iixxhs,iixyhs+10,btemp,'-',iicol)
                  endif
                endif
              endif
            endif

C Transform end point to screen coords.
            CALL CFDTRANS(1,XU(i),YV(NJ),ZW(k),XPP,YPP,ZPP,IER)
            CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
            CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
            call u2pixel(X1,Y1,iix,iiy)

            if(ig.eq.2)then
              if(I.eq.2.and.K.lt.NK)then
                btemp='   '; iicol=0   ! labels along left vertical
                write(btemp,'(a,i2.2)') 'k',k-1
                if(NK.le.20)then
                  call textatxy(iix-20,iiy-5,btemp,'-',iicol)
                else
                  im=MOD(j,2)
                  if(im.eq.1)then
                    continue
                  else
                    call textatxy(iix-20,iiy-5,btemp,'-',iicol)
                  endif
                endif
              endif
            endif

C Draw line to end point.
            if (K.eq.2.or.K.eq.NK.or.I.eq.2.or.I.eq.NI) then
              if ((K.eq.2.and.I.eq.2).or.(K.eq.NK.and.I.eq.NI).or.
     &            (K.eq.2.and.I.eq.NI).or.(K.eq.NK.and.I.eq.2)) then
                call eline(iixs,iiys,1)
                call eline(iix,iiy,0)
                call forceflush()
              else
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                if(mmod.eq.8)then
                  call edline(iixs,iiys,iix,iiy,4)
                else
                  call edlinewwc(iixs,iiys,iix,iiy,4)
                endif
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

C Restore the original font size.
      IF(IFS.GT.0)CALL winfnt(IFS)
      call forceflush()

C If the zone geometry is requested then use some of the code from
C cread3dv.F to draw the surfaces of the zone in bold lines.
      if(iz.gt.0)then

C Continue on with any surfaces in the zone.
  205   DO 1000 I=1,NZSUR(iz)

C Copy polygon for Surface I into structure A for compatibility with
C viewer format 'holes' in surfaces.
C NAP   = Number of vertex points on surface
C IANXT = Index of 'next' point
C IAPNT = Pointer to first vertex of polygon
          IAPNT = 1
          NAP = isznver(iz,I)
          DO 1100 J = 1,isznver(iz,I)
            K = iszjvn(iz,I,J)
            AX(J) = szcoords(iz,K,1)
            AY(J) = szcoords(iz,K,2)
            AZ(J) = szcoords(iz,K,3)
            IANXT(J) = J + 1
 1100     CONTINUE

          IANXT(isznver(iz,I)) = IAPNT

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
C          write(6,*)'nap nbp nb',nap,nbp,nb,
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
            if (izin.lt.0) then
              if (I.eq.isurhlt) then
                if(mmod.eq.8)then
                  call edwline(iixlast,iiylast,iix,iiy)
                else
                  call edwlinewwc(iixlast,iiylast,iix,iiy)
                endif
              else
                if(mmod.eq.8)then
                  call eswline(iixlast,iiylast,iix,iiy)
                else
                  call eswlinewwc(iixlast,iiylast,iix,iiy)
                endif
              endif
            else
              if(mmod.eq.8)then
                call edwline(iixlast,iiylast,iix,iiy)
              else
                call edwlinewwc(iixlast,iiylast,iix,iiy)
              endif
            endif
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
            if (izin.lt.0) then
              if (I.eq.isurhlt) then
                if(mmod.eq.8)then
                  call edwline(iixlast,iiylast,iix,iiy)
                else
                  call edwlinewwc(iixlast,iiylast,iix,iiy)
                endif
              else
                if(mmod.eq.8)then
                  call eswline(iixlast,iiylast,iix,iiy)
                else
                  call eswline(iixlast,iiylast,iix,iiy)
                endif
              endif
            else
              if(mmod.eq.8)then
                call edwline(iixlast,iiylast,iix,iiy)
              else
                call edwlinewwc(iixlast,iiylast,iix,iiy)
              endif
            endif
            call forceflush()
  300     CONTINUE
          call forceflush()
 1000   CONTINUE
      endif

      return
      end


C ********************* VSB3D *********************
C VSB3D - draws the solid boundaries - highlighting the chosen one.
      SUBROUTINE VSB3D(ISBHL)
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
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)

      character LLOCAT*4,outs*124,temp*12
#ifdef OSI
      integer iixs,iiys,iix,iiy,iicol
#else
      integer*8 iixs,iiys,iix,iiy,iicol
#endif
      real XPP,YPP,ZPP  ! points taking into account zone minimum
      real XX,YY,ZZ

      character VOLNAME*12, VCsurf*12, BLKSURF*12

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NSB(ICFD).eq.0) return

C Draw round solid boundaries and last character of label.
      do 70 Isbn=1,NSB(ICFD)
        write (outs,'(i10)') abs(IWSB(Isbn,ICFD))
        write (LLOCAT,'(a)') outs(lnblnk(outs):lnblnk(outs))
        K=0
        call EGETWI(LLOCAT,K,ILOCAT,1,6,'F',
     &    'Illegal surface orient.',IER)
        if (ILOCAT.eq.1.or.ILOCAT.eq.2) then
          if (ILOCAT.eq.2) then
            MNOD=NI
          else
            MNOD=2
          endif

C In YZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL CFDTRANS(1,XU(MNOD),YV(JSBi(Isbn,ICFD)),
     &                  ZW(KSBi(Isbn,ICFD)),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          CALL CFDTRANS(1,XU(MNOD),YV(JSBf(Isbn,ICFD)+1),
     &                  ZW(KSBi(Isbn,ICFD)),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(MNOD),YV(JSBf(Isbn,ICFD)+1),
     &                  ZW(KSBf(Isbn,ICFD)+1),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(MNOD),YV(JSBi(Isbn,ICFD)),
     &                  ZW(KSBf(Isbn,ICFD)+1),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(MNOD),YV(JSBi(Isbn,ICFD)),
     &                  ZW(KSBi(Isbn,ICFD)),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

C Label in center of boundary.
          CALL CFDTRANS(1,XU(MNOD),
     &                  (YV(JSBi(Isbn,ICFD))+YV(JSBf(Isbn,ICFD)))/2.,
     &                  (ZW(KSBi(Isbn,ICFD))+ZW(KSBf(Isbn,ICFD)))/2.,
     &                  XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)

          write (temp,'(a)')VOLNAME(IVOLNSB(Isbn,ICFD),ICFD)
     &                     (1:LNBLNK(VOLNAME(IVOLNSB(Isbn,ICFD),ICFD)))

c          write (temp,'(a,i2)') 'SB',Isbn
          iicol=0
          call textatxy(iix,iiy,temp,'-',iicol)
          call forceflush()

        elseif (ILOCAT.eq.3.or.ILOCAT.eq.4) then
          if (ILOCAT.eq.4) then
            MNOD=NJ
          else
            MNOD=2
          endif

C In XZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL CFDTRANS(1,XU(ISBi(Isbn,ICFD)),YV(MNOD),
     &                  ZW(KSBi(Isbn,ICFD)),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          CALL CFDTRANS(1,XU(ISBf(Isbn,ICFD)+1),YV(MNOD),
     &                  ZW(KSBi(Isbn,ICFD)),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(ISBf(Isbn,ICFD)+1),YV(MNOD),
     &                  ZW(KSBf(Isbn,ICFD)+1),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(ISBi(Isbn,ICFD)),YV(MNOD),
     &                  ZW(KSBf(Isbn,ICFD)+1),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(ISBi(Isbn,ICFD)),YV(MNOD),
     &                  ZW(KSBi(Isbn,ICFD)),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

C Label in center of boundary.
          CALL CFDTRANS(1,(XU(ISBi(Isbn,ICFD))+XU(ISBf(Isbn,ICFD)))/2.,
     &           YV(MNOD),(ZW(KSBi(Isbn,ICFD))+ZW(KSBf(Isbn,ICFD)))/2.,
     &                  XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
C          write (temp,'(a,i2)') 'SB',Isbn
          write (temp,'(a)')VOLNAME(IVOLNSB(Isbn,ICFD),ICFD)
     &                     (1:LNBLNK(VOLNAME(IVOLNSB(Isbn,ICFD),ICFD)))
          iicol=0
          call textatxy(iix,iiy,temp,'-',iicol)
          call forceflush()
        elseif (ILOCAT.eq.5.or.ILOCAT.eq.6) then
          if (ILOCAT.eq.6) then
            MNOD=NK
          else
            MNOD=2
          endif

C In XY plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL CFDTRANS(1,XU(ISBi(Isbn,ICFD)),YV(JSBi(Isbn,ICFD)),
     &                  ZW(MNOD),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          CALL CFDTRANS(1,XU(ISBf(Isbn,ICFD)+1),YV(JSBi(Isbn,ICFD)),
     &                  ZW(MNOD),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(ISBf(Isbn,ICFD)+1),YV(JSBf(Isbn,ICFD)+1),
     &                  ZW(MNOD),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(ISBi(Isbn,ICFD)),YV(JSBf(Isbn,ICFD)+1),
     &                  ZW(MNOD),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(ISBi(Isbn,ICFD)),YV(JSBi(Isbn,ICFD)),
     &                  ZW(MNOD),XPP,YPP,ZPP,IER)
          CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Isbn.eq.ISBHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Isbn.eq.ISBHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

C Label in center of boundary.
          CALL CFDTRANS(1,(XU(ISBi(Isbn,ICFD))+XU(ISBf(Isbn,ICFD)))/2.,
     &                  (YV(JSBi(Isbn,ICFD))+YV(JSBf(Isbn,ICFD)))/2.,
     &                  ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a)')VOLNAME(IVOLNSB(Isbn,ICFD),ICFD)
     &                     (1:LNBLNK(VOLNAME(IVOLNSB(Isbn,ICFD),ICFD)))
c          write (temp,'(a,i2)') 'SB',Isbn
          iicol=0
          call textatxy(iix,iiy,temp,'-',iicol)
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
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12, VCsurf*12, BLKSURF*12
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      character LLOCAT*4,outs*124,temp*12
      logical colok
#ifdef OSI
      integer iixs,iiys,iix,iiy,iicol
#else
      integer*8 iixs,iiys,iix,iiy,iicol
#endif

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NOPEN(ICFD).eq.0)return

C Check if we have colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Draw round solid boundaries and last character of label.
      do 70 Iaop=1,NOPEN(ICFD)
c        write (outs,'(i10)') IWOPEN(Iaop,ICFD)
        write (outs,'(i10)') IVOLF(IVOLNOP(Iaop,ICFD),ICFD)
        write (LLOCAT,'(a)') outs(lnblnk(outs):lnblnk(outs))
        K=0
        call EGETWI(LLOCAT,K,ILOCAT,1,6,'F',
     &                                  'Illegal surface orient.',IER)

C Set colour to red.
        if (colok) then
          iicol=0
          call winscl('z',iicol)
        endif

        if (ILOCAT.eq.1.or.ILOCAT.eq.2) then
          if (ILOCAT.eq.2) then
            MNOD=NI
          else
            MNOD=2
          endif

C In YZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL CFDTRANS(1,XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                  ZW(KOPENi(Iaop,ICFD)),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          CALL CFDTRANS(1,XU(MNOD),YV(JOPENf(Iaop,ICFD)+1),
     &                  ZW(KOPENi(Iaop,ICFD)),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(MNOD),YV(JOPENf(Iaop,ICFD)+1),
     &                  ZW(KOPENf(Iaop,ICFD)+1),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif
          call forceflush()

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                  ZW(KOPENf(Iaop,ICFD)+1),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                  ZW(KOPENi(Iaop,ICFD)),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

C Label in center of boundary.
          CALL CFDTRANS(1,XU(MNOD),
     &                 (YV(JOPENi(Iaop,ICFD))+YV(JOPENf(Iaop,ICFD)))/2.,
     &                 (ZW(KOPENi(Iaop,ICFD))+ZW(KOPENf(Iaop,ICFD)))/2.,
     &                  XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a)')VOLNAME(IVOLNOP(Iaop,ICFD),ICFD)
     &      (1:LNBLNK(VOLNAME(IVOLNOP(Iaop,ICFD),ICFD)))
          iicol=0
          call textatxy(iix,iiy,temp,'z',iicol)
          call forceflush()

        elseif (ILOCAT.eq.3.or.ILOCAT.eq.4) then
          if (ILOCAT.eq.4) then
            MNOD=NJ
          else
            MNOD=2
          endif

C In XZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL CFDTRANS(1,XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                  ZW(KOPENi(Iaop,ICFD)),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          CALL CFDTRANS(1,XU(IOPENf(Iaop,ICFD)+1),YV(MNOD),
     &                  ZW(KOPENi(Iaop,ICFD)),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(IOPENf(Iaop,ICFD)+1),YV(MNOD),
     &                  ZW(KOPENf(Iaop,ICFD)+1),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                  ZW(KOPENf(Iaop,ICFD)+1),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                  ZW(KOPENi(Iaop,ICFD)),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

C Label in center of boundary.
          CALL CFDTRANS(1,
     &                 (XU(IOPENi(Iaop,ICFD))+XU(IOPENf(Iaop,ICFD)))/2.,
     &        YV(MNOD),(ZW(KOPENi(Iaop,ICFD))+ZW(KOPENf(Iaop,ICFD)))/2.,
     &                  XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a)')VOLNAME(IVOLNOP(Iaop,ICFD),ICFD)
     &      (1:LNBLNK(VOLNAME(IVOLNOP(Iaop,ICFD),ICFD)))
          iicol=0
          call textatxy(iix,iiy,temp,'z',iicol)
          call forceflush()
        elseif (ILOCAT.eq.5.or.ILOCAT.eq.6) then
          if (ILOCAT.eq.6) then
            MNOD=NK
          else
            MNOD=2
          endif

C In XY plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL CFDTRANS(1,XU(IOPENi(Iaop,ICFD)),YV(JOPENi(Iaop,ICFD)),
     &                  ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iixs,iiys)

          CALL CFDTRANS(1,XU(IOPENf(Iaop,ICFD)+1),YV(JOPENi(Iaop,ICFD)),
     &                  ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(IOPENf(Iaop,ICFD)+1),
     &                  YV(JOPENf(Iaop,ICFD)+1),ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(IOPENi(Iaop,ICFD)),YV(JOPENf(Iaop,ICFD)+1),
     &                  ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

          iixs=iix
          iiys=iiy
          CALL CFDTRANS(1,XU(IOPENi(Iaop,ICFD)),YV(JOPENi(Iaop,ICFD)),
     &                  ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          if(mmod.eq.8)then
            if (Iaop.eq.IOPENHL) then
              call edwline(iixs,iiys,iix,iiy)
            else
              call eswline(iixs,iiys,iix,iiy)
            endif
            call forceflush()
          else
            if (Iaop.eq.IOPENHL) then
              call edwlinewwc(iixs,iiys,iix,iiy)
            else
              call eswlinewwc(iixs,iiys,iix,iiy)
            endif
          endif

C Label in center of boundary.
          CALL CFDTRANS(1,
     &                 (XU(IOPENi(Iaop,ICFD))+XU(IOPENf(Iaop,ICFD)))/2.,
     &                 (YV(JOPENi(Iaop,ICFD))+YV(JOPENf(Iaop,ICFD)))/2.,
     &                 ZW(MNOD),XX,YY,ZZ,IER)
          CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a)')VOLNAME(IVOLNOP(Iaop,ICFD),ICFD)
     &      (1:LNBLNK(VOLNAME(IVOLNOP(Iaop,ICFD),ICFD)))
          iicol=0
          call textatxy(iix,iiy,temp,'z',iicol)
          call forceflush()
        endif
 70   continue

      if(colok)then
        iicol=0
        call winscl('-',iicol)
      endif

      return
      end

C ********************* VSBL3D *********************
C VSBL3D - draws blockages - highlighting the chosen one.
C << currently only draws in the 3D view not individual sections >>
      SUBROUTINE VSBL3D(ISBL)
#include "building.h"
#include "geometry.h"
#include "cfd.h"
#include "prj3dv.h"
      
C      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

C      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12, VCsurf*12, BLKSURF*12

C integer NBLK() is the number of blockages in each domain
C integer INBLK() points to the volume which defines the blockage shape
C integer NSSO() is the number of small supply openings in each domain
C integer INSSO() point to the volume which defines the small supply shape
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)

      character temp*12

C Arrays iixv iiyv iizz are the 8 coords of a box in the usual order.
#ifdef OSI
      integer iicol,iixs,iiys,iix,iiy,iixv(8),iiyv(8)
#else
      integer*8 iicol,iixs,iiys,iix,iiy,iixv(8),iiyv(8)
#endif
      real XPP,YPP,ZPP  ! points taking into account zone minimum
      real XX,YY,ZZ

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NBLK(ICFD).eq.0) return

C Decode the block identity.
      do 70 iblk=1,NBLK(ICFD)
        indxvol=INBLK(iblk,ICFD)

C Find the cells for this volume (without shift).
        Ii=IVCELLS(indxvol,ICFD,1)
        If=IVCELLS(indxvol,ICFD,2)
        Ji=JVCELLS(indxvol,ICFD,1)
        Jf=JVCELLS(indxvol,ICFD,2)
        Ki=KVCELLS(indxvol,ICFD,1)
        Kf=KVCELLS(indxvol,ICFD,2)

C Debug echo the coordinates of the blockage.
C        xd=XU(If+1)-XU(Ii)
C        write(6,*) 'cords X are:',XU(Ii),XU(If+1),xd
C        yd=YV(Jf+1)-YV(Ji)
C        write(6,*) 'cords Y are:',YV(Ji),YV(Jf+1),yd
C        zd=ZW(Kf+1)-ZW(Ki)
C        write(6,*) 'cords Z are:',ZW(Ki),ZW(Kf+1),zd

C Transform local coords to screen coords and remember.
C Lower front left hand corner of blockage.
        CALL CFDTRANS(1,XU(Ii),YV(Ji),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iixs,iiys)
        iixv(1)=iixs; iiyv(1)=iiys

C Lower front right hand corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Ji),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(2)=iix; iiyv(2)=iiy

C Lower back right hand corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Jf+1),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(3)=iix; iiyv(3)=iiy

C Lower back left hand corner of blockage.
        CALL CFDTRANS(1,XU(Ii),YV(Jf+1),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iixs,iiys)
        iixv(4)=iixs; iiyv(4)=iiys

C Upper back left corner of blockage.
        CALL CFDTRANS(1,XU(Ii),YV(Jf+1),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(5)=iix; iiyv(5)=iiy

C Upper front left corner of blockage
        CALL CFDTRANS(1,XU(Ii),YV(Ji),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(6)=iix; iiyv(6)=iiy

C Upper front right corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Ji),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(7)=iix; iiyv(7)=iiy

C Upper back right corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Jf+1),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(8)=iix; iiyv(8)=iiy
        call forceflush()

C Label in center of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Ji),ZW(Kf+1),XX,YY,ZZ,IER)
        CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a)')VOLNAME(indxvol,ICFD)
     &      (1:LNBLNK(VOLNAME(indxvol,ICFD)))
        iicol=0
        call textatxy(iix,iiy,temp,'-',iicol)
        call forceflush()

C Draw around blockage in single or double lines in the usual order.
        if (iblk.eq.ISBL) then
          call edwline(iixv(1),iiyv(1),iixv(2),iiyv(2))
          call edwline(iixv(2),iiyv(2),iixv(3),iiyv(3))
          call edwline(iixv(3),iiyv(3),iixv(4),iiyv(4))
          call edwline(iixv(4),iiyv(4),iixv(1),iiyv(1))
          call edwline(iixv(5),iiyv(5),iixv(6),iiyv(6))
          call edwline(iixv(6),iiyv(6),iixv(7),iiyv(7))
          call edwline(iixv(7),iiyv(7),iixv(8),iiyv(8))
          call edwline(iixv(8),iiyv(8),iixv(5),iiyv(5))
          call edwline(iixv(1),iiyv(1),iixv(6),iiyv(6))
          call edwline(iixv(2),iiyv(2),iixv(7),iiyv(7))
          call edwline(iixv(3),iiyv(3),iixv(8),iiyv(8))
          call edwline(iixv(4),iiyv(4),iixv(5),iiyv(5))
        else
          call eswline(iixv(1),iiyv(1),iixv(2),iiyv(2))
          call eswline(iixv(2),iiyv(2),iixv(3),iiyv(3))
          call eswline(iixv(3),iiyv(3),iixv(4),iiyv(4))
          call eswline(iixv(4),iiyv(4),iixv(1),iiyv(1))
          call eswline(iixv(5),iiyv(5),iixv(6),iiyv(6))
          call eswline(iixv(6),iiyv(6),iixv(7),iiyv(7))
          call eswline(iixv(7),iiyv(7),iixv(8),iiyv(8))
          call eswline(iixv(8),iiyv(8),iixv(5),iiyv(5))
          call eswline(iixv(1),iiyv(1),iixv(6),iiyv(6))
          call eswline(iixv(2),iiyv(2),iixv(7),iiyv(7))
          call eswline(iixv(3),iiyv(3),iixv(8),iiyv(8))
          call eswline(iixv(4),iiyv(4),iixv(5),iiyv(5))
        endif
        call forceflush()

 70   continue

      return
      end

C ********************* VSRC3D *********************
C VSRC3D - draws sources - highlighting the chosen one.
C << currently only draws in the 3D view not individual sections >>
      SUBROUTINE VSRC3D(ISRC)
#include "building.h"
C #include "geometry.h"
#include "cfd.h"
#include "prj3dv.h"
      
C      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

C      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12, VCsurf*12, BLKSURF*12
       common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      character temp*12
      logical colok

C Arrays iixv iiyv iizz are the 8 coords of a box in the usual order.
#ifdef OSI
      integer iicol,iixs,iiys,iix,iiy,iixv(8),iiyv(8)
#else
      integer*8 iicol,iixs,iiys,iix,iiy,iixv(8),iiyv(8)
#endif
      real XPP,YPP,ZPP  ! points taking into account zone minimum
      real XX,YY,ZZ

C Check in graphics mode.
      IF(MMOD.ne.8) return

C Check if we have colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.
      iisrc=0

C Loop through volumes.
      do 70 iv=1,NVOL(ICFD)

C Identify sources.
        if (IVTYPE(iv,ICFD).ne.20) cycle
        iisrc=iisrc+1

C Find the cells for this volume (without shift).
        Ii=IVCELLS(iv,ICFD,1)
        If=IVCELLS(iv,ICFD,2)
        Ji=JVCELLS(iv,ICFD,1)
        Jf=JVCELLS(iv,ICFD,2)
        Ki=KVCELLS(iv,ICFD,1)
        Kf=KVCELLS(iv,ICFD,2)

C Debug echo the coordinates of the blockage.
C        xd=XU(If+1)-XU(Ii)
C        write(6,*) 'cords X are:',XU(Ii),XU(If+1),xd
C        yd=YV(Jf+1)-YV(Ji)
C        write(6,*) 'cords Y are:',YV(Ji),YV(Jf+1),yd
C        zd=ZW(Kf+1)-ZW(Ki)
C        write(6,*) 'cords Z are:',ZW(Ki),ZW(Kf+1),zd

C Transform local coords to screen coords and remember.
C Lower front left hand corner of blockage.
        CALL CFDTRANS(1,XU(Ii),YV(Ji),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iixs,iiys)
        iixv(1)=iixs; iiyv(1)=iiys

C Lower front right hand corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Ji),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(2)=iix; iiyv(2)=iiy

C Lower back right hand corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Jf+1),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(3)=iix; iiyv(3)=iiy

C Lower back left hand corner of blockage.
        CALL CFDTRANS(1,XU(Ii),YV(Jf+1),ZW(Ki),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iixs,iiys)
        iixv(4)=iixs; iiyv(4)=iiys

C Upper back left corner of blockage.
        CALL CFDTRANS(1,XU(Ii),YV(Jf+1),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(5)=iix; iiyv(5)=iiy

C Upper front left corner of blockage
        CALL CFDTRANS(1,XU(Ii),YV(Ji),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(6)=iix; iiyv(6)=iiy

C Upper front right corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Ji),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(7)=iix; iiyv(7)=iiy

C Upper back right corner of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Jf+1),ZW(Kf+1),XPP,YPP,ZPP,IER)
        CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
        iixv(8)=iix; iiyv(8)=iiy
        call forceflush()

C Label in center of blockage.
        CALL CFDTRANS(1,XU(If+1),YV(Ji),ZW(Kf+1),XX,YY,ZZ,IER)
        CALL ORTTRN(XX,YY,ZZ,TEMAT,X0,Y0,Z0,IERR)
        CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
        call u2pixel(X1,Y1,iix,iiy)
          write (temp,'(a)')VOLNAME(iv,ICFD)
     &      (1:LNBLNK(VOLNAME(iv,ICFD)))
        iicol=1
        call textatxy(iix,iiy,temp,'z',iicol)
        call forceflush()

C Set colour to ??.
        if (colok) then
          iicol=1
          call winscl('z',iicol)
        endif

C Draw around blockage in single or double lines in the usual order.
        if (iisrc.eq.ISRC) then
          call edwline(iixv(1),iiyv(1),iixv(2),iiyv(2))
          call edwline(iixv(2),iiyv(2),iixv(3),iiyv(3))
          call edwline(iixv(3),iiyv(3),iixv(4),iiyv(4))
          call edwline(iixv(4),iiyv(4),iixv(1),iiyv(1))
          call edwline(iixv(5),iiyv(5),iixv(6),iiyv(6))
          call edwline(iixv(6),iiyv(6),iixv(7),iiyv(7))
          call edwline(iixv(7),iiyv(7),iixv(8),iiyv(8))
          call edwline(iixv(8),iiyv(8),iixv(5),iiyv(5))
          call edwline(iixv(1),iiyv(1),iixv(6),iiyv(6))
          call edwline(iixv(2),iiyv(2),iixv(7),iiyv(7))
          call edwline(iixv(3),iiyv(3),iixv(8),iiyv(8))
          call edwline(iixv(4),iiyv(4),iixv(5),iiyv(5))
        else
          call eswline(iixv(1),iiyv(1),iixv(2),iiyv(2))
          call eswline(iixv(2),iiyv(2),iixv(3),iiyv(3))
          call eswline(iixv(3),iiyv(3),iixv(4),iiyv(4))
          call eswline(iixv(4),iiyv(4),iixv(1),iiyv(1))
          call eswline(iixv(5),iiyv(5),iixv(6),iiyv(6))
          call eswline(iixv(6),iiyv(6),iixv(7),iiyv(7))
          call eswline(iixv(7),iiyv(7),iixv(8),iiyv(8))
          call eswline(iixv(8),iiyv(8),iixv(5),iiyv(5))
          call eswline(iixv(1),iiyv(1),iixv(6),iiyv(6))
          call eswline(iixv(2),iiyv(2),iixv(7),iiyv(7))
          call eswline(iixv(3),iiyv(3),iixv(8),iiyv(8))
          call eswline(iixv(4),iiyv(4),iixv(5),iiyv(5))
        endif
        call forceflush()

 70   continue

      return
      end

C ************* INIT_CFDTRANS
C INIT_CFDTRANS populates the CFD2GEOM common block, required for
C subroutine CFDTRANS. It does this using data for the current CFD
C domain (i.e. a domain must be read in before this subroutine is
C called) and data in the "geometry" header.
C
C The convention is that the building geometry domain has axes x, y, and
C z, and is global among all zones. CFD domains each have their own
C local origins and u, v, and w axes. 

C The translation of CFD coordinates into geometry coordinates can be
C represented as:
C Pg = Ogc + Ugc Pc
C coordinates in the geometry domain of point P 
C   [equals]
C coordinates in the geometry domain of the origin of the CFD domain 
C   [plus]
C coordinates in the geometry domain of unit vectors for axes of the CFD
C domain 
C   [times]
C coordinates in the CFD domain of point P
C
C Pg is a column vector in axes x, y, and z
C Ogc is a column vector in axes x, y and z
C Pc is a column vector in axes u, v and w
C Ugc is a matrix of the form:
C [ Uxu Uxv Uxw ]
C [ Uyu Uyv Uyw ]
C [ Uzu Uzv Uzw ]
C where Uab is the coordinate in the a direction of the unit vector for
C axis b.
C
C Rearranging this to do the opposite transformation, we get:
C Pc = Ugc^-1 (Pg - Ogc)
C coordinates in the CFD domain of point P
C   [equals]
C the inverse of
C coordinates in the geometry domain of unit vectors for axes of the CFD
C domain 
C   [times] 
C (
C   coordinates in the geometry domain of point P
C     [minus] 
C   coordinates in the geometry domain of the origin of the CFD domain
C )
C
C Because both domains have orthogonal axes, matrix Ugc must be an
C orthogonal matrix, hence the inverse equals the transpose.

      SUBROUTINE INIT_CFDTRANS(IER)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/CFD2GEOM/Ox,Oy,Oz,Uxu,Uxv,Uxw,Uyu,Uyv,Uyw,Uzu,Uzv,Uzw
      real Ox,Oy,Oz,Uxu,Uxv,Uxw,Uyu,Uyv,Uyw,Uzu,Uzv,Uzw

C Get coordinates of CFD origin vertex, in terms of the geometry domain.
      Ox=szcoords(ICP,IORG(ICFD),1)
      Oy=szcoords(ICP,IORG(ICFD),2)
      Oz=szcoords(ICP,IORG(ICFD),3)

C Get unit vector for i axis in terms of building geometry, from
C coordinates of origin and end of axis vertices.
      vdx=szcoords(ICP,ixend(ICFD),1)-Ox
c      write(6,*)'vdx',vdx
      vdy=szcoords(ICP,ixend(ICFD),2)-Oy
c      write(6,*)'vdy',vdy
      vdz=szcoords(ICP,ixend(ICFD),3)-Oz
c      write(6,*)'vdz',vdz
      CROWA = vdx*vdx + vdy*vdy + vdz*vdz
      CROWA=SQRT(CROWA)
      if (abs(crowa).lt.0.001) then
        call edisp(iuout,'INIT_CFDTRANS: u axis has zero length')
        IER=1
      endif
      Uxu=vdx/crowa
      Uyu=vdy/crowa
      Uzu=vdz/crowa

C Similarly for j axis.
      vdx=szcoords(ICP,iyend(ICFD),1)-Ox
      vdy=szcoords(ICP,iyend(ICFD),2)-Oy
      vdz=szcoords(ICP,iyend(ICFD),3)-Oz
      CROWA = vdx*vdx + vdy*vdy + vdz*vdz
      CROWA=SQRT(CROWA)
      if (abs(crowa).lt.0.001) then
        call edisp(iuout,'INIT_CFDTRANS: v axis has zero length')
        IER=1
      endif
      Uxv=vdx/crowa
      Uyv=vdy/crowa
      Uzv=vdz/crowa

C Similarly for k axis.
      vdx=szcoords(ICP,izend(ICFD),1)-Ox
      vdy=szcoords(ICP,izend(ICFD),2)-Oy
      vdz=szcoords(ICP,izend(ICFD),3)-Oz
      CROWA = vdx*vdx + vdy*vdy + vdz*vdz
      CROWA=SQRT(CROWA)
      if (abs(crowa).lt.0.001) then
        call edisp(iuout,'INIT_CFDTRANS: w axis has zero length')
        IER=1
      endif
      Uxw=vdx/crowa
      Uyw=vdy/crowa
      Uzw=vdz/crowa

      if (IER.ne.0) then
        call edisp(iuout,
     &    'INIT_CFDTRANS: transformation initialisation failed')
      endif
  999 return
      end

C ************* INIT_CFDTRANS_NOGEOM
C INIT_CFDTRANS_NOGEOM is a version of INIT_CFDTRANS that runs without
C building geometry.  This is needed for viewing CFD domains that are
C not associated with a building zone.

      SUBROUTINE INIT_CFDTRANS_NOGEOM(IER)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/CFD2GEOM/Ox,Oy,Oz,Uxu,Uxv,Uxw,Uyu,Uyv,Uyw,Uzu,Uzv,Uzw
      real Ox,Oy,Oz,Uxu,Uxv,Uxw,Uyu,Uyv,Uyw,Uzu,Uzv,Uzw

      Ox=0.0; Oy=0.0; Oz=0.0
      Uxu=1.0; Uxv=0.0; Uxw=0.0
      Uyu=0.0; Uyv=1.0; Uyw=0.0
      Uzu=0.0; Uzv=0.0; Uzw=1.0

      return
      end

C ************* CFDTRANS
C CFDTRANS translates a set of coordinates between the CFD domain and
C the building geometry domain. This transformation can be done either
C way, depending on the value of MODE:
C 1 = CFD coords -> geometry coords
C 2 = geometry coords -> CFD coords
C 
C Subroutine INIT_CFDTRANS must be called before this is used; this
C populates common blocks CFD2GEOM which is required for this
C subroutine. Also see INIT_CFDTRANS for the calculation basis of
C this subroutine.

      SUBROUTINE CFDTRANS(MODE,Xin,Yin,Zin,Xout,Yout,Zout,IER)

      integer MODE
      real Xin,Yin,Zin,Xout,Yout,Zout      

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/CFD2GEOM/Ox,Oy,Oz,Uxu,Uxv,Uxw,Uyu,Uyv,Uyw,Uzu,Uzv,Uzw
      real Ox,Oy,Oz,Uxu,Uxv,Uxw,Uyu,Uyv,Uyw,Uzu,Uzv,Uzw

      if (MODE.eq.1) then
        Xout=Ox+Uxu*Xin+Uxv*Yin+Uxw*Zin
        Yout=Oy+Uyu*Xin+Uyv*Yin+Uyw*Zin
        Zout=Oz+Uzu*Xin+Uzv*Yin+Uzw*Zin
      elseif (MODE.eq.2) then
        Xout=Uxu*(Xin-Ox)+Uyu*(Yin-Oy)+Uzu*(Zin-Oz)
        Yout=Uxv*(Xin-Ox)+Uyv*(Yin-Oy)+Uzv*(Zin-Oz)
        Zout=Uxw*(Xin-Ox)+Uyw*(Yin-Oy)+Uzw*(Zin-Oz)
      else
        call edisp(iuout,'CFDTRANS: Invalid MODE')
        IER=1
      endif

  999 return
      end

      
C ***************** ESMCFDZON 
C Populates G1M (used for point containment checks) with
C geometry for any zone, converted into CFD coordinates.

C Assumes INIT_CFDTRANS has been called.

      SUBROUTINE ESMCFDZON(iz,ier)

#include "building.h"
#include "geometry.h"
  
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
  
      NTVM=NZTV(iz)
      DO 65 iv=1,NTVM
        CALL CFDTRANS(2,SZCOORDS(iz,iv,1),SZCOORDS(iz,iv,2),
     &                SZCOORDS(iz,iv,3),XM(iv),YM(iv),ZM(iv),ier)
        if (ier.ne.0) return
   65 CONTINUE

      NSURM=NZSUR(iz)
      DO 66 is=1,NSURM
        NVERM(is)=ISZNVER(iz,is)
        DO 67 iv=1,NVERM(is)
          JVNM(is,iv)=ISZJVN(iz,is,iv)
   67   CONTINUE
   66 CONTINUE

      RETURN
      END

C     ********************* V1CELD *********************
C V1CEL3D - with the 3D grid displayed, highlights 1 cell with bold
C lines. Also highlights cell on grid boundaries with normal lines.
C II, IJ and IK are grid coordinates of the chosen cell. Note that these
C grid coordinates do not take into account "extra" cells that ESP-r
C adds onto either end of the domain - they refer to cells that are
C visible from other cgd.F subroutines.
      SUBROUTINE V1CEL3D(II,IJ,IK)
#include "building.h"
#include "cfd.h"
#include "prj3dv.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

C Arrays iixv iixv iiyv are the 8 coords of a box in the usual order.
C Other arrays iix? and iiy? are projections on the 6 faces.
#ifdef OSI
      integer iix,iiy,iixv(8),iiyv(8)
      integer iixw(4),iiyw(4)
      integer iixe(4),iiye(4)
      integer iixs(4),iiys(4)
      integer iixn(4),iiyn(4)
      integer iixl(4),iiyl(4)
      integer iixh(4),iiyh(4)
#else
      integer*8 iix,iiy,iixv(8),iiyv(8)
      integer*8 iixw(4),iiyw(4)
      integer*8 iixe(4),iiye(4)
      integer*8 iixs(4),iiys(4)
      integer*8 iixn(4),iiyn(4)
      integer*8 iixl(4),iiyl(4)
      integer*8 iixh(4),iiyh(4)
#endif

C Check in graphics mode.
      IF(MMOD.ne.8) return

      i=II+1; j=IJ+1; k=IK+1

C Transform local coords to screen coords and remember.
C Lower front left hand corner of blockage.
      CALL CFDTRANS(1,XU(i),YV(j),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(1)=iix; iiyv(1)=iiy

C Lower front right hand corner of blockage.
      CALL CFDTRANS(1,XU(i+1),YV(j),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(2)=iix; iiyv(2)=iiy

C Lower back right hand corner of blockage.
      CALL CFDTRANS(1,XU(i+1),YV(j+1),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(3)=iix; iiyv(3)=iiy

C Lower back left hand corner of blockage.
      CALL CFDTRANS(1,XU(i),YV(j+1),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(4)=iix; iiyv(4)=iiy

C Upper back left corner of blockage.
      CALL CFDTRANS(1,XU(i),YV(j+1),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(5)=iix; iiyv(5)=iiy

C Upper front left corner of blockage
      CALL CFDTRANS(1,XU(i),YV(j),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(6)=iix; iiyv(6)=iiy

C Upper front right corner of blockage.
      CALL CFDTRANS(1,XU(i+1),YV(j),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(7)=iix; iiyv(7)=iiy

C Upper back right corner of blockage.
      CALL CFDTRANS(1,XU(i+1),YV(j+1),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixv(8)=iix; iiyv(8)=iiy

C Draw around cell in double lines in the usual order.
      call edwline(iixv(1),iiyv(1),iixv(2),iiyv(2))
      call edwline(iixv(2),iiyv(2),iixv(3),iiyv(3))
      call edwline(iixv(3),iiyv(3),iixv(4),iiyv(4))
      call edwline(iixv(4),iiyv(4),iixv(1),iiyv(1))
      call edwline(iixv(5),iiyv(5),iixv(6),iiyv(6))
      call edwline(iixv(6),iiyv(6),iixv(7),iiyv(7))
      call edwline(iixv(7),iiyv(7),iixv(8),iiyv(8))
      call edwline(iixv(8),iiyv(8),iixv(5),iiyv(5))
      call edwline(iixv(1),iiyv(1),iixv(6),iiyv(6))
      call edwline(iixv(2),iiyv(2),iixv(7),iiyv(7))
      call edwline(iixv(3),iiyv(3),iixv(8),iiyv(8))
      call edwline(iixv(4),iiyv(4),iixv(5),iiyv(5))
      call forceflush()

C Projection on west face.
      CALL CFDTRANS(1,XU(2),YV(j),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixw(1)=iix; iiyw(1)=iiy

      CALL CFDTRANS(1,XU(2),YV(j+1),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixw(2)=iix; iiyw(2)=iiy

      CALL CFDTRANS(1,XU(2),YV(j+1),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixw(3)=iix; iiyw(3)=iiy

      CALL CFDTRANS(1,XU(2),YV(j),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixw(4)=iix; iiyw(4)=iiy

      call eswline(iixw(1),iiyw(1),iixw(2),iiyw(2))
      call eswline(iixw(2),iiyw(2),iixw(3),iiyw(3))
      call eswline(iixw(3),iiyw(3),iixw(4),iiyw(4))
      call eswline(iixw(4),iiyw(4),iixw(1),iiyw(1))
      call eswline(iixw(1),iiyw(1),iixv(1),iiyv(1))
      call eswline(iixw(2),iiyw(2),iixv(4),iiyv(4))
      call eswline(iixw(3),iiyw(3),iixv(5),iiyv(5))
      call eswline(iixw(4),iiyw(4),iixv(6),iiyv(6))
      call forceflush()

C Projection on east face.
      CALL CFDTRANS(1,XU(NI),YV(j),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixe(1)=iix; iiye(1)=iiy

      CALL CFDTRANS(1,XU(NI),YV(j+1),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixe(2)=iix; iiye(2)=iiy

      CALL CFDTRANS(1,XU(NI),YV(j+1),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixe(3)=iix; iiye(3)=iiy

      CALL CFDTRANS(1,XU(NI),YV(j),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixe(4)=iix; iiye(4)=iiy

      call eswline(iixe(1),iiye(1),iixe(2),iiye(2))
      call eswline(iixe(2),iiye(2),iixe(3),iiye(3))
      call eswline(iixe(3),iiye(3),iixe(4),iiye(4))
      call eswline(iixe(4),iiye(4),iixe(1),iiye(1))
      call eswline(iixe(1),iiye(1),iixv(2),iiyv(2))
      call eswline(iixe(2),iiye(2),iixv(3),iiyv(3))
      call eswline(iixe(3),iiye(3),iixv(8),iiyv(8))
      call eswline(iixe(4),iiye(4),iixv(7),iiyv(7))
      call forceflush()

C Projection on south face.
      CALL CFDTRANS(1,XU(i),YV(2),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixs(1)=iix; iiys(1)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(2),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixs(2)=iix; iiys(2)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(2),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixs(3)=iix; iiys(3)=iiy

      CALL CFDTRANS(1,XU(i),YV(2),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixs(4)=iix; iiys(4)=iiy

      call eswline(iixs(1),iiys(1),iixs(2),iiys(2))
      call eswline(iixs(2),iiys(2),iixs(3),iiys(3))
      call eswline(iixs(3),iiys(3),iixs(4),iiys(4))
      call eswline(iixs(4),iiys(4),iixs(1),iiys(1))
      call eswline(iixs(1),iiys(1),iixv(1),iiyv(1))
      call eswline(iixs(2),iiys(2),iixv(2),iiyv(2))
      call eswline(iixs(3),iiys(3),iixv(7),iiyv(7))
      call eswline(iixs(4),iiys(4),iixv(6),iiyv(6))
      call forceflush()

C Projection on north face.
      CALL CFDTRANS(1,XU(i),YV(NJ),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixn(1)=iix; iiyn(1)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(NJ),ZW(k),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixn(2)=iix; iiyn(2)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(NJ),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixn(3)=iix; iiyn(3)=iiy

      CALL CFDTRANS(1,XU(i),YV(NJ),ZW(k+1),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixn(4)=iix; iiyn(4)=iiy

      call eswline(iixn(1),iiyn(1),iixn(2),iiyn(2))
      call eswline(iixn(2),iiyn(2),iixn(3),iiyn(3))
      call eswline(iixn(3),iiyn(3),iixn(4),iiyn(4))
      call eswline(iixn(4),iiyn(4),iixn(1),iiyn(1))
      call eswline(iixn(1),iiyn(1),iixv(4),iiyv(4))
      call eswline(iixn(2),iiyn(2),iixv(3),iiyv(3))
      call eswline(iixn(3),iiyn(3),iixv(8),iiyv(8))
      call eswline(iixn(4),iiyn(4),iixv(5),iiyv(5))
      call forceflush()

C Projection on low face.
      CALL CFDTRANS(1,XU(i),YV(j),ZW(2),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixl(1)=iix; iiyl(1)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(j),ZW(2),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixl(2)=iix; iiyl(2)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(j+1),ZW(2),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixl(3)=iix; iiyl(3)=iiy

      CALL CFDTRANS(1,XU(i),YV(j+1),ZW(2),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixl(4)=iix; iiyl(4)=iiy

      call eswline(iixl(1),iiyl(1),iixl(2),iiyl(2))
      call eswline(iixl(2),iiyl(2),iixl(3),iiyl(3))
      call eswline(iixl(3),iiyl(3),iixl(4),iiyl(4))
      call eswline(iixl(4),iiyl(4),iixl(1),iiyl(1))
      call eswline(iixl(1),iiyl(1),iixv(1),iiyv(1))
      call eswline(iixl(2),iiyl(2),iixv(2),iiyv(2))
      call eswline(iixl(3),iiyl(3),iixv(3),iiyv(3))
      call eswline(iixl(4),iiyl(4),iixv(4),iiyv(4))
      call forceflush()

C Projection on high face.
      CALL CFDTRANS(1,XU(i),YV(j),ZW(NK),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixh(1)=iix; iiyh(1)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(j),ZW(NK),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixh(2)=iix; iiyh(2)=iiy

      CALL CFDTRANS(1,XU(i+1),YV(j+1),ZW(NK),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixh(3)=iix; iiyh(3)=iiy

      CALL CFDTRANS(1,XU(i),YV(j+1),ZW(NK),XPP,YPP,ZPP,IER)
      CALL ORTTRN(XPP,YPP,ZPP,TEMAT,X0,Y0,Z0,IERR)
      CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      call u2pixel(X1,Y1,iix,iiy)
      iixh(4)=iix; iiyh(4)=iiy

      call eswline(iixh(1),iiyh(1),iixh(2),iiyh(2))
      call eswline(iixh(2),iiyh(2),iixh(3),iiyh(3))
      call eswline(iixh(3),iiyh(3),iixh(4),iiyh(4))
      call eswline(iixh(4),iiyh(4),iixh(1),iiyh(1))
      call eswline(iixh(1),iiyh(1),iixv(6),iiyv(6))
      call eswline(iixh(2),iiyh(2),iixv(7),iiyv(7))
      call eswline(iixh(3),iiyh(3),iixv(8),iiyv(8))
      call eswline(iixh(4),iiyh(4),iixv(5),iiyv(5))
      call forceflush()

      return
      END

      SUBROUTINE CFDVIEW(IER)

C CFDVIEW is essentially the equivalent of ADJVIEW in common3dv.F
C for CFD visualisation.

#include "building.h"
#include "cfd.h"
#include "prj3dv.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      CHARACTER*72 TITLE,CFTRFL,LPHI

      character ETEXT*82,tmpstr*6
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Turn off building grid and origin, but preserve global settings.
      ITORGP=ITORG
      ITORG=1
      ITGRDP=ITGRD
      ITGRD=1

C Check initialisation.
C INITD is a 6 character integer string composed of ICFD and ICP, each
C spanning 3 characters and padded with 0s.
      write(tmpstr,'(2I3.3)')ICFD,ICP
      if (INITD.ne.tmpstr) then
        CALL INICNT
        CALL GRID(IER)
        CALL NEW2OLD
        if (HAS_GEOM) then
          CALL INIT_CFDTRANS(IER)
        else
          CALL INIT_CFDTRANS_NOGEOM(IER)
        endif
        if (IER.ne.0) then
          call usrmsg(' Cannot draw this CFD domain in 3D',' ','W')
          goto 999
        endif
        write(INITD,'(2I3.3)')ICFD,ICP
      endif

C Draw grid.
C ISHGEO = 1: Draw zone geometry in bold lines.
C ISHGEO = 0: Do not draw zone geometry.
C ISHGEO < 0: Draw zone ICP geometry in normal lines, highlighting
C             surface ISHGEO in bold lines.
C IFACES = 0: Do not draw the CFD grid.
C IFACES = 1: Draw CFD grid in all 3 dimensions in dotted lines.
C IFACES = 2: Draw CFD grid in u direction (east-west) only.
C IFACES = 3: Draw CFD grid in v direction (north-south) only.
C IFACES = 4: Draw CFD grid in w direction (high-low) only.
      if (ISHGEO.eq.1) then
        call VGRID3D(ICP,IFACES)
      elseif (ISHGEO.lt.0) then
        call VGRID3D(ISHGEO,IFACES)
      else        
        call VGRID3D(0,IFACES)
      endif

C Draw solid boundaries if needed.
C ISHSB = 0: Do not draw BCs.
C ISHSB < 0: Draw all BCs in normal lines.
C ISHSB > 0: Draw BCs, highlighting number ISHSB in bold lines.
      if (ISHSB.lt.0) then 
        call VSB3D(0)
      elseif (ISHSB.gt.0) then
        call VSB3D(ISHSB)
      endif

C Draw blockages if needed.
C ISHBLK = 0: Do not draw BCs.
C ISHBLK < 0: Draw all BCs in normal lines.
C ISHBLK > 0: Draw BCs, highlighting number ISHBLK in bold lines.
      if (ISHBLK.lt.0) then
        call VSBL3D(0)
      elseif (ISHBLK.gt.0) then
        call VSBL3D(ISHBLK)
      endif

C Draw sources if needed.
C ISHSRC = 0: Do not draw BCs.
C ISHSRC < 0: Draw all BCs in normal lines.
C ISHSRC > 0: Draw BCs, highlighting number ISHBLK in bold lines.
      if (ISHSRC.lt.0) then
        call VSRC3D(0)
      elseif (ISHSRC.gt.0) then
        call VSRC3D(ISHSRC)
      endif

C Draw air flow openings if needed.
C ISHAO = 0: Do not draw BCs.
C ISHAO < 0: Draw all BCs in normal lines.
C ISHAO > 0: Draw BCs, highlighting number ISHAO in bold lines.
      if (ISHAO.lt.0) then
        call VAO3D(0) 
      elseif (ISHAO.gt.0) then
        call VAO3D(ISHAO)
      endif

C Draw title.
      WRITE(ETEXT,'(2A)')'Domain: ',
     &  TITLE(ICP)(1:lnblnk(TITLE(ICP)))
      iside=1; isize=1; ifont=1
      call viewtext(ETEXT,iside,isize,ifont)
      call forceflush()

      MODIFYVIEW=.FALSE.
      MODLEN=.FALSE.
      MODBND=.FALSE.

C Restore global settings.
      ITORG=ITORGP
      ITGRD=ITGRDP

 999  return
      end
