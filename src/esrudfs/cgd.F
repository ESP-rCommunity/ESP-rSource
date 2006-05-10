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
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      CHARACTER ITEM(10)*24, H*72
      character outs*124,zname*12,zdesc*64,ETEXT*30

      logical GRMODE

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
        CALL win3d(menuchw,6,6,2,4,igl,igr,igt,igb,igw,igwh)
        GRMODE=.true.
      ENDIF
      CALL INICNT
      CALL GRID(ier)

C Let the user pick a menu item.
   1  INO=-5
      ITEM(1) ='a Plot x-y plane grid'
      ITEM(2) ='b Plot x-z plane grid'
      ITEM(3) ='c Plot y-z plane grid'
      ITEM(4) ='c Plot grids in 3D   '
      ITEM(5) ='  ------------------ '
      ITEM(6) ='d List x plane points'
      ITEM(7) ='e List y plane points'
      ITEM(8) ='f List z plane points'
      ITEM(9) ='? Help               '
      ITEM(10)='- Exit               '
      NITMS=10

C Help text for menu.
      h(1)='This facility displays or lists the CFD grid.'
      h(2)=' '
      h(3)='Aspect ratio can be checked when listing x, y, or z' 
      h(4)='plane points.' 
      h(5)=' '
      h(6)='The aspect ratio (set to be greater than 1.) shows the' 
      h(7)='ratio between distances of the current cell (say cell'
      h(8)='index 8) and the previous one (say cell index 7). For'  
      h(9)='a good grid it should never be greater than 1.2 -->'
      h(10)='check especially cells between two gridding regions.'
      h(11)=' ' 

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
      ELSEIF(INO.EQ.4.and.GRMODE)THEN

C 3D gridding.
        h(1)= 'You have the option of viewing the cell gridding only'
        h(2)= 'of cells plus boundary volumes and flow openings.'
        CALL EASKABCD('Options for 3D gridding:',' ','gridding',
     &    'grid + solids','grid + solids + openings','cancel',IWO,2)
        if(iwo.eq.1)then
          call VGRID3D
        elseif(iwo.eq.2)then
          call VGRID3D
          call VSB3D(0)
        elseif(iwo.eq.3)then
          call VGRID3D
          call VSB3D(0)
          call VAO3D(0)
        else
          continue
        endif
      ELSEIF(INO.EQ.6)THEN

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

      ELSEIF(INO.EQ.7)THEN
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
      ELSEIF(INO.EQ.8)THEN
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
      ELSEIF(INO.EQ.9)THEN
        CALL PHELPD('grid visualization',11,'-',0,0,IER)
      ELSEIF(INO.EQ.10)THEN

C Finish grid visualization, returning display to initial state.
        IF(MMOD.EQ.8)THEN
          call startbuffer()
          CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
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

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      character title*30,xax*10

      dimension NCEL(MNREG,MNZ),plaw(MNREG,MNZ)
      dimension NRG(4,MNZ),REG(MNREG,MNZ)
      dimension xgrd(MCEL1D)

C x-z plane grid.
      call startbuffer()
      Xmin=xoff
      Xmax=amax1(TOTDIS,CTDIS)
      Xmax=Xmax+xoff
      Ymin=yoff
      Ymax=yoff+1.0
      CALL axiscale(igw,igwh,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,Xadd,Yadd)

C Store the scaling parameters used in drawing the axes.
      CALL linescale(igl,Xadd,sca,igb,Yadd,sca)
      call horaxis(Xmin,Xmax,igl,igr,igb+10,Xadd,sca,0,xax)
      CALL viewtext(title,1,0,1)

C Draw a bold box around domain.
      IPEN=-302
      CALL etplot(xoff,yoff,0,0)
      CALL etplot(xoff,yoff+1.0,IPEN,0)
      CALL etplot(xoff+TOTDIS,yoff+1.0,IPEN,0)
      CALL etplot(xoff+TOTDIS,yoff,IPEN,0)
      CALL etplot(xoff,yoff,IPEN,0)

C Draw solid lines at end of each region.
      NCTOT=0
      xg=0.0
      do 10 I=1,NRG(IAX,ICFD)
        NCTOT=NCTOT+abs(NCEL(I,ICFD))
        IPEN=1
        CALL etplot(xg+xoff,yoff,0,0)
        CALL etplot(xg+xoff,yoff+1.0,IPEN,0)
        xg=xg+REG(I,ICFD)
 10   continue
      CALL etplot(xg+xoff,yoff,0,0)
      CALL etplot(xg+xoff,yoff+1.0,IPEN,0)

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
        CALL etplot(xgrd(I)+xoff,yoff,0,0)
        CALL etplot(xgrd(I)+xoff,yoff+1.0,IPEN,0)
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

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      character title*30,xax*10,yax*1

      dimension xgrd(MCEL1D),ygrd(MCEL1D)

C x-z plane grid.
      call startbuffer()
      Xmin=xoff
      Xmax=xgrd(nx)+xoff
      Ymin=yoff
      Ymax=ygrd(ny)+yoff
      CALL axiscale(igw,igwh,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,Xadd,Yadd)

C Store the scaling parameters used in drawing the axes.
      CALL linescale(igl,Xadd,sca,igb,Yadd,sca)
      call horaxis(Xmin,Xmax,igl,igr,igb+10,Xadd,sca,0,xax)
      if((Ymax-Ymin).lt.2.0)then
        call vrtaxis(Ymin,Ymax,igl-10,igb,igt,Yadd,sca,0,0,yax)
        call vrtaxis(Ymin,Ymax,igr+10,igb,igt,Yadd,sca,0,1,yax)
      else
        call vrtaxis(Ymin,Ymax,igl-10,igb,igt,Yadd,sca,1,0,yax)
        call vrtaxis(Ymin,Ymax,igr+10,igb,igt,Yadd,sca,1,1,yax)
      endif
      CALL viewtext(title,1,0,1)
      DO 30 I=2,nx
        IPEN=-204
        if (I.eq.2.or.I.eq.nx) IPEN=1
        CALL etplot(xgrd(I)+xoff,yoff,0,0)
        CALL etplot(xgrd(I)+xoff,ygrd(ny)+yoff,IPEN,0)
   30 CONTINUE
      DO 40 K=2,ny
        IPEN=-204
        if (K.eq.2.or.K.eq.ny) IPEN=1
        CALL etplot(xoff,ygrd(K)+yoff,0,0)
        CALL etplot(xgrd(nx)+xoff,ygrd(K)+yoff,IPEN,0)
   40 CONTINUE

      return
      end


C ********************* VGRID3D *********************
C VGRID3D - draws the CFD domain gridding in 3d.
      SUBROUTINE VGRID3D
#include "cfd.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      logical greyok

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
      CALL win3d(menuchw,1,1,1,1,igl,igr,igt,igb,igw,igwh)

C Get bounds for image.
      HANG=ANG/2.0
      HITH=1.0
      YON=1300.0
      CALL LENS(IER)

C Determine scaling ratios for the drawing and axis.
C Determine the 2D coords of the site extremes.
      CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
      CALL axiscale(igw,igwh,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,Xadd,Yadd)
      call linescale(igl,Xadd,sca,igb,Yadd,sca)

C Loop through all X-Y plane grid lines.
      do 10 I=2,NI
        do 20 J=2,NJ

C Transform first point to screen coords.
          CALL ORTTRN(XU(i),YV(j),0.,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ixs,iys)

C Transform first point to screen coords.
          CALL ORTTRN(XU(i),YV(j),ZW(NK),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)

C Draw dotted line.
          if (I.eq.2.or.I.eq.NI.or.J.eq.2.or.J.eq.NJ) then
            if ((I.eq.2.and.J.eq.2).or.(I.eq.NI.and.J.eq.NJ).or.
     &          (I.eq.2.and.J.eq.NJ).or.(I.eq.NI.and.J.eq.2)) then
              call eline(ixs,iys,1)
              call eline(ix,iy,0)
            else
              if(greyok)call winscl('i',nifgrey-3)
              call edline(ixs,iys,ix,iy,5)
              if(greyok)call winscl('-',0)
              call forceflush()
            endif
          endif
 20     continue
 10   continue

C Loop through all Z-Y plane grid lines.
      do 30 K=2,NK
        do 40 J=2,NJ

C Transform first point to screen coords.
          CALL ORTTRN(0.,YV(j),ZW(k),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ixs,iys)

C Transform first point to screen coords.
          CALL ORTTRN(XU(NI),YV(j),ZW(k),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)

C Draw line to end point.
          if (K.eq.2.or.K.eq.NK.or.J.eq.2.or.J.eq.NJ) then
            if ((K.eq.2.and.J.eq.2).or.(K.eq.NK.and.J.eq.NJ).or.
     &          (K.eq.2.and.J.eq.NJ).or.(K.eq.NK.and.J.eq.2)) then
              call eline(ixs,iys,1)
              call eline(ix,iy,0)
            else
              if(greyok)call winscl('i',nifgrey-3)
              call edline(ixs,iys,ix,iy,4)
              if(greyok)call winscl('-',0)
              call forceflush()
            endif
          endif
 40     continue
 30   continue

C Loop through all Z-X plane grid lines.
      do 50 K=2,NK
        do 60 I=2,NI

C Transform first point to screen coords.
          CALL ORTTRN(XU(i),0.,ZW(k),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ixs,iys)

C Transform end point to screen coords.
          CALL ORTTRN(XU(i),YV(NJ),ZW(k),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)

C Draw line to end point.
          if (K.eq.2.or.K.eq.NK.or.I.eq.2.or.I.eq.NI) then
            if ((K.eq.2.and.I.eq.2).or.(K.eq.NK.and.I.eq.NI).or.
     &          (K.eq.2.and.I.eq.NI).or.(K.eq.NK.and.I.eq.2)) then
              call eline(ixs,iys,1)
              call eline(ix,iy,0)
            else
              if(greyok)call winscl('i',nifgrey-3)
              call edline(ixs,iys,ix,iy,4)
              if(greyok)call winscl('-',0)
              call forceflush()
            endif
          endif
 60     continue
 50   continue

      return
      end


C ********************* VSB3D *********************
C VSB3D - draws the solid boundaries - highlighting the chosen one.
      SUBROUTINE VSB3D(ISBHL)
#include "cfd.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

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

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NSB(ICFD).eq.0) return

C Draw round solid boundaries and label.
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
          CALL ORTTRN(XU(MNOD),YV(JSBi(Isbn,ICFD)),
     &                      ZW(KSBi(Isbn,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ixs,iys)
          CALL ORTTRN(XU(MNOD),YV(JSBf(Isbn,ICFD)+1),
     &                      ZW(KSBi(Isbn,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(MNOD),YV(JSBf(Isbn,ICFD)+1),
     &                      ZW(KSBf(Isbn,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(MNOD),YV(JSBi(Isbn,ICFD)),
     &                      ZW(KSBf(Isbn,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(MNOD),YV(JSBi(Isbn,ICFD)),
     &                      ZW(KSBi(Isbn,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif

C Label in center of boundary.
          Y=(YV(JSBi(Isbn,ICFD))+YV(JSBf(Isbn,ICFD)))/2.
          Z=(ZW(KSBi(Isbn,ICFD))+ZW(KSBf(Isbn,ICFD)))/2.
          CALL ORTTRN(XU(MNOD),Y,Z,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          write (temp,'(a,i2)') 'SB',Isbn
          CALL wstxpt(ix,iy,temp)

        elseif (ILOCAT.eq.3.or.ILOCAT.eq.4) then
          if (ILOCAT.eq.4) then
            MNOD=NJ
          else
            MNOD=2
          endif

C In XZ plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL ORTTRN(XU(ISBi(Isbn,ICFD)),YV(MNOD),
     &                      ZW(KSBi(Isbn,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ixs,iys)
          CALL ORTTRN(XU(ISBf(Isbn,ICFD)+1),YV(MNOD),
     &                      ZW(KSBi(Isbn,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(ISBf(Isbn,ICFD)+1),YV(MNOD),
     &                      ZW(KSBf(Isbn,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(ISBi(Isbn,ICFD)),YV(MNOD),
     &                      ZW(KSBf(Isbn,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(ISBi(Isbn,ICFD)),YV(MNOD),
     &                      ZW(KSBi(Isbn,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif

C Label in center of boundary.
          X=(XU(ISBi(Isbn,ICFD))+XU(ISBf(Isbn,ICFD)))/2.
          Z=(ZW(KSBi(Isbn,ICFD))+ZW(KSBf(Isbn,ICFD)))/2.
          CALL ORTTRN(X,YV(MNOD),Z,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          write (temp,'(a,i2)') 'SB',Isbn
          CALL wstxpt(ix,iy,temp)
        elseif (ILOCAT.eq.5.or.ILOCAT.eq.6) then
          if (ILOCAT.eq.6) then
            MNOD=NK
          else
            MNOD=2
          endif

C In XY plane.
C Transform local coords to screen coords and draw box around boundary.
          CALL ORTTRN(XU(ISBi(Isbn,ICFD)),YV(JSBi(Isbn,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ixs,iys)
          CALL ORTTRN(XU(ISBf(Isbn,ICFD)+1),YV(JSBi(Isbn,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(ISBf(Isbn,ICFD)+1),YV(JSBf(Isbn,ICFD)+1),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(ISBi(Isbn,ICFD)),YV(JSBf(Isbn,ICFD)+1),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(ISBi(Isbn,ICFD)),YV(JSBi(Isbn,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Isbn.eq.ISBHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif

C Label in center of boundary.
          X=(XU(ISBi(Isbn,ICFD))+XU(ISBf(Isbn,ICFD)))/2.
          Y=(YV(JSBi(Isbn,ICFD))+YV(JSBf(Isbn,ICFD)))/2.
          CALL ORTTRN(X,Y,ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          write (temp,'(a,i2)') 'SB',Isbn
          CALL wstxpt(ix,iy,temp)
        endif
 70   continue

      return
      end


C ********************* VAO3D *********************
C VAO3D - draws the air openings - highlighting the chosen one.
      SUBROUTINE VAO3D(IOPENHL)
#include "cfd.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

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

C Check in graphics mode.
      IF(MMOD.ne.8) return

      if(NOPEN(ICFD).eq.0)return

C Draw round solid boundaries and label.
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
          call u2pixel(X1,Y1,ixs,iys)
          CALL ORTTRN(XU(MNOD),YV(JOPENf(Iaop,ICFD)+1),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(MNOD),YV(JOPENf(Iaop,ICFD)+1),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(MNOD),YV(JOPENi(Iaop,ICFD)),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif

C Label in center of boundary.
          Y=(YV(JOPENi(Iaop,ICFD))+YV(JOPENf(Iaop,ICFD)))/2.
          Z=(ZW(KOPENi(Iaop,ICFD))+ZW(KOPENf(Iaop,ICFD)))/2.
          CALL ORTTRN(XU(MNOD),Y,Z,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          write (temp,'(a,i2)') 'AO',Iaop
          CALL wstxpt(ix,iy,temp)

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
          call u2pixel(X1,Y1,ixs,iys)
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(MNOD),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(MNOD),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                      ZW(KOPENf(Iaop,ICFD)+1),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(MNOD),
     &                      ZW(KOPENi(Iaop,ICFD)),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif

C Label in center of boundary.
          X=(XU(IOPENi(Iaop,ICFD))+XU(IOPENf(Iaop,ICFD)))/2.
          Z=(ZW(KOPENi(Iaop,ICFD))+ZW(KOPENf(Iaop,ICFD)))/2.
          CALL ORTTRN(X,YV(MNOD),Z,TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          write (temp,'(a,i2)') 'AO',Iaop
          CALL wstxpt(ix,iy,temp)
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
          call u2pixel(X1,Y1,ixs,iys)
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(JOPENi(Iaop,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(IOPENf(Iaop,ICFD)+1),YV(JOPENf(Iaop,ICFD)+1),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(JOPENf(Iaop,ICFD)+1),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif
          ixs=ix
          iys=iy
          CALL ORTTRN(XU(IOPENi(Iaop,ICFD)),YV(JOPENi(Iaop,ICFD)),
     &                                   ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          if (Iaop.eq.IOPENHL) then
            call edwline(ixs,iys,ix,iy)
          else
            call eswline(ixs,iys,ix,iy)
          endif

C Label in center of boundary.
          X=(XU(IOPENi(Iaop,ICFD))+XU(IOPENf(Iaop,ICFD)))/2.
          Y=(YV(JOPENi(Iaop,ICFD))+YV(JOPENf(Iaop,ICFD)))/2.
          CALL ORTTRN(X,Y,ZW(MNOD),TEMAT,X0,Y0,Z0,IERR)
          CALL ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
          call u2pixel(X1,Y1,ix,iy)
          write (temp,'(a,i2)') 'AO',Iaop
          CALL wstxpt(ix,iy,temp)
        endif
 70   continue

      return
      end



