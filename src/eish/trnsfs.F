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

C TRNSFS.F comprises:
C TRNSF1 - Sets up the coefficients of the coordinate transformation
C          equations.
C TRNSF2 - Expresses the solar azimuth and altitude angles relative to
C          the transformed coordinate system.
C TRNSF3 - Projects the 8 corner points of an obstruction block and
C          expresses the results relative to the transformed coordinate
C          system.
C CORNR  - Returns the X & Y coords of the 4 top corners of the
C          obstruction block.
C PANGS  - Returns the azimuth, elevation and tilt angles of a surface.

C ************** TRNSF1
C Sets up the coefficients of the transform equations
C (translation + z- then x- then y-axis rotations) that move
C the site coordinate system to the local surface under
C consideration. The x and z coordinates of this object body
C face are then re-expressed in terms of the new local
C coordinate system for use in 'point'.

      subroutine trnsf1(izone,is)
#include "building.h"
#include "geometry.h"

      common/tracech/icout
      common/pangl/pazi(ms),pelv(ms),ptilt(ms)
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/g1t/xft(mv),zft(mv)
      integer izstocn
      common/c24/izstocn(mcom,ms)

      dimension yft(mv)
      character outs*124
      character ZSDES*28,ZSDESC*20,ZSDESS*16
      integer lnzsn
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi=4.0*atan(1.0)
      r=pi/180.0

C Convert pazi, pelv and ptilt to the required coordinate axis
C rotations as follows.
C    x-axis - be related to pelv
C    y-axis - ga related to ptilt
C    z-axis - al related to pazi
C Convention: positive angle is anticlockwise when looking along
C the axis towards the origin.
      iq=int(pazi(is)/90.0)
      if(iq.eq.0)goto 1
      goto (1,1,2,2),iq
    1 al=(180.0-pazi(is))*r
      goto 3
    2 al=-(pazi(is)-180.0)*r
    3 be=-pelv(is)*r
      ga=-ptilt(is)*r

C Set up transform equation coefficients.
C Transformation order: z-axis (al from pazi) then
C x-axis (be from pelv) then y-axis (ga from ptilt)
C rotation.
      a(1)=cos(al)*cos(ga)
      a(2)=sin(al)*cos(ga)
      a(3)=sin(al)*sin(be)*sin(ga)
      a(4)=cos(al)*sin(be)*sin(ga)
      a(5)=cos(be)*sin(ga)
      b(1)=sin(al)*cos(be)
      b(2)=cos(al)*cos(be)
      b(3)=sin(be)
      c(1)=cos(al)*sin(ga)
      c(2)=sin(al)*sin(ga)
      c(3)=sin(al)*sin(be)*cos(ga)
      c(4)=cos(al)*sin(be)*cos(ga)
      c(5)=cos(be)*cos(ga)

C Get the current connection
      icc=izstocn(izone,is)

      ip1=nzjvn(icc,1)
      xor=vcoord(izone,ip1,1)
      yor=vcoord(izone,ip1,2)
      zor=vcoord(izone,ip1,3)

C Transform target surface coordinates.
      do 10 i=1,nznver(icc)
         ip=nzjvn(icc,i)
         xs=vcoord(izone,ip,1)
         ys=vcoord(izone,ip,2)
         zs=vcoord(izone,ip,3)

C Transformed x.
         xft(i)=(xs-xor)*a(1)+(ys-yor)*a(2)-(xs-xor)*a(3)
     &          +(ys-yor)*a(4)-(zs-zor)*a(5)

C Transformed y.
         yft(i)=-(xs-xor)*b(1)+(ys-yor)*b(2)+(zs-zor)*b(3)

C Transformed z.
         zft(i)=(xs-xor)*c(1)+(ys-yor)*c(2)+(xs-xor)*c(3)
     &          -(ys-yor)*c(4)+(zs-zor)*c(5)

C Transformed y value should be close to zero; warn if not.
         if(abs(yft(i)).gt.1.E-3)then
            call edisp(icout,' ')
            write(outs,9999)is
 9999       format('TRNSF1: Surface ',i3,' is probably not coplanar!')
            call edisp(icout,outs)
            write(outs,9998)i,xs,ys,zs
 9998       format('Vertex ',i3,', which has coordinates ',3f11.6,',')
            call edisp(icout,outs)
            write(outs,9997)yft(i)
 9997       format('has transformed Y value of ',f11.6,
     &             ' (should be 0).')
            call edisp(icout,outs)
         endif

   10 continue

C Trace output.
      if(tok)then
        icc=izstocn(izone,is)
        call zsid(izone,is,ZSDES,ZSDESC,ZSDESS)
        lnzsn=lnblnk(ZSDES)
        call edisp(icout,' ')
         write(outs,'(3a,i3,a,f5.1,a,f5.1,a,f5.1)')
     &     '*** TRNSF1: Surface ',zsdes(1:lnzsn),' (',is,
     &     ') PAZI= ',pazi(is),'; PELV= ',pelv(is),'; PTILT= ',
     &     ptilt(is)
        call edisp(icout,outs)
        call edisp(icout,' ')
        call edisp(icout,'Axis translations & rotations:')
        write(outs,'(a,2x,3f8.2)')'X, Y & Z translations -',
     &                             xor,yor,zor
        call edisp(icout,outs)
        write(outs,'(a,2x,3f8.1)')
     &    'X (BE), Y (GA) & Z (AL) rotations -',be/r,ga/r,al/r
        call edisp(icout,outs)
        call edisp(icout,'ZXY transform equation coefficients:')
        write(outs,'(a,2x,5f8.2)')'A -',(a(i),i=1,5)
        call edisp(icout,outs)
        write(outs,'(a,2x,5f8.2)')'B -',(b(i),i=1,3)
        call edisp(icout,outs)
        write(outs,'(a,2x,5f8.2)')'C -',(c(i),i=1,5)
        call edisp(icout,outs)
        call edisp(icout,
     &    'Original XYZ coordinates: Transformed X`Y`Z` coords:')
        do 9995 i=1,nznver(icc)
          ip=nzjvn(icc,i)
          xs=vcoord(izone,ip,1)
          ys=vcoord(izone,ip,2)
          zs=vcoord(izone,ip,3)
          write(outs,'(i3,2x,3f8.2,2x,3f8.2)')
     &      i,xs,ys,zs,xft(i),yft(i),zft(i)
          call edisp(icout,outs)
 9995   continue
      endif
      return
      end

C ************** TRNSF2 
C Convert solar angles (sazi1 and salt1) in original coordinate
C system to corresponding angles (sazi2 and salt2) in transformed
C coordinate system (target external surface) to allow obstruction
C point projections in new coordinate system in trnsf3.

      subroutine trnsf2(ihr)

      common/tracech/icout
      common/tc/itc,icnt
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/sangl1/sazi1,salt1
      common/sangl2/sazi2,salt2

      character outs*124
      logical tok

C If writing fort.33 file and verbose then include this.
      tok=.false.
      if(icout.eq.33.and.itc.eq.2)tok=.true.

      pi=4.0*atan(1.0)
      r=pi/180.0
      if(salt1.gt.0.1)then
        tsr=sin(salt1*r)/cos(salt1*r)
      else
        tsr=1.0 ! << ? >>
      endif

C Establish a line from the site origin to a point 'far'
C away at the appropriate solar angles (sazi1 and salt1).
      x1=0.
      y1=0.
      z1=0.

C Dependent on which quadrant sun is in, convert sun position
C to a distant point (x2,y2,z2) for later transformation to the
C new coordinate system, along with point (x1,y1,z1), to give
C the solar incidence line in this new system.
      if(sazi1.lt.90.0)then
         ang=90.0-sazi1
         tang=sin(ang*r)/cos(ang*r)
         y2=5000.
         z2=5000.*tsr/sin(ang*r)
         x2=5000./tang
      elseif(sazi1.gt.90.0.and.sazi1.lt.180.0)then
         ang=sazi1-90.0
         tang=sin(ang*r)/cos(ang*r)
         y2=-5000.
         z2=5000.*tsr/sin(ang*r)
         x2=5000./tang
      elseif(sazi1.gt.180.0.and.sazi1.lt.270.0)then
         ang=sazi1-180.0
         tang=sin(ang*r)/cos(ang*r)
         y2=-5000.
         z2=5000.*tsr/cos(ang*r)
         x2=-5000.*tang
      elseif(sazi1.gt.270.0)then
         ang=sazi1-270.0
         tang=sin(ang*r)/cos(ang*r)
         y2=5000.
         z2=5000.*tsr/sin(ang*r)
         x2=-5000./tang
      endif

      if(sazi1.lt.90.001.and.sazi1.gt.89.999)then
         y2=0.
         x2=5000.
         z2=5000.*tsr
      elseif(sazi1.lt.180.001.and.sazi1.gt.179.999)then
         y2=-5000.
         x2=0.
         z2=5000.*tsr
      elseif(sazi1.lt.270.001.and.sazi1.gt.269.999)then
         x2=-5000.
         y2=0.
         z2=5000.*tsr
      endif

C Translate/rotate both points (in order z-, x- then
C y-axis rotation) to new coordinate system.

C Transformed x.
      xt1=(x1-xor)*a(1)+(y1-yor)*a(2)-(x1-xor)*a(3)+
     &         (y1-yor)*a(4)-(z1-zor)*a(5)
      xt2=(x2-xor)*a(1)+(y2-yor)*a(2)-(x2-xor)*a(3)+
     &         (y2-yor)*a(4)-(z2-zor)*a(5)

C Transformed y.
      yt1=-(x1-xor)*b(1)+(y1-yor)*b(2)+(z1-zor)*b(3)
      yt2=-(x2-xor)*b(1)+(y2-yor)*b(2)+(z2-zor)*b(3)

C Transformed z.
      zt1=(x1-xor)*c(1)+(y1-yor)*c(2)+(x1-xor)*c(3)-
     &         (y1-yor)*c(4)+(z1-zor)*c(5)
      zt2=(x2-xor)*c(1)+(y2-yor)*c(2)+(x2-xor)*c(3)-
     &         (y2-yor)*c(4)+(z2-zor)*c(5)

C Convert transformed points back to pseudo solar angles
C (sazi2 and salt2) expressed relative to the transformed
C coordinate system.
      dist=sqrt((xt2-xt1)**2+(yt2-yt1)**2+(zt2-zt1)**2)
      dz=zt2-zt1
      if(abs(dist).lt.0.0001)then
         call edisp(icout,' ')
         call edisp(icout,'TRNSF2: Sun position transformation error!')
      endif

C Altitude.
      arg=abs(dz)/dist
      salt2=asin(arg)/r
      if(dz.lt.0.0)salt2=-salt2

C Azimuth.
      dx=xt2-xt1
      dy=yt2-yt1
      if(dx.lt.0.001.and.dx.gt.-0.001)then
C Transformed solar azimuth is normal to the transformed surface.
         sazi2=0.
         if(dy.lt.0.)sazi2=180.
      elseif(dy.lt.0.001.and.dy.gt.-0.001)then
C Transformed solar azimuth is parallel to the transformed surface. 
         sazi2=90.
         if(dx.lt.0.)sazi2=270.
      else
         arg=abs(dy/dx)
         angl=atan(arg)/r
         if(dx.gt.0.0.and.dy.lt.0.0)then
            sazi2=90.+angl
         elseif(dx.gt.0.0.and.dy.gt.0.0)then
            sazi2=90.-angl
         elseif(dx.lt.0.0.and.dy.gt.0.0)then
            sazi2=270.+angl
         elseif(dx.lt.0.0.and.dy.lt.0.0)then
            sazi2=270.-angl
         endif
      endif

C Trace output.
      if(tok)then
         call edisp(icout,' ')
         write(outs,9999)ihr
 9999    format('*** TRNSF2: Solar angle transformation at hour ',i2)
         call edisp(icout,outs)
         call edisp(icout,' ')
         write(outs,9998)
 9998    format('Intermediate calculations:')
         call edisp(icout,outs)
         write(outs,9997)x1,y1,z1,x2,y2,z2
 9997    format('X1,Y1,Z1=    ',3f8.1,'; X2,Y2,Z2=    ',3f10.2)
         call edisp(icout,outs)
         write(outs,9996)xt1,yt1,zt1,xt2,yt2,zt2
 9996    format('XT1,YT1,ZT1= ',3f8.1,'; XT2,YT2,ZT2= ',3f10.2)
         call edisp(icout,outs)
         write(outs,9995)dx,dy,dz
 9995    format('DX= ',f10.1,'; DY= ',f10.1,'; DZ= ',f10.1)
         call edisp(icout,outs)
         write(outs,9994)dist,angl,tsr
 9994    format('DIST= ',F10.1,'; ANGL= ',F10.2,'TSR= ',f9.3)
         call edisp(icout,outs)
         call edisp(icout,' ')
         write(outs,9993)sazi1,sazi2
 9993    format('Solar azimuth of ',f8.2,' transforms to ',f7.2)
         call edisp(icout,outs)
         write(outs,9992)salt1,salt2
 9992    format('Solar altitude of  ',f7.2,' transforms to ',f7.2)
         call edisp(icout,outs)
      endif
      return
      end

C ************** TRNSF3 ********************
C Projects the 4 vertices of an obstruction block surface on to the
C the plane of the target surface. The projected points are specified
C relative to the local face coordinate system. The y-coordinate is
C left unprojected but in the new coordinate system.

      subroutine trnsf3(ihr,izone,ibn,ibs,iztsur,iskypatch)
#include "building.h"
#include "geometry.h"

C Parameters
      integer ihr    ! hour of the day
      integer izone  ! current zone
      integer ibn    ! obstruction index
      integer ibs    ! index of obstruction face
      integer iztsur ! target surface in zone
      integer iskypatch ! which sky patch we are dealing with

      common/tracech/icout
      common/obs/xc(8),yc(8),zc(8)
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/sangl2/sazi2,salt2
      common/transform/xt(4),yt(4),zt(4)
      common/proj/xp(4),yp(4),zp(4)

      dimension ivorder(6,4)
      character ZSDES*28,ZSDESC*20,ZSDESS*16
      character outs*124,msg*15
      integer lnzsn
      logical tok

C Anticlockwise ordered vertices of the 6 obstruction block surfaces
C (surfaces read vertically).
      data ivorder/5,6,7,8,1,5,
     &             6,7,8,5,2,8,
     &             2,3,4,1,3,7,
     &             1,2,3,4,4,6/

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi=4.0*atan(1.0)
      r=pi/180.0

C Establish projection angles, eta1 and eta2, which related to the
C pseudo azimuth, sazi2, and pseudo altitude, salt2, respectively.
      eta1=180.0-sazi2
      eta2=salt2

C Establish trigonometric coefficients for use in the following
C projection transforms: ta1 and ta2 are -ve for +ve values of
C eta1 and eta2 respectively.
      a1=abs(eta1*r)
      sa1=sin(a1)
      ca1=cos(a1)
      if(abs(ca1).lt.0.00001)ca1=0.00001
      ta1=sa1/ca1
      if(eta1.gt.0.0)ta1=-ta1

      a2=abs(eta2*r)
      sa2=sin(a2)
      ca2=cos(a2)
      if(abs(ca2).lt.0.00001)ca2=0.00001
      ta2=sa2/ca2
      if(eta2.GT.0.0)ta2=-ta2

C Transform and project the 4 surface points in anticlockwise order. 
      do 10 i=1,4
         ip=ivorder(ibs,i) 

C Transform Y (no need to project since YP will either be 0 or, if
C +ve, i.e. behind the plane, then it will be reset to 0 by finding
C the plane intersection point later).
         yt(i)=-(xc(ip)-xor)*b(1)+(yc(ip)-yor)*b(2)+(zc(ip)-zor)*b(3)
         yp(i)=0.0

C ta1 and ta2 above assume that yt() is +ve whereas it will be
C -ve; xmult imposes the necessary correction. Where a block
C crosses the plane of the surface, a point with a +ve yt() value
C is back-projected at this point but reset in decomp where the
C intersection of the obstruction block with the y'=0 plane is
C determined.
         xmult=-1.0

C Transform and project x.
         xt(i)=(xc(ip)-xor)*a(1)+(yc(ip)-yor)*a(2)-(xc(ip)-xor)*a(3)
     &         +(yc(ip)-yor)*a(4)-(zc(ip)-zor)*a(5)
         xp(i)=xt(i)+yt(i)*ta1*xmult

C Tranform and project z.
         zt(i)=(xc(ip)-xor)*c(1)+(yc(ip)-yor)*c(2)+(xc(ip)-xor)*c(3)
     &         -(yc(ip)-yor)*c(4)+(zc(ip)-zor)*c(5)
         zp(i)=yt(i)*ta2/ca1*xmult+zt(i)
   10 continue

C Trace output. Include context for the calculation.
      if(tok)then
        call edisp(icout,' ')
        call zsid(izone,iztsur,ZSDES,ZSDESC,ZSDESS)
        lnzsn=lnblnk(zsdes)
        if(iskypatch.eq.1)then
          msg=' for direct rad'
        else
          msg=' for sky patch '
        endif
        write(outs,'(3a,i3,a,i3,3a,i2,a,i2,a,i3)')
     &    '*** TRNSF3: projection of obstruction ',
     &    blockname(izone,ibn)(1:lnblockname(izone,ibn)),' (',ibn,
     &    ') face (',ibs,') on surface ',ZSDES(1:lnzsn),' (',iztsur,
     &    ') at hour ',ihr,msg,iskypatch
        call edisp(icout,outs)
        call edisp(icout,
     &    'X,Y,Z coordinates:   X`,Y`,Z` coords:  XP,YP,ZP  coords:')
        do 9998 i=1,4
          ip=ivorder(ibs,i)
          write(outs,'(i2,3f8.2,i4,3f8.2,i4,3f8.2)')
     &      ip,xc(ip),yc(ip),zc(ip),
     &      i,xt(i),yt(i),zt(i),i,xp(i),yp(i),zp(i)
          call edisp(icout,outs)
 9998   continue
        write(outs,'(a,f5.1,a,f5.1,a,f8.2,a,f8.2)')
     &   'Pseudo solar angles: ETA1= ',eta1,'; ETA2= ',eta2,
     &   'TA1= ',ta1,'; TA2= ',ta2
        call edisp(icout,outs)
      endif
      return
      end

C ************** CORNR ********************
C Returns the x, y and z coordinates of the 8 vertices of an
C obstruction block (ibn) in zone (iz), where vertices 1-4 are
C anticlockwise uppermost and verticed 5-8 are anticlockwise lowermost.
C At this stage the coordinates are relative to the original
C coordinate system.

C The ordering of the coordinates is shown below:
C
C        4____________3
C       /|          /|
C     1/_|_________/2|
C     |  |         | |
C     |  |         | |
C     |  |8........|.|7
C     | /          | /
C     |/___________|/ . . .W->
C      5           6
C

      subroutine cornr(iz,ibn)
#include "building.h"
#include "geometry.h"

C Parameter
      integer iz  ! zone index
      integer ibn ! index of block

      common/tracech/icout
      common/tc/itc,icnt
      common/obs/xc(8),yc(8),zc(8)
      integer iobstrtrace  ! set to non-zero if already written
      common/obstrc/iobstrtrace(mcom,mb)
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

      character outs*124
      logical tok

C If the trace unit is 33 and verbose and this obstruction has not
C already been reported on set tok to true.
      tok=.false.
      if(icout.eq.33.and.itc.eq.2)then
        if(iobstrtrace(iz,ibn).eq.0) tok=.true.
      endif
      
      if(BLOCKTYP(iz,ibn)(1:4).eq.'obs ')then

C Use cnvblk to get the standard layout of xyz coordinates.
        CALL CNVBLK(XOB(iz,ibn),YOB(iz,ibn),ZOB(iz,ibn),
     &    DXOB(iz,ibn),DYOB(iz,ibn),DZOB(iz,ibn),BANGOB(iz,ibn,1))

C Copy across from GB1 to obs common block. Treat the Z coordinates
C individually to map to the coordinate layout used within the obs
C common block.
        do 42 ib=1,8
          xc(ib)=XB(ib)
          yc(ib)=YB(ib)
  42    continue
        zc(1)=ZB(5)
        zc(2)=ZB(6)
        zc(3)=ZB(7)
        zc(4)=ZB(8)
        zc(5)=ZB(1)
        zc(6)=ZB(2)
        zc(7)=ZB(3)
        zc(8)=ZB(4)
      elseif(BLOCKTYP(iz,ibn)(1:4).eq.'obs3')then
        CALL CNVBLK3A(XOB(iz,ibn),YOB(iz,ibn),ZOB(iz,ibn),
     &    DXOB(iz,ibn),DYOB(iz,ibn),DZOB(iz,ibn),
     &    BANGOB(iz,ibn,1),BANGOB(iz,ibn,2),BANGOB(iz,ibn,3))

C Copy across from GB1 to obs common block. Treat the Z coordinates
C individually to map to the coordinate layout used within the obs
C common block.
        do 43 ib=1,8
          xc(ib)=XB(ib)
          yc(ib)=YB(ib)
  43    continue
        zc(1)=ZB(5)
        zc(2)=ZB(6)
        zc(3)=ZB(7)
        zc(4)=ZB(8)
        zc(5)=ZB(1)
        zc(6)=ZB(2)
        zc(7)=ZB(3)
        zc(8)=ZB(4)
      elseif(BLOCKTYP(iz,ibn)(1:4).eq.'obsp')then
        CALL CNVBLKP(iz,ibn) ! convert obsp type.

C Copy across from GB1 to obs common block. Treat the Z coordinates
C individually to map to the coordinate layout used within the obs
C common block.
        do 44 ib=1,8
          xc(ib)=XB(ib)
          yc(ib)=YB(ib)
  44    continue
        zc(1)=ZB(5)
        zc(2)=ZB(6)
        zc(3)=ZB(7)
        zc(4)=ZB(8)
        zc(5)=ZB(1)
        zc(6)=ZB(2)
        zc(7)=ZB(3)
        zc(8)=ZB(4)
      endif

      if(tok)then
         call edisp(icout,' ')
         write(outs,'(a,i3,a)')'*** CORNR: Block ',ibn,
     &                         ' site coordinates (XC,YC,ZC)'
         call edisp(icout,outs)
         call edisp(icout,'    X-Coord  Y-Coord  Z-Coord')
         do 9999 i=1,8
            write(outs,'(i3,3f9.2)')i,xc(i),yc(i),zc(i)
            call edisp(icout,outs)
 9999    continue
         iobstrtrace(iz,ibn)=1
      endif
      return
      end

C ******************* PANGS **************************
C omputes the azimuth (pazi), elevation (pelv) and local
C x-axis tilt (ptilt) angles for the target surface where:

C   pazi  - is the angle measured from north to the face normal,
C           clockwise positive.
C   pelv  - is the angle measured from the horizontal to the
C           face normal (upward facing horizontal surface 90 deg.,
C           downward facing horizontal surface -90 deg., 
C           vertical surface 0 deg.
C   ptilt - is the angle between the face x-axis and the horizontal
C           where the x-axis is the line joining the 1st and 2nd
C           face vertices.

      subroutine pangs(izone,is)
#include "building.h"
#include "geometry.h"

      common/pangl/pazi(ms),pelv(ms),ptilt(ms)
      integer izstocn
      common/c24/izstocn(mcom,ms)

      logical bHorizontal

      pi = 4.0 * atan(1.0)
      r=pi/180.
      xsum=0.
      ysum=0.
      zsum=0.

C Compute azimuth and elevation angles for face 'IS'.
C Get the current connection
      icc=izstocn(izone,is)
      nv=nznver(icc)
      do 10 i=1,nv
         k=i+1
         if(i.eq.nv)k=1
         ip1=nzjvn(icc,i)
         ip2=nzjvn(icc,k)

         xsum=xsum+vcoord(izone,ip1,2)*vcoord(izone,ip2,3)-
     &        vcoord(izone,ip1,3)*vcoord(izone,ip2,2)
         ysum=ysum+vcoord(izone,ip1,3)*vcoord(izone,ip2,1)-
     &        vcoord(izone,ip1,1)*vcoord(izone,ip2,3)
         zsum=zsum+vcoord(izone,ip1,1)*vcoord(izone,ip2,2)-
     &        vcoord(izone,ip1,2)*vcoord(izone,ip2,1)
   10 continue
      pazi(is)=90.0
      if(abs(xsum).lt.0.00001)pazi(is)=0.0
      if(xsum.lt.0.0)pazi(is)=-90.0
      pelv(is)=90.0
      if(abs(zsum).lt.0.00001)pelv(is)=0.0
      if(zsum.lt.0.0)pelv(is)=-90.0
      if(abs(ysum).lt.0.00001)goto 1
      pazi(is)=atan2(xsum,ysum)/r
    1 srx2y2=sqrt(xsum*xsum+ysum*ysum)
      if(pazi(is).lt.0.)pazi(is)=pazi(is)+360.
      if(abs(srx2y2).lt.0.00001)goto 2
      pelv(is)=atan2(zsum,srx2y2)/r
    2 continue

C Now compute local X-axis tilt angle.
      iv1=nzjvn(icc,1)
      iv2=nzjvn(icc,2)
      dz=vcoord(izone,iv2,3)-vcoord(izone,iv1,3)
      dist=((vcoord(izone,iv2,1)-vcoord(izone,iv1,1))**2+
     &      (vcoord(izone,iv2,2)-vcoord(izone,iv1,2))**2+
     &      (vcoord(izone,iv2,3)-vcoord(izone,iv1,3))**2)**.5
      ptilt(is)=asin(dz/dist)/r
      if(vcoord(izone,iv1,3).gt.vcoord(izone,iv2,3))
     &  ptilt(is)=-ptilt(is)

C For the special case of a horizontal surface, the
C azimuth calculated above is zero. It is now reset to
C the azimuth of the plane comprising the first two
C vertices of the face and the same two vertices but with
C an adjusted Z value.

      call eclose ( abs(pelv(is)), 90., 0.001, bHorizontal )

      if ( bHorizontal ) then

        xsum=0.
        ysum=0.
        zsum=0.
        do 20 i=1,4
          if(i.eq.1)then
              ip1=nzjvn(icc,1)
              ip2=nzjvn(icc,2)
              xx1=vcoord(izone,ip1,1)
              xx2=vcoord(izone,ip2,1)
              yy1=vcoord(izone,ip1,2)
              yy2=vcoord(izone,ip2,2)
              zz1=vcoord(izone,ip1,3)
              zz2=vcoord(izone,ip2,3)
          elseif(i.eq.2)then
              ip1=nzjvn(icc,2)
              ip2=nzjvn(icc,2)
              xx1=vcoord(izone,ip1,1)
              xx2=vcoord(izone,ip2,1)
              yy1=vcoord(izone,ip1,2)
              yy2=vcoord(izone,ip2,2)
              zz1=vcoord(izone,ip1,3)
              zz2=vcoord(izone,ip2,3)+0.1
          elseif(i.eq.3)then
              ip1=nzjvn(icc,2)
              ip2=nzjvn(icc,1)
              xx1=vcoord(izone,ip1,1)
              xx2=vcoord(izone,ip2,1)
              yy1=vcoord(izone,ip1,2)
              yy2=vcoord(izone,ip2,2)
              zz1=vcoord(izone,ip1,3)+0.1
              zz2=vcoord(izone,ip2,3)+0.1
          elseif(i.eq.4)then
              ip1=nzjvn(icc,1)
              ip2=nzjvn(icc,1)
              xx1=vcoord(izone,ip1,1)
              xx2=vcoord(izone,ip2,1)
              yy1=vcoord(izone,ip1,2)
              yy2=vcoord(izone,ip2,2)
              zz1=vcoord(izone,ip1,3)+0.1
              zz2=vcoord(izone,ip2,3)
          endif
          xsum=xsum+yy1*zz2-zz1*yy2
          ysum=ysum+zz1*xx2-xx1*zz2
          zsum=zsum+xx1*yy2-yy1*xx2
  20    continue
        pazi(is)=90.0
        if(abs(xsum).lt.0.00001)pazi(is)=0.0
        if(xsum.lt.0.0)pazi(is)=-90.0
        if(abs(ysum).lt.0.00001)goto 3
        pazi(is)=atan2(xsum,ysum)/r
    3   if(pazi(is).lt.0.)pazi(is)=pazi(is)+360.

      endif
      return
      end
