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

      subroutine trnsf1(is)
#include "building.h"

      common/tracech/icout
      common/pangl/pazi,pelv,ptilt
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/g1t/xft(mv),zft(mv)

      dimension yft(mv)
      character outs*124
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi = 4.0 * atan(1.0)
      r = pi/180.0

C Compute the surface azimuth (pazi), elevation (pelv)
C X-axis tilt (ptilt) angles.
      call pangs(is)

C Convert pazi, pelv and ptilt to the required coordinate axis
C rotations as follows.
C    x-axis - be related to pelv
C    y-axis - ga related to ptilt
C    z-axis - al related to pazi
C Convention: positive angle is anticlockwise when looking along
C the axis towards the origin.
      iq=int(pazi/90.0)
      if(iq.eq.0)goto 1
      goto (1,1,2,2),iq
    1 al=(180.0-pazi)*r
      goto 3
    2 al=-(pazi-180.0)*r
    3 be=-pelv*r
      ga=-ptilt*r

C Set up transform equation coefficients.
C Transformation order: Z-axis (al from pazi) then
C x-axis (be from pelv) then Y-axis (ga from ptilt)
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
      ip1=jvn(is,1)
      xor=x(ip1)
      yor=y(ip1)
      zor=z(ip1)

C Transform target surface coordinates.
      do 10 i=1,nver(is)
         xs=x(jvn(is,i))
         ys=y(jvn(is,i))
         zs=z(jvn(is,i))

C Transformed x.
         xft(i)=(xs-xor)*a(1)+(ys-yor)*a(2)-(xs-xor)*a(3)
     &          +(ys-yor)*a(4)-(zs-zor)*a(5)

C Transformed y.
         yft(i)=-(xs-xor)*b(1)+(ys-yor)*b(2)+(zs-zor)*b(3)

C Transformed z.
         zft(i)=(xs-xor)*c(1)+(ys-yor)*c(2)+(xs-xor)*c(3)
     &          -(ys-yor)*c(4)+(zs-zor)*c(5)

C Transformed Y value should be zero; error if not.
         if(abs(yft(i)).gt.1.E-5)then
            call edisp(icout,' ')
            write(outs,9999)is
 9999       format('TRNSF1: Surface ',I2,' is probably not coplanar!')
            call edisp(icout,outs)
            write(outs,9998)i,xs,ys,zs
 9998       format('Vertex ',i2,', which has coordinates ',3f11.6,',')
            call edisp(icout,outs)
            write(outs,9997)yft(i)
 9997       format('has transformed Y value of ',f11.6,
     &             ' (should be 0).')
            call edisp(icout,outs)
         endif
   10 continue

C Trace output.
      if(tok)then
         call edisp(icout,' ')
         write(outs,9996)is,pazi,pelv,ptilt
 9996    format('*** TRNSF1: Surface ',i2,': PAZI= ',f5.1,'; PELV= ',
     &          f5.1,'; PTILT= ',f5.1)
         call edisp(icout,outs)
         call edisp(icout,' ')
         call edisp(icout,'Axis translations & rotations:')
         write(outs,'(a,2x,3f8.1)')'X, Y & Z translations -',
     &                             xor,yor,zor
         call edisp(icout,outs)
         write(outs,'(a,2x,3f8.1)')
     &           'X (BE), Y (GA) & Z (AL) rotations -',be/r,ga/r,al/r
         call edisp(icout,outs)
         call edisp(icout,'ZXY transform equation coefficients:')
         write(outs,'(a,2x,5f8.1)')'A -',(a(i),i=1,5)
         call edisp(icout,outs)
         write(outs,'(a,2x,5f8.1)')'B -',(b(i),i=1,3)
         call edisp(icout,outs)
         write(outs,'(a,2x,5f8.1)')'C -',(c(i),i=1,5)
         call edisp(icout,outs)
         call edisp(icout,'Original XYZ coordinates:')
         do 9995 i=1,nver(is)
           xs=x(jvn(is,i))
           ys=y(jvn(is,i))
           zs=z(jvn(is,i))
           write(outs,'(i2,2x,3f8.1)')i,xs,ys,zs
           call edisp(icout,outs)
 9995    continue
         call edisp(icout,'Transformed X`Y`Z` coordinates:')
         do 9994 i=1,nver(is)
           write(outs,'(i2,2x,3f8.1)')i,xft(i),yft(i),zft(i)
           call edisp(icout,outs)
 9994    continue
      endif
      return
      end

C ************** TRNSF2 
C Convert solar angles (sazi1 and salt1) in original coordinate
C system to corresponding angles (sazi2,salt2) in transformed
C coordinate system to allow obstruction point projections
C in new coordinate system in TRNSF3.

      subroutine trnsf2(ihr)

      common/tracech/icout
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/sangl1/sazi1,salt1
      common/sangl2/sazi2,salt2

      character outs*124
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi = 4.0 * atan(1.0)
      r=pi/180.0
      tsr=sin(salt1*r)/cos(salt1*r)

C Establish a line from the site origin to a point 'far'
C away at the appropriate solar angles (sazi1 and salt1).
      x1=0.
      y1=0.
      z1=0.

C Dependent on which quadrant sun is in, convert sun position
C to a distant point (x2,y2,z2) for later transformation to the
C new coordinate system, along with point (x1,y1,z1), to give
C the solar incindence line in this new system.
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
      elseif(sazi1.gt.180.0.and.sazi1.lt.270.0)theN
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

c Transformed x.
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

C Special cases:
C 1. When transformed solar azimuth is normal to the
C transformed surface.
      if(dx.lt..001.and.dx.gt.-.001)then
         sazi2=0.

C 2. When surface is facing away from the transformed sun.
         if(dy.lt.0.)sazi2=180.

C 3. When the transformed solar azimuth is parallel to
C the transformed surface. 
      elseif(dy.lt.0.001.and.dy.gt.-0.001)then
         sazi2=90.
         if(dx.lt.0.)sazi2=270.
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
 9997    format('X1,Y1,Z1=    ',3f8.1,'; X2,Y2,Z2=    ',3f10.1)
         call edisp(icout,outs)
         write(outs,9996)xt1,yt1,zt1,xt2,yt2,zt2
 9996    format('XT1,YT1,ZT1= ',3f8.1,'; XT2,YT2,ZT2= ',3f10.1)
         call edisp(icout,outs)
         write(outs,9995)dx,dy,dz
 9995    format('DX= ',f10.1,'; DY= ',f10.1,'; DZ= ',f10.1)
         call edisp(icout,outs)
         write(outs,9994)dist,angl
 9994    format('DIST= ',F10.1,'; ANGL= ',F10.1)
         call edisp(icout,outs)
         call edisp(icout,' ')
         write(outs,9993)sazi1,sazi2
 9993    format('Solar azimuth of ',f8.1,' transforms to ',f6.1)
         call edisp(icout,outs)
         write(outs,9992)salt1,salt2
 9992    format('Solar altitude of  ',f6.1,' transforms to ',f6.1)
         call edisp(icout,outs)
      endif
      return
      end

C ************** TRNSF3 ********************
C Projects the 4 vertices of an obstruction block surface on to the
C the plane of the target surface. The projected points are specified
C relative to the local face coordinate system. The y-coordinate is
C left unprojected but in the new coordinate system.

      subroutine trnsf3(ibn,ibs)

      common/tracech/icout
      common/obs/xc(8),yc(8),zc(8)
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/sangl2/sazi2,salt2
      common/transform/xt(4),yt(4),zt(4)
      common/proj/xp(4),yp(4),zp(4)

      dimension ivorder(6,4)
      character outs*124
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
      if(abs(ca2).lt..00001)ca2=.00001
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

C Trace output.
      if(tok)then
         call edisp(icout,' ')
         write(outs,9999)ibn,ibs
 9999    format('*** TRNSF3: projection of obstruction block ',i2,
     &          ', surface ',i2,'.')
         call edisp(icout,outs)
         call edisp(icout,'     X-Coord   Y-Coord   Z-Coord')
         call edisp(icout,'X,Y,Z coordinates:')
         do 9998 i=1,4
            ip=ivorder(ibs,i)
            write(outs,'(i2,3f10.1)')ip,xc(ip),yc(ip),zc(ip)
            call edisp(icout,outs)
 9998    continue
         call edisp(icout,'X`,Y`,Z` coordinates:')
         do 9997 i=1,4
            write(outs,'(i2,3f10.1)')i,xt(i),yt(i),zt(i)
            call edisp(icout,outs)
 9997    continue
         write(outs,9996)eta1,eta2
 9996    format('Pseudo solar angles: ETA1= ',f5.1,'; ETA2= ',f5.1)
         call edisp(icout,outs)
         write(outs,9995)ta1,ta2
 9995    format('TA1= ',f8.1,'; TA2= ',f8.1)
         call edisp(icout,outs)
         call edisp(icout,'XP,YP,ZP coordinates:')
         do 9994 i=1,4
            write(outs,'(i2,3f10.1)')i,xp(i),yp(i),zp(i)
            call edisp(icout,outs)
 9994    continue
      endif
      return
      end

C ************** CORNR ********************
C Returns the x, y and z coordinates of the 8 vertices of an
C obstruction block, where vertices 1-4 are anticlockwise uppermost
C and verticed 5-8 are anticlockwise lowermost. At this stage the
C coordinates are relative to the original coordinate system.

      subroutine cornr(ibn)
#include "building.h"

      common/tracech/icout
      common/gs5/nb,xo(mb),y0(mb),zo(mb),dx(mb),dy(mb),dz(mb),ang(mb)
      common/obs/xc(8),yc(8),zc(8)

      character outs*124
      logical tok

      tok=.false.
      if(icout.eq.33)tok=.true.

      pi=4.0*atan(1.0)
      r=pi/180.
      ca=cos(ang(ibn)*r)
      sa=sin(ang(ibn)*r)
      xc(1)=xo(ibn)
      yc(1)=y0(ibn)
      zc(1)=zo(ibn)+dz(ibn)
      xc(5)=xc(1)
      yc(5)=yc(1)
      zc(5)=zo(ibn)
      xc(2)=xc(1)+dx(ibn)*ca
      yc(2)=yc(1)+dx(ibn)*sa
      zc(2)=zc(1)
      xc(6)=xc(2)
      yc(6)=yc(2)
      zc(6)=zc(5)
      xc(3)=xc(2)-dy(ibn)*sa
      yc(3)=yc(2)+dy(ibn)*ca
      zc(3)=zc(1)
      xc(7)=xc(3)
      yc(7)=yc(3)
      zc(7)=zc(5)
      xc(4)=xc(1)-dy(ibn)*sa
      yc(4)=yc(1)+dy(ibn)*ca
      zc(4)=zc(1)
      xc(8)=xc(4)
      yc(8)=yc(4)
      zc(8)=zc(5)

      if(tok)then
         call edisp(icout,' ')
         write(outs,'(a,i2,a)')'*** CORNR: Block ',ibn,
     &                         ' site coordinates (XC,YC,ZC)'
         call edisp(icout,outs)
         call edisp(icout,'     X-Coord   Y-Coord   Z-Coord')
         do 9999 i=1,8
            write(outs,'(i2,3f10.1)')i,xc(i),yc(i),zc(i)
            call edisp(icout,outs)
 9999    continue
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

      subroutine pangs(is)
#include "building.h"

      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/pangl/pazi,pelv,ptilt

      pi = 4.0 * atan(1.0)
      r=pi/180.
      xsum=0.
      ysum=0.
      zsum=0.

C Compute azimuth and elevation angles for face 'IS'.
      nv=nver(is)
      do 10 i=1,nv
         k=i+1
         if(i.eq.nv)k=1
         ip1=jvn(is,i)
         ip2=jvn(is,k)
         xsum=xsum+y(ip1)*z(ip2)-z(ip1)*y(ip2)
         ysum=ysum+z(ip1)*x(ip2)-x(ip1)*z(ip2)
         zsum=zsum+x(ip1)*y(ip2)-y(ip1)*x(ip2)
   10 continue
      pazi=90.0
      if(abs(xsum).lt.0.00001)pazi=0.0
      if(xsum.lt.0.0)pazi=-90.0
      pelv=90.0
      if(abs(zsum).lt.0.00001)pelv=0.0
      if(zsum.lt.0.0)pelv=-90.0
      if(abs(ysum).lt.0.00001)goto 1
      pazi=atan2(xsum,ysum)/r
    1 srx2y2=sqrt(xsum*xsum+ysum*ysum)
      if(pazi.lt.0.)pazi=pazi+360.
      if(abs(srx2y2).lt.0.00001)goto 2
      pelv=atan2(zsum,srx2y2)/r
    2 continue

C Now compute local X-axis tilt angle.
      iv1=jvn(is,1)
      iv2=jvn(is,2)
      dz=z(iv2)-z(iv1)
      dist=((x(iv2)-x(iv1))**2+(y(iv2)-y(iv1))**2+
     &(z(iv2)-z(iv1))**2)**.5
      ptilt=asin(dz/dist)/r
      if(z(iv1).gt.z(iv2))ptilt=-ptilt

C For the special case of a horizontal surface, the
C azimuth calculated above is zero. It is now reset to
C the azimuth of the plane comprising the first two
C vertices of the face and the same two vertices but with
C an adjusted Z value.
      xsum=0.
      ysum=0.
      zsum=0.
      do 20 i=1,4
         if(i.eq.1)then
            ip1=jvn(is,1)
            ip2=jvn(is,2)
            xx1=x(ip1)
            xx2=x(ip2)
            yy1=y(ip1)
            yy2=y(ip2)
            zz1=z(ip1)
            zz2=z(ip2)
         elseif(i.eq.2)then
            ip1=jvn(is,2)
            ip2=jvn(is,2)
            xx1=x(ip1)
            xx2=x(ip2)
            yy1=y(ip1)
            yy2=y(ip2)
            zz1=z(ip1)
            zz2=z(ip2)+0.1
         elseif(i.eq.3)then
            ip1=jvn(is,2)
            ip2=jvn(is,1)
            xx1=x(ip1)
            xx2=x(ip2)
            yy1=y(ip1)
            yy2=y(ip2)
            zz1=z(ip1)+0.1
            zz2=z(ip2)+0.1
         elseif(i.eq.4)then
            ip1=jvn(is,1)
            ip2=jvn(is,1)
            xx1=x(ip1)
            xx2=x(ip2)
            yy1=y(ip1)
            yy2=y(ip2)
            zz1=z(ip1)+0.1
            zz2=z(ip2)
         endif
         xsum=xsum+yy1*zz2-zz1*yy2
         ysum=ysum+zz1*xx2-xx1*zz2
         zsum=zsum+xx1*yy2-yy1*xx2
   20 continue
      pazi=90.0
      if(abs(xsum).lt.0.00001)pazi=0.0
      if(xsum.lt.0.0)pazi=-90.0
      if(abs(ysum).lt.0.00001)goto 3
      pazi=atan2(xsum,ysum)/r
    3 if(pazi.lt.0.)pazi=pazi+360.

      return
      end
