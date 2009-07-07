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

C TRNSFI.F comprises:
C TRNSF1I - Sets up the coefficients of the coordinate transformation
C           equations.
C TRNSF2I - Expresses the solar azimuth and altitude angles relative to
C           the transformed coordinate system.
C TRNSF3I - Projects a window vertex and expresses the result relative
C           to the transformed coordinate system.

C ************** TRNSF1I
C Sets up the coefficients of the transform equations
C (translation + z- then x- then y-axis rotations) that move
C the site coordinate system to the local surface under
C consideration. The x and z coordinates of this object body
C face are then re-expressed in terms of the new local
C coordinate system for use in point. This routine is
C essentially a copy of trnsf1.

      subroutine trnsf1i(izone,is)
#include "building.h"
#include "geometry.h"

      common/tracech/icout
      common/pangl/pazi(ms),pelv(ms),ptilt(ms)
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/g1t/xft(mv),zft(mv)
      common/c24/izstocn(mcom,ms)

      dimension yft(mv)
      character outs*124
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi=4.0*atan(1.0)
      r=pi/180.

C Convert pazi, pelv and ptilt to the required coordinate axis
C rotations as follows.
C    x-axis - be related to pelv
C    y-axis - ga related to ptilt
C    z-axis - al related to pazi
C Convention: positive angle is anticlockwise when looking along
C the axis towards the origin.
      iq=int(pazi(is)/90.0)
      if(iq.eq.0) goto 1
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
 9999       format('TRNSF1I: Surface ',i2,
     &                         ' is probably not coplanar!')
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
         icc=IZSTOCN(izone,is)
         call edisp(icout,' ')
         write(outs,9996)is,pazi(is),pelv(is),ptilt(is)
 9996    format('*** TRNSF1I: Surface ',i2,': PAZI= ',f5.1,'; PELV= ',
     &          f5.1,'; PTILT= ',f5.1)
         call edisp(icout,outs)
         call edisp(icout,' ')
         call edisp(icout,'Axis translations & rotations:')
         write(outs,'(a,2x,3f8.1)')'X, Y & Z translations -',
     &                             xor,yor,zor
         call edisp(icout,outs)
         write(outs,'(a,2x,3f8.1)')
     &         'X (BE), Y (GA) & Z (AL) rotations -',be/r,ga/r,al/r
         call edisp(icout,outs)
         call edisp(icout,'ZXY transform equation coefficients:')
         write(outs,'(a,2x,5f8.1)')'A -',(a(i),i=1,5)
         call edisp(icout,outs)
         write(outs,'(a,2x,5f8.1)')'B -',(b(i),i=1,3)
         call edisp(icout,outs)
         write(outs,'(a,2x,5f8.1)')'C -',(c(i),i=1,5)
         call edisp(icout,outs)
         call edisp(icout,'Original XYZ coordinates:')
         do 9995 i=1,nznver(icc)
           ip=nzjvn(icc,i)
           xs=vcoord(izone,ip,1)
           ys=vcoord(izone,ip,2)
           zs=vcoord(izone,ip,3)
           write(outs,'(i2,2x,3f8.1)')i,xs,ys,zs
           call edisp(icout,outs)
 9995    continue
         call edisp(icout,'Transformed X`Y`Z` coordinates:')
         do 9994 i=1,nznver(icc)
            write(outs,'(i2,2x,3f8.1)')i,xft(i),yft(i),zft(i)
            call edisp(icout,outs)
 9994    continue
      endif
      return
      end

C ************** TRNSF2I 
C Convert solar angles (sazi1 and salt1) in original coordinate
C system to corresponding angles (sazi2 and salt2) in transformed
C coordinate system (target internal surface) to allow TMC grid
C point projections in new coordinate system in trnsf3i. This
C routine is essentially a copy of trnsf2 used for shading.

      subroutine trnsf2i(ihr)

      common/outin/iuout,iuin
      common/tracech/icout
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/sangl1/sazi1,salt1
      common/sangl2/sazi2,salt2

      character outs*124
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi=4.0*atan(1.0)
      r=pi/180.
      tsr=sin(salt1*r)/cos(salt1*r)

C Establish a line from the site origin to a point 'far'
C away at the appropriate solar angles (sazi1 and salt1).
      x1=0.
      y1=0.
      z1=0.

C Dependent on which quadrant sun is in, convert sun position
C to a distant point (x2,y2,z2) for later transformation to the
C new coordinate system, along with point (x1,y1,z1), to give
C the solar incidence line in this new system.
      if(sazi1.lt.90.)then
         ang=90.-sazi1
         tang=sin(ang*r)/cos(ang*r)
         y2=5000.
         z2=5000.*tsr/sin(ang*r)
         x2=5000./tang
      elseif(sazi1.gt.90..and.sazi1.lt.180.)then
         ang=sazi1-90.0
         tang=sin(ang*r)/cos(ang*r)
         y2=-5000.
         z2=5000.*tsr/sin(ang*r)
         x2=5000./tang
      elseif(sazi1.gt.180..and.sazi1.lt.270.)then
         ang=sazi1-180.
         tang=sin(ang*r)/cos(ang*r)
         y2=-5000.
         z2=5000.*tsr/cos(ang*r)
         x2=-5000.*tang
      elseif(sazi1.gt.270.)then
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
        call edisp(iuout,'TRNSF2I: Sun position transformation error!')
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
 9999    format('*** TRNSF2I: Solar angle transformation at hour ',i2)
         call edisp(icout,outs)
         call edisp(icout,' ')
         write(outs,9998)
 9998    format('Intermediate calculations:')
         call edisp(icout,outs)
         write(outs,9997)x1,y1,z1,x2,y2,z2
 9997    format('X1,Y1,Z1= ',3f8.1,'; X2,Y2,Z2= ',3f8.1)
         call edisp(icout,outs)
         write(outs,9996)xt1,yt1,zt1,xt2,yt2,zt2
 9996    format('XT1,YT1,ZT1= ',3f8.1,'; XT2,YT2,ZT2= ',3f8.1)
         call edisp(icout,outs)
         write(outs,9995)dx,dy,dz
 9995    format('DX= ',f8.1,'; DY= ',f8.1,'; DZ= ',f8.1)
         call edisp(icout,outs)
         write(outs,9994)dist,angl
 9994    format('DIST= ',f8.1,'; ANGL= ',f8.1)
         call edisp(icout,outs)
         call edisp(icout,' ')
         write(outs,9993)sazi1,sazi2
 9993    format('Solar azimuth of ',f8.1,' transforms to ',f6.1)
         call edisp(icout,outs)
         write(outs,9992)salt1,salt2
 9992    format('Solar altitude of ',f6.1,' transforms to ',f6.1)
         call edisp(icout,outs)
      endif
      return
      end

C ************** TRNSF3I
C Projects the given window grid point (xxw,yyw,zzw) onto the
C internal surface under consideration. The projected points are
C specified relative to the local internal surface coordinate system.
C The y-coordinate is left unprojected but in the new coordinate system.
C This routine is similar to trnsf3 but with window grid points used
C instead of obstruction block surface vertices.

      subroutine trnsf3i(ihr,intsur,ipass,xxw,yyw,zzw)

      common/tracech/icout
      common/trnsf/a(5),b(3),c(5),xor,yor,zor
      common/sangl2/sazi2,salt2
      common/proji/xp,yp,zp
      common/save3i/ta1,ta2,ca1,xmult,eta1,eta2

      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

C Jump to projection if after first entry for current source
C surface.
      if(ipass.eq.1)goto 1

      pi=4.0*atan(1.0)
      r=pi/180.

C Establish projection angles, eta1 and eta2, which related to the
C pseudo azimuth and altitude, sazi2 and salt2, respectively.
      eta1=180.0-sazi2
      eta2=salt2

C Establish trigonometric coefficients for use in the following
C projection transforms: ta1 and ta2 are -ve for +ve values
C of eta1 and eta2 respectively.
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
      if(eta2.gt.0.0)ta2=-ta2

C Perform translation then projection transformation

C Transform y (no need to project).
    1 yt=-(xxw-xor)*b(1)+(yyw-yor)*b(2)+(zzw-zor)*b(3)
      yp=yt

C ta1 and ta2 above assume that yt is +ve whereas it will be
C -ve; xmult imposes the necessary correction (N.B. switched from
C shading case because coordinate systems are different).
      xmult=1.0
      if(yt.gt.0.0)xmult=-1.0

C Transform and project x.
      xt=(xxw-xor)*a(1)+(yyw-yor)*a(2)-(xxw-xor)*a(3)
     &      +(yyw-yor)*a(4)-(zzw-zor)*a(5)
      xp=xt+yt*ta1*xmult

C Tranform and project z.
      zt=(xxw-xor)*c(1)+(yyw-yor)*c(2)+(xxw-xor)*c(3)
     &      -(yyw-yor)*c(4)+(zzw-zor)*c(5)
      zp=yt*ta2/ca1*xmult+zt

C Trace output (only for 1st grid point in source surface).
      if(tok.and.ipass.eq.0)then
         call edisp(icout,' ')
         write(outs,9999)intsur,ihr
 9999    format('*** TRNSF3I: 1st point projection to surface ',
     &          i2,' for hour ',i2,'.')
         call edisp(icout,outs)
         call edisp(icout,' ')
         call edisp(icout,'x,y,z coordinates:')
         write(outs,'(3f8.1)')xxw,yyw,zzw
         call edisp(icout,outs)
         call edisp(icout,'x`,y`,z` coordinates:')
         write(outs,'(3f8.1)')xt,yt,zt
         call edisp(icout,outs)
         write(outs,9998)eta1,eta2
 9998    format('Pseudo solar angles: eta1= ',f8.1,'; eta2= ',f8.1)
         call edisp(icout,outs)
         write(outs,9997)ta1,ta2
 9997    format('ta1= ',f8.1,'; ta2= ',f8.1)
         call edisp(icout,outs)
         call edisp(icout,'xp,yp,zp coordinates:')
         write(outs,'(3f8.1)')xp,yp,zp
         call edisp(icout,outs)
      endif
      return
      end
