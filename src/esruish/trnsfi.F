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

C TRNSFI.F of ish.  This file contains the following routines:
C TRNSF1I: Sets up the coefficients of the coordinate transformation
C          equations.
C TRNSF2I: Expresses the solar azimuth and altitude angles relative to
C          the transformed coordinate system.
C TRNSF3I: Projects a window vertex and expresses the result relative
C          to the transformed coordinate system.

C ************** TRNSF1I
C Sets up the coefficients of the transform equations
C (translation + Z- then X- then Y-axis rotations) that move
C the site coordinate system to the local surface under
C consideration. The X and Z coordinates of this object body
C face are then re-expressed in terms of the new local
C coordinate system for use in POINT.

C This routine is essentially a copy of TRNSF1.

      SUBROUTINE TRNSF1I(IS)
#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/TC/ITC,ICNT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PANGL/PAZI,PELV,PTILT
      COMMON/TRNSF/A(5),B(3),C(5),XOR,YOR,ZOR
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION YFT(MV)
      character outs*124
      logical tok

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

C Compute the surface azimuth (PAZI), elevation (PELV)
C X-axis tilt (PTILT) angles.
      CALL PANGS(IS)

C Convert PAZI, PELV and PTILT to the required coordinate axis
C rotations as follows.
C    X-axis - BE related to PELV
C    Y-axis - GA related to PTILT
C    Z-axis - AL related to PAZI
C Convention: positive angle is anticlockwise when looking along
C the axis towards the origin.
      IQ=INT(PAZI/90.0)
      IF(IQ.EQ.0) goto 1
      goto (1,1,2,2),IQ
    1 AL=(180.0-PAZI)*R
      goto 3
    2 AL=-(PAZI-180.0)*R
    3 BE=-PELV*R
      GA=-PTILT*R

C Set up transform equation coefficients.
C Transformation order: Z-axis (AL from PAZI) then
C X-axis (BE from PELV) then Y-axis (GA from PTILT)
C rotation.
      A(1)=COS(AL)*COS(GA)
      A(2)=SIN(AL)*COS(GA)
      A(3)=SIN(AL)*SIN(BE)*SIN(GA)
      A(4)=COS(AL)*SIN(BE)*SIN(GA)
      A(5)=COS(BE)*SIN(GA)
      B(1)=SIN(AL)*COS(BE)
      B(2)=COS(AL)*COS(BE)
      B(3)=SIN(BE)
      C(1)=COS(AL)*SIN(GA)
      C(2)=SIN(AL)*SIN(GA)
      C(3)=SIN(AL)*SIN(BE)*COS(GA)
      C(4)=COS(AL)*SIN(BE)*COS(GA)
      C(5)=COS(BE)*COS(GA)
      IP1=JVN(IS,1)
      XOR=X(IP1)
      YOR=Y(IP1)
      ZOR=Z(IP1)

C Transform target surface coordinates.
      DO 10 I=1,NVER(IS)
        XS=X(JVN(IS,I))
        YS=Y(JVN(IS,I))
        ZS=Z(JVN(IS,I))

C Transformed X.
        XFT(I)=(XS-XOR)*A(1)+(YS-YOR)*A(2)-(XS-XOR)*A(3)
     &         +(YS-YOR)*A(4)-(ZS-ZOR)*A(5)

C Transformed Y.
        YFT(I)=-(XS-XOR)*B(1)+(YS-YOR)*B(2)+(ZS-ZOR)*B(3)

C Transformed Z.
        ZFT(I)=(XS-XOR)*C(1)+(YS-YOR)*C(2)+(XS-XOR)*C(3)
     &         -(YS-YOR)*C(4)+(ZS-ZOR)*C(5)

C Transformed Y value should be zero; error if not.
        if(abs(YFT(I)).gt.1.E-5)then
           call edisp(icout,' ')
           write(outs,8888)IS
 8888      format('TRNSF1I: Surface ',I2,' is probably not coplanar!')
           call edisp(icout,outs)
           write(outs,8887)I,XS,YS,ZS
 8887      format('Vertex ',I2,', which has coordinates ',3F11.6,',')
           call edisp(icout,outs)
           write(outs,8886)YFT(I)
 8886      format('has transformed Y value of ',F11.6,
     &            ' (should be 0).')
           call edisp(icout,outs)
        endif

   10 CONTINUE

C Trace output.
      IF(.not.tok)goto  9999
      call edisp(icout,' ')
      WRITE(outs,9998)IS,PAZI,PELV,PTILT
 9998 FORMAT('*** TRNSF1I: Surface ',I2,': PAZI= ',F5.1,'; PELV= ',
     &       F5.1,'; PTILT= ',F5.1)
      call edisp(icout,outs)
      call edisp(icout,' ')
      call edisp(icout,'Axis translations & rotations:')
      WRITE(outs,'(A,2X,3F8.1)')'X, Y & Z translations -',XOR,YOR,ZOR
      call edisp(icout,outs)
      WRITE(outs,'(A,2X,3F8.1)')'X (BE), Y (GA) & Z (AL) rotations -',
     &            BE/R,GA/R,AL/R
      call edisp(icout,outs)
      call edisp(icout,'ZXY transform equation coefficients:')
      WRITE(outs,'(A,2X,5F8.1)')'A -',(A(I),I=1,5)
      call edisp(icout,outs)
      WRITE(outs,'(A,2X,5F8.1)')'B -',(B(I),I=1,3)
      call edisp(icout,outs)
      WRITE(outs,'(A,2X,5F8.1)')'C -',(C(I),I=1,5)
      call edisp(icout,outs)
      call edisp(icout,'Original XYZ coordinates:')
      do 9995 I=1,NVER(IS)
        XS=X(JVN(IS,I))
        YS=Y(JVN(IS,I))
        ZS=Z(JVN(IS,I))
        WRITE(outs,'(I2,2X,3F8.1)')I,XS,YS,ZS
        call edisp(icout,outs)
 9995 continue
      call edisp(icout,'Transformed X`Y`Z` coordinates:')
      do 9994 I=1,NVER(IS)
        WRITE(outs,'(I2,2X,3F8.1)')I,XFT(I),YFT(I),ZFT(I)
        call edisp(icout,outs)
 9994 continue

 9999 RETURN
      END

C ************** TRNSF2I 
C Convert solar angles (SAZI1 and SALT1) in original coordinate
C system to corresponding angles (SAZI2,SALT2) in transformed
C coordinate system to allow obstruction point projections
C in new coordinate system in TRNSF3I.

C This routine is essentially a copy of TRNSF2.

      SUBROUTINE TRNSF2I(IHR)
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRNSF/A(5),B(3),C(5),XOR,YOR,ZOR
      COMMON/SANGL1/SAZI1,SALT1
      COMMON/SANGL2/SAZI2,SALT2
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R = PI/180.
      TSR=SIN(SALT1*R)/COS(SALT1*R)

C Establish a line from the site origin to a point 'far'
C away at the appropriate solar angles (SAZI1 and SALT1).
      X1=0.
      Y1=0.
      Z1=0.

C Dependent on which quadrant sun is in, convert sun position
C to a distant point (X2,Y2,Z2) for later transformation to the
C new coordinate system, along with point (X1,Y1,Z1), to give
C the solar incindence line in this new system.
      IF(SAZI1.LT.90.)THEN
        ANG=90.-SAZI1
        TANG=SIN(ANG*R)/COS(ANG*R)
        Y2=5000.
        Z2=5000.*TSR/SIN(ANG*R)
        X2=5000./TANG
      ELSEIF(SAZI1.GT.90..AND.SAZI1.LT.180.)THEN
        ANG=SAZI1-90.
        TANG=SIN(ANG*R)/COS(ANG*R)
        Y2=-5000.
        Z2=5000.*TSR/SIN(ANG*R)
        X2=5000./TANG
      ELSEIF(SAZI1.GT.180..AND.SAZI1.LT.270.)THEN
        ANG=SAZI1-180.
        TANG=SIN(ANG*R)/COS(ANG*R)
        Y2=-5000.
        Z2=5000.*TSR/COS(ANG*R)
        X2=-5000.*TANG
      ELSEIF(SAZI1.GT.270.)THEN
        ANG=SAZI1-270.
        TANG=SIN(ANG*R)/COS(ANG*R)
        Y2=5000.
        Z2=5000.*TSR/COS(ANG*R)
        X2=-5000./TANG
      ENDIF

      IF(SAZI1.LT.90.001.AND.SAZI1.GT.89.999)THEN
        Y2=0.
        X2=5000.
        Z2=5000.*TSR
      ELSEIF(SAZI1.LT.180.001.AND.SAZI1.GT.179.999)THEN
        Y2=-5000.
        X2=0.
        Z2=5000.*TSR
      ELSEIF(SAZI1.LT.270.001.AND.SAZI1.GT.269.999)THEN
        X2=-5000.
        Y2=0.
        Z2=5000.*TSR
      ENDIF

C Translate/rotate both points (in order Z-, X- then
C Y-axis rotation) to new coordinate system.

C Transformed X.
      XT1=(X1-XOR)*A(1)+(Y1-YOR)*A(2)-(X1-XOR)*A(3)+
     &         (Y1-YOR)*A(4)-(Z1-ZOR)*A(5)
      XT2=(X2-XOR)*A(1)+(Y2-YOR)*A(2)-(X2-XOR)*A(3)+
     &         (Y2-YOR)*A(4)-(Z2-ZOR)*A(5)

C Transformed Y.
      YT1=-(X1-XOR)*B(1)+(Y1-YOR)*B(2)+(Z1-ZOR)*B(3)
      YT2=-(X2-XOR)*B(1)+(Y2-YOR)*B(2)+(Z2-ZOR)*B(3)

C Transformed Z.
      ZT1=(X1-XOR)*C(1)+(Y1-YOR)*C(2)+(X1-XOR)*C(3)-
     &         (Y1-YOR)*C(4)+(Z1-ZOR)*C(5)
      ZT2=(X2-XOR)*C(1)+(Y2-YOR)*C(2)+(X2-XOR)*C(3)-
     &         (Y2-YOR)*C(4)+(Z2-ZOR)*C(5)

C Convert transformed points back to pseudo solar angles
C (SAZI2 and SALT2) expressed relative to the transformed
C coordinate system.
      DIST=SQRT((XT2-XT1)**2+(YT2-YT1)**2+(ZT2-ZT1)**2)
      DZ=ZT2-ZT1
      if((DIST.LT..0001.AND.DIST.GT.-.0001).or.
     &   (DZ.LT..0001.AND.DZ.GT.-.0001))then
         call edisp(iuout,'TRNSF2: Sun position transformation error!')
      endif

C Altitude.
      ARG=DZ/DIST
      SALT2=ASIN(ARG)/R
      if(DZ.lt.0.)SALT2=-SALT2

C Azimuth.
      DX=XT2-XT1
      DY=YT2-YT1
      ARG=ABS(DY/DX)
      ANGL=ATAN(ARG)/R
      IF(DX.GT.0..AND.DY.LT.0.)THEN
        SAZI2=90.+ANGL
      ELSEIF(DX.GT.0..AND.DY.GT.0.)THEN
        SAZI2=90.-ANGL
      ELSEIF(DX.LT.0..AND.DY.GT.0.)THEN
        SAZI2=270.+ANGL
      ELSEIF(DX.LT.0..AND.DY.LT.0.)THEN
        SAZI2=270.-ANGL
      ENDIF

C Special cases:
C 1. When transformed solar azimuth is normal to the
C transformed surface.
      IF(DX.LT..001.AND.DX.GT.-.001)THEN
        SAZI2=0.

C 2. When surface is facing away from the transformed sun.
        IF(DY.LT.0.)SAZI2=180.

C 3. When the transformed solar azimuth is parallel to
C the transformed surface. 
      ELSEIF(DY.LT..001.AND.DY.GT.-.001)THEN
        SAZI2=90.
        IF(DX.LT.0.)SAZI2=270.
      ENDIF

C C Trace output.
      IF(.not.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IHR
 9998 FORMAT('*** TRNSF2I: Solar angle transformation at hour ',I2)
      call edisp(icout,outs)
      call edisp(icout,' ')
      WRITE(outs,9997)
 9997 FORMAT('Intermediate calculations:')
      call edisp(icout,outs)
      WRITE(outs,9996)X1,Y1,Z1,X2,Y2,Z2
 9996 FORMAT('X1,Y1,Z1= ',3F8.1,'; X2,Y2,Z2= ',3F8.1)
      call edisp(icout,outs)
      WRITE(outs,9995)XT1,YT1,ZT1,XT2,YT2,ZT2
 9995 FORMAT('XT1,YT1,ZT1= ',3F8.1,'; XT2,YT2,ZT2= ',3F8.1)
      call edisp(icout,outs)
      WRITE(outs,9994)DX,DY,DZ
 9994 FORMAT('DX= ',F8.1,'; DY= ',F8.1,'; DZ= ',F8.1)
      call edisp(icout,outs)
      WRITE(outs,9993)DIST,ANGL
 9993 FORMAT('DIST= ',F8.1,'; ANGL= ',F8.1)
      call edisp(icout,outs)
      call edisp(icout,' ')
      WRITE(outs,9992)SAZI1,SAZI2
 9992 FORMAT('Solar azimuth of ',F8.1,' transforms to ',F6.1)
      call edisp(icout,outs)
      WRITE(outs,9991)SALT1,SALT2
 9991 FORMAT('Solar altitude of ',F6.1,' transforms to ',F6.1)
      call edisp(icout,outs)

 9999 RETURN
      END

C ************** TRNSF3I
C Projects the given window vertex (XXW,YYW,ZZW) onto the
C plane of the object body face under consideration. The projected
C are specified relative to the local face coordinate system.  The
C Y-coordinate is left unprojected but in the new coordinate system.

C This reoutine is similar to TRNSF3 but with window grid points
C instead of obstruction surface vertices.

      SUBROUTINE TRNSF3I(XXW,YYW,ZZW)

      COMMON/TC/ITC,ICNT
      COMMON/OUTPCH/ICOUT
      COMMON/TRNSF/A(5),B(3),C(5),XOR,YOR,ZOR
      COMMON/SANGL2/SAZI2,SALT2
      COMMON/PROJI/XP,YP,ZP
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Establish projection angles, ETA1 and ETA2, which related to the
C pseudo azimuth, SAZI2, and pseudo altitude, SALT2, respectively.
      ETA1=180-SAZI2
      ETA2=SALT2

C Establish trigonometric coefficients for use in the following
C projection transforms: TA1 and TA2 are -ve for +ve values
C of ETA1 and ETA2 respectively.
      A1=ABS(ETA1*R)
      SA1=SIN(A1)
      CA1=COS(A1)
      IF(ABS(CA1).LT.0.00001)CA1=0.00001
      TA1=SA1/CA1
      IF(ETA1.gt.0.0)TA1=-TA1

      A2=ABS(ETA2*R)
      SA2=SIN(A2)
      CA2=COS(A2)
      IF(ABS(CA2).LT..00001)CA2=.00001
      TA2=SA2/CA2
      IF(ETA2.gt.0.0)TA2=-TA2

C Perform translation then projection transformation

C Transform Y (no need to project).
      YT=-(XXW-XOR)*B(1)+(YYW-YOR)*B(2)+(ZZW-ZOR)*B(3)
      YP=YT

C TA1 and TA2 above assume that YT is +ve whereas it will be
C -ve. XMULT imposes the necessary correction (N.B. switched from
C shading case because coordinate systems are different.
      XMULT=1.0
      IF(YT.gt.0.0)XMULT=-1.0

C Transform and project X.
      XT=(XXW-XOR)*A(1)+(YYW-YOR)*A(2)-(XXW-XOR)*A(3)
     &      +(YYW-YOR)*A(4)-(ZZW-ZOR)*A(5)
      XP=XT+YT*TA1*XMULT

C Tranform and project Z.
      ZT=(XXW-XOR)*C(1)+(YYW-YOR)*C(2)+(XXW-XOR)*C(3)
     &      -(YYW-YOR)*C(4)+(ZZW-ZOR)*C(5)
      ZP=YT*TA2/CA1*XMULT+ZT

C Trace output.
      IF(.not.tok)goto 9999
      call edisp(icout,' ')
      write(outs,9998)
 9998 format('*** TRNSF3I: point projection.')
      call edisp(icout,outs)
      call edisp(icout,' ')
      call edisp(icout,'X,Y,Z coordinates:')
      write(outs,'(3F8.1)')XXW,YYW,ZZW
      call edisp(icout,outs)
      call edisp(icout,'X`,Y`,Z` coordinates:')
      write(outs,'(3F8.1)')XT,YT,ZT
      call edisp(icout,outs)
      write(outs,9997)ETA1,ETA2
 9997 format('Pseudo solar angles: ETA1= ',F8.1,'; ETA2= ',F8.1)
      call edisp(icout,outs)
      write(outs,9996)TA1,TA2
 9996 format('TA1= ',F8.1,'; TA2= ',F8.1)
      call edisp(icout,outs)
      call edisp(icout,'XP,YP,ZP coordinates:')
      write(outs,'(3F8.1)')XP,YP,ZP
      call edisp(icout,outs)

 9999 RETURN
      END
