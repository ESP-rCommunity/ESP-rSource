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

C TRNSF.F of ish.  This file contains the following routines:
C TRNSF1: Sets up the coefficients of the coordinate transformation
C         equations.
C TRNSF2: Expresses the solar azimuth and altitude angles relative to
C         the transformed coordinate system.
C TRNSF3: Projects the 8 corner points of an obstruction block and
C         expresses the results relative to the transformed coordinate
C         system.
C CORNR:  Returns the X & Y coords of the 4 top corners of the
C         obstruction block.
C PANGS:  Returns the azimuth, elevation and tilt angles of a surface.

C ************** TRNSF1
C Sets up the coefficients of the transform equations
C (translation + Z- then X- then Y-axis rotations) that move
C the site coordinate system to the local surface under
C consideration. The X and Z coordinates of this object body
C face are then re-expressed in terms of the new local
C coordinate system for use in POINT.

      SUBROUTINE TRNSF1(IS)
#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/PANGL/PAZI,PELV,PTILT
      COMMON/TRNSF/A(5),B(3),C(5),XOR,YOR,ZOR
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION YFT(MV)
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R = PI/180.

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
 8888      format('TRNSF1: Surface ',I2,' is probably not coplanar!')
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
      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IS,PAZI,PELV,PTILT
 9998 FORMAT('*** TRNSF1: Surface ',I2,': PAZI= ',F5.1,'; PELV= ',
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

C ************** TRNSF2 
C Convert solar angles (SAZI1 and SALT1) in original coordinate
C system to corresponding angles (SAZI2,SALT2) in transformed
C coordinate system to allow obstruction point projections
C in new coordinate system in TRNSF3.

      SUBROUTINE TRNSF2(IHR)
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRNSF/A(5),B(3),C(5),XOR,YOR,ZOR
      COMMON/SANGL1/SAZI1,SALT1
      COMMON/SANGL2/SAZI2,SALT2
      character outs*124
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
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
        Z2=5000.*TSR/SIN(ANG*R)
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
      if(ABS(DIST).LT.0.0001.or.ABS(DZ).LT.0.0001)then
         call edisp(iuout,'TRNSF2: Sun position transformation error!')
      endif

C Altitude.
      ARG=ABS(DZ)/DIST
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

C Trace output.
      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IHR
 9998 FORMAT('*** TRNSF2: Solar angle transformation at hour ',I2)
      call edisp(icout,outs)
      call edisp(icout,' ')
      WRITE(outs,9997)
 9997 FORMAT('Intermediate calculations:')
      call edisp(icout,outs)
      WRITE(outs,9996)X1,Y1,Z1,X2,Y2,Z2
 9996 FORMAT('X1,Y1,Z1=    ',3F8.1,'; X2,Y2,Z2=    ',3F8.1)
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

C ************** TRNSF3 ********************
C Projects the 4 vertices of an obstruction block surface on to the
C the plane of the target surface. The projected points are specified
C relative to the local face coordinate system. The Y-coordinate is
C left unprojected but in the new coordinate system.
      SUBROUTINE TRNSF3(IBN,IBS)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/OUTPCH/ICOUT
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      COMMON/OBS/XC(8),YC(8),ZC(8)
      COMMON/TRNSF/A(5),B(3),C(5),XOR,YOR,ZOR
      COMMON/SANGL2/SAZI2,SALT2
      COMMON/TRANSFORM/XT(4),YT(4),ZT(4)
      COMMON/PROJ/XP(4),YP(4),ZP(4)
      dimension ivorder(6,4)
      character outs*124
      logical tok

C Anticlockwise ordered vertices of the 6 obstruction block surfaces
C (surfaces read vertically).
      DATA IVORDER/5,6,7,8,1,5,
     &             6,7,8,5,2,8,
     &             2,3,4,1,3,7,
     &             1,2,3,4,4,6/

C If trace requested enable writing.
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

C Transform and project the 4 surface points in anticlockwise order. 
      DO 10 I=1,4
         IP=IVORDER(IBS,I) 

C Transform Y (no need to project since YP will either be 0 or, if
C +ve, i.e. behind the plane, then it will be reset to 0 by finding
C the plane intersection point later).
         YT(I)=-(XC(IP)-XOR)*B(1)+(YC(IP)-YOR)*B(2)+(ZC(IP)-ZOR)*B(3)
         YP(I)=0.0

C TA1 and TA2 above assume that YT(I) is +ve whereas it will be
C -ve. XMULT imposes the necessary correction. Where a block
C crosses the plane of the surface, a point with a +ve YT value
C is back projected at this point but reset in DECOMP where the
C intersection of the obstruction block with the Y'=0 plane is
C determined.
         XMULT=-1.0

C Transform and project X.
         XT(I)=(XC(IP)-XOR)*A(1)+(YC(IP)-YOR)*A(2)-(XC(IP)-XOR)*A(3)
     &         +(YC(IP)-YOR)*A(4)-(ZC(IP)-ZOR)*A(5)
         XP(I)=XT(I)+YT(I)*TA1*XMULT

C Tranform and project Z.
         ZT(I)=(XC(IP)-XOR)*C(1)+(YC(IP)-YOR)*C(2)+(XC(IP)-XOR)*C(3)
     &         -(YC(IP)-YOR)*C(4)+(ZC(IP)-ZOR)*C(5)
         ZP(I)=YT(I)*TA2/CA1*XMULT+ZT(I)
   10 CONTINUE

C Trace output.
      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      write(outs,9998)IBN,IBS
 9998 format('*** TRNSF3: projection of obstruction block ',I2,
     &   ', surface ',I2,'.')
      call edisp(icout,outs)
      call edisp(icout,'     X-Coord   Y-Coord   Z-Coord')
      call edisp(icout,'X,Y,Z coordinates:')
      do 9997 I=1,4
        IP=IVORDER(IBS,I)
        write(outs,'(I2,3F10.1)')IP,XC(IP),YC(IP),ZC(IP)
        call edisp(icout,outs)
 9997 continue
      call edisp(icout,'X`,Y`,Z` coordinates:')
      do 9996 I=1,4
        write(outs,'(I2,3F10.1)')I,XT(I),YT(I),ZT(I)
        call edisp(icout,outs)
 9996 continue
      write(outs,9995)ETA1,ETA2
 9995 format('Pseudo solar angles: ETA1= ',F5.1,'; ETA2= ',F5.1)
      call edisp(icout,outs)
      write(outs,9994)TA1,TA2
 9994 format('TA1= ',F8.1,'; TA2= ',F8.1)
      call edisp(icout,outs)
      call edisp(icout,'XP,YP,ZP coordinates:')
      DO 9993 I=1,4
        write(outs,'(I2,3F10.1)')I,XP(I),YP(I),ZP(I)
        call edisp(icout,outs)
 9993 CONTINUE

 9999 RETURN
      END

C ************** CORNR ********************
C Returns the X, Y and Z coordinates of the 8 vertices of an
C obstruction block, where vertices 1-4 are anticlockwise uppermost
C and verticed 5-8 are anticlockwise lowermost. At this stage the
C coordinates are relative to the original coordinate system.

      SUBROUTINE CORNR(IBN)
#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/GS5/NB,XO(MB),Y0(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      COMMON/OBS/XC(8),YC(8),ZC(8)
      character outs*124
      logical tok

      tok=.false.
      if(icout.eq.33)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      CA=COS(ANG(IBN)*R)
      SA=SIN(ANG(IBN)*R)
      XC(1)=XO(IBN)
      YC(1)=Y0(IBN)
      ZC(1)=ZO(IBN)+DZ(IBN)
      XC(5)=XC(1)
      YC(5)=YC(1)
      ZC(5)=ZO(IBN)
      XC(2)=XC(1)+DX(IBN)*CA
      YC(2)=YC(1)+DX(IBN)*SA
      ZC(2)=ZC(1)
      XC(6)=XC(2)
      YC(6)=YC(2)
      ZC(6)=ZC(5)
      XC(3)=XC(2)-DY(IBN)*SA
      YC(3)=YC(2)+DY(IBN)*CA
      ZC(3)=ZC(1)
      XC(7)=XC(3)
      YC(7)=YC(3)
      ZC(7)=ZC(5)
      XC(4)=XC(1)-DY(IBN)*SA
      YC(4)=YC(1)+DY(IBN)*CA
      ZC(4)=ZC(1)
      XC(8)=XC(4)
      YC(8)=YC(4)
      ZC(8)=ZC(5)

      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      write(outs,'(A,I2,A)')'*** CORNR: Block ',IBN,
     &                      ' site coordinates (XC,YC,ZC)'
      call edisp(icout,outs)
      call edisp(icout,'     X-Coord   Y-Coord   Z-Coord')
      do 9998 I=1,8
      write(outs,'(I2,3F10.1)')I,XC(I),YC(I),ZC(I)
      call edisp(icout,outs)
 9998 continue

 9999 RETURN
      END

C ******************* PANGS **************************
C PANGS computes the object body surface azimuth (PAZI), elevation
C (PELV) and local X-axis tilt (PTILT) where:

C   PAZI  - is the angle measured from north to the face normal,
C           clockwise positive.
C   PELV  - is the angle measured from the horizontal to the
C           face normal (upward facing horizontal surface 90 deg.,
C           downward facing horizontal surface -90 deg., 
C           vertical surface 0 deg.
C   PTILT - is the angle between the face X-axis and the horizontal
C           where the X-axis is the line joining the 1st and 2nd
C           face vertices.

      SUBROUTINE PANGS(IS)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/PANGL/PAZI,PELV,PTILT

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      XSUM=0.
      YSUM=0.
      ZSUM=0.

C Compute azimuth and elevation angles for face 'IS'.
      NV=NVER(IS)
      DO 10 I=1,NV
        K=I+1
        IF(I.EQ.NV)K=1
        IP1=JVN(IS,I)
        IP2=JVN(IS,K)
        XSUM=XSUM+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
        YSUM=YSUM+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
        ZSUM=ZSUM+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
   10 CONTINUE
      PAZI=90.0
      if(abs(xsum).lt.0.00001)pazi=0.0
      IF(XSUM.LT.0.0)PAZI=-90.0
      PELV=90.0
      if(abs(zsum).lt.0.00001)pelv=0.0
      IF(ZSUM.LT.0.0)PELV=-90.0
      if(abs(ysum).lt.0.00001)goto 11
      PAZI=ATAN2(XSUM,YSUM)/R
   11 SRX2Y2=SQRT(XSUM*XSUM+YSUM*YSUM)
      IF(PAZI.LT.0.)PAZI=PAZI+360.
      if(abs(srx2y2).lt.0.00001)goto 12
      PELV=ATAN2(ZSUM,SRX2Y2)/R
   12 CONTINUE

C Now compute local X-axis tilt angle.
      IV1=JVN(IS,1)
      IV2=JVN(IS,2)
      dz=z(iv2)-z(iv1)
      DIST=((X(IV2)-X(IV1))**2+(Y(IV2)-Y(IV1))**2+
     &(Z(IV2)-Z(IV1))**2)**.5
      ptilt=asin(dz/dist)/r
      if(z(iv1).gt.z(iv2))ptilt=-ptilt

C For the special case of a horizontal surface, the
C azimuth calculated above is zero. It is now reset to
C the azimuth of the plane comprising the first two
C vertices of the face and the same two vertices but with
C an adjusted Z value.
      XSUM=0.
      YSUM=0.
      ZSUM=0.
      DO 20 I=1,4
        IF(I.EQ.1)THEN
           IP1=JVN(IS,1)
           IP2=JVN(IS,2)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)
           ZZ2=Z(IP2)
        ELSEIF(I.EQ.2)THEN
           IP1=JVN(IS,2)
           IP2=JVN(IS,2)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)
           ZZ2=Z(IP2)+0.1
        ELSEIF(I.EQ.3)THEN
           IP1=JVN(IS,2)
           IP2=JVN(IS,1)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)+0.1
           ZZ2=Z(IP2)+0.1
        ELSEIF(I.EQ.4)THEN
           IP1=JVN(IS,1)
           IP2=JVN(IS,1)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)+0.1
           ZZ2=Z(IP2)
        ENDIF

        XSUM=XSUM+YY1*ZZ2-ZZ1*YY2
        YSUM=YSUM+ZZ1*XX2-XX1*ZZ2
        ZSUM=ZSUM+XX1*YY2-YY1*XX2
   20 CONTINUE
      PAZI=90.0
      if(abs(xsum).lt.0.00001)pazi=0.0
      IF(XSUM.LT.0.0)PAZI=-90.0
      if(abs(ysum).lt.0.00001)goto 13
      PAZI=ATAN2(XSUM,YSUM)/R
   13 IF(PAZI.LT.0.)PAZI=PAZI+360.

      RETURN
      END
