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

C TRNSF.F of ESRUSHD.  This file contains the following routines:
C TRNSF1: Sets of the coefficients of the transformation equations.
C TRNSF2: Original site coordinate system > local face system.
C TRNSF4
C TRNSF5
C TRNSF3: Project the 8 corner point of the specified obstruction block.
C CORNR:  Returns the X & Y coords of the 4 top corners of the
C         obstruction block.
C PANGS:  Returns the azimuth, elevation and tilt of a surface.

C ************** TRNSF1
C Sets up the coefficients of the transformation equations
C that translate and rotate the site coordinate system to the 
C local surface under consideration.
C The X and Z coordinates of this object body face are then
C re-expressed in terms of the new local coordiante system
C for use in POINT.
      SUBROUTINE TRNSF1(IS)
#include "building.h"
      COMMON/OUTPCH/ICOUT
      COMMON/TC/ITC,ICNT
      COMMON/PANGL/PAZI,PELV,PTILT
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33.and.itc.gt.1)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Compute the surface azimuth, elevation, and X-axis tilt angles.
      CALL PANGS(IS)

C Debug...
      if(tok)write(icout,*) 'for surface (shd) ',is,' pazi=',pazi,
     &  ' pelev=',pelv,' ptilt=',ptilt

C Compute the transformation coefficients.
      IQ=INT(PAZI/90.)
      IF(IQ.EQ.0)goto  1
      goto (1,1,2,2),IQ
    1 AL=(180.-PAZI)*R
      goto 3
    2 AL=-(PAZI-180.)*R
    3 BE=-PELV*R
      GA=PTILT*R

C In the case of a horizontal surface reset GA to use -AL
C and reset AL to zero.
      if((PELV.gt.89.9.and.PELV.lt.90.1).or.
     &   (PELV.lt.-89.9.and.PELV.gt.-90.1))then
        GA=-AL
        AL=0.0
C Debug...
        if(tok)write(icout,*) 'AL BE GA = ',AL/R,BE/R,GA/R
      endif

      A(1)=COS(GA)*COS(AL)
      A(2)=SIN(BE)*SIN(GA)*COS(AL)
      A(3)=COS(BE)*SIN(GA)*COS(AL)
      A(4)=COS(BE)*SIN(AL)
      A(5)=SIN(BE)*SIN(AL)
      B(1)=COS(GA)*SIN(AL)
      B(2)=SIN(BE)*SIN(GA)*SIN(AL)
      B(3)=COS(BE)*SIN(GA)*SIN(AL)
      B(4)=COS(BE)*COS(AL)
      B(5)=SIN(BE)*COS(AL)
      C(1)=SIN(GA)
      C(2)=SIN(BE)*COS(GA)
      C(3)=COS(BE)*COS(GA)
      IP1=JVN(IS,1)
      XOR=X(IP1)
      YOR=Y(IP1)
      ZOR=Z(IP1)

C Transform object body face coordinates.
      DO 10 I=1,NVER(IS)
        XX(I)=X(JVN(IS,I))
        YY(I)=Y(JVN(IS,I))
        ZZ(I)=Z(JVN(IS,I))

C Transformed X.
        XFT(I)=(XX(I)-XOR)*A(1)+(YY(I)-YOR)*A(2)-(ZZ(I)-ZOR)*A(3)+
     &         (YY(I)-YOR)*A(4)+(ZZ(I)-ZOR)*A(5)

C Transformed Z.
        ZFT(I)=(XX(I)-XOR)*C(1)-(YY(I)-YOR)*C(2)+(ZZ(I)-ZOR)*C(3)
   10 CONTINUE

C TRACE OUTPUT (if verbose level)
      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IS
 9998 FORMAT(' TRNSF1: Transformation equation details for surface ',
     &       I2,' with an')
      call edisp(icout,outs)

      WRITE(outs,9997)PAZI,PELV,PTILT
 9997 FORMAT(' azimuth of ',F7.3,' degrees, an elevation of ',
     &       F7.3,' & a tilt of ',F6.3,' degrees.')
      call edisp(icout,outs)
      call edisp(icout,' ')
      call edisp(icout,' The translation/rotation coefficients follow:')
      WRITE(outs,'(A,3X,5F8.2)')' A(1) TO A(5)',(A(I),I=1,5)
      call edisp(icout,outs)
      WRITE(outs,'(A,3X,5F8.2)')' B(1) TO B(5)',(B(I),I=1,5)
      call edisp(icout,outs)
      WRITE(outs,'(A,3X,5F8.2)')' C(1) TO C(3)',(C(I),I=1,3)
      call edisp(icout,outs)

      NV=NVER(IS)
      call edisp(icout,' Translated/rotated zone face coordinates: ')
      do 9995 I=1,NV
        WRITE(outs,'(2F10.4)')XFT(I),ZFT(I)
        call edisp(icout,outs)
 9995 continue

 9999 RETURN
      END

C ************** TRNSF2 
C Transform original site coordinate system to new angles
C specified relative to the local face coordinate system.
      SUBROUTINE TRNSF2(IHOUR)
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/SANGL1/SAZI1,SALT1
      COMMON/SANGL2/SAZI2,SALT2
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      TSR=SIN(SALT1*R)/COS(SALT1*R)

C Set origin coordinates.
      X1=0.
      Y1=0.
      Z1=0.

C Which quadrant is the sun within?
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
C        Z2=5000.*TSR/COS(ANG*R)
        Z2=5000.*TSR/SIN(ANG*R)
        X2=-5000./TANG
      ELSEIF(SAZI1.LT.90.001.AND.SAZI1.GT.89.999)THEN
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

C Now translate and rotate both points to new coordinate system.
C Transformed X.
      XT1=(X1-XOR)*A(1)+(Y1-YOR)*A(2)-(Z1-ZOR)*A(3)+(Y1-YOR)*
     &     A(4)+(Z1-ZOR)*A(5)
      XT2=(X2-XOR)*A(1)+(Y2-YOR)*A(2)-(Z2-ZOR)*A(3)+(Y2-YOR)*
     &     A(4)+(Z2-ZOR)*A(5)

C Transformed Y.
      YT1=-(X1-XOR)*B(1)-(Y1-YOR)*B(2)+(Z1-ZOR)*B(3)+(Y1-YOR)*
     &    B(4)+(Z1-ZOR)*B(5)
      YT2=-(X2-XOR)*B(1)-(Y2-YOR)*B(2)+(Z2-ZOR)*B(3)+(Y2-YOR)*
     &    B(4)+(Z2-ZOR)*B(5)

C Transformed Z.
      ZT1=(X1-XOR)*C(1)-(Y1-YOR)*C(2)+(Z1-ZOR)*C(3)
      ZT2=(X2-XOR)*C(1)-(Y2-YOR)*C(2)+(Z2-ZOR)*C(3)

C Convert transformed points back to solar angles
C relative to local coordinate system.
      DIST=SQRT((XT2-XT1)**2+(YT2-YT1)**2+(ZT2-ZT1)**2)
      DZ=ZT2-ZT1
      IF(DIST.LT..0001.AND.DIST.GT.-.0001)goto 16
      IF(DZ.LT..0001.AND.DZ.GT.-.0001)goto 16
      ARG=DZ/DIST
      SALT2=ASIN(ARG)/R
      DX=XT2-XT1
      DY=YT2-YT1
      IF(DX.LT..001.AND.DX.GT.-.001)THEN
        SAZI2=0.
        IF(DY.LT.0.)SAZI2=180.
      ELSEIF(DY.LT..001.AND.DY.GT.-.001)THEN
        SAZI2=90.
        IF(DX.LT.0.)SAZI2=270.
      ENDIF
      ARG=ABS(DY/DX)
      ANGL=ATAN(ARG)/R
      IF(DX.GT.0..AND.DY.GT.0.)THEN
        SAZI2=90.-ANGL
      ELSEIF(DX.LT.0..AND.DY.GT.0.)THEN
        SAZI2=270.+ANGL
      ELSEIF(DX.LT.0..AND.DY.LT.0.)THEN
        SAZI2=270.-ANGL
      ELSEIF(DX.GT.0..AND.DY.LT.0.)THEN
        SAZI2=90.+ANGL
      ENDIF
      goto 17
   16 call edisp(iuout,' Solar transformation error !')

C Trace (if verbose level)?
   17 IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IHOUR
 9998 FORMAT(' TRNSF2: Transformation details @ hour ',I3)
      call edisp(icout,outs)
      WRITE(outs,9997)SAZI1,SAZI2
 9997 FORMAT(' The azimuth ',F8.2,' transforms to ',F8.2,' and the')
      call edisp(icout,outs)
      WRITE(outs,9996)SALT1,SALT2
 9996 FORMAT('   elevation ',F8.2,' transforms to ',F8.2,'.')
      call edisp(icout,outs)
 9999 RETURN
      END

C ************** TRNSF3 ********************
C 'TRNSF3' projects the 8 corner point of the specified obstruction
C block onto the plane of the object body face under consideration.
C The projected points are specified relative to the local face
C coordinate system.  The Y-coordinate is left unprojected but in
C the new coordinate system.
      SUBROUTINE TRNSF3(IB)
#include "building.h"
      COMMON/TC/ITC,ICNT
      COMMON/OUTPCH/ICOUT
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/SANGL2/SAZI2,SALT2
      COMMON/PROJ/XP(8),YP(8),ZP(8)
      DIMENSION XC(8),YC(8)
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33.and.itc.gt.1)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C First compute X and Y coordinates (relative to original site
C coordinate system) of 8 corners of obstruction block.
      CALL CORNR(XO(IB),YO(IB),DX(IB),DY(IB),ANG(IB),XC,YC)

C Perform projection transformation.
      A1=SAZI2*R
      A2=SALT2*R
      SA1=SIN(A1)
      CA1=COS(A1)
      IF(ABS(CA1).LT..0001)CA1=.0001
      S=SIN(A2)
      CC=COS(A2)
      IF(ABS(CC).LT..0001)CC=.0001
      TA2=S/CC

C Projected points 1 to 4 relate to the 4 top points of
C the obstruction block, and points 5 to 8 relate to the
C bottom 4 points. They are ordered anticlockwise from above. 
      DO 10 I=1,8
      ZC=ZO(IB)
      IF(I.LE.4)ZC=ZO(IB)+DZ(IB)

C Projected X.
      XP(I)=(XC(I)-XOR)*A(1)+(YC(I)-YOR)*A(2)-
     &      (ZC-ZOR)*A(3)+(YC(I)-YOR)*A(4)+(ZC-ZOR)*A(5)+
     &      (XC(I)-XOR)*B(1)*SA1/CA1+(YC(I)-YOR)*B(2)*SA1/CA1-
     &      (ZC-ZOR)*B(3)*SA1/CA1-(YC(I)-YOR)*B(4)*SA1/CA1-
     &      (ZC-ZOR)*B(5)*SA1/CA1

C Projected Y.
      YP(I)=-(XC(I)-XOR)*B(1)-(YC(I)-YOR)*B(2)+
     &       (ZC-ZOR)*B(3)+(YC(I)-YOR)*B(4)+(ZC-ZOR)*B(5)

C Projected Z.
      ZP(I)=(XC(I)-XOR)*B(1)*TA2/CA1+(YC(I)-YOR)*B(2)*TA2/CA1-
     &      (ZC-ZOR)*B(3)*TA2/CA1-(YC(I)-YOR)*B(4)*TA2/CA1-
     &      (ZC-ZOR)*B(5)*TA2/CA1+(XC(I)-XOR)*C(1)-
     &      (YC(I)-YOR)*C(2)+(ZC-ZOR)*C(3)
   10 CONTINUE

C Trace?
      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IB
 9998 FORMAT(' TRNSF3: The projected point coordinates for block ',I2,
     &       ' follow:')
      call edisp(icout,outs)

      call edisp(icout,
     &  ' Points 1-4 Top; 5-8 Bottom (anticlockwise from top)) ')
      call edisp(icout,' Point|X-coord|Y-coord|Z-coord ')
      DO 9996 I=1,8
        WRITE(outs,'(I4,3F8.1)')I,XP(I),YP(I),ZP(I)
        call edisp(icout,outs)
 9996 CONTINUE
 9999 RETURN
      END

C *****************************************************************
      SUBROUTINE TRNSF4(IS)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/GR3D09/XP(3),YP(3),ZP(3)

      DIMENSION  XX(MV),YY(MV),ZZ(MV)

      NVRIS=NVER(IS)
      IP1=JVN(IS,1)
      XOR=X(IP1)
      YOR=Y(IP1)
      ZOR=Z(IP1)

C Transform object body face coordinates.
      DO 10 I=1,NVRIS
        IP=JVN(IS,I)
        XX(I)=X(IP)-XOR
        YY(I)=Y(IP)-YOR
        ZZ(I)=Z(IP)-ZOR
   10 CONTINUE

C Define the local X,Y,and Z axes for the current surface.
      CALL TRNSF5(NVRIS,XX,YY,ZZ)

C Perform rotation.
      XMIN=0.
      ZMIN=0.
      DO 20 I=1,NVRIS
        XFT(I)=XX(I)*XP(1)+YY(I)*XP(2)+ZZ(I)*XP(3)
        ZFT(I)=XX(I)*ZP(1)+YY(I)*ZP(2)+ZZ(I)*ZP(3)
        XMIN=AMIN1(XMIN,XFT(I))
        ZMIN=AMIN1(ZMIN,ZFT(I))
   20 CONTINUE
      DO 30 I=1,NVRIS
        XFT(I)=XFT(I)-XMIN
        ZFT(I)=ZFT(I)-ZMIN
   30 CONTINUE
      RETURN
      END

C *****************************************************************
      SUBROUTINE TRNSF5(NVRIS,XX,YY,ZZ)
#include "building.h"

      COMMON/GR3D09/XP(3),YP(3),ZP(3)

      DIMENSION  XX(MV),YY(MV),ZZ(MV)

C Define the two vectors defining the current surface.
      XP(1)=XX(2)
      XP(2)=YY(2)
      XP(3)=ZZ(2)
      DIST=SQRT(XP(1)**2+XP(2)**2+XP(3)**2)
      XP(1)=XP(1)/DIST
      XP(2)=XP(2)/DIST
      XP(3)=XP(3)/DIST
      ZP(1)=XX(NVRIS)
      ZP(2)=YY(NVRIS)
      ZP(3)=ZZ(NVRIS)
      DIST=SQRT(ZP(1)**2+ZP(2)**2+ZP(3)**2)
      ZP(1)=ZP(1)/DIST
      ZP(2)=ZP(2)/DIST
      ZP(3)=ZP(3)/DIST
      CALL CROSS(XP,ZP,YP)
      DIST=SQRT(YP(1)**2+YP(2)**2+YP(3)**2)
      YP(1)=YP(1)/DIST
      YP(2)=YP(2)/DIST
      YP(3)=YP(3)/DIST
      CALL CROSS(YP,XP,ZP)
      DIST=SQRT(ZP(1)**2+ZP(2)**2+ZP(3)**2)
      ZP(1)=ZP(1)/DIST
      ZP(2)=ZP(2)/DIST
      ZP(3)=ZP(3)/DIST
      RETURN
      END

C ************** CORNR ********************
C 'CORNR' returns the X and Y coordinates of the 4 top corners
C of the specified obstruction block.
      SUBROUTINE CORNR(XX,YY,DXX,DYY,ORIEN,XC,YC)
      COMMON/OUTPCH/ICOUT
      COMMON/TC/ITC,ICNT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      DIMENSION XC(8),YC(8)
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33.and.itc.gt.1)tok=.true.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      CA=COS(ORIEN*R)
      SA=SIN(ORIEN*R)
      XC(1)=XX
      YC(1)=YY
      XC(5)=XC(1)
      YC(5)=YC(1)
      XC(2)=XX+DXX*CA
      YC(2)=YY+DXX*SA
      XC(6)=XC(2)
      YC(6)=YC(2)
      XC(3)=XC(2)-DYY*SA
      YC(3)=YC(2)+DYY*CA
      XC(7)=XC(3)
      YC(7)=YC(3)
      XC(4)=XX-DYY*SA
      YC(4)=YY+DYY*CA
      XC(8)=XC(4)
      YC(8)=YC(4)

      IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      call edisp(icout,' CORNR: Details of the upper corners of the ')
      call edisp(icout,'        obstruction block follow:')
      do 9998 I=1,4
        WRITE(outs,'(A,I2,3X,2F10.2)')' CORNER',I,XC(I),YC(I)
        call edisp(icout,outs)
 9998 continue
 9999 RETURN
      END

C ******************* PANGS **************************
C PANGS computes the object body surface azimuth (PAZI), elevation
C (PELV) and local X-Axis tilt (PTILT) where:

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
C      r=atan(1.0)/45.0
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
