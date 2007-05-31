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

C Subroutine 'trnsf1.f' of ESPins.

C TRNSF1I Sets of the coefficients of the transformation equations
C which translate and rotate the site coordinate system to the 
C local surface under consideration.
C The X and Z coordinates of this object body face are then
C re-expressed in terms of the new local coordiante system
C for use in POINT.
      SUBROUTINE TRNSF1I(IS)

      INCLUDE 'building.h'

      COMMON/TC/ITC,ICNT
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PANGL/PAZI,PELV,PTILT
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      character outs*124
      logical tok

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33.and.itc.gt.1)tok=.true.

C Compute the surface azimuth, elevation, and X-axis tilt angles.
      CALL PANGS(IS)
C Debug...
      if(tok)write(icout,*) 'for surface (ins) ',is,' pazi=',pazi,
     &  ' pelev=',pelv,' ptilt=',ptilt

C Compute the transformation coefficients.
C << old >> AL=(180.-PAZI)*R
      IQ=INT(PAZI/90.0)
      IF(IQ.EQ.0) goto 11
      goto (11,11,12,12),IQ
   11 AL=(180.-PAZI)*R
      goto 13
   12 AL=-(PAZI-180.)*R
   13 be=pelv*r
      ga=-ptilt*r

C In the case of a horizontal surface reset GA to use -AL, negate BE
C and reset AL to zero.
      if((PELV.gt.89.9.and.PELV.lt.90.1).or.
     &   (PELV.lt.-89.9.and.PELV.gt.-90.1))then
        GA= -AL
        AL= 0.0
        be= -pelv*r
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

      IF(.not.tok)goto  9999
      call edisp(icout,' ')
      WRITE(outs,9998)IS
 9998 FORMAT(' TRNSF1I: Transformation equation details for surface ',
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
      call edisp(iuout,' Translated/rotated zone face coordinates: ')
      do 9995 I=1,NV
        WRITE(outs,'(2F10.4)')XFT(I),ZFT(I)
        call edisp(icout,outs)
 9995 continue

 9999 RETURN
      END


C ************** TRNSF2I 
C Transform solar angles of the original site coordinate 
C system to new angles specified relative to the local 
C face coordinate system.
      SUBROUTINE TRNSF2I(IHOUR)

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

      r=atan(1.0)/45.0
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
        Z2=5000.*TSR/COS(ANG*R)
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
   17 IF(.not.tok)goto 9999
      call edisp(icout,' ')
      WRITE(outs,9998)IHOUR
 9998 FORMAT(' TRNSF2I: Transformation details @ hour ',I3)
      call edisp(icout,outs)
      WRITE(outs,9997)SAZI1,SAZI2
 9997 FORMAT(' The azimuth ',F8.2,' transforms to ',F8.2,' and the')
      call edisp(icout,outs)
      WRITE(outs,9996)SALT1,SALT2
 9996 FORMAT('   elevation ',F8.2,' transforms to ',F8.2,'.')
      call edisp(icout,outs)
 9999 RETURN
      END

C *****************************************************************
C Similar to TRNSF3I used in shd but without obstruction blocks.
      SUBROUTINE TRNSF3I(XXW,YYW,ZZW)

      COMMON/TC/ITC,ICNT
      COMMON/OUTPCH/ICOUT
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/SANGL2/SAZI2,SALT2
      COMMON/PROJI/XP,YP,ZP
      character outs*124
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33.and.itc.gt.1)tok=.true.

      r=atan(1.0)/45.0

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

C Projected X.
      XP=(XXW-XOR)*A(1)+(YYW-YOR)*A(2)-
     &   (ZZW-ZOR)*A(3)+(YYW-YOR)*A(4)+(ZZW-ZOR)*A(5)+
     &   (XXW-XOR)*B(1)*SA1/CA1+(YYW-YOR)*B(2)*SA1/CA1-
     &   (ZZW-ZOR)*B(3)*SA1/CA1-(YYW-YOR)*B(4)*SA1/CA1-
     &   (ZZW-ZOR)*B(5)*SA1/CA1

C Projected Y.
      YP=-(XXW-XOR)*B(1)-(YYW-YOR)*B(2)+
     &    (ZZW-ZOR)*B(3)+(YYW-YOR)*B(4)+(ZZW-ZOR)*B(5)

C Projected Z.
      ZP=(XXW-XOR)*B(1)*TA2/CA1+(YYW-YOR)*B(2)*TA2/CA1-
     &   (ZZW-ZOR)*B(3)*TA2/CA1-(YYW-YOR)*B(4)*TA2/CA1-
     &   (ZZW-ZOR)*B(5)*TA2/CA1+(XXW-XOR)*C(1)-
     &   (YYW-YOR)*C(2)+(ZZW-ZOR)*C(3)

C Trace?
      IF(.not.tok)goto 9999
      call edisp(icout,' ')
      call edisp(icout,'  Trace output - Subroutine TRNSF3I')
      call edisp(icout,' ')
      call edisp(icout,' The projected point coordinates: ')
      call edisp(icout,' X-Coord Y-Coord Z-Coord')
      WRITE(outs,'(3F8.1)')XP,YP,ZP
      call edisp(icout,outs)
 9999 RETURN
      END
