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

C The file e3dviews.f is a collection of support facilities for 
C producing 3d images of configurations, implemented as Fortran 
C Functions and Subroutines and also making use of C code in wwcut.c.

C e3dviews.f contains the following:
C LENS:   Initialises viewing parameters.
C MATPOL: Transforms polygon described in 3dv format.
C CLIPFL: Takes a polygon description and returns clipping flags based
C         on viewing frustrum.
C PLNBX:  Checks a polygon against a clipping plane.
C CUTPOL: Cuts a polygon within a viewing frustrum.
C VERTLBL: Displays a vertex label based on the coordinates passed and
C          the vertex number.
C ORIGESP: Returns a surface/body indicating the site origin.
C SITE2D:  Returns the 2D viewing coords of the current site extremes
C          found in common block RAY5.
C BNDOBJ:  Does range checking on objects to be displayed.


C-----------------------------------------------------------------------
C     LENS - initialise viewing parameters
      SUBROUTINE LENS(IER)

C Viewing transformation matrices TEMAT is:
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

C View frustrum clip flags
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

      DIMENSION T1MAT(4,4),T2MAT(4,4),R1MAT(4,4)

      SKALE = 1000.0
      PI = 4.0 * ATAN(1.0)

      CALL EYEMAT(EYEM,VIEWM,1.0,T1MAT,R1MAT)

      T2MAT(1,1) = 1.0
      T2MAT(1,2) = 0.0
      T2MAT(1,3) = 0.0
      T2MAT(1,4) = 0.0

      T2MAT(2,1) = 0.0
      T2MAT(2,2) = 1.0
      T2MAT(2,3) = 0.0
      T2MAT(2,4) = 0.0

      T2MAT(3,1) = 0.0
      T2MAT(3,2) = 0.0
      T2MAT(3,3) = 1.0
      T2MAT(3,4) = 0.0

      T2MAT(4,1) = 0.0
      T2MAT(4,2) = 0.0
      T2MAT(4,3) = -HITH
      T2MAT(4,4) = 1.0

      SH=SIN(HANG*PI/180.0)
      CH=COS(HANG*PI/180.0)
      T=SH/CH

      CALL HMATMUL(T1MAT,T2MAT,TEMAT)

      CALL HREVMAT(TEMAT,ETMMAT,IERR)

      IF (IERR .LT. 0) GOTO  999

C     Now compute ETSMAT
      ETSMAT(1,1) = SKALE
      ETSMAT(1,2) = 0.0
      ETSMAT(1,3) = 0.0
      ETSMAT(1,4) = 0.0

      ETSMAT(2,1) = 0.0
      ETSMAT(2,2) = SKALE
      ETSMAT(2,3) = 0.0
      ETSMAT(2,4) = 0.0

      ETSMAT(3,1) = 0.0
      ETSMAT(3,2) = 0.0
      ETSMAT(3,3) = (((HITH*T)/(YON-HITH))+T)*SKALE
      ETSMAT(3,4) = T

      ETSMAT(4,1) = 0.0
      ETSMAT(4,2) = 0.0
      ETSMAT(4,3) = 0.0
      ETSMAT(4,4) = HITH*T

      CALL HMATMUL(TEMAT,ETSMAT,TSMAT)

C Define test point and equations to the 6 clipping
C planes in eye space.
      TPCLIP(1) = 0.0
      TPCLIP(2) = 0.0
      TPCLIP(3) = (YON-HITH)/2.0

      CLIPLN(1,1) = CH
      CLIPLN(1,2) = 0.0
      CLIPLN(1,3) = SH
      CLIPLN(1,4) = -HITH*SH

      CLIPLN(2,1) = -CH
      CLIPLN(2,2) = 0.0
      CLIPLN(2,3) = SH
      CLIPLN(2,4) = -HITH*SH

      CLIPLN(3,1) = 0.0
      CLIPLN(3,2) = CH
      CLIPLN(3,3) = SH
      CLIPLN(3,4) = -HITH*SH

      CLIPLN(4,1) = 0.0
      CLIPLN(4,2) = -CH
      CLIPLN(4,3) = SH
      CLIPLN(4,4) = -HITH*SH

      CLIPLN(5,1) = 0.0
      CLIPLN(5,2) = 0.0
      CLIPLN(5,3) = -1.0
      CLIPLN(5,4) = 0.0

      CLIPLN(6,1) = 0.0
      CLIPLN(6,2) = 0.0
      CLIPLN(6,3) = -1.0
      CLIPLN(6,4) = HITH-YON

      RETURN

  999 CONTINUE
      call edisp(iuout,' Failure to invert matrix in lens ')
      ier=1
      return

      END

C----------------------------------------------------------
C MATPOL transforms polygon (defined by arrays AX(MPL),AY(MPL),AZ(MPL)) 
C where:
C NAP is the number of  vertices on surface,
C IAPNT points to the first vertex of the
C polygon, IANTX is the index of the next point.  Structured
C so that a negative vertex number marks the beginning of a
C hole -
C into polygon B represented by NB,NBP,IBPNT,BX,BY,BZ... 
C
      SUBROUTINE  MATPOL(NAP,IAPNT,AX,AY,AZ,IANXT,TMAT,
     &                   SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)
      parameter( MPL = 200)

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2),TMAT(4,4)

      XMIN = 999999.0
      YMIN = 999999.0
      ZMIN = 999999.0
      XMAX = -999999.0
      YMAX = -999999.0
      ZMAX = -999999.0
      IERR = 0
      NBP = NAP

      IBPNT = IAPNT

      DO 200 J = 1,NAP
        XB = AX(J)*TMAT(1,1)+AY(J)*TMAT(2,1)+AZ(J)*TMAT(3,1)+TMAT(4,1)
        YB = AX(J)*TMAT(1,2)+AY(J)*TMAT(2,2)+AZ(J)*TMAT(3,2)+TMAT(4,2)
        ZB = AX(J)*TMAT(1,3)+AY(J)*TMAT(2,3)+AZ(J)*TMAT(3,3)+TMAT(4,3)
        T4 = AX(J)*TMAT(1,4)+AY(J)*TMAT(2,4)+AZ(J)*TMAT(3,4)+TMAT(4,4)

        if (ABS(T4) .LT. 10E-6)then
          XB = -999.0
          YB = -999.0
          ZB = -999.0
          IERR = -1
          RETURN
        endif

        BX(J) = XB/T4
        BY(J) = YB/T4
        BZ(J) = ZB/T4

        IF (BX(J) .LT. XMIN)  XMIN = BX(J)
        IF (BX(J) .GT. XMAX)  XMAX = BX(J)
        IF (BY(J) .LT. YMIN)  YMIN = BY(J)
        IF (BY(J) .GT. YMAX)  YMAX = BY(J)
        IF (BZ(J) .LT. ZMIN)  ZMIN = BZ(J)
        IF (BZ(J) .GT. ZMAX)  ZMAX = BZ(J)

        IBNXT(J) = IANXT(J)
  200 CONTINUE

      SBBOX(1,1) = XMIN
      SBBOX(1,2) = XMAX
      SBBOX(2,1) = YMIN
      SBBOX(2,2) = YMAX
      SBBOX(3,1) = ZMIN
      SBBOX(3,2) = ZMAX

      RETURN
      END


C----------------------------------------------------------
C MATPOLS transforms a single surface (defined by arrays AX,AY,AZ) 
C into polygon B represented by BX,BY,BZ (both are sized to (MV)
C where:
C NAP & NBP are the number of vertices in the surface.
C IANTX & IBNXT are the index of the next point.  
      SUBROUTINE  MATPOLS(NAP,AX,AY,AZ,IANXT,TMAT,SBBOX,
     &                    NBP,BX,BY,BZ,IBNXT,IERR)
#include "building.h"

      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  BX(MV),BY(MV),BZ(MV),IBNXT(MV)
      DIMENSION  SBBOX(3,2),TMAT(4,4)

      XMIN = 999999.0
      YMIN = 999999.0
      ZMIN = 999999.0
      XMAX = -999999.0
      YMAX = -999999.0
      ZMAX = -999999.0
      IERR = 0
      NBP = NAP

      DO 200 J = 1,NAP
        XB = AX(J)*TMAT(1,1)+AY(J)*TMAT(2,1)+AZ(J)*TMAT(3,1)+TMAT(4,1)
        YB = AX(J)*TMAT(1,2)+AY(J)*TMAT(2,2)+AZ(J)*TMAT(3,2)+TMAT(4,2)
        ZB = AX(J)*TMAT(1,3)+AY(J)*TMAT(2,3)+AZ(J)*TMAT(3,3)+TMAT(4,3)
        T4 = AX(J)*TMAT(1,4)+AY(J)*TMAT(2,4)+AZ(J)*TMAT(3,4)+TMAT(4,4)

        if (ABS(T4) .LT. 10E-6)then
          XB = -999.0
          YB = -999.0
          ZB = -999.0
          IERR = -1
          RETURN
        endif

        BX(J) = XB/T4
        BY(J) = YB/T4
        BZ(J) = ZB/T4

        IF (BX(J) .LT. XMIN)  XMIN = BX(J)
        IF (BX(J) .GT. XMAX)  XMAX = BX(J)
        IF (BY(J) .LT. YMIN)  YMIN = BY(J)
        IF (BY(J) .GT. YMAX)  YMAX = BY(J)
        IF (BZ(J) .LT. ZMIN)  ZMIN = BZ(J)
        IF (BZ(J) .GT. ZMAX)  ZMAX = BZ(J)

        IBNXT(J) = IANXT(J)
  200 CONTINUE

      SBBOX(1,1) = XMIN
      SBBOX(1,2) = XMAX
      SBBOX(2,1) = YMIN
      SBBOX(2,2) = YMAX
      SBBOX(3,1) = ZMIN
      SBBOX(3,2) = ZMAX

      RETURN
      END

C-----------CLIPFL -----------------------------------------------------
C CLIPFL takes a polygon (MPL sized arrays) description and returns
C clipping flags based on viewing frustrum where ISTAT:
C     ISTAT =  0 ALL OF THE POLYGON IS WITHIN THE VIEW FRUSTRUM
C     ISTAT =  1 ALL THE POLYGON IS OUTSIDE THE VIEW FRUSTRUM
C     ISTAT = -1 THE POLYGON STRADDLES THE VIEW FRUSTRUM

      SUBROUTINE CLIPFL(NAP,AX,AY,AZ,ISTAT)

      PARAMETER  ( MPL = 200 )

C View frustrum clip flags
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL)
      DIMENSION  EQN(4)

      ISTAT=-1

C CONSIDER EACH CLIPPING PLANE IN TURN

      DO 100 J=1,6

      EQN(1)=CLIPLN(J,1)
      EQN(2)=CLIPLN(J,2)
      EQN(3)=CLIPLN(J,3)
      EQN(4)=CLIPLN(J,4)

C FIND THE DISTANCE OF THE TEST POINT TO THE CLIPPING PLANE

      TDIS = EQN(1)*TPCLIP(1) + EQN(2)*TPCLIP(2) +
     &       EQN(3)*TPCLIP(3) - EQN(4)

C FIND THE STATUS OF POLYGON TO THE CLIPPING PLANE EQN

      CALL PLNBX(NAP,AX,AY,AZ,EQN,IFBOX)

      IF (IFBOX) 30,50,30

   30 IF ((IFBOX*TDIS).GE.0) GOTO 40

C ALL VERTEX POINTS ARE ON THE OPPOSITE SIDE OF THE CLIPPING PLANE
C EQN TO THE TEST POINT. HENCE NO PART OF THE OBJECT BOUND BOX
C IS WITHIN THE VIEW FRUSTRUM.

      ISTAT=1
      RETURN

C ALL VERTEX POINTS ARE ON THE SAME SIDE AS THE TEST POINT.
C HENCE THE OBJECT NEED NOT BE CLIPPED BY THIS PLANE.

   40 CONTINUE
      IFCLIP(J)=0
      GOTO 100

C VERTEX POINTS ARE ON EITHER SIDE OF THE CLIPPING PLANE EQN
C SO THE OBJECT MUST BE CLIPPED TO THIS PLANE

   50 CONTINUE
      IFCLIP(J)=1

  100 CONTINUE

      RETURN
      END

C-----------CLIPSUR --------
C CLIPSUR takes MV sized AX AY AZ description and returns clipping flags based
C on viewing frustrum where ISTAT:
C     ISTAT =  0 ALL OF THE POLYGON IS WITHIN THE VIEW FRUSTRUM
C     ISTAT =  1 ALL THE POLYGON IS OUTSIDE THE VIEW FRUSTRUM
C     ISTAT = -1 THE POLYGON STRADDLES THE VIEW FRUSTRUM

      SUBROUTINE CLIPSUR(NAP,AX,AY,AZ,ISTAT)
#include "building.h"

C View frustrum clip flags.
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

      DIMENSION  AX(MV),AY(MV),AZ(MV),EQN(4)

C Consider each clipping plane in turn.
      ISTAT=-1
      DO 100 J=1,6
        EQN(1)=CLIPLN(J,1)
        EQN(2)=CLIPLN(J,2)
        EQN(3)=CLIPLN(J,3)
        EQN(4)=CLIPLN(J,4)

C Find the distance of the test point to the clipping plane.
        TDIS = EQN(1)*TPCLIP(1) + EQN(2)*TPCLIP(2) +
     &         EQN(3)*TPCLIP(3) - EQN(4)

C Find the status of the polygon to the clipping plan equation.
      CALL PLNOFSUR(NAP,AX,AY,AZ,EQN,IFBOX)
      IF (IFBOX) 30,50,30
   30 IF ((IFBOX*TDIS).GE.0) GOTO 40

C ALL VERTEX POINTS ARE ON THE OPPOSITE SIDE OF THE CLIPPING PLANE
C EQN TO THE TEST POINT. HENCE NO PART OF THE OBJECT BOUND BOX
C IS WITHIN THE VIEW FRUSTRUM.
      ISTAT=1
      RETURN

C ALL VERTEX POINTS ARE ON THE SAME SIDE AS THE TEST POINT.
C HENCE THE OBJECT NEED NOT BE CLIPPED BY THIS PLANE.
   40 CONTINUE
      IFCLIP(J)=0
      GOTO 100

C VERTEX POINTS ARE ON EITHER SIDE OF THE CLIPPING PLANE EQN
C SO THE OBJECT MUST BE CLIPPED TO THIS PLANE
   50 CONTINUE
      IFCLIP(J)=1

  100 CONTINUE
      RETURN
      END

C-------- PLNBX --------------------------------------------------------
      SUBROUTINE  PLNBX(NA,AX,AY,AZ,EQN,IFBOX)
C                 ------
C     MPL   =  Max. number of vertices in polygon structure
      PARAMETER  (MPL = 200 )

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),EQN(4)

      IFD = 0
      IPFD = 0

      DO 100 I = 1,NA

      DIST = EQN(1)*AX(I) + EQN(2)*AY(I) +
     &       EQN(3)*AZ(I) - EQN(4)

      IF (DIST .LT. -0.0001)  GOTO  10
      IF (DIST .GT. 0.0001)  GOTO  20
      GOTO  100

   10 CONTINUE
      IFD = -1
      GOTO  30
   20 CONTINUE
      IFD = 1
   30 CONTINUE
      IF ((IPFD*IFD) .LT. 0)  GOTO  50
      IPFD = IFD

  100 CONTINUE

      IFBOX = IFD
      RETURN

   50 CONTINUE
      IFBOX = 0

      RETURN
      END

C-------- PLNOFSUR ----
C Find equation of plane (as in PLNBX) but for AX(MV),AY(MV,AZ(MV).
      SUBROUTINE  PLNOFSUR(NAP,AX,AY,AZ,EQN,IFBOX)
#include "building.h"
      DIMENSION  AX(MV),AY(MV),AZ(MV),EQN(4)

      IFD = 0
      IPFD = 0
      DO 100 I = 1,NAP
        DIST = EQN(1)*AX(I) + EQN(2)*AY(I) +
     &         EQN(3)*AZ(I) - EQN(4)

        IF (DIST .LT. -0.0001)  GOTO  10
        IF (DIST .GT. 0.0001)  GOTO  20
        GOTO  100

   10   CONTINUE
        IFD = -1
        GOTO  30
   20   CONTINUE
        IFD = 1
   30   CONTINUE
        IF ((IPFD*IFD) .LT. 0)  GOTO  50
        IPFD = IFD
  100 CONTINUE

      IFBOX = IFD
      RETURN

   50 CONTINUE
      IFBOX = 0

      RETURN
      END

C------- CUTPOL ------------------------------------------------------
C CUTPOL: cuts a polygon within a viewing frustrum.
      SUBROUTINE  CUTPOL(NA,NAP,IAPNT,AX,AY,AZ,IANXT,ISTAT)

C     MPL   =  Max. number of vertices in polygon structure
      PARAMETER  (MPL = 200 )

C View frustrum clip flags.
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  EQNP(4),ADST(100)
      LOGICAL CLOSE

      DO 300 J = 1,6
        IF (IFCLIP(J) .NE. 1)  GOTO  300

        EQNP(1) = CLIPLN(J,1)
        EQNP(2) = CLIPLN(J,2)
        EQNP(3) = CLIPLN(J,3)
        EQNP(4) = CLIPLN(J,4)

        DIST = EQNP(1)*TPCLIP(1) + EQNP(2)*TPCLIP(2) +
     &         EQNP(3)*TPCLIP(3) - EQNP(4)

C On entry assume that first vertex is at AX(1),AY(1),AZ(1). 
        K = IAPNT

  400   CONTINUE
        ADST(K) = EQNP(1)*AX(K) + EQNP(2)*AY(K) +
     &            EQNP(3)*AZ(K) - EQNP(4)
        K = IANXT(K)
        IF (K .NE. IAPNT)  GOTO  400

C Add points which lie on the plane.
        K = IAPNT
  500   CONTINUE

        K1 = IANXT(K)
        CALL ECLOSE(ADST(K),0.0,0.001,CLOSE)
        IF(CLOSE)GOTO 550
        CALL ECLOSE(ADST(K1),0.0,0.001,CLOSE)
        IF(CLOSE)GOTO 550
        IF (ADST(K)*ADST(K1) .GE. 0.0)  GOTO  550

        NAP = NAP + 1
        CALL  VECPLN(AX(K),AY(K),AZ(K), AX(K1),AY(K1),AZ(K1),
     &               EQNP, AX(NAP),AY(NAP),AZ(NAP), IERR)

        IANXT(K) = NAP
        IANXT(NAP) = K1
        ADST(NAP) = 0.0

  550   CONTINUE
        K = IANXT(K)
        IF (K .NE. IAPNT)  GOTO  500

C Delete vertices which are outside view frustrum.
        K = IAPNT
  600   CONTINUE

        K1 = IANXT(K)
        IF (K .EQ. K1)  GOTO  999
        IF (DIST*ADST(K1) .GE. 0.0)  GOTO  650

        IANXT(K) = IANXT(K1)
        IF (K1 .EQ. IAPNT)  IAPNT = IANXT(K1)
        IANXT(K1) = -1
        GOTO  600

  650   CONTINUE
        K = IANXT(K)
        IF (K .NE. IAPNT)  GOTO  600
  300 CONTINUE

      ISTAT = 1
      NA = 1
      RETURN

  999 CONTINUE
      NA = 0
      RETURN

      END

C------- CUTSUR --------
C CUTSUR: cuts a surface (of max MV vertices) within a viewing frustrum.
C This might result in a extra points and cause IAPNT to be returned
C as a different index. 
      SUBROUTINE  CUTSUR(NA,NAP,IAPNT,AX,AY,AZ,IANXT,ISTAT)
#include "building.h"

C View frustrum clip flags.
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  EQNP(4),ADST(MV)
      LOGICAL CLOSE

      DO 300 J = 1,6
        IF (IFCLIP(J) .NE. 1)  GOTO  300

        EQNP(1) = CLIPLN(J,1)
        EQNP(2) = CLIPLN(J,2)
        EQNP(3) = CLIPLN(J,3)
        EQNP(4) = CLIPLN(J,4)

        DIST = EQNP(1)*TPCLIP(1) + EQNP(2)*TPCLIP(2) +
     &         EQNP(3)*TPCLIP(3) - EQNP(4)

C On entry assume that first vertex is at AX(1),AY(1),AZ(1). 
        K = IAPNT

  400   CONTINUE
        ADST(K) = EQNP(1)*AX(K) + EQNP(2)*AY(K) +
     &            EQNP(3)*AZ(K) - EQNP(4)
        K = IANXT(K)
        IF (K .NE. IAPNT)  GOTO  400

C Add points which lie on the plane.
        K = IAPNT
  500   CONTINUE

        K1 = IANXT(K)
        CALL ECLOSE(ADST(K),0.0,0.001,CLOSE)
        IF(CLOSE)GOTO 550
        CALL ECLOSE(ADST(K1),0.0,0.001,CLOSE)
        IF(CLOSE)GOTO 550
        IF (ADST(K)*ADST(K1) .GE. 0.0)  GOTO  550

        NAP = NAP + 1
        CALL  VECPLN(AX(K),AY(K),AZ(K), AX(K1),AY(K1),AZ(K1),
     &             EQNP, AX(NAP),AY(NAP),AZ(NAP), IERR)

        IANXT(K) = NAP
        IANXT(NAP) = K1
        ADST(NAP) = 0.0

  550   CONTINUE
        K = IANXT(K)
        IF (K .NE. IAPNT)  GOTO  500

C Delete vertices which are outside view frustrum.
        K = IAPNT
  600   CONTINUE

        K1 = IANXT(K)
        IF (K .EQ. K1)  GOTO  999
        IF (DIST*ADST(K1) .GE. 0.0)  GOTO  650

        IANXT(K) = IANXT(K1)
        IF (K1 .EQ. IAPNT)  IAPNT = IANXT(K1)
        IANXT(K1) = -1
        GOTO  600

  650   CONTINUE
        K = IANXT(K)
        IF (K .NE. IAPNT)  GOTO  600
  300 CONTINUE

      ISTAT = 1
      NA=1
      RETURN

C No polygons to draw.
  999 CONTINUE
      NA=0
      RETURN

      END

C-----VERTLBL ----------------------------------------------------------
C VERTLBL writes out a vertex label based on the coordinates passed
C and the vertex number.
      SUBROUTINE VERTLBL(ix,iy,IP,ier)
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER VTEXT*4,VT*4

      IER=0

C If vertices are to be labeled do it in the smallest font.
      IF(IFS.GT.0)CALL winfnt(0)
      IF(IP.LE.9)THEN
        WRITE(VT,'(a,i1)')'v',IP
      ELSEIF(IP.GT.9.and.IP.le.99)THEN
        WRITE(VT,'(a,i2)')'v',IP
      ELSEIF(IP.GT.99)THEN
        WRITE(VT,'(a,i3)')'v',IP
      ENDIF
      VTEXT=VT
      CALL wstxpt(ix+1,iy-1,VTEXT)
      IF(IFS.GT.0)CALL winfnt(IFS)
      call forceflush()

      RETURN
      END


C-----ORIGESP -----------------------------------------------------------
C ORIGESP returns a surface/body indicating the site origin. It is 1 
C metre in each direction. Note: this does not reset any of the other
C zone geometry common blocks or the connection based common blocks.
      SUBROUTINE ORIGESP
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      NSUR=1
      NVER(1)=12
      NTV=12
      X(1)=-0.1
      X(2)=0.8
      X(3)=0.8
      X(4)=1.0
      X(5)=0.8
      X(6)=0.8
      X(7)=0.1
      X(8)=0.1
      X(9)=0.2
      X(10)=0.0
      X(11)=-0.2
      X(12)=-0.1
      Y(1)=-0.1
      Y(2)=-0.1
      Y(3)=-0.2
      Y(4)=0.0
      Y(5)=0.2
      Y(6)=0.1
      Y(7)=0.1
      Y(8)=0.8
      Y(9)=0.8
      Y(10)=1.0
      Y(11)=0.8
      Y(12)=0.8
      DO 10 I=1,12
        Z(I)=0.0
        JVN(1,I)=I
   10 CONTINUE

      RETURN
      END


C---- SITE2D -----------------------------------------------------------
C SITE2D Returns the 2D viewing coords of the current site extremes
C found in common block RAY5.
      SUBROUTINE SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
#include "building.h"
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

      SXMX=-1.E+7
      SYMX=-1.E+7
      SXMN=1.E+7
      SYMN=1.E+7

      CALL  ORTTRN(XMN,YMN,ZMN,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X1,Y1,Z1,IERR)
      CALL  ORTTRN(XMX,YMN,ZMN,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X2,Y2,Z2,IERR)
      CALL  ORTTRN(XMN,YMX,ZMN,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X3,Y3,Z3,IERR)
      CALL  ORTTRN(XMX,YMX,ZMN,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X4,Y4,Z4,IERR)
      CALL  ORTTRN(XMN,YMN,ZMX,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X5,Y5,Z5,IERR)
      CALL  ORTTRN(XMX,YMN,ZMX,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X6,Y6,Z6,IERR)
      CALL  ORTTRN(XMN,YMX,ZMX,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X7,Y7,Z7,IERR)
      CALL  ORTTRN(XMX,YMX,ZMX,TEMAT,X0,Y0,Z0,IERR)
      CALL  ORTTRN(X0,Y0,Z0,ETSMAT,X8,Y8,Z8,IERR)
      IER=IERR

C Find minimal 2D x coordinate.
      SXMN=AMIN1(X1,X2,X3,X4,X5,X6,X7,X8,SXMN)

C Find mimimal 2D y coordinate.
      SYMN=AMIN1(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,SYMN)

C Find maximum 2D x coordinate.
      SXMX=AMAX1(X1,X2,X3,X4,X5,X6,X7,X8,SXMX)

C Find maximum 2D y coordinate.
      SYMX=AMAX1(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,SYMX)

      SXMN=SXMN*1.1
      SXMX=SXMX*1.1
      SYMN=SYMN*1.1
      SYMX=SYMX*1.1

      RETURN
      END

C ***************** BNDOBJ 
C BNDOBJ: Does range checking on objects to be displayed.
      SUBROUTINE BNDOBJ(ITRC,IER)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

C XMN,YMN,ZMN are the model minimum bounds.
C XMX,YMX,ZMX are the model maximum bounds.
C ZXMN(),ZYMN(),ZZMN() are the zone minimum bounds.
C ZXMX(),ZYMX(),ZZMX() are the zone maximum bounds.
C ZBFLG()=0 bounds must be recalculated, ZBFLG()=1 use current value.
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

C Ground geometry.
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      logical newgeo  ! to use for testing if new/old geometry file.

      LOGICAL   ISTOK,CLOSE

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER OUTSTR*124

      newgeo=.false.  ! assume older format geometry.
      if(ITDSP.le.5.and.nzg.lt.1)then
        CALL USRMSG(' ',' No zones to bounds check! ','W')
        IER=1
        RETURN
      ENDIF
      IF(nzg.gt.4)CALL USRMSG(' ',' Checking bounds...','-')

C Find the centroid of the collection of objects and each object. If 
C the viewing bounds are not static then find axis max & min.
      IF(ITBND.NE.0)THEN
        XMX=-1.E+7
        YMX=-1.E+7
        XMN=1.E+7
        YMN=1.E+7
        ZMX=-1.E+7
        ZMN=1.E+7
      ENDIF

C If there is a ground geometry then check its bounds.
      if((ITDSP.EQ.5.or.ITDSP.EQ.6).and.NGTV.gt.0)then
        do 41 iv = 1,NGTV
          XMN=AMIN1(XMN,XGT(iv))
          YMN=AMIN1(YMN,YGT(iv))
          ZMN=AMIN1(ZMN,ZGT(iv))
          XMX=AMAX1(XMX,XGT(iv))
          YMX=AMAX1(YMX,YGT(iv))
          ZMX=AMAX1(ZMX,ZGT(iv))
  41    continue
      endif

C If site origin has been included then check its bounds as well.
      ISTOK=.TRUE.
      IZ=0
  100 CONTINUE
      IF(ITORG.EQ.0.AND.ISTOK)THEN
        CALL ORIGESP
        XMN=AMIN1(XMN,-0.2)
        YMN=AMIN1(YMN,-0.2)
        ZMN=AMIN1(ZMN,0.0)
        XMX=AMAX1(XMX,1.0)
        YMX=AMAX1(YMX,1.0)
        ZMX=AMAX1(ZMX,0.0)
      ELSE

C Read in one of the selected geometry files. However, if this
C matches the one currently being edited take this from the
C saved common rather than from the zone file.
        IZ=IZ+1
        IF(IZ.GT.nzg)GOTO 200
        IVZ=nznog(IZ)

C If zone bounds known jump to next zone.
        newgeo=.false.  ! assume older format geometry.
        call eclose(gversion(IVZ),1.1,0.01,newgeo)

C If bounds ok then update project bounds with saved zone values.
        call eclose(ZBFLG(IVZ),1.0,0.001,CLOSE)
        if(CLOSE)then
          XMN=AMIN1(XMN,ZXMN(IVZ))
          YMN=AMIN1(YMN,ZYMN(IVZ))
          ZMN=AMIN1(ZMN,ZZMN(IVZ))
          XMX=AMAX1(XMX,ZXMX(IVZ))
          YMX=AMAX1(YMX,ZYMX(IVZ))
          ZMX=AMAX1(ZMX,ZZMX(IVZ))
        else

C If bounds for this zone need checking reset max & min.
          ZXMX(IVZ)=-1.E+7
          ZYMX(IVZ)=-1.E+7
          ZZMX(IVZ)=-1.E+7
          ZXMN(IVZ)=1.E+7
          ZYMN(IVZ)=1.E+7
          ZZMN(IVZ)=1.E+7
        endif

C If obstructions are to be drawn, get the name of the
C obstruction file, read it, transform the obstruction
C description into a gen body and then get its bounds.
        IF(ITDSP.EQ.0.or.ITDSP.eq.5)THEN
          if(IOBS(IVZ).EQ.1)then
            IUF=IFIL+1
            if(itrc.gt.1)then
              WRITE(OUTSTR,'(A,A)')' Bounds: ',
     &          ZOBS(IVZ)(:lnblnk(ZOBS(IVZ)))
              CALL USRMSG(' ',OUTSTR,'-')
            endif
            CALL EGOMST(IUF,ZOBS(IVZ),0,0,IUOUT,IER)
          elseif(IOBS(IVZ).EQ.2)then

C Check geometry file version and scan geometry file to get obstructions.
            if(newgeo)then
              call georead(IFIL+1,LGEOM(IVZ),IVZ,1,0,IUOUT,IER)
            endif
          endif
          DO 301 IB=1,NB
            CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),
     &               BANG(IB))
            DO 302 I=1,NTV
              IF(ITBND.NE.0)THEN

C Update the overall model bounds to include any obstructions.
                XMN=AMIN1(XMN,X(I))
                YMN=AMIN1(YMN,Y(I))
                ZMN=AMIN1(ZMN,Z(I))
                XMX=AMAX1(XMX,X(I))
                YMX=AMAX1(YMX,Y(I))
                ZMX=AMAX1(ZMX,Z(I))
              ENDIF
  302       CONTINUE
  301     CONTINUE
        ENDIF

C If zone bounds known jump to next zone.
        call eclose(ZBFLG(IVZ),1.0,0.001,CLOSE)
        if(CLOSE)goto 100

C If in single zone mode recover common data from memory 
C otherwise read from file.
        IF(IVZ.NE.izgfoc)THEN
          call eclose(gversion(IVZ),1.1,0.01,newgeo)
          WRITE(OUTSTR,78)LGEOM(IVZ)(1:LNBLNK(LGEOM(IVZ)))
   78     FORMAT(' Bounds: ',A)
          if(itrc.gt.1)CALL USRMSG(' ',OUTSTR,'-')
          if(newgeo)then
            call georead(IFIL+1,LGEOM(IVZ),IVZ,1,0,IUOUT,IER)
          else
            call egomin(IFIL+1,LGEOM(IVZ),IVZ,1,0,IUOUT,IER)
          endif
        ELSEIF(IVZ.EQ.izgfoc)THEN
          CALL ERCZONE(IVZ)
        ENDIF
      ENDIF

C Zone centre of gravity and update overall model bounds.
C (No centre of gravity required if this is the site origin).
      IF(IZ.GE.1)THEN
        DO 402 I=1,NTV
          IF(ITBND.NE.0)THEN
            ZXMN(nznog(IZ))=AMIN1(ZXMN(nznog(IZ)),X(I))
            ZYMN(nznog(IZ))=AMIN1(ZYMN(nznog(IZ)),Y(I))
            ZZMN(nznog(IZ))=AMIN1(ZZMN(nznog(IZ)),Z(I))
            ZXMX(nznog(IZ))=AMAX1(ZXMX(nznog(IZ)),X(I))
            ZYMX(nznog(IZ))=AMAX1(ZYMX(nznog(IZ)),Y(I))
            ZZMX(nznog(IZ))=AMAX1(ZZMX(nznog(IZ)),Z(I))
            XMN=AMIN1(XMN,X(I))
            YMN=AMIN1(YMN,Y(I))
            ZMN=AMIN1(ZMN,Z(I))
            XMX=AMAX1(XMX,X(I))
            YMX=AMAX1(YMX,Y(I))
            ZMX=AMAX1(ZMX,Z(I))
          ENDIF
  402   CONTINUE

C Set flag to denote that the zone bounds have been calculated.
        IVZ=nznog(IZ)
        ZBFLG(IVZ)=1.0

        call eclose(ZXMN(IVZ),0.0,0.001,CLOSE)
        if(ZXMN(IVZ).LT.0..AND.ZXMX(IVZ).GE.0.)then
          ZCOG(IVZ,1)=ZXMN(IVZ)+((ZXMX(IVZ)+ABS(ZXMN(IVZ)))/2.)
        elseif(ZXMN(IVZ).LT.0..AND.ZXMX(IVZ).LE.0.)then
          ZCOG(IVZ,1)=ZXMN(IVZ)+((ABS(ZXMN(IVZ))-ABS(ZXMX(IVZ)))/2.)
        elseif(ZXMN(IVZ).GT.0..AND.ZXMX(IVZ).GT.0.)then
          ZCOG(IVZ,1)=ZXMX(IVZ)-((ZXMX(IVZ)-ZXMN(IVZ))/2.)
        elseif(CLOSE.AND.ZXMX(IVZ).GT.0.)then
          ZCOG(IVZ,1)=ZXMX(IVZ)/2.
        endif

        call eclose(ZYMN(IVZ),0.0,0.001,CLOSE)
        if(ZYMN(IVZ).LT.0..AND.ZYMX(IVZ).GE.0.)then
          ZCOG(IVZ,2)=ZYMN(IVZ)+((ZYMX(IVZ)+ABS(ZYMN(IVZ)))/2.)
        elseif(ZYMN(IVZ).LT.0..AND.ZYMX(IVZ).LE.0.)then
          ZCOG(IVZ,2)=ZYMN(IVZ)+((ABS(ZYMN(IVZ))-ABS(ZYMX(IVZ)))/2.)
        elseif(ZYMN(IVZ).GT.0..AND.ZYMX(IVZ).GT.0.)then
          ZCOG(IVZ,2)=ZYMX(IVZ)-((ZYMX(IVZ)-ZYMN(IVZ))/2.)
        elseif(CLOSE.AND.ZYMX(IVZ).GT.0.)then
          ZCOG(IVZ,2)=ZYMX(IVZ)/2.
        endif

        call eclose(ZZMN(IVZ),0.0,0.001,CLOSE)
        if(ZZMN(IVZ).LT.0..AND.ZZMX(IVZ).GE.0.)then
          ZCOG(IVZ,3)=ZZMN(IVZ)+((ZZMX(IVZ)+ABS(ZZMN(IVZ)))/2.)
        elseif(ZZMN(IVZ).LT.0..AND.ZZMX(IVZ).LE.0.)then
          ZCOG(IVZ,3)=ZZMN(IVZ)+((ABS(ZZMN(IVZ))-ABS(ZZMX(IVZ)))/2.)
        elseif(ZZMN(IVZ).GT.0..AND.ZZMX(IVZ).GT.0.)then
          ZCOG(IVZ,3)=ZZMX(IVZ)-((ZZMX(IVZ)-ZZMN(IVZ))/2.)
        elseif(CLOSE.AND.ZZMX(IVZ).GT.0.)then
          ZCOG(IVZ,3)=ZZMX(IVZ)/2.
        endif
      ENDIF

C Loop back for another body.
      ISTOK=.FALSE.
      GOTO 100

C If site grid then include this in the bounds.
  200 IF(ITGRD.EQ.0)THEN
        XMN=XMN-GRDIS
        XMX=XMX+GRDIS
        YMN=YMN-GRDIS
        YMX=YMX+GRDIS
      ENDIF

C All bodies checked so calculate configuration centre of gravity.
      call eclose(XMN,0.0,0.001,CLOSE)
      if(XMN.LT.0..AND.XMX.GE.0.)then
        VIEWM(1)=XMN+((XMX+ABS(XMN))/2.)
      elseif(XMN.LT.0..AND.XMX.LE.0.)then
        VIEWM(1)=XMN+((ABS(XMN)-ABS(XMX))/2.)
      elseif(XMN.GT.0..AND.XMX.GT.0.)then
        VIEWM(1)=XMX-((XMX-XMN)/2.)
      elseif(CLOSE.AND.XMX.GT.0.)then
        VIEWM(1)=XMX/2.
      endif
      call eclose(YMN,0.0,0.001,CLOSE)
      if(YMN.LT.0..AND.YMX.GE.0.)then
        VIEWM(2)=YMN+((YMX+ABS(YMN))/2.)
      elseif(YMN.LT.0..AND.YMX.LE.0.)then
        VIEWM(2)=YMN+((ABS(YMN)-ABS(YMX))/2.)
      elseif(YMN.GT.0..AND.YMX.GT.0.)then
        VIEWM(2)=YMX-((YMX-YMN)/2.)
      elseif(CLOSE.AND.YMX.GT.0.)then
        VIEWM(2)=YMX/2.
      endif
      call eclose(ZMN,0.0,0.001,CLOSE)
      if(ZMN.LT.0..AND.ZMX.GE.0.)then
        VIEWM(3)=ZMN+((ZMX+ABS(ZMN))/2.)
      elseif(ZMN.LT.0..AND.ZMX.LE.0.)then
        VIEWM(3)=ZMN+((ABS(ZMN)-ABS(ZMX))/2.)
      elseif(ZMN.GT.0..AND.ZMX.GT.0.)then
        VIEWM(3)=ZMX+((ZMX-ZMN)/2.)
      elseif(CLOSE.AND.ZMX.GT.0.)then
        VIEWM(3)=ZMX/2.
      endif

C If site grid then include this in the bounds.
C      IF(ITGRD.EQ.0)THEN
C        ZXMN(I)=ZXMN(I)-GRDIS
C        ZXMX(I)=ZXMX(I)+GRDIS
C        ZYMN(I)=ZYMN(I)-GRDIS
C        ZYMX(I)=ZYMX(I)+GRDIS
C      ENDIF

      RETURN
      END

C ********** INLNST
C INLNST: Initializes surface linestyle array line style.
      SUBROUTINE INLNST(ITYP)
#include "building.h"

      COMMON/C1/NCOMP,NCON
      COMMON/RAY6/LINSTY(MCON)

      do 10 I=1,NCON
        LINSTY(I) = ITYP
   10 continue
      return
      end

C ********** INLNSTG
C INLNSTG: Initializes ground surface linestyle array line style.
C linstyg=1 std line, =2 double width line, =3 close dotted line.
      SUBROUTINE INLNSTG(ITYP)
#include "building.h"
      COMMON/RAY6G/LINSTYG(MGRT)
      do 10 I=1,MGRT
        LINSTYG(I) = ITYP
   10 continue
      return
      end

C ********* SAVEVIEW
C SAVEVIEW: Save the current wireframe view information (eye/view points,
C angle of view and bounds.
      subroutine saveview
#include "building.h"
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/IMAGESV/imtsv,OEYEM(3),OVIEWM(3),OHITH,OYON,OANG,OHANG,
     &  OWIDE
      COMMON/RAY5SV/OXMN,OYMN,OZMN,OXMX,OYMX,OZMX

      imtsv=imt
      OVIEWM(1)=VIEWM(1)
      OVIEWM(2)=VIEWM(2)
      OVIEWM(3)=VIEWM(3)
      OEYEM(1)=EYEM(1)
      OEYEM(2)=EYEM(2)
      OEYEM(3)=EYEM(3)
      oxmn=XMN
      oymn=YMN
      ozmn=ZMN
      oxmx=XMX
      oymx=YMX
      ozmx=ZMX
      oang=ANG
      ohang=HANG
      oyon=yon
      owide=wide

      return
      end

C ********* RECOVIEW
C RECOVIEW: Recover wireframe view information (eye/view points,
C angle of view and bounds.
      subroutine recoview
#include "building.h"
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/IMAGESV/imtsv,OEYEM(3),OVIEWM(3),OHITH,OYON,OANG,OHANG,
     &  OWIDE
      COMMON/RAY5SV/OXMN,OYMN,OZMN,OXMX,OYMX,OZMX

      imt=imtsv
      VIEWM(1)=OVIEWM(1)
      VIEWM(2)=OVIEWM(2)
      VIEWM(3)=OVIEWM(3)
      EYEM(1)=OEYEM(1)
      EYEM(2)=OEYEM(2)
      EYEM(3)=OEYEM(3)
      xmn=OXMN
      ymn=OYMN
      zmn=OZMN
      xmx=OXMX
      ymx=OYMX
      zmx=OZMX
      ang=OANG
      hang=OHANG
      yon=oyon
      wide=owide

      return
      end
