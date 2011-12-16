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
C Functions and Subroutines and also making use of C code in
C the lib source folder.

C e3dviews.f contains the following:
C LENS:   Initialises viewing parameters.
C MATPOL: Transforms polygon described in 3dv format.
C CLIPFL: Takes a polygon description and returns clipping flags based
C         on viewing frustrum.
C PLNBX:  Checks a polygon against a clipping plane.
C CUTPOL: Cuts a polygon within a viewing frustrum.
C VERTLBL: Displays a vertex label based on the coordinates passed and
C          the vertex number.
C ORIGESP: Draws arrows indicating the site origin.
C SITE2D:  Returns the 2D viewing coords of the current site extremes
C          found in common block RAY5.
C BNDOBJ:  Does range checking on objects to be displayed.


C-----------------------------------------------------------------------
C     LENS - initialise viewing parameters
      SUBROUTINE LENS(IER)
#include "building.h"
#include "prj3dv.h"
C prj3dv.h provides common blocks drwmat clip and image

C Viewing transformation matrices TEMAT is:
      COMMON/OUTIN/IUOUT,IUIN

C Local variables
      real T1MAT,T2MAT,R1MAT
      DIMENSION T1MAT(4,4),T2MAT(4,4),R1MAT(4,4)
      real SKALE,PI,T,SH,CH

      SKALE = 1000.0        ! we are looking from 1km
      PI = 4.0 * ATAN(1.0)

      CALL EYEMAT(EYEM,VIEWM,1.0,T1MAT,R1MAT)

C T2MAT is a 4 x 4 transform matrix
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

      SH=SIN(HANG*PI/180.0)  ! sine of half angle
      CH=COS(HANG*PI/180.0)  ! cosine of half angle
      T=SH/CH

      CALL HMATMUL(T1MAT,T2MAT,TEMAT)

      CALL HREVMAT(TEMAT,ETMMAT,IERR)

      IF (IERR .LT. 0) GOTO  999

C Now compute ETSMAT eye to screen matrix
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

C Define test point TPCLIP and equations CLIPLN to the 6 clipping
C planes in eye space.
      TPCLIP(1) = 0.0
      TPCLIP(2) = 0.0
      TPCLIP(3) = (YON-HITH)/2.0   ! at half distance

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

C Debug
C      write(6,*) 'sh ch t hith yon hang ',sh,ch,t,hith,yon,hang
C      do 41 ij=1,4
C        write(6,*) 'clipln ',ij,(clipln(ij,k),k=1,4)
C  41  continue
C      do 42 ij=1,4
C        write(6,*) 'etsmat ',ij,(etsmat(ij,k),k=1,4)
C  42  continue

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
#include "building.h"
#include "prj3dv.h"
C prj3dv.h provides common block clip.

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL)
      DIMENSION  EQN(4)

      ISTAT=-1

C Consider each clipping plant in turn.
      DO 100 J=1,6

        EQN(1)=CLIPLN(J,1)
        EQN(2)=CLIPLN(J,2)
        EQN(3)=CLIPLN(J,3)
        EQN(4)=CLIPLN(J,4)

C Find the distance of the test point to the current clipping plane.
        TDIS = EQN(1)*TPCLIP(1) + EQN(2)*TPCLIP(2) +
     &         EQN(3)*TPCLIP(3) - EQN(4)

C Find the status (ifbox) of the polygon represented by AX AY AZ to
C the clipping plane equation.
        CALL PLNBX(NAP,AX,AY,AZ,EQN,IFBOX)

        IF (IFBOX) 30,50,30

   30   IF ((IFBOX*TDIS).GE.0) GOTO 40

C ALL VERTEX POINTS ARE ON THE OPPOSITE SIDE OF THE CLIPPING PLANE
C EQN TO THE TEST POINT. HENCE NO PART OF THE OBJECT BOUND BOX
C IS WITHIN THE VIEW FRUSTRUM.
        ISTAT=1
        RETURN

C ALL VERTEX POINTS ARE ON THE SAME SIDE AS THE TEST POINT.
C HENCE THE OBJECT NEED NOT BE CLIPPED BY THIS PLANE.
   40   CONTINUE
        IFCLIP(J)=0
        GOTO 100

C VERTEX POINTS ARE ON EITHER SIDE OF THE CLIPPING PLANE EQN
C SO THE OBJECT MUST BE CLIPPED TO THIS PLANE
   50   CONTINUE
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
#include "prj3dv.h"

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

C MPL =  Max. number of vertices in polygon structure
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
#include "building.h"
#include "prj3dv.h"

      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  EQNP(4),ADST(100)
      LOGICAL CLOSE

C For each of the 6 clipping planes.
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
        IF(CLOSE)GOTO 550   ! distance close to zero do not call vecpln
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
#include "prj3dv.h"

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
      SUBROUTINE VERTLBL(iix,iiy,IP,ier)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER VTEXT*4,VT*4
#ifdef OSI
      integer iix,iiy,iix1,iiy1
#else
      integer*8 iix,iiy,iix1,iiy1
#endif

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
      iix1=iix+1; iiy1=iiy-1;
      CALL wstxpt(iix1,iiy1,VTEXT)
      IF(IFS.GT.0)CALL winfnt(IFS)
      call forceflush()

      RETURN
      END


C-----ORIGESP -----------------------------------------------------------
C ORIGESP uses arrows to indicate the site origin.  It should only be
C called after the model bounds and viewpoint information has been 
C defined. Draw arrows slightly off from the axis because of the method
C used in subroutine arrow.  If the display supports colours then draw
C the arrows in a medium grey.
      SUBROUTINE ORIGESP
#include "building.h"
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      real P,Q
      integer ipoints
      DIMENSION P(3),Q(3),ipoints(6,2)
      real al,aw  ! arrow length and width
      real shaft  ! length of arrow based on zone bounds
      real diag   ! diagonal across the site
      logical greyok

C Local variables to pass to edline.
#ifdef OSI
      integer iicol
#else
      integer*8 iicol
#endif       

      diag = XMX-XMN
      if(diag.lt.20.0)then
        shaft=1.0; al=0.3; aw=0.15
      elseif(diag.ge.20.0.and.diag.lt.100.0)then
        shaft=2.0; al=0.4; aw=0.2
      else
        shaft=3.0; al=0.6; aw=0.3
      endif

C If libraries and monitor support greyscale remember this.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Make up coordinates for arrow along X axis.
      if(greyok)then
        iicol=nifgrey-3
        call winscl('i',iicol)
      endif
      P(1)=0.1; P(2)=0.0; P(3)=-0.1
      Q(1)=shaft; Q(2)=0.0; Q(3)=-0.1
      call arrow(P,Q,al,aw,ipoints,'a',2)
      call forceflush()

C Make up coordinates for arrow along Y axis.
      P(1)=-0.1; P(2)=0.1; P(3)=-0.1
      Q(1)=-0.1; Q(2)=shaft; Q(3)=-0.1
      call arrow(P,Q,al,aw,ipoints,'a',2)
      call forceflush()

C Make up coordinate for arrow along Z axis.
      P(1)=-0.1; P(2)=-0.1; P(3)=0.1
      Q(1)=-0.1; Q(2)=-0.1; Q(3)=shaft
      call arrow(P,Q,al,aw,ipoints,'a',2)
      call forceflush()
      if(greyok)then
        iicol=0
        call winscl('-',iicol)
      endif

      RETURN
      END


C---- SITE2D -----------------------------------------------------------
C SITE2D Returns the 2D viewing coords of the current site extremes
C found in common block RAY5.
      SUBROUTINE SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
#include "building.h"
#include "prj3dv.h"

C Parameters
      real SXMX,SXMN,SYMX,SYMN
      integer ier

      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

      real X1,X2,X3,X4,X5,X6,X7,X8 ! use with orttrn calls
      real Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8
      integer ierr   ! to trap errors 

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

C geometry.h provides commons G0,G1 G2, GS5/GS6.
#include "geometry.h"
#include "prj3dv.h"

C XMN,YMN,ZMN are the model minimum bounds.
C XMX,YMX,ZMX are the model maximum bounds.
C iZBFLG()=0 bounds must be recalculated, iZBFLG()=1 use current value.
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

C Ground geometry.
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      LOGICAL ISTOK,CLOSE

      if(ITDSP.le.5.and.nzg.lt.1)then
        CALL USRMSG(' ',' No zones to bounds check! ','W')
        IER=1
        RETURN
      ENDIF

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

      ISTOK=.TRUE.
      IZ=0

C Point to loop back to.
  100 CONTINUE
      if(ITORG.EQ.0.AND.ISTOK)then

C If site origin has been included then include its bounds as well.
        XMN=AMIN1(XMN,-0.2)
        YMN=AMIN1(YMN,-0.2)
        ZMN=AMIN1(ZMN,0.0)
        XMX=AMAX1(XMX,1.0)
        YMX=AMAX1(YMX,1.0)
        ZMX=AMAX1(ZMX,1.0)
      else

C Process selected zones.
        IZ=IZ+1
        IF(IZ.GT.nzg)GOTO 200
        IVZ=nznog(IZ)
        IF(IVZ.le.0) goto 100  ! unknown zone try next one

C If bounds ok then update project bounds with saved zone values.
        if(iZBFLG(IVZ).eq.1)then
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

C If obstructions are to be drawn, transform the obstruction
C description from commons and then get its bounds.
        if(ITDSP.EQ.0.or.ITDSP.eq.5)then
          if(nbobs(ivz).gt.0)then
            do 301 IB=1,nbobs(ivz)

C Use the GB1 common block and CNVBLK or CNVBLK3A to find bounds.
              if(BLOCKTYP(ivz,IB)(1:4).eq.'obs ')then
                call CNVBLK(XOB(ivz,IB),YOB(ivz,IB),ZOB(ivz,IB),
     &            DXOB(ivz,IB),DYOB(ivz,IB),DZOB(ivz,IB),
     &            BANGOB(ivz,IB,1))
              elseif(BLOCKTYP(ivz,IB)(1:4).eq.'obs3')then
                call CNVBLK3A(XOB(ivz,IB),YOB(ivz,IB),ZOB(ivz,IB),
     &            DXOB(ivz,IB),DYOB(ivz,IB),DZOB(ivz,IB),
     &            BANGOB(ivz,IB,1),BANGOB(ivz,IB,2),BANGOB(ivz,IB,3))
              elseif(BLOCKTYP(ivz,IB)(1:4).eq.'obsp')then
                call CNVBLKP(ivz,IB) ! convert obsp type.
              endif

              do 302 I=1,8    ! for each vertex in block
                if(ITBND.NE.0)then

C Update the overall model bounds to include any obstructions.
                  XMN=AMIN1(XMN,XB(I))
                  YMN=AMIN1(YMN,YB(I))
                  ZMN=AMIN1(ZMN,ZB(I))
                  XMX=AMAX1(XMX,XB(I))
                  YMX=AMAX1(YMX,YB(I))
                  ZMX=AMAX1(ZMX,ZB(I))
                endif
  302         continue
  301       continue
          endif
        endif

C If zone bounds known jump to next zone.
        if(iZBFLG(IVZ).eq.1)goto 100

C If in single zone mode recover common data from memory. 
      ENDIF

C Zone centre of gravity and update overall model bounds.
C (No centre of gravity required if this is the site origin).
      IF(IZ.GE.1)THEN
        DO 402 I=1,nztv(IVZ)
          IF(ITBND.NE.0)THEN
            ZXMN(IVZ)=AMIN1(ZXMN(IVZ),VCOORD(IVZ,I,1))
            ZYMN(IVZ)=AMIN1(ZYMN(IVZ),VCOORD(IVZ,I,2))
            ZZMN(IVZ)=AMIN1(ZZMN(IVZ),VCOORD(IVZ,I,3))
            ZXMX(IVZ)=AMAX1(ZXMX(IVZ),VCOORD(IVZ,I,1))
            ZYMX(IVZ)=AMAX1(ZYMX(IVZ),VCOORD(IVZ,I,2))
            ZZMX(IVZ)=AMAX1(ZZMX(IVZ),VCOORD(IVZ,I,3))
            XMN=AMIN1(XMN,VCOORD(IVZ,I,1))
            YMN=AMIN1(YMN,VCOORD(IVZ,I,2))
            ZMN=AMIN1(ZMN,VCOORD(IVZ,I,3))
            XMX=AMAX1(XMX,VCOORD(IVZ,I,1))
            YMX=AMAX1(YMX,VCOORD(IVZ,I,2))
            ZMX=AMAX1(ZMX,VCOORD(IVZ,I,3))
          ENDIF
  402   CONTINUE

C Set flag to denote that the zone bounds have been calculated.
        IVZ=nznog(IZ)
        iZBFLG(IVZ)=1

        call eclose(ZXMN(IVZ),0.0,0.001,CLOSE)
        if(ZXMN(IVZ).LT.0..AND.ZXMX(IVZ).GE.0.0)then
          ZCOG(IVZ,1)=ZXMN(IVZ)+((ZXMX(IVZ)+ABS(ZXMN(IVZ)))/2.0)
        elseif(ZXMN(IVZ).LT.0.0.AND.ZXMX(IVZ).LE.0.0)then
          ZCOG(IVZ,1)=ZXMN(IVZ)+((ABS(ZXMN(IVZ))-ABS(ZXMX(IVZ)))/2.0)
        elseif(ZXMN(IVZ).GT.0.0.AND.ZXMX(IVZ).GT.0.0)then
          ZCOG(IVZ,1)=ZXMX(IVZ)-((ZXMX(IVZ)-ZXMN(IVZ))/2.0)
        elseif(CLOSE.AND.ZXMX(IVZ).GT.0.0)then
          ZCOG(IVZ,1)=ZXMX(IVZ)/2.0
        endif

        call eclose(ZYMN(IVZ),0.0,0.001,CLOSE)
        if(ZYMN(IVZ).LT.0.0.AND.ZYMX(IVZ).GE.0.0)then
          ZCOG(IVZ,2)=ZYMN(IVZ)+((ZYMX(IVZ)+ABS(ZYMN(IVZ)))/2.0)
        elseif(ZYMN(IVZ).LT.0.0.AND.ZYMX(IVZ).LE.0.0)then
          ZCOG(IVZ,2)=ZYMN(IVZ)+((ABS(ZYMN(IVZ))-ABS(ZYMX(IVZ)))/2.0)
        elseif(ZYMN(IVZ).GT.0.0.AND.ZYMX(IVZ).GT.0.0)then
          ZCOG(IVZ,2)=ZYMX(IVZ)-((ZYMX(IVZ)-ZYMN(IVZ))/2.0)
        elseif(CLOSE.AND.ZYMX(IVZ).GT.0.0)then
          ZCOG(IVZ,2)=ZYMX(IVZ)/2.0
        endif

        call eclose(ZZMN(IVZ),0.0,0.001,CLOSE)
        if(ZZMN(IVZ).LT.0.0.AND.ZZMX(IVZ).GE.0.0)then
          ZCOG(IVZ,3)=ZZMN(IVZ)+((ZZMX(IVZ)+ABS(ZZMN(IVZ)))/2.0)
        elseif(ZZMN(IVZ).LT.0.0.AND.ZZMX(IVZ).LE.0.0)then
          ZCOG(IVZ,3)=ZZMN(IVZ)+((ABS(ZZMN(IVZ))-ABS(ZZMX(IVZ)))/2.0)
        elseif(ZZMN(IVZ).GT.0.0.AND.ZZMX(IVZ).GT.0.0)then
          ZCOG(IVZ,3)=ZZMX(IVZ)-((ZZMX(IVZ)-ZZMN(IVZ))/2.0)
        elseif(CLOSE.AND.ZZMX(IVZ).GT.0.0)then
          ZCOG(IVZ,3)=ZZMX(IVZ)/2.0
        endif
      ENDIF

C Loop back for another body and reset ISTOK so origin omitted.
      ISTOK=.FALSE.
      GOTO 100

C If site grid then include one extra metre in the bounds if
C a large site, otherwise include GRDIS to bounds.
  200 IF(ITGRD.EQ.0)THEN
        if(GRDIS.gt.2.0)then
          XMN=XMN-1.0
          XMX=XMX+1.0
          YMN=YMN-1.0
          YMX=YMX+1.0
        else
          XMN=XMN-GRDIS
          XMX=XMX+GRDIS
          YMN=YMN-GRDIS
          YMX=YMX+GRDIS
        endif
      ENDIF

C All bodies checked so calculate model centre within bounding box.
      call eclose(XMN,0.0,0.001,CLOSE)
      if(XMN.LT.0..AND.XMX.GE.0.0)then
        VIEWM(1)=XMN+((XMX+ABS(XMN))/2.0)
      elseif(XMN.LT.0.0.AND.XMX.LE.0.)then
        VIEWM(1)=XMN+((ABS(XMN)-ABS(XMX))/2.0)
      elseif(XMN.GT.0.0.AND.XMX.GT.0.0)then
        VIEWM(1)=XMX-((XMX-XMN)/2.0)
      elseif(CLOSE.AND.XMX.GT.0.0)then
        VIEWM(1)=XMX/2.0
      endif
      call eclose(YMN,0.0,0.001,CLOSE)
      if(YMN.LT.0..AND.YMX.GE.0.0)then
        VIEWM(2)=YMN+((YMX+ABS(YMN))/2.0)
      elseif(YMN.LT.0.0.AND.YMX.LE.0.)then
        VIEWM(2)=YMN+((ABS(YMN)-ABS(YMX))/2.0)
      elseif(YMN.GT.0.0.AND.YMX.GT.0.0)then
        VIEWM(2)=YMX-((YMX-YMN)/2.0)
      elseif(CLOSE.AND.YMX.GT.0.0)then
        VIEWM(2)=YMX/2.0
      endif
      call eclose(ZMN,0.0,0.001,CLOSE)
      if(ZMN.LT.0.0.AND.ZMX.GE.0.0)then
        VIEWM(3)=ZMN+((ZMX+ABS(ZMN))/2.0)
      elseif(ZMN.LT.0.0.AND.ZMX.LE.0.0)then
        VIEWM(3)=ZMN+((ABS(ZMN)-ABS(ZMX))/2.0)
      elseif(ZMN.GT.0.0.AND.ZMX.GT.0.0)then
        VIEWM(3)=ZMX-((ZMX-ZMN)/2.0)
      elseif(CLOSE.AND.ZMX.GT.0.0)then
        VIEWM(3)=ZMX/2.0
      endif

      RETURN
      END

C ********** INLNST
C INLNST: Initializes surface linestyle array line style.
      SUBROUTINE INLNST(ITYP)
#include "building.h"

      integer ncomp,ncon
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
#include "prj3dv.h"
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
#include "prj3dv.h"
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
