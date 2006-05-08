      SUBROUTINE PERS2(IBB)
C
C    DRAW BODY IBB
C   USED WHERE THE BODY IS CONVEX AND UNOBSCURED
C   OR WHERE SINGLE BODY
C
#include "viewer.h"
#include "viewer.h2"
C
C***DIAG
C***D      WRITE(20,*)' PERS2 BODY',IBB
C
C***4.7   GET BODY VISIBILITY INDEX, GLASS OR NOT
      IGLASS=IABS(IBODG(IBB))
C***4.7 IF BODY NOT VISIBLE ,OMIT IT
      IF(IGLASS .EQ. 0) RETURN
C***5.5 OMIT IF LAYER OFF
      IF(LAYER(IBLAY(IBB)).EQ.0)RETURN
C
C   GET EDGE LIMITS OF BODY
      JE1=IBE1(IBB)
      JE2=IBE2(IBB)
C
  11  DO 951 I=JE1,JE2
C
C***4.7   IF EDGE OUTSIDE CONE OF VISION, IGNORE IT
            IF(IABS(IOCV(I)) .NE. 3)THEN
C
C***3.6 GET ENDPOINTS
                KS1=IREPU1(I)
                KS2=IREPU2(I)
                XI1=PUPRVI(KS1,1)
                YI1=PUPRVI(KS1,2)
                ZI1=PUPRVI(KS1,3)
                XI2=PUPRVI(KS2,1)
                YI2=PUPRVI(KS2,2)
                ZI2=PUPRVI(KS2,3)
C
C   IF BODY IS  VIS=+-3 (GLASS), ALL EDGES ARE DRAWN
C                  =+-2 (NORMAL), DRAW FRONT EDGES ONLY
C                  =+-1 (SILHOUETTE),DRAW ONLY SILH. EDGES
C   IF NO HIDDEN LINE SPEC ALL LINES VISIBLE
C   IF SPECIAL EDGE, IOCV=0, DRAW IT
                IF(IGLASS .EQ. 3 .OR.
     &          NTRAT .EQ. 2 .OR.
     &         (IGLASS.EQ.2 .AND. (IOCV(I).EQ.1 .OR. IOCV(I).EQ.2)) .OR.
     &         (IGLASS.EQ.1 .AND. IOCV(I).EQ.1 .OR. IOCV(I).EQ.0)
     &             ) THEN
C
C   PLOT THE EDGE FULL FROM PT1 TO PT2
C***4.7.2 PLOT 3D
  12                 CALL PLOTTR(XI1,YI1,ZI1,XI2,YI2,ZI2,1)
               ELSE
C
C   IF A BACK EDGE
C   PLOT THE HIDDEN EDGE DASHED(NTRAT=1), OR OMIT IT
C**4.7   IF VIS.INDEX =2 (FRONT EDGES SOLID) DRAW BACK EDGES
C                     =1 (SILHOUETTE) DRAW INSIDE FRONT EDGES AND BACK
953                  IF(NTRAT .EQ. 1 .AND.
     &               ((IGLASS.EQ.2 .AND. IOCV(I).EQ.-1) .OR.
     &                (IGLASS.EQ.1 .AND. IOCV(I).EQ.2.OR.IOCV(I).EQ.-1))
     &               ) THEN
                          CALL PLOTTR(XI1,YI1,ZI1,XI2,YI2,ZI2,-20)
                     ENDIF
               ENDIF
          ENDIF
C
951   CONTINUE
C
      RETURN
C
      END
