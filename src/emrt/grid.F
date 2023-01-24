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

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GRID places a grid on a surface.
      SUBROUTINE  GRID(ierror)

#include "building.h"
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)
      COMMON /CELLS/ NCELL(MSM), NCPT(MSM), IGFACT
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      COMMON /GRIDS/ IDG(MGC), XC(MGC), YC(MGC), ZC(MGC)
      COMMON /XXX/  XMIN(MSM,MSM), XMAX(MSM,MSM)
      COMMON /YYY/  YMIN(MSM,MSM), YMAX(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      common/tmrt1/tmrt,temperr
      logical tmrt,temperr
      common /cube5/fstgrp,fstsur,lstsur,strtgr
      integer fstgrp,fstsur,lstsur,strtgr

      DIMENSION  IAPNT(100), APOL(1000)
      DIMENSION  IBPNT(100), BPOL(1000)
      DIMENSION  ICPNT(100), CPOL(1000)
      DIMENSION  EP(3), CG(3), TMAT(4,4), RMAT(4,4)
      DIMENSION  SBBOX(3,2),SCBOX(3,2)
      LOGICAL INSIDEP

c  Initialize.
      NGRID=0
      ierror=0
      DO 100 I=1,nsurmf
        NCELL(I)=0
100   CONTINUE

      DO 1000 I = 1,nsurmf
        if (tmrt) then
          if (i.eq.(nsurmf-5)) fstgrp=ngrid+1
        endif

        IREC = ISPNT(1,I)
        CALL  GETPOL(IREC,NA,NAP,IAPNT,APOL)

        CG(1) = PCG(1,I)
        CG(2) = PCG(2,I)
        CG(3) = PCG(3,I)

        EP(1) = CG(1) + PEQN(1,I)
        EP(2) = CG(2) + PEQN(2,I)
        EP(3) = CG(3) + PEQN(3,I)

C FIND TRANSFORMATION MATRICES TO NORMALISE FACE.
        CALL  EYEMAT(EP,CG,1.0,TMAT,RMAT)

C TRANSFORM FACE TO NORMAL.
        CALL  MATPOLA(NA,NAP,IAPNT,APOL,TMAT,0,
     &               SBBOX,NB,NBP,IBPNT,BPOL,IERR)

        XMIN(I,I) = SBBOX(1,1)
        XMAX(I,I) = SBBOX(1,2)

        YMIN(I,I) = SBBOX(2,1)
        YMAX(I,I) = SBBOX(2,2)

C SAVE NORMALISED FACE IN POLYGON FILE.
        INRP = 0
        CALL  PUTPOL(NB,NBP,IBPNT,BPOL,INRP)
        ISPNT(2,I) = INRP

        DO 250 II = 1,4
          DO 255 JJ = 1,4
            STMAT(II,JJ,I) = TMAT(II,JJ)
            SRMAT(II,JJ,I) = RMAT(II,JJ)
  255     CONTINUE
  250   CONTINUE

C GENERATE BOUND BOX POLYGON AND SAVE IN POLYGON FILE.
        NA = 1
        NAP = 13
        IAPNT(1) = 1
        APOL(1) = 4.
        APOL(2) = SBBOX(1,1)
        APOL(3) = SBBOX(2,1)
        APOL(4) = 1.0
        APOL(5) = SBBOX(1,2)
        APOL(6) = SBBOX(2,1)
        APOL(7) = 1.0
        APOL(8) = SBBOX(1,2)
        APOL(9) = SBBOX(2,2)
        APOL(10) = 1.0
        APOL(11) = SBBOX(1,1)
        APOL(12) = SBBOX(2,2)
        APOL(13) = 1.0

        BOX(1,1,I) = SBBOX(1,1)
        BOX(1,2,I) = SBBOX(1,2)
        BOX(2,1,I) = SBBOX(2,1)
        BOX(2,2,I) = SBBOX(2,2)
        BOX(3,1,I) = SBBOX(3,1)
        BOX(3,2,I) = SBBOX(3,2)

        CALL  MATPOLA(NA,NAP,IAPNT,APOL,RMAT,0,
     &               SCBOX,NC,NCP,ICPNT,CPOL,IERR)

        INRP = 0
        CALL  PUTPOL(NC,NCP,ICPNT,CPOL,INRP)
        ISPNT(5,I) = INRP

C GENERATE GRID POINTS.
        DX = SBBOX(1,2) - SBBOX(1,1)
        DY = SBBOX(2,2) - SBBOX(2,1)

        SX = DX/real(IGFACT*2)
        SY = DY/real(IGFACT*2)

        NCPT(I) = NGRID + 1
        DO 1100 K = 1,IGFACT
          YY = SBBOX(2,1) + (2*K-1)*SY

          DO 1200 L = 1,IGFACT
            XX = SBBOX(1,1) + (2*L-1)*SX
            IF (.NOT.INSIDEP(XX,YY,3,IBPNT(1),BPOL))  GOTO  1200
            CALL  ORTTRN(XX,YY,1.0,RMAT,XT,YT,ZT,IERR)
            NCELL(I) = NCELL(I) + 1
            NGRID = NGRID + 1
            if(ngrid.gt.mgc)then 
              ierror=ngrid
              return
            endif
            IF (NCELL(I) .EQ. 1)  NCPT(I) = NGRID
            XC(NGRID) = XT
            YC(NGRID) = YT
            ZC(NGRID) = ZT

            IDG(NGRID) = I
 1200     CONTINUE
 1100   CONTINUE

        CALL SURINV(I,TMAT)
 1000 CONTINUE
      RETURN

      END
