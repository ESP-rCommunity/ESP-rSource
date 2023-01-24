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

C+++++++++++++++++++++++ MATPOLA
C MATPOL transforms polygon a to b using APOL structure.
      SUBROUTINE  MATPOLA(NA,NAP,IAPNT,APOL,TMAT,IFREV,
     &                   SBBOX,NB,NBP,IBPNT,BPOL,IERR)

      DIMENSION  IAPNT(100),APOL(1000)
      DIMENSION  IBPNT(100),BPOL(1000)
      DIMENSION  SBBOX(3,2),TMAT(4,4)

      XMIN = 999999.0
      YMIN = 999999.0
      ZMIN = 999999.0
      XMAX = -999999.0
      YMAX = -999999.0
      ZMAX = -999999.0
      IERR = 0
      NB = NA
      NBP = NAP

      IF (IFREV .EQ. 1)  GOTO  400

      DO 100 I = 1,NA
        IBPNT(I) = IAPNT(I)
        IP = IABS(IAPNT(I))
        BPOL(IP) = APOL(IP)
        NP = INT(APOL(IP))
        K = IP - 2

        DO 200 J = 1,NP
          K = K + 3
          XA = APOL(K)
          YA = APOL(K+1)
          ZA = APOL(K+2)

          XB = XA*TMAT(1,1) + YA*TMAT(2,1) + ZA*TMAT(3,1) + TMAT(4,1)
          YB = XA*TMAT(1,2) + YA*TMAT(2,2) + ZA*TMAT(3,2) + TMAT(4,2)
          ZB = XA*TMAT(1,3) + YA*TMAT(2,3) + ZA*TMAT(3,3) + TMAT(4,3)
          T4 = XA*TMAT(1,4) + YA*TMAT(2,4) + ZA*TMAT(3,4) + TMAT(4,4)

          IF (ABS(T4) .LT. 10E-6)  GOTO 900

          XB = XB/T4
          YB = YB/T4
          ZB = ZB/T4

          BPOL(K) = XB
          BPOL(K+1) = YB
          BPOL(K+2) = ZB

          IF (XB .LT. XMIN)  XMIN = XB
          IF (XB .GT. XMAX)  XMAX = XB
          IF (YB .LT. YMIN)  YMIN = YB
          IF (YB .GT. YMAX)  YMAX = YB
          IF (ZB .LT. ZMIN)  ZMIN = ZB
          IF (ZB .GT. ZMAX)  ZMAX = ZB
  200   CONTINUE
  100 CONTINUE

      SBBOX(1,1) = XMIN
      SBBOX(1,2) = XMAX
      SBBOX(2,1) = YMIN
      SBBOX(2,2) = YMAX
      SBBOX(3,1) = ZMIN
      SBBOX(3,2) = ZMAX

      RETURN

  400 CONTINUE

      DO 500 I = 1,NA
        IBPNT(I) = IAPNT(I)
        IP = IABS(IAPNT(I))
        BPOL(IP) = APOL(IP)
        NP = INT(APOL(IP))
        K = IP - 2
        K1 = IP + NP*3 + 1

        DO 600 J = 1,NP
          K = K + 3
          XA = APOL(K)
          YA = APOL(K+1)
          ZA = APOL(K+2)

          XB = XA*TMAT(1,1) + YA*TMAT(2,1) + ZA*TMAT(3,1) + TMAT(4,1)
          YB = XA*TMAT(1,2) + YA*TMAT(2,2) + ZA*TMAT(3,2) + TMAT(4,2)
          ZB = XA*TMAT(1,3) + YA*TMAT(2,3) + ZA*TMAT(3,3) + TMAT(4,3)
          T4 = XA*TMAT(1,4) + YA*TMAT(2,4) + ZA*TMAT(3,4) + TMAT(4,4)
          IF (ABS(T4) .LT. 10E-6)  GOTO 900

          XB = XB/T4
          YB = YB/T4
          ZB = ZB/T4

          K1 = K1 - 3
          BPOL(K1) = XB
          BPOL(K1+1) = YB
          BPOL(K1+2) = ZB

          IF (XB .LT. XMIN)  XMIN = XB
          IF (XB .GT. XMAX)  XMAX = XB
          IF (YB .LT. YMIN)  YMIN = YB
          IF (YB .GT. YMAX)  YMAX = YB
          IF (ZB .LT. ZMIN)  ZMIN = ZB
          IF (ZB .GT. ZMAX)  ZMAX = ZB
  600   CONTINUE
  500 CONTINUE

      SBBOX(1,1) = XMIN
      SBBOX(1,2) = XMAX
      SBBOX(2,1) = YMIN
      SBBOX(2,2) = YMAX
      SBBOX(3,1) = ZMIN
      SBBOX(3,2) = ZMAX

      RETURN

  900 CONTINUE
      XB = -999.0
      YB = -999.0
      ZB = -999.0
      IERR = -1

      RETURN

      END

C+++++++++++++++++ GETPOL
C GETPOL reads data from the polygon file (IFPOL).
      SUBROUTINE  GETPOL(INRPOL,NA,NAP,IAPNT,APOL)

      COMMON/FIL/IUF,IFPOL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      DIMENSION  AMA(2000),IAPNT(100),APOL(1000)
      character outs*124

      IF (INRPOL .GT. NTRPOL)  GOTO  999
      IREC = INRPOL
      READ(IFPOL,REC=IREC,ERR=1999)IDSUR,NR,(AMA(K),K=1,18)

      IF (NR .LE. 1)  GOTO  101

      IREC = IREC + 1
      IST = 19

      DO 100 I = 2,NR
        IEN = IST + 19
        READ(IFPOL,REC=IREC,ERR=1999)(AMA(J),J=IST,IEN)
        IREC = IREC + 1
        IST = IST + 20
  100 CONTINUE
  101 CONTINUE

      NA = INT(AMA(1))
      NAP = INT(AMA(2))
      K = 3

      DO 200 I = 1,NA
        IAPNT(I) = INT(AMA(K))
        K = K + 1
  200 CONTINUE

      DO 300 I = 1,NAP
        APOL(I) = AMA(K)
        K = K + 1
  300 CONTINUE

      RETURN

  999 CONTINUE
      WRITE(outs,6000)  INRPOL,NTRPOL
 6000 FORMAT(' ERROR IN GETPOL  -  INRPOL, NTRPOL  = ',2I4)
      call edisp(iuout,outs)
      return

 1999 CONTINUE
      WRITE(outs,6100)  IREC
 6100 FORMAT(' ERROR IN GETPOL  -  ACCESSING RECORD  = ',I4)
      call edisp(iuout,outs)
      return

      END

C *********** PUTPOL
C OUTPUT DATA TO POLYGON FILE.
      SUBROUTINE  PUTPOL(NA,NAP,IAPNT,APOL,INRP)

C IF INRP = 0 THEN OUTPUT POLYGON AS NEW (AT END OF FILE)
C IF INRP > 0 THEN OUTPUT POLYGON AT RECORD INRP
C INRP IS RETURNED WITH THE POSITION OF THE POLYGON

      COMMON/FIL/IUF,IFPOL
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      DIMENSION  IAPNT(100),APOL(1000)
      DIMENSION  AMA(2000)

C Decode the polygon structure into AMA
      IDSUR=0
      AMA(1) = real(NA)
      AMA(2) = real(NAP)

      K = 3
      DO 100 I = 1,NA
        AMA(K) = real(IAPNT(I))
        K = K + 1
  100 CONTINUE

      DO 150 I = 1,NAP
        AMA(K) = APOL(I)
        K = K + 1
  150 CONTINUE

C Write out the polygon structure.
      NAMA = NA+NAP+4
      NRFPOL = NAMA/20
      IF (MOD(NAMA,20) .GT. 0)  NRFPOL = NRFPOL + 1
      IREC = INRP
      IF (INRP .EQ. 0)  IREC = NTRPOL + 1
      WRITE(IFPOL,REC=IREC)  IDSUR,NRFPOL,(AMA(K),K=1,18)

      IF (NRFPOL .EQ. 1)  GOTO  300
      IST = 19

      DO 200 J = 2,NRFPOL
        IEN = IST + 19
        IREC = IREC + 1
        WRITE(IFPOL,REC=IREC)  (AMA(K),K=IST,IEN)
        IST = IST + 20
  200 CONTINUE
  300 CONTINUE

      IF (INRP .GT. 0)  GOTO  999
      INRP = NTRPOL + 1
      NTRPOL = NTRPOL + NRFPOL

  999 CONTINUE
      RETURN
      END


C++++++++++++++++++++ ENCPOL
C ENCPOL converts 'GEN' type data (COMMON/G1M) to a Polygon 
C data structure.
      SUBROUTINE  ENCPOL(I,NA,NAP,IAPNT,APOL)

#include "building.h"

      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM

C Parameters
      integer I   ! supplied index of the surface in the zone
      integer NA  ! returned counter of polygons
      integer NAP ! returned counter of points
      integer IAPNT  ! returned pointer to first vert of each polygon
      real APOL   ! returned packed coordinant array
      DIMENSION IAPNT(100),APOL(1000)

C Decode into polygons.
      NA = 1
      IP = 1
      IJ = 1
   50 CONTINUE
      NV = 1
      DO 100 J = IJ+1,NVERM(I)
         IF (JVNM(I,J) .LE. 0)  GOTO  150
         NV = NV + 1
  100 CONTINUE
  150 CONTINUE
      IAPNT(NA) = IP
      IF (JVNM(I,IJ) .LT. 0)  IAPNT(NA) = -IP
      APOL(IP) = NV
      DO 200 J = 0,NV-1
         K = IABS(JVNM(I,IJ+NV-(J+1)))
         APOL(IP+J*3+1) = XM(K)
         APOL(IP+J*3+2) = YM(K)
         APOL(IP+J*3+3) = ZM(K)
  200 CONTINUE
      IP = IP + NV*3 + 1
      IF (IJ+NV-1 .EQ. NVERM(I))  GOTO  300
      NA = NA + 1
      IJ = IJ + NV
      GOTO  50
  300 CONTINUE

      NAP = IP -1

      RETURN
      END

C ***************** ESMANYZON 
C ESMANYZON populates G1M with geometry for any zone.

      SUBROUTINE ESMANYZON(iz)

#include "building.h"
#include "geometry.h"

      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM

      NTVM=NZTV(iz)
      DO 65 iv=1,NTVM
        XM(iv)=SZCOORDS(iz,iv,1)
        YM(IV)=SZCOORDS(iz,iv,2)
        ZM(IV)=SZCOORDS(iz,iv,3)
   65 CONTINUE

      NSURM=NZSUR(iz)
      DO 66 is=1,NSURM
        NVERM(is)=ISZNVER(iz,is)
        DO 67 iv=1,NVERM(is)
          JVNM(is,iv)=ISZJVN(iz,is,iv)
   67   CONTINUE
   66 CONTINUE

      RETURN
      END
      
