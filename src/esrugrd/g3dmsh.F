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

C This file contains the following subroutines:

C INSPTS defines the intersection points in a surface.
C INS2LN determines the intersection point for two lines in the same 
C        plan.
C CVSSRF creates the control volumes for the surfaces.
C BAVRTX define the line number within which the required vertex was 
C        found. It also returns the number of the vertex before and 
C        after it.
C CZCNVS controls the generation of zone's nodes, control volumes, 
C        and connections.
C CNCSEC creates the nodes ,and control volumes for connection elements
C        (srf-edge, edge-edge, and edge-crn).
C CVNODE creates control volumes and nodes for surface control volumes.
C CNCSRF controls the creation of the connections for surfaces.

C ***************************    INSPTS    ****************************
C INSPTS defines the intersection points in a surface.
C *********************************************************************
      SUBROUTINE INSPTS
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)
      COMMON/GR3D41/NBINT(MS),NTINT(MS)
      COMMON/GR3D42/NVLN(MS,MLS),IVLN(MS,MLS,MLS)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)

      DO 1000 IS=1,NSUR

C Bypass if this surface is lumped.
      IF(INDXS(IS).NE.3)GOTO 1000

C Initialize the mesh generation variables.
      DO 10 I=1,NLINA(IS)

C Number of vertices per line.
        NVLN(IS,I)=0
      DO 10 II=1,NLINA(IS)

C The vertices belonging to each line sequence.
        IVLN(IS,I,II)=0
   10 CONTINUE

C Define the intersection points between boundary lines with themselves
C and boundary lines with gridding lines.
      DO 20 IV=1,NVRTA(IS)

C Number of surface control volumes the current vertex will participate in.
        NCVS(IS,IV)=1

C Save the intersection location.
        XINT(IS,IV)=XVRT(IS,IV)
        ZINT(IS,IV)=ZVRT(IS,IV)
      DO 20 IL=1,NLINA(IS)
        IF(IV.EQ.ILINE(IS,IL,1).OR.IV.EQ.ILINE(IS,IL,2))THEN
          IF(IL.GT.NLINB(IS))NCVS(IS,IV)=NCVS(IS,IV)+1
          NVLN(IS,IL)=NVLN(IS,IL)+1
          IVLN(IS,IL,NVLN(IS,IL))=IV

C Check if the intersection vertices is located on a line but not at 
C the ends.
        ELSE
          CALL PNTLNE(IS,IL,IV,IER)
          IF(IER.EQ.0)THEN
            NVLN(IS,IL)=NVLN(IS,IL)+1
            IVLN(IS,IL,NVLN(IS,IL))=IV
          ENDIF
        ENDIF
   20 CONTINUE
      NBINT(IS)=NVRTA(IS)
      NTINT(IS)=NVRTA(IS)

C Define the intersection points between the gridding lines.
      DO 40 IL1=NLINB(IS)+1,NLINA(IS)
      DO 40 IL2=IL1+1,NLINA(IS)
        CALL INS2LN(IS,IL1,IL2,XINTR,ZINTR,IER)
        IF(IER.EQ.0)THEN
          IF(NTINT(IS).EQ.MGV)THEN
            STOP "error (591): Maximum gridding vertices MGV exceeded."
          ENDIF
          NTINT(IS)=NTINT(IS)+1
          XINT(IS,NTINT(IS))=XINTR
          ZINT(IS,NTINT(IS))=ZINTR
          NCVS(IS,NTINT(IS))=4
          IF(NVLN(IS,IL1).EQ.MLS)THEN
            STOP "error (592): Maximum gridding lines MLS exceeded."
          ENDIF
          NVLN(IS,IL1)=NVLN(IS,IL1)+1
          IVLN(IS,IL1,NVLN(IS,IL1))=NTINT(IS)
          IF(NVLN(IS,IL2).EQ.MLS)THEN
            STOP "error (593): Maximum gridding lines MLS exceeded."
          ENDIF
          NVLN(IS,IL2)=NVLN(IS,IL2)+1
          IVLN(IS,IL2,NVLN(IS,IL2))=NTINT(IS)
        ENDIF
   40 CONTINUE

C Re-order the vertex list for the lines.
      CALL RORDER(IS)
 1000 CONTINUE
      RETURN
      END

C ***************************    INS2LN    ****************************
C INS2LN determines the intersection point for two lines in the same 
C        plan.
C *********************************************************************
      SUBROUTINE INS2LN(IS,IL1,IL2,XINTR,ZINTR,IER)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D41/NBINT(MS),NTINT(MS)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)

      IER=-1
      IA=ILINE(IS,IL1,1)
      IB=ILINE(IS,IL1,2)
      IC=ILINE(IS,IL2,1)
      ID=ILINE(IS,IL2,2)

C Check if there is a common vertex.
      IF(IA.EQ.IC.OR.IA.EQ.ID.OR.IB.EQ.IC.OR.IB.EQ.ID)RETURN
      AX=XVRT(IS,IA)
      AZ=ZVRT(IS,IA)
      BX=XVRT(IS,IB)
      BZ=ZVRT(IS,IB)
      CX=XVRT(IS,IC)
      CZ=ZVRT(IS,IC)
      DX=XVRT(IS,ID)
      DZ=ZVRT(IS,ID)
      CALL INTRSC(AX,AZ,BX,BZ,CX,CZ,DX,DZ,IER,XINTR,ZINTR)
      IF(IER.NE.0)RETURN

C Check if the intersection point is a defined point.
      DO 10 IV=1,NBINT(IS)
        DEL=ABS(XINT(IS,IV)-XINTR)+ABS(ZINT(IS,IV)-ZINTR)
        IF(DEL.LT.1.E-3)RETURN
   10 CONTINUE
      IER=0
      RETURN
      END

C *****************************  CVSSRF  ******************************
C CVSSRF creates the control volumes for the surfaces.
C *********************************************************************
      SUBROUTINE CVSSRF
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D41/NBINT(MS),NTINT(MS)
      COMMON/GR3D42/NVLN(MS,MLS),IVLN(MS,MLS,MLS)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)
      COMMON/GR3D44/NCVSF(MS),NCVV(MS,MCVS),ICVV(MS,MCVS,MCVV)
      COMMON/GR3D45/NLINT,NVLNT(MLS),IVLNT(MLS,MLS),ILINBT(MLBS,2)

      DIMENSION ILEX(3),NLIN(4),ILIN(4,6)
      DIMENSION ILTYP(4),INMV(4,6),IVX(4,6,MLS)

      DO 1000 IS=1,NSUR

C Bypass if this surface is lumped.
      IF(INDXS(IS).NE.3)GOTO 1000

C Fill the temporary lines arrays which differs from the original ones 
C in that in the temporary ones all the co-linear lines will be one line.
        DO 10 II=1,NLINB(IS)
          NVLNT(II)=NVLN(IS,II)
          ILINBT(II,1)=ILINB(IS,II,1)
          ILINBT(II,2)=ILINB(IS,II,2)
          DO 11 IIV=1,NVLNT(II)
            IVLNT(II,IIV)=IVLN(IS,II,IIV)
   11     CONTINUE
          DO 12 IIV=NVLNT(II)+1,MLS
            IVLNT(II,IIV)=0
   12     CONTINUE
   10   CONTINUE
        DO 15 II=NLINB(IS)+1,MLS
          NVLNT(II)=0
        DO 15 IIV=1,MLS
          IVLNT(II,IIV)=0
   15   CONTINUE

C Join co-linear lines.
        I=0
        NLINT=NLINB(IS)
        INBD=NLINB(IS)
        DO 20 I2=1,NLINT
          I=I+1
          IV12=ILINBT(I,2)
          DO 30 J=1,INBD
            IF(I.EQ.J)GOTO 30
            IV21=ILINBT(J,1)
            IF(IV21.EQ.IV12)THEN
              IV11=ILINBT(I,1)
              IV22=ILINBT(J,2)
              CALL ANGCOS(IV11,IV12,IV22,THETA)
              IF(ABS(THETA-180.).LT.9..AND.NCVS(IS,IVLNT(J,1)).EQ.1)THEN
                NCVS(IS,IVLNT(J,1))=-1
                DO 40 JV=2,NVLNT(J)
                  NVLNT(I)=NVLNT(I)+1
                  IVLNT(I,NVLNT(I))=IVLNT(J,JV)
   40           CONTINUE
                ILINBT(I,2)=ILINBT(J,2)
                INBD=INBD-1
                DO 50 II=J,INBD
                  ILINBT(II,1)=ILINBT(II+1,1)
                  ILINBT(II,2)=ILINBT(II+1,2)
                  NVLNT(II)=NVLNT(II+1)
                  DO 55 IIV=1,NVLNT(II)
                    IVLNT(II,IIV)=IVLNT(II+1,IIV)
   55             CONTINUE
   50           CONTINUE
                I=I-1
                GOTO 20
              ENDIF
            ENDIF
   30     CONTINUE
   20   CONTINUE
        DO 60 IL=NLINB(IS)+1,NLINA(IS)
          ILD=IL-(NLINT-INBD)
          NVLNT(ILD)=NVLN(IS,IL)
          DO 70 IIV=1,NVLNT(ILD)
            IVLNT(ILD,IIV)=IVLN(IS,IL,IIV)
   70     CONTINUE
   60   CONTINUE
        NLINT=NLINA(IS)-(NLINT-INBD)

C Create the control volumes for this surface.
C Initialize the control volume counter.
      ICVS=0
      NCVSF(IS)=ICVS
      DO 100 IV=1,NTINT(IS)
        NCVIV=NCVS(IS,IV)

C Vertices with NCVS=-1 will be bypassed.
        DO 110 IIV=1,NCVIV

C Initialize the index for the lines which will be excluded from the
C search.
          DO 120 I=1,3
            ILEX(I)=0
  120     CONTINUE

C Search for the first line segment.
          CALL BAVRTX(1,IS,IV,ILEX,NLIN,ILIN,INMV,IVX)
          IF(NLIN(1).EQ.0)GOTO 100
          DO 140 L2=1,NLIN(1)

C Save the first line segment number.
            LL2=ILIN(1,L2)

C line segment type.
            IF(LL2.LE.INBD)THEN
              ILTYP(1)=1
            ELSE
              ILTYP(1)=0
            ENDIF
            IF(INMV(1,L2).EQ.0)GOTO 140

C Illegal combination of vertices (i.e, at least one vertex is not 
C available for creating new control volumes).
            DO 150 ICHK=1,INMV(1,L2)
              IV2=IVX(1,L2,ICHK)
              IF(NCVS(IS,IV2).EQ.0)GOTO 140
  150       CONTINUE
            ILEX(1)=LL2

C Search for the second line segment.
            CALL BAVRTX(2,IS,IV2,ILEX,NLIN,ILIN,INMV,IVX)
            IF(NLIN(2).EQ.0)GOTO 140
            DO 160 L3=1,NLIN(2)
              LL3=ILIN(2,L3)
              IF(LL3.LE.INBD)THEN
                ILTYP(2)=1
              ELSE
                ILTYP(2)=0
              ENDIF
              IF(INMV(2,L3).EQ.0)GOTO 160
              DO 170 ICHK=1,INMV(2,L3)
                IV3=IVX(2,L3,ICHK)
                IF(NCVS(IS,IV3).EQ.0)GOTO 160
  170         CONTINUE
              ILEX(2)=LL3

C Search for the third line segment.
              CALL BAVRTX(3,IS,IV3,ILEX,NLIN,ILIN,INMV,IVX)
              IF(NLIN(3).EQ.0)GOTO 160
              DO 180 L4=1,NLIN(3)
                LL4=ILIN(3,L4)
                IF(LL4.LE.INBD)THEN
                  ILTYP(3)=1
                ELSE
                  ILTYP(3)=0
                ENDIF
                IF(INMV(3,L4).EQ.0)GOTO 180
                DO 190 ICHK=1,INMV(3,L4)
                  IV4=IVX(3,L4,ICHK)
                  IF(NCVS(IS,IV4).EQ.0)GOTO 180
  190           CONTINUE
                ILEX(3)=LL4

C Search for the fourth line segment.
                CALL BAVRTX(4,IS,IV4,ILEX,NLIN,ILIN,INMV,IVX)
                IF(NLIN(4).EQ.0)GOTO 180
                DO 200 L5=1,NLIN(4)
                  LL5=ILIN(4,L5)
                  IF(LL5.LE.INBD)THEN
                    ILTYP(4)=1
                  ELSE
                    ILTYP(4)=0
                  ENDIF
                  IF(INMV(4,L5).EQ.0)GOTO 200
                  DO 210 ICHK=1,INMV(4,L5)
                    IV5=IVX(4,L5,ICHK)
                    IF(NCVS(IS,IV5).EQ.0)GOTO 200
  210             CONTINUE
                  IF(IV5.NE.IV)GOTO 200


C Bypass the control volume if all its lines are boundary lines 
C (i.e., this control volume does not belong to this surface).
                  IF(ILTYP(1).EQ.1.AND.ILTYP(2).EQ.1.AND.ILTYP(3).EQ.
     &               1.AND.ILTYP(4).EQ.1.AND.INBD.GT.4)GOTO 200

C Check if this control volume is already defined.
                  DO 220 ICHCK=1,NCVSF(IS)
                    NICHK=0
                    DO 230 IR1=1,NCVV(IS,ICHCK)
                      IVCK=ICVV(IS,ICHCK,IR1)
                      IF(IVCK.EQ.IV.OR.IVCK.EQ.IV2.OR.IVCK.EQ.IV3.OR.
     &                   IVCK.EQ.IV4)NICHK=NICHK+1
  230               CONTINUE
                    IF(NICHK.EQ.4)GOTO 200
  220             CONTINUE

C New control volume is found.
                  ICVS=ICVS+1

C Total number of vertices in the suggested control volume.
                  NTOTV=1
                  ICVV(IS,ICVS,1)=IV
                  IF(NCVS(IS,IV).GT.0)THEN
                    NCVS(IS,IV)=NCVS(IS,IV)-1
                  ELSEIF(NCVS(IS,IV).EQ.-1)THEN
                    NCVS(IS,IV)=0
                  ELSE
                    STOP "error (601): surfaces element generation."
                  ENDIF
                  DO 250 IDO=1,INMV(1,L2)
                    NTOTV=NTOTV+1
                    IVDO=IVX(1,L2,IDO)
                    ICVV(IS,ICVS,NTOTV)=IVDO
                    IF(NCVS(IS,IVDO).GT.0)THEN
                      NCVS(IS,IVDO)=NCVS(IS,IVDO)-1
                    ELSEIF(NCVS(IS,IVDO).EQ.-1)THEN
                      NCVS(IS,IVDO)=0
                    ELSE
                      STOP "error (602): surfaces element generation."
                    ENDIF
  250             CONTINUE
                  DO 260 IDO=1,INMV(2,L3)
                    NTOTV=NTOTV+1
                    IVDO=IVX(2,L3,IDO)
                    ICVV(IS,ICVS,NTOTV)=IVDO
                    IF(NCVS(IS,IVDO).GT.0)THEN
                      NCVS(IS,IVDO)=NCVS(IS,IVDO)-1
                    ELSEIF(NCVS(IS,IVDO).EQ.-1)THEN
                      NCVS(IS,IVDO)=0
                    ELSE
                      STOP "error (603): surfaces element generation."
                    ENDIF
  260             CONTINUE
                  DO 270 IDO=1,INMV(3,L4)
                    NTOTV=NTOTV+1
                    IVDO=IVX(3,L4,IDO)
                    ICVV(IS,ICVS,NTOTV)=IVDO
                    IF(NCVS(IS,IVDO).GT.0)THEN
                      NCVS(IS,IVDO)=NCVS(IS,IVDO)-1
                    ELSEIF(NCVS(IS,IVDO).EQ.-1)THEN
                      NCVS(IS,IVDO)=0
                    ELSE
                      STOP "error (604): surfaces element generation."
                    ENDIF
  270             CONTINUE
                  DO 280 IDO=1,INMV(4,L5)-1
                    NTOTV=NTOTV+1
                    IVDO=IVX(4,L5,IDO)
                    ICVV(IS,ICVS,NTOTV)=IVDO
                    IF(NCVS(IS,IVDO).GT.0)THEN
                      NCVS(IS,IVDO)=NCVS(IS,IVDO)-1
                    ELSEIF(NCVS(IS,IVDO).EQ.-1)THEN
                      NCVS(IS,IVDO)=0
                    ELSE
                      STOP "error (605): surfaces element generation."
                    ENDIF
  280             CONTINUE
                  NCVV(IS,ICVS)=NTOTV
                  NCVSF(IS)=ICVS
                  GOTO 160
  200           CONTINUE
  180         CONTINUE
  160       CONTINUE
  140     CONTINUE
  110   CONTINUE
  100 CONTINUE

C Conform that all the available control volumes have been defined.
      DO 300 ICHK=1,NTINT(IS)
        IF(NCVS(IS,ICHK).GT.0)
     &  STOP "error (606): surfaces element generation."
  300 CONTINUE
 1000 CONTINUE
      RETURN
      END

C *****************************  BAVRTX  ******************************
C BAVRTX defines the line number within which the required vertex was 
C found. It also returns the number of the vertex before and after it.
C *********************************************************************
        SUBROUTINE BAVRTX(ISCH,IS,IV,ILEX,NLIN,ILIN,INMV,IVX)
#include "building.h"

      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)
      COMMON/GR3D45/NLINT,NVLNT(MLS),IVLNT(MLS,MLS),ILINBT(MLBS,2)

      DIMENSION ILEX(3),NLIN(4),ILIN(4,6)
      DIMENSION IVXT(MLS),INMV(4,6),IVX(4,6,MLS)

      ICLL=0
      DO 10 ILL=1,NLINT
        DO 20 ILS=1,3
          IF(ILL.EQ.ILEX(ILS))GOTO 10
   20   CONTINUE
        DO 30 IVV=1,NVLNT(ILL)
          IVRT=IVLNT(ILL,IVV)
          IF(IVRT.EQ.IV)THEN
            IVNM=0
            IF(IVV.EQ.1)THEN
              ICLL=ICLL+1
              ILIN(ISCH,ICLL)=ILL
              DO 40 IVV2=2,NVLNT(ILL)
                IVRT2=IVLNT(ILL,IVV2)
                IVNM=IVNM+1
                IVX(ISCH,ICLL,IVNM)=IVRT2
                IF(NCVS(IS,IVRT2).NE.-1)THEN
                  INMV(ISCH,ICLL)=IVNM
                  GOTO 10
                ENDIF
   40         CONTINUE
              STOP "error (607): surfaces element generation."
            ELSEIF(IVV.EQ.NVLNT(ILL))THEN
              ICLL=ICLL+1
              ILIN(ISCH,ICLL)=ILL
              DO 50 IVV2=NVLNT(ILL)-1,1,-1
                IVRT2=IVLNT(ILL,IVV2)
                IVNM=IVNM+1
                IVX(ISCH,ICLL,IVNM)=IVRT2
                IF(NCVS(IS,IVRT2).NE.-1)THEN
                  INMV(ISCH,ICLL)=IVNM
                  GOTO 10
                ENDIF
   50         CONTINUE
              STOP "error (608): surfaces element generation."
            ELSE
              DO 60 IVV2=IVV+1,NVLNT(ILL)
                IVRT2=IVLNT(ILL,IVV2)
                IVNM=IVNM+1
                IVXT(IVNM)=IVRT2
                IF(NCVS(IS,IVRT2).NE.-1)THEN
                  ICLL=ICLL+1
                  ILIN(ISCH,ICLL)=ILL
                  INMV(ISCH,ICLL)=IVNM
                  DO 70 ITMP=1,IVNM
                    IVX(ISCH,ICLL,ITMP)=IVXT(ITMP)
   70             CONTINUE
                  GOTO 80
                ENDIF
   60         CONTINUE
   80         IVNM=0
              DO 90 IVV2=IVV-1,1,-1
                IVRT2=IVLNT(ILL,IVV2)
                IVNM=IVNM+1
                IVXT(IVNM)=IVRT2
                IF(NCVS(IS,IVRT2).NE.-1)THEN
                  ICLL=ICLL+1
                  ILIN(ISCH,ICLL)=ILL
                  INMV(ISCH,ICLL)=IVNM
                  DO 100 ITMP=1,IVNM
                    IVX(ISCH,ICLL,ITMP)=IVXT(ITMP)
  100             CONTINUE
                  GOTO 10
                ENDIF
   90         CONTINUE
            ENDIF
          ENDIF
   30   CONTINUE
   10 CONTINUE
      NLIN(ISCH)=ICLL
      RETURN
      END

C *****************************   CZCNVS   ****************************
C CZCNVS controls the generation of zone's nodes, control volumes, 
C and connections.
C *********************************************************************
      SUBROUTINE CZCNVS(IZ,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)
      COMMON/GR3D44/NCVSF(MS),NCVV(MS,MCVS),ICVV(MS,MCVS,MCVV)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D81/ISNDS(MS),ISNDE(MEZ),ISNDC(MCZ)
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical closemat1,closemat2

      CHARACTER MSG*72,FLTYP*25,outs*124
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LTMP

      DIMENSION XCRD(MGV),ZCRD(MGV)
      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CZCNVS'  ! set for subroutine

C If materials database not opened warn the user.
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('No readable material database was found.',
     &              'check other warnings for advise.','W')
        ier=2
        return
      endif

C Initialize the connections,nodes, and control volumes counters.
      NNDS=0
      NCNV=0
      IR=0

C Setup help text.
      helptopic='grd_CZCNVS_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Open the control volumes file.
      FLTYP='control volumes'
      LTMP=L3DCVS(IZ)
      CALL FLNAME(IZ,LTMP,FLTYP,'.cvs',3,IER)
      L3DCVS(IZ)=LTMP
      ier=0
      CALL EFOPRAN(ICVS,L3DCVS(IZ),5,4,IER)
      IF(IER.NE.0)RETURN

C Open the connections file.
      FLTYP='connections'
      LTMP=L3DCNC(IZ)
      CALL FLNAME(IZ,LTMP,FLTYP,'.cnc',3,IER)
      L3DCNC(IZ)=LTMP
      ier=0
      CALL EFOPRAN(ICNC,L3DCNC(IZ),7,4,IER)
      IF(IER.NE.0)RETURN

C Create a binary file for saving the nodes data (location and the 
C numbers of the control volumes represented by each node).
      FLTYP='nodes coordintes'
      LTMP=L3DNDC(IZ)
      CALL FLNAME(IZ,LTMP,FLTYP,'.ndc',3,IER)
      L3DNDC(IZ)=LTMP
      ier=0
      CALL EFOPRAN(INDC,L3DNDC(IZ),3,4,IER)
      IF(IER.NE.0)RETURN

C Create a binary file for saving the nodes initial temperatures.
      FLTYP='nodes temperature'
      LTMP=L3DTAQ(IZ)
      CALL FLNAME(IZ,LTMP,FLTYP,'.taq',3,IER)
      L3DTAQ(IZ)=LTMP
      ier=0
      CALL EFOPRAN(ITAQ,L3DTAQ(IZ),2,4,IER)
      IF(IER.NE.0)RETURN

C Define the degree of boundary nodes modelling.
      insidx=2
      CALL EASKI(INSIDX,' ',
     &  'Define the degree of boundary nodes modelling',
     &  1,'F',2,'F',2,' modelling degree ',IER,nbhelp)

C Create the nodes ,and control volumes for connection surfaces 
C ( srf-srf, srf-edge, edge-edge, and edge-crn ).
      CALL CNCSEC

C Create the nodes, control volumes, and connections for the building
C elements (corners, edges, and surfaces).

C FIRST,create the nodes, control volumes, and connections for the
C corners.
      ICOUNT=0
      DO 290 ICR=1,NUMCRN
        IF(INDXC(ICR).EQ.1.OR.INDXC(ICR).EQ.3)ICOUNT=ICOUNT+1
  290 CONTINUE
      IF(ICOUNT.GT.0)CALL EDISP(IUOUT,' Gridding the corners.')
      DO 300 ICR=1,NUMCRN

C Save the numbers of the first node in the current corner
        ISNDC(ICR)=NNDS+1

C Lumped corner.
        IF(INDXC(ICR).EQ.1)THEN
          CALL LMPCRN(IZ,ICR)

C Discretized corner.
        ELSEIF(INDXC(ICR).EQ.3)THEN
          CALL DISCRN(IZ,ICR)

C Not defined corner.
        ELSE
          GOTO 300
        ENDIF
  300 CONTINUE

C SECOND,create the nodes, control volumes, and connections for the
C edges.
      ICOUNT=0
      DO 190 III=1,NUMEDG
        IF(INDXE(III).EQ.1.OR.INDXE(III).EQ.3)ICOUNT=ICOUNT+1
  190 CONTINUE
      IF(ICOUNT.GT.0)CALL EDISP(IUOUT,' Gridding the edges.')
      DO 200 IEG=1,NUMEDG

C Save the numbers of the first node in the current edge.
        ISNDE(IEG)=NNDS+1

C Lumped edge.
        IF(INDXE(IEG).EQ.1)THEN
          CALL LMPEDG(IZ,IEG)

C Discretized edge.
        ELSEIF(INDXE(IEG).EQ.3)THEN
          CALL DISEDG(IZ,IEG)

C Not defined edge.
        ELSE
          GOTO 200
        ENDIF
  200 CONTINUE

C THIRD, the surfaces.
      CALL EDISP(IUOUT,' Gridding the surfaces.')
      DO 100 IS=1,NSUR

C Save the number of the first node in the current surface.
        ISNDS(IS)=NNDS+1

C Discretized surface.
        IF(INDXS(IS).EQ.3)THEN
          DO 110 ICV=1,NCVSF(IS)

C Define node location.
            NCRD=NCVV(IS,ICV)
            DO 120 I=1,NCRD
              IV=ICVV(IS,ICV,I)
              XCRD(I)=XINT(IS,IV)
              ZCRD(I)=ZINT(IS,IV)
  120       CONTINUE
            CALL AREACN(NCRD,XCRD,ZCRD,AREC,DISX,DISZ)
            CALL CVNODE(IS,AREC,DISX,DISZ)
  110     CONTINUE

C Lumped surface.
        ELSE

C Calculate the area of the current surface.
          NCRD=NVRTB(IS)
          DO 150 I=1,NCRD
              XCRD(I)=XVRT(IS,I)
              ZCRD(I)=ZVRT(IS,I)
  150     CONTINUE
          CALL AREACN(NCRD,XCRD,ZCRD,AREC,DISX,DISZ)
          CALL CVNODE(IS,AREC,DISX,DISZ)
        ENDIF
  100 CONTINUE

C Create the surface connections.
      CALL CNCSRF(IZ,INSIDX)
      DO 500 ITQ=1,NNDS+1
        WRITE(ITAQ,REC=ITQ)0.0,15.0
  500 CONTINUE
      WRITE(ITAQ,REC=NNDS+2)0.0,0.0
      CALL EDISP(IUOUT,' Gridding process completed successfully.')
      CALL EDISP(IUOUT,' ')
      WRITE(MSG,'(A,I7)')' Total number of nodes (excl. air):',NNDS
      CALL EDISP(IUOUT,MSG)
      WRITE(MSG,'(A,I7)')' Total number of control volumes:  ',NCNV
      CALL EDISP(IUOUT,MSG)
      WRITE(MSG,'(A,I7)')' Total number of connections:      ',IR
      CALL EDISP(IUOUT,MSG)
      CALL ERPFREE(ICVS,ISTAT)
      CALL ERPFREE(ICNC,ISTAT)
      CALL ERPFREE(INDC,ISTAT)
      CALL ERPFREE(ITAQ,ISTAT)
      CALL ERPFREE(IFMAT,ISTAT)

      MATDBOK=.FALSE.
      RETURN
      END

C ****************************   CNCSEC   *****************************
C CNCSEC creates the nodes ,and control volumes for connection surfaces
C ( srf-srf,srf-edge, edge-edge, and edge-crn ).
C *********************************************************************
      SUBROUTINE CNCSEC
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXCV(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYCV(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZCV(MEZ,MZE)
      COMMON/GR3D42/NVLN(MS,MLS),IVLN(MS,MLS,MLS)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D52/NDZSS(MSSZ),DZSS(MSSZ,MZSS)
      COMMON/GR3D53/NDYSS(MSSZ),DYSS(MSSZ,MYSS)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D57/NDZSE(MSEZ),DZSE(MSEZ,MZSE)
      COMMON/GR3D58/NDYSE(MSEZ),DYSE(MSEZ,MYSE)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D62/NDXEE(MEEZ),DXEE(MEEZ,MXEE)
      COMMON/GR3D63/NDYEE(MEEZ),DYEE(MEEZ,MYEE)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D67/NDXEC(MECZ),DXEC(MECZ,MXEC)
      COMMON/GR3D68/NDYEC(MECZ),DYEC(MECZ,MYEC)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D82/ISNDSS(MSSZ),ISNDSE(MSEZ),ISNDEE(MEEZ),ISNDEC(MECZ)
      COMMON/GR3D90/DCOSSS(MSSZ,3,3),DCOSSE(MSEZ,3,3)
      COMMON/GR3D91/DCOSEE(MEEZ,3,3),DCOSEC(MECZ,3,3)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      DIMENSION XP(3),YP(3),ZP(3)

C Set the default values for massless nodes.
      DNHC=0.

C FIRST, create the massles nodes at the surface-surface connections.
      ISP1=-4
      DISX=0.
      DO 10 ISS=1,NUMSS

C Bypass if this surface-surface connection is not defined.
        IF(INDXSS(ISS).NE.3)GOTO 10
        IS1=IBDYSS(ISS,1)
        IS2=IBDYSS(ISS,2)
        CALL LINORM(IVRTSS(ISS,1),IVRTSS(ISS,2),ZP)
        DO 20 I123=1,3
          YP(I123)=DCOSS(IS1,2,I123)
   20   CONTINUE
        CALL CROSS(YP,ZP,XP)
        DIST=SQRT(XP(1)**2+XP(2)**2+XP(3)**2)
        DO 30 I123=1,3
          DCOSSS(ISS,1,I123)=XP(I123)/DIST
          DCOSSS(ISS,2,I123)=YP(I123)
          DCOSSS(ISS,3,I123)=ZP(I123)
   30   CONTINUE
        ISNDSS(ISS)=NNDS+1
        NDYSS(ISS)=1
        DYSS(ISS,1)=Y0SS(ISS)-Y1SS(ISS)

C Fill the Y-intervals associated with the first surface.
        YVALU=Y1S(IS1)-Y1SS(ISS)
        CALL ADDSSY(ISS,YVALU)
        DO 40 IL1=NE(IS1),1,-1
          YVALU=YVALU+THK(IS1,IL1)
          CALL ADDSSY(ISS,YVALU)
   40   CONTINUE

C Fill the Z-intervals associated with the first surface.
        ILB1=ILNBSS(ISS,1)

C Discretized surface.
        IF(INDXS(IS1).EQ.3)THEN
          ILABS=ABS(ILB1)
          IV1=IVLN(IS1,ILABS,1)
          IV9=IVLN(IS1,ILABS,NVLN(IS1,ILABS))
          NDZSS(ISS)=1
          DZSS(ISS,1)=SQRT((XINT(IS1,IV9)-XINT(IS1,IV1))**2+
     &                     (ZINT(IS1,IV9)-ZINT(IS1,IV1))**2)
          IF(ILB1.GT.0)THEN
            IV1=IVLN(IS1,ILABS,1)
            DO 50 IVL=2,NVLN(IS1,ILABS)
              IV2=IVLN(IS1,ILABS,IVL)
              ZVALU=SQRT((XINT(IS1,IV2)-XINT(IS1,IV1))**2+
     &                   (ZINT(IS1,IV2)-ZINT(IS1,IV1))**2)
              CALL ADDSSZ(ISS,ZVALU)
   50       CONTINUE
          ELSEIF(ILB1.LT.0)THEN
            IV1=IVLN(IS1,ILABS,NVLN(IS1,ILABS))
            DO 60 IVL=NVLN(IS1,ILABS)-1,1,-1
              IV2=IVLN(IS1,ILABS,IVL)
              ZVALU=SQRT((XINT(IS1,IV2)-XINT(IS1,IV1))**2+
     &                   (ZINT(IS1,IV2)-ZINT(IS1,IV1))**2)
              CALL ADDSSZ(ISS,ZVALU)
   60       CONTINUE
          ENDIF

C Lumped surface.
        ELSE
          CALL USRMSG(' lumped surface connected by',
     &                'a surface-surface connection surface.','F')
        ENDIF

C Add the Y-intervals associated with the second surface.
        YVALU=Y1S(IS2)-Y1SS(ISS)
        CALL ADDSSY(ISS,YVALU)
        DO 70 IL2=NE(IS2),1,-1
          YVALU=YVALU+THK(IS2,IL2)
          CALL ADDSSY(ISS,YVALU)
   70   CONTINUE

C Add the additional Z-intervals associated with the second surface.
        ILB2=ILNBSS(ISS,2)

C Discretized surface.
        IF(INDXS(IS2).EQ.3)THEN
          ILABS=ABS(ILB2)
          IF(ILB2.GT.0)THEN
            IV1=IVLN(IS2,ILABS,1)
            DO 80 IVL=2,NVLN(IS2,ILABS)
              IV2=IVLN(IS2,ILABS,IVL)
              ZVALU=SQRT((XINT(IS2,IV2)-XINT(IS2,IV1))**2+
     &                   (ZINT(IS2,IV2)-ZINT(IS2,IV1))**2)
              CALL ADDSSZ(ISS,ZVALU)
   80       CONTINUE
          ELSEIF(ILB2.LT.0)THEN
            IV1=IVLN(IS2,ILABS,NVLN(IS2,ILABS))
            DO 90 IVL=NVLN(IS2,ILABS)-1,1,-1
              IV2=IVLN(IS2,ILABS,IVL)
              ZVALU=SQRT((XINT(IS2,IV2)-XINT(IS2,IV1))**2+
     &                   (ZINT(IS2,IV2)-ZINT(IS2,IV1))**2)
              CALL ADDSSZ(ISS,ZVALU)
   90       CONTINUE
          ENDIF

C Lumped surface.
        ELSE
          CALL USRMSG(' lumped surface connected by',
     &                'a surface-surface connection surface.','F')
        ENDIF

C Create the massless nodes associated with this srf-srf connection.
        Z2=0.
        DO 100 IDZ=1,NDZSS(ISS)
          Z1=Z2
          Z2=DZSS(ISS,IDZ)
          DELZ=Z2-Z1
          DISZ=(Z1+Z2)/2.
          Y2=0.
        DO 100 IY=1,NDYSS(ISS)
          Y1=Y2
          Y2=DYSS(ISS,IY)
          DELY=Y2-Y1
          DISY=(Y1+Y2)/2.
          CALL GCOORD(ISP1,ISS,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
          VOLM=DELZ*DELY
          NCNV=NCNV+1
          NNDS=NNDS+1
          WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,ISS
          WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
  100   CONTINUE
   10 CONTINUE


C SECOND, create the massles nodes at the srf-edg connections.
      ISP1=-5
      DISX=0.
      DO 210 ISE=1,NUMSE

C Bypass if this surface-edge connection is not defined.
        IF(INDXSE(ISE).NE.3)GOTO 210
        IS=IBDYSE(ISE,1)
        IEG=IBDYSE(ISE,2)
        IF(IS.EQ.ISFEDG(IEG,1))THEN
          DO 215 I123=1,3
            DCOSSE(ISE,1,I123)=DCOSE(IEG,2,I123)
            DCOSSE(ISE,2,I123)=DCOSE(IEG,1,I123)
            DCOSSE(ISE,3,I123)=DCOSE(IEG,3,I123)
  215     CONTINUE
        ELSE
          DO 216 I123=1,3
          DO 216 IXYZ=1,3
            DCOSSE(ISE,IXYZ,I123)=DCOSE(IEG,IXYZ,I123)
  216     CONTINUE
        ENDIF

C Save the number of the first massless node in this srf-edge connection.
        ISNDSE(ISE)=NNDS+1
        NDYSE(ISE)=1
        DYSE(ISE,1)=Y0SE(ISE)-Y1SE(ISE)

C First, fill the Y-intervals associated with the surface.
        YVALU=Y1S(IS)-Y1SE(ISE)
        CALL ADDSEY(ISE,YVALU)
        DO 220 IL=NE(IS),1,-1
          YVALU=YVALU+THK(IS,IL)
          CALL ADDSEY(ISE,YVALU)
  220   CONTINUE

C Fill the Z-intervals associated with the surface.
        ILB=ILNBSE(ISE)

C Discretized surface.
        IF(INDXS(IS).EQ.3)THEN
          ILABS=ABS(ILB)
          IV1=IVLN(IS,ILABS,1)
          IV9=IVLN(IS,ILABS,NVLN(IS,ILABS))
          NDZSE(ISE)=1
          DZSE(ISE,1)=SQRT((XINT(IS,IV9)-XINT(IS,IV1))**2+
     &                     (ZINT(IS,IV9)-ZINT(IS,IV1))**2)
          IF(ILB.GT.0)THEN
            IV1=IVLN(IS,ILABS,1)
            DO 230 IVL=2,NVLN(IS,ILABS)
              IV2=IVLN(IS,ILABS,IVL)
              ZVALU=SQRT((XINT(IS,IV2)-XINT(IS,IV1))**2+
     &                   (ZINT(IS,IV2)-ZINT(IS,IV1))**2)
              CALL ADDSEZ(ISE,ZVALU)
  230       CONTINUE
          ELSEIF(ILB.LT.0)THEN
            IV1=IVLN(IS,ILABS,NVLN(IS,ILABS))
            DO 240 IVL=NVLN(IS,ILABS)-1,1,-1
              IV2=IVLN(IS,ILABS,IVL)
              ZVALU=SQRT((XINT(IS,IV2)-XINT(IS,IV1))**2+
     &                   (ZINT(IS,IV2)-ZINT(IS,IV1))**2)
              CALL ADDSEZ(ISE,ZVALU)
  240       CONTINUE
          ENDIF

C Lumped surface.
        ELSE
          CALL USRMSG(' lumped surface connected by',
     &                'a surface-edge connection surface.','F')
        ENDIF
C Add the additional Y-intervals associated with the edge.
C Discretized edge.
        IF(INDXE(IEG).EQ.3)THEN
          IF(IS.EQ.ISFEDG(IEG,1))THEN
            YVALU=0.
            DO 250 IX=1,NDXE(IEG)
              YVALU=YVALU+DXE(IEG,IX)
              CALL ADDSEY(ISE,YVALU)
  250       CONTINUE
          ELSEIF(IS.EQ.ISFEDG(IEG,2))THEN
            YVALU=0.
            DO 260 IY=1,NDYE(IEG)
              YVALU=YVALU+DYE(IEG,IY)
              CALL ADDSEY(ISE,YVALU)
  260       CONTINUE
          ELSE
            CALL USRMSG(' ',
     &                'check edges and connection surfaces data.','F')
          ENDIF

C Not defined edge.
        ELSEIF(INDXE(IEG).NE.1)THEN
          CALL USRMSG(' un-defined edge connected by',
     &                'a surface-edge connection surface.','F')
        ENDIF
C Add the additional Z-intervals associated with the edge.
C Discretized edge.
        IF(INDXE(IEG).EQ.3)THEN
          ZVALU=0.
          DO 270 IDZ=1,NDZE(IEG)
            ZVALU=ZVALU+DZE(IEG,IDZ)
            CALL ADDSEZ(ISE,ZVALU)
  270     CONTINUE

C Not defined edge.
        ELSEIF(INDXE(IEG).NE.1)THEN
          CALL USRMSG(' un-defined edge connected by',
     &                'a surface-edge connection surface.','F')
        ENDIF

C Create the massless nodes associated with this srf-edge connection.
        Z2=0.
        DO 280 IDZ=1,NDZSE(ISE)
          Z1=Z2
          Z2=DZSE(ISE,IDZ)
          DELZ=Z2-Z1
          DISZ=(Z1+Z2)/2.
          Y2=0.
        DO 280 IY=1,NDYSE(ISE)
          Y1=Y2
          Y2=DYSE(ISE,IY)
          DELY=Y2-Y1
          DISY=(Y1+Y2)/2.
          CALL GCOORD(ISP1,ISE,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
          VOLM=DELZ*DELY
          NCNV=NCNV+1
          NNDS=NNDS+1
          WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,ISE
          WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
  280   CONTINUE
  210 CONTINUE

C THIRD, define the massless nodes at the interface of co-linear edges.
      ISP1=-6
      DISZ=0.
      DO 310 IEE=1,NUMEE
        IF(INDXEE(IEE).NE.3)GOTO 310
        IEG1=IBDYEE(IEE,1)
        IEG2=IBDYEE(IEE,2)
        DO 315 IXYZ=1,3
        DO 315 I123=1,3
          DCOSEE(IEE,IXYZ,I123)=DCOSE(IEG1,IXYZ,I123)
  315   CONTINUE

C Fill the X and Y-intervals associated with edge (1).
        ISNDEE(IEE)=NNDS+1
        XVALU=0.
        YVALU=0.
        DO 320 IX1=1,NDXE(IEG1)
          XVALU=XVALU+DXE(IEG1,IX1)
  320   CONTINUE
        NDXEE(IEE)=1
        DXEE(IEE,1)=XVALU
        DO 330 IY1=1,NDYE(IEG1)
          YVALU=YVALU+DYE(IEG1,IY1)
  330   CONTINUE
        NDYEE(IEE)=1
        DYEE(IEE,1)=YVALU
        XVALU=0.
        YVALU=0.

C Discretized edge.
        IF(INDXE(IEG1).EQ.3)THEN
          DO 340 IX1=1,NDXE(IEG1)
            XVALU=XVALU+DXE(IEG1,IX1)
            CALL ADDEEX(IEE,XVALU)
  340     CONTINUE
          DO 350 IY1=1,NDYE(IEG1)
            YVALU=YVALU+DYE(IEG1,IY1)
            CALL ADDEEY(IEE,YVALU)
  350     CONTINUE

C Not defined edge.
        ELSEIF(INDXE(IEG1).NE.1)THEN
          CALL USRMSG(' un-defined edge connected by',
     &                'an edge-edge connection surface.','F')
        ENDIF

C Add the X and Y-intervals associated with edge 2.
        XVALU=0.
        YVALU=0.

C Discretized edge.
        IF(IEG2.GT.0.AND.INDXE(IEG2).EQ.3)THEN
          DO 360 IX2=1,NDXE(IEG2)
            XVALU=XVALU+DXE(IEG2,IX2)
            CALL ADDEEX(IEE,XVALU)
  360     CONTINUE
          DO 370 IY2=1,NDYE(IEG2)
            YVALU=YVALU+DYE(IEG2,IY2)
            CALL ADDEEY(IEE,YVALU)
  370     CONTINUE
        ELSEIF(IEG2.LT.0.AND.INDXE(-IEG2).EQ.3)THEN
          IEG2=-IEG2
          DO 380 IY2=1,NDYE(IEG2)
            XVALU=XVALU+DYE(IEG2,IY2)
            CALL ADDEEX(IEE,XVALU)
  380     CONTINUE
          DO 390 IX2=1,NDXE(IEG2)
            YVALU=YVALU+DXE(IEG2,IX2)
            CALL ADDEEY(IEE,YVALU)
  390     CONTINUE

C Not defined edge.
        ELSEIF(INDXE(ABS(IEG2)).NE.1)THEN
          CALL USRMSG(' un-defined edge connected by',
     &                'an edge-edge connection surface.','F')
        ENDIF

C Create the massless node associated with the current edg-edg connection.
        Y2=0.
        DO 400 IY=1,NDYEE(IEE)
          Y1=Y2
          Y2=DYEE(IEE,IY)
          DELY=Y2-Y1
          DISY=(Y1+Y2)/2.
          X2=0.
        DO 400 IX=1,NDXEE(IEE)
          X1=X2
          X2=DXEE(IEE,IX)
          DELX=X2-X1
          DISX=(X1+X2)/2.
          CALL GCOORD(ISP1,IEE,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
          VOLM=DELY*DELX
          NCNV=NCNV+1
          NNDS=NNDS+1
          WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,IEE
          WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
  400   CONTINUE
  310 CONTINUE

C FOURTH,define the massless nodes at the interface of edge-corner.
      ISP1=-7
      DISZ=0.
      DO 510 IEC=1,NUMEC
        IF(INDXEC(IEC).NE.3)GOTO 510
        IEG=IBDYEC(IEC,1)
        ICR=IBDYEC(IEC,2)
        ICN=ABS(ICR)
        DO 515 IXYZ=1,3
        DO 515 I123=1,3
          DCOSEC(IEC,IXYZ,I123)=DCOSE(IEG,IXYZ,I123)
  515   CONTINUE

C Fill the X and Y-intervals associated with the edge.
        ISNDEC(IEC)=NNDS+1
        XVALU=0.
        YVALU=0.
        DO 520 IX=1,NDXE(IEG)
          XVALU=XVALU+DXE(IEG,IX)
  520   CONTINUE
        DXEC(IEC,1)=XVALU
        NDXEC(IEC)=1
        DO 530 IY=1,NDYE(IEG)
          YVALU=YVALU+DYE(IEG,IY)
  530   CONTINUE
        DYEC(IEC,1)=YVALU
        NDYEC(IEC)=1
        XVALU=0.
        YVALU=0.

C Discretized edge.
        IF(INDXE(IEG).EQ.3)THEN
          DO 540 IX=1,NDXE(IEG)
            XVALU=XVALU+DXE(IEG,IX)
            CALL ADDECX(IEC,XVALU)
  540     CONTINUE
          DO 550 IY=1,NDYE(IEG)
            YVALU=YVALU+DYE(IEG,IY)
            CALL ADDECY(IEC,YVALU)
  550     CONTINUE

C Not defined edge.
        ELSEIF(INDXE(IEG).NE.1)THEN
          CALL USRMSG(' un-defined edge connected by',
     &                'an edge-edge connection surface.','F')
        ENDIF

C Discretized corner.
        IF(INDXC(ICN).EQ.3)THEN

C Add the X -intervals associated with the corner.
          XVALU=0.
          IF(ISFEDG(IEG,1).EQ.ISFCRN(ICN,1))THEN
            DO 560 IX=1,NDXC(ICN)
              XVALU=XVALU+DXCV(ICN,IX)
              CALL ADDECX(IEC,XVALU)
  560       CONTINUE
          ELSEIF(ISFEDG(IEG,1).EQ.ISFCRN(ICN,2))THEN
            DO 570 IY=1,NDYC(ICN)
              XVALU=XVALU+DYCV(ICN,IY)
              CALL ADDECX(IEC,XVALU)
  570       CONTINUE
          ELSEIF(ISFEDG(IEG,1).EQ.ISFCRN(ICN,3))THEN
            DO 580 IDZ=1,NDZC(ICN)
              XVALU=XVALU+DZCV(ICN,IDZ)
              CALL ADDECX(IEC,XVALU)
  580       CONTINUE
          ENDIF

C Add the Y -intervals associated with the corner.
          YVALU=0.
          IF(ISFEDG(IEG,2).EQ.ISFCRN(ICN,1))THEN
            DO 590 IX=1,NDXC(ICN)
              YVALU=YVALU+DXCV(ICN,IX)
              CALL ADDECY(IEC,YVALU)
  590       CONTINUE
          ELSEIF(ISFEDG(IEG,2).EQ.ISFCRN(ICN,2))THEN
            DO 600 IY=1,NDYC(ICN)
              YVALU=YVALU+DYCV(ICN,IY)
              CALL ADDECY(IEC,YVALU)
  600       CONTINUE
          ELSEIF(ISFEDG(IEG,2).EQ.ISFCRN(ICN,3))THEN
            DO 610 IDZ=1,NDZC(ICN)
              YVALU=YVALU+DZCV(ICN,IDZ)
              CALL ADDECY(IEC,YVALU)
  610       CONTINUE
          ENDIF

C Not defined corner.
        ELSEIF(INDXC(ICN).EQ.0)THEN
          CALL USRMSG(' un-defined corner connected by',
     &                'an edge-corner connection surface.','F')
        ENDIF

C Create the massless node associated with the current edg-crn connection.
        Y2=0.
        DO 620 IY=1,NDYEC(IEC)
          Y1=Y2
          Y2=DYEC(IEC,IY)
          DELY=Y2-Y1
          DISY=(Y1+Y2)/2.
          X2=0.
        DO 620 IX=1,NDXEC(IEC)
          X1=X2
          X2=DXEC(IEC,IX)
          DELX=X2-X1
          DISX=(X1+X2)/2.
          CALL GCOORD(ISP1,IEC,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
          VOLM=DELY*DELX
          NCNV=NCNV+1
          NNDS=NNDS+1
          WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,IEC
          WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
  620   CONTINUE
  510 CONTINUE
      RETURN
    1 STOP "error (609): while writing to the control volumes file."
    2 STOP "error (610): while writing to the nodes temporary file."
      END

C ***************************    CVNODE    ****************************
C CVNODE creates control volumes and nodes for surface control volumes.
C *********************************************************************
      SUBROUTINE CVNODE(IS,AREC,DISX,DISZ)
#include "building.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      LAIR=1
C Define the exterier massless node.
      NCNV=NCNV+1
      NNDS=NNDS+1
      VOLM=AREC
      DNHC=0.
      ILC=1
      DISY=Y0S(IS)
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,IS,ILC
      CALL GCOORD(IS,IS,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
      WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
      Y2=DISY
      DO 10 IL=1,NE(IS)
        Y1=Y2
        Y2=Y2-THK(IS,IL)
        DISY=(Y2+Y1)/2.
        NCNV=NCNV+1
        NNDS=NNDS+1
        IF(LAIR.LE.NAIRG(IS).AND.IL.EQ.IPAIRG(IS,LAIR))THEN
          DNHC=1.3*1005.5
          LAIR=LAIR+1
        ELSE
          DNHC=DEN(IS,IL)*SHT(IS,IL)
        ENDIF
        VOLM=AREC*THK(IS,IL)
        ILC=ILC+1
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,IS,ILC
        CALL GCOORD(IS,IS,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
        WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
        DISY=Y2
        NCNV=NCNV+1
        NNDS=NNDS+1
        VOLM=AREC
        ILC=ILC+1
        IF(IL.EQ.NE(IS))ILC=-ILC
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,0.,IS,ILC
        CALL GCOORD(IS,IS,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
        WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ
   10 CONTINUE
      RETURN
    1 STOP "error (611): while writing to the control volumes file."
    2 STOP "error (612): while writing to the nodes temporary file."
      END

C *****************************   CNCSRF   ***************************
C CNCSRF controls the creation of the connections for surfaces.
C *********************************************************************
      SUBROUTINE CNCSRF(IZ,INSIDX)
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)
      COMMON/GR3D44/NCVSF(MS),NCVV(MS,MCVS),ICVV(MS,MCVS,MCVV)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D81/ISNDS(MS),ISNDE(MEZ),ISNDC(MCZ)

C First, create the connections for the srf-srf and srf-edg connection
C surfaces, with outside and inside ambient.
      CALL CSSEOI(IZ,INSIDX)

C Create the connections associated directly with surfaces.
      DO 10 IS=1,NSUR
        ND1=ISNDS(IS)-1

C Discretized surface.
        IF(INDXS(IS).EQ.3)THEN
          DO 20 ICV=1,NCVSF(IS)

C Calculate the area of the current control volume.
            SUM=0.
            DO  30 I=1,NCVV(IS,ICV)
              IF(I.EQ.NCVV(IS,ICV))THEN
                J=1
              ELSE
                J=I+1
              ENDIF
              IP1=ICVV(IS,ICV,I)
              IP2=ICVV(IS,ICV,J)
              SUM=SUM+ZINT(IS,IP1)*XINT(IS,IP2)-XINT(IS,IP1)*
     &            ZINT(IS,IP2)
   30       CONTINUE
            AREC=0.5*ABS(SUM)
            CALL SCVCNC(IZ,IS,ICV,AREC)
   20     CONTINUE

C Lumped surface
        ELSE

C Calculate the area of the current surface.
          XS=0.
          YS=0.
          ZS=0.
          DO 40 I=1,NVER(IS)
            IF(I.EQ.NVER(IS))THEN
              J=1
            ELSE
              J=I+1
            ENDIF
            IP1=JVN(IS,I)
            IP2=JVN(IS,J)
            XS=XS+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
            YS=YS+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
            ZS=ZS+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
   40     CONTINUE
          AREC=0.5*SQRT(XS*XS+YS*YS+ZS*ZS)
          CALL SCVCNC(IZ,IS,1,AREC)
        ENDIF
   10 CONTINUE
      RETURN
      END
