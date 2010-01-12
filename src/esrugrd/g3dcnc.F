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

C *********************************************************************
C This file contains the folowing subroutines:

C SCVCNC creates the connections for each control voulme in surfaces.
C ADJCNC creates the adjacent connections for control volumes in
C        surfaces.
C CSSEOI connects the nodes in the srf-srf and srf-edge connections 
C        with outside or inside ambient.
C LMPEDG creates control volumes and connections for lumped edge.
C DISEDG creates control volumes and connections for discretised edge.
C LMPCRN creates control volumes and connections for lumped corner.
C DISCRN creates control volumes and connections for discretised corner.
C CNCISS creates the connections with the srf-srf connection surfaces.
C CNCISE creates the connections with the srf-edge connection surfaces.
C CNCIEE creates the connections with the edge-edge connection surfaces.
C CNCIEC creates the connections with edge-corner connection surfaces.
C *********************************************************************

C ************************    SCVCNC    *******************************
C SCVCNC creates the connections for each control voulme in surfaces.
C *********************************************************************
      SUBROUTINE SCVCNC(IZ,IS,ICV,AREC)
#include "building.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3


      LAIR=1
C The exterier massless node.
C Connection with outside.
      ND1=ND1+1
      DIST=0.
      COND=0.
      ITYP=21
      ISP1=IZ
      ISP2=IS
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,ISP1,ISP2
      ITYP=11
      Y1=Y0S(IS)
      DO 40 IL=1,NE(IS)
        Y2=Y1
        Y1=Y1-THK(IS,IL)

C Connection with the priviuos massless node.
        DIST=THK(IS,IL)/2.
        IF(LAIR.LE.NAIRG(IS).AND.IL.EQ.IPAIRG(IS,LAIR))THEN
          COND=THK(IS,IL)/RAIRG(IS,LAIR)
          LAIR=LAIR+1
        ELSE
          COND=CON(IS,IL)
        ENDIF
        ND1=ND1+1
        ISP2=ND1-1
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,IZ,ISP2

C Connections for the next massless node.
        ISP2=ND1+1
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,IZ,ISP2

C Connection with adjacent nodes.
        IF(INDXS(IS).EQ.3)CALL ADJCNC(IZ,IS,ICV,Y1,Y2,COND)
        ND1=ND1+1
   40 CONTINUE

C Interier massless node connection with inside air.
      DIST=0.
      COND=0.
      ITYP=31
      ISP1=IZ
      ISP2=IS
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,ISP1,ISP2
      RETURN
    3 STOP "error (401): while writing to the connections file."
      END

C ****************************   ADJCNC  ******************************
C ADJCNC creates the adjacent connections for control volumes in
C surfaces.
C *********************************************************************
      SUBROUTINE ADJCNC(IZ,IS,ICV1,Y1,Y2,COND)
#include "building.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D42/NVLN(MS,MLS),IVLN(MS,MLS,MLS)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)
      COMMON/GR3D44/NCVSF(MS),NCVV(MS,MCVS),ICVV(MS,MCVS,MCVV)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D81/ISNDS(MS),ISNDE(MEZ),ISNDC(MCZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      READ(INDC,REC=ND1,ERR=5)DISX1,DISY1,DISZ1
      ITYP=11
      NDCV=2*NE(IS)+1
      DO 10 IB=1,NCVV(IS,ICV1)
        I=ICVV(IS,ICV1,IB)
        IF(IB.EQ.NCVV(IS,ICV1))THEN
          J=ICVV(IS,ICV1,1)
        ELSE
          J=ICVV(IS,ICV1,IB+1)
        ENDIF

C Define the control volume line sigment (connection surface) type.
        CALL CNCLNE(IS,I,J,ICL)

C Common line (surface) is a boundary line (i.e., the adjacent node 
C is a massless node in a srf-edge connection.
        IF(ICL.GT.0)THEN

C Find the number of the adjacent srf-edge connection.
          CALL ISEFNS(IS,ICL,ISSE,NSSE,ILB)

C Check if the current connection surface is active (if not bypass).
          IF((ISSE.EQ.1.AND.INDXSS(NSSE).NE.3).OR.
     &       (ISSE.EQ.2.AND.INDXSE(NSSE).NE.3))GOTO 10

C Define the start and end points for the common line.
          IF(ILB.GT.0)THEN
            IVRT1=IVLN(IS,ICL,1)
          ELSEIF(ILB.LT.0)THEN
            IVRT1=IVLN(IS,ICL,NVLN(IS,ICL))
          ENDIF

C Determine the cooficients for this node with respect to the srf-srf
C or srf-edge connection surface.
          DIV1=SQRT((XINT(IS,I)-XINT(IS,IVRT1))**2+
     &              (ZINT(IS,I)-ZINT(IS,IVRT1))**2)
          DJV1=SQRT((XINT(IS,J)-XINT(IS,IVRT1))**2+
     &              (ZINT(IS,J)-ZINT(IS,IVRT1))**2)
          Z1=AMIN1(DIV1,DJV1)
          Z2=AMAX1(DIV1,DJV1)

C Create the connections with the srf-srf connection surface.
          IF(ISSE.EQ.1)THEN
            Y1PD=Y1-Y1SS(NSSE)
            Y2PD=Y2-Y1SS(NSSE)
            CALL CNCISS(IZ,NSSE,DISX1,DISY1,DISZ1,COND,Y1PD,Y2PD,Z1,Z2)

C Create the connections with the srf-edge connection surface.
          ELSE
            Y1PD=Y1-Y1SE(NSSE)
            Y2PD=Y2-Y1SE(NSSE)
            CALL CNCISE(IZ,NSSE,DISX1,DISY1,DISZ1,COND,Y1PD,Y2PD,Z1,Z2)
          ENDIF

C the adjacent node is within the same surface. Find the control 
C volume number.
        ELSE
          CALL FDSCV2(IS,ICV1,I,J,ICV2)
          IF(ICV2.LT.ICV1)GOTO 10

C Define the second node number.
          IDEL=MOD((ND1-ISNDS(IS)),NDCV)
          ISP2=ISNDS(IS)+(ICV2-1)*NDCV+IDEL
          READ(INDC,REC=ISP2,ERR=5)DISX2,DISY2,DISZ2
          DIST=SQRT((DISX2-DISX1)**2+(DISY2-DISY1)**2+(DISZ2-DISZ1)**2)
          DXZ=SQRT((XINT(IS,I)-XINT(IS,J))**2+
     &             (ZINT(IS,I)-ZINT(IS,J))**2)
          AREC=DXZ*(Y2-Y1)
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,IZ,ISP2
        ENDIF
   10 CONTINUE
      RETURN
    3 STOP "error (402): while writing to the connections file."
    5 STOP "error (403): while reading the nodes coordinates file."
      END

C **************************   CSSEOI   *******************************
C CSSEOI connects the nodes in the srf-srf and srf-edge connections 
C with outside or inside ambient.
C *********************************************************************
      SUBROUTINE CSSEOI(IZ,INSIDX)
#include "building.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D52/NDZSS(MSSZ),DZSS(MSSZ,MZSS)
      COMMON/GR3D53/NDYSS(MSSZ),DYSS(MSSZ,MYSS)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D57/NDZSE(MSEZ),DZSE(MSEZ,MZSE)
      COMMON/GR3D58/NDYSE(MSEZ),DYSE(MSEZ,MYSE)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D82/ISNDSS(MSSZ),ISNDSE(MSEZ),ISNDEE(MEEZ),ISNDEC(MECZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

C First, the srf-srf connection surfaces.
      DO 10 ISS=1,NUMSS

C Bypass this connection surface if not defined (activated).
        IF(INDXSS(ISS).NE.3)GOTO 10
        IS1=IBDYSS(ISS,1)
        IS2=IBDYSS(ISS,2)

C Connections with outside ambient (surface-1 boundary).
        Y1=Y0S(IS1)-Y1SS(ISS)
        Y2=Y0SS(ISS)-Y1SS(ISS)
        DELTO1=Y2-Y1
        IF(INDXS(IS1).EQ.3.AND.DELTO1.GT.1.E-4)THEN
          ZSS2=0.
          DO 30 IDZ=1,NDZSS(ISS)
            ZSS1=ZSS2
            ZSS2=DZSS(ISS,IDZ)
            YSS2=0.
            DO  40 IY=1,NDYSS(ISS)
              YSS1=YSS2
              YSS2=DYSS(ISS,IY)
              IF(Y1.GT.(YSS2-1.E-6))THEN
                GOTO 40
              ELSEIF(Y2.LT.(YSS1+1.E-6))THEN
                GOTO 30
              ELSE
                AREC=(ZSS2-ZSS1)*(YSS2-YSS1)
                ND1=ISNDSS(ISS)+(IDZ-1)*NDYSS(ISS)+(IY-1)
                IR=IR+1
                IF(INSIDX.EQ.1)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,21,IZ,IS1
                ELSEIF(INSIDX.EQ.2)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,22,IZ,IS1
                ENDIF
              ENDIF
   40       CONTINUE
   30     CONTINUE
        ENDIF

C Connections with outside ambient (surface-2 boundary).
        Y1=Y0S(IS2)-Y1SS(ISS)
        Y2=Y0SS(ISS)-Y1SS(ISS)
        DELTO2=Y2-Y1

C Check for the existence of fatal error.
        IF(DELTO1.GT.1.E-4.AND.DELTO2.GT.1.E-4)THEN

C The current srf-srf is connected to outside from both side. This will
C create a node with no future coefficients.
          STOP "error (404): srf-srf connection surface indentation."
        ELSEIF(INDXS(IS2).EQ.3.AND.DELTO2.GT.1.E-4)THEN
          ZSS2=0.
          DO 50 IDZ=1,NDZSS(ISS)
            ZSS1=ZSS2
            ZSS2=DZSS(ISS,IDZ)
            YSS2=0.
            DO  60 IY=1,NDYSS(ISS)
              YSS1=YSS2
              YSS2=DYSS(ISS,IY)
              IF(Y1.GT.(YSS2-1.E-6))THEN
                GOTO 60
              ELSEIF(Y2.LT.(YSS1+1.E-6))THEN
                GOTO 50
              ELSE
                AREC=(ZSS2-ZSS1)*(YSS2-YSS1)
                ND1=ISNDSS(ISS)+(IDZ-1)*NDYSS(ISS)+(IY-1)
                IR=IR+1
                IF(INSIDX.EQ.1)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,21,IZ,IS2
                ELSEIF(INSIDX.EQ.2)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,22,IZ,IS2
                ENDIF
              ENDIF
   60       CONTINUE
   50     CONTINUE
        ENDIF

C Connections with inside ambient (surface-1 boundary).
        Y1=0.
        Y2=Y1S(IS1)-Y1SS(ISS)
        DELTI1=Y2
        IF(INDXS(IS1).EQ.3.AND.DELTI1.GT.1.E-4)THEN
          ZSS2=0.
          DO 70 IDZ=1,NDZSS(ISS)
            ZSS1=ZSS2
            ZSS2=DZSS(ISS,IDZ)
            YSS2=0.
            DO  80 IY=1,NDYSS(ISS)
              YSS1=YSS2
              YSS2=DYSS(ISS,IY)
              IF(Y1.GT.(YSS2-1.E-6))THEN
                GOTO 80
              ELSEIF(Y2.LT.(YSS1+1.E-6))THEN
                GOTO 70
              ELSE
                AREC=(ZSS2-ZSS1)*(YSS2-YSS1)
                ND1=ISNDSS(ISS)+(IDZ-1)*NDYSS(ISS)+(IY-1)
                IR=IR+1
                IF(INSIDX.EQ.1)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,31,IZ,IS1
                ELSEIF(INSIDX.EQ.2)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,32,IZ,IS1
                ENDIF
              ENDIF
   80       CONTINUE
   70     CONTINUE
        ENDIF

C Connections with inside ambient (surface-2 boundary).
        Y1=0.
        Y2=Y1S(IS2)-Y1SS(ISS)
        DELTI2=Y2

C Check for the existence of fatal error.
        IF(DELTI1.GT.1.E-4.AND.DELTI2.GT.1.E-4)THEN
          STOP "error (405): srf-srf connection surface indentation."
        ELSEIF(INDXS(IS2).EQ.3.AND.DELTI2.GT.1.E-4)THEN
          ZSS2=0.
          DO 90 IDZ=1,NDZSS(ISS)
            ZSS1=ZSS2
            ZSS2=DZSS(ISS,IDZ)
            YSS2=0.
            DO  100 IY=1,NDYSS(ISS)
              YSS1=YSS2
              YSS2=DYSS(ISS,IY)
              IF(Y1.GT.(YSS2-1.E-6))THEN
                GOTO 100
              ELSEIF(Y2.LT.(YSS1+1.E-6))THEN
                GOTO 90
              ELSE
                AREC=(ZSS2-ZSS1)*(YSS2-YSS1)
                ND1=ISNDSS(ISS)+(IDZ-1)*NDYSS(ISS)+(IY-1)
                IR=IR+1
                IF(INSIDX.EQ.1)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,31,IZ,IS2
                ELSEIF(INSIDX.EQ.2)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,32,IZ,IS2
                ENDIF
              ENDIF
  100       CONTINUE
   90     CONTINUE
        ENDIF
   10 CONTINUE

C Second, the srf-edg connection surfaces.
      DO 120 ISE=1,NUMSE
        IS=IBDYSE(ISE,1)

C Connections with outside ambient.
        Y1=Y0S(IS)-Y1SE(ISE)
        Y2=Y0SE(ISE)-Y1SE(ISE)
        DELTO=Y2-Y1
        IF(INDXSE(ISE).EQ.3.AND.DELTO.GT.1.E-4)THEN
          ZSE2=0.
          DO 130 IDZ=1,NDZSE(ISE)
            ZSE1=ZSE2
            ZSE2=DZSE(ISE,IDZ)
            YSE2=0.
            DO 140 IY=1,NDYSE(ISE)
              YSE1=YSE2
              YSE2=DYSE(ISE,IY)
              IF(Y1.GT.(YSE2-1.E-6))THEN
                GOTO 140
              ELSEIF(Y2.LT.(YSE1+1.E-6))THEN
                GOTO 130
              ELSE
                AREC=(ZSE2-ZSE1)*(YSE2-YSE1)
                ND1=ISNDSE(ISE)+(IDZ-1)*NDYSE(ISE)+(IY-1)
                IR=IR+1
                IF(INSIDX.EQ.1)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,21,IZ,IS
                ELSEIF(INSIDX.EQ.2)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,22,IZ,IS
                ENDIF
              ENDIF
  140       CONTINUE
  130     CONTINUE
        ENDIF

C Connections with inside ambient.
        Y1=0.
        Y2=Y1S(IS)-Y1SE(ISE)
        DELTI=Y2-Y1
        IF(INDXSE(ISE).EQ.3.AND.DELTI.GT.1.E-4)THEN
          ZSE2=0.
          DO 150 IDZ=1,NDZSE(ISE)
            ZSE1=ZSE2
            ZSE2=DZSE(ISE,IDZ)
            YSE2=0.
            DO 160 IY=1,NDYSE(ISE)
              YSE1=YSE2
              YSE2=DYSE(ISE,IY)
              IF(Y1.GT.(YSE2-1.E-6))THEN
                GOTO 160
              ELSEIF(Y2.LT.(YSE1+1.E-6))THEN
                GOTO 150
              ELSE
                AREC=(ZSE2-ZSE1)*(YSE2-YSE1)
                ND1=ISNDSE(ISE)+(IDZ-1)*NDYSE(ISE)+(IY-1)
                IR=IR+1
                IF(INSIDX.EQ.1)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,31,IZ,IS
                ELSEIF(INSIDX.EQ.2)THEN
                  WRITE(ICNC,REC=IR,ERR=3)ND1,0.,AREC,0.,32,IZ,IS
                ENDIF
              ENDIF
  160       CONTINUE
  150     CONTINUE
        ENDIF
  120 CONTINUE
      RETURN
    3 STOP "error (406): while writing to the connections file"
      END

C *************************    LMPEDG    ******************************
C LMPEDG creates control volumes and connections for lumped edge.
C *********************************************************************
      SUBROUTINE LMPEDG(IZ,IEG)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      CHARACTER PNM*72,TL*72

C Determine the lumped edge thermophysical properties.
      VOLML=0.
      DNHCL=0.
      CNDCL=0.
      IS1=ISFEDG(IEG,1)
      IS2=ISFEDG(IEG,2)
      DO 10 IDZ=1,NDZE(IEG)
      DO 10 IY=1,NDYE(IEG)
      DO 10 IX=1,NDXE(IEG)
        IEL=ITPEDG(IEG,IX,IY,IDZ)
        matarrayindex=IEL   ! which legacy index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
        if(matarrayindex.eq.0)then
          CND=0.0; DBDEN=0.0; DBSHT=0.0 
          E=0.99; A=0.99
          PNM='AIR'
        else
          CND=matdbcon(matarrayindex)
          DBDEN=matdbden(matarrayindex)
          DBSHT=matdbsht(matarrayindex)
          E=matdbine(matarrayindex)
          A=matdbina(matarrayindex)
          write(PNM,'(a)') matname(matarrayindex)(1:32)
        endif

        VL=DZE(IEG,IDZ)*DYE(IEG,IY)*DXE(IEG,IX)
        VOLML=VOLML+VL
        DNHCL=DNHCL+DBDEN*DBSHT*VL
        CNDCL=CNDCL+CND*VL
   10 CONTINUE
      DNHCL=DNHCL/VOLML
      CNDCL=CNDCL/VOLML
      Z2=0.
      DO 20 IDZ=1,NDZE(IEG)
        Z2=Z2+DZE(IEG,IDZ)
   20 CONTINUE
        Y2=0.
      DO 30 IY=1,NDYE(IEG)
        Y2=Y2+DYE(IEG,IY)
   30 CONTINUE
        X2=0.
      DO 40 IX=1,NDXE(IEG)
        X2=X2+DXE(IEG,IX)
   40 CONTINUE

C Create the two massless nodes and their connections with outside.
      DNHC=0.
      DISX=0.
      DISY=0.
      DIST=0.
      COND=0.
      ITYP=21
      ISP1=-2
      AREC=X2*Z2
      VOLM=AREC
      NCNV=NCNV+1
      NNDS=NNDS+1
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,IEG
      WRITE(INDC,REC=NNDS,ERR=2)X2/2.,DISY,Z2/2.
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,IZ,IS2
      AREC=Y2*Z2
      VOLM=AREC
      NCNV=NCNV+1
      NNDS=NNDS+1
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,IEG
      WRITE(INDC,REC=NNDS,ERR=2)DISX,Y2/2.,Z2/2.
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,IZ,IS1

C Create the lumped edge node with its connections with the exterier 
C massless nodes.
      NCNV=NCNV+1
      NNDS=NNDS+1
      ND1=NNDS
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLML,DNHCL,ISP1,IEG
      WRITE(INDC,REC=NNDS,ERR=2)X2/2.,Y2/2.,Z2/2.
      DIST=Y2/2.
      AREC=X2*Z2
      ITYP=11
      N2=NNDS-2
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CNDCL,ITYP,IZ,N2
      DIST=X2/2.
      AREC=Y2*Z2
      N2=NNDS-1
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CNDCL,ITYP,IZ,N2

C Connections with the two srf-edg connections.
      ISE1=ISEFNE(IEG,IS1)
      IF(INDXSE(ISE1).EQ.3)THEN
        CALL CNCISE(IZ,ISE1,Y2/2.,X2/2.,Z2/2.,CNDCL,0.,X2,0.,Z2)
      ENDIF
      ISE2=ISEFNE(IEG,IS2)
      IF(INDXSE(ISE2).EQ.3)THEN
        CALL CNCISE(IZ,ISE2,X2/2.,Y2/2.,Z2/2.,CNDCL,0.,Y2,0.,Z2)
      ENDIF

C Connections with the two edge-edge and/or edge-corner connections.
      CALL IEECFN(IEG,IVXEDG(IEG,1),IEEC,NEEC)

C Edge-edge connection.
      IF(IEEC.EQ.1.AND.INDXEE(NEEC).EQ.3)THEN
        IF(IEG.EQ.IBDYEE(NEEC,1).OR.IEG.EQ.IBDYEE(NEEC,2))THEN
          CALL CNCIEE(IZ,NEEC,X2/2.,Y2/2.,Z2/2.,CNDCL,0.,X2,0.,Y2)
        ELSE
          CALL CNCIEE(IZ,NEEC,Y2/2.,X2/2.,Z2/2.,CNDCL,0.,Y2,0.,X2)
        ENDIF

C Edge-corner connection.
      ELSEIF(IEEC.EQ.2.AND.INDXEC(NEEC).EQ.3)THEN
        CALL CNCIEC(IZ,NEEC,X2/2.,Y2/2.,Z2/2.,CNDCL,0.,X2,0.,Y2)
      ENDIF
      CALL IEECFN(IEG,IVXEDG(IEG,2),IEEC,NEEC)

C Edge-edge connection.
      IF(IEEC.EQ.1.AND.INDXEE(NEEC).EQ.3)THEN
        IF(IEG.EQ.IBDYEE(NEEC,1).OR.IEG.EQ.IBDYEE(NEEC,2))THEN
          CALL CNCIEE(IZ,NEEC,X2/2.,Y2/2.,Z2/2.,CNDCL,0.,X2,0.,Y2)
        ELSE
          CALL CNCIEE(IZ,NEEC,Y2/2.,X2/2.,Z2/2.,CNDCL,0.,Y2,0.,X2)
        ENDIF

C Edge-corner connection.
      ELSEIF(IEEC.EQ.2.AND.INDXEC(NEEC).EQ.3)THEN
        CALL CNCIEC(IZ,NEEC,X2/2.,Y2/2.,Z2/2.,CNDCL,0.,X2,0.,Y2)
      ENDIF
      RETURN
    1 STOP "error (408): while writing to the control volumes file."
    2 STOP "error (409): while writing to the nodes temporary file."
    3 STOP "error (410): while writing to the connections file."
      END

C ****************************    DISEDG    ***************************
C DISEDG creates control volumes and connections for discretised edge.
C *********************************************************************
      SUBROUTINE DISEDG(IZ,IEG)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D81/ISNDS(MS),ISNDE(MEZ),ISNDC(MCZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      CHARACTER PNAM*72,TITL*72

      ISP1=-2
      IS1=ISFEDG(IEG,1)
      IS2=ISFEDG(IEG,2)
      NTX=NDXE(IEG)
      NTY=NDYE(IEG)
      NTZ=NDZE(IEG)
      NDZ=4*NTX*NTY
      Z2=0.
      DO 10 IL=1,NTZ
        IF(IL.EQ.NTZ)THEN
          LL=3
        ELSE
          LL=4
        ENDIF
        NDY=LL*NTX
        Z1=Z2
        Z2=Z2+DZE(IEG,IL)
        DISZ=(Z1+Z2)/2.
        DELZ=Z2-Z1
        Y2=0.
      DO 10 IY=1,NTY
        Y1=Y2
        Y2=Y2+DYE(IEG,IY)
        DISY=(Y1+Y2)/2.
        DELY=Y2-Y1
        X2=0.
      DO 10 IX=1,NTX
        X1=X2
        X2=X2+DXE(IEG,IX)
        DISX=(X1+X2)/2.
        DELX=X2-X1
        IEL=ITPEDG(IEG,IX,IY,IL)
        matarrayindex=IEL   ! which legacy index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
        if(matarrayindex.eq.0)then
          CND=0.0; DEN=0.0; SHT=0.0 
          E=0.99; A=0.99
          PNAM='AIR'
        else
          CND=matdbcon(matarrayindex)
          DEN=matdbden(matarrayindex)
          SHT=matdbsht(matarrayindex)
          E=matdbine(matarrayindex)
          A=matdbina(matarrayindex)
          write(PNAM,'(a)') matname(matarrayindex)(1:32)
        endif

C Create the control volume and node.
        CALL GCOORD(ISP1,IEG,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
        NCNV=NCNV+1
        NNDS=NNDS+1
        ND1=NNDS
        VOLM=DXE(IEG,IX)*DYE(IEG,IY)*DZE(IEG,IL)
        DNHT=DEN*SHT
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,IEG
        WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ

C Create the three adjacent massless nodes.
        CALL GCOORD(ISP1,IEG,X2,DISY,DISZ,GLBX1,GLBY1,GLBZ1)
        NCNV=NCNV+1
        NNDS=NNDS+1
        N1=NNDS
        VOLM=DELY*DELZ
        DNHT=0.
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,IEG
        WRITE(INDC,REC=NNDS,ERR=2)GLBX1,GLBY1,GLBZ1
        CALL GCOORD(ISP1,IEG,DISX,Y2,DISZ,GLBX2,GLBY2,GLBZ2)
        NCNV=NCNV+1
        NNDS=NNDS+1
        N2=NNDS
        VOLM=DELX*DELZ
        DNHT=0.
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,IEG
        WRITE(INDC,REC=NNDS,ERR=2)GLBX2,GLBY2,GLBZ2
        IF(IL.LT.NTZ)THEN
          CALL GCOORD(ISP1,IEG,DISX,DISY,Z2,GLBX3,GLBY3,GLBZ3)
          NCNV=NCNV+1
          NNDS=NNDS+1
          N3=NNDS
          VOLM=DELX*DELY
          DNHT=0.
          WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,IEG
          WRITE(INDC,REC=NNDS,ERR=2)GLBX3,GLBY3,GLBZ3
        ENDIF

C Connect the ordinary node with it's adjacent nodes.
C LEFT.
        IF(IX.EQ.1)THEN
          ISE2=ISEFNE(IEG,IS2)
          IF(INDXSE(ISE2).EQ.3)THEN
              CALL CNCISE(IZ,ISE2,GLBX,GLBY,GLBZ,CND,Y1,Y2,Z1,Z2)
          ENDIF
        ELSE
          ND2=ND1-(LL-1)
          DIST=DELX/2.
          AREC=DELY*DELZ
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF

C BEHIND.
        IF(IY.EQ.1)THEN
          ISE1=ISEFNE(IEG,IS1)
          IF(INDXSE(ISE1).EQ.3)THEN
              CALL CNCISE(IZ,ISE1,GLBX,GLBY,GLBZ,CND,X1,X2,Z1,Z2)
          ENDIF
        ELSE
          ND2=ND1-NDY+2
          DIST=DELY/2.
          AREC=DELX*DELZ
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF

C BELOW.
        IF(IL.EQ.1)THEN
          CALL IEECFN(IEG,IVXEDG(IEG,1),IEEC,NEEC)
          IF(IEEC.EQ.1.AND.INDXEE(NEEC).EQ.3)THEN
            IF(IEG.EQ.IBDYEE(NEEC,1).OR.IEG.EQ.IBDYEE(NEEC,2))THEN
              CALL CNCIEE(IZ,NEEC,GLBX,GLBY,GLBZ,CND,X1,X2,Y1,Y2)
            ELSE
              CALL CNCIEE(IZ,NEEC,GLBX,GLBY,GLBZ,CND,Y1,Y2,X1,X2)
            ENDIF
          ELSEIF(IEEC.EQ.2.AND.INDXEC(NEEC).EQ.3)THEN
            CALL CNCIEC(IZ,NEEC,GLBX,GLBY,GLBZ,CND,X1,X2,Y1,Y2)
          ENDIF
        ELSE
          ND2=ISNDE(IEG)-1+(IL-2)*NDZ+(IY-1)*(4*NTX)+IX*4
          DIST=DELZ/2.
          AREC=DELX*DELY
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF

C RIGHT.
        ND2=N1
        DIST=DELX/2.
        AREC=DELY*DELZ
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        IF(IX.EQ.NTX)THEN
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND2,0.,AREC,0.,21,IZ,IS1
        ENDIF

C FRONT.
        ND2=N2
        DIST=DELY/2.
        AREC=DELX*DELZ
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        IF(IY.EQ.NTY)THEN
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND2,0.,AREC,0.,21,IZ,IS2
        ENDIF

C ABOVE.
        IF(IL.EQ.NTZ)THEN
          CALL IEECFN(IEG,IVXEDG(IEG,2),IEEC,NEEC)
          IF(IEEC.EQ.1.AND.INDXEE(NEEC).EQ.3)THEN
            IF(IEG.EQ.IBDYEE(NEEC,1).OR.IEG.EQ.IBDYEE(NEEC,2))THEN
              CALL CNCIEE(IZ,NEEC,GLBX,GLBY,GLBZ,CND,X1,X2,Y1,Y2)
            ELSE
              CALL CNCIEE(IZ,NEEC,GLBX,GLBY,GLBZ,CND,Y1,Y2,X1,X2)
            ENDIF
          ELSEIF(IEEC.EQ.2.AND.INDXEC(NEEC).EQ.3)THEN
            CALL CNCIEC(IZ,NEEC,GLBX,GLBY,GLBZ,CND,X1,X2,Y1,Y2)
          ENDIF
        ELSE
          ND2=N3
          DIST=DELZ/2.
          AREC=DELX*DELY
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF
   10 CONTINUE
      RETURN
    1 STOP "error (412): while writing to the control volumes file."
    2 STOP "error (413): while writing to the nodes temporary file."
    3 STOP "error (414): while writing to the connections file."
      END

C *************************     LMPCRN     ****************************
C LMPCRN creates control volumes and connections for lumped corner.
C *********************************************************************
      SUBROUTINE LMPCRN(IZ,ICR)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      CHARACTER PNM*72,TL*72

C Determine the lumped corner thermophysical properties.
      VOLML=0.
      DNHCL=0.
      CNDCL=0.
      IS1=ISFCRN(ICR,1)
      IS2=ISFCRN(ICR,2)
      IS3=ISFCRN(ICR,3)
      DO 10 IDZ=1,NDZC(ICR)
      DO 10 IY=1,NDYC(ICR)
      DO 10 IX=1,NDXC(ICR)
        IEL=ITPCRN(ICR,IX,IY,IDZ)
        matarrayindex=IEL   ! which legacy index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
        if(matarrayindex.eq.0)then
          CND=0.0; DBDEN=0.0; DBSHT=0.0 
          E=0.99; A=0.99
          PNM='AIR'
        else
          CND=matdbcon(matarrayindex)
          DBDEN=matdbden(matarrayindex)
          DBSHT=matdbsht(matarrayindex)
          E=matdbine(matarrayindex)
          A=matdbina(matarrayindex)
          write(PNM,'(a)') matname(matarrayindex)(1:32)
        endif

        VL=DZC(ICR,IDZ)*DYC(ICR,IY)*DXC(ICR,IX)
        VOLML=VOLML+VL
        DNHCL=DNHCL+DBDEN*DBSHT*VL
        CNDCL=CNDCL+CND*VL
   10 CONTINUE
      DNHCL=DNHCL/VOLML
      CNDCL=CNDCL/VOLML
      Z2=0.
      DO 20 IDZ=1,NDZC(ICR)
        Z2=Z2+DZC(ICR,IDZ)
   20 CONTINUE
        Y2=0.
      DO 30 IY=1,NDYC(ICR)
        Y2=Y2+DYC(ICR,IY)
   30 CONTINUE
        X2=0.
      DO 40 IX=1,NDXC(ICR)
        X2=X2+DXC(ICR,IX)
   40 CONTINUE

C Create the three massless nodes and their connections with outside.
      DNHC=0.
      DISX=0.
      DISY=0.
      DISZ=0.
      DIST=0.
      COND=0.
      ITYP=21
      ISP1=-3
      AREC=X2*Z2
      VOLM=AREC
      NCNV=NCNV+1
      NNDS=NNDS+1
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,ICR
      WRITE(INDC,REC=NNDS,ERR=2)X2/2.,DISY,Z2/2.
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,IZ,IS2
      NCNV=NCNV+1
      NNDS=NNDS+1
      AREC=Y2*Z2
      VOLM=AREC
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,ICR
      WRITE(INDC,REC=NNDS,ERR=2)DISX,Y2/2.,Z2/2.
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,IZ,IS1
      NCNV=NCNV+1
      NNDS=NNDS+1
      AREC=X2*Y2
      VOLM=AREC
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC,ISP1,ICR
      WRITE(INDC,REC=NNDS,ERR=2)X2/2.,Y2/2.,DISZ
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,IZ,IS3

C Create the lumped corner node with its connections with the exterier 
C massless nodes.
      NCNV=NCNV+1
      NNDS=NNDS+1
      ND1=NNDS
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLML,DNHCL,ISP1,ICR
      WRITE(INDC,REC=NNDS,ERR=2)X2/2.,Y2/2.,Z2/2.
      DIST=Y2/2.
      AREC=X2*Z2
      ITYP=11
      N2=NNDS-3
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CNDCL,ITYP,IZ,N2
      DIST=X2/2.
      AREC=Y2*Z2
      N2=NNDS-2
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CNDCL,ITYP,IZ,N2
      DIST=Z2/2.
      AREC=X2*Y2
      N2=NNDS-1
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CNDCL,ITYP,IZ,N2

C Connections with the three edg-corner connections.
      IEC1=IECFNE(ICR,IS2,IS3)
      IF(INDXEC(IEC1).EQ.3.AND.IBDYEC(IEC1,2).EQ.ICR)THEN
        CALL CNCIEC(IZ,IEC1,Y2/2.,Z2/2.,X2/2.,CNDCL,0.,Y2,0.,Z2)
      ELSEIF(INDXEC(IEC1).EQ.3.AND.IBDYEC(IEC1,2).EQ.-ICR)THEN
        CALL CNCIEC(IZ,IEC1,Z2/2.,Y2/2.,X2/2.,CNDCL,0.,Z2,0.,Y2)
      ENDIF
      IEC2=IECFNE(ICR,IS3,IS1)
      IF(INDXEC(IEC2).EQ.3.AND.IBDYEC(IEC2,2).EQ.ICR)THEN
        CALL CNCIEC(IZ,IEC2,Z2/2.,X2/2.,Y2/2.,CNDCL,0.,Z2,0.,X2)
      ELSEIF(INDXEC(IEC2).EQ.3.AND.IBDYEC(IEC2,2).EQ.-ICR)THEN
        CALL CNCIEC(IZ,IEC2,X2/2.,Z2/2.,Y2/2.,CNDCL,0.,X2,0.,Z2)
      ENDIF
      IEC3=IECFNE(ICR,IS1,IS2)
      IF(INDXEC(IEC3).EQ.3.AND.IBDYEC(IEC3,2).EQ.ICR)THEN
        CALL CNCIEC(IZ,IEC3,X2/2.,Y2/2.,Z2/2.,CNDCL,0.,X2,0.,Y2)
      ELSEIF(INDXEC(IEC3).EQ.3.AND.IBDYEC(IEC3,2).EQ.-ICR)THEN
        CALL CNCIEC(IZ,IEC3,Y2/2.,X2/2.,Z2/2.,CNDCL,0.,Y2,0.,X2)
      ENDIF
      RETURN
    1 STOP "error (416): while writing to the control volumes file."
    2 STOP "error (417): while writing to the nodes temporary file."
    3 STOP "error (418): while writing to the connections file."
      END

C **************************    DISCRN    *****************************
C DISCRN creates control volumes and connections for discretised corner.
C *********************************************************************
      SUBROUTINE DISCRN(IZ,ICR)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      CHARACTER PNAM*72,TITL*72

      ISP1=-3
      IS1=ISFCRN(ICR,1)
      IS2=ISFCRN(ICR,2)
      IS3=ISFCRN(ICR,3)
      NTX=NDXC(ICR)
      NTY=NDYC(ICR)
      NTZ=NDZC(ICR)
      NDY=4*NTX
      NDZ=NTY*NDY
      Z2=0.
      DO 10 IL=1,NTZ
        Z1=Z2
        Z2=Z2+DZC(ICR,IL)
        DISZ=(Z1+Z2)/2.
        DELZ=Z2-Z1
        Y2=0.
      DO 10 IY=1,NTY
        Y1=Y2
        Y2=Y2+DYC(ICR,IY)
        DISY=(Y1+Y2)/2.
        DELY=Y2-Y1
        X2=0.
      DO 10 IX=1,NTX
        X1=X2
        X2=X2+DXC(ICR,IX)
        DISX=(X1+X2)/2.
        DELX=X2-X1
        IEL=ITPCRN(ICR,IX,IY,IL)
        matarrayindex=IEL   ! which legacy index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
        if(matarrayindex.eq.0)then
          CND=0.0; DEN=0.0; SHT=0.0 
          E=0.99; A=0.99
          PNAM='AIR'
        else
          CND=matdbcon(matarrayindex)
          DEN=matdbden(matarrayindex)
          SHT=matdbsht(matarrayindex)
          E=matdbine(matarrayindex)
          A=matdbina(matarrayindex)
          write(PNAM,'(a)') matname(matarrayindex)(1:32)
        endif

C Create the control volume and node.
        CALL GCOORD(ISP1,ICR,DISX,DISY,DISZ,GLBX,GLBY,GLBZ)
        NCNV=NCNV+1
        NNDS=NNDS+1
        ND1=NNDS
        VOLM=DXC(ICR,IX)*DYC(ICR,IY)*DZC(ICR,IL)
        DNHT=DEN*SHT
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,ICR
        WRITE(INDC,REC=NNDS,ERR=2)GLBX,GLBY,GLBZ

C Create the three adjacent massless nodes.
        CALL GCOORD(ISP1,ICR,X2,DISY,DISZ,GLBX1,GLBY1,GLBZ1)
        NCNV=NCNV+1
        NNDS=NNDS+1
        N1=NNDS
        VOLM=DELY*DELZ
        DNHT=0.
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,ICR
        WRITE(INDC,REC=NNDS,ERR=2)GLBX1,GLBY1,GLBZ1
        CALL GCOORD(ISP1,ICR,DISX,Y2,DISZ,GLBX2,GLBY2,GLBZ2)
        NCNV=NCNV+1
        NNDS=NNDS+1
        N2=NNDS
        VOLM=DELX*DELZ
        DNHT=0.
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,ICR
        WRITE(INDC,REC=NNDS,ERR=2)GLBX2,GLBY2,GLBZ2
        CALL GCOORD(ISP1,ICR,DISX,DISY,Z2,GLBX3,GLBY3,GLBZ3)
        NCNV=NCNV+1
        NNDS=NNDS+1
        N3=NNDS
        VOLM=DELX*DELY
        DNHT=0.
        WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHT,ISP1,ICR
        WRITE(INDC,REC=NNDS,ERR=2)GLBX3,GLBY3,GLBZ3

C Connect the ordinary node with it's adjacent nodes.
C LEFT.
        IF(IX.EQ.1)THEN
          IEC1=IECFNE(ICR,IS2,IS3)
          IF(INDXEC(IEC1).EQ.3.AND.IBDYEC(IEC1,2).EQ.ICR)THEN
            CALL CNCIEC(IZ,IEC1,GLBX,GLBY,GLBZ,CND,Y1,Y2,Z1,Z2)
          ELSEIF(INDXEC(IEC1).EQ.3.AND.IBDYEC(IEC1,2).EQ.-ICR)THEN
            CALL CNCIEC(IZ,IEC1,GLBX,GLBY,GLBZ,CND,Z1,Z2,Y1,Y2)
          ENDIF
        ELSE
          ND2=ND1-3
          DIST=DELX/2.
          AREC=DELY*DELZ
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF

C BEHIND.
        IF(IY.EQ.1)THEN
          IEC2=IECFNE(ICR,IS1,IS3)
          IF(INDXEC(IEC2).EQ.3.AND.IBDYEC(IEC2,2).EQ.ICR)THEN
            CALL CNCIEC(IZ,IEC2,GLBX,GLBY,GLBZ,CND,Z1,Z2,X1,X2)
          ELSEIF(INDXEC(IEC2).EQ.3.AND.IBDYEC(IEC2,2).EQ.-ICR)THEN
            CALL CNCIEC(IZ,IEC2,GLBX,GLBY,GLBZ,CND,X1,X2,Z1,Z2)
          ENDIF
        ELSE
          ND2=ND1-NDY+2
          DIST=DELY/2.
          AREC=DELX*DELZ
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF

C BELOW.
        IF(IL.EQ.1)THEN
          IEC3=IECFNE(ICR,IS1,IS2)
          IF(INDXEC(IEC3).EQ.3.AND.IBDYEC(IEC3,2).EQ.ICR)THEN
            CALL CNCIEC(IZ,IEC3,GLBX,GLBY,GLBZ,CND,X1,X2,Y1,Y2)
          ELSEIF(INDXEC(IEC3).EQ.3.AND.IBDYEC(IEC3,2).EQ.-ICR)THEN
            CALL CNCIEC(IZ,IEC3,GLBX,GLBY,GLBZ,CND,Y1,Y2,X1,X2)
          ENDIF
        ELSE
          ND2=ND1-NDZ+3
          DIST=DELZ/2.
          AREC=DELX*DELY
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        ENDIF

C RIGHT.
        ND2=N1
        DIST=DELX/2.
        AREC=DELY*DELZ
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        IF(IX.EQ.NTX)THEN
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND2,0.,AREC,0.,21,IZ,IS1
        ENDIF

C FRONT.
        ND2=N2
        DIST=DELY/2.
        AREC=DELX*DELZ
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        IF(IY.EQ.NTY)THEN
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND2,0.,AREC,0.,21,IZ,IS2
        ENDIF

C ABOVE.
        ND2=N3
        DIST=DELZ/2.
        AREC=DELX*DELY
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,CND,11,IZ,ND2
        IF(IL.EQ.NTZ)THEN
          IR=IR+1
          WRITE(ICNC,REC=IR,ERR=3)ND2,0.,AREC,0.,21,IZ,IS3
        ENDIF
   10 CONTINUE
      RETURN
    1 STOP "error (420): while writing to the control volumes file."
    2 STOP "error (421): while writing to the nodes temporary file."
    3 STOP "error (422): while writing to the connections file."
      END

C **************************   CNCISS   *******************************
C CNCISS creates the connections with the srf-srf connection surfaces.
C *********************************************************************
      SUBROUTINE CNCISS(IZ,ISS,GLBX,GLBY,GLBZ,COND,Y1,Y2,Z1,Z2)
#include "building.h"

      COMMON/GR3D52/NDZSS(MSSZ),DZSS(MSSZ,MZSS)
      COMMON/GR3D53/NDYSS(MSSZ),DYSS(MSSZ,MYSS)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D82/ISNDSS(MSSZ),ISNDSE(MSEZ),ISNDEE(MEEZ),ISNDEC(MECZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      ITYP=11
      XND2=0.
      ZSS2=0.
      DO 10 IDZ=1,NDZSS(ISS)
        ZSS1=ZSS2
        ZSS2=DZSS(ISS,IDZ)
        IF(Z1.GT.(ZSS2-1.E-4))THEN
          GOTO 10
        ELSEIF(Z2.LT.(ZSS1+1.E-4))THEN
          RETURN
        ELSE
          ZND2=(ZSS1+ZSS2)/2.
          ZSSMAX=AMIN1(ZSS2,Z2)
          ZSSMIN=AMAX1(ZSS1,Z1)
          YSS2=0.
          DO 20 IY=1,NDYSS(ISS)
            YSS1=YSS2
            YSS2=DYSS(ISS,IY)
            IF(Y1.GT.(YSS2-1.E-4))THEN
              GOTO 20
            ELSEIF(Y2.LT.(YSS1+1.E-4))THEN
              GOTO 10
            ELSE
              YND2=(YSS1+YSS2)/2.
              CALL GCOORD(-4,ISS,XND2,YND2,ZND2,GX2,GY2,GZ2)
              DIST=EFDIST(-4,ISS,GX2,GY2,GZ2,GLBX,GLBY,GLBZ)
              YSSMAX=AMIN1(YSS2,Y2)
              YSSMIN=AMAX1(YSS1,Y1)
              AREC=(ZSSMAX-ZSSMIN)*(YSSMAX-YSSMIN)
              ISP2=ISNDSS(ISS)+(IDZ-1)*NDYSS(ISS)+(IY-1)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,IZ,ISP2
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
    3 STOP "error (423): while writing to the connections file"
      END

C **************************   CNCISE   *******************************
C CNCISE creates the connections with the srf-edge connection surfaces.
C *********************************************************************
      SUBROUTINE CNCISE(IZ,ISE,GLBX,GLBY,GLBZ,COND,Y1,Y2,Z1,Z2)
#include "building.h"

      COMMON/GR3D57/NDZSE(MSEZ),DZSE(MSEZ,MZSE)
      COMMON/GR3D58/NDYSE(MSEZ),DYSE(MSEZ,MYSE)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D82/ISNDSS(MSSZ),ISNDSE(MSEZ),ISNDEE(MEEZ),ISNDEC(MECZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      ITYP=11
      XND2=0.
      ZSE2=0.
      DO 10 IDZ=1,NDZSE(ISE)
        ZSE1=ZSE2
        ZSE2=DZSE(ISE,IDZ)
        IF(Z1.GT.(ZSE2-1.E-4))THEN
          GOTO 10
        ELSEIF(Z2.LT.(ZSE1+1.E-4))THEN
          RETURN
        ELSE
          ZND2=(ZSE1+ZSE2)/2.
          ZSEMAX=AMIN1(ZSE2,Z2)
          ZSEMIN=AMAX1(ZSE1,Z1)
          YSE2=0.
          DO 20 IY=1,NDYSE(ISE)
            YSE1=YSE2
            YSE2=DYSE(ISE,IY)
            IF(Y1.GT.(YSE2-1.E-4))THEN
              GOTO 20
            ELSEIF(Y2.LT.(YSE1+1.E-4))THEN
              GOTO 10
            ELSE
              YND2=(YSE1+YSE2)/2.
              CALL GCOORD(-5,ISE,XND2,YND2,ZND2,GX2,GY2,GZ2)
              DIST=EFDIST(-5,ISE,GX2,GY2,GZ2,GLBX,GLBY,GLBZ)
              YSEMAX=AMIN1(YSE2,Y2)
              YSEMIN=AMAX1(YSE1,Y1)
              AREC=(ZSEMAX-ZSEMIN)*(YSEMAX-YSEMIN)
              ISP2=ISNDSE(ISE)+(IDZ-1)*NDYSE(ISE)+(IY-1)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,IZ,ISP2
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
    3 STOP "error (424): while writing to the connections file"
      END

C **************************   CNCIEE   *******************************
C CNCIEE creates the connections with the edge-edge connection surfaces.
C *********************************************************************
      SUBROUTINE CNCIEE(IZ,IEE,GLBX,GLBY,GLBZ,COND,X1,X2,Y1,Y2)
#include "building.h"

      COMMON/GR3D62/NDXEE(MEEZ),DXEE(MEEZ,MXEE)
      COMMON/GR3D63/NDYEE(MEEZ),DYEE(MEEZ,MYEE)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D82/ISNDSS(MSSZ),ISNDSE(MSEZ),ISNDEE(MEEZ),ISNDEC(MECZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      ITYP=11
      ZND2=0.
      YEE2=0.
      DO 10 IY=1,NDYEE(IEE)
        YEE1=YEE2
        YEE2=DYEE(IEE,IY)
        IF(Y1.GT.(YEE2-1.E-4))THEN
          GOTO 10
        ELSEIF(Y2.LT.(YEE1+1.E-4))THEN
          RETURN
        ELSE
          YND2=(YEE1+YEE2)/2.
          YEEMAX=AMIN1(YEE2,Y2)
          YEEMIN=AMAX1(YEE1,Y1)
          XEE2=0.
          DO 20 IX=1,NDXEE(IEE)
            XEE1=XEE2
            XEE2=DXEE(IEE,IX)
            IF(X1.GT.(XEE2-1.E-4))THEN
              GOTO 20
            ELSEIF(X2.LT.(XEE1+1.E-4))THEN
              GOTO 10
            ELSE
              XND2=(XEE1+XEE2)/2.
              CALL GCOORD(-6,IEE,XND2,YND2,ZND2,GX2,GY2,GZ2)
              DIST=EFDIST(-6,IEE,GX2,GY2,GZ2,GLBX,GLBY,GLBZ)
              XEEMAX=AMIN1(XEE2,X2)
              XEEMIN=AMAX1(XEE1,X1)
              AREC=(XEEMAX-XEEMIN)*(YEEMAX-YEEMIN)
              ISP2=ISNDEE(IEE)+(IY-1)*NDXEE(IEE)+(IX-1)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=1)ND1,DIST,AREC,COND,ITYP,IZ,ISP2
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
    1 STOP "error (425): while writing to connections file."
      END

C **************************   CNCIEC   *******************************
C CNCIEC creates the connections with edge-corner connection surfaces.
C *********************************************************************
      SUBROUTINE CNCIEC(IZ,IEC,GLBX,GLBY,GLBZ,COND,X1,X2,Y1,Y2)
#include "building.h"

      COMMON/GR3D67/NDXEC(MECZ),DXEC(MECZ,MXEC)
      COMMON/GR3D68/NDYEC(MECZ),DYEC(MECZ,MYEC)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D82/ISNDSS(MSSZ),ISNDSE(MSEZ),ISNDEE(MEEZ),ISNDEC(MECZ)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      ITYP=11
      ZND2=0.
      YEC2=0.
      DO 10 IY=1,NDYEC(IEC)
        YEC1=YEC2
        YEC2=DYEC(IEC,IY)
        IF(Y1.GT.(YEC2-1.E-4))THEN
          GOTO 10
        ELSEIF(Y2.LT.(YEC1+1.E-4))THEN
          RETURN
        ELSE
          YND2=(YEC1+YEC2)/2.
          YECMAX=AMIN1(YEC2,Y2)
          YECMIN=AMAX1(YEC1,Y1)
          XEC2=0.
          DO 20 IX=1,NDXEC(IEC)
            XEC1=XEC2
            XEC2=DXEC(IEC,IX)
            IF(X1.GT.(XEC2-1.E-4))THEN
              GOTO 20
            ELSEIF(X2.LT.(XEC1+1.E-4))THEN
              GOTO 10
            ELSE
              XND2=(XEC1+XEC2)/2.
              CALL GCOORD(-7,IEC,XND2,YND2,ZND2,GX2,GY2,GZ2)
              DIST=EFDIST(-7,IEC,GX2,GY2,GZ2,GLBX,GLBY,GLBZ)
              XECMAX=AMIN1(XEC2,X2)
              XECMIN=AMAX1(XEC1,X1)
              AREC=(XECMAX-XECMIN)*(YECMAX-YECMIN)
              ISP2=ISNDEC(IEC)+(IY-1)*NDXEC(IEC)+(IX-1)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,IZ,ISP2
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE
      RETURN
    3 STOP "error (426): while writing to the connections file."
      END
