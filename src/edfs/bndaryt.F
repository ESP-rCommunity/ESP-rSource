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

C Contains:
C  BNDART - Impose BCs (all except thermal) when k-epsilon model is active.
C  BNDARTT - Impose thermal BCs for all turbulence models.
C  BNDMIT - Impose BCs for momentum equations when MIT 0-eqn model is active.

C ********************* BNDART *********************
C BNDART - Impose boundary conditions (all except thermal) when k-epsilon
C          model is active.
C This subroutine has the following entry's:
C  MODUT  - U direction boundary conditions - turbulent.
C  MODVT  - V direction boundary conditions - turbulent.
C  MODWT  - W direction boundary conditions - turbulent.
C  MODPP  - pressure primed - SIMPLE
C  MODPP1 - pressure primed - SIMPLEC
C  MODTE  - Turbulent energy boundary conditions.
C  MODED  - Energy dissipation - turbulent.
C
C  Types of boundary conditions used {IVTYPE(IV,ICFD)=}:
C  VOLUME HEAT SOURCE = 20
C
      SUBROUTINE BNDART
#include "cfd.h"

      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLN(MCFND,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/WALLF/TAUEV(ntcelx,ntcely,ntcelz),
     1          TAUWV(ntcelx,ntcely,ntcelz),TAUEW(ntcelx,ntcely,ntcelz),
     2          TAUWW(ntcelx,ntcely,ntcelz),TAUNU(ntcelx,ntcely,ntcelz),
     3          TAUSU(ntcelx,ntcely,ntcelz),TAUNW(ntcelx,ntcely,ntcelz),
     4          TAUSW(ntcelx,ntcely,ntcelz),TAUHU(ntcelx,ntcely,ntcelz),
     5          TAULU(ntcelx,ntcely,ntcelz),TAUHV(ntcelx,ntcely,ntcelz),
     6          TAULV(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/CONST/GREAT,small,GRAV
      COMMON/SUSP/SUKD(ntcelx,ntcely,ntcelz),SPKD(ntcelx,ntcely,ntcelz)
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/MFS/IMFACT
      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)
      COMMON/YUANcm/Uqstor(ntcelx,ntcely,ntcelz),Uqinit

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ,Uqinit,OK

C ********************* MODUT *********************
C MODUT - U direction boundary conditions - turbulent.
      ENTRY MODUT

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALV)THEN
        J=2
        DO 1100 I=1,NI
          DO 1101 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1101     CONTINUE
 1100   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 1200 I=1,NI
          DO 1201 J=1,NJ
            AH(I,J,K)=0.0
            AL(I,J,K)=0.0
 1201     CONTINUE
 1200   CONTINUE
      ENDIF

      CDTERM=CMU**0.25

C Solid walls.
      DO 101 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        ISBii=ISBi(L,ICFD)

C To change/ test - sometimes produces non zero coefficients at walls!
        DO 800 M=1,NOPEN(ICFD)
          IF(ISBi(L,ICFD).EQ.IOPENf(M,ICFD)+1) THEN
            ISBii=ISBii+1
            GOTO 801
          ENDIF
 800    CONTINUE

C Set up boundary conditions on South solid wall.
 801    IF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          DO 102 K=KSBi(L,ICFD),KSBf(L,ICFD)

C This code is a proposed solution to the procedure in lines 143 - 148.
C Commented for the time being, as it needs further testing. Also
C present in MODVT and MODWT.

C Because the velocity grid is staggered, handling of grid cells at the
C boundary between BCs can be complicated, as they are half bounded by
C one BC and half by the other in the relevant direction (in this case
C the I direction). The convention is that each solid boundary condition
C handles velocity grid cells with indices corresponding to its scalar
C grid cells. The velocity grid is offset negatively, so the first row
C in the relevant direction is the contentious one. 
C How this is handled depends on what BC is before this one in the I
C direction, in this K row. If it is another solid BC, handle as usual
C (wall temperatures are not factored into calculations here, so no
C interpolation is needed). If it is an opening, do nothing, and allow
C this grid cell to be handled by the opening boundary condition (INOUT
C in cfcalc.F). If it is inside a boundary in a different direction
C (i.e. in a corner), also do nothing, as it will be handled by the
C boundary condition in that direction.
C             I=ISBi(L,ICFD)
C             if (I.eq.2) then
C               ISBii=I+1
C             else
C               ISBii=I
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I-1,J,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   ISBii=I+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 1122 I=ISBii,ISBf(L,ICFD)
              TMULT=TRBSRCU(I,J,K,DYS)
              TAUSU(I,J,K)=-TMULT*Uf(I,J,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaNSU(I,K)
              AS(I,J,K)=0.0
 1122       CONTINUE
 102      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 110 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUSU(II,J,K)=TAUSU(II+1,J,K)
            TAUSU(IFF+1,J,K)=TAUSU(IFF,J,K)
 110      CONTINUE

C Set up boundary conditions on North solid wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          DO 112 K=KSBi(L,ICFD),KSBf(L,ICFD)

C             I=ISBi(L,ICFD)
C             if (I.eq.2) then
C               ISBii=I+1
C             else
C               ISBii=I
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I-1,J,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   ISBii=I+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 1121 I=ISBii,ISBf(L,ICFD)
              TMULT=TRBSRCU(I,J,K,DYN)
              TAUNU(I,J,K)=-TMULT*Uf(I,J,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaNSU(I,K)
              AN(I,J,K)=0.0
 1121       CONTINUE
 112      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 120 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUNU(II,J,K)=TAUNU(II+1,J,K)
            TAUNU(IFF+1,J,K)=TAUNU(IFF,J,K)
 120      CONTINUE

C Set up boundary conditions on Low solid wall.
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          DO 122 J=JSBi(L,ICFD),JSBf(L,ICFD)            

C             I=ISBi(L,ICFD)
C             if (I.eq.2) then
C               ISBii=I+1
C             else
C               ISBii=I
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I-1,J,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   ISBii=I+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 1221 I=ISBii,ISBf(L,ICFD)
              TMULT=TRBSRCU(I,J,K,DZL)
              TAULU(I,J,K)=-TMULT*Uf(I,J,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaHLU(I,J)
              AL(I,J,K)=0.0
 1221       CONTINUE
 122      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 130 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAULU(II,J,K)=TAULU(II+1,J,K)
            TAULU(IFF+1,J,K)=TAULU(IFF,J,K)
 130      CONTINUE

C Set up boundary conditions on High solid wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          DO 132 J=JSBi(L,ICFD),JSBf(L,ICFD)            

C             I=ISBi(L,ICFD)
C             if (I.eq.2) then
C               ISBii=I+1
C             else
C               ISBii=I
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I-1,J,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   ISBii=I+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 1321 I=ISBii,ISBf(L,ICFD)
              TMULT=TRBSRCU(I,J,K,DZH)
              TAUHU(I,J,K)=-TMULT*Uf(I,J,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaHLU(I,J)
              AH(I,J,K)=0.0
 1321       CONTINUE
 132      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 140 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUHU(II,J,K)=TAUHU(II+1,J,K)
            TAUHU(IFF+1,J,K)=TAUHU(IFF,J,K)
 140      CONTINUE

C Symmetric Axis.
C South Wall.
        ELSEIF(location.EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 143 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1431 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J-1,K)=-GREAT
C              SU(I,J-1,K)=GREAT*Uf(I,J,K)
              Uf(I,J-1,K)=Uf(I,J,K)
              AS(I,J,K)=0.0
 1431       CONTINUE
 143      CONTINUE

C North Wall.
        ELSEIF(location.EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 145 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1451 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*Uf(I,J,K)
              Uf(I,J+1,K)=Uf(I,J,K)
              AN(I,J,K)=0.0
 1451       CONTINUE
 145      CONTINUE

C Low Wall.
        ELSEIF(location.EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 148 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1481 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K-1)=-GREAT
C              SU(I,J,K-1)=GREAT*Uf(I,J,K)
              Uf(I,J,K-1)=Uf(I,J,K)
              AL(I,J,K)=0.0
 1481       CONTINUE
 148      CONTINUE

C High Wall.
        ELSEIF(location.EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 150 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1501 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K+1)=-GREAT
C              SU(I,J,K+1)=GREAT*Uf(I,J,K)
              Uf(I,J,K+1)=Uf(I,J,K)
              AH(I,J,K)=0.0
 1501       CONTINUE
 150      CONTINUE
        ENDIF
 101  CONTINUE

C Momentum BCs for openings were formerly handled here, but now are treated
C in INOUT.

C Airflow at inlet not perpendicular to wall. Examine each opening and
C treat those that have `directed. flow.
      DO 10 L=1,NOPEN(ICFD)
        I=IOPENi(L,ICFD)
C South wall.
        IF(IWOPEN(L,ICFD).EQ.3.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 180 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J-1,K)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1801       CONTINUE
 180      CONTINUE

C North wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.4.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 185 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J+1,K)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1851       CONTINUE
 185      CONTINUE

C Low wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.5.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 190 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1901 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J,K-1)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1901       CONTINUE
 190      CONTINUE

C High wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.6.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 195 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1951 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J,K+1)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1951       CONTINUE
 195      CONTINUE
        ENDIF
 10   CONTINUE

      RETURN


C ********************* MODVT *********************
C MODVT -  V direction boundary conditions - turbulent.
      ENTRY MODVT

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 1300 J=1,NJ
          DO 1301 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 1301     CONTINUE
 1300   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 1400 I=1,NI
          DO 1401 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 1401     CONTINUE
 1400   CONTINUE
      ENDIF

      CDTERM=CMU**0.25

C Solid walls.
      DO 201 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        JSBii=JSBi(L,ICFD)

C To change/ test - sometimes produces non zero coefficients at walls!
        DO 810 M=1,NOPEN(ICFD)
          IF(JSBi(L,ICFD).EQ.JOPENf(M,ICFD)+1) THEN
            JSBii=JSBii+1
            GOTO 811
          ENDIF
 810    CONTINUE

 811    IF(location.EQ.1)THEN

C Set up boundary conditions for West solid wall.
          I=ISBi(L,ICFD)

C Calculate distance from wall to next-to-wall grid point ($DELTA y sub p$).
          DXW=XP(I)-XU(I)

C Treat each cell on solid boundary.
          DO 202 K=KSBi(L,ICFD),KSBf(L,ICFD)

C             J=JSBi(L,ICFD)
C             if (J.eq.2) then
C               JSBii=J+1
C             else
C               JSBii=J
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J-1,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   JSBii=J+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 2021 J=JSBii,JSBf(L,ICFD)
              TMULT=TRBSRCV(I,J,K,DXW)

C Calculate wall shear stress (+ value means stress operates in axis direction.
              TAUWV(I,J,K)=-TMULT*Vf(i,j,K)

C Calculate source term. Units are force/velocity. Surface area of cell = SNSV*SHL.
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaEWV(J,K)

C Disconnect cell from fictitious cell to west.
              AW(I,J,K)=0.0
 2021       CONTINUE
 202      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 210 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUWV(I,JI,K)=TAUWV(I,JI+1,K)
            TAUWV(I,JF+1,K)=TAUWV(I,JF,K)
 210      CONTINUE

        ELSEIF(location.EQ.2)THEN

C Set up boundary conditions for East solid wall.
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          DO 212 K=KSBi(L,ICFD),KSBf(L,ICFD)

C             J=JSBi(L,ICFD)
C             if (J.eq.2) then
C               JSBii=J+1
C             else
C               JSBii=J
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J-1,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   JSBii=J+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 2121 J=JSBii,JSBf(L,ICFD)
              TMULT=TRBSRCV(I,J,K,DXE)
              TAUEV(I,J,K)=-TMULT*Vf(i,j,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaEWV(J,K)
              AE(I,J,K)=0.0
 2121       CONTINUE
 212      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 220 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUEV(I,JI,K)=TAUEV(I,JI+1,K)
            TAUEV(I,JF+1,K)=TAUEV(I,JF,K)
 220      CONTINUE

        ELSEIF(location.EQ.5)THEN

C Set up boundary conditions on Low solid wall.
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          DO 222 I=ISBi(L,ICFD),ISBf(L,ICFD)

C             J=JSBi(L,ICFD)
C             if (J.eq.2) then
C               JSBii=J+1
C             else
C               JSBii=J
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J-1,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   JSBii=J+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 2221 J=JSBii,JSBf(L,ICFD)
              TMULT=TRBSRCV(I,J,K,DZL)
              TAULV(I,J,K)=-TMULT*Vf(i,j,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaHLV(I,J)
              AL(I,J,K)=0.0
 2221       CONTINUE
 222      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 230 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAULV(I,JI,K)=TAULV(I,JI+1,K)
            TAULV(I,JF+1,K)=TAULV(I,JF,K)
 230      CONTINUE

        ELSEIF(location.EQ.6)THEN

C Set up boundary conditions on High solid wall.
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          DO 232 I=ISBi(L,ICFD),ISBf(L,ICFD)

C             J=JSBi(L,ICFD)
C             if (J.eq.2) then
C               JSBii=J+1
C             else
C               JSBii=J
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J-1,K,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   JSBii=J+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 2321 J=JSBii,JSBf(L,ICFD)
              TMULT=TRBSRCV(I,J,K,DZH)
              TAUHV(I,J,K)=-TMULT*Vf(i,j,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaHLV(I,J)
              AH(I,J,K)=0.0
 2321       CONTINUE
 232      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 240 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUHV(I,JI,K)=TAUHV(I,JI+1,K)
            TAUHV(I,JF+1,K)=TAUHV(I,JF,K)
 240      CONTINUE

C Symetric axis.
C Set up boundary conditions for West solid wall.
        ELSEIF(location.EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 250 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2501 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I-1,J,K)=-GREAT
C              SU(I-1,J,K)=GREAT*Vf(I,J,K)
              Vf(I-1,J,K)=Vf(I,J,K)
              AW(I,J,K)=0.0
 2501       CONTINUE
 250      CONTINUE

C Set up boundary conditions for East solid wall.
        ELSEIF(location.EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 253 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2531 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*Vf(I,J,K)
              Vf(I+1,J,K)=Vf(I,J,K)
              AE(I,J,K)=0.0
 2531       CONTINUE
 253      CONTINUE

C Set up boundary conditions on Low solid wall.
        ELSEIF(location.EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 255 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2551 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K-1)=-GREAT
C              SU(I,J,K-1)=GREAT*Vf(I,J,K)
              Vf(I,J,K-1)=Vf(I,J,K)
              AL(I,J,K)=0.0
 2551       CONTINUE
 255      CONTINUE

C Set up boundary conditions on High solid wall.
        ELSEIF(location.EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 258 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2581 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K-1)=-GREAT
C              SU(I,J,K-1)=GREAT*Vf(I,J,K)
              Vf(I,J,K-1)=Vf(I,J,K)
              AH(I,J,K)=0.0
 2581       CONTINUE
 258      CONTINUE
        ENDIF
 201  CONTINUE

C Momentum BCs for openings were formerly handled here, but now are treated
C in INOUT.

C Airflow at inlet not perpendicular to wall. Examine each opening and
C treat those that have "directed" flow.
      DO 241 L=1,NOPEN(ICFD)
        J=JOPENi(L,ICFD)
C West wall.
        IF(IWOPEN(L,ICFD).EQ.1.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 280 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 2801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I-1,J,K)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 2801       CONTINUE
 280      CONTINUE

C East wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.2.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 285 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 2851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I+1,J,K)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 2851       CONTINUE
 285      CONTINUE

C Low wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.5.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 290 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2901 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA2(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
              ELSEIF(THETA2(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I,J,K-1)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                ENDIF
              ENDIF
 2901       CONTINUE
 290      CONTINUE

C High wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.6.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 295 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2951 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA2(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
              ELSEIF(THETA2(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I,J,K+1)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                ENDIF
              ENDIF
 2951       CONTINUE
 295      CONTINUE
        ENDIF
 241  CONTINUE

      RETURN


C ********************* MODWT *********************
C MODWT - W direction boundary conditions - turbulent.
      ENTRY MODWT

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 1500 J=1,NJ
          DO 1511 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 1511     CONTINUE
 1500   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 1600 I=1,NI
          DO 1601 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1601   CONTINUE
 1600   CONTINUE
      ENDIF

      CDTERM=CMU**0.25

C Solid walls.
C Two types of wall functions are supported for the k-epsilon model:
C the log-law wall functions and the Yuan wall functions. Use of the latter
C is indicated by IWSB numbers in the 60's. The log-law wall functions are
C used for all other IWSB values.

C If the Yuan wall functions are active and this is the first time they are
C applied this bps time-step, then the velocity scale based on the heat flux (Uq)
C must be initialized. This variable is an outcome of the application of Yuan's
C thermal wall function. However, because dfs solves the momentum equations
C before the thermal equations, on the first iteration Uq must be initialized.
C Otherwise, the dimensionless velocity (ustst) will be calculate as zero,
C which will lead to dividing by zero in updating the friction velocity.
C If the Yuan wall functions are active on any of the vertical surfaces and the
C Yuan wall functions have not been applied this bps time-step (Uqinit is false
C they have not been applied), then initialize Uq. The value of 0.02 was
C arbitrarily selected but seems to be in the correct ballpark.
      IF(.not.Uqinit)THEN
        DO 99 L=1,NSB(ICFD)
          IF( IWSB(L,ICFD).ge.61 .and. IWSB(L,ICFD).le.64 )THEN
            DO 98 I=1,NI
              DO 97 J=1,NJ
                DO 96 K=1,NK
                  Uqstor(i,j,k) = 0.02
   96           CONTINUE
   97         CONTINUE
   98       CONTINUE
            Uqinit = .true.
          ENDIF
   99   CONTINUE
      ENDIF

C Examine each solid boundary in turn and apply the appropriate wall function.
      DO 3013 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        KSBii=KSBi(L,ICFD)

C To change/ test - sometimes produces non zero coefficients at walls!
        DO 820 M=1,NOPEN(ICFD)
          IF(KSBi(L,ICFD).EQ.KOPENf(M,ICFD)+1) THEN
            KSBii=KSBii+1
            GOTO 821
          ENDIF
 820    CONTINUE

C Set up boundary conditions for West solid wall if log-law wall functions in use.
 821    IF(location.EQ.1 .and. IWSB(L,ICFD).ne.61)THEN
          I=ISBi(L,ICFD)
          DXW=XP(I)-XU(I)
          DO 302 J=JSBi(L,ICFD),JSBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3021 K=KSBii,KSBf(L,ICFD)
              TMULT=TRBSRCW(I,J,K,DXW)
              TAUWW(I,J,K)=-TMULT*Wf(i,j,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaEWW(J,K)
              AW(I,J,K)=0.0
 3021       CONTINUE
 302      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 3104 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUWW(I,J,KI)=TAUWW(I,J,KI+1)
            TAUWW(I,J,KF+1)=TAUWW(I,J,KF)
 3104     CONTINUE

C Set up boundary conditions for East solid wall if log-law wall functions in use.
        ELSEIF(location.EQ.2 .and. IWSB(L,ICFD).ne.62)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          DO 312 J=JSBi(L,ICFD),JSBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3121 K=KSBii,KSBf(L,ICFD)
              TMULT=TRBSRCW(I,J,K,DXE)
              TAUEW(I,J,K)=-TMULT*Wf(i,j,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaEWW(J,K)
              AE(I,J,K)=0.0
 3121       CONTINUE
 312      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 320 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUEW(I,J,KI)=TAUEW(I,J,KI+1)
            TAUEW(I,J,KF+1)=TAUEW(I,J,KF)
 320      CONTINUE

C Set up boundary conditions on South solid wall if log-law wall functions in use.
        ELSEIF(location.EQ.3 .and. IWSB(L,ICFD).ne.63)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          DO 322 I=ISBi(L,ICFD),ISBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3221 K=KSBii,KSBf(L,ICFD)
              TMULT=TRBSRCW(I,J,K,DYS)
              TAUSW(I,J,K)=-TMULT*Wf(I,J,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaNSW(I,K)
              AS(I,J,K)=0.0
 3221       CONTINUE
 322      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 3308 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUSW(I,J,KI)=TAUSW(I,J,KI+1)
            TAUSW(I,J,KF+1)=TAUSW(I,J,KF)
 3308     CONTINUE

C Set up boundary conditions on North solid wall if log-law wall functions in use.
        ELSEIF(location.EQ.4 .and. IWSB(L,ICFD).ne.64)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          DO 332 I=ISBi(L,ICFD),ISBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3321 K=KSBii,KSBf(L,ICFD)
              TMULT=TRBSRCW(I,J,K,DYN)
              TAUNW(I,J,K)=-TMULT*Wf(I,J,K)
              SP(I,J,K)=SP(I,J,K)-TMULT*AreaNSW(I,K)
              AN(I,J,K)=0.0
 3321       CONTINUE
 332      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 3404 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUNW(I,J,KI)=TAUNW(I,J,KI+1)
            TAUNW(I,J,KF+1)=TAUNW(I,J,KF)
 3404     CONTINUE

C Set up boundary conditions on West solid wall if Yuan wall functions in use.
        ELSEIF(IWSB(L,ICFD).eq.61)THEN
          I=ISBi(L,ICFD)
          DXW=XP(I)-XU(I)
          DO 902 J=JSBi(L,ICFD),JSBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3921 K=KSBii,KSBf(L,ICFD)
              Utau = YUANBC(I,J,K,DXW)

C Since Utau forced to positive determine direction of shear stress
C based on direction of velocity component in vertical direction
C (+ value means stress operates in axis direction).
              if( Wf(i,j,K).lt.0.)then
                TAUWW(I,J,K) = DENf(i,j,k) * Utau**2
              else
                TAUWW(I,J,K) = - DENf(i,j,k) * Utau**2
              endif

C Disconnect cell from boundary and set source term. Source term units are
C force/velocity. Surface area of cell = SNS*SHLW. Protect the calculation
C against the case of zero velocity.
C              SP(I,J,K)=SP(I,J,K)+TAUWW(I,J,K)*AreaEWW(J,K) /
C     &                  ( Wf(i,j,K) + small )
              SP(I,J,K)=SP(I,J,K)+TAUWW(I,J,K)*AreaEWW(J,K) /
     &                  anotzero(Wf(i,j,K))
              AW(I,J,K)=0.0
 3921       CONTINUE
 902      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 390 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUWW(I,J,KI)=TAUWW(I,J,KI+1)
            TAUWW(I,J,KF+1)=TAUWW(I,J,KF)
 390      CONTINUE

C Set up boundary conditions for East solid wall if Yuan wall functions in use.
        ELSEIF(IWSB(L,ICFD).eq.62)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          DO 912 J=JSBi(L,ICFD),JSBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3991 K=KSBii,KSBf(L,ICFD)
              Utau = YUANBC(I,J,K,DXE)
              if( Wf(i,j,K).lt.0.)then
                TAUEW(I,J,K) = DENf(i,j,k) * Utau**2
              else
                TAUEW(I,J,K) = - DENf(i,j,k) * Utau**2
              endif
              SP(I,J,K)=SP(I,J,K)+TAUEW(I,J,K)*AreaEWW(J,K) /
     &                   anotzero(Wf(i,j,K))
              AE(I,J,K)=0.0
 3991       CONTINUE
 912      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 391 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUEW(I,J,KI)=TAUEW(I,J,KI+1)
            TAUEW(I,J,KF+1)=TAUEW(I,J,KF)
 391      CONTINUE

C Set up boundary conditions on South solid wall if Yuan wall functions in use.
        ELSEIF(IWSB(L,ICFD).eq.63)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          DO 922 I=ISBi(L,ICFD),ISBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3929 K=KSBii,KSBf(L,ICFD)
              Utau = YUANBC(I,J,K,DYS)
              if( Wf(i,j,K).lt.0. )then
                TAUSW(I,J,K) = DENf(i,j,k) * Utau**2
              else
                TAUSW(I,J,K) = - DENf(i,j,k) * Utau**2
              endif
              SP(I,J,K)=SP(I,J,K)+TAUSW(I,J,K)*AreaNSW(I,K) /
     &                   anotzero(Wf(i,j,K))
              AS(I,J,K)=0.0
 3929       CONTINUE
 922      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 392 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUSW(I,J,KI)=TAUSW(I,J,KI+1)
            TAUSW(I,J,KF+1)=TAUSW(I,J,KF)
 392      CONTINUE

C Set up boundary conditions on North solid wall if Yuan wall functions in use.
        ELSEIF(IWSB(L,ICFD).eq.64)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          DO 932 I=ISBi(L,ICFD),ISBf(L,ICFD)

C             K=KSBi(L,ICFD)
C             if (K.eq.2) then
C               KSBii=K+1
C             else
C               KSBii=K
C               do M=1,NOPEN(ICFD)
C                 OK=LINVOL(I,J,K-1,
C      &            IOPENi(M,ICFD),IOPENf(M,ICFD),
C      &            JOPENi(M,ICFD),JOPENf(M,ICFD),
C      &            KOPENi(M,ICFD),KOPENf(M,ICFD))
C                 if (OK) then 
C                   KSBii=K+1
C                   EXIT
C                 endif
C               enddo
C             endif

            DO 3992 K=KSBii,KSBf(L,ICFD)
              Utau = YUANBC(I,J,K,DYN)
              if( Wf(i,j,K).lt.0. )then
                TAUNW(I,J,K) = DENf(i,j,k) * Utau**2
              else
                TAUNW(I,J,K) = - DENf(i,j,k) * Utau**2
              endif
              SP(I,J,K)=SP(I,J,K)+TAUNW(I,J,K)*AreaNSW(I,K) /
     &                   anotzero(Wf(i,j,K))
              AN(I,J,K)=0.0
 3992       CONTINUE
 932      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 393 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUNW(I,J,KI)=TAUNW(I,J,KI+1)
            TAUNW(I,J,KF+1)=TAUNW(I,J,KF)
 393      CONTINUE


C Symetric axis.
        ELSEIF(location.EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 350 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3501 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I-1,J,K)=-GREAT
C              SU(I-1,J,K)=GREAT*Wf(I,J,K)
              Wf(I-1,J,K)=Wf(I,J,K)
              AW(I,J,K)=0.0
 3501       CONTINUE
 350      CONTINUE
        ELSEIF(location.EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 353 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3531 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*Wf(I,J,K)
              Wf(I+1,J,K)=Wf(I,J,K)
              AE(I,J,K)=0.0
 3531       CONTINUE
 353      CONTINUE
        ELSEIF(location.EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 355 I=ISBi(L,ICFD),ISBf(L,ICFD)
          DO 355 K=KSBi(L,ICFD),KSBf(L,ICFD)
C            SP(I,J-1,K)=-GREAT
C            SU(I,J-1,K)=GREAT*Wf(I,J,K)
            Wf(I,J-1,K)=Wf(I,J,K)
            AS(I,J,K)=0.0
 355      CONTINUE
        ELSEIF(location.EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 358 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3581 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*Wf(I,J,K)
              Wf(I,J+1,K)=Wf(I,J,K)
              AN(I,J,K)=0.0
 3581       CONTINUE
 358      CONTINUE
        ENDIF
 3013 CONTINUE

C Momentum BCs for openings were formerly handled here, but now are treated
C in INOUT.

C Airflow at inlet not perpendicular to wall. Examine each opening and
C treat those that have `directed. flow.
      DO 341 L=1,NOPEN(ICFD)
        J=JOPENi(L,ICFD)
C West wall.
        IF(IWOPEN(L,ICFD).EQ.1.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 380 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 3801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I-1,J,K)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 3801       CONTINUE
 380      CONTINUE

C East wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.2.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 385 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 3851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I+1,J,K)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 3851       CONTINUE
 385      CONTINUE
        I=IOPENi(L,ICFD)

C South wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.3.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 3902 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3901 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J-1,K)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 3901       CONTINUE
 3902     CONTINUE

C North wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.4.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 395 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3951 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J+1,K)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 3951       CONTINUE
 395      CONTINUE
        endif
 341  continue

      RETURN


C ********************* MODPP *********************
C MODPP - Pressure correction for SIMPLE.
      ENTRY MODPP

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 1700 J=1,NJ
          DO 1701 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 1701     CONTINUE
 1700   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 1800 I=1,NI
          DO 1802 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1802     CONTINUE
 1800   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 1900 I=1,NI
          DO 1902 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 1902     CONTINUE
 1900   CONTINUE
      ENDIF

      RETURN


C ********************* MODPP1 *********************
C MODPP1 - Pressure correction for SIMPLEC.
      ENTRY MODPP1

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 2000 J=1,NJ
          DO 2001 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 2001     CONTINUE
 2000   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 2100 I=1,NI
          DO 2101 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 2101     CONTINUE
 2100   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 2200 I=1,NI
          DO 2201 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 2201     CONTINUE
 2200   CONTINUE
      ENDIF

C Openings.
      DO 371 L=1,NOPEN(ICFD)
        IF(IWOPEN(L,ICFD).EQ.0.OR.(IWOPEN(L,ICFD).GE.1.AND.
     &     IWOPEN(L,ICFD).LE.6.AND.IMFACT.EQ.1))THEN

C Pressure or velocity.
          DO 372 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3721 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              DO 3722 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
                SP(I,J,K)=-GREAT
                SU(I,J,K)=0.0
 3722         CONTINUE
 3721       CONTINUE
 372      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.99)THEN

C Mass.
          DO 373 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3731 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              DO 3732 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
                SU(I,J,K)=SU(I,J,K)+FIXM(M,ICFD)
 3732         CONTINUE
 3731       CONTINUE
 373      CONTINUE
        ENDIF
 371  CONTINUE

      RETURN


C ********************* MODTE *********************
C MODTE - Turbulent energy boundary conditions.
      ENTRY MODTE

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 2600 J=1,NJ
          DO 2601 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 2601     CONTINUE
 2600   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 2700 I=1,NI
          DO 2701 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 2701     CONTINUE
 2700   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 2800 I=1,NI
          DO 2802 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 2802     CONTINUE
 2800   CONTINUE
      ENDIF

      CDTERM=CMU**0.25

C In case of fixed mass or pressure:
C If an inlet -> Add energy as: Min*Kin
      DO 598 M=1,NOPEN(ICFD)
        IF(IWOPEN(M,ICFD).EQ.0.OR.IWOPEN(M,ICFD).EQ.99)THEN
          IF(FIXM(M,ICFD).GT.0)THEN
            DO 599 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 5991 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                DO 5992 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                  SU(I,J,K)=SU(I,J,K)+FIXM(M,ICFD)*FIXK(M,ICFD)
 5992           CONTINUE
 5991         CONTINUE
 599        CONTINUE
          ENDIF
        ENDIF

C Outlets.
        IF(FIXM(M,ICFD).LE.0.0) THEN

C West wall.
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN
            I=IOPENi(M,ICFD)
            DO 1000 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 1001 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
 1001         CONTINUE
 1000       CONTINUE

C East wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.2) THEN
            I=IOPENi(M,ICFD)
            DO 1002 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 1003 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AE(I,J,K)=0.0
 1003         CONTINUE
 1002       CONTINUE

C South wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.3) THEN
            J=JOPENi(M,ICFD)
            DO 1004 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1005 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AS(I,J,K)=0.0
 1005         CONTINUE
 1004       CONTINUE

C North wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.4) THEN
            J=JOPENi(M,ICFD)
            DO 1006 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1007 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AN(I,J,K)=0.0
 1007         CONTINUE
 1006       CONTINUE

C Low wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.5) THEN
            K=KOPENi(M,ICFD)
            DO 1008 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1009 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AL(I,J,K)=0.0
 1009         CONTINUE
 1008       CONTINUE

C High wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.6) THEN
            K=KOPENi(M,ICFD)
            DO 1010 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1011 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AH(I,J,K)=0.0
 1011         CONTINUE
 1010       CONTINUE
          ENDIF
        ENDIF
 598  CONTINUE

C Solid walls.
      DO 601 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C West wall.
        IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)
          DXW=XP(I)-XU(I)
          DO 602 J=JSBi(L,ICFD),JSBf(L,ICFD)
           DO 6021 K=KSBi(L,ICFD),KSBf(L,ICFD)
            DENV=DENf(i,j,K)
            SQRTK=SQRT(ABS(TEf(i,j,K)))
            VOL=VolP(I,J,K)
            Tvis=VIS(I,J,K)-VISCOS
            if (Tvis.lt.1e-19) Tvis=0.
            XPLUSW=DENV*SQRTK*CDTERM*DXW/VISCOS
            GENCOU=0.5*(ABS(TAUWV(I,J+1,K)*Vf(i,j+1,K))+ABS(TAUWV(I,J,K)
     1            *Vf(i,j,K)))/DXW+0.5*(ABS(TAUWW(I,J,K+1)*Wf(i,j,K+1))+
     2            ABS(TAUWW(I,J,K)*Wf(i,j,K)))/DXW

C Get velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GENRES=GEN(I,J,K)-TVIS*((DUDY+DVDX)**2+
     1             (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)

            GEN(I,J,K)=GENRES+GENCOU
            DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*XPLUSW/DXW
            IF(XPLUSW.GT.11.63)DITERM=CD*DENf(i,j,K)*(CMU**.75)*
     &             SQRTK*ALOG(ELOG*XPLUSW)/(CAPPA*DXW)
            SU(I,J,K)=SUKD(I,J,K)+GEN(I,J,K)*VOL
            SP(I,J,K)=SPKD(I,J,K)-DITERM*VOL
            AW(I,J,K)=0.0
 6021      CONTINUE
 602      CONTINUE

C East wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          DO 612 J=JSBi(L,ICFD),JSBf(L,ICFD)
           DO 6121 K=KSBi(L,ICFD),KSBf(L,ICFD)
            DENV=DENf(i,j,K)
            SQRTK=SQRT(ABS(TEf(i,j,K)))
            VOL=VolP(I,J,K)
            Tvis=VIS(I,J,K)-VISCOS
            if (Tvis.lt.1e-19) Tvis=0.
            XPLUSE=DENV*SQRTK*CDTERM*DXE/VISCOS
            GENCOU=0.5*(ABS(TAUEV(I,J+1,K)*Vf(i,j+1,K))+ABS(TAUEV(I,J,K)
     1            *Vf(i,j,K)))/DXE+0.5*(ABS(TAUEW(I,J,K+1)*Wf(i,j,K+1))+
     2             ABS(TAUEW(I,J,K)*Wf(i,j,K)))/DXE

C Get velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GENRES=GEN(I,J,K)-Tvis*((DUDY+DVDX)**2+
     1             (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)

            GEN(I,J,K)=GENRES+GENCOU
            DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*XPLUSE/DXE
            IF(XPLUSE.GT.11.63)DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*
     &             ALOG(ELOG*XPLUSE)/(CAPPA*DXE)
            SU(I,J,K)=SUKD(I,J,K)+GEN(I,J,K)*VOL
            SP(I,J,K)=SPKD(I,J,K)-DITERM*VOL
            AE(I,J,K)=0.0
 6121      CONTINUE
 612      CONTINUE

C South wall.
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          DO 622 I=ISBi(L,ICFD),ISBf(L,ICFD)
           DO 6221 K=KSBi(L,ICFD),KSBf(L,ICFD)
            DENU=DENf(i,j,K)
            SQRTK=SQRT(ABS(TEf(i,j,K)))
            VOL=VolP(I,J,K)
            Tvis=VIS(I,J,K)-VISCOS
            if (Tvis.lt.1e-19) Tvis=0.
            YPLUSS=DENU*SQRTK*CDTERM*DYS/VISCOS
            GENCOU=0.5*(ABS(TAUSU(I+1,J,K)*Uf(i+1,j,K))+ABS(TAUSU(I,J,K)
     1            *Uf(I,J,K)))/DYS+0.5*(ABS(TAUSW(I,J,K+1)*Wf(i,j,K+1))+
     2            ABS(TAUSW(I,J,K)*Wf(I,J,K)))/DYS

C Get velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GENRES=GEN(I,J,K)-Tvis*((DUDY+DVDX)**2+
     1             (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)

            GEN(I,J,K)=GENRES+GENCOU
            DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*YPLUSS/DYS
            IF(YPLUSS.GT.11.63)DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*
     &             ALOG(ELOG*YPLUSS)/(CAPPA*DYS)
            SU(I,J,K)=GEN(I,J,K)*VOL+SUKD(I,J,K)
            SP(I,J,K)=-DITERM*VOL+SPKD(I,J,K)
            AS(I,J,K)=0.0
 6221      CONTINUE
 622      CONTINUE

C North wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          DO 632 I=ISBi(L,ICFD),ISBf(L,ICFD)
           DO 6321 K=KSBi(L,ICFD),KSBf(L,ICFD)
            DENU=DENf(i,j,K)
            SQRTK=SQRT(ABS(TEf(i,j,K)))
            VOL=VolP(I,J,K)
            Tvis=VIS(I,J,K)-VISCOS
            if (Tvis.lt.1e-19) Tvis=0.
            YPLUSN=DENU*SQRTK*CDTERM*DYN/VISCOS
            GENCOU=0.5*(ABS(TAUNU(I+1,J,K)*Uf(i+1,j,K))+ABS(TAUNU(I,J,K)
     1            *Uf(I,J,K)))/DYN+0.5*(ABS(TAUNW(I,J,K+1)*Wf(i,j,K+1))+
     2            ABS(TAUNW(I,J,K)*Wf(I,J,K)))/DYN

C Get velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GENRES=GEN(I,J,K)-Tvis*((DUDY+DVDX)**2+
     1             (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)

            GEN(I,J,K)=GENRES+GENCOU
            DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*YPLUSN/DYN
            IF(YPLUSN.GT.11.63)DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*
     &             ALOG(ELOG*YPLUSN)/(CAPPA*DYN)
            SU(I,J,K)=GEN(I,J,K)*VOL+SUKD(I,J,K)
            SP(I,J,K)=-DITERM*VOL+SPKD(I,J,K)
            AN(I,J,K)=0.0
 6321      CONTINUE
 632      CONTINUE

C Low wall.
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          DO 642 I=ISBi(L,ICFD),ISBf(L,ICFD)
           DO 6421 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DENU=DENf(i,j,K)
            SQRTK=SQRT(ABS(TEf(i,j,K)))
            VOL=VolP(I,J,K)
            Tvis=VIS(I,J,K)-VISCOS
            if (Tvis.lt.1e-19) Tvis=0.
            ZPLUSL=DENU*SQRTK*CDTERM*DZL/VISCOS
            GENCOU=0.5*(ABS(TAULU(I+1,J,K)*Uf(i+1,j,K))+ABS(TAULU(I,J,K)
     1            *Uf(I,J,K)))/DZL+0.5*(ABS(TAULV(I,J+1,K)*Vf(i,j+1,K))+
     2            ABS(TAULV(I,J,K)*Vf(I,J,K)))/DZL

C Get velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GENRES=GEN(I,J,K)-Tvis*((DUDY+DVDX)**2+
     1             (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)

            GEN(I,J,K)=GENRES+GENCOU
            DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*ZPLUSL/DZL
            IF(ZPLUSL.GT.11.63)DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*
     &             ALOG(ELOG*ZPLUSL)/(CAPPA*DZL)
            SU(I,J,K)=GEN(I,J,K)*VOL+SUKD(I,J,K)
            SP(I,J,K)=-DITERM*VOL+SPKD(I,J,K)
            AL(I,J,K)=0.0
 6421      CONTINUE
 642      CONTINUE

C High wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          DO 652 I=ISBi(L,ICFD),ISBf(L,ICFD)
           DO 6521 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DENU=DENf(i,j,K)
            SQRTK=SQRT(ABS(TEf(i,j,K)))
            VOL=VolP(I,J,K)
            Tvis=VIS(I,J,K)-VISCOS
            if (Tvis.lt.1e-19) Tvis=0.
            ZPLUSH=DENU*SQRTK*CDTERM*DZH/VISCOS
            GENCOU=0.5*(ABS(TAUHU(I+1,J,K)*Uf(i+1,j,K))+ABS(TAUHU(I,J,K)
     1            *Uf(I,J,K)))/DZH+0.5*(ABS(TAUHV(I,J+1,K)*Vf(i,j+1,K))+
     2            ABS(TAUHV(I,J,K)*Vf(I,J,K)))/DZH

C Get velocity derivitaves.
            call VELDERIV(I,J,K,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
            GENRES=GEN(I,J,K)-Tvis*((DUDY+DVDX)**2+
     1             (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)

            GEN(I,J,K)=GENRES+GENCOU
            DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*ZPLUSH/DZH
            IF(ZPLUSH.GT.11.63)DITERM=CD*DENf(i,j,K)*(CMU**.75)*SQRTK*
     &             ALOG(ELOG*ZPLUSH)/(CAPPA*DZH)
            SU(I,J,K)=GEN(I,J,K)*VOL+SUKD(I,J,K)
            SP(I,J,K)=-DITERM*VOL+SPKD(I,J,K)
            AH(I,J,K)=0.0
 6521      CONTINUE
 652      CONTINUE

C Symetric Axis.
C West wall.
        ELSEIF(location.EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 662 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 6621 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I-1,J,K)=-GREAT
C              SU(I-1,J,K)=GREAT*TEf(I,J,K)
              TEf(I-1,J,K)=TEf(I,J,K)
              AW(I,J,K)=0.0
 6621       CONTINUE
 662      CONTINUE

C East wall.
        ELSEIF(location.EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 665 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 6651 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*TEf(I,J,K)
              TEf(I+1,J,K)=TEf(I,J,K)
              AE(I,J,K)=0.0
 6651       CONTINUE
 665       CONTINUE

C South wall.
        ELSEIF(location.EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 668 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6681 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J-1,K)=-GREAT
C              SU(I,J-1,K)=GREAT*TEf(I,J,K)
              TEf(I,J-1,K)=TEf(I,J,K)
              AS(I,J,K)=0.0
 6681        CONTINUE
 668      CONTINUE

C North wall.
        ELSEIF(location.EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 670 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6701 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*TEf(I,J,K)
              TEf(I,J+1,K)=TEf(I,J,K)
              AN(I,J,K)=0.0
 6701       CONTINUE
 670      CONTINUE

C Low wall.
        ELSEIF(location.EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 672 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6721 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K-1)=-GREAT
C              SU(I,J,K-1)=GREAT*TEf(I,J,K)
              TEf(I,J,K-1)=TEf(I,J,K)
              AL(I,J,K)=0.0
 6721       CONTINUE
 672      CONTINUE

C High wall.
        ELSEIF(location.EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 675 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6751 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K+1)=-GREAT
C              SU(I,J,K+1)=GREAT*TEf(I,J,K)
              TEf(I,J,K+1)=TEf(I,J,K)
              AH(I,J,K)=0.0
 6751       CONTINUE
 675      CONTINUE
        ENDIF
 601  CONTINUE

      RETURN


C ********************* MODED *********************
C MODED - Energy dissipation - turbulent.
      ENTRY MODED

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 2900 J=1,NJ
          DO 2902 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 2902   CONTINUE
 2900   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 3000 I=1,NI
          DO 3001 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 3001     CONTINUE
 3000   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 3100 I=1,NI
          DO 3101 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 3101     CONTINUE
 3100   CONTINUE
      ENDIF

C In case of fixed mass or pressure:
C If an inlet -> Add energy as: Min*Ein
      DO 698 M=1,NOPEN(ICFD)
        IF(IWOPEN(M,ICFD).EQ.0.OR.IWOPEN(M,ICFD).EQ.99)THEN
          IF(FIXM(M,ICFD).GT.0)THEN
            DO 699 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 6991 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                DO 6992 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                  SU(I,J,K)=SU(I,J,K)+FIXM(M,ICFD)*FIXE(M,ICFD)
 6992           CONTINUE
 6991         CONTINUE
 699        CONTINUE
          ENDIF
        ENDIF

C Outlets.
        IF(FIXM(M,ICFD).LE.0.0) THEN

C West wall.
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN
            I=IOPENi(M,ICFD)
            DO 1020 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 1021 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
 1021         CONTINUE
 1020       CONTINUE

C East wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.2) THEN
            I=IOPENi(M,ICFD)
            DO 1022 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 1023 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AE(I,J,K)=0.0
 1023         CONTINUE
 1022       CONTINUE

C South wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.3) THEN
            J=JOPENi(M,ICFD)
            DO 1024 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1025 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AS(I,J,K)=0.0
 1025         CONTINUE
 1024       CONTINUE

C North wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.4) THEN
            J=JOPENi(M,ICFD)
            DO 1026 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1027 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AN(I,J,K)=0.0
 1027         CONTINUE
 1026       CONTINUE

C Low wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.5) THEN
            K=KOPENi(M,ICFD)
            DO 1028 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1029 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AL(I,J,K)=0.0
 1029         CONTINUE
 1028       CONTINUE

C High wall.
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.6) THEN
            K=KOPENi(M,ICFD)
            DO 1030 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 1031 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AH(I,J,K)=0.0
 1031         CONTINUE
 1030       CONTINUE

          ENDIF
        ENDIF
 698  CONTINUE

C Solid walls.
      DO 701 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C West wall.
        IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)
          DXW=XP(I)-XU(I)
          TERM=(CMU**.75)/(CAPPA*DXW)
          DO 702 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 7021 K=KSBi(L,ICFD),KSBf(L,ICFD)
              SU(I,J,K)=GREAT*TERM*ABS(TEf(i,j,K))**1.5
              SP(I,J,K)=-GREAT
              AW(I,J,K)=0.0
 7021       CONTINUE
 702      CONTINUE

C East wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          TERM=(CMU**.75)/(CAPPA*DXE)
          DO 712 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 7121 K=KSBi(L,ICFD),KSBf(L,ICFD)
              SU(I,J,K)=GREAT*TERM*ABS(TEf(i,j,K))**1.5
              SP(I,J,K)=-GREAT
              AE(I,J,K)=0.0
 7121       CONTINUE
 712      CONTINUE

C South wall.
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          TERM=(CMU**.75)/(CAPPA*DYS)
          DO 722 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7221 K=KSBi(L,ICFD),KSBf(L,ICFD)
              SU(I,J,K)=GREAT*TERM*ABS(TEf(i,j,K))**1.5
              SP(I,J,K)=-GREAT
              AS(I,J,K)=0.0
 7221       CONTINUE
 722      CONTINUE

C North wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          TERM=(CMU**.75)/(CAPPA*DYN)
          DO 732 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7321 K=KSBi(L,ICFD),KSBf(L,ICFD)
              SU(I,J,K)=GREAT*TERM*ABS(TEf(i,j,K))**1.5
              SP(I,J,K)=-GREAT
              AN(I,J,K)=0.0
 7321       CONTINUE
 732      CONTINUE

C Low wall.
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          TERM=(CMU**.75)/(CAPPA*DZL)
          DO 742 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7421 J=JSBi(L,ICFD),JSBf(L,ICFD)
              SU(I,J,K)=GREAT*TERM*ABS(TEf(i,j,K))**1.5
              SP(I,J,K)=-GREAT
              AL(I,J,K)=0.0
 7421       CONTINUE
 742      CONTINUE

C High wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          TERM=(CMU**.75)/(CAPPA*DZH)
          DO 752 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7521 J=JSBi(L,ICFD),JSBf(L,ICFD)
              SU(I,J,K)=GREAT*TERM*ABS(TEf(i,j,K))**1.5
              SP(I,J,K)=-GREAT
              AH(I,J,K)=0.0
 7521       CONTINUE
 752      CONTINUE

C Symetric axis.
C West wall.
        ELSEIF(location.EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 762 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 7621 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I-1,J,K)=-GREAT
C              SU(I-1,J,K)=GREAT*EDf(I,J,K)
              EDf(I-1,J,K)=EDf(I,J,K)
              AW(I,J,K)=0.0
 7621       CONTINUE
 762      CONTINUE

C East wall.
        ELSEIF(location.EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 765 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 7651 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*EDf(I,J,K)
              EDf(I+1,J,K)=EDf(I,J,K)
              AE(I,J,K)=0.0
 7651       CONTINUE
 765      CONTINUE

C South wall.
        ELSEIF(location.EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 768 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7681 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J-1,K)=-GREAT
C              SU(I,J-1,K)=GREAT*EDf(I,J,K)
              EDf(I,J-1,K)=EDf(I,J,K)
              AS(I,J,K)=0.0
 7681       CONTINUE
 768      CONTINUE

C North wall.
        ELSEIF(location.EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 770 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7701 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*EDf(I,J,K)
              EDf(I,J+1,K)=EDf(I,J,K)
              AN(I,J,K)=0.0
 7701       CONTINUE
 770      CONTINUE

C Low wall.
        ELSEIF(location.EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 772 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7721 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K-1)=-GREAT
C              SU(I,J,K-1)=GREAT*EDf(I,J,K)
              EDf(I,J,K-1)=EDf(I,J,K)
              AL(I,J,K)=0.0
 7721       CONTINUE
 772      CONTINUE

C High wall.
        ELSEIF(location.EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 775 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 7751 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K+1)=-GREAT
C              SU(I,J,K+1)=GREAT*EDf(I,J,K)
              EDf(I,J,K+1)=EDf(I,J,K)
              AH(I,J,K)=0.0
 7751       CONTINUE
 775      CONTINUE
        ENDIF
 701  CONTINUE

      RETURN
      END


C ********************* BNDARTT *********************
C BNDARTT - Impose thermal boundary conditions for turbulent flow.
      SUBROUTINE BNDARTT
#include "cfd.h"

      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLN(MCFND,MNZ)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/CONST/GREAT,small,GRAV
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/MFS/IMFACT
      COMMON/closehow/yplus(ntcelx,ntcely,ntcelz)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      CHARACTER*72 HSID
      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ
      logical BHS


C This subroutine imposes boundary conditions (BCs) for the thermal equation
C when there is turbulent flow. It applies BCs at the air-flow openings, then
C it applies BCs on the solid surfaces, then finally it applies BCs for the
C heat sources.

C Initialize variables.
      CDTERM=CMU**0.25


C If flow is 1D or 2D set coefficients in other directions to zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 2300 J=1,NJ
          DO 2301 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 2301     CONTINUE
 2300   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 2400 I=1,NI
          DO 2401 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 2401     CONTINUE
 2400   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 2500 I=1,NI
          DO 2502 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 2502     CONTINUE
 2500   CONTINUE
      ENDIF


C Reset heat flow to each solid boundary to zero. The heat flows for
C the current iteration will be recalculated later in this subroutine.
      DO 2550 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        CONVF(ICFD,IS)=0.0
 2550 CONTINUE


C Account for flow entering or leaving domain. Examine each opening in turn.
C Note that treatment of `prescribed velocity' type BCs has already been handled
C in subroutine INOUT so no further modifications required here.
      DO 400 M=1,NOPEN(ICFD)
        IF(IWOPEN(M,ICFD).EQ.0)THEN

C `Pressure' type BC. If not conflated with air-flow network, then fix temperature
C at opening to level prescribed. If conflated, then add energy as Min*Tin.
          DO 420 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
            DO 4201 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 4202 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                IF(IMFACT.EQ.1.AND.FIXM(M,ICFD).GT.0.0)THEN

C Conflated with air-flow network and flow is into domain.
C FIXM and FIXT have been mapped from air-flow network results.
                  SU(I,J,K)=SU(I,J,K)+FIXM(M,ICFD)*FIXT(M,ICFD)
                ELSE IF(IMFACT.EQ.0.AND.FIXT(M,ICFD).LT.GREAT)THEN

C No conflation with air-flow network.
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*FIXT(M,ICFD)
                ENDIF
 4202         CONTINUE
 4201       CONTINUE
 420      CONTINUE
        ELSE IF(IWOPEN(M,ICFD).EQ.99)THEN

C `Mass' type BC. If flow is entering domain, then add energy as Min*Tin.
          IF(FIXM(M,ICFD).GT.0)THEN
            DO 421 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 4211 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                DO 4212 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                  SU(I,J,K)=SU(I,J,K)+FIXM(M,ICFD)*FIXT(M,ICFD)
 4212           CONTINUE
 4211         CONTINUE
 421        CONTINUE
          ENDIF
        ENDIF

C Air exiting domain. Disconnect cells in opening from boundary.
        IF(FIXM(M,ICFD).LE.0.0) THEN
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN

C West wall.
            I=IOPENi(M,ICFD)
            DO 900 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9001 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
 9001         CONTINUE
 900        CONTINUE
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.2) THEN

C East wall.
            I=IOPENi(M,ICFD)
            DO 902 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9021 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AE(I,J,K)=0.0
 9021         CONTINUE
 902        CONTINUE
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.3) THEN

C South wall.
            J=JOPENi(M,ICFD)
            DO 904 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9041 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AS(I,J,K)=0.0
 9041         CONTINUE
 904        CONTINUE
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.4) THEN

C North wall.
            J=JOPENi(M,ICFD)
            DO 906 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9061 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AN(I,J,K)=0.0
 9061         CONTINUE
 906        CONTINUE
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.5) THEN

C Low wall.
            K=KOPENi(M,ICFD)
            DO 908 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9081 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AL(I,J,K)=0.0
 9081         CONTINUE
 908        CONTINUE
          ELSE IF(ABS(IWOPEN(M,ICFD)).EQ.6) THEN

C High wall.
            K=KOPENi(M,ICFD)
            DO 910 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9101 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AH(I,J,K)=0.0
 9101         CONTINUE
 910        CONTINUE
          ENDIF
        ENDIF
 400  CONTINUE


C Apply BCs for the solid boundaries. Eight different approaches are available,
C identified below by the associated ITCtype values:
C
C 1&9&13  Qsurf calculated by CFD using k-e model with log-law wall functions.
C  2&10   Qsurf calculated by CFD using MIT 0-eqn model.
C  3&11   Qsurf calculated by CFD using k-e model with Yuan wall functions
C         (not yet implemented).
C   4     Qsurf=A*HC*(Tsurf-Tref) where BSim passes HC, Tsurf, and Tref (TFA).
C         Qsurf absorbed into source terms for next-to-wall cells; therefore,
C         Tsurf not imposed. k-e model.
C   5     Qsurf=A*HC*(Tsurf-Tref) where BSim passes HC and Tsurf; Tref calculated
C         by CFD by averaging previous iteration's temperature results. Qsurf
C         absorbed into source terms for next-to-wall cells; therefore,
C         Tsurf not imposed. k-e model.
C   6     Qsurf=A*HC*(Tsurf-Tref) where BSim passes HC, Tsurf, and Tref (TFA).
C         Log-law wall functions then imposed to `back out' temperature of
C         next-to-wall cells. Qsurf calculated same as in (4) but Tsurf now
C         imposed through wall functions. k-e model.
C   7     Qsurf=A*HC*(Tsurf-Tref) where BSim passes HC and Tsurf; Tref calculated
C         by CFD by averaging previous iteration's temperature results.
C         Log-law wall functions then imposed to `back out' temperature of
C         next-to-wall cells. Qsurf calculated same as in (4) but Tsurf now
C         imposed through wall functions. k-e model.
C 8&12&14 Qsurf=A*HC*(Tsurf-Tp) where BSim passes HC and Tsurf; Tp is the local
C         temperature of the next-to-wall grid point. Therefore, HC treated as
C         a local value. k-e model.
C   n/a   Qsurf prescribed by user and absorbed into source terms for next-to-wall
C         cells (only used for CFD-only analyses). k-e model.
C   n/a   Symmetry type boundary condition (only used for CFD-only analyses).
C         k-e model.

C Examine each CFD solid boundary in turn.
      DO 401 L=1,NSB(ICFD)

C Qsurf calculated by CFD using k-e model with log-law wall functions. Surface
C temperature prescribed.
        IF(IWSB(L,ICFD).EQ.1)THEN

C West wall.
          I=ISBi(L,ICFD)

C Calculate distance from wall to next-to-wall grid point
C ($DELTA y sub p$). Get corresponding ESP-r surface number.
          DXW=XP(I)-XU(I)
          IS=ISUFLC(L,ICFD)

C Treat each cell on solid boundary.
          DO 402 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4021 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Disconnect cell from boundary. Calculate surface area of cell.
C Calculate y+ (see Negrao 4.5). Save y+ to array for use by adaptive
C conflation controller.
              AW(I,J,K)=0.0
              AREA=AreaEWP(J,K)
              XPLUSW=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DXW/VISCOS
              yplus(i,j,k) = XPLUSW
              IF(XPLUSW.GT.11.63) THEN

C y+ is within log-law region. UPLUS is u+; ELOG is `E';
C CAPPA is the von Karmann constant. GT is an intermediate
C quantity and includes the value of T+ in its denominator.
                UPLUS=ALOG(ELOG*XPLUSW)/CAPPA
                GT=DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K)))/(PRANDT*
     &             (UPLUS+PFUN))
              ELSE

C y+ is within laminar sub-layer.
                GT=VISCOS/(PRANDL*DXW)
              ENDIF

C Now calculate the source terms (V&M eq.9.13 & eq.9.24).
              TERM=GT*AREA
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
C Calculate surface convection. QW is heat flow (W) from wall to air.
              QW=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW
 4021       CONTINUE
 402      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.2)THEN

C East wall (see west wall for annotations).
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          IS=ISUFLC(L,ICFD)
          DO 412 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4121 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              AREA=AreaEWP(J,K)
              XPLUSE=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DXE/VISCOS
              yplus(i,j,k) = XPLUSE
              IF(XPLUSE.GT.11.63)THEN
                QW=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
                UPLUS=ALOG(ELOG*XPLUSE)/CAPPA
                GT=DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K)))/(PRANDT*
     &             (UPLUS+PFUN))
              ELSE
                GT=VISCOS/(PRANDL*DXE)
              ENDIF
              TERM=GT*AREA
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QE=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
 4121       CONTINUE
 412      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.3)THEN

C South wall (see west wall for annotations).
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          IS=ISUFLC(L,ICFD)
          DO 422 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4221 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              AREA=AreaNSP(I,K)
              YPLUSS=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DYS/VISCOS
              yplus(i,j,k) = YPLUSS
              IF(YPLUSS.GT.11.63)THEN
                UPLUS=ALOG(ELOG*YPLUSS)/CAPPA
                GT=DENf(i,j,K)*CDTERM*SQRT(ABS(TEf(i,j,K)))/(PRANDT*
     &             (UPLUS+PFUN))
              ELSE
                GT=VISCOS/(PRANDL*DYS)
              ENDIF
              TERM=GT*AREA
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QS=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
 4221       CONTINUE
 422      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.4)THEN

C North wall (see west wall for annotations).
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          IS=ISUFLC(L,ICFD)
          DO 432 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4321 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              AREA=AreaNSP(I,K)
              YPLUSN=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DYN/VISCOS
              yplus(i,j,k) = YPLUSN
              IF(YPLUSN.GT.11.63)THEN
                UPLUS=ALOG(ELOG*YPLUSN)/CAPPA
                GT=DENf(i,j,K)*CDTERM*SQRT(ABS(TEf(i,j,K)))/(PRANDT*
     &             (UPLUS+PFUN))
              ELSE
                GT=VISCOS/(PRANDL*DYN)
              ENDIF
              TERM=GT*AREA
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QN=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
 4321       CONTINUE
 432      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.5)THEN

C Low wall (see west wall for annotations).
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          IS=ISUFLC(L,ICFD)
          DO 442 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4421 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              AREA=AreaHLP(I,J)
              ZPLUSL=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DZL/VISCOS
              yplus(i,j,k) = ZPLUSL
              IF(ZPLUSL.GT.11.63)THEN
                UPLUS=ALOG(ELOG*ZPLUSL)/CAPPA
                GT=DENf(i,j,K)*CDTERM*SQRT(ABS(TEf(i,j,K)))/(PRANDT*
     &             (UPLUS+PFUN))
              ELSE
                GT=VISCOS/(PRANDL*DZL)
              ENDIF
              TERM=GT*AREA
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QL=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
 4421       CONTINUE
 442      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.6)THEN

C High wall (see west wall for annotations).
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          IS=ISUFLC(L,ICFD)
          DO 452 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4521 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              AREA=AreaHLP(I,J)
              ZPLUSH=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DZH/VISCOS
              yplus(i,j,k) = ZPLUSH
              IF(ZPLUSH.GT.11.63)THEN
                UPLUS=ALOG(ELOG*ZPLUSH)/CAPPA
                GT=DENf(i,j,K)*CDTERM*SQRT(ABS(TEf(i,j,K)))/(PRANDT*
     &             (UPLUS+PFUN))
              ELSE
                GT=VISCOS/(PRANDL*DZH)
              ENDIF
              TERM=GT*AREA
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QH=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
 4521       CONTINUE
 452      CONTINUE

C Qsurf=A*HC*(Tsurf-Tref). BSim passes HC and Tsurf; Tref may come from
C BSim (TFA) or CFD (by domain averaging previous iteration's temperature
C results), depending on value of icTREF. Qsurf absorbed into source terms
C for next-to-wall cells; therefore, Tsurf not imposed. k-e model.
        ELSEIF(IWSB(L,ICFD).EQ.21)THEN

C West wall. Get number of corresponding ESP-r surface.
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)

C Treat each cell on solid boundary.
          DO 463 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4631 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Calculate surface area of cell. Disconnect cell from boundary.
              AREA=AreaEWP(J,K)
              AW(I,J,K)=0.0

C Get reference air temp and calculate heat flow (W) from wall to air.
              CALL TREFBC(L,TREF)
              QW=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW

C Add surface convection to source term.
              SU(I,J,K)=SU(I,J,K)+QW/SH
 4631       CONTINUE
 463      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.22)THEN

C East wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)
          DO 473 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4731 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaEWP(J,K)
              AE(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              QE=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
              SU(I,J,K)=SU(I,J,K)+QE/SH
 4731       CONTINUE
 473      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.23)THEN

C South wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          DO 483 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4831 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaNSP(I,K)
              AS(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              QS=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
              SU(I,J,K)=SU(I,J,K)+QS/SH
 4831       CONTINUE
 483      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.24)THEN

C North wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          DO 493 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4931 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaNSP(I,K)
              AN(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              QN=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
              SU(I,J,K)=SU(I,J,K)+QN/SH
 4931       CONTINUE
 493      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.25)THEN

C Low wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          DO 453 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4531 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AREA=AreaHLP(I,J)
              AL(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              QL=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
              SU(I,J,K)=SU(I,J,K)+QL/SH
 4531       CONTINUE
 453      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.26)THEN

C High wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          DO 443 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4431 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AREA=AreaHLP(I,J)
              AH(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              QH=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
              SU(I,J,K)=SU(I,J,K)+QH/SH
 4431       CONTINUE
 443      CONTINUE

C Qsurf=A*HC*(Tsurf-Tref). BSim passes HC and Tsurf; Tref may come from
C BSim (TFA) or CFD (by domain averaging previous iteration's temperature
C results), depending on value of icTREF. Log-law wall functions then
C imposed to `back out' temperature of next-to-wall cells. Therefore,
C Tsurf imposed through wall functions. k-e model.
        ELSEIF(IWSB(L,ICFD).EQ.31)THEN

C West wall.
          I=ISBi(L,ICFD)

C Distance from the wall to the P point.
          DXW=XP(I)-XU(I)
          IS=ISUFLC(L,ICFD)
          DO 608 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 6081 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Disconnect the cell's temperature from all neighbouring cells.
              AW(I,J,K)=0.0
              AE(I,J,K)=0.0
              AN(I,J,K)=0.0
              AS(I,J,K)=0.0
              AL(I,J,K)=0.0
              AH(I,J,K)=0.0

C Get reference air temp and calculate heat flow (W) from wall to air.
              CALL TREFBC(L,TREF)
              AREA=AreaEWP(J,K)
              QW=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW

C Calculate y+ (see Launder & Spalding 1974, p277).
              XPLUSW=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DXW/VISCOS

C Calculate conductance between Twall and Tp.
              if(XPLUSW.LE.11.63)then

C Point is within laminar sub-layer. See V&M eq.9.12.
                TERM=( VISCOS*AREA*SH )/( PRANDL*DXW )
              else

C Point is within log-law region.
C UPLUS is u+; ELOG is `E'; CAPPA is the von Karmann constant.
C For TERM, see V&M eq.9.24, 9.20, and 3.41 on p.202.
                UPLUS=ALOG(ELOG*XPLUSW)/CAPPA
                TERM=AREA*SH*DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K))) /
     &               ( PRANDT*(UPLUS+PFUN) )
              endif

C Calculate Tp.
              Tpoint = SSB(L,ICFD) - QW/TERM

C Now set the source terms to give Tpoint at the P point.
              SU(I,J,K)=GREAT*Tpoint
              SP(I,J,K)=-GREAT
 6081       CONTINUE
  608      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.32)THEN

C East wall (see west wall for annotations).
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          IS=ISUFLC(L,ICFD)
          DO 609 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 6091 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              AE(I,J,K)=0.0
              AN(I,J,K)=0.0
              AS(I,J,K)=0.0
              AL(I,J,K)=0.0
              AH(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              AREA=AreaEWP(J,K)
              QE=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
              XPLUSE=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DXE/VISCOS
              if(XPLUSE.LE.11.63)then
                TERM=( VISCOS*AREA*SH )/( PRANDL*DXE )
              else
                UPLUS=ALOG(ELOG*XPLUSE)/CAPPA
                TERM=AREA*SH*DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K))) /
     &               ( PRANDT*(UPLUS+PFUN) )
              endif
              Tpoint = SSB(L,ICFD) - QW/TERM
              SU(I,J,K)=GREAT*Tpoint
              SP(I,J,K)=-GREAT
 6091       CONTINUE
  609      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.33)THEN

C South wall.
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          IS=ISUFLC(L,ICFD)
          DO 610 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6101 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              AE(I,J,K)=0.0
              AN(I,J,K)=0.0
              AS(I,J,K)=0.0
              AL(I,J,K)=0.0
              AH(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              AREA=AreaNSP(I,K)
              QS=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
              YPLUSS=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DYS/VISCOS
              if(YPLUSS.LE.11.63)then
                TERM=( VISCOS*AREA*SH )/( PRANDL*DYS )
              else
                UPLUS=ALOG(ELOG*YPLUSS)/CAPPA
                TERM=AREA*SH*DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K))) /
     &               ( PRANDT*(UPLUS+PFUN) )
              endif
              Tpoint = SSB(L,ICFD) - QS/TERM
              SU(I,J,K)=GREAT*Tpoint
              SP(I,J,K)=-GREAT
 6101       CONTINUE
 610      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.34)THEN

C North wall (see west wall for annotations).
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          IS=ISUFLC(L,ICFD)
          DO 611 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6111 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              AE(I,J,K)=0.0
              AN(I,J,K)=0.0
              AS(I,J,K)=0.0
              AL(I,J,K)=0.0
              AH(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              AREA=AreaNSP(I,K)
              QN=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
              YPLUSN=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DYN/VISCOS
              if(YPLUSN.LE.11.63)then
                TERM=( VISCOS*AREA*SH )/( PRANDL*DYN )
              else
                UPLUS=ALOG(ELOG*YPLUSN)/CAPPA
                TERM=AREA*SH*DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K))) /
     &               ( PRANDT*(UPLUS+PFUN) )
              endif
              Tpoint = SSB(L,ICFD) - QN/TERM
              SU(I,J,K)=GREAT*Tpoint
              SP(I,J,K)=-GREAT
 6111       CONTINUE
 611      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.35)THEN

C Low wall (see west wall for annotations).
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          IS=ISUFLC(L,ICFD)
          DO 614 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6141 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AW(I,J,K)=0.0
              AE(I,J,K)=0.0
              AN(I,J,K)=0.0
              AS(I,J,K)=0.0
              AL(I,J,K)=0.0
              AH(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              AREA=AreaHLP(I,J)
              QL=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
              ZPLUSL=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DZL/VISCOS
              if(ZPLUSL.LE.11.63)then
                TERM=( VISCOS*AREA*SH )/( PRANDL*DZL )
              else
                UPLUS=ALOG(ELOG*ZPLUSL)/CAPPA
                TERM=AREA*SH*DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K))) /
     &               ( PRANDT*(UPLUS+PFUN) )
              endif
              Tpoint = SSB(L,ICFD) - QL/TERM
              SU(I,J,K)=GREAT*Tpoint
              SP(I,J,K)=-GREAT
 6141       CONTINUE
 614      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.36)THEN

C High wall (see west wall for annotations).
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          IS=ISUFLC(L,ICFD)
          DO 613 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 6131 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AW(I,J,K)=0.0
              AE(I,J,K)=0.0
              AN(I,J,K)=0.0
              AS(I,J,K)=0.0
              AL(I,J,K)=0.0
              AH(I,J,K)=0.0
              CALL TREFBC(L,TREF)
              AREA=AreaHLP(I,J)
              QH=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-TREF )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
              ZPLUSH=DENf(I,J,K)*SQRT(ABS(TEf(I,J,K)))*CDTERM*DZH/VISCOS
              if(ZPLUSH.LE.11.63)then
                TERM=( VISCOS*AREA*SH )/( PRANDL*DZH )
              else
                UPLUS=ALOG(ELOG*ZPLUSH)/CAPPA
                TERM=AREA*SH*DENf(i,j,k)*CDTERM*SQRT(ABS(TEf(i,j,K))) /
     &               ( PRANDT*(UPLUS+PFUN) )
              endif
              Tpoint = SSB(L,ICFD) - QH/TERM
              SU(I,J,K)=GREAT*Tpoint
              SP(I,J,K)=-GREAT
 6131       CONTINUE
 613      CONTINUE

C Qsurf=A*HC*(Tsurf-Tp). BSim passes HC and Tsurf; Tp is the local temperature
C of the next-to-wall grid point. Therefore, HC treated as a local value.
C k-e model.
        ELSEIF(IWSB(L,ICFD).EQ.41)THEN

C West wall. Get number of corresponding ESP-r surface.
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)

C Treat each cell on solid boundary.
          DO 163 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 1631 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Calculate surface area of cell. Disconnect cell from boundary.
              AREA=AreaEWP(J,K)
              AW(I,J,K)=0.0

C Calculate heat flow (W) from wall to air.
              QW=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW

C Absorb surface temp and convection coefficient into source terms.
              SU(I,J,K)=SU(I,J,K)+AREA*SSBHC(L,ICFD)*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*SSBHC(L,ICFD) / SH
 1631       CONTINUE
 163      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.42)THEN

C East wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)
          DO 173 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 1731 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaEWP(J,K)
              AE(I,J,K)=0.0
              QE=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
              SU(I,J,K)=SU(I,J,K)+AREA*SSBHC(L,ICFD)*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*SSBHC(L,ICFD) / SH
 1731       CONTINUE
 173      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.43)THEN

C South wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          DO 183 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1831 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaNSP(I,K)
              AS(I,J,K)=0.0
              QS=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
              SU(I,J,K)=SU(I,J,K)+AREA*SSBHC(L,ICFD)*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*SSBHC(L,ICFD) / SH
 1831       CONTINUE
 183      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.44)THEN

C North wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          DO 193 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1931 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaNSP(I,K)
              AN(I,J,K)=0.0
              QN=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
              SU(I,J,K)=SU(I,J,K)+AREA*SSBHC(L,ICFD)*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*SSBHC(L,ICFD) / SH
 1931       CONTINUE
 193      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.45)THEN

C Low wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          DO 153 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1531 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AREA=AreaHLP(I,J)
              AL(I,J,K)=0.0
              QL=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
              SU(I,J,K)=SU(I,J,K)+AREA*SSBHC(L,ICFD)*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*SSBHC(L,ICFD) / SH
 1531       CONTINUE
 153      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.46)THEN

C High wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          DO 143 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1431 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AREA=AreaHLP(I,J)
              AH(I,J,K)=0.0
              QH=AREA*SSBHC(L,ICFD)*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
              SU(I,J,K)=SU(I,J,K)+AREA*SSBHC(L,ICFD)*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*SSBHC(L,ICFD) / SH
 1431       CONTINUE
 143      CONTINUE

C Qsurf=A*HC*(Tsurf-Tp). BSim passes Tsurf; HC re-calculated here each iteration
C using Reynolds analogy (eq.16 of Chen & Xu 1998 E&B paper; Tp is the local
C temperature of the next-to-wall grid point. Therefore, HC treated as a local
C value. MIT zero-equation model.
        ELSEIF(IWSB(L,ICFD).EQ.51)THEN

C West wall. Get number of corresponding ESP-r surface.
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)

C Treat each cell on solid boundary.
          DO 263 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2631 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Calculate surface area of cell.
              AREA=AreaEWP(J,K)

C Calculate distance to next-to-wall node, $DELTA x sub j$ in
C eq.16 of Chen & Xu paper (see citation in subroutine MITlenV).
              DXW=XP(I)-XU(I)

C Disconnect cell from boundary.
              AW(I,J,K)=0.0

C Calculate local conv coefficient using eq.16 of Chen & Xu paper (eq.16).
              HCMIT = ( VIS(I,J,K)*SH ) / ( PRANDT*DXW )

C Calculate heat flow (W) from wall to air.
              QW=AREA*HCMIT*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW

C Absorb surface temp and convection coefficient into source terms.
              SU(I,J,K)=SU(I,J,K)+AREA*HCMIT*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*HCMIT / SH
 2631       CONTINUE
 263      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.52)THEN

C East wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)
          DO 273 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2731 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaEWP(J,K)
              DXE=XU(I+1)-XP(I)
              AE(I,J,K)=0.0
              HCMIT = ( VIS(I,J,K)*SH ) / ( PRANDT*DXE )
              QE=AREA*HCMIT*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
              SU(I,J,K)=SU(I,J,K)+AREA*HCMIT*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*HCMIT / SH
 2731       CONTINUE
 273      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.53)THEN

C South wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          DO 283 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2831 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaNSP(I,K)
              DYS=YP(J)-YV(J)
              AS(I,J,K)=0.0
              HCMIT = ( VIS(I,J,K)*SH ) / ( PRANDT*DYS )
              QS=AREA*HCMIT*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
              SU(I,J,K)=SU(I,J,K)+AREA*HCMIT*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*HCMIT / SH
 2831       CONTINUE
 283      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.54)THEN

C North wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          DO 293 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2931 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AREA=AreaNSP(I,K)
              DYN=YV(J+1)-YP(J)
              AN(I,J,K)=0.0
              HCMIT = ( VIS(I,J,K)*SH ) / ( PRANDT*DYN )
              QN=AREA*HCMIT*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
              SU(I,J,K)=SU(I,J,K)+AREA*HCMIT*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*HCMIT / SH
 2931       CONTINUE
 293      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.55)THEN

C Low wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          DO 253 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2531 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AREA=AreaHLP(I,J)
              DZL=ZP(K)-ZW(K)
              AL(I,J,K)=0.0
              HCMIT = ( VIS(I,J,K)*SH ) / ( PRANDT*DZL )
              QL=AREA*HCMIT*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
              SU(I,J,K)=SU(I,J,K)+AREA*HCMIT*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*HCMIT / SH
 2531       CONTINUE
 253      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.56)THEN

C High wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          DO 243 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2431 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AREA=AreaHLP(I,J)
              DZH=ZW(K+1)-ZP(K)
              AH(I,J,K)=0.0
              HCMIT = ( VIS(I,J,K)*SH ) / ( PRANDT*DZH )
              QH=AREA*HCMIT*( SSB(L,ICFD)-Tf(I,J,K) )
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
              SU(I,J,K)=SU(I,J,K)+AREA*HCMIT*SSB(L,ICFD) / SH
              SP(I,J,K)=SP(I,J,K)-AREA*HCMIT / SH
 2431       CONTINUE
 243      CONTINUE


C Qsurf calculated by CFD using Yuan wall functions with k-e model.
C Surface temperature prescribed.

        ELSEIF(IWSB(L,ICFD).EQ.61)THEN
C West wall.
          I=ISBi(L,ICFD)

C Calculate distance from wall to next-to-wall grid point
C ($y$ in Yuan's thesis). Get corresponding ESP-r surface number.
          DXW=XP(I)-XU(I)
          IS=ISUFLC(L,ICFD)

C Treat each cell on solid boundary.
          DO 872 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 8721 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Disconnect cell from boundary.
              AW(I,J,K)=0.0
C Calculate surface area of cell.
              AREA=AreaEWP(J,K)
              call CalcYuanWallFlux(I,J,K,AREA,DXW,IS,L)
 8721       CONTINUE
 872      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.62)THEN

C East wall (see west wall for annotations).
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          IS=ISUFLC(L,ICFD)
          DO 873 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 8731 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              AREA=AreaEWP(J,K)
              call CalcYuanWallFlux(I,J,K,AREA,DXE,IS,L)
 8731       CONTINUE
 873      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.63)THEN

C South wall (see west wall for annotations).
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          IS=ISUFLC(L,ICFD)
          DO 874 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 8741 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              AREA=AreaNSP(I,K)
              call CalcYuanWallFlux(I,J,K,AREA,DYS,IS,L)
 8741       CONTINUE
 874      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.64)THEN

C North wall (see west wall for annotations).
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          IS=ISUFLC(L,ICFD)
          DO 875 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 8751 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              AREA=AreaNSP(I,K)
              call CalcYuanWallFlux(I,J,K,AREA,DYN,IS,L)
 8751       CONTINUE
 875      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.65)THEN
C Low wall not applicable since Yuan wall functions are only for vertical surfaces.
        ELSEIF(IWSB(L,ICFD).EQ.66)THEN
C High wall not applicable since Yuan wall functions are only for vertical surfaces


C Qsurf prescribed by user and absorbed into source terms for next-to-wall
C cells (only used for CFD-only analyses). k-e model.
        ELSEIF(IWSB(L,ICFD).EQ.-1)THEN

C West wall.
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)

C Calculate total surface area of solid boundary.
          AREAT=(YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))

C Treat each cell on solid boundary.
          DO 462 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4621 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0

C Area-weight heat flow input by user and absorbe into source term.
              QW=SSB(L,ICFD)*AreaEWP(J,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QW/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW
 4621       CONTINUE
 462      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.-2)THEN

C East wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          I=ISBi(L,ICFD)
          AREAT=(YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 472 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4721 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              QE=SSB(L,ICFD)*AreaEWP(J,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QE/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
 4721       CONTINUE
 472      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.-3)THEN

C South wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 482 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4821 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              QS=SSB(L,ICFD)*AreaNSP(I,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QS/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
 4821       CONTINUE
 482      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.-4)THEN

C North wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          J=JSBi(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 492 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4921 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              QN=SSB(L,ICFD)*AreaNSP(I,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QN/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
 4921       CONTINUE
 492      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.-5)THEN

C Low wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))
          DO 494 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4941 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              QL=SSB(L,ICFD)*AreaHLP(I,J)/AREAT
              SU(I,J,K)=SU(I,J,K)+QL/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
 4941       CONTINUE
 494      CONTINUE
        ELSEIF(IWSB(L,ICFD).EQ.-6)THEN

C High wall (see west wall for annotations).
          IS=ISUFLC(L,ICFD)
          K=KSBi(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))
          DO 497 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4971 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              QH=SSB(L,ICFD)*AreaHLP(I,J)/AREAT
              SU(I,J,K)=SU(I,J,K)+QH/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
 4971       CONTINUE
 497      CONTINUE

C Symmetry type boundary condition (only used for CFD-only analyses). k-e model.
        ELSE IF(IWSB(L,ICFD).EQ.11)THEN

C West wall.
          I=ISBi(L,ICFD)
          DO 500 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 5001 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I-1,J,K)=-GREAT
C              SU(I-1,J,K)=GREAT*Tf(I,J,K)
              Tf(I-1,J,K)=Tf(I,J,K)
              AW(I,J,K)=0.0
 5001       CONTINUE
 500      CONTINUE
        ELSE IF(IWSB(L,ICFD).EQ.12)THEN

C East wall.
          I=ISBi(L,ICFD)
          DO 503 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 5031 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*Tf(I,J,K)
              Tf(I+1,J,K)=Tf(I,J,K)
              AE(I,J,K)=0.0
 5031       CONTINUE
 503      CONTINUE
        ELSE IF(IWSB(L,ICFD).EQ.13)THEN

C South wall.
          J=JSBi(L,ICFD)
          DO 505 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5051 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J-1,K)=-GREAT
C              SU(I,J-1,K)=GREAT*Tf(I,J,K)
              Tf(I,J-1,K)=Tf(I,J,K)
              AS(I,J,K)=0.0
 5051       CONTINUE
 505      CONTINUE
        ELSE IF(IWSB(L,ICFD).EQ.14)THEN

C North wall.
          J=JSBi(L,ICFD)
          DO 510 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5101 K=KSBi(L,ICFD),KSBf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*Tf(I,J,K)
              Tf(I,J+1,K)=Tf(I,J,K)
              AN(I,J,K)=0.0
 5101       CONTINUE
 510      CONTINUE
        ELSE IF(IWSB(L,ICFD).EQ.15)THEN

C Low wall.
          K=KSBi(L,ICFD)
          DO 513 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5131 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K-1)=-GREAT
C              SU(I,J,K-1)=GREAT*Tf(I,J,K)
              Tf(I,J,K-1)=Tf(I,J,K)
              AL(I,J,K)=0.0
 5131       CONTINUE
 513      CONTINUE
        ELSE IF(IWSB(L,ICFD).EQ.16)THEN

C High wall.
          K=KSBi(L,ICFD)
          DO 515 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5151 J=JSBi(L,ICFD),JSBf(L,ICFD)
C              SP(I,J,K+1)=-GREAT
C              SU(I,J,K+1)=GREAT*Tf(I,J,K)
              Tf(I,J,K+1)=Tf(I,J,K)
              AH(I,J,K)=0.0
 5151       CONTINUE
 515      CONTINUE
        ENDIF

C Examine next CFD solid boundary.
 401  CONTINUE


C Apply BCs for heat sources. First examine each `temperature' type heat source.
      do 540 L=1,NHS(1,ICFD)
        do 5401 I=IHSi(L,1,ICFD),IHSf(L,1,ICFD)
          do 5402 J=JHSi(L,1,ICFD),JHSf(L,1,ICFD)
            do 5403 K=KHSi(L,1,ICFD),KHSf(L,1,ICFD)
              SU(I,J,K)=GREAT*SHS(L,1,ICFD)
              SP(I,J,K)=-GREAT
C       pause'540'
 5403       continue
 5402     continue
 5401   continue
 540  continue

C Now examine each `heat flow' type heat sources.
C VOLHeat(IV,ICFD) contains the total heat flux defined by the user
C <<do the same also for laminar conditions >>

        do 541 IV=1,NVOL(ICFD)
          if (IVTYPE(IV,ICFD).eq.20) then
            VOLTOT=(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &             (YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))*
     &             (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))
            do 5411 I=IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2)
              do 5412 J=JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2)
                do 5413 K=KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2)
                  VOLCEL=VolP(I,J,K)
                  SU(I,J,K)=SU(I,J,K)+VOLHeat(IV,ICFD)/VOLTOT*VOLCEL/SH
 5413           continue
 5412         continue
 5411       continue
          endif
 541    continue
      RETURN
      END


C ********************* BNDMIT *********************
C BNDMIT - Impose BCs for momentum equations when MIT 0-eqn model is active.
C This subroutine has the following entry's:
C  MODUMIT  - U-momentum boundary conditions - MIT 0-eqn model.
C  MODVMIT  - V-momentum boundary conditions - MIT 0-eqn model.
C  MODWMIT  - W-momentum boundary conditions - MIT 0-eqn model.

      SUBROUTINE BNDMIT
#include "cfd.h"
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLN(MCFND,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/WALLF/TAUEV(ntcelx,ntcely,ntcelz),
     1          TAUWV(ntcelx,ntcely,ntcelz),TAUEW(ntcelx,ntcely,ntcelz),
     2          TAUWW(ntcelx,ntcely,ntcelz),TAUNU(ntcelx,ntcely,ntcelz),
     3          TAUSU(ntcelx,ntcely,ntcelz),TAUNW(ntcelx,ntcely,ntcelz),
     4          TAUSW(ntcelx,ntcely,ntcelz),TAUHU(ntcelx,ntcely,ntcelz),
     5          TAULU(ntcelx,ntcely,ntcelz),TAUHV(ntcelx,ntcely,ntcelz),
     6          TAULV(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/CONST/GREAT,small,GRAV
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)
      COMMON/ICFNOD/ICFD,ICP

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ


C ********************* MODUMIT *********************
C MODUMIT  - Apply U-momentum boundary conditions for MIT 0-eqn model.
      ENTRY MODUMIT

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALV)THEN
        J=2
        DO 1100 I=1,NI
          DO 1101 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1101     CONTINUE
 1100   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 1200 I=1,NI
          DO 1201 J=1,NJ
            AH(I,J,K)=0.0
            AL(I,J,K)=0.0
 1201     CONTINUE
 1200   CONTINUE
      ENDIF

C Apply BCs on each solid boundary in turn.
      DO 101 L=1,NSB(ICFD)
C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        ISBii=ISBi(L,ICFD)
        DO 800 M=1,NOPEN(ICFD)
          IF(ISBi(L,ICFD).EQ.IOPENf(M,ICFD)+1) THEN
            ISBii=ISBii+1
            GOTO 801
          ENDIF
 800    CONTINUE

C South wall.
 801    IF(location.EQ.3)THEN
          J=JSBi(L,ICFD)

C Calculate distance from wall to next-to-wall grid point ($DELTA y sub p$).
          DYS=YP(J)-YV(J)

C Treat each cell on solid boundary.
          DO 102 I=ISBii,ISBf(L,ICFD)
            DO 1122 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Interpolate to get effective viscosity at U-velocity point.
C Note that effVIS is a viscosity, not a kinematic viscosity.
              effVIS = SIFW(I)*VIS(I,J,K)+SIFE(I-1)*VIS(I-1,J,K)

C Calculate wall shear stress (+ value means stress operates
C in same direction as axis).
              TAUSU(I,J,K) = -effVIS*Uf(I,J,K)/DYS

C Calculate source term. Units are force/velocity. Surface area of cell = SEWU*SHL.
C              SP(I,J,K) = SP(I,J,K) - (effVIS/DYS)*(SEWU(I)*SHL(K))
              SP(I,J,K) = SP(I,J,K) - (effVIS/DYS)*AreaNSU(I,K)

C Disconnect cell from fictitious cell to south.
              AS(I,J,K)=0.0
 1122       CONTINUE
 102      CONTINUE

C TAUSU values required at scalar points.
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 110 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUSU(II,J,K)=TAUSU(II+1,J,K)
            TAUSU(IFF+1,J,K)=TAUSU(IFF,J,K)
 110      CONTINUE

C North wall (see south for annotations).
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          DO 112 I=ISBii,ISBf(L,ICFD)
            DO 1121 K=KSBi(L,ICFD),KSBf(L,ICFD)
              effVIS = SIFW(I)*VIS(I,J,K)+SIFE(I-1)*VIS(I-1,J,K)
              TAUNU(I,J,K) = -effVIS*Uf(I,J,K)/DYN
              SP(I,J,K) = SP(I,J,K) - (effVIS/DYN)*AreaNSU(I,K)
              AN(I,J,K)=0.0
 1121       CONTINUE
 112      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 120 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUNU(II,J,K)=TAUNU(II+1,J,K)
            TAUNU(IFF+1,J,K)=TAUNU(IFF,J,K)
 120      CONTINUE

C Low wall (see south for annotations).
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          DO 122 I=ISBii,ISBf(L,ICFD)
            DO 1221 J=JSBi(L,ICFD),JSBf(L,ICFD)
              effVIS = SIFW(I)*VIS(I,J,K)+SIFE(I-1)*VIS(I-1,J,K)
              TAULU(I,J,K) = -effVIS*Uf(I,J,K)/DZL
              SP(I,J,K) = SP(I,J,K) - (effVIS/DZL)*AreaHLU(I,J)
              AL(I,J,K)=0.0
 1221       CONTINUE
 122      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 130 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAULU(II,J,K)=TAULU(II+1,J,K)
            TAULU(IFF+1,J,K)=TAULU(IFF,J,K)
 130      CONTINUE

C High wall (see south for annotations).
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          DO 132 I=ISBii,ISBf(L,ICFD)
            DO 1321 J=JSBi(L,ICFD),JSBf(L,ICFD)
              effVIS = SIFW(I)*VIS(I,J,K)+SIFE(I-1)*VIS(I-1,J,K)
              TAUHU(I,J,K) = -effVIS*Uf(I,J,K)/DZH
              SP(I,J,K) = SP(I,J,K) - (effVIS/DZH)*AreaHLU(I,J)
              AH(I,J,K)=0.0
 1321       CONTINUE
 132      CONTINUE
          II=ISBi(L,ICFD)
          IFF=ISBf(L,ICFD)
          DO 140 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUHU(II,J,K)=TAUHU(II+1,J,K)
            TAUHU(IFF+1,J,K)=TAUHU(IFF,J,K)
 140      CONTINUE

        ENDIF
 101  CONTINUE

C Momentum BCs for openings are treated in INOUT.

C Airflow at inlet not perpendicular to wall. Examine each opening and
C treat those that have `directed. flow.
      DO 10 L=1,NOPEN(ICFD)
        I=IOPENi(L,ICFD)

C South wall.
        IF(IWOPEN(L,ICFD).EQ.3.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 180 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J-1,K)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1801       CONTINUE
 180      CONTINUE

C North wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.4.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 185 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J+1,K)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1851       CONTINUE
 185      CONTINUE

C Low wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.5.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 190 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1901 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J,K-1)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1901       CONTINUE
 190      CONTINUE

C High wall.
        ELSE IF(IWOPEN(L,ICFD).EQ.6.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 195 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1951 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(I.EQ.IOPENi(L,ICFD)) Uf(I,J,K+1)=-Uf(I,J,K)
                IF(I.EQ.IOPENf(L,ICFD)) THEN
                  SP(I+1,J,K)=-GREAT
                  SU(I+1,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(I.NE.IOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(I.GT.IOPENi(L,ICFD).AND.I.LT.IOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 1951       CONTINUE
 195      CONTINUE
        ENDIF
 10   CONTINUE

      RETURN


C ********************* MODVMIT *********************
C MODVMIT  - Apply V-momentum boundary conditions for MIT 0-eqn model.
      ENTRY MODVMIT

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 1300 J=1,NJ
          DO 1301 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 1301     CONTINUE
 1300   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 1400 I=1,NI
          DO 1401 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 1401     CONTINUE
 1400   CONTINUE
      ENDIF

C Apply BCs on each solid boundary in turn.
      DO 201 L=1,NSB(ICFD)
C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        JSBii=JSBi(L,ICFD)
        DO 810 M=1,NOPEN(ICFD)
          IF(JSBi(L,ICFD).EQ.JOPENf(M,ICFD)+1) THEN
            JSBii=JSBii+1
            GOTO 811
          ENDIF
 810    CONTINUE

C West wall.
 811    IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)

C Calculate distance from wall to next-to-wall grid point ($DELTA y sub p$).
          DXW=XP(I)-XU(I)

C Treat each cell on solid boundary.
          DO 202 J=JSBii,JSBf(L,ICFD)
            DO 2021 K=KSBi(L,ICFD),KSBf(L,ICFD)

C Interpolate to get effective viscosity at V-velocity point.
C Note that effVIS is a viscosity, not a kinematic viscosity.
              effVIS = SIFS(J)*VIS(I,J,K)+SIFN(J-1)*VIS(I,J-1,K)

C Calculate wall shear stress (+ value means stress operates
C in same direction as axis).
              TAUWV(I,J,K) = -effVIS*Vf(I,J,K)/DXW

C Calculate source term. Units are force/velocity. Surface area of cell = SNSV*SHL.
C              SP(I,J,K) = SP(I,J,K) - (effVIS/DXW)*(SNSV(J)*SHL(K))
              SP(I,J,K) = SP(I,J,K) - (effVIS/DXW)*AreaEWV(J,K)

C Disconnect cell from fictitious cell to west.
              AW(I,J,K)=0.0
 2021       CONTINUE
 202      CONTINUE

C TAUWV values required at scalar points.
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 210 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUWV(I,JI,K)=TAUWV(I,JI+1,K)
            TAUWV(I,JF+1,K)=TAUWV(I,JF,K)
 210      CONTINUE

C East wall (see west for annotations).
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          DO 212 J=JSBii,JSBf(L,ICFD)
            DO 2121 K=KSBi(L,ICFD),KSBf(L,ICFD)
              effVIS = SIFS(J)*VIS(I,J,K)+SIFN(J-1)*VIS(I,J-1,K)
              TAUEV(I,J,K) = -effVIS*Vf(I,J,K)/DXE
              SP(I,J,K) = SP(I,J,K) - (effVIS/DXE)*AreaEWV(J,K)
              AE(I,J,K)=0.0
 2121       CONTINUE
 212      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 220 K=KSBi(L,ICFD),KSBf(L,ICFD)
            TAUEV(I,JI,K)=TAUEV(I,JI+1,K)
            TAUEV(I,JF+1,K)=TAUEV(I,JF,K)
 220      CONTINUE

C Low wall (see west for annotations).
        ELSEIF(location.EQ.5)THEN
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          DO 222 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2221 J=JSBii,JSBf(L,ICFD)
              effVIS = SIFS(J)*VIS(I,J,K)+SIFN(J-1)*VIS(I,J-1,K)
              TAULV(I,J,K) = -effVIS*Vf(I,J,K)/DZL
              SP(I,J,K) = SP(I,J,K) - (effVIS/DZL)*AreaHLV(I,J)
              AL(I,J,K)=0.0
 2221       CONTINUE
 222      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 230 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAULV(I,JI,K)=TAULV(I,JI+1,K)
            TAULV(I,JF+1,K)=TAULV(I,JF,K)
 230      CONTINUE

C High wall (see west for annotations).
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          DO 232 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2321 J=JSBii,JSBf(L,ICFD)
              effVIS = SIFS(J)*VIS(I,J,K)+SIFN(J-1)*VIS(I,J-1,K)
              TAUHV(I,J,K) = -effVIS*Vf(I,J,K)/DZH
              SP(I,J,K) = SP(I,J,K) - (effVIS/DZH)*AreaHLV(I,J)
              AH(I,J,K)=0.0
 2321       CONTINUE
 232      CONTINUE
          JI=JSBi(L,ICFD)
          JF=JSBf(L,ICFD)
          DO 240 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUHV(I,JI,K)=TAUHV(I,JI+1,K)
            TAUHV(I,JF+1,K)=TAUHV(I,JF,K)
 240      CONTINUE

        ENDIF
 201  CONTINUE

C Airflow at inlet not perpendicular to wall. Examine each opening and
C treat those that have `directed. flow.
      DO 241 L=1,NOPEN(ICFD)
        J=JOPENi(L,ICFD)
C West wall.
        IF(IWOPEN(L,ICFD).EQ.1.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 280 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 2801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I-1,J,K)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 2801       CONTINUE
 280      CONTINUE

C East wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.2.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 285 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 2851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(THETA1(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
              ELSEIF(THETA1(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I+1,J,K)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                ENDIF
              ENDIF
 2851       CONTINUE
 285      CONTINUE

C Low wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.5.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 290 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2901 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA2(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
              ELSEIF(THETA2(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I,J,K-1)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                ENDIF
              ENDIF
 2901       CONTINUE
 290      CONTINUE

C High wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.6.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 295 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2951 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(THETA2(L,ICFD).LT.0.) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
              ELSEIF(THETA2(L,ICFD).GT.0.) THEN
                IF(J.EQ.JOPENi(L,ICFD)) Vf(I,J,K+1)=-Vf(I,J,K)
                IF(J.EQ.JOPENf(L,ICFD)) THEN
                  SP(I,J+1,K)=-GREAT
                  SU(I,J+1,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                  IF(J.NE.JOPENi(L,ICFD)) THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                  ENDIF
                ENDIF
                IF(J.GT.JOPENi(L,ICFD).AND.J.LT.JOPENf(L,ICFD)) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                ENDIF
              ENDIF
 2951       CONTINUE
 295      CONTINUE
        ENDIF
 241  CONTINUE

      RETURN


C ********************* MODWMIT *********************
C MODWMIT  - Apply W-momentum boundary conditions for MIT 0-eqn model.
      ENTRY MODWMIT

C If flow is 1D or 2D make coefficients in other directions zero.
      IF(.NOT.INCALU)THEN
        I=2
        DO 1500 J=1,NJ
          DO 1511 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 1511     CONTINUE
 1500   CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 1600 I=1,NI
          DO 1601 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1601   CONTINUE
 1600   CONTINUE
      ENDIF

C Apply BCs on each solid boundary in turn.
      DO 301 L=1,NSB(ICFD)
C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10
        KSBii=KSBi(L,ICFD)
        DO 820 M=1,NOPEN(ICFD)
          IF(KSBi(L,ICFD).EQ.KOPENf(M,ICFD)+1) THEN
            KSBii=KSBii+1
            GOTO 821
          ENDIF
 820    CONTINUE

C West wall.
 821      IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)

C Calculate distance from wall to next-to-wall grid point ($DELTA y sub p$).
          DXW=XP(I)-XU(I)

C Treat each cell on solid boundary.
          DO 302 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3021 K=KSBii,KSBf(L,ICFD)

C Interpolate to get effective viscosity at W-velocity point.
C Note that effVIS is a viscosity, not a kinematic viscosity.
              effVIS = SIFL(K)*VIS(I,J,K)+SIFH(K-1)*VIS(I,J,K-1)

C Calculate wall shear stress (+ value means stress operates
C in same direction as axis).
              TAUWW(I,J,K) = -effVIS*Wf(I,J,K)/DXW

C Calculate source term. Units are force/velocity. Surface area of cell = SNS*SHLW.
C              SP(I,J,K) = SP(I,J,K) - (effVIS/DXW)*(SHLW(K)*SNS(J))
              SP(I,J,K) = SP(I,J,K) - (effVIS/DXW)*AreaEWW(J,K)

C Disconnect cell from fictitious cell to west.
              AW(I,J,K)=0.0
 3021       CONTINUE
 302      CONTINUE

C TAUWW values required at scalar points.
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 310 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUWW(I,J,KI)=TAUWW(I,J,KI+1)
            TAUWW(I,J,KF+1)=TAUWW(I,J,KF)
 310      CONTINUE

C East wall (see west for annotations).
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          DO 312 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3121 K=KSBii,KSBf(L,ICFD)
              effVIS = SIFL(K)*VIS(I,J,K)+SIFH(K-1)*VIS(I,J,K-1)
              TAUEW(I,J,K) = -effVIS*Wf(I,J,K)/DXE
              SP(I,J,K) = SP(I,J,K) - (effVIS/DXE)*AreaEWW(J,K)
              AE(I,J,K)=0.0
 3121       CONTINUE
 312      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 320 J=JSBi(L,ICFD),JSBf(L,ICFD)
            TAUEW(I,J,KI)=TAUEW(I,J,KI+1)
            TAUEW(I,J,KF+1)=TAUEW(I,J,KF)
 320      CONTINUE

C South wall (see west for annotations).
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          DO 322 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3221 K=KSBii,KSBf(L,ICFD)
              effVIS = SIFL(K)*VIS(I,J,K)+SIFH(K-1)*VIS(I,J,K-1)
              TAUSW(I,J,K) = -effVIS*Wf(I,J,K)/DYS
              SP(I,J,K) = SP(I,J,K) - (effVIS/DYS)*AreaNSW(I,K)
              AS(I,J,K)=0.0
 3221       CONTINUE
 322      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 330 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUSW(I,J,KI)=TAUSW(I,J,KI+1)
            TAUSW(I,J,KF+1)=TAUSW(I,J,KF)
 330      CONTINUE

C North wall (see west for annotations).
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          DO 332 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3321 K=KSBii,KSBf(L,ICFD)
              effVIS = SIFL(K)*VIS(I,J,K)+SIFH(K-1)*VIS(I,J,K-1)
              TAUNW(I,J,K) = -effVIS*Wf(I,J,K)/DYN
              SP(I,J,K) = SP(I,J,K) - (effVIS/DYN)*AreaNSW(I,K)
              AN(I,J,K)=0.0
 3321       CONTINUE
 332      CONTINUE
          KI=KSBi(L,ICFD)
          KF=KSBf(L,ICFD)
          DO 340 I=ISBi(L,ICFD),ISBf(L,ICFD)
            TAUNW(I,J,KI)=TAUNW(I,J,KI+1)
            TAUNW(I,J,KF+1)=TAUNW(I,J,KF)
 340      CONTINUE

        ENDIF
 301  CONTINUE

      RETURN
      END
      
C ************* Wall functions *************
C Yuan wall function calculation for vertical surfaces. Given cell id, face area,
C distance, ESP-r surface number and CFD boundary number calculate surface 
C convection and add to source term. Valid for k-e model.
C Ref: Ian Beausoleil-Morrison PhD thesis section 5.3.1

      subroutine CalcYuanWallFlux(I,J,K,Area,ydist,Isurf,Lwall)
      implicit none
#include "cfd.h"
      
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/YUANcm/Uqstor(ntcelx,ntcely,ntcelz),Uqinit
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/CONST/GREAT,small,GRAV

C Common variables
      real AP,AE,AW,AN,AS,AH,AL,SU,SP             ! common/COEF
      real URFVIS,VISCOS,PRANDT,SH,DENf,VIS,BETA  ! common/FLUPRf
      real Uqstor                                 ! common/YUANcm
      logical Uqinit                              ! common/YUANcm
      integer ICFD,ICP                            ! common/ICFNOD
      real CONVF                                  ! common/CFDCON
      integer NSB,ISBi,ISBf,JSBi,JSBf,KSBi,KSBf   ! common/Sbdary
      integer ISUFLC,IWSB,ITCtype,icTREF,IVOLNSB  ! common/Sbdary
      real SSB,SSBHC                              ! common/Sbdary
      real Tf,GAMH,RESORT,URFT,FSDTT,PRANDL,PFUN  ! common/TEMPf
      integer NSWPT                               ! common/TEMPf
      real GREAT,small,GRAV                       ! common/CONST
      
C Arguments to subroutine
      integer I,J,K,Isurf,Lwall   ! Current focus cell; ESP-r surface number; CFD surface
      real Area, ydist            ! Area of face incontact with solid boundary; distance to air point.
      
C Local variables
      integer iter
      real Uq, Uqold, therdif, ystar, Tstar, Qconv, qflux, Tq, err
     
C Iterate until Uq converged. Use 0.02 as first guess for Uq, as suggested
C by Yuan (thesis p89).
c      write(74,*) 'Starting iteration for cell',I,J,K
      iter = 0
      Uq = 0.02

C Calculate thermal diffusivity using fixed properties and density of cell.
      therdif = VISCOS / (PRANDL*DENf(i,j,k))

C Get dimensionless distance from wall ($y sup *$) using current Uq.
 10   Uqold = Uq
      ystar = ydist*Uqold/therdif

C Get dimensionless temperature ($T sup *$) using Yuan's temperature
C wall function. Three forms depending on placement of $y sup *$.
      if(ystar .le. 1.)then
        Tstar = ystar
      elseif (ystar .gt. 100.) then
        Tstar = 4.4
      else
        Tstar = 1. + 1.36*ALOG(ystar) - 0.135*(ALOG(ystar))**2
      endif

C Calculate the heat flux temperature ($T sub q$). Force Tq to be positive to
C avoid crashing calculation. Note that Tq is in Kelvin, not oC. This differs from
C the iterative approach laid out in Yuan's thesis. His approach is more implicit
C and contains an equation error on p89.
      Tq = ABS( SSB(Lwall,ICFD) - Tf(i,j,K) ) / Tstar

C Calculate the heat flux from the wall.
      qflux = Tq**(4./3.) * (GRAV*BETA(I,J,K)*therdif)**(1./3.) *
     &                 (DENf(i,j,k)*SH)

C Calculate the updated velocity scale based on the heat flux ($U sub q$).
      Uq = ( (GRAV*BETA(I,J,K)*therdif*qflux)/(DENf(i,j,k)*SH) )**0.25

C Check for convergence. Tolerence based on recommendation on p89 of Yuan thesis.
C Allow at most 20 iterations.
      iter = iter + 1
      err=0.
      if (Uq.ne.0.) then
        err=ABS(Uq-Uqold)/Uq
      endif
      if( err.gt.0.01 .and. iter.le.20 ) goto 10

C Converged. Since Tq forced to positive determine direction of surface convection
C based on wall and fluid temperatures. QW is heat flow (W) from wall to air.
      if( SSB(Lwall,ICFD).gt.Tf(i,j,K) )then
        Qconv = AREA*qflux
      else
        Qconv = -1.*AREA*qflux
      endif
      CONVF(ICFD,Isurf)=CONVF(ICFD,Isurf)-Qconv

C Add surface convection to source term. This should be revised so that
C SU and SP are adjusted as with the other Dirichlet BCs (log-law and MIT).
      SU(I,J,K)=SU(I,J,K)+Qconv/SH

C Save velocity scale based on the heat flux. This will be used in evaluation
C momentum wall functions.
      Uqstor(i,j,k) = Uq

      return
      end
           