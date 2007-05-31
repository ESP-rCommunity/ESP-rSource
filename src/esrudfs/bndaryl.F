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
C   BNDARL - Establish boundary conditions for laminar flow.

C ********************* BNDARL *********************
C BNDARL - Establish the laminar flow boundary conditions.
C This subroutine has the following entry's:
C  MODUL  - U direction boundary conditions - laminar.
C  MODVL  - V direction boundary conditions - laminar.
C  MODWL  - W direction boundary conditions - laminar.
C  MODTL  - temperature boundary conditions - laminar.

      SUBROUTINE BNDARL
      INCLUDE 'cfd.h'

      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),ICNACT(MCFND,MNZ)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
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
      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)

      CHARACTER*72 HSID

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD,BHS,IMITZ
      LOGICAL IZEROT,IZanKE

C ********************* MODUL *********************
C MODUL -  U direction boundary conditions - laminar.
      ENTRY MODUL

C If flow is 1D or 2D make coefficients in other directions zero.  
      IF(.NOT.INCALV)THEN
        J=2
        DO 100 I=1,NI
          DO 1001 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 1001    CONTINUE
 100   CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 105 I=1,NI
          DO 1051 J=1,NJ
            AH(I,J,K)=0.0
            AL(I,J,K)=0.0
 1051     CONTINUE
 105    CONTINUE
      ENDIF

C Solid walls.
      DO 135 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C "No slip" condition.
        IF(location.EQ.3)THEN

C Set up boundary conditions on SOUTH solid wall.
          J=JSBi(L,ICFD)
          DO 140 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Uf(I,J-1,K)=-Uf(I,J,K)
 1401       CONTINUE
 140      CONTINUE

        ELSEIF(location.EQ.4)THEN

C Set up boundary conditions on NORTH solid wall.
          J=JSBi(L,ICFD)
          DO 145 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1451 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Uf(I,J+1,K)=-Uf(I,J,K)
 1451       CONTINUE
 145      CONTINUE

        ELSEIF(location.EQ.5)THEN

C Set up boundary conditions on LOW solid wall.
          K=KSBi(L,ICFD)
          DO 150 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1501 J=JSBi(L,ICFD),JSBf(L,ICFD)
              Uf(I,J,K-1)=-Uf(I,J,K)
 1501       CONTINUE
 150      CONTINUE

C Set up boundary conditions on HIGH solid wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DO 155 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1551 J=JSBi(L,ICFD),JSBf(L,ICFD)
              Uf(I,J,K+1)=-Uf(I,J,K)
 1551       CONTINUE
 155      CONTINUE

C Symmetric axis
C Set up boundary conditions on SOUTH  wall.
        ELSEIF(location.EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 160 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1601 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              Uf(I,J-1,K)=Uf(I,J,K)
 1601       CONTINUE
 160      CONTINUE

C Set up boundary conditions on NORTH  wall.
        ELSEIF(location.EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 165 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1651 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              Uf(I,J+1,K)=Uf(I,J,K)
 1651       CONTINUE
 165      CONTINUE

C Set up boundary conditions on LOW  wall.
        ELSEIF(location.EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 170 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1701 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              Uf(I,J,K-1)=Uf(I,J,K)
 1701       CONTINUE
 170      CONTINUE

C Set up boundary conditions on HIGH wall.
        ELSEIF(location.EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 175 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 1751 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              Uf(I,J,K+1)=Uf(I,J,K)
 1751       CONTINUE
 175      CONTINUE
        ENDIF
  135 CONTINUE

C Openings.
      DO 110 L=1,NOPEN(ICFD)
        I=IOPENi(L,ICFD)

C West wall.
        IF(IWOPEN(L,ICFD).EQ.1)THEN

C Velocity.
C          DO 115 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
C            DO 1151 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I,J,K)=-GREAT
C              SU(I,J,K)=GREAT*Uf(I,J,K)
C 1151       CONTINUE
C 115      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.-1)THEN

C Zero velocity gradient.
          DO 120 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 1201 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I,J,K)=-GREAT
C              SU(I,J,K)=GREAT*Uf(I+1,J,K)
C              Uf(I,J,K)=Uf(I+1,J,K)
              AW(I,J,K)=0.0
 1201       CONTINUE
 120      CONTINUE

C East wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.2)THEN

C Velocity.
C          DO 125 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
C            DO 1251 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*Uf(I+1,J,K)
C 1251       CONTINUE
C 125      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.-2)THEN

C Zero velocity gradient.
          DO 130 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 1301 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I+1,J,K)=-GREAT
C              SU(I+1,J,K)=GREAT*Uf(I,J,K)
C              Uf(I+1,J,K)=Uf(I,J,K)
              AE(I,J,K)=0.0
 1301       CONTINUE
 130      CONTINUE

C Inlet velocity direction.
C South wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.3.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 180 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(I.EQ.IOPENi(L,ICFD)) THEN
                IF(THETA1(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
                ELSE
                  Uf(I,J-1,K)=-Uf(I,J,K)
                ENDIF
              ENDIF
              IF(I.EQ.IOPENf(L,ICFD).AND.THETA1(L,ICFD).GT.0.) THEN
                SP(I+1,J,K)=-GREAT
                SU(I+1,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
              ENDIF
              IF(I.NE.IOPENi(L,ICFD).AND.I.NE.IOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA1(L,ICFD))
              ENDIF
 1801       CONTINUE
 180      CONTINUE

C North wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.4.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 185 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(I.EQ.IOPENi(L,ICFD)) THEN
                IF(THETA1(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
                ELSE
                  Uf(I,J+1,K)=-Uf(I,J,K)
                ENDIF
              ENDIF
              IF(I.EQ.IOPENf(L,ICFD).AND.THETA1(L,ICFD).GT.0.) THEN
                SP(I+1,J,K)=-GREAT
                SU(I+1,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
              ENDIF
              IF(I.NE.IOPENi(L,ICFD).AND.I.NE.IOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA1(L,ICFD))
              ENDIF
 1851       CONTINUE
 185      CONTINUE

C Low wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.5.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 190 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 1901 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(I.EQ.IOPENi(L,ICFD)) THEN
                IF(THETA1(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
                ELSE
                  Uf(I,J,K-1)=-Uf(I,J,K)
                ENDIF
              ENDIF
              IF(I.EQ.IOPENf(L,ICFD).AND.THETA1(L,ICFD).GT.0.) THEN
                SP(I+1,J,K)=-GREAT
                SU(I+1,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
              ENDIF
              IF(I.NE.IOPENi(L,ICFD).AND.I.NE.IOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA1(L,ICFD))
              ENDIF
 1901       CONTINUE
 190      CONTINUE

C High wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.6.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 195 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
          DO 195 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
             IF(I.EQ.IOPENi(L,ICFD)) THEN
               IF(THETA1(L,ICFD).LT.0.) THEN
                 SP(I,J,K)=-GREAT
                 SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
               ELSE
                 Uf(I,J,K+1)=-Uf(I,J,K)
               ENDIF
             ENDIF
             IF(I.EQ.IOPENf(L,ICFD).AND.THETA1(L,ICFD).GT.0.) THEN
               SP(I+1,J,K)=-GREAT
               SU(I+1,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
             ENDIF
             IF(I.NE.IOPENi(L,ICFD).AND.I.NE.IOPENf(L,ICFD)) THEN
               SP(I,J,K)=-GREAT
               SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA1(L,ICFD))
             ENDIF
 195       CONTINUE
        ENDIF
 110  CONTINUE
      RETURN


C ********************* MODVL *********************
C MODVL - V direction boundary conditions - laminar.
      ENTRY MODVL

C If flow is 1D or 2D make coefficients in other directions zero.  
      IF(.NOT.INCALU)THEN
        I=2
        DO 200 J=1,NJ
          DO 2001 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 2001     CONTINUE
 200    CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 205 I=1,NI
          DO 2051 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 2051     CONTINUE
 205    CONTINUE
      ENDIF

C Solid walls.
C Set up boundary conditions for WEST solid wall.
      DO 210 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

        IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)
          DO 215 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2151 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Vf(I-1,J,K)=-Vf(I,J,K)
 2151       CONTINUE
 215      CONTINUE

C Set up boundary conditions for EAST solid wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DO 220 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2201 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Vf(I+1,J,K)=-Vf(I,J,K)
 2201       CONTINUE
 220      CONTINUE

C Set up boundary conditions on LOW solid wall.
        ELSEIF(location.EQ.5)THEN
        K=KSBi(L,ICFD)
        DO 225 I=ISBi(L,ICFD),ISBf(L,ICFD)
          DO 2251 J=JSBi(L,ICFD),JSBf(L,ICFD)
            Vf(I,J,K-1)=-Vf(I,J,K)
 2251     CONTINUE
 225    CONTINUE

C Set up boundary conditions on HIGH solid wall.
        ELSEIF(location.EQ.6)THEN
          K=KSBi(L,ICFD)
          DO 230 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2301 J=JSBi(L,ICFD),JSBf(L,ICFD)
              Vf(I,J,K+1)=-Vf(I,J,K)
 2301       CONTINUE
 230      CONTINUE

C Symmetric Axis
C Set up boundary conditions for WEST solid wall.
        ELSEIF(location.EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 235 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2351 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              Vf(I-1,J,K)=Vf(I,J,K)
 2351       CONTINUE
 235      CONTINUE

C Set up boundary conditions for EAST solid wall.
        ELSEIF(location.EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 240 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 2401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              Vf(I+1,J,K)=Vf(I,J,K)
 2401       CONTINUE
 240      CONTINUE

C Set up boundary conditions on LOW solid wall.
        ELSEIF(location.EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 245 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2451 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              Vf(I,J,K-1)=Vf(I,J,K)
 2451       CONTINUE
 245      CONTINUE

C Set up boundary conditions on HIGH solid wall.
        ELSEIF(location.EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 250 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 2501 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              Vf(I,J,K+1)=Vf(I,J,K)
 2501       CONTINUE
 250      CONTINUE
        ENDIF
  210 CONTINUE

C Openings.
      DO 255 L=1,NOPEN(ICFD)
        J=JOPENi(L,ICFD)

C South wall.
        IF(IWOPEN(L,ICFD).EQ.3)THEN

C Velocity.
C          DO 260 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
C            DO 2601 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I,J,K)=-GREAT
C              SU(I,J,K)=GREAT*Vf(I,J,K)
C 2601        CONTINUE
C 260      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.-3)THEN

C Zero velocity gradient.
          DO 265 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2651 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I,J,K)=-GREAT
C              SU(I,J,K)=GREAT*Vf(I,J+1,K)
C              Vf(I,J,K)=Vf(I,J+1,K)
              AS(I,J,K)=0.0
 2651       CONTINUE
 265      CONTINUE

C North wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.4)THEN

C Velocity.
C          DO 270 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
C            DO 2701 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*Vf(I,J+1,K)
C 2701       CONTINUE
C 270      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.-4)THEN

C Zero velocity gradient.
          DO 275 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2751 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
C              SP(I,J+1,K)=-GREAT
C              SU(I,J+1,K)=GREAT*Vf(I,J,K)
C              Vf(I,J+1,K)=Vf(I,J,K)
              AN(I,J,K)=0.0
 2751       CONTINUE
 275      CONTINUE

C Inlet velocity direction.
C West wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.1.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 280 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 2801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(J.EQ.JOPENi(L,ICFD)) THEN
                IF(THETA1(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
                ELSE
                  Vf(I-1,J,K)=-Vf(I,J,K)
                ENDIF
              ENDIF
              IF(J.EQ.JOPENf(L,ICFD).AND.THETA1(L,ICFD).GT.0.) THEN
                SP(I,J+1,K)=-GREAT
                SU(I,J+1,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
              ENDIF
              IF(J.NE.JOPENi(L,ICFD).AND.J.NE.JOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA1(L,ICFD))
              ENDIF
 2801       CONTINUE
 280      CONTINUE

C East wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.2.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 285 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 2851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(J.EQ.JOPENi(L,ICFD)) THEN
                IF(THETA1(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
                ELSE
                  Vf(I+1,J,K)=-Vf(I,J,K)
                ENDIF
              ENDIF
              IF(J.EQ.JOPENf(L,ICFD).AND.THETA1(L,ICFD).GT.0.) THEN
                SP(I,J+1,K)=-GREAT
                SU(I,J+1,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
              ENDIF
              IF(J.NE.JOPENi(L,ICFD).AND.J.NE.JOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA1(L,ICFD))
              ENDIF
 2851       CONTINUE
 285      CONTINUE

C Low wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.5.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 290 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2901 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(J.EQ.JOPENi(L,ICFD)) THEN
                IF(THETA2(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
                ELSE
                  Vf(I,J,K-1)=-Vf(I,J,K)
                ENDIF
              ENDIF
              IF(J.EQ.JOPENf(L,ICFD).AND.THETA2(L,ICFD).GT.0.) THEN
                SP(I,J+1,K)=-GREAT
                SU(I,J+1,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
              ENDIF
              IF(J.NE.JOPENi(L,ICFD).AND.J.NE.JOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Wf(I,J,K)*ETAND(THETA2(L,ICFD))
              ENDIF
 2901       CONTINUE
 290      CONTINUE

C High wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.6.AND.FIXM(L,ICFD).GT.0.) THEN
          K=KOPENi(L,ICFD)
          DO 295 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 2951 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
              IF(J.EQ.JOPENi(L,ICFD)) THEN
                IF(THETA2(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
                ELSE
                  Vf(I,J,K+1)=-Vf(I,J,K)
                ENDIF
              ENDIF
              IF(J.EQ.JOPENf(L,ICFD).AND.THETA2(L,ICFD).GT.0.) THEN
                SP(I,J+1,K)=-GREAT
                SU(I,J+1,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
              ENDIF
              IF(J.NE.JOPENi(L,ICFD).AND.J.NE.JOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Wf(I,J,K))*ETAND(THETA2(L,ICFD))
              ENDIF
 2951       CONTINUE
 295      CONTINUE
        ENDIF
 255  CONTINUE
      RETURN


C ********************* MODWL *********************
C MODWL - W direction boundary conditions - laminar.
      ENTRY MODWL

C If flow is 1D or 2D make coefficients in other directions zero.  
      IF(.NOT.INCALU)THEN
        I=2
        DO 300 J=1,NJ
          DO 3001 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 3001     CONTINUE
 300    CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 305 I=1,NI
          DO 3051 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 3051    CONTINUE
 305    CONTINUE
      ENDIF

C Solid walls.
      DO 310 L=1,NSB(ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.
        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

C Set up boundary conditions for WEST solid wall.
        IF(location.EQ.1)THEN
          I=ISBi(L,ICFD)
          DO 315 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3151 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Wf(I-1,J,K)=-Wf(I,J,K)
 3151       CONTINUE
 315      CONTINUE

C Set up boundary conditions for EAST solid wall.
        ELSEIF(location.EQ.2)THEN
          I=ISBi(L,ICFD)
          DO 320 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3201 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Wf(I+1,J,K)=-Wf(I,J,K)
 3201       CONTINUE
 320      CONTINUE

C Set up boundary conditions on SOUTH solid wall.
        ELSEIF(location.EQ.3)THEN
          J=JSBi(L,ICFD)
          DO 325 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3251 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Wf(I,J-1,K)=-Wf(I,J,K)
 3251       CONTINUE
 325      CONTINUE

C Set up boundary conditions on NORTH solid wall.
        ELSEIF(location.EQ.4)THEN
          J=JSBi(L,ICFD)
          DO 330 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3301 K=KSBi(L,ICFD),KSBf(L,ICFD)
              Wf(I,J+1,K)=-Wf(I,J,K)
 3301       CONTINUE
 330      CONTINUE

C Symmetric axis.
C Set up boundary conditions for WEST solid wall.
        ELSEIF(location.EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 335 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3351 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              Wf(I-1,J,K)=Wf(I,J,K)
 3351      CONTINUE
 335      CONTINUE

C Set up boundary conditions for EAST solid wall.
        ELSEIF(location.EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 340 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 3401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              Wf(I+1,J,K)=Wf(I,J,K)
 3401       CONTINUE
 340      CONTINUE

C Set up boundary conditions on SOUTH solid wall.
        ELSEIF(location.EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 345 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3451 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              Wf(I,J-1,K)=Wf(I,J,K)
 3451       CONTINUE
 345      CONTINUE

C Set up boundary conditions on NORTH solid wall.
        ELSEIF(location.EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 350 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 3501 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              Wf(I,J+1,K)=Wf(I,J,K)
 3501       CONTINUE
 350      CONTINUE
        ENDIF
 310  CONTINUE

C Openings.
      DO 355 L=1,NOPEN(ICFD)
        K=KOPENi(L,ICFD)

C Low wall.
        IF(IWOPEN(L,ICFD).EQ.5)THEN

C Velocity.
C          DO 360 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
C            DO 3601 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
C              SP(I,J,K)=-GREAT
C              SU(I,J,K)=GREAT*Wf(I,J,K)
C 3601       CONTINUE
C 360      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.-5)THEN

C Zero velocity gradient.
          DO 365 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3651 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
C              SP(I,J,K)=-GREAT
C              SU(I,J,K)=GREAT*Wf(I,J,K+1)
C              Wf(I,J,K)=Wf(I,J,K+1)
              AL(I,J,K)=0.0
 3651       CONTINUE
 365      CONTINUE

C High wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.6)THEN

C Velocity.
C          DO 370 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
C            DO 3701 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
C              SP(I,J,K+1)=-GREAT
C              SU(I,J,K+1)=GREAT*Wf(I,J,K+1)
C 3701       CONTINUE
C 370      CONTINUE
        ELSEIF(IWOPEN(L,ICFD).EQ.-6)THEN

C Zero velocity gradient.
          DO 375 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3751 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
C              SP(I,J,K+1)=-GREAT
C              SU(I,J,K+1)=GREAT*Wf(I,J,K)
C              Wf(I,J,K+1)=Wf(I,J,K)
              AH(I,J,K)=0.0
 3751       CONTINUE
 375      CONTINUE

C Inlet velocity direction.
C West wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.1.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 380 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 3801 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(K.EQ.KOPENi(L,ICFD)) THEN
                IF(THETA2(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA2(L,ICFD))
                ELSE
                  Wf(I-1,J,K)=-Wf(I,J,K)
                ENDIF
              ENDIF
              IF(K.EQ.KOPENf(L,ICFD).AND.THETA2(L,ICFD).GT.0.) THEN
                SP(I,J,K+1)=-GREAT
                SU(I,J,K+1)=GREAT*Uf(I,J,K)*ETAND(THETA2(L,ICFD))
              ENDIF
              IF(K.NE.KOPENi(L,ICFD).AND.K.NE.KOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Uf(I,J,K)*ETAND(THETA2(L,ICFD))
              ENDIF
 3801       CONTINUE
 380      CONTINUE

C East wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.2.AND.FIXM(L,ICFD).GT.0.) THEN
          I=IOPENi(L,ICFD)
          DO 385 J=JOPENi(L,ICFD),JOPENf(L,ICFD)
            DO 3851 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(K.EQ.KOPENi(L,ICFD)) THEN
                IF(THETA2(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA2(L,ICFD))
                ELSE
                  Wf(I+1,J,K)=-Wf(I,J,K)
                ENDIF
              ENDIF
              IF(K.EQ.KOPENf(L,ICFD).AND.THETA2(L,ICFD).GT.0.) THEN
                SP(I,J,K+1)=-GREAT
                SU(I,J,K+1)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA2(L,ICFD))
              ENDIF
              IF(K.NE.KOPENi(L,ICFD).AND.K.NE.KOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Uf(I,J,K))*ETAND(THETA2(L,ICFD))
              ENDIF
 3851       CONTINUE
 385      CONTINUE

C South wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.3.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 390 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3901 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(K.EQ.KOPENi(L,ICFD)) THEN
                IF(THETA2(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA2(L,ICFD))
                ELSE
                  Wf(I,J-1,K)=-Wf(I,J,K)
                ENDIF
              ENDIF
              IF(K.EQ.KOPENf(L,ICFD).AND.THETA2(L,ICFD).GT.0.) THEN
                SP(I,J,K+1)=-GREAT
                SU(I,J,K+1)=GREAT*Vf(I,J,K)*ETAND(THETA2(L,ICFD))
              ENDIF
              IF(K.NE.KOPENi(L,ICFD).AND.K.NE.KOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*Vf(I,J,K)*ETAND(THETA2(L,ICFD))
              ENDIF
 3901       CONTINUE
 390      CONTINUE

C North wall.
        ELSEIF(IWOPEN(L,ICFD).EQ.4.AND.FIXM(L,ICFD).GT.0.) THEN
          J=JOPENi(L,ICFD)
          DO 395 I=IOPENi(L,ICFD),IOPENf(L,ICFD)
            DO 3951 K=KOPENi(L,ICFD),KOPENf(L,ICFD)
              IF(K.EQ.KOPENi(L,ICFD)) THEN
                IF(THETA2(L,ICFD).LT.0.) THEN
                  SP(I,J,K)=-GREAT
                  SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA2(L,ICFD))
                ELSE
                  Wf(I,J+1,K)=-Wf(I,J,K)
                ENDIF
              ENDIF
              IF(K.EQ.KOPENf(L,ICFD).AND.THETA2(L,ICFD).GT.0.) THEN
                SP(I,J,K+1)=-GREAT
                SU(I,J,K+1)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA2(L,ICFD))
              ENDIF
              IF(K.NE.KOPENi(L,ICFD).AND.K.NE.KOPENf(L,ICFD)) THEN
                SP(I,J,K)=-GREAT
                SU(I,J,K)=GREAT*ABS(Vf(I,J,K))*ETAND(THETA2(L,ICFD))
              ENDIF
 3951       CONTINUE
 395      CONTINUE
        ENDIF
 355  CONTINUE
      RETURN


C ********************* MODTL *********************
C MODTL - temperature boundary conditions - laminar.
      ENTRY MODTL

C This ENTRY imposes boundary conditions (BCs) for the thermal equation
C when there is laminar flow. It applies BCs at the air-flow openings, then
C it applies BCs on the solid surfaces, then finally it applies BCs for the
C heat sources.

C If flow is 1D or 2D make coefficients in other directions zero. 
      IF(.NOT.INCALU)THEN
        I=2
        DO 400 J=1,NJ
          DO 4001 K=1,NK
            AW(I,J,K)=0.0
            AE(I,J,K)=0.0
 4001     CONTINUE
 400    CONTINUE
      ENDIF
      IF(.NOT.INCALV)THEN
        J=2
        DO 405 I=1,NI
          DO 4051 K=1,NK
            AN(I,J,K)=0.0
            AS(I,J,K)=0.0
 4051     CONTINUE
 405    CONTINUE
      ENDIF
      IF(.NOT.INCALW)THEN
        K=2
        DO 410 I=1,NI
          DO 4101 J=1,NJ
            AL(I,J,K)=0.0
            AH(I,J,K)=0.0
 4101     CONTINUE
 410    CONTINUE
      ENDIF

C Reset heat flow to each solid boundary to zero. The heat flows for
C the current iteration will be recalculated later in this subroutine.
      DO 412 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        CONVF(ICFD,IS)=0.0
 412  CONTINUE
 
C Account for flow entering or leaving domain. Examine each opening in turn.
C Refer to annotations in subroutine BNDARTT (bndaryt.F) for a description
C of how the air flow openings are treated.
      DO 415 M=1,NOPEN(ICFD)
        IF(IWOPEN(M,ICFD).EQ.0)THEN
            DO 420 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 4201 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                DO 4202 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                  IF(IMFACT.EQ.1.AND.FIXM(M,ICFD).GT.0.0)THEN
                    SU(I,J,K)=SU(I,J,K)+FIXM(M,ICFD)*FIXT(M,ICFD)
                  ELSEIF(IMFACT.EQ.0.AND.FIXT(M,ICFD).LT.GREAT)THEN
                    SP(I,J,K)=-GREAT
                    SU(I,J,K)=GREAT*FIXT(M,ICFD)
                  ENDIF
 4202           CONTINUE
 4201         CONTINUE
 420        CONTINUE
        ELSEIF(IWOPEN(M,ICFD).EQ.99)THEN
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

C Outlets.
        IF(FIXM(M,ICFD).LT.0.) THEN

C West wall.
          IF(ABS(IWOPEN(M,ICFD)).EQ.1) THEN
            I=IOPENi(M,ICFD)
            DO 900 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9001 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AW(I,J,K)=0.0
 9001         CONTINUE
 900        CONTINUE

C East wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.2) THEN
            I=IOPENi(M,ICFD)
            DO 902 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
              DO 9021 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AE(I,J,K)=0.0
 9021         CONTINUE
 902        CONTINUE

C South wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.3) THEN
            J=JOPENi(M,ICFD)
            DO 904 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9041 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AS(I,J,K)=0.0
 9041         CONTINUE
 904        CONTINUE

C North wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.4) THEN
            J=JOPENi(M,ICFD)
            DO 906 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9061 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
                AN(I,J,K)=0.0
 9061         CONTINUE
 906        CONTINUE

C Low wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.5) THEN
            K=KOPENi(M,ICFD)
            DO 908 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9081 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AL(I,J,K)=0.0
 9081         CONTINUE
 908        CONTINUE

C High wall.
          ELSEIF(ABS(IWOPEN(M,ICFD)).EQ.6) THEN
            K=KOPENi(M,ICFD)
            DO 910 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
              DO 9101 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
                AH(I,J,K)=0.0
 9101         CONTINUE
 910        CONTINUE
          ENDIF
        ENDIF
 415  CONTINUE

C Apply BCs for the solid boundaries. Only three approaches are available
C for laminar flow. These are available for CFD-only analyses, but not for
C BSim-CFD conflated analyses.
C
C - Qsurf calculated by CFD using Qsurf=A*k*(Tsurf-Tp)/Yp, where Yp is the
C   distance to the next-to-wall grid point. This is identified by 1<=IWSB<=6.
C   This imposes the surface temperature.
C - Qsurf imposed on CFD and absorbed into the source term. Tsurf is not
C   imposed in this case. This is identified by -11>=IWSB>=-16.
C - Symmetry type boundary condition.

C Fixed temperature at the wall.
      DO 425 L=1,NSB(ICFD)

C West wall.
        IF(IWSB(L,ICFD).EQ.1)THEN
          I=ISBi(L,ICFD)
          DXW=XP(I)-XU(I)
          IS=ISUFLC(L,ICFD)
          DO 430 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4301 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              GT=VISCOS/(PRANDL*DXW)
              TERM=GT*AreaEWP(J,K)
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QW=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW
 4301       CONTINUE
 430      CONTINUE

C East wall.
        ELSEIF(IWSB(L,ICFD).EQ.2)THEN
          I=ISBi(L,ICFD)
          DXE=XU(I+1)-XP(I)
          IS=ISUFLC(L,ICFD)
          DO 435 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4351 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              GT=VISCOS/(PRANDL*DXE)
              TERM=GT*AreaEWP(J,K)
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QE=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
 4351       CONTINUE
 435      CONTINUE

C South wall.
        ELSEIF(IWSB(L,ICFD).EQ.3)THEN
          J=JSBi(L,ICFD)
          DYS=YP(J)-YV(J)
          IS=ISUFLC(L,ICFD)
          DO 440 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4401 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              GT=VISCOS/(PRANDL*DYS)
              TERM=GT*AreaNSP(I,K)
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QS=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
 4401       CONTINUE
 440      CONTINUE

C North wall.
        ELSEIF(IWSB(L,ICFD).EQ.4)THEN
          J=JSBi(L,ICFD)
          DYN=YV(J+1)-YP(J)
          IS=ISUFLC(L,ICFD)
          DO 445 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4451 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              GT=VISCOS/(PRANDL*DYN)
              TERM=GT*AreaNSP(I,K)
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QN=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
 4451       CONTINUE
 445      CONTINUE

C Low wall.
        ELSEIF(IWSB(L,ICFD).EQ.5)THEN
          K=KSBi(L,ICFD)
          DZL=ZP(K)-ZW(K)
          IS=ISUFLC(L,ICFD)
          DO 450 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4501 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              GT=VISCOS/(PRANDL*DZL)
              TERM=GT*AreaHLP(I,J)
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QL=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
 4501       CONTINUE
 450      CONTINUE

C High wall.
        ELSEIF(IWSB(L,ICFD).EQ.6)THEN
          K=KSBi(L,ICFD)
          DZH=ZW(K+1)-ZP(K)
          IS=ISUFLC(L,ICFD)
          DO 455 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4551 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              GT=VISCOS/(PRANDL*DZH)
              TERM=GT*AreaHLP(I,J)
              SU(I,J,K)=SU(I,J,K)+TERM*SSB(L,ICFD)
              SP(I,J,K)=SP(I,J,K)-TERM
              QH=TERM*SH*(SSB(L,ICFD)-Tf(i,j,K))
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
 4551       CONTINUE
 455      CONTINUE

C Fixed heat flux at the wall.
        ELSEIF(IWSB(L,ICFD).EQ.-1)THEN

C West wall.  
          I=ISBi(L,ICFD)
          IS=ISUFLC(L,ICFD)
          AREAT=(YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))*
     &        (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 460 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4601 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
              QW=SSB(L,ICFD)*AreaEWP(J,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QW/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QW
 4601       CONTINUE
 460      CONTINUE

C East wall.
        ELSEIF(IWSB(L,ICFD).EQ.-2)THEN
          I=ISBi(L,ICFD)
          IS=ISUFLC(L,ICFD)
          AREAT=(YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 465 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4651 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
              QE=SSB(L,ICFD)*AreaEWP(J,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QE/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QE
 4651       CONTINUE
 465      CONTINUE

C South wall.
        ELSEIF(IWSB(L,ICFD).EQ.-3)THEN
          J=JSBi(L,ICFD)
          IS=ISUFLC(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 470 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4701 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
              QS=SSB(L,ICFD)*AreaNSP(I,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QS/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QS
 4701       CONTINUE
 470      CONTINUE

C North wall.
        ELSEIF(IWSB(L,ICFD).EQ.-4)THEN
          J=JSBi(L,ICFD)
          IS=ISUFLC(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))
          DO 475 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4751 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
              QN=SSB(L,ICFD)*AreaNSP(I,K)/AREAT
              SU(I,J,K)=SU(I,J,K)+QN/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QN
 4751       CONTINUE
 475      CONTINUE

C Low wall.
        ELSEIF(IWSB(L,ICFD).EQ.-5)THEN
          K=KSBi(L,ICFD)
          IS=ISUFLC(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))
          DO 480 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4801 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
              QL=SSB(L,ICFD)*AreaHLP(I,J)/AREAT
              SU(I,J,K)=SU(I,J,K)+QL/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QL
 4801       CONTINUE
 480      CONTINUE

C High wall.
        ELSEIF(IWSB(L,ICFD).EQ.-6)THEN
          K=KSBi(L,ICFD)
          IS=ISUFLC(L,ICFD)
          AREAT=(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &          (YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))
          DO 485 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 4851 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
              QH=SSB(L,ICFD)*AreaHLP(I,J)/AREAT
              SU(I,J,K)=SU(I,J,K)+QH/SH
              CONVF(ICFD,IS)=CONVF(ICFD,IS)-QH
 4851       CONTINUE
 485      CONTINUE

C Symmetric axis.
C Set up boundary conditions for West solid wall.
        ELSEIF(IWSB(L,ICFD).EQ.11)THEN
          I=ISBi(L,ICFD)
          DO 490 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4901 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AW(I,J,K)=0.0
 4901       CONTINUE
 490      CONTINUE

C Set up boundary conditions for East solid wall.
        ELSEIF(IWSB(L,ICFD).EQ.12)THEN
          I=ISBi(L,ICFD)
          DO 495 J=JSBi(L,ICFD),JSBf(L,ICFD)
            DO 4951 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AE(I,J,K)=0.0
 4951       CONTINUE
 495      CONTINUE

C Set up boundary conditions on South wall.
        ELSEIF(IWSB(L,ICFD).EQ.13)THEN
          J=JSBi(L,ICFD)
          DO 500 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5001 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AS(I,J,K)=0.0
 5001       CONTINUE
 500      CONTINUE

C Set up boundary conditions on North wall.
        ELSEIF(IWSB(L,ICFD).EQ.14)THEN
          J=JSBi(L,ICFD)
          DO 505 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5051 K=KSBi(L,ICFD),KSBf(L,ICFD)
              AN(I,J,K)=0.0
 5051       CONTINUE
 505      CONTINUE
C Set up boundary conditions on Low wall.
        ELSEIF(IWSB(L,ICFD).EQ.15)THEN
          K=KSBi(L,ICFD)
          DO 510 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5101 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AL(I,J,K)=0.0
 5101       CONTINUE
 510      CONTINUE

C Set up boundary conditions on HIGH wall.
        ELSEIF(IWSB(L,ICFD).EQ.16)THEN
          K=KSBi(L,ICFD)
          DO 515 I=ISBi(L,ICFD),ISBf(L,ICFD)
            DO 5151 J=JSBi(L,ICFD),JSBf(L,ICFD)
              AH(I,J,K)=0.0
 5151       CONTINUE
 515      CONTINUE
        ENDIF
 425  CONTINUE

C Heat sources
      do 540 L=1,NHS(1,ICFD)
        do 5401 I=IHSi(L,1,ICFD),IHSf(L,1,ICFD)
          do 5402 J=JHSi(L,1,ICFD),JHSf(L,1,ICFD)
            do 5403 K=KHSi(L,1,ICFD),KHSf(L,1,ICFD)
              SU(I,J,K)=GREAT*SHS(L,1,ICFD)/SH
              SP(I,J,K)=GREAT
 5403       continue
 5402     continue
 5401   continue
 540  continue
       
      do 541 L=1,NHS(2,ICFD)
        VOLTOT=(XU(IHSf(L,2,ICFD)+1)-XU(IHSi(L,2,ICFD)))*
     &         (YV(JHSf(L,2,ICFD)+1)-YV(JHSi(L,2,ICFD)))*
     &         (ZW(KHSf(L,2,ICFD)+1)-ZW(KHSi(L,2,ICFD)))
        do 5411 I=IHSi(L,2,ICFD),IHSf(L,2,ICFD)
          do 5412 J=JHSi(L,2,ICFD),JHSf(L,2,ICFD)
            do 5413 K=KHSi(L,2,ICFD),KHSf(L,2,ICFD)
              VOLCEL=VolP(I,J,K)
              SU(I,J,K)=SU(I,J,K)+SHS(L,2,ICFD)/VOLTOT*VOLCEL/SH
 5413       continue
 5412     continue
 5411   continue
 541  continue

      RETURN
      END
