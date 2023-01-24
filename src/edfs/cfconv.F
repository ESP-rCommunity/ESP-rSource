C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following subroutines.
C  CFAREA - calculates the area of ESP-r surfaces covered by CFD solid boundaries.
C  CFCONV - calculate convection coefficients based on the CFD predictions.

C ********************* CFAREA *********************
C CFAREA - Calculates the area of ESP-r surfaces covered by CFD solid boundaries.

      SUBROUTINE CFAREA(NCNST)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/CFDARE/ATCFD(MNSBZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

C It is possible to assign a number of CFD solid boundaries to each standard
C ESP-r surface. This subroutine calculates the area of each ESP-r surface
C that is covered by CFD solid boundaries, by summing the areas of all the
C CFD solid boundaries on each surface. ATCFD(j) holds the area of the j'th
C ESP-r surface.


C Initialise all areas to zero.
      DO 10 IS=1,NCNST
        ATCFD(IS)=0.0
 10   CONTINUE

C Examine each CFD solid boundary in turn.
      DO 20 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)

C Determine upon which face of the CFD domain the solid boundary resides.
C `location' is equal to the least significant digit of IWSB and has the
C following meanings: 1 for west; 2 for east; 3 for south; 4 for north;
C 5 for low; 6 for high.

        location = abs(IWSB(L,ICFD)) - abs(IWSB(L,ICFD))/10*10

        IF(location.EQ.1 .or. location.EQ.2) THEN
C---------West or east.
          ATCFD(IS)=ATCFD(IS)+(YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))*
     &              (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))

        ELSEIF(location.EQ.3 .or. location.EQ.4) THEN
C---------South or north.
          ATCFD(IS)=ATCFD(IS)+(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &              (ZW(KSBf(L,ICFD)+1)-ZW(KSBi(L,ICFD)))

        ELSEIF(location.EQ.5 .or. location.EQ.6) THEN
C---------low or high.
          ATCFD(IS)=ATCFD(IS)+(XU(ISBf(L,ICFD)+1)-XU(ISBi(L,ICFD)))*
     &              (YV(JSBf(L,ICFD)+1)-YV(JSBi(L,ICFD)))
        ENDIF

 20   CONTINUE

      RETURN
      END


C ********************* CFCONV *********************
C CFCONV - Calculate convection coefficients based on the CFD predictions.
      SUBROUTINE CFCONV(TAIR,HC,SFNAM,NCNST)
#include "building.h"
#include "cfd.h"

      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      COMMON/CFDARE/ATCFD(MNSBZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CONST/GREAT,small,GRAV
C      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
C      CHARACTER*72 LCFD
C      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
C      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
C      1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
C      2            DZHP(ntcelz),DZPL(ntcelz),
C      3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
C      4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
C      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
C     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
C     2            BETA(ntcelx,ntcely,ntcelz)
C      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
C     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN

      DIMENSION SFNAM(MNSBZ),HC(MNSBZ),TIS(MNSBZ)

      CHARACTER*12 SFNAM
      CHARACTER*2 oneway(MNSBZ)

      ifl=ICFMON(ICFD)

C This subroutine calculates the convection coefficient for each surface
C based on the CFD-predicted temperature and flow fields. TAIR is the referernce
C temperature used in calculating the coefficients, and is passed to this
C subroutine. TAIR is either equal to ESP-r's prediction for the future air-point
C temperature (for surface conflation) or is equal to the CFD-averaged temperatures
C (for integrated conflation). The results are written to the CFD monitoring file.

C Initialise character array indicating presence of 1-way surface conflation.
      DO 10 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        oneway(IS) = '  '
   10 CONTINUE

C Get the temperature of each zone surface. Set flag indicating presence of 1-way
C surface conflation.
      DO 20 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        TIS(IS)=SSB(L,ICFD)
        if(ITCtype(L,ICFD).ge.1 .and. ITCtype(L,ICFD).le.8)
     &                                    oneway(IS)='**'
   20 CONTINUE

C Write header line to monitoring file.
      write(ifl,*)
      write(ifl,*) 'CFD predicted surface convection and coefficients'
      write(ifl,*)

C Examine each zone surface in turn. Calculate driving potential for surface
C convection (Tsurf - Tair) then normalise the heat transfer by the driving
C potential and the surface area. Write results to monitoring file.
      DO 30 IS=1,NCNST
        DELT = ABS(TAIR-TIS(IS))
        if(DELT.GT.SMALL.AND.ATCFD(IS).GT.SMALL)then
          HC(IS) = ABS(CONVF(ICFD,IS)) / DELT / ATCFD(IS)
        endif
        QTS = -CONVF(ICFD,IS)
        write(ifl,1010) SFNAM(IS),QTS,HC(IS),TIS(IS),TAIR,oneway(IS)
 1010   FORMAT(A12,' Qwall->air(W) = ',f11.1,' HC-CFD = ',f7.2,
     &         ' Tsurf = ',f7.2,' Tair = ',f7.2,2x,a2)
   30 CONTINUE

      write(ifl,*)
      write(ifl,*) 'If 1-way surface conflation (indicated by **) is',
     &             'in use, the heat transfer results given here are'
      write(ifl,*) 'for information purposes only; they will not be ',
     &             'used by BSim.'

      RETURN
      END
