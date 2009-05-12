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
C  CFAREA - Calculates the area of ESP-r surfaces covered by CFD solid boundaries.
C  CFCONV - Calculate convection coefficients based on the CFD predictions.

C ********************* CFAREA *********************
C CFAREA - Calculates the area of ESP-r surfaces covered by CFD solid boundaries.
      SUBROUTINE CFAREA(NCNST)
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/CFDARE/ATCFD(MNSBZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
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


C Initialize all areas to zero.
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
#include "cfd.h"
#include "building.h"


      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB
      COMMON/CFDARE/ATCFD(MNSBZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CONST/GREAT,small,GRAV
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN

      COMMON/MFS/IMFACT,NOSTOP,MITMFS

      DIMENSION SFNAM(MNSBZ),HC(MNSBZ),TIS(MNSBZ)

      CHARACTER*12 SFNAM
      CHARACTER*72 LCFD
      CHARACTER*2 oneway(MNSBZ)

      ifl=ICFMON

C This subroutine calculates the convection coefficient for each surface
C based on the CFD-predicted temperature and flow fields. TAIR is the referernce
C temperature used in calculating the coefficients, and is passed to this
C subroutine. TAIR is either equal to ESP-r's prediction for the future air-point
C temperature (for surface conflation) or is equal to the CFD-averaged temperatures
C (for integrated conflation). The results are written to the CFD monitoring file.

C Initialize the character array indicating presence of 1-way surface conflation.
      DO 10 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        oneway(IS) = '  '
   10 CONTINUE

C Get the temperature of each ESP-r surface. Set flag indicating presence of 1-way
C surface conflation.
      DO 20 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        TIS(IS)=SSB(L,ICFD)
        if(ITCtype(L,ICFD).ge.1 .and. ITCtype(L,ICFD).le.8)
     &                                                 oneway(IS)='**'
   20 CONTINUE

C Write header line to monitoring file.
      if (NOSTOP.eq.0) then
        write(ifl,*)
      write(ifl,*)'CFD-predicted Surface Convection and Coefficients:'
        write(ifl,*)
      endif

C Examine each ESP-r surface in turn. Calculate driving potential for surface
C convection (Tsurf - Tair) then normalize the heat transfer by the driving
C potential and the surface area. Write results to monitoring file.
      DO 30 IS=1,NCNST
        DELT = ABS(TAIR-TIS(IS))
        if(DELT.GT.SMALL.AND.ATCFD(IS).GT.SMALL)then
          HC(IS) = ABS(CONVF(ICFD,IS)) / DELT / ATCFD(IS)
        endif
        QTS = -CONVF(ICFD,IS)
        if (NOSTOP.eq.0) then
           write(ifl,1010) SFNAM(IS),QTS,HC(IS),TIS(IS),TAIR,oneway(IS)
        endif
   30 CONTINUE

      if (NOSTOP.eq.0) then
       write(ifl,*)
       write(ifl,*)'If 1-way surface conflation (indicated by **)',
     &       ' is in use the heat transfer results given here are'
       write(ifl,*)'for information purposes only; they will not',
     &             ' be used by BSim.'
      else
C Transmit the above to xml output for multi-timestep cfd-bsim runs with NOSTOP.ne.0
       call ConflationData_to_h3k(ICFD,SFNAM,HC,NCNST)
      endif

      RETURN
C Format statements.
 1010   FORMAT(A12,' Qwall->air(W) = ',f11.1,' HC-CFD = ',f7.2,
     &         ' Tsurf = ',f7.2,' Tair = ',f7.2,2x,a2)
      END


C ***************** ConflationData_to_h3k *****************
C This routine transfers conflation data to the H3K reporting
C facilities. Created by Achim Geissler, May 9 2009.

      subroutine ConflationData_to_h3k(iZone,SFNAM,HC,NCNST)
      implicit none
#include "cfd.h"
#include "building.h"

C Parameters.
      integer iZone,NCNST
	  

C Commons.
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      real CONVF

      dimension SFNAM(MNSBZ),HC(MNSBZ)

      CHARACTER*12 SFNAM
      real HC

C Local variables.
      character*2 cZone_Chars
      integer IS

C References.
      integer lnblnk

C Pad index to 'XX'.
      if(iZone.gt.9)then
         write(cZone_Chars,'(i2)')iZone
      else
         write(cZone_Chars,'(a,i1)') '0',iZone
      endif

C Output cfd surface heat transfer coefficients
C and cfd surface - air heat flux.
      do IS=1,NCNST
         call add_to_xml_reporting(
     &     HC(IS),
     &     'cfd/'//'z'// cZone_Chars //
     &     '/' // SFNAM(IS)(1:lnblnk(SFNAM(IS))) //'/HC-CFD',
     &     'units',
     &     '(W/(m2 K)',
     &     'CFD surface heat transfer coefficient')
	 
         call add_to_xml_reporting(
     &     -CONVF(iZone,IS),
     &     'cfd/'//'z' // cZone_Chars //
     &     '/' // SFNAM(IS)(1:lnblnk(SFNAM(IS))) // '/Qwall->air',
     &     'units',
     &     '(W)',
     &     'Heat flux from wall to air, Qwall->air')
      enddo

      return
      end
