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
C  BS2CFDSB - Maps BSim thermal conditions onto CFD solid boundaries.
C  TREFBC - Calculate the reference temperature for the surface convection.
C  BS2CFDBLK - Maps BSim thermal conditions onto CFD blockages.

C ********************* BS2CFDSB *********************
C BS2CFDSB - Maps BSim thermal conditions onto CFD solid boundaries. Only used
C            when there is thermal conflation.

      SUBROUTINE BS2CFDSB
#include "cfd.h"
#include "building.h"

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ACCrec/IACC
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      CHARACTER*72 LCFD

C This subroutine imposes thermal boundary conditions (BCs) on the CFD domain
C when BSim and CFD are thermally conflated, mapping BSim thermal conditions
C to the CFD solid boundaries. The data that are mapped depend upon the thermal
C conflation `type'. There are 14 `types' in total:
C    1  `One-way surface' conflation; k-epsilon model with log-law wall
C       functions; Qsurf calculated by CFD. Tsurf passed from BSim.
C    2  `One-way surface' conflation; MIT 0-eqn model; Qsurf calculated by CFD.
C       Tsurf passed from BSim.
C    3  `One-way surface' conflation; k-epsilon model with Yuan wall
C       functions; Qsurf calculated by CFD. Tsurf passed from BSim.
C    4  `One-way surface' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tref); Tref=TFA (ie. from BSim).
C       Tsurf and HC passed from BSim.
C    5  `One-way surface' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tref); Tref=Tcfd (domain averaged).
C       Tsurf and HC passed from BSim.
C    6  `One-way surface' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tref); Tref=TFA (ie. from BSim); temperature of
C       next-to-wall points fixed using log-law wall functions.
C       Tsurf and HC passed from BSim.
C    7  `One-way surface' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tref); Tref=Tcfd (domain averaged); temperature
C       of next-to-wall points fixed using log-law wall functions.
C       Tsurf and HC passed from BSim.
C    8  `One-way surface' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tp); Tp is temperature of next-to-wall grid points;
C       HC treated as local value. Tsurf and HC passed from BSim.
C    9  `Two-way surface' conflation; k-epsilon model with log-law wall
C       functions; Qsurf calculated by CFD. Tsurf passed from BSim.
C   10  `Two-way surface' conflation; MIT 0-eqn model; Qsurf calculated by CFD.
C       Tsurf passed from BSim.
C   11  `Two-way surface' conflation; k-epsilon model with Yuan wall
C       functions; Qsurf calculated by CFD. Tsurf passed from BSim.
C   12  `Two-way surface' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tp); Tp is temperature of next-to-wall grid points;
C       HC treated as local value. Tsurf and HC passed from BSim.
C   13  `Two-way integrated' conflation; k-epsilon model with log-law wall
C       functions; Qsurf calculated by CFD. Tsurf passed from BSim.
C   14  `Two-way integrated' conflation; k-epsilon model;
C       Qsurf=A*HC*(Tsurf-Tp); Tp is temperature of next-to-wall grid points;
C       HC treated as local value. Tsurf and HC passed from BSim.

C Variable definitions:
C    NSB : the number of solid boundaries on the CFD domain.
C    ICP : the zone number in ESP-r's standard data model.
C    ISUFLC : the surface number in ESP-r's standard data model.
C    SSB : the temperature to impose on the CFD boundary.
C    SSBHC : the convection coefficient to impose on the CFD boundary.
C    ITCtype : thermal conflation `type'.
C    TFS : BSim's `future' surface temperature.
C    TPS : BSim's `present' surface temperature.
C    HCIF : BSim's `future' surface convection coefficient.

C Treat each CFD solid boundary in turn, mapping BSim thermal conditions to the
C CFD boundary according to the BC `type'.
C Record ACC actions (begin).
        IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
          write(IACC,*)
          write(IACC,*) 'Handshaking from thermal zone (',ICP,
     &                  ') to CFD domain (',ICFD,').'
        ENDIF
C Record ACC actions (end).
      DO 20 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
C Record ACC actions (begin).
        IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
          write(IACC,*) '  At CFD surf ',L,' ITCtype = ',ITCtype(L,ICFD)
          write(IACC,*) '    and IWSB = ',IWSB(L,ICFD)
          write(IACC,*) '    Tsurf = ',0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          write(IACC,*) '    Tf Tp = ',TFS(ICP,IS),TPS(ICP,IS)
          write(IACC,*) '    IS = ',IS
          write(IACC,*) '    HC = ',HCIF(ICP,IS)
        ENDIF
C Record ACC actions (end).
        if(ITCtype(L,ICFD).eq.1)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.2)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.3)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.4)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        elseif(ITCtype(L,ICFD).eq.5)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        elseif(ITCtype(L,ICFD).eq.6)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        elseif(ITCtype(L,ICFD).eq.7)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        elseif(ITCtype(L,ICFD).eq.8)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        elseif(ITCtype(L,ICFD).eq.9)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.10)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.11)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.12)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        elseif(ITCtype(L,ICFD).eq.13)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif(ITCtype(L,ICFD).eq.14)then
          SSB(L,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          SSBHC(L,ICFD)=HCIF(ICP,IS)
        else
          call edisp(iuout,' Error: invalid BC type')
        endif
   20 CONTINUE

      RETURN
      END


C ********************* TREFBC *********************
C TREFBC - Calculate the reference temperature for the surface convection.

      SUBROUTINE TREFBC(L,TREF)
#include "cfd.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
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
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/ICFNOD/ICFD,ICP
      COMMON/FVALA/TFA(MCOM),qfa(MCOM)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)

C This subroutine calculates the reference temperature (TREF) to use in the
C calculation of surface convection for thermal conflation ITCtypes 4, 5,
C 6, and 7. That is, when BSim passes CFD the surface temperature and the
C convection coefficient, and surface convection is calculated by:
C   Q = A*HC*(Twall - Tref)
C                     ^^^^
C TREF is the temperature of the room air to use in the boundary condition.

C There are two possibilities for calculating TREF (indicated by icTREF):
C   1 : For ITCtypes 4 and 6, TREF is taken as ESP-r's `future' air-point temp.
C   2 : For ITCtypes 5 and 7, TREF is calculated using the CFD temperatures from
C       the previous iteration; in this case, it is averaged over the CFD domain
C       since ESP-r's convection model assumes the room air to be well mixed.


      IF( icTREF(L,ICFD).eq.1 ) THEN

C Return `future' air-point temperature for zone.
        TREF = TFA(ICFD)
      ELSEIF (icTREF(L,ICFD).eq.2) THEN

C Average temperature over CFD domain.
C Initialize variables.
        rmass=0.
        total=0.

C Iterate through each cell of the domain. This must be modified once
C blockages have been implemented to exclude the blocked regions.
        DO 10 I=2,NIM1
          DO 10 J=2,NJM1
            DO 10 K=2,NKM1
              rmass=rmass+DENf(I,J,K)*VolP(I,J,K)
              total=total+Tf(I,J,K)*DENf(I,J,K)*VolP(I,J,K)
  10    CONTINUE

C Return the mass-averaged temperature to the calling routine.
        TREF = total/rmass
      ELSE
        call edisp(iuout,' Error: invalid method for calculating TREF.')
      ENDIF

      RETURN
      END


C ********************* BS2CFDBLK *********************
C BS2CFDBLK - Maps BSim thermal conditions onto CFD blockages. Only used
C             when there is thermal conflation.

      SUBROUTINE BS2CFDBLK
#include "cfd.h"
#include "building.h"

      COMMON/FVALA/TFA(MCOM),qfa(MCOM)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/ICFNOD/ICFD,ICP

      CHARACTER*72 HSID
      LOGICAL BHS

C This subroutine imposes BSim thermal conditions onto `blocked' areas
C of the CFD domain. Currently this is only used when a blocked heat source
C of type `APT' (short for `air-point') is prescribed. Functionality of this
C subroutine can be expanded in the future to map the temperatures and heat
C flows of ESP-r surfaces onto the CFD domain when the treatment of blockages
C is refined.

      M=1
      DO 30 L=1,NHS(M,ICFD)
        IF(HSID(L,M,ICFD).EQ.'APT'.OR.HSID(L,M,ICFD).EQ.'apt')then
          SHS(L,M,ICFD)=TFA(ICP)
        endif
   30 CONTINUE

      RETURN
      END
