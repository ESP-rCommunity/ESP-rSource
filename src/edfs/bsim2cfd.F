C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains.
C  BS2CFDSB  - Maps BSim thermal conditions to CFD solid boundaries.
C  TREFBC    - Calculates the reference temperature for surface convection.
C  BS2CFDBLK - Maps BSim thermal conditions to CFD blockages.


C ********************* BS2CFDSB *********************
C Imposes thermal boundary conditions (BCs) on the CFD domain
C when BSim and CFD are thermally conflated, mapping BSim thermal conditions
C to the CFD solid boundaries. The data mapped depend on the thermal
C conflation type as follows.
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

      SUBROUTINE BS2CFDSB
#include "building.h"
#include "cfd.h"

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ACCrec/IACC(MNZ)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      CHARACTER*72 LCFD

C Treat each CFD solid boundary in turn, mapping BSim thermal conditions to the
C CFD boundary according to the BC `type'.
C Record ACC actions (begin).
        IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
          write(IACC(icfd),*)
          write(IACC(icfd),*) 'Handshaking from thermal zone (',ICP,
     &                  ') to CFD domain (',ICFD,').'
        ENDIF
C Record ACC actions (end).
      DO 20 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
C Record ACC actions (begin).
        IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
          write(IACC(icfd),*) 
     &      '  At CFD surf ',L,' ITCtype = ',ITCtype(L,ICFD)
          write(IACC(icfd),*) '    and IWSB = ',IWSB(L,ICFD)
          write(IACC(icfd),*) 
     &      '    Tsurf = ',0.5*(TFS(ICP,IS)+TPS(ICP,IS))
          write(IACC(icfd),*) '    Tf Tp = ',TFS(ICP,IS),TPS(ICP,IS)
          write(IACC(icfd),*) '    IS = ',IS
          write(IACC(icfd),*) '    HC = ',HCIF(ICP,IS)
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
        elseif(ITCtype(L,ICFD).eq.0)then
          call edisp(iuout,' Error: zero BC type')
        else
          call edisp(iuout,' Error: invalid BC type')
        endif
        VOLTemp(IVOLNSB(L,ICFD),ICFD)=SSB(L,ICFD)
   20 CONTINUE

      RETURN
      END


C ********************* TREFBC *********************
C This subroutine calculates the reference temperature (TREF) to use in the
C calculation of surface convection for thermal conflation ITCtypes 4, 5,
C 6, and 7. That is, when BSim passes CFD the surface temperature and the
C convection coefficient, and surface convection is calculated by:
C   Q = A*HC*(Twall - Tref)
C                     ^^^^
C TREF is the temperature of the room air to use in the boundary condition.

C There are two possibilities for calculating TREF (indicated by icTREF):
C   1.  For ITCtypes 4 and 6, TREF is taken as ESP-r's `future' air-point temp.
C   2.  For ITCtypes 5 and 7, TREF is calculated using the CFD temperatures from
C       the previous iteration; in this case, it is averaged over the CFD domain
C       since ESP-r's convection model assumes the room air to be well mixed.

      SUBROUTINE TREFBC(L,TREF)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
C      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
C     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
C     2            DZHP(ntcelz),DZPL(ntcelz),
C     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
C     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
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
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)

C Return 'future' air-point temperature for zone.
      IF( icTREF(L,ICFD).eq.1 ) THEN
        TREF = TFA(ICP)

C Average temperature over CFD domain.
      ELSEIF (icTREF(L,ICFD).eq.2) THEN
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
        call edisp(iuout,'Error: invalid method for calculating TREF.')
      ENDIF

      RETURN
      END


C ********************* BS2CFDBLK *********************
C Maps BSim thermal conditions onto CFD blockages. Only used
C when there is thermal conflation.

      SUBROUTINE BS2CFDBLK
#include "building.h"
#include "cfd.h"

      COMMON/FVALA/TFA(MCOM),qfa(MCOM)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)

      CHARACTER*72 HSID
      LOGICAL BHS

C This section used for experimental radiator handling.
      M=1
      DO 30 L=1,NHS(M,ICFD)
        IF(HSID(L,M,ICFD).EQ.'APT'.OR.HSID(L,M,ICFD).EQ.'apt')then
          SHS(L,M,ICFD)=TFA(ICP)
        endif
   30 CONTINUE

C This section maps surface temperatures onto blockages.
      do IBLK=1,NBLK(ICFD)
        IV=INBLK(IBLK,ICFD)
        if (IVTYPE(IV,ICFD).eq.32) then
          IS=IDSURF(VCsurf(IV,ICFD))
          VOLTemp(IV,ICFD)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
        elseif (IVTYPE(IV,ICFD).eq.33) then
          do i=1,6
            if (BLKSURF(IV,ICFD,i)(1:4).ne.'NONE') then
              IS=IDSURF(BLKSURF(IV,ICFD,i))
              BLKTEMP(IV,i)=0.5*(TFS(ICP,IS)+TPS(ICP,IS))
            endif
          enddo
        endif
      enddo
      
      RETURN
      END
