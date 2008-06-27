C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file contains routines used to collate and transport data from bps
C to the H3K reporting facilities. Included:
C
C h3k_transport_build_data:     Transports building domain data
C
C h3k_transport_climate__data:  Transports climate domain data
C
C h3k_report_time:              Transports simulation time parameters
C
C h3k_transport_plant_data:     Transports plant domain data
C
C h3k_connection_property:      Function returning data for plant connections.
C
C h3k_transport_mfn_data:     Transports flow network data
C
C *********************************************************************************************
C
C ******************************* H3K Init Set Meta *******************************************
C
C Created by: Alex Ferguson
C Created on: March 28, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This subroutine sets flags determining if H3K reporting routines should initialize meta
C varaibles
C
C**********************************************************************************************
      subroutine H3K_init_set_meta(bFlag_Value)
      implicit none

      COMMON / H3KmetaInitVal /
     &     bBuildMeta,
     &     bClimMeta,
     &     bTimeMeta,
     &     bPlantMeta,
     &     bElectMeta

      logical  bBuildMeta,bClimMeta,bTimeMeta,bPlantMeta,bElectMeta

      logical bFlag_Value

C----------------------------------------------------------------------------------------------
C     Set values:
C----------------------------------------------------------------------------------------------
      bBuildMeta =  bFlag_Value
      bClimMeta  =  bFlag_Value
      bTimeMeta  =  bFlag_Value
      bPlantMeta =  bFlag_Value
      bElectMeta =  bFlag_Value
      return
      end


C ******************************* H3K Transport Building Data *********************************
C
C Created by: Alex Ferguson
C Created on: March 28, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a very simple routine used to transport miscellanoues building data to the H3K
C reporting facilities. It duplicates some of the functionality found in CETC's H3Kstore
C routine and ESRU's MZSL3 routine, but this reduncancy is necessary to permit the this
C code to be called independently of these routines.
C
C**********************************************************************************************

      SUBROUTINE H3K_transport_build_data(iZone)
      IMPLICIT NONE
#include "building.h"

C.....Passed variable
      integer iZone            ! Zone number

C.....Common variables
      common/c20/nzsur(mcom),nztv(mcom)
      integer nzsur           ! # of surfaces in a zone
      integer nztv            ! # of verticies in a zone (not used)

      common/prec9/nConst(mcom),nELts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      integer nConst          ! # of surfaces per zone
      integer nELts           ! # number of layers per surface (who knew?)
      integer ngaps           ! # number of air gaps witin surface (not used)
      integer npgap           ! Location of air gap. (not used)


C TPA(MCOM),           ! - present zone air point temperature (oC)
C QPA(MCOM)            ! - zone air point heat injection (W)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      REAL TPA, QPA

C TFA(MCOM)            ! - future zone air point temperature (oC)
C QFA(MCOM)            ! - zone air point heat injection (W)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      REAL TFA, QFA

C TPC(MCOM,MS,MN),     ! - present construction temperature (oC)
C QPC(MCOM)            ! - construction heat injection (W)
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      REAL TPC, QPC

C TFC(MCOM,MS,MN),     ! - future construction temperature (oC)
C QFC(MCOM)            ! - construction heat injection (W)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      REAL TFC, QFC

C.....Gridding common 
      common/gr1d01/nnds,nndz(mcom),nndc(mcom,ms),nndl(mcom,ms,me)      
      integer nnds            ! ? (not used)
      integer nndz            ! ? (not used)
      integer nndc            ! # number of nodes within a construction
      integer nndl            ! # of nodes per construction layer

C TPS(MCOM,MS),        ! - present surface temperature (oC)
C QPS(MCOM)            ! - surface heat injection (W)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      REAL TPS, QPS

C TFS(MCOM,MS),        ! - future surface temperature (oC)
C QFS(MCOM)            ! - surface heat injection (W)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      REAL TFS, QFS

      COMMON/SETUQ/           ! PLANT heat injection/extration:
     &     QPLTP(MCOM),         !  - present (W)
     &     QPLTF(MCOM),         !  - future (W)
     &     CONV(MCOM)           !  - convective ratio
      real qpltp, qpltf, conv

      COMMON/CasDat/            ! Building casual gains
     &     Cas_Rad_dat(MCOM),   !  - Radiant (W)
     &     Cas_Con_dat(MCOM),   !  - Convective (W)
     &     Cas_Lat_dat(MCOM)    !  - Latent (W)
      real Cas_Rad_dat, Cas_Con_dat, Cas_Lat_dat

      common/solsum/            ! SOLAR GAINS:
     &     q1adjz(2),           ! - from adjacent zones (W)
     &     q1outs(2),           ! - from exterior (W)
     &     q2adjz(2),           ! - lost to adjacent zones (W)
     &     q2lost(2),           ! - lost to exterior (W)
     &     q2tmc(2),            ! - Absorbed by glazing (W)
     &     q2wall(2),           ! - Absorbed by opaque construction (W)
     &     q2rem(2)             ! - Absorbed by roof? (W)
      REAL q1adjz, q1outs, q2adjz, q2lost, q2tmc, q2wall,q2rem

      common/cctl/icascf(mcom)  ! Zone control type
      integer icascf

      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &     ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),                         ! not used
     &     ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &     bmiscd(mcf,mbcdt,mbcdp,misc)
      integer ncf, ibsn, iban, idcdtl, ibcdv, nbcdp, ibctyp, ibclaw
      integer nbcdt
      real tbcps, bmiscd

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)
      integer IE
      real ATP,ATF,ARP,ARF

      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      real HCIP,HCIF,HCOP,HCOF ! convective heat transfer coefficients "present" and "future"
                                            ! for "inside" and "outside" surface, each
C=== 26.06.08
      COMMON/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)
      integer ICORI,ICORE ! Transfer coefficient index for internal and external face of MLC

C===

      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      integer IVF,ISI,IHC,ITW,ICGC,IOBS ! indices for "is there"

      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),
     &              AE(MCOM,MS)
      real EI,EE,AI,AE ! internal and external emissivity, areas (?)

      COMMON/V2/CFB(MCOM,MST,MST)
      real CFB ! black body viewfactors

      COMMON/ELWE1/A1(MCOM,MS),B1(MCOM,MS),C1(MCOM,MS)
      real A1,B1,C1 ! view factors of building, sky and ground of external surfaces

      COMMON/COE31S/HRS(MCOM,MS,MS),ZHRS(MCOM,MS)
      real HRS,ZHRS

C.....Sky temperature and cloud cover; ground temperature, 
      COMMON/ELWE3/GTP(6),GTF(6),QGLP,QGLF,QGGP,QGGF,GC(6,4),
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST

      REAL GTP,GTF,QGLP,QGLF,QGGP,QGGF,GC
      REAL TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH
      REAL TBSUTH,TBEAST,TBWEST

C.....Local variables
      REAL TMA                  ! Mean air point temperature (oC)
      real trsum
      REAL Q_zone,Q_heat,Q_cool ! Total heat injection / extraction
      INTEGER i_ZnCtl           ! Zone control flag

      INTEGER iSurface, iNode,iViewSurface  ! Counters

      real SBC

C.....DECLARATIONS for H3Kreporting object
      CHARACTER*128 H3K_rep_NAME
      CHARACTER*24  H3K_format
      CHARACTER*24  H3K_format_HC                                      ! 26.06.08
      CHARACTER*2   cZone_Chars, cSurf_Chars, cNode_Chars

C lnblnk is a function returning the index of last non-blank character in a string.
      INTEGER LNBLNK 

C.....Timerows (named constants)
      integer iPresent, iFuture
      parameter (iPresent=1, iFuture=2) 

C.....Commons indicating if meta items have been initialized
      COMMON / H3KmetaInitVal /
     &     bBuildMeta,
     &     bClimMeta,
     &     bTimeMeta,
     &     bPlantMeta,
     &     bElectMeta

      logical  bBuildMeta,bClimMeta,bTimeMeta,bPlantMeta,bElectMeta

C----------------------------------------------------------------------     
C     References
C----------------------------------------------------------------------
      real fSurface_radiant_gain   ! Function returining the
                                   ! radiant gain on a surface (W)
C----------------------------------------------------------------------
C.....Determine control function for current zone.
C.....The following code should determine the zone heat injection
C.....For each zone regardless of the mechanism used to inject the
C.....heat. 
C----------------------------------------------------------------------
      SBC=56.7E-9 ! Stefan-Boltzmann constant

      i_ZnCtl=icascf(iZone)

      if(i_ZnCtl.ne.0)then

C If i_ZnCtl is non-zero then iban will be testable.
C Determine where heat is injected/extracted
        if ( iban(i_ZnCtl,2) .gt. 0 .and. iban(i_ZnCtl,3) .eq. 0 ) then

c PIN is at surface.
C         print*, iZone, 'surface'
          Q_zone = QFS(iZone)
        elseif ( iban(i_ZnCtl,3) .gt. 0 ) then

C PIN is within construction
C         print*, iZone, 'constr'
          Q_zone = QFC(iZone)
        elseif ( iban(i_ZnCtl,1) .eq. -1 ) then

C PIN is in a plant component node - not implemented yet.

        elseif ( iban(i_ZnCtl,1) .eq. -2 ) then
C         print*, iZone, 'mix'
C PIN is mix of radiative/convective flux (ie - plant component)
         Q_zone = QPLTF(iZone)
        else

C PIN is air point
C         print*, iZone, 'air point'
          Q_zone = QFA(iZone)
        endif
      else

C A zone with no control 
C       print*, iZone, 'no control'
        Q_zone = 0.0
      endif


C.....The following code is used to transport data to
C.....the H3K reporting object
C.....
C.....Any changes to this code should be made within
C.....the H3Kreports demarcaration comments
C.....Set format depending on number of zones
      if ( iZone .gt. 9 ) then
         cZone_Chars = 'I2'
      else
         cZone_Chars = 'I1'
      endif
      write (H3K_format, '(A,A,A)') '(A,', cZone_Chars(1:2), ',A)'

C.....AIR POINT TEMPERATURE
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/air_point/temperature'
      call add_to_xml_reporting (
     &         TFA(iZone),
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Zone air-point temperature' )


C.....ZONE HEAT INJECTION / EXTRACTION
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/heat_injection'
      call add_to_xml_reporting (
     &         Q_ZONE,
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone heat injection (obsolete debugging variable)' )


C.....ZONE HEAT INJECTION / EXTRACTION
C.....-> set name

      if ( Q_ZONE .GT. 0) then

         Q_Heat = Q_ZONE
         Q_Cool = 0.

      else

         Q_Heat = 0.
         Q_Cool = ABS(Q_ZONE)

      endif
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/flux/net_flux'
      call add_to_xml_reporting (
     &         Q_ZONE,
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone net heat injection/extraction ' )


      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/flux/heat_injection'
      call add_to_xml_reporting (
     &         Q_Heat,
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone net heat injection ' )

      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/flux/heat_extraction'
      call add_to_xml_reporting (
     &         Q_cool,
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone net heat extraction ' )


C.....ZONE SOLAR GAINS: net gains from adjacent zones
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/solar_gains/net_from_adjacent_zones'
      call add_to_xml_reporting (
     &         q1adjz(2)-q2adjz(2),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Net solar gains from adjacent zones' )

C.....ZONE SOLAR GAINS: net gains from ambient
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/solar_gains/net_from_ambient'
      call add_to_xml_reporting (
     &         q1outs(2)-q2lost(2),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Net solar gains from ambient' )


C.....ZONE SOLAR GAINS: absorbed by glazing
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/solar_gains/absorbed_glazing'

      call add_to_xml_reporting (
     &         q2tmc(2),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Net solar gains absorbed by transparent surfaces' )

C.....ZONE SOLAR GAINS: absorbed by opaque construction
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/solar_gains/absorbed_opaque_construction'

      call add_to_xml_reporting (
     &         q2wall(2)+q2rem(2),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Net solar gains transmitted by glazing and absorbed'
     &         // ' on interior surfaces' )

C.....ZONE SOLAR GAINS: Net total absorbed
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/solar_gains/total_net_absorbed'
      call add_to_xml_reporting (
     &         q2tmc(2)+q2rem(2)+q2wall(2),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Total net absorbed solar gains' )


C.....ZONE SOLAR GAINS: Net total gain
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &     '/solar_gains/total_net_gain'
      call add_to_xml_reporting (
     &         q1outs(2)+q1adjz(2)-(q2adjz(2)+q2lost(2)),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Total net solar gains' )


C.......H3Kreports.(CASUAL_GAINS).start......................
C.......The following code is used to trasnport data to
C.......the H3K reporting object
C.......
C.......Any changes to this code should be made within
C.......the H3Kreports demarcaration comments


C.......CONVECTIVE CASUAL GAINS
C.......-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &       '/casual_gains/convective'
      call add_to_xml_reporting (
     &         Cas_con_dat(iZone),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone convective casual gains' )


C.......RADIANT CASUAL GAINS
C.......-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &       '/casual_gains/radiant'
      call add_to_xml_reporting (
     &         Cas_rad_dat(iZone),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone radatiant casual gains' )


C.......LATENT CASUAL GAINS
C.......-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &       '/casual_gains/latent'
      call add_to_xml_reporting (
     &         Cas_lat_dat(iZone),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone latient casual gains' )


C.......TOTAL CASUAL GAINS
C.......-> set name
        WRITE(H3K_rep_NAME,H3K_FORMAT)'building/zone_',iZone,
     &       '/casual_gains/total'
      call add_to_xml_reporting (
     &         Cas_lat_dat(iZone)
     &           +  Cas_rad_dat(iZone)
     &           +  Cas_con_dat(iZone),
     &         H3K_rep_NAME,
     &         'units', '(W)',
     &         'Zone total casual gains '
     &             // '(convective + radiant + latent)' )


C.......H3Kreports.(CASUAL_GAINS).end........................

C---------------------------------------------------------------------
C     Loop through zone surfaces and MLC layers, reporting data 
C---------------------------------------------------------------------
      do iSurface = 1, nzsur(iZone)

         if ( iSurface .gt. 9 ) then
            cSurf_Chars = 'I2'
         else
            cSurf_Chars = 'I1'
         endif
         write (H3K_format, '(A,A,A,A,A)')
     &        '(A,',cZone_Chars(1:2),',A,',cSurf_Chars(1:2),',A)'
C=== 26.06.08
         write (H3K_format_HC, '(A,A,A,A,A,A,A)')
     &       '(A,',cZone_Chars(1:2),',A,',cSurf_Chars(1:2),',A,I2,A)'
C===

C........Surface temperature 
C........-> set name
         WRITE(H3K_rep_NAME,H3K_FORMAT)
     &      'building/zone_',iZone,
     &      '/surface_',iSurface,
     &      '/temperature'
C........->Report data
         call add_to_xml_reporting (
     &      tfs(iZone,iSurface),
     &      H3K_rep_NAME,
     &      'units', '(oC)',
     &      'Temperature on zone inside face' )

C........External facing surfaces -> Surface temperature
         if (IE(iZone,iSurface) .eq. 0) THEN ! the surface faces external
            WRITE(H3K_rep_NAME,H3K_FORMAT)
     &         'building/zone_',iZone,
     &         '/surface_',iSurface,
     &         '/ext. surf. temperature'
C........->Report data
            call add_to_xml_reporting (
     &         TFC(iZone,iSurface,1), ! is this field filled if ISAVE .ne. 3 ??
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Temperature on exterior face' )
         Endif ! if ie = 0

C........Radiant heat injection associated with plant containment.
         WRITE(H3K_rep_NAME,H3K_FORMAT)
     &      'building/zone_',iZone,
     &      '/surface_',iSurface,
     &      '/plant_containment_flux'

C........->Report data
         call add_to_xml_reporting (
     &      fSurface_radiant_gain(iZone,iSurface,iFuture),
     &      H3K_rep_NAME,
     &      'units', '(W)',
     &      'Plant containment radiant heat flux at surface' )

C.......CONVECTIVE COEFFICIENTS
C........Internal surface convective coefficient.
         WRITE(H3K_rep_NAME,H3K_format_HC)  ! 26.06.08
     &      'building/zone_',iZone,
     &      '/surface_',iSurface,
     &      '/HCi (icor=',ICORI(iZone,iSurface),')'  ! 26.06.08

C........->Report data
         call add_to_xml_reporting (
     &      HCIP(iZone,iSurface),
     &      H3K_rep_NAME,
     &      'units', '(W/(m2 K))',
     &      'Internal side convective coefficient' )

C........External surface convective coefficient.
         WRITE(H3K_rep_NAME,H3K_format_HC)  ! 26.06.08
     &      'building/zone_',iZone,
     &      '/surface_',iSurface,
     &      '/HCe (icor=',ICORE(iZone,iSurface),')' ! 26.06.08

C........->Report data
         call add_to_xml_reporting (
     &      HCOP(iZone,iSurface),
     &      H3K_rep_NAME,
     &      'units', '(W/(m2 K))',
     &      'External side convective coefficient' )

C......MEAN RADIANT TEMPERATURE OF SURROUNDINGS
C calc plane radiant temp. for each internal surface (w/o and w emissivity)
         if (IVF(iZone) .eq. 1) THEN ! there is a view factor file for this zone
            trsum = 0.0
C            wtrsum = 0.0
            do iViewSurface = 1,nzsur(iZone)
C tps(iZone,iSurface) present (i? e?) surface temp.
               trsum = trsum + CFB(iZone,iSurface,iViewSurface)*
     &                           (TFS(iZone,iViewSurface)+273.15)**4
C               wtrsum = wtrsum + EI(iZone,iViewSurface)*
C                              CFB(iZone,iSurface,iViewSurface)*
C     &                           (TFS(iZone,iViewSurface)+273.15)**4

            enddo 
            trsum = trsum**0.25 - 273.15
C            wtrsum = wtrsum**0.25 - 273.15
C........Plane radiant temperature 
C........-> set name
            WRITE(H3K_rep_NAME,H3K_FORMAT)
     &         'building/zone_',iZone,
     &         '/surface_',iSurface,
     &         '/PRT'
C........->Report data
            call add_to_xml_reporting (
     &         trsum,
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Plane radiant temperature on zone inside face' )

C........Emissivity weighted plane radiant temperature 
C........-> set name
C            WRITE(H3K_rep_NAME,H3K_FORMAT)
C     &         'building/zone_',iZone,
C     &         '/surface_',iSurface,
C     &         '/wPRT'
C........->Report data
C            call add_to_xml_reporting (
C     &         wtrsum,
C     &         H3K_rep_NAME,
C     &         'units', '(oC)',
C     &         'Emissivity weighted plane radiant temp. (inside face)' )

C.......RADIATION HEAT TRANSFER COEFFICIENT
C........-> set name
            WRITE(H3K_rep_NAME,H3K_FORMAT)
     &         'building/zone_',iZone,
     &         '/surface_',iSurface,
     &         '/HRi'
C........->Report data
            call add_to_xml_reporting (
C     &         ZHRS(iZone,iSurface)/(TFS(iZone,iSurface)-trsum), ! <= problems with DT approx. zero !
C          Incropera et. al., page 10:
     &        EI(iZone,iSurface)*SBC*
     &            (TFS(iZone,iSurface)+trsum+2*273.15)*
     &            ((TFS(iZone,iSurface)+273.15)**2+(trsum+273.15)**2),
     &         H3K_rep_NAME,
     &         'units', '(W/(m2 K))',
     &         'Radiant heat transfer coef. (inside face)' )

         ENDIF ! <- end if viewfactor file present

C calc mean surrounding temp of external surfaces
         if (IE(iZone,iSurface) .eq. 0) THEN ! the surfaces faces external
            trsum = B1(iZone,iSurface)*TSKY**4 +
     &                  C1(iZone,iSurface)*(GTF(1)+273.15)**4 +
     &                    A1(iZone,iSurface)*TBAVE**4
            trsum = trsum**0.25 - 273.15
C........-> set name
            WRITE(H3K_rep_NAME,H3K_FORMAT)
     &         'building/zone_',iZone,
     &         '/surface_',iSurface,
     &         '/AmbRT'
C........->Report data
            call add_to_xml_reporting (
     &         trsum,
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Ambient radiant temperature for exterior surface' )

         endif ! <- end if surface faces external

C......=====================================================
C......NODE DATA
         do iNode = 1, (nndc (iZone, iSurface )-1)
            if ( iNode .gt. 9 ) then
               cNode_Chars = 'I2'
            else
               cNode_Chars = 'I1'
            endif
            write (H3K_format, '(A,A,A,A,A,A,A)')
     &        '(A,',cZone_Chars(1:2),
     &        ',A,',cSurf_Chars(1:2),
     &        ',A,',cNode_Chars(1:2),',A)'

C...........MLC layer temperature 
C...........-> set name
            WRITE(H3K_rep_NAME,H3K_FORMAT)
     &         'building/zone_',iZone,
     &         '/surface_',iSurface,
     &         '/node_',iNode,
     &         '/temperature'
C...........->Report data     
            call add_to_xml_reporting (
     &         tfc(iZone,iSurface,iNode),
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Temperature at node within multilayer construction' )

         enddo  ! <- end iNode = 1,N of iSurface 1,N of iZone

      enddo ! <- end iSurface 1,N of iZone

      RETURN
      END



C
C ******************************* H3K Transport Climate Data *********************************
C
C Created by: Alex Ferguson
C Created on: May 13, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a very simple routine used to transport miscellanoues climate data to the H3K
C reporting facilities.
C
C**********************************************************************************************

      SUBROUTINE H3K_transport_climi_data()
      IMPLICIT NONE

C.....Climate data (present time row)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
C QFP Diffuse horizontal radiation - present time row
C QFF Diffuse horizontal radiation - future time row
C TP Ambient temperature - present time row
C TF Ambient temperature - future time row
C QDP Direct normal radiation - present time row
C QDF Direct normal radiation - future time row
C VP Wind velocity - present time row
C VF Wind velocity - future time row
C DP Wind direction - present time row
C DF Wind direction - future time row
C HP Relative humidity - present time row
C HF relative humidity - future time row
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
C.....Zone averaging
      common/avrage/idaver      ! Flag for timestep averaging
      integer idaver

C.....Sky temperature and cloud cover
      COMMON/ELWE3/GTP(6),GTF(6),QGLP,QGLF,QGGP,QGGF,GC(6,4),
     &TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH,
     &TBSUTH,TBEAST,TBWEST

      REAL GTP,GTF,QGLP,QGLF,QGGP,QGGF,GC
      REAL TSKY,CLOUDC,CLOUD1,CLOUD2,CLOUD3,TBAVE,TBROOF,TBNRTH
      REAL TBSUTH,TBEAST,TBWEST

C.....DECLARATIONS for H3Kreporting object
      CHARACTER*128 H3K_rep_NAME
      CHARACTER*12  H3K_format
      
      INTEGER LNBLNK

C----------------------------------------------------------------------     
C     References
C----------------------------------------------------------------------
      REAL HUMR      ! Function returning the humidity ratio
      REAL DEWPT    ! Function returning the saturation temperature

C.....The following code is used to transport data to
C.....the H3K reporting object.

C.....Set format
      H3K_FORMAT = '(A)'

C.....DIFFUSE HORIZONTAL RADIATION
C.....-> set name

      call add_to_xml_reporting (
     &         QFF,
     &         'climate/solar/diffuse_horizontal_radiation',
     &         'units', '(W/m2)',
     &         'Climate diffuse horizontal solar radiation' )


C.....DIRECT NORMAL RADIATION
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/solar/direct_normal_radiation'
      call add_to_xml_reporting (
     &         QDF,
     &         H3K_rep_NAME,
     &         'units', '(W/m2)',
     &         'Climate direct normal solar radiation' )

C.....AMBIENT TEMPERATURE
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/dry_bulb_temperature'
      call add_to_xml_reporting (
     &         TF,
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Climate dry-bulb temperature' )

C.....Relative humidity
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/relative_humidity'
      call add_to_xml_reporting (
     &         HF,
     &         H3K_rep_NAME,
     &         'units', '(%)',
     &         'Climate relative humidity' )


C.....WIND VELOCITY
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/wind/velocity'
      call add_to_xml_reporting (
     &         VF,
     &         H3K_rep_NAME,
     &         'units', '(m/s)',
     &         'Climate wind velocity' )

C.....WIND Direction
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/wind/direction'
      call add_to_xml_reporting (
     &         DF,
     &         H3K_rep_NAME,
     &         'units', '(degrees)',
     &         'Climate wind direction' )

C.....Cloud Cover
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/cloud_cover'
      call add_to_xml_reporting (
     &         CLOUDC,
     &         H3K_rep_NAME,
     &         'units', '(fraction)',
     &         'Climate cloud cover' )

C.....Sky temperature
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/sky_temperature'
      call add_to_xml_reporting (
     &         (TSKY-273.15),
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Climate sky temperature' )

C.....Sky temperature depression
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/sky_temperature_depression'
      call add_to_xml_reporting (
     &         (TF-(TSKY-273.15)),
     &         H3K_rep_NAME,
     &         'units', '(K)',
     &         'Climate sky temperature depression' )

C.....Ambient air saturation temperature
C.....-> set name
      WRITE(H3K_rep_NAME,H3K_FORMAT)
     &     'climate/ambient_air_tsat'
      call add_to_xml_reporting (
     &         DEWPT(HUMR(TF,HF,1013.),1013.),
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'ambient air saturation temperature' )


      RETURN
      END


C ******************************* H3K_Report_time *********************************
C
C Created by: Alex Ferguson
C Created on: May 13, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a simple function that dumps ESP-r's time data out to the
C XML reporting object
C
C inputs:
C   - Val_P:  present row value
C   - Val_F:  future row vaue
C
C   - idaver: common block variable indicating if time-step averaging should
C             be applied
C
C*********************************************************************************

      SUBROUTINE H3K_Report_time(
     &     btimep, btimef, ihrp, ihrf, idyp, idyf, its, ntstep, nsinc
     &     )
      IMPLICIT NONE

#include "building.h"
#include "MultiYear_simulations.h"
      
C.....Declare passed variables
      real     btimep, btimef   ! building present & future time (0.0->23.999..)
      integer  ihrp, ihrf       ! building present & future hour (1->24)
      integer  idyp, idyf       ! building present & future day  (1->365)
      integer  ntstep           ! # building timesteps/hour
      integer  its              ! index of timestep in current hour
      integer  nsinc            ! number of timesteps that have lapsed since
                                ! simulation start
C.....Local variables
      character*128 H3K_rep_NAME
      real    temp_day          ! Temporary vbls used to calculate day
      real    real_day
      integer idyp_adjust
      integer iCurrent_year, iStart_year
C.....References
      integer LNBLNK            ! function returning # of non-blank characters in
                                ! a string
      integer h3k_month         ! function returning index of current month when
                                ! passed the current day (1.0->365.999...)

                                
C.....Building simulation time (present)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/time/present'
C.....-> update meta-data
      call add_to_xml_reporting (
     &         btimep,
     &         H3K_rep_NAME,
     &         'units', '(hours)',
     &         'Time corresponding to current simulation time row' )

C.....Building simulation time (future)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/time/future'
      call add_to_xml_reporting (
     &         btimef,
     &         H3K_rep_NAME,
     &         'units', '(hours)',
     &         'Time corresponding to future simulation time row' )

C.....Building simulation hour (present)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/hour/present'
      call add_to_xml_reporting (
     &         real(ihrp),
     &         H3K_rep_NAME,
     &         'units', '(hours)',
     &         'Hour corresponding to current simulation hour' )

C.....Building simulation hour (future)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/hour/future'
      call add_to_xml_reporting (
     &         real(ihrf),
     &         H3K_rep_NAME,
     &         'units', '(hours)',
     &         'Hour corresponding to future simulation hour' )

C.....Building simulation day (present)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/day_number/present'
      call add_to_xml_reporting (
     &         real(idyp),
     &         H3K_rep_NAME,
     &         'units', '(days)',
     &         'Day # corresponding to current simulation day' )


C.....Building simulation day (future)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/day_number/future'
      call add_to_xml_reporting (
     &         real(idyf),
     &         H3K_rep_NAME,
     &         'units', '(day)',
     &         'Day # corresponding to future simulation day' )


C.....Simulation year for multi-year simulation
      if ( bMY_sim_enabled ) then
C........Building simulation year (present)
C........-> set name
         WRITE(H3K_rep_NAME,'(A)')'building/year/present'

         call add_to_xml_reporting (
     &         real(iMY_current_year),
     &         H3K_rep_NAME,
     &         'units', '(year)',
     &         'Year corresponding to current simulation day' )


C........Building simulation day (future)
C........-> set name
         WRITE(H3K_rep_NAME,'(A)')'building/year/future'
         call add_to_xml_reporting (
     &         real(iMY_future_year),
     &         H3K_rep_NAME,
     &         'units', '(year)',
     &         'Year corresponding to future simulation day' )


      endif 
     
     
C.....Building simulation day (present) ->real number

      if ( ihrp .eq. 24 .and. idyp .eq. 365
     &       .and. .not. bMY_sim_enabled) then
         idyp_adjust = idyp - 365
      else
         idyp_adjust = idyp
      endif
C.....Year numbers for multi-year simulations
      if ( bMY_sim_enabled ) then

         iCurrent_year = iMY_current_year
         iStart_year   = iMY_start_year

      else

         iCurrent_year = 0
         iStart_year   = 0

      endif 
C.....Calculate present 'real' day -> day number       
      real_day = real(idyp_adjust)
     &      +(real(ihrp)+real(its-1)/real(ntstep))/24.0
     &      + real( iCurrent_year - iStart_year) * 365.0

      temp_day = real(idyp_adjust)
     &         +(real(ihrp)+real(its-1)/real(ntstep))/24.0

   
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/day/present'
C.....-> update meta-data
      call add_to_xml_reporting (
     &         real_day,
     &         H3K_rep_NAME,
     &         'units', '(days)',
     &         'Day corresponding to current simulation time row' )

C.....Building simulation month
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/month'
      call add_to_xml_reporting (
     &         real(h3k_month(temp_day)),
     &         H3K_rep_NAME,
     &         'units', '(-)',
     &         'Month corresponding to current simulation time row ' )

C.....Building simulation day (future) ->real number


      real_day = real (idyp_adjust)
     &   +(real(ihrp)+real(its)/real(ntstep))/24.0
     &   + real( iCurrent_year - iStart_year )  * 365.0

      WRITE(H3K_rep_NAME,'(A)')'building/day/future'
      
      call add_to_xml_reporting (
     &         real_day,
     &         H3K_rep_NAME,
     &         'units', '(day)',
     &         'Day corresponding to future simulation time row' )

C.....Building simulation timestep
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'building/time_step'
      call add_to_xml_reporting (
     &         real(nsinc),
     &         H3K_rep_NAME,
     &         'units', '(-)',
     &         'Time step #' )

C.....H3Kreports.(time).end.................
      return
      end


C ******************************* H3K Transport Plant Data ************************************
C
C Created by: Alex Ferguson
C Created on: June 21, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a very simple routine used to transport plant domain state-variable data to the H3K
C reporting facilities. It duplicates some of the functionality found in CETC's H3Kstore
C routine and ESRU's PZSL3 routine, but this reduncancy is necessary to permit the this
C code to be called independently of these routines.
C
C**********************************************************************************************
      subroutine h3k_transport_plant_data()
      implicit none
#include "plant.h"
C.....ESP-r commons
      common/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)  ! Control data for each component
      integer NPCOMP, NCI
      real CDATA

      common/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR) ! ? (unused)
      integer NPCDAT, IPOFS1, IPOFS2

      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15, pcname

      common/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)   ! Plant present state variables
      real CSVF, CSVP

      common/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)    ! ? type and simulation modes supported by nodes
      integer NDCON, ISV

      common/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &  ICONDX(MPCOM,MNODEC,MPCONC) ! index of connections
      integer icondx, icontp
      real convar

C.....Local variables
      integer ii,jj,kk          ! counters
      integer iNode             ! pointer to current node
      character*128 char_temp   ! holding variable
      character*128 Base_Name   ! component base name
      character*128 Node_Name   ! node name
      character*128 Connect_name! connection name
      character*128 H3K_rep_NAME ! XML leaf name

      integer iState, iSim_Type ! node state and supported simulation type
                                ! (results from ISV_convert)
      integer iConn_numb        ! index of a given connection

      real temperature          ! air temp (oC), used in humidity calculation
      real air_flow             ! air flow (kg/s),  used in humidity calculation
      real moisture_flow        ! moisture flow (kg/s),  used in humidity calculation
      real humidity_ratio       ! humidity ratio (-)

      integer iLength           ! Length of string
      integer iLength_tmp       ! Temporary holder of length of string

C.....References
      integer LNBLNK            ! function returning # of non-blank characters in
                                ! a string

      real h3k_connect_property ! function returning the properties of a connection

C.....Named constants
      integer iProp_temp        ! named constant for temperature properity
      integer iProp_1st_flow    ! named constant for first phase flow properity
      integer iProp_2nd_flow    ! named constant for second phase flow properity
      integer iProp_h2_flow     ! named constant for hydrogen flow
      parameter ( iProp_temp     = 1,
     &            iProp_1st_flow = 2,
     &            iProp_2nd_flow = 3,
     &            iProp_h2_flow  = 4)

C.....Commons indicating if meta items have been initialized
      COMMON / H3KmetaInitVal /
     &     bBuildMeta,
     &     bClimMeta,
     &     bTimeMeta,
     &     bPlantMeta,
     &     bElectMeta

      logical  bBuildMeta,bClimMeta,bTimeMeta,bPlantMeta,bElectMeta

C.....Loop through all components in plant network
      do ii = 1, npcomp

C........Build base name
         char_temp = pcname(ii)
         iLength = lnblnk(char_temp)
         iLength_tmp = iLength
         write(Base_Name, '(A,A)')
     &        'plant/',char_temp(1:iLength)

C........Loop through nodes (NPCDAT(i,8)
         do jj = 1, NPCDAT(ii,8)

C...........Build node name
            iLength = iLength_tmp  ! Reset string length in case it was incremeted for 2-digit nodes.
            if(jj<10)then  ! First nine nodes.
              write(Node_name, '(A,A,I1)')
     &             Base_Name(1:iLength+6),
     &             '/node_',jj
            else           ! This presumes that no plant component will have more than 99 nodes! 
              write(Node_name, '(A,A,I2)')
     &             Base_Name(1:iLength+6),
     &             '/node_',jj
            endif

C...........Get index of node in overall plant network
C...........NPCDAT(ii,9) holds pointer to start of
C...........current plant's nodes
            iNode = NPCDAT(ii,9) + jj-1

C...........Set attribute for node state:
C...........ISV contains state and simulation type data
C...........for each node in network. Function ISV_convert
C...........breaks this data into managable bits.
            call isv_convert(ISV(ii,jj),iState,iSim_type)

C...........Set string name to water/air/solid for
C...........istate = 0/1/9
            if (iState .eq. 0 ) then
               char_temp = 'water'
            elseif (iState .eq. 1 ) then
               char_temp = 'air'
            elseif (iState .eq. 2 ) then
               char_temp = 'hydrogen'
            elseif (iState .eq. 9 ) then
               char_temp = 'solid'
            else
               char_temp = 'unknown'
            endif

C..........-> Update meta-data - tag node with type.
C          -> Nice idea. Doesn't work b/c 'REP_SET_META'
C             configures a h3kreports node to accept data,
C             but no data will be subsequently be sent to
C             h3kreports for this node.
C
C            call REP_SET_META(
C     &           node_name(1:LNBLNK(node_name)),
C     &           'type',
C     &           char_temp(1:LNBLNK(char_temp))
C     &           )
C.....................................................

C...........Transport State variables
            if(jj<10)then
              iLength = iLength_tmp
            else                           ! Longer string length required for 2-digit nodes.
              iLength = iLength_tmp + 1
            endif

           !Temperature:
            write(H3K_rep_NAME,'(A,A)')
     &           Node_Name(1:iLength+13),
     &           '/temperature'
            call add_to_xml_reporting (
     &         CSVF(iNode,iProp_temp),
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Plant component node temperature' )


           !Mass flow (1st phase)
            if (iState .ne.9) then
               write(H3K_rep_NAME,'(A,A,A,A)')
     &              Node_Name(1:iLength+13),
     &              '/',char_temp(1:LNBLNK(char_temp)),'_flow'
               call add_to_xml_reporting (
     &            CSVF(iNode,iProp_1st_flow),
     &            H3K_rep_NAME,
     &            'units', '(kg/s)',
     &            'Plant component node first-phase flow' )

            endif
           !Mass flow (2nd phase)
            if( iState .eq. 1 ) then
               write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:iLength+13),
     &              '/moisture_flow'
               call add_to_xml_reporting (
     &            CSVF(iNode,iProp_2nd_flow),
     &            H3K_rep_NAME,
     &            'units', '(kg/s)',
     &            'Plant component node second-phase flow' )

            endif
            if( iState .eq. 2 ) then
               write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:iLength+13),
     &              '/hydrogen_flow'
               call add_to_xml_reporting (
     &            CSVF(iNode,iProp_h2_flow),
     &            H3K_rep_NAME,
     &            'units', '(kg/s)',
     &            'Plant component node hydrogen flow flow' )

            endif

C........-> Get connection data (MPDONC = max connections per node)

            do kk = 1, MPCONC

C..............ICONDX(ii,jj,kk) = connection # for component ii,
C..............node jj, coupling kk

               if ( ICONDX(ii,jj,kk) .ne. 0 ) then ! connection exists!
C.................Connection #
                  iConn_numb =  ICONDX(ii,jj,kk)

C.................Build connection base name

                  write(Connect_name,'(A,A,I1)')
     &                 Node_Name(1:iLength+13),
     &                 '/connection_',kk

C.................Get connection properties using H3k_connect_property
C.................function. Synopsys.
C.................
C.................   Property value =  H3k_connect_property (
C.................                        Connection index # ,
C.................                        Requested Property
C.................                     )

C.................Temperature


                  write(H3K_rep_NAME,'(A,A)')
     &                 Connect_name(1:iLength+26),
     &                 '/temperature'
                  call add_to_xml_reporting (
     &               H3K_Connect_property(iConn_numb,iProp_temp),
     &               H3K_rep_NAME,
     &               'units', '(oC)',
     &               'Plant component node connection temperature' )

C.................Mass flow rate - check state of material
                  if (iState .eq. 0) then
C....................Connection is water. 1st-phase flow only!
                     write(H3K_rep_NAME,'(A,A)')
     &                    Connect_name(1:iLength+26),
     &                    '/water_flow'
                     call add_to_xml_reporting (
     &                    H3K_Connect_property(
     &                          iConn_numb,
     &                          iProp_1st_flow
     &                    ),
     &                    H3K_rep_NAME,
     &                    'units', '(kg/s)',
     &                    'Plant component node connection water flow' )

                  elseif (iState .eq. 1) then
C....................Connection is to air. 1st & 2nd phase flow
                      write(H3K_rep_NAME,'(A,A)')
     &                    Connect_name(1:iLength+26),
     &                    '/air_flow'
                      call add_to_xml_reporting (
     &                    H3K_Connect_property(
     &                          iConn_numb,
     &                          iProp_1st_flow
     &                    ),
     &                    H3K_rep_NAME,
     &                    'units', '(kg/s)',
     &                    'Plant component node connection air flow' )


                      write(H3K_rep_NAME,'(A,A)')
     &                    Connect_name(1:iLength+26),
     &                    '/moisture_flow'

                      call add_to_xml_reporting (
     &                    H3K_Connect_property(
     &                          iConn_numb,
     &                          iProp_2nd_flow
     &                    ),
     &                    H3K_rep_NAME,
     &                    'units', '(kg/s)',
     &                    'Plant component node connection '
     &                    // 'moisture flow' )

                  elseif (iState .eq. 2 ) then
C....................Connection is to hydrogen.
                     write(H3K_rep_NAME,'(A,A)')
     &                    Connect_name(1:iLength+26),
     &                    '/hydrogen_flow'

                      call add_to_xml_reporting (
     &                    H3K_Connect_property(
     &                          iConn_numb,
     &                          iProp_h2_flow
     &                    ),
     &                    H3K_rep_NAME,
     &                    'units', '(kg/s)',
     &                    'Plant component node connection '
     &                    // 'Hydrogen flow' )

                  else
C....................Connection is solid. No flow rate.


                  endif


               endif

            enddo               !<- connection loop end

         enddo                  !<- node loop end

      enddo                     !<- component loop end


C.....Call component-by-component post_processing routines
      call h3k_transport_plant_comp_data()

      return
      end



C ************** H3K_Connect_property *********************************
C Created by: Alex Ferguson
C Created on: June 20, 2004
C Copyright:  CETC 2004
C
C ABSTRACT:
C
C This function returns data ( temperature, 1st-phase flow, 2nd-phase
C flow ) for a given plant connection. Note that this data is also contained
C in the ESP-r CONVAR common block, but the subroutine MZPADJ only
C updates CONVAR prior to solution of the plant matrix. This function can
C be called at any time during the simulation to return the most recently
C calculated values of connection data. Thus, it is useful for reporting
C the values of these data after the plant matrix has converged.
C
C Inputs:
C  - iConnection: Index of connection of interest.
C  - iProperty:   Index of requested property:
C                    1 = temperature (oC)
C                    2 = 1st-phase mass flow (kg/s)
C                    3 = 2nd-phase mass flow (kg/s)
C
C Output:
C
C  - Most recently calculated value of requested property
C
C**********************************************************************

      real function H3K_Connect_property(
     &     iConnection,         ! <- index of requested connection
     &     iProperty            ! <- property (1= temperature, 2=1st-phase flow,
     &     )                    !              3= 2nd-phase flow)

      implicit none

#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN                 ! trace output unit numbers
      integer iuout, iuin

C.....ESP-r commons
C NPCON,      # of connections in plant network
C IPC1(MPCON) Index of recieving component
C IPN1(MPCON) Index of recieving node
C IPCT(MPCON) connecion type
C IPC2(MPCON) Index of sending component
C IPN2(MPCON) Index of sending node
C PCONDR(MPCON) connection mass diversion ratio
C PConSD(MPCON,2) supplementary data for some connections
      common/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),IPC2(MPCON),
     &     IPN2(MPCON),PCONDR(MPCON),PConSD(MPCON,2)
      integer npcon, ipc1, ipn1, ipct, ipc2, ipn2
      real pcondr, PConSD

C NPCDAT(MPCOM,9) Misc component data -> NPCDAT(i,9)= pointer to first
C node of component i
C IPOFS1(MCOEFG),      ! row positions for matrix coefficients (not used)
C IPOFS2(MCOEFG,MPVAR) ! column positions for matrix coefficients (not used)
      common/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      integer npcdat, ipofs1, ipofs2

C CSVF(MPNODE,MPVAR),  ! Plant state variables future values
C CSVP(MPNODE,MPVAR)   ! plant state variables present values
      common/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      real csvf, csvp

      common/CLIMIP/
     &     QFPP,                ! not used
     &     QFFP,                ! not used
     &     TPP,                 ! not used
     &     TFP,                 ! Future outdoor temperature
     &     QDPP,                ! not used
     &     QDFP,                ! not used
     &     VPP,                 ! not used
     &     VFP,                 ! not used
     &     DPP,                 ! not used
     &     DFP,                 ! not used
     &     HPP,                 ! not used
     &     HFP                  ! not used
      real qfpp, qffp, tpp, tfp, qdpp, qdfp,
     &     vpp, vfp, dpp, dfp, hpp, hfp
      common/CLMPHG/
     &     HEXTPP,              ! not used
     &     HEXTFP,              ! not used
     &     GEXTPP,              ! not used
     &     GEXTFP,              ! Future outdoor humidity
     &     TWBPP,               ! not used
     &     TWBFP                ! not used
      real hextpp, hextfp, gextpp, gextfp, twbpp, twbfp

C TFA(MCOM),           ! zone future temperatures
      common/FVALA/TFA(MCOM),QFA(MCOM)
      real tfa, qfa
C GFA(MCOM)            ! zone futue  relative humidity values
      common/FVALG/GFA(MCOM)
      real gfa

C.....Passed variables
      integer iConnection       ! index of requested connection
      integer iProperty         ! property requested


C.....Local variables

      integer iSend_component   ! index of sending component
      integer iSend_node        ! index of sending node
      integer iReci_component   ! index of recieving component
      integer iReci_node        ! index of recieving node
      integer iConn_type        ! connection type
      character*256 outmsg      ! messages

      real H3K_connect_temp     ! temperature of connection
      real H3K_connect_1st_flow ! 1st-phase flow rate of connection
      real H3K_connect_2nd_flow ! 2nd-phase flow rate of connection
      real H3K_connect_h2_flow  ! hydrogen flow rate of connection


C.....Named constants
      integer iConn_identical   ! named constant for connection to like node
      integer iConn_known       ! named constant for connection to known state
      integer iConn_other_comp  ! named constant for connection to other component
      integer iConn_zone_amb    ! named constant for connection to building or
                                ! ambient air
      parameter (iConn_identical   = 1,
     &           iConn_known       = 2,
     &           iConn_other_comp  = 3,
     &           iConn_zone_amb    = 4)

      integer iProp_temp        ! named constant for temperature properity
      integer iProp_1st_flow    ! named constant for first phase flow properity
      integer iProp_2nd_flow    ! named constant for second phase flow properity
      integer iProp_h2_flow     ! named constant for hydrogen flow
      parameter ( iProp_temp     = 1,
     &            iProp_1st_flow = 2,
     &            iProp_2nd_flow = 3,
     &            iProp_H2_flow  = 4)
      integer iZone_number



C.....Check inputs. Shouldn't be necessary - but might be helpful if diagnosing
C.....problems if ESP-r data structures change.
      if ( ( iProperty .ne. iProp_temp ) .and.
     &     ( iProperty .ne. iProp_1st_flow ) .and.
     &     ( iProperty .ne. iProp_2nd_flow ) .and.
     &     ( iProperty .ne. iProp_H2_flow  ) ) then

         write (IUOUT,'(A)')
     &        'Fatal error: subroutine H3K_Connect_Property'
         write (IUOUT,'(A,A,I2,A)')
     &        '           ',
     &        ' Requested parameter (iProperty=', iProperty,')'
         write (IUOUT,'(A,A)')
     &        '           ',
     &        ' is out of range (1-3). Check calling routine.'
         stop ' H3K_Connect_Property: unresolvable error '
      endif

      if ( ( iConnection .le. 0 ) .or.
     &     ( iConnection .gt. npcon ) ) then
         write (IUOUT,'(A)')
     &        'Fatal error: subroutine H3K_Connect_Property'
         write (IUOUT,'(A,A,I2,A)')
     &        '           ',
     &        ' Requested connection (iConnection=', iConnection,')'
         write (IUOUT,'(A,A,i2,A)')
     &        '           ',
     &        ' is out of range (1-',npcon,'). Check calling routine.'
         stop ' H3K_Connect_Property: unresolvable error '
      endif


C.....Check if plant connections exist
      if ( npcon.ne.0 ) then

C........Get data out of ESP-r commons
C........Recieving component index, and recieving node index
         iReci_component = ipc1(iConnection)
         iReci_node      =
     &        ipn1(iConnection) + npcdat(iReci_component,9) - 1

C........Sending component index, and sending node index
         iSend_component = ipc2(iConnection)
         iSend_node      =
     &        ipn2(iConnection) + npcdat(iSend_component,9) - 1

C........Connection type
         iConn_type = ipct(iConnection)

C........Check connection type for validity
         if ( ( iConn_type .ne. iConn_identical  ) .and.
     &        ( iConn_type .ne. iConn_known      ) .and.
     &        ( iConn_type .ne. iConn_other_comp ) .and.
     &        ( iConn_type .ne. iConn_zone_amb   ) ) then
            write (IUOUT,'(A)')
     &           'Fatal error: subroutine H3K_Connect_Property'
            write (IUOUT,'(A,A,I2,A)')
     &           '           ',
     &           ' Connection type (Type=', iConn_type,')'
            write (IUOUT,'(A,I2,A)')
     &           ' is out of range (1-4) for connection ',
     &           iConnection,'. Check calling routine.'
            stop ' H3K_Connect_Property: unresolvable error'
         endif


C........Check connection type and set requested property
C........accordingly using appropriate common variable

         if ( iConn_type .eq. iConn_identical ) then
C...........Connection is to like node. Return temperature/humidity ratio
C...........for recieving node and 1st-phase flow rate of reference (sending)
C...........node . Note: this routine and the common
C...........block variable CSVF use the same shema for property,
C...........permitting iProperty to be used directly

C...........Temperature: use recieving node's value
            H3K_connect_temp = CSVF ( iReci_node, iProp_temp ) ! (oC)

C...........1st-phase flow: use referenced node's flow
            H3K_connect_1st_flow = CSVF ( iSend_node, iProp_1st_flow ) ! (kg/s)

C...........2nd-phase flow: use recieving node's humidity ratio,
C...........and scale by reference (sending) node's 1st-phase flow
C...........rate:
C...........                                           (2nd-phase, rec. node)
C........... 2nd-phase flow = (1st-phase, ref. node) * ----------------------
C...........                                           (1st-phase, rec. node)
            H3K_connect_2nd_flow =
     &           CSVF( iSend_node, iProp_1st_flow ) * !<-ref. node 1st-phase flow
     &           CSVF( iReci_node, iProp_2nd_flow ) / !<-rec. node 2nd-phase flow
     &           CSVF( iReci_node, iProp_2nd_flow )   !<-rec. node 1st-phase flow

            H3K_connect_H2_flow =  CSVF ( iSend_node, iProp_h2_flow )

         elseif ( iConn_type .eq. iConn_known ) then
C...........Connection is to known conditions. Temperature
C...........and humidity ratio are described by PConSD(i,1/2)
C...........array, flow rate is described by reference (sending
C...........node)

C...........Temperature  ( use data stored in supplemental data array, oC )
            H3K_connect_temp = PConSD( iConnection, 1 )

C...........First-phase flow rate. (use flow rate for reference "sending" node)
            H3K_connect_1st_flow = CSVF( iSend_node, iProperty )

C...........2nd-phase flow: use humid.ratio stored in supplemntal data array
C...........and scale by reference (sending) node's 1st-phase flow
C...........rate:
C...........
C........... 2nd-phase flow = (1st-phase, ref. node) * (specified humid.ratio)
C...........
            H3K_connect_2nd_flow =
     &           CSVF ( iSend_node, iProp_1st_flow ) * !<-ref. node 1st-phase flow
     &           PConSD ( iConnection, 2 )             !<-specified humidity


         elseif ( iConn_type .eq. iConn_other_comp ) then
C...........Connection is to node in other plant component
C...........(iSend_node) Note: this routine and the common
C...........block variable CSVF use the same shema for property,
C...........permitting iProperty to be used directly

            H3K_connect_temp     = CSVF( iSend_node, iProp_temp )     !(oC)
            H3K_connect_1st_flow = CSVF( iSend_node, iProp_1st_flow ) !(kg/s)
            H3K_connect_2nd_flow = CSVF( iSend_node, iProp_2nd_flow ) !(kg/s)
            H3K_connect_h2_flow  = CSVF( iSend_node, iProp_H2_flow ) !(kg/s)

         elseif ( iConn_type .eq. iConn_zone_amb ) then
C...........Connection is to building zone or outside (zone 0)
C...........Get zone number (contained in PConSD supplemental data array)

            iZone_number = int(PConSD(iConnection,1))

            if (iZone_number .eq. 0 ) then
C..............Connection is to outside.

C..............tfp: esp-r common for future  outdoor temperature
               H3K_connect_temp = tfp ! (oC)

C..............First-phase flow rate. (use flow rate for reference "sending" node)
               H3K_connect_1st_flow = CSVF( iSend_node, iProp_1st_flow ) !(kg/s)

C..............2nd-phase flow: use future exterior humidity ratio (GEXTFP)
C..............and scale by reference (sending) node's 1st-phase flow
C..............rate:
C..............
C.............. 2nd-phase flow = (1st-phase, ref. node) * (ambient humid.ratio)
C..............
               H3K_connect_2nd_flow  =
     &              CSVF( iSend_node, iProp_1st_flow ) * gextfp

C..............H2 flow (should be zero)
               H3K_connect_h2_flow  = CSVF( iSend_node, iProp_H2_flow ) !(kg/s)

            else
C..............Connection is to zone.

C..............tfa(i): esp-r common for future air temperature of zine i
               H3K_connect_temp = tfa(iZone_number) ! (oC)

C..............First-phase flow rate. (use flow rate for reference "sending" node)
               H3K_connect_1st_flow = CSVF( iSend_node, iProp_1st_flow ) !(kg/s)

C..............2nd-phase flow: use zone i future humidity ratio gfa(i)
C..............and scale by reference (sending) node's 1st-phase flow
C..............rate:
C..............
C.............. 2nd-phase flow = (1st-phase, ref. node) * (ambient humid.ratio)
C..............
               H3K_connect_2nd_flow  =
     &              CSVF( iSend_node, iProp_1st_flow ) *
     &              gfa( iZone_number )

C..............H2 flow (should be zero)
               H3K_connect_h2_flow  = CSVF( iSend_node, iProp_H2_flow ) !(kg/s)

            endif

         else
C...........Can't happen - connection types checked above

         endif

      endif

C.....Assign value to returned variable
      if ( iProperty .eq. iProp_temp ) then

         H3K_connect_property = H3K_connect_temp

      elseif ( iProperty .eq. iProp_1st_flow ) then

         H3K_connect_property = H3K_connect_1st_flow

      elseif ( iProperty .eq. iProp_2nd_flow ) then

         H3K_connect_property = H3K_connect_2nd_flow

      elseif ( iProperty .eq. iProp_h2_flow ) then

         H3K_connect_property = H3K_connect_h2_flow

      else

C........Can't happen - iProperty checked above

      endif


      RETURN
      END                       ! <- That's it!



C ******************************* H3K Transport Plant Component Data ***************************
C
C Created by: Alex Ferguson
C Created on: June 24, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a very simple routine used to invoke transport plant domain miscellaneous data to
C h3kreports after convergence of the plant matrix solutions. It loops through all plant
C components and calls the appropriate post-processing routine, if it exists.
C
C**********************************************************************************************
      subroutine h3k_transport_plant_comp_data()
      implicit none
#include "plant.h"
C.....ESP-r commons (see documentation above)
      common/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      integer NPCDAT, IPOFS1, IPOFS2

      common/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      integer npcomp, nci
      real cdata

C.....Local variables
      integer iComponent_index  ! index of current component
      integer iComponent_type   ! type of current component

C.....Named constants
      integer iGas_tank         ! flag for gas-fired tank
      integer iEle_tank         ! flag for electrially heated tank
      parameter( iGas_tank = 1,
     &           iEle_tank = 2
     &     )
      integer iInitialize, iReport ! flags indicating h3kreports
                                   ! initialization status
      parameter ( iInitialize  = 1,
     &            iReport      = 2 )

      do iComponent_index = 1, npcomp

C     Determine appropriate post-processor for current component (ii)
C     Remember NPCDAT(ii,4) is obtained from the component's
C     database code (NPCDAT(ii,3)) divided by 10

         iComponent_type = NPCDAT( iComponent_index, 4 )

C     ESRU 3-node heating coil
         if ( iComponent_type .eq. 41 ) then

            CALL H3K_HeatingCoil_PostPro(iComponent_index)

C     SOFC Fuel cell
         elseif ( iComponent_type .eq. 80 .or.
     &            iComponent_type .eq. 112 ) then

            CALL FC_data_transport(iComponent_index)

C     Gas-fired hot water tank
         elseif ( iComponent_type .eq. 81 ) then

            CALL CETC_tank_data_transport(iComponent_index,iGas_tank)

C     Electrically heated water tank
         elseif ( iComponent_type .eq. 82 ) then

            CALL CETC_tank_data_transport(iComponent_index,iEle_tank)
            
C     Hydrogen Storage
         elseif ( iComponent_type .eq. 101 ) then

            CALL H2_storage_post_process (iComponent_Index)

C     Stirling Engine
         elseif ( iComponent_type .eq. 102 .or.
     &            iComponent_type .eq. 103       ) then

            CALL A42_CHP_H3Kreports_module (
     &                      iComponent_index,
     &                      iReport )

C     Compressed gas cylinder
         elseif ( iComponent_type .eq. 105 ) then
            CALL Comp_Cyl_PostPro(iComponent_Index, iReport)

C     Hydrogen MH Storage
         elseif ( iComponent_type .eq. 106 ) then

            CALL MH_hydride_PostPro(iComponent_Index, iReport)

C     Hydrogen PEMFC 
         elseif ( iComponent_type .eq. 107 ) then

            CALL H2_PEMFC_H3Kreports_module (
     &                 iComponent_Index, iReport, iComponent_type )

C.....CETC models that do not yet have post-processors

C     3 node (ISV=20) gas-fired water tank for first phase of AIMS work.
C         elseif ( iComponent_type .eq. 83 ) then
C            CALL AIMS_tank1_fuel_coeff_gen(IPCOMP,OUT,ISTATS)

C     3 node (ISV=20) first phase thermally activated cooling component.
C         elseif ( iComponent_type .eq. 84 ) then
C            CALL TAC_1_coeff_gen(IPCOMP,OUT,ISTATS)

C     1 node (ISV=20) cold water storage tank for TAC.
C         elseif ( iComponent_type .eq. 85 ) then
C            CALL TAC_coldtank_coeff_gen(IPCOMP,OUT,ISTATS)

C     1 node (ISV=20) PEM cogeneration fuel cell
C         elseif ( iComponent_type .eq. 95 ) then
C            CALL PEM_coeff_gen(IPCOMP,OUT,ISTATS)

C     1 node tank with immersed HX coil
          elseif ( iComponent_type .eq. 119 ) then
             CALL tank_intank_hx_H3Kreports_module(iComponent_Index)  

         else

C     Assume post-processor doesn't exist
         end if
      end do


      return
      end


C ******************************* H3K Transport Electric Data *********************************
C
C Created by: Alex Ferguson
C Created on: June 30, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a very simple routine used to transport miscellanoues electrical network data to
C the H3K reporting facilities.
C
C**********************************************************************************************

      subroutine h3k_transport_electric_data()
      implicit none

C     Load misc. named parameters:
C       total_load / hvac_load / occupant_load / cluster_load / total_gen
#include "CETC_definitions.h"
#include "power.h"

C The electrical nodes common
      common/enod/
     &     nenod,
     &     enodno(menod),
     &     enodphtyp(menod),
     &     enodph(menod),
     &     enodtyp(menod),
     &     enodbasev(menod),
     &     enodbasep
      integer nenod                 ! number of nodes in elec network
      integer enodno                ! not used in current context
      integer enodphtyp             ! not used in current context
      integer enodph                ! not used in current context
      integer enodtyp               ! not used in current context
      real enodbasev                ! not used in current context
      real enodbasep                ! not used in current context



C The elec net load, generated and transmitted nodal power flows (SI)
      common/enodlgt/
     &     penodl(menod),
     &     qenodl(menod),
     &     penodg(menod),
     &     qenodg(menod),
     &     penodt(menod),
     &     qenodt(menod)

      real penodl     ! real load
      real qenodl     ! reactive load
      real penodg     ! real generated power
      real qenodg     ! reactive generated power
      real penodt     ! real transmitted power
      real qenodt     ! reactive transmitted power


C---------------------------------------------------------------------
C     Hybird Componenet data
C---------------------------------------------------------------------
      COMMON/HYBE/
     &     nHYBcom,
     &     HYBCOMNO,
     &     HYCOMTYP,
     &     HYCOMPHTYP,
     &     HYCONEN,
     &     HYLOC,
     &     NHYBDAT,
     &     HYBDAT

      integer nHYBcom
      integer NHYBDAT(MHYCOM)
      integer HYBCOMNO(MHYCOM), HYCOMTYP(MHYCOM),  HYCOMPHTYP(MHYCOM)
      integer HYCONEN(MHYCOM,MPHAS), HYLOC(MHYCOM,MPHAS)
      real HYBDAT(MHYCOM,MHYDAT)

C---------------------------------------------------------------------
C     HOT3000 Storage common blocks used for plant->electrical network
C     coupling (W). Used to transport network solution back to explicit
C     plant domain.
C---------------------------------------------------------------------
      COMMON/PLANT_ENET_TRANSPORT/
     &     Hybrid_Power_Real(MHYCOM),
     &     Elec_Power_Real(MHYCOM)
C.....Real power from hybrid & power only components
      REAL Hybrid_Power_Real, Elec_Power_Real

C---------------------------------------------------------------------------------
C Common holding the power-only component description read from the electric
C network input file.
C---------------------------------------------------------------------------------
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C---------------------------------------------------------------------------------
C Common holding power generation and loads data for power-only components.
C---------------------------------------------------------------------------------
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)

      INTEGER      NPOWCOM, POWCOMNO, POWCOMID, POWCOMPHTYP, POWCONEN,
     &             NPOWCDAT, NPOWCDATS
      REAL         POWCDAT
      REAL         PPOWOC, QPOWOC
      COMPLEX      VPOWC
C---------------------------------------------------------------------------------
C     Names for electrial network comoponents
C---------------------------------------------------------------------------------

      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

      CHARACTER*12 ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM
      CHARACTER*72 POWCDATS

C.....Common for meta initialization flags
      COMMON / H3KmetaInitVal /
     &     bBuildMeta,
     &     bClimMeta,
     &     bTimeMeta,
     &     bPlantMeta,
     &     bElectMeta

      logical  bBuildMeta,bClimMeta,bTimeMeta,bPlantMeta,bElectMeta

C---------------------------------------------------------------------------------
C Common holding nodal voltages (actual voltages, not "per unit" voltages.
C---------------------------------------------------------------------------------
      COMMON/ENODSLV/ENODVLT(MENOD)
      COMPLEX ENODVLT

C---------------------------------------------------------------------------------
C     local varibles
C---------------------------------------------------------------------------------
C.....Counters
      integer iElec_node
      integer iHybridComponent
      integer iPOnlyComponent
      integer LNBLNK

      character*128 H3K_rep_NAME ! XML leaf name
      character*128 node_name   ! elec-net node name
      real elec_net_load_calc   ! miscellaneous function that retuns data
                                ! from electrical network depending on the
                                ! integer parameter passed to it.

      integer iLength

C.....TOTAL electrical load
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'electrical_net/loads/total_load'

      call add_to_xml_reporting (
     &          elec_net_load_calc(total_load),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total load on electrical network')

C.....HVAC electrical load
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'electrical_net/loads/HVAC_load'
      call add_to_xml_reporting (
     &          elec_net_load_calc(hvac_load),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'HVAC load on electrical network')

C.....Occupant electrical load
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')'electrical_net/loads/occupant_load'
      call add_to_xml_reporting (
     &          elec_net_load_calc(occupant_load),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Occupant-driven load on electrical network')


C.....External electrical load (ie load from clusters of houses)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')
     &     'electrical_net/loads/external_load'
      call add_to_xml_reporting (
     &          elec_net_load_calc(cluster_load),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Load on electrical network from off-site sources')

C.....Total generation (on+off site)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')
     &     'electrical_net/generation/total_generation'
      call add_to_xml_reporting (
     &          elec_net_load_calc(total_gen)
     &          + elec_net_load_calc(offsite_gen),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total onsite+offsite electrical generation')

C.....Total generation (onsite)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')
     &     'electrical_net/generation/onsite_generation'
      call add_to_xml_reporting (
     &          elec_net_load_calc(total_gen),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total onsite electrical generation')


C.....Total generation (offsite)
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')
     &     'electrical_net/generation/offsite_generation'
      call add_to_xml_reporting (
     &          elec_net_load_calc(offsite_gen),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total offsite electrical generation')

C.....Total generation (offsite) coincident with occupant load
C.....-> set name
      WRITE(H3K_rep_NAME,'(A)')
     &     'electrical_net/generation/offsite_coincident'

      if( (elec_net_load_calc(offsite_gen) .eq. 0.0) .or.
     &    (elec_net_load_calc(occupant_load) .eq. 0.0) ) then
C.....-> update data
         call add_to_xml_reporting (
     &          0.0,
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total offsite generation coincident '
     &          // 'with occupant load')

      else if( elec_net_load_calc(offsite_gen) .ge.
     &     elec_net_load_calc(occupant_load) ) then
C.....-> update data
            call add_to_xml_reporting (
     &          elec_net_load_calc(occupant_load),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total offsite generation coincident '
     &          // 'with occupant load')

      else
C.....-> update data
            call add_to_xml_reporting (
     &          elec_net_load_calc(offsite_gen),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Total offsite generation coincident '
     &          // 'with occupant load')
      endif



C.....Grid import
C.....-> set name
      if ( elec_net_load_calc(balance) .lt. 0.0 ) then
C.....-> update data
         call add_to_xml_reporting (ABS(elec_net_load_calc(balance)),
     &                              'electrical_net/grid/import',
     &                              'units', '(W)',
     &                              'Net balance on electrical grid')
      else
         call add_to_xml_reporting (0.0,
     &                              'electrical_net/grid/import',
     &                              'units', '(W)',
     &                              'Net balance on electrical grid')

      endif



C.....Grid export
       IF(elec_net_load_calc(balance) .gt. 0.0 ) then
C.....-> update data
         call add_to_xml_reporting (elec_net_load_calc(balance),
     &                              'electrical_net/grid/export',
     &                              'units', '(W)',
     &                        'Surplus electricity exported go grid')

      else
        call add_to_xml_reporting (0.0,
     &                              'electrical_net/grid/export',
     &                              'units', '(W)',
     &                        'Surplus electricity exported go grid')

      endif


C.....Net grid export
         call add_to_xml_reporting (elec_net_load_calc(balance),
     &                              'electrical_net/grid/net_balance',
     &                              'units', '(W)',
     &        'Net electricity balance')


C.....Dump load/generated/transmitted power for each node in
C.....network. This data is not very useful, but does provide
C.....a snap-shot of the total network performance, and thus
C.....is handy for testing

      do iElec_node = 1, nenod

         if (iElec_node .le. 9) then
            WRITE (node_name, '(A,I1);')
     &           'electrical_net/nodes/node_00', iElec_node
         elseif(iElec_node .le. 99) then
            WRITE (node_name, '(A,I2);')
     &           'electrical_net/nodes/node_0', iElec_node
         else
            WRITE (node_name, '(A,I3);')
     &           'electrical_net/nodes/node_', iElec_node
         endif

C........Nodal voltage. Note that the voltage is a complex variable.
C........Report both the magnitude and the angle of the voltage phasor.
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/voltage/magnitude'
         call add_to_xml_reporting (
     &          REAL(ENODVLT(iElec_node)),
     &          H3K_rep_NAME,
     &          'units', '(Volts)',
     &          'Electrical network node: V magnitude')
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/voltage/angle'
         call add_to_xml_reporting (
     &          AIMAG(ENODVLT(iElec_node)),
     &          H3K_rep_NAME,
     &          'units', '(radians)',
     &          'Electrical network node: V angle')

C........Nodal real load
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:29), '/load/real'
         call add_to_xml_reporting (
     &          penodl(iElec_node),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network node: real load')

C........nodal reactive load
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/load/reactive'
         call add_to_xml_reporting (
     &          qenodl(iElec_node),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network node: reactive load')

C........Nodal real generation
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/generation/real'
         call add_to_xml_reporting (
     &          penodg(iElec_node),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network node: real generation')


C........nodal reactive generation
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/generation/reactive'
         call add_to_xml_reporting (
     &          qenodg(iElec_node),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network node: reactive generation')

C........Nodal real transmission
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/transmission/real'
         call add_to_xml_reporting (
     &          penodt(iElec_node),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network node: real transmission')


C........nodal reactive transmission
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:LNBLNK(node_name)), '/transmission/reactive'
         call add_to_xml_reporting (
     &          qenodt(iElec_node),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network node: reactive transmission')

      enddo

C.....Write out electrical power of hybrid components.

      do iHybridComponent = 1, nHYBcom

         WRITE (node_name, '(A,A);')
     &        'electrical_net/hybrid_components/',
     &        HYCOMNAM(iHybridComponent)

         iLength = lnblnk ( node_name )

C........hybrid component flux
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:iLength), '/flux'
         call add_to_xml_reporting (
     &          Hybrid_Power_Real(iHybridComponent),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network hybrid component net '
     &          // 'electrical power ')

      enddo

C.....Power-only component flux
      do iPOnlyComponent = 1, nPOWcom

         WRITE (node_name, '(A,A);')
     &        'electrical_net/power_only_components/',
     &        POWCOMNAM(iPOnlyComponent)

         iLength = lnblnk ( node_name )

C........component flux
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)')
     &        node_name(1:iLength), '/flux'
         call add_to_xml_reporting (
     &          PPOWOC(iPOnlyComponent),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Electrical network power only component: '
     &          // 'electrical power')

      enddo

      return
      end

C----------------  h3K_InitStringStorage -----------------------
C
C     This routine is a short-hand interface used to invoke
C     the initialization facility in h3k_String_Inventory.
C
C     Inputs & outputs: None
C
C---------------------------------------------------------------
      subroutine h3k_InitStringStore ()
      implicit none

      integer iAction
      character*128 cDummy1
      integer iDummy2, iDummy3

      iAction = 1 ! Initialize inventory

      call h3k_String_Inventory( iAction, cDummy1, iDummy2, iDummy3 )

      return
      end

C---------------- h3k_StoreString -----------------------------
C
C     This routine is a short-hand interface used to invoke
C     the string-storage facility in h3k_String_Inventory
C
C     Inputs:
C        - cString: string to be stored.
C
C     Outputs:
C        - iLocation: unique integer identifying string
C        - iLength: length of string
C
C---------------------------------------------------------------
      subroutine h3k_StoreString ( cString, iLocation, iLength )
      implicit none

      integer iAction
      character*128 cString
      integer iLocation
      integer iLength

      iAction = 2 ! Store a string

      call h3k_String_Inventory(iAction, cString, iLocation, iLength)

      return
      end
C---------------- h3k_RecoverString ---------------------------
C
C     This routine is a short-hand interface used to invoke
C     the string-recovery facility in h3k_String_Inventory
C
C     Inputs:
C        - iLocation: unique integer identifying string

C
C     Outputs:
C        - cString: string to be stored.
C        - iLength: length of string
C
C---------------------------------------------------------------

      subroutine h3k_RecoverString ( iLocation, cString, iLength )
      implicit none

      integer iAction
      character*128 cString
      integer iLocation
      integer iLength

      iAction = 3 ! Recover a string

      call h3k_String_Inventory(iAction, cString, iLocation, iLength)

      return
      end
C---------------- h3k_String_Inventory ------------------------
C
C     This is a simple string-handling facility that
C     can store and recover strings from an inventory. It
C     greatly reduces the number of string manipulations
C     that must be performed in support of the h3kreports
C     module, and thus also reduces the burden on computing
C     resources.
C
C     h3k_String_Inventory is not intended to be called
C     directly, but rather through one of the short-hand
C     interfaces (h3K_InitStringStorage, h3k_StoreString,
C     and h3k_RecoverString). These interfaces invoke one
C     of three possible actions: initialization, storage
C     and recovery. Inputs & outputs are as follows:
C
C     iAction:   input indicating which action should be
C                invoked
C     cString:   The string to be stored (input), or
C                recovered (output) from the inventory
C     iLength:   The length of the string (output)
C     iLocation: Pointer to the string to be recovered
C                (input), or the location in which the
C                string was stored (output)
C--------------------------------------------------------------
      subroutine h3k_String_Inventory
     &           ( iAction, cString, iLocation, iLength )
      implicit none

C External functions.     
      integer lnblnk

C Passed arguements
      integer iAction
      character*128 cString
      integer iLocation
      integer iLength

C Named constants & local variables
      integer iMaxH3KVars
      parameter (iMaxH3KVars = 100)

      integer iActive_index
      save iActive_index

      character*128 cStrings (iMaxH3KVars)
      integer iLengths (iMaxH3KVars)
      save cStrings, iLengths

      if ( iAction .eq. 1 ) then
        iActive_index = 0  ! Initialize iActive_index
      elseif ( iAction .eq. 2 ) then

C Store a new string
        iActive_index = iActive_index + 1
        if ( iActive_index .gt. iMaxH3KVars ) then
          stop "H3K reports error! Too many variables"
        endif
        iLength = lnblnk( cString )
         
        cStrings (iActive_Index ) = cString
        iLengths (iActive_index ) = iLength

        iLocation = iActive_index

      elseif ( iAction .eq. 3 ) then

C Recover the string indicated by iLocation
        cString = cStrings (iLocation)
        iLength = iLengths (iLocation)

      endif

      return
      end

C ******************************* H3K Transport Massflow Network Data ************************
C
C Created by: Achim Geissler
C Created on: June 17, 2008
C Copyright: GaCH
C ________
C ABSTRACT:
C This is the routine used to transport mass flow domain state-variable data to the H3K
C reporting facilities.
C
C**********************************************************************************************
      subroutine h3k_transport_mfn_data()
      implicit none
#include "building.h"
#include "net_flow.h"
C.....ESP-r commons
C      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
C      CHARACTER LAPROB*72

      COMMON/MFLOW1/NNOD,NCMP,NCNN
C Network size
      integer     NNOD  !  - number of nodes (building zones and/or plant components)
      Integer     NCMP  ! - number of fluid flow components (flow restrictions)
      integer     NCNN  ! - number of interconnections (branches)
C
      COMMON/MFLOW2/NDNAM(0:MNOD)
      character*12  NDNAM  ! - identifier/name
C
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),
     & SUPNOD(MNOD,MNDS)
      integer     NDFLD ! - fluid type (1=air, 2=water)
      Integer     NDTYP ! - type (0=internal; unknown pressure
C                                                  1=internal; known total pressure
C                                                  2=boundary; known total pressure
C                                                  3=boundary; wind induced pressure; implies NDFLD=1)
      REAL          HNOD  ! - height above datum (m)
      REAL          SUPNOD ! - supplementary data items (max. MNDS)
C              if NDTYP=0 none
C                 NDTYP=1 total pressure (Pa)
C                 NDTYP=2 total pressure (Pa)
C                         node fluid temperature flag, indicating:
C                         0: TNOD is constant
C                         1: TNOD equals DRYB
C                 NDTYP=3 wind pressure coefficients index
C                         surface azimuth (degrees clockwise from north)
C
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      integer     ITND !  - node temperature index; if ITND(I)=N then TNOD(I)=TNOD(N)
      real      TNOD  ! - node fluid temperature (C)
C Components: fixed input data      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)      character*12     CMNAM ! - identifier/name      Character*60     LTPCMP ! - short description of that type      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),     &              SUPCMP(MCMP,MCMS)      integer     ITPCMP ! - type number      integer     ISDCMP  ! - number of supplementary data items (max. MCMS)      integer     ISDCNN  ! - number of connection level supplementary items (max. MCNS)      real         SUPCMP ! - component supplementary data items (1st item = fluid type)C Connections: fixed input data
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      integer     NODPS ! - node number on positive side of connection
      real          HGTPS ! - height of +ve linkage point relative to NODPS (m)
      integer     NODNE ! - node number on negative side of connection
      Real          HGTNE ! - height of -ve linkage point relative to NODNE (m)
      integer     ITPCON ! - number of linking fluid flow component
      integer     NDSCNN ! - connection level component supplementary node numbersC Output data
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

C     FLW1   - 1st fluid flow through connection; positive if flow
C              from NODPS to NODNE (kg/s)
C     FLW2   - 2nd fluid flow through connection (applicable in case of
C              e.g. a door); positive if flow from NODPS to NODNE (kg/s)
C     PRES   - node total pressure (Pa)
C     RESID  - node fluid mass flow residual (kg/s)
C     SAFLW  - node coupled sum of absolute mass flow rates (kg/s)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION FLWIN(MNOD),FLWUT(MNOD)
      DOUBLE PRECISION FLWINV(MNOD),FLWUTV(MNOD)
C.....Local variables
      integer ii,jj                           ! counters
      CHARACTER*12 fluid                ! fluid type ('air' or 'water')
      character*128 Node_Name     ! node name
      character*128 Connect_Name ! connection name
      character*128 H3K_rep_NAME ! XML leaf name
      integer i_neg, i_CompType,i_CompIndex      real area,rhon,PI ! component area and fluid density at node, PI            Logical b_noveloc ! logical for velocity calc.C.....Commons indicating if meta items have been initialized      COMMON / H3KmetaInitVal /     &     bBuildMeta,     &     bClimMeta,     &     bTimeMeta,     &     bPlantMeta,     &     bElectMeta      logical  bBuildMeta,bClimMeta,bTimeMeta,bPlantMeta,bElectMeta
C.....References
      integer LNBLNK    ! function returning # of non-blank characters in
                                ! a string
      real DENSIT         ! function returning the density of a fluid
C.....Crude check if there is any flow network
      IF (NNOD.le.0) THEN        write(*,*)"no flow network to output"        RETURN
      ENDIF
C..... Call MFSTFL which sums up all flows in and out of a node ...
C..... IDIM = 1 => flows in kg/s
      CALL MFSTFL(1,FLWIN,FLWUT)
C..... IDIM = 2 => flows in m3/h      CALL MFSTFL(2,FLWINV,FLWUTV)
C.....Loop through all nodes in a flow network
      do ii = 1,NNOD
C...........Set string name to water/air/unknown (the latter shouldn't really happen!)
         if (NDFLD(ii) .eq. 1 ) then
            fluid = 'air'
         elseif (NDFLD(ii) .eq. 2 ) then
            fluid = 'water'
         else
            fluid = 'unknown'
         endif

C........Build node name
         write(Node_Name, '(A,A,A,A)')
     &        'mfn/',fluid(1:LNBLNK(fluid)),'/',     &        NDNAM(ii)(1:LNBLNK(NDNAM(ii)))
C.........output node data	! total mass flow entering a node *******************************
         write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:LNBLNK(Node_Name)),
     &              '/flowrate'

         call add_to_xml_reporting (
     &              real(FLWIN(ii)),
     &              H3K_rep_NAME,
     &              'units', '(kg/s)',
     &              'mfn total node flow rate' )
	! total volume flow entering a node *******************************         write(H3K_rep_NAME,'(A,A)')     &              Node_Name(1:LNBLNK(Node_Name)),     &              '/volflowrate'         call add_to_xml_reporting (     &              real(FLWINV(ii)),     &              H3K_rep_NAME,     &              'units', '(m3/h)',     &              'mfn total node volume flow rate' )	! node temperature from flow network *******************************         write(H3K_rep_NAME,'(A,A)')     &              Node_Name(1:LNBLNK(Node_Name)),     &              '/temp'         call add_to_xml_reporting (     &              TNOD(ii),     &              H3K_rep_NAME,     &              'units', '(oC)',     &              'mfn node temperature' )C........Build connection nameC........Only use positive side of connectionsC.........Loop through all interconnections in a flow network         do jj = 1,NCNN            if (NODPS(jj) .eq. ii) THEN ! only look for positive side conn. of cur. node               i_neg = NODNE(jj) ! the node on the other side of the conn.               write(Connect_Name, '(A,A,A,A,A,A,A,A,A)')     &             'mfn/',fluid(1:LNBLNK(fluid)),'/',     &             NDNAM(ii)(1:LNBLNK(NDNAM(ii))),'->',     &             NDNAM(i_neg)(1:LNBLNK(NDNAM(i_neg))),     &             ' (via ',     &             CMNAM(ITPCON(jj))(1:LNBLNK(CMNAM(ITPCON(jj)))),')'
	! pressure drop between nodes of connection ***********************	       write(H3K_rep_NAME,'(A,A)')     &             Connect_Name(1:LNBLNK(Connect_Name)),     &             '/Dp'               call add_to_xml_reporting (     &             real(PRES(ii)-PRES(i_neg)),     &             H3K_rep_NAME,     &             'units', '(Pa)',     &             'mfn connection pressure drop' )
	! flow through connection ****************************************               write(H3K_rep_NAME,'(A,A)')     &             Connect_Name(1:LNBLNK(Connect_Name)),     &             '/flow'               call add_to_xml_reporting (     &             real(FLW1(jj)),     &             H3K_rep_NAME,     &             'units', '(kg/s)',     &             'mfn connection flow rate' )        ! velocity at connection *********************************        ! The following code is taken from mfget.F, lines 1266 ff., (moved type 460	! to "no velocity" and added type 211 to "no velocity")        ! Find the related component type and reject those	! connections which are not appropriate.               i_CompIndex=ITPCON(jj)               i_CompType=ITPCMP(i_CompIndex)               b_noveloc=.false.               if (i_CompType .eq. 10 .or. i_CompType .eq. 15 .or.   ! flow resistances     &             i_CompType .eq. 17 .or. i_CompType .eq. 20 .or.   ! of various definitions     &             i_CompType .eq. 25 .or. i_CompType .eq. 30 .or.   ! see mfmach.F, lines     &             i_CompType .eq. 35 .or. i_CompType .eq. 310 .or.  ! 580 ff.     &             i_CompType .eq. 410 .or. i_CompType .eq. 211 .or.     &             i_CompType .eq. 460) then                      ! ** skip this component **		      b_noveloc=.true.               elseif(i_CompType .eq. 40) then      ! common orifice flow component                  area=SUPCMP(i_CompIndex,2)               elseif(i_CompType.eq.50)then   ! laminar pipe volume flow rate                  PI = 4.0 * ATAN(1.0)                  area= PI * (SUPCMP(i_CompIndex,3)*     &                     SUPCMP(i_CompIndex,3))               elseif(i_CompType.eq.110)then  ! specific air flow opening                  area=SUPCMP(i_CompIndex,2)               elseif(i_CompType.eq.120)then  ! specific air flow crack component                  area=SUPCMP(i_CompIndex,2)*SUPCMP(i_CompIndex,3)               elseif(i_CompType.eq.130)then  ! specific air flow door                  area=SUPCMP(i_CompIndex,2)*SUPCMP(i_CompIndex,3)               elseif(i_CompType .eq. 210 .or.  ! general flow conduit (ie. duct or pipe).     &                   i_CompType .eq. 220 .or.  ! flow conduit ending in converging 3-leg junction     &                   i_CompType .eq. 230 .or.  ! flow conduit starting in diverging 3-leg junction     &                   i_CompType .eq. 240 .or.  ! flow conduit ending in converging 4-leg junction     &                   i_CompType .eq. 250)then  ! flow conduit starting in diverging 4-leg junction                  area=SUPCMP(i_CompIndex,3)               elseif(i_CompType .eq. 420)then ! flow corrector (ie. valve or damper) with polynomial flow resistance                  area=SUPCMP(i_CompIndex,2)               else                  area=1.0 ! should'nt really happen ...               endif        ! end code snippet from mfget.F	       if (b_noveloc .eqv. .false.) THEN        ! Now get density of "sending" node. If flw1(jj) is negative, use density of 	! node "i_neg". Densit() returns density in kg/m3                  if (FLW1(jj) .lt. 0) THEN	             rhon=DENSIT(NDFLD(i_neg),TNOD(i_neg))                  Else	             rhon=DENSIT(NDFLD(ii),TNOD(ii))	          Endif        ! Finally, write to H3K reporting                  write(H3K_rep_NAME,'(A,A)')     &               Connect_Name(1:LNBLNK(Connect_Name)),     &               '/veloc'                  call add_to_xml_reporting (     &               real(FLW1(jj))/rhon/area,     &               H3K_rep_NAME,     &               'units', '(m/s)',     &               'mfn connection flow velocity' )               endif ! end b_noveloc .true.            endif     !<- end if positive side connection for current node         enddo                  !<- connection loop end
        ! whatever *****************************************************

      enddo                     !<- node loop end

      return
      end
