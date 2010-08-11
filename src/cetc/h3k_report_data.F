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
C h3k_connect_property:      Function returning data for plant connections.
C
C h3k_transport_mfn_data:     Transports flow network data
C
C *********************************************************************************************

C ******************************* H3K Transport Building Data *********************************
C
C Created by: Alex Ferguson
C Created on: March 28, 2004
C Copyright: CETC
C ________
C ABSTRACT:
C This is a very simple routine used to transport miscellanoues building data to the H3K
C reporting facilities. It duplicates some of the functionality found in CETC's H3Kstore
C routine and ESRU's MZSL3 routine, but this redundancy is necessary to permit this
C code to be called independently of these routines.
C
C __________
C REFERENCES:
C
C D.M. Sandars and S.W. Barakat (1983), "A method for estimating the
C   utilization of solar gain through windows", ASHRAE Transactions
C   Vol 89, Part 1A, pg 12--22.
C
C**********************************************************************************************

      SUBROUTINE H3K_transport_build_data(iZone)
      IMPLICIT NONE
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "aim2_common.h"
#include "control.h"
C.....Passed variable
      integer iZone            ! Zone number

C.....Common variables

      common/btime/btimep,btimef
      real btimep,btimef

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Calender and day types
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Basic zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! number of connections 

      common/prec9/nConst(mcom),nELts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      integer nConst          ! # of surfaces per zone
      integer nELts           ! # number of layers per surface (who knew?)
      integer ngaps           ! # number of air gaps witin surface (not used)
      integer npgap           ! Location of air gap. (not used)

C TPA(MCOM),           ! - present zone air point temperature (oC)
C QPA(MCOM)            ! - present zone air point heat injection (W)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      REAL TPA, QPA
      
C TFA(MCOM)            ! - future zone air point temperature (oC)
C QFA(MCOM)            ! - future zone air point heat injection (W)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      REAL TFA, QFA

C TPC(MCOM,MS,MN),     ! - present construction temperature (oC)
C QPC(MCOM)            ! - present construction heat injection (W)
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      REAL TPC, QPC

C TFC(MCOM,MS,MN),     ! - future construction temperature (oC)
C QFC(MCOM)            ! - future construction heat injection (W)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      REAL TFC, QFC

C TPS(MCOM,MS),        ! - present surface temperature (oC)
C QPS(MCOM)            ! - present surface heat injection (W)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      REAL TPS, QPS

C TFS(MCOM,MS),        ! - future surface temperature (oC)
C QFS(MCOM)            ! - future surface heat injection (W)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      REAL TFS, QFS

      COMMON/SETUQ/           ! PLANT heat injection/extration:
     &     QPLTP(MCOM),         !  - present (W)
     &     QPLTF(MCOM),         !  - future (W)
     &     CONV(MCOM)           !  - convective ratio
      real qpltp, qpltf, conv

C.....Climate data 
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

C .... should be climate data:
      COMMON/atpres/PATMOS
      real PATMOS  ! atmospheric pressure - a constant, sadly

C.....Surface orientation flags
      common/SurfInfo/iSurfOrient(MCOM,MS)
      integer iSurfOrient

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM),
     &       IDBTZNM(MCOM)
      integer IALLCLM,ICASUAL,IZIVENT,IRAIRVL,ISETPTT,ICTLSTA
      integer ISKYLUX,IDBTEXT,IWINDVL,IWINDDR,IRELHUM,IDIFHSL
      integer IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,ISKYTMP,IGLOVRT
      integer ICASUAL3,IACTIV,IBIDIRS,IVERSOL,IHTCLSETP,IDBTZNM

C Common for occupant/lighting/equipment sensible convective, sensible
C radiant and latent W for future and present in each zone.
      common/applyctl/ctlperocupc(MCOM,2),ctlperocupr(MCOM,2),
     &  ctlperocupl(MCOM,2),ctlperlightc(MCOM,2),ctlperlightr(MCOM,2),
     &  ctlperlightl(MCOM,2),ctlperequipc(MCOM,2),ctlperequipr(MCOM,2),
     &  ctlperequipl(MCOM,2) 
      real ctlperocupc,ctlperocupr,ctlperocupl ! occupant after control applied
      real ctlperlightc,ctlperlightr,ctlperlightl ! lighting after control applied
      real ctlperequipc,ctlperequipr,ctlperequipl ! equipment after control applied

C Casual gain data.
C NCGPER - number of periods for each gain (zone, day type, gain type)
C TCGS - start time of gain (zone, day type, gain type, period)
C CGSENR - sensible gain radiant portion (zone, day type, gain type, period)
C CGSENC - sensible gain convective portion (zone, day type, gain type, period)
C CGLAT - latent gain (zone, day type, gain type, period)
C CGCTL - gain control fraction (zone, present/future, gain type)
C ICGCTL - pointer to controlled gain type.
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      integer ncgper
      real tcgs,cgsenc,cgsenr,cglat,cgctl

      COMMON/LIGHTS/ICGCTL(MCOM)
      integer icgctl

      COMMON/CasDat/            ! Building casual gains
     &     Cas_Rad_dat(MCOM),   !  - Radiant (W)
     &     Cas_Con_dat(MCOM),   !  - Convective (W)
     &     Cas_Lat_dat(MCOM)    !  - Latent (W)
      real Cas_Rad_dat, Cas_Con_dat, Cas_Lat_dat

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,MGTY)
      CHARACTER lodlabel*6

C Average illuminance in lighting zone
      common/CFCLighting/AveLtzIllum(mcom,mlcom),Nlitz2(MCOM)
      real AveLtzIllum
      integer Nlitz2
C Infiltration

      COMMON/COE39S/CIF(MCOM),CVF(MCOM),X3(MCOM),CVM(MCOM)
      real CIF,CVF,X3,CVM	  
      COMMON/COE39PS/CIP(MCOM),CVP(MCOM),QVP(MCOM),CVPM(MCOM)
      real CIP,CVP,QVP,CVPM

C.....Window properties
      common/prectc/itmcfl(mcom,ms),tmct(mcom,mtmc,5),
     &       tmca(mcom,mtmc,me,5),tmcref(mcom,mtmc),tvtr(mcom,mtmc)
      real tmct, tmca, tmcref, tvtr
      integer itmcfl

C.....Description of zone control action; these data are used
C.....in H3Kreports to determine heating, cooling loads and
C.....to evaluate passive solar design performance.
      common/H3KReportsControl/bZoneHeated,   bZoneCooled,
     &                         fHeatSetpoint, fCoolSetpoint

C.....Flags indicating zone is heated, cooled.
      logical bZoneHeated(MCOM), bZoneCooled(MCOM)

C.....Heating and cooling setpoint (oC)
      real fHeatSetpoint(MCOM), fCoolSetpoint(MCOM)

C.....Ventilation, infiltration data 
      common/coe39/cvip,cvvp,cvif,cvvf,qvnp,xx3,cvvpm,cvvfm
      real cvip, cvif           ! Conductances assoc. w/ infiltration
                                !   on present and future time rows
      real cvvp, cvvf           ! Conductances assoc. w/ ventilation
                                !   on present and future time rows
      real qvnp,xx3,cvvpm,cvvfm ! Not used in current context 

C ..... Zone humidity data
      COMMON/VAPC2/ZMDAP(MCOM),ZMDAF(MCOM)
      real ZMDAP,ZMDAF  ! zone present and future mass of dry air [kg]
      
      COMMON/PVALG/GPA(MCOM)
      COMMON/FVALG/GFA(MCOM)
      real GPA,GFA   ! zone pres. and fut. air moisture content [kg/(kg dry air)]

      COMMON/MOIST54/Pfair(MCOM),Ffair(MCOM),Sfair(MCOM)
      real Pfair,Ffair,Sfair  ! zone future air node partial vapour
                              ! pressure, future xxx and 
                              ! future saturation vapour pressure (? mbar?)

C.....Zone boundary conditions
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)

      integer IE                ! Indicates type of connection for zone
                                !   surfaces
      real ARF                  ! Future time-row radiation from
                                !   an external surface.
      real ATP,ATF,ARP          ! Not used in current context

C.....Named constants for decoding IE common;
      integer iBCExternal, iBCInternalSame, iBCInternalConst,
     &        iBCGround,
     &        iBCAdiabatic, iBCBASESIMP, iBCCEN13917

      parameter ( iBCExternal      =  0 ,
     &            iBCInternalSame  = -1 ,
     &            iBCInternalConst = -2 ,
     &            iBCGround        = -4 ,
     &            iBCAdiabatic     = -5 ,
     &            iBCBASESIMP      = -6 ,
     &            iBCCEN13917      = -7  )

C.....Named constants for surface types
      integer iSurfWindow, iSurfFloor, iSurfWall, iSurfCeiling,
     &        iSurfInternal, iSurfFoundation
      parameter ( iSurfWall       = 1,
     &            iSurfFloor      = 2,
     &            iSurfCeiling    = 3,
     &            iSurfWindow     = 4,
     &            iSurfFoundation = 5,
     &            iSurfInternal   = 6 )
C.....Variable indicating type of surface. 
      integer iSurfType

C.....Surface Energy balances.
      common/SRFBAL/              ! Surface energy balances (W):
     &         qswrdi(MCOM,MS,2), !  - INT face: radiation: short-wave
     &         qlwrdi(MCOM,MS,2), !                         long-wave
     &         qconvi(MCOM,MS,2), !              convection
     &         qcondi(MCOM,MS,2), !              conduction ?
     &         qstori(MCOM,MS,2), !              storage ?
     &         qcri(MCOM,MS,2),   !              radiation: casual gains
     &         qpltri(MCOM,MS,2), !              radiation: plant comp.
     &         qstore(MCOM,MS,2), !              energy stored ?
     &         qconde(MCOM,MS,2), !  - EXT face: conduction
     &         qconve(MCOM,MS,2), !              convection
     &         qlwrde(MCOM,MS,2), !              radiation: long-wave to ground
     &         qlwrbd(MCOM,MS,2), !                         long-wave to buildings
     &         qlwrsk(MCOM,MS,2), !                         long-wave to sky
     &         qswrde(MCOM,MS,2)  !                         short wave

      real qswrdi, qlwrdi, qconvi, qcondi, qstori, qcri, qpltri, qstore,
     &     qconde, qconve, qlwrde, qlwrbd, qlwrsk, qswrde

      common/solsum/            ! SOLAR GAINS:
     &     q1adjz(2),           ! - from adjacent zones (W)
     &     q1outs(2),           ! - from exterior (W)
     &     q2adjz(2),           ! - lost to adjacent zones (W)
     &     q2lost(2),           ! - lost to exterior (W)
     &     q2tmc(2),            ! - Absorbed by glazing (W)
     &     q2wall(2),           ! - Absorbed by opaque construction (W)
     &     q2rem(2),            ! - Absorbed by roof? (W)
     &     q2cfc(2)             ! - Absorbed by CFC (W) - currently not used
      REAL q1adjz, q1outs, q2adjz, q2lost, q2tmc, q2wall,q2rem,q2cfc

C.....MLC data
      common/gr1d01/nnds,nndz(mcom),nndc(mcom,ms),nndl(mcom,ms,me)
      integer nnds             ! - # of nodes per: building
      integer nndz             !                   zone
      integer nndc             !                   construction
      integer nndl             !                   layer

C.....Thermal characteristics of MLC layers
      common/vthp14/thrmli(mcom,ms,me,7)
      real ThrMlI

      common/vthp22/thconf(ms,mn),thdnsf(ms,mhcv),thcapf(ms,mhcv)
      real thconf                    ! Thermal conductivity of layers.
      real thdnsf, thcapf            ! Not used in this context

C.....Flags for temperature-dependence of MLC themal properties
      common/vthp32/ivthps,ivthpz(mcom)
      logical ivthps, ivthpz

C.....Surface area data
      common/prec1/sna(mcom,ms)
      real sna                 ! Area of a surface

      common/cctl/icascf(mcom)  ! Zone control type
      integer icascf

      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      real HCIP,HCIF,HCOP,HCOF ! convective heat transfer coefficients "present" and "future"
                                            ! for "inside" and "outside" surface, each

      COMMON/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)
      integer ICORI,ICORE ! Transfer coefficient index for internal and external face of MLC

C......Surface properties (emissivity and ??  area or absorption??)
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
C      REAL TMA                  ! Mean air point temperature (oC)
      REAL ZRH                  ! Zone Relative Humidity (-)
      real TRESL  ! Zone resultant temperature
      real YDUM   ! Dummy variable
      real trsum
      REAL Q_zone,Q_heat,Q_cool   ! Total heat injection / extraction
      REAL Qm2_zone,Qm2_heat,Qm2_cool ! Total heat injection / extraction per m2 of floor area  
      INTEGER i_ZnCtl             ! Zone control flag

      integer IDTY,IDAY           ! index for current day type, current day

      integer IGN,IPER,iPID       ! index of gain type, casual gain period, loop index for casual gain period

      real TS
      DIMENSION TS(MS)  ! surface temperatures for current zone (for resultant temp calc)
      
      INTEGER iSurface,iNode,iViewSurface,iLtz,iSur  ! Counters
      integer iH3K_NameLen ! length of name root

      real SBC                              ! Stephan-Boltzmann constant
      parameter ( SBC=56.7E-9 )

C.....DECLARATIONS for H3Kreporting object
      CHARACTER*128 H3K_rep_NAME
      CHARACTER*2   cZone_Chars, cSurf_Chars, cNode_Chars

C .... String declarations for base output names. The names may *not* include the separator !
C .... Preferably, this string-allocation would be put in a header file so that it could easily be
C .... used wherever h3k output is defined.
      character*3 cBuildingName ! root name for "building"
      character*3 cZoneName ! root name for "zone"
      character*5 cAllZonesName ! root name for data output concerning all zones
      character*1 cSurfaceRootName ! root name for "surface"
      character*1 cNodeRootName ! root name for "node"
      character*1 cSep ! The separator
      character*1 cOne ! generic 1-character string

C lnblnk is a function returning the index of last non-blank character in a string.
      INTEGER LNBLNK

C .... Humidity functions
      REAL DEWPT
      REAL HUMR

C.....Timerows (named constants)
      integer iPresent, iFuture
      parameter (iPresent=1, iFuture=2)

C----------------------------------------------------------------------
C     Zone heat fluxes, gains (W)
C----------------------------------------------------------------------
      real fZoneSolarGains                ! Total solar insolation
      real fZoneUsefulSolarGains          ! Solar gains coincident with
                                          !   heaiting loads
C       real fm2ZoneUsefulSolarGains        ! As fZoneUsefulSolarGains but
C                                           ! per m2 of floor area
                                          
      real fZoneAdverseSolarGains         ! Gains coincident with cooling

      real fZoneSurfaceFlux               ! Flux across all zone surfaces

      real fZoneHeatFlux                  ! Sum of conduction+infiltration
      real fZoneHeatLoad                  !     - when positive
      real fZoneCoolLoad                  !     - when negative
      real fm2ZoneHeatLoad                !fZoneHeatLoad per m2
      real fm2ZoneCoolLoad                !fZoneCoolLoad per m2
      real fm2ZoneHeatFlux                !fZoneHeatFlux per m2
      
      real fZoneHeatFluxAboveGrade        ! Heat flux through above-grade
                                          !    components 
      real fZoneHeatLossBelowGrade
      real fZoneHeatLossAboveGrade
      real fZoneHeatGainBelowGrade
      real fZoneHeatGainAboveGrade
      real fZoneInfiltrationLoad          ! Infiltration load 
      real fZoneVentilationLoad           ! Ventilation load
      real fZoneInternalGains             ! Internal gains
      real fZoneUsefulInternalGains       ! Useful internal gains
C       real fm2ZoneUsefulInternalGains     ! As fZoneUsefulInternalGains but
C                                           ! per m2 of floor area
      real fZoneAdverseInternalGains      ! Gains during cooling

      real fZoneHeatLossWindows           ! Heat load metrics for
      real fZoneHeatLossWalls             !   a zone's windows,
      real fZoneHeatLossCeiling           !   Ceiling and wall.  (W)
      real fZoneHeatLossFloor
      real fZoneHeatLossInfiltration
      real fZoneHeatLossVentilation
      real fZoneInfiltrationACH

      real fZoneHeatGainWindows           ! Heat load metrics for
      real fZoneHeatGainWalls             !   a zone's windows,
      real fZoneHeatGainCeiling           !   Ceiling and wall.  (W)
      real fZoneHeatGainFloor
      real fZoneHeatGainInfiltration
      real fZoneHeatGainVentilation

      real fSurface_balance_net           ! Net surface energy balance

      real fSurLoss, fSurGain                   ! General loss, gain vars (W)

      real fThickness                ! Thickness of a layer in a MLC
      real fConductivity             ! Conductivity of MLC layer.

      real fZoneRH                   ! Zone relative humidity (%)

C.....Misc Zone parameters returned by zoneinfo
C      real fSurfArea(MS)          ! Area of each surface`
C      real fTotalZoneArea         ! Total Zone area
C      real fSurfAzimuth(MS)       ! Azimuth of each surface
C      real fSurfElevation(MS)     ! Elevation of each surface
C      real fTotalZoneVolume       ! Total bounded volume in the zone
C      integer iDummy              ! Dummy variable.

C.....Save elevations for use later
C      real fSurfaceElevation(MCOM,MS)
C      logical bElevationsRecovered(MCOM)
C      data bElevationsRecovered /MCOM * .false./
C      save fSurfElevation, bElevationsRecovered
C      integer iZoneCount, iSurfCount


C----------------------------------------------------------------------
C     Store total building heat fluxes, gains, allowing them to be
C     added zone-by-zone.
C----------------------------------------------------------------------

C.....Plant interaction
      real fSUMHeatInjection                 ! Total heat injection (W)
      real fSUMHeatExtraction                ! Total heat extraction (W)
      real fSUMNetFlux                       ! Total net flux (W)

C.....Loads
      real fSUMNetLoad                       ! Net load 
      real fSUMHeatLoss                      ! Heating load (W)
      real fSUMHeatGain                      ! Cooling load (W)
      real fSUMHeatLossAboveGrade            ! Above-grade heat load (W)
      real fSUMHeatLossBelowGrade            ! below-grade heat load (W)
      real fSUMHeatGainAboveGrade            ! Above-grade Cool load (W)
      real fSUMHeatGainBelowGrade            ! below-grade Cool load (W)
      real fSUMHeatLossWindows               ! Envelope component
      real fSUMHeatLossWalls                 !   heat loss (W)
      real fSUMHeatLossCeiling               !
      real fSUMHeatLossFloor                 !
      real fSUMHeatLossInfiltration          !
      real fSUMHeatLossVentilation
      real fSUMHeatGainWindows               ! Envelope component
      real fSUMHeatGainWalls                 !   heat Gain (W)
      real fSUMHeatGainCeiling               !
      real fSUMHeatGainFloor                 !
      real fSUMHeatGainInfiltration          !
      real fSUMHeatGainVentilation

C.....Gains (W)
      real fSUMInternalGains                 ! Internal sources
      real fSUMUsefulInternalGains           !   - coincident with heating
      real fSUMAdverseInternalGains          !   - coincident with cooling 
      real fSUMSolarGains                    ! Total solar gains
      real fSUMUsefulSolarGains              !   - coincident with heating
      real fSUMAdverseSolarGains             !   - coincident with cooling
      real fSUMExternalSolarAbsorbtion       ! Total opaque absorbtion
      real fSUMPassiveSolarFraction          ! Passive solar heating
                                             !   fraction


C.....Air loads (W)
      real fSUMVentilationLoad               ! Ventilation load (W)
      real fSUMInfiltrationLoad              ! Infiltration load (W)

      save fSUMHeatInjection,
     &     fSUMHeatLoss, fSUMHeatGain, fSUMHeatLossAboveGrade,
     &     fSUMHeatLossBelowGrade, fSUMHeatGainAboveGrade,
     &     fSUMHeatGainBelowGrade,
     &     fSUMInternalGains, fSUMSolarGains, fSUMUsefulSolarGains,
     &     fSUMExternalSolarAbsorbtion, fSUMVentilationLoad,
     &     fSUMInfiltrationLoad, bBuildingHeated,
     &     fSUMAdverseInternalGains, fSUMAdverseSolarGains,
     &     fSUMHeatGainCeiling, fSUMHeatGainWindows, fSUMHeatGainWalls,
     &     fSUMHeatGainFloor, fSUMHeatLossVentilation,
     &     fSUMHeatGainVentilation, fSUMHeatLossInfiltration,
     &     fSUMHeatGainInfiltration



C----------------------------------------------------------------------
C     Criteria to determine if solar gains are useful, or not;
C----------------------------------------------------------------------

      logical bWindowsOpen                    ! Are windows open?
      real fSwingTemperature                  ! Max permissible
                                              !    swing temp. (oC)
      logical bBuildingHeated                 ! Is building heated?
      ! ( These need to be picked up from other parts of esp-r.
      !   Hard-coded for now. )
      parameter ( bWindowsOpen      = .false. )
      parameter ( fSwingTemperature = 5.0     )


C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real fSurface_radiant_gain   ! Function returining the
                                   ! radiant gain on a surface (W)
      real PCRH2                   ! Function returning relative 
                                   ! humidity given zone temperature, 
                                   ! moisture content and atm. pressure.
      real PSat01 ! Function returns the saturation vapour pressure at temperature T
      
C----------------------------------------------------------------------
C      Root Names
C----------------------------------------------------------------------
      cSep='/'
      cBuildingName='bui'

C.....Pad zone index to 'XX'
      if ( iZone .gt. 9 ) then
         write (cZone_Chars,'(I2)') iZone
      else
        write (cZone_Chars,'(A,I1)') '0', iZone
      endif
      
      cZoneName='z' // cZone_Chars
      cAllZonesName='all_z'
      cSurfaceRootName='s'
      cNodeRootName='n'

C----------------------------------------------------------------------
C     Check zone index. If iZone=1, re-zero running totals for
C     entire building.
C----------------------------------------------------------------------

      reset_building_totals: if ( iZone == 1 ) then

        bBuildingHeated                   = .false.
        fSUMHeatInjection                 = 0.0
        fSUMHeatExtraction                = 0.0
        fSUMNetFlux                       = 0.0
        fSUMNetLoad                       = 0.0
        fSUMHeatLoss                      = 0.0
        fSUMHeatGain                      = 0.0
        fSUMHeatLossAboveGrade            = 0.0
        fSUMHeatLossBelowGrade            = 0.0
        fSUMHeatGainAboveGrade            = 0.0
        fSUMHeatGainBelowGrade            = 0.0
        fSUMInternalGains                 = 0.0
        fSUMSolarGains                    = 0.0
        fSUMUsefulSolarGains              = 0.0
        fSUMExternalSolarAbsorbtion       = 0.0
        fSUMUsefulInternalGains           = 0.0
        fSUMVentilationLoad               = 0.0
        fSUMInfiltrationLoad              = 0.0
        fSUMHeatLossWindows               = 0.0
        fSUMHeatLossWalls                 = 0.0
        fSUMHeatLossFloor                 = 0.0
        fSUMHeatLossCeiling               = 0.0
        fSUMHeatLossInfiltration          = 0.0
        fSUMHeatLossVentilation           = 0.0
        fSUMHeatGainWindows               = 0.0
        fSUMHeatGainWalls                 = 0.0
        fSUMHeatGainFloor                 = 0.0
        fSUMHeatGainCeiling               = 0.0
        fSUMHeatGainInfiltration          = 0.0
        fSUMAdverseInternalGains          = 0.0
        fSUMAdverseSolarGains             = 0.0
        fSUMHeatGainVentilation           = 0.0

      endif reset_building_totals

C------------------------- current day type ---------------------------
      IDAY=IDYP
      IF(IHRF.EQ.1)IDAY=IDYP+1
      IF(IDAY.GT.365)IDAY=IDAY-365
      IDTY=ICALENDER(IDAY)

C----------------------------------------------------------------------
C.....Determine control function for current zone.
C.....The following code should determine the zone heat injection
C.....For each zone regardless of the mechanism used to inject the
C.....heat.
      i_ZnCtl=icascf(iZone)

      if(i_ZnCtl.ne.0)then

C If i_ZnCtl is non-zero then iban will be testable.
C Determine where heat is injected/extracted
        if ( iban(i_ZnCtl,2) .gt. 0 .and. iban(i_ZnCtl,3) .eq. 0 ) then

c PIN is at surface.
          Q_zone = QFS(iZone)

        elseif ( iban(i_ZnCtl,3) .gt. 0 ) then

C PIN is within construction
          Q_zone = QFC(iZone)

        elseif ( iban(i_ZnCtl,1) .eq. -1 ) then

C PIN is in a plant component node - not implemented yet.

        elseif ( iban(i_ZnCtl,1) .eq. -2 ) then

C PIN is mix of radiative/convective flux (ie - plant component)
         Q_zone = QPLTF(iZone)

        else

C PIN is air point
          Q_zone = QFA(iZone)

        endif
      else

C A zone with no control 
        Q_zone = 0.0

      endif

C-----------------------------------------------------------------------
C     Determine if flux is injection or extraction.
C-----------------------------------------------------------------------

      if ( Q_ZONE .GT. 0.0) then

         Q_Heat = Q_ZONE
         Q_Cool = 0.

      else

         Q_Heat = 0.
         Q_Cool = ABS(Q_ZONE)

      endif

C-----------------------------------------------------------------------
C     Reset zone energy storage variables to zero.
C-----------------------------------------------------------------------

      fZoneSurfaceFlux             = 0.0
      fZoneHeatLossBelowGrade      = 0.0
      fZoneHeatLossAboveGrade      = 0.0
      fZoneHeatLossWindows         = 0.0
      fZoneHeatLossWalls           = 0.0
      fZoneHeatLossCeiling         = 0.0
      fZoneHeatLossInfiltration    = 0.0
      fZoneHeatLossVentilation     = 0.0
      fZoneHeatLossFloor           = 0.0
      fZoneHeatGainBelowGrade      = 0.0
      fZoneHeatGainAboveGrade      = 0.0
      fZoneHeatGainWindows         = 0.0
      fZoneHeatGainWalls           = 0.0
      fZoneHeatGainCeiling         = 0.0
      fZoneHeatGainInfiltration    = 0.0
      fZoneHeatGainVentilation     = 0.0
      fZoneHeatGainFloor           = 0.0
      fZoneInfiltrationACH         = 0.0

C-----------------------------------------------------------------------
C     Compute ventilation and infiltration loads
C
C         Load = equivlent conductivity * temperature difference
C
C-----------------------------------------------------------------------
      fZoneInfiltrationLoad = cvif * ( tfa(iZone) - tf )
      fZoneVentilationLoad  = cvvf * ( tfa(iZone) - tf )

        INF_is_zone_heated:
     &  if ( bZoneHeated(iZone) .and. bZoneCooled(iZone) ) then

          INF_is_flux_positive:
     &    if ( fZoneInfiltrationLoad > 0.) then

            fZoneHeatLossInfiltration =fZoneInfiltrationLoad
            fZoneHeatGainInfiltration = 0.0

          else

            fZoneHeatLossInfiltration = 0.0
            fZoneHeatGainInfiltration = ABS( fZoneInfiltrationLoad )

          endif INF_is_flux_positive

        elseif ( bZoneHeated(iZone) ) then

          fZoneHeatLossInfiltration =fZoneInfiltrationLoad
          fZoneHeatGainInfiltration = 0.0

        elseif ( bZoneCooled(iZone) ) then

          fZoneHeatLossInfiltration = 0.0
          fZoneHeatGainInfiltration = ABS( fZoneInfiltrationLoad )

        else

          fZoneHeatLossVentilation = 0.0
          fZoneHeatGainVentilation = 0.0

        endif INF_is_zone_heated

C.....Collect zone air-changes/hour
      fZoneInfiltrationACH = AIM2_infil_to_zones(iZone) / VOL(iZone)
     &                          * 3600.0


C-----------------------------------------------------------------------
C     Compute external heat loss/heat gain metrics by examining all
C     surfaces.
C-----------------------------------------------------------------------
      zone_surface_loop: do iSurface = 1, nzsur(iZone)

C.......Pad surface index to 'XX'
        if ( iSurface .gt. 9 ) then
          write (cSurf_Chars,'(I2)') iSurface
        else
          write (cSurf_Chars,'(A,I1)') '0', iSurface
        endif
C.......Create base name for surface values output
        H3K_rep_NAME = cBuildingName  
     &              // cSep // zname(iZone)(1:lnzname(iZone))
     &              // cSep // cSurfaceRootName // cSurf_Chars

        iH3K_NameLen = 3+1+lnzname(iZone)+1+3

c        iH3K_NameLen=lnblnk(H3K_rep_NAME)

C-----------------------------------------------------------------------
C       Report total short wave radiation on this surface.
C-----------------------------------------------------------------------
        call add_to_xml_reporting (
     &          qswrde(iZone,iSurface,2),
     &          H3K_rep_NAME(1:iH3K_NameLen) 
     &            // '/heat_flux/radiation/shortwave',
     &          'units', '(W)',
     &          'Short-wave solar radiation'
     &           )

        call add_to_xml_reporting (
     &          qswrde(iZone,iSurface,2)/sna(iZone,iSurface),
     &          H3K_rep_NAME(1:iH3K_NameLen) 
     &            // '/heat_flux/radiation/'
     &            //  'shortwave_m2' ,
     &          'units', '(W/m2)',
     &          'Short-wave solar radiation per unit area'
     &          )     

C-----------------------------------------------------------------------
C       Estimate heat loss from surface by performing energy
C       balance on exterior surface node. Equation depends on
C       whether surface is transparent.
C-----------------------------------------------------------------------
        check_for_window: if ( itmcfl( iZone, iSurface ) == 0 ) then

C-----------------------------------------------------------------------
C       Opaque surface:
C       Estimate heat loss from surface, based on flux through
C       outermost node of envelope assembly
C
C                  Conductivity
C           LOSS = ------------  * Surface area * Temperature difference
C                   Thickness
C
C           Thickness = layer thickness / number of nodes
C
C      ( Positive values indicate heat loss. )
C
C-----------------------------------------------------------------------


C.........Conductivity may be temperature dependent.
          temp_dependent: if ( ivthpz( iSurface ) ) then

            fConductivity = ( THCONf(iSurface,1) + THCONf(iSurface,2) )
     &                        / 2.0

          else

            fConductivity = THrMlI(iZone,iSurface,1,1)

          endif temp_dependent


C.........Compute thickness
          fThickness = THrMlI( iZone, iSurface, 1, 4 )
     &                   / float ( NNDL( iZone, iSurface, 1 ) )

C.........Compute flux
          fSurface_balance_net =   fConductivity
     &                         / fThickness
     &                         * sna( iZone, iSurface )
     &                         * ( TFc( iZone ,iSurface, 2 ) -
     &                             TFc( iZone, iSurface, 1 )
     &                            )

        else

C-----------------------------------------------------------------------
C         Transparent surfaces:
C
C           net heat loss =   heat transfer by convection (qconve)
C                           + short-wave radiation exchange (qswrde)
C                           + long-wave radiation exhange with
C                                 buildings (qlwrbd)
C                           + long-wave radiation exhange with
C                                 ground (qlwrde)
C                           + long-wave radiation exhange with
C                                 sky (qlwrsk)
C
C         ( Multiply by -1 to convert from ESP-r to HOT3000 sign
C           conventions. Positive values of fSurface_balance_net 
C           indicate heat transfer from the building to the surroundings.
C-----------------------------------------------------------------------

          fSurface_balance_net =
     &      ( qconve(iZone,iSurface,2)
     &        + qlwrde(iZone,iSurface,2)
     &        + qlwrbd(iZone,iSurface,2)
     &        + qlwrsk(iZone,iSurface,2)
     &        + qswrde(iZone,iSurface,2) ) * (-1.0)


        endif check_for_window
        fZoneSurfaceFlux = fZoneSurfaceFlux + fSurface_balance_net

C-----------------------------------------------------------------------
C       Determine the boundary condition for the surface; check
C       value of IE common block.
C-----------------------------------------------------------------------
        surf_BCs: select case ( IE (iZone, iSurface ) )

          case ( iBCExternal )

C...........Above-grade heat loss
            call add_to_xml_reporting (
     &          fSurface_balance_net,
     &    H3K_rep_Name(1:iH3K_NameLen) // '/heat_flux/above_grade/net',
     &          'units', '(W)',
     &          'Heat transfer through building envelope '
     &          //'(above-grade)' )


C...........Append above grade heat loss to running total
            fZoneHeatFluxAboveGrade = fZoneHeatFluxAboveGrade +
     &                                fSurface_balance_net

            if ( itmcfl( iZone, iSurface ) == 0 ) then
C.............Read surface type from iSurfOreint array
              iSurfType = iSurfOrient( iZone, iSurface )
            else
C.............Surface is transparent
              iSurfType = iSurfWindow
            endif

          case ( iBCInternalConst, iBCInternalSame )

C...........Flux to known conditions
            call add_to_xml_reporting (
     &          fSurface_balance_net,
     &          H3K_rep_Name(1:iH3K_NameLen) 
     &                       // '/heat_flx/to_spec_BCs/net',
     &          'units', '(W)',
     &          'Heat loss to specified boundary conditions' )

            iSurfType = iSurfOrient( iZone, iSurface )

          case ( iBCAdiabatic )

C...........Surface has zero heat transfer
            iSurfType = iSurfOrient( iZone, iSurface )

          case ( iBCGround, iBCBASESIMP )

C...........Below-grade heat loss
            call add_to_xml_reporting (
     &          fSurface_balance_net,
     &    H3K_rep_Name(1:iH3K_NameLen) // '/heat_flux/below_grade/net',
     &          'units', '(W)',
     &          'Heat transfer through building envelope '
     &          //'(below-grade)' )

            iSurfType = iSurfFoundation

          case ( iBCCEN13917 )

C...........Heat transfer to CEN partition
            call add_to_xml_reporting (
     &          fSurface_balance_net,
     &          H3K_rep_Name(1:iH3K_NameLen) 
     &                           // '/heat_flux/CEN_partition/net',
     &          'units', '(W)',
     &          'Heat transfer through CEN 13917 partition' )

            iSurfType = iSurfOrient( iZone, iSurface )

          case default
C...........Internal partition
            call add_to_xml_reporting (
     &          fSurface_balance_net,
     &          H3K_rep_Name(1:iH3K_NameLen) //
     &          '/heat_flux/above_grade/net' ,
     &          'units', '(W)',
     &          'Heat transfer through building envelope '
     &          //'(above-grade)' )

            iSurfType = iSurfOrient( iZone, iSurface )

        end select surf_BCs

C-----------------------------------------------------------------------
C       Determine if flux is loss/gain
C-----------------------------------------------------------------------
        is_zone_heated:
     &  if ( bZoneHeated(iZone) .and. bZoneCooled(iZone) ) then

          is_flux_positive:
     &    if ( fSurface_balance_net > 0.) then

            fSurLoss = fSurface_balance_net
            fSurGain = 0.0

          else

            fSurLoss = 0.0
            fSurGain = ABS( fSurface_balance_net )

          endif is_flux_positive

        elseif ( bZoneHeated(iZone) ) then

          fSurLoss = fSurface_balance_net
          fSurGain = 0.0

        elseif ( bZoneCooled(iZone) ) then

          fSurLoss = 0.0
          fSurGain = ABS( fSurface_balance_net )

        else

          fSurLoss = 0.0
          fSurGain = 0.0

        endif is_zone_heated
C-----------------------------------------------------------------------
C       Append flux to zone/building total
C-----------------------------------------------------------------------

        check_surface_type: select case ( iSurfType )

          case ( iSurfFloor )

            fZoneHeatLossFloor = fZoneHeatLossFloor + fSurLoss
            fZoneHeatGainFloor = fZoneHeatGainFloor + fSurGain
            fSUMHeatLossFloor  = fSUMHeatLossFloor  + fSurLoss
            fSUMHeatGainFloor  = fSUMHeatGainFloor  + fSurGain

          case ( iSurfCeiling )

            fZoneHeatLossCeiling =  fZoneHeatLossCeiling + fSurLoss
            fZoneHeatGainCeiling =  fZoneHeatGainCeiling + fSurGain
            fSUMHeatLossCeiling  =  fSUMHeatLossCeiling  + fSurLoss
            fSUMHeatGainCeiling  =  fSUMHeatGainCeiling  + fSurGain

          case ( iSurfWindow )

            fZoneHeatLossWindows = fZoneHeatLossWindows + fSurLoss
            fZoneHeatGainWindows = fZoneHeatGainWindows + fSurGain
            fSUMHeatLossWindows  = fSUMHeatLossWindows  + fSurLoss
            fSUMHeatGainWindows  = fSUMHeatGainWindows  + fSurGain

          case ( iSurfWall )

            fZoneHeatLossWalls = fZoneHeatLossWalls + fSurLoss
            fZoneHeatGainWalls = fZoneHeatGainWalls + fSurGain
            fSUMHeatLossWalls  = fSUMHeatLossWalls  + fSurLoss
            fSUMHeatGainWalls  = fSUMHeatGainWalls  + fSurGain

          case ( iSurfFoundation )

            fZoneHeatLossBelowGrade = fZoneHeatLossBelowGrade + fSurLoss
            fZoneHeatGainBelowGrade = fZoneHeatGainBelowGrade + fSurGain
            fSUMHeatLossBelowGrade  = fSUMHeatLossBelowGrade  + fSurLoss
            fSUMHeatGainBelowGrade  = fSUMHeatGainBelowGrade  + fSurGain

          case default

C...........Unknown surface type.

        end select check_surface_type

C-----------------------------------------------------------------------
C       Report internal and external surface temperatures
C-----------------------------------------------------------------------

C........External facing surfaces -> Surface temperature
C         if (IE(iZone,iSurface) .eq. 0) THEN ! the surface faces external
            call add_to_xml_reporting (
     &          tfc(iZone,iSurface,1),
     &          H3K_rep_Name(1:iH3K_NameLen) // '/temp/outside',
     &          'units', '(oC)',
     &          'Temperature on external surface facing "exterior" ' )
C         Endif ! if ie = 0

         call add_to_xml_reporting (
     &          tfs(iZone,iSurface),
     &          H3K_rep_Name(1:iH3K_NameLen) // '/temp/inside',
     &          'units', '(oC)',
     &          'Temperature on internal surface' )

C node temperature for node "nndc" is zero for whatever reasons ... (???)
C         call add_to_xml_reporting (
C     &          tfc(iZone,iSurface,nndc(iZone,iSurface)),
C     &          H3K_rep_Name(1:iH3K_NameLen) // '/tfc/inside',
C     &          'units', '(oC)',
C     &          'Node temperature on internal surface' )

      enddo zone_surface_loop

C-----------------------------------------------------------------------
C     Now determine if flux through walls is a heating or cooling load:
C-----------------------------------------------------------------------

      fZoneHeatFlux =   fZoneSurfaceFlux
     &                + fZoneInfiltrationLoad
     &                + fZoneVentilationLoad

C-----------------------------------------------------------------------
C     Compute solar gain metrics
C-----------------------------------------------------------------------

C.....Zone solar gains:   solar entering from outside (q1outs)
C                       - solar escaping back to outside (q2lost)
C                       + solar from other zones (q1adjz)
C                       - solar to other zones  (q2adjz)
      fZoneSolarGains =   q1outs(iFuture) - q2lost(iFuture)
     &                  + q1adjz(iFuture) - q2adjz(iFuture)

C.....Internal gains: sum of convective and radiative gains
      fZoneInternalGains = Cas_Rad_dat(iZone) + Cas_Con_dat(iZone)


C-----------------------------------------------------------------------
C     Use Sandars and Barakat (1983) method for estimating the
C     useful solar gain (that is, the solar gains coincident with
C     heating loads). A solar gain is useful if ESP-r's ventilation
C     controls have not opened windows, or in the case of models
C     without air-flow networks, the zone temperature is not greater
C     than the thermostat setpoint + a pre-determined 'swing temperature'
C     (above which Sandars and Barakat assume the building will be
C     ventilated).
C-----------------------------------------------------------------------

      are_gains_useful:
     &if ( .not. bWindowsOpen .and.
     &           bZoneHeated(iZone)  .and.
     &     TFA(iZone) <= fHeatSetpoint(iZone) + fSwingTemperature ) then

        fZoneUsefulSolarGains    = fZoneSolarGains
        fZoneUsefulInternalGains = fZoneInternalGains

C Calculate these variables also per zone floor area        
C         fm2ZoneUsefulSolarGains    = fZoneSolarGains/zbasea(iZone)
C         fm2ZoneUsefulInternalGains = fZoneInternalGains/zbasea(iZone)
      else

        fZoneUsefulSolarGains    = 0.0
        fZoneUsefulInternalGains = 0.0
C         fm2ZoneUsefulSolarGains  = 0.0
C         fm2ZoneUsefulInternalGains =0.0
      endif are_gains_useful

C.....Set flag if zone is heated.
      if ( bZoneHeated(iZone) ) bBuildingHeated = .true.

C-----------------------------------------------------------------------
C     And log occurance with adverse solar gains --- those coincident
C     with cooling loads
C-----------------------------------------------------------------------
      are_gains_adverse:
     &if ( bZoneCooled(iZone) ) then

        fZoneAdverseSolarGains    = fZoneSolarGains
        fZoneAdverseInternalGains = fZoneInternalGains

      else

        fZoneAdverseSolarGains    = 0.0
        fZoneAdverseInternalGains = 0.0

      endif are_gains_adverse

C=======================================================================
C     Compute heating and cooling loads:
C=======================================================================

      fZoneHeatLoad =   Q_Heat
     &                + fZoneUsefulSolarGains
     &                + fZoneUsefulInternalGains

      fZoneCoolLoad =   Q_Cool
     &                + fZoneAdverseSolarGains    ! ? added ag@10042010
     &                + fZoneAdverseInternalGains ! ? added ag@10042010


C=======================================================================
C     Append zone results to running total for entire building.
C=======================================================================

C.....Update total building heat injection/extraction variables
      fSUMHeatInjection  = fSUMHeatInjection  + Q_Heat
      fSUMHeatExtraction = fSUMHeatExtraction + Q_Cool
      fSUMNetFlux        = fSUMNetFlux        + Q_Zone

C.....Solar gains
      fSUMSolarGains = fSUMSolarGains + fZoneSolarGains

      fSUMUsefulSolarGains =   fSUMUsefulSolarGains
     &                       + fZoneUsefulSolarGains

      fSUMAdverseSolarGains =   fSUMAdverseSolarGains
     &                        + fZoneAdverseSolarGains

C.....Thermal loads

      fSUMNetLoad  = fSUMNetLoad  + fZoneHeatLoad - fZoneCoolLoad
      fSUMHeatLoss = fSUMHeatLoss + fZoneHeatLoad
      fSUMHeatGain = fSUMHeatGain + fZoneCoolLoad


C.....Air load
      fSUMVentilationLoad  = fSUMVentilationLoad + fZoneVentilationLoad
      fSUMInfiltrationLoad =   fSUMInfiltrationLoad
     &                       + fZoneInfiltrationLoad
      fSUMHeatLossInfiltration =   fSUMHeatLossInfiltration
     &                           + fZoneHeatLossInfiltration
      fSUMHeatLossVentilation =    fSUMHeatLossVentilation
     &                           + fZoneHeatLossVentilation
      fSUMHeatGainInfiltration =   fSUMHeatGainInfiltration
     &                           + fZoneHeatGainInfiltration
      fSUMHeatGainVentilation =    fSUMHeatGainVentilation
     &                           + fZoneHeatGainVentilation

C.....Internal gains
      fSUMInternalGains = fSUMInternalGains + fZoneInternalGains

      fSUMUsefulInternalGains =   fSUMUsefulInternalGains
     &                          + fZoneUsefulInternalGains

      fSUMAdverseInternalGains =   fSUMAdverseInternalGains
     &                           + fZoneAdverseInternalGains
C 
C C Calculate loads per m2
C       fm2ZoneHeatLoad=fZoneHeatLoad/zbasea(iZone)
C       fm2ZoneCoolLoad=fZoneCoolLoad/zbasea(iZone)
C       
C       fm2SUMNetLoad=fm2SUMNetLoad/zbasea(iZone)
C       fm2SUMHeatLoss=fm2SUMHeatLoss/zbasea(iZone)
C       fm2SUMHeatGain=fm2SUMHeatGain/zbasea(iZone)
C       fm2SUMHeatLossInfiltration=fm2SUMHeatLossInfiltration/
C      &  zbasea(iZone)      
C       fm2SUMHeatGainInfiltration=fm2SUMHeatGainInfiltration/
C      &  zbasea(iZone)
C       fm2SUMInternalGains=fm2SUMInternalGains/zbasea(iZone)
C       fSUMUsefulInternalGains=fm2SUMUsefulInternalGains/zbasea(iZone)    
C       fm2SUMAdverseInternalGains=fSUMAdverseInternalGains/zbasea(iZone) 
      
C=======================================================================
C
C     Pass data to H3KReports
C
C=======================================================================


C.....The following code is used to transport data to
C.....the H3K reporting object
C.....
C.....Any changes to this code should be made within
C.....the H3Kreports demarcaration comments
C.....Set format depending on number of zones

C.......Create base name for zone data output
        H3K_rep_NAME = cBuildingName  
     &              // cSep // zname(iZone)(1:lnzname(iZone))

        iH3K_NameLen = 3+1+lnzname(iZone)

C---------------------*****---------------------------------------------
C       Report heating and cooling setpoints if zone is controlled
C---------------------*****---------------------------------------------

      if (icascf(iZone).ne.0) then
      ! Heating
        call add_to_xml_reporting (
     &    fHeatSetpoint(iZone),
     &    H3K_rep_NAME(1:iH3K_NameLen) 
     &     // '/ctl/heatsetpt',
     &     'units', '(°C)',
     &     'zone control heating setpoint'
     &  )

      ! Cooling
        call add_to_xml_reporting (
     &    fCoolSetpoint(iZone),
     &    H3K_rep_NAME(1:iH3K_NameLen) 
     &     // '/ctl/coolsetpt',
     &     'units', '(°C)',
     &     'zone control cooling setpoint'
     &  )
      endif

C-----------------------------*****-------------------------------------
C       Report casual gains by type.
C       Code logic is taken from /esrubld/casual.F and /esrubld/blibsv.F
C       Code is kind of duplicated ...
C       SHOCC loads are disregarded (?)
C-----------------------------*****-------------------------------------

C Check if ICASUAL(), ICASUAL3() or IACTIV() is set. If yes, 
C write casual data. Else (casual data via .opr) check for period id
C and if in current period write casual data
        if (ICASUAL(iZone).ne.0 .or. ICASUAL3(iZone).ne.0 
     &       .or. IACTIV(iZone).ne.0) then
		! Occupants
          call add_to_xml_reporting (
     &      ctlperocupc(iZone,1)/zbasea(iZone), ! convective
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Occupt/con',
     &       'units', '(W/m2)',
     &       'convective gain'
     &    )
          call add_to_xml_reporting (
     &      ctlperocupr(iZone,1)/zbasea(iZone), ! radiative
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Occupt/rad',
     &       'units', '(W/m2)',
     &       'radiative gain'
     &    )
          call add_to_xml_reporting (
     &      ctlperocupl(iZone,1)/zbasea(iZone),  ! latent
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Occupt/lat',
     &       'units', '(W/m2)',
     &       'latent gain'
     &    )
        ! Lights (no latent)
          call add_to_xml_reporting (
     &      ctlperlightc(iZone,1)/zbasea(iZone), ! convective
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Lights/con',
     &       'units', '(W/m2)',
     &       'convective gain'
     &    )
          call add_to_xml_reporting (
     &      ctlperlightr(iZone,1)/zbasea(iZone), ! radiative
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Lights/rad',
     &       'units', '(W/m2)',
     &       'radiative gain'
     &    )
        ! Equipment (possible latent for plants etc.)
          call add_to_xml_reporting (
     &      ctlperequipc(iZone,1)/zbasea(iZone), ! convective
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Equipt/con',
     &       'units', '(W/m2)',
     &       'convective gain'
     &    )
          call add_to_xml_reporting (
     &      ctlperequipr(iZone,1)/zbasea(iZone), ! radiative
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Equipt/rad',
     &       'units', '(W/m2)',
     &       'radiative gain'
     &    )
          call add_to_xml_reporting (
     &      ctlperequipl(iZone,1)/zbasea(iZone),  ! latent
     &      H3K_rep_NAME(1:iH3K_NameLen) 
     &       // '/casual/Equipt/lat',
     &       'units', '(W/m2)',
     &       'latent gain'
     &    )

      else ! data in operations file
C     For each casual gain type get gain for current period.
        gain_type_loop: do IGN=1,3!MGTY

C Calculate the current period id.
          IPER=NCGPER(iZone,IDTY,IGN)

          period_id_loop: do iPID=1,NCGPER(iZone,IDTY,IGN)
            if (BTIMEF.le.TCGS(iZone,IDTY,IGN,iPID+1).and.
     &            BTIMEF.gt.TCGS(iZone,IDTY,IGN,iPID)) then
              IPER=iPID
            endif
          enddo period_id_loop

C Get future casual gain values if there is periods defined.
          if ((IPER.gt.0).and.(lodlabel(iZone,IGN)(1:1).ne.'')) then
            call add_to_xml_reporting (
     &        CGSENC(iZone,IDTY,IGN,IPER)*
     &                  CGCTL(iZone,2,IGN)/zbasea(iZone), ! convective
     &        H3K_rep_NAME(1:iH3K_NameLen) 
     &         // '/casual/' // lodlabel(iZone,IGN) // '/con',
     &         'units', '(W/m2)',
     &         'convective gain'
     &      )
            call add_to_xml_reporting (
     &        CGSENR(iZone,IDTY,IGN,IPER)*
     &                  CGCTL(iZone,2,IGN)/zbasea(iZone), ! radiative
     &        H3K_rep_NAME(1:iH3K_NameLen) 
     &         // '/casual/' // lodlabel(iZone,IGN) // '/rad',
     &         'units', '(W/m2)',
     &         'radiative gain'
     &      )
            call add_to_xml_reporting (
     &        CGLAT(iZone,IDTY,IGN,IPER)*
     &                 CGCTL(iZone,2,IGN)/zbasea(iZone),  ! latent
     &        H3K_rep_NAME(1:iH3K_NameLen) 
     &         // '/casual/' // lodlabel(iZone,IGN) // '/lat',
     &         'units', '(W/m2)',
     &         'latent gain'
     &      )
          endif
        enddo gain_type_loop
      endif

C--------------------------------***-----------------------------------
C Report infiltration rate
            call add_to_xml_reporting (
c     &        CIF(iZone)/(0.33*Vol(iZone)),  ! approx. ACH (O.K. for .opr infil, but off somewhat for .tdf. Also, hour fraction values not good)
     &        CVIF/(0.33*Vol(iZone)),  ! ACH (?)
     &        H3K_rep_NAME(1:iH3K_NameLen) 
     &         // '/infiltration',
     &         'units', '(ACH)',
     &         'infiltration rate'
     &      )
C-------------------------------*****----------------------------------

C.....Lighting zone illuminance
      if (nlitz2(iZone).gt.0) then
        lightingzone_loop: do iLtz = 1, nlitz2(iZone)
          write(cOne,'(I1)')iLtz
          call add_to_xml_reporting (
     &         AveLtzIllum(iZone,iLtz),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/ill_ltz' // cOne,
     &         'units', '(lux)',
     &         'Lighting zone average illuminance' )
        enddo lightingzone_loop
      endif

C.....Zone Resultant TEMPERATURE
      do 22 iSur=1,NZSUR(iZone)
         TS(iSur)=TFS(iZone,iSur)
 22   continue
 
      call MORESL(iZone,TFA(iZone),TS,YDUM,NZSUR(iZone),TRESL)

      call add_to_xml_reporting (
     &      TRESL,
     &      H3K_rep_Name(1:iH3K_NameLen)// '/resultant_temp',
     &      'units', '(oC)',
     &      'Zone resultant temperature' )
      
C.....AIR POINT TEMPERATURE

      call add_to_xml_reporting (
     &         TFA(iZone),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/air_point/temp',
     &         'units', '(oC)',
     &         'Zone air-point temperature' )

C .... ZONE DRY AIR MASS
      call add_to_xml_reporting (
     &         ZMDAF(iZone),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/air_point/mass',
     &         'units', '(kg)',
     &         'Zone dry air mass' )

C .... ZONE AIR MOISTURE CONTENT
      call add_to_xml_reporting (
     &         GFA(iZone),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/air_point/moisture',
     &         'units', '(kg/kg_dry-air)',
     &         'Zone air moisture content' )
     
C .... ZONE AIR NODE RELATIVE HUMIDITY    
      ZRH=PCRH2(TFA(iZone),GFA(iZone),PATMOS)
      call add_to_xml_reporting (
     &         ZRH,
     &         H3K_rep_Name(1:iH3K_NameLen)// '/air_point/rh',
     &         'units', '(%)',
     &         'Zone air node relative humidity' )

C .... ZONE AIR NODE PARTIAL VAPOUR PRESSURE
      call add_to_xml_reporting (
C     &         Pfair(iZone),
     &         ZRH/100.0*Psat01(TFA(iZone)),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/air_point/pvap',
     &         'units', '(Pa)',
     &         'Zone air node partial vapour pressure' )

C.....Zone air saturation temperature
      call add_to_xml_reporting (
     &         DEWPT(HUMR(TFA(iZone),ZRH,PATMOS),PATMOS),
     &         H3K_rep_NAME(1:iH3K_NameLen)// '/air_point/tsat',
     &         'units', '(oC)',
     &         'Zone air saturation temperature' )

C.....ZONE HEAT INJECTION / EXTRACTION

      call add_to_xml_reporting (
     &         Q_ZONE,
     &          H3K_rep_Name(1:iH3K_NameLen)//'/supplied_energy/net',
     &         'units', '(W)',
     &         'Zone net heat injection/extraction ' )

      call add_to_xml_reporting (
     &         Q_Heat,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/supplied_energy/heating',
     &         'units', '(W)',
     &         'Zone net heat injection ' )

      call add_to_xml_reporting (
     &         Q_cool,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/supplied_energy/cooling',
     &         'units', '(W)',
     &         'Zone net heat extraction ' )
     
C Convert heat injection/extraction metrics to "per m2 of floor area"
      Qm2_ZONE=Q_ZONE/zbasea(iZone)
      Qm2_Heat=Q_Heat/zbasea(iZone)
      Qm2_cool=Q_cool/zbasea(iZone)
      call add_to_xml_reporting (
     &         Qm2_ZONE,
     &          H3K_rep_NAME(1:iH3K_NameLen)
     &                                //'/supplied_energy/net_Perm2',
     &         'units', '(W/m2)',
     &         'Zone net heat injection/extraction ' )

      call add_to_xml_reporting (
     &         Qm2_Heat,
     &         H3K_rep_NAME(1:iH3K_NameLen)
     &                          //'/supplied_energy/heating_Perm2',
     &         'units', '(W/m2)',
     &         'Zone net heat injection ' )

      call add_to_xml_reporting (
     &         Qm2_cool,
     &         H3K_rep_NAME(1:iH3K_NameLen)
     &                          //'/supplied_energy/cooling_Perm2',
     &         'units', '(W/m2)',
     &         'Zone net heat extraction ' )

C.....Total heating and cooling loads
      call add_to_xml_reporting (
     &         fZoneHeatLoad,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                                 //'/thermal_loads/heating/total',
     &         'units', '(W)',
     &         'Total heating load ( heat loss trough envelope + '
     &         // ' ventilation and infiltration loads )' )

      call add_to_xml_reporting (
     &         fZoneCoolLoad,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                         //'/thermal_loads/cooling/total',
     &         'units', '(W)',
     &         'Total cooling load ( heat gain trough envelope + '
     &         // ' ventilation and infiltration loads )' )

      call add_to_xml_reporting (
     &         fZoneHeatFlux,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/thermal_loads/net_load',
     &         'units', '(W)',
     &         'Net load ( heat gain/loss trough envelope + '
     &         // ' ventilation and infiltration loads )' )

C Conver Total heating and cooling loads to  "per m2 of floor area"
      fm2ZoneHeatLoad=fZoneHeatLoad/zbasea(iZone)
      fm2ZoneCoolLoad=fZoneCoolLoad/zbasea(iZone)
      fm2ZoneHeatFlux=fZoneHeatFlux/zbasea(iZone)
      call add_to_xml_reporting (
     &         fm2ZoneHeatLoad,
     &         H3K_rep_NAME(1:iH3K_NameLen)
     &                  //'/thermal_loads/heating/total_Perm2',
     &         'units', '(W/m2)',
     &         'Total heating load ( heat loss trough envelope + '
     &         // ' ventilation and infiltration loads )' )

      call add_to_xml_reporting (
     &         fm2ZoneCoolLoad,
     &         H3K_rep_NAME(1:iH3K_NameLen)
     &                  //'/thermal_loads/cooling/total_Perm2',
     &         'units', '(W/m2)',
     &         'Total cooling load ( heat gain trough envelope + '
     &         // ' ventilation and infiltration loads )' )

      call add_to_xml_reporting (
     &         fm2ZoneHeatFlux,
     &         H3K_rep_NAME(1:iH3K_NameLen)
     &                      //'/thermal_loads/net_load_Perm2',
     &         'units', '(W/m2)',
     &         'Net load ( heat gain/loss trough envelope + '
     &         // ' ventilation and infiltration loads )' )

C.....Envelope heat loss
      call add_to_xml_reporting (
     &         fZoneHeatLossWindows,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                       //'/envelope/windows/heat_loss',
     &         'units', '(W)',
     &         'Contribution of windows to heating load' )

      call add_to_xml_reporting (
     &         fZoneHeatLossWalls,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                            //'/envelope/walls/heat_loss',
     &         'units', '(W)',
     &         'Contribution of walls to heating load' )

      call add_to_xml_reporting (
     &         fZoneHeatLossFloor,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                            //'/envelope/floors/heat_loss',
     &         'units', '(W)',
     &         'Contribution of overhanging floors to heating load' )


      call add_to_xml_reporting (
     &         fZoneHeatLossCeiling,
     &       H3K_rep_Name(1:iH3K_NameLen)
     &                         //'/envelope/ceilings/heat_loss',
     &         'units', '(W)',
     &         'Contribution of Ceilings to heating load' )

      call add_to_xml_reporting (
     &         fZoneHeatLossBelowGrade,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                            //'/envelope/foundation/heat_loss',
     &         'units', '(W)',
     &         'Contribution of foundation to heating load' )

      call add_to_xml_reporting (
     &         fZoneHeatLossInfiltration,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &            // '/envelope/infiltration/heat_loss',
     &         'units', '(W)',
     &         'Contribution of infiltration to heating load' )

      call add_to_xml_reporting (
     &         fZoneHeatLossWalls
     &           + fZoneHeatLossInfiltration
     &           + fZoneHeatLossCeiling
     &           + fZoneHeatLossFloor
     &           + fZoneHeatLossWindows
     &           + fZoneHeatLossBelowGrade,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &            // '/envelope/all_components/heat_loss',
     &         'units', '(W)',
     &         'Heat lost through all components of envelope' )

C.....Zone air change rate (AIM2)
      call add_to_xml_reporting (
     &         fZoneInfiltrationACH,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/envelope/infiltration/'
     &                           //'air_changes_per_hour',
     &         'units', '(ACH)',
     &         'Zone infiltration (air changes per hour)')

! Not sure if this is being computed correctly
!       call add_to_xml_reporting (
!      &         fZoneHeatLossVentilation,
!      &         H3K_rep_Name(1:iH3K_NameLen)
!      &            //'/envelope/ventilation/heat_loss',
!      &         'units', '(W)',
!      &         'Contribution of ventilation to heating load' )

C.....Envelope heat gains
      call add_to_xml_reporting (
     &         fZoneHeatGainWindows,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                //'/envelope/windows/heat_gain',
     &         'units', '(W)',
     &         'Contribution of windows to cooling load' )

      call add_to_xml_reporting (
     &         fZoneHeatGainWalls,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &               //'/envelope/walls/heat_gain',
     &         'units', '(W)',
     &         'Contribution of walls to cooling load' )

      call add_to_xml_reporting (
     &         fZoneHeatGainFloor,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                //'/envelope/floors/heat_gain',
     &         'units', '(W)',
     &         'Contribution of overhanging floors to cooling load' )

      call add_to_xml_reporting (
     &         fZoneHeatGainCeiling,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                    //'/envelope/ceilings/heat_gain',
     &         'units', '(W)',
     &         'Contribution of ceilings to cooling load' )

      call add_to_xml_reporting (
     &         fZoneHeatGainBelowGrade,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                   //'/envelope/foundation/heat_gain',
     &         'units', '(W)',
     &         'Contribution of foundation to cooling load' )

      call add_to_xml_reporting (
     &         fZoneHeatGainInfiltration,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &            // '/envelope/infiltration/heat_gain',
     &         'units', '(W)',
     &         'Contribution of infiltration to cooling load' )

      call add_to_xml_reporting (
     &         fZoneHeatGainWalls
     &           + fZoneHeatGainInfiltration
     &           + fZoneHeatGainCeiling
     &           + fZoneHeatGainFloor
     &           + fZoneHeatGainWindows
     &           + fZoneHeatGainBelowGrade,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &            // '/envelope/all_components/heat_gain',
     &         'units', '(W)',
     &         'Heat gained through all components of envelope' )

C.....Envelope: net flux
      call add_to_xml_reporting (
     &         fZoneHeatLossWindows-fZoneHeatGainWindows,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                           //'/envelope/windows/net_flux',
     &         'units', '(W)',
     &         'Contribution of windows to net heat flux' )


      call add_to_xml_reporting (
     &         fZoneHeatLossWalls-fZoneHeatGainWalls,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                   //'/envelope/walls/net_flux',
     &         'units', '(W)',
     &         'Contribution of walls to net heat flux' )

      call add_to_xml_reporting (
     &         fZoneHeatLossFloor-fZoneHeatGainFloor,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                      //'/envelope/floors/net_flux',
     &         'units', '(W)',
     &         'Contribution of overhanging floors to net heat flux' )

      call add_to_xml_reporting (
     &         fZoneHeatLossCeiling-fZoneHeatGainCeiling,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                   //'/envelope/ceilings/net_flux',
     &         'units', '(W)',
     &         'Contribution of ceilings to net heat flux' )

      call add_to_xml_reporting (
     &         fZoneHeatLossBelowGrade-fZoneHeatGainBelowGrade,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &                          //'/envelope/foundation/net_flux',
     &         'units', '(W)',
     &         'Contribution of foundation to net heat flux' )

      call add_to_xml_reporting (
     &         fZoneHeatLossInfiltration-fZoneHeatGainInfiltration,
     &         H3K_rep_Name(1:iH3K_NameLen)
     &            // '/envelope/infiltration/net_flux',
     &         'units', '(W)',
     &         'Contribution of infiltration to net heat flux' )

      call add_to_xml_reporting (
     &         fZoneHeatLossWalls
     &           + fZoneHeatLossInfiltration
     &           + fZoneHeatLossCeiling
     &           + fZoneHeatLossFloor
     &           + fZoneHeatLossWindows
     &           + fZoneHeatLossBelowGrade
     &           - ( fZoneHeatGainWalls
     &              + fZoneHeatGainInfiltration
     &              + fZoneHeatGainCeiling
     &              + fZoneHeatGainFloor
     &              + fZoneHeatGainWindows
     &              + fZoneHeatGainBelowGrade
     &             ),
     &         H3K_rep_Name(1:iH3K_NameLen)
     &            // '/envelope/all_components/net_flux',
     &         'units', '(W)',
     &         'Net flux through all components of envelope' )


C.....Zone solar gains
      call add_to_xml_reporting (
     & fZoneSolarGains,
     & H3K_rep_Name(1:iH3K_NameLen)//'/insolation/total',
     & 'units', '(W)',
     & 'Solar gains admitted through glazing '
     & )

C.....Zone solar gains coinciding with heating loads
      call add_to_xml_reporting (
     & fZoneUsefulSolarGains,
     & H3K_rep_Name(1:iH3K_NameLen)//'/insolation/useful',
     & 'units', '(W)',
     & 'Solar gains admitted through glazing that supplant heating '
     & // 'loads')

C C.....Zone solar gains coinciding with heating loads per m2 of floor area
C       call add_to_xml_reporting (
C      & fm2ZoneUsefulSolarGains,
C      & H3K_rep_NAME(1:16)//'/insolation/useful_per_m2',
C      & 'units', '(W/m2)',
C      & 'Solar gains admitted through glazing that supplant heating '
C      & // 'loads')

C.....Zone solar gains coinciding with heating loads !cooling loads?
      call add_to_xml_reporting (
     & fZoneAdverseSolarGains,
     & H3K_rep_Name(1:iH3K_NameLen)//'/insolation/adverse',
     & 'units', '(W)',
     & 'Solar gains admitted through glazing that increase cooling '
     & // 'loads')

C.....ARTIFICIAL LIGHTING GAIN FRACTION

      if (ICGCTL(iZone).gt.0) then
        call add_to_xml_reporting (
     &         CGCTL(iZone,2,ICGCTL(iZone)),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/lighting/frac',
     &         'units', '(-)',
     &         'Zone lighting gain fraction (controlled) FRAC' )
      endif

C.....TOTAL CASUAL GAINS
      call add_to_xml_reporting (
     &         fZoneInternalGains,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/internal_gains/total',
     &         'units', '(W)',
     &         'Internal heat gains'
     &             // '(convective + radiant)' )

C.....Useful internal gains
      call add_to_xml_reporting (
     &         fZoneUsefulInternalGains,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/internal_gains/useful',
     &         'units', '(W)',
     &         'Internal heat gains'
     &         // '(convective + radiant) that supplant heating loads' )

C C.....Useful internal gains per m2
C       call add_to_xml_reporting (
C      &         fm2ZoneUsefulInternalGains,
C      &         H3K_rep_NAME(1:16)//'/internal_gains/useful_per_m2',
C      &         'units', '(W/m2)',
C      &         'Internal heat gains'
C      &         // '(convective + radiant) that supplant heating loads' )

C.....Adverse internal gains
      call add_to_xml_reporting (
     &         fZoneAdverseInternalGains,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/internal_gains/adverse',
     &         'units', '(W)',
     &         'Internal heat gains'
     &         // '(convective + radiant) that increase cooling loads' )

C-----------------------------------------------------------------------
C     Check zone number: if this is the last zone, dump out aggregated
C     building parameters.
C-----------------------------------------------------------------------

      report_whole_building: if ( iZone == ncomp ) then

C.......Create base name for whole building data output
        H3K_rep_NAME = cBuildingName//cSep//cAllZonesName
        iH3K_NameLen=lnblnk(H3K_rep_NAME)

C.......Total heating
        call add_to_xml_reporting (
     &   fSUMHeatInjection,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/supplied_energy/heating',
     &   'units', '(W)',
     &   'Total amount of heat supplied to the building (all zones).'
     &   )

C.......Total cooling
        call add_to_xml_reporting (
     &   fSUMHeatExtraction,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/supplied_energy/cooling',
     &   'units', '(W)',
     &   'Total amount of cooling supplied to the building (all zones).'
     &   )

C.......Net flux for building
        call add_to_xml_reporting (
     &   fSUMNetFlux,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/supplied_energy/net_flux',
     &   'units', '(W)',
     &   'Net heat/cooling supplied to '
     &   // 'the building (all zones).'
     &   )

C.......Total solar gains
        call add_to_xml_reporting (
     &   fSUMSolarGains,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/insolation/total',
     &   'units', '(W)',
     &   'Total solar gains admitted through glazing '
     &   // '(all zones).'
     &   )

C.......Useful solar gains
        call add_to_xml_reporting (
     &   fSUMUsefulSolarGains,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/insolation/useful',
     &   'units', '(W)',
     &   'Solar gains admitted through glazing that supplant heating'
     &   // ' loads (all zones).'
     &   )

C.......Adverse solar gains
        call add_to_xml_reporting (
     &   fSUMAdverseSolarGains,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/insolation/adverse',
     &   'units', '(W)',
     &   'Solar gains admitted through glazing that increase cooling'
     &   // ' loads (all zones).'
     &   )


C.....Heat loss disaggregated by envelope components
      call add_to_xml_reporting (
     &         fSUMHeatLossWindows,
     &    H3K_rep_Name(1:iH3K_NameLen)//'/envelope/windows/heat_loss',
     &         'units', '(W)',
     &         'Heat loss to surroundings through windows '
     &          // ' (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossWalls,
     &     H3K_rep_Name(1:iH3K_NameLen)//'/envelope/walls/heat_loss',
     &         'units', '(W)',
     &         'Heat loss to surroundings through walls (all zones)' )

      call add_to_xml_reporting (
     &         fSUMHeatLossFloor,
     &     H3K_rep_Name(1:iH3K_NameLen)//'/envelope/floors/heat_loss',
     &         'units', '(W)',
     &         'Heat loss to surroundings through overhanging floors'
     &         //' (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossCeiling,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/envelope/ceilings/heat_loss',
     &         'units', '(W)',
     &         'Heat loss to surroundings through ceilings'
     &         //' (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossBelowGrade,
     & H3K_rep_Name(1:iH3K_NameLen)//'/envelope/foundation/heat_loss',
     &         'units', '(W)',
     &         'Heat loss to ground through below-grade elements'
     &          // ' (all zones)  ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossInfiltration,
     & H3K_rep_Name(1:iH3K_NameLen)//'/envelope/infiltration/heat_loss',
     &         'units', '(W)',
     &         'Heat loss to surroundings via infiltration'
     &          // ' (all zones)  ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossWalls
     &           + fSUMHeatLossInfiltration
     &           + fSUMHeatLossCeiling
     &           + fSUMHeatLossFloor
     &           + fSUMHeatLossWindows
     &           + fSUMHeatLossBelowGrade,
     &         H3K_rep_Name(1:iH3K_NameLen)//
     &            '/envelope/all_components/heat_loss',
     &         'units', '(W)',
     &         'Heat lost through all components of envelope'
     &         // ' (all zones)' )

!       call add_to_xml_reporting (
!      &         fSUMHeatLossVentilation,
!      &         'building/all_zones/envelope/ventilation/heat_loss',
!      &         'units', '(W)',
!      &         'Heat loss to surroundings via ventilation'
!      &          // ' (all zones)  ' )

C.....Heat gain disaggregated by envelope components
      call add_to_xml_reporting (
     &         fSUMHeatGainWindows,
     &    H3K_rep_Name(1:iH3K_NameLen)//'/envelope/windows/heat_gain',
     &         'units', '(W)',
     &         'Heat gain from surroundings through windows'
     &         //' (all zones, excluding insolation) ' )

      call add_to_xml_reporting (
     &         fSUMHeatGainWalls,
     &      H3K_rep_Name(1:iH3K_NameLen)//'/envelope/walls/heat_gain',
     &         'units', '(W)',
     &         'Heat gain from surroundings through walls'
     &         //' (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatGainFloor,
     &     H3K_rep_Name(1:iH3K_NameLen)//'/envelope/floors/heat_gain',
     &         'units', '(W)',
     &         'Heat gain from surroundings through overhanging floors'
     &         //' (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatGainCeiling,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/envelope/ceilings/heat_gain',
     &         'units', '(W)',
     &         'Heat gain from surroundings through ceilings'
     &         //' (all zones) ' )


      call add_to_xml_reporting (
     &         fSUMHeatGainBelowGrade,
     & H3K_rep_Name(1:iH3K_NameLen)//'/envelope/foundation/heat_gain',
     &         'units', '(W)',
     &         'Heat gain from ground through below-grade elements'
     &          // ' (all zones)  ' )

      call add_to_xml_reporting (
     &         fSUMHeatGainInfiltration,
     &         H3K_rep_Name(1:iH3K_NameLen)//
     &                '/envelope/infiltration/heat_gain',
     &         'units', '(W)',
     &         'Heat gain from surroundings via infiltration'
     &          // ' (all zones)  ' )

!       call add_to_xml_reporting (
!      &         fSUMHeatGainVentilation,
!     &         H3K_rep_Name(1:iH3K_NameLen)//
!      &         '/building/all_zones/envelope/ventilation/heat_gain',
!      &         'units', '(W)',
!      &         'Heat gain from surroundings via ventilation'
!      &          // ' (all zones)  ' )

C.....Net flux disaggregated by envelope components
      call add_to_xml_reporting (
     &         fSUMHeatLossWindows-fSUMHeatGainWindows,
     &     H3K_rep_Name(1:iH3K_NameLen)//'/envelope/windows/net_flux',
     &         'units', '(W)',
     &         'Net heat loss to surroundings through windows '
     &          // ' (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossWalls-fSUMHeatGainWalls,
     &       H3K_rep_Name(1:iH3K_NameLen)//'/envelope/walls/net_flux',
     &         'units', '(W)',
     &         'Net heat loss to surroundings through walls'
     &         //' (all zones)' )

      call add_to_xml_reporting (
     &         fSUMHeatLossFloor-fSUMHeatGainFloor,
     &      H3K_rep_Name(1:iH3K_NameLen)//'/envelope/floors/net_flux',
     &         'units', '(W)',
     &         'Net heat loss to surroundings through overhanging'
     &         //' floors (all zones) ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossCeiling-fSUMHeatGainCeiling,
     &    H3K_rep_Name(1:iH3K_NameLen)//'/envelope/ceilings/net_flux',
     &         'units', '(W)',
     &         'Net heat loss to surroundings through ceilings'
     &         //' (all zones) ' )


      call add_to_xml_reporting (
     &         fSUMHeatLossBelowGrade-fSUMHeatGainBelowGrade,
     &  H3K_rep_Name(1:iH3K_NameLen)//'/envelope/foundation/net_flux',
     &         'units', '(W)',
     &         'Net heat loss to ground through below-grade elements'
     &          // ' (all zones)  ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossInfiltration-fSUMHeatGainInfiltration,
     &         H3K_rep_Name(1:iH3K_NameLen)//
     &            '/envelope/infiltration/net_flux',
     &         'units', '(W)',
     &         'Net heat loss to surroundings via infiltration'
     &          // ' (all zones)  ' )

      call add_to_xml_reporting (
     &         fSUMHeatLossWalls
     &           + fSUMHeatLossInfiltration
     &           + fSUMHeatLossCeiling
     &           + fSUMHeatLossFloor
     &           + fSUMHeatLossWindows
     &           + fSUMHeatLossBelowGrade
     &           - ( fSUMHeatLossWalls
     &               + fSUMHeatLossInfiltration
     &               + fSUMHeatLossCeiling
     &               + fSUMHeatLossFloor
     &               + fSUMHeatLossWindows
     &               + fSUMHeatLossBelowGrade
     &             ),
     &         H3K_rep_Name(1:iH3K_NameLen)//
     &                 '/envelope/all_components/net_flux',
     &         'units', '(W)',
     &         'Net heat lost through all components of envelope'
     &         // ' (all zones)' )


C.....Total heating and cooling loads
      call add_to_xml_reporting (
     &         fSUMHeatLoss,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/thermal_loads/heating/total',
     &         'units', '(W)',
     &         'Total heating load ( heat loss through envelope + '
     &         // ' ventilation and infiltration loads; all zones)' )

      call add_to_xml_reporting (
     &         fSUMHeatGain,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/thermal_loads/cooling/total',
     &         'units', '(W)',
     &         'Total cooling load ( heat gain through envelope + '
     &         // ' ventilation and infiltration loads; all zones )' )

      call add_to_xml_reporting (
     &         fSUMNetLoad,
     &         H3K_rep_Name(1:iH3K_NameLen)//'/thermal_loads/net',
     &         'units', '(W)',
     &         'Total net load ( heat gain through envelope + '
     &         // ' ventilation and infiltration loads; all zones )' )

C.....Internal gains
        call add_to_xml_reporting(
     &   fSUMInternalGains,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/internal_gains/total',
     &   'units', '(W)',
     &   'Sensible heat transfer from occupants, lights and equipment'
     &   // ' (all zones).'
     &   )

        call add_to_xml_reporting(
     &   fSUMUsefulInternalGains,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/internal_gains/useful',
     &   'units', '(W)',
     &   'Sensible heat transfer from occupants, lights and equipment'
     &   // ' that supplant heating loads (all zones).'
     &   )

        call add_to_xml_reporting(
     &   fSUMAdverseInternalGains,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/internal_gains/adverse',
     &   'units', '(W)',
     &   'Sensible heat transfer from occupants, lights and equipment'
     &   // ' that increase cooling loads (all zones).'
     &   )

C.....Net energy balance
        call add_to_xml_reporting(
     &   fSUMSolarGains
     &      + fSUMInternalGains
     &      + fSUMNetFlux
     &      - fSUMNetLoad,
     &   H3K_rep_Name(1:iH3K_NameLen)//'/energy_balance/net',
     &   'units', '(W)',
     &   'Energy balance in building (Gains + Plant interaction -'
     &   // ' Loads; all zones).'
     &   )

      endif report_whole_building

C---------------------------------------------------------------------
C     Loop through zone surfaces and MLC layers, reporting data 
C---------------------------------------------------------------------

      do iSurface = 1, nzsur(iZone)

C.......Note: there is some redundant code here: Achim's and Alex's
C.......contributions overlap in functionality, and they should
C.......be almagamated. Alex has adopted a slightly more efficient
C.......method of writing out the variable names, which the code
C.......below should also make use of.
         if ( iSurface .gt. 9 ) then
            write(cSurf_Chars, '(I2)') iSurface
         else
            write(cSurf_Chars, '(A,I1)') '0', iSurface
         endif

C.......Create base name for surface values output
        H3K_rep_NAME = cBuildingName  
     &              // cSep // zname(iZone)(1:lnzname(iZone))
     &              // cSep // cSurfaceRootName // cSurf_Chars

        iH3K_NameLen = 3+1+lnzname(iZone)+1+3

C........Radiant heat injection associated with plant containment.
         call add_to_xml_reporting (
     &      fSurface_radiant_gain(iZone,iSurface,iFuture),
     &      H3K_rep_NAME(1:iH3K_NameLen)//'/plant_containment_flux',
     &      'units', '(W)',
     &      'Plant containment radiant heat flux at surface' )

C.......CONVECTIVE COEFFICIENTS
C........Internal surface convective coefficient.
         call add_to_xml_reporting (
     &      HCIP(iZone,iSurface),
     &      H3K_rep_NAME(1:iH3K_NameLen)//'/HCi',
     &      'units', '(W/(m2 K))',
     &      'Internal side convective coefficient' )

C........Internal surface convective coefficient model index
         call add_to_xml_reporting (
     &      real(ICORI(iZone,iSurface)),
     &      H3K_rep_NAME(1:iH3K_NameLen)//'/ICORI',
     &      'units', '(-)',
     &      'Internal side convective model' )

C........External surface convective coefficient.
         call add_to_xml_reporting (
     &      HCOP(iZone,iSurface),
     &      H3K_rep_NAME(1:iH3K_NameLen)//'/HCe',
     &      'units', '(W/(m2 K))',
     &      'External side convective coefficient' )

C........External surface convective coefficient model index
         call add_to_xml_reporting (
     &      real(ICORE(iZone,iSurface)),
     &      H3K_rep_NAME(1:iH3K_NameLen)//'/ICORE',
     &      'units', '(-)',
     &      'External side convective model' )

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
            call add_to_xml_reporting (
     &         trsum,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/PRT',
     &         'units', '(oC)',
     &         'Plane radiant temperature on zone inside face' )

C........Emissivity weighted plane radiant temperature 
C            call add_to_xml_reporting (
C     &         wtrsum,
C     &      H3K_rep_NAME(1:iH3K_NameLen)//'/wPRT',
C     &         'units', '(oC)',
C     &         'Emissivity weighted plane radiant temp. (inside face)' )

C.......RADIATION HEAT TRANSFER COEFFICIENT
            call add_to_xml_reporting (
C     &         ZHRS(iZone,iSurface)/(TFS(iZone,iSurface)-trsum), ! <= problems with DT approx. zero !
C          Incropera et. al., page 10:
     &        EI(iZone,iSurface)*SBC*
     &            (TFS(iZone,iSurface)+trsum+2.0*273.15)*
     &            ((TFS(iZone,iSurface)+273.15)**2+(trsum+273.15)**2),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/HRi',
     &         'units', '(W/(m2 K))',
     &         'Radiant heat transfer coef. (inside face)' )

         ENDIF ! <- end if viewfactor file present

C calc mean surrounding temp of external surfaces
         if (IE(iZone,iSurface) .eq. 0) THEN ! the surfaces faces external
            trsum = B1(iZone,iSurface)*TSKY**4 +
     &                  C1(iZone,iSurface)*(GTF(1)+273.15)**4 +
     &                    A1(iZone,iSurface)*TBAVE**4
            trsum = trsum**0.25 - 273.15
C........->Report data
            call add_to_xml_reporting (
     &         trsum,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/AmbRT',
     &         'units', '(oC)',
     &         'Ambient radiant temperature for exterior surface' )

         endif ! <- end if surface faces external

C......=====================================================
C......NODE DATA (ignore nodes 1 and N, as these are output as "outside" and "inside")
         do iNode = 2, (nndc (iZone, iSurface )-1)
            if ( iNode .gt. 9 ) then
              write(cNode_Chars, '(I2)')  iNode
            else
              write(cNode_Chars, '(A,I1)') '0', iNode
            endif

C.......Create base name for node values output
        H3K_rep_NAME = cBuildingName  
     &              // cSep // zname(iZone)(1:lnzname(iZone))
     &              // cSep // cSurfaceRootName // cSurf_Chars
     &              //cSep//cNodeRootName//cNode_Chars

        iH3K_NameLen = 3+1+lnzname(iZone)+1+3+1+3

C...........MLC layer temperature 
            call add_to_xml_reporting (
     &         tfc(iZone,iSurface,iNode),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/temp',
     &         'units', '(oC)',
     &         'Temperature at node within multilayer construction' )

         enddo  ! <- end iNode = 2,N-1 of iSurface 1,N of iZone

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

C .... should be climate data:
      COMMON/atpres/PATMOS
      real PATMOS  ! atmospheric pressure - a constant, sadly

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
      character*3   cClimateName
      integer       iH3K_NameLen

      INTEGER LNBLNK

C----------------------------------------------------------------------     
C     References
C----------------------------------------------------------------------
      REAL HUMR     ! Function returning the humidity ratio
      REAL HUMRT1   ! Function returning the humidity ratio kg/kg (esrucom/psychro.F)
      REAL DEWPT    ! Function returning the saturation temperature
      real PSat01   ! Function returns the saturation vapour pressure at temperature T (esrubld/mstmsc.F)

C.....The following code is used to transport data to
C.....the H3K reporting object.

C.....Set climate data output base name
      cClimateName='clm'

C.......Set base name for climate data output
      H3K_rep_NAME = cClimateName
      iH3K_NameLen=lnblnk(H3K_rep_NAME)

C.....DIFFUSE HORIZONTAL RADIATION
      call add_to_xml_reporting (
     &         QFF,
     &         H3K_rep_NAME(1:iH3K_NameLen)//
     &         '/solar/diffuse_horizontal_radiation',
     &         'units', '(W/m2)',
     &         'Climate diffuse horizontal solar radiation' )

C.....DIRECT NORMAL RADIATION
      call add_to_xml_reporting (
     &         QDF,
     &         H3K_rep_NAME(1:iH3K_NameLen)//
     &         '/solar/direct_normal_radiation',
     &         'units', '(W/m2)',
     &         'Climate direct normal solar radiation' )

C.....AMBIENT TEMPERATURE
      call add_to_xml_reporting (
     &         TF,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/dry_bulb_temp',
     &         'units', '(oC)',
     &         'Climate dry-bulb temperature' )

C.....Relative humidity
      call add_to_xml_reporting (
     &         HF,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/relative_humidity',
     &         'units', '(%)',
     &         'Climate relative humidity' )

C .... Ambient AIR PARTIAL VAPOUR PRESSURE
      call add_to_xml_reporting (
     &         HF/100.0*Psat01(TF),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/pvap',
     &         'units', '(Pa)',
     &         'Climate partial vapour pressure' )

C .... Ambient AIR MOISTURE CONTENT
      call add_to_xml_reporting (
     &         HUMRT1(TF,HF,PATMOS,0),
     &         H3K_rep_Name(1:iH3K_NameLen)// '/moisture',
     &         'units', '(kg/kg_dry-air)',
     &         'Climate moisture content' )

C.....WIND VELOCITY
      call add_to_xml_reporting (
     &         VF,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/wind/velocity',
     &         'units', '(m/s)',
     &         'Climate wind velocity' )

C.....WIND Direction
      call add_to_xml_reporting (
     &         DF,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/wind/direction',
     &         'units', '(degrees)',
     &         'Climate wind direction' )

C.....Cloud Cover
      call add_to_xml_reporting (
     &         CLOUDC,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/cloud_cover',
     &         'units', '(fraction)',
     &         'Climate cloud cover' )

C.....Sky temperature
      call add_to_xml_reporting (
     &         (TSKY-273.15),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/sky_temp',
     &         'units', '(oC)',
     &         'Climate sky temperature' )

C.....Sky temperature depression
      call add_to_xml_reporting (
     &         (TF-(TSKY-273.15)),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/sky_temp_depression',
     &         'units', '(K)',
     &         'Climate sky temperature depression' )

C.....Ambient air saturation temperature
      call add_to_xml_reporting (
     &         DEWPT(HUMR(TF,HF,1013.),1013.),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/ambient_air_tsat',
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

C Calender and day types
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

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
      character*3 cBuildingName ! "Building" name for data output
      real    temp_day          ! Temporary vbls used to calculate day
      real    real_day
      integer idyp_adjust
      integer iCurrent_year, iStart_year
      integer iH3K_NameLen
      integer IDTY, IDAY        ! index for current day type, current day

C.....References
      integer LNBLNK            ! function returning # of non-blank characters in
                                ! a string
      integer h3k_month         ! function returning index of current month when
                                ! passed the current day (1.0->365.999...)

C.....Set time data output base name
      cBuildingName='bui'

C.......Set base name for time data output
      H3K_rep_NAME = cBuildingName
      iH3K_NameLen=lnblnk(H3K_rep_NAME)

C------------------------- current day type ---------------------------
      IDAY=IDYP
      IF(IHRF.EQ.1)IDAY=IDYP+1
      IF(IDAY.GT.365)IDAY=IDAY-365
      IDTY=ICALENDER(IDAY)

C.....Building simulation time (present)
C.....-> update meta-data
      call add_to_xml_reporting (
     &         btimep,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/time/present',
     &         'units', '(hours)',
     &         'Time corresponding to current simulation time row' )

C.....Building simulation time (future)
      call add_to_xml_reporting (
     &         btimef,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/time/future',
     &         'units', '(hours)',
     &         'Time corresponding to future simulation time row' )

C.....Building simulation hour (present)
      call add_to_xml_reporting (
     &         real(ihrp),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/hour/present',
     &         'units', '(hours)',
     &         'Hour corresponding to current simulation hour' )

C.....Building simulation hour (future)
      call add_to_xml_reporting (
     &         real(ihrf),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/hour/future',
     &         'units', '(hours)',
     &         'Hour corresponding to future simulation hour' )

C.....Building simulation day (present)
      call add_to_xml_reporting (
     &         real(idyp),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/day_number/present',
     &         'units', '(days)',
     &         'Day # corresponding to current simulation day' )

C.....Building simulation day (future)
      call add_to_xml_reporting (
     &         real(idyf),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/day_number/future',
     &         'units', '(day)',
     &         'Day # corresponding to future simulation day' )

C.....Simulation year for multi-year simulation
      if ( bMY_sim_enabled ) then
C........Building simulation year (present)
         call add_to_xml_reporting (
     &         real(iMY_current_year),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/year/present',
     &         'units', '(year)',
     &         'Year corresponding to current simulation day' )

C........Building simulation day (future)
         call add_to_xml_reporting (
     &         real(iMY_future_year),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/year/future',
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

C.....-> update meta-data
      call add_to_xml_reporting (
     &         real_day,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/day/present',
     &         'units', '(days)',
     &         'Day corresponding to current simulation time row' )

C.....Building simulation month
      call add_to_xml_reporting (
     &         real(h3k_month(temp_day)),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/month',
     &         'units', '(-)',
     &         'Month corresponding to current simulation time row ' )

C.....Building simulation day (future) ->real number
      real_day = real (idyp_adjust)
     &   +(real(ihrp)+real(its)/real(ntstep))/24.0
     &   + real( iCurrent_year - iStart_year )  * 365.0

      call add_to_xml_reporting (
     &         real_day,
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/day/future',
     &         'units', '(day)',
     &         'Day corresponding to future simulation time row' )

C.....Current building simulation daytype
      call add_to_xml_reporting (
     &         real(IDTY),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/day/type',
     &         'units', '(-)',
     &         'Current daytype (casual gains, controls)' )

C.....Building simulation timestep
      call add_to_xml_reporting (
     &         real(nsinc),
     &         H3K_rep_NAME(1:iH3K_NameLen)//'/time_step',
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

C Strings describing plant components.
      common/pcddsc/pcdesc(maxpc),npref(mpcom)   ! Plant component description and reference number (e.g. "63" for Slab-on-grade)
      common/pcnam/pcname(mpcom)                 ! Plant component names 
      integer npref
      character pcdesc*80,pcname*15

      common/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)   ! Plant present state variables
      real CSVF, CSVP

      common/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)    ! ? type and simulation modes supported by nodes
      integer NDCON, ISV

      common/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &  ICONDX(MPCOM,MNODEC,MPCONC) ! index of connections
      integer icondx, icontp
      real convar

C..... (Additional) results data from plant components
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),NAPDAT(MPCOM)
      REAL QDATA,PCAOUT
      INTEGER napdat

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

C.....Loop through all components in plant network
      do ii = 1, npcomp

C........Build base name plt/<component_name>
         char_temp = pcname(ii)
         iLength = lnblnk(char_temp)
         iLength_tmp = iLength
         write(Base_Name, '(A,A)')
     &        'plt/',char_temp(1:iLength)

C === output (additional) node data

        if (npref(ii).eq.63) then
           write(H3K_rep_NAME,'(A,A)')
     &           Base_Name(1:iLength+4),
     &           '/Qboiler'
           call add_to_xml_reporting (
     &          PCAOUT(ii,3),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Plant component node heat injection' )
        endif ! check for radiant_slab_on_grade

        if (npref(ii).eq.50) then
           write(H3K_rep_NAME,'(A,A)')
     &           Base_Name(1:iLength+4),
     &           '/Qboiler'
           call add_to_xml_reporting (
     &          PCAOUT(ii,1),
     &          H3K_rep_NAME,
     &          'units', '(W)',
     &          'Plant component node heat injection' )
        endif ! check for WCH calorifier

C === end output of node data

C........Loop through nodes (NPCDAT(i,8)
         do jj = 1, NPCDAT(ii,8)

C...........Build node name
            iLength = iLength_tmp  ! Reset string length in case it was incremeted for 2-digit nodes.
            if(jj<10)then  ! First nine nodes.
              write(Node_name, '(A,A,I1)')
     &             Base_Name(1:iLength+4),
     &             '/node_',jj
            else           ! This presumes that no plant component will have more than 99 nodes!
              write(Node_name, '(A,A,I2)')
     &             Base_Name(1:iLength+4),
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

C...........Transport State variables
            if(jj<10)then
              iLength = iLength_tmp
            else                           ! Longer string length required for 2-digit nodes.
              iLength = iLength_tmp + 1
            endif

           !Temperature:
            write(H3K_rep_NAME,'(A,A)')
     &           Node_Name(1:iLength+11),
     &           '/temperature'
            call add_to_xml_reporting (
     &         CSVF(iNode,iProp_temp),
     &         H3K_rep_NAME,
     &         'units', '(oC)',
     &         'Plant component node temperature' )


           !Mass flow (1st phase)
            if (iState .ne.9) then
               write(H3K_rep_NAME,'(A,A,A,A)')
     &              Node_Name(1:iLength+11),
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
     &              Node_Name(1:iLength+11),
     &              '/moisture_flow'
               call add_to_xml_reporting (
     &            CSVF(iNode,iProp_2nd_flow),
     &            H3K_rep_NAME,
     &            'units', '(kg/s)',
     &            'Plant component node second-phase flow' )

            endif
            if( iState .eq. 2 ) then
               write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:iLength+11),
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
     &                 Node_Name(1:iLength+11),
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
     &                 Connect_name(1:iLength+24),
     &                 '/temp'
                  call add_to_xml_reporting (
     &               H3K_Connect_property(iConn_numb,iProp_temp),
     &               H3K_rep_NAME,
     &               'units', '(oC)',
     &               'Plant component node connection temperature' )

C.................Mass flow rate - check state of material
                  if (iState .eq. 0) then
C....................Connection is water. 1st-phase flow only!
                     write(H3K_rep_NAME,'(A,A)')
     &                    Connect_name(1:iLength+24),
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
     &                    Connect_name(1:iLength+24),
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
     &                    Connect_name(1:iLength+24),
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
     &                    Connect_name(1:iLength+24),
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

C     NCHE
C         elseif ( iComponent_type .eq. 123 ) then

C            CALL NCHE_H3Kreports_module (
C     &                 iComponent_Index, iReport )


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

C.....Call RE-H2-CTL reporting function
      call RESH2_Ctl_PostProcess()      


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

C Components: fixed input data
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      character*12     CMNAM ! - identifier/name
      Character*60     LTPCMP ! - short description of that type

      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      integer     ITPCMP ! - type number
      integer     ISDCMP  ! - number of supplementary data items (max. MCMS)
      integer     ISDCNN  ! - number of connection level supplementary items (max. MCNS)
      real         SUPCMP ! - component supplementary data items (1st item = fluid type)

C Connections: fixed input data
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      integer     NODPS ! - node number on positive side of connection
      real          HGTPS ! - height of +ve linkage point relative to NODPS (m)
      integer     NODNE ! - node number on negative side of connection
      Real          HGTNE ! - height of -ve linkage point relative to NODNE (m)
      integer     ITPCON ! - number of linking fluid flow component
      integer     NDSCNN ! - connection level component supplementary node numbers

      COMMON/mfctl/ctlpos(MCNN)
      real ctlpos

C Output data
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
      integer ii,jj,ICONTM                    ! counters
      CHARACTER*12 fluid                ! fluid type ('air' or 'water')
      character*128 Node_Name     ! node name
      character*128 Connect_Name ! connection name
      character*128 H3K_rep_NAME ! XML leaf name

      integer i_neg, i_CompType,i_CompIndex
      integer lnConnName  ! flow connection name length
      integer lnNodeName  ! flow node name length

      real area,rhon,PI ! component area and fluid density at node, PI
      
      Logical b_noveloc ! logical for velocity calc.

C.....Commons indicating if meta items have been initialized
      COMMON / H3KmetaInitVal /
     &     bBuildMeta,
     &     bClimMeta,
     &     bTimeMeta,
     &     bPlantMeta,
     &     bElectMeta

      logical  bBuildMeta,bClimMeta,bTimeMeta,bPlantMeta,bElectMeta

C.....Contaminants
      COMMON/CONTM8/CCONC(MNOD,MCONTM)
      REAL CCONC            ! Contaminant concentration
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      INTEGER NCONTM        ! number of contaminants
      INTEGER NOCNTM        ! =1 if contaminants file is defined
      CHARACTER*12 CONTMNAM ! Contaminant name

C.....References
      integer LNBLNK    ! function returning # of non-blank characters in
                                ! a string
      real DENSIT         ! function returning the density of a fluid

C.....Crude check if there is any flow network
      IF (NNOD.le.0) THEN
        write(*,*)"no flow network to output"
        RETURN
      ENDIF

C..... Call MFSTFL which sums up all flows in and out of a node ...
C..... IDIM = 1 => flows in kg/s
      CALL MFSTFL(1,FLWIN,FLWUT)
C..... IDIM = 2 => flows in m3/h
      CALL MFSTFL(2,FLWINV,FLWUTV)

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
     &        'mfn/',fluid(1:LNBLNK(fluid)),'/',
     &        NDNAM(ii)(1:LNBLNK(NDNAM(ii)))

         lnNodeName=LNBLNK(Node_Name)
C.........output node data
      ! total mass flow entering a node *******************************
         write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:lnNodeName),
     &              '/flowrate'

         call add_to_xml_reporting (
     &              real(FLWIN(ii)),
     &              H3K_rep_NAME,
     &              'units', '(kg/s)',
     &              'mfn total node flow rate' )

       ! total volume flow entering a node *******************************
         write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:lnNodeName),
     &              '/volflowrate'

         call add_to_xml_reporting (
     &              real(FLWINV(ii)),
     &              H3K_rep_NAME,
     &              'units', '(m3/h)',
     &              'mfn total node volume flow rate' )

       ! node temperature from flow network *******************************
         write(H3K_rep_NAME,'(A,A)')
     &              Node_Name(1:lnNodeName),
     &              '/temp'

         call add_to_xml_reporting (
     &              TNOD(ii),
     &              H3K_rep_NAME,
     &              'units', '(oC)',
     &              'mfn node temperature' )

c	! contaminant concentration from flow network *******************************
c       do jj=1,NCONTM
c         write(H3K_rep_NAME,'(A,A,A)')
c     &              Node_Name(1:lnNodeName),
c     &              '/conc/',
c     &              CONTMNAM(jj)(1:LNBLNK(CONTMNAM(jj)))
c
c         call add_to_xml_reporting (
c     &              CCONC(ii,jj),
c     &              H3K_rep_NAME,
c     &              'units', 'kg/kg',
c     &              'mfn node contaminant concentration' )
c       enddo

C........Only use positive side of connections

C........Loop through all interconnections of the current node 'ii'
         do jj = 1,NCNN

            if (NODPS(jj) .eq. ii) THEN ! only look for positive side conn. of cur. node
               i_neg = NODNE(jj) ! the node on the other side of the conn.

C              Build connection name
               write(Connect_Name, '(A,A,A,A,A,A,A,A,A)')
     &             'mfn/',fluid(1:LNBLNK(fluid)),'/',
     &             NDNAM(ii)(1:LNBLNK(NDNAM(ii))),'->',
     &             NDNAM(i_neg)(1:LNBLNK(NDNAM(i_neg))),
     &             ' (via ',
     &             CMNAM(ITPCON(jj))(1:LNBLNK(CMNAM(ITPCON(jj)))),')'

              lnConnName=LNBLNK(Connect_Name)

	! pressure drop between nodes of connection ***********************
              write(H3K_rep_NAME,'(A,A)')
     &             Connect_Name(1:lnConnName),
     &             '/Dp'

               call add_to_xml_reporting (
     &             real(PRES(ii)-PRES(i_neg)),
     &             H3K_rep_NAME,
     &             'units', '(Pa)',
     &             'mfn connection pressure drop' )

        ! flow through connection ****************************************
               write(H3K_rep_NAME,'(A,A)')
     &             Connect_Name(1:lnConnName),
     &             '/flow'

               call add_to_xml_reporting (
     &             real(FLW1(jj)),
     &             H3K_rep_NAME,
     &             'units', '(kg/s)',
     &             'mfn connection flow rate' )

        ! connection control setting ****************************************
               write(H3K_rep_NAME,'(A,A)')
     &             Connect_Name(1:lnConnName),
     &             '/ctlpos'

               call add_to_xml_reporting (
     &             ctlpos(jj),
     &             H3K_rep_NAME,
     &             'units', '(-)',
     &             'mfn connection control position' )

	          ! velocity at connection *********************************
              ! The following code is taken from mfget.F, lines 1266 ff., (moved type 460
              ! to "no velocity" and added type 211 to "no velocity")
              ! Find the related component type and reject those
              ! connections which are not appropriate.
               i_CompIndex=ITPCON(jj)
               i_CompType=ITPCMP(i_CompIndex)
               b_noveloc=.false.

               if (i_CompType .eq. 10 .or. i_CompType .eq. 15 .or.   ! flow resistances
     &             i_CompType .eq. 17 .or. i_CompType .eq. 20 .or.   ! of various definitions
     &             i_CompType .eq. 25 .or. i_CompType .eq. 30 .or.   ! see mfmach.F, lines
     &             i_CompType .eq. 35 .or. i_CompType .eq. 310 .or.  ! 580 ff.
     &             i_CompType .eq. 410 .or. i_CompType .eq. 211 .or.
     &             i_CompType .eq. 460) then

                      ! ** skip this component **
               b_noveloc=.true.

               elseif(i_CompType .eq. 40) then      ! common orifice flow component
                  area=SUPCMP(i_CompIndex,2)
               elseif(i_CompType.eq.50)then   ! laminar pipe volume flow rate
                  PI = 4.0 * ATAN(1.0)
                  area= PI * (SUPCMP(i_CompIndex,3)*
     &                     SUPCMP(i_CompIndex,3))
               elseif(i_CompType.eq.110)then  ! specific air flow opening
                  area=SUPCMP(i_CompIndex,2)
               elseif(i_CompType.eq.120)then  ! specific air flow crack component
                  area=SUPCMP(i_CompIndex,2)*SUPCMP(i_CompIndex,3)
               elseif(i_CompType.eq.130)then  ! specific air flow door
                  area=SUPCMP(i_CompIndex,2)*SUPCMP(i_CompIndex,3)
               elseif(i_CompType .eq. 210 .or.  ! general flow conduit (ie. duct or pipe).
     &                   i_CompType .eq. 220 .or.  ! flow conduit ending in converging 3-leg junction
     &                   i_CompType .eq. 230 .or.  ! flow conduit starting in diverging 3-leg junction
     &                   i_CompType .eq. 240 .or.  ! flow conduit ending in converging 4-leg junction
     &                   i_CompType .eq. 250)then  ! flow conduit starting in diverging 4-leg junction
                  area=SUPCMP(i_CompIndex,3)
               elseif(i_CompType .eq. 420)then ! flow corrector (ie. valve or damper) with polynomial flow resistance
                  area=SUPCMP(i_CompIndex,2)
               else
                  area=1.0 ! should'nt really happen ...
               endif
        ! end code snippet from mfget.F

               if (b_noveloc .eqv. .false.) THEN
        ! Now get density of "sending" node. If flw1(jj) is negative, use density of 
        ! node "i_neg". Densit() returns density in kg/m3
                  if (FLW1(jj) .lt. 0.0) THEN
                    rhon=DENSIT(NDFLD(i_neg),TNOD(i_neg))
                  Else
                    rhon=DENSIT(NDFLD(ii),TNOD(ii))
                  Endif

        ! Finally, write to H3K reporting
                  write(H3K_rep_NAME,'(A,A)')
     &               Connect_Name(1:lnConnName),
     &               '/veloc'

                  call add_to_xml_reporting (
     &               real(FLW1(jj))/rhon/area,
     &               H3K_rep_NAME,
     &               'units', '(m/s)',
     &               'mfn connection flow velocity' )

               endif ! end b_noveloc .true.

            endif     !<- end if positive side connection for current node

         enddo                  !<- connection loop end

C.........if contaminants are defined output these as well
         IF(NOCNTM.EQ.1)THEN
           do ICONTM=1,NCONTM

C.........output contaminant concentration
         write(H3K_rep_NAME,'(A,A,A)')
     &              Node_Name(1:lnNodeName),
     &              '/',CONTMNAM(ICONTM)

         call add_to_xml_reporting (
     &              CCONC(II,ICONTM),
     &              H3K_rep_NAME,
     &              'units', '(kg/kg)',
     &              'mfn contaminant concentration' )
           END DO               !<- contaminants loop end
         ENDIF

        ! whatever *****************************************************

      enddo                     !<- node loop end

      return
      end



C-----------------------------------------------------------------------
C     Initialize h3k zone control flags to .false.
C-----------------------------------------------------------------------

      blockdata initialize_h3k_zone_ctl_flags
#include "building.h"

C.....Description of zone control action; these data are used
C.....in H3Kreports to determine heating, cooling loads and
C.....to evaluate passive solar design performance.
      common/H3KReportsControl/bZoneHeated,   bZoneCooled,
     &                         fHeatSetpoint, fCoolSetpoint

C.....Flags indicating zone is heated, cooled.
      logical bZoneHeated(MCOM), bZoneCooled(MCOM)

C.....Heating and cooling setpoint (oC)
      real fHeatSetpoint(MCOM), fCoolSetpoint(MCOM)

      data bZoneHeated / MCOM * .false. /
      data bZoneCooled / MCOM * .false. /

      end blockdata initialize_h3k_zone_ctl_flags

C-----------------------------------------------------------------------
C     Reset zone control flags to false.
C-----------------------------------------------------------------------

      subroutine h3kReports_reset_zone_flags()
#include "building.h"

C.....Description of zone control action; these data are used
C.....in H3Kreports to determine heating, cooling loads and
C.....to evaluate passive solar design performance.
      common/H3KReportsControl/bZoneHeated,   bZoneCooled,
     &                         fHeatSetpoint, fCoolSetpoint

C.....Flags indicating zone is heated, cooled.
      logical bZoneHeated(MCOM), bZoneCooled(MCOM)

C.....Heating and cooling setpoint (oC)
      real fHeatSetpoint(MCOM), fCoolSetpoint(MCOM)

      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! number of connections 

      integer iZone          ! counter


      do iZone = 1, ncomp

        bZoneHeated(iZone) = .false.
        bZoneCooled(iZone) = .false.

      enddo

      end subroutine h3kReports_reset_zone_flags

