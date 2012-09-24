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

C This file ESPrTrnsysData.F contains COSIMDATATYPES module for use in co-simulation
C with TRNSYS using the Harmonizer.

! Purpose: This module holds the derived data type for passing data between ESP-r and TRNSYS. The use of
! a  derived data structure (DDS) means that data can be passed between the subroutines of ESP-r, TRNSYS and the harmonizer
! using a single variable (the DDS) which is basically an array containing all the data which needs to be passed.
!**********************************************************************************************************
!**********************************************************************************************************
! WARNING: Trnsys compiles in a file almost identical to this called CosimDataTypes.f90, the only differences between these
! files should be that the TCC_Indeces are not defined in CosimDataTypes.f90, and that TRNSYS defines its
! REALs as double precision.  If there are differences the co-simulation will not work as the data will become corrupt.
! The harmonizer has a thirdcopy of this file which again must be the C++ equivalent called HarmonizerDataTypes.h &
! HarmonizerDataTypes.cpp.
!**********************************************************************************************************
!**********************************************************************************************************
      MODULE COSIMDATATYPES
      IMPLICIT NONE
      Save
! Maximum number of fluid coupling components (HCCs & ACCs used in TRNSYS co-simulation)
      INTEGER, PARAMETER :: MAX_FCCs = 20
! Maximum number of electrical coupling components (used in TRNSYS co-simulation)
      INTEGER, PARAMETER :: MAX_ECCs = 10
! Maximum number of ESPr zones (used in TRNSYS co-simulation)
      INTEGER, PARAMETER :: MAX_ZONES = 100

! Derived data types
      TYPE TCC_Indeces!(Indeces For mapping to TRNSYS Type 130)
        INTEGER, DIMENSION(MAX_FCCs) :: iHCCtoT     !Indeces for Hydronic coupling component data to be sent to TRNSYS
        INTEGER, DIMENSION(MAX_FCCs) :: iHCCtoE     !Indeces for Hydronic coupling component data to be received from TRNSYS
        INTEGER, DIMENSION(MAX_FCCs) :: iACCtoT     !Indeces for Air coupling component data to be sent to TRNSYS
        INTEGER, DIMENSION(MAX_FCCs) :: iACCtoE     !Indeces for Air coupling component data to be received from TRNSYS
      END TYPE TCC_Indeces

      TYPE :: FCC                                 ! Fluid Coupling Component (FCC)
        REAL :: Flowrate                          ! Flow rate of liquid water (HCC) or dry air (ACC) (kg/s)
        REAL :: Temperature                       ! (deg C)
        REAL :: Moisture_flowrate                 ! Moisture flow rate (ACC only) (kg/s)
      END TYPE FCC

      TYPE :: ZoneData
        REAL :: AirPointTemperatures                ! Zone Air Point Temperature
        REAL :: AirPointHumidities                  ! Zone Humidities
        REAL :: AirPointCasualGains                 ! Casual gains to zone air-point (W)
      END TYPE ZoneData

! The electrical coupling component is not currently used.  The design of the ESP-r-TRNSYS co-simulation will hopefully/eventually
! adress the electrical domain in future.
      TYPE :: ECC                                   ! Electrical Coupling Component (ECC)
        LOGICAL :: Source                           !(Source=true->Power source, Source=false->Power sink)
        REAL :: Power                               !(W)
      END TYPE ECC

      TYPE :: EsprTrnsysData
        TYPE(FCC), DIMENSION(MAX_FCCs) :: HCC_to_TRNSYS     !Hydronic coupling component data to be sent to TRNSYS
        TYPE(FCC), DIMENSION(MAX_FCCs) :: HCC_to_ESPr       !Hydronic coupling component data to be received from TRNSYS
        TYPE(FCC), DIMENSION(MAX_FCCs) :: ACC_to_TRNSYS     !Air coupling component data to be sent to TRNSYS
        TYPE(FCC), DIMENSION(MAX_FCCs) :: ACC_to_ESPr       !Air coupling component data to be received from TRNSYS
!        TYPE(ECC), DIMENSION(MAX_ECCs) :: ElectricalCouplingComponents
        TYPE(ZoneData), DIMENSION(MAX_ZONES) :: ESPrZonesData   !Temperature, Humidity & Casual Gains for ESP-r Zones
        Integer :: Invocations                              !Number of ESP-r and TRNSYS invocations
        INTEGER :: ESPrConverged                            !Flag for ESPr convergence (not used yet)
        INTEGER :: TrnsysConverged                          !Flag for TRNSYS convergence (notused yet)
        INTEGER :: ESPrTimestep;                            !Number of ESP-r  timestep
        INTEGER :: TotalESPrIterations;                     !Number of ESP-r iteration
        INTEGER :: TrnsysTimestep;                          !Number of TRNSYS timestep
        INTEGER :: TotalTrnsysIterations;                   !Number of TRNSYS iteration
        INTEGER :: TrnsysCode;                              !Flag for TRNSYS
      END TYPE EsprTrnsysData
      END MODULE COSIMDATATYPES

