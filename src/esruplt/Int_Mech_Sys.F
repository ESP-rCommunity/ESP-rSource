C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2007. Please contact Alex Ferguson
C for details concerning licensing.

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
C
C================== Int_Mech_Sys.F =====================================
C
C Date:      Sep 12, 2007
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada, 2007
C
C This file contains code necessary to model a integrated
C mechanical system (that is, a device meeting both space-
C and water-heating loads).
C
C ROUTINES:
C
C   IMS_ConvertP10Data: Converts P10 data for use with general IMS
C     model.
C
C   IMS_CharPerformance: Supervisory routine that characterizes
C     the IMS unit's performance in response to building loads.
C
C REFERENCES:
C
C  Ferguson, A., Edwards, P. and Landsberg, D. (2007).
C     Modelling Advanced Integrated Mechanical Systems,
C     CETC report.
C
C  Lopez, P. (2001). Design of a domestic hot water model for Hot3000,
C     CETC Report
C
C  Haltrech, D. (1995). HOT2000 - Domestic Hot Water Model.
C     CETC report in NRCan archives (EA8890-9/OPE94-02-10 CLO)
C
C=======================================================================

C=======================================================================
C IMS_ConvertP10Data 
C=======================================================================
C
C SYNOPSYS:
C This routine converts data collected during P10 testing for use with
C the IMS model. These conversions (and the assumptions behind them)
C are described in Ferguson et al. (2007)
C
C INPUTS:
C   - iSystem: index of HVAC system
C
C OUTPUTS:
C   - none.
C
C=======================================================================
      subroutine IMS_ConvertP10Data(iSystem)
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "IMS_common.h"

C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem


C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      integer iOperPoint  ! Counter

      real fSUT_vol            ! Volume drawn during simulated usage test
      real fSUT_mass           ! Mass of water drawn during SUT
      real fSUT_del_temp       ! SUT delivery temperature
      real fSUT_sup_temp       ! SUT supply temperature
      real fSUT_duration       ! Duration of SUT
      real fSUT_SpecHeatCap    ! Heat capacity of water during SUT.
      real fSUT_Qavg           ! Average heat transfer during SUT

C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      integer iWater
      parameter ( iWater = 3 )

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real RhoFld              ! returns density of a fluid
      real SHtFld              ! returns specific heat of a fluid.
      real fIMS_Interpolate    ! Function to interpolate between
                               !   operating points in IMS system
                               !   performance map
C-----------------------------------------------------------------------
C     Flow rates associated with ventilation: See equatiions 53/54 in
C     Ferguson et al. (2007)
C-----------------------------------------------------------------------
      fIMS_VentFlow( iSystem, iMode_NoVent   ) = 0.0
      fIMS_VentFlow( iSystem, iMode_LowVent  )
     &  = max( fP10_Vent_FA_Flow( iSystem,iMode_LowVent  ),
     &         fP10_Vent_EA_Flow( iSystem,iMode_LowVent  ) )
      fIMS_VentFlow( iSystem, iMode_HighVent  )
     &  = max( fP10_Vent_FA_Flow( iSystem,iMode_HighVent ),
     &         fP10_Vent_EA_Flow( iSystem,iMode_HighVent ) )


C-----------------------------------------------------------------------
C     Power consumption for ventilation: See eqn 34/35 in
C     Ferguson et al. (2007)
C-----------------------------------------------------------------------
      fIMS_VentPower( iSystem, iMode_NoVent  )  = 0.0

      fIMS_VentPower( iSystem, iMode_LowVent )
     &    =   fP10_Vent_FA_Power ( iSystem, iMode_LowVent )
     &      + fP10_Vent_EA_Power ( iSystem, iMode_LowVent )

      fIMS_VentPower( iSystem, iMode_HighVent )
     &    =   fP10_Vent_FA_Power ( iSystem, iMode_HighVent )
     &      + fP10_Vent_EA_Power ( iSystem, iMode_HighVent )
C-----------------------------------------------------------------------
C     Incremental power consumption used to distribute ventilation
C     air when not in space heating mode: See eqn. 37 in Ferguson
C     et al. (2007)
C-----------------------------------------------------------------------
      fIMS_IncDistPower(iSystem) 
     &   = fP10_PowerCirc(iSystem)
     &       - fIMS_ControlsPower(iSystem)
     &       - fIMS_VentPower(iSystem, iMode_LowVent)

C-----------------------------------------------------------------------
C     For each P.10 space heating test point (15%/40%/100%), compute
C     incremental space heaing fan and auxiliary power.
C-----------------------------------------------------------------------

C.....P.10 defines 3 space heating operating points:
      iIMS_NumOPSpaceH(iSystem) = iP10_num_SpaceH_PLP

C.....Assign part-load ratio parameters for P.10
      fIMS_SpaceHPLR(iSystem, iP10_PLR_15)  = 0.15
      fIMS_SpaceHPLR(iSystem, iP10_PLR_40)  = 0.40
      fIMS_SpaceHPLR(iSystem, iP10_PLR_100) = 1.00

C.....Loop through all space heating operating points...
      SpaceH_OP_loop: do iOperPoint = 1, iIMS_NumOPSpaceH(iSystem)

C.......Incremental power consumption used by IMS unit in space
C.......heating mode: See eqn. 40 in Ferguson et al. (2007)
        fIMS_FanPower( iSystem, iOperPoint )
     &     = fP10_SpaceH_Power( iSystem, iOperPoint )
     &         - fIMS_ControlsPower( iSystem )
     &         - fIMS_VentPower( iSystem, iMode_LowVent)

C.......In P.10 compatability mode, Auxiliary power consuption
C.......is assumed to be zero (see sec. 3.2 in Ferguson et al. 2007)
        fIMS_AuxPower( iSystem, iOperPoint ) = 0.0

      enddo SpaceH_OP_loop

C-----------------------------------------------------------------------
C     P.10 does not detemine minimum space-heating capacity. Set to
C     zero. 
C-----------------------------------------------------------------------
      fIMS_SpaceHMinCapacity( iSystem ) = 0.0

C-----------------------------------------------------------------------
C     Compute water heating performance metrics.
C-----------------------------------------------------------------------

C.....P.10 defines a single water heater performance operating
C.....point at 100% capacity
      iIMS_NumOPWaterH(iSystem) = 1
      fIMS_WaterHPLR(iSystem,1) = 1.0

C-----------------------------------------------------------------------
C     Assign Flue-loss/skin-loss ratio based on system type, fuel.
C
C     See Equations 50--52 and Table 3 in Ferguson et al. (2007).
C     These data were originally derived by Haltrech (1995), and
C     also form the basis of the standard HOT2000/HOT3000 DHW models.
C 
C-----------------------------------------------------------------------

      IMSHeatSource: select case ( iIMS_HeatSource(iSystem) )

        case ( iIMS_CondensingTank )

          fIMS_FlueSkinLossRatio(iSystem) = 0.0
        
        case ( iIMS_ConventionalTank )

          if ( iFuel_Type(iSystem) == iOil )
     &           fIMS_FlueSkinLossRatio(iSystem) = 0.468

          if ( iFuel_Type(iSystem) == iNaturalGas ) 
     &           fIMS_FlueSkinLossRatio(iSystem) = 0.314

        case ( iIMS_InducedDraftFanTank )

          fIMS_FlueSkinLossRatio(iSystem) = 0.490
        
        case ( iIMS_InstantaneousHeater )

          fIMS_FlueSkinLossRatio(iSystem) = 0.588
          
        case ( iIMS_TanklessHeater      )

          fIMS_FlueSkinLossRatio(iSystem) = 1.0

      end select IMSHeatSource

C-----------------------------------------------------------------------
C     Compute average rate of heat transfer during P.10 simulated
C     24-hour DHW usage test (See eqn 42 in Ferguson 2007)
C
C     Mass of drawn for dhw use:
C     --------------------------
C
C     water-mass = (DHW-draw-volume) * (density @ delivery temp)
C
C
C     Heat capacity of drawn water:
C     -----------------------------
C
C                (Cp @ supply temp ) + (Cp @ delivery temp )
C     heat-cap = -------------------------------------------
C                                    2
C
C
C     Average rate of heat transfer over a 24-hour period
C     ---------------------------------------------------
C 
C              (water-mass)(heat-cap)( Delivery-temp - Supply-temp)
C     Q-xfer = ----------------------------------------------------
C                           (Duration-of-test)
C
C
C     All of the right-hand terms are known values for a P.10 test:
C
C     DHW-draw-volume  =  0.3 m^3
C     Delivery temp.   = 48.9 oC
C     Supply temp.     = 14.4 oC
C     Duration of test = 24   h
C
C-----------------------------------------------------------------------

C.....Initialize values from P10
      fSUT_vol       =  0.3                ! m^3
      fSUT_del_temp  = 48.9                ! oC
      fSUT_sup_temp  = 14.4                ! oC
      fSUT_duration  = 60.0 * 60.0 * 24.0  ! s

C.....Compute mass of water (kjg/s)
      fSUT_mass = fSUT_vol * RhoFld( iWater, fSUT_del_temp )

C.....Compute specific heat capacity (j/kg oC)
      fSUT_SpecHeatCap = ( SHtFld( iWater, fSUT_del_temp ) +
     &                     SHtFld( iWater, fSUT_sup_temp )   ) / 2.0

C.....Compute average rate of heat transfer (W)
      fSUT_Qavg =fSUT_mass * fSUT_SpecHeatCap
     &             * ( fSUT_del_temp - fSUT_sup_temp ) / fSUT_duration

C-----------------------------------------------------------------------
C     Now estimate skin-losses during 24-hour SUT. See equation 52
C     in Ferguson et al. (2007):
C
C     Q-skin-loss =
C
C          (1 - flue/skin-loss-rato)
C
C             * ( Q-xfer / WHPF - Q-xfer - avg-power-use )
C
C    Where WHPP is the water heater performance factor, and
C    avg-power-use is the average power used duing the P.10 test.
C
C    Note: for P.10 tests, we assume avg-power-use = 0
C
C-----------------------------------------------------------------------

      fIMS_WaterHSkinLoss(iSystem)
     &   = ( 1.0 - fIMS_FlueSkinLossRatio(iSystem) )
     &      * ( fSUT_Qavg / fP10_WaterH_PF(iSystem) - fSUT_Qavg )

C-----------------------------------------------------------------------
C     Finally, estimate water heater efficiency from WHPF. See
C     Equation 47 in Ferguson et al. (2007):
C
C                             Q-xfer + Q-skin-loss
C     efficiency = WHPF * ------------------------------
C                         Q-xfer - WHPF * avg-power-use 
C
C     Again, avg-power-use = 0
C
C-----------------------------------------------------------------------

      fIMS_WaterHEff(iSystem,1) = fP10_WaterH_PF(iSystem)
     &   * ( fSUT_Qavg + fIMS_WaterHSkinLoss(iSystem) ) / fSUT_Qavg

C-----------------------------------------------------------------------
C     Compute nominal burner output, from nominal burner input:
C
C     output = (input) * (efficiency @ 100% capacity)
C
C-----------------------------------------------------------------------
      fIMS_NominalBurnerOutput( iSystem ) =
     &   fIMS_NominalBurnerInput( iSystem ) *
     &   fIMS_Interpolate( iSystem, 1.0, iIMSVar_WaterHEfficiency )

      return
      end

C=======================================================================
C IMS_CharPerformance
C=======================================================================
C
C SYNOPSYS:
C This supervisory routine characterizes the performance of an IMS
C system in response to building loads.
C
C INPUTS:
C  - iSystem:         System index
C  - fSpaceHLoad:     Combined space heating load (W)
C  - fWaterHLoad:     Combined water heating load (W)
C  - fWaterHDuration: Duration of water heating load (s)
C
C OUTPUTS:
C  - fFuelUseRate:     Rate of fuel consumption (kg/s)
C  - fElecUseRate:     Rate of electrical consumption (kg/s)
C  - fVentFlow:        Ventilation flow rate (kg/s)
C  - fVaporChangeRate: Total rate of change in building moisture
C                      distribution
C
C=======================================================================
      subroutine IMS_CharPerformance( iSystem,
     &                                fSpaceHLoad,
     &                                fWaterHLoad,
     &                                fWaterHLoadDuration,
     &                                fFuelUseRate,
     &                                fElecUseRate,
     &                                fVentFlow,
     &                                fVaporChangeRate )

      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "IMS_common.h"
C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem          ! system index
      real fSpaceHLoad         ! space heating load (W)
      real fWaterHLoad         ! water heating load (W)
      real fWaterHLoadDuration ! duration of water heating load
      real fFuelUseRate        ! Computed fuel use (kg/s)
      real fElecUseRate        ! Computed electric use (W)
      real fVentFlow           ! Ventilation flow rate (kg/s)
      real fVaporChangeRate    ! Rate of change in building encapsulated
                               ! moisture.

C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------

C.....Time
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1,             ! not used in current context
     &        ism1,             !
     &        isd2,             !
     &        ism2,             !
     &        isds,             !
     &        isdf              !
      integer ntstep            ! number of building steps/hour
      common/pctstp/ntstpp
      integer ntstpp            ! number of plant timesteps / building timestep

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      logical bCloseToZero     ! Result of close-to-zero comparison
      logical bNoSpaceHLoad    ! Flag space heating load
      logical bNoWaterHLoad    ! Flag for water heating load

      real fTS_Duration        ! Duration of timestep (S)

      real fTS_FracWaterH,     ! Fraction of time step spend in
     &     fTS_FracSpaceH,     !   space, water, and combined
     &     fTS_FracSpaceWaterH,!   space and water heating modes.
     &     fTS_NoDemand

      real fPLR_SpaceH,        ! Part-load ratios for space, water
     &     fPLR_WaterH         !   heating

      real fSpaceHEff,         ! Space heating efficiency, fan power (W)
     &     fFanPower,          !   and auxiliary sys. power (W)
     &     fAuxPower,          !
     &     fSHIncPower         !

      real fWaterHEff,         ! Water heater efficiency (-)
     &     fWaterHPower        !   and power consumption (W)

      real fQCoil              ! Heat transfer at the coil. (W)

      real fQFuel_SH,          ! Fuel power required for space-heating,
     &     fQFuel_WH           !   water-heating functions (W)

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fIMS_Interpolate    ! Function to interpolate between
                               !   operating points in IMS system
                               !   performance map

C-----------------------------------------------------------------------
C     Compute number of seconds/timestep
C
C                                 3600s/hour
C     TS duration =   -------------------------------------
C                     (# bld ts / hour)*(# plt ts / bld ts)
C
C-----------------------------------------------------------------------
      fTS_Duration = ( 60.0 * 60.0 ) / real ( ntstep * ntstpp )   ! (s)
      
C-----------------------------------------------------------------------
C     Compute fraction of time step spent in each mode.
C
C     Fraction in water-heating mode:
C
C                          skin losses             DHW load duration
C     fraction = ------------------------------  + ------------------
C                (burner output) * (efficiency)    time step duration
C
C     See equation 1 in Ferguson et al (2007).
C
C-----------------------------------------------------------------------
      
C.....Is water heating load close to zero? Use 1 W tolerance for
C.....comparison.
      call eclose ( fWaterHLoad + fIMS_WaterHSkinLoss(iSystem),
     *              0.0, 1.0, bNoWaterHLoad )

      no_WH_load:
     &if ( bNoWaterHLoad .or.
     &     fWaterHLoad + fIMS_WaterHSkinLoss(iSystem) < 0.0 ) then

        bNoWaterHLoad = .true.
        fTS_FracWaterH = 0.0

      else

        fTS_FracWaterH = fIMS_WaterHSkinLoss(iSystem)
     &       / ( fIMS_WaterHEff(iSystem,iIMS_NumOPWaterH(iSystem))
     &           * fIMS_NominalBurnerOutput(iSystem)  )
     &       + fWaterHLoadDuration / fTS_Duration

      endif no_WH_load


C-----------------------------------------------------------------------
C     Compute fraction of time step spent in space heating mode.
C
C     See equation 2 in Ferguson et al. 2007.
C-----------------------------------------------------------------------

C.....Is space heating load close to zero? Use 1 W tolerance for
C.....comparison
      call eclose ( fSpaceHLoad, 0.0, 1.0, bNoSpaceHLoad )
      call eclose ( fIMS_SpaceHMinCapacity(iSystem),
     &              0.0, 1.0, bCloseToZero )
     
      no_SH_load: if ( bNoSpaceHLoad .or. fSpaceHLoad < 0.0 ) then

        bNoSpaceHLoad = .true.
        fTS_FracSpaceH = 0.0

      elseif ( .not. bCloseToZero .and.
     &         fIMS_SpaceHMinCapacity(iSystem) > 0.0 ) then

        fTS_FracSpaceH = fSpaceHLoad / fIMS_SpaceHMinCapacity(iSystem)

      else

        fTS_Duration = 1.0

      endif no_SH_load


C-----------------------------------------------------------------------
C     If sum of time step fraction spent in space and water heating modes
C     is greater than unity, the unit must spend some time in a combined
C     space- and water-heating mode. Compute this amount as per
C     equations 3--6 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

      combined_mode: if ( fTS_FracSpaceH + fTS_FracWaterH > 1.0 ) then

        fTS_FracSpaceWaterH = fTS_FracSpaceH + fTS_FracWaterH - 1.0

        fTS_FracSpaceH = fTS_FracSpaceH - fTS_FracSpaceWaterH
        fTS_FracWaterH = fTS_FracWaterH - fTS_FracSpaceWaterH

C.......Fraction of timestep for which there is no demand
C.......on IMS unit:
        fTS_NoDemand = 0.0

      else

        fTS_FracSpaceWaterH = 0.0

C.......Fraction of timestep for which there is no demand
C.......on IMS unit:
        fTS_NoDemand = 1.0 - fTS_FracSpaceH - fTS_FracWaterH

      endif combined_mode

C-----------------------------------------------------------------------
C     Begin characterization of ventilation system.
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C     Begin characterization of space heating operation
C-----------------------------------------------------------------------
      space_heating: if ( bNoSpaceHLoad ) then

C.......No space heating load - set results to zero.
        fPLR_SpaceH = 0.0
        fSpaceHEff  = 0.0
        fFanPower   = 0.0
        fAuxPower   = 0.0
        fSHIncPower = 0.0
        fQCoil      = 0.0
        fQFuel_SH   = 0.0

      else 

C-----------------------------------------------------------------------
C       First, estimate the part-load ratio. See eqn. 25 in
C       Ferguson et al. (2007)
C-----------------------------------------------------------------------
        fPLR_SpaceH = fSpaceHLoad / fIMS_SpaceHCapacity(iSystem) ! (-)

C-----------------------------------------------------------------------
C       Next, estimate Space-heating efficiency and power
C       consumption by interpolating from performance map. (See
C       equations 27--29 in Ferguson et al. [2007]). Function
C       fIMS_Interpolate handles the interpolation, we need only
C       specify the system #, the part-load ratio, and a integer
C       flag indicating which variable we wish to interpolate:
C         -iIMSVar_SpaceHEfficiency = 1,
C         -iIMSVar_SpaceHFanPower   = 2,
C         -iIMSVar_SpaceHAuxPower   = 3,
C         -iIMSVar_WaterHEfficiency = 4,
C         -iIMSVar_WaterHPower      = 5
C-----------------------------------------------------------------------
        fSpaceHEff = fIMS_Interpolate( iSystem,
     &                                 fPLR_SpaceH,
     &                                 iIMSVar_SpaceHEfficiency ) ! (-)

        fFanPower =  fIMS_Interpolate( iSystem,
     &                                 fPLR_SpaceH,
     &                                 iIMSVar_SpaceHFanPower ) ! (W)

        fAuxPower =  fIMS_Interpolate( iSystem,
     &                                 fPLR_SpaceH,
     &                                 iIMSVar_SpaceHAuxPower ) ! (W)

C-----------------------------------------------------------------------
C       Compute the total amount of heat transfer at the heating coil
C       that is, the space heating load less the energy added by the
C       fan:
C
C         Q_coil = ( total SH load ) - ( fan power )
C
C       See Equation 9 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

        !ToDo: I need to adjust this value to reflect ventilation load!

        fQCoil = fSpaceHLoad - fFanPower    ! (W)

C-----------------------------------------------------------------------
C       Compute the fuel power required by the IMS to supply this heat
C
C                              Q_coil
C         fuel power = -------------------------
C                      space-heaining efficiency
C
C       See Equation 11 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

        fQFuel_SH = fQCoil / fSpaceHEff    ! (W)

C-----------------------------------------------------------------------
C       ToDo: Compute fuel flow:
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C       Compute rate of incremental IMS power consumption in heating
C       mode
C
C       incremental power =  ( fan power ) + ( auxiliary systems power )
C-----------------------------------------------------------------------

        fSHIncPower = fFanPower + fAuxPower     ! (W)

      endif space_heating

C-----------------------------------------------------------------------
C     Begin characterization of water heating operation
C-----------------------------------------------------------------------
      water_heating: if ( bNoWaterHLoad ) then

C.......ToDo: No water heating load. Set results to zero.
        fPLR_WaterH  = 0.0
        fWaterHEff   = 0.0
        fWaterHPower = 0.0
        fQFuel_WH    = 0.0

      else

C-----------------------------------------------------------------------
C       Compute part-load ratio for water heater. See equation 30
C       in Ferguson et al. (2007)
C-----------------------------------------------------------------------
        fPLR_WaterH = ( fWaterHLoad + fIMS_WaterHSkinLoss( iSystem ) ) 
     &       / fIMS_NominalBurnerOutput (iSystem)

C-----------------------------------------------------------------------
C       Next, estimate water-heating efficiency and power
C       consumption by interpolating from performance map. (See
C       equations 31--32 in Ferguson et al. [2007]). Function
C       fIMS_Interpolate handles the interpolation, we need only
C       specify the system #, the part-load ratio, and a integer
C       flag indicating which variable we wish to interpolate:
C         -iIMSVar_SpaceHEfficiency = 1,
C         -iIMSVar_SpaceHFanPower   = 2,
C         -iIMSVar_SpaceHAuxPower   = 3,
C         -iIMSVar_WaterHEfficiency = 4,
C         -iIMSVar_WaterHPower      = 5
C-----------------------------------------------------------------------
        fWaterHEff   = fIMS_Interpolate( iSystem,
     &                                   fPLR_WaterH,
     &                                   iIMSVar_WaterHEfficiency ) ! (-)

        fWaterHPower = fIMS_Interpolate( iSystem,
     &                                   fPLR_WaterH,
     &                                   iIMSVar_WaterHPower ) ! (W)

C-----------------------------------------------------------------------
C       Now compute the fuel power required by the IMS for water heating
C       function
C-----------------------------------------------------------------------
        fQFuel_WH = ( fWaterHLoad + fIMS_WaterHSkinLoss( iSystem ) )
     &     / fWaterHEff

      endif water_heating

C-----------------------------------------------------------------------
C     Debugging:
C-----------------------------------------------------------------------
!       print*, "------------------------"
!       print*, "IMS: space heating load:", fSpaceHLoad
!       print*, "     SH PLR            :", fPLR_SpaceH
!       print*, "     SH eff            :", fSpaceHEff
!       print*, "     SH fan power      :", fFanPower
!       print*, "     SH aux power      :", fAuxPower
!       print*, "     SH inc power      :", fSHIncPower
!       print*, "     SH fuel power     :", fQFuel_SH
!       print*, "     water heating load:", fWaterHLoad
!       print*, "     WH PLR            :", fPLR_WaterH
!       print*, "     WH Eff            :", fWaterHEff
!       print*, "     WH Power          :", fWaterHPower
!       print*, "     WH Fuel power     :", fQFuel_WH

      return
      end 

C=fIMS_Interpolate======================================================
C
C SYNOPSYS
C This routine performs simple linear interpolation, returning the
C interpolated value from an array corresponding to a provided index.
C
C NOTE: This routine assumes that the index values (fIndexVals) are
C in ascending numerical order!
C
C INPUTS:
C   iSystem:    System description
C   fIndex:     Index value for interpolation
C   iInterpVar: Integer flag indicating which variable should be used
C               in interpolation. Tested against the following
C               named constants:pr
C                     iIMSVar_SpaceHEfficiency = 1,
C                     iIMSVar_SpaceHFanPower   = 2,
C                     iIMSVar_SpaceHAuxPower   = 3,
C                     iIMSVar_WaterHEfficiency = 4,
C                     iIMSVar_WaterHPower      = 5
C
C OUTPUTS:
C   fIMS_Interpolate: Result of interpolation
C
C=======================================================================
      real function fIMS_Interpolate( iSystem,
     &                                fIndex,
     &                                iInterpVar
     &                               )
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "IMS_common.h"
C-----------------------------------------------------------------------
C     Passed arguements
C-----------------------------------------------------------------------
      integer iSystem             ! System index
      real fIndex                 ! Index to search for
      integer iInterpVar          ! Flag indicating which variables
                                  !   to use in interplolation

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      logical bBoundsFound,       ! Flags controlling search loop.
     &        bAboveRange,        !
     &        bBelowRange         !


C.....Arrays holding X & Y values for interpolation
      real fVarX(iIMS_MaxOP),
     &     fVarY(iIMS_MaxOP)

      integer iRow                ! Counter
      integer iRowCount           ! # of rows in arrays

      real fResult                ! Result of interpolation
      
C-----------------------------------------------------------------------
C     Initialize flags
C-----------------------------------------------------------------------
      bBoundsFound = .false.
      bBelowRange  = .false.
      bAboveRange  = .false.
C-----------------------------------------------------------------------
C     First, check which variable has been specified, and determine
C     number of rows in arrays
C-----------------------------------------------------------------------
      set_row_count: select case ( iInterpVar )

        case (iIMSVar_SpaceHEfficiency,
     &        iIMSVar_SpaceHFanPower,
     &        iIMSVar_SpaceHAuxPower )
C.........Space-heating performance map
          iRowCount = iIMS_NumOPSpaceH( iSystem ) 

        case ( iIMSVar_WaterHEfficiency,
     &         iIMSVar_WaterHPower )
C.........Water heating performanc map
          iRowCount = iIMS_NumOPWaterH( iSystem )

      end select set_row_count

C-----------------------------------------------------------------------
C     Next, fill x and y arrays with appropriate data, depending on
C     specified variable
C-----------------------------------------------------------------------
      fill_XY_arrays: do iRow = 1, iRowCount

        which_data: select case ( iInterpVar )

          case ( iIMSVar_SpaceHEfficiency )

            fVarX( iRow ) = fIMS_SpaceHPLR( iSystem, iRow )
            fVarY( iRow ) = fIMS_SpaceHNetEff( iSystem, iRow )

          case ( iIMSVar_SpaceHFanPower )

            fVarX( iRow ) = fIMS_SpaceHPLR( iSystem, iRow )
            fVarY( iRow ) = fIMS_FanPower( iSystem, iRow )

          case ( iIMSVar_SpaceHAuxPower )

            fVarX( iRow ) = fIMS_SpaceHPLR( iSystem, iRow )
            fVarY( iRow ) = fIMS_AuxPower( iSystem, iRow )

          case ( iIMSVar_WaterHEfficiency )

            fVarX( iRow ) = fIMS_WaterHPLR( iSystem, iRow )
            fVarY( iRow ) = fIMS_WaterHEff( iSystem, iRow )

          case ( iIMSVar_WaterHPower )

            fVarX( iRow ) = fIMS_WaterHPLR( iSystem, iRow )
            fVarY( iRow ) = fIMS_WaterHPower( iSystem, iRow )

          case default

            ! Can't happen --- iInterpVar set programically

            STOP 'fIMS_Interpolate: iInterpVar out of range'

        end select which_data

      enddo fill_XY_arrays

C-----------------------------------------------------------------------
C     Then, check the index falls within the specified range
C-----------------------------------------------------------------------
      below_range: if ( fIndex < fVarX( 1 ) ) then

        bBelowRange = .true.

        fResult = fVarY( 1 )

      endif below_range

      above_range: if ( fIndex >= fVarX( iRowCount ) ) then

        bAboveRange = .true.

        fResult = fVarY( iRowCount )

      endif above_range

C-----------------------------------------------------------------------
C     And search through the index array, looking for the two rows that
C     bound the supplied search index.
C-----------------------------------------------------------------------

      iRow = 1

      find_index_loop: do while ( .not. bBoundsFound     .and.
     &                            .not. bBelowRange      .and.
     &                            .not. bAboveRange      .and.
     &                                  iRow < iRowCount       )

C.......Check if this row bounds the search index:
        check_row: if ( fIndex >= fVarX( iRow )    .and.
     &                  fIndex <  fVarX( iRow + 1 )      )then

C.........Row does bound search index. Set flag.
          bBoundsFound = .true.

        else 

C.........Increment row number:
          iRow = iRow + 1

        endif check_row

      enddo find_index_loop


C-----------------------------------------------------------------------
C     Now, if bounds were found, apply simple linear interpolation
C     between rows i and i+1 
C
C                                     [X - X(i)]
C     Y = Y(i) + [Y(i+1) - Y(i)] * ---------------
C                                  [X(i+1) - X(i)]
C
C     See eqns 28, 29, 32 & 33 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

      bounds_were_found: if ( bBoundsFound ) then

        fResult =
     &      fVarY( iRow )
     &        +  (   fVarY( iRow + 1 )
     &             - fVarY( iRow )
     &            )
     &        *  (  ( fIndex - fVarX( iRow ) )
     &             /( fVarX( iRow + 1 ) - fVarX( iRow ) )
     &           )

      endif bounds_were_found

      fIMS_Interpolate = fResult

      return
      end



