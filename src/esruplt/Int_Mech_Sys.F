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
C mechanical system (that is, a device meeting both
C space- and water-heating loads.
C
C ROUTINES:
C
C   IMS_ConvertP10Data: Converts P10 data for use with general IMS
C     model.
C
C   IMS_ComputeFuelConsumption: Determines energy consumption of 
C     an IMS system in response to building loads
C
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

C=IMS_ConvertP10Data====================================================
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
C     Compute water heating performance metrics.
C-----------------------------------------------------------------------

C.....P.10 defines a single water heater performance operating
C.....point
      iIMS_NumOPWaterH(iSystem) = 1

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

      return
      end











      