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
#include "OffsiteUtilitiesPublic.h"

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
C.....No ventilation
      fIMS_VentFlow( iSystem, iMode_NoVent   ) = 0.0
C.....Continuious (low) ventilation
      fIMS_VentFlow( iSystem, iMode_LowVent  )
     &  = max( fP10_Vent_FA_Flow( iSystem,iMode_LowVent  ),
     &         fP10_Vent_EA_Flow( iSystem,iMode_LowVent  ) )
C.....High ventilation
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
     &         - fIMS_VentPower( iSystem, iMode_LowVent )

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
C    Note: for P.10 tests, we assume avg-power-use = controls power
C
C-----------------------------------------------------------------------

      fIMS_WaterHSkinLoss(iSystem)
     &   = ( 1.0 - fIMS_FlueSkinLossRatio(iSystem) )
     &      * ( fSUT_Qavg / fP10_WaterH_PF(iSystem)
     &           - fSUT_Qavg
     &           - fIMS_ControlsPower(iSystem)
     &         )

C-----------------------------------------------------------------------
C     Finally, estimate water heater efficiency from WHPF. See
C     Equation 47 in Ferguson et al. (2007):
C
C                             Q-xfer + Q-skin-loss
C     efficiency = WHPF * ------------------------------
C                         Q-xfer - WHPF * avg-power-use 
C
C     Again, avg-power-use = controls-power
C
C-----------------------------------------------------------------------

      fIMS_WaterHEff(iSystem,1) = fP10_WaterH_PF(iSystem)
     &   * ( fSUT_Qavg + fIMS_WaterHSkinLoss(iSystem) )
     &   / ( fSUT_Qavg
     *        - fP10_WaterH_PF(iSystem) * fIMS_ControlsPower(iSystem) )

C-----------------------------------------------------------------------
C     Compute nominal burner output, from nominal burner input:
C
C     output = (input) * (efficiency @ 100% capacity)
C
C-----------------------------------------------------------------------
      fIMS_NominalBurnerOutput( iSystem ) =
     &   fIMS_NominalBurnerInput( iSystem ) *
     &   fIMS_Interpolate( iSystem, 1.0, iIMSVar_WaterHEfficiency )

C-----------------------------------------------------------------------
C     Assign values for sensible heat recovery efficiency (SRE).
C
C     P.10 provides a single SRE number; assume it prevails for
C     sensible, condensing and defrost conditions
C-----------------------------------------------------------------------
      fIMS_VentHREfficiency(iSystem,iVentEff_Sensible) =
     &       fP10_Vent_SRE(iSystem)

      fIMS_VentHREfficiency(iSystem,iVentEff_Condensing) =
     &       fP10_Vent_SRE(iSystem)

      fIMS_VentHREfficiency(iSystem,iVentEff_Defrost) =
     &       fP10_Vent_SRE(iSystem)

C-----------------------------------------------------------------------
C     Assign arbitrary temperature to defrost threshold (that is,
C     the temperature below which the IMS unit will enter defrost
C     mode). The actual value doesn't matter; we've assigned the 
C     same SRE value to all states ( sensible/condensing/defrost
C     operation )
C-----------------------------------------------------------------------
      fIMS_VentDefrostTemp(iSystem) = ( -20.0 )

      return
      end

C=IMS_CharVentilation===================================================
C
C SYNOPSYS:
C This procedure characterizes the ventilation operation of the IMS
C system. It reports the resulting ventilation flow rate,
C rate of mosture addition/extraction and thermal flux associated
C with the IMS unit's ventilation operation.
C
C INPUTS:
C   - iSystem: index of system
C   - iVentMode: Flag indicating which ventilation mode the
C                system is in.
C
C OUTPUTS:
C  - fVentFlow:        Ventilation flow rate (kg/s)
C  - fVentLoad:        Thermal load associated with ventilation (W)
C  - fVaporChangeRate: Total rate of change in building air-point
C                      mosture balance (kg/s)
C  - Supply air temperature. 
C
C=======================================================================
      subroutine IMS_CharVentilation(iSystem,
     &                               iVentMode,
     &                               fVentFlow,
     &                               fVentLoad,
     &                               fVaporChangeRate,
     &                               fDBTemp_supply,
     &                               fVentPower)
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "IMS_common.h"
C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem         ! System index
      real fVentFlow          ! Flow rate of ventilation air (kg/s)
      real fVentLoad          ! Thermal load associated with ventilation
                              !  (W)
      integer iVentMode       ! Ventilation mode. 
      real fVaporChangeRate   ! Rate of change in zone moisture balance
                              !  (kg/s)
      real fVentPower 
C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
C.....Climate:
      common/climi/qfp,qff,tp,tf,qdp,qdf,vp,vf,dp,df,hp,hf
      real qfp,               ! Not used in current context
     &     qff,               !
     &     tp,                !
     &     qdp,               !
     &     qdf,               !
     &     vp,                !
     &     vf,                !
     &     dp,                !
     &     df,                !
     &     hp                 !

      real tf,                ! Future row dry bulb temperature
     &     hf                 ! Future row relitive humitity

C.....Building conditions:
      common/fvala/tfa,qfa
      real tfa(mcom)           ! Future zone temperature
      real qfa(mcom)           ! Not used in current context.

      common/fvalg/gfa
      real gfa(mcom)           ! Zone future relative humitity.

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      
C.....Climatic and Zone conditions
      real fDBTemp_amb,        ! Ambient and zone dry bulb temperatures (K)
     &     fDBTemp_zone

      real fPPres_amb,         ! Ambient and zone dry-air parital
     &     fPPres_zone         !    pressures (kPa)

      real fDBTemp_supply      ! Temperature of air leaving the
                               !   heat exchanger (oC)

      real fDBTemp_exhaust     ! Temperature of air exhausted to ambient (oC)

      real fMFA_dry, fMEA_dry  ! Fresh and exahust dry-air flow rates (kg/s)
      real fMFA_vap, fMEA_vap  ! Fresh and exh. air vapor flow rates (kg/s)

      real fMFA_dry_LL,        ! Lower and upper limits on bisection
     &     fMFA_dry_UL         !   root search

      real fFA_HeatCapacity,   ! Specific heat capacity of fresh, exhaust air
     &     fEA_HeatCapacity    !   (W/oC)

      real fHR_zone, fHR_amb   ! Ambient and zone humidity ratios (kg/kg)
     
      logical bUnConverged     ! Controls for ventilation iteration

      real fVentHREfficiency   ! Heat recovery efficiency (-)
      logical bDefrost         ! Flag for defrost operation

      real fQVent_HR           ! Heat recovery in IMS HE core (W)
      real fQVentLoad          ! Additional space-heating load
                               !   associated with ventilation (W)

                               
C.....Initialization flag for h3kreprots module
      logical bH3KReportsInitialized(max_sys)
      save bH3KReportsInitialized
      integer iNumber

      data ( bH3KReportsInitialized(iNumber),
     &       iNumber=1,max_sys ) / max_sys * .false. /
C.....Storage string.
      character*128 cRootName(max_sys)
      save cRootName
      character*8 cFormat
      integer iStrLen(max_sys)
      save iStrLen

C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      real fIG_R_air           ! Ideal gas constant for air
      parameter ( fIG_R_air = 0.2870 ) ! kJ/kg K

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real Hum_Ratio_OA        ! Function returning humitity ratio
                               !   for outdoor air.
      real Ret_Air_Hum_Ratio   ! Humitity ratio of return air

      real Ret_Air_Temp        ! Temperature of return air

      real PSat01              ! Saturation vapor pressure of air

      real Pvpr01              ! Function returing humidity ratio of
                               !   air.
      real CPmst01             ! Function returning specific heat
                               !   of moist air 

      real Atm_Pressure        ! Function returing atmospheric pressure

      integer LNBlnk           ! Returns length of a string.

C-----------------------------------------------------------------------
C     Characterize ventilation operation
C-----------------------------------------------------------------------

C.....Collect ventilation flow rates and ventilation power
      fVentFlow  = fIMS_VentFlow(iSystem,iVentMode)        ! (kg/s)

      vent_mode_ND: select case ( iVentMode )

        case ( iMode_NoVent )

          fVentFlow          = 0.0                            ! (kg/s)
          fVaporChangeRate   = 0.0                            ! (kg/s)
          fQVentLoad         = 0.0                            ! (W)

        case ( iMode_LowVent, iMode_HighVent )

C-----------------------------------------------------------------------
C            Compute ventilation thermal load and resulting moisture
C            balance.
C-----------------------------------------------------------------------

C.........Collect outside and return-air temperatures, humidity ratios

          fDBTemp_amb  = tf                       ! (oC)
          fDBTemp_zone = Ret_Air_Temp(iSystem)    ! (oC)

          fHR_amb  = Hum_Ratio_OA( fDBTemp_amb, hf, site_altitude ) ! (kg/kg)
          fHR_zone = Ret_Air_Hum_Ratio(iSystem)                     ! (kg/kg)

C-----------------------------------------------------------------------
C         Compute partial pressure of dry air in zone, ambinet:
C
C         p.pressure = (atm press.) - (saturation press.)
C
C         Note: Function Pvpr01 returns the vapor pressure directly
C
C-----------------------------------------------------------------------
          fPPres_amb = Atm_Pressure( site_altitude )
     &               - Pvpr01( fHR_amb, Atm_Pressure( site_altitude ) )

          fPPres_zone = Atm_Pressure( site_altitude )
     &               - Pvpr01( fHR_zone, Atm_Pressure( site_altitude ) )

C-----------------------------------------------------------------------
C         Compute dry-air mass flow rates:
C
C                                     (IG constant ) * ( Temperature [K] )
C         mass flow = ( volume flow ) * ------------------------------------
C                                             Dry air partial pressure
C
C         See  eqns 16 and 17 in Ferguson et al. (2007)
C
C         The supply air flow rate will depend on the temperature at the
C         heat exchanger outlet, which in turn depends on the heat capacity
C         flowing through the heat exchanger. Therefore iteration is
C         required.
C-----------------------------------------------------------------------


C.........Exhaust dry air flow rate (kg/s)
          fMEA_dry = fVentFlow * fIG_R_air * ( fDBTemp_zone + 273.15 )
     &         / fPPres_zone

C.........Exhaust moisture flow rate (kg/s)
          fMEA_vap = fMEA_dry * fHR_zone

C-----------------------------------------------------------------------
C         Establish iteration loop for heat transfer, supply air mass
C         flow rate:
C-----------------------------------------------------------------------

C.........Guesstimate fresh air mass flow(kg/s)
          fMFA_dry = fMEA_dry

C.........And corresponding moisture flow rate (kg/s)
          fMFA_vap = fMFA_dry * fHR_amb

C.........Assume unit operates sensibly, to start, unless amb. temperature
C.........is below defrost temperature
          if ( fDBTemp_amb <= fIMS_VentDefrostTemp ( iSystem ) ) then

            fVentHREfficiency =
     &         fIMS_VentHREfficiency(iSystem, iVentEff_Defrost)
            bDefrost = .true.

          else

            fVentHREfficiency =
     &         fIMS_VentHREfficiency(iSystem, iVentEff_Sensible)
            bDefrost = .false.

          endif

C.........Guesstimate supply temperature, exhaust temperature
C.........by assuming inlet, exhaust heat capacities are equal
C.........(we'll improve these estimates inside the iteration loop)
          fDBTemp_supply = fDBTemp_amb
     &      + ( fDBTemp_zone - fDBTemp_amb) * fVentHREfficiency

          fDBTemp_exhaust = fDBTemp_zone
     &      + ( fDBTemp_amb - fDBTemp_zone ) * fVentHREfficiency


C.........Set limits for bisection search. These are probably quite generous.
          fMFA_dry_LL = 0.0
          fMFA_dry_UL = fMFA_dry * 2.0


          bUnConverged = .true.

C-----------------------------------------------------------------------
C         Bisection search iteration loop:
C
C            1. Using estimated supply mass flow rates, compute
C               heat capacity.
C
C            2. Select appropriate HR efficiency
C
C            3. Estimate resulting heat transfer.
C
C            4. Estimate outlet temperatures
C
C            5. Compute corresponding supply mass flow rate.
C
C            6. Check value of mass flow rate, and go to 1, if necessary.
C
C-----------------------------------------------------------------------
          FA_search_loop: do while ( bUnConverged )

C-----------------------------------------------------------------------
C           Compute heat capacity of supply, exhaust streams using function
C           CPmst01:
C
C
C              heat capacity = (mass flow rate)
C
C                               (inlet-spec-heat) + (outlet-spec-heat)
C                             * --------------------------------------
C                                                 2
C
C-----------------------------------------------------------------------


            fFA_HeatCapacity = ( fMFA_dry + fMFA_vap )
     &          * ( CPmst01(fDBTemp_amb,fHR_amb)
     &                + CPmst01(fDBTemp_supply, fHR_amb)
     &             ) / 2.0

            fEA_HeatCapacity= ( fMEA_dry + fMEA_vap )
     &          * ( CPmst01(fDBTemp_zone,fHR_zone)
     &                + CPmst01(fDBTemp_exhaust, fHR_zone)
     &             ) / 2.0

C-----------------------------------------------------------------------
C           Update HRV efficiency to reflect most recently
C           computed exhaust temperature.
C-----------------------------------------------------------------------

            defrost_operation: if ( .not. bDefrost ) then

C.............Check if vapor pressure in exhaust stream is greater
C.............than can be supported at exhaust stream temperature.
C.............If so, condenstation occurs.
              condens_operation: if ( Psat01( fDBTemp_exhaust ) <
     &          Pvpr01( fHR_zone, Atm_Pressure( site_altitude ) ) ) then

                fVentHREfficiency =
     &             fIMS_VentHREfficiency(iSystem, iVentEff_Condensing )

              else

                fVentHREfficiency =
     &             fIMS_VentHREfficiency(iSystem, iVentEff_Sensible )

              endif condens_operation

            else

              ! Do nothing; fVentHREfficiency assigned above.

            endif defrost_operation

C-----------------------------------------------------------------------
C           Now estimate heat transfer between hot and cold streams
C           using vent heat recovery efficiency:
C
C           heat-transfer = (HR-efficiency) * ( Max heat capacity)
C
C                       * ( Zone air temp - Outside air temp )
C
C-----------------------------------------------------------------------
            fQVent_HR = fVentHREfficiency
     &          * MAX( fFA_HeatCapacity, fEA_HeatCapacity )
     &          * ( fDBTemp_zone - fDBTemp_amb )             ! (W)

C-----------------------------------------------------------------------
C           Now compute resulting supply, exhaust temperatures
C
C                                      heat trasnfer
C           Outlet temp = inlet temp + -------------  (oC)
C                                      heat capacity
C
C-----------------------------------------------------------------------
            fDBTemp_supply = fDBTemp_amb   + fQVent_HR
     &             / fFA_HeatCapacity
            fDBTemp_exhaust = fDBTemp_zone - fQVent_HR
     &             / fEA_HeatCapacity

C-----------------------------------------------------------------------
C           Finally, compute dry air and vapor mass flow rates for
C           fresh air supply stream
C
C                                           (IG constant ) * ( Temperature [K] )
C             mass flow = ( volume flow ) * ------------------------------------
C                                                 Dry air partial pressure
C
C           See  eqns 16 and 17 in Ferguson et al. (2007)
C
C           Note: We're assuming the same vapor pressure at the
C           fresh-air inlet and outlet (that is, moisture in the
C           supply stream doesn't condense). See section 2.5 in
C           Ferguson et al. (2007).
C-----------------------------------------------------------------------


C...........Fresh dry air flow rate (kg/s)
            fMFA_dry = fVentFlow * fIG_R_air
     &           * ( fDBTemp_supply + 273.15 ) / fPPres_amb


C-----------------------------------------------------------------------
C           Test how close bounds are to each other, and reduce
C           search range if necessary.
C-----------------------------------------------------------------------

            check_convergance:
     &      if ( ( fMFA_dry_UL - fMFA_dry_LL ) < 0.01 * fMFA_dry ) then
C.............Bounds differ by less than 1% of estimated value.
C.............Stop iteration.
              bUnConverged = .false.

            else

C.............Bounds differ by greater than 1%. Update bounds and
C.............continue iteration.

              if ( fMFA_dry < ( fMFA_dry_LL + fMFA_dry_UL ) / 2.0 ) then
C...............Estimate is below range midpoint; move upper bound
                fMFA_dry_UL = ( fMFA_dry_LL + fMFA_dry_UL ) / 2.0

              else
C...............Estimate is above range midpoint, move lower bound
                fMFA_dry_LL = ( fMFA_dry_LL + fMFA_dry_UL ) / 2.0

              endif

C.............Estimate mass flow rate as midpoint between lower
C.............and upper bounds.
              fMFA_dry = ( fMFA_dry_LL + fMFA_dry_UL ) / 2.0

            endif check_convergance

C...........Fresh air vapor flow rate (kg/s)
            fMFA_vap = fMFA_dry * fHR_amb

          enddo FA_search_loop

C-----------------------------------------------------------------------
C         Compute moisture mass balance for entire building:
C
C           rate-of-change in moisture = (inlet moisture mass-flow-rate)
C
C                   - ( outlet moisture mass-flow-rate )
C
C         See eqn. 20 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

          fVaporChangeRate = fMFA_vap - fMEA_vap

C-----------------------------------------------------------------------
C         Compute thermal load associated with ventilation
C
C         Vent load = ( 1 - Vent-HR-efficiency )
C
C                        * max-spec-heat * ( Zone temp - OA temp )
C
C                      - 0.5 * ( fan vent power )
C
C         See eqn 24 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

          fVentLoad =   ( 1.0 - fVentHREfficiency )
     &              * MAX ( fFA_HeatCapacity, fEA_HeatCapacity )
     &              * ( fDBTemp_zone - fDBTemp_amb )
     &              - 0.5 * fIMS_VentPower(iSystem, iVentMode)


        case default

C.........Shouldn't happen, as iVentMode is trapped above

      end select vent_mode_ND
      
C.........Return ventilation power consumption: 
      
      fVentPower = fIMS_VentPower(iSystem, iVentMode)
C-----------------------------------------------------------------------
C     Now report data to h3kreports facility
C-----------------------------------------------------------------------

C      h3kreports_init:
C     &if ( .not. bH3KReportsInitialized ( iSystem ) ) then
C
C        if ( iSystem > 9 ) then
C          cFormat = '(A,I2,A)'
C
C        else
C          cFormat = '(A,I1,A)'
C        endif
C
C        write (cRootName(iSystem),cFormat)
C     &        'plant/CETC_HVAC/system_' , iSystem,
C     &        '(IMS)'
C
C        iStrLen(iSystem) = lnblnk( cRootName( iSystem ) )
C
C        bH3KReportsInitialized ( iSystem ) = .true.
C
C      endif h3kreports_init
C
C      call add_to_xml_reporting(
C     &    fIMS_VentFlow(iSystem,iVentMode),
C     &    cRootName(iSystem)(1:iStrLen(iSystem))
C     &       // '/vent/flow_rate',
C     &    'units','(l/s)',
C     &    'IMS ventilation air flow rate.' )
C
C      call add_to_xml_reporting(
C     &    fHR_zone,
C     &    cRootName(iSystem)(1:iStrLen(iSystem))
C     &       // '/return_air/humidity_ratio',
C     &    'units','(kg/kg)',
C     &    'Humidity ratio of return air' )
C
C      call add_to_xml_reporting(
C     &    fHR_amb,
C     &    cRootName(iSystem)(1:iStrLen(iSystem))
C     &       // '/fresh_air/humidity_ratio',
C     &    'units','(kg/kg)',
C     &    'Humidity ratio of fresh air' )


      return
      end subroutine IMS_CharVentilation

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
C  - iVentMode:       Flag indicating ventilation mode
C                         No ventilation = 1,
C                         Low (continuious ventilation) = 2,
C                         High ventilation = 3
C  - fSpaceHLoad:     Combined space heating load (W)
C  - fWaterHLoad:     Combined water heating load (W)
C  - fWaterHDuration: Duration of water heating load (s)
C
C OUTPUTS:
C  - fFuelPower:      Rate of fuel consumption (W)
C  - fElecPower:      Rate of electrical consumption (W)
C  - fFanPower:       Power consumed by circulation fan (W)
C  - fVentPower:      Power consumed by ventilation sytem 
C  - fAddGains:       Additional heating/cooling unaccounted
C                     for in space-heaing load.
C
C=======================================================================
      subroutine IMS_CharPerformance( iSystem,
     &                                iVentMode,
     &                                fInSpaceHLoad,
     &                                fInWaterHLoad,
     &                                fInWaterHLoadDuration,
     &                                fOutFuelPower,
     &                                fOutElecPower,
     &                                fOutFanPower,
     &                                fOutVentPower,
     &                                fOutAuxPower,
     &                                fOutTS_FanOp,  
     &                                fOutAddGains )

      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "IMS_common.h"
C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem          ! system index
      integer iVentMode        ! Ventilation mode
      real fInSpaceHLoad       ! space heating load from simulator (W)
      real fInWaterHLoad       ! water heating load from simulator (W)
      real fInWaterHLoadDuration ! duration of water heating load
                               !    from simulator (s)
      real fOutFuelPower       ! Computed fuel use (kg/s)
      real fOutElecPower       ! Computed electric use (W)
      real fOutFanPower        ! Computed circ. fan elec. use (W)
      real fOutVentPower       ! Computed power consumption for
                               !   ventilation (W).
      real fOutAddGains        ! Additional heat injection/extraction
                               !   that is not accounted for in space-
                               !   heating load (W)
      real fOutAuxPower        !
      real fOutTS_FanOp  
      real fOutFanLowPower
      real fOutFanHighPower
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

      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      character*248 cMsg  ! Message buffer

      logical bCloseToZero     ! Result of close-to-zero comparison
      logical bNoSpaceHLoad    ! Flag space heating load
      logical bNoWaterHLoad    ! Flag for water heating load
      logical bSpaceAndWaterLoad ! Flag for combined load

      real fSpaceHLoad         ! Space heating load met by IMS (W)
      real fWaterHLoad         ! Water heating load met by IMS (W)
      real fWaterHLoadDuration ! Duration of water heating load (s)

      real fBCDSpaceHLoad      ! Space heating load specified in BCD (W)
      real fBCDWaterHFlow      ! Water heating flow rate (W) in BCD
      real fBCDWaterHDuration  ! Duration of water heating load (s)
                               !   from BCD file
      real fBCDWaterHDelTemp   ! Delivery temperature for water heating
                               !   from BCD file (oC)
      real fBCDWaterHMakeupTemp ! Make-up temperature for water heating
                               !   from BCD file (oC)

      real fTS_Duration        ! Duration of timestep (S)

      real fTS_FracWaterH,     ! Fraction of time step spend in
     &     fTS_FracSpaceH,     !   space, water, and combined
     &     fTS_FracSpaceWaterH,!   space and water heating modes.
     &     fTS_NoDemand,
     &     fTS_FracSpaceH_unadj

      real fPLR_SpaceH,        ! Part-load ratios for space, water
     &     fPLR_WaterH         !   heating

      real fSpaceHEff,         ! Space heating efficiency, fan power (W)
     &     fFanPower,          !   and auxiliary sys. power (W)
     &     fAuxPower,          !
     &     fSpaceHPower        !

      real fWaterHEff,         ! Water heater efficiency (-)
     &     fWaterHPower,       !   and power consumption (W)
     &     fWaterHIncPower     !

      real fQCoil              ! Heat transfer at the coil. (W)
      real fQSpaceH_deficit    ! Heating deficit
      real fQFuel_SH,          ! Fuel power required for space-heating,
     &     fQFuel_WH,          !   water-heating functions (W)
     &     fQFuel_SWH

      real fSpaceWaterHPower   ! Power used by both space and water-heating
                               !   mode.

      real fNoDemandPower      ! Power used for ventilation in no-demand
                               !   mode.

      real fDistPower          ! Power required to distribute
                               !   ventilation air.

      real fAddGains_WaterH,      ! Additional heat transfer to zone
     &     fAddGains_SpaceH,      !  that is unaccounted for in 
     &     fAddGains_SpaceWaterH, !  ideal controller's zone heat
     &     fAddGains_NoDemand     !  injection. (W)

      real fQOutput_SH         ! Space heating themal output (W).

      logical bBCDNotFound     ! Flag indicating BCD columns were
                               !   not located correctly.

      real fDay                ! Current time step expressed as a
                               !   real fraction of the day.

C.....Initialization flag for BCD coupling
      logical bBCDCouplingChecked(max_sys)
      integer iNumber
      save bBCDCouplingChecked

      data ( bBCDCouplingChecked(iNumber),
     &       iNumber=1,max_sys ) / max_sys * .false. /

C.....Initialization flag for h3kreprots module
      logical bH3KReportsInitialized(max_sys)
      save bH3KReportsInitialized

      data ( bH3KReportsInitialized(iNumber),
     &       iNumber=1,max_sys ) / max_sys * .false. /

C.....Storage string.
      character*128 cRootName(max_sys)
      save cRootName
      character*8 cFormat
      integer iStrLen(max_sys)
      save iStrLen

C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      integer iWater
      parameter ( iWater = 3 )

      integer iStep
      parameter ( iStep = 1 )

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fIMS_Interpolate    ! Function to interpolate between
                               !   operating points in IMS system
                               !   performance map

      integer LNBlnk           ! Returns length of a string.

      real SHtFld              ! Returns string

      logical bInquire_BC_Name_Exists ! Check if column is defined
                                      !   in BCD facility.

      real fConvert_current_TS_to_DAY ! Function returing day as 
                                      !   a real number

      real fGet_BC_data_by_name ! Function returning value of
                                !   specified bounary condition

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
C     First, check that specified bcd columns could be found. This
C     test really belongs in the HVAC parsing routines, but
C     unfortunately, the HVAC files are parsed prior to the bcd file.
C-----------------------------------------------------------------------
      BCD_Check: if ( .not. bBCDCouplingChecked ( iSystem ) ) then

        bBCDNotFound = .false.

C.......DHW Loads 
        dhw_BCD_check: if (bIMS_DHWLoadsDefinedInBCD(iSystem))  then

C.........DHW Flow rate
          if (
     &         .not. bInquire_BC_Name_Exists(cIMS_BcdDhwFlow(iSystem))
     &        ) then

            write (cMsg, '(A,A,A)')
     &        ' Error parsing IMS model specified DHW flow rate '
     &      //' boundary condition. Column "',
     &        cIMS_BcdDhwFlow(iSystem)
     &             (1:lnblnk(cIMS_BcdDhwFlow(iSystem))),
     &         '" was not found in .bcd file.'

            call edisp248(iuout, cMsg, 80)
            bBCDNotFound = .true.
          endif

C.........DHW Delivery temperature 
          if ( .not.
     &         bInquire_BC_Name_Exists(cIMS_BcdDhwDeliveryTemp(iSystem))
     &        ) then

            write (cMsg, '(A,A,A)')
     &        ' Error parsing IMS model specified DHW delivery '
     &      //' temperature boundary condition. Column "',
     &        cIMS_BcdDhwDeliveryTemp(iSystem)
     &             (1:lnblnk(cIMS_BcdDhwDeliveryTemp(iSystem))),
     &         '" was not found in .bcd file.'

            call edisp248(iuout, cMsg, 80)
            bBCDNotFound = .true.
          endif

C.........DHW makeup temperature
          if ( .not.
     &         bInquire_BC_Name_Exists(cIMS_BcdDhwMakeupTemp(iSystem))
     &        ) then

            write (cMsg, '(A,A,A)')
     &        ' Error parsing IMS model specified DHW makeup '
     &      //' temperature boundary condition. Column "',
     &        cIMS_BcdDhwMakeupTemp(iSystem)
     &             (1:lnblnk(cIMS_BcdDhwMakeupTemp(iSystem))),
     &         '" was not found in .bcd file.'

            call edisp248(iuout, cMsg, 80)
            bBCDNotFound = .true.

          endif

        endif dhw_BCD_check

C.......Space heating loads

        sh_BCD_check: if ( bIMS_SHLoadsDefinedinBCD(iSystem)) then

          if ( .not.
     &         bInquire_BC_Name_Exists(cIMS_BcdSHLoad(iSystem))
     &        ) then

            write (cMsg, '(A,A,A)')
     &        ' Error parsing IMS model specified space-heating '
     &      //' load boundary condition. Column "',
     &        cIMS_BcdSHLoad(iSystem)
     &             (1:lnblnk(cIMS_BcdSHLoad(iSystem))),
     &         '" was not found in .bcd file.'

            call edisp248(iuout, cMsg, 80)
            bBCDNotFound = .true.

          endif

        endif sh_BCD_check

C.......Were all columns found correctly?
        BCD_not_found: if ( bBCDNotFound ) then
C.........No! Inform user and stop simulatiom.
          call edisp248(iUout,
     &        ' Fatal error: IMS model`s specified boundary conditions'
     &      //' could not be found!', 80)
          stop 'Ideal HVAC models: Fatal Error!'

        endif BCD_not_found

C.......Set initialization flag:
        bBCDCouplingChecked(iSystem) = .true.

      endif BCD_Check


C-----------------------------------------------------------------------
C     Check if BCD space heating and DHW loadss have been specified,
C     and if so, collect loads from BCD facility. Otherwise, use
C     loads determined by simulator (and passed in procedure
C     arguements).
C-----------------------------------------------------------------------


C.....Use function fConvert_current_TS_to_DAY to express
C.....current day (and fraction thereof) as a real number.
C.....The BCD facility uses this parameter to determine the
C.....appropriate row.
      fDay = fConvert_current_TS_to_DAY()

      collect_dhw_BCD: if (bIMS_DHWLoadsDefinedInBCD(iSystem)) then
C-----------------------------------------------------------------------
C       Collect specified water flow rate, load duration,
C       delivery and makeup temperatures from bcd file.
C-----------------------------------------------------------------------

C.......Collect DHW flow rate
        fBCDWaterHFlow = fGet_BC_data_by_name(
     &                      cIMS_BcdDhwFlow(iSystem),
     &                      fDay,
     &                      iStep,
     &                      'IMS_CharPerformance(): '
     &                        //'Recovering IMS boundry '
     &                        //'conditions from .bcd File.'
     &                    )

C.......Collect DHW delivery temperature
        fBCDWaterHDelTemp = fGet_BC_data_by_name(
     &                      cIMS_BcdDhwDeliveryTemp(iSystem),
     &                      fDay,
     &                      iStep,
     &                      'IMS_CharPerformance(): '
     &                        //'Recovering IMS boundry '
     &                        //'conditions from .bcd File.'
     &                    )

C.......Collect DHW make-up temperature
        fBCDWaterHMakeupTemp = fGet_BC_data_by_name(
     &                      cIMS_BcdDhwMakeupTemp(iSystem),
     &                      fDay,
     &                      iStep,
     &                      'IMS_CharPerformance(): '
     &                        //'Recovering IMS boundry '
     &                        //'conditions from .bcd File.'
     &                    )

C.......Collect DHW load duration
        fWaterHLoadDuration = fGet_BC_data_by_name(
     &                      cIMS_BcdDhwLoadDuration(iSystem),
     &                      fDay,
     &                      iStep,
     &                      'IMS_CharPerformance(): '
     &                        //'Recovering IMS boundry '
     &                        //'conditions from .bcd File.'
     &                    )

C-----------------------------------------------------------------------
C
C       Compute DHW load:
C
C                       (Cp delivery) + (Cp makeup)
C          DHW Cp Avg. = ---------------------------
C                                   2
C
C          Load = [DHW volume] * [DHW Cp Avg]
C
C                              * ( [Delivery temp.] - [Makeup temp.] )
C
C       See equation 13 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

        fWaterHLoad = fBCDWaterHFlow
     &                 * (
     &                        SHtFld(iWater, fBCDWaterHDelTemp)
     &                      + SHtFld(iWater, fBCDWaterHMakeupTemp)
     &                     ) / 2.0
     &                 *  ( fBCDWaterHDelTemp - fBCDWaterHMakeupTemp )

      else

C.......Use arguements passed by simulator to establish loads
        fWaterHLoad         = fInWaterHLoad
        fWaterHLoadDuration = fInWaterHLoadDuration

      endif collect_dhw_BCD

      collect_sh_BCD: if (bIMS_SHLoadsDefinedInBCD(iSystem)) then
C---------------------------------------------------------------
C       Collect specified space-heating load from bcd file.
C---------------------------------------------------------------

C.......Collect DHW flow rate
        fSpaceHLoad = fGet_BC_data_by_name(
     &                      cIMS_BcdSHLoad(iSystem),
     &                      fDay,
     &                      iStep,
     &                      'IMS_CharPerformance(): '
     &                        //'Recovering IMS boundry '
     &                        //'conditions from .bcd File.'
     &                    )

      else

C.......Use arguement passed from the simulator to establish load.
        fSpaceHLoad = fInSpaceHLoad

      endif collect_sh_BCD

C-----------------------------------------------------------------------
C     Begin characterization of ventilation system.
C-----------------------------------------------------------------------

C.....Assign distrubution fan power consumption based on
C.....ventilation mode:
      vent_mode_Dist: select case ( iVentMode )

        case ( iMode_NoVent )

          fDistPower = 0.0
          fNoDemandPower     = fIMS_ControlsPower(iSystem)    ! (W)

        case ( iMode_LowVent, iMode_HighVent )

          fDistPower = fIMS_IncDistPower(iSystem)

          fNoDemandPower =   fIMS_ControlsPower(iSystem)
     &                     + fIMS_VentPower(iSystem,iVentMode)
     &                     + fDistPower

        case default

C.........Shouldn't happen, as iVentMode is set programmically.
          STOP
     &    'IMS_CharPerformance: ventilation mode must be 1, 2, or 3'

      end select vent_mode_Dist

C-----------------------------------------------------------------------
C     Compute fraction of time step spent in water heating mode.
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
     &              0.0, 1.0, bNoWaterHLoad )

      no_WH_load: if ( bNoWaterHLoad .or.
     &     fWaterHLoad + fIMS_WaterHSkinLoss(iSystem) < 0.0 ) then

        bNoWaterHLoad = .true.
        fTS_FracWaterH = 0.0

      else

        fTS_FracWaterH = fIMS_WaterHSkinLoss(iSystem)
     &       / ( fIMS_WaterHEff(iSystem,iIMS_NumOPWaterH(iSystem))
     &           * fIMS_NominalBurnerOutput(iSystem)  )
     &       + fWaterHLoadDuration / fTS_Duration

        if ( fTS_FracWaterH > 1.0 ) fTS_FracWaterH = 1.0
        if ( fTS_FracWaterH < 0.0 ) fTS_FracWaterH = 0.0


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

      no_SH_load: if ( bNoSpaceHLoad .or.
     &                 fSpaceHLoad < 0.0 ) then

        bNoSpaceHLoad = .true.
        fTS_FracSpaceH = 0.0

      elseif ( .not. bCloseToZero .and.
     &         fIMS_SpaceHMinCapacity(iSystem) > 0.0 ) then

        fTS_FracSpaceH = ( fSpaceHLoad )
     &        / fIMS_SpaceHMinCapacity(iSystem) - fTS_FracWaterH

        fSpaceHLoad = fIMS_SpaceHMinCapacity(iSystem)

      else

        fTS_FracSpaceH = 1.0

      endif no_SH_load

      if ( fTS_FracSpaceH > 1.0 ) fTS_FracSpaceH = 1.0
      if ( fTS_FracSpaceH < 0.0 ) fTS_FracSpaceH = 0.0


      
C-----------------------------------------------------------------------
C     If sum of time step fraction spent in space and water heating modes
C     is greater than unity, the unit must spend some time in a combined
C     space- and water-heating mode. Compute this amount as per
C     equations 3--6 in Ferguson et al. (2007)
C-----------------------------------------------------------------------

      combined_mode: if ( fTS_FracSpaceH + fTS_FracWaterH > 1.0 ) then

        fTS_FracSpaceWaterH =  fTS_FracSpaceH + fTS_FracWaterH - 1.0

        fTS_FracSpaceH_unadj = fTS_FracSpaceH
        fTS_FracSpaceH = 1.0 - fTS_FracWaterH
        fTS_FracWaterH = 1.0 - fTS_FracSpaceH_unadj
    
        
C.......Possibly reset SH load if the unit is in combined 
C.......SH & water heating model for the entire timestep.         
        call eclose ( fTS_FracSpaceH, 0.0, 0.001, bNoSpaceHLoad )
        
C.......Fraction of timestep for which there is no demand
C.......on IMS unit:
        fTS_NoDemand = 0.0

        bSpaceAndWaterLoad = .true.

      else

        fTS_FracSpaceWaterH = 0.0

C.......Fraction of timestep for which there is no demand
C.......on IMS unit:
        fTS_NoDemand = 1.0 - fTS_FracSpaceH - fTS_FracWaterH

        bSpaceAndWaterLoad = .false.

      endif combined_mode

C.....Fraction of timestep for which there is no demand
C.....on IMS unit:
      fTS_NoDemand = 1.0 - fTS_FracSpaceH
     &                   - fTS_FracWaterH
     &                   - fTS_FracSpaceWaterH

      if ( fTS_NoDemand < 0. ) fTS_NoDemand = 0.0

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

        fWaterHIncPower = fIMS_Interpolate( iSystem,
     &                                   fPLR_WaterH,
     &                                   iIMSVar_WaterHPower )
     &

C-----------------------------------------------------------------------
C       Now compute the fuel power required by the IMS for water heating
C       function. See Equation 14 in Ferguson et al. (2007)
C       And make sure total output does not exceed burner capacity
C-----------------------------------------------------------------------
        fQFuel_WH = ( fWaterHLoad + fIMS_WaterHSkinLoss( iSystem ) )
     &     / fWaterHEff

C-----------------------------------------------------------------------
C       And compute total water heating power:
C
C         power = ( incrremental water heating power )
C                  + ( controls power )
C                  + ( ventilation power )
C                  + ( distribution power )
C
C       See Table 1 in Ferguson et al. (2007)
C-----------------------------------------------------------------------
        fWaterHPower = fWaterHIncPower
     &                   + fIMS_ControlsPower(iSystem)
     &                   + fIMS_VentPower(iSystem,iVentMode)
     &                   + fDistPower

      endif water_heating


C-----------------------------------------------------------------------
C     Begin characterization of space heating operation
C-----------------------------------------------------------------------
      space_heating: if ( bNoSpaceHLoad ) then

C.......No space heating load - set results to zero.
        fPLR_SpaceH  = 0.0
        fSpaceHEff   = 0.0
        fFanPower    = 0.0
        fAuxPower    = 0.0
        fSpaceHPower = 0.0
        fQCoil       = 0.0
        fQFuel_SH    = 0.0

      else

C-----------------------------------------------------------------------
C       First, estimate the part-load ratio. See eqn. 25 in
C       Ferguson et al. (2007)
C-----------------------------------------------------------------------


        fPLR_SpaceH = ( fSpaceHLoad )
     &                      / fIMS_SpaceHCapacity(iSystem)
     &                      / fTS_FracSpaceH ! (-)

        if ( fPLR_SpaceH > 1.0 ) fPLR_SpaceH = 1.0
        if ( fPLR_SpaceH < 0.0 ) fPLR_SpaceH = 0.0
        

C-----------------------------------------------------------------------
C       Next, estimate Space-heating efficiency and power
C       consumption by interpolating from performance map. (See
C       equations 27--29 in Ferguson et al. [2007]). Function
C       fIMS_Interpolate handles the interpolation; we need only
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

        fQCoil = fSpaceHLoad - fFanPower

        if  ( fQCoil < 0. ) fQCoil = 0.0

C-----------------------------------------------------------------------
C       Compute the fuel power required by the IMS to supply this heat
C
C                              Q_coil
C         fuel power = -------------------------
C                      space-heaining efficiency
C
C       See Equation 11 in Ferguson et al. (2007).
C-----------------------------------------------------------------------

        fQFuel_SH = fQCoil / fSpaceHEff    ! (W)

C-----------------------------------------------------------------------
C       Compute rate IMS power consumption in heating  mode
C
C        power =  ( fan power ) + ( auxiliary systems power ) 
C                     ( ventilation power ) + ( controls power )
C-----------------------------------------------------------------------

        fSpaceHPower = fFanPower + fAuxPower 
     &                           + fIMS_VentPower(iSystem,iVentMode)
     &                           + fIMS_ControlsPower(iSystem)     ! (W)

      endif space_heating

C-----------------------------------------------------------------------
C     And compute fuel flows and power consumption for combined space
C     and water heating mode [See Table 1. in Ferguson et al. (2007)]
C
C-----------------------------------------------------------------------
      space_and_water_heating: if ( bSpaceAndWaterLoad ) then

        fQFuel_SWH = fQFuel_SH + fQFuel_WH

        fSpaceWaterHPower =   fIMS_ControlsPower(iSystem)
     &                      + fIMS_VentPower(iSystem,iVentMode)
     &                      + fSpaceHPower
     &                      + fWaterHPower

      else

        fQFuel_SWH = 0.0
        fSpaceWaterHPower = 0.0

      endif space_and_water_heating


C-----------------------------------------------------------------------
C     Check that space and water heating fuel requirements are
C     not greater than burner nominal input 
C-----------------------------------------------------------------------

      WH_burner_capacity_exceeded:
     &if ( fQFuel_WH > fIMS_NominalBurnerInput(iSystem) ) then
C.......Water heating requirement exceeds burner capacity
        fQFuel_WH = fIMS_NominalBurnerInput(iSystem)
      endif WH_burner_capacity_exceeded

      SH_burner_capacity_exceeded:
     &if ( fQFuel_SH > fIMS_NominalBurnerInput(iSystem) ) then
C.......Space heating requirement exceeds burner capacity
        fQFuel_SH = fIMS_NominalBurnerInput(iSystem)

      endif SH_burner_capacity_exceeded

      SWH_burner_capacity_exceeded: if ( bSpaceAndWaterLoad .and.
     &   fQFuel_SWH > fIMS_NominalBurnerInput( iSystem ) ) then
C.......Combined space and water heating requirement exceeds
C.......capacity. Compute actual fuel energy used.
        fQFuel_SWH = fIMS_NominalBurnerInput ( iSystem )
      
      endif SWH_burner_capacity_exceeded

C-----------------------------------------------------------------------
C     Compute space-heating thermal output 
C-----------------------------------------------------------------------
      fQOutput_SH = fTS_FracSpaceH
     &       * (
     &             fFanPower
     &           + fQFuel_SH * fSpaceHEff
     &          )
     &   + fTS_FracSpaceWaterH
     &       * (
     &             fFanPower
     &           + ( fQFuel_SWH - fQFuel_WH ) * fSpaceHEff
     &          ) ! (W)

C-----------------------------------------------------------------------
C     Compute space heating deficit, if any:
C
C       Deficit = load - space heating output
C
C-----------------------------------------------------------------------

      fQSpaceH_deficit = fSpaceHLoad - fQOutput_SH !(W)

C-----------------------------------------------------------------------
C     Finally, compute additional heat transfer from unit to
C     space (that is, heat transferred that is not accounted
C     for in ESP-r's ideal control model.) Sources of 
C     additional heat transfer include:
C        - fan power for distribution of ventilation air 
C        - tank losses
C        - un-met space heating loads.
C-----------------------------------------------------------------------

      fOutAddGains =  fIMS_WaterHSkinLoss( iSystem )
     &           + fIMS_ControlsPower ( iSystem )
     &           + fQSpaceH_deficit
     &           + fTS_NoDemand * ( fDistPower )
     &           + fTS_FracWaterH * ( fWaterHIncPower + fDistPower )
     &           + fTS_FracSpaceH * ( fAuxPower )
     &           + fTS_FracSpaceWaterH * ( fWaterHIncPower + fAuxPower )

C-----------------------------------------------------------------------
C     Compute total fuel and electric use
C-----------------------------------------------------------------------
      fOutFuelPower =   fTS_FracSpaceH * fQFuel_SH
     &             + fTS_FracWaterH * fQFuel_WH
     &             + fTS_FracSpaceWaterH * fQFuel_SWH

      fOutElecPower =   fTS_NoDemand * fNoDemandPower
     &             + fTS_FracWaterH * fWaterHPower
     &             + fTS_FracSpaceH * fSpaceHPower
     &             + fTS_FracSpaceWaterH * fSpaceWaterHPower

      fOutFanPower = fFanPower
     &                  * ( fTS_FracSpaceH + fTS_FracSpaceWaterH )

     
C.....Collect other data about fan for use by fan control loop code
C.....in HVACsim
     
      fOutVentPower = fIMS_VentPower(iSystem, iVentMode)

      
C.....Fraction of time that IMS needs fan to be on:      
      fOutTS_FanOp = fTS_FracSpaceH + fTS_FracSpaceWaterH

      
C-----------------------------------------------------------------------
C     Debugging:
C-----------------------------------------------------------------------
 
!     print*, "IMS: space heating load:", fSpaceHLoad
!     print*, "     SH PLR            :", fPLR_SpaceH
!     print*, "     SH eff            :", fSpaceHEff
!     print*, "     SH fan power      :", fFanPower
!     print*, "     SH aux power      :", fAuxPower
!     print*, "     SH elec power     :", fSpaceHPower
!     print*, "     SH fuel power     :", fQFuel_SH
!     print*, "     SH deficit        :", fQSpaceH_deficit
!     !print*, "     Additional xfer   :", fAddGains
!     print*, "     water heating load:", fWaterHLoad
!     print*, "     WH flue/skin ratio:",
!    &                             fIMS_FlueSkinLossRatio(iSystem)
!     print*, "     WH Skin loss      :", fIMS_WaterHSkinLoss(iSystem)
!     print*, "     WH PLR            :", fPLR_WaterH
!     print*, "     WH Eff            :", fWaterHEff
!     print*, "     WH Power          :", fWaterHPower
!     print*, "     WH Fuel power     :", fQFuel_WH
!     print*, "     Vent Power        :",
!    &                             fIMS_VentPower(iSystem,iVentMode)
!     print*, "     Vent Flow         :",
!    &                             fIMS_VentFlow(iSystem,iVentMode)
      !print*, "     Vent Load         :", fVentLoad
      !print*, "     Zone HR           :", fHR_zone
      !print*, "     Amb  HR           :", fHR_amb
 
      !print*, "     EA Mf Dry         :", fMEA_dry
      !print*, "     EA Mf Vap         :", fMEA_vap
      !print*, "     FA MF Dry         :", fMFA_dry
      !print*, "     FA MF Vap         :", fMFA_vap
      !print*, "     FA MF UL          :", fMFA_dry_UL
      !print*, "     FA MF LL          :", fMFA_dry_LL
 
      !print*, "     Temp Zone         :", fDBTemp_zone
      !print*, "     Temp OA           :", fDBTemp_amb
      !print*, "     Temp Exhaust      :", fDBTemp_exhaust
      !print*, "     Temp Supply       :", fDBTemp_supply
      !print*, "     dMV/dt            :", fVaporChangeRate
 

C-----------------------------------------------------------------------
C     Now report data to h3kreports facility
C-----------------------------------------------------------------------

C     h3kreports_init:
C    &if ( .not. bH3KReportsInitialized ( iSystem ) ) then
C
C       if ( iSystem > 9 ) then
C         cFormat = '(A,I2,A)'
C
C       else
C         cFormat = '(A,I1,A)'
C       endif
C
C       write (cRootName(iSystem),cFormat)
C    &        'plant/CETC_HVAC/system_' , iSystem,
C    &        '(IMS)'
C
C       iStrLen(iSystem) = lnblnk( cRootName( iSystem ) )
C
C       bH3KReportsInitialized ( iSystem ) = .true.
C
C     endif h3kreports_init

C     call add_to_xml_reporting(
C    &    fSpaceHLoad,
C    &    cRootName(iSystem)(1:iStrLen(iSystem))
C    &       // '/loads/space_heating',
C    &    'units','(W)',
C    &    'Space-heating load placed on IMS unit' )
C
C     call add_to_xml_reporting(
C    &    fWaterHLoad,
C    &    cRootName(iSystem)(1:iStrLen(iSystem))
C    &       // '/loads/water_heating',
C    &    'units','(W)',
C    &    'Water-heating load placed on IMS unit' )

!       call add_to_xml_reporting(
!      &    fVentLoad,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/loads/ventiltion',
!      &    'units','(W)',
!      &    'Ventilation load placed on IMS unit' )

!     call add_to_xml_reporting(
!    &    fPLR_SpaceH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/efficiency/space_heating',
!    &    'units','(-)',
!    &    'IMS efficiency in space-heating mode' )
!
!     call add_to_xml_reporting(
!    &    fQFuel_SH * fTS_FracSpaceH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/fuel_power/space_heating',
!    &    'units','(W)',
!    &    'Average fuel power used in space-heating mode' )
!
!     call add_to_xml_reporting(
!    &    fQFuel_WH * fTS_FracWaterH ,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/fuel_power/water_heating',
!    &    'units','(W)',
!    &    'Average fuel power used in water-heating mode' )
!
!     call add_to_xml_reporting(
!    &    fQFuel_SWH * fTS_FracSpaceWaterH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/fuel_power/space_and_water_heating',
!    &    'units','(W)', 'Average ' //
!    &    'fuel power used in combined space- and water-heating mode' )
!
!     call add_to_xml_reporting(
!    &    fTS_NoDemand,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/time_step_fraction/no_demand',
!    &    'units','(-)',
!    &    'Fraction of time-step spent with no demand ' )
!
!     call add_to_xml_reporting(
!    &    fTS_FracSpaceH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/time_step_fraction/space_heating',
!    &    'units','(-)',
!    &    'Fraction of time-step spent in space-heating mode.' )
!
!     call add_to_xml_reporting(
!    &    fTS_FracWaterH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/time_step_fraction/water_heating',
!    &    'units','(-)',
!    &    'Fraction of time-step spent in water-heating mode.' )
!
!
!     call add_to_xml_reporting(
!    &    fTS_FracSpaceWaterH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/time_step_fraction/space_and_water_heating',
!    &    'units','(-)',
!    &   'Fraction of time-step spent in space- & water-heating mode.' )
!
!     call add_to_xml_reporting(
!    &    fQOutput_SH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/space_heating_output',
!    &    'units','(W)',
!    &    'IMS unit space-heating thermal output.' )
!
!
!     call add_to_xml_reporting(
!    &    fQSpaceH_deficit,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/unmet_space_heating_loads',
!    &    'units','(W)',
!    &    'Space heating loads unmet by IMS unit.' )
!
!     call add_to_xml_reporting(
!    &    fOutAddGains,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/additional_heat_transfer_to_space',
!    &    'units','(W)',
!    &    'Additional heat transferred from IMS unit to building' )
!
!     call add_to_xml_reporting(
!    &    fIMS_WaterHSkinLoss(iSystem) ,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/skin_losses',
!    &    'units','(W)',
!    &    'Heat transfer through IMS unit`s casing' )
!
!     call add_to_xml_reporting(
!    &    fPLR_WaterH,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/water_heating_PLR',
!    &    'units','(-)',
!    &    'IMS unit part-load-ratio in water heating.' )
!
!     call add_to_xml_reporting(
!    &    fWaterHEff,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/efficiency/water_heating',
!    &    'units','(-)',
!    &    'Efficiency of IMS unit in water-heating operation' )
!
!     call add_to_xml_reporting(
!    &    fIMS_VentPower(iSystem,iVentMode),
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/vent/fan_power',
!    &    'units','(W)',
!    &    'Power used by IMS ventilation fans.' )
!
!     call add_to_xml_reporting(
!    &    fOutFuelPower,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/fuel_power/total',
!    &    'units','(W)',
!    &    'Fuel power used by the IMS unit' )
!
!     call add_to_xml_reporting(
!    &    fOutElecPower,
!    &    cRootName(iSystem)(1:iStrLen(iSystem))
!    &       // '/electric_power/total',
!    &    'units','(W)',
!    &    'Electric consumption of IMS unit' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
! 
!       call add_to_xml_reporting(
!      &    ,
!      &    cRootName(iSystem)(1:iStrLen(iSystem))
!      &       // '/',
!      &    'units','()',
!      &    '' )
     

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




