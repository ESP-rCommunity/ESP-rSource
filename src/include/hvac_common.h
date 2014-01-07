c Common for hvac system input data. These variables are used for hvac systems
c modeled ideally

C      integer iFurnaceTokenCount
C      parameter (iFurnaceTokenCount = 7)
C      integer iIMSTokenCount
C      parameter (iIMSTokenCount = 1)
C      integer iBaseboardTokenCount
C      parameter (iBaseboardTokenCount = 4)
C      integer iHPTokenCount
C
C      parameter (iHPTokenCount = 23)
C      integer iMAXTokenCount
C      parameter (iMAXTokenCount = iHPTokenCount)


      common/HVAC_parsing/iHVACFileVersion,
     &bParseError,
     &cFurnaceTokenNames,
     &cBaseboardTokenNames(iBaseboardTokenCount),
     &cP10_IMSTokenNames(iP10_IMSTokenCount),
     &cHPTokenNames(iHPTokenCount),
     &fFurnaceTokenVals(max_sys,iFurnaceTokenCount),
     &fBaseboardTokenVals(max_sys,iBaseboardTokenCount),
     &fP10_IMSTokenVals(max_sys,iP10_IMSTokenCount),
     &fHPTokenVals(max_sys,iHPTokenCount),
     &iDataCount(max_sys),
     &cTokens(max_sys,iMAXTokenCount),
     &fTokenVals(max_sys,iMAXTokenCount),cTokenVals,
     &iUnit_Function(max_sys), 
     &bHVACVersion2
     

      character*72 cFurnaceTokenNames(iFurnaceTokenCount),
     &             cBaseboardTokenNames,
     &             cP10_IMSTokenNames, cHPTokenNames, cTokens,
     &              cTokenVals(max_sys,iMAXTokenCount)
      real fFurnaceTokenVals, fBaseboardTokenVals, fP10_IMSTokenVals,
     &     fHPTokenVals, fTokenVals
     &     
      integer iUnit_Function
      logical bHVACVersion2

      common/HVAC_INPUT_DATA/site_altitude, num_hvac_sys,
     &ihvac_type(max_hvac_types), ipriority(max_sys),
     &isys_type(max_hvac_vars), num_sys_zones(max_sys),
     &ss_capacity(max_sys), flow_rate(max_sys),
     &flow_rate_r(max_sys), ifan_operation(max_sys),
     &ifan_position(max_sys), fan_power(max_sys),
     &fan_power_auto(max_sys), ifan_position_r(max_sys),
     &fan_power_r(max_sys), draft_fan_power(max_sys),
     &isys_zone_num(max_sys,mcom), iduct_sys_flag(max_sys),
     &sys_zone_cap_frac(max_sys,mcom), icontrol_function(max_sys),
     &izonecontrol(max_sys),      
     &var_flow_rate(max_sys,mcom),
     &var_fan_power(max_sys,mcom),
     &sys_zone_cap_frac_initial(max_sys,mcom),
     &sys_zone_cap_frac_ret_air(max_sys,mcom),
     &cSys_Name(max_sys), iSys_Function(max_sys), iSys_Backup(max_sys),
     &cSys_Backup(max_sys),bFan_AutoSized(max_sys),
     &bFan_Auto_AutoSized(max_sys), bFan_Rated_AutoSized(max_sys)

      common/HVAC_fuel_type/iFuel_type(max_sys)

Common for fan control loops: 
      integer max_fan_loops                  ! Number of loops permitted
      parameter  ( max_fan_loops = max_sys ) 
      integer iFanControlLoop(max_sys)       ! Index indicating which 
                                             ! loop a given system is on
      
      real fFanSysPLR_lo(max_sys)            ! Fraction that a system 
                                             ! needs the fan on for 
                                             ! in low-speed (0-1)
                                             
      real fFanSysPLR_hi(max_sys)            ! Fraction that a system 
                                             ! needs the fan on for 
                                             ! in high-speed (0-1)
                                             
      real fFanSysPower_off(max_sys)         ! Fan power consumption in 
                                             ! off mode. (W)
      
      real fFanSysPower_lo(max_sys)          ! Fan power consumption in 
                                             ! low-speed (W)
      
      real fFanSysPower_hi(max_sys)          ! Fan power consumption in 
                                             ! high-speed (W)
                                             
      logical bSysHasFanCtlLoop(max_sys)     ! Flag indicating system 
                                             ! is connected to a fan 
                                             ! control loop 
      
      logical bFanCtlLoopActive(max_fan_loops) ! Flag indicating a fan conrol 
                                               ! loop is active 
      
      common/HVAC_Fan_Control/iFanControlLoop,
     &                        fFanSysPLR_hi,
     &                        fFanSysPLR_lo,
     &                        fFanSysPower_off,
     &                        fFanSysPower_lo,
     &                        fFanSysPower_hi,
     &                        bSysHasFanCtlLoop,
     &                        bFanCtlLoopActive 
      
      
      
C Declare miscellaneous parameter storage array 
      common/HVAC_misc_parameters/
     &bMiscTokenValsDefined,
     &iMiscParamCount,
     &cMiscTokens,
     &cMiscVals
      logical bMiscTokenValsDefined(max_sys)
      character*72 cMiscTokens(max_sys,iMAXTokenCount)
      character*72 cMiscVals(max_sys,iMAXTokenCount)
      integer iMiscParamCount(max_sys)



c Declare integer values in HVAC_INPUT_DATA common
      INTEGER num_hvac_sys, ihvac_type, ipriority, isys_type,
     &num_sys_zones, ifan_operation, ifan_position, ifan_position_r,
     &isys_zone_num, iduct_sys_flag, icontrol_function, iFuel_type,
     &iDataCount,
     &iHVACFileVersion, iSys_Function, iSys_Backup,iZonecontrol
     

C....flag for zone control (0 = no zone control, 1 = zone control )

c Declare real parameters in HVAC_INPUT_DATA common
      REAL 
     &site_altitude, ss_capacity, flow_rate, flow_rate_r,
     &fan_power, fan_power_auto, fan_power_r, draft_fan_power,
     &sys_zone_cap_frac, 

      

C....Variable flow rate [m3/s] as a function of the number of calling zones
     &var_flow_rate,

C....Variable fan power [W] corresponding to the variable flow rate
C....as a function of the number of calling zones
     &var_fan_power,
     
C....Initial fractions as defined in *.hvac file. Zone capacity fractions
C....become variable when zoning control is enabled so need to save these
C....initial values for calculations of future timestep capacity fractions.
     &sys_zone_cap_frac_initial,
     
C....Return air zone capacity fractions. These are fixed fractions that are
C....indpendent of zoning calls. 
     &sys_zone_cap_frac_ret_air

      logical bParseError, bFan_AutoSized, bFan_Auto_AutoSized,
     &        bFan_Rated_AutoSized

      character*72 cSys_Name,cSys_Backup

c Common for time step computed parameters of HVAC system

      COMMON/hvac_timestep_parameters/outdoor_air_inf_cond(mcom),
     &coil_moist_in(mcom),coil_moist_out1(mcom),
     &coil_moist_out2(mcom),cont_fun_cool_cap(mcom),output_flag,
     &cont_fun_heat_cap(mcom), zone_call_heat(max_sys,mcom),
     &zone_call_cool(max_sys,mcom),sys_on_time(max_sys)

      REAL outdoor_air_inf_cond,coil_moist_in,coil_moist_out1,
     &coil_moist_out2,cont_fun_cool_cap,cont_fun_heat_cap,sys_on_time

      LOGICAL output_flag
      
C.....flags to keep track of zone heating/cooling calls
      LOGICAL zone_call_heat
      LOGICAL zone_call_cool

c Common for underheated cooling

       common/UNDER_HEAT_COOL_HOURS/ihrs_under_heat(max_sys),
     &                              ihrs_under_cool(max_sys)

c Declare integer in UNDER_HEAT_COOL_HOURS common
      INTEGER ihrs_under_heat, ihrs_under_cool


C.....Commons for storing HVAC system names, lengths
      common/HVAC_H3kNames/iHVACTypeLen,cHVACTypes
      character*23 cHVACTypes(9)
      integer iHVACTypeLen(9)

C.....Commons for storing interesting data from HVAC models
      common/HVAC_Report_to_H3K/fASHP_AC_Coil_Load_Sensible,
     &                          fASHP_AC_Coil_Load_Latent,
     &                          fASHP_AC_Coil_Load_Total,
     &                          bLinked,
     &                          iLinkedSystem,
     &                          bSuppressThisSystem,
     &                          bTwoModes,
     &                          fH3KActualCapacity

C.....Flags for tracking heating/cooling components of heat-pump systems.
      integer iLinkedSystem(max_sys)
      logical bSuppressThisSystem(max_sys), bLinked(max_sys)
      logical bTwoModes(max_sys)

C.....ASHP cooling load on coil (W)
      real fASHP_AC_Coil_Load_Sensible(max_sys)
      real fASHP_AC_Coil_Load_Latent(max_sys)
      real fASHP_AC_Coil_Load_Total(max_sys)

C.....Actual system capacity under given conditions at
C.....current timestep (W)

      real fH3KActualCapacity(max_sys)
      
C Common for day/period information for Conventional Cooling
      common/HVAC_Period_Information/
     &        itotal_num_day_periods,
     &        num_out_air_day_types(max_sys),
     &        num_outside_air_day_periods(max_sys,10),
     &        iend_hour_out_air_daytype(max_sys,8760),
     &        iend_hour_out_air_dayperiod(max_sys,100),
     &        day_period_out_air_flow(max_sys,100)

C.....day/period information for Conventional Cooling
      INTEGER num_out_air_day_types,
     &        itotal_num_day_periods,
     &        num_outside_air_day_periods,
     &        iend_hour_out_air_daytype,
     &        iend_hour_out_air_dayperiod
     
      REAL day_period_out_air_flow


C Common for additional "casual" gains associated with HVAC systems ---
C that is, additional heat trasnfer not accounted for in the zone's
C ideal controls. These amounts include the resulting 'defecit'
C occuring when the HVAC system cannot provide the heat transfer
C injected into the space by the ideal controls. 

      common/HVAC_additional_gains/bIdealHVACSysGain(max_sys),
     &                             fIdealHVACSysAddGain(max_sys),
     &                             fIdealHVACSysConGainFrac(mcom),
     &                             fIdealHVACSysRadGainFrac(mcom),
     &                             fIdealHVACSysLatGainFrac(mcom),
     &                             bIdealHVACZoneGain(mcom),
     &                             fIdealHVACZoneConGain(mcom),
     &                             fIdealHVACZoneRadGain(mcom),
     &                             fIdealHVACZoneLatGain(mcom)


C Flag indicating presense of additional gains from idealized hvac
C systems
      logical bIdealHVACSysGain

C Total gain from a HVAC system
      real fIdealHVACSysAddGain

C HVAC system conective/latent/radiant fractions

      real fIdealHVACSysConGainFrac,
     &     fIdealHVACSysRadGainFrac,
     &     fIdealHVACSysLatGainFrac


C Logical flag indicating if zone has associated gains from idealized
C hvac components.
      logical bIdealHVACZoneGain

C Total gain into a zone from all HVAC systems (convective,
C radiant and latent gains)
      real fIdealHVACZoneConGain,
     &     fIdealHVACZoneRadGain,
     &     fIdealHVACZoneLatGain


