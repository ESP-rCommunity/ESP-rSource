c Common for hvac system input data. These variables are used for hvac systems
c modeled ideally

      integer iFurnaceTokenCount
      parameter (iFurnaceTokenCount = 7)
      integer iIMSTokenCount
      parameter (iIMSTokenCount = 1)
      integer iBaseboardTokenCount
      parameter (iBaseboardTokenCount = 4)
      integer iHPTokenCount
      parameter (iHPTokenCount = 24)
      integer iMAXTokenCount
      parameter (iMAXTokenCount = iHPTokenCount)


      common/HVAC_parsing/iHVACFileVersion,
     &bParseError,
     &cFurnaceTokenNames(iFurnaceTokenCount),
     &cBaseboardTokenNames(iBaseboardTokenCount),
     &cIMSTokenNames(iIMSTokenCount),
     &cHPTokenNames(iHPTokenCount),
     &bFurnaceTokenSet(max_sys,iFurnaceTokenCount),
     &bBaseboardTokenSet(max_sys,iBaseboardTokenCount),
     &bIMSTokenSet(max_sys,iIMSTokenCount),
     &bHPTokenSet(max_sys,iHPTokenCount),
     &fFurnaceTokenVals(max_sys,iFurnaceTokenCount),
     &fBaseboardTokenVals(max_sys,iBaseboardTokenCount),
     &fIMSTokenVals(max_sys,iIMSTokenCount),
     &fHPTokenVals(max_sys,iHPTokenCount),
     &iDataCount(max_sys),
     &cTokens(max_sys,iMAXTokenCount),
     &fTokenVals(max_sys,iMAXTokenCount)


      character*72 cFurnaceTokenNames,
     &             cBaseboardTokenNames,
     &             cIMSTokenNames, cHPTokenNames, cTokens
      logical bFurnaceTokenSet, bIMSTokenSet, bBaseboardTokenSet,
     &        bHPTokenSet
      real fFurnaceTokenVals, fBaseboardTokenVals, fIMSTokenVals,
     &     fHPTokenVals, fTokenVals

      common/HVAC_INPUT_DATA/
     &site_altitude, num_hvac_sys,
     &ihvac_type(max_hvac_types), ipriority(max_sys),
     &isys_type(max_hvac_vars), num_sys_zones(max_sys),
     &ss_capacity(max_sys), flow_rate(max_sys),
     &flow_rate_r(max_sys), ifan_operation(max_sys),
     &ifan_position(max_sys), fan_power(max_sys),
     &fan_power_auto(max_sys), ifan_position_r(max_sys),
     &fan_power_r(max_sys), draft_fan_power(max_sys),
     &isys_zone_num(max_sys,mcom), iduct_sys_flag(max_sys),
     &sys_zone_cap_frac(max_sys,mcom), icontrol_function(max_sys),
     &cSys_Name(max_sys), iSys_Function(max_sys), iSys_Backup(max_sys),
     &cSys_Backup(max_sys)

c Declare integer values in HVAC_INPUT_DATA common
      INTEGER num_hvac_sys, ihvac_type, ipriority, isys_type,
     &num_sys_zones, ifan_operation, ifan_position, ifan_position_r,
     &isys_zone_num, iduct_sys_flag, icontrol_function, iDataCount,
     &iHVACFileVersion, iSys_Function, iSys_Backup
     

c Declare real parameters in HVAC_INPUT_DATA common
      REAL 
     &site_altitude, ss_capacity, flow_rate, flow_rate_r,
     &fan_power, fan_power_auto, fan_power_r, draft_fan_power,
     &sys_zone_cap_frac

      logical bParseError
      character*72 cSys_Name,cSys_Backup

c Common for time step computed parameters of HVAC system

      COMMON/hvac_timestep_parameters/outdoor_air_inf_cond(mcom),
     &coil_moist_in(mcom),coil_moist_out1(mcom),
     &coil_moist_out2(mcom),cont_fun_cool_cap(max_cont_fun),output_flag

      REAL outdoor_air_inf_cond,coil_moist_in,coil_moist_out1,
     &coil_moist_out2,cont_fun_cool_cap

      LOGICAL output_flag

c Common for underheated cooling

       common/UNDER_HEAT_COOL_HOURS/ihrs_under_heat(max_sys),
     &                             ihrs_under_cool(max_sys)

c Declare integer in UNDER_HEAT_COOL_HOURS common
      INTEGER ihrs_under_heat, ihrs_under_cool
