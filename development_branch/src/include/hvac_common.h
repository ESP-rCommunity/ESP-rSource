c Common for hvac system input data. These variables are used for hvac systems
c modeled ideally
      common/HVAC_INPUT_DATA/site_altitude, num_hvac_sys,
     &ihvac_type(max_hvac_types), ipriority(max_sys),
     &isys_type(max_hvac_vars), num_sys_zones(max_sys),
     &ss_capacity(max_sys), flow_rate(max_sys),
     &flow_rate_r(max_sys), ifan_operation(max_sys),
     &ifan_position(max_sys), fan_power(max_sys),
     &fan_power_auto(max_sys), ifan_position_r(max_sys),
     &fan_power_r(max_sys), draft_fan_power(max_sys),
     &isys_zone_num(max_sys,mcom), iduct_sys_flag(max_sys),
     &sys_zone_cap_frac(max_sys,mcom), icontrol_function(max_sys)

      common/HVAC_fuel_type/iFuel_type(max_sys)

c Declare integer values in HVAC_INPUT_DATA common
      INTEGER num_hvac_sys, ihvac_type, ipriority, isys_type,
     &num_sys_zones, ifan_operation, ifan_position, ifan_position_r,
     &isys_zone_num, iduct_sys_flag, icontrol_function, iFuel_type

c Declare real parameters in HVAC_INPUT_DATA common
      REAL site_altitude, ss_capacity, flow_rate, flow_rate_r,
     &fan_power, fan_power_auto, fan_power_r, draft_fan_power,
     &sys_zone_cap_frac

c Common for time step computed parameters of HVAC system

      COMMON/hvac_timestep_parameters/outdoor_air_inf_cond(mcom),
     &coil_moist_in(mcom),coil_moist_out1(mcom),
     &coil_moist_out2(mcom),cont_fun_cool_cap(mcom),output_flag,
     &cont_fun_heat_cap(mcom)

      REAL outdoor_air_inf_cond,coil_moist_in,coil_moist_out1,
     &coil_moist_out2,cont_fun_cool_cap,cont_fun_heat_cap

      LOGICAL output_flag

c Common for underheated cooling

       common/UNDER_HEAT_COOL_HOURS/ihrs_under_heat(max_sys),
     &                             ihrs_under_cool(max_sys)

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
