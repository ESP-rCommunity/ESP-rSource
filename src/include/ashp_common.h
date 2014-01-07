c Use common for heat pump input data
C (JWH remove continuation lines based on F90 warnings)
      common/HEAT_PUMP_INPUT_DATA/
     & ss_cop(max_sys), shrr(max_sys),
     & time_constant(max_sys),
     & itemp_control(max_sys),cutoff_temp(max_sys), 
     & ibackup_heat(max_sys),ibackup_sys_num(max_sys), 
     & icooling_type(max_sys), bfr(max_sys), isys_num_heating(max_sys),
     & ahp(max_sys), bhp(max_sys), chp(max_sys),dhp(max_sys), 
     & outdoor_air_flow(max_sys,8760), ieconomizer_control(max_sys), 
     & economizer_min_out_air_flow(max_sys),
     & economizer_ind_temp_setpoint(max_sys),
     & economizer_out_temp_limitcon(max_sys),
     & economizer_out_enth_limitcon(max_sys),
     & outdoor_fan_power(max_sys),
     & iHeatPumpType(max_sys)
     

c Real variables in heat pump input data
      REAL ss_cop,time_constant,shrr,cutoff_temp,bfr,ahp,bhp,chp,dhp 
      REAL outdoor_air_flow
      REAL economizer_min_out_air_flow
      REAL economizer_ind_temp_setpoint
      REAL economizer_out_temp_limitcon 
      REAL economizer_out_enth_limitcon
      REAL outdoor_fan_power

      
            
c Integer variables in heat pump input data
      INTEGER itemp_control
      INTEGER ibackup_heat 
      INTEGER ibackup_sys_num 
      INTEGER icooling_type
      INTEGER isys_num_heating 
      INTEGER ieconomizer_control

      INTEGER iHeatPumpType
      
      INTEGER, PARAMETER :: iASHP_typical       = 1 
      INTEGER, PARAMETER :: iASHP_good          = 2 
      INTEGER, PARAMETER :: iASHP_poor          = 3 
      INTEGER, PARAMETER :: iASHP_CCASHP_CEF    = 4 
      INTEGER, PARAMETER :: iASHP_CCASHP_CCHT   = 5 
      INTEGER, PARAMETER :: iASHP_CCASHP_MANUF  = 6 
      
   

c Common for air-conditioner output
      common/output_acsystem/
     & total_elec_consumption(max_sys),
     & elec_compressor(max_sys),
     & elec_outdoor_fan(max_sys),
     & elec_indoor_fan(max_sys),
     & average_cop(max_sys),
     & coil_sensible_load(max_sys),
     & coil_latent_load(max_sys),
     & coil_total_load(max_sys)

      REAL total_elec_consumption
      REAL elec_compressor
      REAL elec_outdoor_fan
      REAL elec_indoor_fan,average_cop
      REAL coil_sensible_load
      REAL coil_latent_load
      REAL coil_total_load



