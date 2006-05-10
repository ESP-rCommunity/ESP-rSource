C------------------------------------------------------------------------------------
C     local named constants 
C------------------------------------------------------------------------------------
C.....tank status:
      integer sta_primary      ! primary inventory
      integer sta_auxiliary    ! secondary inventory
      integer sta_freefloat    ! primary inventory
      parameter (sta_primary      = 2) ! primary inventory
      parameter (sta_auxiliary    = 1) ! secondary inventory
      parameter (sta_freefloat    = 0) ! primary inventory
C.....Storage systems
      integer cold_storage      ! cold inventroy
      integer hot_storage       ! hot inventory
      PARAMETER (cold_storage      = 1) ! cold inventroy
      PARAMETER (hot_storage       = 2) ! hot inventory
C.....algorithms
      integer ALG_freefloat    ! No control
      integer ALG_constant     ! Constant electric production
      integer ALG_electric_L   ! Electric load following
      integer ALG_thermal_L    ! Thermal PID load following
      integer ALG_thermal_P    ! Thermal PID load priority
C      integer ALG_average  ! Running average (presently not supported)
      parameter (ALG_freefloat    = 0) ! No control
      parameter (ALG_constant     = 1) ! Constant electric production
      parameter (ALG_electric_L   = 2) ! Electric load following
      parameter (ALG_thermal_L    = 3) ! Thermal PID load following
      parameter (ALG_thermal_P    = 4) ! Thermal PID load priority
C      parameter (ALG_average     = 5)! Running average (presently not supported)
C.....Fuel cell model types
      integer noFC_Model       ! No fuel cell model in use
      integer SOFC_Model       ! SOFC model in use
      integer PEFC_Model       ! PEFC model in use
      integer Stirling_model   ! Stirling model in use
      integer A42_SOFC_model   ! Annex 42 SOFC model in use
      parameter (noFC_Model       = 0) ! No fuel cell model in use
      parameter (SOFC_Model       = 1) ! SOFC model in use
      parameter (PEFC_Model       = 2) ! PEFC model in use
      parameter (Stirling_model   = 3) ! Stirling model in use
      parameter (A42_SOFC_model   = 4) ! Annex 42 SOFC model in use
C.....Reset period - length of time controller keeps TAC in heat-dump/off states after
C.....an overcharge has occured and normal operation can be resumed.
      real Reset_period     ! Minutes
      parameter (Reset_period     = 25.0 ) ! Minutes
C.....Thermal priority control primary-auxiliary threshold: primary tank error threshold
C.....above which DGS controller will meet demands on primary tank, and below which 
C.....DGS controller will meet demands on which ever tank has highest demand.
      real Error_threshold  ! 0 <Error_threshold < 1
      parameter (Error_threshold  = 0.2 ) ! 0 <= Error_threshold <= 1  

C-------------------------------------------------------------------------------------
C     PARAMETERS read from the input file:
C-------------------------------------------------------------------------------------

C.....Timestep output required flag
      integer  iTS_out        ! Timestep output req'd flag (0-1)

C.....Misc system data
      real  COGEN_max_W       ! COGEN system max output (W)
      real  COGEN_min_W       ! cogen system min output (W)   
      real  COGEN_con_W       ! cogen system constant output (W)  

      integer  Thermal_p         ! Thermal priority (0-1)

C.....Hot storage control data 
      integer  HS_start          ! Heating season start (1-365)
      integer  HS_end            ! Heating season end   (1-365)
      integer  HWT_algorithm  ! hot storage algorithm:
      real  HWT_T_sl          ! Hot tank lower setpoint (oC)
      real  HWT_T_su          ! Hot tank upper setpoint (oC)
      real  HWT_coeff_a       ! Coefficient for target temp (0<a<1)
      real  HWT_coeff_Kp      ! Proportional gain (0<Kp<oo)
      real  HWT_coeff_Ki      ! Integral gain (0<Ki<oo) 
      real  HWT_coeff_Kd      ! Derivitive gain (0<Kd<oo)
      real  HWT_per_int       ! Integral period (hours)
      real  HWT_per_der       ! Derivitive period (hours)

C.....Cold storage control data 
      integer  CS_start          ! Cooling season start (1-365)
      integer  CS_end            ! Cooling season end   (1-365)
      integer  CWT_algorithm  ! Cold storage algorithm:
      real  CWT_T_sl          ! Cold tank lower setpoint (oC)
      real  CWT_T_su          ! Cold tank upper setpoint (oC)
      real  CWT_coeff_a       ! Coefficient for target temp (0<a<1)
      real  CWT_coeff_Kp      ! Proportional gain (0<Kp<oo)
      real  CWT_coeff_Ki      ! Integral gain (0<Ki<oo) 
      real  CWT_coeff_Kd      ! Derivitive gain (0<Kd<oo)
      real  CWT_per_int       ! Integral period (hours)
      real  CWT_per_der       ! Derivitive period (hours)

      real  W_elec_demand     ! Building + HVAC electric loads 

      real  CWT_T             ! CWT temperature (oC)
      real  HWT_T             ! HWT temperature (oC)

      real  CWT_T_target      ! CWT target temperature
      real  HWT_T_target      ! HWT target temperature 

      logical  bHS_flag        ! heating season flag (0-1)
      logical  bCS_flag        ! cooling season flag (0-1)

      integer hs_flag, cs_flag
      
      logical bHW_tank        ! flag indicating a hot water tank is present
      logical bCW_tank        ! flag inicating a cold water tank is present
      
      Integer  PRI_algorithm  ! Specifies which algorithm to use when both tanks 
                              ! require heating or cooling 

      integer  CWT_status     ! Status of cold water tank
      integer  HWT_status     ! Status of hot water tank

      real  COGEN_tar_W       ! Target electrical output 
      integer  FC_TYPE        ! Integer identifying the type of fuel cell model used      

C-------------------------------------------------------------------------------------
C     Controller data
C-------------------------------------------------------------------------------------
      integer HWT_index         ! pointer to hot water tank component (<0 if not HWT)
      integer HWT_node          ! pointer to node in hot water tank
      integer CWT_index         ! pointer to cold water tank component (<0 if not HWT)
      integer CWT_node          ! pointer to node in cold water tank

C-------------------------------------------------------------------------------------
C     PID data. Arrays are allocated dynamically because very large integral
C     (somewhat plausable) and derivitive (not so plausable) periods could be specified. 
C     At 5-minute timesteps, a one month integral period will require 60480 array 
C     locations.
C-------------------------------------------------------------------------------------
      real  CWT_error         ! CWT error signal
      real  HWT_error         ! HWT error signal
      real  CWT_control       ! CWT control signal
      real  HWT_control       ! HWT control signal
 
      real  DG_control_signal   ! Cogen unit control signal (0-1)
      integer TAC_Cooling_Signal  ! TAC cooling control signal (on-off)
      integer TAC_HeatDump_Signal ! TAC heat dump control signal (on-off)

      real  HWT_overcharge_t    ! Counters tracking the number of minutes
      real  CWT_overcharge_t    ! an overcharge condition has persisted on
                                ! the hot and cold tanks

      real HWT_undercharge_t    ! Counters tracking the number of minutes
      real  CWT_undercharge_t   ! since last  overcharge condition on the
                                ! hot and cold tanks

C.....Contorller array dimensions
      integer HWT_n_max_int           ! HWT integral period
      integer HWT_n_max_der           ! HWT derivative period
      integer CWT_n_max_int           ! CWT integral period
      integer CWT_n_max_der           ! CWT derivative period
      parameter(HWT_n_max_int=288)    
      parameter(CWT_n_max_int=288) 
      parameter(HWT_n_max_der=288)    
      parameter(CWT_n_max_der=288)

C-------------------------------------------------------------------------------------
C     Misc local counters, pointers, indexes and flags.
C-------------------------------------------------------------------------------------
      logical First_Run_Done    ! Flag indicating if this is the first time the 
                                ! controller has been called.
      integer FC_index          ! Pointer to fuel cell in plant index
      integer TAC_index         ! Pointer to TAC index 
      integer ii                ! misc. counter
      logical config_output     ! flag indicating controller configuration should be 
                                ! written to the screen.
      logical fatal_error       ! flag indicating that the simulation must be halted



C------------------------------------------------------------------------------------
C     local/ESP-r Variables
C------------------------------------------------------------------------------------
      REAL thermal_control_signal ! cogen unit control signal determined by PID controller
      REAL electrical_control_signal ! cogen unit electrical control signal
      LOGICAL bInStartUp        ! logical variable indicating building is in 
                                ! startup
      INTEGER STO_priority      ! Variable indicating which tank has priority

      LOGICAL bCWT_overcharge   ! logical variable indicating if CWT is too cold
      LOGICAL bHWT_overcharge   ! logical variable indicating if HWT is too hot
      INTEGER  n_plant_time     ! # plant timesteps /hour
      REAL dt_timestep          ! duration of timesteps
      REAL q_par_elec           ! Dummy variable storing HVAC parasitic loads. Not used.

      integer iStirling_control_mode ! Flag indicating electric loads are to be met.
