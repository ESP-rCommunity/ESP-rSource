C ******************************************************************************
C ******************************* GSHP_INPUT ***********************************
C Created by: Julia Purdy
C Initial Creation Date: January 7, 2002
C Copyright CETC 2001

C This common declares and initializes all the general input data required for the
C GSHP simulation.
C---------------------------------------------------------------------------------
      common/GSHP_INPUT/igshp_type,IDdia,OD,Kpipe,pipe_length,
     & Density,Cp,Flow,Tmean,Atemp,DAYo

C--------------------------------------------------------------------------------
C This is the general parameter that defines the GSHP type simulated in
C the current model.
C--------------------------------------------------------------------------------
      INTEGER  igshp_type

C---------------------------------------------------------------------------------
C General parameters for ground source heat pump systems - pipe size and 
C conductivity, heat exchanger length, fluid density, specific heat, and flow
C rate, and soil temperatures.
C---------------------------------------------------------------------------------
C [mm]
      REAL  IDdia            
C [mm]
      REAL  OD           
C [W/mK]       
      REAL  Kpipe    
C [m]     
      REAL  pipe_length 
C [kg/m3]  
      REAL  Density 
C [kJ/kgC]      
      REAL  Cp 
C [L/s]
      REAL  Flow  
C [C]        
      REAL  Tmean 
C [C]       
      REAL  Atemp 
C [day]       
      INTEGER  DAYo

C ********************************************************************************
C ******************************** GSHP_DAILY ************************************
C Created by: Julia Purdy
C Initial Creation Date: January 9, 2002
C Copyright CETC 2001

C This common defines the timestep of the previous day as well as the daily 
C heating or cooling loads required for the GSHP simulation. It also contains the 
C daily entering water temperature (EWT) calculated during the simulation.
C It also defines two variables required from timestep to timestep that 
c accumulate throughout the simulation.
C---------------------------------------------------------------------------------     
      common/GSHP_DAILY/daily_heat_load,daily_cool_load,
     & dQ(750),EWT,Day,TimeDay,Period,StartDay,StopDay,gshp_init_call

C Daily totals for heating and cooling loads, summed in GSHP_load subroutine,
c written to an array, so that previous days values can be accessed.
      REAL  daily_heat_load
      REAL  daily_cool_load
      REAL  dQ

c Daily entering water temparature, as calculated for the specific ground
c loop configuration. It is initially set to 5.0 degrees Celcius.
      REAL  EWT

c Day of the simulation, starting with the StartDay (including presimulation time).
      INTEGER  Day 

c The length of each simuliaton, one day
      REAL  TimeDay

C The simulation period, start day (including startup period) and stop day.
C [day]
      INTEGER  Period
C [day]           
      INTEGER  StartDay  
C [day]       
      INTEGER  StopDay                   

c The following is a dummy variable used to determine if the GSHP_init subroutine 
c has been called. If it is TRUE then the subroutine has not been called, 
c if it is FALSE, then the subroutine has been called and will not be called again.
      LOGICAL  gshp_init_call

C ********************************************************************************
C ***************************** GSHP_COP *****************************************
C Created by: Julia Purdy
C Initial Creation Date: Novemer 21, 2002
C Copyright CETC 2002

C This common defines the ground source heat pump coefficient of performance
C values. 
C These values are used in several subroutines.
C The load-weighted COP is passed to the main GSHP simulation subroutine 
c GSHPSIM, where the daily system COP is calculated.
C---------------------------------------------------------------------------------
      common/GSHP_COP/load_weighted_GSHP_COP,GSHP_COP

c Load-weighted daily ground source heat pump COP, calculated in 
c weighted_GSHP_cop subroutine.
      REAL  load_weighted_GSHP_COP

c Ground source heat pump COP, calculated for heating or cooling mode.
      REAL  GSHP_COP














