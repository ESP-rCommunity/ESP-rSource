C******************************************************************************
C******************************* GCEP_INPUT ***********************************
C Created by: Patrice Pinel
C Initial Creation Date: October, 2002
C
C This common declares and initializes all the general input data required for the
C GCEP simulation.
C GCEP stands for: Ground Coupling Ecole Polytechnique
C---------------------------------------------------------------------------------
      common/GCEP_INPUT_GENERAL/
     & GCEP_K_s, GCEP_Alpha_s, GCEP_Rho_s, GCEP_K_g, GCEP_D_bore, 
     & GCEP_Deapth_bore, GCEP_N_bor_x, GCEP_N_bor_y, GCEP_L_bor_x, 
     & GCEP_L_bor_y, GCEP_Tube_Case, GCEP_TypeTubes, GCEP_K_t, 
     & GCEP_pipe_length, GCEP_K_f, GCEP_Rho_f, GCEP_Cp_f, GCEP_Mu_f, 
     & GCEP_Tg_ND,GCEP_Tg_init,GCEP_Flow,GCEP_Time_Tg,GCEP_Di_tubes, 
     & GCEP_Do_tubes, GCEP_Ndx, GCEP_Ndy, GCEP_Ndx_1b, GCEP_Ndy_1b, 
     & GCEP_LoadHis(589),    
c            ! 589 = NHis = =1+Xm+Xw+Xd+Xh (see parameters)
     & GCEP_Tg, GCEP_Tw, GCEP_Tw_1b, GCEP_Tpen, GCEP_Xy, 
     & GCEP_Nyear, GCEP_Nmonth, GCEP_Nweek, GCEP_Nday, GCEP_Nhour, 
     & GCEP_Py, GCEP_Pm, GCEP_Pw, GCEP_Pd, GCEP_P(12),
c      ! 12 = Xh (see parameters)
     & GCEP_T_y, GCEP_T_m, GCEP_T_w, GCEP_T_d, GCEP_T_h, GCEP_Rb,
     & GCEP_Re, GCEP_Time,GCEP_HP_mode,GCEP_Gload,GCEP_hourly_HP_hload, 
     & GCEP_hourly_HP_cload, GCEP_hourly_load, GCEP_HEAT_COP,
     & GCEP_COOL_COP, GCEP_OWT, GCEP_EWT, gcep_init_call

      
C---------------------------------------------------------------------------------
C General parameters for ground coupling systems - dimensions, properties and layout
C---------------------------------------------------------------------------------
C Ground properties (k=conductivity, alpha=difusivity, rho=density) 
C [W/mC]       
      REAL  GCEP_K_s 
C [m**2/day]              
      REAL   GCEP_Alpha_s
C [Kg/m‘]          
      REAL  GCEP_Rho_s            

C Grout properties (k=conductivity)
C [W/mC]
      REAL  GCEP_K_g              

C Borehole dimensions (D=diameter, Deapth=length)
C [m]
      REAL  GCEP_D_bore
C [m]           
      REAL  GCEP_Deapth_bore     

C Borehole layout in field (N_x= number in x direction, L_x=distance between boreholes in x direction, N_y=..., L_y=...)
C [boreholes]
      INTEGER  GCEP_N_bor_x 
C [boreholes]    
      INTEGER  GCEP_N_bor_y
C [m]      
      REAL     GCEP_L_bor_x 
C [m]     
      REAL     GCEP_L_bor_y      

C Tubes layout in borehole (choice)
C     1=2 tubes (1U) Tubes in contact at the center of the borehole
C     2=2 tubes (1U) Tubes midway between borehole center and wall
C     3=2 tubes (1U) Tubes in contact with borehole wall (max distance)
C     4=4 tubes (2U) Tubes in contact at the center of the borehole
C     5=4 tubes (2U) Tubes midway between borehole center and wall
C     6=4 tubes (2U) Tubes in contact with borehole wall (max distance)

C [1..6]
      INTEGER  GCEP_Tube_Case      

C Tubes dimensions (choice)  OD(cm)  ID(cm)
C     1 ¾"      SDR-11       2.67    2.18
C     2 1"      SDR-11       3.34    2.73
C     3 1¡"     SDR-11       4.22    3.45
C     4 25 mm   SDR-11       2.50    2.04
C     5 32 mm   SDR-11       3.20    2.60
C     6 40 mm   SDR-11       4.00    3.26
C     7 ¾"      SDR-135      2.67    2.27
C     8 1"      SDR-135      3.34    2.85
C     9 1¡"     SDR-135      4.22    3.59
C     10 25 mm   SDR-135     2.50    2.04
C     11 32 mm   SDR-135     3.20    2.62
C     12 40 mm   SDR-135     4.00    3.40

C [1..12]
      INTEGER  GCEP_TypeTubes     

C Tube properties (k=conductivity)
C [W/mC]
      REAL  GCEP_K_t               

C Tube length
C [m]
      REAL  GCEP_pipe_length             

C Fluid property (k=conductivity, rho=density, Cp=specific heat, Mu=dynamic viscosity)
C  [W/mC]
      REAL  GCEP_K_f
C [Kg/m**3]             
      REAL  GCEP_Rho_f  
C [J/KgC]         
      REAL  GCEP_Cp_f 
C [N/m]            
      REAL  GCEP_Mu_f             

C Undisturbed ground temperature
C  [C]
      REAL  GCEP_Tg_ND            

C Initial ground temperature
C [C]
      REAL  GCEP_Tg_init         
C Flow of fluid to the ground
c should at one point be coming from a heat pump model
c ground model already configured for possibility of 
c iterative process between heat pumps and ground loop including varrying flow
c  [l/s]
      REAL  GCEP_Flow        

C---------------------------------------------------------------------------------
C Time parameters
C---------------------------------------------------------------------------------
     
C Number of hours since last calculations of ground temperatures
C [hours]
      REAL  GCEP_Time_Tg                                   

C---------------------------------------------------------------------------------
C     General Parameters calculated in GCEPInit module
C---------------------------------------------------------------------------------
C     Tubes inside diameter
C [m]
      REAL    GCEP_Di_tubes         
C     Tubes outside diameter
C [m]
      REAL     GCEP_Do_tubes        
C     Total number of elements for actual field (X direction)
C [elements]
      INTEGER  GCEP_Ndx              
C     Total number of elements for actual field (Y direction)
C [elements]
      INTEGER  GCEP_Ndy             
C     Total number of elements for a field containing one borehole (X direction)
C [elements]
      INTEGER  GCEP_Ndx_1b           
C     Total number of elements for a field containing one borehole (Y direction)
C [elements]
      INTEGER  GCEP_Ndy_1b           


C---------------------------------------------------------------------------------
C General Variables
C---------------------------------------------------------------------------------


C History of ground loads
C [W]
      REAL  GCEP_LoadHis

C     Variable calculated by GCEPtground
C Average temperature of the ground in the borefield region
C [C]
      REAL  GCEP_Tg              
C Average temperature at the walls of the boreholes
C [C]
      REAL  GCEP_Tw                
C Average temperature at the walls of the boreholes if ther were only one borehole
C [C]
      REAL  GCEP_Tw_1b            
C Temperature penalty for boreholes thermal interference (Tw-Tw_1b)
C [C]
      REAL  GCEP_Tpen                

C     Variable used by GCEP_MLAinit and GCEP_MLAiterate
C Number of hours in the rest (y is for yearly)
C [hours]
      INTEGER   GCEP_Xy           
C     Number of hours in the term i (0 or Xi)
C rest (yearly)
C  [hours]
      INTEGER  GCEP_Nyear         
C month
C  [hours]
      INTEGER  GCEP_Nmonth          
C week
C  [hours]
      INTEGER  GCEP_Nweek          
C day
C  [hours]
      INTEGER  GCEP_Nday          
C hours
C [hours]
      INTEGER  GCEP_Nhour          
C     Average ground load on each period
C rest (yearly)
C [W]
      REAL  GCEP_Py             
C month
C [W]
      REAL  GCEP_Pm             
C week
C [W]
      REAL  GCEP_Pw             
C day
C [W]
      REAL  GCEP_Pd             
C Ground loads for the last Xh hours
C [W]
      REAL  GCEP_P
C     MLA (Multple load agregation) scheme terms
C rest (yearly)
C [W/C]
      REAL  GCEP_T_y              
C month
C [W/C]
      REAL  GCEP_T_m             
C week
C [W/C]
      REAL  GCEP_T_w             
C day
C [W/C]
      REAL  GCEP_T_d            
C last Xh hours
C [W/C]
      REAL  GCEP_T_h            
C thermal resistance of the boreholes
C [mC/W]
      REAL  GCEP_Rb              
C Reynold number in the tubes
      REAL  GCEP_Re             

C Hours passed since the begining of the simulation
C incremented every time a call ro GCEP_MLAinit is made
C [hour]
      REAL  GCEP_Time           

C Mode of operation of the GCEP
C GCEP_HP_mode= 1 for heating and -1 for cooling
      INTEGER  GCEP_HP_mode

C     Iteration variables for GCEP_MLAiterate
C     INPUTS
C Ground load
C [W]
      REAL  GCEP_Gload                 
C Variables used for conversion of time step loads to hourly loads
C Load at the ground = min(Building load, HP capacity)*(1+-1/COP)
C [Wh]
      REAL  GCEP_hourly_HP_hload 
C [Wh]   
      REAL  GCEP_hourly_HP_cload   
C Sum of the heating and cooling loads at the ground
C [Wh]
      REAL  GCEP_hourly_load        
C Heat Pumps COP (calculated in ashp_heating and ashp_cooling)
C     Heating
      REAL  GCEP_HEAT_COP
C     Cooling
      REAL  GCEP_COOL_COP

C     OUTPUTS
C Fluid temperature at the exit of the heat pumps (entrance of the borefield)
C  [C]
      REAL  GCEP_OWT            
C Fluid temperature at the entrance ofthe heat pumps (exit of the borefield)
C  [C]
      REAL  GCEP_EWT            

c The following is a dummy variable used to determine if the GCEP_init subroutine 
c has been called. If it is TRUE then the subroutine has not been called, 
c if it is FALSE, then the subroutine has been called and will not be called again.
      LOGICAL  gcep_init_call   
