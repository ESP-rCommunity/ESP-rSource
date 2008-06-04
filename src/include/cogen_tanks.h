C ******************************************************************************** 
C This file contains common blocks, declarations, and assignments related to 
C the water storage tank component models that were designed for coupling to 
C cogeneration devices. These were previously coded in F90 modules, but 
C recast into the current format to make this code compatible with the g77   
C compiler.
C ********************************************************************************  

C ******************************************************************************** 
C Declare variables and initializes data for the hot-water storage 
C tanks that can be connected to the fuel cell component. 
C (Formerly MODULE FC_tanks).  
C ******************************************************************************** 
      COMMON/FC_tanks/ 
     &     HHV_fuel_tank, LHV_fuel_tank, tank_fuel_molefrac_N2,
     &     tank_fuel_molefrac_CO2,tank_fuel_molefrac_CH4,
     &     tank_fuel_molefrac_C2H6,tank_fuel_molefrac_C3H8, 
     &     tank_fuel_molefrac_total,molW_fuel_tank,tank_mass,
     &     tank_Cp,tank_UA,tank_burner_ON,tank_burner_OFF,
     &     element_capacity_ON,element_capacity_OFF,
     &     tank_efficiency,tank_excess_air,tank_DHW_draw,
     &     tank_space_heat,FC_DHW_CSA_Lperhour,
     &     FC_DHW_CSA_Lperday,FC_DHW_CSA_fraction,
     &     FC_DHW_nominal_Lperday,Href_tank,  
     &     node1,node2,node3,con1a,con1b,
     &     con1c,              !  Node & Connection indexes
     &     m_dot_fuel_kmol_t,m_dot_fuel_kg_t,          ! <--
     &     Vdot_fuel_STP_t,q_burner, q_capture_future, ! <--
     &     m_dot_air_kg_t,m_dot_exh_kg_t,              ! Calculated data for post-processor
     &     Troom_future,        ! <----- Containment temperature           
     &     q_heatdump_present,  ! <----- Heat rejection from tank (present time row)
     &     q_heatdump_future,   ! <----- Heat rejection from tank (future time row)
     &     mdot_conn1a_present,mdot_conn1a_future,
     &     mdot_conn1b_present,mdot_conn1b_future,  ! <-- connection mass flow rates
     &     mdot_conn1c_present,mdot_conn1c_future

      common/FC_tanks_DHWdraw/DHW_draw_present,
     &  DHW_draw_future,T_makeup_present,T_makeup_future,
     &  mdotCp_DHW_present,mdotCp_DHW_future

C--------------------------------------------------------------------------------- 
C Model parameters 
C--------------------------------------------------------------------------------- 
      integer node1,node2, node3 ! node index
      integer con1a, con1b, con1c      ! connection index


C--------------------------------------------------------------------------------- 
C Named constants for the higher heating values (HHV) of the fuel and 
C its constituents. 
C Data from ASHRAE Fundamentals 2001, Table 3, page 18.3. Units of constituent
C gas HHVs are MJ/kg; units of `HHV_fuel_tank' are MJ/kmol. 
C--------------------------------------------------------------------------------- 
      REAL HHV_fuel_tank, HHV_CH4, HHV_C2H6, HHV_C3H8, HHV_C4H10 
      PARAMETER ( HHV_CH4   = 55.533 ) 
      PARAMETER ( HHV_C2H6  = 51.923 ) 
      PARAMETER ( HHV_C3H8  = 50.402 ) 
      PARAMETER ( HHV_C4H10 = 49.593 ) ! n-butane not currently used

C---------------------------------------------------------------------------------
C     Fuel mixture lower heating value (MJ/kmol)
C---------------------------------------------------------------------------------
      real LHV_fuel_tank
      
C---------------------------------------------------------------------------------
C Variables to describe the fuel's composition in mole fractions. This is the
C fuel combusted in the hot-water storage tank.
C---------------------------------------------------------------------------------
      REAL tank_fuel_molefrac_N2
      REAL tank_fuel_molefrac_CO2
      REAL tank_fuel_molefrac_CH4    ! methane
      REAL tank_fuel_molefrac_C2H6   ! ethane
      REAL tank_fuel_molefrac_C3H8   ! propane: includes contr of higher hydrocarbons

C---------------------------------------------------------------------------------
C Sum of molar fractions of fuel constituents (should equal 1).
C---------------------------------------------------------------------------------
      REAL tank_fuel_molefrac_total

C---------------------------------------------------------------------------------
C Molecular weight of the fuel (kg/kmol).
C---------------------------------------------------------------------------------
      REAL molW_fuel_tank

C---------------------------------------------------------------------------------
C Mass (kg) and specific heat (J/kgK) of node 1 (water in tank + casing).
C---------------------------------------------------------------------------------
      REAL tank_mass
      REAL tank_Cp

C---------------------------------------------------------------------------------
C Heat loss coefficient between node 1 and surroundings (UA-value) (W/K).
C---------------------------------------------------------------------------------
      REAL tank_UA

C---------------------------------------------------------------------------------
C Burner capacity (W) for on and off (i.e. pilot) states.
C---------------------------------------------------------------------------------
      REAL tank_burner_ON
      REAL tank_burner_OFF

C---------------------------------------------------------------------------------
C Connection mass flow storage rates
C---------------------------------------------------------------------------------
C-----1. present time-step storage variables
      real mdot_conn1a_present, mdot_conn1b_present ! (kg/s)
      real mdot_conn1c_present
C-----2. future time-step storage variables
      real mdot_conn1a_future, mdot_conn1b_future ! (kg/s)
      real mdot_conn1c_future

C---------------------------------------------------------------------------------
C Electric resistance heater element heat output (W) for on and off states.
C---------------------------------------------------------------------------------
      REAL element_capacity_ON
      REAL element_capacity_OFF

C---------------------------------------------------------------------------------
C Combustion + flue efficiency based on HHV (fraction).
C---------------------------------------------------------------------------------
      REAL tank_efficiency

C---------------------------------------------------------------------------------
C Excess air ratio (fraction).
C---------------------------------------------------------------------------------
      REAL tank_excess_air

C---------------------------------------------------------------------------------
C DHW draw profile. Two options exist for FCT project: no DHW or DHW draws
C following the CSA draw profile.  Other draw profiles can be added in the
C future.
C---------------------------------------------------------------------------------
      INTEGER tank_DHW_draw, tank_no_DHW, tank_CSA_DHW
      PARAMETER ( tank_no_DHW = 0  )
      PARAMETER ( tank_CSA_DHW = 1 )

C---------------------------------------------------------------------------------
C Flag indicating whether the tank serves a fan-coil for space heating.
C---------------------------------------------------------------------------------
      INTEGER tank_space_heat, tank_no_heat, tank_yes_heat
      PARAMETER ( tank_no_heat  = 0 )
      PARAMETER ( tank_yes_heat = 1 )

C---------------------------------------------------------------------------------
C Number of occupants for purposes of determining nominal DHW water draw.
C This is hard-wired for the FCT project, but could be made a variable in
C the future.
C---------------------------------------------------------------------------------
      INTEGER FC_NumOfOccupants
      PARAMETER ( FC_NumOfOccupants = 4 )

C---------------------------------------------------------------------------------
C DHW nominal water draws.
C---------------------------------------------------------------------------------
      REAL FC_DHW_CSA_Lperhour(24)     ! CSA hourly draw profile (L/hour)
      REAL FC_DHW_CSA_Lperday          ! Daily total of CSA profile (L/day) 
      REAL FC_DHW_CSA_fraction(24)     ! CSA hourly/daily total (-)
      REAL FC_DHW_nominal_Lperday      ! Nominal L/day draw based on occupancy
      REAL FC_DHW_nominal_tank_T       ! Nominal tank temp used as basis for draws
      PARAMETER ( FC_DHW_nominal_tank_T = 60. )

C---------------------------------------------------------------------------------
C DHW calculated data for reporting
C---------------------------------------------------------------------------------
      REAL DHW_draw_present     ! present domestic hot water draw (l/s)
      REAL DHW_draw_future      ! future domestic hot water draw (l/s)
      REAL T_makeup_present     ! Present make-up temperatrue (oC)
      REAL T_makeup_future      ! future make-up temperature (oC)
      REAL mdotCp_DHW_present   ! present DHW load heat capacity (W/s)
      REAL mdotCp_DHW_future    ! future DHW load heat capacity (W/s)


C---------------------------------------------------------------------------------
C Tank fuel flow data
C---------------------------------------------------------------------------------
      REAL m_dot_fuel_kmol_t    ! Fuel molar flow (kmol/s)
      REAL m_dot_fuel_kg_t      ! Fuel mass flow  (kg/s)
      REAL Vdot_fuel_STP_t      ! Fuel volumetric flow (m3/ s @ stp)

C---------------------------------------------------------------------------------
C Burner heat production/recovery
C---------------------------------------------------------------------------------
      REAL q_burner             ! heat produced by gas buener (W)
      REAL q_capture_present,q_capture_future ! heat recovered on most-recent timestep (W)

C---------------------------------------------------------------------------------
C Air/Exhaust flow
C---------------------------------------------------------------------------------
      REAL m_dot_exh_kg_t       ! Exhaust mass flow (kg/s)
      REAL m_dot_air_kg_t       ! Exhaust mass flow (kg/s)

C---------------------------------------------------------------------------------
C Containing Room temperature
C---------------------------------------------------------------------------------
      REAL Troom_present, Troom_future    

C---------------------------------------------------------------------------------
C Heat dump 
C---------------------------------------------------------------------------------
      REAL q_heatdump_present, q_heatdump_future 

C---------------------------------------------------------------------------------
C Reference temperature and enthalpy at that temperature. Used for calculating
C specific heat of combustion and exhaust gases. Based upon sensitivity testing
C `T_refCp' set to -100oC.
C---------------------------------------------------------------------------------
      REAL T_refCp_tank 
      PARAMETER ( T_refCp_tank = -100. )
      REAL Href_tank

C---------------------------------------------------------------------------------
C Fictitious "heat dump" which extracts energy from the tank to ensure that water
C in the system does not boil.  This feature was added because it was found that
C the FC often adds more energy to the tank that is required for space heating
C and DHW purposes.  As such, the HVAC system may require some kind of heat dump.
C This fictitious heat dump will allow manufacturers to analyze this situation
C without first having to explicitly design a heat dump into the system.
C The set-points that control the fictitious heat dump are hard-wired for now,
C but these can be added as user inputs in the .pln file in the future.
C 
C Note: In order to model a TAC system charged by the thermal output of a cogen
C device, set these parameters to high values (e.g. 95oC) because the TAC system
C is used to reject excess heat from the hot water tank.  This will ensure that
C the hot water tank model will not go into heat dump mode.
C---------------------------------------------------------------------------------
      REAL FC_heatdump_min, FC_heatdump_max
      PARAMETER ( FC_heatdump_min = 75. )
      PARAMETER ( FC_heatdump_max = 80. )
C-------FCT software v 1.2.1 and Beausoleil-Morrison et al eSim 2002 paper
C-------used FC_heatdump_max = 65. and FC_heatdump_min = 60.

C-------Ferguson BS2005 paper (Modeling of Building-integrated Stirling CHP systems)
C-------used FC_heatdump_max = 100. and FC_heatdump_min = 0.

C-------Mottillo (2006) Thesis: Investigation of an Adsorption System for the
C-------Seasonal Storage of Heat Applied to Residential Buildings
C-------used FC_heatdump_max = 90. and FC_heatdump_min = 85.
