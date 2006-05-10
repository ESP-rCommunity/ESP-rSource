C This file contains the following modules related to Advanced Integrated
C Mechanical Systems (AIMS):

C   AIMS_tank1


C ********************************************************************************
C ********************************* AIMS_tank1 ***********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: April 30, 2002
C Copyright CETC 2002

C This module declares variables and initializes data for an AIMS fuel-fired
C water storage tank. Previously known as AIMS_tank1 module. 
C---------------------------------------------------------------------------------

      COMMON/AIMS_tank1/
     &     AIMS_tank1_HHV, AIMS_tank1_excess_air, 
     &     AIMS_tank1_fuel_molefrac_N2, AIMS_tank1_fuel_molefrac_CO2,
     &     AIMS_tank1_fuel_molefrac_CH4, AIMS_tank1_fuel_molefrac_C2H6, 
     &     AIMS_tank1_fuel_molefrac_C3H8,AIMS_tank1_fuel_molefrac_total, 
     &     AIMS_tank1_molW_fuel, AIMS_tank1_mass, AIMS_tank1_Cp,
     &     AIMS_tank1_UA, AIMS_tank1_burner_ON, AIMS_tank1_burner_OFF,  
     &     AIMS_tank1_efficiency, AIMS_tank1_DHW_draw,
     &     AIMS_tank1_space_heat,  AIMS_tank1_DHW_CSA_Lperday, 
     &     AIMS_tank1_DHW_CSA_fraction, AIMS_tank1_DHW_nominal_Lperday, 
     &     AIMS_tank1_Href, AIMS_tank1_MOLW_AIR,
     &     AIMS_tank1_NumOfOccupants

C---------------------------------------------------------------------------------
C Heating value (HHV) of the fuel mixture supplied to the burner (MJ/kmol).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_HHV

C---------------------------------------------------------------------------------
C Molecular weight of air (kg/kmol).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_excess_air

C---------------------------------------------------------------------------------
C Variables to describe the fuel's composition in mole fractions.
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_fuel_molefrac_N2
        REAL  AIMS_tank1_fuel_molefrac_CO2
        REAL  AIMS_tank1_fuel_molefrac_CH4    ! methane
        REAL  AIMS_tank1_fuel_molefrac_C2H6   ! ethane
        REAL  AIMS_tank1_fuel_molefrac_C3H8   ! propane: includes contr of higher hydrocarbons

C---------------------------------------------------------------------------------
C Sum of molar fractions of fuel constituents (should equal 1).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_fuel_molefrac_total

C---------------------------------------------------------------------------------
C Molecular weight of the fuel (kg/kmol).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_molW_fuel

C---------------------------------------------------------------------------------
C Mass (kg) and specific heat (J/kgK) of node 1 (water in tank + casing).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_mass
        REAL  AIMS_tank1_Cp

C---------------------------------------------------------------------------------
C Heat loss coefficient between node 1 and surroundings (UA-value) (W/K).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_UA

C---------------------------------------------------------------------------------
C Burner capacity (W) for on and off (i.e. pilot) states.
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_burner_ON
        REAL  AIMS_tank1_burner_OFF

C---------------------------------------------------------------------------------
C Combustion + flue efficiency (fraction).
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_efficiency

C---------------------------------------------------------------------------------
C Excess air ratio (fraction).
C---------------------------------------------------------------------------------
c        REAL  AIMS_tank1_excess_air

C---------------------------------------------------------------------------------
C DHW draw profile. Two options currently exist: no DHW or DHW draws
C following the CSA draw profile.  Other draw profiles will be added in the
C future.
C---------------------------------------------------------------------------------

        INTEGER AIMS_tank1_no_DHW 
        parameter ( AIMS_tank1_no_DHW = 0 )
        INTEGER AIMS_tank1_CSA_DHW  
        parameter ( AIMS_tank1_CSA_DHW = 1)

        INTEGER  AIMS_tank1_DHW_draw

C---------------------------------------------------------------------------------
C Flag indicating whether the tank serves a fan-coil for space heating.
C---------------------------------------------------------------------------------

        INTEGER AIMS_tank1_no_heat
        parameter ( AIMS_tank1_no_heat  = 0 )
        INTEGER AIMS_tank1_yes_heat
        parameter ( AIMS_tank1_yes_heat = 1 )
        INTEGER  AIMS_tank1_space_heat

C---------------------------------------------------------------------------------
C Number of occupants for purposes of determining nominal DHW water draw.
C This is hard-wired for nowt, but will be made a variable in
C the future.
C---------------------------------------------------------------------------------
C        INTEGER, parameter  AIMS_tank1_NumOfOccupants = 4

         INTEGER AIMS_tank1_NumOfOccupants
C---------------------------------------------------------------------------------
C DHW nominal water draws.
C---------------------------------------------------------------------------------
        REAL  AIMS_tank1_DHW_CSA_Lperday          ! Daily total of CSA profile (L/day) 
        REAL  AIMS_tank1_DHW_CSA_fraction(24)     ! CSA hourly/daily total (-)
        REAL  AIMS_tank1_DHW_nominal_Lperday      ! Nominal L/day draw based on occupancy

        REAL AIMS_tank1_DHW_nominal_tank_T
        parameter ( AIMS_tank1_DHW_nominal_tank_T = 60. ) 

C---------------------------------------------------------------------------------
C Reference temperature and enthalpy at that temperature. Used for calculating
C specific heat of combustion and exhaust gases. Based upon sensitivity testing
C `AIMS_tank1_T_refCp' set to -100oC.
C---------------------------------------------------------------------------------

        REAL AIMS_tank1_T_refCp
        parameter ( AIMS_tank1_T_refCp = -100. )

        REAL  AIMS_tank1_Href

        REAL AIMS_tank1_MOLW_AIR


