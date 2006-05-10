C ********************************************************************************
C This file contains common blocks, declarations, and assignments related
C to the CETC fuel cell cogeneration model (aka SOFC model). These were
C previously coded in F90 modules, but recast into the current format
C to make this code compatible with the g77 compiler.
C ********************************************************************************


C ********************************************************************************
C Data for the composition and properties of the fluids that are analyzed in the
C fuel cell component model.
C (Formerly MODULE FC_FLUIDS)
C ********************************************************************************
      COMMON/FC_FLUIDS/
     &                 hcoef,
     &                 ho_datum,
     &                 fuel_molefrac_N2,
     &                 fuel_molefrac_CO2,
     &                 fuel_molefrac_CH4,
     &                 fuel_molefrac_C2H6,
     &                 fuel_molefrac_C3H8,
     &                 fuel_molefrac_H2,
     &                 molW_air,
     &                 LHV_fuel,
     &                 exh_molefrac_N2,
     &                 exh_molefrac_Ar,
     &                 exh_molefrac_O2,
     &                 exh_molefrac_CO2,
     &                 exh_molefrac_H2O

C---------------------------------------------------------------------------------
C Named constants for the constituents of the fuel and air streams supplied to
C the fuel cell.
C---------------------------------------------------------------------------------
      INTEGER N2,O2,CO2,H2O,Ar,CH4,C2H6,C3H8,H2
      PARAMETER ( N2   = 1 )
      PARAMETER ( O2   = 2 )
      PARAMETER ( CO2  = 3 )
      PARAMETER ( H2O  = 4 )
      PARAMETER ( Ar   = 5 )
      PARAMETER ( CH4  = 6 )
      PARAMETER ( C2H6 = 7 )
      PARAMETER ( C3H8 = 8 )
      PARAMETER ( H2   = 9 )

C---------------------------------------------------------------------------------
C Correlation equation coefficients for calculating relative enthalpies of gases.
C From Cuthbert (2001) Table 2. The a1 coefficients for O2 and CO2 in Table 2 were
C incorrect so the values used here do not agree with Cuthbert (2001). Refer to
C Dave Cuthbert e-mail of 2001-08-07.
C---------------------------------------------------------------------------------
c      INTEGER i,j
      REAL hcoef(3,9) ! first dimension for coeff number; second for gas number

C---------------------------------------------------------------------------------
C Datum points for gas enthalpies. Evaluated using above correlations at 1 atm
C and 25oC. Values from Dave Cuthbert's e-mails of 2001-08-03 and 2001-08-07.
C---------------------------------------------------------------------------------
      REAL ho_datum_T
      PARAMETER ( ho_datum_T = 25. )
      REAL ho_datum(9) ! index for gas number

C---------------------------------------------------------------------------------
C Variables to describe the fuel's composition in mole fractions.
C---------------------------------------------------------------------------------
      REAL fuel_molefrac_N2
      REAL fuel_molefrac_CO2
      REAL fuel_molefrac_CH4    ! methane
      REAL fuel_molefrac_C2H6   ! ethane
      REAL fuel_molefrac_C3H8   ! propane: includes contr of higher hydrocarbons.
      REAL fuel_molefrac_H2

C---------------------------------------------------------------------------------
C Named constants to describe the composition of the air stream supplied to the
C fuel cell.  These are fixed here rather than treated as a user input since
C variations from these values are minor.  At some point in the future this could
C be altered to get the containing room's humidity level to establish the mole
C fraction of H2O in the air stream, although this will have a negligible affect
C on results.
C---------------------------------------------------------------------------------
      REAL air_molefrac_N2,
     &     air_molefrac_O2,
     &     air_molefrac_CO2,
     &     air_molefrac_H2O,
     &     air_molefrac_Ar
      PARAMETER ( air_molefrac_N2  = 0.7728 )
      PARAMETER ( air_molefrac_O2  = 0.2073 )
      PARAMETER ( air_molefrac_CO2 = 0.0003 )  ! typo in orig Cuthbert report.
      PARAMETER ( air_molefrac_H2O = 0.0104 )
      PARAMETER ( air_molefrac_Ar  = 0.0092 )

C---------------------------------------------------------------------------------
C Molecular weight of air (kg/kmol). Placed here rather than MODULE FC_calculated
C so that it is accessible by gas-fired tank component as well.
C---------------------------------------------------------------------------------
        REAL molW_air   ! air

C---------------------------------------------------------------------------------
C Named constants for the lower heating values (LHV) of the fuel and
C its constituents.
C From Cuthbert (2001) Table 3 (derived from ASHRAE Fundamentals 2001,
C Table 3, page 18.3). Units are kJ/kmol.
C---------------------------------------------------------------------------------
      REAL LHV_fuel,
     &     LHV_H2,
     &     LHV_CH4,
     &     LHV_C2H6,
     &     LHV_C3H8,
     &     LHV_C4H10
      PARAMETER ( LHV_H2    =  242507. )
      PARAMETER ( LHV_CH4   =  802088. )
      PARAMETER ( LHV_C2H6  = 1428056. )
      PARAMETER ( LHV_C3H8  = 2044879. )
      PARAMETER ( LHV_C4H10 = 2660328. )  ! n-butane not currently used

C---------------------------------------------------------------------------------
C Atomic weights. Source : periodic table at
C http://chemlab.pc.maricopa.edu/periodic/periodic.html.
C---------------------------------------------------------------------------------
      REAL atomic_O,
     &     atomic_H,
     &     atomic_C,
     &     atomic_N,
     &     atomic_Ar
      PARAMETER ( atomic_O  = 15.9994 )
      PARAMETER ( atomic_H  = 1.0079  )
      PARAMETER ( atomic_C  = 12.001  )
      PARAMETER ( atomic_N  = 14.0067 )
      PARAMETER ( atomic_Ar = 39.948  )

C---------------------------------------------------------------------------------
c Molecular weights of fuel and air constituents.
C---------------------------------------------------------------------------------
      REAL molW_N2,
     &     molW_O2,
     &     molW_CO2,
     &     molW_H2O,
     &     molW_Ar,
     &     molW_H2,
     &     molW_CH4,
     &     molW_C2H6,
     &     molW_C3H8,
     &     molW_C4H10
      PARAMETER ( molW_N2    = 2.*atomic_N                )
      PARAMETER ( molW_O2    = 2.*atomic_O                )
      PARAMETER ( molW_CO2   = atomic_C + 2.*atomic_O     )
      PARAMETER ( molW_H2O   = 2.*atomic_H + atomic_O     )
      PARAMETER ( molW_Ar    = atomic_Ar                  )
      PARAMETER ( molW_H2    = 2.*atomic_H                )
      PARAMETER ( molW_CH4   = atomic_C + 4.*atomic_H     )
      PARAMETER ( molW_C2H6  = 2.*atomic_C + 6.*atomic_H  )
      PARAMETER ( molW_C3H8  = 3.*atomic_C + 8.*atomic_H  )
      PARAMETER ( molW_C4H10 = 4.*atomic_C + 10.*atomic_H )

C---------------------------------------------------------------------------------
C Exhaust gas constituents molar fractions.
C---------------------------------------------------------------------------------
      REAL exh_molefrac_N2
      REAL exh_molefrac_Ar
      REAL exh_molefrac_O2
      REAL exh_molefrac_CO2
      REAL exh_molefrac_H2O


C ********************************************************************************
C User-defined performance characteristic data that are employed in the
C fuel cell component model.
C (Formerly MODULE FC_CHARACTERISTICS)
C ********************************************************************************
      COMMON/FC_CHARACTERISTICS/eta_0,eta_1,eta_2,
     &                          q_heatleak,
     &                          par_0,par_1,par_2,par_3,
     &                          wa_0,wa_1,wa_2,
     &                          qhr_0,qhr_1,qhr_2,
     &                          liq_0,liq_1,liq_2,
     &		                        FC_control,
     &                          FC_elec_min_output,
     &                          FC_elec_max_output,
     &                          FC_timestep_output

C---------------------------------------------------------------------------------
C Variables for the polynomial correlation that describes the fuel cell's
C electrical efficiency:
C    eta_fc = eta_0 + eta_1*kWe + eta_2*kWe**2.
C---------------------------------------------------------------------------------
      REAL eta_0
      REAL eta_1
      REAL eta_2

C---------------------------------------------------------------------------------
C Variable to describe the heat leakage from the fuel cell (i.e. thermal losses
C that are not captured in the exhaust stream that is delivered to node 2).
C Units of Watts.
C---------------------------------------------------------------------------------
      REAL q_heatleak

C---------------------------------------------------------------------------------
C Variables for the polynomial correlation that describes the fuel cell's
C parasitic electrical losses:
C    q_par_elec = [par_0 + par_1*kWe + par_2*kWe**2. + par_3*kWe**3.]*1000.
C---------------------------------------------------------------------------------
      REAL par_0
      REAL par_1
      REAL par_2
      REAL par_3

C---------------------------------------------------------------------------------
C Variables for the polynomial correlation that describes the air supply
C to the fuel cell:
C    Vdot_air_STP = wa_0 + wa_1*kWe + wa_2*kWe**2.
C---------------------------------------------------------------------------------
      REAL wa_0
      REAL wa_1
      REAL wa_2

C---------------------------------------------------------------------------------
C Variables for the polynomial correlation that describes the UA-value
C of the exhaust-to-water heat exchanger:
C    q_heatrecovery / deltaT = qhr_0 + qhr_1*Vdot_ex_STP + qhr_2*Vdot_ex_STP**2.
C---------------------------------------------------------------------------------
      REAL qhr_0
      REAL qhr_1
      REAL qhr_2

C---------------------------------------------------------------------------------
C Variables for the polynomial correlation that describes the liquid water
C supply:
C    Vwater = liq_0 + liq_1*mfuel + liq_2*mfuel**2
C---------------------------------------------------------------------------------
      REAL liq_0
      REAL liq_1
      REAL liq_2

C---------------------------------------------------------------------------------
C Variables and constants for the method used to control the fuel cell.
C    FC_control               variable indicating control method set by user
C    FC_ctl_elec_priority     named constant indicating the fuel cell is controlled
C                             to give priority to the house's electrical demands
C    FC_ctl_thermal_priority  named constant indicating fuel cell is controlled
C                             to give priority to space heating and DHW demands
C    _ctl_const_output      named constant indicating fuel cell runs at constant
C                             electrical output
C    FC_ctl_ext               Named constant indicating fuel cell is controlled 
C                             using external controller model
C---------------------------------------------------------------------------------
	INTEGER FC_control,
     &        FC_ctl_elec_priority,
     &        FC_ctl_thermal_priority,
     &        FC_ctl_const_output,
     &        FC_ctl_ext
    	PARAMETER ( FC_ctl_elec_priority    = 1 )
 	PARAMETER ( FC_ctl_thermal_priority = 2 )
	PARAMETER ( FC_ctl_const_output     = 3 )
 	PARAMETER ( FC_ctl_ext              = 4 )

C---------------------------------------------------------------------------------
C Variables for the fuel cell's minimum and maximum net electrical output (W)
C---------------------------------------------------------------------------------
      REAL FC_elec_min_output
      REAL FC_elec_max_output

C---------------------------------------------------------------------------------
C Variable and named constants indicating whether user has requested (through .pln)
C that an ASCII file with time-step results be produced.
C---------------------------------------------------------------------------------
      INTEGER FC_timestep_output,
     &        FC_timestep_output_no,
     &        FC_timestep_output_yes
      PARAMETER ( FC_timestep_output_no  = 1 )
      PARAMETER ( FC_timestep_output_yes = 1 )


C ************************************************************************************
C Calculated variables for the fuel cell component model.
C (Formerly MODULE FC_calculated)
C (JWH continuation lines removed based on syntax warning)
C ************************************************************************************
      COMMON/FC_calculated/INOD1,INOD2,INOD3,ICON3,
     &  molW_fuel,molW_exh,m_dot_fuel_kmol,m_dot_fuel_kg,Vdot_fuel_STP,
     &  m_dot_H2_kmol,m_dot_CH4_kmol,m_dot_C2H6_kmol,m_dot_C3H8_kmol,
     &  fuel_molefrac_total,T_fuel,phi_source,
     &  m_dot_air_kmol,m_dot_air_kg,Vdot_air_STP,T_air,
     &  m_dot_O2_for_H2_kmol,m_dot_O2_for_CH4_kmol,
     &  m_dot_O2_for_C2H6_kmol,m_dot_O2_for_C3H8_kmol,
     &  m_dot_O2_stoich,stoichs,Vdot_liqwater,
     &  m_dot_liqwater_kg,m_dot_liqwater_kmol,T_water,
     &  DEN_liqwater,H_water,
     &  m_dot_N2_exh_kmol,m_dot_Ar_exh_kmol,m_dot_O2_exh_kmol,
     &  m_dot_CO2_exh_kmol,m_dot_H2O_exh_kmol,m_dot_exh_kmol,
     &  m_dot_exh_kg,Vdot_exh_STP,T_exh,H_exh,Cp_exh,
     &  T_exit,H_exit,Href,
     &  eta_fc,eta_fc_elec_bop,eta_fc_cogen,
     &  q_par_elec,q_net_elec,q_net_elec_kW,q_total_elec,
     &  q_heatrecovery,UA_exh,mdotCp_water,
     &  Twater_enter,T_room,H_gas_at_Tm,UA_exh_max,FC_onoff_previous

C---------------------------------------------------------------------------------
C Node and connection indices.
C---------------------------------------------------------------------------------
      INTEGER INOD1  ! node 1
      INTEGER INOD2  ! node 2
      INTEGER INOD3  ! node 3
      INTEGER ICON3  ! connection to node 3

C---------------------------------------------------------------------------------
C Molecular weights.
C---------------------------------------------------------------------------------
      REAL molW_fuel  ! fuel
      REAL molW_exh   ! exhaust from node 1

C---------------------------------------------------------------------------------
C Fuel flow rates.
C---------------------------------------------------------------------------------
      REAL m_dot_fuel_kmol  ! (kmol/s)
      REAL m_dot_fuel_kg    ! (kg/s)
      REAL Vdot_fuel_STP    ! (standard L/min)

C---------------------------------------------------------------------------------
C Fuel constituent flow rates.
C---------------------------------------------------------------------------------
      REAL m_dot_H2_kmol
      REAL m_dot_CH4_kmol
      REAL m_dot_C2H6_kmol
      REAL m_dot_C3H8_kmol

C---------------------------------------------------------------------------------
C Sum of molar fractions of fuel constituents (should equal 1).
C---------------------------------------------------------------------------------
      REAL fuel_molefrac_total

C---------------------------------------------------------------------------------
C Properties of fuel supplied to fuel cell.
C---------------------------------------------------------------------------------
      REAL T_fuel      ! temperature (oC)
      REAL phi_source  ! rate of chemical energy content converted (W)

C---------------------------------------------------------------------------------
C Air supply to FC flow rates and temperature.
C---------------------------------------------------------------------------------
      REAL m_dot_air_kmol
      REAL m_dot_air_kg
      REAL Vdot_air_STP
      REAL T_air

C---------------------------------------------------------------------------------
C Stoichiometric O2 flow rates.
C---------------------------------------------------------------------------------
      REAL m_dot_O2_for_H2_kmol
      REAL m_dot_O2_for_CH4_kmol
      REAL m_dot_O2_for_C2H6_kmol
      REAL m_dot_O2_for_C3H8_kmol
      REAL m_dot_O2_stoich
      REAL stoichs

C---------------------------------------------------------------------------------
C Liquid water supply to FC.
C---------------------------------------------------------------------------------
      REAL Vdot_liqwater        ! volumetric flow rate (L/min)
      REAL m_dot_liqwater_kg    ! mass flow rate (kg/s)
      REAL m_dot_liqwater_kmol  ! molar flow rate (kmol/s)
      REAL T_water              ! temperature (oC)
      REAL DEN_liqwater         ! density (kg/m3)
      REAL H_water              ! relative enthalpy (kJ/kg)

C---------------------------------------------------------------------------------
C Flow rate of exhaust gases from node 1.
C---------------------------------------------------------------------------------
      REAL m_dot_N2_exh_kmol
      REAL m_dot_Ar_exh_kmol
      REAL m_dot_O2_exh_kmol
      REAL m_dot_CO2_exh_kmol
      REAL m_dot_H2O_exh_kmol
      REAL m_dot_exh_kmol
      REAL m_dot_exh_kg
      REAL Vdot_exh_STP

C---------------------------------------------------------------------------------
C Temperature and enthalpy of gases exhausted from node 1.
C---------------------------------------------------------------------------------
      REAL T_exh  ! temperature (oC)
      REAL H_exh  ! enthalpy (kJ/kmol)
      REAL Cp_exh ! pseudo specific heat (J/(kgmol*oC).

C---------------------------------------------------------------------------------
C Temperature and enthalpy of gases exiting fuel cell.
C---------------------------------------------------------------------------------
      REAL T_exit  ! temperature (oC)
      REAL H_exit  ! enthalpy (kJ/kmol)

C---------------------------------------------------------------------------------
C Reference temperature and enthalpy at that temperature. Used for calculating
C specific heat of exhaust and exit gases. Based upon sensitivity testing
C `T_refCp' set to -100oC.
C---------------------------------------------------------------------------------
      REAL T_refCp
      PARAMETER ( T_refCp = -100. )
      REAL Href

C---------------------------------------------------------------------------------
C Fuel cell electrical performance characteristics.
C---------------------------------------------------------------------------------
      REAL eta_fc           ! electrical efficiency (-)
      REAL eta_fc_elec_bop  ! Balance of plant electirical efficiency (-)
      REAL eta_fc_cogen     ! fuel cell cogeneration efficiency (-)
      REAL q_par_elec       ! parasitic electrical losses (W)
      REAL q_net_elec       ! net electrical output (W)
      REAL q_net_elec_kW    ! net electrical output (kW)
      REAL q_total_elec     ! total elec load placed on FC controller (W)

C---------------------------------------------------------------------------------
C Fuel cell thermal performance characteristics.
C---------------------------------------------------------------------------------
      REAL q_heatrecovery  ! heat recovered from exhaust (W)
      REAL UA_exh          ! U-value of exhaust-to-water HE (W/K)
  
C---------------------------------------------------------------------------------
C Water side of exhaust-to-water heat exchanger.
C---------------------------------------------------------------------------------
      REAL mdotCp_water ! product of mass flow and Cp of water in HE (W/K)
      REAL Twater_enter ! temp of water entering HE (oC)

C---------------------------------------------------------------------------------
C Temperature of room containing fuel cell.
C---------------------------------------------------------------------------------
      REAL T_room

C---------------------------------------------------------------------------------
C Variables used in calculating the theoretical maximum heat transfer within
C the exhaust-to-water heat exchanger.
C---------------------------------------------------------------------------------
      REAL H_gas_at_Tm
      REAL UA_exh_max

C---------------------------------------------------------------------------------
C State of fuel cell operation the previous iteration through the coefficient
C generator. This is used to "ramp up" the fuel cell's exhaust temperature
C when the fuel cell turns on following an idle period. A value of TRUE indicates
C that the fuel cell was operating the previous iteration through the coefficient
C generator's energy balance section.
C---------------------------------------------------------------------------------
      LOGICAL FC_onoff_previous


C ********************************************************************************
C Declare variables for the user-defined electrical loads placed
C on the fuel cell.
C (Formerly MODULE FC_ELEC_LOADS)
C ********************************************************************************
      COMMON/FC_ELEC_LOADS/IFCloads_ascii,IFCloads,
     &                     FC_lds_input,
     &                     FC_lds_per_rec,FC_lds_per_rec_bin

C---------------------------------------------------------------------------------
C Unit numbers for fuel cell electric loads data files.
C    IFCloads_ascii   the ASCII file that holds the user's data (i.e. .fcl file).
C    IFCloads         the direct access (i.e. binary format) file that is used
C                     during the time-step simulation.
C---------------------------------------------------------------------------------
      INTEGER IFCloads_ascii
      INTEGER IFCloads

C---------------------------------------------------------------------------------
C Method for prescribing electrical loads on fuel cell.
C    FC_lds_annual    named constant to indicate there is a record for each hour
C                     of the year (i.e. 8760 records).
C    FC_lds_daily     named constant to indicate there is record for each minute
C                     of a single day (i.e. 1440 records).
C    FC_lds_input     variable holding user selection (as indicated in .fcl file).
C---------------------------------------------------------------------------------
      INTEGER FC_lds_annual,FC_lds_daily
      PARAMETER ( FC_lds_annual = 1 )
      PARAMETER ( FC_lds_daily  = 2 )
      INTEGER FC_lds_input

C---------------------------------------------------------------------------------
C Number of data items per record of electric loads data in user-specified
C .fcl ASCII file. For `FC_lds_annual' input, the user can specify loads for
C up to 12 increments per hour (i.e. every 5 minutes). For `FC_lds_daily' input,
C the user can specify loads for up to 6 increments per minute (i.e. every 10
C seconds). The direct access file used during the time-step simulation always
C holds 12 increments of data for `FC_lds_annual' and 6 increments of
C data for `FC_lds_daily'.
C    FC_lds_per_rec_annual  named constant to indicate number of records of
C                           loads data for `FC_lds_annual'
C    FC_lds_per_rec_daily   named constant to indicate number of records of
C                           loads of data for `FC_lds_daily'
C    FC_lds_per_rec         variable holding user-specified number of increments
C                           per record (as indicated in .fcl file).
C    FC_lds_per_rec_bin     variable holding number of increments per record
C                           in direct access file (6 or 12, as explained above).
C---------------------------------------------------------------------------------
      INTEGER FC_lds_per_rec_annual,FC_lds_per_rec_daily
      PARAMETER ( FC_lds_per_rec_annual = 12 )
      PARAMETER ( FC_lds_per_rec_daily  =  6 )
      INTEGER FC_lds_per_rec
      INTEGER FC_lds_per_rec_bin


C ********************************************************************************
C Declare variables related to the fuel cell `internal' controller.
C The fuel cell can also be controlled via a control loop using the
C `DG controller' control law (aka as the `external' controller).
C (Formerly MODULE FC_controls)
C ********************************************************************************
      COMMON/FC_controls/FC_avg_steps,FC_avg_steps_rampup,
     &                   FC_tank_energy_capture_avg,
     &                   FC_tank_energy_capture,
     &                   FC_heatX_energy_capture_avg,
     &                   FC_heatX_energy_capture,
     &                   FC_tank_temp_avg,
     &                   FC_tank_temp,
     &                   FC_net_elec_avg,
     &                   FC_net_elec,
     &                   q_net_elec_previous

C---------------------------------------------------------------------------------
C Number of simulation time-steps in plant domain over which running averages
C calculated. Two counters required. One is used for most of the simulation while
C the other is used during the start of the simulation up to the point in time at
C which the averaging vectors are fully populated.
C---------------------------------------------------------------------------------
      INTEGER FC_avg_steps             ! used most of the time
      INTEGER FC_avg_steps_rampup      ! used during ramp-up period

C---------------------------------------------------------------------------------
C Period over which running averages calculated (hours). If this altered, then
C change the dimension of the averaging vectors which follow.
C---------------------------------------------------------------------------------
      INTEGER avg_per
      PARAMETER ( avg_per = 1 )

C---------------------------------------------------------------------------------
C Running averages and arrays holding time-history data used to calculate running
C averages. Averaging vectors dimensioned to handle averaging over a one hour
C period at 1 min or longer time-steps. If `avg_per' changed above, then
C re-dimension these arrays.
C---------------------------------------------------------------------------------
C-----Energy transferred to water from burner or electric resistance element (J).
      REAL FC_tank_energy_capture_avg
 					REAL FC_tank_energy_capture(60)
C-----Energy transferred from fuel cell exhaust to water through heat exchanger (J).
      REAL FC_heatX_energy_capture_avg
    		REAL FC_heatX_energy_capture(60)
C-----Temperature of water in storage tank (oC).
      REAL FC_tank_temp_avg
    		REAL FC_tank_temp(60)
C-----Fuel cell's electrical output (J).
      REAL FC_net_elec_avg
      REAL FC_net_elec(60)

C---------------------------------------------------------------------------------
C Operating point for fuel cell electrical output (W) at previous time-step.
C---------------------------------------------------------------------------------
      REAL q_net_elec_previous

C---------------------------------------------------------------------------------
C Control temperatures used when fuel cell operates with `thermal priority'
C strategy.
C   FC_therm_ctl_maxT  Reduce fuel cell electrical output when tank
C                      temperature exceeds this value (oC).
C   FC_therm_ctl_minT  Increase fuel cell electrical output when tank temperature
C                      drops below this value (oC).
C---------------------------------------------------------------------------------
      REAL FC_therm_ctl_minT, FC_therm_ctl_maxT
      PARAMETER ( FC_therm_ctl_minT = 55. )
      PARAMETER ( FC_therm_ctl_maxT = 60. )


C ********************************************************************************
C Declare variables that track the electrical draws of the
C HOT3000 HVAC equipment (ie. HVAC simulated in the building thermal domain).
C The first implementation considers only the HOT3000 equipment that is used
C in the FCT simulation tool.  This needs to be updated to track other equipment
C in the future.
C (Formerly MODULE FC_ctl_h3khvacloads)
C ********************************************************************************
      COMMON/FC_ctl_h3khvacloads/FCctl_HRV_draw,
     &                           FCctl_AC_compressor,
     &                           FCctl_AC_circfan,
     &                           FCctl_furnace_draft,
     &                           FCctl_furnace_circfan,
     &                           FCctl_pumps,
     &                           FCctl_fans,
     &                           fCurrent_TS_q_total_elec,
     &                           FCctl_boiler_draft,
     &                           FCctl_boiler_circfan,
     &                           FCctl_baseboard_fan,
     &                           FCctl_ashp_compressor,
     &                           FCctl_ashp_circfan,
     &                           FCctl_gshp_pump,
     &                           FCctl_TAC_draw,
     &                           FCctl_elec_water_tank

C---------------------------------------------------------------------------------
C Electrical draw (W) of HRV.  This includes the supply fan, exhaust fan,
C and electrical resistance pre-heater.
C---------------------------------------------------------------------------------
      REAL FCctl_HRV_draw

C---------------------------------------------------------------------------------
C Electrical draw (W) of A/C equipment (ie. ASHP operating in cooling mode).
C One variable for the A/C unit (compressor and condensor fan) and another for the
C ciculating fan that distributes cool air through the house's ductwork.
C---------------------------------------------------------------------------------
      REAL FCctl_AC_compressor
      REAL FCctl_AC_circfan

C---------------------------------------------------------------------------------
C Electrical draw (W) of gas-fired furnace.  One variable for the draft fan
C and another for the ciculating fan that distributes warm air through the
C house's ductwork. If the furnace is a backup system, the circulating fan
C power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
      REAL FCctl_furnace_draft
      REAL FCctl_furnace_circfan

C---------------------------------------------------------------------------------
C Electrical draw (W) of explicit plant pumps and fan.
C---------------------------------------------------------------------------------
      REAL FCctl_pumps
      REAL FCctl_fans
      REAL fCurrent_TS_q_total_elec

C---------------------------------------------------------------------------------
C Electrical draw (W) of gas-fired boiler. One variable for the draft fan
C (applicable for induced draft and condensing boilers). another for the
C circulating fan (though likely there is no circulating fan power for a boiler).
C Power draw of electric boiler not included. If the boiler is a backup system,
C the circulating fan power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
      REAL FCctl_boiler_draft
      REAL FCctl_boiler_circfan

C---------------------------------------------------------------------------------
C Electric draw (W) of circulating fan for baseboards. Power draw of 
C electric baseboards not included. If the baseboard is a backup system,
C the circulating fan power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
      REAL FCctl_baseboard_fan

C---------------------------------------------------------------------------------
C Electric draw (W) of air source heat pump in heating mode. One variable for the
C compressor and condenser fan. One variable for the circulating fan that distributes
C warm air to the house's ductwork. The fan power of the circulating fan for the 
C backup furnace, boiler or baseboard is also included in FCctl_ashp_circfan.
C---------------------------------------------------------------------------------
      REAL FCctl_ashp_compressor
      REAL FCctl_ashp_circfan

C---------------------------------------------------------------------------------
C Electric draw (W) of ground source heat pump. One variable for the pump that
C circulates fluid through the ground loop.  Electric draw  
C for the fan that distributes warm air through the house's ductwork and the 
C compressor is captured in FCctl_ashp_circfan and FCctl_ashp_compressor, respectively.
C---------------------------------------------------------------------------------
      REAL FCctl_gshp_pump

C---------------------------------------------------------------------------------
C Electric draw (W) of thermally activated cooling equipment. This includes the
C refrigerant pump power and the condenser fan power.
C---------------------------------------------------------------------------------
      REAL FCctl_TAC_draw

C---------------------------------------------------------------------------------
C Electric draw (W) of electrically heated water tank.
C---------------------------------------------------------------------------------
      REAL FCctl_elec_water_tank
