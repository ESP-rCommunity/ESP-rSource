C
C Stirling_CHP.h
C
C This header contains data structures associated with the Stirling_CHP
C model.
C
C Requires: cetc_cogen.h

C----------------------------------------------------------------------
C     Named constants
C----------------------------------------------------------------------
C.....Convergence tolerance
      real fConvergence_tolerance
      parameter (fConvergence_tolerance   = 1.0E-06 )
      
C.....Close to zero tolerance      
      real fClose_to_zero_tolerance 
      parameter ( fClose_to_zero_tolerance = 1.0E-06 )   

C.....Model 'type'
      integer iICE_model
      integer iSE_model
      parameter ( iICE_model  = 102, iSE_model = 103 )

C.....Fuel 'type'
      integer iLiquid_fuel
      integer iGaseous_mixture
      parameter (iGaseous_mixture = 2, iLiquid_fuel = 1 )

C.....Flag for internal or external circulator
      integer iInternal_pump
      integer iExternal_pump
      parameter ( iInternal_pump = 1 , iExternal_pump = 2 )
      
C.....Flags for rate-limiting calculation
      integer iStepwise
      integer iLinear
      parameter ( iStepwise = 1 , iLinear = 2 )
      
C----------------------------------------------------------------------
C     Data structures
C----------------------------------------------------------------------
      
C.....Parameters common to A42 CHP and SE models
      common / A42_CHP_common_parameters / 
     &   iFuel_type,
     &   fFuel_LHV,
     &   fFuel_HHV,
     &   fFuel_CO2_intensity,
     &   fLiquid_fuel_CO2_coeff,
     &   iCW_loop_configuration,
     &   fCW_temperature_MAX,
     &   fEffective_MC_engine,
     &   fEffective_UA_HX,
     &   fEffective_UA_loss,
     &   fEffective_MC_HX,
     &   fCorr_MAX_CW_temp,
     &   fCorr_MIN_CW_temp,
     &   fCorr_MAX_CW_flow,
     &   fCorr_MIN_CW_flow,
     &   fPower_Out_MAX,
     &   fPower_Out_MIN,
     &   fCorr_MAX_Power_out,
     &   fCorr_MIN_Power_out,
     &   fPM_elec_coeff,
     &   fPM_ther_coeff,
     &   fPM_air_coeff,
     &   fPM_flow_coeff,
     &   fNominal_engine_temp,
     &   fFuel_Temp_sens,
     &   fPower_Temp_sens,
     &   iCooldown_mode


C.....System Size
      real fSystem_Size (MPCom)
      real fPower_Out_MAX (MPCom)
      real fPower_Out_MIN (MPCom)

C.....Cooling water loop configuration: internal vs external pump:
      integer iCW_loop_configuration(MPCom)
      
C.....Maximum permissible cooling water outlet temperautre (oC)
      real fCW_temperature_MAX (MPCom)
      


C.....Effective thermal mass of engine for thermal mass model (J/K)
      real fEffective_MC_engine (MPCom)

C.....Effective heat transfer coefficient between engine and cooling
C.....water (used in thermal mass model ) (W/K)
      real fEffective_UA_HX (MPCom)

C.....Heat exchanger thermal mass (J/K)
      real fEffective_MC_HX (MPCom)

C.....Heat loss coefficient (W/K)
      real fEffective_UA_loss (MPCom)

C.....Fuel type
      integer iFuel_type(MPCom)

C.....Liquid fuel carbon intensity factor
      real fLiquid_fuel_CO2_coeff (MPCom)      

      
C.....Fuel heating value (J/kg)
      real fFuel_LHV ( MPCom )
      real fFuel_HHV ( MPCom ) 

C.....Carbon dioxide intensity of fuel ( kg CO2 / kg fuel )
      real fFuel_CO2_intensity ( MPCom ) 

C.....Performance map correlation bounds
      real fCorr_MAX_Q_avail ( MPCom )
      real fCorr_MIN_Q_avail ( MPCom )
      real fCorr_MAX_CW_temp ( MPCom )
      real fCorr_MIN_CW_temp ( MPCom )
      real fCorr_MAX_CW_flow ( MPCom )
      real fCorr_MIN_CW_flow ( MPCom )
      real fCorr_MAX_amb_temp ( MPCom )
      real fCorr_MIN_amb_temp ( MPCom )
      real fCorr_MAX_Power_out ( MPCom )
      real fCorr_MIN_Power_out ( MPCom )

C.....Reference conditions for correlations
      real fCorr_REF_CW_temp(MPCom)   ! Cooling water temperature
      real fCorr_REF_CW_flow(MPCom)   ! cooling water flow rate
      real fCorr_REF_amb_temp(MPCom)  ! ambient temperature 

C----------------------------------------------------------------------
C     Performance map coefficients
C     Note: the resulting matricies are likely large & sparse,
C           but a more efficient structure will not be possible
C           f90 ( and dynamic memory allocation ) is available...
C----------------------------------------------------------------------
      integer iNumElecCoeff, iNumTherCoeff, iNumAirCoeff, iNumFlowCoeff
      parameter ( iNumElecCoeff = 27,
     &            iNumTherCoeff = 27,
     &            iNumFlowCoeff = 9,
     &            iNumAirCoeff  = 3  )
     
      real fPM_Flow_Coeff(MPCom,iNumFlowCoeff)
      real fPM_elec_coeff(MPCom,iNumElecCoeff)
      real fPM_ther_coeff(MPCom,iNumTherCoeff)
      real fPM_air_coeff(MPCom,iNumAirCoeff)

C----------------------------------------------------------------------      
C     Reporting variables
C----------------------------------------------------------------------
      common / A42_CHP_results_dyn /
     &   bReport_unit_active,
     &   fReport_Control_Signal,
     &   fReport_Control_Method,
     &   fReport_Net_Power,
     &   fReport_Heat_Recovery,
     &   fReport_Fuel_Flow,
     &   fReport_Air_Flow,
     &   fReport_Fuel_LHV_rate, 
     &   fReport_Containment_Temp,
     &   fReport_CW_Mass_Flow,
     &   fReport_CW_Temperature,
     &   fReport_Aux_Work,
     &   fReport_Aux_Power,
     &   fReport_Heat_Loss,
     &   fReport_Heat_Available, 
     &   fReport_Heat_Transfer,
     &   fReport_Electric_efficiency,
     &   fReport_Thermal_efficiency,
     &   fReport_Cogen_efficiency,
     &   fReport_Engine_work,
     &   fReport_time_spent,
     &   bReport_TS_modes,
     &   fAverage_Net_Power,
     &   fAverage_Heat_Recovery,
     &   fAverage_Fuel_Flow,
     &   fAverage_Fuel_LHV_rate,
     &   fAverage_Electric_efficiency,
     &   fAverage_Thermal_efficiency,
     &   fAverage_Cogen_efficiency,
     &   fAverage_CO2_emissions,
     &   fReport_Alpha,
     &   fReport_TC_eng,
     &   fReport_TC_cw
     

C.....Status of unit (on/off)
      logical bReport_unit_active (MPCom)

C.....Control method and signal
      real fReport_Control_Signal (MPCom)
      real fReport_Control_Method (MPCom)

C.....Gross power (W)
      real fReport_Gross_Power(MPCom)

C.....Net power (W)
      real fReport_Net_Power(MPCom)

C.....Heat recovery (W)
      real fReport_Heat_Recovery(MPCom)

C.....Aux. systems work & power requirements. (W)
      real fReport_Aux_Work(MPCom)
      real fReport_Aux_Power(MPCom)

C.....Heat lost from heater (W)
      real fReport_Heat_Loss(MPCom)

C.....Available heat in heater (W), and heat transferred to engine (W)
      real fReport_Heat_Available(MPCom)
      real fReport_Heat_Transfer(MPCom)

C.....Electric, thermal and cogeneration efficiencies (-)
      real fReport_Electric_efficiency(MPCom)
      real fReport_Thermal_efficiency(MPCom)
      real fReport_Cogen_efficiency(MPCom)
      
C.....Engine work output
      real fReport_Engine_Work (MPCom)

C.....Cooling water temperature (oC) and mass flow rate (kg/s)
      real fReport_CW_Mass_Flow(MPCom)
      real fReport_CW_Temperature(MPCom)

C.....Fuel flow rate, and rate of gross energy input
      real fReport_Fuel_Flow(MPCom)
      real fReport_Fuel_LHV_rate(MPCom)

C.....Combustion air flow
      real fReport_Air_Flow(MPCom)      
            
C.....Containment temperature (oC)
      real fReport_Containment_Temp(MPCom)

C.....Array containing fractions of timestep spent in each operating mode
      real fReport_time_spent (MPCom, iOP_Mode_Count)

C.....Array containing flags that indicate which modes unit operated in 
C.....on the current time step
      logical bReport_TS_modes (MPCom, iOP_Mode_Count)

C.....Time-step averaged fuel flow, net power and heat-recovery
      real fAverage_Net_Power(MPCom)
      real fAverage_Heat_Recovery(MPCom)
      real fAverage_Fuel_Flow(MPCom)
      real fAverage_Fuel_LHV_rate(MPCom)

C.....Time-step averaged efficiency
      real fAverage_Electric_efficiency(MPCom)
      real fAverage_Thermal_efficiency(MPCom)
      real fAverage_Cogen_efficiency(MPCom)

C.....Engine nominal temperature (oC)
      real fNominal_engine_temp(MPCom)

C.....Sensitivity of fuel flow and power output to engine temperature      
      real fFuel_Temp_sens(MPCom)      ! (-)
      real fPower_Temp_sens(MPCom)     ! (-)

C.....Cooldown period mode (optional cooldown/madatory cooldown)
      integer iCooldown_mode(MPCom)

C.....CO2 Emissions ( kg/s )
      real fAverage_CO2_emissions ( MPCom )

C.....Solution implicit/explicit weighting factor
      real fReport_Alpha ( MPCom )

C.....Engine control volume time constant (s)
      real fReport_TC_eng ( MPCom )
      
C.....Cooling water control volume time constant (s)
      real fReport_TC_cw ( MPCom )
