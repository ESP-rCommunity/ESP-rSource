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

C.....Dynamic thermal model
      integer iHeat_lag_model
      integer iThermal_mass_model
      parameter ( iHeat_lag_model = 1 , iThermal_mass_model = 2 )      

C----------------------------------------------------------------------
C     Data structures
C----------------------------------------------------------------------
      
C.....Model parameters
      common / Stirling_coefficients_dyn /
     &   fFuel_LHV, fCW_temperature_MAX, iDynamic_thermal_flag,
     &   fHeat_lag_TC, fEffective_MC_engine, fEffective_UA_HX,
     &   fEffective_UA_loss, fEffective_MC_HX,
     &   fSkin_loss_C_1, fSkin_loss_C_2, fSkin_loss_C_3,
     &   fSkin_loss_C_4, fSkin_loss_C_5, fSkin_loss_C_6,
     &   fHeat_eff_C_1, fHeat_eff_C_2, fHeat_eff_C_3, fHeat_eff_C_4,
     &   fHeat_eff_C_5, fHeat_eff_C_6, fHeat_eff_C_7, fHeat_eff_C_8,
     &   fHeat_eff_C_9, fElec_eff_C_1, fElec_eff_C_2, fElec_eff_C_3,
     &   fElec_eff_C_4, fElec_eff_C_5, fElec_eff_C_6, fElec_eff_C_7,
     &   fElec_eff_C_8, fElec_eff_C_9,
     &   fCorr_REF_CW_temp, fCorr_REF_CW_flow, fCorr_REF_amb_temp,
     &   fCorr_MAX_CW_temp, fCorr_MIN_CW_temp, fCorr_MAX_CW_flow,
     &   fCorr_MIN_CW_flow, fCorr_MAX_amb_temp, fCorr_MIN_amb_temp,
     &   fCorr_MAX_Q_avail, fCorr_MIN_Q_avail


C.....System Size
      real fSystem_Size (MPCom)

C.....Maximum permissible cooling water outlet temperautre (oC)
      real fCW_temperature_MAX (MPCom)
      
C.....Flag indicating if heat lag or thermal mass model will
C.....be used to describe dynamic themal behavior      
      integer iDynamic_thermal_flag (MPCom)

C.....Time constant for heat lag model (1/s)      
      real fHeat_lag_TC (MPCom)
      
      
C.....Effective thermal mass of engine for thermal mass model (J/K)
      real fEffective_MC_engine (MPCom)

C.....Effective heat transfer coefficient between engine and cooling
C.....water (used in thermal mass model ) (W/K)
      real fEffective_UA_HX (MPCom)

C.....Heat exchanger thermal mass (J/K)
      real fEffective_MC_HX (MPCom)

C.....Heat loss coefficient (W/K)
      real fEffective_UA_loss (MPCom)

C.....Fuel heating value (J/kmol)
      real fFuel_LHV ( MPCom )

C.....LOW-resolution model correlation bounds
      real fCorr_MAX_Q_avail ( MPCom )
      real fCorr_MIN_Q_avail ( MPCom )
      real fCorr_MAX_CW_temp ( MPCom )
      real fCorr_MIN_CW_temp ( MPCom )
      real fCorr_MAX_CW_flow ( MPCom )
      real fCorr_MIN_CW_flow ( MPCom )
      real fCorr_MAX_amb_temp ( MPCom )
      real fCorr_MIN_amb_temp ( MPCom )

      
C.....Heat loss
      real fSkin_loss_C_1( MPCom )
      real fSkin_loss_C_2( MPCom )
      real fSkin_loss_C_3( MPCom )
      real fSkin_loss_C_4( MPCom )
      real fSkin_loss_C_5( MPCom )
      real fSkin_loss_C_6( MPCom )
C.....Heater efficiency
      real fHeat_eff_C_1( MPCom )
      real fHeat_eff_C_2( MPCom )
      real fHeat_eff_C_3( MPCom )
      real fHeat_eff_C_4( MPCom )
      real fHeat_eff_C_5( MPCom )
      real fHeat_eff_C_6( MPCom )
      real fHeat_eff_C_7( MPCom )
      real fHeat_eff_C_8( MPCom )
      real fHeat_eff_C_9( MPCom )
C.....Mechanical conversion efficiency
      real fElec_eff_C_1( MPCom )
      real fElec_eff_C_2( MPCom )
      real fElec_eff_C_3( MPCom )
      real fElec_eff_C_4( MPCom )
      real fElec_eff_C_5( MPCom )
      real fElec_eff_C_6( MPCom )
      real fElec_eff_C_7( MPCom )
      real fElec_eff_C_8( MPCom )
      real fElec_eff_C_9( MPCom )


      common / Stirling_results_dyn /
     &   bReport_unit_active, fReport_Control_Signal,
     &   fReport_Control_Method, fReport_Gross_Power,
     &   fReport_Net_Power, fReport_Heat_Recovery,
     &   fReport_Fuel_Flow, fReport_Fuel_LHV_rate, 
     &   fReport_Containment_Temp, fReport_CW_Mass_Flow,
     &   fReport_CW_Temperature, fReport_Aux_Work,
     &   fReport_Aux_Power, fReport_Heat_Loss,
     &   fReport_Heat_Available, fReport_Heat_Transfer,
     &   fReport_Electric_efficiency, fReport_Thermal_efficiency,
     &   fReport_Cogen_efficiency, fReport_Engine_work,
     &   fReport_time_spent, bReport_TS_modes,
     &   fAverage_Net_Power, fAverage_Heat_Recovery,
     &   fAverage_Fuel_Flow, fAverage_Fuel_LHV_rate,
     &   fAverage_Electric_efficiency,fAverage_Thermal_efficiency,
     &   fAverage_Cogen_efficiency

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

C.....Reference conditions for correlations
      real fCorr_REF_CW_temp(MPCom)   ! Cooling water temperature
      real fCorr_REF_CW_flow(MPCom)   ! cooling water flow rate
      real fCorr_REF_amb_temp(MPCom)  ! ambient temperature 
