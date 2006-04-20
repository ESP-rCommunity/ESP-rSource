c=================================== Common H3KSTORE_DATA Begin =======================================
C ( JWH reduce number of continuation lines )
C AIM:iNumberOfBuildingTimeSteps(months)
c HRV: fHRV_Elec_Energy(months)
C GSHP: fGSHP_pump_energy_monthly(months)
C GCEP: fGCEP_pump_energy_monthly(months)
c H3KOUTPUT HVAC: sum_plr_capacity_bin(max_sys,ipercent_capacity)
c H3KOUTPUT HVAC: imax_air_change_day(months)
c ESP-r Load Storage Routines:Solar_From_Extern_Monthly(mcom,months)

      common/H3KSTORE_DATA/iCurrentMonth,
     & iNumberOfBuildingTimeSteps(months),
     & iNumberOfPlantTimeSteps(months),fHeating_load_monthly(months),
     & fCooling_load_monthly(months),fFan_energy_monthly(months),
     & fPilot_energy_monthly(max_sys,months),
     & fHeat_energy_monthly(max_sys,months),
     & fCool_energy_monthly(max_sys,months),fHRV_Elec_Energy(months),
     & fGSHP_pump_energy_monthly(months),
     & fGCEP_pump_energy_monthly(months)

c Declare variables associated with H3KSTORE_DATA common
      INTEGER iCurrentMonth, iNumberOfBuildingTimeSteps
      INTEGER iNumberOfPlantTimeSteps
c AIM
      REAL fHeating_load_monthly, fCooling_load_monthly
      REAL fFan_energy_monthly, fPilot_energy_monthly
      REAL fHeat_energy_monthly, fCool_energy_monthly
c HRV
      REAL fHRV_Elec_Energy

C GSHP and GCEP.
      REAL fGSHP_pump_energy_monthly, fGCEP_pump_energy_monthly

c H3KOUTPUT HVAC related data.
      common/H3KSTORE_HVAC/
     & sum_plr_capacity_bin(max_sys,ipercent_capacity),
     & plr_percent_capacity_bin(max_sys,ipercent_capacity),
     & total_num_of_hours(max_sys),underheated_hours(max_sys),
     & foundation_losses(mcom,months),imax_air_change_day(months),
     & imax_air_change_hour(months),fmax_air_change(months),
     & imin_air_change_day(months),imin_air_change_hour(months),
     & fmin_air_change(months),faverage_air_change(months),
     & fnumber_of_hours(months),fmonthly_air_infiltration(months)

      REAL sum_plr_capacity_bin, plr_percent_capacity_bin
      REAL total_num_of_hours, underheated_hours, foundation_losses
      INTEGER imax_air_change_day, imax_air_change_hour
      REAL fmax_air_change
      INTEGER imin_air_change_day, imin_air_change_hour
      REAL fmin_air_change, faverage_air_change
      REAL fnumber_of_hours, fmonthly_air_infiltration

c Data from ESP-r Solar Load Storage Routines
      common/H3KSTORE_SOLAR/
     & Solar_From_Extern_Monthly(mcom,months),
     & Solar_From_Intern_Monthly(mcom,months),
     & Solar_Abs_From_Trans_Monthly(mcom,months),
     & Solar_Abs_From_Opaque_Monthly(mcom,months),
     & Casual_Radiant_Monthly(mcom,months),
     & Casual_Conv_Monthly(mcom,months),
     & fSummed_Zone_Temps(mcom,months)

      REAL Solar_From_Extern_Monthly, Solar_From_Intern_Monthly
      REAL Solar_Abs_From_Trans_Monthly, Solar_Abs_From_Opaque_Monthly
      REAL Casual_Radiant_Monthly, Casual_Conv_Monthly
      REAL fSummed_Zone_Temps

c=============================== Common for H3KSTORE_DATA End ================================

c=============================== Common H3KSTORE_FC_DATA Start ================================
      common/H3KSTORE_FC_DATA/fPreviousTimeStepNumber,
     & fH3KSTORE_FC_q_net_elec(months),fH3KSTORE_FC_q_total(months),
     & fH3KSTORE_FC_q_nonHVAC_el(months),
     & fH3KSTORE_FC_q_HVAC_elec(months),
     & iPreviousTimeStepMonth,iPreviousTimeStepDay,
     & fPreviousTimeStepHour,iNumberTimeStepsFC_Active(months),
     & fPreviousTS_q_total_elec,fPreviousTS_q_nonHVAC_elec,
     & fPreviousTS_q_HVAC_elec,fPreviousTS_q_net_elec,
     & fPreviousTS_q_par_elec,fPreviousTS_eta_fc,
     & fPreviousTS_FC_Vdot_fuel_STP,fPreviousTS_Vdot_air_STP,
     & fPreviousTS_stoichs,fPreviousTS_Vdot_liqwater,
     & fPreviousTS_Vdot_exh_STP,fPreviousTS_T_exh,
     & fPreviousTS_T_exit,fPreviousTS_Twater_enter,
     & fPreviousTS_UA_exh,fPreviousTS_fHeatToWater,
     & fH3KSTORE_FC_Vdot_fuel_STP(months),
     & fH3KSTORE_FC_eta_fc(months),fH3KSTORE_FC_q_par_elec(months)

      REAL fPreviousTimeStepNumber, fH3KSTORE_FC_q_net_elec
      REAL fH3KSTORE_FC_q_total, fH3KSTORE_FC_q_nonHVAC_el
      REAL fH3KSTORE_FC_q_HVAC_elec
 
      INTEGER iPreviousTimeStepMonth, iPreviousTimeStepDay
      REAL fPreviousTimeStepHour
      INTEGER iNumberTimeStepsFC_Active
      REAL fPreviousTS_q_total_elec, fPreviousTS_q_nonHVAC_elec
      REAL fPreviousTS_q_HVAC_elec, fPreviousTS_q_net_elec         
      REAL fPreviousTS_q_par_elec, fPreviousTS_eta_fc             
      REAL fPreviousTS_FC_Vdot_fuel_STP, fPreviousTS_Vdot_air_STP          
      REAL fPreviousTS_stoichs, fPreviousTS_Vdot_liqwater     
      REAL fPreviousTS_Vdot_exh_STP, fPreviousTS_T_exh             
      REAL fPreviousTS_T_exit, fPreviousTS_Twater_enter    
      REAL fPreviousTS_UA_exh, fPreviousTS_fHeatToWater
      REAL fH3KSTORE_FC_Vdot_fuel_STP, fH3KSTORE_FC_eta_fc
      REAL fH3KSTORE_FC_q_par_elec


      common/H3KSTORE_FC_EL/
     & fPreviousTS_q_EL_element,fPreviousTS_q_EL_capture_fut,
     & fPreviousTS_T_EL_Node1_fut,fPreviousTS_T_EL_1a_present,
     & fPreviousTS_f_EL_First_Conn,fPreviousTS_T_EL_1b_present,
     & fPreviousTS_f_EL_Second_Conn,fPreviousTS_T_EL_makeup_fut,
     & fPreviousTS_f_EL_DHW_draw_fut,fPreviousTS_f_EL_JacketLosses,
     & fPreviousTS_UA_EL_Tank,fPreviousTS_T_EL_Room_fut,
     & fPreviousTS_f_EL_heatdump,fH3KSTORE_EL_q_element(months),
     & fH3KSTORE_EL_T_Node1_future(months),
     & fPreviousTS_f_FF_Vdot_fuel_STP,
     & fPreviousTS_q_FF_capture_fut,fPreviousTS_T_FF_node1_fut,
     & fPreviousTS_f_FF_T_1a_present,fPreviousTS_f_FF_First_Conn,
     & fPreviousTS_f_FF_T_1b_present,fPreviousTS_f_FF_Second_Conn,
     & fPreviousTS_T_FF_makeup_fut,fPreviousTS_f_FF_DHW_draw_fut,
     & fPreviousTS_f_FF_JacketLosses, fPreviousTS_UA_FF_Tank,
     & fPreviousTS_T_FF_Room_fut, fPreviousTS_f_FF_heatdump

      REAL fPreviousTS_q_EL_element, fPreviousTS_q_EL_capture_fut    
      REAL fPreviousTS_T_EL_Node1_fut, fPreviousTS_T_EL_1a_present
      REAL fPreviousTS_f_EL_First_Conn, fPreviousTS_T_EL_1b_present
      REAL fPreviousTS_f_EL_Second_Conn, fPreviousTS_T_EL_makeup_fut     
      REAL fPreviousTS_f_EL_DHW_draw_fut, fPreviousTS_f_EL_JacketLosses
      REAL fPreviousTS_UA_EL_Tank, fPreviousTS_T_EL_Room_fut  
      REAL fPreviousTS_f_EL_heatdump, fH3KSTORE_EL_q_element
      REAL fH3KSTORE_EL_T_Node1_future, fPreviousTS_f_FF_Vdot_fuel_STP   
      REAL fPreviousTS_q_FF_capture_fut, fPreviousTS_T_FF_node1_fut 
      REAL fPreviousTS_f_FF_T_1a_present, fPreviousTS_f_FF_First_Conn  
      REAL fPreviousTS_f_FF_T_1b_present, fPreviousTS_f_FF_Second_Conn 
      REAL fPreviousTS_T_FF_makeup_fut, fPreviousTS_f_FF_DHW_draw_fut  
      REAL fPreviousTS_f_FF_JacketLosses, fPreviousTS_UA_FF_Tank          
      REAL fPreviousTS_T_FF_Room_fut, fPreviousTS_f_FF_heatdump

      common/H3KSTORE_FF/fH3KSTORE_FF_Vdot_fuel_STP(months),
     & fH3KSTORE_FF_T_node1_future(months),iCurrent_FCT_OUTPUT_FILE,
     & iNumber_Of_Lines_In_FCT_File,
     & bQ1Started,bQ2Started,bQ3Started,bQ4Started,
     & Num_pumps,Num_fans,pump_IPCOMP(50),fan_IPCOMP(50),
     & fPlant_Fan_Elec(months),fPlant_Pump_Elec(months),
     & fPreviousTS_Plant_Fan_Elec,fPreviousTS_Plant_Pump_Elec,
     & fCurrentTS_Plant_Fan_Elec,fCurrentTS_Plant_Pump_Elec,
     & fCurrentTimestepNumber,iCurrentTimestepMonth,
     & iCurrentTimestepDay,fCurrentTimestepHour,
     & fCurrentTS_q_total_elec,fCurrentTS_q_nonHVAC_elec,
     & fCurrentTS_q_HVAC_elec,fCurrentTS_q_net_elec,
     & fCurrentTS_q_par_elec,fCurrentTS_eta_fc,
     & fCurrentTS_FC_Vdot_fuel_STP,fCurrentTS_Vdot_air_STP,
     & fCurrentTS_stoichs,fCurrentTS_Vdot_liqwater,
     & fCurrentTS_Vdot_exh_STP,fCurrentTS_T_exh,fCurrentTS_T_exit,
     & fCurrentTS_Twater_enter,fCurrentTS_UA_exh,fCurrentTS_fHeatToWater

      REAL fH3KSTORE_FF_Vdot_fuel_STP, fH3KSTORE_FF_T_node1_future
      INTEGER  iCurrent_FCT_OUTPUT_FILE, iNumber_Of_Lines_In_FCT_File
      Logical  bQ1Started,bQ2Started,bQ3Started,bQ4Started

C Variables hold the number of pumps and fans, respectively, defined in plant network.
      INTEGER  Num_pumps, Num_fans

C Integer arrays for storing the component numbers of any explicit pumps and fans 
C modelled. The size of the array is set according to the maximum number of 
C plant components (set to 50 in plant.h).
      INTEGER  pump_IPCOMP, fan_IPCOMP

C Arrays for storing monthly explicit plant pump and fan electrical energy
C and variables for storing the explicit plant pump and fan electrical energy
C values from the previous timestep (once converged).
      REAL fPlant_Fan_Elec, fPlant_Pump_Elec, fPreviousTS_Plant_Fan_Elec
      REAL fPreviousTS_Plant_Pump_Elec, fCurrentTS_Plant_Fan_Elec
      REAL fCurrentTS_Plant_Pump_Elec

C Current Timestep Data last timestep hour, month, day and hour.
      REAL      fCurrentTimestepNumber         !Keeps track of last timestep hour
      INTEGER   iCurrentTimestepMonth          !Date and time info.
      INTEGER   iCurrentTimestepDay
      REAL      fCurrentTimestepHour

      REAL       fCurrentTS_q_total_elec       !elec demands from previous timestep
      REAL       fCurrentTS_q_nonHVAC_elec
      REAL       fCurrentTS_q_HVAC_elec
      REAL       fCurrentTS_q_net_elec         !elec output from FC
      REAL       fCurrentTS_q_par_elec         !
      REAL       fCurrentTS_eta_fc             !Fuel Cell electrical efficiency.
      REAL       fCurrentTS_FC_Vdot_fuel_STP   !
      REAL       fCurrentTS_Vdot_air_STP          
      REAL       fCurrentTS_stoichs           
      REAL       fCurrentTS_Vdot_liqwater     
      REAL       fCurrentTS_Vdot_exh_STP      
      REAL       fCurrentTS_T_exh             
      REAL       fCurrentTS_T_exit            
      REAL       fCurrentTS_Twater_enter    
      REAL       fCurrentTS_UA_exh           
      REAL       fCurrentTS_fHeatToWater

C Commons related to EL data.
      common/H3KSTORE_EL/fCurrentTS_q_EL_element,
     &fCurrentTS_q_EL_capture_fut,fCurrentTS_T_EL_Node1_fut,
     &fCurrentTS_T_EL_1a_present,fCurrentTS_f_EL_First_Conn,
     &fCurrentTS_T_EL_1b_present,fCurrentTS_f_EL_Second_Conn,
     &fCurrentTS_T_EL_makeup_fut,fCurrentTS_f_EL_DHW_draw_fut,
     &fCurrentTS_f_EL_Jacket_losses,fCurrentTS_f_EL_heatdump,
     &fCurrentTS_UA_EL_Tank,fCurrentTS_T_EL_Room_fut,
     &fCurrentTS_f_FF_Vdot_fuel_STP,fCurrentTS_q_FF_capture_fut,  
     &fCurrentTS_T_FF_node1_fut,fCurrentTS_f_FF_T_1a_present,
     &fCurrentTS_f_FF_First_Conn,fCurrentTS_f_FF_T_1b_present,
     &fCurrentTS_f_FF_Second_Conn,fCurrentTS_T_FF_makeup_fut,
     &fCurrentTS_f_FF_DHW_draw_fut,fCurrentTS_f_FF_JacketLosses,
     &fCurrentTS_f_FF_heatdump,fCurrentTS_UA_FF_Tank,
     &fCurrentTS_T_FF_Room_fut

c Declare variables in fuel common block data
      REAL fCurrentTS_q_EL_element, fCurrentTS_q_EL_capture_fut    
      REAL fCurrentTS_T_EL_Node1_fut, fCurrentTS_T_EL_1a_present
      REAL fCurrentTS_f_EL_First_Conn, fCurrentTS_T_EL_1b_present
      REAL fCurrentTS_f_EL_Second_Conn, fCurrentTS_T_EL_makeup_fut     
      REAL fCurrentTS_f_EL_DHW_draw_fut, fCurrentTS_f_EL_Jacket_losses    
      REAL fCurrentTS_f_EL_heatdump, fCurrentTS_UA_EL_Tank
      REAL fCurrentTS_T_EL_Room_fut

      REAL fCurrentTS_f_FF_Vdot_fuel_STP, fCurrentTS_q_FF_capture_fut  
      REAL fCurrentTS_T_FF_node1_fut, fCurrentTS_f_FF_T_1a_present
      REAL fCurrentTS_f_FF_First_Conn, fCurrentTS_f_FF_T_1b_present
      REAL fCurrentTS_f_FF_Second_Conn, fCurrentTS_T_FF_makeup_fut   
      REAL fCurrentTS_f_FF_DHW_draw_fut, fCurrentTS_f_FF_JacketLosses     
      REAL fCurrentTS_f_FF_heatdump, fCurrentTS_UA_FF_Tank
      REAL fCurrentTS_T_FF_Room_fut

C Arrays for storing monthly explicit plant pump and fan electrical energy
C and variables for storing the explicit plant pump and fan electrical energy
C values from the previous timestep (once converged).
c      REAL      fCurrentTS_Plant_Fan_Elec
c      REAL      fCurrentTS_Plant_Pump_Elec

c================================== Common H3KSTORE_FC_DATA End===================================      

