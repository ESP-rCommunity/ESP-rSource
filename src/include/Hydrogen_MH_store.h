
C Hydrogen_MH_store.h

C This file contains data structures associated with the hydrogen-
C metal hydride storage tank.

C.....Model parameters
      common / hydrogen_MH_param /
     &    fMH_kmols_alloy,
     &    fMH_free_volume,
     &    fMH_cyl_mass_kg,
     &    fMH_cyl_wall_C,
     &    fMH_cyl_wall_hA,
     &    fMH_Heat_XFer_UA,
     &    fMH_alloy_C,
     &    fMH_hydride_C,
     &    fMH_concentration_TRANS,
     &    fMH_concentration_MAX,
     &    fMH_concentration_MIN,
     &    fMH_pressure_MIN,
     &    fMH_pressure_MAX,
     &    fMH_pressure_INIT,
     &    fMH_PCT_param_Kc,
     &    fMH_PCT_param_b1o,
     &    fMH_PCT_param_b2o,
     &    fMH_PCT_param_dH1,
     &    fMH_PCT_param_dH2,
     &    fMH_PCT_param_Do,
     &    fMH_PCT_param_dHs

C.....Cylinder properties

      real fMH_cyl_mass_kg ( MPCom )           ! Mass of cylinder (kg)
      real fMH_cyl_wall_C  ( MPCom )           ! Specific heat of cylinder wall (J/kg K)

      real fMH_kmols_alloy ( MPCom )           ! # of mol of alloy in cylinder
                                               !    when uncharged (kmol)

      real fMH_cyl_wall_hA ( MPCom )           ! Cylinder wall overall heat transfer
                                               !    coefficient (W/K)

      real fMH_Heat_XFer_UA ( MPCom )          ! Heat exchanger effective heat
                                               !    transfer coefficient (W/K)                                               
                                                     
      real fMH_alloy_C     ( MPCom )           ! Specific heat of unabsorbed alloy (J/kmol K)
      real fMH_hydride_C   ( MPCom )           ! Specific heat of unabsorbed hydride (J/kmol K)


      real fMH_free_volume ( MPCom )           ! free volume of cylinder (m3)

      real fMH_pressure_MAX ( MPCom )          ! Maximum pressure permitted in
                                               !    cylinder (kPa)
      real fMH_pressure_MIN ( MPCom )          ! Minimum downstream operating
                                               !    pressure (kPa)
      real fMH_pressure_INIT ( MPCom )         ! Pressure in cylinder at start of
                                               !    simulation (kPa)
C.....PCT parameters            
      real fMH_concentration_TRANS ( MPCom )   ! Concentration corresponding to
                                               !    transition from undercharged
                                               !    to plateau regions. (mol/mol)
      real fMH_concentration_MIN ( MPCom )     ! Minimum concentration permitted
                                               !    in cylinder (mol/mol)
      real fMH_concentration_MAX ( MPCom )     ! Maximum concentration permitted
                                               !    in cylinder (mol/mol)

      real fMH_PCT_param_b1o ( MPCom )         ! PCT curve coefficient
      real fMH_PCT_param_b2o ( MPCom )         ! PCT curve coefficient
      
      real fMH_PCT_param_dH1 ( MPCom )         ! Enthalpy of absorbtion below
                                               !    transition concentration (J/kmol)
      real fMH_PCT_param_dH2 ( MPCom )         ! Enthalpy of absorbtion above
                                               !    transition concentration (J/kmol)
      
      real fMH_PCT_param_dHs ( MPCom )         ! PCT curve coefficient
      real fMH_PCT_param_Do  ( MPCom )         ! PCT curve coefficient
      
      real fMH_PCT_param_Kc  ( MPCom )         ! PCT mass transfer coefficient (1/s)
      



      

C.....Reporting variables
      common / hydrogen_MH_report /
     &   fMH_Report_H2_free_kmol,
     &   fMH_Report_H_abs_kmol,
     &   fMH_Report_H2_charge,
     &   fMH_Report_H2_discharge,
     &   fMH_Report_H2_vent,
     &   fMH_Report_Pressure,
     &   fMH_Report_Demand_IMP,
     &   fMH_Report_Demand_EXP,
     &   fMH_Report_Containment_TEMP,
     &   fMH_Report_Q_absorbtion,
     &   fMH_Report_Charge_FWork,
     &   fMH_Report_Disch_FWork,
     &   fMH_Report_Charge_dU

     

      real fMH_Report_H2_free_kmol ( MPCom )   ! Free hydrogen in cylinder (kmol)
      real fMH_Report_H_abs_kmol   ( MPCom )   ! Absorbed hydrogen in cylinder (kmol)
      real fMH_Report_H2_charge    ( MPCom )   ! Rate of charge into cylinder (kmol/s)
      real fMH_Report_H2_discharge ( MPCom )   ! Rate at which cylinder discharges (kmol/s)
      real fMH_Report_H2_vent      ( MPCom )   ! Rate at which hydrogen is vented (kmol/s)
      real fMH_Report_Pressure     ( MPCom )   ! Pressure inside the cylinder (PA)
      real fMH_Report_Demand_IMP   ( MPCom )   ! Hydrogen required by implicitly modelled
                                               !   plant components (kmol/s)
      real fMH_Report_Demand_EXP   ( MPCom )   ! Hydrogen required by explicitly modelled
                                               !   plant components (kmol/s)
      real fMH_Report_Containment_TEMP (MPCom) ! Component containment temp (oC)
      real fMH_Report_Q_absorbtion ( MPCom )   ! Heat released/absorbed during absorbtion/desorption (W)
      real fMH_Report_Charge_FWork ( MPCom )   ! Flow work associated w/ charging (W)
      real fMH_Report_Disch_FWork ( MPCom )    ! Flow work associated w/ discharging (W)
      real fMH_Report_Charge_dU   ( MPCom )    ! Internal energu change occuring in charging fluid (W)
