C
C Hydrogen_PEMFC.F
C
C This file contains data structures associated with the hydrogen
C PEMFC model.
C
C Requires headers plant.h & cetc_cogen.h
C


C.....Model type
      integer iModel_Empirical        ! <- empirical model
      integer iModel_Mechanistic      ! <- mechanistic model
                                      !    (not yet supported)

      parameter ( iModel_Empirical = 107  )
      parameter ( iModel_Mechanistic = -1 )

C.....Named constants
      real fNerst_Voltage                 ! Ideal LHV voltage
      parameter ( fNerst_Voltage = 1.25 ) ! Volts (source ?)

C.....Common for storing model parameters
      common / H2_PEMFC_Empirical_Params /
     &            iNum_Cells_in_Stack,
     &            fCW_Temp_MAX,
     &            fReference_CW_Temp,
     &            fReference_CW_Flow,
     &            fReference_Amb_Temp,
     &            fSkinLoss_Coeff,
     &            fTher_Eff_Coeff,
     &            fElec_Eff_Coeff,
     &            fAux_Power_Coeff,
     &            fCorr_Range_LHV_MIN,
     &            fCorr_Range_LHV_MAX,
     &            fCorr_Range_CW_temp_MIN,
     &            fCorr_Range_CW_temp_MAX,
     &            fCorr_Range_CW_flow_MIN,
     &            fCorr_Range_CW_flow_MAX,
     &            fCorr_Range_AMB_temp_MIN,
     &            fCorr_Range_AMB_temp_MAX,
     &            fInitial_accum_time




C.....Common for storing model parameters
      integer iNum_Cells_in_Stack(MPCom)      ! # of cells in stack
      real fCW_Temp_MAX(MPCom)                ! Max CW *outlet* temperature (oC)
      real fReference_CW_Temp(MPCom)          ! Refernece CW temp for correlations (oC)
      real fReference_CW_Flow(MPCom)          ! Reference CW flow for correlations (kg/s)
      real fReference_Amb_Temp(MPCom)         ! Reference ambient temp for correlations (oC)
      real fSkinLoss_Coeff(MPCom,5)           ! Heat loss correlation coefficients
      real fTher_Eff_Coeff(MPCom,8)           ! Thermal efficiency correlation coefficients
      real fElec_Eff_Coeff(MPCom,8)           ! Electrical efficiency correlation coefficients
      real fAux_Power_Coeff(MPCom,7)          ! Aux. Power use correlation coefficients
      real fCorr_Range_LHV_MIN(MPCom)         ! Correlation fuel LHV minimum bound (W)
      real fCorr_Range_LHV_MAX(MPCom)         ! Correlation fuel LHV maximum bound (W)
      real fCorr_Range_CW_temp_MIN(MPCom)     ! Correlation CW temp minimum bound (oC)
      real fCorr_Range_CW_temp_MAX(MPCom)     ! Correlation CW temp maximum bound (oC)
      real fCorr_Range_CW_flow_MIN(MPCom)     ! Correlation CW flow minimum bound (kg/s)
      real fCorr_Range_CW_flow_MAX(MPCom)     ! Correlation CW flow maximum bound (kg/s)
      real fCorr_Range_AMB_temp_MIN(MPCom)    ! Correlation AMB temp minimum bound (oC)
      real fCorr_Range_AMB_temp_MAX(MPCom)    ! Correlation AMB temp maximum bound (oC)
      real fInitial_accum_time(MPCom)         ! Unit's accumulated operating time at
                                              !      start of simulation (s)


C.....Common storing data to be reported.
      common /  H2_PEMFC_Reporting_vars /
     &      fReport_heat_TS_avg,
     &      fReport_DC_Power_TS_avg,
     &      fReport_AC_Power_TS_avg,
     &      fReport_Skin_Losses_TS_avg,
     &      fReport_Stack_Voltage,
     &      fReport_Stack_Current_TS_avg,
     &      fReport_Stack_Eff_Elec_TS_avg,
     &      fReport_Stack_Eff_Ther_TS_avg,
     &      fReport_Stack_Eff_Cogen_TS_avg,
     &      fReport_Hydrogen_flow,
     &      fReport_Hydrogen_demand,
     &      fReport_Hydrogen_LHV,
     &      fReport_Hydrogen_demand_LHV,
     &      iReport_Control_method,
     &      fReport_Control_signal,
     &      fReport_TS_modes

C.....Reporting variables

      real fReport_heat_TS_avg(MPCom)             ! Time step-averaged heat output (W)
      real fReport_DC_Power_TS_avg(MPCom)         ! Time step-averaged DC power output (W)
      real fReport_AC_Power_TS_avg(MPCom)         ! Time step-averaged AC power output (W)
      real fReport_Skin_Losses_TS_avg(MPCom)      ! Time step-averaged heat losses  (W)
      real fReport_Stack_Voltage(MPCom)           ! Stack voltage (V)
      real fReport_Stack_Current_TS_avg(MPCom)    ! Time step-averaged current (A)
      real fReport_Stack_Eff_Elec_TS_avg(MPCom)   ! Time step-averaged electric efficiency
      real fReport_Stack_Eff_Ther_TS_avg(MPCom)   ! Time step-averaged thermal efficiency
      real fReport_Stack_Eff_Cogen_TS_avg(MPCom)  ! Time step-averated cogen efficiency
      real fReport_Hydrogen_flow(MPCom)           ! Time step hydrogen flow rate (kg/s)
      real fReport_Hydrogen_demand(MPCom)         ! Unit's hydrogen demand (kg/s)
      real fReport_Hydrogen_LHV(MPCom)            ! LHV value of hydrogen (W)
      real fReport_Hydrogen_demand_LHV(MPCom)     ! LHV value of requested hydrogen (W)
      integer iReport_Control_method(MPCom)       ! Control scheme
      real fReport_Control_signal(MPCom)          ! Signal from external controller.
      real fReport_TS_modes(MPCom,iOP_Mode_Count) ! time spent in each operating mode (s)





