C=======================================================================
C This file contains data structures used by the idealized Integrated
C Mechanical System model (IMS).
C
C=======================================================================

C-----------------------------------------------------------------------
C     Data for P.10 description
C-----------------------------------------------------------------------

C.....Number of part-load points
      integer iP10_num_SpaceH_PLP
      parameter ( iP10_num_SpaceH_PLP = 3 )

C.....Named constants for part-load ratios
      integer iP10_PLR_15, iP10_PLR_40, iP10_PLR_100
      parameter ( iP10_PLR_15  = 1,
     &            iP10_PLR_40  = 2,
     &            iP10_PLR_100 = 3 )

C.....Number of ventilation modes
      integer iNum_vent_modes
      parameter ( iNum_vent_modes = 4 )

C.....Named consants for ventilaton modes
      integer iMode_NoVent, iMode_LowVent, iMode_HighVent, iMode_SHVent
      parameter ( iMode_NoVent   = 1,
     &            iMode_LowVent  = 2,
     &            iMode_HighVent = 3,
     &            iMode_SHVent   = 4 )

C.....Hourly Water heater delivery rating (L)
      real fP10_WaterH_OneHourRating(max_sys)

C.....Water heater performance factor (-)
      real fP10_WaterH_PF(max_sys)

C.....Power consumption during space heating (W)
      real fP10_SpaceH_Power(max_sys, iP10_num_SpaceH_PLP)

C.....Power consumption in circulation mode (W)
      real fP10_PowerCirc(max_sys)

C.....Fresh and exhaust ventilation rates (l/s)
      real fP10_Vent_FA_Flow(max_sys,iNum_vent_modes)
      real fP10_Vent_EA_Flow(max_sys,iNum_vent_modes)
      
C.....Fresh and exhaust fan power consumption (W)
      real fP10_Vent_FA_Power(max_sys,iNum_vent_modes)
      real fP10_Vent_EA_Power(max_sys,iNum_vent_modes)

C.....Sensible heat recovery efficiecy
      real fP10_Vent_SRE(max_sys, iNum_vent_modes)

C.....P10 input common
      common/IMS_P10_inputs/
     &   fP10_WaterH_OneHourRating,
     &   fP10_WaterH_PF,
     &   fP10_SpaceH_Power,
     &   fP10_PowerCirc,
     &   fP10_Vent_FA_Flow,
     &   fP10_Vent_EA_Flow,
     &   fP10_Vent_FA_Power,
     &   fP10_Vent_EA_Power,
     &   fP10_Vent_SRE


C-----------------------------------------------------------------------
C     Data for general description
C-----------------------------------------------------------------------

C.....Number of space heating data points
      integer iIMS_NumOPSpaceH(max_sys)

C.....IMS heat source
      integer iIMS_HeatSource(max_sys)

C.....Maximum number of space heating data points
      integer iIMS_MaxOPSpaceH
      parameter ( iIMS_MaxOPSpaceH = 10 )

C.....Space heating capacity (W)
      real fIMS_SpaceH_capacity(max_sys)

C.....Space heating net efficiency (-)
      real fIMS_SpaceH_NetEff(max_sys,iIMS_MaxOPSpaceH)

C.....Power for ventilation fans (supply+exhaust, W)
      real fIMS_VentPower(max_sys, iNum_vent_modes)

C.....Ventilation flow rates (m3/s)
      real fIMS_VentFlow(max_sys, iNum_vent_modes )

C.....Power consumption of controls (W)
      real fIMS_ControlsPower(max_sys)

C.....Incremental consumption of distribution equipment (W)
      real fIMS_IncDistPower(max_sys)

C.....Fan power (W)
      real fIMS_FanPower(max_sys, iIMS_MaxOPSpaceH)

C.....Auxiliary system power (W)
      real fIMS_AuxPower(max_sys, iIMS_MaxOPSpaceH)

C.....Number of WaterH data points
      integer iIMS_NumOPWaterH (max_sys)

C.....Maximum number of water heating data points
      integer iIMS_MaxOPWaterH
      parameter ( iIMS_MaxOPWaterH = 10 )

C.....WaterH Efficiency
      real fIMS_WaterHEff( max_sys, iIMS_MaxOPWaterH )

C.....WaterH Power consumption
      real fIMS_WaterHPower( max_sys, iIMS_MaxOPWaterH)

C.....Estimated WaterH skin losses
      real fIMS_WaterHSkinLoss(max_sys)

C.....Flue/Skin loss ratio
      real fIMS_FlueSkinLossRatio(max_sys)

C.....General model input common
      common/IMS_General_inputs/
     &   iIMS_HeatSource,
     &   iIMS_NumOPSpaceH,
     &   fIMS_SpaceH_capacity,
     &   fIMS_SpaceH_NetEff,
     &   fIMS_VentPower,
     &   fIMS_VentFlow,
     &   fIMS_ControlsPower,
     &   fIMS_IncDistPower,
     &   fIMS_FanPower,
     &   fIMS_AuxPower,
     &   iIMS_NumOPWaterH,
     &   fIMS_WaterHEff,
     &   fIMS_WaterHSkinLoss,
     &   fIMS_WaterHPower,
     &   fIMS_FlueSkinLossRatio









