C=======================================================================
C This file contains data structures used by the idealized Integrated
C Mechanical System model (IMS).
C
C=======================================================================

C.....Number of part-load points
      integer iP10_num_SpaceH_PLP
      parameter ( iP10_num_SpaceH_PLP = 3 )

C.....Named constants for part-load ratios
      integer iP10_PLR_15, iP10_PLR_40, iP10_PLR_100
      parameter ( iP10_PLR_15  = 1,
     &            iP10_PLR_40  = 2,
     &            iP10_PLR_100 = 3 )

C.....Number of ventilation modes
      integer iP10_num_vent_modes
      parameter ( iP10_num_vent_modes = 3 )
      
C.....Named consants for ventilaton modes
      integer iP10_NoVent, iP10_LowVent, iP10_HighVent, iP10_SHVent
      parameter ( iP10_NoVent   = 1,
     &            iP10_LowVent  = 2,
     &            iP10_HighVent = 3,
     &            iP10_SHVent   = 4 )

C.....Space heating capacity (W)
      real fP10_SpaceH_capacity(max_sys)

C.....Hourly Water heater delivery rating (L)
      real fP10_WaterH_OneHourRating(max_sys)

C.....Water heater performance factor (-)
      real fP10_WaterH_PF(max_sys)

C.....Space heating net efficiency (-)
      real fP10_SpaceH_NetEff(max_sys,iP10_num_SpaceH_PLP)

C.....Power consumption during space heating (W)
      real fP10_SpaceH_Power(max_sys, iP10_num_SpaceH_PLP)

C.....Power consumption of controls (W)
      real fP10_PowerControls(max_sys)

C.....Power consumption in circulation mode (W)
      real fP10_PowerCirc(max_sys)

C.....Fresh and exhaust ventilation rates (l/s)
      real fP10_Vent_FA_Flow(max_sys,iP10_num_vent_modes)
      real fP10_Vent_EA_Flow(max_sys,iP10_num_vent_modes)
      
C.....Fresh and exhaust fan power consumption (W)
      real fP10_Vent_FA_Power(max_sys,iP10_num_vent_modes)
      real fP10_Vent_EA_Power(max_sys,iP10_num_vent_modes)

C.....Sensible heat recovery efficiecy
      real fP10_Vent_SRE(max_sys, iP10_num_vent_modes)

      common/IMS_P10_inputs/
     &   fP10_SpaceH_capacity,
     &   fP10_WaterH_OneHourRating,
     &   fP10_WaterH_PF,
     &   fP10_SpaceH_NetEff,
     &   fP10_SpaceH_Power,
     &   fP10_PowerControls,
     &   fP10_PowerCirc,
     &   fP10_Vent_FA_Flow,
     &   fP10_Vent_EA_Flow,
     &   fP10_Vent_FA_Power,
     &   fP10_Vent_EA_Power,
     &   fP10_Vent_SRE