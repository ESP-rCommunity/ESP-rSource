C
C cetc_cogen.h
C
C This file contains generic data structures used in CETC's cogeneration model
C

C.....Models supporting cetc_cogen.h interface
      integer iH2_PEMFC_empirical
      parameter ( iH2_PEMFC_empirical = 107 )
      integer iStirling_CHP_DYN
      parameter ( iStirling_CHP_DYN = 102 )

C.....Named constants

      integer iEngine_deactivated    ! Named constant indicating that a
                                     !   high-level controller has requested
                                     !   the engine be deactivated
      integer iDimensionless_control ! Named constant indicating that the
                                     !   controller is providing 
      integer iPower_out_controlled  ! Named constant indicating the model's
                                     !   fuel flow has been specified
                                     !   by external controller
      integer iHeat_out_controlled   ! Named constant indicating model's
                                     !   thermal output is controlled

      parameter ( iEngine_deactivated    = 0,
     &            iDimensionless_control = 1,
     &            iPower_out_controlled  = 2,
     &            iHeat_out_controlled   = 3 )




C.....System operating modes
      integer iOP_inoperative         ! Unit is inoperative
      integer iOP_startup             ! Unit is starting-up
      integer iOP_warmup              ! Unit is warming up
      integer iOP_normal_operation    ! Normal, active operation
      integer iOP_shutdown            ! Unit is shutting down



      parameter ( iOP_inoperative        = 1,
     &            iOP_startup            = 2,
     &            iOP_warmup             = 3,
     &            iOP_normal_operation   = 4,
     &            iOP_shutdown           = 5 )


C.....System error flags
      integer iStatus_Normal                 ! normal operation
      integer iStatus_Fuel_fault             ! fuel shortage
      integer iStatus_CW_fault               ! cooling water is too hot
      integer iStatus_Corr_Err               ! correlation error 
      parameter ( iStatus_Normal     = 0,
     &            iStatus_Fuel_fault = 1,
     &            iStatus_CW_fault   = 2,
     &            iStatus_Corr_Err   = 3)


C.....Cooldown mode
      integer iCM_mandatory
      integer iCM_optional
      parameter ( iCM_mandatory = 1,
     &            iCM_optional  = 2 )

C.....Misc. Array dimensions
      integer iOP_Mode_Count
      parameter ( iOP_Mode_Count = 5 )


C.....Close to zero tolerance
      real fCogen_zero_tolerance
      parameter ( fCogen_zero_tolerance = 1.0E-06 )

C.....CETC cogen parameters
      common / cetc_cogen_models /
     &            fNominal_Power_Output,
     &            fFuel_Flow_MIN,
     &            fFuel_Flow_MAX,
     &            fPeriod_duration,
     &            fPeriod_fuel_flow,
     &            fPeriod_AC_Power,
     &            fPeriod_Heat_recovery,
     &            fFuel_Flow_Change_MAX,
     &            fGross_Power_Change_MAX,
     &            fHeat_Recovery_Change_MAX

      real fNominal_Power_Output(MPCom)       ! Nominal power output (W)
      real fFuel_Flow_MIN(MPCom)              ! Min fuel flow (kg/s)
      real fFuel_Flow_MAX(MPCom)              ! Max fuel flow (kg/s)
      real fPeriod_duration(MPCom,iOP_Mode_Count)      ! duration of periods (s)
      real fPeriod_Fuel_Flow(MPCom,iOP_Mode_Count)     ! Hydrogen flow during periods (kg/s)
      real fPeriod_AC_Power(MPCom,iOP_Mode_Count)      ! AC power produced during periods (W)
      real fPeriod_Heat_Recovery(MPCom,iOP_Mode_Count) ! heat recovered during periods (W)
      real fFuel_Flow_Change_MAX(MPCom)       ! MAX change in fuel flow (kg/s^2)
      real fGross_Power_Change_MAX(MPCom)     ! MAX change in power output (W/s)
      real fHeat_Recovery_Change_MAX(MPCom)   ! MAX change in heat output  (W/s)


