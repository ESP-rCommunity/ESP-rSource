c Declare integer variables in parameter file hvac.h

c Maximum number of hvac systems that can be declared in the .hvac file
      INTEGER max_sys
      parameter (max_sys = 25)

C Named constants for system types
      integer iSysFurnace, iSysBoiler, iSysBaseboard, iSysIMS_P10,
     &        iSysASHP, iSysGSHP, iSysGSHP_GCEP

      parameter (iSysFurnace         = 1,
     &           iSysBoiler          = 2,
     &           iSysBaseboard       = 3,
     &           iSysIMS_P10         = 4,
     &           iSysASHP            = 7,
     &           iSysGSHP            = 8,
     &           iSysGSHP_GCEP       = 9 )

C Named constants for primary/backup
      integer iHVACPrimarySys, iHVACBackupSys
      parameter ( iHVACPrimarySys = 1,
     &            iHVACBackupSys  = 2 )

C Named constants for furnace Types
      integer iFurn_AdvWoodStove
      integer iFurn_CatConverter
      integer iFurn_FlameRetenHead
      integer iFurn_FlueVentDamper
      integer iFurn_SparkIgn
      integer iFurn_SparkIgnVentDamper
      integer iFurn_ContPilot
      integer iFurn_Condensing
      integer iFurn_NonCondensingDirVent
      integer iFurn_InducedDraft
      integer iFurn_MidEfficiency
      integer iFurn_ElecForcedAir
      integer iFurn_Wood
      integer iFurn_Oil
      
      parameter( iFurn_AdvWoodStove             = 1   )
      parameter( iFurn_CatConverter             = 2   )
      parameter( iFurn_FlameRetenHead           = 3   )
      parameter( iFurn_FlueVentDamper           = 4   )
      parameter( iFurn_SparkIgn                 = 5   )
      parameter( iFurn_SparkIgnVentDamper       = 6   )
      parameter( iFurn_ContPilot                = 7   )
      parameter( iFurn_Condensing               = 8   )
      parameter( iFurn_NonCondensingDirVent     = 9   )
      parameter( iFurn_InducedDraft             = 10  )
      parameter( iFurn_MidEfficiency            = 11  )
      parameter( iFurn_ElecForcedAir            = 12  )
      parameter( iFurn_Wood                     = 13  )
      parameter( iFurn_Oil                      = 14  )


C Named constants for furnace fan operation
      integer iFan_None, iFan_Auto, iFan_Cont
      parameter ( iFan_None = 0,
     &            iFan_Auto = 1,
     &            iFan_Cont = 2 )

C Named constants for ASHP operation
      integer iASHP_heating
      integer iASHP_cooling
      parameter( iASHP_heating = 1, iASHP_cooling = 2 )

C Named consants for ASHP perforamnce
      integer iASHP_GoodUnit
      integer iASHP_TypUnit
      integer iASHP_PoorUnit
      parameter( iASHP_TypUnit=1, iASHP_GoodUnit=2, iASHP_PoorUnit=3 )


C Named constants for IMS heat sources:
      integer iIMS_CondensingTank
      integer iIMS_ConventionalTank
      integer iIMS_InducedDraftFanTank
      integer iIMS_InstantaneousHeater
      integer iIMS_TanklessHeater
      parameter ( iIMS_CondensingTank       = 1,
     &            iIMS_ConventionalTank     = 2,
     &            iIMS_InducedDraftFanTank  = 3,
     &            iIMS_InstantaneousHeater  = 4,
     &            iIMS_TanklessHeater       = 5  )


C Named constants for fan position
      integer iFan_BlowThrough, iFan_DrawThrough
      parameter ( iFan_BlowThrough = 1,
     &            iFan_DrawThrough = 2  )

C Named constants for system functions
      integer iFuncHeating, iFuncCooling
      parameter ( iFuncHeating = 1,
     &            iFuncCooling = 2 )

C Named constants for heat pump heating control
      integer iTemp_BalancedPoint, iTemp_Restricted, iTemp_Unrestricted
      parameter ( iTemp_BalancedPoint = 1,
     &            iTemp_Restricted    = 2,
     &            iTemp_Unrestricted  = 3 )


C Named constants for heat pump cooling control
      integer iCool_Economizer, iCool_Conventional
      parameter ( iCool_Conventional = 1,
     &            iCool_Economizer   = 2  )

C Named constants for economizer control
      integer iEcon_TempIntegrated, iEcon_TempNonIntegrated,
     &        iEcon_EnthIntegrated, iEcon_EnthNonIntegrated
      parameter ( iEcon_TempIntegrated    = 1,
     &            iEcon_TempNonIntegrated = 2,
     &            iEcon_EnthIntegrated    = 3,
     &            iEcon_EnthNonIntegrated = 4   )


c Maximum number of hvac system types (furnace, heat pump, etc ...). This defines
c the size of the hvac array ihvac_type
      INTEGER max_hvac_types
      parameter (max_hvac_types = 8)

c Maximum number of variations for a specific hvac system type. For a furnace for
c instance, we can have condensing, induced draft, spark ignition, etc ... This
c parameter defines the maximum number of these variations. This parameter sets
c the size of the hvac array isys_type
      INTEGER max_hvac_vars
      parameter (max_hvac_vars = 10)

c Maximum number of fuels. This sets the size of the array ifuel_type
      INTEGER max_fuels
      parameter (max_fuels = 6)


C Dimensions of token/data arrays for version 2+ format.
      integer iFurnaceTokenCount
      parameter (iFurnaceTokenCount = 8)
      integer iP10_IMSTokenCount
      parameter (iP10_IMSTokenCount = 21)
      integer iBaseboardTokenCount
      parameter (iBaseboardTokenCount = 4)
      integer iHPTokenCount
      parameter (iHPTokenCount = 24)
      integer iMAXTokenCount
      parameter (iMAXTokenCount = iHPTokenCount)


C Parameters associated with GSHP_INPUT common

      INTEGER max_layer
C Maximum number of soil layers
      Parameter (max_layer=10)

C Named constants for token, parameter searches
      integer iSearchDataTokens
      integer iSearchParamTokens
      parameter ( iSearchDataTokens  = 1,
     &            iSearchParamTokens = 2 )

