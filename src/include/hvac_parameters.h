c Declare integer variables in parameter file hvac.h

c Maximum number of hvac systems that can be declared in the .hvac file
      INTEGER max_sys
      parameter (max_sys = 5)

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

C Named constants for ASHP operation
      integer iASHP_heating
      integer iASHP_cooling
      parameter( iASHP_heating = 1, iASHP_cooling = 2 )

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

C Patrice -> Commented out all parameters associated with the GCEP model
C These are already in GCEP_parameters.h

C Parameters associated with GCEP_INPUT common statement

c      REAL L_res_x
c      REAL L_res_y
c      REAL Inter_Tg

c      INTEGER max_gcep_systems
c      INTEGER Ndx_res
c      INTEGER Ndy_res
c      INTEGER Ndx_bor_c
c      INTEGER Ndy_bor_c
c      INTEGER Ndx_bor
c      INTEGER Ndy_bor
c      INTEGER Xm
c      INTEGER Xw
c      INTEGER Xd
c      INTEGER Xh
c      INTEGER NHis

c      Parameter (max_gcep_systems = 1)
C Length of the thermal reservoirs in X and Y
c      Parameter (L_res_x=500.0)
c      Parameter (L_res_y=L_res_x)
C Number of elements to mesh the reservoirs in X and Y
c      Parameter (Ndx_res=20)
c      Parameter (Ndy_res=Ndx_res)
C Number of elements on each side of the borefields (1/2 times the space between borefields)
c      Parameter ( Ndx_bor_c=5)
c      Parameter (Ndy_bor_c=Ndx_bor_c)
C Number of elements for borehole+space on each side (1 element forr borehole)
c      Parameter (Ndx_bor=2*Ndx_bor_c+1)
c      Parameter (Ndy_bor=2*Ndy_bor_c+1)
C Number of hours between numerical calculations of ground temperatures
c      Parameter (Inter_Tg=336)
C Number of hours in every term of the aggregation scheme (m=month, w=week, d=day, h=number of non-aggregated hours kept)
c      Parameter (Xm=360)
c      Parameter (Xw=168)
c      Parameter (Xd=48)
c      Parameter (Xh=12)
C Number of ground loads held in the history
c      Parameter (NHis=1+Xm+Xw+Xd+Xh)

C Parameters associated with GSHP_INPUT common

      INTEGER max_layer
C Maximum number of soil layers
      Parameter (max_layer=10)

