!---------------------------------------------------------------------------------
! Constant initialization.
!---------------------------------------------------------------------------------

! Specific heat of water given at Joules/(Litres*Celsius).
      REAL fSpecHeatWater
      PARAMETER (fSpecHeatWater = 4190.0)
! PI.
      REAL PI
      PARAMETER ( PI=3.141592 ) 

! Fuel Type Parameter.
      INTEGER ELECTRICITY
      PARAMETER (ELECTRICITY =1 )
      INTEGER NATURALGAS
      PARAMETER ( NATURALGAS =2 )
      INTEGER OIL
      PARAMETER ( OIL =3 )
      INTEGER PROPANE
      PARAMETER ( PROPANE =4 )
      INTEGER WOOD
      PARAMETER ( WOOD =5 )
      INTEGER SOLAR
      PARAMETER ( SOLAR =6 )

!Tank Type (propane,Gas)
      INTEGER Conventional_tank
      PARAMETER ( Conventional_tank = 1 )
      INTEGER Conventional_tank_pilot
      PARAMETER ( Conventional_tank_pilot = 2 )
      INTEGER Tankless
      PARAMETER ( Tankless = 3 )
      INTEGER Instantaneous 
      PARAMETER ( Instantaneous = 4 )
      INTEGER Instantaneous_pilot
      PARAMETER ( Instantaneous_pilot = 5 )
      INTEGER Induced_draft_fan
      PARAMETER ( Induced_draft_fan = 6 )
      INTEGER Induced_draft_fan_pilot
      PARAMETER ( Induced_draft_fan_pilot = 7)
      INTEGER Direct_vent
      PARAMETER ( Direct_vent = 8 )
      INTEGER Direct_vent_pilot
      PARAMETER ( Direct_vent_pilot = 9 )
      INTEGER Condensing
      PARAMETER ( Condensing = 10 )
!Tank Type (Oil) 
      INTEGER Oil_conventional_tank
      PARAMETER ( Oil_conventional_tank = 11 )
      INTEGER Oil_tankless
      PARAMETER ( Oil_tankless = 12 )
!TankType  (Wood) 
      INTEGER Wood_Fireplace
      PARAMETER ( Wood_Fireplace = 13 )
      INTEGER Wood_stove_water_coil
      PARAMETER ( Wood_stove_water_coil = 14 )
!Tank Type (Elec)
      INTEGER Elec_Conventional_tank
      PARAMETER ( Elec_Conventional_tank = 15 )
      INTEGER Elec_Conserver_tank
      PARAMETER ( Elec_Conserver_tank = 16 )
      INTEGER Elec_Tankless_Heatpump
      PARAMETER ( Elec_Tankless_Heatpump = 17 )
      INTEGER Elec_Heatpump
      PARAMETER ( Elec_Heatpump = 18 )
      INTEGER Elec_Instantaneous
      PARAMETER ( Elec_Instantaneous = 19 )
!TankType  (Solar)
      INTEGER Solar_Collector_System
      PARAMETER ( Solar_Collector_System = 20 )
      


! Time Type Parameters.
      REAL fHOURSINDAY 
      PARAMETER ( fHOURSINDAY = 24. )
      REAL fSECONDSINHOUR 
      PARAMETER ( fSECONDSINHOUR = 3600.0 ) 
      REAL fSECONDSINDAY 
      PARAMETER ( fSECONDSINDAY = 86400.0 )

! DOE Constants For UA Estimate
      REAL  fDOELitresUsedOneDay 
      PARAMETER ( fDOELitresUsedOneDay  = 243.4 ) !DOE standard demand on DHW
      REAL  fDOETankTemp 
      PARAMETER ( fDOETankTemp = 57.2)     !Standard DOE Tank Temp
      REAL  fDOEColdMainTemp
      PARAMETER ( fDOEColdMainTemp = 14.3) !Standard Cold main Temp
      REAL  fDOERoomTemp
      PARAMETER ( fDOERoomTemp = 20.0)     !Satndard Doe Zone Temp
      REAL   fUsageTemp 
      PARAMETER ( fUsageTemp = 60.0)       !Satndard DOE Usage Temp
      REAL  fGammaFactor
      PARAMETER ( fGammaFactor = 0.6)      !Factor of Flue losses of Pilot light (Brian Bradley)
      REAL  fPilotToSpaceHeat
      PARAMETER ( fPilotToSpaceHeat = 0.22)  !Factor of Energy of Pilot light imparted to Zone
      REAL  fTankHeight
      PARAMETER ( fTankHeight = 1.28 )     !Estm. tank height in meters
      REAL  fFreductFactor
      PARAMETER ( fFreductFactor = 0.93 )  !Freduct factor used in H2K sim runs only
      REAL  iH2K
      PARAMETER ( iH2K = 0 )   !Set to 0 to run in H3k mode or 1 for H2k Mode
