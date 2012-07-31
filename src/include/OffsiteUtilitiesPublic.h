
C-----------------------------------------------------------------------
C     This file contains data structures used to track fuel consumption,
C     costs and green-house-gas emissions associated with a building's
C     energy use. These data are public, and may be used in any
C     routine referencing the offsite utilities facility.
C-----------------------------------------------------------------------

C.....Number of fuel types currently supported:
      integer iNumFuel
      parameter ( iNumFuel = 8 )

C.....Named constants for fuel types
      integer iElectricity, iNaturalGas, iOil, iPropane, iMixedWood,
     &        iHardWood, iSoftWood, iWoodPellets
      parameter ( iElectricity = 1,
     &            iNaturalGas  = 2,
     &            iOil         = 3,
     &            iPropane     = 4,
     &            iMixedWood   = 5,
     &            iHardWood    = 6,
     &            iSoftWood    = 7,
     &            iWoodPellets = 8 )

C.....Supported end-uses
      integer iNumUses
      parameter ( iNumUses = 9 )
      integer iUseSpaceHeat, iUseSpaceCool, iUseDHW,
     &        iUseLights, iUseEquipment, iUseUncatagorized,
     &        iUseHRV, iUseOther, iExtraBillingCharges
      parameter ( iUseSpaceHeat     = 1,
     &            iUseSpaceCool     = 2,
     &            iUseDHW           = 3,
     &            iUseLights        = 4,
     &            iUseEquipment     = 5,
     &            iUseUncatagorized = 6,
     &            iUseHRV           = 7,
     &            iUseOther         = 8,
                  ! 25-Nov-2011 BAL: Added. Only used to store monthly minimum fuel cost charges. 
     &            iExtraBillingCharges = 9 ) 

C.....Number of "components" in ESP-r that report data to OffsiteUtilities
      integer iNumOffsiteUtilComp
      parameter ( iNumOffsiteUtilComp = 5 )

C.....Named constants for components that report data

      integer iIdealHvac
      parameter ( iIdealHvac = 1 )      ! Ideal HVAC model (hvacsim.F)

      integer iIdealDHW
      parameter ( iIdealDHW = 2 )       ! Ideal DHW model (dhw_module.F)

      integer iPltComponents
      parameter ( iPltComponents = 3 )  ! Explicit plant models (esruplt/)

      integer iIdealVent
      parameter ( iIdealVent = 4 )       ! Ideal Ventilation/HRV model (CentralVentSys.F)

      integer iH3KBaseLoads
      parameter ( iH3KBaseLoads = 5 )    ! HOT3000 Base Loads linked with BCD file (casual.F)

C-----------------------------------------------------------------------
C     Named constants used to describe plt/pfs loads and
C     energy conversion.
C-----------------------------------------------------------------------

! C.....Domains
!       integer iDomainCount
!       parameter ( iDomainCount = 2 )
!       integer iDomainPlt, iDomainPfS
!       parameter ( iDomainPlt = 1 )     ! Plant domain
!       parameter ( iDomainPfs = 2 )     ! Electric domain

C.....Types of end-use loads
      integer iLoadCount
      parameter ( iLoadCount = 4 )
      integer iLoadSpaceH, iLoadSpaceC, iLoadWaterH, iLoadElec
      parameter ( iLoadSpaceH = 1 )            ! Space heating
      parameter ( iLoadSpaceC = 2 )            ! Space cooling 
      parameter ( iLoadWaterH = 3 )            ! Water heating 
      parameter ( iLoadElec   = 4 )            ! electric loads 

! C.....Types of generation
!       integer ( iGenCount = 3 )
!       integer iGenHeat, iGenCool, iGenElec
!       parameter ( iGenHeat = 1 )            ! Heat generation
!       parameter ( iGenCool = 2 )            ! Cooling generation
!       parameter ( iGenElec = 3 )            ! Electricity generation



C-----------------------------------------------------------------------
C     Array for passing fuel energy consumption into StoreSiteFuelUse.
C     This variable *should not* be common!
C-----------------------------------------------------------------------
      real fSUFuelEnergyUse(iNumFuel,iNumUses)  ! (W)

C-----------------------------------------------------------------------
C     String names and units for the various fuel types
C-----------------------------------------------------------------------
      integer iFuelNameLen( iNumFuel ), iFuelUnitLen( iNumFuel ),
     &        iFuelDescLen( iNumFuel )

      character*24 cFuelName( iNumFuel ), cFuelUnit( iNumFuel ),
     &             cFuelDesc( iNumFuel )

      save iFuelNameLen, iFuelUnitLen, iFuelDescLen
      save cFuelName, cFuelUnit, cFuelDesc

C.....Electricity
      data cFuelName(iElectricity) /'electricity'/
      data iFuelNameLen(iElectricity) /11/

      data cFuelDesc(iElectricity) /'electricity'/
      data iFuelDescLen(iElectricity) /11/

      data cFuelUnit(iElectricity) /'(kWh/s)'/
      data iFuelUnitLen(iElectricity) /7/

C.....Oil
      data cFuelName(iOil) /'oil'/
      data iFuelNameLen(iOil) /3/

      data cFuelDesc(iOil) /'oil'/
      data iFuelDescLen(iOil) /3/

      data cFuelUnit(iOil) /'(l/s)'/
      data iFuelUnitLen(iOil) /5/

C.....Natural gas
      data cFuelName(iNaturalGas) /'natural_gas'/
      data iFuelNameLen(iNaturalGas) /11/

      data cFuelDesc(iNaturalGas) /'natural gas'/
      data iFuelDescLen(iNaturalGas) /11/

      data cFuelUnit(iNaturalGas) /'(m3/s)'/
      data iFuelUnitLen(iNaturalGas) /6/

C.....Propane
      data cFuelName(iPropane) /'propane'/
      data iFuelNameLen(iPropane) /7/

      data cFuelDesc(iPropane) /'propane'/
      data iFuelDescLen(iPropane) /7/

      data cFuelUnit(iPropane) /'(m3/s)'/
      data iFuelUnitLen(iPropane) /6/

C.....HardWood
      data cFuelName(iHardWood) /'hard_wood'/
      data iFuelNameLen(iHardWood) /9/

      data cFuelDesc(iHardWood) /'hard wood'/
      data iFuelDescLen(iHardWood) /9/

      data cFuelUnit(iHardWood) /'(tonne/s)'/
      data iFuelUnitLen(iHardWood) /9/

C.....SoftWood
      data cFuelName(iSoftWood) /'soft_wood'/
      data iFuelNameLen(iSoftWood) /9/

      data cFuelDesc(iSoftWood) /'soft wood'/
      data iFuelDescLen(iSoftWood) /9/

      data cFuelUnit(iSoftWood) /'(tonne/s)'/
      data iFuelUnitLen(iSoftWood) /9/

C.....MixedWood
      data cFuelName(iMixedWood) /'mixed_wood'/
      data iFuelNameLen(iMixedWood) /10/

      data cFuelDesc(iMixedWood) /'mixed wood'/
      data iFuelDescLen(iMixedWood) /10/

      data cFuelUnit(iMixedWood) /'(tonne/s)'/
      data iFuelUnitLen(iMixedWood) /9/

C.....WoodPellets
      data cFuelName(iWoodPellets) /'wood_pellets'/
      data iFuelNameLen(iWoodPellets) /12/

      data cFuelDesc(iWoodPellets) /'wood pellets'/
      data iFuelDescLen(iWoodPellets) /12/

      data cFuelUnit(iWoodPellets) /'(tonne/s)'/
      data iFuelUnitLen(iWoodPellets) /9/

C-----------------------------------------------------------------------
C     String names for the various end-uses
C-----------------------------------------------------------------------
      integer iUseNameLen(iNumUses), iUseDescLen(iNumUses)
      character*24 cUseName(iNumUses), cUseDesc(iNumUses)
      save iUseNameLen, cUseName, iUseDescLen, cUseDesc

C.....Space heating 
      data cUseName(iUseSpaceHeat) /'space_heating'/
      data iUseNameLen(iUseSpaceHeat) /13/

      data cUseDesc(iUseSpaceHeat) /'space heating'/
      data iUseDescLen(iUseSpaceHeat) /13/

C.....Space cooling
      data cUseName(iUseSpaceCool) /'space_cooling'/
      data iUseNameLen(iUseSpaceCool) /13/

      data cUseDesc(iUseSpaceCool) /'space cooling'/
      data iUseDescLen(iUseSpaceCool) /13/

C.....Water heating
      data cUseName(iUseDHW) /'water_heating'/
      data iUseNameLen(iUseDHW) /13/

      data cUseDesc(iUseDHW) /'water heating'/
      data iUseDescLen(iUseDHW) /13/

C.....Appliances/equipment
      data cUseName(iUseEquipment) /'equipment'/
      data iUseNameLen(iUseEquipment) /9/

      data cUseDesc(iUseEquipment) /'equipment'/
      data iUseDescLen(iUseEquipment) /9/

C.....Lights
      data cUseName(iUseLights) /'lights'/
      data iUseNameLen(iUseLights) /6/

      data cUseDesc(iUseLights) /'lights'/
      data iUseDescLen(iUseLights) /6/

C.....Uncatagorized
      data cUseName(iUseUncatagorized) /'uncatagorized'/
      data iUseNameLen(iUseUncatagorized) /13/

      data cUseDesc(iUseUncatagorized) /'uncatagorized'/
      data iUseDescLen(iUseUncatagorized) /13/

C.....Ventilation/HRV
      data cUseName(iUseHRV) /'ventilation'/
      data iUseNameLen(iUseHRV) /11/

      data cUseDesc(iUseHRV) /'ventilation'/
      data iUseDescLen(iUseHRV) /11/
      
C.....Other
      data cUseName(iUseOther) /'other'/
      data iUseNameLen(iUseOther) /5/

      data cUseDesc(iUseOther) /'other'/
      data iUseDescLen(iUseOther) /5/ 

C.....Extra billing charges
      data cUseName(iExtraBillingCharges) /'extra_billing_charges'/
      data iUseNameLen(iExtraBillingCharges) /21/

      data cUseDesc(iExtraBillingCharges) /'extra_billing_charges'/
      data iUseDescLen(iExtraBillingCharges) /21/ 