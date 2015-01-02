C-----------------------------------------------------------------------
C     This file contains data structures used to track fuel consumption,
C     costs and green-house-gas emissions associated with a building's
C     energy use. These data are private, and are only used withn
C     the offsite utilities facility.
C-----------------------------------------------------------------------

C.....Counters
      integer iComponent, iFuel, iEndUse

      common/EnergyUse/fSiteEnergyUse, 
     &                 fPltEnergyUse, 
     &                 fPFSElectricityUse

C.....Common storing site-wide energy use
      real fSiteEnergyUse(iNumOffsiteUtilComp, iNumFuel, iNumUses)

C.....Common storing energy use specific to plant components
C.....(MPCom is the maximum number of plant components.)
      real fPltEnergyUse(MPCom, iNumFuel, iNumUses)

      
      real fPfsElectricityUse(MPOWCOM,iNumUses)
      
C.....Array indicating if energy used by plt components is catagorized, or not.
      logical bPltUseCatagorized(MPCom, iNumFuel)
      common/PltFuelCatagorize/bPltUseCatagorized

C.....Total energy used by fuel type (W)
      real fTotalEnergyUse( iNumFuel )
C.....Actual fuel consumption (various units)
      real fTotalFuelUse( iNumFuel )
C.....Total GHG emmissions  by fuel type (kg)
      real fTotalGHGEmissions( iNumFuel )

C.....Energy use by end-use (W) 
      real fEndUseEnergyUse( iNumFuel, iNumUses )
C.....Actual fuel consumption by fuel, end/use (various units)
      real fEndUseFuelUse( iNumFuel, iNumUses )
C.....Total GHG emmissions  by end-use (kg)
      real fEndUseGHGEmissions( iNumUses )

      common/SiteFuelUse/fTotalEnergyUse,
     &                   fTotalFuelUse,
     &                   fTotalGHGEmissions,
     &                   fEndUseFuelUse,
     &                   fEndUseEnergyUse,
     &                   fEndUseGHGEmissions

C.....Calorific value of fuels
      real fFuelConversionFactor(iNumFuel)

C-----------------------------------------------------------------------
C     Calorific value of fuels
C
C     These heating values were pulled from Hot2000 source code by
C     Brian Bradley (NRCan).
C
C     WOOD:
C      - Hardwood:    30.5  GJ/cord (17738 MJ/Tonne)   
C      - Softwood:    18.49 GJ/cord (10753 MJ/Tonne)   
C      - Mixed wood:  24.0  GJ/cord (13956 MJ/Tonne)   
C      - pellets                    (19800 MJ/Tonne)   
C
C     NATURAL GAS:    26.8392 M3/GJ
C     OIL:            25.9578 L/GJ
C     PROPANE:        39.065  L/GJ (?!?)
C         
C-----------------------------------------------------------------------

C.....Electricity: Convert W->kWH
      data fFuelConversionFactor(iElectricity) / 2.77778E-07 /

C.....Natural gas: Convert W->m3
      data fFuelConversionFactor(iNaturalGas) / 26.8932E-09 /

C.....Oil: Convert W->L
      data fFuelConversionFactor(iOil) / 25.9578E-09 /

C.....PROPANE: Convert W->L
      data fFuelConversionFactor(iPropane) / 39.065E-09 /

C.....Hardwood: Convert W->Tonne
      data fFuelConversionFactor(iHardWood) / 5.63761E-11 /

C.....Softwood: Convert W->Tonne
      data fFuelConversionFactor(iSoftWood) / 9.29973E-11 /

C.....Mixed Wood: Convert W->Tonne
      data fFuelConversionFactor(iMixedWood) / 7.16538E-11 /

C.....Wood Pellets Convert W->Tonne
      data fFuelConversionFactor(iWoodPellets) / 5.0505E-11 /

