C     OffsiteUtilitiesPrivate.h

C-----------------------------------------------------------------------
C     This file contains data structures used to track fuel consumption,
C     costs and green-house-gas emissions associated with a building's
C     energy use. These data are private, and are only used withn
C     the offsite utilities facility.
C-----------------------------------------------------------------------


C.....Counters
      integer iSiteComp, iFuel, iEndUse, iPltComp, iPowocComp, iZone
     
      common/FuelCost/fSiteFuelCost, fPltFuelCost,
     &                 fH3KBaseLoadsFuelCost

C.....Common storing site-wide fues costs
      real fSiteFuelCost(iNumOffsiteUtilComp, iNumFuel, iNumUses)

C.....Common storing fuel costs specific to plant components
C.....(MPCom is the maximum number of plant components.)
      real fPltFuelCost(MPCom, iNumFuel, iNumUses)
      
C.....Common storing fuel costs specific to HOT3000 Base Loads
C       MCOM (maximum number of zones) is provided by building.h
      real fH3KBaseLoadsFuelCost(MCOM, iNumFuel, iNumUses)


C     Commons storing energy use by site utility or plant component
      common/EnergyUse/fSiteEnergyUse, fPltEnergyUse, fPowocEnergyUse,
     &  fCasualNGEnergyUse, fH3KBaseLoadsEnergyUse

C.....Common storing site-wide energy use
      real fSiteEnergyUse(iNumOffsiteUtilComp, iNumFuel, iNumUses)
C.....Common storing energy use specific to plant components
C       MPCom (maximum number of plant components) is provided by plant.h
      real fPltEnergyUse(MPCom, iNumFuel, iNumUses)
C.....Common storing energy use specific to Power only components
C       MPOWCOM (maximum number of power only components) is provided by power.h
      real fPowocEnergyUse(MPOWCOM, iNumFuel, iNumUses)
C.....Common storing energy use specific to Casual gain NG components (e.g. NG clothes dryer or stove)
C       MCOM (maximum number of zones) is provided by building.h
      real fCasualNGEnergyUse(MCOM, iNumFuel, iNumUses)
C.....Common storing energy use specific to HOT3000 Base Loads
C       MCOM (maximum number of zones) is provided by building.h
      real fH3KBaseLoadsEnergyUse(MCOM, iNumFuel, iNumUses)

C.....Array indicating if energy used by plt components is catagorized, or not.
      common/PltFuelCatagorize/bPltUseCatagorized
      logical bPltUseCatagorized(MPCom, iNumFuel)


C     Common storing characteristics (energy, mass/volume, GHG) by energy source and end-use
      common/SiteFuelUse/fTotalEnergyUse,
     &                   fTotalFuelUse,
     &                   fTotalGHGEmissions,
     &                   fTotalFuelCost,
     &                   fEndUseFuelUse,
     &                   fEndUseEnergyUse,
     &                   fEndUseGHGEmissions,
     &                   fEndUseFuelCost
     
C.....Total energy used by fuel type (W)
      real fTotalEnergyUse( iNumFuel )
C.....Actual fuel consumption (various units)
      real fTotalFuelUse( iNumFuel )
C.....Total GHG emmissions  by fuel type (kg)
      real fTotalGHGEmissions( iNumFuel )
C.....Total fuel cost by fuel type
      real fTotalFuelCost( iNumFuel)
      
C.....Energy use by end-use (W) 
      real fEndUseEnergyUse( iNumFuel, iNumUses )
C.....Actual fuel consumption by fuel, end/use (various units)
      real fEndUseFuelUse( iNumFuel, iNumUses )
C.....Total GHG emmissions  by end-use (kg)
      real fEndUseGHGEmissions(iNumFuel, iNumUses )
C.....Total fuel cost by fuel, end-use
      real fEndUseFuelCost ( iNumFuel, iNumUses )

C.....Arrays for storing fuel rate blocks
C.....Uses the following energy unit convention:
C           Electricity -> kWh
C           Natural Gas -> m3
C           Oil         -> L
C           Propane     -> L
C           Wood(mixed) -> Tonne
C
      common/SiteFuelRates/fFuelBlockUnits,
     &                   fFuelBlockCost,
     &                   fFuelMinCharge,
     &                   fFuelMinUnits,
     &                   fSumFuelUse,
     &                   bIncFuelCostCalcs,
     &                   iCurrentMonth,
     &                   bApplyMonthlyMinCharge,
     &                   bTOUelecRates,
     &                   iTOUcolumn
     
C.....Minimum charge for each fuel type
      real fFuelMinCharge( iNumFuel, 12 )
C.....Minimum units for each fuel type
      real fFuelMinUnits( iNumFuel, 12 )      
C.....Number of energy units for a max. of 4 rate blocks
      real fFuelBlockUnits( iNumFuel, 12, 4 )
C.....Fuel cost for each rate block
      real fFuelBlockCost( iNumFuel, 12, 4 )      
C.....Sum of fuel cost - used to determine which block rate
C.....energy unit cost to use for each simulation time-step
      real fSumFuelUse( iNumFuel )      
C.....Flag indicating whether to include fuel cost calculations
      logical bIncFuelCostCalcs
C.....Index to store the current month
      integer iCurrentMonth
C.....Flag indicating whether to apply the minimum monthly fuel charge
      logical bApplyMonthlyMinCharge( iNumFuel )     
C.....Flag indicating whether time of use (TOU) electricity rates have been specified
      logical bTOUelecRates
C.....Column in BC data file corresponding to TOU electricity rates schedule
      integer iTOUcolumn
      
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
C     PROPANE:        39.065  L/GJ (?!?) Wikipedia says 46 MJ/kg which at 0.51 kg/L results in 42 L/GJ
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
      data fFuelConversionFactor(iHardWood) / 17.738E-09 /

C.....Softwood: Convert W->Tonne
      data fFuelConversionFactor(iSoftWood) / 10.753E-09 /

C.....Mixed Wood: Convert W->Tonne
      data fFuelConversionFactor(iMixedWood) / 13.956E-09 /

C.....Wood Pellets 
      data fFuelConversionFactor(iWoodPellets) / 19.0800E-09 /

