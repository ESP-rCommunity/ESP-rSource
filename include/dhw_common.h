c Unless otherwise stated all units are as follows
c Temperatures are in Celsius
c Energy are in Joules
c Volumes are in Litres

c Global DHW Variables

      COMMON /DHW_DATA/ iMonth,iNTSTEP,
     &  iNumberOfTanks,fAmpGroundTempVar,fMeanGroundTemp,
     &  fNumOfOccupants,fWaterDraw,fCold_Main_Temp,
     &  fHotSupplyTemp,iDHW_Flag,fWaterDrawCurveHourly,  
     &  fWaterDrawCurveTotal,sDHW_InputFileName

c Common block modified to remove fAmpGroundTempVar and fMeanGroundTemp.
c These variables are now in the common block:
C COMMON/Ground_Temp/Temp_Ground_Avg,Temp_Ground_Amp,Temp_Ground_phaseshift
     
C iDay    Contains the day of the year number.
C iMonth  Contains the Month Number.
C iNTSTEP Number of Timestep in one hour.
C iNumberOfTanks Number Tanks in simulation.
       INTEGER iDay,iMonth,iNTSTEP,iNumberOfTanks

C fNumOfOccupants   Number of Occupants.
C fWaterDraw        Water Draw for current time step
C fCold_Main_Temp(12) Cold Main temp array (Monthly).
C fHotSupplyTemp    Hot water supply (set) temperature.
       REAL    fAmpGroundTempVar,fMeanGroundTemp,fNumOfOccupants
c       REAL    fNumOfOccupants
       REAL    fWaterDraw,fCold_Main_Temp(12),fHotSupplyTemp

C iDHW_Flag     Flag to indicate that dhw model is active.
       INTEGER iDHW_Flag
c Inserting hourly demand curve. May change this to an input file in the future.
       REAL fWaterDrawCurveHourly(24)
c This is the total water cumsumed in one day at to the data in fWaterDrawCurveHourly. 
c This is used to normalize the demand. ie demand for hour(1) = 9/239.0*Actual daily load
       REAL fWaterDrawCurveTotal
       CHARACTER*72 sDHW_InputFileName

c end remove later      
      
       COMMON/DHW_TANK_DATA/
     &  fRoomTemp,  iFuelType,iTankType,iDHWZoneLocation,
     &  fDOEEF,fHeatInjectorPower,fPilotEnergyRate,
     &  fTankSize,fTemperatureBand,fBlanketRSI,
c Calulated Values
     &  fOldAverageWaterTemp,fOldFinalWaterTemp,
     &  fOldStandbyTankLosses,fEnergyRequired,
     &  fEnergyInputWater,fAlpha,
c Hot 2000 model
     &  fEFTank,PercentOfDraw,fFGas,fFTank,fUA,
c Monthly values Arrays 
     &  fSumTotalEnergy,fSumTotalEnergyToSpace,
     &  fSumStandbyTankLosses,fSumEnergyInputWater,
     &  fSumFlueLosses,fSumPilotEnergyToRoom,
     &  fSumWaterLoad,fSumPilotEnergy,fSumTotalPipeGains,
     &  fSumTotalPipeLosses
       
        



c ESP-r Inputs
       REAL    fRoomTemp               !Temperature of Room which tank resides
        
c Tank Inputs
       INTEGER iFuelType               !Fuel Type (See fuel type parameters beow)
       INTEGER iTankType               !Tank COnfig type (See tank type paraters below)
       INTEGER iDHWZoneLocation        !Zone Number in which tank resides
       REAL    fDOEEF                  !The EF factor input by users.
       REAL    fHeatInjectorPower      !Power of Burner in Watts
       REAL    fPilotEnergyRate        !Power of Pilot in Watts
       REAL    fTankSize               !Volume of water tank can hold in Litres
       REAL    fTemperatureBand        !Temperature band of tank C
       REAL    fBlanketRSI             !Blanket RSI Value..zero if none.


c Calulated Values
       REAL    fOldAverageWaterTemp        !Average temperature fromt the previous timestep (Used for standbytanklosses)
       REAL    fOldFinalWaterTemp          !Final water temperature
       REAL    fOldStandbyTankLosses       !StandbyTankLosses from previous timestep.
       REAL    fEnergyRequired             !EnergyRequired to deal with water draw and standby losses from previous timestep.
       REAL    fEnergyInputWater           !Energy burner transferred to water during timestep.
       REAL    fAlpha                      !This is a portion of the inefficiency that goes to flue loss (obtained from H2KDHW.F.
                                           !Hot 2000 model
       REAL    fEFTank
       REAL    PercentOfDraw               !Percent of total water draw which tank satisfies.
       REAL    fFGas                       !FlueGas Loss Factor
       REAL    fFTank                      !Energy Factor of Tank
       REAL    fUA                         !UA value of Tank
        
c Monthly values Arrays 
        REAL    fSumTotalEnergy(12)             !Fuel energy required
        REAL    fSumTotalEnergyToSpace(12)      !Net energy sent to Zone.
        REAL    fSumStandbyTankLosses(12)       !Standby Tank Losses
        REAL    fSumEnergyInputWater(12)        !EnergyInputWater
        REAL    fSumFlueLosses(12)              !Combustion inefficiency flue losses
        REAL    fSumPilotEnergyToRoom(12)       !Pilot enegy imparted to Zone
        REAL    fSumWaterLoad(12)               !Water Load served by this tank
        REAL    fSumPilotEnergy(12)             !Pilot Energy Used
        REAL    fSumTotalPipeGains(12)          !PipeLosses for the Month.
        REAL    fSumTotalPipeLosses(12)         !PipeGains for the Month.
        
        
c Function Declarations
        REAL  fDHW_FlueGasLossFactor
        REAL  fDHW_DepthAmplitudeMod
        REAL  fDHW_LimitMeanGroundTemp
        REAL  fDHW_ColdMainTemp
        INTEGER  iDHW_GroundOffset
        REAL  fDHW_WaterDraw
        REAL  fDHW_StartTemp
        REAL  fDHW_WaterTempFromEnergy
        REAL  fDHW_WaterVolumeFromTempEnergy
        REAL  fDHW_WaterEnergyFromTemp
        REAL  fDHW_PilotEnergy
        REAL  fDHW_PilotEnergyToRoom
        REAL  fDHW_FuelEnergyRequired
        REAL  fDHW_EnergyToZone
        REAL  fDHW_PipeGains
        REAL  fDHW_PipeLosses
        REAL  fDHW_DOEEFTank
        REAL  fDHW_StandbyTankLosses
        REAL  fDHW_UA
        REAL  fDHW_EnergyRequired
        REAL  fDHW_EnergyInputWater
        REAL  fDHW_FinalWaterTemp
        REAL  fDHW_AverageWaterTemp
        REAL  fDHW_FlueLosses

