! ********************************************************************
! Module: h3kmodule
! Purpose: h3k module is the main interface for all calls to interact
!           with the C++ code for generating xml, csv, sql output.
!           For information on how to use the H3KReports see the
!           cetc/h3kreports/ConfigureH3kReports.txt document.
!
! Error handling: to be determined
! ********************************************************************
MODULE h3kmodule
   IMPLICIT NONE

   !Private subroutines
   private :: AddVariable, AddToReportWild, AddToReportWild1,  &
              AddToReportWild2, AddToReportWild3, AddToReportDetailsWild, &
              AddToReportDetailsWild1, AddToReportDetailsWild2, &
              AddToReportDetailsWild3


   !Public subroutines and functions
   public :: ReportNextTimeStep, ReportNewSeason, SetReportParameter, &
             isH3KEnabled, UpdateH3kSimInfo, UpdateH3kReport, &
             GenerateOutput, UpdateConfigFile, SetReportConfig, &
             GetReportConfig, ReportToggleConfig, GetReportList, &
             isReportingInstalled, SetReportEnable, IsH3kVarEnabled, &
             SetAdditionalValues


   !Function overloading interface to push a value to the TReportsManager
   INTERFACE AddToReport
      MODULE PROCEDURE AddToReportWild, AddToReportWild1,AddToReportWild2, AddToReportWild3
   END INTERFACE

   !Function overloading interface to overwrite a variable description and meta info during runtime
   INTERFACE AddToReportDetails
      MODULE PROCEDURE AddToReportDetailsWild, AddToReportDetailsWild1, AddToReportDetailsWild2, AddToReportDetailsWild3
   END INTERFACE

   !Declare interface of functions returning a value from c++, used internally to the h3kmodule.f90 this
   !is not intended to be called anywhere else in the esp-r code.
   INTERFACE
      logical function rep_xmlstatus()
      end function rep_xmlstatus

      logical function rep_report_list(cType,cValue)
         character(len=*), intent(in)::cType, cValue
      end function rep_report_list

      logical function bH3K_rep_enabled()
      end function bH3K_rep_enabled

      logical function rep_toggle_config(cVariable)
         character(len=*), intent(in)::cVariable
      end function rep_toggle_config

      character(50) function rep_report_config(cDescription)
         character(len=*), intent(in)::cDescription
      end function rep_report_config

      logical function is_variable_enabled(cPattern)
         character(len=*), intent(in)::cPattern
      end function is_variable_enabled
   END INTERFACE


   !ReportVariable construct,
   !note that the C++ code will manipulate them for two reasons:
   !  1. autogenerate the id (sequencial)
   !  2. NUL terminate all the strings
   !  3. populate Enabled from the input.xml file
   Type ReportVariable
      integer Identifier !overwriten on the C++ side
      character(256)	:: VariableName !NUL terminated in C++
      character(16)  :: MetaType !NUL terminated in C++
      character(16)  :: VariableType !NUL terminated in C++
      character(512) :: Description !NUL terminated in C++
      logical*1      :: Enabled !Variable requested or not, populated by c++
   End Type ReportVariable

   SAVE
   !Used by h3kstore.F
   Type(ReportVariable) :: rvPlantPumpElec

   !Used by h3k_report_data.F
   Type(ReportVariable) :: rvHeatFluxRadiationShortwave, rvHeatFluxRadiationShortwaveUnitArea, &
         rvHeatFluxAboveGradeNet, rvHeatFluxSpecifiedBCsNet, rvHeatFluxBelowGradeNet, &
         rvHeatFluxCENPartitionNet, rvAirPointTemperature, rvWindowsPosition, rvAirFlowModel, &
         rvAirPointRelativeHumidity, rvSuppliedEnergyNet, rvSuppliedEnergyHeating, &
         rvSuppliedEnergyCooling, rvSuppliedEnergyNetPerm2, rvSuppliedEnergyHeatingPerm2, &
         rvSuppliedEnergyCoolingPerm2, rvThermalLoadsHeatingTotal, rvThermalLoadsCoolingTotal, &
         rvThermalLoadsNetLoad, rvThermalLoadsHeatingTotalPerm2, rvThermalLoadsCoolingTotalPerm2, &
         rvThermalLoadsNetLoadPerm2, rvEnvWindowsHeatLoss, rvEnvWallsHeatLoss, &
         rvEnvFloorsHeatLoss, rvEnvCeilingsHeatLoss, rvEnvFoundationHeatLoss, &
         rvEnvInfAirChangePerHour, rvEnvInfHeatGain, &
         rvEnvInfHeatLoss, rvEnvInfLoad, rvEnvInfNetFlux, &
         rvEnvInfBalanceAir, rvEnvInfBalanceAirExchange, &
         rvEnvAllComponentsHeatLost, rvEnvWindowsHeatGain, rvEnvWallsHeatGain, &
         rvEnvFloorsHeatGain, rvEnvCeilingsHeatGain, rvEnvFoundationHeatGain, &
         rvEnvAllComponentsHeatGain, rvEnvWindowsNetFlux, rvEnvWallsNetFlux, &
         rvEnvFloorsNetFlux, rvEnvCeilingsNetFlux, rvEnvFoundationNetFlux, &
         rvEnvAllComponentsNetFlux, rvInsolationTotal, rvInsolationUseful, &
         rvInsolationAdverse, rvInternalGainsTotal, rvInternalGainsUseful, &
         rvInternalGainsAdverse, rvBuildingAllZonesSuppliedEnergyHeating, &
         rvBuildingAllZonesSuppliedEnergyCooling, rvBuildingAllZonesSuppliedEnergyNetFlux, &
         rvFreeCoolingDelivered , rvNodeTemp, rvBldZnLightPow, rvBldZnEquipPow, rvFreeCoolCtlFlag
   Type(ReportVariable) :: rvBuildingAllZonesInsolationTotal, rvBuildingAllZonesInsolationUseful, &
         rvBuildingAllZonesInsolationAdverse, rvBuildingAllZonesEnvelopeWindowsHeatLoss, &
         rvBuildingAllZonesEnvelopeWallsHeatLoss, rvBuildingAllZonesEnvelopeFloorsHeatLoss, &
         rvBuildingAllZonesEnvelopeCeilingsHeatLoss, rvBuildingAllZonesEnvelopeFoundationHeatLoss, &
         rvBuildingAllZonesEnvelopeInfiltrationHeatLoss, rvBuildingAllZonesEnvelopeAllComponentsHeatLoss, &
         rvBuildingAllZonesEnvelopeWindowsHeatGain, rvBuildingAllZonesEnvelopeWallsHeatGain, &
         rvBuildingAllZonesEnvelopeFloorsHeatGain, rvBuildingAllZonesEnvelopeCeilingsHeatGain, &
         rvBuildingAllZonesEnvelopeFoundationHeatGain, rvBuildingAllZonesEnvelopeInfiltrationHeatGain, &
         rvBuildingAllZonesEnvelopeWindowsNetFlux, rvBuildingAllZonesEnvelopeWallsNetFlux, &
         rvBuildingAllZonesEnvelopeFloorNetFlux, rvBuildingAllZonesEnvelopeCeilingsNetFlux, &
         rvBuildingAllZonesEnvelopeFoundationNetFlux, rvBuildingAllZonesEnvelopeInfiltrationNetFlux, &
         rvBuildingAllZonesEnvelopeAllComponentsNetFlux, rvBuildingAllZonesThermalLoadsHeatingTotal, &
         rvBuildingAllZonesThermalLoadsCoolingTotal, rvBuildingAllZonesThermalLoadsNet, &
         rvBuildingAllZonesInternalGainsTotal, rvBuildingAllZonesInternalGainsUseful, &
         rvBuildingAllZonesInternalGainsAdverse, rvBuildingAllZonesEnergyBalanceNet, rvTemperature, &
         rvBldAllZonesLightPowTtl, rvBldAllZonesEquipPowTtl, &
         rvExtSurfTemperature, rvPlantContainmentFlux, rvHCi, rvHCe, rvPRT, rvHRi, rvAmbRT, &
         rvClimateSolarDiffuseHorizontalRadiation, rvClimateSolarDirectNormalRadiation, &
         rvClimateDryBulbTemperature, rvClimateRelativeHumidity, rvClimateWindVelocity, &
         rvClimateWindDirection, rvClimateCloudCover, rvClimateSkyTemperature, &
         rvClimateSkyTemperatureDepression, rvClimateAmbientAirTsat, &
         rvBuildingAllZonesFreeCooling
   Type(ReportVariable) :: rvBuildingTimePresent, rvBuildingTimeFuture,rvBuildingHourPresent, &
         rvBuildingHourFuture,rvBuildingDayNumberPresent, rvBuildingDayNumberFuture,&
         rvBuildingYearPresent,rvBuildingYearFuture, rvBuildingDayPresent,rvBuildingMonth, &
         rvBuildingDayFuture, rvBuildingTimeStep, rvBldSeason
   Type(ReportVariable) :: rvPlantCompNodeTemperature, rvPlantCompNodeFirstPhaseFlow, &
         rvPlantCompNodeSecondPhaseFlow,rvPlantCompNodeHydrogenFlow,rvPlantCompNodeConnectTemperature, &
         rvPlantCompNodeConnectWaterFlow,rvPlantCompNodeConnectHydrogenFlow, &
         rvPlantCompNodeConnectMoistureFlow, rvPlantCompNodeConnectAirFlow
   Type(ReportVariable) :: rvElecNetLoadsTotalLoad,rvElecNetLoadsHvacLoad, &
         rvElecNetLoadsOccupantLoad, rvElecNetGenTotalGeneration, &
         rvElecNetLoadsExternalLoad, rvElecNetGenOnsiteGeneration, &
         rvElecNetGenOffsiteGeneration, rvElecNetGenOffsiteCoincident,rvElecNetGridImport, &
         rvElecNetGridExport,rvElecNetGridNetBalance, rvElecNetNodesVoltageMagnitude, &
         rvElecNetNodesVoltageAngle, rvElecNetNodesLoadReal, rvElecNetNodesLoadReactive, &
         rvElecNetNodesGenerationReal, rvElecNetNodesGenerationReactive, &
         rvElecNetNodesTransmissionReal, rvElecNetNodesTransmissionReative, &
         rvElecNetHybridComponentFlux, rvElecNetPowerOnlyComponents, &
         rvMfnTotalNodeFlowRate,rvMfnTotalNodeVolFlowRate,rvMfnTotalNodeTemp, &
         rvMfnConnectPressureDrop, rvMfnConnectFlowRate,rvMfnConnectVeloc, &
         rvMfnContamCon, rvZoneLabel

   !Used by SiteUtilities.F
   Type(ReportVariable) :: rvTFuelAllEndEnergyContent, rvTFuelAllEndQty, &
         rvTFuelAllEndQtyElectricity,rvTFuelAllEndQtyNaturalGas, &
         rvTFuelAllEndQtyOil, rvTFuelAllEndQtyPropane,rvTFuelAllEndQtyMixedWood, &
         rvTFuelAllEndQtyHardWood, rvTFuelAllEndQtySoftWood, rvTFuelAllEndQtyWoodPellet, &
         rvTFuelAllEndEnergyContElec,rvTFuelAllEndEnergyContNatGas,rvTFuelAllEndEnergyContOil, &
         rvTFuelAllEndEnergyContPropane,rvTFuelAllEndEnergyContMixWood,rvTFuelAllEndEnergyContHardWood, &
         rvTFuelAllEndEnergyContSoftWood,rvTFuelAllEndEnergyContPellets, &
         rvTFuelQty,rvTFuelQtyElec,rvTFuelQtyNatGas,rvTFuelQtyOil,rvTFuelQtyProp, &
         rvTFuelQtyMixWood,rvTFuelQtyHardWood,rvTFuelQtySoftWood,rvTFuelQtyPellets, &
         rvTFuelCstAllEnd, rvTFuelCstAllEndPropane, rvTFuelCstAllEndOil, rvTFuelCstAllEndNaturalGas,  &
         rvTFuelCstAllEndElectricity, rvTFuelCstAllEndMixedWood, rvTFuelCstAllEndHardWood, &
         rvTFuelCstAllEndSoftWood, rvTFuelCstAllEndWoodPellet, &
         rvTFuelCstMixWood, rvTFuelCstHardWood, rvTFuelCstSoftWood, rvTFuelCstPellets, &
         rvTFuelCst, rvTFuelCstElec, rvTFuelCstNatGas, rvTFuelCstOil, rvTFuelCstProp, rvTEnergyQty

   !Used by Solar.F
   Type(ReportVariable) :: rvBuildingGroundReflectivity,rvClimateSnownDepth

   !Used by complex_fenestration.F
   Type(ReportVariable) :: rvCFCazimuth,rvCFCelevation,rvSolarIncidentDirect,rvSolarIncidentDiff, &
         rvCFCtranDir, rvCFCtranDiff,rvCFCvertprofileangle

   !Used by water_tanks.F
   Type(ReportVariable) :: rvPltSDHWSumDHWTankFuel,rvPltSDHWSumDHWTankElec,rvPltWaterTemp, &
         rvPltConn1HeatInject, rvPltConn2HeatInject, rvPltDomHotWtrVol,rvPltDomHotWtrMkTemp, &
         rvPltDomHotWtrThermal,rvPltBurnOut, rvPltFuelFlow,rvPltFuelFlowVolSTP,&
         rvPltFuelFlowLHVVal,rvPltAirFlow,rvPltExhaustFlow,rvPltEInput,&
         rvPltHTransWater,rvPltJacketLoss,rvPltHeatDump

   !Used by the tank_intak_hx.F
   Type(ReportVariable) :: rvPltTHeatTrans,rvPltTHeatTransJacketHeatLoss, &
         rvPltTHeatTransTubeSideHi,rvPltTHeatTransTankSideHo,rvPltTHeatTransIntankCoilTUValue

   !used by stratified_tank.F;stratified_tank_2HX.F;stratified_tank_1HX.F
   Type(ReportVariable) :: rvPltAvgTemp

   !Used by solar_collectors.F
   Type(ReportVariable) :: rvPltSDHWSumRecH,rvPltSDHWsumAvailSolEn

   !Used by pcomp2.F
   !Claude - potential error found rvPltDefrostStat
   Type(ReportVariable) :: rvPltQAddedH,rvPltWCHPumpEInput,rvPltHOut,rvPltCOP, &
         rvPltTambient,rvPltDeviceONOFF,rvPltReturnTSP,rvPltRealPow, &
         rvPltReacPow,rvPltApparPow,rvPltDefrostStat,rvPltDHWDrawStoch, &
         rvPltDHWDrawStochTp

   !Used by NCHE-steady_state.F
   Type(ReportVariable) :: rvPltHXchgEff,rvPltHXchgHTrans, &
         rvPltHXchgHFlowRate,rvPltHXchgCFlowRate,rvPltHXchgHTempIn, &
         rvPltHXchgHTempOut,rvPltHXchgCTempIn,rvPltHEXchgCTempOut

   !Used by h3k_plant_postprocess
   !Claude: definitions required
   Type(ReportVariable) :: rvPltHeatTransAir, rvPltAirHumRatio ,rvPltHTransWater2

   !Used by FC_components.F
   !Claude: Discrepency here, multiple meta types
   Type(ReportVariable) :: rvPltElecEffBOP,rvPltElecEffFC,rvPltFuelHHV, &
         rvPltQThermalNet,rvPltQElecParasitic,rvPltQElecNet,rvPltQElecDemand, &
         rvPltCogEff,rvPltFuelFlowMass,rvPltFuelFlowVol,rvPltAirFlowMass, &
         rvPltAirFlowVol,rvPltAirFlowStoi,rvPltWaterFlowVol, &
         rvPltExhaustFlowMass,rvPltExhaustFlowVol,rvPltExhaustFlowTemp,&
         rvPltHExchgExitTemp,rvPltHExchgUAVal

   !Used by mains_temp_draw_profiles.F
   Type(ReportVariable) :: rvPltDHWTermalLoad

   !Used by the Hydrogen_PEMFC.F
   Type(ReportVariable) :: rvDebugH2LHV,rvPltAvrEffCogen,rvPltAvrEffElec , &
         rvPltAvrEffThermal,rvPltHydroSupMassFlowRate, rvPltHydroSupLHVFlowRate, &
         rvPltHydroDemMassFlowRate,rvPltHydroDemLHVFlowRate,rvPltStackVolt, &
         rvPltStackCurrent,rvPltAvgPowAC,rvPltAvrPowDC,rvPltAvgHOutput, &
         rvPltAvgSkinLoss,rvPltCtrlScheme,rvPltCtrlSignal,rvPltOperModeStandBy, &
         rvPltOperModeStartup,rvPltOperModeNormOp,rvPltOperModeCoolDown

   !Used by Hydrogen_MH_store.F
   Type(ReportVariable) :: rvPltHydroFreeKMol,rvPltHydroFreeKg, &
         rvPltHydroAbsorbedKmol,rvPltHydroAbsorbedKg,rvPltHydroVentKmol, &
         rvPltHydroChargeKmol,rvPltHydroDisKmol,rvPltCylPressure,rvPltDemandExpl,&
         rvPltDemandImpl,rvPltHRecovery,rvPltHLoss,rvPltHReaction, &
         rvPltContainTemp,rvPltH2ConnFlowWork,rvPltH2DiscFlowWork

   !Used by Hydrogen_appliances.F
   Type(ReportVariable) :: rvPltH2AppLoad,rvPltH2AppDemand

   !Used by compressed_cylinder.F
   Type(ReportVariable) :: rvPltMassPresent,rvPltMassFuture,rvPltHydroChargeRt, &
         rvPltHydroDischgRt,rvPltVentH2Rt,rvPltPressPres,rvPltPressFut, &
         rvPltHydroDefIMP,rvPltHydroDefEXP,rvDebugGasCylm3,rvDebugGasCylkg, &
         rvDebugGasCylJKgoC,rvDebugGasCylkPa,rvDebugGasCylkjkgK,rvDebugGasCyloC, &
         rvDebugGasCylkgs,rvDebugGasCylw,rvDebugGasCyljkgk, rvDebugGasCylWoC

   !Used by Annex42_fuel_cell.F
   Type(ReportVariable) :: rvPltAPel,rvPltEtaEl,rvPltNdotFCPMFuel,rvPltNdotFCPMAir, &
         rvPltLambdaFCPM,rvPltPelAncAc,rvPltNdotAuxBurnFuel,rvPltNdotAuxBurnAir, &
         rvPltQAuxBurn,rvPltAuxBurnPel,rvPltAuxBurnSkinLoss,rvPltQHX,rvPltQHSSens, &
         rvPltQHXLatent,rvPltCWater,rvPltCGas,rvPltUAHX,rvPltHXNdotCondense, &
         rvPltHXWaterVapFrac,rvPltHXHfg,rvPltNdoHXGax,rvPltLHVFuel, &
         rvPltDeltaHFCPMFuel,rvPltDeltaHFCPMAir,rvPltNdotFCPMexhN2,rvPltNdotFCPMexhAr, &
         rvPltNdotFCPMexh02,rvPltNdotFCPMexhC02,rvPltNdotFCPMexhH20, &
         rvPltDeltaHFCPMexh,rvPltBlowerPel, rvPltTBlowerIn,rvPltCompPel,rvPltPumpPel, &
         rvPltNdotFCPMLiqWater,rvPltDeltaHFCPMLiqWater,rvPltBattSOCP,rvPltBattSOCf, &
         rvPltDCReqPCU,rvPltDCInPCU,rvPltPBatt,rvPltACDemNet,rvPltACProdGross, &
         rvPltACProdNet,rvPltPCULosses,rvPltEtaPCU,rvPltQSCool,rvPltQSCogen, &
         rvPltQSCoolAir,rvPltPAirEl,rvPltUASCogen,rvPltQHRV,rvPltTVentAir, &
         rvPltDilutionHeatToAir,rvPltLHVToACEff,rvPltLHVToThermEff,rvPltLHVToCogenEff, &
         rvPltHeatToRoomConv,rvPltHeatToRoomRad

   !Used by Annex42_combustion_CHP.F
   Type(ReportVariable) :: rvPltExtCtrlScheme,rvPltExtCtrlSignal,rvPltOperModeInop, &
         rvPltA42OperModeStartUp,rvPltOperModeWarmup,rvPltOpenModeNormOper, &
         rvPltOperModeShutdown,rvPltContTemp,rvPltCoolWaterTemp,rvPltCoolWaterFlowRt, &
         rvPltInstPowerNet,rvPltInstHeatRec,rvPltInstFlowRt,rvPltInstFuelFlowRt, &
         rvPltInstFuelGrossEnInput,rvPltInstFuelLowHeatVal,rvPltInstHeatLoss, &
         rvPltAvgHeatRec,rvPltAvgPowNet,rvPltAvgFuelFlowRt,rvPltAvgFuelGrossEnIn, &
         rvPltAvgEmisCarbDio,rvPltAvgEffElec,rvPltAvgEffTherm,rvPltAvgEffCogen, &
         rvPltInstEffCogen, rvPltInstEffElec,rvPltInstEffTherm,rvPltSolWeightFact, &
         rvPltSolTmeCstEngCtrlVol,rvPltSolTmeCstCWCtrlVol

   !Used by ADS_storege_unit.F
   Type(ReportVariable) :: rvPltMaxAdsTemp,rvPltMinAdsTemp,rvPltAbsorbPress, &
         rvPltAdsVapFlow,rvPltCondFanPow,rvPltAdsorpHeat,rvPltAdsHeatTransCH, &
         rvPltAdsHeatTransDis,rvPltAdsAmbHeatLoss,rvPltCondAmbHeatLoss, &
         rvPltEvapAmbHeatLoss,rvPltTempCondIn,rvPltCondHeat,rvPltEvapHeat,rvPltRatioX, &
         rvPltCyclePhase

   !Used by ADS_storage_tanks.F
   Type(ReportVariable) :: rvPltPowDraw,rvPltBurnerOut,rvPltBurnerWater,rvPltFuelConsKGS, &
         rvPltFuelConsSLPM,rvPltAirFlowKGS,rvPltAirFlowSLPM,rvPltTempComb,rvPltTempExh, &
         rvPltHeatDumpFuture,rvPltDHWDraw,rvPltDHWMakeupTemp,rvPltHeatingOut

   !Used by wind_turbine_profile_model.F
   Type(ReportVariable) :: rvEPowWindGenOut,rvEPowWindSpeed

   !Used by powoc_calc.F
   Type(ReportVariable) :: rvEPowLoadProf,rvEPowH2Load,rvEPowLoadAdj, &
         rvEPowPCUPowLoss,rvEPowPCUEff,rvEPowPCUPowIn, rvEPowPCUPowOut

   !Used by CETC_BATTERY.F
   Type(ReportVariable) :: rvEPowBattDem,rvEPowBattOper,rvEPowBattVolt,rvEPowBattDOD, &
         rvEPowBattCurr,rvEPowbattIntH,rvEPowBattSOC,rvEPowBattLd, &
         rvEPowBattMaxPow,rvEPowBattMaxPowChrg,rvEPowBattMaxDschrg,rvEPowBattMaxHPow, &
         rvEPowBattHPow,rvEPowBattPowBal,rvEPowBattSysLd,rvEPowBattSysChrgLd, &
         rvEPowBattSysDschrgLd,rvEPowBattTemp,rvEPowBattLfUsed,rvEPowBattLfUsedCum, &
         rvEPowBattLfUseFac,rvEPowBattChrgCyc,rvEPowBattManChrgPh,rvEPowBattManChrgPhInc, &
         rvEPowBattTmeLstFulChrg,rvEPowBattBdTreatFlg,rvEPowBattCtrlScn,rvEPowBattLiOnCycUsed

   !Used by RE-H2-ctl.F
   Type(ReportVariable) :: rvCtrlReH2NPwElAct,rvCtrlReH2NPwBattChrg,rvCtrlReH2NPwBattDschrg, &
         rvCtrlReH2NPwPEMFCAct

   !Used by hvacsim.F
   Type(ReportVariable) :: rvPltHvacComFuelAmnt,rvPltHvacFuelEnPilot,rvPltHvacEff, &
         rvPltHvacCOPHeat,rvPltHvacCOPCool,rvPltHvacFuelEnHeat,rvPltHvacPrtLdHeat, &
         rvPltHvacThrmOutHeat,rvPltHvacFuelEnCool,rvPltHvacPrtCool,rvPltHvacThrmCool, &
         rvPltHvacFuelEnTtl,rvPltHvacCoilCoolSns,rvPltHvacCoilCoolLtnt, &
         rvPltHvacCoilCoolTtl,rvPltHvacCrcFuelEnIn,rvPltHvacCrcFElecAmnt, &
         rvPltHvacComFuelAmntHeat, rvPltHvacComFuelAmntCool, rvPltHvacComFlAmntAux, &         
         rvPltHvacCrcHeatTrn

   !Used by ashp_cooling.F
   Type(ReportVariable) :: rvPltHvacZnCapFrac,rvPltHvacZnRtCapFrac, &
         rvPltHvacCrcFlwRt,rvPltHvacOutFanPw

   !Used by trnsys_wrapper.F
   Type(ReportVariable) :: rvPltMiscData

   !Used by CentralVentSys.F
   Type(ReportVariable) :: rvPltHvacHRVElecLd

   !Used by DHW_module.F
   Type(ReportVariable) :: rvPltDHWWtr,rvPltDHWEnIn,rvPltDHWSupTmp,rvPltDHWDlvTmp, &
         rvPltDHWBrnHOut,rvPltDHWSknLss,rvPltDHWFluLss,rvPltDHWHTrnBld,rvPltDHWWtrHLd

   !Used by h2_comps.F
   Type(ReportVariable) :: rvPltOprtSgn,rvPltStndSgn,rvPltCtrlSgnEltrzr,rvPltTtlAuxPow,rvPltDCAuxPow, &
         rvPltACAuxPow,rvPltStckPow,rvPltMxStckPow,rvPltOpMode,rvPltDCPowDmd, &
         rvPltACPowDmd,rvPltElecEffEltrzr,rvPltH2FlwKgs,rvPlt02FlwKgs,rvPltH20FlwKgs, &
         rvPltQIntPrst,rvPltQlssPrst,rvPltQCoolPrst,rvPltQInltWtrHtg,rvPltTnltWtr

   !Used by h2_matrix_library.F
   Type(ReportVariable) :: rvPltElecWrk,rvPltMchWrk,rvPltHFlow,rvPltHydgnIn, &
         rvPltHydgnOut,rvPltHydgnLss,rvPltHydgnStrPrs,rvPltHydgnStrFut, &
         rvPltElecFlx,rvPltUndlvFlxSysSz,rvZZZ,rvZZZZ,rvPltHFlx,rvPltCtrlSgn, &
         rvPltFlxCnstrnSz,rvPltElecEff,rvPltEnLss,rvPltLdRt,rvPltH2Dmd, &
         rvPltAuxFuelFlw,rvPltUndlvFlxH2Shrt,rvPltThrmEff,rvPltOxgnFlw,rvPltWtrFlw

   !Used by h3kstore.F
   Type(ReportVariable) :: rvPltSDHWSumPmpElec

   !Used by basesimp.F
   Type(ReportVariable) :: rvBsImpQag,rvBsImpQbgAvg,rvBsImpQbgVar,rvBsImpQTtl, &
         rvBsImpTBSAvg,rvBsImpTamb,rvBsImpBsTGAvg

   !Used by spmatl.F
   Type(ReportVariable) :: rvBldSPMatlEffIrr,rvBldSPMatlIncAngl,rvBldSPMatlTrnsAngl, &
         rvBldSPMatlTrns0,rvBldSPMatlTrns60,rvBldSPMatlTtlIncAr,rvBldSPMatlTtlIncTtl, &
         rvBldSPMatlDrtIncAr,rvBldSPMatlDffIncAr,rvBldSPMatlPVPow,rvBuiSpm

   !Used by BC_data.F
   Type(ReportVariable) :: rvBndCndStpInt,rvBndCndLnInt

   !Used by PEM_FC_pt1.F
   Type(ReportVariable) :: rvpltFCellQElecNet,rvpltFCellQElecPrstc,rvpltFCellQthrmNet, &
         rvpltFCellFuelHHV,rvpltFCellElecEffBOP,rvpltFCellCogenEff,rvpltFCellFuelFlw

   Type(ReportVariable) :: rvBldInfAirInf,rvBldInfAirChg
   
   !Used by TCC.F
   Type(ReportVariable) :: rvpltCosimInvocations, rvpltCosimEsprIter,rvpltCosimTrnsysIter,rvpltHCCTempToTrnsys, &
      rvpltHCCFlowToTrnsys, rvpltACCTempToTrnsys, rvpltACCFlowToTrnsys, rvpltACCMoistFlowToTrnsys, &
      rvpltHCCTempToEspr, rvpltHCCFlowToEspr, rvpltACCTempToEspr, rvpltACCFlowToEspr, &
      rvpltACCMoistFlowToEspr, rvpltCosimAirPointTemperatures, rvpltCosimAirPointHumidities, &
      rvpltCosimAirPointCasualGains

   !Used by complex_fenestration.F
   Type(ReportVariable) :: rvCFCShadeCtl, rvCFCSlatAngle
   
CONTAINS
   ! ********************************************************************
   ! Subroutine: UpdateH3kReport
   ! Purpose:  All declared Paramaters must be defined in this subroutine
   ! Call By:  This subroutine is called once during the simulation by
   !           bmatsv.F **only call once**
   ! Params:   N/A
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-06-27
   ! ********************************************************************
   Subroutine UpdateH3kReport()
      !Used by h3kstore.F
      rvPlantPumpElec%VariableName = 'plant/SDHW_summary/pump_elec'
      rvPlantPumpElec%MetaType = 'units'
      rvPlantPumpElec%VariableType = '(W)'
      rvPlantPumpElec%Description = 'SDHW system: Electric consumption of the all pumps'
      Call AddVariable(rvPlantPumpElec)

      !Used by h3k_report_data.F
      rvZoneLabel%VariableName = 'building/zone_*/zone_label/*'
      rvZoneLabel%MetaType = 'units'
      rvZoneLabel%VariableType = '(-)'
      rvZoneLabel%Description = 'Zone label'
      Call AddVariable(rvZoneLabel)
      
      rvHeatFluxRadiationShortwave%VariableName = 'building/zone_*/surface_*/heat_flux/radiation/shortwave'
      rvHeatFluxRadiationShortwave%MetaType = 'units'
      rvHeatFluxRadiationShortwave%VariableType = '(W)'
      rvHeatFluxRadiationShortwave%Description = 'Short-wave solar radiation'
      Call AddVariable(rvHeatFluxRadiationShortwave)

      rvHeatFluxRadiationShortwaveUnitArea%VariableName = 'building/zone_*/surface_*/heat_flux/radiation/shortwave/unit_area'
      rvHeatFluxRadiationShortwaveUnitArea%MetaType = 'units'
      rvHeatFluxRadiationShortwaveUnitArea%VariableType = '(W/m2)'
      rvHeatFluxRadiationShortwaveUnitArea%Description = 'Short-wave solar radiation per unit area'
      Call AddVariable(rvHeatFluxRadiationShortwaveUnitArea)

      rvHeatFluxAboveGradeNet%VariableName = 'building/zone_*/surface_*/heat_flux/above_grade/net'
      rvHeatFluxAboveGradeNet%MetaType = 'units'
      rvHeatFluxAboveGradeNet%VariableType = '(W)'
      rvHeatFluxAboveGradeNet%Description = 'Heat transfer through building envelope (above-grade)'
      Call AddVariable(rvHeatFluxAboveGradeNet)

      rvHeatFluxSpecifiedBCsNet%VariableName = 'building/zone_*/surface_*/heat_flux/to_specified_BCs/net'
      rvHeatFluxSpecifiedBCsNet%MetaType = 'units'
      rvHeatFluxSpecifiedBCsNet%VariableType = '(W)'
      rvHeatFluxSpecifiedBCsNet%Description = 'Heat loss to specified boundary conditions'
      Call AddVariable(rvHeatFluxSpecifiedBCsNet)

      rvHeatFluxBelowGradeNet%VariableName = 'building/zone_*/surface_*/heat_flux/below_grade/net'
      rvHeatFluxBelowGradeNet%MetaType = 'units'
      rvHeatFluxBelowGradeNet%VariableType = '(W)'
      rvHeatFluxBelowGradeNet%Description = 'Heat transfer through building envelope (below-grade)'
      Call AddVariable(rvHeatFluxBelowGradeNet)

      rvHeatFluxCENPartitionNet%VariableName = 'building/zone_*/surface_*/heat_flux/CEN_partition/net'
      rvHeatFluxCENPartitionNet%MetaType = 'units'
      rvHeatFluxCENPartitionNet%VariableType = '(W)'
      rvHeatFluxCENPartitionNet%Description = 'Heat transfer through CEN 13917 partition'
      Call AddVariable(rvHeatFluxCENPartitionNet)

      rvAirPointTemperature%VariableName = 'building/zone_*/air_point/temperature'
      rvAirPointTemperature%MetaType = 'units'
      rvAirPointTemperature%VariableType = '(oC)'
      rvAirPointTemperature%Description = 'Zone air-point temperature'
      Call AddVariable(rvAirPointTemperature)

      rvWindowsPosition%VariableName = 'building/zone_*/windows/position'
      rvWindowsPosition%MetaType = 'units'
      rvWindowsPosition%VariableType = '(-)'
      rvWindowsPosition%Description = 'Window position as per BCL25 controller (0->1)'
      Call AddVariable(rvWindowsPosition)

      rvAirFlowModel%VariableName = 'building/zone_*/air_flow_model'
      rvAirFlowModel%MetaType = 'units'
      rvAirFlowModel%VariableType = '(-)'
      rvAirFlowModel%Description = 'Air-flow-model (0->3)'
      Call AddVariable(rvAirFlowModel)

      rvAirPointRelativeHumidity%VariableName = 'building/zone_*/air_point/relative_humidity'
      rvAirPointRelativeHumidity%MetaType = 'units'
      rvAirPointRelativeHumidity%VariableType = '(%)'
      rvAirPointRelativeHumidity%Description = 'Zone relative humidity'
      Call AddVariable(rvAirPointRelativeHumidity)

      rvSuppliedEnergyNet%VariableName = 'building/zone_*/supplied_energy/net'
      rvSuppliedEnergyNet%MetaType = 'units'
      rvSuppliedEnergyNet%VariableType = '(W)'
      rvSuppliedEnergyNet%Description = 'Zone net heat injection/extraction'
      Call AddVariable(rvSuppliedEnergyNet)

      rvSuppliedEnergyHeating%VariableName = 'building/zone_*/supplied_energy/heating'
      rvSuppliedEnergyHeating%MetaType = 'units'
      rvSuppliedEnergyHeating%VariableType = '(W)'
      rvSuppliedEnergyHeating%Description = 'Zone net heat injection'
      Call AddVariable(rvSuppliedEnergyHeating )

      rvSuppliedEnergyCooling%VariableName = 'building/zone_*/supplied_energy/cooling'
      rvSuppliedEnergyCooling%MetaType = 'units'
      rvSuppliedEnergyCooling%VariableType = '(W)'
      rvSuppliedEnergyCooling%Description = 'Zone net heat extraction'
      Call AddVariable(rvSuppliedEnergyCooling )

      rvFreeCoolingDelivered%VariableName = 'building/zone_*/BCL29_free_cooling'
      rvFreeCoolingDelivered%MetaType = 'units'
      rvFreeCoolingDelivered%VariableType = '(W)'
      rvFreeCoolingDelivered%Description = 'Free cooling delivered to zone through BCL 29'
      Call AddVariable(rvFreeCoolingDelivered )
      
      rvSuppliedEnergyNetPerm2%VariableName = 'building/zone_*/supplied_energy/net_Perm2'
      rvSuppliedEnergyNetPerm2%MetaType = 'units'
      rvSuppliedEnergyNetPerm2%VariableType = '(W/m2)'
      rvSuppliedEnergyNetPerm2%Description = 'Zone net heat injection/extraction'
      Call AddVariable(rvSuppliedEnergyNetPerm2 )

      rvSuppliedEnergyHeatingPerm2%VariableName = 'building/zone_*/supplied_energy/heating_Perm2'
      rvSuppliedEnergyHeatingPerm2%MetaType = 'units'
      rvSuppliedEnergyHeatingPerm2%VariableType = '(W/m2)'
      rvSuppliedEnergyHeatingPerm2%Description = 'Zone net heat injection'
      Call AddVariable(rvSuppliedEnergyHeatingPerm2 )

      rvSuppliedEnergyCoolingPerm2%VariableName = 'building/zone_*/supplied_energy/cooling_Perm2'
      rvSuppliedEnergyCoolingPerm2%MetaType = 'units'
      rvSuppliedEnergyCoolingPerm2%VariableType = '(W/m2)'
      rvSuppliedEnergyCoolingPerm2%Description = 'Zone net heat extraction'
      Call AddVariable(rvSuppliedEnergyCoolingPerm2)

      rvThermalLoadsHeatingTotal%VariableName = 'building/zone_*/thermal_loads/heating/total'
      rvThermalLoadsHeatingTotal%MetaType = 'units'
      rvThermalLoadsHeatingTotal%VariableType = '(W)'
      rvThermalLoadsHeatingTotal%Description = 'Total heating load ( heat loss ' // &
            'trough envelope + ventilation and infiltration loads )'
      Call AddVariable(rvThermalLoadsHeatingTotal)

      rvThermalLoadsCoolingTotal%VariableName = 'building/zone_*/thermal_loads/cooling/total'
      rvThermalLoadsCoolingTotal%MetaType = 'units'
      rvThermalLoadsCoolingTotal%VariableType = '(W)'
      rvThermalLoadsCoolingTotal%Description = 'Total cooling load ( heat gain ' // &
            'trough envelope + ventilation and infiltration loads )'
      Call AddVariable(rvThermalLoadsCoolingTotal)

      rvThermalLoadsNetLoad%VariableName = 'building/zone_*/thermal_loads/net_load'
      rvThermalLoadsNetLoad%MetaType = 'units'
      rvThermalLoadsNetLoad%VariableType = '(W)'
      rvThermalLoadsNetLoad%Description = 'Net load ( heat gain/loss trough ' // &
            'envelope + ventilation and infiltration loads )'
      Call AddVariable(rvThermalLoadsNetLoad)

      rvThermalLoadsHeatingTotalPerm2%VariableName = 'building/zone_*/thermal_loads/heating/total_Perm2'
      rvThermalLoadsHeatingTotalPerm2%MetaType = 'units'
      rvThermalLoadsHeatingTotalPerm2%VariableType = '(W/m2)'
      rvThermalLoadsHeatingTotalPerm2%Description = 'Total heating load ( heat ' // &
            'loss trough envelope + ventilation and infiltration loads )'
      Call AddVariable(rvThermalLoadsHeatingTotalPerm2)

      rvThermalLoadsCoolingTotalPerm2%VariableName = 'building/zone_*/thermal_loads/cooling/total_Perm2'
      rvThermalLoadsCoolingTotalPerm2%MetaType = 'units'
      rvThermalLoadsCoolingTotalPerm2%VariableType = '(W/m2)'
      rvThermalLoadsCoolingTotalPerm2%Description = 'Total cooling load ( heat gain ' // &
            'trough envelope + ventilation and infiltration loads )'
      Call AddVariable(rvThermalLoadsCoolingTotalPerm2 )

      rvThermalLoadsNetLoadPerm2%VariableName = 'building/zone_*/thermal_loads/net_load_Perm2'
      rvThermalLoadsNetLoadPerm2%MetaType = 'units'
      rvThermalLoadsNetLoadPerm2%VariableType = '(W/m2)'
      rvThermalLoadsNetLoadPerm2%Description = 'Net load ( heat gain/loss trough ' // &
            'envelope + ventilation and infiltration loads )'
      Call AddVariable(rvThermalLoadsNetLoadPerm2 )

      rvEnvWindowsHeatLoss%VariableName = 'building/zone_*/envelope/windows/heat_loss'
      rvEnvWindowsHeatLoss%MetaType = 'units'
      rvEnvWindowsHeatLoss%VariableType = '(W)'
      rvEnvWindowsHeatLoss%Description = 'Contribution of windows to heating load'
      Call AddVariable(rvEnvWindowsHeatLoss)

      rvEnvWallsHeatLoss%VariableName = 'building/zone_*/envelope/walls/heat_loss'
      rvEnvWallsHeatLoss%MetaType = 'units'
      rvEnvWallsHeatLoss%VariableType = '(W)'
      rvEnvWallsHeatLoss%Description = 'Contribution of walls to heating load'
      Call AddVariable(rvEnvWallsHeatLoss)

      rvEnvFloorsHeatLoss%VariableName = 'building/zone_*/envelope/floors/heat_loss'
      rvEnvFloorsHeatLoss%MetaType = 'units'
      rvEnvFloorsHeatLoss%VariableType = '(W)'
      rvEnvFloorsHeatLoss%Description = 'Contribution of overhanging floors to heating load'
      Call AddVariable(rvEnvFloorsHeatLoss)

      rvEnvCeilingsHeatLoss%VariableName = 'building/zone_*/envelope/ceilings/heat_loss'
      rvEnvCeilingsHeatLoss%MetaType = 'units'
      rvEnvCeilingsHeatLoss%VariableType = '(W)'
      rvEnvCeilingsHeatLoss%Description = 'Contribution of Ceilings to heating load'
      Call AddVariable(rvEnvCeilingsHeatLoss)

      rvEnvFoundationHeatLoss%VariableName = 'building/zone_*/envelope/foundation/heat_loss'
      rvEnvFoundationHeatLoss%MetaType = 'units'
      rvEnvFoundationHeatLoss%VariableType = '(W)'
      rvEnvFoundationHeatLoss%Description = 'Contribution of foundation to heating load'
      Call AddVariable(rvEnvFoundationHeatLoss)

      rvEnvInfAirChangePerHour%VariableName = 'building/zone_*/envelope/infiltration/air_changes_per_hour'
      rvEnvInfAirChangePerHour%MetaType = 'units'
      rvEnvInfAirChangePerHour%VariableType = '(ACH)'
      rvEnvInfAirChangePerHour%Description = 'Zone infiltration (air changes per hour)'
      Call AddVariable(rvEnvInfAirChangePerHour)

      rvEnvInfHeatGain%VariableName = 'building/zone_*/envelope/infiltration/heat_gain'
      rvEnvInfHeatGain%MetaType = 'units'
      rvEnvInfHeatGain%VariableType = '(W)'
      rvEnvInfHeatGain%Description = 'Contribution of infiltration to cooling load'
      Call AddVariable(rvEnvInfHeatGain)

      rvEnvInfHeatLoss%VariableName = 'building/zone_*/envelope/infiltration/heat_loss'
      rvEnvInfHeatLoss%MetaType = 'units'
      rvEnvInfHeatLoss%VariableType = '(W)'
      rvEnvInfHeatLoss%Description = 'Contribution of infiltration to heating load'
      Call AddVariable(rvEnvInfHeatLoss)

      rvEnvInfLoad%VariableName = 'building/zone_*/envelope/infiltration/load'
      rvEnvInfLoad%MetaType = 'units'
      rvEnvInfLoad%VariableType = '(W)'
      rvEnvInfLoad%Description = 'Contribution of infiltration to heating load'
      Call AddVariable(rvEnvInfLoad)

      rvEnvInfNetFlux%VariableName = 'building/zone_*/envelope/infiltration/net_flux'
      rvEnvInfNetFlux%MetaType = 'units'
      rvEnvInfNetFlux%VariableType = '(W)'
      rvEnvInfNetFlux%Description = 'Contribution of infiltration to net heat flux'
      Call AddVariable(rvEnvInfNetFlux)

      rvEnvInfBalanceAir%VariableName = 'building/zone_*/envelope/infiltration/balance/air'
      rvEnvInfBalanceAir%MetaType = 'units'
      rvEnvInfBalanceAir%VariableType = '(kg/s?)'
      rvEnvInfBalanceAir%Description = 'Air movement into zone'
      Call AddVariable(rvEnvInfBalanceAir)

      rvEnvInfBalanceAirExchange%VariableName = 'building/zone_*/envelope/infiltration/balance/air_exchange'
      rvEnvInfBalanceAirExchange%MetaType = 'units'
      rvEnvInfBalanceAirExchange%VariableType = '(ACH)'
      rvEnvInfBalanceAirExchange%Description = 'Air movement into zone'
      Call AddVariable(rvEnvInfBalanceAirExchange)

      rvEnvAllComponentsHeatLost%VariableName = 'building/zone_*/envelope/all_components/heat_loss'
      rvEnvAllComponentsHeatLost%MetaType = 'units'
      rvEnvAllComponentsHeatLost%VariableType = '(W)'
      rvEnvAllComponentsHeatLost%Description = 'Heat lost through all components of envelope'
      Call AddVariable(rvEnvAllComponentsHeatLost )

      rvEnvWindowsHeatGain%VariableName = 'building/zone_*/envelope/windows/heat_gain'
      rvEnvWindowsHeatGain%MetaType = 'units'
      rvEnvWindowsHeatGain%VariableType = '(W)'
      rvEnvWindowsHeatGain%Description = 'Contribution of windows to cooling load'
      Call AddVariable(rvEnvWindowsHeatGain)

      rvEnvWallsHeatGain%VariableName = 'building/zone_*/envelope/walls/heat_gain'
      rvEnvWallsHeatGain%MetaType = 'units'
      rvEnvWallsHeatGain%VariableType = '(W)'
      rvEnvWallsHeatGain%Description = 'Contribution of walls to cooling load'
      Call AddVariable(rvEnvWallsHeatGain)

      rvEnvFloorsHeatGain%VariableName = 'building/zone_*/envelope/floors/heat_gain'
      rvEnvFloorsHeatGain%MetaType = 'units'
      rvEnvFloorsHeatGain%VariableType = '(W)'
      rvEnvFloorsHeatGain%Description = 'Contribution of overhanging floors to cooling load'
      Call AddVariable(rvEnvFloorsHeatGain)

      rvEnvCeilingsHeatGain%VariableName = 'building/zone_*/envelope/ceilings/heat_gain'
      rvEnvCeilingsHeatGain%MetaType = 'units'
      rvEnvCeilingsHeatGain%VariableType = '(W)'
      rvEnvCeilingsHeatGain%Description = 'Contribution of ceilings to cooling load'
      Call AddVariable(rvEnvCeilingsHeatGain )

      rvEnvFoundationHeatGain%VariableName = 'building/zone_*/envelope/foundation/heat_gain'
      rvEnvFoundationHeatGain%MetaType = 'units'
      rvEnvFoundationHeatGain%VariableType = '(W)'
      rvEnvFoundationHeatGain%Description = 'Contribution of foundation to cooling load'
      Call AddVariable(rvEnvFoundationHeatGain)

      rvEnvAllComponentsHeatGain%VariableName = 'building/zone_*/envelope/all_components/heat_gain'
      rvEnvAllComponentsHeatGain%MetaType = 'units'
      rvEnvAllComponentsHeatGain%VariableType = '(W)'
      rvEnvAllComponentsHeatGain%Description = 'Heat gained through all components of envelope'
      Call AddVariable(rvEnvAllComponentsHeatGain )

      rvEnvWindowsNetFlux%VariableName = 'building/zone_*/envelope/windows/net_flux'
      rvEnvWindowsNetFlux%MetaType = 'units'
      rvEnvWindowsNetFlux%VariableType = '(W)'
      rvEnvWindowsNetFlux%Description = 'Contribution of windows to net heat flux'
      Call AddVariable(rvEnvWindowsNetFlux)

      rvEnvWallsNetFlux%VariableName = 'building/zone_*/envelope/walls/net_flux'
      rvEnvWallsNetFlux%MetaType = 'units'
      rvEnvWallsNetFlux%VariableType = '(W)'
      rvEnvWallsNetFlux%Description = 'Contribution of walls to net heat flux'
      Call AddVariable(rvEnvWallsNetFlux )

      rvEnvFloorsNetFlux%VariableName = 'building/zone_*/envelope/floors/net_flux'
      rvEnvFloorsNetFlux%MetaType = 'units'
      rvEnvFloorsNetFlux%VariableType = '(W)'
      rvEnvFloorsNetFlux%Description = 'Contribution of overhanging floors to net heat flux'
      Call AddVariable(rvEnvFloorsNetFlux )

      rvEnvCeilingsNetFlux%VariableName = 'building/zone_*/envelope/ceilings/net_flux'
      rvEnvCeilingsNetFlux%MetaType = 'units'
      rvEnvCeilingsNetFlux%VariableType = '(W)'
      rvEnvCeilingsNetFlux%Description = 'Contribution of ceilings to net heat flux'
      Call AddVariable(rvEnvCeilingsNetFlux)

      rvEnvFoundationNetFlux%VariableName = 'building/zone_*/envelope/foundation/net_flux'
      rvEnvFoundationNetFlux%MetaType = 'units'
      rvEnvFoundationNetFlux%VariableType = '(W)'
      rvEnvFoundationNetFlux%Description = 'Contribution of foundation to net heat flux'
      Call AddVariable(rvEnvFoundationNetFlux)

      rvEnvAllComponentsNetFlux%VariableName = 'building/zone_*/envelope/all_components/net_flux'
      rvEnvAllComponentsNetFlux%MetaType = 'units'
      rvEnvAllComponentsNetFlux%VariableType = '(W)'
      rvEnvAllComponentsNetFlux%Description = 'Net flux through all components of envelope'
      Call AddVariable(rvEnvAllComponentsNetFlux)

      rvInsolationTotal%VariableName = 'building/zone_*/insolation/total'
      rvInsolationTotal%MetaType = 'units'
      rvInsolationTotal%VariableType = '(W)'
      rvInsolationTotal%Description = 'Solar gains admitted through glazing'
      Call AddVariable(rvInsolationTotal)

      rvInsolationUseful%VariableName = 'building/zone_*/insolation/useful'
      rvInsolationUseful%MetaType = 'units'
      rvInsolationUseful%VariableType = '(W)'
      rvInsolationUseful%Description = 'Solar gains admitted through glazing that supplant heating loads'
      Call AddVariable(rvInsolationUseful)

      rvInsolationAdverse%VariableName = 'building/zone_*/insolation/adverse'
      rvInsolationAdverse%MetaType = 'units'
      rvInsolationAdverse%VariableType = '(W)'
      rvInsolationAdverse%Description = 'Solar gains admitted through glazing that increase cooling loads'
      Call AddVariable(rvInsolationAdverse )

      rvInternalGainsTotal%VariableName = 'building/zone_*/internal_gains/total'
      rvInternalGainsTotal%MetaType = 'units'
      rvInternalGainsTotal%VariableType = '(W)'
      rvInternalGainsTotal%Description = 'Internal heat gains (convective + radiant)'
      Call AddVariable(rvInternalGainsTotal)

      rvInternalGainsUseful%VariableName = 'building/zone_*/internal_gains/useful'
      rvInternalGainsUseful%MetaType = 'units'
      rvInternalGainsUseful%VariableType = '(W)'
      rvInternalGainsUseful%Description = 'Internal heat gains (convective + radiant) that supplant heating loads'
      Call AddVariable(rvInternalGainsUseful )

      rvInternalGainsAdverse%VariableName = 'building/zone_*/internal_gains/adverse'
      rvInternalGainsAdverse%MetaType = 'units'
      rvInternalGainsAdverse%VariableType = '(W)'
      rvInternalGainsAdverse%Description = 'Internal heat gains (convective + radiant) that increase cooling loads'
      Call AddVariable(rvInternalGainsAdverse )
      
      rvBldZnLightPow%VariableName = 'building/zone_*/lighting_power'
      rvBldZnLightPow%MetaType = 'units'
      rvBldZnLightPow%VariableType = '(W)'
      rvBldZnLightPow%Description = 'Lighting power'
      Call AddVariable(rvBldZnLightPow )

      rvBldZnEquipPow%VariableName = 'building/zone_*/equipment_power'
      rvBldZnEquipPow%MetaType = 'units'
      rvBldZnEquipPow%VariableType = '(W)'
      rvBldZnEquipPow%Description = 'Equipment power'
      Call AddVariable(rvBldZnEquipPow )


      rvBuildingAllZonesSuppliedEnergyHeating%VariableName = 'building/all_zones/supplied_energy/heating'
      rvBuildingAllZonesSuppliedEnergyHeating%MetaType = 'units'
      rvBuildingAllZonesSuppliedEnergyHeating%VariableType = '(W)'
      rvBuildingAllZonesSuppliedEnergyHeating%Description = 'Total amount of heat supplied to the building (all zones).'
      Call AddVariable(rvBuildingAllZonesSuppliedEnergyHeating)

      rvBuildingAllZonesSuppliedEnergyCooling%VariableName = 'building/all_zones/supplied_energy/cooling'
      rvBuildingAllZonesSuppliedEnergyCooling%MetaType = 'units'
      rvBuildingAllZonesSuppliedEnergyCooling%VariableType = '(W)'
      rvBuildingAllZonesSuppliedEnergyCooling%Description = 'Total amount of cooling supplied to the building (all zones).'
      Call AddVariable(rvBuildingAllZonesSuppliedEnergyCooling )

      rvBuildingAllZonesFreeCooling%VariableName = 'building/all_zones/free_cooling'
      rvBuildingAllZonesFreeCooling%MetaType = 'units'
      rvBuildingAllZonesFreeCooling%VariableType = '(W)'
      rvBuildingAllZonesFreeCooling%Description = 'Free cooling used in building (all zones).'
      Call AddVariable(rvBuildingAllZonesFreeCooling )        

      rvFreeCoolCtlFlag%VariableName = 'building/zone_*/free_cooling_ctl_flag'
      rvFreeCoolCtlFlag%MetaType = 'units'
      rvFreeCoolCtlFlag%VariableType = '(-)'
      rvFreeCoolCtlFlag%Description = 'Free cooling control flag'
      Call AddVariable(rvFreeCoolCtlFlag )
	  
      rvBuildingAllZonesSuppliedEnergyNetFlux%VariableName = 'building/all_zones/supplied_energy/net_flux'
      rvBuildingAllZonesSuppliedEnergyNetFlux%MetaType = 'units'
      rvBuildingAllZonesSuppliedEnergyNetFlux%VariableType = '(W)'
      rvBuildingAllZonesSuppliedEnergyNetFlux%Description = 'Net heat/cooling supplied to the building (all zones).'
      Call AddVariable(rvBuildingAllZonesSuppliedEnergyNetFlux)

      rvBuildingAllZonesInsolationTotal%VariableName = 'building/all_zones/insolation/total'
      rvBuildingAllZonesInsolationTotal%MetaType = 'units'
      rvBuildingAllZonesInsolationTotal%VariableType = '(W)'
      rvBuildingAllZonesInsolationTotal%Description = 'Total solar gains admitted through glazing (all zones).'
      Call AddVariable(rvBuildingAllZonesInsolationTotal)

      rvBuildingAllZonesInsolationUseful%VariableName = 'building/all_zones/insolation/useful'
      rvBuildingAllZonesInsolationUseful%MetaType = 'units'
      rvBuildingAllZonesInsolationUseful%VariableType = '(W)'
      rvBuildingAllZonesInsolationUseful%Description = 'Solar gains admitted through glazing ' // &
            'that supplant heating loads (all zones).'
      Call AddVariable(rvBuildingAllZonesInsolationUseful)

      rvBuildingAllZonesInsolationAdverse%VariableName = 'building/all_zones/insolation/adverse'
      rvBuildingAllZonesInsolationAdverse%MetaType = 'units'
      rvBuildingAllZonesInsolationAdverse%VariableType = '(W)'
      rvBuildingAllZonesInsolationAdverse%Description = 'Solar gains admitted through glazing ' // &
            'that increase cooling loads (all zones).'
      Call AddVariable(rvBuildingAllZonesInsolationAdverse )

      rvBuildingAllZonesEnvelopeWindowsHeatLoss%VariableName = 'building/all_zones/envelope/windows/heat_loss'
      rvBuildingAllZonesEnvelopeWindowsHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeWindowsHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeWindowsHeatLoss%Description = 'Heat loss to surroundings through windows (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeWindowsHeatLoss )

      rvBuildingAllZonesEnvelopeWallsHeatLoss%VariableName = 'building/all_zones/envelope/walls/heat_loss'
      rvBuildingAllZonesEnvelopeWallsHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeWallsHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeWallsHeatLoss%Description = 'Heat loss to surroundings through walls (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeWallsHeatLoss)

      rvBuildingAllZonesEnvelopeFloorsHeatLoss%VariableName = 'building/all_zones/envelope/floors/heat_loss'
      rvBuildingAllZonesEnvelopeFloorsHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeFloorsHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeFloorsHeatLoss%Description = 'Heat loss to surroundings through overhanging floors (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeFloorsHeatLoss)

      rvBuildingAllZonesEnvelopeCeilingsHeatLoss%VariableName = 'building/all_zones/envelope/ceilings/heat_loss'
      rvBuildingAllZonesEnvelopeCeilingsHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeCeilingsHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeCeilingsHeatLoss%Description = 'Heat loss to surroundings through ceilings (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeCeilingsHeatLoss )

      rvBuildingAllZonesEnvelopeFoundationHeatLoss%VariableName = 'building/all_zones/envelope/foundation/heat_loss'
      rvBuildingAllZonesEnvelopeFoundationHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeFoundationHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeFoundationHeatLoss%Description = 'Heat loss to ground through below-grade elements (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeFoundationHeatLoss)

      rvBuildingAllZonesEnvelopeInfiltrationHeatLoss%VariableName = 'building/all_zones/envelope/infiltration/heat_loss'
      rvBuildingAllZonesEnvelopeInfiltrationHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeInfiltrationHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeInfiltrationHeatLoss%Description = 'Heat loss to surroundings via infiltration (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeInfiltrationHeatLoss)

      rvBuildingAllZonesEnvelopeAllComponentsHeatLoss%VariableName = 'building/all_zones/envelope/all_components/heat_loss'
      rvBuildingAllZonesEnvelopeAllComponentsHeatLoss%MetaType = 'units'
      rvBuildingAllZonesEnvelopeAllComponentsHeatLoss%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeAllComponentsHeatLoss%Description = 'Heat lost through all components of envelope (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeAllComponentsHeatLoss)

      rvBuildingAllZonesEnvelopeWindowsHeatGain%VariableName = 'building/all_zones/envelope/windows/heat_gain'
      rvBuildingAllZonesEnvelopeWindowsHeatGain%MetaType = 'units'
      rvBuildingAllZonesEnvelopeWindowsHeatGain%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeWindowsHeatGain%Description = 'Heat gain from surroundings through ' // &
            'windows (all zones, excluding insolation)'
      Call AddVariable(rvBuildingAllZonesEnvelopeWindowsHeatGain)

      rvBuildingAllZonesEnvelopeWallsHeatGain%VariableName = 'building/all_zones/envelope/walls/heat_gain'
      rvBuildingAllZonesEnvelopeWallsHeatGain%MetaType = 'units'
      rvBuildingAllZonesEnvelopeWallsHeatGain%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeWallsHeatGain%Description = 'Heat gain from surroundings through walls (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeWallsHeatGain )

      rvBuildingAllZonesEnvelopeFloorsHeatGain%VariableName = 'building/all_zones/envelope/floors/heat_gain'
      rvBuildingAllZonesEnvelopeFloorsHeatGain%MetaType = 'units'
      rvBuildingAllZonesEnvelopeFloorsHeatGain%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeFloorsHeatGain%Description = 'Heat gain from surroundings through overhanging floors (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeFloorsHeatGain)

      rvBuildingAllZonesEnvelopeCeilingsHeatGain%VariableName = 'building/all_zones/envelope/ceilings/heat_gain'
      rvBuildingAllZonesEnvelopeCeilingsHeatGain%MetaType = 'units'
      rvBuildingAllZonesEnvelopeCeilingsHeatGain%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeCeilingsHeatGain%Description = 'Heat gain from surroundings through ceilings (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeCeilingsHeatGain )

      rvBuildingAllZonesEnvelopeFoundationHeatGain%VariableName = 'building/all_zones/envelope/foundation/heat_gain'
      rvBuildingAllZonesEnvelopeFoundationHeatGain%MetaType = 'units'
      rvBuildingAllZonesEnvelopeFoundationHeatGain%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeFoundationHeatGain%Description = 'Heat gain from ground through below-grade elements (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeFoundationHeatGain)

      rvBuildingAllZonesEnvelopeInfiltrationHeatGain%VariableName = 'building/all_zones/envelope/infiltration/heat_gain'
      rvBuildingAllZonesEnvelopeInfiltrationHeatGain%MetaType = 'units'
      rvBuildingAllZonesEnvelopeInfiltrationHeatGain%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeInfiltrationHeatGain%Description = 'Heat gain from surroundings via infiltration (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeInfiltrationHeatGain)

      rvBuildingAllZonesEnvelopeWindowsNetFlux%VariableName = 'building/all_zones/envelope/windows/net_flux'
      rvBuildingAllZonesEnvelopeWindowsNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeWindowsNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeWindowsNetFlux%Description = 'Net heat loss to surroundings through windows (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeWindowsNetFlux)

      rvBuildingAllZonesEnvelopeWallsNetFlux%VariableName = 'building/all_zones/envelope/walls/net_flux'
      rvBuildingAllZonesEnvelopeWallsNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeWallsNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeWallsNetFlux%Description = 'Net heat loss to surroundings through walls (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeWallsNetFlux)

      rvBuildingAllZonesEnvelopeFloorNetFlux%VariableName = 'building/all_zones/envelope/floors/net_flux'
      rvBuildingAllZonesEnvelopeFloorNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeFloorNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeFloorNetFlux%Description = 'Net heat loss to surroundings through overhanging floors (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeFloorNetFlux)

      rvBuildingAllZonesEnvelopeCeilingsNetFlux%VariableName = 'building/all_zones/envelope/ceilings/net_flux'
      rvBuildingAllZonesEnvelopeCeilingsNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeCeilingsNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeCeilingsNetFlux%Description = 'Net heat loss to surroundings through ceilings (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeCeilingsNetFlux)

      rvBuildingAllZonesEnvelopeFoundationNetFlux%VariableName = 'building/all_zones/envelope/foundation/net_flux'
      rvBuildingAllZonesEnvelopeFoundationNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeFoundationNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeFoundationNetFlux%Description = 'Net heat loss to ground through below-grade elements (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeFoundationNetFlux)

      rvBuildingAllZonesEnvelopeInfiltrationNetFlux%VariableName = 'building/all_zones/envelope/infiltration/net_flux'
      rvBuildingAllZonesEnvelopeInfiltrationNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeInfiltrationNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeInfiltrationNetFlux%Description = 'Net heat loss to surroundings via infiltration (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeInfiltrationNetFlux )

      rvBuildingAllZonesEnvelopeAllComponentsNetFlux%VariableName = 'building/all_zones/envelope/all_components/net_flux'
      rvBuildingAllZonesEnvelopeAllComponentsNetFlux%MetaType = 'units'
      rvBuildingAllZonesEnvelopeAllComponentsNetFlux%VariableType = '(W)'
      rvBuildingAllZonesEnvelopeAllComponentsNetFlux%Description = 'Net heat lost through all components of envelope (all zones)'
      Call AddVariable(rvBuildingAllZonesEnvelopeAllComponentsNetFlux)

      rvBuildingAllZonesThermalLoadsHeatingTotal%VariableName = 'building/all_zones/thermal_loads/heating/total'
      rvBuildingAllZonesThermalLoadsHeatingTotal%MetaType = 'units'
      rvBuildingAllZonesThermalLoadsHeatingTotal%VariableType = '(W)'
      rvBuildingAllZonesThermalLoadsHeatingTotal%Description = 'Total heating load ( ' // &
            'heat loss through envelope + ventilation and infiltration loads; all zones )'
      Call AddVariable(rvBuildingAllZonesThermalLoadsHeatingTotal)

      rvBuildingAllZonesThermalLoadsCoolingTotal%VariableName = 'building/all_zones/thermal_loads/cooling/total'
      rvBuildingAllZonesThermalLoadsCoolingTotal%MetaType = 'units'
      rvBuildingAllZonesThermalLoadsCoolingTotal%VariableType = '(W)'
      rvBuildingAllZonesThermalLoadsCoolingTotal%Description = 'Total cooling load ( heat gain ' // &
            'through envelope + ventilation and infiltration loads; all zones )'
      Call AddVariable(rvBuildingAllZonesThermalLoadsCoolingTotal)

      rvBuildingAllZonesThermalLoadsNet%VariableName = 'building/all_zones/thermal_loads/net'
      rvBuildingAllZonesThermalLoadsNet%MetaType = 'units'
      rvBuildingAllZonesThermalLoadsNet%VariableType = '(W)'
      rvBuildingAllZonesThermalLoadsNet%Description = 'Total net load ( heat gain through envelope + ' // &
            'ventilation and infiltration loads; all zones )'
      Call AddVariable(rvBuildingAllZonesThermalLoadsNet)

      rvBuildingAllZonesInternalGainsTotal%VariableName = 'building/all_zones/internal_gains/total'
      rvBuildingAllZonesInternalGainsTotal%MetaType = 'units'
      rvBuildingAllZonesInternalGainsTotal%VariableType = '(W)'
      rvBuildingAllZonesInternalGainsTotal%Description = 'Sensible heat transfer from occupants, lights and equipment (all zones).'
      Call AddVariable(rvBuildingAllZonesInternalGainsTotal )

      rvBuildingAllZonesInternalGainsUseful%VariableName = 'building/all_zones/internal_gains/useful'
      rvBuildingAllZonesInternalGainsUseful%MetaType = 'units'
      rvBuildingAllZonesInternalGainsUseful%VariableType = '(W)'
      rvBuildingAllZonesInternalGainsUseful%Description = 'Sensible heat transfer from occupants, lights ' // &
            'and equipment that supplant heating loads (all zones).'
      Call AddVariable(rvBuildingAllZonesInternalGainsUseful)

      rvBuildingAllZonesInternalGainsAdverse%VariableName = 'building/all_zones/internal_gains/adverse'
      rvBuildingAllZonesInternalGainsAdverse%MetaType = 'units'
      rvBuildingAllZonesInternalGainsAdverse%VariableType = '(W)'
      rvBuildingAllZonesInternalGainsAdverse%Description = 'Sensible heat transfer from occupants, ' // &
            'lights and equipment that increase cooling loads (all zones).'
      Call AddVariable(rvBuildingAllZonesInternalGainsAdverse)
      
      rvBldAllZonesLightPowTtl%VariableName = 'building/all_zones/lighting_power/total'
      rvBldAllZonesLightPowTtl%MetaType = 'units'
      rvBldAllZonesLightPowTtl%VariableType = '(W)'
      rvBldAllZonesLightPowTtl%Description = 'Power consumption of lights (all zones).'
      Call AddVariable(rvBldAllZonesLightPowTtl)

      rvBldAllZonesEquipPowTtl%VariableName = 'building/all_zones/equipment_power/total'
      rvBldAllZonesEquipPowTtl%MetaType = 'units'
      rvBldAllZonesEquipPowTtl%VariableType = '(W)'
      rvBldAllZonesEquipPowTtl%Description = 'Power consumption of equipment (all zones).'
      Call AddVariable(rvBldAllZonesEquipPowTtl)

      rvBuildingAllZonesEnergyBalanceNet%VariableName = 'building/all_zones/energy_balance/net'
      rvBuildingAllZonesEnergyBalanceNet%MetaType = 'units'
      rvBuildingAllZonesEnergyBalanceNet%VariableType = '(W)'
      rvBuildingAllZonesEnergyBalanceNet%Description = 'Energy balance in building (Gains + Plant interaction - Loads; all zones).'
      Call AddVariable(rvBuildingAllZonesEnergyBalanceNet)

      rvTemperature%VariableName = 'building/zone_*/surface_*/temperature'
      rvTemperature%MetaType = 'units'
      rvTemperature%VariableType = '(oC)'
      rvTemperature%Description = 'Temperature on zone inside face'
      Call AddVariable(rvTemperature)

      rvExtSurfTemperature%VariableName = 'building/zone_*/surface_*/ext_surf_temperature'
      rvExtSurfTemperature%MetaType = 'units'
      rvExtSurfTemperature%VariableType = '(oC)'
      rvExtSurfTemperature%Description = 'Temperature on exterior face'
      Call AddVariable(rvExtSurfTemperature )

      rvPlantContainmentFlux%VariableName = 'building/zone_*/surface_*/plant_containment_flux'
      rvPlantContainmentFlux%MetaType = 'units'
      rvPlantContainmentFlux%VariableType = '(W)'
      rvPlantContainmentFlux%Description = 'Plant containment radiant heat flux at surface'
      Call AddVariable(rvPlantContainmentFlux)

      rvHCi%VariableName = 'building/zone_*/surface_*/HCi'
      rvHCi%MetaType = 'units'
      rvHCi%VariableType = '(W/(m2 K))'
      rvHCi%Description = 'Internal side convective coefficient'
      Call AddVariable(rvHCi)

      rvHCe%VariableName = 'building/zone_*/surface_*/HCe'
      rvHCe%MetaType = 'units'
      rvHCe%VariableType = '(W/(m2 K))'
      rvHCe%Description = 'External side convective coefficient'
      Call AddVariable(rvHCe)

      rvPRT%VariableName = 'building/zone_*/surface_*/PRT'
      rvPRT%MetaType = 'units'
      rvPRT%VariableType = '(oC)'
      rvPRT%Description = 'Plane radiant temperature on zone inside face'
      Call AddVariable(rvPRT)

      rvHRi%VariableName = 'building/zone_*/surface_*/HRi'
      rvHRi%MetaType = 'units'
      rvHRi%VariableType = '(W/(m2 K))'
      rvHRi%Description = 'Radiant heat transfer coef. (inside face)'
      Call AddVariable(rvHRi )

      rvAmbRT%VariableName = 'building/zone_*/surface_*/AmbRT'
      rvAmbRT%MetaType = 'units'
      rvAmbRT%VariableType = '(oC)'
      rvAmbRT%Description = 'Ambient radiant temperature for exterior surface'
      Call AddVariable(rvAmbRT)

      rvNodeTemp%VariableName = 'building/zone_*/surface_*/node_*/temperature'
      rvNodeTemp%MetaType = 'units'
      rvNodeTemp%VariableType = '(oC)'
      rvNodeTemp%Description = 'Temperature at node within multilayer construction'
      Call AddVariable(rvNodeTemp)

      rvClimateSolarDiffuseHorizontalRadiation%VariableName = 'climate/solar/diffuse_horizontal_radiation'
      rvClimateSolarDiffuseHorizontalRadiation%MetaType = 'units'
      rvClimateSolarDiffuseHorizontalRadiation%VariableType = '(W/m2)'
      rvClimateSolarDiffuseHorizontalRadiation%Description = 'Climate diffuse horizontal solar radiation'
      Call AddVariable(rvClimateSolarDiffuseHorizontalRadiation)

      rvClimateSolarDirectNormalRadiation%VariableName = 'climate/solar/direct_normal_radiation'
      rvClimateSolarDirectNormalRadiation%MetaType = 'units'
      rvClimateSolarDirectNormalRadiation%VariableType = '(W/m2)'
      rvClimateSolarDirectNormalRadiation%Description = 'Climate direct normal solar radiation'
      Call AddVariable(rvClimateSolarDirectNormalRadiation)

      rvClimateDryBulbTemperature%VariableName = 'climate/dry_bulb_temperature'
      rvClimateDryBulbTemperature%MetaType = 'units'
      rvClimateDryBulbTemperature%VariableType = '(oC)'
      rvClimateDryBulbTemperature%Description = 'Climate dry-bulb temperature'
      Call AddVariable(rvClimateDryBulbTemperature )

      rvClimateRelativeHumidity%VariableName = 'climate/relative_humidity'
      rvClimateRelativeHumidity%MetaType = 'units'
      rvClimateRelativeHumidity%VariableType = '(%)'
      rvClimateRelativeHumidity%Description = 'Climate relative humidity'
      Call AddVariable(rvClimateRelativeHumidity)

      rvClimateWindVelocity%VariableName = 'climate/wind/velocity'
      rvClimateWindVelocity%MetaType = 'units'
      rvClimateWindVelocity%VariableType = '(m/s)'
      rvClimateWindVelocity%Description = 'Climate wind velocity'
      Call AddVariable(rvClimateWindVelocity )

      rvClimateWindDirection%VariableName = 'climate/wind/direction'
      rvClimateWindDirection%MetaType = 'units'
      rvClimateWindDirection%VariableType = '(degrees)'
      rvClimateWindDirection%Description = 'Climate wind direction'
      Call AddVariable(rvClimateWindDirection)

      rvClimateCloudCover%VariableName = 'climate/cloud_cover'
      rvClimateCloudCover%MetaType = 'units'
      rvClimateCloudCover%VariableType = '(fraction)'
      rvClimateCloudCover%Description = 'Climate cloud cover'
      Call AddVariable(rvClimateCloudCover)

      rvClimateSkyTemperature%VariableName = 'climate/sky_temperature'
      rvClimateSkyTemperature%MetaType = 'units'
      rvClimateSkyTemperature%VariableType = '(oC)'
      rvClimateSkyTemperature%Description = 'Climate sky temperature'
      Call AddVariable(rvClimateSkyTemperature)

      rvClimateSkyTemperatureDepression%VariableName = 'climate/sky_temperature_depression'
      rvClimateSkyTemperatureDepression%MetaType = 'units'
      rvClimateSkyTemperatureDepression%VariableType = '(K)'
      rvClimateSkyTemperatureDepression%Description = 'Climate sky temperature depression'
      Call AddVariable(rvClimateSkyTemperatureDepression)

      rvClimateAmbientAirTsat%VariableName = 'climate/ambient_air_tsat'
      rvClimateAmbientAirTsat%MetaType = 'units'
      rvClimateAmbientAirTsat%VariableType = '(oC)'
      rvClimateAmbientAirTsat%Description = 'ambient air saturation temperature'
      Call AddVariable(rvClimateAmbientAirTsat)

      rvBuildingTimePresent%VariableName = 'building/time/present'
      rvBuildingTimePresent%MetaType = 'units'
      rvBuildingTimePresent%VariableType = '(hours)'
      rvBuildingTimePresent%Description = 'Time corresponding to current simulation time row'
      Call AddVariable(rvBuildingTimePresent)

      rvBuildingTimeFuture%VariableName = 'building/time/future'
      rvBuildingTimeFuture%MetaType = 'units'
      rvBuildingTimeFuture%VariableType = '(hours)'
      rvBuildingTimeFuture%Description = 'Time corresponding to future simulation time row'
      Call AddVariable(rvBuildingTimeFuture)

      rvBuildingHourPresent%VariableName = 'building/hour/present'
      rvBuildingHourPresent%MetaType = 'units'
      rvBuildingHourPresent%VariableType = '(hours)'
      rvBuildingHourPresent%Description = 'Hour corresponding to current simulation hour'
      Call AddVariable(rvBuildingHourPresent)

      rvBuildingHourFuture%VariableName = 'building/hour/future'
      rvBuildingHourFuture%MetaType = 'units'
      rvBuildingHourFuture%VariableType = '(hours)'
      rvBuildingHourFuture%Description = 'Hour corresponding to future simulation hour'
      Call AddVariable(rvBuildingHourFuture)

      rvBuildingDayNumberPresent%VariableName = 'building/day_number/present'
      rvBuildingDayNumberPresent%MetaType = 'units'
      rvBuildingDayNumberPresent%VariableType = '(days)'
      rvBuildingDayNumberPresent%Description = 'Day # corresponding to current simulation day'
      Call AddVariable(rvBuildingDayNumberPresent)

      rvBuildingDayNumberFuture%VariableName = 'building/day_number/future'
      rvBuildingDayNumberFuture%MetaType = 'units'
      rvBuildingDayNumberFuture%VariableType = '(day)'
      rvBuildingDayNumberFuture%Description = 'Day # corresponding to future simulation day'
      Call AddVariable(rvBuildingDayNumberFuture)

      rvBuildingYearPresent%VariableName = 'building/year/present'
      rvBuildingYearPresent%MetaType = 'units'
      rvBuildingYearPresent%VariableType = '(year)'
      rvBuildingYearPresent%Description = 'Year corresponding to current simulation day'
      Call AddVariable(rvBuildingYearPresent)

      rvBuildingYearFuture%VariableName = 'building/year/future'
      rvBuildingYearFuture%MetaType = 'units'
      rvBuildingYearFuture%VariableType = '(year)'
      rvBuildingYearFuture%Description = 'Year corresponding to future simulation day'
      Call AddVariable(rvBuildingYearFuture)

      rvBuildingDayPresent%VariableName = 'building/day/present'
      rvBuildingDayPresent%MetaType = 'units'
      rvBuildingDayPresent%VariableType = '(days)'
      rvBuildingDayPresent%Description = 'Day corresponding to current simulation time row'
      Call AddVariable(rvBuildingDayPresent)

      rvBuildingMonth%VariableName = 'building/month'
      rvBuildingMonth%MetaType = 'units'
      rvBuildingMonth%VariableType = '(-)'
      rvBuildingMonth%Description = 'Month corresponding to current simulation time row'
      Call AddVariable(rvBuildingMonth)

      rvBuildingDayFuture%VariableName = 'building/day/future'
      rvBuildingDayFuture%MetaType = 'units'
      rvBuildingDayFuture%VariableType = '(day)'
      rvBuildingDayFuture%Description = 'Day corresponding to future simulation time row'
      Call AddVariable(rvBuildingDayFuture)

      rvBuildingTimeStep%VariableName = 'building/time_step'
      rvBuildingTimeStep%MetaType = 'units'
      rvBuildingTimeStep%VariableType = '(-)'
      rvBuildingTimeStep%Description = 'Time step #'
      Call AddVariable(rvBuildingTimeStep)
      
      rvBldSeason%VariableName = 'building/season'
      rvBldSeason%MetaType = 'units'
      rvBldSeason%VariableType = '(-)'
      rvBldSeason%Description = 'Current season in simulation (Quick-run mode)'
      Call AddVariable(rvBldSeason)


      rvPlantCompNodeTemperature%VariableName = 'plant/*/node_*/temperature'
      rvPlantCompNodeTemperature%MetaType = 'units'
      rvPlantCompNodeTemperature%VariableType = '(oC)'
      rvPlantCompNodeTemperature%Description = 'Plant component node temperature'
      Call AddVariable(rvPlantCompNodeTemperature)

      rvPlantCompNodeFirstPhaseFlow%VariableName = 'plant/*/node_*/*_flow'
      rvPlantCompNodeFirstPhaseFlow%MetaType = 'units'
      rvPlantCompNodeFirstPhaseFlow%VariableType = '(kg/s)'
      rvPlantCompNodeFirstPhaseFlow%Description = 'Plant component node first-phase flow'
      Call AddVariable(rvPlantCompNodeFirstPhaseFlow)

      rvPlantCompNodeSecondPhaseFlow%VariableName = 'plant/*/node_*/second_phase/moisture_flow'
      rvPlantCompNodeSecondPhaseFlow%MetaType = 'units'
      rvPlantCompNodeSecondPhaseFlow%VariableType = '(kg/s)'
      rvPlantCompNodeSecondPhaseFlow%Description = 'Plant component node second-phase flow'
      Call AddVariable(rvPlantCompNodeSecondPhaseFlow)

      rvPlantCompNodeHydrogenFlow%VariableName = 'plant/*/node_*/second_phase/hydrogen_flow'
      rvPlantCompNodeHydrogenFlow%MetaType = 'units'
      rvPlantCompNodeHydrogenFlow%VariableType = '(kg/s)'
      rvPlantCompNodeHydrogenFlow%Description = 'Plant component node hydrogen flow flow'
      Call AddVariable(rvPlantCompNodeHydrogenFlow)

      rvPlantCompNodeConnectTemperature%VariableName = 'plant/*/node_*/connection_*/temperature'
      rvPlantCompNodeConnectTemperature%MetaType = 'units'
      rvPlantCompNodeConnectTemperature%VariableType = '(oC)'
      rvPlantCompNodeConnectTemperature%Description = 'Plant component node connection temperature'
      Call AddVariable(rvPlantCompNodeConnectTemperature)

      rvPlantCompNodeConnectWaterFlow%VariableName = 'plant/*/node_*/connection_*/water_flow'
      rvPlantCompNodeConnectWaterFlow%MetaType = 'units'
      rvPlantCompNodeConnectWaterFlow%VariableType = '(kg/s)'
      rvPlantCompNodeConnectWaterFlow%Description = 'Plant component node connection water flow'
      Call AddVariable(rvPlantCompNodeConnectWaterFlow)

      rvPlantCompNodeConnectAirFlow%VariableName = 'plant/*/node_*/connection_*/air_flow'
      rvPlantCompNodeConnectAirFlow%MetaType = 'units'
      rvPlantCompNodeConnectAirFlow%VariableType = '(kg/s)'
      rvPlantCompNodeConnectAirFlow%Description = 'Plant component node connection air flow'
      Call AddVariable(rvPlantCompNodeConnectAirFlow)

      rvPlantCompNodeConnectMoistureFlow%VariableName = 'plant/*/node_*/connection_*/moisture_flow'
      rvPlantCompNodeConnectMoistureFlow%MetaType = 'units'
      rvPlantCompNodeConnectMoistureFlow%VariableType = '(kg/s)'
      rvPlantCompNodeConnectMoistureFlow%Description = 'Plant component node connection moisture flow'
      Call AddVariable(rvPlantCompNodeConnectMoistureFlow)

      rvPlantCompNodeConnectHydrogenFlow%VariableName = 'plant/*/node_*/connection_*/hydrogen_flow'
      rvPlantCompNodeConnectHydrogenFlow%MetaType = 'units'
      rvPlantCompNodeConnectHydrogenFlow%VariableType = '(kg/s)'
      rvPlantCompNodeConnectHydrogenFlow%Description = 'Plant component node connection Hydrogen flow'
      Call AddVariable(rvPlantCompNodeConnectHydrogenFlow)

      rvElecNetLoadsTotalLoad%VariableName = 'electrical_net/loads/total_load'
      rvElecNetLoadsTotalLoad%MetaType = 'units'
      rvElecNetLoadsTotalLoad%VariableType = '(W)'
      rvElecNetLoadsTotalLoad%Description = 'Total load on electrical network'
      Call AddVariable(rvElecNetLoadsTotalLoad)

      rvElecNetLoadsHvacLoad%VariableName = 'electrical_net/loads/HVAC_load'
      rvElecNetLoadsHvacLoad%MetaType = 'units'
      rvElecNetLoadsHvacLoad%VariableType = '(W)'
      rvElecNetLoadsHvacLoad%Description = 'HVAC load on electrical network'
      Call AddVariable(rvElecNetLoadsHvacLoad)

      rvElecNetLoadsOccupantLoad%VariableName = 'electrical_net/loads/occupant_load'
      rvElecNetLoadsOccupantLoad%MetaType = 'units'
      rvElecNetLoadsOccupantLoad%VariableType = '(W)'
      rvElecNetLoadsOccupantLoad%Description = 'Occupant-driven load on electrical network'
      Call AddVariable(rvElecNetLoadsOccupantLoad)

      rvElecNetLoadsExternalLoad%VariableName = 'electrical_net/loads/external_load'
      rvElecNetLoadsExternalLoad%MetaType = 'units'
      rvElecNetLoadsExternalLoad%VariableType = '(W)'
      rvElecNetLoadsExternalLoad%Description = 'Load on electrical network from off-site sources'
      Call AddVariable(rvElecNetLoadsExternalLoad)

      rvElecNetGenTotalGeneration%VariableName = 'electrical_net/generation/total_generation'
      rvElecNetGenTotalGeneration%MetaType = 'units'
      rvElecNetGenTotalGeneration%VariableType = '(W)'
      rvElecNetGenTotalGeneration%Description = 'Total onsite+offsite electrical generation'
      Call AddVariable(rvElecNetGenTotalGeneration)

      rvElecNetGenOnsiteGeneration%VariableName = 'electrical_net/generation/onsite_generation'
      rvElecNetGenOnsiteGeneration%MetaType = 'units'
      rvElecNetGenOnsiteGeneration%VariableType = '(W)'
      rvElecNetGenOnsiteGeneration%Description = 'Total onsite electrical generation'
      Call AddVariable(rvElecNetGenOnsiteGeneration)

      rvElecNetGenOffsiteGeneration%VariableName = 'electrical_net/generation/offsite_generation'
      rvElecNetGenOffsiteGeneration%MetaType = 'units'
      rvElecNetGenOffsiteGeneration%VariableType = '(W)'
      rvElecNetGenOffsiteGeneration%Description = 'Total offsite electrical generation'
      Call AddVariable(rvElecNetGenOffsiteGeneration)

      rvElecNetGenOffsiteCoincident%VariableName = 'electrical_net/generation/offsite_coincident'
      rvElecNetGenOffsiteCoincident%MetaType = 'units'
      rvElecNetGenOffsiteCoincident%VariableType = '(W)'
      rvElecNetGenOffsiteCoincident%Description = 'Total offsite generation coincident with occupant load'
      Call AddVariable(rvElecNetGenOffsiteCoincident)

      rvElecNetGridImport%VariableName = 'electrical_net/grid/import'
      rvElecNetGridImport%MetaType = 'units'
      rvElecNetGridImport%VariableType = '(W)'
      rvElecNetGridImport%Description = 'Net balance on electrical grid'
      Call AddVariable(rvElecNetGridImport)

      rvElecNetGridExport%VariableName = 'electrical_net/grid/export'
      rvElecNetGridExport%MetaType = 'units'
      rvElecNetGridExport%VariableType = '(W)'
      rvElecNetGridExport%Description = 'Surplus electricity exported go grid'
      Call AddVariable(rvElecNetGridExport)

      rvElecNetGridNetBalance%VariableName = 'electrical_net/grid/net_balance'
      rvElecNetGridNetBalance%MetaType = 'units'
      rvElecNetGridNetBalance%VariableType = '(W)'
      rvElecNetGridNetBalance%Description = 'Net electricity balance'
      Call AddVariable(rvElecNetGridNetBalance)

      rvElecNetNodesVoltageMagnitude%VariableName = 'electrical_net/nodes/node_*/voltage/magnitude'
      rvElecNetNodesVoltageMagnitude%MetaType = 'units'
      rvElecNetNodesVoltageMagnitude%VariableType = '(Volts)'
      rvElecNetNodesVoltageMagnitude%Description = 'Electrical network node: V magnitude'
      Call AddVariable(rvElecNetNodesVoltageMagnitude)

      rvElecNetNodesVoltageAngle%VariableName = 'electrical_net/nodes/node_*/voltage/angle'
      rvElecNetNodesVoltageAngle%MetaType = 'units'
      rvElecNetNodesVoltageAngle%VariableType = '(radians)'
      rvElecNetNodesVoltageAngle%Description = 'Electrical network node: V angle'
      Call AddVariable(rvElecNetNodesVoltageAngle)

      rvElecNetNodesLoadReal%VariableName = 'electrical_net/nodes/node_*/load/real'
      rvElecNetNodesLoadReal%MetaType = 'units'
      rvElecNetNodesLoadReal%VariableType = '(W)'
      rvElecNetNodesLoadReal%Description = 'Electrical network node: real load'
      Call AddVariable(rvElecNetNodesLoadReal)

      rvElecNetNodesLoadReactive%VariableName = 'electrical_net/nodes/node_*/load/reactive'
      rvElecNetNodesLoadReactive%MetaType = 'units'
      rvElecNetNodesLoadReactive%VariableType = '(W)'
      rvElecNetNodesLoadReactive%Description = 'Electrical network node: reactive load'
      Call AddVariable(rvElecNetNodesLoadReactive)

      rvElecNetNodesGenerationReal%VariableName = 'electrical_net/nodes/node_*/generation/real'
      rvElecNetNodesGenerationReal%MetaType = 'units'
      rvElecNetNodesGenerationReal%VariableType = '(W)'
      rvElecNetNodesGenerationReal%Description = 'Electrical network node: real generation'
      Call AddVariable(rvElecNetNodesGenerationReal)

      rvElecNetNodesGenerationReactive%VariableName = 'electrical_net/nodes/node_*/generation/reactive'
      rvElecNetNodesGenerationReactive%MetaType = 'units'
      rvElecNetNodesGenerationReactive%VariableType = '(W)'
      rvElecNetNodesGenerationReactive%Description = 'Electrical network node: reactive generation'
      Call AddVariable(rvElecNetNodesGenerationReactive)

      rvElecNetNodesTransmissionReal%VariableName = 'electrical_net/nodes/node_*/transmission/real'
      rvElecNetNodesTransmissionReal%MetaType = 'units'
      rvElecNetNodesTransmissionReal%VariableType = '(W)'
      rvElecNetNodesTransmissionReal%Description = 'Electrical network node: real transmission'
      Call AddVariable(rvElecNetNodesTransmissionReal)

      rvElecNetNodesTransmissionReative%VariableName = 'electrical_net/nodes/node_*/transmission/reactive'
      rvElecNetNodesTransmissionReative%MetaType = 'units'
      rvElecNetNodesTransmissionReative%VariableType = '(W)'
      rvElecNetNodesTransmissionReative%Description = 'Electrical network node: reactive transmission'
      Call AddVariable(rvElecNetNodesTransmissionReative)

      rvElecNetHybridComponentFlux%VariableName = 'electrical_net/hybrid_components/*/flux'
      rvElecNetHybridComponentFlux%MetaType = 'units'
      rvElecNetHybridComponentFlux%VariableType = '(W)'
      rvElecNetHybridComponentFlux%Description = 'Electrical network hybrid component net electrical power'
      Call AddVariable(rvElecNetHybridComponentFlux)

      rvElecNetPowerOnlyComponents%VariableName = 'electrical_net/power_only_components/*/flux'
      rvElecNetPowerOnlyComponents%MetaType = 'units'
      rvElecNetPowerOnlyComponents%VariableType = '(W)'
      rvElecNetPowerOnlyComponents%Description = 'Electrical network power only component: electrical power'
      Call AddVariable(rvElecNetPowerOnlyComponents)

      rvMfnTotalNodeFlowRate%VariableName = 'mfn/*/*/flowrate'
      rvMfnTotalNodeFlowRate%MetaType = 'units'
      rvMfnTotalNodeFlowRate%VariableType = '(kg/s)'
      rvMfnTotalNodeFlowRate%Description = 'mfn total node flow rate'
      Call AddVariable(rvMfnTotalNodeFlowRate)

      rvMfnTotalNodeVolFlowRate%VariableName = 'mfn/*/*/volflowrate'
      rvMfnTotalNodeVolFlowRate%MetaType = 'units'
      rvMfnTotalNodeVolFlowRate%VariableType = '(m3/h)'
      rvMfnTotalNodeVolFlowRate%Description = 'mfn total node volume flow rate'
      Call AddVariable(rvMfnTotalNodeVolFlowRate)

      rvMfnTotalNodeTemp%VariableName = 'mfn/*/*/temp'
      rvMfnTotalNodeTemp%MetaType = 'units'
      rvMfnTotalNodeTemp%VariableType = '(oC)'
      rvMfnTotalNodeTemp%Description = 'mfn node temperature'
      Call AddVariable(rvMfnTotalNodeTemp)

      rvMfnConnectPressureDrop%VariableName = 'mfn/*/*/Dp'
      rvMfnConnectPressureDrop%MetaType = 'units'
      rvMfnConnectPressureDrop%VariableType = '(Pa)'
      rvMfnConnectPressureDrop%Description = 'mfn connection pressure drop'
      Call AddVariable(rvMfnConnectPressureDrop)

      rvMfnConnectFlowRate%VariableName = 'mfn/*/*/flow'
      rvMfnConnectFlowRate%MetaType = 'units'
      rvMfnConnectFlowRate%VariableType = '(kg/s)'
      rvMfnConnectFlowRate%Description = 'mfn connection flow rate'
      Call AddVariable(rvMfnConnectFlowRate)

      rvMfnConnectVeloc%VariableName = 'mfn/*/*/veloc'
      rvMfnConnectVeloc%MetaType = 'units'
      rvMfnConnectVeloc%VariableType = '(m/s)'
      rvMfnConnectVeloc%Description = 'mfn connection flow velocity'
      Call AddVariable(rvMfnConnectVeloc)

      rvMfnContamCon%VariableName = 'mfn/*/*/*'
      rvMfnContamCon%MetaType = 'units'
      rvMfnContamCon%VariableType = '(kg/kg)'
      rvMfnContamCon%Description = 'mfn contaminant concentration'
      Call AddVariable(rvMfnContamCon)

      !Used by SiteUtilities.F
      rvTFuelAllEndEnergyContent%VariableName = 'total_fuel_use/*/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContent%MetaType = 'units'
      rvTFuelAllEndEnergyContent%VariableType = '(W)'
      rvTFuelAllEndEnergyContent%Description = '*** Not Defined ***'
      Call AddVariable(rvTFuelAllEndEnergyContent)

      rvTFuelAllEndEnergyContElec%VariableName = 'total_fuel_use/electricity/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContElec%MetaType = 'units'
      rvTFuelAllEndEnergyContElec%VariableType = '(W)'
      rvTFuelAllEndEnergyContElec%Description = 'Energy content of all electricity used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContElec)

      rvTFuelAllEndEnergyContNatGas%VariableName = 'total_fuel_use/natural_gas/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContNatGas%MetaType = 'units'
      rvTFuelAllEndEnergyContNatGas%VariableType = '(W)'
      rvTFuelAllEndEnergyContNatGas%Description = 'Energy content of all natural gas used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContNatGas)

      rvTFuelAllEndEnergyContOil%VariableName = 'total_fuel_use/oil/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContOil%MetaType = 'units'
      rvTFuelAllEndEnergyContOil%VariableType = '(W)'
      rvTFuelAllEndEnergyContOil%Description = 'Energy content of all oil used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContOil)

      rvTFuelAllEndEnergyContPropane%VariableName = 'total_fuel_use/propane/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContPropane%MetaType = 'units'
      rvTFuelAllEndEnergyContPropane%VariableType = '(W)'
      rvTFuelAllEndEnergyContPropane%Description = 'Energy content of all propane used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContPropane)

      rvTFuelAllEndEnergyContMixWood%VariableName = 'total_fuel_use/mixed_wood/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContMixWood%MetaType = 'units'
      rvTFuelAllEndEnergyContMixWood%VariableType = '(W)'
      rvTFuelAllEndEnergyContMixWood%Description = 'Energy content of all mixed wood used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContMixWood)

      rvTFuelAllEndEnergyContHardWood%VariableName = 'total_fuel_use/hard_wood/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContHardWood%MetaType = 'units'
      rvTFuelAllEndEnergyContHardWood%VariableType = '(W)'
      rvTFuelAllEndEnergyContHardWood%Description = 'Energy content of all hard wood used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContHardWood)

      rvTFuelAllEndEnergyContSoftWood%VariableName = 'total_fuel_use/soft_wood/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContSoftWood%MetaType = 'units'
      rvTFuelAllEndEnergyContSoftWood%VariableType = '(W)'
      rvTFuelAllEndEnergyContSoftWood%Description = 'Energy content of all soft wood used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContSoftWood)

      rvTFuelAllEndEnergyContPellets%VariableName = 'total_fuel_use/wood_pellets/all_end_uses/energy_content'
      rvTFuelAllEndEnergyContPellets%MetaType = 'units'
      rvTFuelAllEndEnergyContPellets%VariableType = '(W)'
      rvTFuelAllEndEnergyContPellets%Description = 'Energy content of all wood pellets used on site.'
      Call AddVariable(rvTFuelAllEndEnergyContPellets)

      rvTFuelAllEndQty%VariableName = 'total_fuel_use/*/all_end_uses/quantity'
      rvTFuelAllEndQty%MetaType = 'units'
      rvTFuelAllEndQty%VariableType = '*** Not defined ***'
      rvTFuelAllEndQty%Description = 'Total amount of fuel used on site'
      Call AddVariable(rvTFuelAllEndQty)
      
      rvTFuelCstAllEnd%VariableName = 'total_fuel_cost/*/all_end_uses/quantity'
      rvTFuelCstAllEnd%MetaType = 'units'
      rvTFuelCstAllEnd%VariableType = '($/s)'
      rvTFuelCstAllEnd%Description = 'Total cost of ##Undefined## used on site'
      Call AddVariable(rvTFuelCstAllEnd)

      rvTFuelAllEndQtyElectricity%VariableName = 'total_fuel_use/electricity/all_end_uses/quantity'
      rvTFuelAllEndQtyElectricity%MetaType = 'units'
      rvTFuelAllEndQtyElectricity%VariableType = '(kWh/s)'
      rvTFuelAllEndQtyElectricity%Description = 'Total amount of electricity used on site.'
      Call AddVariable(rvTFuelAllEndQtyElectricity)
      
      rvTFuelCstAllEndElectricity%VariableName = 'total_fuel_cost/electricity/all_end_uses/quantity'
      rvTFuelCstAllEndElectricity%MetaType = 'units'
      rvTFuelCstAllEndElectricity%VariableType = '($/s)'
      rvTFuelCstAllEndElectricity%Description = 'Total cost of electricity used on site.'
      Call AddVariable(rvTFuelCstAllEndElectricity)

      rvTFuelAllEndQtyNaturalGas%VariableName = 'total_fuel_use/natural_gas/all_end_uses/quantity'
      rvTFuelAllEndQtyNaturalGas%MetaType = 'units'
      rvTFuelAllEndQtyNaturalGas%VariableType = '(m3/s)'
      rvTFuelAllEndQtyNaturalGas%Description = 'Total amount of natural gas used on site.'
      Call AddVariable(rvTFuelAllEndQtyNaturalGas)

      rvTFuelCstAllEndNaturalGas%VariableName = 'total_fuel_cost/natural_gas/all_end_uses/quantity'
      rvTFuelCstAllEndNaturalGas%MetaType = 'units'
      rvTFuelCstAllEndNaturalGas%VariableType = '($/s)'
      rvTFuelCstAllEndNaturalGas%Description = 'Total cost of natural gas used on site.'
      Call AddVariable(rvTFuelCstAllEndNaturalGas)
      
      rvTFuelAllEndQtyOil%VariableName = 'total_fuel_use/oil/all_end_uses/quantity'
      rvTFuelAllEndQtyOil%MetaType = 'units'
      rvTFuelAllEndQtyOil%VariableType = '(l/s)'
      rvTFuelAllEndQtyOil%Description = 'Total amount of oil used on site.'
      Call AddVariable(rvTFuelAllEndQtyOil)

      rvTFuelCstAllEndOil%VariableName = 'total_fuel_cost/oil/all_end_uses/quantity'
      rvTFuelCstAllEndOil%MetaType = 'units'
      rvTFuelCstAllEndOil%VariableType = '($/s)'
      rvTFuelCstAllEndOil%Description = 'Total cost of oil used on site.'
      Call AddVariable(rvTFuelCstAllEndOil)

      rvTFuelAllEndQtyPropane%VariableName = 'total_fuel_use/propane/all_end_uses/quantity'
      rvTFuelAllEndQtyPropane%MetaType = 'units'
      rvTFuelAllEndQtyPropane%VariableType = '(m3/s)'
      rvTFuelAllEndQtyPropane%Description = 'Total amount of propane used on site.'
      Call AddVariable(rvTFuelAllEndQtyPropane)
      
      rvTFuelCstAllEndPropane%VariableName = 'total_fuel_cost/propane/all_end_uses/quantity'
      rvTFuelCstAllEndPropane%MetaType = 'units'
      rvTFuelCstAllEndPropane%VariableType = '($/s)'
      rvTFuelCstAllEndPropane%Description = 'Total cost of propane used on site.'
      Call AddVariable(rvTFuelCstAllEndPropane)

      rvTFuelAllEndQtyMixedWood%VariableName = 'total_fuel_use/mixed_wood/all_end_uses/quantity'
      rvTFuelAllEndQtyMixedWood%MetaType = 'units'
      rvTFuelAllEndQtyMixedWood%VariableType = '(tonne/s)'
      rvTFuelAllEndQtyMixedWood%Description = 'Total amount of mixed wood used on site.'
      Call AddVariable(rvTFuelAllEndQtyMixedWood)

      rvTFuelCstAllEndMixedWood%VariableName = 'total_fuel_cost/mixed_wood/all_end_uses/quantity'
      rvTFuelCstAllEndMixedWood%MetaType = 'units'
      rvTFuelCstAllEndMixedWood%VariableType = '($/s)'
      rvTFuelCstAllEndMixedWood%Description = 'Total cost of mixed wood used on site.'
      Call AddVariable(rvTFuelCstAllEndMixedWood)
      
      rvTFuelAllEndQtyHardWood%VariableName = 'total_fuel_use/hard_wood/all_end_uses/quantity'
      rvTFuelAllEndQtyHardWood%MetaType = 'units'
      rvTFuelAllEndQtyHardWood%VariableType = '(tonne/s)'
      rvTFuelAllEndQtyHardWood%Description = 'Total amount of hard wood used on site.'
      Call AddVariable(rvTFuelAllEndQtyHardWood)

      rvTFuelCstAllEndHardWood%VariableName = 'total_fuel_cost/hard_wood/all_end_uses/quantity'
      rvTFuelCstAllEndHardWood%MetaType = 'units'
      rvTFuelCstAllEndHardWood%VariableType = '($/s)'
      rvTFuelCstAllEndHardWood%Description = 'Total cost of hard wood used on site.'
      Call AddVariable(rvTFuelCstAllEndHardWood)

      rvTFuelAllEndQtySoftWood%VariableName = 'total_fuel_use/soft_wood/all_end_uses/quantity'
      rvTFuelAllEndQtySoftWood%MetaType = 'units'
      rvTFuelAllEndQtySoftWood%VariableType = '(tonne/s)'
      rvTFuelAllEndQtySoftWood%Description = 'Total amount of soft wood used on site.'
      Call AddVariable(rvTFuelAllEndQtySoftWood)

      rvTFuelCstAllEndSoftWood%VariableName = 'total_fuel_cost/soft_wood/all_end_uses/quantity'
      rvTFuelCstAllEndSoftWood%MetaType = 'units'
      rvTFuelCstAllEndSoftWood%VariableType = '($/s)'
      rvTFuelCstAllEndSoftWood%Description = 'Total cost of soft wood used on site.'
      Call AddVariable(rvTFuelCstAllEndSoftWood)
      
      rvTFuelAllEndQtyWoodPellet%VariableName = 'total_fuel_use/wood_pellets/all_end_uses/quantity'
      rvTFuelAllEndQtyWoodPellet%MetaType = 'units'
      rvTFuelAllEndQtyWoodPellet%VariableType = '(tonne/s)'
      rvTFuelAllEndQtyWoodPellet%Description = 'Total amount of wood pellets used on site.'
      Call AddVariable(rvTFuelAllEndQtyWoodPellet)

      rvTFuelCstAllEndWoodPellet%VariableName = 'total_fuel_cost/wood_pellets/all_end_uses/quantity'
      rvTFuelCstAllEndWoodPellet%MetaType = 'units'
      rvTFuelCstAllEndWoodPellet%VariableType = '($/s)'
      rvTFuelCstAllEndWoodPellet%Description = 'Total cost of wood pellets used on site.'
      Call AddVariable(rvTFuelCstAllEndWoodPellet)
      
      rvTFuelQty%VariableName = 'total_fuel_use/*/*/quantity'
      rvTFuelQty%MetaType = 'units'
      rvTFuelQty%VariableType = '*** Type not defined ***'
      rvTFuelQty%Description = '*** Description not defined ***'
      Call AddVariable(rvTFuelQty)

      rvTFuelCst%VariableName = 'total_fuel_cost/*/*/quantity'
      rvTFuelCst%MetaType = 'units'
      rvTFuelCst%VariableType = '*** Type not defined ***'
      rvTFuelCst%Description = '*** Description not defined ***'
      Call AddVariable(rvTFuelCst)

      rvTEnergyQty%VariableName = 'total_fuel_use/test/*/*/energy_content'
      rvTEnergyQty%MetaType = 'units'
      rvTEnergyQty%VariableType = '(W)'
      rvTEnergyQty%Description = 'Energy content of fuel used on site'
      Call AddVariable(rvTEnergyQty)      
      
      !Claude - the following 16 variable's description differs from the original reporting
      rvTFuelQtyElec%VariableName = 'total_fuel_use/electricity/*/quantity'
      rvTFuelQtyElec%MetaType = 'units'
      rvTFuelQtyElec%VariableType = '(kWh/s)'
      rvTFuelQtyElec%Description = 'Total amount of electricity used.'
      Call AddVariable(rvTFuelQtyElec)

      rvTFuelCstElec%VariableName = 'total_fuel_cost/electricity/*/quantity'
      rvTFuelCstElec%MetaType = 'units'
      rvTFuelCstElec%VariableType = '($/s)'
      rvTFuelCstElec%Description = 'Total cost of electricity used.'
      Call AddVariable(rvTFuelCstElec)

      rvTFuelQtyNatGas%VariableName = 'total_fuel_use/natural_gas/*/quantity'
      rvTFuelQtyNatGas%MetaType = 'units'
      rvTFuelQtyNatGas%VariableType = '(m3/s)'
      rvTFuelQtyNatGas%Description = 'Total amount of natural gas used.'
      Call AddVariable(rvTFuelQtyNatGas)

      rvTFuelCstNatGas%VariableName = 'total_fuel_cost/natural_gas/*/quantity'
      rvTFuelCstNatGas%MetaType = 'units'
      rvTFuelCstNatGas%VariableType = '($/s)'
      rvTFuelCstNatGas%Description = 'Total cost of natural gas used.'
      Call AddVariable(rvTFuelCstNatGas)

      rvTFuelQtyOil%VariableName = 'total_fuel_use/oil/*/quantity'
      rvTFuelQtyOil%MetaType = 'units'
      rvTFuelQtyOil%VariableType = '(l/s)'
      rvTFuelQtyOil%Description = 'Total amount of oil used.'
      Call AddVariable(rvTFuelQtyOil)

      rvTFuelCstOil%VariableName = 'total_fuel_cost/oil/*/quantity'
      rvTFuelCstOil%MetaType = 'units'
      rvTFuelCstOil%VariableType = '($/s)'
      rvTFuelCstOil%Description = 'Total cost of oil used.'
      Call AddVariable(rvTFuelCstOil)

      rvTFuelQtyProp%VariableName = 'total_fuel_use/propane/*/quantity'
      rvTFuelQtyProp%MetaType = 'units'
      rvTFuelQtyProp%VariableType = '(m3/s)'
      rvTFuelQtyProp%Description = 'Total amount of propane used.'
      Call AddVariable(rvTFuelQtyProp)

      rvTFuelCstProp%VariableName = 'total_fuel_cost/propane/*/quantity'
      rvTFuelCstProp%MetaType = 'units'
      rvTFuelCstProp%VariableType = '($/s)'
      rvTFuelCstProp%Description = 'Total cost of propane used.'
      Call AddVariable(rvTFuelCstProp)

      rvTFuelQtyMixWood%VariableName = 'total_fuel_use/mixed_wood/*/quantity'
      rvTFuelQtyMixWood%MetaType = 'units'
      rvTFuelQtyMixWood%VariableType = '(tonne/s)'
      rvTFuelQtyMixWood%Description = 'Total amount of mixed wood used.'
      Call AddVariable(rvTFuelQtyMixWood)

      rvTFuelCstMixWood%VariableName = 'total_fuel_cost/mixed_wood/*/quantity'
      rvTFuelCstMixWood%MetaType = 'units'
      rvTFuelCstMixWood%VariableType = '($/s)'
      rvTFuelCstMixWood%Description = 'Total cost of mixed wood used.'
      Call AddVariable(rvTFuelCstMixWood)

      rvTFuelQtyHardWood%VariableName = 'total_fuel_use/hard_wood/*/quantity'
      rvTFuelQtyHardWood%MetaType = 'units'
      rvTFuelQtyHardWood%VariableType = '(tonne/s)'
      rvTFuelQtyHardWood%Description = 'Total amount of hard wood used.'
      Call AddVariable(rvTFuelQtyHardWood)

      rvTFuelCstHardWood%VariableName = 'total_fuel_cost/hard_wood/*/quantity'
      rvTFuelCstHardWood%MetaType = 'units'
      rvTFuelCstHardWood%VariableType = '($/s)'
      rvTFuelCstHardWood%Description = 'Total cost of hard wood used.'
      Call AddVariable(rvTFuelCstHardWood)

      rvTFuelQtySoftWood%VariableName = 'total_fuel_use/soft_wood/*/quantity'
      rvTFuelQtySoftWood%MetaType = 'units'
      rvTFuelQtySoftWood%VariableType = '(tonne/s)'
      rvTFuelQtySoftWood%Description = 'Total amount of soft wood used.'
      Call AddVariable(rvTFuelQtySoftWood)

      rvTFuelCstSoftWood%VariableName = 'total_fuel_cost/soft_wood/*/quantity'
      rvTFuelCstSoftWood%MetaType = 'units'
      rvTFuelCstSoftWood%VariableType = '($/s)'
      rvTFuelCstSoftWood%Description = 'Total cost of soft wood used.'
      Call AddVariable(rvTFuelCstSoftWood)

      rvTFuelQtyPellets%VariableName = 'total_fuel_use/wood_pellets/*/quantity'
      rvTFuelQtyPellets%MetaType = 'units'
      rvTFuelQtyPellets%VariableType = '(tonne/s)'
      rvTFuelQtyPellets%Description = 'Total amount of wood pellets used.'
      Call AddVariable(rvTFuelQtyPellets)

      rvTFuelCstPellets%VariableName = 'total_fuel_cost/wood_pellets/*/quantity'
      rvTFuelCstPellets%MetaType = 'units'
      rvTFuelCstPellets%VariableType = '($/s)'
      rvTFuelCstPellets%Description = 'Total cost of wood pellets used.'
      Call AddVariable(rvTFuelCstPellets)

      !Used by Solar.F
      rvBuildingGroundReflectivity%VariableName = 'Building/Ground_Reflectivity'
      rvBuildingGroundReflectivity%MetaType = 'units'
      rvBuildingGroundReflectivity%VariableType = 'dimensionless'
      rvBuildingGroundReflectivity%Description = 'Reflectivity of the ground for solar radiation'
      Call AddVariable(rvBuildingGroundReflectivity)

      rvClimateSnownDepth%VariableName = 'Climate/SnowDepth'
      rvClimateSnownDepth%MetaType = 'units'
      rvClimateSnownDepth%VariableType = 'cm'
      rvClimateSnownDepth%Description = 'Depth of the snow on the ground'
      Call AddVariable(rvClimateSnownDepth)

      ! Used by complex_fenestration.F
      rvCFCazimuth%VariableName = 'CFC/zone_*/surface_*/azimuth'
      rvCFCazimuth%MetaType = 'units'
      rvCFCazimuth%VariableType = 'degrees'
      rvCFCazimuth%Description = 'Solar wall azimuth'
      Call AddVariable(rvCFCazimuth)

      rvCFCelevation%VariableName = 'CFC/zone_*/surface_*/elevation'
      rvCFCelevation%MetaType = 'units'
      rvCFCelevation%VariableType = 'degrees'
      rvCFCelevation%Description = 'Solar elevation '
      Call AddVariable(rvCFCelevation)

      rvSolarIncidentDirect%VariableName = 'CFC/zone_*/surface_*/SolIncDir'
      rvSolarIncidentDirect%MetaType = 'units'
      rvSolarIncidentDirect%VariableType = 'W/m2'
      rvSolarIncidentDirect%Description = 'Solar incident direct radiation '
      Call AddVariable(rvSolarIncidentDirect)

      rvSolarIncidentDiff%VariableName = 'CFC/zone_*/surface_*/SolIncDiff'
      rvSolarIncidentDiff%MetaType = 'units'
      rvSolarIncidentDiff%VariableType = 'W/m2'
      rvSolarIncidentDiff%Description = 'Solar incident diffuse radiation '
      Call AddVariable(rvSolarIncidentDiff)

      rvCFCtranDir%VariableName = 'CFC/zone_*/surface_*/CFCtranDir'
      rvCFCtranDir%MetaType = 'units'
      rvCFCtranDir%VariableType = 'W/m2'
      rvCFCtranDir%Description = 'CFC direct transmitted '
      Call AddVariable(rvCFCtranDir)

      rvCFCtranDiff%VariableName = 'CFC/zone_*/surface_*/CFCtranDiff'
      rvCFCtranDiff%MetaType = 'units'
      rvCFCtranDiff%VariableType = 'W/m2'
      rvCFCtranDiff%Description = 'CFC diffuse transmitted '
      Call AddVariable(rvCFCtranDiff)

      rvCFCvertprofileangle%VariableName = 'CFC/zone_*/surface_*/CFCvertprofileangle'
      rvCFCvertprofileangle%MetaType = 'units'
      rvCFCvertprofileangle%VariableType = 'deg'
      rvCFCvertprofileangle%Description = 'CFC vertical profile angle '
      Call AddVariable(rvCFCvertprofileangle)

      !Used by water_tanks.F
      rvPltSDHWSumDHWTankFuel%VariableName = 'plant/SDHW_summary/DHW_tank_fuel'
      rvPltSDHWSumDHWTankFuel%MetaType = 'units'
      rvPltSDHWSumDHWTankFuel%VariableType = '(W)'
      rvPltSDHWSumDHWTankFuel%Description = 'Fuel burner input to the DHW tank'
      Call AddVariable(rvPltSDHWSumDHWTankFuel)

      rvPltSDHWSumDHWTankElec%VariableName = 'plant/SDHW_summary/DHW_tank_elec'
      rvPltSDHWSumDHWTankElec%MetaType = 'units'
      rvPltSDHWSumDHWTankElec%VariableType = '(W)'
      rvPltSDHWSumDHWTankElec%Description = 'Electric element input of the DHW tank'
      Call AddVariable(rvPltSDHWSumDHWTankElec)

      rvPltWaterTemp%VariableName = 'plant/*/misc_data/water_temperature'
      rvPltWaterTemp%MetaType = 'units'
      rvPltWaterTemp%VariableType = '(oC)'
      rvPltWaterTemp%Description = 'Hot water tank water temperature'
      Call AddVariable(rvPltWaterTemp)

      rvPltConn1HeatInject%VariableName = 'plant/*/misc_data/connection_1/heat_injection'
      rvPltConn1HeatInject%MetaType = 'units'
      rvPltConn1HeatInject%VariableType = '(W)'
      rvPltConn1HeatInject%Description = 'Hot water tank connection 1: heat injection'
      Call AddVariable(rvPltConn1HeatInject)

      rvPltConn2HeatInject%VariableName = 'plant/*/misc_data/connection_2/heat_injection'
      rvPltConn2HeatInject%MetaType = 'units'
      rvPltConn2HeatInject%VariableType = '(W)'
      rvPltConn2HeatInject%Description = 'Hot water tank connection 2: heat injection'
      Call AddVariable(rvPltConn2HeatInject)

      rvPltDomHotWtrVol%VariableName = 'plant/*/misc_data/domestic_hot_water/volumetric_draw'
      rvPltDomHotWtrVol%MetaType = 'units'
      rvPltDomHotWtrVol%VariableType = '(m3/s)'
      rvPltDomHotWtrVol%Description = 'Hot water tank: domestic hot water volumetric draw'
      Call AddVariable(rvPltDomHotWtrVol)

      rvPltDomHotWtrMkTemp%VariableName = 'plant/*/misc_data/domestic_hot_water/make_up_temperature'
      rvPltDomHotWtrMkTemp%MetaType = 'units'
      rvPltDomHotWtrMkTemp%VariableType = '(oC)'
      rvPltDomHotWtrMkTemp%Description = 'Hot water tank: water make-up temperature'
      Call AddVariable(rvPltDomHotWtrMkTemp)

      rvPltDomHotWtrThermal%VariableName = 'plant/*/misc_data/domestic_hot_water/thermal_load'
      rvPltDomHotWtrThermal%MetaType = 'units'
      rvPltDomHotWtrThermal%VariableType = '(W)'
      rvPltDomHotWtrThermal%Description = 'Hot water tank: domestic hot water thermal load'
      Call AddVariable(rvPltDomHotWtrThermal)

      rvPltBurnOut%VariableName = 'plant/*/misc_data/burner_heat_production'
      rvPltBurnOut%MetaType = 'units'
      rvPltBurnOut%VariableType = '(W)'
      rvPltBurnOut%Description = 'Hot water tank: rate of burner heat production'
      Call AddVariable(rvPltBurnOut)

      rvPltFuelFlow%VariableName = 'plant/*/misc_data/fuel_flow'
      rvPltFuelFlow%MetaType = 'units'
      rvPltFuelFlow%VariableType = '(kg/s)'
      rvPltFuelFlow%Description = 'Hot water tank: burner fuel flow rate'
      Call AddVariable(rvPltFuelFlow)

      rvPltFuelFlowVolSTP%VariableName = 'plant/*/misc_data/fuel_flow_vol_STP'
      rvPltFuelFlowVolSTP%MetaType = 'units'
      rvPltFuelFlowVolSTP%VariableType = '(slpm)'
      rvPltFuelFlowVolSTP%Description = 'Hot water tank: burner fuel flow rate'
      Call AddVariable(rvPltFuelFlowVolSTP)

      rvPltFuelFlowLHVVal%VariableName = 'plant/*/misc_data/fuel_flow_LHV_value'
      rvPltFuelFlowLHVVal%MetaType = 'units'
      rvPltFuelFlowLHVVal%VariableType = '(W)'
      rvPltFuelFlowLHVVal%Description = 'Hot water tank: LHV of fuel used'
      Call AddVariable(rvPltFuelFlowLHVVal)

      rvPltAirFlow%VariableName = 'plant/*/misc_data/air_flow'
      rvPltAirFlow%MetaType = 'units'
      rvPltAirFlow%VariableType = '(kg/s)'
      rvPltAirFlow%Description = 'Hot water tank: burner air flow rate'
      Call AddVariable(rvPltAirFlow)

      rvPltExhaustFlow%VariableName = 'plant/*/misc_data/exhaust_flow'
      rvPltExhaustFlow%MetaType = 'units'
      rvPltExhaustFlow%VariableType = '(kg/s)'
      rvPltExhaustFlow%Description = 'Hot water tank: burner exhaust flow rate'
      Call AddVariable(rvPltExhaustFlow)

      rvPltEInput%VariableName = 'plant/*/misc_data/energy_input'
      rvPltEInput%MetaType = 'units'
      rvPltEInput%VariableType = '(W)'
      rvPltEInput%Description = 'Hot water tank: Energy requirements'
      Call AddVariable(rvPltEInput)

      rvPltHTransWater%VariableName = 'plant/*/misc_data/heat_transfer_to_water'
      rvPltHTransWater%MetaType = 'units'
      rvPltHTransWater%VariableType = '(W)'
      rvPltHTransWater%Description = 'Hot water tank: rate of heat transfer to water'
      Call AddVariable(rvPltHTransWater)

      rvPltJacketLoss%VariableName = 'plant/*/misc_data/jacket_losses'
      rvPltJacketLoss%MetaType = 'units'
      rvPltJacketLoss%VariableType = '(W)'
      rvPltJacketLoss%Description = 'Hot water tank: rate of heat loss to surroundings'
      Call AddVariable(rvPltJacketLoss)

      rvPltHeatDump%VariableName = 'plant/*/misc_data/heat_dump'
      rvPltHeatDump%MetaType = 'units'
      rvPltHeatDump%VariableType = '(W)'
      rvPltHeatDump%Description = 'Hot water tank: rate at which heat is "dumped" from '// &
         'tank to prevent water from boiling.'
      Call AddVariable(rvPltHeatDump)


      !Used by tank_intake_hx.F
      rvPltTHeatTrans%VariableName = 'plant/*/misc_data/total_heat_transfer'
      rvPltTHeatTrans%MetaType = 'units'
      rvPltTHeatTrans%VariableType = 'W'
      rvPltTHeatTrans%Description = 'Heat transfer from coil to tank water'
      Call AddVariable(rvPltTHeatTrans)

      rvPltTHeatTransJacketHeatLoss%VariableName = 'plant/*/misc_data/tank_jacket_heat_loss'
      rvPltTHeatTransJacketHeatLoss%MetaType = 'units'
      rvPltTHeatTransJacketHeatLoss%VariableType = 'W'
      rvPltTHeatTransJacketHeatLoss%Description = 'Tank heat loss to surroundings'
      Call AddVariable(rvPltTHeatTransJacketHeatLoss)

      rvPltTHeatTransTubeSideHi%VariableName = 'plant/*/misc_data/tube_side_hi'
      rvPltTHeatTransTubeSideHi%MetaType = 'units'
      rvPltTHeatTransTubeSideHi%VariableType = 'W/m2-K'
      rvPltTHeatTransTubeSideHi%Description = 'Inside coil heat transfer coeff'
      Call AddVariable(rvPltTHeatTransTubeSideHi)

      rvPltTHeatTransTankSideHo%VariableName = 'plant/*/misc_data/tank_side_ho'
      rvPltTHeatTransTankSideHo%MetaType = 'units'
      rvPltTHeatTransTankSideHo%VariableType = 'W/m2-K'
      rvPltTHeatTransTankSideHo%Description = 'Inside coil heat transfer coeff'
      Call AddVariable(rvPltTHeatTransTankSideHo)

      rvPltTHeatTransIntankCoilTUValue%VariableName = 'plant/*/misc_data/intank_coil_total_Uvalue'
      rvPltTHeatTransIntankCoilTUValue%MetaType = 'units'
      rvPltTHeatTransIntankCoilTUValue%VariableType = 'W/m2-K'
      rvPltTHeatTransIntankCoilTUValue%Description = 'coil total heat transfer coeff'
      Call AddVariable(rvPltTHeatTransIntankCoilTUValue)


      !Used by stratified_tank.F;stratified_tank_2HX.F;stratified_tank_1HX.F
      rvPltAvgTemp%VariableName = 'plant/*/misc_data/average_temperature'
      rvPltAvgTemp%MetaType = 'units'
      rvPltAvgTemp%VariableType = '(C)'
      rvPltAvgTemp%Description = 'Stratified tank : Average temperature'
      Call AddVariable(rvPltAvgTemp)


      !Used by solar_collectors.F
      rvPltSDHWSumRecH%VariableName = 'plant/SDHW_summary/recovered_heat'
      rvPltSDHWSumRecH%MetaType = 'units'
      rvPltSDHWSumRecH%VariableType = '(W)'
      rvPltSDHWSumRecH%Description = 'SHDW system: Collector net heat gain'
      Call AddVariable(rvPltSDHWSumRecH)

      rvPltSDHWsumAvailSolEn%VariableName = 'plant/SDHW_summary/available_solar_energy'
      rvPltSDHWsumAvailSolEn%MetaType = 'units'
      rvPltSDHWsumAvailSolEn%VariableType = '(W)'
      rvPltSDHWsumAvailSolEn%Description = 'SDHW system: insolation on collector'
      Call AddVariable(rvPltSDHWsumAvailSolEn)

      !Used by pcomp2.F
      rvPltQAddedH%VariableName = 'plant/*/misc_data/Q_added_heat'
      rvPltQAddedH%MetaType = 'units'
      rvPltQAddedH%VariableType = '(W)'
      rvPltQAddedH%Description = 'Heat added to boiler'
      Call AddVariable(rvPltQAddedH)

      rvPltWCHPumpEInput%VariableName = 'plant/*/misc_data/energy_input'
      rvPltWCHPumpEInput%MetaType = 'units'
      rvPltWCHPumpEInput%VariableType = '(W)'
      rvPltWCHPumpEInput%Description = 'WCH pump: Energy input'
      Call AddVariable(rvPltWCHPumpEInput)

      rvPltHOut%VariableName = 'plant/*/misc_data/Heat_Out'
      rvPltHOut%MetaType = 'units'
      rvPltHOut%VariableType = '(W)'
      rvPltHOut%Description = 'ASHP: Heat Output'
      Call AddVariable(rvPltHOut)

      rvPltCOP%VariableName = 'plant/*/misc_data/COP'
      rvPltCOP%MetaType = 'units'
      rvPltCOP%VariableType = '(-)'
      rvPltCOP%Description = 'ASHP: COP'
      Call AddVariable(rvPltCOP)

      rvPltTambient%VariableName = 'plant/*/misc_data/Tambient'
      rvPltTambient%MetaType = 'units'
      rvPltTambient%VariableType = '(degC)'
      rvPltTambient%Description = 'ASHP: Ambient Temp'
      Call AddVariable(rvPltTambient)

      rvPltDeviceONOFF%VariableName = 'plant/*/misc_data/DeviceONOFF'
      rvPltDeviceONOFF%MetaType = 'units'
      rvPltDeviceONOFF%VariableType = '(-)'
      rvPltDeviceONOFF%Description = 'ASHP: ON-OFF status'
      Call AddVariable(rvPltDeviceONOFF)

      rvPltReturnTSP%VariableName = 'plant/*/misc_data/Return T SP'
      rvPltReturnTSP%MetaType = 'units'
      rvPltReturnTSP%VariableType = '(degC)'
      rvPltReturnTSP%Description = 'ASHP: Return Temp Set Pt.'
      Call AddVariable(rvPltReturnTSP)

      rvPltRealPow%VariableName = 'plant/*/misc_data/Real Power'
      rvPltRealPow%MetaType = 'units'
      rvPltRealPow%VariableType = '(W)'
      rvPltRealPow%Description = 'ASHP: Real Power Demand'
      Call AddVariable(rvPltRealPow)

      rvPltReacPow%VariableName = 'plant/*/misc_data/Reac. Power'
      rvPltReacPow%MetaType = 'units'
      rvPltReacPow%VariableType = '(VAr)'
      rvPltReacPow%Description = 'ASHP: Reactive Power Demand'
      Call AddVariable(rvPltReacPow)

      rvPltApparPow%VariableName = 'plant/*/misc_data/Appar. Power'
      rvPltApparPow%MetaType = 'units'
      rvPltApparPow%VariableType = '(VA)'
      rvPltApparPow%Description = 'ASHP: Apparent Power Demand'
      Call AddVariable(rvPltApparPow)

      rvPltDefrostStat%VariableName = 'plant/*/misc_data/desfrost_status'
      rvPltDefrostStat%MetaType = 'units'
      rvPltDefrostStat%VariableType = '(-)'
      rvPltDefrostStat%Description = 'ASHP: Defrost Status 0-off, 1-on, -1-lockout'
      Call AddVariable(rvPltDefrostStat)

      rvPltDHWDrawStoch%VariableName = 'plant/*/misc_data/DHW_draw_stoch'
      rvPltDHWDrawStoch%MetaType = 'units'
      rvPltDHWDrawStoch%VariableType = '(kg/s)'
      rvPltDHWDrawStoch%Description = 'Hot water draw rate'
      Call AddVariable(rvPltDHWDrawStoch)

      rvPltDHWDrawStochTp%VariableName = 'plant/*/misc_data/DHW_draw_stock/type_*'
      rvPltDHWDrawStochTp%MetaType = 'units'
      rvPltDHWDrawStochTp%VariableType = '(kg/s)'
      rvPltDHWDrawStochTp%Description = 'Draw for type'
      Call AddVariable(rvPltDHWDrawStochTp)


      !Used by NCHE-steady_state.F
      rvPltHXchgEff%VariableName = 'plant/*/heat_exchanger/effectiveness'
      rvPltHXchgEff%MetaType = 'units'
      rvPltHXchgEff%VariableType = '(-)'
      rvPltHXchgEff%Description = 'NCHE: Heat exchanger effectiveness'
      Call AddVariable(rvPltHXchgEff)

      rvPltHXchgHTrans%VariableName = 'plant/*/heat_exchanger/heat_transfer'
      rvPltHXchgHTrans%MetaType = 'units'
      rvPltHXchgHTrans%VariableType = '(W)'
      rvPltHXchgHTrans%Description = 'NCHE: Heat exchanger heat transfer rate'
      Call AddVariable(rvPltHXchgHTrans)

      rvPltHXchgHFlowRate%VariableName = 'plant/*/heat_exchanger/hot/flow_rate'
      rvPltHXchgHFlowRate%MetaType = 'units'
      rvPltHXchgHFlowRate%VariableType = '(kg/s)'
      rvPltHXchgHFlowRate%Description = 'NCHE: flow rate of water at HX inlet from solar collector'
      Call AddVariable(rvPltHXchgHFlowRate)

      rvPltHXchgCFlowRate%VariableName = 'plant/*/heat_exchanger/cold/flow_rate'
      rvPltHXchgCFlowRate%MetaType = 'units'
      rvPltHXchgCFlowRate%VariableType = '(kg/s)'
      rvPltHXchgCFlowRate%Description = 'NCHE: flow rate of water at HX inlet from storage tank'
      Call AddVariable(rvPltHXchgCFlowRate)

      rvPltHXchgHTempIn%VariableName = 'plant/*/heat_exchanger/hot/temperature_in'
      rvPltHXchgHTempIn%MetaType = 'units'
      rvPltHXchgHTempIn%VariableType = '(oC)'
      rvPltHXchgHTempIn%Description = 'NCHE: temperature of fluid at HX inlet from solar collector'
      Call AddVariable(rvPltHXchgHTempIn)

      rvPltHXchgHTempOut%VariableName = 'plant/*/heat_exchanger/hot/temperature_out'
      rvPltHXchgHTempOut%MetaType = 'units'
      rvPltHXchgHTempOut%VariableType = '(oC)'
      rvPltHXchgHTempOut%Description = 'NCHE: temperature of fluid at return to solar collector'
      Call AddVariable(rvPltHXchgHTempOut)

      rvPltHXchgCTempIn%VariableName = 'plant/*/heat_exchanger/cold/temperature_in'
      rvPltHXchgCTempIn%MetaType = 'units'
      rvPltHXchgCTempIn%VariableType = '(oC)'
      rvPltHXchgCTempIn%Description = 'NCHE: temperature of water at HX inlet from storage tank'
      Call AddVariable(rvPltHXchgCTempIn)

      rvPltHEXchgCTempOut%VariableName = 'plant/*/heat_exchanger/cold/temperature_out'
      rvPltHEXchgCTempOut%MetaType = 'units'
      rvPltHEXchgCTempOut%VariableType = '(oC)'
      rvPltHEXchgCTempOut%Description = 'NCHE: temperature of water at return to storage tank'
      Call AddVariable(rvPltHEXchgCTempOut)

      !Used by h3k_plant_postprocess.F
      rvPltHeatTransAir%VariableName = 'plant/*/misc_data/heat_transfer_to_air'
      rvPltHeatTransAir%MetaType = 'units'
      rvPltHeatTransAir%VariableType = '(W)'
      rvPltHeatTransAir%Description = ''
      Call AddVariable(rvPltHeatTransAir)

      rvPltAirHumRatio%VariableName = 'plant/*/misc_data/air_humidity_ratio'
      rvPltAirHumRatio%MetaType = 'units'
      rvPltAirHumRatio%VariableType = '(kg/kg)'
      rvPltAirHumRatio%Description = ''
      Call AddVariable(rvPltAirHumRatio)

      rvPltHTransWater2%VariableName = 'plant/*/misc_data/heat_transfer_to_water'
      rvPltHTransWater2%MetaType = 'units'
      rvPltHTransWater2%VariableType = '(W)'
      rvPltHTransWater2%Description = ''
      Call AddVariable(rvPltHTransWater2)

      !Used by the FC_components.F
      rvPltQElecDemand%VariableName = 'plant/*/misc_data/Q_electric_demand'
      rvPltQElecDemand%MetaType = 'units'
      rvPltQElecDemand%VariableType = '(WattsToGJ)'
      rvPltQElecDemand%Description = ''
      Call AddVariable(rvPltQElecDemand)

      rvPltQElecNet%VariableName = 'plant/*/misc_data/Q_electric_net'
      rvPltQElecNet%MetaType = 'units'
      rvPltQElecNet%VariableType = '(WattsToGJ)'
      rvPltQElecNet%Description = ''
      Call AddVariable(rvPltQElecNet)

      rvPltQElecParasitic%VariableName = 'plant/*/misc_data/Q_electric_parasitic'
      rvPltQElecParasitic%MetaType = 'units'
      rvPltQElecParasitic%VariableType = '(WattsToGJ)'
      rvPltQElecParasitic%Description = ''
      Call AddVariable(rvPltQElecParasitic)

      rvPltQThermalNet%VariableName = 'plant/*/misc_data/Q_thermal_net'
      rvPltQThermalNet%MetaType = 'units'
      rvPltQThermalNet%VariableType = '(WattsToGJ)'
      rvPltQThermalNet%Description = ''
      Call AddVariable(rvPltQThermalNet)

      rvPltFuelHHV%VariableName = 'plant/*/misc_data/Fuel_HHV'
      rvPltFuelHHV%MetaType = 'units'
      rvPltFuelHHV%VariableType = '(WattsToGJ)'
      rvPltFuelHHV%Description = ''
      Call AddVariable(rvPltFuelHHV)

      rvPltElecEffFC%VariableName = 'plant/*/misc_data/Electric_efficiency_FC'
      rvPltElecEffFC%MetaType = 'units'
      rvPltElecEffFC%VariableType = '(%)'
      rvPltElecEffFC%Description = ''
      Call AddVariable(rvPltElecEffFC)

      rvPltElecEffBOP%VariableName = 'plant/*/misc_data/Electric_efficiency_BOP'
      rvPltElecEffBOP%MetaType = 'units'
      rvPltElecEffBOP%VariableType = '(%)'
      rvPltElecEffBOP%Description = ''
      Call AddVariable(rvPltElecEffBOP)

      rvPltCogEff%VariableName = 'plant/*/misc_data/Cogeneration_efficiency'
      rvPltCogEff%MetaType = 'units'
      rvPltCogEff%VariableType = '(%)'
      rvPltCogEff%Description = ''
      Call AddVariable(rvPltCogEff)

      rvPltFuelFlowMass%VariableName = 'plant/*/misc_data/fuel_flow/mass'
      rvPltFuelFlowMass%MetaType = 'units'
      rvPltFuelFlowMass%VariableType = '(KgPerStoKg)'
      rvPltFuelFlowMass%Description = ''
      Call AddVariable(rvPltFuelFlowMass)

      rvPltFuelFlowVol%VariableName = 'plant/*/misc_data/fuel_flow/volume'
      rvPltFuelFlowVol%MetaType = 'units'
      rvPltFuelFlowVol%VariableType = '(slpm)'
      rvPltFuelFlowVol%Description = ''
      Call AddVariable(rvPltFuelFlowVol)

      rvPltAirFlowMass%VariableName = 'plant/*/misc_data/air_flow/mass'
      rvPltAirFlowMass%MetaType = 'units'
      rvPltAirFlowMass%VariableType = '(KgPerStoKg)'
      rvPltAirFlowMass%Description = ''
      Call AddVariable(rvPltAirFlowMass)

      rvPltAirFlowVol%VariableName = 'plant/*/misc_data/air_flow/volume'
      rvPltAirFlowVol%MetaType = 'units'
      rvPltAirFlowVol%VariableType = '(slpm)'
      rvPltAirFlowVol%Description = ''
      Call AddVariable(rvPltAirFlowVol)

      rvPltAirFlowStoi%VariableName = 'plant/*/misc_data/air_flow/stoicheometry'
      rvPltAirFlowStoi%MetaType = 'units'
      rvPltAirFlowStoi%VariableType = '(-)'
      rvPltAirFlowStoi%Description = ''
      Call AddVariable(rvPltAirFlowStoi)

      rvPltWaterFlowVol%VariableName = 'plant/*/misc_data/water_flow/volume'
      rvPltWaterFlowVol%MetaType = 'units'
      rvPltWaterFlowVol%VariableType = '(slpm)'
      rvPltWaterFlowVol%Description = ''
      Call AddVariable(rvPltWaterFlowVol)

      rvPltExhaustFlowMass%VariableName = 'plant/*/misc_data/exhaust_flow/mass'
      rvPltExhaustFlowMass%MetaType = 'units'
      rvPltExhaustFlowMass%VariableType = '(KgPerStoKg)'
      rvPltExhaustFlowMass%Description = ''
      Call AddVariable(rvPltExhaustFlowMass)

      rvPltExhaustFlowVol%VariableName = 'plant/*/misc_data/exhaust_flow/volume'
      rvPltExhaustFlowVol%MetaType = 'units'
      rvPltExhaustFlowVol%VariableType = '(slpm)'
      rvPltExhaustFlowVol%Description = ''
      Call AddVariable(rvPltExhaustFlowVol)

      rvPltExhaustFlowTemp%VariableName = 'plant/*/misc_data/exhaust_flow/temperature'
      rvPltExhaustFlowTemp%MetaType = 'units'
      rvPltExhaustFlowTemp%VariableType = '(oC)'
      rvPltExhaustFlowTemp%Description = ''
      Call AddVariable(rvPltExhaustFlowTemp)

      rvPltHExchgExitTemp%VariableName = 'plant/*/misc_data/heat_exchanger/exit_temperature'
      rvPltHExchgExitTemp%MetaType = 'units'
      rvPltHExchgExitTemp%VariableType = '(oC)'
      rvPltHExchgExitTemp%Description = ''
      Call AddVariable(rvPltHExchgExitTemp)

      rvPltHExchgUAVal%VariableName = 'plant/*/misc_data/heat_exchanger/UA_value'
      rvPltHExchgUAVal%MetaType = 'units'
      rvPltHExchgUAVal%VariableType = '(Watts/oC)'
      rvPltHExchgUAVal%Description = ''
      Call AddVariable(rvPltHExchgUAVal)

      !Used by mains_temp_draw_profiles.F
      rvPltDHWTermalLoad%VariableName = 'plant/*/misc_data/DHW_thermal_load'
      rvPltDHWTermalLoad%MetaType = 'units'
      rvPltDHWTermalLoad%VariableType = '(W)'
      rvPltDHWTermalLoad%Description = 'Make-up water: thermal load associated with DHW service'
      Call AddVariable(rvPltDHWTermalLoad)

      !Used by Hydrogen_PEMFC.F
      rvDebugH2LHV%VariableName = 'debug/H2_PEMFC/H2/LHV'
      rvDebugH2LHV%MetaType = 'units'
      rvDebugH2LHV%VariableType = '(J/kg)'
      rvDebugH2LHV%Description = 'Heating value of hydrogen'
      Call AddVariable(rvDebugH2LHV)

      rvPltAvrEffElec%VariableName = 'plant/*/misc_data/average/efficiency/electric'
      rvPltAvrEffElec%MetaType = 'units'
      rvPltAvrEffElec%VariableType = '(-)'
      rvPltAvrEffElec%Description = 'Hydrogen PEMFC: time step-averaged electric efficiency'
      Call AddVariable(rvPltAvrEffElec)

      rvPltAvrEffThermal%VariableName = 'plant/*/misc_data/average/efficiency/thermal'
      rvPltAvrEffThermal%MetaType = 'units'
      rvPltAvrEffThermal%VariableType = '(-)'
      rvPltAvrEffThermal%Description = 'Hydrogen PEMFC: time step-averaged thermal efficiency'
      Call AddVariable(rvPltAvrEffThermal)

      rvPltAvrEffCogen%VariableName = 'plant/*/misc_data/average/efficiency/cogeneration'
      rvPltAvrEffCogen%MetaType = 'units'
      rvPltAvrEffCogen%VariableType = '(-)'
      rvPltAvrEffCogen%Description = 'Hydrogen PEMFC: time step-averaged cogeneration efficiency'
      Call AddVariable(rvPltAvrEffCogen)

      rvPltHydroSupMassFlowRate%VariableName = 'plant/*/misc_data/hydrogen/supply/mass_flow_rate'
      rvPltHydroSupMassFlowRate%MetaType = 'units'
      rvPltHydroSupMassFlowRate%VariableType = '(kg/s)'
      rvPltHydroSupMassFlowRate%Description = 'Hydrogen PEMFC: mass flow rate of supplied hydrogen'
      Call AddVariable(rvPltHydroSupMassFlowRate)

      rvPltHydroSupLHVFlowRate%VariableName = 'plant/*/misc_data/hydrogen/supply/LHV_flow_rate'
      rvPltHydroSupLHVFlowRate%MetaType = 'units'
      rvPltHydroSupLHVFlowRate%VariableType = '(W)'
      rvPltHydroSupLHVFlowRate%Description = 'Hydrogen PEMFC: LHV flow rate of supplied hydrogen'
      Call AddVariable(rvPltHydroSupLHVFlowRate)

      rvPltHydroDemMassFlowRate%VariableName = 'plant/*/misc_data/hydrogen/demand/mass_flow_rate'
      rvPltHydroDemMassFlowRate%MetaType = 'units'
      rvPltHydroDemMassFlowRate%VariableType = '(kg/s)'
      rvPltHydroDemMassFlowRate%Description = 'Hydrogen PEMFC: hydrogen mass flow rate requested by unit'
      Call AddVariable(rvPltHydroDemMassFlowRate)

      rvPltHydroDemLHVFlowRate%VariableName = 'plant/*/misc_data/hydrogen/demand/LHV_flow_rate'
      rvPltHydroDemLHVFlowRate%MetaType = 'units'
      rvPltHydroDemLHVFlowRate%VariableType = '(W)'
      rvPltHydroDemLHVFlowRate%Description = 'Hydrogen PEMFC: LHV flow rate hydrogen requested by unit'
      Call AddVariable(rvPltHydroDemLHVFlowRate)

      rvPltStackVolt%VariableName = 'plant/*/misc_data/stack/voltage'
      rvPltStackVolt%MetaType = 'units'
      rvPltStackVolt%VariableType = '(V)'
      rvPltStackVolt%Description = 'Hydrogen PEMFC: Stack operating voltage'
      Call AddVariable(rvPltStackVolt)

      rvPltStackCurrent%VariableName = 'plant/*/misc_data/stack/current'
      rvPltStackCurrent%MetaType = 'units'
      rvPltStackCurrent%VariableType = '(amps)'
      rvPltStackCurrent%Description = 'Hydrogen PEMFC: Stack current, averaged over time step'
      Call AddVariable(rvPltStackCurrent)

      rvPltAvgPowAC%VariableName = 'plant/*/misc_data/average/power/AC'
      rvPltAvgPowAC%MetaType = 'units'
      rvPltAvgPowAC%VariableType = '(W)'
      rvPltAvgPowAC%Description = 'Hydrogen PEMFC: AC power generation, averaged over time step'
      Call AddVariable(rvPltAvgPowAC)

      rvPltAvrPowDC%VariableName = 'plant/*/misc_data/average/power/DC'
      rvPltAvrPowDC%MetaType = 'units'
      rvPltAvrPowDC%VariableType = '(W)'
      rvPltAvrPowDC%Description = 'Hydrogen PEMFC: DC power generation, averaged over time step'
      Call AddVariable(rvPltAvrPowDC)

      rvPltAvgHOutput%VariableName = 'plant/*/misc_data/average/heat_output'
      rvPltAvgHOutput%MetaType = 'units'
      rvPltAvgHOutput%VariableType = '(W)'
      rvPltAvgHOutput%Description = 'Hydrogen PEMFC: thermal output, averaged over time step'
      Call AddVariable(rvPltAvgHOutput)

      rvPltAvgSkinLoss%VariableName = 'plant/*/misc_data/average/skin_losses'
      rvPltAvgSkinLoss%MetaType = 'units'
      rvPltAvgSkinLoss%VariableType = '(W)'
      rvPltAvgSkinLoss%Description = 'Hydrogen PEMFC: skin losses, averaged over timestep'
      Call AddVariable(rvPltAvgSkinLoss)

      rvPltCtrlScheme%VariableName = 'plant/*/misc_data/control/scheme'
      rvPltCtrlScheme%MetaType = 'units'
      rvPltCtrlScheme%VariableType = '(-)'
      rvPltCtrlScheme%Description = 'Hydrogen PEMFC: control scheme'
      Call AddVariable(rvPltCtrlScheme)

      rvPltCtrlSignal%VariableName = 'plant/*/misc_data/control/signal'
      rvPltCtrlSignal%MetaType = 'units'
      rvPltCtrlSignal%VariableType = '(-)'
      rvPltCtrlSignal%Description = 'Hydrogen PEMFC: signal from external controller'
      Call AddVariable(rvPltCtrlSignal)

      rvPltOperModeStandBy%VariableName = 'plant/*/misc_data/operating_mode/standby'
      rvPltOperModeStandBy%MetaType = 'units'
      rvPltOperModeStandBy%VariableType = '(s)'
      rvPltOperModeStandBy%Description = 'Hydrogen PEMFC: time spent in standby'
      Call AddVariable(rvPltOperModeStandBy)

      rvPltOperModeStartup%VariableName = 'plant/*/misc_data/operating_mode/startup'
      rvPltOperModeStartup%MetaType = 'units'
      rvPltOperModeStartup%VariableType = '(s)'
      rvPltOperModeStartup%Description = 'Hydrogen PEMFC: time spent in startup'
      Call AddVariable(rvPltOperModeStartup)

      rvPltOperModeNormOp%VariableName = 'plant/*/misc_data/operating_mode/normal_operation'
      rvPltOperModeNormOp%MetaType = 'units'
      rvPltOperModeNormOp%VariableType = '(s)'
      rvPltOperModeNormOp%Description = 'Hydrogen PEMFC: time spent in normal operation'
      Call AddVariable(rvPltOperModeNormOp)

      rvPltOperModeCoolDown%VariableName = 'plant/*/misc_data/operating_mode/cool_down'
      rvPltOperModeCoolDown%MetaType = 'units'
      rvPltOperModeCoolDown%VariableType = '(s)'
      rvPltOperModeCoolDown%Description = 'Hydrogen PEMFC: time spent in cool down'
      Call AddVariable(rvPltOperModeCoolDown)


      !Used by Hydrogen_MH_store.F
      rvPltHydroFreeKMol%VariableName = 'plant/*/misc_data/hydrogen/free/kmol'
      rvPltHydroFreeKMol%MetaType = 'units'
      rvPltHydroFreeKMol%VariableType = '(kmol)'
      rvPltHydroFreeKMol%Description = 'Amount of hydrogen (H2) in gaseous form in cylinder'
      Call AddVariable(rvPltHydroFreeKMol)

      rvPltHydroFreeKg%VariableName = 'plant/*/misc_data/hydrogen/free/kg'
      rvPltHydroFreeKg%MetaType = 'units'
      rvPltHydroFreeKg%VariableType = '(kg)'
      rvPltHydroFreeKg%Description = 'Amount of hydrogen (H2) in gaseous form in cylinder'
      Call AddVariable(rvPltHydroFreeKg)

      rvPltHydroAbsorbedKmol%VariableName = 'plant/*/misc_data/hydrogen/absorbed/kmol'
      rvPltHydroAbsorbedKmol%MetaType = 'units'
      rvPltHydroAbsorbedKmol%VariableType = '(kmol)'
      rvPltHydroAbsorbedKmol%Description = 'Amount of hydrogen (H) absorbed in cylinder'
      Call AddVariable(rvPltHydroAbsorbedKmol)

      rvPltHydroAbsorbedKg%VariableName = 'plant/*/misc_data/hydrogen/absorbed/kg'
      rvPltHydroAbsorbedKg%MetaType = 'units'
      rvPltHydroAbsorbedKg%VariableType = '(kg)'
      rvPltHydroAbsorbedKg%Description = 'Amount of hydrogen (H) absorbed in cylinder'
      Call AddVariable(rvPltHydroAbsorbedKg)

      rvPltHydroVentKmol%VariableName = 'plant/*/misc_data/hydrogen/vent/kmol'
      rvPltHydroVentKmol%MetaType = 'units'
      rvPltHydroVentKmol%VariableType = '(kmol/s)'
      rvPltHydroVentKmol%Description = 'Rate at which hydrogen is vented from cylinder'
      Call AddVariable(rvPltHydroVentKmol)

      rvPltHydroChargeKmol%VariableName = 'plant/*/misc_data/hydrogen/charge/kmol'
      rvPltHydroChargeKmol%MetaType = 'units'
      rvPltHydroChargeKmol%VariableType = '(kmol/s)'
      rvPltHydroChargeKmol%Description = 'Rate at which hydrogen is charged to cylinder'
      Call AddVariable(rvPltHydroChargeKmol)

      rvPltHydroDisKmol%VariableName = 'plant/*/misc_data/hydrogen/discharge/kmol'
      rvPltHydroDisKmol%MetaType = 'units'
      rvPltHydroDisKmol%VariableType = '(kmol/s)'
      rvPltHydroDisKmol%Description = 'Rate at which hydrogen is discharged from cylinder'
      Call AddVariable(rvPltHydroDisKmol)

      rvPltCylPressure%VariableName = 'plant/*/misc_data/pressure'
      rvPltCylPressure%MetaType = 'units'
      rvPltCylPressure%VariableType = '(kPa)'
      rvPltCylPressure%Description = 'Cylinder pressure'
      Call AddVariable(rvPltCylPressure)

      rvPltDemandExpl%VariableName = 'plant/*/misc_data/demand/explicit'
      rvPltDemandExpl%MetaType = 'units'
      rvPltDemandExpl%VariableType = '(kmol/s)'
      rvPltDemandExpl%Description = 'Hydrogen requirements of components in plant network'
      Call AddVariable(rvPltDemandExpl)

      rvPltDemandImpl%VariableName = 'plant/*/misc_data/demand/implicit'
      rvPltDemandImpl%MetaType = 'units'
      rvPltDemandImpl%VariableType = '(kmol/s)'
      rvPltDemandImpl%Description = 'Hydrogen requirements of devices not necessarly contained within the plant network'
      Call AddVariable(rvPltDemandImpl)

      rvPltHRecovery%VariableName = 'plant/*/misc_data/heat_recovery'
      rvPltHRecovery%MetaType = 'units'
      rvPltHRecovery%VariableType = '(W)'
      rvPltHRecovery%Description = 'Heat transferred to cooling water (+ive is transfer from MH unit to water)'
      Call AddVariable(rvPltHRecovery)

      rvPltHLoss%VariableName = 'plant/*/misc_data/heat_loss'
      rvPltHLoss%MetaType = 'units'
      rvPltHLoss%VariableType = '(W)'
      rvPltHLoss%Description = 'Heat lost to surroundings'
      Call AddVariable(rvPltHLoss)

      rvPltHReaction%VariableName = 'plant/*/misc_data/heat_of_reaction'
      rvPltHReaction%MetaType = 'units'
      rvPltHReaction%VariableType = '(W)'
      rvPltHReaction%Description = 'Heat generated during absorption of hydrogen'
      Call AddVariable(rvPltHReaction)

      rvPltContainTemp%VariableName = 'plant/*/misc_data/containment_temperature'
      rvPltContainTemp%MetaType = 'units'
      rvPltContainTemp%VariableType = '(oC)'
      rvPltContainTemp%Description = 'Temperature of containing zone'
      Call AddVariable(rvPltContainTemp)

      rvPltH2ConnFlowWork%VariableName = 'plant/*/misc_data/h2_connection/flow_work'
      rvPltH2ConnFlowWork%MetaType = 'units'
      rvPltH2ConnFlowWork%VariableType = '(W)'
      rvPltH2ConnFlowWork%Description = 'Flow work associated with charging tank'
      Call AddVariable(rvPltH2ConnFlowWork)

      rvPltH2DiscFlowWork%VariableName = 'plant/*/misc_data/h2_discharge/flow_work'
      rvPltH2DiscFlowWork%MetaType = 'units'
      rvPltH2DiscFlowWork%VariableType = '(W)'
      rvPltH2DiscFlowWork%Description = 'Flow work assocaited with discharging tank'
      Call AddVariable(rvPltH2DiscFlowWork)

      !Used by Hydrogen_appliances.F
      rvPltH2AppLoad%VariableName = 'plant/*/misc_data/H2_app_load'
      rvPltH2AppLoad%MetaType = 'units'
      rvPltH2AppLoad%VariableType = '(W)'
      rvPltH2AppLoad%Description = 'Hydrogen appliance electric load'
      Call AddVariable(rvPltH2AppLoad)

      rvPltH2AppDemand%VariableName = 'plant/*/misc_data/H2_app_demand'
      rvPltH2AppDemand%MetaType = 'units'
      rvPltH2AppDemand%VariableType = '(kg/s)'
      rvPltH2AppDemand%Description = 'Hydrogen demand of appliance'
      Call AddVariable(rvPltH2AppDemand)

      !Used by the compressed_cylinder.F
      rvPltMassPresent%VariableName = 'plant/*/misc_data/mass/present'
      rvPltMassPresent%MetaType = 'units'
      rvPltMassPresent%VariableType = '(kg)'
      rvPltMassPresent%Description = 'Compressed gas cylinder: hydrogen mass at present time-row'
      Call AddVariable(rvPltMassPresent)

      rvPltMassFuture%VariableName = 'plant/*/misc_data/mass/future'
      rvPltMassFuture%MetaType = 'units'
      rvPltMassFuture%VariableType = '(kg)'
      rvPltMassFuture%Description = 'Compressed gas cylinder: hydrogen mass at future time-row'
      Call AddVariable(rvPltMassFuture)

      rvPltHydroChargeRt%VariableName = 'plant/*/misc_data/hydrogen_charge_rate'
      rvPltHydroChargeRt%MetaType = 'units'
      rvPltHydroChargeRt%VariableType = '(kg/s)'
      rvPltHydroChargeRt%Description = 'Compressed gas cylinder: rate of charge'
      Call AddVariable(rvPltHydroChargeRt)

      rvPltHydroDischgRt%VariableName = 'plant/*/misc_data/hydrogen_discharge_rate'
      rvPltHydroDischgRt%MetaType = 'units'
      rvPltHydroDischgRt%VariableType = '(kg/s)'
      rvPltHydroDischgRt%Description = 'Compressed gas cylinder: rate of discharge'
      Call AddVariable(rvPltHydroDischgRt)

      rvPltVentH2Rt%VariableName = 'plant/*/misc_data/vent_H2_rate'
      rvPltVentH2Rt%MetaType = 'units'
      rvPltVentH2Rt%VariableType = '(kg/s)'
      rvPltVentH2Rt%Description = 'Compressed gas cylinder: rate at which excess hydrogen is vented'
      Call AddVariable(rvPltVentH2Rt)

      rvPltPressPres%VariableName = 'plant/*/misc_data/pressure/present'
      rvPltPressPres%MetaType = 'units'
      rvPltPressPres%VariableType = '(kPa)'
      rvPltPressPres%Description = 'Compressed gas cylinder: gas pressure at present time-row'
      Call AddVariable(rvPltPressPres)

      rvPltPressFut%VariableName = 'plant/*/misc_data/pressure/future'
      rvPltPressFut%MetaType = 'units'
      rvPltPressFut%VariableType = '(kPa)'
      rvPltPressFut%Description = 'Compressed gas cylinder: gas pressure at future time-row'
      Call AddVariable(rvPltPressFut)

      rvPltHydroDefIMP%VariableName = 'plant/*/misc_data/hydrogen_deficit_IMP'
      rvPltHydroDefIMP%MetaType = 'units'
      rvPltHydroDefIMP%VariableType = '(kg/s)'
      rvPltHydroDefIMP%Description = 'Compressed gas cylinder: H2 deficit (IMP)'
      Call AddVariable(rvPltHydroDefIMP)

      rvPltHydroDefEXP%VariableName = 'plant/*/misc_data/hydrogen_deficit_EXP'
      rvPltHydroDefEXP%MetaType = 'units'
      rvPltHydroDefEXP%VariableType = '(kg/s)'
      rvPltHydroDefEXP%Description = 'Compressed gas cylinder: H2 deficit (EXP)'
      Call AddVariable(rvPltHydroDefEXP)

      rvDebugGasCylm3%VariableName = 'debug/gas_cylinder/init/*'
      rvDebugGasCylm3%MetaType = 'units'
      rvDebugGasCylm3%VariableType = '(m3)'
      rvDebugGasCylm3%Description = ''
      Call AddVariable(rvDebugGasCylm3)

      rvDebugGasCylkg%VariableName = 'debug/gas_cylinder/init/*'
      rvDebugGasCylkg%MetaType = 'units'
      rvDebugGasCylkg%VariableType = '(kg)'
      rvDebugGasCylkg%Description = ''
      Call AddVariable(rvDebugGasCylkg)

      rvDebugGasCylJKgoC%VariableName = 'debug/gas_cylinder/init/*'
      rvDebugGasCylJKgoC%MetaType = 'units'
      rvDebugGasCylJKgoC%VariableType = '(J/kg oC)'
      rvDebugGasCylJKgoC%Description = ''
      Call AddVariable(rvDebugGasCylJKgoC)

      rvDebugGasCylWoC%VariableName = 'debug/gas_cylinder/init/*'
      rvDebugGasCylWoC%MetaType = 'units'
      rvDebugGasCylWoC%VariableType = '(W /oC)'
      rvDebugGasCylWoC%Description = ''
      Call AddVariable(rvDebugGasCylWoC)

      rvDebugGasCylkPa%VariableName = 'debug/gas_cylinder/init/*'
      rvDebugGasCylkPa%MetaType = 'units'
      rvDebugGasCylkPa%VariableType = '(kPa)'
      rvDebugGasCylkPa%Description = ''
      Call AddVariable(rvDebugGasCylkPa)

      rvDebugGasCylkjkgK%VariableName = 'debug/gas_cylinder/init/*'
      rvDebugGasCylkjkgK%MetaType = 'units'
      rvDebugGasCylkjkgK%VariableType = '(kJ/kg K)'
      rvDebugGasCylkjkgK%Description = ''
      Call AddVariable(rvDebugGasCylkjkgK)

      rvDebugGasCyloC%VariableName = 'debug/gas_cylinder/*'
      rvDebugGasCyloC%MetaType = 'units'
      rvDebugGasCyloC%VariableType = '(oC)'
      rvDebugGasCyloC%Description = ''
      Call AddVariable(rvDebugGasCyloC)

      rvDebugGasCylkgs%VariableName = 'debug/gas_cylinder/*'
      rvDebugGasCylkgs%MetaType = 'units'
      rvDebugGasCylkgs%VariableType = '(kg/s)'
      rvDebugGasCylkgs%Description = ''
      Call AddVariable(rvDebugGasCylkgs)

      rvDebugGasCylw%VariableName = 'debug/gas_cylinder/*'
      rvDebugGasCylw%MetaType = 'units'
      rvDebugGasCylw%VariableType = '(W)'
      rvDebugGasCylw%Description = ''
      Call AddVariable(rvDebugGasCylw)

      rvDebugGasCyljkgk%VariableName = 'debug/gas_cylinder/*'
      rvDebugGasCyljkgk%MetaType = 'units'
      rvDebugGasCyljkgk%VariableType = '(J/kg K)'
      rvDebugGasCyljkgk%Description = ''
      Call AddVariable(rvDebugGasCyljkgk)


      !Used by Annex42_fuel_cell.F
      rvPltAPel%VariableName = 'plant/*/misc_data/A_Pel'
      rvPltAPel%MetaType = 'units'
      rvPltAPel%VariableType = '(W)'
      rvPltAPel%Description = 'SOFC-A42: DC power delivered by FCPM'
      Call AddVariable(rvPltAPel)

      rvPltEtaEl%VariableName = 'plant/*/misc_data/eta_el'
      rvPltEtaEl%MetaType = 'units'
      rvPltEtaEl%VariableType = '(%LHV)'
      rvPltEtaEl%Description = 'SOFC-A42: Electrical efficiency of FCPM'
      Call AddVariable(rvPltEtaEl)

      rvPltNdotFCPMFuel%VariableName = 'plant/*/misc_data/Ndot_FCPM_fuel'
      rvPltNdotFCPMFuel%MetaType = 'units'
      rvPltNdotFCPMFuel%VariableType = '(kmol/s)'
      rvPltNdotFCPMFuel%Description = 'SOFC-A42: Fuel supply to FCPM'
      Call AddVariable(rvPltNdotFCPMFuel)

      rvPltNdotFCPMAir%VariableName = 'plant/*/misc_data/Ndot_FCPM_air'
      rvPltNdotFCPMAir%MetaType = 'units'
      rvPltNdotFCPMAir%VariableType = '(kmol/s)'
      rvPltNdotFCPMAir%Description = 'SOFC-A42: Air supply to FCPM'
      Call AddVariable(rvPltNdotFCPMAir)

      rvPltLambdaFCPM%VariableName = 'plant/*/misc_data/lambda_FCPM'
      rvPltLambdaFCPM%MetaType = 'units'
      rvPltLambdaFCPM%VariableType = '(-)'
      rvPltLambdaFCPM%Description = 'SOFC-A42: FCPM excess air ratio'
      Call AddVariable(rvPltLambdaFCPM)

      rvPltPelAncAc%VariableName = 'plant/*/misc_data/Pel_ancAC'
      rvPltPelAncAc%MetaType = 'units'
      rvPltPelAncAc%VariableType = '(W)'
      rvPltPelAncAc%Description = 'SOFC-A42: FCPM AC ancillaries'
      Call AddVariable(rvPltPelAncAc)

      rvPltNdotAuxBurnFuel%VariableName = 'plant/*/misc_data/Ndot_auxburn_fuel'
      rvPltNdotAuxBurnFuel%MetaType = 'units'
      rvPltNdotAuxBurnFuel%VariableType = '(kmol/s)'
      rvPltNdotAuxBurnFuel%Description = 'SOFC-A42: Fuel supply to auxiliary burner'
      Call AddVariable(rvPltNdotAuxBurnFuel)

      rvPltNdotAuxBurnAir%VariableName = 'plant/*/misc_data/Ndot_auxburn_air'
      rvPltNdotAuxBurnAir%MetaType = 'units'
      rvPltNdotAuxBurnAir%VariableType = '(kmol/s)'
      rvPltNdotAuxBurnAir%Description = 'SOFC-A42: Air supply to auxiliary burner'
      Call AddVariable(rvPltNdotAuxBurnAir)

      rvPltQAuxBurn%VariableName = 'plant/*/misc_data/Q_auxburn'
      rvPltQAuxBurn%MetaType = 'units'
      rvPltQAuxBurn%VariableType = '(W)'
      rvPltQAuxBurn%Description = 'SOFC-A42: Heat output of auxiliary burner'
      Call AddVariable(rvPltQAuxBurn)

      rvPltAuxBurnPel%VariableName = 'plant/*/misc_data/auxburn_Pel'
      rvPltAuxBurnPel%MetaType = 'units'
      rvPltAuxBurnPel%VariableType = '(W)'
      rvPltAuxBurnPel%Description = 'SOFC-A42: Elec draw of auxiliary burner ancillaries'
      Call AddVariable(rvPltAuxBurnPel)

      rvPltAuxBurnSkinLoss%VariableName = 'plant/*/misc_data/auxburn_skin_loss'
      rvPltAuxBurnSkinLoss%MetaType = 'units'
      rvPltAuxBurnSkinLoss%VariableType = '(W)'
      rvPltAuxBurnSkinLoss%Description = 'SOFC-A42: Skin losses from auxiliary burner'
      Call AddVariable(rvPltAuxBurnSkinLoss)

      rvPltQHX%VariableName = 'plant/*/misc_data/Q_HX'
      rvPltQHX%MetaType = 'units'
      rvPltQHX%VariableType = '(W)'
      rvPltQHX%Description = 'SOFC-A42: Heat recovery to water'
      Call AddVariable(rvPltQHX)

      rvPltQHSSens%VariableName = 'plant/*/misc_data/Q_HX_sensible'
      rvPltQHSSens%MetaType = 'units'
      rvPltQHSSens%VariableType = '(W)'
      rvPltQHSSens%Description = 'SOFC-A42: Sensible eat recovery to water'
      Call AddVariable(rvPltQHSSens)

      rvPltQHXLatent%VariableName = 'plant/*/misc_data/Q_HX_latent'
      rvPltQHXLatent%MetaType = 'units'
      rvPltQHXLatent%VariableType = '(W)'
      rvPltQHXLatent%Description = 'SOFC-A42: Latent heat recovery to water'
      Call AddVariable(rvPltQHXLatent)

      rvPltCWater%VariableName = 'plant/*/misc_data/C_water'
      rvPltCWater%MetaType = 'units'
      rvPltCWater%VariableType = '(W/K)'
      rvPltCWater%Description = 'Heat capacity rate of water in HX'
      Call AddVariable(rvPltCWater)

      rvPltCGas%VariableName = 'plant/*/misc_data/C_gas'
      rvPltCGas%MetaType = 'units'
      rvPltCGas%VariableType = '(W/K)'
      rvPltCGas%Description = 'Heat capacity rate of gas in HX'
      Call AddVariable(rvPltCGas)

      rvPltUAHX%VariableName = 'plant/*/misc_data/UA_HX'
      rvPltUAHX%MetaType = 'units'
      rvPltUAHX%VariableType = '(W/K)'
      rvPltUAHX%Description = 'Effective UA of HX'
      Call AddVariable(rvPltUAHX)

      rvPltHXNdotCondense%VariableName = 'plant/*/misc_data/HX_Ndot_condense'
      rvPltHXNdotCondense%MetaType = 'units'
      rvPltHXNdotCondense%VariableType = '(kmol/s)'
      rvPltHXNdotCondense%Description = 'Condensation rate in HX'
      Call AddVariable(rvPltHXNdotCondense)

      rvPltHXWaterVapFrac%VariableName = 'plant/*/misc_data/HX_water_vap_frac'
      rvPltHXWaterVapFrac%MetaType = 'units'
      rvPltHXWaterVapFrac%VariableType = '(-)'
      rvPltHXWaterVapFrac%Description = 'Fraction of water vapour in exhaust'
      Call AddVariable(rvPltHXWaterVapFrac)

      rvPltHXHfg%VariableName = 'plant/*/misc_data/HX_Hfg'
      rvPltHXHfg%MetaType = 'units'
      rvPltHXHfg%VariableType = '(J/kmol)'
      rvPltHXHfg%Description = 'Latent heat of vapourization of water in HX'
      Call AddVariable(rvPltHXHfg)

      rvPltNdoHXGax%VariableName = 'plant/*/misc_data/Ndot_HX_gas'
      rvPltNdoHXGax%MetaType = 'units'
      rvPltNdoHXGax%VariableType = '(kmol/s)'
      rvPltNdoHXGax%Description = 'Molar flow of gas through HX'
      Call AddVariable(rvPltNdoHXGax)

      rvPltLHVFuel%VariableName = 'plant/*/misc_data/LHV_fuel'
      rvPltLHVFuel%MetaType = 'units'
      rvPltLHVFuel%VariableType = '(J/kmol)'
      rvPltLHVFuel%Description = 'SOFC-A42: Fuel LHV'
      Call AddVariable(rvPltLHVFuel)

      rvPltDeltaHFCPMFuel%VariableName = 'plant/*/misc_data/deltaH_FCPM_fuel'
      rvPltDeltaHFCPMFuel%MetaType = 'units'
      rvPltDeltaHFCPMFuel%VariableType = '(W)'
      rvPltDeltaHFCPMFuel%Description = 'SOFC-A42: Enthalpy flow rate of fuel to FCPM'
      Call AddVariable(rvPltDeltaHFCPMFuel)

      rvPltDeltaHFCPMAir%VariableName = 'plant/*/misc_data/deltaH_FCPM_air'
      rvPltDeltaHFCPMAir%MetaType = 'units'
      rvPltDeltaHFCPMAir%VariableType = '(W)'
      rvPltDeltaHFCPMAir%Description = 'SOFC-A42: Enthalpy flow rate of air to FCPM'
      Call AddVariable(rvPltDeltaHFCPMAir)

      rvPltNdotFCPMexhN2%VariableName = 'plant/*/misc_data/Ndot_FCPMexh_N2'
      rvPltNdotFCPMexhN2%MetaType = 'units'
      rvPltNdotFCPMexhN2%VariableType = '(kmol/s)'
      rvPltNdotFCPMexhN2%Description = 'SOFC-A42: N2 exh'
      Call AddVariable(rvPltNdotFCPMexhN2)

      rvPltNdotFCPMexhAr%VariableName = 'plant/*/misc_data/Ndot_FCPMexh_Ar'
      rvPltNdotFCPMexhAr%MetaType = 'units'
      rvPltNdotFCPMexhAr%VariableType = '(kmol/s)'
      rvPltNdotFCPMexhAr%Description = 'SOFC-A42: Ar exh'
      Call AddVariable(rvPltNdotFCPMexhAr)

      rvPltNdotFCPMexh02%VariableName = 'plant/*/misc_data/Ndot_FCPMexh_O2'
      rvPltNdotFCPMexh02%MetaType = 'units'
      rvPltNdotFCPMexh02%VariableType = '(kmol/s)'
      rvPltNdotFCPMexh02%Description = 'SOFC-A42: O2 exh'
      Call AddVariable(rvPltNdotFCPMexh02)

      rvPltNdotFCPMexhC02%VariableName = 'plant/*/misc_data/Ndot_FCPMexh_CO2'
      rvPltNdotFCPMexhC02%MetaType = 'units'
      rvPltNdotFCPMexhC02%VariableType = '(kmol/s)'
      rvPltNdotFCPMexhC02%Description = 'SOFC-A42: CO2 exh'
      Call AddVariable(rvPltNdotFCPMexhC02)

      rvPltNdotFCPMexhH20%VariableName = 'plant/*/misc_data/Ndot_FCPMexh_H2O'
      rvPltNdotFCPMexhH20%MetaType = 'units'
      rvPltNdotFCPMexhH20%VariableType = '(kmol/s)'
      rvPltNdotFCPMexhH20%Description = 'SOFC-A42: H2O exh'
      Call AddVariable(rvPltNdotFCPMexhH20)

      rvPltDeltaHFCPMexh%VariableName = 'plant/*/misc_data/deltaH_FCPM_exh'
      rvPltDeltaHFCPMexh%MetaType = 'units'
      rvPltDeltaHFCPMexh%VariableType = '(kmol/s)'
      rvPltDeltaHFCPMexh%Description = 'SOFC-A42: deltaH_FCPM_exh'
      Call AddVariable(rvPltDeltaHFCPMexh)

      rvPltBlowerPel%VariableName = 'plant/*/misc_data/blower_Pel'
      rvPltBlowerPel%MetaType = 'units'
      rvPltBlowerPel%VariableType = '(W)'
      rvPltBlowerPel%Description = 'SOFC-A42: blower_Pel'
      Call AddVariable(rvPltBlowerPel)

      rvPltTBlowerIn%VariableName = 'plant/*/misc_data/T_blower_in'
      rvPltTBlowerIn%MetaType = 'units'
      rvPltTBlowerIn%VariableType = '(oC)'
      rvPltTBlowerIn%Description = 'SOFC-A42: T_blower_in'
      Call AddVariable(rvPltTBlowerIn)

      rvPltCompPel%VariableName = 'plant/*/misc_data/comp_Pel'
      rvPltCompPel%MetaType = 'units'
      rvPltCompPel%VariableType = '(W)'
      rvPltCompPel%Description = 'SOFC-A42: comp_Pel'
      Call AddVariable(rvPltCompPel)

      rvPltPumpPel%VariableName = 'plant/*/misc_data/pump_Pel'
      rvPltPumpPel%MetaType = 'units'
      rvPltPumpPel%VariableType = '(W)'
      rvPltPumpPel%Description = 'SOFC-A42: pump_Pel'
      Call AddVariable(rvPltPumpPel)

      rvPltNdotFCPMLiqWater%VariableName = 'plant/*/misc_data/Ndot_FCPM_liqwater'
      rvPltNdotFCPMLiqWater%MetaType = 'units'
      rvPltNdotFCPMLiqWater%VariableType = '(kmol/s)'
      rvPltNdotFCPMLiqWater%Description = 'SOFC-A42: Ndot_FCPM_liqwater'
      Call AddVariable(rvPltNdotFCPMLiqWater)

      rvPltDeltaHFCPMLiqWater%VariableName = 'plant/*/misc_data/deltaH_FCPM_liqwater'
      rvPltDeltaHFCPMLiqWater%MetaType = 'units'
      rvPltDeltaHFCPMLiqWater%VariableType = '(W)'
      rvPltDeltaHFCPMLiqWater%Description = 'SOFC-A42: deltaH_FCPM_liqwater'
      Call AddVariable(rvPltDeltaHFCPMLiqWater)

      rvPltBattSOCP%VariableName = 'plant/*/misc_data/battery_SOC_p'
      rvPltBattSOCP%MetaType = 'units'
      rvPltBattSOCP%VariableType = '(-)'
      rvPltBattSOCP%Description = 'SOFC-A42: battery_SOC_p'
      Call AddVariable(rvPltBattSOCP)

      rvPltBattSOCf%VariableName = 'plant/*/misc_data/battery_SOC_f'
      rvPltBattSOCf%MetaType = 'units'
      rvPltBattSOCf%VariableType = '(-)'
      rvPltBattSOCf%Description = 'SOFC-A42: battery_SOC_f'
      Call AddVariable(rvPltBattSOCf)

      rvPltDCReqPCU%VariableName = 'plant/*/misc_data/DC_required_by_PCU'
      rvPltDCReqPCU%MetaType = 'units'
      rvPltDCReqPCU%VariableType = '(W)'
      rvPltDCReqPCU%Description = 'SOFC-A42: DC_required_by_PCU'
      Call AddVariable(rvPltDCReqPCU)

      rvPltDCInPCU%VariableName = 'plant/*/misc_data/DC_into_PCU'
      rvPltDCInPCU%MetaType = 'units'
      rvPltDCInPCU%VariableType = '(W)'
      rvPltDCInPCU%Description = 'SOFC-A42: DC_into_PCU'
      Call AddVariable(rvPltDCInPCU)

      rvPltPBatt%VariableName = 'plant/*/misc_data/P_battery'
      rvPltPBatt%MetaType = 'units'
      rvPltPBatt%VariableType = '(W)'
      rvPltPBatt%Description = 'SOFC-A42: P_battery'
      Call AddVariable(rvPltPBatt)

      rvPltACDemNet%VariableName = 'plant/*/misc_data/AC_demand_net'
      rvPltACDemNet%MetaType = 'units'
      rvPltACDemNet%VariableType = '(W)'
      rvPltACDemNet%Description = 'SOFC-A42: AC_demand_net'
      Call AddVariable(rvPltACDemNet)

      rvPltACProdGross%VariableName = 'plant/*/misc_data/AC_production_gross'
      rvPltACProdGross%MetaType = 'units'
      rvPltACProdGross%VariableType = '(W)'
      rvPltACProdGross%Description = 'SOFC-A42: AC_production_gross'
      Call AddVariable(rvPltACProdGross)

      rvPltACProdNet%VariableName = 'plant/*/misc_data/AC_production_net'
      rvPltACProdNet%MetaType = 'units'
      rvPltACProdNet%VariableType = '(W)'
      rvPltACProdNet%Description = 'SOFC-A42: AC_production_net'
      Call AddVariable(rvPltACProdNet)

      rvPltPCULosses%VariableName = 'plant/*/misc_data/PCU_losses'
      rvPltPCULosses%MetaType = 'units'
      rvPltPCULosses%VariableType = '(W)'
      rvPltPCULosses%Description = 'SOFC-A42: PCU_losses'
      Call AddVariable(rvPltPCULosses)

      rvPltEtaPCU%VariableName = 'plant/*/misc_data/eta_PCU'
      rvPltEtaPCU%MetaType = 'units'
      rvPltEtaPCU%VariableType = '(W)'
      rvPltEtaPCU%Description = 'SOFC-A42: eta_PCU'
      Call AddVariable(rvPltEtaPCU)

      rvPltQSCool%VariableName = 'plant/*/misc_data/q_s_cool'
      rvPltQSCool%MetaType = 'units'
      rvPltQSCool%VariableType = '(W)'
      rvPltQSCool%Description = 'SOFC-A42: q_s_cool'
      Call AddVariable(rvPltQSCool)

      rvPltQSCogen%VariableName = 'plant/*/misc_data/q_s_cogen'
      rvPltQSCogen%MetaType = 'units'
      rvPltQSCogen%VariableType = '(W)'
      rvPltQSCogen%Description = 'SOFC-A42: q_s_cogen'
      Call AddVariable(rvPltQSCogen)

      rvPltQSCoolAir%VariableName = 'plant/*/misc_data/q_s_cool_air'
      rvPltQSCoolAir%MetaType = 'units'
      rvPltQSCoolAir%VariableType = '(W)'
      rvPltQSCoolAir%Description = 'SOFC-A42: q_s_cool_air'
      Call AddVariable(rvPltQSCoolAir)

      rvPltPAirEl%VariableName = 'plant/*/misc_data/P_air_el'
      rvPltPAirEl%MetaType = 'units'
      rvPltPAirEl%VariableType = '(W)'
      rvPltPAirEl%Description = 'SOFC-A42: P_air_el'
      Call AddVariable(rvPltPAirEl)

      rvPltUASCogen%VariableName = 'plant/*/misc_data/UA_s_cogen'
      rvPltUASCogen%MetaType = 'units'
      rvPltUASCogen%VariableType = '(W)'
      rvPltUASCogen%Description = 'SOFC-A42: UA_s_cogen'
      Call AddVariable(rvPltUASCogen)

      rvPltQHRV%VariableName = 'plant/*/misc_data/q_HRV'
      rvPltQHRV%MetaType = 'units'
      rvPltQHRV%VariableType = '(W)'
      rvPltQHRV%Description = 'SOFC-A42: q_HRV'
      Call AddVariable(rvPltQHRV)

      rvPltTVentAir%VariableName = 'plant/*/misc_data/T_vent_air'
      rvPltTVentAir%MetaType = 'units'
      rvPltTVentAir%VariableType = '(oC)'
      rvPltTVentAir%Description = 'SOFC-A42: T_vent_air'
      Call AddVariable(rvPltTVentAir)

      rvPltDilutionHeatToAir%VariableName = 'plant/*/misc_data/Dilution_heattoair'
      rvPltDilutionHeatToAir%MetaType = 'units'
      rvPltDilutionHeatToAir%VariableType = '(W)'
      rvPltDilutionHeatToAir%Description = 'SOFC-A42: Dilution_heattoair'
      Call AddVariable(rvPltDilutionHeatToAir)

      rvPltLHVToACEff%VariableName = 'plant/*/misc_data/LHV_to_AC_eff'
      rvPltLHVToACEff%MetaType = 'units'
      rvPltLHVToACEff%VariableType = '(-)'
      rvPltLHVToACEff%Description = 'SOFC-A42: LHV_to_AC_eff'
      Call AddVariable(rvPltLHVToACEff)

      rvPltLHVToThermEff%VariableName = 'plant/*/misc_data/LHV_to_therm_eff'
      rvPltLHVToThermEff%MetaType = 'units'
      rvPltLHVToThermEff%VariableType = '(-)'
      rvPltLHVToThermEff%Description = 'SOFC-A42: LHV_to_therm_eff'
      Call AddVariable(rvPltLHVToThermEff)

      rvPltLHVToCogenEff%VariableName = 'plant/*/misc_data/LHV_to_cogen_eff'
      rvPltLHVToCogenEff%MetaType = 'units'
      rvPltLHVToCogenEff%VariableType = '(-)'
      rvPltLHVToCogenEff%Description = 'SOFC-A42: LHV_to_cogen_eff'
      Call AddVariable(rvPltLHVToCogenEff)

      rvPltHeatToRoomConv%VariableName = 'plant/*/misc_data/heat_to_room_conv'
      rvPltHeatToRoomConv%MetaType = 'units'
      rvPltHeatToRoomConv%VariableType = '(W)'
      rvPltHeatToRoomConv%Description = 'heat_to_room_conv'
      Call AddVariable(rvPltHeatToRoomConv)

      rvPltHeatToRoomRad%VariableName = 'plant/*/misc_data/heat_to_room_rad'
      rvPltHeatToRoomRad%MetaType = 'units'
      rvPltHeatToRoomRad%VariableType = '(W)'
      rvPltHeatToRoomRad%Description = 'heat_to_room_rad'
      Call AddVariable(rvPltHeatToRoomRad)


      !Used by Annex42_combustion_CHP.F
      rvPltExtCtrlScheme%VariableName = 'plant/*/misc_data/external_control/scheme'
      rvPltExtCtrlScheme%MetaType = 'units'
      rvPltExtCtrlScheme%VariableType = '(-)'
      rvPltExtCtrlScheme%Description = 'A42 CHP model: External control scheme type'
      Call AddVariable(rvPltExtCtrlScheme)

      rvPltExtCtrlSignal%VariableName = 'plant/*/misc_data/external_control/signal'
      rvPltExtCtrlSignal%MetaType = 'units'
      rvPltExtCtrlSignal%VariableType = '(-)'
      rvPltExtCtrlSignal%Description = 'A42 CHP model: External control signal'
      Call AddVariable(rvPltExtCtrlSignal)

      rvPltOperModeInop%VariableName = 'plant/*/misc_data/operating_mode/inoperative'
      rvPltOperModeInop%MetaType = 'units'
      rvPltOperModeInop%VariableType = '(s)'
      rvPltOperModeInop%Description = 'A42 CHP model: Cumulative time spent inoperative'
      Call AddVariable(rvPltOperModeInop)

      rvPltA42OperModeStartUp%VariableName = 'plant/*/misc_data/operating_mode/start_up'
      rvPltA42OperModeStartUp%MetaType = 'units'
      rvPltA42OperModeStartUp%VariableType = '(s)'
      rvPltA42OperModeStartUp%Description = 'A42 CHP model: Cumulative time spent in start-up mode'
      Call AddVariable(rvPltA42OperModeStartUp)

      rvPltOperModeWarmup%VariableName = 'plant/*/misc_data/operating_mode/warmup'
      rvPltOperModeWarmup%MetaType = 'units'
      rvPltOperModeWarmup%VariableType = '(s)'
      rvPltOperModeWarmup%Description = 'A42 CHP model: Cumulative time spent warming up'
      Call AddVariable(rvPltOperModeWarmup)

      rvPltOpenModeNormOper%VariableName = 'plant/*/misc_data/operating_mode/normal_operation'
      rvPltOpenModeNormOper%MetaType = 'units'
      rvPltOpenModeNormOper%VariableType = '(s)'
      rvPltOpenModeNormOper%Description = 'A42 CHP model: Cumulative time spent in normal operation'
      Call AddVariable(rvPltOpenModeNormOper)

      rvPltOperModeShutdown%VariableName = 'plant/*/misc_data/operating_mode/shutdown'
      rvPltOperModeShutdown%MetaType = 'units'
      rvPltOperModeShutdown%VariableType = '(s)'
      rvPltOperModeShutdown%Description = 'A42 CHP model: Cumulative time spent in shut-down mode'
      Call AddVariable(rvPltOperModeShutdown)

      rvPltContTemp%VariableName = 'plant/*/misc_data/containment_temperature'
      rvPltContTemp%MetaType = 'units'
      rvPltContTemp%VariableType = '(oC)'
      rvPltContTemp%Description = 'A42 CHP model: temperature of containing room'
      Call AddVariable(rvPltContTemp)

      rvPltCoolWaterTemp%VariableName = 'plant/*/misc_data/cooling_water/temperature'
      rvPltCoolWaterTemp%MetaType = 'units'
      rvPltCoolWaterTemp%VariableType = '(oC)'
      rvPltCoolWaterTemp%Description = 'A42 CHP model: temperature of cooling water at inlet'
      Call AddVariable(rvPltCoolWaterTemp)

      rvPltCoolWaterFlowRt%VariableName = 'plant/*/misc_data/cooling_water/flow_rate'
      rvPltCoolWaterFlowRt%MetaType = 'units'
      rvPltCoolWaterFlowRt%VariableType = '(kg/s)'
      rvPltCoolWaterFlowRt%Description = 'A42 CHP model: mass flow rate of cooling water'
      Call AddVariable(rvPltCoolWaterFlowRt)

      rvPltInstPowerNet%VariableName = 'plant/*/misc_data/instantaneous/power/net'
      rvPltInstPowerNet%MetaType = 'units'
      rvPltInstPowerNet%VariableType = '(W)'
      rvPltInstPowerNet%Description = 'A42 CHP model: net power produced'
      Call AddVariable(rvPltInstPowerNet)

      rvPltInstHeatRec%VariableName = 'plant/*/misc_data/instantaneous/heat_recovery'
      rvPltInstHeatRec%MetaType = 'units'
      rvPltInstHeatRec%VariableType = '(W)'
      rvPltInstHeatRec%Description = 'A42 CHP model: Rate of heat transfer to cooling water'
      Call AddVariable(rvPltInstHeatRec)

      rvPltInstFlowRt%VariableName = 'plant/*/misc_data/instantaneous/combustion_air/flow_rate'
      rvPltInstFlowRt%MetaType = 'units'
      rvPltInstFlowRt%VariableType = '(kg/s)'
      rvPltInstFlowRt%Description = 'A42 CHP model: combustion air flow rate'
      Call AddVariable(rvPltInstFlowRt)

      rvPltInstFuelFlowRt%VariableName = 'plant/*/misc_data/instantaneous/fuel/flow_rate'
      rvPltInstFuelFlowRt%MetaType = 'units'
      rvPltInstFuelFlowRt%VariableType = '(kg/s)'
      rvPltInstFuelFlowRt%Description = 'A42 CHP model: fuel mass flow rate'
      Call AddVariable(rvPltInstFuelFlowRt)

      rvPltInstFuelGrossEnInput%VariableName = 'plant/*/misc_data/instantaneous/fuel/gross_energy_input'
      rvPltInstFuelGrossEnInput%MetaType = 'units'
      rvPltInstFuelGrossEnInput%VariableType = '(W)'
      rvPltInstFuelGrossEnInput%Description = 'A42 CHP model: LHV flow rate of fuel used by engine'
      Call AddVariable(rvPltInstFuelGrossEnInput)

      rvPltInstFuelLowHeatVal%VariableName = 'plant/*/misc_data/instantaneous/fuel/lower_heating_value'
      rvPltInstFuelLowHeatVal%MetaType = 'units'
      rvPltInstFuelLowHeatVal%VariableType = '(J/kg)'
      rvPltInstFuelLowHeatVal%Description = 'A42 CHP model: LHV of fuel used by engine'
      Call AddVariable(rvPltInstFuelLowHeatVal)

      rvPltInstHeatLoss%VariableName = 'plant/*/misc_data/instantaneous/heat_loss'
      rvPltInstHeatLoss%MetaType = 'units'
      rvPltInstHeatLoss%VariableType = '(W)'
      rvPltInstHeatLoss%Description = 'A42 CHP model: rate of heat loss to surroundings'
      Call AddVariable(rvPltInstHeatLoss)

      rvPltAvgHeatRec%VariableName = 'plant/*/misc_data/average/heat_recovery'
      rvPltAvgHeatRec%MetaType = 'units'
      rvPltAvgHeatRec%VariableType = '(W)'
      rvPltAvgHeatRec%Description = 'A42 CHP model: Rate of heat transfer to cooling water averaged over time step'
      Call AddVariable(rvPltAvgHeatRec)

      rvPltAvgPowNet%VariableName = 'plant/*/misc_data/average/power/net'
      rvPltAvgPowNet%MetaType = 'units'
      rvPltAvgPowNet%VariableType = '(W)'
      rvPltAvgPowNet%Description = 'A42 CHP model: Rate of net electrical production averaged over timestep'
      Call AddVariable(rvPltAvgPowNet)

      rvPltAvgFuelFlowRt%VariableName = 'plant/*/misc_data/average/fuel/flow_rate'
      rvPltAvgFuelFlowRt%MetaType = 'units'
      rvPltAvgFuelFlowRt%VariableType = '(kg/s)'
      rvPltAvgFuelFlowRt%Description = 'A42 CHP model: Fuel mass flow rate averaged over timestep'
      Call AddVariable(rvPltAvgFuelFlowRt)

      rvPltAvgFuelGrossEnIn%VariableName = 'plant/*/misc_data/average/fuel/gross_energy_input'
      rvPltAvgFuelGrossEnIn%MetaType = 'units'
      rvPltAvgFuelGrossEnIn%VariableType = '(W)'
      rvPltAvgFuelGrossEnIn%Description = 'A42 CHP model: LHV of fuel used by engine, averaged over timestep'
      Call AddVariable(rvPltAvgFuelGrossEnIn)

      rvPltAvgEmisCarbDio%VariableName = 'plant/*/misc_data/average/emissions/carbon_dioxide'
      rvPltAvgEmisCarbDio%MetaType = 'units'
      rvPltAvgEmisCarbDio%VariableType = '(kg/s)'
      rvPltAvgEmisCarbDio%Description = 'A42 CHP model: carbon dioxide emissions, averaged over timestep'
      Call AddVariable(rvPltAvgEmisCarbDio)

      rvPltAvgEffElec%VariableName = 'plant/*/misc_data/average/efficiency/electric'
      rvPltAvgEffElec%MetaType = 'units'
      rvPltAvgEffElec%VariableType = '(-)'
      rvPltAvgEffElec%Description = 'A42 CHP model: Electrical efficiency averaged over timestep'
      Call AddVariable(rvPltAvgEffElec)

      rvPltAvgEffTherm%VariableName = 'plant/*/misc_data/average/efficiency/thermal'
      rvPltAvgEffTherm%MetaType = 'units'
      rvPltAvgEffTherm%VariableType = '(-)'
      rvPltAvgEffTherm%Description = 'A42 CHP model: Thermal efficiency averaged over timestep'
      Call AddVariable(rvPltAvgEffTherm)

      rvPltAvgEffCogen%VariableName = 'plant/*/misc_data/average/efficiency/cogeneration'
      rvPltAvgEffCogen%MetaType = 'units'
      rvPltAvgEffCogen%VariableType = '(-)'
      rvPltAvgEffCogen%Description = 'A42 CHP model: Cogeneration (overall) efficiency averaged over timestep'
      Call AddVariable(rvPltAvgEffCogen)

      rvPltInstEffCogen%VariableName = 'plant/*/misc_data/instantaneous/efficiency/cogeneration'
      rvPltInstEffCogen%MetaType = 'units'
      rvPltInstEffCogen%VariableType = '(-)'
      rvPltInstEffCogen%Description = 'A42 CHP model: Cogeneration (overall) efficiency at end of time step'
      Call AddVariable(rvPltInstEffCogen)

      rvPltInstEffElec%VariableName = 'plant/*/misc_data/instantaneous/efficiency/electric'
      rvPltInstEffElec%MetaType = 'units'
      rvPltInstEffElec%VariableType = '(-)'
      rvPltInstEffElec%Description = 'A42 CHP model: Electrical efficiency at end of timestep'
      Call AddVariable(rvPltInstEffElec)

      rvPltInstEffTherm%VariableName = 'plant/*/misc_data/instantaneous/efficiency/thermal'
      rvPltInstEffTherm%MetaType = 'units'
      rvPltInstEffTherm%VariableType = '(-)'
      rvPltInstEffTherm%Description = 'A42 CHP model: Electrical efficiency at end of timestep'
      Call AddVariable(rvPltInstEffTherm)

      rvPltSolWeightFact%VariableName = 'plant/*/misc_data/solution_params/weighting_factor'
      rvPltSolWeightFact%MetaType = 'units'
      rvPltSolWeightFact%VariableType = '(-)'
      rvPltSolWeightFact%Description = 'A42 CHP model: Implicit/explicit solution weighting factor.'
      Call AddVariable(rvPltSolWeightFact)

      rvPltSolTmeCstEngCtrlVol%VariableName = 'plant/*/misc_data/solution_params/time_constant/engine_control_volume'
      rvPltSolTmeCstEngCtrlVol%MetaType = 'units'
      rvPltSolTmeCstEngCtrlVol%VariableType = '(s)'
      rvPltSolTmeCstEngCtrlVol%Description = 'A42 CHP model: Engine control volume time constant'
      Call AddVariable(rvPltSolTmeCstEngCtrlVol)

      rvPltSolTmeCstCWCtrlVol%VariableName = 'plant/*/misc_data/solution_params/time_constant/cw_control_volume'
      rvPltSolTmeCstCWCtrlVol%MetaType = 'units'
      rvPltSolTmeCstCWCtrlVol%VariableType = '(s)'
      rvPltSolTmeCstCWCtrlVol%Description = 'A42 CHP model: Cooling water control volume time constant'
      Call AddVariable(rvPltSolTmeCstCWCtrlVol)

      !Used by ADS_storage_unit.F
      rvPltMaxAdsTemp%VariableName = 'plant/*/misc_data/max_ads_temp'
      rvPltMaxAdsTemp%MetaType = 'units'
      rvPltMaxAdsTemp%VariableType = '(oC)'
      rvPltMaxAdsTemp%Description = 'Maximum temperature of adsorber'
      Call AddVariable(rvPltMaxAdsTemp)

      rvPltMinAdsTemp%VariableName = 'plant/*/misc_data/min_ads_temp'
      rvPltMinAdsTemp%MetaType = 'units'
      rvPltMinAdsTemp%VariableType = '(oC)'
      rvPltMinAdsTemp%Description = 'Minimum temperature of adsorber'
      Call AddVariable(rvPltMinAdsTemp)

      rvPltAbsorbPress%VariableName = 'plant/*/misc_data/adsorber_press'
      rvPltAbsorbPress%MetaType = 'units'
      rvPltAbsorbPress%VariableType = '(kPa)'
      rvPltAbsorbPress%Description = 'Adsorber pressure'
      Call AddVariable(rvPltAbsorbPress)

      rvPltAdsVapFlow%VariableName = 'plant/*/misc_data/ads_vapour_flow'
      rvPltAdsVapFlow%MetaType = 'units'
      rvPltAdsVapFlow%VariableType = '(kg/s)'
      rvPltAdsVapFlow%Description = 'Mass flow rate of vapour leaving/entering the adsorber'
      Call AddVariable(rvPltAdsVapFlow)

      rvPltCondFanPow%VariableName = 'plant/*/misc_data/cond_fan_power'
      rvPltCondFanPow%MetaType = 'units'
      rvPltCondFanPow%VariableType = '(W)'
      rvPltCondFanPow%Description = 'Power draw of condenser fan'
      Call AddVariable(rvPltCondFanPow)

      rvPltAdsorpHeat%VariableName = 'plant/*/misc_data/adsorption_heat'
      rvPltAdsorpHeat%MetaType = 'units'
      rvPltAdsorpHeat%VariableType = '(W)'
      rvPltAdsorpHeat%Description = 'Rate of heat stored in desorption/released by adsorption'
      Call AddVariable(rvPltAdsorpHeat)

      rvPltAdsHeatTransCH%VariableName = 'plant/*/misc_data/ads_heat_transfer_ch'
      rvPltAdsHeatTransCH%MetaType = 'units'
      rvPltAdsHeatTransCH%VariableType = '(W)'
      rvPltAdsHeatTransCH%Description = 'Rate of heat transfer to adsorber'
      Call AddVariable(rvPltAdsHeatTransCH)

      rvPltAdsHeatTransDis%VariableName = 'plant/*/misc_data/ads_heat_transfer_dis'
      rvPltAdsHeatTransDis%MetaType = 'units'
      rvPltAdsHeatTransDis%VariableType = '(W)'
      rvPltAdsHeatTransDis%Description = 'Rate of heat transfer from adsorber'
      Call AddVariable(rvPltAdsHeatTransDis)

      rvPltAdsAmbHeatLoss%VariableName = 'plant/*/misc_data/ads_amb_heat_loss'
      rvPltAdsAmbHeatLoss%MetaType = 'units'
      rvPltAdsAmbHeatLoss%VariableType = '(W)'
      rvPltAdsAmbHeatLoss%Description = 'Rate of heat loss from adsorber to ambient'
      Call AddVariable(rvPltAdsAmbHeatLoss)

      rvPltCondAmbHeatLoss%VariableName = 'plant/*/misc_data/cond_amb_heat_loss'
      rvPltCondAmbHeatLoss%MetaType = 'units'
      rvPltCondAmbHeatLoss%VariableType = '(W)'
      rvPltCondAmbHeatLoss%Description = 'Rate of heat loss from condenser to ambient'
      Call AddVariable(rvPltCondAmbHeatLoss)

      rvPltEvapAmbHeatLoss%VariableName = 'plant/*/misc_data/evap_amb_heat_loss'
      rvPltEvapAmbHeatLoss%MetaType = 'units'
      rvPltEvapAmbHeatLoss%VariableType = '(W)'
      rvPltEvapAmbHeatLoss%Description = 'Rate of heat loss from evaporator to ambient'
      Call AddVariable(rvPltEvapAmbHeatLoss)

      rvPltTempCondIn%VariableName = 'plant/*/misc_data/temp_cond_in'
      rvPltTempCondIn%MetaType = 'units'
      rvPltTempCondIn%VariableType = '(oC)'
      rvPltTempCondIn%Description = 'Temperature of air entering condenser'
      Call AddVariable(rvPltTempCondIn)

      rvPltCondHeat%VariableName = 'plant/*/misc_data/cond_heat'
      rvPltCondHeat%MetaType = 'units'
      rvPltCondHeat%VariableType = '(W)'
      rvPltCondHeat%Description = 'Rate of heat rejected by condenser'
      Call AddVariable(rvPltCondHeat)

      rvPltEvapHeat%VariableName = 'plant/*/misc_data/evap_heat'
      rvPltEvapHeat%MetaType = 'units'
      rvPltEvapHeat%VariableType = '(W)'
      rvPltEvapHeat%Description = 'Rate of heat input to evaporator'
      Call AddVariable(rvPltEvapHeat)

      rvPltRatioX%VariableName = 'plant/*/misc_data/ratio_X'
      rvPltRatioX%MetaType = 'units'
      rvPltRatioX%VariableType = '(kg/kg adsorbent)'
      rvPltRatioX%Description = 'Ratio of adsorbate to adsorbent'
      Call AddVariable(rvPltRatioX)

      rvPltCyclePhase%VariableName = 'plant/*/misc_data/cycle_phase'
      rvPltCyclePhase%MetaType = 'units'
      rvPltCyclePhase%VariableType = '(-)'
      rvPltCyclePhase%Description = 'Phase of adsorption cycle'
      Call AddVariable(rvPltCyclePhase)

      !Used by ADS_storage_tanks.F
      rvPltPowDraw%VariableName = 'plant/*/misc_data/power_draw'
      rvPltPowDraw%MetaType = 'units'
      rvPltPowDraw%VariableType = '(W)'
      rvPltPowDraw%Description = 'Power demand of pump'
      Call AddVariable(rvPltPowDraw)

      rvPltBurnerOut%VariableName = 'plant/*/misc_data/burner_output'
      rvPltBurnerOut%MetaType = 'units'
      rvPltBurnerOut%VariableType = '(W)'
      rvPltBurnerOut%Description = 'Burner output'
      Call AddVariable(rvPltBurnerOut)

      rvPltBurnerWater%VariableName = 'plant/*/misc_data/burner_water'
      rvPltBurnerWater%MetaType = 'units'
      rvPltBurnerWater%VariableType = '(W)'
      rvPltBurnerWater%Description = 'Burner energy captured in water'
      Call AddVariable(rvPltBurnerWater)

      rvPltFuelConsKGS%VariableName = 'plant/*/misc_data/fuel_consumption_kgs'
      rvPltFuelConsKGS%MetaType = 'units'
      rvPltFuelConsKGS%VariableType = '(kg/s)'
      rvPltFuelConsKGS%Description = 'Burner fuel consumption in kg/s'
      Call AddVariable(rvPltFuelConsKGS)

      rvPltFuelConsSLPM%VariableName = 'plant/*/misc_data/fuel_consumption_slpm'
      rvPltFuelConsSLPM%MetaType = 'units'
      rvPltFuelConsSLPM%VariableType = '(l/min)'
      rvPltFuelConsSLPM%Description = 'Burner fuel consumption in l/min'
      Call AddVariable(rvPltFuelConsSLPM)

      rvPltAirFlowKGS%VariableName = 'plant/*/misc_data/airflow_kgs'
      rvPltAirFlowKGS%MetaType = 'units'
      rvPltAirFlowKGS%VariableType = '(kg/s)'
      rvPltAirFlowKGS%Description = 'Airflow in kg/s'
      Call AddVariable(rvPltAirFlowKGS)

      rvPltAirFlowSLPM%VariableName = 'plant/*/misc_data/airflow_slpm'
      rvPltAirFlowSLPM%MetaType = 'units'
      rvPltAirFlowSLPM%VariableType = '(l/min)'
      rvPltAirFlowSLPM%Description = 'Airflow in l/min'
      Call AddVariable(rvPltAirFlowSLPM)

      rvPltTempComb%VariableName = 'plant/*/misc_data/temp_comb'
      rvPltTempComb%MetaType = 'units'
      rvPltTempComb%VariableType = '(oC)'
      rvPltTempComb%Description = 'Temperature of combustion gases'
      Call AddVariable(rvPltTempComb)

      rvPltTempExh%VariableName = 'plant/*/misc_data/temp_exh'
      rvPltTempExh%MetaType = 'units'
      rvPltTempExh%VariableType = '(oC)'
      rvPltTempExh%Description = 'Temperature of exhaust gases'
      Call AddVariable(rvPltTempExh)

      rvPltHeatDumpFuture%VariableName = 'plant/*/misc_data/heatdump_future'
      rvPltHeatDumpFuture%MetaType = 'units'
      rvPltHeatDumpFuture%VariableType = '(W)'
      rvPltHeatDumpFuture%Description = 'Heatdump for water storage tank'
      Call AddVariable(rvPltHeatDumpFuture)

      rvPltDHWDraw%VariableName = 'plant/*/misc_data/dhw_draw'
      rvPltDHWDraw%MetaType = 'units'
      rvPltDHWDraw%VariableType = '(m3/s)'
      rvPltDHWDraw%Description = 'Domestic hot water draw'
      Call AddVariable(rvPltDHWDraw)

      rvPltDHWMakeupTemp%VariableName = 'plant/*/misc_data/dhw_makeup_temp'
      rvPltDHWMakeupTemp%MetaType = 'units'
      rvPltDHWMakeupTemp%VariableType = '(oC)'
      rvPltDHWMakeupTemp%Description = 'Domestic hot water makeup temperature'
      Call AddVariable(rvPltDHWMakeupTemp)

      rvPltHeatingOut%VariableName = 'plant/*/misc_data/heating_output'
      rvPltHeatingOut%MetaType = 'units'
      rvPltHeatingOut%VariableType = '(W)'
      rvPltHeatingOut%Description = 'Output of heating element'
      Call AddVariable(rvPltHeatingOut)

      !Used by wind_turbine_profile_model.F
      rvEPowWindGenOut%VariableName = 'electrical_net/power_only_components/*/misc_data/Wind_Gen_Output'
      rvEPowWindGenOut%MetaType = 'units'
      rvEPowWindGenOut%VariableType = '(W)'
      rvEPowWindGenOut%Description = 'Wind generator output'
      Call AddVariable(rvEPowWindGenOut)

      rvEPowWindSpeed%VariableName = 'electrical_net/power_only_components/*/misc_data/Wind_Speed'
      rvEPowWindSpeed%MetaType = 'units'
      rvEPowWindSpeed%VariableType = '(m/s)'
      rvEPowWindSpeed%Description = 'Wind speed'
      Call AddVariable(rvEPowWindSpeed)

      !Used by powoc_calc.F
      rvEPowLoadProf%VariableName = 'electrical_net/power_only_components/elec_load/elec_load_profile'
      rvEPowLoadProf%MetaType = 'units'
      rvEPowLoadProf%VariableType = '(W)'
      rvEPowLoadProf%Description = 'Original E-load'
      Call AddVariable(rvEPowLoadProf)

      rvEPowH2Load%VariableName = 'electrical_net/power_only_components/elec_load/elec_H2_loads'
      rvEPowH2Load%MetaType = 'units'
      rvEPowH2Load%VariableType = '(W)'
      rvEPowH2Load%Description = 'Total H2 loads'
      Call AddVariable(rvEPowH2Load)

      rvEPowLoadAdj%VariableName = 'electrical_net/power_only_components/elec_load/elec_load_adjusted'
      rvEPowLoadAdj%MetaType = 'units'
      rvEPowLoadAdj%VariableType = '(W)'
      rvEPowLoadAdj%Description = 'Adjusted E-load'
      Call AddVariable(rvEPowLoadAdj)

      rvEPowPCUPowLoss%VariableName = 'electrical_net/power_only_components/*/misc_data/PCU_power_loss'
      rvEPowPCUPowLoss%MetaType = 'units'
      rvEPowPCUPowLoss%VariableType = '(W)'
      rvEPowPCUPowLoss%Description = 'Power loss from PCU'
      Call AddVariable(rvEPowPCUPowLoss)

      rvEPowPCUEff%VariableName = 'electrical_net/power_only_components/*/misc_data/PCU_efficiency'
      rvEPowPCUEff%MetaType = 'units'
      rvEPowPCUEff%VariableType = '(-)'
      rvEPowPCUEff%Description = 'Efficiency of PCU'
      Call AddVariable(rvEPowPCUEff)

      rvEPowPCUPowIn%VariableName = 'electrical_net/power_only_components/*/misc_data/PCU_power_input'
      rvEPowPCUPowIn%MetaType = 'units'
      rvEPowPCUPowIn%VariableType = '(W)'
      rvEPowPCUPowIn%Description = 'Power input to PCU'
      Call AddVariable(rvEPowPCUPowIn)

      rvEPowPCUPowOut%VariableName = 'electrical_net/power_only_components/*/misc_data/PCU_power_output'
      rvEPowPCUPowOut%MetaType = 'units'
      rvEPowPCUPowOut%VariableType = '(W)'
      rvEPowPCUPowOut%Description = 'Power output from PCU'
      Call AddVariable(rvEPowPCUPowOut)

      !Used by CETC_BATTERY.F
      rvEPowBattDem%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_system_demand'
      rvEPowBattDem%MetaType = 'units'
      rvEPowBattDem%VariableType = '(W)'
      rvEPowBattDem%Description = 'Demand on battery system'
      Call AddVariable(rvEPowBattDem)

      rvEPowBattOper%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_mode'
      rvEPowBattOper%MetaType = 'units'
      rvEPowBattOper%VariableType = '(-)'
      rvEPowBattOper%Description = 'Battery mode of operation'
      Call AddVariable(rvEPowBattOper)

      rvEPowBattVolt%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_voltage'
      rvEPowBattVolt%MetaType = 'units'
      rvEPowBattVolt%VariableType = '(V)'
      rvEPowBattVolt%Description = 'Battery operating voltage'
      Call AddVariable(rvEPowBattVolt)

      rvEPowBattDOD%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_DOD'
      rvEPowBattDOD%MetaType = 'units'
      rvEPowBattDOD%VariableType = '(-)'
      rvEPowBattDOD%Description = 'Battery depth of discharge'
      Call AddVariable(rvEPowBattDOD)

      rvEPowBattCurr%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_current'
      rvEPowBattCurr%MetaType = 'units'
      rvEPowBattCurr%VariableType = '(A)'
      rvEPowBattCurr%Description = 'Battery current'
      Call AddVariable(rvEPowBattCurr)

      rvEPowbattIntH%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_internal_heat'
      rvEPowbattIntH%MetaType = 'units'
      rvEPowbattIntH%VariableType = '(W)'
      rvEPowbattIntH%Description = 'Battery internal heat'
      Call AddVariable(rvEPowbattIntH)

      rvEPowBattSOC%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_SOC'
      rvEPowBattSOC%MetaType = 'units'
      rvEPowBattSOC%VariableType = '(%)'
      rvEPowBattSOC%Description = 'Battery state of charge'
      Call AddVariable(rvEPowBattSOC)

      rvEPowBattLd%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_load'
      rvEPowBattLd%MetaType = 'units'
      rvEPowBattLd%VariableType = '(W)'
      rvEPowBattLd%Description = 'Load on the battery'
      Call AddVariable(rvEPowBattLd)

      rvEPowBattMaxPow%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_max_power'
      rvEPowBattMaxPow%MetaType = 'units'
      rvEPowBattMaxPow%VariableType = '(W)'
      rvEPowBattMaxPow%Description = 'Battery max power'
      Call AddVariable(rvEPowBattMaxPow)

      rvEPowBattMaxPowChrg%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_max_charge_power'
      rvEPowBattMaxPowChrg%MetaType = 'units'
      rvEPowBattMaxPowChrg%VariableType = '(W)'
      rvEPowBattMaxPowChrg%Description = 'Battery max charge power'
      Call AddVariable(rvEPowBattMaxPowChrg)

      rvEPowBattMaxDschrg%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_max_discharge_power'
      rvEPowBattMaxDschrg%MetaType = 'units'
      rvEPowBattMaxDschrg%VariableType = '(W)'
      rvEPowBattMaxDschrg%Description = 'Battery max discharge power'
      Call AddVariable(rvEPowBattMaxDschrg)

      rvEPowBattMaxHPow%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_max_heater_power'
      rvEPowBattMaxHPow%MetaType = 'units'
      rvEPowBattMaxHPow%VariableType = '(W)'
      rvEPowBattMaxHPow%Description = 'Battery max heater power'
      Call AddVariable(rvEPowBattMaxHPow)

      rvEPowBattHPow%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_heater_power'
      rvEPowBattHPow%MetaType = 'units'
      rvEPowBattHPow%VariableType = '(W)'
      rvEPowBattHPow%Description = 'Battery heater power'
      Call AddVariable(rvEPowBattHPow)

      rvEPowBattPowBal%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_power_balance'
      rvEPowBattPowBal%MetaType = 'units'
      rvEPowBattPowBal%VariableType = '(W)'
      rvEPowBattPowBal%Description = 'Battery power balance'
      Call AddVariable(rvEPowBattPowBal)

      rvEPowBattSysLd%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_system_load'
      rvEPowBattSysLd%MetaType = 'units'
      rvEPowBattSysLd%VariableType = '(W)'
      rvEPowBattSysLd%Description = 'Load on battery system'
      Call AddVariable(rvEPowBattSysLd)

      rvEPowBattSysChrgLd%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_system_charge_load'
      rvEPowBattSysChrgLd%MetaType = 'units'
      rvEPowBattSysChrgLd%VariableType = '(W)'
      rvEPowBattSysChrgLd%Description = 'Charge load on battery system'
      Call AddVariable(rvEPowBattSysChrgLd)

      rvEPowBattSysDschrgLd%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_system_discharge_load'
      rvEPowBattSysDschrgLd%MetaType = 'units'
      rvEPowBattSysDschrgLd%VariableType = '(W)'
      rvEPowBattSysDschrgLd%Description = 'Discharge load on battery system'
      Call AddVariable(rvEPowBattSysDschrgLd)

      rvEPowBattTemp%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_temp'
      rvEPowBattTemp%MetaType = 'units'
      rvEPowBattTemp%VariableType = '(oC)'
      rvEPowBattTemp%Description = 'Battery temperature'
      Call AddVariable(rvEPowBattTemp)

      rvEPowBattLfUsed%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_life_used'
      rvEPowBattLfUsed%MetaType = 'units'
      rvEPowBattLfUsed%VariableType = '(years)'
      rvEPowBattLfUsed%Description = 'Battery life used'
      Call AddVariable(rvEPowBattLfUsed)

      rvEPowBattLfUsedCum%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_life_used_cum'
      rvEPowBattLfUsedCum%MetaType = 'units'
      rvEPowBattLfUsedCum%VariableType = '(years)'
      rvEPowBattLfUsedCum%Description = 'Cumulative battery life used'
      Call AddVariable(rvEPowBattLfUsedCum)

      rvEPowBattLfUseFac%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_life_use_factor'
      rvEPowBattLfUseFac%MetaType = 'units'
      rvEPowBattLfUseFac%VariableType = '(-)'
      rvEPowBattLfUseFac%Description = 'Battery life use factor'
      Call AddVariable(rvEPowBattLfUseFac)

      rvEPowBattChrgCyc%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_mand_charge_cycle'
      rvEPowBattChrgCyc%MetaType = 'units'
      rvEPowBattChrgCyc%VariableType = '(-)'
      rvEPowBattChrgCyc%Description = 'Battery mandatory charge cycle'
      Call AddVariable(rvEPowBattChrgCyc)

      rvEPowBattManChrgPh%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_mand_charge_phase'
      rvEPowBattManChrgPh%MetaType = 'units'
      rvEPowBattManChrgPh%VariableType = '(-)'
      rvEPowBattManChrgPh%Description = 'Battery mandatory charge phase'
      Call AddVariable(rvEPowBattManChrgPh)

      rvEPowBattManChrgPhInc%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_mand_charge_phase_increase'
      rvEPowBattManChrgPhInc%MetaType = 'units'
      rvEPowBattManChrgPhInc%VariableType = '(-)'
      rvEPowBattManChrgPhInc%Description = 'Battery mandatory charge phase increase'
      Call AddVariable(rvEPowBattManChrgPhInc)

      rvEPowBattTmeLstFulChrg%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_time_since_last_full_charge'
      rvEPowBattTmeLstFulChrg%MetaType = 'units'
      rvEPowBattTmeLstFulChrg%VariableType = '(hours)'
      rvEPowBattTmeLstFulChrg%Description = 'Battery time since last full charge'
      Call AddVariable(rvEPowBattTmeLstFulChrg)

      rvEPowBattBdTreatFlg%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_bad_treatment_flag'
      rvEPowBattBdTreatFlg%MetaType = 'units'
      rvEPowBattBdTreatFlg%VariableType = '(-)'
      rvEPowBattBdTreatFlg%Description = 'Battery abuse flag'
      Call AddVariable(rvEPowBattBdTreatFlg)


      rvEPowBattCtrlScn%VariableName = 'electrical_net/power_only_components/*/misc_data/battery_control_scenario'
      rvEPowBattCtrlScn%MetaType = 'units'
      rvEPowBattCtrlScn%VariableType = '(-)'
      rvEPowBattCtrlScn%Description = 'Battery control scenario'
      Call AddVariable(rvEPowBattCtrlScn)

      rvEPowBattLiOnCycUsed%VariableName = 'electrical_net/power_only_components/*/misc_data/Lion_cycles_used'
      rvEPowBattLiOnCycUsed%MetaType = 'units'
      rvEPowBattLiOnCycUsed%VariableType = '(-)'
      rvEPowBattLiOnCycUsed%Description = 'Li-on cycles used'
      Call AddVariable(rvEPowBattLiOnCycUsed)

      !Used by RE-H2-ctl.F
      rvCtrlReH2NPwElAct%VariableName = 'control/re_h2_ctl/net_power_balance/electrolyzer_active'
      rvCtrlReH2NPwElAct%MetaType = 'units'
      rvCtrlReH2NPwElAct%VariableType = '(W)'
      rvCtrlReH2NPwElAct%Description = 'Balance on AC bus when PV power is used to produce H2'
      Call AddVariable(rvCtrlReH2NPwElAct)

      rvCtrlReH2NPwBattChrg%VariableName = 'control/re_h2_ctl/net_power_balance/battery_charge'
      rvCtrlReH2NPwBattChrg%MetaType = 'units'
      rvCtrlReH2NPwBattChrg%VariableType = '(W)'
      rvCtrlReH2NPwBattChrg%Description = 'Balance on AC bus when PV power is used to charge battery'
      Call AddVariable(rvCtrlReH2NPwBattChrg)

      rvCtrlReH2NPwBattDschrg%VariableName = 'control/re_h2_ctl/net_power_balance/battery_discharge'
      rvCtrlReH2NPwBattDschrg%MetaType = 'units'
      rvCtrlReH2NPwBattDschrg%VariableType = '(W)'
      rvCtrlReH2NPwBattDschrg%Description = 'Balance on AC bus when battery is responding to loads'
      Call AddVariable(rvCtrlReH2NPwBattDschrg)

      rvCtrlReH2NPwPEMFCAct%VariableName = 'control/re_h2_ctl/net_power_balance/PEMFC_active'
      rvCtrlReH2NPwPEMFCAct%MetaType = 'units'
      rvCtrlReH2NPwPEMFCAct%VariableType = '(W)'
      rvCtrlReH2NPwPEMFCAct%Description = 'Balance on AC bus when PEMFC is responding to loads'
      Call AddVariable(rvCtrlReH2NPwPEMFCAct)

      !Used by hvacsim.F
      rvPltHvacComFuelAmnt%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/*/amount'
      rvPltHvacComFuelAmnt%MetaType = '*'
      rvPltHvacComFuelAmnt%VariableType = '*'
      rvPltHvacComFuelAmnt%Description = '*'
      Call AddVariable(rvPltHvacComFuelAmnt)
      
      rvPltHvacComFuelAmntHeat%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/*/amount/heating'
      rvPltHvacComFuelAmntHeat%MetaType = '*'
      rvPltHvacComFuelAmntHeat%VariableType = '*'
      rvPltHvacComFuelAmntHeat%Description = '*'
      Call AddVariable(rvPltHvacComFuelAmntHeat)

      rvPltHvacComFuelAmntCool%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/*/amount/cooling'
      rvPltHvacComFuelAmntCool%MetaType = '*'
      rvPltHvacComFuelAmntCool%VariableType = '*'
      rvPltHvacComFuelAmntCool%Description = '*'
      Call AddVariable(rvPltHvacComFuelAmntCool)

      rvPltHvacComFlAmntAux%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/*/amount/auxiliaries'
      rvPltHvacComFlAmntAux%MetaType = '*'
      rvPltHvacComFlAmntAux%VariableType = '*'
      rvPltHvacComFlAmntAux%Description = '*'
      Call AddVariable(rvPltHvacComFlAmntAux)

      rvPltHvacFuelEnPilot%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/energy_input/pilot'
      rvPltHvacFuelEnPilot%MetaType = '*'
      rvPltHvacFuelEnPilot%VariableType = '*'
      rvPltHvacFuelEnPilot%Description = '*'
      Call AddVariable(rvPltHvacFuelEnPilot)

      rvPltHvacEff%VariableName = 'plant/ideal_hvac_models/component_*/efficiency'
      rvPltHvacEff%MetaType = '*'
      rvPltHvacEff%VariableType = '*'
      rvPltHvacEff%Description = '*'
      Call AddVariable(rvPltHvacEff)

      rvPltHvacCOPHeat%VariableName = 'plant/ideal_hvac_models/component_*/COP/heating'
      rvPltHvacCOPHeat%MetaType = '*'
      rvPltHvacCOPHeat%VariableType = '*'
      rvPltHvacCOPHeat%Description = '*'
      Call AddVariable(rvPltHvacCOPHeat)

      rvPltHvacCOPCool%VariableName = 'plant/ideal_hvac_models/component_*/COP/cooling'
      rvPltHvacCOPCool%MetaType = '*'
      rvPltHvacCOPCool%VariableType = '*'
      rvPltHvacCOPCool%Description = '*'
      Call AddVariable(rvPltHvacCOPCool)

      rvPltHvacFuelEnHeat%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/energy_input/heating'
      rvPltHvacFuelEnHeat%MetaType = '*'
      rvPltHvacFuelEnHeat%VariableType = '*'
      rvPltHvacFuelEnHeat%Description = '*'
      Call AddVariable(rvPltHvacFuelEnHeat)

      rvPltHvacPrtLdHeat%VariableName = 'plant/ideal_hvac_models/component_*/part_load_ratio/heating'
      rvPltHvacPrtLdHeat%MetaType = '*'
      rvPltHvacPrtLdHeat%VariableType = '*'
      rvPltHvacPrtLdHeat%Description = '*'
      Call AddVariable(rvPltHvacPrtLdHeat)

      rvPltHvacThrmOutHeat%VariableName = 'plant/ideal_hvac_models/component_*/thermal_output/heating'
      rvPltHvacThrmOutHeat%MetaType = '*'
      rvPltHvacThrmOutHeat%VariableType = '*'
      rvPltHvacThrmOutHeat%Description = '*'
      Call AddVariable(rvPltHvacThrmOutHeat)

      rvPltHvacFuelEnCool%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/energy_input/cooling'
      rvPltHvacFuelEnCool%MetaType = '*'
      rvPltHvacFuelEnCool%VariableType = '*'
      rvPltHvacFuelEnCool%Description = '*'
      Call AddVariable(rvPltHvacFuelEnCool)

      rvPltHvacPrtCool%VariableName = 'plant/ideal_hvac_models/component_*/part_load_ratio/cooling'
      rvPltHvacPrtCool%MetaType = '*'
      rvPltHvacPrtCool%VariableType = '*'
      rvPltHvacPrtCool%Description = '*'
      Call AddVariable(rvPltHvacPrtCool)

      rvPltHvacThrmCool%VariableName = 'plant/ideal_hvac_models/component_*/thermal_output/cooling'
      rvPltHvacThrmCool%MetaType = '*'
      rvPltHvacThrmCool%VariableType = '*'
      rvPltHvacThrmCool%Description = '*'
      Call AddVariable(rvPltHvacThrmCool)

      rvPltHvacFuelEnTtl%VariableName = 'plant/ideal_hvac_models/component_*/fuel_use/energy_input/total'
      rvPltHvacFuelEnTtl%MetaType = '*'
      rvPltHvacFuelEnTtl%VariableType = '*'
      rvPltHvacFuelEnTtl%Description = '*'
      Call AddVariable(rvPltHvacFuelEnTtl)

      rvPltHvacCoilCoolSns%VariableName = 'plant/ideal_hvac_models/component_*/coil_load/cooling/sensible'
      rvPltHvacCoilCoolSns%MetaType = '*'
      rvPltHvacCoilCoolSns%VariableType = '*'
      rvPltHvacCoilCoolSns%Description = '*'
      Call AddVariable(rvPltHvacCoilCoolSns)

      rvPltHvacCoilCoolLtnt%VariableName = 'plant/ideal_hvac_models/component_*/coil_load/cooling/latent'
      rvPltHvacCoilCoolLtnt%MetaType = '*'
      rvPltHvacCoilCoolLtnt%VariableType = '*'
      rvPltHvacCoilCoolLtnt%Description = '*'
      Call AddVariable(rvPltHvacCoilCoolLtnt)

      rvPltHvacCoilCoolTtl%VariableName = 'plant/ideal_hvac_models/component_*/coil_load/cooling/total'
      rvPltHvacCoilCoolTtl%MetaType = '*'
      rvPltHvacCoilCoolTtl%VariableType = '*'
      rvPltHvacCoilCoolTtl%Description = '*'
      Call AddVariable(rvPltHvacCoilCoolTtl)

      rvPltHvacCrcFuelEnIn%VariableName = 'plant/ideal_hvac_models/circulation_fans/fuel_use/energy_input'
      rvPltHvacCrcFuelEnIn%MetaType = 'units'
      rvPltHvacCrcFuelEnIn%VariableType = '(W)'
      rvPltHvacCrcFuelEnIn%Description = 'Idealized HVAC models: energy used by circulation fans.'
      Call AddVariable(rvPltHvacCrcFuelEnIn)

      rvPltHvacCrcFElecAmnt%VariableName = 'plant/ideal_hvac_models/circulation_fans/fuel_use/electricity/amount'
      rvPltHvacCrcFElecAmnt%MetaType = 'units'
      rvPltHvacCrcFElecAmnt%VariableType = '(kWh/s)'
      rvPltHvacCrcFElecAmnt%Description = 'Idealized HVAC models: electricity used by circulation fans.'
      Call AddVariable(rvPltHvacCrcFElecAmnt)

      rvPltHvacCrcHeatTrn%VariableName = 'plant/ideal_hvac_models/circulation_fans/heat_transfer'
      rvPltHvacCrcHeatTrn%MetaType = 'units'
      rvPltHvacCrcHeatTrn%VariableType = '(W)'
      rvPltHvacCrcHeatTrn%Description = 'Idealized HVAC models: heat transferred to space by circulation fans.'
      Call AddVariable(rvPltHvacCrcHeatTrn)

      !Used by ashp_cooling.F
      rvPltHvacZnCapFrac%VariableName = 'plant/ideal_hvac_models/component_*/zone_cap_frac/zone_*'
      rvPltHvacZnCapFrac%MetaType = 'units'
      rvPltHvacZnCapFrac%VariableType = '(-)'
      rvPltHvacZnCapFrac%Description = 'Idealized HVAC models: cooling capacity fraction'
      Call AddVariable(rvPltHvacZnCapFrac)

      rvPltHvacZnRtCapFrac%VariableName = 'plant/ideal_hvac_models/component_*/zone_ret_cap_frac/zone_*'
      rvPltHvacZnRtCapFrac%MetaType = 'units'
      rvPltHvacZnRtCapFrac%VariableType = '(-)'
      rvPltHvacZnRtCapFrac%Description = 'Idealized HVAC models: return capacity fraction'
      Call AddVariable(rvPltHvacZnRtCapFrac)

      rvPltHvacCrcFlwRt%VariableName = 'plant/ideal_hvac_models/component_*/circ_flow_rate'
      rvPltHvacCrcFlwRt%MetaType = 'units'
      rvPltHvacCrcFlwRt%VariableType = '(m3/s)'
      rvPltHvacCrcFlwRt%Description = 'Idealized HVAC models: circulation flow rate'
      Call AddVariable(rvPltHvacCrcFlwRt)


      rvPltHvacOutFanPw%VariableName = 'plant/ideal_hvac_models/component_*/outdoor_fan_power'
      rvPltHvacOutFanPw%MetaType = 'units'
      !C.L.to match the testing, removed unit that was originaly not there. rvPltHvacOutFanPw%VariableType = '(W)'
      rvPltHvacOutFanPw%VariableType = ''
      rvPltHvacOutFanPw%Description = 'Idealized HVAC models: outdoor fan power'
      Call AddVariable(rvPltHvacOutFanPw)

      !Used by trnsys_wrapper.F
      rvPltMiscData%VariableName = 'plant/*/misc_data/*'
      rvPltMiscData%MetaType = '*'
      rvPltMiscData%VariableType = '*'
      rvPltMiscData%Description = '*'
      Call AddVariable(rvPltMiscData)

      !Used by CentralVentSys.F
      rvPltHvacHRVElecLd%VariableName = 'plant/ideal_hvac_models/HRV/elec_load'
      rvPltHvacHRVElecLd%MetaType = 'units'
      rvPltHvacHRVElecLd%VariableType = '(W)'
      rvPltHvacHRVElecLd%Description = 'Idealized HVAC models: Amount of electricity' &
            // ' used by Ventilation/HRV system'
      Call AddVariable(rvPltHvacHRVElecLd)

      !Used by DHW_module.F
      rvPltDHWWtr%VariableName = 'plant/ideal_DHW_model/water_draw'
      rvPltDHWWtr%MetaType = 'units'
      rvPltDHWWtr%VariableType = '(l/s)'
      rvPltDHWWtr%Description = 'Ideal DWH model: timestep-averaged volumetric water draw'
      Call AddVariable(rvPltDHWWtr)

      rvPltDHWEnIn%VariableName = 'plant/ideal_DHW_model/energy_input'
      rvPltDHWEnIn%MetaType = 'units'
      rvPltDHWEnIn%VariableType = '(W)'
      rvPltDHWEnIn%Description = 'Ideal DWH model: energy requirement'
      Call AddVariable(rvPltDHWEnIn)

      rvPltDHWSupTmp%VariableName = 'plant/ideal_DHW_model/supply_temperature'
      rvPltDHWSupTmp%MetaType = 'units'
      rvPltDHWSupTmp%VariableType = '(oC)'
      rvPltDHWSupTmp%Description = 'Ideal DHW model: temperature of make-up water'
      Call AddVariable(rvPltDHWSupTmp)

      rvPltDHWDlvTmp%VariableName = 'plant/ideal_DHW_model/delivery_temperature'
      rvPltDHWDlvTmp%MetaType = 'units'
      rvPltDHWDlvTmp%VariableType = '(oC)'
      rvPltDHWDlvTmp%Description = 'Ideal DHW model: temperature at which water is delivered'
      Call AddVariable(rvPltDHWDlvTmp)

      rvPltDHWBrnHOut%VariableName = 'plant/ideal_DHW_model/burner_heat_output'
      rvPltDHWBrnHOut%MetaType = 'units'
      rvPltDHWBrnHOut%VariableType = '(W)'
      rvPltDHWBrnHOut%Description = 'Ideal DHW model: heat delivered by burner'
      Call AddVariable(rvPltDHWBrnHOut)

      rvPltDHWSknLss%VariableName = 'plant/ideal_DHW_model/skin_loss'
      rvPltDHWSknLss%MetaType = 'units'
      rvPltDHWSknLss%VariableType = '(W)'
      rvPltDHWSknLss%Description = 'Ideal DHW model: skin-loss to surroundings'
      Call AddVariable(rvPltDHWSknLss)

      rvPltDHWFluLss%VariableName = 'plant/ideal_DHW_model/flue_loss'
      rvPltDHWFluLss%MetaType = 'units'
      rvPltDHWFluLss%VariableType = '(W)'
      rvPltDHWFluLss%Description = 'Ideal DHW model: thermal energy lost in exhaust gases'
      Call AddVariable(rvPltDHWFluLss)

      rvPltDHWHTrnBld%VariableName = 'plant/ideal_DHW_model/heat_transfer_to_building'
      rvPltDHWHTrnBld%MetaType = 'units'
      rvPltDHWHTrnBld%VariableType = '(W)'
      rvPltDHWHTrnBld%Description = 'Ideal DHW model: rate of heat transfer to surrounding space'
      Call AddVariable(rvPltDHWHTrnBld)

      rvPltDHWWtrHLd%VariableName = 'plant/ideal_DHW_model/water_heating_load'
      rvPltDHWWtrHLd%MetaType = 'units'
      rvPltDHWWtrHLd%VariableType = '(W)'
      rvPltDHWWtrHLd%Description = 'Ideal DHW model: theoretical minimum heat required to meet' &
                  // ' water heating requirements'
      Call AddVariable(rvPltDHWWtrHLd)

      !Used by h2_comps.F
      rvPltOprtSgn%VariableName = 'plant/*/misc_data/operating_signal'
      rvPltOprtSgn%MetaType = 'units'
      rvPltOprtSgn%VariableType = '(-)'
      rvPltOprtSgn%Description = 'Electrolyzer operating signal (on/off)'
      Call AddVariable(rvPltOprtSgn)

      rvPltStndSgn%VariableName = 'plant/*/misc_data/standby_signal'
      rvPltStndSgn%MetaType = 'units'
      rvPltStndSgn%VariableType = '(-)'
      rvPltStndSgn%Description = 'Electrolyzer standby signal (on/off)'
      Call AddVariable(rvPltStndSgn)

      rvPltCtrlSgnEltrzr%VariableName = 'plant/*/misc_data/control_signal'
      rvPltCtrlSgnEltrzr%MetaType = 'units'
      rvPltCtrlSgnEltrzr%VariableType = '(W)'
      rvPltCtrlSgnEltrzr%Description = 'Power available for input to electrolyzer'
      Call AddVariable(rvPltCtrlSgnEltrzr)

      rvPltTtlAuxPow%VariableName = 'plant/*/misc_data/total_aux_power'
      rvPltTtlAuxPow%MetaType = 'units'
      rvPltTtlAuxPow%VariableType = '(W)'
      rvPltTtlAuxPow%Description = 'Total power consumed by auxiliary equipment'
      Call AddVariable(rvPltTtlAuxPow)

      rvPltDCAuxPow%VariableName = 'plant/*/misc_data/DC_aux_power'
      rvPltDCAuxPow%MetaType = 'units'
      rvPltDCAuxPow%VariableType = '(W)'
      rvPltDCAuxPow%Description = 'DC power consumed by auxiliary equipment'
      Call AddVariable(rvPltDCAuxPow)

      rvPltACAuxPow%VariableName = 'plant/*/misc_data/AC_aux_power'
      rvPltACAuxPow%MetaType = 'units'
      rvPltACAuxPow%VariableType = '(W)'
      rvPltACAuxPow%Description = 'AC power consumed by auxiliary equipment'
      Call AddVariable(rvPltACAuxPow)

      rvPltStckPow%VariableName = 'plant/*/misc_data/stack_power'
      rvPltStckPow%MetaType = 'units'
      rvPltStckPow%VariableType = '(W)'
      rvPltStckPow%Description = 'Power consumed by electrolyzer stack'
      Call AddVariable(rvPltStckPow)

      rvPltMxStckPow%VariableName = 'plant/*/misc_data/max_stack_power'
      rvPltMxStckPow%MetaType = 'units'
      rvPltMxStckPow%VariableType = '(W)'
      rvPltMxStckPow%Description = 'Maximum power input to electrolyzer stack'
      Call AddVariable(rvPltMxStckPow)

      rvPltOpMode%VariableName = 'plant/*/misc_data/op_mode'
      rvPltOpMode%MetaType = 'units'
      rvPltOpMode%VariableType = '(-)'
      rvPltOpMode%Description = 'Operating mode of electrolyzer'
      Call AddVariable(rvPltOpMode)

      rvPltDCPowDmd%VariableName = 'plant/*/misc_data/DC_power_demand'
      rvPltDCPowDmd%MetaType = 'units'
      rvPltDCPowDmd%VariableType = '(W)'
      rvPltDCPowDmd%Description = 'DC power demand of electrolyzer'
      Call AddVariable(rvPltDCPowDmd)

      rvPltACPowDmd%VariableName = 'plant/*/misc_data/AC_power_demand'
      rvPltACPowDmd%MetaType = 'units'
      rvPltACPowDmd%VariableType = '(W)'
      rvPltACPowDmd%Description = 'AC power demand of electrolyzer'
      Call AddVariable(rvPltACPowDmd)

      rvPltElecEffEltrzr%VariableName = 'plant/*/misc_data/elec_efficiency'
      rvPltElecEffEltrzr%MetaType = 'units'
      rvPltElecEffEltrzr%VariableType = '(-)'
      rvPltElecEffEltrzr%Description = 'Electrical efficiency of electrolyzer'
      Call AddVariable(rvPltElecEffEltrzr)

      rvPltH2FlwKgs%VariableName = 'plant/*/misc_data/H2_flow_kgs'
      rvPltH2FlwKgs%MetaType = 'units'
      rvPltH2FlwKgs%VariableType = '(kg/s)'
      rvPltH2FlwKgs%Description = 'Net production rate of H2'
      Call AddVariable(rvPltH2FlwKgs)

      rvPlt02FlwKgs%VariableName = 'plant/*/misc_data/O2_flow_kgs'
      rvPlt02FlwKgs%MetaType = 'units'
      rvPlt02FlwKgs%VariableType = '(kg/s)'
      rvPlt02FlwKgs%Description = 'Net production rate of O2'
      Call AddVariable(rvPlt02FlwKgs)

      rvPltH20FlwKgs%VariableName = 'plant/*/misc_data/H2O_flow_kgs'
      rvPltH20FlwKgs%MetaType = 'units'
      rvPltH20FlwKgs%VariableType = '(kg/s)'
      rvPltH20FlwKgs%Description = 'Net consumption rate of H2O'
      Call AddVariable(rvPltH20FlwKgs)

      rvPltQIntPrst%VariableName = 'plant/*/misc_data/Qinternal_present'
      rvPltQIntPrst%MetaType = 'units'
      rvPltQIntPrst%VariableType = '(W)'
      rvPltQIntPrst%Description = 'Internal heat generated by electrolyser'
      Call AddVariable(rvPltQIntPrst)

      rvPltQlssPrst%VariableName = 'plant/*/misc_data/Qloss_present'
      rvPltQlssPrst%MetaType = 'units'
      rvPltQlssPrst%VariableType = '(W)'
      rvPltQlssPrst%Description = 'Ambient heat loss'
      Call AddVariable(rvPltQlssPrst)

      rvPltQCoolPrst%VariableName = 'plant/*/misc_data/Qcool_present'
      rvPltQCoolPrst%MetaType = 'units'
      rvPltQCoolPrst%VariableType = '(W)'
      rvPltQCoolPrst%Description = 'Heat transfer to cooling water'
      Call AddVariable(rvPltQCoolPrst)

      rvPltQInltWtrHtg%VariableName = 'plant/*/misc_data/Qinlet_water_htg'
      rvPltQInltWtrHtg%MetaType = 'units'
      rvPltQInltWtrHtg%VariableType = '(W)'
      rvPltQInltWtrHtg%Description = 'Energy required to heat inlet water'
      Call AddVariable(rvPltQInltWtrHtg)

      rvPltTnltWtr%VariableName = 'plant/*/misc_data/Tinlet_water'
      rvPltTnltWtr%MetaType = 'units'
      rvPltTnltWtr%VariableType = '(C)'
      rvPltTnltWtr%Description = 'Temperature of inlet water'
      Call AddVariable(rvPltTnltWtr)

      !Used by h2_matrix_library.F
      rvPltElecWrk%VariableName = 'plant/*/misc_data/electricalWork'
      rvPltElecWrk%MetaType = 'units'
      rvPltElecWrk%VariableType = '(W)'
      rvPltElecWrk%Description = 'Compressor power consumption'
      Call AddVariable(rvPltElecWrk)

      rvPltMchWrk%VariableName = 'plant/*/misc_data/mechanicalWork'
      rvPltMchWrk%MetaType = 'units'
      rvPltMchWrk%VariableType = '(W)'
      rvPltMchWrk%Description = 'Compressor work requirement'
      Call AddVariable(rvPltMchWrk)

      rvPltHFlow%VariableName = 'plant/*/misc_data/heatFlow'
      rvPltHFlow%MetaType = 'units'
      rvPltHFlow%VariableType = '(W)'
      rvPltHFlow%Description = 'Compressor heat generation'
      Call AddVariable(rvPltHFlow)

      rvPltHydgnIn%VariableName = 'plant/*/misc_data/hydrogenFlow_in'
      rvPltHydgnIn%MetaType = 'units'
      rvPltHydgnIn%VariableType = '(kg/s)'
      rvPltHydgnIn%Description = ''
      Call AddVariable(rvPltHydgnIn)

      rvPltHydgnOut%VariableName = 'plant/*/misc_data/hydrogenFlow_out'
      rvPltHydgnOut%MetaType = 'units'
      rvPltHydgnOut%VariableType = '(kg/s)'
      rvPltHydgnOut%Description = ''
      Call AddVariable(rvPltHydgnOut)

      rvPltHydgnLss%VariableName = 'plant/*/misc_data/hydrogenLoss'
      rvPltHydgnLss%MetaType = 'units'
      rvPltHydgnLss%VariableType = '(kg/s)'
      rvPltHydgnLss%Description = ''
      Call AddVariable(rvPltHydgnLss)

      rvPltHydgnStrPrs%VariableName = 'plant/*/misc_data/hydrogen_storage/present'
      rvPltHydgnStrPrs%MetaType = 'units'
      rvPltHydgnStrPrs%VariableType = '(kg)'
      rvPltHydgnStrPrs%Description = ''
      Call AddVariable(rvPltHydgnStrPrs)

      rvPltHydgnStrFut%VariableName = 'plant/*/misc_data/hydrogen_storage/future'
      rvPltHydgnStrFut%MetaType = 'units'
      rvPltHydgnStrFut%VariableType = '(kg)'
      rvPltHydgnStrFut%Description = ''
      Call AddVariable(rvPltHydgnStrFut)

      rvPltElecFlx%VariableName = 'plant/*/misc_data/electrical_flux'
      rvPltElecFlx%MetaType = 'units'
      rvPltElecFlx%VariableType = '(W)'
      rvPltElecFlx%Description = ''
      Call AddVariable(rvPltElecFlx)

      rvPltUndlvFlxSysSz%VariableName = 'plant/*/misc_data/undelivered_flux/system_size'
      rvPltUndlvFlxSysSz%MetaType = 'units'
      rvPltUndlvFlxSysSz%VariableType = '(W)'
      rvPltUndlvFlxSysSz%Description = ''
      Call AddVariable(rvPltUndlvFlxSysSz)

      rvZZZ%VariableName = 'zzz/requested'
      rvZZZ%MetaType = '?'
      rvZZZ%VariableType = '?'
      rvZZZ%Description = ''
      Call AddVariable(rvZZZ)

      rvZZZZ%VariableName = 'zzz/z_recieved'
      rvZZZZ%MetaType = '?'
      rvZZZZ%VariableType = '?'
      rvZZZZ%Description = ''
      Call AddVariable(rvZZZZ)

      rvPltHFlx%VariableName = 'plant/*/misc_data/heat_flux'
      rvPltHFlx%MetaType = 'units'
      rvPltHFlx%VariableType = '(W)'
      rvPltHFlx%Description = ''
      Call AddVariable(rvPltHFlx)

      rvPltCtrlSgn%VariableName = 'plant/*/misc_data/control_signal'
      rvPltCtrlSgn%MetaType = 'units'
      rvPltCtrlSgn%VariableType = '(W)'
      rvPltCtrlSgn%Description = ''
      Call AddVariable(rvPltCtrlSgn)

      rvPltFlxCnstrnSz%VariableName = 'plant/*/misc_data/flux_constrained_by_system_size'
      rvPltFlxCnstrnSz%MetaType = 'units'
      rvPltFlxCnstrnSz%VariableType = '(W)'
      rvPltFlxCnstrnSz%Description = ''
      Call AddVariable(rvPltFlxCnstrnSz)

      rvPltElecEff%VariableName = 'plant/*/misc_data/electrical_eff'
      rvPltElecEff%MetaType = 'units'
      rvPltElecEff%VariableType = '-'
      rvPltElecEff%Description = ''
      Call AddVariable(rvPltElecEff)

      rvPltEnLss%VariableName = 'plant/*/misc_data/energy_losses'
      rvPltEnLss%MetaType = 'units'
      rvPltEnLss%VariableType = '(W)'
      rvPltEnLss%Description = ''
      Call AddVariable(rvPltEnLss)

      rvPltLdRt%VariableName = 'plant/*/misc_data/load_ratio'
      rvPltLdRt%MetaType = 'units'
      rvPltLdRt%VariableType = '-'
      rvPltLdRt%Description = ''
      Call AddVariable(rvPltLdRt)

      rvPltH2Dmd%VariableName = 'plant/*/misc_data/H2_demand'
      rvPltH2Dmd%MetaType = 'units'
      rvPltH2Dmd%VariableType = '(kg/s)'
      rvPltH2Dmd%Description = ''
      Call AddVariable(rvPltH2Dmd)

      rvPltAuxFuelFlw%VariableName = 'plant/*/misc_data/auxiliary_fuel_flow'
      rvPltAuxFuelFlw%MetaType = 'units'
      rvPltAuxFuelFlw%VariableType = '(kg/s)'
      rvPltAuxFuelFlw%Description = ''
      Call AddVariable(rvPltAuxFuelFlw)

      rvPltUndlvFlxH2Shrt%VariableName = 'plant/*/misc_data/undelivered_flux/H2_shortage'
      rvPltUndlvFlxH2Shrt%MetaType = 'units'
      rvPltUndlvFlxH2Shrt%VariableType = '(W)'
      rvPltUndlvFlxH2Shrt%Description = ''
      Call AddVariable(rvPltUndlvFlxH2Shrt)

      rvPltThrmEff%VariableName = 'plant/*/misc_data/thermal_eff'
      rvPltThrmEff%MetaType = 'units'
      rvPltThrmEff%VariableType = '-'
      rvPltThrmEff%Description = ''
      Call AddVariable(rvPltThrmEff)

      rvPltOxgnFlw%VariableName = 'plant/*/misc_data/oxygen_flow'
      rvPltOxgnFlw%MetaType = 'units'
      rvPltOxgnFlw%VariableType = '(kg/s)'
      rvPltOxgnFlw%Description = ''
      Call AddVariable(rvPltOxgnFlw)

      rvPltWtrFlw%VariableName = 'plant/*/misc_data/water_flow'
      rvPltWtrFlw%MetaType = 'units'
      rvPltWtrFlw%VariableType = '(kg/s)'
      rvPltWtrFlw%Description = ''
      Call AddVariable(rvPltWtrFlw)



      !Used by h3kstore.F
      rvPltSDHWSumPmpElec%VariableName = 'plant/SDHW_summary/pump_elec'
      rvPltSDHWSumPmpElec%MetaType = 'units'
      rvPltSDHWSumPmpElec%VariableType = '(W)'
      rvPltSDHWSumPmpElec%Description = 'SDHW system: Electric consumption of the all pumps'
      Call AddVariable(rvPltSDHWSumPmpElec)

      !Used by basesimp.F
      rvBsImpQag%VariableName = 'basesimp/Qag'
      rvBsImpQag%MetaType = 'units'
      rvBsImpQag%VariableType = '(W)'
      rvBsImpQag%Description = 'Basesimp: Above ground heat loss'
      Call AddVariable(rvBsImpQag)

      rvBsImpQbgAvg%VariableName = 'basesimp/Qbgavg'
      rvBsImpQbgAvg%MetaType = 'units'
      rvBsImpQbgAvg%VariableType = '(W)'
      rvBsImpQbgAvg%Description = 'Basesimp: Average below ground heat loss'
      Call AddVariable(rvBsImpQbgAvg)

      rvBsImpQbgVar%VariableName = 'basesimp/Qbgvar'
      rvBsImpQbgVar%MetaType = 'units'
      rvBsImpQbgVar%VariableType = '(W)'
      rvBsImpQbgVar%Description = 'Basesimp: Variable below ground heat loss'
      Call AddVariable(rvBsImpQbgVar)

      rvBsImpQTtl%VariableName = 'basesimp/Qtotal'
      rvBsImpQTtl%MetaType = 'units'
      rvBsImpQTtl%VariableType = '(W)'
      rvBsImpQTtl%Description = 'Basesimp: Total heat loss'
      Call AddVariable(rvBsImpQTtl)

      rvBsImpTBSAvg%VariableName = 'basesimp/TBSAVG'
      rvBsImpTBSAvg%MetaType = 'units'
      rvBsImpTBSAvg%VariableType = '(oC)'
      rvBsImpTBSAvg%Description = 'Basesimp: Total heat loss'
      Call AddVariable(rvBsImpTBSAvg)

      rvBsImpTamb%VariableName = 'basesimp/Tamb'
      rvBsImpTamb%MetaType = 'units'
      rvBsImpTamb%VariableType = '(oC)'
      rvBsImpTamb%Description = 'Basesimp: Ambient temperature'
      Call AddVariable(rvBsImpTamb)

      rvBsImpBsTGAvg%VariableName = 'basesimp/bsTGavg'
      rvBsImpBsTGAvg%MetaType = 'units'
      rvBsImpBsTGAvg%VariableType = '(oC)'
      rvBsImpBsTGAvg%Description = 'Basesimp: Average ground temperature'
      Call AddVariable(rvBsImpBsTGAvg)

      !Used by spmatl.F
      rvBldSPMatlEffIrr%VariableName = 'building/spmatl/*/misc_data/eff_irradiance/unit_area'
      rvBldSPMatlEffIrr%MetaType = 'units'
      rvBldSPMatlEffIrr%VariableType = 'W/m^2'
      rvBldSPMatlEffIrr%Description = 'Effective irradiance of PV module (/m^2)'
      Call AddVariable(rvBldSPMatlEffIrr)

      rvBldSPMatlIncAngl%VariableName = 'building/spmatl/*/misc_data/incidence_angle'
      rvBldSPMatlIncAngl%MetaType = 'units'
      rvBldSPMatlIncAngl%VariableType = '-'
      rvBldSPMatlIncAngl%Description = 'Angle of incidence'
      Call AddVariable(rvBldSPMatlIncAngl)

      rvBldSPMatlTrnsAngl%VariableName = 'building/spmatl/*/misc_data/transmittance_inc_angle'
      rvBldSPMatlTrnsAngl%MetaType = 'units'
      rvBldSPMatlTrnsAngl%VariableType = '-'
      rvBldSPMatlTrnsAngl%Description = 'Transmittance at angle of incidence'
      Call AddVariable(rvBldSPMatlTrnsAngl)

      rvBldSPMatlTrns0%VariableName = 'building/spmatl/*/misc_data/transmittance_0'
      rvBldSPMatlTrns0%MetaType = 'units'
      rvBldSPMatlTrns0%VariableType = '-'
      rvBldSPMatlTrns0%Description = 'Transmittance at 0 degrees'
      Call AddVariable(rvBldSPMatlTrns0)

      rvBldSPMatlTrns60%VariableName = 'building/spmatl/*/misc_data/transmittance_60'
      rvBldSPMatlTrns60%MetaType = 'units'
      rvBldSPMatlTrns60%VariableType = '(-)'
      rvBldSPMatlTrns60%Description = 'Transmittance at 60 degrees'
      Call AddVariable(rvBldSPMatlTrns60)

      rvBldSPMatlTtlIncAr%VariableName = 'building/spmatl/*/misc_data/total_incident_irradiance/unit_area'
      rvBldSPMatlTtlIncAr%MetaType = 'units'
      rvBldSPMatlTtlIncAr%VariableType = '(W/m^2)'
      rvBldSPMatlTtlIncAr%Description = 'Total irradiance of PV module (/m^2'
      Call AddVariable(rvBldSPMatlTtlIncAr)

      rvBldSPMatlTtlIncTtl%VariableName = 'building/spmatl/*/misc_data/total_incident_irradiance/total'
      rvBldSPMatlTtlIncTtl%MetaType = 'units'
      rvBldSPMatlTtlIncTtl%VariableType = '(W)'
      rvBldSPMatlTtlIncTtl%Description = 'Total irradiance of PV module'
      Call AddVariable(rvBldSPMatlTtlIncTtl)

      rvBldSPMatlDrtIncAr%VariableName = 'building/spmatl/*/misc_data/direct_incident_irradiance/unit_area'
      rvBldSPMatlDrtIncAr%MetaType = 'units'
      rvBldSPMatlDrtIncAr%VariableType = '(W/m^2)'
      rvBldSPMatlDrtIncAr%Description = 'Direct irradiance of PV module (/m^2)'
      Call AddVariable(rvBldSPMatlDrtIncAr)

      rvBldSPMatlDffIncAr%VariableName = 'building/spmatl/*/misc_data/diffuse_incident_irradiance/unit_area'
      rvBldSPMatlDffIncAr%MetaType = 'units'
      rvBldSPMatlDffIncAr%VariableType = '(W/m^2)'
      rvBldSPMatlDffIncAr%Description = 'Diffuse irradiance of PV module (/m^2)'
      Call AddVariable(rvBldSPMatlDffIncAr)

      rvBldSPMatlPVPow%VariableName = 'building/spmatl/*/misc_data/pv_power'
      rvBldSPMatlPVPow%MetaType = 'units'
      rvBldSPMatlPVPow%VariableType = '(W)'
      rvBldSPMatlPVPow%Description = 'Power produced by PV module'
      Call AddVariable(rvBldSPMatlPVPow)

      rvBuiSpm%VariableName = 'bui/spm/*/z*/*'
      rvBuiSpm%MetaType = '*'
      rvBuiSpm%VariableType = '*'
      rvBuiSpm%Description = '*'
      Call AddVariable(rvBuiSpm)

      !Used by BC_data.F
      rvBndCndStpInt%VariableName = 'boundry_conditions/*/step_interpolation'
      rvBndCndStpInt%MetaType = '*'
      rvBndCndStpInt%VariableType = '*'
      rvBndCndStpInt%Description = '*'
      Call AddVariable(rvBndCndStpInt)

      rvBndCndLnInt%VariableName = 'boundry_conditions/*/linear_interpolation'
      rvBndCndLnInt%MetaType = '*'
      rvBndCndLnInt%VariableType = '*'
      rvBndCndLnInt%Description = '*'
      Call AddVariable(rvBndCndLnInt)

      !Used by PEM_FC_pt1.F
      rvpltFCellQElecNet%VariableName = 'plant/fuel_cell/Q_electric_net'
      rvpltFCellQElecNet%MetaType = 'units'
      rvpltFCellQElecNet%VariableType = '(W)'
      rvpltFCellQElecNet%Description = ''
      Call AddVariable(rvpltFCellQElecNet)

      rvpltFCellQElecPrstc%VariableName = 'plant/fuel_cell/Q_electric_parasitic'
      rvpltFCellQElecPrstc%MetaType = 'units'
      rvpltFCellQElecPrstc%VariableType = '(W)'
      rvpltFCellQElecPrstc%Description = ''
      Call AddVariable(rvpltFCellQElecPrstc)

      rvpltFCellQthrmNet%VariableName = 'plant/fuel_cell/Q_thermal_net'
      rvpltFCellQthrmNet%MetaType = 'units'
      rvpltFCellQthrmNet%VariableType = '(W)'
      rvpltFCellQthrmNet%Description = ''
      Call AddVariable(rvpltFCellQthrmNet)

      rvpltFCellFuelHHV%VariableName = 'plant/fuel_cell/Fuel_HHV'
      rvpltFCellFuelHHV%MetaType = 'units'
      rvpltFCellFuelHHV%VariableType = '(WattsToGJ)'
      rvpltFCellFuelHHV%Description = ''
      Call AddVariable(rvpltFCellFuelHHV)

      rvpltFCellElecEffBOP%VariableName = 'plant/fuel_cell/Electric_efficiency_BOP'
      rvpltFCellElecEffBOP%MetaType = 'units'
      rvpltFCellElecEffBOP%VariableType = '(%)'
      rvpltFCellElecEffBOP%Description = ''
      Call AddVariable(rvpltFCellElecEffBOP)

      rvpltFCellCogenEff%VariableName = 'plant/fuel_cell/Cogeneration_efficiency'
      rvpltFCellCogenEff%MetaType = 'units'
      rvpltFCellCogenEff%VariableType = '(%)'
      rvpltFCellCogenEff%Description = ''
      Call AddVariable(rvpltFCellCogenEff)

      rvpltFCellFuelFlw%VariableName = 'plant/fuel_cell/fuel_flow'
      rvpltFCellFuelFlw%MetaType = 'units'
      rvpltFCellFuelFlw%VariableType = '(kg/s)'
      rvpltFCellFuelFlw%Description = ''
      Call AddVariable(rvpltFCellFuelFlw)

      !Used by fort.f
      rvBldInfAirInf%VariableName = 'building/infiltration/air_infiltration'
      rvBldInfAirInf%MetaType = 'units'
      rvBldInfAirInf%VariableType = '(m3/s)'
      rvBldInfAirInf%Description = ''
      Call AddVariable(rvBldInfAirInf)

      rvBldInfAirChg%VariableName = 'building/infiltration/air_changes_per_hour'
      rvBldInfAirChg%MetaType = 'units'
      rvBldInfAirChg%VariableType = '(ACH)'
      rvBldInfAirChg%Description = ''
      Call AddVariable(rvBldInfAirChg)

      !Used by TCC.F
      rvpltCosimInvocations%VariableName = 'plant/co-sim/Invocations'
      rvpltCosimInvocations%MetaType = 'units'
      rvpltCosimInvocations%VariableType = '-'
      rvpltCosimInvocations%Description = ''
      Call AddVariable(rvpltCosimInvocations)

      rvpltCosimEsprIter%VariableName = 'plant/co-sim/Total_Esp-r_Iterations'
      rvpltCosimEsprIter%MetaType = 'units'
      rvpltCosimEsprIter%VariableType = '-'
      rvpltCosimEsprIter%Description = ''
      Call AddVariable(rvpltCosimEsprIter)

      rvpltCosimTrnsysIter%VariableName = 'plant/co-sim/Total_Trnsys_Iterations'
      rvpltCosimTrnsysIter%MetaType = 'units'
      rvpltCosimTrnsysIter%VariableType = '-'
      rvpltCosimTrnsysIter%Description = ''
      Call AddVariable(rvpltCosimTrnsysIter)

      rvpltHCCTempToTrnsys%VariableName ='plant/co-sim/HCC_*/HCC_Temp_to_Trnsys'
      rvpltHCCTempToTrnsys%MetaType = 'units'
      rvpltHCCTempToTrnsys%VariableType = '(oC)'
      rvpltHCCTempToTrnsys%Description = ''
      Call AddVariable(rvpltHCCTempToTrnsys)

      rvpltHCCFlowToTrnsys%VariableName = 'plant/co-sim/HCC_*/HCC_Flow_To_Trnsys'
      rvpltHCCFlowToTrnsys%MetaType = 'units'
      rvpltHCCFlowToTrnsys%VariableType = 'kg/s'
      rvpltHCCFlowToTrnsys%Description = ''
      Call AddVariable(rvpltHCCFlowToTrnsys)
            
      rvpltACCTempToTrnsys%VariableName = 'plant/co-sim/ACC_*/ACC_Temp_to_Trnsys'
      rvpltACCTempToTrnsys%MetaType = 'units'
      rvpltACCTempToTrnsys%VariableType = '(oC)'
      rvpltACCTempToTrnsys%Description = ''
      Call AddVariable(rvpltACCTempToTrnsys)

      rvpltACCFlowToTrnsys%VariableName = 'plant/co-sim/ACC_*/ACC_Flow_To_Trnsys'
      rvpltACCFlowToTrnsys%MetaType = 'units'
      rvpltACCFlowToTrnsys%VariableType = 'kg/s'
      rvpltACCFlowToTrnsys%Description = ''
      Call AddVariable(rvpltACCFlowToTrnsys)

      rvpltACCMoistFlowToTrnsys%VariableName = 'plant/co-sim/ACC_*/ACC_MoistFlow_To_Trnsys'
      rvpltACCMoistFlowToTrnsys%MetaType = 'units'
      rvpltACCMoistFlowToTrnsys%VariableType = 'kg/s'
      rvpltACCMoistFlowToTrnsys%Description = ''
      Call AddVariable(rvpltACCMoistFlowToTrnsys)
            
      rvpltHCCTempToEspr%VariableName = 'plant/co-sim/HCC_*/HCC_Temp_to_Espr'
      rvpltHCCTempToEspr%MetaType = 'units'
      rvpltHCCTempToEspr%VariableType = '(oC)'
      rvpltHCCTempToEspr%Description = ''
      Call AddVariable(rvpltHCCTempToEspr)

      rvpltHCCFlowToEspr%VariableName = 'plant/co-sim/HCC_*/HCC_Flow_To_Espr'
      rvpltHCCFlowToEspr%MetaType = 'units'
      rvpltHCCFlowToEspr%VariableType = 'kg/s'
      rvpltHCCFlowToEspr%Description = ''
      Call AddVariable(rvpltHCCFlowToEspr)
            
      rvpltACCTempToEspr%VariableName = 'plant/co-sim/ACC_*/ACC_Temp_to_Espr'
      rvpltACCTempToEspr%MetaType = 'units'
      rvpltACCTempToEspr%VariableType = '(oC)'
      rvpltACCTempToEspr%Description = ''
      Call AddVariable(rvpltACCTempToEspr)

      rvpltACCFlowToEspr%VariableName = 'plant/co-sim/ACC_*/ACC_Flow_To_Espr'
      rvpltACCFlowToEspr%MetaType = 'units'
      rvpltACCFlowToEspr%VariableType = 'kg/s'
      rvpltACCFlowToEspr%Description = ''
      Call AddVariable(rvpltACCFlowToEspr)

      rvpltACCMoistFlowToEspr%VariableName = 'plant/co-sim/ACC_*/ACC_MoistFlow_To_Espr'
      rvpltACCMoistFlowToEspr%MetaType = 'units'
      rvpltACCMoistFlowToEspr%VariableType = 'kg/s'
      rvpltACCMoistFlowToEspr%Description = ''
      Call AddVariable(rvpltACCMoistFlowToEspr)

      rvpltCosimAirPointTemperatures%VariableName = 'plant/co-sim/zone_*/air_point_temperature'
      rvpltCosimAirPointTemperatures%MetaType = 'units'
      rvpltCosimAirPointTemperatures%VariableType = '(oC)'
      rvpltCosimAirPointTemperatures%Description = ''
      Call AddVariable(rvpltCosimAirPointTemperatures)

      rvpltCosimAirPointHumidities%VariableName = 'plant/co-sim/zone_*/air_point_relative_humidity'
      rvpltCosimAirPointHumidities%MetaType = 'units'
      rvpltCosimAirPointHumidities%VariableType = '(%)'
      rvpltCosimAirPointHumidities%Description = ''
      Call AddVariable(rvpltCosimAirPointHumidities)

      rvpltCosimAirPointCasualGains%VariableName = 'plant/co-sim/zone_*/casual_gains'
      rvpltCosimAirPointCasualGains%MetaType = 'units'
      rvpltCosimAirPointCasualGains%VariableType = 'W'
      rvpltCosimAirPointCasualGains%Description = ''
      Call AddVariable(rvpltCosimAirPointCasualGains)

      !Used by complex_fenestration.F
      rvCFCShadeCtl%VariableName = 'building/zone_*/cfc_*/cfc_shade_ctl'
      rvCFCShadeCtl%MetaType = 'units'
      rvCFCShadeCtl%VariableType = ''
      rvCFCShadeCtl%Description = 'state of shade control'
      Call AddVariable(rvCFCShadeCtl)

      rvCFCSlatAngle%VariableName = 'building/zone_*/cfc_*/cfc_shade_angle'
      rvCFCSlatAngle%MetaType = 'units'
      rvCFCSlatAngle%VariableType = 'degrees'
      rvCFCSlatAngle%Description = 'angle of cfc controlled shade'
      Call AddVariable(rvCFCSlatAngle)
      
      End Subroutine UpdateH3kReport


   ! ********************************************************************
   ! Subroutine: UpdateH3kSimInfo
   ! Purpose:  Method that send the simulation information to the C++
   !           routine.
   ! Params:   isd1 - simulation start day
   !           ism1 - simulation start month
   !           isd2 - simulation end day
   !           ism2 - simulation end month
   !           isds - simulation start day in relation to 365 day/year
   !           isdf - simulation end day in relation to 365 day/year
   !           ntstep - simulation step per hour
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-07-20
   ! ********************************************************************
   Subroutine UpdateH3kSimInfo(isd1,ism1, isd2, ism2,isds,isdf,ntstep)
      integer,intent(in)::isd1,ism1,isd2,ism2,isds,isdf,ntstep

      if (isH3KEnabled()) then
         !call c++
         call set_report_simulation_info(isds,isdf,ntstep)
      endif
   End Subroutine UpdateH3kSimInfo



   ! ********************************************************************
   ! Subroutine: AddVariable
   ! Scope:    Private
   ! Purpose:  private method that sends the ReportVariable information
   !           to the C++ routine.
   !           ** C++ will append a \0 char to the string parameters
   !           ** C++ will assign the identifier numbers
   !           ** C++ will set the Enabled boolean (based on input.xml)
   ! Params:   Type ReportVariable
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-06-27
   ! ********************************************************************
   subroutine AddVariable(rvReportVariable)
      Type(ReportVariable),intent(in) :: rvReportVariable

      !call to the C++ function
      !The true string length is passed along, c++ will change
      !the string parameters and append a NUL character
      !the boolean is set based on the input.xml file
      call set_report_variable(rvReportVariable%Identifier, &
                     rvReportVariable%Enabled, &
                     len_trim(rvReportVariable%VariableName), &
                     len_trim(rvReportVariable%MetaType), &
                     len_trim(rvReportVariable%VariableType), &
                     len_trim(rvReportVariable%Description), &
                     rvReportVariable%VariableName, &
                     rvReportVariable%MetaType, &
                     rvReportVariable%VariableType, &
                     rvReportVariable%Description)
   End Subroutine AddVariable

   ! ********************************************************************
   ! Subroutine: AddToReportWild
   ! Scope:    Private, accessible only through AddToReport interface
   ! Purpose:  Wrapper to the C++ call add_to_report(int,float)
   ! Params:   integer, real
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-07-04
   ! ********************************************************************
   subroutine AddToReportWild(iIdentifier, rValue)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report(iIdentifier,rValue)
      endif
   End Subroutine AddToReportWild

   ! ********************************************************************
   ! Subroutine: AddToReportWild1
   ! Scope:    Private, accessible only through AddToReport interface
   ! Purpose:  Wrapper to the C++ call add_to_report_wild1(int,float,char*)
   !           Use this subroutine to send information to the xml and
   !           replace one '*' character in the Variable name with the
   !           passed in value.
   ! Params:   integer, real, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-07-04
   ! ********************************************************************
   subroutine AddToReportWild1(iIdentifier, rValue, cWild1)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      character(len=*), intent(in) :: cWild1

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_wild1(iIdentifier, rValue, cWild1)
      endif
   End Subroutine AddToReportWild1

   ! ********************************************************************
   ! Subroutine: AddToReportWild2
   ! Scope:    Private, accessible only through AddToReport interface
   ! Purpose:  Wrapper to the C++ call add_to_report_wild1(int,float,char*,char*)
   !           Use this subroutine to send information to the xml and
   !           replace two '*' character in the Variable name with the
   !           passed in value.
   ! Params:   integer, real, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-07-04
   ! ********************************************************************
   subroutine AddToReportWild2(iIdentifier, rValue, cWild1, cWild2)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      character(len=*), intent(in) :: cWild1, cWild2

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_wild2(iIdentifier, rValue, cWild1, cWild2)
      endif
   End Subroutine AddToReportWild2

   ! ********************************************************************
   ! Subroutine: AddToReportWild3
   ! Scope:    Private, accessible only through AddToReport interface
   ! Purpose:  Wrapper to the C++ call add_to_report_wild1(int,float,char*,char*,char*)
   !           Use this subroutine to send information to the xml and
   !           replace three '*' character in the Variable name with the
   !           passed in value.
   ! Params:   integer, real, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-07-04
   ! ********************************************************************
   subroutine AddToReportWild3(iIdentifier, rValue, cWild1, cWild2, cWild3)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      character(len=*), intent(in) :: cWild1, cWild2, cWild3

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_wild3(iIdentifier, rValue, cWild1,cWild2,cWild3)
      endif
   End Subroutine AddToReportWild3


   ! ********************************************************************
   ! Subroutine: AddToReportDetailsWild
   ! Scope:    Private, accessible only through AddToReportDetails interface
   ! Purpose:  Wrapper to the C++ call add_to_report_details
   !           Use this subroutine to send dynamic report description.
   !           When possible you should avoid the use of these
   !           familly of routines (performance reason).
   ! Params:   integer, char*, char*, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-08-30
   ! ********************************************************************
   subroutine AddToReportDetailsWild(iIdentifier, cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_details(iIdentifier,cUnit,cType,cDescription)
      endif
   End Subroutine


   ! ********************************************************************
   ! Subroutine: AddToReportDetailsWild1
   ! Scope:    Private, accessible only through AddToReportDetails interface
   ! Purpose:  Wrapper to the C++ call add_to_report_details
   !           Use this subroutine to send dynamic report description.
   !           When possible you should avoid the use of these
   !           familly of routines (performance reason).
   ! Params:   integer, char*, char*, char*, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-08-30
   ! ********************************************************************
   subroutine AddToReportDetailsWild1(iIdentifier, cWild1, cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription, cWild1

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_details_wild1(iIdentifier,cWild1,cUnit,cType,cDescription)
      endif
   End Subroutine


   ! ********************************************************************
   ! Subroutine: AddToReportDetailsWild2
   ! Scope:    Private, accessible only through AddToReportDetails interface
   ! Purpose:  Wrapper to the C++ call add_to_report_details
   !           Use this subroutine to send dynamic report description.
   !           When possible you should avoid the use of these
   !           familly of routines (performance reason).
   ! Params:   integer, char*, char*, char*, char*, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-08-30
   ! ********************************************************************
   subroutine AddToReportDetailsWild2(iIdentifier, cWild1, cWild2, cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription, cWild1, cWild2

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_details_wild2(iIdentifier,cWild1,cWild2,cUnit,cType,cDescription)
      endif
   End Subroutine


   ! ********************************************************************
   ! Subroutine: AddToReportDetailsWild3
   ! Scope:    Private, accessible only through AddToReportDetails interface
   ! Purpose:  Wrapper to the C++ call add_to_report_details
   !           Use this subroutine to send dynamic report description.
   !           When possible you should avoid the use of these
   !           familly of routines (performance reason).
   ! Params:   integer, char*, char*, char*, char*, char*, char*
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-08-30
   ! ********************************************************************
   subroutine AddToReportDetailsWild3(iIdentifier, cWild1, cWild2, cWild3, cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription, cWild1, cWild2, cWild3

      !Call c++
      if (isH3KEnabled()) then
         call add_to_report_details_wild3(iIdentifier,cWild1,cWild2,cWild3,cUnit,cType,cDescription)
      endif
   End Subroutine


   ! ********************************************************************
   ! Subroutine: ReportNextTimeStep
   ! Purpose:  Wrapper to the C++ call report_next_time_step, this routine
   !           indicate what time step the add_to_report* data is attached
   !           too as well as pass in the step relevant data
   ! Params:   iStep    - current step number
   !           iHour    - the step's hour
   !           iDay     - the step's day
   !           iStartup - 1/0 if the step is in startup mode or not
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2012-02-21 - change method to access int for startup Qrun
   ! Mod Data: 2013-03-16 - added iQuick_run to function parameters
   ! ********************************************************************
   Subroutine ReportNextTimeStep(iStep,rTime,iDay,iStartup, iQrun)
      integer, intent(in) :: iStep
      real, intent(in) :: rTime
      integer, intent(in) :: iDay
      integer, intent(in) :: iStartup
      integer, intent(in) :: iQrun
      logical :: bStartup


      !Call c++
      if (isH3KEnabled()) then
         if(iStartup == 1) then
            bStartup = .true.
         else
            bStartup = .false.
         end if

         call report_next_time_step(iStep,int(rTime),iDay,bStartup, iQrun)
      endif
   End Subroutine


   ! ********************************************************************
   ! Subroutine: ReportNextSeason
   ! Purpose:  Wrapper to the C++ call report_new_season, this routine,
   !           used with QuickRun mode, indicates that a new season has
   !           begun.
   ! Params:   iSeason        - Seasons Index
   !           fHtgMultiplier - the HTG multiplier to use for that season
   !           fClgMultiplier - the CLG multiplier to use for that season
   !           fGenMultiplier - the GEN multiplier to use for that season
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2012-02-03
   ! ********************************************************************
   Subroutine ReportNewSeason(iSeason, fHtgMultiplier, fClgMultiplier,fGenMultiplier)
      integer, intent(in) :: iSeason
      real, intent(in) :: fHtgMultiplier,fClgMultiplier,fGenMultiplier

      !Call c++
      if (isH3KEnabled()) then
         call report_new_season(iSeason, fHtgMultiplier, fClgMultiplier,fGenMultiplier)
      endif
   End Subroutine

   ! ********************************************************************
   ! Subroutine: GenerateOutput
   ! Purpose:  Wrapper to the C++ call generate_output()
   !           This method signals the end of a simulation, the c++
   !           code will then generate the requested output specified in
   !           the input.xml file.
   ! Params:   none
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-07-04
   ! ********************************************************************
   Subroutine GenerateOutput()
      if (isH3KEnabled()) then
         !Call c++
         call generate_output()
      endif

      !Terminate the report call the c++
      call rep_cleanup_files()
   End Subroutine


   ! ********************************************************************
   ! Subroutine: SetReportParameter
   ! Purpose:  Wrapper to the C++ call rep_set_parameter
   ! Params:   cName - name of the parameter
   !           cValue - value of the parameter
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-09
   ! ********************************************************************
   Subroutine SetReportParameter(cName,cValue)
      character(len=*), intent(in) :: cName, cValue

      !Call the c++
      if (isH3KEnabled()) then
         call rep_set_parameter(cName,cValue)
      endif
   End Subroutine


   ! ********************************************************************
   ! Subroutine: SetReportEnable
   ! Purpose:  Enable or disable the report generation
   ! Params:   bStatus - true/false if enable or not
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-09
   ! ********************************************************************
   Subroutine SetReportEnable(bStatus)
      logical, intent(in) :: bStatus

      !Call c++
      Call h3k_enable_reports(bStatus)
   End Subroutine


   ! ********************************************************************
   ! Function: isH3KEnabled
   ! Purpose:  Return true/false if the reporting is enabled
   ! Params:   N/A
   ! Returns:  true/false
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-12
   ! ********************************************************************
   logical Function isH3KEnabled()
      logical::lRtn = .false.

      !Call the c++ routine
      lRtn  = bH3K_rep_enabled()

      isH3KEnabled = lRtn
   End Function isH3KEnabled


   ! ********************************************************************
   ! Function: isReportingInstalled
   ! Purpose:  Return true/false if the reporting module is installed
   ! Params:   N/A
   ! Returns:  true/false
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-15
   ! ********************************************************************
   logical Function isReportingInstalled()
      logical::lRtn = .false.

      lRtn = rep_xmlstatus()

      isReportingInstalled = lRtn
   End Function isReportingInstalled



   ! ********************************************************************
   ! Function: GetReportConfig
   ! Purpose:  Wrapper to the c++ call rep_report_config, wich returns
   !           the value of a report tag for in the input.xml
   ! Params:   cDescription -
   ! Returns:  cReturn - value in the configuration
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-13
   ! ********************************************************************
   Subroutine GetReportConfig(cDescription, cReturn)
      character(len=*), intent(in)::cDescription
      character(len=50), intent(out) :: cReturn

      !call the c++
      if (isH3KEnabled()) then
          cReturn = rep_report_config(cDescription)
      endif
   End Subroutine


   ! ********************************************************************
   ! Function: GetReportList
   ! Purpose:  Wrapper to the c++ call rep_report_list, wich returns
   !           true/false if a type/value combination
   ! Params:   cType -
   !           cValue -
   ! Returns:  cReturn - true/false if item found in list
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-14
   ! ********************************************************************
   logical Function GetReportList(cType, cValue)
      character(len=*), intent(in)::cType, cValue
      logical::lRtn = .false.

      !call the c++
      if (isH3KEnabled()) then
         lRtn = rep_report_list(cType,cValue)
      endif

      GetReportList = lRtn
   End Function GetReportList



   ! ********************************************************************
   ! Function: SetReportConfig
   ! Purpose:  Wrapper to the c++ call rep_update_config, wich sets
   !           the value of a report tag for in the input.xml
   ! Params:   cVariable - the variable name
   !           cValue - the desired value
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-13
   ! ********************************************************************
   Subroutine SetReportConfig(cVariable,cValue)
      character(len=*), intent(in)::cVariable,cValue

      !call the c++
      call rep_Update_Config(cVariable,cValue)
   End Subroutine


   ! ********************************************************************
   ! Function: UpdateConfigFile
   ! Purpose:  Wrapper to the c++ call rep_update_config_file, wich
   !           write the configured changes to the input.xml file
   !           will also create a new input.xml file if it doesn't exists
   ! Params:   N/A
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-13
   ! ********************************************************************
   Subroutine UpdateConfigFile()
      !call the c++
      call rep_update_config_file()
   End Subroutine


   ! ********************************************************************
   ! Function: ReportToggleConfig
   ! Purpose:  Wrapper to the c++ call rep_toggle_config, method that
   !           toggles report configuration,
   ! Params:   cVariable - xml variable tag name to toggle
   ! Returns:  true/false, false on error or if the cVariable was not found
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-09-14
   ! ********************************************************************
   logical Function ReportToggleConfig(cVariable)
      character(len=*), intent(in)::cVariable
      logical::lRtn = .false.

      !call the c++
      if (isH3KEnabled()) Then
         lRtn = rep_toggle_config(cVariable)
      endif

      ReportToggleConfig = lRtn
   End Function ReportToggleConfig

   ! ********************************************************************
   ! Function: SetAdditionalValues
   ! Purpose:  Wrapper to the c++ call set_additional_flag, method that
   !           sets extra flags for identifying variables
   ! Params:   iIdentifier  - ReportVariable Identifier
   !           iPropertyNum - Hardcoded number identifying a property
   !                          same numbers will be found if c++
   !           bValue       - the value to pass
   ! Returns:  N/A
   ! Author:   Claude Lamarche
   ! Mod Date: 2012-02-03
   ! Note:     For now these are hardcoded constants numbers found in both
   !           Fortran at C++ but if needs be in the future these constants
   !           could be shared (only one instance created) and method could
   !           be overloaded to send different value types (similar to
   !           AddToReport).
   !
   ! ** Property Table **
   !  ___________________
   ! | 1   |   scale_htg |
   ! | 2   |   scale_clg |
   ! | 3   |   scale_gen |
   !  -------------------
   ! ********************************************************************
   Subroutine SetAdditionalValues(iIdentifier, iPropertyNum, bValue)
      integer,intent(in) :: iIdentifier, iPropertyNum
      logical, intent(in) :: bValue

      !call the c+
      if (isH3KEnabled()) then
         call set_var_additional_info(iIdentifier,iPropertyNum,bValue)
      endif
   End Subroutine

   ! ********************************************************************
   ! ***** NEEDS MORE TESTING METHOD DISABLE AT THE MOMENT ******
   ! *******************************
   ! Function: IsH3kVarEnabled
   ! Purpose:  Wrapper to the c++ call is_variable_enabled, method that
   !           perform a pattern lookup to see if a variable is enabled.
   !           this can be used to optimize reports by avoiding sections of
   !           code if the no report variables will be part of the simulation
   !           results.  The lookup does not perform a regular expression
   !           check, it only verifies if the cPattern is part of an
   !           enabled variable VariableName defined above.
   ! Params:   cPattern, string pattern ex: "/zone_*/surface_*/"
   ! Returns:  true/false is enabled or not
   ! Author:   Claude Lamarche
   ! Mod Date: 2011-10-04
   ! ********************************************************************
   logical Function IsH3kVarEnabled(cPattern)
      character(len=*), intent(in)::cPattern
      logical::lRtn = .false.

      !call the c++
      !if (isH3kEnabled()) Then
      !   lRtn = is_variable_enabled(cPattern)
      !endif

      !IsH3kVarEnabled = lRtn

      !Disable for now
      IsH3kVarEnabled = .true.
   End Function IsH3kVarEnabled
END MODULE



