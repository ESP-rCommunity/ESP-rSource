
C------------------------------------------------------------------------------------
C This file contains common blocks and variable declarations related to the
C plant component model of an adsorption storage unit.
C------------------------------------------------------------------------------------


C------------------------------------------------------------------------------------
C COMMON holding input data for adsorption storage unit component model.
C------------------------------------------------------------------------------------

       COMMON/ADS_storage_1/
     &                   fMassAdsorbent,
     &                   fCpAdsorbent,
     &                   fCoeff_Wo,
     &                   fCoeff_D,
     &                   fCoeff_n,
     &                   fMassAdsVessel,
     &                   fCpAdsVessel,
     &                   fMassAdsHeatX,
     &                   fCpAdsHeatX,   
     &                   fAdsEnthalpy,
     &                   fDT_Derivative,
     &                   fSurfAreaAdsorber,
     &                   fAdsInsulL,
     &                   fAdsInsulK,
     &                   fAdsOverallHAmb,
     &                   fAdsFCHeatXeff,
     &                   fPressAdsFC,
     &                   fAdsTankHeatXeff,
     &                   fPressAdsTank,
     &                   fMinTempDiff,
     &                   fChargeMinTemp,
     &                   fCondPressure,
     &                   fCondEffec,
     &                   fMassFlowAirCond,
     &                   fSurfAreaCond,     
     &                   fCondOverallHamb,
     &                   fCondFanPower,
     &                   fEvapPressure,
     &                   fEvapEffec,
     &                   fSurfAreaEvap,
     &                   fEvapOverallHamb


C------------------------------------------------------------------------------------
C Adsorber characteristics
C------------------------------------------------------------------------------------

       REAL   fMassAdsorbent      !-mass of adsorbent contained in the adsorber (kg).
       REAL   fCpAdsorbent        !-specific heat of adsorbent contained in adsorber
                                  !- (J/kgK).
       REAL   fCoeff_Wo           !-coefficient Wo of Dubinin-Astakhov (D-A) adsorption
                                  !-equilibrium equation. Wo represents the volume of
                                  !-the adsorption space, a charateristic of the
                                  !-adsorbent (m3/kg adsorbent).
       REAL   fCoeff_D            !-coefficient D of D-A adsorption equilibrium
                                  !-equation. D is a  characteristic of the
                                  !-adsorbent/adsorbate pair (-).
       REAL   fCoeff_n            !-coefficient n of D-A adsorption equilibrium
                                  ! equation. n is a  characteristic of the
                                  !-adsorbent/adsorbate pair (-).
       REAL   fMassAdsVessel      !-mass of the adsorber vessel (kg).
       REAL   fCpAdsVessel        !-specific heat of adsorber vessel material (J/kgK).
       REAL   fMassAdsHeatX       !-mass of heat exchanger within the adsorber (kg).
       REAL   fCpAdsHeatX         !-specific heat of h/x  material within the adsorber
                                  !- (J/kgK).
       REAL   fAdsEnthalpy        !-enthalpy of adsorption, assumed constant (J/kg).
       REAL   fDT_Derivative      !-delta T used to obtain dX/dT derivative (oC).
       REAL   fSurfAreaAdsorber   !-surface area of adsorber (m2).
       REAL   fAdsInsulL          !-thickness of adsorber insulation layer (m).
       REAL   fAdsInsulK          !-thermal conductivity of adsorber insulation (W/mK).
       REAL   fAdsOverallHAmb     !-overall heat transfer coefficient adsorber-
                                  !-surroundings (W/m2K).
       REAL   fAdsFCHeatXeff      !-effectiveness of heat exchanger adsorber-fuel cell (-).
       REAL   fPressAdsFC         !-pressure of adsorber-fuel cell heat exchange loop (kPa).
       REAL   fAdsTankHeatXeff    !-effectiveness of heat exchanger adsorber-main water
                                  !- storage tank (-).
       REAL   fPressAdsTank       !-pressure of adsorber-main water storage tank heat
                                  !- exchange loop (kPa).
       REAL   fMinTempDiff        !-minimum temperature difference between adsorber and
                                  !-the heat source or heat sink (oC). This variable is used to
                                  !-determine end of charging/discharge phases, respectively.
       REAL   fChargeMinTemp      !-minimum temperature of heat source required to
                                  !-charge the adsorber (oC)

C------------------------------------------------------------------------------------
C Condenser characteristics
C------------------------------------------------------------------------------------

       REAL   fCondPressure    !-condenser pressure (kPa).
       REAL   fCondEffec       !-condenser heat exchanger effectiveness (-).
       REAL   fMassFlowAirCond !-mass flow rate of air entering condenser (kg/s).
       REAL   fSurfAreaCond    !-surface area of condenser (m2). 
       REAL   fCondOverallHamb !-overall heat transfer coefficient condenser-surroundings (W/m2K).
       REAL   fCondFanPower    !-condenser fan power rating (W).


C------------------------------------------------------------------------------------
C Evaporator characteristics
C------------------------------------------------------------------------------------

       REAL   fEvapPressure    !-evaporator pressure (kPa).
       REAL   fEvapEffec       !-evaporator heat exchanger effectiveness (-).
       REAL   fSurfAreaEvap    !-surface area of evaporator (m2).
       REAL   fEvapOverallHamb !-overall heat transfer coefficient evaporator-surroundings (W/m2K).




C------------------------------------------------------------------------------------
C COMMON holding calculated data for adsorption storage unit component model.
C------------------------------------------------------------------------------------


       COMMON/ADS_storage_2/fMassCp_Adsorbent,fMassCp_AdsVessel,
     &                      fMassCp_AdsHeatX,
     &                      fRtotal_Adsorber


       REAL   fMassCp_Adsorbent  !-product of mass and specific heat of adsorbent (J/K).
       REAL   fMassCp_AdsVessel  !-product of mass and specific heat of adsorber vessel (J/K).
       REAL   fMassCp_AdsHeatX   !-product of mass and specific heat of h/x within 
                                 !-adsorber (J/K)
       REAL   fRtotal_Adsorber   !-Total resistance adsorber-surroundings (m2oC/W).



C------------------------------------------------------------------------------------
C COMMON holding initial and previous time-step values of temperature and pressure.
C------------------------------------------------------------------------------------


       COMMON/ADS_storage_3/fTempAdsorber_Init, fPressAdsorber_Init,
     &                     fPressAdsorber_Present, fX_Initial,
     &                     fX_Present, fX_Past, fMdotVap,
     &                     bInit


       REAL    fTempAdsorber_Init  !-initial temperature of adsorber (node 1)
       REAL    fPressAdsorber_Init !-initial pressure of adsorber
       REAL    fPressAdsorber_Present !-present value of pressure of adsorber
       REAL    fX_Initial    !-initial value of adsorbate to adsorbent mass
                             !-based on initial temperature and pressure
       REAL    fX_Present    !-present value of adsorbate to adsorbent mass
       REAL    fX_Past       !-past value of adsorbate to adsorbent mass
                             !-this is used to calculate dX/dt at current timestep
       REAL    fMdotVap      !-mass flow rate of vapour desorbed or adsorbed from
                             !-adsorbent during the charging and discharge phases,
                             !-respectively.
       LOGICAL bInit         !-boolean to flag if adsorption storage unit is
                             !-initialized.
