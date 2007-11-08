C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004-2007. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file contains the following routines related to the fuel cell
C component model.

C   SOFC_A42_static       Static template executed prior to time-step simulation.
C   SOFC_A42_coeffgen     Coefficient generator executed each time-step (more
C                         if iteration) of the simulation.
C   Shomate               Function used to calculate gas properties.
C *******************************************************************************
C References:
C
C    Beausoleil-Morrison I. and Kelly N., Editors (2007), Specification for Modelling
C    Fuel Cell and Combustion-Based Residential Cogeneration Device within Whole-Building
C    Simulation Programs, Section II, IEA/ECBCS Annex 42 Report.
C *********************************************************************************************

C *********************************************************************************************
C ********************************* SOFC_A42_static *******************************************

C This subroutine is the static template for the Annex 42 FC-cogen component model.
C It checks whether the component is properly used in the user-defined plant network,
C it assigns user-input data to module variables, it performs time-invariant
C calculations, and it checks the integrity of the user-specified electric
C loads data file.

C The FC-cogen device is represented with 14 control volumes (aka nodes):
C   CV1 - The air within the air supply blower.
C   CV2 - The gas within the fuel compressor.
C   CV3 - The water within the water pump.
C   CV4 - The gas within the fuel cell power module (FCPM).
C   CV5 - The gas within the auxiliary burner.
C   CV6 - The water within and the solid portion of the heat recovery device which
C         transfers energy from the hot exhaust gases to the heat recovery loop. This
C         water then flows to connected plant components.
C   CV7 - The gas within the heat recovery device.
C   CV8 - The gas within the dilution air mixing system.
C   CV9 - The gas within the HRV (hot side of HRV).
C   CV10 - The water within the stack cooling loop, at the inlet to the stack
C         heat exchanger.
C   CV11- The water within the stack cooling loop, at the outlet from the stack
C         heat exchanger.
C   CV12- The water within the stack cooling loop, at the outlet from the external
C         heat exchanger which transfers energy to heat recovery loop.
C   CV13- The water within the stack cooling loop, at the outlet from the air cooler.
C   CV14- The water within the heat recovery loop, at the outlet from the external
C         heat exchanger which transfers energy from the stack cooling loop. This water
C         then flows to connected plant components.
C--------------------------------------------------------------------------------------------

      SUBROUTINE SOFC_A42_static(IPCOMP)
      IMPLICIT NONE
#include "Annex42_fuel_cell.h"
#include "plant.h"
#include "building.h"

C--------------------------------------------------------------------------------------------
C Common blocks.
C--------------------------------------------------------------------------------------------
C Unit numbers for providing feedback to user during simulation.
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Trace output.
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C Number of components in plant network and control actuation signals.
      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

C Plant component data supplied by user (via plant network file).
      COMMON/PDBDT/ADATA,BDATA
      REAL ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)

C Connecting node data.
      COMMON/PCOND/CONVAR,ICONTP,ICONDX
      REAL CONVAR(MPCON,MCONVR)
      INTEGER ICONTP(MPCON),ICONDX(MPCOM,MNODEC,MPCONC)

C Flag to enable inter-domain iteration 
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C FCPM electrical performance.
      COMMON/FCPM_elec/FCPM_eta_0, FCPM_eta_1, FCPM_eta_2,
     &                 FCPM_degrade_ONOFF, FCPM_degrade_optime,
     &                 FCPM_degrade_thresh,
     &                 FCPM_elec_min,FCPM_elec_max
      REAL FCPM_eta_0          ! Polynomial coeff that expresses FCPM elec efficiency.
      REAL FCPM_eta_1          ! Ditto.
      REAL FCPM_eta_2          ! Ditto.
      REAL FCPM_degrade_ONOFF  ! Fractional performance degradation for off-on cycling.
      REAL FCPM_degrade_optime ! Fractional performance degradation due to operating time.
      REAL FCPM_degrade_thresh ! Time threshold (hrs) before which no degradation due to
                               ! operating time occurs.
      REAL FCPM_elec_min       ! FCPM's min elec output (W).
      REAL FCPM_elec_max       ! FCPM's max elec output (W).

C FCPM transient response.
      COMMON/FCPM_transient/FCPM_dPdt_inc_limit, FCPM_dPdt_dec_limit,
     &                      FCPM_dt_startup, FCPM_fuel_cons_startup,
     &                      FCPM_elec_cons_startup,
     &                      FCPM_elec_prod_startup, FCPM_dt_cooldown,
     &                      FCPM_elec_cons_cooldown,
     &                      FCPM_fuel_cons_cooldown
      REAL FCPM_dPdt_inc_limit     ! Max allowable time derivative for elec output in W/s (power increasing)
      REAL FCPM_dPdt_dec_limit     ! Max allowable time derivative for elec output in W/s (power increasing)
      REAL FCPM_dt_startup         ! Duration of start-up period (seconds).
      REAL FCPM_fuel_cons_startup  ! Fuel consumption during start-up period (kmol).
      REAL FCPM_elec_cons_startup  ! Electrical consumption during start-up period (MJ).
      REAL FCPM_elec_prod_startup  ! Net DC electrical production during start-up period (MJ).
      REAL FCPM_dt_cooldown        ! Duration of cool-down period (seconds).
      REAL FCPM_fuel_cons_cooldown ! Fuel consumption during cool-down period (kmol).
      REAL FCPM_elec_cons_cooldown ! Electrical consumption during cool-down period (MJ).

C Fuel constituents.
      COMMON/SOFC_fuel_constituents/
     &                 chi_fuel_H2, chi_fuel_CH4, chi_fuel_C2H6,
     &                 chi_fuel_C3H8, chi_fuel_C4H10, chi_fuel_C5H12,
     &                 chi_fuel_C6H14, chi_fuel_CH3OH, chi_fuel_C2H5OH,
     &                 chi_fuel_CO2, chi_fuel_N2, chi_fuel_O2
      REAL chi_fuel_H2     ! Molar fraction of hydrogen.
      REAL chi_fuel_CH4    ! Molar fraction of methans.
      REAL chi_fuel_C2H6   ! Molar fraction of ethane.
      REAL chi_fuel_C3H8   ! Molar fraction of propane.
      REAL chi_fuel_C4H10  ! Molar fraction of butane.
      REAL chi_fuel_C5H12  ! Molar fraction of pentane.
      REAL chi_fuel_C6H14  ! Molar fraction of hexane.
      REAL chi_fuel_CH3OH  ! Molar fraction of methanol.
      REAL chi_fuel_C2H5OH ! Molar fraction of ethanol.
      REAL chi_fuel_CO2    ! Molar fraction of carbon dioxide in fuel (inert).
      REAL chi_fuel_N2     ! Molar fraction of nitrogen in fuel (inert).
      REAL chi_fuel_O2     ! Molar fraction of oxygen in fuel (inert).

C Fuel mixture properties.
      COMMON/SOFC_fuel_mixture/LHV_fuel
      REAL LHV_fuel       ! LHV of fuel supplied to FCPM and auxiliary burner (J/kmol).

C Air constituents.
      COMMON/SOFC_air/chi_air_N2, chi_air_O2, chi_air_H2O,
     &                chi_air_Ar, chi_air_CO2
      REAL chi_air_N2      ! Molar fraction of nitrogen.
      REAL chi_air_O2      ! Molar fraction of oxygen.
      REAL chi_air_H2O     ! Molar fraction of water vapour.
      REAL chi_air_Ar      ! Molar fraction of argon.
      REAL chi_air_CO2     ! Molar fraction of carbon dioxide.


C Air supply.
      COMMON/SOFC_air_supply/FCPM_air_supply_method,
     &                       FCPM_air_0, FCPM_air_1, FCPM_air_2,
     &                       FCPM_air_3
      INTEGER FCPM_air_supply_method  ! Method used to establish air supply to FCPM.
      REAL FCPM_air_0                 ! Excess air ratio or coeff to polynomial (depends on method)
      REAL FCPM_air_1                 ! Coeff to polynomial determining air supply.
      REAL FCPM_air_2                 ! Ditto.
      REAL FCPM_air_3                 ! Ditto.

C Water supply.
      COMMON/SOFC_water_supply/FCPM_water_0, FCPM_water_1, FCPM_water_2
      REAL FCPM_water_0    ! Coeff to polynomial determing water supply.
      REAL FCPM_water_1    ! Ditto.
      REAL FCPM_water_2    ! Ditto.

C AC power supply to FCPM for ancillaries.
      COMMON/SOFC_ancillaries/FCPM_anc_0, FCPM_anc_1
      REAL FCPM_anc_0      ! Coeff to polynomial determining FCPM AC ancillary power draw.
      REAL FCPM_anc_1      ! Ditto.

C FCPM skin losses.
      COMMON/SOFC_skin_losses/FCPM_skin_loss_method,FCPM_skin_loss_conv,
     &                        FCPM_skin_0, FCPM_skin_1, FCPM_skin_2
      INTEGER FCPM_skin_loss_method  ! Method used to determine skin losses from FCPM.
      REAL FCPM_skin_loss_conv       ! Frac of heat loss that is convective.
      REAL FCPM_skin_0               ! Skin loss (W), UA-value (W/K), or coeff (depends on method).
      REAL FCPM_skin_1               ! Coeff to polynomial for `fuel flow' method.
      REAL FCPM_skin_2               ! Ditto.

C Air supply blower.
      COMMON/SOFC_blower/Blower_source_air, blower_0, blower_1,
     &                   blower_2, blower_3, Blower_heat_loss
      INTEGER Blower_source_air  ! Indicates source from which air is drawn.
      REAL blower_0              ! Coeff to polynomial that establishes blower power draw.
      REAL blower_1              ! Ditto.
      REAL blower_2              ! Ditto.
      REAL blower_3              ! Ditto.
      REAL Blower_heat_loss      ! Ratio of heat loss from blower to elec power supply.

C Fuel supply compressor.
      COMMON/SOFC_compressor/Compressor_source_fuel, compressor_0,
     &                       compressor_1, compressor_2, compressor_3,
     &                       Compressor_heat_loss
      INTEGER Compressor_source_fuel  ! Indicates temp of fuel entering comprssor.
      REAL compressor_0               ! Coeff to polynomial that establishes compressor power draw.
      REAL compressor_1               ! Ditto.
      REAL compressor_2               ! Ditto.
      REAL compressor_3               ! Ditto.
      REAL Compressor_heat_loss       ! Ratio of heat loss from compressor to elec power supply.

C Water pump.
      COMMON/SOFC_pump/Pump_source_water,pump_0, pump_1,
     &                 pump_2, pump_3, Pump_heat_loss

      INTEGER Pump_source_water  ! Indicates temp of water entering pump.
      REAL pump_0                ! Coeff to polynomial that establishes pump power draw.
      REAL pump_1                ! Ditto.
      REAL pump_2                ! Ditto.
      REAL pump_3                ! Ditto.
      REAL Pump_heat_loss        ! Ratio of heat loss from pump to elec power supply.

C Auxiliary burner.
      COMMON/SOFC_auxburner/Auxburn_present, Auxburn_W_or_kmols,
     &                      Auxburn_min_output, Auxburn_max_output,
     &                      Auxburn_heat_to_where,
     &                      Auxburn_UA, Auxburn_anc_0, Auxburn_anc_1,
     &                      Auxburn_lambda

      INTEGER Auxburn_present       ! Indicates whether there is an auxiliary burner.
      INTEGER Auxburn_W_or_kmols    ! Indicates whether burner cap spec in heat output or fuel input.
      REAL Auxburn_min_output       ! Minimum operating point for burner (W or kmol/s).
      REAL Auxburn_max_output       ! Maximum operating point for burner (W or kmol/s).
      INTEGER Auxburn_heat_to_where ! Indicates where the heat loss from the burner goes.
      REAL Auxburn_UA               ! Heat loss coefficient for burner (W/K).
      REAL Auxburn_anc_0            ! Coeff to polynomial that determines burner ancillary power draw.
      REAL Auxburn_anc_1            ! Ditto.
      REAL Auxburn_lambda           ! Auxiliary burner excess air ratio (-).

C Gas-to-water heat exchanger.
      COMMON/SOFC_HX/HX_method, HX_eff,
     &               HX_sen_0, HX_sen_1, HX_sen_2, HX_sen_3, HX_sen_4,
     &               HX_h0_gas, HX_Ndot0_gas, HX_n, HX_Area_gas,
     &               HX_h0_water, HX_Ndot0_water, HX_m, HX_Area_water,
     &               HX_F, HX_lat_1, HX_lat_2, HX_T_cond_thresh

      INTEGER HX_method             ! Indicates which method is used to calculate heat exchange.
      REAL HX_eff                   ! Fixed effectiveness (used with `HX_fixed_eff' method.
      REAL HX_sen_0                 ! Coeffs to polynomial to calc UA (used with `HX_lmtd_empirical'
      REAL HX_sen_1                 !   and `HX_condensing' methods.
      REAL HX_sen_2                 ! Ditto.
      REAL HX_sen_3                 ! Ditto.
      REAL HX_sen_4                 ! Ditto.
      REAL HX_h0_gas                ! HX coeff to gas at nominal gas flow (used with `HX_lmtd_deterministic' method).
      REAL HX_Ndot0_gas             ! Nominal gas flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_n                     ! Exponent to gas flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_Area_gas              ! Reference heat exchange area to gas (used with `HX_lmtd_deterministic' method).
      REAL HX_h0_water              ! HX coeff to water at nominal water flow (used with `HX_lmtd_deterministic' method).
      REAL HX_Ndot0_water           ! Nominal water flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_m                     ! Exponent to water flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_Area_water            ! Reference heat exchange area to water (used with `HX_lmtd_deterministic' method).
      REAL HX_F                     ! Adjustment factor (used with `HX_lmtd_deterministic' method).
      REAL HX_lat_1                 ! Coeffs to polynomial to calc rate of condensation (used with
      REAL HX_lat_2                 !   `HX_condensing' method).
      REAL HX_T_cond_thresh         ! Temperature threshold for condensation (used with `HX_condensing' method).

C Dilution air system and associated HRV.
      COMMON/SOFC_Dilution/Dilution_present, Dilution_airflow,
     &                     Dilution_fanpower, Dilution_heattoair,
     &                     HRV_present, HRV_freshairflow,
     &                     HRV_fanpower, HRV_effectiveness

      INTEGER Dilution_present   ! Indicates whether there is a dilution air system.
      REAL Dilution_airflow      ! Flow rate of dilution air (kmol/s).
      REAL Dilution_fanpower     ! Electrical power of fan drawing dilution air (W).
      REAL Dilution_heattoair    ! Heat transfer from FCPM to dilution air (W).
      INTEGER HRV_present        ! Indicates whether an HRV is present.
      REAL HRV_freshairflow      ! Flow rate of fresh air through HRV (kmol/s).
      REAL HRV_fanpower          ! Electrical power of fan drawing air through HRV (W).
      REAL HRV_effectiveness     ! Effectiveness of gas-to-air heat exchange (-).

C Battery.
      COMMON/SOFC_battery/battery_capacity, battery_max_charge_rate,
     &                    battery_max_discharge_rate,
     &                    battery_charge_eff, battery_discharge_eff,
     &                    battery_SOC_initial, battery_heat_to_where

      REAL battery_capacity           ! Battery's energy storage capacity (J).
      REAL battery_max_charge_rate    ! Max rate at which battery can be charged (W).
      REAL battery_max_discharge_rate ! Max rate at which battery can be discharged (W).
      REAL battery_charge_eff         ! Energetic efficiency during charging (-).
      REAL battery_discharge_eff      ! Energetic efficiency during discharging (-).
      REAL battery_SOC_initial        ! Battery's SOC (fraction of battery_capacity at
                                      ! start of simulation.
      INTEGER battery_heat_to_where   ! Indicates where heat loss from battery goes.

C PCU.
      COMMON/SOFC_PCU/PCU_u0, PCU_u1, PCU_u2, PCU_heat_to_where

      REAL PCU_u0               ! Coeff to polynomial to calculate efficiency of PCU.
      REAL PCU_u1               ! Ditto.
      REAL PCU_u2               ! Ditto.
      INTEGER PCU_heat_to_where ! Indicates where heat loss from PCU goes.

C Heat extraction for stack  cooling  (PEMFC)  
      COMMON/PEMFC_STACK_COOL/stack_cooling_present,
     &                        T_stack,T_stack_0, 
     &                        s_cool_e_0,s_cool_e_1,
     &                        s_cool_e_2,s_cool_e_3,
     &                        UA_s_cool,
     &                        Mdot_s_cool,
     &                        UA_s_cogen,Calc_UA_s_cogen,
     &                        Mdot_s_cogen,Mdot_s_cogen_0,
     &                        A_s_cogen,R_s_cogen,h_s_cogen_0,n_s_cogen,
     &                        air_cool_f_0,air_cool_f_1,air_cool_f_2,
     &                        s_cool_pump_heat_loss,s_cool_pump_Pel,
     &                        cogen_HX_state


      INTEGER stack_cooling_present  ! indicates whether there is a stack cooling system
      REAL T_stack               ! Stack temperature
      REAL T_stack_0             ! Nominal stack temperature

      REAL s_cool_e_0            ! Coeff to polynomial to calculate heat extracted from stack
      REAL s_cool_e_1            ! Ditto
      REAL s_cool_e_2            ! Ditto
      REAL s_cool_e_3            ! Ditto

      REAL UA_s_cool             ! UA value of stack cooling heat exchanger
      REAL Mdot_s_cool        ! Massflow rate in stack cooling heat exchanger 
      REAL UA_s_cogen            ! UA value of heat exchanger for cogen from stack cooling
      REAL Mdot_s_cogen       ! Massflow rate in heat exchanger for cogen from stack cooling 
      REAL Mdot_s_cogen_0        ! Nominal massflow rate in heat exchanger for cogen from stack cooling
      REAL A_s_cogen             ! Area of heat exchanger for cogen from stack cooling
      REAL R_s_cogen             ! Constant part of heat exchanger resistant
      REAL h_s_cogen_0           ! Nominal film heat transfer coefficient
      REAL n_s_cogen             ! Exponent to water flow rate   
      REAL air_cool_f_0          ! Coeff to polynomial to calculate electrical power consumption of air cooler fan
      REAL air_cool_f_1          ! Ditto
      REAL air_cool_f_2          ! Ditto
      REAL s_cool_pump_heat_loss ! Ratio of heat loss of the stack cooling pump
      REAL s_cool_pump_Pel       ! Electrical power consumption of the stack cooling pump 
      INTEGER cogen_HX_state     ! state variable of the cogen-HX of stack cooling system
      LOGICAL Calc_UA_s_cogen    ! Flag to indicat whether UA_s_cogen has to be calculated in each timestep

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA,Itemp,N_expect,IPCONC
      INTEGER IPCOMP
      INTEGER j
      REAL fuel_molefrac_total  ! Sum of fuel molar fractions.
      REAL air_molefrac_total   ! Sum of air molar fractions.
      LOGICAL mistake,CLOSE
      REAL Shomate         ! Function that applies Shomate equation to calculate gas properties.


C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. ITRACE(35)/=0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-sim data for the:'
        WRITE(ITU,*) ' 14-node Annex 42 SOFC-cogen model  '
        NumADATA = 134  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NumADATA)
        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(IPCOMP) holds the number of possible plant control variables
C as specified in the .pln file. There are two control variables: one for the
C electrical output and one to control the auxiliary burner.
C---------------------------------------------------------------------------------
      N_expect = 2
      IF(NCI(IPCOMP) /= N_expect) THEN
        WRITE(ITU,*) ' SOFC_A42_static: incorrect number',
     &               ' of controlled variables specified.'
      ENDIF


C---------------------------------------------------------------------------------
C Check that each node in the component has the correct number of connections
C to other components.
C Variables used:
C    MPCONC           the maximum allowable connections to a node (from plant.h).
C    ICONDX(i,j,k)    the connection number for the k'th connection to the j'th node
C                     of component i. It is used as a pointer.
C    `mistake'        a flag indicating whether there are connection errors:
C                     .true. means there are errors.
C---------------------------------------------------------------------------------
      mistake = .false.
C-----There should be no connections to nodes 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, and 13.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP, 1,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 2,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 3,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 4,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 5,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 7,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 8,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP, 9,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,10,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,11,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,12,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,13,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----There should be one connection each to nodes 6 and 14.
      IF( ICONDX(IPCOMP, 6,1) == 0 ) mistake=.true.
      IF( ICONDX(IPCOMP,14,1) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX(IPCOMP, 6,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,14,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: incorrect number of'
        WRITE(IUOUT,*) ' connection for component ',IPCOMP
        STOP ' SOFC_A42_static: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connections to nodes 6 and 14 are of the correct type. These
C connections should be of type ISV=20 so that the SOFC-cogen component can be used
C in networks in which both first and second phase mass balances are formed.
C Variables used:
C    ISV        defines nodal fluid type & coefficient generator model capabilities:
C               ISV=0,10,20 node represents water + ....
C               ISV=1,11,21 node represents dry air + ....
C               ISV=9,19,29 node represents some solid material only
C               0 <=ISV<10  model suitable for energy balance only
C               10<=ISV<20  model suitable for energy + single phase mass balance
C               20<=ISV<30  model suitable for energy + two phase mass balances
C---------------------------------------------------------------------------------
      mistake = .false.
      IF( ICONTP( ICONDX(IPCOMP, 6,1) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: incorrect connection type'
        WRITE(IUOUT,*) ' to node 6 for component ',IPCOMP
        STOP ' SOFC_A42_static: unresolvable error'
      END IF
      mistake = .false.
      IF( ICONTP( ICONDX(IPCOMP,14,1) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: incorrect connection type'
        WRITE(IUOUT,*) ' to node 14 for component ',IPCOMP
        STOP ' SOFC_A42_static: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Set flag to force iteration between the plant and electrical domains. This will
C ensure that in the case of electric load following that the SOFC-cogeneration
C unit responds to the electrical load solved for the same time-step.
C---------------------------------------------------------------------------------
      bInter_domain_iteration = .true.


C=================
C FCPM ELECTRICAL
C=================

C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients and the degredation term that
C describe the FCPM's electrical efficiency.
C---------------------------------------------------------------------------------
      FCPM_eta_0          = ADATA(IPCOMP,1)
      FCPM_eta_1          = ADATA(IPCOMP,2)
      FCPM_eta_2          = ADATA(IPCOMP,3)
      FCPM_degrade_ONOFF  = ADATA(IPCOMP,4)  ! Dedgradation term for on/off cycling.
      FCPM_degrade_optime = ADATA(IPCOMP,5)  ! Degradation due to operating time.
      FCPM_degrade_thresh = ADATA(IPCOMP,6)  ! Time threshold for `optime' degradation.


C---------------------------------------------------------------------------------
C Assign user-specified minimum and maximum allowable electrical output from
C FCPM.
C---------------------------------------------------------------------------------
      FCPM_elec_min = ADATA(IPCOMP,7)
      FCPM_elec_max = ADATA(IPCOMP,8)

C---------------------------------------------------------------------------------
C Assign user-specified transient response characteristics.
C---------------------------------------------------------------------------------
      FCPM_dPdt_inc_limit     = ADATA(IPCOMP,9)
      FCPM_dPdt_dec_limit     = ADATA(IPCOMP,10)
      FCPM_dt_startup         = ADATA(IPCOMP,11)
      FCPM_fuel_cons_startup  = ADATA(IPCOMP,12)
      FCPM_elec_cons_startup  = ADATA(IPCOMP,13)
      FCPM_elec_prod_startup  = ADATA(IPCOMP,14)
      FCPM_dt_cooldown        = ADATA(IPCOMP,15)
      FCPM_fuel_cons_cooldown = ADATA(IPCOMP,16)
      FCPM_elec_cons_cooldown = ADATA(IPCOMP,17)

C---------------------------------------------------------------------------------
C Assign user-specified coefficients that are used to determine the AC power
C that is supplied to the FCPM for ancillaries.
C---------------------------------------------------------------------------------
      FCPM_anc_0 = ADATA(IPCOMP,18)
      FCPM_anc_1 = ADATA(IPCOMP,19)


C=============
C FUEL SUPPLY 
c=============

C---------------------------------------------------------------------------------
C Assign user-specified molar fractions of fuel constituents to common block
C variables. These will be used in the coefficient generator. The FCPM and
C auxiliary burner are supplied with the same fuel mixture.
C---------------------------------------------------------------------------------
      chi_fuel_H2     = ADATA(IPCOMP,20)
      chi_fuel_CH4    = ADATA(IPCOMP,21)  ! methane
      chi_fuel_C2H6   = ADATA(IPCOMP,22)  ! ethane
      chi_fuel_C3H8   = ADATA(IPCOMP,23)  ! propane
      chi_fuel_C4H10  = ADATA(IPCOMP,24)  ! butnae
      chi_fuel_C5H12  = ADATA(IPCOMP,25)  ! pentane
      chi_fuel_C6H14  = ADATA(IPCOMP,26)  ! hexane
      chi_fuel_CH3OH  = ADATA(IPCOMP,27)  ! methanol
      chi_fuel_C2H5OH = ADATA(IPCOMP,28)  ! ethanol
      chi_fuel_CO2    = ADATA(IPCOMP,29)
      chi_fuel_N2     = ADATA(IPCOMP,30)
      chi_fuel_O2     = ADATA(IPCOMP,31)

C---------------------------------------------------------------------------------
C Check to ensure that the molar fractions of the fuel constituents sum to unity.
C---------------------------------------------------------------------------------
      fuel_molefrac_total = chi_fuel_H2 + chi_fuel_CH4 + chi_fuel_C2H6
     &                      + chi_fuel_C3H8 + chi_fuel_C4H10
     &                      + chi_fuel_C5H12 + chi_fuel_C6H14
     &                      + chi_fuel_CH3OH + chi_fuel_C2H5OH
     &                      + chi_fuel_CO2 + chi_fuel_N2 + chi_fuel_O2
      CALL ECLOSE(fuel_molefrac_total,1.0,0.001,CLOSE)
      IF(.not.CLOSE)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: molar fractions of fuel'
        WRITE(IUOUT,*) ' do not sum to unity.'
        STOP ' SOFC_A42_static: unresolvable error'
      END IF

C---------------------------------------------------------------------------------
C Calculate LHV of fuel mixture.  Note that the temperature supplied to the
C Shomate function is inconsequential for the purposes of calculating the LHV.
C---------------------------------------------------------------------------------
      LHV_fuel = chi_fuel_H2     * Shomate(LHV_i,333.,H2,IUOUT)
     &         + chi_fuel_CH4    * Shomate(LHV_i,333.,CH4,IUOUT)
     &         + chi_fuel_C2H6   * Shomate(LHV_i,333.,C2H6,IUOUT)
     &         + chi_fuel_C3H8   * Shomate(LHV_i,333.,C3H8,IUOUT)
     &         + chi_fuel_C4H10  * Shomate(LHV_i,333.,C4H10,IUOUT)
     &         + chi_fuel_C5H12  * Shomate(LHV_i,333.,C5H12,IUOUT)
     &         + chi_fuel_C6H14  * Shomate(LHV_i,333.,C6H14,IUOUT)
     &         + chi_fuel_CH3OH  * Shomate(LHV_i,333.,CH3OH,IUOUT)
     &         + chi_fuel_C2H5OH * Shomate(LHV_i,333.,C2H5OH,IUOUT)

C---------------------------------------------------------------------------------
C Determine method specified by user for establishing temperature of fuel supply
C to compressor and assign user-specified coefficients.
C---------------------------------------------------------------------------------
      Compressor_source_fuel = INT( ADATA(IPCOMP,32) )
      compressor_0           = ADATA(IPCOMP,33)
      compressor_1           = ADATA(IPCOMP,34)
      compressor_2           = ADATA(IPCOMP,35)
      compressor_3           = ADATA(IPCOMP,36)
      Compressor_heat_loss   = ADATA(IPCOMP,37)
      IF( Compressor_heat_loss>1. )THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: heat loss ratio of fuel '
        WRITE(IUOUT,*) ' compressor must be less than 1. '
        STOP ' SOFC_A42_static: unresolvable error'
      END IF


C=====
C AIR
C=====

C---------------------------------------------------------------------------------
C Assign user-specified molar fractions of air constituents to common block
C variables. These will be used in the coefficient generator.
C---------------------------------------------------------------------------------
      chi_air_N2  = ADATA(IPCOMP,38)
      chi_air_O2  = ADATA(IPCOMP,39)
      chi_air_H2O = ADATA(IPCOMP,40)
      chi_air_Ar  = ADATA(IPCOMP,41)
      chi_air_CO2 = ADATA(IPCOMP,42)

C---------------------------------------------------------------------------------
C Check to ensure that the molar fractions of the air constituents sum to unity.
C---------------------------------------------------------------------------------
      air_molefrac_total = chi_air_N2 + chi_air_O2 + chi_air_H2O
     &                     + chi_air_Ar + chi_air_CO2
      CALL ECLOSE(air_molefrac_total,1.0,0.001,CLOSE)
      IF(.not.CLOSE)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: molar fractions of air'
        WRITE(IUOUT,*) ' do not sum to unity.'
        STOP ' SOFC_A42_static: unresolvable error'
      END IF

C---------------------------------------------------------------------------------
C Determine method specified by user for establishing air supply to FCPM and
C assign user-specified air-supply coefficients. 
C---------------------------------------------------------------------------------
      FCPM_air_supply_method = INT( ADATA(IPCOMP,43) )
      FCPM_air_0             = ADATA(IPCOMP,44)
      FCPM_air_1             = ADATA(IPCOMP,45)
      FCPM_air_2             = ADATA(IPCOMP,46)
      FCPM_air_3             = ADATA(IPCOMP,47)

C---------------------------------------------------------------------------------
C Determine method specified by user for establishing temperature of air intake
C to supply blower and assign user-specified coefficients.
C---------------------------------------------------------------------------------
      Blower_source_air = INT( ADATA(IPCOMP,48) )
      blower_0          = ADATA(IPCOMP,49)
      blower_1          = ADATA(IPCOMP,50)
      blower_2          = ADATA(IPCOMP,51)
      blower_3          = ADATA(IPCOMP,52)
      Blower_heat_loss  = ADATA(IPCOMP,53)
      IF( Blower_heat_loss>1. )THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: heat loss ratio of air '
        WRITE(IUOUT,*) ' supply blower must be less than 1. '
        STOP ' SOFC_A42_static: unresolvable error'
      END IF


C==============
C WATER SUPPLY
C==============

C---------------------------------------------------------------------------------
C Assign user-specified coefficients for establishing water supply to FCPM.
C---------------------------------------------------------------------------------
      FCPM_water_0 = ADATA(IPCOMP,54)
      FCPM_water_1 = ADATA(IPCOMP,55)
      FCPM_water_2 = ADATA(IPCOMP,56)

C---------------------------------------------------------------------------------
C Determine method specified by user for establishing temperature of water supply
C to pump and assign user-specified coefficients.
C---------------------------------------------------------------------------------
      Pump_source_water = INT( ADATA(IPCOMP,57) )
      pump_0            = ADATA(IPCOMP,58)
      pump_1            = ADATA(IPCOMP,59)
      pump_2            = ADATA(IPCOMP,60)
      pump_3            = ADATA(IPCOMP,61)
      Pump_heat_loss    = ADATA(IPCOMP,62)


C=============================
C GAS-TO-WATER HEAT EXCHANGER
C=============================

C---------------------------------------------------------------------------------
C Determine method used to characterize gas-to-water heat exchanger and assign
C user-specified coefficients.
C---------------------------------------------------------------------------------
      HX_method = INT( ADATA(IPCOMP,63) )
      SELECT CASE ( HX_method )
      CASE( HX_fixed_eff )
        HX_eff           = ADATA(IPCOMP,64)
      CASE( HX_lmtd_empirical )
        HX_sen_0         = ADATA(IPCOMP,65)
        HX_sen_1         = ADATA(IPCOMP,66)
        HX_sen_2         = ADATA(IPCOMP,67)
        HX_sen_3         = ADATA(IPCOMP,68)
        HX_sen_4         = ADATA(IPCOMP,69)
      CASE( HX_lmtd_deterministic )
        HX_h0_gas        = ADATA(IPCOMP,70)
        HX_Ndot0_gas     = ADATA(IPCOMP,71)
        HX_n             = ADATA(IPCOMP,72)
        HX_Area_gas      = ADATA(IPCOMP,73)
        HX_h0_water      = ADATA(IPCOMP,74)
        HX_Ndot0_water   = ADATA(IPCOMP,75)
        HX_m             = ADATA(IPCOMP,76)
        HX_Area_water    = ADATA(IPCOMP,77)
        HX_F             = ADATA(IPCOMP,78)
      CASE ( HX_condensing )
        HX_sen_0         = ADATA(IPCOMP,65)
        HX_sen_1         = ADATA(IPCOMP,66)
        HX_sen_2         = ADATA(IPCOMP,67)
        HX_sen_3         = ADATA(IPCOMP,68)
        HX_sen_4         = ADATA(IPCOMP,69)
        HX_lat_1         = ADATA(IPCOMP,79)
        HX_lat_2         = ADATA(IPCOMP,80)
        HX_T_cond_thresh = ADATA(IPCOMP,81)
      END SELECT


C=================
C FCPM SKIN LOSSES
C=================

C---------------------------------------------------------------------------------
C Determine method specified by user for calculating skin losses from FCPM,
C the convective portion of these losses, and assign user-specified coefficients.
C---------------------------------------------------------------------------------
      FCPM_skin_loss_method = INT( ADATA(IPCOMP,82) )
      FCPM_skin_loss_conv   = ADATA(IPCOMP,83)
      FCPM_skin_0           = ADATA(IPCOMP,84)
      FCPM_skin_1           = ADATA(IPCOMP,85)
      FCPM_skin_2           = ADATA(IPCOMP,86)


C=================
C AUXILIARY BURNER
C=================

C---------------------------------------------------------------------------------
C Determine whether there is an auxiliary burner, whether its heat loss will be
C transferred to FCPM air intake or containing room, and establish burner
C parameters.
C---------------------------------------------------------------------------------
      Auxburn_present       = INT( ADATA(IPCOMP,87) )
      SELECT CASE ( Auxburn_present )
      CASE ( Auxburn_yes )
        Auxburn_W_or_kmols    = INT( ADATA(IPCOMP,88) )
        Auxburn_min_output    = ADATA(IPCOMP,89)
        Auxburn_max_output    = ADATA(IPCOMP,90)
        Auxburn_heat_to_where = INT( ADATA(IPCOMP,91) )
        Auxburn_UA            = ADATA(IPCOMP,92)
        Auxburn_anc_0         = ADATA(IPCOMP,93)
        Auxburn_anc_1         = ADATA(IPCOMP,94)
        Auxburn_lambda        = ADATA(IPCOMP,95)
      CASE ( Auxburn_no )
      END SELECT


C=============================
C DILUTION AIR SYSTEM AND HRV
C=============================
      Dilution_present     = INT( ADATA(IPCOMP,96 ) )
      SELECT CASE ( Dilution_present )
      CASE ( Dilution_yes )
        Dilution_airflow   = ADATA(IPCOMP,97)
        Dilution_fanpower  = ADATA(IPCOMP,98)
        Dilution_heattoair = ADATA(IPCOMP,99)
      CASE ( Dilution_no )
        Dilution_airflow   = 0.
        Dilution_fanpower  = 0.
        Dilution_heattoair = 0.
      END SELECT
      HRV_present          = INT( ADATA(IPCOMP,100 ) )
      SELECT CASE ( HRV_present )
      CASE (SOFC_HRV_yes)
        HRV_freshairflow   = ADATA(IPCOMP,101)
        HRV_fanpower       = ADATA(IPCOMP,102)
        HRV_effectiveness  = ADATA(IPCOMP,103)
      CASE (SOFC_HRV_no)
        HRV_freshairflow   = 0.
        HRV_fanpower       = 0.
        HRV_effectiveness  = 0.
      END SELECT


C=========
C BATTERY
C=========
      battery_capacity           = ADATA(IPCOMP,104)
      CALL ECLOSE(battery_capacity, 0.,1.E-12,CLOSE)
      IF (CLOSE) THEN
        battery_capacity         = 1.E-5
        WRITE(IUOUT,*)
     &' SOFC_A42_static: battery capacity must not be zero!'
        WRITE(IUOUT,*)' It has been set to 1.E-5 J'
      ENDIF   
      battery_max_charge_rate    = ADATA(IPCOMP,105)
      battery_max_discharge_rate = ADATA(IPCOMP,106)
      battery_charge_eff         = ADATA(IPCOMP,107)
      battery_discharge_eff      = ADATA(IPCOMP,108)
      battery_SOC_initial        = ADATA(IPCOMP,109)
      battery_heat_to_where      = INT( ADATA(IPCOMP,110) )


C=====
C PCU
C=====
      PCU_u0            = ADATA(IPCOMP,111)
      PCU_u1            = ADATA(IPCOMP,112)
      PCU_u2            = ADATA(IPCOMP,113)
      PCU_heat_to_where = INT( ADATA(IPCOMP,114) )


C=============================
C STACK COOLING SYSTEM (PEMFC)
C=============================
      stack_cooling_present = INT(ADATA(IPCOMP,115))
      SELECT CASE (stack_cooling_present)
      CASE (s_cool_yes)  
        T_stack               = ADATA(IPCOMP,116)
        T_stack_0             = ADATA(IPCOMP,117)

        s_cool_e_0            = ADATA(IPCOMP,118)
        s_cool_e_1            = ADATA(IPCOMP,119)
        s_cool_e_2            = ADATA(IPCOMP,120)
        s_cool_e_3            = ADATA(IPCOMP,121)
        UA_s_cool             = ADATA(IPCOMP,122)
        Mdot_s_cool           = ADATA(IPCOMP,123)
        CALL ECLOSE(Mdot_s_cool, 0.,1.E-12,CLOSE) 
        IF (CLOSE) THEN                                 ! Flow in stack cool heat exchanger too small
           stack_cooling_present=s_cool_no              ! --> no stack cooling
        ENDIF
        UA_s_cogen            = ADATA(IPCOMP,124)
        CALL ECLOSE(UA_s_cogen, 0.,1.E-12,Calc_UA_s_cogen) 

        Mdot_s_cogen_0        = ADATA(IPCOMP,125)
        A_s_cogen             = ADATA(IPCOMP,126)
        R_s_cogen             = ADATA(IPCOMP,127)
        h_s_cogen_0           = ADATA(IPCOMP,128)
        n_s_cogen             = ADATA(IPCOMP,129)

        air_cool_f_0          = ADATA(IPCOMP,130)
        air_cool_f_1          = ADATA(IPCOMP,131)
        air_cool_f_2          = ADATA(IPCOMP,132)
        s_cool_pump_heat_loss = ADATA(IPCOMP,133)
        s_cool_pump_Pel       = ADATA(IPCOMP,134)
      CASE (s_cool_no)
      END SELECT


C---------------------------------------------------------------------------------
C Completion of analysis prior to commencing time-step simulation.
C---------------------------------------------------------------------------------
      RETURN
      END



C *********************************************************************************************
C ********************************* SOFC_A42_coeffgen *******************************************

C This subroutine is the coefficient generator for the Annex 42 SOFC-cogen component model.
C It is invoked each iteration of the plant matrix processing during each time-step
C to establish the coefficients for the sub-matrices that define the energy, 1st phase
C mass flow, and 2nd phase mass flow balances on the SOFC-cogen unit's control volumes.

C The FC-cogen device is represented with eight control volumes (aka nodes):
C   CV1 - The air within the air supply blower.
C   CV2 - The gas within the fuel compressor.
C   CV3 - The water within the water pump.
C   CV4 - The gas within the fuel cell power module (FCPM).
C   CV5 - The gas within the auxiliary burner.
C   CV6 - The water within and the solid portion of the heat recovery device which
C         transfers energy from the hot exhaust gases to the heat recovery loop. This
C         water then flows to connected plant components.
C   CV7 - The gas within the heat recovery device.
C   CV8 - The gas within the dilution air mixing system.
C   CV9 - The gas within the HRV (hot side of HRV).
C   CV10 - The water within the stack cooling loop, at the inlet to the stack
C         heat exchanger.
C   CV11- The water within the stack cooling loop, at the outlet from the stack
C         heat exchanger.
C   CV12- The water within the stack cooling loop, at the outlet from the external
C         heat exchanger which transfers energy to heat recovery loop.
C   CV13- The water within the stack cooling loop, at the outlet from the air cooler.
C   CV14- The water within the heat recovery loop, at the outlet from the external
C         heat exchanger which transfers energy from the stack cooling loop. This water
C         then flows to connected plant components.
C -------------------------------------------------------------------------------------------

      SUBROUTINE SOFC_A42_coeffgen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "Annex42_fuel_cell.h"
#include "plant.h"
#include "building.h"
#include "ground_temp_mains_common.h"
#include "dhw_common.h"

C External functions
      integer lnblnk

C--------------------------------------------------------------------------------------------
C Common blocks.
C--------------------------------------------------------------------------------------------
C Unit numbers for providing feedback to user during simulation.
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Connecting node data.
      COMMON/C10/NPCON,IPC1,IPN1,IPCT,IPC2,IPN2,PCONDR,PCONSD
      INTEGER NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON)
      INTEGER IPC2(MPCON),IPN2(MPCON)
      REAL PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/PCOND/CONVAR,ICONTP,ICONDX
      REAL CONVAR(MPCON,MCONVR)
      INTEGER ICONTP(MPCON),ICONDX(MPCOM,MNODEC,MPCONC)

C Trace output.
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C Additional res output from plant domain.
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA, PCAOUT
      INTEGER napdat

C Simulation time step data.
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Plant component pointer.
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2 
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Iteration control.
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV,CSVI
      INTEGER MAXITP, itrclp, ICSV(MPNODE,MPVAR)
      REAL PERREL, PERTMP, PERFLX, PERMFL, CSVI(MPNODE,MPVAR)

C Plant state variables.
      COMMON/PCVAL/CSVF,CSVP
      REAL CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

C Number of components in plant network and control actuation signals.
      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

C `Additional' plant component data.
      common/pcdat/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      REAL PCDATF   ! Future time-row.
      real PCDATP   ! Present time-row.

C Length of time-step in plant domain (s).
      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

C Plant containment data.
      COMMON/PCVAR/PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP
      REAL PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE)
      REAL PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE)
      REAL PCQP(MPNODE),PCNTMP(MPCOM)

C Climate data at the present and future time rows.
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

C Plant component ASCII names.
      common/pcnam/pcname(mpcom)
      character*15 pcname        ! Plant component names

C Electrical domain data.
      common/elpcp/
     &     NPEL,         ! number of "electrified" plant components 
     &     PFP,          ! power factor of componet i (0->1)
     &     IPFP,         ! integer indicating if power factor lags (-1) or leads (+1)
     &     PWRP,         ! real power consumption of component (generation is -ive)
     &     BVOLTP,       ! opertional voltage of component 
     &     IPHP          ! phase component is connected to -> dummy variable
      INTEGER NPEL,IPFP(mpcom),IPHP(mpcom)
      REAL PFP(mpcom),PWRP(mpcom),BVOLTP(mpcom)

C FCPM electrical performance.
      COMMON/FCPM_elec/FCPM_eta_0, FCPM_eta_1, FCPM_eta_2,
     &                 FCPM_degrade_ONOFF, FCPM_degrade_optime,
     &                 FCPM_degrade_thresh,
     &                 FCPM_elec_min,FCPM_elec_max
      REAL FCPM_eta_0          ! Polynomial coeff that expresses FCPM elec efficiency.
      REAL FCPM_eta_1          ! Ditto.
      REAL FCPM_eta_2          ! Ditto.
      REAL FCPM_degrade_ONOFF  ! Fractional performance degradation for off-on cycling.
      REAL FCPM_degrade_optime ! Fractional performance degradation due to operating time.
      REAL FCPM_degrade_thresh ! Time threshold (hrs) before which no degradation due to
                               ! operating time occurs.
      REAL FCPM_elec_min       ! FCPM's min elec output (W).
      REAL FCPM_elec_max       ! FCPM's max elec output (W).

C FCPM transient response.
      COMMON/FCPM_transient/FCPM_dPdt_inc_limit, FCPM_dPdt_dec_limit,
     &                      FCPM_dt_startup, FCPM_fuel_cons_startup,
     &                      FCPM_elec_cons_startup,
     &                      FCPM_elec_prod_startup, FCPM_dt_cooldown,
     &                      FCPM_elec_cons_cooldown,
     &                      FCPM_fuel_cons_cooldown
      REAL FCPM_dPdt_inc_limit     ! Max allowable time derivative for elec output in W/s (power increasing)
      REAL FCPM_dPdt_dec_limit     ! Max allowable time derivative for elec output in W/s (power increasing)
      REAL FCPM_dt_startup         ! Duration of start-up period (seconds).
      REAL FCPM_fuel_cons_startup  ! Fuel consumption during start-up period (kmol).
      REAL FCPM_elec_cons_startup  ! Electrical consumption during start-up period (MJ).
      REAL FCPM_elec_prod_startup  ! Net DC electrical production during start-up period (MJ).
      REAL FCPM_dt_cooldown        ! Duration of cool-down period (seconds).
      REAL FCPM_fuel_cons_cooldown ! Fuel consumption during cool-down period (kmol).
      REAL FCPM_elec_cons_cooldown ! Electrical consumption during cool-down period (MJ).

C Fuel constituents.
      COMMON/SOFC_fuel_constituents/
     &                 chi_fuel_H2, chi_fuel_CH4, chi_fuel_C2H6,
     &                 chi_fuel_C3H8, chi_fuel_C4H10, chi_fuel_C5H12,
     &                 chi_fuel_C6H14, chi_fuel_CH3OH, chi_fuel_C2H5OH,
     &                 chi_fuel_CO2, chi_fuel_N2, chi_fuel_O2
      REAL chi_fuel_H2     ! Molar fraction of hydrogen.
      REAL chi_fuel_CH4    ! Molar fraction of methans.
      REAL chi_fuel_C2H6   ! Molar fraction of ethane.
      REAL chi_fuel_C3H8   ! Molar fraction of propane.
      REAL chi_fuel_C4H10  ! Molar fraction of butane.
      REAL chi_fuel_C5H12  ! Molar fraction of pentane.
      REAL chi_fuel_C6H14  ! Molar fraction of hexane.
      REAL chi_fuel_CH3OH  ! Molar fraction of methanol.
      REAL chi_fuel_C2H5OH ! Molar fraction of ethanol.
      REAL chi_fuel_CO2    ! Molar fraction of carbon dioxide in fuel (inert).
      REAL chi_fuel_N2     ! Molar fraction of nitrogen in fuel (inert).
      REAL chi_fuel_O2     ! Molar fraction of oxygen in fuel (inert).

C Fuel mixture properties.
      COMMON/SOFC_fuel_mixture/LHV_fuel
      REAL LHV_fuel       ! LHV of fuel supplied to FCPM and auxiliary burner (J/kmol).

C Air constituents.
      COMMON/SOFC_air/chi_air_N2, chi_air_O2, chi_air_H2O,
     &                chi_air_Ar, chi_air_CO2
      REAL chi_air_N2      ! Molar fraction of nitrogen.
      REAL chi_air_O2      ! Molar fraction of oxygen.
      REAL chi_air_H2O     ! Molar fraction of water vapour.
      REAL chi_air_Ar      ! Molar fraction of argon.
      REAL chi_air_CO2     ! Molar fraction of carbon dioxide.

C Air supply.
      COMMON/SOFC_air_supply/FCPM_air_supply_method,
     &                       FCPM_air_0, FCPM_air_1, FCPM_air_2,
     &                       FCPM_air_3
      INTEGER FCPM_air_supply_method  ! Method used to establish air supply to FCPM.
      REAL FCPM_air_0                 ! Excess air ratio or coeff to polynomial (depends on method)
      REAL FCPM_air_1                 ! Coeff to polynomial determining air supply.
      REAL FCPM_air_2                 ! Ditto.
      REAL FCPM_air_3                 ! Ditto.

C Water supply.
      COMMON/SOFC_water_supply/FCPM_water_0, FCPM_water_1, FCPM_water_2
      REAL FCPM_water_0    ! Coeff to polynomial determing water supply.
      REAL FCPM_water_1    ! Ditto.
      REAL FCPM_water_2    ! Ditto.

C AC power supply to FCPM for ancillaries.
      COMMON/SOFC_ancillaries/FCPM_anc_0, FCPM_anc_1
      REAL FCPM_anc_0      ! Coeff to polynomial determining FCPM AC ancillary power draw.
      REAL FCPM_anc_1      ! Ditto.

C FCPM skin losses.
      COMMON/SOFC_skin_losses/FCPM_skin_loss_method,FCPM_skin_loss_conv,
     &                        FCPM_skin_0, FCPM_skin_1, FCPM_skin_2
      INTEGER FCPM_skin_loss_method  ! Method used to determine skin losses from FCPM.
      REAL FCPM_skin_loss_conv       ! Frac of heat loss that is convective.
      REAL FCPM_skin_0               ! Skin loss (W), UA-value (W/K), or coeff (depends on method).
      REAL FCPM_skin_1               ! Coeff to polynomial for `fuel flow' method.
      REAL FCPM_skin_2               ! Ditto.

C Air supply blower.
      COMMON/SOFC_blower/Blower_source_air, blower_0, blower_1,
     &                   blower_2, blower_3, Blower_heat_loss
      INTEGER Blower_source_air  ! Indicates source from which air is drawn.
      REAL blower_0              ! Coeff to polynomial that establishes blower power draw.
      REAL blower_1              ! Ditto.
      REAL blower_2              ! Ditto.
      REAL blower_3              ! Ditto.
      REAL Blower_heat_loss      ! Ratio of heat loss from blower to elec power supply.

C Fuel supply compressor.
      COMMON/SOFC_compressor/Compressor_source_fuel, compressor_0,
     &                       compressor_1, compressor_2, compressor_3,
     &                       Compressor_heat_loss
      INTEGER Compressor_source_fuel  ! Indicates temp of fuel entering comprssor.
      REAL compressor_0               ! Coeff to polynomial that establishes compressor power draw.
      REAL compressor_1               ! Ditto.
      REAL compressor_2               ! Ditto.
      REAL compressor_3               ! Ditto.
      REAL Compressor_heat_loss       ! Ratio of heat loss from compressor to elec power supply.

C Water pump.
      COMMON/SOFC_pump/Pump_source_water,pump_0, pump_1,
     &                 pump_2, pump_3, Pump_heat_loss

      INTEGER Pump_source_water  ! Indicates temp of water entering pump.
      REAL pump_0                ! Coeff to polynomial that establishes pump power draw.
      REAL pump_1                ! Ditto.
      REAL pump_2                ! Ditto.
      REAL pump_3                ! Ditto.
      REAL Pump_heat_loss        ! Ratio of heat loss from pump to elec power supply.

C Auxiliary burner.
      COMMON/SOFC_auxburner/Auxburn_present, Auxburn_W_or_kmols,
     &                      Auxburn_min_output, Auxburn_max_output,
     &                      Auxburn_heat_to_where,
     &                      Auxburn_UA, Auxburn_anc_0, Auxburn_anc_1,
     &                      Auxburn_lambda

      INTEGER Auxburn_present       ! Indicates whether there is an auxiliary burner.
      INTEGER Auxburn_W_or_kmols    ! Indicates whether burner cap spec in heat output or fuel input.
      REAL Auxburn_min_output       ! Minimum operating point for burner (W or kmol/s).
      REAL Auxburn_max_output       ! Maximum operating point for burner (W or kmol/s).
      INTEGER Auxburn_heat_to_where ! Indicates where the heat loss from the burner goes.
      REAL Auxburn_UA               ! Heat loss coefficient for burner (W/K).
      REAL Auxburn_anc_0            ! Coeff to polynomial that determines burner ancillary power draw.
      REAL Auxburn_anc_1            ! Ditto.
      REAL Auxburn_lambda           ! Auxiliary burner excess air ratio (-).

C Gas-to-water heat exchanger.
      COMMON/SOFC_HX/HX_method, HX_eff,
     &               HX_sen_0, HX_sen_1, HX_sen_2, HX_sen_3, HX_sen_4,
     &               HX_h0_gas, HX_Ndot0_gas, HX_n, HX_Area_gas,
     &               HX_h0_water, HX_Ndot0_water, HX_m, HX_Area_water,
     &               HX_F, HX_lat_1, HX_lat_2, HX_T_cond_thresh

      INTEGER HX_method             ! Indicates which method is used to calculate heat exchange.
      REAL HX_eff                   ! Fixed effectiveness (used with `HX_fixed_eff' method.
      REAL HX_sen_0                 ! Coeffs to polynomial to calc UA (used with `HX_lmtd_empirical'
      REAL HX_sen_1                 !   and `HX_condensing' methods.
      REAL HX_sen_2                 ! Ditto.
      REAL HX_sen_3                 ! Ditto.
      REAL HX_sen_4                 ! Ditto.
      REAL HX_h0_gas                ! HX coeff to gas at nominal gas flow (used with `HX_lmtd_deterministic' method).
      REAL HX_Ndot0_gas             ! Nominal gas flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_n                     ! Exponent to gas flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_Area_gas              ! Reference heat exchange area to gas (used with `HX_lmtd_deterministic' method).
      REAL HX_h0_water              ! HX coeff to water at nominal water flow (used with `HX_lmtd_deterministic' method).
      REAL HX_Ndot0_water           ! Nominal water flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_m                     ! Exponent to water flow rate (used with `HX_lmtd_deterministic' method).
      REAL HX_Area_water            ! Reference heat exchange area to water (used with `HX_lmtd_deterministic' method).
      REAL HX_F                     ! Adjustment factor (used with `HX_lmtd_deterministic' method).
      REAL HX_lat_1                 ! Coeffs to polynomial to calc rate of condensation (used with
      REAL HX_lat_2                 !   `HX_condensing' method).
      REAL HX_T_cond_thresh         ! Temperature threshold for condensation (used with `HX_condensing' method).

C Dilution air system and associated HRV.
      COMMON/SOFC_Dilution/Dilution_present, Dilution_airflow,
     &                     Dilution_fanpower, Dilution_heattoair,
     &                     HRV_present, HRV_freshairflow,
     &                     HRV_fanpower, HRV_effectiveness

      INTEGER Dilution_present   ! Indicates whether there is a dilution air system.
      REAL Dilution_airflow      ! Flow rate of dilution air (kmol/s).
      REAL Dilution_fanpower     ! Electrical power of fan drawing dilution air (W).
      REAL Dilution_heattoair    ! Heat transfer from FCPM to dilution air (W).
      INTEGER HRV_present        ! Indicates whether an HRV is present.
      REAL HRV_freshairflow      ! Flow rate of fresh air through HRV (kmol/s).
      REAL HRV_fanpower          ! Electrical power of fan drawing air through HRV (W).
      REAL HRV_effectiveness     ! Effectiveness of gas-to-air heat exchange (-).

C Battery.
      COMMON/SOFC_battery/battery_capacity, battery_max_charge_rate,
     &                    battery_max_discharge_rate,
     &                    battery_charge_eff, battery_discharge_eff,
     &                    battery_SOC_initial, battery_heat_to_where

      REAL battery_capacity           ! Battery's energy storage capacity (J).
      REAL battery_max_charge_rate    ! Max rate at which battery can be charged (W).
      REAL battery_max_discharge_rate ! Max rate at which battery can be discharged (W).
      REAL battery_charge_eff         ! Energetic efficiency during charging (-).
      REAL battery_discharge_eff      ! Energetic efficiency during discharging (-).
      REAL battery_SOC_initial        ! Battery's SOC (fraction of battery_capacity at
                                      ! start of simulation.
      INTEGER battery_heat_to_where   ! Indicates where heat loss from battery goes.

C PCU.
      COMMON/SOFC_PCU/PCU_u0, PCU_u1, PCU_u2, PCU_heat_to_where

      REAL PCU_u0               ! Coeff to polynomial to calculate efficiency of PCU.
      REAL PCU_u1               ! Ditto.
      REAL PCU_u2               ! Ditto.
      INTEGER PCU_heat_to_where ! Indicates where heat loss from PCU goes.

C  Stack  cooling system  (PEMFC)  
      COMMON/PEMFC_STACK_COOL/stack_cooling_present,
     &                        T_stack,T_stack_0, 
     &                        s_cool_e_0,s_cool_e_1,
     &                        s_cool_e_2,s_cool_e_3,
     &                        UA_s_cool,
     &                        Mdot_s_cool,
     &                        UA_s_cogen,Calc_UA_s_cogen,
     &                        Mdot_s_cogen,Mdot_s_cogen_0,
     &                        A_s_cogen,R_s_cogen,h_s_cogen_0,n_s_cogen,
     &                        air_cool_f_0,air_cool_f_1,air_cool_f_2,
     &                        s_cool_pump_heat_loss,s_cool_pump_Pel,
     &                        cogen_HX_state

      INTEGER stack_cooling_present  ! indicates whether there is a stack cooling system
      REAL T_stack               ! Stack temperature
      REAL T_stack_0             ! Nominal stack temperature

      REAL s_cool_e_0            ! Coeff to polynomial to calculate heat extracted from stack
      REAL s_cool_e_1            ! Ditto
      REAL s_cool_e_2            ! Ditto
      REAL s_cool_e_3            ! Ditto

      REAL UA_s_cool             ! UA value of stack cooling heat exchanger
      REAL Mdot_s_cool        ! Massflow rate in stack cooling heat exchanger 
      REAL UA_s_cogen            ! UA value of heat exchanger for cogen from stack cooling
      REAL Mdot_s_cogen       ! Massflow rate in heat exchanger for cogen from stack cooling 
      REAL Mdot_s_cogen_0        ! Nominal massflow rate in heat exchanger for cogen from stack cooling
      REAL A_s_cogen             ! Area of heat exchanger for cogen from stack cooling
      REAL R_s_cogen             ! Constant part of heat exchanger resistant
      REAL h_s_cogen_0           ! Nominal film heat transfer coefficient
      REAL n_s_cogen             ! Exponent to water flow rate   
      REAL air_cool_f_0          ! Coeff to polynomial to calculate electrical power consumption of air cooler fan
      REAL air_cool_f_1          ! Ditto
      REAL air_cool_f_2          ! Ditto
      REAL s_cool_pump_heat_loss ! Ratio of heat loss of the stack cooling pump
      REAL s_cool_pump_Pel       ! Electrical power consumption of the stack cooling pump 
      INTEGER cogen_HX_state     ! state variable of the cogen-HX of stack cooling system
      LOGICAL Calc_UA_s_cogen    ! Flag to indicat whether UA_s_cogen has to be calculated in each timestep

C Iteration criteria for `additional' data items (PCDATF).
      common/pAdd_iter_criteria/iPlt_Output_Iter_Flag,
     &                          fPlt_Output_Init_Val, fPlt_Output_Tol
      INTEGER iPlt_Output_Iter_Flag (mpcom,mpcdat) ! flag denoting iteration is req'd
                                                   ! for plt additional outputs
      REAL fPlt_Output_Init_Val ( mpcom,mpcdat)    ! initial value of plt additional output
      REAL fPlt_Output_Tol (mpcom,mpcdat)          ! tolerance for convergence


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
C Bookeeping variables:
      INTEGER IPCOMP             ! Index number of component (passed in calling statement)
      REAL COUT(MPCOE)           ! Plant matrix coeffs (passed in calling statement)
      INTEGER ISTATS             ! Flag for balance under consideration: 1=energy, 2=1st phase mass flow,
                                 ! 3=2nd phase mass flow (passed in calling statement).
      INTEGER CV1,CV2,CV3,CV4,CV5,CV6,CV7,CV8 ! Indices for nodes.
      INTEGER CV9,CV10,CV11,CV12,CV13,CV14
      INTEGER con_CV6            ! Index for connection to node 6.
      INTEGER con_CV14           ! Index for connection to node 14.
      LOGICAL CLOSE,CLOSEA
      INTEGER itemp              ! A counter used in outputting trace results.
      LOGICAL bInStartup         ! Function indicating whether currently in simulation start-up
                                 ! period (=.TRUE. when in start-up period).
      INTEGER month_mains        ! The month number (for the purposes of calculating the water mains temp).
      INTEGER day_mains          ! The day of the month (not used, but returned by a subroutine call).
      CHARACTER*128 char_temp

C Variables related to electrical calculations:
      REAL eta_el                ! FCPM elec efficiency.
      REAL OFFON_count           ! Counter for number of off-on cycles.
      REAL OPER_TIME_HRS         ! Counter for number of hours that FCPM has operated since initial start.
      REAL AC_demand_net         ! Net AC power demanded from FC-cogen unit (W).
      REAL AC_production_gross   ! AC power produced by PCU (W).
      REAL AC_production_net     ! Net AC power produced by FC-cogen unit after powering ancillais (W).
      REAL DC_required_by_PCU    ! DC power that must flow into PCU (W) in order to meed net AC_demand (W).
      REAL AC_required_from_PCU  ! AC power that must flow out of PCU (W) in order to power
                                 ! ancillaries and meet net AC demand.
      REAL DC_into_PCU           ! DC power that flows into PCU (W).
      REAL PCU_losses            ! Power lost within PCU (W).
      SAVE PCU_losses            ! Variable saved as calculation of DC power flowing into PCU is iterative.
      REAL eta_PCU               ! Efficiency of PCU (AC-out/DC-in) (-).
      REAL Pel                   ! Net DC power supplied by FCPM (W).
      REAL Pel_ancAC             ! Power draw of FCPM ancillaries power by AC (W).
      SAVE Pel_ancAC             ! Variable saved as calculation of DC power flowing into PCU is iterative.

C Variables related to determining operating point:
      REAL sys_op                ! Operational mode for current time-step.
      REAL sys_op_last_dt        ! Operational mode during last time-step.
      INTEGER initiate_startup   ! Simulation time-step increment (NSINC) at which start-up
                                 ! procedure commenced.
      SAVE initiate_startup
      REAL how_long_starting_up  ! Elapsed time (s) since start-up procedure commenced.
      INTEGER initiate_shutdown  ! Simulation time-step increment (NSINC) at which shut-down
                                 ! procedure commenced.
      SAVE initiate_shutdown
      REAL how_long_shutting_down! Elapsed time (s) since shut-down procedure commenced.
      REAL P_battery             ! Power drawn from battery for (- value indicates charging) (W).
      REAL P_fully_charge        ! Rate of charging that would fully charge battery by end
                                 ! of time-step (W).
      REAL P_fully_discharge     ! Rate of charging that would fully deplete battery by end
                                 ! of time-step (W).
      REAL battery_SOC_p         ! Battery's SOC at beginning of time-step (frac of max capacity).
      REAL battery_SOC_f         ! Battery's SOC at end of time-step (frac of max capacity).
      REAL q_battery_loss        ! Heat loss from battery (W).

C Variables related to modelling fuel and air consumption:
      REAL Ndot_FCPM_fuel        ! Flow rate of fuel supplied to FCPM (kmol/s).
      REAL Ndot_FCPM_H2          ! Flow rate of fuel constituent to FCPM (kmol/s).
      REAL Ndot_FCPM_CH4         ! Ditto.
      REAL Ndot_FCPM_C2H6        ! Ditto.
      REAL Ndot_FCPM_C3H8        ! Ditto.
      REAL Ndot_FCPM_C4H10       ! Ditto.
      REAL Ndot_FCPM_C5H12       ! Ditto.
      REAL Ndot_FCPM_C6H14       ! Ditto.
      REAL Ndot_FCPM_CH3OH       ! Ditto.
      REAL Ndot_FCPM_C2H5OH      ! Ditto.
      REAL Ndot_FCPM_O2_stoich   ! Stoichiometric O2 flow rate to FCPM (kmol/s).
      REAL Ndot_FCPM_air_stoich  ! Stoichiometric air flow rate to FCPM (kmol/s).
      REAL Ndot_FCPM_air         ! Air flow rate to FCPM (kmol/s).
      REAL lambda_FCPM           ! Excess air ratio for FCPM.
      REAL T_FCPM_air            ! Temperature of air entering FCPM.
      REAL deltaH_FCPM_air       ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of air entering FCPM
      REAL deltaH_FCPM_fuel      ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of fuel entering FCPM
      REAL Shomate               ! Function that applies Shomate equation to calculate gas properties.

C Variables for liquid water supply:
      REAL Ndot_FCPM_liqwater    ! Supply rate of liquid water to FCPM (kmol/s).
      REAL T_pump_in             ! Temperature of liquid water at pump inlet (oC).
      REAL pump_Pel              ! Elec power draw of water pump (W).
      SAVE pump_Pel              ! Variable saved as calculation of DC power flowing into PCU is iterative.
      REAL pump_q_loss           ! Heat loss from water pump to containing room (W).
      REAL deltaH_FCPM_liqwater  ! Total enthalpy (relative to standard state) flow rate
                                 ! of liquid water entering FCPM (W).
      REAL T_FCPM_liqwater       ! Temperature of liquid water entering FCPM (oC).
      REAL H_latent_heat         ! Product of liquid water flow rate and latent heat of vapourization
                                 ! of water at standard state (W).
      REAL cphat_liqwater        ! Heat capacity of liquid water enterging FCPM (J/kmolK).
      REAL T_FCPM_fuel           ! Temperature of fuel entering FCPM (oC).
      REAL T_std_state           ! Temperature at standard reference state (oC).

C Variables for modelling fuel compressor:
      REAL cphat_fuel            ! Heat capacity of fuel entering compressor (J/kmolK).
      REAL T_comp_in             ! Temp of fuel at compressor inlet (oC).
      REAL comp_Pel              ! Elec power draw of fuel compressor (W).
      SAVE comp_Pel              ! Variable saved as calculation of DC power flowing into PCU is iterative.
      REAL comp_q_loss           ! Heat loss from fuel compressor to containing room (W).

C Variables for modelling air supply blower:
      REAL cphat_air             ! Heat capacity of air entering blower (J/kmolK).
      REAL T_blower_in           ! Temp of air at blower inlet (oC).
      REAL blower_Pel            ! Elec power draw of air supply blower (W).
      SAVE blower_Pel            ! Variable saved as calculation of DC power flowing into PCU is iterative.
      REAL blower_q_loss         ! Heat loss from air supply blower to containing room (W).
      REAL q_recover_to_air      ! Heat losses from auxiliary burner, battery, and PCU 
                                 ! that are recovered to heat the air at the FCPM inlet (W).

C Variables for FCPM exhaust gases:
      REAL Ndot_FCPMexh_N2       ! Molar flow rate of N2 in FCPM exhaust.
      REAL Ndot_FCPMexh_Ar       ! Molar flow rate of Ar in FCPM exhaust.
      REAL Ndot_FCPMexh_O2       ! Molar flow rate of O2 in FCPM exhaust.
      REAL Ndot_FCPMexh_CO2      ! Molar flow rate of CO2 in FCPM exhaust.
      REAL Ndot_FCPMexh_H2O      ! Molar flow rate of H2O in FCPM exhaust.
      REAL deltaH_FCPM_exh       ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of gases exiting FCPM.
      REAL C_FCPM                ! Heat capacity rate of FCPM exhaust gases (W/K).
      REAL T_FCPM_exh_guess

C Variables used to calculate skin losses:
      REAL FCPM_skin_loss        ! Skin losses from FCPM (W).
      REAL T_FCPM_exh            ! Temp of product gases exiting FCPM (oC).
      REAL T_room                ! Temp of air in room containing SOFC-cogen device (oC).

C Variables use to inject heat loss from FC-cogen device to room.
      REAL heat_to_room          ! Heat transfer to room (W).
      REAL heat_to_room_conv     ! Convective portion of heat transfer (W).
      REAL heat_to_room_rad      ! Radiative portion of heat transfer (W).

C Variables use in modelling auxiliary burner:
      REAL Q_auxburn             ! Heat output from auxiliary burner (W).
      REAL Ndot_auxburn_fuel     ! Flow rate of fuel supplied to auxiliary burner (kmol/s).
      REAL T_auxburn_fuel        ! Temperature of fuel entering auxiliary burner (oC).
      REAL deltaH_auxburn_fuel   ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of fuel entering auxiliary burner.
      REAL Ndot_auxburn_H2       ! Flow rate of fuel constituent to auxiliary burner (kmol/s).
      REAL Ndot_auxburn_CH4      ! Ditto.
      REAL Ndot_auxburn_C2H6     ! Ditto.
      REAL Ndot_auxburn_C3H8     ! Ditto.
      REAL Ndot_auxburn_C4H10    ! Ditto.
      REAL Ndot_auxburn_C5H12    ! Ditto.
      REAL Ndot_auxburn_C6H14    ! Ditto.
      REAL Ndot_auxburn_CH3OH    ! Ditto.
      REAL Ndot_auxburn_C2H5OH   ! Ditto.
      REAL Ndot_auxburn_O2_stoich ! Flow rate of O2 required for stoichiometric combustion
                                  ! in auxiliary burner (kmol/s).
      REAL Ndot_auxburn_air_stoich ! Flow rate of air required for stoichiometric combustion
                                   ! in auxiliary burner (kmol/s).
      REAL Ndot_auxburn_air      ! Flow rate of air to auxiliary burner (includes excess air, kmol/s).
      REAL T_auxburn_air         ! Temperature of air entering auxiliary burner (oC).
      REAL deltaH_auxburn_air    ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of air entering auxiliary burner.
      REAL Ndot_auxburnexh_N2    ! Molar flow rate of N2 in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_Ar    ! Molar flow rate of Ar in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_O2    ! Molar flow rate of O2 in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_CO2   ! Molar flow rate of CO2 in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_H2O   ! Molar flow rate of H2O in auxiliary burner exhaust.
      REAL deltaH_auxburn_exh    ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of gases exiting auxiliary burner.
      REAL auxburn_Pel           ! Elec power draw of auxiliary burner ancillaires (W).
      SAVE auxburn_Pel           ! Variable saved as calculation of DC power flowing into PCU is iterative.
      REAL auxburn_skin_loss     ! Skin losses from auxiliary burner (W). This value is saved
      SAVE auxburn_skin_loss     ! since its value is required on the plant iteration following its
                                 ! calculation. 
      REAL T_auxburn_exh         ! Temp of auxiliary burner exhaust (oC).
      REAL C_auxburn             ! Heat capacity reate of aux burner exhaust gases (W/K).
      REAL T_auxburn_exh_guess

C Variables used in modelling gas-to-water heat exchanger:
      REAL T_HX_gas_in           ! Temp of gases entering gas-to-water heat exchanger (oC).
      REAL C_gas                 ! Heat capacity rate (W/K) of gas stream entering HX.
      REAL C_water               ! Heat capacity rate (W/K) of water stream entering HX.
      REAL C_min                 ! Minimim heat capacity flow rate of gas and water streams (W/K).
      REAL SHTFLD                ! Function to calculate heat capacity of water.
      REAL T_HX_water_in         ! Temp of water entering HX (oC).
      REAL Q_HX                  ! Heat recovery to water in gas-to-water HX (W).
      REAL Q_HX_sensible         ! Sensible portion of heat recovery (W).
      REAL Q_HX_latent           ! Latent portion of heat recovery (W).
      REAL Mdot_HX_liqwater      ! Mass flow rate of liquid water through plant side of HX (kg/s).
      REAL Ndot_HX_liqwater      ! Molar flow rate of liquid water through plant side of HX (kmol/s).
      REAL Ndot_HX_gas           ! Molar flow rate of all exhaust gases through HX (kmol/s).
      REAL UA_HX                 ! Effective UA value of HX (W/K).
      REAL T_HX_water_out        ! Temp of water exiting HX (oC).
      REAL T_HX_gas_out          ! Temp of gas exiting HX (oC).
      REAL HX_h_gas              ! Heat transfer coeff on gas side of HX (W/m2K).
      REAL HX_h_water            ! Heat transfer coeff on water side of HX (W/m2K).
      REAL HX_water_vap_frac     ! Fraction of water vapour in gas stream entering HX (-).
      REAL HX_Ndot_condense      ! Rate of condensation of water from gas in HX (kmol/s).
      REAL HX_Hfg                ! Molar heat of vapourization of water (J/kmol).

C Variables related to modelling PEM stack cooling:
      REAL Ndot_s_cool_in_cp     ! Heat capacity rate (W/K) water stream entering stack cooling HX
      REAL Ndot_s_cool_out_cp    ! Heat capacity rate (W/K) water stream leaving stack cooling HX
      REAL Ndot_cogen_cp         ! Heat capacity rate (W/K) water stream entering cogen HX of stack cooling system
      REAL alpha_s_cool_HX       ! Coefficient of stack cooling HX
      REAL h_s_cogen             ! Flow dependent film heat transfer coefficient in cogen HX of stack cooling system 
      REAL alpha_cogen_HX        ! Coefficient of cogen HX of stack cooling system
      REAL beta_cogen_HX         ! ditto
      REAL T_21,T_22,T_23,T_25   ! Temperatures in stack cooling circuit.
      REAL T_23_nobypass,        ! Intermediate quantities for modelling stack cooling circuit.
     &     T_24_required
      REAL q_s_cool              ! Heat to be extracted for stack cooling 
      REAL q_s_cogen             ! Heat used for cogen from stack cooling
      REAL q_s_cool_air          ! Heat to be evacuated by air cooler
      REAL P_air_el              ! Electrical power consumption of air cooler fan 
      SAVE P_air_el
      REAL cp_21,cp_22,cp_25     ! Heat capacities at state points 21, 22 and 25

C Variables used in modelling diluation air system/HRV:
      REAL C_HX_gas_out          ! Heat capacity rate of HX exhaust gases (W/K).
      REAL C_dilution_exh        ! Heat capacity rate of dilution air system exhaust gases (W/K).
      REAL T_dilution_exh_guess  ! Temperature of dilution air system exhaust gases at previous
                                 ! solver iteration (oC).
      REAL T_dilution_air_in     ! Temperature of incoming dilution air (oC).
      REAL deltaH_HX_gas_out     ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of gases exiting HX and entering dilution air system.
      REAL deltaH_dilution_exh   ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of gases exiting dilution air system.
      REAL deltaH_dilution_in    ! Total enthalpy (relative to standard state) flow rate (W)
                                 ! of air entering dilution air system.
      REAL Ndot_dilution_N2      ! Molar flow rate of N2 in auxiliary burner exhaust.
      REAL Ndot_dilution_Ar      ! Molar flow rate of Ar in auxiliary burner exhaust.
      REAL Ndot_dilution_O2      ! Molar flow rate of O2 in auxiliary burner exhaust.
      REAL Ndot_dilution_CO2     ! Molar flow rate of CO2 in auxiliary burner exhaust.
      REAL Ndot_dilution_H2O     ! Molar flow rate of H2O in auxiliary burner exhaust.
      REAL T_fresh_air           ! Temperature of fresh air supply to HRV (oC).
      REAL C_HRV_fresh_air       ! Heat capacity rate of fresh air supply to HRV (W/K).
      REAL C_HRV_min             ! Min heat capacity rate of two streams flowing through HRV (W/K).
      REAL q_HRV                 ! Heat recovered in HRV (W).
      REAL T_vent_air            ! Temp of fresh air exiting HRV (oC).

C Variables used to communicate results to electric domain:
      INTEGER IEMODEL            ! Power calculation flag for electrical domain.
      REAL PQ                    ! Complex power (W).
      REAL PA                    ! Overall power (W).

C Variables calculated strictly for user output purposes.
      REAL LHV_to_AC_eff         ! Net AC output / fuel LHV input efficiency (-).
      REAL LHV_to_therm_eff      ! Useful thermal output / fuel LHV input efficiency (-).
      REAL LHV_to_cogen_eff      ! LHV_to_AC_eff + LHV_to_therm_eff (-).


C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Entering subroutine SOFC_A42_coeffgen'
      END IF


C---------------------------------------------------------------------------------
C Set local variables to point to the nodes and to the connection to node 6.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C                   within the global plant matrix.
C    CV(i)          global matrix node number for node 'i'
C    con_CV6        connection number to node 6 (water in heat exchanger)
C    con_CV14       connection number to node 14 (water in heat exchanger stack cooling system)
C---------------------------------------------------------------------------------
      CV1     = NPCDAT(IPCOMP,9)
      CV2     = NPCDAT(IPCOMP,9)+1
      CV3     = NPCDAT(IPCOMP,9)+2
      CV4     = NPCDAT(IPCOMP,9)+3
      CV5     = NPCDAT(IPCOMP,9)+4
      CV6     = NPCDAT(IPCOMP,9)+5
      CV7     = NPCDAT(IPCOMP,9)+6
      CV8     = NPCDAT(IPCOMP,9)+7
      CV9     = NPCDAT(IPCOMP,9)+8
      CV10    = NPCDAT(IPCOMP,9)+9
      CV11    = NPCDAT(IPCOMP,9)+10
      CV12    = NPCDAT(IPCOMP,9)+11
      CV13    = NPCDAT(IPCOMP,9)+12
      CV14    = NPCDAT(IPCOMP,9)+13

      con_CV6 = ICONDX(IPCOMP,6,1)
      con_CV14= ICONDX(IPCOMP,14,1)


C---------------------------------------------------------------------------------
C Mark all temperatures for iteration.  Future testing will indicate which CVs
C are critical in terms of marking for iteration and thus some CVs may be
C removed from the list below.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C---------------------------------------------------------------------------------
      ICSV(CV1,1) = 1
      CSVI(CV1,1) = CSVF(CV1,1)
      ICSV(CV2,1) = 1
      CSVI(CV2,1) = CSVF(CV2,1)
      ICSV(CV3,1) = 1
      CSVI(CV3,1) = CSVF(CV3,1)
      ICSV(CV4,1) = 1
      CSVI(CV4,1) = CSVF(CV4,1)
      ICSV(CV5,1) = 1
      CSVI(CV5,1) = CSVF(CV5,1)
      ICSV(CV6,1) = 1
      CSVI(CV6,1) = CSVF(CV6,1)
      ICSV(CV7,1) = 1
      CSVI(CV7,1) = CSVF(CV7,1)
      ICSV(CV8,1) = 1
      CSVI(CV8,1) = CSVF(CV8,1)
      ICSV(CV9,1) = 1
      CSVI(CV9,1) = CSVF(CV9,1)
      ICSV(CV10,1) = 1
      CSVI(CV10,1) = CSVF(CV10,1)
      ICSV(CV11,1) = 1
      CSVI(CV11,1) = CSVF(CV11,1)
      ICSV(CV12,1) = 1
      CSVI(CV12,1) = CSVF(CV12,1)
      ICSV(CV13,1) = 1
      CSVI(CV13,1) = CSVF(CV13,1)
      ICSV(CV14,1) = 1
      CSVI(CV14,1) = CSVF(CV14,1)


C---------------------------------------------------------------------------------
C Mark the heat recovery from hot gases to the water for iteration.  Also mark
C the temperature of node 4 for iteration (for an absolute change in temperature)
C as well as 'deltaH_FCPM_exh'.  Marking the latter two was found to avoid
C possible oscillations in the solution of node 4's temperature.
C Variables used:
C    iPlt_Output_Iter_Flag(i,j) flag denoting that additional data item `j' for
C                               component `i' is marked for iteration.
C    fPlt_Output_Init_Val(i,j)  previous iteration's value for additional data
C                               item `j' for node `i'.
C    fPlt_Output_Tol(i,j)       tolerance for convergence for additional data
C                               item `j' for node `i'.
C---------------------------------------------------------------------------------
C-----Heat recovery to water.
      iPlt_Output_Iter_Flag(IPCOMP,2) = 1
      fPlt_Output_Init_Val(IPCOMP,2)  = PCDATF(IPCOMP,2)
      fPlt_Output_Tol(IPCOMP,2)       = 1. ! 1 W tolerance.
C-----Temperature of CV4.
      iPlt_Output_Iter_Flag(IPCOMP,3) = 1
      fPlt_Output_Init_Val(IPCOMP,3)  = PCDATF(IPCOMP,3)
      fPlt_Output_Tol(IPCOMP,3)       = 1. ! 1 oC tolerance.
C-----Enthalpy flow rate of gases existing CV4.
      iPlt_Output_Iter_Flag(IPCOMP,4) = 1
      fPlt_Output_Init_Val(IPCOMP,4)  = PCDATF(IPCOMP,4)
      fPlt_Output_Tol(IPCOMP,4)       = 5. ! tolerance in W.


C---------------------------------------------------------------------------------
C Determine how the SOFC-cogeneration device is operating this time-step.
C CDATA(IPCOMP,1) is the control signal (coming from an external controller) that
C indicates the fraction of full load power requested.  The external controller
C is instructing the SOFC-cogeneration device to shut down when this signal is
C equal to -999. `TIMSEC' is the length of the simulation time-step (seconds).
C Operating modes (`sys_op'):
C    COLD            Device previously shutdown and is still turned off.
C                    Must move through STARTING_UP sequence before operating
C                    in NORMAL mode.
C    SHUTTING_DOWN   Device is going through controlled cool-down which
C                    lasts for `FCPM_dt_cooldown' seconds.
C    STARTING_UP     Device is going through controlled start-up which lasts
C                    for `FCPM_dt_startup' seconds.
C    NORMAL          The device has previously passed through the STARTING_UP
C                    sequence and is producing electricity and heat. This is the
C                    common operating model.
C---------------------------------------------------------------------------------
C-----Initialize counters and reference points for first pass through coeff generator.
      IF (NSINC==1) THEN
        sys_op_last_dt   = NORMAL  ! Unit assumed to be on initially.
        initiate_startup = 1       ! NSINC at which start-up initiated.
        initiate_startup = 1       ! NSINC at which shutdown intiiated.
        OFFON_count      = 0.      ! Unit has never been cycled off.
        OPER_TIME_HRS    = 0.
      ENDIF

C-----Get state of operation at previous time-step.
      IF (NSINC/=1) THEN
        sys_op_last_dt = PCDATP(IPCOMP,5)
        OFFON_count    = PCDATP(IPCOMP,7)
      ENDIF

C-----Get signal from external controller.
      CALL ECLOSE( CDATA(IPCOMP,1),-999.,0.1,CLOSE)

C-----The external controller is requesting a system shutdown.
      IF (CLOSE) THEN
        IF ( sys_op_last_dt == COLD ) THEN
C---------Unit was previously shutdown and will remain turned off.
          sys_op = COLD
        ELSEIF ( sys_op_last_dt == SHUTTING_DOWN ) THEN
C---------Shutdown was in progress last time-step. Switch off if shutdown procedure
C---------has completed; otherwise continue shutdown procedure.
          how_long_shutting_down =
     &                        FLOAT(NSINC - initiate_shutdown) * TIMSEC
          IF ( how_long_shutting_down > FCPM_dt_cooldown ) THEN
            sys_op = COLD
          ELSE
            sys_op = SHUTTING_DOWN
          ENDIF
        ELSEIF ( sys_op_last_dt == NORMAL ) THEN
C---------Unit was operating last time-step, so initiate shutdown procedure.
          sys_op = SHUTTING_DOWN
          initiate_shutdown = NSINC
        ELSEIF ( sys_op_last_dt == STARTING_UP ) THEN
C---------Unit is starting-up. Shutdown cannot be initiated until start-up completed.
          how_long_starting_up =
     &                          FLOAT(NSINC - initiate_startup) * TIMSEC
          IF ( how_long_starting_up > FCPM_dt_startup ) THEN
            sys_op = SHUTTING_DOWN
          ELSE
            sys_op = STARTING_UP
          ENDIF
        ELSE
C---------Error trap.
          WRITE(IUOUT,*) ' Impossible operating sequence [a]'
          STOP ' SOFC_A42_coeffgen: unresolvable error'
        ENDIF

C-----The external controller is requesting electrical power.
      ELSE
        IF ( sys_op_last_dt == COLD ) THEN
C---------Unit is turned off so initiate start-up procedure.
          sys_op = STARTING_UP
          initiate_startup = NSINC
          OFFON_count = PCDATP(IPCOMP,7) + 1.  ! Performance degrades with each off/on cycle.
        ELSEIF ( sys_op_last_dt == SHUTTING_DOWN ) THEN
C---------Unit is shutting down. Start-up cannot be initiated until shutdown completed.
          how_long_shutting_down =
     &                         FLOAT(NSINC - initiate_shutdown) * TIMSEC
          IF ( how_long_shutting_down > FCPM_dt_cooldown ) THEN
            sys_op = STARTING_UP
            initiate_startup = NSINC
            OFFON_count = PCDATP(IPCOMP,7) + 1.  ! Performance degrades with each off/on cycle.
          ELSE
            sys_op = SHUTTING_DOWN
          ENDIF
        ELSEIF ( sys_op_last_dt == NORMAL ) THEN
C---------Unit was operating last time-step: continue to operate.
          sys_op = NORMAL
        ELSEIF ( sys_op_last_dt == STARTING_UP ) THEN
C---------Start-up was in progress last time-step. Switch to normal operation if start-up
C---------procedure-has completed; otherwise continue start-up procedure.
          how_long_starting_up =
     &                          FLOAT(NSINC - initiate_startup) * TIMSEC
          IF ( how_long_starting_up > FCPM_dt_startup ) THEN
            sys_op = NORMAL
          ELSE
            sys_op = STARTING_UP
          ENDIF
        ELSE
C---------Error trap.
          WRITE(IUOUT,*) ' Impossible operating sequence [b]'
          STOP ' SOFC_A42_coeffgen: unresolvable error'
        ENDIF

      ENDIF

C-----Increment counter holding total operating time since initial start-up
C-----if system is operating normally, starting up, or shutting down this time-step.
C-----Discount operating time during simulation start-up period (identified by
C-----FUNCTION bInStartup()=.true.).
      IF ( sys_op==NORMAL .or. sys_op==STARTING_UP
     &     .or. sys_op==SHUTTING_DOWN ) THEN
        IF ( .NOT. bInStartup() ) THEN
          OPER_TIME_HRS = PCDATP(IPCOMP,8) + TIMSEC/3600.
        END IF
      ELSE
        OPER_TIME_HRS = PCDATP(IPCOMP,8)  ! Not really required.
      ENDIF


C*********************************************************************************
C*********************************************************************************
C Establish the energy or mass balances and set the matrix coefficients
C accordingly.  Separate blocks of code are used to establish the energy balances
C depending upon the mode of operation.
C*********************************************************************************
C*********************************************************************************

C*********************************************************************************
C*********************************************************************************
C Beginning of energy balances for NORMAL operation.
C*********************************************************************************
C*********************************************************************************
      IF(ISTATS==1 .and. sys_op==NORMAL) THEN

C==============================================================
C OPERATING POINT, BATTERY, PCU, AND FCPM ELECTRICAL EFFICIENCY
C==============================================================

C---------------------------------------------------------------------------------
C Determine the AC power output demanded from the SOFC-cogeneration device.
C CDATA(IPCOMP,1) is the control signal (coming from an external controller) that
C indicates the fraction of full load power requested.
C---------------------------------------------------------------------------------
        AC_demand_net = FCPM_elec_max * CDATA(IPCOMP,1)  ! W


C---------------------------------------------------------------------------------
C Estimate the DC power output required from the FCPM and/or battery to meet
C this net AC demand. This is the DC power that must flow into the PCU in order to
C produce sufficient AC power to operate the FC-cogen unit's AC ancillaries and to
C satisfy the net AC demand. The PCU losses at the last iteration are used.
C Additionally, the AC power requirements of the ancillaries determined at the
C last itereation are used. When the FCPM and battery can supply the DC power
C necessary to produce the net AC demand, then `DC_required_by_PCU' should equal
C `DC_into_PCU' as the solution iterates within the time-step.
C Refer to section 5.3 and section 13 of the model spec.
C---------------------------------------------------------------------------------
C-------Estimate the gross AC power that must be produced by the PCU.
        AC_required_from_PCU = AC_demand_net + Pel_ancAC + blower_Pel
     &                         + comp_Pel + pump_Pel + auxburn_Pel 
     &                         + P_air_el + s_cool_pump_Pel
     &                         + Dilution_fanpower + HRV_fanpower
C-------Update estimate of DC power that has to flow into PCU to meet net AC demand.
        DC_required_by_PCU = AC_required_from_PCU + PCU_losses


C---------------------------------------------------------------------------------
C Determine the FCPM's operating point (Pel). The FCPM attempts to supply the DC
C power that is required by the PCU in order to meet the AC demand. However,
C changes in the operating point from the previous time-step are constrained by
C the FCPM's transient response characteristics. Refer to section 5.4 of the model
C spec. The FCPM is also constrained by user-specified operating range. Refer to
C section 5.3 of the model spec.
C---------------------------------------------------------------------------------
C-------Transient response constraints.
        IF( DC_required_by_PCU == PCDATP(IPCOMP,1) ) THEN
C---------Demand on FCPM is same as output last time-step: no change in operating point.
          Pel = DC_required_by_PCU
        ELSEIF( DC_required_by_PCU > PCDATP(IPCOMP,1) ) THEN
C---------More power demanded than was output last time-step: ramp up FCPM.
          IF( (DC_required_by_PCU-PCDATP(IPCOMP,1))/TIMSEC >
     &        FCPM_dPdt_inc_limit ) THEN   ! Can't ramp up this quickly.
            Pel = PCDATP(IPCOMP,1) + FCPM_dPdt_inc_limit*TIMSEC
          ELSE  ! FCPM can ramp to meet demand.
            Pel = DC_required_by_PCU
          ENDIF
        ELSE
C---------Less power demanded than was output last time-step: ramp down FCPM.
          IF( (PCDATP(IPCOMP,1)-DC_required_by_PCU)/TIMSEC >
     &        FCPM_dPdt_dec_limit ) THEN  ! Can't ramp down this quickly.
            Pel = PCDATP(IPCOMP,1) - FCPM_dPdt_dec_limit*TIMSEC
          ELSE  ! FCPM can ramp.
            Pel = DC_required_by_PCU
          ENDIF
        ENDIF
C-------Operating range constraints.
        IF ( Pel < FCPM_elec_min ) Pel = FCPM_elec_min
        IF ( Pel > FCPM_elec_max ) Pel = FCPM_elec_max


C---------------------------------------------------------------------------------
C Charge the battery if the FCPM's operating point results in a surplus of power.
C If the FCPM's operating point results in a deficit of power then draw power from
C the battery. The battery is subject to operating constraints. Refer to section
C 12 of the model spec.  PCDATP(IPCOMP,6) represents the battery's SOC (fraction
C of energy capacity) at the end of the last time-step/beginning of the current
C time-step. Power drawn from the battery is treated as a positive flow and power
C sent to charge the battery is treated as a negative flow. 
C---------------------------------------------------------------------------------
C-------Battery's SOC at beginning of time-step. Initialize to user-input
C-------if this is first time-step of simulation.
        IF (NSINC == 1) THEN
          battery_SOC_p = battery_SOC_initial
        ELSE
          battery_SOC_p = PCDATP(IPCOMP,6)
        ENDIF

C-------Charge or draw from battery.
        IF ( Pel > DC_required_by_PCU ) THEN
C---------Attemp to charge. Power sent to battery cannot exceed limit.
          P_battery = -1.* (Pel - DC_required_by_PCU)
          IF( -1.*P_battery > battery_max_charge_rate )
     &                           P_battery = -1.*battery_max_charge_rate
C---------Prevent over-charging of battery.
          P_fully_charge = -1.*(1.-battery_SOC_p) * battery_capacity
     &                     / ( TIMSEC*battery_charge_eff )
          IF( -1.*P_battery > -1.*P_fully_charge )
     &                                        P_battery = P_fully_charge
C---------Determine SOC at end of time-step.
          battery_SOC_f = ( battery_SOC_p*battery_capacity
     &                      + (-1.)*P_battery*TIMSEC*battery_charge_eff
     &                    ) / battery_capacity
C---------Determine heat generated due to charging losses.
          q_battery_loss = -1. * P_battery * (1. - battery_charge_eff)

        ELSEIF ( Pel < DC_required_by_PCU ) THEN
C---------Attempt to supply deficit of power from battery. Power draw cannot exceed limit.
          P_battery = DC_required_by_PCU - Pel
          IF( P_battery > battery_max_discharge_rate )
     &                            P_battery = battery_max_discharge_rate
C---------Prevent complete depletion of battery.
          P_fully_discharge = battery_SOC_p * battery_capacity
     &                        * battery_discharge_eff / TIMSEC
          IF( P_battery > P_fully_discharge )
     &                                     P_battery = P_fully_discharge
C---------Determine SOC at end of time-step.
          battery_SOC_f = ( battery_SOC_p*battery_capacity
     &                      - P_battery*TIMSEC/battery_discharge_eff
     &                    ) / battery_capacity
C---------Determine heat generated due to discharging losses.
          q_battery_loss = P_battery * (1. - battery_discharge_eff)

        ELSE
C---------No interaction with battery.
          P_battery      = 0.
          battery_SOC_f  = battery_SOC_p
          q_battery_loss = 0.

        ENDIF


C---------------------------------------------------------------------------------
C Now that FCPM and battery operating points have been established, calculate the
C DC power flowing into the PCU and the AC power flowing out of the PCU. The net
C AC production of the FC-cogen device will be calculated later following the
C evaluation of the AC power draws of the ancillaries.
C---------------------------------------------------------------------------------
        DC_into_PCU = Pel + P_battery
        eta_PCU    = PCU_u0 + PCU_u1 * DC_into_PCU
     &               + PCU_u2 * DC_into_PCU**2.
        PCU_losses = (1.-eta_PCU) * DC_into_PCU
        AC_production_gross = DC_into_PCU - PCU_losses


C---------------------------------------------------------------------------------
C An operating point of Pel=0 can lead to division by zero errors later in the
C coefficient generator.  Such an operating point is nonsensical for the 'NORMAL'
C mode of operation, but can occur for NSINC=1.  However, if the user can given
C a realistic value for 'FCPM_elec_min', 'Pel' will have been set to this value
C above.  The following code is used to protect subsequent calculation for the
C specific case of NSINC=1 and when FCPM_elec_min=0.
C---------------------------------------------------------------------------------
        IF( NSINC==1 .and. Pel<1. ) THEN
          WRITE(IUOUT,*) ' SOFC_A42_coeffgen: Resetting Pel'
          WRITE(IUOUT,*) ' to 100 W at NSINC=',NSINC
          Pel = 100.
        ENDIF


C---------------------------------------------------------------------------------
C Calculate the electrical efficiency (equation 8).
C---------------------------------------------------------------------------------
        eta_el = ( FCPM_eta_0
     &             + FCPM_eta_1*Pel
     &             + FCPM_eta_2*Pel**2. ) *
     &           ( 1. - OFFON_count*FCPM_degrade_ONOFF ) *
     &           ( 1. - MAX( OPER_TIME_HRS-FCPM_degrade_thresh , 0. )
     &                  *FCPM_degrade_optime )
        IF ( eta_el <= 0. .or. eta_el > 1. ) THEN
          WRITE(IUOUT,*) ' SOFC_A42_coeffgen: FC elec eff =',eta_el
          WRITE(IUOUT,*) ' This is unrealistic. Adjust coeffs.' 
          STOP ' SOFC_A42_coeffgen: unresolvable error'
        ENDIF


C==================================
C FUEL COMPRESSOR AND INLET TO FCPM
C==================================

C---------------------------------------------------------------------------------
C Determine the flow rate of fuel supplied to the FCPM in terms of kmol/s
C (equation 10).
C---------------------------------------------------------------------------------
        Ndot_FCPM_fuel = Pel / eta_el / LHV_fuel


C---------------------------------------------------------------------------------
C Establish the temperature of the fuel entering the fuel compressor. The fuel
C can either be drawn from the room that contains the SOFC-cogen device or from
C outdoors.
C Variables used:
C    PCNTMF(i) is the future time-row temperature of room containing
c              component `i'; =-99 if no containment defined.
C    TF is the future value of outdoor temperature in oC.
C---------------------------------------------------------------------------------
        SELECT CASE (Compressor_source_fuel)
        CASE (fuel_from_room)
          CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
          IF(CLOSE)THEN
            WRITE(IUOUT,*) ' No containment has been defined for the '
            WRITE(IUOUT,*) ' SOFC-cogen plant component. Therefore, '
            WRITE(IUOUT,*) ' the fuel cannot be drawn at room T. '
            STOP ' SOFC_A42_coeffgen: unresolvable error'
          ELSE
            T_comp_in = PCNTMF(IPCOMP)
          END IF

        CASE (fuel_from_outdoors)
          T_comp_in = TF

        CASE DEFAULT
          WRITE(IUOUT,*) ' The fuel must be drawn either at room T '
          WRITE(IUOUT,*) ' or the outdoor T.'
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C---------------------------------------------------------------------------------
C Determine the heat capacity of the fuel entering the fuel compressor.
C---------------------------------------------------------------------------------
        cphat_fuel =
     &       chi_fuel_H2     * Shomate(heatcap,T_comp_in,H2,IUOUT)
     &     + chi_fuel_CH4    * Shomate(heatcap,T_comp_in,CH4,IUOUT)
     &     + chi_fuel_C2H6   * Shomate(heatcap,T_comp_in,C2H6,IUOUT)
     &     + chi_fuel_C3H8   * Shomate(heatcap,T_comp_in,C3H8,IUOUT)
     &     + chi_fuel_C4H10  * Shomate(heatcap,T_comp_in,C4H10,IUOUT)
     &     + chi_fuel_C5H12  * Shomate(heatcap,T_comp_in,C5H12,IUOUT)
     &     + chi_fuel_C6H14  * Shomate(heatcap,T_comp_in,C6H14,IUOUT)
     &     + chi_fuel_CH3OH  * Shomate(heatcap,T_comp_in,CH3OH,IUOUT)
     &     + chi_fuel_C2H5OH * Shomate(heatcap,T_comp_in,C2H5OH,IUOUT)


C---------------------------------------------------------------------------------
C Determine AC elec power draw of fuel compressor (W).
C---------------------------------------------------------------------------------
        comp_Pel = compressor_0
     &           + compressor_1*Ndot_FCPM_fuel
     &           + compressor_2*Ndot_FCPM_fuel**2.
     &           + compressor_3*Ndot_FCPM_fuel**3.


C---------------------------------------------------------------------------------
C Calculate the heat loss from the compressor to the containing room (W). This
C will be added as a source term to the energy balance of the containing room.
C---------------------------------------------------------------------------------
        comp_q_loss = Compressor_heat_loss * comp_Pel  


C---------------------------------------------------------------------------------
C Warn user if temperature rise of fuel through compressor is unrealistic.
C---------------------------------------------------------------------------------
        IF( ( ( 1.-Compressor_heat_loss ) * comp_Pel
     &        / ( Ndot_FCPM_fuel*cphat_fuel ) ) > 100. ) THEN
          WRITE(IUOUT,*) ' SOFC_A42_coeffgen: temp rise of fuel in',
     &                   ' compressor is > 100 oC.'
          WRITE(IUOUT,*) ' Examine validatity of input data.'
        END IF


C---------------------------------------------------------------------------------
C Set the temperature of the fuel entering to the FCPM to that calculated for
C the exit of the compressor at the previous iteration. Once the solution
C converges this will equal the temperature of the fuel exiting the compressor
C for the current itereation.
C---------------------------------------------------------------------------------
        T_FCPM_fuel = CSVF(CV2,1)


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C fuel entering the FCPM.
C---------------------------------------------------------------------------------
        deltaH_FCPM_fuel =
     &    chi_fuel_H2*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,H2,IUOUT)
     &  + chi_fuel_CH4*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,CH4,IUOUT)
     &  + chi_fuel_C2H6*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,C2H6,IUOUT)
     &  + chi_fuel_C3H8*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,C3H8,IUOUT)
     &  + chi_fuel_C4H10*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,C4H10,IUOUT)
     &  + chi_fuel_C5H12*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,C5H12,IUOUT)
     &  + chi_fuel_C6H14*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,C6H14,IUOUT)
     &  + chi_fuel_CH3OH*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,CH3OH,IUOUT)
     &  + chi_fuel_C2H5OH*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,C2H5OH,IUOUT)
     &  + chi_fuel_CO2*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,CO2,IUOUT)
     &  + chi_fuel_N2*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,N2,IUOUT)
     &  + chi_fuel_O2*Ndot_FCPM_fuel*
     &    Shomate(deltaH,T_FCPM_fuel,O2,IUOUT)


C=================
C FCPM ANCILLARIES
C=================

C---------------------------------------------------------------------------------
C Calculate the power draw of ancillaries that are included in the FCPM and that
C are powered by AC (equation 19).
C---------------------------------------------------------------------------------
        Pel_ancAC = FCPM_anc_0 + FCPM_anc_1*Ndot_FCPM_fuel


C====================================
C AIR SUPPLY BLOWER AND INLET TO FCPM
C====================================

C---------------------------------------------------------------------------------
C Calculate the stoichiometric air flow requirement. First, determine the
C molar flow rates (kmol/s) of the fuel constituents that react with O2.
C Then determine the stoichiometric air required to react with these fuel
C constituents (equation 15).
C---------------------------------------------------------------------------------
C-------Flow rates of fuel constituents (kmol/s).
        Ndot_FCPM_H2     = chi_fuel_H2     * Ndot_FCPM_fuel
        Ndot_FCPM_CH4    = chi_fuel_CH4    * Ndot_FCPM_fuel
        Ndot_FCPM_C2H6   = chi_fuel_C2H6   * Ndot_FCPM_fuel
        Ndot_FCPM_C3H8   = chi_fuel_C3H8   * Ndot_FCPM_fuel
        Ndot_FCPM_C4H10  = chi_fuel_C4H10  * Ndot_FCPM_fuel
        Ndot_FCPM_C5H12  = chi_fuel_C5H12  * Ndot_FCPM_fuel
        Ndot_FCPM_C6H14  = chi_fuel_C6H14  * Ndot_FCPM_fuel
        Ndot_FCPM_CH3OH  = chi_fuel_CH3OH  * Ndot_FCPM_fuel
        Ndot_FCPM_C2H5OH = chi_fuel_C2H5OH * Ndot_FCPM_fuel
C-------Stoichiometric O2 flow rate required for each fuel constituent (kmol/s).
        Ndot_FCPM_O2_stoich = 0.5 * Ndot_FCPM_H2
     &                      + 2.  * Ndot_FCPM_CH4
     &                      + 3.5 * Ndot_FCPM_C2H6
     &                      + 5.  * Ndot_FCPM_C3H8
     &                      + 6.5 * Ndot_FCPM_C4H10
     &                      + 8.  * Ndot_FCPM_C5H12
     &                      + 9.5 * Ndot_FCPM_C6H14
     &                      + 1.5 * Ndot_FCPM_CH3OH
     &                      + 3.  * Ndot_FCPM_C2H5OH
C-------Stoichiometric air flow rate (kmol/s).
        Ndot_FCPM_air_stoich = Ndot_FCPM_O2_stoich / chi_air_O2


C---------------------------------------------------------------------------------
C Establish the temperature of the air entering the air supply blower. The air
C can either be drawn from the room that contains the SOFC-cogen device or from
C outdoors.
C Variables used:
C    PCNTMF(i) is the future time-row temperature of room containing
c              component `i'; =-99 if no containment defined.
C    TF is the future value of outdoor temperature in oC.
C---------------------------------------------------------------------------------
        SELECT CASE (Blower_source_air)
        CASE (air_from_room)
          CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
          IF(CLOSE)THEN
            WRITE(IUOUT,*) ' No containment has been defined for the '
            WRITE(IUOUT,*) ' SOFC-cogen plant component. Therefore, '
            WRITE(IUOUT,*) ' the air cannot be drawn at room T. '
            STOP ' SOFC_A42_coeffgen: unresolvable error'
          ELSE
            T_blower_in = PCNTMF(IPCOMP)
          END IF

        CASE (air_from_outdoors)
          T_blower_in = TF

        CASE DEFAULT
          WRITE(IUOUT,*) ' The air must be drawn either at room T '
          WRITE(IUOUT,*) ' or the outdoor T.'
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C---------------------------------------------------------------------------------
C Set the temperature of the air entering to the FCPM to that calculated for
C the exit of the blower at the previous iteration. Once the solution
C converges this will equal the temperature of the air exiting the blower
C for the current itereation.
C---------------------------------------------------------------------------------
        T_FCPM_air = CSVF(CV1,1)


C---------------------------------------------------------------------------------
C Calculate the air supply rate to the FCPM and the excess air ratio
C (section 5.6).
C---------------------------------------------------------------------------------
        SELECT CASE (FCPM_air_supply_method) 

        CASE (const_excess_air_ratio)
          lambda_FCPM = FCPM_air_0
          Ndot_FCPM_air = (1.+lambda_FCPM) * Ndot_FCPM_air_stoich

        CASE (air_f_elec_output)
          Ndot_FCPM_air = ( FCPM_air_0 + FCPM_air_1*Pel
     &                      + FCPM_air_2*Pel**2. )
     &                  * ( 1. + FCPM_air_3*T_FCPM_air )
          lambda_FCPM = Ndot_FCPM_air / Ndot_FCPM_air_stoich - 1.

        CASE (air_f_fuel_flow)
          Ndot_FCPM_air = ( FCPM_air_0 + FCPM_air_1*Ndot_FCPM_fuel
     &                      + FCPM_air_2*Ndot_FCPM_fuel**2. )
     &                  * ( 1. + FCPM_air_3*T_FCPM_air )
          lambda_FCPM = Ndot_FCPM_air / Ndot_FCPM_air_stoich - 1.

        END SELECT


C---------------------------------------------------------------------------------
C Determine the heat capacity of the air entering the air supply blower.
C---------------------------------------------------------------------------------
        cphat_air =
     &       chi_air_N2  * Shomate(heatcap,T_blower_in,N2,IUOUT)
     &     + chi_air_O2  * Shomate(heatcap,T_blower_in,O2,IUOUT)
     &     + chi_air_H2O * Shomate(heatcap,T_blower_in,H2O,IUOUT)
     &     + chi_air_Ar  * Shomate(heatcap,T_blower_in,Ar,IUOUT)
     &     + chi_air_CO2 * Shomate(heatcap,T_blower_in,CO2,IUOUT)


C---------------------------------------------------------------------------------
C Determine AC elec power draw of air supply blower (W).
C---------------------------------------------------------------------------------
        blower_Pel = blower_0
     &             + blower_1*Ndot_FCPM_air
     &             + blower_2*Ndot_FCPM_air**2.
     &             + blower_3*Ndot_FCPM_air**3.


C---------------------------------------------------------------------------------
C Calculate the heat loss from the air supply blower to the containing room (W).
C This will be added as a source term to the energy balance of the containing
C room.
C---------------------------------------------------------------------------------
        blower_q_loss = Blower_heat_loss * blower_Pel  


C---------------------------------------------------------------------------------
C Warn user if temperature rise of air through blower is unrealistic. Only
C issue warning if there is an air flow through the blower.
C---------------------------------------------------------------------------------
        CALL ECLOSE(Ndot_FCPM_air, 0., 1.E-12, CLOSE)
        IF(CLOSE)THEN
          IF( ( ( 1.-Blower_heat_loss ) * blower_Pel
     &          / ( Ndot_FCPM_air*cphat_air ) ) > 100. ) THEN
            WRITE(IUOUT,*) ' SOFC_A42_coeffgen: temp rise of air in',
     &                     ' air supply blower is > 100 oC.'
            WRITE(IUOUT,*) ' Examine validatity of input data.'
          END IF
        END IF


C---------------------------------------------------------------------------------
C Calculate heat losses from auxiliary burner, battery, and PCU that are recovered
C to pre-heat the air stream entering the FCPM.
C---------------------------------------------------------------------------------
        q_recover_to_air = 0.
C-------Auxiliary burner.
        IF( Auxburn_heat_to_where==Auxburn_heat_to_air )THEN
          q_recover_to_air = q_recover_to_air + auxburn_skin_loss
        END IF
C-------Battery.
        IF( battery_heat_to_where==battery_heat_to_air )THEN
          q_recover_to_air = q_recover_to_air + q_battery_loss
        END IF
C-------PCU.
        IF( PCU_heat_to_where==PCU_heat_to_air )THEN
          q_recover_to_air = q_recover_to_air + PCU_losses
        ENDIF

C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W) of the air entering the FCPM.
C---------------------------------------------------------------------------------
        deltaH_FCPM_air =
     &    chi_air_N2*Ndot_FCPM_air*Shomate(deltaH,T_FCPM_air,N2,IUOUT)
     &  + chi_air_O2*Ndot_FCPM_air*Shomate(deltaH,T_FCPM_air,O2,IUOUT)
     &  + chi_air_H2O*Ndot_FCPM_air*Shomate(deltaH,T_FCPM_air,H2O,IUOUT)
     &  + chi_air_Ar*Ndot_FCPM_air*Shomate(deltaH,T_FCPM_air,Ar,IUOUT)
     &  + chi_air_CO2*Ndot_FCPM_air*Shomate(deltaH,T_FCPM_air,CO2,IUOUT)


C===========================================
C WATER PUMP AND LIQUID WATER SUPPLY TO FCPM
C===========================================

C---------------------------------------------------------------------------------
C Determine the flow rate of liquid water that is supplied for steam reformation
C in terms of kmol/s (equation 18).
C---------------------------------------------------------------------------------
        Ndot_FCPM_liqwater = FCPM_water_0
     &                     + FCPM_water_1*Ndot_FCPM_fuel
     &                     + FCPM_water_2*Ndot_FCPM_fuel**2.


C---------------------------------------------------------------------------------
C Establish the temperature of the liquid water entering the water pump. The water
C can either be drawn from the room that contains the FC-cogen device or from
C the water mains.
C Variables used:
C    PCNTMF(i) is the future time-row temperature of room containing
c              component `i'; =-99 if no containment defined.
C---------------------------------------------------------------------------------
        SELECT CASE (Pump_source_water)
        CASE (water_from_room)
          CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
          IF(CLOSE)THEN
            WRITE(IUOUT,*) ' No containment has been defined for the '
            WRITE(IUOUT,*) ' SOFC-cogen plant component. Therefore, '
            WRITE(IUOUT,*) ' the water cannot be supplied at room T. '
            STOP ' SOFC_A42_coeffgen: unresolvable error'
          ELSE
            T_pump_in = PCNTMF(IPCOMP)
          END IF

        CASE (water_from_mains)
          CALL EDAYR(idyf,day_mains,month_mains)
          T_pump_in = fDHW_ColdMainTemp(month_mains)

        CASE DEFAULT
          WRITE(IUOUT,*) ' The liq water must be supplied either at '
          WRITE(IUOUT,*) ' room T or water mains T.'
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C---------------------------------------------------------------------------------
C Determine the heat capacity of the liquid water entering the pump.
C---------------------------------------------------------------------------------
        cphat_liqwater = Shomate(heatcap,T_pump_in,liq_H2O,IUOUT)


C---------------------------------------------------------------------------------
C Determine AC elec power draw of water pump (W).
C---------------------------------------------------------------------------------
        pump_Pel = pump_0
     &           + pump_1*Ndot_FCPM_liqwater
     &           + pump_2*Ndot_FCPM_liqwater**2.
     &           + pump_3*Ndot_FCPM_liqwater**3.


C---------------------------------------------------------------------------------
C Calculate the heat loss from the pump to the containing room (W). This
C will be added as a source term to the energy balance of the containing room.
C---------------------------------------------------------------------------------
        pump_q_loss = Pump_heat_loss * pump_Pel  


C---------------------------------------------------------------------------------
C Warn user if temperature rise of fuel through pump is unrealistic.
C---------------------------------------------------------------------------------
        CALL ECLOSE(Ndot_FCPM_liqwater*cphat_liqwater,0.,1.E-12,CLOSE)
        IF (.NOT.CLOSE) THEN
        IF( ( ( 1.-Pump_heat_loss ) * pump_Pel
     &        / ( Ndot_FCPM_liqwater*cphat_liqwater ) ) > 70. ) THEN
          WRITE(IUOUT,*) ' SOFC_A42_coeffgen: temp rise of water in',
     &                   ' pump is > 70 oC.'
          WRITE(IUOUT,*) ' Examine validatity of input data.'
        END IF
        ENDIF


C---------------------------------------------------------------------------------
C Set the temperature of the water entering to the FCPM to that calculated for
C the exit of the water pump at the previous iteration. Once the solution
C converges this will equal the temperature of the water exiting the water pump
C for the current itereation.
C---------------------------------------------------------------------------------
        T_FCPM_liqwater = CSVF(CV3,1)


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
c liquid water entering the FCPM. Refer to equation 6a of the model spec.
C---------------------------------------------------------------------------------
        deltaH_FCPM_liqwater = Ndot_FCPM_liqwater
     &                   * Shomate(deltaH,T_FCPM_liqwater,liq_H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the product of the liquid water flow rate entering the FCPM and the
C latent heat of vapourization of water at the standard state (W). Refer to
c equation 6a of the model spec. The latent heat of vapourization is evaluated
C by subtracting the enthalpy of formation of liquid water from that of water
C vapour, both being evaluated at the standard reference state (25.0 oC). The
C enthalpies of formation are determined by subtracting the enthalpy relative
C to that at the standard state from the enthalpy, yielding the "H" coefficient
C of the Shomate equation. Refer to equation 12 of the model spec.
C---------------------------------------------------------------------------------
        T_std_state = 25.0
        H_latent_heat = Ndot_FCPM_liqwater *
     &             (  (   Shomate(totalH,T_std_state,H2O,IUOUT)
     &                  - Shomate(deltaH,T_std_state,H2O,IUOUT) )
     &             -  (   Shomate(totalH,T_std_state,liq_H2O,IUOUT)
     &                  - Shomate(deltaH,T_std_state,liq_H2O,IUOUT) )  )


C===========================
C PRODUCT GASES EXITING FCPM
C===========================

C---------------------------------------------------------------------------------
C Calculate the molar flow rate of each gas constituent that flows out of the
C FCPM. Complete reactions assumed.
C---------------------------------------------------------------------------------
C-------N2 in air and fuel flowing into FCPM does not react.
        Ndot_FCPMexh_N2  = chi_air_N2*Ndot_FCPM_air
     &                   + chi_fuel_N2*Ndot_FCPM_fuel
C-------Ar in air flowing into FCPM does not react.
        Ndot_FCPMexh_Ar  = chi_air_Ar*Ndot_FCPM_air
C-------O2 in exhaust comes from fuel and excess air.
        Ndot_FCPMexh_O2  = chi_fuel_O2*Ndot_FCPM_fuel
     &                   + lambda_FCPM*Ndot_FCPM_O2_stoich
C-------CO2 in exhaust comes from CO2 in air and fuel, and from
C-------electrochemical and combustion reactions.
        Ndot_FCPMexh_CO2 = chi_air_CO2*Ndot_FCPM_air
     &                   + chi_fuel_CO2*Ndot_FCPM_fuel
     &                   + 1. * Ndot_FCPM_CH4
     &                   + 2. * Ndot_FCPM_C2H6
     &                   + 3. * Ndot_FCPM_C3H8
     &                   + 4. * Ndot_FCPM_C4H10
     &                   + 5. * Ndot_FCPM_C5H12
     &                   + 6. * Ndot_FCPM_C6H14
     &                   + 1. * Ndot_FCPM_CH3OH
     &                   + 2. * Ndot_FCPM_C2H5OH
C-------H2O in exhaust comes from H2O in air and fuel, from
C-------electrochemical and combustion reactions, and due to addition
C-------of liquid water for reformation.
        Ndot_FCPMexh_H2O = chi_air_H2O*Ndot_FCPM_air
     &                   + 1. * Ndot_FCPM_H2
     &                   + 2. * Ndot_FCPM_CH4
     &                   + 3. * Ndot_FCPM_C2H6
     &                   + 4. * Ndot_FCPM_C3H8
     &                   + 5. * Ndot_FCPM_C4H10
     &                   + 6. * Ndot_FCPM_C5H12
     &                   + 7. * Ndot_FCPM_C6H14
     &                   + 2. * Ndot_FCPM_CH3OH
     &                   + 3. * Ndot_FCPM_C2H5OH
     &                   + Ndot_FCPM_liqwater


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C FCPM exhaust gases at temperature solved for the previous iteration. Once the
C solution converges this will equal the temperature of the exhaust gases for the
C current itereation.
C---------------------------------------------------------------------------------
        T_FCPM_exh_guess = CSVF(CV4,1)  ! Use previous iteration's value.
        IF(T_FCPM_exh_guess<100.) THEN
          T_FCPM_exh_guess = 300.  ! Should only be necessary on first pass through this sub.
        END IF
        deltaH_FCPM_exh = 
     &       Ndot_FCPMexh_N2*Shomate(deltaH,T_FCPM_exh_guess,N2,IUOUT)
     &     + Ndot_FCPMexh_Ar*Shomate(deltaH,T_FCPM_exh_guess,Ar,IUOUT)
     &     + Ndot_FCPMexh_O2*Shomate(deltaH,T_FCPM_exh_guess,O2,IUOUT)
     &     + Ndot_FCPMexh_CO2*Shomate(deltaH,T_FCPM_exh_guess,CO2,IUOUT)
     &     + Ndot_FCPMexh_H2O*Shomate(deltaH,T_FCPM_exh_guess,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the heat capacity rate (W/K), sum of the products of the molar flow
C rates and heat capacities, of the FCPM exhaust gas constituents. This is
C evaluated at the temperature determined at the previous solver iteration. This
C is used to linearize the energy balances for the FCPM and auxiliary burner.
C---------------------------------------------------------------------------------
        C_FCPM   =
     &      Ndot_FCPMexh_N2*Shomate(heatcap,T_FCPM_exh_guess,N2,IUOUT)
     &    + Ndot_FCPMexh_Ar*Shomate(heatcap,T_FCPM_exh_guess,Ar,IUOUT)
     &    + Ndot_FCPMexh_O2*Shomate(heatcap,T_FCPM_exh_guess,O2,IUOUT)
     &    + Ndot_FCPMexh_CO2*Shomate(heatcap,T_FCPM_exh_guess,CO2,IUOUT)
     &    + Ndot_FCPMexh_H2O*Shomate(heatcap,T_FCPM_exh_guess,H2O,IUOUT)


C======================
C SKIN LOSSES FROM FCPM
C======================

C---------------------------------------------------------------------------------
C Calculate the skin losses from the FCPM to the containing room.
C---------------------------------------------------------------------------------
        SELECT CASE (FCPM_skin_loss_method)

        CASE (const_skin_loss)
          FCPM_skin_loss = FCPM_skin_0  ! W

        CASE (skin_f_dT)
C---------Temp of gases exiting FCPM at previous iteration. Once solution
C---------converges this will equal temp for current iteration.
          T_FCPM_exh = CSVF(CV4,1)
C---------Air temperature of room containing SOFC-cogen device.
          CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
          IF(CLOSE)THEN
            WRITE(IUOUT,*) ' No containment has been defined for the '
            WRITE(IUOUT,*) ' SOFC-cogen plant component. Therefore, '
            WRITE(IUOUT,*) ' FCPM skin losses cannot be calc this way.'
            STOP ' SOFC_A42_coeffgen: unresolvable error'
          ELSE
            T_room = PCNTMF(IPCOMP)
          END IF
C---------Skin losses (W).
          FCPM_skin_loss = FCPM_skin_0 * ( T_FCPM_exh-T_room )

        CASE (skin_f_fuel_flow)
          FCPM_skin_loss = FCPM_skin_0 + FCPM_skin_1*Ndot_FCPM_fuel
     &                   + FCPM_skin_2*Ndot_FCPM_fuel**2.
        CASE DEFAULT
          WRITE(IUOUT,*) ' Invalid method for calculating skin ',
     &                   ' loss from FCPM.'
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C=================
C AUXILIARY BURNER
C=================

C---------------------------------------------------------------------------------
C Determine the fuel flow rate to the auxiliary burner and its heat output. The
C user can specify the burner's capacity either in terms of fuel input (kmol/s)
C or heat output (W). CDATA(IPCOMP,2) is the control signal that indicates the
C fraction of the burner's capacity requested. The fuel supplied to the auxiliary
C burner is the same mixture as that supplied to the FCPM.
C---------------------------------------------------------------------------------
        SELECT CASE (Auxburn_present)
        CASE (Auxburn_yes)
          SELECT CASE (Auxburn_W_or_kmols)

          CASE (Auxburn_cap_in_W)
            Q_auxburn         = Auxburn_max_output * CDATA(IPCOMP,2)  ! W
C-----------External control algorithm should not allow operation outwith the
C-----------user-input operating range. If signal is outwith range then either
C-----------turn burner off or run at full capacity and issue a warning.
            IF ( Q_auxburn < Auxburn_min_output ) THEN
              Q_auxburn = 0.
              WRITE(IUOUT,*) ' Controller trying to operate auxiliary',
     &                       ' burner outside of its applicable range.',
     &                       ' Burner turned off.'
            ELSEIF ( Q_auxburn > Auxburn_max_output ) THEN
              Q_auxburn = Auxburn_max_output
              WRITE(IUOUT,*) ' Controller trying to operate auxiliary',
     &                       ' burner outside of its applicable range.',
     &                       ' Burner turned full on.'
            END IF
C-----------Calculate fuel supply rate to burner.
            Ndot_auxburn_fuel = Q_auxburn / LHV_fuel                  ! kmol/s

          CASE (Auxburn_cap_in_kmols)
            Ndot_auxburn_fuel = Auxburn_max_output * CDATA(IPCOMP,2)  ! kmol/s
C-----------External control algorithm should not allow operation outwith the
C-----------user-input operating range. If signal is outwith range then either
C-----------turn burner off or run at full capacity and issue a warning.
            IF ( Ndot_auxburn_fuel < Auxburn_min_output ) THEN
              Ndot_auxburn_fuel = 0.
              WRITE(IUOUT,*) ' Controller trying to operate auxiliary',
     &                       ' burner outside of its applicable range.',
     &                       ' Burner turned off.'
            ELSEIF ( Ndot_auxburn_fuel > Auxburn_max_output ) THEN
              Ndot_auxburn_fuel = Auxburn_max_output
              WRITE(IUOUT,*) ' Controller trying to operate auxiliary',
     &                       ' burner outside of its applicable range.',
     &                       ' Burner turned full on.'
            END IF
C-----------Calculate burner output.
            Q_auxburn         = Ndot_auxburn_fuel * LHV_fuel          ! W

          CASE DEFAULT
            WRITE(IUOUT,*) ' Auxiliary burner cap must be specified',
     &                     ' in terms of fuel input or heat output. '
            STOP ' SOFC_A42_coeffgen: unresolvable error'
          END SELECT


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C fuel entering the auxiliary burner. The auxiliary burner's fuel is assumed to
C be drawn from the same source as that supplied to the FCPM, i.e. if the FCPM
C fuel is drawn from outdoors then so is the auxiliary burner fuel. In this way,
C the temperature of the fuel at the inlet to the auxiliary burner is set to the
C temperature of the fuel entering the compressor.
C---------------------------------------------------------------------------------
        T_auxburn_fuel = T_comp_in
        deltaH_auxburn_fuel =
     &    chi_fuel_H2*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,H2,IUOUT)
     &  + chi_fuel_CH4*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,CH4,IUOUT)
     &  + chi_fuel_C2H6*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,C2H6,IUOUT)
     &  + chi_fuel_C3H8*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,C3H8,IUOUT)
     &  + chi_fuel_C4H10*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,C4H10,IUOUT)
     &  + chi_fuel_C5H12*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,C5H12,IUOUT)
     &  + chi_fuel_C6H14*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,C6H14,IUOUT)
     &  + chi_fuel_CH3OH*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,CH3OH,IUOUT)
     &  + chi_fuel_C2H5OH*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,C2H5OH,IUOUT)
     &  + chi_fuel_CO2*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,CO2,IUOUT)
     &  + chi_fuel_N2*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,N2,IUOUT)
     &  + chi_fuel_O2*Ndot_auxburn_fuel*
     &    Shomate(deltaH,T_auxburn_fuel,O2,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the air supply rate to the auxiliary burner. First, determine the
C molar flow rates (kmol/s) of the fuel constituents that react with O2.
C Then determine the stoichiometric air required to react with these fuel
C constituents. Finally, using the user-specified excess air ratio determine the
C air flow rate.
C---------------------------------------------------------------------------------
C-------Flow rates of fuel constituents (kmol/s).
        Ndot_auxburn_H2     = chi_fuel_H2     * Ndot_auxburn_fuel
        Ndot_auxburn_CH4    = chi_fuel_CH4    * Ndot_auxburn_fuel
        Ndot_auxburn_C2H6   = chi_fuel_C2H6   * Ndot_auxburn_fuel
        Ndot_auxburn_C3H8   = chi_fuel_C3H8   * Ndot_auxburn_fuel
        Ndot_auxburn_C4H10  = chi_fuel_C4H10  * Ndot_auxburn_fuel
        Ndot_auxburn_C5H12  = chi_fuel_C5H12  * Ndot_auxburn_fuel
        Ndot_auxburn_C6H14  = chi_fuel_C6H14  * Ndot_auxburn_fuel
        Ndot_auxburn_CH3OH  = chi_fuel_CH3OH  * Ndot_auxburn_fuel
        Ndot_auxburn_C2H5OH = chi_fuel_C2H5OH * Ndot_auxburn_fuel
C-------Stoichiometric O2 flow rate required for each fuel constituent (kmol/s).
        Ndot_auxburn_O2_stoich = 0.5 * Ndot_auxburn_H2
     &                         + 2.  * Ndot_auxburn_CH4
     &                         + 3.5 * Ndot_auxburn_C2H6
     &                         + 5.  * Ndot_auxburn_C3H8
     &                         + 6.5 * Ndot_auxburn_C4H10
     &                         + 8.  * Ndot_auxburn_C5H12
     &                         + 9.5 * Ndot_auxburn_C6H14
     &                         + 1.5 * Ndot_auxburn_CH3OH
     &                         + 3.  * Ndot_auxburn_C2H5OH
C-------Stoichiometric air flow rate (kmol/s).
        Ndot_auxburn_air_stoich = Ndot_auxburn_O2_stoich / chi_air_O2
C-------Air supply rate (kmol/s).
        Ndot_auxburn_air = (1.+Auxburn_lambda) * Ndot_auxburn_air_stoich


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C air entering the auxiliary burner. The auxiliary burner's air is assumed to
C be drawn from the same source as that supplied to the FCPM, i.e. if the FCPM
C air is drawn from outdoors then so is the auxiliary burner air. In this way,
C the temperature of the air at the inlet to the auxiliary burner is set to the
C temperature of the air entering the compressor.
C---------------------------------------------------------------------------------
        T_auxburn_air = T_blower_in
        deltaH_auxburn_air =
     &    chi_air_N2*Ndot_auxburn_air
     &                         *Shomate(deltaH,T_auxburn_air,N2,IUOUT)
     &  + chi_air_O2*Ndot_auxburn_air
     &                         *Shomate(deltaH,T_auxburn_air,O2,IUOUT)
     &  + chi_air_H2O*Ndot_auxburn_air
     &                         *Shomate(deltaH,T_auxburn_air,H2O,IUOUT)
     &  + chi_air_Ar*Ndot_auxburn_air
     &                         *Shomate(deltaH,T_auxburn_air,Ar,IUOUT)
     &  + chi_air_CO2*Ndot_auxburn_air
     &                         *Shomate(deltaH,T_auxburn_air,CO2,IUOUT)


C---------------------------------------------------------------------------------
C Determine AC elec power draw of the ancillary burner's ancillaries
C (e.g. combustion air supply fan, controls, ignition system) (W). There is no
C electric power draw when the burner is inoperative.
C---------------------------------------------------------------------------------
        CALL ECLOSE(Q_auxburn,0.,0.001,CLOSE)
        IF (CLOSE) THEN
C---------Burner is off.
          auxburn_Pel = 0.
        ELSE
C---------Burner is on.
          auxburn_Pel = Auxburn_anc_0
     &                + Auxburn_anc_1*Ndot_auxburn_fuel
        END IF


C---------------------------------------------------------------------------------
C Calculate the skin losses from the auxiliary burner to the containing room (W).
C This heat can either be added to the inlet air to the FCPM or it can be added
C to the energy balance of the containing room.  There are no skin losses when
C the burner is inoperative.
C---------------------------------------------------------------------------------
        CALL ECLOSE(Q_auxburn,0.,0.001,CLOSE)
        IF (CLOSE) THEN
C---------Burner is off.
          auxburn_skin_loss = 0.
        ELSE
C---------Burner is on.
C---------Temp of gases exiting auxiliary burner at previous iteration. Once solution
C---------converges this will equal temp for current iteration.
          T_auxburn_exh = CSVF(CV5,1)
C---------Air temperature of room containing SOFC-cogen device.
          CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
          IF(CLOSE)THEN
            WRITE(IUOUT,*) ' No containment has been defined for the '
            WRITE(IUOUT,*) ' SOFC-cogen plant component. This is '
            WRITE(IUOUT,*) ' required when modelling an aux burner.'
            STOP ' SOFC_A42_coeffgen: unresolvable error'
          ELSE
            T_room = PCNTMF(IPCOMP)
          END IF
          auxburn_skin_loss = Auxburn_UA * ( T_auxburn_exh-T_room )
        END IF


C---------------------------------------------------------------------------------
C Calculate the molar flow rate of each gas constituent that flows out of the
C auxiliary burner. This is a sum of the products of combustion in the `burner'
C section and the product gases that flow into the `mixing section' from the FCPM.
C Refer to section 9 of the model spec for further details. Complete reactions
C are assumed for the combustion.
C---------------------------------------------------------------------------------
C-------N2 in air and fuel flowing into the auxiliary burner does not react.
C-------This is mixed with N2 from FCPM.
        Ndot_auxburnexh_N2  = chi_air_N2*Ndot_auxburn_air
     &                      + chi_fuel_N2*Ndot_auxburn_fuel
     &                      + Ndot_FCPMexh_N2
C-------Ar in air flowing into auxiliary burner does not react. this is mixed
C-------with Ar from FCPM.
        Ndot_auxburnexh_Ar  = chi_air_Ar*Ndot_auxburn_air
     &                      + Ndot_FCPMexh_Ar
C-------O2 in exhaust comes from fuel and excess air. This is mixed with
C-------O2 from FCPM.
        Ndot_auxburnexh_O2  = chi_fuel_O2*Ndot_auxburn_fuel
     &                      + Auxburn_lambda*Ndot_auxburn_O2_stoich
     &                      + Ndot_FCPMexh_O2
C-------CO2 in exhaust comes from CO2 in air and fuel, and from
C-------combustion reactions. This is mixed with CO2 from FCPM.
        Ndot_auxburnexh_CO2 = chi_air_CO2*Ndot_auxburn_air
     &                      + chi_fuel_CO2*Ndot_auxburn_fuel
     &                      + 1. * Ndot_auxburn_CH4
     &                      + 2. * Ndot_auxburn_C2H6
     &                      + 3. * Ndot_auxburn_C3H8
     &                      + 4. * Ndot_auxburn_C4H10
     &                      + 5. * Ndot_auxburn_C5H12
     &                      + 6. * Ndot_auxburn_C6H14
     &                      + 1. * Ndot_auxburn_CH3OH
     &                      + 2. * Ndot_auxburn_C2H5OH
     &                      + Ndot_FCPMexh_CO2
C-------H2O in exhaust comes from H2O in air and fuel, and from
C-------combustion reactions. This is mixed with H2O from FCPM.
        Ndot_auxburnexh_H2O = chi_air_H2O*Ndot_auxburn_air
     &                      + 1. * Ndot_auxburn_H2
     &                      + 2. * Ndot_auxburn_CH4
     &                      + 3. * Ndot_auxburn_C2H6
     &                      + 4. * Ndot_auxburn_C3H8
     &                      + 5. * Ndot_auxburn_C4H10
     &                      + 6. * Ndot_auxburn_C5H12
     &                      + 7. * Ndot_auxburn_C6H14
     &                      + 2. * Ndot_auxburn_CH3OH
     &                      + 3. * Ndot_auxburn_C2H5OH
     &                      + Ndot_FCPMexh_H2O


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C exhaust gases at temperature solved for the previous iteration. Once the
C solution converges this will equal the temperature of the exhaust gases for the
C current itereation.
C---------------------------------------------------------------------------------
        T_auxburn_exh_guess = CSVF(CV5,1)  ! Use previous iteration's value.
        IF(T_auxburn_exh_guess<100.) THEN
          T_auxburn_exh_guess = 500.  ! Should only be necessary on first pass through this sub.
        END IF
        deltaH_auxburn_exh = 
     &       Ndot_auxburnexh_N2
     &                    *Shomate(deltaH,T_auxburn_exh_guess,N2,IUOUT)
     &     + Ndot_auxburnexh_Ar
     &                    *Shomate(deltaH,T_auxburn_exh_guess,Ar,IUOUT)
     &     + Ndot_auxburnexh_O2
     &                    *Shomate(deltaH,T_auxburn_exh_guess,O2,IUOUT)
     &     + Ndot_auxburnexh_CO2
     &                    *Shomate(deltaH,T_auxburn_exh_guess,CO2,IUOUT)
     &     + Ndot_auxburnexh_H2O
     &                    *Shomate(deltaH,T_auxburn_exh_guess,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the heat capacity rate (W/K, sum of the products of the molar flow
C rates and heat capacities) of the auxiliary burner exhaust gas constituents.
C This is evaluated at the temperature determined at the previous solver
C iteration. This is used to linearize the energy balances for the auxiliary
C burner.
C---------------------------------------------------------------------------------
        C_auxburn   =
     &      Ndot_auxburnexh_N2
     &                   *Shomate(heatcap,T_auxburn_exh_guess,N2,IUOUT)
     &    + Ndot_auxburnexh_Ar
     &                   *Shomate(heatcap,T_auxburn_exh_guess,Ar,IUOUT)
     &    + Ndot_auxburnexh_O2
     &                   *Shomate(heatcap,T_auxburn_exh_guess,O2,IUOUT)
     &    + Ndot_auxburnexh_CO2
     &                   *Shomate(heatcap,T_auxburn_exh_guess,CO2,IUOUT)
     &    + Ndot_auxburnexh_H2O
     &                   *Shomate(heatcap,T_auxburn_exh_guess,H2O,IUOUT)


C---------------------------------------------------------------------------------
C The SOFC-cogen device does not have an auxiliary burner. Set appropriate
C variables that will be written to results files. Also set gas constituent flow
C rates for a flow through control volume.
C---------------------------------------------------------------------------------
        CASE (Auxburn_no)
          Q_auxburn         = 0.
          Ndot_auxburn_fuel = 0.
          Ndot_auxburn_air  = 0.
          auxburn_Pel       = 0.
          auxburn_skin_loss = 0.
          Ndot_auxburnexh_N2  = Ndot_FCPMexh_N2
          Ndot_auxburnexh_Ar  = Ndot_FCPMexh_Ar
          Ndot_auxburnexh_O2  = Ndot_FCPMexh_O2
          Ndot_auxburnexh_CO2 = Ndot_FCPMexh_CO2
          Ndot_auxburnexh_H2O = Ndot_FCPMexh_H2O

C-------Error in input file.
        CASE DEFAULT
          WRITE(IUOUT,*) ' Invalid indication regarding existence of',
     &                   ' auxiliary burner. '
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C============================
C GAS-TO-WATER HEAT EXCHANGER
C============================

C---------------------------------------------------------------------------------
C There are four methods for resolving the heat exchanger. With the first method
C the HX is characterized with a fixed effectiveness and heat transfer calculated
C using the approach temperature. The LMTD approach is used in other three
C methods. In method 2 the effective UA is determined using an entirely empirical
C approach whereas in method 3 it is expressed in an idealized form based upon
C more fundamental heat transfer processes. Both methods 2 and 3 are for sensible
C heat transfer only. Method 4 considers condensation using the LMTD approach to
C characterize the sensible heat transfer
C---------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C Establish the entering water and gas temperatures and the fluid heat capacity
C rates (W/K) for the gas and water streams flowing into the heat exchanger. The
C heat capacity of the gas stream at the heat exchanger inlet is determined at
C temperature solved for the auxiliary burner at the previous iteration. Once the
C solution converges this will equal the temperature of the exhaust gases for the
C current itereation.
C Variables/functions used:
C    SHTFLD(3,T) function that returns the heat capacity of water (J/kgK) at
C    temp 'T' (oC).
C    CONVAR(i,1) holds the temperature (oC) of sending node for connection `i'.
C    CONVAR(i,2) holds the mass flow rate (kg/s) at sending node for conn `i'.
C    PCONDR(i) is the ratio of flow rate leaving sending node `i' that reaches
C              receiving node.
C---------------------------------------------------------------------------------
C-------Gas stream.
        T_HX_gas_in = CSVF(CV5,1)  ! Use previous iteration's value.
        IF(T_HX_gas_in<100.) THEN
          T_HX_gas_in = 400.  ! Should only be necessary on first pass through this sub.
        END IF
        C_gas   =
     &        Ndot_auxburnexh_N2 *Shomate(heatcap,T_HX_gas_in,N2,IUOUT)
     &      + Ndot_auxburnexh_Ar *Shomate(heatcap,T_HX_gas_in,Ar,IUOUT)
     &      + Ndot_auxburnexh_O2 *Shomate(heatcap,T_HX_gas_in,O2,IUOUT)
     &      + Ndot_auxburnexh_CO2*Shomate(heatcap,T_HX_gas_in,CO2,IUOUT)
     &      + Ndot_auxburnexh_H2O*Shomate(heatcap,T_HX_gas_in,H2O,IUOUT)
C-------Water stream.
        T_HX_water_in = CONVAR(con_CV6,1)
        Mdot_HX_liqwater = PCONDR(con_CV6) * CONVAR(con_CV6,2)
        C_water = Mdot_HX_liqwater * SHTFLD( 3,T_HX_water_in )


C---------------------------------------------------------------------------------
C Determine the molar flow rates on both the water side and gas side of HX. This
C is required for the methods that employ the LMTD approach.
C---------------------------------------------------------------------------------
        SELECT CASE ( HX_method )

        CASE ( HX_lmtd_empirical, HX_lmtd_deterministic, HX_condensing )
          Ndot_HX_liqwater = Mdot_HX_liqwater / molar_mass_H2O
          Ndot_HX_gas = Ndot_auxburnexh_N2 + Ndot_auxburnexh_Ar
     &                  + Ndot_auxburnexh_O2 + Ndot_auxburnexh_CO2
     &                  + Ndot_auxburnexh_H2O

        CASE DEFAULT
          Ndot_HX_liqwater = 0.
          Ndot_HX_gas      = 0.

        END SELECT


C---------------------------------------------------------------------------------
C Establish the water and gas temperatures at the outlet of the HX at the
C previous iteration. Once the solution converges this will equal the temperature
C of the exhaust gases for the current itereation. This is required for the
C methods that employ the LMTD approach.
C---------------------------------------------------------------------------------
        SELECT CASE ( HX_method )

        CASE ( HX_lmtd_empirical, HX_lmtd_deterministic, HX_condensing )

          T_HX_water_out = CSVF(CV6,1)  ! Use previous iteration's value.
          IF(T_HX_water_out<1.) THEN
            T_HX_water_out = 10.  ! Should only be necessary on first pass through this sub.
          END IF
          T_HX_gas_out = CSVF(CV7,1)  ! Use previous iteration's value.
          IF(T_HX_gas_in<10.) THEN
            T_HX_gas_out = 10.  ! Should only be necessary on first pass through this sub.
          END IF
C---------It is physically impossible for the HX to cool the exhaust gases to
C---------below the temperature of the incoming water. However, this modelling
C---------artifact can occur during the convergence of the solution. To protect
C---------the LMTD calculation (such a situation leads to NaN) constrain the
C---------existing gas temperature.
          IF( T_HX_gas_out < T_HX_water_in ) THEN
            T_HX_gas_out = T_HX_water_in + 0.1
            WRITE(IUOUT,*) ' Value for T_HX_gas_out is physically ',
     &                     ' unrealistic.'
          END IF

        CASE DEFAULT
          T_HX_water_out = 0.
          T_HX_gas_out   = 0.

        END SELECT


C---------------------------------------------------------------------------------
C Calculate the effective UA value of the HX's sensible heat transfer. This is
C required for the methods that employ the LMTD approach.
C---------------------------------------------------------------------------------
        SELECT CASE ( HX_method )

        CASE ( HX_lmtd_empirical, HX_condensing )
          UA_HX = HX_sen_0
     &          + HX_sen_1 * Ndot_HX_liqwater
     &          + HX_sen_2 * Ndot_HX_liqwater**2.
     &          + HX_sen_3 * Ndot_HX_gas
     &          + HX_sen_4 * Ndot_HX_gas**2.
          HX_h_gas   = 0.
          HX_h_water = 0.

        CASE ( HX_lmtd_deterministic )
          HX_h_gas   = HX_h0_gas
     &                 * ( Ndot_HX_gas      / HX_Ndot0_gas   )**HX_n
          HX_h_water = HX_h0_water
     &                 * ( Ndot_HX_liqwater / HX_Ndot0_water )**HX_m
          UA_HX = 1. / (
     &                     1. / ( HX_h_gas   * HX_Area_gas   )
     &                   + 1. / ( HX_h_water * HX_Area_water )
     &                   + HX_F )

        CASE DEFAULT
          HX_h_gas   = 0.
          HX_h_water = 0.
          UA_HX = 0.

        END SELECT


C---------------------------------------------------------------------------------
C Calculate the rate of condensation of water vapour from the gas stream. This is
C required only for the method which considers condensing HX. Condensation will
C only occur when the water inlet temperature is at or below the user-specified
C condensation threshold.  The latent heat of vapourization is calculated at the
C temperature of the water entering the heat exchanger.
C---------------------------------------------------------------------------------
        SELECT CASE ( HX_method )

        CASE ( HX_condensing )
          HX_water_vap_frac = Ndot_auxburnexh_H2O / Ndot_HX_gas
          HX_Hfg =  (   Shomate(totalH,T_HX_water_in,H2O,IUOUT)
     &                - Shomate(deltaH,T_HX_water_in,H2O,IUOUT)   )
     &            - (   Shomate(totalH,T_HX_water_in,liq_H2O,IUOUT)
     &                - Shomate(deltaH,T_HX_water_in,liq_H2O,IUOUT)   )

          IF( T_HX_water_in <= HX_T_cond_thresh ) THEN
            HX_Ndot_condense = ( HX_T_cond_thresh - T_HX_water_in ) *
     &                         ( HX_lat_1 * HX_water_vap_frac
     &                           + HX_lat_2 * HX_water_vap_frac**2. )

          ELSE
            HX_Ndot_condense = 0.
          END IF

        CASE DEFAULT
          HX_Ndot_condense = 0.

        END SELECT


C---------------------------------------------------------------------------------
C Calculate the heat transfer from the gas to the water. This is not used in the
C coefficients of the energy balance equations which are subsequently formed.
C Rather, the heat transfer is calculated here simply for outputting to the
C results file. For the condensing HX method the sensible and latent components
C of the heat transfer are also calculated. This is necessary for the 
C establishment of the coefficients for the energy balances. These heat transfers
C are calculated using temperatures at the previous iteration. Once the solution
C converges these heat transfers should equal the converged solution.
C---------------------------------------------------------------------------------
        SELECT CASE ( HX_method )

        CASE ( HX_fixed_eff )
          C_min = MIN( C_gas,C_water )
          Q_HX = HX_eff * C_min * (T_HX_gas_in - T_HX_water_in)

        CASE ( HX_lmtd_empirical, HX_lmtd_deterministic)
          Q_HX = UA_HX * (
     &                      ( T_HX_gas_in  - T_HX_water_out )
     &                     -( T_HX_gas_out - T_HX_water_in  )
     &                   ) /
     &                LOG(
     &                      ( T_HX_gas_in  - T_HX_water_out ) /
     &                      ( T_HX_gas_out - T_HX_water_in  )
     &                   )

        CASE ( HX_condensing )
          Q_HX_sensible = UA_HX * (
     &                       ( T_HX_gas_in  - T_HX_water_out )
     &                      -( T_HX_gas_out - T_HX_water_in  )
     &                            ) /
     &                    LOG(
     &                         ( T_HX_gas_in  - T_HX_water_out ) /
     &                         ( T_HX_gas_out - T_HX_water_in  )
     &                       )
          Q_HX_latent   = HX_Ndot_condense * HX_Hfg
          Q_HX = Q_HX_sensible + Q_HX_latent

        CASE DEFAULT

        END SELECT


C====================
C DILUTION AIR SYSTEM
C====================

        SELECT CASE (Dilution_present)
        CASE (Dilution_yes)


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C air entering the dilution air system. The dilution air system's air is assumed
C to be drawn from the same source as that supplied to the FCPM, i.e. if the FCPM
C air is drawn from outdoors then so is the dilution air.  In this way, the
C temperature of the air at the inlet to the dilution air system is set to the
C temperature of the air entering the compressor.
C---------------------------------------------------------------------------------
        T_dilution_air_in = T_blower_in
        deltaH_dilution_in =
     &    chi_air_N2*Dilution_airflow
     &                      *Shomate(deltaH,T_dilution_air_in,N2,IUOUT)
     &  + chi_air_O2*Dilution_airflow
     &                      *Shomate(deltaH,T_dilution_air_in,O2,IUOUT)
     &  + chi_air_H2O*Dilution_airflow
     &                      *Shomate(deltaH,T_dilution_air_in,H2O,IUOUT)
     &  + chi_air_Ar*Dilution_airflow
     &                      *Shomate(deltaH,T_dilution_air_in,Ar,IUOUT)
     &  + chi_air_CO2*Dilution_airflow
     &                      *Shomate(deltaH,T_dilution_air_in,CO2,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C gases entering the dilution air system from the HX at temperature solved for the
C previous iteration.  Once the solution converges this will equal the temperature
C for the current itereation.  The temperature of this stream (i.e. the gases
C exhausted from the HX) are calculated above in the HX section.  This stream has
C the same gas composition as the stream exiting the auxiliary burner.
C---------------------------------------------------------------------------------
          deltaH_HX_gas_out =
     &       Ndot_auxburnexh_N2
     &                   *Shomate(deltaH,T_HX_gas_out,N2,IUOUT)
     &     + Ndot_auxburnexh_Ar
     &                   *Shomate(deltaH,T_HX_gas_out,Ar,IUOUT)
     &     + Ndot_auxburnexh_O2
     &                   *Shomate(deltaH,T_HX_gas_out,O2,IUOUT)
     &     + Ndot_auxburnexh_CO2
     &                   *Shomate(deltaH,T_HX_gas_out,CO2,IUOUT)
     &     + Ndot_auxburnexh_H2O
     &                   *Shomate(deltaH,T_HX_gas_out,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the heat capacity rate (W/K, sum of the products of the molar flow
C rates and heat capacities) of the gases entering the dilution air system from
C the HX.  This is evaluated at the temperature determined at the previous solver
C iteration. This is used to linearize the energy balances for the dilution air
C system.
C---------------------------------------------------------------------------------
          C_HX_gas_out   =
     &        Ndot_auxburnexh_N2
     &                  *Shomate(heatcap,T_HX_gas_out,N2,IUOUT)
     &    +   Ndot_auxburnexh_Ar
     &                  *Shomate(heatcap,T_HX_gas_out,Ar,IUOUT)
     &    +   Ndot_auxburnexh_O2
     &                  *Shomate(heatcap,T_HX_gas_out,O2,IUOUT)
     &    +   Ndot_auxburnexh_CO2
     &                  *Shomate(heatcap,T_HX_gas_out,CO2,IUOUT)
     &    +   Ndot_auxburnexh_H2O
     &                  *Shomate(heatcap,T_HX_gas_out,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the molar flow rate of each gas constituent that flows out of the
C dilution air system. This is a sum of the mixing of the gases exiting the
C heat exchanger (same composition as the gases exiting the auxiliary burner) and
C the dilution air that is drawn through the FCPM.
C---------------------------------------------------------------------------------
          Ndot_dilution_N2  = chi_air_N2*Dilution_airflow
     &                        + Ndot_auxburnexh_N2
          Ndot_dilution_Ar  = chi_air_Ar*Dilution_airflow
     &                        + Ndot_auxburnexh_Ar
          Ndot_dilution_O2  = chi_air_O2*Dilution_airflow
     &                        + Ndot_auxburnexh_O2
          Ndot_dilution_CO2  = chi_air_CO2*Dilution_airflow
     &                        + Ndot_auxburnexh_CO2
          Ndot_dilution_H2O  = chi_air_H2O*Dilution_airflow
     &                        + Ndot_auxburnexh_H2O


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W), relative to the standard state, of the
C gases exiting the dilution air system at temperature solved for the previous
C iteration.  Once the solution converges this will equal the temperature for the
C current itereation.
C---------------------------------------------------------------------------------
          T_dilution_exh_guess = CSVF(CV8,1)  ! Use previous iteration's value.
          deltaH_dilution_exh =
     &       Ndot_dilution_N2
     &                   *Shomate(deltaH,T_dilution_exh_guess,N2,IUOUT)
     &     + Ndot_dilution_Ar
     &                   *Shomate(deltaH,T_dilution_exh_guess,Ar,IUOUT)
     &     + Ndot_dilution_O2
     &                   *Shomate(deltaH,T_dilution_exh_guess,O2,IUOUT)
     &     + Ndot_dilution_CO2
     &                   *Shomate(deltaH,T_dilution_exh_guess,CO2,IUOUT)
     &     + Ndot_dilution_H2O
     &                   *Shomate(deltaH,T_dilution_exh_guess,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the heat capacity rate (W/K, sum of the products of the molar flow
C rates and heat capacities) of the gases exiting the dilution air system.
C This is evaluated at the temperature determined at the previous solver
C iteration. This is used to linearize the energy balances for the dilution air
C system.
C---------------------------------------------------------------------------------
          C_dilution_exh   =
     &        Ndot_dilution_N2
     &                  *Shomate(heatcap,T_dilution_exh_guess,N2,IUOUT)
     &    +   Ndot_dilution_Ar
     &                  *Shomate(heatcap,T_dilution_exh_guess,Ar,IUOUT)
     &    +   Ndot_dilution_O2
     &                  *Shomate(heatcap,T_dilution_exh_guess,O2,IUOUT)
     &    +   Ndot_dilution_CO2
     &                  *Shomate(heatcap,T_dilution_exh_guess,CO2,IUOUT)
     &    +   Ndot_dilution_H2O
     &                  *Shomate(heatcap,T_dilution_exh_guess,H2O,IUOUT)


C-------No calculations required if dilution air system not preesnt.
        CASE (Dilution_no)

C-------Error in input file.
        CASE DEFAULT
          WRITE(IUOUT,*) ' Invalid indication regarding existence of',
     &                   ' dilution air system. '
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C====
C HRV
C====

        SELECT CASE (HRV_present)
        CASE (SOFC_HRV_yes)


C---------------------------------------------------------------------------------
C Establish the fluid heat capacity rate (W/K) for the fresh air stream flowing
C through the HRV.  The fluid heat capacity rate of the warm exhaust gas stream
C was determined above ('C_dilution_exh').  Then determine the minimum of the two
C fluid heat capacity rates.  This is used to establish the energy balance for the
C HRV.  It is assumed that the fresh air is drawn from outdoors.
C---------------------------------------------------------------------------------
          T_fresh_air = TF
          C_HRV_fresh_air =
     &      chi_air_N2*HRV_freshairflow
     &                        *Shomate(heatcap,T_fresh_air,N2,IUOUT)
     &  +   chi_air_O2*HRV_freshairflow
     &                        *Shomate(heatcap,T_fresh_air,O2,IUOUT)
     &  +   chi_air_H2O*HRV_freshairflow
     &                        *Shomate(heatcap,T_fresh_air,H2O,IUOUT)
     &  +   chi_air_Ar*HRV_freshairflow
     &                        *Shomate(heatcap,T_fresh_air,Ar,IUOUT)
     &  +   chi_air_CO2*HRV_freshairflow
     &                        *Shomate(heatcap,T_fresh_air,CO2,IUOUT)
          C_HRV_min = MIN( C_HRV_fresh_air,C_dilution_exh )


C---------------------------------------------------------------------------------
C Calculate the HRV's heat recovery rate and the temperature of the fresh air
C supplied to the building.  Thees values will be recorded and available in the
C plant component's output.  However, in the current implementation the impact
C of the HRV upon the building thermal domain is not treated.  In the future an
C additional node could be added to this plant component to generalize the
C treatment of the HRV and to fully integrate it with the building thermal domain
C and the plant and network air flow domains.  This additional node would be an
C air node and would represent the fresh air exiting from the HRV.
C---------------------------------------------------------------------------------
          q_HRV = HRV_effectiveness * C_HRV_min
     &            * ( T_dilution_exh_guess - T_fresh_air )
          T_vent_air = T_fresh_air
     &                 + ( HRV_fanpower + q_HRV ) / C_HRV_fresh_air

C-------No HRV present.
        CASE (SOFC_HRV_no)
          q_HRV = 0.
          T_vent_air = -99.

C-------Error in input file.
        CASE DEFAULT
          WRITE(IUOUT,*) ' Invalid indication regarding existence of',
     &                   ' HRV. '
          STOP ' SOFC_A42_coeffgen: unresolvable error'

        END SELECT


C=============================
C STACK COOLING SYSTEM (PEMFC)
C=============================
        SELECT CASE (stack_cooling_present)
        CASE (s_cool_yes)


C---------------------------------------------------------------------------------
C Calculate the rate at which heat must be extracted from the stack.
C---------------------------------------------------------------------------------
          CALL ECLOSE(T_stack_0, 0.,1.E-12,CLOSE)
          IF (CLOSE) THEN
            q_s_cool = ( s_cool_e_0
     &                 + s_cool_e_2*Pel
     &                 + s_cool_e_3*Pel**2. ) * Pel
          ELSE
            q_s_cool = ( s_cool_e_0 + s_cool_e_1*(T_stack-T_stack_0) )
     &                 * ( 1. + s_cool_e_2*Pel
     &                     + s_cool_e_3*Pel**2. ) * Pel
          ENDIF


C---------------------------------------------------------------------------------
C Establish the state point temperatures at the previous iteration. Once the
C solution converges these will equal the temperatures at the current iteration.
C The `T_' indices refer to the state point numbering system used in the model
C spec.
C---------------------------------------------------------------------------------
          T_21 = CSVF(CV10,1)
          T_22 = CSVF(CV11,1)
          T_23 = CSVF(CV12,1)
          T_25 = CONVAR(con_CV14,1)


C---------------------------------------------------------------------------------
C Establish the heat capacity of the water at the relevant state points.
C---------------------------------------------------------------------------------
          IF (NSINC == 1) THEN  ! Assume density on first time-step of simulation.
            cp_21 = 4180.
            cp_22 = 4180.
            cp_25 = 4180.
          ELSE
            cp_21 = SHTFLD( 3,T_21 )
            cp_22 = SHTFLD( 3,T_22 )
            cp_25 = SHTFLD( 3,T_25 ) 
          ENDIF


C---------------------------------------------------------------------------------
C Calculate heat capacity rates (W/K, sum of the products of the mass flow
C rates and heat capacities) of the water flowing into and out of the internal
C heat exchanger as well as out of the external heat exchanger. Also determine
C the "alpha" value of the internal HX (the exponential of the UA value divided
C by the heat capacity rate).
C---------------------------------------------------------------------------------
          Ndot_s_cool_in_cp  = Mdot_s_cool * cp_21
          Ndot_s_cool_out_cp = Mdot_s_cool * cp_22
          Mdot_s_cogen       = PCONDR(con_CV14) * CONVAR(con_CV14,2)
          Ndot_cogen_cp      = Mdot_s_cogen * cp_25
          alpha_s_cool_HX    = exp( UA_s_cool / Ndot_s_cool_in_cp )


C---------------------------------------------------------------------------------
C Determine whether there is water flow on the heat-recovery side of the external
C HX. If so, then calculate the external heat exchanger's "alpha" value (the
C exponential of the UA value multiplied by the difference of the inverse of
C the heat capacity rates) and "beta" value (the ratio of the heat capacity rates
C of the two incoming streams).
C---------------------------------------------------------------------------------
          CALL ECLOSE(Mdot_s_cogen, 0.,1.E-12,CLOSE)
          IF (CLOSE) THEN    ! Cold water flow into cogen heat exchanger too small.
            cogen_HX_state = no_flow
          ELSE
            cogen_HX_state = no_bypass  ! Assume for now: decision will be made later.
C-----------Determine UA value of external HX: either user input or calculated.
            IF (Calc_UA_s_cogen) THEN
              h_s_cogen  = h_s_cogen_0*
     &                     ( Mdot_s_cogen / Mdot_s_cogen_0 )**n_s_cogen 
              UA_s_cogen = 1. / ( 1./( A_s_cogen*h_s_cogen )
     &                            + R_s_cogen )
            ENDIF
            alpha_cogen_HX = exp ( UA_s_cogen
     &                             * ( 1./Ndot_s_cool_out_cp
     &                                 -1./Ndot_cogen_cp )   )
            beta_cogen_HX  = Ndot_s_cool_out_cp / Ndot_cogen_cp
          ENDIF


C---------------------------------------------------------------------------------
C There is a bypass loop within the stack cooling loop. This diverts flow through
C the HX when conditions are such that the full flow rate of water would result
C in excess cooling. Determine the operational state of this bypass loop.
C---------------------------------------------------------------------------------
          IF( cogen_HX_state /= no_flow )THEN
            T_23_nobypass = ( 1. - beta_cogen_HX )
     &                      / ( alpha_cogen_HX - beta_cogen_HX ) * T_22
     &                    + ( alpha_cogen_HX - 1. )
     &                      / ( alpha_cogen_HX - beta_cogen_HX ) * T_25
            T_24_required = T_21
     &                      - ( 1. - s_cool_pump_heat_loss )
     &                        * s_cool_pump_Pel / Ndot_s_cool_in_cp
            IF ( T_23_nobypass < T_24_required) THEN
              cogen_HX_state = bypass
            ELSE
              cogen_HX_state = no_bypass
            ENDIF
C---------------------------------------------------------------------------------
C If the water in the heat recovery loop is hotter then in the stack cooling loop 
C then heat would be transfered in the wrong direction. To prevent from this the 
C controller totally opens the bypass which has the same effect as if no water flows
C in the heat recovery loop.     
C---------------------------------------------------------------------------------

            IF ( T_23_nobypass > T_22) THEN
              cogen_HX_state = no_flow
            ENDIF

          ENDIF


C---------------------------------------------------------------------------------
C Calculate heat transferred from PEM cooling loop to heat recovery loop (i.e. the
C useful heat output for cogeneration purposes). Also calculate the rate at which
C heat is rejected from the air cooler. These quantities are not used in the
C coefficients of the energy balance equations which are subsequently formed.
C Rather, the heat transfers are calculated here simply for outputting to the
C results file. These heat transfers are calculated using temperatures at the
C previous iteration. Once the solution converges these heat transfers should
C equal the converged solution.
C Also calculate the AC power draw of the air-cooler's fan.
C---------------------------------------------------------------------------------
C---------Heat transferred to heat recovery loop.
          q_s_cogen = Ndot_s_cool_out_cp * ( T_22 - T_23 )
C---------Heat that must be rejected from stack loop water via air cooler.
          q_s_cool_air = q_s_cool
     &                   + ( 1.-s_cool_pump_heat_loss )*s_cool_pump_Pel
     &                   - q_s_cogen
          IF( q_s_cool_air<0. ) q_s_cool_air = 0.
C---------AC power draw of air cooler fan.
          P_air_el= ( air_cool_f_0                       ! power consumption of the cooler fan
     &              + air_cool_f_1*q_s_cool_air  
     &              + air_cool_f_2*q_s_cool_air**2.)
C---------Air cooler also rejects the heat generated by the fan.
          q_s_cool_air = q_s_cool_air + P_air_el

        CASE DEFAULT
        END SELECT


C==================
C NET AC PRODUCTION
C==================

C---------------------------------------------------------------------------------
C Calculate the net AC production from the FC-cogen device. This is the gross AC
C produced by the PCU less the draws of the AC-powered ancillaries.
C---------------------------------------------------------------------------------
        AC_production_net = AC_production_gross - Pel_ancAC - blower_Pel
     &                      - comp_Pel - pump_Pel - auxburn_Pel 
     &                      - P_air_el - s_cool_pump_Pel
     &                      - Dilution_fanpower - HRV_fanpower

C---------------------------------------------------------------------------------
C Communicate the net AC production to the electrical modelling domain.
C---------------------------------------------------------------------------------
        pwrp(ipcomp) = AC_production_net     ! Real power (W).
        IEMODEL=1
        CALL EMACH( IPCOMP,IEMODEL,pwrp(ipcomp),PQ,PA)


C=========================================
C TRANSFER HEAT TO BUILDING THERMAL DOMAIN
C=========================================

C---------------------------------------------------------------------------------
C Tally up heat losses from compressor, blower, pump, and skin losses from FCPM.
C These are assumed to be transferred to the room that contains the FC-cogen
C device.
C---------------------------------------------------------------------------------
        heat_to_room = comp_q_loss + blower_q_loss + pump_q_loss
     &               + FCPM_skin_loss


C---------------------------------------------------------------------------------
C The losses from the auxiliary burner, battery, and PCU may have been recovered
C to pre-heat the air stream entering the FCPM. If the user has indicated that
C these losses are to be injected to the room air, then add them to the above
C summation.
C---------------------------------------------------------------------------------
C-------Auxiliary burner.
        IF ( Auxburn_heat_to_where==Auxburn_heat_to_room )THEN
          heat_to_room = heat_to_room + auxburn_skin_loss
        ENDIF
C-------Battery.
        IF( battery_heat_to_where==battery_heat_to_room )THEN
          heat_to_room = heat_to_room + q_battery_loss
        END IF
C-------PCU.
        IF( PCU_heat_to_where==PCU_heat_to_room )THEN
          heat_to_room = heat_to_room + PCU_losses
        ENDIF


C---------------------------------------------------------------------------------
C Determine the convective and radiative split and call the routine that stores
C these heat transfer rates for subsequent injection into the zone energy
C balances. 'iConvective' and 'iRadiant' are parameters defined in plant.h.
C---------------------------------------------------------------------------------
        heat_to_room_conv = heat_to_room * FCPM_skin_loss_conv
        heat_to_room_rad  = heat_to_room * ( 1.-FCPM_skin_loss_conv )
        CALL STORE_PLT_GAIN (IPCOMP, heat_to_room_conv, iConvective)
        CALL STORE_PLT_GAIN (IPCOMP, heat_to_room_rad, iRadiant)


C========================================================
C CALCULATE NOMINAL EFFICIENCY VALUES FOR OUTPUT PURPOSES
C========================================================
        LHV_to_AC_eff    = AC_production_net /
     &                     ( ( Ndot_FCPM_fuel + Ndot_auxburn_fuel )
     &                       * LHV_fuel
     &                     )
        LHV_to_therm_eff = q_HX / 
     &                     ( ( Ndot_FCPM_fuel + Ndot_auxburn_fuel )
     &                       * LHV_fuel
     &                     )
        LHV_to_cogen_eff = LHV_to_AC_eff + LHV_to_therm_eff


C===========================================
C COEFFICIENTS FOR MATRIX OF ENERGY BALANCES
C===========================================

C CV1 (air within blower).
C-------Prevent division by zero in RHS coefficient when there is no air flow.
        COUT( 1) = 1.
        CALL ECLOSE(Ndot_FCPM_air, 0., 1.E-12, CLOSE)
        IF(CLOSE)THEN
          COUT(32) = 0.
        ELSE
          COUT(32) = T_blower_in
     &             + ( ( 1.-Blower_heat_loss ) * blower_Pel
     &                 + q_recover_to_air )
     &               / ( Ndot_FCPM_air*cphat_air )
        END IF
C CV2 (fuel within compressor).
C-------Prevent division by zero in RHS coefficient when there is no fuel flow.
        COUT( 2) = 1.
        CALL ECLOSE(Ndot_FCPM_fuel, 0., 1.E-12, CLOSE)
        IF(CLOSE)THEN
          COUT(33) = 0.
        ELSE
          COUT(33) = T_comp_in
     &             + ( 1.-Compressor_heat_loss ) * comp_Pel
     &               / ( Ndot_FCPM_fuel*cphat_fuel )
        END IF
C CV3 (water within pump).
C-------Prevent division by zero in RHS coefficient when there is no fuel flow.
        COUT( 3) = 1.
        CALL ECLOSE(Ndot_FCPM_liqwater, 0., 1.E-12, CLOSE)
        IF(CLOSE)THEN
          COUT(34) = 0.
        ELSE
          COUT(34) = T_pump_in
     &             + ( 1.-Pump_heat_loss ) * pump_Pel
     &               / ( Ndot_FCPM_liqwater*cphat_liqwater )
        END IF
C CV4 (gas within FCPM).
        COUT( 4) = 0.
        COUT( 5) = 0.
        COUT( 6) = 0.
        COUT( 7) = 1.
        COUT(35) = ( deltaH_FCPM_fuel + deltaH_FCPM_air
     &               + Ndot_FCPM_fuel*LHV_fuel + Pel_ancAC
     &               + deltaH_FCPM_liqwater
     &               - H_latent_heat
     &               - Pel - FCPM_skin_loss - q_s_cool
     &               - Dilution_heattoair
     &               - deltaH_FCPM_exh ) / C_FCPM
     &             + T_FCPM_exh_guess
C CV5 (gas within burner).
        SELECT CASE (Auxburn_present)
        CASE (Auxburn_yes)
          COUT( 8) = -1. * C_FCPM  ! Cross-coupling to FCPM gas.
          COUT( 9) = C_auxburn                           ! Self-coupling.
          COUT(36) = -1. * C_FCPM * T_FCPM_exh_guess
     &               + C_auxburn * T_auxburn_exh_guess
     &               + deltaH_FCPM_exh
     &               -1. * deltaH_auxburn_exh
     &               + deltaH_auxburn_fuel + deltaH_auxburn_air
     &               + Ndot_auxburn_fuel*LHV_fuel + auxburn_Pel
     &               - auxburn_skin_loss
        CASE (Auxburn_no)
          COUT( 8) = -1. ! Cross-coupling to FCPM gas.
          COUT( 9) = 1.  ! Self-coupling.
          COUT(36) = 0.
        END SELECT ! CASE DEFAULT not required because already checked above.

C Gas-to-water heat exchanger: CV6 (HX-water) and CV7 (HX-gas).
        SELECT CASE ( HX_method )
        CASE ( HX_fixed_eff )
C---------CV6 (HX-water).
          COUT(10) = -1. * HX_eff * C_min / C_water   ! Coupling to gas inlet temp.
          COUT(11) = 1.                               ! Self-coupling.
          COUT(12) = 0.                               ! Coupling to gas outlet temp.
          COUT(30) = HX_eff * C_min / C_water - 1.    ! Cross-coupling to cold water inlet.
          COUT(37) = 0.                               ! RHS.
C---------CV7 (HX-gas).
          COUT(13) = HX_eff * C_min / C_gas - 1.      ! Coupling to gas inlet temp.
          COUT(14) = 1.                               ! Self-coupling.
          COUT(38) = HX_eff * C_min / C_gas           ! RHS (Cannot cross-couple to cold
     &               * T_HX_water_in                  ! water inlet since only CV6 is physically                           
                                                      ! connected to water inlet, so this
                                                      ! this appears instead on RHS.

        CASE ( HX_lmtd_empirical, HX_lmtd_deterministic )
C---------CV6 (HX-water).
          COUT(10) = -1. * C_gas / C_water
          COUT(11) = 1.
          COUT(12) = C_gas / C_water
          COUT(30) = -1.
          COUT(37) = 0.
C---------CV7 (HX-gas).
          COUT(13) = ( C_gas/C_water -1. ) /
     &               ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                 - C_gas / C_water )
          COUT(14) = 1.
          COUT(38) = T_HX_water_in *
     &               ( EXP ( UA_HX * (1./C_gas - 1./C_water) ) - 1. )
     &               /
     &               ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                 - C_gas / C_water )

        CASE ( HX_condensing )
C---------Determine if there is sufficient condensation and ensure that there
C---------is a heat transfer, otherwise there will be divide-by-zero errors.
          CALL ECLOSE(HX_Ndot_condense, 0., 1.E-8, CLOSE)
          CALL ECLOSE(Q_HX_sensible, 0., 1., CLOSEA)       ! Can be zero on first iteration.
          IF(CLOSE .or. CLOSEA)THEN
C-----------There is no condensation this time-step.
C-----------CV6 (HX-water).
            COUT(10) = -1. * C_gas / C_water
            COUT(11) = 1.
            COUT(12) = C_gas / C_water
            COUT(30) = -1.
            COUT(37) = 0.
C-----------CV7 (HX-gas).
            COUT(13) = ( C_gas/C_water -1. ) /
     &                 ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                   - C_gas / C_water )
            COUT(14) = 1.
            COUT(38) = T_HX_water_in *
     &                 ( EXP ( UA_HX * (1./C_gas - 1./C_water) ) - 1. )
     &                 /
     &                 ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                   - C_gas / C_water )

          ELSE
C-----------Water is condensing.
C-----------CV6 (HX-water),
            COUT(10) = -1. * C_gas / C_water
            COUT(11) = 1.
            COUT(12) = C_gas / C_water
            COUT(30) = -1.
            COUT(37) = Q_HX_latent / C_water
C-----------CV7 (HX-gas).
            COUT(13) = ( C_gas/C_water -1. ) /
     &                 ( (  EXP ( UA_HX * (1./C_gas - 1./C_water) )  ) /
     &                   (  EXP (   ( UA_HX * Q_HX_latent     )
     &                            / ( C_water * Q_HX_sensible ) ) )  
     &                   - C_gas / C_water )
            COUT(14) = 1.
            COUT(38) = T_HX_water_in *
     &                 ( (  EXP ( UA_HX * (1./C_gas - 1./C_water) )  ) /
     &                   (  EXP (   ( UA_HX * Q_HX_latent     )
     &                            / ( C_water * Q_HX_sensible ) )  )
     &                    - 1. )
     &                 /
     &                 ( (  EXP ( UA_HX * (1./C_gas - 1./C_water) )  ) /
     &                   (  EXP (   ( UA_HX * Q_HX_latent     )
     &                            / ( C_water * Q_HX_sensible ) ) )  
     &                   - C_gas / C_water )
     &                -
     &                  ( Q_HX_latent / C_water )
     &                  /
     &                 ( (  EXP ( UA_HX * (1./C_gas - 1./C_water) )  ) /
     &                   (  EXP (   ( UA_HX * Q_HX_latent     )
     &                            / ( C_water * Q_HX_sensible ) ) )  
     &                   - C_gas / C_water )
          END IF


        CASE DEFAULT

        END SELECT


C CV8 (dilution air system).
        SELECT CASE (Dilution_present)
        CASE (Dilution_yes)
          COUT(15) = -1. * C_HX_gas_out                       ! Cross-coupling to HX exhaust gas.
          COUT(16) = C_dilution_exh                           ! Self-coupling.
          COUT(39) = -1. * C_HX_gas_out * T_HX_gas_out
     &               + C_dilution_exh * T_dilution_exh_guess
     &               + deltaH_HX_gas_out
     &               -1. * deltaH_dilution_exh
     &               + deltaH_dilution_in + Dilution_heattoair
     &               + Dilution_fanpower
        CASE (Dilution_no)
          COUT(15) = -1. ! Cross-coupling to HX exhaust gas.
          COUT(16) = 1.  ! Self-coupling.
          COUT(39) = 0.
        END SELECT ! CASE DEFAULT not required because already checked above.
C CV9 (HRV).
        SELECT CASE (HRV_present)
        CASE (SOFC_HRV_yes)
          COUT(17) = HRV_effectiveness*C_HRV_min/C_dilution_exh - 1.
          COUT(18) = 1.
          COUT(40) = HRV_fanpower/C_dilution_exh
     &               + (HRV_effectiveness*C_HRV_min/C_dilution_exh)
     &                  *T_fresh_air
        CASE (SOFC_HRV_no)
          COUT(17) = -1. ! Cross-coupling to dilution air exhaust.
          COUT(18) = 1.  ! Self-coupling.
          COUT(40) = 0.
        END SELECT ! CASE DEFAULT not required because already checked above.

        SELECT CASE (stack_cooling_present)
        CASE (s_cool_yes)
C CV10 (stack cooling loop: water at pump outlet = water flowing into stack HX
C      = state point 21 in model spec)
          COUT(19) = 1.                                       ! self coupling
          COUT(20) = -1.                                      ! cross coupling to stack HX outlet
                                                              ! (state point 22 in model spec)
          COUT(41) = -q_s_cool/Ndot_s_cool_in_cp              ! RHS
C CV11 (stack cooling loop: water at outlet of stack HX)
          COUT(21) = 1.                                       ! self coupling
          COUT(42) = T_stack                                  ! RHS
     &               + 1. / ( 1.-alpha_s_cool_HX )
     &                 * q_s_cool / Ndot_s_cool_in_cp
C CV12 (stack cooling loop: water at outlet of external HX/air cooler inlet
C       = state point 23 in model spec)
          SELECT CASE (cogen_HX_state)
          CASE (no_bypass)
            COUT(22) = 0.                                     ! cross-coupling to stack HX inlet
            COUT(23) = -1. * ( 1. - beta_cogen_HX )           ! cross-coupling to stack HX outlet
     &                 / ( alpha_cogen_HX - beta_cogen_HX )
            COUT(24) = 1.                                     ! self-coupling
            COUT(43) =   ( alpha_cogen_HX - 1. )              ! RHS (cannot cross-couple to T_25 since
     &                 / ( alpha_cogen_HX - beta_cogen_HX )   ! CV12 is not physically connected to T_25).
     &                 * T_25
          CASE (bypass)
            COUT(22) = -1.                                    ! cross-coupling to stack HX inlet
            COUT(23) = 0.                                     ! cross-coupling to stack HX outlet
            COUT(24) = 1.                                     ! self-coupling
            COUT(43) =  ( s_cool_pump_heat_loss - 1. )        ! RHS
     &                * s_cool_pump_Pel / Ndot_s_cool_in_cp
          CASE (no_flow)
            COUT(22) = 0.                                     ! cross-coupling to stack HX inlet
            COUT(23) = -1.                                    ! cross-coupling to stack HX outlet
            COUT(24) = 1.                                     ! self-coupling
            COUT(43) = 0.                                     ! RHS
          END SELECT
C CV13 (stack cooling loop: outlet temperature of air cooler
C       = state point 24 in model spec)
          COUT(25) = -1.                                      ! cross-coupling to stack HX inlet
          COUT(26) = 1.                                       ! self-coupling
          COUT(44) =  ( s_cool_pump_heat_loss - 1. )          ! RHS
     &               * s_cool_pump_Pel / Ndot_s_cool_in_cp
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
          SELECT CASE (cogen_HX_state)
          CASE (no_flow)
            COUT(27) = 0.                                        ! cross-coupling to stack_HX outlet (T_22)
            COUT(28) = 0.                                        ! cross-coupling to cogen_HX outlet (stack loop side) (T_23)
            COUT(29) = 1.                                       ! self-coupling
            COUT(31) = -1.                                      ! cross coupling to cold water inlet
            COUT(45) = 0.                                       ! RHS
          CASE DEFAULT
            COUT(27) = -beta_cogen_HX                           ! cross-coupling to stack_HX outlet (T_22)
            COUT(28) = beta_cogen_HX                            ! cross-coupling to cogen_HX outlet (stack loop side) (T_23)
            COUT(29) = 1.                                       ! self-coupling
            COUT(31) = -1.                                      ! cross coupling to cold water inlet
            COUT(45) = 0.                                       ! RHS
          END SELECT                                       

        CASE (s_cool_no)
C Stack cooling system is not present: coefficients are set to solve a temperature of -99.
C The energy balance for the cogen-side of the external heat exchanger becomes
C "flow through" in that the temperature of the water exiting is equal to that of
C the water entering.
C CV10  (stack cooling system; stack_HX inlet (T_21))
          COUT(19) = 1.                                       ! self coupling
          COUT(20) = 0.                                       ! cross coupling to stack_HX outlet (T_22) 
          COUT(41) = -99                                      ! RHS
C CV11 (stack cooling system; stack_HX outlet (T_22))
          COUT(21) = 1.                                       ! self coupling
          COUT(42) = -99.                                     !RHS
     &                   
C CV12 (stack cooling system; outlet temperature of cogen_HX (T_23)
          COUT(22) = 0.                                       ! cross-coupling to stack HX inlet
          COUT(23) = 0.                                       ! cross-coupling to stack HX outlet
          COUT(24) = 1.                                       ! self-coupling
          COUT(43) = -99.                                     ! RHS
C CV13 (stack cooling system; outlet temperature of air cooler (T_24)
          COUT(25) = 0.                                       ! cross coupling to stack_HX inlet (T_21)
          COUT(26) = 1.                                       ! self coupling
          COUT(44) = -99.                                     ! RHS
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
          COUT(27) = 0                                        ! cross coupling to stack_HX outlet (T_22)
          COUT(28) = 0                                        ! cross coupling to cogen_HX outlet (T_23)
          COUT(29) = 1.                                       ! self coupling
          COUT(31) = -1.                                      ! cross coupling to cold water inlet
          COUT(45) = 0.                                       ! RHS

        CASE DEFAULT
        END SELECT 


C---------------------------------------------------------------------------------
C Save additional plant data items.  These record the current operational
C state of the SOFC-cogen device into the future time-row variables (PCDATF).
C Once the plant domain converges, these future time-row additional data items
C will be written to the present variables.  In this way, PCDATP records the
C operational state from the previous time-row.
C---------------------------------------------------------------------------------
        PCDATF(IPCOMP,1)  = Pel
        PCDATF(IPCOMP,2)  = Q_HX
        PCDATF(IPCOMP,5)  = sys_op
        PCDATF(IPCOMP,6)  = battery_SOC_f
        PCDATF(IPCOMP,7)  = OFFON_count
        PCDATF(IPCOMP,8)  = OPER_TIME_HRS
C-------Save temperature of CV4 only so that a convergence criterion can be
C-------set. Without this, ESP-r will converge once the relative change in CV4's
C-------temperature is less than 1% (by default). Due to CV4's high temperature
C-------this can cause the iteration to end prematurely. The same effect could
C-------be achieved by setting PERREL to a lower value (e.g. 0.001) but this
C-------requires the use of a simulation toggle, and hence makes running bps
C-------in script mode more difficult.
        PCDATF(IPCOMP,3)  = T_FCPM_exh_guess
        PCDATF(IPCOMP,4)  = deltaH_FCPM_exh


C*********************************************************************************
C*********************************************************************************
C End of energy balances for NORMAL operation / Beginning of energy balances for
C STARTING_UP.
C*********************************************************************************
C*********************************************************************************
      ELSE IF(ISTATS==1 .and. sys_op==STARTING_UP) THEN


C---------------------------------------------------------------------------------
C The FCPM's electrical efficiency is not pertinent in this operating mode.
C Set to zero for reporting purposes.
C---------------------------------------------------------------------------------
        eta_el = 0.


C---------------------------------------------------------------------------------
C Determine the AC power output demanded from the SOFC-cogeneration device.
C CDATA(IPCOMP,1) is the control signal (coming from an external controller) that
C indicates the fraction of full load power requested.
C---------------------------------------------------------------------------------
        AC_demand_net = FCPM_elec_max * CDATA(IPCOMP,1)  ! W


C---------------------------------------------------------------------------------
C Determine the following based upon the user inputs that characterize the
C start-up period: the FCPM's DC output; the FCPM's rate of fuel consumption;
C the AC power supplied to the FCPM.  All three are assumed to be constant
C throughout the start-up period.  Refer to section 5.4 of the model spec.
C---------------------------------------------------------------------------------
        Pel            = FCPM_elec_prod_startup / FCPM_dt_startup * 1.E6  ! W
        Ndot_FCPM_fuel = FCPM_fuel_cons_startup / FCPM_dt_startup         ! kmol/s
        Pel_ancAC      = FCPM_elec_cons_startup / FCPM_dt_startup * 1.E6  ! W


C---------------------------------------------------------------------------------
C There is no interaction with the battery during start-up.  If this is the first
C time-step of the simulation then the initial SOC must be set.  Otherwise, set
C the SOC at the end of the time-step to that at the beginning.
C---------------------------------------------------------------------------------
C-------Battery's SOC at beginning of time-step. Initialize to user-input
C-------if this is first time-step of simulation.
        IF (NSINC == 1) THEN
          battery_SOC_p = battery_SOC_initial
        ELSE
          battery_SOC_p = PCDATP(IPCOMP,6)
        ENDIF
        battery_SOC_f  = battery_SOC_p


C---------------------------------------------------------------------------------
C Calculate the PCU losses and then the AC power flowing out of the PCU (the
C gross AC production).
C---------------------------------------------------------------------------------
        DC_into_PCU = Pel
        eta_PCU    = PCU_u0 + PCU_u1 * DC_into_PCU
     &               + PCU_u2 * DC_into_PCU**2.
        PCU_losses = (1.-eta_PCU) * DC_into_PCU
        AC_production_gross = DC_into_PCU - PCU_losses


C---------------------------------------------------------------------------------
C Calculate the net AC production from the cogen device and communicate this to
C the electrical modelling domain.
C---------------------------------------------------------------------------------
        AC_production_net = AC_production_gross - Pel_ancAC
        pwrp(ipcomp) = AC_production_net     ! Real power (W).
        IEMODEL=1
        CALL EMACH( IPCOMP,IEMODEL,pwrp(ipcomp),PQ,PA)


C---------------------------------------------------------------------------------
C The cogen device produces no thermal output. Set to zero for reporting purposes.
C---------------------------------------------------------------------------------
        Q_HX_sensible = 0.
        Q_HX_latent   = 0.
        Q_HX          = 0.


C---------------------------------------------------------------------------------
C The cogen device produces no thermal output during start-up.  Additionally,
C the parasitic heat losses to the containing room and air drawn from the
C containing room are ignored during start-up.  Consequently, the
C energy balances become trivial.  Those representing the blower, compressor,
C water pump, FCPM, auxiliary burner, gas-side of gas-to-water heat exchanger,
C and gas side of the dilution air/HRV are meaningless: coefficients are set to
C solve to a temperature of -99.  The energy balance for the water-side of the
C gas-to-water heat exchanger, the PEM cooling loop heat exchanger, and the
C air-side of the HRV  becomes "flow through" in that the temperature of the water
C or air exiting is equal to that of the water or air entering.
C---------------------------------------------------------------------------------
C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(32) = -99.
C CV2 (fuel within compressor).
        COUT( 2) = 1.
        COUT(33) = -99.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(34) = -99.
C CV4 (gas within FCPM).
        COUT( 4) = 0.
        COUT( 5) = 0.
        COUT( 6) = 0.
        COUT( 7) = 1.
        COUT(35) = -99.
C CV5 (gas within burner).
        COUT( 8) = 0.
        COUT( 9) = 1.
        COUT(36) = -99.
C CV6 (water-side of HX).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(30) = -1.
        COUT(37) = 0.
C CV7 (gas-side of HX).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(38) = -99.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(39) = 99.
C CV9 (HRV).
        COUT(17) = -1.
        COUT(18) = 1.
        COUT(40) = 0.
C CV10  (stack cooling system; stack_HX inlet (T_21))
        COUT(19) = 1.                                       ! self coupling
        COUT(20) = 0.                                       ! cross coupling to stack_HX outlet (T_22) 
        COUT(41) = -99.                                     ! RHS
C CV11 (stack cooling system; stack_HX outlet (T_22))
        COUT(21) = 1.                                       ! self coupling
        COUT(42) = -99.                                     !RHS
C CV12 (stack cooling system; outlet temperature of cogen_HX (T_23)
        COUT(22) = 0.                                       ! cross-coupling to stack HX inlet
        COUT(23) = 0.                                       ! cross-coupling to stack HX outlet
        COUT(24) = 1.                                       ! self-coupling
        COUT(43) = -99.                                     ! RHS
C CV13 (stack cooling system; outlet temperature of air cooler (T_24)
        COUT(25) = 0.                                       ! cross coupling to stack_HX inlet (T_21)
        COUT(26) = 1                                        ! self coupling
        COUT(44) = -99                                      ! RHS
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
        COUT(27) = 0                                        ! cross coupling to stack_HX outlet (T_22)
        COUT(28) = 0                                        ! cross coupling to cogen_HX outlet (T_23)
        COUT(29) = 1.                                       ! self coupling
        COUT(31) = -1.                                      ! cross coupling to cold water inlet
        COUT(45) = 0.                                       ! RHS


C---------------------------------------------------------------------------------
C Save additional plant data items.
C---------------------------------------------------------------------------------
        PCDATF(IPCOMP,1)  = Pel
        PCDATF(IPCOMP,5)  = sys_op
        PCDATF(IPCOMP,6)  = battery_SOC_f
        PCDATF(IPCOMP,7)  = OFFON_count
        PCDATF(IPCOMP,8)  = OPER_TIME_HRS


C*********************************************************************************
C*********************************************************************************
C End of energy balances for STARTING_UP operation / Beginning of energy balances
C for SHUTTING_DOWN.
C*********************************************************************************
C*********************************************************************************
      ELSE IF(ISTATS==1 .and. sys_op==SHUTTING_DOWN) THEN


C---------------------------------------------------------------------------------
C The FCPM's electrical efficiency is not pertinent in this operating mode.
C Set to zero for reporting purposes.
C---------------------------------------------------------------------------------
        eta_el = 0.


C---------------------------------------------------------------------------------
C For reporting purposes set the AC power output demanded from the
C SOFC-cogeneration device to zero.
C---------------------------------------------------------------------------------
        AC_demand_net = 0.


C---------------------------------------------------------------------------------
C Determine the following based upon the user inputs that characterize the
C shutting down period: the FCPM's rate of fuel consumption; the AC power supplied
C to the FCPM.  Both are assumed to be constant throughout the start-up period.
C Refer to section 5.4 of the model spec.
C---------------------------------------------------------------------------------
        Pel            = 0.
        Ndot_FCPM_fuel = FCPM_fuel_cons_cooldown / FCPM_dt_cooldown  ! kmol/s
        Pel_ancAC      = FCPM_elec_cons_cooldown / FCPM_dt_cooldown  ! W
     &                   * 1.E6


C---------------------------------------------------------------------------------
C There is no interaction with the battery while shutting down.  If this is the
C first time-step of the simulation then the initial SOC must be set.  Otherwise,
C set the SOC at the end of the time-step to that at the beginning.
C---------------------------------------------------------------------------------
C-------Battery's SOC at beginning of time-step. Initialize to user-input
C-------if this is first time-step of simulation.
        IF (NSINC == 1) THEN
          battery_SOC_p = battery_SOC_initial
        ELSE
          battery_SOC_p = PCDATP(IPCOMP,6)
        ENDIF
        battery_SOC_f  = battery_SOC_p


C---------------------------------------------------------------------------------
C Calculate the PCU losses and then the AC power flowing out of the PCU (the
C gross AC production).
C---------------------------------------------------------------------------------
        DC_into_PCU = Pel
        eta_PCU    = PCU_u0 + PCU_u1 * DC_into_PCU
     &               + PCU_u2 * DC_into_PCU**2.
        PCU_losses = (1.-eta_PCU) * DC_into_PCU
        AC_production_gross = DC_into_PCU - PCU_losses


C---------------------------------------------------------------------------------
C Calculate the net AC production from the cogen device and communicate this to
C the electrical modelling domain.
C---------------------------------------------------------------------------------
        AC_production_net = AC_production_gross - Pel_ancAC
        pwrp(ipcomp) = AC_production_net     ! Real power (W).
        IEMODEL=1
        CALL EMACH( IPCOMP,IEMODEL,pwrp(ipcomp),PQ,PA)


C---------------------------------------------------------------------------------
C The cogen device produces no thermal output. Set to zero for reporting purposes.
C---------------------------------------------------------------------------------
        Q_HX_sensible = 0.
        Q_HX_latent   = 0.
        Q_HX          = 0.


C---------------------------------------------------------------------------------
C The cogen device is assumed to produce no thermal output during shutdown.
C Additionally, the parasitic heat losses to the containing room and air drawn
C from the containing room are ignored during the shutdown.  Consequently, the
C energy balances become trivial.  Those representing the blower, compressor,
C water pump, FCPM, auxiliary burner, gas-side of gas-to-water heat exchanger,
C and gas side of the dilution air/HRV are meaningless: coefficients are set to
C solve to a temperature of -99.  The energy balance for the water-side of the
C gas-to-water heat exchanger, the PEM cooling loop heat exchanger, and the
C air-side of the HRV  becomes "flow through" in that the temperature of the water
C or air exiting is equal to that of the water or air entering.
C---------------------------------------------------------------------------------
C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(32) = -99.
C CV2 (fuel within compressor).
        COUT( 2) = 1.
        COUT(33) = -99.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(34) = -99.
C CV4 (gas within FCPM).
        COUT( 4) = 0.
        COUT( 5) = 0.
        COUT( 6) = 0.
        COUT( 7) = 1.
        COUT(35) = -99.
C CV5 (gas within burner).
        COUT( 8) = 0.
        COUT( 9) = 1.
        COUT(36) = -99.
C CV6 (water-side of HX).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(30) = -1.
        COUT(37) = 0.
C CV7 (gas-side of HX).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(38) = -99.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(39) = 99.
C CV9 (HRV).
        COUT(17) = -1.
        COUT(18) = 1.
        COUT(40) = 0.
C CV10  (stack cooling system; stack_HX inlet (T_21))
        COUT(19) = 1.                                       ! self coupling
        COUT(20) = 0.                                       ! cross coupling to stack_HX outlet (T_22) 
        COUT(41) = -99.                                     ! RHS
C CV11 (stack cooling system; stack_HX outlet (T_22))
        COUT(21) = 1.                                       ! self coupling
        COUT(42) = -99.                                     ! RHS
C CV12 (stack cooling system; outlet temperature of cogen_HX (T_23)
        COUT(22) = 0.                                       ! cross-coupling to stack HX inlet
        COUT(23) = 0.                                       ! cross-coupling to stack HX outlet
        COUT(24) = 1.                                       ! self-coupling
        COUT(43) = -99.                                     ! RHS
C CV13 (stack cooling system; outlet temperature of air cooler (T_24)
        COUT(25) = 0.                                       ! cross coupling to stack_HX inlet (T_21)
        COUT(26) = 1                                        ! self coupling
        COUT(44) = -99                                      ! RHS
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
        COUT(27) = 0                                        ! cross coupling to stack_HX outlet (T_22)
        COUT(28) = 0                                        ! cross coupling to cogen_HX outlet (T_23)
        COUT(29) = 1.                                       ! self coupling
        COUT(31) = -1.                                      ! cross coupling to cold water inlet
        COUT(45) = 0.                                       ! RHS


C---------------------------------------------------------------------------------
C Save additional plant data items.
C---------------------------------------------------------------------------------
        PCDATF(IPCOMP,1)  = Pel
        PCDATF(IPCOMP,5)  = sys_op
        PCDATF(IPCOMP,6)  = battery_SOC_f
        PCDATF(IPCOMP,7)  = OFFON_count
        PCDATF(IPCOMP,8)  = OPER_TIME_HRS


C*********************************************************************************
C*********************************************************************************
C End of energy balances for SHUTTING_DOWN operation / Beginning of energy
C balances for COLD (i.e. the cogen device is inoperative).
C*********************************************************************************
C*********************************************************************************
      ELSE IF(ISTATS==1 .and. sys_op==COLD) THEN


C---------------------------------------------------------------------------------
C The FCPM's electrical efficiency is not pertinent in this operating mode.
C Set to zero for reporting purposes.
C---------------------------------------------------------------------------------
        eta_el = 0.


C---------------------------------------------------------------------------------
C For reporting purposes set the AC power output demanded from the
C SOFC-cogeneration device to zero.
C---------------------------------------------------------------------------------
        AC_demand_net = 0.


C---------------------------------------------------------------------------------
C No energy is produced or consumed.
C---------------------------------------------------------------------------------
        Pel            = 0.
        Ndot_FCPM_fuel = 0.
        Pel_ancAC      = 0.


C---------------------------------------------------------------------------------
C There is no interaction with the battery.  If this is the first time-step of
C the simulation then the initial SOC must be set.  Otherwise, set the SOC at the
C end of the time-step to that at the beginning.
C---------------------------------------------------------------------------------
C-------Battery's SOC at beginning of time-step. Initialize to user-input
C-------if this is first time-step of simulation.
        IF (NSINC == 1) THEN
          battery_SOC_p = battery_SOC_initial
        ELSE
          battery_SOC_p = PCDATP(IPCOMP,6)
        ENDIF
        battery_SOC_f  = battery_SOC_p


C---------------------------------------------------------------------------------
C There are no losses from the PC and no AC production.
C---------------------------------------------------------------------------------
        PCU_losses          = 0.
        AC_production_gross = 0.
        AC_production_net   = 0.


C---------------------------------------------------------------------------------
C Communicate the AC production to the electrical modelling domain.
C---------------------------------------------------------------------------------
        pwrp(ipcomp) = AC_production_net     ! Real power (W).
        IEMODEL=1
        CALL EMACH( IPCOMP,IEMODEL,pwrp(ipcomp),PQ,PA)


C---------------------------------------------------------------------------------
C The cogen device produces no thermal output. Set to zero for reporting purposes.
C---------------------------------------------------------------------------------
        Q_HX_sensible = 0.
        Q_HX_latent   = 0.
        Q_HX          = 0.


C---------------------------------------------------------------------------------
C The cogen device produces no thermal output.  Consequently, the
C energy balances become trivial.  Those representing the blower, compressor,
C water pump, FCPM, auxiliary burner, gas-side of gas-to-water heat exchanger,
C and gas side of the dilution air/HRV are meaningless: coefficients are set to
C solve to a temperature of -99.  The energy balance for the water-side of the
C gas-to-water heat exchanger, the PEM cooling loop heat exchanger, and the
C air-side of the HRV  becomes "flow through" in that the temperature of the water
C or air exiting is equal to that of the water or air entering.
C---------------------------------------------------------------------------------
C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(32) = -99.
C CV2 (fuel within compressor).
        COUT( 2) = 1.
        COUT(33) = -99.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(34) = -99.
C CV4 (gas within FCPM).
        COUT( 4) = 0.
        COUT( 5) = 0.
        COUT( 6) = 0.
        COUT( 7) = 1.
        COUT(35) = -99.
C CV5 (gas within burner).
        COUT( 8) = 0.
        COUT( 9) = 1.
        COUT(36) = -99.
C CV6 (water-side of HX).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(30) = -1.
        COUT(37) = 0.
C CV7 (gas-side of HX).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(38) = -99.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(39) = 99.
C CV9 (HRV).
        COUT(17) = -1.
        COUT(18) = 1.
        COUT(40) = 0.
C CV10  (stack cooling system; stack_HX inlet (T_21))
        COUT(19) = 1.                                       ! self coupling
        COUT(20) = 0.                                       ! cross coupling to stack_HX outlet (T_22) 
        COUT(41) = -99.                                     ! RHS
C CV11 (stack cooling system; stack_HX outlet (T_22))
        COUT(21) = 1.                                       ! self coupling
        COUT(42) = -99.                                     ! RHS
C CV12 (stack cooling system; outlet temperature of cogen_HX (T_23)
        COUT(22) = 0.                                       ! cross-coupling to stack HX inlet
        COUT(23) = 0.                                       ! cross-coupling to stack HX outlet
        COUT(24) = 1.                                       ! self-coupling
        COUT(43) = -99.                                     ! RHS
C CV13 (stack cooling system; outlet temperature of air cooler (T_24)
        COUT(25) = 0.                                       ! cross coupling to stack_HX inlet (T_21)
        COUT(26) = 1                                        ! self coupling
        COUT(44) = -99                                        ! RHS
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
        COUT(27) = 0                                        ! cross coupling to stack_HX outlet (T_22)
        COUT(28) = 0                                        ! cross coupling to cogen_HX outlet (T_23)
        COUT(29) = 1.                                       ! self coupling
        COUT(31) = -1.                                      ! cross coupling to cold water inlet
        COUT(45) = 0.                                       ! RHS


C---------------------------------------------------------------------------------
C Save additional plant data items.
C---------------------------------------------------------------------------------
        PCDATF(IPCOMP,1)  = Pel
        PCDATF(IPCOMP,5)  = sys_op
        PCDATF(IPCOMP,6)  = battery_SOC_f
        PCDATF(IPCOMP,7)  = OFFON_count
        PCDATF(IPCOMP,8)  = OPER_TIME_HRS


C*********************************************************************************
C*********************************************************************************
C End of energy balances section / Beginning of 1st phase mass balances. *********
C*********************************************************************************
C*********************************************************************************
      ELSE IF(ISTATS==2) THEN

C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(32) = 0.
C CV1 (gas within compressor).
        COUT( 2) = 1.
        COUT(33) = 0.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(34) = 0.
C CV4 (gas within FCPM).
        COUT( 4) = 0.   ! Coupling to CV1.
        COUT( 5) = 0.   ! Coupling to CV2.
        COUT( 6) = 0.   ! Coupling to CV3.
        COUT( 7) = 1.   ! Self-coupling.
        COUT(35) = 0.
C CV5 (gas within burner).
        COUT( 8) = 0.   ! Cross-coupling to FCPM gas.
        COUT( 9) = 1.   ! Self-coupling.
        COUT(36) = 0.
C CV6 (HX-water).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(30) =  - PCONDR(con_CV6)
        COUT(37) = 0.
C CV7 (HX-gas).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(38) = 0.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(39) = 0.
C CV9 (HRV).
        COUT(17) = -1.
        COUT(18) = 1.
        COUT(40) = 0.
C CV10  (stack cooling system; stack_HX inlet (T_21))
        COUT(19) = 1.
        COUT(20) = 0.
        COUT(41) = 0.
C CV11 (stack cooling system; stack_HX outlet (T_22))
        COUT(21) = 1.
        COUT(42) = 0.
C CV12 (stack cooling system; outlet temperature of cogen_HX (T_23)
        COUT(22) = 0.
        COUT(23) = 0.
        COUT(24) = 1.
        COUT(43) = 0.
C CV13 (stack cooling system; outlet temperature of air cooler (T_24)
        COUT(25) = 0.
        COUT(26) = 1.
        COUT(44) = 0.
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
        COUT(27) = 0.
        COUT(28) = 0.
        COUT(29) = 1.
        COUT(31) = - PCONDR(con_CV14)
        COUT(45) = 0.


C*********************************************************************************
C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances. **
C*********************************************************************************
C*********************************************************************************
      ELSE IF(ISTATS==3) THEN

C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(32) = 0.
C CV1 (gas within compressor).
        COUT( 2) = 1.
        COUT(33) = 0.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(34) = 0.
C CV4 (gas within FCPM).
        COUT( 4) = 0.   ! Coupling to CV1.
        COUT( 5) = 0.   ! Coupling to CV2.
        COUT( 6) = 0.   ! Coupling to CV3.
        COUT( 7) = 1.   ! Self-coupling.
        COUT(35) = 0.
C CV5 (gas within burner).
        COUT( 8) = 0.   ! Cross-coupling to FCPM gas.
        COUT( 9) = 1.   ! Self-coupling.
        COUT(36) = 0.
C CV6 (HX-water).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(30) = 0.
        COUT(37) = 0.
C CV7 (HX-gas).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(38) = 0.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(39) = 0.
C CV9 (HRV).
        COUT(17) = -1.
        COUT(18) = 1.
        COUT(40) = 0.
C CV10  (stack cooling system; stack_HX inlet (T_21))
        COUT(19) = 1.
        COUT(20) = 0.
        COUT(41) = 0.
C CV11 (stack cooling system; stack_HX outlet (T_22))
        COUT(21) = 1.
        COUT(42) = 0.
     &                   
C CV12 (stack cooling system; outlet temperature of cogen_HX (T_23)
        COUT(22) = 0.
        COUT(23) = 0.
        COUT(24) = 1.
        COUT(43) = 0.
C CV13 (stack cooling system; outlet temperature of air cooler (T_24)
        COUT(25) = 0.
        COUT(26) = 1.
        COUT(44) = 0.
C CV14 (stack cooling system; outlet temperature of cogen HX (T_26))
        COUT(27) = 0.
        COUT(28) = 0.
        COUT(29) = 1.
        COUT(31) = 0.
        COUT(45) = 0.


      END IF
C*********************************************************************************
C*********************************************************************************
C End of energy and mass balances.
C*********************************************************************************
C*********************************************************************************


C---------------------------------------------------------------------------------
C Make select results available in XML and CVS output.
C---------------------------------------------------------------------------------
      IF (ISTATS==1) THEN
C-------FCPM.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/A_Pel'
        call add_to_xml_reporting( Pel, char_temp, 'units', '(W)',
     &        'SOFC-A42: DC power delivered by FCPM' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/eta_el'
        call add_to_xml_reporting( eta_el, char_temp, 'units', '(%LHV)',
     &        'SOFC-A42: Electrical efficiency of FCPM' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPM_fuel'
        call add_to_xml_reporting( Ndot_FCPM_fuel, char_temp, 'units',
     &        '(kmol/s)',
     &        'SOFC-A42: Fuel supply to FCPM' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPM_air'
        call add_to_xml_reporting( Ndot_FCPM_air, char_temp, 'units',
     &        '(kmol/s)','SOFC-A42: Air supply to FCPM' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/lambda_FCPM'
        call add_to_xml_reporting( lambda_FCPM, char_temp, 'units',
     &        '(-)',
     &        'SOFC-A42: FCPM excess air ratio' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Pel_ancAC'
        call add_to_xml_reporting( Pel_ancAC, char_temp, 'units',
     &        '(W)',
     &        'SOFC-A42: FCPM AC ancillaries' )
C-------Auxiliary burner.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_auxburn_fuel'
        call add_to_xml_reporting( Ndot_auxburn_fuel, char_temp,
     &        'units', '(kmol/s)',
     &        'SOFC-A42: Fuel supply to auxiliary burner' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_auxburn_air'
        call add_to_xml_reporting( Ndot_auxburn_air, char_temp,
     &        'units', '(kmol/s)',
     &        'SOFC-A42: Air supply to auxiliary burner' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Q_auxburn'
        call add_to_xml_reporting( Q_auxburn, char_temp,
     &        'units', '(W)',
     &        'SOFC-A42: Heat output of auxiliary burner' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/auxburn_Pel'
        call add_to_xml_reporting( auxburn_Pel, char_temp,
     &        'units', '(W)',
     &        'SOFC-A42: Elec draw of auxiliary burner ancillaries' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/auxburn_skin_loss'
        call add_to_xml_reporting( auxburn_skin_loss, char_temp,
     &        'units', '(W)',
     &        'SOFC-A42: Skin losses from auxiliary burner' )
C-------Heat exchanger.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Q_HX'
        call add_to_xml_reporting( Q_HX, char_temp, 'units', '(W)',
     &        'SOFC-A42: Heat recovery to water' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Q_HX_sensible'
        call add_to_xml_reporting( Q_HX_sensible, char_temp, 'units',
     &       '(W)', 'SOFC-A42: Sensible eat recovery to water' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Q_HX_latent'
        call add_to_xml_reporting( Q_HX_latent, char_temp, 'units',
     &        '(W)', 'SOFC-A42: Latent heat recovery to water' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/C_water'
        call add_to_xml_reporting( C_water, char_temp, 'units', '(W/K)',
     &        'Heat capacity rate of water in HX' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/C_gas'
        call add_to_xml_reporting( C_gas, char_temp, 'units', '(W/K)',
     &        'Heat capacity rate of gas in HX' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/UA_HX'
        call add_to_xml_reporting( UA_HX, char_temp, 'units', '(W/K)',
     &        'Effective UA of HX' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/HX_Ndot_condense'
        call add_to_xml_reporting( HX_Ndot_condense, char_temp,
     &        'units', '(kmol/s)',
     &        'Condensation rate in HX' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/HX_water_vap_frac'
        call add_to_xml_reporting( HX_water_vap_frac, char_temp,
     &        'units', '(-)',
     &        'Fraction of water vapour in exhaust' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/HX_Hfg'
        call add_to_xml_reporting( HX_Hfg, char_temp,
     &        'units', '(J/kmol)',
     &        'Latent heat of vapourization of water in HX' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_HX_gas'
        call add_to_xml_reporting( Ndot_HX_gas, char_temp,
     &        'units', '(kmol/s)',
     &        'Molar flow of gas through HX' )
C-------Inter-model comparisons.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/LHV_fuel'
        call add_to_xml_reporting( LHV_fuel, char_temp, 'units',
     &        '(J/kmol)', 'SOFC-A42: Fuel LHV' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/deltaH_FCPM_fuel'
        call add_to_xml_reporting( deltaH_FCPM_fuel, char_temp, 'units',
     &        '(W)', 'SOFC-A42: Enthalpy flow rate of fuel to FCPM' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/deltaH_FCPM_air'
        call add_to_xml_reporting( deltaH_FCPM_air, char_temp, 'units',
     &        '(W)', 'SOFC-A42: Enthalpy flow rate of air to FCPM' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPMexh_N2'
        call add_to_xml_reporting( Ndot_FCPMexh_N2, char_temp, 'units',
     &        '(kmol/s)', 'SOFC-A42: N2 exh' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPMexh_Ar'
        call add_to_xml_reporting( Ndot_FCPMexh_Ar, char_temp, 'units',
     &        '(kmol/s)', 'SOFC-A42: Ar exh' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPMexh_O2'
        call add_to_xml_reporting( Ndot_FCPMexh_O2, char_temp, 'units',
     &        '(kmol/s)', 'SOFC-A42: O2 exh' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPMexh_CO2'
        call add_to_xml_reporting( Ndot_FCPMexh_CO2, char_temp, 'units',
     &        '(kmol/s)', 'SOFC-A42: CO2 exh' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPMexh_H2O'
        call add_to_xml_reporting( Ndot_FCPMexh_H2O, char_temp, 'units',
     &        '(kmol/s)', 'SOFC-A42: H2O exh' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/deltaH_FCPM_exh'
        call add_to_xml_reporting( deltaH_FCPM_exh, char_temp, 'units',
     &        '(kmol/s)', 'SOFC-A42: deltaH_FCPM_exh' )
C-------Blower, compressor, pump.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/blower_Pel'
        call add_to_xml_reporting( blower_Pel, char_temp, 'units',
     &        '(W)', 'SOFC-A42: blower_Pel' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/T_blower_in'
        call add_to_xml_reporting( T_blower_in, char_temp, 'units',
     &        '(oC)', 'SOFC-A42: T_blower_in' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/comp_Pel'
        call add_to_xml_reporting( comp_Pel, char_temp, 'units',
     &        '(W)', 'SOFC-A42: comp_Pel' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/pump_Pel'
        call add_to_xml_reporting( pump_Pel, char_temp, 'units',
     &        '(W)', 'SOFC-A42: pump_Pel' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPM_liqwater'
        call add_to_xml_reporting( Ndot_FCPM_liqwater, char_temp,
     &        'units', '(kmol/s)', 'SOFC-A42: Ndot_FCPM_liqwater' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/deltaH_FCPM_liqwater'
        call add_to_xml_reporting( deltaH_FCPM_liqwater, char_temp,
     &        'units', '(W)', 'SOFC-A42: deltaH_FCPM_liqwater' )
C-------Battery and PCU.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/battery_SOC_p'
        call add_to_xml_reporting( battery_SOC_p, char_temp,
     &        'units', '(-)', 'SOFC-A42: battery_SOC_p' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/battery_SOC_f'
        call add_to_xml_reporting( battery_SOC_f, char_temp,
     &        'units', '(-)', 'SOFC-A42: battery_SOC_f' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/DC_required_by_PCU'
        call add_to_xml_reporting( DC_required_by_PCU, char_temp,
     &        'units', '(W)', 'SOFC-A42: DC_required_by_PCU' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/DC_into_PCU'
        call add_to_xml_reporting( DC_into_PCU, char_temp,
     &        'units', '(W)', 'SOFC-A42: DC_into_PCU' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/P_battery'
        call add_to_xml_reporting( P_battery, char_temp,
     &        'units', '(W)', 'SOFC-A42: P_battery' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/AC_demand_net'
        call add_to_xml_reporting( AC_demand_net, char_temp,
     &        'units', '(W)', 'SOFC-A42: AC_demand_net' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/AC_production_gross'
        call add_to_xml_reporting( AC_production_gross, char_temp,
     &        'units', '(W)', 'SOFC-A42: AC_production_gross' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/AC_production_net'
        call add_to_xml_reporting( AC_production_net, char_temp,
     &        'units', '(W)', 'SOFC-A42: AC_production_net' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/PCU_losses'
        call add_to_xml_reporting( PCU_losses, char_temp,
     &        'units', '(W)', 'SOFC-A42: PCU_losses' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/eta_PCU'
        call add_to_xml_reporting( eta_PCU, char_temp,
     &        'units', '(W)', 'SOFC-A42: eta_PCU' )
C-------Stack cooling system
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/q_s_cool'
        call add_to_xml_reporting( q_s_cool, char_temp, 'units',
     &        '(W)', 'SOFC-A42: q_s_cool' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/q_s_cogen'
        call add_to_xml_reporting( q_s_cogen, char_temp, 'units',
     &        '(W)', 'SOFC-A42: q_s_cogen' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/q_s_cool_air'
        call add_to_xml_reporting( q_s_cool_air, char_temp, 'units',
     &        '(W)', 'SOFC-A42: q_s_cool_air' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/P_air_el'
        call add_to_xml_reporting( P_air_el, char_temp, 'units',
     &        '(W)', 'SOFC-A42: P_air_el' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/UA_s_cogen'
        call add_to_xml_reporting( UA_s_cogen, char_temp, 'units',
     &        '(W)', 'SOFC-A42: UA_s_cogen' )
C-------Dilution air system/HRV.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/q_HRV'
        call add_to_xml_reporting( q_HRV, char_temp, 'units',
     &        '(W)', 'SOFC-A42: q_HRV' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/T_vent_air'
        call add_to_xml_reporting( T_vent_air, char_temp, 'units',
     &        '(oC)', 'SOFC-A42: T_vent_air' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Dilution_heattoair'
        call add_to_xml_reporting(Dilution_heattoair,char_temp,'units',
     &        '(W)', 'SOFC-A42: Dilution_heattoair' )
C-------Net efficiency values.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/LHV_to_AC_eff'
        call add_to_xml_reporting( LHV_to_AC_eff, char_temp, 'units',
     &        '(-)', 'SOFC-A42: LHV_to_AC_eff' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/LHV_to_therm_eff'
        call add_to_xml_reporting( LHV_to_therm_eff, char_temp, 'units',
     &        '(-)', 'SOFC-A42: LHV_to_therm_eff' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/LHV_to_cogen_eff'
        call add_to_xml_reporting( LHV_to_cogen_eff, char_temp, 'units',
     &        '(-)', 'SOFC-A42: LHV_to_cogen_eff' )
C-------Heat transfer from FC-cogen unit to containing room.
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/heat_to_room_conv'
        call add_to_xml_reporting( heat_to_room_conv, char_temp,'units',
     &        '(W)', 'heat_to_room_conv' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/heat_to_room_rad'
        call add_to_xml_reporting( heat_to_room_rad, char_temp, 'units',
     &        '(W)', 'heat_to_room_rad' )

      ENDIF


C---------------------------------------------------------------------------------
C Save the `additional' output variables for outputting to res on time-step basis.
C---------------------------------------------------------------------------------
      NAPDAT(IPCOMP)    = 4                    ! number of additional outputs
      PCAOUT(IPCOMP,1)  = Pel
      PCAOUT(IPCOMP,2)  = LHV_to_AC_eff
      PCAOUT(IPCOMP,3)  = LHV_to_therm_eff
      PCAOUT(IPCOMP,4)  = LHV_to_cogen_eff


C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 14 node Annex 42 SOFC-cogen model'
        WRITE(ITU,*) ' Matrix node(s) ',CV1,CV2,CV3,CV4,CV5,CV6,CV7,CV8,
     &               CV9,CV10,CV11,CV12,CV13,CV14
        WRITE(ITU,*) ' Connection(s)  ',con_CV6,con_CV14
        IF(ISTATS.eq.1) THEN
          WRITE(ITU,*) ' CDATA(elec)       = ',CDATA(IPCOMP,1),' (-)'
          WRITE(ITU,*) ' CDATA(thermal)    = ',CDATA(IPCOMP,1),' (-)'
          WRITE(ITU,*) ' Pel               = ',Pel,' (W)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(itemp),itemp=1,45)
        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving subroutine SOFC_A42_coeffgen '
      END IF


      RETURN
      END



C *********************************************************************************************
C ************************************** Shomate **********************************************

C This function applies the Shomate equation to return one of four properties
C for the gas under consideration, depending upon the value of `calcprop':
C  `deltaH'  - The difference between a gas' enthalpy and its enthalpy at the
C              standard state (J/kmol). This is calculated as a function of the
C              supplied temperature.
C  `totalH'  - The gas' enthalpy (J/kmol). This is calculated as a function of
C              the supplied temperature.
C  `LHV_i'   - The LHV (J/kmol). This is determined by evaluating the enthalpies
C              at the standard state (i.e. standard enthalpies of formation) of the
C              the hydrocabon or alcohol molecules and those of CO2 and H2O.
C  `heatcap' - The heat capacity (J/kmolK). This is calculated as a function of
C              the supplied temperature.

C The formulation of the Shomate equation is taken from NIST's Chemistry Webbook
C (webbook.nist.gov/chemistry).  The coefficients for the following gases are taken
C from this same source: N2, O2, Ar, CO2, H2, and CH4.  NIST's Chemistry Webbook
C does not include data for the higher hydrocarbons nor for the alcohol fuels.
C Additionally, correlation for H2O vapour does not span the temperature range of
C interest.  Consequently, an alternate approach was used for these other gases.
C The correlation and coefficients proposed by S. Gordon and B.J. McBride,
C "Computer Program for Calculation of Complex Chemical Equilibrium Composition,
C Rocket Performance, Incident and Reflected Shocks and Chapman-Jouguet Detonations",
C NASA SP-273 (1971) were evaluated over a wide range of temperatures and then
C these data regressed to the form of the Shomate equation to produce the
C coefficients.  This represents a regression of regressed data, so its accuracy
C should be thoroughly examined through detailed testing.

      FUNCTION Shomate(calcprop,tempC,gas,IUOUT)
      IMPLICIT NONE
#include "Annex42_fuel_cell.h"

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      REAL Shomate
      REAL tempC       ! Temperature in C.
      REAL tempK       ! Temperature in K.
      INTEGER gas      ! Index number of gas.
      INTEGER IUOUT    ! Unit number for providing feedback to user.
      REAL A(15)       ! Shomate "A" coefficients for gas "j".
      REAL B(15)       ! Shomate coefficients.
      REAL C(15)       ! Shomate coefficients.
      REAL D(15)       ! Shomate coefficients.
      REAL E(15)       ! Shomate coefficients.
      REAL F(15)       ! Shomate coefficients.
      REAL H(15)       ! Shomate coefficients.
      REAL min_range(15) ! Minimum temperature (K) of range of applicability of correlation.
      REAL max_range(15) ! Maximum temperature (K) of range of applicability of correlation.
      INTEGER j        ! Used as index in DATA statements.
      INTEGER calcprop ! Index of property to calculate.
      REAL xC          ! Number of carbon atoms in fuel molecule.
      REAL yH          ! Number of hydrogen atoms in fuel molecule.
      REAL dH_at_min_range ! Difference in relative enthalpy between temp at min of range and
                           ! that at standard state (J/kmol).
      REAL tH_at_min_range ! Difference in enthalpy between temp at min of range and
                           ! that at standard state (J/kmol).
      REAL Cp_at_min_range ! Heat capacity at temp at min of range of applicability (J/kmolK).

C---------------------------------------------------------------------------------
C Shomate coefficients from NIST web site for N2, O2, Ar, CO2, H2, CH4, and
C liquid H2O (`liq_H2O').  Coefficients for other gases determined by Brent
C Griffith (NREL) using procedure outlined above.  Note that "G" coefficients are
C not required for the calculation of the properties considered by this
C implementation. 
C---------------------------------------------------------------------------------
      DATA ( A(j),j=1,15 ) / 26.092, 29.659, 29.0373, 20.786,
     &                       24.99735, 33.066178, -0.703029,
     &                       -3.03849, -23.1747, -5.24343,
     &                       -34.9431, -46.7786, 14.1952, -8.87256,
     &                       -203.6060 /
      DATA ( B(j),j=1,15 ) / 8.218801, 6.137261, 10.2573, 2.825911E-7,
     &                       55.18696, -11.363417, 108.4773,
     &                       199.202, 363.742, 426.442, 576.777,
     &                       711.187, 97.7218, 282.389, 1523.290 /
      DATA ( C(j),j=1,15 ) / -1.976141, -1.186521, 2.81048,
     &                       -1.464191E-7,-33.69137,11.432816,
     &                       -42.52157,
     &                       -84.9812, -222.981, -257.955, -338.353,
     &                       -438.39, -9.73279, -178.85, -3196.413 /
      DATA ( D(j),j=1,15 ) / 0.159274, 0.09578, -0.959141, 1.092131E-8,
     &                       7.948387, -2.772874, 5.862788,
     &                       11.0348, 56.253, 66.535, 76.8232,
     &                       103.784, -12.8461, 46.3528, 2474.455 /
      DATA ( E(j),j=1,15 ) / 0.044434, -0.219663, 0.117253,
     &                       -3.661371E-8, -0.136638, -0.158558,
     &                       0.678565,
     &                       0.303476, 0.611637, -0.269941, 1.00948,
     &                       1.23887, 0.158186, 0.483644, 3.855326 /
      DATA ( F(j),j=1,15 ) / -7.98923, -9.861391, -250.569, -6.19735,
     &                       -403.6075, -9.980797, -76.84376,
     &                       -90.0633, -109.206, -149.365, -155.348,
     &                       -176.813, -209.037, -241.239, -256.5478 /
      DATA ( H(j),j=1,15 ) / 0., 0., -241.826, 0., -393.5224,
     &                       0., -74.8731,
     &                       -83.8605, -103.855, -133.218, -146.348,
     &                       -166.966, -201.102, -234.441, -285.8304 /

C---------------------------------------------------------------------------------
C Range of applicability of above coefficients.
C---------------------------------------------------------------------------------
      DATA ( min_range(j),j=1,15 ) / 298., 298., 200., 298., 298., 298.,
     &                               298., 300., 300., 300., 300., 300.,
     &                               300., 300., 298. /
      DATA ( max_range(j),j=1,15 ) / 6000., 6000., 1000., 6000., 1200.,
     &                               1000., 1300., 1000., 1000., 1000.,
     &                               1000., 1000., 1000., 1000., 500. /

C---------------------------------------------------------------------------------
C Determine temperature in Kelvin.
C---------------------------------------------------------------------------------
      tempK = tempC + 273.15


C---------------------------------------------------------------------------------
C Issue error message and halt simulation if temperature exceeds maximum of
C range of applicability of the correlations.
C---------------------------------------------------------------------------------
      IF (tempK > max_range(gas) ) THEN
        write(IUOUT,*) ' Temperature > range of applicability'
        write(IUOUT,*) ' of Shomate correlation for gas: ',gas
        write(IUOUT,*) '   : ',tempK,' > ',max_range(gas)
      END IF
      IF (tempK < min_range(gas) ) THEN
        write(IUOUT,*) ' Temperature < range of applicability'
        write(IUOUT,*) ' of Shomate correlation for gas: ',gas
        write(IUOUT,*) '   : ',tempK,' < ',min_range(gas)
      END IF


C---------------------------------------------------------------------------------
C Evaluate the requested property.
C---------------------------------------------------------------------------------
      SELECT CASE (calcprop)

C---------------------------------------------------------------------------------
C Difference between the gas' enthalpy at the supplied temperature and that
C at the standard state.  If temperature is lower than minimum of range of
C applicability of correlation then evaluate enthalpy difference at lowest
C applicable temperature and extrapolate by assuming that heat capacity is
C constant from temperature in question to mimimum temperature of range of
C applicability.
C---------------------------------------------------------------------------------
      CASE (deltaH)
        IF ( tempK < min_range(gas) ) THEN
C---------Extrapolation required.
C---------Determine diff between gas' enthalpy at min of range and that at standard state.
          dH_at_min_range = A(gas) * min_range(gas)/1000.
     &                    + B(gas) / 2. * (min_range(gas)/1000.)**2.
     &                    + C(gas) / 3. * (min_range(gas)/1000.)**3.
     &                    + D(gas) / 4. * (min_range(gas)/1000.)**4.
     &                    - E(gas) / (min_range(gas)/1000.)
     &                    + F(gas)
     &                    - H(gas)
C---------Shomate correlation returns kJ/mol. Convert to J/kmol.
          dH_at_min_range = dH_at_min_range * 1000. * 1000.
C---------Determine heat capacity at min of range.
          Cp_at_min_range = A(gas)
     &                    + B(gas) * (min_range(gas)/1000.)
     &                    + C(gas) * (min_range(gas)/1000.)**2.
     &                    + D(gas) * (min_range(gas)/1000.)**3.
     &                    + E(gas) / (min_range(gas)/1000.)**2.
C---------Shomate correlation returns J/molK. Convert to J/kmolK.
          Cp_at_min_range = Cp_at_min_range * 1000.
C---------Extrapolate enthalpy by assuming Cp constant from tempK to min_range.
          Shomate = dH_at_min_range
     &            - Cp_at_min_range * ( min_range(gas) - tempK )

        ELSE
C---------Within range of applicability.
          Shomate = A(gas) * tempK/1000.
     &            + B(gas) / 2. * (tempK/1000.)**2.
     &            + C(gas) / 3. * (tempK/1000.)**3.
     &            + D(gas) / 4. * (tempK/1000.)**4.
     &            - E(gas) / (tempK/1000.)
     &            + F(gas)
     &            - H(gas)
C---------Shomate correlation returns kJ/mol. Convert to J/kmol.
          Shomate = Shomate * 1000. * 1000.
        END IF


C---------------------------------------------------------------------------------
C Gas' enthalpy at supplied temperature.  If temperature is lower than minimum
C of range of applicability of correlation then evaluate enthalpy at lowest
C applicable temperature and extrapolate by assuming that heat capacity is
C constant from temperature in question to mimimum temperature of range of
C applicability.
C---------------------------------------------------------------------------------
      CASE (totalH)
        IF ( tempK < min_range(gas) ) THEN
C---------Extrapolation required.
C---------Determine diff between gas' enthalpy at min of range and that at standard state.
          tH_at_min_range = A(gas) * min_range(gas)/1000.
     &                    + B(gas) / 2. * (min_range(gas)/1000.)**2.
     &                    + C(gas) / 3. * (min_range(gas)/1000.)**3.
     &                    + D(gas) / 4. * (min_range(gas)/1000.)**4.
     &                    - E(gas) / (min_range(gas)/1000.)
     &                    + F(gas)
C---------Shomate correlation returns kJ/mol. Convert to J/kmol.
          tH_at_min_range = tH_at_min_range * 1000. * 1000.
C---------Determine heat capacity at min of range.
          Cp_at_min_range = A(gas)
     &                    + B(gas) * (min_range(gas)/1000.)
     &                    + C(gas) * (min_range(gas)/1000.)**2.
     &                    + D(gas) * (min_range(gas)/1000.)**3.
     &                    + E(gas) / (min_range(gas)/1000.)**2.
C---------Shomate correlation returns J/molK. Convert to J/kmolK.
          Cp_at_min_range = Cp_at_min_range * 1000.
C---------Extrapolate enthalpy by assuming Cp constant from tempK to min_range.
          Shomate = tH_at_min_range
     &            - Cp_at_min_range * ( min_range(gas) - tempK )

        ELSE
C---------Within range of applicability.
          Shomate = A(gas) * tempK/1000.
     &            + B(gas) / 2. * (tempK/1000.)**2.
     &            + C(gas) / 3. * (tempK/1000.)**3.
     &            + D(gas) / 4. * (tempK/1000.)**4.
     &            - E(gas) / (tempK/1000.)
     &            + F(gas)
C---------Shomate correlation returns kJ/mol. Convert to J/kmol.
          Shomate = Shomate * 1000. * 1000.
        END IF


C---------------------------------------------------------------------------------
C LHV of the gas. First determine the number of carbon and hydrogen atoms
C in the gas. Then evaluate the LHV by evaluating the enthalpy at the standard
C state of the fuel molecule, CO2, and H2O.
C---------------------------------------------------------------------------------
      CASE (LHV_i)
        SELECT CASE (gas)
        CASE(H2)
          xC = 0.
          yH = 2.
        CASE(CH4)
          xC = 1.
          yH = 4.
        CASE(C2H6)
          xC = 2.
          yH = 6.
        CASE(C3H8)
          xC = 3.
          yH = 8.
        CASE(C4H10)
          xC = 4.
          yH = 10.
        CASE(C5H12)
          xC = 5.
          yH = 12.
        CASE(C6H14)
          xC = 6.
          yH = 14.
        CASE(CH3OH)
          xC = 1.
          yH = 4.
        CASE(C2H5OH)
          xC = 2.
          yH = 6.
        CASE DEFAULT
          write(IUOUT,*) ' LHV cannot be calculated for this gas.'
          STOP ' Error in FUNCTION Shomate'
        END SELECT

C-------Shomate correlation returns kJ/mol. Convert to J/kmol.
        Shomate = H(gas) - xC*H(CO2) - yH/2.*H(H2O)
        Shomate = Shomate * 1000. * 1000.


C---------------------------------------------------------------------------------
C Gas' heat capacity.  If temperature is lower than minimum of range of
C applicability of correlation then evaluate heat capacity at the minimum of range
C of applicability.  This inherently assumes that there is negligible variation
C in the heat capacity between the temperature of interest and the minimum of
C range of applicability.
C---------------------------------------------------------------------------------
      CASE (heatcap)
        IF ( tempK < min_range(gas) ) THEN
C---------Evaluate heat capacity at min range of applicability of correlation.
          tempK = min_range(gas)
        END IF
        Shomate = A(gas)
     &          + B(gas) * (tempK/1000.)
     &          + C(gas) * (tempK/1000.)**2.
     &          + D(gas) * (tempK/1000.)**3.
     &          + E(gas) / (tempK/1000.)**2.
C-------Shomate correlation returns J/molK. Convert to J/kmolK.
        Shomate = Shomate * 1000.


C---------------------------------------------------------------------------------
      CASE DEFAULT
        write(IUOUT,*) ' Implementation of Shomate equation does not '
        write(IUOUT,*) ' support calculation of this property.'
        STOP ' Error in FUNCTION Shomate'

      END SELECT

      RETURN
      END
