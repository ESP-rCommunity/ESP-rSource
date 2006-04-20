C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
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
C *******************************************************************************
C References:
C
C    Beausoleil-Morrison I. (2005), Model Specification for an SOFC-Cogeneration
C    Device, IEA/ECBCS Annex 42 Working Document, Revision of March 2, 2005.
C *********************************************************************************************


C *********************************************************************************************
C ********************************* SOFC_A42_static *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: March 2005
C Copyright CETC 2005

C This subroutine is the static template for the Annex 42 SOFC-cogen component model.
C It checks whether the component is properly used in the user-defined plant network,
C it assigns user-input data to module variables, it performs time-invariant
C calculations, and it checks the integrity of the user-specified electric
C loads data file.

C The SOFC-cogen device is represented with eight control volumes (aka nodes):
C   CV1 - The air within the air supply blower.
C   CV2 - The gas within the fuel compressor.
C   CV3 - The water within the water pump.
C   CV4 - The gas within the fuel cell power module (FCPM).
C   CV5 - The gas within the auxiliary burner.
C   CV6 - The water within and the solid portion of the heat recovery device.
C   CV7 - The gas within the heat recovery device.
C   CV8 - The gas within the dilution air system/HRV.

C INPUTS:
C    IPCOMP     index number of the component under consideration
C    NPCOMP     number of plant components in user-defined plant network
C    NCI(i)     holds the number of possible plant control variables for plant component `i'
C    ADATA(i,j) holds `j'th data item for component `i' read from .pln file
C               (derived from plant components database)
C    ICONTP(i)  holds the state variable index (ISV) of the sending node for connection `i'.
C    IUOUT      output channel to feedback to user
C    ITC        trace output index (>0 gives a trace output)
C    ITU        output channel for trace output
C    ITRACE(35) flag indicating one of the plant traces (1= active)
C    << To be added. >>
 
C OUTPUTS:
C    << To be added. >>
C--------------------------------------------------------------------------------------------

      SUBROUTINE SOFC_A42_static(IPCOMP)
      IMPLICIT NONE
#include "SOFC_Annex42.h"
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

C Plant component pointer.
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2 
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

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
     &                     HRV_present, HRV_fanpower, HRV_effectiveness

      INTEGER Dilution_present   ! Indicates whether there is a dilution air system.
      REAL Dilution_airflow      ! Flow rate of dilution air (kmol/s).
      REAL Dilution_fanpower     ! Electrical power of fan drawing dilution air (W).
      REAL Dilution_heattoair    ! Heat transfer from FCPM to dilution air (W).
      INTEGER HRV_present        ! Indicates whether an HRV is present.
      REAL HRV_fanpower          ! Electrical power of fan drawing air through HRV (W).
      REAL HRV_effectiveness     ! Effectiveness of gas-to-air heat exchange (-).

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
        WRITE(ITU,*) ' 7-node Annex 42 SOFC-cogen model  '
        NumADATA = 27  ! Number of ADATA items.
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
C-----There should be no connections to nodes 1, 2, 3, 4, 5, and 7.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP,1,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,2,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,3,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,4,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,5,IPCONC) /= 0 ) mistake=.true.
        IF( ICONDX(IPCOMP,7,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----There should be one connection to node 6.
      IF( ICONDX(IPCOMP,6,1) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX(IPCOMP,6,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: incorrect number of'
        WRITE(IUOUT,*) ' connection for component ',IPCOMP
        STOP ' SOFC_A42_static: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connection to node 6 is of the correct type. The one connection
C to node 6 should be of type ISV=20 so that the SOFC-cogen component can be used
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
      IF( ICONTP( ICONDX(IPCOMP,6,1) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' SOFC_A42_static: incorrect connection type'
        WRITE(IUOUT,*) ' to node 6 for component ',IPCOMP
        STOP ' SOFC_A42_static: unresolvable error'
      END IF


C============
C ELECTRICAL
C============

C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients and the degredation term that
C describe the FCPM's electrical efficiency.
C---------------------------------------------------------------------------------
      FCPM_eta_0          = ADATA(IPCOMP,1)
      FCPM_eta_1          = ADATA(IPCOMP,2)
      FCPM_eta_2          = ADATA(IPCOMP,3)
      FCPM_degrade_ONOFF  = ADATA(IPCOMP,4)  ! Dedgradation term for on/off cycling.
      FCPM_degrade_optime = ADATA(IPCOMP,5) ! Degradation due to operating time.
      FCPM_degrade_thresh = ADATA(IPCOMP,6) ! Time threshold for `optime' degradation.


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


C==================
C AUXILIARY BURNER
C==================

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
      END SELECT
      HRV_present          = INT( ADATA(IPCOMP,100 ) )
      SELECT CASE ( HRV_present )
      CASE (SOFC_HRV_yes)
        HRV_fanpower       = ADATA(IPCOMP,101)
        HRV_effectiveness  = ADATA(IPCOMP,102)
      CASE (SOFC_HRV_no)
      END SELECT


C To do: initialize variables: refer to FCT model.


C---------------------------------------------------------------------------------
C Completion of analysis prior to commencing time-step simulation.
C---------------------------------------------------------------------------------
      RETURN
      END



C *********************************************************************************************
C ********************************* SOFC_A42_coeffgen *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: March 2005
C Copyright CETC 2005


C This subroutine is the coefficient generator for the Annex 42 SOFC-cogen component model.
C It is invoked each iteration of the plant matrix processing during each time-step
C to establish the coefficients for the sub-matrices that define the energy, 1st phase
C mass flow, and 2nd phase mass flow balances on the SOFC-cogen unit's control volumes.

C The SOFC-cogen device is represented with eight control volumes (aka nodes):
C   CV1 - The air within the air supply blower.
C   CV2 - The gas within the fuel compressor.
C   CV3 - The water within the water pump.
C   CV4 - The gas within the fuel cell power module (FCPM).
C   CV5 - The gas within the auxiliary burner.
C   CV6 - The water within and the solid portion of the heat recovery device.
C   CV7 - The gas within the heat recovery device.
C   CV8 - The gas within the dilution air system/HRV.

C INPUTS:
C    << To be added. >>

C OUTPUTS:
C    << To be added. >>
C -------------------------------------------------------------------------------------------

      SUBROUTINE SOFC_A42_coeffgen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "SOFC_Annex42.h"
#include "plant.h"
#include "building.h"

C External functions.     
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

C Simulation time step data.
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Plant component pointer.
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2 
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Iteration control.
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP, itrclp, ICSV
      REAL PERREL, PERTMP, PERFLX, PERMFL, CSVI

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
     &                     HRV_present, HRV_fanpower, HRV_effectiveness

      INTEGER Dilution_present   ! Indicates whether there is a dilution air system.
      REAL Dilution_airflow      ! Flow rate of dilution air (kmol/s).
      REAL Dilution_fanpower     ! Electrical power of fan drawing dilution air (W).
      REAL Dilution_heattoair    ! Heat transfer from FCPM to dilution air (W).
      INTEGER HRV_present        ! Indicates whether an HRV is present.
      REAL HRV_fanpower          ! Electrical power of fan drawing air through HRV (W).
      REAL HRV_effectiveness     ! Effectiveness of gas-to-air heat exchange (-).

C Iteration criteria for `additional' data items (PCDATF).
      common/pAdd_iter_criteria/iPlt_Output_Iter_Flag,
     &                          fPlt_Output_Init_Val, fPlt_Output_Tol
      INTEGER iPlt_Output_Iter_Flag (mpcom,mpcdat) ! flag denoting iteration is req'd
                                                   ! for plt additional outputs
      REAL fPlt_Output_Init_Val ( mpcom,mpcdat)    ! initial value of plt additional output
      REAL fPlt_Output_Tol (mpcom,mpcdat)          ! tolerance for convergence

C Test and debug (begin).
      COMMON/PTIME/PTIMEP,PTIMEF
      REAL PTIMEP,PTIMEF
C Test and debug (end).

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER IPCOMP      ! Index number of component (passed in calling statement)
      REAL COUT(MPCOE)    ! Plant matrix coeffs (passed in calling statement)
      INTEGER ISTATS      ! Flag for balance under consideration: 1=energy, 2=1st phase mass flow,
                          ! 3=2nd phase mass flow (passed in calling statement).
      INTEGER CV1,CV2,CV3,CV4,CV5,CV6,CV7,CV8 ! Indices for nodes.
      INTEGER con_CV6     ! Index for connection to node 6.
      REAL eta_el         ! FCPM elec efficiency.
      INTEGER OFFON_count ! Counter for number of off-on cycles.
      REAL OPER_TIME_HRS  ! Counter for number of hours that FCPM has operated since initial start.
      REAL AC_demand      ! AC power demanded from FC-cogen unit.
      REAL DC_demand      ! DC power demanded from FCPM and/or battery.
      REAL Pel            ! Net DC power supplied by FCPM (W).
      REAL Ndot_FCPM_fuel ! Flow rate of fuel supplied to FCPM (kmol/s).
      REAL Pel_ancAC      ! Power draw of FCPM ancillaries power by AC (W).
      REAL Ndot_FCPM_H2   ! Flow rate of fuel constituent to FCPM (kmol/s).
      REAL Ndot_FCPM_CH4  ! Ditto.
      REAL Ndot_FCPM_C2H6 ! Ditto.
      REAL Ndot_FCPM_C3H8 ! Ditto.
      REAL Ndot_FCPM_C4H10 ! Ditto.
      REAL Ndot_FCPM_C5H12 ! Ditto.
      REAL Ndot_FCPM_C6H14 ! Ditto.
      REAL Ndot_FCPM_CH3OH ! Ditto.
      REAL Ndot_FCPM_C2H5OH ! Ditto.
      REAL Ndot_FCPM_O2_stoich   ! Stoichiometric O2 flow rate to FCPM (kmol/s).
      REAL Ndot_FCPM_air_stoich  ! Stoichiometric air flow rate to FCPM (kmol/s).
      REAL Ndot_FCPM_air         ! Air flow rate to FCPM (kmol/s).
      REAL lambda_FCPM     ! Excess air ratio for FCPM.
      REAL T_FCPM_air      ! Temperature of air entering FCPM.
      REAL deltaH_FCPM_air ! Total enthalpy (relative to standard state) flow rate (W)
                           ! of air entering FCPM
      REAL Shomate         ! Function that applies Shomate equation to calculate gas properties.
      REAL Ndot_FCPM_liqwater ! Supply rate of liquid water to FCPM (kmol/s).
      REAL T_pump_in       ! Temperature of liquid water at pump inlet (oC).
      REAL pump_Pel        ! Elec power draw of water pump (W).
      REAL pump_q_loss       ! Heat loss from water pump to containing room (W).
      REAL H_FCPM_liqwater  ! Total enthalpy flow rate of liquid water entering FCPM (W).
      REAL T_FCPM_liqwater      ! Temperature of liquid water entering FCPM (oC).
      REAL cphat_liqwater  ! Heat capacity of liquid water enterging FCPM (J/kmolK).
      REAL H_formation_H2O_from_liquid ! Enthalpy of formation flow rate for water vapour
                                       ! associated with liquid water added for reformation (W).
      REAL T_FCPM_fuel      ! Temperature of fuel entering FCPM (oC).
      REAL deltaH_FCPM_fuel ! Total enthalpy (relative to standard state) flow rate (W)
                            ! of fuel entering FCPM
      REAL Ndot_FCPMexh_N2   ! Molar flow rate of N2 in FCPM exhaust.
      REAL Ndot_FCPMexh_Ar   ! Molar flow rate of Ar in FCPM exhaust.
      REAL Ndot_FCPMexh_O2   ! Molar flow rate of O2 in FCPM exhaust.
      REAL Ndot_FCPMexh_CO2  ! Molar flow rate of CO2 in FCPM exhaust.
      REAL Ndot_FCPMexh_H2O  ! Molar flow rate of H2O in FCPM exhaust.
      REAL deltaH_FCPM_exh   ! Total enthalpy (relative to standard state) flow rate (W)
                             ! of gases exiting FCPM.
      REAL FCPM_skin_loss    ! Skin losses from FCPM (W).
      LOGICAL CLOSE,CLOSEA
      REAL cphat_fuel        ! Heat capacity of fuel entering compressor (J/kmolK).
      REAL T_comp_in         ! Temp of fuel at compressor inlet (oC).
      REAL comp_Pel          ! Elec power draw of fuel compressor (W).
      REAL comp_q_loss       ! Heat loss from fuel compressor to containing room (W).
      REAL cphat_air         ! Heat capacity of air entering blower (J/kmolK).
      REAL T_blower_in       ! Temp of air at blower inlet (oC).
      REAL blower_Pel        ! Elec power draw of air supply blower (W).
      REAL blower_q_loss     ! Heat loss from air supply blower to containing room (W).
      REAL q_recover_to_air  ! Heat losses from auxiliary burner, battery, and PCU 
                             ! that are recovered to heat the air at the FCPM inlet (W).
      REAL T_FCPM_exh        ! Temp of product gases exiting FCPM (oC).
      REAL T_room            ! Temp of air in room containing SOFC-cogen device (oC).
      REAL FCPM_to_room_conv ! Convective heat transfer from FCPM to room due to
                             ! skin losses (W).
      REAL FCPM_to_room_rad  ! Radiative heat transfer from FCPM to room due to
                             ! skin losses (W).
      REAL Q_auxburn         ! Heat output from auxiliary burner (W).
      REAL Ndot_auxburn_fuel ! Flow rate of fuel supplied to auxiliary burner (kmol/s).
      REAL T_auxburn_fuel    ! Temperature of fuel entering auxiliary burner (oC).
      REAL deltaH_auxburn_fuel ! Total enthalpy (relative to standard state) flow rate (W)
                               ! of fuel entering auxiliary burner.
      REAL Ndot_auxburn_H2     ! Flow rate of fuel constituent to auxiliary burner (kmol/s).
      REAL Ndot_auxburn_CH4    ! Ditto.
      REAL Ndot_auxburn_C2H6   ! Ditto.
      REAL Ndot_auxburn_C3H8   ! Ditto.
      REAL Ndot_auxburn_C4H10  ! Ditto.
      REAL Ndot_auxburn_C5H12  ! Ditto.
      REAL Ndot_auxburn_C6H14  ! Ditto.
      REAL Ndot_auxburn_CH3OH  ! Ditto.
      REAL Ndot_auxburn_C2H5OH ! Ditto.
      REAL Ndot_auxburn_O2_stoich  ! Flow rate of O2 required for stoichiometric combustion
                                   ! in auxiliary burner (kmol/s).
      REAL Ndot_auxburn_air_stoich ! Flow rate of air required for stoichiometric combustion
                                   ! in auxiliary burner (kmol/s).
      REAL Ndot_auxburn_air        ! Flow rate of air to auxiliary burner (includes excess air, kmol/s).
      REAL T_auxburn_air        ! Temperature of air entering auxiliary burner (oC).
      REAL deltaH_auxburn_air   ! Total enthalpy (relative to standard state) flow rate (W)
                                ! of air entering auxiliary burner.
      REAL Ndot_auxburnexh_N2   ! Molar flow rate of N2 in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_Ar   ! Molar flow rate of Ar in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_O2   ! Molar flow rate of O2 in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_CO2  ! Molar flow rate of CO2 in auxiliary burner exhaust.
      REAL Ndot_auxburnexh_H2O  ! Molar flow rate of H2O in auxiliary burner exhaust.
      REAL deltaH_auxburn_exh   ! Total enthalpy (relative to standard state) flow rate (W)
                                ! of gases exiting auxiliary burner.
      REAL auxburn_Pel          ! Elec power draw of auxiliary burner ancillaires (W).
      REAL auxburn_skin_loss    ! Skin losses from auxiliary burner (W).
      REAL T_auxburn_exh        ! Temp of auxiliary burner exhaust (oC).
      REAL T_HX_gas_in          ! Temp of gases entering gas-to-water heat exchanger (oC).
      REAL C_gas                ! Heat capacity rate (W/K) of gas stream entering HX.
      REAL C_water              ! Heat capacity rate (W/K) of water stream entering HX.
      REAL C_min                ! Minimim heat capacity flow rate of gas and water streams (W/K).
      REAL H20LIQUIDCPCel       ! Function to calculate heat capacity of water.
      REAL T_HX_water_in        ! Temp of water entering HX (oC).
      REAL P_HX_water           ! Pressure of water entering HX (Pa).
      REAL Q_HX                 ! Heat recovery to water in gas-to-water HX (W).
      REAL Q_HX_sensible        ! Sensible portion of heat recovery (W).
      REAL Q_HX_latent          ! Latent portion of heat recovery (W).
      REAL Mdot_HX_liqwater     ! Mass flow rate of liquid water through plant side of HX (kg/s).
      REAL Ndot_HX_liqwater     ! Molar flow rate of liquid water through plant side of HX (kmol/s).
      REAL Ndot_HX_gas          ! Molar flow rate of all exhaust gases through HX (kmol/s).
      REAL UA_HX                ! Effective UA value of HX (W/K).
      REAL T_HX_water_out       ! Temp of water exiting HX (oC).
      REAL T_HX_gas_out         ! Temp of gas exiting HX (oC).
      REAL HX_h_gas             ! Heat transfer coeff on gas side of HX (W/m2K).
      REAL HX_h_water           ! Heat transfer coeff on water side of HX (W/m2K).
      REAL HX_water_vap_frac    ! Fraction of water vapour in gas stream entering HX (-).
      REAL HX_Ndot_condense     ! Rate of condensation of water from gas in HX (kmol/s).
      REAL HX_Hfg               ! Molar heat of vapourization of water (J/kmol).
      INTEGER itemp             ! A counter used in outputting trace results.
      REAL C_FCPM               ! Heat capacity rate of FCPM exhaust gases (W/K).
      REAL C_auxburn            ! Heat capacity reate of aux burner exhaust gases (W/K).

C Test & debug (begin).
      REAL T_FCPM_exh_guess, T_auxburn_exh_guess
      CHARACTER*128 char_temp
C Test & debug (end).


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
C    CV1            global matrix node number for node 1
C    CV2            global matrix node number for node 2
C    CV3            global matrix node number for node 3
C    CV4            global matrix node number for node 4
C    CV5            global matrix node number for node 5
C    CV6            global matrix node number for node 6
C    CV7            global matrix node number for node 7
C    CV8            global matrix node number for node 8
C    con_CV6        connection number to node 6 (water in heat exchanger)
C---------------------------------------------------------------------------------
      CV1     = NPCDAT(IPCOMP,9)
      CV2     = NPCDAT(IPCOMP,9)+1
      CV3     = NPCDAT(IPCOMP,9)+2
      CV4     = NPCDAT(IPCOMP,9)+3
      CV5     = NPCDAT(IPCOMP,9)+4
      CV6     = NPCDAT(IPCOMP,9)+5
      CV7     = NPCDAT(IPCOMP,9)+6
      CV8     = NPCDAT(IPCOMP,9)+7
      con_CV6 = ICONDX(IPCOMP,6,1)


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


C---------------------------------------------------------------------------------
C Mark the heat recovery to the water for iteration.
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


C To do: Need to determine whether unit is on or off this time-step.
C If it has just been switched off then need to increment OFFON_count.
C Also need to increment OPER_TIME_HRS (perhaps here).
C Need to have separate blocks to manage start-up and shut-down sequences.

C*********************************************************************************
C Beginning of energy balance for when fuel cell is on section *******************
C*********************************************************************************
c      IF(ISTATS==1 .and. FC_onoff) THEN
      IF(ISTATS==1) THEN


C==========================================
C OPERATING POINT AND ELECTRICAL EFFICIENCY
C==========================================

C---------------------------------------------------------------------------------
C Determine the AC power output demanded from the SOFC-cogeneration device
C and determine the DC power output demanded from the FCPM and/or battery.
C The current implementation does not consider the power losses incurred by the
C PCU nor the interaction between the battery and FCPM.
C To do: implement logic presented in section 5.3 of the model spec.  
C CDATA(IPCOMP,1) is the control signal (coming from the DG controller) that
C indicates the fraction of full load power requested.
C---------------------------------------------------------------------------------
        AC_demand = FCPM_elec_max * CDATA(IPCOMP,1)  ! W
        DC_demand = AC_demand  ! PCU ignored for now.
C-------External control algorithm should not allow operation outwith the
C-------user-input operating range. Halt simulation if this is not the case.
C-------(This could be replaced by a warning in the future.)
       IF ( AC_demand < FCPM_elec_min .OR.
     &      AC_demand > FCPM_elec_max ) THEN
         WRITE(IUOUT,*) ' Controller trying to operate SOFC-cogen',
     &                  ' device outside of its applicable range.'
         STOP ' SOFC_A42_coeffgen: unresolvable error'
       END IF


C---------------------------------------------------------------------------------
C Determine the FCPM's operating point.  The FCPM attempts to meet the DC power
C demanded but changes in the operating point from the previous time-step are
C constrained by the FCPM's transient response characteristics.
C Refer to section 5.4 of the model spec.
C---------------------------------------------------------------------------------
        IF( DC_demand == PCDATP(IPCOMP,1) ) THEN
C---------Demand on FCPM is same as output last time-step: no change in operating point.
          Pel = DC_demand
        ELSEIF( DC_demand > PCDATP(IPCOMP,1) ) THEN
C---------More power demanded than was output last time-step: ramp up FCPM.
          IF( (DC_demand-PCDATP(IPCOMP,1))/TIMSEC >
     &        FCPM_dPdt_inc_limit ) THEN   ! Can't ramp up this quickly.
            Pel = PCDATP(IPCOMP,1) + FCPM_dPdt_inc_limit*TIMSEC
          ELSE
            Pel = DC_demand
          ENDIF
        ELSE
C---------Less power demanded than was output last time-step: ramp down FCPM.
          IF( (PCDATP(IPCOMP,1)-DC_demand)/TIMSEC >
     &        FCPM_dPdt_dec_limit ) THEN  ! Can't ramp down this quickly.
            Pel = PCDATP(IPCOMP,1) - FCPM_dPdt_dec_limit*TIMSEC
          ELSE
            Pel = DC_demand
          ENDIF
        ENDIF

C Test and debug (begin).
        IF( Pel<1. ) Pel=100. ! To do: add protection when Pel=0 (e.g. NSINC=1). 
C Test and debug (end).

C---------------------------------------------------------------------------------
C Calculate the electrical efficiency (equation 8).
C---------------------------------------------------------------------------------
        OFFON_count = 0    ! Counter for number of off-on cycles. To do: add functionality.
        OPER_TIME_HRS = 0. ! Operating time of FCPM since initial power up (hours).
                           ! To do: add functionality.
        eta_el = ( FCPM_eta_0
     &             + FCPM_eta_1*Pel
     &             + FCPM_eta_2*Pel**2. ) *
     &           ( 1. - REAL(OFFON_count)*FCPM_degrade_ONOFF ) *
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
C Determine elec power draw of fuel compressor (W).
C---------------------------------------------------------------------------------
        comp_Pel = compressor_0
     &           + compressor_1*Ndot_FCPM_fuel
     &           + compressor_2*Ndot_FCPM_fuel**2.
     &           + compressor_3*Ndot_FCPM_fuel**3.


C---------------------------------------------------------------------------------
C Calculate the heat loss from the compressor to the containing room (W). This
C should be added as a source term to the energy balance of the containing room.
C To do: add this to room energy balance.
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
C Determine elec power draw of air supply blower (W).
C---------------------------------------------------------------------------------
        blower_Pel = blower_0
     &             + blower_1*Ndot_FCPM_air
     &             + blower_2*Ndot_FCPM_air**2.
     &             + blower_3*Ndot_FCPM_air**3.


C---------------------------------------------------------------------------------
C Calculate the heat loss from the air supply blower to the containing room (W).
C This should be added as a source term to the energy balance of the containing
C room.
C To do: add this to room energy balance.
C---------------------------------------------------------------------------------
        blower_q_loss = Blower_heat_loss * blower_Pel  


C---------------------------------------------------------------------------------
C Warn user if temperature rise of air through blower is unrealistic. Only
C issue warning if there is an air flow through the blower.
C---------------------------------------------------------------------------------
        CALL ECLOSE(Ndot_FCPM_air, 0., 1.E-12, CLOSE) ! To do: what is the right tolerance?
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
        IF( Auxburn_heat_to_where==Auxburn_heat_to_air  )THEN
          q_recover_to_air = q_recover_to_air + auxburn_skin_loss
C To do: auxburn_skin_loss calculated after here. Will this converge? Does data persist?
        END IF
C-------Battery. (To do)
C       IF-THEN
C         q_recover_to_air = q_recover_to_air + 
C       END IF
C-------PCU. (To do)
C       IF-THEN
C         q_recover_to_air = q_recover_to_air + 
C       END IF


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
C can either be drawn from the room that contains the SOFC-cogen device or from
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
C To do: Add support for drawing water from mains. Make use of new facility
C being added to ESP-r to support solar DHW system modelling.
c          T_pump_in = ??
            WRITE(IUOUT,*) ' Support not yet provided for supplying '
            WRITE(IUOUT,*) ' pump from water mains. '
            STOP ' SOFC_A42_coeffgen: unresolvable error'

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
C Determine elec power draw of water pump (W).
C---------------------------------------------------------------------------------
        pump_Pel = pump_0
     &           + pump_1*Ndot_FCPM_liqwater
     &           + pump_2*Ndot_FCPM_liqwater**2.
     &           + pump_3*Ndot_FCPM_liqwater**3.


C---------------------------------------------------------------------------------
C Calculate the heat loss from the pump to the containing room (W). This
C should be added as a source term to the energy balance of the containing room.
C To do: add this to room energy balance.
C---------------------------------------------------------------------------------
        pump_q_loss = Pump_heat_loss * pump_Pel  


C---------------------------------------------------------------------------------
C Warn user if temperature rise of fuel through pump is unrealistic.
C---------------------------------------------------------------------------------
        IF( ( ( 1.-Pump_heat_loss ) * pump_Pel
     &        / ( Ndot_FCPM_liqwater*cphat_liqwater ) ) > 20. ) THEN
          WRITE(IUOUT,*) ' SOFC_A42_coeffgen: temp rise of fuel in',
     &                   ' compressor is > 20 oC.'
          WRITE(IUOUT,*) ' Examine validatity of input data.'
        END IF


C---------------------------------------------------------------------------------
C Set the temperature of the water entering to the FCPM to that calculated for
C the exit of the water pump at the previous iteration. Once the solution
C converges this will equal the temperature of the water exiting the water pump
C for the current itereation.
C---------------------------------------------------------------------------------
        T_FCPM_liqwater = CSVF(CV3,1)


C---------------------------------------------------------------------------------
C Calculate the enthalpy flow rate (W) of the liquid water entering the FCPM.
C This enthalpy flow rate is NOT relative to the standard state because the
C liquid water added for reformation purposes results in a comensurate increase
C in the water vapour flow rate in the FCPM product gases.  Consequently, the
C total enthalpy flow rates of the liquid water entering and the resulting water
C vapour exiting the FCPM are considered, this to account for the enthalpy
C associated with the phase change from liquid to vapour state.
C---------------------------------------------------------------------------------
        H_FCPM_liqwater = Ndot_FCPM_liqwater
     &                  * Shomate(totalH,T_FCPM_liqwater,liq_H2O,IUOUT)


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
          T_FCPM_exh_guess = 300.  ! Just to get ball rolling. To do: refine this.
        END IF
        deltaH_FCPM_exh = 
     &       Ndot_FCPMexh_N2*Shomate(deltaH,T_FCPM_exh_guess,N2,IUOUT)
     &     + Ndot_FCPMexh_Ar*Shomate(deltaH,T_FCPM_exh_guess,Ar,IUOUT)
     &     + Ndot_FCPMexh_O2*Shomate(deltaH,T_FCPM_exh_guess,O2,IUOUT)
     &     + Ndot_FCPMexh_CO2*Shomate(deltaH,T_FCPM_exh_guess,CO2,IUOUT)
     &     + Ndot_FCPMexh_H2O*Shomate(deltaH,T_FCPM_exh_guess,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the heat capacity rate (W/K), sum of the products of the molar flow
C rates and heat capacities) of the FCPM exhaust gas constituents. This is
C evaluated at the temperature determined at the previous solver iteration. This
C is used to linearize the energy balances for the FCPM and auxiliary burner.
C---------------------------------------------------------------------------------
        C_FCPM   =
     &      Ndot_FCPMexh_N2*Shomate(heatcap,T_FCPM_exh_guess,N2,IUOUT)
     &    + Ndot_FCPMexh_Ar*Shomate(heatcap,T_FCPM_exh_guess,Ar,IUOUT)
     &    + Ndot_FCPMexh_O2*Shomate(heatcap,T_FCPM_exh_guess,O2,IUOUT)
     &    + Ndot_FCPMexh_CO2*Shomate(heatcap,T_FCPM_exh_guess,CO2,IUOUT)
     &    + Ndot_FCPMexh_H2O*Shomate(heatcap,T_FCPM_exh_guess,H2O,IUOUT)


C---------------------------------------------------------------------------------
C The water vapour produced as a result of the supply of liquid water for
C reformation was added to `Ndot_FCPMexh_H2O'.  Subsequently the enthalpy,
C relative to the standard state, of this water vapour at the exit of the FCPM
C was determined (`deltaH_FCPM_exh').  The enthalpy of formation of this portion
C of the water vapour is now determined here for consideration in the FCPM energy
C balance.  This treatment is equivalent to evaluating the total enthalpy flow
C rate of the outgoing water water associated with the liquid water supply.
C This is necessary because the energy balance must consider the total enthalpy
C flow rates of the incoming liquid water and the comensurate outgoing water
C vapour, this to account for the enthalpy associated with the phase change from
C liquid to vapour state.
C---------------------------------------------------------------------------------
        H_formation_H2O_from_liquid =
     &                     Ndot_FCPM_liqwater
     &                   * (   Shomate(totalH,T_FCPM_exh,H2O,IUOUT)
     &                       - Shomate(deltaH,T_FCPM_exh,H2O,IUOUT)  )


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

C---------------------------------------------------------------------------------
C Determine the convective and radiative split of these skin losses. These should
C be added as a source term to the energy balance of the containing rooom.
C To do: add this to room energy balance.
C---------------------------------------------------------------------------------
        FCPM_to_room_conv = FCPM_skin_loss*FCPM_skin_loss_conv
        FCPM_to_room_rad  = FCPM_skin_loss*( 1.-FCPM_skin_loss_conv )


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
            Ndot_auxburn_fuel = Q_auxburn / LHV_fuel                  ! kmol/s
C-----------External control algorithm should not allow operation outwith the
C-----------user-input operating range. Halt simulation if this is not the case.
C-----------(This could be replaced by a warning in the future.)
            IF ( Q_auxburn < Auxburn_min_output .OR.
     &           Q_auxburn > Auxburn_max_output ) THEN
              WRITE(IUOUT,*) ' Controller trying to operate auxiliary',
     &                       ' burner outside of its applicable range.'
              STOP ' SOFC_A42_coeffgen: unresolvable error'
            END IF

          CASE (Auxburn_cap_in_kmols)
            Ndot_auxburn_fuel = Auxburn_max_output * CDATA(IPCOMP,2)  ! kmol/s
            Q_auxburn         = Ndot_auxburn_fuel * LHV_fuel          ! W
C-----------External control algorithm should not allow operation outwith the
C-----------user-input operating range. Halt simulation if this is not the case.
C-----------(This could be replaced by a warning in the future.)
            IF ( Ndot_auxburn_fuel < Auxburn_min_output .OR.
     &           Ndot_auxburn_fuel > Auxburn_max_output ) THEN
              WRITE(IUOUT,*) ' Controller trying to operate auxiliary',
     &                       ' burner outside of its applicable range.'
              STOP ' SOFC_A42_coeffgen: unresolvable error'
            END IF

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
C Determine elec power draw of the ancillary burner's ancillaries (e.g. combustion
C air supply fan, controls, ignition system) (W).
C---------------------------------------------------------------------------------
        auxburn_Pel = Auxburn_anc_0
     &              + Auxburn_anc_1*Ndot_auxburn_fuel


C---------------------------------------------------------------------------------
C Calculate the skin losses from the auxiliary burner to the containing room (W).
C This heat can either be added to the inlet air to the FCPM or it can be added
C to the energy balance of the containing room.
C To do: add this to room energy balance.
C---------------------------------------------------------------------------------
C-------Temp of gases exiting auxiliary burner at previous iteration. Once solution
C-------converges this will equal temp for current iteration.
        T_auxburn_exh = CSVF(CV5,1)
C-------Air temperature of room containing SOFC-cogen device.
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' No containment has been defined for the '
          WRITE(IUOUT,*) ' SOFC-cogen plant component. This is '
          WRITE(IUOUT,*) ' required when modelling an auxiliary burner.'
          STOP ' SOFC_A42_coeffgen: unresolvable error'
        ELSE
          T_room = PCNTMF(IPCOMP)
        END IF
        auxburn_skin_loss = Auxburn_UA * ( T_auxburn_exh-T_room )


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
          T_auxburn_exh_guess = 500.  ! Just to get ball rolling. To do: refine this.
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
C    H20LIQUIDCPCel(T,P) is a function that returns the heat capacity (J/kgK)
C                        of liquid water at temperature `T' (oC) and pressure
C                        'P' (Pa).
C    CONVAR(i,1) holds the temperature (oC) of sending node for connection `i'.
C    CONVAR(i,2) holds the mass flow rate (kg/s) at sending node for conn `i'.
C    PCONDR(i) is the ratio of flow rate leaving sending node `i' that reaches
C              receiving node.
C---------------------------------------------------------------------------------
C-------Gas stream.
        T_HX_gas_in = CSVF(CV5,1)  ! Use previous iteration's value.
        IF(T_HX_gas_in<100.) THEN
          T_HX_gas_in = 400.  ! Just to get ball rolling. To do: refine this.
        END IF
        C_gas   =
     &        Ndot_auxburnexh_N2 *Shomate(heatcap,T_HX_gas_in,N2,IUOUT)
     &      + Ndot_auxburnexh_Ar *Shomate(heatcap,T_HX_gas_in,Ar,IUOUT)
     &      + Ndot_auxburnexh_O2 *Shomate(heatcap,T_HX_gas_in,O2,IUOUT)
     &      + Ndot_auxburnexh_CO2*Shomate(heatcap,T_HX_gas_in,CO2,IUOUT)
     &      + Ndot_auxburnexh_H2O*Shomate(heatcap,T_HX_gas_in,H2O,IUOUT)
C-------Water stream.
        T_HX_water_in = CONVAR(con_CV6,1)
        P_HX_water    = 101325. ! Assume water is at atmospheric pressure.
        Mdot_HX_liqwater = PCONDR(con_CV6) * CONVAR(con_CV6,2)
        C_water = Mdot_HX_liqwater
     &            * H20LIQUIDCPCel( T_HX_water_in,P_HX_water )


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
            T_HX_water_out = 10.  ! Just to get ball rolling. To do: refine this.
          END IF
          T_HX_gas_out = CSVF(CV7,1)  ! Use previous iteration's value.
          IF(T_HX_gas_in<10.) THEN
            T_HX_gas_out = 10.  ! Just to get ball rolling. To do: refine this.
          END IF
C---------It is physically impossible for the HX to cool the exhaust gases to
C---------below the temperature of the incoming water. However, this modelling
C---------artifact can occur during the convergence of the solution. To protect
C---------the LMTD calculation (such a situation leads to NaN) constrain the
C---------existing gas temperature.
C To do: Refine this approach.
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
C condensation threshold.
C---------------------------------------------------------------------------------
        SELECT CASE ( HX_method )

        CASE ( HX_condensing )
          HX_water_vap_frac = Ndot_auxburnexh_H2O / Ndot_HX_gas
          IF( T_HX_water_in <= HX_T_cond_thresh ) THEN
            HX_Ndot_condense = ( HX_T_cond_thresh - T_HX_water_in ) *
     &                         ( HX_lat_1 * HX_water_vap_frac
     &                           + HX_lat_2 * HX_water_vap_frac**2. )
            HX_Hfg = 44.E6  ! Approximate value for molar heat of vapourization (J/kmol).
                            ! To do: calc as function of temp (but what temp?). 
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
C To do: examine alternate forms for the energy balance for the condensing method.
C To do: examine stability and convergence of condensing method.
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


C===========================================
C COEFFICIENTS FOR MATRIX OF ENERGY BALANCES
C===========================================

C CV1 (air within blower).
C-------Prevent division by zero in RHS coefficient when there is no air flow.
        COUT( 1) = 1.
        CALL ECLOSE(Ndot_FCPM_air, 0., 1.E-12, CLOSE) ! To do: what is the right tolerance?
        IF(CLOSE)THEN
          COUT(18) = 0.
        ELSE
          COUT(18) = T_blower_in
     &             + ( ( 1.-Blower_heat_loss ) * blower_Pel
     &                 + q_recover_to_air )
     &               / ( Ndot_FCPM_air*cphat_air )
        END IF
C CV2 (fuel within compressor).
C-------Prevent division by zero in RHS coefficient when there is no fuel flow.
        COUT( 2) = 1.
        CALL ECLOSE(Ndot_FCPM_fuel, 0., 1.E-12, CLOSE) ! To do: what is the right tolerance?
        IF(CLOSE)THEN
          COUT(19) = 0.
        ELSE
          COUT(19) = T_comp_in
     &             + ( 1.-Compressor_heat_loss ) * comp_Pel
     &               / ( Ndot_FCPM_fuel*cphat_fuel )
        END IF
C CV3 (water within pump).
C-------Prevent division by zero in RHS coefficient when there is no fuel flow.
        COUT( 3) = 1.
        CALL ECLOSE(Ndot_FCPM_liqwater, 0., 1.E-12, CLOSE) ! To do: what is the right tolerance?
        IF(CLOSE)THEN
          COUT(20) = 0.
        ELSE
          COUT(20) = T_pump_in
     &             + ( 1.-Pump_heat_loss ) * pump_Pel
     &               / ( Ndot_FCPM_liqwater*cphat_liqwater )
        END IF
C CV4 (gas within FCPM).
        COUT( 4) = 0.
        COUT( 5) = 0.
        COUT( 6) = 0.
        COUT( 7) = 1.
        COUT(21) = ( deltaH_FCPM_fuel + deltaH_FCPM_air
     &               + Ndot_FCPM_fuel*LHV_fuel + Pel_ancAC
     &               + H_FCPM_liqwater
     &               - Pel - FCPM_skin_loss
     &               - H_formation_H2O_from_liquid
     &               - deltaH_FCPM_exh ) / C_FCPM
     &             + T_FCPM_exh_guess
C CV5 (gas within burner).
        SELECT CASE (Auxburn_present)
        CASE (Auxburn_yes)
          COUT( 8) = -1. * C_FCPM  ! Cross-coupling to FCPM gas.
          COUT( 9) = C_auxburn                           ! Self-coupling.
          COUT(22) = -1. * C_FCPM * T_FCPM_exh_guess
     &               + C_auxburn * T_auxburn_exh_guess
     &               + deltaH_FCPM_exh
     &               -1. * deltaH_auxburn_exh
     &               + deltaH_auxburn_fuel + deltaH_auxburn_air
     &               + Ndot_auxburn_fuel*LHV_fuel + auxburn_Pel
     &               - auxburn_skin_loss
        CASE (Auxburn_no)
          COUT( 8) = -1. ! Cross-coupling to FCPM gas.
          COUT( 9) = 1.  ! Self-coupling.
          COUT(22) = 0.
        END SELECT ! CASE DEFAULT not required because already checked above.

C Gas-to-water heat exchanger: CV6 (HX-water) and CV7 (HX-gas).
        SELECT CASE ( HX_method )
        CASE ( HX_fixed_eff )
C---------CV6 (HX-water).
          COUT(10) = -1. * HX_eff * C_min / C_water   ! Coupling to gas inlet temp.
          COUT(11) = 1.                               ! Self-coupling.
          COUT(12) = 0.                               ! Coupling to gas outlet temp.
          COUT(17) = HX_eff * C_min / C_water - 1.    ! Cross-coupling to cold water inlet.
          COUT(23) = 0.                               ! RHS.
C---------CV7 (HX-gas).
          COUT(13) = HX_eff * C_min / C_gas - 1.      ! Coupling to gas inlet temp.
          COUT(14) = 1.                               ! Self-coupling.
          COUT(24) = HX_eff * C_min / C_gas           ! RHS (Cannot cross-couple to cold
     &               * T_HX_water_in                  ! water inlet since only CV6 is physically                           
                                                      ! connected to water inlet, so this
                                                      ! this appears instead on RHS.
                                                      ! To do: examine implication on convergence.

        CASE ( HX_lmtd_empirical, HX_lmtd_deterministic )
C---------CV6 (HX-water).
          COUT(10) = -1. * C_gas / C_water
          COUT(11) = 1.
          COUT(12) = C_gas / C_water
          COUT(17) = -1.
          COUT(23) = 0.
C---------CV7 (HX-gas).
          COUT(13) = ( C_gas/C_water -1. ) /
     &               ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                 - C_gas / C_water )
          COUT(14) = 1.
          COUT(24) = T_HX_water_in *            ! RHS (To do: same as `HX_fixed_eff' case).
     &               ( EXP ( UA_HX * (1./C_gas - 1./C_water) ) - 1. )
     &               /
     &               ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                 - C_gas / C_water )

        CASE ( HX_condensing )
C---------Determine if there is sufficient condensation and ensure that there
C---------is a heat transfer, otherwise there will be divide-by-zero errors.
          CALL ECLOSE(HX_Ndot_condense, 0., 1.E-12, CLOSE)  ! To do: what is appropriate tolerance?
          CALL ECLOSE(Q_HX_sensible, 0., 1., CLOSEA)        ! Can be zero on first iteration.
          IF(CLOSE .or. CLOSEA)THEN
C-----------There is no condensation this time-step.
C-----------CV6 (HX-water).
            COUT(10) = -1. * C_gas / C_water
            COUT(11) = 1.
            COUT(12) = C_gas / C_water
            COUT(17) = -1.
            COUT(23) = 0.
C-----------CV7 (HX-gas).
            COUT(13) = ( C_gas/C_water -1. ) /
     &                 ( EXP ( UA_HX * (1./C_gas - 1./C_water) )
     &                   - C_gas / C_water )
            COUT(14) = 1.
            COUT(24) = T_HX_water_in *            ! RHS (To do: same `to do' as `HX_fixed_eff' case).
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
            COUT(17) = -1.
            COUT(23) = Q_HX_latent / C_water
C-----------CV7 (HX-gas).
            COUT(13) = ( C_gas/C_water -1. ) /
     &                 ( (  EXP ( UA_HX * (1./C_gas - 1./C_water) )  ) /
     &                   (  EXP (   ( UA_HX * Q_HX_latent     )
     &                            / ( C_water * Q_HX_sensible ) ) )  
     &                   - C_gas / C_water )
            COUT(14) = 1.
            COUT(24) = T_HX_water_in *            ! RHS (To do: same `to do' as `HX_fixed_eff' case).
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


C CV8 (dilution air/HRV exhaust gas).
C To do: only `flow through' for now.
        COUT(15) = -1.
        COUT(16) = 1.
        COUT(25) = 0.


C---------------------------------------------------------------------------------
C Save additional plant data items.  These record the current operational
C state of the SOFC-cogen device into the future time-row variables (PCDATF).
C Once the plant domain converges, these future time-row additional data items
C will be written to the present variables.  In this way, PCDATP records the
C operational state from the previous time-row.
C---------------------------------------------------------------------------------
        PCDATF(IPCOMP,1)  = Pel
        PCDATF(IPCOMP,2)  = Q_HX
C-------Save temperature of CV4 only so that a convergence criterion can be
C-------set. Without this, ESP-r will converge once the relative change in CV4's
C-------temperature is less than 1% (by default). Due to CV4's high temperature
C-------this can cause the iteration to end prematurely. The same effect could
C-------be achieved by setting PERREL to a lower value (e.g. 0.001) but this
C-------requires the use of a simulation toggle, and hence makes running bps
C-------in script mode more difficult.
        PCDATF(IPCOMP,3)  = T_FCPM_exh_guess
        PCDATF(IPCOMP,4)  = deltaH_FCPM_exh


C---------------------------------------------------------------------------------
C Make select results available in XML and CVS output.
C To do: Make select results available in res as well: PCAOUT and db changes.
C---------------------------------------------------------------------------------
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
     &        '/comp_Pel'
        call add_to_xml_reporting( comp_Pel, char_temp, 'units',
     &        '(W)', 'SOFC-A42: comp_Pel' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/pump_Pel'
        call add_to_xml_reporting( comp_Pel, char_temp, 'units',
     &        '(W)', 'SOFC-A42: pump_Pel' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Ndot_FCPM_liqwater'
        call add_to_xml_reporting( Ndot_FCPM_liqwater, char_temp,
     &        'units', '(kmol/s)', 'SOFC-A42: Ndot_FCPM_liqwater' )


C*********************************************************************************
C Beginning of energy balance for when fuel cell is off **************************
C*********************************************************************************
c      ELSE IF(ISTATS==1 .and. .not.FC_onoff) THEN


C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      ELSE IF(ISTATS==2) THEN
C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(18) = 0.
C CV1 (gas within compressor).
        COUT( 2) = 1.
        COUT(19) = 0.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(20) = 0.
C CV4 (gas within FCPM).
        COUT( 4) = 0.   ! Coupling to CV1.
        COUT( 5) = 0.   ! Coupling to CV2.
        COUT( 6) = 0.   ! Coupling to CV3.
        COUT( 7) = 1.   ! Self-coupling.
        COUT(21) = 0.
C CV5 (gas within burner).
        COUT( 8) = 0.   ! Cross-coupling to FCPM gas.
        COUT( 9) = 1.   ! Self-coupling.
        COUT(22) = 0.
C CV6 (HX-water).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(17) =  - PCONDR(con_CV6)
        COUT(23) = 0.
C CV7 (HX-gas).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(24) = 0.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(25) = 0.

C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      ELSE IF(ISTATS==3) THEN
C CV1 (air within blower).
        COUT( 1) = 1.
        COUT(18) = 0.
C CV1 (gas within compressor).
        COUT( 2) = 1.
        COUT(19) = 0.
C CV3 (water within pump).
        COUT( 3) = 1.
        COUT(20) = 0.
C CV4 (gas within FCPM).
        COUT( 4) = 0.   ! Coupling to CV1.
        COUT( 5) = 0.   ! Coupling to CV2.
        COUT( 6) = 0.   ! Coupling to CV3.
        COUT( 7) = 1.   ! Self-coupling.
        COUT(21) = 0.
C CV5 (gas within burner).
        COUT( 8) = 0.   ! Cross-coupling to FCPM gas.
        COUT( 9) = 1.   ! Self-coupling.
        COUT(22) = 0.
C CV6 (HX-water).
        COUT(10) = 0.
        COUT(11) = 1.
        COUT(12) = 0.
        COUT(17) = 0.
        COUT(23) = 0.
C CV7 (HX-gas).
        COUT(13) = 0.
        COUT(14) = 1.
        COUT(24) = 0.
C CV8 (dilution air/HRV exhaust gas).
        COUT(15) = 0.
        COUT(16) = 1.
        COUT(25) = 0.

      END IF


C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 8 node Annex 42 SOFC-cogen model'
        WRITE(ITU,*) ' Matrix node(s) ',CV1,CV2,CV3,CV4,CV5,CV6,CV7,CV8
        WRITE(ITU,*) ' Connection(s)  ',con_CV6
        IF(ISTATS.eq.1) THEN
          WRITE(ITU,*) ' T_HX_water_in     = ',T_HX_water_in
          WRITE(ITU,*) ' C_gas             = ',C_gas
          WRITE(ITU,*) ' C_water           = ',C_water
          WRITE(ITU,*) ' UA_HX             = ',UA_HX
C To do: Modify (begin).
c           WRITE(ITU,*) ' CDATA             = ',CDATA(IPCOMP,2),' (-)'
c           WRITE(ITU,*) ' Water Temp (p)    = ',T_node1_present,
c     &          ' (oC)'
c           WRITE(ITU,*) ' Water Temp (f)    = ',T_node1_future,' (oC)'
c           WRITE(ITU,*) ' Conn 1. Temp (p)  = ',T_1a_present,' (oC)'
c           WRITE(ITU,*) ' Conn 2. Temp (p)  = ',T_1b_present,' (oC)'
c           WRITE(ITU,*) ' q_burner          = ',q_burner,' (W)'
c           WRITE(ITU,*) ' q_capture_future  = ',q_capture_future,' (W)'
c           WRITE(ITU,*) ' m_dot_fuel_kg_t     = ',m_dot_fuel_kg_t,
c     &                                          ' (kg/s)'
c           WRITE(ITU,*) ' m_dot_air_kg_t      = ',m_dot_air_kg_t,
c     &                                          ' (kg/s)'
c           WRITE(ITU,*) ' m_dot_exh_kg_t      = ',m_dot_exh_kg_t,
c     &                                          ' (kg/s)'
c           WRITE(ITU,*) ' T_comb            = ',T_comb,' (oC)'
c           WRITE(ITU,*) ' T_exh_tank             = ',T_exh_tank,' (oC)'
c           WRITE(ITU,*) ' alpha             = ',alpha,' (-)'
C To do: Modify (end).
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(itemp),itemp=1,25)
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
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: April 2005
C Copyright CETC 2005

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
#include "SOFC_Annex42.h"

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
      DATA ( B(j),j=1,15 ) / 8.218801,6.137261,10.2573,2.825911E-7,
     &                      55.18696,-11.363417,108.4773,
     &                       199.202, 363.742, 426.442, 576.777,
     &                       711.187, 97.7218, 282.389, 1523.290 /
      DATA ( C(j),j=1,15 ) / -1.976141,-1.186521,2.81048,
     &                       -1.464191E-7,-33.69137,11.432816,
     &                      -42.52157,
     &                       -84.9812, -222.981, -257.955, -338.353,
     &                       -438.39, -9.73279, -178.85, -3196.413 /
      DATA ( D(j),j=1,15 ) / 0.159274,0.09578, -0.959141,1.092131E-8,
     &                      7.948387,-2.772874,5.862788,
     &                       11.0348, 56.253, 66.535, 76.8232,
     &                       103.784, -12.8461, 46.3528, 2474.455 /
      DATA ( E(j),j=1,15 ) / 0.044434,-0.219663,0.117253,
     &                       -3.661371E-8,-0.136638,-0.158558,
     &                      0.678565,
     &                       0.303476, 0.611637, -0.269941, 1.00948,
     &                       1.23887, 0.158186, 0.483644, 3.855326 /
      DATA ( F(j),j=1,15 ) / -7.98923,-9.861391,-250.569,-6.19735,
     &                      -403.6075,-9.980797,-76.84376,
     &                       -90.0633, -109.206, -149.365, -155.348,
     &                       -176.813, -209.037, -241.239, -256.5478 /
      DATA ( H(j),j=1,15 ) / 0.,0.,-241.826,0.,-393.5224,
     &                      0.,-74.8731,
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
        write(IUOUT,*) ' Temperature exceeds range of applicability.'
        write(6,*) gas,calcprop,tempK,max_range(gas)
C To do: how should this case be handled? Similar to temperatures below range?
c        STOP ' Error in FUNCTION Shomate'
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
