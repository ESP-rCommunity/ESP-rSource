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

C   FC_static_template        Static template executed prior to time-step simulation.
C   FC_coeff_generator        Coefficient generator executed each time-step (more
C                             if iteration) of the simulation.
C   FC_eloads_convert         Checks .fcl file and creates binary file with time series
C                             of electrical loads.
C   FC_eloads_establish       Establishes the electrical load demanded from the fuel
C                             cell controller for the current time-step.
C   FC_eloads_manage          Determines how much load the fuel cell controller demands
C                             from the fuel cell for the current time-step.
C   FC_thermal_priority_type1 Controller for type 1 thermal priority control.
C   FC_pump_fan_comp_number   Associates any pumps and fans with their respective
c                             component number - IPCOMP
C   H2_appliance_coeff_gen    Coefficient generator for fuel burning appliance model
C   app_load_data_process     Process fuel burning appliance load profile data file
C                             before the beginning of the time step simulation
C   app_load_establish        Read appropriate load profile data for fuel burning appliance
C                             during the time step simulation
C *******************************************************************************
C References:
C
C    Beausoleil-Morrison I. (2001), Design of Fuel Cell Component Model for
C    FCT project, CETC internal report, 16 pages, revised November 29, 2001.
C
C    Cuthbert D. (2001), Residential FC Energy Model, Kinectrics report P8518,
C    5 pages, Draft of June 22, 2001.
C *********************************************************************************************


C *********************************************************************************************
C ****************************** FC_static_template *******************************************
C Created by: Ian Beausoleil-Morrison
C Modified by: Maria Mottillo
C Initial Creation Date: August 9, 2001
C Modified on: August 17, 2006
C Copyright CETC 2001

C This subroutine is the static template for the fuel cell component. It checks
C whether the component is properly used in the user-defined plant network,
C it assigns user-input data to module variables, it performs time-invariant
C calculations, and it checks the integrity of the user-specified electric
C loads data file.

C This subroutine was modified to permit two connections to node 3 so that
C the SOFC plant component could be connected to two heat sinks. This variation of
C the original model is used to investigate the coupling of a SOFC to an
C adsorption storage unit for seasonal heat storage and to a hot water tank. To
C access the modified model, the appropriate plant component (code 112) must
C be specified in the plant network.


C The fuel cell is represented with three nodes:
C   node 1 represents the cell stack and ancillary devices, such as the power
C          conditioning system, circulating fans, etc.
C   node 2 represents the gas within the exhaust-to-water heat exchanger.
C   node 3 represents the water within and the solid portion of the exhaust-to-water
C          heat exchanger.

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
C    molW_N2    molecular weight of fuel constituent, kg/kmol (in MODULE FC_FLUIDS)
C    molW_CO2   ditto
C    molW_CH4   ditto
C    molW_C2H6  ditto
C    molW_C3H8  ditto
C    LHV_H2     lower heating value of fuel constituent, kJ/kmol (in MODULE FC_FLUIDS)
C    LHV_CH4    ditto
C    LHV_C2H6   ditto
C    LHV_C3H8   ditto
C    iFClds     Flag indicating whether a non-HVAC elec loads input file has been
C               specified (1 = yes)

C OUTPUTS:
C    fuel_molefrac_N2    fuel molar fractions (in MODULE FC_FLUIDS)
C    fuel_molefrac_CO2   ditto
C    fuel_molefrac_H2    ditto
C    fuel_molefrac_CH4   ditto
C    fuel_molefrac_C2H6  ditto
C    fuel_molefrac_C3H8  ditto
C    eta_0               poly coeff for FC elec eff (in MODULE FC_CHARACTERISTICS)
C    eta_1               ditto
C    eta_2               ditto
C    q_heatleak          heat leakage from the fuel cell, W (in MODULE FC_CHARACTERISTICS)
C    par_0               poly coeff for FC parasitic losses (in MODULE FC_CHARACTERISTICS)
C    par_1               ditto
C    par_2               ditto
C    par_3               ditto
C    wa_0                poly coeff for air supply to FC (in MODULE FC_CHARACTERISTICS)
C    wa_1                ditto
C    wa_2                ditto
C    qhr_0               poly coeff for heat exchanger U-value (in MODULE FC_CHARACTERISTICS)
C    qhr_1               ditto
C    qhr_2               ditto
C    LHV_fuel            energy content of fuel, kJ/kmol (in MODULE FC_FLUIDS)
C    molW_fuel           molecular weight of fuel, kg/kmol (in MODULE FC_FLUIDS)
C    molW_air            molecular weight of air, kg/kmol (in MODULE FC_FLUIDS)
C    liq_0               poly coeff for liquid water supply (in MODULE FC_CHARACTERISTICS)
C    liq_1               ditto
C    liq_2               ditto
C    FC_control          fuel cell control method (in MODULE FC_CHARACTERISTICS)
C    FC_elec_min_output  min net elec output from fuel cell (in MODULE FC_CHARACTERISTICS)
C    FC_elec_max_output  max net elec output from fuel cell (in MODULE FC_CHARACTERISTICS)
C    FC_avg_steps        number of simulation time-steps in plant domain over which running
C                        averages calculated (in MODULE FC_controls)
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_static_template(IPCOMP)
      IMPLICIT NONE
#include "SOFC.h"
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER	IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER	ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

      COMMON/PDBDT/ADATA,BDATA
      REAL	ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)

      COMMON/PCOND/CONVAR,ICONTP,ICONDX
      REAL	CONVAR(MPCON,MCONVR)
      INTEGER	ICONTP(MPCON),ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

      common/Fcell/iFClds,LFClds
      INTEGER iFClds
      CHARACTER LFClds*72

C.....Flag to enable inter-domain iteration
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA,Itemp,N_expect,IPCONC,iNum_DG_controller,init
      INTEGER IPCOMP
      INTEGER j
      INTEGER iComponent_code
      LOGICAL mistake,CLOSE

C---------------------------------------------------------------------------------
C Initialize variables. These were formerly initialized in MODULEs prior to the
C conversion of the code to enable compilation with g77.
C---------------------------------------------------------------------------------
C-----State of FC operation previous iter through coeff generator.
      FC_onoff_previous = .FALSE.
C-----Averaging period (during FC ramp-up) used by FC thermal priority controller.
      FC_avg_steps_rampup = 0
C-----Running avg of burner energy input to tank (used by FC thermal priority control).
      FC_tank_energy_capture_avg = 0.
C-----Running avg of thermal energy input from FC to tank (ditto).
      FC_heatX_energy_capture_avg = 0.
C-----Running avg of tank temperature (ditto).
      FC_tank_temp_avg = 0.
C-----Running avg of FC's electrical output (ditto).
      FC_net_elec_avg = 0.
C-----FC electrical output at previous time-step (ditto).
      q_net_elec_previous = 0.
C-----Initialize arrays for running avg's (used by FC thermal priority control).
      DO init=1,60
        FC_tank_energy_capture(init)  = 0.  ! Burner energy to tank.
        FC_heatX_energy_capture(init) = 0.  ! FC thermal energy to tank.
        FC_tank_temp(init)            = 0.  ! Tank temperature.
        FC_net_elec(init)             = 0.  ! FC electrical output.
      END DO
C-----Initialize variables that track HVAC electrical draws (not used if
C-----electrical network active).
      FCctl_HRV_draw        = 0.  ! HRV fans.
      FCctl_AC_compressor   = 0.  ! A/C compressor.
      FCctl_AC_circfan      = 0.  ! A/C circulation fan.
      FCctl_furnace_draft   = 0.  ! Furnace draft fan.
      FCctl_furnace_circfan = 0.  ! Furnace circulation fan.
      FCctl_pumps           = 0.  ! Explicit plant pumps.
      FCctl_fans            = 0.  ! Explicit plant fans.
      FCctl_boiler_draft    = 0.  ! Boiler draft fan.
      FCctl_boiler_circfan  = 0.  ! Boiler circulation fan (strange but consistent with H2K).
      FCctl_baseboard_fan   = 0.  ! Baseboard fan (strange but consistent with H2K).
      FCctl_ashp_compressor = 0.  ! ASHP compressor.
      FCctl_ashp_circfan    = 0.  ! ASHP circulation fan.
      FCctl_gshp_pump       = 0.  ! GSHP pump.
      FCctl_TAC_draw        = 0.  ! TAC condenser fan and refrigerant pump.
      FCctl_elec_water_tank = 0.  ! Electric hot water tank element.
C---------------------------------------------------------------------------------
C Correlation equation coefficients for calculating relative enthalpies of gases.
C From Cuthbert (2001) Table 2. The a1 coefficients for O2 and CO2 in Table 2 were
C incorrect so the values used here do not agree with Cuthbert (2001). Refer to
C Dave Cuthbert e-mail of 2001-08-07.
C---------------------------------------------------------------------------------
      DATA ( hcoef(1,j),j=1,9 ) / 134.9411, 2.353073, 1.07698,  ! a1 coefficients
     &                            265.9231, 96.3865, 628.4948,
     &                            -747.89, -2123.62, 80.27916 /
      DATA ( hcoef(2,j),j=1,9 ) / 6.49039, 7.40E+33, 5.90E+19,   ! a2 coefficients
     &                            7.017675, 4.635993, 4.969704,
     &                            7.373341, 10.49068, 6.57 /
      DATA ( hcoef(3,j),j=1,9 ) / 3.520E-4, 1.148765, 1.296, 7.86E-04, ! a3 coefficients
     &                            2.514E-4, 3.45E-03, 0.006258,
     &                            9.061999E-3, 1.95E-04  /
C---------------------------------------------------------------------------------
C Datum points for gas enthalpies. Evaluated using above correlations at 1 atm
C and 25oC. Values from Dave Cuthbert's e-mails of 2001-08-03 and 2001-08-07.
C---------------------------------------------------------------------------------
      DATA ( ho_datum(j),j=1,9 ) / 3722., 3722., 4046., 4261.,
     &                             2658., 4293., 5016., 6123., 3664. /


C---------------------------------------------------------------------------------
C Get component code. A component code of 80 refers to the original SOFC model
C that allows 1 connection to node 3. A component code of 112 refers to the
C modified SOFC model that permits 2 connections to node 3.
C---------------------------------------------------------------------------------

       iComponent_code = NPCDAT(IPCOMP,4)


C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. ITRACE(35)/=0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 3 node fuel cell  '
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
C as specified in the .pln file. The fuel cell component has a single control
C variable.
C---------------------------------------------------------------------------------
      N_expect = 1
      IF(NCI(IPCOMP) /= N_expect) THEN
        WRITE(ITU,*) ' FC_static_template warning: incorrect number',
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
C-----There should be no connections to node 1.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP,1,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----There should be no connections to node 2.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP,2,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----There should be one connection to node 3 if component code = 80
C-----and two connections to node 3 if component code =112.
      if ( iComponent_code .eq. 80 ) then
         IF( ICONDX(IPCOMP,3,1) == 0 ) mistake=.true.
         DO IPCONC=2,MPCONC
           IF( ICONDX(IPCOMP,3,IPCONC) /= 0 ) mistake=.true.
         END DO
      elseif ( iComponent_code .eq. 112 ) then
         DO IPCONC=3,MPCONC
           IF( ICONDX(IPCOMP,3,IPCONC) /= 0 ) mistake=.true.
         END DO
      endif
C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' FC_static_template: incorrect number of'
        WRITE(IUOUT,*) ' connection for component ',IPCOMP
        STOP ' FC_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connection(s) to node 3 is of the correct type. The connection(s) to node 3
C should be of type ISV=20 so that the fuel cell component can be used in networks in which
C both first and second phase mass balances are formed.
C Variables used:
C    ICONTP(i)  holds the state variable index (ISV) of the sending node for connection `i'.
C    ISV        defines nodal fluid type & coefficient generator model capabilities:
C               ISV=0,10,20 node represents water + ....
C               ISV=1,11,21 node represents dry air + ....
C               ISV=9,19,29 node represents some solid material only
C               0 <=ISV<10  model suitable for energy balance only
C               10<=ISV<20  model suitable for energy + single phase mass balance
C               20<=ISV<30  model suitable for energy + two phase mass balances
C---------------------------------------------------------------------------------
      mistake = .false.
      IF( ICONTP( ICONDX(IPCOMP,3,1) ) /= 20 ) mistake=.true.
      if ( iComponent_code .eq. 112 ) then
        if( icontp( icondx(ipcomp,3,2) ) .ne. 20 ) mistake = .true.
      endif
      IF(mistake)THEN
        WRITE(IUOUT,*) ' FC_static_template: incorrect connection type'
        WRITE(IUOUT,*) ' to node 3 for component ',IPCOMP
        STOP ' FC_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Assign user-specified molar fractions of fuel constituents to module variables.
C These will be used in the coefficient generator.
C---------------------------------------------------------------------------------
      fuel_molefrac_N2   = ADATA(IPCOMP,9)
      fuel_molefrac_CO2  = ADATA(IPCOMP,10)
      fuel_molefrac_H2   = ADATA(IPCOMP,11)
      fuel_molefrac_CH4  = ADATA(IPCOMP,12)
      fuel_molefrac_C2H6 = ADATA(IPCOMP,13)
      fuel_molefrac_C3H8 = ADATA(IPCOMP,14)


C---------------------------------------------------------------------------------
C Check to ensure that the molar fractions of the fuel constituents sum to unity.
C---------------------------------------------------------------------------------
      fuel_molefrac_total = fuel_molefrac_N2   + fuel_molefrac_CO2
     &                    + fuel_molefrac_H2   + fuel_molefrac_CH4
     &                    + fuel_molefrac_C2H6 + fuel_molefrac_C3H8
      CALL ECLOSE(fuel_molefrac_total,1.0,0.001,CLOSE)
      IF(.not.CLOSE)THEN
        WRITE(IUOUT,*) ' FC_static_template: molar fractions of fuel'
        WRITE(IUOUT,*) ' do not sum to unity.'
        STOP ' FC_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients that describe the fuel cell's
C electrical efficiency.
C---------------------------------------------------------------------------------
      eta_0 = ADATA(IPCOMP,1)
      eta_1 = ADATA(IPCOMP,2)
      eta_2 = ADATA(IPCOMP,3)


C---------------------------------------------------------------------------------
C Assign user-specified heat leakage and ensure the value is reasonable.
C---------------------------------------------------------------------------------
      q_heatleak = ADATA(IPCOMP,4)
      IF( q_heatleak > 500.)THEN
        WRITE(IUOUT,*) ' FC_static_template: heat leakage =',q_heatleak
        WRITE(IUOUT,*) ' This is unrealistic: adjust value.'
      ENDIF


C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients that describe the fuel cell's
C parasitic electrical losses.
C---------------------------------------------------------------------------------
      par_0 = ADATA(IPCOMP,5)
      par_1 = ADATA(IPCOMP,6)
      par_2 = ADATA(IPCOMP,7)
      par_3 = ADATA(IPCOMP,8)


C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients that describe the air supply
C to the fuel cell.
C---------------------------------------------------------------------------------
      wa_0 = ADATA(IPCOMP,15)
      wa_1 = ADATA(IPCOMP,16)
      wa_2 = ADATA(IPCOMP,17)


C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients that describe the U-value of
C the exhaust-to-water heat exchanger.
C---------------------------------------------------------------------------------
      qhr_0 = ADATA(IPCOMP,18)
      qhr_1 = ADATA(IPCOMP,19)
      qhr_2 = ADATA(IPCOMP,20)


C---------------------------------------------------------------------------------
C Calculate the energy content of the fuel, LHV (Beausoleil-Morrison eq 8).
C Units of kJ/kmol.
C---------------------------------------------------------------------------------
      LHV_fuel = fuel_molefrac_H2   * LHV_H2
     &         + fuel_molefrac_CH4  * LHV_CH4
     &         + fuel_molefrac_C2H6 * LHV_C2H6
     &         + fuel_molefrac_C3H8 * LHV_C3H8


C---------------------------------------------------------------------------------
C Calculate the molecular weight of the fuel (Beausoleil-Morrison eq 9).
C---------------------------------------------------------------------------------
      molW_fuel = fuel_molefrac_H2   * molW_H2
     &          + fuel_molefrac_CH4  * molW_CH4
     &          + fuel_molefrac_C2H6 * molW_C2H6
     &          + fuel_molefrac_C3H8 * molW_C3H8
     &          + fuel_molefrac_N2   * molW_N2
     &          + fuel_molefrac_CO2  * molW_CO2


C---------------------------------------------------------------------------------
C Calculate the molecular weight of air (Beausoleil-Morrison eq 13).
C---------------------------------------------------------------------------------
      molW_air = air_molefrac_N2  * molW_N2
     &         + air_molefrac_O2  * molW_O2
     &         + air_molefrac_CO2 * molW_CO2
     &         + air_molefrac_H2O * molW_H2O
     &         + air_molefrac_Ar  * molW_Ar


C---------------------------------------------------------------------------------
C Assign user-specified polynomial coefficients that describe the liquid water
C supply to the fuel cell.
C---------------------------------------------------------------------------------
      liq_0 = ADATA(IPCOMP,21)
      liq_1 = ADATA(IPCOMP,22)
      liq_2 = ADATA(IPCOMP,23)


C---------------------------------------------------------------------------------
C Assign user-specified control method for fuel cell.
C - Modification:
C   The uppel limit of the valid control variables has been changed from 3 to
C   4 to permit an external control scheme to be applied to the fuel cell
C   system
C
C   Modified by: Alex Ferguson on April 14, 2003.
C---------------------------------------------------------------------------------
      FC_control = ADATA(IPCOMP,24)
      IF( FC_control<1 .or. FC_control>4 )THEN
        WRITE(IUOUT,*) ' FC_static_template: invalid control on FC'
        STOP ' FC_static_template: unresolvable error'
      ENDIF

C---------------------------------------------------------------------------------
C Assign user-specified minimum and maximum net electrical output from
C fuel cell (W).
C---------------------------------------------------------------------------------
      FC_elec_min_output = ADATA(IPCOMP,25)
      FC_elec_max_output = ADATA(IPCOMP,26)
      IF( FC_elec_min_output > FC_elec_max_output )THEN
        WRITE(IUOUT,*) ' FC_static_template: max & min elec outputs'
        STOP ' FC_static_template: unresolvable error'
      ENDIF


C---------------------------------------------------------------------------------
C Determine whether user has requested (through .pln) that an ASCII file with
C time-step results be produced.
C---------------------------------------------------------------------------------
      FC_timestep_output = ADATA(IPCOMP,27)


C---------------------------------------------------------------------------------
C Process user-specified electrical loads data. Determine data type and convert
C from ASCII file to binary-format for direct access during simulation.
C
C Modification: Only process loads if an external controller is NOT present.
C               Otherwise, leave everything up to external controller.
C
C Modified by: Alex Ferguson on April 14, 2003.
C---------------------------------------------------------------------------------
      IF (.NOT.(FC_control==4)) THEN
         CALL FC_eloads_convert
      ELSEIF (iFClds==1) THEN
        WRITE(IUOUT,*) ' FC_static_template: .fcl file must not be'
        WRITE(IUOUT,*) ' specified in .cfg when DG controller is used.'
        STOP ' FC_static_template: unresolvable error'
      ENDIF

C---------------------------------------------------------------------------------
C Specify which plant network component (IPCOMP) is associated with the pumps or
C fans. This is required to account for the pump and fan electrical energy in the
C plant results output.
C
C Modification: Only manage HVAC loads if external controller is NOT present.
C               Otherwise, leave everything up to external controller.
C
C Modified by: Alex Ferguson on April 14, 2003.
C---------------------------------------------------------------------------------
      IF (.NOT.(FC_control==4)) THEN
         CALL FC_pump_fan_comp_number
      ENDIF

C---------------------------------------------------------------------------------
C     Set flag for inter-domain iteration
C---------------------------------------------------------------------------------
      bInter_domain_iteration = .true.

C---------------------------------------------------------------------------------
C Completion of time-invariant operations for fuel cell component.
C---------------------------------------------------------------------------------
      RETURN
      END



C ********************************************************************************
C ****************************** FC_coeff_generator ******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 14, 2001
C Modified by: Maria Mottillo
C Modified on: August 17, 2006
C Copyright CETC 2001

C This subroutine is the coefficient generator for the fuel cell component.
C It performs the time-step calculations and establishes coefficients for the
C sub-matrices that define the energy, 1st phase mass flow, and 2nd phase mass
C flow balances on the fuel cell's nodes. A fully explicit form of the energy
C balances is formed since the thermal transients of the fuel cell are ignored.

C The fuel cell is represented with three nodes:
C   Node 1 represents the cell stack and ancillary devices, such as the power
C          conditioning system, circulating fans, etc.
C   Node 2 represents the gas within the exhaust-to-water heat exchanger.
C   Node 3 represents the water within and the solid portion of the exhaust-to-water
C          heat exchanger.

C This subroutine was modified to permit two connections to node 3 so that
C the SOFC plant component could be connected to two heat sinks. This variation of
C the original model is used to investigate the coupling of a SOFC to an
C adsorption storage unit for seasonal heat storage and to a hot water tank. To
C access the modified model, the appropriate plant component (code 112) must
C be specified in the plant network.

C INPUTS:
C    IPCOMP              index number of the component under consideration
C    NPCOMP              number of plant components in user-defined plant network
C    ISTATS              flag indicating balance under consideration: 1=energy,
C                        2=1st phase mass flow, 3=2nd phase mass flow
C    NPCDAT(i,9)         identifies location of the current component's nodes
C                        within the global plant matrix.
C    ICONDX(i,j,k)       connection number for the k'th connection to the j'th node
C                        of component i: used as a pointer.
C    ICSV(i,j)           flag indicating that node `i' is marked for iteration for state
C                        variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                        j=3 for 2nd phase mass flow
C    CSVI(i,j)           initial value for judging whether iteration required: same
C                        indices as ICSV
C    CSVF(i,j)           future time-row solution variable for plant: same indices
C                        as ICSV
C    PCNTMF(i)           future time-row temperature of room containing component `i';
C                        =-99 if no containment defined.
C    fuel_molefrac_N2    fuel molar fractions (in MODULE FC_FLUIDS)
C    fuel_molefrac_CO2   ditto
C    fuel_molefrac_H2    ditto
C    fuel_molefrac_CH4   ditto
C    fuel_molefrac_C2H6  ditto
C    fuel_molefrac_C3H8  ditto
C    eta_0               poly coeff for FC elec eff (in MODULE FC_CHARACTERISTICS)
C    eta_1               ditto
C    eta_2               ditto
C    q_heatleak          heat leakage from the fuel cell, W (in MODULE FC_CHARACTERISTICS)
C    par_0               poly coeff for FC parasitic losses (in MODULE FC_CHARACTERISTICS)
C    par_1               ditto
C    par_2               ditto
C    par_3               ditto
C    wa_0                poly coeff for air supply to FC (in MODULE FC_CHARACTERISTICS)
C    wa_1                ditto
C    wa_2                ditto
C    qhr_0               poly coeff for heat exchanger U-value (in MODULE FC_CHARACTERISTICS)
C    qhr_1               ditto
C    qhr_2               ditto
C    LHV_fuel            energy content of fuel, kJ/kmol (in MODULE FC_FLUIDS)
C    molW_fuel           molecular weight of fuel, kg/kmol (in MODULE FC_FLUIDS)
C    molW_air            molecular weight of air, kg/kmol (in MODULE FC_FLUIDS)
C    T_refCp             reference temperature use for calculating specific heat of
C                        exhaust gases, oC (in MODULE FC_calculated)
C    liq_0               poly coeff for liquid water supply (in MODULE FC_CHARACTERISTICS)
C    liq_1               ditto
C    liq_2               ditto
C    IUOUT               output channel to feedback to user
C    ITC                 trace output index (>0 gives a trace output)
C    ITU                 output channel for trace output
C    ITRACE(37)          flag indicating one of the plant traces (1= active)
C    idyp                number of present day (0 to 365) (in COMMON/SIMTIM)
C    ptimef              `future' time of current day in hour fraction (0. to 24.) for
C                        current plant simulation increment (in COMMON/ptime).

C OUTPUTS:
C    COUT(i)             coefficient for ISTATS balance for `i'th matrix position
C    NAPDAT(i)           number of "additional" output for component `i'
C    PCAOUT(i,j)         additional output `j' for component `i'
C --------------------------------------------------------------------------------

      SUBROUTINE FC_coeff_generator(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "CETC_definitions.h"
#include "SOFC.h"


      COMMON/OUTIN/IUOUT,IUIN
      INTEGER	IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER	ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER	IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/BTIME/BTIMEP,BTIMEF
      REAL	BTIMEP,BTIMEF

      COMMON/PTIME/PTIMEP,PTIMEF
      REAL	PTIMEP,PTIMEF

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV,CSVI
      INTEGER MAXITP, itrclp, ICSV(MPNODE,MPVAR)
      REAL PERREL, PERTMP, PERFLX, PERMFL, CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

      COMMON/C10/NPCON,IPC1,IPN1,IPCT,IPC2,IPN2,PCONDR,PCONSD
      INTEGER	NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON)
      INTEGER	IPC2(MPCON),IPN2(MPCON)
      REAL	PCONDR(MPCON),PCONSD(MPCON,2)

      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

      COMMON/PCVAL/CSVF,CSVP
      REAL CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCVAR/PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP
      REAL	PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE)
      REAL	PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE)
      REAL	PCQP(MPNODE),PCNTMP(MPCOM)

      COMMON/PCOND/CONVAR,ICONTP,ICONDX
      REAL	CONVAR(MPCON,MCONVR)
      INTEGER	ICONTP(MPCON),ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      common/elpcp/
     &     NPEL,                ! number of "electrified" plant components
     &     PFP,          ! power factor of componet i (0->1)
     &     IPFP,         ! integer indicating if power factor lags (-1) or leads (+1)
     &     PWRP,         ! real power consumption of component (generation is -ive)
     &     BVOLTP,       ! opertional voltage of component
     &     IPHP          ! phase component is connected to -> dummy variable
      INTEGER NPEL,IPFP(mpcom),IPHP(mpcom)
      REAL PFP(mpcom),PWRP(mpcom),BVOLTP(mpcom)

      COMMON/ENTFILE/ENTFLNAM, IENTXIST
      CHARACTER*72 ENTFLNAM  ! Electrical networks filename.
      INTEGER IENTXIST       ! Elec netowrks flag. True if > 0

C The COMMON variables below are used by the modified fuel cell model when
C it is connected to an adsorption storage unit.

      COMMON/Stgdata/iCharge_signal,iDischarge_signal
      INTEGER iCharge_signal    ! charging signal for adsorption storage unit (on/off)
      INTEGER iDischarge_signal ! discharge signal for adsorption storage unit (on/off)

      COMMON/FCSignal/iFC_opr_signal   ! fuel cell operating signal required by
                                       ! adsorption storage controller.
      INTEGER iFC_opr_signal


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      LOGICAL CLOSE,FC_onoff
      REAL COUT(MPCOE)                ! Plant matrix coeffs (passed in calling statement)
      INTEGER IPCOMP, ISTATS          ! Passed in calling statement.
      REAL q_nonHVAC_elec,q_HVAC_elec ! non-HVAC and HVAC electrical loads.
      REAL Control_signal             ! Control signal from `DG controller' control law.
      REAL H_fuel                     ! Enthalpy of fuel entering FC.
      REAL H_gas                      ! Function that evaluates enthalpy of gas constituents.
      REAL Air_Sto_error
      REAL H_air                      ! Enthalpy of air entering FC.
      REAL RHOFLD                     ! Function that evaluates density.
      REAL Cp_exit                    ! Specific heat of gases existing heat exchanger.
      REAL SHTFLD                     ! Function that evaluates specific heat.
      INTEGER IEMODEL                 ! Electrical network flag.
      REAL PQ,PA                      ! Complex and overall power output from elec domain.
      INTEGER I                       ! Just a counter.
      INTEGER itemp                   ! Used to format trace output.
      INTEGER LNBLNK

      REAL T_exh_previous       ! exhaust temperature @ HE inlet for previous timestep
      REAL T_exit_previous      ! exhaust temperature @ HE outlet for previous timestep
      REAL Twater_enter_previous ! water temperature @ HE inlet for previous timestep

      INTEGER  iComponent_code        ! Plant component code

C The following variables are used for the case where the SOFC is connected to two
C heat sinks, a hot water tank and an adsorption storage unit. This code is project-specific
C and should be replaced with a more generalized code in the future.

      REAL Twater_enter_tank_previous ! water temperature @ HE inlet from tank for previous timestep
      REAL Twater_enter_adsorber_previous  ! water temperature @ HE inlet from adsorber
                                           ! for previous timestep

      REAL beta1, beta2         ! variables used to identify whether fuel cell exhaust heat
                                ! is directed to tank or adsorber
                                ! if heat is directed to tank, beta1 = 1, beta2 = 0
                                ! if heat is directed to adsorber, beta1 = 0, beta2 = 1


C---------------------------------------------------------------------------------
C Get component code. A component code of 80 refers to the original SOFC model
C that has 1 connection to node 3. A component code of 112 refers to the
C modified SOFC model that has 2 connections to node 3.
C---------------------------------------------------------------------------------

       iComponent_code = NPCDAT(IPCOMP,4)

C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Entering subroutine FC_coeff_generator'
      END IF

C---------------------------------------------------------------------------------
C Set local variables to point to the nodes and to the connection to node 3.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C                   within the global plant matrix.
C    INOD1          global matrix node number for fuel cell component node 1
C    INOD2          global matrix node number for fuel cell component node 2
C    INOD3          global matrix node number for fuel cell component node 3
C    ICON3          connection number to node 3 (water in heat exchanger)
C    ICON3b         number of 2nd connection to node 3 represents connection to
C                   adsorber (node 1 of adsorption storage unit model), if applicable
C---------------------------------------------------------------------------------
      INOD1 = NPCDAT(IPCOMP,9)
      INOD2 = NPCDAT(IPCOMP,9)+1
      INOD3 = NPCDAT(IPCOMP,9)+2
      ICON3 = ICONDX(IPCOMP,3,1)
      if ( iComponent_code .eq. 112 ) then
         ICON3b = icondx(ipcomp,3,2)
      endif

C---------------------------------------------------------------------------------
C Mark the temperatures of the cell stack and the gas within the exhaust-to-water
C heat exchanger (i.e. nodes 1 and 2) for iteration.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C---------------------------------------------------------------------------------
      ICSV(INOD1,1) = 1
      CSVI(INOD1,1) = CSVF(INOD1,1)
      ICSV(INOD2,1) = 1
      CSVI(INOD2,1) = CSVF(INOD2,1)


C---------------------------------------------------------------------------------
C Establish the fuel cell's electrical output. The total of the non-HVAC + HVAC
C loads `q_total_elec' are first determined. The method for determing the elec
C loads depends upon the control (FC_control) imposed by the user:
C - If FC_control <= 3 then control is imposed within the fuel cell plant
C   coefficient generator. This requires that the electrical loads be established
C   by the plant coefficient generator (as opposed to the electrical domain).
C - If FC_control = 4 then control is imposed by the `DG controller' control law.
C---------------------------------------------------------------------------------
      IF(.NOT.(FC_control==4)) THEN  ! Control imposed by plant coeff generator.
         IF (IENTXIST .le. 0) THEN   ! Ensures that elec domain not active.
           CALL FC_eloads_establish(idyp,btimep,ptimef,q_nonHVAC_elec,
     &                              q_HVAC_elec)
           q_total_elec = q_nonHVAC_elec + q_HVAC_elec
         ELSE
           WRITE(IUOUT,*) ' If an elec network is active then external'
           WRITE(IUOUT,*) ' controller must be used on cogen unit.'
           STOP
         ENDIF
      ELSE  ! Control imposed by `DG controller' control law.
         Control_signal = CDATA(IPCOMP,1)                    ! Ctl signal from 0 to 1.
         q_total_elec = Control_signal * FC_elec_max_output  ! Scale by FC's max elec output.
      ENDIF


C---------------------------------------------------------------------------------
C `q_total_elec' has been established. Now the controller's logic determines
C how the fuel cell responds to this load, and thus the fuel cell's
C net electrical output `q_net_elec'.
C---------------------------------------------------------------------------------
      CALL FC_eloads_manage


C---------------------------------------------------------------------------------
C Determine whether fuel cell is on this time-step. If there is an electrical
C load on the fuel cell, then it will be on (FC_onoff=.true.).
C---------------------------------------------------------------------------------
      IF( q_net_elec > 0. )THEN
        FC_onoff = .true.
      ELSE
        FC_onoff = .false.
      ENDIF

C---------------------------------------------------------------------------------
C Determine operating signal for adsorption storage controller. A more elegant
C generalized controller law should replace this code in the future.
C---------------------------------------------------------------------------------

      if ( FC_onoff ) then
         iFC_opr_signal = 1
      else
         iFC_opr_signal = 0
      endif

C---------------------------------------------------------------------------------
C Generate coefficients for the appropriate equation set. This subroutine was
C called to either generate coefficients for energy balances (ISTATS=1), 1st
C phase mass balances (ISTATS=2), or 2nd phase mass balances (ISTATS=3).
C Start with the energy balances.
C---------------------------------------------------------------------------------

C*********************************************************************************
C Beginning of energy balance for when fuel cell is on section *******************
C*********************************************************************************
      IF(ISTATS==1 .and. FC_onoff) THEN


C---------------------------------------------------------------------------------
C Determine the fuel cell's electrical efficiency (eta_fc) using the polynomial
C correlation (Beausoleil-Morrison eq 5). This must lie between 0 and 1. If not,
C halt simulation.
C---------------------------------------------------------------------------------
        q_net_elec_kW = q_net_elec/1000.
        eta_fc = eta_0 + eta_1*q_net_elec_kW + eta_2*q_net_elec_kW**2.
        IF( eta_fc <= 0. .or. eta_fc > 1. )THEN
          WRITE(IUOUT,*) ' FC_coeff_generator: FC elec eff =',eta_fc
          WRITE(IUOUT,*) ' This is unrealistic. Adjust n0, n1, n2.'
          STOP ' FC_coeff_generator: unresolvable error'
        ENDIF


C---------------------------------------------------------------------------------
C Determine the fuel cell's parasitic electrical losses in W (q_par_elec) using
C the polynomial correlation (Beausoleil-Morrison eq 6). Halt the simulation if
C the parasitic losses are unrealistic.
C---------------------------------------------------------------------------------
        q_par_elec = ( par_0 + par_1*q_net_elec_kW
     &                 + par_2*q_net_elec_kW**2.
     &                 + par_3*q_net_elec_kW**3. ) * 1000.
        IF( q_par_elec < 0. .or. q_par_elec > 5000. )THEN
          WRITE(IUOUT,*) ' FC_coeff_generator: FC parasitic elec'
          WRITE(IUOUT,*) ' losses = ',q_par_elec,' W'
          WRITE(IUOUT,*) ' This is unrealistic. Adjust p0, p1, p2, p3.'
          STOP ' FC_coeff_generator: unresolvable error'
        ENDIF


C---------------------------------------------------------------------------------
C Calculate the rate (W) at which the fuel's chemical energy content is converted
C to electrical and thermal energy (Beausoleil-Morrison eq 7).
C---------------------------------------------------------------------------------
        phi_source = ( q_net_elec+q_par_elec ) / eta_fc

C---------------------------------------------------------------------------------
C Determine the flow rate of fuel introduced into the fuel cell
C (Beausoleil-Morrison eq 10). Calculate this in terms of kmol/s, kg/s,
C and standard L/min..
C---------------------------------------------------------------------------------
        m_dot_fuel_kmol = ( phi_source / LHV_fuel ) /1000.
        m_dot_fuel_kg = m_dot_fuel_kmol * molW_fuel
        Vdot_fuel_STP = m_dot_fuel_kmol * 22414. * 60.


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the fuel entering the fuel cell
C (Beausoleil-Morrison eq 11). Assume that the fuel enters at a temperature equal
C to that of the room containing the fuel cell. The function H_gas returns the
C relative enthalpy of each fuel constituent in kJ/kmol.
C Variables used:
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' FC_coeff_generator: the fuel cell must'
          WRITE(IUOUT,*) ' be contained within a room.'
          STOP ' FC_coeff_generator: unresolvable error'
        ELSE
          T_fuel = PCNTMF(IPCOMP)
        END IF
        H_fuel = fuel_molefrac_N2   * H_gas(T_fuel,N2,IUOUT)
     &         + fuel_molefrac_CO2  * H_gas(T_fuel,CO2,IUOUT)
     &         + fuel_molefrac_CH4  * H_gas(T_fuel,CH4,IUOUT)
     &         + fuel_molefrac_C2H6 * H_gas(T_fuel,C2H6,IUOUT)
     &         + fuel_molefrac_C3H8 * H_gas(T_fuel,C3H8,IUOUT)
     &         + fuel_molefrac_H2   * H_gas(T_fuel,H2,IUOUT)


C---------------------------------------------------------------------------------
C Determine the flow rate of air introduced into the fuel cell using the
C polynomial correlation (Beausoleil-Morrison eq 12 and 14). Express the flow rate
C in three fashions: standard Litres/minute, kmol/s, and kg/s.
C---------------------------------------------------------------------------------
        Vdot_air_STP = wa_0 + wa_1*q_net_elec_kW + wa_2*q_net_elec_kW**2.
        m_dot_air_kmol = Vdot_air_STP / 60. / 22414.
        m_dot_air_kg = m_dot_air_kmol * molW_air


C---------------------------------------------------------------------------------
C Calculate the excess air flow rate in terms of stoichs. First, determine the
C molar flow rates (kmol/s) of the four fuel constituents that react with O2.
C Then determine the stoichiometric air required to react with these fuel
C constituents (Beausoleil-Morrison eq 15). Finally, calculate the excess air
C supplied to the fuel cell (Beausoleil-Morrison eq 16) by dividing the actual
C O2 flow rate by the stoichiometric O2.
C---------------------------------------------------------------------------------
C-------Flow rates of fuel constituents (kmol/s).
        m_dot_H2_kmol   = fuel_molefrac_H2   * m_dot_fuel_kmol
        m_dot_CH4_kmol  = fuel_molefrac_CH4  * m_dot_fuel_kmol
        m_dot_C2H6_kmol = fuel_molefrac_C2H6 * m_dot_fuel_kmol
        m_dot_C3H8_kmol = fuel_molefrac_C3H8 * m_dot_fuel_kmol
C-------Stoichiometric O2 flow rate required for each fuel constituent (kmol/s).
        m_dot_O2_for_H2_kmol   = 1./2. * m_dot_H2_kmol
        m_dot_O2_for_CH4_kmol  = 2.    * m_dot_CH4_kmol
        m_dot_O2_for_C2H6_kmol = 7./2. * m_dot_C2H6_kmol
        m_dot_O2_for_C3H8_kmol = 5.    * m_dot_C3H8_kmol
        m_dot_O2_stoich = m_dot_O2_for_H2_kmol
     &                    + m_dot_O2_for_CH4_kmol
     &                    + m_dot_O2_for_C2H6_kmol
     &                    + m_dot_O2_for_C3H8_kmol
C-------Excess air (stoichs).
        stoichs = ( m_dot_air_kmol*air_molefrac_O2 ) / m_dot_O2_stoich
C-------Warn user if air flow not within expected range.
C-------Modification:
C-------The air stoicheometry is pretty sensitive to changes in operating point and can
C-------produce 100's of error messages over the course of the simulation. Therefore,
C-------only report that the stiochiometry is outside expected range if it has changed
C-------by 10%. (Modified by Alex Ferguson, May 12, 2003)
        IF(stoichs<2. .or. stoichs>8.)THEN
C----------Check that Air_sto_error != 0
          CALL ECLOSE(Air_Sto_error,0.0,0.001,CLOSE)
          IF(.NOT.(CLOSE))THEN
             IF(ABS(1-(stoichs/Air_Sto_error))>0.1) THEN
C---------------Error has changed by 10%. Warn user
                WRITE(IUOUT,*) ' FC_coeff_generator: stoichs = ',stoichs
                WRITE(IUOUT,*) ' This is outside expected range.'
C---------------Update stored Stoichiometry error
                Air_Sto_error = stoichs
             ELSE
C------------Error has not changed significantly. Do nothing.
             ENDIF
          ELSE
C------------Air_sto_error has not been defined, or is zero. Stoich
C------------is outside expected range, so warn user.
             WRITE(IUOUT,*) ' FC_coeff_generator: stoichs = ',stoichs
             WRITE(IUOUT,*) ' This is outside expected range.'
C------------Update stored Stoichiometry error.
             Air_Sto_error = stoichs
          ENDIF
       ENDIF


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the air entering the fuel cell
C (Beausoleil-Morrison eq 17). Assume that the air enters at a temperature equal
C to that of the room containing the fuel cell. The function H_gas returns the
C relative enthalpy of each fuel constituent in kJ/kmol.
C Variables used:
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        T_air = PCNTMF(IPCOMP) ! Already confirmed above that containment defined.
        H_air = air_molefrac_N2   * H_gas(T_air,N2,IUOUT)
     &         + air_molefrac_O2  * H_gas(T_air,O2,IUOUT)
     &         + air_molefrac_CO2 * H_gas(T_air,CO2,IUOUT)
     &         + air_molefrac_H2O * H_gas(T_air,H2O,IUOUT)
     &         + air_molefrac_Ar  * H_gas(T_air,Ar,IUOUT)


C---------------------------------------------------------------------------------
C Determine the flow rate of liquid water supplied to the fuel cell using the
C polynomial correlation (Beausoleil-Morrison eq 17a). Express the flow rate
C in three fashions: Litres/min, kg/s (Beausoleil-Morrison eq 17b), and kmol/s.
C If the flow rate is excessive, halt the simulation and advise the user.
C Assume that the water is at a temperature equal to that of the room containing
C the fuel cell. The function RHOFLD returns the density of the water (kg/m3)
C at the supplied temperature.
C Variables used:
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        Vdot_liqwater = liq_0 + liq_1*Vdot_fuel_STP
     &                + liq_2*Vdot_fuel_STP**2.
        T_water = PCNTMF(IPCOMP) ! Already confirmed above that containment defined.
C-------Check if water temperaure is greater than 0 oC (the plant containment may
C-------not be heated). If not, use 0 oC.
        if( T_water .gt. 0.0 ) then
           DEN_liqwater = RHOFLD(3,T_water)
        else
           DEN_liqwater = RHOFLD(3,0.01)
        endif
        m_dot_liqwater_kg = Vdot_liqwater * DEN_liqwater / 1000. / 60.
        m_dot_liqwater_kmol = m_dot_liqwater_kg / molW_H2O
        IF( Vdot_liqwater/q_net_elec_kW > 0.1 )THEN
          WRITE(IUOUT,*) ' FC_coeff_generator: water supplied to FC'
          WRITE(IUOUT,*) ' is excessive. Adjust f0, f1, f2.'
          STOP ' FC_coeff_generator: unresolvable error'
        ENDIF


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kg) of the liquid water supplied to the fuel cell
C (Beausoleil-Morrison eq 17c). This is the enthalpy relative to the datum
C of 25oC.
C---------------------------------------------------------------------------------
        H_water = 4.22*T_water - 0.0011*T_water**2.
     &          + 7.E-6*T_water**3. - 104.92


C---------------------------------------------------------------------------------
C Determine the composition and flow rate of the exhaust gases. Based on the
C chemical reactions occuring within the stack (Beausoleil-Morrison eq 15),
C calculate the molar flow rate of each constituent. As reactions are assumed to
C be complete, there are no H2 or hydrocarbons in the exhaust.
C---------------------------------------------------------------------------------
C-------N2 (Beausoleil-Morrison eq 18).
        m_dot_N2_exh_kmol = m_dot_air_kmol  * air_molefrac_N2
     &                    + m_dot_fuel_kmol * fuel_molefrac_N2
C-------Ar (Beausoleil-Morrison eq 19)
        m_dot_Ar_exh_kmol = m_dot_air_kmol  * air_molefrac_Ar
C-------O2 (Beausoleil-Morrison eq 20)
        m_dot_O2_exh_kmol = m_dot_air_kmol * air_molefrac_O2
     &                    - 1./2. * m_dot_fuel_kmol * fuel_molefrac_H2
     &                    - 2.    * m_dot_fuel_kmol * fuel_molefrac_CH4
     &                    - 7./2. * m_dot_fuel_kmol * fuel_molefrac_C2H6
     &                    - 5.    * m_dot_fuel_kmol * fuel_molefrac_C3H8
C-------CO2 (Beausoleil-Morrison eq 21)
        m_dot_CO2_exh_kmol = m_dot_air_kmol  * air_molefrac_CO2
     &                     + m_dot_fuel_kmol * fuel_molefrac_CO2
     &                     + 1. * m_dot_fuel_kmol * fuel_molefrac_CH4
     &                     + 2. * m_dot_fuel_kmol * fuel_molefrac_C2H6
     &                     + 3. * m_dot_fuel_kmol * fuel_molefrac_C3H8
C-------H20 (Beausoleil-Morrison eq 22)
        m_dot_H2O_exh_kmol = m_dot_air_kmol * air_molefrac_H2O
     &                     + m_dot_liqwater_kmol
     &                     + 1. * m_dot_fuel_kmol * fuel_molefrac_H2
     &                     + 2. * m_dot_fuel_kmol * fuel_molefrac_CH4
     &                     + 3. * m_dot_fuel_kmol * fuel_molefrac_C2H6
     &                     + 4. * m_dot_fuel_kmol * fuel_molefrac_C3H8


C---------------------------------------------------------------------------------
C Calculate the flow rate of all exhaust gases in terms of kmol/s `m_dot_exh_kmol'
C and standard Litres/min `Vdot_exh_STP' (Beausoleil-Morrison eq 30).
C Then determine the mole fractions of the exhaust gas constituents
C (Beausoleil-Morrison eq 23).
C---------------------------------------------------------------------------------
        m_dot_exh_kmol = m_dot_N2_exh_kmol
     &                 + m_dot_Ar_exh_kmol
     &                 + m_dot_O2_exh_kmol
     &                 + m_dot_CO2_exh_kmol
     &                 + m_dot_H2O_exh_kmol
        Vdot_exh_STP = m_dot_exh_kmol * 22414. * 60.
        exh_molefrac_N2  = m_dot_N2_exh_kmol  / m_dot_exh_kmol
        exh_molefrac_Ar  = m_dot_Ar_exh_kmol  / m_dot_exh_kmol
        exh_molefrac_O2  = m_dot_O2_exh_kmol  / m_dot_exh_kmol
        exh_molefrac_CO2 = m_dot_CO2_exh_kmol / m_dot_exh_kmol
        exh_molefrac_H2O = m_dot_H2O_exh_kmol / m_dot_exh_kmol


C---------------------------------------------------------------------------------
C Determine the molecular weight of the exhaust gases (Beausoleil-Morrison eq 24).
C---------------------------------------------------------------------------------
        molW_exh = exh_molefrac_N2  * molW_N2
     &           + exh_molefrac_Ar  * molW_Ar
     &           + exh_molefrac_O2  * molW_O2
     &           + exh_molefrac_CO2 * molW_CO2
     &           + exh_molefrac_H2O * molW_H2O


C---------------------------------------------------------------------------------
C Determine the flow rate of all exhaust gases in terms of kg/s.
C---------------------------------------------------------------------------------
        m_dot_exh_kg = m_dot_exh_kmol * molW_exh


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the exhaust gases leaving node 1.
C (Beausoleil-Morrison eq 25). The function H_gas returns the relative enthalpy
C of each fuel constituent in kJ/kmol. Evaluate the enthalpies at the temperature
C solved the previous time-step.
C If the fuel cell has been idle and has just switched on, T_exh is not evaluated
C at the previous time-step value, since this would be the room air temperature.
C Rather, T_exh is set to a "typical" value. This approach improves the stability
C of the plant matrix convergence AND avoids an inappropriate error from the
C heat exchanger check below.
C Variables used:
C     CSVF(i,1)  future time-row temperature for node `i'.
C---------------------------------------------------------------------------------
        IF(FC_onoff_previous)THEN
          T_exh = CSVF(INOD1,1)  ! Use previous iteration value.
          T_exh_previous = CSVP(INOD1,1) ! use previous TS value
        ELSE
          T_exh = 300.           ! FC just turning on: use typical value.
        ENDIF
        H_exh = exh_molefrac_N2  * H_gas(T_exh,N2,IUOUT)
     &        + exh_molefrac_Ar  * H_gas(T_exh,Ar,IUOUT)
     &        + exh_molefrac_O2  * H_gas(T_exh,O2,IUOUT)
     &        + exh_molefrac_CO2 * H_gas(T_exh,CO2,IUOUT)
     &        + exh_molefrac_H2O * H_gas(T_exh,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the exhaust gas mixture at the reference
C temperature `T_refCp' (see Beausoleil-Morrison eq 26a and 26b). The function
C H_gas returns the relative enthalpy of each fuel constituent in kJ/kmol.
C---------------------------------------------------------------------------------
        Href =  exh_molefrac_N2  * H_gas(T_refCp,N2,IUOUT)
     &        + exh_molefrac_Ar  * H_gas(T_refCp,Ar,IUOUT)
     &        + exh_molefrac_O2  * H_gas(T_refCp,O2,IUOUT)
     &        + exh_molefrac_CO2 * H_gas(T_refCp,CO2,IUOUT)
     &        + exh_molefrac_H2O * H_gas(T_refCp,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Linearise the enthalpy of the exhaust gases so that temperature can appear in
C the energy balance for node 1 (Beausoleil-Morrison eq 26a). The Cp calculated
C here is used to linearise the enthalpy term. The difference between the exhaust
C gas temperature at the previous time-step and `T_refCp' is used to linearise
C the enthalpy. Degrees Celcius are used since ESP-r's plant matrix solver works
C in these units. The units of this heat capacity are J/(kmol*oC)
C---------------------------------------------------------------------------------
        Cp_exh = (H_exh-Href)*1000. / (T_exh - T_refCp)


C---------------------------------------------------------------------------------
C Calculate the UA-value of the exhaust-to-water heat exchanger using the
C polynomial correlation (Beausoleil-Morrison eq 29). Units are W/oC.
C---------------------------------------------------------------------------------
        UA_exh = qhr_0 + qhr_1*Vdot_exh_STP + qhr_2*Vdot_exh_STP**2.


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the gases exiting the heat exchanger
C (i.e. at node 2). The function H_gas returns the relative enthalpy
C of each fuel constituent in kJ/kmol. Evaluate the enthalpies at the temperature
C solved the previous time-step.
C If the fuel cell has been idle and has just switched on, T_exit is not evaluated
C at the previous time-step value, since this would be the room air temperature.
C Rather, T_exit is set to a "typical" value. This approach improves the stability
C of the plant matrix convergence.
C Variables used:
C     CSVF(i,1)  future time-row temperature for node `i'.
C---------------------------------------------------------------------------------
        IF(FC_onoff_previous)THEN
          T_exit = CSVF(INOD2,1)  ! Use previous iteration value.
          T_exit_previous = CSVP(INOD2,1)  ! Use previous iteration value.
        ELSE
          T_exit = 100.           ! FC just turning on: use typical value.
        ENDIF
        H_exit = exh_molefrac_N2  * H_gas(T_exit,N2,IUOUT)
     &         + exh_molefrac_Ar  * H_gas(T_exit,Ar,IUOUT)
     &         + exh_molefrac_O2  * H_gas(T_exit,O2,IUOUT)
     &         + exh_molefrac_CO2 * H_gas(T_exit,CO2,IUOUT)
     &         + exh_molefrac_H2O * H_gas(T_exit,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Linearise the enthalpy of the gases leaving the heat exchanger. The Cp
C calculated here is used to linearise the enthalpy term. The difference between
C the exhaust gas temperature at the previous time-step `T_refCp' is used to
C linearise the enthalpy. Degrees Celcius are used since ESP-r's plant matrix
C solver works in these units. The units of this heat capacity are J/(kmol*oC).
C Variables used:
C     CSVF(i,1)  future time-row temperature for node `i'.
C---------------------------------------------------------------------------------
        Cp_exit = (H_exit-Href)*1000. / (T_exit - T_refCp)


C---------------------------------------------------------------------------------
C Calculate the product of mass flow rate and heat capacity (W/K) for the water
C entering and leaving the heat exchanger (`mdotCp_water'). There is no water
C leakage from heat exchanger and the heat capacity of water is almost constant
C over the range of temperatures from the inlet to the outlet. Therefore, assume
C that the product of mass flow and heat capacity is the same at the inlet and
C outlet.
C Variables used:
C     CONVAR(i,1)  temperature of sending node for connection `i'.
C     CONVAR(i,2)  water mass flow rate (kg/s) at sending node for connection `i'.
C     SHTFLD(j,T)  function that returns specific heat of fluid at temperature `T';
C                  j=1 for dry air, j=2 for water vapour, j=3 for for water.
C     PCONDR(i)    ratio of flow rate leaving sending note that reaches receiving
C                  node for connection `i'. It will equal one here, but will be
C                  less than one in the case of a diverging connection.
C---------------------------------------------------------------------------------

        if ( iComponent_code .eq. 80 ) then
C Original SOFC model, 1 connection to node 3
            Twater_enter = CONVAR(ICON3,1)
            Twater_enter_previous = PCTP(ICON3)
            mdotCp_water = PCONDR(ICON3) * CONVAR(ICON3,2)
     &                     * SHTFLD(3,Twater_enter)
        elseif ( iComponent_code .eq. 112 ) then
C Modified SOFC model, 2 connections to node 3
C It is assumed that the first connection to node 3 is to the water tank
C and the second connection is to the adsorption storage unit.

            Twater_enter_tank = CONVAR(ICON3,1)
            Twater_enter_tank_previous = PCTP(ICON3)
            mdotCp_water_tank = PCONDR(ICON3) * CONVAR(ICON3,2)
     &                          * SHTFLD(3,Twater_enter_tank)

            Twater_enter_adsorber = CONVAR(ICON3b,1)
            Twater_enter_adsorber_previous = PCTP(ICON3b)
            mdotCp_water_adsorber = PCONDR(ICON3b) * CONVAR(ICON3b,2)
     &                              * SHTFLD(3,Twater_enter_adsorber)
                            !may need to use other function for pressurized loop?

C The entering water is either from the water tank or from the
C adsorption storage unit (adsorber node 1). The latter occurs if the storage unit
C is in the charging phase only.

            if ( iCharge_signal .eq. 1 ) then  ! fuel cell sends exhaust heat to adsorption storage unit
                                          ! the adsorption storage unit is in charging mode
                Twater_enter = Twater_enter_adsorber
                beta1 = 0.
                beta2 = 1.
            else                          ! fuel cell sends exhaust heat to tank
                Twater_enter = Twater_enter_tank
                beta1 = 1.
                beta2 = 0.
            endif

        endif

C---------------------------------------------------------------------------------
C Ensure that the value of `UA_exh' is realistic. Otherwise, the temperatures
C calculated for the gas and water exiting the heat exchanger will be unrealistic.
C This can lead to divergence within ESP-r's plant matrix solver. Halt the
C simulation if this occurs and advise the user to adjust the coefficients of
C the heat exchanger (UA) polynomial.
C The heat transfer to the water stream as calculated with `UA_exh' has a
C theoretical maximum. The gases exiting the heat exchanger cannot be colder than
C the water entering. Therefore, the theoretical maximum heat transfer to the
C water is equal to the exhaust gas flow rate times the enthalpy difference of
C the exhaust gases as evaluated at T1 (exhaust gas) and Tm (entering water).
C---------------------------------------------------------------------------------
        H_gas_at_Tm = exh_molefrac_N2  * H_gas(Twater_enter,N2,IUOUT)
     &              + exh_molefrac_Ar  * H_gas(Twater_enter,Ar,IUOUT)
     &              + exh_molefrac_O2  * H_gas(Twater_enter,O2,IUOUT)
     &              + exh_molefrac_CO2 * H_gas(Twater_enter,CO2,IUOUT)
     &              + exh_molefrac_H2O * H_gas(Twater_enter,H2O,IUOUT)
        UA_exh_max = m_dot_exh_kmol * (H_exh - H_gas_at_Tm) * 1000.
     &             / (T_exh - Twater_enter)
        IF( UA_exh > UA_exh_max )THEN
          WRITE(IUOUT,*) ' FC_coeff_generator: UA value of heat = '
          WRITE(IUOUT,*) ' exchanger unrealistic. Adjust q0, q1, q2.'
          STOP ' FC_coeff_generator: unresolvable error'
        ENDIF


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.

C Original SOFC model (1 connection to node 3)
C Node coefficient transport for COUT (refer to Beausoleil-Morrison p13):
C       <--self-->|<cross>
C  node   1  2  3 |   m      RHS
C       ---------------------------
C         1  0  0 |   0       7
C         2  3  0 |   0   =   8
C         4  0  5 |   6       9

C Modified SOFC model (2 connections to node 3)
C Node coefficient transport for COUT (refer to Mottillo,p92):
C       <--self-->|<cross>
C  node   1  2  3 |   tank  ads     RHS
C       --------------------------------
C         1  0  0 |   0     0        8
C         2  3  0 |   0     0   =    9
C         4  0  5 |   6     7        10
C---------------------------------------------------------------------------------

C-------Node 1 energy balance
        COUT(1) = m_dot_exh_kmol * Cp_exh              ! self-coupling to itself (W/oC)

        if ( iComponent_code .eq. 80 ) then
           COUT(7) = m_dot_fuel_kmol * H_fuel    * 1000.  ! RHS (W)
     &             + m_dot_air_kmol  * H_air     * 1000.
     &             + m_dot_liqwater_kg * H_water * 1000.
     &             + m_dot_fuel_kmol * LHV_fuel * 1000. * (1.-eta_fc)
     &             - q_heatleak
     &             + m_dot_exh_kmol * Cp_exh * T_refCp
     &             - m_dot_exh_kmol * Href * 1000.
        elseif ( iComponent_code .eq. 112 ) then
           COUT(8) = m_dot_fuel_kmol * H_fuel    * 1000.  ! RHS (W)
     &             + m_dot_air_kmol  * H_air     * 1000.
     &             + m_dot_liqwater_kg * H_water * 1000.
     &             + m_dot_fuel_kmol * LHV_fuel * 1000. * (1.-eta_fc)
     &             - q_heatleak
     &             + m_dot_exh_kmol * Cp_exh * T_refCp
     &             - m_dot_exh_kmol * Href * 1000.
        endif

C-------Node 2 energy balance
        COUT(2) = m_dot_exh_kmol * Cp_exh - UA_exh  ! self-coupling to node 1 (W/oC)
        COUT(3) = - m_dot_exh_kmol * Cp_exit        ! self-coupling to itself (W/oC)
                                                    ! (assumes no leakage of exhaust gases)
        if ( iComponent_code .eq. 80 ) then
           COUT(8) = - UA_exh * Twater_enter           ! RHS (W)
     &             + m_dot_exh_kmol * Cp_exh * T_refCp
     &             - m_dot_exh_kmol * Cp_exit * T_refCp
        elseif ( iComponent_code .eq. 112) then
           COUT(9) = - UA_exh * Twater_enter           ! RHS (W)
     &             + m_dot_exh_kmol * Cp_exh * T_refCp
     &             - m_dot_exh_kmol * Cp_exit * T_refCp
        endif

C-------Node 3 energy balance
        if ( iComponent_code .eq. 80 ) then
           COUT(4) = UA_exh                 ! self-coupling to node 1 (W/oC)
           COUT(5) = - mdotCp_water         ! self-coupling to itself (W/oC)
           COUT(6) = mdotCp_water - UA_exh  ! cross-coupling to node m (W/oC)
           COUT(9) = 0.                     ! RHS (W)
        elseif ( iComponent_code .eq. 112) then
           COUT(4) = UA_exh * (beta1 + beta2)
           COUT(5) = - mdotCp_water_tank - mdotCp_water_adsorber
           COUT(6) = mdotCp_water_tank - (beta1*UA_exh)
           COUT(7) = mdotCp_water_adsorber - (beta2*UA_exh)
           COUT(10) = 0.
        endif

C---------------------------------------------------------------------------------
C       Store heat leakage for injection into containing zone
C---------------------------------------------------------------------------------
        call store_plt_gain ( IPCOMP, q_heatleak, iConvective)

C*********************************************************************************
C Beginning of energy balance for when fuel cell is off **************************
C*********************************************************************************
      ELSE IF(ISTATS==1 .and. .not.FC_onoff) THEN


C---------------------------------------------------------------------------------
C Nodes 1 and 2 will equal room air temperature. Node 3 will equal temperature
C of water flowing into heat exchanger.
C---------------------------------------------------------------------------------
C-------Node 1
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' FC_coeff_generator: the fuel cell must'
          WRITE(IUOUT,*) ' be contained within a room.'
          STOP ' FC_coeff_generator: unresolvable error'
        ELSE
          T_room = PCNTMF(IPCOMP)
        END IF
        COUT(1) = 1.      ! self-coupling to itself (-)
        if ( iComponent_code .eq. 80 ) then
           COUT(7) = T_room  ! RHS (oC)
        elseif ( iComponent_code .eq. 112) then
           COUT(8) = T_room  ! RHS (oC)
        endif

C-------Node 2
        COUT(2) = -1.     ! self-coupling to node 1 (-)
        COUT(3) = 1.      ! self-coupling to itself (-)
        if ( iComponent_code .eq. 80 ) then
           COUT(8) = 0.      ! RHS (oC)
        elseif ( iComponent_code .eq. 112) then
           COUT(9) = 0.      ! RHS (oC)
        endif

C-------Node 3
        COUT(4) = 0.      ! self-coupling to node 1 (oC)
        COUT(5) = 1.      ! self-coupling to itself (-)
        COUT(6) = -1.     ! cross-coupling to node m (-)
        if ( iComponent_code .eq. 80 ) then
           COUT(9) = 0.      ! RHS (oC)
        elseif ( iComponent_code .eq. 112) then
           COUT(7) = 0.      ! cross-coupling to node m2 - adsorber(-)
           COUT(10) = 0.     ! RHS (oC)
        endif

C-------Set additional outputs to appropriate values for inoperative fuel cell.
        eta_fc         = 0.              ! electrical efficiency
        q_net_elec     = 0.              ! net electrical output (W)
        q_par_elec     = 0.              ! parasitic electrical losses (W)
        m_dot_fuel_kg  = 0.              ! fuel consumption (kg/s)
        Vdot_fuel_STP  = 0.              ! fuel consumption (slpm)
        m_dot_air_kg   = 0.              ! air flow (kg/s)
        Vdot_air_STP   = 0.              ! air flow (slpm)
        stoichs        = 0.              ! air flow (stoichs)
        Vdot_liqwater  = 0.              ! water supply (L/min)
        m_dot_exh_kg   = 0.              ! exhaust flow (kg/s)
        Vdot_exh_STP   = 0.              ! exhaust flow (slpm)
        T_exh          = T_room          ! temp of exhaust gases into HX (oC)
        T_exit         = T_room          ! temp of gases at outlet of HX (oC)
        UA_exh         = 0.              ! UA-value of heat exchanger (W)
        phi_source     = 0.              ! heating value of fuel used (W)

        if ( iComponent_code .eq. 80 ) then
            Twater_enter   = CONVAR(ICON3,1) ! temp of water entering heat exchanger (oC)
         elseif ( iComponent_code .eq. 112) then
            Twater_enter_tank =
     &                 CONVAR(ICON3,1)   ! temp of water entering heat exchanger from tank(oC)
            Twater_enter_adsorber=
     &                 CONVAR(ICON3b,1)  ! temp of water entering heat exchanger from adsorber (oC)
        endif

C---------------------------------------------------------------------------------
C       Set heat leakage for injection into containing zone to zero.
C       '1' indicates that gain is convective.
C---------------------------------------------------------------------------------
        call store_plt_gain ( IPCOMP, 0.0, 1)

C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      ELSE IF(ISTATS==2) THEN
C--------Node 1: no balance required so make flow zero.
         COUT(1) = 1.
         if ( iComponent_code .eq. 80 ) then
            COUT(7) = 0.
         elseif ( iComponent_code .eq. 112) then
            COUT(8) = 0.
         endif
C--------Node 2: no balance required so make flow zero.
         COUT(2) = 0.
         COUT(3) = 1.
         if ( iComponent_code .eq. 80 ) then
            COUT(8) = 0.
         elseif ( iComponent_code .eq. 112) then
            COUT(9) = 0.
         endif
C--------Node 3: flow in equals flow out.
         COUT(4) = 0.
         COUT(5) = 1.
         if ( iComponent_code .eq. 80 ) then
            COUT(6) = - PCONDR(ICON3) ! (dimensionless)
            COUT(9) = 0.
         elseif ( iComponent_code .eq. 112) then
            if ( iCharge_signal .ne. 1 .and.
     &        iDischarge_signal .ne. 1 ) then
               COUT(6) = - PCONDR(ICON3) * 2.                  ! (dimensionless)
               COUT(7) = 0.
            elseif ( iCharge_signal .eq. 1 ) then
               COUT(6) = 0.
               COUT(7) = -PCONDR(ICON3b) * 2.
            elseif ( iDischarge_signal .eq. 1 ) then
               COUT(6) = -1. * PCONDR(ICON3b) * 2.
               COUT(7) = 0.
            endif
            COUT(10) = 0.
         endif

C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      ELSE IF(ISTATS==3) THEN
C--------Node 1: no balance required so make flow zero.
         COUT(1) = 1.
         if ( iComponent_code .eq. 80 ) then
            COUT(7) = 0.
         elseif ( iComponent_code .eq. 112) then
            COUT(8) = 0.
         endif
C--------Node 2: no balance required so make flow zero.
         COUT(2) = 0.
         COUT(3) = 1.
         if ( iComponent_code .eq. 80 ) then
            COUT(8) = 0.
         elseif ( iComponent_code .eq. 112) then
            COUT(9) = 0.
         endif
C--------Node 3: no balance required so make flow zero.
         COUT(4) = 0.
         COUT(5) = 1.
         COUT(6) = 0.
         if ( iComponent_code .eq. 80 ) then
            COUT(9) = 0.
         elseif ( iComponent_code .eq. 112) then
            COUT(7) = 0.
            COUT(10) = 0.
         endif
      END IF


C---------------------------------------------------------------------------------
C Electrical network related operations - for ISTATS == 1 only!
C 1. Save real power output in electrical network common block variable
C 2. Call subroutine EMACH to (i) calculate the reactive component of the power
C    produced by the network, and (ii) save this data in the electrical
C    network common block
C
C Modified by Alex Ferguson, Jan 09, 2004.
C---------------------------------------------------------------------------------
      IF (ISTATS .EQ. 1) THEN
         pwrp(ipcomp) = q_net_elec ! Real power output (W), generation is +ive...
         IEMODEL=1              ! Flag indicating which power calculation will be performed
         CALL EMACH(
     &        IPCOMP,           ! Component number (input)
     &        IEMODEL,          ! power calculation flag (input)
     &        pwrp(ipcomp),     ! Real power (input, W)
     &        PQ,               ! Complex power (output, W)
     &        PA)               ! Overall power (output, W)
      ENDIF

C*********************************************************************************
C End of energy and mass balances ************************************************
C*********************************************************************************


C---------------------------------------------------------------------------------
C Save the `additional' output variables for outputting on time-step basis.
C---------------------------------------------------------------------------------
      NAPDAT(IPCOMP)    = 15                          ! number of additional outputs
      PCAOUT(IPCOMP,1)  = eta_fc                      ! electrical efficiency
      PCAOUT(IPCOMP,2)  = q_total_elec                ! total elec demand on FC controller (W)
      PCAOUT(IPCOMP,3)  = q_net_elec                  ! net electrical output (W)
      PCAOUT(IPCOMP,4)  = q_par_elec                  ! parasitic electrical losses (W)
      PCAOUT(IPCOMP,5)  = m_dot_fuel_kg               ! fuel consumption (kg/s)
      PCAOUT(IPCOMP,6)  = Vdot_fuel_STP               ! fuel consumption (slpm)
      PCAOUT(IPCOMP,7)  = m_dot_air_kg                ! air flow (kg/s)
      PCAOUT(IPCOMP,8)  = Vdot_air_STP                ! air flow (slpm)
      PCAOUT(IPCOMP,9)  = stoichs                     ! air flow (stoichs)
      PCAOUT(IPCOMP,10) = Vdot_liqwater               ! water supply (L/min)
      PCAOUT(IPCOMP,11) = m_dot_exh_kg                ! exhaust flow (kg/s)
      PCAOUT(IPCOMP,12) = Vdot_exh_STP                ! exhaust flow (slpm)
      PCAOUT(IPCOMP,13) = T_exh                       ! temp of exhaust gases into HX (oC)
      PCAOUT(IPCOMP,14) = T_exit                      ! temp of gases at outlet of HX (oC)

      if ( iComponent_code .eq. 80 ) then
         PCAOUT(IPCOMP,15) = UA_exh*(T_exh-Twater_enter) ! heat exchange to water (W)
      elseif ( iComponent_code .eq. 112) then
         PCAOUT(IPCOMP,15) = UA_exh*(T_exh-
     &             Twater_enter_tank)*beta1 +            ! heat exchange to water (W)
     &                    UA_exh*(T_exh-
     &             Twater_enter_adsorber) * beta2
      endif


C Debug and testing (begin).
C      if(istats==1)then
C        write(93,*) '    - Total demand on FC   = ',q_total_elec
c        write(93,*) '    - FC electrical output = ',q_net_elec
c        write(93,*) '    - FC thermal output    = ',
c     &                                      UA_exh*(T_exh-Twater_enter)
c        write(93,*) '    - Temp of water extering FC HX = ',Twater_enter
c      endif
C Debug and testing (end).



C---------------------------------------------------------------------------------
C If this coefficient generator has been invoked to generate coefficients for the
C energy balances then save the state of the fuel cell's operation. This logical
C flag will be used the next iteration through the energy balance section of this
C coefficient generator to indicate whether the fuel cell was operational this
C iteration.
C---------------------------------------------------------------------------------
      IF(ISTATS==1) FC_onoff_previous = FC_onoff


C---------------------------------------------------------------------------------
C Store the electrical demand and output data to calculate monthly averages
C and optionally output the data each time-step.
C---------------------------------------------------------------------------------
      IF(ISTATS==1)THEN
        CALL H3KSTORE_FuelCell(
     &        q_total_elec,
     &        q_nonHVAC_elec,
     &        q_HVAC_elec,
     &        q_net_elec,
     &        q_par_elec,
     &        eta_fc,
     &        Vdot_fuel_STP,
     &        Vdot_air_STP,
     &        stoichs,
     &        Vdot_liqwater,
     &        Vdot_exh_STP,
     &        T_exh_previous,
     &        T_exit_previous,
     &        Twater_enter_previous,
     &        UA_exh )
C Call to subroutine to sum plant pump and fan electrical power.
        IF(.NOT.(FC_control==4)) THEN
           CALL H3KSTORE_PLANT_ELEC
        ENDIF
      END IF


C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 3 node fuel cell'
        WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3

        if ( iComponent_code .eq. 80 ) then
           WRITE(ITU,*) ' Connection(s)  ',ICON3
           WRITE(ITU,*) ' Connection data:'
           WRITE(ITU,*) '  - Temp.      = ', Twater_enter,' (oC)'
           WRITE(ITU,*) '  - Heat cap.  = ', mdotCp_water,' (oC)'
        elseif ( iComponent_code .eq. 112) then
           WRITE(ITU,*) '  - Temp.      = ', Twater_enter_tank,' (oC)'
           WRITE(ITU,*) '  - Heat cap.  = ', mdotCp_water_tank,' W/K'
           WRITE(ITU,*) '  - Temp       = ', Twater_enter_adsorber,
     &                                       '  (oC)'
           WRITE(ITU,*) '  - Heat cap   = ', mdotCp_water_adsorber,
     &                                       ' W/K'
        endif

        IF(ISTATS==1) THEN
           WRITE(ITU,*) ' CDATA         = ',CDATA(IPCOMP,1),' (-)'
           WRITE(ITU,*) ' eta_fc        = ',eta_fc,' (-)'
           WRITE(ITU,*) ' q_net_elec    = ',q_net_elec,' (W)'
           WRITE(ITU,*) ' q_par_elec    = ',q_par_elec,' (W)'
           WRITE(ITU,*) ' m_dot_fuel_kg = ',m_dot_fuel_kg,' (kg/s)'
           WRITE(ITU,*) ' m_dot_air_kg  = ',m_dot_air_kg,' (kg/s)'
           WRITE(ITU,*) ' m_dot_exh_kg  = ',m_dot_exh_kg,' (kg/s'
           WRITE(ITU,*) ' T_exh         = ',T_exh,' (oC)'
           WRITE(ITU,*) ' T_exit        = ',T_exit,' (oC)'
           WRITE(ITU,*) ' UA_exh        = ',UA_exh,' (W/K)'
           WRITE(ITU,*) ' T_water       = ',T_water,' (oC)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        if ( iComponent_code .eq. 80 ) then
           WRITE(ITU,*) (COUT(I),I=1,9)
        elseif ( iComponent_code .eq. 112) then
           WRITE(ITU,*) (COUT(I),I=1,10)
        endif

        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving subroutine FC_coeff_generator'
      END IF




      RETURN
      END



C *********************************************************************************************
C ******************************* FC_eloads_convert *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: September 21, 2001
C Copyright CETC 2001

C This subroutine checks the integrity of the user-specified electric
C loads data file and produces a binary format loads file that can
C be used for direct access during the time-step simulation. This is executed
C once per simulation, prior to the time-step calculations. All loads are
C specified in Watts.

C INPUTS:
C    LFClds              name of .fcl file specified by user
C    IFIL                unit number of climate file (ESP-r convention is to number all other
C                        file units relative to IFIL)

C OUTPUTS:
C    FC_lds_input        type of elec loads specified (daily or yearly) (in MODULE FC_ELEC_LOADS)
C    FC_lds_per_rec      number of data points specified for each record of loads data
C                        (in MODULE FC_ELEC_LOADS)
C    IFCloads            unit number of binary file containing time-series of electrical
C                        loads (in MODULE FC_ELEC_LOADS)
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_eloads_convert
      IMPLICIT NONE
#include "SOFC.h"

      COMMON/FILEP/IFIL
      INTEGER	IFIL

      common/Fcell/iFClds,LFClds
      INTEGER iFClds
      CHARACTER LFClds*72

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER	IUOUT,IUIN

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER ND,K,num_recs,num_col,num_incbin,inc,incbi,ire

      CHARACTER OUTSTR*124

      LOGICAL fclerror, fc_loads_exist

      REAL current,left,right
      REAL elecload(12),elecloadb(12)
      INTEGER ISTAT,IER,I
      LOGICAL fc_loads_exists
      INTEGER irec, incbin
C---------------------------------------------------------------------------------
C Read the header information from the user-specified fuel-cell electrical loads
C file (.fcl) and perform some checks to ensure that the correct number of loads
C data are specified. Note that this does not check the validity of the loads,
C data, but rather ensures that the correct number of data items are specified.
C The existence of the .fcl file was confirmed when the .cfg file was read.
C The header data from the .fcl header are placed in MODULE FC_ELEC_LOADS.
C `fclerror' is a flag indicating whether there were errors reading the .fcl file.
C---------------------------------------------------------------------------------
C-----Notify user that fuel cell electric loads being processed (this can take some time).
      call usrmsg(' Processing occupant-driven electric loads...',
     &            ' ','-')
C-----Open the .fcl file.
      fclerror = .false.
      IFCloads_ascii = IFIL+28  ! Should be a safe unit number to use.
      CALL ERPFREE(IFCloads_ascii,ISTAT)
      CALL EFOPSEQ(IFCloads_ascii,LFClds,1,IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error opening FC elec loads input file.'
        fclerror = .true.
      ENDIF
C-----Read the flag indicating the method used for prescribing the loads.
      CALL STRIPC(IFCloads_ascii,OUTSTR,0,ND,1,' FC lds data type',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error reading FC loads input type.'
        fclerror = .true.
      ENDIF
      K=0
      CALL EGETWI(OUTSTR,K,FC_lds_input,1,2,'F',' FC loads type ',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error reading FC loads input type.'
        fclerror = .true.
      ENDIF
C-----Read the number of data points specified for each record of loads data.
C-----Ensure that the maximum number of data points has not been exceeded
C-----for the selected input method.
      CALL STRIPC(IFCloads_ascii,OUTSTR,0,ND,1,' FC loads per hour',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error reading number of data items per record.'
        fclerror = .true.
      ENDIF
      K=0
      CALL EGETWI(OUTSTR,K,FC_lds_per_rec,1,12,'F',
     &            ' FC loads per rec ',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error reading number of data items per record.'
        fclerror = .true.
      ENDIF
      SELECT CASE (FC_lds_input)
        CASE(FC_lds_annual)
          if( FC_lds_per_rec > FC_lds_per_rec_annual )then
            WRITE(IUOUT,*) ' Too many data items per record.'
            fclerror = .true.
          endif
        CASE(FC_lds_daily)
          if( FC_lds_per_rec > FC_lds_per_rec_daily )then
            WRITE(IUOUT,*) ' Too many data items per record.'
            fclerror = .true.
          endif
        CASE DEFAULT
          fclerror = .true.
      END SELECT
C-----Check that the correct number of records of electric loads data are present.
      SELECT CASE (FC_lds_input)
        CASE(FC_lds_annual)
          num_recs = 8760
        CASE(FC_lds_daily)
          num_recs = 1440
        CASE DEFAULT
          fclerror = .true.
      END SELECT
      DO I=1,num_recs
        CALL STRIPC(IFCloads_ascii,OUTSTR,99,num_col,1,
     &              ' Test read of a record',IER)
        if(IER /= 0)then
          WRITE(IUOUT,*) ' End of fuel cell elec loads file.'
          fclerror = .true.
        endif
        if( num_col/=FC_lds_per_rec )then
          WRITE(IUOUT,*) ' Incorrect number of data items per record.'
          fclerror = .true.
        endif
      END DO
C-----Error handling on reading of .fcl file.
      IF(fclerror)THEN
        STOP ' Error in elec loads input file.'
      ENDIF
C-----Reposition .fcl file file to the first record containing loads data.
      REWIND(IFCloads_ascii)
      DO I=1,2
        CALL STRIPC(IFCloads_ascii,OUTSTR,0,ND,1,' skip header',IER)
      END DO


C---------------------------------------------------------------------------------
C Open a file with direct access (i.e. a binary file) that will be used during
C the time-step simulation to set the electrical load on the fuel cell. This
C temporary file will be deleted following completion of the time-step simulation.
C---------------------------------------------------------------------------------
      IFCloads =  IFIL+30
      CALL ERPFREE(IFCloads,ISTAT)
      INQUIRE (UNIT=IFCloads,EXIST=fc_loads_exists) ! check if file exists
      IF (fc_loads_exists ) then
         CALL EFDELET(IFCloads,ISTAT) ! Delete file if it exists
      endif
      SELECT CASE (FC_lds_input)
        CASE(FC_lds_annual)
          CALL FPRAND(IFCloads,ISTAT,FC_lds_per_rec_annual,3,
     &                'FCloads.tmp')  ! File may exist if prev. simulation was
                                      ! interrupted
        CASE(FC_lds_daily)
          CALL FPRAND(IFCloads,ISTAT,FC_lds_per_rec_daily,3,
     &                'FCloads.tmp')
        CASE DEFAULT  ! Clear by now that FC_lds_input is valid.
      END SELECT


C---------------------------------------------------------------------------------
C Iterate through each record of electric loads data. For each record of data in
C .fcl ASCII file, read the load at each time increment specified. Write the
C loads data to the binary file at the pre-determined time resolution
C (i.e. 5 minutes for `FC_lds_annual' and 10 seconds for `FC_lds_daily'. If user
C has specified loads at a coarser time increment, then assume the load is
C constant over a number of time increments when creating the binary file.
C---------------------------------------------------------------------------------
C-----Set number of increments in binary file.
      SELECT CASE (FC_lds_input)
        CASE(FC_lds_annual)
          FC_lds_per_rec_bin = FC_lds_per_rec_annual
        CASE(FC_lds_daily)
          FC_lds_per_rec_bin = FC_lds_per_rec_daily
        CASE DEFAULT  ! Clear by now that FC_lds_input is valid.
      END SELECT

C-----Iterate through each record in the .fcl ASCII file.
      DO irec=1,num_recs
C-------Read the loads data from the ASCII file.
        READ(IFCloads_ascii,*) (elecload(inc),inc=1,FC_lds_per_rec)
C-------If user time-increment differs from time increment in binary file,
C-------then spread user data over number of time increments in binary file.
C-------Uses a step-function approach.
        DO inc=1,FC_lds_per_rec
          DO incbin=1,FC_lds_per_rec_bin
            current = float(incbin) / float(FC_lds_per_rec_bin)
            left    = float(inc-1)  / float(FC_lds_per_rec)
            right   = float(inc)    / float(FC_lds_per_rec)
            if( current > left .and. current <= right ) then
              elecloadb(incbin) = elecload(inc)
            endif
          END DO
        END DO
C-------Write the loads data to the binary file.
        WRITE(IFCloads,REC=irec,IOSTAT=ISTAT)
     &       (elecloadb(incbin),incbin=1,FC_lds_per_rec_bin)
      END DO
C-----Write header information at end of binary file.
      WRITE(IFCloads,REC=8761,IOSTAT=ISTAT)
     &      FC_lds_input,FC_lds_per_rec


C Processing complete. Leave binary file open for use during time-step simulation.
      call usrmsg(' Processing occupant-driven electric loads...done.',
     &            ' ','-')

      RETURN
      END



C *********************************************************************************************
C ******************************* FC_eloads_establish *****************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: September 24, 2001
C Copyright CETC 2001

C This subroutine establishes the electrical load demanded from the fuel cell controller
C for the current time-step. This is not necessarily how much power the fuel cell must
C output. Subroutine FC_eloads_manage will control how the fuel cell will operate in
C response from this demand placed on the controller.
C This subroutine reads the non-HVAC loads data from the binary file created by
C FC_eloads_convert. The loads drawn by HVAC equipment the current time-step are also
C determined here.
C This subroutine is executed each time the fuel cell coefficient generator is executed, which
C may be multiple times per plant simulation increment.

C Electric draw of electric boilers and baseboards not currently being tracked.

C INPUTS:
C    FC_lds_input        type of elec loads specified (daily or yearly) (in MODULE FC_ELEC_LOADS)
C    FC_lds_per_rec      number of data points specified for each record of loads data
C                        (in MODULE FC_ELEC_LOADS)
C    IFCloads            unit number of binary file containing time-series of electrical
C    btimep              `present' time of current day in hour fraction (0. to 24.) for
C                        current building simulation increment (standard ESP-r variable taken
C                        from COMMON/BTIME)
C    idyp                number of present day (0 to 365) (standard ESP-r variable taken from
C                        COMMON/SIMTIM)
C    ptimef              `future' time of current day in hour fraction (0. to 24.) for
C                        current plant simulation increment (standard ESP-r variable taken
C                        from COMMON/ptime).
C    FCctl_HRV_draw        Power draw of HRV, W (in MODULE FC_ctl_h3khvacloads)
C    FCctl_AC_compressor   Power draw of A/C compressor and condensor fan, W
C                          (in MODULE FC_ctl_h3khvacloads)
C    FCctl_AC_circfan      Power draw of fan that circulates cool air to house, W
C                          (in MODULE FC_ctl_h3khvacloads)
C    FCctl_furnace_draft   Power draw of furnace draft fan, W (in MODULE FC_ctl_h3khvacloads)
C    FCctl_furnace_circfan Power draw of fan that circulates warm air to house, W
C                          (in MODULE FC_ctl_h3khvacloads)
C    FCctl_pumps           Power draw of pumps defined in the plant network, W
C                          (in MODULE FC_ctl_h3khvacloads)
C    FCctl_fans            Power draw of fans defined in the plant network, W
C                          (in MODULE FC_ctl_h3khvacloads)
C    q_par_elec            Parasitic electric losses of fuel cell, W
C    FCctl_boiler_draft    Power draw of boiler draft fan, W (in MODULE FC_ctl_h3khvacloads)
C    FCctl_boiler_circfan  Power draw of boiler circulating fan, W (in MODULE
C                          FC_ctl_h3khvacloads). This does not make sense but
C                          track anyway.
C    FCctl_baseboard_fan   Power draw of baseboard circulating fan, W (in MODULE
C                          FC_ctl_h3khvacloads).
C    FCctl_ashp_compressor Power draw of heat pump compressor and condenser fan (air
C                          source heat pump in heating mode), W
C                          (in MODULE FC_ctl_h3khvacloads)
C    FCctl_ashp_circfan    Power draw of air source heat pump (heating) circulating fan.
C                          This variable also contains the power draw of the backup system
C                          circulating fan, if applicable, W (im MODULE FC_ctl_h3khvacloads).
C                          (in MODULE FC_ctl_h3khvacloads).
C    FCctl_gshp_pump       Power draw of pump that circulates fluid from the ground loop
C                          to the heat pump, W (in MODULE FC_ctl_h3khvacloads)
C    FCctl_TAC_draw        Power draw of thermally activated cooling equipment, W (in
C                          MODULE FC_ctl_h3khvacloads)
C    FCctl_elec_water_tank Power draw of electrically heated water tank, W (in MODULE
C                          FC_ctl_h3khvacloads)

C OUTPUTS:
C    q_nonHVAC_elec        The non-HVAC electrical load for the current plant simulation
C                          increment (W).
C    q_HVAC_elec           An estimate of the HVAC electrical load for the current plant
C                          simulation increment (W). Is is only an estimate as it depends
C                          on the state of other plant components that are being solved
C                          concurrently.
C -------------------------------------------------------------------------------------------

c      SUBROUTINE FC_eloads_establish(IDYP,BTIMEP,PTIMEF,q_nonHVAC_elec,
c     &                               q_HVAC_elec)
      SUBROUTINE FC_eloads_establish(IDYP,BTIMEP,PTIMEF,q_nonHVAC_elec,
     &                               q_HVAC_elec)
      IMPLICIT NONE
#include "SOFC.h"
#include "plant.h"

C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------
      COMMON/OUTIN/IUOUT,IUIN           ! Number for default output and input screens
      INTEGER	IUOUT,IUIN

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL    ADATA                     !-array containing component data
      REAL    BDATA                     !-array containing component data

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP                    ! Number of components in the plant network
      INTEGER NCI                       ! Not used in this subroutine
      REAL CDATA                        ! Not used in this subroutine

      common/c12ps/
     &     npcdat(mpcom,9),             ! miscellaneous component data
     &     ipofs1(mcoefg),              ! component coefficient locations (rows)
     &     ipofs2(mcoefg,mpvar)         ! component coefficient locations (cols)
      INTEGER NPCDAT
      INTEGER IPOFS1
      INTEGER IPOFS2
C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER record,incbin,after_midnight
      REAL elecloadb(12),frac_hr,frac_min,tenth
      REAL q_nonHVAC_elec,q_HVAC_elec
      LOGICAL CLOSE
      INTEGER IDYP,ISTAT
      REAL BTIMEP,PTIMEF


C=================================================================================
C First determine the non-HVAC electrical load.
C=================================================================================

C---------------------------------------------------------------------------------
C Determine which record of the binary file contains the non-HVAC electrical loads data
C for the `future' time that is currently being simulated in the explicit plant
C domain. There are two possible formats for the binary file: one record for each
C hour of the year (indicated by `FC_lds_annual') or one record for each minute
C of a typical day (indicated by `FC_lds_daily').
C---------------------------------------------------------------------------------
      SELECT CASE (FC_lds_input)

C---------------------------------------------------------------------------------
C One record for each hour of the year.
C The hour of the year defines the record in the file: the first
C record of the binary file contains the data for 0h00m00s to 0h59m59s of
C January 1; the 8760th record contains the data for 23h00m00s to 23h59m59s of
C December 31.
C Special attention has to be paid when the `future' time in the plant domain
C crosses over midnight, as the `present' day will not have yet been incremented.
C When this occurs, the `present' time in the building domain will be 24 hours
C and the `future' time in the plant domain will be less than or equal to 1 hour.
C Special attention must also be be paid when the `present' time in the building
C domain (BTIMEP) crosses over midnight and when sub-hourly building time-steps
C are used. In this case, the `present' day (IDYP) does not increment until
C BTIMEP=1.0.
C The electrical load at 24h00 is taken as the load at 0h00 of the next day
C (as described above, each record covers the period from 0h00m00s to 23h59m59s).
C Care has to be taken when the simulation crosses over the New Year: the load for
C midnight December 31 is set using the data at 0h00 on January 1.
C---------------------------------------------------------------------------------
        CASE(FC_lds_annual)
C---------Have we just crossed over midnight?
          CALL ECLOSE(BTIMEP,24.0,0.0001,CLOSE)
          IF( CLOSE .and. PTIMEF<=1. )THEN
            after_midnight = 24
          ELSEIF( BTIMEP<1.0 )THEN  ! IDYP does not increment until BTIMEP=1.0.
            after_midnight = 24
          ELSE
            after_midnight = 0
          END IF
C---------Determine which record to use.
          record = 24*(IDYP-1) + INT(PTIMEF) + 1 + after_midnight
C---------Has midnight on New Year's eve just struck?
          CALL ECLOSE(PTIMEF,24.0,0.0001,CLOSE)
          IF( CLOSE .and. IDYP==365 )THEN
            record = record - 8760
          END IF
          CALL ECLOSE(BTIMEP,24.0,0.0001,CLOSE)
          IF( CLOSE .and. PTIMEF<=1. .and. IDYP==365 )THEN
            record = record - 8760
          END IF
          IF( BTIMEP<1.0 .and. IDYP==365 )THEN ! IDYP does not increment until BTIMEP=1.0.
            record = record - 8760
          END IF
C---------Read the loads for each time increment for the current hour.
          READ(IFCloads,REC=record,IOSTAT=ISTAT)
     &        (elecloadb(incbin),incbin=1,FC_lds_per_rec_bin)
C---------Now determine which of these increments corresponds to the current
C---------time-step. `frac_hr' represents the fraction of an hour that we are
C---------past the top of the hour.
          frac_hr = PTIMEF - AINT(PTIMEF)                        ! AINT truncates a real.
          incbin = INT( frac_hr*FC_lds_per_rec_bin + 0.001 ) + 1 ! 0.001 prevents rounding error.
C---------Set the electrical load for the current time-step.
          q_nonHVAC_elec = elecloadb(incbin)

C---------------------------------------------------------------------------------
C One record for each minute of a typical day (used for very short time-step
C simulations).
C The minute of the day defines the record in the file: the first record contains
C the data for 00h00m00s to 00h00m59s; the 1440th record contains the data for
C 23h59m00s to 23h59m59.
C---------------------------------------------------------------------------------
        CASE(FC_lds_daily)
C---------Determine which record to use.
          tenth = 0.1 / 60.  ! Tenth of sec: used because PTIMEF sometimes has rounding errors.
          record = INT( PTIMEF*60. + tenth ) + 1
C---------Has midnight just struck in the plant domain?
          CALL ECLOSE(PTIMEF,24.0,0.0001,CLOSE)
          IF( CLOSE ) record = 1
C---------Read the loads for each time increment for the current minute.
          READ(IFCloads,REC=record,IOSTAT=ISTAT)
     &        (elecloadb(incbin),incbin=1,FC_lds_per_rec_bin)
C---------Now determine which of these increments corresponds to the current
C---------time-step. `frac_min' represents the fraction of a minute that we are
C---------past the top of the minute.
          frac_min = (PTIMEF*60. + tenth) - AINT( PTIMEF*60. + tenth )
          incbin = INT( frac_min*FC_lds_per_rec_bin + 0.001 ) + 1 ! 0.001 prevents rounding error.
C---------Set the electrical load for the current time-step.
          q_nonHVAC_elec = elecloadb(incbin)

C---------------------------------------------------------------------------------
        CASE DEFAULT  ! This cannot happen as there are error traps upstream.
      END SELECT

C-----Update non HVAC loads for time step
      q_nonHVAC_elec = q_nonHVAC_elec

C-----If the updated non HVAC load is less than 0., then set to 0. and
C-----issue warning to user
      IF(q_nonHVAC_elec.LT.0.) THEN
        q_nonHVAC_elec = 0.
        WRITE(IUOUT,*) 'Warning: Total load for fuel burning appliances'
        WRITE(IUOUT,*) 'from .lp files is greater than total electrical'
        WRITE(IUOUT,*) 'load from .fcl file on day ',IDYP,' and hour',
     &  BTIMEP
      ENDIF

C=================================================================================
C Non-HVAC loads have been determined. Now determine loads from HVAC equipment.
C The five HVAC systems supported in FCT RFC v1 are handled here. This block of
C code must be expanded if additional HVAC equipment is added in the future.
C As ESP-r solves the building thermal domain prior to the plant domain, the
C electrical draws of the HOT3000 HVAC equipment (furnace, air-air HP, HRV) have
C already been determined.
C The power draw of the explicit plant equipment will be one time-step in arrears
C on the first iteration through the plant matrix generator each plant time-step.
C Therefore if the plant matrix converges on its first iteration, then the
C explicit plant power draws will be one time-step in arrears for that time-step
C solution.
C=================================================================================
      q_HVAC_elec = FCctl_HRV_draw                                ! HRV
     &            + FCctl_AC_compressor + FCctl_AC_circfan        ! A/C (air-air HP)
     &            + FCctl_furnace_draft + FCctl_furnace_circfan   ! Furnace
     &            + FCctl_pumps                                   ! Pumps
     &            + FCctl_fans                                    ! Fans
     &            + q_par_elec                                    ! FC parasitic losses
     &            + FCctl_boiler_draft + FCctl_boiler_circfan     ! Boiler
     &            + FCctl_baseboard_fan                           ! Baseboards
     &            + FCctl_ashp_compressor + FCctl_ashp_circfan    ! ASHP (heating)
     &            + FCctl_gshp_pump                               ! GSHP
     &            + FCctl_TAC_draw                                ! TAC
     &            + FCctl_elec_water_tank                         ! Electric hot water tank


c-----Store q_total_elec in module FC_ctl_h3khvacloads.
      fCurrent_TS_q_total_elec = q_nonHVAC_elec + q_HVAC_elec


      RETURN
      END



C *********************************************************************************************
C ******************************* FC_eloads_manage *****************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: September 27, 2001
C Copyright CETC 2001

C This subroutine determines the electrical output of the fuel cell for the current time-step.
C Subroutine FC_eloads_establish determines the electrical load demanded from the fuel cell
C controller. This subroutine simulates the behaviour of this controller. Currently four
C control laws are supported.


C INPUTS:
C    q_total_elec        total electric load (W) demanded from fuel cell electric controller

C OUTPUTS:
C    q_net_elec          net electrical output (W) from fuel cell
C--------------------------------------------------------------------------------------------

      SUBROUTINE FC_eloads_manage
      IMPLICIT NONE
#include "plant.h"
#include "SOFC.h"

      SELECT CASE(FC_control)

C---------------------------------------------------------------------------------
C Fuel cell is controlled to give priority to the house's electrical demands.
C Fuel cell output is made to equal total demand placed upon controller, subject
C to operating limits defined by user. Run fuel cell at maximum capacity if demand
C exceeds maximum capacity. Turn fuel cell off if demand is less than minimum
C operating capacity.
C---------------------------------------------------------------------------------
        CASE(FC_ctl_elec_priority)
          IF( q_total_elec > FC_elec_max_output )THEN
            q_net_elec = FC_elec_max_output
          ELSEIF( q_total_elec < FC_elec_min_output )THEN
            q_net_elec = 0.
          ELSE
            q_net_elec = q_total_elec
          ENDIF


C---------------------------------------------------------------------------------
C Fuel cell is controlled to give priority to space heating and DHW thermal loads.
C Control fuel cell based on previous demands on tank's burner or element input.
C---------------------------------------------------------------------------------
        CASE(FC_ctl_thermal_priority)
           CALL FC_thermal_priority_type1


C---------------------------------------------------------------------------------
C Fuel cell is controlled to give constant electrical output.
C---------------------------------------------------------------------------------
        CASE(FC_ctl_const_output)
           q_net_elec = FC_elec_max_output


C---------------------------------------------------------------------------------
C Modification: Additional case added to support external controller.
C               In this case, the fuel cell's electric output is determined by an
C               external controller. We just need to determine if the signal sent
C               by the external controller is within the fuel cell's operating
C               range. (Same code as FC_ctl_elec_priority case)
C
C Modified by Alex Ferguson on April 14, 2003.
C---------------------------------------------------------------------------------
        CASE(FC_ctl_ext)
          IF( q_total_elec > FC_elec_max_output )THEN
            q_net_elec = FC_elec_max_output
          ELSEIF( q_total_elec < FC_elec_min_output )THEN
            q_net_elec = 0.
          ELSE
            q_net_elec = q_total_elec
          ENDIF


        CASE DEFAULT  ! can't happen: range on FC_ctl_elec_priority checked in static template.
      END SELECT

      RETURN
      END



C *********************************************************************************************
C **************************** FC_thermal_priority_type1 **************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 13, 2001
C Copyright CETC 2001

C This subroutine applies the `type 1' thermal priority control on the fuel cell. For the
C FCT project this is the only type of thermal priority control, but others will be added
C in the future.

C This subroutine determines the electrical output of the fuel cell for the current time-step.
C It determines the amount of energy added by the water tank's heater (gas-fired or
C electrical) to the water over the past `avg_per' hours. It then controls the fuel
C cell's output in an attempt to minimize the amount of energy required from the tank's
C heater to maintain the tank within its operating temperature range.


C INPUTS:
C    q_total_elec      total electric load (W) demanded from fuel cell electric controller
C    TIMSEC            time-step in explicit plant domain (seconds)
C    ntstep            number of simulation time-steps/hour in building domain (in common pers)
C    ntstpp            number of plant domain simulation time steps/building domain time-steps
C                      (in common pctstp)
C    NSINC             simulation time increment in building domain (in common/SIMTIM)
C    bpltcon           logical variable indicating whether plant domain converged previous
C                      iteration (in common/pltcon)
C    avg_per           time period over which running averages will be calculated, hours
C                      (in MODULE FC_controls)
C    FC_avg_steps & FC_avg_steps_rampup are the number of simulation time-steps in plant
C                      domain over which running averages calculated (in MODULE FC_controls)
C    FC_tank_energy_capture_avg, FC_heatX_energy_capture_avg, FC_tank_temp_avg, FC_net_elec_avg are
C                      running averages (in MODULE FC_controls)
C    FC_tank_energy_capture, FC_heatX_energy_capture, FC_tank_temp, FC_net_elec are arrays
C                      holding data used to calculate the running averages (in MODULE FC_controls)
C    fPreviousTS_?_??  variables holding previous time-step solved quantities
C    (in MODULE H3KSTORE_FC_DATA)
C    FC_therm_ctl_minT and FC_therm_ctl_maxT are the temperature control band for the fuel cell
C                      operation (in MODULE FC_controls)
C
C OUTPUTS:
C    q_net_elec          net electrical output (W) from fuel cell
C
C--------------------------------------------------------------------------------------------

      SUBROUTINE FC_thermal_priority_type1
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "SOFC.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER	IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER	ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/PCTIME/TIMSEC
      REAL	TIMSEC

      common/pltcon/bpltcon,LastTSTP
      logical bpltcon  ! Boolean indicating whether plant domain has converged.
      real LastTStp ! not used
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      INTEGER isd1,ism1,isd2,ism2,isds,isdf,ntstep

      COMMON/PCTSTP/NTSTPP
      INTEGER	NTSTPP

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER	IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER num_items,i
      REAL total1,total2,total3,total4,ratio,target


C---------------------------------------------------------------------------------
C Determine the number of simulation time-steps in the plant domain over which
C running averages will be calculated. These running averages are used by the
C fuel cell controller. Calculate this only once, on the first simulation
C increment. Calculated here rather than in FC_static_template because
C `ntstep' and `ntstpp' unknown when FC_static_template called.
C Variables used:
C    avg_per    time period over which running averages will be calculated (hours)
C    ntstep     number of simulation time-steps/hour in building domain
C    ntstpp     number of plant domain simulation time steps/building domain time-steps
C---------------------------------------------------------------------------------
      IF(NSINC==1) FC_avg_steps = avg_per * ntstep * ntstpp


C---------------------------------------------------------------------------------
C Check whether the plant domain converged following the previous iteration
C through the component coefficient generators. If so, add the previous time-step's
C converged solution to the running averages, update the running averages, and
C apply the control logic. If the solution has not yet converged, then we are at
C the same point in time as the previous call to this subroutine. In that case,
C do not alter the fuel cell's operation.
C---------------------------------------------------------------------------------
      IF(bpltcon) THEN

C---------------------------------------------------------------------------------
C Plant domain converged previous iteration. Therefore, we have just stepped
C forward in time. Add previous time-step's converged solution to the running
C averages. The treatment depends on whether sufficient simulation time-steps
C have passed to fully populate the averaging vectors.
C Averaging vectors (not all used in control logic below, but retained for
C future use):
C     FC_tank_energy_capture  energy added by tank burner or elec element to
C                             tank water (J).
C     FC_heatX_energy_capture energy transferred from FC exhaust to water
C                             through heat exchanger (J).
C     FC_tank_temp            temperature of water in storage tank (oC).
C     FC_net_elec             fuel cell net electrical output (J).
C     num_items               number of items in each averaging vector.
C---------------------------------------------------------------------------------
        IF( FC_avg_steps==FC_avg_steps_rampup )THEN
C---------Vectors fully populated: drop oldest item from each vector.
          DO i=1,FC_avg_steps
            FC_tank_energy_capture(i)  = FC_tank_energy_capture(i+1)
            FC_heatX_energy_capture(i) = FC_heatX_energy_capture(i+1)
            FC_tank_temp(i)            = FC_tank_temp(i+1)
            FC_net_elec(i)             = FC_net_elec(i+1)
          END DO
C---------Now add one new item to end of each vector.
          FC_tank_energy_capture(FC_avg_steps) =
     &                             fPreviousTS_q_FF_capture_fut * TIMSEC ! Either gas-fired or
     &                           + fPreviousTS_q_EL_capture_fut * TIMSEC ! elec value will be zero.
          FC_heatX_energy_capture(FC_avg_steps) =
     &                             fPreviousTS_fHeatToWater * TIMSEC
          FC_tank_temp(FC_avg_steps) = fPreviousTS_T_FF_node1_fut        ! Either gas-fired or
     &                               + fPreviousTS_T_EL_Node1_fut        ! elec value will be zero.
          FC_net_elec(FC_avg_steps) = fPreviousTS_q_net_elec * TIMSEC
          num_items = FC_avg_steps

        ELSE
C---------Still in ramp-up period: add one more item to each vector.
          FC_tank_energy_capture( FC_avg_steps_rampup+1 ) =
     &                             fPreviousTS_q_FF_capture_fut * TIMSEC
     &                           + fPreviousTS_q_EL_capture_fut * TIMSEC
          FC_heatX_energy_capture( FC_avg_steps_rampup+1 ) =
     &                             fPreviousTS_fHeatToWater * TIMSEC
          FC_tank_temp( FC_avg_steps_rampup+1 ) =
     &                                        fPreviousTS_T_FF_node1_fut
     &                                      + fPreviousTS_T_EL_Node1_fut
          FC_net_elec( FC_avg_steps_rampup+1 ) =
     &                                   fPreviousTS_q_net_elec * TIMSEC
          FC_avg_steps_rampup = FC_avg_steps_rampup + 1
          num_items = FC_avg_steps_rampup
        END IF

C---------------------------------------------------------------------------------
C Calculate the average quantities over the averaging period. Only update the
C averages if the plant domain converged following the previous iteration
C through the component coefficient generators.
C---------------------------------------------------------------------------------
        total1 = 0.
        total2 = 0.
        total3 = 0.
        total4 = 0.
        DO i=1,num_items
          total1 = total1 + FC_tank_energy_capture(i)
          total2 = total2 + FC_heatX_energy_capture(i)
          total3 = total3 + FC_tank_temp(i)
          total4 = total4 + FC_net_elec(i)
        END DO
        FC_tank_energy_capture_avg  = total1 / float(num_items)
        FC_heatX_energy_capture_avg = total2 / float(num_items)
        FC_tank_temp_avg            = total3 / float(num_items)
        FC_net_elec_avg             = total4 / float(num_items)


C---------------------------------------------------------------------------------
C Now control the fuel cell based on the calculated average quantities and
C instantaneous quantities.
C There are five operating strategies:
C   1) The tank burner/heating element has been operating over past `avg_per'
C      hours AND current tank temperature is below FC_therm_ctl_maxT AND the fuel
C      cell has been operating over the past `avg_per' hours. Increase FC
C      electrical output in an attempt to displace the burner/heating element
C      energy by the energy recovered from the FC heat exchanger. Make sure
C      that fuel cell operational range respected.e
C   2) The tank burner/heating element has been operating over past `avg_per'
C      hours AND current tank temperature is below FC_therm_ctl_maxT BUT the fuel
C      cell has not been operating over the past `avg_per' hours. Turn fuel
C      cell on at its minimum operating point.
C   3) The tank burner/heating element has not been operating over past `avg_per'
C      hours AND current tank temperature is below FC_therm_ctl_minT. Increase
C      fuel cell output by 10% in attempt to maintain tank temperature between
C      FC_therm_ctl_minT and FC_therm_ctl_maxT without need for burner/heating
C      element.
C   4) The current tank temperature is above FC_therm_ctl_maxT. Reduce
C      fuel cell output by 10% in attempt to reduce tank temperature.
C   5) If none of the above conditions are met, then system is operating within
C      desired range. Therefore, do not change operation point.
C---------------------------------------------------------------------------------
C-------Operating strategy 1.

        IF( FC_tank_energy_capture_avg>0.
     &                  .and. FC_tank_temp(num_items)<=FC_therm_ctl_maxT
     &                  .and. FC_net_elec_avg>0.
     &                  .and. FC_heatX_energy_capture_avg>0.)THEN


          ratio = ( FC_tank_energy_capture_avg
     &              +FC_heatX_energy_capture_avg )
     &           / FC_heatX_energy_capture_avg
          target = ratio * FC_net_elec_avg / TIMSEC  ! target operating point (W)
          IF( target > FC_elec_max_output )THEN
            q_net_elec = FC_elec_max_output
          ELSEIF( target < FC_elec_min_output )THEN
            q_net_elec = 0.
          ELSE
            q_net_elec = target
          ENDIF

C-------Operating strategy 2.
        ELSEIF( FC_tank_energy_capture_avg>0.
     &                  .and. FC_tank_temp(num_items)<=FC_therm_ctl_maxT
     &                  .and. FC_net_elec_avg==0. )THEN
          q_net_elec = FC_elec_min_output

C-------Operating strategy 3.
        ELSEIF( FC_tank_energy_capture_avg==0.
     &              .and. FC_tank_temp(num_items)<FC_therm_ctl_minT)THEN
          target = 1.1 * q_net_elec_previous
          IF( target > FC_elec_max_output )THEN
            q_net_elec = FC_elec_max_output
          ELSEIF( target < FC_elec_min_output )THEN
            q_net_elec = FC_elec_min_output
          ELSE
            q_net_elec = target
          ENDIF



C-------Operating strategy 4.
        ELSEIF( FC_tank_temp(num_items)>FC_therm_ctl_maxT )THEN
          target = 0.9 * q_net_elec_previous
          IF( target < FC_elec_min_output )THEN
            q_net_elec = 0.
          ELSE
            q_net_elec = target
          ENDIF


C-------Operating strategy 5.
        ELSE
          q_net_elec = q_net_elec_previous

        END IF
C-------Save fuel cell's operating point for next time-step.
        q_net_elec_previous = q_net_elec


        IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF
     &       .AND.ITRACE(37)/=0)THEN
           WRITE(ITU,*) ' Fuel cell thermal controller data:'
           WRITE(ITU,*) '    -- Plant converged: ', bpltcon
c           WRITE(ITU,*) '    -- operating strategy ', iOS
           WRITE(ITU,*) '    -- Tank T  : ', FC_tank_temp(num_items)
           WRITE(ITU,*) '    -- burner Q: ', FC_tank_energy_capture_avg
           WRITE(ITU,*) '    -- FC Q    : ', FC_heatX_energy_capture_avg
           WRITE(ITU,*) '    -- Tank T  : ', FC_tank_temp_avg
           WRITE(ITU,*) '    -- FC out v: ', FC_net_elec_avg
           WRITE(ITU,*) '    -- FC out p: ', q_net_elec_previous
           WRITE(ITU,*) '    -- FC out n: ', q_net_elec
        ENDIF


      ELSE
C---------------------------------------------------------------------------------
C Plant domain has not yet converged. Maintain the operating point established at
C the first iteration for this time-step.
C---------------------------------------------------------------------------------
        q_net_elec = q_net_elec_previous

      END IF




      RETURN
      END



C *********************************************************************************************
C ***************************** FC_pump_fan_comp_number ***************************************
C Created by: Julia Purdy
C Initial Creation Date: February 27, 2002
C Modified by Maria Mottillo: March 12, 2003 and September 8, 2006
C Copyright CETC 2001

C This subroutine associates any pumps and fans with their respective component number - IPCOMP,
C which is dependent on the user-generated model. This subroutine is called once at the beginning
C of the simulation and is required to account for the pump and fan electrical energy in
C the HOT3000 outputs.


C INPUTS:
C    NPCDAT              the number of plant components in the user defined model
C    IPCOMP              index number of the component under consideration

C OUTPUTS:
C    pump_IPCOMP         array holds component number for first pump modelled, if applicable
C    fan_IPCOMP          component number for first fan modelled, if applicable
C--------------------------------------------------------------------------------------------

      SUBROUTINE FC_pump_fan_comp_number
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"

C Common blocks used for Plant Additional Outputs (PCAOUT)
C MPCOM       maximum number of plant components in configuration (set to 50 in plant.h)
C MPCRES      maximum number of additional plant component outputs (set to 15 in plant.h)
C IPCOMP      index number of the component under consideration
C NPCDAT(?,4) plant component model type number for component ? (equal to NPCDAT(?,3)/10)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

      INTEGER I,J

C Initialize the number of fans and pumps to zero
           Num_fans  = 0
           Num_pumps = 0

C Inititalize component-specific numbers to zero
C Maximum size of arrays holding the fan and pump component numbers is 50.
           DO J = 1, 50
              fan_IPCOMP(J)  = 0
              pump_IPCOMP(J) = 0
           END DO

c Cycle through all of the specified plant components, where
c NPCOMP is the number of plant components simulated.
           DO I=1, NPCOMP

c Count the number of fans specified in the plant network.
C If a fan has been specified in the explicit plant model (NPCDAT(I, 4) == 3), then
c save the component number NPCOMP (where the fan is located) in the fan-specific
C component number array.
              IF (NPCDAT(I, 4) == 3) THEN
                Num_fans = Num_fans + 1
                fan_IPCOMP(Num_fans) = I
              END IF

C Count the number of pumps specified in the plant network.
c If a pump has been specified in the explicit plant model (NPCDAT(I, 4) == 24 or
C NPCDAT(I, 4) = 117), then save the component number NPCOMP (where the pump
C is located) to the pump-specific component number array.
              IF (NPCDAT(I, 4) == 24 .or. NPCDAT(I, 4) == 117) THEN
                Num_pumps = Num_pumps + 1
                pump_IPCOMP(Num_pumps) = I
              END IF

           END DO

      RETURN
      END




C *******************************************************************************************
C ***************************** FC_data_transport *******************************************
C Created by: Alex Ferguson
C Initial Creation Date: June 24, 2004
C
C Copyright CETC 2004
C
C This subroutine transfers miscellaneous (non-state-variable) data from the fuel cell
C model to the h3kreports object. The relevant data is stored in the fc_calculated
C commonblock, and transported using the h3kmodule's AddToReport routines.
C
C--------------------------------------------------------------------------------------------

      Subroutine FC_data_transport(iComponentIndex)
      use h3kmodule
      implicit none
#include "plant.h"
#include "SOFC.h"

C.....Passed variables
      integer iComponentIndex   ! index of FC in plant component network

C.....ESP-r Commons
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15 pcname

      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

      COMMON/Stgdata/iCharge_signal,iDischarge_signal
      INTEGER iCharge_signal    ! charging signal for adsorption storage unit (on/off)
      INTEGER iDischarge_signal ! discharge signal for adsorption storage unit (on/off)


C.....Local Variables
      character*128 hold_name    ! temporary variable for string manipulation
      logical bClose             ! flag for close-to-zero comparison
      integer hold_len           ! The lenght of the hold name

C.....References
      real  H3K_connect_property ! function returning data for a given connection

C.....Named constants
      integer iTemperature      ! named constant for temperature properity
      integer i1st_flow         ! named constant for first phase flow properity
      integer i2nd_flow    ! named constant for second phase flow properity
      parameter ( iTemperature     = 1,
     &            i1st_flow        = 2,
     &            i2nd_flow        = 3)

C.....State variables
C CSVF(MPNODE,MPVAR),  ! Plant future state variables
C CSVP(MPNODE,MPVAR)   ! Plant present state variables
      common/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      real CSVF, CSVP

C.....Plant component present/future variables
      common/pcvar/pctf(mpcon),pcrf(mpcon),puaf(mpnode),pcqf(mpnode),
     & pcntmf(mpcom),pctp(mpcon),pcrp(mpcon),puap(mpnode),
     & pcqp(mpnode),pcntmp(mpcom)
      real pctf,pcrf,puaf,pcqf,pcntmf,pctp, pcrp, puap, pcqp, pcntmp

C.....Local variables
      real T_exh_recent         !  most recent value of exhaust temp
      real T_exit_recent        !  most recent value of exhaust exit temp
      real Twater_recent        !  most recent value of coolant entering temp
      INTEGER LNBLNK

C------------------------------------------------------------------------
C     Get most recent data:
C------------------------------------------------------------------------
      T_exh_recent = CSVF(inod1,iTemperature)
      T_exit_recent = CSVF(inod2,iTemperature)
      if ( NPCDAT(iComponentIndex,4).eq. 112 .and.
     &     iCharge_signal .eq. 1 ) then       ! for fuel cell coupled with adsorption storage unit
         Twater_recent = h3k_connect_property(icon3b,iTemperature)
      else
         TWater_recent =  h3k_connect_property(icon3,iTemperature)
      endif


C.....Set future connection temperature, for incommoming water connection
      pctf(icon3) = TWater_recent ! oC

C.....Get component name
      hold_name = pcname(iComponentIndex)
      hold_len = lnblnk(hold_name)

C.....H3Kreports.(HIGH_TEMP_FC).start......................
C.....The following code is used to trasnport data to
C.....the H3K reporting object
C.....
C.....Any changes to this code should be made within
C.....the H3Kreports demarcaration comments

C.....Calculations required for H3K reports:
C.....
C.....Fuel cell balance of plant electrical efficiency
C.....

C.....Total electrical demand
      call AddToReportWild1(rvPltQElecDemand%Identifier,
     &      q_total_elec,
     &      hold_name(1:hold_len))

C.....Net electrical output
      call AddToReportWild1(rvPltQElecNet%Identifier,
     &      q_net_elec,
     &      hold_name(1:hold_len))

C.....Parasitic electrical losses
      call AddToReportWild1(rvPltQElecParasitic%Identifier,
     &      q_par_elec,
     &      hold_name(1:hold_len))

C.....Net thermal output
      call AddToReportWild1(rvPltQThermalNet%Identifier,
     &      UA_exh*(T_exh_recent-Twater_recent),
     &      hold_name(1:hold_len))

C.....Fuel HHV
      call AddToReportWild1(rvPltFuelHHV%Identifier,
     &      phi_source,
     &      hold_name(1:hold_len))


C.....Electrical Efficiency - fuel cell
      call AddToReportWild1(rvPltElecEffFC%Identifier,
     &      eta_fc*100.,
     &      hold_name(1:hold_len))

C.....Electrical efficiency - fuel cell + Balance of plant
C.....-> only report if phi_source is non-zero
      call eclose (phi_source, 0.0, 0.1, bClose)
      if (.not. bClose) then

         call AddToReportWild1(rvPltElecEffBOP%Identifier,
     &      q_net_elec / phi_source *100.,
     &      hold_name(1:hold_len))
      endif


C.....Thermal efficiency ~ only report if phi_source is non-zero
      if (.not. bClose) then
         call AddToReportWild1(rvPltCogEff%Identifier,
     &        (q_net_elec + UA_exh*(T_exh_recent-Twater_recent) )
     &            / phi_source * 100.,
     &      hold_name(1:hold_len))
      endif

C.....Fuel Flow
      call AddToReportWild1(rvPltFuelFlowMass%Identifier,
     &        m_dot_fuel_kg,
     &      hold_name(1:hold_len))

C.....Fuel Flow - slpm
      call AddToReportWild1(rvPltFuelFlowVol%Identifier,
     &        Vdot_fuel_STP,
     &      hold_name(1:hold_len))

C.....Air Flow - mass
      call AddToReportWild1(rvPltAirFlowMass%Identifier,
     &        m_dot_air_kg,
     &      hold_name(1:hold_len))

C.....Air Flow - volume
      call AddToReportWild1(rvPltAirFlowVol%Identifier,
     &        Vdot_air_STP,
     &      hold_name(1:hold_len))

C.....Air Flow - stoicheometry
      call AddToReportWild1(rvPltAirFlowStoi%Identifier,
     &        stoichs,
     &      hold_name(1:hold_len))


C.....water Flow - volume
      call AddToReportWild1(rvPltWaterFlowVol%Identifier,
     &        Vdot_liqwater,
     &      hold_name(1:hold_len))

C.....Exhaust Gas - mass flow
      call AddToReportWild1(rvPltExhaustFlowMass%Identifier,
     &        m_dot_exh_kg,
     &      hold_name(1:hold_len))


C.....Exhaust Gas - volumetric flow
      call AddToReportWild1(rvPltExhaustFlowVol%Identifier,
     &        Vdot_exh_STP,
     &      hold_name(1:hold_len))

C.....Exhaust Gas - temperature
      call AddToReportWild1(rvPltExhaustFlowTemp%Identifier,
     &        T_exh_recent,
     &      hold_name(1:hold_len))

C.....Heat exchanger exit temperature
      call AddToReportWild1(rvPltHExchgExitTemp%Identifier,
     &        T_exit_recent,
     &      hold_name(1:hold_len))

C.....Heat exchanger heat transfer coefficient
      call AddToReportWild1(rvPltHExchgUAVal%Identifier,
     &        UA_exh,
     &      hold_name(1:hold_len))


C.....H3Kreports.(HIGH_TEMP_FC).end........................

      return
      end

C************************************************************************************************
