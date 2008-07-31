
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
C hot-water storage tank component models.

C FC_tank_fuel_static_temp    Static template for fuel-fired water tank.
C FC_tank_fuel_coeff_gen      Coefficient generator for fuel-fired water tank.
C FC_tank_elec_static_temp    Static template for electrically heated water tank.
C FC_tank_elec_coeff_gen      Coefficient generator for electrically heated water tank.
C FC_DHW_load                 Calculates DHW water draws and make-up water temperature.
C FC_check_for_boil           Checks for overheating condition in plant network.
C FC_dump_excess_energy       Fictitious heat dump to prevent overheating in plant network.


C *********************************************************************************************
C References:
C
C    Beausoleil-Morrison I. (2001a), Design of Fuel Cell Component Model for
C    FCT project, CETC internal report, 16 pages, revised November 29, 2001.
C
C    Beausoleil-Morrison I. (2001b), Design of Storage Tank Component Models for
C    FCT project, CETC internal report, ?? pages, October ??, 2001.
C
C    Kelly N. (2001), Development of a Heated Water Storage Plant Component
C    Model, ESRU Technical Report 150/01, University of Strathclyde,
C    12 pages, March 19, 2001.
C
C    Hensen J.L.M. (1991), On the Thermal Interaction of Building Structure and
C    Heating and Ventilation System, PhD Thesis, Technical University of Eindhoven,
C    The Netherlands.
C *********************************************************************************************


C *********************************************************************************************
C ****************************** FC_tank_fuel_static_temp *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 17, 2001
C Copyright CETC 2001

C This subroutine is the static template for a gas-fired water tank that is fed by
C a residential fuel cell system. It was developed for connection to the residential
C fuel cell component model but could be used in other plant arrangements as well.
C The model supports the following configurations :
C   1) a combo tank that supplies a fan-coil for space heat + domestic hot water (DHW)
C   2) a tank that supplies a fan-coil for space heat
C   3) a tank that supplies DHW

C This subroutine checks whether the component is properly used in the user-defined
C plant network, it assigns user-input data to module variables, and it performs
C time-invariant calculations.

C The tank, including its combustion system, is represented with three nodes:
C   Node 1 represents the water and the tank casing.
C   Node 2 represents the combustion chamber.
C   Node 3 represents the flue gases.

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
C    HHV_CH4    higher heating value of fuel constituent, MJ/kg (in MODULE FC_tanks)
C    HHV_C2H6   ditto
C    HHV_C3H8   ditto
C    FC_DHW_CSA_fraction(j)    Nominal DHW draw (L/hour) for hour `j' based on CSA schedule
C                              (in MODULE FC_tanks)

C OUTPUTS:
C    tank_mass                 mass of node 1, kg (in MODULE FC_tanks)
C    tank_Cp                   specific heat of node 1, J/kgK (in MODULE FC_tanks)
C    tank_UA                   heat loss coefficient between node 1 and surroundings,
C                              W/K (in MODULE FC_tanks)
C    tank_burner_ON            burner heat output when on, W (in MODULE FC_tanks)
C    tank_burner_OFF           burner heat output when off, W (in MODULE FC_tanks)
C    tank_efficiency           combustion efficiency, fraction (in MODULE FC_tanks)
C    tank_excess_air           excess air ratio, fraction (in MODULE FC_tanks)
C    tank_fuel_molefrac_N2     fuel molar fractions (in MODULE FC_tanks)
C    tank_fuel_molefrac_CO2    ditto
C    tank_fuel_molefrac_CH4    ditto
C    tank_fuel_molefrac_C2H6   ditto
C    tank_fuel_molefrac_C3H8   ditto
C    tank_fuel_molefrac_total  ditto
C    molW_fuel_tank            molecular weight of fuel, kg/kmol (in MODULE FC_tanks)
C    HHV_fuel_tank             HHV of fuel, MJ/kmol (in MODULE FC_tanks)
C    tank_DHW_draw             flag indicating whether tank supplies DHW (in MODULE FC_tanks)
C    tank_space_heat           flag indicating whether tank is connected to a 
C                              fan-coil (in MODULE FC_tanks)
C                              that supplies space heating (in MODULE FC_tanks)
C    FC_DHW_nominal_Lperday    Nominal DHW draw (L/day) based on occupancy (in MODULE FC_tanks)
C    FC_DHW_CSA_Lperday        Nominal DHW draw (L/day) based on CSA schedule
C                              (in MODULE FC_tanks)
C    FC_DHW_CSA_fraction(j)  fraction of total daily water draw that occurs during
C                            hour `j' (from MODULE FC_tanks)
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_tank_fuel_static_temp(IPCOMP)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "SOFC.h"
#include "cogen_tanks.h"
#include "chemical_properties.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA,Itemp,N_expect,IPCONC
      LOGICAL mistake,CLOSE
      INTEGER IPCOMP,J,jhour

C---------------------------------------------------------------------------------
C Fuel composition array used w/ interface to chemical_properties.F
C---------------------------------------------------------------------------------
      real fFuel_composition ( iCompound_count )

C---------------------------------------------------------------------------------
C Interface to chemical_properties.F
C---------------------------------------------------------------------------------      
      real fEval_Mixture_HV
      
C---------------------------------------------------------------------------------
C Initialize variables. These were formerly initialized in MODULEs prior to the
C conversion of the code to enable compilation with g77.
C---------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C DHW nominal water draws.
C---------------------------------------------------------------------------------
      DATA ( FC_DHW_CSA_Lperhour(jhour),jhour=1,24 )
     &                           /   9., 7., 0., 0., 0., 
     &                               0., 0., 8.,18.,15.,
     &                              16.,16.,14.,13.,12.,
     &                              10., 9.,10.,12.,16.,
     &                              17.,14.,12.,11. /

C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC.gt.0 .AND. ITRACE(35).ne.0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 3 node gas-fired space-heating water tank  '
        NumADATA = 14  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NumADATA)
        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(IPCOMP) holds the number of possible plant control variables
C as specified in the .pln file. There should be a single control variable.
C---------------------------------------------------------------------------------
      N_expect = 1
      IF(NCI(IPCOMP) .ne. N_expect) THEN
        WRITE(ITU,*) ' FC_tank_fuel_static_temp warning: ',
     &               ' incorrect num of ctl variables specified.'
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
C-----There should be two connections to node 1.
      IF( ICONDX(IPCOMP,1,1) .eq. 0 ) mistake=.true.
      IF( ICONDX(IPCOMP,1,2) .eq. 0 ) mistake=.true.
      DO IPCONC=3,MPCONC
        IF( ICONDX(IPCOMP,1,IPCONC) .ne. 0 ) mistake=.true.
      END DO
C-----There should be no connections to node 2.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP,2,IPCONC) .ne. 0 ) mistake=.true.
      END DO
C-----There should be no connections to node 3.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP,3,IPCONC) .ne. 0 ) mistake=.true.
      END DO
C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' FC_tank_fuel_static_temp: incorrect'
        WRITE(IUOUT,*) ' number of connections for component ',IPCOMP
        STOP ' FC_tank_fuel_static_temp: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connections to node 1 are of the correct type. The two connections
C to node 1 should be of type ISV=20 so that the tank component can be used in
C networks in which both first and second phase mass balances are formed.
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
      IF( ICONTP( ICONDX(IPCOMP,1,1) ) .ne. 20 ) mistake=.true.
      IF( ICONTP( ICONDX(IPCOMP,1,2) ) .ne. 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' FC_tank_fuel_static_temp: incorrect'
        WRITE(IUOUT,*) ' connection type to node 1 for comp ',IPCOMP
        STOP ' FC_tank_fuel_static_temp: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Assign user-specified mass (kg) and specific heat (J/kgK) of node 1
C (water in tank + casing).
C---------------------------------------------------------------------------------
      tank_mass = ADATA(IPCOMP,1)
      tank_Cp   = ADATA(IPCOMP,2)


C---------------------------------------------------------------------------------
C Assign user-specified heat loss coefficient (UA-value) between node 1 and
C surroundings (W/K).
C---------------------------------------------------------------------------------
      tank_UA = ADATA(IPCOMP,3)


C---------------------------------------------------------------------------------
C Assign user-specified burner capacity (W) for on and off (i.e. pilot) states.
C---------------------------------------------------------------------------------
      tank_burner_ON  = ADATA(IPCOMP,4)
      tank_burner_OFF = ADATA(IPCOMP,5)


C---------------------------------------------------------------------------------
C Assign user-specified combustion + flue efficiency (fraction). This is the
C percentage of the fuel's chemical energy content that is converted to the
C water's thermal energy. As such, this includes both the effects of combustion
C inefficiencies and the effects of the heat exchanger between the flue gases
C and the water. This is assumed to be a constant and thus not a function of tank
C temperature or cyclying frequency. This treatment could be refined in the future.
C---------------------------------------------------------------------------------
      tank_efficiency = ADATA(IPCOMP,6)/100.


C---------------------------------------------------------------------------------
C Assign user-specified excess air ratio (fraction).
C---------------------------------------------------------------------------------
      tank_excess_air = ADATA(IPCOMP,7)/100.


C---------------------------------------------------------------------------------
C Assign user-specified molar fractions of fuel constituents.
C---------------------------------------------------------------------------------
      tank_fuel_molefrac_N2    = ADATA(IPCOMP,8)
      tank_fuel_molefrac_CO2   = ADATA(IPCOMP,9)
      tank_fuel_molefrac_CH4   = ADATA(IPCOMP,10)
      tank_fuel_molefrac_C2H6  = ADATA(IPCOMP,11)
      tank_fuel_molefrac_C3H8  = ADATA(IPCOMP,12)

      

C---------------------------------------------------------------------------------
C Assign variable indicating whether there is a DHW as well as a space-heating
C draw from the storage tank. The DHW draw is not treated as a `connection'
C but rather is handled within the coefficient generator as a right-hand side
C term, this to simplify the plant networks.
C---------------------------------------------------------------------------------
      tank_DHW_draw = ADATA(IPCOMP,13)


C---------------------------------------------------------------------------------
C Assign variable indicating whether the tank is connected to a fan-coil that
C supplies space heat.  The tank may be configured to supply DHW, space-heat,
C or both.
C---------------------------------------------------------------------------------
      tank_space_heat = ADATA(IPCOMP,14)


C---------------------------------------------------------------------------------
C Determine the DHW nominal water draw. This equation is taken from the HOT3000
C DHW model, which took it directly from the HOT2000 bin model.
C---------------------------------------------------------------------------------
      FC_DHW_nominal_Lperday = 85. + ( 35. * float(FC_NumOfOccupants) )


C---------------------------------------------------------------------------------
C Determine the fraction of the daily DHW water draw that is drawn at each hour of
C the day. The actual water draws will be determined using these hourly profiles,
C the total DHW draw over the day, and the time-varying tank temperature.
C---------------------------------------------------------------------------------
      FC_DHW_CSA_Lperday = 0.
      DO j=1,24
        FC_DHW_CSA_Lperday = FC_DHW_CSA_Lperday
     &                     + FC_DHW_CSA_Lperhour(j)
      END DO
      DO j=1,24
        FC_DHW_CSA_fraction(j) = 
     &                       FC_DHW_CSA_Lperhour(j) / FC_DHW_CSA_Lperday
      END DO


C---------------------------------------------------------------------------------
C Check to ensure that the molar fractions of the fuel constituents sum to unity.
C---------------------------------------------------------------------------------
      tank_fuel_molefrac_total = tank_fuel_molefrac_N2
     &                         + tank_fuel_molefrac_CO2
     &                         + tank_fuel_molefrac_CH4
     &                         + tank_fuel_molefrac_C2H6
     &                         + tank_fuel_molefrac_C3H8
      CALL ECLOSE(tank_fuel_molefrac_total,1.0,0.001,CLOSE)
      IF(.not.CLOSE)THEN
        WRITE(IUOUT,*) ' FC_tank_fuel_static_temp:'
        WRITE(IUOUT,*) ' molar fractions of fuel'
        WRITE(IUOUT,*) ' do not sum to unity.'
        STOP ' FC_tank_fuel_static_temp: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Calculate the molecular weight of the fuel (kg/kmol).
C---------------------------------------------------------------------------------
      molW_fuel_tank = tank_fuel_molefrac_CH4   * molW_CH4
     &               + tank_fuel_molefrac_C2H6  * molW_C2H6
     &               + tank_fuel_molefrac_C3H8  * molW_C3H8
     &               + tank_fuel_molefrac_N2    * molW_N2
     &               + tank_fuel_molefrac_CO2   * molW_CO2


C---------------------------------------------------------------------------------
C Calculate the energy content of the fuel, HHV (Beausoleil-Morrison 2001b).
C Units of MJ/kmol.
C---------------------------------------------------------------------------------
      HHV_fuel_tank = tank_fuel_molefrac_CH4   * molW_CH4   * HHV_CH4
     &              + tank_fuel_molefrac_C2H6  * molW_C2H6  * HHV_C2H6
     &              + tank_fuel_molefrac_C3H8  * molW_C3H8  * HHV_C3H8


C---------------------------------------------------------------------------------
C Zero fFuel_composition array, and assign compositions 
C---------------------------------------------------------------------------------
      do  J = 1, iCompound_count
         fFuel_composition(j) = 0.0
      enddo
      
      fFuel_composition ( iMethane ) = tank_fuel_molefrac_CH4
      fFuel_composition ( iEthane  ) = tank_fuel_molefrac_C2H6
      fFuel_composition ( iPropane ) = tank_fuel_molefrac_C3H8

C--------------------------------------------------------------------------------
C Collect lower heating value of fuel (MJ/kmol)
C--------------------------------------------------------------------------------
      LHV_fuel_tank = fEval_Mixture_HV( fFuel_composition, iLHV )
     &                / 1.0E06

C---------------------------------------------------------------------------------
C Set local variables to point to the nodes and to the connections.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C                   within the global plant matrix.
C    node1          global matrix node number for water tank component node 1
C    node2          global matrix node number for water tank component node 2
C    node3          global matrix node number for water tank component node 3
C    con1a          number of first connection to node 1 (to fuel cell or fan coil)
C    con1b          number of second connection to node 1 (to fuel cell or fan coil)
C---------------------------------------------------------------------------------
      node1 = NPCDAT(IPCOMP,9)
      node2 = NPCDAT(IPCOMP,9)+1
      node3 = NPCDAT(IPCOMP,9)+2
      con1a = ICONDX(IPCOMP,1,1)
      con1b = ICONDX(IPCOMP,1,2)

C---------------------------------------------------------------------------------
C Completion of time-invariant operations for component.
C---------------------------------------------------------------------------------
      RETURN
      END



C *********************************************************************************************
C ******************************** FC_tank_fuel_coeff_gen *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 22, 2001
C Copyright CETC 2001

C This subroutine is the coefficient generator for the gas-fired storage tank component
C model. It was developed for connection to the residential
C fuel cell component model but could be used in other plant arrangements as well.
C The model supports the following configurations :
C   1) a combo tank that supplies a fan-coil for space heat + domestic hot water (DHW)
C   2) a tank that supplies a fan-coil for space heat
C   3) a tank that supplies DHW

C The DHW loads are treated as `connectionless'.  The DHW water draws and make-up water
C temperature are calculated within the coefficient generator.  These appear on the RHS
C of the energy balance for the water node.  This is done to minimize the number of
C components and connections and to allow for time-varying draws and make-up water
C temperatures.

C The tank is configured to be connected to a fuel cell. A second connection to the
C water node ia also required.  A flag can be set to indicate whether the tank supplies
C a fan-coil unit for space heating.  If the tank does supply space heat, then this
C second connection is to the fan-coil system.  If the tank does not supply space heat,
C then a second connection is required to satisfy the matrix requirements.  However,
C this connection is a `dummy'.  This coefficient generator nullifies the impact of
C this second connection.  A constant temperature supply is recommended as the inlet
C for this dummy connection: a pipe is recommended for the output.

C This subroutine performs the time-step calculations and establishes coefficients for the
C sub-matrices that define the energy, 1st phase mass flow, and 2nd phase mass
C flow balances on the storage tank's nodes. A fully explicit form of the energy
C balances is formed for nodes 2 and 3 since transients in the combustion chamber and flue
C are ignored. A weighted implicit-explicit energy balance is formed for node 1.

C The tank, including its combustion system, is represented with three nodes:
C   Node 1 represents the water and the tank casing.
C   Node 2 represents the combustion chamber.
C   Node 3 represents the flue gases.

C INPUTS:
C    tank_mass                 mass of node 1, kg (in MODULE FC_tanks)
C    tank_Cp                   specific heat of node 1, J/kgK (in MODULE FC_tanks)
C    tank_UA                   heat loss coefficient between node 1 and surroundings,
C                              W/K (in MODULE FC_tanks)
C    tank_burner_ON            burner heat output when on, W (in MODULE FC_tanks)
C    tank_burner_OFF           burner heat output when off, W (in MODULE FC_tanks)
C    tank_efficiency           combustion efficiency, fraction (in MODULE FC_tanks)
C    tank_excess_air           excess air ratio, fraction (in MODULE FC_tanks)
C    tank_fuel_molefrac_N2     fuel molar fractions (in MODULE FC_tanks)
C    tank_fuel_molefrac_CO2    ditto
C    tank_fuel_molefrac_CH4    ditto
C    tank_fuel_molefrac_C2H6   ditto
C    tank_fuel_molefrac_C3H8   ditto
C    tank_fuel_molefrac_total  ditto
C    molW_fuel_tank            molecular weight of fuel, kg/kmol (in MODULE FC_tanks)
C    HHV_fuel_tank             HHV of fuel, MJ/kmol (in MODULE FC_tanks)
C    tank_DHW_draw             flag indicating whether tank supplies DHW (in MODULE FC_tanks)
C    tank_space_heat           flag indicating whether tank is connected to a fan-coil
C                              that supplies space heating (in MODULE FC_tanks)
C    T_refCp                   reference temperature used to calculate heat capacities, oC
C                              (in MODULE FC_FLUIDS)
C    air_molefrac_N2           mole fraction of gas in air (in MODULE FC_FLUIDS)
C    air_molefrac_O2           ditto
C    air_molefrac_CO2          ditto
C    air_molefrac_H2O          ditto
C    air_molefrac_Ar           ditto
C    molW_air                  molecular weight of air, kg/kmol (in MODULE FC_FLUIDS)
C    PCNTMP(i)                 present time-row temperature of room containing
C                              component `i' (common/PCVAR)
C    PCNTMF(i)                 future time-row temperature of room containing
C                              component `i' (common/PCVAR)
C    IMPEXP                    identifies implicit/explicit handling of plant
C                              equations (common/PCEQU)
C    RATIMP                    user-specified implicit weighting fraction (common/PCEQU)
C    TIMSEC                    time-step in explicit plant domain (seconds) (common/PCTIME)
C    CDATA(i,j)                control signal for component `i', node `j' (common/C9) 

C OUTPUTS:
C    COUT(i)             coefficient for ISTATS balance for `i'th matrix position
C    NAPDAT(i)           number of "additional" output for component `i'
C    PCAOUT(i,j)         additional output `j' for component `i'
C    TC(i)               time constant (seconds) of plant component `i'
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_tank_fuel_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "SOFC.h"
#include "cogen_tanks.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "OffsiteUtilitiesPublic.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     & ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP
      REAL PERREL,PERTMP,PERFLX,PERMFL
      INTEGER itrclp
      INTEGER ICSV
      REAL CSVI


      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA


      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     & IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD


      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP


      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),
     & PCQF(MPNODE),PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),
     & PUAP(MPNODE),PCQP(MPNODE),PCNTMP(MPCOM)

      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP
      REAL PCQP,PCNTMP

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/PCTC/TC(MPCOM)
      REAL TC

      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP
      REAL RATIMP

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC



C     Common for determining if the plant domain has converged
      COMMON/pltcon/bpltcon,lastTStp

      LOGICAL bpltcon
      REAL lastTStp

C---------------------------------------------------------------------------------
C Declare local variables. These variables are defined at their first use.
C---------------------------------------------------------------------------------
      LOGICAL CLOSE
      REAL COUT(MPCOE) ! plant matrix coefficients (passed in calling statement)
      REAL tank_UA_future,tank_UA_present
      REAL T_node1_present,T_node1_future
      REAL mdotCp_1a_future,mdotCp_1b_future,mdotCp_1a_present,
     &     mdotCp_1b_present
      REAL T_1a_present,T_1b_present
      REAL Rinv,alpha
      REAL part1,part2,part3,part4,part5,part6,part7,part8
      REAL T_fuel_tank,T_air_tank
      REAL m_dot_CH4_kmol_t,m_dot_C2H6_kmol_t,m_dot_C3H8_kmol_t
      REAL m_dot_O2_for_CH4_kmol_t,m_dot_O2_for_C2H6_kmol_t,
     &     m_dot_O2_for_C3H8_kmol_t,m_dot_O2_stoich_t,m_dot_air_stoich,
     &     m_dot_air_kmol_t,Vdot_air_STP_t
      REAL H_fuel,H_air
      REAL m_dot_N2_exh_kmol_t,m_dot_Ar_exh_kmol_t,m_dot_O2_exh_kmol_t,
     &     m_dot_CO2_exh_kmol_t,m_dot_H20_exh_kmol_t
      REAL m_dot_exh_kmol_t,Vdot_exh_STP_t,tank_exh_molefrac_N2,
     &     tank_exh_molefrac_Ar,tank_exh_molefrac_O2,
     &     tank_exh_molefrac_CO2,tank_exh_molefrac_H2O
      REAL molW_exh_t
      REAL T_comb,H_comb,T_exh_tank,H_exh_tanka,Cp_exh_tank,Cp_comb


      INTEGER IPCOMP
      INTEGER ISTATS

      INTEGER ITEMP

      REAL SHTFLD
      REAL RHOFLD
      REAL H_gas
      INTEGER I



C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC.gt.0 .AND. NSINC.gt.ITC .AND.NSINC.le.ITCF.AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Entering sub FC_tank_fuel_coeff_gen'
      END IF

C---------------------------------------------------------------------------------
C Mark the temperature of the water node for iteration.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C---------------------------------------------------------------------------------
      ICSV(node1,1) = 1
      CSVI(node1,1) = CSVF(node1,1)


C---------------------------------------------------------------------------------
C Generate coefficients for the appropriate equation set. This subroutine was
C called to either generate coefficients for energy balances (ISTATS=1), 1st
C phase mass balances (ISTATS=2), or 2nd phase mass balances (ISTATS=3).
C Start with the energy balances.
C---------------------------------------------------------------------------------

C*********************************************************************************
C Beginning of energy balance section ********************************************
C*********************************************************************************
      IF(ISTATS.eq.1) THEN


C---------------------------------------------------------------------------------
C Determine the `present' and `future' temperature of the room containing the
C water tank. This is used to calculate the heat lost from the hot tank to the
C room (Beausoleil-Morrison 2001b, eq 4).
C Variables used:
C    PCNTMP(i)  present time-row temperature of room containing component `i'.
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' FC_tank_fuel_coeff_gen: '
          WRITE(IUOUT,*) ' the storage tank must be contained within '
          WRITE(IUOUT,*) ' a room.'
          STOP ' FC_tank_fuel_coeff_gen: unresolvable error'
        ELSE
          Troom_present = PCNTMP(IPCOMP)
          Troom_future  = PCNTMF(IPCOMP)
        END IF


C---------------------------------------------------------------------------------
C Establish the heat loss coefficient (UA-value, W/K) between node 1 and the
C surroundings for the present and future time-rows. A fixed UA-value is used here.
C However, this code is structured to easily enable a time-varying UA-value in the
C future. PUAP and PUAF are standard ESP-r variables holding the present and future
C time-row values of the UA-value. PUAF is mapped to PUAP by subroutine MZNASS
C following the solution of the energy matrix for the current time-step.
C Variables used:
C    PUAP(i)  present time-row UA-value
C    PUAF(i)  future time-row UA-value 
C---------------------------------------------------------------------------------
C-------Determine UA.
        tank_UA_future  = tank_UA  ! `tank_UA' from dbase (could be replaced by a correlation)
C-------Save future value.
        PUAF(node1)     = tank_UA_future
C-------Set present value.
        tank_UA_present = PUAP(node1)

C---------------------------------------------------------------------------------
C     Determine most recently calculated mass flow:
C     CONVAR(i,2) water mass flow rate (kg/s) at sending node for connection `i'
C     PCONDR(i)   ratio of flow rate leaving sending node `i' that reaches
C                receiving node.
C---------------------------------------------------------------------------------
         mdot_conn1a_future = PCONDR(con1a) * CONVAR(con1a,2)  ! (kg/s)
         mdot_conn1b_future = PCONDR(con1b) * CONVAR(con1b,2)  ! (kg/s)

C---------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the two
c connections to node 1, for the present and future time-rows. These are the heat
C capacity rates (W/K) of the the water entering the storage tank from the fuel
C cell and from the fan-coil return. PCRP and PCRF are standard ESP-r variables
C holding the present and future time-row values of the mass flow * heat capacity.
C PCRF is mapped to PCRP by subroutine MZNASS following the solution of the energy
C matrix for the current time-step.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C    CONVAR(i,2) water mass flow rate (kg/s) at sending node for connection `i'
C    PCONDR(i)   ratio of flow rate leaving sending node `i' that reaches
C                receiving node.
C    SHTFLD(3,T) function that returns heat capacity (J/kgK) of water (index 3)
C                at temperature `T'.
C    PCRP(i)     present time-row mass flow * heat capacity for connection `i'
C    PCRF(i)     future time-row mass flow * heat capacity for connection `i'
C---------------------------------------------------------------------------------
         mdotCp_1a_future = mdot_conn1a_future
     &                    * SHTFLD( 3,CONVAR(con1a,1) )
C--------If space-heat flag set, then second connection is real: otherwise make flow zero.
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           mdotCp_1b_future = mdot_conn1b_future
     &                      * SHTFLD( 3,CONVAR(con1b,1) )
         ELSE
           mdotCp_1b_future = 0.
         END IF
C--------Save future values.
         PCRF(con1a) = mdotCp_1a_future
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           PCRF(con1b) = mdotCp_1b_future
         END IF
C--------Set present values.
         mdotCp_1a_present = PCRP(con1a)
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           mdotCp_1b_present = PCRP(con1b)
         ELSE
           mdotCp_1b_present = 0.
         END IF


C---------------------------------------------------------------------------------
C Establish the present time-row temperatures of the water flowing into the tank
C at the two connections.  PCTP and PCTF are standard ESP-r variables holding the
C present and future time-row values of the temperature of the fluid flowing at
C the connection. PCTF is mapped to PCTP by subroutine MZNASS following the
C solution of the energy matrix for the current time-step.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C    PCTP(i)     present temperature of sending node for connection `i'
C    PCTF(i)     future temperature of sending node for connection `i'
C---------------------------------------------------------------------------------
         T_1a_present = PCTP(con1a)

C--------If space-heat flag set, then second connection is real: otherwise make temp zero.
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           T_1b_present = PCTP(con1b)
         ELSE
           T_1b_present = 0.
         END IF


C---------------------------------------------------------------------------------
C Establish the present and future time-row temperatures of node 1.
C Variables used:
C     CSVP(i,j)  present time-row solution variable for node `i' and variable `j';
C                j=1 for temperature
C     CSVF(i,j)  future time-row solution variable (same indices as with CSVP)
C---------------------------------------------------------------------------------
         T_node1_present = CSVP(node1,1)
         T_node1_future  = CSVF(node1,1)


C---------------------------------------------------------------------------------
C Check the temperature of the tank and activiate the fictitious heat dump if
C necessary to maintain the temperature within a reasonable range. This should
C prevent the occurence of boiling and will allow the examination of scenarios for
C dealing with excessive energy build-up in the system.
C---------------------------------------------------------------------------------
         CALL FC_dump_excess_energy(node1)


C---------------------------------------------------------------------------------
C Check the temperature of the tank to ensure that the water is not going to
C boil. This may occur is the fuel cell consistently adds more energy to the
C tank than is drawn off to meet the space heating and/or DHW loads. This should
C never occur with addition of call above to `FC_dump_excess_energy', but left
C here anyway for safety.
C---------------------------------------------------------------------------------
         CALL FC_check_for_boil(node1)


C---------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the DHW draw
C on the tank, for the present and future time-rows. Also determine the temperature
C of the DHW make-up water, for the present and future time-rows.
C Variables used:
C    RHOFLD(3,T)   function that returns density (kg/m3) of water (index 3) at
C                  temperaturere `T'.
C    SHTFLD(3,T)   function that returns heat capacity (J/kgK) of water (index 3)
C                  at temperature `T'.
C---------------------------------------------------------------------------------
        CALL FC_DHW_load(T_node1_future)

        mdotCp_DHW_present = DHW_draw_present
     &                     * RHOFLD(3,T_makeup_present)
     &                     * SHTFLD( 3,T_makeup_present ) 
        mdotCp_DHW_future  = DHW_draw_future
     &                     * RHOFLD(3,T_makeup_future)
     &                     * SHTFLD( 3,T_makeup_future ) 


C---------------------------------------------------------------------------------
C Calculate the fuel flow rate (Beausoleil-Morrison 2001b, eq 9). The burner will
C either be on or off, depending on the control signal. CDATA=0 signals that the
C burner should be off; CDATA=1 signals that it should be on. Only the combustion
C chamber (node 2) is controlled and should be made to respond to the water
C temperature (node 1).
C Variables used:
C    CDATA(i,j)   control signal for component `i's node `j'.
C---------------------------------------------------------------------------------
C-------Determine whether burner is on or off this time-step.
        CALL ECLOSE( CDATA(IPCOMP,2),0.,0.0001,CLOSE )
        IF(CLOSE)THEN  ! Burner is in "off" state (there may be a pilot light)
          q_burner = tank_burner_OFF
        ELSE
          q_burner = tank_burner_ON
        ENDIF
C-------Determine the fuel flow rate in kmol/s (Beausoleil-Morrison 2001b eq 9).
        m_dot_fuel_kmol_t = q_burner / HHV_fuel_tank / 1000000.
C-------Determine fuel flow rate in kg/s and standard L/min (slpm). Note that
C-------a kmol occupies 22 414 L at STP.
        m_dot_fuel_kg_t = m_dot_fuel_kmol_t * molW_fuel_tank
        Vdot_fuel_STP_t = m_dot_fuel_kmol_t * 22414. * 60.


C---------------------------------------------------------------------------------
C Calculate the energy liberated from the fuel's combustion which is transferred
C to the water (node 1) using Beausoleil-Morrison 2001b eq 2 (W). PCQP and PCQF
C are standard ESP-r variables holding the present and future time-row values of
C nodal injections (the energy transferred from the combustion to the water in
C this case). PCQF is mapped to PCQP by subroutine MZNASS following the solution
C of the energy matrix for the current time-step.
C Variables used:
C    PCQP(i)     present energy transferred to water from combustion for node `i'
C    PCQF(i)     future energy transferred to water from combustion for node `i'
C---------------------------------------------------------------------------------
        q_capture_future = tank_efficiency * HHV_fuel_tank
     &                   * m_dot_fuel_kmol_t * 1000000.
C-------Save future value.
        PCQF(node1) = q_capture_future
C-------Set present value.
        q_capture_present = PCQP(node1)


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the fuel entering the combustion chamber
C (node 2) using Beausoleil-Morrison 2001a eq 11. Assume that the fuel enters
C at a temperature equal to that of the room containing the tank. The function
C H_gas returns the relative enthalpy of each fuel constituent in kJ/kmol.
C Variables used:
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        T_fuel_tank = PCNTMF(IPCOMP) ! Already confirmed above that containment defined.
        H_fuel = tank_fuel_molefrac_N2   * H_gas(T_fuel_tank,N2,IUOUT)
     &         + tank_fuel_molefrac_CO2  * H_gas(T_fuel_tank,CO2,IUOUT)
     &         + tank_fuel_molefrac_CH4  * H_gas(T_fuel_tank,CH4,IUOUT)
     &         + tank_fuel_molefrac_C2H6 * H_gas(T_fuel_tank,C2H6,IUOUT)
     &         + tank_fuel_molefrac_C3H8 * H_gas(T_fuel_tank,C3H8,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the air flow rate to node 2 (combustion chamber).
C First, determine the molar flow rates (kmol/s) of the three fuel constituents
C that react with O2. Then determine the O2 flow rate required to
C stoichiometrically react with these fuel constituents (Beausoleil-Morrison
C 2001b eq 11). Determine the stoichiometric air flow requirement, then
C finally the actual air flow rate (Beausoleil-Morrison 2001b eq 10). Note that
C this approach assumes that the hydrocarbons fully react in the combustion
C chamber.
C---------------------------------------------------------------------------------
C-------Flow rates of hydrocarbons (kmol/s).
        m_dot_CH4_kmol_t  = tank_fuel_molefrac_CH4  * m_dot_fuel_kmol_t
        m_dot_C2H6_kmol_t = tank_fuel_molefrac_C2H6 * m_dot_fuel_kmol_t
        m_dot_C3H8_kmol_t = tank_fuel_molefrac_C3H8 * m_dot_fuel_kmol_t
C-------Stoichiometric O2 flow rate required for each fuel constituent (kmol/s).
        m_dot_O2_for_CH4_kmol_t  = 2.    * m_dot_CH4_kmol_t
        m_dot_O2_for_C2H6_kmol_t = 7./2. * m_dot_C2H6_kmol_t
        m_dot_O2_for_C3H8_kmol_t = 5.    * m_dot_C3H8_kmol_t
        m_dot_O2_stoich_t = m_dot_O2_for_CH4_kmol_t
     &                    + m_dot_O2_for_C2H6_kmol_t
     &                    + m_dot_O2_for_C3H8_kmol_t
C-------Stoichiometric air flow rate (kmol/s).
        m_dot_air_stoich = m_dot_O2_stoich_t / air_molefrac_O2
C-------Air flow rate to node 2 (kmol/s, kg/s, and slpm).
        m_dot_air_kmol_t = m_dot_air_stoich * tank_excess_air
        m_dot_air_kg_t   = m_dot_air_kmol_t * molW_air     ! molW_air calculated in FC_static_template
        Vdot_air_STP_t   = m_dot_air_kmol_t * 22414. * 60.


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the air entering node 2 (combustion chamber)
C (Beausoleil-Morrison 2001a eq 11). Assume that the air enters at a temperature
C equal to that of the room containing the water tank. The function H_gas returns
C the relative enthalpy of each fuel constituent in kJ/kmol.
C Variables used:
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        T_air_tank = PCNTMF(IPCOMP) ! Already confirmed above that containment defined.
        H_air = air_molefrac_N2   * H_gas(T_air_tank,N2,IUOUT)
     &         + air_molefrac_O2  * H_gas(T_air_tank,O2,IUOUT)
     &         + air_molefrac_CO2 * H_gas(T_air_tank,CO2,IUOUT)
     &         + air_molefrac_H2O * H_gas(T_air_tank,H2O,IUOUT)
     &         + air_molefrac_Ar  * H_gas(T_air_tank,Ar,IUOUT)


C---------------------------------------------------------------------------------
C Determine the composition and flow rate of the exhaust gases. Assume that
C the combustion process is complete. Based on the chemical reactions occuring
C within the combustion chamber (Beausoleil-Morrison 2001b eq 11), calculate
C the molar flow rate of each constituent. As reactions are assumed to
C be complete, there are no hydrocarbons in the exhaust.
C---------------------------------------------------------------------------------
C-------N2 (Beausoleil-Morrison 2001a eq 18).
        m_dot_N2_exh_kmol_t = m_dot_air_kmol_t  * air_molefrac_N2
     &                    + m_dot_fuel_kmol_t * tank_fuel_molefrac_N2
C-------Ar (Beausoleil-Morrison 2001a eq 19)
        m_dot_Ar_exh_kmol_t = m_dot_air_kmol_t  * air_molefrac_Ar
C-------O2 (Beausoleil-Morrison 2001a eq 20)
        m_dot_O2_exh_kmol_t =
     &                 m_dot_air_kmol_t * air_molefrac_O2
     &               - 2.    * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_CH4
     &               - 7./2. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_C2H6
     &               - 5.    * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_C3H8
C-------CO2 (Beausoleil-Morrison 2001a eq 21)
        m_dot_CO2_exh_kmol_t =
     &                    m_dot_air_kmol_t  * air_molefrac_CO2
     &                  + m_dot_fuel_kmol_t * tank_fuel_molefrac_CO2
     &                  + 1. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_CH4
     &                  + 2. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_C2H6
     &                  + 3. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_C3H8
C-------H2O (Beausoleil-Morrison 2001a eq 22)
        m_dot_H20_exh_kmol_t =
     &                    m_dot_air_kmol_t * air_molefrac_H2O
     &                  + 2. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_CH4
     &                  + 3. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_C2H6
     &                  + 4. * m_dot_fuel_kmol_t
     &                       * tank_fuel_molefrac_C3H8


C---------------------------------------------------------------------------------
C Calculate the flow rate of all exhaust gases in terms of kmol/s `m_dot_exh_kmol_t'
C and standard Litres/min `Vdot_exh_STP_t'. Then determine the mole fractions of
C the exhaust gas constituents (Beausoleil-Morrison 2001a eq 23).
C---------------------------------------------------------------------------------
        m_dot_exh_kmol_t = m_dot_N2_exh_kmol_t
     &                 + m_dot_Ar_exh_kmol_t
     &                 + m_dot_O2_exh_kmol_t
     &                 + m_dot_CO2_exh_kmol_t
     &                 + m_dot_H20_exh_kmol_t
        Vdot_exh_STP_t = m_dot_exh_kmol_t * 22414. * 60.
C-------Are there any exhaust gases?
        IF( m_dot_exh_kmol_t > 0. )THEN
C---------There is exhaust, so calculate molar fractions.
          tank_exh_molefrac_N2  = m_dot_N2_exh_kmol_t / m_dot_exh_kmol_t
          tank_exh_molefrac_Ar  = m_dot_Ar_exh_kmol_t / m_dot_exh_kmol_t
          tank_exh_molefrac_O2  = m_dot_O2_exh_kmol_t / m_dot_exh_kmol_t
          tank_exh_molefrac_CO2 = m_dot_CO2_exh_kmol_t/ m_dot_exh_kmol_t
          tank_exh_molefrac_H2O = m_dot_H20_exh_kmol_t/ m_dot_exh_kmol_t
        ELSE
C---------No exhaust: set molar fractions to zero (avoids div by 0).
          tank_exh_molefrac_N2  = 0.
          tank_exh_molefrac_Ar  = 0.
          tank_exh_molefrac_O2  = 0.
          tank_exh_molefrac_CO2 = 0.
          tank_exh_molefrac_H2O = 0.
        END IF


C---------------------------------------------------------------------------------
C Determine the molecular weight of the exhaust gases (Beausoleil-Morrison
C 2001a eq 24).
C---------------------------------------------------------------------------------
        molW_exh_t = tank_exh_molefrac_N2  * molW_N2
     &           + tank_exh_molefrac_Ar  * molW_Ar
     &           + tank_exh_molefrac_O2  * molW_O2
     &           + tank_exh_molefrac_CO2 * molW_CO2
     &           + tank_exh_molefrac_H2O * molW_H2O


C---------------------------------------------------------------------------------
C Determine the flow rate of all exhaust gases in terms of kg/s.
C---------------------------------------------------------------------------------
        m_dot_exh_kg_t = m_dot_exh_kmol_t * molW_exh_t


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the combustion gases leaving node 2 and
C entering node 3 (Beausoleil-Morrison 2001a eq 25). The function H_gas returns the
C relative enthalpy of each fuel constituent in kJ/kmol. Evaluate the enthalpies
C at the temperature solved the previous time-step. 
C The coefficients established for node 2 the previous iteration may have led
C to an unrealistic CSVF solution. This is particularly prone to happen on the
C very first plant simulation increment. In order to prevent the H_comb calculation
C from blowing up, place a range check on T_comb to force it within a physically
C realistic range. 
C Variables used:
C     CSVF(i,1)  future time-row temperature for node `i'.
C---------------------------------------------------------------------------------
        T_comb = CSVF(node2,1)
        IF( T_comb.lt.100. .or. T_comb.gt.2000.) T_comb=400.
        H_comb = tank_exh_molefrac_N2  * H_gas(T_comb,N2,IUOUT)
     &         + tank_exh_molefrac_Ar  * H_gas(T_comb,Ar,IUOUT)
     &         + tank_exh_molefrac_O2  * H_gas(T_comb,O2,IUOUT)
     &         + tank_exh_molefrac_CO2 * H_gas(T_comb,CO2,IUOUT)
     &         + tank_exh_molefrac_H2O * H_gas(T_comb,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the exhaust gas mixture at the reference
C temperature `T_refCp' (see Beausoleil-Morrison eq 26a and 26b). The function
C H_gas returns the relative enthalpy of each fuel constituent in kJ/kmol.
C---------------------------------------------------------------------------------
        Href_tank = tank_exh_molefrac_N2  * H_gas(T_refCp_tank,N2,IUOUT)
     &          + tank_exh_molefrac_Ar   * H_gas(T_refCp_tank,Ar,IUOUT)
     &          + tank_exh_molefrac_O2   * H_gas(T_refCp_tank,O2,IUOUT)
     &          + tank_exh_molefrac_CO2  * H_gas(T_refCp_tank,CO2,IUOUT)
     &          + tank_exh_molefrac_H2O  * H_gas(T_refCp_tank,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Linearise the enthalpy of the node 2 gases so that temperature can appear in
C the energy balance for nodes 2 and 3 (Beausoleil-Morrison 2001a eq 26a). The
C Cp calculated here is used to linearise the enthalpy term. The difference
C between the exhaust gas temperature at the previous time-step and `T_refCp_tank'
C is used to linearise the enthalpy. Degrees Celcius are used since ESP-r's plant
C matrix solver works in these units.
C The units of this heat capacity are J/(kmol*oC)
C---------------------------------------------------------------------------------
        Cp_comb = (H_comb-Href_tank)*1000. / (T_comb - T_refCp_tank)


C---------------------------------------------------------------------------------
C Calculate the enthalpy (kJ/kmol) of the exhaust gases leaving node 3
C (Beausoleil-Morrison 2001a eq 25). The function H_gas returns the
C relative enthalpy of each fuel constituent in kJ/kmol. Evaluate the enthalpies
C at the temperature solved the previous time-step.
C The coefficients established for node 3 the previous iteration may have led
C to an unrealistic CSVF solution. This is particularly prone to happen on the
C very first plant simulation increment. In order to prevent the H_exh_tanka calculation
C from blowing up, place a range check on T_exh_tank to force it within a physically
C realistic range. 
C Variables used:
C     CSVF(i,1)  future time-row temperature for node `i'.
C---------------------------------------------------------------------------------
        T_exh_tank = CSVF(node3,1)
        IF( T_exh_tank.lt.100. .or. T_exh_tank.gt.2000.) T_exh_tank=400.
        H_exh_tanka = tank_exh_molefrac_N2  * H_gas(T_exh_tank,N2,IUOUT)
     &        + tank_exh_molefrac_Ar  * H_gas(T_exh_tank,Ar,IUOUT)
     &        + tank_exh_molefrac_O2  * H_gas(T_exh_tank,O2,IUOUT)
     &        + tank_exh_molefrac_CO2 * H_gas(T_exh_tank,CO2,IUOUT)
     &        + tank_exh_molefrac_H2O * H_gas(T_exh_tank,H2O,IUOUT)


C---------------------------------------------------------------------------------
C Linearise the enthalpy of the node 3 gases so that temperature can appear in
C the energy balance for node 3 (Beausoleil-Morrison 2001a eq 26a). The Cp
C calculated here is used to linearise the enthalpy term. The difference between
C the exhaust gas temperature at the previous time-step and `T_refCp_tank'.
C Degrees Celcius are used since ESP-r's plant matrix solver works in these units.
C The units of this heat capacity are J/(kmol*oC)
C---------------------------------------------------------------------------------
        Cp_exh_tank = (H_exh_tanka-Href_tank)*1000. /
     &           (T_exh_tank - T_refCp_tank)


C---------------------------------------------------------------------------------
C Calculate component's time-constant for the current time-step. Refer to
C Hensen (1991) pages 5.9 and 5.10.  The environmental losses from the tank and
C the water streams flowing into the tank are the three heat transfers that
C affect the transient storage. Therefore, treat these as parallel paths for
C the purposes of calculating the time constant, as described in eq 5.5 of
C Hensen (1991).
C Variables used:
C     tank_mass    mass of node 1, kg
C     tank_Cp      specific heat of node 1, J/kgK
C     TC(i)        time constant (seconds) of plant component `i'
C---------------------------------------------------------------------------------
        Rinv = tank_UA_future + mdotCp_1a_future + mdotCp_1b_future
        CALL ECLOSE(Rinv,0.,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' FC_tank_fuel_coeff_gen: '
          WRITE(IUOUT,*) ' impossible time constant. '
          STOP ' FC_tank_fuel_coeff_gen: unresolvable error'
        END IF
        TC(IPCOMP) = tank_mass*tank_Cp / Rinv


C---------------------------------------------------------------------------------
C Determine implicit/explicit weighting for the energy balance on node 1. `alpha'
C is the implicit weighting factor used to form the energy balance this time-step.
C It is the Greek "alpha" in Beausoleil-Morrison (2001b) equation 7. The approach
C used here to determine `alpha' is used with most plant components, as discussed
C by Hensen (1991).
C Variables used:
C    IMPEXP   identifies handling of plant equations: 1= fully implicit case;
C             2= mixed explicit/implicit; 3= 1 or 2 depending on component time
C             constant and time-step (this is the default treatment);
C             4= steady state.
C    RATIMP   user-specified implicit weighting fraction
C    TIMSEC   the time-step in the explicit plant domain (seconds)
C---------------------------------------------------------------------------------
C-------Fully implicit.
        IF( IMPEXP.eq.1 ) THEN
          alpha = 1.
C-------Implicit/explicit with user-specified (fixed) weighting.
        ELSE IF( IMPEXP .eq. 2 ) THEN
          alpha = RATIMP
C-------General case: implicit/explicit with calculated weighting.
        ELSE IF( IMPEXP .eq. 3 ) THEN
          IF( TIMSEC>0.63*TC(IPCOMP) ) THEN
            alpha = 1.
          ELSE
            alpha = RATIMP
          END IF
C-------Steady-state.
        ELSE IF( IMPEXP .eq. 4 ) THEN
          tank_mass = 0.  ! make node 1 massless
          alpha = 1.
        END IF


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.
C Node coefficient transport for COUT (refer to Beausoleil-Morrison 2001b,
C Figure 2):
C       <--self-->|<cross>
C  node   1  2  3 |  fc fan-coil    RHS
C       ---------------------------------
C         1  0  0 |   5    6          7
C         0  2  0 |   0    0       =  8
C         0  3  4 |   0    0          9
C---------------------------------------------------------------------------------
C-------Node 1 energy balance given by Beausoleil-Morrison (2001b) eq 7.
        COUT(1) = tank_mass * tank_Cp / TIMSEC       ! self-coupling to itself (W/K)
     &          + alpha * tank_UA_future
     &          + alpha * mdotCp_1a_future
     &          + alpha * mdotCp_1b_future
     &          + alpha * mdotCp_DHW_future
        COUT(5) = -1. * alpha * mdotCp_1a_future     ! cross-coupling (W/K)
        COUT(6) = -1. * alpha * mdotCp_1b_future     ! cross-coupling (W/K)
        part1 = tank_mass * tank_Cp / TIMSEC         ! (W/K)
     &        - (1.-alpha) * tank_UA_present
     &        - (1.-alpha) * mdotCp_1a_present
     &        - (1.-alpha) * mdotCp_1b_present
     &        - (1.-alpha) * mdotCp_DHW_present
        part2 = (1.-alpha) * mdotCp_1a_present       ! (W)
        part3 = (1.-alpha) * mdotCp_1b_present       ! (W)
        part6 = (1.-alpha) * mdotCp_DHW_present      ! (W)
        part7 = alpha * mdotCp_DHW_future            ! (W)
        part4 = (1.-alpha) * q_capture_present       ! (W)
     &        + alpha * q_capture_future
        part5 = (1.-alpha) * tank_UA_present * Troom_present
     &        + alpha * tank_UA_future * Troom_future
        part8 = -1.*(1.-alpha) * q_heatdump_present  ! (W)
     &          -1*alpha * q_heatdump_future
        COUT(7) = part1 * T_node1_present            ! RHS (W)
     &          + part2 * T_1a_present
     &          + part3 * T_1b_present
     &          + part6 * T_makeup_present
     &          + part7 * T_makeup_future
     &          + part4
     &          + part5
     &          + part8

C-------Node 2 energy balance. Treatment depends on whether there is combustion.
C-------Are there any exhaust gases (indicates that there is combustion)?
        IF( m_dot_exh_kmol_t > 0. )THEN
C---------There is exhaust: calculate coefficients using energy balance given by
C---------Beausoleil-Morrison (2001b) eq 12.
          COUT(2) = m_dot_exh_kmol_t * Cp_comb               ! self-coupling to itself (W/K)
          COUT(8) = m_dot_fuel_kmol_t * H_fuel * 1000.     ! RHS (W)
     &            + m_dot_air_kmol_t  * H_air  * 1000.
     &            + q_burner
     &            + m_dot_exh_kmol_t * Cp_comb * T_refCp_tank
     &            - m_dot_exh_kmol_t * Href_tank * 1000.
        ELSE
C---------No exhaust: make combustion temperature equal room temperature.
          COUT(2) = 1.
          COUT(8) = Troom_future
        END IF

C-------Node 3 energy balance. Treatment depends on whether there is combustion.
C-------Are there any exhaust gases (indicates that there is combustion)?
        IF( m_dot_exh_kmol_t > 0. )THEN
C---------There is exhaust: calculate coefficients using energy balance given by
C---------Beausoleil-Morrison (2001b) eq 14.
          COUT(3) = -1. * m_dot_exh_kmol_t * Cp_comb           ! self-coupling to node 2 (W/K)
          COUT(4) = m_dot_exh_kmol_t * Cp_exh_tank                  ! self-coupling to itself (W/K)
          COUT(9) = m_dot_exh_kmol_t * Cp_exh_tank  * T_refCp_tank  ! RHS (W)
     &            - m_dot_exh_kmol_t * Cp_comb * T_refCp_tank
     &            - q_capture_future
        ELSE
C---------No exhaust: make exhaust temperature equal room temperature.
          COUT(3) = 0.
          COUT(4) = 1.
          COUT(9) = Troom_future
        END IF

C-------Store time step burner output for h3kstore output
        fSDHW_TSTank_fuel_watts = q_burner
C......add to XML reporting
      call add_to_xml_reporting(
     &         fSDHW_TSTank_fuel_watts,
     &         'plant/SDHW_summary/DHW_tank_fuel',
     &         'units','(W)',
     &         'Fuel burner input to the DHW tank')

C.....Report fuel consumption.
      fSUFuelEnergyUse( iNaturalGas, iUseUncatagorized) = q_burner
      call StorePltCompEnergyUse( iPComp, fSUFuelEnergyUse )
C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      ELSE IF(ISTATS.eq.2) THEN
C-------Node 1: total flow in equals total flow out.
        COUT(1) = 1.
        COUT(5) = -1. * PCONDR(con1a) ! (dimensionless)
C-------If space-heat flag set, then second connection is real: otherwise make flow zero.
        IF( tank_space_heat.eq.tank_yes_heat )THEN
          COUT(6) = -1. * PCONDR(con1b)
        ELSE
          COUT(6) = 0.
        END IF
        COUT(7) = 0.
C-------Node 2: no balance required so make flow zero.
        COUT(2) = 1.
        COUT(8) = 0.
C-------Node 3: flow in equals flow out.
        COUT(3) = -1.
        COUT(4) = 1.
        COUT(9) = 0.


C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      ELSE IF(ISTATS.eq.3) THEN
C-------Node 1: no balance required so make flow zero.
        COUT(1) = 1.
        COUT(5) = 0.
        COUT(6) = 0.
        COUT(7) = 0.
C-------Node 2: no balance required so make flow zero.
        COUT(2) = 1.
        COUT(8) = 0.
C-------Node 3: no balance required so make flow zero.
        COUT(3) = 0.
        COUT(4) = 1.
        COUT(9) = 0.
      END IF

C*********************************************************************************
C End of energy and mass balances ************************************************
C*********************************************************************************


C---------------------------------------------------------------------------------
C Save the `additional' output variables for outputting on time-step basis.
C---------------------------------------------------------------------------------
      NAPDAT(IPCOMP)    = 8                    ! number of additional outputs
      PCAOUT(IPCOMP,1)  = q_burner             ! burner output (W)
      PCAOUT(IPCOMP,2)  = q_capture_future     ! burner energy captured in water (W)
      PCAOUT(IPCOMP,3)  = m_dot_fuel_kg_t        ! fuel consumption (kg/s)
      PCAOUT(IPCOMP,4)  = Vdot_fuel_STP_t        ! fuel consumption (slpm)
      PCAOUT(IPCOMP,5)  = m_dot_air_kg_t         ! air flow (kg/s)
      PCAOUT(IPCOMP,6)  = Vdot_air_STP_t         ! air flow (slpm)
      PCAOUT(IPCOMP,7)  = T_comb               ! temperature of combustion gases (oC)
      PCAOUT(IPCOMP,8)  = T_exh_tank                ! temperature of exhaust gases (oC)


C---------------------------------------------------------------------------------
C Store results to calculate monthly averages and (optionally) to produce an
C ASCII file with time-step output.
C
C  - Note: report "present" (ie. last timestep values of all state variables) 
C          since they are not available for the current tiemstep yet
C---------------------------------------------------------------------------------
      IF(ISTATS.eq.1) CALL H3KSTORE_FuelCell_Fossil_Tank(
     &     Vdot_fuel_STP_t,     ! fuel supply
     &     q_capture_future,    ! burner -> water
     &     T_node1_present,     ! tank temperature  (state variable)
     &     T_1a_present,        ! temp of water at 1st connection (state variable)
     &     mdot_conn1a_present, ! flow rate of water at 1st connection (state variable)
     &     T_1b_present,         ! temp of water at 2nd connection (state variable)
     &     mdot_conn1b_present, ! flow rate of water at 2nd connection (state variable)
     &     T_makeup_future,     ! temp of DHW make-up water
     &     DHW_draw_future,     ! flow rate of DHW draw
     &     tank_UA_future,      ! UA of tank
     &     Troom_future,        ! Predicted future temp of Room (from building domain)
     $     q_heatdump_future    ! rate of energy dumped
     &                        )

C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 3 node gas-fired storage tank for fuel cell'
        WRITE(ITU,*) ' Matrix node(s) ',node1,node2,node3

        WRITE(ITU,*) ' Connection(s)  ',con1a,con1b
        IF(ISTATS.eq.1) THEN
           WRITE(ITU,*) ' CDATA             = ',CDATA(IPCOMP,2),' (-)'
           WRITE(ITU,*) ' Water Temp (p)    = ',T_node1_present,
     &          ' (oC)'
           WRITE(ITU,*) ' Water Temp (f)    = ',T_node1_future,' (oC)'
           WRITE(ITU,*) ' Conn 1. Temp (p)  = ',T_1a_present,' (oC)'
           WRITE(ITU,*) ' Conn 2. Temp (p)  = ',T_1b_present,' (oC)'
           WRITE(ITU,*) ' q_burner          = ',q_burner,' (W)'
           WRITE(ITU,*) ' q_capture_future  = ',q_capture_future,' (W)'
           WRITE(ITU,*) ' m_dot_fuel_kg_t     = ',m_dot_fuel_kg_t,
     &                                          ' (kg/s)'
           WRITE(ITU,*) ' m_dot_air_kg_t      = ',m_dot_air_kg_t,
     &                                          ' (kg/s)'
           WRITE(ITU,*) ' m_dot_exh_kg_t      = ',m_dot_exh_kg_t,
     &                                          ' (kg/s)'
           WRITE(ITU,*) ' T_comb            = ',T_comb,' (oC)'
           WRITE(ITU,*) ' T_exh_tank             = ',T_exh_tank,' (oC)'
           WRITE(ITU,*) ' alpha             = ',alpha,' (-)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(I),I=1,9)
        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving sub FC_tank_fuel_coeff_gen'
      END IF



      RETURN
      END



C *********************************************************************************************
C ************************************* FC_DHW_load *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 30, 2001
C Copyright CETC 2001

C This subroutine determines volumetric flow rate of water supplied to DHW
C and temperature of DHW make-up water. This is done for both the present and
C future time-rows. The techniques employed by HOT3000's DHW model are used here
C as much as possible, this to allow direct comparison between heating DHW using
C a fuel cell with a conventional system.

C INPUTS:
C    ptimef                  `future' time of current day in hour fraction (0. to 24.) for
C                            current plant simulation increment (from COMMON/ptime)
C    ptimep                  `present' time of current day in hour fraction (0. to 24.) for
C                            current plant simulation increment (from COMMON/ptime).
C    TIMSEC                  time-step in explicit plant domain (sec) (from common/PCTIME)
C    idyp                    day number (1-365) at present time-row (from common/simtim)
C    idyf                    day number (1-365) at future time-row (from common/simtim)
C    T_node1                 `present' temperature of water in tank (oC)
C    IUOUT                   output channel to feedback to user
C    tank_DHW_draw           flag indicating DHW draw treatment (from MODULE FC_tanks)
C    tank_no_DHW             ditto
C    tank_CSA_DHW            ditto
C    FC_DHW_nominal_tank_T   nominal tank temp (oC) used as basis for determining actual
C                            draw (from MODULE FC_tanks)
C    FC_DHW_CSA_fraction(j)  fraction of total daily water draw that occurs during
C                            hour `j' (from MODULE FC_tanks)
C    FC_DHW_nominal_Lperday  water draw for given occupancy, based on draw at
C                            `FC_DHW_nominal_tank_T' (from MODULE FC_tanks)

C OUTPUTS:
C    DHW_draw_present    DHW water draw (m3/s) at the present time-row.
C    DHW_draw_future     DHW water draw (m3/s) at the future time-row.
C    T_makeup_present    temperature (oC) of make-up water (from mains) at present time-row.
C    T_makeup_future     temperature (oC) of make-up water (from mains) at future time-row.
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_DHW_load(T_tank)

      IMPLICIT NONE
#include "building.h"
#include "cogen_tanks.h"
#include "ground_temp_mains_common.h"
#include "dhw_common.h"

      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its


      COMMON/ptime/ptimep,ptimef
      REAL ptimep,ptimef

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      REAL T_tank 
      REAL adjust_p,adjust_f
      INTEGER hour_p,hour_f,iday_p,imonth_p,iday_f,imonth_f
      LOGICAL close_to_zero
      REAL PI

C-----Determine the month number at the present and future time-rows.
      CALL EDAYR(idyp,iday_p,imonth_p)
      CALL EDAYR(idyf,iday_f,imonth_f)
C-----Calculate the make-up water temperature using the Moore model implemented in
C-----file /esrubps/ground_temp_mains.F.
      T_makeup_present = fDHW_ColdMainTemp(imonth_p)
      T_makeup_future  = fDHW_ColdMainTemp(imonth_f)

C--------------------------------------------------------------------------------------------
C This may be a: combo system; a space-heating system; or a DHW system. If the tank supplies
C only space heating, then the user will have set a flag in the .pln file indicating that the
C tank does not supply DHW. Determine the tank configuration and set the DHW water draw
C accordingly.
C--------------------------------------------------------------------------------------------
      SELECT CASE (tank_DHW_draw)


C--------------------------------------------------------------------------------------------
C Tank does not supply DHW.
C--------------------------------------------------------------------------------------------
        CASE (tank_no_DHW)
          DHW_draw_present = 0.
          DHW_draw_future  = 0.


C--------------------------------------------------------------------------------------------
C Tank supplies DHW water draw following CSA draw profile (with corrections for temperature
C and occupant density).  The water draws are based upon the CSA draw profile, which is
C given in hourly terms.  The water draw is adjusted to account for the current tank
C temperature using a technique from the HOT2000 bin model.  This assumes that 25% of
C the hot-water usage (in terms of flow rate) is not affected by the tank temperature
C (probably due to appliances such as dishwashers and clothes washers).  The remaining 75%
C is affected by tank temperature (probably due to showers, etc).  For this 75%, the flow
C rate is increased if the tank is below the assumed standard supply temperature
C of `FC_DHW_nominal_tank_T'.  The flow rate is increased so that the energy delivered
C is equivalent to the standard flow rate at `FC_DHW_nominal_tank_T'.
C--------------------------------------------------------------------------------------------
        CASE (tank_CSA_DHW)
C---------Determine the hour of the day at the present and future time-rows.
          hour_p = INT(PTIMEP)
          if( hour_p.eq.0 ) hour_p = 24  ! Avoids going out of bounds on array.
          hour_f = INT(PTIMEF)
          if( hour_f.eq.0 ) hour_f = 24
C---------Calculate the adjustment to the draw to account for the current tank temperature,
C---------at the present and future time-rows.
C---------
C---------Modification: Coefficients adjust_p and adjust_f become meaningless when the 
C---------tank temperature and makeup temperatures are approximately equal, but their 
C---------values grow very large under these circumstances. Therefore, check to see 
C---------if the denominator (tank temperature - makeup temperature) is close to zero
C---------and set the resulting coefficient to zero if this is the case. This modification
C---------effectively zero's the DHW draw under these circumstances, but if the tank 
C---------temperature is anywhere near the make-up temperature it is extremely unlikely 
C---------that the occupant will be using the tank for DHW use anyway.
          CALL ECLOSE(T_tank-T_makeup_present, 0.0, 1.0, close_to_zero)
          IF (close_to_zero) THEN
             adjust_p = 0.
          ELSE
             adjust_p = ( FC_DHW_nominal_tank_T - T_makeup_present ) /
     &            ( T_tank - T_makeup_present )
          ENDIF
          CALL ECLOSE(T_tank-T_makeup_future, 0.0, 1.0, close_to_zero)
          IF (close_to_zero) THEN
             adjust_f = 0.
          ELSE
             adjust_f = ( FC_DHW_nominal_tank_T - T_makeup_future ) /
     &            ( T_tank - T_makeup_future )
          ENDIF
C---------Calculate the water draws (L/hr) at the present and future time-rows.
          DHW_draw_present =
     &            FC_DHW_CSA_fraction(hour_p) * FC_DHW_nominal_Lperday *
     &            ( 0.25 + 0.75 * adjust_p )
          DHW_draw_future =
     &            FC_DHW_CSA_fraction(hour_f) * FC_DHW_nominal_Lperday *
     &            ( 0.25 + 0.75 * adjust_f )
C---------Adjust units of water draws (m3/s) to pass to coefficient generator.
          DHW_draw_present = DHW_draw_present / 1000. / 3600.
          DHW_draw_future  = DHW_draw_future  / 1000. / 3600.


C--------------------------------------------------------------------------------------------
C Other DHW draw profiles not yet implemented.
C--------------------------------------------------------------------------------------------
        CASE DEFAULT
          WRITE(IUOUT,*) ' FC_DHW_load: '
          WRITE(IUOUT,*) ' DHW draw profile not yet supported. '
          STOP ' FC_DHW_load: unresolvable error'


C--------------------------------------------------------------------------------------------
      END SELECT

      RETURN
      END



C *********************************************************************************************
C ****************************** FC_tank_elec_static_temp *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 5, 2001
C Copyright CETC 2001

C This subroutine is the static template for an electrically heated water tank that is fed by
C a residential fuel cell system. It was developed for connection to the residential
C fuel cell component model but could be used in other plant arrangements as well.
C The model supports the following configurations :
C   1) a combo tank that supplies a fan-coil for space heat + domestic hot water (DHW)
C   2) a tank that supplies a fan-coil for space heat
C   3) a tank that supplies DHW

C This subroutine checks whether the component is properly used in the user-defined
C plant network, it assigns user-input data to module variables, and it performs
C time-invariant calculations.

C The tank, including its electric resistance heater, is represented with a single node:

C INPUTS:
C    IPCOMP       index number of the component under consideration
C    NPCOMP       number of plant components in user-defined plant network
C    NCI(i)       holds the number of possible plant control variables for plant component `i'
C    ADATA(i,j)   holds `j'th data item for component `i' read from .pln file
C                 (derived from plant components database)
C    ICONTP(i)    holds the state variable index (ISV) of the sending node for connection `i'.
C    IUOUT        output channel to feedback to user
C    ITC          trace output index (>0 gives a trace output)
C    ITU          output channel for trace output
C    ITRACE(35)   flag indicating one of the plant traces (1= active)
C    FC_DHW_CSA_fraction(j)    Nominal DHW draw (L/hour) for hour `j' based on CSA schedule
C                              (in MODULE FC_tanks)
C    NPCDAT(i,4)  pointer to static template and coefficient generator for component `i'
C                 (in common/C12PS)

C OUTPUTS:
C    tank_mass                 mass of node 1, kg (in MODULE FC_tanks)
C    tank_Cp                   specific heat of node 1, J/kgK (in MODULE FC_tanks)
C    tank_UA                   heat loss coefficient between node 1 and surroundings,
C                              W/K (in MODULE FC_tanks)
C    element_capacity_ON       resistance heater heat output when on, W (in MODULE FC_tanks)
C    element_capacity_OFF      resistance heater heat output when off, W (in MODULE FC_tanks)
C    tank_DHW_draw             flag indicating whether tank supplies DHW (in MODULE FC_tanks)
C    tank_space_heat           flag indicating whether tank is connected to a fan-coil
C                              that supplies space heating (in MODULE FC_tanks)
C    FC_DHW_nominal_Lperday    Nominal DHW draw (L/day) based on occupancy (in MODULE FC_tanks)
C    FC_DHW_CSA_Lperday        Nominal DHW draw (L/day) based on CSA schedule
C                              (in MODULE FC_tanks)
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_tank_elec_static_temp(IPCOMP)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "SOFC.h"
#include "cogen_tanks.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER  ITCF,ITRACE,IZNTRC,ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),
     & IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA,Itemp,N_expect,IPCONC
      LOGICAL mistake,CLOSE

      INTEGER IPCOMP,I,J


C---------------------------------------------------------------------------------
C The electrically heated and gas-fired water tanks cannot be used concurrently
C because they share common variables through MODULE FC_tanks. If there is a need
C in the future to allow multiple storage tanks, this restriction can be overcome
C by creating separate MODULEs for the two component models.
C Variables used:
C    NPCDAT(i,4)  pointer to static template and coefficient generator for component `i'
C                 (in common/C12PS): =81 for gas-fired component model.
C    NPCOMP       number of plant components in user-defined plant network
C---------------------------------------------------------------------------------
      DO I=1,NPCOMP
        IF( NPCDAT(I,4) .eq. 81 )THEN
          WRITE(IUOUT,*) ' FC_tank_elec_static_temp: the elec'
          WRITE(IUOUT,*) ' and gas-fired water tanks cannot be used'
          WRITE(IUOUT,*) ' concurrently.'
          STOP ' FC_tank_elec_static_temp: unresolvable error'
        END IF
      END DO


C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. ITRACE(35).ne.0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 1 node elec heated water tank  '
        NumADATA = 7  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NumADATA)
        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(IPCOMP) holds the number of possible plant control variables
C as specified in the .pln file. There should be a single control variable.
C---------------------------------------------------------------------------------
      N_expect = 1
      IF(NCI(IPCOMP) .ne. N_expect) THEN
        WRITE(ITU,*) ' FC_tank_elec_static_temp warning: ',
     &               ' incorrect num of ctl variables specified.'
      ENDIF


C---------------------------------------------------------------------------------
C Check that the node in the component has the correct number of connections
C to other components.
C Variables used:
C    MPCONC           the maximum allowable connections to a node (from plant.h).
C    ICONDX(i,j,k)    the connection number for the k'th connection to the j'th node
C                     of component i. It is used as a pointer.
C    `mistake'        a flag indicating whether there are connection errors:
C                     .true. means there are errors.
C---------------------------------------------------------------------------------
      mistake = .false.
C-----There should be two connections to node 1.
      IF( ICONDX(IPCOMP,1,1) .eq. 0 ) mistake=.true.
      IF( ICONDX(IPCOMP,1,2) .eq. 0 ) mistake=.true.
      DO IPCONC=3,MPCONC
        IF( ICONDX(IPCOMP,1,IPCONC) .ne. 0 ) mistake=.true.
      END DO
C-----Write error message if the number of connections to the node is incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' FC_tank_elec_static_temp: incorrect'
        WRITE(IUOUT,*) ' number of connections for component ',IPCOMP
        STOP ' FC_tank_elec_static_temp: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connections to node 1 are of the correct type. The two connections
C to node 1 should be of type ISV=20 so that the tank component can be used in
C networks in which both first and second phase mass balances are formed.
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
      IF( ICONTP( ICONDX(IPCOMP,1,1) ) .ne. 20 ) mistake=.true.
      IF( ICONTP( ICONDX(IPCOMP,1,2) ) .ne. 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' FC_tank_elec_static_temp: incorrect'
        WRITE(IUOUT,*) ' connection type to node 1 for comp ',IPCOMP
        STOP ' FC_tank_elec_static_temp: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Assign user-specified mass (kg) and specific heat (J/kgK) of node 1
C (water in tank + casing).
C---------------------------------------------------------------------------------
      tank_mass = ADATA(IPCOMP,1)
      tank_Cp   = ADATA(IPCOMP,2)


C---------------------------------------------------------------------------------
C Assign user-specified heat loss coefficient (UA-value) between node 1 and
C surroundings (W/K).
C---------------------------------------------------------------------------------
      tank_UA = ADATA(IPCOMP,3)


C---------------------------------------------------------------------------------
C Assign user-specified resistance heater element capacity (W) for on and off
C states. Off-state will normally have zero heating capacity, but the user can
C alter this.
C---------------------------------------------------------------------------------
      element_capacity_ON  = ADATA(IPCOMP,4)
      element_capacity_OFF = ADATA(IPCOMP,5)


C---------------------------------------------------------------------------------
C Assign variable indicating whether there is a DHW as well as a space-heating
C draw from the storage tank. The DHW draw is not treated as a `connection'
C but rather is handled within the coefficient generator as a right-hand side
C term, this to simplify the plant networks.
C---------------------------------------------------------------------------------
      tank_DHW_draw = ADATA(IPCOMP,6)


C---------------------------------------------------------------------------------
C Assign variable indicating whether the tank is connected to a fan-coil that
C supplies space heat.  The tank may be configured to supply DHW, space-heat,
C or both.
C---------------------------------------------------------------------------------
      tank_space_heat = ADATA(IPCOMP,7)


C---------------------------------------------------------------------------------
C Determine the DHW nominal water draw. This equation is taken from the HOT3000
C DHW model, which took it directly from the HOT2000 bin model.
C---------------------------------------------------------------------------------
      FC_DHW_nominal_Lperday = 85. + ( 35. * float(FC_NumOfOccupants) )


C---------------------------------------------------------------------------------
C Determine the fraction of the daily DHW water draw that is drawn at each hour of
C the day. The actual water draws will be determined using these hourly profiles,
C the total DHW draw over the day, and the time-varying tank temperature.
C---------------------------------------------------------------------------------
      FC_DHW_CSA_Lperday = 0.
      DO j=1,24
        FC_DHW_CSA_Lperday = FC_DHW_CSA_Lperday
     &                     + FC_DHW_CSA_Lperhour(j)
      END DO
      DO j=1,24
        FC_DHW_CSA_fraction(j) = 
     &                       FC_DHW_CSA_Lperhour(j) / FC_DHW_CSA_Lperday
      END DO


C---------------------------------------------------------------------------------
C Completion of time-invariant operations for component.
C---------------------------------------------------------------------------------
      RETURN
      END



C *********************************************************************************************
C ******************************** FC_tank_elec_coeff_gen *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 5, 2001
C Copyright CETC 2001

C This subroutine is the coefficient generator for the electrically heated water tank
C component model. It was developed for connection to the residential
C fuel cell component model but could be used in other plant arrangements as well.
C The model supports the following configurations :
C   1) a combo tank that supplies a fan-coil for space heat + domestic hot water (DHW)
C   2) a tank that supplies a fan-coil for space heat
C   3) a tank that supplies DHW

C The DHW loads are treated as `connectionless'.  The DHW water draws and make-up water
C temperature are calculated within the coefficient generator.  These appear on the RHS
C of the energy balance for the water node.  This is done to minimize the number of
C components and connections and to allow for time-varying draws and make-up water
C temperatures.

C The tank is configured to be connected to a fuel cell. A second connection to the
C water node ia also required.  A flag can be set to indicate whether the tank supplies
C a fan-coil unit for space heating.  If the tank does supply space heat, then this
C second connection is to the fan-coil system.  If the tank does not supply space heat,
C then a second connection is required to satisfy the matrix requirements.  However,
C this connection is a `dummy'.  This coefficient generator nullifies the impact of
C this second connection.  A constant temperature supply is recommended as the inlet
C for this dummy connection: a pipe is recommended for the output.

C This subroutine performs the time-step calculations and establishes coefficients for the
C sub-matrices that define the energy, 1st phase mass flow, and 2nd phase mass
C flow balances on the storage tank's node. A weighted implicit-explicit energy balance
C is formed for the single node.

C The tank, including the electric resistance heating element, is represented with
C a single node.

C INPUTS:
C    tank_mass                 mass of node 1, kg (in MODULE FC_tanks)
C    tank_Cp                   specific heat of node 1, J/kgK (in MODULE FC_tanks)
C    tank_UA                   heat loss coefficient between node 1 and surroundings,
C                              W/K (in MODULE FC_tanks)
C    element_capacity_ON       resistance heater heat output when on, W (in MODULE FC_tanks)
C    element_capacity_OFF      resistance heater heat output when off, W (in MODULE FC_tanks)
C    tank_DHW_draw             flag indicating whether tank supplies DHW (in MODULE FC_tanks)
C    tank_space_heat           flag indicating whether tank is connected to a fan-coil
C                              that supplies space heating (in MODULE FC_tanks)
C    PCNTMP(i)                 present time-row temperature of room containing
C                              component `i' (common/PCVAR)
C    PCNTMF(i)                 future time-row temperature of room containing
C                              component `i' (common/PCVAR)
C    IMPEXP                    identifies implicit/explicit handling of plant
C                              equations (common/PCEQU)
C    RATIMP                    user-specified implicit weighting fraction (common/PCEQU)
C    TIMSEC                    time-step in explicit plant domain (seconds) (common/PCTIME)
C    CDATA(i,j)   control signal for component `i', node `j' (common/C9) 

C OUTPUTS:
C    COUT(i)             coefficient for ISTATS balance for `i'th matrix position
C    NAPDAT(i)           number of "additional" output for component `i'
C    PCAOUT(i,j)         additional output `j' for component `i'
C    TC(i)               time constant (seconds) of plant component `i'
C -------------------------------------------------------------------------------------------

      SUBROUTINE FC_tank_elec_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "SOFC.h"
#include "cogen_tanks.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "OffsiteUtilitiesPublic.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER  ITCF,ITRACE,IZNTRC,ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,
     & itrclp,ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP
      REAL PERREL,PERTMP,PERFLX,PERMFL
      INTEGER itrclp,ICSV
      REAL CSVI

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     & IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),
     & IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2


      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),
     & PCQF(MPNODE),PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),
     & PUAP(MPNODE),PCQP(MPNODE),PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP
      REAL PCQP,PCNTMP

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/PCTC/TC(MPCOM)
      REAL TC

      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP
      REAL RATIMP

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

C     Common for determining if the plant domain has converged
      COMMON/pltcon/bpltcon,lastTStp
      LOGICAL bpltcon
      REAL lastTStp

C---------------------------------------------------------------------------------
C Declare local variables. These variables are defined at their first use.
C---------------------------------------------------------------------------------
      LOGICAL CLOSE
      REAL COUT(MPCOE) ! plant matrix coefficients (passed in calling statement)
      REAL tank_UA_future,tank_UA_present
      REAL T_node1_present,T_node1_future
      REAL mdotCp_1a_future,mdotCp_1b_future,mdotCp_1a_present,
     &     mdotCp_1b_present
      REAL T_1a_present,T_1b_present
      REAL Rinv,alpha
      REAL part1,part2,part3,part4,part5,part6,part7,part8

      INTEGER IPCOMP
      INTEGER ISTATS
      INTEGER ITEMP
      REAL SHTFLD
      REAL RHOFLD
      INTEGER I
      

C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC.gt.0 .AND. NSINC.ge.ITC .AND.NSINC.le.ITCF 
     & .AND.ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Entering sub FC_tank_fuel_coeff_gen'
      END IF


C---------------------------------------------------------------------------------
C Set local variables to point to the nodes and to the connections.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C                   within the global plant matrix.
C    node1          global matrix node number for single node
C    con1a          number of first connection to node 1 (to fuel cell or fan coil)
C    con1b          number of second connection to node 1 (to fuel cell or fan coil)
C---------------------------------------------------------------------------------
      node1 = NPCDAT(IPCOMP,9)
      con1a = ICONDX(IPCOMP,1,1)
      con1b = ICONDX(IPCOMP,1,2)


C---------------------------------------------------------------------------------
C Mark the temperature of node 1 for iteration.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C---------------------------------------------------------------------------------
      ICSV(node1,1) = 1
      CSVI(node1,1) = CSVF(node1,1)


C---------------------------------------------------------------------------------
C Generate coefficients for the appropriate equation set. This subroutine was
C called to either generate coefficients for energy balances (ISTATS=1), 1st
C phase mass balances (ISTATS=2), or 2nd phase mass balances (ISTATS=3).
C Start with the energy balances.
C---------------------------------------------------------------------------------

C*********************************************************************************
C Beginning of energy balance section ********************************************
C*********************************************************************************
      IF(ISTATS.eq.1) THEN


C---------------------------------------------------------------------------------
C Determine the `present' and `future' temperature of the room containing the
C water tank. This is used to calculate the heat lost from the hot tank to the
C room (Beausoleil-Morrison 2001b, eq 4).
C Variables used:
C    PCNTMP(i)  present time-row temperature of room containing component `i'.
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' FC_tank_elec_coeff_gen: '
          WRITE(IUOUT,*) ' the storage tank must be contained within '
          WRITE(IUOUT,*) ' a room.'
          STOP ' FC_tank_tank_coeff_gen: unresolvable error'
        ELSE
          Troom_present = PCNTMP(IPCOMP)
          Troom_future  = PCNTMF(IPCOMP)
        END IF


C---------------------------------------------------------------------------------
C Establish the heat loss coefficient (UA-value, W/K) between node 1 and the
C surroundings for the present and future time-rows. A fixed UA-value is used here.
C However, this code is structured to easily enable a time-varying UA-value in the
C future. PUAP and PUAF are standard ESP-r variables holding the present and future
C time-row values of the UA-value. PUAF is mapped to PUAP by subroutine MZNASS
C following the solution of the energy matrix for the current time-step.
C Variables used:
C    PUAP(i)  present time-row UA-value
C    PUAF(i)  future time-row UA-value 
C---------------------------------------------------------------------------------
C-------Determine UA.
        tank_UA_future  = tank_UA  ! `tank_UA' from dbase (could be replaced by a correlation)
C-------Save future value.
        PUAF(node1)     = tank_UA_future
C-------Set present value.
        tank_UA_present = PUAP(node1)


C---------------------------------------------------------------------------------
C     Determine most recently calculated mass flow:
C     CONVAR(i,2) water mass flow rate (kg/s) at sending node for connection `i'
C     PCONDR(i)   ratio of flow rate leaving sending node `i' that reaches
C                receiving node.
C---------------------------------------------------------------------------------
         mdot_conn1a_future = PCONDR(con1a) * CONVAR(con1a,2)  ! (kg/s)
         mdot_conn1b_future = PCONDR(con1b) * CONVAR(con1b,2)  ! (kg/s)

C---------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the two
c connections to node 1, for the present and future time-rows. These are the heat
C capacity rates (W/K) of the the water entering the storage tank from the fuel
C cell and from the fan-coil return. PCRP and PCRF are standard ESP-r variables
C holding the present and future time-row values of the mass flow * heat capacity.
C PCRF is mapped to PCRP by subroutine MZNASS following the solution of the energy
C matrix for the current time-step.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C    CONVAR(i,2) water mass flow rate (kg/s) at sending node for connection `i'
C    PCONDR(i)   ratio of flow rate leaving sending node `i' that reaches
C                receiving node.
C    SHTFLD(3,T) function that returns heat capacity (J/kgK) of water (index 3)
C                at temperature `T'.
C    PCRP(i)     present time-row mass flow * heat capacity for connection `i'
C    PCRF(i)     future time-row mass flow * heat capacity for connection `i'
C---------------------------------------------------------------------------------
         mdotCp_1a_future = mdot_conn1a_future
     &                    * SHTFLD( 3,CONVAR(con1a,1) )
C--------If space-heat flag set, then second connection is real: otherwise make flow zero.
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           mdotCp_1b_future = mdot_conn1b_future
     &                      * SHTFLD( 3,CONVAR(con1b,1) )
         ELSE
           mdotCp_1b_future = 0.
         END IF
C--------Save future values.
         PCRF(con1a) = mdotCp_1a_future
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           PCRF(con1b) = mdotCp_1b_future
         END IF
C--------Set present values.
         mdotCp_1a_present = PCRP(con1a)
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           mdotCp_1b_present = PCRP(con1b)
         ELSE
           mdotCp_1b_present = 0.
         END IF



C---------------------------------------------------------------------------------
C Establish the present time-row temperatures of the water flowing into the tank
C at the two connections.  PCTP and PCTF are standard ESP-r variables holding the
C present and future time-row values of the temperature of the fluid flowing at
C the connection. PCTF is mapped to PCTP by subroutine MZNASS following the
C solution of the energy matrix for the current time-step.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C    PCTP(i)     present temperature of sending node for connection `i'
C    PCTF(i)     future temperature of sending node for connection `i'
C---------------------------------------------------------------------------------
         T_1a_present = PCTP(con1a)
C--------If space-heat flag set, then second connection is real: otherwise make temp zero.
         IF( tank_space_heat.eq.tank_yes_heat )THEN
           T_1b_present = PCTP(con1b)
         ELSE
           T_1b_present = 0.
         END IF


C---------------------------------------------------------------------------------
C Establish the present time-row temperature of node 1.
C Variables used:
C     CSVP(i,j)  present time-row solution variable for node `i' and variable `j';
C                j=1 for temperature
C     CSVF(i,j)  future time-row solution variable (same indices as with CSVP)
C---------------------------------------------------------------------------------
         T_node1_present = CSVP(node1,1)
         T_node1_future  = CSVF(node1,1)


C---------------------------------------------------------------------------------
C Check the temperature of the tank and activiate the fictitious heat dump if
C necessary to maintain the temperature within a reasonable range. This should
C prevent the occurence of boiling and will allow the examination of scenarios for
C dealing with excessive energy build-up in the system.
C---------------------------------------------------------------------------------
         CALL FC_dump_excess_energy(node1)



C---------------------------------------------------------------------------------
C Check the temperature of the tank to ensure that the water is not going to
C boil. This may occur is the fuel cell consistently adds more energy to the
C tank than is drawn off to meet the space heating and/or DHW loads. This should
C never occur with addition of call above to `FC_dump_excess_energy', but left
C here anyway for safety.
C---------------------------------------------------------------------------------
         CALL FC_check_for_boil(node1)


C---------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the DHW draw
C on the tank, for the present and future time-rows. Also determine the temperature
C of the DHW make-up water, for the present and future time-rows.
C Variables used:
C    RHOFLD(3,T)   function that returns density (kg/m3) of water (index 3) at
C                  temperaturere `T'.
C    SHTFLD(3,T)   function that returns heat capacity (J/kgK) of water (index 3)
C                  at temperature `T'.
C
C    Modification: Check to see if the tank 
C---------------------------------------------------------------------------------
        CALL FC_DHW_load(T_node1_future)

        mdotCp_DHW_present = DHW_draw_present
     &                     * RHOFLD(3,T_makeup_present)
     &                     * SHTFLD( 3,T_makeup_present )
        mdotCp_DHW_future  = DHW_draw_future
     &                     * RHOFLD(3,T_makeup_future)
     &                     * SHTFLD( 3,T_makeup_future ) 


C---------------------------------------------------------------------------------
C Calculate the energy added by the electric resistance heating element. The
C heating element will either be on or off, depending on the control signal.
C CDATA=0 signals that the heating element should be off; CDATA=1 signals that it
C should be on.
C    CDATA(i,j)   control signal for component `i's node `j'.
C---------------------------------------------------------------------------------
C-------Determine whether the heating element is on or off this time-step.
        CALL ECLOSE( CDATA(IPCOMP,1),0.,0.0001,CLOSE )
        IF(CLOSE)THEN  ! Element is in "off" state (there may be heat output still)
          q_element = element_capacity_OFF
        ELSE
          q_element = element_capacity_ON
        ENDIF


C---------------------------------------------------------------------------------
C All of the energy liberated from the heating element is transferred to the water
C PCQP and PCQF are standard ESP-r variables holding the present and future
C time-row values of nodal injections (the energy transferred from the combustion
C to the water in this case). PCQF is mapped to PCQP by subroutine MZNASS
C following the solution of the energy matrix for the current time-step.
C Variables used:
C    PCQP(i)     present energy transferred to water from combustion for node `i'
C    PCQF(i)     future energy transferred to water from combustion for node `i'
C---------------------------------------------------------------------------------
        q_capture_future = q_element
C-------Save future value.
        PCQF(node1) = q_capture_future
C-------Set present value.
        q_capture_present = PCQP(node1)


C---------------------------------------------------------------------------------
C Calculate component's time-constant for the current time-step. Refer to
C Hensen (1991) pages 5.9 and 5.10.  The environmental losses from the tank and
C the water streams flowing into the tank are the three heat transfers that
C affect the transient storage. Therefore, treat these as parallel paths for
C the purposes of calculating the time constant, as described in eq 5.5 of
C Hensen (1991).
C Variables used:
C     tank_mass    mass of node 1, kg
C     tank_Cp      specific heat of node 1, J/kgK
C     TC(i)        time constant (seconds) of plant component `i'
C---------------------------------------------------------------------------------
        Rinv = tank_UA_future + mdotCp_1a_future + mdotCp_1b_future
        CALL ECLOSE(Rinv,0.,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' FC_tank_elec_coeff_gen: '
          WRITE(IUOUT,*) ' impossible time constant. '
          STOP ' FC_tank_elec_coeff_gen: unresolvable error'
        END IF
        TC(IPCOMP) = tank_mass*tank_Cp / Rinv


C---------------------------------------------------------------------------------
C Determine implicit/explicit weighting for the energy balance on node 1. `alpha'
C is the implicit weighting factor used to form the energy balance this time-step.
C It is the Greek "alpha" in Beausoleil-Morrison (2001b) equation 7. The approach
C used here to determine `alpha' is used with most plant components, as discussed
C by Hensen (1991).
C Variables used:
C    IMPEXP   identifies handling of plant equations: 1= fully implicit case;
C             2= mixed explicit/implicit; 3= 1 or 2 depending on component time
C             constant and time-step (this is the default treatment);
C             4= steady state.
C    RATIMP   user-specified implicit weighting fraction
C    TIMSEC   the time-step in the explicit plant domain (seconds)
C---------------------------------------------------------------------------------
C-------Fully implicit.
        IF( IMPEXP.eq.1 ) THEN
          alpha = 1.
C-------Implicit/explicit with user-specified (fixed) weighting.
        ELSE IF( IMPEXP .eq. 2 ) THEN
          alpha = RATIMP
C-------General case: implicit/explicit with calculated weighting.
        ELSE IF( IMPEXP .eq. 3 ) THEN
          IF( TIMSEC>0.63*TC(IPCOMP) ) THEN
            alpha = 1.
          ELSE
            alpha = RATIMP
          END IF
C-------Steady-state.
        ELSE IF( IMPEXP .eq. 4 ) THEN
          tank_mass = 0.  ! make node 1 massless
          alpha = 1.
        END IF


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.
C Node coefficient transport for COUT (refer to Beausoleil-Morrison 2001b,
C Figure 2):
C       <-self->| <---cross-->
C  node   1     |  fc fan-coil    RHS
C       -------------------------------
C         1     |   2    3          4
C---------------------------------------------------------------------------------
C-------Node 1 energy balance given by Beausoleil-Morrison (2001b) eq 7 & 17.
        COUT(1) = tank_mass * tank_Cp / TIMSEC       ! self-coupling to itself (W/K)
     &          + alpha * tank_UA_future
     &          + alpha * mdotCp_1a_future
     &          + alpha * mdotCp_1b_future
     &          + alpha * mdotCp_DHW_future
        COUT(2) = -1. * alpha * mdotCp_1a_future     ! cross-coupling (W/K)
        COUT(3) = -1. * alpha * mdotCp_1b_future     ! cross-coupling (W/K)
        part1 = tank_mass * tank_Cp / TIMSEC         ! (W/K)
     &        - (1.-alpha) * tank_UA_present
     &        - (1.-alpha) * mdotCp_1a_present
     &        - (1.-alpha) * mdotCp_1b_present
     &        - (1.-alpha) * mdotCp_DHW_present
        part2 = (1.-alpha) * mdotCp_1a_present       ! (W)
        part3 = (1.-alpha) * mdotCp_1b_present       ! (W)
        part6 = (1.-alpha) * mdotCp_DHW_present      ! (W)
        part7 = alpha * mdotCp_DHW_future            ! (W)
        part4 = (1.-alpha) * q_capture_present       ! (W)
     &        + alpha * q_capture_future
        part5 = (1.-alpha) * tank_UA_present * Troom_present
     &        + alpha * tank_UA_future * Troom_future
        part8 = -1.*(1.-alpha) * q_heatdump_present  ! (W)
     &          -1*alpha * q_heatdump_future
        COUT(4) = part1 * T_node1_present            ! RHS (W)
     &          + part2 * T_1a_present
     &          + part3 * T_1b_present
     &          + part6 * T_makeup_present
     &          + part7 * T_makeup_future
     &          + part4
     &          + part5
     &          + part8

C---------------------------------------------------------------------------------
C Save the `additional' output variables for outputting on time-step basis.
C---------------------------------------------------------------------------------
      NAPDAT(IPCOMP)    = 1                    ! number of additional outputs
      PCAOUT(IPCOMP,1)  = q_element            ! output of heating element (W)

C---------------------------------------------------------------------------------
C Save time step electric element input for solar DHW .h3k output
C---------------------------------------------------------------------------------
      fSDHW_TSTank_Elec_watts = q_element

C ...add to XML reporting
      call add_to_xml_reporting(
     &        fSDHW_TSTank_Elec_watts,
     &        'plant/SDHW_summary/DHW_tank_elec',
     &        'units','(W)',
     &        'Electric element input of the DHW tank')
C.....Save Elecrical energy use in storage array.
      fSUFuelEnergyUse( iElectricity, iUseUncatagorized) = q_element
C.....Pass storage array to Site utilities facility.
      call StorePltCompEnergyUse( iPComp, fSUFuelEnergyUse )
C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      ELSE IF(ISTATS.eq.2) THEN
C-------Node 1: total flow in equals total flow out.
        COUT(1) = 1.
        COUT(2) = -1. * PCONDR(con1a) ! (dimensionless)
C-------If space-heat flag set, then second connection is real: otherwise make flow zero.
        IF( tank_space_heat.eq.tank_yes_heat )THEN
          COUT(3) = -1. * PCONDR(con1b)
        ELSE
          COUT(3) = 0.
        END IF
        COUT(4) = 0.


C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      ELSE IF(ISTATS.eq.3) THEN
C-------Node 1: no balance required so make flow zero.
        COUT(1) = 1.
        COUT(2) = 0.
        COUT(3) = 0.
        COUT(4) = 0.

      END IF

C*********************************************************************************
C End of energy and mass balances ************************************************
C*********************************************************************************

C Record the electrical draw (W) of the electric resistance heater for use by the
C fuel cell controller.
      FCctl_elec_water_tank = q_element

C---------------------------------------------------------------------------------
C Store results to calculate monthly averages and (optionally) to produce an
C ASCII file with time-step output. Note: pass 'present' (ie previous TS values
C of ALL state variables), as the final values of the future variables
C will not be determined until the next time step
C---------------------------------------------------------------------------------
      IF(ISTATS.eq.1) CALL H3KSTORE_FuelCell_Elec_Tank(
     &     q_element,           ! element power
     &     q_capture_future,    ! element -> water
     &     T_node1_present,      ! tank temperature
     &     PCTP(con1a),         ! temp of water at 1st connection
     &     mdot_conn1a_present, ! flow rate of water at 1st con
     &     PCTP(con1b),         ! temp of water at 2nd connection
     &     mdot_conn1b_present, ! flow rate of water at 2nd con
     &     T_makeup_future,     ! temp of DHW make-up water
     &     DHW_draw_future,     ! flow rate of DHW draw
     &     tank_UA_future,      !UA Tank
     &     Troom_future,        ! Predicted Temp of Room.            
     &     q_heatdump_future    ! rate of energy dumped
     &     )
C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC.le.ITCF .AND.
     &  ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 1 node elec heated storage tank for fuel cell'
        WRITE(ITU,*) ' Matrix node(s) ',node1
        WRITE(ITU,*) ' Connection(s)  ',con1a,con1b
        IF(ISTATS.eq.1) THEN
           WRITE(ITU,*) ' CDATA         = ',CDATA(IPCOMP,1),' (-)'
           WRITE(ITU,*) ' q_element        = ',q_element,' (W)'
           WRITE(ITU,*) ' q_capture_future  = ',q_capture_future,' (W)'
           WRITE(ITU,*) ' alpha        = ',alpha,' (-)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(I),I=1,4)
        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving sub FC_tank_elec_coeff_gen'
      END IF


      RETURN
      END


C *********************************************************************************************
C ************************************ FC_check_for_boil *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 21, 2001
C Copyright CETC 2001

C This subroutine checks the temperature of a specified node in the plant network.
C If the temperature of the water at that node approaches the boiling point,
C the simulation is halted and an error message displayed. The purpose of this subroutine
C is to prevent undiagnosed crashes that can occur when the HVAC system is configured
C such that the fuel cell consistently adds more energy to the tank than is drawn off to
C meet the space heating and/or DHW loads.

C INPUTS:
C    CSVF(i,1)  future time-row temperature for plant node `i' (oC).
C    node       number of node in plant network to check for overheating
C    IUOUT      output channel to feedback to user

C OUTPUTS:
C    Error message if network about to boil.
C-------------------------------------------------------------------------------------------

      SUBROUTINE FC_check_for_boil(node)

      IMPLICIT NONE
#include "plant.h"



      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/pltcon/bpltcon,lastTStp
      LOGICAL bpltcon
      REAL lastTStp

C-------------------------------------------------------------------------------------------
C Declare local and passed variables.
C-------------------------------------------------------------------------------------------
      REAL T_threshold
      INTEGER node

C-------------------------------------------------------------------------------------------
C Set threshold temperature (oC). If any nodes are hotter than this temperature then the
C simulation will be halted.
C-------------------------------------------------------------------------------------------
      T_threshold = 95.

C-------------------------------------------------------------------------------------------
C Only check the temperature of `node' if the plant domain converged the previous iteration
C through the component coefficient generators.
C-------------------------------------------------------------------------------------------
      IF(bpltcon) THEN
        IF( CSVF(node,1) > T_threshold ) THEN
          WRITE(IUOUT,*) ' FC_check_for_boil: water in tank about to '
          WRITE(IUOUT,*) ' boil. Space heat/DHW draws insufficient '
          WRITE(IUOUT,*) ' for energy added to tank by fuel cell.'
          STOP ' FC_check_for_boil: unresolvable error'
        END IF
      END IF

      RETURN
      END


C *********************************************************************************************
C ********************************** FC_dump_excess_energy ************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: February 15, 2002
C Copyright CETC 2002

C This subroutine executes a fictitious "heat dump". Essentially it extracts energy from the
C tank to ensure that water in the system does not boil.  This feature was added because it
C was found that the FC often adds more energy to the tank that is required for space heating
C and DHW purposes. As such, the HVAC system may require some kind of heat dump. This
C fictitious heat dump will allow manufacturers to analyze this situation without first
C having to explicitly design a heat dump into the system.

C INPUTS:
C    CSVF(i,1)           future time-row temperature for plant node `i' (oC).
C    node                number of node in plant network to check for overheating
C    FC_heatdump_max     tank temperature at which fictitious heat dump is
C                        activated, oC (from MODULE FC_tanks)
C    FC_heatdump_min     fictitious heat dump activated until tank temperature drops to
C                        this level, oC (from MODULE FC_tanks)
C    TIMSEC              time-step in explicit plant domain (seconds) (common/PCTIME)
C    NSINC               simulation increment counter (common/SIMTIM)

C OUTPUTS:
C    q_heatdump_present  heat dump rate at present time-row, which is incorporated
C                        into the water node's energy balance (W).
C    q_heatdump_future   heat dump rate at future time-row, which is incorporated
C                        into the water node's energy balance (W).
C-------------------------------------------------------------------------------------------

      SUBROUTINE FC_dump_excess_energy(node)

      IMPLICIT NONE
#include "plant.h"
#include "cogen_tanks.h"

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

      COMMON/pltcon/bpltcon,lastTStp
      LOGICAL bpltcon
      REAL lastTStp

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS


C-------------------------------------------------------------------------------------------
C Declare local and passed variables.
C-------------------------------------------------------------------------------------------
C      logical bpltcon  ! Boolean indicating whether plant domain has converged.
      INTEGER node


C-------------------------------------------------------------------------------------------
C Initialize heat dump rates if this is the first time the subroutine is called.
C-------------------------------------------------------------------------------------------
      IF(NSINC.eq.1) THEN
        q_heatdump_present = 0.
        q_heatdump_future  = 0.
      END IF

C-------------------------------------------------------------------------------------------
C Only check the temperature of `node' if the plant domain converged the previous iteration
C through the component coefficient generators.
C-------------------------------------------------------------------------------------------
      IF(bpltcon) THEN
C-------Check tank temperature and activate heat dump if it is too hot.
C-------Calculate rate of heat dump required to bring tank temperature down to
C-------`FC_heatdump_min' by end of time-step, in absence of other energy flows.
        IF( CSVF(node,1) > FC_heatdump_max ) THEN
          q_heatdump_future =
     &                 (CSVF(node,1)-FC_heatdump_min) *tank_mass*tank_Cp
     &                 / TIMSEC
        ELSE
          q_heatdump_future = 0.
        END IF

      END IF

      RETURN
      END



C *******************************************************************************************
C ************************* Gas_fired_tank_data_transport ***********************************
C Created by: Alex Ferguson
C Initial Creation Date: June 30, 2004
C
C Copyright CETC 2004
C
C This subroutine transfers miscellaneous (non-state-variable) data from the water tank
C model to the h3kreports object. The relevant data is stored in the 
C commonblock, and transported using the rep_set_meta / rep_report routines.
C
C--------------------------------------------------------------------------------------------

      Subroutine CETC_tank_data_transport(iComponentIndex,iTankType)

      implicit none 

#include "plant.h"
#include "cogen_tanks.h"

C.....Passed variables 
      integer iComponentIndex   ! index of FC in plant component network
      integer iTankType         ! flag for gas or electircal tank types

C.....ESP-r Commons
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15 pcname

      common/PCVAL/
     &     CSVF(MPNODE,MPVAR),  ! Plant future state variables
     &     CSVP(MPNODE,MPVAR)   ! Plant present state variables
      real CSVF, CSVP

C.....Plant component present/future variables 
      common/pcvar/pctf(mpcon),pcrf(mpcon),puaf(mpnode),pcqf(mpnode),
     & pcntmf(mpcom),pctp(mpcon),pcrp(mpcon),puap(mpnode),
     & pcqp(mpnode),pcntmp(mpcom)

      real pctf,pcrf,puaf,pcqf,pcntmf,pctp, pcrp, puap, pcqp, pcntmp

C.....Common contaning diversion ratios for plant network connections
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     & IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2

      REAL PCONDR,PCONSD

      common/report_loads/fReportDHWLoad(mpcom)
      real fReportDHWLoad                ! DHW load computed by a plant 
                                         ! component (W)

C.....Local Variables
      CHARACTER*128 H3K_rep_NAME ! H3Kreporting object
      character*128 root_name    ! root name of component node 
      character*128 hold_name    ! temporary variable for string manipulation
      logical bClose             ! flag for close-to-zero comparison

      integer iNameLen           ! Length of a plant component name
      integer iBaseLen           ! Length of a string.

C.....Named constants
      integer iTemperature      ! named constant for temperature properity 
      integer i1st_flow         ! named constant for first phase flow properity
      integer i2nd_flow    ! named constant for second phase flow properity      
      parameter ( iTemperature     = 1,
     &            i1st_flow        = 2, 
     &            i2nd_flow        = 3)
      integer iGas_tank         ! flag for gas-fired tank
      integer iEle_tank         ! flag for electrially heated tank
      parameter( iGas_tank = 1,
     &           iEle_tank = 2
     &     )

C.....Calculated values 
      real fWater_temperature   ! water temperature in tank (oC)
      real fConn_heat_injection ! heat injected/extracted by connection (W)
      real DHW_load             ! instantaneous dhw load (W)
      real fJacket_losses       ! losses to environment (W)

C.....References: 
      real H3K_connect_property ! function returning data for a given connection
      real SHTFLD     

C.....Update future row values using most recently calcuated data obtained
C.....from state variables

C.....Connection temperatures
      PCTF(con1a) = H3K_connect_property( con1a, iTemperature )
      PCTF(con1b) = H3K_connect_property( con1b, iTemperature ) 


      iNameLen = iPltNameLen(iComponentIndex)

C.....Format string as: 'plant/NAME/misc_data'

      WRITE(root_name,'(A,A,A)') 
     &     'plant/',
     &     pcname(iComponentIndex)(1:iNameLen),
     &     '/misc_data'

      iBaseLen = iNameLen + 16

C.....Water temperature
      fWater_temperature =  CSVF( node1, iTemperature ) 

      call add_to_xml_reporting(
     &     CSVF(node1,iTemperature),
     &     root_name(1:iBaseLen) // '/water_temperature',
     &     'units', '(oC)',
     &     'Hot water tank water temperature' )

C.....Connection 1 heat injection
C.....Heat injection = flow rate * ( T_in - T_tank)
      fConn_heat_injection = pcondr(con1a)  
     &     * H3K_connect_property(con1a,i1st_flow)
     &     * SHTFLD(3,(H3K_connect_property(con1a,iTemperature)
     &                + fWater_temperature )/2.0)
     &     * ( H3K_connect_property(con1a,iTemperature) -  
     &     fWater_temperature )
      

      call add_to_xml_reporting(
     &     fConn_heat_injection,
     &     root_name(1:iBaseLen) // '/connection_1/heat_injection',
     &     'units', '(W)',
     &     'Hot water tank connection 1: heat injection' )
      

C.....Connection 2 heat injection
C.....Heat injection = flow rate * Cp * ( T_in - T_tank)
      fConn_heat_injection = pcondr(con1b)
     &     * H3K_connect_property(con1b,i1st_flow)
     &     * SHTFLD(3,(H3K_connect_property(con1b,iTemperature)
     &                + fWater_temperature )/2.0)
     &     * ( H3K_connect_property(con1b,iTemperature) -  
     &     fWater_temperature )

      call add_to_xml_reporting(
     &     fConn_heat_injection,
     &     root_name(1:iBaseLen) // '/connection_2/heat_injection',
     &     'units', '(W)',
     &     'Hot water tank connection 2: heat injection' )


C.....DHW draw 
      call add_to_xml_reporting(
     &     dhw_draw_future,
     &     root_name(1:iBaseLen) //
     &       '/domestic_hot_water/volumetric_draw',
     &     'units', '(m3/s)',
     &     'Hot water tank: domestic hot water volumetric draw' )


C.......DHW make-up temperature 

      call add_to_xml_reporting(
     &     T_makeup_future,
     &     root_name(1:iBaseLen) //
     &           '/domestic_hot_water/make_up_temperature',
     &     'units', '(oC)',
     &     'Hot water tank: water make-up temperature' )

C.......DHW thermal load
C.......
C.......Calculate data:
C.......LOAD = (T - T_makeup) * (m * Cp)     
      
      DHW_load = (fWater_temperature - T_makeup_future) 
     &     * mdotCp_DHW_future

      call add_to_xml_reporting(
     &     DHW_load,
     &     root_name(1:iBaseLen) // '/domestic_hot_water/thermal_load',
     &     'units', '(W)',
     &     'Hot water tank: domestic hot water thermal load' )


      fReportDHWLoad(iComponentIndex) = DHW_load
C.....Gas-fired tank calculations
      if ( iTankType .eq. iGas_tank ) then

C.......Burner heat procuction

        call add_to_xml_reporting(
     &     q_burner,
     &     root_name(1:iBaseLen) // '/burner_heat_production',
     &     'units', '(W)',
     &     'Hot water tank: rate of burner heat production' )


C.......Fuel Flow

        call add_to_xml_reporting(
     &     m_dot_fuel_kg_t,
     &     root_name(1:iBaseLen) // '/fuel_flow',
     &     'units', '(kg/s)',
     &     'Hot water tank: burner fuel flow rate' )

C.......Volumetric fuel flow at STP.
C.......-> set name
        call add_to_xml_reporting(
     &     Vdot_fuel_STP_t,
     &     root_name(1:iBaseLen) // '/fuel_flow_vol_STP',
     &     'units', '(slpm)',
     &     'Hot water tank: burner fuel flow rate' )


C.......'gross heat input' (lower heating value flow rate)

        call add_to_xml_reporting(
     &     m_dot_fuel_kmol_t * LHV_fuel_tank * 1.0E06,
     &     root_name(1:iBaseLen) // '/fuel_flow_LHV_value',
     &     'units', '(W)',
     &     'Hot water tank: LHV of fuel used' )

     
C.......Air Flow


        call add_to_xml_reporting(
     &     m_dot_air_kg_t,
     &     root_name(1:iBaseLen) // '/air_flow',
     &     'units', '(kg/s)',
     &     'Hot water tank: burner air flow rate' )

C.......Exhaust Gas

        call add_to_xml_reporting(
     &     m_dot_exh_kg_t,
     &     root_name(1:iBaseLen) // '/exhaust_flow',
     &     'units', '(kg/s)',
     &     'Hot water tank: burner exhaust flow rate' )


        call add_to_xml_reporting(
     &     m_dot_fuel_kmol_t * LHV_fuel_tank * 1.0E06,
     &     root_name(1:iBaseLen) // '/energy_input',
     &     'units', '(W)',
     &     'Hot water tank: Energy requirements' )


      
      elseif( iTankType .eq. iEle_tank ) then
C........Operations specific to electrical tank would go here.

        call add_to_xml_reporting(
     &     q_element,
     &     root_name(1:iBaseLen) // '/energy_input',
     &     'units', '(W)',
     &     'Hot water tank: Energy requirements'  )


      endif 

C.......Heat_transfer to water


      call add_to_xml_reporting(
     &     q_capture_future,
     &     root_name(1:iBaseLen) // '/heat_transfer_to_water', 
     &     'units', '(W)',
     &     'Hot water tank: rate of heat transfer to water' )

C.......Jacket losses 
      fJacket_losses = tank_UA * (fWater_temperature - Troom_future)

      call add_to_xml_reporting(
     &     fJacket_losses,
     &     root_name(1:iBaseLen) // '/jacket_losses',
     &     'units', '(W)',
     &     'Hot water tank: rate of heat loss to surroundings' )

C---------------------------------------------------------------------------------
C       Store heat leakage for injection into containing zone
C       '1' indicates that gain is convective. 
C---------------------------------------------------------------------------------
       call store_plt_gain ( iComponentIndex,
     &                        fJacket_losses, iConvective)

     
C.......Heat dump

      call add_to_xml_reporting(
     &     q_heatdump_future,
     &     root_name(1:iBaseLen) // '/heat_dump',
     &     'units', '(W)',
     &     'Hot water tank: rate at which heat is "dumped" from '
     &      // 'tank to prevent water from boiling.' )


     
     
C.......H3Kreports.(HIGH_TEMP_FUEL_WATER_TANK).end........................


      return 
      end
