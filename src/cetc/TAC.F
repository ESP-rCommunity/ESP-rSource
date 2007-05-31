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

C This file contains the following routines related to the thermally
C activated cooling (TAC) model:

C TAC_1_static_template        Static template for TAC component, executed prior
C                              to time-step simulation.
C TAC_1_coeff_gen              Coefficient generator for TAC component, executed
C                              each time-step (more if iteration).
C TAC_coldtank_static_temp     Static template for cold water storage tank component,
C                              executed prior to time-step simulation.
C TAC_coldtank_coeff_gen       Coefficient generator for cold water storage tank
C                              component, executed each time-step (more if iteration).

C *******************************************************************************
C References:
C
C    Beausoleil-Morrison I. (2002), Design of Explicit Plant Component Models for
C    Simulating Thermally Activated Cooling, CETC internal report, 11 pages,
C    original draft November 1 2002, revision date February 7, 2003.
C
C    Hensen J.L.M. (1991), On the Thermal Interaction of Building Structure and
C    Heating and Ventilation System, PhD Thesis, Technical University of Eindhoven,
C    The Netherlands.
C *********************************************************************************************


C *********************************************************************************************
C ****************************** TAC_1_static_template ****************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 13, 2002.
C Copyright CETC 2002

C This subroutine is the static template created for the first phase of the thermally
C activated cooling (TAC) component. This was structured to support both the DY ammonia
C absorption technology and the ejector cycle. This subroutine checks whether the
C component is properly used in the user-defined plant network; it assigns user-input
C data to module variables, and it performs time-invariant calculations.

C The TAC component is represented with three nodes:
C   node 1 represents the water-side of the generator.
C   node 2 represents the water-side of the evaporator.
C   node 3 represents the air-side of the condenser.

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

C OUTPUTS:
C    TAC_Tgenr          Temperature of refrigerant at generator inlet (oC), in MODULE TAC_1.
C    TAC_gen_eff        Effectiveness of generator's heat exchanger (-), in MODULE TAC_1.
C    TAC_Pw_gen_inlet   Pressure of water in FC-cogen/generator loop (Pa), in MODULE TAC_1.
C    TAC_a,TAC_b?,TAC_c?,TAC_d?  Parametric coefficients for calculating COP, in MIDULE TAC_1.
C    TAC_Tgenref        Standard operating temp at generator water inlet (oC), in MODULE TAC_1.
C    TAC_Tcondref       Standard operating temp at condenser air inlet (oC), in MODULE TAC_1.
C    TAC_Tevapref       Standard operating temp at evap water inlet (oC), in MODULE TAC_1.
C    TAC_pump           Power of pump that circulates refrig within TAC (W), in MODULE TAC_1.
C    TAC_cond_mdot_air  Flow rate of air through condenser (kg/s), in MODULE TAC_1.
C    TAC_cond_qfan      Power of fan that blows air through cond (W), in MODULE TAC_1.
C    TAC_Triselocal     Temp rise of cond inlet air due to local (oC), in MODULE TAC_1.
C    TAC_gen_limit      Maximum allowable heat transfer to generator at std conditions
C                       (to limit cooling output), (W), in MODULE TAC_1.
C -------------------------------------------------------------------------------------------

      SUBROUTINE TAC_1_static_template(IPCOMP)
      IMPLICIT NONE
      INCLUDE 'plant.h'
      INCLUDE 'building.h'
      INCLUDE 'TAC.h'

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
      INTEGER NumADATA,J,N_expect,Itemp,IPCONC
      LOGICAL mistake  !CLOSE

      INTEGER IPCOMP
      INTEGER INUM_DG_CONTROLLER
      INTEGER I

C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. ITRACE(35)/=0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 3 node TAC '
        NumADATA = 21  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NumADATA)
        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(IPCOMP) holds the number of possible plant control variables
C as specified in the .pln file. The TAC component requires two control variables:
C one to indicate a demand for cooling and the second to indicate a demand for
C dumping excess heat.
C
C Modification: 
C 
C - Support added for external control of TAC system using distributed generation
C   controller: 
C     - IF the number of controlled variables is zero, check to see if DG
C       controller is present. 
C     - IF controller is found, set pointer iIP_DG_controller
C     - IF more than one controller is found, give warning message.
C
C       Modified by Alex Ferguson on April 16, 2003.
C---------------------------------------------------------------------------------
      N_expect = 2
      iNum_DG_controller = 0


C --------- Iterate through plant components
      DO i=1, NPCOMP
         IF (NPCDAT(i, 4) == 86) THEN
            iNum_DG_controller = iNum_DG_controller + 1 ! increment counter
            iIP_DG_controller = i ! set controller pointer to current i
         END IF     
      END DO
      IF(iNum_DG_controller==0.AND.(NCI(IPCOMP) /= N_expect)) THEN
         WRITE(IUOUT,*) ' FC_static_template: no control for ',
     &        'TAC system has been specified. System must be ',
     &        'controlled using either control loops or ',
     &        'external controller.'
         STOP ' FC_static_template: unresolvable error'              
      ELSEIF(iNum_DG_controller>1) THEN
         WRITE(IUOUT,*) ' FC_static_template: Currently, only one ',
     &        'instance of external controller is supported, but ',
     &        iNum_DG_controller, ' instances were found in plant ',
     &        'configuration file.'
         STOP ' FC_static_template: unresolvable error'  
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
C-----There should be one connection to node 1.
      IF( ICONDX(IPCOMP,1,1) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX(IPCOMP,1,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----There should be one connection to node 2.
      IF( ICONDX(IPCOMP,2,1) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX(IPCOMP,2,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----There should be no connections to node 3.
      DO IPCONC=1,MPCONC
        IF( ICONDX(IPCOMP,3,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' TAC_1_static_template: incorrect number of'
        WRITE(IUOUT,*) ' connection for component ',IPCOMP
        STOP ' TAC_1_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connections to nodes 1 and 2 are of the correct type. Both
C connections should be of type ISV=20 so that the TAC component can be used in
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
      IF( ICONTP( ICONDX(IPCOMP,1,1) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' TAC_1_static_template: incorrect conn type'
        WRITE(IUOUT,*) ' to node 1 for component ',IPCOMP
        STOP ' TAC_1_static_template: unresolvable error'
      END IF
      IF( ICONTP( ICONDX(IPCOMP,2,1) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' TAC_1_static_template: incorrect conn type'
        WRITE(IUOUT,*) ' to node 2 for component ',IPCOMP
        STOP ' TAC_1_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Assign user-specified inputs.  Refer to variable definitions in MODULE.
C---------------------------------------------------------------------------------
      TAC_Tgenr         = ADATA(IPCOMP,1)
      TAC_gen_eff       = ADATA(IPCOMP,2)
      TAC_Pw_gen_inlet  = ADATA(IPCOMP,3)
      TAC_a             = ADATA(IPCOMP,4)
      TAC_b1            = ADATA(IPCOMP,5)
      TAC_b2            = ADATA(IPCOMP,6)
      TAC_b3            = ADATA(IPCOMP,7)
      TAC_c1            = ADATA(IPCOMP,8)
      TAC_c2            = ADATA(IPCOMP,9)
      TAC_c3            = ADATA(IPCOMP,10)
      TAC_d1            = ADATA(IPCOMP,11)
      TAC_d2            = ADATA(IPCOMP,12)
      TAC_d3            = ADATA(IPCOMP,13)
      TAC_Tgenref       = ADATA(IPCOMP,14)
      TAC_Tcondref      = ADATA(IPCOMP,15)
      TAC_Tevapref      = ADATA(IPCOMP,16)
      TAC_pump          = ADATA(IPCOMP,17)
      TAC_cond_mdot_air = ADATA(IPCOMP,18)
      TAC_cond_qfan     = ADATA(IPCOMP,19)
      TAC_Triselocal    = ADATA(IPCOMP,20)
      TAC_gen_limit     = ADATA(IPCOMP,21)


C---------------------------------------------------------------------------------
C Completion of time-invariant operations for TAC component.
C---------------------------------------------------------------------------------
      RETURN
      END



C ********************************************************************************
C ****************************** TAC_1_coeff_gen *********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 14, 2002.
C Copyright CETC 2002

C This subroutine is the coefficient generator for the TAC component.
C It performs the time-step calculations and establishes coefficients for the
C sub-matrices that define the energy, 1st phase mass flow, and 2nd phase mass
C flow balances on the TAC's nodes. A fully explicit form of the energy
C balances is formed since the thermal transients of the TAC are ignored.

C The TAC is represented with three nodes:
C The TAC component is represented with three nodes:
C   node 1 represents the water-side of the generator.
C   node 2 represents the water-side of the evaporator.
C   node 3 represents the air-side of the condenser.

C INPUTS:
C    TAC_Tgenr          Temperature of refrigerant at generator inlet (oC), in MODULE TAC_1.
C    TAC_gen_eff        Effectiveness of generator's heat exchanger (-), in MODULE TAC_1.
C    TAC_Pw_gen_inlet   Pressure of water in FC-cogen/generator loop (Pa), in MODULE TAC_1.
C    TAC_a,TAC_b?,TAC_c?,TAC_d?  Parametric coefficients for calculating COP, in MIDULE TAC_1.
C    TAC_Tgenref        Standard operating temp at generator water inlet (oC), in MODULE TAC_1.
C    TAC_Tcondref       Standard operating temp at condenser air inlet (oC), in MODULE TAC_1.
C    TAC_Tevapref       Standard operating temp at evap water inlet (oC), in MODULE TAC_1.
C    TAC_pump           Power of pump that circulates refrig within TAC (W), in MODULE TAC_1.
C    TAC_cond_mdot_air  Flow rate of air through condenser (kg/s), in MODULE TAC_1.
C    TAC_cond_qfan      Power of fan that blows air through cond (W), in MODULE TAC_1.
C    TAC_Triselocal     Temp rise of cond inlet air due to local (oC), in MODULE TAC_1.
C    TAC_gen_limit      Maximum allowable heat transfer to generator at std conditions
C                       (to limit cooling output), (W), in MODULE TAC_1.
C    TF                 Future value of outdoor temperature in oC (in common/CLIMI).

C OUTPUTS:
C    COUT(i)            coefficient for ISTATS balance for `i'th matrix position
C    NAPDAT(i)          number of "additional" output for component `i'
C    PCAOUT(i,j)        additional output `j' for component `i'
C    TC(i)               time constant (seconds) of plant component `i'
C -------------------------------------------------------------------------------------------

      SUBROUTINE TAC_1_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
      INCLUDE 'plant.h'
      INCLUDE 'building.h'
      INCLUDE 'SOFC.h'
      INCLUDE 'TAC.h'

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     & ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      INTEGER MAXITP
      REAL PERREL,PERTMP,PERFLX,PERMFL
      INTEGER itrclp,ICSV
      REAL CSVI

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),IPC2(MPCON),
     & IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      LOGICAL COOL     ! Indicates that there is a demand for cooling.
      REAL rCOOL       ! Real indicating that there is a demand for cooling.
      LOGICAL HEATDUMP ! Indicates that heat must be dumped from tank.
      REAL rHEATDUMP   ! Real indicating that there is a need to dump heat.
      REAL COUT(MPCOE) ! Plant matrix coefficients (passed in calling statement).
      INTEGER node1,node2,node3,con1,con2 ! Node and connection indices.
      REAL mdotCp_con1,mdotCp_con2        ! Heat capacity flow rates at connections.
      REAL mdot_con1                      ! Mass flow rate at connection to node 1.
      REAL Cp_con1                        ! Heat capacity at connection to node 1.
      REAL T_con1,T_con2                  ! Temperatures at connections.
      REAL T_node1,T_node2,T_node3        ! Nodal temps.
      REAL TAC_Tdelta_gen,TAC_Tdelta_evap,TAC_Tdelta_cond ! Deviations from std conditions.
      REAL T_gen_beforemix  ! Temp of water exiting gen when bypass valve modulates.
      REAL mdot_bypass  ! Flow rate of water diverted from gen when bypass valve modulates.
      REAL bypass_ratio  ! Ratio of flow into node 1 that is diverted past generator.
      REAL q_generator,q_evaporator,q_condenser ! Heat transfer at heat exchangers.
      REAL T_cond_air_in,mdotCp_intonode3    ! Temp and mass flow rate of air into condenser.
      REAL TAC_COP                           ! COP of TAC unit.
      REAL TAC_pump_elec,TAC_cond_qfan_elec  ! Elec draw (W) of pump and fan current time-step.
      REAL CtoKelvinTemp !Temporary used for Kelvin to Celsius conversion.
      REAL H20LIQUIDCPCel ! function to obtain Cp of water.

      INTEGER IPCOMP
      INTEGER ISTATS
      REAL TAC_PCOLDTANK
      INTEGER I
      INTEGER ITEMP
      REAL SHTFLD



C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Entering subroutine TAC_1_coeff_gen'
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
C    node1          global matrix node number for TAC component node 1
C    node2          global matrix node number for TAC component node 2
C    node3          global matrix node number for TAC component node 3
C    con1           connection number to node 1 (water in generator's HX)
C    con2           connection number to node 2 (water in evaporator's HX)
C---------------------------------------------------------------------------------
      node1 = NPCDAT(IPCOMP,9)
      node2 = NPCDAT(IPCOMP,9)+1
      node3 = NPCDAT(IPCOMP,9)+2
      con1  = ICONDX(IPCOMP,1,1)
      con2  = ICONDX(IPCOMP,2,1)


C---------------------------------------------------------------------------------
C Mark the temperatures of the water exiting the generator and exiting the
C evaporator (ie. nodes 1 and 2) for iteration.
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
      ICSV(node2,1) = 1
      CSVI(node2,1) = CSVF(node2,1)


C---------------------------------------------------------------------------------
C The TAC has three possible modes of operation: cooling, heat rejection, or off.
C The TAC is switched on to cool when the cold water storage tank warms to above
C its "ON" set-point. When the cold water storage tank's temperature drops below
C its "OFF" set-point the TAC is switched off. And when the hot water storage
C tank's temperature rises above its safety limit (this indicates that the
C FC-cogen's thermal output is greater than DHW, space heat, and TAC cooling
C requirements), the TAC is switched into heat dump mode. Note that the heat dump
C mode takes precedence over the cooling mode of operation.
C Determine the mode of operation for this time-step.
C 
C Modification: 
C  - Check to see if an external controller was identified by the static template
C    If so, determine cooling and heat dump demands by reading its PCAOUT array.
C    Otherwise, collect the demands from the CDATA control array.
C   
C    Modified by Alex Ferguson on April 16th, 2003
C---------------------------------------------------------------------------------

      IF(iIP_DG_controller>0) THEN
C--------COOL=.TRUE. indicates that there is a demand for cooling.
         CALL ECLOSE(PCAOUT(iIP_DG_controller,2),1.,0.0001,COOL )
C--------HEATDUMP=.TRUE. indicates that heat must be dumped from the tank.
         CALL ECLOSE(PCAOUT(iIP_DG_controller,3),1.,0.0001,HEATDUMP )    
      ELSE
C--------COOL=.TRUE. indicates that there is a demand for cooling.
         CALL ECLOSE( CDATA(IPCOMP,1),1.,0.0001,COOL )
C--------HEATDUMP=.TRUE. indicates that heat must be dumped from the tank.
         CALL ECLOSE( CDATA(IPCOMP,2),1.,0.0001,HEATDUMP )
      ENDIF


C---------------------------------------------------------------------------------
C Ensure that the temperature of the water flowing into the generator is
C sufficient to transfer energy into the refrigerant. If not, then the TAC
C can neither cool nor dump heat.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C---------------------------------------------------------------------------------
      T_con1 = CONVAR(con1,1)
      IF( T_con1 < TAC_Tgenr )THEN
        COOL     = .false.
        HEATDUMP = .false.
      ENDIF


C---------------------------------------------------------------------------------
C Generate coefficients for the appropriate equation set. This subroutine was
C called to either generate coefficients for energy balances (ISTATS=1), 1st
C phase mass balances (ISTATS=2), or 2nd phase mass balances (ISTATS=3).
C Start with the energy balances.
C---------------------------------------------------------------------------------

C*********************************************************************************
C Beginning of energy balance section when TAC is providing cooling.
C*********************************************************************************
      IF(ISTATS==1 .and. COOL .and. .not.HEATDUMP) THEN
C-------Flags used to report operation status in `additional output' section.
        rCOOL     = 1.  ! Cold tank requires cooling.
        rHEATDUMP = 0.  ! No need to dump heat from hot tank.


C---------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the connections
c to nodes 1 and 2. These are the heat capacity rates (W/K) of: 1) the water
C entering the generator from the fuel cell's heat exchanger; and 2) the water
C entering the evaporator from the cold water tank. Also determine the mass flow
C for the connection to node 1 and the heat capacity of the water at node 1.
C Variables used:
C    CONVAR(i,1)          Temperature (oC) of sending node for connection `i'
C    CONVAR(i,2)          Water mass flow rate (kg/s) at sending node for
C                         connection `i'
C    PCONDR(i)            Ratio of flow rate leaving sending node `i' that
C                         reaches receiving node.
C    H20LIQUIDCPCel(T, P) Function that returns heat capacity (J/kgK) of liquid
C                         water at temperature `T' (oC) and pressure 'P' (Pa).
C                         This function consumes inordinate cpu resources but
C                         is required here because it can calculate the heat
C                         capacity of pressurized water.  Another function,
C                         'SHTFLD', is much more efficient and should be used
C                         in cases where the liquid water is not pressurized.
C    TAC_Pw_gen_inlet     Pressure of water in FC-cogen/generator loop (Pa).
C---------------------------------------------------------------------------------
        mdot_con1   = PCONDR(con1) * CONVAR(con1,2)
        Cp_con1     = H20LIQUIDCPCEL( CONVAR(con1,1),TAC_Pw_gen_inlet )
        mdotCp_con1 = mdot_con1*Cp_con1
        TAC_Pcoldtank = 101325.  ! Pa, cold tank is not pressurized.
        mdotCp_con2 = PCONDR(con2) * CONVAR(con2,2)
     &              * H20LIQUIDCPCEL( CONVAR(con2,1),TAC_Pcoldtank ) ! Phylroy to fix.


C---------------------------------------------------------------------------------
C Establish the temperatures of the water flowing into the generator (node 1) and
C the evaporator (node 2).
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C---------------------------------------------------------------------------------
        T_con1 = CONVAR(con1,1)
        T_con2 = CONVAR(con2,1)


C---------------------------------------------------------------------------------
C Establish the temperature of the air flowing into the condenser (node 3).
C Refer to Beausoleil-Morrison (2002) eq 17. This is not represented as a
C `connection' within the plant network. Rather, it is represented within this
C component and thus treated as a known quantity (ie. right hand term).
C Variables used:
C    TF  Future value of outdoor temperature in oC.
C---------------------------------------------------------------------------------
        T_cond_air_in = TF + TAC_Triselocal


C---------------------------------------------------------------------------------
C Determine the product of mass flow and heat capacity (W/K) for the air flowing
c into the condenser (node 3).
C Variables used:
C    SHTFLD(1,T) function that returns heat capacity (J/kgK) of air (index 1)
C                at temperature `T'.
C---------------------------------------------------------------------------------
        mdotCp_intonode3 = TAC_cond_mdot_air
     &                   * SHTFLD( 1,T_cond_air_in )


C---------------------------------------------------------------------------------
C Establish the temperatures of the three nodes by taking their state at the
C previous iteration (this may be the previous time-step, depending on whether
C iteration is required to converge the plant matrix). These values are not used
C to form the energy balances below. Rather, they are used to calculate
C `additional output' quantities, such as the heat transfer rates at the
C generator, evaporator, and condenser.
C Variables used:
C     CSVF(i,j)  future time-row solution variable for node `i' and variable `j';
C                j=1 for temperature
C---------------------------------------------------------------------------------
        T_node1 = CSVF(node1,1)
        T_node2 = CSVF(node2,1)
        T_node3 = CSVF(node3,1)


C---------------------------------------------------------------------------------
C Calculate the energy transfer at the generator (from FC-cogen to TAC) if the
C bypass valve upstream of the generator does not modulate (ie. the full flow
C rate enters the generator's heat exchanger). Refer to
C Beausoleil-Morrison (2002) eq 1. Note that this is based upon the nodal
C temperature solution from the previous iteration (perhaps the previous
C time-step).
C---------------------------------------------------------------------------------
        q_generator = mdotCp_con1 * ( T_con1 - T_node1 )


C---------------------------------------------------------------------------------
C Calculate the energy transfer at the evaporator (from cold water tank to TAC).
C Refer to Beausoleil-Morrison (2002) eq 11. Note that this is based upon the nodal
C temperature solution from the previous iteration (time-step).
C---------------------------------------------------------------------------------
        q_evaporator = mdotCp_con2 * ( T_con2 - T_node2 )


C---------------------------------------------------------------------------------
C Calculate the energy transfer at the condenser (from TAC refrigerant to outdoor
C environment). Refer to Beausoleil-Morrison (2002) eq 15. Note that this is based
C upon the nodal temperature solution from the previous iteration (time-step).
C---------------------------------------------------------------------------------
        q_condenser = q_generator + q_evaporator + TAC_pump


C---------------------------------------------------------------------------------
C Calculate the deviations of the generator, evaporator, and condenser
C temperatures from standard operating conditions. Then calculate TAC's COP.
C Refer to Beausoleil-Morrison 2002 eq 13.
C---------------------------------------------------------------------------------
C-------Temperature deviations from standard conditions.
        TAC_Tdelta_gen  = T_con1 - TAC_Tgenref             ! Generator.
        TAC_Tdelta_evap = T_con2 - TAC_Tevapref            ! Evaporator.
        TAC_Tdelta_cond = T_cond_air_in - TAC_Tdelta_cond  ! Condenser.
C-------COP.
        TAC_COP = TAC_a
     &          + TAC_b1 * TAC_Tdelta_gen
     &          + TAC_b2 * TAC_Tdelta_cond
     &          + TAC_b3 * TAC_Tdelta_evap
     &          + TAC_c1 * TAC_Tdelta_gen**2.
     &          + TAC_c2 * TAC_Tdelta_cond**2.
     &          + TAC_c3 * TAC_Tdelta_evap**2.
     &          + TAC_d1 * TAC_Tdelta_gen  * TAC_Tdelta_cond
     &          + TAC_d2 * TAC_Tdelta_gen  * TAC_Tdelta_evap
     &          + TAC_d3 * TAC_Tdelta_cond * TAC_Tdelta_evap


C---------------------------------------------------------------------------------
C Electrical draws of TAC refrigerant pump and condenser fan (for reporting
C purposes).
C---------------------------------------------------------------------------------
        TAC_pump_elec      = TAC_pump
        TAC_cond_qfan_elec = TAC_cond_qfan


C---------------------------------------------------------------------------------
C The flow rate and temperature of the water entering node 1 may result in a heat
C transfer rate to the TAC's generator that exceeds the device's limit
C (`TAC_gen_limit'). If this is the case, the bypass valve upstream of the
C generator will be modulated to ensure this limit is respected (this is the
C technique employed to restrict the TAC's cooling output). Perform a check
C here to determine whether the bypass valve with have to divert some of the
C flow around the generator.
C---------------------------------------------------------------------------------
        IF (q_generator > TAC_gen_limit) THEN
C---------------------------------------------------------------------------------
C Bypass valve must modulate flow to generator. Determine temperature of water
C exiting the generator, but upstream of the mixing valve. This is not the
C temperature of node 1, but rather the temperature of one of the streams that is 
C mixed at the mixing valve to determine the temperature of node 1. Refer to
C Beausoleil-Morrison (2002) eq 7.
C---------------------------------------------------------------------------------
          T_gen_beforemix = TAC_gen_eff * TAC_Tgenr
     &                    + (1.-TAC_gen_eff) * T_con1

C---------------------------------------------------------------------------------
C Determine position of bypass valve, specifically the flow rate of the water
C diverted from the generator. Refer to Beausoleil-Morrison (2002) eq 8.
C Variables used:
C    CONVAR(i,1)          Temperature (oC) of sending node for connection `i'
C    CONVAR(i,2)          Water mass flow rate (kg/s) at sending node for
C                         connection `i'
C    PCONDR(i)            Ratio of flow rate leaving sending node `i' that
C                         reaches receiving node.
C    H20LIQUIDCPCel(T, P) Function that returns heat capacity (J/kgK) of liquid
C                         water at temperature `T' (oC) and pressure 'P' (Pa).
C                         This function consumes inordinate cpu resources but
C                         is required here because it can calculate the heat
C                         capacity of pressurized water.  Another function,
C                         'SHTFLD', is much more efficient and should be used
C                         in cases where the liquid water is not pressurized.
C    TAC_Pw_gen_inlet     Pressure of water in FC-cogen/generator loop (Pa).
C---------------------------------------------------------------------------------
          mdot_bypass = mdot_con1
     &                - TAC_gen_limit /
     &                (
     &                   Cp_con1 * ( T_con1-T_gen_beforemix )
     &                )
          bypass_ratio = mdot_bypass / mdot_con1

        ELSE
C---------------------------------------------------------------------------------
C Bypass valve closes, allowing full flow to generator.
C---------------------------------------------------------------------------------
          bypass_ratio = 0.

        END IF


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.
C Node coefficient transport for COUT (refer to Beausoleil-Morrison 2002 eq 19):
C       <--self-->|<cross>
C  node   1  2  3 |  m  n      RHS
C       ---------------------------
C         1  0  0 |  7  0       9
C         2  3  0 |  0  8   =  10
C         4  5  6 |  0  0      11
C---------------------------------------------------------------------------------
C---------Node 1 energy balance given by Beausoleil-Morrison (2002) eq 4.
          COUT(1) = 1.                                           ! Self-coupling to itself (-).
          COUT(7) = ( 1.-bypass_ratio ) * (TAC_gen_eff - 1.)     ! Cross-coupling (-).
     &            - bypass_ratio
          COUT(9) = ( 1.-bypass_ratio ) * TAC_gen_eff * TAC_Tgenr  ! RHS (oC).
C---------Node 2 energy balance given by Beausoleil-Morrison (2002) eq 14.
          COUT(3)  = 1.                           ! Self-coupling to itself (-).
          COUT(2)  = -1. * TAC_COP * mdotCp_con1  ! Self-coupling to T1 (-).
     &             / mdotCp_con2
          COUT(8)  =  -1.                         ! Cross-coupling (-).
          COUT(10) = -1. * TAC_COP * T_con1       ! RHS (oC).
     &             * mdotCp_con1 / mdotCp_con2
C---------Node 3 energy balance given by Beausoleil-Morrison (2002) eq 18.
          COUT(6)  = 1.                              ! Self-coupling to itself (-).
          COUT(4)  = mdotCp_con1 / mdotCp_intonode3  ! Self-coupling to T1 (-).
          COUT(5)  = mdotCp_con2 / mdotCp_intonode3  ! Self-coupling to T2 (-).
          COUT(11) = ( TAC_pump + TAC_cond_qfan ) / mdotCp_intonode3  ! RHS (oC).
     &             + T_cond_air_in
     &             + mdotCp_con1 / mdotCp_intonode3 * T_con1
     &             + mdotCp_con2 / mdotCp_intonode3 * T_con2



C*********************************************************************************
C Beginning of energy balance section when TAC is in heat dump mode.
C*********************************************************************************
      ELSEIF(ISTATS==1 .and. HEATDUMP) THEN
C-------Flags used to report operation status in `additional output' section.
        IF (COOL) THEN
          rCOOL = 1.    ! Cold tank requires cooling, but TAC in heat dump mode.
        ELSE
          rCOOL = 0.    ! Cold tank does not require cooling anyway.
        ENDIF
        rHEATDUMP = 1.  ! Hot tank needs to dump heat.


C---------------------------------------------------------------------------------
C Determine the product of mass flow and heat capacity (W/K) for the connection
c to node 1. This is the heat capacity rate (W/K) of the water entering the
C generator from the fuel cell's heat exchanger. Note that there is no flow to
C the evaporator (node 2) when in heat dump mode.
C Variables used:
C    CONVAR(i,1)          Temperature (oC) of sending node for connection `i'
C    CONVAR(i,2)          Water mass flow rate (kg/s) at sending node for
C                         connection `i'
C    PCONDR(i)            Ratio of flow rate leaving sending node `i' that
C                         reaches receiving node.
C    H20LIQUIDCPCel(T, P) Function that returns heat capacity (J/kgK) of liquid
C                         water at temperature `T' (oC) and pressure 'P' (Pa).
C                         This function consumes inordinate cpu resources but
C                         is required here because it can calculate the heat
C                         capacity of pressurized water.  Another function,
C                         'SHTFLD', is much more efficient and should be used
C                         in cases where the liquid water is not pressurized.
C    TAC_Pw_gen_inlet     Pressure of water in FC-cogen/generator loop (Pa).
C---------------------------------------------------------------------------------
        mdotCp_con1 = PCONDR(con1) * CONVAR(con1,2)
     &              * H20LIQUIDCPCel( CONVAR(con1,1),TAC_Pw_gen_inlet )


C---------------------------------------------------------------------------------
C Establish the temperature of the water flowing into the generator (node 1).
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C---------------------------------------------------------------------------------
        T_con1 = CONVAR(con1,1)


C---------------------------------------------------------------------------------
C Establish the temperature of the air flowing into the condenser (node 3).
C Refer to Beausoleil-Morrison (2002) eq 17. This is not represented as a
C `connection' within the plant network. Rather, it is represented within this
C component and thus treated as a known quantity (ie. right hand term).
C Variables used:
C    TF  Future value of outdoor temperature in oC.
C---------------------------------------------------------------------------------
        T_cond_air_in = TF + TAC_Triselocal


C---------------------------------------------------------------------------------
C Determine the product of mass flow and heat capacity (W/K) for the air flowing
c into the condenser (node 3).
C Variables used:
C    SHTFLD(1,T) function that returns heat capacity (J/kgK) of air (index 1)
C                at temperature `T'.
C---------------------------------------------------------------------------------
        mdotCp_intonode3 = TAC_cond_mdot_air
     &                   * SHTFLD( 1,T_cond_air_in )


C---------------------------------------------------------------------------------
C Establish the temperatures of nodes 1 and 3 (node 2 not required when in heat
C dump mode) by taking their state at the previous iteration (this may be the
C previous time-step, depending on whether iteration is required to converge the
C plant matrix). These values are not used to form the energy balances below.
C Rather, they are used to calculate `additional output' quantities, such as the
C heat transfer rates at the generator, evaporator, and condenser.
C Variables used:
C     CSVF(i,j)  future time-row solution variable for node `i' and variable `j';
C                j=1 for temperature
C---------------------------------------------------------------------------------
        T_node1 = CSVF(node1,1)
        T_node3 = CSVF(node3,1)


C---------------------------------------------------------------------------------
C Calculate the energy transfer at the generator (from FC-cogen to TAC). Refer
C to Beausoleil-Morrison (2002) eq 1. Note that this is based upon the nodal
C temperature solution from the previous iteration (time-step).
C---------------------------------------------------------------------------------
        q_generator = mdotCp_con1 * ( T_con1 - T_node1 )


C---------------------------------------------------------------------------------
C There is no energy transfer at the evaporator since there is no water flow
C from the cold tank to the evaporator when in heat dump mode.
C---------------------------------------------------------------------------------
        q_evaporator = 0.


C---------------------------------------------------------------------------------
C Set the COP to zero for reporting purposes.
C---------------------------------------------------------------------------------
        TAC_COP = 0.


C---------------------------------------------------------------------------------
C Calculate the energy transfer at the condenser (from TAC refrigerant to outdoor
C environment). This represents the heat dump to the outdoor environment. Refer to
C Beausoleil-Morrison (2002) eq 15. Note that this is based upon the nodal
C temperature solution from the previous iteration (time-step).
C---------------------------------------------------------------------------------
        q_condenser = q_generator + TAC_pump


C---------------------------------------------------------------------------------
C Electrical draws of TAC refrigerant pump and condenser fan (for reporting
C purposes).
C---------------------------------------------------------------------------------
        TAC_pump_elec      = TAC_pump
        TAC_cond_qfan_elec = TAC_cond_qfan


C---------------------------------------------------------------------------------
C Establish matrix equation. Refer to "cooling" section for ordering of
C coefficients.
C---------------------------------------------------------------------------------
C-------Node 1 (same energy balance as when TAC is cooling).
        COUT(1) = 1.                       ! Self-coupling to itself (-).
        COUT(7) = TAC_gen_eff - 1.         ! Cross-coupling (-).
        COUT(9) = TAC_gen_eff * TAC_Tgenr  ! RHS (oC).
C-------Node 2 (made equal to temperature of water flowing into evaporator).
        COUT(3)  = 1.    ! Self-coupling to itself (-).
        COUT(2)  = 0.    ! Self-coupling to T1 (-).
        COUT(8)  = -1.   ! Cross-coupling (-).
        COUT(10) = 0.    ! RHS (oC).
C-------Node 3 (same energy balance as when TAC cooling, but qevap term elinated).
        COUT(6)  = 1.                              ! Self-coupling to itself (-).
        COUT(4)  = mdotCp_con1 / mdotCp_intonode3  ! Self-coupling to T1 (-).
        COUT(5)  = 0.                              ! No coupling to T2 (-).
        COUT(11) = ( TAC_pump + TAC_cond_qfan ) / mdotCp_intonode3  ! RHS (oC).
     &           + T_cond_air_in
     &           + mdotCp_con1 / mdotCp_intonode3 * T_con1


C*********************************************************************************
C Beginning of energy balance section when TAC is inoperative.
C*********************************************************************************
      ELSEIF(ISTATS==1 .and. .not.COOL .and. .not.HEATDUMP) THEN
C-------Flags used to report operation status in `additional output' section.
        rCOOL     = 0.  ! Cold tank does not require cooling.
        rHEATDUMP = 0.  ! No need to dump heat from hot tank.

C---------------------------------------------------------------------------------
C There is no heat transfer at the generator, evaporator, or condenser.
C---------------------------------------------------------------------------------
        q_generator  = 0.
        q_evaporator = 0.
        q_condenser  = 0.


C---------------------------------------------------------------------------------
C Set the COP to zero for reporting purposes.
C---------------------------------------------------------------------------------
        TAC_COP = 0.


C---------------------------------------------------------------------------------
C The TAC refrigerant pump and condenser fan do not draw any power (for reporting
C purposes).
C---------------------------------------------------------------------------------
        TAC_pump_elec      = 0.
        TAC_cond_qfan_elec = 0.

C---------------------------------------------------------------------------------
C Make the temperature of the air inlet to the conderser equal -99 for reporting
C purposes.
C---------------------------------------------------------------------------------
        T_cond_air_in = -99.


C---------------------------------------------------------------------------------
C Establish matrix equation. Refer to "cooling" section for ordering of
C coefficients.
C---------------------------------------------------------------------------------
C-------Node 1 (made equal to temperature of water flowing into generator).
        COUT(1) = 1.     ! Self-coupling to itself (-).
        COUT(7) = -1.    ! No cross-coupling (-).
        COUT(9) = 0.     ! RHS (oC).
C-------Node 2 (made equal to temperature of water flowing into evaporator).
        COUT(3)  = 1.    ! Self-coupling to itself (-).
        COUT(2)  = 0.    ! Self-coupling to T1 (-).
        COUT(8)  = -1.   ! Cross-coupling (-).
        COUT(10) = 0.    ! RHS (oC).
C-------Node 3 (made equal to -99oC).
        COUT(6)  = 1.    ! Self-coupling to itself (-).
        COUT(4)  = 0.    ! No coupling to T1 (-).
        COUT(5)  = 0.    ! No coupling to T2 (-).
        COUT(11) = -99.  ! RHS (oC).


C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      ELSEIF(ISTATS==2) THEN
C-------Node 1: flow in equals flow out.
        COUT(1)  = 1.
        COUT(7)  = - PCONDR(con1) ! (dimensionless)
        COUT(9)  = 0.
C-------Node 2: flow in equals flow out.
        COUT(2)  = 0.
        COUT(3)  = 1.
        COUT(8)  = - PCONDR(con2) ! (dimensionless)
        COUT(10) = 0.
C-------Node 3: no balance required so make flow zero.
        COUT(4)  = 0.
        COUT(5)  = 0.
        COUT(6)  = 1.
        COUT(11) = 0.


C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      ELSEIF(ISTATS==3) THEN
C-------Node 1: no balance required so make flow zero.
        COUT(1)  = 1.
        COUT(7)  = 0.
        COUT(9)  = 0.
C-------Node 2: no balance required so make flow zero.
        COUT(2)  = 0.
        COUT(3)  = 1.
        COUT(8)  = 0.
        COUT(10) = 0.
C-------Node 3: no balance required so make flow zero.
        COUT(4)  = 0.
        COUT(5)  = 0.
        COUT(6)  = 1.
        COUT(11) = 0.


      ENDIF
C*********************************************************************************
C End of energy and mass balances ************************************************
C*********************************************************************************

C Record the electrical draw (W) of the refrigerant pump and condenser fan for
C use by the fuel cell controller.
      FCctl_TAC_draw = TAC_pump_elec + TAC_cond_qfan_elec


C---------------------------------------------------------------------------------
C Save the `additional' output variables for outputting on time-step basis.
C---------------------------------------------------------------------------------
      NAPDAT(IPCOMP)    = 9                   ! Number of additional outputs.
      PCAOUT(IPCOMP,1)  = q_generator         ! Heat transfer at generator (W).
      PCAOUT(IPCOMP,2)  = q_evaporator        ! Heat transfer at evaporator (W).
      PCAOUT(IPCOMP,3)  = TAC_COP             ! COP (-).
      PCAOUT(IPCOMP,4)  = q_condenser         ! Heat transfer at condenser (W).
      PCAOUT(IPCOMP,5)  = T_cond_air_in       ! Temp of air at inlet to condenser (oC).
      PCAOUT(IPCOMP,6)  = TAC_pump_elec       ! Pump power (W).
      PCAOUT(IPCOMP,7)  = TAC_cond_qfan_elec  ! Condenser fan (W).
      PCAOUT(IPCOMP,8)  = rCOOL               ! Demand for cooling (1=yes; 0=no).
      PCAOUT(IPCOMP,9)  = rHEATDUMP           ! Heat dump required (1=yes; 0=no).


C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 3 node TAC '
        WRITE(ITU,*) ' Matrix node(s) ',node1,node2,node3
        WRITE(ITU,*) ' Connection(s)  ',con1,con2
        IF(ISTATS==1) THEN
           WRITE(ITU,*) ' CDATA(1)      = ',CDATA(IPCOMP,1),' (-)'
           WRITE(ITU,*) ' CDATA(2)      = ',CDATA(IPCOMP,2),' (-)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(I),I=1,11)
        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving subroutine TAC_1_coeff_gen'
      END IF


      RETURN
      END


C---------------------------------------------------------------------------------
C The following are just place holders to allow compilation and linking
C during development.  They will be replaced by the full subroutine that Maria
C is writing.
C---------------------------------------------------------------------------------

C *********************************************************************************************
C ****************************** TAC_coldtank_static_temp *************************************
C Created by: Maria Mottillo
C Initial Creation Date: November 15, 2002.
C Copyright CETC 2002

C This subroutine is the static template created for the cold water storage tank
C in the HVAC system configuration for the TAC. The TAC is activated when the 
C temperature in the cold water tank drops below the setpoint. Water from the
C tank is circulated over the TAC's evaporator. This subroutine checks whether the
C component is properly used in the user-defined plant network; it assigns user-input
C data to module variables, and it performs time-invariant calculations.

C The cold water storage tank is represented with one node.
C
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
C
C OUTPUTS:
C    TAC_coldtank_Mass     Mass of water stored in cold tank (kg), in MODULE TAC_coldtank.
C    TAC_coldtank_UA       Tank heat gain coefficient (W/K), in MODULE TAC_coldtank.
C    TAC_coldtank_Cp       Specific heat of water in tank (J/kgK), in MODULE TAC_coltank.
C ------------------------------------------------------------------------------------------


      SUBROUTINE TAC_coldtank_static_temp(IPCOMP)
      IMPLICIT NONE
      INCLUDE 'plant.h'
      INCLUDE 'building.h'
      INCLUDE 'TAC.h'

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
C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA,J,N_expect,Itemp,IPCONC
      LOGICAL mistake  !CLOSE

      INTEGER IPCOMP

C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. ITRACE(35)/=0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 1 node cold water storage tank '
        NumADATA = 3  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NumADATA)
        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(IPCOMP) holds the number of possible plant control variables
C as specified in the .pln file. The cold water storage tank does not require
C any control variables.
C---------------------------------------------------------------------------------
      N_expect = 0
      IF(NCI(IPCOMP) /= N_expect) THEN
        WRITE(ITU,*) ' TAC_coldtank_static_template warning: incorrect',
     &               ' number of controlled variables specified.'
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
C-----There should be 2 connections to node 1.
      IF( ICONDX(IPCOMP,1,1) == 0 ) mistake=.true.
      IF( ICONDX(IPCOMP,1,2) == 0 ) mistake=.true.
      DO IPCONC=3,MPCONC
        IF( ICONDX(IPCOMP,1,IPCONC) /= 0 ) mistake=.true.
      END DO
C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' TAC_coldtank_static_template: incorrect '
        WRITE(IUOUT,*) ' number of connections for component ',IPCOMP
        STOP ' TAC_coldtank_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the 2 connections to the node are of the correct type. Both
C connections should be of type ISV=20 so that the cold tank component can be used in
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
      IF( ICONTP( ICONDX(IPCOMP,1,1) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' TAC_coldtank_static_template: incorrect conn'
        WRITE(IUOUT,*) ' type to node 1 for component ',IPCOMP
        STOP ' TAC_coldtank_static_template: unresolvable error'
      END IF
      IF( ICONTP( ICONDX(IPCOMP,1,2) ) /= 20 ) mistake=.true.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' TAC_coldtank_static_template: incorrect conn'
        WRITE(IUOUT,*) ' type to node 1 for component ',IPCOMP
        STOP ' TAC_coldtank_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Assign user-specified inputs.  Refer to variable definitions in MODULE.
C---------------------------------------------------------------------------------
      TAC_coldtank_Mass         = ADATA(IPCOMP,1)
      TAC_coldtank_UA           = ADATA(IPCOMP,2)
      TAC_coldtank_Cp           = ADATA(IPCOMP,3)

C---------------------------------------------------------------------------------
C Completion of time-invariant operations for TAC cold water storage tank.
C---------------------------------------------------------------------------------

      RETURN
      END



C *********************************************************************************************
C ******************************** TAC_coldtank_coeff_gen *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: November 20, 2002
C Copyright CETC 2002

C This subroutine is the coefficient generator for the cold water tank component. It was
C developed for for the thermally activated cooling (TAC) project. This cold tank will be
C connected to the TAC's evaporator node and to a fan-coil for space cooling. Therefore,
C the tank has two connections. A single node is used to represent the tank.

C This subroutine performs the time-step calculations and establishes coefficients for the
C sub-matrices that define the energy, 1st phase mass flow, and 2nd phase mass
C flow balances on the storage tank's node. A weighted implicit-explicit energy balance
C is formed for the single node.

C The tank, including the electric resistance heating element, is represented with
C a single node.

C INPUTS:
C    TAC_coldtank_Mass         mass of node 1, kg (in MODULE TAC_coldtank)
C    TAC_coldtank_Cp           specific heat of node 1, J/kgK (in MODULE TAC_coldtank)
C    TAC_coldtank_UA           heat loss coefficient between node 1 and surroundings,
C                              W/K (in MODULE TAC_coldtank)
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

      SUBROUTINE TAC_coldtank_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
      INCLUDE 'plant.h'
      INCLUDE 'building.h'
      INCLUDE 'TAC.h'

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA


      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     & ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      INTEGER MAXITP
      REAL PERREL,PERTMP,PERFLX,PERMFL
      INTEGER itrclp
      INTEGER ICSV
      REAL CSVI

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),IPC2(MPCON),
     & IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat


      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     & PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     & PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP

      COMMON/PCTC/TC(MPCOM)
      REAL TC

      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP
      REAL RATIMP

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      LOGICAL CLOSE                           ! Used to check that tank located in a room.
      INTEGER node1,con1a,con1b               ! Node and connection indices.
      REAL COUT(MPCOE)                        ! Plant matrix coeffs (passed in call).
      REAL Troom_present,Troom_future         ! Temperature of room containing tank.
      REAL tank_UA_future,tank_UA_present     ! Heat transfer coeff between room and tank.
      REAL T_node1_present,T_node1_future     ! Temperature of tank.
      REAL mdotCp_1a_future,mdotCp_1b_future, ! Mass flow * heat capacity at connections.
     &     mdotCp_1a_present,mdotCp_1b_present
      REAL T_1a_present,T_1b_present          ! Temperatures at connections.
      REAL Rinv,alpha                         ! Implicit/explicit weighting.
      REAL part1,part2,part3,part5            ! Used for intermediate COUT calcs.

      INTEGER IPCOMP
      INTEGER ISTATS
      INTEGER I
      INTEGER ITEMP
      REAL SHTFLD


C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Entering sub TAC_coldtank_coeff_gen'
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
      IF(ISTATS==1) THEN


C---------------------------------------------------------------------------------
C Determine the `present' and `future' temperature of the room containing the
C water tank. This is used to calculate the heat lost from the hot tank to the
C room. Refer to Beausoleil-Morrison 2002 eq 21.
C Variables used:
C    PCNTMP(i)  present time-row temperature of room containing component `i'.
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C---------------------------------------------------------------------------------
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' TAC_coldtank_coeff_gen: '
          WRITE(IUOUT,*) ' the cold tank must be contained within '
          WRITE(IUOUT,*) ' a room.'
          STOP ' TAC_coldtank_coeff_gen: unresolvable error'
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
        tank_UA_future  = TAC_coldtank_UA
C-------Save future value.
        PUAF(node1)     = tank_UA_future
C-------Set present value.
        tank_UA_present = PUAP(node1)


C---------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the two
c connections to node 1, for the present and future time-rows. These are the heat
C capacity rates (W/K) of the the water entering the cold tank from the TAC's
C evaporator and returning from the fan-coil return. PCRP and PCRF are standard
C ESP-r variables holding the present and future time-row values of the
C mass flow * heat capacity. PCRF is mapped to PCRP by subroutine MZNASS following
C the solution of the energy matrix for the current time-step.
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
         mdotCp_1a_future = PCONDR(con1a) * CONVAR(con1a,2)
     &                    * SHTFLD( 3,CONVAR(con1a,1) )
         mdotCp_1b_future = PCONDR(con1b) * CONVAR(con1b,2)
     &                    * SHTFLD( 3,CONVAR(con1b,1) )
C--------Save future values.
         PCRF(con1a) = mdotCp_1a_future
         PCRF(con1b) = mdotCp_1b_future
C--------Set present values.
         mdotCp_1a_present = PCRP(con1a)
         mdotCp_1b_present = PCRP(con1b)


C---------------------------------------------------------------------------------
C Establish the present time-row temperatures of the water flowing into the tank
C at the two connections. PCTP and PCTF are standard ESP-r variables holding the
C present and future time-row values of the temperature of the fluid flowing at
C the connection. PCTF is mapped to PCTP by subroutine MZNASS following the
C solution of the energy matrix for the current time-step.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C    PCTP(i)     present temperature of sending node for connection `i'
C    PCTF(i)     future temperature of sending node for connection `i'
C---------------------------------------------------------------------------------
         T_1a_present = PCTP(con1a)
         T_1b_present = PCTP(con1b)
C--------Save future values.
         PCTF(con1a) = CONVAR(con1a,1)
         PCTF(con1b) = CONVAR(con1b,1)


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
C Calculate component's time-constant for the current time-step. Refer to
C Hensen (1991) pages 5.9 and 5.10. The environmental losses from the tank and
C the water streams flowing into the tank are the three heat transfers that
C affect the transient storage. Therefore, treat these as parallel paths for
C the purposes of calculating the time constant, as described in eq 5.5 of
C Hensen (1991).
C Variables used:
C     TAC_coldtank_Mass    mass of node 1, kg
C     TAC_coldtank_Cp      specific heat of node 1, J/kgK
C     TC(i)        time constant (seconds) of plant component `i'
C---------------------------------------------------------------------------------
        Rinv = tank_UA_future + mdotCp_1a_future + mdotCp_1b_future
        CALL ECLOSE(Rinv,0.,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' TAC_coldtank_coeff_gen: '
          WRITE(IUOUT,*) ' impossible time constant. '
          STOP ' TAC_coldtank_coeff_gen: unresolvable error'
        END IF
        TC(IPCOMP) = TAC_coldtank_Mass*TAC_coldtank_Cp / Rinv


C---------------------------------------------------------------------------------
C Determine implicit/explicit weighting for the energy balance on node 1. `alpha'
C is the implicit weighting factor used to form the energy balance this time-step.
C It is the Greek "alpha" in Beausoleil-Morrison (2002) equation 24. The approach
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
        IF( IMPEXP==1 ) THEN
          alpha = 1.
C-------Implicit/explicit with user-specified (fixed) weighting.
        ELSE IF( IMPEXP == 2 ) THEN
          alpha = RATIMP
C-------General case: implicit/explicit with calculated weighting.
        ELSE IF( IMPEXP == 3 ) THEN
          IF( TIMSEC>0.63*TC(IPCOMP) ) THEN
            alpha = 1.
          ELSE
            alpha = RATIMP
          END IF
C-------Steady-state.
        ELSE IF( IMPEXP==4 ) THEN
          TAC_coldtank_Mass = 0.  ! make node 1 massless
          alpha = 1.
        END IF


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.
C Node coefficient transport for COUT (refer to Beausoleil-Morrison 2002 eq 25):
C       <-self->| <---cross-->
C  node   1     | evap fan-coil    RHS
C       -------------------------------
C         1     |   2    3          4
C---------------------------------------------------------------------------------
C-------Node 1 energy balance given by Beausoleil-Morrison (2002) eq 24.
        COUT(1) = TAC_coldtank_Mass * TAC_coldtank_Cp / TIMSEC ! self-coupling to itself (W/K)
     &          + alpha * mdotCp_1a_future
     &          + alpha * mdotCp_1b_future
     &          + alpha * tank_UA_future
        COUT(2) = -1. * alpha * mdotCp_1a_future               ! cross-coupling (W/K)
        COUT(3) = -1. * alpha * mdotCp_1b_future               ! cross-coupling (W/K)
        part1 = TAC_coldtank_Mass * TAC_coldtank_Cp / TIMSEC   ! (W/K)
     &        - (1.-alpha) * tank_UA_present
     &        - (1.-alpha) * mdotCp_1a_present
     &        - (1.-alpha) * mdotCp_1b_present
        part2 = (1.-alpha) * mdotCp_1a_present                 ! (W)
        part3 = (1.-alpha) * mdotCp_1b_present                 ! (W)
        part5 = (1.-alpha) * tank_UA_present * Troom_present
     &        + alpha * tank_UA_future * Troom_future
        COUT(4) = part1 * T_node1_present                      ! RHS (W)
     &          + part2 * T_1a_present
     &          + part3 * T_1b_present
     &          + part5


C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      ELSE IF(ISTATS==2) THEN
C-------Node 1: total flow in equals total flow out.
        COUT(1) = 1.
        COUT(2) = -1. * PCONDR(con1a) ! (dimensionless)
        COUT(3) = -1. * PCONDR(con1b)
        COUT(4) = 0.


C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      ELSE IF(ISTATS==3) THEN
C-------Node 1: no balance required so make flow zero.
        COUT(1) = 1.
        COUT(2) = 0.
        COUT(3) = 0.
        COUT(4) = 0.

      END IF

C*********************************************************************************
C End of energy and mass balances ************************************************
C*********************************************************************************


C---------------------------------------------------------------------------------
C Save the `additional' output variables for outputting on time-step basis.
C---------------------------------------------------------------------------------
      NAPDAT(IPCOMP)    = 1                     ! number of additional outputs
      PCAOUT(IPCOMP,1)  = tank_UA_future        ! Heat gain from room to tank (W).
     &                  * ( Troom_future - T_node1_future)


C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 1 node cold water tank for TAC'
        WRITE(ITU,*) ' Matrix node(s) ',node1
        WRITE(ITU,*) ' Connection(s)  ',con1a,con1b
        IF(ISTATS==1) THEN
           WRITE(ITU,*) ' alpha        = ',alpha,' (-)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(I),I=1,4)
        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving sub TAC_coldtank_coeff_gen'
      END IF


      RETURN
      END
