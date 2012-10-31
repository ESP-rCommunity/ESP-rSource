
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
C
C This file contains the following routines:
C H2_electrolyser_static_temp: static template for 3 node electrolyser
C                              component model
C H2_electrolyser:             coefficient generator for 3 node electrolyser
C                              component model



C************************ H2_electrolyser_static_temp *************************
C
C     Created by: Maria Mottillo
C     Created on: February 3, 2005
C     Copyright: CETC
C
C     This subroutine is the static template for the component model of
C     a 3 node electrolyser.The electrolyser is represented with 3 nodes.
C     The first node encompasses the electrolyser stack, the second
C     encompasses the cooling water heat exchanger and the third node
C     encompasses the heating element to increase the feed water to the
C     stack inlet temperature.
C
C     References:
C     NRCan, 2005. 'Design of an electrolyser component model', Internal Document,
C     Original draft January 27, 2005.
C
C******************************************************************************


          SUBROUTINE H2_electrolyser_static_temp( IPCOMP )
          IMPLICIT NONE

#include "plant.h"
#include "building.h"

C-----------------------------------------------------------------------------------
C Passed variables
C-----------------------------------------------------------------------------------

       INTEGER IPCOMP           !-plant component index


C------------------------------------------------------------------------------------
C ESP-r COMMONs
C------------------------------------------------------------------------------------

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
       INTEGER NPCOMP          !-number of plant components
       INTEGER NCI             !-number of possible control variables for component
       REAL    CDATA           !-array containing control data values

       COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &              ICONDX(MPCOM,MNODEC,MPCONC)
       REAL    CONVAR          !-component connection variable
       INTEGER ICONTP          !-state variable index for connection
       INTEGER ICONDX          !-pointer to connection of component/node

       COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
       INTEGER NPCDAT          !-miscellaneous plant component data
       INTEGER IPOFS1          !-row position of network matrix coefficient
       INTEGER IPOFS2          !-column position of network matrix coefficient

      common/c14ps/
     &     ndcon(mpcom,mnodec), ! pointers to node connections
     &     isv(mpcom,mnodec)    ! node state & supported simulation types
      integer ndcon, isv

C.....Common storing status of plant initialization.
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)

C Logical array indicating if plant components have support for 4th,
C hydrogen matrix
      common  / H2_lib /
     &     h2FlowSupport
      logical h2FlowSupport(mpcom)

C.....Flag to enable inter-domain iteration
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C--------------------------------------------------------------------------



C--------------------------------------------------------------------------
C Local Variables
C--------------------------------------------------------------------------

        INTEGER    iNumCtlVar    !-number of control variables for component model
        INTEGER    iNode1_state   !-2nd digit of ISV for node 1
        INTEGER    iNode1_SIMtype !-1st digit of ISV for node 1
        INTEGER    iConnection    !-connection index to node 2
        LOGICAL    bMistake       !-flag indicating that there are connection errors
        INTEGER    i              !-counter


C------------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C the plant network (.pln) file. NCI(IPCOMP) holds the number of possible
C plant control variables as specified in the .pln file.
C------------------------------------------------------------------------------------


        iNumCtlVar = 3

        if ( NCI(IPCOMP) .NE. iNumCtlVar ) then
           WRITE(IUOUT,*) 'H2_electrolyser__static_temp : incorrect',
     &                    'number of controlled variables specified.;'
        endif


C-----------------------------------------------------------------------------------
C Check the ISV variable for node 1 and set hydrogen flow support flag to true.
C The ISV_convert subroutine is used to break up the ISV value into two variables:
C
C SIMtype -> the first digit of ISV, describes the types of simulations the
C            node can be used in.
C
C state -> the second digit of ISV, describes the type of matter that the node
C          represents (this digit must be 2 for nodes permitting hydrogen flow
C
C-----------------------------------------------------------------------------------


         call ISV_convert( ISV(IPCOMP,1),    !-ISV variable for node 1
     &                     iNode1_state,
     &                     iNode1_SIMtype )

         if ( iNode1_state .EQ. 2 ) then
             h2FlowSupport(IPCOMP) = .true.
         else
             h2FlowSupport(IPCOMP) = .false.
         endif



C------------------------------------------------------------------------------------
C Check that each node for the plant component  has the correct number of connections
C to other plant components.
C Variables used:
C    MPCONC           the maximum allowable connections to a node (from plant.h).
C    ICONDX(i,j,k)    the connection number for the k'th connection to the j'th node
C                     of component i. It is used as a pointer.
C    bMistake         flag indicating whether there are connection errors.
C                     (true means there are errors)
C------------------------------------------------------------------------------------

       bMistake = .false.

C There should be one receiving connection to node 2 (the cooling water heat exchanger)

        iConnection = ICONDX(IPCOMP,2,1)         !-connection index to node 2

        if ( iConnection .EQ. 0 ) bMistake = .true.

        DO i = 2, MPCONC

           iConnection =  ICONDX(IPCOMP,2,i)
           if ( iConnection .NE. 0 ) bMistake = .true.

        ENDDO


        if ( bMistake ) then

           WRITE(IUOUT,*) ' H2_electrolyser_static_temp: incorrect'
           WRITE(IUOUT,*) ' number of connections for component ',IPCOMP
           STOP ' H2_electrolyser_static_temp: unresolvable error'

        endif



C-------------------------------------------------------------------------------------
C Check that the connection to node 2 is of the correct type. Should be water type (20)
C Variables used
C ICONTP(i) holds the state variable index ISV of the sending node for
C           the connection i
C  ISV      defines nodal fluid type & coefficient generator model capabilities:
C            ISV=0,10,20 node represents water + ....
C            ISV=1,11,21 node represents dry air + ....
C            ISV=9,19,29 node represents some solid material only
C            0 <=ISV<10  model suitable for energy balance only
C            10<=ISV<20  model suitable for energy + single phase mass balance
C            20<=ISV<30  model suitable for energy + two phase mass balances
C------------------------------------------------------------------------------------


        iConnection = ICONDX(IPCOMP,2,1)

        if ( ICONTP(iConnection) .NE. 20 ) then

             WRITE(IUOUT,*) ' H2 electrolyser_static_temp: incorrect'
             WRITE(IUOUT,*) ' conn type to node 2 for component', IPCOMP
             STOP ' H2 electrolyser_static_temp: unresolvable error'

        endif



C------------------------------------------------------------------------------------
C Set initialization flag to false. Flag used in coefficient generator to
C do perform time-invariant operations at first time-step only.
C------------------------------------------------------------------------------------

         bInitialized(IPCOMP) = .false.


C------------------------------------------------------------------------------------
C Set flag for inter-domain iteration.
C------------------------------------------------------------------------------------

         bInter_domain_iteration = .true.



         RETURN
         END



C************************ H2_electrolyser_coeff_gen *************************************
C
C     Created by: Maria Mottillo
C     Created on: February 3, 2005
C     Copyright: CETC
C
C
C     This subroutine creates a set of ESP-r plant matrix coefficients for
C     an electrolyser. The electrolyser is represented with 3 nodes.
C     The first node encompasses the electrolyser stack, the second
C     encompasses the cooling water heat exchanger and the third node
C     encompasses the heating element to increase the feed water to the
C     stack inlet temperature.
C
C
C     REFERENCES:
C
C     NRCan 2005. 'Design of an electrolyser component model',Internal Document,
C     Original draft December 21, 2004.
C
C*******************************************************************************


          SUBROUTINE H2_electrolyser_coeff_gen( IPCOMP, COUT, ISTATS)
          use h3kmodule
          IMPLICIT NONE

#include "plant.h"
#include "building.h"
#include "chemical_properties.h"


C External functions.
      integer lnblnk

C-------------------------------------------------------------------------------
C Passed variables
C-------------------------------------------------------------------------------
       INTEGER IPCOMP          !-plant component index
       REAL    COUT(MPCOE)     !-array of matrix coefficients for
                               !-plant component
       INTEGER ISTATS          !-flag indicating which matrix is being solved
                               ! istats = 1 -> temperature
                               ! istats = 2 -> 1st phase flow
                               ! istats = 3 -> 2nd phase flow
                               ! istats = 4 -> hydrogen flow

C------------------------------------------------------------------------------------
C ESP-r COMMONs
C------------------------------------------------------------------------------------

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       COMMON/TC/ITC,ICNT
       INTEGER ITC             !-trace output index
       INTEGER ICNT            !-trace output counting variable

       COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
       INTEGER ITCF            !-building-side time increment
                               !-to end trace output
       INTEGER ITRACE          !-trace call index
       INTEGER IZNTRC          !-zone trace index
       INTEGER ITU             !-trace output unit number

       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS             !-current building time-step within
                               !-current hour

       COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,ITRCLP,
     &              ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
       INTEGER MAXITP          !-maximum allowed number of iterations
                               !-for plant solution
       REAL    PERREL          !-maximum allowed relative error
                               !-for plant solution
       REAL    PERTMP          !-maximum allowed absolute error
                               !-temperature for plant solution (K)
       REAL    PERMFL          !-maximum allowed absolute error
                               !-mass flow rate for plant solution (kg/s)
       REAL    PERFLX          !-maximum allowed absolute error
                               !-heat flux for plant solution (W)
       INTEGER ITRCLP          !-?
       INTEGER ICSV            !-array holding iteration flag for
                               !-node and state variable
       REAL    CSVI            !-initial value for plant node and
                               !-state variable in case of iteration cycle


       COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
       INTEGER NPCOMP          !-number of plant components
       INTEGER NCI             !-number of possible control variables for component
       REAL    CDATA           !-array containing control data values


       COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),IPC2(MPCON),
     &            IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
       INTEGER NPCON           !-number of inter-component connections
       INTEGER IPC1            !-component number defining the receiving
                               !-component
       INTEGER IPN1            !-node number defining the receiving component
       INTEGER IPCT            !-plant component inter-connection type
       INTEGER IPC2            !-component number defining the sending
                               !-component
       INTEGER IPN2            !-node number defining the sending component
       REAL    PCONDR          !-ratio of mass flow rate through the connection
                               !-and the mass flow rate entering the receiving
                               !-node for each plant component inter-connection
       REAL    PCONSD          !-supplementary data for plant component inter-
                               !-connection

       COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &              ICONDX(MPCOM,MNODEC,MPCONC)
       REAL    CONVAR          !-component connection variable
       INTEGER ICONTP          !-state variable index for connection
       INTEGER ICONDX          !-pointer to connection of component/node

       COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
       REAL    ADATA           !-array containing component data
       REAL    BDATA           !-array containing component data


       COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
       INTEGER NPCDAT          !-miscellaneous plant component data
       INTEGER IPOFS1          !-row position of network matrix coefficient
       INTEGER IPOFS2          !-column position of network matrix coefficient


       COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
       REAL    CSVF            !-array holding future value for plant node
                               !-and state variable
       REAL    CSVP            !-array holding present value for plant node
                               !-and state variable

       COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &              PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),
     &              PCQP(MPNODE),PCNTMP(MPCOM)
       REAL    PCTF            !-future value of sending node temperature
                               !-for plant connection
       REAL    PCRF            !-future value of heat capacity rate for plant
                               !-connection
       REAL    PUAF            !-future value of plant node's UA value
       REAL    PCQF            !-future value of plant node heat addition/extraction
       REAL    PCNTMF          !-future value of component containment temperature
       REAL    PCTP            !-present value of sending node temperature for
                               !-plant connection
       REAL    PCRP            !-present value of heat capacity rate for plant
                               !-connection
       REAL    PUAP            !-present value of plant node's UA value
       REAL    PCQP            !-present value of plant node heat addition/extraction
       REAL    PCNTMP          !-present value of component containment temperature


       COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),NAPDAT(MPCOM)
       REAL    QDATA           !-?
       REAL    PCAOUT          !-additional output for component
       INTEGER NAPDAT          !-number of additional output for component

       COMMON/PCTC/TC(MPCOM)
       REAL    TC              !-time constant in seconds for plant component

       COMMON/PCEQU/IMPEXP,RATIMP
       INTEGER IMPEXP          !-flag identifying handling of plant equations
       REAL    RATIMP          !-user-specified implicit weighting fraction

       COMMON/PCTIME/TIMSEC
       REAL    TIMSEC          !-length of time increment (second)


       COMMON/pltcon/bpltcon,lastTStp
       LOGICAL bpltcon                  !-logical variable indicating if plant domain
       REAL    lastTStp                 !-converged in previous iteration


       COMMON/ELPCP/NPEL,PFP,IPFP,PWRP,BVOLTP, IPHP
       INTEGER  npel                 ! number of "electrified" plant components
       REAL     pfp(mpcom)           ! power factor of componet i (0->1)
       INTEGER  ipfp(mpcom)          ! integer indicating if power factor lags (-1) or leads (+1)
       REAL     pwrp(mpcom)          ! real power consumption of component (generation is -ive)
       REAL     bvoltp(mpcom)        ! opertional voltage of component
       INTEGER  iphp(mpcom)          ! phase component is connected to -> dummy variable

C.....Additional data
      common/pcdat/
     &     pcdatf(mpcom,mpcdat),
     &     pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)


C.....Common storing status of plant initialization.
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)


C.....Common storing plant component names
      common/pcnam/pcname(mpcom)
      character*15 pcname


C-------------------------------------------------------------------------------
C     Local variables
C------------------------------------------------------------------------------

       REAL      fEffCoeffE0      !- efficiency parametric coefficient E0 (-)
       REAL      fEffCoeffE1      !- efficiency coefficient E1 (-/W)
       REAL      fEffCoeffE2      !- efficiency coefficient E2 (-/W^2)
       REAL      fEffCoeffE3      !- efficiency coefficient E3 (/oC)
       REAL      fEffCoeffE4      !- efficiency coefficient E4 (/oC^2)
       REAL      fEffCoeffE5      !- efficiency coefficient E5 (/s)

       REAL      fInitAge         !- age of the electrolyser (seconds of operation)
                                  !- at the start of the simulation (s)

       REAL      fAuxACPowerCoeffW0 !- auxiliary AC power coefficient W0 (W)
       REAL      fAuxACPowerCoeffW1 !- auxiliary AC power coefficient W1 (-)
       REAL      fAuxDCPowerCoeffW0 !- auxiliary DC power coefficient W0 (W)
       REAL      fAuxDCPowerCoeffW1 !- auxiliary DCpower coefficient W1 (-)


       REAL      fMinPower        !- minimum protective power when electrolyser is in standby mode (W)
       REAL      fMaxStackPower   !- maximum power demand of electrolyser stack (W)
                                  !- if mode of operation is constant current (1) then this is the
                                  !- power demand of stack. if mode of operation is variable current (2)
                                  !- then this is the maximum capacity of the stack.

       REAL      fStackPower      !- power consumed by electrolyser stack (W)

       REAL      fO2CoeffA0       !- O2 production rate coefficient A0 (mol/s)
       REAL      fO2CoeffA1       !- O2 production rate coefficient A1 (-)
       REAL      fO2CoeffA2       !- O2 production rate coefficient A2 (s/mol)

       REAL      fH2OCoeffB0      !- H2O consumption rate coefficient B0 (mol/s)
       REAL      fH2OCoeffB1      !- H2O consumption rate coefficient B1 (-)
       REAL      fH2OCoeffB2      !- H2O consumption rate coefficient B2 (s/mol)

       REAL      fStackOpTemp     !- electrolyser stack operating temperature (oC)


       REAL      fQgenCoeffF0     !- internal heat generation coefficient F0 (W)
       REAL      fQgenCoeffF1     !- internal heat generation coefficient F1 (-)
       REAL      fQgenCoeffF2     !- internal heat generation coefficient F2 (/W)
       REAL      fQgenCoeffF3     !- internal heat generation coefficient F3 (W/oC)
       REAL      fQgenCoeffF4     !- internal heat generation coefficient F4 (W/oC^2)
       REAL      fQgenCoeffF5     !- internal heat generation coefficient F5 (W/s)

       REAL      fOverallHamb     !- overall heat transfer coefficent to surroudings (W/m2C)
       REAL      fSurfArea        !- surface area of electrolyser (m2)

       REAL      fHRCoeffG0       !- heat recovery coefficient G0 (W)
       REAL      fHRCoeffG1       !- heat recovery coefficient G1 (/W)
       REAL      fHRCoeffG2       !- heat recovery coefficient G2 (/W^2)
       REAL      fHRCoeffG3       !- heat recovery coefficient G3 (W/oC)
       REAL      fHRCoeffG4       !- heat recovery coefficient G4 (W/oC^2)
       REAL      fHRCoeffG5       !- heat recovery coefficient G5 (Ws/kg)
       REAL      fHRCoeffG6       !- heat recovery coefficient G6 (Ws^2/kg^2)
       REAL      fHRCoeffG7       !- heat recovery coefficient G7 (W/oC)
       REAL      fHRCoeffG8       !- heat recovery coefficient G8 (W/oC^2)

       INTEGER   iTempFlag        !- flag to indicate if feed water temperature
                                  !- is set to a constant value (flag .ne. 1) or if it
                                  !- set to the containment temperature (flag = 1)

       INTEGER   iContainment     !-Named constant
       PARAMETER( iContainment = 1 )

       REAL      fTempInlet       !- temperature of feed water to electrolyser (oC)
                                  !- variable holds ADATA model input if iTempFlag is not equal to 1
                                  !- or set to containment temperature if iTempFlag = 1.

       REAL      fVoltage         !- operating voltage of electrolyser (V)

       INTEGER   iOpMode          !- mode of operation of electrolyzer: 1 = constant current;
                                  !- 2 = variable current

       INTEGer   iFeedWaterHtg    !- flag indicating if feed water is heated with electric
                                  !- resistance heating (0=no, 1=yes)

       INTEGER   iNumADATAItems   !- number of ADATA items for this plant component


       INTEGER   iNode1, iNode2, iNode3 !- indices to nodes in overall plant matrix
       INTEGER   iConn2a                !- index to connection to node 2

       REAL      fTemp_node1      !- temperature of node 1 (oC)
       REAL      fTemp_node2      !- temperature of node 2 (oC)
       REAL      fTemp_node3      !- temperature of node 3 (oC)

       REAL      fDCPowerAux      !- DC power required by auxiliary equipment (W)
       REAL      fACPowerAux      !- AC power required by auxiliary equipment (W)
       REAL      fPowerAux        !- total power required by auxiliary equipment (W)

       REAL      fPresentTime     !- time electrolyser operates during simulation (s)

       REAL      fElecEfficiency  !- electrical efficiency of the electrolyser
       REAL      fH2_HHV          !- higher heating value of hydrogen (J/mol)
       save      fH2_HHV

       REAL      fH2NetFlow_mols  !- net production rate of hydrogen (mol/s)
       REAL      fH2NetFlow_kgs   !- net production rate of hydrogen (kg/s)

       REAL      fH2_MolWeight    !- molecular weight of hydrogen (kg/mol)
       save      fH2_MolWeight

       REAL      fO2NetFlow_mols  !- net production rate of oxygen (mol/s)
       REAL      fO2NetFlow_kgs   !- net production rate of oxygen (kg/s)

       REAL      fO2_MolWeight    !- molecular weight of oxygen (kg/mol)
       save      fO2_MolWeight

       REAL      fH2OTotalFlow_mols  !- total consumption rate of water (mol/s)
       REAL      fH2OTotalFlow_kgs   !- total consumption rate of water (kg/s)

       REAL      fH2O_MolWeight   !- molecular weight of water (kg/mol)
       save      fH2O_MolWeight

       REAL      H2OLiquidEnthalpy   !- function calculates standard enthalpy of
                                     !- liquid water (kJ/mol)
       REAL      fH2OEnthalpy        !- enthalpy of water fed to stack (J/mol)

       REAL      fMdotCW             !- mass flow rate of cooling water entering
                                     !- node 2 (kg/s)
       REAL      fTemp_con2a         !- temperature of cooling water entering
                                     !- node 3 (oC)
       REAL      fCpCW               !- specific heat of cooling water entering
                                     !- nod 2 (J/kgC)
       REAL      fQcool              !- heat transfer from node 1 (stack) to node
                                     !- 2 (cooling water), calculated based on previous
                                     !- timestep values (W)
       REAL      fUAeffective        !- effective heat transfer coefficient used to
                                     !- linearize heat transfer term from node 1 to
                                     !- node 2 in energy balance (W/oC)
       REAL      fTambient           !- future time-row temperature of containing
                                     !- room (oC)
       LOGICAL   CLOSE               !- used for comparing two values

       REAL      H2GasEnthalpy       !- function calculates the enthalpy of hydrogen
                                     !- gas (kJ/mol)
       REAL      O2GasEnthalpy       !- function calculates the enthalpy of oxygen
                                     !- gas (kJ/mol)
       REAL      fH2EnthalpyRef      !- enthalpy of hydrogen gas at a reference
                                     !- temperature of 25 oC (J/mol)
       PARAMETER( fH2EnthalpyRef = 0. )
       REAL      fO2EnthalpyRef      !- enthalpy of oxygen gas at a reference
                                     !- temperature of 25 oC (J/mol)
       PARAMETER( fO2EnthalpyRef = 0. )
       REAL      fTemp_ref           !- reference temperature for enthalpy/specific
                                     !- heat calculations (oC)
       PARAMETER( fTemp_ref = 25. )
       REAL      fCpH2               !- specific heat of hydrogen gas (J/molC)
       REAL      fCpO2               !- specific heat of oxygen gas (J/molC)

       REAL      SHTFLD              !- function evaluates specific heat (J/kgC)
       REAL      fCpInlet            !- specific heat of water supplied to node 3 (J/kgC)

       REAL      fQheat              !- energy required to heat inlet water to
                                     !- electrolyser stack operating temperature (W)

       REAL      fDCElecDemand       !- DC power consumption of electrolyser (W)
       REAL      fACElecDemand       !- AC power consumption of electrolyser (W)
       REAL      fTotalElecDemand    !- total power consumption of electrolyser (W)

       REAL      fOperatingSignal    !- operating signal (on/off) from control data
                                     !- a value of 1 means the unit is on, 0 means off
       REAL      fOnSignal           !- named constant
       PARAMETER( fOnSignal  = 1. )
       LOGICAL   bOn                 !- flag set to true if electrolyser is on according
                                     !- to control data

       REAL      fStandbySignal      !- standby mode signal from control data
                                     !- standby signal only applies when the electrolyser is off
                                     !- a value of 1 means the unit is on standby, 0 means the
                                     !- unit is not on standby but instead completely off.
       REAL      fStandbyOn          !- named constants
       PARAMETER( fStandbyOn =  1. )
       LOGICAL   bStandby            !- flag set to true if electrolyser is in standby mode
                                     !- according to control data

       INTEGER   i, itemp, j         !- counters

       INTEGER   iemodel             !- flag used for electrical calculations for plant component

       REAL      fPA_dummy           !- dummy variable holding total power (real + reactive)
                                     !- of plant component; not used but necessary for call to
                                     !- EMACH (subroutine output)

       REAL      fQgen_present       !- Internal heat generated by electrolyser (W) at
                                     !- present time-step

       REAL      fQloss_present      !- Heat loss to ambient (W) at present time-step


       REAL      fControlSignal      !- Power available to electrolyzer (W). Only applicable
                                     !- if the electrolyzer operating mode = 2 (variable current)


C......Variables used for XML output
       INTEGER Name_Length           ! temporary varaible's length

C.....Functions
      real fEval_Compound_HV      ! Function returning the heating
                                  !   value of a compound (J/kmol)
      real fEval_Compound_MM      ! Function returning the molar mass
                                  !   of a compound (kg/kmol)


C-------------------------------------------------------------------------------
C Start trace if trace output requested
C-------------------------------------------------------------------------------

       if ( ITC.GT.0 .AND. NSINC.GE.ITC .AND.
     &      NSINC.LE.ITCF .AND. ITRACE(37).NE.0 ) then

            WRITE(ITU,*) ' Entering sub H2_electrolyser_coeff_gen '

       endif



C .........Parametric coefficients for efficiency
           fEffCoeffE0      = ADATA( IPCOMP, 1 )
           fEffCoeffE1      = ADATA( IPCOMP, 2 )
           fEffCoeffE2      = ADATA( IPCOMP, 3 )
           fEffCoeffE3      = ADATA( IPCOMP, 4 )
           fEffCoeffE4      = ADATA( IPCOMP, 5 )
           fEffCoeffE5      = ADATA( IPCOMP, 6 )

C..........Age of electrolyser at start of simulation
           fInitAge         = ADATA( IPCOMP, 7 )

C......... Auxiliary AC Power coefficients, minimum protective power and stack power demand
           fAuxACPowerCoeffW0 = ADATA( IPCOMP, 8 )
           fAuxACPowerCoeffW1 = ADATA( IPCOMP, 9 )
           fMinPower        = ADATA( IPCOMP, 10 )
           fMaxStackPower   = ADATA( IPCOMP, 11 )

C..........Parametric coefficients for oxygen production rate
           fO2CoeffA0       = ADATA( IPCOMP, 12 )
           fO2CoeffA1       = ADATA( IPCOMP, 13 )
           fO2CoeffA2       = ADATA( IPCOMP, 14 )

C..........Parametric coefficients for water consumption rate
           fH2OCoeffB0      = ADATA( IPCOMP, 15 )
           fH2OCoeffB1      = ADATA( IPCOMP, 16 )
           fH2OCoeffB2      = ADATA( IPCOMP, 17 )

C..........Temperature of water supplied to electrolyser stack
           fStackOpTemp     = ADATA( IPCOMP, 18 )


C..........Parametric coefficients to calculate internal heat generation
           fQgenCoeffF0     = ADATA( IPCOMP, 19 )
           fQgenCoeffF1     = ADATA( IPCOMP, 20 )
           fQgenCoeffF2     = ADATA( IPCOMP, 21 )
           fQgenCoeffF3     = ADATA( IPCOMP, 22 )
           fQgenCoeffF4     = ADATA( IPCOMP, 23 )
           fQgenCoeffF5     = ADATA( IPCOMP, 24 )

C..........Parameters to calculate heat transfer to surroundings
           fOverallHamb     = ADATA( IPCOMP, 25 )
           fSurfArea        = ADATA( IPCOMP, 26 )

C..........Parametric coefficients to calculate heat rejected to cooling water
           fHRCoeffG0       = ADATA( IPCOMP, 27 )
           fHRCoeffG1       = ADATA( IPCOMP, 28 )
           fHRCoeffG2       = ADATA( IPCOMP, 29 )
           fHRCoeffG3       = ADATA( IPCOMP, 30 )
           fHRCoeffG4       = ADATA( IPCOMP, 31 )
           fHRCoeffG5       = ADATA( IPCOMP, 32 )
           fHRCoeffG6       = ADATA( IPCOMP, 33 )
           fHRCoeffG7       = ADATA( IPCOMP, 34 )
           fHRCoeffG8       = ADATA( IPCOMP, 35 )

C..........Characteristics of water fed to electrolyser
           iTempFlag        = int(ADATA( IPCOMP, 36 ))
           fTempInlet       = ADATA( IPCOMP, 37 )

C..........Operating voltage of electrolyser
           fVoltage         = ADATA( IPCOMP, 38 )

C..........Operating mode of electrolyser (1=Constant current, 2= Variable current)
           iOpMode          = int(ADATA( IPCOMP, 39 ))

C..........Flag indicating if water fed to electrolyzer is heated
           iFeedWaterHtg    = int(ADATA( IPCOMP, 40 ))

C..........Auxiliary DC power coefficients
           fAuxDCPowerCoeffW0 = ADATA( IPCOMP, 41 )
           fAuxDCPowerCoeffW1 = ADATA( IPCOMP, 42 )

C Determine the temperature of the room containing the electrolyser.
C PCNTMF(i) holds the future time-row temperature of the room containing
C component 'i'. It equals -99 if no containment is defined.

       CALL ECLOSE( PCNTMF(IPCOMP),-99.0, 0.001,CLOSE )

       if ( CLOSE ) then

           WRITE(IUOUT,*) ' H2_electrolyser_coeff_gen: '
           WRITE(IUOUT,*) ' the electrolyser must be contained '
           WRITE(IUOUT,*) ' within a room. '
           STOP ' H2_electrolyser_coeff_gen: unresolvable error '

       else

           fTambient = PCNTMF(IPCOMP)

       endif


C Determine the inlet temperature of the water fed to node 3. The variable
C iTempFlag indicates whether the inlet temperature should be set to a constant
C value (iTempFlag .NE. 1) or to the containment temperature (iTempFlag = 1).
C if iTempFlag  is not equal to 1, then fTempInlet is a model input
C stored in the variable fTempInlet.

       if ( iTempFlag .EQ. iContainment) then

               fTempInlet = fTambient         !- oC

       endif


C-------------------------------------------------------------------------------
C     Initialization
C-------------------------------------------------------------------------------

       if ( .not. bInitialized(IPCOMP) ) then

           iNumADATAItems = 42

C------------------------------------------------------------------------------------
C Write out ADATA if there is a trace output.
C------------------------------------------------------------------------------------

           if ( ITC.GT.0 .AND. ITRACE(35).NE.0 ) then  !-trace output requested for plant components

             WRITE(ITU,*) 'Component ',IPCOMP, 'pre-simulation',
     &                    'data for a'
             WRITE(ITU,*) 'hydrogen electrolyser'
             WRITE(ITU,*) 'ADATA ',(ADATA(IPCOMP,J),J=1,iNumADATAItems)

             if ( ITU.EQ.IUOUT ) then      !-trace output to screen
                itemp = (IPCOMP/5)*5
                if ( itemp.EQ.IPCOMP .OR. IPCOMP.EQ.NPCOMP) then
                    CALL EPAGEW            !-write 5 lines at a time
                endif
             endif

           endif



C Initialize counter for seconds that electrolyser is on
           fPresentTime = 0.


C Obtain HHV of H2 and molar mass of H2, O2 and H2O

           fH2_HHV = fEval_Compound_HV ( iHydrogen, iHHV ) / 1000.    ! J/mol

           fH2_MolWeight = fEval_Compound_MM ( iHydrogen ) / 1000.    ! kg/mol

           fO2_MolWeight = fEval_Compound_MM ( iOxygen ) / 1000.      ! kg/mol

           fH2O_MolWeight = fEval_Compound_MM ( iWater_l ) / 1000.    ! kg/mol

C end of time invariant calculations. Set initialization flag to true.

           bInitialized(IPCOMP) = .true.


       endif


C------------------------------------------------------------------------------------
C Set local variables to point to the nodes and to the connections.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C    iNode1         global matrix node number for component node 1
C    iNode2         global matrix node number for component node 2
C    iNode3         global matrix node number for component node 3
C    iConn2a        number of connection to node 2
C------------------------------------------------------------------------------------

           iNode1   = NPCDAT(IPCOMP,9)        !-index to node 1
           iNode2   = NPCDAT(IPCOMP,9) + 1    !-index to node 2
           iNode3   = NPCDAT(IPCOMP,9) + 2    !-index to node 3
           iConn2a  = ICONDX(IPCOMP,2,1)      !-index to connection to node 2





C-----------------------------------------------------------------------------------
C Obtain control data and set appropriate flags
C-----------------------------------------------------------------------------------

C first control data item is for operating signal (on/off)

       fOperatingSignal = CDATA(IPCOMP,1)

       call eclose( fOperatingSignal, fOnSignal, 0.001, close )

       if ( close ) then

          bOn = .true.

       else

          bOn = .false.

       endif

C second control data item is for standby mode (on/off). Standby mode
C applies for case when electrolyser is off.

       fStandbySignal = CDATA(IPCOMP,2)

       call eclose( fStandbySignal, fStandbyOn, 0.001, close )

       if ( close ) then

           bStandby = .true.

       else

           bStandby = .false.

       endif

C third control data item is the control signal to the electrolyzer (W)
C if the electrolyzer operates in variable current mode. This represents the
C power available to the electrolyzer

       fControlSignal = CDATA(IPCOMP,3)

C Check if control signal is less than zero, and if so, set to zero. Otherwise,
C the electrolyzer will turn into a PEM, inducing a negitive hydrogen flow and
C causing chaos in downstream components!
C
C This implementation merits additional thought --- a control signal of zero
C implies standby mode, which may necessitate setting the bStandby flag to
C true (above).

       if ( fControlSignal < 0.0 ) fControlSignal = 0.0


C Save operational state to PCDATF array. This will enable the time-step manipulation
C by subroutine Plt_TimeRow_Manipulate and thus enable control sensors to detect
C the operation state of the electrolyzer at present and future time rows.
      PCDATF(IPCOMP,1) = fOperatingSignal
      PCDATF(IPCOMP,2) = fStandbySignal
      PCDATF(IPCOMP,3) = fControlSignal


C------------------------------------------------------------------------------------
C Mark the temperatures of the first and second node for iteration. Mark the
C hydrogen flow rate for the first node for iteration.
C Variables used:
C    ICSV(i,j) flag indicating that node `i' is marked for iteration for state
C              variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C              j=3 for 2nd phase mass flow, j=4 for hydrogen flow
C    CSVI(i,j) initial value for judging whether iteration required. Same
C              indices as ICSV.
C    CSVF(i,j) future time-row solution variable for plant. Same indices
C     .        as ICSV.
C------------------------------------------------------------------------------------

         ICSV(iNode1,1) = 1
         ICSV(iNode2,1) = 1
         ICSV(iNode1,4) = 1

         CSVI(iNode1,1) = CSVF(iNode1,1)
         CSVI(iNode2,1) = CSVF(iNode2,1)
         CSVI(iNode1,4) = CSVF(iNode1,4)


C------------------------------------------------------------------------------------
C Establish the temperature of node 1 (stack) and  node 2 (cooling water h/x) from
C the previous timestep.
C Variables used:
C    CSVF(i,j)      future time-row solution variable for node i and state variable
C                   j. j=1 for temperature, j=2 for 1st phase mass flow and j=3 for
C                   2nd phase mass flow.
C------------------------------------------------------------------------------------

       fTemp_node1 = CSVF(iNode1,1)
       fTemp_node2 = CSVF(iNode2,1)


C The temperature of node 3 (feed water heating component) is the operating
C temperature of the electrolyser stack if the feed water is heated, otherwise
C it is the temperature of the feed water to electrolyzer.

       if ( iFeedWaterHtg .eq. 1 ) then

          fTemp_node3 = fStackOpTemp

       else

          fTemp_node3 = fTempInlet

       endif


C------------------------------------------------------------------------------------
C Determine power consumed by stack.
C------------------------------------------------------------------------------------

       if ( iOpMode .eq. 2 ) then

C Electrolyzer operates at variable current (power).

          if ( fControlSignal .le. fMaxStackPower ) then

C Available power less than maximum capacity of electrolyzer

               fStackPower = fControlSignal

          else
C Maximum capacity of electrolyzer surpassed. Electrolyzer operates at
C maximum capacity.

               fStackPower = fMaxStackPower

          endif

       else

C Electrolyzer operates at constant current (power).
C It is assumed that the stack power is equal to the maximum power demand of the stack.

              fStackPower = fMaxStackPower


       endif


C------------------------------------------------------------------------------------
C Determine power required by ancillary components.
C------------------------------------------------------------------------------------

       if ( bOn ) then

           fDCPowerAux = fAuxDCPowerCoeffW0 +
     &                   fAuxDCPowerCoeffW1 * fStackPower

           fACPowerAux = fAuxACPowerCoeffW0 +
     &                   fAuxACPowerCoeffW1 * fStackPower

      else                 !- electrolyser is off

           fDCPowerAux = 0.

           fACPowerAux = 0.

      endif

      fPowerAux = fDCPowerAux + fACPowerAux

C------------------------------------------------------------------------------------
C Determine AC and DC power consumption of electrolyser depending on mode of operation.
C It is assumed that the stack consumes DC power while the ancillaries consume AC power,
C DC power or a combination of both.
C Minimum power requirement during standby mode is DC.
C------------------------------------------------------------------------------------

       if ( bOn ) then            !- unit is on

          fDCElecDemand = fDCPowerAux + fStackPower
          fACElecDemand = fACPowerAux


       elseif ( .not.bOn .and. bStandby ) then         !- unit is not operating, in standby mode

          fDCElecDemand = fMinPower
          fACElecDemand = 0.

       elseif ( .not.bOn .AND. .not.bStandby ) then    !- unit is off

          fDCElecDemand = 0.
          fACElecDemand = 0.

       endif


       fTotalElecDemand = fDCElecDemand + fACElecDemand


C------------------------------------------------------------------------------
C Save DC and AC electrical demand of electrolyser  in electric network hybrid
C components power generation/use array.
C------------------------------------------------------------------------------

C Assign flag for calculation
       iemodel = 2

       call EMACH( IPCOMP,
     &             iemodel,
     &             -1. * fDCElecDemand,              !- loads are negative
     &             -1. * fACElecDemand,              !- loads are negative
     &             fPA_dummy )


C------------------------------------------------------------------------------
C Save voltage of electrolyser component
C------------------------------------------------------------------------------


       bvoltp(IPCOMP) = fVoltage             !-volt


C-----------------------------------------------------------------------------
C Tally seconds electrolyser is in operation. Only tally once per iteration.
C-----------------------------------------------------------------------------

       if ( istats . eq. 2 ) then

          if ( bOn ) fPresentTime = fPresentTime + TIMSEC            !-sec

       endif


C Determine electrical efficiency of electrolyser

       fElecEfficiency = fEffCoeffE0 +
     &                   fEffCoeffE1 * fStackPower +
     &                   fEffCoeffE2 * fStackPower**2 +
     &                   fEffCoeffE3 * fTemp_node1 +
     &                   fEffCoeffE4 * fTemp_node1**2 +
     &                   fEffCoeffE5 * ( fPresentTime - fInitAge )


C Determine the net production rate of hydrogen
C No hydrogen is produced if electrolyser is in standby mode

       if ( bOn ) then        !- unit is on and not in standby mode

          fH2NetFlow_mols = fElecEfficiency * fStackPower / fH2_HHV     !-mol/s

       else       !- unit is in standby mode

          fH2NetFlow_mols = 0.

       endif

        fH2NetFlow_kgs = fH2NetFlow_mols * fH2_MolWeight         !- convert to kg/s

C Determine the net production rate of oxygen
C No oxygen is produced if electrolyser is in standby mode.

       if ( bOn ) then

          fO2NetFlow_mols = fO2CoeffA0 + fO2CoeffA1 * fH2NetFlow_mols +
     &                   fO2CoeffA2 * fH2NetFlow_mols**2             !- mol/s

       else

          fO2NetFlow_mols = 0.

       endif

       fO2NetFlow_kgs = fO2NetFlow_mols * fO2_MolWeight         !- convert to kg/s


C Determine the total consumption rate of water
C No water is consumed if electrolyser is in standby mode.

       if ( bOn ) then

          fH2OTotalFlow_mols = fH2OCoeffB0 +
     &                         fH2OCoeffB1 * fH2NetFlow_mols +
     &                         fH2OCoeffB2 * fH2NetFlow_mols**2         !- mol/s

      else

          fH2OTotalFlow_mols = 0.

      endif

      fH2OTotalFlow_kgs = fH2OTotalFlow_mols * fH2O_MolWeight  !- convert to kg/s


C Determine the enthalpy of the water entering the stack

          fH2OEnthalpy = H2OLiquidEnthalpy(fTemp_node3) * 1000.     !- J/mol


C Determine the mass flow rate of the cooling water entering node 2
       fMdotCW = CONVAR(iConn2a,2)


C Determine the temperature of the cooling water entering node 2
       fTemp_con2a = CONVAR(iConn2a,1)

C Determine the specific heat of the cooling water entering node 2
C using ESP-r's SHTFLD(j,T) function. SHTFLD evaluates the specific heat
C of a function at a temperature T; j=1 for dry air, j=2 for water vapour
C and j=3 for water.

       fCpCW = SHTFLD(3,fTemp_con2a)       !- J/kgC


C Find the effective heat transfer coefficient between the stack and the
C cooling water.

       fQcool = fHRCoeffG0 + fHRCoeffG1 * fTotalElecDemand +
     &          fHRCoeffG2 * fTotalElecDemand**2 +
     &          fHRCoeffG3 * fTemp_node1 +
     &          fHRCoeffG4 * fTemp_node1**2 +
     &          fHRCoeffG5 * fMdotCW +
     &          fHRCoeffG6 * fMdotCW**2 +
     &          fHRCoeffG7 * fTemp_con2a +
     &          fHRCoeffG8 * fTemp_con2a**2

      call eclose( fTemp_node1, fTemp_con2a, 0.001, close )
      if ( close ) then
         fUAeffective = 0.
      else
         fUAeffective = fQcool / ( fTemp_node1 - fTemp_con2a )        !- W/oC
      endif


C Determine specific heat of hydrogen

       fCpH2 = ( H2GasEnthalpy(fTemp_node1) - fH2EnthalpyRef ) /
     &         ( fTemp_node1 - fTemp_ref ) * 1000.                      !- J/molC


C Determine specific heat of oxygen

       fCpO2 = ( O2GasEnthalpy(fTemp_node1) - fO2EnthalpyRef ) /
     &         ( fTemp_node1 - fTemp_ref ) * 1000.                      !- J/molC



C Determine specific heat of water fed to node 3 using ESP-r's SHTFLD(j,T)
C function. SHTFLD evaluates the specific heat of a function at a temperature
C T; j=1 for dry air, j=2 for water vapour and j=3 for water.

       fCpInlet = SHTFLD(3,fTempInlet)         !- J/kgC


C Determine the energy required to heat the inlet water to the electrolyser
C stack operating temperature.

       fQheat = fH2OTotalFlow_kgs * fCpInlet *
     &             ( fTemp_node3 - fTempInlet )       !- W


C If fQheat is zero, feed water is not heated.
       call eclose ( fQheat, 0., 0.001, close)
       if ( close ) then
         fQheat = 0.
       endif

C Determine internal heat generated by electrolyser at present time-step
          fQgen_present = fQgenCoeffF0 +
     &               fQgenCoeffF1 * fTotalElecDemand +
     &               fQgenCoeffF2 * fTotalElecDemand**2 +
     &               fQgenCoeffF3 * fTemp_node1 +
     &               fQgenCoeffF4 * fTemp_node1**2 +
     &               fQgenCoeffF5 * ( fPresentTime - fInitAge )

C Determine heat loss to ambient at present time-step
          fQloss_present = fOverallHamb * fSurfArea *
     &                     ( fTemp_node1 - fTambient )


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.
C Node coefficient transport for COUT:
C       <--self-->|<cross>
C  node   1  2  3 |  m      RHS
C       -----------------------
C         1  0  0 |  0       6
C         2  3  0 |  5   =   7
C         0  0  4 |  0       8
C---------------------------------------------------------------------------------


C  Beginning of energy balance

       if ( ISTATS .EQ. 1 ) then

            if ( bOn ) then

C...........Node 1 energy balance

               COUT(1) = fOverallHamb * fSurfArea + fUAeffective +
     &                   fH2NetFlow_mols * fCpH2 +
     &                   fO2NetFlow_mols * fCpO2 +
     &                   (-1.) * ( fQgenCoeffF3 + fQgenCoeffF4 *
     &                           fTemp_node1 )


               COUT(6) = fTotalElecDemand + fPowerAux +
     &                   fH2OTotalFlow_mols * fH2OEnthalpy +
     &                   fQgenCoeffF0 +
     &                   fQgenCoeffF1 * fTotalElecDemand +
     &                   fQgenCoeffF2 * fTotalElecDemand**2 +
     &                   fQgenCoeffF5 * ( fPresentTime - fInitAge ) +
     &                   fOverallHamb * fSurfArea * fTambient +
     &                   fH2NetFlow_mols * fCpH2 * fTemp_ref -
     &                   fH2NetFlow_mols * fH2EnthalpyRef +
     &                   fO2NetFlow_mols * fCpO2 * fTemp_ref -
     &                   fO2NetFlow_mols * fO2EnthalpyRef +
     &                   fUAeffective * fTemp_con2a

C...........Node 2 energy balance

               COUT(2) = fUAeffective
               COUT(3) = -1. * fMdotCW * fCpCW
               COUT(5) = fMdotCW * fCpCW - fUAeffective
               COUT(7) = 0.

C...........Node 3 energy balance: temperature known

               COUT(4) = 1.
               COUT(8) = fTemp_node3


           else                  !- unit is off


C...........Node 1 energy balance
               if ( bStandby ) then   !- unit is in standby mode

                  COUT(1) = fOverallHamb * fSurfArea
                  COUT(6) = fMinPower + fOverallHamb * fSurfArea *
     &                      fTambient

               else               !- unit is completely off

                  COUT(1) = 1.
                  COUT(6) = fTambient

               endif

C...........Node 2 energy balance: no heat transfer from node 1 to node 2.
               COUT(2) = 0.
               COUT(3) = 1.
               COUT(5) = -1.
               COUT(7) = 0.           !- should this be set to dummy value, other than 0?

C...........Node 3 energy balance:

               COUT(4) = 1.
               COUT(8) = 0.           !- should this be set to dummy value, other than 0?

            endif



C  End of energy balance/start of 1st phase mass balance

       elseif ( ISTATS .EQ. 2 ) then

C...........Node 1: no balance required so make flow zero
            COUT(1) = 1.
            COUT(6) = 0.

C            if ( bOn ) then

C...........Node 2 mass balance: flow in equals flow out
               COUT(2) = 0.
               COUT(3) = 1.
               COUT(5) = -1. * PCONDR(iConn2a)
               COUT(7) = 0.

C            else           !- unit is off

C               COUT(2) = 0.
C               COUT(3) = 1.
C               COUT(6) = 0.
C               COUT(8) = 0.

C            endif

C...........Node 3: no balance so make flow zero
            COUT(4) = 1.
            COUT(8) = 0.

!- End of 1st phase mass balance/start of 2nd phase mass balance

       elseif ( ISTATS .EQ. 3 ) then

C...........Node 1: no balance required so make flow zero
            COUT(1) = 1.
            COUT(6) = 0.

C...........Node 2: no balance required so make flow zero
            COUT(2) = 0.
            COUT(3) = 1.
            COUT(5) = 0.
            COUT(7) = 0.

C...........Node 3: no balance so make flow zero
            COUT(4) = 1.
            COUT(8) = 0.

!- End of 2nd phase mass balance/start of hydrogen flow

       elseif ( ISTATS .EQ. 4 ) then

            if ( bOn ) then

C...........Node 1: electrolyser stack source of hydrogen
               COUT(1) = 1.
               COUT(6) = fH2NetFlow_kgs

            else             !- unit if off, no hydrogen is produced

               COUT(1) = 1.
               COUT(6) = 0.

            endif

C...........Node 2: no balance required so make flow zero
            COUT(2) = 0.
            COUT(3) = 1.
            COUT(5) = 0.
            COUT(7) = 0.

C...........Node 3: no balance so make flow zero
            COUT(4) = 1.
            COUT(8) = 0.


       endif

C  End of energy, mass and hydrogen balances



C----------------------------------------------------------------------------------------
C Complete trace if trace output requested
C----------------------------------------------------------------------------------------

       if ( (ITC.GT.0) .AND. (NSINC.GE.ITC) .AND. (NSINC.LE.ITCF)
     &       .AND. (ITRACE(37).NE.0) ) then

          WRITE(ITU,*) 'Component     ', IPCOMP, ':'
          WRITE(ITU,*) '3 node electrolyser '
          WRITE(ITU,*) 'Matrix nodes  ', iNode1, iNode2, iNode3
          WRITE(ITU,*) 'Connections   ', iConn2a

          WRITE(ITU,*) 'Matrix coefficients for ISTATS = ', ISTATS
          WRITE(ITU,*) ( COUT(i), i = 1,9 )
          if ( ITU.EQ.IUOUT ) then
               itemp = ( IPCOMP / 4 ) * 4
               if ( ( itemp.EQ.IPCOMP ) .OR. ( IPCOMP.EQ.NPCOMP ) )
     &                call epagew                     !- write 4 lines at a time
          endif
          WRITE(ITU,*) ' Leaving subroutine H2_electrolyser_coeff_gen '

      endif



C----------------------------------------------------------------------------------------
C XML output
C----------------------------------------------------------------------------------------

C.....Get component name's length
       name_length = lnblnk(pcname(IPCOMP))

       Call AddToReport(rvPltOprtSgn%Identifier,
     &         fOperatingSignal,
     &         pcname(IPCOMP)(1:name_length))


      Call AddToReport(rvPltStndSgn%Identifier,
     &         fStandbySignal,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltCtrlSgnEltrzr%Identifier,
     &         fControlSignal,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltTtlAuxPow%Identifier,
     &         fPowerAux,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltDCAuxPow%Identifier,
     &         fDCPowerAux,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltACAuxPow%Identifier,
     &         fACPowerAux,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltStckPow%Identifier,
     &         fStackPower,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltMxStckPow%Identifier,
     &         fMaxStackPower,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltOpMode%Identifier,
     &         real(iOpMode),
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltDCPowDmd%Identifier,
     &         fDCElecDemand,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltACPowDmd%Identifier,
     &         fACElecDemand,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltElecEffEltrzr%Identifier,
     &         fElecEfficiency,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltH2FlwKgs%Identifier,
     &         fH2NetFlow_kgs,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPlt02FlwKgs%Identifier,
     &         fO2NetFlow_kgs,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltH20FlwKgs%Identifier,
     &         fH2OTotalFlow_kgs,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltQIntPrst%Identifier,
     &         fQgen_present,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltQlssPrst%Identifier,
     &         fQloss_present,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltQCoolPrst%Identifier,
     &         fQcool,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltQInltWtrHtg%Identifier,
     &         fQheat,
     &         pcname(IPCOMP)(1:name_length))

      Call AddToReport(rvPltTnltWtr%Identifier,
     &         fTempInlet,
     &         pcname(IPCOMP)(1:name_length))


       RETURN
       END




C*************************************************************************************************


C****************************************Function H2OLiquidEnthalpy*******************************
C
C This function determines the enthalpy of liquid water using a correlation to a reference
C temperature. Specifically, the Shomate equation provided on the National Institute of
C Standards and Technology (NIST) website is used. The Shomate equation is valid
C for temperatures in the range 25-227 oC.
C
C Input:
C fTemp: temperature (oC)
C
C Output:
C H2OLiquidEnthalpy: standard enthalpy of liquid water (kJ/mol)
C
C--------------------------------------------------------------------------------------------------

       FUNCTION H2OLiquidEnthalpy(fTemp)
       IMPLICIT NONE

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       REAL   fTemp    !- temperature (oC)
       REAL   fTemp_K  !- temperature (K)
       REAL   ftau     !- T(K)/1000
       REAL   fCoeffA, fCoeffB, fCoeffC, fCoeffD, fCoeffE     !- coefficients of Shomate equation
       REAL   fCoeffF, fCoeffH

       PARAMETER( fCoeffA = -203.6060,
     &            fCoeffB = 1523.290,
     &            fCoeffC = -3196.413,
     &            fCoeffD = 2474.455,
     &            fCoeffE = 3.855326,
     &            fCoeffF = -256.5478,
     &            fCoeffH = -285.8304 )

       REAL   H2OLiquidEnthalpy    !- output in kJ/mol

       REAL   fHref                !- standard enthalpy of liquid water at reference
                                   !- temperature of 25 oC (298.15 K), kJ/mol
       PARAMETER( fHref = 1.889  ) !- kJ/mol  (Van Wylen et al., 1994) CHECK THIS VALUE.

       REAL   fDeltaH              !- difference between standard enthalpy at temperature
                                   !- fTemp and at the reference temperature (kJ/mol)

       REAL   fTempRangeMin        !- lower limit of temperature for which Shomate equation is valid (oC)
       REAL   fTempRangeMax        !- upper limit of temperature for which Shomate equation is valid (oC)
       PARAMETER( fTempRangeMin = -10.,       !- changed from 25 to -10 to avoid potential errors.
     &            fTempRangeMax = 227. )


C error message if temperature is outside valid range

       if ( fTemp.LT. fTempRangeMin .OR. fTemp.GT.fTempRangeMax ) then

           WRITE(IUOUT,*) 'temperature outside valid range'
           STOP 'FUNCTION H2OLiquidEnthalpy: unresolvable error'

       endif


       fTemp_K = fTemp + 273.15    !- convert to K

       ftau = fTemp_K / 1000.

       fDeltaH = fCoeffA * ftau +
     &           fCoeffB / 2. * ftau**2 +
     &           fCoeffC / 3. * ftau**3 +
     &           fCoeffD / 4. * ftau**4 -
     &           fCoeffE / ftau +
     &           fCoeffF - fCoeffH


       H2OLiquidEnthalpy = fDeltaH + fHref         !- kJ/mol




       RETURN
       END

C*************************************************************************************************


C*************************************FUNCTION H2GasEnthalpy**************************************
C
C This function determines the enthalpy of hydrogen gas using a correlation to a reference
C temperature. Specifically, the Shomate equation provided on the National Institute of
C Standards and Technology (NIST) website is used. The Shomate equation is valid
C for temperatures in the range 25-5727 oC.
C
C Input:
C fTemp: temperature (oC)
C
C Output:
C H2GasEnthalpy: standard enthalpy of hydrogen gas (kJ/mol)
C
C--------------------------------------------------------------------------------------------------

       FUNCTION H2GasEnthalpy(fTemp)
       IMPLICIT NONE

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       REAL   fTemp    !- temperature (oC)
       REAL   fTemp_K  !- temperature (K)
       REAL   ftau     !- T(K)/1000
       REAL   fCoeffA1, fCoeffB1, fCoeffC1, fCoeffD1, fCoeffE1   !- coefficients of Shomate equation
       REAL   fCoeffF1, fCoeffH1                                 !- valid in the temperature range
                                                                 !- 25-727 oC
       PARAMETER( fCoeffA1 = 33.066178,
     &            fCoeffB1 = -11.363417,
     &            fCoeffC1 = 11.432816,
     &            fCoeffD1 = -2.772874,
     &            fCoeffE1 = -0.158558,
     &            fCoeffF1 = -9.980797,
     &            fCoeffH1 = 0.0 )

       REAL   fCoeffA2, fCoeffB2, fCoeffC2, fCoeffD2, fCoeffE2   !- coefficients of Shomate equation
       REAL   fCoeffF2, fCoeffH2                                 !- valid in the temperature range
                                                                 !- 728-2227 oC
       PARAMETER( fCoeffA2 = 18.563083,
     &            fCoeffB2 = 12.257357,
     &            fCoeffC2 = -2.859786,
     &            fCoeffD2 = 0.268238,
     &            fCoeffE2 = 1.977990,
     &            fCoeffF2 = -1.147438,
     &            fCoeffH2 = 0.0 )


       REAL   fCoeffA3, fCoeffB3, fCoeffC3, fCoeffD3, fCoeffE3   !- coefficients of Shomate equation
       REAL   fCoeffF3, fCoeffH3                                 !- valid in the temperature range
                                                                 !- 2228-5727 oC
       PARAMETER( fCoeffA3 = 43.413560,
     &            fCoeffB3 = -4.293079,
     &            fCoeffC3 = 1.272428,
     &            fCoeffD3 = -0.096876,
     &            fCoeffE3 = -20.533862,
     &            fCoeffF3 = -38.515158,
     &            fCoeffH3 = 0.0 )


       REAL   H2GasEnthalpy    !- output in kJ/mol

       REAL   fHref                !- standard enthalpy of hydrogen gas at reference
                                   !- temperature of 25 oC (298.15 K), kJ/mol
       PARAMETER( fHref = 0. ) !- kJ/mol  (Van Wylen et al., 1994) CHECK THIS VALUE.

       REAL   fDeltaH              !- difference between standard enthalpy at temperature
                                   !- fTemp and at the reference temperature (kJ/mol)

       REAL   fTempRangeMin        !- lower limit of temperature for which Shomate equation is valid (oC)
       REAL   fTempRange1          !- upper value of first temperature range
       REAL   fTempRange2          !- upper value of second temperature range
       REAL   fTempRangeMax        !- upper limit of temperature for which Shomate equation is valid (oC)
       PARAMETER( fTempRangeMin = -10.,         !- on nist = 25, changed to avoid potential errors.
     &            fTempRange1   = 727.,
     &            fTempRange2   = 2227.,
     &            fTempRangeMax = 5727. )



       fTemp_K = fTemp + 273.15    !- convert to K

       ftau = fTemp_K / 1000.

       if ( fTemp.GE.fTempRangeMin .AND. fTemp.LE.fTempRange1 ) then


          fDeltaH = fCoeffA1 * ftau +
     &              fCoeffB1 / 2. * ftau**2 +
     &              fCoeffC1 / 3. * ftau**3 +
     &              fCoeffD1 / 4. * ftau**4 -
     &              fCoeffE1 / ftau +
     &              fCoeffF1 - fCoeffH1


       elseif ( fTemp.GT.fTempRange1 .AND. fTemp.LE.fTempRange2 ) then

          fDeltaH = fCoeffA2 * ftau +
     &              fCoeffB2 / 2. * ftau**2 +
     &              fCoeffC2 / 3. * ftau**3 +
     &              fCoeffD2 / 4. * ftau**4 -
     &              fCoeffE2 / ftau +
     &              fCoeffF2 - fCoeffH2

       elseif ( fTemp.GT.fTempRange2 .AND. fTemp.LE.fTempRangeMax ) then

          fDeltaH = fCoeffA3 * ftau +
     &              fCoeffB3 / 2. * ftau**2 +
     &              fCoeffC3 / 3. * ftau**3 +
     &              fCoeffD3 / 4. * ftau**4 -
     &              fCoeffE3 / ftau +
     &              fCoeffF3 - fCoeffH3


       else                    !- error message if temperature is outside valid range


          WRITE(IUOUT,*) 'temperature outside valid range'
          STOP 'FUNCTION H2GasEnthalpy: unresolvable error'

       endif


       H2GasEnthalpy = fDeltaH + fHref         !- kJ/mol


       RETURN
       END

C************************************************************************************************

C*************************************FUNCTION O2GasEnthalpy*************************************
C
C This function determines the enthalpy of oxygen gas using a correlation to a reference
C temperature. Specifically, the Shomate equation provided on the National Institute of
C Standards and Technology (NIST) website is used. The Shomate equation is valid
C for temperatures in the range 25-5727 oC.
C
C Input:
C fTemp: temperature (oC)
C
C Output:
C O2GasEnthalpy: standard enthalpy of oxygen gas (kJ/mol)
C
C--------------------------------------------------------------------------------------------------

       FUNCTION O2GasEnthalpy(fTemp)
       IMPLICIT NONE

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       REAL   fTemp    !- temperature (oC)
       REAL   fTemp_K  !- temperature (K)
       REAL   ftau     !- T(K)/1000
       REAL   fCoeffA, fCoeffB, fCoeffC, fCoeffD, fCoeffE     !- coefficients of Shomate equation
       REAL   fCoeffF, fCoeffH

       PARAMETER( fCoeffA = 29.65900,
     &            fCoeffB = 6.137261,
     &            fCoeffC = -1.186521,
     &            fCoeffD = 0.095780,
     &            fCoeffE = -0.219663,
     &            fCoeffF = -9.861391,
     &            fCoeffH = 0.0 )

       REAL   O2GasEnthalpy    !- output in kJ/mol

       REAL   fHref                !- standard enthalpy of oxygen gas at reference
                                   !- temperature of 25 oC (298.15 K), kJ/mol
       PARAMETER( fHref = 0.  ) !- kJ/mol  (Van Wylen et al., 1994) CHECK THIS VALUE.

       REAL   fDeltaH              !- difference between standard enthalpy at temperature
                                   !- fTemp and at the reference temperature (kJ/mol)

       REAL   fTempRangeMin        !- lower limit of temperature for which Shomate equation is valid (oC)
       REAL   fTempRangeMax        !- upper limit of temperature for which Shomate equation is valid (oC)
       PARAMETER( fTempRangeMin = -10.,        !- nist website, = 25; changed to avoid potential errors.
     &            fTempRangeMax = 5727. )


C error message if temperature is outside valid range

       if ( fTemp.LT. fTempRangeMin .OR. fTemp.GT.fTempRangeMax ) then

           WRITE(IUOUT,*) 'temperature outside valid range'
           STOP 'FUNCTION O2GasEnthalpy: unresolvable error'

       endif


       fTemp_K = fTemp + 273.15    !- convert to K

       ftau = fTemp_K / 1000.

       fDeltaH = fCoeffA * ftau +
     &           fCoeffB / 2. * ftau**2 +
     &           fCoeffC / 3. * ftau**3 +
     &           fCoeffD / 4. * ftau**4 -
     &           fCoeffE / ftau +
     &           fCoeffF - fCoeffH


       O2GasEnthalpy = fDeltaH + fHref         !- kJ/mol



       RETURN
       END

C*************************************************************************************************
