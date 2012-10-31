C------------------------------------------------------------------------------------
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

C This file contains the following subroutines related to the component model
C of an adorption storage unit.
C
C ADS_stg_static_template: Static template executed prior to timestep simulation.
C
C ADS_stg_coeff_gen: Coefficient generator for adsorption storage unit component.
C                    This subroutine is executed each timestep, or iteration (there
C                    may be several iterations per timestep for convergence).
C
C This file also contains the following function:
C ADS_mass_ratio: This function calculates the ratio of adsorbate to adsorbent, X,
C                 within the adsorber using the Dubinin-Astakov (D-A) equation. X
C                 is a function of pressure and temperature within the adsorber.
C
C------------------------------------------------------------------------------------
C References:

C Hensen J.L.M. (1991), On the Thermal Interaction of Building Structure and
C Heating and Ventilation System, PhD Thesis, Technical University of Eindhoven,
C The Netherlands.

C Mottillo M. (2006), Investigation of an Adsorption System for the Seasonal
C Storage of Heat Applied to Residential Buildings, M.A.Sc. dissertation,
C Concordia University, Montreal, Canada.
C------------------------------------------------------------------------------------


C------------------------------------------------------------------------------------
C------------------------------ADS_stg_static_template-------------------------------
C Created by: Maria Mottillo
C Initial creation date: November 10, 2004

C This subroutine is the static template created for the component
C model of an adsorption storage unit. The model is structured to
C support any adsorbent (whose characteristics are user inputs) and
C a water adsorbate. This subroutine assigns user-input data to the
C common variables, checks that the component is properly defined
C in the plant network and performs time-invariant calculations.

C The adsorption storage unit component is represented with 3 nodes:
C node 1 represents the adsorber
C node 2 represents the condenser.
C node 3 represents the evaporator.

C INPUTS:
C    IPCOMP     plant component index
C    NPCOMP     number of plant components
C    NCI(i)     number of possible plant control variables for plant component `i'
C    ADATA(i,j) holds `j'th data item for component `i' read from .pln file
C               (derived from plant components database)
C    ICONTP(i)  holds the state variable index (ISV) of the sending node for connection `i'.
C    IUOUT      output channel to feedback to user
C    ITC        trace output index (>0 gives a trace output)
C    ITU        trace output unit number
C    ITRACE(35) flag for plant component trace output (1= active)

C OUTPUTS:
C    fMassAdsorbent:    Mass of adsorbent contained in the adsorber (kg).
C    fCpAdsorbent:      Specific heat of adsorbent contained in adsorber (J/kgK).
C    fCoeff_Wo:         Coefficient Wo of Dubinin-Astakhov (D-A) adsorption equilibrium
C                       equation. Wo represents the volume of the adsorption space,
C                       a charateristic of the adsorbent (m3/kg adsorbent).
C    fCoeff_D:          Coefficient D of D-A adsorption equilibrium equation. D is a
C                       characteristic of the adsorbent/adsorbate pair (-).
C    fCoeff_n:          Coefficient n of D-A adsorption equilibrium equation. n is a
C                       characteristic of the adsorbent/adsorbate pair (-).
C    fMassAdsVessel:    Mass of the adsorber vessel (kg).
C    fCpAdsVessel:      Specific heat of adsorber vessel material (J/kgK).
C    fMassAdsHeatX:     Mass of heat exchanger within the adsorber (kg).
C    fCpAdsHeatX:       Specific heat of heat exchanger material within the adsorber (J/kgK).
C    fAdsEnthalpy:      Enthalpy of adsorption (assumed constant) (J/kg);
C    fDT_Derivative:    Delta T used to obtain dX/dT derivative (oC).
C    fSurfAreaAdsorber: Surface area of adsorber (m2).
C    fAdsInsulL:        Thickness of adsorber insulation layer (m).
C    fAdsInsulK:        Thermal conductivity of adsorber insulation (W/mK).
C    fAdsOverallHAmb:   Overall heat transfer coefficient adsorber-surroundings (W/m2K).
C    fAdsFCHeatXeff:    Effectiveness of heat exchanger adsorber-fuel cell (-).
C    fPressAdsFC:       Pressure of adsorber-fuel cell heat exchange loop (kPa).
C    fAdsTankHeatXeff:  Effectiveness of heat exchanger adsorber-main water storage tank (-).
C    fPressAdsTank:     Pressure of adsorber-main water storage tank heat exchange loop (kPa).
C    fMinTempDiff:      Minimum temperature difference between adsorber and heat source or
C                       heat sink. This is used to determine end of charging/discharge phases,
C                       respectively (oC).
C    fChargeMinTemp:    Minimum temperature of heat source required to charge the
C                       the adsorber (oC).
C    fCondPressure:     Condenser pressure (kPa).
C    fCondEffec:        Condenser heat exchanger effectiveness (-).
C    fMassFlowAirCond:  Mass flow rate of air entering condenser (kg/s).
C    fSurfAreaCond:     Surface area of condenser (m2).
C    fCondOverallHamb:  Overall heat transfer coefficient condenser-surroundings (W/m2K).
C    fCondFanPower:     Condenser fan power rating (W).
C    fEvapPressure:     Evaporator pressure (kPa).
C    fEvapEffec:        Evaporator heat exchanger effectiveness (-).
C    fSurfAreaEvap:     Surface area of evaporator (m2).
C    fEvapOverallHamb:  Overall heat transfer coefficient evaporator-surroundings (W/m2K).
C    fMassCp_Adsorbent: product of mass and specific heat of adsorbent within
C                       adsorber (J/K).
C    fMassCp_AdsVessel: product of mass and specific heat of adsorber vessel (J/K).
C    fMassCp_AdsHeatX:  product of mass and specific heat of h/x within adsorber (J/K).
C    fRotal_Adsorber:   total resistance adsorber-surroundings (m2oC/W).
C    NOTE: variables stored in COMMON/ADS_storage_1/ and COMMON/ADS_storage_2 (in ADS_storage.h)
C------------------------------------------------------------------------------------


       SUBROUTINE ADS_stg_static_template(IPCOMP)
       IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "ADS_storage.h"


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


       COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
       INTEGER NPCOMP          !-number of plant components
       INTEGER NCI             !-number of possible control variables for component
       REAL    CDATA           !-array containing control data values

       COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
       REAL    ADATA           !-array containing component data
       REAL    BDATA           !-array containing component data

       COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &              ICONDX(MPCOM,MNODEC,MPCONC)
       REAL    CONVAR          !-component connection variable
       INTEGER ICONTP          !-state variable index for connection
       INTEGER ICONDX          !-pointer to connection of component/node

       COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
       INTEGER NPCDAT          !-miscellaneous plant component data
       INTEGER IPOFS1          !-row position of network matrix coefficient
       INTEGER IPOFS2          !-column position of network matrix coefficient


C------------------------------------------------------------------------------------

C------------------------------------------------------------------------------------
C COMMON holding values indicating phase of cycle
C------------------------------------------------------------------------------------

       COMMON/ADS_storage_4/iPhaseCycle


       INTEGER    iPhaseCycle


C------------------------------------------------------------------------------------
C Local variables
C------------------------------------------------------------------------------------

       INTEGER  IPCOMP                     !-plant component index
       INTEGER  iNumADATAItems             !-number of ADATA items for component
       PARAMETER ( iNumADATAItems = 33 )
       INTEGER  iNumCtlVar                 !-number of control variables for component
       PARAMETER ( iNumCtlVar = 2 )        !-if only simulating component, change to 0!
       INTEGER  itemp, i, j
       LOGICAL  bMistake
       REAL     ADS_mass_ratio


C------------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C------------------------------------------------------------------------------------

      if ( ITC.GT.0 .AND. ITRACE(35).NE.0 ) then
         WRITE(ITU,*) 'Component ',IPCOMP, 'pre-simulation data for a'
         WRITE(ITU,*) '3-node adsorption storage unit'
         WRITE(ITU,*) 'ADATA ',(ADATA(IPCOMP,J),J=1,iNumADATAItems)
         if ( ITU.EQ.IUOUT ) then      !-trace output to screen
            itemp = (IPCOMP/5)*5
            if ( itemp.EQ.IPCOMP .OR. IPCOMP.EQ.NPCOMP) then
                CALL EPAGEW            !-write 5 lines at a time
            endif
         endif
      endif



C------------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C the plant network (.pln) file. NCI(IPCOMP) holds the number of possible
C plant control variables as specified in the .pln file.
C The adsorption storage component has two control variables.
C------------------------------------------------------------------------------------
C This code is commented out here for testing of plant component model-no control
C variables are considered.

      if ( NCI(IPCOMP) .NE. iNumCtlVar ) then
         WRITE(ITU,*) 'ADS_storage_static_template warning: incorrect',
     &                'number of controlled variables specified.;'
      endif



C------------------------------------------------------------------------------------
C Check that each node for the plant component  has the correct number of connections
C to other plant components.
C Variables used:
C    MPCONC           the maximum allowable connections to a node (from plant.h).
C    ICONDX(i,j,k)    the connection number for the k'th connection to the j'th node
C                     of component i. It is used as a pointer.
C    bMistake         flag indicating whether there are connection errors;
C                     (.true. means there are errors).
C------------------------------------------------------------------------------------
      bMistake = .false.
C-----There should be two connections to node 1 (adsorber).
      if ( ICONDX(IPCOMP,1,1).EQ.0 ) bMistake = .true.
      if ( ICONDX(IPCOMP,1,2).EQ.0 ) bMistake = .true.
      do i=3,MPCONC
        if ( ICONDX(IPCOMP,1,i).NE.0 ) bMistake= .true.
      enddo
C-----There should be one connection to node 2 (condenser).
C      if ( ICONDX(IPCOMP,2,1).EQ.0 ) bMistake = .true.
C      do i=2,MPCONC
C        if ( ICONDX(IPCOMP,2,i).NE.0 ) bMistake= .true.
C      enddo
C-----There should be two connections to node 3 (evaporator).
      if ( ICONDX(IPCOMP,3,1).EQ.0 ) bMistake = .true.
      if ( ICONDX(IPCOMP,3,2).EQ.0 ) bMistake = .true.
      do i=3,MPCONC
        if ( ICONDX(IPCOMP,3,i).NE.0 ) bMistake = .true.
      enddo
C-----Write error message if the number of connections to the nodes are incorrect.
      if ( bMistake ) then
        WRITE(IUOUT,*) ' ADS_storage_static_template: incorrect number'
        WRITE(IUOUT,*) ' of connections for component ',IPCOMP
        STOP ' ADS_storage_static_template: unresolvable error'
      endif



C------------------------------------------------------------------------------------
C Check that the connections to nodes 1 and 3 are of the correct type. All
C connections should be of type ISV=20 so that the adsorption storage unit can be
C used in networks in which both first and second phase mass balances are formed.
C Variables used:
C    ICONTP(i)  holds the state variable index (ISV) of the sending node for connection `i'.
C    ISV        defines nodal fluid type & coefficient generator model capabilities:
C               ISV=0,10,20 node represents water + ....
C               ISV=1,11,21 node represents dry air + ....
C               ISV=9,19,29 node represents some solid material only
C               0 <=ISV<10  model suitable for energy balance only
C               10<=ISV<20  model suitable for energy + single phase mass balance
C               20<=ISV<30  model suitable for energy + two phase mass balances
C------------------------------------------------------------------------------------
      bMistake = .false.
      if ( ICONTP( ICONDX(IPCOMP,1,1) ) .NE. 20 ) bMistake = .true.
      if ( ICONTP( ICONDX(IPCOMP,1,2) ) .NE. 20 ) bMistake = .true.
      if ( bMistake ) then
        WRITE(IUOUT,*) ' ADS_storage_static_template: incorrect conn'
        WRITE(IUOUT,*) ' type to node 1 for component ',IPCOMP
        STOP ' ADS_storage_static_template: unresolvable error'
      endif
C      if ( ICONTP( ICONDX(IPCOMP,2,1) ) .NE. 20 ) bMistake = .true.
C      if ( bMistake ) then
C        WRITE(IUOUT,*) ' ADS_storage_static_template: incorrect conn'
C        WRITE(IUOUT,*) ' type to node 2 for component ',IPCOMP
C        STOP ' ADS_storage_static_template: unresolvable error'
C      endif
      if ( ICONTP( ICONDX(IPCOMP,3,1) ) .NE. 20 ) bMistake = .true.
      if ( ICONTP( ICONDX(IPCOMP,3,2) ) .NE. 20 ) bMistake = .true.
      if ( bMistake ) then
        WRITE(IUOUT,*) ' ADS_storage_static_template: incorrect conn'
        WRITE(IUOUT,*) ' type to node 3 for component ',IPCOMP
        STOP ' ADS_storage_static_template: unresolvable error'
      endif


C------------------------------------------------------------------------------------
C Assign user-specified inputs.
C------------------------------------------------------------------------------------

      fMassAdsorbent         = ADATA(IPCOMP,1)
      fCpAdsorbent           = ADATA(IPCOMP,2)
      fCoeff_Wo              = ADATA(IPCOMP,3)
      fCoeff_D               = ADATA(IPCOMP,4)
      fCoeff_n               = ADATA(IPCOMP,5)
      fMassAdsVessel         = ADATA(IPCOMP,6)
      fCpAdsVessel           = ADATA(IPCOMP,7)
      fMassAdsHeatX          = ADATA(IPCOMP,8)
      fCpAdsHeatX            = ADATA(IPCOMP,9)
      fAdsEnthalpy           = ADATA(IPCOMP,10)
      fDT_Derivative         = ADATA(IPCOMP,11)
      fSurfAreaAdsorber      = ADATA(IPCOMP,12)
      fAdsInsulL             = ADATA(IPCOMP,13)
      fAdsInsulK             = ADATA(IPCOMP,14)
      fAdsOverallHAmb        = ADATA(IPCOMP,15)
      fAdsFCHeatXeff         = ADATA(IPCOMP,16)
      fPressAdsFC            = ADATA(IPCOMP,17)
      fAdsTankHeatXeff       = ADATA(IPCOMP,18)
      fPressAdsTank          = ADATA(IPCOMP,19)
      fMinTempDiff           = ADATA(IPCOMP,20)
      fChargeMinTemp         = ADATA(IPCOMP,21)
      fCondPressure          = ADATA(IPCOMP,22)
      fCondEffec             = ADATA(IPCOMP,23)
      fMassFlowAirCond       = ADATA(IPCOMP,24)
      fSurfAreaCond          = ADATA(IPCOMP,25)
      fCondOverallHamb       = ADATA(IPCOMP,26)
      fCondFanPower          = ADATA(IPCOMP,27)
      fEvapPressure          = ADATA(IPCOMP,28)
      fEvapEffec             = ADATA(IPCOMP,29)
      fSurfAreaEvap          = ADATA(IPCOMP,30)
      fEvapOverallHamb       = ADATA(IPCOMP,31)
      fTempAdsorber_Init     = ADATA(IPCOMP,32)
      fPressAdsorber_Init    = ADATA(IPCOMP,33)

C------------------------------------------------------------------------------------
C Perform time-invariant calculations for component.
C------------------------------------------------------------------------------------

      fMassCp_Adsorbent  = fMassAdsorbent * fCpAdsorbent
      fMassCp_AdsVessel  = fMassAdsVessel * fCpAdsVessel
      fMassCp_AdsHeatX   = fMassAdsHeatX * fCpAdsHeatX
      fRtotal_Adsorber   = fAdsInsulL/fAdsInsulK + 1./fAdsOverallHamb


C------------------------------------------------------------------------------------
C Set initial conditions of adsorber (node 1).
C------------------------------------------------------------------------------------


      fPressAdsorber_Present = fPressAdsorber_Init   !-present value set for first
                                                     !-iteration of coefficient generator


      fX_Initial = ADS_mass_ratio( fPressAdsorber_Init,    !-initial adsorbate to
     &                             fTempAdsorber_Init,     !-adsorbent mass based on
     &                             fCoeff_Wo,              !-initial temperature and
     &                             fCoeff_D,               !-pressure
     &                             fCoeff_n )




      fX_Present = fX_Initial                   !-present value set for first
                                                !-iteration of coefficient generator

      bInit = .false.             !-will be set to true in coefficient generator
                                  !-after first timestep.


      iPhaseCycle = 0             !- set initial phase of cycle to unknown

C------------------------------------------------------------------------------------
C Completion of time-invariant operations for component.
C------------------------------------------------------------------------------------


C------------------------------------------------------------------------------------
C End of subroutine: ADS_stg_static_temp
C------------------------------------------------------------------------------------

       RETURN
       END





C------------------------------------------------------------------------------------
C------------------------------ADS_stg_coeff_gen-------------------------------------
C Created by: Maria Mottillo
C Initial creation date: November 14, 2004

C This subroutine is the coefficient generated created for the component
C model of an adsorption storage unit. The model is structured to
C support any adsorbent (whose characteristics are user inputs) and
C a water adsorbate. This subroutine performs the time-step calculations
C and establishes the matrix coefficients for the sub-matrices that
C define the energy, 1st phase mass flow, and 2nd phase mass
C flow balances on the nodes representing the adsorption storage
C unit. A fully explicit form of the energy  balances is formed for
C nodes 2 and 3 since transients in the condenser and evaporator, respectively,
C are ignored. A weighted implicit-explicit energy balance is formed for node 1.

C The adsorption storage unit component is represented with 3 nodes:
C node 1 represents the adsorber.
C node 2 represents the condenser.
C node 3 represents the evaporator.

C Node 1 is configured to be connected to a fuel cell and a
C water storage tank. The former connection is active when the storage
C unit is in charging mode, while the latter is active when the storage
C unit is in discharge mode. Node 2, which is only active during the
C charging mode, is configured to be connected to a reservoir
C that stores the adsorbate in liquid form. Node 3, which is active
C during the discharge of the adsorption storage unit, is configured
C to be connected to the reservoir of liquid adsorbate and to an
C auxiliary water tank that provides low-temperature heat.


C INPUTS:
C
C    fMassAdsorbent:    Mass of adsorbent contained in the adsorber (kg).
C    fCpAdsorbent:      Specific heat of adsorbent contained in adsorber (J/kgK).
C    fCoeff_Wo:         Coefficient Wo of Dubinin-Astakhov (D-A) adsorption equilibrium
C                       equation. Wo represents the volume of the adsorption space,
C                       a charateristic of the adsorbent (m3/kg adsorbent).
C    fCoeff_D:          Coefficient D of D-A adsorption equilibrium equation. D is a
C                       characteristic of the adsorbent/adsorbate pair (-).
C    fCoeff_n:          Coefficient n of D-A adsorption equilibrium equation. n is a
C                       characteristic of the adsorbent/adsorbate pair (-).
C    fMassAdsVessel:    Mass of the adsorber vessel (kg).
C    fCpAdsVessel:      Specific heat of adsorber vessel material (J/kgK).
C    fMassAdsHeatX:     Mass of heat exchanger within the adsorber (kg).
C    fCpAdsHeatX:       Specific heat of heat exchanger material within the adsorber (J/kgK).
C    fAdsEnthalpy:      Enthalpy of adsorption (assumed constant) (J/kg);
C    fDT_Derivative:    Delta T used to obtain dX/dT derivative (oC).
C    fSurfAreaAdsorber: Surface area of adsorber (m2).
C    fAdsInsulL:        Thickness of adsorber insulation layer (m).
C    fAdsInsulK:        Thermal conductivity of adsorber insulation (W/mK).
C    fAdsOverallHAmb:   Overall heat transfer coefficient adsorber-surroundings (W/m2K).
C    fAdsFCHeatXeff:    Effectiveness of heat exchanger adsorber-fuel cell (-).
C    fPressAdsFC:       Pressure of adsorber-fuel cell heat exchange loop (kPa).
C    fAdsTankHeatXeff:  Effectiveness of heat exchanger adsorber-main water storage tank (-).
C    fPressAdsTank:     Pressure of adsorber-main water storage tank heat exchange loop (kPa).
C    fMinTempDiff:      Minimum temperature difference between adsorber and heat source or
C                       heat sink. This is used to determine end of charging/discharge phases,
C                       respectively (oC).
C    fCondPressure:     Condenser pressure (kPa).
C    fCondEffec:        Condenser heat exchanger effectiveness (-).
C    fMassFlowAirCond:  Mass flow rate of air entering condenser (kg/s).
C    fSurfAreaCond:     Surface area of condenser (m2).
C    fCondOverallHamb:  Overall heat transfer coefficient condenser-surroundings (W/m2K).
C    fCondFanPower:     Condenser fan power rating (W).
C    fEvapPressure:     Evaporator pressure (kPa).
C    fEvapEffec:        Evaporator heat exchanger effectiveness (-).
C    fSurfAreaEvap:     Surface area of evaporator (m2).
C    fEvapOverallHamb:  Overall heat transfer coefficient evaporator-surroundings (W/m2K).
C    PCNTMP(i)          Present value of the containment temperature of plant component
C                       'i' (common/PCVAR)
C    PCNTMF(i)          Future value of the containment temperature of plant component
C                       `i' (common/PCVAR)
C    IMPEXP             Index identifying the implicit/explicit handling of plant
C                       equations (common/PCEQU)
C    RATIMP             User-specified implicit weighting fraction (common/PCEQU)
C    TIMSEC             Length of time-step in explicit plant domain (seconds) (common/PCTIME)
C    CDATA(i,j)         Cntrol signal for component `i', node `j' (common/C9)
C    IUOUT              Output channel to feedback to user
C    ITC                Trace output index (>0 gives a trace output)
C    ITU                Output channel for trace output
C    ITRACE(37)         Flag indicating one of the plant traces (1= active)
C    idyp               Number of present day (0 to 365) (in COMMON/SIMTIM)
C    ptimef             'Future' time of current day in hour fraction (0. to 24.) for
C                       current plant simulation increment (in COMMON/ptime).
C    TF                 Future value of outdoor temperature in oC (in common/CLIMI).

C OUTPUTS:
C
C    COUT(i)             coefficient for ISTATS balance for `i'th matrix position
C    NAPDAT(i)           number of "additional" output for component `i'
C    PCAOUT(i,j)         additional output `j' for component `i'
C    TC(i)               time constant (seconds) of plant component `i'
C
C------------------------------------------------------------------------------------



       SUBROUTINE ADS_stg_coeff_gen(IPCOMP,COUT,ISTATS)
       use h3kmodule
       IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "ADS_storage.h"


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
       REAL    PERFLX          !-maximum allowed absolute error
                               !-heat flux for plant solution (W)
       REAL    PERMFL          !- maximum allowed absolution error for
                               !- mass flow plant solution
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


       COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
       REAL    QFP             !-diffuse horizontal radiation, present hour
       REAL    QFF             !-diffuse horizontal radiation, future hour
       REAL    TP              !-external temperature, present hour
       REAL    TF              !-external temperature, future hour
       REAL    QDP             !-direct normal radiation, present hour
       REAL    QDF             !-direct normal radiation, future hour
       REAL    VP              !-wind velocity, present hour
       REAL    VF              !-wind velocity, future hour
       REAL    DP              !-wind direction, present hour
       REAL    DF              !-wind direction, future hour
       REAL    HP              !-relative humidity, present hour
       REAL    HF              !-relative humidity, future hour


       COMMON/pltcon/bpltcon,lastTStp
       LOGICAL bpltcon                  !-logical variable indicating if plant domain
       REAL    lastTStp                 !-converged in previous iteration

       common/pcnam/pcname(mpcom)       ! Plant component names
       character*15 pcname


C------------------------------------------------------------------------------------
C COMMON holding values indicating phase of cycle
C------------------------------------------------------------------------------------

       COMMON/ADS_storage_4/iPhaseCycle


       INTEGER    iPhaseCycle
       INTEGER    iIsotericHtg
       INTEGER    iDesorption
       INTEGER    iIsotericClg
       INTEGER    iAdsorption
       INTEGER    iStorage
       PARAMETER( iIsotericHtg = 1,
     &            iDesorption  = 2,
     &            iIsotericClg = 3,
     &            iAdsorption  = 4,
     &            iStorage     = 5 )


C------------------------------------------------------------------------------------
C Local variables
C------------------------------------------------------------------------------------
       INTEGER    IPCOMP,ISTATS
       LOGICAL    bCharging, bDischarge, bStorage          !-flags indicating mode of operation
       LOGICAL    CLOSE
       REAL       COUT(MPCOE)
       INTEGER    node1, node2, node3                      !-indices to nodes
       INTEGER    con1a, con1b, con2, con3a, con3b         !-indices to connections
       REAL       fTemp_con1a, fTemp_con1b, fTemp_con3a    !-temperatures of sending connections
       REAL       fTemp_con3b, fTemp_con3b_K
       REAL       fTemp_con1a_present, fTemp_con1b_present !-present values of temperature for connections to node 1

       REAL       fTamb_present, fTamb_future              !-containment temperatures
       REAL       fMdot_con1a_future, fMdot_con1b_future   !-future value of mass flow rates
       REAL       fMdot_con3a                              !-mass flow rates
       REAL       fCp_con1a, fCp_con1b, fCp_con3a          !-specific heat capacity
       REAL       fMdotCp_con1a_future, fMdotCp_con1b_future    !-product of mass flow and specific heat, future values
       REAL       fMdotCp_con1a_present, fMdotCp_con1b_present  !-product of mass flow and specific heat, present values
       REAL       fMdotCp_con3a                            !-product of mass and specific heat
       REAL       fPressAdsFC_Pa, fPressAdsTank_Pa         !-pressure in Pa
       REAL       fTemp_node1_present, fTemp_node1_future  !-present and future temperature values of node 1, oC
       REAL       fTemp_node2, fTemp_node3      !-temperature values of nodes 2 and 3, respectively, oC
       REAL       fTemp_node2_K, fTemp_node3_K  !-temperature values of nodes 2 and 3, respectively, K
       REAL       R_gas                    !-universal gas constant
       PARAMETER ( R_gas = 8.3144 )        !-(kJ/kmolK)

       REAL       fH2O_molW       !-molecular weight of water (kg/kmol)
       PARAMETER ( fH2O_molW = 18.0155 )
       REAL       H20LiquidCpCel  !-function to obtain Cp of water; Cp=f(P,T)
       REAL       SHTFLD          !-function to calculate Cp of water; Cp=f(T)
       REAL       fVapPressureAdsorber, fTemp_node1_K
       REAL       fdX_dT          !-partial pressure of X with respect to temperature T
                                  !-at constant pressure P.
       REAL       Tplus, Tminus, X_Tplus, X_Tminus
       REAL       fCp_water, fTemp_outdoor, fCp_air, fMdotCp_air
       REAL       fMassCp_effective, Rinv, alpha
       REAL       ADS_mass_ratio      !- function to calculate X(P,T)
       REAL       H2OPSAT             !- function to calculate saturated vapour pressure
                                      !- using Clausius-Clapeyron equation
       REAL       CpVapour            !- function to approximate specific heat of
                                      !- saturated vapour wrt reference temperature (kJ/kgK)
       REAL       CpLiquid            !- function to approximate specific heat of
                                      !- saturated liquid wrt reference temperature (kJ/kgK)
       REAL       fCpVap, fCpLiq
       REAL       fTemp_ref           !- reference temperature for evaluation of Cp, oC
       PARAMETER( fTemp_ref = 25.)
       REAL       fHvap_ref           !- enthalpy of saturated water vapour at reference temperature = 25oC
       PARAMETER( fHvap_ref = 2546470. )   !- J/kg
       REAL       fHliq_ref           !- enthalpy of saturated liquid water at reference temperature = 25oC
       PARAMETER( fHliq_ref = 104830. )    !- J/kg
       REAL       part1, part2, part3, part4, part5, part6
       REAL       fTemp_Adsorber_Max, fTemp_Adsorber_Min
       REAL       fQElecCondFan       !-power draw of condenser fan, W.
       REAL       fPress_Pa
       LOGICAL    IsInLiquidPhase    !-function to determine whether water is in liquid or vapour phase.
       REAL       H20GasCpCel, H20GasCp   !-functions to calculate specific heat of water vapour.
       REAL       CpWater            !-function to calculate specific heat of liquid water using
                                     !-Shomate correlation (kJ/kgK)
       REAL       TSOLVE             !-function to calculate temperature at which adsorber
                                     !-changes phase from isoteric heating to desorption (charging)
                                     !-and from isoteric cooling to adsorption (discharge)
       REAL       fTemp_switch       !-variable assigned to solution TSOLVE (oC)
       REAL       PSOLVE             !-function to calculate pressure during isoteric heating
                                     !-and isoteric cooling phases (kPa)
       LOGICAL    bSuccess
       REAL       fAccuracy          !-required accuracy of solution for functions TSOLVE and PSOLVE
       PARAMETER( fAccuracy = 0.0001 )
       INTEGER    i                  !-counter
       INTEGER    itemp

       REAL       fTemp_cond_out     !- temperature of air leaving condenser (oC)
       REAL       fTemp_evap_out     !- temperature of water leaving evaporator (oC)

       REAL       H3K_Connect_property  !- function used to return temperature of most recently
                                        !- calculated temperature (oC)
       REAL       fAdsQloss          !- heat loss from adsorber to ambient (W)

       real       fCondQloss         !- heat loss from condenser to ambient (W)

       real       fEvapQloss         !- heat loss from evaporator to ambient (W)

       REAL       fAdsorptionHeat    !- W
       REAL       fChHeatTransfer    !- heat transferred between fuel cell and adsorber (charging), (W)
       REAL       fDisHeatTransfer   !- heat transferred between tank and adsorber (discharge), (W)

       integer    iNameLength        ! temporary variable for storing string length

       real       fCondHeat          !- heat rejected from condenser to outdoors (W)
       real       fEvapHeat          !- heat input to evaporator from aux heat source (W)

       real       fPhaseCycle        !- phase of cycle in real number format, required for XML reporting

       integer    lnblnk             ! implicit function returning the length of a string


C------------------------------------------------------------------------------------
C Start trace if trace output requested.
C------------------------------------------------------------------------------------
      if ( ITC.GT.0 .AND. NSINC.GE.ITC .AND.
     &     NSINC.LE.ITCF .AND. ITRACE(37).NE.0 ) then
         WRITE(ITU,*) ' Entering sub ADS_stg_coeff_gen '
      endif


C------------------------------------------------------------------------------------
C Set local variables to point to the nodes and to the connections.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C                   within the global plant matrix.
C    node1          global matrix node number for component node 1
C    node2          global matrix node number for component node 2
C    node3          global matrix node number for component node 3
C    con1a          number of first connection to node 1 (to fuel cell)
C    con1b          number of second connection to node 1 (to water tank)
C    con2           number of connection to  node 2 (to reservoir)
C    con3a          number of first connection to node 3 (to auxiliary water tank)
C    con3b          number of second connection to node 3 (to reservoir)
C----------------------------------------------------------------------------------     --
      node1 = NPCDAT(IPCOMP,9)
      node2 = NPCDAT(IPCOMP,9)+1
      node3 = NPCDAT(IPCOMP,9)+2
      con1a = ICONDX(IPCOMP,1,1)
      con1b = ICONDX(IPCOMP,1,2)
      con2  = ICONDX(IPCOMP,2,1)
      con3a = ICONDX(IPCOMP,3,1)
      con3b = ICONDX(IPCOMP,3,2)


C------------------------------------------------------------------------------------
C Mark the temperatures of all three nodes for iteration.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C------------------------------------------------------------------------------------

      ICSV(node1,1) = 1
      CSVI(node1,1) = CSVF(node1,1)
      ICSV(node2,1) = 1
      CSVI(node2,1) = CSVF(node2,1)
      ICSV(node3,1) = 1
      CSVI(node3,1) = CSVF(node3,1)


C------------------------------------------------------------------------------------
C Mark the 1st phase mass flow rates of all three nodes for iteration.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C------------------------------------------------------------------------------------

C      ICSV(node1,2) = 1
C      CSVI(node1,2) = CSVF(node1,2)
C      ICSV(node2,2) = 1
C      CSVI(node2,2) = CSVF(node2,2)
C      ICSV(node3,2) = 1
C      CSVI(node3,2) = CSVF(node3,2)


C------------------------------------------------------------------------------------
C The adsorption storage unit has three possible modes of operation:
C (1) charging mode
C (2) discharge mode
C (3) storage mode.
C In the latter case, essentially the unit is off.
C The adsorption storage unit is in charging mode when the temperature in the main
C water storage tank is above a maximum setpoint temperature. The adsorption storage
C unit is in discharge mode when the temperature in the main water storage tank is
C below a minimum setpoint temperature.
C A MORE SOPHISTICATED CONTROL SHOULD BE IMPLEMENTED TO AVOID THE STORAGE UNIT
C GOING INTO DISCHARGE MODE WHEN THE FUEL CELL MAY OPERATE SOON (WILL CAUSE HEAT
C DUMP).
C
C WHEN ONLY RUNNING SIMULATION OF COMPONENT,
C CODE PERTAINING TO CDATA IS COMMENTED OUT.
C------------------------------------------------------------------------------------

C Determine the mode of operation for this time-step.
C bCharging = TRUE: indicates that the adsorption storage unit is in charging mode.
         CALL ECLOSE( CDATA(IPCOMP,1),1.,0.0001,bCharging )
C bDischarge = TRUE: indicates that the adsorption storage unit is in discharge mode.
         CALL ECLOSE( CDATA(IPCOMP,2),1.,0.0001,bDischarge)
C If the adsorption storage unit is not in charging or discharge mode, then
C it is in storage mode.

C for testing of plant component model
C charging phase testing
C         bCharging = .TRUE.
C discharge phase testing
C         bDischarge = .TRUE.

C for testing when unit goes into storage mode, do not want to reset to charging/discharge mode
C         if ( iPhaseCycle .EQ. iStorage ) then
C            bCharging = .FALSE.
C            bDischarge = .FALSE.
C         endif

C          bCharging = .FALSE.

C         bDischarge = .FALSE.
C end code for testing.

         if ( .NOT.bCharging .AND. .NOT.bDischarge ) then
             bStorage = .TRUE.
         else
             bStorage = .FALSE.
         endif

C-----------------------------------------------------------------------------------
C NOTE: THE WAY THE CODE IS SET UP WILL HAVE TO ENSURE THAT THE  FIRST CONNECTION
C TO THE ADSORBER IS TO HEAT SOURCE (IE FUEL CELL). MAY WANT TO ADD ERROR
C TRAPPING OR MODIFY CODE TO ELIMINATE THIS RESTRICTION.

C Variables used:
C    CONVAR(i,1)       temperature (oC) of sending node for connection `i'
C-----------------------------------------------------------------------------------
      fTemp_con1a = CONVAR(con1a,1)


C-----------------------------------------------------------------------------------
C Generate coefficients for the appropriate equation set. This subroutine is
C called to either generate coefficients for energy balances (ISTATS=1), 1st
C phase mass balances (ISTATS=2), or 2nd phase mass balances (ISTATS=3).
C Start with the energy balances.
C-----------------------------------------------------------------------------------

C*********************************************************************************
C Beginning of energy balance section.
C*********************************************************************************

      if ( ISTATS.EQ.1 ) then

C-----------------------------------------------------------------------------------
C Establish the present and future time-row temperatures of the adsorber (node 1).
C Variables used:
C     CSVP(i,j)  present time-row solution variable for node `i' and variable `j';
C                j=1 for temperature
C     CSVF(i,j)  future time-row solution variable (same indices as with CSVP)
C-----------------------------------------------------------------------------------

       if ( .not.bInit  ) then

C Set initial temperature of adsorber node

         CSVP(node1,1) = fTempAdsorber_Init
         CSVF(node1,1) = fTempAdsorber_Init
                                                         !-at first timestep
         bInit = .true.

       endif


       fTemp_node1_present = CSVP(node1,1)
       fTemp_node1_future  = CSVF(node1,1)


C If unit just switched into storage mode,
C recalculate X with latest temperature value.


      if ( bStorage ) then



         if ( iPhaseCycle .eq. iDesorption ) then

            fX_Present = ADS_mass_ratio( fCondPressure,
     &                                   fTemp_node1_present,
     &                                   fCoeff_Wo,
     &                                   fCoeff_D,
     &                                   fCoeff_n )




         elseif ( iPhaseCycle .eq. iAdsorption ) then


            fX_Present = ADS_mass_ratio( fEvapPressure,
     &                                   fTemp_node1_present,
     &                                   fCoeff_Wo,
     &                                   fCoeff_D,
     &                                   fCoeff_n )


         endif




      endif


C-----------------------------------------------------------------------------------
C If the adsorption storage unit is in charging mode, the maximum temperature
C of the adsorber to ensure proper heat transfer with the heat source is
C the temperature of the heat source minus the minimum temperature difference
C specified by the user. Similarly, during the discharge phase, the minimum
C temperature of the adsorber to ensure proper heat transfer with the heat
C sink is the temperature of the heat sink plus the minimum temperature difference.
C The adsorption storage unit is set to storage mode if the adsorber is greater
C than or equal to the maximum temperature or if the adsorber is less than or
C equal to the minimum temperature (this is done by the controller).
C NOTE: THE WAY THE CODE IS SET UP, THE FIRST CONNECTION TO THE ADSORBER NODE
C MUST BE TO THE HEAT SOURCE. THE SECOND CONNECTION TO THE ADSORBER NODE IS
C TO THE HEAT SINK.
C-----------------------------------------------------------------------------------


           fTemp_con1b = CONVAR(con1b,1)  !-oC, temperature of heat sink

           fTemp_Adsorber_Max = fTemp_con1a - fMinTempDiff

           fTemp_Adsorber_Min = fTemp_con1b + fMinTempDiff

C-----------------------------------------------------------------------------------
C Determine the `present' and `future' temperature of the room containing the
C adsorption storage unit. This is used to calculate the heat lost from the
C unit to the room during the charging, discharge and storage phases.
C Variables used:
C    PCNTMP(i)  present time-row temperature of room containing component `i'.
C    PCNTMF(i)  future time-row temperature of room containing component `i';
C               =-99 if no containment defined.
C-----------------------------------------------------------------------------------
        CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
        if ( CLOSE ) then
          WRITE(IUOUT,*) ' ADS_stg_coeff_gen: '
          WRITE(IUOUT,*) ' the adsorption storage unit  must be '
          WRITE(IUOUT,*) ' contained within a room.'
          STOP ' ADS_stg_coeff_gen: unresolvable error'
        else
          fTamb_present = PCNTMP(IPCOMP)
          fTamb_future  = PCNTMF(IPCOMP)
        endif


C-----------------------------------------------------------------------------------
C Determine the products of mass flow and heat capacity (W/K) for the connections
c to nodes 1. These are the heat capacity rates (W/K) of:
C 1) the pressurized water to the adsorber (node 1) from the fuel cell's
C    heat exchanger (during charging mode)
C 2) the pressurized water from the adsorber to the main water
C    storage tank (discharge mode)
C
C Variables used:
C    CONVAR(i,1)          Temperature (oC) of sending node for connection `i'
C    CONVAR(i,2)          Water mass flow rate (kg/s) at sending node for
C                         connection `i'
C    PCONDR(i)            Ratio of flow rate leaving sending node `i' that
C                         reaches receiving node.
C    H20LiquidCpCel(T, P) Function that returns heat capacity (J/kgK) of liquid
C                         water at temperature `T' (oC) and pressure 'P' (Pa).
C                         This function consumes inordinate cpu resources but
C                         is required here because it can calculate the heat
C                         capacity of pressurized water.  Another function,
C                         'SHTFLD', is much more efficient and should be used
C                         in cases where the liquid water is not pressurized.
C    SHTFLD(3,T)          Function that returns heat capacity (J/kgK) of water (index 3)
C                         at temperature `T'.
C    fPressAdsFC          Pressure of adsorber-fuel cell h/x loop (kPa) (user input)
C    fPressAdsTank        Pressure of adsorber-water tank h/x loop (kPa) (user input)
C    PCRP(i)              present time-row mass flow * heat capacity for connection `i'
C    PCRF(i)              future time-row mass flow * heat capacity for connection `i'
C    PCRP and PCRF are standard ESP-r variables holding the present and future
C    time-row values of the mass flow * heat capacity. PCRF is mapped to PCRP by
C    subroutine MZNASS following the solution of the energy  matrix for the current
C    time-step.
C-----------------------------------------------------------------------------------

C NOTE- THE WAY THE CODE IS SET UP THE FIRST CONNECTION TO THE ADSORBER (NODE 1)
C MUST BE TO THE HEAT SOURCE (FUEL CELL) AND THE SECOND CONNECTION MUST BE
C TO THE HEAT SINK (WATER TANK).


        if ( bCharging ) then

           fMdot_con1a_future = PCONDR(con1a) * CONVAR(con1a,2)    !-mass flow rate of water from heat source
                                                                   !-(fuel cell)

           fPressAdsFC_Pa = fPressAdsFC * 1000.                    !-convert from kPa to Pa

           fCp_con1a = H20LiquidCpCel( fTemp_con1a,fPressAdsFC_Pa )

           fMdotCp_con1a_future  = fMdot_con1a_future * fCp_con1a  !-W/K

           fMdotCp_con1b_future  = 0.                              !-connection to heat sink not active


        elseif ( bDischarge ) then

           fMdot_con1b_future = PCONDR(con1b) * CONVAR(con1b,2)    !-mass flow rate of water from heat sink
                                                                   !-(main water storage tank)
           fPressAdsTank_Pa = fPressAdsTank * 1000.                !-convert to Pa

           fCp_con1b = H20LiquidCpCel( fTemp_con1b,fPressAdsTank_Pa )

           fMdotCp_con1b_future = fMdot_con1b_future * fCp_con1b   !-W/K

           fMdotCp_con1a_future = 0.                               !-connection to heat source not active


        elseif ( bStorage ) then

           fMdotCp_con1a_future = 0.      !-connection to heat source (fuel cell) not active
           fMdotCp_con1b_future = 0.      !-connection to heat sink (main water tank) not active

        endif


C-------Save future values.
        PCRF(con1a) = fMdotCp_con1a_future
        PCRF(con1b) = fMdotCp_con1b_future

C-------Set present values.
        fMdotCp_con1a_present = PCRP(con1a)
        fMdotCp_con1b_present = PCRP(con1b)


C------ Save future values of temperature
        PCTF(con1a) = H3K_Connect_property(con1a,1)
        PCTF(con1b) = H3K_Connect_property(con1b,1)


C-----------------------------------------------------------------------------------
C Establish the present time-row temperatures of the pressurized water flowing into
C the adsorber h/x at the two connections.  PCTP and PCTF are standard
C ESP-r variables holding the present and future time-row values of the temperature
C of the fluid flowing at the connection. PCTF is mapped to PCTP by subroutine
C MZNASS following the solution of the energy matrix for the current time-step.
C Variables used:
C    PCTP(i)     present temperature of sending node for connection `i'
C    PCTF(i)     future temperature of sending node for connection `i'
C-----------------------------------------------------------------------------------
        fTemp_con1a_present = PCTP(con1a)
        fTemp_con1b_present = PCTP(con1b)



C---------------------------------------------------------------------------------
C Establish the temperatures of the node 2 (condenser) and node 3 (evaporator)
C by taking their state at the previous iteration (this may be the previous
C time-step, depending on whether iteration is required to converge the
C plant matrix). These values are not used to form the energy balances below.
C Rather, they are used to calculate the specific heat of the liquid leaving
C the condenser or the specific heat of the vapour leaving the evaporator.
C Variables used
C     CSVF(i,j)  future time-row solution variable for node `i' and variable `j';
C                j=1 for temperature
C---------------------------------------------------------------------------------
        fTemp_node2 = CSVF(node2,1)           !- oC
        fTemp_node2_K = fTemp_node2 + 273.1   !- K
        fTemp_node3 = CSVF(node3,1)           !- oC
        fTemp_node3_K = fTemp_node3 + 273.1   !- K



C-----------------------------------------------------------------------------------
C Determine the phase of the cycle and set flags. Phases are:
C - isoteric heating (charging mode)
C - isobaric desorption (charging mode)
C - isoteric cooling (discharge mode)
C - isobaric adsorption (discharge mode)
C If the unit is in the isoteric heating or cooling phase, calculate pressure using
C the D-A equation (pressure found iteratively).
C--------------------------------------------------------------------


        if ( bCharging ) then


           if ( iPhaseCycle .EQ. 0 .OR.
     &          iPhaseCycle .EQ. iStorage ) then       !-phase of the cycle not yet found
                                                       !-or previous phase of cycle was storage

C Find the temperature for which the adsorber switches from the isoteric heating
C to the desorption phase.

C First make sure that a value exists for T  for which
C X = fX_present and P = fCondPressure

                CALL TCheck( fCoeff_Wo,
     &                       fCoeff_D,
     &                       fCoeff_n,
     &                       fX_Present,
     &                       fCondPressure,
     &                       40.0,
     &                       200.0,
     &                       bSuccess )

                if ( bSuccess ) then         !-value exists within acceptable range

C Find the temperature iteratively using the D-A
C equilibrium equation (function TSOLVE).

                    fTemp_switch = TSOLVE( fCoeff_Wo,
     &                                    fCoeff_D,
     &                                    fCoeff_n,
     &                                    fX_Present,
     &                                    fCondPressure,
     &                                    fTemp_node1_present,
     &                                    200.0,
     &                                    fAccuracy )

                else

                      write(iuout,*) 'fTemp_switch not found'

                endif

           endif



C Determine phase of cycle

           if ( fTemp_node1_present .LT. fTemp_switch ) then


                     iPhaseCycle = iIsotericHtg

C Calculate pressure

                     fVapPressureAdsorber = PSOLVE( fCoeff_Wo,
     &                                             fCoeff_D,
     &                                             fCoeff_n,
     &                                             fX_Present,
     &                                             fTemp_node1_present,
     &                                             fEvapPressure,
     &                                             fCondPressure,
     &                                             fAccuracy )


            else

                     iPhaseCycle = iDesorption
                     fVapPressureAdsorber = fCondPressure            !-assume ideal cycle


            endif




        elseif ( bDischarge ) then


            if ( iPhaseCycle .EQ. 0 .OR.
     &           iPhaseCycle .EQ. iStorage ) then            !-phase of the cycle not yet found
                                                             !-or previous phase of the cycle was storage

C Find the temperature for which the adsorber switches from the
C isoteric cooling to the adsorption phase.


C First make sure that a value exists for T within the minimum
C and the present adsorber temperature, for which X = fX_present and P = fEvapPressure


                CALL TCheck( fCoeff_Wo,
     &                       fCoeff_D,
     &                       fCoeff_n,
     &                       fX_Present,
     &                       fEvapPressure,
     &                       40.0,
     &                       200.0,
     &                       bSuccess )



                if ( bSuccess ) then


                     fTemp_switch = TSolve(  fCoeff_Wo,
     &                                       fCoeff_D,
     &                                       fCoeff_n,
     &                                       fX_Present,
     &                                       fEvapPressure,
     &                                       40.0,
     &                                       fTemp_node1_present,
     &                                       fAccuracy )

                else

                     write(iuout,*) 'fTemp_switch not found'

                endif

            endif



            if ( fTemp_node1_present .GT. fTemp_switch ) then

                 iPhaseCycle = iIsotericClg

C Calculate pressure
                 fVapPressureAdsorber = PSOLVE( fCoeff_Wo,
     &                                          fCoeff_D,
     &                                          fCoeff_n,
     &                                          fX_Present,
     &                                          fTemp_node1_present,
     &                                          fEvapPressure,
     &                                          fCondPressure,
     &                                          fAccuracy )

            else

                 iPhaseCycle = iAdsorption
                 fVapPressureAdsorber = fEvapPressure       !- assume ideal cycle

            endif




        else

            iPhaseCycle = iStorage

        endif



C Save future value just calculated for next iteration
        fPressAdsorber_Present = fVapPressureAdsorber



C-----------------------------------------------------------------------------------
C Determine dX/dT at constant pressure P. This term forms part of the energy balance
C during the isobaric desorption of the vapour adsorbate (charging mode) and during
C the isobaric adsorption of the vapour adsorbate (discharge mode).
C-----------------------------------------------------------------------------------

C dX/dT is approximated by:
C dX =( X at T+0.5*fDT_Derivative,P ) - ( X at T-0.5*fDT_Derivative,P )
C dT = fDT_Derivative  (user input)

         Tplus = fTemp_node1_present + 0.5*fDT_Derivative
         Tminus = fTemp_node1_present - 0.5*fDT_Derivative


         if ( iPhaseCycle.EQ.iDesorption ) then


               X_Tplus = ADS_mass_ratio( fCondPressure,
     &                                   Tplus,
     &                                   fCoeff_Wo,
     &                                   fCoeff_D,
     &                                   fCoeff_n )

               X_Tminus = ADS_mass_ratio( fCondPressure,
     &                                    Tminus,
     &                                    fCoeff_Wo,
     &                                    fCoeff_D,
     &                                    fCoeff_n )

               fdX_dT = ( X_Tplus - X_Tminus ) / fDT_Derivative



         elseif ( iPhaseCycle .EQ.iAdsorption ) then



               X_Tplus = ADS_mass_ratio( fEvapPressure,
     &                                   Tplus,
     &                                   fCoeff_Wo,
     &                                   fCoeff_D,
     &                                   fCoeff_n )

               X_Tminus = ADS_mass_ratio( fEvapPressure,
     &                                    Tminus,
     &                                    fCoeff_Wo,
     &                                    fCoeff_D,
     &                                    fCoeff_n )

               fdX_dT = ( X_Tplus - X_Tminus ) / fDT_Derivative


         else

              fdX_dT = 0.             !-set value to zero so that source/sink term becomes zero in
                                      !-energy balance during isoteric heating, isoteric cooling
                                      !-and storage phases.
         endif


C-----------------------------------------------------------------------------------
C Determine ratio of mass of adsorbate to mass of adsorbent, X, in the adsorber
C using the D-A equation. X is a function of temperature T and pressure P.
C X is constant during the isoteric heating and isoteric cooling phases
C and therefore the value of X from the previous iteration (X_Present) remains
C unchanged.
C However, X varies during the desorption and adsorption phases, therefore
C X_Present is recalculated.
C-----------------------------------------------------------------------------------

C Save value of X from previous timestep (only if previous solution converged)
C this is required to determine dX/dt later

         if ( bpltcon ) then

             fX_Past = fX_present

         endif


         if ( iPhaseCycle .EQ. iDesorption ) then


              fX_Present = ADS_mass_ratio( fCondPressure,
     &                                     fTemp_node1_present,
     &                                     fCoeff_Wo,
     &                                     fCoeff_D,
     &                                     fCoeff_n )

         elseif ( iPhaseCycle .EQ. iAdsorption ) then


              fX_Present = ADS_mass_ratio( fEvapPressure,
     &                                     fTemp_node1_present,
     &                                     fCoeff_Wo,
     &                                     fCoeff_D,
     &                                     fCoeff_n )

         endif


C----------------------------------------------------------------------
C Determine specific heat of water in liquid phase, used to approximate
C the specific heat of the adsorbate in its adsorbed state.
C Use Shomate correlation (used in function CpWater).
C----------------------------------------------------------------------

         fTemp_node1_K = fTemp_node1_present + 273.1    !-convert to Kelvin
         fCp_water =  CpWater(fTemp_node1_K)            !-J/kgK



C---------------------------------------------------------------------------------
C Calculate the component time-constant for the current time-step.
C Refer to Chapter 5 of Hensen (1991). The environmental losses from the adsorber
C and the water streams flowing into the adsorber from the fuel cell and the
C water tank  are the three heat transfers that affect the transient storage.
C
C NOTE- TRANSIENT TERM ONLY APPLIES TO ADSORBER (NODE 1)
C
C QUESTION: DOES THE SOURCE TERM IN THE ADSORBER ENERGY BALANCE AFFECT THE TIME
C CONSTANT? DISREGARD THE INFLUENCE OF THE SOURCE TERM FOR NOW.
C
C Variables used:
C     TC(i)        time constant (seconds) of plant component `i'
C---------------------------------------------------------------------------------

        Rinv = ( 1. / fRtotal_Adsorber ) + fMdotCp_con1a_future +
     &            fMdotCp_con1b_future                                 !-W/K
        CALL ECLOSE(Rinv,0.,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(IUOUT,*) ' ADS_storage_coeff_gen: '
          WRITE(IUOUT,*) ' impossible time constant. '
          STOP ' ADS_storage_coeff_gen: unresolvable error'
        END IF

        fMassCp_effective = fMassAdsorbent * fCpAdsorbent +
     &                      fMassAdsorbent * fX_Present * fCp_water +
     &                      fMassAdsVessel * fCpAdsVessel +
     &                      fMassAdsHeatX * fCpAdsHeatX                !-J/K

        TC(IPCOMP) = fMassCp_effective / Rinv                          !-s


C---------------------------------------------------------------------------------
C Determine implicit/explicit weighting for the energy balance on node 1. `alpha'
C is the implicit weighting factor used to form the energy balance this time-step.
C The approach used here to determine `alpha' is used with most plant components,
C as discussed by Hensen (1991).
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
          fMassCp_effective = 0.  ! make node 1 massless
          alpha = 1.
        END IF



C-----------------------------------------------------------------------------------
C Determine temperature of outdoor air flowing into the condenser (node 2).
C Variables used:
C TF: Future value of outdoor temperature (oC)
C-----------------------------------------------------------------------------------
        fTemp_outdoor = TF



C-----------------------------------------------------------------------------------
C Determine specific heat of air entering the condenser
C Variables used:
C SHTFLD(1,T): function that returns the heat capacity (J/kgK) of air (index 1)
C              at temperature T
C-----------------------------------------------------------------------------------

        fCp_air = SHTFLD(1,fTemp_outdoor)


C---------------------------------------------------------------------------------
C Calculate the product of mass flow and heat capacity (W/K) for the air flowing
C into the condenser (node 2).
C Condenser is only active during the desorption phase of the charging cycle.
C---------------------------------------------------------------------------------

        if ( iPhaseCycle .EQ. iDesorption ) then
           fMdotCp_air = fMassFlowAirCond * fCp_air
        else
           fMdotCp_air = 0.
        endif


C---------------------------------------------------------------------------------
C Assume that the rate of vapour desorbed from the adsorbent during the
C charging phase is equal to the mass flow rate of vapour entering the
C condenser. Similarly, assume that the rate of vapour adsorbed by the
C adsorbent during the discharge phase is equal to the mass flow rate
C of vapour leaving the evaporator.
C---------------------------------------------------------------------------------


        if ( iPhaseCycle .EQ. iDesorption .OR.
     &       iPhaseCycle .EQ. iAdsorption ) then

            fMdotVap =  fMassAdsorbent *
     &                  ABS( fX_Present - fX_Past ) / TIMSEC            !-kg/s


        else

            fMdotVap= 0.

        endif



C---------------------------------------------------------------------------------
C Determine specific heat of vapour entering the condenser from the adsorber
C (desorption phase) or the specific heat of the vapour entering the
C adsorber from the evaporator (adsorption phase).
C Specific heat is found by linearizing the enthalpy of the vapour at the
C temperature of the "sending" component (adsorber or evaporator)
C at the present time-step with respect to a reference
C temperature of 25oC (298 K). The saturation enthalpy of the vapour is
C determined using the Shomate equation (source National Institute of Standards
C and Technology (NIST) Chemistry Handbook available on-line).
C---------------------------------------------------------------------------------

        if( iPhaseCycle .EQ. iDesorption ) then

           fCpVap = CpVapour(fTemp_node1_K) * 1000.       !-J/kgK

        elseif( iPhaseCycle .EQ. iAdsorption ) then

           fCpVap = CpVapour(fTemp_node3_K) * 1000.       !-J/kgK

        else

           fCpVap = 0.

        endif

C---------------------------------------------------------------------------------
C Determine the product of mass flow and heat capacity (W/K) of the connection
C to node 3 (the evaporator). This is the heat capacity rate (W/K) of the
C water from the auxiliary water tank to the evaporator.
C Note that the way the code is set up,the first connection to the evaporator
C must be to the heat source (auxiliary water tank) and the second
C connection must be to the mass source (reservoir that contains the
C adsorbate liquid.
C The connection is only active when the adsorption unit is in the adsorption
C phase of the discharge cycle.
C
C Variables used:
C    CONVAR(i,1)        Temperature (oC) of the sending node for connection 'i'
C    CONVAR(i,2)        Water mass flow rate (kg/s) at sending node for
C                       connection 'i'
C    PCONDR(i)          Ratio of flow rate leaving sending node 'i' that
C                       reaches the receiving node.
C    SHTFLD(3,T)        Function that return the heat capacity (J/kgK) of
C                       liquid water (index 3) at temperature T (oC)
C---------------------------------------------------------------------------------


        if ( iPhaseCycle .EQ. iAdsorption ) then

           fMdot_con3a = PCONDR(con3a) * CONVAR(con3a,2)       !-mass flow rate of water from heat source
                                                               !-(i.e. auxiliary water tank) connected to evaporator
           fTemp_con3a = CONVAR(con3a,1)                       !-temperature (oC)
           fCp_con3a = SHTFLD( 3, fTemp_con3a )
           fMdotCp_con3a = fMdot_con3a * fCp_con3a

        else


           fMdotCp_con3a = 0.


        endif



C---------------------------------------------------------------------------------
C Determine the temperature of the liquid adsorbate flowing from the
C reservoir to the evaporator. This temperature is used to determine
C the enthalpy of the liquid so that an approximate value of specific
C heat can be determined.
C Variables used:
C    CONVAR(i,1) temperature (oC) of sending node for connection `i'
C---------------------------------------------------------------------------------
        fTemp_con3b = CONVAR(con3b,1)
        fTemp_con3b_K = fTemp_con3b + 273.1   !- K


C---------------------------------------------------------------------------------
C Determine specific heat of liquid water leaving the condenser (desorption phase)
C or the specific heat of the liquid water entering the evaporator (adsorption
C phase). The specific heat is found by linearizing the enthalpy of the liquid
C at the temperature of the "sending" component with respect to a reference
C temperature of 25oC (298 K). The saturation enthalpy of the liquid is
C determined using the Shomate equation (source National Institute of Standards
C and Technology (NIST) Chemistry Handbook available on-line).
C---------------------------------------------------------------------------------

        if( iPhaseCycle .EQ. iDesorption ) then

           fCpLiq = CpLiquid(fTemp_node2_K) * 1000.   !-J/kgK

        elseif( iPhaseCycle .EQ. iAdsorption ) then

           fCpLiq = CpLiquid(fTemp_con3b_K) * 1000.   !-J/kgK

        else

           fCpLiq = 0.

        endif



C---------------------------------------------------------------------------------
C Calculate condenser fan power consumption. Note that this is not taken into
C account in the energy balance on the condenser but is only reported as an
C additional output variable. The condenser only operates when the adsorption
C storage unit is in the desorption mode.
C---------------------------------------------------------------------------------

        if ( iPhaseCycle .EQ. iDesorption ) then

           fQElecCondFan = fCondFanPower

        else

           fQElecCondFan = 0.

        endif


C Calculate heat loss from adsorber to the ambient (using previous timestep temperature)

        fAdsQloss = ( fTemp_node1_present - fTamb_future ) /        ! -W
     &               fRtotal_Adsorber * fSurfAreaAdsorber

C Calculate heat loss from condenser to the ambient (using previous timestep temperature)

        fCondQloss = fCondOverallHamb * fSurfAreaCond *
     &               ( fTemp_node2 - fTamb_future )                 !- W

C Calculate heat loss from evaporator to the ambient (using previous timestep temperature)

        fEvapQloss = fEvapOverallHamb * fSurfAreaEvap *
     &               ( fTemp_node3 - fTamb_future )                 !- W



C Calculate the adsorption heat source or desorption heat sink term

        fAdsorptionHeat = fMdotVap * fAdsEnthalpy


C Calculate the heat transferred from the fuel cell (heat source) during
C the charging process and the heat transferred to the hot water tank
C (heat sink) during the discharge process


       fChHeatTransfer = fAdsFCHeatXeff * fMdotCp_con1a_future *
     &                     ( fTemp_con1a - fTemp_node1_future )



       fDisHeatTransfer = fAdsTankHeatXeff * fMdotCp_con1b_future *
     &                     ( fTemp_node1_future - fTemp_con1b )



C Calculate heat rejected by condenser

        fCondHeat = fCondEffec * fMdotCp_air *
     &              ( fTemp_node2 - fTemp_outdoor )

C Calculate heat input at evaporator

        fEvapHeat = fEvapEffec * fMdotCp_con3a *
     &              ( fTemp_con3a - fTemp_node3 )


C---------------------------------------------------------------------------------
C Preparation for forming matrix coefficients for energy balances is now complete.
C Establish matrix equation self-coupling, cross-coupling, and RHS coefficients.
C Node coefficient transport for COUT:
C       <--self-->|<cross>
C  node   1  2  3 |  m  n  o  p     RHS
C       -------------------------------
C         1  0  0 |  5  6  0  0       9
C         2  3  0 |  0  0  0  0   =  10
C         0  0  4 |  0  0  7  8      11
C---------------------------------------------------------------------------------

C Note that the terms that do not apply for the particular phase of the cycle
C are set to zero in preceding code.


C Node 1 energy balance
        part1 = 1./TIMSEC * ( fMassCp_effective -
     &                    ( fMassAdsorbent * fdX_dT * fAdsEnthalpy ) )
        part2 = fSurfAreaAdsorber / fRtotal_Adsorber
        part3 = fAdsFCHeatXeff * fMdotCp_con1a_future
        part4 = fAdsTankHeatXeff * fMdotCp_con1b_future
        part5 = fAdsFCHeatXeff * fMdotCp_con1a_present
        part6 = fAdsTankHeatXeff * fMdotCp_con1b_present
        COUT(1) = part1 +
     &            alpha * part2 +
     &            alpha * part3 +
     &            alpha * part4
        COUT(5) = -1. * alpha * part3
        COUT(6) = -1. * alpha * part4

        COUT(9) = ( part1 +
     &            (-1.) * (1. - alpha) * part2 +
     &            (-1.) * (1. - alpha) * part5 +
     &            (-1.) * (1. - alpha) * part6  ) *  fTemp_node1_present
     &            + (1. - alpha) * part5 * fTemp_con1a_present +
     &            (1. - alpha) * part6 * fTemp_con1b_present +
     &            (1. - alpha) * part2 * fTamb_present +
     &            alpha * part2 * fTamb_future


C Node 2 energy balance
C Only applicable during desorption phase
        if ( iPhaseCycle .EQ. iDesorption ) then
           COUT(2)  = fMdotVap * fCpVap
           COUT(3)  = -1. * ( fCondEffec * fMdotCp_air +
     &                        fCondOverallHamb * fSurfAreaCond +
     &                        fMdotVap * fCpLiq )
           COUT(10) = fMdotVap * ( fCpVap * fTemp_ref - fHvap_ref ) +
     &                fMdotVap * ( fHliq_ref - fCpLiq * fTemp_ref ) +
     &              (-1.) * ( fCondEffec * fMdotCp_air * fTemp_outdoor )
     &              + (-1.) * ( fCondOverallHamb * fSurfAreaCond *
     &                                               fTamb_future )
       else
          COUT(2) = 0.
          COUT(3) = 1.
          COUT(10) = fTamb_future        ! energy balance leads to qloss = 0
       endif


C Node 3 energy balance
C Only applicable during adsorption phase
        if ( iPhaseCycle .EQ. iAdsorption ) then
            COUT(4)  = -1. * ( fEvapEffec * fMdotCp_con3a +
     &                     fMdotVap * fCpVap +
     &                     fEvapOverallHamb * fSurfAreaEvap )
            COUT(7)  = fEvapEffec * fMdotCp_con3a
            COUT(8)  = fMdotVap * fCpLiq
            COUT(11) = fMdotVap * ( fHvap_ref - fCpVap * fTemp_ref ) +
     &             fMdotVap * ( fCpLiq * fTemp_ref - fHliq_ref ) +
     &             (-1.) * ( fEvapOverallHamb * fSurfAreaEvap *
     &                                               fTamb_future )
       else
           COUT(4) = 1.
           COUT(7) = 0.
           COUT(8) = 0.
           COUT(11) = fTamb_future      ! energy balance leads to qloss = 0
       endif



C*********************************************************************************
C End of energy balance section / Beginning of 1st phase mass balances ***********
C*********************************************************************************
      elseif ( ISTATS .EQ. 2 ) then
C Node 1: mass flow in equals mass flow out
C Note the mass flow rate of vapour leaving the adsorber and going to
C the condenser during the desorption phase is taken care of in
C the code above.
        COUT(1) = 1.

        if ( bCharging ) then
          COUT(5) = -1. * PCONDR(con1a) * 2.           !-connection to heat source only active
        else                                       !-during the charging phase
          COUT(5) = 0.
        endif

        if ( bDischarge ) then
          COUT(6) = -1. * PCONDR(con1b) * 2.           !-connection to heat sink only active
        else                                       !-during the discharge phase
          COUT(6) = 0.
        endif

        COUT(9) = 0.

C Node 2: condenser is a source of mass
        COUT(2) = 0.
        COUT(3) = 1.
        COUT(10)= fMdotVap                 !-value is set to zero when unit
                                           !-is not in the desorption phase

C Node 3: do not consider mass flow rate of water entering
C the evaporator from the reservoir
        COUT(4) = 1.
        if ( iPhaseCycle .EQ. iAdsorption ) then
          COUT(7) = -1. * PCONDR(con3a)        !- connection to evaporator heat source
        else                                   !- only active during adsorption phase
          COUT(7) = 0.
        endif
        COUT(8) = 0.
        COUT(11)= 0.

C*********************************************************************************
C End of energy 1st phase mass balances / Beginning of 2nd phase mass balances ***
C*********************************************************************************
      elseif ( ISTATS .EQ. 3 ) then
C Node 1: no balance required so make flow zero.
        COUT(1) = 1.
        COUT(5) = 0.
        COUT(6) = 0.
        COUT(9) = 0.
C Node 2: no balance required so make flow zero.
        COUT(2) = 0.
        COUT(3) = 1.
        COUT(10)= 0.
C Node 3: no balance required so make flow zero.
        COUT(4) = 1.
        COUT(7) = 0.
        COUT(8) = 0.
        COUT(11)= 0.



      endif
C*********************************************************************************
C End of energy and mass balances ************************************************
C*********************************************************************************


C------------------------------------------------------------------------------------
C Save the `additional' output variables for outputting on time-step basis.
C------------------------------------------------------------------------------------

      NAPDAT(IPCOMP) = 9
      PCAOUT(IPCOMP,1) = fTemp_Adsorber_Max      !-max temperature of adsorber during desorption,oC
      PCAOUT(IPCOMP,2) = fTemp_Adsorber_Min      !-min temperature of adsorber during adsorption,oC
      PCAOUT(IPCOMP,3) = fVapPressureAdsorber    !-pressure in adsorber, kPa
      PCAOUT(IPCOMP,4) = real(iPhaseCycle)       !-flag indication phase of cycle unit is in:
                                                 !- 1 = Isoteric Heating
                                                 !- 2 = Desorption
                                                 !- 3 = Isoteric Cooling
                                                 !- 4 = Adsorption
                                                 !- 5 = Storage
      PCAOUT(IPCOMP,5) = fMdotVap                !-mass flow rate of vapour,kg/s, from
                                                 !-adsorber to condenser (desorption) or
                                                 !-evaporator to adsorber (adsorption)
      PCAOUT(IPCOMP,6) = fQElecCondFan           !-power draw of condenser fan, W.
      PCAOUT(IPCOMP,7) = fAdsorptionHeat         !-heat source/sink term, W.
      PCAOUT(IPCOMP,8) = fChHeatTransfer + fDisHeatTransfer  !-heat transfer from FC or tank, W.
      PCAOUT(IPCOMP,9) = fAdsQloss               !-adsorber heat loss to the ambient, W


C------------------------------------------------------------------------------------
C Complete trace if trace output requested.
C------------------------------------------------------------------------------------

      if ( (ITC.GT.0) .AND. (NSINC.GE.ITC) .AND. (NSINC.LE.ITCF)
     &      .AND. (ITRACE(37).NE.0) ) then

          WRITE(ITU,*) 'Component     ', IPCOMP,':'
          WRITE(ITU,*) '3 node adsorption storage unit'
          WRITE(ITU,*) 'Matrix nodes  ', node1, node2, node3
          WRITE(ITU,*) 'Connections  ', con1a, con1b, con3a, con3b

          if ( ISTATS .EQ. 1 ) then
             WRITE(ITU,*) 'Connection data:'
             WRITE(ITU,*) 'Temp of heat source connected to ',
     &                       'adsorber = ',fTemp_con1a, '  (oC)'
             WRITE(ITU,*) 'Heat cap of heat source =  ',
     &                       fMdotCp_con1a_future, ' (W/K)'
             WRITE(ITU,*) 'Temp of heat sink connected to adsorber = ',
     &                       fTemp_con1b, '  (oC)'
             WRITE(ITU,*) 'Heat cap of heat sink = ',
     &                       fMdotCp_con1b_future, ' (W/K)'
             WRITE(ITU,*) 'Temp of heat source connected to ',
     &                       'evaporator = ',fTemp_con3a, '  (oC)'
             WRITE(ITU,*) 'Heat cap of heat source to evap = ',
     &                       fMdotCp_con3a,  '(W/K)'

          endif

          WRITE(ITU,*) 'CDATA : '

          if ( bCharging ) then
             WRITE(ITU,*) 'Unit is in charging mode.'
          elseif ( bDischarge ) then
             WRITE(ITU,*) 'Unit is in discharge mode.'
          elseif ( bStorage ) then
             WRITE(ITU,*) 'Unit is in storage mode.'
          else
             WRITE(ITU,*) 'ERROR: Mode of unit is unknown'
          endif

          WRITE(ITU,*) 'Phase of cycle: '
          if ( iPhaseCycle .EQ. iIsotericHtg ) then
             WRITE(ITU,*) ' isoteric heating '
          elseif ( iPhaseCycle .EQ. iDesorption ) then
             WRITE(ITU,*) ' desorption '
          elseif ( iPhaseCycle .EQ. iIsotericClg ) then
             WRITE(ITU,*) ' isoteric cooling '
          elseif ( iPhaseCycle .EQ. iAdsorption ) then
             WRITE(ITU,*) ' adsorption '
          elseif ( iPhaseCycle .EQ. iStorage ) then
             WRITE(ITU,*) ' n/a, unit is in storage mode '
          else
             WRITE(ITU,*) ' ERROR: unknown '
          endif

          if ( ISTATS .EQ. 1 ) then
             if ( bCharging ) then
                WRITE(ITU,*) 'Temp switch from isoteric htg to ',
     &                       'desorption = ', fTemp_switch, ' (oC)'
             elseif ( bDischarge ) then
                WRITE(ITU,*) 'Temp switch from isoteric clg to ',
     &                       'adsorption = ', fTemp_switch, ' (oC)'
             endif
             WRITE(ITU,*) 'Adsorber pressure = ', fVapPressureAdsorber,
     &                    ' (kPa)'
             WRITE(ITU,*) 'Adsorber temperature = ',
     &                     fTemp_node1_present, ' (oC)'
             WRITE(ITU,*) 'fdX_dT = ', fdX_dT
             WRITE(ITU,*) 'X = ', fX_Present
             if ( bCharging ) then
                WRITE(ITU,*) 'Vapour flow from adsorber to ',
     &                       'condenser = ', fMdotVap
             elseif ( bDischarge ) then
                WRITE(ITU,*) 'Vapour flow from evaporator to',
     &                       'adsorber = ', fMdotVap
             endif
          endif


          WRITE(ITU,*) ' Matrix coefficients for ISTATS = ', ISTATS
          WRITE(ITU,*) (COUT(i), i = 1,11)
          if ( ITU.EQ.IUOUT ) then            !-trace output going to screen (vs file)
               itemp = ( IPCOMP / 4 ) * 4
               if ( (itemp .EQ. IPCOMP) .OR. (IPCOMP.EQ.NPCOMP) )
     &                           call epagew          !-write 4 lines at a time
          endif
          WRITE(ITU,*) ' Leaving subroutine ADS_stg_coeff_gen'
       endif


C------------------------------------------------------------------------------------
C XML output.
C------------------------------------------------------------------------------------

      fPhaseCycle = real(iPhaseCycle)      !- real number required for AddToReport subroutines


C.....Get component name's length
      iNameLength = lnblnk(pcname(IPCOMP))
C.....Format string as: 'plant/NAME/misc_data'
       call AddToReport(rvPltMaxAdsTemp%Identifier,
     &      fTemp_Adsorber_Max,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltMinAdsTemp%Identifier,
     &      fTemp_Adsorber_Min,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltAbsorbPress%Identifier,
     &      fVapPressureAdsorber,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltAdsVapFlow%Identifier,
     &      fMdotVap,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltCondFanPow%Identifier,
     &      fQElecCondFan,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltAdsorpHeat%Identifier,
     &      fAdsorptionHeat,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltAdsHeatTransCH%Identifier,
     &      fChHeatTransfer,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltAdsHeatTransDis%Identifier,
     &      fDisHeatTransfer,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltAdsAmbHeatLoss%Identifier,
     &      fAdsQloss,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltCondAmbHeatLoss%Identifier,
     &      fCondQloss,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltEvapAmbHeatLoss%Identifier,
     &      fEvapQloss,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltTempCondIn%Identifier,
     &      fTemp_outdoor,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltCondHeat%Identifier,
     &      fCondHeat,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltEvapHeat%Identifier,
     &      fEvapHeat,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltRatioX%Identifier,
     &      fX_Present,
     &      pcname(IPCOMP)(1:iNameLength))

       call AddToReport(rvPltCyclePhase%Identifier,
     &      fPhaseCycle,
     &      pcname(IPCOMP)(1:iNameLength))

C------------------------------------------------------------------------------------
C End of subroutine: ADS_stg_coeff_gen
C------------------------------------------------------------------------------------


       RETURN
       END





C------------------------------------------------------------------------------------
C-------------------------Function ADS_mass_ratio------------------------------------
C------------------------------------------------------------------------------------
C This function calculates the ratio of adsorbate to adsorbent, X, within the
C adsorber using the Dubinin-Astakov (D-A) equation. X is a function of the
C temperature and pressure of the adsorbate and coefficients that apply to
C a specific adsorbent/adsorbate pair.
C.
C INPUTS:
C Pressure: Pressure in adsorber   !-kPa
C Temp:     Temperature            !-oC
C Wo:       Volume of adsorption space !-m3/kg adsorbent
C D:        Coefficient of D-A equation
C n:        Coefficient of D-A equation
C
C OUTPUTS:
C ADS_mass_ratio      !- X, kg adsorbate/kg adsorbent
C------------------------------------------------------------------------------------

       FUNCTION ADS_mass_ratio(Pressure,Temp,Wo,D,n)
       IMPLICIT NONE

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       REAL    ADS_mass_ratio
       REAL    Pressure       !-kPa
       REAL    Temp           !-oC
       REAL    Wo, D, n       !-m3/kg,dimensionless, dimensionless, respectively

       REAL    FUNPV          !-function calculates saturation vapour pressure (Pa)
                              !-as a function of temperature T(Kelvin) and tau (-)
       REAL    FUNRHOL        !-function calculates the saturated liquid density
                              !-as a function of tau

       REAL    Temp_Kelvin
       REAL    Temp_critical                !-from IAPWS 1992 supplementary release
       PARAMETER( Temp_critical = 647.096 ) !-on saturation properties of water
       REAL    tau                          !-tau = 1 - T/Tcrit
       REAL    Pressure_Pa
       REAL    Psat                         !-saturated vapour pressure (Pa)
       REAL    LiqDensity                   !-saturated liquid density (kg/m3)


       Temp_Kelvin = Temp + 273.15                 !-convert to Kelvin
       Pressure_Pa = Pressure * 1000.0             !-convert to Pa


C Calculate saturation vapour pressure
C NOTE- CAN ALSO USE CLAUSIUS-CLAPEYRON EQN:
C Psat = 20.5896 - 5098.26/T where Psat is in millibar and T in Kelvin
C (see function H2OPSAT)

       tau = 1. - ( Temp_Kelvin / Temp_critical)

       Psat = FUNPV(Temp_Kelvin,tau)               !-Pa


C Calculate density of adsorbate (water) in liquid form
        LiqDensity = FUNRHOL(tau)                  !-kg/m3

C Calculate X using D-A equation

        ADS_mass_ratio = Wo * LiqDensity *
     &                  exp( -1.0*D *
     &                  ( Temp_Kelvin * LOG(Psat/Pressure_Pa) )**n )



       RETURN
       END



C------------------------------------------------------------------------------------
C------------------------------Function H2OPSAT--------------------------------------
C------------------------------------------------------------------------------------
C This function calculates the saturation pressure of water vapour using
C the Clausius-Clapeyron equation for an ideal gas.
C.
C INPUTS:
C Temp:     Temperature               !-Kelvin
C
C OUTPUTS:
C H2OPSAT   Saturated vapour pressure !-Pa
C------------------------------------------------------------------------------------


       FUNCTION H2OPSAT(Temp)
       IMPLICIT NONE

       REAL    H2OPSAT
       REAL    Temp
       REAL    Psat_mbar      !-Saturated vapour pressure in millibar
       REAL    ln_Psat


       ln_Psat = 20.5896 - 5098.26 / Temp
       Psat_mbar = exp(ln_Psat)
       H2OPSAT = Psat_mbar * 100.               !-convert to Pa



       RETURN
       END


C************************************************************************************

C******************************Function CpVapour*************************************
C
C This function calculates the specific heat of saturated vapour by linearizing the
C enthalpy of the saturated vapour at the temperature T with respect to a reference
C temperature of 25oC (298 K). The enthalpy of the saturated vapour is
C determined using the Shomate equation (source National Institute of Standards
C and Technology (NIST) Chemistry Handbook available on-line).
C INPUTS:
C Temp:       Temperature                                         !-Kelvin
C
C OUTPUTS:
C CpVapour:   Specific heat of vapour approximated by dh/dT
C             with respect to a reference temperature of 25oC     !-kJ/kgK
C------------------------------------------------------------------------------------


       FUNCTION CpVapour(Temp)
       IMPLICIT NONE

       REAL    CpVapour
       REAL    Temp             !- K
       REAL    t                !- Temp[K]/1000
       REAL    Temp_ref         !- Reference temperature = 25oC
       PARAMETER( Temp_ref = 298.15 ) !- K
       REAL    Hvap_ref         !- Enthalpy of saturated vapour at reference
                                !- temperature
       PARAMETER( Hvap_ref = 45.876 )        !- kJ/mol
       REAL    A, B, C, D, E, F, G, H        !- coefficients of Shomate equation
       PARAMETER( A = 30.09200,
     &            B = 6.832514,
     &            C = 6.793435,
     &            D = -2.534480,
     &            E = 0.082139,
     &            F = -250.8810,
     &            G = 223.3967,
     &            H = -241.8264 )
       REAL    H2O_mol_weight                !- molecular weight of water
       PARAMETER( H2O_mol_weight = 18.0155 ) !- kg/kmol
       REAL    DeltaH_mol                    !- (Hvap - Hvap_ref), kJ/mol
       REAL    DeltaH_kg                     !- (Hvap - Hvap_ref), kJ/kg
       REAL    DeltaT                        !- (Temp - Temp_ref), K


       t = Temp / 1000.
       DeltaH_mol = A * t +
     &              B / 2. * ( t**2 ) +
     &              C / 3. * ( t**3 ) +
     &              D / 4. * ( t**4 ) -
     &              E / t + F - H


       DeltaH_kg = DeltaH_mol * 1000. / H2O_mol_weight   !-convert to kJ/kg

       DeltaT = Temp - Temp_ref

       CpVapour = DeltaH_kg / DeltaT      !- kJ/kgK


       RETURN
       END


C*************************************************************************************

C****************************Function CpLiquid****************************************
C
C This function calculates the specific heat of saturated liquid by linearizing the
C enthalpy of the saturated liquid at the temperature T with respect to a reference
C temperature of 25oC (298 K). The enthalpy of the saturated liquid is
C determined using the Shomate equation (source National Institute of Standards
C and Technology (NIST) Chemistry Handbook available on-line).
C INPUTS:
C Temp:       Temperature                                         !-Kelvin
C
C OUTPUTS:
C CpLiquid:   Specific heat of liquid approximated by dh/dT
C             with respect to a reference temperature of 25oC     !-kJ/kgK
C------------------------------------------------------------------------------------


       FUNCTION CpLiquid(Temp)
       IMPLICIT NONE

       REAL    CpLiquid
       REAL    Temp             !- K
       REAL    t                !- Temp[K]/1000
       REAL    Temp_ref         !- Reference temperature = 25oC
       PARAMETER( Temp_ref = 298.15 ) !- K
       REAL    Hliq_ref         !- Enthalpy of saturated vapour at reference
                                !- temperature
       PARAMETER( Hliq_ref = 1.8885 )        !- kJ/mol
       REAL    A, B, C, D, E, F, G, H        !- coefficients of Shomate equation
       PARAMETER( A = -203.6060,
     &            B = 1523.980,
     &            C = -3196.413,
     &            D = 2474.455,
     &            E = 3.855326,
     &            F = -256.5478,
     &            G = -488.7163,
     &            H = -285.8304 )
       REAL    H2O_mol_weight                !- molecular weight of water
       PARAMETER( H2O_mol_weight = 18.0155 ) !- kg/kmol
       REAL    DeltaH_mol                    !- (Hvap - Hvap_ref), kJ/mol
       REAL    DeltaH_kg                     !- (Hvap - Hvap_ref), kJ/kg
       REAL    DeltaT                        !- (Temp - Temp_ref), K


       t = Temp / 1000.
       DeltaH_mol = A * t +
     &              B / 2. * ( t**2 ) +
     &              C / 3. * ( t**3 ) +
     &              D / 4. * ( t**4 ) -
     &              E / t + F - H


       DeltaH_kg = DeltaH_mol * 1000. / H2O_mol_weight   !-convert to kJ/kg

       DeltaT = Temp - Temp_ref

       CpLiquid = DeltaH_kg / DeltaT      !- kJ/kgK


       RETURN
       END


C **********************************H20GasCpCel ***************************************
C Created by: Maria Mottillo
C Adapted from H20LiquidCpCel by Phylroy Lopez.
C Initial Creation Date: January, 2005
C Copyright CETC 2001
c     This function will determine the Cp of water vapour at a given
C     Temperature C and Pressure.
C INPUTS:
c     REAL Temp  ! Degrees celsius
c     REAL Pressure       ! pascals.
C OUTPUT:
c     REAL  H20GasCpCel   ! joules/(kg Kelvin)
c Note no unit testing has been performed for this function.
C -------------------------------------------------------------------------------------

      FUNCTION H20GasCpCel(Temp, Pressure)
      implicit none
c     Return type
      REAL H20GasCpCel
c     Kelvin version of call
      REAL H20GasCp
c     Liquid density function.
      REAL H20GasDensity
c     Temperature in Celsius!
      REAL Temp
c     Temperature in Kelvin.
      REAL TempKel
c     Pressure in Pa
      REAL Pressure
c     Switch to Kelvin
      TempKel = Temp + 273.15
c     Obtain Cp
      H20GasCpCel = H20GasCp(TempKel, Pressure)
      RETURN
      END
C***************************************************************************************


C **********************************H20GasCp *******************************************
C Created by: Maria Mottillo
C Adapted from H20LiquidCp created by Phylroy Lopez
C Initial Creation Date: January, 2005
C     This function will determine the specific heat of water vapour
C     given the temperature and pressure.
C INPUTS:
C     REAL Temp  ! Temperature in Kelvin
C     REAL Pressure       ! in pascals.
C OUTPUT:
C     REAL  H20GasCp    joules/(kg Kelvin)
C -------------------------------------------------------------------------------------


      FUNCTION H20GasCp(Temp, Pressure)
      implicit none
      REAL H20GasCp
      REAL H20GasDensity !-existing function
      REAL IAPWSCP
      REAL Temp
      REAL Pressure
      REAL CV
      H20GasCp =
     &     REAL(IAPWSCP(H20GasDensity(Temp, Pressure),Temp,CV))
      RETURN
      END
C***************************************************************************************



C******************************Function CpWater*****************************************
C
C This function calculates the specific heat of liquid by water using the Shomate
C equation (source National Institute of Standards
C and Technology (NIST) Chemistry Handbook available on-line).
C INPUTS:
C Temp:      Temperature                                         !-Kelvin
C
C OUTPUTS:
C CpWater:   Specific heat of liquid water                       !-J/kgK
C------------------------------------------------------------------------------------


       FUNCTION CpWater(Temp)
       IMPLICIT NONE

       REAL    CpWater          !-specific heat in kJ/kgK
       REAL    Temp             !- K
       REAL    t                !- Temp[K]/1000
       REAL    A, B, C, D, E, F, G, H        !- coefficients of Shomate equation
       PARAMETER( A = -203.6060,
     &            B = 1523.980,
     &            C = -3196.413,
     &            D = 2474.455,
     &            E = 3.855326,
     &            F = -256.5478,
     &            G = -488.7163,
     &            H = -285.8304 )
       REAL    H2O_mol_weight                !- molecular weight of water
       PARAMETER( H2O_mol_weight = 18.0155 ) !- kg/kmol
       REAL    Cp                            !- specific heat in J/mo K


       t = Temp / 1000.
       Cp = A + B* t + C*(t**2) +         !-J/molK
     &      D*(t**3) + E/(t**2)


       CpWater = Cp / H2O_mol_weight * 1000.     !- J/kgK


       RETURN
       END


***************************************************************************************

C********************************SUBROUTINE TCheck*************************************
C Created by: Maria Mottillo
C Initial creation date: January 24, 2005
C This subroutine checks that a solution for temperature can be found within the range
C T1 and T2 for the a given value of X and P (i.e. that roots of the function
C f(T) = X - Wo*rho(T)exp[-D*(Tln(Psat/P))**n] = 0
C exists for a given value of P and X).
C This subroutine is a revised version of the subroutine ZBRAC provided in Chapter 9 of
C Press, W.H., Flannery, B.P., Teukolsky, S.A. and Vetterling, W.T. (1986), Numerical
C Recipes: The Art of Scientific Computing, Cambridge University Press.
C**************************************************************************************


      SUBROUTINE TCheck(Wo, D, n, X, P,T1,T2,SUCCESS)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT           !-write unit number
      INTEGER IUIN            !-read unit number


      REAL Wo, D, n       !- adsorbent/adsorbate characteristics, needed to determine X
                          !- using function ADS_mass_ratio
      REAL X, P           !- known values for which want to determine T
      REAL T1, T2         !- acceptable range for T
      LOGICAL SUCCESS     !- true if T is within acceptable range


      REAL F1, F2         !- function F(T) = 0
      REAL ADS_mass_ratio !- function used to determine X


      if ( T1 .EQ. T2 ) then
          WRITE(IUOUT,*) 'Guess for initial range required'
          STOP 'SUBROUTINE Tcheck: unresolvable error'
      endif


      F1 = X - ADS_mass_ratio( P, T1, Wo, D, n )
      F2 = X - ADS_mass_ratio( P, T2, Wo, D, n )


      SUCCESS = .false.

      if ( (F1*F2) .LT. 0. )  SUCCESS = .true.


      RETURN
      END

C***************************************************************************************

C **********************************TSOLVE *********************************************
C Created by: Maria Mottillo
C Initial creation date: January 13, 2005
C This function determines the temperature of the adsorber given the ratio of adsorbate
C to adsorbent X, and the pressure of the adsorber. It is used to determine the
C temperature at which the phase of the cycle goes from isoteric heating to desorption
C during the charging stage or the temperature at which the phase of the cycle goes
C from isoteric cooling to adsorption during the discharge phase. Temperature is
C determined using the bisection method. This function is a revised version of the function
C RTBIS in Chapter 9 of (Press et al., 1986).

C INPUTS:
C Wo: Coefficient of D-A equation (m3/kg adsorbent)
C D:  Coefficient of D-A equation (-)
C n:  Coefficient od D-A equation (-)
C X: Ratio of mass of adsorbate to mass of adsorbent (kg/kg adsorbent)
C P: Pressure (kPa), equal to the condenser pressure (charging phase) or
C    evaporator pressure (discharge phase)
C T1,T2: Range of values the solution will lie between (oC)
C
C OUTPUTS:
C T: Temperature (oC) at end of isoteric heating phase (charging) or
C    end of isoteric cooling phase (discharge).
C---------------------------------------------------------------------------------------

      FUNCTION TSOLVE(Wo,D,n,X,P,T1,T2,ACC)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT           !-write unit number
      INTEGER IUIN            !-read unit number

      REAL TSOLVE
      REAL Wo,D,n
      REAL X, P
      REAL T1, T2
      REAL ACC

      INTEGER JMAX         !- maximum number of iterations
      PARAMETER( JMAX = 40 )

      REAL FMID, F, DT, TMID
      REAL ADS_mass_ratio       !-function to calculate X
      INTEGER J
      LOGICAL closet2, closet1


      FMID = X - ADS_mass_ratio( P, T2, Wo, D, n )
      F = X - ADS_mass_ratio( P, T1, Wo, D, n )

      call eclose(FMID,0.,ACC,closet2)
      call eclose(F,0.,ACC,closet1)

      if ( closet2 ) then

           TSOLVE = T2
           RETURN

      elseif ( closet1 ) then

           TSOLVE = T1
           RETURN

      endif


      if ( (F*FMID) .GT. 0. ) then
           WRITE(IUOUT,*) 'root must be bracketed for bisection'
           WRITE(IUOUT,*) ' X = ', X
           WRITE(IUOUT,*) ' P = ', P
           WRITE(IUOUT,*) ' T1 = ', T1
           WRITE(IUOUT,*) ' T2 = ', T2
           WRITE(IUOUT,*) ' FMID = ', FMID
           WRITE(IUOUT,*) ' F = ', F
           WRITE(IUOUT,*) ' X(P,T2) = ', ADS_mass_ratio(P,T2,Wo,D,n)
           WRITE(IUOUT,*) ' X(P,T1) = ', ADS_mass_ratio(P,T1,Wo,D,n)
           STOP 'FUNCTION TSOLVE: unresolvable error'
      endif

      if ( F .LT. 0. ) then

          TSOLVE = T1
          DT = T2 - T1

      else

          TSOLVE = T2
          DT = T1 - T2

      endif

      DO 20 J = 1, JMAX

          DT = DT * 0.5
          TMID = TSOLVE + DT
          FMID = X - ADS_mass_ratio( P, TMID, Wo, D, n )

          if ( FMID .LE. 0. ) TSOLVE = TMID

          if ( (ABS(DT) .lt. ACC) .OR. (FMID.EQ.0.) ) RETURN


 20   CONTINUE

      WRITE(IUOUT,*) 'too many bisections'
      WRITE(IUOUT,*) ' X = ', X
      WRITE(IUOUT,*) ' P = ', P
      WRITE(IUOUT,*) ' T1 = ', T1
      WRITE(IUOUT,*) ' T2 = ', T2
      WRITE(IUOUT,*) ' TMID = ', TMID
      WRITE(IUOUT,*) ' FMID = ', FMID
      WRITE(IUOUT,*) ' TSOLVE = ', TSOLVE
      STOP 'FUNCTION TSOLVE: unresolvable error'

      RETURN
      END

C*****************************************************************************************

C**************************************Function PSOLVE************************************
C Created by: Maria Mottillo
C Initial creation date: January 13, 2005
C
C This function is used to solve for pressure during the isoteric heating and cooling
C phases using the Dubinin - Astakhov equation. During the isoteric heating and cooling
C phases, the value of X is constant. Knowing X and the temperature T, the pressure
C is determined iteratively using the bisection method. This function is a revised
C version of the RTBIS function in Chapter 9 of (Press et al., 1986)

C INPUTS:
C Wo: Coefficient of D-A equation (m3/kg adsorbent)
C D:  Coefficient of D-A equation (-)
C n:  Coefficient od D-A equation (-)
C X:  Ratio of mass of adsorbate to mass of adsorbent (kg/kg adsorbent)
C T:  Temperature (oC)
C P1, P2: Range of pressures solution will be lie between  (kPa)
C ACC: The iteration will continue until the solution is +/- this value

C
C OUTPUTS:
C PSolve: Pressure for a given X and T within the range P1-P2 (kPa)
C-------------------------------------------------------------------------------------------

      FUNCTION PSOLVE(Wo,D,n,X,T,P1,P2,ACC)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT           !-write unit number
      INTEGER IUIN            !-read unit number

C for debugging
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
C end debugging


      REAL PSOLVE
      REAL Wo,D,n
      REAL X, T
      REAL P1, P2
      REAL ACC

      INTEGER JMAX         !- maximum number of iterations
      PARAMETER( JMAX = 40 )

      REAL FMID, F, DP, PMID
      REAL ADS_mass_ratio       !-function to calculate X
      INTEGER J

      LOGICAL closep2, closep1


      FMID = X - ADS_mass_ratio( P2, T, Wo, D, n )
      F = X - ADS_mass_ratio( P1, T, Wo, D, n )

      call eclose(FMID,0.,ACC,closep2)
      call eclose(F,0.,ACC,closep1)

      if ( closep2 ) then

           PSOLVE = P2
           RETURN

      elseif ( closep1 ) then

           PSOLVE = P1
           RETURN

      endif


      if ( (F*FMID) .GT. 0. ) then
           WRITE(IUOUT,*) 'root must be bracketed for bisection'
           WRITE(IUOUT,*) ' X = ', X
           WRITE(IUOUT,*) ' T = ', T
           WRITE(IUOUT,*) ' P1 = ', P1
           WRITE(IUOUT,*) ' P2 = ', P2
           WRITE(IUOUT,*) ' FMID = ', FMID
           WRITE(IUOUT,*) ' F = ', F
           WRITE(IUOUT,*) ' X(P2,T) = ', ADS_mass_ratio(P2,T,Wo,D,n)
           WRITE(IUOUT,*) ' X(P1,T) = ', ADS_mass_ratio(P1,T,Wo,D,n)
C for debugging
C           write(iuout,*) 'nsinc = ', nsinc
C end debugging
           STOP 'FUNCTION PSOLVE: unresolvable error'
      endif

      if ( F .LT. 0. ) then

          PSOLVE = P1
          DP = P2 - P1

      else

          PSOLVE = P2
          DP = P1 - P2

      endif

      DO 20 J = 1, JMAX

          DP = DP * 0.5
          PMID = PSOLVE + DP
          FMID = X - ADS_mass_ratio( PMID, T, Wo, D, n )

          if ( FMID .LE. 0. ) PSOLVE = PMID

          if ( (ABS(DP) .lt. ACC) .OR. (FMID.EQ.0.) ) RETURN


 20   CONTINUE

      WRITE(IUOUT,*) 'too many bisections'
      WRITE(IUOUT,*) ' X = ', X
      WRITE(IUOUT,*) ' T = ', T
      WRITE(IUOUT,*) ' P1 = ', P1
      WRITE(IUOUT,*) ' P2 = ', P2
      WRITE(IUOUT,*) ' PSOLVE = ', PSOLVE
      WRITE(IUOUT,*) ' PMID = ', PMID
      WRITE(IUOUT,*) ' FMID = ', FMID
      STOP 'FUNCTION PSOLVE: unresolvable error'

      RETURN
      END

C***********************************************************************************************


