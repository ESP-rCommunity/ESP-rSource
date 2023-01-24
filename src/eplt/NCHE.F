C===================================================================================
C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2008. Please Contact Ian Beausoleil-Morrison for details
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
C
C======================== NCHE.F ===================================================
C
C This file contains code necessary to model a natural convection
C heat exchanger from experimental performance results.
C
C This calculation requires two major sections: 
C   - the natural convection heat exchanger plant component model
C     which solves for the exit temperatures of the heat exchanger
C   - the natural convection flow rate generator component model
C     which calculates the flow rate based on the configuration of
C     the loop.
C
C   NCHE_static_template: Static template executed prior to time-step simulation.
C                         Collects NCHE model inputs and performs some
C                         error trapping
C   NCHE_coeff_gen: Routine called by the plant modelling domain to
C                   characterize the natural convection heat exchanger
C   NCHE_flow: Calculates the natural convection flow rate based on the 
C              pressure drop correlation input by the user
C
C REFERENCES:
C
C Equation numbers refer to:
C Didier Thevenard (2008). Development of a Natural Convection Heat Exchanger 
C model in esp-r. Report submitted to Natural Resources Canada.
C
C The report above is based on a model described in:
C Cruickshank and Harrison (2006). Experimental characterization of a natural 
C convection heat exchanger for solar domestic hot water systems. Proc. 
C ISEC 2006 & 2006 International Solar Energy conference, July 8-13, 2006, 
C Denver, CO, USA
C
C===================================================================================


C====================== NCHE_static_template =======================================
C 
C Date:      September 10, 2007
C Author:    Julia Purdy
C Copyright: Natural Resources Canada
C
C Modified:  December 1, 2008
C Author:    Didier Thevenard
C
C ABSTRACT:
C This subroutine is the static template for the natural convection 
C heat exchanger (NCHE) component. 
C
C It checks whether the component is properly used in the user-defined 
C plant network and it collects time-invariant plant component parameters
C stored in ESP-r's adata & bdata arrays, and stores them in the
C common block COMMON/NCHE/.
C
C The NCHE component is represented with two nodes:
C   node 1 represents the forced flow side with fluid from the solar collector,
C          the "hot-side".
C   node 2 represents the natural convection side with fluid from the
C          storage tank, the "cold side".
C
C The static template also ensures that the NCHE is connected to a stratified storage
C tank - and will issue an error message if this is not the case.
C
C INPUTS:
C    iComp_index  index number of the component under consideration (=IPCOMP)
C    NPCOMP       number of plant components in user-defined plant network
C    NCI(i)       holds the number of possible plant control variables for plant component `i'
C    ADATA(i,j)   holds `j'th data item for component `i' read from .pln file
C                 (derived from plant components database)
C    ICONTP(i)    holds the state variable index (ISV) of the sending node for connection `i'.
C    IUOUT        output channel to feedback to user
C    ITC          trace output index (>0 gives a trace output)
C    ITU          output channel for trace output
C    ITRACE(35)   flag indicating one of the plant traces (1= active)
  
C OUTPUTS:
C    NONE
C
C-----------------------------------------------------------------------------------
      SUBROUTINE NCHE_static_template( iComp_index )

      IMPLICIT NONE

      INCLUDE "building.h"
      INCLUDE "plant.h"
      INCLUDE "NCHE.h"

C-----------------------------------------------------------------------------------
C ESP-r Common Block Variables.
C-----------------------------------------------------------------------------------

C.....Trace & reporting data
      COMMON/tc/itc,icnt
      COMMON/outin/iuout,iuin,ieout
      COMMON/trace/itcf,itrace(mtrace),izntrc(mcom),itu

C.....Trace/error reporting unit numbers
      INTEGER itc,icnt
      INTEGER iuout,iuin,ieout
      INTEGER itcf,itrace,izntrc,itu

C.....Network & control data
      COMMON/c9/npcomp,nci,cdata
      INTEGER npcomp            ! number of plant components
      INTEGER nci(mpcom)        ! number of controls / component
      REAL cdata(mpcom,mmiscd)  ! control data for each component

C.....Plant component configuration data - read from input file
      COMMON/pdbdt/adata,bdata
      REAL adata(mpcom,madata) ! miscellaneous data for component
      REAL bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Plant component connection data
      COMMON/pcond/convar, icontp, icondx
      REAL    convar(mpcon,mconvr)        ! connection ->  state variables,  
                                          ! ...temperature/flow data
      INTEGER icontp(mpcon)               ! connection -> type
      INTEGER icondx(mpcom,mnodec,mpconc) ! connection -> index for each
                                          ! ...component / node / connection
C.....Stratified tank
      INCLUDE "stratified_tank.h"

C.....ESP-r common variable holding node numbers for plant components within plant
C     network
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9) ! Miscellanous plant data
      INTEGER IPOFS1(MCOEFG)
      INTEGER IPOFS2(MCOEFG,MPVAR)

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON ! number of inter-component connections
      INTEGER IPC1  ! component number defining receiving component
      INTEGER IPN1  ! node number defining receiving component
                    ! ...(ie defining inlet node)
      INTEGER IPCT  ! plant component inter-connection type where:
                    !    1 = connected to identical environmental conditions
                    !        as the source component
                    !    2 = connected to constant and known environmental
                    !        conditions (defined by PCONSD(?,1) and PCONSD(?,2))
                    !    3 = connected to another component (as defined
                    !        by IPC2 and IPN2);
                    !    4 = connected to a building zone (as defined by
                    !        PCONSD(?,1)) or ambient conditions (PCONSD(?,1)=0)
      INTEGER IPC2  ! component number defining the sending component
      INTEGER IPN2  ! node number defining the sneding component
      REAL PCONDR   ! ratio of the mass flow rate through the current connection
      REAL PCONSD   ! Array containing a list of constant data items for the
                    ! ...current connection.

C-----------------------------------------------------------------------------------
C Declare local variables.
C-----------------------------------------------------------------------------------
      INTEGER NumADATA, IPCONC
      INTEGER iComp_index     ! index of component in plant network = IPCOMP
      INTEGER I,J             ! counters
      INTEGER iTemp_trace

C --- Heat Exchanger ADATA variables not stored in NCHE.h
      REAL fHX_hot_mass(mpcom)      ! mass of hot side fluid + solid (kg)
      REAL fHX_hot_C_p(mpcom)       ! Specific heat of the hot-side fluid and solids
      REAL fHX_cold_mass(mpcom)     ! Mass of the cold-side fluid + solid (kg)
      REAL fHX_cold_C_p(mpcom)      ! Specific heat of the cold-side fluid and solids

C --- Plant connection index numbers
      INTEGER iHX_cold_node_number  ! Node number for heat exchanger component cold side 
                                    ! ...node in plant network (Node 2)
      INTEGER iHX_hot_node_number   ! Node number for heat exchanger component cold side 
                                    ! ...node in plant network (Node 1)
      INTEGER iHX_Conn_Index_in     ! index of incoming fluid connection
                                    ! ...to cold side (Node 2) of NCHE model
      INTEGER iSendCompIndex        ! index of the sending component
      INTEGER iSendCompNodeNumber   ! node number of the sending component
      INTEGER iRecevCompIndex       ! index of the receiving component
      INTEGER iRecevCompNodeNumber  ! node number of the receiving component
      INTEGER iCon                  ! counter - outgoing connection index numbers


C --- Logical variables
      LOGICAL mistake               ! to flag that mistake has happened
      LOGICAL isclose               ! to indicate variable is close to another
      LOGICAL found                 ! to indicate component found

C --- Functions
      REAL fHX_get_stratified_tank_height   ! function returning tank height   

C-----------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C-----------------------------------------------------------------------------------

      IF( ITC>0 .AND. ITRACE(35) .NE. 0 ) THEN
        WRITE(ITU,*) ' Component ',iComp_index,
     &               ' pre-simulation data for:'
        WRITE(ITU,*) ' 2-node Natural Convection Heat Exchanger (NCHE)'
        NumADATA = 5  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',( ADATA(iComp_index,J),J = 1,NumADATA )
        IF( ITU==IUOUT ) THEN  ! trace output going to screen, not file
          iTemp_trace=( iComp_index/5 ) * 5
          IF( iTemp_trace == iComp_index .OR. iComp_index == NPCOMP ) 
     &                 call epagew ! write 5 lines at a time.
        END IF
      END IF

C-----------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(iComp_index) holds the number of possible plant control variables
C as specified in the .pln file. The NCHE component has no control variables.
C-----------------------------------------------------------------------------------

      IF( NCI(iComp_index) /= 0 ) THEN
        WRITE(ITU,*) ' NCHE_static_template warning: incorrect number',
     &               ' of controlled variables specified.'
      ENDIF


C-----------------------------------------------------------------------------------
C Check that each node in the component has the correct number of connections
C to other components.
C Variables used:
C    MPCONC           the maximum allowable connections to a node (from plant.h).
C    ICONDX(i,j,k)    the connection number for the k'th connection to the j'th node
C                     of component i. It is used as a pointer.
C    'mistake'        a flag indicating whether there are connection errors:
C                     .true. means there are errors.
C-----------------------------------------------------------------------------------
      mistake = .false.

C-----There should be one connection to node 1.
      IF( ICONDX( iComp_index,1,1 ) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX( iComp_index,1,IPCONC ) /= 0 ) mistake=.true.
      END DO

C-----There should be one connection to node 2.
      IF( ICONDX( iComp_index,2,1 ) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX( iComp_index,2,IPCONC ) /= 0 ) mistake=.true.
      END DO

C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' NCHE_static_template: incorrect number of'
        WRITE(IUOUT,*) ' connection for component ',iComp_index
        STOP ' NCHE_static_template: unresolvable error'
      END IF


C-----------------------------------------------------------------------------------
C Check that the connection to nodes 1 and 2 are of the correct type. 
C The connections should be of type ISV=20 so that the NCHE component can 
C be used in networks in which both first and second phase mass balances are formed.
C Variables used:
C    ICONTP(i)  holds the state variable index (ISV) of the sending 
C                node for connection `i'.
C    ISV        defines nodal fluid type & coefficient generator model capabilities:
C               ISV = 0,10,20 node represents water + ....
C               ISV = 1,11,21 node represents dry air + ....
C               ISV = 9,19,29 node represents some solid material only
C               0  <= ISV < 10  model suitable for energy balance only
C               10 <= ISV < 20  model suitable for energy + single phase mass balance
C               20 <= ISV < 30  model suitable for energy + two phase mass balances
C-----------------------------------------------------------------------------------

      IF( ICONTP( ICONDX(iComp_index,1,1) ) /= 20 ) THEN
        WRITE( IUOUT,* ) 
     &   ' NCHE_static_template: incorrect connection type'
        WRITE( IUOUT,* ) ' to node 1 for component ',iComp_index
        STOP ' NCHE_static_template: unresolvable error'
      END IF

      IF( ICONTP( ICONDX(iComp_index,2,1) ) /= 20 ) THEN
        WRITE( IUOUT,* ) 
     &   ' NCHE_static_template: incorrect connection type'
        WRITE( IUOUT,* ) ' to node 2 for component ',iComp_index
        STOP ' NCHE_static_template: unresolvable error'
      END IF


C-----------------------------------------------------------------------------------
C Check that the cold (natural convection) side of the heat exchanger is connected 
C to an appropriate storage tank. This is required by the empirical model so that 
C a pressure differential between the top and the bottom of the storage tank will 
C promote natural convection flow.
C
C There should be a connection to/from:
C  - the stratified tank model (component code = 123); or
C  - hx-in-tank stratified tank model (component code = 123)
C-----------------------------------------------------------------------------------

C     The cold-side of the NCHE component is node 2
      iHX_cold_node_number = 2
C     The hot-side of the NCHE component is node 1
      iHX_hot_node_number = 1


C-----Get index of incoming fluid connection
C.....Storage tank to node 2 (HX-cold side) water connection index
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iHX_Conn_Index_in = 
     &             ICONDX( iComp_index,iHX_Cold_Node_number,1 )

C-----Determine the connection type/index for the incoming and outgoing 
C-----connections 
C.....Incoming connection type 
C.....  IPC2(iHX_Conn_Index_in)  ! <- sending component
      iSendCompIndex = IPC2( iHX_Conn_Index_in )
      iSendCompNodeNumber = IPN2( iHX_Conn_Index_in )

C.....Outgoing connection index
C.....  IPC2(iHX_Conn_Index_in)  ! <- receiving component 
      DO iCon = 1, NPCON

         IF ( ( IPC2( iCon ) .EQ. iSendCompIndex )  .AND.  
     &             ( IPN2( iCon ) .EQ. iSendCompNodeNumber ) ) THEN

             iRecevCompIndex = IPC2( iCon )
             iRecevCompNodeNumber = IPN2( iCon )

         ENDIF 

      ENDDO


C-----If the index numbers for the incoming and outgoing connections are
C-----the same, then the heat exchanger is connected to only one component

      IF ( NPCDAT(iRecevCompIndex,4) /= NPCDAT (iSendCompIndex,4) ) THEN

         WRITE( IUOUT,* ) ' NCHE_static_template: input and output of '
         WRITE( IUOUT,* ) ' side-arm heat exchanger must be connected '
         WRITE( IUOUT,*)  ' to the same tank component'
         STOP ' NCHE_static_template: unresolvable error'

      ENDIF


C-----Determine if the NCHE cold-side connection is to:
C-----the stratified tank model (iRecevCompIndex = 123)

C......Stratified storage tank 
      IF ( NPCDAT(iRecevCompIndex,4) /= 123 ) THEN

         WRITE( IUOUT,* ) ' NCHE_static_template: side-arm heat '
         WRITE( IUOUT,* ) ' exchanger must be connected to a '
         WRITE( IUOUT, *) ' stratified tank '
         STOP ' NCHE_static_template: unresolvable error'

      ENDIF
 

C-----------------------------------------------------------------------------------
C     Component is well defined. Store iComp_index in NCHE common block
C-----------------------------------------------------------------------------------

      N_NCHE = N_NCHE+1
      IF (N_NCHE .GT. MAX_NCHE) THEN

        WRITE(IUOUT,*) ' NCHE_static_template: '
        WRITE(IUOUT,*) ' too many NCHEs defined'
        WRITE(IUOUT,*) ' maximum should be ',MAX_NCHE
        STOP ' NCHE_static_template: unresolvable error'

      ENDIF
      IPCOMP_NCHE(N_NCHE) = iComp_index

C-----------------------------------------------------------------------------------
C Read in the time-invariant user-input data (ADATA) from the
C plant input and store this data in the common block 
C COMMON/NCHE/.
C-----------------------------------------------------------------------------------
C     Heat Exchanger Empirical Results
C     These parameters are stored in the NCHE common block. This is 
C     not absolutely necessary (they could be accessed through ADATA)
C     but it provides some paralellism with other parameters for the
C     NCHE model, such as the tank height.
C-----------------------------------------------------------------------------------

C.....Modified effectiveness vs. capacitance ratio    
      fHX_eff_CR_a ( N_NCHE ) = ADATA ( iComp_index, 1 )
      fHX_eff_CR_b ( N_NCHE ) = ADATA ( iComp_index, 2 )

C.....Natural convection flow rate vs. pressure drop
      fHX_flow_P_c ( N_NCHE ) = ADATA ( iComp_index, 3 )
      fHX_flow_P_d ( N_NCHE ) = ADATA ( iComp_index, 4 )
      
C-----------------------------------------------------------------------------------
C     Heat Exchanger height (m)
C-----------------------------------------------------------------------------------

      fHX_height ( N_NCHE ) = ADATA ( iComp_index, 5 )

C.....Is HX height close to zero?
      call eclose( ( fHX_height(N_NCHE) ), 0.0, 1.0E-10, isclose )
      IF ( isclose ) THEN

         write (itu,*) 'NCHE Error: Specified height is zero!  '
         write (itu,'(A,I2,A,I2)') 
     &     'Check misc data item 5 in Component # ', iComp_index
         stop 'NCHE_static_template (NCHE.F): Fatal error!'

      ENDIF 

      
C-----------------------------------------------------------------------------------
C     Tank information
C-----------------------------------------------------------------------------------
C.....Variable iStrat_tank_CompIndex is saved to the NCHE common block. 
C.....It is used to get the average stratified tank temperature each timestep.
      iStrat_tank_CompIndex(N_NCHE) = iRecevCompIndex

C.....Call to function to get tank height from stratified tank model
      fTank_Height(N_NCHE) = 
     &  fHX_get_stratified_tank_height( iRecevCompIndex )

C.....Identify number of stratified tank in common stratified_tank.h
      iStrat_tank_number(N_NCHE) = 0
      found = .false.
      DO I=1,N_STANK
        IF (iStrat_tank_CompIndex(N_NCHE) == IPCOMP_STANK(I)) THEN
          iStrat_tank_number(N_NCHE) = I
          found = .true.
        ENDIF
      ENDDO

      if (.not. found) THEN
        WRITE(ITU,*) 'Stratified tank information not found'
        WRITE(ITU,*) 'for component ', iStrat_tank_CompIndex(N_NCHE)
        WRITE(ITU,*) 'Aborting simulation'
        STOP
      endif

      RETURN

      END SUBROUTINE


C==================== NCHE_coeff_gen ===============================================
C
C Date:      November 8, 2005
C Author:    Julia Purdy
C Copyright: Natural Resources Canada
C
C Updated:   November 6, 2007 
C Author:    Alex Ferguson and Julia Purdy
C
C Updated:   December 1, 2008
C Author:    Didier Thevenard
C
C ABSTRACT:
C This routine calculates the matrix coefficients for the natural convection
C heat exchanger component model.
C
C
C Inputs:
C
C  iComp_index        - component index number in plant network
C  ISTATS             - index of plant matrix being solved
C
C Outputs:
C  fCoefficients      - array (of 5) containing components matrix
C                       coefficients
C-----------------------------------------------------------------------------------


      SUBROUTINE NCHE_coeff_gen ( iComp_index, fCoefficients,
     &                            ISTATS )
      IMPLICIT NONE

      include "plant.h"
      include "building.h"
      include "NCHE.h"

C-----------------------------------------------------------------------------------
C     Passed arguments
C-----------------------------------------------------------------------------------
      INTEGER iComp_index     ! pointer to component in network
      INTEGER ISTATS          ! index of matrix to be solved, will generate 
                              ! coefficients for energy balances (ISTATS=1), 
                              ! 1st phase mass balances (ISTATS=2), or 
                              ! 2nd phase mass balances (ISTATS=3).


      REAL fCoefficients(mpcoe)   ! array containing matrix coefficients 
                                  ! calculated by the component.


C-----------------------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------------------
C.....Network & control data
      COMMON/c12ps/npcdat,ipofs1,ipofs2
      INTEGER npcdat(mpcom,9)      ! miscellaneous plant data
      INTEGER ipofs1(mcoefg)       ! not used in current context
      INTEGER ipofs2(mcoefg,mpvar) ! not used in current context

C.....Trace & error reporting data (unit numbers)
      COMMON/tc/itc,icnt
      COMMON/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      INTEGER itc,icnt
      INTEGER itcf,itrace,izntrc,itu

C.....Plant interation data
      COMMON/piter/maxitp,perrel,pertmp,perflx,permfl,itrclp,
     &             icsv,csvi
      INTEGER maxitp            ! not used in current context
      INTEGER itrclp            ! not used in current context
      INTEGER icsv(mpnode,mpvar)! flag marking nodes for iteration
      REAL perrel               ! not used in current context
      REAL pertmp               ! not used in current context
      REAL perflx               ! not used in current context
      REAL permfl               ! not used in current context
      REAL csvi(mpnode,mpvar)   ! 'initial' values of state variables
                                ! at start of iteration

C.....Plant network time constants
      COMMON/pctc/tc
      REAL tc(mpcom) ! time contant (seconds)

C.....Plant present and future state variables
      COMMON/pcval/csvf,csvp
      REAL csvf(mpnode,mpvar)   ! future time-row state variables
      REAL csvp(mpnode,mpvar)   ! present time-row state variables

C.....Plant component connection data
      COMMON/pcond/convar, icontp, icondx
      REAL    convar(mpcon,mconvr)        ! state varibles for connections
      INTEGER icontp(mpcon)               ! type of each connection
      INTEGER icondx(mpcom,mnodec,mpconc) ! pointer to connections for each
                                          ! component/node

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON ! number of inter-component connections
      INTEGER IPC1  ! component number defining receiving component
      INTEGER IPN1  ! node number defining receiving component
                    ! ...(ie defining inlet node)
      INTEGER IPCT  ! plant component inter-connection type where:
                    !    1 = connected to identical environmental conditions
                    !        as the source component
                    !    2 = connected to constant and known environmental
                    !        conditions (defined by PCONSD(?,1) and PCONSD(?,2))
                    !    3 = connected to another component (as defined
                    !        by IPC2 and IPN2);
                    !    4 = connected to a building zone (as defined by
                    !        PCONSD(?,1)) or ambient conditions (PCONSD(?,1)=0)
      INTEGER IPC2  ! component number defining the sending component
      INTEGER IPN2  ! node number defining the sneding component
      REAL PCONDR   ! ratio of the mass flow rate through the current connection
      REAL PCONSD   ! Array containing a list of constant data items for the
                    ! ...current connection.

C.....Common storing status of plant initialization
      COMMON/plant_initialization/bInitialized
      LOGICAL bInitialized(mpcom)

C.....Simulation timestep
      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER ihrp              ! not used in current context
      INTEGER ihrf              ! not used in current context
      INTEGER idyp              ! not used - present day #
      INTEGER idyf              ! not used in current context
      INTEGER idwp              ! not used in current context
      INTEGER idwf              ! not used in current context
      INTEGER nsinc             ! time step number
      INTEGER its,idynow        ! not used in current context

C.....Time
      COMMON/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      INTEGER isd1              ! not used in current context
      INTEGER ism1              ! not used in current context
      INTEGER isd2              ! not used in current context
      INTEGER ism2              ! not used in current context
      INTEGER isds              ! not used in current context
      INTEGER isdf              ! not used in current context
      INTEGER ntstep            ! number of building steps/hour
      COMMON/pctstp/ntstpp
      INTEGER ntstpp            ! number of plant timesteps / building timestep

C.....Plant node values
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCNTMP, PCTP, PCRP, PUAP, PCQP, PCNTMF, PCTF, PCRF, 
     &  PUAF, PCQF

C-----------------------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------------------

      INTEGER iTemp             ! named constant for temperature property
      INTEGER i1stFlow          ! named constant for first phase flow property
                                !  (water)
      INTEGER i2ndFlow          ! named constant for second phase flow property
                                !  (glycol)
      PARAMETER ( iTemp       = 1,
     &            i1stFlow    = 2,
     &            i2ndFlow    = 3 )

      REAL fSoln_alpha          ! Implicit / explicit weighting
      INTEGER num_matrix_coeff  ! Number of Matrix coefficients

      INTEGER iInitialize       ! Named constant indicating h3kreports
                                !  modules should self-initialize
      INTEGER iReport           ! Named constant indicating h3kreports
                                !  modules should report data
      PARAMETER ( iInitialize = 1,
     &            iReport     = 2 )

      REAL small    ! used to ensure there are no divide by zero errors
      PARAMETER ( small = 1.0E-15 )  

C-----------------------------------------------------------------------------------
C     ESP-r functions
C-----------------------------------------------------------------------------------

      REAL SHtFld       ! Standard ESP-r function - returns the specific heat
                        ! of air, water vapor, or water.


      REAL glycol_water_cp          ! Function for glycol-water specific heat 
      REAL glycol_vol_fraction_find ! Function for finding volume fraction of 
                                    ! ...glycol inside water-glycol mixture   

C-----------------------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------------------

C.....Heat exchanger number
      INTEGER iNCHE_number        ! Number of heat exchanger [1..MAX_NCHE]

C.....Heat exchanger conditions
C....... Hot Side, future time-row
      REAL fHX_hot_mass_flow_F    ! HX hot side flow rate (kg/s)
      REAL fHX_hot_temp_in_F      ! HX hot side inlet temperature (oC)
      REAL fHX_hot_fluid_cp_F     ! HX hot side specific heat
      REAL fHX_hot_heat_cap_F     ! HX hot side heat capacity (W/K)
      REAL fHX_hot_temp_out_F     ! HX hot side outlet temperature (oC)
C....... Cold side, future time-row
      REAL fHX_cold_mass_flow_F   ! HX cold side flow rate (kg/s)
      REAL fHX_cold_temp_in_F     ! HX cold side inlet temperature (oC)
      REAL fHX_cold_fluid_cp_F    ! HX cold side specific heat
      REAL fHX_cold_heat_cap_F    ! HX cold side heat capacity (W/K)
      REAL fHX_cold_temp_out_F    ! HX cold side outlet temperature (oC)

C.....Modified heat exchanger performance indices
      REAL fHX_mod_eff          ! modified HX effectiveness
      REAL fHX_mod_cap_ratio    ! modified capacitance ratio
      REAL fHX_Q_F              ! heat transfer rate (future)

C.....Heat exchanger glycol-water solution fluid properties
C......... Present time-row
      REAL fHX_glycol_mass_fraction_P   ! mass fraction of glycol in incoming flow  [%]
      REAL fHX_glycol_volume_fraction_P ! volume fraction of glycol in incoming flow
C......... Present time-row
      REAL fHX_glycol_mass_fraction_F   ! mass fraction of glycol in incoming flow  [%]
      REAL fHX_glycol_volume_fraction_F ! volume fraction of glycol in incoming flow

C.....Model Topology
C....... Nodal indices
      INTEGER iHX_Hot_Node_index  ! Index of heat exchanger component hot side 
                                  !  node in plant network.
      INTEGER iHX_Cold_Node_index ! Index of heat exchanger component cold side 
                                  !  node in plant network.
C....... Connection indices
      INTEGER iHX_hot_conn_index  ! Index number of HX component hot node 
                                  !  in plant network
      INTEGER iHX_cold_conn_index ! Index number of HX component cold node 
                                  !  in plant network

      LOGICAL bNoFlowHot, bNoFlowCold   ! Flags for no-flow condition
      INTEGER I                 ! Counter  
      LOGICAL found             ! 

C-----------------------------------------------------------------------------------
C Start trace if trace output requested.
C-----------------------------------------------------------------------------------
      IF( ITC.gt.0 .AND. NSINC.ge.ITC .AND. NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0 )THEN
        WRITE(ITU,*) ' Entering subroutine NCHE_coeff_gen '
      END IF

C-----------------------------------------------------------------------------------
C     Find number of NCHE
C     There can be up to MAX_NCHE natural convection heat exchangers in a 
C     plant, as specified in the common /NCHE/. Here we just find which 
C     of these NCHEs we are dealing with, by looking which one of them has
C     the requested ICOMP
C-----------------------------------------------------------------------------------
      iNCHE_number = 0
        found = .false.
      DO I=1,N_NCHE
        IF (IPCOMP_NCHE(I) == iComp_index) THEN
          iNCHE_number = I
          found = .true.
        ENDIF
      ENDDO
      if (.not. found) THEN
        WRITE(ITU,*) 'Natural convection heat exchanger information '
        WRITE(ITU,*) 'not found for component ', iComp_index
        WRITE(ITU,*) 'Aborting simulation'
        STOP
      endif

C-----------------------------------------------------------------------------------
C     Determine component location in plant network topology, and
C     mark component node for iteration
C
C     -> Get node indicies, and mark nodes for iteration
C
C         NPCDAT(i,9):
C          row and column number defining location of component 'i'
C          sub-matrix template in the overall plant network matrix. It is
C          used to identify the location of the current component's nodes
C          within the global plant matrix.
C
C         ICSV(i,j)
C          flag indicating that node 'i' is marked for iteration for state
C          variable 'j'; j=1 for temperature, j=2 for 1st phase mass flow,
C          j=3 for 2nd phase mass flow.
C
C         CSVI(i,j)
C          initial value for judging whether iteration required. Same
C          indices as ICSV.
C
C         CSVF(i,j)
C          future time-row solution variable for plant. Same indices
C          as ICSV.
C-----------------------------------------------------------------------------------

      iHX_Hot_Node_index  = NPCDAT ( iComp_index, 9 )
      iHX_Cold_Node_index = NPCDAT ( iComp_index, 9 ) + 1

C.....Iteration on HX hot side for temperature and flow rate
C.......temperature
      icsv( iHX_Hot_Node_index, 1 )  = 1
C.......first flow rate
      icsv( iHX_Hot_Node_index, 2 )  = 1
C.......second flow rate
      icsv( iHX_Hot_Node_index, 3 )  = 1
C.....Iteration on HX cold side for temperature and flow rate
C.......temperature
      icsv( iHX_Cold_Node_index, 1 ) = 1
C.......1st phase flow
      icsv( iHX_Cold_Node_index, 2 ) = 1
C........there is no 2nd phase flow (for the potential of glycol) on the cold
C........side of the natural convection heat exchanger.

C.....Transport future values to initial values for iteration.
      csvi( iHX_Hot_Node_index, 1 )  =
     &                         csvf( iHX_Hot_Node_index, 1 )
      csvi( iHX_Hot_Node_index, 2 )  =
     &                         csvf( iHX_Hot_Node_index, 2 )
      csvi( iHX_Hot_Node_index, 3 )  =
     &                         csvf( iHX_Hot_Node_index, 3 )
      csvi( iHX_Cold_Node_index, 1)  =
     &                         csvf( iHX_Cold_Node_index, 1 )
      csvi( iHX_Cold_Node_index, 2 ) =
     &                         csvf( iHX_Cold_Node_index, 2 )

C.....HX connection indices
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iHX_hot_conn_index  = icondx ( iComp_index, 1, 1 ) ! connection to node 1 (hot)
      iHX_cold_conn_index = icondx ( iComp_index, 2, 1 ) ! connection to node 2 (cold)

C-----------------------------------------------------------------------------------
C     Get most recent inlet and outlet temperatures 
C-----------------------------------------------------------------------------------
      fHX_hot_temp_in_F = CONVAR( iHX_hot_conn_index, iTemp )
      fHX_cold_temp_in_F = CONVAR( iHX_cold_conn_index, iTemp )
      fHX_hot_temp_out_F   = CSVF( iHX_Hot_Node_index, iTemp )   ! oC
      fHX_cold_temp_out_F  = CSVF( iHX_Cold_Node_index, iTemp )  ! oC

C-----------------------------------------------------------------------------------
C     Calculate flows
C     Hot side flow is simply calculated from CONVAR
C     Natural convection flow is calculated using function NCHE_flow
C-----------------------------------------------------------------------------------
C.....Total mass flow rate is sum of water mass flow and
C.....glycol mass flow
      fHX_hot_mass_flow_F
     &  = PCONDR( iHX_hot_conn_index ) * 
     &      CONVAR( iHX_hot_conn_index, i1stFlow )
     &  + PCONDR( iHX_hot_conn_index ) *
     &       CONVAR( iHX_hot_conn_index, i2ndFlow )         ! kg/s

C.....Calculate flow rate by calling appropriate function
       call NCHE_flow( iNCHE_number,
     &                 fHX_cold_temp_in_F,
     &                 fHX_cold_temp_out_F,
     &                 fHX_hot_temp_in_F,
     &                 fHX_cold_mass_flow_F   )

C-----------------------------------------------------------------------------------
C Calculate properties of water-glycol solution - if applicable - or water
C at HX hot side.
C-----------------------------------------------------------------------------------
C.....Set mass fraction of glycol in incoming flow to HX. This is a %
C.......for glycol-water mixture properties functions use % for volume fraction
C.........future time-row
      fHX_glycol_mass_fraction_F =
     &            100 * CONVAR( iHX_hot_conn_index, i2ndFlow ) /
     &            ( fHX_hot_mass_flow_F + small )

C.....Find volume fraction of glycol in mixture of incoming flow, based on mass
C.....fraction
C.........future time-row
      fHX_glycol_volume_fraction_F =
     &       glycol_vol_fraction_find( fHX_glycol_mass_fraction_F,
     &              CONVAR( iHX_hot_conn_index, iTemp ))

C.....Specifc heat of fluid inside hot side of HX at the future timestep
C.......future time-row
      fHX_hot_fluid_cp_F =
     &                glycol_water_cp( fHX_glycol_volume_fraction_F,
     &                                fHX_hot_temp_in_F )

C.....Specifc heat of fluid inside hot side of HX at the future timestep
C.......future time-row
       fHX_cold_fluid_cp_F = SHtFld ( 3, fHX_cold_temp_in_F )


C-----------------------------------------------------------------------------------
C Calculate the heat capacity of fluid in the heat exchanger
C-----------------------------------------------------------------------------------
C.....Hot-side fluid - could be a glycol-water solution.
C.......future time-row
      fHX_hot_heat_cap_F = fHX_hot_mass_flow_F
     &                 * fHX_hot_fluid_cp_F            ! W/K

C.....Cold-side fluid (liquid water)
C.......future time-row
      fHX_cold_heat_cap_F = fHX_cold_mass_flow_F
     &                 * fHX_cold_fluid_cp_F           ! W/K


C-----------------------------------------------------------------------------------
C Check that there is flow through the heat exchanger.
C-----------------------------------------------------------------------------------
      call ECLOSE ( fHX_hot_mass_flow_F,0.0,0.0001,bNoFlowHot  )
      call ECLOSE ( fHX_cold_mass_flow_F,0.0,0.0001,bNoFlowCold  )


C-----------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------
      check_ISTATS: select case ( ISTATS )
C-----------------------------------------------------------------------------------

        case ( iTemp )

C-----------------------------------------------------------------------------------
C         Now set temperature matrix coefficients.
C-----------------------------------------------------------------------------------

          no_hot_flow: if ( bNoFlowHot .OR. bNoFlowCold) then

            fHX_Q_F = 0.0
            fHX_mod_eff = 0.0
            fHX_mod_cap_ratio = 0.0

C-----------------------------------------------------------------------------------
C           For no-flow situation, heat transfer is zero. Set outlet
C           temperatures to inlet temperatures. 
C-----------------------------------------------------------------------------------
C............assign ESP-r plant temperature solution matrix coefficients
C
C             |  1   -  -1  -   |  | T_{h_o} |     | 0 |
C             |  -   1  -   -1  |  | T_{c_o} |  =  | 0 |
C                                  | T_{h,i} |
C                                  | T_{c,i} |
C           where:
C
C             |  1   -   3   -  |  | T_{h_o} |     | 5 |
C             |  -   2   -   4  |  | T_{c_o} |  =  | 6 |
C                                  | T_{h,i} |
C                                  | T_{c,i} |

            fCoefficients( 1 ) =  1.0
            fCoefficients( 2 ) =  1.0
            fCoefficients( 3 ) = -1.0
            fCoefficients( 4 ) = -1.0
            fCoefficients( 5 ) =  0.0
            fCoefficients( 6 ) =  0.0


          else


C-----------------------------------------------------------------------------------
C         The performance of natural convection heat exchangers
C         is based on modified performance indices.
C-----------------------------------------------------------------------------------
C...........Modified capacitance Ratio
C...........  (equation 14 in Thevenard, 2008)
            fHX_mod_cap_ratio = fHX_cold_heat_cap_F / fHX_hot_heat_cap_F 
c
C...........But the correlation is apparently only
C...........  valid when 0<=Cr<1.1 (Purdy, private communication)
            fHX_mod_cap_ratio = min( fHX_mod_cap_ratio, 1.1 )
C...........Modified effectiveness, based on experimental results
C...........  (equation 15 in Thevenard, 2008)
             fHX_mod_eff = fHX_eff_CR_a ( iNCHE_number ) *
     &                      fHX_mod_cap_ratio
     &                    + fHX_eff_CR_b ( iNCHE_number ) *
     &                      fHX_mod_cap_ratio**2
C...........Modidified effectiveness has to be less than modified capacitance
C...........ratio (see equ. 17 in Thevenard, 2008)
             fHX_mod_eff = min(fHX_mod_eff, fHX_mod_cap_ratio)

C............The heat transfer rate through the
C............heat exchanger is determined from the
C............modified performance indices and the
C............inlet fluid temperatures 
C..............(equation 13 in Thevenard, 2008)
             fHX_Q_F = fHX_mod_eff
     &              * (fHX_hot_temp_in_F - fHX_cold_temp_in_F) 
     &              * fHX_hot_heat_cap_F

C-----------------------------------------------------------------------------------
C          Assign ESP-r plant temperature solution matrix coefficients
C-----------------------------------------------------------------------------------
C
C              ESP-r matrix has the following form:
C
C                 | a11   -  a13  -  |  | T_{h_o} |     | R1 |
C                 |  -   a22  -  a24 |  | T_{c_o} |  =  | R2 |
C                                       | T_{h,i} |
C                                       | T_{c,i} |
C
C                 |  1   -    3   -  |  | T_{h_o} |     | 5 |
C                 |  -   2    -   4  |  | T_{c_o} |  =  | 6 |
C                                       | T_{h,i} |
C                                       | T_{c,i} |
C
C-----------------------------------------------------------------------------------
C        First self-coupling coefficient (equation 30 in Thevenard, 2008):
C           a11 = hot-side heat capacity
C-----------------------------------------------------------------------------------

            fCoefficients(1) = fHX_hot_heat_cap_F
         
C-----------------------------------------------------------------------------------
C        Second self-coupling coefficient (equation 32 in Thevenard, 2008):
C           a22 = cold-side heat capacity
C-----------------------------------------------------------------------------------
 
            fCoefficients(2) = fHX_cold_heat_cap_F

C-----------------------------------------------------------------------------------
C        First cross-coupling coefficient (equation 31 in Thevenard, 2008):
C           a13 = - hot-side heat capacity
C-----------------------------------------------------------------------------------

            fCoefficients(3) = -fHX_hot_heat_cap_F

C-----------------------------------------------------------------------------------
C        Second cross coupling coefficient (equation 33 in Thevenard, 2008):
C           a24 = - cold-side heat capacity
C-----------------------------------------------------------------------------------
 
            fCoefficients(4) = -fHX_cold_heat_cap_F
 
C-----------------------------------------------------------------------------------
C        Right-hand side coefficient 1 (equation 34 in Thevenard, 2008):
C           R1 = - Future HX heat
C-----------------------------------------------------------------------------------
        
            fCoefficients(5) = -fHX_Q_F

C-----------------------------------------------------------------------------------
C        Right-hand side coefficient 2 (equation 35 in Thevenard, 2008):
C           R2 = Future HX heat
C-----------------------------------------------------------------------------------
            fCoefficients(6) = fHX_Q_F


          endif no_hot_flow

C Store "environment" variables future values
          PCTF(iHX_Hot_Conn_index)=CONVAR(iHX_Hot_Conn_index,1)
          PCTF(iHX_Cold_Conn_index)=CONVAR(iHX_Cold_Conn_index,1)
          PCRF(iHX_Hot_Conn_index)=fHX_hot_heat_cap_F
          PCRF(iHX_Cold_Conn_index)=fHX_cold_heat_cap_F
          PCQF(iHX_Hot_Node_index)= fHX_Q_F
          PCQF(iHX_Cold_Node_index)= fHX_Q_F

C-----------------------------------------------------------------------------------
C       First-phase flow matrix solution
C-----------------------------------------------------------------------------------
        case ( i1stFlow )
C........ assign ESP-r plant flow solution matrix coefficients
C         'mass-flow' scenario:
C           |  1   -   -1  -   |  | M_{h_o} |     |    0       |
C           |  -   1   -   0   |  | M_{c_o} |  =  | fNCHE_flow |
C                                 | M_{h,i} |
C                                 | M_{c,i} |
C         where:
C           |  1   -    3   -  |  | M_{h_o} |     | 5 |
C           |  -   2    -   4  |  | M_{c_o} |  =  | 6 |
C                                 | M_{h,i} |
C                                 | M_{c,i} |

C---------Node 1: flows in and out are equal
          fCoefficients( 1 ) =  1.0
          fCoefficients( 3 ) = -1.0 * PCONDR( iHX_hot_conn_index )
C---------Node 2: flow in and flow out equal zero.
          fCoefficients( 2 ) = 1.0
          fCoefficients( 4 ) = 0.0
C---------RHS coefficients
          fCoefficients( 5 ) = 0.0
          fCoefficients( 6 ) = fHX_cold_mass_flow_F

C-----------------------------------------------------------------------------------
C       2nd-phase (gylcol) solution. Glycol mixtures are supported
C       on the hot-side of the heat exchanger.
C-----------------------------------------------------------------------------------

        case ( i2ndFlow ) 
C........ assign ESP-r plant flow solution matrix coefficients
C         'mass-flow' scenario:
C           |  1   -   -1  -   |  | M_{h_o} |     |    0   |
C           |  -   1   -   -1  |  | M_{c_o} |  =  |    0   |
C                                 | M_{h,i} |
C                                 | M_{c,i} |
C         where:
C           |  1   -    3   -  |  | M_{h_o} |     | 5 |
C           |  -   2    -   4  |  | M_{c_o} |  =  | 6 |
C                                 | M_{h,i} |
C                                 | M_{c,i} |
C---------Node 1: flows in and our are equal
          fCoefficients( 1 ) = 1.0
          fCoefficients( 3 ) = -1.0 * PCONDR( iHX_hot_conn_index )
C---------Node 2: flow in and flow out equal zero.
          fCoefficients( 2 ) = 1.0
          fCoefficients( 4 ) = 0.0
C---------RHS coefficients
          fCoefficients( 5 ) = 0.0
          fCoefficients( 6 ) = 0.0
          
      end select check_ISTATS


C-----------------------------------------------------------------------------------
C Complete trace if trace output requested.
C-----------------------------------------------------------------------------------

      IF(ITC.gt.0 .AND. NSINC.ge.ITC .AND. NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0) THEN
         WRITE(ITU,*) ' Component      ',iComp_index,':'
         WRITE(ITU,*) ' 2 node natural convection heat exchanger'
         WRITE(ITU,*) ' Matrix node(s) ',iHX_Hot_Node_index,
     &                                   iHX_Cold_Node_index
         WRITE(ITU,*) ' Connection(s)  ',iHX_hot_conn_index,
     &                                   iHX_cold_conn_index
         IF(ISTATS.EQ.1) THEN
             WRITE(ITU,*) ' RHS 1 values :'
             WRITE(ITU,*) ' fQ_HX =', fHX_Q_F
             WRITE(ITU,*) ' and it is made up of :'
             WRITE(ITU,*) ' fHX_mod_eff =', fHX_mod_eff
             WRITE(ITU,*) ' fHX_mod_cap_ratio =', fHX_mod_cap_ratio
             WRITE(ITU,*) ' fHX_hot_heat_cap =', fHX_hot_heat_cap_F
             WRITE(ITU,*) ' fHX_hot_mass_flow =', fHX_hot_mass_flow_F
             WRITE(ITU,*) ' fHX_hot_temp_in = ', fHX_hot_temp_in_F
             WRITE(ITU,*) ' fHX_cold_temp_in = ', fHX_cold_temp_in_F
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         num_matrix_coeff = 6
         WRITE(ITU,*)  (fCoefficients(I),I = 1,num_matrix_coeff)
        WRITE(ITU,*) ' Leaving subroutine NCHE_coeff_gen'
      END IF
C Trace output
      if (ITRACE(35) .NE. 0) THEN
      endif


      return

      END SUBROUTINE



C============================= NCHE_flow ===========================================
C

C Date:      November 22, 2005
C Author:    Julia Purdy
C Copyright: Natural Resources Canada
C
C Updated:   December 2008
C Author:    Didier Thevenard
C
C This subroutine is used to evaluate the natural convection flow rate in the
C stratified storage tank-side loop of a solar domestic hot water (SDHW) system. 
C 
C This component requires information input by the user for the Natural Convection
C Heat Exchanger (NCHE) model - component 124 - specifically the heat exchanger
C Flow Rate vs Pressure Drop (DP) coeffiecients, c and d, where 
C flow=c*DP^d.
C
C INPUTS
C    fHX_flow_P_c       ! Natural convection flow rate vs. pressure drop coefficient c
C    fHX_flow_P_d       ! Natural convection flow rate vs. pressure drop coefficient d
C    fTank_height       ! Height of storage tank (m)
C    fHX_height         ! Height of heat exchanger (m)
C    fHX_cold_temp_in   ! HX cold side inlet temperature (oC)
c    fHX_cold_temp_out  ! HX cold side outlet temperature (oC) 
C
C OUTPUTS
C    fNCHE_flow         ! natural convection flow rate around loop (kg/s)
C
C-----------------------------------------------------------------------------------

      SUBROUTINE NCHE_flow( iNCHE_number,
     &                      fHX_cold_temp_in,
     &                      fHX_cold_temp_out,
     &                      fHX_hot_temp_in, 
     &                      fNCHE_flow )
      IMPLICIT NONE

      INCLUDE "plant.h"
      INCLUDE "building.h"
      INCLUDE "NCHE.h"

C Passed Variables
      INTEGER iNCHE_number   ! number of NCHE [1..MAX_NCHE]
      REAL fHX_cold_temp_in  ! HX cold side inlet temperature (oC)
      REAL fHX_cold_temp_out ! HX cold side outlet temperature (oC) 
      REAL fHX_hot_temp_in   ! HX hot side inlet temperature (oC)
      REAL fNCHE_flow        ! resulting flow

C Local Variables
      REAL fTank_temp_mean   ! mean stratified storage tank temperature
      REAL fRHO_tank_mean    ! density of fluid at mean storage tank temperature
      REAL fRHO_HX_cold_in   ! density of water at HX cold side inlet
      REAL fRHO_HX_cold_out  ! density of water at HX cold side outlet
      REAL fRHO_HX_cold_avg  ! average density of cold water in HX
      REAL fRHO_HX_hot_in    ! density of water at HX cold side outlet in no-flow situation
      REAL fNCHE_DP          ! pressure drop due to shear around the natural
                             ! convection loop

      REAL g                 ! acceleration due gravity (m/s^2)
      INTEGER iFluid_type    ! fluid type in storage tank (water=2)

C Called Function
      REAL DENSIT            ! ESP-r function that returns the density (kg/m^3) 
                             ! as function of fluid type (1=air, 2=water) and 
                             ! fluid temperature (C)


C.....Trace & reporting data
      COMMON/tc/itc,icnt
      COMMON/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      INTEGER itc,icnt
      INTEGER itcf,itrace,izntrc,itu
C.....Simulation timestep
      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER ihrp              ! not used in current context
      INTEGER ihrf              ! not used in current context
      INTEGER idyp              ! not used - present day #
      INTEGER idyf              ! not used in current context
      INTEGER idwp              ! not used in current context
      INTEGER idwf              ! not used in current context
      INTEGER nsinc             ! time step number
      INTEGER its,idynow        ! not used in current context
C.....Functions
      REAL fHX_get_stratified_tank_avg_temp
C Constants 
      g = 9.81               ! acceleration due gravity (m/s^2)
      iFluid_type = 2        ! fluid type in cold-side of HX -> water 

C The average tank temperature is retrieved from the stratified tank model for the present timestep. 
C This value is stored in the common block defined in stratified_tank.h

       fTank_temp_mean = fHX_get_stratified_tank_avg_temp
     &   ( iStrat_tank_number(iNCHE_number ) )  ! Average tank temperature (oC)

C-----------------------------------------------------------------------------------
C   Calculate the density of the storage tank
C-----------------------------------------------------------------------------------
C..... DENSIT is a standard ESP-r subroutine that returns density (kg/m^3) as function of
C..... fluid type (1=air, 2=water) and fluid temperature (oC)

      fRHO_tank_mean = DENSIT( iFluid_type, fTank_temp_mean ) 

C-----------------------------------------------------------------------------------
C   Calculate the densities of the fluid at the heat exchanger cold-side
C   inlet and outlet
C-----------------------------------------------------------------------------------
C..... DENSIT is a standard ESP-r subroutine that returns density (kg/m^3) as 
C..... function of fluid type (1=air, 2=water) and fluid temperature (oC)

      fRHO_HX_cold_in  = DENSIT( iFluid_type,fHX_cold_temp_in ) 
      fRHO_HX_cold_out = DENSIT( iFluid_type,fHX_cold_temp_out )       

C-----------------------------------------------------------------------------------
C   Pressure drop around the loop (equation 7 in Thevenard, 2008)
C-----------------------------------------------------------------------------------

        fNCHE_DP = g *
     &    (
     &      ( fRHO_tank_mean * fTank_height (iNCHE_number) ) -
     &      ( fRHO_HX_cold_in * ( fHX_height (iNCHE_number) / 2 )) -
     &      ( fRHO_HX_cold_out * 
     &        (fTank_height(iNCHE_number) - fHX_height(iNCHE_number)/2))
     &     )

C-----------------------------------------------------------------------------------
C     Check for possible negative pressure drop around the loop. 
C     If that's the case, redo the calculation with fHX_cold_temp_out set to 
C     fHX_hot_temp_in
C-----------------------------------------------------------------------------------

      if (fNCHE_DP .LE. 0.0 ) then
      fRHO_HX_hot_in = DENSIT( iFluid_type, fHX_hot_temp_in )       
        fNCHE_DP = g *
     &    (
     &      ( fRHO_tank_mean * fTank_height (iNCHE_number) ) -
     &      ( fRHO_HX_cold_in * ( fHX_height (iNCHE_number) / 2 )) -
     &      ( fRHO_HX_hot_in * 
     &        (fTank_height(iNCHE_number) - fHX_height(iNCHE_number)/2))
     &    )
      endif

      if (fNCHE_DP .LT. 0.0 ) fNCHE_DP = 0.0

C-----------------------------------------------------------------------------------
C   Natural convection mass flow rate (equation 8 in Thevenard, 2008)
C-----------------------------------------------------------------------------------

      fNCHE_flow = (fHX_flow_P_c ( iNCHE_number ) *
     &               fNCHE_DP ** fHX_flow_P_d ( iNCHE_number ) )

C.....Convert from L/min to kg/s, using the average density of water
C.....in the NCHE
      fRHO_HX_cold_avg = (fRHO_HX_cold_in+fRHO_HX_cold_out)/2.
      fNCHE_flow = fNCHE_flow*fRho_HX_cold_avg/1000./60.

      IF(ITC.gt.0 .AND. NSINC.ge.ITC .AND. NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0) THEN
         WRITE(ITU,*) ' NCHE_flow subroutine:'
         WRITE(ITU,*) ' fHX_flow_P_c (iNCHE_number) =',
     &                    fHX_flow_P_c (iNCHE_number)   
         WRITE(ITU,*) ' fHX_flow_P_d (iNCHE_number) =',
     &                    fHX_flow_P_d (iNCHE_number)       
         WRITE(ITU,*) ' fNCHE_DP =', fNCHE_DP
         WRITE(ITU,*) ' fNCHE_flow =', fNCHE_flow
         WRITE(ITU,*) ' leaving NCHE_flow subroutine:'
      end if


      return

      END SUBROUTINE


C=================== fHX_get_stratified_tank_height ================================

C fHX_get_stratified_tank_height is a real function which returns the 
C height (m) of the stratified tank model.
C This stratified tank is connected to the cold side (Node 2) of the 
C NCHE - Natural Convection Heat Exchanger

      FUNCTION fHX_get_stratified_tank_height( iStrat_tank_CompIndex )

      IMPLICIT NONE
      INCLUDE "building.h"
      INCLUDE "plant.h"

      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component
      INTEGER iStrat_tank_CompIndex
      REAL fHX_get_stratified_tank_height

      fHX_get_stratified_tank_height = 
     &                        ADATA( iStrat_tank_CompIndex,2 )

      RETURN
      END

C=================== fHX_get_stratified_tank_avg_temp ==============================

C fHX_get_stratified_tank_avg_temp is a real function which returns the 
C average temperature (degrees C) of the stratified tank model for the 
C current timestep.

      FUNCTION fHX_get_stratified_tank_avg_temp( iStrat_tank )

      IMPLICIT NONE
      INCLUDE "stratified_tank.h"

        INTEGER iStrat_tank
        REAL fHX_get_stratified_tank_avg_temp

        fHX_get_stratified_tank_avg_temp =
     &                  TAVGF_STANK( iStrat_tank )

      RETURN
      END 
