C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

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
C
C======================================================================
C================= Hydrogen_PEMFC.F ===================================
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.

C This file contains procedures associated with modelling a hydrogen
C PEM fuel cell system
C
C It contains the following procedures:
C
C    - H2_PEMFC_Coeff_Gen: Top level solution routine that
C        returns the plant component matrix coefficients
C        expected by ESP-r.
C
C    - H2_PEMFC_Q_balance: Supervisory routine for the
C        PEMFC energy balance.
C
C    - H2_PEMFC_Collect_Data: Collects & error-traps inputs to
C        the PEMFC model
C
C    - H2_PEMFC_H3Kreports_module: Transports data to H3Kreports.
C
C    - Char_H2_PEMFC_Empirical: Procedure evaluating empirical
C        correlations for the H2 PEMFC model
C
C======================================================================
C======================================================================

C---------------- H2_PEMFC_Coeff_Gen ----------------------------------
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.
C
C ABSTRACT:
C This routine characterizes the heat balance inside a hydrogen-
C fueled PEM fuel cell. It performs the following tasks:
C
C
C  -> On the first call, model inputs are collected
C     (see procedure H2_PEMFC_Collect_Data )
C
C  -> The control scheme and signal from an extermal controller
C     are collected
C
C  -> The time spent by the unit in each of four operational
C     modes ( standby / startup /normal operation / cooldown )
C     is determined (see subordinate routine Determine_Cogen_Mode )
C
C  -> If the controller has specified the system's heat or
C     power output, the fuel (hydrogen) flow required to
C     meet the controller's demands is calculated ( see
C     subordinate routine Solve_Cogen_Model)
C
C  -> The system's response to the actual amount of hydrogen
C     supplied by the mechanical plant is determined. ( see
C     subordinate routine Char_H2_PEMFC_Empirical )
C
C  -> The energy balance matrix coefficients are calculated and
C     stored in the fCoefficients array.
C
C  -> Miscellaneous data decribing operation of the unit are
C     stored in the fReports_********* commons, which are
C     later transported by the H2_PEMFC_H3Kreports_module
C     to the h3kreports module.
C
C Inputs:
C
C  iComponent       - index of component in plant network
C  iPlant_Matrix    - index of plant component matrix to be solved 
C
C Outputs
C
C  fCoefficients    - array containing components matrix
C                     coefficients
C
C----------------------------------------------------------------------
      subroutine H2_PEMFC_Coeff_Gen(
     &            iComponent,
     &            fCoefficients,
     &            iPlant_Matrix )
      implicit none
#include "building.h"
#include "plant.h"

#include "cetc_cogen.h"
#include "Hydrogen_PEMFC.h"
#include "chemical_properties.h"
#include "Hydrogen_demand_controller.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent        ! pointer to component in plant network
      real fCoefficients(mpcoe) ! plant matrix coefficints
      integer iPlant_Matrix     ! index of plant matrix to be solved 
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu

C.....Network & control data
      common/c9/npcomp,nci,cdata
      integer npcomp            ! number of plant components
      integer nci(mpcom)        ! number of controls / component
      real cdata(mpcom,mmiscd)  ! control data for each component

      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

C.....Electrical power use of plant 'hybrid' components
      common/elpcp/npel, pfp, ipfp, pwrp, bvoltp, iphp
      integer npel              ! number of "elctrified" plant components
      real pfp(mpcom)           ! power factor of componet i (0->1)
      integer ipfp(mpcom)       ! integer indicating if power factor lags (-1)
                                ! or leads (+1)
      real pwrp(mpcom)          ! real power consumption of component
                                ! (generation is -ive)
      real bvoltp(mpcom)        ! opertional voltage of component
      integer iphp(mpcom)       ! phase component is connected to
                                ! ( dummy variable )

      real PQ_dummy, PA_dummy   ! Dummy variables for elec. network
      integer IEMODEL           ! flag passed to elec. power domain.

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Plant interation data
      common/piter/maxitp,perrel,pertmp,perflx,permfl,itrclp,
     &             icsv(mpnode,mpvar),csvi(mpnode,mpvar)
      integer maxitp            ! not used in current context
      integer itrclp            ! not used in current context
      integer icsv              ! flag marking nodes for iteration
      real perrel               ! not used in current context
      real pertmp               ! not used in current context
      real perflx               ! not used in current context
      real permfl               ! not used in current context
      real csvi                 ! 'initial' values of state variables
                                ! at start of iteration
      common/pcres/
     &     qdata(mpcom),
     &     pcaout(mpcom,mpcres),
     &     napdat(mpcom)
      real qdata     ! not used in current context
      real pcaout    ! not used in current context
      integer napdat ! # of plant additional outputs


C.....Plant present and future state variables
      common/pcval/csvf,csvp
      real csvf(mpnode,mpvar)   ! future time-row state variables
      real csvp(mpnode,mpvar)   ! present time-row state variables

C.....Common for additional iteration criteria
      common / pAdd_iter_criteria /
     &   iPlt_Output_Iter_Flag,
     &   fPlt_Output_Init_Val,
     &   fPlt_Output_Tol
      integer iPlt_Output_Iter_Flag (mpcom,mpcdat) ! flag denoting iteration is req'd
                                                   ! for plt additional outputs
      real fPlt_Output_Init_Val ( mpcom,mpcdat)    ! initial value of plt additional output
      real fPlt_Output_Tol (mpcom,mpcdat)          ! tolerance for convergence


C.....Plant component connection data
      common/pcond/convar, icontp, icondx
      real    convar(mpcon,mconvr)        ! state varibles for connections
      integer icontp(mpcon)               ! type of each connection
      integer icondx(mpcom,mnodec,mpconc) ! pointer to connections for each
                                          ! component/node

C.....Miscellaneous plant data
      common/pcvar/pctf,pcrf,puaf,pcqf,pcntmf,pctp,
     &      pcrp,puap,pcqp,pcntmp
      real pctf(mpcon)
      real pcrf(mpcon)
      real puaf(mpnode)
      real pcqf(mpnode)
      real pcntmf(mpcom)      ! future time row plant containment temp (oC)
      real pctp(mpcon)
      real pcrp(mpcon)
      real puap(mpnode)
      real pcqp(mpnode)
      real pcntmp(mpcom)
      
C.....Common containing plant componet mass diversion ratios 
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),ipc2(mpcon),
     &            ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      integer npcon           ! number of inter-component connections
      integer ipc1            ! component number defining the receiving
                              !    component
      integer ipn1            ! node number defining the receiving component
      integer ipct            ! plant component inter-connection type
      integer ipc2            ! component number defining the sending
                              !    component
      integer ipn2            ! node number defining the sending component
      real    pcondr          ! ratio of mass flow rate through the connection
                              !    and the mass flow rate entering the receiving
                              !    node for each plant component inter-connection
      real    pconsd          ! supplementary data for plant component inter-
                              !    connection

C.....Additional data
      common/pcdat/
     &     pcdatf(mpcom,mpcdat),
     &     pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)

C.....Time
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1              ! not used in current context
      integer ism1              ! not used in current context
      integer isd2              ! not used in current context
      integer ism2              ! not used in current context
      integer isds              ! not used in current context
      integer isdf              ! not used in current context
      integer ntstep            ! number of building steps/hour
      common/pctstp/ntstpp
      integer ntstpp            ! number of plant timesteps / building timestep

C.....Common storing status of plant initialization.
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)

C.....Simulation timestep
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp              ! not used in current context
      integer ihrf              ! not used in current context
      integer idyp              ! present day #
      integer idyf              ! not used in current context
      integer idwp              ! not used in current context
      integer idwf              ! not used in current context
      integer nsinc             ! time step number
      integer its               ! not used in current context


C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------

C.....Simulation properties
      real fTS_duration         ! timestep duration

C.....Model syntax
      integer iH2_Node          ! Index of hydrogen & stack node
                                !    in plant network
      integer iWater_Node       ! Index of cooling water node in plant
                                !    network
      integer iH2_Conn_Index    ! Index of hydrogen connection in plant
                                !    network
      integer iWater_Conn_Index ! Index of water connection in plant
                                !    network

C.....Model type
      integer iModel_Type       ! Flag indicating which model is in use.


C.....Connection properties

      real fH2_Conn_Flow_P      ! Flowrate of incomming hydrogen  (kg/s)
                                !     on present time row
      real fH2_Conn_Flow_F      ! Flowrate of incomming hydrogen  (kg/s)
                                !     on future time row
      real fH2_Conn_Temp_P      ! Temperature of incomming hydrogen (oC)
                                !     on present time row
      real fH2_Conn_Temp_F      ! Temperature of incomming hydrogen (oC)
                                !     on future time row

      real fWater_Conn_Flow_P   ! Flow rate of incomming water (kg/s)
                                !     on present time row
      real fWater_Conn_Flow_F   ! Flow rate of incomming water (kg/s)
                                !     on future time row
      real fWater_Conn_Temp_P   ! Temperature of incomming water (oC)
                                !     on present time row
      real fWater_Conn_Temp_F   ! Temperature of incomming water (oC)
                                !     on future time row

      real fWater_conn_Cp_F     ! Heat capacity of incomming water (J/kg oC)
                                !     on future time row

C.....State variables
      real fH2_Node_Temp_P      ! Present time row H2+stack node temp (oC)
      real fH2_Node_Temp_F      ! Future time row H2+stack node temp (oC)

      real fWater_Node_Temp_P   ! Present time row water node temp (oC)
      real fWater_Node_Temp_F   ! Future time row water node temp (oC)

C.....Containment data
      real fTemp_Room           ! temperature of containing room (oC)

C.....High-level control flags
      integer iControl_Method   ! flag indicating which control scheme is
                                !     in use.

      real fControl_Signal      ! Control signal from external controller.

      real fDesired_Fuel_Norm   ! Estimated hydrogen flow required
                                !     to match control signal in
                                !     normal operation
      real fDesired_Fuel_Flow_TSavg
                                ! Estimated average hydrogen flow over
                                !     entire time step to meet
                                !     controller requests.

      real fFuel_Flow_used      ! Amount of supplied hydrogen used by
                                !   PEMFC (kg/s)

      real fFuel_Flow_remain    ! Amount of supplied hydrogen unused
                                !   by PEMFC (kg/s)

      real fFuel_mode_requirements ! Fuel flow requirements for current
                                ! operating mode (kg/s)

      logical bControl_Active   ! Flag indicating that controller has
                                !     requested  output

C.....Low-level control flags
      logical bNo_cooling_water  ! Flag indicating if there is no
                                 !   cooling water flowing though
                                 !   the system.
      logical bCooling_W_ToHot   ! Flag indicating cooling water is to hot
                                 !   for safe operation
      logical bNo_H2_flow        ! Flag indicating that there is no hydrogen
                                 !   flowing in the unit.

C.....Operating mode parameters

      integer iOperating_mode_P  ! flag indicating which mode the unit is
                                 !     operating in on present time row

      integer iOperating_mode_F  ! flag indicating which mode the unit will
                                 !     operate in on future time-row.

      integer iTarget_mode       ! flag indicating which operating mode has
                                 !     been requested by the external controller.

      integer iSystem_Status     ! flag indicating if system faults have
                                 !     occured

      real fOP_target            ! Operating point requested by high-level
                                 !     controller.

                                       
      real fOP_time_P            ! time spent in the current operating mode,
      real fOP_time_F            ! at beginning & end of timestep
      real fOP_time

      real fTS_mode_fractions ( iOP_Mode_Count )
                                 ! fraction of time step spent in each mode

      logical bTS_modes( iOP_Mode_Count )
                                 ! logical flags indicating which operating
                                 !    modes the system will spend time in



      logical bStartup_Complete  ! Flag indicating startup mode will be completed
                                 !      on the present timestep


      real fMIN_Req_Fuel_Flow_TSavg
                                 ! Minimum required fuel supply for unit
                                 !       to operate normally. (kg/s)

      logical bFuel_Shortage     ! Flag indicating that insufficient fuel
                                 !      is available

C.....Normal operation data
      real fNorm_OP_Fuel_Supply   ! Rate of fuel flow for normal operating
                                  !       mode (kg/s)
      real fNorm_OP_DC_Power      ! Power produced by unit in normal operation (W)
      real fNorm_OP_Heat          ! Heat produced by unit in normal operation (W)
      real fNorm_OP_AC_Power_use  ! rate of AC power use during normal operation (W)
      real fNorm_OP_Skin_Losses   ! rate of heat loss from the unit
                                  !     when in normal operation (W)
      real fNorm_OP_Stack_Current ! Stack current (amps)
      real fNorm_OP_Stack_Voltage ! Stack voltage (volts)


C.....Timestep-averaged operating data

      real fDC_Power_TS_average  ! Average power produced by unit over
                                 !    timestep (W)

      real fAC_Power_TS_average  ! Average AC power used by unit's auxiliary
                                 !    systems over timestep (W)

      real fHeat_TS_average      ! Average heat recovery from unit over
                                 !    timestep (W)

      real fCurrent_TS_Average   ! Average DC current produced by unit over
                                 !    timestep (A)


C.....Model historical operation data
      real fStack_Accum_Time_o  ! accumulated operating time (s)
                                !     at start of simulation
      real fStack_Accum_Time_P  ! stack accumulated operating
                                !     time (s) at start of current
                                !     timestep
      real fStack_Accum_Time_F  ! stack accumulated operating
                                !     time (s) at start of next
                                !     timestep

C.....Misc local variables
      logical bNums_are_close   ! Logical result of close-to-zero
                                !     comparisons

      logical bFuel_Flow_Determined ! loop-control indicating that
                                !     the system's fuel flow has been
                                !     determined.

      real fH2_heating_value    ! Lower heating value of hydrogen (J/kg)
      save fH2_heating_value

      integer ii                ! Counter

C----------------------------------------------------------------------
C     Named constants
C----------------------------------------------------------------------
      integer iPropTemp         ! named constant for temperature properity
      integer iProp1stFlow      ! named constant for first phase flow properity
      integer iProp2ndFlow      ! named constant for second phase flow properity
      integer iPropH2Flow       ! named constant for hydrogen flow
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )

      integer iInitialize           ! Named constant indicating h3kreports
                                    ! modules should self-initialize
      integer iReport               ! Named constant indicating h3kreports
                                    ! modules should report data
      parameter ( iInitialize  = 1,
     &            iReport      = 2 )

C.....Enable debugging output
      logical bDebug

C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real shtfld                 ! ESRU function returning the
                                  !   specific heat of air, water vapor,
                                  !   or water.
      logical bLinearize          ! Function returning a linear
                                  !   equation [y=Ax+b] passing
                                  !   through two data points (x1,y1) &
                                  !   (x2,y2)
      logical bTS_complete        ! Logical function indicating if
                                  !   unit's operation has been characterized
                                  !   over the entire timestep, and the
                                  !   fraction of the time-step remaining.
      real fEval_Compound_HV      ! Function returning the heating
                                  !   value of a compound (J/kmol)
      real fEval_Compound_MM      ! Function returning the molar mass
                                  !   of a compound (kg/kmol)
C----------------------------------------------------------------------
C     Initialization
C----------------------------------------------------------------------

C.....Debugging flag
      bDebug = .false.

      if (bDebug) write(ITU,*)
     &   "Entering subroutine H2_PEMFC_Q_balance()"

C.....Has empirical model been specified?
C.....Collect model type (value of NPCDAT(i,4))
      iModel_Type = NPCDAT(iComponent,4)

C.....Error trap -> is model type supported?
      if ( iModel_Type .ne. iMODEL_Empirical ) then
         write(ITU,*) 'H2 PEMFC(Hydrogen_PEMFC.F):'
         write(ITU,*) ' -Passed model type (', iModel_Type,
     &                ' ) is unsupported.'
         write(ITU,*) '  Check call in MZPMSU (pmatrx.f) '
         stop 'Hydrogen_PEMFC.F: Fatal error!'
      endif


C----------------------------------------------------------------------
C     Determine component location in plant network topology, and
C     mark component node for iteration
C
C     -> Get node index, and mark nodes for iteration.
C
C      NPCDAT(i,9):
C       row and column number defining location of component `i'
C       sub-matrix template in the overall plant network matrix. It is
C       used to identify the location of the current component's nodes
C       within the global plant matrix.
C
C      ICSV(i,j)
C       flag indicating that node `i' is marked for iteration for state
C       variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C       j=3 for 2nd phase mass flow.
C
C      CSVI(i,j)
C       initial value for judging whether iteration required. Same
C       indices as ICSV.
C
C      CSVF(i,j)
C       future time-row solution variable for plant. Same indices
C       as ICSV.
C----------------------------------------------------------------------

      iH2_Node = NPCDAT ( iComponent, 9 )
      iWater_Node = iH2_Node + 1

C.....First node -> iterate on hydrogen flow
      icsv( iH2_Node, iPropH2Flow ) = 1
      csvi( iH2_Node, iPropH2Flow ) = csvf( iH2_Node, iPropH2Flow )


      if ( bDebug ) then
         write (itu, *) '  water-node:'
         write (itu, *) '  -CSVP: ', csvp( iWater_node, iPropTemp )
         write (itu, *) '  -CSVI: ', csvi( iWater_node, iPropTemp )
         write (itu, *) '  -CSVF: ', csvf( iWater_node, iPropTemp )
      endif
C.....Second node -> iterate on temperature
      icsv( iWater_node, iPropTemp ) = 1
      csvi( iWater_node, iPropTemp ) =
     &                      csvf( iWater_node, iPropTemp )

C.....Get timestep duration (seconds)
C.....  -> ntstep: # building time steps / hour
C.....  -> ntstpp: # plant time steps / building time step

      fTS_duration = 3600.0 / ( ntstep * ntstpp )

C----------------------------------------------------------------------
C     If this is the first call to H2_PEMFC_Q_balance, perform
C     initialization actions.
C----------------------------------------------------------------------
      if ( .not. bInitialized ( iComponent ) ) then

C........Collect time-invariant data in ADATA, BDATA arrays
         call H2_PEMFC_Collect_Data ( iComponent, iModel_Type, bDebug )

C........Initialize Stirling engine module for H3K reports
         call H2_PEMFC_H3Kreports_module ( iComponent,
     &                                     iInitialize,
     &                                     iModel_Type   )

C........Number of additional output (pcdatp/pcdatf) array
C........locations used
         nAPDat (iComponent) = 5

C........Calculate the heating value of hydrogen:
C........ -> use heating value and molar mass functions
C........    from chemical_properties.F
C........
         fH2_heating_value = fEval_Compound_HV ( iHydrogen, iLHV )
     &        / fEval_Compound_MM ( iHydrogen )

C........Set flag to ensure initialization is not repeated
         bInitialized ( iComponent ) = .true.

C........Set tolerance for convergence on hydrogen demand
         fPlt_Output_Tol(iComponent,1) = 1.0E-07

      endif
C------------------------------------------------------------------------
C     Is this the first timestep of the simulation?
C------------------------------------------------------------------------
      if ( nsinc .eq. 1 ) then
C------------------------------------------------------------------------
C        PCDatP array has not been populated. Set initial starting mode
C        to 'inoperative'
C------------------------------------------------------------------------
         iOperating_mode_P = iOP_inoperative               ! (-)
         fOP_time_P        = 0.0                           ! (s)
         iSystem_Status    = iStatus_normal                ! (-)

         fStack_Accum_Time_P = fInitial_accum_time ( iComponent ) ! (s)

C........Mass & Energy flow initial conditions for transient behavior
C         fP_net_init          = 0.0                      ! (W)
C         fQ_Cogeneration_init = 0.0                      ! (W)
C         fN_fuel_flow_init    = 0.0                      ! (kg/s)

      else
C------------------------------------------------------------------------
C        Collect historical operating mode data from PCDatP arrays:
C------------------------------------------------------------------------
         iOperating_mode_P    = int(PCDatP(iComponent,2))   ! (-)
         fOP_time_P           = PCDATP(iComponent,3)        ! (s)
         iSystem_Status       = int(PCDatP(iComponent,4))   ! (-)
         fStack_Accum_Time_P  = PCDatP(iComponent, 5 )      ! (s)

C........Mass & Energy flow initial conditions for transient behavior
C         fN_fuel_flow_init    = PCDATP(iComponent,3)        ! (kg/s)
C         fP_net_init          = PCDATP(iComponent,4)        ! (W)
C         fQ_Cogeneration_init = PCDATP(iComponent,5)        ! (W)

      endif

C----------------------------------------------------------------------
C     Collect connection & containment properties
C----------------------------------------------------------------------
C.....Get hydrogen & cooling water connection index
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iH2_Conn_Index    = icondx ( iComponent, 1, 1 )
      iWater_Conn_Index = icondx ( iComponent, 2, 1 )

C.....Get hydrogen temperature and mass flow rate
      fH2_Conn_Temp_F = convar ( iH2_Conn_Index, iPropTemp )     ! oC
      fH2_Conn_Flow_F = convar ( iH2_Conn_Index, iPropH2Flow )
     &            * pcondr( iH2_Conn_Index )  ! kg/s
      
C.....Get cooling water temperature & mass flow rate
      fWater_Conn_Temp_F = convar ( iWater_Conn_Index, iPropTemp )     ! oC
      fWater_Conn_Flow_F = convar ( iWater_Conn_Index, iProp1stFlow )
     &            * pcondr ( iWater_Conn_Index )! kg/s

C.....Get containing room temperature
      fTemp_Room = PCnTmF ( iComponent )  ! oC

C.....Calculate cooling water heat capacity:
C.....  specific heat capactiy = Shtfld ( X , Temperature ) (J/kg oC)
C.....  where X=1 -> air, X=2 -> vapor, X=3 -> liq. water

      fWater_conn_Cp_F = fWater_Conn_Flow_F
     &             * SHtFld ( 3, fWater_Conn_Temp_F ) ! W/ K

C----------------------------------------------------------------------
C     Check that sufficient cooling is provided.
C
C     -> is there cooling water flowing through unit?
C     -> is the temperature of the node's cooling water
C        node less than maximum operating temperature?
C
C----------------------------------------------------------------------
      call eclose ( fWater_Conn_Flow_F, 0.0, 1.0E-05, bNums_are_close)
      if ( bNums_are_close .or. fWater_Conn_Flow_F .lt. 0.0 ) then
C........there is no cooling water flow
         bNo_cooling_water = .true.
      else
         bNo_cooling_water = .false.
      endif


      if ( csvp ( iWater_Node, iPropTemp )
     &             .gt. fCW_Temp_MAX( iComponent ) )then
C........Cooling water is too hot.
         bCooling_W_ToHot = .true.
      else
         bCooling_W_ToHot = .false.
      endif

C----------------------------------------------------------------------
C     Check that hydrogen flow exists.
C----------------------------------------------------------------------
      call eclose ( fH2_Conn_Flow_F, 0.0, 1.0E-07, bNums_are_close )

      if ( bNums_are_close .or. fH2_Conn_Flow_F .lt. 0.0 ) then
C........There's no hydrogen flow!
         bNo_H2_flow = .true.
      else
         bNo_H2_flow = .false.
      endif



C----------------------------------------------------------------------
C     MODEL TOPOLOGY:
C
C                             .------------> Heat loss
C                             |
C                    ---------------------
C     Hydrogen       |       Fuel        |
C     -------------->|       cell        |
C    (connection 1)  |       stack       |
C                    ---------------------
C                              A
C                              |
C                              |  Heat transfer
C                              |
C                              V
C                     ---------------------
C     Cooling water   |  Heat exchanger   |
C     --------------->|      water        |--------------->
C    (connection 2)   |      side         |
C                     ---------------------
C
C
C     Coefficient locations:
C
C        |<-- self -->|<- cross ->| |<- sol'n ->|   |<-Present
C        |  coupling  |  coupling | |   vector  |   |  time & known
C
C        |   1     2     5    --  | |    v1     | = |  7 |
C        |   3     4    --     6  | |    v2     |   |  8 |
C                                   |    vi     |
C                                   |    vj     |
C
C----------------------------------------------------------------------

C----------------------------------------------------------------------
C     Determine which matrix is being solved, and set coefficients
C     accordingly.
C----------------------------------------------------------------------
      if ( iPlant_Matrix .eq. iProp1stFlow ) then

C----------------------------------------------------------------------
C        Plant first-flow (ie water/air) matrix is to be solved.
C        Water flows through 2nd node of H2 PEMFC model, &
C        conservation of mass applies. Therefore, state equation is:
C
C        | 1.0    0.0    0.0   0.0 | | mass flow, 1st node| = | 0.0 |
C        | 0.0    1.0    0.0  -1.0 | | mass flow, 2nd node|   | 0.0 |
C                                    | mass flow, conn. 1 |
C                                    | mass flow, conn. 2 |
C
C
C----------------------------------------------------------------------
C........Self coupling coefficients
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0
         fCoefficients(4) =  1.0
C........cross coupling coefficients
         fCoefficients(5) =  0.0
         fCoefficients(6) = -1.0  * pcondr ( iWater_Conn_Index )
C........RHS vector
         fCoefficients(7) =  0.0
         fCoefficients(8) =  0.0

      elseif (iPlant_Matrix .eq. iProp2ndFlow ) then
C----------------------------------------------------------------------
C        Plant second-flow (ie water vapor) matrix is to be solved.
C        H2 PEMFC model does not support explicit modelling of water
C        vapor flows. Thus, state equation is:
C
C        | 1.0    0.0    0.0   0.0 | | mass flow, 1st node| =  | 0.0 |
C        | 0.0    1.0    0.0   0.0 | | mass flow, 2nd node|    | 0.0 |
C                                    | mass flow, conn. 1 |
C                                    | mass flow, conn. 2 |
C
C
C----------------------------------------------------------------------
C........Self coupling coefficients
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0
         fCoefficients(4) =  1.0
C........cross coupling coefficients
         fCoefficients(5) =  0.0
         fCoefficients(6) =  0.0
C........RHS vector
         fCoefficients(7) =  0.0
         fCoefficients(8) =  0.0
      elseif (iPlant_Matrix .eq. iPropH2Flow ) then
C----------------------------------------------------------------------
C        Plant hydrogen flow matrix is to be solved. H2 PEMFC system
C        uses all of the hydrogen supplied to it, and none is permitted
C        to flow out of the unit. Thus, state equation is:
C
C        | 1.0    0.0    0.0   0.0 | | mass flow, 1st node| =  | 0.0 |
C        | 0.0    1.0    0.0   0.0 | | mass flow, 2nd node|    | 0.0 |
C                                    | mass flow, conn. 1 |
C                                    | mass flow, conn. 2 |
C
C
C----------------------------------------------------------------------
C........Self coupling coefficients
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0
         fCoefficients(4) =  1.0
C........cross coupling coefficients
         fCoefficients(5) =  0.0
         fCoefficients(6) =  0.0
C........RHS vector
         fCoefficients(7) =  0.0
         fCoefficients(8) =  0.0
      
      else
C-------------------------------------------------------------------------
C        High-level control interfaces:
C
C        -> check first control variable to determine if controller has
C           (i) deactivated engine, or is using (ii) the dimensionless
C           interface, (iii) the electrical load following interface, or
C           (iv) the thermal load following interface.
C
C        -> collect control signal from second control variable.
C
C-------------------------------------------------------------------------

C........Collect control signals:
C........ -> Flag indicating which control interface is in use:
         iControl_method = int ( CData ( iComponent,1 ) )
C........ -> Control signal
         fControl_signal = CData ( iComponent, 2 )

C........Testing & debugging code       
!          if ( nsinc .le. 20 ) then
!             iControl_method = iEngine_deactivated
!          elseif ( nsinc .lt. 40 ) then
!             iControl_Method = iDimensionless_control
!             fControl_Signal = 1.0
!          elseif ( nsinc .lt. 60 ) then
!             iControl_Method = iDimensionless_control
!             fControl_Signal = 1.0 -
!         &     float( nsinc - 40 ) / 20.0
!          elseif ( nsinc .lt. 80 ) then
!             iControl_Method = iPower_out_controlled
!             fControl_signal =
!         &    fNominal_Power_Output ( iComponent )
!         &      - float( nsinc - 60 ) / 20.0
!         &      * fNominal_Power_Output ( iComponent )
!          elseif ( nsinc .lt. 100 ) then
!             iControl_Method = iHeat_out_controlled
!             fControl_signal =
!         &    fNominal_Power_Output ( iComponent )
!         &      - float( nsinc - 80 ) / 20.0 *
!         &     * fNominal_Power_Output ( iComponent )
!          else
!             iControl_method = iEngine_deactivated
!          endif


C........Is this code necessary?
         if ( iControl_method .eq. iEngine_deactivated ) then
C...........Controller has deactivated the engine.
            bControl_Active = .false.
         else
            bControl_Active = .true.
         endif


         if ( iControl_method .eq. iDimensionless_control ) then

C...........If fuel flow control interface is active, control signal
C...........must lie between 0 and 1. Set 'Desired fuel flow' variable
C...........for hydrogen supply controller.

            if ( fControl_signal .gt. 1.0 ) then

               fDesired_Fuel_Norm  = fFuel_Flow_MAX (iComponent ) ! kmol/s

            elseif ( fControl_signal .lt. 0.0 ) then

               fDesired_Fuel_Norm = fFuel_Flow_MIN ( iComponent )! kmol/s

            else

               fDesired_Fuel_Norm = fFuel_Flow_MIN (iComponent ) +
     &            fControl_signal * (
     &               fFuel_Flow_MAX (iComponent)
     &             - fFuel_Flow_MIN (iComponent)
     &            )
            endif

         elseif ( iControl_method .eq. iPower_out_controlled .or.
     &            iControl_method .eq. iHeat_out_controlled ) then

C...........Controller is specifying electric or thermal output.
C...........System's required fuel flow will have to be determined.
C...........Check that control signal is not less than zero.
            if ( fControl_signal .lt. 0.0 ) fControl_signal = 0.0

C...........Assign control signal directly to target operating point
            fOP_target = fControl_signal  ! Watts, electric or thermal.

         else
C...........Controller has requested an unsupported control mode. Deactivate
C...........unit and issue warning.
            bControl_Active = .false.

            ! Warning goes here....

         endif

C-------------------------------------------------------------------------
C        Low-level controls
C
C        Determine which state the component is in.
C
C        -> Make sure cooling water is flowing, and that cooling water
C           outlet temperature does not exceed maximum permissible
C           cooling water temperature. NOTE: the calculated value of
C           the cooling water outlet temperature from the last time-step
C           is used.
C-------------------------------------------------------------------------
C-------------------------------------------------------------------------
C        Determine desired 'target' operating mode:
C        Is it safe to operate system?
C-------------------------------------------------------------------------
         if ( bNo_cooling_water .or. bCooling_W_ToHot ) then
C-------------------------------------------------------------------------
C           Overheating danger exists. Initiate shutdown.
C-------------------------------------------------------------------------
            iSystem_Status = iStatus_CW_fault
         else
C-------------------------------------------------------------------------
C           Check to see if power has been requested.
C-------------------------------------------------------------------------
            if ( bControl_Active ) then
C..............Power requested. - unit will operate
               iTarget_mode = iOP_normal_operation
            else
C..............No power requested. Turn unit off.
               iTarget_mode = iOP_inoperative
            endif
         endif


         if ( bDebug ) then
            write (itu,*) 'System Status: ', iSystem_Status
            write (itu,*) 'Intended Frac of TS spent in each mode: ',
     &         fTS_mode_fractions(iOP_inoperative),
     &         fTS_mode_fractions(iOP_startup),
     &         fTS_mode_fractions(iOP_normal_operation),
     &         fTS_mode_fractions(iOP_shutdown)

         endif

C-------------------------------------------------------------------------
C        Two steps must be used to characterize the fuel cell's operation:
C
C          -> Determine the amount of hydrogen required by the stack to
C             meet electrical & thermal demand.
C
C          -> Determine the system's operating point based on the actual
C             flow rate of hydrogen provided by the controller.
C
C        First, the hydrogen required by the unit to operate in
C        normal operaton mode be determined. This value will be added
C        to the hydrogen requirements for the standby, start-up &
C        cooldown modes.
C-------------------------------------------------------------------------
C-------------------------------------------------------------------------
C        Characterize unit's operation in 'normal operating mode', and
C        estimate 'desired fuel flow'
C
C        Use function Determine_Cogen_OP to determine the fraction
C        of the timestep that the unit will spend in each operating
C        mode.
C-------------------------------------------------------------------------

         call Determine_Cogen_Mode(
     &                 iComponent,
     &                 iOperating_mode_P,
     &                 iSystem_Status,
     &                 fOP_time,
     &                 iTarget_Mode,
     &                 iCM_mandatory,
     &                 iOperating_mode_F,
     &                 fTS_mode_fractions,
     &                 bTS_modes,
     &                 bStartup_complete,
     &                 bDebug              )


         if ( bTS_modes ( iOP_normal_operation ) ) then

C-------------------------------------------------------------------------
C           Has high-level controller specified system power or
C           heat output?
C-------------------------------------------------------------------------
            if ( iControl_Method .eq. iPower_out_controlled .or.
     &           iControl_Method .eq. iHeat_out_controlled       ) then
C-------------------------------------------------------------------------
C              The controller has specified the system's electrical or
C              thermal output. The model must be solved implicitly to
C              determine the fuel flow corresponding to the desired
C              operating point.
C-------------------------------------------------------------------------
               call Solve_Cogen_Model (
     &              iComponent,
     &              iModel_Type,
     &              iControl_Method,
     &              fOP_target,
     &              fDesired_Fuel_Norm,
     &              fWater_Conn_Temp_F,
     &              fWater_Conn_Flow_F,
     &              fTemp_Room,
     &              fStack_Accum_Time_P,
     &              fNorm_OP_DC_Power,
     &              fNorm_OP_Heat     )

            else
C..............Controller has specified fuel flow. There's no need
C..............to explicitly solve model. Do nothing.

            endif

         else

C..........System does not operate in normal operation. Set fuel flow to
C..........Zero,
           fDesired_Fuel_Norm = 0.0

         endif

C-------------------------------------------------------------------------
C        Calculate hydrogen required by all operating modes (kg/s)
C-------------------------------------------------------------------------
         fDesired_Fuel_Flow_TSavg =
     &       fPeriod_Fuel_Flow( iComponent, iOP_inoperative)
     &                 * fTS_mode_fractions(iOP_inoperative)
     &     + fPeriod_Fuel_Flow( iComponent, iOP_startup)
     &                 * fTS_mode_fractions(iOP_startup)
     &     + fDesired_Fuel_Norm
     &                 * fTS_mode_fractions(iOP_normal_operation)
     &     + fPeriod_Fuel_Flow( iComponent, iOP_shutdown)
     &                 * fTS_mode_fractions(iOP_shutdown)


C........Interface to external controller
         fH2_plt_EXP_demand ( iComponent ) = fDesired_Fuel_Flow_TSavg

C-------------------------------------------------------------------------
C        Determine if the supplied hydrogen flow is sufficient to run the
C        system in the desired modes of operation. An iterative
C        approach is used:
C
C          -> Calculate time spent in each mode, as system moves
C             from current operating mode to target mode.
C
C          -> Calculate minimum fuel flow required by system
C             over time step.
C
C          -> Check if system fuel flow is sufficicient:
C
C             -> YES: calculate system's response,
C                     electrical & thermal output.
C
C             -> NO:  Adjust target operating mode, and
C                     iterate again.
C
C-------------------------------------------------------------------------

C........Iteration control.
         bFuel_Flow_Determined = .false.

         do while ( .not. bFuel_Flow_Determined )

C-------------------------------------------------------------------------
C          Calculate time spend in each mode, as system moves from
C          current to target operating points. The outputs of this
C          function may differ from the call above, if insufficient
C          hydrogen is delivered to the unit and a 'fuel fault'
C          occurs.
C-------------------------------------------------------------------------

C...........Assign present time row value to operating time
            fOP_time = fOP_time_P

            call Determine_Cogen_Mode(
     &                 iComponent,
     &                 iOperating_mode_P,
     &                 iSystem_Status,
     &                 fOP_time,
     &                 iTarget_Mode,
     &                 iCM_mandatory,
     &                 iOperating_mode_F,
     &                 fTS_mode_fractions,
     &                 bTS_modes,
     &                 bStartup_complete,
     &                 bDebug              )


C...........Save future time row operating time value
            fOP_time_F = fOP_time   

C----------------------------------------------------------------------------
C           Calculate the minimum fuel requirement for all four
C           modes of operation
C
C           Min fuel flow =
C
C                (fraction of TS in Standby) * (fuel flow in standby)
C
C              + (fraction of TS in Startup) * (fuel flow in startup)
C
C              + (fraction of TS in Operation) * (Minimum fuel flow)
C
C              + (fraction of TS in Cooldown) * (fuel flow in cooldown)
C
C           Note: the 'minimum' fuel flow is used to characterize the
C           the fuel flow in normal operation as the system can operate
C           at an operating point lower than that requested by the
C           controller---it's electrical and thermal output will simply
C           be lower than that expected by the controller. However,
C           the system cannot operate at a fuel flow rate below it's
C           specified minimum.
C
C----------------------------------------------------------------------------
            fMIN_Req_Fuel_Flow_TSavg =
     &          fPeriod_Fuel_Flow( iComponent, iOP_inoperative)
     &                    * fTS_mode_fractions(iOP_inoperative)
     &        + fPeriod_Fuel_Flow( iComponent, iOP_startup)
     &                    * fTS_mode_fractions(iOP_startup)
     &        + fFuel_Flow_MIN ( iComponent )
     &                    * fTS_mode_fractions(iOP_normal_operation)
     &        + fPeriod_Fuel_Flow( iComponent, iOP_shutdown)
     &                    * fTS_mode_fractions(iOP_shutdown)

C-------------------------------------------------------------------------
C           Is supplied fuel flow sufficient?
C-------------------------------------------------------------------------
            call eclose ( fH2_Conn_Flow_F, fMIN_Req_Fuel_Flow_TSavg,
     &                    fCogen_zero_tolerance, bNums_are_close )

            if ( fH2_Conn_Flow_F .ge. fMIN_Req_Fuel_Flow_TSavg .or.
     &           bNums_are_close ) then
C-------------------------------------------------------------------------
C              Fuel flow is sufficient. Determine fuel flow rate to
C              system in normal operation ( remember to scale time-step
C              averaged valye by actual fraction of the time step spent
C              in normal operation )
C
C                 fuel used in normal operation =
C
C                   [  ( connection fuel flow )
C
C                         - (standby fuel flow) * (fraction of TS in standby)
C
C                         - (startup fuel flow) * (fraction of TS in startup)
C
C                         - (cooldown fuel flow) * (fraction of TS in cooldown)
C
C                  ] /  ( fraction of TS in normal operation )
C
C-------------------------------------------------------------------------

               if ( bTS_modes ( iOP_normal_operation ) ) then

                  fNorm_OP_Fuel_Supply =
     &             (  fH2_Conn_Flow_F
     &               - fPeriod_Fuel_Flow( iComponent, iOP_inoperative)
     &                      * fTS_mode_fractions(iOP_inoperative)
     &               - fPeriod_Fuel_Flow( iComponent, iOP_startup)
     &                    * fTS_mode_fractions(iOP_startup)
     &               - fPeriod_Fuel_Flow( iComponent, iOP_shutdown)
     &                    * fTS_mode_fractions(iOP_shutdown) )
     &           / fTS_mode_fractions ( iOP_normal_operation )  ! (kg/s)


C-----------------------------------------------------------------------------
C                 Determine system power and heat output in normal operation
C                 for the given fuel flow. Use subordinate routine
C                 Char_H2_PEMFC_Empirical to evaluate model coefficients.
C-----------------------------------------------------------------------------
                  call Char_H2_PEMFC_Empirical (
     &                     iComponent,
     &                     fNorm_OP_Fuel_Supply,
     &                     fWater_Conn_Temp_F,
     &                     fWater_Conn_Flow_F,
     &                     fTemp_Room,
     &                     fStack_Accum_Time_P,
     &                     fNorm_OP_DC_Power,
     &                     fNorm_OP_Heat,
     &                     fNorm_OP_Stack_Voltage,
     &                     fNorm_OP_Stack_Current,
     &                     fNorm_OP_Skin_Losses,
     &                     fNorm_OP_AC_Power_use  )

              else

C-------------------------------------------------------------------------
C                Unit will not operate in normal operation.
C                Set heat & power flow in normal operation to zero.
C-------------------------------------------------------------------------
                 fNorm_OP_DC_Power                 = 0.0     ! (W)
                 fNorm_OP_Heat                     = 0.0     ! (W)
                 fNorm_OP_Stack_Voltage            =
     &             fNerst_Voltage
     &             * float ( iNum_Cells_In_Stack ( iComponent ) )     ! (V)
                 fNorm_OP_Stack_Current            = 0.0     ! (Amps)
                 fNorm_OP_Skin_Losses              = 0.0     ! (W)
                 fNorm_OP_AC_Power_use             = 0.0     ! (W)

              endif

C----------------------------------------------------------------------------
C             Calcuate time-step averaged heat and power output.
C
C             Time-step-averaged value =
C
C                (fraction of TS in Standby)   * (value in standby)
C
C              + (fraction of TS in Startup)   * (value in startup)
C
C              + (fraction of TS in Operation) * (value in normal op.)
C
C              + (fraction of TS in Cooldown)  * (value in cooldown )
C
C----------------------------------------------------------------------------
C.............Heat output ( +ive is heat transfer to water, O-> )
              fHeat_TS_average =
     &         fNorm_OP_Heat * fTS_mode_fractions(iOP_normal_operation)
     &         + fPeriod_Heat_Recovery( iComponent, iOP_inoperative)
     &                 * fTS_mode_fractions(iOP_inoperative)
     &         + fPeriod_Heat_Recovery( iComponent, iOP_startup)
     &              * fTS_mode_fractions(iOP_startup)
     &         + fPeriod_Heat_Recovery( iComponent, iOP_shutdown)
     &              * fTS_mode_fractions(iOP_shutdown)                 ! (W)


C.............AC power ( +ive is use, ->O )
              fAC_Power_TS_average =
     &        fNorm_OP_AC_Power_use
     &                   * fTS_mode_fractions (iOP_normal_operation )
     &          + fPeriod_AC_Power( iComponent, iOP_inoperative)
     &                   * fTS_mode_fractions(iOP_inoperative)
     &            + fPeriod_AC_Power( iComponent, iOP_startup)
     &                 * fTS_mode_fractions(iOP_startup)
     &            + fPeriod_AC_Power( iComponent, iOP_shutdown)
     &                 * fTS_mode_fractions(iOP_shutdown)              ! (W)

C..............DC power (+ive is generation, O-> )
               fDC_Power_TS_average = fNorm_OP_DC_Power
     &                   * fTS_mode_fractions (iOP_normal_operation )  ! (W)

C----------------------------------------------------------------------------
C              Calcuate time-step averaged current:
C
C              Current = ( time step averaged power ) / ( stack voltage )
C
C              Remember to check that stack voltage is not zero!
C
C----------------------------------------------------------------------------

               call eclose ( fNorm_OP_Stack_Voltage,
     &                       0.0, 0.001, bNums_are_close)

               if ( .not. bNums_are_close .and.
     &               fNorm_OP_Stack_Voltage .gt. 0.0 ) then

                  fCurrent_TS_Average = fDC_Power_TS_average
     &                 / fNorm_OP_Stack_Voltage

               else

                  fCurrent_TS_Average = 0.0

               endif

C..............Exit loop
               bFuel_Flow_Determined = .true.

            else

C-------------------------------------------------------------------------
C              Fuel flow is insufficient & system fault has occured.
C              Was system previously in normal operation?
C-------------------------------------------------------------------------
               
               if ( iSystem_Status .eq. iStatus_normal  ) then

C.................Flag system fuel fault, and attempt to switch system
C.................into cool-down/inoperative modes.
                  
                  iSystem_Status = iStatus_Fuel_Fault

               else

C.................Fuel supply is insufficient to run system in
C.................cooldown/inoperative modes. System will become
C.................'inert' (no electrical / thermal output )

                  fHeat_TS_average       = 0.0   ! (W)
                  fAC_Power_TS_average   = 0.0   ! (W)
                  fDC_Power_TS_average   = 0.0   ! (W)

                  fNorm_OP_Stack_Voltage = 0.0   ! (V)
                  fCurrent_TS_Average    = 0.0   ! (A)

                  iOperating_mode_F = iOP_inoperative
                  
                  fOP_time_F = fTS_duration

C.................Exit loop
                  bFuel_Flow_Determined = .true.

               endif

            endif
         enddo
C-------------------------------------------------------------------------
C        PEMFC energy balance state equations.
C
C        -> The model does not presently use a stack energy balance model,
C           set stack temperature to inlet temperature
C
C        -> Temperature of second (cooling water) control volume is determined
C           by rate of heat recovery.
C
C        State equations are:
C
C        |<-- self -->|<-- cross -->|<----- Solution  ----->|   |<-- Known ---->|
C        |  coupling  |  coupling   |        vector         |   |    data       |

C        |  A      B     E    ---  | | temperature, 1st node| = |     0.0       |
C        |  C      D    ---    F   | | temperature, 2nd node|   |      G        |
C                                    | temperature, conn. 1 |
C                                    | temperature, conn. 2 |
C
C        where:
C
C          A =  1.0
C          B =  0.0
C          C =  0.0
C          D =  ( cooling water heat capacity )
C          E = -1.0
C          F = -( cooling water heat capacity )
C          G =  ( rate of heat recovery )
C
C-------------------------------------------------------------------------

C........Self coupling coefficients
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0

         if ( bNo_cooling_water ) then 
            fCoefficients(4) =  1.0
         else
            fCoefficients(4) =  fWater_conn_Cp_F 
         endif 
C........cross coupling coefficients
         fCoefficients(5) = -1.0
         if ( bNo_cooling_water ) then
            fCoefficients(6) = -1.0 
         else 
            fCoefficients(6) = -1.0 * fWater_conn_Cp_F
         endif 
C........RHS vector
         fCoefficients(7) =  0.0
         if ( bNo_cooling_water ) then
            fCoefficients(8) =  0.0
         else 
            fCoefficients(8) =  fHeat_TS_average
         endif 

C-------------------------------------------------------------------------
C        Save DC power output (W, generation is +ive...), and
C        AC power consumption (W, load is -ive...)
C        use function EMACH to transport data to electrical domain. 
C-------------------------------------------------------------------------
C         pwrp(iComponent) = fDC_Power_TS_average
C         IEMODEL=1              ! Flag indicating which power calculation
C                                !    will be performed

C         CALL EMACH(
C     &        iComponent,       ! Component number (input)
C     &        IEMODEL,          ! power calculation flag (input)
C     &        pwrp(iComponent), ! Real power (input, W)
C     &        PQ_dummy,         ! Complex power (output, W)
C     &        PA_dummy)         ! Overall power (output, W)

          IEMODEL = 2

          CALL EMACH(
     &         iComponent,
     &         IEMODEL,
     &         fDC_Power_TS_average,
     &         fAC_Power_TS_average,
     &         PA_dummy )
          


C-------------------------------------------------------------------------
C        Save stack operational voltage in esp-r common block
C-------------------------------------------------------------------------     
         bVoltP (iComponent) = fNorm_OP_Stack_Voltage         ! (V)
C-------------------------------------------------------------------------
C        Calculate stack accumulated operating time
C-------------------------------------------------------------------------
         fStack_Accum_Time_F = fStack_Accum_Time_P
     &      + fTS_duration * fTS_mode_fractions (iOP_normal_operation) ! (s)

C-------------------------------------------------------------------------
C        Save future time row 'additional data' for use on next time row
C
C        -> Note: the unit's hydrogen demand is saved in the first
C           additional data array location, and this data is marked
C           for iteration.  Marking this data for iteration ensures
C           that the unit's demand and the hydrogen supplied by
C           upstream components (ie. a hydrogen storage tank) are
C           'in sync' on this time step.
C
C           This is a necessity, as the PEMFC's operating point is
C           determined by the rate of hydrogen suppled to the unit,
C           and not by the control signals sent to the unit. Thus,
C           changing the unit's operating point requires two
C           iterations. On the first iteration, the PEMFC notes
C           a change in control signals and calculates the hydrogen
C           flow rate required by the new operating point, but
C           the system's operation is determined by the hydrogen
C           mass flow computed on the previous solution of the
C           mass balance matrix. On the following iteration, the
C           change in hydrogen demand will be noted by upstream
C           components, and the actual amount of hydrogen delivered
C           to the unit will be changed.
C
C           However, since the PEMFC state variables are not
C           effected by the change in control signals on the first
C           iteration, the plant matrix solver will not realize
C           that a significant adjustement to the unit's operating
C           point has occured. It may prematurely ajourn iteration
C           before the plant network's response to the control
C           signal has been characterized.
C
C           Therefore, the actual hydrogen demanded by the unit
C           is flagged for iteration. Two successive iterations
C           must pass with the same control signal before iteration
C           can be adjourned.
C
C-------------------------------------------------------------------------

C........Mark the first plant 'additional data' for iteration
         iPlt_Output_Iter_Flag (iComponent,1) = 1

C........Save data from last iteration for comparison
         fPlt_Output_Init_Val ( iComponent, 1) =
     &     pcdatf( iComponent , 1 )

C........Save hydrogen demand as a plant 'additional output'
         PCDatF( iComponent , 1 ) = fDesired_Fuel_Flow_TSavg

C-------------------------------------------------------------------------
C        Remainder of future time-row data
C-------------------------------------------------------------------------

C........operating mode
         PCDatF ( iComponent, 2 ) = float(iOperating_mode_F)

C........Time spent in this operating mode
         PCDatF ( iComponent, 3 ) = fOP_time_F

C........What about PCDATF (x,4)?

C........Accumulated stack operating time
         PCDatF ( iComponent, 5 ) = fStack_Accum_Time_F

C-------------------------------------------------------------------------
C        Save data in reporting commons for use by post-processor
C        procdure.
C-------------------------------------------------------------------------

C........Time step averaged data: heat recovery
         fReport_heat_TS_avg ( iComponent ) = fHeat_TS_average         ! (W)

C........Time step averaged data: DC power output
         fReport_DC_Power_TS_avg ( iComponent ) = fDC_Power_TS_average ! (W)

C........Time step averaged data: AC power usage
         fReport_AC_Power_TS_avg ( iComponent ) = fAC_Power_TS_average ! (W)

C........Time step averaged data: Skin losses
          fReport_Skin_Losses_TS_avg ( iComponent ) = 0.0              ! (W)

C........Time step averaged data: Stack Current
         fReport_Stack_Current_TS_avg (iComponent ) =
     &                  fCurrent_TS_Average ! (W)

C........Stack voltage:
         fReport_Stack_Voltage( iComponent ) =
     &                  fNorm_OP_Stack_Voltage     ! (V)

C........Hydrogen flow rate
         fReport_Hydrogen_flow ( iComponent ) = fH2_Conn_Flow_F       ! (kg/s)

C........Heating value of hydrogen flow
         fReport_Hydrogen_LHV ( iComponent ) =
     &                 fH2_Conn_Flow_F * fH2_heating_value            ! (W)
         
C........Hydrogen flow rate requested by unit to meet demands
         fReport_Hydrogen_demand ( iComponent ) =
     &                 fDesired_Fuel_Flow_TSavg ! (kg/s)

C........Heating value of hydrogen requested by unit to meet demands
         fReport_Hydrogen_demand_LHV ( iComponent ) =
     &              fDesired_Fuel_Flow_TSavg * fH2_heating_value      ! (W)


         if ( .not. bNo_H2_flow ) then
C........Electrical efficiency (timestep averaged)
         fReport_Stack_Eff_Elec_TS_avg ( iComponent ) =
     &         ( fAC_Power_TS_average + fDC_Power_TS_average )
     &       / ( fH2_Conn_Flow_F * fH2_heating_value )                ! (-)

C........Thermal efficiency (timestep averaged)
         fReport_Stack_Eff_Ther_TS_avg ( iComponent ) =
     &         ( fHeat_TS_average )
     &       / ( fH2_Conn_Flow_F * fH2_heating_value )                ! (-)

C........Cogeneration efficiency (timestep averaged)
         fReport_Stack_Eff_Cogen_TS_avg ( iComponent ) =
     &      ( fAC_Power_TS_average + fDC_Power_TS_average
     &           + fHeat_TS_average )
     &      / ( fH2_Conn_Flow_F * fH2_heating_value )                ! (-)
         else
            fReport_Stack_Eff_Cogen_TS_avg ( iComponent ) = 0.0
            fReport_Stack_Eff_Ther_TS_avg  ( iComponent ) = 0.0
            fReport_Stack_Eff_Elec_TS_avg  ( iComponent ) = 0.0
         endif
C........Control scheme
         iReport_Control_method ( iComponent ) = iControl_Method     ! (-)

C........Control signal
         fReport_Control_signal ( iComponent ) = fControl_signal     ! (?)

C........Time spent in each operating mode

         do ii = 1, iOP_Mode_Count
            fReport_TS_modes ( iComponent, ii ) =
     &          fTS_duration * fTS_mode_fractions ( ii )             ! (s)
         enddo

C-------------------------------------------------------------------------
C        Additional outputs dumped when 'debugging'
C-------------------------------------------------------------------------
         if ( bDebug ) then

             call add_to_xml_reporting(
     &                fH2_heating_value,
     &                'debug/H2_PEMFC/H2/LHV',
     &                'units', '(J/kg)',
     &                'Heating value of hydrogen')


         endif

      endif

C----------------------------------------------------------------------
C     That's it!
C----------------------------------------------------------------------
      if (bDebug) write(ITU,*)
     &   "Exiting subroutine SH2_PEMFC_Q_balance()"

      return
      end




C---------------- Char_H2_PEMFC_Empirical() -------------------------------
C
C This procedure is used to characterize the performance of a hydrogen
C fueled PEMFC system in response to conditions in the mechanical plant,
C using impirical correlations
C
C
C Inputs:
C
C   - iComponent: Index of component in plant network
C   - iModel_Type: type of model to be called.
C   - fFuel_Flow: system fuel flow (kg/s or kmol/s)
C   - fCW_temperature_in (oC)
C   - fCW_mass_flow (kg/s)
C   - fTemp_Room    (oC)
C
C Outputs:
C
C  - fPower_output: system electrical output  (W)
C  - fHeat_Output: system thermal output (W)
C  - Voltage
C  - Current
C
C----------------------------------------------------------------------
      subroutine Char_H2_PEMFC_Empirical (
     &              iComponent,
     &              fFuel_flow,
     &              fCW_temperature_in,
     &              fCW_mass_flow,
     &              fTemp_Room,
     &              fAccum_time,
     &              fPower_Output,
     &              fHeat_Output,
     &              fStack_Voltage,
     &              fStack_Current,
     &              fSkin_Losses,
     &              fAux_Power_use )

      implicit none

#include "building.h"
#include "plant.h"
#include "chemical_properties.h"
#include "cetc_cogen.h"
#include "Hydrogen_PEMFC.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent            ! index of component in plant
                                    !   network
      integer iModel_Type           ! flag indicating which model is
                                    !   to be used.

      real fFuel_flow               ! Calcuated fuel flow (kg/s)
      real fOP_target               ! Power/heat output requested by controller (W)
      real fCW_mass_flow            ! Cooling water flow (kg/s)
      real fCW_temperature_in       ! Cooling water temp at inlet (oC)
      real fAccum_time              ! Unit's accumulated operating time (s)
      real fTemp_Room               ! Ambient temperature (oC)

      real fHeat_Output             ! Calcuated heat output (W)
      real fPower_Output            ! Calculate power output (W)


      real fCell_Voltage            ! Voltage of individual cells
      real fStack_Voltage           ! Voltage of entire stack

      real fStack_Current           ! Unit's current (amps)

      real fSkin_Losses             ! heat transfer from the unit to surroundings (W)

      real fAux_Power_use           ! power used by auxiliary systems



C---------------------------------------------------------------------
C     Copies of correlation parameters.
C---------------------------------------------------------------------
      real fCOPY_Fuel_LHV_Flow
      real fCOPY_CW_mass_flow
      real fCOPY_CW_temperature_in
      real fCOPY_Temp_Room
C---------------------------------------------------------------------
C     Misc local variables
C---------------------------------------------------------------------
      real fFuel_LHV_Flow           ! fuel LHV flow rate (W)
      real fEff_electric            ! electric efficiency (-)
      real fEff_thermal             ! thermal efficiency (-)

C----------------------------------------------------------------------
C     Correlation reference parameters
C----------------------------------------------------------------------
      real fCW_ref_temp             ! Cooling water reference temperature (oC)
      real fCW_ref_flow             ! Cooling water reference flow rate (kg/s)
      real fAmb_ref_temp            ! Ambient reference temperature (oC)

C----------------------------------------------------------------------
C     Misc local variables
C----------------------------------------------------------------------
      logical bNums_are_close       ! result of close to zero comparisons

C----------------------------------------------------------------------
C     Interfaces to chemical_properties.F
C----------------------------------------------------------------------
      real fEval_Compound_HV   !  returns heating value of a compound (J/kmol)
      real fEval_Compound_MM   !  returns the molar mass of a compound (kg/kmol)
C----------------------------------------------------------------------
C     Assign reference conditions to local variables.
C----------------------------------------------------------------------

      fCW_ref_temp  = fReference_CW_Temp  ( iComponent ) ! (oC)
      fCW_ref_flow  = fReference_CW_Flow  ( iComponent ) ! (kg/s)
      fAmb_ref_temp = fReference_Amb_Temp ( iComponent ) ! (oC)


C----------------------------------------------------------------------
C     Calculate fuel heating value using function fEval_Compound_HV.
C     Remember, fEval_Compound_HV returns the heating value in J/kmol,
C     and this value must be divided by the molar mass of hydrogen to
C     obtain the heating value in J/kg
C----------------------------------------------------------------------
      fFuel_LHV_Flow = fFuel_flow
     &      * fEval_Compound_HV ( iHydrogen, iLHV )
     &      / fEval_Compound_MM ( iHydrogen )            ! (W)

C----------------------------------------------------------------------
C     Check that correlation parameters are within bounds
C----------------------------------------------------------------------

C.....Fuel heating value flow rate.
      if ( fFuel_LHV_Flow
     &    .lt.  fCorr_Range_LHV_MIN( iComponent ) ) then

         fCOPY_Fuel_LHV_Flow =
     &          fCorr_Range_LHV_MIN( iComponent )

      elseif ( fFuel_LHV_Flow
     &    .gt.  fCorr_Range_LHV_MAX( iComponent ) ) then

         fCOPY_Fuel_LHV_Flow =
     &          fCorr_Range_LHV_MAX( iComponent )

      else
      
        fCOPY_Fuel_LHV_Flow = fFuel_LHV_Flow
    
      endif


C.....Cooling water inlet temp.
      if ( fCW_temperature_in
     &    .lt.  fCorr_Range_CW_temp_MIN( iComponent ) ) then

         fCOPY_CW_temperature_in =
     &          fCorr_Range_CW_temp_MIN( iComponent )

      elseif ( fCW_temperature_in
     &    .gt.  fCorr_Range_CW_temp_MAX( iComponent ) ) then

         fCOPY_CW_temperature_in =
     &          fCorr_Range_CW_temp_MAX( iComponent )

      else

         fCOPY_CW_temperature_in = fCW_temperature_in

      endif

C.....Cooling water mass flow rate
      if ( fCW_mass_flow
     &    .lt.  fCorr_Range_CW_flow_MIN( iComponent ) ) then

         fCOPY_CW_mass_flow =
     &          fCorr_Range_CW_flow_MIN( iComponent )

      elseif ( fCW_mass_flow
     &    .gt.  fCorr_Range_CW_flow_MAX( iComponent ) ) then

         fCOPY_CW_mass_flow =
     &          fCorr_Range_CW_flow_MAX( iComponent )

      else

         fCOPY_CW_mass_flow = fCW_mass_flow

      endif

C.....Ambient temperature. Note: the ambient temperature
C.....varible is overloaded---a value of -99 indicates that
C.....no containment temperature is specified.

      call eclose ( fTemp_Room, -99.0, 0.01, bNums_are_close )

      if ( bNums_are_close ) then
C........No containment has been specified. Set room temperature
C........to reference temperature.
         fTemp_Room = fAmb_ref_temp

      elseif ( fTemp_room .lt.
     &     fCorr_Range_AMB_temp_MIN ( iComponent ) ) then

         fCOPY_Temp_Room =
     &       fCorr_Range_AMB_temp_MIN ( iComponent )

      elseif ( fTemp_room .gt.
     &     fCorr_Range_AMB_temp_MAX ( iComponent ) ) then

         fCOPY_Temp_Room =
     &       fCorr_Range_AMB_temp_MAX(MPCom)

      else

         fCOPY_Temp_Room = fTemp_Room

      endif

C----------------------------------------------------------------------
C     Evaluate electrical efficiency. Correlation form is:
C
C     efficiency = c1 + c2 * ( fuel LHV )  + c3 * ( fuel LHV ) ^ 2.0
C                     + c4 * ( CW temp - CW ref temp )
C                     + c5 * ( CW temp - CW ref temp ) ^ 2.0
C                     + c6 * ( CW flow - CW ref flow )
C                     + c7 * ( CW flow - CW ref flow ) ^ 2.0
C                     + c8 * ( accumulated operating time )
C
C
C----------------------------------------------------------------------
      fEff_electric =
     &   fElec_Eff_Coeff ( iComponent, 1 )
     & + fElec_Eff_Coeff ( iComponent, 2 ) * fCOPY_Fuel_LHV_Flow
     & + fElec_Eff_Coeff ( iComponent, 3 ) * fCOPY_Fuel_LHV_Flow ** 2.0
     & + fElec_Eff_Coeff ( iComponent, 4 )
     &             * ( fCOPY_CW_temperature_in - fCW_ref_temp )
     & + fElec_Eff_Coeff ( iComponent, 5 )
     &             * ( fCOPY_CW_temperature_in - fCW_ref_temp ) ** 2.0
     & + fElec_Eff_Coeff ( iComponent, 6 )
     &             * ( fCOPY_CW_mass_flow - fCW_ref_flow )
     & + fElec_Eff_Coeff ( iComponent, 7 )
     &             * ( fCOPY_CW_mass_flow - fCW_ref_flow ) ** 2.0
     & + fElec_Eff_Coeff ( iComponent, 8 ) * fAccum_time

C----------------------------------------------------------------------
C     Evaluate thermal efficiency. Correlation form is:
C
C     efficiency = c1 + c2 * ( fuel LHV )  + c3 * ( fuel LHV ) ^ 2.0
C                     + c4 * ( CW temp - CW ref temp )
C                     + c5 * ( CW temp - CW ref temp ) ^ 2.0
C                     + c6 * ( CW flow - CW ref flow )
C                     + c7 * ( CW flow - CW ref flow ) ^ 2.0
C                     + c8 * ( accumulated operating time )
C
C
C----------------------------------------------------------------------
      fEff_thermal =
     &   fTher_Eff_Coeff ( iComponent, 1 )
     & + fTher_Eff_Coeff ( iComponent, 2 ) * fCOPY_Fuel_LHV_Flow
     & + fTher_Eff_Coeff ( iComponent, 3 ) * fCOPY_Fuel_LHV_Flow ** 2.0
     & + fTher_Eff_Coeff ( iComponent, 4 )
     &             * ( fCOPY_CW_temperature_in - fCW_ref_temp )
     & + fTher_Eff_Coeff ( iComponent, 5 )
     &             * ( fCOPY_CW_temperature_in - fCW_ref_temp ) ** 2.0
     & + fTher_Eff_Coeff ( iComponent, 6 )
     &             * ( fCOPY_CW_mass_flow - fCW_ref_flow )
     & + fTher_Eff_Coeff ( iComponent, 7 )
     &             * ( fCOPY_CW_mass_flow - fCW_ref_flow ) ** 2.0
     & + fTher_Eff_Coeff ( iComponent, 8 ) * fAccum_time

C----------------------------------------------------------------------
C     Evaluate electrical and thermal output. Note: electical output
C     is DC, and thus will be dumped onto the house's dc bus.
C
C        Electrical output = ( elec. efficiency ) * ( fuel heating value )
C
C        thermal output    = ( ther. efficiency ) * ( fuel heating value )
C
C----------------------------------------------------------------------
      fPower_Output = fEff_electric * fFuel_LHV_Flow     ! (W)
      fHeat_Output  = fEff_thermal  * fFuel_LHV_Flow     ! (W)

C----------------------------------------------------------------------
C     Evalueate system skin losses. Correlation form is:
C
C     skin losses = = c1 + c2 * ( fuel LHV )  + c3 * ( fuel LHV ) ^ 2.0
C                        + c4 * ( Amb. temp - Amb. ref temp )
C                        + c5 * ( Amb. temp - Amb. ref temp ) ^ 2.0
C
C----------------------------------------------------------------------
      fSkin_Losses =
     &   fSkinLoss_Coeff ( iComponent, 1 )
     & + fSkinLoss_Coeff ( iComponent, 2 ) * fCOPY_Fuel_LHV_Flow
     & + fSkinLoss_Coeff ( iComponent, 3 ) * fCOPY_Fuel_LHV_Flow ** 2.0
     & + fTher_Eff_Coeff ( iComponent, 4 )
     &             * ( fCOPY_Temp_Room - fAmb_ref_temp )
     & + fTher_Eff_Coeff ( iComponent, 5 )
     &             * ( fCOPY_Temp_Room - fAmb_ref_temp ) ** 2.0

C----------------------------------------------------------------------
C     Evaluate system auxiliary energy use. Note: aux equipment use
C     AC power, which must be pulled off the house's AC bus.
C
C     Correlation form is:
C
C     Aux. power use = c1 + c2 * ( fuel LHV )  + c3 * ( fuel LHV ) ^ 2.0
C                         + c4 * ( CW temp - CW ref temp )
C                         + c5 * ( CW temp - CW ref temp ) ^ 2.0
C                         + c6 * ( CW flow - CW ref flow )
C                         + c7 * ( CW flow - CW ref flow ) ^ 2.0
C
C----------------------------------------------------------------------
      fAux_Power_use =
     &   fAux_Power_Coeff ( iComponent, 1 )
     & + fAux_Power_Coeff ( iComponent, 2 ) * fCOPY_Fuel_LHV_Flow
     & + fAux_Power_Coeff ( iComponent, 3 ) * fCOPY_Fuel_LHV_Flow ** 2.0
     & + fAux_Power_Coeff ( iComponent, 4 )
     &             * ( fCOPY_CW_temperature_in - fCW_ref_temp )
     & + fAux_Power_Coeff ( iComponent, 5 )
     &             * ( fCOPY_CW_temperature_in - fCW_ref_temp ) ** 2.0
     & + fAux_Power_Coeff ( iComponent, 6 )
     &             * ( fCOPY_CW_mass_flow - fCW_ref_flow )
     & + fAux_Power_Coeff ( iComponent, 7 )
     &             * ( fCOPY_CW_mass_flow - fCW_ref_flow ) ** 2.0

C----------------------------------------------------------------------
C     Calculate system voltage and current:
C
C        cell voltage  = ( nerst voltage ) * ( electric efficiency )
C
C        stack voltage = ( cell voltage ) * ( # cells in stack )

C        stack current  = ( power )  /  ( stack voltage )
C
C     ( this analysis assumes that the fuel cell unit can be modelled
C       as a single lumped electrical conversion device, or a group
C       of electircal conversion + power conditioning devices that
C       are *in series*. If the unit contains fuel processing
C       equipment, such as a humidifier, this analysis will tend
C       to underpredict the voltage and overpredict the current.
C       A more detailed model is required to model such systems. )
C
C     ( Additionally, all cells are assumed to comprise a single stack )
C
C----------------------------------------------------------------------

      fCell_Voltage = fNerst_Voltage * fEff_electric
      fStack_Voltage = fCell_Voltage
     &         * float ( iNum_Cells_In_Stack ( iComponent ) )
      fStack_Current = fPower_output / fStack_Voltage


      return
      end




C------------------- H2_PEMFC_Collect_Data ----------------------------
C
C This subrouine collects time-invariant plant component parameters
C stored in ESP-r's adata & bdata arrays, and stuffs them in the
C common block COMMON/H2_PEMFC_Params/. Rudimentary error checking
C is also performed
C
C INPUTS:
C
C  - iComp: index of component in plant network
C  - iModel_Type: flag indicating which model (empirical
C    or semi-mechanistic) has been specified.
C
C OUTPUTS:
C   - NONE
C
C
C
C----------------------------------------------------------------------
      subroutine H2_PEMFC_Collect_Data ( iComp, iModel_Type, bDebug )
      implicit none

#include "building.h"
#include "plant.h"

#include "cetc_cogen.h"
#include "Hydrogen_PEMFC.h"
#include "chemical_properties.h"

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Passed arguements
      integer iComp            ! index of component in plant network
      integer iModel_Type      ! type of model in use
      logical bDebug           ! flag for debugging output

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu

      logical bClose_a  ! result of close-to-zero comparisons
      logical bClose_b  ! result of close-to-zero comparisons
      logical bFatal_error       ! flag indicating error has occurred.

C.....Local variables
      integer iShift           ! counter

C.....References
      real fEval_Mixture_HV    ! Function to evaluate a mixture's
                               !   heating value
      real fEval_Mixture_MM    ! Function to evaluate a mixture's
                               !   molar mass
      logical bFlammable_Mixture ! Function determining if a mixture
                               !   can be oxidized.

      bFatal_error = .false.

C----------------------------------------------------------------------
C     Collect and validate inputs
C----------------------------------------------------------------------
C.....System nominal size - Must be > 0.
      fNominal_Power_Output ( iComp )  = ADATA ( iComp, 1 )       ! (W)

      call eclose ( fNominal_Power_Output( iComp ), 0.0, 1.0, bClose_a)

      if ( bClose_a .or. fNominal_Power_Output ( iComp ) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                System nominal output must'
     &                   // ' be greater than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 1 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.

      end if

C.....Number of cells in stack, must be > 0.
       iNum_Cells_In_Stack ( iComp )  = int( ADATA ( iComp, 2 ) )   ! (-)

      if ( iNum_Cells_In_Stack ( iComp ) .le. 0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Stack cell count'
     &                   // ' be greater than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 2 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.

      end if

C.....Cumulative operating time at start of simulation, cannot
C.....be less than zero .
      fInitial_accum_time ( iComp ) = ADATA (iComp, 3 )      ! (s)

      call eclose ( fInitial_accum_time (iComp), 0.0, 0.001, bClose_a )

      if ( .not. bClose_a .and.
     &      fInitial_accum_time (icomp) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Stack initial accumulated'
     &                   // 'operating time cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 3 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Min fuel flow, cannot be less than zero.
      fFuel_Flow_MIN ( iComp ) = ADATA ( iComp,  4)      ! (kg/s)
      call eclose ( fFuel_Flow_MIN( iComp ) , 0.0, 1.0E-07 , bClose_a )
      if ( .not. bClose_a .and. fFuel_Flow_MIN ( iComp ) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                System nominal output cannot'
     &                   // ' be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 4 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Max fuel flow, cannot be less than min fuel flow rate
      fFuel_Flow_MAX( iComp ) = ADATA ( iComp, 5 )      ! (kg/s)

      if ( fFuel_Flow_MAX ( iComp ) .lt. fFuel_Flow_MIN ( iComp ) ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Warning.'
         write (itu,*) '                System minimum H2 flow rate'
     &                   // ' is larger than maximum.'
         write (itu,*) '                Switching over minimum & '
     &                   // 'maximum.'
         write (itu,'(A,I2)')
     &                 '                 Check inputs # 4 & 5 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '

         fFuel_Flow_MIN ( iComp ) = ADATA ( iComp, 5 )      ! (kg/s)
         fFuel_Flow_MAX ( iComp ) = ADATA ( iComp, 4 )      ! (kg/s)

      endif

C.....Cooling water maximum temperature. No restricitions
      fCW_Temp_MAX( iComp ) = ADATA ( iComp,  6)      ! (oC)

C.....Maximum rate of change in power output. No restrictions
      fGross_Power_Change_MAX( iComp ) = ADATA ( iComp, 7 )      ! (W/s)

C.....Maximum rate of change in heat output. No restrictions
      fHeat_Recovery_Change_MAX( iComp ) = ADATA ( iComp, 8 )      ! (W/s)

C.....Start-up period duration. Cannot be less than zero
      fPeriod_duration( iComp, iOP_startup ) = ADATA ( iComp,  9 ) ! (s)
      call eclose (fPeriod_duration (iComp, iOP_startup),
     &             0.0,0.1,bClose_a )
      if ( .not. bClose_a .and.
     &     fPeriod_duration ( iComp, iOP_startup) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Startup period duration'
     &                   // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 9 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Start-up period fuel flow.
      fPeriod_Fuel_Flow( iComp, iOP_startup ) = ADATA ( iComp,  10)  ! (kg/s)
      call eclose (fPeriod_Fuel_Flow(iComp, iOP_startup),
     &             0.0, 1.0E-07, bClose_a )
      if ( .not. bClose_a .and.
     &     fPeriod_Fuel_Flow(iComp,iOP_startup) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Start-up period fuel flow'
     &                   // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 10 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Start-up period electic output.
      fPeriod_AC_Power( iComp, iOP_startup ) = ADATA ( iComp, 11 )  ! (W)


C.....Start-up period thermal output, no restrictions
      fPeriod_Heat_Recovery( iComp,iOP_startup ) = ADATA ( iComp, 12 ) ! (W)


C.....Cooldown period duration. Cannot be less than zero
      fPeriod_duration( iComp, iOP_shutdown ) = ADATA ( iComp,  13 )  ! (s)
      call eclose (fPeriod_duration (iComp, iOP_shutdown),
     &             0.0,0.1,bClose_a )
      if ( .not. bClose_a .and.
     &     fPeriod_duration(iComp, iOP_shutdown) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Cooldown period duration'
     &                   // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 13 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Cooldown period fuel flow. (Cannot be less than zero)
      fPeriod_Fuel_Flow( iComp,iOP_shutdown ) = ADATA ( iComp,  14)  ! (kg/s)
      call eclose (fPeriod_Fuel_Flow(iComp,iOP_shutdown),
     &             0.0, 1.0E-07, bClose_a )
      if ( .not. bClose_a .and.
     &     fPeriod_Fuel_Flow(iComp,iOP_shutdown) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Cooldown period fuel flow'
     &                   // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 14 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Cooldown period ac electic output.
      fPeriod_AC_Power( iComp,iOP_shutdown ) = ADATA ( iComp, 15 )  ! (W)


C.....Cooldown period heat recovery, no restrictions
      fPeriod_Heat_Recovery( iComp,iOP_shutdown ) = ADATA (iComp, 16 ) ! (W)

C.....Standby period fuel flow.
      fPeriod_Fuel_Flow( iComp, iOP_inoperative ) =
     &                 ADATA ( iComp,  17)      ! (kg/s)

      call eclose (fPeriod_Fuel_Flow(iComp,iOP_inoperative),
     &             0.0, 1.0E-07, bClose_a )

      if ( .not. bClose_a .and.
     &      fPeriod_Fuel_Flow (iComp,iOP_inoperative) .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'H2 PEMFC model: Fatal error!'
         write (itu,*) '                Standby mode fuel flow'
     &                   // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                 Check input # 17 in'
     &                 // ' plant component # ', iComp
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Standby period ac electrical power
      fPeriod_AC_Power( iComp, iOP_inoperative ) =
     &            ADATA ( iComp, 18 )      ! (W)


C.....Standby period heat recovery, no restrictions
      fPeriod_Heat_Recovery( iComp, iOP_inoperative ) =
     &            ADATA ( iComp, 19 )    ! (W)


C.....Refernece cooling water temperature, no restrictions.
      fReference_CW_Temp ( iComp ) = ADATA ( iComp, 20 )       ! (oC)

C.....Reference cooling water flow rate, no restrictions
      fReference_CW_Flow ( iComp ) = ADATA ( iComp, 21 )       ! (kg/s)

C.....Reference ambient temperature, no restricitions
      fReference_Amb_Temp ( iComp ) = ADATA ( iComp, 22 )      ! (kg/s)

C.....Correlation cofficients - Skin heat loss
      fSkinLoss_Coeff ( iComp, 1 ) = ADATA ( iComp, 23 )       ! ( W )
      fSkinLoss_Coeff ( iComp, 2 ) = ADATA ( iComp, 24 )       ! ( W / W )
      fSkinLoss_Coeff ( iComp, 3 ) = ADATA ( iComp, 25 )       ! ( W / W^2 )
      fSkinLoss_Coeff ( iComp, 4 ) = ADATA ( iComp, 26 )       ! ( W / oC )
      fSkinLoss_Coeff ( iComp, 5 ) = ADATA ( iComp, 27 )       ! ( W / oC )

C.....Correlation coefficients - Thermal Efficiency
      fTher_Eff_Coeff ( iComp, 1 ) = ADATA ( iComp, 28 )       ! ( - )
      fTher_Eff_Coeff ( iComp, 2 ) = ADATA ( iComp, 29 )       ! ( 1/W )
      fTher_Eff_Coeff ( iComp, 3 ) = ADATA ( iComp, 30 )       ! ( 1/W^2 )
      fTher_Eff_Coeff ( iComp, 4 ) = ADATA ( iComp, 31 )       ! ( 1/oC )
      fTher_Eff_Coeff ( iComp, 5 ) = ADATA ( iComp, 32 )       ! ( 1/oC^2 )
      fTher_Eff_Coeff ( iComp, 6 ) = ADATA ( iComp, 33 )       ! ( s/kg )
      fTher_Eff_Coeff ( iComp, 7 ) = ADATA ( iComp, 34 )       ! ( s^2/kg^2 )
      fTher_Eff_Coeff ( iComp, 8 ) = ADATA ( iComp, 35 )       ! ( 1/s )

C.....Correlation coefficients - Electrical Efficiency
      fElec_Eff_Coeff ( iComp, 1 ) = ADATA ( iComp, 36 )       ! ( - )
      fElec_Eff_Coeff ( iComp, 2 ) = ADATA ( iComp, 37 )       ! ( 1/W )
      fElec_Eff_Coeff ( iComp, 3 ) = ADATA ( iComp, 38 )       ! ( 1/W^2 )
      fElec_Eff_Coeff ( iComp, 4 ) = ADATA ( iComp, 39 )       ! ( 1/oC )
      fElec_Eff_Coeff ( iComp, 5 ) = ADATA ( iComp, 40 )       ! ( 1/oC^2 )
      fElec_Eff_Coeff ( iComp, 6 ) = ADATA ( iComp, 41 )       ! ( s/kg )
      fElec_Eff_Coeff ( iComp, 7 ) = ADATA ( iComp, 42 )       ! ( s^2/kg^2 )
      fElec_Eff_Coeff ( iComp, 8 ) = ADATA ( iComp, 43 )       ! ( 1/s )

C.....Correlation coefficients - Auxiliary power usage
      fAux_Power_Coeff ( iComp, 1 ) = ADATA ( iComp, 44 )      ! ( W )
      fAux_Power_Coeff ( iComp, 2 ) = ADATA ( iComp, 45 )      ! ( 1/W )
      fAux_Power_Coeff ( iComp, 3 ) = ADATA ( iComp, 46 )      ! ( 1/W^2 )
      fAux_Power_Coeff ( iComp, 4 ) = ADATA ( iComp, 47 )      ! ( 1/oC )
      fAux_Power_Coeff ( iComp, 5 ) = ADATA ( iComp, 48 )      ! ( 1/oC^2 )
      fAux_Power_Coeff ( iComp, 6 ) = ADATA ( iComp, 49 )      ! ( s/kg )
      fAux_Power_Coeff ( iComp, 7 ) = ADATA ( iComp, 50 )      ! ( s^2/kg^2 )

C.....Correlation ranges of validity
      fCorr_Range_LHV_MIN ( iComp ) = ADATA ( iComp, 51 )      ! ( W )
      fCorr_Range_LHV_MAX ( iComp ) = ADATA ( iComp, 52 )      ! ( W )
      fCorr_Range_CW_temp_MIN ( iComp ) = ADATA ( iComp, 53 )  ! ( oC )
      fCorr_Range_CW_temp_MAX ( iComp ) = ADATA ( iComp, 54 )  ! ( oC )
      fCorr_Range_CW_flow_MIN ( iComp ) = ADATA ( iComp, 55 )  ! ( kg/s )
      fCorr_Range_CW_flow_MAX ( iComp ) = ADATA ( iComp, 56 )  ! ( kg/s )
      fCorr_Range_AMB_temp_MIN ( iComp ) = ADATA ( iComp, 57 ) ! ( oC )
      fCorr_Range_AMB_temp_MAX ( iComp ) = ADATA ( iComp, 58 ) ! ( oC )

      if ( bDebug ) then

C........Dump model configuration to buffer

         write (itu,*)
     &     '   Hydrogen_PEMFC.F: Model configuration'

         write (itu,'(A55,I2)')
     &     '                    Component: ', iComp

         write (itu,'(A55,F10.3,A)')
     &     '                    Nomimal power: ',
     &       fNominal_Power_Output ( iComp ), ' (W) '

         write (itu,'(A55,I3,A)')
     &     '                    Number of cells: ',
     &       iNum_Cells_In_Stack ( iComp ), '  '

         write (itu,'(A55,F10.3,A)')
     &     '                    Stack accumulated time: ',
     &      fInitial_accum_time ( iComp ), ' (s) '

         write (itu,'(A55,E12.5,A,E12.5,A)')
     &     '                    Min & Max fuel flow : ',
     &       fFuel_Flow_MIN ( iComp ),',',
     &       fFuel_Flow_MAX (iComp), ' (kg/s) '

         write (itu,'(A55,F10.3,A)')
     &     '           Max cooling water temperature: ',
     &      fCW_Temp_MAX ( iComp ), ' (oC) '

         write (itu,'(A55,F10.3,A,F10.3,A)')
     &     '          Max rates of change in Q & P ',
     &      fHeat_Recovery_Change_MAX(iComp),',',
     &      fGross_Power_Change_MAX(iComp),' (W/s) '

         write (itu,'(A55,F10.3,A)')
     &     '          Start-up period duration: ',
     &      fPeriod_duration(iComp,iOP_startup),' (s) '

         write (itu,'(A55,E10.4,A)')
     &     '         Start-up period fuel flow: ',
     &      fPeriod_Fuel_Flow(iComp,iOP_startup),' (kg/s) '

         write (itu,'(A55,F10.3,A)')
     &     '       Start-up period AC power use: ',
     &      fPeriod_AC_Power(iComp,iOP_startup),' (W) '

         write (itu,'(A55,F10.3,A)')
     &     '       Start-up period heat output: ',
     &      fPeriod_Heat_Recovery(iComp,iOP_startup),' (W) '




         write (itu,'(A55,F10.3,A)')
     &     '                    Cool-down period duration: ',
     &      fPeriod_duration(iComp,iOP_shutdown),' (s) '
         write (itu,'(A55,E10.4,A)')
     &     '         Cooldown period fuel flow: ',
     &      fPeriod_Fuel_Flow(iComp,iOP_startup),' (kg/s) '

         write (itu,'(A55,F10.3,A)')
     &     '       Cooldown period AC power use: ',
     &      fPeriod_AC_Power(iComp,iOP_shutdown),' (W) '

         write (itu,'(A55,F10.3,A)')
     &     '       Cooldown period heat output: ',
     &      fPeriod_Heat_Recovery(iComp,iOP_shutdown),' (W) '




         write (itu,'(A55,E10.4,A)')
     &     '         Standby period fuel flow: ',
     &      fPeriod_Fuel_Flow(iComp,iOP_inoperative),' (kg/s) '

         write (itu,'(A55,F10.3,A)')
     &     '       Standby period AC power use: ',
     &      fPeriod_AC_Power(iComp,iOP_inoperative),' (W) '

         write (itu,'(A55,F10.3,A)')
     &     '       Standby period heat output: ',
     &      fPeriod_Heat_Recovery(iComp,iOP_inoperative),' (W) '

         write (itu,'(A55)')
     &     '       Skin loss coefficients: '
         do iShift = 1, 5
            write (itu,'(A55,F10.3)')
     &     ' ',
     &      fSkinLoss_Coeff( iComp, iShift)
         enddo

         write (itu,'(A55)')
     &     '       Elec. Eff Coefficients: '
         do iShift = 1, 8
            write (itu,'(A55,F10.3)')
     &     ' ',
     &      fElec_Eff_Coeff(iComp, iShift)
         enddo

         write (itu,'(A55)')
     &     '       Ther. Eff Coefficients: '
         do iShift = 1, 8
            write (itu,'(A55,F10.3)')
     &     ' ',
     &      fTher_Eff_Coeff(iComp, iShift)
         enddo

         write (itu,'(A55, F10.3,A,F10.3,A)')
     &     '  Correlation LHV bounds: ',
     &      fCorr_Range_LHV_MIN ( iComp ), ' , ',
     &      fCorr_Range_LHV_MAX ( iComp ), ' (W)'



      endif

      if ( bFatal_error ) stop 'Hydrogen_PEMFC.F: unresolvable error!'

      return
      end



C---------------- H2_PEMFC_H3Kreports_module --------------------------
C
C This procedure transports Data from the H2 PEMFC model to
C the h3kreporting object.
C
C Inputs:
C   - iComponent: pointer to component in plant network
C   - iAction: flag indicating if reports should be initialized,
C              or if data can be transported to the h3kreports
C              module.
C
C Outputs:
C
C    None.
C
C-----------------------------------------------------------------------

      subroutine H2_PEMFC_H3Kreports_module (
     &                 iComponent, iStatus, iModel_Type )
      implicit none
#include "plant.h"
#include "cetc_cogen.h"
#include "Hydrogen_PEMFC.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iComponent
      integer iStatus
      integer iModel_Type

C.....ESP-r commons necessary for reporting

      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15, pcname

C.....Local variables
      character*128 H3Kname       ! holding variable
      character*128 char_temp     ! holding variable
      integer iLength,iNameLength ! Length of strings

      integer iUnit_Status_flag   ! flag indicating unit status

      logical bNums_are_close     ! result of close to zero comparisons

C.....Arrays storing locations of output variables
      integer iStorage_index(mpcom)


C.....Save storage arrays
      save iStorage_index

C.....Named constants:
C.....  -> reporting module status
      integer iInitialize, iReport
      parameter ( iInitialize  = 1,
     &            iReport      = 2 )

C.....  -> Stirling engine status (more flags to come?)
      integer iOff, iOn, iOverHeated
      parameter ( iOff         = 0,
     &            iOn          = 1,
     &            iOverHeated  = 2 )


      if ( iStatus .eq. iInitialize ) then
C----------------------------------------------------------------------
C        Initialize reports
C----------------------------------------------------------------------
C........Get component name and length
         write(char_temp,'(A,A,A)')
     &      'plant/',
     &      pcname(iComponent)
     &               (1:lnblnk(pcname(iComponent))),
     &      '/misc_data'
         call h3k_StoreString(char_temp,
     &                        iStorage_index(iComponent),
     &                        iLength)

      else
C----------------------------------------------------------------------
C        Report data
C----------------------------------------------------------------------
C........Recover component name
         call h3k_RecoverString(iStorage_index(iComponent),
     &                       char_temp, iNameLength)


C----------------------------------------------------------------------
C        Check if efficiencies can be reported: is fuel flow non-zero?
C----------------------------------------------------------------------
         call eclose ( fReport_Hydrogen_flow ( iComponent),
     &                 0.0, 1.0E-07, bNums_are_close )
         if ( .not. bNums_are_close ) then

C...........Fuel Flow is non-zero: efficiencies can be calculated.

C...........Time-step averaged electric efficiency
            write(H3Kname,'(A,A)')
     &         char_temp(1:iNameLength),
     &         '/average/efficiency/electric'
            call add_to_xml_reporting(
     &         fReport_Stack_Eff_Elec_TS_avg(iComponent),
     &         H3Kname,
     &         'units', '(-)',
     &         'Hydrogen PEMFC: time step-averaged electric efficiency'
     &      )

C...........Time-step averaged thermal efficiency
            write(H3Kname,'(A,A)')
     &         char_temp(1:iNameLength),
     &         '/average/efficiency/thermal'
            call add_to_xml_reporting(
     &         fReport_Stack_Eff_Ther_TS_avg(iComponent),
     &         H3Kname,
     &         'units', '(-)',
     &         'Hydrogen PEMFC: time step-averaged thermal efficiency'
     &      )

C...........Time-step averaged cogeneration efficiency
            write(H3Kname,'(A,A)')
     &         char_temp(1:iNameLength),
     &         '/average/efficiency/cogeneration'
            call add_to_xml_reporting(
     &         fReport_Stack_Eff_Cogen_TS_avg(iComponent),
     &         H3Kname,
     &         'units', '(-)',
     &         'Hydrogen PEMFC: time step-averaged cogeneration '
     &         // 'efficiency'  )

         endif

C........Hydrogen supply flow rate
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/hydrogen/supply/mass_flow_rate'

         call add_to_xml_reporting(
     &      fReport_Hydrogen_flow(iComponent),
     &      H3Kname,
     &      'units', '(kg/s)',
     &      'Hydrogen PEMFC: mass flow rate of supplied hydrogen '
     &      )

C........Hydrogen supply LHV
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/hydrogen/supply/LHV_flow_rate'

         call add_to_xml_reporting(
     &      fReport_Hydrogen_LHV(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Hydrogen PEMFC: LHV flow rate of supplied hydrogen '
     &      )

C........Hydrogen demand flow rate
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/hydrogen/demand/mass_flow_rate'

         call add_to_xml_reporting(
     &      fReport_Hydrogen_demand(iComponent),
     &      H3Kname,
     &      'units', '(kg/s)',
     &      'Hydrogen PEMFC: hydrogen mass flow rate requested '
     &      // 'by unit')

C........Hydrogen demand LHV
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/hydrogen/demand/LHV_flow_rate'

         call add_to_xml_reporting(
     &      fReport_Hydrogen_demand_LHV(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Hydrogen PEMFC: LHV flow rate hydrogen requested '
     &      // ' by unit')


C........Stack voltage
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/stack/voltage'

         call add_to_xml_reporting(
     &      fReport_Stack_Voltage(iComponent),
     &      H3Kname,
     &      'units', '(V)',
     &      'Hydrogen PEMFC: Stack operating voltage'
     &      )

C........Stack current
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/stack/current'

         call add_to_xml_reporting(
     &      fReport_Stack_Current_TS_avg(iComponent),
     &      H3Kname,
     &      'units', '(amps)',
     &      'Hydrogen PEMFC: Stack current, averaged over time step'
     &      )

C........AC power usage (-ive ->O)
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/average/power/AC'

         call add_to_xml_reporting(
     &      fReport_AC_Power_TS_avg(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Hydrogen PEMFC: AC power generation, averaged over'
     &      // ' time step')

C........DC power generation ( +ive O->)
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/average/power/DC'

         call add_to_xml_reporting(
     &      fReport_DC_Power_TS_avg(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Hydrogen PEMFC: DC power generation, averaged over'
     &      // ' time step')

C........Heat production
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/average/heat_output'

         call add_to_xml_reporting(
     &      fReport_heat_TS_avg(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Hydrogen PEMFC: thermal output, averaged over time step')

C........Skin losses
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/average/skin_losses'

         call add_to_xml_reporting(
     &      fReport_Skin_Losses_TS_avg(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Hydrogen PEMFC: skin losses, averaged over timestep')

C.......Control scheme
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/control/scheme'

         call add_to_xml_reporting(
     &      float ( iReport_Control_method (iComponent) ),
     &      H3Kname,
     &      'units', '(-)',
     &      'Hydrogen PEMFC: control scheme')

C........Control signal
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/control/signal'

         call add_to_xml_reporting(
     &      fReport_Control_signal (iComponent) ,
     &      H3Kname,
     &      'units', '(-)',
     &      'Hydrogen PEMFC: signal from external controller')

C........Time spent in standby
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/operating_mode/standby'

         call add_to_xml_reporting(
     &      fReport_TS_modes (iComponent, iOP_inoperative),
     &      H3Kname,
     &      'units', '(s)',
     &      'Hydrogen PEMFC: time spent in standby')

C........Time spent in startup
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/operating_mode/startup'

         call add_to_xml_reporting(
     &      fReport_TS_modes (iComponent, iOP_startup),
     &      H3Kname,
     &      'units', '(s)',
     &      'Hydrogen PEMFC: time spent in startup')

C........Time spent in normal operation
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/operating_mode/normal_operation'

         call add_to_xml_reporting(
     &      fReport_TS_modes (iComponent, iOP_normal_operation),
     &      H3Kname,
     &      'units', '(s)',
     &      'Hydrogen PEMFC: time spent in normal operation')

C........Time spent in standby
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/operating_mode/cool_down'

         call add_to_xml_reporting(
     &      fReport_TS_modes (iComponent, iOP_shutdown),
     &      H3Kname,
     &      'units', '(s)',
     &      'Hydrogen PEMFC: time spent in cool down')


      endif

      return
      end
