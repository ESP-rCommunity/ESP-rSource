C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
! C of Canada 2004/2005. Please Contact Ian
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
C================= Hydrogen_MH_store.F ================================
C
C This file contains procedures associated with modelling a hydrogen
C metal-hydride storage tank
C
C It contains the following procedures:
C
C    - H2_MH_Coeff_Gen: Top level solution routine that
C        returns the plant component matrix coefficients
C        expected by ESP-r.
C
C    - H2_MH_H2_balance: Routine characterizing
C        storage tank's mass balance.
C
C    - H2_MH_Q_balance: Supervisory routine for the
C        storage tank's energy balance.
C
C----------------------------------------------------------------------
C
C References:
C
C  Ferguson, 2005. "Hydrogen compression, storage and conversion
C      technologies: literature review and modelling recommendations"
C      CETC working document.
C
C  Jaing, Dougal, Lui et al. 2004 "Simulation of a thermally coupled
C      metal hydride hydrogen storage and fuel cell system",
C      Journal of Power sources, article in press.
C
C======================================================================
C======================================================================

C---------------- H2_MH_Coeff_Gen -------------------------------------
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.
C
C ABSTRACT:
C This routine provides ESP-r's plant matrix domain with the
C coefficients necessary to characterize hydrogen metal hydride storage
C tank's interactions with the rest of the plant network. The
C coefficients for the first and second phase matricies are calculated
C directly, while the hydrogen flow and  temperature matrix
C coefficients are calculated by the subordinate routines
C H2_MH_H2_balance and H2_MH_Q_balance.
C
C Inputs:
C
C  iComponent       - index of component in plant network
C  iPlant_Matrix          - index of plant matrix being solved
C
C Outputs
C  fCoefficients          - array containing components matrix
C                           coefficients
C----------------------------------------------------------------------
      subroutine H2_MH_Coeff_Gen(
     &               iComponent,
     &               fCoefficients,
     &               iPlant_Matrix  )

      implicit none

#include "building.h"
#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"
#include "CETC_definitions.h"
#include "Hydrogen_demand_controller.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent         ! pointer to component in plant network
      integer iPlant_Matrix      ! index of plant matrix to be solved

      real fCoefficients(mpcoe)  ! array of component matrix coefficients

C----------------------------------------------------------------------
C     ESP-r common blocks
C----------------------------------------------------------------------

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu


C.....Common storing status of plant component initialization.
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Additional plant component data requiring time-row transport.
      common/pcdat/
     &     pcdatf(mpcom,mpcdat),
     &     pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)

C.....Plant network & control data
      common/c9/npcomp,nci,cdata
      integer npcomp            ! number of plant components
      integer nci(mpcom)        ! number of controls / component
      real cdata(mpcom,mmiscd)  ! control data for each component

C.....Misc. plant component data.
      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

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
      real csvi                 !'initial' values of state variables
                                !    at start of iteration
      common/pcres/
     &     qdata(mpcom),
     &     pcaout(mpcom,mpcres),
     &     napdat(mpcom)
      real qdata                ! not used in current context
      real pcaout               ! not used in current context
      integer napdat            ! # of plant additional outputs

C.....Plant network time constants
      COMMON/pctc/tc
      real tc(mpcom)            ! time contant (seconds)

C.....Plant present and future state variables
      common/pcval/csvf,csvp
      real csvf(mpnode,mpvar)   ! future time-row state variables
      real csvp(mpnode,mpvar)   ! present time-row state variables

C.....Plant component connection data
      common/pcond/convar, icontp, icondx
      real    convar(mpcon,mconvr)        ! state varibles for connections
      integer icontp(mpcon)               ! type of each connection
      integer icondx(mpcom,mnodec,mpconc) ! pointer to connections for each
                                          ! component/node

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

C.....Plant matrix solver: implicit vs explicit weighting
      common/pcequ/impexp,ratimp
      integer impexp
      real ratimp

C.....Miscellaneous plant data
      common/pcvar/pctf,pcrf,puaf,pcqf,pcntmf,pctp,
     &      pcrp,puap,pcqp,pcntmp
      real pctf(mpcon)
      real pcrf(mpcon)
      real puaf(mpnode)
      real pcqf(mpnode)
      real pcntmf(mpcom)        ! future time row plant containment temp (oC)
      real pctp(mpcon)
      real pcrp(mpcon)
      real puap(mpnode)
      real pcqp(mpnode)
      real pcntmp(mpcom)        ! present time row plant containment temp (oC)


C----------------------------------------------------------------------
C     Logical array indicating if plant components have support for
C     the fourth hydrogen matrix
C----------------------------------------------------------------------
      common  / H2_lib / h2flowsupport
      logical h2flowsupport(MPCOM)

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------

C.....model topology
      integer iH2_node         ! index of hydrogen storage node
      integer iH2_connection   ! index of connection to hydrogen storage
                               !    node
      integer iH2O_node        ! index of cooling water node
      integer iH2O_connection  ! index of connection to cooling water
                               !    node

C.....Cylinder parameters
      real fCyl_free_volume    ! Free volume of cylinder (that which can
                               !   be occupied by gaseous hydrogen, m3)
      real fCyl_solid_mass     ! Mass of cylinder wall (kg)
      real fCyl_solid_Cp       ! Specific heat of cylinder wall (J/kg oC)
      real fCyl_UA_ambient     ! Heat transfer coeff. between cylinder
                               !   & ambient (W/oC)
      real fCyl_MAX_press      ! Maximum gas pressure (Pa)
      real fCyl_MIN_press      ! Downstream operating pressure (Pa)
      real fCyl_Init_press     ! Inital pressure in cylinder @
                               !    start of simulation. (Pa)

      real fEffective_C        ! Effective heat capacitance of
                               !    cylinder and contents
                                                                   

C.....State variables & other data
      real fCyl_Temp_P          ! Present time-row temperature (oC)
      real fCyl_Temp_F          ! Future time-row temperature  (oC)
      real fCyl_Press_P         ! Present time-row pressure (Pa)
      real fCyl_Press_F         ! Future time-row pressure (Pa)
      real fCyl_H2_mass_P       ! Present time-row mass of encapsulated
                                !   hydrogen (kg)
      real fCyl_H2_mass_F       ! future time-row mass of encapsulated
                                !   hydrogen (kg)
      real fCyl_H2_Cv_P         ! Specific heat of encapsulated hydrogen
                                !   at present time-row system averaged
                                !   temperature (J/kg oC)
      real fCyl_H2_Cv_F         ! Specific heat of encapsulated hydrogen
                                !   at future time-row system averaged
                                !   temperature (J/kg oC)

      real fH2_free_kmol_P      ! Present time row encapsulated hydrogen
                                !   in gaseous form (kmol)
      real fH_absorbed_kmol_P   ! Present time row encapsulated hydrogen
                                !   in hydride form (kmol)
      real fH2_free_kmol_F      ! Future time row encapsulated hydrogen
                                !   in gaseous form (kmol)
      real fH_absorbed_kmol_F   ! Future time row encapsulated hydrogen
                                !   in hydride form (kmol)

      real fH_absorbed_dNdt_P   ! Rate of change in kmol of absorbed hydrogen
                                !   on present time row (kmol/s)
      real fH_absorbed_dNdt_F   ! Rate of change in kmol of absorbed hydrogen
                                !   on future time row (kmol/s)
                                                          
      real fH_absorbed_kmol_MIN ! Amount of absorbed hydrogen corresponding
                                !   to minimum operating pressure (kmol)
      real fH_absorbed_kmol_MAX ! Amount of absorbed hydrogen corresponding
                                !   maximum concentration (kmol)

      real fConcentration_F     ! Concentration of absorbed hydrogen (mol/mol)

      real fPressure_dPdt       ! Rate of change in the pressure inside the
                                !   cylinder (Pa/s)

      real fCyl_Time_Constant   ! Cylinder time constant (s)

      real fCyl_H2_demand_EXP   ! Hydrogen demanded by external controller
                                !    for explicitly modelled hydrogen flow
                                !    (kmol/s)
      real fCyl_H2_demand_IMP   ! Hydrogen demanded by external controller
                                !    for implicitly modelled hydrogen flow
                                !    (kmol/s)
      real fCyl_H2_demand_SUM   ! Total Hydrogen by from external controller
                                !    (kmol/s)
      real fCyl_H2_vent         ! Rate at which hydrogen must be vented to
                                !     the atmosphere (kmol/s)
      real fCyl_H2_discharge_P  ! Rate at which hydrogen is delivered by
                                !     the tank (kmol/s)
      real fCyl_H2_discharge_F  ! Rate at which hydrogen is delivered by
                                !     the tank (kmol/s)
      real fCyl_H2_charge_P     ! Rate at which hydrogen is stored within
                                !     the tank (kmol/s)
      real fCyl_H2_charge_F     ! Rate at which hydrogen is stored within
                                !     the tank (kmol/s)

      real fH_abs_dN_kmols_P    ! Present time row change in kmols of absorbed
                                !     hydrogen (kmol/s)
      real fH_abs_dN_kmols_F    ! Future time row change in kmols of absorbed
                                !     hydrogen (kmol/s)

      real fQ_absorbtion_J_P    ! Heat released during absorbtion (J), present
                                !     time row                                 
      real fQ_absorbtion_J_F    ! Heat released during absorbtion (J), future
                                !     time row

      real fCyl_dH2_kmol_s      ! Rate at which the amount of hydrogen stored
                                !     in the tank changes (kmol/s)
      real fSoln_alpha          ! implicit/explicit weighting factor (-)

      real fH2_Disc_FlowWork_P  ! Flow work associated with hydrogen discharge
                                !    present time row (J)
      real fH2_Disc_FlowWork_F  ! Flow work associated with hydrogen discharge
                                !    future time row (J)

      real fQ_heat_loss_P       ! Present time row heat loss to surroundings (J)
                                                                                             
C.....Connection properties (hydrogen connection)
      real fH2_Conn_Cv_P        ! Specific heat of incomming hydrogen
                                !   at present time-row system averaged
                                !   temperature (J/kg oC)
      real fH2_Conn_Cv_F        ! Specific heat of incomming hydrogen
                                !   at future time-row system averaged
                                !   temperature (J/kg oC)
      real fH2_Conn_Temp_P      ! Presnet time-row H2 connection temperature (oC)
      real fH2_Conn_Temp_F      ! Future time-row H2 connection temperaturte (oC)
      real fH2_Conn_Flow_kgs    ! Connection hydrogen mass flow rate (kg/s)

      real fH2_Conn_FlowWork_P  ! Flow work associated with incomming connection
                                !    present time row (J)
      real fH2_Conn_FlowWork_F  ! Flow work associated with incomming connection
                                !    future time row (J)

      real fH2_Conn_dU_P        ! Internal energy change occuring in incomming
                                !    hydrogen flow (J) on present time row.
      real fH2_Conn_dU_F        ! Internal energy change occuring in incomming
                                !    hydrogen flow (J) on present time row.
                                                                                        
                                
C.....Connection properties (water connection)
      logical bH2O_connected    ! Flag indicating that cooling
                                !    water connectione exists.    
      logical bNo_H2O_flow      ! Flag indicating no cooling water is
                                !    flowing the cylinder
      
      real fH2O_node_temp_P     ! Present water node temperature (oC)
      real fH2O_node_temp_F     ! Future water node temperature (oC)

      real fH2O_Conn_Cv_P       ! Specific heat of incomming water
                                !   at present time-row system averaged
                                !   temperature (J/kg oC)
      real fH2O_Conn_Cv_F       ! Specific heat of incomming water
                                !   at future time-row system averaged
                                !   temperature (J/kg oC)
      real fH2O_Conn_Temp_P     ! Present time-row water connection temperature (oC)
      real fH2O_Conn_Temp_F     ! Future time-row water connection temperaturte (oC)
      
      real fH2O_Conn_Flow_P     ! Present time row Water connection mass flow rate(kg/s)
      real fH2O_Conn_Flow_F     ! Future time row Water connection mass flow rate (kg/s)

      real fQ_cooling_water_P   ! Present time row heat transfer to cooling water (J)
      real fQ_cooling_water_F   ! Future time row heat transfer to cooling water (J)
      
C.....Ambient conditions
      real fAmb_Temp_P          ! Ambient present temperature (oC)
      real fAmb_Temp_F          ! Ambient future temperature (oC)

C.....Global properties
      real fR_hydrogen          ! H2 ideal gas constant ( kJ/kg K)
      save fR_hydrogen

      real fMM_hydrogen         ! H2 molar mass (kg/kmol)
      save fMM_hydrogen

      real fTS_duration         ! timestep duration (s)
      save fTS_duration

      integer iGas_LAW          ! Flag indicating which gas law is in use
                                ! -> 1 = ideal gas
                                ! -> 2 = Lee Kesler (not yet supported)

      logical bDebug            ! flag indicating if debugging is active

C-----------------------------------------------------------------------
C     Misc local variables
C-----------------------------------------------------------------------
      integer iCount_a,iCount_b ! Counters
      logical bNums_Are_Close   ! logical result of close-to-zero comparisons

C----------------------------------------------------------------------
C     Named constants
C----------------------------------------------------------------------
      real fC_to_K              ! celsius -> kevlin conversion
      parameter ( fC_to_K = 273.15 )


C-----------------------------------------------------------------------
C     Interfaces to chemical_properties.F
C-----------------------------------------------------------------------
      real fEval_Compound_Cv    ! returns the ideal gas constant
                                ! pressure specific heat of a compound
      real fCompound_R          ! returns the ideal gas constant
                                ! of a compound (kJ/kg K)
      real fEval_Compound_MM    ! returns the molar mass of a compound

C-----------------------------------------------------------------------
C     ESP-r references
C-----------------------------------------------------------------------
      real fMH_Eval_Langmuir_C  ! Returns the equilibrium concentration
                                !   associated with a given pressure.
      real fMH_Eval_Langmuir_P  ! Returns the pressure associated with
                                !   a given equilibrium concentration.
      real fMH_Eval_Q_reaction  ! Returns the heat of reaction
                                !   associated with the absorption
                                !   of hydrogen
      real shtfld               ! ESRU function returning the
                                !   specific heat of air, water vapor,
                                !   or water.
C----------------------------------------------------------------------
C     Initialization
C----------------------------------------------------------------------
      bDebug = .false.

      if (bDebug) write(ITU,*)
     &     "Entering subroutine H2_MH_balance()"



C----------------------------------------------------------------------
C     Initialization
C----------------------------------------------------------------------

      if ( .not. bInitialized ( iComponent ) ) then

         bInitialized ( iComponent ) = .true.

C----------------------------------------------------------------------
C        Collect & validate inputs
C----------------------------------------------------------------------
         call MH_Validate_Inputs ( iComponent )
C----------------------------------------------------------------------
C        Collect molar mass of hydrogen
C----------------------------------------------------------------------
         fMM_hydrogen = fEval_compound_MM ( iHydrogen ) ! (kg/kmol)

C----------------------------------------------------------------------
C        Calculate timestep duration
C----------------------------------------------------------------------
         fTS_duration = 3600. / ( ntstep * ntstpp ) ! s

C----------------------------------------------------------------------
C        Initialize post-processing routines.
C----------------------------------------------------------------------
         call MH_hydride_PostPro ( iComponent, iInitialize )

C-----------------------------------------------------------------------
C        Enable hydrogen flow support for MH component
C-----------------------------------------------------------------------
         h2flowsupport ( iComponent ) = .true.
         
C----------------------------------------------------------------------
C        Dump sample PCT and discharge profiles for debugging /
C        validation.
C----------------------------------------------------------------------
         
         if ( bDebug ) then

            call MH_debug_PCT ( iComponent )

         endif

      endif

      
C-------------------------------------------------------------------------------
C     Get timestep duration:
C
C                                      seconds per hour
C     duration = -------------------------------------------------------------
C                (# building steps / hour ) * ( # plant steps / building step )
C
C-------------------------------------------------------------------------------

      fTS_duration = 3600.0 / real ( nTStep * nTStpp )

C----------------------------------------------------------------------
C     Determine component location in plant network topology, and
C     mark component node for iteration
C
C     -> Get node index, and mark node for iteration:
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
C
C----------------------------------------------------------------------

C.....H2 & H2O node indicies
      iH2_node  = NPCDAT ( iComponent, 9 )
      iH2O_node = NPCDAT ( iComponent, 9 ) + 1

C.....Mark node temperatures for iteration & update
C.....initial value
      icsv( iH2_node, iPropTemp )  = 1
      csvi( iH2_node, iPropTemp )  = csvf( iH2_node, iPropTemp )

      icsv( iH2O_node, iPropTemp ) = 1
      csvi( iH2O_node, iPropTemp ) = csvf( iH2O_node, iPropTemp )


C----------------------------------------------------------------------
C     Collect properties of incomming hydrogen connection
C----------------------------------------------------------------------

C.....Connection index
C..... -> icondx (i,j,k) holds the connection number for the k'th
C.....    connection to the j'th node of component i. It is used
C.....    as a pointer.
      iH2_connection  = iCondX ( iComponent, 1, 1 )
      iH2O_connection = iCondX ( iComponent, 2, 1 )

C----------------------------------------------------------------------
C     Does H2O connection exist?
C----------------------------------------------------------------------     
      if ( iH2O_connection .gt. 0 ) then
C........Connection exists --- set flag      
         bH2O_connected = .true.
      else
         bH2O_connected = .false.
      endif 

C.....Future time row hydrogen connection temperature (oC)
      fH2_Conn_Temp_F  = ConVar ( iH2_connection, iPropTemp )

C.....Future time row hydrogen connection mass flow rate (kg/s)
      fH2_Conn_Flow_kgs  =  ConVar ( iH2_connection, iPropH2Flow )
     &        * pcondr( iH2_connection )

C.....Collect cooling water properties if connection exists       
      if ( bH2O_connected ) then 

C........Future time row water connection temperature (oC)
         fH2O_Conn_Temp_F = ConVar ( iH2O_connection, iPropTemp )

C........Future time row water connection mass flow rate (kg/s)
         fH2O_Conn_Flow_F = ConVar ( iH2O_connection, iProp1stFlow )
     &        * pcondr( iH2O_connection )
      else
C........No connection exists --- set connection properties to zero.
         fH2O_Conn_Temp_F = 0.0
         fH2O_Conn_Flow_F = 0.0
         fMH_Heat_XFer_UA ( iComponent ) = 0.0
                  
      endif 

C.....Check if cooling water is flowing through cylinder
      call eclose ( fH2O_Conn_Flow_F, 0.0, 1.0E-06, bNo_H2O_flow )
      
C----------------------------------------------------------------------
C     Collect present & future time-row plant containment temperatures.
C
C     -> Check that containment has been defined. If containment has
C        not been defined, pcntmp/pcntmf will contain a value of -99.
C----------------------------------------------------------------------
      fAmb_Temp_P = pcntmp ( iComponent )        ! (oC)
      fAmb_Temp_F = pcntmf ( iComponent )        ! (oC)

      call eclose ( fAmb_Temp_F , -99.0, 0.1, bNums_Are_Close )

      if ( bNums_Are_Close ) then

C........Containment has not been defined --- set UA value to zero.
         fCyl_UA_ambient = 0.0

      endif

C----------------------------------------------------------------------
C     Collect temperatue state variables from present-row time blocks
C----------------------------------------------------------------------
      
C.....Hydride cylinder temperatures
      fCyl_Temp_P = CSVp( iH2_node, iPropTemp )        ! (oC)
      fCyl_Temp_F = CSVf( iH2_node, iPropTemp )        ! (oC)

C.....Cooling water outlet temperatures
      fH2O_node_temp_P = CSVp ( iH2O_node, iPropTemp ) ! (oC)
      fH2O_node_temp_f = CSVf ( iH2O_node, iPropTemp ) ! (oC)



      if ( nsinc .eq. 1 ) then

C........Set tank pressure to initial value
         fCyl_Press_P = fMH_Pressure_INIT (iComponent) ! (Pa)

C----------------------------------------------------------------------
C        Calculate initial free & absorbed hydrogen. Remember to
C        convert oC->K!
C----------------------------------------------------------------------
         call MH_Eval_initial_conditions(
     &        iComponent,
     &        fCyl_Press_P,
     &        fCyl_Temp_P + fC_to_K ,
     &        fH2_free_kmol_P,
     &        fH_absorbed_kmol_P )

C........Set initial connection present time-row temperature
C........to initial future time-row value
         fH2_Conn_Temp_P    = fH2_Conn_Temp_F               ! (oC)

      else

C----------------------------------------------------------------------
C        Collect present time-row state variables from miscellaneous
C        time row data arrays ( PCDatP / PCDatF )
C----------------------------------------------------------------------

C........Free & absorbed hydrogen
         fH2_free_kmol_P     = PCDatP ( iComponent, 1 ) ! (kmol)
         fH2_free_kmol_F     = PCDatF ( iComponent, 1 ) ! (kmol)
         
         fH_absorbed_kmol_P  = PCDatP ( iComponent, 2 ) ! (kmol)
         fH_absorbed_kmol_F  = PCDatF ( iComponent, 2 ) ! (kmol)
         
C........Cylinder pressure
         fCyl_Press_P        = PCDatP ( iComponent, 3 ) ! (Pa)
         fCyl_Press_F        = PCDatF ( iComponent, 3 ) ! (Pa)
         
C........Rate of change in absorbed hydrogen         
         fH_absorbed_dNdt_P  = PCDatP ( iComponent, 4 ) ! (kmol/s)
         fH_absorbed_dNdt_F  = PCDatF ( iComponent, 4 ) ! (kmol/s)
         
C........Charging rate         
         fCyl_H2_charge_P    = PCDatP ( iComponent, 5 ) ! (kmol/s)
         fCyl_H2_charge_F    = PCDatF ( iComponent, 5 ) ! (kmol/s)

C........Discharging rate
         fCyl_H2_discharge_P = PCDatP ( iComponent, 6 ) ! (kmol/s)
         fCyl_H2_discharge_F = PCDatP ( iComponent, 6 ) ! (kmol/s)


         
         if ( bH2O_connected ) then 
C...........Cooling water connection flow rate
            fH2O_Conn_Flow_P    = PCDatP ( iComponent, 7 ) ! (kg/s)

C...........Cooling water connection temperature
            fH2O_Conn_Temp_P    = PCDatP ( iComponent, 8)  ! (oC)
         else
            fH2O_Conn_Flow_P = 0.0
            fH2O_Conn_Temp_P = 0.0
         endif 

C........Hydrogen connection temperature 
         fH2_Conn_Temp_P     = PCDatP ( iComponent, 9 ) ! (K)

      endif
      
C----------------------------------------------------------------------
C     MODEL TOPOLOGY:
C
C                             .------------> Heat loss
C                             |
C                    ---------------------
C     Hydrogen       |   Metal Hydride   |        Hydrogen
C     -------------->|      storage      |---------------->
C    (connection 1)  |       tank        |
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

C     Determine which matrix is being solved, and set coefficients
C     accordingly.
C----------------------------------------------------------------------
      if ( iPlant_Matrix .eq. iProp1stFlow ) then

C----------------------------------------------------------------------
C        Plant first-flow (ie water/air) matrix is to be solved.
C        Water flows through 2nd node of H2 MH model, &
C        conservation of mass applies. Therefore, state equation is:
C
C        | 1.0    0.0    0.0   0.0 | | mass flow, 1st node| =  | 0.0 |
C        | 0.0    1.0    0.0  -1.0 | | mass flow, 2nd node|    | 0.0 |
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
         if ( bH2O_connected ) then
            fCoefficients(6) =  -1.0 * pcondr ( iH2O_connection )
         else
            fCoefficients(6) = 0.0
         endif 
C........RHS vector
         fCoefficients(7) =  0.0
         fCoefficients(8) =  0.0

      elseif (iPlant_Matrix .eq. iProp2ndFlow ) then
C----------------------------------------------------------------------
C        Plant second-flow (ie water vapor) matrix is to be solved.
C        H2 MH model does not support explicit modelling of water
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

      elseif ( iPlant_Matrix .eq. iPropTemp ) then
C----------------------------------------------------------------------
C        Calculate cooling water specific heat at present & future
C        time rows --- use averate of inlet and outlet tempertatures
C        to determine Cv.
C
C        function ShtFld (X, Y) returns the heat capacity of fluid X
C        at temperature Y in J/kg oC. 
C
C        X=1 -> air, X=2 -> vapor, X=3 -> liq. water
C
C----------------------------------------------------------------------

C.....Present time row
      fH2O_Conn_Cv_P =  SHtFld ( 3,
     &     0.5 * (fH2O_Conn_Temp_P + fH2O_node_Temp_P) ) ! J/kg K

C.....Future time row       
      fH2O_Conn_Cv_F =  SHtFld ( 3,
     &     0.5 * (fH2O_Conn_Temp_F + fH2O_node_Temp_F) ) ! J/kg K
      
C-------------------------------------------------------------------------
C        Collect constant-volume specific heats at present & future
C        temperatures (J/kg K)
C-------------------------------------------------------------------------

C........Fill Context buffer for for possible warning messages
C........Generated by chemical_properties.F
         write (cContext,'(A,I2,A)')
     &    'Metal Hydride storage model (plant component # ', iComponent,
     &    ')'
         fH2_Conn_Cv_P =
     &     fEval_Compound_Cv ( iHydrogen, fH2_Conn_Temp_P+ fC_to_K )

         fH2_Conn_Cv_F   =
     &     fEval_Compound_Cv ( iHydrogen, fH2_Conn_Temp_F + fC_to_K )

         fCyl_H2_Cv_P   =
     &     fEval_Compound_Cv ( iHydrogen, fCyl_Temp_P + fC_to_K )

         fCyl_H2_Cv_F    =
     &     fEval_Compound_Cv ( iHydrogen, fCyl_Temp_F + fC_to_K )

C........Clear context buffer to prevent erroneous error messages
         cContext = ' '         

C----------------------------------------------------------------------
C        Determine the internal energy change associated as
C        incomming hydrogen flow heats/cools to the system
C        temperature
C
C           dU = ( inlet H2 mass flow ) * ( present inlet H2 cv )
C              * ( inlet temperature - cylinder temperature )
C
C----------------------------------------------------------------------

C........Present time row         
         fH2_Conn_dU_P = fCyl_H2_charge_P / fMM_hydrogen
     &        * fH2_Conn_Cv_P * ( fH2_Conn_Temp_P - fCyl_Temp_P )

! C........Future time row 
!          fH2_Conn_dU_F = fCyl_H2_charge_F / fMM_hydrogen
!      &        * fH2_Conn_Cv_P * ( fH2_Conn_Temp_P - fCyl_Temp_P )

C-----------------------------------------------------------------------
C        Determine heat released during absorption of hydrogen
C
C                dN absorbed
C           Q =  ----------- * heat of absorbtion
C                    2.0
C
C        The function fMH_Eval_Q_reaction will be used to determine the
C        heat of absorbtion --- it will return the energy released
C        when one kmol of *hydrogen gas* (ie H2) is absorbed.
C
C-----------------------------------------------------------------------

C........Present time row 
         fQ_absorbtion_J_P = fH_absorbed_dNdt_P / 2.0
     &       * fMH_Eval_Q_reaction
     &              ( iComponent, fCyl_Press_P, fCyl_Press_P + fC_to_K ) 

C........Future time row 
         fQ_absorbtion_J_F = fH_absorbed_dNdt_F / 2.0
     &       * fMH_Eval_Q_reaction
     &              ( iComponent, fCyl_Press_F, fCyl_Press_F + fC_to_K )

C----------------------------------------------------------------------
C        Evaluate flow work associated with charging/discharging
C        cylinder.
C
C        flow work = pressure * volumetric flow 
C
C                  = molar flow * Univ. Gas Const. * temperature (ideal gas)
C
C----------------------------------------------------------------------

C........Present time row charging          
         fH2_Conn_FlowWork_P = fCyl_H2_charge_P
     &         * fR_Universal * ( fH2_Conn_Temp_P + fC_to_K )  ! (J)

C........Future time row charging              
         fH2_Conn_FlowWork_F = fCyl_H2_charge_F
     &         * fR_Universal * ( fH2_Conn_Temp_F + fC_to_K )  ! (J)

C........Present time row discharging
         fH2_Disc_FlowWork_P = fCyl_H2_discharge_P
     &         * fR_universal * ( fCyl_Temp_P + fC_to_K )      ! (J)

C........Future time row discharging
         fH2_Disc_FlowWork_F = fCyl_H2_discharge_F
     &         * fR_universal * ( fCyl_Temp_F + fC_to_K )      ! (J)

C----------------------------------------------------------------------
C        Determine heat transfer to cooling water:
C
C           Q_cooling = mass flow
C                * specific heat ( outlet temp - inlet temp )
C
C----------------------------------------------------------------------     

C........Present time row
         fQ_cooling_water_P = fH2O_Conn_Flow_P * fH2O_Conn_Cv_P
     &        * ( fH2O_node_temp_P - fH2O_Conn_Temp_P )

C----------------------------------------------------------------------
C        Determine present-time row heat losses
C
C           q loss = hA * ( hydride temp - ambient temp )
C       
C----------------------------------------------------------------------

C........Present time row
         fQ_heat_loss_P = fMH_cyl_wall_hA ( iComponent ) 
     &          * ( fCyl_Temp_P - fAmb_Temp_P ) 



C-------------------------------------------------------------------------
C        Estimate time constant using present time row data
C
C                        H2 heat capacity + Cyl. heat capacity
C        time constant = -------------------------------------------  (s)
C                        connection heat capacity +
C                              tank->ambient heat xfer coefficient.
C
C-------------------------------------------------------------------------
C........Ensure denominator will not be zero!
         call eclose (
     &      (  fCyl_H2_charge_P * fCyl_H2_Cv_P
     &        + fH2O_Conn_Flow_P * fH2O_Conn_Cv_P
     &        + fMH_cyl_wall_hA ( iComponent )
     &      ), 0.0, 1.0E-03, bNums_Are_Close )
         
         if ( bNums_Are_Close ) then
C...........Time constant is arbitrarly large:             
     
            fCyl_Time_Constant = 10.0 * fTS_duration
         
         else 
            fCyl_Time_Constant =
     &       (  fH2_free_kmol_P * fCyl_H2_Cv_P
     &        + fH_absorbed_kmol_P * fMH_hydride_C ( iComponent )
     &        + (   fMH_kmols_alloy ( iComponent )
     &            - fH_absorbed_kmol_P ) * fMH_alloy_C ( iComponent )
     &        + fMH_cyl_mass_kg(iComponent) * fMH_cyl_wall_C(iComponent)
     &       )
     &     / (  fCyl_H2_charge_P * fCyl_H2_Cv_P
     &        + fH2O_Conn_Flow_P * fH2O_Conn_Cv_P
     &        + fMH_cyl_wall_hA ( iComponent )
     &      )

         endif 
C........Save time constant in ESRU commonblock (why?)
         TC(iComponent) = fCyl_Time_Constant

C-------------------------------------------------------------------------
C        Determine explicit/implicit weighting factor (alpha).
C
C        -> weighting factor determines if the forward difference
C           solution of the energy balance differential equation
C          (alpha = 1), the backwards differewce solution (alpha = 0),
C           or some weighted average of the two (0 < alpha < 1) should
C           be used.
C
C        See Equation 68 in Ferguson 2005
C
C-------------------------------------------------------------------------
         if ( impexp .eq. 1 ) then
C...........Fully implicit solution has been specified by the user
            fSoln_alpha = 1.0

         elseif (impexp .eq. 2 ) then
C...........Mixed  solution scheme. Use provided weighting factor ratimp
            fSoln_alpha = ratimp

         elseif (impexp .eq. 3 ) then
C...........Compare simulation timestep with component's estimated
C...........time consant, and revert to fully implicit solution
C...........if timesteps are too large to accuartely characterize
C...........component
            if ( fTS_duration .gt. 0.63 * fCyl_Time_Constant ) then

               fSoln_alpha = 1.0

            else

               fSoln_alpha = ratimp

            endif

         elseif ( IMPEXP .EQ. 4 ) then
C..........'steady state' solution. This feature is untested, and
C...........will quite possibly cause spectacular errors in
C...........the simulation. A warning is issued during the
C...........error trapping procedure.


         else
C...........Shouldn't happen, as impexp is error trapped elsewhere?
            STOP 'Comp_Cyl_Q_balance: Solution scheme is unsupported'
         endif


C----------------------------------------------------------------------
C        Determine Effective system heat capacitance
C
C
C        eff. Q Cap = ( 1 - alpha ) * (   mols H2 * Cv H2
C                                       + mols MH * Cv MH
C                                       + mols M  * Cv M )_{t}
C                     + alpha       * (   mols H2 * Cv H2
C                                       + mols MH * Cv MH
C                                       + mols M  * Cv M )_{t+dt}
C 
C                     + Cyl mass * Cyl specific heat
C
C        See Equation 119, Ferguson 2005
C
C----------------------------------------------------------------------
         if ( IMPEXP .eq. 4 ) then
C...........'quasi-steady-state' solution: these capabilities are
C...........untested, and may produce spetacular errors!
            fEffective_C = 0.0

         else
C...........Normal, dynamic solution. Effective heat capacity must
C...........be determined.
            fEffective_C = ( 1.0 - fSoln_alpha )
     &           * (   fH2_free_kmol_P * fCyl_H2_Cv_P
     &               + fH_absorbed_kmol_P * fMH_hydride_C ( iComponent )
     *               + (  fMH_kmols_alloy ( iComponent )
     &                  - fH_absorbed_kmol_P
     &                  ) * fMH_alloy_C ( iComponent )
     &             )
     &      + fSoln_alpha
     &           * (   fH2_free_kmol_F * fCyl_H2_Cv_F
     &               + fH_absorbed_kmol_F * fMH_hydride_C ( iComponent )
     *               + (  fMH_kmols_alloy ( iComponent )
     &                  - fH_absorbed_kmol_F
     &                  ) * fMH_alloy_C ( iComponent )
     &             )
     &      + fMH_cyl_mass_kg ( iComponent )
     &          * fMH_cyl_wall_C ( iComponent )
  
         
         endif 


         
C-------------------------------------------------------------------------
C        Hydrogen metal-hydride storage thermal balance matrix:
C
C        | a    b    e   - | | temperature, 1st node| =  | R1 |
C        | c    d    -   f | | temperature, 2nd node|    | R2 |
C                            | temperature, conn. 1 |
C                            | temperature, conn. 2 |
C
C        See Equation 120, Ferguson 2005.
C
C-------------------------------------------------------------------------
C-------------------------------------------------------------------------
C        First coefficient: a
C
C        a = ( eff Q. cap ) / ( TS duration )
C
C              + alpha * ( Cylinder UA coefficient
C                             + Incomming H2 connection heat capacitance )
C
C-------------------------------------------------------------------------
         fCoefficients(1) = fEffective_C / fTS_duration
     &       + fSoln_alpha * ( fMH_cyl_wall_hA ( iComponent )
     &                + fCyl_H2_charge_F * fH2_Conn_Cv_F )

C-------------------------------------------------------------------------
C        Second coefficient: b
C
C        b = alpha * future cooling water flow rate
C                   * future cooling water heat capacity
C
C-------------------------------------------------------------------------
         fCoefficients(2) = 
     &        fSoln_alpha * fH2O_Conn_Flow_F * fH2O_Conn_Cv_F

C-------------------------------------------------------------------------
C        Third coefficient: c
C
C                 /    - heat exchanger UA coefficient     \
C        c = exp | --------------------------------------  | - 1.0
C                \  Cooling water heat capacity flow rate /
C
C
C        Note: if there is no cooling water flowing through the cylinder,
C        set cooling water node temperature to system temperature.
C-------------------------------------------------------------------------
         if ( bNo_H2O_flow ) then
            fCoefficients (3) = -1.0
         else 
            fCoefficients (3) =
     &         exp (
     &              - 1.0 * fMH_Heat_XFer_UA ( iComponent )
     &              / ( fH2O_Conn_Flow_F * fH2O_Conn_Cv_F ) ) - 1.0
         endif

C-------------------------------------------------------------------------
C        Fourth coefficient: d
C         
C        d = 1.0
C
C-------------------------------------------------------------------------
         fCoefficients (4) = 1.0
         
C-------------------------------------------------------------------------
C        fifth coefficent: e
C
C        e = alpha * ( future H2 connection heat capacitance flow rate )
C
C-------------------------------------------------------------------------
         fCoefficients (5) =
     &      -1.0 * fSoln_alpha
     &           * ( fCyl_H2_charge_F * fH2_Conn_Cv_F ) 

C-------------------------------------------------------------------------
C        Sixth coefficient: f
C
C                   /    - heat exchanger UA coefficient     \
C        f = - exp | --------------------------------------  | 
C                  \  Cooling water heat capacity flow rate /
C
C        Note: if there is no cooling water flowing through the cylinder,
C        set cooling water node temperature to system temperature.
C-------------------------------------------------------------------------      
         if ( bNo_H2O_flow ) then
            fCoefficients (6) = 0.0
         else
            fCoefficients (6) =
     &         -1.0 *exp (
     &              - 1.0 * fMH_Heat_XFer_UA ( iComponent )
     &             / ( fH2O_Conn_Flow_F * fH2O_Conn_Cv_F ) )
         endif
         
C-------------------------------------------------------------------------
C        seventh coefficient: R1
C
C        R1 = (eff. Q. Cap ) / ( TS duration )
C
C          + ( 1 - alpha )
C               * (   H2 conn. present internal energy change
C                   + H2 conn present flow work
C                   - H2 discharge present flow work
C                   - present time row heat of absorption
C                   - present time row heat loss
C                   - present time row heat transfer to cooling water
C                  )
C
C          + ( alpha )
C               * (   H2 conn. future time row flow work     (a)
C                   - H2 discharge future time row flow work (a)
C                   - future time row heat of absorption
C                   - ambient heat transfer coefficient
C                       * future time row amb. temperature
C                   + future time row cooling water heat capacitance
C                       * future time row cooling water inlet temp (b).
C
C                  )
C
C        Notes:
C
C        (a) The future time row flow work are entirely functions
C            of the node and connection future temperatures. However,
C            they must be evaluated using absolute temperature values,
C            and since ESP-r's plant matrix temperature state variables
C            are all formatted in celcius, these terms must be included
C            in the present-time & known coefficient.
C 
C        (b) The future time row cooling water inlet temperature does
C            appear future time row cooling water solution vector.
C            However, ESP-r does not allow specification of cross
C            coupling coefficients that multiply the temperatures of
C            connections to other nodes.  Therefore, this future-time-row
C            term must be approximated using values from the previous
C            iteration.
C-------------------------------------------------------------------------
         if ( bH2O_connected ) then
            iCount_a = 7
         else
            iCount_a = 6
         endif 
         fCoefficients (iCount_A) =
     &       fEffective_C / fTS_duration * fCyl_Temp_P
     &
     &     + ( 1.0 - fSoln_alpha )
     &          * (    fH2_Conn_dU_P
     &               + fH2_Conn_FlowWork_P
     &               - fH2_Disc_FlowWork_P
     &               - fQ_absorbtion_J_P
     &               - fQ_heat_loss_P
     &               - fQ_cooling_water_P
     &            )
     & 
     &     +  fSoln_alpha
     &          * (    fH2_Conn_FlowWork_F
     &               - fH2_Disc_FlowWork_F
     &               - fQ_absorbtion_J_F
     &               - fMH_ cyl_wall_hA ( iComponent ) * fAmb_Temp_F
     &               + fH2O_Conn_Cv_F * fH2O_Conn_Flow_F
     &                     * fH2O_Conn_Temp_F
     &             )

C----------------------------------------------------------------------
C        eighth coefficient: R2
C
C        R2 = 0
C
C----------------------------------------------------------------------
         iCount_A = iCount_a + 1
      
         fCoefficients(iCount_A) = 0.0

!          do iCount_A = 1, 8
!             print*, iCount_a, fCoefficients(iCount_a)
!          enddo
         
C----------------------------------------------------------------------
C        Report thermal data
C----------------------------------------------------------------------
         fMH_Report_Containment_TEMP ( iComponent ) = fAmb_Temp_F
         fMH_Report_Q_absorbtion ( iComponent ) = fQ_absorbtion_J_F
         fMH_Report_Charge_FWork ( iComponent ) = fH2_Conn_FlowWork_F
         fMH_Report_Disch_FWork  ( iComponent ) = fH2_Disc_FlowWork_F

   
         

      elseif ( iPlant_Matrix .eq. iPropH2Flow ) then
!          if ( nsinc .ge. 60 ) then
!             print*, 'this is it!', fH2_Conn_Flow_kgs
!          endif
      
C----------------------------------------------------------------------
C        The following code implements a crude mass balance model
C        for a metal hydride storage system. A call to a more
C        sophesticated model, which which does not readily
C        converge at present, is commented-out below.
C
C        The underlying assumptions of the simple model are:
C
C          1. All of the mass in the cylinder is stored
C             in hydride form.
C          2. The concentration of absorbed hydrogen inside
C             the cylinder is equal to the equilibrium
C             concentration under all conditions (ie -
C             mass diffusion effects are negligable).
C          3. There is no practical limit to the rates at
C             which hydrogen can be charged to, or discharged
C             from the cylinder.
C
C        These assumptions introduce the following errors in
C        the model:
C
C          1. The model will tend to underestimate the pressure
C             in the cylinder for any given amount of hydrogen.
C          2. The model will not account for mass-diffusion
C             effects that in practice will limit the rates
C             of charge & discharge.
C
C----------------------------------------------------------------------

C----------------------------------------------------------------------
C        Determine maximum & minumum amount of hydrogen that can
C        be stored in the cylinder.
C
C        Minimum mass = 0.
C
C        Maximum mass = ( mols alloy ) ( MAX concentration )
C
C                                   MAX mols absorbed H2
C        where: MAX concentration = --------------------
C                                        mol alloy
C
C        Remember to convert temperature to Kevlin!
C
C----------------------------------------------------------------------
         fH_absorbed_kmol_MIN =  fMH_kmols_alloy ( iComponent )
     &        * fMH_Eval_Langmuir_C(
     &             iComponent,
     &             fMH_Pressure_MIN(iComponent),
     &             fCyl_Temp_F + fC_to_K )


         fH_absorbed_kmol_MAX =
     &      amin1(fMH_concentration_MAX ( iComponent ),
     &            fMH_Eval_Langmuir_C(
     &                 iComponent,
     &                 fMH_Pressure_MAX(iComponent),
     &                 fCyl_Temp_F + fC_to_K )
     &            )
     &      * fMH_kmols_alloy ( iComponent )

C----------------------------------------------------------------------
C        Collect hydrogen demand from external controller. Note -
C        two types of demand are supported:
C          - hydrogen required by components that are downstream
C            in the plant network, which will detect the incomming
C            hydrogen flow from the upstream connection, and
C          - hydrogen required by components that are not connected
C            to the hydrogen tank in the plant network, or which
C            may not exist in the plant domain. The actual amount
C            of hydrogen delivered to these components will be
C            written to a common-block variable, which is
C            read by the hydrogen demand controller
C
C        The controller describes the hydrogen requirements in kg/s,
C        which must be converted into kmol/s for use in the MH model.
C----------------------------------------------------------------------         

         fCyl_H2_demand_EXP = CDATA ( iComponent, 1 ) / fMM_hydrogen ! (kmol/s)

         fCyl_H2_demand_IMP = CDATA ( iComponent, 2 ) / fMM_hydrogen ! (kmol/s)

C........Total demand
         fCyl_H2_demand_SUM = fCyl_H2_demand_EXP + fCyl_H2_demand_IMP ! (kmol/s)

         


C----------------------------------------------------------------------
C        Estimate rate of change in hydrogen inventory:
C
C        rate of  = ( incomming connection flow - demand )
C        change
C
C----------------------------------------------------------------------
         fCyl_dH2_kmol_s = fH2_Conn_Flow_kgs / fMM_Hydrogen
     &        - fCyl_H2_demand_SUM

C----------------------------------------------------------------------
C        Determine if mols of absorbed hydrogen will exceed cylinder
C        maximum, or fall below minumum on this timestep.
C
C          -> this code presumes that the model's initial conditions
C             fall within the maximum & minimum bounds.
C
C----------------------------------------------------------------------

         if ( fH_absorbed_kmol_P
     &        + 2.0 * fCyl_dH2_kmol_s * fTS_duration
     &        .gt. fH_absorbed_kmol_MAX )then

C----------------------------------------------------------------------
C           Clinder will exceed its upper pressure --- some hydrogen
C           will have to be vented.
C----------------------------------------------------------------------

C...........Set future amount of absorbed hydrogen to MAX

            fH_absorbed_kmol_F = fH_absorbed_kmol_MAX

C...........Calculate mass change in timestep. Remeber,
C...........absorbtion equation is H2 -> 2H

            fCyl_dH2_kmol_s = ( fH_absorbed_kmol_F
     &           - fH_absorbed_kmol_P ) /( 2.0 * fTS_duration )


C...........Set discharge rate to demand

            fCyl_H2_discharge_F = fCyl_H2_demand_SUM

C...........Set charge rate to sum of discharge rate and
C...........rate of change in kmols of absorbed hydrogen

            fCyl_H2_charge_F = fCyl_H2_discharge_F
     &           + fCyl_dH2_kmol_s

C...........Charge rate cannot be less than zero.
            if ( fCyl_H2_charge_F .lt. 0.0 ) fCyl_H2_charge_F = 0.0
           
C...........Calculate vent rate: incomming connection flow
C...........less charge rate

            fCyl_H2_vent = 
     &            + fH2_Conn_Flow_kgs / fMM_Hydrogen
     &            - abs(fCyl_dH2_kmol_s)
     &            - fCyl_H2_discharge_F


         elseif (  fH_absorbed_kmol_P
     &        + 2.0 * fCyl_dH2_kmol_s * fTS_duration
     &        .lt. fH_absorbed_kmol_MIN )then


C----------------------------------------------------------------------
C           Cylinder will reach minumum charge state. The system
C           demand cannot be met.
C----------------------------------------------------------------------

C...........Set charge rate to supply

            fCyl_H2_charge_F =  fH2_Conn_Flow_kgs / fMM_Hydrogen

C...........Set vent rate to zero.

            fCyl_H2_vent = 0.0

C...........Is system charge state above minimum?
            if ( fH_absorbed_kmol_P .gt. fH_absorbed_kmol_MIN ) then

C..............Yes. Set future amount of absorbed hydrogen to MIN

               fH_absorbed_kmol_F = fH_absorbed_kmol_MIN

C..............Calculate mass change in timestep. Remeber,
C..............absorbtion equation is H2 -> 2H

               fCyl_dH2_kmol_s = ( fH_absorbed_kmol_F
     &              - fH_absorbed_kmol_P ) /( 2.0 * fTS_duration )

C..............Set discharge rate to supply + rate of change
C..............stored hydrogen

               fCyl_H2_discharge_F = fCyl_H2_demand_SUM
     &              + fCyl_dH2_kmol_s

            else

C..............No. Set dischage rate to zero.

               fCyl_H2_discharge_F = 0.0

C..............Calculate future amount of absorbed hydrogen
               fH_absorbed_kmol_F = fH_absorbed_kmol_P
     &            + fCyl_H2_charge_F * fTS_duration * 2.0

C..............Calculate mass change in timestep. Remeber,
C..............absorbtion equation is H2 -> 2H

               fCyl_dH2_kmol_s = ( fH_absorbed_kmol_F
     &              - fH_absorbed_kmol_P ) /( 2.0 * fTS_duration )

            endif

         else
C----------------------------------------------------------------------
C           Cylinder will charge / discharge normally.
C----------------------------------------------------------------------

C...........Set charge rate to supply

            fCyl_H2_charge_F =  fH2_Conn_Flow_kgs / fMM_Hydrogen

C...........Set discharge rate to demand

            fCyl_H2_discharge_F = fCyl_H2_demand_SUM

C...........Set vent rate to zero.

            fCyl_H2_vent = 0.0

C...........Calculate future time row mols of absorbed hydrogen

            fH_absorbed_kmol_F = fH_absorbed_kmol_P
     &       + 2.0 * fTS_duration
     &             * ( fCyl_H2_charge_F - fCyl_H2_discharge_F )
                        

         endif

C----------------------------------------------------------------------
C        Calculate concentration of absorbed hydrogen
C----------------------------------------------------------------------
         fConcentration_F = fH_absorbed_kmol_F
     &        / fMH_kmols_alloy ( iComponent ) ! (mol/mol)

C----------------------------------------------------------------------
C        Calculate rate of change in kmols of absorbed hydrogen
C        (needed by thermal model)
C----------------------------------------------------------------------
         fH_absorbed_dNdt_F = ( fH_absorbed_kmol_F
     &         - fH_absorbed_kmol_P ) / fTS_duration 
     
C----------------------------------------------------------------------
C        Calculate pressure at future time row, using subordinate
C        routine fEval_Langmuir_P.
C----------------------------------------------------------------------
         fCyl_Press_F = fMH_Eval_Langmuir_P
     &        ( iComponent, fConcentration_F , fCyl_Temp_F + fC_to_K )

C----------------------------------------------------------------------
C        The following code implements a more sophisticated mass
C        balance model of a metal hydride storage cylinder --- the
C        amounts of absorbed and free hydrogen are modelled using
C        separate differential equations, which are integrated
C        simultaneously. However, the DE's are divergent, and
C        in their present form require *VERY* small timesteps
C        to ensure stable solution.
C
C        The code should be revisited as time permits.
C
C        call MH_Integrate_DEs (
C     &     iComponent,
C     &     fH2_free_kmol_P,
C     &     fH_absorbed_kmol_P,
C     &     fCyl_Temp_P + fC_to_K,
C     &     fCyl_Temp_F + fC_to_K,
C     &     fH2_conn_mass_flow,
C     &     fCyl_H2_demand,
C     &     fTS_duration,
C     &     fH2_free_kmol_F,
C     &     fH_absorbed_kmol_F,
C     &     fCyl_Press_F 
C     &     )
C
C----------------------------------------------------------------------
         
C----------------------------------------------------------------------
C        Calculate total amount of hydrogen delivered through
C        implicitly-modelled connections
C----------------------------------------------------------------------
         if ( fCyl_H2_discharge_F .lt. fCyl_H2_demand_EXP ) then

C...........Explicit demand cannot be met. Set implicit supply to
C...........zero
            fH2_plt_IMP_supply_TOTAL( iComponent ) = 0.0
            
            fH2_plt_EXP_supply_TOTAL( iComponent ) =
     &             fCyl_H2_discharge_F
         
         else

C...........Explicit demand can be met, and at least some of
C...........the implicit demand can be met.
            
            fH2_plt_EXP_supply_TOTAL ( iComponent )
     &         = fCyl_H2_demand_EXP
            
            fH2_plt_IMP_supply_TOTAL (iComponent )
     &         = fCyl_H2_discharge_F
     &          - fH2_plt_EXP_supply_TOTAL ( iComponent )
     
         endif 


C----------------------------------------------------------------------
C        Calculate mass flow matrix coefficients:
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
         if ( bH2O_connected ) fCoefficients(6) =  -1.0

C........RHS vector
         if ( bH2O_connected ) then 
            fCoefficients(7) =  fH2_plt_EXP_supply_TOTAL ( iComponent )
         else
            fCoefficients(6) =  fH2_plt_EXP_supply_TOTAL ( iComponent )
            fCoefficients(7) =  0.0
         endif
         fCoefficients(8) =  0.0
C----------------------------------------------------------------------
C        Save future time row free and absorbed hydrogen
C----------------------------------------------------------------------
         PCDatF ( iComponent, 1) = fH2_free_kmol_F     ! (kmol)
         PCDatF ( iComponent, 2) = fH_absorbed_kmol_F  ! (kmol)
         PCDatF ( iComponent, 3) = fCyl_Press_F        ! (Pa)
         PCDatF ( iComponent, 4) = fH_absorbed_dNdt_F  ! (kmol/s)
         PCDatF ( iComponent, 5) = fCyl_H2_charge_F    ! (kmol/s)
         PCDatF ( iComponent, 6) = fCyl_H2_discharge_F + fCyl_H2_vent
     &                                                 ! (kmol/s)
         PCDatF ( iComponent, 7) = fH2O_Conn_Flow_F    ! (kg/s)
         PCDatF ( iComponent, 8) = fH2O_Conn_Temp_F    ! (oC)
         PCDatF ( iComponent, 9) = fH2_Conn_Temp_F     ! (oC)
                  
C-----------------------------------------------------------------------
C     Save hydrogen flow reporting data
C-----------------------------------------------------------------------
         fMH_Report_H2_free_kmol ( iComponent ) = fH2_free_kmol_F     ! (kmol)
         fMH_Report_H_abs_kmol   ( iComponent ) = fH_absorbed_kmol_F  ! (kmol)
         fMH_Report_H2_vent      ( iComponent ) = fCyl_H2_vent        ! (kmol/s)
         fMH_Report_H2_Discharge ( iComponent ) = fCyl_H2_discharge_F ! (kmol/s)
         fMH_Report_H2_Charge    ( iComponent ) = fCyl_H2_charge_F    ! (kmol/s)
         fMH_Report_Pressure     ( iComponent ) = fCyl_Press_F        ! (Pa)
         fMH_Report_Demand_EXP   ( iComponent ) = fCyl_H2_demand_EXP  ! (kmol/s)
         fMH_Report_Demand_IMP   ( iComponent ) = fCyl_H2_demand_IMP  ! (kmol/s)

      endif

      return
      end

C----------------- MH_Validate_Inputs ---------------------------------
C
C This procedure collects the metal hydride model's inputs from ESP-r's
C ADATA array, and performs necessary error trapping.
C
C Inputs:
C
C   - iComponent:  Index of component in plant network
C
C Outputs:
C
C   - None.
C
C----------------------------------------------------------------------
      subroutine MH_Validate_Inputs ( iComponent )
      implicit none
#include "building.h"
#include "plant.h"
#include "Hydrogen_MH_store.h"

C----------------------------------------------------------------------
C     Passed argument
C----------------------------------------------------------------------
      integer iComponent       ! Index of component in plant net.
      
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu

      character*128 cContext, cMsg
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      logical bClose_To_Zero  ! result of close to zero comparison
      real fSmall             ! tolerance for close to zero
      parameter ( fSmall = 0.000001 )

      logical bFatal_error    ! flag indicating simulation must be halted
      logical bDummy          ! Dummy variable

      integer iWarning
      integer iFatal
      integer iReset 
      
      parameter (iWarning=1, iReset=2, iFatal=3)
C----------------------------------------------------------------------
C     Referneces
C----------------------------------------------------------------------
      logical bValue_GE_Min       ! Function checking if  value >= min
      logical bValue_GT_Min       ! Function checking if  value >  min
      logical bValue_LE_Max       ! Function checking if  value <= max
      logical bValue_LT_Max       ! Function checking if  value <  max

      bFatal_error = .false.
      
C----------------------------------------------------------------------
C     Collect inputs.
C----------------------------------------------------------------------

C.....Cylinder wall mass: 
      fMH_cyl_mass_kg         (iComponent) = AData ( iComponent,  1) ! (kg)
C.....Cylinder wall specific heat
      fMH_cyl_wall_C          (iComponent) = AData ( iComponent,  2) ! (J/kg oC)
C.....Cylinder wall heat loss coefficient
      fMH_cyl_wall_hA         (iComponent) = AData ( iComponent,  3) ! (W/oC)
C.....Cooling water heat transfer coefficient                           
      fMH_Heat_XFer_UA        (iComponent) = AData ( iComponent,  4) ! (W/oC)                                
C.....Cylinder free volume
      fMH_free_volume         (iComponent) = AData ( iComponent,  5) ! (m3)
C.....Maximum operating pressure
      fMH_Pressure_MAX        (iComponent) = AData ( iComponent,  6) ! (atm)
C.....Minimum downstream operaing pressure
      fMH_Pressure_MIN        (iComponent) = AData ( iComponent,  7) ! (atm)
C.....Inital operating pressure
      fMH_Pressure_INIT       (iComponent) = AData ( iComponent,  8) ! (atm
C.....Mols of alloy
      fMH_kmols_alloy         (iComponent) = AData ( iComponent,  9) ! (kmol)
C.....Alloy specific heat
      fMH_alloy_C             (iComponent) = AData ( iComponent, 10) ! (J/kmol oC)
C.....Hydride specific heat
      fMH_hydride_C           (iComponent) = AData ( iComponent, 11) ! (J/kmol oC)

C.....PCT parameters
      fMH_Concentration_TRANS (iComponent) = AData ( iComponent, 12) ! ()
      fMH_Concentration_MAX   (iComponent) = AData ( iComponent, 13) ! ()
      fMH_PCT_param_b1o       (iComponent) = AData ( iComponent, 14) ! ()
      fMH_PCT_param_b2o       (iComponent) = AData ( iComponent, 15) ! ()
      fMH_PCT_param_Do        (iComponent) = AData ( iComponent, 16) ! ()
      fMH_PCT_param_Kc        (iComponent) = AData ( iComponent, 17) ! ()
      fMH_PCT_param_dH1       (iComponent) = AData ( iComponent, 18) ! ()
      fMH_PCT_param_dH2       (iComponent) = AData ( iComponent, 19) ! ()
      fMH_PCT_param_dHs       (iComponent) = AData ( iComponent, 20) ! ()

C----------------------------------------------------------------------
C     Error Trapping
C----------------------------------------------------------------------
      cContext = 'Metal Hydride storage model'
      
C.....Cylinder mass cannot be less than zero
      cMsg = 'Cylinder wall mass is out of range'
      bDummy = bValue_GE_Min (
     &       fMH_cyl_mass_kg ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 1, iReset )
C.....Cylinder wall specific heat cannot be less than zero
      cMsg = 'Cylinder wall specific heat is out of range'
      bDummy = bValue_GE_Min (
     &       fMH_cyl_wall_C ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 2, iReset )
C.....Cylinder wall heat loss coefficint less than zero
      cMsg = 'Cylinder wall heat loss coefficient is out of range'
      bDummy = bValue_GE_Min (
     &       fMH_cyl_wall_hA ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 3, iReset )
C.....Cooling water heat transfer coefficient cannot be less than zero
      cMsg = 'Cooling water heat transfer coefficient is out of range'
      bDummy = bValue_GE_Min (
     &       fMH_Heat_XFer_UA ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 4, iReset )


C.....Cylinder free volume: no error trappin at present 
      cMsg = 'Cylinder free volume is out of range'
      bDummy = bValue_GT_Min (
     &       fMH_free_volume ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 5, iReset )
         
C.....Minimum operating pressure
      cMsg = 'Cylinder minimum operating pressure is out of range'
      bDummy = bValue_GT_Min (
     &       fMH_Pressure_MIN ( iComponent ),
     &       1.0,
     &       cContext,cMsg,
     &       iComponent, 7, iReset )

C.....Maximum operating pressure
      cMsg = 'Cylinder maximum operating pressure is out of range'
      if ( .not. bValue_GT_Min (
     &       fMH_Pressure_MAX ( iComponent ),
     &       fMH_Pressure_MIN ( iComponent ),
     &       cContext,cMsg,
     &       iComponent, 6, iFatal ) ) then
       
         bFatal_error = .true.
         
      endif 
         
C.....Initial operating pressure
      cMsg = 'Cylinder initial pressure is out of range'
      bDummy = bValue_GT_Min (
     &       fMH_Pressure_INIT ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 8, iReset )
      bDummy = bValue_LE_Max (
     &       fMH_Pressure_INIT ( iComponent ),
     &       fMH_Pressure_MAX ( iComponent),
     &       cContext,cMsg,
     &       iComponent, 8, iReset )
     
C.....kmols of alloy
      cMsg = 'amount (kmols) of alloy is out of range'
      bDummy = bValue_GT_Min (
     &       fMH_kmols_alloy (iComponent),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 9, iReset )

C.....Alloy specific heat cannot be less than zero
      cMsg = 'Alloy specific heat is out of range'
      bDummy = bValue_GE_Min (
     &       fMH_alloy_C ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 10, iReset )
     
C.....Hydride specific heat cannot be less than zero
      cMsg = 'Hydride specific heat is out of range'
      bDummy = bValue_GE_Min (
     &       fMH_hydride_C ( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 11, iReset )

C.....Max concentration must be between 0 & 1
      cMsg = 'Maximum concentration is out of range'
      bDummy = bValue_GT_Min (
     &       fMH_concentration_MAX( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 13, iReset )
      bDummy = bValue_LE_Max (
     &       fMH_Concentration_MAX ( iComponent ),
     &       1.0,
     &       cContext,cMsg,
     &       iComponent, 13, iReset )

C.....Transitional concentration must be between 0 & max
      cMsg = 'Transitional concentration is out of range'
      bDummy = bValue_GT_Min (
     &       fMH_concentration_TRANS( iComponent ),
     &       0.0,
     &       cContext,cMsg,
     &       iComponent, 12, iReset )
      if ( .not. bValue_LT_Max (
     &       fMH_Concentration_TRANS ( iComponent ),
     &       fMH_Concentration_MAX (iComponent),
     &       cContext,cMsg,
     &       iComponent, 12, iFatal ) ) then

         bFatal_error = .true.

      endif


C-------------------------------------------------------------------------
C     Has a fatal error occured?
C-------------------------------------------------------------------------      
      if ( bFatal_error ) then
         write (itu,*) 'Metal Hydride storage model: Fatal error!'
         stop
      endif 
C-------------------------------------------------------------------------
C     Convert pressures to Pa
C-------------------------------------------------------------------------
      fMH_Pressure_INIT (iComponent) = fMH_Pressure_INIT (iComponent)
     &                                            * 101.32E03        ! (Pa)
      fMH_Pressure_MAX (iComponent) = fMH_Pressure_MAX (iComponent)
     &                                            * 101.32E03        ! (Pa)
      fMH_Pressure_MIN (iComponent) = fMH_Pressure_MIN (iComponent)
     &                                            * 101.32E03        ! (Pa)
     
      return
      end

C----------------- MH_Eval_initial_conditions ----------------------------
C
C This procedure returns the number of free and absorbed mols of
C hydrogen for a given cylinder operating temperature and pressure.
C The free gas is characterized using the ideal gas law, while the
C absorbed gas is evaluated using the relevant PCT model.
C
C Inputs:
C
C   - iComponent: Component index in plant network
C   - fPressure: Cylinder pressure (Pa)
C   - fTemperature: Cylinder temperature (K)
C
C Outputs:
C   - fH2_free_kmols: mols of free hydrogen (H2, kmol)
C   - fH_absorbed_kmols: mols of absorbed hydrogen (H, kmol)
C
C-------------------------------------------------------------------------
      subroutine MH_Eval_initial_conditions(
     &     iComponent,
     &     fPressure,
     &     fTemperature,
     &     fH2_free_kmols,
     &     fH_absorbed_kmols )
      implicit none

#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"


C-----------------------------------------------------------------------
C     Passed arguements
C-----------------------------------------------------------------------
C.....Inputs
      integer iComponent        ! pointer to component in plant net.
      real fPressure            ! pressure inside cylinder (Pa)
      real fTemperature         ! temperature inside cylinder (K)
C.....Outputs
      real fH2_free_kmols       ! mols of free hydrogen gas (kmol, H2)
      real fH_absorbed_kmols    ! mols of absorbed hydrogen (kmol, H)

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      real fConcentration       ! Concentration of hydride (mol/mol)

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fMH_Eval_Langmuir_C  ! function returning concentration of
                                !   absorbed hydrogen at a given
                                !   temperature and pressure

C-----------------------------------------------------------------------
C     Determine the number of mols of free gas using the ideal gas
C     law:
C                ( pressure ) ( free volume )
C     mols = ------------------------------------
C            (univ. gas constant ) ( temperature )
C
C     See eqn 88, Ferguson 2005. Note: fR_universal is defined
C     in terms of (J/kmol K)
C-----------------------------------------------------------------------

      fH2_free_kmols = fPressure * fMH_free_volume (iComponent )
     &     / fR_universal / fTemperature

C-----------------------------------------------------------------------
C     Evaluate concentration [C=f(P,T)] using Langmuir PCT model. Note:
C     other PCT models could be substituted here!
C-----------------------------------------------------------------------
      fConcentration = fMH_Eval_Langmuir_C(
     &     iComponent,
     &     fPressure,
     &     fTemperature )

C-----------------------------------------------------------------------
C     Determine the number of hydrogen mols corresponding to the
C     calculated concentration:
C
C     absorbed mols = ( concentration ) ( total mols of alloy )
C
C-----------------------------------------------------------------------
      fH_absorbed_kmols = fConcentration * fMH_kmols_alloy (iComponent)


      return
      end


C----------------- MH_Integrate_DEs ------------------------------------
C
C This procedure integrates the differential equations governing the
C absorbtion of hydrogen and pressure inside the cylinder. It
C
C-----------------------------------------------------------------------
      subroutine MH_Integrate_DEs (
     &     iComponent,
     &     fH2_free_kmol_P,
     &     fH_absorbed_kmol_P,
     &     fTemperature_P,
     &     fTemperature_F,
     &     fH2_conn_mass_flow,
     &     fH2_demand,
     &     fTS_duration,
     &     fH2_free_kmol_F,
     &     fH_absorbed_kmol_F,
     &     fPressure_F
     &     )
      implicit none

#include "plant.h"
#include "Hydrogen_MH_store.h"

C-----------------------------------------------------------------------
C     Passed arguements
C-----------------------------------------------------------------------

C.....Inputs
      integer iComponent        ! pointer to component in plant network

      real fH2_free_kmol_P      ! mols of hydrogen (H2) stored in a gaseous
                                !     state on present time row (kmol)
      real fH_absorbed_kmol_P   ! mols of hydrogen absorbed in the
                                !     alloy on present time row (kmol)

      real fTemperature_P       ! Cylinder temperature on present 
                                !     time row (K)
      real fTemperature_F       ! Cylinder temperature on future 
                                !     time row (K)

      real fH2_Conn_Mass_Flow   ! Rate at which hydrogen enters
                                !     cylinder (kmol/s)
      real fH2_Demand           ! Hydrogen flow rate demanded from
                                !     cylinder (kmol/s)

      real fTS_duration         ! Timestep duration (s)

C.....Outputs
      real fH2_free_kmol_F      ! mols of hydrogen (H2) stored in a gaseous
                                !     state on future time row (kmol)
      real fH_absorbed_kmol_F   ! mols of hydrogen absorbed in the
                                !     alloy on future time row (kmol)

      real fPressure_F          ! pressure in cylinder on future time row (Pa)



C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------

C.....Subsolution parameters

      integer iSubSol_TS        ! sub-simulation timestep counter

      integer iNum_SubSol_TS    ! number of sub-simulation timesteps. 

      real fSubSol_TS           ! sub-simulation time-step duration (s)

      real fTemp_dTdt           ! rate of change in tank temperature (K/s)

      real fPressure_P          ! present time row pressure (Pa)

      logical bPress_HiLo       ! flag indicating that pressure is outside
                                !     limits. 


C.....Results from DEs
      real fH2_free_dNdt        ! Rate of change in the number of free
                                !     mols of hydrogen (H2) in the
                                !     cylinder (kmol/s)
      real fH_absorbed_dNdt     ! Rate of change in the number of
                                !     absorbed mols of hydrogen (H) in
                                !     the cylinder (kmol/s)
      real fPressure_dPdt       ! Rate of change in the cylinder
                                !     pressure (Pa/s)
      real fConc_dCdt           ! Rate of change in concentration 
                                !     (mol/mol s)

      real fConc_dCdt_P         ! Rate of change in concentration at
                                !     start of timestep (mol/mol s)

      real fPressure_dPdt_P     ! Dimensionless Rate of change in 
                                !     the cylinder pressure (1/s)

      real fH2_free_dNdt_P      ! Dimensionless rate of change in the 
                                !     mols of free hydrogen (H2) in the
                                !     cylinder (1/s) 

      real fH_absorbed_dNdt_P   ! Maximum rate of change in the 
                                !     mols of free hydrogen (H2) in the
                                !     cylinder (1/s) 

      real fH2_free_dNdt_MAX    ! Maximum rate of change in the 
                                !     mols of free hydrogen (H2) in the
                                !     cylinder (1/s)
      
      real fH_abs_dNdt_MAX      ! Maximum  rate of change in the 
                                !     mols of free hydrogen (H2) in the
                                !     cylinder (1/s)
      real fPressure_dPdt_MAX   ! Maximum Rate of change in 
                                !     the cylinder pressure (Pa/s)

      real fRateChange_P        ! Maximum dimensionless rate of 
                                !     change at start of timestep

      real fRateChange_MAX      ! Maximum dimensionless rate of 
                                !     encountered in sub-simulation loop

C.....Parameters used to characterize dimensionless rates of change
      real fPressure_Char        ! Pressure at undercharge-> plateau
                                 !    transition (Pa)
      real fH2_free_kmol_char    ! Free hydrogen at undercharge-> plateau
                                 !    transition (kmol)
      real fH_absorbed_kmol_char ! Absorbed hydrogen at undercharge
                                 !    -> plateau transition (kmol)
 

C.....Runge Kutta working variables  
      real fRK_temperature
      real fRK_H2_free_kmol
      real fRK_H_abs_kmol 

C.....Runge-Kutta integration intermediate results
      real fRK_H2_free_dN_1   
      real fRK_H2_free_dN_2
      real fRK_H2_free_dN_3 
      real fRK_H2_free_dN_4
      real fRK_H2_free_dN_avg
      real fRK_H2_free_dN_next

      real fRK_H_abs_dN_1  
      real fRK_H_abs_dN_2  
      real fRK_H_abs_dN_3  
      real fRK_H_abs_dN_4  
      real fRK_H_abs_dN_avg
      real fRK_H_abs_dN_next

C.....Rates of change calculated by runge kutta integration
      real fRK_H2_free_dNdt_1
      real fRK_H2_free_dNdt_2
      real fRK_H2_free_dNdt_3
      real fRK_H2_free_dNdt_4
      real fRK_H2_free_dNdt_avg

      real fRK_H_abs_dNdt_1
      real fRK_H_abs_dNdt_2
      real fRK_H_abs_dNdt_3
      real fRK_H_abs_dNdt_4
      real fRK_H_abs_dNdt_avg

      real fRK_Press_dPdt_1
      real fRK_Press_dPdt_2
      real fRK_Press_dPdt_3
      real fRK_Press_dPdt_4
      real fRK_Press_dPdt_avg

C.....Misc flags.
      logical bNums_are_close   ! flag for close to zero comparisons

      logical bTS_too_big       ! flag indicating subsolution timestep
                                !     is too big. 

      integer iIter_count

      logical bSol_Diverged     ! Flag indicating that runge-kutta 
                                !     integration is diverging.

C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      real fMaxChange           ! max change in state variables per 
                                !    sub-simulation timestep (1/s)
      parameter (fMaxChange = 0.1)


C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fMH_Eval_RateChange  ! Function determining max rate of 
                                !     change in state variables.

C-----------------------------------------------------------------------
C     Calculate linearized rate of change in tank temperature
C
C 
C       dT    Future temp - present temp
C       --  = ---------------------------
C       dt         TS duration 
C
C-----------------------------------------------------------------------
      fTemp_dTdt = ( fTemperature_F - fTemperature_P ) / fTS_duration

C-----------------------------------------------------------------------
C     Determine free & absorbed hydrogen and 'switch' pressure at
C     transition from undercharged to plateau stages, using subordinate
C     function MH_Eval_Langmuir_TRANS. These parameters will be 
C     used to characterize the dimensionless rates of change in 
C     key state variables in the sub-simulation loop. Note: use
C     average of present and future temperatures to determine 
C     conditions at 'switch' point.
C-----------------------------------------------------------------------
      call MH_Eval_Langmuir_TRANS (
     &     iComponent,
     &     ( fTemperature_P + fTemperature_F) / 2.0,
     &     fPressure_char,
     &     fH2_free_kmol_char,
     &     fH_absorbed_kmol_char )

C-----------------------------------------------------------------------
C     Determine the rate of change in concentration at start of 
C     simulation. 
C-----------------------------------------------------------------------
      
      call MH_Eval_PCT_DEs (
     &      iComponent,
     &      fH2_free_kmol_P,
     &      fH_absorbed_kmol_P,
     &      fH2_Conn_Mass_Flow,
     &      fH2_Demand,
     &      fTemperature_P,
     &      fTemp_dTdt,
     &      fTS_duration,
     &      fPressure_P,
     &      fH2_free_dNdt,
     &      fH_absorbed_dNdt,
     &      fConc_dCdt,
     &      fPressure_dPdt )

C-----------------------------------------------------------------------
C        Determine dimensionless rates of change in absorbed & free
C        hydrogen, and pressure. 
C-----------------------------------------------------------------------
         fH2_free_dNdt_P    = abs(fH2_free_dNdt)    / fH2_free_kmol_char
         fH_absorbed_dNdt_P = abs(fH_absorbed_dNdt)
     &        / fH_absorbed_kmol_char
         fPressure_dPdt_P   = abs(fPressure_dPdt)   / fPressure_char
         fRateChange_P = amax1
     &        (fH2_free_dNdt_P,fH_absorbed_dNdt_P)



C------------------------------------------------------------------------
C     Start sub subsimulation timestep iteration loop:
C
C       -> guesstimate the sub-simulation timestep required to produce
C          a maximum 5% change in concentration.
C       -> Simulate storage tank over simulation timestep
C       -> compare maximum change in concentration with 5% limit, and 
C          repeat if necessary
C------------------------------------------------------------------------

      bTS_too_big = .true. 
      iIter_Count = 0
      do while ( bTS_too_big .and. iIter_count .lt.10  )

         iIter_count = iIter_count + 1
C-----------------------------------------------------------------------
C        Estimate the subsolution timestep that will produce a 5% change 
C        in the concentration of absorbed hydrogen.
C-----------------------------------------------------------------------

C........Is change in concentration close to zero?
         call eclose (  fRateChange_P,0.0, 1.0E-09, bNums_are_close )

         if ( bNums_are_close ) then

C...........Concentration will not change on this timestep. Even so, 
C...........solve state equation set at least once. 

            fSubSol_TS = fTS_duration
            iNum_SubSol_TS = 1
            
         else 

C-----------------------------------------------------------------------
C           Determine appropriate resolution
C
C                                 Max permitted change / sub-soln TS
C           soln. resolution  = ---------------------------------------
C                               MAX rate of change @ start of time-step
C
C-----------------------------------------------------------------------

            fSubSol_TS = fMaxChange / abs ( fRateChange_P )

C...........Calculate # of timesteps required.
            iNum_SubSol_TS = int ( fTS_duration / fSubSol_TS ) + 1

         end if 

C........Recalculate time resolution to ensure that future time-row results
C........coincide with sub-simulation results.

         fSubSol_TS =  fTS_duration / real ( iNum_SubSol_TS )



C........Initialize max change variable
         fRateChange_MAX = 0.0

C........Runge-kutta working variables
         fRK_H2_free_kmol = fH2_free_kmol_P
         fRK_H_abs_kmol   = fH_absorbed_kmol_P

C-----------------------------------------------------------------------
C        Begin sub-simulation loop.
C-----------------------------------------------------------------------
         do iSubSol_TS = 1, iNum_SubSol_TS 
      
C------------------------------------------------------------------------
C           Determine temperature @ sub-simulaton timestep: 
C           (assume linear changes in temperature)
C------------------------------------------------------------------------  
            fRK_Temperature = fTemperature_P
     &           + float( iSubSol_TS - 1 ) / iNum_SubSol_TS * fTemp_dTdt  

C------------------------------------------------------------------------
C           Initiate Runge-Kutta solution. For each sub-simulation 
C           time-step (i) :
C
C                                         | dY |
C             k1 = (time step duration) * | -- |
C                                         | dt | (Y = Yi)
C     
C                                         | dY |
C             k2 = (time step duration) * | -- |
C                                         | dt | (Y = Yi + k1/2)
C     
C                                         | dY |
C             k3 = (time step duration) * | -- |
C                                         | dt | (Y = Yi + k2/2)
C     
C                                         | dY |
C             k4 = (time step duration) * | -- |
C                                         | dt | (Y = Yi + k3)
C     
C                         k1   k2   k3   k4
C             Yi+1 = Yi + -- + -- + -- + --
C                         6.   3.   3.   6.
C     
C           Where Y = # of free mols of hydrogen, number of absorbed
C                     mols of hydrogen
C     
C           The value of dY/dt is calculated using subroutine
C           MH_Eval_PCT_DEs, which returns the rate of change in 
C           the nunber of free and absorbed mols of hydrogen,
C     
C------------------------------------------------------------------------

C------------------------------------------------------------------------
C           Evaluate differential equations at sub-simulation time step i
C------------------------------------------------------------------------
            call MH_Eval_PCT_DEs (
     &           iComponent,
     &           fRK_H2_free_kmol,
     &           fRK_H_abs_kmol,
     &           fH2_Conn_Mass_Flow,
     &           fH2_Demand,
     &           fRK_Temperature,
     &           fTemp_dTdt,
     &           fSubSol_TS,
     &           fPressure_P,
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fConc_dCdt,
     &           fPressure_dPdt )

           
C-----------------------------------------------------------------------
C           Integrate results over time slice. Note: there's no need
C           to integrate the amounts of free and absorbed hydrogen,
C           and pressure ---  we're only interested in their's rate 
C           of change at present.
C-----------------------------------------------------------------------
            fRK_H2_free_dN_1 = fH2_free_dNdt    * fSubSol_TS   ! (kmols)
            fRK_H_abs_dN_1   = fH_absorbed_dNdt * fSubSol_TS   ! (kmols)

            fRK_H2_free_dNdT_1 = fH2_free_dNdt    ! (kmol/s)
            fRK_H_abs_dNdt_1   = fH_absorbed_dNdt ! (kmol/s)
            fRK_Press_dPdt_1   = fPressure_dPdt   ! (Pa/s)

            fRateChange_MAX = fMH_Eval_RateChange(
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fPressure_dPdt,
     &           fH2_free_kmol_char,
     &           fH_absorbed_kmol_char,
     &           fPressure_char,
     &           fRateChange_MAX )  



C-----------------------------------------------------------------------
C           Set rates of change for next 1/2 TS approximation. If
C           pressure is outside cylinder operating bounds, set 
C           rate of change in free hydrogen to zero. This will 
C           encourage convergence. 
C-----------------------------------------------------------------------
            fRK_H_abs_dN_next = fRK_H_abs_dN_1 
            fRK_H2_free_dN_next = fRK_H2_free_dN_1


C------------------------------------------------------------------------
C           Evaluate differential equations at sub-simulation time step 
C           i + 1/2 
C------------------------------------------------------------------------
            call MH_Eval_PCT_DEs (
     &           iComponent,
     &           fRK_H2_free_kmol +  fRK_H2_free_dN_next / 2.0 ,
     &           fRK_H_abs_kmol   +  fRK_H_abs_dN_next   / 2.0 ,
     &           fH2_Conn_Mass_Flow,
     &           fH2_Demand,
     &           fRK_Temperature + fTemp_dTdt * fSubSol_TS / 2.0,
     &           fTemp_dTdt,
     &           fSubSol_TS,
     &           fPressure_P,
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fConc_dCdt,
     &           fPressure_dPdt )
            


C-----------------------------------------------------------------------
C           Integrate results over time slice. Note: there's no need
C           to integrate the concentration of absorbed hydrogen ---
C           we're only interested in it's rate of change at present.
C-----------------------------------------------------------------------
            fRK_H2_free_dN_2 = fH2_free_dNdt    * fSubSol_TS   ! (kmols)
            fRK_H_abs_dN_2   = fH_absorbed_dNdt * fSubSol_TS   ! (kmols)

            fRK_H2_free_dNdT_2 = fH2_free_dNdt    ! (kmol/s)
            fRK_H_abs_dNdt_2   = fH_absorbed_dNdt ! (kmol/s)
            fRK_Press_dPdt_2   = fPressure_dPdt   ! (Pa/s)

            fRateChange_MAX = fMH_Eval_RateChange(
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fPressure_dPdt,
     &           fH2_free_kmol_char,
     &           fH_absorbed_kmol_char,
     &           fPressure_char,
     &           fRateChange_MAX )  

C-----------------------------------------------------------------------
C           Set rates of change for next 1/2 TS approximation. If
C           pressure is outside cylinder operating bounds, set 
C           rate of change in free hydrogen to zero. This will 
C           encourage convergence. 
C-----------------------------------------------------------------------
            fRK_H_abs_dN_next = fRK_H_abs_dN_2
            fRK_H2_free_dN_next = fRK_H2_free_dN_2

c$$$            if ( bPress_HiLo ) then 
c$$$               fRK_H2_free_dN_next = 0.0
c$$$            else
c$$$               fRK_H2_free_dN_next = fRK_H2_free_dN_2
c$$$            endif



C------------------------------------------------------------------------
C           Evaluate differential equations at sub-simulation time step 
C           i + 1/2 
C------------------------------------------------------------------------
            call MH_Eval_PCT_DEs (
     &           iComponent,
     &           fRK_H2_free_kmol +  fRK_H2_free_dN_next / 2.0 ,
     &           fRK_H_abs_kmol   +  fRK_H_abs_dN_next   / 2.0 ,
     &           fH2_Conn_Mass_Flow,
     &           fH2_Demand,
     &           fRK_Temperature + fTemp_dTdt * fSubSol_TS / 2.0,
     &           fTemp_dTdt,
     &           fSubSol_TS,
     &           fPressure_P,
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fConc_dCdt,
     &           fPressure_dPdt )
            
C-----------------------------------------------------------------------
C           Integrate results over time slice. Note: there's no need
C           to integrate the concentration of absorbed hydrogen ---
C           we're only interested in it's rate of change at present.
C-----------------------------------------------------------------------
            fRK_H2_free_dN_3 = fH2_free_dNdt    * fSubSol_TS   ! (kmols)
            fRK_H_abs_dN_3   = fH_absorbed_dNdt * fSubSol_TS   ! (kmols)

            fRK_H2_free_dNdT_3 = fH2_free_dNdt    ! (kmol/s)
            fRK_H_abs_dNdt_3   = fH_absorbed_dNdt ! (kmol/s)
            fRK_Press_dPdt_3   = fPressure_dPdt   ! (Pa/s)

            fRateChange_MAX = fMH_Eval_RateChange(
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fPressure_dPdt,
     &           fH2_free_kmol_char,
     &           fH_absorbed_kmol_char,
     &           fPressure_char,
     &           fRateChange_MAX )  

C-----------------------------------------------------------------------
C           Set rates of change for next 1/2 TS approximation. If
C           pressure is outside cylinder operating bounds, set 
C           rate of change in free hydrogen to zero. This will 
C           encourage convergence. 
C-----------------------------------------------------------------------
            fRK_H_abs_dN_next = fRK_H_abs_dN_3
            fRK_H2_free_dN_next = fRK_H2_free_dN_3

c$$$            if ( bPress_HiLo ) then 
c$$$               fRK_H2_free_dN_next = 0.0
c$$$            else
c$$$               fRK_H2_free_dN_next = fRK_H2_free_dN_3
c$$$            endif


C------------------------------------------------------------------------
C           Evaluate differential equations at sub-simulation time step 
C           i + 1
C------------------------------------------------------------------------
            call MH_Eval_PCT_DEs (
     &           iComponent,
     &           fRK_H2_free_kmol +  fRK_H2_free_dN_next ,
     &           fRK_H_abs_kmol   +  fRK_H_abs_dN_next   ,
     &           fH2_Conn_Mass_Flow,
     &           fH2_Demand,
     &           fRK_Temperature + fTemp_dTdt * fSubSol_TS, 
     &           fTemp_dTdt,
     &           fSubSol_TS,
     &           fPressure_P,
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fConc_dCdt,
     &           fPressure_dPdt )
            
C-----------------------------------------------------------------------
C           Integrate results over time slice. Note: there's no need
C           to integrate the concentration of absorbed hydrogen ---
C           we're only interested in its rate of change at present.
C-----------------------------------------------------------------------
            fRK_H2_free_dN_4 = fH2_free_dNdt    * fSubSol_TS   ! (kmols)
            fRK_H_abs_dN_4   = fH_absorbed_dNdt * fSubSol_TS   ! (kmols)

            fRK_H2_free_dNdT_4 = fH2_free_dNdt    ! (kmol/s)
            fRK_H_abs_dNdt_4   = fH_absorbed_dNdt ! (kmol/s)
            fRK_Press_dPdt_4   = fPressure_dPdt   ! (Pa/s)

            fRateChange_MAX = fMH_Eval_RateChange(
     &           fH2_free_dNdt,
     &           fH_absorbed_dNdt,
     &           fPressure_dPdt,
     &           fH2_free_kmol_char,
     &           fH_absorbed_kmol_char,
     &           fPressure_char,
     &           fRateChange_MAX )                                             

C------------------------------------------------------------------------
C           Average results over time-slice. Note: segment midpoints 
C           2 & 3 have twice the weight as end points 1 & 4
C------------------------------------------------------------------------
            fRK_H2_free_dN_avg = 
     &           fRK_H2_free_dN_1 / 6.0 
     &         + fRK_H2_free_dN_2 / 3.0
     &         + fRK_H2_free_dN_3 / 3.0 
     &         + fRK_H2_free_dN_4 / 6.0

            fRK_H_abs_dN_avg = 
     &           fRK_H_abs_dN_1 / 6.0 
     &         + fRK_H_abs_dN_2 / 3.0
     &         + fRK_H_abs_dN_3 / 3.0 
     &         + fRK_H_abs_dN_4 / 6.0

            fRK_H2_free_dNdt_avg = 
     &           fRK_H2_free_dNdt_1 / 6.0 
     &         + fRK_H2_free_dNdt_2 / 3.0
     &         + fRK_H2_free_dNdt_3 / 3.0 
     &         + fRK_H2_free_dNdt_4 / 6.0

            fRK_H_abs_dNdt_avg = 
     &           fRK_H_abs_dNdt_1 / 6.0 
     &         + fRK_H_abs_dNdt_2 / 3.0
     &         + fRK_H_abs_dNdt_3 / 3.0 
     &         + fRK_H_abs_dNdt_4 / 6.0

            fRK_Press_dPdt_avg = 
     &           fRK_Press_dPdt_1 / 6.0 
     &         + fRK_Press_dPdt_2 / 3.0
     &         + fRK_Press_dPdt_3 / 3.0 
     &         + fRK_Press_dPdt_4 / 6.0

C------------------------------------------------------------------------
C           Calculate cumulative changes in free & absorbed hydrogen
C------------------------------------------------------------------------
            fRK_H2_free_kmol = fRK_H2_free_kmol + fRK_H2_free_dN_avg ! (kmol)
            fRK_H_abs_kmol   = fRK_H_abs_kmol   + fRK_H_abs_dN_avg   ! (kmol)

         enddo 

C----------------------------------------------------------------------- 
C           Evaluate dimensionless rates of change and compare to 
C           current maximum. Update maximum if necessary
C-----------------------------------------------------------------------
c$$$            fRateChange_MAX = fMH_Eval_RateChange(
c$$$     &           fRK_H2_free_dNdt_avg,
c$$$     &           fRK_H_abs_dNdt_avg,
c$$$     &           fRK_Press_dPdt_avg,
c$$$     &           fH2_free_kmol_char,
c$$$     &           fH_absorbed_kmol_char,
c$$$     &           fPressure_char,
c$$$     &           fRateChange_MAX )



C-----------------------------------------------------------------------
C        Check and see if the differential equations reported a
C        rate of change in concentration larger than that used 
C        to calculate the subsolution timestep.
C-----------------------------------------------------------------------

         if ( fRateChange_MAX * fSubSol_TS .gt. fMaxChange ) then

C...........Greater time resolution is required. 
            fRateChange_P = fRateChange_MAX * 1.5

         else 

C...........Time resolution is adequate. Kill subsimulation loop 
            bTS_too_big = .false.

         endif 

      enddo 

C-----------------------------------------------------------------------
C     Update future time-row variables
C-----------------------------------------------------------------------
      fH2_free_kmol_F    = fRK_H2_free_kmol
      fH_absorbed_kmol_F = fRK_H_abs_kmol 

C-----------------------------------------------------------------------
C     Call function MH_Eval_PCT_DEs one more time to characterize 
C     pressure at future time row 
C-----------------------------------------------------------------------

      call MH_Eval_PCT_DEs (
     &      iComponent,
     &      fH2_free_kmol_F,
     &      fH_absorbed_kmol_F,
     &      fH2_Conn_Mass_Flow,
     &      fH2_Demand,
     &      fTemperature_P + fTemp_dTdt * fTS_duration, 
     &      fTemp_dTdt,
     &      fSubSol_TS,
     &      fPressure_F,
     &      fH2_free_dNdt,
     &      fH_absorbed_dNdt,
     &      fConc_dCdt_P,
     &      fPressure_dPdt )

      return
      end 

C----------------- MH_Eval_PCT_DEs -------------------------------------
C
C This procedure estimates the rates of change of:
C
C     i) free hydrogen,
C    ii) absorbed hydrogen,
C   iii) pressure
C
C inside a metal-hydride storage cylinder. The subordanant procedure
C fMH_Eval_Langmuir_dCdT is used to determine the effect of pressure
C and temperature on the concentration of absorbed hydrogen.  The
C ideal gas law and conservation of mass principles are then used to
C determine the change in the amount of free hydrogen and the pressure
C inside the cylinder.
C
C Inputs:
C
C  - iComponent: pointer to component in plant network
C
C  - fH2_free_kmol: mols of hydrogen (H2) stored in a gaseous state (kmol)
C
C  - fH_absorbed_kmol: mols of hydrogen absorbed in the alloy (kmol)
C
C  - fH2_Conn_Mass_Flow: Rate at which hydrogen enters cylinder (kmol/s)
C
C  - fH2_Demand: Hydrogen flow rate demanded from cylinder (kmol/s)
C
C  - fTemperature: Temperature inside cylinder (K)
C
C  - fTemp_dTdt: rate of change in cylinder temperature, as calculated by
C         the thermal model (K/s)
C
C Outputs:
C
C  - fPressure: Cylinder pressure (Pa)
C
C  - fH2_free_dNdt: Rate of change in the number of free mols of
C         hydrogen (H2) in the cylinder (kmol/s)
C
C  - fH_absorbed_dNdt: Rate of change in the number of absorbed mols of
C         hydrogen (H) in the cylinder (kmol/s)
C
C  - fConc_dCdt: Rate of change in the concentration in the cylinder 
C         ( mol / mol s)
C
C  - fPressure_dPdt: Rate of change in the cylinder pressure (Pa/s)
C
C-------------------------------------------------------------------------
      subroutine MH_Eval_PCT_DEs (
     &      iComponent,
     &      fH2_free_kmol,
     &      fH_absorbed_kmol,
     &      fH2_Conn_Mass_Flow,
     &      fH2_Demand,
     &      fTemperature,
     &      fTemp_dTdt,
     &      fTS_duration,
     &      fPressure,
     &      fH2_free_dNdt,
     &      fH_absorbed_dNdt,
     &      fConc_dCdt,
     &      fPressure_dPdt      )

      implicit none

#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"
C-------------------------------------------------------------------------
C     Passed arguements
C-------------------------------------------------------------------------
C.....inputs
      integer iComponent        ! pointer to component in plant network
      real  fH2_free_kmol       ! mols of hydrogen (H2) stored in a gaseous
                                !     state (kmol)
      real fH_absorbed_kmol     ! mols of hydrogen absorbed in the
                                !     alloy (kmol)
      real fH2_Conn_Mass_Flow   ! Rate at which hydrogen enters
                                !     cylinder (kmol/s)
      real fH2_Demand           ! Hydrogen flow rate demanded from
                                !     cylinder (kmol/s)
      real fTemperature         ! Temperature inside cylinder (K)
      real fTemp_dTdt           ! Rate of change in cylinder temperature,
                                !     as calculated by the thermal model (K/s)

      real fTS_duration         ! timestep duration

C.....Outputs
      real fPressure            ! Pressure inside cylinder (Pa)

      real fH2_free_dNdt        ! Rate of change in the number of free
                                !     mols of hydrogen (H2) in the
                                !     cylinder (kmol/s)
      real fH_absorbed_dNdt     ! Rate of change in the number of
                                !     absorbed mols of hydrogen (H) in
                                !     the cylinder (kmol/s)
      real fConc_dCdt           ! Rate of change in concentration 
                                !     (mol/mol s)
      real fPressure_dPdt       ! Rate of change in the cylinder
                                !     pressure (Pa/s)

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fConcentration       ! Concentration of absorbed hydrogen
                                !     ( mol H / mol alloy )
      real fConc_Equilibrium    ! Concentration of absorbed hydrogen 
                                !     under equilibrium conditions
                                !     ( mol H / mol alloy ) 
      real fH2_charge_rate      ! Rate at which hydrogen is admitted
                                !     into the tank (kmol/s)
      real fH2_discharge_rate   ! Rate at which hydrogen is delivered
                                !     by the tank (kmol/s)


C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real fMH_Eval_Langmuir_C  ! Function requrning the equilibrium
                                !     concentration of absorbed
                                !     hydrogen

C----------------------------------------------------------------------
C     Calculate pressure inside the cylinder using ideal gas law.
C
C             (mols of free hydrogen) * (univ. gas constant) * (temp.)
C        P =  --------------------------------------------------------
C                             ( free volume )
C
C     See Equation 88, Ferguson 2005. Note: chemical_properties.h
C     defines the universal gas constant, R, in J/kmol K.
C----------------------------------------------------------------------
      fPressure = fH2_free_kmol * (fR_universal) * fTemperature
     &            / fMH_free_volume ( iComponent )   ! (Pa)

C----------------------------------------------------------------------
C     Check that pressure is within maximum / minimum bounds.
C
C       -> should this code allow hydrogen to 'flow through' tank
C          when there is coincident supply/demand?
C
C----------------------------------------------------------------------
      if ( fPressure .lt. fMH_pressure_MIN ( iComponent ) ) then
C........Tank cannot deliver any hydrogen
         fH2_discharge_rate = 0.              ! (kmol/s)
      else
         fH2_discharge_rate = fH2_Demand      ! (kmol/s)
      endif

      if ( fPressure .gt. fMH_pressure_MAX ( iComponent ) ) then
C........Tank cannot accept any more hydrogen
         fH2_charge_rate = 0.
      else
         fH2_charge_rate = fH2_Conn_Mass_Flow
      endif

C----------------------------------------------------------------------
C     Calculate concentration of absorbed alloy
C
C             mols of absrobed hydrogen
C        C =  -------------------------
C               total mols of alloy
C
C     See equation 89, Ferguson 2005.
C----------------------------------------------------------------------
      fConcentration = fH_absorbed_kmol / fMH_kmols_alloy (iComponent) ! (mol/mol)

C----------------------------------------------------------------------
C     Use subordinate function, fMH_Eval_Langmuir_C to determine
C     the equilibrium concentration of absorbed hydrogen. Note:
C     other PCT models could be substituted here.
C----------------------------------------------------------------------
      fConc_Equilibrium = fMH_Eval_Langmuir_C
     &    ( iComponent, fPressure, fTemperature)          ! (mol/mol)

C----------------------------------------------------------------------
C     Determine rate of change in concentration:
C
C       dC
C       --  = mass transfer coeff. * ( Equilibrium conc. - conc. )
C       dt 
C
C     See Equation 94, Ferguson 2005.
C----------------------------------------------------------------------

      fConc_dCdt = fMH_PCT_param_Kc ( iComponent )
     &     * ( fConc_Equilibrium - fConcentration )

C----------------------------------------------------------------------
C     If concentration is greater than max, rate of change cannot
C     be positive. If concentration is near zero, rate of
C     change cannot be negative
C----------------------------------------------------------------------
      if ( fConcentration .ge. fMH_concentration_MAX(iComponent) .and.
     &     fConc_dCdt .gt. 0.0 ) fConc_dCdt = 0.0
      if ( fConcentration .le. 0.0  .and. fConc_dCdt .lt. 0.0 )
     *     fConc_dCdt = 0.0
C----------------------------------------------------------------------
C     Calculate the rate of change in absorbed hydrogen (H):
C
C         d (mols absorbed)/dT = ( dC / dt ) * ( total mols of alloy)
C
C----------------------------------------------------------------------
      fH_absorbed_dNdt = fConc_dCdt * fMH_kmols_alloy ( iComponent ) ! (kmol/s)

C----------------------------------------------------------------------
C     Calculate rate of change in free hydrogen (H2):
C
C     d (mols free)/dT = ( charge rate ) - ( discharge rate )
C
C               + 0.5 * d (mols absorbed ) / dt
C
C----------------------------------------------------------------------
      fH2_free_dNdt = ( fH2_charge_rate - fH2_discharge_rate )
     &      - fH_absorbed_dNdt / 2.0

C----------------------------------------------------------------------
C     Calculate rate of change in pressure
C
C              univ. gas constant
C      dP/dt = ------------------ x
C                free volume
C
C               |               d (mols free)               d temp. |
C               | temperature * ------------- + mols free * ------- |
C               |                    dt                       dt    |
C
C     See equation 95, Ferguson 2005. Note: Universal gas constant
C     is defined in J/kmol K
C
C----------------------------------------------------------------------

      fPressure_dPdt = fR_universal  / fMH_free_volume (iComponent)
     & * ( fTemperature * fH2_free_dNdt + fH2_free_kmol * fTemp_dTdt )




      return
      end

C----------------- fMH_Eval_Q_reaction --------------------------------
C
C This function evaluates the heat of reaction associated with
C the absorbtion of hydrogen at a given pressure and temperature. 
C
C Inputs:
C
C   -  iComponent: index of component in plant network
C   -  fTemperature: Temperature inside cylinder (K) 
C   -  fPressure: pressure inside the cylinder (Pa)
C
C Outputs: 
C   -  fMH_Eval_Q_reaction: Heat released during absorbtion
C         of hydrogen (J/kmol H2 absorbed)
C
C-----------------------------------------------------------------------
      real function fMH_Eval_Q_reaction (
     &    iComponent,
     &    fPressure, 
     &    fTemperature )
      implicit none
#include "plant.h"
#include "Hydrogen_MH_store.h"
C-----------------------------------------------------------------------
C     Passed arguements
C-----------------------------------------------------------------------
      integer iComponent         ! pointer to component in plant net.
      real fTemperature          ! temperature inside cylinder (K)
      real fPressure             ! pressure inside cylinder
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      real fPress_TRAN           ! Pressure corresponding to transition
                                 !    from undercharge to plateau region (Pa)
      real fDummy1, fDummy2      ! Dummy arguements.
                                 
C-----------------------------------------------------------------------
C     Use function MH_Eval_Langmuir_TRANS to determine transitional
C     pressure. Note: we're not presently interested in procedure
C     MH_Eval_Langmuir_TRANS' other two outputs.
C-----------------------------------------------------------------------
      call MH_Eval_Langmuir_TRANS(
     &     iComponent,
     &     fTemperature,
     &     fPress_TRAN,
     &     fDummy1,
     &     fDummy2 )
                  
C-----------------------------------------------------------------------
C     Calculate enthalpy of absorbtion:
C-----------------------------------------------------------------------
      if ( fPressure .lt. fPress_TRAN ) then
      
C-----------------------------------------------------------------------
C        System is in undercharge region --- enthalpy of absorption is
C        constant. See Equation 6, Jiang et al. 2004.
C-----------------------------------------------------------------------
         fMH_Eval_Q_reaction = 2.0 * fMH_PCT_param_dH1 ( iComponent )  ! (J)

      else

C-----------------------------------------------------------------------
C        System is in plateau region --- enthalpy of absorption is
C        pressure-dependent:
C
C
C        dH  = 2 * [ dH2 + ( dH1 - dH2 ) * sqrt(Ps) / sqrt (P)
C
C
C        See Equation 6, Jiang et al 2004.
C
C-----------------------------------------------------------------------      
         fMH_Eval_Q_reaction = 2.0 * ( fMH_PCT_param_dH2 ( iComponent )
     &      +  (   fMH_PCT_param_dH1 ( iComponent )
     &           - fMH_PCT_param_dH1 ( iComponent ) )
     &         * fPress_TRAN ** 0.5 / fPressure ** 0.5 )

      endif
      
      return
      end 

C----------------- fMH_Eval_RateChange ---------------------------------
C
C This function evaluates the maximum dimensionless rate of change
C occuring in the:
C    a) # mols of absorbed hydrogen
C    b) # mols of free hydrogen 
C    c) pressure.
C
C It then compares this value with the previous maximum and returns
C the 'maximum rate of change encountered thus far'
C
C Inputs:
C
C   -  fH2_free_dNdt: Rate of change in free hydrogen (kmol/s)
C   -  fH_absorbed_dNdt: Rate of change in absorbed hydrogen (kmol/s)
C   -  fPressure_dPdt: Rate of change in cylinder pressure (kPa)
C   -  fH2_free_kmol_P: free hydrogen (kmol)
C   -  fH_absorbed_kmol_P: absorbed hydrogen (kmol)
C   -  fPressure_P: Pressure in the cylinder (Pa)
C   -  fRateChange_MAX: Previous value of maximum rate of change (1/s)
C
C Outputs: 
C   -  fRateChange_MAX: New value of maximum rate of change. (1/s)
C
C-----------------------------------------------------------------------
      real function fMH_Eval_RateChange(
     &     fH2_free_dNdt,
     &     fH_absorbed_dNdt,
     &     fPressure_dPdt,
     &     fH2_free_kmol_P,
     &     fH_absorbed_kmol_P,
     &     fPressure_P,
     &     fRateChange_MAX )
      implicit none 

C-----------------------------------------------------------------------
C     Passed arguements 
C-----------------------------------------------------------------------
      real fH2_free_dNdt        ! Rate of change in free hydrogen (kmol/s)
      real fH_absorbed_dNdt     ! Rate of change in absorbed hydrogen (kmol/s)
      real fPressure_dPdt       ! Rate of change in cylinder pressure (kPa)
      real fH2_free_kmol_P      ! free hydrogen (kmol)
      real fH_absorbed_kmol_P   ! absorbed hydrogen (kmol)
      real fPressure_P          ! Pressure in the cylinder (Pa)
      real fRateChange_MAX      ! Previous value of maximum rate of change (1/s)
  
C-----------------------------------------------------------------------
C     Local variables 
C-----------------------------------------------------------------------
      real fH2_free_dNdt_P      ! dimensionless rates of change
      real fH_absorbed_dNdt_P   !
      real fPressure_dPdt_P     !

C-----------------------------------------------------------------------
C     Determine dimensionless rates of change in absorbed & free
C     hydrogen, and pressure, and compare to maximum value 
C     encountered thus far. Update maximum, if necessary.
C-----------------------------------------------------------------------

      fH2_free_dNdt_P    = abs( fH2_free_dNdt )
     &     / fH2_free_kmol_P 

      fH_absorbed_dNdt_P = abs( fH_absorbed_dNdt )
     &     / fH_absorbed_kmol_P

      fPressure_dPdt_P   = abs( fPressure_dPdt )
     &     / fPressure_P

      if ( amax1 ( fH2_free_dNdt_P, fH_absorbed_dNdt_P)
     &     .gt. fRateChange_MAX ) then
         
         fMH_Eval_RateChange  =
     &        amax1 (
     &        fH2_free_dNdt_P,
     &        fH_absorbed_dNdt_P )
         

!          print*, '       -> updating maximum:'
!          print*, '          nfree:',fH2_free_dNdt_P,'=',
!      &        abs( fH2_free_dNdt ),'/',fH2_free_kmol_P
!          print*, '          nabs: ',fH_absorbed_dNdt_P,'=',
!      &        abs( fH_absorbed_dNdt ),'/',fH_absorbed_kmol_P
!          print*, '          press:',fPressure_dPdt_P,'=',
!      &        abs( fPressure_dPdt  ),'/',fPressure_P
      else 

         fMH_Eval_RateChange = fRateChange_MAX 

      endif 

      return 
      end 


C----------------- fMH_Eval_Langmuir_C -------------------------------
C
C This function evaluates the concentration of 
C hydride ( hydrogen + alloy ) mols inside a metal hydride
C storage container. It uses the Langmuir PCT isotherm
C correlation proposed by Jiang et. al (2004)
C
C Inputs:
C  - iComponent: index of component in plant network
C  - fPressure: pressure inside the cylinder (Pa)
C  - fTemperature: Temperature inside cylinder (K)
C
C Outputs:
C
C  - fMH_Eval_PCT_C: Concentration inside the cylinder 
C       ( mols hydride / mols alloy + mols hydride )
C
C----------------------------------------------------------------------

      real function fMH_Eval_Langmuir_C(
     &     iComponent, 
     &     fPressure,
     &     fTemperature )
      implicit none 

#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"


C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent        ! pointer to component in plant net.
      real fPressure            ! pressure inside cylinder (kPa)
      real fTemperature         ! temperature inside cylinder (K)
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
      real fPress_TRAN          ! Switch pressure (kPa)
      real fB1, fB2             ! correlation coefficients
      real fDummy1,fDummy2      ! dummy variables
       

C----------------------------------------------------------------------
C     Calculate 'switch' presssure at which cylinder transitions from
C     undercharged to plateau regions.
C
C     Use subordinate procedure MH_Eval_Langmuir_TRANS, and note
C     that the last two outputs from this procedure are not needed 
C     in this context.
C----------------------------------------------------------------------
      call MH_Eval_Langmuir_TRANS 
     &     ( iComponent,fTemperature,fPress_TRAN,fDummy1, fDummy2 )


      if ( fPressure .le. fPress_TRAN ) then
C----------------------------------------------------------------------
C        System is operating in the undercharge region.
C
C        Calculate coefficient B1:
C
C                        |           dH absorbtion             |
C        B1 = b1,o * exp |-------------------------------------|
C                        | universal gas constant * Temperature|
C
C
C        where b1,o is an empirical coefficient.
C----------------------------------------------------------------------
         fB1 = fMH_PCT_param_b1o( iComponent)
     &        * exp ( -1.0 *  fMH_PCT_param_dH1 ( iComponent )
     &                /  ( fR_universal * fTemperature ) )  ! (Pa)^0.5

C----------------------------------------------------------------------
C        Calculate concentration 'C'
C
C             (Cs) [ 1 + b1 * Ps^(1/2)]        P^(1/2)
C        C =  -------------------------  x  --------------
C                    Ps^(1/2)               1 + b1 P^(1/2)
C                    
C
C
C        where: Cs is the concentration of absorbed hydrogen at
C               pressure Ps
C
C
C        See Equation 90, Ferguson 2005.
C----------------------------------------------------------------------
         fMH_Eval_Langmuir_C = 
     &         fMH_concentration_TRANS ( iComponent )
     &     * ( 1.0 + fB1 * fPress_TRAN ** 0.5 )
     &     / ( fPress_TRAN ** 0.5 )
     &     * ( fPressure ** 0.5 )
     &     / ( 1.0 + fB1 * fPressure ** 0.5 )        ! (mol/mol)

      else
C----------------------------------------------------------------------
C        System is operating in the plateau or overcharge
C        region
C
C        Calculate coefficient B1:
C
C                        |           dH absorbtion             |
C        B2 = b2,o * exp |-------------------------------------|
C                        | universal gas constant * Temperature|
C
C
C        where b1,o is an empirical coefficient.
C----------------------------------------------------------------------
         fB2 = fMH_PCT_param_b2o( iComponent)
     &        * exp ( -1.0 * fMH_PCT_param_dH2 ( iComponent )
     &                /  ( fR_universal * fTemperature ) ) ! (Pa)^0.5

C----------------------------------------------------------------------
C        Calculate concentration 'C'
C
C                   (Cmax-Cs) b2 ( P^(1/2) - Ps^(1/2) )
C        C = (Cs) + -----------------------------------
C                     1 + b2 (  P^(1/2) - Ps^(1/2) )
C
C
C
C        where: Cmax is the maximum possible concentration
C               of absorbed hydrogen.C
C
C        See Equation 90, Ferguson 2005.
C----------------------------------------------------------------------

         fMH_Eval_Langmuir_C = 
     &        fMH_concentration_TRANS ( iComponent ) 
     &      + fB2 * ( fMH_concentration_MAX ( iComponent )
     &                 - fMH_concentration_TRANS ( iComponent ) )
     &      * ( fPressure ** 0.5 - fPress_TRAN ** 0.5 )
     &            / ( 1.0 + fB2 
     &                 * ( fPressure ** 0.5 - fPress_TRAN ** 0.5 ) )  ! (mol/mol)


      endif

      return
      end


C----------------- fMH_Eval_Langmuir_P -------------------------------
C
C This function evaluates the pressure inside a inside a metal hydride
C storage container. for a given equilibrium concentration.It uses the
C Langmuir PCT isotherm correlation proposed by Jiang et. al (2004).
C
C Inputs:
C  - iComponent: index of component in plant network
C  - fConcentration: equilibrium concentration (mol/mol)
C  - fTemperature: Temperature inside cylinder (K)
C
C Outputs:
C
C  - fMH_Eval_Langmuir_P: Pressure inside the cylinder (Pa)
C
C----------------------------------------------------------------------

      real function fMH_Eval_Langmuir_P(
     &     iComponent,
     &     fConcentration,
     &     fTemperature )
      implicit none

#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent        ! pointer to component in plant net.
      real fConcentration       ! Equilibriun concentration inside
                                !   the cylinder (mol/mol)
      real fTemperature         ! temperature inside cylinder (K)
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fPress_TRAN          ! Switch pressure (Pa)
      real fB1, fB2             ! correlation coefficients
      real fDummy1,fDummy2      ! dummy variables

      logical bNums_Are_Close   ! flag indicating that numbers
                                ! are close.


C----------------------------------------------------------------------
C     Ensure that concentration does not equal maximum concentration
C     --- otherwise divide-by-zero error will ensue.
C----------------------------------------------------------------------
      call eclose ( fConcentration,
     &              fMH_concentration_MAX ( iComponent ),
     &              fMH_concentration_MAX ( iComponent ) * 5.0E-03,
     &              bNums_Are_Close )

      if (  fConcentration .gt. fMH_concentration_MAX ( iComponent )
     &      .or. bNums_Are_Close  ) then

C........Set concentration to prevent divide-by-zero error.
         fConcentration = fMH_concentration_MAX ( iComponent )
     &      - fMH_concentration_MAX ( iComponent ) * 5.0E-03

      endif

C----------------------------------------------------------------------
C     Calculate 'switch' presssure at which cylinder transitions from
C     undercharged to plateau regions.
C
C     Use subordinate procedure MH_Eval_Langmuir_TRANS, and note
C     that the last two outputs from this procedure are not needed
C     in this context.
C----------------------------------------------------------------------
      call MH_Eval_Langmuir_TRANS
     &     ( iComponent,fTemperature,fPress_TRAN,fDummy1, fDummy2 )

C----------------------------------------------------------------------
C     Compare equilibrium concentration with concentration
C     at transition point
C----------------------------------------------------------------------
      if ( fConcentration.lt.fMH_concentration_TRANS(iComponent) )then


C----------------------------------------------------------------------
C        System is operating in the undercharge region.
C
C        Calculate coefficient B1:
C
C                        |           dH absorbtion             |
C        B1 = b1,o * exp |-------------------------------------|
C                        | universal gas constant * Temperature|
C
C
C        where b1,o is an empirical coefficient.
C----------------------------------------------------------------------
         fB1 = fMH_PCT_param_b1o( iComponent)
     &        * exp ( -1.0 *  fMH_PCT_param_dH1 ( iComponent )
     &                /  ( fR_universal * fTemperature ) )  ! (Pa)^0.5

C----------------------------------------------------------------------
C        Calculate pressure corresponing to equilibrium
C        concentration. The equilibrium concentration is
C        described by Equation 90, in Ferguson 2005:
C
C
C             (Cs) [ 1 + b1 * Ps^(1/2)]        P^(1/2)
C        C =  -------------------------  x  --------------
C                    Ps^(1/2)               1 + b1 P^(1/2)
C
C
C        This equation must be solved to determine the pressure P.
C        Thus:
C
C             /                                            \ 2
C             | C  / |(Cs) [ 1 + b1 * Ps^(1/2)]          | |
C        P =  |   /  |------------------------- - (b1) C | |
C             |  /   |        Ps^(1/2)                   | |
C             \                                            /
C
C----------------------------------------------------------------------
         fMH_Eval_Langmuir_P =
     &        ( fConcentration
     &
     &            / ( fMH_concentration_TRANS(iComponent)
     &                   * ( 1.0 + fB1 * fPress_TRAN ** 0.5 )
     &                   / fPress_TRAN ** 0.5
     &
     &                - fB1 * fConcentration
     &              )
     &        ) ** 2.0

      else
C----------------------------------------------------------------------
C        System is operating in the plateau or overcharge
C        region
C
C        Calculate coefficient B1:
C
C                        |           dH absorbtion             |
C        B2 = b2,o * exp |-------------------------------------|
C                        | universal gas constant * Temperature|
C
C
C        where b1,o is an empirical coefficient.
C----------------------------------------------------------------------
         fB2 = fMH_PCT_param_b2o( iComponent)
     &        * exp ( -1.0 * fMH_PCT_param_dH2 ( iComponent )
     &                /  ( fR_universal * fTemperature ) ) ! (Pa)^0.5

C----------------------------------------------------------------------
C        Calculate pressure corresponing to equilibrium
C        concentration. The equilibrium concentration is
C        described by Equation 90, in Ferguson 2005:
C
C                   (Cmax-Cs) b2 ( P^(1/2) - Ps^(1/2) )
C        C = (Cs) + -----------------------------------
C                     1 + b2 (  P^(1/2) - Ps^(1/2) )
C
C
C        This equation must be solved to determine the pressure P.
C        Thus:
C
C            /                          \ 2
C            |              ( C - Cs )  |
C        P = | Ps^(1/2) + ------------- |
C            |            b2 (Cmax - C) |
C            \                          /
C
C----------------------------------------------------------------------

         fMH_Eval_Langmuir_P =
     &        (   fPress_TRAN ** 0.5
     &          + ( fConcentration
     &                - fMH_Concentration_TRANS(iComponent)
     &            )
     &          / ( fB2 *
     &               ( fMH_Concentration_MAX(iComponent)
     &                   - fConcentration
     &                )
     &            )
     &        ) ** 2.0


      endif

      return
      end








C----------------- MH_Eval_Langmuir_TRANS ---------------------------
C
C This function determines, for a given temperature,
C the hydride storage's transition pressure / and the number of 
C free & absorbed mols of hydrogen under these conditions. The PCT
C model is based on the Langmuir isotherm correlations proposed 
C by Jiang et. al (2004)
C
C Inputs:
C  - iComponent: index of component in plant network
C  - fTemperature: Temperature inside cylinder (K)
C
C Outputs:
C
C  - fPress_TRANS: pressure at transition point,
C  - fH2_Free_kmol_TRANS: free hydrogen at transition point (kmol)
C  - fH_abs_kmol_TRANS: absorbed mols of hydrogen at transition point 
C        (kmol)
C
C----------------------------------------------------------------------

      subroutine MH_Eval_Langmuir_TRANS (
     &     iComponent,
     &     fTemperature,
     &     fPress_TRAN,
     &     fH2_free_kmol_TRAN,
     &     fH_abs_kmol_TRAN )
      implicit none
#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iComponent        ! index of component in plant network
      real fTemperature         ! temperature inside cylinder (K)

C Outputs
      real fPress_TRAN          ! Transition pressure (Pa)
      real fH2_free_kmol_TRAN   ! free hydrogen at transition (kmol)
      real fH_abs_kmol_TRAN     ! absorbed hydrogen at transition (kmol)


C----------------------------------------------------------------------
C     Calculate 'switch' pressure, at which cylinder transitions from
C     undercharged to plateau regions.
C
C     Ps = exp [ Do + dH_s / ( universal gas constant * temperature ) ]
C
C     See Equation 95, Ferguson 2005. Note: both dH & R are defined
C     in J/kmok K. 
C----------------------------------------------------------------------
      fPress_TRAN = exp ( fMH_PCT_param_Do ( iComponent )
     &        + fMH_PCT_param_dHs ( iComponent )
     &        / ( fR_universal * fTemperature ) )  ! (Pa)


C----------------------------------------------------------------------
C     Determine number of mols of free hydrogen using the ideal gas
C     law:
C
C                           (Pressure) (free volume)
C       H2 N free =  ---------------------------------------
C                    (universal gas constant) ( temperature)
C
C----------------------------------------------------------------------
      fH2_free_kmol_TRAN = fPress_TRAN * fMH_free_volume ( iComponent )
     &     / fR_Universal / fTemperature ! (kmol)

C----------------------------------------------------------------------
C     Determine the number of mols of absorbed hydrogen 
C
C       H N abs = (concentration) * (kmol of alloy)
C
C----------------------------------------------------------------------
      fH_abs_kmol_TRAN = fMH_concentration_TRANS ( iComponent )
     &                 * fMH_kmols_alloy ( iComponent )

      return
      end


C---------------- MH_hydride_PostPro -----------------------------------
C
C This is a postprocessing and reporting routine for the
C compressed gas cylinder model.
C
C Inputs:
C
C  iComponent - index of component in plant network
C  iStatus - flag indicating if module must be initalized
C
C Outputs
C
C  None.
C
C----------------------------------------------------------------------
      subroutine MH_hydride_PostPro(iComponent, iStatus)
      implicit none
#include "plant.h"
#include "chemical_properties.h"
#include "Hydrogen_MH_store.h"
#include "CETC_definitions.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iComponent        ! pointer to comp. in network
      integer iStatus                 ! Flag indicating comp. status.

C----------------------------------------------------------------------
C     ESP-r common blocks
C----------------------------------------------------------------------

C.....Plant component ascii names
      common/pcnam/
     &     pcname(mpcom)
      character*15 pcname       ! Plant component names

C.....Celsius -> Kelvin conversion
      real fC_to_K
      parameter ( fC_to_K  = 273.15 )

C.....Plant present and future state variables
      common/pcval/csvf,csvp
      real csvf(mpnode,mpvar)   ! future time-row state variables
      real csvp(mpnode,mpvar)   ! present time-row state variables

C.....Plant component connection data
      common/pcond/convar, icontp, icondx
      real    convar(mpcon,mconvr)        ! state varibles for connections
      integer icontp(mpcon)               ! type of each connection
      integer icondx(mpcom,mnodec,mpconc) ! pointer to connections for each
                                          ! component/node

C.....Misc. plant component data.
      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      character*128 H3Kname       ! holding variable
      character*128 char_temp     ! holding variable
      integer iLength,iNameLength ! Length of strings

C.....Connection indicies:
      integer iH2O_connection, iH2_connection 

      integer iH2O_node, iH2_node
 
      
C.....Arrays storing locations of output variables
      integer iStorage_index(mpcom)
      save iStorage_index

C.....Molar mass of hydrogen (kg/kmol)
      real fMM_Hydrogen
      save fMM_Hydrogen

C.....Heat transfer to ambient (W)
      real fQ_loss

C.....Heat transfer to cooling water (W)
      real fQ_cooling

C.....Cooling water average temperature (oC)
      real fTemp_Cooling_avg          

                        
      
C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real fCompound_R          ! function returing ideal gas constant
                                !   a compound

      real fEval_Compound_MM    ! Funcion returning the molar mass
                                !   of a compound
      real shtfld               ! Function returning the heat capacity
                                !   of water
      real H3K_Connect_property ! Function returning properties of a
                                !   connection   
                                
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
C........Get hydrogen molar mass
         fMM_Hydrogen = fEval_Compound_MM ( iHydrogen )


      else

C----------------------------------------------------------------------
C        Recover Component name
C----------------------------------------------------------------------
         call h3k_RecoverString(iStorage_index(iComponent),
     &                       char_temp, iNameLength)

C........Get water connection index
         iH2_connection  = iCondX ( iComponent, 1, 1 )
         iH2O_connection = iCondX ( iComponent, 2, 1 )
         
C........H2O node index
         iH2_node  = NPCDAT ( iComponent, 9 )
         iH2O_node = NPCDAT ( iComponent, 9 ) + 1
         
C----------------------------------------------------------------------
C        Calculate rate of heat transfer to cooling water
C----------------------------------------------------------------------

         if ( iH2O_connection .gt. 0 ) then
C........Average inlet & outlet temperatures
            fTemp_Cooling_avg = 0.5 * ( CSVF(iH2O_node,1)
     &         + H3K_Connect_property( iH2O_connection, iProp1stflow ) )

C........Use temperature rise in cooling water (Q=mCdT)
            fQ_cooling =
     &        H3K_Connect_property( iH2O_connection, iProp1stflow )
     &      * ShtFld ( 3, fTemp_Cooling_avg )
     &      * ( csvf(iH2O_node,1)
     &          -H3K_Connect_property( iH2O_connection, iPropTemp ) )

!             print*, fQ_cooling, '=',
!      &        H3K_Connect_property( iH2O_connection, iProp1stflow ),
!      &      '*', ShtFld ( 3, fTemp_Cooling_avg ),
!      &      '*(', csvf(iH2O_node,1),
!      &      '-',H3K_Connect_property( iH2O_connection, iPropTemp ),')'
         else

            fQ_cooling = 0.0
         
         endif
         
C----------------------------------------------------------------------
C        Calculate rate of heat loss to ambient
C----------------------------------------------------------------------     
C........Q = hA ( T - T_amb )
         fQ_loss = fMH_cyl_wall_hA ( iComponent ) *
     &       (  csvf ( iH2_node, 1)
     &         -fMH_Report_Containment_TEMP ( iComponent ) )
     
C-----------------------------------------------
C........Output data: future time-row free hydrogen (kmol)
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/free/kmol'
         call add_to_xml_reporting(
     &      fMH_Report_H2_free_kmol(iComponent) ,
     &      H3Kname,
     &      'units', '(kmol)',
     &      'Amount of hydrogen (H2) in gaseous form in cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row free hydrogen (kg)
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/free/kg'
         call add_to_xml_reporting(
     &      fMH_Report_H2_free_kmol(iComponent) * fMM_Hydrogen,
     &      H3Kname,
     &      'units', '(kg)',
     &      'Amount of hydrogen (H2) in gaseous form in cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row absorbed hydrogen (kmol)
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/absorbed/kmol'
         call add_to_xml_reporting(
     &      fMH_Report_H_abs_kmol(iComponent) ,
     &      H3Kname,
     &      'units', '(kmol)',
     &      'Amount of hydrogen (H) absorbed in cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row absorbed hydrogen (kg)
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/absorbed/kg'
         call add_to_xml_reporting(
     &      fMH_Report_H_abs_kmol(iComponent) * fMM_Hydrogen,
     &      H3Kname,
     &      'units', '(kg)',
     &      'Amount of hydrogen (H) absorbed in cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row hydrogen vent rate
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/vent/kmol'
         call add_to_xml_reporting(
     &      fMH_Report_H2_vent(iComponent),
     &      H3Kname,
     &      'units', '(kmol/s)',
     &      'Rate at which hydrogen is vented from cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row hydrogen charge rate
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/charge/kmol'
         call add_to_xml_reporting(
     &      fMH_Report_H2_charge(iComponent),
     &      H3Kname,
     &      'units', '(kmol/s)',
     &      'Rate at which hydrogen is charged to cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row hydrogen discharge rate
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/hydrogen/discharge/kmol'
         call add_to_xml_reporting(
     &      fMH_Report_H2_discharge(iComponent),
     &      H3Kname,
     &      'units', '(kmol/s)',
     &      'Rate at which hydrogen is discharged from cylinder'
     &      )

C-----------------------------------------------
C........Output data: future time-row pressure
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/pressure'
         call add_to_xml_reporting(
     &      fMH_Report_Pressure(iComponent) / 1000.,
     &      H3Kname,
     &      'units', '(kPa)',
     &      'Cylinder pressure'
     &      )

C-----------------------------------------------
C........Output data: future time-row pressure
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/pressure'
         call add_to_xml_reporting(
     &      fMH_Report_Pressure(iComponent) / 1000.,
     &      H3Kname,
     &      'units', '(kPa)',
     &      'Cylinder pressure'
     &      )

C-----------------------------------------------
C........Output data: Explicit hydrogen demand
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/demand/explicit'
         call add_to_xml_reporting(
     &      fMH_Report_Demand_EXP ( iComponent ),
     &      H3Kname,
     &      'units', '(kmol/s)',
     &      'Hydrogen requirements of components in plant network'
     &      )
C-----------------------------------------------
C........Output data: Implicit hydrogen demand
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/demand/implicit'
         call add_to_xml_reporting(
     &      fMH_Report_Demand_IMP(iComponent),
     &      H3Kname,
     &      'units', '(kmol/s)',
     &      'Hydrogen requirements of devices not necessarly contained '
     &      // 'within the plant network'
     &      )

     
C-----------------------------------------------
C........Output data: Heat transfer to cooling water
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/heat_recovery'
         call add_to_xml_reporting(
     &      fQ_cooling,
     &      H3Kname,
     &      'units', '(W)',
     &      'Heat transferred to cooling water (+ive is transfer'
     &      // ' from MH unit to water)')

     
C-----------------------------------------------
C........Output data: Heat loss
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/heat_loss'
         call add_to_xml_reporting(
     &      fQ_loss,
     &      H3Kname,
     &      'units', '(W)',
     &      'Heat lost to surroundings')

C-----------------------------------------------
C........Output data: Heat production (absorbtion)
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/heat_of_reaction'
         call add_to_xml_reporting(
     &      fMH_Report_Q_absorbtion(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Heat generated during absorption of hydrogen')
     
C-----------------------------------------------
C........Output data: Containment temperature
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/containment_temperature'
         call add_to_xml_reporting(
     &      fMH_Report_Containment_TEMP(iComponent),
     &      H3Kname,
     &      'units', '(oC)',
     &      'Temperature of containing zone')

C-----------------------------------------------
C........Flow work associated with charging
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/h2_connection/flow_work'
         call add_to_xml_reporting(
     &      fMH_Report_Charge_FWork(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Flow work associated with charging tank')

C-----------------------------------------------
C........Flow work associated with charging
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/h2_discharge/flow_work'
         call add_to_xml_reporting(
     &      fMH_Report_Disch_FWork(iComponent),
     &      H3Kname,
     &      'units', '(W)',
     &      'Flow work assocaited with discharging tank')
                   
      endif

      return
      end

C--------------- fMH_Debug_PCT -----------------------------------------
C
C     This is a debugging procedure that outputs PCT isotherms for the 
C     metal-hydride model. It is not called during normal simulation.
C
C-----------------------------------------------------------------------
      subroutine MH_debug_PCT ( iComponent )
      implicit none 
#include "plant.h"
#include "Hydrogen_MH_store.h"
#include "chemical_properties.h"

      integer iComponent 
C-----------------------------------------------------------------------
C     Debugging variables
C-----------------------------------------------------------------------
      real fDebug_Temp
      real fDebug_Press
      real fDebug_Charge
      real fDebug_Discharge
      real fDebug_Nfree_P
      real fDebug_Nabs_P
      real fDebug_Nfree_F
      real fDebug_Nabs_F
      real fDebug_Press_F
      real fDebug_dNfree_dT
      real fDebug_dNabs_dT
      real fDebug_dPdT
      real fDebug_dTdt
      real fDebug_Vol_flow 

      real fDebug_C_results (11)

      real fDebug_DisTime (3600*16)
      real fDebug_DisP_results (3600*16+1,8)
      real fDebug_DisC_results (3600*16+1,8)

      real fDebug_time
      real fDebug_dt
      integer iErr
      integer iCount_a, iCount_b, iDebug_TS_count

C.....reference
      real fMH_Eval_Langmuir_C



C----------------------------------------------------------------------
C     Model parms: If this section of the code is uncommented, 
C     the model will produce the PCT isotherms & discharge profiles
C     published by Gadre et al. 2003.
C----------------------------------------------------------------------


c$$$      fMH_kmols_alloy ( iComponent ) = 0.357723
c$$$
c$$$      fMH_free_volume ( iComponent ) = 0.0041 
c$$$
c$$$      fMH_concentration_TRANS ( iComponent ) = 0.125
c$$$      fMH_concentration_MAX ( iComponent )   = 0.984
c$$$      fMH_concentration_MIN ( iComponent )   = 0.0
c$$$
c$$$      fMH_pressure_MAX ( iComponent )  = 80.0  * 101.32 * 1.0E03
c$$$
c$$$      fMH_pressure_MIN ( iComponent )  = 1.0   * 101.32 * 1.0E03
c$$$ 
c$$$      fMH_pressure_INIT ( iComponent ) = 0.001 * 101.32 * 1.0E03
c$$$
c$$$      fMH_PCT_param_b1o ( iComponent ) =  2.36E-06
c$$$      fMH_PCT_param_b2o ( iComponent ) =  7.87E-06
c$$$
c$$$      fMH_PCT_param_dH1 ( iComponent ) = -1.38E04 * 1.0E03
c$$$      fMH_PCT_param_dH2 ( iComponent ) = -2.06E04 * 1.0E03
c$$$      fMH_PCT_param_dHs ( iComponent ) = -2.77E04 * 1.0E03
c$$$
c$$$      fMH_PCT_param_Do  ( iComponent ) = 2.40E01
c$$$
c$$$      fMH_PCT_param_Kc  ( iComponent ) =  8.14E-03

C----------------------------------------------------------------------
C     The following code will produce a set of PCT
C     isotherms that can be used when evaluating the model. The 
C     results should agree with Figure 4 from Gadre et. al 2004.
C----------------------------------------------------------------------
     
      
C.....Open file, and check that no errors were encountered
      open ( unit=93, File='PCT.csv', STATUS='unknown', 
     &     IOSTAT=iErr)
      if ( iErr .ne. 0 ) then
         print *, ' H2_MH_balance: Could not open file PCT.csv ',
     &        'for writing. PCT curves will not be written out. '
      else
         
C........Header row 
         write(93,*) ',273,283,293,303,313,323,333,343,353,363,373'

         do iCount_a = 0, 9999 
C           Calculate pressure
            fDebug_Press = 1.0 
     &           + ( fMH_Pressure_MAX ( iComponent ) - 1.0 )
     &           * float( iCount_a )/ 9999.0 

            do iCount_b = 1, 11
C..............Calculate temerature                
               fDebug_temp = 263.15 + float (iCount_b) * 10. 

C..............Determine concentraton at temperature, pressure
               fDebug_C_results (iCount_b) = 
     &              fMH_Eval_Langmuir_C(
     &              iComponent,
     &              fDebug_Press,
     &              fDebug_temp )

            enddo

C...........Output            
            write(93,10) fDebug_press / 1000.0, ',',
     &           fDebug_C_results (1),',',
     &           fDebug_C_results (2),',',
     &           fDebug_C_results (3),',',
     &           fDebug_C_results (4),',',
     &           fDebug_C_results (5),',',
     &           fDebug_C_results (6),',',
     &           fDebug_C_results (7),',',
     &           fDebug_C_results (8),',',
     &           fDebug_C_results (9),',',
     &           fDebug_C_results (10),',',
     &           fDebug_C_results (11),','

         enddo

         close ( unit = 93 )

      endif 




C-----------------------------------------------------------------------
C     The following code will characterize the discharge from a metal-
C     hydride storage container at various discharge rates. The container
C     is modelled isothermally.
C-----------------------------------------------------------------------

C.....Initialize parameters
      fDebug_temp = 295.0       ! K
      fDebug_Charge = 0.0       ! (kmol/s)
      fDebug_dTdt = 0.0         ! (K/s)
      fDebug_dt   = 60.0        ! s

C.....Calculate timesteps req'd for 16 hours:
      iDebug_TS_Count = int(16.*3600./fDebug_dt) 

C.....Open file, and check that no errors were encountered
      open ( unit=93, File='MH_discharge.csv', STATUS='unknown', 
     &     IOSTAT=iErr)
      if ( iErr .ne. 0 ) then
         print *, ' H2_MH_balance: Could not open file ',
     &        'MH_discharge.csv for writing. Discharge profiles ',
     &        'curves will not be written out. '
      else

         
C........Header rows
         write(93,*) ',Pressure,,,,,,,,Concentration'
         write(93,*) ',05,10,15,20,25,30,35,40,'
     &        // '05,10,15,20,25,30,35,40,'


         do iCount_a = 1, 8

C...........Characterize discharge @ 5-40 SLPM, in increments  
C...........of 5 SLPM.

C...........STP volumetric flow 
            fDebug_Vol_flow = float (iCount_a ) * 5.0  / 60.0 / 1000.0 ! (m3/s) 

C...........mass flow 
            fDebug_Discharge = 101.32 * fDebug_Vol_flow / 
     &           ( fR_universal / 1000.) / 273.15


C...........Collect initial conditions:         

C...........Pressure -> 25 atm
            fDebug_press = 25.0 * 101.32 * 1.0E03 ! (Pa)
            

            call MH_Eval_initial_conditions(
     &           iComponent,
     &           fDebug_press, 
     &           fDebug_temp, 
     &           fDebug_Nfree_P,
     &           fDebug_Nabs_P    )


C...........Set initial conditions             
            fDebug_DisTime(1)= 0.
            fDebug_DisP_results (1,iCount_a) = fDebug_press
     &           / 101.32 /1.0E03
            fDebug_DisC_results (1,iCount_a) = 
     &          fDebug_Nabs_P / fMH_kmols_alloy ( iComponent )

C...........Characterize system over 16 hours
            do iCount_b = 1, iDebug_TS_count


               call MH_Integrate_DEs (
     &              iComponent,
     &              fDebug_Nfree_P,
     &              fDebug_Nabs_P,
     &              fDebug_temp,
     &              fDebug_temp,
     &              fDebug_charge,
     &              fDebug_discharge,
     &              fDebug_dt,
     &              fDebug_Nfree_F,
     &              fDebug_Nabs_F,
     &              fDebug_Press_F
     &              )

C..............Calculate time (hrs)
               if ( iCount_a .eq. 1 ) then
                  fDebug_DisTime ( iCount_b) =  
     &                 float ( iCount_b ) * fDebug_dt / 3600.0
               endif

C..............Save results
               fDebug_DisP_results (iCount_b+1, iCount_a) 
     &            = fDebug_Press_F / 101.32 /1.0E03
               fDebug_DisC_results (iCount_b+1, iCount_a) =
     &              fDebug_Nabs_F / fMH_kmols_alloy ( iComponent )

C..............Time-row transport
! 
!                print*, '      -> mols      ', fDebug_Nfree_P +
!      &              fDebug_Nabs_P / 2.0
!                print*, '      -> mol change', fDebug_Nfree_F
!      &              + fDebug_Nabs_F / 2.0 
!      &              - fDebug_Nfree_P - fDebug_Nabs_P/ 2.0
!                print*, '      -> mols flow ',
!      &              ( fDebug_Nfree_F + fDebug_Nabs_F/ 2.0
!      &              - fDebug_Nfree_P -fDebug_Nabs_P/ 2.0)
!      &              / fDebug_dt
! 
!  
               fDebug_Nfree_P =  fDebug_Nfree_F
               fDebug_Nabs_P  =  fDebug_Nabs_F
              


            enddo

         enddo

C........Dump results to file 
         do iCount_b =  1, iDebug_TS_count

            write ( 93, 11 )
     &           fDebug_DisTime ( iCount_b),',',
     &           fDebug_DisP_results (iCount_b, 1),',',
     &           fDebug_DisP_results (iCount_b, 2),',',
     &           fDebug_DisP_results (iCount_b, 3),',',
     &           fDebug_DisP_results (iCount_b, 4),',',
     &           fDebug_DisP_results (iCount_b, 5),',',
     &           fDebug_DisP_results (iCount_b, 6),',',
     &           fDebug_DisP_results (iCount_b, 7),',',
     &           fDebug_DisP_results (iCount_b, 8),',',
     &           fDebug_DisC_results (iCount_b, 1),',',
     &           fDebug_DisC_results (iCount_b, 2),',',
     &           fDebug_DisC_results (iCount_b, 3),',',
     &           fDebug_DisC_results (iCount_b, 4),',',
     &           fDebug_DisC_results (iCount_b, 5),',',
     &           fDebug_DisC_results (iCount_b, 6),',',
     &           fDebug_DisC_results (iCount_b, 7),',',
     &           fDebug_DisC_results (iCount_b, 8),','

         enddo

         close ( unit = 93 )

      endif 


 10   FORMAT (
     &     F9.2,A,
     &     F9.7,A, 
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A )

 11   Format (
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.2,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A,
     &     F9.7,A
     &     )
      return
      end

