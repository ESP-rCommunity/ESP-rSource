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
C================== Stirling_CHP.F ====================================
C
C This file contains code necessary to model a Stirling=cycle
C based cogeneration device. These procedures provide two levels
C of resolution, depending on which model has been chosen from
C the plant component database. Selecting the low-resolution
C model will cause the low resolution correlations to be used
C to determine the operating point, while selecting the high
C resolution model will cause the high-resolution correlations
C to be used.
C
C GENERAL ROUTINES:
C
C   Stirling_coeff_gen: The top-level routine called by the plant
C       modelling domain to characterize the Stirling engine
C
C   Stirling_thermal_char: Top level-procedure controlling the thermal
C       characterization of the Stirling engine (called when ISTATS=1)
C
C   Stirling_Collect_Data: Collects Stirling model inputs and performs
C       rudamentry error trapping
C
C   Eval_correlations: Procedure to evaluate Stirling empirical correlations
C
C   Scale_flows: Procedure to calculate time-step averaged fuel usage,
C       power & thermal output.
C
C   bLinearize: Returns a linear equation (y=ax+b) of a line passing through
C       two supplied points
C
C   Stirling_H3Kreports_module: Transports data to the h3kreports module
C
C   Eval_TS_average: Determines if the predicted change of a provided
C       variable exceeds a maximum value, the final & time step averaged
C       value ofthe variable
C
C MODEL CORRELATIONS:
C
C   fEval_heat_efficiency: Evaluates the thermal efficiency correlation
C       for the low-resolution Stirling model
C
C   fEval_elec_efficiency: Evaluates the electrical efficiency correlation
C       for the low-resolution Stirling model
C
C   fEval_Skin_losses: Evaluates the Stirling model's heat loss correlation
C
C
C REFERENCES:
C
C A. Ferguson 2005, "Model Specifications for Stirling-cycle based
C     cogeneration device", IEA/ECBCS Annex 42.
C
C======================================================================

C------------------ Stirling_coeff_gen ---------------------------------
C
C Date:      Dec 16, 2004
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada
C
C ABSTRACT:
C This routine calculates the matrix coefficients for the stirling
C component model. Coefficients for the first phase, second phase,
C and hydrogen flow matricies are determined directly. Coefficients
C for the temperature matrix are determined in a subordinate routine,
C Stirling_thermal_char.
C
C
C Inputs:
C
C  iC_Index       - index of component in plant network
C  iPlant_Matrix          - index of plant matrix being solved
C
C Outputs
C  fCoefficients          - array containing components matrix
C                           coefficients
C
C----------------------------------------------------------------------
      
                       
      subroutine Stirling_coeff_gen (
     &               iC_Index,
     &               fCoefficients,
     &               iPlant_Matrix  )
      implicit none

      INCLUDE 'plant.h'

C----------------------------------------------------------------------
C     ESP-r commons 
C----------------------------------------------------------------------
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),
     &           ipc2(mpcon),ipn2(mpcon),pcondr(mpcon),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD
C----------------------------------------------------------------------
C     Passed arguemets
C----------------------------------------------------------------------
      integer iC_Index          ! pointer to component in net.
      integer iPlant_matrix             ! index of matrix to be solved

      real fCoefficients(mpcoe)         ! array containing matrix
                                        ! coefficients calculated by
                                        ! the component.      
C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      integer iPropTemp         ! named constant for temperature properity
      integer iProp1stFlow      ! named constant for first phase flow properity
      integer iProp2ndFlow      ! named constant for second phase flow properity
      integer iPropH2Flow       ! named constant for hydrogen flow
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )


      integer ii
C----------------------------------------------------------------------
C     Determine which matrix is being solved, and set coefficients
C     accordingly.
C----------------------------------------------------------------------
      if ( iPlant_Matrix .eq. iProp1stFlow ) then
      
C----------------------------------------------------------------------      
C        Plant first phase flow (ie air, water) matrix is to be
C        solved. The stirling engine has a single incomming connection
C        containing cooling water. Applying conservation of mass:
C
C           | 1.0    0.0   0.0 | | mo_1 |  = 0.0
C           | 0.0    1.0  -1.0 | | mo_2 |
C                                | mi_2 |
C
C        Where: mo is the flow through the engine, and
C               mi is the flow entering the engine.
C
C----------------------------------------------------------------------
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0
         fCoefficients(4) =  1.0
         fCoefficients(5) = -1.0
         fCoefficients(6) =  0.0
         fCoefficients(7) =  0.0

      elseif (iPlant_Matrix .eq. iProp2ndFlow ) then
C----------------------------------------------------------------------      
C        Plant second phase flow (ie water vapor) matrix is to be
C        solved. The stirling engine does not support 2nd-phase flow.
C        therefore:
C
C
C           | 1.0    0.0   0.0 | | mo_1 |  = 0.0
C           | 0.0    1.0   0.0 | | mo_2 |
C                                | mi_2 |
C
C        Where: mo is the flow through the engine, and
C               mi is the flow entering the engine.
C
C----------------------------------------------------------------------
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0
         fCoefficients(4) =  1.0
         fCoefficients(5) =  0.0
         fCoefficients(6) =  0.0
         fCoefficients(7) =  0.0
           

      elseif (iPlant_Matrix .eq. iPropH2Flow ) then
C----------------------------------------------------------------------
C        Plant hydrogen flow matrix is to be solved. The Stirling
C        engine model does not presently support hydrogen flow
C        analysis. Thus:
C
C           | 1.0    0.0   0.0 | | mo_1 |  = 0.0
C           | 0.0    1.0   0.0 | | mo_2 |
C                                | mi_2 |
C
C        Where: mo is the flow through the engine, and
C               mi is the flow entering the engine.
C
C----------------------------------------------------------------------
         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0
         fCoefficients(4) =  1.0
         fCoefficients(5) =  0.0
         fCoefficients(6) =  0.0
         fCoefficients(7) =  0.0
C----------------------------------------------------------------------
C        Note: A future inprovement might permit the Stirling engine's
C        operating point to be determined dynamically using an
C        incomming hydrogen flow rate.
C----------------------------------------------------------------------
      
      elseif ( iPlant_Matrix .eq. iPropTemp ) then
C----------------------------------------------------------------------
C        Temperature flow matrix is to be solved. Use subordinate
C        routine, Stirling_thermal_char() to determine temperature
C        matrix coefficients.
C----------------------------------------------------------------------
         call Stirling_thermal_dyn (iC_Index, fCoefficients)
      
      endif    
      
      return 
      end   ! of subroutine

C------------------ Stirling_thermal_char -----------------------------
C
C Date:      Dec 16, 2004
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada
C
C ABSTRACT:
C This routine characterizes the electrical and performance of a
C stirling-cycle based cogeneration system, and calculates
C matrix coefficients for the plant temperature solution
C matrix ( ISTATS = 1 )
C
C
C Inputs:
C
C  iC_Index       - index of component in plant network
C
C Outputs
C  fCoefficients          - array containing components matrix
C                           coefficients
C
C----------------------------------------------------------------------
      subroutine Stirling_Thermal_dyn ( iC_Index, fCoefficients )
      implicit none

      INCLUDE 'building.h'
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'
      INCLUDE 'chemical_properties.h'

C----------------------------------------------------------------------
C     Passed arguemets
C----------------------------------------------------------------------
      integer iC_Index             ! pointer to component in net.
      integer iPlant_matrix        ! index of matrix to be solved

      real fCoefficients(mpcoe)    ! array containing matrix
                                   ! coefficients calculated by
                                   ! the component.
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace/error reporting unit numbers
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu

C.....Plant matrix solver: implicit vs explicit weighting factor
      common/pcequ/impexp,ratimp
      integer impexp
      real ratimp
      
C.....Plant network time constants
      COMMON/pctc/tc
      real tc(mpcom)            ! time contant (seconds)

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

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

      common/pcnam/
     &     pcname(mpcom)
      character*15 pcname       ! Plant component names

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
      integer iEngine_Node_index ! Index of component engine node in plant
                                 !   Network.
      integer iHX_Node_index     ! Index of component HX node in plant
                                 !   Network.                                 
      integer iModel_Type        ! Model 'type' -> pointer to unique
                                 !   record in plant component database
                                      
      integer iCW_conn_index     ! Cooling water connection index


C.....Key model parameters
      real fN_fuel_flow          ! Fuel flow (mol/s)

      real fTime_Constant        ! Thermal time constant (s)
      real fSoln_alpha           ! Implicit / explicit weighting factor
      
C.....Results from correlations
      real fQ_avail_LHV          ! Lower heating value of fuel used (W)
      real fP_net                ! Net power produced (W)
      real fQ_cogen              ! Recoverable heat generation (W)
      real fQ_skin_losses        ! Parasitic heat loss from unit (W)

C.....Thermal state variables      
      real fQ_cogen_P            ! Present time row average recoverable 
                                 !   heat generation (W)

      real fQ_cogen_F            ! Future time row average recoverable 
                                 !   heat generation (W)                             

      real fQ_trans_to_water_P   ! Thermal output of engine & exhaust
                                 !   gas heat exchanger on present time
                                 !   row (W)
                                 
      real fQ_trans_to_water_F   ! Thermal output of engine & exhaust
                                 !   gas heat exchanger on future time
                                 !   row (W)

      real fEngine_Temp_P        ! Temperature of stirling engine control
                                 !   volume on present time row (oC)                                 

C.....Initial (instantaneous) conditions at start of timestep
      real fN_fuel_flow_init     ! Fuel flow at end of pervious timestep/mode (kg/s)
      real fP_net_init           ! Power output at end of previous timestep/mode (W)
      real fQ_cogen_init         ! SS Heat gen. at end of pervious timestep/mode (W)      

C.....Instantaneous conditions at end of timestep
      real fN_fuel_flow_final     ! Fuel flow at end of current timestep (kg/s)
      real fP_net_final           ! Power output at end of current timestep (W)
      real fQ_cogen_final         ! SS heat gen. at end of current timestep (W)                          

C.....Average conditions over current time-step.
      real fN_fuel_flow_average  ! Average Fuel flow over current timestep (kg/s)
      real fP_net_average        ! Average power output over current timestep (W)
      real fQ_cogen_average      ! Average recoverable heat generation over
                                 !    current time step
                                 
C.....Average conditions resulting from transient behavior during
C.....normal operation
      real fN_fuel_flow_trans_average  ! Average Fuel flow over  (kg/s)
      real fP_net_trans_average        ! Average power output  (W)
      real fQ_Cogen_trans_average      ! Final SS heat gen. (W)
  
                           
C.....Final conditions resulting from transient behavior during
C.....normal operation
      real fN_fuel_flow_trans_final    ! Final Fuel flow (kg/s)
      real fP_net_trans_final          ! Final power output (W)
      real fQ_Cogen_trans_final        ! Final SS heat gen. (W)

  
      
C.....Boundary conditions / cooling water present time-row conditions
      real fCW_mass_flow_P       ! Cooling water flow (kg/s)
      real fCW_temperature_in_P  ! Cooling water temp at inlet (oC)
      real fCW_temperature_out_P ! Cooling water temperature at outlet (oC)
      real fCW_heat_cap_P        ! Cooling water heat capacity (W/K)

C.....Cooling water present time-row conditions
      real fCW_mass_flow_F       ! Cooling water flow (kg/s)
      real fCW_temperature_in_F  ! Cooling water temp at inlet (oC)
      real fCW_heat_cap_F        ! Cooling water heat capacity (W/K)


C.....Flags related to cooling water      
      logical bNo_cooling_water  ! Flag indicating if there is no
                                 !   cooling water flowing though
                                 !   the system.

      logical bCooling_W_ToHot   ! Flag indicating cooling water is to hot
                                 !   for safe operation

      real fTemp_Room_P          ! Ambient temperature
      real fTemp_Room_F

C.....Operating mode & control
      integer iSystem_Status     ! flag indicating if system faults have
                                 !     occured
      integer iOperating_mode_P  ! Flag indicating which mode the system
                                 !   is operating in on present time row
      integer iOperating_mode_F  ! Flag indicating which mode the system
                                 !   is operating in on future time row
      integer iTarget_mode       ! Desired mode of operation indicated by
                                 !   controller signal
      real fOP_time              ! Accumulated time in current operating mode

      real fTS_mode_fractions( iOP_Mode_Count )
                                 ! Fraction of timestep spent in
                                 !    each operating mode

      logical bTS_modes( iOP_Mode_Count )
                                 ! logical flags indicating which operating
                                 !    modes the system will spend time in

      logical bStartup_complete  ! Flag indicating that startup period
                                 !     will be completed on this timestep

      integer iControl_method    ! Flag indicating which type of control
                                 !   should be applied
      real fControl_signal       ! Input from control interface.
                                 !  (Watts or dimensionless)
      real fOP_target            ! Target operating point described by
                                 !   the control signal. May describe a
                                 !   fuel flow rate (kmol/s) , power output
                                 !   (W), or rate of thermal output (W)
      logical bControl_Active    ! Flag indicating that controller is requesting
                                 !   engine turn on.
      logical bEngine_Active     ! Flag indicating that engine is on

C.....Electrical-network related variables
      integer iEModel            ! flag indicating which model should be used
      integer PQ, PA             ! Dummy arguements
      
C.....Counters
      integer iMode
                  
C.....Misc parameters
      logical bNums_are_close    ! flag for close to zero comparisons
      real fTS_duration          ! Timestep duration (seconds)

      logical bStatus_Warned     ! Flag indicating if a warning has
                                 ! been issued regarding the
                                 ! experminental status of the SE
                                 ! model

C.....Dummy variables --- req'd because not all of the outputs
C.....passed from Solve_Cogen_Model() are used in this implementation
      real fDummy_time, fDummy_fuel, fDummy_heat
           
C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
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

      logical bDebug
      parameter (bDebug = .false.)

C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real shtfld                 ! ESRU function returning the
                                  !   specific heat of air, water vapor,
                                  !   or water.
                                  
C----------------------------------------------------------------------
C     Warn user that annex 42 model may not be completely stable
C----------------------------------------------------------------------     
      if ( .not. bStatus_Warned ) then

         write (ITU,*) " "
      
         write (ITU,*)
     &      "WARNING: Annex 42 Stirling CHP model"
         write (ITU,*)
     &      "         While the source code associated with"
         write (ITU,*)
     &      "         the Stirling engine model is stable,"
         write (ITU,*)
     &      "         development and validation of the model"
         write (ITU,*)
     &      "         is not yet complete.  Its inclusion in"
         write (ITU,*)
     &      "         the ESP-r distribution does not guarentee"
         write (ITU,*)
     &      "         its accuracy, and predictions should be"
         write (ITU,*)
     &      "         used with caution."

         write (ITU,*) " "

         write (ITU,*)
     &      "         Annex 42 is scheduled to complete model"
         write (ITU,*)
     &      "         development and validation activities in"
         write (ITU,*)
     &      "         the spring of 2006, and it is anticipated"
         write (ITU,*)
     &      "         that a final version of the Annex 42 "
         write (ITU,*)
     &      "         Stirling engine model will be available at"
         write (ITU,*)
     &      "         this time."

         write (ITU,*) " "
     
         bStatus_Warned = .true.   
    
      endif      
C----------------------------------------------------------------------
C     Initialization
C----------------------------------------------------------------------

      if (bDebug) write(ITU,*)
     &   "Entering subroutine Stirling_thermal_char"
      
      
C.....Get timestep duration (seconds)
      fTS_duration = 3600. / ( ntstep * ntstpp ) 

      if ( .not. bInitialized ( iC_Index ) ) then
         
C........Collect time-invariant data in ADATA, BDATA arrays
         call Stirling_Collect_Data_dyn ( iC_Index )

C........Initialize Stirling engine module for H3K reports
         
         call Stirling_H3Kreports_module ( iC_Index,
     &                                     iInitialize )

C........Number of additional output (pcdatp/pcdatf) array
C........locations used
         nAPDat (iC_Index) = 5
     
C........Set flag to ensure initialization is not repeated
         bInitialized ( iC_Index ) = .true.

      endif

C------------------------------------------------------------------------
C     Is this the first timestep of the simulation?
C------------------------------------------------------------------------
      if ( nsinc .eq. 1 ) then
C------------------------------------------------------------------------
C        PCDatP array has not been populated. Set initial starting mode
C        to 'inoperative'
C------------------------------------------------------------------------
         iOperating_mode_P = iOP_inoperative             ! (-)
         iSystem_Status    = iStatus_normal              ! (-)
         fOP_time = 0.0                                  ! (s)
         
C........Mass & Energy flow initial conditions for transient behavior         
         fP_net_init          = 0.0                      ! (W)
         fQ_trans_to_water_P  = 0.0                      ! (W)
         fN_fuel_flow_init    = 0.0                      ! (kg/s)

      else
C------------------------------------------------------------------------
C        Collect historical operating mode data from PCDatP arrays:
C------------------------------------------------------------------------
         iOperating_mode_P    = int(PCDatP(iC_Index, 1))   ! (-)
         iSystem_Status       = int(PCDatP(iC_Index, 2))   ! (-)
         fOP_time             = PCDATP(iC_Index, 3)        ! (s)

C........Mass & Energy flow initial conditions for transient behavior
         fN_fuel_flow_init    = PCDATP(iC_Index, 4)        ! (kg/s)
         fP_net_init          = PCDATP(iC_Index, 5)        ! (W)
         fQ_cogen_init        = PCDATP(iC_Index, 6)        ! (W)
         fQ_cogen_P           = PCDATP(iC_Index, 7)        ! (W)
         fQ_trans_to_water_P  = PCDATP(iC_Index, 8)        ! (W)
        
         fCW_temperature_in_P = PCDATP(iC_Index, 9)        ! (oC)
         fCW_mass_flow_P      = PCDATP(iC_Index,10)        ! (kg/s)

      endif

       
C------------------------------------------------------------------------
C     Determine component location in plant network topology, and
C     mark component node for iteration
C
C     -> Get node indicies, and mark nodes for iteration
C
C         NPCDAT(i,9):
C          row and column number defining location of component `i'
C          sub-matrix template in the overall plant network matrix. It is
C          used to identify the location of the current component's nodes
C          within the global plant matrix.
C
C         ICSV(i,j)
C          flag indicating that node `i' is marked for iteration for state
C          variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C          j=3 for 2nd phase mass flow.
C
C         CSVI(i,j)
C          initial value for judging whether iteration required. Same
C          indices as ICSV.
C
C         CSVF(i,j)
C          future time-row solution variable for plant. Same indices
C          as ICSV.
C----------------------------------------------------------------------
      iEngine_Node_index = NPCDAT ( iC_Index, 9 )
      iHX_Node_index = iEngine_Node_index + 1 

      if ( iDynamic_thermal_flag(iC_Index)
     &       .eq. iThermal_mass_model ) then
C........Iteration on engine temperature is only req'd for thermal
C........mass model
         icsv( iEngine_Node_index, 1 ) = 1
         csvi( iEngine_Node_index, 1 ) = csvf( iEngine_Node_index, 1 )
      endif

      icsv( iHX_Node_index, 1 ) = 1
      csvi( iHX_Node_index, 1 ) = csvf( iHX_Node_index, 1 )
      


C.....Cooling water connection index
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iCW_conn_index = icondx ( iC_Index, 2, 1 )

C.....Get cooling water temperature & mass flow rate (future row)
      fCW_temperature_in_F = convar ( iCW_conn_index, iPropTemp )   ! oC
      fCW_mass_flow_F =   convar ( iCW_conn_index, iProp1stFlow )   ! kg/s



      
C----------------------------------------------------------------------
C     If this is the first timestep, set present time row
C     cooling water inlet temperature and mass flow rate to
C     20oC and 0kg/s (esp-r defaults)
C----------------------------------------------------------------------
      
      if ( nsinc .eq. 1 ) then
         
         fCW_temperature_in_P = 20.0  ! oC
         fCW_mass_flow_P      = 0.0   ! kg/s
      
      endif 
C----------------------------------------------------------------------
C     Get temperature of cooling water at outlet of engine heat
C     exchangers. Note, this will be the temperature calculated
C     during the last time step. This data is used by the unit's
C     internal 'overheat protection' control.
C----------------------------------------------------------------------
      fCW_temperature_out_P = CSVP( iHX_Node_index, 1 )           ! oC

C----------------------------------------------------------------------
C     Get temperature of engine node at end of last timestep 
C----------------------------------------------------------------------
      fEngine_Temp_P = CSVP( iEngine_Node_index, 1 )              ! oC

      
C.....Check to see if cooling water flow is non-zero
      call eclose ( fCW_mass_flow_F, 0.0,
     &              fClose_to_zero_tolerance , bNo_cooling_water )

C.....Is cooling water exhaust temp. less than max safe temp?
      if ( fCW_temperature_out_P .ge.
     &     fCW_temperature_MAX(iC_Index) ) then
         
         bCooling_W_ToHot = .true.
         
      else
      
         bCooling_W_ToHot = .false.
      
      endif
     
C.....Calculate cooling water heat capacity:
C.....  specific heat capactiy = Shtfld ( X , Temperature ) (J/kg oC)
C.....  where X=1 -> air, X=2 -> vapor, X=3 -> liq. water

      fCW_heat_cap_F = fCW_mass_flow_F
     &                  * SHtFld ( 3, fCW_temperature_in_F ) ! W/ K
      fCW_heat_cap_P = fCW_mass_flow_P
     &                  * SHtFld ( 3, fCW_temperature_in_P ) ! W/ K

     
C.....Collect containing room temperature
      fTemp_Room_F = PCnTmF ( iC_Index )  ! oC
      fTemp_Room_P = PCnTmP ( iC_Index )  ! oC
      

C----------------------------------------------------------------------
C     High-level control interfaces:
C
C     -> check first control variable to determine if controller has
C        (i) deactivated engine, or is using (ii) the dimensionless
C        interface, (iii) the electrical load following interface, or
C        (iv) the thermal load following interface.
C
C     -> collect control signal from second control variable.
C
C----------------------------------------------------------------------

C.....Collect control signals:
C..... -> Flag indicating which control interface is in use:
      iControl_method = int ( CData ( iC_Index, 1 ) )
C..... -> Control signal
      fControl_signal =  CData ( iC_Index, 2 )

      if ( iControl_method .eq. iEngine_deactivated ) then
C........Controller has deactivated the engine.
         bControl_Active = .false.
      else
         bControl_Active = .true.
      endif

      if ( iControl_method .eq. iFuel_flow_controlled ) then
C........If fuel flow control interface is active, control signal
C........must lie between 0 and 1
         if ( fControl_signal .gt. 1.0 ) then
            fN_fuel_flow = fFuel_Flow_MAX (iC_Index ) ! kmol/s
         elseif ( fControl_signal .lt. 0.0 ) then
            fN_fuel_flow = fFuel_Flow_MIN ( iC_Index )! kmol/s
         else
            fN_Fuel_Flow = fFuel_Flow_MIN (iC_Index ) +
     &         fControl_signal * (
     &            fFuel_Flow_MAX (iC_Index)
     &          - fFuel_Flow_MIN (iC_Index)
     &         )
         endif

      elseif ( iControl_method .eq. iPower_out_controlled .or.
     &         iControl_method .eq. iHeat_out_controlled ) then

C........Check that control signal is not less than zero.
         if ( fControl_signal .lt. 0 ) fControl_signal = 0.0

C........Assign control signal directly to target operating point
         fOP_target = fControl_signal  ! Watts, electric or thermal.

      else
C........Controller has requested an unsupported control mode. Deactivate
C........unit and issue warning.
         bControl_Active = .false.

         ! Warning goes here....

      endif
      
C----------------------------------------------------------------------
C     Low-level controls
C
C     Determine which state the component is in.
C
C     -> Make sure cooling water is flowing, and that cooling water
C        outlet temperature does not exceed maximum permissible
C        cooling water temperature. NOTE: the calculated value of
C        the cooling water outlet temperature from the last time-step
C        is used.
C
C-------------------------------------------------------------------
C----------------------------------------------------------------------
C     Determine desired 'target' operating mode:
C     Is it safe to operate system?
C----------------------------------------------------------------------
      if ( bNo_cooling_water .or. bCooling_W_ToHot ) then
C----------------------------------------------------------------------
C        Overheating danger exists. Initiate shutdown.
C----------------------------------------------------------------------
         iSystem_Status = iStatus_CW_fault
      else
C----------------------------------------------------------------------
C        Check to see if power has been requested.
C----------------------------------------------------------------------
         if ( bControl_Active ) then
C...........Power requested. - unit will operate
            iTarget_mode = iOP_normal_operation
         else
C...........No power requested. Turn unit off.
            iTarget_mode = iOP_inoperative
         endif

C-------------------------------------------------------------------
C        Check if cooling water fault exists, and clear in necessary
C-------------------------------------------------------------------
         if ( iSystem_Status .eq. iStatus_CW_fault )  then

               iSystem_Status = iStatus_normal

         endif 

      endif



C----------------------------------------------------------------------
C     Use function Determine_Cogen_OP to determine the fraction of
C     the current timestep the system will spend in each operating
C     mode as it moves from its current to target operating points.
C----------------------------------------------------------------------

      call Determine_Cogen_Mode(
     &              iC_Index,
     &              iOperating_mode_P,
     &              iSystem_Status,
     &              fOP_time,
     &              iTarget_Mode,
     &              iOperating_mode_F,
     &              fTS_mode_fractions,
     &              bTS_modes,
     &              bStartup_complete,
     &              bDebug              )


C----------------------------------------------------------------------
C     Check to see if the unit will operate in normal operating mode,
C     and characterize behavior using empirically-derived correlations
C     if necessary.
C
C----------------------------------------------------------------------
      if ( .not. bTS_modes (iOP_normal_operation) ) then
C........Engine is not operating in this mode. Set modes' fuel flows,
C........power & heat generation to zero.
         fPeriod_AC_Power
     &          ( iC_Index, iOP_normal_operation ) = 0.0
         fPeriod_Heat_Recovery
     &          ( iC_Index, iOP_normal_operation ) = 0.0
         fPeriod_fuel_flow
     &          ( iC_Index, iOP_normal_operation ) = 0.0

      else
C----------------------------------------------------------------------
C        Engine is operating during this timestep. Characterize
C        performance.
C
C        Check which control interface is currently being used, and
C        apply the appropriate explicit/implicit solution scheme as
C        required:
C
C        CASE:
C          - Power output specificed   -> implicitly solve model
C          - Heat output specified     -> implicitly solve model
C          - Fuel flow specified       -> solve model directly
C
C        Has high-level controller specified system power or
C        heat output?
C----------------------------------------------------------------------
         if ( iControl_Method .eq. iPower_out_controlled .or.
     &        iControl_Method .eq. iHeat_out_controlled       ) then
C----------------------------------------------------------------------
C           The controller has specified the system's electrical or
C           thermal output. The model must be solved implicitly to
C           determine the fuel flow corresponding to the desired
C           operating point.
C----------------------------------------------------------------------
            call Solve_Cogen_Model (
     &           iC_Index,
     &           iModel_Type,
     &           iControl_Method,
     &           fOP_target,
     &           fN_fuel_flow,
     &           fCW_temperature_in_F,
     &           fCW_temperature_in_F,
     &           fTemp_Room_P,
     &           fDummy_time,
     &           fDummy_fuel,
     &           fDummy_heat )


         else
         
C...........Controller has specified fuel flow. There's no need
C...........to explicitly solve model --- do nothing.

         endif


C----------------------------------------------------------------------
C        Check that fuel flow is within maximum / minimum bounds
C----------------------------------------------------------------------
         if (fN_fuel_flow .lt. fFuel_Flow_MIN (iC_Index))
     &         fN_fuel_flow = fFuel_Flow_MIN (iC_Index)
         if (fN_fuel_flow .gt. fFuel_Flow_MAX (iC_Index))
     &         fN_fuel_flow = fFuel_Flow_MAX (iC_Index)

C----------------------------------------------------------------------
C        Ensure that change in fuel flow over this timestep does
C        not exceed the maximump permissible rate of change.
C
C        Use function Eval_TS_average to determine the maximum
C        permissible change in the fuel flow over the timestep, the
C        fuel flow value at the end of the time-step, and the
C        average fuel flow rate over the time-step.
C----------------------------------------------------------------------
         call Eval_TS_average (
     &           fN_fuel_flow_init,
     &           fN_fuel_flow,
     &           fFuel_Flow_Change_MAX(iC_Index),
     &           fTS_duration
     &                * fTS_mode_fractions( iOP_normal_operation),
     &           fN_fuel_flow_Trans_average,
     &           fN_fuel_flow_TRans_final   )
C----------------------------------------------------------------------
C        Use the function Eval_correlations to characterize mass and
C        energy flows at end of time-step    
C----------------------------------------------------------------------
         call Eval_Stirling_Corr(
     &                 iC_Index,
     &                 fN_fuel_flow_trans_final,
     &                 fTemp_Room_P,
     &                 fCW_mass_flow_F,
     &                 fCW_temperature_in_F,
     &                 fQ_skin_losses,
     &                 fQ_avail_LHV,
     &                 fP_net,
     &                 fQ_cogen )

C----------------------------------------------------------------------
C        Make sure that power output does not exceed maximum
C        permissible rate of change over the timestep. Again,
C        Use function Eval_TS_average to determine the maximum
C        permissible change in the power over the timestep, the
C        power at the end of the time-step, and the average 
C        power over the time-step.
C----------------------------------------------------------------------

         call Eval_TS_average (
     &           fP_net_init,
     &           fP_net,
     &           fGross_Power_Change_MAX(iC_Index),
     &           fTS_duration
     &               * fTS_mode_fractions (iOP_normal_operation),
     &           fP_net_Trans_average,
     &           fP_net_Trans_final   )

C----------------------------------------------------------------------
C        Use function Eval_TS_average to determine the average
C        rate of steady-state cogeneration heat recovery. Note:
C        the change in the average rate of steady state heat
C        recovery is not rate limited. A value of '-1' is passed
C        to Eval_TS_average() to disable rate limiting.
C----------------------------------------------------------------------

         call Eval_TS_average (
     &           fQ_cogen_init,
     &           fQ_cogen,
     &           -1.0,
     &           fTS_duration
     &               * fTS_mode_fractions (iOP_normal_operation),
     &           fQ_cogen_Trans_average,
     &           fQ_cogen_Trans_final   )

     
C----------------------------------------------------------------------
C        Assign fuel flow power / heat generation to cetc_cogen
C        arrays
C----------------------------------------------------------------------
         fPeriod_fuel_flow
     &      ( iC_Index, iOP_normal_operation )
     &    = fN_fuel_flow_Trans_average
         fPeriod_AC_Power
     &      ( iC_Index, iOP_normal_operation )
     &    = fP_net_Trans_average
         fPeriod_Heat_Recovery
     &      ( iC_Index, iOP_normal_operation )
     &     = fQ_cogen_Trans_average

      endif 


C----------------------------------------------------------------------
C     Zero model aggregate energy and mass flows.
C----------------------------------------------------------------------
      fN_fuel_flow_average  = 0.0   ! (kg/s)
      fP_net_average        = 0.0   ! (W)
      fQ_Cogen_average      = 0.0   ! (W)

C----------------------------------------------------------------------
C     Loop through each operating mode, and scale energy and mass
C     flows by fraction of time-step spent in corresponding mode.
C----------------------------------------------------------------------

      do iMode = 1, iOP_Mode_Count

         fP_net_average = fP_net_average
     &      + fTS_mode_fractions(iMode)
     &      * fPeriod_AC_Power(iC_Index,iMode)

         fN_fuel_flow_average = fN_fuel_flow_average
     &      + fTS_mode_fractions(iMode)
     &      * fPeriod_fuel_flow(iC_Index,iMode)

         fQ_Cogen_average = fQ_Cogen_average
     &      + fTS_mode_fractions(iMode)
     &      * fPeriod_Heat_Recovery(iC_Index,iMode)

      enddo

C----------------------------------------------------------------------
C     Save instantaneous fuel/heat/power flows at end of timestep 
C----------------------------------------------------------------------
      if ( iOperating_mode_F .eq. iOP_normal_operation ) then
C..........In normal operating mode, the flows may have changed
C..........during operating mode. Use final state calculated by
C..........Eval_TS_average()
           fP_net_final = fP_net_trans_final
           fQ_cogen_final = fQ_cogen_trans_final
           fN_fuel_flow_final = fN_fuel_flow_trans_final

      else
C..........In all other operating modes, the flows are constant
C..........over operating mode.
           fP_net_final = fPeriod_AC_Power(iC_Index, iOperating_mode_F)
           fQ_cogen_final =
     &          fPeriod_Heat_Recovery(iC_Index, iOperating_mode_F)
           fN_fuel_flow_final =
     &          fPeriod_fuel_flow(iC_Index, iOperating_mode_F)

      endif


C.....Save average heat transfer to water as future-time-row variable.
      fQ_cogen_F = fQ_Cogen_average

C-------------------------------------------------------------------
C     Estimate time constant using present time row data
C
C                     Effective heat capacity
C     time constant = ------------------------  (s)
C                     connection heat capacity
C
C
C     Note: if the heat lag model is in use, the effective heat
C           capacity of the Stirling engine is set to zero in
C           subrougine Stirling_Collect_Data.
C
C-------------------------------------------------------------------
C.....Ensure denominator will not be zero!
      call eclose ( (fCW_heat_cap_P + fCW_heat_cap_F ) / 2.0 ,
     &               0.0, 1.0E-03, bNums_are_close )

      if ( bNums_Are_Close ) then
C........Time constant is arbitrarly large:

         fTime_Constant = 10.0 * fTS_duration

      else
         fTime_Constant = ( fEffective_MC_engine(iC_index) +
     &                      fEffective_MC_HX(iC_Index) )
     &    / ( ( fCW_heat_cap_P + fCW_heat_cap_F ) / 2.0 )
      endif

C.....Save time constant in ESRU commonblock (why?)
      TC(iC_index) = fTime_Constant

C-------------------------------------------------------------------
C     Determine explicit/implicit weighting factor (alpha).
C
C     -> weighting factor determines if the forward difference
C        solution of the energy balance differential equation
C       (alpha = 1), the backwards differewce solution (alpha = 0),
C        or some weighted average of the two (0 < alpha < 1) should
C        be used.
C
C     See Equation 68 in Ferguson 2005
C
C-------------------------------------------------------------------
      if ( impexp .eq. 1 ) then
C........Fully implicit solution has been specified by the user
         fSoln_alpha = 1.0

      elseif (impexp .eq. 2 ) then
C........Mixed  solution scheme. Use provided weighting factor ratimp
         fSoln_alpha = ratimp

      elseif (impexp .eq. 3 ) then
C........Compare simulation timestep with component's estimated
C........time consant, and revert to fully implicit solution
C........if timesteps are too large to accuartely characterize
C........component
         if ( fTS_duration .gt. 0.63 * fTime_Constant ) then

            fSoln_alpha = 1.0

         else

            fSoln_alpha = ratimp

         endif

      elseif ( IMPEXP .EQ. 4 ) then
C.......'steady state' solution. This feature is untested, and
C........will quite possibly cause spectacular errors in
C........the simulation. A warning is issued during the
C........error trapping procedure.

      else
C........Shouldn't happen, as impexp is error trapped elsewhere?
         STOP 'Stirling_Thermal_Char: Solution scheme is unsupported'
      endif

C----------------------------------------------------------------------
C     Check which dynamic themral model has been specified, and
C     assign ESP-r plant temperature solution matrix coefficients
C     accordingly.
C----------------------------------------------------------------------      
      if ( iDynamic_thermal_flag (iC_index ) .eq.
     &     iThermal_mass_model ) then
C----------------------------------------------------------------------
C        Thermal mass mode is in use.
C
C        ESP-r matrix has the following form:
C
C           |  a11  a12   -  | |  T_{sys} |     | R1 |
C           |  a21  a22  a23 | |  T_{HX}  |  =  | R2 |
C                              | T_{cw,i} |
C
C        First self-coupling coefficient:
C
C                 Engine heat capacity 
C           a11 = --------------------
C                  time step duration
C
C                   + alpha * ( HX UA value + UA loss value )
C
C----------------------------------------------------------------------

         fCoefficients(1) =
     &  
     &   fEffective_MC_engine(iC_index) / fTS_duration
     &
     &   + fSoln_alpha * (
     &       fEffective_UA_HX(iC_Index) + fEffective_UA_loss(iC_Index))

C----------------------------------------------------------------------
C        Second self-coupling coefficient:
C
C            a12 = - alpha * HX UA value
C
C----------------------------------------------------------------------
         
         fCoefficients(2) = -1.0 * fSoln_alpha
     &                           * fEffective_UA_HX(iC_Index)

C----------------------------------------------------------------------
C        Third self-coupling coefficient:
C
C           a21 = - alpha * HX UA value
C
C----------------------------------------------------------------------

         fCoefficients(3) = -1.0 * fSoln_alpha
     &                           * fEffective_UA_HX(iC_Index)

C----------------------------------------------------------------------
C        Fourth self-coupling coefficient:
C
C                  HX heat capacity
C           a22 = ------------------  +
C                 time step duration
C
C                    alpha * ( HX UA value + cooling water heat capacity )
C
C----------------------------------------------------------------------

         fCoefficients(4) =
     &
     &        fEffective_MC_HX(iC_Index) / fTS_duration
     & 
     &      + fSoln_alpha * ( fEffective_UA_HX(iC_Index)
     &                                      + fCW_heat_cap_F )


C----------------------------------------------------------------------     
C        Cross coupling coefficient:
C
C           a23 = - alpha * cooling water heat capacity
C
C----------------------------------------------------------------------

         fCoefficients(5) = -1.0 * fSoln_alpha * fCW_heat_cap_F

C----------------------------------------------------------------------         
C        Present-time & known coefficient 1:
C
C                Engine heat capacity 
C           R1 = -------------------- * Present engine temperature
C                 Time step duration
C
C                   + ( 1 - alpha ) *
C                        ( HX UA Value *
C                              ( present HX temp - present engine temp )
C                           + Present cogen heat )
C
C                   + alpha * Future cogen heat
C
C----------------------------------------------------------------------
         
         fCoefficients(6) =
     &
     &       fEffective_MC_engine(iC_Index) / fTS_duration
     &          * fEngine_Temp_P
     &
     &    + ( 1.0 - fSoln_alpha )
     &
     &         * ( fEffective_UA_HX ( iC_Index )
     &              * ( fCW_temperature_out_P - fEngine_Temp_P )
     &         
     &             + fQ_cogen_P
     &
     &             + fEffective_UA_loss ( iC_Index ) *
     &                  ( fTemp_Room_P - fEngine_Temp_P )
     &            )
     &
     &    + fSoln_alpha * ( fQ_cogen_F + fEffective_UA_loss ( iC_Index )
     &                                     * fTemp_Room_F )




C----------------------------------------------------------------------         
C        Present-time & known coefficient 2:
C
C                  HX heat capacity
C           R1 = -------------------- * Present engine temperature
C                 Time step duration
C
C                   + ( 1 - alpha ) *
C
C                        ( HX UA Value *
C                              ( present engne temp - present HX out. temp )
C
C                          + Present cooling water heat capacity *
C                              ( present CW inlet temp
C                                      - present cw outlet temp )
C                                
C----------------------------------------------------------------------
         fCoefficients(7) =
     &
     &      fEffective_MC_HX ( iC_Index ) / fTS_duration
     &                                    * fCW_temperature_out_P
     &
     &    + ( 1.0 - fSoln_alpha ) * (
     &
     &         fEffective_UA_HX ( iC_Index )
     &               * ( fEngine_Temp_P - fCW_temperature_out_P )
     &
     &       + fCW_heat_cap_P
     &               * ( fCW_temperature_in_P - fCW_temperature_out_P )
     & 
     &       )

          
      elseif ( iDynamic_thermal_flag(iC_Index) .eq.
     &         iHeat_lag_model ) then
C----------------------------------------------------------------------
C        Heat lag model is in use
C
C        Calculate future time row heat transfer using exponential
C        decay term.
C
C        Q future = (Q cogen future)
C
C                + (Q present - Q cogen future ) exp ( - a * delta t )
C
C----------------------------------------------------------------------
         fQ_trans_to_water_F = fQ_cogen_F
     &       + ( fQ_trans_to_water_P - fQ_cogen_F )
     &       *  exp ( -1.0 * fHeat_lag_TC ( iC_index ) * fTS_duration )

C----------------------------------------------------------------------
C
C        ESP-r matrix has the following form:
C
C           |   1    0    -  | |  T_{sys} |     |  0 |
C           |   0   a22  a23 | |  T_{HX}  |  =  | R2 |
C                              | T_{cw,i} |
C
C        Note: T_{sys} is only used in the thermal mass model, and does not
C              really represent anything in the heat lag model. Therefore,
C              it is set to zero. 
C
C        First self-coupling coefficient:
C
C
C           a11 = 1.0
C
C----------------------------------------------------------------------

         fCoefficients(1) = 1.0

C----------------------------------------------------------------------
C        Second self-coupling coefficient:
C
C            a12 = 0.0
C
C----------------------------------------------------------------------
         
         fCoefficients(2) = 0.0

C----------------------------------------------------------------------
C        Third self-coupling coefficient:
C
C           a21 = 0.0
C
C----------------------------------------------------------------------

         fCoefficients(3) = 0.0

C----------------------------------------------------------------------
C        Fourth self-coupling coefficient:
C
C                  HX heat capacity
C           a22 = ------------------  +
C                 time step duration
C
C                    alpha * ( cooling water heat capacity )
C
C----------------------------------------------------------------------

         fCoefficients(4) =
     &
     &        fEffective_MC_HX(iC_Index) / fTS_duration
     & 
     &      + fSoln_alpha * fCW_heat_cap_F 


C----------------------------------------------------------------------     
C        Cross coupling coefficient:
C
C           a23 = - alpha * cooling water heat capacity
C
C----------------------------------------------------------------------

         fCoefficients(5) = -1.0 * fSoln_alpha * fCW_heat_cap_F

C----------------------------------------------------------------------         
C        Present-time & known coefficient 1:
C
C           R1 = 0.0
C
C----------------------------------------------------------------------
         
         fCoefficients(6) = 0.0

C----------------------------------------------------------------------         
C        Present-time & known coefficient 2:
C
C                  HX heat capacity
C           R1 = -------------------- * Present HX temperature
C                 Time step duration
C
C                   + ( 1 - alpha ) *
C
C                        ( Present cooling water heat capacity *
C                              ( present CW inlet temp
C                                      - present cw outlet temp )
C
C                          + Present heat transfer to water )
C
C                   + alpha * ( future heat transfer to water )
C                                
C----------------------------------------------------------------------
         fCoefficients(7) =
     &
     &      fEffective_MC_HX ( iC_Index ) / fTS_duration
     &                                    * fCW_temperature_out_P
     &
     &    + ( 1.0 - fSoln_alpha ) * (
     &
     &       fCW_heat_cap_P
     &               * ( fCW_temperature_in_P - fCW_temperature_out_P )
     &
     &       + fQ_trans_to_water_P
     & 
     &       )
     &
     &    + fSoln_alpha * fQ_trans_to_water_F

      
      endif 

C---------------------------------------------------------------------
C     Save timerow data for use on the next time step
C---------------------------------------------------------------------
      PCDatF(iC_Index, 1)      = float(iOperating_mode_F)    ! (-)
      PCDatF(iC_Index, 2)      = float(iSystem_Status)       ! (-)
      PCDATF(iC_Index, 3)      = fOP_time                    ! (s)

C.....Mass & Energy flow initial conditions for transient behavior
      PCDatF(iC_Index, 4)      =  fN_fuel_flow_final    ! (kg/s)
      PCDatF(iC_Index, 5)      =  fP_net_final          ! (W)
      PCDatF(iC_Index, 6)      =  fQ_cogen_final        ! (W)
      PCDatF(iC_Index, 7)      =  fQ_cogen_F            ! (W)
      PCDatF(iC_Index, 8)      =  fQ_trans_to_water_F   ! (W)

      PCDatF(iC_Index, 9)      =  fCW_temperature_in_F  ! (oC)
      PCDatF(iC_Index,10)      =  fCW_mass_flow_F       ! (kg/s)



C-----------------------------------------------------------------------
C     Electrical network related operations ---
C
C     1. Save real power output in electrical network common block
C        variable
C     2. Call subroutine EMACH to (i) calculate the reactive component
C        of the power produced by the network, and (ii) save this data
C        in the electrical network common block
C        EMACH arguements     
C            IPCOMP,                    Component number (input)
C            IEMODEL,                   power calculation flag (input)
C            pwrp(iC_index),    Real power (input, W)
C            PQ,                        Complex power (output, W)
C            PA)                        Overall power (output, W)
C-----------------------------------------------------------------------

C.....Real power output (W), generation is +ive...
      pwrp(iC_index) = fP_net_average
      IEMODEL=1              ! Flag indicating which power calculation
                             ! will be performed                            
      CALL EMACH(
     &     iC_index,       ! Component number (input)
     &     IEMODEL,                ! power calculation flag (input)
     &     pwrp(iC_index), ! Real power (input, W)
     &     PQ,                     ! Complex power (output, W)
     &     PA)                     ! Overall power (output, W)


C----------------------------------------------------------------------
C     Save reportable data in commons for use at end of simulation
C     timestep. 
C
C     -> Report instantaneous data describing system operation at
C        end of the timestep.
C----------------------------------------------------------------------
C.....Engine status 
      bReport_unit_active(iC_index) = bEngine_Active

C.....Control
      fReport_Control_Method (iC_index ) =
     &                              float (iControl_method)
     
      fReport_Control_Signal (iC_index) = fControl_signal
                                  
C.....Net power
      fReport_Net_Power(iC_index)     = fP_net_final          ! (W)
C.....Heat recovery      
      fReport_Heat_Recovery(iC_index) = fQ_Cogen_final ! (W)

C.....Heater skin losses & containment temperature 
!      fReport_Heat_Loss(iC_index) = fQ_skin_losses_final      ! (W)
      fReport_Containment_Temp(iC_index) = fTemp_Room_P         ! (oC)


C.....Fuel flow, and rate of gross heat input
      fReport_Fuel_Flow(iC_index) = fN_fuel_flow_final        ! (kg/s)
      fReport_Fuel_LHV_rate(iC_index) =
     &         fN_fuel_flow_final * fFuel_LHV(iC_index)       ! (W)
      
C.....Electric, thermal & cogeneration efficiencies --- Note that
C.....efficiencies can only be calculated if fuel is being used
C.....by the system
      call eclose (fN_fuel_flow_final, 0.0,
     &             fClose_to_zero_tolerance, bNums_are_close)
      if ( .not. bNums_are_close .and. fN_fuel_flow_final .gt. 0) then
C----------------------------------------------------------------------
C        Calculate instantaneous efficiencies:
C
C                               useful output
C          efficiency = -------------------------------
C                        energy value of fuel used (LHV)
C
C----------------------------------------------------------------------      
         
         fReport_Electric_Efficiency(iC_index) = fP_net_final
     &       / ( fN_fuel_flow_final * fFuel_LHV(iC_index) ) !(-)
         
         fReport_Thermal_Efficiency(iC_index)=
     &          fQ_Cogen_final
     &       / ( fN_fuel_flow_final * fFuel_LHV(iC_index) )
         
         fReport_Cogen_efficiency(iC_index)=
     &         ( fP_net_final + fQ_Cogen_final )
     &       / ( fN_fuel_flow_final * fFuel_LHV(iC_index))  ! (-)

      else 
         fReport_Electric_Efficiency(iC_index)  = 0.0
         fReport_Thermal_Efficiency(iC_index)   = 0.0
         fReport_Cogen_Efficiency(iC_index)     = 0.0
      endif
      
C.....Cooling water temperature & flow rate
      fReport_CW_Mass_Flow(iC_index)   = fCW_mass_flow_F      ! (kg/s)
      fReport_CW_Temperature(iC_index) = fCW_temperature_in_F ! (kg/s)

C----------------------------------------------------------------------
C     -> Report averaged data describing system operation over
C        entire timestep. Note: it is only possible to report
C        averaged data for the variables for which transient
C        characteristics are available (Net power, fuel flow,
C        heat output.)
C----------------------------------------------------------------------
      fAverage_Heat_Recovery(iC_index)
     &    = fQ_Cogen_average
      fAverage_Net_Power(iC_index)     = fP_net_average
      fAverage_Fuel_Flow(iC_index)     = fN_fuel_flow_average
      fAverage_Fuel_LHV_rate(iC_index) = fN_fuel_flow_average
     &    * fFuel_LHV(iC_index)

C.....Electric, thermal & cogeneration efficiencies --- Note that
C.....efficiencies can only be calculated if fuel is being used
C.....by the system
      call eclose (fN_fuel_flow_average, 0.0,
     &             fClose_to_zero_tolerance, bNums_are_close)
     
      if ( .not. bNums_are_close .and. fN_fuel_flow_average.gt.0) then
         
         fAverage_Electric_Efficiency(iC_index)= fP_net_average
     &       / ( fN_fuel_flow_average * fFuel_LHV(iC_index) ) !(-)
         
         fAverage_Thermal_Efficiency(iC_index) =
     &          fQ_Cogen_average
     &       / ( fN_fuel_flow_average * fFuel_LHV(iC_index) ) !(-)
         
         fAverage_Cogen_efficiency(iC_index)=
     &         ( fP_net_average + fQ_Cogen_average )
     &       / ( fN_fuel_flow_average * fFuel_LHV(iC_index))  ! (-)

      else
         fAverage_Electric_Efficiency(iC_index)  = 0.0
         fAverage_Thermal_Efficiency(iC_index)   = 0.0
         fAverage_Cogen_Efficiency(iC_index)     = 0.0
      endif
C.....Fraction of timestep spent in each mode
      do iMode = 1, iOP_Mode_Count
C........Flags indicating which modes unit operated in         
         bReport_TS_modes (iC_index, iMode) = bTS_modes(iMode)  ! (-)
C........Time spent in each mode 
         fReport_time_spent (iC_index, iMode) =
     &          fTS_mode_fractions(iMode) * fTS_duration             ! (s)

      enddo


      if (bDebug) write(ITU,*)
     &   "Exiting subroutine Stirling_thermal_char"
            
     
      return
      end



C------------------- Stirling_Collect_Data ----------------------------
C
C This subrouine collects time-invariant plant component parameters
C stored in ESP-r's adata & bdata arrays, and stores them in the
C common block COMMON/Stirling_coefficients/. This procedure performs
C some rudementry error checking, but would benifit from more comprehensive
C error checking routines.
C
C INPUTS:
C
C  - iComp: index of component in plant network
C  - iModel_Type: flag indicating which model (low-resolution or
C      high resolution) has been specified.
C
C OUTPUTS:
C   - NONE
C
C
C  
C----------------------------------------------------------------------
      subroutine Stirling_Collect_Data_dyn ( iComp )
      implicit none

      INCLUDE 'building.h'       
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'
      INCLUDE 'chemical_properties.h'

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

      integer iComp            ! index of component in plant network
      integer iShift           ! index where empirical coefficients
                               ! are stored

C.....Array containing fuel composition
      real fFuel_composition ( iCompound_count )
      integer ii               ! counter

      real fFuel_fraction      ! 

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu

      logical bNums_are_close_a  ! result of close-to-zero comparisons
      logical bNums_are_close_b  ! result of close-to-zero comparisons
      logical bFatal_error       ! flag indicating error has occurred.      

C.....References
      real fEval_Mixture_HV    ! Function to evaluate a mixture's
                               !   heating value
      real fEval_Mixture_MM    ! Function to evaluate a mixture's
                               !   molar mass
      logical bFlammable_Mixture ! Function determining if a mixture
                               !   can be oxidized.                               

      bFatal_error = .false.
                               
C.....Make sure array is empty
      do ii = 1, iCompound_count
         fFuel_composition (ii) = 0.0
      enddo

C--------------------------------------------------------------------
C     Nominal system size  (kW)
C--------------------------------------------------------------------
      fNominal_Power_Output(iComp) = ADATA ( iComp, 1 )

C--------------------------------------------------------------------
C     Max & minimum fuel flow (kmol/s)
C--------------------------------------------------------------------
      fFuel_Flow_MIN(iComp) = ADATA ( iComp, 2 )
      fFuel_Flow_MAX(iComp) = ADATA ( iComp, 3 )

C.....Error checking
      if ( fFuel_Flow_MIN(iComp) .gt. fFuel_Flow_MAX(iComp) ) then
         write (itu,*) 'Stirling CHP Warning: Specified '
     &        // 'minimum fuel flow rate is larger then specified '
     &        // 'maximum '
         write (itu,*) '                      Switching over maximum '
     &         // ' and minimum flow rates'
         fFuel_Flow_MIN(iComp) = ADATA ( iComp, 3 )
         fFuel_Flow_MAX(iComp) = ADATA ( iComp, 2 )

      endif
C.....Is maximum flow rate close to zero?
      call eClose( fFuel_Flow_MAX(iComp), 0.0, 1.0E-10,
     &             bNums_are_close_a )
      if (bNums_are_close_a ) then
         write (itu,*) 'Stirling CHP Error: Specified '
     &        // 'maximum fuel flow rate is zero!  '
         write (itu,'(A,I2,A,I2)')
     &                 '                 Check misc data item',
     &                 3,' in Component # ', iComp

         bFatal_error = .true.

      endif 
      
C--------------------------------------------------------------------
C     Maximum cooling water outlet temperature oC
C--------------------------------------------------------------------
      fCW_temperature_MAX (iComp) = ADATA(iComp, 4 )

C--------------------------------------------------------------------
C     Fuel composition (mol/mol basis)
C--------------------------------------------------------------------

C.....Check if constituents are between 0 & 1.
      fFuel_fraction = 0.0
      do ii = 5, 15
         call eclose (ADATA(iComp, ii),0.0,0.0001,bNums_are_close_a)
         call eclose (ADATA(iComp, ii),0.0,0.0001,bNums_are_close_b)
         
         if ( ( ADATA(iComp, ii) .lt. 0.0 .OR.
     &          ADATA(iComp, ii) .gt. 1.0     ).AND. .not.
     &        ( bNums_are_close_a .OR. bNums_are_close_b ) ) then
            write (itu,*) 'Stirling CHP Error: Fuel contituent mol'
     &                 // ' fraction must be between 0 & 1.'
            write (itu,'(A,I2,A,I2)')
     &                    '                     Check misc data item',
     &                    ii,' in Component # ', iComp

            bfatal_error = .true.
         endif
         
         fFuel_fraction = fFuel_fraction + ADATA(iComp, ii)
      
      enddo
C.....Do fractions add to 1?      
      call eclose (fFuel_fraction, 1.0, 0.0001,bNums_are_close_a) 


      if ( .not. bNums_are_close_a ) then
C........Warn user
         write(itu,*) 'Stirling CHP Warning: Fuel constituent'
     &                 // ' mole fractions do not add to 1. '
         write(itu,'(A,F8.6,A)')
     &                '                       Normalizing '
     &                 // 'constituents by total (',fFuel_fraction,')'
      endif
      
      fFuel_Composition ( iHydrogen   ) = ADATA ( iComp,  5 )
     &                                    / fFuel_fraction
      fFuel_composition ( iMethane    ) = ADATA ( iComp,  6 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iEthane     ) = ADATA ( iComp,  7 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iPropane    ) = ADATA ( iComp,  8 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iButane     ) = ADATA ( iComp,  9 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iPentane    ) = ADATA ( iComp, 10 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iHexane     ) = ADATA ( iComp, 11 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iC_Monoxide ) = ADATA ( iComp, 12 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iC_Dioxide  ) = ADATA ( iComp, 13 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iNitrogen   ) = ADATA ( iComp, 14 )
     &                                    / fFuel_fraction      
      fFuel_composition ( iOxygen     ) = ADATA ( iComp, 15 )
     &                                    / fFuel_fraction      
      
      iShift = 15
      
C--------------------------------------------------------------------
C     Evaluate fuel lower heating value.
C      -> function fEval_Mixture_HV returns heating value (J/kmol)
C      -> function fEval_Mixture_MM returns molar mass (kg/kmol)
C
C     Note: fEval_Mixture_HV can return both the higher heating
C           value and lower heating value. Use iLHV to specify
C           lower heating value.
C--------------------------------------------------------------------
      fFuel_LHV( iComp ) =
     &             fEval_Mixture_HV ( fFuel_Composition, iLHV )
     &           / fEval_Mixture_MM ( fFuel_Composition )       ! J/kg

C.....IS LHV value greater than zero?
      if ( .not. bFlammable_Mixture ( fFuel_Composition ) ) then
         write(itu,*) 'Stirling CHP Error: Fuel mixture is not '
     &                // ' flammable! '
         write(itu,'(A,I2)') '                     Check fuel '
     &                // ' constituent mol fractions in component # ',
     &                iComp
         bFatal_error = .true.
      endif 

C.....Transient behavior (max rates of change in fuel flow,
C.....electrical & thermal output)
      fFuel_Flow_Change_MAX(iComp)  = ADATA ( iComp, iShift +  1 )
      fGross_Power_Change_MAX(iComp)= ADATA ( iComp, iShift +  2 )

C.....Linear heat recovery rate of change not used in this model      
      fHeat_Recovery_Change_MAX (iComp) = -1.0     

C.....Dynamic heat transfer model: 
      iDynamic_thermal_flag(iComp) = int( ADATA ( iComp, iShift +  3 ))

C.....Heat lag model: heat lag time constant.
C.....   -> This data will only be used if iDynamic_thermal_flag = 1      
      fHeat_lag_TC(iComp)          = ADATA ( iComp, iShift +  4 )
      
C.....Thermal mass model: SPS thermal mass and effective heat
C.....transfer coefficient.
C.....   -> These data will only be used if iDynamic_thermal_flag = 2
      fEffective_MC_engine(iComp)  = ADATA ( iComp, iShift +  5 )
      fEffective_UA_HX(iComp)      = ADATA ( iComp, iShift +  6 )
      fEffective_UA_loss(iComp)    = ADATA ( iComp, iShift +  7 )

C.....Heat exchanger thermal mass
      fEffective_MC_HX(iComp)      = ADATA ( iComp, iShift +  8 )

C.....If heat lag model is in use, set engine thermal mass to zero.
      if ( iDynamic_thermal_flag (iComp) .eq. iHeat_lag_model )
     &     fEffective_MC_engine(iComp) = 0.0      
      
      iShift = iShift + 8

C.....Heat transfer coefficient between SPS and cooling water,
      
C.....Start-up period data
      fPeriod_duration(iComp,iOP_startup)
     &                  = ADATA ( iComp, iShift +  1 )
      fPeriod_fuel_flow(iComp,iOP_startup)
     &                  = ADATA ( iComp, iShift +  2 )
      fPeriod_AC_Power(iComp, iOP_startup)
     &                  = ADATA ( iComp, iShift +  3 )
      fPeriod_Heat_Recovery(iComp,iOP_startup)
     &                  = ADATA ( iComp, iShift +  4 )      
      iShift = iShift + 4

C.....Shutdown period data
      fPeriod_duration(iComp,iOP_shutdown)
     &                  = ADATA ( iComp, iShift +  1 )
      fPeriod_fuel_flow(iComp,iOP_shutdown)
     &                  = ADATA ( iComp, iShift +  2 )
      fPeriod_AC_Power(iComp, iOP_shutdown)
     &                  = ADATA ( iComp, iShift +  3 )
      fPeriod_Heat_Recovery(iComp,iOP_shutdown)
     &                  = ADATA ( iComp, iShift +  4 )
           
      iShift = iShift + 4


C.....Standby mode data
      
      fPeriod_fuel_flow(iComp,iOP_inoperative)
     &                  = ADATA ( iComp, iShift +  1 )
      fPeriod_AC_Power(iComp, iOP_inoperative)
     &                  = ADATA ( iComp, iShift +  2 )
      fPeriod_Heat_Recovery(iComp,iOP_inoperative)
     &                  = ADATA ( iComp, iShift +  3 )
     
      iShift = iShift + 3

      
C.....Correlation reference conditions
      fCorr_REF_CW_temp(iComp)   =  ADATA ( iComp, iShift +  1 )
      fCorr_REF_CW_flow(iComp)   =  ADATA ( iComp, iShift +  2 )
      fCorr_REF_amb_temp(iComp)  =  ADATA ( iComp, iShift +  3 )

      iShift = iShift + 3
      
C--------------------------------------------------------------------
C     General Model correlations
C--------------------------------------------------------------------
      
C.....Skin loss coefficients
      fSkin_loss_C_1(iComp)        = ADATA ( iComp, iShift +  1 )
      fSkin_loss_C_2(iComp)        = ADATA ( iComp, iShift +  2 )
      fSkin_loss_C_3(iComp)        = ADATA ( iComp, iShift +  3 )
      fSkin_loss_C_4(iComp)        = ADATA ( iComp, iShift +  4 )
      fSkin_loss_C_5(iComp)        = ADATA ( iComp, iShift +  5 )

      iShift = iShift + 5
      
C.....Heater efficiency
      fHeat_eff_C_1(iComp)         = ADATA ( iComp, iShift +  1 )
      fHeat_eff_C_2(iComp)         = ADATA ( iComp, iShift +  2 )
      fHeat_eff_C_3(iComp)         = ADATA ( iComp, iShift +  3 )
      fHeat_eff_C_4(iComp)         = ADATA ( iComp, iShift +  4 )
      fHeat_eff_C_5(iComp)         = ADATA ( iComp, iShift +  5 )
      fHeat_eff_C_6(iComp)         = ADATA ( iComp, iShift +  6 )
      fHeat_eff_C_7(iComp)         = ADATA ( iComp, iShift +  7 )

      iShift = iShift + 7
      
C.....Mechanical efficiency correlations
      fElec_eff_C_1(iComp)         = ADATA ( iComp, iShift +  1 )
      fElec_eff_C_2(iComp)         = ADATA ( iComp, iShift +  2 )
      fElec_eff_C_3(iComp)         = ADATA ( iComp, iShift +  3 )
      fElec_eff_C_4(iComp)         = ADATA ( iComp, iShift +  4 )
      fElec_eff_C_5(iComp)         = ADATA ( iComp, iShift +  5 )
      fElec_eff_C_6(iComp)         = ADATA ( iComp, iShift +  6 )
      fElec_eff_C_7(iComp)         = ADATA ( iComp, iShift +  7 )

      iShift = iShift + 7

c.....Correlation bounds
      fCorr_MIN_Q_avail(iComp)   =  ADATA ( iComp, iShift + 1 )
      fCorr_MAX_Q_avail(iComp)   =  ADATA ( iComp, iShift + 2 )
      fCorr_MIN_CW_temp(iComp)   =  ADATA ( iComp, iShift + 3 )
      fCorr_MAX_CW_temp(iComp)   =  ADATA ( iComp, iShift + 4 )
      fCorr_MIN_CW_flow(iComp)   =  ADATA ( iComp, iShift + 5 )
      fCorr_MAX_CW_flow(iComp)   =  ADATA ( iComp, iShift + 6 )
      fCorr_MIN_amb_temp(iComp)  =  ADATA ( iComp, iShift + 7 )
      fCorr_MAX_amb_temp(iComp)  =  ADATA ( iComp, iShift + 8 )

      iShift = iShift + 8
      
      if (bFatal_error) 
     &   stop 'Stirling_Collect_Data (Stirling_CHP.F): Fatal error!'
      
      return
      end

C----------------------- Eval_correlations ----------------------------
C
C Procedure to collect results from empirical correlations and
C aggregate results. Effectively characterizes engine provided
C fuel flow rate is known
C
C Inputs:
C  - Index of component in plant network            --
C  - Fuel flow rate                              (kmol/s)
C  - Room temperature                            (oC)
C  - Cooling water flow rate                     (kg/s)
C  - Cooling water temperature                   (oC)
C
C Outputs
C  - heater skin losses                          (W)
C  - Available heat in heater                    (W)
C  - Net power                                   (W)
C  - Heat recovered for cogeneration             (W)
C
C----------------------------------------------------------------------
      subroutine Eval_Stirling_Corr (
     &                iC_index,
     &                fN_fuel_flow,
     &                fTemp_Room_P,
     &                fCW_mass_flow,
     &                fCW_temperature_in,
     &                fQ_skin_losses,
     &                fQ_avail,
     &                fP_net,
     &                fQ_Cogeneration )
      implicit none
     
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'

C.....Inputs
      integer iC_index    ! Pointer to component in network
      integer iModel_Type         ! Type of stirling engine in use
      real fN_fuel_flow           ! Fuel flow (kmol/s)
      real fTemp_Room_P             ! Room temp (oC)
      real fCW_mass_flow          ! Mass flow (kg/s)
      real fCW_temperature_in     ! Cooling water temp (oC)

C.....Outputs
      real fQ_skin_losses         ! Heat lost from heater (W)
      real fQ_avail               ! Available heat in heater (W)
      real fP_net                 ! Net power production (W)
      real fQ_Cogeneration        ! Heat recovered for cogen (W)

C.....Local variables
      real fEFF_electric          ! net electrical efficiency
      real fEFF_thermal           ! net thermal efficiency
      logical bClose_To_Zero_a    ! flag for close-to-zero testing
      logical bClose_To_Zero_b    ! flag for close-to-zero testing

C.....Low-resolution correlation data adjusted to respect max/min
C.....correlation bounds
      real fCorr_Q_avail          ! Available heat
      real fCorr_T_cw             ! Cooling water temperature
      real fCorr_T_ambient        ! Ambient temperature
      real fCorr_CW_flow          ! Cooling water flow rate. 
      

C.....Close to zero tolerance
      real fClose_to_zero_tol
      parameter ( fClose_to_zero_tol = 1.0E-06 )
      
C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------

C.....Functions evaluating empirical correlations for Stirling model
      real fEval_Skin_Losses
      real fEval_elec_efficiency_LR
      real fEval_heat_efficiency_LR

C-------------------------------------------------------------------
C     Apply Annex 42 system-level model correlations
C-------------------------------------------------------------------

C-------------------------------------------------------------------
C     Calculate gross energy input to system:
C
C        Avail. heat = fuel flow * LHV
C
C-------------------------------------------------------------------
      fQ_avail =  fN_fuel_flow * fFuel_LHV(iC_index)  ! (W)

C-------------------------------------------------------------------
C     Check that correlation inputs maximum/minimum bounds
C-------------------------------------------------------------------
C.....Available heat
      if ( fQ_avail .gt. fCorr_MAX_Q_avail(iC_index) ) then
         fCorr_Q_avail = fCorr_MAX_Q_avail(iC_index)
      elseif ( fQ_avail .lt.
     &         fCorr_MIN_Q_avail(iC_index) ) then
         fCorr_Q_avail = fCorr_MIN_Q_avail(iC_index)
      else
         fCorr_Q_avail = fQ_avail
      endif

C.....Cooling water temperature
      if ( fCW_temperature_in .gt.
     &     fCorr_MAX_CW_temp (iC_index ) ) then
         fCorr_T_cw = fCorr_MAX_CW_temp (iC_index )
      elseif ( fCW_temperature_in .lt.
     &     fCorr_MIN_CW_temp (iC_index ) ) then
         fCorr_T_cw = fCorr_MIN_CW_temp (iC_index )
      else
         fCorr_T_cw = fCW_temperature_in
      endif

C.....Cooling water flow rate
      if ( fCW_mass_flow .gt.
     &     fCorr_MAX_CW_flow (iC_index ) ) then
         fCorr_CW_flow = fCorr_MAX_CW_flow (iC_index )
      elseif ( fCW_mass_flow .lt.
     &     fCorr_MIN_CW_flow (iC_index ) ) then
         fCorr_CW_flow= fCorr_MIN_CW_flow (iC_index )
      else
         fCorr_CW_flow = fCW_mass_flow
      endif

C.....Collect electrical and thermal efficiency
      fEFF_electric = fEval_elec_efficiency_LR( iC_index,
     &                                          fCorr_Q_avail,
     &                                          fCorr_T_CW,
     &                                          fCorr_CW_flow )

      fEFF_thermal  = fEval_heat_efficiency_LR( iC_index,
     &                                          fCorr_Q_avail,
     &                                          fCorr_T_CW,
     &                                          fCorr_CW_flow )



C-------------------------------------------------------------------
C     Determine net power and heat output
C
C        Power = (fuel flow) * (LHV) * (electrical efficency)
C        Heat  = (fuel flow) * (LHV) * (thermal efficency)
C
C     See Equations 29 & 31, Ferguson 2005
C ------------------------------------------------------------------

      fP_net = fEFF_electric * fQ_avail            ! (W)
      fQ_Cogeneration = fEFF_thermal * fQ_avail    ! (W)

      
      return
      end


               
C-------------------- fEval_heat_efficiency_LR --------------------------
C
C Function to evaluate the thermal efficiency of a Stirling engine,
C as per the IEA/ECBCS Annex 42 system-level model
C
C Function form:
C
C    efficiency = C1 + C2 * (fuel flow) + C3 * (fuel flow )^2
C                    + C5 * (CWT -Tref ) + C6 * (CWT -Tref )^2
C                    + C8 * (CW flow - Ref flow.)
C                    + C9 * (CW flow - Ref flow.) ^2
C
C    ( CW = cooling water )
C
C----------------------------------------------------------------------
      real function fEval_heat_efficiency_LR ( iC_index,
     &                                         fQ_avail,
     &                                         fCW_temperature_in,
     &                                         fCW_mass_flow )
      implicit none
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'

      integer iC_index
      real fCW_temperature_in               ! cooling water temperature (oC)
      real fCW_mass_flow                    ! cooling water mass flow rate
                                            ! (kg/s)
      real fQ_avail                         ! gross heat available in heater
                                            ! (W)
C.....Coefficients (used to save space)
      real C1,C2,C3,C4,C5,C6,C7

C.....Reference conditions
      real fTemp_ref, fFlow_Ref

C.....Intermediate data
      real fTemp_diff, fFlow_diff
      
      C1 = fHeat_eff_C_1(iC_index)
      C2 = fHeat_eff_C_2(iC_index)
      C3 = fHeat_eff_C_3(iC_index)
      C4 = fHeat_eff_C_4(iC_index)
      C5 = fHeat_eff_C_5(iC_index)
      C6 = fHeat_eff_C_6(iC_index)
      C7 = fHeat_eff_C_7(iC_index)

C.....Reference conditions
      fTemp_ref = fCorr_REF_CW_temp(iC_index)
      fFlow_ref = fCorr_REF_CW_flow(iC_index)
      
C----------------------------------------------------------------------
C     Difference between actual conditons and reference conditions
C----------------------------------------------------------------------
      fTemp_diff = fCW_temperature_in - fTemp_ref    ! (oC)
      fFlow_diff = fCW_mass_flow      - fFlow_Ref    ! (kg/s)   

      fEval_heat_efficiency_LR = 
     &   C1 + C2 * fFlow_diff + C3 * fFlow_diff ** 2.0
     &      + C4 * fTemp_diff + C5 * fTemp_diff ** 2.0
     &      + C6 * fQ_avail   + C7 * fQ_avail   ** 2.0

      return
      end



C-------------------- fEval_elec_efficiency_LR -------------------------
C
C Function to evaluate the electrical efficiency of a Stirling
C engine cycle, as per the IEA/ECBCS Annex 42 system-level model
C
C Function form:
C
C    efficiency = C1 + C2 * (fuel flow) + C3 * (fuel flow )^2
C                    + C5 * (CWT -Tref ) + C6 * (CWT -Tref )^2
C                    + C8 * (CW flow - Ref flow.)
C                    + C9 * (CW flow - Ref flow.) ^2
C
C    ( CW = cooling water )
C
C----------------------------------------------------------------------
      real function fEval_elec_efficiency_LR ( iC_index,
     &                                         fQ_avail,
     &                                         fCW_temperature_in,
     &                                         fCW_mass_flow )
      implicit none
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'

      integer iC_index
      real fCW_temperature_in          ! Cooling water temperature (oC)
      real fCW_mass_flow               ! Cooling water flow rate (kg/s)
      real fQ_avail                    ! Gross available heat in heater (W)

C.....Coefficients (used to save space)
      real C1,C2,C3,C4,C5,C6,C7
C.....Reference conditions
      real fTemp_ref, fFlow_Ref

C.....Intermediate data
      real fTemp_diff, fFlow_diff

      C1 = fElec_eff_C_1(iC_index)
      C2 = fElec_eff_C_2(iC_index)
      C3 = fElec_eff_C_3(iC_index)
      C4 = fElec_eff_C_4(iC_index)
      C5 = fElec_eff_C_5(iC_index)
      C6 = fElec_eff_C_6(iC_index)
      C7 = fElec_eff_C_7(iC_index)

C.....Reference conditions
      fTemp_ref = fCorr_REF_CW_temp(iC_index)
      fFlow_ref = fCorr_REF_CW_flow(iC_index)
      
C----------------------------------------------------------------------
C     Difference between atual conditons and reference conditions
C----------------------------------------------------------------------
      fTemp_diff = fCW_temperature_in - fTemp_ref    ! (oC)
      fFlow_diff = fCW_mass_flow      - fFlow_Ref    ! (kg/s)   
      
C----------------------------------------------------------------------
C     Evaluate correlation
C----------------------------------------------------------------------
      fEval_elec_efficiency_LR =
     &   C1 + C2 * fFlow_diff + C3 * fFlow_diff ** 2.0
     &      + C4 * fTemp_diff + C5 * fTemp_diff ** 2.0
     &      + C6 * fQ_avail   + C7 * fQ_avail   ** 2.0

      return
      end


C-------------------- fEval_Skin_Losses -------------------------------
C
C Function to evaluate the parasitic heat transfer from
C a Stirling engine device based on empirical data, as
C per the IEA/ECBCS Annex 42 subsystem-level model
C
C Function form:
C
C     skin-loss = C1 + C2 * (fuel flow) + C3 * (fuel flow )^2
C               + C4 + C5 * (room temp) + C6 * (room temp )^2
C
C----------------------------------------------------------------------

      real function fEval_Skin_Losses ( iC_index,
     &                                  fN_fuel_flow,
     &                                  fTemp_Room_P )
      implicit none
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'

      real fN_fuel_flow
      real fTemp_Room_P
      integer iC_index

C.....Coefficients (used to save space)
      real C1,C2,C3,C4,C5
C.....Reference conditions
      real fTemp_ref

C.....Difference between room temperature and reference condition
      real fTemp_diff      
      
      C1 = fSkin_loss_C_1(iC_index)
      C2 = fSkin_loss_C_2(iC_index)
      C3 = fSkin_loss_C_3(iC_index)
      C4 = fSkin_loss_C_4(iC_index)
      C5 = fSkin_loss_C_5(iC_index)

      fTemp_ref  = fCorr_REF_amb_temp(iC_index)
      fTemp_diff = fTemp_Room_P - fTemp_ref
      
      fEval_Skin_Losses =
     &   C1 + C2 * fN_fuel_flow + C3 * fN_fuel_flow ** 2.0
     &      + C4 * fTemp_diff   + C5 * fTemp_diff ** 2.0

      return
      end


C---------------- Stirling_H3Kreports_module --------------------------
C
C This procedure transports Data from the Stirling Engine model to
C the h3kreporting object.
C
C Inputs:
C   - iC_index: pointer to component in plant network
C   - iAction: flag indicating if reports should be initialized,
C              or if data can be transported to the h3kreports
C              module.
C   - bEngine_Active: flag indicating if engine is operaitonal
C
C Outputs:
C
C    None.
C
C-----------------------------------------------------------------------

      subroutine Stirling_H3Kreports_module (
     &                 iIndex, iStatus )
      implicit none
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'
      INCLUDE 'CETC_definitions.h'

C External functions.     
      integer lnblnk

C.....Arguements
      integer iIndex
      integer iStatus
      integer iModel_Type

C-----------------------------------------------------------------------
C     ESP-r commons necessary for reporting
C-----------------------------------------------------------------------
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15, pcname

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
C.....Plant Miscellaneous data
      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

C-----------------------------------------------------------------------      
C     Local variables
C-----------------------------------------------------------------------
      character*128 H3Kname       ! holding variable
      character*128 char_temp     ! holding variable
      character*124 cContext      ! Contextual buffer
      character*248 cParamName    ! holding variable
      integer iLength,iNameLength ! Length of strings

      integer iUnit_Status_flag   ! flag indicating unit status

C.....Arrays storing locations of output variables
      integer iStorage_index(mpcom)
      
C.....Save storage arrays
      save iStorage_index

C.....Flag indicating parameter identification is being performed.
      logical bCCHT_ParamIdent       ! Flag for ccht stirling data
      save bCCHT_ParamIdent
      
C.....Residual error in parameter identification
      
C.....  -> Stirling engine status (more flags to come?)
      integer iOff, iOn, iOverHeated
      parameter ( iOff         = 0,
     &            iOn          = 1,
     &            iOverHeated  = 2 )

C.....Comparison flag
      logical bClose_To_Zero

     
C.....Model Topology
      integer iEngine_Node_index ! Index of component engine node in plant
                                 !   Network.
      integer iHX_Node_index     ! Index of component HX node in plant
                                 !   Network.                                 

      integer iCW_conn_index     ! Cooling water connection index

C.....Temperature & flow state varaibles
      real fCW_Temp_In           ! Inlet water temperature (oC)
      real fCW_Temp_Out          ! Outlet water temperature (oC)
      real fQ_recovery_average   ! TS averaged heat recovery (W)
      real fREF_Temp_out         ! Reference outlet temp (oC)
      real fREF_Heat_out         ! Reference heat output (W)
      real fCW_Flow              ! Cooling water flow (kg/s)
      real fCW_Cp                ! Cooling water specific heat (J / kg oC)

      
      real fDay                  ! Current day, as a fractional real
                                 !   (0.0->365.0)

      real fResidual_Temp        ! Residual temperauture (oC) used for
                                 !   parameter identification                                 
      real fResidual_Heat        ! Residual temperauture (oC) used for
                                 !   parameter identification
C----------------------------------------------------------------------      
C     Named constants
C----------------------------------------------------------------------
      integer iInterp            ! Interpolation method
      parameter (iInterp = 1 )   !   <- 1 = 'step' interpolation
      
C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      logical bInquire_BC_Name_Exists ! Function used to detemine if
                                 !   a boundary conditon with a given
                                 !   name has been defined.
      real H3K_Connect_property  ! Function returning property of
                                 !   requested connection
      real shtfld                ! Function returning specific heat
                                 !   of a fluid (J kg/oC)
      real fGet_BC_data_by_name  ! Function returning the value of
                                 !   of a requested boundary condition
      real fConvert_Current_TS_to_DAY ! Function converting the current
                                 !   timestep to a real # representing
                                 !   the current day & fraction thereof                                                                  

      
      if ( iStatus .eq. iInitialize ) then

C----------------------------------------------------------------------
C        Initialize reports -> boundary conditions and statistics
C----------------------------------------------------------------------
C........Get component name and length
         write(char_temp,'(A,A,A)')
     &      'plant/',
     &      pcname(iIndex)(1:lnblnk(pcname(iIndex))),
     &      '/misc_data'
         call h3k_StoreString(char_temp,iStorage_index(iIndex),
     &      iLength)

C----------------------------------------------------------------------
C        Check if reserved boundary condition keyword for
C        parameter identification research is in use.
C
C        Parameter identification mode is triggered when the boundary
C        condition 'REFERENCE_Stirling_CCHT_Temp_out' is found
C        in the boundary definition file.
C----------------------------------------------------------------------
C........Write parameter identification name
         write (cParamName, '(A)') 'REFERENCE_Stirling_CCHT_Temp_out'
C........Use function bInquire_BC_Name_Exists to check for name.         
         bCCHT_ParamIdent = bInquire_BC_Name_Exists ( cParamName )

      endif 

C--------------------------------------------
C.....Recover Component name
      call h3k_RecoverString(iStorage_index(iIndex),
     &                       char_temp, iNameLength)
      
C.....Get model node and connection indicies.
      iEngine_Node_index = NPCDAT ( iIndex, 9 )
      iHX_Node_index     = iEngine_Node_index + 1
      iCW_conn_index     = icondx ( iIndex, 2, 1 )

C.....Get water flow rate (kg/s)
      fCW_Flow = CSVF ( iHX_Node_index, iProp1stFlow )

C.....Get HX node temperature (oC)
      fCW_Temp_Out = CSVF ( iHX_Node_index, iPropTemp )

C.....Get HX Inlet temperature (oC). We need to use function
C.....H3K_Connect_property as the convar common is not updated until
C.....the next timestep
      fCW_Temp_In = H3K_Connect_property( iCW_conn_index, iPropTemp )

C.....Get connection spacific heat. Use function shtfld:
C.....  specific heat capactiy = Shtfld ( X , Temperature ) (J/kg oC)
C.....  where X=1 -> air, X=2 -> vapor, X=3 -> liq. water
      fCW_Cp = shtfld ( 3, ( fCW_Temp_In + fCW_Temp_Out ) / 2 )

C.....Calculate heat recovery
      fQ_recovery_average = fCW_Flow * fCW_Cp
     &                           * ( fCW_Temp_OUT - fCW_Temp_In )

C.....Check if cooling water flow rate is zero.
      call eClose( fCW_Flow, 0.0, 1E-05, bClose_To_Zero )

C----------------------------------------------------------------------
C     Check if parameter identification is being performed, and if so,
C     calculate residuals. These data are used when running parameter
C     identification studies in which the inputs providing the
C     best fit to experimental data are to be determined.
C
C     Note: the CCHT data collection device only measured the engine 
C           outlet temperature when water was flowing through the
C           engine. The CCHT data is 'meaningless' when there's no
C           flow through the engine - therefore, there's no need
C           to calculate residuals in these conditions. 
C     
C----------------------------------------------------------------------      
      if ( bCCHT_ParamIdent .and. .not. bClose_To_Zero ) then

C........Write contextual buffer
         write (cContext, '(A,A,I2,A)')
     &      'Recovering reference conditions for Stirling ',
     &      'engine parameter identification (plant component #',
     &      iIndex,').'
      
C........Get current day 
         fDay = fConvert_Current_TS_to_DAY()
         
C........Collect reference temperature
         write (cParamName, '(A)') 'REFERENCE_Stirling_CCHT_Temp_out'
         fREF_Temp_out = fGet_BC_data_by_name ( cParamName,
     &                                          fDay,
     &                                          iInterp,
     &                                          cContext )

C........Residual (error) in temperature (oC)               
         fResidual_Temp = ABS(fCW_Temp_Out - fREF_Temp_out)

C........Output data to h3kreports
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/param_ident/residual_temperature'
     
         call add_to_xml_reporting(
     &      fResidual_Temp,
     &      H3Kname,
     &      'units', '(oC)',
     &      'Stirling engine: parameter identification '
     &        // ' - residual outlet temperautre'
     &   )

     
C........Collect reference heat recovery          
         write (cParamName, '(A)') 'REFERENCE_Stirling_CCHT_Heat_out'
         fREF_Heat_out = fGet_BC_data_by_name ( cParamName,
     &                                          fDay,
     &                                          iInterp,
     &                                          cContext )     

C........Residual (error) in heat (W)
         fResidual_Heat = ABS(fQ_recovery_average - fREF_Heat_out)

C........Output data to h3kreports
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),
     &      '/param_ident/residual_heat'
     
         call add_to_xml_reporting(
     &      fResidual_Heat,
     &      H3Kname,
     &      'units', '(W)',
     &      'Stirling engine: parameter identification '
     &        // ' - residual heat output'
     &   )
         
      endif       

C--------------------------------------------
C.....Output data: Control Scheme
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/external_control/scheme'
      call add_to_xml_reporting(
     &   fReport_Control_Method(iIndex),
     &   H3Kname,
     &   'units', '(-)',
     &   'Stirling engine: External control scheme type'
     &   )

C--------------------------------------------
C.....Write a name: Control Signal
      write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/external_control/signal'
      call add_to_xml_reporting(
     &   fReport_Control_Signal(iIndex),
     &   H3Kname,
     &   'units', '(-)',
     &   'Stirling engine: External control signal'
     &   )

C--------------------------------------------
C.....Write a name: Time spent inoperative
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/operating_mode/inoperative'
      call add_to_xml_reporting(
     &   fReport_time_spent(iIndex,iOP_inoperative),
     &   H3Kname,
     &   'units', '(s)',
     &   'Stirling engine: Cumulative time spent inoperative'
     &   )

     
C-----------------------------------------
C.....Write a name: Time spent in start-up
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/operating_mode/start_up'
      call add_to_xml_reporting(
     &   fReport_time_spent(iIndex,iOP_startup),
     &   H3Kname,
     &   'units', '(s)',
     &   'Stirling engine: Cumulative time spent in start-up mode'
     &   )


C-----------------------------------------
C.....Write a name: Time spent in normal operation
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/operating_mode/normal_operation'
      call add_to_xml_reporting(
     &   fReport_time_spent(iIndex,iOP_normal_operation),
     &   H3Kname,
     &   'units', '(s)',
     &   'Stirling engine: Cumulative time spent in normal operation'
     &   )     

C-----------------------------------------
C.....Write a name: Time spent in shutdown
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/operating_mode/shutdown'
      call add_to_xml_reporting(
     &   fReport_time_spent(iIndex,iOP_shutdown),
     &   H3Kname,
     &   'units', '(s)',
     &   'Stirling engine: Cumulative time spent in shut-down mode'
     &   )      

C-----------------------------------------
C.....Write a name: Containment temperature
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/containment_temperature'
      call add_to_xml_reporting(
     &   fReport_Containment_Temp(iIndex),
     &   H3Kname,
     &   'units', '(oC)',
     &   'Stirling engine: temperature of containing room'
     &   )

C-----------------------------------------
C.....Write a name: Cooling water temperature
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength), '/cooling_water/temperature'
      call add_to_xml_reporting(
     &   fReport_CW_Temperature(iIndex),
     &   H3Kname,
     &   'units', '(oC)',
     &   'Stirling engine: temperature of cooling water at inlet'
     &   )



C-----------------------------------------
C.....Write a name: Cooling water mass flow rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength), '/cooling_water/flow_rate'
      call add_to_xml_reporting(
     &   fReport_CW_Mass_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'Stirling engine: mass flow rate of cooling water'
     &   )
     
C-------------------------------------------------------------------
C     Initialize reports. -> instantaneous data
C-------------------------------------------------------------------

C-----------------------------------------
C.....Write a name: Net power
      write(H3Kname,'(A,A)')
     &   char_temp(1:iNameLength), '/instantaneous/power/net'
      call add_to_xml_reporting(
     &   fReport_Net_Power(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: net power produced'
     &   )

C-----------------------------------------
C.....Write a name: Heat recovery
      write(H3Kname,'(A,A)')
     &   char_temp(1:iNameLength),'/instantaneous/heat_recovery'
      call add_to_xml_reporting(
     &   fReport_Heat_Recovery(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: Rate of heat transfer to cooling water'
     &   )

     
C-----------------------------------------
C.....Write a name: fuel flow
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/instantaneous/fuel/flow_rate'
      call add_to_xml_reporting(
     &   fReport_Fuel_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'Stirling engine: fuel mass flow rate'
     &   )     
C-----------------------------------------
C.....Write a name: fuel LHV flow rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/instantaneous/fuel/gross_energy_input'
      call add_to_xml_reporting(
     &   fReport_Fuel_LHV_rate(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: LHV of fuel used by engine'
     &   )

C-----------------------------------------
C.....Write a name: Skin losses
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/instantaneous/heat_loss'
      call add_to_xml_reporting(
     &   fReport_Heat_Loss(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: rate of heat loss to surroundings'
     &   )


C-------------------------------------------------------------------
C     Reports -> time-step averaged data
C-------------------------------------------------------------------
C-----------------------------------------
C.....Write a name: Heat recovery
      write(H3Kname,'(A,A)')
     &   char_temp(1:iNameLength),'/average/heat_recovery'
      call add_to_xml_reporting(
     &   fQ_recovery_average,
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: Rate of heat transfer to cooling water'
     &    // 'averaged over time step'
     &   ) 

C-----------------------------------------
C.....Write a name: Net power
      write(H3Kname,'(A,A)')
     &   char_temp(1:iNameLength),'/average/power/net'
      call add_to_xml_reporting(
     &   fAverage_Net_Power(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: Rate of net electrical production averaged'
     &     // 'over timestep' ) 

C-----------------------------------------
C.....Write a name: fuel flow
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/average/fuel/flow_rate'
      call add_to_xml_reporting(
     &   fAverage_Fuel_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'Stirling engine: Fuel mass flow rate averaged over timestep'
     &   ) 

C-----------------------------------------
C.....Write a name: fuel LHV flow rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/average/fuel/gross_energy_input'
      call add_to_xml_reporting(
     &   fAverage_Fuel_LHV_rate(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'Stirling engine: LHV of fuel used by engine, averaged over'
     &      //' timestep' )

C----------------------------------------------------------------------
C        Check if unit is operating, and if so, report efficiencies 
C----------------------------------------------------------------------
      if ( bReport_TS_modes(iIndex,iOP_startup) .OR.
     &     bReport_TS_modes(iIndex,iOP_normal_operation) ) then
     
C-----------------------------------------
C........Write a name: electrical efficiency
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),'/average/efficiency/electric'
         call add_to_xml_reporting(
     &      fAverage_Electric_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'Stirling engine: Electrical efficiency' 
     &        // ' averaged over timestep' )      

C-----------------------------------------
C........Write a name: thermal efficiency
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),'/average/efficiency/thermal'
         call add_to_xml_reporting(
     &      fAverage_Thermal_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'Stirling engine: Thermal efficiency averaged over '
     &        // 'timestep') 


C-----------------------------------------
C........Write a name: cogeneration efficiency
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),'/average/efficiency/cogeneration'
         call add_to_xml_reporting(
     &      fAverage_Cogen_efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'Stirling engine: Cogeneration (overall) efficiency'
     &       // ' averaged over timestep') 
         
     
C........Write a name: Cogeneration efficency
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/instantaneous/efficiency/cogeneration'
         call add_to_xml_reporting(
     &      fReport_Cogen_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'Stirling engine: Cogeneration (overall) efficiency '
     &       // 'at end of time step'
     &    )

C--------------------------------------
C........Write a name: Electric efficency
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/instantaneous/efficiency/electric'
         call add_to_xml_reporting(
     &      fReport_Electric_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'Stirling engine: Electrical efficiency'
     &      // ' at end of timestep')

C--------------------------------------
C........Write a name: Thermal Efficiency
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/instantaneous/efficiency/thermal'
         call add_to_xml_reporting(
     &      fReport_Thermal_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'Stirling engine: Electrical efficiency'
     &       // 'at end of timestep')

      endif 

      return
      end   ! of subroutine

      
C------------------- Eval_TS_average ------------------------------------
C
C  This procedure will evaluate the time-step averaged value of
C  variable X that is changing between values X1 and X2, and 
C  for which the rate of change is constrained.
C
C  Inputs:
C     fX_initial    -  Value of variable at start of time-step.
C     fX_target     - 'Target' value variable X
C     fX_max_change -  Maximum rate of change permitted in
C                      variable X (1/seconds)
C     fTS_duration  -  Timestep duration (seconds)
C      
C
C  Outputs:
C     fX_average    -  Time-step averaged value of variable X
C     fX_final      -  Value of variable X at end of timestep.
C
C  Assumptions:
C
C     - Variable X exhibits linear behavior between 
C
C----------------------------------------------------------------------
      subroutine Eval_TS_average(
     &           fX_initial,
     &           fX_target,
     &           fX_max_change,
     &           fTS_duration,
     &           fX_average,
     &           fX_final )
      implicit none
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'
      INCLUDE 'Stirling_CHP.h'
C----------------------------------------------------------------------
C     Passed variables
C----------------------------------------------------------------------
      real fX_initial,fX_target,fX_max_change,fX_average,fX_final,
     &     fTS_duration
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fLag_Time            ! Time required for X to go from
                                ! X1 to X2 (seconds)
      real fTS_frac_transient   ! Fraction of timestep for which
                                !   variable exhibits transient behavior
      real fTS_frac_steady      ! Fraction of time step for which
                                !   variable exhibits steady-state
                                !   behavior
      real fX_rate_change       ! Rate of change in X                                
      logical bNums_are_close   ! Result of close-to-zero comparisons
      logical bTransients       ! Flag indicating that variable 
                                !   exhibits dynamic (ie non
                                !   quasi-steady-state) behavior

C---------------------------------------------------------------------      
C     Rate of change in X: assume that X changes at max possible rate
C---------------------------------------------------------------------
      if ( fX_initial .le. fX_target ) then
         fX_rate_change = fX_max_change
      else
         fX_rate_change = -1 * fX_max_change
      endif
                                
C---------------------------------------------------------------------
C     Determine the time required for X to go from X1 to X2
C
C                     | X2 - X1 |
C         time =  ----------------------
C                 maximum rate of change 
C
C
C     - If maximum rate of change is equal to, or less than zero,
C       assume that variable exhibits no dynamic characteristics
C       (ie time=0)
C----------------------------------------------------------------------      
      call eclose (fX_max_change,0.0,
     &             fClose_to_zero_tolerance, bNums_are_close)
      
      if ( bNums_are_close .or. fX_max_change .lt. 0.0 ) then
         
         fLag_Time = 0.0  ! (s)
         bTransients = .false.
         
      else 

         fLag_Time = abs ( fX_target - fX_initial ) / fX_max_change ! (s)
         bTransients = .true.
         
      endif

C----------------------------------------------------------------------
C     Determine fraction for which value of X is changing:
C
C                       lag time 
C       fraction = -------------------
C                   time step duration
C
C----------------------------------------------------------------------
      
      fTS_frac_transient = fLag_Time / fTS_duration

C.....Is lag time greater than 1?
      if ( fTS_frac_transient .gt. 1.0 ) fTS_frac_transient = 1.0

C.....Fraction of timestep in steady-state
      fTS_frac_steady = 1.0 - fTS_frac_transient

C----------------------------------------------------------------------
C     Calculate the value of variable X at the end of the
C     time-step
C
C        X_final = X_initial + ( rate of change )
C                                * (transient fraction) * (TS duration)
C
C----------------------------------------------------------------------

      if ( bTransients ) then 
      
         fX_final = fX_initial
     &            + fX_rate_change * fTS_frac_transient * fTS_duration

      else

         fX_final = fX_target

      endif

C----------------------------------------------------------------------
C     Calculate the average value of the variable over the timestep
C
C                X_initial + X_final
C     Average =  -------------------  * (Transient fraction)
C                       2
C
C                + X_final * (steady-state fraction)
C
C----------------------------------------------------------------------

      fX_average = (fX_initial + fX_final) / 2.0 * fTS_frac_transient
     &             + fX_final * fTS_frac_steady


      return
      end

