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
C================= compressed_cylinder.F ==============================
C
C This file contains procedures associated with modelling a compressed
C gas cylinder that is presumably connected to ESP-r's plant network.
C
C It contains the following procedures:
C
C    - Comp_Cyl_Coeff_Gen: Top level solution routine that
C        returns the plant component matrix coefficients
C        expected by ESP-r.
C
C
C    - Comp_Cyl_Char Supervisory routine for the gas
C        cylinder energy balance.
C
C    - Comp_Cyl_Validate_inputs: Error trapping routine
C
C    - Comp_Cyl_PostPro: Post processing and reporting routine
C
C======================================================================
C======================================================================

C---------------- Comp_Cyl_Coeff_Gen ----------------------------------
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.
C
C ABSTRACT:
C This routine provides ESP-r's plant matrix domain with the
C coefficients necessary to characterize the component's reactions with
C the rest of the plant network. The cylinder does not support first or
C second phase flow connections (water/air/vapor), and the values of
C these coefficients are set directly. The procedure Comp_Cyl_Char
C is used to characterize the hydrogen & temperature matricies.
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

      subroutine Comp_Cyl_Coeff_Gen(
     &               iComponent,
     &               fCoefficients,
     &               iPlant_Matrix  )
      implicit none

#include "plant.h"
C----------------------------------------------------------------------
C     Logical array indicating if plant components have support for 4th, 
C     hydrogen matrix
C----------------------------------------------------------------------
      common  / H2_lib / 
     &     h2FlowSupport      
      logical h2FlowSupport(mpcom)

C----------------------------------------------------------------------
C     Passed arguemets
C----------------------------------------------------------------------
      integer iComponent          ! pointer to component in net.
      integer iPlant_matrix             ! index of matrix to be solved

      real fCoefficients(mpcoe)         ! array containing matrix
                                        ! coefficients calculated by
                                        ! the component.
C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      integer iPropTemp         ! named constant for temperature property
      integer iProp1stFlow      ! named constant for first phase flow property
      integer iProp2ndFlow      ! named constant for second phase flow property
      integer iPropH2Flow       ! named constant for hydrogen flow
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )
                       
C-----------------------------------------------------------------------
C     Enable the fourth, hydrogen flow matrix
C-----------------------------------------------------------------------
      h2FlowSupport ( iComponent ) = .true.
         
C----------------------------------------------------------------------
C     Determine which matrix is being solved, and set coefficients
C     accordingly.
C----------------------------------------------------------------------
      if ( iPlant_Matrix .eq. iProp1stFlow ) then

C----------------------------------------------------------------------
C        Plant first phase flow (ie air, water) matrix is to be
C        solved. The gas cylinder does not support a water
C        connection. Thus
C
C           | 1.0    0.0 | | mo |  = 0.0
C                          | mi |
C
C        Where: mo is the flow through the cyliner, and
C               mi is the flow entering the cyliner.
C
C----------------------------------------------------------------------

         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0

      elseif (iPlant_Matrix .eq. iProp2ndFlow ) then
C----------------------------------------------------------------------
C        Plant second phase flow (ie water vapor) matrix is to be
C        solved. The compressed cylinder model does not support
C        2nd-phase flow therefore:
C
C           | 1.0    0.0 | | mo |  = 0.0
C                          | mi |
C
C        Where: mo is the flow through the cylinder, and
C               mi is the flow enteriing the cyliner.
C
C----------------------------------------------------------------------

         fCoefficients(1) =  1.0
         fCoefficients(2) =  0.0
         fCoefficients(3) =  0.0

      elseif (iPlant_Matrix .eq. iPropH2Flow ) then
C----------------------------------------------------------------------
C        Plant hydrogen flow matrix is to be solved. The
C        routine Comp_Cyl_H2_balance is used to characterize
C        the component's hydrogen balance:
C
C           | 1.0    0.0 | | mo |  = Hydrogen delivered
C                          | mi |
C
C        Where: mo is the flow through the cylinder, and
C               mi is the flow entering the cylinder.
C
C----------------------------------------------------------------------

         call Comp_Cyl_Char (
     &           iComponent,
     &           iPlant_Matrix,
     &           fCoefficients )


      elseif ( iPlant_Matrix .eq. iPropTemp ) then
C----------------------------------------------------------------------
C        Temperature matrix is to be solved. Use subordinate
C        routine to characterize themral balance:
C
C           | Coeff_a    Coeff_b  | |  To  | = [ Present time & known ]
C                                   |  Ti  |
C
C----------------------------------------------------------------------

         call Comp_Cyl_Char (
     &           iComponent,
     &           iPlant_Matrix,
     &           fCoefficients )

      endif

      return
      end

C---------------- Comp_Cyl_Char ---------------------------------------
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.
C
C ABSTRACT:
C This routine characterizes the heat balance inside a compressed
C gas cylinder.
C
C Inputs:
C
C  iComponent       - index of component in plant network
C
C Outputs
C  fCoefficients          - array containing components matrix
C                           coefficients
C----------------------------------------------------------------------
      subroutine Comp_Cyl_Char(
     &            iComponent,
     &            iPlant_Matrix,
     &            fCoefficients )
      implicit none

#include "plant.h"
#include "chemical_properties.h"
#include "Hydrogen_demand_controller.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent   ! pointer to component in plant network
      integer iPlant_Matrix      ! index of plant matrix to be solved

      real fCoefficients(mpcoe)  ! array of component matrix coefficients
C----------------------------------------------------------------------
C     ESP-r common blocks
C----------------------------------------------------------------------

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
      real qdata     ! not used in current context
      real pcaout    ! not used in current context
      integer napdat ! # of plant additional outputs

C.....Plant network time constants
      COMMON/pctc/tc
      real tc(mpcom) ! time contant (seconds)

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
      real pcntmf(mpcom)      ! future time row plant containment temp (oC)
      real pctp(mpcon)
      real pcrp(mpcon)
      real puap(mpnode)
      real pcqp(mpnode)
      real pcntmp(mpcom)      ! present time row plant containment temp (oC)

C.....Common containing plant componet mass diversion ratios 
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),ipc2(mpcon),
     &            ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      integer npcon           !-number of inter-component connections
      integer ipc1            !-component number defining the receiving
                              !-component
      integer ipn1            !-node number defining the receiving component
      integer ipct            !-plant component inter-connection type
      integer ipc2            !-component number defining the sending
                              !-component
      integer ipn2            !-node number defining the sending component
      real    pcondr          !-ratio of mass flow rate through the connection
                              !-and the mass flow rate entering the receiving
                              !-node for each plant component inter-connection
      real    pconsd          !-supplementary data for plant component inter-
                              !-connection
      
C----------------------------------------------------------------------
C     Model commons
C----------------------------------------------------------------------

C.....Compressed cylinder results.
      common / Comp_cyl_results /
     &       fReport_H2_mass_present     ( MPCom ),
     &       fReport_H2_mass_future      ( MPCom ),
     &       fReport_H2_charge           ( MPCom ),
     &       fReport_H2_discharge        ( MPcom ),
     &       fReport_H2_vent             ( MPcom ),
     &       fReport_H2_pressure_present ( MPCom ),
     &       fReport_H2_pressure_future  ( MPCom ),
     &       iStore_Connect_index(MPCom)

      real fReport_H2_mass_present
      real fReport_H2_mass_future
      real fReport_H2_charge
      real fReport_H2_discharge
      real fReport_H2_vent
      real fReport_H2_pressure_present
      real fReport_H2_pressure_future
      integer iStore_Connect_index


C----------------------------------------------------------------------
C     Local Variables
C----------------------------------------------------------------------

C.....Model topology
      integer iNode_index    ! index of model's node in plant network
      integer iConn_index    ! index of model's connection


C.....Model parameters
      real fCyl_Volume       ! Cylinder gas volume (m3)
      real fCyl_solid_mass   ! Mass of cylinder wall (kg)
      real fCyl_solid_Cp     ! Specific heat of cylinder wall (J/kg oC)
      real fCyl_UA_ambient   ! Heat transfer coeff. between cylinder
                             !   & ambient (W/oC)
      real fCyl_MAX_press    ! Maximum gas pressure (kPa)
      real fCyl_MIN_press    ! Downstream operating pressure (kPa)
      real fCyl_Init_press   ! Inital pressure in cylinder @
                             !    start of simulation.

C.....State variables & other data
      real fCyl_Temp_Present  ! Present time-row temperature (oC)
      real fCyl_Temp_Future   ! Future time-row temperature  (oC)

      real fCyl_Press_Present ! Present time-row pressure (kPa)
      real fCyl_Press_Future  ! Future time-row pressure (kPa)


      real fCyl_H2_mass_present ! present time-row mass of encapsulated
                                !   hydrogen (kg)
      real fCyl_H2_mass_future  ! future time-row mass of encapsulated
                                !   hydrogen (kg)

      real fCyl_H2_Cv_present   ! Specific heat of encapsulated hydrogen
                                !   at present time-row system averaged
                                !   temperature (J/kg oC)

      real fCyl_H2_Cv_future    ! Specific heat of encapsulated hydrogen
                                !   at future time-row system averaged
                                !   temperature (J/kg oC)

      real fCyl_Time_Constant   ! Cylinder time constant (s)

      real fCyl_H2_mass_MAX     ! maximum mass that can be stored in the
                                !    cylinder

      real fCyl_H2_mass_MIN     ! minimum mass that can be stored in the
                                !    cylinder

      real fCyl_H2_demand       ! Total Hydrogen demanded by external
                                !    controller (kg/s)

      real fCyl_H2_demand_EXP   ! Hydrogen demanded by external
                                !    controller (kg/s) for explicitly
                                !    modelled components

      real fCyl_H2_demand_IMP   ! Hydrogen demanded by external
                                !    controller (kg/s) for implicitly
                                !    modelled components
                                
      real fCyl_H2_vent         ! Rate at which hydrogen must be vented to
                                !     the atmosphere (kg/s)

      real fCyl_H2_discharge_present   ! Rate at which hydrogen is delivered by
                                       !     the tank (kg/s)
      real fCyl_H2_discharge_future    ! Rate at which hydrogen is delivered by
                                       !     the tank (kg/s)

      real fCyl_H2_charge_present      ! Rate at which hydrogen is stored within
                                       !     the tank (kg/s)

      real fCyl_H2_charge_future       ! Rate at which hydrogen is stored within
                                       !     the tank (kg/s)


      real fSoln_alpha          ! implicit/explicit weighting factor (-)

C.....Connection properties
      real fConn_H2_Cv_present  ! Specific heat of incomming hydrogen
                                !   at present time-row system averaged
                                !   temperature (J/kg oC)

      real fConn_H2_Cv_future   ! Specific heat of incomming hydrogen
                                !   at future time-row system averaged
                                !   temperature (J/kg oC)

      real fConn_Temp_Present   ! Presnet time-row connection temperature (oC)
      real fConn_Temp_Future    ! Future time-row connection temperaturte (oC)

      real fConn_Mass_Flow      ! Connection hydrogen mass flow rate (kg/s)

C.....Ambient conditions
      real fAmb_Temp_Present    ! Ambient present temperature (oC)
      real fAmb_Temp_Future     ! Ambient future temperature (oC)

C.....Intermediate results used when calculating matrix coefficients
      real fCyl_heat_cap        ! Cylinder heat capacitiance

      real fConn_dU_present     ! internal energy change of
                                ! incomming hydrogen on present time
                                ! row (J)

      real fCharge_Pv_present   ! flow work performed by charging tank
                                ! on present time-row (W)

      real fDischarge_Pv_present ! flow work performed by dischargine tank
                                 ! on present time-row (W)

      real fCharge_Pv_future    ! flow work performed by charging tank
                                ! on future time-row (W)

      real fDischarge_Pv_future ! flow work performed by dischargine tank
                                ! on future time-row (W)

      real fCyl_HeatLoss_present ! Rate of heat transfer between present
                                 ! and future (W)

C.....Global properties
      real fR_hydrogen          ! H2 ideal gas constant (kJ/kg K)
      save fR_hydrogen

      real fTS_duration         ! timestep duration (s)
      save fTS_duration

      integer iGas_LAW          ! Flag indicating which gas law is in use
                                ! -> 1 = ideal gas
                                ! -> 2 = Lee Kesler (not yet supported)

      logical bDebug            ! flag indicating if debugging is active
      parameter ( bDebug = .false. )

C-----------------------------------------------------------------------
C     Misc local variables
C-----------------------------------------------------------------------
      integer iCount            ! Counter
      logical bNums_Are_Close   ! logical result of close-to-zero comparisons
C----------------------------------------------------------------------
C     Named constants
C----------------------------------------------------------------------
      real fC_to_K              ! celsius -> kevlin conversion
      parameter ( fC_to_K = 273.15 )

      integer iPropTemp         ! named constant for temperature property
      integer iProp1stFlow      ! named constant for first phase flow property
      integer iProp2ndFlow      ! named constant for second phase flow property
      integer iPropH2Flow       ! named constant for hydrogen flow
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )

C.....reporting module status
      integer iInitialize, iReport
      parameter ( iInitialize  = 1,
     &            iReport      = 2 )

C-----------------------------------------------------------------------
C     Interfaces to chemical_properties.F
C-----------------------------------------------------------------------
      real fEval_Compound_Cv    ! returns the ideal gas constant
                                ! pressure specific heat of a compound
      real fCompound_R          ! returns the ideal gas constant
                                ! of a compound (kJ/kg K)
      real fEval_Compound_MM    ! returns the molar mass of a compound

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
C----------------------------------------------------------------------

      iNode_index = NPCDAT ( iComponent, 9 )
      icsv( iNode_index, 1 ) = 1
      csvi( iNode_index, 1 ) = csvf( iNode_index, 1 )

C----------------------------------------------------------------------
C     Collect model parameters from ESP-r's ADATA array.
C----------------------------------------------------------------------
      fCyl_Volume      =  ADATA ( iComponent, 1 )     ! m3
      fCyl_solid_mass  =  ADATA ( iComponent, 2 )     ! kg
      fCyl_solid_Cp    =  ADATA ( iComponent, 3 )     ! J/kg oC
      fCyl_UA_ambient  =  ADATA ( iComponent, 4 )     ! W/oC
      fCyl_MIN_press   =  ADATA ( iComponent, 5 )     ! kPa
      fCyl_MAX_press   =  ADATA ( iComponent, 6 )     ! kPa
      fCyl_Init_press  =  ADATA ( iComponent, 7 )     ! kPa
      iGas_LAW     = int( ADATA ( iComponent, 8 ) )   ! (-)

C----------------------------------------------------------------------
C     Collect properties of incomming hydrogen connection
C----------------------------------------------------------------------

C.....Connection index
C..... -> icondx (i,j,k) holds the connection number for the k'th
C.....    connection to the j'th node of component i. It is used
C.....    as a pointer.
      iConn_index = icondx ( iComponent, 1, 1 )
C.....Future time row connection temperature (oC)
      fConn_Temp_Future = CONVAR( iConn_index, iPropTemp ) 
C.....Connection hydrogen mass flow rate (kg/s)
      fConn_Mass_Flow   = CONVAR (iConn_index, iPropH2Flow )
     &                       * pcondr ( iConn_index )


C----------------------------------------------------------------------
C     Collect present & future time-row plant containment temperatures
C----------------------------------------------------------------------
      fAmb_Temp_Present = pcntmp ( iComponent )        ! (oC)
      fAmb_Temp_Future  = pcntmf ( iComponent )        ! (oC)

      call eclose ( fAmb_Temp_Future , -99.0, 0.1, bNums_Are_Close )

      if ( bNums_Are_Close ) then

         fCyl_UA_ambient = 0.0
         
      endif 
      
C----------------------------------------------------------------------
C     Collect temperatue state variables from present-row time blocks
C----------------------------------------------------------------------
      fCyl_Temp_Present = CSVp( iNode_index, iPropTemp )    ! (oC)
      fCyl_Temp_Future  = CSVf( iNode_index, iPropTemp )    ! (oC)


C----------------------------------------------------------------------
C     Initialization
C----------------------------------------------------------------------
      if ( .not. bInitialized (iComponent) ) then

C........Error trap inputs
         call Comp_Cyl_Validate_inputs (
     &                             iComponent,
     &                             fCyl_Volume     ,
     &                             fCyl_solid_mass ,
     &                             fCyl_solid_Cp   ,
     &                             fCyl_UA_ambient ,
     &                             fCyl_MIN_press  ,
     &                             fCyl_MAX_press  ,
     &                             fCyl_Init_press ,
     &                             iGas_LAW          )

C........Initialize component post-processor
         call Comp_Cyl_PostPro ( iComponent, iInitialize )

C........reset flag ensuring initialization is not repeated.
         bInitialized (iComponent) = .true.

C........Save connection index for reporting module
         iStore_Connect_index (iComponent) = iConn_index

      endif

      if ( nsinc .eq. 1 ) then

C........Set tank pressure to initial value
         fCyl_Press_Present = fCyl_Init_press               ! (kPa)

C........Set Connection temperature to initial values
         fConn_Temp_Present = fConn_Temp_Future             ! (oC)

C........Future time-row temperature has not been calcuated
C........yet, use present time-row value
         fCyl_Temp_Future = fCyl_Temp_Present               ! (oC)

C........Collect ideal gas constant for hydrogen
         fR_hydrogen = fCompound_R ( iHydrogen )            ! (J/kg K)

C........Calculate timestep duration
         fTS_duration = 3600. / ( ntstep * ntstpp )         ! s

C----------------------------------------------------------------------
C        Calculate present time-row mass using the ideal gas law
C        ( m = PV / RT ). Note: temperature data provided by ESP-r
C        is in the Celsius scale, and must be converted to Kelvin
C        for use with the Ideal gas law. The parameter fC_to_K
C        provides the conversion factor.
C----------------------------------------------------------------------

           fCyl_H2_mass_present = fCyl_Press_Present * fCyl_Volume
     &        /  ( fCompound_R ( iHydrogen ) / 1000.
     &               * ( fCyl_Temp_Present + fC_to_K )
     &            )  ! (kg)


      else

C........Collect present time row data from
C........Misc time-row data array (PCDatP)
C
C........Tank mass at present & future time rows
         fCyl_H2_mass_Present = PCDatP(iComponent,1)    ! (kg)

C........Connection temperature
         fConn_Temp_Present = PCDatP(iComponent,5)    ! (oC)

      endif



      if ( iPlant_Matrix .eq. iPropH2Flow ) then

C-----------------------------------------------------------------------
C        Mass flow matrix is to be solved. Collect hydrogen demand
C        from external controller.
C-----------------------------------------------------------------------

C........Explicit hydrogen demand (kg/s)
         fCyl_H2_demand_EXP = CDATA ( iComponent, 1 )

C........Implicit hydrogen demand (kg/s)
         fCyl_H2_demand_IMP = CDATA ( iComponent, 2 )

C........Total demand (kg/s)         
         fCyl_H2_demand     = fCyl_H2_demand_EXP + fCyl_H2_demand_IMP

C----------------------------------------------------------------------
C        Determine the maximum and minimum amount of hydrogen that can be
C        stored in the tank assuming that the future time-row tank
C        temperature prevails. Use the ideal gas law (PV=mRT)
C
C
C                                   max pressure * volume
C         max mass =  ---------------------------------------------------
C                     Ideal gas constant * future time row temperature (K)
C
C                                   min pressure * volume
C         min mass =  ---------------------------------------------------
C                     Ideal gas constant * future time row temperature (K)
C
C----------------------------------------------------------------------

         fCyl_H2_mass_MAX = fCyl_MAX_press * fCyl_Volume
     &       / ( fCompound_R (iHydrogen) / 1000.0
     &              * ( fCyl_Temp_Future + fC_to_K )
     &          )                                             ! (kg)

         fCyl_H2_mass_MIN = fCyl_MIN_press * fCyl_Volume
     &       / ( fCompound_R (iHydrogen) / 1000.0
     &              * ( fCyl_Temp_Future + fC_to_K )
     &          )                                             ! (kg)

C----------------------------------------------------------------------
C        Estimate future time row hydrogen mass:
C
C            future mass = present mass
C                          + ( flow rate in - flow rate out )
C                              * time-step duration
C
C----------------------------------------------------------------------
         fCyl_H2_mass_future = fCyl_H2_mass_present
     &          + ( fConn_Mass_Flow - fCyl_H2_demand )
     &                * fTS_duration                          ! (kg)

C----------------------------------------------------------------------
C        Ensure that the maximum & minimum mass limits of hydrogen that
C        can be stored in the tank are respected:
C----------------------------------------------------------------------
         if ( fCyl_H2_mass_future .gt. fCyl_H2_mass_MAX ) then
C----------------------------------------------------------------------
C           Tank mass will exceed maximum; some hydrogen will have
C           to be vented:
C-----------------------------------------------------------------------
C...........Rate at which hydrogen is vented
            fCyl_H2_vent = ( fCyl_H2_mass_future - fCyl_H2_mass_MAX )
     &                 / fTS_duration           ! (kg/s)

C...........Rate at which tank is charged
            fCyl_H2_charge_future = fConn_Mass_Flow - fCyl_H2_vent   ! (kg/s)
            if (  fCyl_H2_charge_future .lt. 0.0 )
     &                   fCyl_H2_charge_future = 0.0

C...........Rate at which tank is discharged
            fCyl_H2_discharge_future = fCyl_H2_demand  ! (kg/s)


         elseif ( fCyl_H2_mass_future .lt. fCyl_H2_mass_MIN ) then
C----------------------------------------------------------------------
C           Tank mass will drop below minimum; Not all of the
C           hydrogen demanded can be delivered:
C----------------------------------------------------------------------
C...........Rate of hydrogen delivery:
            fCyl_H2_discharge_future = fCyl_H2_demand -
     &               ( fCyl_H2_mass_MIN - fCyl_H2_mass_future )
     &                  / fTS_duration                          ! (kg/s)
            if (  fCyl_H2_discharge_future .lt. 0.0 )
     &                   fCyl_H2_discharge_future = 0.0

C...........Rate at which tank is charged:
            fCyl_H2_charge_future = fConn_Mass_Flow             ! (kg/s)
C...........Rate at which hydrogen is vented
            fCyl_H2_vent = 0.0                                  !(kg/s)

         else
C----------------------------------------------------------------------
C           Tank will charge/discharge normally
C----------------------------------------------------------------------
            fCyl_H2_discharge_future = fCyl_H2_demand                 ! (kg/s)
            fCyl_H2_charge_future    = fConn_Mass_Flow                ! (kg/s)
            fCyl_H2_vent             = 0.0                            ! (kg/s)

         endif


C----------------------------------------------------------------------
C        Recalculate future mass of hydrogen using adjusted charge/
C        discharge rates
C----------------------------------------------------------------------
         fCyl_H2_mass_future = fCyl_H2_mass_present
     &         + ( fConn_Mass_Flow
     &               - fCyl_H2_discharge_future - fCyl_H2_vent )
     *         * fTS_duration

C----------------------------------------------------------------------
C        Calculate total amount of hydrogen delivered through
C        implicitly-modelled connections
C----------------------------------------------------------------------
         if ( fCyl_H2_discharge_future .lt. fCyl_H2_demand_EXP ) then

C...........Explicit demand cannot be met. Set implicit supply to
C...........zero
            fH2_plt_IMP_supply_TOTAL( iComponent ) = 0.0
            
            fH2_plt_EXP_supply_TOTAL( iComponent ) =
     &             fCyl_H2_discharge_future
         
         else

C...........Explicit demand can be met, and at least some of
C...........the implicit demand can be met.
            
            fH2_plt_EXP_supply_TOTAL ( iComponent )
     &         = fCyl_H2_demand_EXP
            
            fH2_plt_IMP_supply_TOTAL (iComponent )
     &         = fCyl_H2_discharge_future
     &          - fH2_plt_EXP_supply_TOTAL ( iComponent )
     
         endif 

     
C----------------------------------------------------------------------
C        Calculate the matrix coefficients characterizing
C        the component's hydrogen balance:
C
C           | 1.0    0.0 | | mo |  = Hydrogen delivered
C                          | mi |
C
C        Where: mo is the flow through the cylinder, and
C               mi is the flow entering the cylinder.
C
C----------------------------------------------------------------------
         fCoefficients(1) = 1.0
         fCoefficients(2) = 0.0
         fCoefficients(3) = fH2_plt_EXP_supply_TOTAL (iComponent )

C----------------------------------------------------------------------
C        Store charge/discharge rates for reporting
C----------------------------------------------------------------------
         fReport_H2_mass_present(iComponent) = fCyl_H2_mass_present
         fReport_H2_mass_future (iComponent) = fCyl_H2_mass_Future
         fReport_H2_charge (iComponent)      = fCyl_H2_charge_future
         fReport_H2_discharge(iComponent)    = fCyl_H2_discharge_future
         fReport_H2_vent(iComponent)         = fCyl_H2_vent

         PCDatF(iComponent,1) = fCyl_H2_mass_Future         ! (kg)
         PCDatF(iComponent,2) = fCyl_H2_charge_Future       ! (kg/s)
         PCDatF(iComponent,3) = fCyl_H2_discharge_Future    ! (kg/s)

      else
C-----------------------------------------------------------------------
C        Energy blance matrix is to be solved. Collect most-recently
C        calculated future time-row mass, hydrogen charge & discharge
C        rates.
C-----------------------------------------------------------------------

         fCyl_H2_mass_Future       = PCDatF(iComponent,1)    ! (kg)
         fCyl_H2_charge_Future     = PCDatF(iComponent,2)    ! (kg/s)
         fCyl_H2_discharge_Future  = PCDatF(iComponent,3)    ! (kg/s)

         fCyl_H2_charge_Present    = PCDatP(iComponent,2)    ! (kg/s)
         fCyl_H2_discharge_Present = PCDatP(iComponent,3)    ! (kg/s)

C-------------------------------------------------------------------------
C        Collect constant-volume specific heats at present & future
C        temperatures (J/kg K)
C-------------------------------------------------------------------------

C........Fill Context buffer for for possible warning messages
C........Generated by chemical_properties.F
         write (cContext,'(A,I2,A)')
     &    'Gas cylinder model (plant component # ', iComponent,
     &    ')'

          fConn_H2_Cv_present  =
     &     fEval_Compound_Cv ( iHydrogen, fConn_Temp_Present + fC_to_K )
     &   / fEval_Compound_MM ( iHydrogen )
          fConn_H2_Cv_future   =
     &     fEval_Compound_Cv ( iHydrogen, fConn_Temp_Future + fC_to_K )
     &   / fEval_Compound_MM ( iHydrogen )
         fCyl_H2_Cv_present   =
     &     fEval_Compound_Cv ( iHydrogen, fCyl_Temp_Present + fC_to_K )
     &   / fEval_Compound_MM ( iHydrogen )
         fCyl_H2_Cv_future    =
     &     fEval_Compound_Cv ( iHydrogen, fCyl_Temp_future + fC_to_K )
     &   / fEval_Compound_MM ( iHydrogen )

C........Empty context buffer
         cContext = ' '

C-------------------------------------------------------------------------
C        Calculate time constant using present time row data
C
C                        H2 heat capacity + Cyl. heat capacity
C        time constant = -------------------------------------------  (s)
C                        connection heat capacity +
C                              tank->ambient heat xfer coefficient.
C
C-------------------------------------------------------------------------
         fCyl_Time_Constant =
     &     (   fCyl_H2_mass_present * fCyl_H2_Cv_present
     &       + fCyl_solid_mass * fCyl_solid_Cp           )
     &    /(   fConn_Mass_Flow * fConn_H2_Cv_present
     &       + fCyl_UA_ambient )

C........Save time constant in ESRU commonblock
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

            fCyl_solid_mass      = 0.0
            fCyl_H2_mass_present = 0.0
            fCyl_H2_Cv_future    = 0.0

         else
C...........Shouldn't happen, as impexp is error trapped elsewhere?
            STOP 'Comp_Cyl_Q_balance: Solution scheme is unsupported'
         endif

C----------------------------------------------------------------------
C        Calculate intermediate results, for determining
C        component matrix coefficients:
C
C        Cylinder heat capacitance (J/oC)
C
C        heat cap. =
C
C               [1-alpha) * ( H2 Cv present ) * ( present encapsulated H2 mass)
C
C               +  alpha  * ( H2 Cv future  ) * ( future encapsulated H2 mass )
C
C               + ( Cyl Cv ) (Cyl mass)
C
C----------------------------------------------------------------------
         fCyl_heat_cap = (
     &               ( 1 - fSoln_alpha )
     &                    * fCyl_H2_mass_present * fCyl_H2_Cv_present
     &                + fSoln_alpha
     &                    * fCyl_H2_mass_future  * fCyl_H2_Cv_future
     &                + fCyl_solid_mass * fCyl_solid_Cp
     &             )

C----------------------------------------------------------------------
C        Present time-row internal energy change of incomming
C        hydrogen
C
C        dU = ( inlet H2 mass flow ) * ( present inlet H2 cv )
C              * ( inlet present temperature
C                                        - cylinder present temperature )
C
C        (+ive value denotes transfer from the incomming H2 to the tank)
C----------------------------------------------------------------------
         fConn_dU_present  =   fCyl_H2_charge_present
     &       * fConn_H2_Cv_present
     &       * ( fConn_Temp_Present - fCyl_Temp_Present )

C----------------------------------------------------------------------
C        Present time-row flow work performed by charging tank (W)
C
C          Pv = ( inlet h2 mass flow ) * ( H2 ideal gas constant )
C                       * ( present time row connection temp )
C
C----------------------------------------------------------------------
         fCharge_Pv_present = fCyl_H2_charge_present
     &       * fR_hydrogen * ( fConn_Temp_Present + fC_to_K )


C----------------------------------------------------------------------
C        Future time-row flow work performed by charging tank (W)
C
C          Pv = ( inlet h2 mass flow ) * ( H2 ideal gas constant )
C                       * ( future time row connection temp )
C
C----------------------------------------------------------------------
         fCharge_Pv_future = fCyl_H2_charge_future
     &         * fR_hydrogen * ( fConn_Temp_future + fC_to_K )

C----------------------------------------------------------------------
C        Present time-row flow work performed by discharging tank (W)
C
C          Pv = ( outlet h2 mass flow ) * ( H2 ideal gas constant )
C                       * ( present time row connection temp )
C
C----------------------------------------------------------------------
         fDischarge_Pv_present = fCyl_H2_discharge_present
     &         * fR_hydrogen * ( fCyl_Temp_Present + fC_to_K )

C----------------------------------------------------------------------
C        Future time-row flow work performed by discharging tank (W)
C
C          Pv = ( outlet h2 mass flow ) * ( H2 ideal gas constant )
C                       * ( future time row connection temp )
C
C----------------------------------------------------------------------
         fDischarge_Pv_future = fCyl_H2_discharge_future
     &         * fR_hydrogen * ( fCyl_Temp_Future + fC_to_K )

C----------------------------------------------------------------------
C        Present time-row heat transfer to ambient
C
C        Q = ( cylinder UA coefficient )
C                  * ( cyliner present temperature
C                                      - present ambient temperature )
C
C        +ive result indicates heat transfer from cylinder to surroundings.
C
C----------------------------------------------------------------------
         fCyl_HeatLoss_present =
     &       fCyl_UA_ambient * ( fCyl_Temp_Present - fAmb_Temp_Present )

C-------------------------------------------------------------------------
C        Calculate Temperature matrix solution coefficients:
C
C          | a_1   a2  | | To |  =  | R |
C                        | Ti |
C
C        See Equation 70, Ferguson 2005
C
C
C        a1  = ( heat capacitance) / fTS_duration
C
C              +  alpha * [ (inlet H2 mass flow)
C                                * ( future inlet H2 Cv)
C                                + ( cylinder UA value ) ]
C
C-------------------------------------------------------------------------

         fCoefficients(1) =
     &             fCyl_heat_cap / fTS_duration
     &                + fSoln_alpha * (
     &                      fCyl_H2_charge_future * fConn_H2_Cv_future
     &                    + fCyl_UA_ambient )

C-------------------------------------------------------------------------
C
C        a2 = (- alpha)  * ( inlet H2 mass flow )
C                        * ( future inlet H2 Cv )
C
C-------------------------------------------------------------------------

         fCoefficients(2) = -1 * fSoln_alpha
     &                         * fCyl_H2_charge_future
     &                         * fConn_H2_Cv_future

C-------------------------------------------------------------------------
C
C        R = ( Cylinder heat capacitance ) / ( timestep duration )
C                       * ( cylinder present temperature )
C
C           + (1 - alpha) * [ ( present inlet H2 internal energy change )
C                             + ( present flow work done by charging )
C                             - ( present flow work done by discharging )
C                             - ( heat transferred to surroundsings
C                                ]

C            + alpha *  [  ( future flow work done by charging )
C                        - ( future flow work done by discharging )
C                        + ( cylinder UA coefficient )
C                             * future ambient temperature
C                        ]
C
C-------------------------------------------------------------------------
         fCoefficients(3) = (fCyl_heat_cap )
     &                      / fTS_duration * fCyl_Temp_Present
     &             + ( 1.0 - fSoln_alpha )
     &                   * (    fConn_dU_present
     &                        + fCharge_Pv_present
     &                        - fDischarge_Pv_present
     &                        - fCyl_HeatLoss_present
     &                     )
     &             + fSoln_alpha
     &                    * (  fCharge_Pv_future
     &                       - fDischarge_Pv_future
     &                       + fCyl_UA_ambient
     &                            * fAmb_Temp_Future  )



C-------------------------------------------------------------------------
C        Placeholder for initial testing: Set outlet temperature to
C        25 oC. Component energy balance is:
C
C              | 1.0    0.0 | | To |  = Outlet temperature.
C                             | Ti |
C
C           Where: mo is the flow through the cylinder, and
C                  mi is the flow entering the cylinder.
C
C--------------------------------------------------------------------------

      endif

C.....Debugging output
      if ( bDebug ) then
         call add_to_xml_reporting(
     &             fCyl_Volume,
     &             'debug/gas_cylinder/init/Volume',
     &             'units', '(m3)',
     &             ' ')
         call add_to_xml_reporting(
     &             fCyl_solid_mass,
     &             'debug/gas_cylinder/init/S_mass',
     &             'units', '(kg)',
     &             ' ')
         call add_to_xml_reporting(
     &             fCyl_solid_Cp,
     &             'debug/gas_cylinder/init/S_Cp',
     &             'units', '(J/kg oC)',
     &             ' ')
         call add_to_xml_reporting(
     &             fCyl_UA_ambient,
     &             'debug/gas_cylinder/init/UA',
     &             'units', '(W /oC)',
     &             ' ')
          call add_to_xml_reporting(
     &             fCyl_MAX_press,
     &             'debug/gas_cylinder/init/press/max',
     &             'units', '(kPa)',
     &             ' ')
          call add_to_xml_reporting(
     &             fCyl_MIN_press,
     &             'debug/gas_cylinder/init/press/min',
     &             'units', '(kPa)',
     &             ' ')
          call add_to_xml_reporting(
     &             fCyl_Init_press,
     &             'debug/gas_cylinder/init/press/init',
     &             'units', '(kPa)',
     &             ' ')

          call add_to_xml_reporting(
     &             fR_hydrogen,
     &             'debug/gas_cylinder/init/H2_R',
     &             'units', '(kJ/kg K)',
     &             ' ')

          call add_to_xml_reporting(
     &             fCyl_Temp_Present,
     &             'debug/gas_cylinder/node/temp/present',
     &             'units', '(oC)',
     &             ' ')

          call add_to_xml_reporting(
     &             fCyl_Temp_Future,
     &             'debug/gas_cylinder/node/temp/future',
     &             'units', '(oC)',
     &             ' ')

          call add_to_xml_reporting(
     &             fCyl_H2_demand,
     &             'debug/gas_cylinder/H2_demand',
     &             'units', '(kg/s)',
     &             ' ')

          call add_to_xml_reporting(
     &             fConn_Temp_Future,
     &             'debug/gas_cylinder/conn/temp/future',
     &             'units', '(oC)',
     &             ' ')

          call add_to_xml_reporting(
     &             fConn_Temp_Present,
     &             'debug/gas_cylinder/conn/temp/present',
     &             'units', '(oC)',
     &             ' ')

          call add_to_xml_reporting(
     &          fSoln_alpha,
     &          'debug/gas_cylinder/alpha',
     &          'units', '(W)',
     &          ' ')

         call add_to_xml_reporting(
     &          fCoefficients(1),
     &          'debug/gas_cylinder/coeff/1',
     &          'units', '(W)',
     &          ' ')
         call add_to_xml_reporting(
     &          fCoefficients(2),
     &          'debug/gas_cylinder/coeff/2',
     &          'units', '(W)',
     &          ' ')
         call add_to_xml_reporting(
     &          fCoefficients(3),
     &          'debug/gas_cylinder/coeff/3',
     &          'units', '(W)',
     &          ' ')

         call add_to_xml_reporting(
     &          fCharge_Pv_present,
     &          'debug/gas_cylinder/flow_work/charge/present',
     &          'units', '(W)',
     &          ' ')
         call add_to_xml_reporting(
     &          fCharge_Pv_future,
     &          'debug/gas_cylinder/flow_work/charge/future',
     &          'units', '(W)',
     &          ' ')

         call add_to_xml_reporting(
     &          fDischarge_Pv_present,
     &          'debug/gas_cylinder/flow_work/discharge/present',
     &          'units', '(W)',
     &          ' ')
         call add_to_xml_reporting(
     &          fDischarge_Pv_future,
     &          'debug/gas_cylinder/flow_work/discharge/future',
     &          'units', '(W)',
     &          ' ')

         call add_to_xml_reporting(
     &          fConn_dU_present,
     &          'debug/gas_cylinder/dU/charge/present',
     &          'units', '(W)',
     &          ' ')

         call add_to_xml_reporting(
     &          fCyl_HeatLoss_present,
     &          'debug/gas_cylinder/heat_loss/present',
     &          'units', '(W)',
     &          ' ')

          call add_to_xml_reporting(
     &          fConn_H2_Cv_future,
     &          'debug/gas_cylinder/H2/fConn_H2_Cv_future',
     &          'units', '(J/kg K)',
     &          ' ')

         call add_to_xml_reporting(
     &          fConn_H2_Cv_present,
     &          'debug/gas_cylinder/H2/fConn_H2_Cv_present',
     &          'units', '(J/kg K)',
     &          ' ')
          call add_to_xml_reporting(
     &          fCyl_H2_Cv_future,
     &          'debug/gas_cylinder/H2/fCyl_H2_Cv_future',
     &          'units', '(J/kg K)',
     &          ' ')

         call add_to_xml_reporting(
     &          fCyl_H2_Cv_present,
     &          'debug/gas_cylinder/H2/fCyl_H2_Cv_present',
     &          'units', '(J/kg K)',
     &          ' ')

         call add_to_xml_reporting(
     &          fCyl_heat_cap,
     &          'debug/gas_cylinder/heat_capacitance',
     &          'units', '(J/kg K)',
     &          ' ')
      endif


      return
      end




C---------------- Comp_Cyl_Validate_Inputs ----------------------------
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.
C
C ABSTRACT:
C This routine checks the inputs to the compressed cylinder model
C and issues warnings or halts the simulation if errors are found.
C
C Inputs:
C
C   iComponent   - index of component in network
C   fCyl_Volume        - Cylinder volume (m3)
C   fCyl_solid_mass    - Solid mass of the cylinder (kg)
C   fCyl_solid_Cp      - Specific heat capacity of the cylinder (J/kg K)
C   fCyl_UA_ambient    - Coefficient of heat transfer to ambient (W/oC)
C   fCyl_MIN_press     - Minimum pressure in the cylinder  (kPa)
C   fCyl_MAX_press     - Maximum pressure in the cylinder  (kPa)
C   fCyl_Init_press    - Initial pressure in the cylinder  (kPa)
C
C Outputs
C  None
C----------------------------------------------------------------------
      subroutine Comp_Cyl_Validate_Inputs (
     &                             iComponent,
     &                             fCyl_Volume,
     &                             fCyl_solid_mass,
     &                             fCyl_solid_Cp,
     &                             fCyl_UA_ambient,
     &                             fCyl_MIN_press,
     &                             fCyl_MAX_press,
     &                             fCyl_Init_press,
     &                             iGas_LAW)

      implicit none
#include "building.h"
C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent, iGas_LAW
      real  fCyl_Volume, fCyl_solid_mass, fCyl_solid_Cp,
     &      fCyl_UA_ambient, fCyl_MIN_press, fCyl_MAX_press,
     &      fCyl_Init_press

C----------------------------------------------------------------------
C     ESP-r commons for inputs/outputs
C----------------------------------------------------------------------
C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      logical bClose_To_Zero  ! result of close to zero comparison
      real fSmall             ! tolerance for close to zero
      parameter ( fSmall = 0.000001 )

      logical bFatal_error    ! flag indicating simulation must be halted

C----------------------------------------------------------------------
C     Error trapping.
C----------------------------------------------------------------------

      bFatal_error = .false.

C.....Cylinder volume: must be greater than zero
      call eclose( fCyl_Volume, 0.0, fSmall, bClose_To_Zero )
      if ( bClose_To_Zero .or. fCyl_Volume .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder volume must'
     &                 // ' be greater than zero!'
         write (itu,'(A,I2)')
     &                 '                          Check input # 1 in'
     &                 // ' plant component # ', iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Cylinder mass cannot be less than zero
      call eclose( fCyl_solid_mass, 0.0, fSmall, bClose_To_Zero )
      if ( .not. bClose_To_Zero .and. fCyl_solid_mass .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder mass cannot'
     &                 // ' be less than zero!'
         write (itu,'(A,I2)')
     &                 '                          Check input # 2 in'
     &                 // ' plant component # ', iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Cylinder Cp cannot be less than zero
      call eclose( fCyl_solid_Cp, 0.0, fSmall, bClose_To_Zero )
      if ( .not. bClose_To_Zero .and. fCyl_solid_Cp  .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder specific heat'
     &                 // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                          Check input # 3 in'
     &                 // ' plant component # ', iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.
      endif

C.....Cylinder UA cannot be less than zero
      call eclose( fCyl_UA_ambient, 0.0, fSmall, bClose_To_Zero )
      if ( .not. bClose_To_Zero .and. fCyl_UA_ambient .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder UA value'
     &                 // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                          Check input # 4 in'
     &                 // ' plant component # ', iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.

      endif

C.....Cylinder minimum pressure cannot be less than zero
      call eclose( fCyl_MIN_press, 0.0, fSmall, bClose_To_Zero )
      if ( .not. bClose_To_Zero .and. fCyl_MIN_press .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder MIN pressure'
     &                 // ' cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                          Check input # 5 in'
     &                 // ' plant component # ', iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.

      endif


C.....Cylinder maxmimu pressure must be greater than minimum

      if ( fCyl_MIN_press .gt. fCyl_MAX_press ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder MIN pressure'
     &                 // ' cannot be greater than max pressure!'
         write (itu,'(A,I2)')
     &                 '                          Check inputs # 5'
     &                 // ' and # 6 in plant component # ',
     &                 iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.

      endif

C.....Cylinder initial pressure cannot be less than zero
      call eclose( fCyl_Init_press, 0.0, fSmall, bClose_To_Zero )
      if ( .not. bClose_To_Zero .and. fCyl_Init_press .lt. 0.0 ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Fatal error!'
         write (itu,*) '                         Cylinder initial'
     &                 // ' pressure cannot be less than zero!'
         write (itu,'(A,I2)')
     &                 '                          Check input # 7 in'
     &                 // ' plant component # ', iComponent
         write (itu,'(A)') ' '
         bFatal_error = .true.

      endif

C.....Cylinder initial pressure should not be greater than max,
C.....warn user but do not halt simulation as the excess h2
C.....will merely be vented.

      if (  fCyl_Init_press .gt. fCyl_MAX_press ) then
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Warning!'
         write (itu,*) '                         Cylinder initial'
     &                 // ' pressure is greater than maximum pressure.'

         write (itu,'(A,I2)')
     &                 '                          Check inputs # 6'
     &                 // ' and # 7 in plant component # ',
     &                 iComponent
         write (itu,'(A)') ' '
      endif


C.....Check that specified gas model is one of supported gas models
C.....(presently only the ideal gas model is supported)

      if ( iGAS_LAW .ne. 1) then
         iGas_LAW = 1
         write (itu,'(A)') ' '
         write (itu,*) 'Compressed gas cylinder: Warning!'
         write (itu,*) '                         Specified'
     &                 // ' gas law is unsupported. Using ideal gas'
     &                 // ' law instead.'

         write (itu,'(A,I2)')
     &                 '                          Check input # 8'
     &                 // ' in plant component # ',
     &                 iComponent

         write (itu,'(A)') ' '
      endif

      if ( bFatal_error ) then
         write (itu,*) 'Compressed gas cylinder: Halting simulation!'
         stop
      endif

      return
      end

C---------------- Comp_Cyl_PostPro -------------------------------------
C
C Date:       Feb 17, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.
C
C ABSTRACT:
C This is a postprocessing and reporting routine for the
C compressed gas cylinder model.
C
C Inputs:
C
C  iComponent       - index of component in plant network
C
C Outputs
C  None
C----------------------------------------------------------------------
      subroutine Comp_Cyl_PostPro(iComponent, iStatus)
      implicit none
#include "plant.h"
#include "chemical_properties.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iComponent        ! pointer to comp. in network
      integer iStatus                 ! Flag indicating comp. status.

C----------------------------------------------------------------------
C     ESP-r common blocks
C----------------------------------------------------------------------

C.....Plant component ascii names
      common/pcnam/pcname(mpcom)
      character*15 pcname       ! Plant component names

C.....Additional plant component data requiring time-row transport.
      common/pcdat/
     &     pcdatf(mpcom,mpcdat),
     &     pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Plant present and future state variables
      common/pcval/csvf,csvp
      real csvf(mpnode,mpvar)   ! future time-row state variables
      real csvp(mpnode,mpvar)   ! present time-row state variables

C.....Misc. plant component data.
      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

C----------------------------------------------------------------------
C     Model commons
C----------------------------------------------------------------------

C.....Compressed cylinder results.
      common / Comp_cyl_results /
     &       fReport_H2_mass_present     ( MPCom ),
     &       fReport_H2_mass_future      ( MPCom ),
     &       fReport_H2_charge           ( MPCom ),
     &       fReport_H2_discharge        ( MPcom ),
     &       fReport_H2_vent             ( MPcom ),
     &       fReport_H2_pressure_present ( MPCom ),
     &       fReport_H2_pressure_future  ( MPCom ),
     &       iStore_Connect_index        ( MPCom )

      real fReport_H2_mass_present
      real fReport_H2_mass_future
      real fReport_H2_charge
      real fReport_H2_discharge
      real fReport_H2_vent
      real fReport_H2_pressure_present
      real fReport_H2_pressure_future
      integer iStore_Connect_index

C.....Named constants:
C.....reporting module status
      integer iInitialize, iReport
      parameter ( iInitialize  = 1,
     &            iReport      = 2 )


C.....Celsius -> Kelvin conversion
      real fC_to_K
      parameter ( fC_to_K  = 273.15 )

C.....Properties
      integer iPropTemp         ! named constant for temperature property
      integer iProp1stFlow      ! named constant for first phase flow property
      integer iProp2ndFlow      ! named constant for second phase flow property
      integer iPropH2Flow       ! named constant for hydrogen flow
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      character*128 H3Kname       ! holding variable
      character*128 char_temp     ! holding variable
      integer iLength,iNameLength ! Length of strings

C.....Arrays storing locations of output variables
      integer iStorage_index(mpcom)
      save iStorage_index

C.....Node index
      integer iNode_index

C.....Node temperatures
      real fCyl_H2_temp_present       ! (K)
      real fCyl_H2_temp_future        ! (K)
C.....Cylinder Volume
      real fCyl_Volume                ! (m3)
C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real fCompound_R          ! function returing ideal gas constant
                                ! a compound

      real H3K_Connect_property ! function returning properties
                                ! associated with a connection


      if ( iStatus .eq. iInitialize ) then
C----------------------------------------------------------------------
C        Initialize reports
C----------------------------------------------------------------------
C........Get component name and length
         write(char_temp,'(A,A,A)')
     &      'plant/',
     &      pcname(iComponent)(1:lnblnk(pcname(iComponent))),
     &      '/misc_data'
         call h3k_StoreString(char_temp,
     &                        iStorage_index(iComponent),
     &                        iLength)

      else

C-----------------------------------------------------------------------
C     Collect cylinder volume
C-----------------------------------------------------------------------
      fCyl_Volume = ADATA ( iComponent, 1)    ! (m3)
C-----------------------------------------------------------------------
C     Recover present & future time-row tempertatures
C-----------------------------------------------------------------------
      iNode_index = NPCDAT ( iComponent, 9 )
      fCyl_H2_temp_present = CSVP( iNode_index, 1 ) + fC_to_K  ! (K)
      fCyl_H2_temp_future  = CSVF( iNode_index, 1 ) + fC_to_K  ! (K)

C----------------------------------------------------------------------
C     Calculate tank pressure using ideal gas law (P = mRT / V )
C----------------------------------------------------------------------
      fReport_H2_pressure_present ( iComponent ) =
     &      fReport_H2_mass_present ( iComponent )
     &    * fCompound_R ( iHydrogen ) / 1000.0
     &    * fCyl_H2_Temp_Present / fCyl_Volume

      fReport_H2_pressure_future ( iComponent ) =
     &      fReport_H2_mass_future ( iComponent )
     &    * fCompound_R ( iHydrogen ) / 1000.0
     &    * fCyl_H2_Temp_future / fCyl_Volume
C----------------------------------------------------------------------
C     Recover Component name
C----------------------------------------------------------------------
      call h3k_RecoverString(iStorage_index(iComponent),
     &                       char_temp, iNameLength)

C--------------------------------------------
C.....Output data: Present time-row mass
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/mass/present'
      call add_to_xml_reporting(
     &   fReport_H2_mass_present(iComponent),
     &   H3Kname,
     &   'units', '(kg)',
     &   'Compressed gas cylinder: hydrogen mass at present time-row'
     &   )

C--------------------------------------------
C.....Output data: Future time-row mass
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/mass/future'
      call add_to_xml_reporting(
     &   fReport_H2_mass_future(iComponent),
     &   H3Kname,
     &   'units', '(kg)',
     &   'Compressed gas cylinder: hydrogen mass at future time-row'
     &   )

C--------------------------------------------
C.....Output data: H2 charge rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/hydrogen_charge_rate'
      call add_to_xml_reporting(
     &   fReport_H2_charge(iComponent),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'Compressed gas cylinder: rate of charge'
     &   )

C--------------------------------------------
C.....Output data: H2 discharge rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/hydrogen_discharge_rate'
      call add_to_xml_reporting(
     &   fReport_H2_discharge(iComponent),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'Compressed gas cylinder: rate of discharge'
     &   )

C--------------------------------------------
C.....Output data: Flow rate @ vent
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/vent_H2_rate'
      call add_to_xml_reporting(
     &   fReport_H2_vent(iComponent),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'Compressed gas cylinder: rate at which excess hydrogen '
     &    // 'is vented' )

C----------------------------------------------------------------------
C--------------------------------------------
C.....Output data: Pressure at present time-row
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/pressure/present'
      call add_to_xml_reporting(
     &   fReport_H2_pressure_present(iComponent),
     &   H3Kname,
     &   'units', '(kPa)',
     &   'Compressed gas cylinder: gas pressure at present time-row '
     &    )
C--------------------------------------------
C.....Output data: Pressure at future time-row
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/pressure/future'
      call add_to_xml_reporting(
     &   fReport_H2_pressure_future(iComponent),
     &   H3Kname,
     &   'units', '(kPa)',
     &   'Compressed gas cylinder: gas pressure at future time-row '
     &    )

C----------------------------------------------------------------------
C     Save connection temperature for next time-row
C----------------------------------------------------------------------
      PcDatF (iComponent, 5) = H3K_Connect_property(
     &           iStore_Connect_index ( iComponent ),
     &           iPropTemp )


      endif
      return
      end



               
