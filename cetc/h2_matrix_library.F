
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
C
C--------------------  HVAC_model_static_temp()   -------------------
C     Created by: Alex Ferguson
C     Creation date: June 2003
C     Copyright: CETC 2003
C-------------------------------------------------------------------------- 
C     This subroutine reads a plant component description checks it's 
C     connections for conformance with the model topology. 
C 
C     INPUTS:
C       + Data contained in misc common blocks
C           - NCI(x): # of controlled variables for component x
C           - CDATA(x,y): control data for component x
C           - NPCDAT(x,y): misc. data for component x
C           - ISV(x,y): "type" of node y in component x
C                     =  0,10,20 node represents water + ....
C                     =  1,11,21 node represents dry air + ....
C                     =  9,19,29 node represents some solid material only
C                     =   0 - 9  model suitable for energy balance only
C                     =  10- 19  model suitable for energy 
C                                + single phase mass balance
C                     =  20 - 19 model suitable for energy 
C                                + two phase mass balances
C           - ICONTP(x): "type" of connection for node x - same as ISV
C           - ICONDX( ): connection number for node/connection
C     
C     The routine also sets the  bComponent_status(iComponent) flag 
C     to false, indicating that the component has not been initialized. 
C     this permits model-specific initialization operations to be performed
C     in the coefficient generator.
C     
C--------------------------------------------------------------------------
      subroutine HVAC_model_static_temp(iComponent)
      implicit none
#include "building.h"
#include "plant.h"

C--------------------------------------------------------------------------
C     ESP-r common blocks
C--------------------------------------------------------------------------
C     -IUOUT: Write unit for error trace
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout, iuin
C     -ITU:  Write unit numnbers for warning/information trace
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      integer itcf, itrace, izntrc, itu
      common/c9/
     &     npcomp,              ! number of components in network
     &     nci(mpcom),          ! number of controlled variables per component
     &     cdata(mpcom,mmiscd)  ! component control data
      integer npcomp, nci
      real cdata
      
      common/c14ps/
     &     ndcon(mpcom,mnodec), ! pointers to node connections
     &     isv(mpcom,mnodec)    ! node state & supported simulation types
      integer ndcon, isv
      
      common/c12ps/
     &     npcdat(mpcom,9),     ! miscellaneous component data
     &     ipofs1(mcoefg),      ! component coefficient locations (rows)
     &     ipofs2(mcoefg,mpvar) ! component coefficient locations (cols)
      integer npcdat, ipofs1, ipofs2

      common/pcond/
     &     convar(mpcon,mconvr), ! state varibles for connections
     &     icontp(mpcon),       ! type of each connection 
     &     icondx(mpcom,mnodec,mpconc) ! pointer to connections for each component/node
      real convar
      integer icontp, icondx

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
C     MISC variable declarations

      integer iComponent        ! index of component in network

      integer i , j             ! Counters

      integer iNode_State,      ! describes the matter that a type or 
     &     iConn_State          ! connection represents (water/air/solid)

      integer iNode_SIMtype,    ! describes the type of simulation that
     &     iConn_SIMtype        ! a connection or node may be used in

      integer iControlCount    ! number of controlled variables
      integer iNodeCount       ! number of nodes in model
      integer iConnect_Count(mnodec)  ! number of connections / node
      integer iNodeIndex       ! index of current node
      integer iConnectionIndex ! index of current connection
      integer iTotal_num_connect ! number of connections in component

      logical bFatalError

C.....References
      character cSim_Type_Name, cState_Name
      
C------------------------------------------------------------------------------------
C     SET number of controlled variables
C------------------------------------------------------------------------------------
      iControlCount =  nci(iComponent)
    
C------------------------------------------------------------------------------------
C     READ number of nodes in current component
C     .  - NPCDAT: common variable containing misc. data for each 
C     .    component in the plant network
C------------------------------------------------------------------------------------
      iNodeCount = npcdat( iComponent, 8)

C------------------------------------------------------------------------------------
C     COLLECT node data - loop through each node in the component
C------------------------------------------------------------------------------------
      iTotal_num_connect = 0    ! Initialize counter for total number of connections
                                ! to current model

      h2FlowSupport(iComponent) = .false.                          
                                
      do i=1, iNodeCount
C------------------------------------------------------------------------------------
C     Get Number of current node
C------------------------------------------------------------------------------------
         iNodeIndex = npcdat(iComponent,9)+(i-1)

C-----------------------------------------------------------------------------------
C     Collect node type form ISV variable and call subroutine ISV_convert 
C     to break up ISV value into two useful variables:
C
C     .   SIMtype -> first digit of ISV, describes the material that the node
C     .              represents
C
C     .   state   -> second digit of ISV, describes the types of simulations that 
C     .              the node may be used in
C
C     Assign SIMtype and state to node.
C------------------------------------------------------------------------------------

         call ISV_convert(isv(iComponent,i),
     &        iNode_State, iNode_SIMtype)

C------------------------------------------------------------------------------------
C     Check node state for hydrogen-flow support
C------------------------------------------------------------------------------------
         if ( iNode_state .eq. 2 ) then
            h2FlowSupport(iComponent) = .true.
         endif

         
         do j=1, mpconc         !<- max number of connections per node
C------------------------------------------------------------------------------------
C     Check ICONDX array: if non-zero, connection exists. Check connection
C     state / simulation type for conformance with connected node
C------------------------------------------------------------------------------------
            iConnectionIndex = ICONDX ( iComponent, i, j )
            if (iConnectionIndex .gt. 0 )  then

C..............Connection exists            
               
               call ISV_convert(
     &              icontp (iConnectionIndex),
     &              iConn_State, 
     &              iConn_SIMtype
     &              )

               iConnect_count(i) = iConnect_count(i) + 1
     
               if ( iConn_state .NE. iNode_state ) then
               
                  WRITE(IUOUT,*)  'Plant Component Connectivity Error:'
                  WRITE(IUOUT,11) ' - Plant component: ',iComponent,
     &                 ' Node: ',i,
     &                 ' Connection: ',iConnect_count(i)
                  WRITE(IUOUT,12) '   (Connection # in plant network: ',
     &                 ICONDX(iComponent,i,iConnect_count(i)),')'

                  WRITE(IUOUT,13) ' - Receiving node is type ', 
     &                 iNode_State, ' (',cState_Name(iNode_State),')'
                  WRITE(IUOUT,13) ' - Sending node is type ', 
     &                 iConn_State, ' (',cState_Name(iConn_State),')'
                  
                  bFatalError = .TRUE.
                                ! Error message to go here.
               endif

               if ( iConn_SIMType .NE. iNODE_SIMType ) then
                  WRITE(IUOUT,*)  'Plant Component Connectivity Error:'
                  WRITE(IUOUT,11) ' - Plant component: ',iComponent,
     &                 ' Node: ',i,
     &                 ' Connection: ',iConnect_count(i)
                  WRITE(IUOUT,12) '   (Connection # in plant network: ',
     &                 ICONDX(iComponent,i,iConnect_count(i)),')'
                  WRITE(IUOUT,13) ' - Receiving node supports type',
     &                 iNode_State, ' simulations (',
     &                 cSim_Type_Name(iNode_State),')'
                  WRITE(IUOUT,13) ' - Sending node supports type', 
     &                 iConn_State, ' simulations (',
     &                 cSim_Type_Name(iConn_State),')'
               endif
                  
            endif 
 
         enddo

      enddo
C------------------------------------------------------------------------------------
C     Set component's initialization flag status to false
C------------------------------------------------------------------------------------
      bInitialized(iComponent) = .false.

C------------------------------------------------------------------------------------
C     Check if component requires inter-domain iteration
C------------------------------------------------------------------------------------
      
      if ( npcdat ( iComponent, 4 )  .eq. 87  .or.
     &     npcdat ( iComponent, 4 )  .eq. 89  .or.
     &     npcdat ( iComponent, 4 )  .eq. 102 .or.
     &     npcdat ( iComponent, 4 )  .eq. 103  ) then
C........Component is an electrolyzer,h2 converter or stirling engine ---
C........interdomain iteration required. (NOTE: when the PEMFC or SOFC models
C........are active, the model-specific static template sets this variable.
         bInter_domain_iteration = .true.
      endif

C-------------------------------------------------------------------------
C     Misc. Format statements
C-------------------------------------------------------------------------
 11   FORMAT(A,I3,A,I3,A,I3)
 12   FORMAT(A,I3,A)
 13   FORMAT(A,I3,A,I3,A)



C---------------------------------------------------------------------------
C     Kill simulation, if nesessary.
C---------------------------------------------------------------------------
      if (bFatalError) STOP 'HVAC_model_static_temp: Fatal ERROR!'

  
      return
      end

C--------------------------------------------------------------------------
C     ABSTRACT:
C
C     These are simple character functions that return the name of a state
C     or simulation type described by ISV variables
C
C--------------------------------------------------------------------------
      CHARACTER FUNCTION cState_Name(State)
      IMPLICIT NONE
      INTEGER State
      IF( State .EQ. 0 ) THEN 
         cState_Name = 'water'
      ELSEIF( State .EQ. 1 ) THEN 
         cState_Name = 'air'
      ELSEIF( State .EQ. 9 ) THEN
         cState_Name = 'solid'
      ELSE
         cState_Name = 'Unknown state type'
      ENDIF
      END  ! of FUNCTION 
      
      CHARACTER FUNCTION cSim_Type_Name(Sim_Type)
      IMPLICIT NONE
      INTEGER Sim_Type
      IF( Sim_Type .EQ. 0 ) THEN 
         cSim_Type_Name = 'energy balance simulation only'
      ELSEIF( Sim_Type .EQ. 1 ) THEN 
         cSim_Type_Name = 'energy + 1-phase mass balance simulation'
      ELSEIF( Sim_Type .EQ. 2 ) THEN
         cSim_Type_Name = 'energy + 1-phase mass balance simulation'
      ELSE
         cSim_Type_Name = 'unknown simulation type'
      ENDIF
      END  ! of FUNCTION 






      
C====================== H2_Elec_converter ===============================
C
C     Created by: Alex Ferguson
C     Created on: September 07, 2004
C     Copyright: CETC
C     ________
C     ABSTRACT: 
C 
C     This subroutine creates a set of ESP-r plant matrix coefficients for
C     a hydrogen --> electricity convesion device.
C
C     istats = 1 -> temperature matrix:
C     
C     - set first component's node temperature to arbitrary value.
C     - set subsequent components' temperature to the value of the preceeding 
C     . component
C
C     istats = 2 -> 1st phase flow: 
C     
C     - set first phase (water/air) flow to zero 
C     
C     istats = 3 -> 2nd phase flow:
C
C     - set second phase (water/air) flow to zero
C
C     istats = 4 -> hydrogen flow:
C
C     - set first component's h2 flow to arbitrary value.
C     - set subsequent components' flow to the value of the preceeding 
C     component
C
C     __________
C     REFERENCES:
C
C     NRCan 2004. ``System Design for Wind-powered Hydrogen-Based Building
C     .    Cogeneration Project for Standard Hydrogen''. Internal Document, 
C     .    Revised Aug 17, 2004.

C===============================================================================

      subroutine H2_Elec_converter(
     &     iComponent,
     &     fCoeffArray,
     &     iStats)
      implicit none 

#include "plant.h"
#include "building.h"
#include "CETC_definitions.h" 

C External functions.     
      integer lnblnk

C-------------------------------------------------------------------------------
C     Passed variables
C-------------------------------------------------------------------------------
      integer iComponent        ! pointer to component 
      integer iStats            ! flag indicating which matrix is being solved
                                ! istats = 1 -> temperature
                                ! istats = 2 -> 1st phase flow
                                ! istats = 3 -> 2nd phase flow
                                ! istats = 4 -> hydrogen flow

      real fCoeffArray(mpcoe)   ! array of matrix coefficients for 
                                ! given component

      integer iCount            ! counter
      
C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------

      common/c9/npcomp,nci,cdata
      integer npcomp            ! number of plant components 
      integer nci(mpcom)        ! number of controls / component 
      real cdata(mpcom,mmiscd)  ! control data for each component 

      common/c10/npcon,ipc1,ipn1,ipct,ipc2,ipn2,pcondr,pconsd
      integer npcon             ! number of connections in plant network
      integer ipc1(mpcon)       ! connection -> index of recieving component
      integer ipn1(mpcon)       ! connection -> index of recieving node
      integer ipct(mpcon)       ! connection -> type
      integer ipc2(mpcon)       ! connection -> index of sending component 
      integer ipn2(mpcon)       ! connection -> index of sending node
      real pcondr(mpcon)        ! connection -> mass diversion ratio
      real pconsd(mpcon,2)      ! connection -> const. temperature / humid ratio

      common/pcond/convar,icontp,icondx
      real convar(mpcon,mconvr) ! connection -> temperature/flow data 
      integer icontp(mpcon)     !  connection -> type
      integer icondx(mpcom,mnodec,mpconc) ! connection index for each 
                                !  component / node / connection.

      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component 
      real bdata(mpcom,mbdata) ! more miscellaneous data for component 

      common/pcnam/pcname(mpcom)  
      character*15 pcname       ! Plant component names

      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar) 
      integer npcdat            ! miscellaneous plant data
      integer ipofs1            ! matrix coefficient location (rows)
      integer ipofs2            ! matrix coefficient location (columns)

      common / pcval / csvf, csvp
      real csvf(mpnode,mpvar)   ! future values of state variables
      real csvp(mpnode,mpvar)   ! present values of state variables 

      common/piter/maxitp,perrel,pertmp,perflx,permfl,itrclp,
     &             icsv,csvi
      integer maxitp            ! not used in current context
      integer itrclp            ! not used in current context
      integer icsv(mpnode,mpvar)! flag marking nodes for iteration
      real perrel               ! not used in current context
      real pertmp               ! not used in current context
      real perflx               ! not used in current context
      real permfl               ! not used in current context
      real csvi(mpnode,mpvar)   ! 'initial' values of state variables
                                ! at start of iteration

C.....Electrical power use of plant 'hybrid' components
      common/elpcp/npel, pfp, ipfp, pwrp, bvoltp, iphp
      integer npel              ! number of "elctrified" plant components 
      real pfp(mpcom)           ! power factor of componet i (0->1)
      integer ipfp(mpcom)       ! integer indicating if power factor lags (-1) or leads (+1)
      real pwrp(mpcom)          ! real power consumption of component (generation is -ive)
      real bvoltp(mpcom)        ! opertional voltage of component 
      integer iphp(mpcom)       ! phase component is connected to -> dummy variable

C.....Trace/error messages
      common/trace/itcf,itrace,izntrc,itu
      integer itcf,itrace(mtrace),izntrc(mcom),itu

C.....Additional data 
      common/pcdat/pcdatf(mpcom,mpcdat), pcdatp(mpcom,mpcdat)  
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)
      
      common/pcres/qdata(mpcom),pcaout(mpcom,mpcres),napdat(mpcom)         
      real qdata     ! not used in current context
      real pcaout    ! not used in current context
      integer napdat ! # of plant additional outputs

C.....Common for additional iteration criteria
      common / pAdd_iter_criteria /
     &   iPlt_Output_Iter_Flag,
     &   fPlt_Output_Init_Val,
     &   fPlt_Output_Tol 
      integer iPlt_Output_Iter_Flag (mpcom,mpcdat) ! flag denoting iteration is req'd
                                                   ! for plt additional outputs
      real fPlt_Output_Init_Val ( mpcom,mpcdat)    ! initial value of plt additional output
      real fPlt_Output_Tol (mpcom,mpcdat)          ! tolerance for convergence
      
      
C.....Common storing status of plant initialization. 
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)

C.....Temporary common used to transport data to H2 storage tank model. 
C.....(to be replaced by a control?)
      common / temp_control / fH2FlowDemandKGs

C-------------------------------------------------------------------------------
C     Local variables
C------------------------------------------------------------------------------

C.....System description
      integer iOperatingMode ! flag indicating if component is an electrolyzer 
                                ! or an h2->electric conversion device.

      integer iElectrolyzer     ! named constant for electrolyzer operating mode
      parameter ( iElectrolyzer = 87 )

      integer iH2converter      ! named constant for h2 conversion device
      parameter ( iH2converter  = 89 )

      real fElecCoeff0,         ! system electrical performance coefficients
     &     fElecCoeff1,         ! 
     &     fElecCoeff2          !
      
      real fTherCoeff0,         ! system thermal performance coefficients 
     &     fTherCoeff1,         ! 
     &     fTherCoeff2          !    

      real fH2massFlowMax       ! maximum mass flow (kg/s)
      real fH2massFlowMin       ! minimum mass flow (kg/s)

C.....Flags 
      logical bNumsAreClose     ! flag for close-to-X comparisons
      logical bFatalError       ! flag for fatal error

C.....Operating conditions / calculated data

      real fElecEfficiency      ! efficiency of electrical <-> chemical conversion (-)
      real fTherEfficiency      ! efficiency of heat production (-)


      real fControlFlux         ! Electrical supply/demand requested by controller (W)
      real fElecFluxDemand      ! Electrical supply/demand, constrained by system size (W)
      real fElecFluxActual(mpcom) ! Actual Electricity flowing to/from device (W)

      logical bNoElecFlux       ! flag indicating that there is no electrical 
                                ! flux to/from the component

      logical bInOperative      ! flag indicating that system is inoperative 

      real fTherFlux            ! Heat flowing to/from device (W)

      real fElecFluxMAX(mpcom)  ! Electrical flux corresponding to maximum H2 flow (W)
      real fElecFluxMIN(mpcom)  ! Electrical flux corresponding to mimimum H2 flow (W)

      real fFuel_LHV            ! lower heating value of supplemental fuel (J/kg)
      real fH2_LHV              ! Hydrogen lower heating value (J/kmol)
      parameter ( fH2_LHV = 242507000. ) 
      
      real fH2Fraction          ! Supplemental fuel stoicheiometry (kg H2 / kg total)

      real fAverageLHV          ! average heating value of hydrogen/hydrocarbon 
                                ! mixture (J/kg)

      real fElecUnderSized      ! Amount of electrical load that can't be delivered
                                ! due to system size 
      real fElecH2Shortage      ! Amount of electrical load that can't be delivered 
                                ! due to insufficint hydrogen

      real fEnergyLoss          ! System losses = amount of energy supplied to
                                ! the electrolyzer that is not converted to H2
                                ! = amount of energy not converted to
                                ! electric power or thermal energy for the H2 converter (W)

      real fLoadRatio           ! Operating point of electrolyzer (fraction
                                ! of full load (dimensionless)



C.....Dummy varibles
      real fDummy1, fDummy2     ! Dummy variables
      integer iEModel           ! flag indicating which calculation should be 
                                ! performaed by EMACH subroutine
C.....Connection data 
      integer iConnection       ! index of component's connection in plant network
      real fConnTempC           ! Temperature of incomming H2 (oC)
      real fConnTempK           ! Temperature of incomming H2 (K) 
      real fConnH2Flow          ! Flow rate of incomming H2 (kg/s)

      logical bNoH2Flow         ! logical flag indicating that hydrogen is 
                                ! flowing in system

      real fH2molarMass         ! Named constant: molar mass of hydrogen (kg/kmol)
      parameter ( fH2molarMass = 2.016 ) ! VAN WYLEN 1994

      real fO2molarMass         ! Named constant: molar mass of oxygen (kg/kmol)
      parameter ( fO2molarMass = 31.999 ) ! VAN WYLEN and SONNTAG 1985

      real fH2OmolarMass        ! Named constant: molar mass of water (kg/kmol)
      parameter ( fH2OmolarMass = 18.0155 ) ! VAN WYLEN and SONNTAG 1985

      real fH2FlowDemandKGs     ! hydrogen flow rate through component, to meet 
                                ! targeted operating point
      real fH2FlowActualKGs     ! Actual hydrogen flow through component
      real fH2FlowActualKMOLs   ! Flow in kMol of H2 (kmol/s)

      real fO2FlowKGs           ! rate of oxygen produced by electrolyzer (kg/s)
      real fO2FlowKMOLs         ! rate of oxygen produced by electrolyzer (kmol/s)

      real fH2OFlowKGs          ! rate of water consumed by electrolyzer (kg/s)
      real fH2OFlowKMOLs        ! rate of water consumed by electrolyzer (kmol/s)


      real fFuelFlowDemandKGs   ! Fuel flow rate demanded by hydrogen conversion
                                ! device

      real fFuelFlowActualKGs   ! Fuel flow rate received by hydrogen conversion 
                                ! device

      integer iNode             ! pointer to component's node in network

C.....H3K reporting
      CHARACTER*128 H3K_rep_NAME ! H3Kreporting object
      character*128 root_name    ! root name of component node 
      character*128 hold_name    ! temporary variable for string manipulation

C-------------------------------------------------------------------------------
C     Named constants
C-------------------------------------------------------------------------------
      real fToleranceWattsOrKelvin ! named constants describing tolerance of
                                   ! close-to-zero comparisons
      real fToleranceKG    

C-------------------------------------------------------------------------------
C     References
C-------------------------------------------------------------------------------
      real fFirstCubicRoot      ! function used to obtain the lowest, 
                                ! non-negative root for a cubic equation.
      real Elec_Net_Load_Calc   ! function performing miscellaneous calculations
                                ! on the electrical network
      real Elec_Net_Plant_Calc  ! function performing miscellaneous calculations
                                ! on the electrical network, which can
                                ! eclude a component from the load/gen calculation
C-------------------------------------------------------------------------------
C     Saved data
C-------------------------------------------------------------------------------
      save fElecFluxMAX, fElecFluxMIN, fElecFluxActual

C-------------------------------------------------------------------------------
C     Initialize variables
C-------------------------------------------------------------------------------
C.....Set Tolerance for close-to zero comparisons
      fToleranceWattsOrKelvin = 10. ! Watts, K
      fToleranceKG            = 1.0e-04 ! kg/s

C-------------------------------------------------------------------------------
C     Initialization
C-------------------------------------------------------------------------------

C.....Operating mode: electrolyzer or H2->electric conversion device?
      iOperatingMode = NPCDAT (iComponent, 4) ! (-)

C.....Max/min H2 flow rate
      fH2massFlowMIN  = ADATA ( iComponent,  1 ) ! (kg/s)
      fH2massFlowMAX  = ADATA ( iComponent,  2 ) ! (kg/s)

C.....electrical conversion coefficients
      fElecCoeff0     = ADATA ( iComponent,  3 ) ! (-)
      fElecCoeff1     = ADATA ( iComponent,  4 ) ! (- / W)
      fElecCoeff2     = ADATA ( iComponent,  5 ) ! (- / W^2 )
C.....thermal conversion coefficients 
      fTherCoeff0     = ADATA ( iComponent,  6 ) ! (-)
      fTherCoeff1     = ADATA ( iComponent,  7 ) ! (- / W )
      fTherCoeff2     = ADATA ( iComponent,  8 ) ! (- / W^2 )

C.....Supplemental fuel lower heating value
      fFuel_LHV       = ADATA ( iComponent,  9 ) ! (J / KG )

C.....Supplemental fuel stiochomentry (per kg h2)
      fH2Fraction     = ADATA ( iComponent, 10 ) ! (kg/kg)

C-------------------------------------------------------------------------------
C     .     Average heating value of hydrogen-hydrocarbon mixture
C     .                    
C     .         AVG. LHV =   (H2 LHV * Fraction H2 ) 
C     .                    + ( fuel LHV * (1-Fraction H2 ) ) 
C     .
C-------------------------------------------------------------------------------
      fAverageLHV = fH2_LHV * fH2Fraction / fH2MolarMass
     &     + fFuel_LHV * ( 1.0 - fH2Fraction)


C--------------------------------------------------------------------------------
C     Time-invariant calculations:
C     .  -> error trapping
C     .  -> calculation of maximum & minumum electric fluxes 
C--------------------------------------------------------------------------------

      if ( .not. bInitialized(iComponent) ) then

C-------------------------------------------------------------------------------
C     Error trapping:
C-------------------------------------------------------------------------------

         call eclose ( fH2massFlowMAX, 0.0,fToleranceKG, bNumsAreClose)

         if ( bNumsAreClose .or. fH2massFlowMAX .lt. 0.0 ) then

            write (itu,'(a,i2,a)') 'Error: Plant component ', 
     &           iComponent,':'
            write (itu,'(a,e10.3,a)') ' Maximum hydrogen mass flow ', 
     &           fH2massFlowMAX, ' (kg/s) must be greater than zero!.'
            
            bFatalError = .true.

         endif

         call eclose ( fH2massFlowMIN, 0.0,fToleranceKG, bNumsAreClose)

         if ( fH2massFlowMIN .lt. 0.0 .and. .not. bNumsAreClose ) then

            write (itu,'(a,i2,a)') 'Error: Plant component ', 
     &           iComponent,':'
            write (itu,'(a,e10.3,a,a)') ' Mimimum hydrogen mass flow ', 
     &           fH2massFlowMIN, ' (kg/s) must be greater or',
     &           ' equal to zero!'
  
            bFatalError = .true.

         endif

C-------------------------------------------------------------------------------
C     .  Determine electrical flux corresponding to mimimum and maximum 
C     .  hydogen flow. The electrical flux and hydrogen flow rate are 
C     .  related by the electricalconversion efficiency. The actual form 
C     .  of this equation depends on how the system has been configured
C     .  to operate:
C-------------------------------------------------------------------------------
         if ( iOperatingMode .eq. iElectrolyzer ) then
C-------------------------------------------------------------------------------
C     .     System is an electrolyzer. Flux-flow relationship is:
C     .
C     .                   (electrical efficiency) * (Electrical flux)
C     .       mass flow = -------------------------------------------
C     .                         Hydrogen lower heating value
C     .
C     .     (Equation 4, NRCan 2004)
C     .
C     .     However, the electrical efficiency is described using a quadratic
C     .     equation:
C     .
C     .       elec. eff. = e0 + e1 * (elec. flux.) + e2 * (elec. flux.) ^ 2
C     .
C     .     (Equation 5, NRCan 2004)
C     .
C     .     Substituting the electrical efficiency correlation into the 
C     .     flux-flow relationship yields a cubic equation:
C     . 
C     .     e2 * P^3 + e1 * P^2 + e0 * P - m * LHV = 0.
C     .
C     .     Where: P   = Electrical flux 
C     .            m   = Hydrogen mass flow rate
C     .            LHV = Hydrogen lower heating value.
C     .
C     .     The electrical flux corresponding for the miniumum and maximum 
C     .     mass flows can be determined by substituting mass flow m into
C     .     the cubic equation and solving for the corresponding flux P.
C     . 
C     .     The solving the cubic equation may yield multiple roots.  The 
C     .     correct flux is assumed to be the lowest real , non-negative root. 
C     .     Fortunately, we have a function to do the hard work --- 
C     .     fFirstCubicRoot(A,B,C,D, tolerance) returns the lowest real 
C     .     non-negative  root for the cubic equation:
C     .
C     .              Ax^3 + Bx^2 + Cx + D = 0
C     .
C     .     fFirstCubicRoot can also solve lower-order equations should 
C     .     A,B,C and/or D be zero. If no roots are found, -99.0 is returned.
C     .     The 'tolerance' arguement is used to specify how far a root may be 
C     .     from zero and still be considered 'zero'.
C-------------------------------------------------------------------------------
 
            fElecFluxMAX(iComponent) = fFirstCubicRoot(
     &           fElecCoeff2,
     &           fElecCoeff1,
     &           fElecCoeff0,
     &           -1.0 * fH2massFlowMAX * fH2_LHV / fH2molarMass,
     &           fToleranceWattsOrKelvin )
           


            fElecFluxMIN(iComponent) = fFirstCubicRoot(
     &           fElecCoeff2,
     &           fElecCoeff1,
     &           fElecCoeff0,
     &           -1.0 * fH2massFlowMIN * fH2_LHV / fH2molarMass,
     &           fToleranceWattsOrKelvin )



         else       
C-------------------------------------------------------------------------------
C     .     System is an a hydrogen->electricity conversion device
C     .
C     .                                  (Electrical flux)
C     .       mass flow = ---------------------------------------------------
C     .                   ( Average fuel lower heating value ) * (elec. eff.)
C     .
C     .     (Equation 11, NRCan 2004)
C     .
C     .     However, the electrical efficiency is described using a quadratic
C     .     equation:
C     .
C     .       elec. eff. = e0 + e1 * (elec. flux.) + e2 * (elec. flux.) ^ 2
C     .
C     .     (Equation 14, NRCan 2004)
C     .
C     .     Substituting the electrical efficiency correlation into the 
C     .     flux-flow relationship yields a quadratic equation:
C     . 
C     .                |           1         |
C     .     e2 * P^2 + | e1 - -------------  | * P + e0 = 0.
C     .                |      m * (avg. LHV) |
C     .
C     .     Where: P       = Electrical flux 
C     .            m       = Hydrogen mass flow rate
C     .            AVG LHV = Average heating value of hydrogen/hydrocarbons
C     .
C     .     The electrical flux corresponding for the miniumum and maximum 
C     .     mass flows can be determined by substituting mass flow m into
C     .     the quadratic equation and solving for the corresponding flux P.
C     . 
C     .     The solving the quadratic equation may yield multiple roots.  The 
C     .     correct flux is assumed to be the lowest real , non-negative root. 
C     .     Fortunately, we have a function to do the hard work --- 
C     .     fFirstCubicRoot(A,B,C,D) returns the lowest real non-negative  
C     .     root for the cubic equation:
C     .
C     .              Ax^3 + Bx^2 + Cx + D = 0
C     .
C     .     fFirstCubicRoot can also solve lower-order equations should 
C     .     A,B,C and/or D be zero. If no roots are found, -99.0 is returned.
C     .     The 'tolerance' arguement is used to specify how far a root may be 
C     .     from zero and still be considered 'zero'.
C-------------------------------------------------------------------------------
            fElecFluxMAX(iComponent) = fFirstCubicRoot(
     &           0.0,
     &           fElecCoeff2,
     &           (fElecCoeff1 
     &            - 1.0 / (fH2massFlowMAX/fH2Fraction *fAverageLHV)) ,
     &           fElecCoeff0,
     &           fToleranceWattsOrKelvin )

C-------------------------------------------------------------------------------
C     .     Note: if minimum mass flow is zero, minimum electrical flux is also 
C     .     zero (no parasitic losses)
C-------------------------------------------------------------------------------

            call eclose ( fH2massFlowMIN, 0.0, 
     &           fToleranceKG, bNumsAreClose)

            if ( bNumsAreClose ) then

               fElecFluxMIN(iComponent) = 0.0 ! (W)

            else

               fElecFluxMIN(iComponent) = fFirstCubicRoot(
     &              0.0,
     &              fElecCoeff2,
     &              (fElecCoeff1 
     &              - 1.0 / (fH2massFlowMIN/fH2Fraction *fAverageLHV)) ,
     &              fElecCoeff0,
     &              fToleranceWattsOrKelvin )

            endif
C--------------------------------------------------------------------------------
C     .     Tolerance for convergence
C--------------------------------------------------------------------------------
            fPlt_Output_Tol(iComponent,1) = 1.0E-05
            
         endif

C-------------------------------------------------------------------------------
C     .  Error trapping: if fFirstCubicRoot returns -99., no non-negative real
C     .  root was found. Coefficients are not valid for specified operating
C     .  range.
C-------------------------------------------------------------------------------
         call eclose (fElecFluxMAX(iComponent), 
     &        -99.0, 0.1, bNumsAreClose)
         if ( bNumsAreClose ) then 
            write (itu,'(a,i2,a)') 'Error: Plant component ', 
     &           iComponent,':'

            write (itu,'(a,e10.3,a,e10.3,a,e10.3)') 
     &           ' Electrical conversion coeff. ', 
     &           fElecCoeff0,', ',fElecCoeff1,', ',fElecCoeff2

            write (itu,'(a,a)') ' describe efficiency correlation ', 
     &           'that is invalid at maximum '              
            
            write (itu,'(a,e10.3,a)') ' hydrogen mass flow ', 
     &           fH2massFlowMAX, ' kg/s.'

            bFatalError = .true.

         endif
         
         call eclose (fElecFluxMIN(iComponent),
     &        -99.0, 0.1, bNumsAreClose)
         if ( bNumsAreClose ) then 
            write (itu,'(a,i2,a)') 'Error: Plant component ', 
     &           iComponent,':'

            write (itu,'(a,e10.3,a,e10.3,a,e10.3)')
     &           ' Electrical conversion coeff. ', 
     &           fElecCoeff0,', ',fElecCoeff1,', ',fElecCoeff2
            write (itu,'(a,a)') ' describe efficiency correlation ', 
     &           ' that is invalid at minimum '              
            
            write (itu,'(a,e10.3,a)') ' hydrogen mass flow ', 
     &           fH2massFlowMIN, ' kg/s.'

            
            bFatalError = .true.

         endif

         if ( bFatalError ) STOP 'H2_elec_converter(): Fatal error'

C-------------------------------------------------------------------------------
C     .     Set electircal flux associated with component to zero 
C-------------------------------------------------------------------------------
         fElecFluxActual(iComponent) = 0.


C-------------------------------------------------------------------------------
C     .     SEND metadata results to h3kreports:
C     .     Get component name
C-------------------------------------------------------------------------------
         hold_name = pcname(iComponent) 
C...........Format string as: 'plant/NAME/misc_data'
         WRITE(root_name,'(A,A,A)') 
     &        'plant/',
     &        hold_name(1:lnblnk(hold_name)),
     &        '/misc_data'
C-------------------------------------------------------------------------------
C     .     Electricity produced/used
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/electrical_flux'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')

C-------------------------------------------------------------------------------
C     .     Thermal energy produced/used
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/heat_flux'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')
C-------------------------------------------------------------------------------
C     .     Control signal - as dictated by electrical loads / surplus
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/control_signal'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')

C-------------------------------------------------------------------------------
C     .  Demand - control signal as constrained by system size
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), 
     &        '/flux_constrained_by_system_size'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')

C-------------------------------------------------------------------------------
C     .  Portion of control signal that can't be met due to system size 
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), 
     &        '/undelivered_flux/system_size'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')
C........-> update data 
         call REP_REPORT(
     &        fElecUnderSized, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )

C------------------------------------------------------------------------------
C    Electrical efficiency
C------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/electrical_eff'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '-' )

C------------------------------------------------------------------------------
C    .   Energy losses
C------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), 
     &        '/energy_losses'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')


         if (iOperatingMode .eq. iElectrolyzer ) then
C------------------------------------------------------------------------------
C    .   Operating point of electrolyzer (fraction of full load)
C------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/load_ratio'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '-' )
         endif



         if (iOperatingMode .eq. iH2converter ) then
C-------------------------------------------------------------------------------
C     .  Portion of control signal that can't be met due to system size 
C-------------------------------------------------------------------------------
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/H2_demand'
C........-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', 
     &           '(kg/s)' )

            
C-------------------------------------------------------------------------------
C     .  Portion of control signal that can't be met due to shortage of 
C     .  hydrogen: 
C-------------------------------------------------------------------------------
C........-> set name

            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/undelivered_flux/H2_shortage'
C........-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', 
     &           '(W)')
C------------------------------------------------------------------------------
C     .     Thermal efficiency
C------------------------------------------------------------------------------

C........-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/thermal_eff'
C........-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '-' )

C------------------------------------------------------------------------------
C     .     Auxiliary fuel use
C------------------------------------------------------------------------------
C........-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/auxiliary_fuel_flow'
C........-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units', '(kg/s)')

         else
C------------------------------------------------------------------------------
C     .     Electrolyzer: oxygen/water use 
C------------------------------------------------------------------------------   

C........-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/oxygen_flow'
C........-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units', '(kg/s)')

C........-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/water_flow'
C........-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units', '(kg/s)')     

         endif


C-------------------------------------------------------------------------------
C     .  Set flag ensuring initialization is not repeated.
C-------------------------------------------------------------------------------
         bInitialized(iComponent) = .true.
         
      endif

C-------------------------------------------------------------------------------
C     If unit is an H2->electric converter, it will have one conneciton.
C     Get connection index and properties:
C-------------------------------------------------------------------------------
      if ( iOperatingMode .eq. iH2converter ) then

C........Connection index
         iConnection = icondx(iComponent, 1, 1)

C........Connection temperature 
         fConnTempC  = convar(iConnection, iPropTemp) ! (oC)
         fConnTempK  = fConnTempC + 273.15 ! (K) 

C........Connection flow:
         fH2FlowActualKGs = convar(iConnection, iPropH2Flow) 
     &        * pcondr(iConnection) ! (kg/s) 

         fH2FlowActualKMOLs = fH2FlowActualKGs / fH2molarMass ! (kmol/s)
         
C.....Check that h2 flow exists
         call eclose(fH2FlowActualKGs, 0.0, fToleranceKG, bNoH2Flow)
 
      endif


 

C-------------------------------------------------------------------------------
C     Check value of ISTATS, and set coeffArray accordingly:
C     
C     .   State eqn:  Coef1 * T_(i) + Coef2 * T_(i-1) = Coef3
C     
C-------------------------------------------------------------------------------

C.....Zero coefficients
      do iCount = 1, mpcoe
         fCoeffArray(iCount) = 0.0
      end do

      if ( iStats .eq. 1 ) then
C-------------------------------------------------------------------------------
C     .  Temperature Matrix: In the present implementation, the temperature
C     .  of the electrolyzer/converter is not known. Set node temperature
C     .  to inlet temperature:
C     .
C     .   -> electrolyzer: set temperature to constant. Assume 80oC.
C-------------------------------------------------------------------------------

         if ( iOperatingMode .eq. iElectrolyzer ) then

            fCoeffArray(1) =  1.0
            fCoeffArray(2) =  80.0
            fCoeffArray(3) =  80.0

         else

            fCoeffArray(1) =  1.0
            fCoeffArray(2) = -1.0
            fCoeffArray(3) =  0.0

         endif

         
      elseif ( istats .eq. 2 ) then 
C-------------------------------------------------------------------------------
C     .  1st phase flow: no flow
C-------------------------------------------------------------------------------
         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = 0.0
         
      elseif ( istats .eq. 3 ) then
C-------------------------------------------------------------------------------
C     .  2nd phase flow: no flow 
C-------------------------------------------------------------------------------
         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = 0.0

      elseif ( istats .eq. 4 ) then

C-------------------------------------------------------------------------------
C     .  Hydrogen flow:
C
C     .  State Eqn:  
C
C     .  Coeff1 * (flow through current node) - (Flow through previous node) 
C     .              = (flow injected in current node)
C-------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C     Mark the flow-rate of hydrogen for iteration. Variables used:
C     
C     NPCDAT(i,9) row and column number defining location of component `i'
C     .       sub-matrix template in the overall plant network matrix. It is
C     .       used to identify the location of the current component's nodes
C     .       within the global plant matrix.
C
C     iCSV(i,j)  flag indicating that node `i' is marked for iteration for state
C     .       variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C     .       j=3 for 2nd phase mass flow, j=4 for hydrogen flow
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C     .       indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices 
C     .       as ICSV.
C---------------------------------------------------------------------------------
         iNode = NPCDAT (iComponent, 9)
         ICSV(iNode,4) = 1            
         CSVI(iNode,4) =  CSVF(iNode,4)

C-------------------------------------------------------------------------------
C     .  Get control data: Ideally, the operating point of the electrolyzer/
C     .  H2 converter would be controlled using a generic esp-r control 
C     .  function that determined the surplus/deficit on the network, and 
C     .  set the CDATA array accordingly. For now, this functionality is 
C     .  hard-coded using the 'Elec_net_load_calc' function, which 
C     .  performs miscellaneous calculations on the electrical network
C-------------------------------------------------------------------------------
            
         if (iOperatingMode .eq. iElectrolyzer ) then
C--------------------------------------------------------------------------------
C     .     Set electrical flux used by unit to amount of 'surplus' electricity
C     .     used on the network:
C     .     
C     .        surplus = off-site generation - load, excluding electrolyzer.
C     .
C--------------------------------------------------------------------------------

            fControlFlux = Elec_Net_Load_Calc(offsite_gen) 
     &                   - Elec_Net_plant_Calc(total_load,iComponent)
   
     
         else
C--------------------------------------------------------------------------------
C     .     Set electrical flux used by unit to excess load:
C     .     
C     .     Flux = (total load) - (off-site generation)
C     .
C--------------------------------------------------------------------------------

            fControlFlux =  Elec_Net_Load_Calc(total_load) 
     &           - Elec_Net_Load_Calc(offsite_gen) !(W)

         endif 
 
         if (fControlFlux .lt. 0.0 ) fControlFlux = 0.0 ! (W)

C-------------------------------------------------------------------------------
C     .  Check that unit's control signal is within its operating range, and 
C     .  set fElecFlux accordingly
C-------------------------------------------------------------------------------

         call eclose (fControlFlux,fElecFluxMIN(iComponent),
     &        fToleranceWattsOrKelvin, bNumsAreClose)

         if ( fControlFlux .LT. fElecFluxMIN(iComponent) .or. 
     &        bNumsAreClose ) then
C-------------------------------------------------------------------------------
C     .     Control signal is below minimum operating point. Switch unit off.
C     .
C     .     RE. Standard Hydrogen project:
C     .     this approach is consistent with the assumptions that the 
C     .     unit cannot export electricity to the grid, and will not use
C     .     grid electricity (ie. non-renewable energy) to generate 
C     .     hydrogen.
C-------------------------------------------------------------------------------     
            
            fElecFluxDemand = 0.      ! (W)
            bNoElecFlux = .true. 
            bInOperative = .true.
C...........Calculate electrical power that can't be delivered/used due
C...........to system size
            fElecUnderSized = fControlFlux ! (W)
            

         elseif ( fControlFlux .GT. fElecFluxMAX(iComponent) ) then
C-------------------------------------------------------------------------------
C     .     Control signal is above maximum operating point. Set to maximum.
C------------------------------------------------------------------------------- 

            fElecFluxDemand = fElecFluxMAX(iComponent) ! (W)
            bNoElecFlux = .false.
            bInOperative = .false.
C...........Calculate electrical power that can't be delivered/used due
C...........to system size
            fElecUnderSized = fControlFlux -  fElecFluxMAX(iComponent) ! (W)

         else
C-------------------------------------------------------------------------------
C     .     Control signal is within valid operating range. Set operating 
C     .     point to control signal 
C------------------------------------------------------------------------------- 

            fElecFluxDemand = fControlFlux ! (W) 
            bNoElecFlux = .false.
            bInOperative = .false.
C...........Calculate electrical power that can't be delivered/used due
C...........to system size
            fElecUnderSized = 0.0 ! (W)

         endif

C-------------------------------------------------------------------------------
C     .  Determine electrical/thermal efficiencies 
C     .  based on empirical equation:
C     .        
C     .          Efficiency = C0 + C1 * ( Electric power )  
C     .                          + C2 * ( Electric power ) ^ 2
C     .
C-------------------------------------------------------------------------------
            
         fElecEfficiency = fElecCoeff0 
     &        + fElecCoeff1 * abs ( fElecFluxDemand )
     &        + fElecCoeff2 * ( fElecFluxDemand ** 2.0 )

         fTherEfficiency  = fTherCoeff0 
     &        + fTherCoeff1 * abs ( fElecFluxDemand )
     &        + fTherCoeff2 * ( fElecFluxDemand ** 2.0 )

         if ( iOperatingMode .eq. iElectrolyzer ) then 
C------------------------------------------------------------------------------
C     .     Unit uses electricity to generate hydrogen. Actual electricity 
C     .     use will be equal to 'demanded' quantity:
C------------------------------------------------------------------------------

            fElecFluxActual(iComponent) = fElecFluxDemand ! (W)

C------------------------------------------------------------------------------
C     .     Electric->H2 conversion is described by:
C     .
C     .                        ( conversion eff. ) * (electric flux)
C     .       h2 flow rate  =  -------------------------------------
C     .                           (Hydrogen lower heating value)
C     . 
C     .     See Equation 4, NRCan 2004. Note: hydrogen heating value is
C     .     described in J/kmol
C     .   
C------------------------------------------------------------------------------

            fH2FlowActualKMOLs = fElecEfficiency 
     &           * fElecFluxActual(iComponent) / fH2_LHV   ! (kmol/s)
            fH2FlowActualKGs = fH2FlowActualKMOLs * fH2MolarMass ! (kg/s)

C------------------------------------------------------------------------------
C           Calculate the rate of oxygen produced and the water consumption
C           rate based on stochiometry of water splitting reaction.
C
C           H2O (l) -> H2 (g) + 1/2 O2 (g)
C
C------------------------------------------------------------------------------

            fO2FlowKMOLs = fH2FlowActualKMOLs / 2.0      ! (kmol/s)
            fO2FlowKGs = fO2FlowKMOLs * fO2molarMass     ! (kg/s)

            fH2OFlowKMOLs = fH2FlowActualKMOLs           ! (kmol/s)
            fH2OFlowKGs = fH2OFlowKMOLs * fH2OmolarMass  ! (kg/s)

C------------------------------------------------------------------------------
C     .     Calculate component matrix coefficients for ESP-r:
C     .      
C     .       Coeff1 * (flow through current node) 
C     .           = (flow injected in current node)
C------------------------------------------------------------------------------

            fCoeffArray(1) = 1.0
            fCoeffArray(2) = fH2FlowActualKGs ! Rate of H2 production

C------------------------------------------------------------------------------
C     .     Save electrical flux in electric network hybrid components 
C     .     power generation/use array
C------------------------------------------------------------------------------
            pwrp(iComponent) = -1.0 * fElecFluxActual(iComponent) ! Real power output (W), 
                                ! loads are -ive...

C------------------------------------------------------------------------------
C     .     Assume thermal energy cannot be recovered from electrolyzer.
C------------------------------------------------------------------------------
            fTherFlux = 0.0 

C------------------------------------------------------------------------------
C     .     Calculate energy losses of the electrolyzer
C     .
C     .     Energy Loss = (1 - electrical efficiency) * electrical input
C------------------------------------------------------------------------------
            fEnergyLoss = (1. - fElecEfficiency)
     &                     * fElecFluxActual(iComponent)


C------------------------------------------------------------------------------
C     .     Calculate operating point of electrolyzer (fraction of full load)
C------------------------------------------------------------------------------
            fLoadRatio = fH2FlowActualKGs / fH2massFlowMAX


         else 
C------------------------------------------------------------------------------
C     .     Unit uses hydrogen (and optionally, additional hydrocarbons) to
C     .     produce electricity. Calculate the amount of hydrogen required
C     .     to meet the 'demanded' electrical load.
C     .     
C     .                                 (electric flux)
C     .      demand fuel flow rate = ----------------------
C     .                              (AVG LHV) * (elec eff)
C     .
C     .     See Equation 11, NRCan 2004. Note: fuel+hydrogen average
C     .     heating value is described in J/kg
C     .
C------------------------------------------------------------------------------

            fH2FlowDemandKGs = 
     &           fH2Fraction * fElecFluxDemand  
     &           / ( fAverageLHV * fElecEfficiency ) ! (kg/s)
            fFuelFlowDemandKGs = 
     &           (1.0 - fH2Fraction ) * fElecFluxDemand
     &           / ( fAverageLHV * fElecEfficiency ) ! (kg/s)

C...........Use fH2FlowDemandKGs to set control variable for storage
C...........Tank flow rate HERE.

            call rep_report(fH2FlowDemandKGs,'zzz/requested')

C-------------------------------------------------------------------------------
C     .     Save the hydrogen demand as an 'additional output', and instruct
C     .     bps to iterate until this component converges
C-------------------------------------------------------------------------------
            nAPDat(iComponent) = 1
C...........Mark the first plant 'additional data' for iteration
            iPlt_Output_Iter_Flag (iComponent,1) = 1
C...........Save data from last iteration for comparison            
            fPlt_Output_Init_Val ( iComponent, 1) =
     &        pcdatf( iComponent , 1 )
C...........Save hydrogen demand as a plant 'additional output'
            pcdatf( iComponent , 1 ) = fH2FlowDemandKGs

                        

C------------------------------------------------------------------------------
C     .     The actual flow rate of hydrogen may be less than required
C     .     to meet the electrical load. Calculate actual electrical output
C     .     based on the amount of hydrogen flowing through the unit
C     .
C     .     Substituting the electrical efficiency correlation into the 
C     .     flux-flow relationship yields a quadratic equation:
C     . 
C     .                |           1         |
C     .     e2 * P^2 + | e1 - -------------  | * P + e0 = 0.
C     .                |      m * (avg. LHV) |
C     .
C     .     Where: P       = Electrical flux 
C     .            m       = Hydrogen mass flow rate
C     .            AVG LHV = Average heating value of hydrogen/hydrocarbons
C     .
C     .     The electrical flux corresponding for the actual hydrogen flow
C     .     rate can be determined by substituting mass flow m into the 
C     .     quadratic equation and solving for the corresponding flux P.
C     . 
C     .     The solving the quadratic equation may yield multiple roots.  The 
C     .     correct flux is assumed to be the lowest real , non-negative root. 
C     .     Fortunately, we have a function to do the hard work --- 
C     .     fFirstCubicRoot(A,B,C,D) returns the lowest real non-negative  
C     .     root for the cubic equation:
C     .
C     .              Ax^3 + Bx^2 + Cx + D = 0
C     .
C     .     fFirstCubicRoot can also solve lower-order equations should 
C     .     A,B,C and/or D be zero. If no roots are found, -1.0 is returned.
C     .     The 'tolerance' arguement is used to specify how far a root may be 
C     .     from zero and still be considered 'zero'.
C------------------------------------------------------------------------------- 


            if ( bNoH2Flow ) then 

               fElecFluxActual(iComponent) = 0.0 ! (W)
               fFuelFlowActualKGs = 0.0

               bInoperative = .true.

            else

               fElecFluxActual(iComponent) = fFirstCubicRoot(
     &              0.0,
     &              fElecCoeff2,
     &              (fElecCoeff1 
     &              - 1.0 /(fH2FlowActualKGs/fH2Fraction*fAverageLHV) ),
     &              fElecCoeff0,
     &              fToleranceWattsOrKelvin )

               fFuelFlowActualKGs = ( fH2FlowActualKGs / fH2Fraction )
     &              * ( 1.0 - fH2Fraction )

               bInOperative = .false.

            endif
C-------------------------------------------------------------------------------
C     .     Calculate the amount of electricity that can't be delivered
C     .     due to insufficient hydrogen supply
C-------------------------------------------------------------------------------
            call eclose(fElecFluxActual(iComponent), fElecFluxDemand, 
     &           fToleranceWattsOrKelvin, bNumsAreClose)
            if ( .not. bNumsAreClose 
     &           .and. fElecFluxActual(iComponent) 
     &           .lt.  fElecFluxDemand) then

               fElecH2Shortage = fElecFluxDemand 
     &              - fElecFluxActual(iComponent) ! (W)

            else

               fElecH2Shortage = 0.0 ! (W)

            endif

C------------------------------------------------------------------------------
C     .     Save electrical flux in electric network hybrid components 
C     .     power generation/use array:
C------------------------------------------------------------------------------
            pwrp(iComponent) = fElecFluxActual(iComponent) ! Real power output (W), generation is +ive...

C------------------------------------------------------------------------------
C    .      Calculate efficiencies based on amount of electricity
C    .      generated vs the amount of electricity demand
C------------------------------------------------------------------------------

            fElecEfficiency = fElecCoeff0 
     &           + fElecCoeff1 * abs ( fElecFluxActual(iComponent) )
     &           + fElecCoeff2 * ( fElecFluxActual(iComponent) ** 2.0 )

            fTherEfficiency  = fTherCoeff0 
     &           + fTherCoeff1 * abs ( fElecFluxActual(iComponent) )
     &           + fTherCoeff2 * ( fElecFluxActual(iComponent) ** 2.0 )


C------------------------------------------------------------------------------
C     .     Estimate thermal energy available for recovery
C     .
C     .        Thermal flux = (thermal efficiency) * (electrical flux) 
C------------------------------------------------------------------------------

            fTherFlux = fTherEfficiency * fElecFluxActual(iComponent)  ! (W)


C------------------------------------------------------------------------------
C     .     Calculate energy losses of electrical generation device
C     .
C     .     Energy Loss = (electricity generated) * (1/electric efficiency -1 - thermal efficiency)
C------------------------------------------------------------------------------

            fEnergyLoss = fElecFluxActual(iComponent) *
     &                    (1./fElecEfficiency - 1. - fTherEfficiency)


C------------------------------------------------------------------------------
C     .     Calculate hydrogen component matrix coefficients for ESP-r:
C     .      
C     .       Coeff1 * (flow through current node) 
C     .           - Coeff2 * (Flow through previous node) 
C     .           = (flow injected in current node)
C------------------------------------------------------------------------------

            fCoeffArray(1) = 1.0
            fCoeffArray(2) = -1.0 * pcondr(iConnection)
            fCoeffArray(3) = 0.0 ! Rate of H2 flowing through unit

         endif
      
C---------------------------------------------------------------------------------
C     .  Electrical network related operations - for ISTATS == 4 only!
C
C     .     1. Save real power output in electrical network common block 
C     .        variable
C     .     2. Call subroutine EMACH to (i) calculate the reactive 
C     .        component of the power produced by the network, and (ii) 
C     .        save this data in the electrical network common block
C
C---------------------------------------------------------------------------------

         iEModel=1              ! Flag indicating which power calculation will be performed
         
         CALL EMACH( 
     &        iComponent,       ! Component number (input)
     &        iEModel,          ! power calculation flag (input)
     &        pwrp(iComponent), ! Real power (input, W)
     &        fDummy1,          ! Complex power (output, W)
     &        fDummy2)          ! Overall power (output, W)

C-------------------------------------------------------------------------------
C     .  SEND results to h3kreports:
C     .  Get component name
C-------------------------------------------------------------------------------
         hold_name = pcname(iComponent) 
C........Format string as: 'plant/NAME/misc_data'
         WRITE(root_name,'(A,A,A)') 
     &        'plant/',
     &        hold_name(1:lnblnk(hold_name)),
     &        '/misc_data'
C-------------------------------------------------------------------------------
C     .  Electricity produced/used
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/electrical_flux'
C........-> update data 
         call REP_REPORT(
     &        pwrp(iComponent), H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )
C-------------------------------------------------------------------------------
C     .  Thermal energy produced 
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/heat_flux'
C........-> update data 
         call REP_REPORT(
     &        fTherFlux, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )
C-------------------------------------------------------------------------------
C     .  Control signal - as dictated by electrical loads / surplus
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/control_signal'
C........-> update data 
         call REP_REPORT(
     &        fControlFlux, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )

C-------------------------------------------------------------------------------
C     .  Demand - control signal as constrained by system size
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), 
     &        '/flux_constrained_by_system_size'
C........-> update data 
         call REP_REPORT(
     &        fElecFluxDemand, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )

C-------------------------------------------------------------------------------
C     .  Portion of control signal that can't be met due to system size 
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), 
     &        '/undelivered_flux/system_size'
C........-> update data 
         call REP_REPORT(
     &        fElecUnderSized, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )


C------------------------------------------------------------------------------
C    .   Electrical efficiency: ONLY report if system is active
C------------------------------------------------------------------------------
         if ( .not. bInOperative ) then
C.,,,.......-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/electrical_eff' 
C...........-> update data
            call REP_REPORT(
     &           fElecEfficiency,
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )
         endif
C-------------------------------------------------------------------------------
C    .   Energy losses 
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), 
     &        '/energy_losses'
C........-> update data 
         call REP_REPORT(
     &        fEnergyLoss, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )


         if (iOperatingMode .eq. iElectrolyzer ) then
C------------------------------------------------------------------------------
C    .   Operating point of electrolyzer (fraction of full load)
C------------------------------------------------------------------------------
C..........-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/load_ratio'
C..........-> update data
            call REP_REPORT(
     &           fLoadRatio, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )

         endif



         if (iOperatingMode .eq. iH2converter ) then
C-------------------------------------------------------------------------------
C     .  Hydrogen demand
C-------------------------------------------------------------------------------
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/H2_demand'
C........-> update data 
            call REP_REPORT(
     &           fH2FlowDemandKGs, 
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )

C-------------------------------------------------------------------------------
C     .  Auxiliary fuel flow
C-------------------------------------------------------------------------------
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/auxiliary_fuel_flow'
C........-> update data 
            call REP_REPORT(
     &           fFuelFlowActualKGs, 
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )
        
C-------------------------------------------------------------------------------
C     .  Portion of control signal that can't be met due to shortage of 
C     .  hydrogen: 
C-------------------------------------------------------------------------------
C........-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/undelivered_flux/H2_shortage'
C........-> update data 
            call REP_REPORT(
     &           fElecH2Shortage, 
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)))

C------------------------------------------------------------------------------
C    Thermal efficiency
C------------------------------------------------------------------------------
            if ( .not. bInOperative ) then

C...........-> set name          
               WRITE(H3K_rep_NAME,'(A,A)') 
     &              root_name(1:lnblnk(root_name)), '/thermal_eff'

C...........-> update data 
               call REP_REPORT(
     &              fTherEfficiency, 
     &              H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)))

            endif

         else 
C------------------------------------------------------------------------------
C     .     Unit is electrolyzer. Export consumption of water/oxygen
C------------------------------------------------------------------------------
C........-> Set name 
            WRITE (H3K_rep_NAME,'(A,A)') 
     &              root_name(1:lnblnk(root_name)), '/oxygen_flow'
C........-> update data 
            call REP_REPORT(
     &              fO2FlowKGs, 
     &              H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)))
C........-> Set name 
            WRITE (H3K_rep_NAME,'(A,A)') 
     &              root_name(1:lnblnk(root_name)), '/water_flow'
C........-> update data 
            call REP_REPORT(
     &              fH2OFlowKGs, 
     &              H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)))

         endif

      endif                     ! <- end of if(istats.eq.?) structure

   
C------------------------------------------------------------------------------
C     End of subroutine: H2_elec_converter()
C------------------------------------------------------------------------------
      return 
      end

C====================== H2_compressor_model ================================
C
C     Created by: Alex Ferguson
C     Created on: August 16, 2004
C     Copyright: CETC
C     ________
C     ABSTRACT: 
C     
C     This routine generates ESP-r plant matrix coefficients for a hydrogen-
C     compressor/decompressor component. It supports two types of analysis:
C     
C     .  - Ideal gas analysis: evaluates the component's isentropic 
C     .    performance assuming fluid is an ideal gas, and determines 
C     .    the kinematic work requirement/production using a user-supplied
C     .    isentropic efficiency correlation.
C
C     .  - Empirical analysis: uses user-supplied correlations ot evaluate
C     .    the kinematic work requirement/production, and the rate of heat
C     .    production.
C
C     The ideal gas analysis makes use of the PEM_COMPRESSOR routine, in 
C     PEM_FC_pt1.F.
C
C     It returns three coefficients describing the energy or mass flow 
C     balance across a single control volume, depending on the value of 
C     the ISTATS matrix passed into it:
C
C     istats = 1 -> temperature matrix:
C
C     .  - Determine the work/heat trasnfer associated with compressing/
C     .    expanding the working fluid.
C     .  - Optionally calculate outlet temperature. Otherwise, set 
C     .    outlet temperature to inlet temperature
C
C     istats = 2 -> 1st phase flow: 
C
C     .  - set first phase (water/air) flow to zero 
C
C     istats = 3 -> 2nd phase flow:
C
C     .  - set second phase (water/air) flow to zero
C     
C     istats = 4 -> hydrogen flow:
C
C     .  - set components' flow to the value of the preceeding 
C     .    component
C
C     __________
C     REFERENCES:
C
C     NRCan 2004. ``System Design for Wind-powered Hydrogen-Based Building
C     .    Cogeneration Project for Standard Hydrogen''. Internal Document, 
C     .    Revised July 21, 2004.
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical 
C     .    thermodynamics. Wiley and Sons
C
C===============================================================================
      
      subroutine h2_compressor_model(
     &     iComponent,
     &     fCoeffArray,
     &     iStats)
      implicit none 

#include "plant.h"

C External functions.     
      integer lnblnk

C-------------------------------------------------------------------------------
C     Passed variables
C-------------------------------------------------------------------------------
      integer iComponent        ! pointer to component 
      integer iStats            ! flag indicating which matrix is being solved
                                ! istats = 1 -> temperature
                                ! istats = 2 -> 1st phase flow
                                ! istats = 3 -> 2nd phase flow
                                ! istats = 4 -> hydrogen flow

      real fCoeffArray(mpcoe)  ! array of matrix coefficients for 
                                ! given component

C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------
      common/c10/npcon,ipc1,ipn1,ipct,ipc2,ipn2,pcondr,pconsd
      integer npcon             ! number of connections in plant network
      integer ipc1(mpcon)       ! connection -> index of recieving component
      integer ipn1(mpcon)       ! connection -> index of recieving node
      integer ipct(mpcon)       ! connection -> type
      integer ipc2(mpcon)       ! connection -> index of sending component 
      integer ipn2(mpcon)       ! connection -> index of sending node
      real pcondr(mpcon)        ! connection -> mass diversion ratio
      real pconsd(mpcon,2)      ! connection -> const. temperature / humid ratio

      common/pcond/convar,icontp,icondx
      real convar(mpcon,mconvr) ! connection -> temperature/flow data 
      integer icontp(mpcon)     !  connection -> type
      integer icondx(mpcom,mnodec,mpconc) ! connection index for each 
                                !  component / node / connection.

      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component 
      real bdata(mpcom,mbdata) ! more miscellaneous data for component 


      common/pcnam/pcname(mpcom)  
      character*15 pcname       ! Plant component names

C.....Electrical power use of plant 'hybrid' components
      common/elpcp/npel, pfp, ipfp, pwrp, bvoltp, iphp
      integer npel              ! number of "electrified" plant components 
      real pfp(mpcom)           ! power factor of componet i (0->1)
      integer ipfp(mpcom)       ! integer indicating if power factor lags (-1) or leads (+1)
      real pwrp(mpcom)          ! real power consumption of component (generation is -ive)
      real bvoltp(mpcom)        ! opertional voltage of component 
      integer iphp(mpcom)       ! phase component is connected to -> dummy variable

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp              ! not used in current context
      integer ihrf              ! not used in current context
      integer idyp              ! present day #
      integer idyf              ! not used in current context
      integer idwp              ! not used in current context
      integer idwf              ! not used in current context
      integer nsinc             ! time step number
      integer its               ! not used in current context

C-------------------------------------------------------------------------------
C     Common storing status of plant initialization. 
C-------------------------------------------------------------------------------
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)


C-------------------------------------------------------------------------------
C     Local variables
C-------------------------------------------------------------------------------

      integer iStream           ! counter

C.....Model configuration
      integer iOperation_mode   ! flag indicating if system is operating 
                                ! as a compressor, or decompressor

      integer iCompressor       ! Named constant indicating system operates
                                ! as a compressor

      integer iDeCompressor     ! Named conatant indicating system operates 
                                ! as a decompressor

      parameter ( iCompressor = 1, iDeCompressor = 2 )

      integer iIdeal_gas        ! Named constant for ideal gas analysis
      
      integer iEmpirical        ! Named constant for Empirical gas analysis

      parameter ( iIdeal_gas = 1, iEmpirical = 2 ) 


C.....Component parameters from plant network file  

      integer iAnalysis_type    ! flag indicating if system should be 
                                ! characterized using ideal gas law, or 
                                ! imperical equation

      real fPressureInlet       ! inlet pressure (kPa)
      real fPressureOutlet      ! outlet pressure (kPa)

      real fElecCoeff0,         ! system electrical performance coefficients
     &     fElecCoeff1,         ! 
     &     fElecCoeff2          !
     
      real fTherCoeff0,         ! system thermal performance coefficients 
     &     fTherCoeff1,         ! 
     &     fTherCoeff2          !    

      

C.....Connection data 
      integer iConnection       ! index of component's connection in plant network

      real fConnTempC           ! Temperature of incomming H2 (oC)
      real fConnTempK           ! Temperature of incomming H2 (K) 
      
      real fConnH2Flow          ! Flow rate of incomming H2 (kg/s)

      logical bNoH2Flow         ! logical flag indicating that hydrogen is 
                                ! flowing in system

C.....Misc data
      real fStream_comp(7)      ! Array  contianing composition of stream 
                                ! entering h2 compressor. (kmol/s)

      real fH2molarMass         ! Named constant: molar mass of hydrogen (kg/kmol)
      parameter ( fH2molarMass = 2.016 ) ! VAN WYLEN 1994

      real fEfficencyIsentropic ! Isentropic efficiency of compressor (-)

      real fH2enthalpyInlet     ! Enthalpy flow at inlet/outlet 
      real fH2enthalpyOutlet    ! (W)

      real fH2flowKMOL          ! Flow in kMol of H2 (kmol/s)

C.....Results
      real fElectricalWork      ! Electrical power produced (+) or used (-). (W)
      real fKinematicWorkW      ! Mechanical power produced (+) or used (-). (W) 
      real fKinematicWorkkW     ! Mechanical power produced (+) or used (-). (kW) 
      real fThermalWork         ! Heat energy produced (+) or used (-). (W) 
      
      real fOutletTempK         ! temperature at outlet of compressor (K)
      real fOutletTempC         ! temperature at outlet of compressor (oC)

C.....H3K reporting
      CHARACTER*128 H3K_rep_NAME ! H3Kreporting object
      character*128 root_name    ! root name of component node 
      character*128 hold_name    ! temporary variable for string manipulation

C.....loop data for outlet temperature estimate
      logical bSolnUnconverged  ! convergance flag
      logical bNoChange         ! flag indicating temperature does not change  
      real fMaxTemperature      ! temperature bounds on outlet temperature estimate 
      real fMinTemperature      ! (K)
      integer iIterCount        ! iteration counter
      real fEnthalpyDiff        ! result of comparison between target & iterating
                                ! enthalpies
      
C.....Dummy variables 
      real fDummy1, fDummy2
      integer iEModel           ! flag indicating which calculation should be 
                                ! performaed by EMACH subroutine

C-------------------------------------------------------------------------------
C     Named constants
C-------------------------------------------------------------------------------
      integer iPropTemp         ! named constant for temperature properity 
      integer iProp1stFlow      ! named constant for first phase flow properity
      integer iProp2ndFlow      ! named constant for second phase flow properity    
      integer iPropH2Flow       ! named constant for hydrogen flow 
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2, 
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )

      real fToleranceWattsOrKelvin ! named constants describing tolerance of 
                                   ! close-to-zero comparisons
      real fToleranceKG            
      
C-------------------------------------------------------------------------------
C     References 
C-------------------------------------------------------------------------------
      real PEMf_HH2             ! returns specific enthalpy of hydrogen (kJ/Kmol)

C-------------------------------------------------------------------------------
C     Program controls 
C-------------------------------------------------------------------------------

      logical bCalculateOutTemp ! flag for outlet temperature calculation

C-------------------------------------------------------------------------------
C     Initialize variables
C-------------------------------------------------------------------------------

C.....Set Tolerance for close-to zero comparisons
      fToleranceWattsOrKelvin = 10. ! Watts, K
      fToleranceKG            = 1.0e-04 ! kg/s

C.....Set hard-coded switch for outlet temperature calculation

      bCalculateOutTemp = .true.

C.....Initialize variables - necessary for use of g77 'fno-automatic' flag
      fElectricalWork = 0.
      fKinematicWorkW = 0.
      fThermalWork    = 0.

C-------------------------------------------------------------------------------
C     If this is the first time the coefficient generator has been called to 
C     characterize this component, set meta flags for h3k report output 
C-------------------------------------------------------------------------------
      if ( .not. bInitialized(iComponent) ) then 
         hold_name = pcname(iComponent) 
C........Format string as: 'plant/NAME/misc_data'
         WRITE(root_name,'(A,A,A)') 
     &        'plant/',
     &        hold_name(1:lnblnk(hold_name)),
     &        '/misc_data'
C-------------------------------------------------------------------------------
C     .  Electricity produced/used
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/electricalWork'
C........-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')

C-------------------------------------------------------------------------------
C     .  Mechanical work produced/used
C-------------------------------------------------------------------------------
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/mechanicalWork'
C........-> update meta-data 
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')
         
C-------------------------------------------------------------------------------
C     .  Thermal energy work produced/used
C-------------------------------------------------------------------------------
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/heatFlow'
C.....-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'units', '(W)' )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)), 'WattsToGJ', ' ')
 
         bInitialized(iComponent) = .true.

      endif

      
C-------------------------------------------------------------------------------
C     Get connection index and properties
C-------------------------------------------------------------------------------
      iConnection = icondx(iComponent, 1, 1)

      fConnTempC  = convar(iConnection, iPropTemp) ! (oC)

      fConnTempK  = fConnTempC + 273.15 ! (K) 

      fConnH2Flow = convar(iConnection, iPropH2Flow) 
     &     * pcondr(iConnection) ! (kg/s) 

      fH2flowKMOL = fConnH2Flow / fH2molarMass ! kmol/s

C.....Check that h2 flow exists
      call eclose(fConnH2Flow, 0.0, fToleranceKG, bNoH2Flow) 


C-------------------------------------------------------------------------------
C     Check value of ISTATS, and set coeffArray accordingly:
C     
C     .   State eqn:  Coef1 * T_(i) + Coef2 * T_(i-1) = Coef3
C     
C-------------------------------------------------------------------------------

      if ( istats .eq. iPropTemp ) then 

C-------------------------------------------------------------------------------
C     .  Temperature matrix
C
C     
C-------------------------------------------------------------------------------

C........Read model inputs 

C........-> Flag for ideal gas or empirical analysis
         iAnalysis_type  = int ( ADATA ( iComponent, 1 ) ) ! (-)
C........-> inlet and outlet conditions
         fPressureInlet  = ADATA ( iComponent, 2 ) ! (kPa)
         fPressureOutlet = ADATA ( iCOmponent, 3 ) ! (kPa) 
C........-> electrical conversion coefficients
         fElecCoeff0     = ADATA ( iComponent, 4 ) ! (W)
         fElecCoeff1     = ADATA ( iComponent, 5 ) ! (W / kg/s )
         fElecCoeff2     = ADATA ( iComponent, 6 ) ! (W / (kg/s)^2 )
C........-> thermal conversion coefficients 
         fTherCoeff0     = ADATA ( iComponent, 7 ) ! (W)
         fTherCoeff1     = ADATA ( iComponent, 8 ) ! (W / kg/s )
         fTherCoeff2     = ADATA ( iComponent, 9 ) ! (W / (kg/s)^2 )

C........Error trapping should go here

         if ( iAnalysis_type  .eq. iIdeal_gas ) then
C-------------------------------------------------------------------------------
C     .     Evaluate isentropic efficiency:
C     .     
C     .       eff = c0 + c1 * h2_flow + c2 * h2_flow **2
C     .
C-------------------------------------------------------------------------------            
            fEfficencyIsentropic =  fElecCoeff0 
     &           +  fElecCoeff1 * fConnH2Flow 
     &           +  fElecCoeff2 * fConnH2Flow ** 2.0

C-------------------------------------------------------------------------------
C     .     Set electrical work to zero, as we are not entirely sure
C     .     what will happen to recovered energy.
C-------------------------------------------------------------------------------           
            pwrp(iComponent) = 0.0 ! (W)

         elseif ( iAnalysis_type .eq. iEmpirical ) then
C-------------------------------------------------------------------------------
C     .     Calculate electrical and thermal work using empirical data:
C     .
C     .     WORK = coeff_0 + coeff1 * H2Flow + coeff2 * H2Flow ^ 2
C     .     
C     .     Note: the heat/electricity fluxes will be considered to flow into
C     .           the compressor/expander if the results of thelece following 
C     .           are negitive. Thus, the values of the electrical/thermal 
C     .           coefficients determine if the system will place a load
C     .           on the network and liberate heat (compressor) or produce
C     .           electricity and use heat (expander)
C------------------------------------------------------------------------------- 
            fElectricalWork = fElecCoeff0 
     &           +  fElecCoeff1 * fConnH2Flow 
     &           +  fElecCoeff2 * fConnH2Flow ** 2.0 ! (W, -ive ->O )

            fThermalWork =    fTherCoeff0 
     &           +  fTherCoeff1 * fConnH2Flow 
     &           +  fTherCoeff2 * fConnH2Flow ** 2.0 ! (W, -ive ->O )

C-------------------------------------------------------------------------------
C     .     Assumed isentropic efficiency for temperature-out calculation
C-------------------------------------------------------------------------------
            fEfficencyIsentropic = 0.75

C-------------------------------------------------------------------------------
C     .     Save electrical work for electrical network solution (loads are 
C     .     -ive.)
C-------------------------------------------------------------------------------           
            pwrp(iComponent) = -fElectricalWork ! (W)


         endif

C-------------------------------------------------------------------------------
C     .  Zero fStream_comp array (this array is used to pass the amount of
C     .  hydrogen flow to subordinate routine PEM_COMPRESSOR)
C-------------------------------------------------------------------------------
         do iStream = 1, 7
            fStream_comp ( iStream )  = 0.0 ! (kmol/s)
         end do 
C-------------------------------------------------------------------------------
C     .  Set hydrogen flow (fStream_comp location 5)
C-------------------------------------------------------------------------------
         fStream_comp ( 5 ) = fConnH2Flow /  fH2molarMass ! (kmol/s)

C-------------------------------------------------------------------------------
C     .  Check to see if hydrogen flow exists. If so, call 'PEM_compressor' 
C     .  to determine outlet conditions at compressor
C     .
C     .  Synopsys:
C     .
C     .    PEM_compressor(
C     .       stream composition array (input, kmol/s),
C     .       inlet pressure (input, kPa),
C     .       outlet pressure (input, kPa),
C     .       Inlet temperature (input, K), 
C     .       Outlet temperature (output, K),
C     .       Isentropic efficiency (input, -),
C     .       Kinematic work (output, kW) 
C     .       )
C     .
C     .
C     .  Note: PEM_compressor is called to characterize the outlet temperature
C     .        using either the calculated isentropic efficiency (ideal-gas 
C     .        model), or an assumed isentropic efficiency of 0.75 (empirical
C     .        model). It would be desirable to calculate the outlet temperature
C     .        using the actual work requirement caluculated using the 
C     .        empirical model. 
C
C     .        However, there is a lot of uncertianty about 
C     .        whether this energy flow is tranferred to the fluid, ambient, 
C     .        or is comprised of some other 'standing-loss'. Assuming that
C     .        all of the heat will be transferred to the working fluid 
C     .        can lead to some extremely unrealistic temperatures and 
C     .        instability of the simulation. 
C     .
C     .        Instead, the inlet temperature is calculated by assumng that the 
C     .        mecahnical performance of the compressor is approximated by a 
C     .        constant, 75% efficiency. This should not introduce too much 
C     .        error into the results, as the energy change associated with
C     .        compression/decompression is several orders of magnitude 
C     .        less than that associated with the chemical reactions.
C     .
C-------------------------------------------------------------------------------
         if ( .not. bNoH2Flow ) then
            call pem_compressor(
     &           fStream_comp, 
     &           fPressureInlet, 
     &           fPressureOutlet, 
     &           fConnTempK, 
     &           fOutletTempK, 
     &           fEfficencyIsentropic,
     &           fKinematicWorkKW )
            
C...........Convert kW to W
            fKinematicWorkW = fKinematicWorkKW * 1000.
         
         else
            
C-------------------------------------------------------------------------------
C     .     No flow in compressor / expander. Set work to zero, and outlet
C     .     temperature to inlet temperature
C-------------------------------------------------------------------------------              

            fKinematicWorkW = 0.0 ! (kW)
            fOutletTempK = fConnTempK ! (K)

         endif 

C---------------------------------------------------------------------------------
C     .  Electrical network related operations - for ISTATS == 1 only!
C
C     .   1. Save real power output in electrical network common block 
C     .      variable
C     .   2. Call subroutine EMACH to (i) calculate the reactive 
C     .      component of the power produced by the network, and (ii) 
C     .      save this data in the electrical network common block
C
C---------------------------------------------------------------------------------

         iEModel=1              ! Flag indicating which power calculation will be performed
         
         CALL EMACH( 
     &        iComponent,       ! Component number (input)
     &        iEModel,          ! power calculation flag (input)
     &        pwrp(iComponent), ! Real power (input, W)
     &        fDummy1,          ! Complex power (output, W)
     &        fDummy2)          ! Overall power (output, W)


C--------------------------------------------------------------------------------
C     .  Convert outlet temperature to oC
C--------------------------------------------------------------------------------
         fOutletTempC =  fOutletTempK - 273.15 ! oC
C--------------------------------------------------------------------------------
C     .  Set array coefficients: temperature of node = calculated outlet temp.
C--------------------------------------------------------------------------------
         fCoeffArray(1) =  1.0
         fCoeffArray(2) =  0.0
         fCoeffArray(3) =  fOutletTempC ! oC 

C-------------------------------------------------------------------------------
C     .  SEND results to h3kreports:
C     .  Get component name
C-------------------------------------------------------------------------------
         hold_name = pcname(iComponent) 
C........Format string as: 'plant/NAME/misc_data'
         WRITE(root_name,'(A,A,A)') 
     &        'plant/',
     &        hold_name(1:lnblnk(hold_name)),
     &        '/misc_data'
C-------------------------------------------------------------------------------
C     .  Electricity produced/used
C-------------------------------------------------------------------------------
C........-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/electricalWork'
C........-> update data 
         call REP_REPORT(
     &        fElectricalWork, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )

C-------------------------------------------------------------------------------
C     .  Mechanical work produced/used
C-------------------------------------------------------------------------------
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/mechanicalWork'
C........-> update data 
         call REP_REPORT(
     &        fKinematicWorkW, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )
         
C-------------------------------------------------------------------------------
C     .  Thermal energy work produced/used
C-------------------------------------------------------------------------------
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/heatFlow'
C.....-> update data 
         call REP_REPORT(
     &        fThermalWork, H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)) )


      elseif ( istats .eq. iProp1stFlow ) then 

C-------------------------------------------------------------------------------
C     .  1st phase flow: no flow
C-------------------------------------------------------------------------------

         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = 0.0
         
      elseif ( istats .eq. iProp2ndFlow ) then

C-------------------------------------------------------------------------------
C     .  2nd phase flow: no flow 
C-------------------------------------------------------------------------------

         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = 0.0

      elseif ( istats .eq. iPropH2Flow ) then

C-------------------------------------------------------------------------------
C     .  Hydrogen flow : Apply conservation of mass ( flow in = flow out )
C-------------------------------------------------------------------------------

         fCoeffArray(1) = 1.0
         fCoeffArray(2) = -1.0
         fCoeffArray(3) =  0.0

      endif
C-------------------------------------------------------------------------------
C     End of subroutine: H2_compressor_model()
C-------------------------------------------------------------------------------
      return
      end

C====================== H2_storage_model ====================================
C
C     Created by: Alex Ferguson
C     Created on: August 16, 2004
C     Copyright: CETC
C     ________
C     ABSTRACT: 
C     
C     This subroutine characterizes the behavior of a hydrogen storage
C     tank (database type 1010) for matrix types 1->4. The tank is modelled
C     as a single control volume with a single incomming hydrogen flow
C     
C     iStats = 1 (temperature flow)
C     
C     .  The temperature of the control volume (and concequently, the 
C     .  outgoing flows are set to that of the incomming flow
C
C     iStats = 2 (1st-phase mass flow)
C
C     .  The flow out of the tank is set to zero
C
C     iStats = 3 (2nd-phase mass flow)
C
C     .  The flow out of the tank is set to zero
C
C     istats = 4 (hydrogen flow)
C     
C     .  The flow out of the tank is regulated by an external control.
C     .  The model considers the flow rate of hydrogen into the tank
C     .  and losses from the tank (described by an emperical equation),
C     .  and caculates the amount of hyrogen remaining in the tank at the 
C     .  end of the time-step
C     . 
C     .  The state of the tank is described by a non-linear ordinary 
C     .  differential equation. This equation is solved using the 
C     .  Runge-Kutta integration method, and varying time resolution.
C
C     INPUTS:
C     
C     .  - iComponent: index of component in plant network
C     .  - iStats: index of matrix to be solved
C     
C     OUTPUTS: 
C
C     .  - fCoeffArray: Array contiaing ESP-r matirx coefficients 
C     .            for this component.
C
C     LIMITATIONS:
C     
C     .  - At present, only a single instance of the hydrogen storage model
C     .    is supported. It would not be difficult to add support for multiple 
C     .    instances.
C
C     __________
C     REFERENCES:
C
C     NRCan 2004. ``System Design for Wind-powered Hydrogen-Based Building
C     .    Cogeneration Project for Standard Hydrogen''. Internal Document, 
C     .    Revised July 21, 2004.
C
C===============================================================================

      subroutine H2_storage_model(
     &     iComponent,
     &     fCoeffArray,
     &     iStats)
      implicit none 

#include "plant.h"

C External functions.     
      integer lnblnk

C-------------------------------------------------------------------------------
C     Passed variables
C-------------------------------------------------------------------------------
      integer iComponent        ! pointer to component 
      integer iStats            ! flag indicating which matrix is being solved
                                ! istats = 1 -> temperature
                                ! istats = 2 -> 1st phase flow
                                ! istats = 3 -> 2nd phase flow
                                ! istats = 4 -> hydrogen flow

      real fCoeffArray(mpcoe)  ! array of matrix coefficients for 
                                ! given component
C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------
      common/c10/npcon,ipc1,ipn1,ipct,ipc2,ipn2,pcondr,pconsd
      integer npcon             ! number of connections in plant network
      integer ipc1(mpcon)       ! connection -> index of recieving component
      integer ipn1(mpcon)       ! connection -> index of recieving node
      integer ipct(mpcon)       ! connection -> type
      integer ipc2(mpcon)       ! connection -> index of sending component 
      integer ipn2(mpcon)       ! connection -> index of sending node
      real pcondr(mpcon)        ! connection -> mass diversion ratio
      real pconsd(mpcon,2)      ! connection -> const. temperature / humid ratio

      common/pcond/convar,icontp,icondx
      real convar(mpcon,mconvr) ! connection -> temperature/flow data 
      integer icontp(mpcon)     !  connection -> type
      integer icondx(mpcom,mnodec,mpconc) ! connection index for each 
                                !  component / node / connection.

      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component 
      real bdata(mpcom,mbdata) ! more miscellaneous data for component 


      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1              ! not used in current context
      integer ism1              ! not used in current context
      integer isd2              ! not used in current context           
      integer ism2              ! not used in current context
      integer isds              ! not used in current context
      integer isdf              ! not used in current context 
      integer ntstep            ! number of building steps/hour

      common / pcval / csvf, csvp
      real csvf(mpnode,mpvar)   ! future values of state variables
      real csvp(mpnode,mpvar)   ! present values of state variables 

      common/piter/maxitp,perrel,pertmp,perflx,permfl,itrclp,
     &             icsv,csvi
      integer maxitp            ! not used in current context
      integer itrclp            ! not used in current context
      integer icsv(mpnode,mpvar)! flag marking nodes for iteration
      real perrel               ! not used in current context
      real pertmp               ! not used in current context
      real perflx               ! not used in current context
      real permfl               ! not used in current context
      real csvi(mpnode,mpvar)   ! 'initial' values of state variables
                                ! at start of iteration
      common/pcdat/
     &     pcdatf(mpcom,mpcdat), ! component additional data (present)
     &     pcdatp(mpcom,mpcdat)  ! component additional data (future)
      real pcdatf, pcdatp

      common/pcres/
     &     qdata(mpcom),        ! not used in current context
     &     pcaout(mpcom,mpcres), ! not used in current context
     &     napdat(mpcom)        ! # of plant additional outputs
      real qdata, pcaout
      integer napdat

      common/c12ps/
     &     npcdat(mpcom,9),     ! miscellaneous plant data
     &     ipofs1(mcoefg),      ! matrix coefficient location (rows)
     &     ipofs2(mcoefg,mpvar) ! matrix coefficient location (cols)
      integer npcdat, ipofs1, ipofs2

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp              ! not used in current context
      integer ihrf              ! not used in current context
      integer idyp              ! not used in current context
      integer idyf              ! not used in current context
      integer idwp              ! not used in current context
      integer idwf              ! not used in current context
      integer nsinc             ! time-step number
      integer its               ! not used in current context

C.....Temporary common used to transport data to H2 storage tank model. 
C.....(to be replaced by a control?)
      common / temp_control / fH2FlowDemandKGs
      real fH2FlowDemandKGs

C-------------------------------------------------------------------------------
C     Common storing status of plant initialization. 
C-------------------------------------------------------------------------------
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)
      
      common/pctstp/ntstpp
      integer ntstpp            ! number of plant timesteps / building timestep

C-------------------------------------------------------------------------------
C     Local variables
C-------------------------------------------------------------------------------
      integer i                 ! counter

      real fH2massPresent     ! storage unit's state of charge (kg)
                                ! at present time row 
      real fH2massFuture      ! storage unit's state of charge (kg)
                                ! at future time row       

      real fH2mass_initial     ! Initial mass in tank at start of simulation (kg)

      real fTS_duration         ! duration of timesteps - seconds

      integer iConnection       ! index of incomming connection

      integer iNode             ! index of component's node in network 

C-------------------------------------------------------------------------------
C     Boundry conditions (input flow, and hydrogen demand) 
C-------------------------------------------------------------------------------
      real fH2massFlow_in       ! rate of mass flow into the unit (kg/s)
      real fH2massFlow_out      ! rate of mass flow out of the unit (kg/s)

      real fH2temperature_in    ! temperature of H2 flow entering unit (oC)

      logical bNoMassFlowIn     ! flag indicating that no flow is comming into
                                ! tank

      logical bNoInitialMass    ! flag indicating that tank is empty @ start of
                                ! timestep
      
      logical bMassFlowsAreEq   ! flag indicating that inlet & outlet mass flows
                                ! are equal


      logical bTempsAreEqual    ! flag indicating that inlet & outlet mass flows
                                ! are equal                               
C-------------------------------------------------------------------------------     
C     Tank parameters
C-------------------------------------------------------------------------------
      real fLossCoeff_c         ! coefficient of hydrogen loss 0 (kg/s)
      real fLossCoeff_b         ! coefficient of hydrogen loss 1 (/s)
      real fLossCoeff_a         ! coefficient of hydrogen loss 2 (/kg.s)
      real fH2massMAX           ! maximum amount of mass that can be stored 
                                ! in tank (kg)
      real fH2massMIN           ! minimum amount of mass in tank during 
                                ! normal operation.


      logical bNoMassMaxLimit   ! flag indicating that no limit has been placed
                                ! on mass that can be stored in tank

C-------------------------------------------------------------------------------
C     Instantaneous rates of change in tank
C-------------------------------------------------------------------------------
      real fRateMass_Change    ! rate of change in the tank (kg/s)
      real fRateFlow_in        ! flow rate into tank (kg/s)
      real fRateFlow_out       ! flow rate out of tank (kg/s)
      real fRate_tankLoss      ! rate of h2 loss from tank (kg/s)


C-------------------------------------------------------------------------------
C     Runge-Kutta parameters
C-------------------------------------------------------------------------------
      integer iNumber_SubSol_TS ! number of time-steps used in Runge-Kutta solution
                                ! of differential equation

      real fSubSol_TS           ! Timestep-duration for Runge-Kutta solution  (s) 

      real fRK_TankMass        ! Tank mass at start/end of Runge-Kutta soln loop. (kg)

C-------------------------------------------------------------------------------
C     Runge-Kutta intermediate results.
C-------------------------------------------------------------------------------
      real fRK_dMass1          ! Tank mass-change results from Runge-Kutta solution (kg)
      real fRK_dMass2          
      real fRK_dMass3  
      real fRK_dMass4
      real fRK_dMass_T

      real fRKFlow_in1        ! Tank incomming flow results from Runge-Kutta solution (kg)
      real fRKFlow_in2          
      real fRKFlow_in3  
      real fRKFlow_in4
      real fRKFlow_in_T

      real fRKFlow_out1       ! Tank outgoing flow results from Runge-Kutta solution (kg)
      real fRKFlow_out2          
      real fRKFlow_out3  
      real fRKFlow_out4
      real fRKFlow_out_T

      real fRK_tankLoss1      ! Tank h2 loss results from Runge-Kutta solution (kg)
      real fRK_tankLoss2          
      real fRK_tankLoss3  
      real fRK_tankLoss4
      real fRK_tankLoss_T


C-------------------------------------------------------------------------------
C     Runge-Kutta results, integrated over RK time-slice
C-------------------------------------------------------------------------------
      real fRKFlow_in          ! integrated flow into tank, as determined by Runge-Kutta soln. (kg)
      real fRKFlow_out         ! integrated flow out of tank, as determined by Runge-Kutta soln. (kg)
      real fRK_tankLoss        ! integrated loss from tank, as determined by Runge-Kutta soln. (kg)

C-------------------------------------------------------------------------------
C     Runge-Kutta results, averaged over ESP-r time-step
C-------------------------------------------------------------------------------
      real fAVGFlow_in         ! Actual flow rate into tank, averaged over timestep. (kg/s)
      real fAVGFlow_out        ! Actual flow rate out of tank, averaged over timestep (kg/s)
      real fAVG_tankLoss       ! Actual loss rate from tank, averaged over timestep (kg/s)
! 
C-------------------------------------------------------------------------------
C     Runge-Kutta results, averaged over ESP-r time-step
C-------------------------------------------------------------------------------
      real fTank_TemperatureOLD    ! temperature of hydrogen stored in tank from 
                                   ! past timestep (oC)

      real fTank_TemperatureNEW    ! temperature of h2 stored in tank, future 
                                   ! timestep   (oC) 

      real fIntegrationConstant    ! constant of integration
 
C-------------------------------------------------------------------------------
C     Dummy variables, 
C-------------------------------------------------------------------------------
      real fDummy1,fDummy2,fDummy3 ! Dummy variables
      logical bNumsAreClose      ! flag for close-to-zero comparisons
C-------------------------------------------------------------------------------      
C     Commons associated with H2 storage tank
C-------------------------------------------------------------------------------
      common/H2StorageTank/  
     &     fH2massPresent,    
     &     fH2massFlow_in,    
     &     fH2massFlow_out,   
     &     fH2massMIN,         
     &     fH2massMAX,
     &     bNoMassMaxLimit, 
     &     fLossCoeff_a,       
     &     fLossCoeff_b,       
     &     fLossCoeff_c,       
     &     fTolerance,          
     &     fAVGFlow_in,        
     &     fAVGFlow_out,       
     &     fAVG_tankLoss,      
     &     fH2massFuture      
      
C-------------------------------------------------------------------------------
C     Named constants
C-------------------------------------------------------------------------------
      integer iPropTemp        ! named constant for temperature properity 
      integer iProp1stFlow    ! named constant for first phase flow properity
      integer iProp2ndFlow    ! named constant for second phase flow properity    
      integer iPropH2Flow     ! named constant for hydrogen flow 
      parameter ( iPropTemp     = 1,
     &            iProp1stFlow = 2, 
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )
      
      real fTolerance           ! named constant describing tolerance of 

C-------------------------------------------------------------------------------
C     Character variables for h3kreports
C-------------------------------------------------------------------------------                                ! close-to-zero comparisons
C.....ESP-r Commons

      common/pcnam/
     &     pcname(mpcom)        ! Plant component names
      character*15 pcname

C.....Local Variables
      CHARACTER*128 H3K_rep_NAME ! H3Kreporting object
      character*128 root_name    ! root name of component node 
      character*128 hold_name    ! temporary variable for string manipulation
      logical bClose             ! flag for close-to-zero comparison

C-------------------------------------------------------------------------------
C     Initialize variables
C-------------------------------------------------------------------------------

C.....Set Tolerance for close-to zero comparisons

      fTolerance = 1.0E-06

C.....Zero array coefficients

      do i = 1, mpcoe
         fCoeffArray(mpcoe) = 0.0
      end do

C-------------------------------------------------------------------------------
C     Check value of ISTATS, and set coeffArray accordingly:
C     
C     .   State eqn:  Coef1 * T_(i) + Coef2 * T_(i-1) = Coef3
C     
C-------------------------------------------------------------------------------

      if ( istats .eq. iPropTemp ) then

C-------------------------------------------------------------------------------
C     .  Temperature flow: set equal to inlet conditions
C-------------------------------------------------------------------------------
         fTank_TemperatureNEW = PCDatF(iComponent,2)
         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = fTank_TemperatureNEW
         
      elseif ( istats .eq. iProp1stFlow ) then 

C-------------------------------------------------------------------------------
C     .  1st phase flow: no flow
C-------------------------------------------------------------------------------

         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = 0.0
         
      elseif ( istats .eq. iProp2ndFlow ) then

C-------------------------------------------------------------------------------
C     .  2nd phase flow: no flow 
C-------------------------------------------------------------------------------

         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = 0.0

      elseif ( istats .eq. iPropH2Flow ) then

C-------------------------------------------------------------------------------
C     .  HYDROGEN FLOW!!! 
C 
C     .  Check to see if plant solution has converged, and update present/future 
C     .  time-row as necessary. - this implementation will have to be modified 
C     .  to account for inter-domain iteration.
C-------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C     Mark the flow-rate of hydrogen for iteration. Variables used:
C     
C     NPCDAT(i,9) row and column number defining location of component `i'
C     .       sub-matrix template in the overall plant network matrix. It is
C     .       used to identify the location of the current component's nodes
C     .       within the global plant matrix.
C
C     iCSV(i,j)  flag indicating that node `i' is marked for iteration for state
C     .       variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C     .       j=3 for 2nd phase mass flow, j=4 for hydrogen flow
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C     .       indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices 
C     .       as ICSV.
C---------------------------------------------------------------------------------
      iNode = NPCDAT (iComponent, 9)
      ICSV(iNode,4) = 1            
      CSVI(iNode,4) =  CSVF(iNode,4)

C-------------------------------------------------------------------------------
C     .  Read model configuration from input file.
C-------------------------------------------------------------------------------

         fH2massMIN      = ADATA(iComponent, 1) ! (kg)
         fH2massMAX      = ADATA(iComponent, 2) ! (kg)
         fH2mass_initial = ADATA(iComponent, 3) ! (kg) 
         fLossCoeff_c    = ADATA(iComponent, 4) ! (kg/s)
         fLossCoeff_b    = ADATA(iComponent, 5) ! (/s)
         fLossCoeff_a    = ADATA(iComponent, 6) ! (/kg.s 


C........If maximum mass storage value is less than -0.5, assume there is
C........no limit to how much hydrogen can be stored.
         if ( fH2massMAX .lt. -0.5 ) then

            bNoMassMaxLimit = .true.

         else 

            bNoMassMaxLimit = .false.

         endif


C-------------------------------------------------------------------------------
C     Initialize MetaData for h3kReports
C-------------------------------------------------------------------------------
         if ( .not. bInitialized(iComponent) ) then 
C.....Get component name
            hold_name = pcname(iComponent) 
C.....Format string as: 'plant/NAME/misc_data'
            WRITE(root_name,'(A,A,A)') 
     &           'plant/',
     &           hold_name(1:lnblnk(hold_name)),
     &           '/misc_data'

C.....Rate of flow into tank
C.....-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), '/hydrogenFlow_in'
C.....-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units',
     &           '(kg/s)'
     &           )
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'KgPerStoKG',
     &           ' '
     &           )

C.....Rate of flow into tank
C.....-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/hydrogenFlow_out'
C.....-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units',
     &           '(kg/s)'
     &           )
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'KgPerStoKG',
     &           ' '
     &           )

C.....Rate of flow into tank
C.....-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/hydrogenLoss'
C.....-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units',
     &           '(kg/s)'
     &           )
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'KgPerStoKG',
     &           ' '
     &           )

C.....Rate of flow into tank
C.....-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/hydrogen_storage/present'
C.....-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units',
     &           '(kg)'
     &           )



C.....Rate of flow into tank
C.....-> set name
            WRITE(H3K_rep_NAME,'(A,A)') 
     &           root_name(1:lnblnk(root_name)), 
     &           '/hydrogen_storage/future'
C.....-> update meta-data
            call REP_SET_META(
     &           H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &           'units',
     &           '(kg)'
     &           )
            bInitialized(iComponent) = .true.
         endif 
         
C-------------------------------------------------------------------------------
C     .  Get mass of hydrogen in storage tank
C-------------------------------------------------------------------------------
         if ( nsinc .eq. 1 ) then
C-------------------------------------------------------------------------------
C     .     This is the first time step -> use initial mass
C-------------------------------------------------------------------------------
            fH2massPresent = fH2mass_initial  ! (kg)

C...........Tank Temperature
            fTank_TemperatureOLD = 0.0 ! oC

         else
C-------------------------------------------------------------------------------
C     .     Read stored mass from present-value miscellaneous
C     .     storage array
C-------------------------------------------------------------------------------        
            fH2massPresent = PCDatP(iComponent,1) !(kg)

C...........Tank temperature
            fTank_TemperatureOLD = PCDatP(iComponent,2)! oC

         endif


             
         
C-------------------------------------------------------------------------------
C     .  Get timestep duration:
C     
C     .                                   seconds per hour 
C     .  duration = -------------------------------------------------------------
C     .             (# building steps / hour ) * ( # plant steps / building step ) 
C     
C-------------------------------------------------------------------------------

         fTS_duration = 3600.0 / real ( nTStep * nTStpp ) 

C-------------------------------------------------------------------------------
C     .  Set flag for incomming connection:
C
C     .      icondx(i,j,k) = index of k'th connection to node j in component i
C
C------------------------------------------------------------------------------

         iConnection = icondx(iComponent, 1, 1)

C-------------------------------------------------------------------------------
C     .  Get temperature / mass flow into storage unit
C     .    -> Use "mass-diversion-ratio" data stored in pcondr
C     .       common array to determine the ratio of mass flow 
C     .       through the given connection to that in the sending 
C     .       node.
C-------------------------------------------------------------------------------

         fH2temperature_in = convar(iConnection, iPropTemp) ! (oC)

         fH2massFlow_in   = convar(iConnection, iPropH2Flow) 
     &        * pcondr(iConnection) ! (kg/s) 

C........Does flow exist?
         call eclose( fH2massFlow_in, 0.0, fTolerance, bNoMassFlowIn )

C........Get mass flow out of storage unit. Where does this come from ?!?

         fH2massFlow_out = fH2FlowDemandKGs
         call rep_report(fH2FlowDemandKGs, 'zzz/z_recieved')
C-------------------------------------------------------------------------------
C     .  Storage state equation is (Eq 8&9, NRCan 2004):
C     
C     .      dM 
C     .     ---- = (m_in) - (m_out) - (Ca) * M^2 - (Cb) * M - (Cc)  
C     .      dt
C     
C     .  where: 
C     
C     .    M               = mass 
C     .    t               = time 
C     .    m_in, m_out     = mass flow in & mass flow out
C     .    Ca, Cb, Cc      = 2nd order, 1st order & constant loss coefficient 
C     
C     .  There is no single analytical solution to the state equations. The
C     .  analytical solution procedure of the non-homogeneous equation is 
C     .  dependent on the values of the constant, first-order and second-order 
C     .  loss coefficients, and inadvertant application of the incorrect solution 
C     .  may give invalid results and produce instability in the simulation. 
C     
C     .  Instead, a simple numerical solution is used that, while slower than the 
C     .  analytical apporaches, consistantly yields stable results.
C
C     .  Evaluate the rate of change in the tank at the start of the time step 
C     .  using the functionfEvalH2Tank_DEqn(). Function's additional outputs 
C     .  are ignored. 
C
C-------------------------------------------------------------------------------

         fRK_TankMass = fH2massPresent

C........Is there mass in the tank?
         call eclose(fH2massPresent, 0.0, fTolerance,  bNoInitialMass)

         call EvalH2Tank_DEqn ( 
     &        fSubSol_TS,
     &        fRK_TankMass,  
     &        fRateMass_Change, 
     &        fDummy1, fDummy2, fDummy3 )

C-------------------------------------------------------------------------------
C     .  Check if rate of change is close to zero: 
C-------------------------------------------------------------------------------
         call eclose (fRateMass_change, 0.0, fTolerance, bNumsAreClose) 

C-------------------------------------------------------------------------------
C     .  Determine the solution time-step that will produce less than 10% 
C     .  change in the total mass in the tank for first time-step:
C     
C     .                                        10 % of mass in tank 
C     .  numerical soln. resolution  = ----------------------------------
C     .                                rate of change @ start of time-step
C     .
C
C     .  Then determine how many of these time slices must be processed per 
C     .  ESP-r time-step to obtain desired frequency
C     
C-------------------------------------------------------------------------------
         if ( .not. bNumsAreClose ) then

C...........Solution time-slice duration (seconds)
            fSubSol_TS = 0.1 * fH2massPresent 
     &           / abs ( fRateMass_Change )

C...........Check for zero TS duration (ie: no mass in tank
            call eclose (fSubSol_TS, 0.0, fTolerance, bNumsAreClose)

C...........# of timeslices per ESP-r time slice
            if ( bNumsAreClose ) then 

               iNumber_SubSol_TS = 10 

            else

               iNumber_SubSol_TS = int ( fTS_duration / fSubSol_TS ) + 1

            endif 

         else  

C...........Even if mass does not change, solve state equation once to 
C...........determine resulting flows & losses to/from tank

            iNumber_SubSol_TS = 1

         endif

C-------------------------------------------------------------------------------
C     .  Recalculate solution time-resolution to ensure that results 
C     .  coincide with future time row:
C-------------------------------------------------------------------------------
            
         fSubSol_TS =  fTS_duration / real ( iNumber_SubSol_TS ) 

C-------------------------------------------------------------------------------
C     .  Initiate Runge-Kutta solution. For each solution time-step (i) :
C     
C     .                                | dY |
C     .    k1 = (time step duration) * | -- |
C     .                                | dt | (Y = Yi)
C     .
C     .                                | dY |
C     .    k2 = (time step duration) * | -- |
C     .                                | dt | (Y = Yi + k1/2)
C     .
C     .                                | dY |
C     .    k3 = (time step duration) * | -- |
C     .                                | dt | (Y = Yi + k2/2)
C     .
C     .                                | dY |
C     .    k4 = (time step duration) * | -- |
C     .                                | dt | (Y = Yi + k3)
C     .
C     .                k1   k2   k3   k4
C     .    Yi+1 = Yi + -- + -- + -- + -- 
C     .                6.   3.   3.   6.
C     .
C     .  Where Y = tank mass, flow into tank, flow out of tank, losses from tank
C     . 
C     .  The value of dY/dt is calculated using subroutine EvalH2Tank_DEqn,
C     .  which returns the rate of change in the tank's mass, the flows into
C     .  and out of the tank, and the rate of H2 loss from the tank for a 
C     .  given amount of mass in the tank.
C     . 
C-------------------------------------------------------------------------------

C........Initialize starting points
         fRK_TankMass = fH2massPresent 
         fRKFlow_in   = 0.0
         fRKFlow_out  = 0.0
         fRK_tankLoss = 0.0

C........Loop through Runge-Kutta sub-timestep time-slices         
         do i = 1,  iNumber_SubSol_TS

C-------------------------------------------------------------------------------
C     .     Get tank mass rate of change, flow rates and loss rate using
C     .     slope at i
C-------------------------------------------------------------------------------

            call EvalH2Tank_DEqn ( 
     &           fSubSol_TS          ,
     &           fRK_TankMass        ,  
     &           fRateMass_change    , 
     &           fRateFlow_in        , 
     &           fRateFlow_out       , 
     &           fRate_tankLoss      )

C-----------Integrate results over time-slice

            fRK_dMass1      =  fRateMass_change * fSubSol_TS ! (kg)
            fRKFlow_in1     =  fRateFlow_in     * fSubSol_TS ! (kg/s)
            fRKFlow_out1    =  fRateFlow_out    * fSubSol_Ts ! (kg/s)
            fRK_tankLoss1   =  fRate_tankLoss   * fSubSol_Ts ! (kg/s)

C-------------------------------------------------------------------------------
C     .    Get tank mass rate of change, flow rates and loss rate using
C     .    slope at i+1/2, assuming mass in tank has changed by fRK_dMass1/2.
C-------------------------------------------------------------------------------
            call EvalH2Tank_DEqn ( 
     &           fSubSol_TS          ,
     &           fRK_TankMass + fRK_dMass1 / 2.0 ,  
     &           fRateMass_change    , 
     &           fRateFlow_in        , 
     &           fRateFlow_out       , 
     &           fRate_tankLoss      )

C-----------Integrate results over time-slice

            fRK_dMass2      =  fRateMass_change * fSubSol_TS ! (kg)
            fRKFlow_in2     =  fRateFlow_in     * fSubSol_TS ! (kg/s)
            fRKFlow_out2    =  fRateFlow_out    * fSubSol_Ts ! (kg/s)
            fRK_tankLoss2   =  fRate_tankLoss   * fSubSol_Ts ! (kg/s)

C-------------------------------------------------------------------------------
C     .     Get tank mass rate of change, flow rates and loss rate using
C     .     slope at i+1/2, assuming mass in tank has changed by fRK_dMass2/2.
C-------------------------------------------------------------------------------
            call EvalH2Tank_DEqn ( 
     &           fSubSol_TS          ,
     &           fRK_TankMass + fRK_dMass2 / 2.0 ,  
     &           fRateMass_change    , 
     &           fRateFlow_in        , 
     &           fRateFlow_out       , 
     &           fRate_tankLoss      )

C-----------Integrate results over time-slice

            fRK_dMass3      =  fRateMass_change * fSubSol_TS ! (kg)
            fRKFlow_in3     =  fRateFlow_in     * fSubSol_TS ! (kg/s)
            fRKFlow_out3    =  fRateFlow_out    * fSubSol_Ts ! (kg/s)
            fRK_tankLoss3   =  fRate_tankLoss   * fSubSol_Ts ! (kg/s)

C-------------------------------------------------------------------------------
C     .     Get tank mass rate of change, flow rates and loss rate using
C     .     slope at i+1, assuming mass in tank has changed by fRK_dMass3.
C-------------------------------------------------------------------------------

            call EvalH2Tank_DEqn ( 
     &           fSubSol_TS          ,
     &           fRK_TankMass + fRK_dMass3 ,  
     &           fRateMass_change    , 
     &           fRateFlow_in        , 
     &           fRateFlow_out       , 
     &           fRate_tankLoss      )

C-----------Integrate results over time-slice
            fRK_dMass4      =  fRateMass_change * fSubSol_TS ! (kg)
            fRKFlow_in4     =  fRateFlow_in     * fSubSol_TS ! (kg/s)
            fRKFlow_out4    =  fRateFlow_out    * fSubSol_Ts ! (kg/s) 
            fRK_tankLoss4   =  fRate_tankLoss   * fSubSol_Ts ! (kg/s)

C-------------------------------------------------------------------------------
C     .     Average results over time-slice. Note: segment midpoints 2 & 3
C     .     have twice the weight than the end points 1 & 4
C-------------------------------------------------------------------------------

            fRK_dMass_T = 
     &           + fRK_dMass1 / 6.0
     &           + fRK_dMass2 / 3.0
     &           + fRK_dMass3 / 3.0
     &           + fRK_dMass4 / 6.0

            fRKFlow_in_T =
     &           + fRKFlow_in1 / 6.0
     &           + fRKFlow_in2 / 3.0
     &           + fRKFlow_in3 / 3.0
     &           + fRKFlow_in4 / 6.0

            fRKFlow_out_T =
     &           + fRKFlow_out1 / 6.0
     &           + fRKFlow_out2 / 3.0
     &           + fRKFlow_out3 / 3.0
     &           + fRKFlow_out4 / 6.0

            fRK_tankLoss_T = 
     &           + fRK_tankLoss1 / 6.0
     &           + fRK_tankLoss2 / 3.0
     &           + fRK_tankLoss3 / 3.0
     &           + fRK_tankLoss4 / 6.0

C-------------------------------------------------------------------------------
C     Make sure change in mass does not exceed max/min permissible values
C-------------------------------------------------------------------------------

            if ( fRK_TankMass + fRK_dMass_T .gt. fH2massMAX .and. 
     &           fRK_dMass_T .gt. 0.0 
     &           .and. .not. bNoMassMaxLimit ) then

C..............Mass change limited by tank max capacity 
               fRK_dMass_T = fH2massMAX - fRK_TankMass
               fRKFlow_in_T = fRK_dMass_T 
     &              + fRKFlow_out_T + fRK_tankLoss_T

            elseif ( fRK_TankMass + fRK_dMass_T .lt. fH2massMIN .and. 
     &              fRK_dMass_T .lt. 0.0 ) then
C..............Mass  change limited by tank min capacity       
               fRKFlow_out_T = ( fRK_TankMass - fH2massMIN ) 
     &              +  fRKFlow_in_T - fRK_tankLoss_T

C..............Can't have a negitive flow 
               if ( fRKFlow_out_T .lt. 0.0 ) then 
                  fRKFlow_out_T = 0.0

C.................Check to see if result produces tank mass below zero
                  if ( fRK_TankMass 
     &                 + fRKFlow_in_T - fRK_tankLoss_T .lt. 0.0 ) then

                     fRK_tankLoss_T = fRK_TankMass +  fRKFlow_in_T 

                  end if
                     
               endif 

C..............Total change in tank for time slice
               fRK_dMass_T = fRKFlow_in_T 
     &              - fRKFlow_out_T - fRK_tankLoss_T
           
            endif
C------------------------------------------------------------------------------
C     .     Calculate cumulative amounts
C------------------------------------------------------------------------------         
            
            fRK_TankMass = fRK_TankMass + fRK_dMass_T 
            fRKFlow_in   = fRKFlow_in   + fRKFlow_in_T
            fRKFlow_out  = fRKFlow_out  + fRKFlow_out_T
            fRK_tankLoss = fRK_tankLoss + fRK_tankLoss_T

         enddo
C-------------------------------------------------------------------------------
C     .  Save future stored-mass in future plant additional data array PCDatF.
C-------------------------------------------------------------------------------
         NAPDat(iComponent)  = 2 !  1 additional output 
         PCDatF(iComponent,1) = fRK_TankMass ! (kg)

C-------------------------------------------------------------------------------
C     .  Store 'future' mass: Variable fH2massFuture is in common, and will 
C     .  be used to report the future mass of hydrogen in the XML reporting
C     .  facility
C-------------------------------------------------------------------------------
         fH2massFuture =  fRK_TankMass

C-------------------------------------------------------------------------------
C     .  Average integrated flows in & out of storage unit over ESP-r 
C     .  time step 
C-------------------------------------------------------------------------------
         fAVGFlow_in   = fRKFlow_in   / fTS_Duration ! (kg/s)
         fAVGFlow_out  = fRKFlow_out  / fTS_Duration ! (kg/s)
         fAVG_tankLoss = fRK_tankLoss / fTS_Duration ! (kg/s)         


         if ( fAVGFlow_out .lt. 0.0 ) then
            fAVGFlow_out = 0.0
         endif

C........is there a temperature difference to heat or cool tank?
         call eclose ( fH2temperature_in,
     &                 fTank_TemperatureOLD,
     &                 fTolerance,
     &                 bTempsAreEqual )
C-------------------------------------------------------------------------------
C     .  Guesstimate temperature in tank - assume average mass over tank.
C-------------------------------------------------------------------------------
         if ( bNoMassFlowIn .and. bNoInitialMass ) then
C...........Tank is empty & no flow. Set temperature to zero .
            fTank_TemperatureNEW = 0.

         elseif ( bNoInitialMass ) then
C...........Tank is empty, but has flow. set temperature to inlet temperature
            fTank_TemperatureNEW =  fH2temperature_in

         elseif ( bNoMassFlowIn ) then
C...........Tank is not empty, but there's no incomming flow. Store temperature
C...........from last timestep
            fTank_TemperatureNEW = fTank_TemperatureOLD
         elseif ( bTempsAreEqual ) then
C...........Inlet temperature & tank temperatures are the same. Temperaature
C...........wont' change 
            fTank_TemperatureNEW = fTank_TemperatureOLD            
         else 
C----------------------------------------------------------------------------------
C     .     Tank is not empty, and flow is entering the tank. First approximation: 
C     .     assume average CP prevails.
C     .
C     .     temperature differential equation:
C     .
C     .                   dT
C     .      M(t)(Cp avg) -- = (mass flow in) (Cp avg) * ( T in  -  T )
C     .                   dt
C     .
C     .     Where:
C     .
C     .       M(t) = initial mass + (mass flow in - mass flow out ) * t
C     .      
C     .
C     .     Solution is:
C     .
C     .                           
C     .        T future  = T in +
C     .                    C * exp [ A * ln ( initial mass
C     .                                       + ( mass flow in - mass flow out) * t
C     .                                      )
C     .                             ]
C     .
C     .     Where:
C     .
C     .        C = ( T initial - T in ) / exp ( A * ln ( initial mass ) )
C     .
C     .        A = - mass flow in / ( mass flow in - mass flow out )
C     .
C     .
C     .
C     .     Special Case: M in = M out
C     .
C     .
C     .                dT
C     .      M(Cp avg) -- = (mass flow in) (Cp avg) * ( T in  -  T )
C     .                dt
C     .
C     .     Solution is
C     .
C     .       T_future =  T in + ( T initial - T in )
C     .                             * exp [ mass flow in * time / initial mass ]
C     .
C----------------------------------------------------------------------------------
            call eclose ( fAVGFlow_in,
     &                    fAVGFlow_out,
     &                    fTolerance,
     &                    bMassFlowsAreEq )

            if ( bMassFlowsAreEq ) then
C.............Special case: mass flows are equal
              fTank_TemperatureNew = fH2temperature_in
     &            + ( fTank_TemperatureOLD - fH2temperature_in )
     &            * exp ( fAVGFlow_in / fH2massPresent * fTS_duration)
            else
C.............General case
              fIntegrationConstant =
     &             ( fTank_TemperatureOLD - fH2temperature_in )
     &              / exp( -1.0 *  fAVGFlow_in
     &                          / ( fAVGFlow_in -  fAVGFlow_out )
     &                          * log ( fH2massPresent )
     &                    )

              fTank_TemperatureNew = fH2temperature_in
     &           + fIntegrationConstant
     &           * exp ( - 1.0 * fAVGFlow_in
     &                         / ( fAVGFlow_in -  fAVGFlow_out )
     &                         * log ( fH2massPresent
     &                                + ( fAVGFlow_in - fAVGFlow_out )
     &                                * fTS_duration )
     &                  )
            
            endif

         endif
C-------------------------------------------------------------------------------
C     .  Save exit temperature
C-------------------------------------------------------------------------------         
         PCDatF(iComponent,2) = fTank_TemperatureNew ! (oC)

C-------------------------------------------------------------------------------
C     .  Calculate ESP-r component matrix coefficients.
C
C     .  ESP-r State Eqn:  
C
C     .  Coeff1 * (flow through current node ) 
C     .                        = Calculated flow rate at outlet
C     .
C
C-------------------------------------------------------------------------------

         fCoeffArray(1) = 1.0
         fCoeffArray(2) = 0.0
         fCoeffArray(3) = fAVGFlow_out

      endif



C-------------------------------------------------------------------------------
C     End of subroutine:  H2_storage_model
C-------------------------------------------------------------------------------
      return 
      end


C=========================H2_storage_post_process =========================
C
C     Created by: Alex Ferguson
C     Created on: August 2004.
C     Copyright: CETC
C     ________
C     ABSTRACT: 
C
C     This subroutine transports data associated with the hydrogen storage
C     tank to the XML reporting facilities. 
C    
C===========================================================================
      Subroutine H2_storage_post_process (iComponentIndex) 
      implicit none 

#include "plant.h"

C External functions.     
      integer lnblnk

C.....Passed variables 
      integer iComponentIndex   ! index of FC in plant component network

C.....ESP-r Commons
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15 pcname

C.....Local Variables
      CHARACTER*128 H3K_rep_NAME ! H3Kreporting object
      character*128 root_name    ! root name of component node 
      character*128 hold_name    ! temporary variable for string manipulation
      logical bClose             ! flag for close-to-zero comparison

C.....References
      real  H3K_connect_property ! function returning data for a given connection

C.....Named constants
      integer iTemperature      ! named constant for temperature properity 
      integer i1stFlow         ! named constant for first phase flow properity
      integer i2ndFlow    ! named constant for second phase flow properity      
      parameter ( iTemperature     = 1,
     &            i1stFlow        = 2, 
     &            i2ndFlow        = 3)

C.....State variables
      common/PCVAL/
     &     CSVF(MPNODE,MPVAR),  ! Plant future state variables
     &     CSVP(MPNODE,MPVAR)   ! Plant present state variables
      real CSVF, CSVP

C.....Plant component present/future variables 
      common/pcvar/
     & pctf(mpcon),
     & pcrf(mpcon),
     & puaf(mpnode),
     & pcqf(mpnode),
     & pcntmf(mpcom),
     & pctp(mpcon),
     & pcrp(mpcon),
     & puap(mpnode),
     & pcqp(mpnode),
     & pcntmp(mpcom)
      real pctf,pcrf,puaf,pcqf,pcntmf,pctp, pcrp, puap, pcqp, pcntmp


C-------------------------------------------------------------------------------      
C     Commons associated with H2 storage tank
C-------------------------------------------------------------------------------
      common / H2StorageTank /  
     &     fH2massPresent,  
     &     fH2massFlow_in,  
     &     fH2massFlow_out, 
     &     fH2massMIN,       
     &     fH2massMAX,    
     &     bNoMassMaxLimit,
     &     fLossCoeff_a,   
     &     fLossCoeff_b,   
     &     fLossCoeff_c,   
     &     fTolerance,      
     &     fAVGFlow_in,    
     &     fAVGFlow_out,   
     &     fAVG_tankLoss,  
     &     fH2massFuture  

C-------------------------------------------------------------------------------     
C     Tank parameters
C-------------------------------------------------------------------------------
      real fLossCoeff_c         ! coefficient of hydrogen loss 0 (kg/s)
      real fLossCoeff_b         ! coefficient of hydrogen loss 1 (/s)
      real fLossCoeff_a         ! coefficient of hydrogen loss 2 (/kg.s)

      real fH2massMAX           ! maximum amount of mass that can be stored 
                                ! in tank (kg)
      real fH2massMIN           ! minimum amount of mass in tank during 
                                ! normal operation.

      logical bNoMassMaxLimit   ! flag indicating that there is no limit to mass that
                                ! can be stored in tank

      real fH2massPresent       ! storage unit's state of charge (kg)
                                ! at present time row 
      real fH2massFuture        ! storage unit's state of charge (kg)
                                ! at future time row

      real fH2massFlow_in
      real fH2massFlow_out

C-------------------------------------------------------------------------------
C     Runge-Kutta results, averaged over ESP-r time-step
C-------------------------------------------------------------------------------
      real fAVGFlow_in         ! Actual flow rate into tank, averaged over timestep. (kg/s)
      real fAVGFlow_out        ! Actual flow rate out of tank, averaged over timestep (kg/s)
      real fAVG_tankLoss       ! Actual loss rate from tank, averaged over timestep (kg/s)

      real fTolerance           ! named constant describing tolerance of 
                                ! close-to-zero comparisons

C.....Get component name
      hold_name = pcname(iComponentIndex) 
C.....Format string as: 'plant/NAME/misc_data'
      WRITE(root_name,'(A,A,A)') 
     &     'plant/',
     &     hold_name(1:lnblnk(hold_name)),
     &     '/misc_data'

C.....Rate of flow into tank
C.....-> set name
      WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/hydrogenFlow_in'
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(kg/s)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'KgPerStoKG',
     &     ' '
     &     )
C.....-> update data 
         call REP_REPORT(
     &        fAVGFlow_in,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )
C.....Rate of flow into tank
C.....-> set name
      WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/hydrogenFlow_out'
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(kg/s)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'KgPerStoKG',
     &     ' '
     &     )
C.....-> update data 
         call REP_REPORT(
     &        fAVGFlow_out,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )
C.....Rate of flow into tank
C.....-> set name
      WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/hydrogenLoss'
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(kg/s)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'KgPerStoKG',
     &     ' '
     &     )
C.....-> update data 
         call REP_REPORT(
     &        fAVG_tankLoss,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )

C.....Rate of flow into tank
C.....-> set name
      WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/hydrogen_storage/present'
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(kg)'
     &     )

C.....-> update data 
         call REP_REPORT(
     &     fH2massPresent,
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &     )

C.....Rate of flow into tank
C.....-> set name
      WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/hydrogen_storage/future'
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(kg)'
     &     )

C.....-> update data 
         call REP_REPORT(
     &        fH2massFuture,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )
      return 
      end


C=========================EvalH2Tank_DEqn  ===============================
C
C     Created by: Alex Ferguson
C     Created on: August 2004.
C     Copyright: CETC
C     ________
C     ABSTRACT: 
C
C     This subroutine simply evaluates the instantaneous rate of
C     change in the H2 storage tank using the state equation:
C
C     .  dM 
C     . ---- = (m_in) - (m_out) - (Ca) * M^2 - (Cb) * M - (Cc)  
C     .  dt
C
C     The actual flow rates & losses from the tank are also calculated
C     according to the following assumptions:
C
C     . - The mass in the tank cannot exceed fH2massMAX, unless 
C     .   the value of fH2massMAX has been specified as less than zero,
C     .   which indicates that the tank has infinite volume.
C     . - If the tank's mass is equal to fH2massMAX, the flow 
C     .   into the tank cannot exceed the sum of the flow out of the 
C     .   tank and the losses from the tank
C     . - If the mass drops below fH2massMAX, the flow from the tank
C     .   cannot exceed the difference between the flow into the tank and
C     .   the losses from the tank.
C     . - If the mass of the tank is zero, the losses from the tank are
C     .   also zero.
C
C     INPUTS: 
C     
C     . - mass of H2 in tank (fTankMass)
C     . - flows into and out of tank fH2massFlow_in, fH2massFlow_out
C     .   (in commons), as determined by ESP-r 
C     . - loss coefficients ( fLossCoeff_a,  fLossCoeff_b,  fLossCoeff_c )
C     .   (in commons)
C     . - Tank max and minimum masses (fH2massMIN, fH2massMAX) 
C     .   (in commons)
C     
C     OUTPUTS:
C      
C     . - instantaneous change in mass of tank (f_dMass)
C     . - instantaneous actual flow into tank (fFlow_in)
C     . - instantaneous actual flow out of tank (fFlow_out)
C     . - instantaneous actual rate of h2 loss from tank (f_tankLoss)
C
C===============================================================================


      Subroutine EvalH2Tank_DEqn(
     &     fSubSol_TS,          ! input
     &     fTankMass,           ! input
     &     f_dMass,             ! output
     &     fFlow_in,            ! output
     &     fFlow_out,           ! output
     &     f_tankLoss           ! output
     &     ) 

      implicit none 
C-------------------------------------------------------------------------------
C.....Passed variables (inputs)
C-------------------------------------------------------------------------------
      real fSubSol_TS           ! sub-solution timestep 

      real fTankMass            ! mass in the tank (kg) 
      
      real fH2massFlow_in       ! rate of mass flow into the unit (kg/s)
      real fH2massFlow_out      ! rate of mass flow out of the unit (kg/s)

      real fLossCoeff_c         ! coefficient of hydrogen loss 0 (kg/s)
      real fLossCoeff_b         ! coefficient of hydrogen loss 1 (/s)
      real fLossCoeff_a         ! coefficient of hydrogen loss 2 (/kg.s)

      real fH2massMIN           ! minimum permissible mass in the tank 
      real fH2massMAX           ! maximum permissible mass in the tank
      logical bNoMassMaxLimit   ! flag indicating that there is no limit 
                                ! to the amount of mass that can be stored 
                                ! in the building 
      real fTolerance           ! tolerance for close-to-zero comparisons

C-------------------------------------------------------------------------------
C     Unused variables carried in  H2StorageTank  common:
C-------------------------------------------------------------------------------
      real fAVGFlow_in, fAVGFlow_out, fAVG_tankLoss
      real fH2massPresent, fH2massFuture
C-------------------------------------------------------------------------------
C     Local variables: these are copies of data in the H2StorageTank common
C     that can be constrained depending on the real conditions in the tank
C-------------------------------------------------------------------------------

      real fFlow_in            ! rate of mass flow into the unit (kg/s)
      real fFlow_out           ! rate of mass flow out of unit (kg/s)  
      real f_dmass              ! rate of change in the tank mass (kg/s)
      real f_tankLoss          ! rate of H2 loss from tank (kg/s)

      logical bNumsAreClose      ! Logical variable for close to zero comparisons

C-------------------------------------------------------------------------------      
C     Commons associated with H2 storage tank
C-------------------------------------------------------------------------------
      common/H2StorageTank/  
     &     fH2massPresent,    
     &     fH2massFlow_in,    
     &     fH2massFlow_out,   
     &     fH2massMIN,         
     &     fH2massMAX, 
     &     bNoMassMaxLimit,
     &     fLossCoeff_a,       
     &     fLossCoeff_b,       
     &     fLossCoeff_c,       
     &     fTolerance,          
     &     fAVGFlow_in,        
     &     fAVGFlow_out,       
     &     fAVG_tankLoss,      
     &     fH2massFuture      

C-------------------------------------------------------------------------------
C     Check if tank mass is zoro
C-------------------------------------------------------------------------------

      call eclose ( fTankMass, 0.0, fTolerance, bNumsAreClose )

C-------------------------------------------------------------------------------
C     Evaluate rate of change in tank:
C-------------------------------------------------------------------------------

      if ( fTankMass .GE. fH2massMAX .AND. 
     &     fH2massMAX .GT. 0.0 .and.
     &     .not. bNoMassMaxLimit ) then 
C-------------------------------------------------------------------------------
C     .  Tank is fully charged: flow into tank is constrained by total 
C     .  flow from tank and losses:
C-------------------------------------------------------------------------------
         f_tankLoss =  
     &        fLossCoeff_c 
     &        + fLossCoeff_b * fTankMass 
     &        + fLossCoeff_a * fTankMass ** 2.0

         fFlow_out = fH2massFlow_out

         if ( fFlow_out + f_tankLoss .lt.  fH2massFlow_in ) then

            fFlow_in = fFlow_out + f_tankLoss 

         else

            fFlow_in = fH2massFlow_in

         endif 

      elseif ( fTankMass .LT. fH2massMIN 
     &        .OR. bNumsAreClose ) then
C-------------------------------------------------------------------------------
C     .  Tank is under-charged: flow out of tank is constrained by total flow into 
C     .  tank and losses. 
C     .  -> check if tank mass is zero, and set losses to zero if true.
C-------------------------------------------------------------------------------
         
         if ( bNumsAreClose ) then

            f_tankLoss = 0. 
            
         else 
            f_tankLoss = 
     &           fLossCoeff_c 
     &           + fLossCoeff_b * fTankMass 
     &           + fLossCoeff_a * fTankMass ** 2.0

         endif
         
         fFlow_in = fH2massFlow_in

         if (  fFlow_in - f_tankLoss .lt. fH2massFlow_out ) then
            fFlow_out = fFlow_in - f_tankLoss
         else 
            fFlow_out = fH2massFlow_out
         endif
         

      else
C-------------------------------------------------------------------------------
C     Tank flows are not constrained
C-------------------------------------------------------------------------------
         f_tankLoss = 
     &        fLossCoeff_c 
     &        + fLossCoeff_b * fTankMass 
     &        + fLossCoeff_a * fTankMass ** 2.0
                 
         fFlow_in  = fH2massFlow_in

         fFlow_out = fH2massFlow_out  
         
      endif
C-------------------------------------------------------------------------------
C     Calculate total change in tank's mass
C-------------------------------------------------------------------------------
      f_dMass =  fFlow_in -  fFlow_out - f_tankLoss 


      return
      end

C=============================== zeroH2Flow  ===============================
C
C     Created by: Alex Ferguson
C     Created on: August 2004.
C     Copyright: CETC
C     ________
C     ABSTRACT: 
C     When called, this subroutine returns an array of coefficients 
C     (fCoeffArray) for component iComponent that presumbably does 
C     not support the current plant matrix type. It tests each 
C     coefficient location on the matrix:
C
C     . - the coefficient is set to 1.0 if it lies on the diagnol
C     . - the coefficient is set to 0. otherwise
C
C     The resulting coeffieient array ensures that the component's 
C     state-varuables will evaluate to zero, and have no impact on 
C     the simulation of other components. 
C
C     zeroH2Flow was developed to permit mixing of plant comoonents
C     that support the hydrogen flow matrix (istats = 4) in networks
C     with components that do not. However, it could be used to obtain
C     an identity matrix (I=0) for any component. 
C
C     INPUTS:
C     . - iComponent -> index of component in plant network
C     
C     OUTPUTS:
C     . - fCoeffArray -> Coefficients describing identity matrix for 
C     .                   plant component.
C
C
C===============================================================================
      subroutine zero_H2_Flow (iComponent, fCoeffArray)
      implicit none

#include "plant.h"

C-------------------------------------------------------------------------------------
C     Passed variables
C-------------------------------------------------------------------------------------
      integer iComponent        ! pointer to plant component 

      real fCoeffArray(mpcoe)  ! Array of matrix coefficients

C-------------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------------
      common/c12ps/
     &     npcdat(mpcom,9),     ! miscellaneous plant data
     &     ipofs1(mcoefg),      ! matrix coefficient location (rows)
     &     ipofs2(mcoefg,mpvar) ! matrix coefficient location (cols)
      integer npcdat, ipofs1, ipofs2

      common / plant_coeff / 
     &     iPMC_locations(mpcom,mpcoe)  ! plant matrix locations
      integer iPMC_locations

C-------------------------------------------------------------------------------------
C     References
C-------------------------------------------------------------------------------------
      logical Coeff_diagnol_test ! function for testing if a coefficient lies
                                 ! on the diagnol of the coefficient matrix.

C-------------------------------------------------------------------------------------
C     Local variables
C-------------------------------------------------------------------------------------
      integer iNodeCount        ! Number of nodes
      integer iCoefCount        ! Matrix coefficient count
      integer iCoef              ! counter
      integer iCoef_location     ! coefficient location

                                
C-------------------------------------------------------------------------------------      
C     Initialize
C-------------------------------------------------------------------------------------  
      iNodeCount = npcdat (iComponent, 8)  ! number of nodes
      iCoefCount = npcdat (iComponent, 6)  ! number of coefficients 

C------------------------------------------------------------------------------------- 
C     Loop through component coefficients and test if each coefficient is on the 
C     matrix's diagnol using function Coeff_diagnol_test().
C
C     .  - If coefficient is on matrix diagnol, set coefficient matrix value to 1.
C     .  - If coefficient is not on matrix diagnol, set coefficient matrix value to 0.
C-------------------------------------------------------------------------------------  
      do iCoef = 1,  mpcoe

          iCoef_location = iPMC_locations(iComponent,iCoef)

          if (Coeff_diagnol_test(iCoef_location, iNodeCount) .and.
     &         iCoef .LE. iCoefCount ) then

             fCoeffArray(iCoef) = 1.0
          else

             fCoeffArray(iCoef) = 0.0
          endif

      enddo

      return 
      end

C====================== Coeff_diagnol_test ===================================
C
C Created by: Alex Ferguson
C Created on: August 11, 04
C Copyright: CETC
C ________
C ABSTRACT: 
C 
C This function tests the i'th coefficient in an n x n matrix and returns true 
C if it is on the matrix's diagnol.
C
C===============================================================================

      logical function Coeff_diagnol_test(iCoeff, iRowCount)

      implicit none
C.....Passed variables
      integer iCoeff            ! index of matrix coefficient 
      integer iRowCount        ! number of rows in the matrix
C.....Local variables
      integer iRow              ! counter 

      coeff_diagnol_test = .false.

C-------------------------------------------------------------------------------
C     Loop through rows, and check diagnol. Location of diagnol coeffiicents 
C     defined by
C 
C         location = (rowIndex - 1) * rowCount + rowIndex
C------------------------------------------------------------------------------  
      do iRow = 1, iRowCount
         if ( (iRow-1) * iRowCount + iRow .eq. iCoeff ) then
            Coeff_diagnol_test = .true.
         endif
      enddo

      return
      end

C====================== FirstCubicRoot ===================================
C
C Created by: Alex Ferguson
C Created on: Sept. 07, 2004
C Copyright: CETC
C ________
C ABSTRACT: 
C 
C This function returns the lowest positive root for a supplied cubic
C polynomial of the form:
C
C    Ax^3 + Bx^2 + Cx + D = 0
C
C Returns -1.0 if no positive roots are found. A, B, C, D may be zero.
C
C Quadratic / cubic solutions are widely available in literature. Ex:
C
C     Spiegal 1997, "Schaum's outlines: Mathematical Handbook", 
C     .   McGraw Hill. p 32.
C
C=========================================================================

      real function fFirstCubicRoot(fA,fB,fC,fD,fToleranceRoot)

      implicit none

C.....Passed variables
      real fA, fB, fC, fD       ! coefficients for polynomial equation

C.....Flags for constant/linear/second-order/third-order solution
      logical bNo0thOrder       ! equation does not contain constant
      logical bNo1stOrder       ! equation does not contain linear term
      logical bNo2ndOrder       ! equation does not contain 2nd-order term
      logical bNo3rdOrder       ! equation does not contain 3rd-order term
     
      logical bNumsAreClose      ! flag for close to zero comparisons

C.....Local variables
      real fToleranceDisc
      parameter ( fToleranceDisc = 1e-09 ) ! tolerance for close-to-zero 
                                ! with discriminate

      real fToleranceRoot       ! tolerance for comparisons between root
                                ! and zero 

      real fPi                  ! pi
      parameter ( fPi = 3.14152719 )

      logical bRootFound        ! flag indicating if a root has been found

      integer iCount            ! counter

C.....Cubic solution variables
      real fDiscriminate
      real fE, fF, fG           ! coefficients used by cubic solution
      real fCubicQ, fCubicR, fCubicS, fCubicT ! intermediate variables 
      real fTheta               ! intermediate variables 
      
      real fSign                ! temporary variable used to store the sign 
                                ! of an expression



C.....Roots
      real fRoots(3)

C.....Initialize roots array and function value
      fRoots(1) = -99.0
      fRoots(2) = -99.0
      fRoots(3) = -99.0
      fFirstCubicRoot = -99.0

C.....check if coefficients a,b,c,d are non-zero 
      call eclose( fA, 0.0, fToleranceDisc, bNo3rdOrder)
      call eclose( fB, 0.0, fToleranceDisc, bNo2ndOrder)
      call eclose( fC, 0.0, fToleranceDisc, bNo1stOrder)
      call eclose( fD, 0.0, fToleranceDisc, bNo0thOrder)

      if ( .not. bNo3rdOrder ) then
C-----------------------------------------------------------------------
C     Cubic equation
C     .  Re-arrange coefficients into Cubic solutuon lexicion:
C        
C     .  x^3 + Ex^2 + Fx + G = 0
C-----------------------------------------------------------------------      
         fE = fB/fA
         fF = fC/fA
         fG = fD/fA
C-----------------------------------------------------------------------
C     .  Intermediate term Q
C     .
C     .      3F - E^2
C     .  Q = --------
C     .         9
C-----------------------------------------------------------------------

         fCubicQ = (3.0 * fF - fE ** 2.0) / 9.0

C-----------------------------------------------------------------------
C     .  Intermediate term R
C     .
C     .      9EF - 27G - 2E^3
C     .  R = ----------------
C     .            54
C-----------------------------------------------------------------------

         fCubicR= ( 9.0 * fE * fF - 27.0 * fG - 2.0 * fE ** 3.0 ) /54.0

C-----------------------------------------------------------------------
C     .  Discriminate:
C     .  
C     .  Discriminate = Q^3 + R^2
C
C-----------------------------------------------------------------------
         
         fDiscriminate = fCubicQ ** 3.0 + fCubicR ** 2.0

C-----------------------------------------------------------------------
C     .  Solution depends on whether discriminate is greater than zero:
C-----------------------------------------------------------------------
         call eclose( fDiscriminate, 0.0, fToleranceDisc, bNumsAreClose) 

         if ( bNumsAreClose .or.  fDiscriminate .gt. 0.0 ) then 

C-----------------------------------------------------------------------
C     .     One or two real roots
C
C     .     Intermediate terms S & T:
C     .
C     .     S = (R + (Q^3 + R^2)^0.5)^(1/3)
C     . 
C     .     T = (R - (Q^3 + R^2)^0.5)^(1/3)
C     .
C     .
C     .     NOTE: g77 refuses to take the cubic root of a negative
C     .           number --- returns NAN instead! Convert result to 
C     .           positive, and multiply by its original sign.
C-----------------------------------------------------------------------
            fSign = (fCubicR + (fDiscriminate) ** 0.5) / 
     &           ABS (fCubicR + (fDiscriminate) ** 0.5) ! -1 or 1
            fCubicS = (abs(fCubicR + (fDiscriminate) ** 0.5))
     &           ** ( 1.0 / 3.0 ) * fSign
            fSign = (fCubicR - (fDiscriminate) ** 0.5) / 
     &           ABS (fCubicR - (fDiscriminate) ** 0.5) ! -1 or 1           
            fCubicT = (abs(fCubicR - (fDiscriminate) ** 0.5))
     &           ** ( 1.0 / 3.0 ) * fSign
C-----------------------------------------------------------------------
C     .     First Root is: 
C     .
C     .     X = S + T - fE / 3
C-----------------------------------------------------------------------
            fRoots(1) = fCubicS + fCubicT - fE / 2.0

C-----------------------------------------------------------------------
C     .     Subsequent root at:
C     . 
C     .     X = -0.5 * (S + T)  - fE / 3  if Discriminate = 0. 
C     .
C-----------------------------------------------------------------------
            if ( bNumsAreClose ) then 
               fRoots(2) = -0.5 * (fCubicS + fCubicT) - fE / 2.0  
            endif

         else
C-----------------------------------------------------------------------
C     .     Three real roots.
C
C     .     Intermediate term theta:
C     .
C     .     theta = acos ( R / -(Q^3) )
C     . 
C-----------------------------------------------------------------------
            
            fTheta = acos ( fCubicR / ((-1.0*(fCubicQ ** 3.0))**0.5))

C-----------------------------------------------------------------------
C     .      Roots:
C     . 
C     .      X = 2 * (-Q)^0.5 cos ( theta/3 ) - fE / 3
C     .      X = 2 * (-Q)^0.5 cos ( theta/3 + 2*pi/3 ) - fE / 3
C     .      X = 2 * (-Q)^0.5 cos ( theta/3 + 4*pi/3 ) - fE / 3
C-----------------------------------------------------------------------
            fRoots(1) = 2.0 
     &           * ( -fCubicQ )**0.5 * cos( ftheta / 3.0 ) - fE / 3.0

            fRoots(2) = 2.0 
     &           * ( -fCubicQ ) ** 0.5 
     &           * cos( ftheta / 3.0 + 2.0 * fPi / 3.0 ) 
     &           - fE / 3.0
            fRoots(3) = 2.0 
     &           * ( -fCubicQ ) ** 0.5 
     &           * cos( ftheta / 3.0 + 4.0 * fPi / 3.0 ) 
     &           - fE / 3.0

         endif



      elseif ( .not. bNo2ndOrder ) then
C------------------------------------------------------------------------
C     Quadratic equation - Note that under the current lexicion, 
C     the quadratic equation is:
C
C     .      -C +- (C^2 - 4BD)^0.5 
C     .  X = ---------------------
C     .              2B
C
C     .  Discriminate = c^2 - 4bd
C------------------------------------------------------------------------
 
         fDiscriminate = fC ** 2.0 - 4.0 * fB * fD
         
         call eclose( fDiscriminate, 0.0, fToleranceDisc, bNumsAreClose)

         if ( bNumsAreClose ) then
C------------------------------------------------------------------------
C     .     One root:
C     .  
C     .     X = -C / 2B
C     .  
C------------------------------------------------------------------------
            fRoots(1) = -fC / ( 2.0 * fA )

         elseif (  fDiscriminate .GT. 0.0 ) then 
C------------------------------------------------------------------------
C     .     Two real roots
C     .  
C     .      -C +- (C^2 - 4BD)^0.5 
C     .  X = ---------------------
C     .              2B 
C     .  
C------------------------------------------------------------------------
            fRoots(1) = (-fC + (fC**2.0-4*fB*fD)**0.5)/(2.0*fB)
            fRoots(2) = (-fC - (fC**2.0-4*fB*fD)**0.5)/(2.0*fB)            
         else 
C------------------------------------------------------------------------
C     .     No real roots
C-----------------------------------------------------------------------

         endif

      elseif ( .not. bNo1stOrder ) then
C------------------------------------------------------------------------
C     First order equation. 1 root:
C     
C     .  X = -D/C 
C     
C------------------------------------------------------------------------
         fRoots(1) = -fD / fC

      elseif ( bNo0thOrder ) then
C------------------------------------------------------------------------
C     polynimial returns zero for all values of X. Root is 0.
C------------------------------------------------------------------------
         fRoots(1) = 0.0

      else
C------------------------------------------------------------------------
C     polynomial is non-zero constant. no roots.
C------------------------------------------------------------------------        
      endif


C------------------------------------------------------------------------
C     Loop through roots, and find the smallest, non-negative root.
C------------------------------------------------------------------------

      bRootFound = .false. 

      do iCount = 1, 3

         call eclose( fRoots(iCount), 0.0, 
     &        fToleranceRoot, bNumsAreClose )

         if ( bNumsAreClose ) fRoots(iCount) = 0.0

         if ( bNumsAreClose .or.  fRoots(iCount) .gt. 0.0 ) then
C...........Root is non-negative,

            if ( .not. bRootFound .or. 
     &           fRoots(iCount) .lt. fFirstCubicRoot ) then

C..............Root is less than current minimum root, or this is 
C..............the first root. Update value

               fFirstCubicRoot = fRoots(iCount)

               bRootFound = .true.

            end if

         endif 

      enddo

      return
      end


C--------------------------------------------------------------------------
