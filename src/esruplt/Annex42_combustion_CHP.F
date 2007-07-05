C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004--2006. Please Contact Ian
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
C================== Annex42_combustion_CHP.F =========================
C
C Date:      Dec 16, 2004
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada, (2004--2006)
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
C   A42_combustion_chp_coeff_gen: The top-level routine called
C       by the plant modelling domain to characterize
C       the SE/ICE chp unit
C
C   A42_CHP_thermal_char: Top level-procedure controlling the thermal
C       characterization of the Stirling engine (called when ISTATS=1)
C
C   A42_CHP_Collect_Data: Collects Stirling model inputs and performs
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
C   A42_CHP_H3Kreports_module: Transports data to the h3kreports module
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
C   Beausoleil-Morrison, I., editor (2007). Experimental Investigation 
C     of Residential Cogeneration Devices and Calibration of Annex 42 
C     Models. IEA/ECBCS Annex 42.
C
C   Beausoleil-Morrison, I. and Kelly, N., editors (2007). 
C     Specifications for Modelling Fuel cell and Combustion-Based 
C     Residential Cogneration devices within Whole-Building
C     Simulation programs. IEA/ECBCS Annex 42. 
C
C======================================================================

C------------------ A42_combustion_chp_coeff_gen ----------------------
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
C A42_CHP_thermal_char.
C
C
C Inputs:
C
C  iPComp         - index of component in plant network
C  iType          - flag indicating 'type' of model to be invoked.
C  iStats         - index of plant matrix being solved
C
C Outputs
C  fCOut          - array containing components matrix
C                     coefficients
C
C----------------------------------------------------------------------


      subroutine A42_combustion_chp_coeff_gen (
     &               iPComp,
     &               iType,
     &               fCOut,
     &               iStats  )
      implicit none

#include "plant.h"
#include "CETC_definitions.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Common storing status of plant initialization.
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)

C.....Additional data for plant components
      common/pcdat/
     &     pcdatf(mpcom,mpcdat),
     &     pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)

C----------------------------------------------------------------------
C     Passed arguemets
C----------------------------------------------------------------------
      integer iPComp            ! pointer to component in net.
      integer iStats            ! index of matrix to be solved
      integer iType             ! flag indicating 'type' of model

      real fCOut(mpcoe)         ! array containing matrix
                                ! coefficients calculated by
                                ! the component.

      integer ii

C-----------------------------------------------------------------------
C     Initialize component inputs, if necessary
C-----------------------------------------------------------------------
      if ( .not. bInitialized ( iPComp ) ) then

C........Collect time-invariant data in ADATA, BDATA arrays
         call A42_CHP_Collect_Data_dyn ( iPComp, iType )

C........Initialize Stirling engine module for H3K reports
         call A42_CHP_H3Kreports_module (  iPComp,
     &                                     iInitialize )

C........Zero PCDATF array, which may be referenced by
C........routine A42_CHP_flow_char before it is initialized
         do ii = 1, mpcdat
           PCDatF ( iPComp, ii ) = 0.0
         enddo

C........Set bTS_modes array to .false., as it may be referenced by
C........routine A42_CHP_flow_char before it is initialized.
         do ii = 1, iOP_Mode_Count
            bReport_TS_modes(iPComp, ii ) = .false.
         enddo

C........Set flag to ensure initialization is not repeated
         bInitialized ( iPComp ) = .true.

      endif

C----------------------------------------------------------------------
C     Determine which matrix is being solved, and set coefficients
C     accordingly.
C----------------------------------------------------------------------
      if ( iStats == iProp1stFlow ) then

C----------------------------------------------------------------------
C        Plant first phase flow (ie air, water) matrix is to be
C        solved. Call subordinate routine A42_CHP_flow_char to
C        determine flow through CHP unit.
C----------------------------------------------------------------------
         call A42_CHP_flow_char ( iPComp, fCOut )

      elseif (iStats == iProp2ndFlow ) then
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
         fCOut(1) =  1.0
         fCOut(2) =  0.0
         fCOut(3) =  0.0
         fCOut(4) =  1.0
         fCOut(5) =  0.0
         fCOut(6) =  0.0
         fCOut(7) =  0.0


      elseif (iStats == iPropH2Flow ) then
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
         fCOut(1) =  1.0
         fCOut(2) =  0.0
         fCOut(3) =  0.0
         fCOut(4) =  1.0
         fCOut(5) =  0.0
         fCOut(6) =  0.0
         fCOut(7) =  0.0
C----------------------------------------------------------------------
C        Note: A future inprovement might permit the Stirling engine's
C        operating point to be determined dynamically using an
C        incomming hydrogen flow rate.
C----------------------------------------------------------------------

      elseif ( iStats == iPropTemp ) then
C----------------------------------------------------------------------
C        Temperature flow matrix is to be solved. Use subordinate
C        routine, A42_CHP_thermal_char() to determine temperature
C        matrix coefficients.
C----------------------------------------------------------------------
         call A42_CHP_thermal_dyn (iPComp, iType, fCOut)

      else

C........plt matrix not supported. Do nothing.

      endif

      return
      end  ! of subroutine

C------------------ A42_CHP_flow_char -----------------------------
C
C ABSTRACT:
C This routine characterizes the flow through the Annex 42
C
C Inputs:
C  iPComp       - index of component in plant network
C  iType        - 'type' of model in use --- can be
C                 A42 CHP model, or earlier SE model.
C
C Outputs
C  fCOut        - array containing components' matrix coefficients
C
C----------------------------------------------------------------------
      subroutine A42_CHP_flow_char ( iPComp, fCOut )
      implicit none
#include "plant.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
#include "CETC_definitions.h"
C----------------------------------------------------------------------
C     Passed arguemets
C----------------------------------------------------------------------
      integer iPComp            ! pointer to component in net.
      integer iType             ! flag indicating 'type' of model

      real fCOut(mpcoe)         ! array containing matrix
                                ! coefficients calculated by
                                ! the component.

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C.....I/O
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C.....Common containing plant componet mass diversion ratios
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),ipc2(mpcon),
     &            ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      integer npcon            ! number of inter-component connections
      integer ipc1             ! component number defining the receiving
                               !   component
      integer ipn1             ! node number defining the receiving component
      integer ipct             ! plant component inter-connection type
      integer ipc2             ! component number defining the sending
                               !   component
      integer ipn2             ! node number defining the sending component
      real    pcondr           ! ratio of mass flow rate through the connection
                               !   and the mass flow rate entering the receiving
                               !   node for each plant component inter-connection
      real    pconsd           ! supplementary data for plant component inter-
                               !   connection

C.....Plant component connection data
      common/pcond/convar, icontp, icondx
      real    convar(mpcon,mconvr)        ! state varibles for connections
      integer icontp(mpcon)               ! type of each connection
      integer icondx(mpcom,mnodec,mpconc) ! pointer to connections for each
                                          ! component/node

      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

C.....Additional data
      common/pcdat/
     &     pcdatf(mpcom,mpcdat),
     &     pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iCW_conn_index              ! Index of cooling water connection

      real fCW_temperature_in_F           ! temperature of incomming
      real fCW_temperature_in_P           !   cooling water (oC)

      real fCorr_CW_temp                  ! Cooling water inlet temp.
                                          !   used in correlation (oC)

      real fCW_Flow_F                     ! Cooling water flow rate
      real fCW_Flow_P                     ! imposed by CHP device (kg/s)
                                          ! (present & future time rows)

      integer iOperating_mode_F           ! System operating mode and
      integer iSystem_Status              ! status

      real fOP_System                     ! System steady-state operating
                                          !    point (W)

      real fP_net_ss                      ! System power production
                                          !    used in correlations (W)

      logical bApply_correlation          ! Flag indicating correlation
                                          !    must be applied.

      logical bClose_To_Zero              ! Result of close-to-zero
                                          !    comparison

      character*124 cContext   ! Buffers used to write messages to
      character*124 cMsg(6)    !   screen.

      logical bWarning                    ! Warning flags
      logical bWarned(MPCom)
      integer iLoc
      data ( bWarned (iLoc), iLoc = 1, MPCom ) / MPCom * .false. /

      bWarning = .false.

C----------------------------------------------------------------------
C     Get index of incomming water connection
C----------------------------------------------------------------------

C.....Cooling water connection index
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iCW_conn_index = icondx ( iPComp, 2, 1 )

C----------------------------------------------------------------------
C     Collect unit's present time row flow rate and cooling water
C     temperature
C----------------------------------------------------------------------
      FCW_Flow_P        = PCDATP( iPComp,  13 )
      fCW_temperature_in_P  = PCDATP( iPComp, 9  )           ! (oC)

C----------------------------------------------------------------------
C     Collect unit's future time row and steady-state power production
C     --- these values are calculated by A42_CHP_thermal_dyn prior
C     when iStats = 1
C----------------------------------------------------------------------

      iOperating_mode_F     = int ( PCDatF( iPComp,  1 ) )
      iSystem_Status        = int ( PCDatF( iPComp,  2 ) )

      fOP_system            = PCDatF( iPComp, 12 )           ! (W)


C----------------------------------------------------------------------
C     Get future time row connection temperature from ESP-r
C     state variables
C----------------------------------------------------------------------
      fCW_temperature_in_F = convar ( iCW_conn_index, iPropTemp )   ! oC


      if ( iCW_Loop_configuration(iPComp) == iExternal_pump ) then
C----------------------------------------------------------------------
C        Flow is determined by an external circulator --- what comes
C        in must go out.
C
C        Applying conservation of mass:
C
C           | 1.0    0.0   0.0 | | mo_1 |  = 0.0
C           | 0.0    1.0  -1.0 | | mo_2 |
C                                | mi_2 |
C
C        Where: mo is the flow through the engine, and
C               mi is the flow entering the engine.
C
C----------------------------------------------------------------------
         fCOut(1) =  1.0
         fCOut(2) =  0.0
         fCOut(3) =  0.0
         fCOut(4) =  1.0
         fCOut(5) = -1.0 * pcondr ( iCW_conn_index )
         fCOut(6) =  0.0
         fCOut(7) =  0.0

      elseif ( iCW_Loop_configuration(iPComp) == iInternal_pump ) then
C----------------------------------------------------------------------
C       CHP unit imposes flow rate on cooling loop
C----------------------------------------------------------------------
        if ( bReport_TS_modes (iPComp, iOP_startup) .or.
     &       bReport_TS_modes (iPComp, iOP_warmup) .or.
     &       iSystem_Status == iStatus_CW_fault          ) then
C----------------------------------------------------------------------
C         The system is attempting to startup and/or but there's
C         no cooling water. Use init's maximum power when evaluating
C         cooling water correlation.
C----------------------------------------------------------------------
          fP_net_ss = fPower_Out_MAX ( iPComp )  ! (W)

          bApply_correlation = .true.

        elseif ( bReport_TS_modes (iPComp, iOP_normal_operation) .or.
     &           bReport_TS_modes (iPComp, iOP_warmup) ) then

C----------------------------------------------------------------------
C         In warm-up or normal operation, use unit's operating point
C         to determine flow rate
C----------------------------------------------------------------------
          fP_net_ss = fOP_System         ! (W)

          bApply_correlation = .true.

        elseif ( bReport_TS_modes (iPComp, iOP_shutdown) ) then
C----------------------------------------------------------------------
C         During shutdown, apply whatever flow rate was in affect
C         on last timestep
C----------------------------------------------------------------------
          fCW_Flow_F = fCW_Flow_P          ! (kg/s)

          bApply_correlation = .false.

        elseif ( bReport_TS_modes ( iPComp, iOP_inoperative ) ) then
C----------------------------------------------------------------------
C         Units is inoperative --- stop cooling water flow
C----------------------------------------------------------------------
          fCW_Flow_F = 0.0                ! (kg/s)

          bApply_correlation = .false.

        else
C.......Can't happen --- all possibilities enumerated above!
        endif


        if ( bApply_correlation ) then
C----------------------------------------------------------------------
C         The coolant flow  must be calculated using the cooling water
C         flow rate correlation --- see Equation 16 in Ferguson and
C         Kelly (2006)
C
C         Flow rate = c0 + c1 * (Steady-state power)^2
C
C                        + c2 * (Steady-state power)
C
C                        + c3 * (Cooling water temperature)^2
C
C                        + c4 * (Cooling water temperature)
C
C                        + c5 * (Steady-state power)^2
C                             * (Cooling water temperature)^2
C
C                        + c6 * (Steady-state power)
C                             * (Cooling water temperature)
C
C                        + c7 * (Steady-state power)
C                             * (Cooling water temperature) ^2
C
C                        + c8 * (Steady-state power) ^2
C                             * (Cooling water temperature)      (kg/s)
C
C----------------------------------------------------------------------
C.........Use future row inlet temperature for correlations
          fCorr_CW_temp = fCW_temperature_in_F ! (oC)

C.........Apply correlation
          fCW_Flow_F =   fPM_flow_coeff(iPComp, 1)
     &
     &                 + fPM_flow_coeff(iPComp, 2)
     &                    * fP_net_ss ** 2.0
     &
     &                 + fPM_flow_coeff(iPComp, 3)
     &                    * fP_net_ss
     &
     &                 + fPM_flow_coeff(iPComp, 4)
     &                    * fCorr_CW_temp ** 2.0
     &
     &                 + fPM_flow_coeff(iPComp, 5)
     &                    * fCorr_CW_temp
     &
     &                 + fPM_flow_coeff(iPComp, 6)
     &                    * fP_net_ss ** 2.0
     &                    * fCorr_CW_temp ** 2.0
     &
     &                 + fPM_flow_coeff(iPComp, 7)
     &                    * fP_net_ss
     &                    * fCorr_CW_temp
     &
     &                 + fPM_flow_coeff(iPComp, 8)
     &                    * fP_net_ss
     &                    * fCorr_CW_temp ** 2.0
     &
     &                 + fPM_flow_coeff(iPComp, 9)
     &                    * fP_net_ss ** 2.0
     &                    * fCorr_CW_temp               ! (kg/s)

C..........Make sure correlation flow is not negative!

           call eclose ( fCW_Flow_F, 0.0, fClose_to_zero_tolerance,
     &                                    bClose_To_Zero )

           if ( fCW_Flow_F .lt. 0.0      .and.
     &            .not. bClose_To_Zero .and.
     &            .not. bWarned(iPComp)      ) then

C.............Warn user
              bWarning = .true.

              write (cMsg(1),'(A)')
     &          'Calculated cooling water flow rate is less than zero!'
              write (cMsg(2),'(A)')
     &          'Cooling water flow rate value reset to zero.'
              write (cMsg(3),'(A)')
     &          'Cooling water flow correlation inputs:'

              write (cMsg(4),'(A,g12.6)')
     &          '   -Power input (W):        ', fP_net_ss

              write (cMsg(5),'(A,g12.6)')
     &          '   -Cooling water temp (oC):', fCW_temperature_in_F

              write (cMsg(6),'(A,g12.6)')
     &          'Resulting flow rate (kg/s) :', fCW_Flow_F

              bWarned(iPComp) = .true.

           endif
C..........Reset negative flow to zero
           if ( fCW_Flow_F .lt. 0.0 ) then
             fCW_Flow_F = 0.0
           endif

        else
C----------------------------------------------------------------------
C         The cooling water flow rate is already assigned, and there's
C         no need to apply the correlation.
C----------------------------------------------------------------------
        endif

C----------------------------------------------------------------------
C       Assign flow matrix coefficients. Set flow through second node
C       to calculated value
C
C          | 1.0    0.0   0.0 | | mo_1 |  = |    0.0     |
C          | 0.0    1.0   0.0 | | mo_2 |    | fCW_Flow_F |
C                               | mi_2 |
C
C       Where: mo is the flow through the engine, and
C              mi is the flow entering the engine.
C
C---------------------------------------------------------------------
        fCOut(1) =  1.0
        fCOut(2) =  0.0
        fCOut(3) =  0.0
        fCOut(4) =  1.0
        fCOut(5) =  0.0
        fCOut(6) =  0.0
        fCOut(7) =  fCW_Flow_F


      endif   ! <- matches 'if ( iCW_Loop_configuration(iPComp) == ...'

C---------------------------------------------------------------------
C     Save cooling water flow rate for use on next time step
C---------------------------------------------------------------------
      PCDatF ( iPComp, 13 ) = fCW_Flow_F                 ! (kg/s)

C.....Issue warning, if necessary.
      if ( bWarning ) then

        write (cContext,'(A,A,I2,A)') 'Annex 42 combustion CHP model',
     &      ' (Component number ', iPComp, '):'

        call edisp( iUOut, cContext )
        call edisp( iUOut, cMsg(1) )
        call edisp( iUOut, cMsg(2) )
        call edisp( iUOut, cMsg(3) )
        call edisp( iUOut, cMsg(4) )
        call edisp( iUOut, cMsg(5) )
        call edisp( iUOut, cMsg(6) )

      endif

      return
      end

C------------------ A42_CHP_thermal_dyn ----------------------------
C
C ABSTRACT:
C This routine characterizes the electrical and performance of a
C stirling-cycle based cogeneration system, and calculates
C matrix coefficients for the plant temperature solution
C matrix ( ISTATS = 1 )
C
C
C Inputs:
C  iPComp       - index of component in plant network
C  iType        - 'type' of model in use --- can be
C                 A42 CHP model, or earlier SE model.
C
C Outputs
C  fCOut        - array containing components' matrix coefficients
C
C----------------------------------------------------------------------
      subroutine A42_CHP_Thermal_dyn ( iPComp, iType, fCOut )
      implicit none

#include "building.h"
#include "plant.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
#include "chemical_properties.h"
#include "CETC_definitions.h"
C----------------------------------------------------------------------
C     Passed arguemets
C----------------------------------------------------------------------
      integer iPComp        ! pointer to component in net.
      integer iType         ! 'type' of model called.
      real fCOut(mpcoe)     ! array containing matrix
                            ! coefficients calculated by
                            ! the component.
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace/error reporting unit numbers
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
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

C.....Plant interation data
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

C.....Common holding additional outputs for plant components
C.....(not used at present)
      common/pcres/
     &     qdata(mpcom),
     &     pcaout(mpcom,mpcres),
     &     napdat(mpcom)
      real qdata     ! not used in current context
      real pcaout    ! array containing additional plant outputs
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

C.....Common containing plant componet mass diversion ratios
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),ipc2(mpcon),
     &            ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      integer npcon            ! number of inter-component connections
      integer ipc1             ! component number defining the receiving
                               !   component
      integer ipn1             ! node number defining the receiving component
      integer ipct             ! plant component inter-connection type
      integer ipc2             ! component number defining the sending
                               !   component
      integer ipn2             ! node number defining the sending component
      real    pcondr           ! ratio of mass flow rate through the connection
                               !   and the mass flow rate entering the receiving
                               !   node for each plant component inter-connection
      real    pconsd           ! supplementary data for plant component inter-
                               !   connection

C.....Miscellaneous plant data
      common/pcvar/pctf,pcrf,puaf,pcqf,pcntmf,pctp,
     &      pcrp,puap,pcqp,pcntmp
      real pctf(mpcon)
      real pcrf(mpcon)
      real puaf(mpnode)
      real pcqf(mpnode)
      real pcntmf(mpcom)          ! future time row plant containment temp (oC)
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

C Common for additional iteration criteria. These data structures permit bps
C to iterate until the data in the pcdatf array has converged.
      common / pAdd_iter_criteria /
     &   iPlt_Output_Iter_Flag,
     &   fPlt_Output_Init_Val,
     &   fPlt_Output_Tol
      integer iPlt_Output_Iter_Flag (mpcom,mpcdat) ! flag denoting iteration is req'd
                                                   ! for plt additional outputs
      real fPlt_Output_Init_Val ( mpcom,mpcdat)    ! initial value of plt additional output
      real fPlt_Output_Tol (mpcom,mpcdat)          ! tolerance for convergence


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

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iEngine_Node_index ! Index of component engine node in plant
                                 !   Network.
      integer iHX_Node_index     ! Index of component HX node in plant
                                 !   Network.

      integer iCW_conn_index     ! Cooling water connection index

C.....Flag indicating status of unit's warmup cycle.
      logical bWarmup_complete ( MPCom )
      save bWarmup_complete

C.....Key model parameters
      real fN_fuel_flow          ! Fuel flow (kg/s)
      real fN_fuel_flow_startup  ! Fuel flow in start-up (kg/s)

      real fRate_Limit           ! Restricted rate of change in fuel flow
                                 ! (kg/s^2) or power output (W/s)

      real fTime_Constant        ! Effective thermal time constant (s)
      real fTC_eng_node          ! Individual time constants for both 
      real fTC_cw_node           !   engine and cooling water nodes (s)
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

      real fEngine_Temp_P        ! Temperature of stirling engine control
                                 !   volume on present time row (oC)
      real fEngine_Temp_F        ! Temperature of stirling engine control
                                 !   volume on future time row (oC)

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

      real fN_air_flow                 ! air flow rate.

C.....Final conditions resulting from transient behavior during
C.....normal operation
      real fN_fuel_flow_trans_final    ! Final Fuel flow (kg/s)
      real fP_net_trans_final          ! Final power output (W)
      real fQ_Cogen_trans_final        ! Final SS heat gen. (W)

C.....Maximum change that can be achieved in fuel flow
      real fN_fuel_flow_Trans_limit    ! Rate-limited fuel flow (kg/s)
      real fRate_limited_value         ! rate limit reported by
                                       !   Eval_TS_average

      logical bLimitSet                ! Flag indicating rate limit
                                       !   has been established.

C.....Boundary conditions / cooling water present time-row conditions
      real fCW_mass_flow_P       ! Cooling water flow (kg/s)
      real fCW_temperature_in_P  ! Cooling water temp at inlet (oC)
      real fCW_temperature_out_P ! Cooling water temperature at outlet (oC)
      real fCW_heat_cap_P        ! Cooling water heat capacity (W/K)

C.....Cooling water present time-row conditions
      real fCW_mass_flow_F       ! Cooling water flow (kg/s)
      real fCW_temperature_in_F  ! Cooling water temp at inlet (oC)
      real fCW_heat_cap_F        ! Cooling water heat capacity (W/K)

C.....Startup electrial and thermal efficiency,
      real fStartUP_eff_ther     ! Thermal efficiency during
                                 !   startup

C.....Tolerance
      real fVar_Tolerance                 ! Variable tolerance for close-to-zero
                                          !    comparisons. Note: fixed value
                                          !    fClose_to_zero_tolerance is also
                                          !    defined in Annex42_combustion_CHP.F



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
                                 !   should be applied, future time row
      real fControl_signal       ! Input from control interface, future time row
                                 !  (Watts or dimensionless)

      real fOP_target_P,         ! Target operating point described by
     &     fOP_target_F          !   the control signal. May describe a
                                 !   fuel flow rate (kg/s) , power output
                                 !   (W), or rate of thermal output (W)

      real fOP_system            ! Actual operating point chosen by low-level
                                 !   controller. May differ from fOP_target_F
                                 !   in Annex 42 CHP model during 'warmup'
                                 !   period (W).




      logical bControl_Active    ! Flag indicating that controller is requesting
                                 !   engine turn on.

C.....Electrical-network related variables
      integer iEModel            ! flag indicating which model should be used
      real PQ, PA                ! Dummy arguements

C.....Counters
      integer iMode

C.....Misc parameters / flags
      logical bNums_are_close    ! flag for close to zero comparisons
      real fTS_duration          ! Timestep duration (seconds)

      logical bControl_Warned    ! Flag indicating if warning
                                 !   has been issued about
                                 !   inappropriate control.

      data bControl_Warned  /.false./
      save bControl_Warned

      logical bFuel_dNdT_within_limits ! Flag indicating solution
                                       !   of Annex 42 CHP model fuel flow
                                       !   has converged

      logical bFuel_Rate_Limited       ! Flag indicating rate of change
                                       !   in fuel flow is rate-limited.

      integer iStrategy                ! Flag indicating how function 
                                       !   Eval_TS_average should treat
                                       !   variables when the maximum
                                       !   specified rate of change is not 
                                       !   exceeded.

      logical bLimited                 ! Flag indicating output of function
                                       !   Eval_TS_average is rate-limited.

      logical bPowerLimited            ! Flag indicating if rate of change
                                       !   in power output is rate-limited.

      logical bCorrelation_error       ! Flag indicating an error was
                                       !    encountered while evaluating
                                       !    correlations.

      real fP_net_LB, fP_net_UB  ! Bisection bounds for Annex 42 CHP model
                                 !   fuel flow iteration loop.

C.....Efficiencies returned by Eval_Annex42_CHP_Correlations
      real fElec_Efficiency      ! Electrical efficiency (-)
      real fTher_Efficiency      ! Heat generation efficiency (-)

C.....Dummy variables --- req'd because not all of the outputs
C.....passed from Solve_Cogen_Model() are used in this implementation
      real fDummy_time, fDummy_fuel, fDummy_heat
      real fDummy_Power1, fDummy_Power2, fDummy_Qloss, fDummy_Qavail


C.....bDebug: manually set to true to enable debugging.
      logical bDebug
      parameter (bDebug = .false.)

C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real shtfld                 ! ESRU function returning the
                                  !   specific heat of air, water vapor,
                                  !   or water.

C----------------------------------------------------------------------
C     Initialization
C----------------------------------------------------------------------

      if (bDebug) write(ITU,*)
     &   "Entering subroutine A42_CHP_thermal_char"


C.....Get timestep duration (seconds)
      fTS_duration = 3600. / ( ntstep * ntstpp )

C------------------------------------------------------------------------
C     Is this the first timestep of the simulation?
C------------------------------------------------------------------------
      if ( nsinc == 1 ) then
C------------------------------------------------------------------------
C        PCDatP array has not been populated. Set initial starting mode
C        to 'inoperative'
C------------------------------------------------------------------------
         iOperating_mode_P = iOP_inoperative             ! (-)
         iSystem_Status    = iStatus_normal              ! (-)
         fOP_time = 0.0                                  ! (s)

C........Mass & Energy flow initial conditions for transient behavior
         fP_net_init          = 0.0                      ! (W)
         fN_fuel_flow_init    = 0.0                      ! (kg/s)

C........If this is the first timestep, set present time row
C........cooling water inlet temperature and mass flow rate to
C........20oC and 0kg/s (ESP-r defaults). Otherwise, use data
C........from last timestep.
         fCW_temperature_in_P = 20.0  ! (oC)
         fCW_mass_flow_P      = 0.0   ! (kg/s)

      else
C------------------------------------------------------------------------
C        Collect historical operating mode data from PCDatP arrays:
C------------------------------------------------------------------------
         iOperating_mode_P    = int(PCDatP(iPComp, 1))   ! (-)
         iSystem_Status       = int(PCDatP(iPComp, 2))   ! (-)
         fOP_time             = PCDATP(iPComp, 3)        ! (s)

C........Mass & Energy flow initial conditions for transient behavior
         fN_fuel_flow_init    = PCDATP(iPComp, 4)        ! (kg/s)
         fP_net_init          = PCDATP(iPComp, 5)        ! (W)
         fQ_cogen_init        = PCDATP(iPComp, 6)        ! (W)
         fQ_cogen_P           = PCDATP(iPComp, 7)        ! (W)

C........Connection temperature & flow data
         fCW_temperature_in_P = PCDATP(iPComp, 9)        ! (oC)
         fCW_mass_flow_P      = PCDATP(iPComp,10)        ! (kg/s)

C........Control data
         fOP_target_P         = PCDATP(iPComp,11)        ! (-)

      endif

C------------------------------------------------------------------------
C     If unit starts timestep in standby, initialize warmup completion
C     flag to false.
C------------------------------------------------------------------------
      if ( iOperating_mode_P == iOP_inoperative )
     &       bWarmup_complete ( iPComp ) = .false.

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

C.....Node indicies

      iEngine_Node_index = NPCDAT ( iPComp, 9 )
      iHX_Node_index = iEngine_Node_index + 1

C.....Engine thermal mass node must be marked for iteration
      icsv( iEngine_Node_index, 1 ) = 1
C.....Update initial value
C.....(Why isn't this done for all components elsewhere in ESP-r?)
      csvi( iEngine_Node_index, 1 ) = csvf( iEngine_Node_index, 1 )


C.....Mark cooling water node for iteration (temperature and flow)
      icsv( iHX_Node_index, 1 ) = 1
      icsv( iHX_Node_index, 2 ) = 1
C.....Update initial value
      csvi( iHX_Node_index, 1 ) = csvf( iHX_Node_index, 1 )
      csvi( iHX_Node_index, 2 ) = csvf( iHX_Node_index, 2 )

C.....Mark iteration on additional plant component data array:
C.....  -> Mark the unit's operating 'mode' and 'status' for
C.....    iteration, ensuring the the unit has not switched
C.....    between modes on the last timestep.
      iPlt_Output_Iter_Flag( iPComp, 1 ) = 1        ! operating mode
      iPlt_Output_Iter_Flag( iPComp, 2 ) = 1        ! system status
      fPlt_Output_Init_Val( iPComp, 1 ) = PCDatF(iPcomp, 1)
      fPlt_Output_Init_Val( iPComp, 2 ) = PCDatF(iPcomp, 2)
      fPlt_Output_Tol( iPComp, 1 ) = 0.1
      fPlt_Output_Tol( iPComp, 2 ) = 0.1

C.....Cooling water connection index
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iCW_conn_index = icondx ( iPComp, 2, 1 )

C----------------------------------------------------------------------
C     Get present timerow temperature and flow data from ESP-r
C     state variables
C----------------------------------------------------------------------

C.....Get cooling water temperature & mass flow rate (future row)
      fCW_temperature_in_F = convar ( iCW_conn_index, iPropTemp )   ! oC
      fCW_mass_flow_F =   pcondr( iCW_conn_index ) 
     &                  * convar ( iCW_conn_index, iProp1stFlow )   ! kg/s

C.....Get temperature of cooling water at outlet of engine heat
C.....exchangers. Note, this will be the temperature calculated
C.....during the last time step. This data is used by the unit's
C.....internal 'overheat protection' control.
      fCW_temperature_out_P = CSVP( iHX_Node_index, iPropTemp )   ! oC

C.....Get temperature of engine node at end of last timestep
      fEngine_Temp_P = CSVP( iEngine_Node_index, iPropTemp )      ! oC
      fEngine_Temp_F = CSVF( iEngine_Node_index, iPropTemp )      ! oC
C.....Collect containing room temperature
      fTemp_Room_F = PCnTmF ( iPComp )  ! oC (future time row)
      fTemp_Room_P = PCnTmP ( iPComp )  ! oC (present time row)

C----------------------------------------------------------------------
C     Set flags for low-level controls - overheat protection
C----------------------------------------------------------------------

C.....Check to see if cooling water flow is non-zero (use
C.....future time row)
      call eclose ( fCW_mass_flow_F, 0.0,
     &              fClose_to_zero_tolerance , bNo_cooling_water )

C.....Is cooling water exhaust temp. less than max safe temp?
C.....( Check present time row value --- use of present time
C.....  row value prevents low-level controller from
C.....  toggling overheat protection on and off between
C.....  iterations.
      if ( fCW_temperature_out_P .ge.
     &     fCW_temperature_MAX(iPComp) ) then

         bCooling_W_ToHot = .true.

      else

         bCooling_W_ToHot = .false.

      endif

C----------------------------------------------------------------------
C     Calculate cooling water heat capacity flow rate:
C       specific heat capactiy = Shtfld ( X , Temperature ) (J/kg oC)
C       where X=1 -> air, X=2 -> vapor, X=3 -> liq. water
C----------------------------------------------------------------------
      fCW_heat_cap_F = fCW_mass_flow_F
     &                  * SHtFld ( 3, fCW_temperature_in_F ) ! W/K
      fCW_heat_cap_P = fCW_mass_flow_P
     &                  * SHtFld ( 3, fCW_temperature_in_P ) ! W/K


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
      iControl_method = int ( CData ( iPComp, 1 ) )
C..... -> Control signal
      fControl_signal = CData ( iPComp, 2 )

      if ( iControl_method == iEngine_deactivated ) then
C........Controller has deactivated the engine.
         bControl_Active = .false.

      elseif ( iControl_method == iDimensionless_control ) then
C........If dimensionless control interface is active
         bControl_Active = .true.

C........Control siganl must lie between 0 and 1.
         if ( fControl_signal > 1.0 ) fControl_signal = 1.0
         if ( fControl_signal < 0.0 ) fControl_signal = 0.0

C........Map control signal to power output
         fOP_target_F = fPower_Out_MIN (iPComp ) +
     &      fControl_signal * (
     &         fPower_Out_MAX (iPComp)
     &       - fPower_Out_MIN (iPComp)
     &      )


      elseif ( iControl_method == iPower_out_controlled ) then
C........Power out control is active.
         bControl_Active = .true.
C........Check that control signal is not less than zero.
         if ( fControl_signal < 0 ) fControl_signal = 0.0

C........Assign control signal directly to target operating point
         fOP_target_F = fControl_signal  ! Watts, electric or thermal.

      elseif ( iControl_method == iHeat_out_controlled ) then

C........Heat out control is not supported in final A42 model.
         bControl_Active = .false.

         if ( .not. bControl_Warned ) then

           write (ITU,*) "Warning: Annex 42 Combustion CHP model "
           write (ITU,'(A,I2,A)')
     &                    "         (Component #", iPComp, ")"
           write (ITU,*) " "
           write (ITU,*) "         Thermal load following control "
           write (ITU,'(A,I2,A,I1,A)')
     &                    "         scheme [CDATA(",iPComp,",1)=",
     &                    iHeat_out_controlled,"] is not "
           write (ITU,*) "presently supported. "
     &                        //  "Deactivating unit."
           write (ITU,*) " "

           bControl_Warned = .true.

         endif

      else

C........Controller has requested an unsupported control mode. Deactivate
C........unit and issue warning. (Note: bControl_Warned is overloaded
C........here --- it should really be split into two variables.)
         bControl_Active = .false.

         if ( .not. bControl_Warned ) then

            write (ITU,*) "Warning: Annex 42 Combustion CHP model "
            write (ITU,'(A,I2,A)')
     &                     "         (Component #", iPComp, ")"
            write (ITU,*) " "
            write (ITU,*) "         Unsupported control scheme:"
            write (ITU,'(A,I2,A,I1,A)')
     &                     "          [CDATA(",iPComp,",1)=",
     &                     CDATA(iPcomp,1),"]"
            write (ITU,*) "         Deactivating unit."
            write (ITU,*) " "
            bControl_Warned = .true.

          endif

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

C----------------------------------------------------------------
C     Check if cooling water fault exists, and clear if found.
C     ( fault will be reset if cooling water is needed! )
C----------------------------------------------------------------
      if ( iSystem_Status == iStatus_CW_fault )  then

            iSystem_Status = iStatus_normal

      endif

C-------------------------------------------------------------------
C     Check to see if power has been requested.
C-------------------------------------------------------------------
      if ( bControl_Active ) then
C........Power requested. - unit will operate.
C-------------------------------------------------------------------
C       Activate unit in normal operation.
C       ICE unit does not have to complete warmup---
C       therefore, set warmup-complete tag to true.
C       (It will, however, complete a 'start-up'.
C-------------------------------------------------------------------
        if ( iType .eq. iICE_model ) then

           iTarget_mode = iOP_normal_operation
           bWarmup_complete (iPComp) = .true.

C-------------------------------------------------------------------
C        Stirling engine model: Check to see if warm up is complete.
C-------------------------------------------------------------------
        else
C.........Check to see if warmup is complete. Has engine temperature
C.........exceeded specified nominal value, or power output exceeded
C.........demand?
          if ( (fEngine_Temp_P .ge. fNominal_engine_temp(iPComp) .OR.
     &          fP_net_init .ge. fOP_target_F ) .AND.
     &         .not. bWarmup_complete (iPComp) ) then
C.............yes!
            bWarmup_complete ( iPComp ) = .true.
          endif
C.........If warm up is complete, set mode to normal operation.
C.........Otherwise, set to warmup.

          if ( bWarmup_complete ( iPComp ) ) then
            iTarget_mode = iOP_normal_operation
          else
            iTarget_Mode = iOP_warmup
          endif

        endif

      else
C........No power requested. Turn unit off.
         iTarget_mode = iOP_inoperative
      endif




C----------------------------------------------------------------------
C     If the correlations were evaluated incorrectly on the last
C     iteration and unit has halted, clear status flag.
C----------------------------------------------------------------------
      if ( iSystem_Status == iStatus_Corr_Err .and.
     &     iOperating_mode_P == iOP_inoperative ) then

           iSystem_Status = iStatus_normal

      endif
C----------------------------------------------------------------------
C     Use function Determine_Cogen_OP to determine the fraction of
C     the current timestep the system will spend in each operating
C     mode as it moves from its current to target operating points.
C----------------------------------------------------------------------
      call Determine_Cogen_Mode(
     &              iPComp,
     &              iOperating_mode_P,
     &              iSystem_Status,
     &              fOP_time,
     &              iTarget_Mode,
     &              iCooldown_mode(iPComp),
     &              iOperating_mode_F,
     &              fTS_mode_fractions,
     &              bTS_modes,
     &              bStartup_complete,
     &              bDebug              )

C----------------------------------------------------------------------
C     If unit is operating in warm-up, start-up, normal operation,
C     and/or shutdown modes, check if cooling water is available:
C----------------------------------------------------------------------

      if (  ( bTS_modes( iOP_startup          ) .or.
     &        bTS_modes( iOP_warmup           ) .or.
     &        bTS_modes( iOP_normal_operation ) .or.
     &        bTS_modes( iOP_shutdown         )     ) .and.
     &      ( bNo_cooling_water .or. bCooling_W_ToHot )      ) then

C----------------------------------------------------------------------
C        Overheating danger exists. Set cooling water fault flag.
C----------------------------------------------------------------------
         iSystem_Status = iStatus_CW_fault


C-------------------------------------------------------------------------
C        Use function Determine_Cogen_OP to re-determine the fraction of
C        the current timestep the system will spend in each operating
C        point, given that a cooling water fault has occured.
C-------------------------------------------------------------------------

         call Determine_Cogen_Mode(
     &                 iPComp,
     &                 iOperating_mode_P,
     &                 iSystem_Status,
     &                 fOP_time,
     &                 iTarget_Mode,
     &                 iCooldown_mode(iPComp),
     &                 iOperating_mode_F,
     &                 fTS_mode_fractions,
     &                 bTS_modes,
     &                 bStartup_complete,
     &                 bDebug              )

      endif

C----------------------------------------------------------------------
C     Initialize fuel and energy flows for normal operation and
C     warm-up phases to zero
C----------------------------------------------------------------------
      fPeriod_AC_Power( iPComp, iOP_normal_operation ) = 0.0
      fPeriod_Heat_Recovery( iPComp, iOP_normal_operation ) = 0.0
      fPeriod_fuel_flow( iPComp, iOP_normal_operation ) = 0.0

      fPeriod_AC_Power( iPComp, iOP_warmup ) = 0.0
      fPeriod_Heat_Recovery( iPComp, iOP_warmup ) = 0.0
      fPeriod_fuel_flow( iPComp, iOP_warmup ) = 0.0


C----------------------------------------------------------------------
C     Check to see if the unit will operate in normal operation or
C     warmup modes, and characterize behavior using empirically-
C     derived correlations if necessary.
C----------------------------------------------------------------------
      if ( bTS_modes (iOP_normal_operation) .or.
     &     bTS_modes (iOP_warmup)           .or.
     &     bTS_modes (iOP_startup) ) then
C---------------------------------------------------------------------
C       Engine is operating during this timestep. Characterize
C       performance.
C
C       Check which control interface is currently being used, and
C       apply the appropriate explicit/implicit solution scheme as
C       required:

C       Annex 42 model:
C         - Power output specificed   -> solve model directly
C         - Heat output specified     -> not supported
C         - Dimensionless control     -> Mapped to power output,
C                                          solve model directly
C
C       Check that desired operating point is within minimum &
C       maximum bounds.
C-------------------------------------------------------------------
        if ( fOP_target_F > fPower_Out_MAX ( iPComp ) )
     &       fOP_target_F = fPower_Out_MAX ( iPComp )

        if ( fOP_target_F < fPower_Out_MIN ( iPComp ) )
     &       fOP_target_F = fPower_Out_MIN ( iPComp )

C.......Make sure power rate of power change respects 
C.......controller limit 
        call Eval_TS_average (
     &           fP_net_init,
     &           fOP_target_F,
     &           fGross_Power_Change_MAX(iPComp),
     &           fTS_duration
     &               * fTS_mode_fractions (iOperating_mode_F),
     &           iStepwise,
     &           fDummy_Power1,
     &           fDummy_Power2,
     &           fRate_limited_value, bPowerLimited   )

C.......If requested change in OP will exceed rate-limited
C.......value, reset OP to respect rate-limit.
        if ( bPowerLimited ) then 
          fOP_target_F = fRate_limited_value
        endif 


C-----------------------------------------------------------------
C       For stirling engine: if warm-up period is not completed,
C       set operating point to maximimum power output. Note: The
C       ICE unit does not have to complete a warmup, and
C       bWarmup_complete will always be set to 'true' when the
C       ICE unit configuration is in use.
C-----------------------------------------------------------------
        if ( .not. bWarmup_complete(iPComp ) ) then
          fOP_system = fPower_Out_MAX (iPComp)
        else
          fOP_system = fOP_target_F
        endif

C-------------------------------------------------------------------
C       Solution of the Annex 42 model may require iteration:
C        1. The controller specifies the power demand
C        2. The model solves the performance map correlations to
C           determine the operating point, and hence, the fuel flow
C           at the desired operating point.
C        3. The rate of change in the fuel flow must be checked
C           against the maximum permissible value. If this limit
C           is exceeded, a new fuel flow should be chosen, and
C           items 2 & 3 repeated.
C-------------------------------------------------------------------

C.......Set flag for iteration convergence
        bFuel_dNdT_within_limits  = .false.

C.......Flag indicating fuel flow is rate limited.
        bFuel_Rate_Limited = .false.

C.......Flag indicating rate limit has been established.
        bLimitSet = .false.

C.......Initialize bounds for bisection root searches.
        fP_net_UB = fPower_Out_MAX ( iPComp )
        fP_net_LB = fPower_Out_MIN ( iPcomp )

C-------------------------------------------------------------------
C       Loop until operating point is found
C-------------------------------------------------------------------
        bCorrelation_error = .false.
        do while ( .not. bFuel_dNdT_within_limits .and.
     &             .not. bCorrelation_error )


C-------------------------------------------------------------------
C         Use function Eval_Annex42_CHP_Correlations to characterize
C         unit at desired operating point (Power output = fOP_target_F)
C-------------------------------------------------------------------
          call Eval_Annex42_CHP_Correlations (
     &                iPComp,
     &                fOP_system,
     &                fCW_mass_flow_F,
     &                fCW_temperature_in_F,
     &                fN_fuel_flow,
     &                fQ_cogen,
     &                fElec_Efficiency,
     &                fTher_Efficiency,
     &                bCorrelation_error    )


C-------------------------------------------------------------------
C         Is warm-up complete?
C
C          - if so, set fuel flow rate limit to user supplied value
C          - if not, scale fuel flow rate using engine temperature
C            correlation, and set fuel flow rate limit to -1
C            to disable rate limiting.
C-------------------------------------------------------------------
          if ( bWarmup_complete( iPComp ) ) then

            fRate_Limit = fFuel_Flow_Change_MAX ( iPComp ) ! (kg/s^2)

          else

C-------------------------------------------------------------------
C           Scale fuel flow to reflect dependance on temperature
C           during start-up period:
C
C           Fuel Flow = nominal fuel flow +
C
C                 scalar * ( nominal fuel flow )
C
C                 * ( T nominal - T room ) / ( T engine - T room )
C
C-------------------------------------------------------------------

C...........Trap potential divide by zero:
            call eclose ( fNominal_engine_temp ( iPComp ),
     &                    fTemp_Room_F,
     &                    fClose_to_zero_tolerance,
     &                    bNums_Are_Close )

C...........If denominator is not zero, apply correlation
            if ( .not. bNums_Are_Close .and.
     &                 fEngine_Temp_F > fTemp_Room_F ) then

              fN_fuel_flow_startup =
     &             fN_fuel_flow
     &           + fFuel_Temp_sens ( iPComp )
     &           * fN_fuel_flow
     &           * ( fNominal_engine_temp ( iPComp) - fTemp_Room_F )
     &           / ( fEngine_Temp_F - fTemp_Room_F )

C.............Check that correlation hasn't produced a fuel flow
C.............exceeding maximum permissible value. Fuel flow must be
C.............less than the the value at steady state multiplied by
C.............the startup-to-steady-state ratio. See Equation 24 in
C.............Ferguson and Kelly (2006)
C.............
C.............  ( Fuel flow at startup )
C.............
C.............         <= (Fuel flow at steady-state)
C.............
C.............                  * ( startup-to-steady-state ratio )
C.............
              if ( fN_fuel_flow_startup .gt.
     &             fFuel_Flow_MAX ( iPComp ) * fN_fuel_flow ) then

C...............Fuel flow is limited by specified ratio
                fN_fuel_flow = fFuel_Flow_MAX ( iPComp ) * fN_fuel_flow

              else

C...............Fuel flow is not limited by specified ratio.
                fN_fuel_flow = fN_fuel_flow_startup

              endif

            else
C..............If denominator is zero and correlation cannot be applied,
C..............set fuel flow to maximum permissible value.

               fN_fuel_flow_startup = fFuel_Flow_MAX ( iPComp )
     &               * fN_Fuel_Flow

            endif ! <- matches 'if ( .not. bNums_Are_Close .and. ...'

C...........Disable rate limiting.
            fRate_Limit = -1.0

C-------------------------------------------------------------------
C           Recalculate heat generation using adjusted
C           fuel flow.
C
C             Q cogen = ( fuel flow ) * ( fuel LHV ) *
C                         ( thermal efficiency )
C-------------------------------------------------------------------
            fQ_cogen = fN_Fuel_Flow * fFuel_LHV ( iPComp )
     &                  * fTher_Efficiency


          endif  ! <- matches 'if ( bWarmup_complete( iPComp ) ...'

C-------------------------------------------------------------------
C         Use function Eval_TS_average to determine rate-limited
C         final fuel flow rate based on present timerow fuel flow
C         rate and maximum rate of change. Eval_TS_average will
C         also return the fuel flow at the future timerow, and
C         the average flow over the timestep.
C
C         If the bPowerLimited flag is set, the iLinear strategy 
C         is set to ensure the resulting fuel flow varies linearly 
C         from timestep to time step. Otherwise, use iStepwise
C         strategy, which assumes the fuel flow rate will 
C         vary as fast as permitted by the fuel flow rate limit. 
C-------------------------------------------------------------------
          if ( bPowerLimited ) then
            iStrategy = iLinear
          else
            iStrategy = iStepwise
          endif 

          call Eval_TS_average (
     &        fN_fuel_flow_init,
     &        fN_fuel_flow,
     &        fRate_Limit,
     &        fTS_duration
     &             * fTS_mode_fractions( iOperating_mode_F ),
     &        iStrategy,
     &        fN_fuel_flow_Trans_average,
     &        fN_fuel_flow_Trans_final,
     &        fRate_limited_value, bLimited )

C-------------------------------------------------------------------
C         Save reported limit. Note: the reported limit calculated
C         during the first iteration is used for all subsequent
C         iterations when calculating the rate-limited fuel flow.
C         this measure is necesary to ensure stability of the
C         iteration loop.
C
C         When searching for the rate-limited operating point, the
C         bisection search may select future-row operating points
C         that lie on both sides of the present-row point:
C
C          iteration 1:  fN_fuel_flow > fN_fuel_flow_init
C          iteration 2:  fN_fuel_flow < fN_fuel_flow_init
C          iteration 3:  ...
C
C         Under these circumstances, Eval_TS_average will calculate
C         a rate limited fuel flow that alternately reflects 1) an
C         increase power output, and 2) a decrease in power output.
C         If converging towards the most recently rate-limited fuel
C         flow calculated by Eval_TS_average, the bisection search
C         will likely converge on the incorrect operating point.
C
C         Instead, the rate limited value calulated on the first
C         iteration is saved, convergence is tested against this
C         value. On the first iteration, the estimated fuel flow,
C         fN_fuel_flow, reflects the operating point requested by
C         the CHP unit's controller. With this value, Eval_TS_average
C         will determine the correct rate-limited fuel flow.
C
C-------------------------------------------------------------------
          if ( .not. bLimitSet ) then
            fN_fuel_flow_Trans_limit = fRate_limited_value
            bLimitSet = .true.
          endif

C.........If Eval_TS_average reports change in fuel flow is rate
C.........limited, set loop flag.
          if ( bLimited ) then
            bFuel_Rate_Limited = .true.
          endif

C-------------------------------------------------------------------
C         Is the rate-limited fuel flow rate (roughly) equal to
C         the rate reported by in Eval_Annex42_CHP_Correlations?
C-------------------------------------------------------------------
          if ( bFuel_Rate_Limited ) then

C...........Note: fN_fuel_flow & fN_fuel_flow_init are both small,
C...........but their values will be comparable within a single
C...........order of magnitude. Therefore, there's no need to
C...........worry about machine precision --- we just need to
C...........provide a suitabily small comparison tolerance.

            fVar_Tolerance = AMAX1 ( fN_Fuel_Flow,
     &                          fN_fuel_flow_Trans_limit,
     &                          1.0E-15 ) * 1.0E-3

          call eclose ( fN_Fuel_Flow,
     &                    fN_fuel_flow_Trans_limit,
     &                  fVar_Tolerance,
     &                  bFuel_dNdT_within_limits )

          else

            bFuel_dNdT_within_limits = .true.

          endif

C-------------------------------------------------------------------
C         But rate limited fuel flow is disabled during warm-up
C         period!
C-------------------------------------------------------------------
          if ( .not. bWarmup_complete (iPComp ) .and.
     &         .not. bFuel_dNdT_within_limits ) then
            bFuel_dNdT_within_limits = .true.
          endif
C-------------------------------------------------------------------
C         If fuel flows are not equal, update bounds and loop again.
C-------------------------------------------------------------------
          if ( .not. bFuel_dNdT_within_limits .and.
     &         .not. bCorrelation_error ) then
C-------------------------------------------------------------------
C           Limit on fuel flow rate of change pervents unit
C           from reaching desired operating point. Initialize
C           bisection search for rate-limited operating point.
C-------------------------------------------------------------------

            if ( fN_Fuel_Flow > fN_fuel_flow_Trans_limit ) then
C.............Rate limited fuel flow is less than flow at
C.............desired operating point. - Power must be
C.............reduced.
              fP_net_UB = fOP_system
            else
C.............Rate limited fuel flow greater than flow at
C.............desired operating point. - Power must be
C.............increased.
              fP_net_LB = fOP_system
            endif
C...........Calculate new bisection point
            fOP_system = ( fP_net_UB + fP_net_LB ) / 2.0

          endif

        enddo   ! <- matches 'do while ( .not. bFuel_dNdT_within_limits'

        if ( bCorrelation_error ) then
C-------------------------------------------------------------------
C         An error was encountered while evaluating the
C         performance map correlations - deactivate unit.
C-------------------------------------------------------------------
          fP_net   = 0.0
          fQ_cogen = 0.0
          fN_fuel_flow = 0.0
          fN_fuel_flow_trans_average = 0.0
          fN_fuel_flow_trans_final   = 0.0
          iSystem_Status = iStatus_Corr_Err
        else
          fP_net = fOP_System
        endif


C----------------------------------------------------------------------
C        If the unit is still in warm-up period, adjust power
C        to reflect dependance on engine temperature.
C
C        P net = scalar * P nominal
C
C                   * ( T engine - T room ) / ( T nominal - T room )
C
C----------------------------------------------------------------------
         if ( .not. bWarmup_complete(iPComp) ) then

C..........Trap potential divide by zero:
           call eclose ( fNominal_engine_temp ( iPComp ),
     &                   fTemp_Room_F,
     &                   fClose_to_zero_tolerance,
     &                   bNums_Are_Close )

           if ( bNums_Are_Close .or.
     &          fNominal_engine_temp (iPComp) .lt. fTemp_Room_F ) then
             fP_net = 0.0
             ! Warning message to go here !
           else

             fP_net = fPower_Temp_sens ( iPComp )
     &            *  fPower_Out_MAX ( iPComp )
     &            * ( fEngine_Temp_F - fTemp_Room_F )
     &            / ( fNominal_engine_temp ( iPComp ) - fTemp_Room_F )
           endif

C..........Disable rate limiting
           fRate_Limit = -1.0


         else

C..........Set rate limit to user supplied value
           fRate_Limit = fGross_Power_Change_MAX(iPComp)

         endif !<- matches if ( .not. bWarmup_complete(iPComp) ) then

C----------------------------------------------------------------------
C        Make sure that power output does not exceed maximum
C        permissible rate of change over the timestep. Again,
C        Use function Eval_TS_average to determine the maximum
C        permissible change in the power over the timestep, the
C        power at the end of the time-step, and the average
C        power over the time-step.
C
C        Note: in the case of the Annex 42 CHP model, the rate
C              limit is applied *AFTER* the power demand is
C              used determine the system operating point.
C              The actual power output may differ from the
C              system demand due to thermal mass and other
C              lag effects.
C
C              Rate limiting can also be disabled by providing
C              a value of '-1'.
C----------------------------------------------------------------------

C.......ICE unit doesn't produce power during startup
        if ( iOperating_mode_F == iOP_startup ) fP_net = 0

        if ( bFuel_Rate_Limited ) then
        
          iStrategy = iLinear
          
        else 
          
          iStrategy = iStepwise
          
        endif 

        call Eval_TS_average (
     &           fP_net_init,
     &           fP_net,
     &           fGross_Power_Change_MAX(iPComp),
     &           fTS_duration
     &               * fTS_mode_fractions (iOperating_mode_F),
     &           iStrategy,
     &           fP_net_Trans_average,
     &           fP_net_Trans_final,
     &           fRate_limited_value, bLimited   )


C----------------------------------------------------------------------
C        Use function Eval_TS_average to determine the average
C        rate of steady-state cogeneration heat recovery. Note:
C        the change in the average rate of steady state heat
C        recovery is not rate limited. A value of '-1' is passed
C        to Eval_TS_average() to disable rate limiting.
C----------------------------------------------------------------------

           call Eval_TS_average (
     &             fQ_cogen_init,
     &             fQ_cogen,
     &             -1.0,
     &             fTS_duration
     &                 * fTS_mode_fractions (iOperating_mode_F),
     &             iStepwise,
     &             fQ_cogen_Trans_average,
     &             fQ_cogen_Trans_final, fRate_limited_value, bLimited )


C----------------------------------------------------------------------
C        Assign fuel flow power / heat generation to cetc_cogen
C        arrays
C----------------------------------------------------------------------
         fPeriod_fuel_flow
     &      ( iPComp, iOperating_mode_F )
     &    = fN_fuel_flow_Trans_average

         fPeriod_AC_Power
     &      ( iPComp, iOperating_mode_F )
     &    = fP_net_Trans_average

         fPeriod_Heat_Recovery
     &      ( iPComp, iOperating_mode_F )
     &     = fQ_cogen_Trans_average

C--------------------------------------------------------------------
C        If ICE unit will operate in startup, but will complete
C        startup cycle on this timestep, then assign fuel flow and heat
C        generation terms to start-up period as well. Electric power
C        is set to zero during start-up, however.
C--------------------------------------------------------------------
         if ( iOperating_mode_F /= iOP_startup .and.
     &        bTS_modes( iOP_startup ) ) then

            fPeriod_fuel_flow
     &        ( iPComp, iOP_startup )
     &         = fN_fuel_flow

            fPeriod_AC_Power
     &      ( iPComp, iOP_startup )
     &        = 0.0

            fPeriod_Heat_Recovery
     &        ( iPComp, iOP_startup )
     &        = fQ_cogen

         endif

      endif    ! <- matches 'if ( .not. bTS_modes (iOP_normal_operation) ) then'

C--------------------------------------------------------------------
C     If device is using fuel, determine air flow, using model
C     correlation --- See equation 17 in Ferguson and Kelly (2006)
C--------------------------------------------------------------------
      if ( bTS_modes ( iOP_normal_operation ) .or.
     &     bTS_modes ( iOP_warmup )           .or.
     &     bTS_modes ( iOP_startup )                ) then

        fN_air_flow =   fPM_air_coeff(iPComp, 1)
     &                + fPM_air_coeff(iPComp, 2) * fN_fuel_flow ** 2.0
     &                + fPM_air_coeff(iPComp, 3) * fN_fuel_flow

      else
        fN_air_flow = 0.0
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
     &      * fPeriod_AC_Power(iPComp,iMode)

         fN_fuel_flow_average = fN_fuel_flow_average
     &      + fTS_mode_fractions(iMode)
     &      * fPeriod_fuel_flow(iPComp,iMode)

         fQ_Cogen_average = fQ_Cogen_average
     &      + fTS_mode_fractions(iMode)
     &      * fPeriod_Heat_Recovery(iPComp,iMode)

      enddo


C----------------------------------------------------------------------
C     Save instantaneous fuel/heat/power flows at end of timestep
C----------------------------------------------------------------------
      if ( iOperating_mode_F == iOP_normal_operation .OR.
     &     iOperating_mode_F == iOP_warmup ) then
C..........In normal operating mode, the flows may have changed
C..........during operating mode. Use final state calculated by
C..........Eval_TS_average()

           fP_net_final = fP_net_trans_final

           fQ_cogen_final = fQ_cogen_trans_final

           fN_fuel_flow_final = fN_fuel_flow_Trans_final

      else
C..........In all other operating modes, the flows are assumed to be
C..........constant.

           fP_net_final = fPeriod_AC_Power(iPComp, iOperating_mode_F)

           fQ_cogen_final =
     &          fPeriod_Heat_Recovery(iPComp, iOperating_mode_F)

           fN_fuel_flow_final =
     &          fPeriod_fuel_flow(iPComp, iOperating_mode_F)

      endif


C.....Save average heat transfer to water as future-time-row variable.
      fQ_cogen_F = fQ_Cogen_average

C-----------------------------------------------------------------------
C     Estimate time constant using future time row data
C
C     Consider the linear system:
C
C       dX
C       --  = -a X(t) + c 
C       dt
C
C     The time constant is:
C
C
C       TC = 1 / a 
C
C     If each node in the engine model is set considered to be a 
C     linear system subject to constant boundary conditions, 
C     meaningful time constants can be calculated. For the 
C     engine 
C
C     Consider the engine control volume:
C
C       d T_eng    - UA_HX ( T_eng - T_cw )  - UA_loss ( T_eng - T_amb )
C       -------  = -----------------------------------------------------
C         dt                        [MC]_eng 
C
C       d T_eng    -[UA_HX + UA_loss] ( T_eng ) + C
C       -------  = --------------------------------
C         dt                   [MC]_eng
C
C       ( where C = UA_HX * T_cw + UA_loss * T_amb )
C
C
C     Then a = [UA_HX + UA_loss]/[MC]_eng, and the time constant is:
C
C                    [MC]_eng
C       TC_eng = -----------------
C                [UA_HX + UA_loss]
C
C-----------------------------------------------------------------------
    

C.....Ensure denominator will not be zero!
      call eclose ( fEffective_UA_HX(iPComp) 
     &                + fEffective_UA_loss(iPComp),
     &                0.0, 1.0E-03, bNums_are_close )

      if ( bNums_Are_Close ) then

C........Time constant is arbitrarly large:
         fTC_eng_node = 10.0 * fTS_duration

      else

         fTC_eng_node = fEffective_MC_engine(iPComp)
     &                    / 
     &                      (   fEffective_UA_HX(iPComp) 
     &                        + fEffective_UA_loss(iPComp)
     &                      )

      endif

C-----------------------------------------------------------------------
C     Similarly, consider the cooling water control volume
C
C       d T_cw    - UA_HX ( T_cw - T_eng ) - [mC_p]_cw ( T_cw - T_cw,i )
C       ------  = ------------------------------------------------------
C         dt                        [MC]_cw
C
C       d T_cw     -[UA_HX + [mC_p]_cw] ( T_cw ) + C
C       -------  = ---------------------------------
C         dt                 [MC]_cw
C
C       ( where C = UA_HX * T_cw + UA_loss * T_amb )
C
C
C     Then a = [UA_HX + [mC_p]_cw]/[MC]_cw, and the time constant is:
C
C                  [MC]_cw
C       TC_cw = -----------------
C              [UA_HX + (mC_p)_cw]
C
C-----------------------------------------------------------------------



C.....Ensure denominator will not be zero!
      call eclose ( fEffective_UA_HX(iPComp) 
     &                + fCW_heat_cap_F,
     &                0.0, 1.0E-03, bNums_are_close )

      if ( bNums_Are_Close ) then

C........Time constant is arbitrarly large:
         fTC_cw_node = 10.0 * fTS_duration

      else

         fTC_cw_node = fEffective_MC_HX(iPComp)
     &                    / 
     &                      (   fEffective_UA_HX(iPComp) 
     &                        + fCW_heat_cap_F
     &                      )

      endif

C-----------------------------------------------------------------------
C     Now use the smallest time constant when evaluating stability 
C     of mixed solution scheme. 
C-----------------------------------------------------------------------
      fTime_Constant = amin1(fTC_cw_node,fTC_eng_node)

C.....Save time constant in ESRU commonblock (why?)
      tc(iPComp) = fTime_Constant

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
      if ( impexp == 1 ) then
C........Fully implicit solution has been specified by the user
         fSoln_alpha = 1.0

      elseif (impexp == 2 ) then
C........Mixed  solution scheme. Use provided weighting factor ratimp
         fSoln_alpha = ratimp

      elseif (impexp == 3 ) then
C........Compare simulation timestep with component's estimated
C........time consant, and revert to fully implicit solution
C........if timesteps are too large to accuartely characterize
C........component
         if ( fTS_duration > 0.63 * fTime_Constant ) then

            fSoln_alpha = 1.0

         else

            fSoln_alpha = ratimp

         endif

      elseif ( IMPEXP == 4 ) then
C.......'steady state' solution. This feature is untested, and
C........will quite possibly cause spectacular errors in
C........the simulation. A warning is issued during the
C........error trapping procedure.

         ! Warning goes here....

      else
C........Shouldn't happen, as impexp is error trapped elsewhere?
         STOP 'A42_CHP_Thermal_Char: Solution scheme is unsupported'
      endif


C-------------------------------------------------------------------
C
C     ESP-r matrix has the following form:
C
C        |  a11  a12   -  | |  T_{sys} |     | R1 |
C        |  a21  a22  a23 | |  T_{HX}  |  =  | R2 |
C                           | T_{cw,i} |
C
C     First self-coupling coefficient:
C
C              Engine heat capacity
C        a11 = --------------------
C               time step duration
C
C                + alpha * ( HX UA value + UA loss value )
C
C-------------------------------------------------------------------

      fCOut(1) =
     &
     &fEffective_MC_engine(iPComp) / fTS_duration
     &
     &+ fSoln_alpha * (
     &    fEffective_UA_HX(iPComp) + fEffective_UA_loss(iPComp))

C-------------------------------------------------------------------
C     Second self-coupling coefficient:
C
C         a12 = - alpha * HX UA value
C
C-------------------------------------------------------------------

      fCOut(2) = -1.0 * fSoln_alpha
     &                        * fEffective_UA_HX(iPComp)

C-------------------------------------------------------------------
C     Third self-coupling coefficient:
C
C        a21 = - alpha * HX UA value
C
C-------------------------------------------------------------------

      fCOut(3) = -1.0 * fSoln_alpha
     &                        * fEffective_UA_HX(iPComp)

C-------------------------------------------------------------------
C     Fourth self-coupling coefficient:
C
C               HX heat capacity
C        a22 = ------------------  +
C              time step duration
C
C                 alpha * ( HX UA value + cooling water heat capacity )
C
C-------------------------------------------------------------------

      fCOut(4) =
     &
     &     fEffective_MC_HX(iPComp) / fTS_duration
     &
     &   + fSoln_alpha * ( fEffective_UA_HX(iPComp)
     &                                   + fCW_heat_cap_F )


C-------------------------------------------------------------------
C     Cross coupling coefficient:
C
C        a23 = - alpha * cooling water heat capacity
C
C-------------------------------------------------------------------

      fCOut(5) = -1.0 * fSoln_alpha * fCW_heat_cap_F

C-------------------------------------------------------------------
C     Present-time & known coefficient 1:
C
C             Engine heat capacity
C        R1 = -------------------- * Present engine temperature
C              Time step duration
C
C                + ( 1 - alpha ) *
C                     ( HX UA Value *
C                           ( present HX temp - present engine temp )
C                        + Present cogen heat )
C
C                + alpha * Future cogen heat
C
C-------------------------------------------------------------------

      fCOut(6) =
     &
     &    fEffective_MC_engine(iPComp) / fTS_duration
     &       * fEngine_Temp_P
     &
     & + ( 1.0 - fSoln_alpha )
     &
     &      * ( fEffective_UA_HX ( iPComp )
     &           * ( fCW_temperature_out_P - fEngine_Temp_P )
     &
     &          + fQ_cogen_P
     &
     &          + fEffective_UA_loss ( iPComp ) *
     &               ( fTemp_Room_P - fEngine_Temp_P )
     &         )
     &
     & + fSoln_alpha * ( fQ_cogen_F + fEffective_UA_loss ( iPComp )
     &                                  * fTemp_Room_F )




C-------------------------------------------------------------------
C     Present-time & known coefficient 2:
C
C               HX heat capacity
C        R1 = -------------------- * Present engine temperature
C              Time step duration
C
C                + ( 1 - alpha ) *
C
C                     ( HX UA Value *
C                           ( present engne temp - present HX out. temp )
C
C                       + Present cooling water heat capacity *
C                           ( present CW inlet temp
C                                   - present cw outlet temp )
C
C-------------------------------------------------------------------
      fCOut(7) =
     &
     &   fEffective_MC_HX ( iPComp ) / fTS_duration
     &                                 * fCW_temperature_out_P
     &
     & + ( 1.0 - fSoln_alpha ) * (
     &
     &      fEffective_UA_HX ( iPComp )
     &            * ( fEngine_Temp_P - fCW_temperature_out_P )
     &
     &    + fCW_heat_cap_P
     &            * ( fCW_temperature_in_P - fCW_temperature_out_P )
     &
     &    )


C---------------------------------------------------------------------
C     Save timerow data for use on the next time step
C---------------------------------------------------------------------
      PCDatF(iPComp, 1)      = float(iOperating_mode_F)    ! (-)
      PCDatF(iPComp, 2)      = float(iSystem_Status)       ! (-)
      PCDATF(iPComp, 3)      = fOP_time                    ! (s)

C.....Mass & Energy flow initial conditions for transient behavior
      PCDatF(iPComp, 4)      =  fN_fuel_flow_final    ! (kg/s)
      PCDatF(iPComp, 5)      =  fP_net_final          ! (W)
      PCDatF(iPComp, 6)      =  fQ_cogen_final        ! (W)
      PCDatF(iPComp, 7)      =  fQ_cogen_F            ! (W)

      PCDatF(iPComp, 9)      =  fCW_temperature_in_F  ! (oC)
      PCDatF(iPComp,10)      =  fCW_mass_flow_F       ! (kg/s)

      PCDatF(iPComp,11)      =  fOP_target_F          ! Control signal

C.....Actual operating point. This data is not used by A42_CHP_Thermal_dyn,
C.....but is used by A42_CHP_flow_char.
      PCDatF(iPComp,12)      =  fOP_system            ! Actual operating point

C-----------------------------------------------------------------------
C     Calculate device's carbon emissions:
C
C     CO2 Emissions = ( fuel flow, kg/s )
C                * ( fuel carbon dioxide intensity, kg CO2/ kg fuel)
C
C-----------------------------------------------------------------------
      fAverage_CO2_emissions ( iPComp ) =
     &    fN_fuel_flow_average * fFuel_CO2_intensity ( iPComp )

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
C            pwrp(iPComp),              Real power (input, W)
C            PQ,                        Complex power (output, W)
C            PA)                        Overall power (output, W)
C-----------------------------------------------------------------------

C.....Real power output (W), generation is +ive...
      pwrp(iPComp) = fP_net_average
      IEMODEL=1               ! Flag indicating which power calculation
                              ! will be performed
      CALL EMACH(
     &     iPComp,            ! Component number (input)
     &     IEMODEL,           ! power calculation flag (input)
     &     pwrp(iPComp),      ! Real power (input, W)
     &     PQ,                ! Complex power (output, W)
     &     PA)                ! Overall power (output, W)


C----------------------------------------------------------------------
C     Save reportable data in commons for use at end of simulation
C     timestep.
C
C     -> Report instantaneous data describing system operation at
C        end of the timestep.
C----------------------------------------------------------------------

C.....Control
      fReport_Control_Method (iPComp ) =
     &                              float (iControl_method)

      fReport_Control_Signal (iPComp) = fControl_signal

C.....Net power
      fReport_Net_Power(iPComp)     = fP_net_final          ! (W)

C.....Heat recovery
      fReport_Heat_Recovery(iPComp) = fQ_Cogen_final ! (W)

C.....Heater skin losses & containment temperature
C     fReport_Heat_Loss(iPComp) = fQ_skin_losses_final      ! (W, not supported?)
      fReport_Containment_Temp(iPComp) = fTemp_Room_P       ! (oC)

C.....Fuel flow, and rate of gross heat input
      fReport_Fuel_Flow(iPComp) = fN_fuel_flow_final        ! (kg/s)
      fReport_Fuel_LHV_rate(iPComp) =
     &         fN_fuel_flow_final * fFuel_LHV(iPComp)       ! (W)

C.....Air Flow
      fReport_Air_Flow(iPcomp) = fN_air_flow                ! (kg/s)

C.....Electric, thermal & cogeneration efficiencies --- Note that
C.....efficiencies can only be calculated if fuel is being used
C.....by the system
      call eclose (fN_fuel_flow_final, 0.0,
     &             fClose_to_zero_tolerance, bNums_are_close)

      if ( .not. bNums_are_close .and. fN_fuel_flow_final > 0) then
C----------------------------------------------------------------------
C        Calculate instantaneous efficiencies:
C
C                               useful output
C          efficiency = -------------------------------
C                        energy value of fuel used (LHV)
C
C----------------------------------------------------------------------

         fReport_Electric_Efficiency(iPComp) = fP_net_final
     &       / ( fN_fuel_flow_final * fFuel_LHV(iPComp) ) !(-)

         fReport_Thermal_Efficiency(iPComp)=
     &          fQ_Cogen_final
     &       / ( fN_fuel_flow_final * fFuel_LHV(iPComp) )

         fReport_Cogen_efficiency(iPComp)=
     &         ( fP_net_final + fQ_Cogen_final )
     &       / ( fN_fuel_flow_final * fFuel_LHV(iPComp))  ! (-)

      else
         fReport_Electric_Efficiency(iPComp)  = 0.0
         fReport_Thermal_Efficiency(iPComp)   = 0.0
         fReport_Cogen_Efficiency(iPComp)     = 0.0
      endif

C.....Cooling water temperature & flow rate
      fReport_CW_Mass_Flow(iPComp)   = fCW_mass_flow_F      ! (kg/s)
      fReport_CW_Temperature(iPComp) = fCW_temperature_in_F ! (kg/s)

C----------------------------------------------------------------------
C     -> Report averaged data describing system operation over
C        entire timestep. Note: it is only possible to report
C        averaged data for the variables for which transient
C        characteristics are available (Net power, fuel flow,
C        heat output.)
C----------------------------------------------------------------------
      fAverage_Heat_Recovery(iPComp)
     &    = fQ_Cogen_average

      fAverage_Net_Power(iPComp)     = fP_net_average

      fAverage_Fuel_Flow(iPComp)     = fN_fuel_flow_average

      fAverage_Fuel_LHV_rate(iPComp) = fN_fuel_flow_average
     &    * fFuel_LHV(iPComp)

C.....Electric, thermal & cogeneration efficiencies --- Note that
C.....efficiencies can only be calculated if fuel is being used
C.....by the system
      call eclose (fN_fuel_flow_average, 0.0,
     &             fClose_to_zero_tolerance, bNums_are_close)

      if ( .not. bNums_are_close .and. fN_fuel_flow_average > 0) then

         fAverage_Electric_Efficiency(iPComp)= fP_net_average
     &       / ( fN_fuel_flow_average * fFuel_LHV(iPComp) ) !(-)

         fAverage_Thermal_Efficiency(iPComp) =
     &          fQ_Cogen_average
     &       / ( fN_fuel_flow_average * fFuel_LHV(iPComp) ) !(-)

         fAverage_Cogen_efficiency(iPComp)=
     &         ( fP_net_average + fQ_Cogen_average )
     &       / ( fN_fuel_flow_average * fFuel_LHV(iPComp))  ! (-)

      else
         fAverage_Electric_Efficiency(iPComp)  = 0.0
         fAverage_Thermal_Efficiency(iPComp)   = 0.0
         fAverage_Cogen_Efficiency(iPComp)     = 0.0
      endif


C.....Fraction of timestep spent in each mode
      do iMode = 1, iOP_Mode_Count
C........Flags indicating which modes unit operated in
         bReport_TS_modes (iPComp, iMode) = bTS_modes(iMode)  ! (-)
C........Time spent in each mode
         fReport_time_spent (iPComp, iMode) =
     &          fTS_mode_fractions(iMode) * fTS_duration             ! (s)

      enddo

C.....Solution implicit / explicit weighting factor
      fReport_alpha ( iPComp ) = fSoln_alpha

C.....Control volume time constants
      fReport_TC_eng ( iPComp ) = fTC_eng_node                ! (s)
      fReport_TC_cw  ( iPComp ) = fTC_cw_node                 ! (s)

      if (bDebug) write(ITU,*)
     &   'Exiting subroutine A42_CHP_thermal_char'

      return
      end



C------------------- A42_CHP_Collect_Data ----------------------------
C
C This subrouine collects time-invariant plant component parameters
C stored in ESP-r's adata & bdata arrays, and stores them in the
C common blocks defined in Annex42_combustion_CHP.h and cetc_cogen.h
C
C This routine performs data collection for two different model
C configurations:
C   - Annex 42 ICE CHP model ( iType = 102 )
C   - Annex 42 stirling CHP model ( iType = 103 )
C
C Both of these models share some common parameters --- to reduce
C code redundancy, the code determines which model is in use and
C then executes the appropriate statements.
C
C This procedure also performs some rudementry error checking, but
C would benifit from more comprehensive error checking routines.
C
C INPUTS:
C
C  - iComp: index of component in plant network
C  - iType: flag indicating which model (ICE or SE)
C           has been specified.
C
C OUTPUTS:
C   - NONE
C
C
C
C----------------------------------------------------------------------
      subroutine A42_CHP_Collect_Data_dyn ( iComp, iType )
      implicit none

#include "building.h"
#include "plant.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
#include "chemical_properties.h"

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

      integer iType            ! Flag indicating 'type' of model
                               !   in use.
      integer iComp            ! index of component in plant network
      integer iShift           ! index where empirical coefficients
                               ! are stored

C.....Array containing fuel composition
      real fFuel_composition ( iCompound_count )
      integer ii               ! counter

      real fFuel_fraction      ! buffer used to determine of
                               !   specified fuel composition adds
                               !   up to 1.0

      real fMols_C_in_products ! # of mols of carbon released per mol
                               !    of fuel used.

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
      real fEval_Compound_MM   ! Function returning molar mass of a
                               !   specified compound.
      real fEval_Mixture_HV    ! Function to evaluate a mixture's
                               !   heating value
      real fEval_Mixture_MM    ! Function to evaluate a mixture's
                               !   molar mass
      logical bFlammable_Mixture ! Function determining if a mixture
                               !   can be oxidized.

      real fEval_element_mols_in_mixture ! Function returning
                               !           the number of mols of an
                               !           element in a mixture

      bFatal_error = .false.

C.....Make sure array is empty
      do ii = 1, iCompound_count
         fFuel_composition (ii) = 0.0
      enddo

C--------------------------------------------------------------------
C     Note: this routine uses a shift register to keep track of its
C           location in the input data array. This is merely
C           a crude work around to avoid re-numbering the 100+ inputs
C           whenever the inputs are reordered.
C--------------------------------------------------------------------
      iShift = 0

C--------------------------------------------------------------------
C     Max & minimum power output  (W)
C--------------------------------------------------------------------
      fPower_Out_MAX(iComp) = ADATA ( iComp, iShift + 1 )
      fPower_Out_MIN(iComp) = ADATA ( iComp, iShift + 2 )

C.....Error checking
      if ( fPower_Out_MIN(iComp) > fPower_Out_MAX(iComp) ) then
         write (itu,*) 'A42 CHP Warning: Specified '
     &        // 'minimum power output is larger then specified '
     &        // 'maximum '
         write (itu,*)
     &            '                      Switching over maximum '
     &         // ' and minimum power outputs'
         fPower_Out_MIN(iComp) = ADATA ( iComp, iShift + 1 )
         fPower_Out_MAX(iComp) = ADATA ( iComp, iShift + 2 )

      endif
C.....Is maximum power close to zero?
      call eClose( fPower_Out_MAX(iComp), 0.0, 1.0E-10,
     &             bNums_are_close_a )
      if (bNums_are_close_a ) then
         write (itu,*) 'A42 CHP Error: Specified '
     &        // 'maximum power output  is zero!  '
         write (itu,'(A,I2,A,I2)')
     &                 '                 Check misc data item',
     &                 iShift + 2,' in Component # ', iComp

         bFatal_error = .true.

      endif

      iShift = iShift + 2

C--------------------------------------------------------------------
C     Maximum cooling water outlet temperature oC
C--------------------------------------------------------------------
      fCW_temperature_MAX (iComp) = ADATA(iComp, iShift + 1 )
      iShift = iShift + 1


C--------------------------------------------------------------------
C     Fuel type (liquid fuel w/ LHV or gaseous mixture...)
C--------------------------------------------------------------------
      iFuel_type ( iComp ) = int ( ADATA(iComp, iShift + 1 ) )
      iShift = iShift + 1

C--------------------------------------------------------------------
C     Liquid fuel parameters. These parameters are only meaningful
C     if a liquid fuel has been specified above.
C--------------------------------------------------------------------
      if ( iFuel_type (iComp) == iLiquid_Fuel ) then
C.......Fuel LHV (J/kg)
        fFuel_LHV(iComp)               = ADATA(iComp, iShift + 1 )

C.......We don't know the higher heating value ---
C.......Use a conservitive guesstimate: HHV = 1.25 * LHV
C.......( The higher heating value is only used to warn
C.......  users when the efficiency predicitons are likely out
C.......  of range! )
        fFuel_HHV (iComp) = 1.25 * fFuel_LHV (iComp)

C.......Fuel carbon intensity: kg CO2 / kg fuel
        fFuel_CO2_intensity ( iComp ) = ADATA(iComp, iShift + 2 )


      endif
      iShift = iShift + 2

C--------------------------------------------------------------------
C     Gaseous Fuel composition (mol/mol basis)
C--------------------------------------------------------------------

      if ( iFuel_type (iComp) == iGaseous_mixture ) then
C........Check if constituents are between 0 & 1.
         fFuel_fraction = 0.0
         do ii = iShift + 1 , iShift + 13
            call eclose (ADATA(iComp, ii),0.0,0.0001,bNums_are_close_a)
            call eclose (ADATA(iComp, ii),0.0,0.0001,bNums_are_close_b)

            if ( ( ADATA(iComp, ii) < 0.0 .OR.
     &             ADATA(iComp, ii) > 1.0     ).AND. .not.
     &           ( bNums_are_close_a .OR. bNums_are_close_b ) ) then
               write (itu,*) 'A42 CHP Error: Fuel contituent mol'
     &                    // ' fraction must be between 0 & 1.'
               write (itu,'(A,I2,A,I2)')
     &                  '                     Check misc data item',
     &                  ii,' in Component # ', iComp

               bfatal_error = .true.
            endif

            fFuel_fraction = fFuel_fraction + ADATA(iComp, ii)

         enddo
C........Do fractions add to 1?
         call eclose (fFuel_fraction, 1.0, 0.0001,bNums_are_close_a)


         if ( .not. bNums_are_close_a ) then
C...........Warn user
            write(itu,*) 'A42 CHP Warning: Fuel constituent'
     &                   // ' mole fractions do not add to 1. '
            write(itu,'(A,F8.6,A)')
     &                   '                       Normalizing '
     &               // 'constituents by total (',fFuel_fraction,')'
         endif

         fFuel_Composition(iHydrogen  ) = ADATA ( iComp, iShift + 1 )
     &                                    / fFuel_fraction
         fFuel_composition(iMethane   ) = ADATA ( iComp, iShift + 2 )
     &                                    / fFuel_fraction
         fFuel_composition(iEthane    ) = ADATA ( iComp, iShift + 3 )
     &                                    / fFuel_fraction
         fFuel_composition(iPropane   ) = ADATA ( iComp, iShift + 4 )
     &                                    / fFuel_fraction
         fFuel_composition(iButane    ) = ADATA ( iComp, iShift + 5 )
     &                                    / fFuel_fraction
         fFuel_composition(iPentane   ) = ADATA ( iComp, iShift + 6 )
     &                                    / fFuel_fraction
         fFuel_composition(iHexane    ) = ADATA ( iComp, iShift + 7 )
     &                                    / fFuel_fraction
         fFuel_composition(iMethanol  ) = ADATA ( iComp, iShift + 8 )
     &                                    / fFuel_fraction
         fFuel_composition(iEthanol   ) = ADATA ( iComp, iShift + 9 )
     &                                    / fFuel_fraction
         fFuel_composition(iC_Monoxide) = ADATA ( iComp, iShift + 10 )
     &                                    / fFuel_fraction
         fFuel_composition(iC_Dioxide ) = ADATA ( iComp, iShift + 11 )
     &                                    / fFuel_fraction
         fFuel_composition(iNitrogen  ) = ADATA ( iComp, iShift + 12 )
     &                                    / fFuel_fraction
         fFuel_composition(iOxygen    ) = ADATA ( iComp, iShift + 13 )
     &                                       / fFuel_fraction



C-----------------------------------------------------------------------
C        Evaluate fuel lower heating value.
C         -> function fEval_Mixture_HV returns heating value (J/kmol)
C         -> function fEval_Mixture_MM returns molar mass (kg/kmol)
C
C        Note: fEval_Mixture_HV can return both the higher heating
C              value and lower heating value. Use iLHV to specify
C              lower heating value, and iHHV to specify higher heating
C              value.
C-----------------------------------------------------------------------
         fFuel_LHV( iComp ) =
     &                fEval_Mixture_HV ( fFuel_Composition, iLHV )
     &              / fEval_Mixture_MM ( fFuel_Composition )       ! J/kg

         fFuel_HHV( iComp ) =
     &                fEval_Mixture_HV ( fFuel_Composition, iHHV )
     &              / fEval_Mixture_MM ( fFuel_Composition )       ! J/kg

C........IS LHV value greater than zero?
         if ( .not. bFlammable_Mixture ( fFuel_Composition ) ) then
            write(itu,*) 'A42 CHP Error: Fuel mixture is not '
     &                // ' flammable! '
            write(itu,'(A,I2)') '                     Check fuel '
     &                // ' constituent mol fractions in component # ',
     &                   iComp
            bFatal_error = .true.
         endif


C-----------------------------------------------------------------------
C       Determine mixture's carbon dioxide intensity. This is a very
C       simple calculation assuming:
C
C               i)  complete combustion,
C               ii) CO2 is the only green house gas emission under
C                   consideration - equivlant emisssions for higher
C                   compounds are not calculated.
C
C       This calculation will hopefully one day be replaced
C       by a general function from chemical_properties.F that
C       quantifies the products of combustion when given a fuel
C       mixture, air stochiometry and combustion efficiency.
C
C-----------------------------------------------------------------------

        fMols_C_in_products = fEval_element_mols_in_mixture (
     &      fFuel_Composition, iElement_Carbon )

        fFuel_CO2_intensity ( iComp ) =
     &    fMols_C_in_products * fEval_Compound_MM ( iC_Dioxide )
     &  / fEval_Mixture_MM ( fFuel_composition )

      endif    ! <- matches 'if ( iFuel_type (iComp) ) == ...'
      iShift = iShift + 13

C--------------------------------------------------------------------
C     Transient behavior (max rates of change in fuel flow,
C     electrical & thermal output) Note: negative
C     values can be used to disable range checking.
C--------------------------------------------------------------------
      fFuel_Flow_Change_MAX(iComp)  = ADATA ( iComp, iShift +  1 ) ! (kg/s^2)
      fGross_Power_Change_MAX(iComp)= ADATA ( iComp, iShift +  2 ) ! W/s
      iShift = iShift + 2

C--------------------------------------------------------------------
C     Linear heat recovery rate of change limit is not used in the
C     A42 models - set to -1 to disable range checking.
C--------------------------------------------------------------------
      fHeat_Recovery_Change_MAX (iComp) = -1.0

C--------------------------------------------------------------------
C     Parameters for thermal mass model
C     Note: in the early SE model, these parameters are only used
C     if the heat lag model is active (ie iDynamic_thermal_flag = 1)
C--------------------------------------------------------------------

C.....Thermal mass model: SPS thermal mass and effective heat
C.....transfer coefficient.
      fEffective_MC_engine(iComp)  = ADATA ( iComp, iShift +  1 )

      fEffective_UA_HX(iComp)      = ADATA ( iComp, iShift +  2 )

      fEffective_UA_loss(iComp)    = ADATA ( iComp, iShift +  3 )

C.....Heat exchanger thermal mass
      fEffective_MC_HX(iComp)      = ADATA ( iComp, iShift +  4 )

      iShift = iShift + 4

      if ( iType == iSE_model) then
C----------------------------------------------------------------------
C       Engine nominal temperature, and sensitivity of fuel flow and
C       power output to engine temperature --- SE model only.
C----------------------------------------------------------------------

        fNominal_engine_temp(iComp)
     &                       = ADATA ( iComp, iShift + 1 ) ! (oC)
        fFuel_Temp_sens(iComp)
     &                       = ADATA ( iComp, iShift + 2 ) ! (-)
        fPower_Temp_sens(iComp)
     &                       = ADATA ( iComp, iShift + 3 ) ! (-)

        fFuel_Flow_MAX(iComp)
     &                       = ADATA ( iComp, iShift + 4 ) ! (-)

        iShift = iShift + 4

C----------------------------------------------------------------------
C       Start-up period is not modelled separately, set values
C       to zero
C----------------------------------------------------------------------
        fPeriod_duration(iComp,iOP_startup)  = 0.0

      elseif ( iType == iICE_model ) then

C----------------------------------------------------------------------
C       Start-up period is not modelled separately, set values
C       to zero
C----------------------------------------------------------------------
        fPeriod_duration(iComp,iOP_startup)
     &                     = ADATA ( iComp, iShift + 1 ) ! (s)

        iShift = iShift + 1


      endif

C----------------------------------------------------------------------
C     Shutdown period data - all models
C----------------------------------------------------------------------

      fPeriod_duration(iComp,iOP_shutdown)
     &                  = ADATA ( iComp, iShift +  1 ) ! (s)

      iCooldown_mode(iComp) = int( ADATA ( iComp, iShift +  2 ) ) ! (-)


      fPeriod_AC_Power(iComp, iOP_shutdown)
     &                  = ADATA ( iComp, iShift +  3 ) ! (W)

C.....Assume fuel flwo at start-up is zero
      fPeriod_fuel_flow(iComp,iOP_shutdown) = 0.0

      iShift = iShift + 3


C--------------------------------------------------------------------
C     Standby mode data
C--------------------------------------------------------------------

C.....Assume fuel flow and heat generaation during standby are zero
      fPeriod_fuel_flow(iComp,iOP_inoperative) = 0.0 ! (kg/s)

      fPeriod_Heat_Recovery(iComp,iOP_inoperative) = 0.0 ! (W)

      fPeriod_AC_Power(iComp, iOP_inoperative)
     &                  = ADATA ( iComp, iShift +  1 )   !(W)


      iShift = iShift + 1

C----------------------------------------------------------------------
C     Now collect remainder of Annex 42 CHP model performance
C     map inputs
C----------------------------------------------------------------------


C.....Performance map correlation minimum & maximum bounds
      fCorr_MIN_Power_OUT(iComp) =  ADATA ( iComp, iShift + 1 )
      fCorr_MAX_Power_OUT(iComp) =  ADATA ( iComp, iShift + 2 )

      fCorr_MIN_CW_temp(iComp)   =  ADATA ( iComp, iShift + 3 )
      fCorr_MAX_CW_temp(iComp)   =  ADATA ( iComp, iShift + 4 )

      fCorr_MIN_CW_flow(iComp)   =  ADATA ( iComp, iShift + 5 )
      fCorr_MAX_CW_flow(iComp)   =  ADATA ( iComp, iShift + 6 )

      iShift = iShift + 6

C.....Collect electrical efficiency performance map coefficients
      do ii = 1, iNumElecCoeff

        fPM_elec_coeff ( iComp, ii ) =
     &           ADATA ( iComp, iShift + ii )

      enddo

      iShift = iShift + iNumElecCoeff


C.....Collect thermal efficiency performance map coefficients
      do ii = 1, iNumTherCoeff

        fPM_ther_coeff ( iComp, ii ) =
     &           ADATA ( iComp, iShift + ii )

      enddo

      iShift = iShift + iNumTherCoeff

C.....Collect cooling water loop configuration and performance map flow
C.....coefficients

      iCW_Loop_configuration(iComp) = int ( ADATA(iComp, iShift + 1) )
      iShift = iShift + 1

      do ii = 1, iNumFlowCoeff

        fPM_flow_coeff ( iComp, ii ) =  ADATA ( iComp, iShift + ii )

      enddo

      iShift = iShift + iNumFlowCoeff

C.....Collect airflow performance map coefficients
      do ii = 1, iNumAirCoeff

        fPM_air_coeff ( iComp, ii ) =
     &           ADATA ( iComp, iShift + ii )

      enddo

      iShift = iShift + iNumAirCoeff

C.....Additional parameters go here.


      if (bFatal_error)
     &   stop
     &   'A42_CHP_Collect_Data (Annex42_combustion_CHP): Fatal error!'

      return
      end



C---------------- Eval_Annex42_CHP_Correlations -----------------------
C
C This procedure evaluates the 'performance map' correlations for
C Annex 42 combustion CHP model's electrical and thermal efficiency,
C and uses these data to determine the system's fuel flow, gross
C heat input and heat output.
C
C Inputs:
C
C   - iPComp: Index of plant component
C   - fP_net: Net power delivery at desired operating point.
C   - fCW_mass_flow: cooling water mass flow rate at future timerow
C   - fCW_temperature_in: cooling wate temperature at future time
C        row.
C
C Outputs:
C
C    - fN_fuel_flow: Fuel flow at desired operating point
C    - fQ_cogen: Gross heat generation in system.
C    - bCorrelation_error: flag indicating correlations return
C        an unexpected result.
C
C
C----------------------------------------------------------------------

      subroutine Eval_Annex42_CHP_Correlations (
     &                   iPComp,
     &                   fP_net,
     &                   fCW_flow,
     &                   fCW_temp,
     &                   fN_fuel_flow,
     &                   fQ_cogen,
     &                   fEfficiency_elec,
     &                   fEfficiency_ther,
     &                   bError )
      implicit none
#include "building.h"
#include "plant.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
C----------------------------------------------------------------------
C     Passed variables
C----------------------------------------------------------------------
      integer iPComp              ! Index of plant component
      real fP_net                 ! Net power output (W)
      real fCW_flow               ! Cooling water mass flow rate (kg/s)
      real fCW_temp               ! Cooling water inlet temp (oC)
      real fN_Fuel_Flow           ! Fuel flow rate (kg/s)
      real fN_air_flow            ! Air flow rate (kg/s)
      real fQ_cogen               ! Rate of heat generation (W)
      logical bError              ! Flag indicating correlations
                                  !   are returning an unexpected
                                  !   result.
C----------------------------------------------------------------------
C     Global variables
C----------------------------------------------------------------------

C.....Trace/error reporting unit numbers
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fEfficiency_elec       ! Electrical efficiency
      real fEfficiency_ther       ! Thermal efficiency

      real fCOPY_cw_flow          ! Copy of CW flow rate
      real fCOPY_cw_temp          ! Copy of CW temperature
      real fCOPY_P_net            ! Copy of net power

C.....warning buffer & flag
      logical bCorrelation_error(3) ! flag indicating error has occured
      character*128 cWarning_msg(3) ! Warning buffer
      logical bWarned(3)            ! flag for warning user
      save bWarned
      data bWarned / .false., .false., .false. /

      logical bClose_To_Zero_a    ! Flags for close-to-zero comparisons
      logical bClose_To_Zero_b

      integer iCounter

      bError = .false.
C----------------------------------------------------------------------
C     Check that correlation inputs (power/temperature/flow) are
C     within correlaion bounds, and adjust inputs if necesary to
C     to ensure correlations are not referenced out of bounds.
C
C     ??? Should we warn the user if the correlation's
C         inputs out of bounds ???
C
C----------------------------------------------------------------------

      fCOPY_cw_flow = fCW_flow
      fCOPY_cw_temp = fCW_temp
      fCOPY_P_net   = fP_net

      if ( fCOPY_cw_flow > fCorr_MAX_CW_flow ( iPComp ) )
     &     fCOPY_cw_flow = fCorr_MAX_CW_flow ( iPComp )

      if ( fCOPY_cw_flow < fCorr_MIN_CW_flow ( iPComp ) )
     &     fCOPY_cw_flow = fCorr_MIN_CW_flow ( iPComp )

      if ( fCOPY_cw_temp > fCorr_MAX_CW_temp ( iPComp ) )
     &     fCOPY_cw_temp = fCorr_MAX_CW_temp ( iPComp )

      if ( fCOPY_cw_temp < fCorr_MIN_CW_temp ( iPComp ) )
     &     fCOPY_cw_temp = fCorr_MIN_CW_temp ( iPComp )

      if ( fCOPY_P_net   > fCorr_MAX_Power_OUT ( iPComp ) )
     &     fCOPY_P_net   = fCorr_MAX_Power_OUT ( iPComp )

      if ( fCOPY_P_net   < fCorr_MIN_Power_OUT ( iPComp ) )
     &     fCOPY_P_net   = fCorr_MIN_Power_OUT ( iPComp )

C----------------------------------------------------------------------
C     Determine electrical efficiency using performance map
C     correlation. Use Equation 10 in Ferguson & Kelly 2006.
C
C     Note: The correlation coefficients are stored in array
C           fPM_elec_coeff. Each array location corresponds to
C           the coefficient's subscript, shifted by 1. Thus:
C
C             a0  -> fPM_elec_coeff ( iPComp,  1 )
C             a1  -> fPM_elec_coeff ( iPComp,  2 )
C             a2  -> fPM_elec_coeff ( iPComp,  3 )...
C
C             a26 -> fPM_elec_coeff ( iPComp, 27 )
C
C
C----------------------------------------------------------------------

      fEfficiency_elec =
     &
     &   fPM_elec_coeff ( iPComp,  1 )
     &
     & + fPM_elec_coeff ( iPComp,  2 ) * fCOPY_P_net ** 2.0
     &
     & + fPM_elec_coeff ( iPComp,  3 ) * fCOPY_P_net
     &
     & + fPM_elec_coeff ( iPComp,  4 ) * fCOPY_cw_flow ** 2.0
     &
     & + fPM_elec_coeff ( iPComp,  5 ) * fCOPY_cw_flow
     &
     & + fPM_elec_coeff ( iPComp,  6 ) * fCOPY_cw_temp ** 2.0
     &
     & + fPM_elec_coeff ( iPComp,  7 ) * fCOPY_cw_temp
     &
     & + fPM_elec_coeff ( iPComp,  8 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_flow ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp,  9 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_flow )
     &
     & + fPM_elec_coeff ( iPComp, 10 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_flow ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 11 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_flow )
     &
     & + fPM_elec_coeff ( iPComp, 12 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 13 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 14 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 15 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 16 )
     &      * ( fCOPY_cw_flow ** 2.0 ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 17 )
     &      * ( fCOPY_cw_flow ) * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 18 )
     &      * ( fCOPY_cw_flow ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 19 )
     &      * ( fCOPY_cw_flow ** 2.0 ) * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 19 )
     &      * ( fCOPY_cw_flow ** 2.0 ) * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 20 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 21 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 22 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 23 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 24 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 25 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp )
     &
     & + fPM_elec_coeff ( iPComp, 26 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_elec_coeff ( iPComp, 27 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp )


C----------------------------------------------------------------------
C     Determine thermal efficiency using performance map
C     correlation. Use Equation 11 in Ferguson & Kelly 2006.
C
C     Note: The correlation coefficients are stored in array
C           fPM_elec_coeff. Each array location corresponds to
C           the coefficient's subscript, shifted by 1. Thus:
C
C             a0  -> fPM_elec_coeff ( iPComp,  1 )
C             a1  -> fPM_elec_coeff ( iPComp,  2 )
C             a2  -> fPM_elec_coeff ( iPComp,  3 )...
C
C             a26 -> fPM_elec_coeff ( iPComp, 27 )
C
C
C----------------------------------------------------------------------

      fEfficiency_ther =
     &
     &   fPM_ther_coeff ( iPComp,  1 )
     &
     & + fPM_ther_coeff ( iPComp,  2 ) * fCOPY_P_net ** 2.0
     &
     & + fPM_ther_coeff ( iPComp,  3 ) * fCOPY_P_net
     &
     & + fPM_ther_coeff ( iPComp,  4 ) * fCOPY_cw_flow ** 2.0
     &
     & + fPM_ther_coeff ( iPComp,  5 ) * fCOPY_cw_flow
     &
     & + fPM_ther_coeff ( iPComp,  6 ) * fCOPY_cw_temp ** 2.0
     &
     & + fPM_ther_coeff ( iPComp,  7 ) * fCOPY_cw_temp
     &
     & + fPM_ther_coeff ( iPComp,  8 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_flow ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp,  9 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_flow )
     &
     & + fPM_ther_coeff ( iPComp, 10 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_flow ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 11 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_flow )
     &
     & + fPM_ther_coeff ( iPComp, 12 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 13 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 14 )
     &      * ( fCOPY_P_net ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 15 )
     &      * ( fCOPY_P_net ** 2.0 ) * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 16 )
     &      * ( fCOPY_cw_flow ** 2.0 ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 17 )
     &      * ( fCOPY_cw_flow ) * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 18 )
     &      * ( fCOPY_cw_flow ) * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 19 )
     &      * ( fCOPY_cw_flow ** 2.0 ) * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 19 )
     &      * ( fCOPY_cw_flow ** 2.0 ) * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 20 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 21 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 22 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 23 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 24 )
     &      * ( fCOPY_P_net ** 2.0 )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 25 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow ** 2.0 )
     &      * ( fCOPY_cw_temp )
     &
     & + fPM_ther_coeff ( iPComp, 26 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp ** 2.0 )
     &
     & + fPM_ther_coeff ( iPComp, 27 )
     &      * ( fCOPY_P_net )
     &      * ( fCOPY_cw_flow )
     &      * ( fCOPY_cw_temp )

C--------------------------------------------------------------------
C     Determine fuel flow rate necessary to produce requested power:
C
C                                     Power requested (W)
C     fuel flow (kg/s)  = ----------------------------------------
C                           efficiency (-) * Fuel LHV value (J/kg)
C
C     See Equations 2 & 4 in Ferguson and Kelly (2006)
C
C--------------------------------------------------------------------

      fN_fuel_flow = fP_net / fEfficiency_elec / fFuel_LHV (iPComp)


C--------------------------------------------------------------------
C     Determine generated heat:
C
C     Heat gen (W) = Fuel flow (kg/s)
C                           * fuel LHV value (J/kg)
C                           * thermal efficiency (-)
C
C     See Equations 3 & 4 in Ferguson and Kelly (2006)
C
C--------------------------------------------------------------------

      fQ_cogen = fN_fuel_flow * fFuel_LHV (iPComp) * fEfficiency_ther

C----------------------------------------------------------------------
C     Check that electrical and thermal efficiencies are
C     within acceptable bounds:
C        - Electrical efficiency must be greater than zero,
C        - Thermal efficiency must be greater than zero, and
C        - Combined electrical and thermal output does
C          not exceed fuel higher heating value.
C----------------------------------------------------------------------
      bCorrelation_error(1) = .false.
      bCorrelation_error(2) = .false.
      bCorrelation_error(3) = .false.

      call eclose ( fEfficiency_elec, 0.0,
     &              fClose_to_zero_tolerance, bClose_To_Zero_a )
      call eclose ( fEfficiency_ther, 0.0,
     &              fClose_to_zero_tolerance, bClose_To_Zero_b )


      if ( fEfficiency_elec < 0.0 .and. .not. bClose_To_Zero_a ) then
C.......Electrical efficiency is less than zero!
        bCorrelation_error(1) = .true.
        cWarning_msg(1) = 'Electrical efficiency is less than zero!'
      endif

      if ( fEfficiency_ther < 0.0 .and. .not. bClose_To_Zero_b ) then
C.......Thermal efficiency is less than zero!
        bCorrelation_error(2) = .true.
        cWarning_msg(2) = 'Thermal efficiency is less than zero!'
      endif


      if ( fP_net + fQ_cogen > fN_fuel_flow * fFuel_HHV(iPComp) ) then
C.......Combined electrical and thermal output exceeds fuel
C.......higher heating value.
        bCorrelation_error(3) = .true.
        cWarning_msg(3) = 'Combined electric and thermal output '
     &      // 'exceeeds fuel higher heating value!'
      endif

C----------------------------------------------------------------------
C      Issue warning if necesary
C----------------------------------------------------------------------
      if ( ( bCorrelation_error(1) .and. .not. bWarned(1) ) .or.
     &     ( bCorrelation_error(2) .and. .not. bWarned(2) ) .or.
     &     ( bCorrelation_error(3) .and. .not. bWarned(3) )      ) then

         write (ITU,*) ' '
         write (ITU,*)
     &        'WARNING: Annex 42 CHP model '
         write (ITU,'(A,I2,A)')
     &        '         (Component # ', iPComp, ')'
         write (ITU,*) ' '
         write (ITU,*)
     &        '         Performance map correlations '
         write (ITU,*)
     &        '         yielded invalid results for the '
         write (ITU,*)
     &        '         following conditions:             '

         write (ITU,'(A,g12.5,A)')
     &        '           - CW flow rate:   ', fCOPY_cw_flow, ' (kg/s)'
         write (ITU,'(A,g12.5,A)')
     &        '           - CW temperature: ', fCOPY_cw_temp, ' (oC)'
         write (ITU,'(A,g12.5,A)')
     &        '           - Net power:      ', fCOPY_P_net, ' (W)'
         write (ITU,'(A,g12.5,A)') ' '
         write (ITU,'(A,g12.5)')
     &        '           - Elec. efficiency: ', fEfficiency_elec
              write (ITU,'(A,g12.5)')
     &        '           - Ther. efficiency: ', fEfficiency_ther
        write (ITU,'(A,g12.5,A)')
     &        '           - Fuel HHV:         ', fFuel_HHV(iPComp),
     &                                        ' (W)'

        write (ITU,'(A,g12.5,A)')
     &        '           - Net power:        ', fP_net, ' (W)'
        write (ITU,'(A,g12.5,A)')
     &        '           - Heat generation:  ', fQ_cogen, ' (W)'

        do iCounter = 1, 3

          if ( bCorrelation_error ( iCounter ) ) then
            write (ITU,*)  ' '
            write (ITU,'(A,A)') '         ',cWarning_msg(iCounter)
            bWarned(iCounter) = .true.

          endif

        enddo

        write (ITU,'(A)')
     &       '         Unit will be deactivated on this timestep.'
        write (ITU,'(A)')
     &       '         Duplicate errors suppressed.'

        write (ITU,*) ' '

C.......Set unit outputs to zero

      endif ! <- matches 'if ( bCorrelation_error(1) ) then '

C----------------------------------------------------------------------
C     If any errors were encountered, return error flag
C----------------------------------------------------------------------
      if ( bCorrelation_error(1) .or.
     &     bCorrelation_error(2) .or.
     &     bCorrelation_error(3)      ) then

        bError = .true.

      else

        bError = .false.

      endif



      return
      end
C---------------- A42_CHP_H3Kreports_module --------------------------
C
C This procedure transports Data from the Stirling Engine model to
C the h3kreporting object.
C
C Inputs:
C   - iPComp: pointer to component in plant network
C   - iType: flag indicating the type of model in use
C   - iAction: flag indicating if reports should be initialized,
C              or if data can be transported to the h3kreports
C              module.
C
C Outputs:
C
C    None.
C
C-----------------------------------------------------------------------

      subroutine A42_CHP_H3Kreports_module (
     &                 iIndex, iStatus )
      implicit none
#include "plant.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
#include "CETC_definitions.h"

C.....Arguements
      integer iIndex
      integer iStatus

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

C.....Miscellaneous plant data
      common/pcvar/pctf,pcrf,puaf,pcqf,pcntmf,pctp,
     &      pcrp,puap,pcqp,pcntmp
      real pctf(mpcon)
      real pcrf(mpcon)
      real puaf(mpnode)
      real pcqf(mpnode)
      real pcntmf(mpcom)          ! future time row plant containment temp (oC)
      real pctp(mpcon)
      real pcrp(mpcon)
      real puap(mpnode)
      real pcqp(mpnode)
      real pcntmp(mpcom)

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
      real fEngine_Temp_F           ! Engine temperature
      real fRoom_Temp_F          ! Room temperature

      real fHeat_loss            ! Heat lost to room

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

      integer lnblnk             ! function returning length of a string

      if ( iStatus == iInitialize ) then

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

C.....Get Engine node temperature (oC)
      fEngine_Temp_F = CSVF ( iEngine_Node_index, iPropTemp )
C.....Get containment temperature (oC)
      fRoom_Temp_F = PCnTmF ( iIndex )

C.....Get HX Inlet temperature (oC). We need to use function
C.....H3K_Connect_property as the convar common is not updated until
C.....the next timestep
      fCW_Temp_In = H3K_Connect_property( iCW_conn_index, iPropTemp )

C.....Get connection spacific heat. Use function shtfld:
C.....  specific heat capactiy = Shtfld ( X , Temperature ) (J/kg oC)
C.....  where X=1 -> air, X=2 -> vapor, X=3 -> liq. water
      fCW_Cp = shtfld ( 3, ( fCW_Temp_In + fCW_Temp_Out ) / 2 )

C.....Calculate heat recovery
      fQ_recovery_average = fEffective_UA_HX ( iIndex )
     &     * ( fEngine_Temp_F - fCW_Temp_OUT )
C.....Calculate heat losses to surroundings (W)
      fHeat_loss = fEffective_UA_loss ( iIndex ) *
     &                ( fEngine_Temp_F - fRoom_Temp_F )

C.....Check if cooling water flow rate is zero.
      call eClose( fCW_Flow, 0.0, 1E-05, bClose_To_Zero )


C--------------------------------------------
C.....Output data: Control Scheme
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/external_control/scheme'
      call add_to_xml_reporting(
     &   fReport_Control_Method(iIndex),
     &   H3Kname,
     &   'units', '(-)',
     &   'A42 CHP model: External control scheme type'
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
     &   'A42 CHP model: External control signal'
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
     &   'A42 CHP model: Cumulative time spent inoperative'
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
     &   'A42 CHP model: Cumulative time spent in start-up mode'
     &   )

C-----------------------------------------
C.....Write a name: Time spent in warm-up
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/operating_mode/warmup'
      call add_to_xml_reporting(
     &   fReport_time_spent(iIndex,iOP_warmup),
     &   H3Kname,
     &   'units', '(s)',
     &   'A42 CHP model: Cumulative time spent warming up'
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
     &   'A42 CHP model: Cumulative time spent in normal operation'
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
     &   'A42 CHP model: Cumulative time spent in shut-down mode'
     &   )

C-----------------------------------------
C.....Write a name: Containment temperature
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/containment_temperature'
      call add_to_xml_reporting(
     &   fReport_Containment_Temp(iIndex),
     &   H3Kname,
     &   'units', '(oC)',
     &   'A42 CHP model: temperature of containing room'
     &   )

C-----------------------------------------
C.....Write a name: Cooling water temperature
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength), '/cooling_water/temperature'
      call add_to_xml_reporting(
     &   fReport_CW_Temperature(iIndex),
     &   H3Kname,
     &   'units', '(oC)',
     &   'A42 CHP model: temperature of cooling water at inlet'
     &   )



C-----------------------------------------
C.....Write a name: Cooling water mass flow rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength), '/cooling_water/flow_rate'
      call add_to_xml_reporting(
     &   fReport_CW_Mass_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'A42 CHP model: mass flow rate of cooling water'
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
     &   'A42 CHP model: net power produced'
     &   )

C-----------------------------------------
C.....Write a name: Heat recovery
      write(H3Kname,'(A,A)')
     &   char_temp(1:iNameLength),'/instantaneous/heat_recovery'
      call add_to_xml_reporting(
     &   fReport_Heat_Recovery(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'A42 CHP model: Rate of heat transfer to cooling water'
     &   )

C-----------------------------------------
C.....Write a name: Air flow
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/instantaneous/combustion_air/flow_rate'
      call add_to_xml_reporting(
     &   fReport_Air_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'A42 CHP model: combustion air flow rate'
     &   )


C-----------------------------------------
C.....Write a name: fuel flow
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/instantaneous/fuel/flow_rate'
      call add_to_xml_reporting(
     &   fReport_Fuel_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'A42 CHP model: fuel mass flow rate'
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
     &   'A42 CHP model: LHV flow rate of fuel used by engine'
     &   )

C-----------------------------------------
C.....Write a name: fuel LHV value:
C.....This data should be time-invariant, but there's presently
C.....no better way to export time-invariant data out of ESP-r.
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/instantaneous/fuel/lower_heating_value'
      call add_to_xml_reporting(
     &   fFuel_LHV(iIndex),
     &   H3Kname,
     &   'units', '(J/kg)',
     &   'A42 CHP model: LHV of fuel used by engine'
     &   )


C-----------------------------------------
C.....Write a name: Skin losses
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),
     &  '/instantaneous/heat_loss'
      call add_to_xml_reporting(
     &   fHeat_loss,
     &   H3Kname,
     &   'units', '(W)',
     &   'A42 CHP model: rate of heat loss to surroundings'
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
     &   'A42 CHP model: Rate of heat transfer to cooling water'
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
     &   'A42 CHP model: Rate of net electrical production averaged'
     &     // 'over timestep' )

C-----------------------------------------
C.....Write a name: fuel flow
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/average/fuel/flow_rate'
      call add_to_xml_reporting(
     &   fAverage_Fuel_Flow(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'A42 CHP model: Fuel mass flow rate averaged over timestep'
     &   )

C-----------------------------------------
C.....Write a name: fuel LHV flow rate
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/average/fuel/gross_energy_input'
      call add_to_xml_reporting(
     &   fAverage_Fuel_LHV_rate(iIndex),
     &   H3Kname,
     &   'units', '(W)',
     &   'A42 CHP model: LHV of fuel used by engine, averaged over'
     &      //' timestep' )

C-----------------------------------------
C.....Write a name: carbon dioixide emissions
      write(H3Kname,'(A,A)')
     &  char_temp(1:iNameLength),'/average/emissions/carbon_dioxide'
      call add_to_xml_reporting(
     &   fAverage_CO2_emissions(iIndex),
     &   H3Kname,
     &   'units', '(kg/s)',
     &   'A42 CHP model: carbon dioxide emissions, averaged over'
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
     &      'A42 CHP model: Electrical efficiency'
     &        // ' averaged over timestep' )

C-----------------------------------------
C........Write a name: thermal efficiency
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),'/average/efficiency/thermal'
         call add_to_xml_reporting(
     &      fAverage_Thermal_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'A42 CHP model: Thermal efficiency averaged over '
     &        // 'timestep')


C-----------------------------------------
C........Write a name: cogeneration efficiency
         write(H3Kname,'(A,A)')
     &      char_temp(1:iNameLength),'/average/efficiency/cogeneration'
         call add_to_xml_reporting(
     &      fAverage_Cogen_efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'A42 CHP model: Cogeneration (overall) efficiency'
     &       // ' averaged over timestep')


C........Write a name: Cogeneration efficency
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/instantaneous/efficiency/cogeneration'
         call add_to_xml_reporting(
     &      fReport_Cogen_Efficiency(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'A42 CHP model: Cogeneration (overall) efficiency '
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
     &      'A42 CHP model: Electrical efficiency'
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
     &      'A42 CHP model: Electrical efficiency'
     &       // 'at end of timestep')

      endif

C--------------------------------------
C........Write a name: Solution weighting factor
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/solution_params/weighting_factor'
         call add_to_xml_reporting(
     &      fReport_alpha(iIndex),
     &      H3Kname,
     &      'units', '(-)',
     &      'A42 CHP model: Implicit/explicit solution'
     &       // ' weighting factor.')

C--------------------------------------
C........Write a name: engine control volume TC 
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/solution_params/time_constant/engine_control_volume'
         call add_to_xml_reporting(
     &      fReport_TC_eng(iIndex),
     &      H3Kname,
     &      'units', '(s)',
     &      'A42 CHP model: Engine control volume time constant')

C--------------------------------------
C........Write a name: cooling water control volume TC 
         write(H3Kname,'(A,A)')
     &     char_temp(1:iNameLength),
     &     '/solution_params/time_constant/cw_control_volume'
         call add_to_xml_reporting(
     &      fReport_TC_cw(iIndex),
     &      H3Kname,
     &      'units', '(s)',
     &      'A42 CHP model: Cooling water control volume time constant')

      return
      end  ! of subroutine


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
C     iStrategy     -  Flag indicating if step will be accomplished 
C                      as quickly as possible, or linearly
C                      between two points.
C
C
C  Outputs:
C     fX_average    -  Time-step averaged value of variable X
C     fX_final      -  Value of variable X at end of timestep.
C     fX_limit      -  Upper limit of rate of change.
C     bLimited      -  Flag indicating that change in variable
C                      is rate limited.
C
C  Assumptions:
C
C     - Variable X exhibits linear behavior between initial and 
C       final values.
C
C     If stepwise strategy is specified, allow variable to change 
C     at maximum rate until future value is reached, afterwhich
C     the remainder of the timestep is spend at steady-state.
C         
C         ^
C         |
C         |
C         |
C         |                
C      Y2=+              +  +  +  +  +  +  +  +  
C         |             +
C         |            +
C         |           +
C         |          +
C         |         +
C         |        +
C         |       +
C         |      +
C      Y1-+     +
C         |
C         |
C         +--------------------------------------->
C               |<- Rate->|<----   Steady    --->|
C                 limited          state
C
C     If linear strategy is specified, assume variable will vary
C     linarly across timestep 
C         
C         ^
C         |
C         |
C         |                / <- max rate-limited change
C         |               / 
C      Y2=+              /                 +  <- Actual 
C         |             /                +       rate of 
C         |            /              +          change.
C         |           /            +
C         |          /          +
C         |         /        +
C         |        /      + 
C         |       /    +
C         |      /  +
C      Y1-+     +
C         |
C         |
C         +--------------------------------------->
C
C
C----------------------------------------------------------------------
      subroutine Eval_TS_average(
     &           fX_initial,
     &           fX_target,
     &           fX_max_change,
     &           fTS_duration,
     &           iStrategy,
     &           fX_average,
     &           fX_final,
     &           fX_limit,
     &           bLimited        )
      implicit none
#include "plant.h"
#include "cetc_cogen.h"
#include "Annex42_combustion_CHP.h"
C----------------------------------------------------------------------
C     Passed variables
C----------------------------------------------------------------------
      real fX_initial,fX_target,fX_max_change,fX_average,fX_final,
     &     fTS_duration, fX_limit
      integer iStrategy
      logical bLimited
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
      if ( fX_initial <= fX_target ) then
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
C     - If maximum rate of change is less than zero,
C       assume that variable exhibits no dynamic characteristics
C       (ie time=0)
C----------------------------------------------------------------------

      if ( fX_max_change < 0.0 ) then

         fLag_Time = 0.0  ! (s)
         bTransients = .false.

      else

C---------------------------------------------------------------------
C       Rate of change in X: assume that X changes at max possible rate
C---------------------------------------------------------------------

        if ( fX_initial <= fX_target ) then
           fX_rate_change = fX_max_change
        else
           fX_rate_change = -1 * fX_max_change
        endif

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
      call eclose (fTS_duration, 0.0, 1E-06,bNums_are_close)

      if ( .not. bNums_are_close ) then
        fTS_frac_transient = fLag_Time / fTS_duration
      else
        fTS_frac_transient = 0.0
      endif

C.....Is lag time greater than 1?
      if ( fTS_frac_transient > 1.0 ) fTS_frac_transient = 1.0

C.....Fraction of timestep in steady-state
      fTS_frac_steady = 1.0 - fTS_frac_transient

C----------------------------------------------------------------------
C     Calculate the value of variable X at the end of the
C     time-step
C
C        X_final = X_initial + ( rate of change )
C                                * (transient fraction) * (TS duration)
C
C        X_Limit = X_imitial + ( rate of change )
C                                * ( TS duration )
C
C----------------------------------------------------------------------

      if ( bTransients ) then

         fX_final = fX_initial
     &            + fX_rate_change * fTS_frac_transient * fTS_duration

         fX_limit =  fX_initial + fX_rate_change * fTS_duration

      else

         fX_final = fX_target
         fX_limit = -1.0

      endif

C----------------------------------------------------------------------
C     Is variable rate limited? Check if fTS_frac_steady = 0,
C     and set flag bLimited if so.
C     ( ie. will unit spend all of its time in transient? )
C----------------------------------------------------------------------
      call eclose ( fTS_frac_steady, 0.0, fClose_to_zero_tolerance,
     &              bLimited )


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

C.....Reset steady-state and transient fractions for linear case:
C.....  Variable will spend all of its time in transient.
      if ( iStrategy == iLinear ) then
        fTS_frac_transient = 1.0
        fTS_frac_steady = 0.0
      endif 
        

      fX_average = (fX_initial + fX_final) / 2.0 * fTS_frac_transient
     &             + fX_final * fTS_frac_steady


      return
      end

