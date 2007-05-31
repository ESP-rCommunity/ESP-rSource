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
C================= CETC_cogen.F =======================================
C
C Date:       June 8, 2005
C Author:     Alex Ferguson
C Copyright:  Natural Resources Canada 2005.

C This file contains general procedures associated with modelling
C cogeneration devices
C
C It contains the following procedures:
C
C    - Determine_Cogen_Mode: Procedure determining the fraction
C        of each timestep that the model spends in standby,
C        startup, normal operation, and cool down modes.
C
C    - Determine_Cogen_OP: Generic interface calling model-specific
C        characterization routines (in this case, it calls
C        Char_H2_PEMFC_Empirical )
C
C    - Solve_Cogen_Model: Procedure used to implicitly solve
C        a cogeneration model and determine the fuel flow
C        corresponding to a given heat or power output.
C
C    - bLinearize: Returns a linear equation (y=ax+b)
C        of a line passing through two supplied points
C
C======================================================================
C======================================================================

C---------------- Solve_Cogen_Model() ---------------------------------
C
C This procedure is used to implicitly solve the Hydrogen PEMFC
C model and determine the fuel flow that corresponds to a given
C amount of electrical or thermal output. It is a modular
C implementation of code copied form the Stirling CHP model
C (Stirling_CHP.F), and the Stirling CHP model should be
C modified to reference this procedure instead, as time permits.
C
C Two procedures are used to implicitly solve the model:
C Newton's method, and Biscection Method. Using newton's method,
C the unit's heat and power corresponding to two different fuel
C flows are determined, and a linear function describing the
C change in heat/power over this region is obtained. This
C function is solved to estimate the unit's fuel flow
C corresponding to the heat/power output requested by the
C controller. The procedure is repeated until successive
C estimates of fuel flow converge.
C
C If newton's method fails to converge, biscection method is
C used instead. In bisection method, the system's operating range
C is evaluated using two bounds corresponding to its maximum
C and minimum fuel flow rates. The system's operaing point is
C evaluated at the midpoint of this range. If the calculated
C heat/power output is found to be greater than that requested
C by the controller, the upper bound is set equal to the midpoint.
C and the procedure is repeated. Otherwise, the lower bound is
C set equal to the midpoint and the procedure repeated. Iteration
C is adjourned when the upper and lower bounds are deemed to be
C sufficietly close.
C
C While bisection method is slower than newton's method, it
C has the following advangages:
C
C  -> it always converges.
C  -> if it fails to find a fuel flow rate correspoding to
C     the requested value, it will return the approprate maximum
C     or minumum fuel flow rate.
C
C Inputs:
C
C   - iComponent: Index of component in plant network
C   - iType: Type of model to be used.
C   - iControl_Method: flag indicating if electric or thermal output
C        is to be used to determine system operating point
C   - fControl_signal: electric/thermal output requested by controller.
C
C Outputs:
C
C   - fFuel_flow: system's calculated fuel flow (kg/s)
C   - fHeat_output: system's calculated thermal output  (W)
C   - fPower_output: system's calculated electrical output (W)
C
C----------------------------------------------------------------------
      subroutine Solve_Cogen_Model(
     &                iComponent,
     &                iModel_Type,
     &                iControl_Method,
     &                fOP_target,
     &                fFuel_flow,
     &                fCW_temperature_in,
     &                fCW_mass_flow,
     &                fTemp_Room,
     &                fAccum_time,
     &                fPower_output,
     &                fHeat_output   )
      implicit none

      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'

C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------


      integer iComponent      ! index of component in plant
                                    !   network
      integer iModel_Type           ! flag indicating which model is
                                    !   to be used.

      integer iControl_Method       ! flag indicating which control scheme is
                                    !     in use.

      real fFuel_flow               ! Calcuated fuel flow (kg/s)
      real fOP_target               ! Power/heat output requested by controller (W)
      real fCW_mass_flow            ! Cooling water flow (kg/s)
      real fCW_temperature_in       ! Cooling water temp at inlet (oC)
      real fTemp_Room               ! Ambient temperature (oC)
      real fAccum_time              ! Accumulated operating time (s)

      real fHeat_Output             ! Calcuated heat output (W)
      real fPower_Output            ! Calculate power output (W)


C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
C.....Intermediate variables used for implicit solution of model equations
      real fFuel_flow_LB       ! Fuel flow at lower iteration bound (kg/s)
      real fFuel_flow_UB       ! Fuel flow at upper iteration bound (kg/s)
      real fFuel_flow_LB_norm  ! Normalized fuel flow at lower bound (kg/s)
      real fFuel_flow_UB_norm  ! Normalized fuel flow at upper bound (kg/s)
      real fFuel_flow_new      ! Fuel flow predicted by most recent (kg/s)
                               !   iteration

      real fFuel_flow_linear_range ! Range (kg/s) over which fuel flow will be
                                   !   linearized

      real fHeat_Output_LB       ! Heat output at lower iteration bound (W)
      real fHeat_Output_UB       ! Heat output at upper iteration bound (W)
      real fPower_Output_LB      ! Power output at lower iteration bound (W)
      real fPower_Output_UB      ! Power output at upper iteration bound (W)

C.....Coolinng water data

      !real fCW_temperature_out   ! Cooling water temperature at outlet (oC)

C.....Linearized heat/power output parameters
      real fIntercept            ! line y-intercept
      real fSlope                ! line slope

C.....Iteration controllers
      logical bUnconverged
      logical bStable
      integer iIteration_count
      integer iMax_iterations
      parameter ( iMax_iterations = 10 )


C.....Compare two mumbers
      logical bNums_are_close

C----------------------------------------------------------------------
C     Named constants
C----------------------------------------------------------------------
      real fConvergence_tolerance
      parameter ( fConvergence_tolerance = 1.0E-04 )
C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      logical bLinearize        ! returns slope & intercept of straight
                                !    line described by two points

C----------------------------------------------------------------
C.....Initialize iteration controls.
      bUnconverged = .true.   ! iteration convergance control
      bStable      = .true.   ! solution method control
      iIteration_count = 0
C.....Initialize fuel flow to midpoint between max and min.
      fFuel_flow = fFuel_Flow_MIN ( iComponent )
     &    + 0.5 * (  fFuel_Flow_MAX ( iComponent )
     &              -fFuel_Flow_MIN ( iComponent )
     &            )

      do while ( bUnconverged )
C----------------------------------------------------------------------
C        First try newton's method, with linearization.
C        -> Use control bStable to denote if iteration is
C           stable/unstable
C-----------------------------------------------------------------------
         if ( bStable ) then
C...........NEWTON's METHOD.
C...........Determine linearization range for fuel flow:
            fFuel_flow_linear_range = 0.025
C...........Bounds for linearization
            fFuel_Flow_UB = fFuel_Flow
     &                   * ( 1.0 +fFuel_flow_linear_range )
            fFuel_Flow_LB = fFuel_Flow
     &                   * ( 1.0 -fFuel_flow_linear_range )

C...........Check bounds do not exceed min/max ranges of operation.
            if ( fFuel_Flow_LB
     &            .LT. fFuel_Flow_MIN (iComponent) ) then
               fFuel_Flow_LB = fFuel_Flow_MIN(iComponent)
               fFuel_Flow_UB = fFuel_Flow_MIN(iComponent)
     &                    * ( 1.0 + 2.0 * fFuel_flow_linear_range)
            elseif ( fFuel_Flow_UB
     &            .GT. fFuel_Flow_MAX(iComponent) ) then
               fFuel_Flow_UB = fFuel_Flow_MAX(iComponent)
               fFuel_Flow_LB = fFuel_Flow_MAX(iComponent)
     &                    * ( 1.0 - 2.0 * fFuel_flow_linear_range)
            endif
C-------------------------------------------------------------------
C           Create linearized expressions. Use function Determine_OP
C           to characterize the model at both fuel flow linearization
C           bounds.
C-------------------------------------------------------------------

C...........Lower bound (fuel flow = fFuel_flow_LB )
            call Determine_cogen_OP (
     &                           iComponent,
     &                           iModel_Type,
     &                           fFuel_flow_LB,
     &                           fCW_temperature_in,
     &                           fCW_mass_flow,
     &                           fTemp_Room,
     &                           fAccum_time,
     &                           fPower_Output_LB,
     &                           fHeat_Output_LB )


C...........Upper bound ( fuel flow = fFuel_flow_UB )
            call Determine_cogen_OP (
     &                           iComponent,
     &                           iModel_Type,
     &                           fFuel_flow_UB,
     &                           fCW_temperature_in,
     &                           fCW_mass_flow,
     &                           fTemp_Room,
     &                           fAccum_time,
     &                           fPower_Output_UB,
     &                           fHeat_Output_UB )


C-------------------------------------------------------------------
C           If electrical or thermal output is to be controlled,
C           create linear approximation correlating output to
C           fuel flow rate.
C
C           Logical function bLinearize is used to obtain the
C           linear approximation. It returns the slobe and intercept
C           for the linear equation y = Ax+B, when given two data
C           points (x1,y1) & (x2,y2). The funcion's value is set to
C           true if no errors were encountered, and false othewise.
C
C           The choice of the output variable (ie y1, y2) depends
C           on whether electrical or thermal load following is to
C           be effected.
C
C           The dependent variable, fuel flow, is normalized
C           since its value can be very small.
C-------------------------------------------------------------------
            fFuel_flow_LB_norm = fFuel_flow_LB
     &               / fFuel_Flow_MAX ( iComponent )
            fFuel_flow_UB_norm = fFuel_flow_UB
     &               / fFuel_Flow_MAX ( iComponent )

            if ( iControl_method .eq. iPower_out_controlled ) then
C..............Electrical load following:
C..............   y1 = net power output at lower bound
C..............   y2 = net power output at upper bound
               if ( .not. bLinearize (
     &                    fFuel_flow_LB_norm, fPower_Output_LB,
     &                    fFuel_flow_UB_norm, fPower_Output_UB,
     &                    fslope, fIntercept )
     &             ) then
               ! error trap here (Fuel flows must be equal)
               endif

            elseif (iControl_method
     &         .eq. iHeat_out_controlled) then
C..............Thermal load following:
C..............   y1 = heat output at lower bound
C..............   y2 = heat output at upper bound
               if ( .not. bLinearize (
     &                 fFuel_flow_LB_norm, fHeat_Output_LB,
     &                 fFuel_flow_UB_norm, fHeat_Output_UB,
     &                 fslope, fIntercept )
     &             ) then
                  ! error trap here (Fuel flows must be equal)
               endif

            endif

C-------------------------------------------------------------------
C           Solve linear equation for fuel flow delivering desired
C           operating point.
C
C           Equation:
C
C              Control signal  =  slope * fuel flow + intercept
C
C           Solution is:
C
C              fuel flow =  ( Control signal - intercept ) / slope
C
C           Remember, normalized fuel flow is used to find the
C           linearized equation. Therefore, multiply by maximum
C           fuel flow to obtain actual flow.
C-------------------------------------------------------------------
C...........Check that slope is not zero!
            call eclose ( fSlope, 0.0, 1.0E-06, bNums_are_close )
            if ( bNums_are_close ) then
C..............Mark solution as unstable
               bStable = .false.
            else
               fFuel_flow_new = (fOP_target - fIntercept)
     &                  / fslope
     &                  * fFuel_Flow_MAX ( iComponent )
            endif
C--------------------------------------------------------------------
C           Ensure that calculated fuel flow is within system bounds.
C--------------------------------------------------------------------
            if (fFuel_flow_new .lt.
     &          fFuel_Flow_MIN (iComponent))
     &          fFuel_flow = fFuel_Flow_MIN (iComponent)
            if (fFuel_flow_new .gt.
     &          fFuel_Flow_MAX (iComponent))
     &          fFuel_flow = fFuel_Flow_MAX (iComponent)
C--------------------------------------------------------------------
C           Check for convergence / stability (Compare normalized
C           values for fuel flow)
C--------------------------------------------------------------------
            call eclose (
     &        fFuel_flow_new
     &              / fFuel_Flow_MAX ( iComponent ),
     &        fFuel_flow
     &              / fFuel_Flow_MAX ( iComponent ),
     &        fConvergence_tolerance,
     &        bNums_are_close )

            if ( bstable .and. bNums_are_close ) then

C..............Solution has converged
               bUnconverged = .false.

C..............Set fuel flow to new value, evaluate operating point
C..............at final fuel flow.
               fFuel_flow = fFuel_flow_new
               call Determine_cogen_OP (
     &                           iComponent,
     &                           iModel_Type,
     &                           fFuel_flow,
     &                           fCW_temperature_in,
     &                           fCW_mass_flow,
     &                           fTemp_Room,
     &                           fAccum_time,
     &                           fPower_Output,
     &                           fHeat_Output)

            else
C..............Solution has not converged            
               if ( .not. bStable .or.
     &              iIteration_count .gt. iMax_iterations ) then
C.................Assume iteration is unstable. Switch to
C.................bisection method
                  bStable = .false.
                  fFuel_flow_LB =
     &                   fFuel_Flow_MIN(iComponent)
                  fFuel_flow_UB =
     &                   fFuel_Flow_MAX(iComponent)
               else
C.................Update result, and iterate again
                  fFuel_flow = fFuel_flow_new
                  iIteration_count = iIteration_count + 1
               endif
            endif
            
         else  ! <- matches if ( bStable ) then ...
         
C--------------------------------------------------------------------
C           Newton's method is unstable. Switch to more robust
C           bisection method. Note: search bounds (fFuel_flow_LB &
C           fFuel_flow_UB) are initialized above when bStable flag
C           is set to false!
C
C           Calculate midpoint from lower and upper bounds
C--------------------------------------------------------------------
            fFuel_flow =
     &           ( fFuel_flow_LB + fFuel_flow_UB ) / 2.0
C...........Determine operating point corresponding to midpoint
C...........using procedure Determine_OP
            call Determine_cogen_OP (
     &                           iComponent,
     &                           iModel_Type,
     &                           fFuel_flow,
     &                           fCW_temperature_in,
     &                           fCW_mass_flow,
     &                           fTemp_Room,
     &                           fAccum_time,
     &                           fPower_Output,
     &                           fHeat_Output)


C...........Check for convergence (compare normalized values
C...........of upper & lower bounds of fuel flow)
            call eclose ( fFuel_flow_LB
     &                       / fFuel_Flow_MAX(iComponent),
     &                    fFuel_flow_UB
     &                       / fFuel_Flow_MAX(iComponent),
     &                    fConvergence_tolerance,
     &                    bNums_are_close )

            if  ( bNums_are_close ) then
C..............Solution has converged
               bUnconverged = .false.
            else
C..............Iterate again. Update bounds.
               if ( iControl_method
     &                .eq. iPower_out_controlled ) then
C.................Compare electrical output to target operating
C.................point (fOP_target)
                  if ( fPower_Output .gt. fOP_target ) then
                     fFuel_flow_UB = fFuel_flow
                  else
                     fFuel_flow_LB = fFuel_flow
                  endif

               else
C.................Compare thermal output to target operating
C.................point (fOP_target)
                  if ( fHeat_Output
     &                      .gt. fOP_target ) then
                     fFuel_flow_UB = fFuel_flow
                  else
                     fFuel_flow_LB = fFuel_flow
                  endif
               endif
            endif ! <- matches 'if ( bNums_are_close ...'
         endif    ! <- matches 'if ( bStable ) then ...'
      enddo       ! <- matches 'do while ( bUnconverged ) ...'

      return
      end

C---------------- Determine_Cogen_OP() -------------------------------
C
C This procedure is called to characterize the operation of a
C cogeneration model in response to the system fuel flow,
C cooling water temperature & flow rate, and ambient temperature.
C
C It is merely an interface to model-specific routines that
C handle the characteriation of each cogeneration mdoel.
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
C
C----------------------------------------------------------------------
      subroutine Determine_Cogen_OP (
     &              iComponent,
     &              iModel_Type,
     &              fFuel_flow,
     &              fCW_temperature_in,
     &              fCW_mass_flow,
     &              fTemp_Room,
     &              fAccum_time,
     &              fPower_Output,
     &              fHeat_Output )
      implicit none

      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent      ! index of component in plant
                                    !   network
      integer iModel_Type           ! flag indicating which model is
                                    !   to be used.

      real fFuel_flow               ! Calcuated fuel flow (kg/s)
      real fOP_target               ! Power/heat output requested by controller (W)
      real fCW_mass_flow            ! Cooling water flow (kg/s)
      real fCW_temperature_in       ! Cooling water temp at inlet (oC)
      real fTemp_Room               ! Ambient temperature (oC)
      real fAccum_time              ! Unit's accumulated operating time (s)

      real fHeat_Output             ! Calcuated heat output (W)
      real fPower_Output            ! Calculate power output (W)

C----------------------------------------------------------------------
C     Outputs from model-specific procedures that are not used
C     in the current context.
C----------------------------------------------------------------------
      real fVoltage, fCurrent, fSkin_Losses, fAux_Power_use, fQ_avail

C----------------------------------------------------------------------
C     Check supplied model type, and call appropriate model procedure
C----------------------------------------------------------------------
      if ( iModel_Type .eq. iH2_PEMFC_empirical ) then
C........Empirical Hydrogen PEMFC model.
         call Char_H2_PEMFC_Empirical (
     &              iComponent,
     &              fFuel_flow,
     &              fCW_temperature_in,
     &              fCW_mass_flow,
     &              fTemp_Room,
     &              fAccum_time,
     &              fPower_Output,
     &              fHeat_Output,
     &              fVoltage,
     &              fCurrent,
     &              fSkin_Losses,
     &              fAux_Power_use  )

      elseif ( iModel_Type .eq. iStirling_CHP_DYN ) then
C........Empirical Dynamic Stirling model
         call Eval_Stirling_Corr (
     &                iComponent,
     &                fFuel_flow,
     &                fTemp_Room,
     &                fCW_mass_flow,
     &                fCW_temperature_in,
     &                fSkin_Losses,
     &                fQ_avail,
     &                fPower_Output,
     &                fHeat_output )

      else

C........can't happen, as iModel_Type is set programically
         STOP 'Determine_Cogen_OP(): unknown model.'

      endif

      return
      end


C---------------- Determine_Cogen_Mode() -------------------------------
C
C This procedure is used to determine a cogeneration device's respose
C to high-level controls. It considers the unit's startup and cool-down
C times, and determines what fraction of the timestep the unit will
C spend in each operating mode.
C
C This procedure is a more modular implementation of code developed for
C the Stirling engine CHP model. As time permits, the Stirling model
C should be adapted to use this structure and the duplicate code
C eliminated.
C
C
C Inputs:
C
C   - iComponent: Index of component in plant network
C   - iOperating_mode: Unit's current operating mode
C   - iSystem_Status: flag indicating if fault has occured.
C   - fOP_time: Time unit has spent in current mode
C   - iTarget_Mode: Flag indicating which operating mode
C        the controller is requesting
C   - bDebug:  Flag indicating that debugging messages should be streamed
C       to the buffer
C
C Outputs:
C
C  - fTS_mode_fractions: Fraction of timestep spent in each mode
C  - bStartup-active: flag indicating that startup period will
C       be completed on this timestep
C
C----------------------------------------------------------------------
      subroutine Determine_Cogen_Mode(
     &              iComponent,
     &              iOperating_mode_P,
     &              iSystem_Status,
     &              fOP_time,
     &              iTarget_Mode,
     &              iOperating_mode_F,
     &              fTS_mode_fractions,
     &              bTS_modes,
     &              bStartup_complete,
     &              bDebug )
      implicit none

      INCLUDE 'building.h'
      INCLUDE 'plant.h'
      INCLUDE 'cetc_cogen.h'

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent      ! Index of plant component in network
      integer iControl_method       ! Flag indicating which control method
                                    !    is in use

      integer iSystem_status        ! Flag indicating if system fault has
                                    !    occured.

      integer iOperating_mode_P     ! Flag indicating unit's current
                                    !    operating mode on the present time
                                    !    row
      integer iOperating_mode_F     ! Flag indicating unit's future operating
                                    !    mode on the future time row

      real fOP_time                 ! Time spent in current operating mode (s)
      integer iTarget_Mode            ! Flag indicating desired operating
                                    !    mode

      real fTS_mode_fractions( iOP_Mode_Count )
                                    ! Fraction of timestep spent in
                                    !    each operating mode

      logical bTS_modes( iOP_Mode_Count )
                                    ! logical flags indicating which operating
                                    !    modes the system will spend time in

      logical bStartup_complete     ! Flag indicating that startup period
                                    !     will be completed on this timestep

      logical bDebug                ! Flag indicating debugging output
                                    !     should be dumped to buffer.


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

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fTS_duration          ! length of timestep (s)
      real fTS_fraction_reqd     ! fraction of timestep required
                                 !    to complete startup/shutdown
      real fTS_fracion_complete  ! fraction of timestep that has lapsed.
      real fTS_fraction_remain   ! fraction of timestep that has not
                                 !    been characterized
C.....Controls for operating point search
      logical bOP_mode_found     ! has OP been found?
      logical bTS_done           ! has timestep been fully characterized?

      integer ii                 ! counter

      logical bNums_are_close    ! result of close-to-zero comparisons




C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      logical bTS_complete       ! function determining if
                                 !   timestep has been fully
                                 !   characterized


C----------------------------------------------------------------------


      if ( bDebug ) write(itu,*)
     &       '   -> Entering procedure Determine_Cogen_OP() '

C.....Assign present time row operating mode to future time-row.
      iOperating_mode_F = iOperating_mode_P

C.....Time step duration (s)
      fTS_duration = 3600.0 / ( ntstep * ntstpp )

C.....Set loop controls
      bOP_mode_found  = .false.
      bStartup_complete = .false.
C.....Zero arrys containing fraction of time-step spent in each mode
      do ii = 1, iOP_Mode_Count
         fTS_mode_fractions(ii) = 0.0
      enddo

C.....Check system status for faults, and adjust target operating
C.....point to 'inactive' if any are found
      if ( iSystem_Status .ne. iStatus_normal )
     &     iTarget_Mode = iOP_inoperative

C----------------------------------------------------------------------
C     Determine if system can get from present operating mode to
C     target mode on this timestep.
C----------------------------------------------------------------------
      do while ( .not. bOP_mode_found )
C----------------------------------------------------------------------
C        Use function bTS_complete to determine if system's operating
C        point has been characterized over the entire timestep.
C----------------------------------------------------------------------
         bTS_done = bTS_complete (fTS_mode_fractions,
     &                   fTS_fraction_remain )

         if ( iOperating_Mode_F .eq. iTarget_mode ) then
C----------------------------------------------------------------------
C           Target operating mode has been reached.
C----------------------------------------------------------------------
            bOP_mode_found = .true.
            fTS_mode_fractions(iOperating_Mode_F)= fTS_fraction_remain
            fOP_time = fOP_time + fTS_mode_fractions(iOperating_Mode_F)
     &             * fTS_duration

C----------------------------------------------------------------------
C        Is timestep complete?
C----------------------------------------------------------------------
         elseif ( bTS_done ) then
C----------------------------------------------------------------------
C           Target operating mode will not be reached during current
C           timestep.
C----------------------------------------------------------------------
            bOP_mode_found = .true.
C...........Calculate fraction of timestep spent in current operating
C...........mode.

            fOP_time = fOP_time + fTS_mode_fractions(iOperating_Mode_F)
     &             * fTS_duration

         else
C----------------------------------------------------------------------
C           Switch operating points depending on system status
C----------------------------------------------------------------------
            if ( iOperating_Mode_F .eq. iOP_inoperative .AND.
     &           iTarget_mode    .eq. iOP_normal_operation ) then

C..............Switch to start-up mode
               iOperating_Mode_F = iOP_startup
               fOP_time  = 0.0

            elseif ( iTarget_mode    .eq. iOP_inoperative  .AND.
     &               iOperating_Mode_F .eq. iOP_normal_operation  ) then

C..............Initiate shutdown
               iOperating_Mode_F = iOP_shutdown
               fOP_time = 0.0

            elseif ( iOperating_Mode_F .eq. iOP_startup .AND.
     &               iTarget_mode    .eq. iOP_normal_operation ) then
C----------------------------------------------------------------------
C              System is in start-up. Will engine complete start-up
C              period before completion of timestep?
C
C              Fraction of      (start-up time) - (completed time)
C              TS required   =  ----------------------------------
C              for start-up              TS duration
C
C----------------------------------------------------------------------
               fTS_fraction_reqd =
     &            ( fperiod_duration(iComponent,iOP_startup)
     &               - fOP_time ) / fTS_duration
C----------------------------------------------------------------------
C              Is time required for start-up greater than time-step
C              duration?
C----------------------------------------------------------------------

               call eclose ( fTS_fraction_reqd ,
     &                       fTS_fraction_remain,
     &                       fCogen_zero_tolerance,
     &                       bNums_are_close )

               if ( bNums_are_close .or.
     &              ( fTS_fraction_reqd .le.
     &                fTS_fraction_remain ) ) then
C----------------------------------------------------------------------
C                 Start-up will be completed on this time step.
C                 - Switch mode to 'normal operation', and update
C                   'fTS_fracion_complete' variable to reflect the
C                   amount of time spent in start-up.
C----------------------------------------------------------------------
                  iOperating_Mode_F = iOP_normal_operation
                  fOP_time = 0.0
                  fTS_mode_fractions (iOP_startup) = fTS_fraction_reqd

C----------------------------------------------------------------------
C                 Set startup-active flag. Note: this flag is used
C                 to initialize variables prior to the start-up->normal
C                 operation transition. If the unit will not
C                 complete the start-up period on this timeste, these
C                 operations need not be performed
C----------------------------------------------------------------------
                  bStartup_complete = .true.

               else
C----------------------------------------------------------------------
C                 Start-up will not be completed on this time step.
C                 - Leave unit in start-up mode, and calculate
C                   the fraction of this timestep spent in start-up.
C                 - Update  'fTS_fracion_complete' variable to
C                   mark timestep as 'complete'.
C----------------------------------------------------------------------
                  fTS_mode_fractions(iOP_startup)= fTS_fraction_remain

               endif

            elseif ( iOperating_Mode_F .eq. iOP_startup .AND.
     &               iTarget_mode    .eq. iOP_inoperative ) then

C..............Switch unit into shutdown.
               iOperating_Mode_F = iOP_shutdown
               fOP_time = 0.0

            elseif ( iOperating_Mode_F .eq. iOP_shutdown ) then
C----------------------------------------------------------------------
C              System is in shutdown Will engine complete start-up
C              period before completion of timestep?
C
C              Fraction of      (shutdown time) - (completed time)
C              TS required   =  ----------------------------------
C              for shutdown              TS duration
C
C
C              NOTE: Shutdown MUST be completed before startup
C                    can be initiated and operation can resume.
C
C----------------------------------------------------------------------
               fTS_fraction_reqd = (
     &            fPeriod_duration(iComponent,iOP_shutdown)
     &              - fOP_time ) / fTS_duration

C----------------------------------------------------------------------
C              Is time required for start-up greater than time-step
C              duration?
C----------------------------------------------------------------------

               call eclose ( fTS_fraction_reqd ,
     &                       fTS_fraction_remain,
     &                       fCogen_zero_tolerance,
     &                       bNums_are_close )

               if ( bNums_are_close .or.
     &              ( fTS_fraction_reqd .lt.
     &                fTS_fraction_remain ) ) then
C----------------------------------------------------------------------
C                 Shutdown will be completed on this time step.
C                 - Switch mode to 'inoperative', and update
C                   'fTS_mode_fractions' array to reflect the
C                   amount of time spent in shutdown.
C----------------------------------------------------------------------
                  iOperating_Mode_F = iOP_inoperative
                  fOP_time = 0.0
                  fTS_mode_fractions (iOP_shutdown) = fTS_fraction_reqd

               else
C----------------------------------------------------------------------
C                 Shutdown will not be completed on this time step.
C                 - Leave unit in start-up mode, and calculate
C                   the fraction of this timestep spent in shutdown.
C----------------------------------------------------------------------
                  fTS_mode_fractions(iOP_shutdown)= fTS_fraction_remain

               endif

            else
C..............Can't happen. All possibilities included above.
               STOP
     &         "Determine_Cogen_OP() ERROR: operating mode not found"
            endif   ! <- matches 'if ( iOperating_Mode_F .eq. ...'
         endif      ! <- matches 'if ( iOperating_Mode_F .eq. ...'
      enddo         ! <- matches 'do while ( .not. bOP_mode_found )'


C----------------------------------------------------------------------
C     Set flags indicating which modes system is operating in
C----------------------------------------------------------------------
      do ii = 1, iOP_Mode_Count
         call eclose (fTS_mode_fractions(ii), 0.0,
     &                fCogen_zero_tolerance, bNums_are_close)
         if ( .not. bNums_are_close
     &        .and. fTS_mode_fractions(ii).gt.0.0) then
            bTS_modes (ii) = .true.
         else
            bTS_modes (ii) = .false.
         endif
      enddo

      if ( bDebug ) write(itu,*)
     &       '  <-  Leaving procedure Determine_Cogen_OP() '

      return
      end




            
C------------------- bLinearize ---------------------------------------
C
C function returning a linear equation [y(x)=Ax+b] for two
C data points describing (presumably) non-linear data over
C small values of dx
C
C Inputs:
C   two data points described by (X1,Y1) and (X2,Y2)
C
C Outputs
C   coefficients A and b
C   status: true for ok, false for error
C
C----------------------------------------------------------------------
      logical function bLinearize ( X1, Y1, X2, Y2, A, b)
      implicit none


      real  X1, Y1, X2, Y2, A, b
      real fTolerance
      parameter (fTolerance = 1.0e-10)
      logical bNums_are_close

C----------------------------------------------------------------------
C     Check that X1, X2 are not equal to each other.
C----------------------------------------------------------------------
      call eclose ( X1, X2, fTolerance, bNums_are_close )
      if ( bNums_are_close ) then
C........Return error
         bLinearize = .false.
      else
C........Calculate slope:
         A = (Y2-Y1) / ( X2 - X1 )
C........Calculate Y intercept  (Solve y = Ax + b for x = X1, y=Y1)
         b = Y1 - A * X1
C........Set status
         bLinearize = .true.
      endif

      return
      end



C------------------ bTS_complete -------------------------------------
C
C Very simple function that returns true if the system's operation
C has been characterized over the entirity of the current timestep.
C Also returns the fraction remaining in the current time step.
C
C Inputs:
C
C    fTS_mode_fractions(x) -> Array containing fraction of timestep
C                             spent in each operating model
C    
C Outputs:
C
C    bTS_complete -> logical variable indicating if timestep is complete
C
C    fTS_frac_remaining -> fraction of timestep remaining ( = 0.0 if
C                          bTS_complete = true )
C
C----------------------------------------------------------------------

      logical function bTS_complete ( fTS_mode_fractions,
     &                                fTS_frac_remaining )
      implicit none

      INCLUDE 'plant.h'      
      INCLUDE 'cetc_cogen.h'

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------      
      real fTS_mode_fractions (iOP_Mode_Count ) ! array containing frac.
                                     ! of timestep spent in each mode.

      real fTS_frac_remaining        ! fraction of timestep remaining.


C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------

      real fTS_frac_passed           ! Fraction of timestep that has passed

      integer ii                     ! counter

   
      real fClose_to_zero_tolerance 
      parameter ( fClose_to_zero_tolerance = 1.0E-06 )
C----------------------------------------------------------------------
C     Determine fraction of timestep that has passed.
C----------------------------------------------------------------------      
      fTS_frac_passed = 0.0
      
      do ii = 1, iOP_Mode_Count

         fTS_frac_passed = fTS_frac_passed + fTS_mode_fractions(ii)

      enddo

C----------------------------------------------------------------------
C     Check if entire timestep has lapsed.
C----------------------------------------------------------------------
      call eclose ( fTS_frac_passed, 1.0, fClose_to_zero_tolerance,
     &              bTS_complete )

      if ( bTS_complete .OR. fTS_frac_passed .GT. 1.0 ) then
         fTS_frac_passed = 1.0
         bTS_complete = .true.
      endif
      
      fTS_frac_remaining = 1.0 - fTS_frac_passed
      
      return
      end
      