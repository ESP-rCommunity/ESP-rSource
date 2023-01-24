C-----------------------------------------------------------------------------------------
C This is a customized controller for purposes of modelling a PV-powered system with
C hydrogen production, storage, and conversion.  Unlike conventional ESP-r controllers
C this controller senses the state of numerous plant and electrical domain components
C and actuates numerous plant and electrical domain components.  Its structure is highly
C customized to a specific system configuration.  It is anticipated that in the future a
C more generalized control structure will be established to provide this functionality
C and thus supplant this controller.
C
C This controller is invoked from ENETSOLV at the beginning of the execution of the
C electric domain.  Consequently, it senses the state of electric domain components
C from the previous solution of the electric domain.  If bps is iterating within the
C time-step between the plant and electrical domains to converge a solution, then the
C controller is sensing the state of the electric domain components at the current
C time-step.  Otherwise, if this is the first pass through the electric domain for the
C current time-step then the controller is sensing the state of electric domain components
C at the previous time-step.  Since the plant domain is solved prior to the invocation
C of the electric domain, the controller is sensing the state of plant components at the
C current time-step.
C
C The controller actuates a number of plant domain components.  If one of these control
C actions results in a different operational mode for a plant component, a flag is set
C that forces another iteration through the plant and electric domains at the current
C time-step.
C-----------------------------------------------------------------------------------------
      SUBROUTINE RESH2_Ctl
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"


C-----------------------------------------------------------------------------------------
C Input file information
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

C Common block storing control and other data for plant components.
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP          !-number of plant components
      INTEGER NCI             !-number of possible control variables for component
      REAL    CDATA           !-array containing control data values

C Common block storing additional time-row data items for plant domain.
      common/pcdat/pcdatf(mpcom,mpcdat),pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)


C Power-only component information specific to RE-H2 Ctl.
      common/resh2_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery,IPWC_PEM_DCDC,IPWC_Elec_DCDC
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component
      integer   IPWC_PEM_DCDC   !-index number of the DC/DC converter between the PEMFC
                                !-and the DC bus
      integer   IPWC_Elec_DCDC  !-index number of the DC/DC converter between the electrolyzer
                                !-and the DC bus

C Plant component information
      common/resh2_plt/IPCOMP_electrolyzer,IPCOMP_PEMFC,
     &                 IPCOMP_elec_pump, IPCOMP_PEM_pump,
     &                 fElec_pump_flow, fPEM_pump_flow
      INTEGER   IPCOMP_electrolyzer  ! Plant domain component number of electrolyzer.
      INTEGER   IPCOMP_PEMFC         ! Plant domain component number of PEMFC.
      integer   IPCOMP_elec_pump     !-index number of the electrolyzer cooling pump component
      integer   IPCOMP_PEM_pump      !-index number of the PEMFC cooling pump component
      real      fElec_pump_flow      !-flow rate of the electrolyzer cooling pump (m3/s)
      real      fPEM_pump_flow       !-flow rate of the PEMFC cooling pump (m3/s)

C Electric node information
      common/resh2_enode/IENOD_ACbus
      INTEGER IENOD_ACbus          ! Elec domain node number of AC bus.

C Electrolyzer and PEMFC setpoints based upon battery SOC.
      common/resh2_soc/SOC_electrolyzer_ON,SOC_electrolyzer_OFF,
     &                 SOC_pemfc_ON,SOC_pemfc_OFF
      real      SOC_electrolyzer_ON   !-battery SOC at which electrolyzer turns on
      real      SOC_electrolyzer_OFF  !-battery SOC at which electrolyzer switches off
      real      SOC_pemfc_ON          !-battery SOC at which PEM fuel cell switches on
      real      SOC_pemfc_OFF         !-battery SOC at which PEM fuel cell switches off

C Controller flag information
      common/resh2_comp_status/bPV_active,bElectrolyzer_active,
     &                    bPEM_active, bBattery_charge,
     &                    bBattery_discharge

C Flags indicating status of controller
      logical bPV_active,bElectrolyzer_active,bPEM_active,
     &                    bBattery_charge, bBattery_discharge

C Vbls used to regulate iteration between demains
      common / resh2_ctl_status / bResH2_ctl_unconverged
      logical bResH2_ctl_unconverged    !-logical flag indicating additional iterations req'd
C      data bResH2_ctl_unconverged/.false./ -> replaced by blockdata at end of this file


C Common block holding information on the battery's current operation.
      COMMON/battery_op_data/maxPBatCharge_common(MPOWCOM),
     &   maxPBatDischarge_common(MPOWCOM), RE_H2_control_scenario
      REAL maxPBatCharge_common      ! Maximum charge rate of battery (W) as determined
                                     ! for mode of operation at previous calulcation (charge or discharge).
      REAL maxPBatDischarge_common   ! Maximum discharge rate of battery (W) as determined
                                     ! for mode of operation at previous calulcation (charge or discharge).
      INTEGER RE_H2_control_scenario ! Param indicating which control scenario is chosen

C Other controller inputs
      common/resh2/fH2_deadband
      real      fH2_deadband          !-controller deadband for H2 production (W). This
                                      !-value is applied so that the electrolyzer only switches on
                                      !-when the excess PV power is greater than power to meet
                                      !-the loads + power allowance for compressor and pumps,
                                      !-The power allowance for compressor and pumps is referred
                                      !-to as the H2 deadband.
C Variable from H2 storage model
      common/resh2_stg/bCyl_H2_max_reached, fCyl_reported_pressure
      logical bCyl_H2_max_reached    ! Flag indicating that maximum pressure
                                     ! in storage reached.
      real fCyl_reported_pressure    ! Variable used to report current
                                     ! pressure to compressor


C for debugging
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER IHRP            !-hour of present time-step
      INTEGER IHRF            !-hour of future time-step
      INTEGER IDYP            !-year day number of present day
      INTEGER IDYF            !-year day number of future day
      INTEGER IDWP            !-day of the week of present day
      INTEGER IDWF            !-day of the week of future day
      INTEGER NSINC           !-number of building-side time increments
                              !-since start of simulation
      INTEGER ITS,idynow      !-current building time-step within
                              !-current hour

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER IUOUT           !-write unit number
      INTEGER IUIN            !-read unit number
      INTEGER IEOUT           !-error unit number

      real  check
C end debugging


C-----------------------------------------------------------------------------------------
C Type declarations for local variables.
C-----------------------------------------------------------------------------------------

      REAL PV_power_to_DCbus       ! PV-produced power that is added to DC bus (W).
      REAL load_ACbus              ! Power load on AC bus (W).
      REAL DCAC_eff                ! Efficiency of DC-AC converter.
      REAL power_DC_to_AC_required ! Power required to DC-AC converter to meet load on AC bus (W).
      REAL Electrolyzer_ON_p       ! ON/OFF status of electrolyzer at present time row.
      REAL Electrolyzer_standby_p  ! STANDBY status of electrolyzer at present time row.
      REAL fPEMFC_ON_p             ! Operating signal of PEM at present time row.

      REAL SOC_battery             ! battery state of charge.

      REAL PEM_DCDC_loss          ! power losses from the DC-DC converter between the PEMFC and the DC bus (W)
      REAL Elec_DCDC_loss         ! power losses from the DC-DC converter between the
                                  ! electrolyzer and the DC bus (W)

      LOGICAL bPEMFC_ON_p          !- boolean indicating if PEM is on at present time row.

      real fPEM_mode_old         !-Previous PEMFC control scheme
      real fPEM_signal_old       !-Previous PEMFC control signal

      logical bPEM_same_mode, bPEM_same_signal !-flags indicating that signals have
                                               !-not changed

      real fElectrolyzer_on_old        !- Previous electrolyzer on signal
      real fElectrolyzer_standby_old   !- Previous electrolyzer standby signal
      real fElectrolyzer_signal_old    !- Previous electrolyzer control signal

      logical bElectrolyzer_same_on, bElectrolyzer_same_standby  !- flags indicating that
                                                                 !- signals have not changed

      logical bElec_same_signal, bBat_same_signal

      real    fBattery_signal_old      !- Previous battery demand

      real    fH2_prod_allowance       !- Allowance for power draw of compressor and
                                       !- electrolyzer cooling pump (W). This value is set
                                       !- to zero if electrolyzer previously on, otherwise
                                       !- set to deadband input by user.

      real    fSOC_battery_max         !- Maximum SOC of battery (%). This variable is only
                                       !- used for PV-battery only case (no H2 loop). User
                                       !- input specified using SOC_electrolyzer_OFF input
                                       !- in control file.

      real    fSOC_battery_min         !- Minimum SOC of battery (%). This variable is only
                                       !- used for PV-battery only case (no H2 loop). User input
                                       !- input specified using SOC_pemfc_OFF input in control file.

      logical bPV_bat                  !- Boolean indicating that a PV-battery only system (no
                                       !- H2 loop) is being considered. This is indicated by
                                       !- an input > 100 for the SOC_electrolyzer_ON variable
                                       !- and an input < 0 for the SOC_pemfc_ON variable in the
                                       !- control file.

C-----------------------------------------------------------------------------------------
C     Check if controller is in use
C-----------------------------------------------------------------------------------------
      if ( iResH2Ctl .ne. 1 ) then
C........Controller is not in use. Set flag preventing iteration
         bResH2_ctl_unconverged = .false.
      else
C........Controller is in use.


C-----------------------------------------------------------------------------------------
C     Check if PV-battery only system (no H2 loop) is being considered. This is the case
C     if the SOC_electrolyzer_ON setpoint is > 100 and if the SOC_pemfc_ON setpoint is
C     < 0. Set flag bPV_bat to true if this is the case and read battery max and min
C     SOC setpoints (specified using SOC_electrolyzer_OFF and SOC_pemfc_OFF setpoints,
C     respectively.
C-----------------------------------------------------------------------------------------

C Initialize boolean and battery max/min setpoints

        bPV_bat = .false.
        fSOC_battery_max = 100.
        fSOC_battery_min = 0.

        if ( SOC_electrolyzer_ON .gt. 100.0 .and.
     &       SOC_pemfc_ON .lt. 0.0 ) then

            bPV_bat = .true.
            fSOC_battery_max = SOC_electrolyzer_OFF
            fSOC_battery_min = SOC_pemfc_OFF

        endif


C-----------------------------------------------------------------------------------------
C     Initialize component status flags to false. They will be set to true when
C     the components are activated.
C-----------------------------------------------------------------------------------------
        bPV_active           = .false.
        bElectrolyzer_active = .false.
        bPEM_active          = .false.
        bBattery_charge      = .false.
        bBattery_discharge   = .false.
C-----------------------------------------------------------------------------------------
C     Get historical PEMFC control data.
C-----------------------------------------------------------------------------------------
         fPEM_mode_old = CDATA(IPCOMP_PEMFC,1)
         fPEM_signal_old = CDATA(IPCOMP_PEMFC,2)

C-----------------------------------------------------------------------------------------
C     Get historical electrolyzer control data
C-----------------------------------------------------------------------------------------
         fElectrolyzer_on_old = CDATA(IPCOMP_electrolyzer,1)
         fElectrolyzer_standby_old = CDATA(IPCOMP_electrolyzer,2)
         fElectrolyzer_signal_old = CDATA(IPCOMP_electrolyzer,3)

C----------------------------------------------------------------------------------------
C     Get historical battery data
C----------------------------------------------------------------------------------------

        fBattery_signal_old = batDemandP(IPWC_battery)

C-----------------------------------------------------------------------------------------
C Sense power generation from PCU that is connected between PV and DC bus.  This
C is the PV-produced power that is added to the DC bus.  This is the value at the
C previous iteration through the electric domain (perhaps the same time-step).
C IPWC_PVPCU is the index number of the PCU powoc (i.e. the sensed component in the
C electrical domain).
C-----------------------------------------------------------------------------------------

         PV_power_to_DCbus = fPow_Time_Row_Future(IPWC_PVPCU,1)


C-----------------------------------------------------------------------------------------
C Sense the loading on the AC bus from the previous iteration.
C IENOD_ACbus is the node number of the AC bus.
C The load or generation of the grid-connection powoc model should be excluded.
C IPWC_gridconn is the index number of the grid-connection powoc.
C Note that the function Elec_Net_Calculator cannot be used to evaluate the load
C on the AC bus because this function sums the loads and generation
C for all powoc and hybrid components on the entire electrical network.  In this case
C I want to evaluate only the loading on a single node.
C-----------------------------------------------------------------------------------------

         load_ACbus = ABS(PENODL(IENOD_ACbus))
C                                - PPOWOC(IPWC_gridconn) ! - for load; UNCOMMENT WHEN GRID CONNECTED COMPONENT ADDED



C-----------------------------------------------------------------------------------------
C Determine how much power was required (at the previous iteration) to be supplied
C by the DC bus to the AC-DC converter to meet the load on the AC bus.  This is not
C necessarily how much power flowed along this path because there may have been grid
C interaction on the AC bus.
C IPWC_ACDC is the index number of the AC-DC converter PCU powoc.
C-----------------------------------------------------------------------------------------

         DCAC_eff = fPow_Time_Row_Future(IPWC_ACDC,2)    ! Eff of DC-AC converter.
         if ( DCAC_eff .eq. 0. ) DCAC_eff = 1.           ! avoid divide by zero error if
                                                         ! DC/AC inverter is idle
         power_DC_to_AC_required = load_ACbus / DCAC_eff


C-----------------------------------------------------------------------------------------
C Determine the power losses of the DC-DC converter between the PEMFC and the DC bus.
C These losses will have to be included in the control signal to the PEMFC so that
C the output from the DC-DC converter matches the load the PEMFC has to meeet.
C-----------------------------------------------------------------------------------------

          PEM_DCDC_loss = fPow_Time_Row_Future(IPWC_PEM_DCDC,3)     ! Power losses of PCU

C-----------------------------------------------------------------------------------------
C Determine the power losses of the DC-DC converter between the electrolyzer and the
C DC bus. These losses will be taken into account in the control signal to the
C electrolyzer so that the input to the DC-DC converter matches the excess power
C that is available.
C-----------------------------------------------------------------------------------------

        Elec_DCDC_loss = fPow_Time_Row_future(IPWC_Elec_DCDC,3)

C-----------------------------------------------------------------------------------------
C Determine the battery's SOC at the previous iteration and establish the
C following:
C  1) If there is excess PV-produced power on the DC bus whether this
C     power will be directed to the battery or to the electrolyzer.
C  2) If there is a deficit of PV-produced power on the DC bus whether
C     the battery or the PEMFC will supply the deficit.
C-----------------------------------------------------------------------------------------

         SOC_battery = fPow_Time_Row_Future(IPWC_battery,1)


C-----Establish the operational state of the electrolyzer during the previous pass
C-----through the plant domain (i.e. at the current time-step).
C-----IPCOMP_electrolyzer is the index number of the electrolyzer plant component and
C-----IPCOMP_PEMFC is the index number of the PEMFC plant component.

         Electrolyzer_ON_p      = PCDATP( IPCOMP_electrolyzer,1 )  ! =1. for ON
         Electrolyzer_standby_p = PCDATP( IPCOMP_electrolyzer,2)   ! =1. for STANDBY


C-----Establish the operational state of the PEMFC during the previous pass through
C-----the plant domain (i.e. at the current time-step).
C-----PEMFC is on if value is 2 or 3.
         fPEMFC_ON_p = PCDATP(IPCOMP_PEMFC,2)
         bPEMFC_ON_p = .false.        ! initialize
         if ( fPEMFC_ON_p .eq. 2. .or. fPEMFC_ON_p .eq. 3. ) then
            bPEMFC_ON_p = .true.
         endif

C-----Determine allowance for power draw of compressor and electrolyzer cooling pump.
C-----The surplus PV power should be sufficient to meet the power demand of these
C-----ancillaries. If the electrolyzer was previously on, then power
C-----draw of the compressor and cooling pump are already part of the AC load; in this case
C-----allowance is set to zero. If the electrolyzer was not previously on, then allowance
C-----should be set to the 'H2 deadband' input by the user.
      if ( Electrolyzer_ON_p > 0.99 ) then
           fH2_prod_allowance = 0.
      else
           fH2_prod_allowance = fH2_deadband
      endif



C-----Determine if there is a surplus or deficit of power from the PV and actuate
C-----the electrolyzer and PEMFC accordingly.
         IF ( PV_power_to_DCbus > power_DC_to_AC_required ) THEN

C-------There is an excess of PV power. Send to battery and/or electrolyzer?
          IF ( SOC_battery > SOC_electrolyzer_ON .and.
     &         ( PV_power_to_DCbus >
     &           (power_DC_to_AC_required + fH2_prod_allowance) ) ) THEN
C---------The battery is too full: send to electrolyzer if pressure
C---------in H2 storage is below maximum pressure. CDATA(ipcomp,1) is ON/OFF signal.
C---------If maximum pressure in H2 storage reached, electrolyzer does not turn on
C---------and excess power goes to grid.
            RE_H2_control_scenario = 1
            batDemandP(IPWC_battery) = 0.
            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = 0.

            if ( .not. bCyl_H2_max_reached ) then
               CDATA( IPCOMP_electrolyzer,1 ) = 1.
               CDATA( IPCOMP_electrolyzer,2 ) = 0.
               CDATA( IPCOMP_electrolyzer,3 ) = PV_power_to_DCbus -
     &                                       power_DC_to_AC_required -
     &                                       Elec_DCDC_loss
               CDATA( IPCOMP_elec_pump,1 ) = fElec_pump_flow
               bElectrolyzer_active = .true.
            else
               CDATA( IPCOMP_electrolyzer,1 ) = 0.
               CDATA( IPCOMP_electrolyzer,2 ) = 0.
               CDATA( IPCOMP_electrolyzer,3 ) = 0.
               CDATA( IPCOMP_elec_pump,1 ) = 0.
            endif

          ELSEIF ( SOC_battery > SOC_electrolyzer_OFF
     &            .and. Electrolyzer_ON_p > 0.99
     &            .and. ( PV_power_to_DCbus >
     &           (power_DC_to_AC_required + fH2_prod_allowance) ) ) THEN

C---------Electrolyzer was on previous time-step and battery has not discharged sufficiently
C---------to turn electrolyzer off: send power to electrolyzer if pressure in H2 storage
C---------is below maximum pressure.
C---------If maximum pressure in H2 storage reached, electrolyzer does not turn on
C---------and excess power goes to grid.
            RE_H2_control_scenario = 2
            batDemandP(IPWC_battery) = 0.
            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = 0.

            if ( .not. bCyl_H2_max_reached ) then
               CDATA( IPCOMP_electrolyzer,1 ) = 1.
               CDATA( IPCOMP_electrolyzer,2 ) = 0.
               CDATA( IPCOMP_electrolyzer,3 ) = PV_power_to_DCbus -
     &                                       power_DC_to_AC_required -
     &                                       Elec_DCDC_loss
               CDATA( IPCOMP_elec_pump,1 ) = fElec_pump_flow
               bElectrolyzer_active = .true.
            else
               CDATA( IPCOMP_electrolyzer,1 ) = 0.
               CDATA( IPCOMP_electrolyzer,2 ) = 0.
               CDATA( IPCOMP_electrolyzer,3 ) = 0.
               CDATA( IPCOMP_elec_pump,1 ) = 0.
            endif

C The following code should be uncommented if it is decided that the PEMFC should turn off
C anytime there is excess power, even if the battery SOC is less then the pemfc_OFF setpoint

C          ELSEIF ( SOC_battery < SOC_pemfc_OFF
C     &             .and. bPEMFC_ON_p ) THEN
C---------PEMFC was previously on but battery SOC is not high enough to switch PEMFC off:
C---------send excess power to battery, keep PEMFC on to meet load
C            batDemandP = -1. * PV_power_to_DCbus            ! -ve = charge
C            CDATA( IPCOMP_electrolyzer,1 ) = 0.
C            CDATA( IPCOMP_electrolyzer,2 ) = 0.
C            CDATA( IPCOMP_electrolyzer,3 ) = 0.
C            CDATA( IPCOMP_PEMFC,1) = 2.
C            CDATA( IPCOMP_PEMFC,2) = power_DC_to_AC_required +
C     &                               PEM_DCDC_loss


          ELSEIF (   (PV_power_to_DCbus - power_DC_to_AC_required
     &                                  - fH2_prod_allowance )
     &             > ( maxPBatCharge_common(IPWC_battery)
     &                                    + Elec_DCDC_loss )  ) THEN
C---------Battery cannot accept all available power. Rather than sending the excess
C---------power to the grid, turn electrolyzer on and send it the power that the
C---------battery cannot accept. Electrolyzer only turned on if pressure in H2 storage
C---------is below maximum pressure.
C---------If maximum pressure in H2 storage reached, electrolyzer does not turn on
C---------and excess power goes to grid.

            RE_H2_control_scenario = 3
            batDemandP(IPWC_battery) = -1.
     &                             * maxPBatCharge_common(IPWC_battery)     ! -ve = charge

            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = 0.
            bBattery_charge = .true.

            if ( .not. bCyl_H2_max_reached ) then
               CDATA( IPCOMP_electrolyzer,1 ) = 1.
               CDATA( IPCOMP_electrolyzer,2 ) = 0.
               CDATA( IPCOMP_electrolyzer,3 ) = PV_power_to_DCbus -
     &                                       power_DC_to_AC_required -
     &                                       Elec_DCDC_loss
     &                              - maxPBatCharge_common(IPWC_battery)
               CDATA( IPCOMP_elec_pump,1 ) = fElec_pump_flow
               bElectrolyzer_active = .true.
            else
               CDATA( IPCOMP_electrolyzer,1 ) = 0.
               CDATA( IPCOMP_electrolyzer,2 ) = 0.
               CDATA( IPCOMP_electrolyzer,3 ) = 0.
               CDATA( IPCOMP_elec_pump,1 ) = 0.
            endif

C--------For a grid connected PV-battery system without H2 loop, the excess electricity
C--------will be exported to the grid. This is done through the battery model, so all
C--------power available will be offered to the battery. The battery model will sort out
C--------the amount it can use and what is exported to the grid.
C--------The current situation is not ideal as the battery is located before the DC/AC convertor.
C--------Future code changes to improve this situation are welcomed.
C            if ( bPV_bat ) then
C               batDemandP = -1. * (PV_power_to_DCbus -
C     &                            power_DC_to_AC_required)     ! -ve = charge
C            endif

          ELSE
C---------Send all the power to battery and set electrolyzer to standby. CDATA(ipcomp,2) is standby signal.
            RE_H2_control_scenario = 4
            batDemandP(IPWC_battery) = -1. * (PV_power_to_DCbus -
     &                         power_DC_to_AC_required)  ! -ve = charge
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 1.
            CDATA( IPCOMP_electrolyzer,3 ) = 0.
            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.
            CDATA( IPCOMP_elec_pump,1 ) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = 0.
            bBattery_charge = .true.
          ENDIF ! Corresponds to 'IF ( SOC_battery > SOC_electrolyzer_ON ) THEN'.

       ELSE

C-------There is insufficient PV power. Decide whether PEMFC or battery meets the load.
          IF ( SOC_battery < SOC_pemfc_ON ) THEN
C---------The battery has insufficient energy: PEMFC supplies power. The load the PEMFC has to
C---------meet is provided by the control signal CDATA(ipcomp,2). The PEMFC is actuated
C---------by the control signal CDATA(ipcomp,1).
            RE_H2_control_scenario = 5
            CDATA(IPCOMP_PEMFC,1) = 2.             ! iPower_out_controlled
            CDATA(IPCOMP_PEMFC,2) = power_DC_to_AC_required -
     &                              PV_power_to_DCbus +
     &                              PEM_DCDC_loss
            batDemandP(IPWC_battery) = 0.
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_electrolyzer,3 ) = 0.
            CDATA( IPCOMP_elec_pump,1 ) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = fPEM_pump_flow
            bPEM_active = .true.

          ELSEIF ( SOC_battery < SOC_pemfc_OFF
     &           .and. bPEMFC_ON_p  ) THEN
C---------The battery has not been charged sufficiently: PEMFC continues to supply power. The
C---------load the PEMFC has to meet is provided by the control signal CDATA(ipcomp,2).
C---------The PEMFC is actuated by the control signal CDATA(ipcomp,1).
            RE_H2_control_scenario = 6
            batDemandP(IPWC_battery) = 0.
            CDATA(IPCOMP_PEMFC,1) = 2.             ! iPower_out_controlled
            CDATA(IPCOMP_PEMFC,2) = power_DC_to_AC_required -
     &                              PV_power_to_DCbus +
     &                              PEM_DCDC_loss
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_electrolyzer,3 ) = 0.
            CDATA( IPCOMP_elec_pump,1 ) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = fPEM_pump_flow
            bPEM_active = .true.

          ELSEIF ( ( power_DC_to_AC_required - PV_power_to_DCbus ) >
     &               maxPBatDischarge_common(IPWC_battery) ) then

C---------The battery cannot supply all of the load. Battery supplies maximum it can and PEMFC
C---------is turned on to meet the rest of the load.
            RE_H2_control_scenario = 7
            batDemandP(IPWC_battery) =
     &                          maxPBatDischarge_common(IPWC_battery)
            CDATA(IPCOMP_PEMFC,1) = 2.             ! iPower_out_controlled
            CDATA(IPCOMP_PEMFC,2) = power_DC_to_AC_required -
     &                              PV_power_to_DCbus
     &                          - maxPBatDischarge_common(IPWC_battery)
     &                          + PEM_DCDC_loss
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_electrolyzer,3 ) = 0.
            CDATA( IPCOMP_elec_pump,1 ) = 0.
            IF ( .NOT. bPV_bat ) THEN
              CDATA( IPCOMP_PEM_pump,1 ) = fPEM_pump_flow
            ELSE
              CDATA( IPCOMP_PEM_pump,1 ) = 0.
            ENDIF
            bPEM_active = .true.
            bBattery_discharge = .true.

C--------For a grid connected PV-battery system without H2 loop, the shortage of electricity
C--------will be imported from the grid. This is done through the battery model, so all
C--------power required will be asked from the battery. The battery model will sort out
C--------the amount it can provide itself and what is imported from the grid.
C--------The current situation is not ideal as the battery is located before the DC/AC convertor.
C--------Future code changes to improve this situation are welcomed.
C            if ( bPV_bat ) then
C               batDemandP = power_DC_to_AC_required - PV_power_to_DCbus
C            endif

          ELSE


C---------The battery is charged sufficiently and supplies power, the PEMFC is off.
            RE_H2_control_scenario = 8
            batDemandP(IPWC_battery) = power_DC_to_AC_required
     &                                       - PV_power_to_DCbus   ! +ve = discharge
            CDATA(IPCOMP_PEMFC,1) = 0.             ! iEngine_deactivated
            CDATA(IPCOMP_PEMFC,2) = 0.0
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_electrolyzer,3 ) = 0.
            CDATA( IPCOMP_elec_pump,1 ) = 0.
            CDATA( IPCOMP_PEM_pump,1 ) = 0.
            bBattery_discharge = .true.




          ENDIF ! Corresponds to 'IF ( SOC_battery < SOC_pemfc_ON ) THEN'.


        ENDIF ! Corresponds to 'IF ( PV_power_to_DCbus > power_DC_to_AC_required ) THEN'.


C The next lines of code ensure that the electrolyzer and PEMFC never turn on when
C considering a PV-battery only case with no H2 loop. The electrolyzer or PEMFC
C may be 'turned on' in the above code if the battery load is above the maximum
C charge/discharge and the electrolyzer and PEMFC takes the remaining load.
C Also, the battery SOC is checked to ensure that it is within the minimum
C and maximum setpoint range. If it is not, the battery load is set to zero.

        IF ( bPV_bat ) THEN

            CDATA(IPCOMP_PEMFC,1) = 0.
            CDATA(IPCOMP_PEMFC,2) = 0.0
            bPEM_active = .false.

            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_electrolyzer,3 ) = 0.
            bElectrolyzer_active = .false.

            if ( bBattery_discharge .and.
     &           SOC_battery .lt. fSOC_battery_min ) then

                 batDemandP(IPWC_battery) = 0.

            elseif ( bBattery_charge .and.
     &               SOC_battery .gt. fSOC_battery_max ) then
                 if (mandChargeCycle(IPWC_battery) .NE. 1) then  ! Check on SOC above SOC_battery_max should only
                                                  ! be done if battery is not performing mandatory charge cycle
                    batDemandP(IPWC_battery) = 0.
                 endif
            endif


        ENDIF




C--------------------------------------------------------------------------------------
C Check if plant domain controls have been adjusted, and if so, stiplulate another
C plt<->pfs domain iteration on this timestep
C---------------------------------------------------------------------------------------
         call eclose ( fPEM_mode_old,
     &              CDATA(IPCOMP_PEMFC,1), 0.5, bPEM_same_mode )
         call eclose ( fPEM_signal_old,
     &              CDATA(IPCOMP_PEMFC,2), 2.0, bPEM_same_signal )    ! 2W tolerance
         call eclose ( fElectrolyzer_on_old,
     &              CDATA(IPCOMP_electrolyzer,1),0.5,
     &              bElectrolyzer_same_on )
         call eclose ( fElectrolyzer_standby_old,
     &              CDATA(IPCOMP_electrolyzer,2),0.5,
     &              bElectrolyzer_same_standby )
        call eclose ( fElectrolyzer_signal_old,
     &              CDATA(IPCOMP_electrolyzer,3),
     &              2.0,                              ! 2W tolerance
     &              bElec_same_signal )
C It is not clear why a change in the battery signal should trigger a plt-pfs
C iteration since the battery is a pfs powoc model. This should be explored
C in the future.
        call eclose ( fBattery_signal_old,
     &                batDemandP(IPWC_battery),
     &                2.0,                            ! 2W tolerance
     &                bBat_same_signal )

         if ( bPEM_same_mode .and. bPEM_same_signal .and.
     &     bElectrolyzer_same_on .and.
     &       bElectrolyzer_same_standby .and.
     &       bElec_same_signal .and.
     &       bBat_same_signal ) then
C--------Plt controls have not changed, and additional interdomain iteration is unnecessary
            bResH2_ctl_unconverged = .false.
         else
C--------Plt controls have changed, and additional iteration is required.
            bResH2_ctl_unconverged = .true.
         endif


C Control logic for grid-independent operation:
C a) Sense PV power added to DC bus.
C b) Sense load on AC bus (does not include grid ineraction).
C c) Determine how much power would have to be supplied from the DC
C    bus to the DC-AC converter to meet (b).  This has to account
C    for the losses of the DC-AC converter.
C d) Sense battery SOC.  This determines whether the battery or
C    the PEMFC will meet excess loading on the DC bus.  This also
C    determines whether the battery or the electrolyzer will
C    be used to store excess generation on the DC bus.
C e) If a>c then there is excess PV power.  Based upon (d) send
C    this excess either to the battery or to the electrolyzer.
C f) If a<c then there is insufficient power from the PV to meet
C    the full load on the AC bus.  Based upon (d) this deficit
C    will either be met by the battery or by the PEMFC.
C g) How do I control the grid-interaction powoc??

      endif  ! <- Matches "if (iResH2Ctl .ne. 1...", above.

      RETURN
      END





C----------------------------------------------------------------------------------------
C This subroutine reads the information required by the residential hydrogen
C cogen controller (subroutine ??? above) ans saves the information in
C the appropriate COMMON blocks.
C-----------------------------------------------------------------------------------------

      SUBROUTINE RESH2_Ctl_Read
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

C-----------------------------------------------------------------------------------------
C COMMON declarations

      common/outin/iuout,iuin,ieout
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number
      integer   ieout           !- error unit number

      common/filep/ifil
      integer   ifil            !- basic file unit number

C Input file information
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

C Power-only component information
      common/resh2_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery,IPWC_PEM_DCDC,IPWC_Elec_DCDC
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component
      integer   IPWC_PEM_DCDC   !-index number of the DC/DC converter between the PEMFC
                                !-and the DC bus
      integer   IPWC_Elec_DCDC  !-index number of the DC/DC converter between the electrolyzer
                                !-and the DC bus

C Plant component information
      common/resh2_plt/IPCOMP_electrolyzer,IPCOMP_PEMFC,
     &                 IPCOMP_elec_pump, IPCOMP_PEM_pump,
     &                 fElec_pump_flow, fPEM_pump_flow
      integer   IPCOMP_electrolyzer   !-index number of the electrolyzer plant component
      integer   IPCOMP_PEMFC          !-index number of the PEM fuel cell plant component
      integer   IPCOMP_elec_pump      !-index number of the electrolyzer cooling pump component
      integer   IPCOMP_PEM_pump       !-index number of the PEMFC cooling pump component
      real      fElec_pump_flow       !-flow rate of the electrolyzer cooling pump (m3/s)
      real      fPEM_pump_flow        !-flow rate of the PEMFC cooling pump (m3/s)


C Electric node information
      common/resh2_enode/IENOD_ACbus
      integer   IENOD_ACbus     !-node number of the AC bus

C Battery SOC setpoints for electrolyser and PEM fuel cell
      common/resh2_soc/SOC_electrolyzer_ON,SOC_electrolyzer_OFF,
     &                 SOC_pemfc_ON,SOC_pemfc_OFF
      real      SOC_electrolyzer_ON   !-battery SOC at which electrolyzer turns on
      real      SOC_electrolyzer_OFF  !-battery SOC at which electrolyzer switches off
      real      SOC_pemfc_ON          !-battery SOC at which PEM fuel cell switches on
      real      SOC_pemfc_OFF         !-battery SOC at which PEM fuel cell switches off

C Other controller inputs
      common/resh2/fH2_deadband
      real      fH2_deadband          !-controller deadband for H2 production (W). This
                                      !-value is applied so that the electrolyzer only switches on
                                      !-when the excess PV power is greater than power to meet
                                      !-the loads + power allowance for compressor and pumps,
                                      !-The power allowance for compressor and pumps is referred
                                      !-to as the H2 deadband.

C-----------------------------------------------------------------------------------------


C-----------------------------------------------------------------------------------------
C Type declarations for local variables.
C-----------------------------------------------------------------------------------------

      character outstr*124            !- string containing error message

      integer   iresh2fl_ascii        !- input file unit number
      integer   istat, ier            !- error flags
      integer   nd, k

      logical   bFileError            !- boolean signalling file error
      logical   xst                   !- boolean to signal that input file exists

      real      SOC_MAX               !- maximum state of charge of battery
      real      SOC_MIN               !- minimum state of charge of battery

      parameter( SOC_MAX = 100.,
     &           SOC_MIN = 0. )

      integer   lnblnk                ! function desribing non-bank string length

C-----------------------------------------------------------------------------------------
C Ensure that residential hydrogen cogen controller file exists.
C-----------------------------------------------------------------------------------------

      bFileError = .false.

      call FINDFIL(resh2flnam,xst)
      if ( .not. xst ) then
C filename is blank or unknown
             bFileError = .true.
             write(outstr,'(3A)') 'Residential H2 ctl file ',
     &                            resh2flnam(1:lnblnk(resh2flnam)),
     &                            ' was referenced but not found.'
      endif
C if there is an error, write message and stop simulation
      if ( bFileError ) then
         call edisp(iuout,outstr)
         STOP 'Need residential H2 ctl file: unresolvable error.'
      endif


C-----------------------------------------------------------------------------------------
C Read input file.
C-----------------------------------------------------------------------------------------

C Notify user that residential h2 cogen control file is being processed

      call usrmsg( 'Reading residential h2 cogen control file ... ',
     &             ' ','P')

C Open the input file

      bFileError = .false.

      iresh2fl_ascii = ifil + 40       !- actual file unit number, should be safe to use

      call erpfree(iresh2fl_ascii, istat)             !- closes file

      call efopseq(iresh2fl_ascii,resh2flnam,1,ier)   !- opens a sequential ASCII file

      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error opening residential h2 cogen ctl file. '
         bFileError = .true.
      endif

C Read the index number of the PCU that is connected between the PV and the DC bus.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'pvpcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading PV PCU index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_PVPCU,1,MPOWCOM,'W','pvpcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading PV PCU index number. '
         bFileError = .true.
      endif

C Read the node number of the AC bus.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'ac bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading AC bus node number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IENOD_ACbus,1,MENOD,'W','ac bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading AC bus node number. '
         bFileError = .true.
      endif

C Read the index number of the grid-connection power-only component.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'grid conn',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading grid conn index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_gridconn,1,MPOWCOM,'W','grid conn',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading grid conn index number. '
         bFileError = .true.
      endif

C Read the index number of the DC-AC converter.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'DC-AC pcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading DC-AC converter index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_ACDC,1,MPOWCOM,'W','DC-AC pcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading DC-AC converter index number. '
         bFileError = .true.
      endif

C Read the index number of the battery power-only component.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'battery',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading battery index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_battery,1,MPOWCOM,'W','battery',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading battery index number. '
         bFileError = .true.
      endif

C Read the battery SOC at which the electrolyzer turns on.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'SOC elec on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for electrolyzer on. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_electrolyzer_ON,SOC_MIN,SOC_MAX,
     &            'W','soc elec on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for electrolyzer on. '
         bFileError = .true.
      endif

C Read the battery SOC at which the electrolyzer switches off.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'SOC elec off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for electrolyzer off. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_electrolyzer_OFF,SOC_MIN,SOC_MAX,
     &            'W','soc elec off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for electrolyzer off. '
         bFileError = .true.
      endif

C Read the battery SOC at which the PEM fuel cell turns on.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'SOC pem on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PEM fuel cell on. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_pemfc_ON,SOC_MIN,SOC_MAX,
     &            'W','soc pem on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PEM fuel cell on. '
         bFileError = .true.
      endif

C Read the battery SOC at which the PEM fuel cell turns off.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'SOC pem off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PEM fuel cell off. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_pemfc_OFF,SOC_MIN,SOC_MAX,
     &            'W','soc pem off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PEM fuel cell off. '
         bFileError = .true.
      endif

C Read the index number of the electrolyzer plant component.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'elec ipcomp',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading electrolyzer plant comp index .'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPCOMP_electrolyzer,1,MPCOM,
     &            'W','elec ipcomp',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading electrolyzer plant comp index.'
         bFileError = .true.
      endif

C Read the index number of the PEM fuel cell plant component
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'pemfc ipcomp',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading pemfc plant comp index .'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPCOMP_PEMFC,1,MPCOM,
     &            'W','pemfc ipcomp',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading pemfc plant comp index.'
         bFileError = .true.
      endif

C Read the index number of the PCU between the PEMFC and the DC bus
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'pem-dc-dc ipwc',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading pemfc DC/DC index .'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_PEM_DCDC,1,MPOWCOM,
     &            'W','pem-dc-dc ipwc',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading pemfc DC/DC index.'
         bFileError = .true.
      endif


C Read the index number of the PCU between the electrolyzer and the DC bus
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'elec-dc-dc ipwc',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading electrolyzer DC/DC index .'
        bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_Elec_DCDC,1,MPOWCOM,
     &            'W','elec-dc-dc ipwc',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading electrolyzer DC/DC index.'
        bFileError = .true.
      endif

C Read the index number of the electrolyzer cooling pump plant component
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'elec-pump ipcomp',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading electrolyzer pump index .'
        bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPCOMP_elec_pump,1,MPCOM,
     &            'W','elec-pump ipcomp',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading electrolyzer pump index.'
        bFileError = .true.
      endif


C Read the index number of the PEMFC cooling pump plant component
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'pemfc-pump ipcomp',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading pemfc pump plant comp index .'
        bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPCOMP_PEM_pump,1,MPCOM,
     &            'W','pemfc-pump ipcomp',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading pemfc pump plant comp index.'
        bFileError = .true.
      endif

C Read the flow rate of the electrolyzer cooling pump.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'elec pump flow',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading electrolzer pump flow rate. '
        bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,fElec_pump_flow,0.,1000.,
     &            'W','elec pump flow',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading electrolyzer pump flow rate. '
        bFileError = .true.
      endif

C Read the flow rate of the PEMFC cooling pump.
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'pemfc pump flow',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading PEMFC pump flow rate. '
        bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,fPEM_pump_flow,0.,1000.,
     &            'W','pemfc pump flow',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading PEMFC pump flow rate. '
        bFileError = .true.
      endif

C Read the controller deadband for hydrogen production (W)
      call stripc(iresh2fl_ascii,outstr,0,nd,1,'power deadband',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading deadband for H2 production. '
        bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,fH2_deadband,0.,1000.,
     &            'W','power deadband',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading deadband for H2 production. '
        bFileError = .true.
      endif

C Error handling
      if ( bFileError ) then
         STOP ' Error in residential h2 cogen control file.'
      endif

C Reposition file to first line.
      rewind(iresh2fl_ascii)

C Notify user that residential h2 cogen control file processing is complete.
      call usrmsg( 'Reading res h2 cogen control file ... done.',
     &             ' ', 'P')



      RETURN
      END

C-----------------------    RESH2_Ctl_PostProcess   -------------------
C
C SYNOPSYS: This routine collects some interesting data pretaining
C to the H2 controller, and sends it on to h3kreports
C
C----------------------------------------------------------------------
      subroutine RESH2_Ctl_PostProcess()
      use h3kmodule
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"
C Input file information
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

C Power-only component information
      common/resh2_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery,IPWC_PEM_DCDC,IPWC_Elec_DCDC
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component
      integer   IPWC_PEM_DCDC   !-index number of the DC/DC converter between the PEMFC
                                !-and the DC bus
      integer   IPWC_Elec_DCDC  !-index number of the DC/DC converter between the electrolyzer
                                !-and the DC bus

C Electric node information
      common/resh2_enode/IENOD_ACbus
      INTEGER IENOD_ACbus          ! Elec domain node number of AC bus.

C Controller flag information
      common/resh2_comp_status/bPV_active,bElectrolyzer_active,
     &                    bPEM_active, bBattery_charge,
     &                    bBattery_discharge

C Flags indicating status of controller
      logical bPV_active,bElectrolyzer_active,bPEM_active,
     &                    bBattery_charge, bBattery_discharge


C 'Network interaction'
      real fNetworkBalance

C Node number of DC bus
      INTEGER IENOD_DCBUS    ! TO BE CHANGED TO READ IN FROM CTL FILE lATER

C Get power balance on AC bus (balance = generation + load, loads are -ive)
C

C Return if Res-H2 control not active.
      if ( iResH2Ctl /= 1 ) return

      fNetworkBalance = penodg(IENOD_ACbus) + penodl(IENOD_ACbus)

      IF (mandChargeCycle(IPWC_battery) .EQ. 1) THEN
C When the battery performs a mandatory charge mode, the direction of flow of power through the
C DC/AC converter could be reversed. This will not be possible with the electric current network.
C Instead of changing the electric network, the potential load of the DC node (node 1) is added to the
C network balance of the AC node (node 3) using an average efficiency of the then AC/DC convertor of 98%.
        IENOD_DCBUS = 1
        fNetworkBalance = penodg(IENOD_ACbus) + penodl(IENOD_ACbus) +
     &               (penodg(IENOD_DCbus) + penodl(IENOD_DCbus))/0.98
      ENDIF


C Determine operating mode, and report data to h3kreports

      if ( iResH2Ctl .ne. 1 ) then
C.......There's no h2 controller. do nothing.

      elseif ( bElectrolyzer_active ) then
C.......PV is running electrolyzer
        Call AddToReport(rvCtrlReH2NPwElAct%Identifier,fNetworkBalance)
      else
        Call AddToReport(rvCtrlReH2NPwElAct%Identifier,0.0)
      endif


      if (  bBattery_charge ) then
C.......PV is charging battery
        Call AddToReport(rvCtrlReH2NPwBattChrg%Identifier,
     &         fNetworkBalance)
      else
        Call AddToReport(rvCtrlReH2NPwBattChrg%Identifier,0.0)
      endif

      if ( bBattery_discharge ) then
C.......Battery is responding to loads
        Call AddToReport(rvCtrlReH2NPwBattDschrg%Identifier,
     &         fNetworkBalance)
      else
        Call AddToReport(rvCtrlReH2NPwBattDschrg%Identifier,0.0)
      endif

      if ( bPEM_active ) then
C.......Battery is responding to loads
        Call AddToReport(rvCtrlReH2NPwPEMFCAct%Identifier,
     &         fNetworkBalance)
      else
C.......Battery is responding to loads
        Call AddToReport(rvCtrlReH2NPwPEMFCAct%Identifier,0.0)
      endif

      return
      end


C-----------------------    RESH2_Ctl_Status_Blockdata ----------------
C
C SYNOPSYS: Initialization of value of bResH2_ctl_unconverged flag
C
C----------------------------------------------------------------------
      blockdata resh2_ctl_status_blockdata
      common / resh2_ctl_status / bResH2_ctl_unconverged
      logical bResH2_ctl_unconverged         !-logical flag indicating additional iterations req'd
      data bResH2_ctl_unconverged/.false./   !initial value
      end blockdata resh2_ctl_status_blockdata


