C-----------------------------------------------------------------------------------------
C A hack to start testing controls for RE-H2 project.
C This routine will be called at the beginning of the execution of the electric
C domain.  It must be called before subroutine ENETASGN since this routine
C sets variables such as PENODL to zero at the beginning of the establishment
C of the electric domain balances.  This control routine should perhaps be called
C from ENETSOLV prior to its call to ENETASGN.
C Note that some of the control loops will
C sense in the electrical domain and actuate plant components.  As well,
C the sensing in the electrical domain will be highly dependent upon
C where the control code is executed: before, during, or following the
C evaluation of all powoc models?  Need to think about this.
C-----------------------------------------------------------------------------------------
      SUBROUTINE RESH2_Ctl
      IMPLICIT NONE
      INCLUDE 'power.h'
      INCLUDE 'plant.h'
      INCLUDE 'BATTERY.h'


C-----------------------------------------------------------------------------------------
C Input file information
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

C Common block storing electrical domain time-row data for additional data items.
      common / Enet_Power_Time_Row_Data /
     &    iPow_Time_Row_Count,
     &    fPow_Time_Row_Present,
     &    fPow_Time_Row_Future
      integer iPow_Time_Row_Count ( MPOWCOM ) ! # of additional data items associated with power-only component.
      real fPow_Time_Row_Present ( MPOWCOM, MEnet_Time_Row_MAX ) ! Component present time-row data
      real fPow_Time_Row_Future ( MPOWCOM, MEnet_Time_Row_MAX ) ! Component future time-row data

C Common block storing load, generation, and transmission at elec domain nodes.
      COMMON/ENODLGT/PENODL(MENOD),QENODL(MENOD),
     &PENODG(MENOD),QENODG(MENOD),PENODT(MENOD),QENODT(MENOD)
      real    penodl         !- real power load on node
      real    qenodl         !- reactive power load on node
      real    penodg         !- real power generation at node
      real    qenodg         !- reactive power generation at node
      real    penodt         !- real transmitted power at node
      real    qenodt         !- reactive transmitted power at node

C Common block storing power load/generation and voltage of elec domain powoc's.
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)
      real    ppowoc         !-real power generated/load for power-only component
      real    qpowoc         !-reactive power generated/load for power-only component
      complex vpowc          !-voltage of power only component

C Common block storing control and other data for plant components.
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP          !-number of plant components
      INTEGER NCI             !-number of possible control variables for component
      REAL    CDATA           !-array containing control data values

C Common block storing additional time-row data items for plant domain. 
      common/pcdat/pcdatf(mpcom,mpcdat),pcdatp(mpcom,mpcdat)  
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)


C Power-only component information
      common/resh2_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery,IPWC_PEM_DCDC
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component    
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component
      integer   IPWC_PEM_DCDC   !-index number of the DC/DC converter between the PEMFC 
                                !-and the DC bus

C Plant component information
      common/resh2_plt/IPCOMP_electrolyzer,IPCOMP_PEMFC
      INTEGER IPCOMP_electrolyzer  ! Plant domain component number of electrolyzer.
      INTEGER IPCOMP_PEMFC         ! Plant domain component number of PEMFC.

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

C Vbls used to regulate iteration between demains

      common / resh2_ctl_status / bResH2_ctl_unconverged
      logical bResH2_ctl_unconverged    !-logical flag indicating additional iterations req'd
      data bResH2_ctl_unconverged/.false./
      
      real fPEM_mode_old         !-Previous PEMFC control scheme
      real fPEM_signal_old       !-Previous PEMFC control signal

      logical bPEM_same_mode, bPEM_same_signal !-flags indicating that signals have
                                               !-not changed

      real fElectrolyzer_on_old        !- Previous electrolyzer on signal
      real fElectrolyzer_standby_old   !- Previous electrolyzer standby signal

      logical bElectrolyzer_same_on, bElectrolyzer_same_standby  !- flags indicating that
                                                                 !- signals have not changed


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

      real  check
       
C end debugging


C-----------------------------------------------------------------------------------------

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

      LOGICAL bPEMFC_ON_p          !- boolean indicating if PEM is on at present time row.


C-----------------------------------------------------------------------------------------
C     Check if controller is in use
C-----------------------------------------------------------------------------------------
      if ( iResH2Ctl .ne. 1 ) then
C........Controller is not in use. Set flag preventing iteration
         bResH2_ctl_unconverged = .false.
      else
C........Controller is in use.       
      
C for debugging
         if ( nsinc. eq. 401 ) then
            check = 1.
         endif
C end debugging

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
C Determine the battery's SOC at the previous iteration and establish the
C following:
C  1) If there is excess PV-produced power on the DC bus whether this
C     power will be directed to the battery or to the electrolyzer.
C  2) If there is a deficit of PV-produced power on the DC bus whether
C     the battery or the PEMFC will supply the deficit.
C-----------------------------------------------------------------------------------------

         SOC_battery = fPow_Time_Row_Future(IPWC_battery,1)


C-----Establish the operational state of the electrolyzer at the present
C-----time row (i.e. whether it was on the previous time step).
C-----IPCOMP_electrolyzer is the index number of the electrolyzer plant component and
C-----IPCOMP_PEMFC is the index number of the PEMFC plant component.  

         Electrolyzer_ON_p      = PCDATP( IPCOMP_electrolyzer,1 )  ! =1. for ON
         Electrolyzer_standby_p = PCDATP( IPCOMP_electrolyzer,2)   ! =1. for STANDBY


C-----Establish the operational state of the PEMFC at the present time
C-----row (i.e. whether it was on the previous time step).
C-----PEMFC is on if value is 2 or 3.
         fPEMFC_ON_p = PCDATP(IPCOMP_PEMFC,2)
         bPEMFC_ON_p = .false.        ! initialize
         if ( fPEMFC_ON_p .eq. 2. .or. fPEMFC_ON_p .eq. 3. ) then
            bPEMFC_ON_p = .true.
         endif

C-----Determine if there is a surplus or deficit of power from the PV and actuate
C-----the electrolyzer and PEMFC accordingly.
         IF ( PV_power_to_DCbus > power_DC_to_AC_required ) THEN

C-------There is an excess of PV power. Send to battery or electrolyzer?
         IF ( SOC_battery > SOC_electrolyzer_ON ) THEN
C---------The battery is too full: send to electrolyzer. CDATA(ipcomp,1) is ON/OFF signal.
            batDemandP = 0.
            CDATA( IPCOMP_electrolyzer,1 ) = 1.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.

         ELSEIF ( SOC_battery > SOC_electrolyzer_OFF
     &           .and. Electrolyzer_ON_p==1. ) THEN
C---------Electrolyzer was on previous time-step and battery has not discharged sufficiently
C---------to turn electrolyzer off: send power to electrolyzer.
            batDemandP = 0.
            CDATA( IPCOMP_electrolyzer,1 ) = 1.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.
            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.

         ELSE
C---------Send to battery and set electrolyzer to standby. CDATA(ipcomp,2) is standby signal.
            batDemandP = -1. * (PV_power_to_DCbus -
     &                         power_DC_to_AC_required)     ! -ve = charge
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 1.
            CDATA( IPCOMP_PEMFC,1) = 0.
            CDATA( IPCOMP_PEMFC,2) = 0.

         ENDIF

       ELSE

C-------There is not sufficient PV power. Decide whether PEMFC or battery meets the load
         IF ( SOC_battery < SOC_pemfc_ON ) THEN
C---------The battery is too low: PEMFC supplies power. The load the PEMFC has to 
C---------meet is provided by the control signal CDATA(ipcomp,2). The PEMFC's control method
C---------is provided by the control signal CDATA(ipcomp,1).
            CDATA(IPCOMP_PEMFC,1) = 2.             ! iPower_out_controlled
            CDATA(IPCOMP_PEMFC,2) = power_DC_to_AC_required - 
     &                            PV_power_to_DCbus +
     &                            PEM_DCDC_loss
            batDemandP = 0.
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.


        ELSEIF ( SOC_battery < SOC_pemfc_OFF
     &           .and. bPEMFC_ON_p  ) THEN
C---------The battery has not been charged sufficiently: PEMFC continues to supply power. The  
C---------load the PEMFC has to meet is provided by the control signal CDATA(ipcomp,2).
C-------- The PEMFC's control method is provided by the control signal CDATA(ipcomp,1).
            batDemandP = 0.
            CDATA(IPCOMP_PEMFC,1) = 2.             ! iPower_out_controlled
            CDATA(IPCOMP_PEMFC,2) = power_DC_to_AC_required -
     &                            PV_power_to_DCbus +
     &                            PEM_DCDC_loss
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.

        ELSE
C---------The battery is charged sufficiently and supplies power, the PEMFC is off.
            batDemandP = power_DC_to_AC_required - PV_power_to_DCbus   ! +ve = discharge
            CDATA(IPCOMP_PEMFC,1) = 0.             ! iEngine_deactivated
            CDATA(IPCOMP_PEMFC,2) = 0.0
            CDATA( IPCOMP_electrolyzer,1 ) = 0.
            CDATA( IPCOMP_electrolyzer,2 ) = 0.

        ENDIF


      ENDIF


C--------------------------------------------------------------------------------------
C Check if plant domain controls have been adjusted, and if so, stiplulate another
C plt<->pfs domain iteration on this timestep
C---------------------------------------------------------------------------------------
         call eclose ( fPEM_mode_old,
     &              CDATA(IPCOMP_PEMFC,1), 0.5, bPEM_same_mode )
         call eclose ( fPEM_signal_old,
     &              CDATA(IPCOMP_PEMFC,2), 1.0E-03, bPEM_same_signal )
         call eclose ( fElectrolyzer_on_old,
     &              CDATA(IPCOMP_electrolyzer,1),0.5,
     &              bElectrolyzer_same_on )
         call eclose ( fElectrolyzer_standby_old,
     &              CDATA(IPCOMP_electrolyzer,2),0.5,
     &              bElectrolyzer_same_standby )


         if ( bPEM_same_mode .and. bPEM_same_signal .and. 
     &     bElectrolyzer_same_on .and.
     &     bElectrolyzer_same_standby ) then
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
C the appropriate COMMON blocks
C-----------------------------------------------------------------------------------------

      SUBROUTINE RESH2_Ctl_Read
      IMPLICIT NONE
      INCLUDE 'plant.h'
      INCLUDE 'power.h'

C-----------------------------------------------------------------------------------------
C COMMON declarations

      common/outin/iuout,iuin
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number

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
     &                   IPWC_battery,IPWC_PEM_DCDC
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component    
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component
      integer   IPWC_PEM_DCDC   !-index number of the DC/DC converter between the PEMFC 
                                !-and the DC bus

C Plant component information
      common/resh2_plt/IPCOMP_electrolyzer,IPCOMP_PEMFC
      integer   IPCOMP_electrolyzer   !-index number of the electrolyzer plant component
      integer   IPCOMP_PEMFC          !-index number of the PEM fuel cell plant component

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
