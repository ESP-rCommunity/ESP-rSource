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

C*************************** NCHE_control_components.F *********************************
C
C This file contains routines related to the natural convection heat exchanger (NCHE) 
c controller model:
C 
C   - NCHE_controller:          Controller routine for side-arm solar domestic hot water systems
C                               Currently does the following:
C                                   - Determines hot and cold storage temperatures
c								    - calculates the natural convection flow rate based on the
C								      system configuration and the densities in the other
C								      system components
C                                   - Manages the flowrate of the associated pump as required
C
C   - NCHE_thermal_XXX:         Routine containing XXX control algorithm for thermal
C                               load control:
C                                   - Saves thermal storage temperature data for use 
C                                     on subsequent timesteps
C 
C   - NCHE_controller_output:   Routine outputting miscellaneous controller data to a 
C                               text file, if requested. Intended mostly for debugging
C                               use.
C
C   Notes: These files describe a controller that will calculate the natural convection
C		   heat exchanger (NCHE) as used in a solar domestic hot water (SDHW) system.
C
C************************************************************************************

C************************************************************************************
C************************** NCHE_controller *****************************************
C
C Created by: Julia Purdy	
C Initial Creation Date: October 20, 2005.
C Copyright CETC 2005
C
C This subroutine is used to evaluate conditions in the plant environment and to 
C control various on-site generation equipment in response to these conditions. It 
C can be considered a coefficient generator in the sense that it does create matrix
C coefficients for the state equations describing its nodes, but these nodes are 
C merely used to measure conditions in other parts of the plant, and have no impact 
C on the simulation. Instead, this routine produces a set of control data passed out 
C in the PCAOUT array, which are used elsewehre to determine operating points in 
C distrubuted generation and cogeneration equipment.
C
C INPUTS
C
C --- Misc system data
C     COGEN_max_W:      COGEN system max output (W)
C     COGEN_min_W:      cogen system min output (W)     
C
C --- Hot storage control data 
C     HS_start:         Heating season start (1-365)
C     HS_end:           Heating season end   (1-365)
C     HWT_algorithm:    hot storage algorithm:
C                         0 = Constant 
C                         1 = Electrical load following
C                         2 = PID (thermal following)
C                         3 = PID (thermal priority)
C                         4 = Running average
C     HWT_T_sl:         Hot tank lower setpoint (oC)
C     HWT_T_su:         Hot tank upper setpoint (oC)
C     HWT_coeff_a:      Coefficient for target temp (0<a<1)
C     HWT_coeff_Kp:     Proportional gain (0<Kp<oo)
C     HWT_coeff_Ki:     Integral gain (0<Ki<oo) 
C     HWT_coeff_Kd:     Derivitive gain (0<Kd<oo)
C     HWT_per_int:      Integral period (hours)
C     HWT_per_der:      Derivitive period (hours)
C      
C --- Cold storage control data 
C     CS_start:         Cooling season start (1-365)
C     CS_end:           Cooling season end   (1-365)
C     CWT_algorithm:    Cold storage algorithm:
C                          0 = Constant 
C                          1 = Electrical load following
C                          2 = PID (thermal following)
C                          3 = PID (thermal priority)
C                          4 = Running average
C     CWT_T_sl:         Cold tank lower setpoint (oC)
C     CWT_T_su:         Cold tank upper setpoint (oC)
C     CWT_coeff_a:      Coefficient for target temp (0<a<1)
C     CWT_coeff_Kp:     Proportional gain (0<Kp<oo)
C     CWT_coeff_Ki:     Integral gain (0<Ki<oo) 
C     CWT_coeff_Kd:     Derivitive gain (0<Kd<oo)
C     CWT_per_int:      Integral period (hours)
C     CWT_per_der:      Derivitive period (hours)
C 
C --- Simulation Data: 
C     HWT_T:            Hot water tank temperature (oC)
C     CWT_T:            Cold water tank temperature (oC)
C     IDYP:             Current Day (1-365)
C     ptimef:           Future time (0-23.99...)
C
C
C Reference: Ferguson, 2003. "Design and implementation of thermal load following 
C               control for distributed generation systems coupled to thermally 
C               activated cooling equipment", CETC internal report.
C------------------------------------------------------------------------------------

      SUBROUTINE NCHE_controller(iterp)
      IMPLICIT NONE

      INCLUDE 'plant.h'
      INCLUDE 'building.h'
      INCLUDE 'DG_controller_include.h'
      INCLUDE 'CETC_definitions.h'
                                

      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/pclsol/icfp,idtypp,iperp
C      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),npcdt(mcl),
C     &            ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),
C     &            tpcps(mcl,mpcdt,mpcdp),ipctyp(mcl,mpcdt,mpcdp),
C     &            ipclaw(mcl,mpcdt,mpcdp),pmiscd(mcl,mpcdt,mpcdp,misc)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep 
      common/pctstp/ntstpp
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar) 
      common/pdbdt/adata(mpcom,madata),bdata(mpcom,mbdata)
      common/entfile/entflnam, ientxist
      common/ptime/ptimep,ptimef
      common/btime/btimep,btimef
      common/pltcon/bpltcon,lastTStp
      common/DgCont/first_run_done
      COMMON/PltMtrx/ISTATS
c------------------------------------------------------------------------------------
C     Declare Variables in common block (used for debugging with IMPLICIT NONE 
C     statement)
C------------------------------------------------------------------------------------
C.....Trace/error reporting unit numbers
      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu
C.....Simulation time
      integer ihrp, ihrf, idyp, idyf, idwp, idwf,nsinc,its
      real ptimep, ptimef, btimep, btimef
C.....Plant component data
      integer npcomp, nci
      real cdata
      integer icfp, idtypp, iperp
      integer ncl, ipsn, ipan, npcdt, ipcdv, npcdp
      real tpcps
C.....Control data
      integer ipctyp, ipclaw
      real pmiscd
C.....Plant component indexes
      integer npcdat, ipofs1, ipofs2
C.....Plant component parameters
      real adata, bdata
C.....Electrical network
      character*72 entflnam
      integer ientxist
C.....Plant future and present state variables
      real csvf, csvp
C.....Timestep diration
      integer isd1,ism1,isd2,ism2,isds,isdf,ntstep, ntstpp
C.....Plant solution state
      logical bpltcon
C.....Time on last timestep
      real lastTStp
C.....Flag indicating which plant matrix is being solved 
      integer ISTATS
C------------------------------------------------------------------------------------
C     Local/Passed  variables
C------------------------------------------------------------------------------------
      integer iterp             ! Flag indicating if there is iteration occuring in the
                                ! plant
      real q_nonHVAC_elec, q_HVAC_elec

      logical file_open         ! flag indicating that time step open file is open
      logical cont_debug        ! flag indicating if debugging is active 
C-------------------------------------------------------------------------------------
C     Functions called:
C-------------------------------------------------------------------------------------
      real DG_temperature_sensor
      real Elec_Net_Load_Calc
C-------------------------------------------------------------------------------------
C     Save data required on subsequent timesteps
C-------------------------------------------------------------------------------------
      Save  CWT_control,  HWT_control , 
     &     DG_control_signal,  TAC_Cooling_Signal,  TAC_HeatDump_Signal,
     &     HWT_overcharge_t,  CWT_overcharge_t,
     &     HWT_undercharge_t,  CWT_undercharge_t, FC_Type, file_open

C-------------------------------------------------------------------------------------
C     Named constants used to support control of Stirling engine model. Fuel
C     cell models might be updated to use this lexicon in the future.
C-------------------------------------------------------------------------------------

      integer iEngine_deactivated   ! Named constant indicating that a
                                    !   high-level controller has requested
                                    !   the engine be deactivated
      integer iFuel_flow_controlled ! Named constant indicating the model's
                                    !   fuel flow has been specified
                                    !   by external controller
      integer iPower_out_controlled ! Named constant indicating the model's
                                    !   fuel flow has been specified
                                    !   by external controller
      integer iHeat_out_controlled  ! Named constant indicating model's
                                    !   thermal output is controlled
      parameter ( iEngine_deactivated   = 0,
     &            iFuel_flow_controlled = 1,
     &            iPower_out_controlled = 2,
     &            iHeat_out_controlled  = 3 )
     
C-------------------------------------------------------------------------------------
C     If this is the first time the routine has been called, perform time-invariant
C     operations
C-------------------------------------------------------------------------------------
      if (.not. First_Run_Done ) then
C........Call subtoutine to initialize values. ALL ARGUEMENTS ARE OUTPUTS!
C (JWH continuation lines in call reduced due to warnings)
C FC_index,         ! pointer to fuel cell 
C FC_type,          ! flag indicating fuel cell type 
C TAC_index,        ! pointer to TAC 
C COGEN_max_W,      ! Cogen-system maximum power 
C COGEN_min_W,      ! Cogen-system minimum power 
C COGEN_con_W,      ! cogen-system constant electrical operating point 
C Thermal_p,        ! Flag indicating priority tank 
C HWT_index,        ! Pointer to hot water tank
C HWT_node,         ! Pointer to hot water tank water node
C CWT_index,        ! Pointer to cold water tank
C CWT_node,         ! Pointer to cold water tank water node
C HS_start,         ! Heating season start day
C HS_end,           ! Heating season end-day
C CS_start,         ! Cooling season start day
C CS_end,           ! Cooling season end day
C HWT_t_sl,         ! hot water tank lower setpoint 
C HWT_T_su,         ! hot water tank upper setpoint
C HWT_algorithm,    ! flag for hot water tank control algorithm
C HWT_coeff_a,      ! Target coefficient for hot water tank
C HWT_coeff_Kp,     ! hot water tank proportional gain  
C HWT_coeff_Ki,     ! Hot water tank integral gain   
C HWT_coeff_Kd,     ! Hot water tank derivitive gain
C HWT_per_int,      ! Hot water tank integral period
C HWT_per_der,      ! Hot water tank derivitive period
C CWT_T_sl,         ! Cold water tank lower setpoint  
C CWT_T_su,         ! Cold water tank upper setpoint 
C CWT_algorithm,    ! flag for cold water tank control algorithm
C CWT_coeff_a,      ! Cold water tank target coefficient
C CWT_coeff_Kp,     ! Cold water tank porportional gain 
C CWT_coeff_Ki,     ! Cold water tank integral gain  
C CWT_coeff_Kd,     ! Cold water tank derivitive gain
C CWT_per_int,      ! Cold water tank integral period
C CWT_per_der,      ! Cold water tank derivitive period
C HWT_T_target,     ! Hot water tank target temperature
C CWT_T_target,     ! Cot water tank target temperature
C iTS_out,          ! Flag indicating timestep output is req'd
C fatal_error       ! Flag indicating simulation must be halted
C bCW_tank          ! Flag indicating if hot water tank exists,
C bHW_tank          ! Flag indicating if cold water tank exists.
         call DG_controller_initialize(
     &        FC_index, FC_type, TAC_index, COGEN_max_W, COGEN_min_W,
     &        COGEN_con_W, Thermal_p, HWT_index, HWT_node, CWT_index,
     &        CWT_node, HS_start, HS_end, CS_start, CS_end,
     &        HWT_t_sl, HWT_T_su, HWT_algorithm, HWT_coeff_a,
     &        HWT_coeff_Kp, HWT_coeff_Ki, HWT_coeff_Kd, HWT_per_int, 
     &        HWT_per_der, CWT_T_sl, CWT_T_su, CWT_algorithm, 
     &        CWT_coeff_a, CWT_coeff_Kp, CWT_coeff_Ki, CWT_coeff_Kd, 
     &        CWT_per_int, CWT_per_der, HWT_T_target, CWT_T_target, 
     &        iTS_out, fatal_error, bHW_tank,bCW_tank
     &        )
C---------------------------------------------------------------------------------
C        SET HWT & CWT overcharge counters to zero
C---------------------------------------------------------------------------------
         HWT_overcharge_t = 0.
         CWT_overcharge_t = 0.
C---------------------------------------------------------------------------------
C        SET HWT & CWT undercharge counters to greater than 25.
C---------------------------------------------------------------------------------
         HWT_undercharge_t = 30.
         CWT_undercharge_t = 30.
C---------------------------------------------------------------------------------
C        Initialize TAC control signals
C---------------------------------------------------------------------------------
         TAC_cooling_signal  = 0
         TAC_heatdump_signal = 0
C---------------------------------------------------------------------------------
C	 Set flag for time step output file 
C---------------------------------------------------------------------------------
         file_open = .false.
      endif

C--------------------------------------------------------------------------------
C     ONLY determine control signal if temperature matrix is being solved 
C     (istats = 1) and plant has converged (ie. 2nd phase mass flow matrix sol'n
C     is complete, and this is the first go at the temperature matrix.)
C--------------------------------------------------------------------------------
      IF ( istats .eq. 1 .AND. bpltcon ) then
C---------------------------------------------------------------------------------
C        Determine if we're in the heating season. This is dependent on how HS_start
C        and HS_end are specified:
C            HS_start > HS_end - Winter in the nothern hemisphere
C            HS_start < HS_end - Winter in the southern hemisphere 
C        The heating season start/end dates are compared agianst the variable IDYP 
C        which holds the present simulation day.
C---------------------------------------------------------------------------------
         if( bHW_tank ) then
            if( HS_start .gt. HS_end) then
C..............Typical N.H. winter...
               if(IDYP .ge. HS_start.or.IDYP .le.HS_end) then
C................In heating season - set flag
                  bHS_flag = .true.
               else
                  bHS_flag = .false.
               endif
            else
C..............Typical S.H. winter...
               if(IDYP .ge. HS_start.AND. IDYP .le. HS_end) then
C.................In heating season - set flag
                  bHS_flag = .true.
               else
                  bHS_flag = .false.
               endif 
            endif
         else
C...........No hot water tank -> no heating season
            bHS_flag = .false.
         endif
                  
C---------------------------------------------------------------------------------
C        Determine if we're in the cooling season. This is dependent on how CS_start
C        and CS_end are specified:
C            CS_start < CS_end - Summer in the nothern hemisphere
C            CS_start > CS_end - Summer in the southern hemisphere 
C        The cooling season start/end dates are compared agianst the variable IDYP 
C        which holds the present simulation day.
C---------------------------------------------------------------------------------
         if ( bCW_tank ) then
            if(CS_start .lt. CS_end) then
C..............Typical N.H. summer...
               if(IDYP .ge. CS_start.AND.IDYP .le. CS_end) then
C.................In cooling season - set flag
                  bCS_flag = .true.
               else
                  bCS_flag = .false.
               endif
            else
C..............Typical S.H. summer...
               if(IDYP.ge.CS_start.or.IDYP.le.HS_end ) then
C.................In cooling season - set flag
                  bCS_flag = .true.
               else
                  bCS_flag = .false.
               endif 
            endif
         else
C...........No cold water tank -> no cooling season
            bCS_flag = .false.
         endif
         
C---------------------------------------------------------------------------------
C        Determine cold and hot storage temperatures using subroutine 
C        DG_temperature_sensor
C---------------------------------------------------------------------------------
         if ( bCS_flag )  CWT_T = DG_temperature_sensor(CWT_node) ! CWT_temperature (oC)
         if ( bHS_flag )  HWT_T = DG_temperature_sensor(HWT_node) ! HWT_temperature (oC)

C--------------------------------------------------------------------------------
C        Determine length of timestep (minutes)
C--------------------------------------------------------------------------------
         n_plant_time = NTSTEP * NTSTPP ! # plant timesteps / hour
         dt_timestep = 60.0/real(n_plant_time) ! # of minutes / plant timestep
C--------------------------------------------------------------------------------
C        Check to see if hot water tank is too hot and/or if cold water tank is
C        too cold. If so, set bXWT_overcharge flags, and update counters:
C           - CWT_overcharge_t: minutes CWT has been overcharged for
C           - HWT_overcharge_t: minutes HWT has been overcharged for
C           - CWT_undercharge_t: minutes CWT has been undercharged for
C           - HWT_undercharge_t: minutes HWT has been undercharged for
C--------------------------------------------------------------------------------
         if (bCW_tank) then
            if ( CWT_T .lt. CWT_T_sl ) then
C..............Cold water tank is overcharged. Set flag.
               bCWT_overcharge = .true.
C..............Update counter tracking # minutes overcharge state has persisted for
               CWT_overcharge_t = CWT_overcharge_t + dt_timestep
C...............Reset undercharge counter
               CWT_undercharge_t = 0.
            else
C..............Cold water tank is undercharged. Reset flag.
               bCWT_overcharge = .false.
C..............Update counter tracking # minutes undercharge state has persisted for
               CWT_undercharge_t = CWT_undercharge_t + dt_timestep
C..............Reset overcharge counter
               CWT_overcharge_t = 0.
            endif
         endif 

         if (bHW_tank) then
            if ( HWT_T .gt. HWT_T_su ) then
C..............Hot water tank is overcharged. Set flag.
               bHWT_overcharge = .true.
C..............Update counter tracking # minutes overcharge state has persisted for
               HWT_overcharge_t = HWT_overcharge_t + dt_timestep 
C..............Reset undercharge counter
               HWT_undercharge_t = 0.
            else
C..............Hot water tank is undercharged. Reset flag.
               bHWT_overcharge = .false.
C..............Update counter tracking # minutes undercharge state has persisted for
               HWT_undercharge_t = HWT_undercharge_t + dt_timestep
C..............Reset overcharge counter
               HWT_overcharge_t = 0.0
            endif
         endif 

C---------------------------------------------------------------------------------
C        Check if TAC system is in heat dump mode, and if so, if it can be 
C        switched out of heat dump mode. Refer to flow chart (Figure 10 from 
C        Ferguson (2003)). Note: Reset_period is a named constant defined in 
C        DG_module.F, and is nomimally 25 minutes.
C---------------------------------------------------------------------------------
         if (bCW_tank .AND. TAC_heatdump_signal .eq. 1) then
C........Check to see if TAC can be switched out of heat dump mode
            if(HWT_undercharge_t .ge. Reset_period) then
C...........HWT has been below upper setpoint for atleast 25 minutes,
C...........turn heat dump off
               TAC_heatdump_signal = 0
            elseif( CWT_undercharge_t .ge. Reset_period ) then
C...........CWT has not been overcharged in last 25 minutes. Try to cool HWT in
C...........cooling mode. 
               TAC_heatdump_signal = 0
            endif
         endif

C---------------------------------------------------------------------------------
C           if TAC systen is not presently in heat dump mode, run system according 
C           to state of storage tanks. (Refer to Figure 10, Ferguson (2003))
C
C          HWT_undercharge_t|CWT_undercharge_t|         meaning / action
C          --------------------------------------------------------------------------
C                           |                 |  HWT is too hot, but cwt can still 
C           < Reset_period  | >= Reset_period |  be cooled. Turn on TAC system in 
C                           |                 |  cooling mode to cool HWT and CWT. (see note)
C          --------------------------------------------------------------------------
C                           |                 |  CWT is too cold, but hwt can still 
C           >= Reset_period | < Reset_period  |  be heated. Turn off TAC system to 
C                           |                 |  heat hot water tank. 
C          --------------------------------------------------------------------------
C                           |                 |  HWT is too hot and CWT is too cold
C           < Reset_period  |  < Reset_period |  Turn on  TAC system in heat dump
C                           |                 |  mode.
C          --------------------------------------------------------------------------
C                           |                 |  Both tanks are operating normally.
C          >= Reset_period  | >= Reset_period |  Operate TAC system according to 
C                           |                 |  cooling demand
C          --------------------------------------------------------------------------
C
C           NOTE: When the hot water tank is overcharged and the TAC system runs in 
C                 cooling mode, the TAC may not extract enough heat at the 
C                 generator to effectively cool the hot water tank. Therefore, if this 
C                 condition persists for an extended period (currently 25 minutes),
C                 the controller will switch the system into heat dump mode.
C---------------------------------------------------------------------------------
         if ( bCW_tank .AND. TAC_heatdump_signal .eq. 0  ) then
            if ( ( HWT_undercharge_t .lt. Reset_period )
     &           .and. ( CWT_undercharge_t .ge. Reset_period ) ) then
C..............HWT is too hot, CWT can be cooled.
C..............Check to see if TAC is already in cooling
               if(TAC_cooling_signal .eq. 0) then
C..............TAC was previously off - turn it on.
                  TAC_cooling_signal = 1.
               elseif(HWT_overcharge_t .ge. Reset_period) then
C..............TAC is already on, and overcharge condition has 
C..............persisted for more than reset period munites. 
C..............Switch to heat dump mode
                  TAC_cooling_signal  = 0.
                  TAC_heatdump_signal = 1.
               else
C..............TAC is already on, but overcharge condition has 
C..............not persisted for more than reset period munites. 
C..............DO nothing.
               endif
               
            elseif((HWT_undercharge_t.ge.Reset_period)
     &              .AND.(CWT_undercharge_t.lt.Reset_period)) then
C...........CWT is too cold. Turn TAC off.
               TAC_cooling_signal = 0.
            elseif((HWT_undercharge_t.lt.Reset_period)
     &              .AND.(CWT_undercharge_t.lt.Reset_period)) then
C...........CWT is too cold and HWT is too hot. Turn
C...........heat dump on
               TAC_cooling_signal = 0.          
               TAC_heatdump_signal = 1.
            else
C...........TAC will be managed according to loads on thermal inventories
C...........do nothing now.              
            endif
         endif

C---------------------------------------------------------------------------------
C        Determine if hot and cold temperature tanks must be controlled:
C
C         CS_flag  |   HS_flag  | Meaning
C        --------------------------------------------------------
C            1     |      0     | Control of CWT required only, HWT tank will float
C            0     |      1     | Control of HWT required only, CWT tank will float
C            1     |      1     | Control of both tanks required 
C            0     |      0     | Neither tank will be controlled. Let system float
C    
C        Use this information to select the appropriate algorithm and to set the 
C        status of the thermal storage inventories:
C          XXX_status = STA_primary   : Primary thermal storage
C          XXX_status = STA_secondary : Auxiliary thermal storage
C          XXX_status = STA_freefloat : Free floating
C        
C        Where STA_primary, STA_secondary, STA_freefloat are named constants defined
C        in module DG_controller
C       
C        Refer to Section 3.3.3, Ferguson (2003)
C---------------------------------------------------------------------------------
         if ( .not. bCW_tank .AND. .not. bHW_tank ) then
C...........Hot/cold water tanks do not exist. Use electrical load
C...........following algorithm
            PRI_algorithm = ALG_electric_L
            CWT_status = STA_freefloat
            HWT_status = STA_freefloat       
         elseif (bCS_flag .AND. .not. bHS_flag) then
C........Loads on cold water tank only. Use specidied CWT algorithm
            PRI_algorithm = CWT_algorithm
C........Set tank status
            CWT_status = STA_primary
            HWT_status = STA_freefloat
         elseif (.not. bCS_flag .AND. bHS_flag ) then
C........Loads on hot water tank only. Use specidied HWT algorithm
            PRI_algorithm = HWT_algorithm
C.......Set tank status
            CWT_status = STA_freefloat
            HWT_status = STA_primary
         elseif (bCS_flag.AND. bHS_flag) then
C........Loads on both tanks. Select algorithm and set tank status  
C........using specified preference:
            if(thermal_p .eq. cold_storage) then
C...........Cold tank is primary inventory
               PRI_algorithm = CWT_algorithm
               CWT_status = STA_primary
               HWT_status = STA_auxiliary
            elseif(thermal_p .eq. hot_storage) then
C...........Hot tank is primary inventroy
               PRI_algorithm = HWT_algorithm
               CWT_status = STA_auxiliary
               HWT_status = STA_primary
            endif
         elseif (.not. bCS_flag .AND. .not. bHS_flag ) then
C...........Free floating control
            PRI_algorithm = ALG_freefloat
            CWT_status = STA_freefloat
            HWT_status = STA_freefloat
         endif

C---------------------------------------------------------------------------------
C        Determine electric loads. Call FC_eloads_establish routine written for
C        CETC fuel cell model (located FC_components.F).
C
C        Synopsys:
C
C        FC_eloads_establish reads the instantaneous building electric load
C        (W_elec_demand, Watts) from a binary file (FCloads.tmp) written before
C        runtime. FC_eloads_establish is passed the day number (IDYP, 1-365), the
C        present building time (BTIMEP, hours) and the future plant time
C        (PTIMEF, hours). IDYP, PTIMEF and BTIMEP are carried in ESP-r common
C        blocks. The variable q_par_elec stores the fuel cell system parasitic
C        loads, and is not used in this case.
C
C        Note: W_elec_demand includes both building and HVAC electrical loads/
C---------------------------------------------------------------------------------
         if ( IENTXIST .le. 0 ) then
C........Only establish and manage electrical loads if the electrical network 
C........is not active     
            call FC_eloads_establish(
     &           IDYP,           ! present day             (input)
     &           BTIMEP,         ! present building time   (input)
     &           PTIMEF,         ! future plant time       (input)
     &           q_nonHVAC_elec, ! non-HVAC electric load  (output)
     &           q_HVAC_elec     ! HVAC electrical load    (output)
     &           )
            W_elec_demand = q_nonHVAC_elec + q_HVAC_elec
         else
C.......If the electrical network is active, call this routine to estimate 
C.......the electrical loads. This is an estimate since the electrical domain
C.......establishes the non-HVAC loads, and this domain is solved following
C.......convergence of the plant domain.         
            W_Elec_demand =  Elec_Net_Load_Calc(total_load)
         endif

C---------------------------------------------------------------------------------
C        Apply appropriate control scheme based on PRI_algorithm. Refer to 
C        Section 3.4 in Ferguson (2003)
C---------------------------------------------------------------------------------
         if ( PRI_algorithm .eq. ALG_constant ) then
C---------------------------------------------------------------------------------
C              Constant electrical output scenario. Set target electrical output
C              equal to supplied constant electrical output.
C      
C              DG_control_signal = COGEN_con_W / COGEN_max_w
C
C              Refer to Section 3.4.1, Ferguson (2003).
C--------------------------------------------------------------------------------
            DG_control_signal = COGEN_con_W / COGEN_max_w
            iStirling_Control_mode = iPower_out_controlled
C---------------------------------------------------------------------------------
C              Call 'DG_manage_thermal_loads' routine to assign priority to tanks.
C              Ignore the thermal_control_signal variable that it will return
C
C              Synopsys:
C              DG_manage_thermal_loads(tank_priority,control_signal)
C
C              Tank thermal loads returns an interger value (STO_priority) 
C              indicating which tank the TAC system should be run to replenish, 
C              and a control signal (thermal_control_signal, 0->1) that can be 
C              to determine the fuel cell's operating point. In this case, where 
C              the fuel cell is operated to meet the electrical requirements of 
C              the building, the control signal returned by DG_manage_thermal_loads 
C              is ignored.The tank priority returned by DG_manage_thermal_loads is 
C              used to determine the operation of the TAC system.    
C---------------------------------------------------------------------------------
C ( JWH remove continuation lines)
C iterp,         ! flag indicating plant solution is in iteration
C CWT_index,     ! pointer to cold water tank (Input)
C HWT_index,     ! pointer to hot water tank        
C CWT_T,         ! Cold water tank temperature            
C CWT_T_target,  ! Cold water tank target temperature 
C CWT_T_su,      ! Cold water tank upper setpoint 
C CWT_T_sl,      ! Cold water tank lower setpoint
C HWT_T,         ! Hot water tank temperature
C HWT_T_target,  ! Hot water tank target temperature
C HWT_T_su,      ! hot water tank upper setpoint
C hwt_t_sl,      ! hot water tank lower setpoint
C HWT_coeff_Kp,  ! hot water tank proportional gain
C HWT_coeff_Ki,  ! Hot water tank integral gain
C HWT_coeff_Kd,  ! Hot water tank derivitive gain
C HWT_per_int,   ! Hot water tank integral period
C HWT_per_der,   ! Hot water tank derivitive period
C CWT_coeff_Kp,  ! cold water tank proportional gain
C CWT_coeff_Ki,  ! Cold water tank integral gain
C CWT_coeff_Kd,  ! Cold water tank derivitive gain
C CWT_per_int,   ! Cold water tank integral period
C CWT_per_der,   ! Cold water tank derivitive period
C CWT_Status,    ! Priority status of cold water tank
C HWT_Status,    ! Priority status of hot water tank
C CWT_error,     ! Cold water tank error coefficient      
C CWT_Control,   ! Cold water tank control signal
C HWT_error,     ! Hot water tank error coefficient
C HWT_control,   ! Hot water tank control signal
C STO_priority,  ! Storage tank thermal priority        
C thermal_control_signal
            call DG_manage_thermal_loads(
     &           iterp, CWT_index, HWT_index, CWT_T, CWT_T_target, 
     &           CWT_T_su, CWT_T_sl, HWT_T, HWT_T_target, HWT_T_su,
     &           hwt_t_sl, HWT_coeff_Kp, HWT_coeff_Ki, HWT_coeff_Kd, 
     &           HWT_per_int, HWT_per_der, CWT_coeff_Kp, CWT_coeff_Ki, 
     &           CWT_coeff_Kd, CWT_per_int, CWT_per_der, CWT_Status, 
     &           HWT_Status, CWT_error, CWT_Control, HWT_error, 
     &           HWT_control, STO_priority, thermal_control_signal
     &           )
         elseif ( PRI_algorithm .eq. ALG_electric_L ) then
C---------------------------------------------------------------------------------
C              Electric load following scenario. Use electric loads to determine
C              control signal.   
C    
C              Refer to Section 3.4.2, Ferguson (2003).
C---------------------------------------------------------------------------------
            DG_control_signal = W_elec_demand  / COGEN_max_w
C...........Set flag for Stirling engine control interface.            
            iStirling_Control_mode = iPower_out_controlled
            
C---------------------------------------------------------------------------------
C              Call 'DG_manage_thermal_loads' routine to assign priority to tanks.
C              Ignore the thermal_control_signal variable that it will return
C
C              Synopsys:
C              DG_manage_thermal_loads(tank_priority,control_signal)
C
C              Tank thermal loads returns an interger value (STO_priority) 
C              indicating which tank the TAC system should be run to replenish, 
C              and a control signal (thermal_control_signal, 0->1) that can be 
C              to determine the fuel cell's operating point. In this case, where 
C              the fuel cell is operated to meet the electrical requirements of 
C              the building, the control signal returned by DG_manage_thermal_loads 
C              is ignored.The tank priority returned by DG_manage_thermal_loads is 
C              used to determine the operation of the TAC system.    
C---------------------------------------------------------------------------------
C (see above call for notes on parameters)
            call DG_manage_thermal_loads(
     &           iterp, CWT_index, HWT_index, CWT_T, CWT_T_target, 
     &           CWT_T_su, CWT_T_sl, HWT_T, HWT_T_target, HWT_T_su, 
     &           hwt_t_sl, HWT_coeff_Kp, HWT_coeff_Ki, HWT_coeff_Kd, 
     &           HWT_per_int, HWT_per_der, CWT_coeff_Kp, CWT_coeff_Ki, 
     &           CWT_coeff_Kd, CWT_per_int, CWT_per_der, CWT_Status, 
     &           HWT_Status, CWT_error, CWT_Control, HWT_error, 
     &           HWT_control, STO_priority, thermal_control_signal
     &        )
         elseif ( PRI_algorithm .eq. ALG_thermal_L ) then
C---------------------------------------------------------------------------------
C           Thermal load following using PID algorithm. Fuel cell will be
C           operated to meet the themal loads on the hot and cold water tanks.
C
C           Refer to Section 3.4.3, Ferguson (2003).
C
C           Check to see that:
C             1. the TAC is not in heat dump mode,
C             2. the cold water tank has not been overcharged in the last 25
C                minutes, and
C             3. the selected control algorithm is not "free-floating".
C           If any of these conditions is met, the surplus heat from the
C           fuel cell cannot be used. Therefore, set contol signal to zero.
C---------------------------------------------------------------------------------
            if((TAC_heatdump_signal .eq. 1)
     &           .or.(CWT_undercharge_t.lt.Reset_period
     &           .AND.HWT_status .eq. STA_freefloat)
     &           .or.(PRI_algorithm .eq. ALG_freefloat)) then
               DG_control_signal = 0.
               STO_priority = 0 ! neither tank is assigned priority

C.............Set flag for Stirling engine control interface.
              iStirling_Control_mode = iEngine_deactivated
              
            else
C---------------------------------------------------------------------------------
C              Heat pricuced by fuel cell can be used. Call subroutine
C              'DG_manage_thermal_loads' to determine fuel cell control signal and
C              the storage priority
C---------------------------------------------------------------------------------
C (see above call for notes on parameters)
               call DG_manage_thermal_loads(
     &            iterp, CWT_index, HWT_index, CWT_T, CWT_T_target, 
     &            CWT_T_su, CWT_T_sl, HWT_T, HWT_T_target, HWT_T_su, 
     &            hwt_t_sl, HWT_coeff_Kp, HWT_coeff_Ki,HWT_coeff_Kd, 
     &            HWT_per_int, HWT_per_der, CWT_coeff_Kp, CWT_coeff_Ki, 
     &            CWT_coeff_Kd, CWT_per_int, CWT_per_der, CWT_Status, 
     &            HWT_Status, CWT_error, CWT_Control, HWT_error, 
     &            HWT_control, STO_priority, thermal_control_signal
     &            )
               DG_control_signal = thermal_control_signal

C..............Set flag for Stirling engine control interface.
               iStirling_Control_mode = iFuel_flow_controlled
            
            endif
         elseif ( PRI_algorithm .eq. ALG_thermal_P ) then 
C---------------------------------------------------------------------------------
C           Thermal priority PID algorithm. Fuel cell will be run to meet (a)
C           the electrical load, (b) the HWT load or (c) the CWT load. Refer
C           to section 3.4.4, Ferguson (2003).
C
C           Check to see that:
C             1. the TAC is not in heat dump mode,
C             2. the cold water tank has not been overcharged in the last 25
C                minutes, and
C             3. the selected control algorithm is not "free-floating".
C           If any of these conditions is met, the surplus heat from the
C           fuel cell cannot be used. Therefore, set contol signal to
C           electrical requirements, and tank priority flag to zero.
C--------------------------------------------------------------------------------- 
            if((TAC_heatdump_signal .eq. 1)
     &           .or.(CWT_undercharge_t.lt.Reset_period
     &           .and. HWT_status .eq. STA_freefloat)
     &           .or.(PRI_algorithm .eq. ALG_freefloat)) then
               DG_control_signal = W_elec_demand  / COGEN_max_w
  
               STO_priority = 0 ! neither tank is assigned priority

C..............Set flag for Stirling engine control interface.            
               iStirling_Control_mode = iPower_out_controlled
               
            else
C---------------------------------------------------------------------------------
C              Heat pricuced by fuel cell can be used. Call subroutine
C              'DG_manage_thermal_loads' to determine fuel cell control signal and
C              the storage priority
C
C              The fuel cell is run as follows:
C               1. If the error coefficient on the primary thermal storage is
C                  less than Error_threshold, the controller will run the
C                  fuel cell to meet either:
C                    a. the electrical demands in the building,
C                    b. the loads on the primary storage, or
C                    c. the loads on the secondary storage,
C                  which ever results in the largest amount of electrical output.
C               2. If the error coefficient on the primary thermal storage, is
C                  greater than Error_threshold, the controller will run the fuel
C                  cell to meet either:
C                    a. the electrical demands in the building, or
C                    b. the loads on the primary storage,
C                  which ever results in the largest amount of electrical output.
C
C              Note: The subroutine 'DG_manage_thermal_loads' automatically determines
C                    if the primary storage error coefficient is greater than
C                    Error_threshold, and returns the appropriate thermal control
C                    signal for the fuel cell. Thus, once 'DG_manage_thermal_loads'
C                    is called, the  returned thermal control signal need only be
C                    compared to the calculated electric demand.
C---------------------------------------------------------------------------------  
C (see above call for notes on parameters)
               electrical_control_signal = 
     &              W_elec_demand  / COGEN_max_w
               call DG_manage_thermal_loads(
     &            iterp, CWT_index, HWT_index, CWT_T, CWT_T_target,    
     &            CWT_T_su, CWT_T_sl, HWT_T, HWT_T_target, HWT_T_su, 
     &            hwt_t_sl, HWT_coeff_Kp, HWT_coeff_Ki, HWT_coeff_Kd, 
     &            HWT_per_int, HWT_per_der, CWT_coeff_Kp, CWT_coeff_Ki, 
     &            CWT_coeff_Kd, CWT_per_int, CWT_per_der, CWT_Status, 
     &            HWT_Status, CWT_error, CWT_Control, HWT_error, 
     &            HWT_control, STO_priority, thermal_control_signal
     &            )
               if(electrical_control_signal .gt.
     &              thermal_control_signal) then
                  DG_Control_signal = electrical_control_signal

C................Set flag for Stirling engine control interface.            
                 iStirling_Control_mode = iPower_out_controlled
               
               else
                  DG_Control_signal = thermal_control_signal
                           
C.................Set flag for Stirling engine control interface.            
                  iStirling_Control_mode = iFuel_flow_controlled
                  
               endif
            endif
         else
C---------------------------------------------------------------------------------
C        Can't happen - algorithms checked above.
C---------------------------------------------------------------------------------              
         endif

C---------------------------------------------------------------------------------
C        Determine if TAC should be turned on or off, depending on status of tanks
C         1. Check to see TAC operation is not dictated by overcharge condition
C         2. Set TAC operation according to which storage has been assigned priority.
C--------------------------------------------------------------------------------- 
         if(CWT_undercharge_t .lt. Reset_period .or.
     &        HWT_undercharge_t .lt. Reset_period) then
C---------------------------------------------------------------------------------
C           TAC is responding to overcharge condition. Leave control signals alone
C---------------------------------------------------------------------------------
         else 
C---------------------------------------------------------------------------------
C           Determine overcharge condition based on priority tank
C---------------------------------------------------------------------------------
            SELECT CASE(STO_priority)
               CASE(cold_storage)
C---------------------------------------------------------------------------------
C                 CWT has priority. Turn TAC on if cogen system is also operating
C---------------------------------------------------------------------------------
                  if(DG_control_signal .gt. 0.) then
                     TAC_cooling_signal = 1
                  else 
                     TAC_cooling_signal = 0
                  endif
               CASE(hot_storage)
C---------------------------------------------------------------------------------
C                 HWT has priority. Turn TAC off
C---------------------------------------------------------------------------------
                  TAC_cooling_signal = 0
               CASE DEFAULT
C---------------------------------------------------------------------------------
C                 Neither thermal store has been assigned priority. Turn TAC off
C---------------------------------------------------------------------------------
                  TAC_cooling_signal = 0                  
            end SELECT
         endif
      endif
C---------------------------------------------------------------------------------
C     Set control signals: write data into fuel cell and TAC CDATA arrays:
C
C     Set CDATA(FC_index,1) equal to calculated control signal. 
C     Set CDATA(TAC_index,1) equal to TAC cooling control signal. 
C     Set CDATA(TAC_index,2) equal to TAC heat dump control signal.
C
C     Note: if plant energy solution has not converged, (ie. ISTATS != 1 and/or 
C     bpltcon = .FALSE.) the DG_control_signal variable will remain unchanged 
C     from the last iteration,  and this value will again be passed out to the 
C     cogeneration system in the PCAOUT(IPCOMP,?) array.
C
C     Note: Stirling engine model uses a more sophesticated control interface.
C---------------------------------------------------------------------------------
      if ( FC_index .gt. 0 ) then
         
         if ( FC_Type .eq. Stirling_model ) then

C...........Specify the type of control scheme in use:
            CDATA(FC_index,1) = iStirling_Control_mode 

C...........Configure control signal
            if ( iStirling_Control_mode
     &            .eq. iEngine_deactivated ) then

C..............Set control signal to zero
               CDATA(FC_index,2) = 0.0
                                            
            elseif ( iStirling_Control_mode
     &            .eq. iPower_out_controlled ) then

C..............Pass electric load directly on to engine.
               CDATA(FC_index,2) = W_elec_demand  ! electric demand ()
            
            elseif (iStirling_Control_mode
     &             .eq. iFuel_flow_controlled ) then 

C..............Pass dimensionless output to engine               
               CDATA(FC_index,2) = DG_control_signal  ! dimensionless signal
            
            else
C..............Shouldn't happen - iStirling_Control_mode set above.
               write(ITU,*) 'DG_controller.F: Stirling Control',
     &               ' mode (', iStirling_Control_mode,
     &               ' ) is unsupported!'
               fatal_error = .true.
            endif
         else
C...........Non-Stirling models          
            CDATA(FC_index,1)  = DG_control_signal
         endif
      endif
C.....TAC control signals       
      if ( TAC_index .gt. 0 ) then
         CDATA(TAC_index,1) = float(TAC_cooling_signal)
         CDATA(TAC_index,2) = float(TAC_heatdump_signal)
      endif
C---------------------------------------------------------------------------------
C Store the electrical demand and output data to calculate monthly averages
C and optionally output the data each time-step.
C---------------------------------------------------------------------------------
      call H3KSTORE_PLANT_ELEC     
C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      cont_debug = .false.
      if((ITC .gt. 0 
     &  .AND. NSINC .ge. ITC 
     &  .AND.NSINC .le. ITCF 
     &  .AND.ITRACE(37) .ne. 0)
     &     .or. ( cont_debug .or. fatal_error) )then
        write(ITU,*) 'DG controller'
        write(ITU,*) ' Plant state: bpltcon = ',bpltcon
        if( bpltcon ) then
           write(ITU,*) '  - Updating control signal'
        else 
           write(ITU,*) '  - Control signal unchanged'
        endif
        write(ITU,*) ' *** DG_controller Output ***'
        write(ITU,*) '     HEATING season / Cooling season info :'
        write(ITU,*) '     - Day #     :', IDYP
        write(ITU,*) '     - HS start  :', HS_start 
        write(ITU,*) '     - HS end    :', HS_end
        write(ITU,*) '     - CS start  :', CS_start 
        write(ITU,*) '     - CS end    :', CS_end
        write(ITU,*) '     - HS flag   :', bHS_flag
        write(ITU,*) '     - CS flag   :', bCS_flag
        write(ITU,*) '     INPUT DATA - cold storage control :'
        write(ITU,*) '     - Algorithm :', CWT_algorithm
        write(ITU,*) '     - T sl      :', CWT_T_sl
        write(ITU,*) '     - T su      :', CWT_T_su
        write(ITU,*) '     - coeff a   :', CWT_coeff_a
        write(ITU,*) '     - Kp        :', CWT_coeff_kp
        write(ITU,*) '     - Ki        :', CWT_coeff_ki
        write(ITU,*) '     - Kd        :', CWT_coeff_kd
        write(ITU,*) '     - int. per. :', CWT_per_int
        write(ITU,*) '     - der. per. :', CWT_per_der
        write(ITU,*) '     INPUT DATA - hot storage control :'
        write(ITU,*) '     - Algorithm :', HWT_algorithm
        write(ITU,*) '     - T sl      :', HWT_T_sl
        write(ITU,*) '     - T su      :', HWT_T_su
        write(ITU,*) '     - coeff a   :', HWT_coeff_a
        write(ITU,*) '     - Kp        :', HWT_coeff_kp
        write(ITU,*) '     - Ki        :', HWT_coeff_ki
        write(ITU,*) '     - Kd        :', HWT_coeff_kd
        write(ITU,*) '     - int. per. :', HWT_per_int
        write(ITU,*) '     - der. per. :', HWT_per_der
        write(ITU,*) '     Storage temperatures :'
        write(ITU,*) '     - CWT       :', CWT_T
        write(ITU,*) '     - HWT       :', HWT_T
        write(ITU,*) '   * START calculations:'   
        write(ITU,*) '     - NTSTEP    :', NTSTEP
        write(ITU,*) '     - NTSTPP    :', NTSTPP
        write(ITU,*) '     TARGET temperatures:'
        write(ITU,*) '     - CWT Targ. :', CWT_T_target
        write(ITU,*) '     - HWT Targ. :', HWT_T_target
        write(ITU,*) '     Error signals:'
        write(ITU,*) '     - CWT error :', CWT_error 
        write(ITU,*) '     - HWT error :', HWT_error 
        write(ITU,*) '     Control signals:'
        write(ITU,*) '     - CWT       :', CWT_control
        write(ITU,*) '     - HWT       :', HWT_control 
        write(ITU,*) '     TANK STATUS:'
        write(ITU,*) '     - CWT       :', CWT_status
        write(ITU,*) '     - HWT       :', HWT_status
        write(ITU,*) '     - ALGorithm :', PRI_algorithm
        write(ITU,*) '     - priority  :', STO_priority
        write(ITU,*) '     TAC operation:'
        write(ITU,*) '     - HWT_over_C:',bHWT_overcharge
        write(ITU,*) '     - CWT_over_C:',bCWT_overcharge
        write(ITU,*) '     - Cool sig. :',TAC_cooling_signal
        write(ITU,*) '     - HD sig.   :',TAC_HeatDump_signal

        write(ITU,*) '     TEXT OUTPUT:'
        write(ITU,*) '     - iTS_out   :',iTS_out

        write(ITU,*) '     PLANT convergance state:'
        write(ITU,*) '     - bpltcon   :', bpltcon
        write(ITU,*) '     ARRAY allocation state:'
        write(ITU,*) ' Leaving subroutine DG_control_coeff_generator'
      end if

C---------------------------------------------------------------------------------
C     Call routine DG_controller_output to write timestep data if:
C      - Timestep data has been requested (iTS_out =1)
C      - Current solution is for 1st-phase mass balance (ISTATS == 2)
C      - Plant solution has converged (bpltcon = 1)
C      - Simulation has moved out of the start up period
C        (function bInStartup() = .FALSE.)
C---------------------------------------------------------------------------------
      if( iTS_out.eq.1 .and. bpltcon.and.( .not. bInStartup())) then 
         call DG_controller_output(
     &        IDYP, ptimep,
     &        DG_control_signal, !  Cogen unit control signal
     &        TAC_Cooling_Signal, !  TAC cooling control signal (on-off)
     &        TAC_HeatDump_Signal, !  TAC heat dump control signal (on-off)
     &        bHS_flag,         !  heating season flag
     &        bCS_flag,         !  cooling season flag
     &        HWT_T,            !  HWT tank temperature
     &        HWT_T_target,     !  Hot water tank target temperature
     &        HWT_error,        !  HWT error
     &        HWT_control,      !  HWT control signal
     &        CWT_T,            !  CWT tank temperature
     &        CWT_T_target,     !  CWT target temperature
     &        CWT_error,        !  CWT error
     &        CWT_control,      !  CWT control signal
     &        W_elec_demand,    !  Dwelling electrical demand 
     &        file_open         !  Flag indicating that file has been opened.
     &        )
      endif
C--------------------------------------------------------------------------------- 
C     Check for fatal error, and kill simulation as req'd.
C---------------------------------------------------------------------------------
      if ( fatal_error ) then
         stop 'Cogeneration system controller: unresolvable error'
      endif
C--------------------------------------------------------------------------------- 
C     Set flag to ensure that time-invarient operations are
C     not repeated
C--------------------------------------------------------------------------------- 
      if ( .not. First_Run_Done ) First_Run_Done = .true. 
      RETURN
      END
 
C--------------------DG_controller_initialize---------------------------------------
C Created by: Alex Ferguson
C Initial Creation Date: Feb 10, 2004.
C Copyright CETC 2004
C
C This subroutine is used to initialize values for the DG_controller, and to perform
C time-invariant calculations.
C
C INPUTS: NONE. OUTUTS: ALL ARGUEMENTS
C-----------------------------------------------------------------------------------
C FC_index,         ! pointer to fuel cell 
C FC_type,          ! flag indicating fuel cell type 
C TAC_index,        ! pointer to TAC 
C COGEN_max_W,      ! Cogen-system maximum power 
C COGEN_min_W,      ! Cogen-system minimum power 
C COGEN_con_W,      ! cogen-system constant electrical operating point 
C Thermal_p,        ! Flag indicating priority tank 
C HWT_index,        ! Pointer to hot water tank
C HWT_node,         ! Pointer to hot water tank water node
C CWT_index,        ! Pointer to cold water tank
C CWT_node,         ! Pointer to cold water tank water node
C HS_start,         ! Heating season start day
C HS_end,           ! Heating season end-day
C CS_start,         ! Cooling season start day
C CS_end,           ! Cooling season end day
C HWT_t_sl,         ! hot water tank lower setpoint 
C HWT_T_su,         ! hot water tank upper setpoint
C HWT_algorithm,    ! flag for hot water tank control algorithm
C HWT_coeff_a,      ! Target coefficient for hot water tank
C HWT_coeff_Kp,     ! hot water tank proportional gain  
C HWT_coeff_Ki,     ! Hot water tank integral gain   
C HWT_coeff_Kd,     ! Hot water tank derivitive gain
C HWT_per_int,      ! Hot water tank integral period
C HWT_per_der,      ! Hot water tank derivitive period
C CWT_T_sl,         ! Cold water tank lower setpoint  
C CWT_T_su,         ! Cold water tank upper setpoint 
C CWT_algorithm,    ! flag for cold water tank control algorithm
C CWT_coeff_a,      ! Cold water tank target coefficient
C CWT_coeff_Kp,     ! Cold water tank porportional gain 
C CWT_coeff_Ki,     ! Cold water tank integral gain  
C CWT_coeff_Kd,     ! Cold water tank derivitive gain
C CWT_per_int,      ! Cold water tank integral period
C CWT_per_der,      ! Cold water tank derivitive period
C HWT_T_target,     ! Hot water tank target temperature
C CWT_T_target,     ! Cot water tank target temperature
C iTS_out,          ! Flag indicating timestep output is req'd
C fatal_error       ! Flag indicating simulation must be halted
C bCW_tank          ! Flag indicating if hot water tank exists,
C bHW_tank          ! Flag indicating if cold water tank exists.
      SUBROUTINE  DG_controller_initialize(  
     &     FC_index, FC_type, TAC_index, COGEN_max_W, COGEN_min_W, 
     &     COGEN_con_W, Thermal_p, HWT_index, HWT_node, CWT_index, 
     &     CWT_node, HS_start, HS_end, CS_start, CS_end, hwt_t_sl, 
     &     HWT_T_su, HWT_algorithm, HWT_coeff_a, HWT_coeff_Kp, 
     &     HWT_coeff_Ki, HWT_coeff_Kd, HWT_per_int, HWT_per_der, 
     &     CWT_T_sl, CWT_T_su, CWT_algorithm, CWT_coeff_a, CWT_coeff_Kp,  
     &     CWT_coeff_Ki, CWT_coeff_Kd, CWT_per_int, CWT_per_der, 
     &     HWT_T_target, CWT_T_target, iTS_out, fatal_error, 
     &     bHW_tank,bCW_tank)

      IMPLICIT NONE

      INCLUDE 'plant.h'
      INCLUDE 'building.h'
      INCLUDE 'DG_controller_include.h'
C-------------------------------------------------------------------------------------
C     Required common blocks
C-------------------------------------------------------------------------------------
      common/outin/iuout,iuin
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/ptime/ptimep,ptimef
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/pclsol/icfp,idtypp,iperp
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),npcdt(mcl),
     &            ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),
     &            tpcps(mcl,mpcdt,mpcdp),ipctyp(mcl,mpcdt,mpcdp),
     &            ipclaw(mcl,mpcdt,mpcdp),pmiscd(mcl,mpcdt,mpcdp,misc)

      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar) 
      common/pdbdt/adata(mpcom,madata),bdata(mpcom,mbdata)
      common/entfile/entflnam, ientxist
C------------------------------------------------------------------------------------
C     Declare common block variables
C------------------------------------------------------------------------------------
      integer iuout,iuin
C.....Simulation time
      integer ihrp, ihrf, idyp, idyf, idwp, idwf,nsinc,its
      real ptimep, ptimef
      integer npcomp, nci
      real cdata
      integer icfp, idtypp, iperp
      integer ncl, ipsn, ipan, npcdt, ipcdv, npcdp
      real tpcps
C.....Control data
      integer ipctyp, ipclaw
      real pmiscd
C.....Plant component indexes
      integer npcdat, ipofs1, ipofs2
C.....Plant component parameters
      real adata, bdata
C.....Electrical network
      character*72 entflnam  
      integer ientxist
C----------------------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------------------
      integer Number_TACs       ! Counter for TAC systems
      integer Number_FCs        ! Counter for fuel cell systems
      integer hold_node         ! temp. variable used to recover data
      

C-------------------------------------------------------------------------------------
C     Check to see if sufficient data (7 items) have been provided.
C-------------------------------------------------------------------------------------
         if(int(pmiscd(icfp,idtypp,iperp,1)).lt.7)then
            write(iuout,*)
     &           ' Cogeneration system controller: fatal error!'
            write(iuout,*)
     &           ' Incorrect number of control data specified.'
            write(iuout,*)
     &           ' at least 7 items were expected, but ',
     &           pmiscd(icfp,idtypp,iperp,1), ' items were found'
            call EPWAIT
            call EPAGend
            fatal_error= .true.
         endif
C---------------------------------------------------------------------------------
C     Determine component number for fuel cell system
C---------------------------------------------------------------------------------
         Number_FCs = 0         ! initialize counter to zero
         do ii=1, NPCOMP
            if ( NPCDAT(ii,4) .eq.  80 .or.
     &           NPCDAT(ii,4) .eq.  86 .or.
     &           NPCDAT(ii,4) .eq. 102 .OR.
     &           NPCDAT(ii,4) .eq. 103      )then
               Number_FCs = Number_FCs + 1 ! increment counter
               FC_index = ii    ! set controller pointer to current i
C..............Save model type for later use
               if(NPCDAT(ii,4) .eq.  80 ) FC_TYPE = SOFC_model
               if(NPCDAT(ii,4) .eq.  86 ) FC_TYPE = PEFC_model
               if(NPCDAT(ii,4) .eq. 102 ) FC_TYPE = Stirling_model
               if(NPCDAT(ii,4) .eq. 103 ) FC_TYPE = Stirling_model
            end if     
         end do
         if(Number_FCs.eq.0) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'NO cogeneration unit found in plant configuration ',
     &           'file.'
            fatal_error = .true.
         elseif(Number_FCs.gt.1) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'only one cogeneration unit is supported, but ',
     &           Number_FCs, ' units were found in plant ',
     &           'configuration file.'
            fatal_error = .true.
         endif

C---------------------------------------------------------------------------------
C     Determine component number for TAC system
C---------------------------------------------------------------------------------
         Number_TACs = 0        ! initialize counter to zero
         do ii=1, NPCOMP 
            if ( NPCDAT(ii,4) .eq. 84 ) then
               Number_TACs = Number_TACs + 1 ! increment counter
               TAC_index = ii   ! set controller pointer to current i
            end if     
         end do
C........Note: TAC system is optional...
         if(Number_TACs.eq.0) then
            TAC_index = -1     
         elseif(Number_TACs.gt.1) then
C...........But there can't be more than one!
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'only one TAC unit is supported, but ',
     &           Number_TACs, ' units were found in plant ',
     &           'configuration file.'
            fatal_error = .true.
         endif

C---------------------------------------------------------------------------------
C     Assign user-specified inputs. 
C---------------------------------------------------------------------------------

C.....Data from cogen system
      select case (FC_TYPE)
      case (SOFC_model)
         COGEN_max_W = adata(FC_index,26) ! COGEN system max output (W)
         COGEN_min_W = adata(FC_index,25) ! cogen system min output (W) 

      case (PEFC_model)
         COGEN_max_W = adata(FC_index,2) ! COGEN system max output (W)
         COGEN_min_W = adata(FC_index,2)*adata(FC_index,3)
                                ! cogen system min output (W)
      case (Stirling_model)
         COGEN_max_W = adata(FC_index,1)
C........Scale max output by ratio between min & max fuel flow to get
C........Minumum power output          
         COGEN_min_W = COGEN_max_W
     &      * adata(FC_index,3) / adata(FC_index,2)
      
      end select

      COGEN_con_W = pmiscd(icfp,idtypp,iperp,2) ! cogen system constant output (W)
C.....Warnings
      if(COGEN_con_W .gt. COGEN_max_W ) then
         write(IUOUT,*) ' Cogeneration system controller: ',
     &        'WARNING: Specified cogen system constant output',
     &        ' (',COGEN_con_W,' W) is larger than cogen system ',
     &        ' maximum output (',COGEN_max_W,' W). Using maximum ',
     &        ' output instead.'
         COGEN_con_W = COGEN_max_W
      elseif(COGEN_con_W.lt.COGEN_min_W .and. 
     &        .not.(COGEN_con_W.lt.0) )  then
         write(IUOUT,*) ' Cogeneration system controller: ',
     &        'WARNING: Specified cogen system constant output',
     &        ' (',COGEN_con_W,' W) is smaller than cogen system ',
     &        ' minimum output (',COGEN_min_W,'). '
         COGEN_con_W = COGEN_min_W
      endif
      


C.....Thermal load following priority (1 = cooling system, 2=heating system)
      Thermal_p = int(pmiscd(icfp,idtypp,iperp,3))
      if(Thermal_p.lt.0 .or. Thermal_p.gt.2) then
         write(IUOUT,*) ' Cogenertion system controller',
     &        'Specified thermal priority is invalid. Please ',
     &        'specify a value of 0, 1 or 2.'
         fatal_error = .true.
      endif

C.....Hot and cold storage tank pointers
      HWT_index = int(pmiscd(icfp,idtypp,iperp,4)) ! -1 for none
      hold_node = int(pmiscd(icfp,idtypp,iperp,5)) ! -1 for none
C.....Does HWT exist?      
      if (HWT_index .lt. 0 ) then
         bHW_tank = .false.
      else
         bHW_tank = .true.
      endif
      
      if ( hold_node .gt. 0 .AND. HWT_index .gt. 0) then
C........Get tank one number: npcdat(i,9) holds the index of the
C........first node of plant component i, we need to 
C........increment this counter by (i-1) to get the apporpriate
C........node index.         
         HWT_node = npcdat(HWT_index,9)+(hold_node-1)
      else 
         HWT_node = -1 
      endif
      CWT_index = int(pmiscd(icfp,idtypp,iperp,6)) ! -1 for none
      hold_node = int(pmiscd(icfp,idtypp,iperp,7)) ! -1 for none
C.....Does CWT exist?
      if (CWT_index .lt. 0 ) then
         bCW_tank = .false.
      else
         bCW_tank = .true.
      endif
  
      if ( hold_node .gt. 0 .AND. CWT_index .gt. 0) then
C........Get tank none number: npcdat(i,9) holds the index of the 
C........first node of plant component i, we need to 
C........increment this counter by (i-1) to get the apporpriate
C........node index.         
         CWT_node = npcdat(CWT_index,9)+(hold_node-1)
      else 
         CWT_node = -1 
      endif

      if (bHW_tank .and. HWT_node .lt. 0. ) then
         write(IUOUT,*) ' Cogeneration system controller: ',
     &        'HWT node index is invalid. Please',
     &        'specify a value between 1 and 4.'
         fatal_error = .true.
      endif
      if (bCW_tank .and. CWT_node .lt. 0. ) then
         write(IUOUT,*) ' Cogeneration system controller: ',
     &        'CWT node index is invalid. Please',
     &        'specify a value between 1 and 4.'
         fatal_error = .true.
      endif

C.....Hot storage control data 
      if ( bHW_tank ) then
         HS_start = int(pmiscd(icfp,idtypp,iperp,8)) ! Heating season start (1-365)
         HS_end   = int(pmiscd(icfp,idtypp,iperp,9)) ! Heating season end   (1-365)
         if(HS_start .lt. 1.or. HS_start .gt. 366) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Heating season start day is invalid. Please ',
     &           'specify a value between 1 and 366.'
            fatal_error = .true.
         endif
         if(HS_end .lt. 1.or. HS_end .gt. 366) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Heating season end day is invalid. Please ',
     &           'specify a value between 1 and 366.'
            fatal_error = .true.
         endif

         HWT_algorithm = INT(pmiscd(icfp,idtypp,iperp,10)) ! hot storage algorithm:
                                !   0 = FREE FLOAT
                                !   1 = Constant electrical output
                                !   2 = electrical load following
                                !   3 = PID (thermal following)
                                !   4 = PID (thermal priority)
         if(HWT_algorithm .lt. 0.or. HWT_algorithm .gt. 4) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Specified hot thermal control algorithm is ',
     &           'unsupported. Please specify a value between 0 and 4.'
            fatal_error = .true.
         endif
         HWT_T_sl = pmiscd(icfp,idtypp,iperp,11) ! Hot tank lower setpoint (oC)
         HWT_T_su = pmiscd(icfp,idtypp,iperp,12) ! Hot tank upper setpoint (oC)
         if(HWT_T_su .lt. HWT_T_sl) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Hot thermal storage upper set point is below ',
     &           'lower setpoint.'
            fatal_error = .true.
         endif         

         HWT_coeff_a  = pmiscd(icfp,idtypp,iperp,13) ! Coefficient for target temp (0<a<1)
         if(HWT_coeff_a .gt. 1 .or. HWT_coeff_a .lt. 0) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Hot thermal storage target coefficient must be ',
     &           'between 0 and 1.'
            fatal_error = .true.
         endif

         HWT_coeff_Kp = pmiscd(icfp,idtypp,iperp,14) ! Proportional gain (0<Kp<oo)
         HWT_coeff_Ki = pmiscd(icfp,idtypp,iperp,15) ! Integral gain (0<Ki<oo) 
         HWT_coeff_Kd = pmiscd(icfp,idtypp,iperp,16) ! Derivitive gain (0<Kd<oo)
         HWT_per_int  = pmiscd(icfp,idtypp,iperp,17) ! Integral period (hours)
         HWT_per_der  = pmiscd(icfp,idtypp,iperp,18) ! Derivitive period (hours)     
C........Set counter for start of cold tank data
         ii = 18
      else
         ii = 8
      endif

      if ( bCW_tank ) then
C........Cold storage control data 
         CS_start = int(pmiscd(icfp,idtypp,iperp,ii+1)) ! Cooling season start (1-365)
         CS_end   = int(pmiscd(icfp,idtypp,iperp,ii+2)) ! Cooling season end   (1-365)
         if(CS_start .lt. 1.or. CS_start .gt. 366) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Cooling season start day is invalid. Please ',
     &           'specify a value between 1 and 366.'
            fatal_error = .true.
         endif
         if(CS_end .lt. 1.or. CS_end  .gt. 366) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Cooling season end day is invalid. Please ',
     &           'specify a value between 1 and 366.'
            fatal_error = .true.
         endif


         CWT_algorithm = INT(pmiscd(icfp,idtypp,iperp,ii+3)) ! Cold storage algorithm:
                                !   0 = FREE FLOAT
                                !   1 = Constant electrical output
                                !   2 = electrical load following
                                !   3 = PID (thermal following)
                                !   4 = PID (thermal priority)
         if(CWT_algorithm .lt. 0.or. CWT_algorithm .gt. 4) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Specified cold thermal control algorithm is ',
     &           'unsupported.'
            fatal_error = .true.
         endif      

         CWT_T_sl = pmiscd(icfp,idtypp,iperp,ii+4) ! Cold tank lower setpoint (oC)
         CWT_T_su = pmiscd(icfp,idtypp,iperp,ii+5) ! Cold tank upper setpoint (oC)
         if(CWT_T_su .lt. CWT_T_sl) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'cold thermal storage upper set point is below ',
     &           'lower setpoint.'
            fatal_error = .true.
         endif 
         CWT_coeff_a  = pmiscd(icfp,idtypp,iperp,ii+6) ! Coefficient for target temp (0<a<1)
         if(CWT_coeff_a .gt. 1 .or. CWT_coeff_a .lt. 0) then
            write(IUOUT,*) ' Cogeneration system controller: ',
     &           'Hot thermal storage target coefficient must be ',
     &           'between 0 and 1.'
            fatal_error = .true.
         endif
         CWT_coeff_Kp = pmiscd(icfp,idtypp,iperp,ii+7) ! Proportional gain (0<Kp<oo)
         CWT_coeff_Ki = pmiscd(icfp,idtypp,iperp,ii+8) ! Integral gain (0<Ki<oo) 
         CWT_coeff_Kd = pmiscd(icfp,idtypp,iperp,ii+9) ! Derivitive gain (0<Kd<oo)
         CWT_per_int = pmiscd(icfp,idtypp,iperp,ii+10) ! Integral period (hours)
         CWT_per_der = pmiscd(icfp,idtypp,iperp,ii+11) ! Derivitive period (hours)
      endif
C.....Timestep output
      iTS_out =int(pmiscd(icfp,idtypp,iperp,ii+12))
      if(iTS_out .lt. 0 .or. iTS_out .gt. 1) then
         write(IUOUT,*) ' Cogeneration system controller: ',
     &        'WARNING: Timestep output flag value not valid. ',
     &        'Timestep data will not be output.'
         iTS_out = 0
      endif

C---------------------------------------------------------------------------------
C     SET target temperatures (which are time invariant). See Equation 9,
C     Ferguson 2003.
C---------------------------------------------------------------------------------
      CWT_T_target = CWT_T_sl + CWT_coeff_a*(CWT_T_su - CWT_T_sl)
      HWT_T_target = HWT_T_sl + HWT_coeff_a*(HWT_T_su - HWT_T_sl)

C--------------------------------------------------------------------------------
C     Output configuration for debugging, if req'd
C--------------------------------------------------------------------------------
      config_output = .false.
      if ( config_output .or. fatal_error ) then
         write(iuout,*) ' Distributed generation controller config: '
         write(iuout,*) ' Max electric output   :', COGEN_max_W
         write(iuout,*) ' Min electric ouptut   :', COGEN_min_W 
         write(iuout,*) ' Consant elec. output  :', COGEN_con_W
         write(iuout,*) ' HWT index             :', HWT_index
         write(iuout,*) ' HWT node              :', HWT_node
         write(iuout,*) ' CWT index             :', CWT_index
         write(iuout,*) ' CWT node              :', CWT_node
         if (Thermal_p .eq. Hot_storage) then
            write(iuout,*) ' Thermal load priority : Hot storage'
         else 
            write(iuout,*) ' Thermal load priority : Cold storage'            
         endif
         if ( bHW_tank ) then
            write(iuout,*) ' Heating season -'
            write(iuout,*) '   Start               :', HS_start
            write(iuout,*) '   End                 :', HS_end
            write(iuout,*) '   HWT algorithm       :', HWT_algorithm
            write(iuout,*) '   Lower setpoint      :', HWT_T_sl
            write(iuout,*) '   Upper setpoint      :', HWT_T_su
            write(iuout,*) '   Target coeff.       :', HWT_coeff_a
            write(iuout,*) '   HWT Kp              :', HWT_coeff_Kp
            write(iuout,*) '   HWT Ki              :', HWT_coeff_Ki
            write(iuout,*) '   HWT Kd              :', HWT_coeff_Kd
            write(iuout,*) '   HWT integral period :', HWT_per_int 
            write(iuout,*) '   HWT deriv. period   :', HWT_per_der
         endif
         if ( bCW_tank ) then
            write(iuout,*) ' Cooling season -'
            write(iuout,*) '   Start               :', CS_start
            write(iuout,*) '   End                 :', CS_end
            write(iuout,*) '   CWT algorithm       :', CWT_algorithm
            write(iuout,*) '   Lower setpoint      :', CWT_T_sl
            write(iuout,*) '   Upper setpoint      :', CWT_T_su
            write(iuout,*) '   Target coeff.       :', CWT_coeff_a
            write(iuout,*) '   CWT Kp              :', CWT_coeff_Kp
            write(iuout,*) '   CWT Ki              :', CWT_coeff_Ki
            write(iuout,*) '   CWT Kd              :', CWT_coeff_Kd
            write(iuout,*) '   CWT integral period :', CWT_per_int 
            write(iuout,*) '   CWT deriv. period   :', CWT_per_der
            write(iuout,*) ' Output flag           :', iTS_out 
         endif
      endif

C--------------------------------------------------------------------------------
C     Convert user-specified electric loads into binary file useful for 
C     simulation. Then determine the component numbers of fans and pumps appearing 
C     plant network file.
C     
C     Use the 'FC_eloads_convert' and 'FC_pump_fan_comp_number' routines 
C     written by Ian Beausoleil-Morrison for the CETC fuel cell model.
C     (The fuel cell model coefficient generator has been modified to ensure
C     that these routines are NOT called if the external contoller model
C     is present in the plant network - thus preventing the loads binary file 
C     from being produced twice)
C--------------------------------------------------------------------------------
      if ( IENTXIST .le. 0 ) then
C........Only call thse routines if electrical network is not present
         call FC_eloads_convert
         call FC_pump_fan_comp_number
      endif

C---------------------------------------------------------------------------------
C     Completion of time-invariant operations for distributed generation 
C     controller component. Note: if fatal_error has been set, the calling 
C     subroutine will be halted
C---------------------------------------------------------------------------------
      end




C***************** DG_manage_thermal_loads  ****************************************
C Author:  Alex Ferguson
C Creation date: May 1, 2003
C Copyright CETC 2003
C
C This subroutine estimates the thermal loads on the hot water tank, assigns 
C priority to the tank requiring heating or cooling, and calculates the the 
C control signal necessary to meet the loads on the hot and cold thermal storages
C 
C It passes back two variables:
C      STO_priority           : Describes which storage the TAC system should be
C                               operated to meet
C      thermal_control_signal : holds the cogen system operating point determined
C                               by the PID load following algorithm.
C
C The subroutine makes use of numerous varibles contained in the DG_control_module:
C   - The status of the hot and cold thermal storage (XXX_status)
C   - Temperature of the hot and cold water tanks (XXX_T)
C   - Upper and lower setpoints for water tanks (XXX_T_su / XXX_T_sl)
C   - Named constants STA_primary, STA_auxiliary, STA_freefloat, cold_storage
C     hot_storage
C*********************************************************************************
C iterp,         ! flag indicating plant solution is in iteration
C CWT_index,     ! pointer to cold water tank (Input)
C HWT_index,     ! pointer to hot water tank        
C CWT_T,         ! Cold water tank temperature            
C CWT_T_target,  ! Cold water tank target temperature 
C CWT_T_su,      ! Cold water tank upper setpoint 
C CWT_T_sl,      ! Cold water tank lower setpoint
C HWT_T,         ! Hot water tank temperature
C HWT_T_target,  ! Hot water tank target temperature
C HWT_T_su,      ! hot water tank upper setpoint
C hwt_t_sl,      ! hot water tank lower setpoint
C HWT_coeff_Kp,  ! hot water tank proportional gain
C HWT_coeff_Ki,  ! Hot water tank integral gain
C HWT_coeff_Kd,  ! Hot water tank derivitive gain
C HWT_per_int,   ! Hot water tank integral period
C HWT_per_der,   ! Hot water tank derivitive period
C CWT_coeff_Kp,  ! cold water tank proportional gain
C CWT_coeff_Ki,  ! Cold water tank integral gain
C CWT_coeff_Kd,  ! Cold water tank derivitive gain
C CWT_per_int,   ! Cold water tank integral period
C CWT_per_der,   ! Cold water tank derivitive period
C CWT_Status,    ! Priority status of cold water tank
C HWT_Status,    ! Priority status of hot water tank
C CWT_error,     ! Cold water tank error coefficient      
C CWT_Control,   ! Cold water tank control signal
C HWT_error,     ! Hot water tank error coefficient
C HWT_control,   ! Hot water tank control signal
C STO_priority,  ! Storage tank thermal priority        
C thermal_control_signal
      SUBROUTINE DG_manage_thermal_loads(
     &     iterp, CWT_index, HWT_index, CWT_T, CWT_T_target, CWT_T_su, 
     &     CWT_T_sl, HWT_T, HWT_T_target, HWT_T_su, hwt_t_sl,  
     &     HWT_coeff_Kp,HWT_coeff_Ki, HWT_coeff_Kd, HWT_per_int,  
     &     HWT_per_der,CWT_coeff_Kp, CWT_coeff_Ki, CWT_coeff_Kd, 
     &     CWT_per_int, CWT_per_der, CWT_Status, HWT_Status, CWT_error,  
     &     CWT_Control,HWT_error, HWT_control, STO_priority, 
     &     thermal_control_signal 
     &     )
      IMPLICIT NONE 

C--------------------------------------------------------------------------------
C     Common declarations
C--------------------------------------------------------------------------------
      INCLUDE 'DG_controller_include.h'


C--------------------------------------------------------------------------------
C     Local declarations
C--------------------------------------------------------------------------------
      INTEGER iterp               ! flag indicating that plt. sol'n is in iteration

C--------------------------------------------------------------------------------
C     Calculate hot and cold water tank error coefficients:
C            
C                        CWT_T - CWT_T_target
C            CWT_error = --------------------
C                         CWT_T_su - CWT_T 
C
C                        HWT_T_target - HWT_T
C            HWT_error = ---------------------
C                        HWT_target - HWT_T_sl
C
C     Refer to Equation 10, Ferguson (2003)
C--------------------------------------------------------------------------------- 
      if ( CWT_index .gt. 0 ) then
         CWT_error = (CWT_T-CWT_T_target)/
     &        (CWT_T_su - CWT_T_target)
      endif
      if ( HWT_index .gt. 0 ) then
         HWT_error = (HWT_T_target-HWT_T)/
     &        (HWT_T_target-HWT_T_sl)                        
      endif
C---------------------------------------------------------------------------------
C     Determine which inventory is primary storage. See section 3.4.3, Ferguson
C     (2003)
C---------------------------------------------------------------------------------
      IF(CWT_status .eq. STA_primary) THEN
C---------------------------------------------------------------------------------        
C        Cold tank is the primary inventory
C---------------------------------------------------------------------------------
         IF(CWT_error .gt. Error_threshold
     &        .OR.CWT_error.ge.HWT_error
     &        .OR.HWT_status .eq. STA_freefloat) THEN
C---------------------------------------------------------------------------------
C           Either:
C              CWT error is greater than threshold OR
C              CWT error is greater than HWT error OR
C              there are no loads on the HWT.
C
C           Cold storage system has priority - Call DG_thermal_PID
C           routine to determine cogen system control signal.
C
C           Sypnopsys:
C
C           DG_thermal_PID(storage_type, error_signal, control_signal)
C
C           DG_thermal_PID applies PID control to the tank indicated by storage_type.
C           Given error_signal, it calculates control_signal. It also makes use of 
C           variables stored in DG_control_module
C---------------------------------------------------------------------------------        
            STO_priority = cold_storage
            CALL DG_thermal_PID(
     &           iterp,         ! flag indicating plant solution is in iteration 
     &           HWT_coeff_Kp,  ! hot water tank proportional gain
     &           HWT_coeff_Ki,  ! Hot water tank integral gain
     &           HWT_coeff_Kd,  ! Hot water tank derivitive gain
     &           HWT_per_int,   ! Hot water tank integral period
     &           HWT_per_der,   ! Hot water tank derivitive period
     &           CWT_coeff_Kp,  ! Cold water tank porportional gain 
     &           CWT_coeff_Ki,  ! Cold water tank integral gain
     &           CWT_coeff_Kd,  ! Cold water tank derivitive gain
     &           CWT_per_int,   ! Cold water tank integral period
     &           CWT_per_der,   ! Cold water tank derivitive period  
     &           cold_storage,  ! Flag indicating tank of interest
     &           CWT_error,     ! Cold water tank error coefficient
     &           CWT_control    ! Cold water tank control (output)
     &           )
C---------------------------------------------------------------------------------
C           Check to make sure that control signal is between 0 and 1, 
C           and assign to DG_control signal variable.
C---------------------------------------------------------------------------------
            IF (CWT_control.gt.1.) THEN 
               thermal_control_signal = 1.0
            ELSEIF(CWT_control.lt.0.) THEN
               thermal_control_signal = 0.0
            ELSE
               thermal_control_signal = CWT_control
            ENDIF
            
            IF(HWT_status .eq. STA_auxiliary) THEN
C---------------------------------------------------------------------------------
C              HWT tank is also loaded. 
C              Call DG_thermal_PID control routine for hot storage 
C              tank. This is used to populate the hot storage
C              integral and derivative arrays, permitting 
C              resumption of hot storage control next time 
C              HWT tank is assigned priority. For this time step, 
C              the returned control signal (HWT_control) is ignored.
C---------------------------------------------------------------------------------   
               CALL DG_thermal_PID(
     &              iterp,      ! flag indicating plant solution is in iteration 
     &              HWT_coeff_Kp, ! hot water tank proportional gain
     &              HWT_coeff_Ki, ! Hot water tank integral gain
     &              HWT_coeff_Kd, ! Hot water tank derivitive gain
     &              HWT_per_int, ! Hot water tank integral period
     &              HWT_per_der, ! Hot water tank derivitive period
     &              CWT_coeff_Kp, ! Cold water tank porportional gain 
     &              CWT_coeff_Ki, ! Cold water tank integral gain
     &              CWT_coeff_Kd, ! Cold water tank derivitive gain
     &              CWT_per_int, ! Cold water tank integral period
     &              CWT_per_der, ! Cold water tank derivitive period  
     &              hot_storage, ! flag indicating which tank is to be used
     &              HWT_error,  ! Hot water tank error coefficient
     &              HWT_control ! hot water tank control (output)
     &           )
            ENDIF            
         ELSE
C---------------------------------------------------------------------------------
C           Hot storage system has priority -  Call DG_thermal_PID
C           routine to determine cogen system control signal.
C---------------------------------------------------------------------------------  
            STO_priority = hot_storage
               CALL DG_thermal_PID(
     &           iterp,         ! flag indicating plant solution is in iteration
     &           HWT_coeff_Kp,  ! hot water tank proportional gain
     &           HWT_coeff_Ki,  ! Hot water tank integral gain
     &           HWT_coeff_Kd,  ! Hot water tank derivitive gain
     &           HWT_per_int,   ! Hot water tank integral period
     &           HWT_per_der,   ! Hot water tank derivitive period
     &           CWT_coeff_Kp,  ! Cold water tank porportional gain 
     &           CWT_coeff_Ki,  ! Cold water tank integral gain
     &           CWT_coeff_Kd,  ! Cold water tank derivitive gain
     &           CWT_per_int,   ! Cold water tank integral period
     &           CWT_per_der,   ! Cold water tank derivitive period  
     &           hot_storage,   ! flag indicating which tank is to be used
     &           HWT_error,     ! Hot water tank error coefficient
     &           HWT_control    ! hot water tank control (output)
     &           )
C---------------------------------------------------------------------------------
C           Check to make sure that control signal is between 0 and 1, 
C           and assign to DG_control signal variable.
C---------------------------------------------------------------------------------
            IF (HWT_control.gt.1.) THEN 
               thermal_control_signal = 1.0
            ELSEIF(HWT_control .lt.0.) THEN
               thermal_control_signal = 0.0
            ELSE
               thermal_control_signal = HWT_control
            ENDIF

C---------------------------------------------------------------------------------
C           Call DG_thermal_PID control routine for cold storage 
C           tank. This is used to populate the hot storage
C           integral and derivative arrays, permitting 
C           resumption of hot storage control when the 
C           control signal drops below zero agian. (we don't have to 
C           check the tank's status b/c we already know it's the 
C           primary tank
C---------------------------------------------------------------------------------   
            CALL DG_thermal_PID(
     &           iterp,         ! flag indicating plant solution is in iteration 
     &           HWT_coeff_Kp,  ! hot water tank proportional gain
     &           HWT_coeff_Ki,  ! Hot water tank integral gain
     &           HWT_coeff_Kd,  ! Hot water tank derivitive gain
     &           HWT_per_int,   ! Hot water tank integral period
     &           HWT_per_der,   ! Hot water tank derivitive period
     &           CWT_coeff_Kp,  ! Cold water tank porportional gain 
     &           CWT_coeff_Ki,  ! Cold water tank integral gain
     &           CWT_coeff_Kd,  ! Cold water tank derivitive gain
     &           CWT_per_int,   ! Cold water tank integral period
     &           CWT_per_der,   ! Cold water tank derivitive period  
     &           cold_storage,  ! Flag indicating tank of interest
     &           CWT_error,     ! Cold water tank error coefficient
     &           CWT_control    ! cold water tank control (output)
     &           )           
         ENDIF
      ELSEIF(HWT_status .eq. STA_primary) THEN
C---------------------------------------------------------------------------------        
C        Hot tank is the primary inventory
C---------------------------------------------------------------------------------
         IF(HWT_error.gt.Error_threshold
     &        .OR.HWT_error.ge.CWT_error
     &        .OR.CWT_status.eq.STA_freefloat) THEN
C---------------------------------------------------------------------------------
C        Either:
C           HWT error is greater than threshold OR
C           HWT error is greater than CWT error OR
C           there are no loads on the CWT.
C
C        Hot storage system has priority - Call DG_thermal_PID
C        routine to determine cogen system control signal.
C---------------------------------------------------------------------------------        
            STO_priority = hot_storage
            CALL DG_thermal_PID(
     &           iterp,         ! flag indicating plant solution is in iteration 
     &           HWT_coeff_Kp,  ! hot water tank proportional gain
     &           HWT_coeff_Ki,  ! Hot water tank integral gain
     &           HWT_coeff_Kd,  ! Hot water tank derivitive gain
     &           HWT_per_int,   ! Hot water tank integral period
     &           HWT_per_der,   ! Hot water tank derivitive period
     &           CWT_coeff_Kp,  ! Cold water tank porportional gain 
     &           CWT_coeff_Ki,  ! Cold water tank integral gain
     &           CWT_coeff_Kd,  ! Cold water tank derivitive gain
     &           CWT_per_int,   ! Cold water tank integral period
     &           CWT_per_der,   ! Cold water tank derivitive period  
     &           hot_storage,   ! flag indicating which tank is to be used
     &           HWT_error,     ! Hot water tank error coefficient
     &           HWT_control    ! Hot water tank control (output) 
     &           )
C---------------------------------------------------------------------------------
C        Check to make sure that control signal is between 0 and 1, 
C        and assign to DG_control signal variable.
C---------------------------------------------------------------------------------
            IF (HWT_control.gt.1.) THEN 
               thermal_control_signal = 1.0
            ELSEIF(HWT_control.lt.0.) THEN
               thermal_control_signal = 0.0
            ELSE
               thermal_control_signal = HWT_control
            ENDIF

            IF(CWT_status .eq. STA_auxiliary) THEN
C---------------------------------------------------------------------------------
C           CWT tank is also loaded. 
C           Call DG_thermal_PID control routine for hot storage 
C           tank. This is used to populate the hot storage
C           integral and derivative arrays, permitting 
C           resumption of hot storage control next time 
C           CWT tank is assigned priority.
C---------------------------------------------------------------------------------
               CALL DG_thermal_PID(
     &              iterp,      ! flag indicating plant solution is in iteration 
     &              HWT_coeff_Kp, ! hot water tank proportional gain
     &              HWT_coeff_Ki, ! Hot water tank integral gain
     &              HWT_coeff_Kd, ! Hot water tank derivitive gain
     &              HWT_per_int, ! Hot water tank integral period
     &              HWT_per_der, ! Hot water tank derivitive period
     &              CWT_coeff_Kp, ! Cold water tank porportional gain 
     &              CWT_coeff_Ki, ! Cold water tank integral gain
     &              CWT_coeff_Kd, ! Cold water tank derivitive gain
     &              CWT_per_int, ! Cold water tank integral period
     &              CWT_per_der, ! Cold water tank derivitive period  
     &              cold_storage, ! Flag indicating tank of interest
     &              CWT_error,  ! Cold water tank error coefficient
     &              CWT_control ! cold water tank control (output)
     &              )
            ENDIF
         ELSE
C---------------------------------------------------------------------------------
C           Cold storage system has priority  -  Call DG_thermal_PID
C           routine to determine cogen system control signal.
C---------------------------------------------------------------------------------  
            STO_priority = cold_storage
            CALL DG_thermal_PID(
     &           iterp,         ! flag indicating plant solution is in iteration
     &           HWT_coeff_Kp,  ! hot water tank proportional gain
     &           HWT_coeff_Ki,  ! Hot water tank integral gain
     &           HWT_coeff_Kd,  ! Hot water tank derivitive gain
     &           HWT_per_int,   ! Hot water tank integral period
     &           HWT_per_der,   ! Hot water tank derivitive period
     &           CWT_coeff_Kp,  ! Cold water tank porportional gain 
     &           CWT_coeff_Ki,  ! Cold water tank integral gain
     &           CWT_coeff_Kd,  ! Cold water tank derivitive gain
     &           CWT_per_int,   ! Cold water tank integral period
     &           CWT_per_der,   ! Cold water tank derivitive period  
     &           cold_storage,  ! Flag indicating tank of interest
     &           CWT_error,     ! Cold water tank error coefficient
     &           CWT_control    ! cold water tank control (output)
     &           )
C---------------------------------------------------------------------------------
C           Check to make sure that control signal is between 0 and 1, 
C           and assign to DG_control signal variable.
C---------------------------------------------------------------------------------
            IF (CWT_control.gt.1.) THEN 
               thermal_control_signal = 1.0
            ELSEIF(CWT_control .lt.0.) THEN
               thermal_control_signal = 0.0
            ELSE
               thermal_control_signal = CWT_control
            ENDIF
C---------------------------------------------------------------------------------
C           Call DG_thermal_PID control routine for hot storage 
C           tank. This is used to populate the hot storage
C           integral and derivative arrays, permitting 
C           resumption of hot storage control when the 
C           control signal drops below zero agian. (we don't have to 
C           check the tank's status b/c we already know it's the 
C           primary tank
C---------------------------------------------------------------------------------   
            CALL DG_thermal_PID(
     &           iterp,         ! flag indicating plant solution is in iteration 
     &           HWT_coeff_Kp,  ! hot water tank proportional gain
     &           HWT_coeff_Ki,  ! Hot water tank integral gain
     &           HWT_coeff_Kd,  ! Hot water tank derivitive gain
     &           HWT_per_int,   ! Hot water tank integral period
     &           HWT_per_der,   ! Hot water tank derivitive period
     &           CWT_coeff_Kp,  ! Cold water tank porportional gain 
     &           CWT_coeff_Ki,  ! Cold water tank integral gain
     &           CWT_coeff_Kd,  ! Cold water tank derivitive gain
     &           CWT_per_int,   ! Cold water tank integral period
     &           CWT_per_der,   ! Cold water tank derivitive period  
     &           hot_storage,   ! flag indicating which tank is to be used
     &           HWT_error,     ! Hot water tank error coefficient
     &           HWT_control    ! hot water tank control (output)   
     &           )
        
         ENDIF
      ENDIF

      RETURN
      END

C*******************************  DG_thermal_PID     *****************************
C
C Created by: Alex Ferguson
C Initial Creation Date: April 09, 2003.
C Copyright CETC 2003
C
C This subroutine applies PID control to an error signal passed to it from 
C DG_controller_coeff_generator. It also makes use of variables stored in 
C DG_controller_module.
C
C Inputs: 
C  - mode    : flag indicating if the PID algorithm should be applied to:
C  - error   : Error signal calculated for hot/cold storage
C Outputs: 
C  - control : Control signal indicating the optimal operating point for the 
C              cogeneration system based on the loads on the specified storage
C Simulation Data:
C  -NSTEP    : # building timesteps / hour
C  -NSTPP    : # plant timesteps / hour
C DG_thermal_PID also makes use of several variables stored in DG_controller_module,
C including
C   - STA_primary, STA_auxiliary, STA_freefloat, cold_storage
C     hot_storage: Named constants
C   - XWT_Coeff_kp, XWT_Coeff_ki, XWT_Coeff_kd: Coefficients used for PID control 
C   - Storage integral and derivative periods: XWT_per_int, XWT_per_der
C   - array_zeroed: Flag indicating if arrays have been zeroded
C   - XWT_INT_DATA: Arrays containing historical integral error data for storage 
C     tanks
C   - XWT_DER_DATA: Arrays containing historical derivative error for storage 
C     tanks 
C*******************************************************************************

      SUBROUTINE DG_thermal_PID(
     &     iterp,               ! flag indicating plant solution is in iteration 
     &     HWT_coeff_Kp,        ! hot water tank proportional gain
     &     HWT_coeff_Ki,        ! Hot water tank integral gain
     &     HWT_coeff_Kd,        ! Hot water tank derivitive gain
     &     HWT_per_int,         ! Hot water tank integral period
     &     HWT_per_der,         ! Hot water tank derivitive period
     &     CWT_coeff_Kp,        ! Cold water tank porportional gain 
     &     CWT_coeff_Ki,        ! Cold water tank integral gain
     &     CWT_coeff_Kd,        ! Cold water tank derivitive gain
     &     CWT_per_int,         ! Cold water tank integral period
     &     CWT_per_der,         ! Cold water tank derivitive period  
     &     mode,                ! flag indicating which tank is to be controlled
     &     error,               ! error signal
     &     control              ! control signal (output)
     &     )
      IMPLICIT NONE

C---------------------------------------------------------------------------------
C     Standard vbl. declarations
C---------------------------------------------------------------------------------
#include"DG_controller_include.h"

C---------------------------------------------------------------------------------
C     Common blocks req'd to determine timestep frequency 
C        NTSTEP: # of building timesteps/hour
C        NTSTPP: # of plant timesteps/building timestep
C---------------------------------------------------------------------------------
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep 
      common/pctstp/ntstpp
C.....unit numbers for trace/screen reporting
      common/outin/iuout,iuin
C---------------------------------------------------------------------------------
C     Declare common blocks
C---------------------------------------------------------------------------------
      integer isd1,ism1,isd2,ism2,isds,isdf,ntstep, ntstpp, iuout, iuin
C---------------------------------------------------------------------------------
C     Declare local variables.
C---------------------------------------------------------------------------------
      integer iterp             ! flag indicating plt. sol'n is in iteration
      real CWT_integral         ! Integral of CWT error over CWT_n_max_der 
      real HWT_integral         ! Integral of HWT error over HWT_n_max_der
      real error                ! error signal
      real control              ! control signal
      integer mode              ! integer flag for heating or cooling control
      logical array_zeroed   ! flag used to determine status of arrays
C.....Save array status
      save array_zeroed
C.....Contorller array dimensions
      integer HWT_n_time_int           ! HWT integral period
      integer HWT_n_time_der           ! HWT derivative period
      integer CWT_n_time_int           ! CWT integral period
      integer CWT_n_time_der           ! CWT derivative period
C.....Controller array time limit
      real fMAX_period 
C-------------------------------------------------------------------------------------
C     OLD FORTRAN 90 implementation:
C     Arrays were allocated dynamically because very large integral
C     (somewhat plausable) and derivitive (not so plausable) periods could be specified. 
C     At 5-minute timesteps, a one month integral period will require 60480 array 
C     locations.
C
C     real, dimension(:), allocatable :: CWT_int_data ! CWT integral data array
C     real, dimension(:), allocatable :: HWT_int_data ! HWT integral data array
C     real, dimension(:), allocatable :: CWT_der_data ! CWT derivitive data array
C     real, dimension(:), allocatable :: HWT_der_data ! HWT integer data array
C 
C     This approach permitted the arrays to be scaled to arbitrary timescales, but
C     dynamic arrays are not supported by G77. Therefore, the dimensions on the arrays
C     are fixed in the include file. 
C
C     NEW GNU77 implementation:
C-------------------------------------------------------------------------------------
      real CWT_int_data(CWT_n_max_int)
      real CWT_int_sum          ! Holds sum of CWT_int_data
      real HWT_int_data(HWT_n_max_int)
      real HWT_int_sum          ! Holds sum of HWT_int_data

      real CWT_der_data(CWT_n_max_der)      
      real CWT_der_ave          ! Holds average derivative
      real HWT_der_data(HWT_n_max_der)
      real HWT_der_ave          ! Holds average derivative  

C.....Save historical data 
      save CWT_int_data,  HWT_int_data, CWT_der_data,  HWT_der_data,
     &     CWT_int_sum, HWT_int_sum, CWT_der_ave, HWT_der_ave,
     &     HWT_n_time_int,HWT_n_time_der,CWT_n_time_int,CWT_n_time_der
C---------------------------------------------------------------------------------
C     Check to see if arrays have been zeroed - if so, skip this step. Otherwise
C     calculate integral and derivative periods, and zero array memory.
C     
C     ???_per_int       : Storage integral period (hours)
C     ???_n_max_int     : Integer indicating how many timesteps of data must
C                          be stored in the storage integral array 
C     ???_per_der       : Storage derivative period (hours)
C     ???_n_max_der     : Integer indicating how many timesteps of data must
C                          be stored in the storage derivative array 
C     (# plant timesteps/hour) = (# building timesteps/hour) * 
C                                (# plant timesteps / building timesteps)
C
C---------------------------------------------------------------------------------
      n_plant_time = NTSTEP * NTSTPP
      dt_timestep = 3600./real(n_plant_time) ! # of seconds per hour 
                                             !      / plant timestep
C--------------------------------------------------------------------------------
C     If arrays have not been zero-ed, zero them now.
C--------------------------------------------------------------------------------
      IF ( .not. array_zeroed ) THEN 
         HWT_n_time_int = int(HWT_per_int * real(n_plant_time)) ! HWT integral period
         HWT_n_time_der = int(HWT_per_der * real(n_plant_time)) ! HWT derivative period
         CWT_n_time_int = int(CWT_per_int * real(n_plant_time)) ! CWT integral period
         CWT_n_time_der = int(CWT_per_der * real(n_plant_time)) ! CWT derivative period
C---------------------------------------------------------------------------------
C        Check to see if defined integral periods exceed the array limits
C---------------------------------------------------------------------------------
         IF ( HWT_n_time_int .GT. HWT_n_max_int ) THEN
C...........Calculate maximum period, and use this instead
            fMAX_period =  real(HWT_n_max_int) * dt_timestep / 3600.
            write(IUOUT,*)' Cogeneration system controller: '
            write(IUOUT,*)' WARNING: Specified hot water tank integral' 
            write(IUOUT,*)' period (',HWT_per_int,' hours) is larger '
            write(IUOUT,*)' than cogen system controller limit '
            write(IUOUT,*)' (',fMAX_period,' hours).  Using maximum  '
            write(IUOUT,*)' output instead.'
            HWT_n_time_int = HWT_n_max_int
         ENDIF
         IF ( CWT_n_time_int .GT. CWT_n_max_int ) THEN
C...........Calculate maximum period, and use this instead
            fMAX_period =  real(CWT_n_max_int) * dt_timestep / 3600.
            write(IUOUT,*)' Cogeneration system controller: '
            write(IUOUT,*)' WARNING: Specified cold water tank integral' 
            write(IUOUT,*)' period (',CWT_per_int,' hours) is larger '
            write(IUOUT,*)' than cogen system controller limit '
            write(IUOUT,*)' (',fMAX_period,' hours).  Using maximum  '
            write(IUOUT,*)' output instead.'
            CWT_n_time_int = CWT_n_max_int
         ENDIF

         IF ( HWT_n_time_der .GT. HWT_n_max_der ) THEN
C...........Calculate maximum period, and use this instead
            fMAX_period =  real(HWT_n_max_der) * dt_timestep / 3600.
            write(IUOUT,*)' Cogeneration system controller: '
            write(IUOUT,*)' WARNING: Specified hot water tank '
     &           ,'derivative' 
            write(IUOUT,*)' period (',HWT_per_der,' hours) is larger '
            write(IUOUT,*)' than cogen system controller limit '
            write(IUOUT,*)' (',fMAX_period,' hours).  Using maximum  '
            write(IUOUT,*)' output instead.'
            HWT_n_time_der = HWT_n_max_der
         ENDIF

         IF ( CWT_n_time_der .GT. CWT_n_max_der ) THEN
C...........Calculate maximum period, and use this instead
            fMAX_period =  real(CWT_n_max_der) * dt_timestep / 3600.
            write(IUOUT,*)' Cogeneration system controller: '
            write(IUOUT,*)' WARNING: Specified cold water tank '
     &           ,'derivative' 
            write(IUOUT,*)' period (',CWT_per_der,' hours) is larger '
            write(IUOUT,*)' than cogen system controller limit '
            write(IUOUT,*)' (',fMAX_period,' hours).  Using maximum  '
            write(IUOUT,*)' output instead.'
            CWT_n_time_int = CWT_n_max_int
         ENDIF


C---------------------------------------------------------------------------------
C        Initialize arrays to zero (probably not necessary, depending on the flags
C        used at compliation)
C---------------------------------------------------------------------------------
	if( HWT_n_time_int .gt. 0 ) then
         do ii = 1, HWT_n_time_int	
            HWT_INT_DATA(ii) = 0.
         ENDDO
        endif
        if ( HWT_n_time_der .gt. 0 ) then
         do ii = 1, HWT_n_time_der
            HWT_DER_DATA(ii) = 0.
         ENDDO
        endif
        if ( CWT_n_time_int .gt. 0 ) then
         do ii = 1, HWT_n_time_int
            CWT_INT_DATA(ii) = 0.
         ENDDO
       endif 
       if ( CWT_n_time_der .gt. 0 ) then
         do ii = 1, CWT_n_time_der
            CWT_DER_DATA(ii) = 0.
         ENDDO
       endif 
C--------------------------------------------------------------------------------
C        Set flag to ensure that we don't zero arrays a second time
C--------------------------------------------------------------------------------
         array_zeroed  = .true.
      END IF        
C---------------------------------------------------------------------------------
C     Determine if hot or cold tank is being controlled.
C      - mode = cold_storage: cold tank is being controlled
C      - mode = hot_storage:  hot tank is being controlled
C---------------------------------------------------------------------------------
      SELECT CASE (mode)
           
         CASE(cold_storage)
C---------------------------------------------------------------------------------
C           If integral control req'd, update integral matrix:
C            - Drop oldest error data from running sum (CWT_INT_SUM)
C            - Update hot storage integer array (ie shift array location i to
C              i+1). This overwrites the oldest entry in the array.
C            - Calculate integral sum for start of current timestep
C---------------------------------------------------------------------------------
            IF(CWT_n_time_int .gt. 0) THEN
C..............Check if plant sol'n is in iteration. If not, calculate sum 
C..............at start of timestep and update derivative data
               if ( iterp .eq. 1 ) then
C.................Subtract the last error term in the integral array (corresponding
C.................to the end of the integral period) from the running sum
                  CWT_INT_SUM = CWT_INT_SUM - 
     &                 CWT_INT_DATA(CWT_n_time_int) 
C.................Add the error term from the previous timestep to the running sum
                  CWT_INT_SUM = CWT_INT_SUM + CWT_INT_DATA(1) 
C.................Update array locations.
                  do ii = CWT_n_time_int - 1, 1, -1
                     CWT_INT_DATA(ii+1) = CWT_INT_DATA(ii)
                  enddo                             
               endif
C..............Add error term top of array. If this is the first plant iteration,
C..............the error term will be written to an empty array location. Otherwise
C..............the value from the previous iteration will be overwritten
               CWT_INT_DATA(1) = error * dt_timestep  
C..............Calculate total integral for this iteration
               CWT_integral = CWT_INT_SUM + CWT_INT_DATA(1) 
                                                          
C---------------------------------------------------------------------------------
C              If sum of integral is less than zero, set to zero. This effectively
C              means that we're only applying integtral control when it will 
C              increase the control signal, and ignoring it when it will decrease 
C              the control signal. Doing so helps mitigate integral wind up and
C              results in a faster controller.
C---------------------------------------------------------------------------------               
               if ( CWT_integral .lt. 0 ) CWT_integral = 0.
            else
               CWT_integral = 0.0
            endif

C---------------------------------------------------------------------------------
C           If derivative control is req'd, update derivative matrix:
C            - Update hot storage derivative array (ie shift array location i to
C              i+1). This overwrites the oldest entry in the array.
C            - Save cirrent error data in first array location
C            - Calculate error derivative for current iteration, and write error
C              for this iteration into the CWT_INT_DATA array. 
C
C                             error(1) + error(CWT_n_time_der)
C               CWT_DER_AVE = --------------------------------
C                               CWT_n_time_der * dt_timestep
C---------------------------------------------------------------------------------
            if (CWT_n_time_der .gt. 0) then
C..............Check if plant sol'n is in iteration. If not, update derivitive data
               if ( iterp .eq. 1 ) then
                  do ii = CWT_n_time_der - 1, 1, -1
                     CWT_DER_DATA(ii+1) = CWT_DER_DATA(ii) ! Update array locations
                  enddo
               endif
C..............Add error term top of array. If this is the first plant iteration,
C..............the error term will be written to an empty array location. Otherwise
C..............the value from the previous iteration will be overwritten
               CWT_DER_DATA(1) = error 

               CWT_DER_AVE = ( CWT_DER_DATA(1)        !<-Average change in 
     &              - CWT_DER_DATA(CWT_n_time_der))   !  error over derivative
     &              / ( real(CWT_n_time_der)          !  period
     &                  * dt_timestep)   
            else
               CWT_DER_AVE = 0.0
            endif           
C---------------------------------------------------------------------------------
C           Calculate control signal using PID control
C           
C           Control =   Kp * error             <- proportional term
C                     + Ki * INT_SUM           <- integral term 
C                     + Kd * DER_AVE           <- derivative term
C---------------------------------------------------------------------------------
            control =   CWT_coeff_Kp * error 
     &                + CWT_coeff_Ki * CWT_integral
     &                + CWT_coeff_Kd * CWT_DER_AVE

         CASE(hot_storage)
C---------------------------------------------------------------------------------
C           If integral control req'd, update integral matrix:
C            - Drop oldest error data from running sum (HWT_INT_SUM)
C            - Update hot storage integer array (ie shift array location i to
C              i+1). This overwrites the oldest entry in the array.
C            - Calculate integral sum for start of current timestep
C---------------------------------------------------------------------------------
            if ( HWT_n_time_int .gt. 0 ) then
C..............Check if plant sol'n is in iteration. If not, calculate sum 
C..............at start of timestep and update derivative data
               if ( iterp .eq. 1 ) then
C.................Subtract the last error term in the integral array (corresponding
C.................to the end of the integral period) from the running su
                  HWT_INT_SUM = HWT_INT_SUM - 
     &                 HWT_INT_DATA(HWT_n_time_int) 
C.................Add the error term from the previous timestep to the running sum
                  HWT_INT_SUM = HWT_INT_SUM + HWT_INT_DATA(1) 
C.................Update array locations
                  do ii = HWT_n_time_int - 1, 1, -1
                     HWT_INT_DATA(ii+1) = HWT_INT_DATA(ii) 
                  enddo
               endif
C..............Add error term top of array. If this is the first plant iteration,
C..............the error term will be written to an empty array location. Otherwise
C..............the value from the previous iteration will be overwritten
               HWT_INT_DATA(1) = error * dt_timestep
C..............Calculate total integral for this iteration
               HWT_integral = HWT_INT_SUM + HWT_INT_DATA(1) 

C---------------------------------------------------------------------------------
C              If sum of integral is less than zero, set to zero. This helps 
C              mitigate integral wind up and results in a faster controller.
C---------------------------------------------------------------------------------               
               if ( HWT_integral .lt. 0 ) HWT_integral = 0.

            else
               HWT_integral = 0.0 
            endif

C---------------------------------------------------------------------------------
C           If derivative control is req'd, update derivative matrix:
C            - Update hot storage derivative array (ie shift array location i to
C              i+1). This overwrites the oldest entry in the array.
C            - Save cirrent error data in first array location
C            - Calculate error derivative for current iteration, and write error
C              for this iteration into the HWT_INT_DATA array. 
C
C                             error(1) + error(HWT_n_time_der)
C               HWT_DER_AVE = --------------------------------
C                              HWT_n_time_der * dt_timestep
C---------------------------------------------------------------------------------
            if (HWT_n_time_der .gt. 0) then
C..............Check if plant sol'n is in iteration. If not, update derivitive data
               if ( iterp .eq. 1 ) then
                  do ii = HWT_n_time_der - 1, 1, -1
                     HWT_DER_DATA(ii+1) = HWT_DER_DATA(ii) ! Update array locations
                  enddo 
               endif
C..............Add error term top of array. If this is the first plant iteration,
C..............the error term will be written to an empty array location. Otherwise
C..............the value from the previous iteration will be overwritten
               HWT_DER_DATA(1) = error 

               HWT_DER_AVE = ( HWT_DER_DATA(1)        !<-Average change in 
     &              - HWT_DER_DATA(HWT_n_time_der))   !  error over derivative
     &              / ( real(HWT_n_time_der)          !  period
     &                  * dt_timestep)
            else
               HWT_DER_AVE = 0.0
            endif
            
C---------------------------------------------------------------------------------
C           Calculate control signal using PID control
C           
C           Control =   Kp * error             <- proportional term
C                     + Ki * INT_SUM           <- integral term 
C                     + Kd * DER_AVE           <- derivative term
C---------------------------------------------------------------------------------
            control =   HWT_coeff_Kp * error 
     &                + HWT_coeff_Ki * HWT_integral
     &                + HWT_coeff_Kd * HWT_DER_AVE

         CASE DEFAULT
C---------------------------------------------------------------------------------
C           Can't happen - mode is assigned upstream.
C---------------------------------------------------------------------------------

      END SELECT
      RETURN
      END

C**************************   DG_controller_output   **************************
C
C Created by: Alex Ferguson
C Initial Creation Date: April 15, 2003.
C Copyright CETC 2003
C
C This subroutine is used to output timestep data from the distributed gen. 
C controller model to a text file. It is called each time the plant 
C energy solution converges (ie ISTATS = 1 and bpltcon = .TRUE.  The data it 
C outputs is stored by the DG_controller_coeff_generator and Thermal_PID 
C routines in DG_controller_module.
C
C This routine is intended to be used mostly for debugging purposes
C
C The subroutine is passed to variables:
C   IDYP: Integer represening the day (1-365)
C   ptimep: present plant time after plant solution has converged (0-24)
C******************************************************************************

      SUBROUTINE DG_controller_output(
     &     IDYP, 
     &     ptimep,
     &     DG_control_signal,   !  Cogen unit control signal
     &     TAC_Cooling_Signal,  !  TAC cooling control signal (on-off)
     &     TAC_HeatDump_Signal, !  TAC heat dump control signal (on-off)
     &     bHS_flag,            !  heating season flag
     &     bCS_flag,            !  cooling season flag
     &     HWT_T,               !  HWT tank temperature
     &     HWT_T_target,        !  Hot water tank target temperature
     &     HWT_error,           !  HWT error
     &     HWT_control,         !  HWT control signal
     &     CWT_T,               !  CWT tank temperature
     &     CWT_T_target,        !  CWT target temperature
     &     CWT_error,           !  CWT error
     &     CWT_control,         !  CWT control signal
     &     W_elec_demand,       !  Dwelling electrical demand  
     &     file_open            !  Flag indicating that file has been opened
     &     )
      IMPLICIT NONE
C------------------------------------------------------------------------------
C     Common definitions 
C------------------------------------------------------------------------------
#include"DG_controller_include.h"
C------------------------------------------------------------------------------
C     Local variables
C------------------------------------------------------------------------------
      integer idyp              ! Integer represening the day (0-365)
      real ptimep               ! present plant time in day
      real date                 ! real variable representing the date 0-365
      logical file_open         ! flag indicating that file is open 

C------------------------------------------------------------------------------
C     Format date:
C       date = (day #) + (hour)/24
C------------------------------------------------------------------------------
      date = real(IDYP) + ptimep/24.
C------------------------------------------------------------------------------
C     Open output file, if necessary
C------------------------------------------------------------------------------
      if ( .not. file_open ) then
         open(unit=23,file='DG_controller.res',status='replace')
         file_open = .true.
         write (23,'(A19,1x,A19,1x,A19,2x,A19,1x,A19,9(1x,A19))')
     &     'date ',
     &     'DG_control_signal',   ! Cogen unit control signal
     &     'TAC_Cooling_Signal',  ! TAC cooling control signal (on-off)
     &     'TAC_HeatDump_Signal', ! TAC heat dump control signal (on-off)
     &     'HS_flag',            ! heating season flag
     &     'CS_flag',             ! cooling season flag
     &     'HWT_T',               !  HWT tank temperature
     &     'HWT_T_target',        !  HWT target temperature
     &     'HWT_error',           !  HWT error
     &     'HWT_control',         !  HWT control signal
     &     'CWT_T',               !  CWT tank temperature
     &     'CWT_T_target',        !  CWT target temperature
     &     'CWT_error',           !  CWT error
     &     'CWT_control',         !  CWT control signal
     &     'W_elec_demand'       ! Dwelling electrical demand
      endif     
      write(23,'(F12.8,1x,F12.8,1x,F12.8,1x,F12.8,1x,I3,1x,I3,
     &           9(1x,e10.4))')
     &     Date,
     &     DG_control_signal,   !  Cogen unit control signal
     &     float(TAC_Cooling_Signal),  !  TAC cooling control signal (on-off)
     &     float(TAC_HeatDump_Signal), !  TAC heat dump control signal (on-off)
     &     bHS_flag,            !  heating season flag
     &     bCS_flag,            !  cooling season flag
     &     HWT_T,               !  HWT tank temperature
     &     HWT_T_target,        !  Hot water tank target temperature
     &     HWT_error,           !  HWT error
     &     HWT_control,         !  HWT control signal
     &     CWT_T,               !  CWT tank temperature
     &     CWT_T_target,        !  CWT target temperature
     &     CWT_error,           !  CWT error
     &     CWT_control,         !  CWT control signal
     &     W_elec_demand        !  Dwelling electrical demand           
      RETURN
      END

C--------------------DG_temperature_sensor --------------------------------------
C     Created by: Alex Ferguson
C     Initial Creation Date: Feb 10, 2004.
C     Copyright CETC 2004
C     
C     This function is a stand-in for proper temerature sensors in the distributed 
C     generation controller. It is passed a node number, and returns the node's
C     corresponding future temperature.
C---------------------------------------------------------------------------------
      REAL FUNCTION DG_temperature_sensor(node)
      IMPLICIT NONE 
#include"plant.h"
C.....Common block variable containing plant present and future temperatures
      common/pcval/csvf(mpnode,mpvar),csvp(mpnode,mpvar)
C.....Common block declarations
      real csvf                 ! future state variables
      real csvp                 ! present state variables
C.....Passed & local variables
      integer node              ! pointer to node of interest
      real temperature          ! temperature of node of interest (oC)

C.....Get temperature
      DG_temperature_sensor = csvf(node,1)
      
C....Return temperature
      RETURN 
      END              ! that's it!
