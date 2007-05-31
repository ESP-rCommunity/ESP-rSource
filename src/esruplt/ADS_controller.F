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

C***************************************************************************

C******************ADS_stg_controller***************************************
C
C Created by: Maria Mottillo
C Initial creation date: August 8, 2005
C
C This subroutine is used to evaluate the conditions in the plant network
C and to control the operation of the adsorption storage unit in response
C to these conditions. 
C
C References:
C
C Mottillo M. (2006), Investigation of an Adsorption System for the Seasonal
C Storage of Heat Applied to Residential Buildings, M.A.Sc. dissertation,
C Concordia University, Montreal, Canada.
C
C-----------------------------------------------------------------------------

       SUBROUTINE ADS_stg_controller(iterp)
       IMPLICIT NONE

      INCLUDE 'plant.h'
      INCLUDE 'building.h'

C-----------------------------------------------------------------------------
C ESP-r COMMONs
C-----------------------------------------------------------------------------

       COMMON/OUTIN/IUOUT,IUIN
       INTEGER IUOUT           !-write unit number
       INTEGER IUIN            !-read unit number

       COMMON/TC/ITC,ICNT
       INTEGER ITC             !-trace output index
       INTEGER ICNT            !-trace output counting variable
      
       COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
       INTEGER ITCF            !-building-side time increment
                               !-to end trace output 
       INTEGER ITRACE          !-trace call index
       INTEGER IZNTRC          !-zone trace index
       INTEGER ITU             !-trace output unit number

       COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
       INTEGER NPCOMP          !-number of plant components
       INTEGER NCI             !-number of possible control variables for component
       REAL    CDATA           !-array containing control data values

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

       COMMON/PCLSOL/ICFP,IDTYPP,IPERP
       INTEGER ICFP            !-control loop identifier
       INTEGER IDTYPP          !-plant side control day type
       INTEGER IPERP           !-plant side control period type

       COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
       INTEGER ISD1            !-start day number
       INTEGER ISM1            !-start month number
       INTEGER ISD2            !-end day number
       INTEGER ISM2            !-end month number
       INTEGER ISDS            !-start year day number
       INTEGER ISDF            !-finish year day number
       INTEGER NTSTEP          !-number of building time steps within each hour

       COMMON/PCTSTP/NTSTPP
       INTEGER NTSTPP          !-number of plant time steps within each building
                               !-time step
       COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
       INTEGER NPCDAT          !-miscellaneous plant component data
       INTEGER IPOFS1          !-row position of network matrix coefficient
       INTEGER IPOFS2          !-column position of network matrix coefficient

       COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
       REAL    ADATA           !-array containing plant component data
       REAL    BDATA           !-array containing plant component data

       COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
       REAL    CSVF            !-array holding future value for plant node
                               !-and state variable
       REAL    CSVP            !-array holding present value for plant node
                               !-and state variable

       COMMON/PTIME/PTIMEP,PTIMEF
       REAL    PTIMEP          !-present time row time plant side
       REAL    PTIMEF          !-future time row time plant side

       COMMON/BTIME/BTIMEP,BTIMEF
       REAL    BTIMEP          !-present time row time building side
       REAL    BTIMEF          !-future time row time building side

       COMMON/PCTL/NCL,IPSN(MCL,4),IPAN(MCL,4),NPCDT(MCL),
     &             IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &             TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &             IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
       INTEGER NCL             ! number of plant control loops
       INTEGER IPSN            ! defines sensor node associated with plant control loop
       INTEGER IPAN            ! defines actuator node associated with plant control loop
       INTEGER NPCDT           ! number of control day types for plant side control
       INTEGER IPCDV           ! dates of validity for each day type for plant side control
       INTEGER NPCDP           ! number of distinct control periods for each day type
       REAL    TPCPS           ! start time for each control period (plant control)
       INTEGER IPCTYP          ! controller type for each control loop, day type and period
       INTEGER IPCLAW          ! control law for each control loop, day type and period
       REAL    PMISCD          ! holds data required by control laws



       COMMON/pltcon/bpltcon,lastTStp   
       LOGICAL bpltcon         !-logical variable indicating if plant domain
                               !-converged in previous iteration
       REAL    lastTStp

       COMMON/PltMtrx/ISTATS
       INTEGER ISTATS          !-flag indicating which plant matrix is being solved
                               !- 1 = temperature, 2= 1st phase mass balance, 
                               !- 3 = 2nd phase mass balance.

       COMMON/ADSCont/bAds_ctl_init
       LOGICAL bADS_ctl_init   !- boolean indicating if adsorption storage unit
                               !- controller has been initialized.

       COMMON/Stgdata/iCharge_signal,iDischarge_signal
       INTEGER iCharge_signal    ! charging signal for adsorption storage unit (on/off)
       INTEGER iDischarge_signal ! discharge signal for adsorption storage unit (on/off)

       COMMON/FCSignal/iFC_opr_signal   ! fuel cell operating signal passed from
                                        ! fuel cell coefficient generator
       INTEGER iFC_opr_signal


C----------------------------------------------------------------------------
C Passed variables
C----------------------------------------------------------------------------

       INTEGER ITERP           !-flag indication if there is iteration occuring
                               !-in the plant


C----------------------------------------------------------------------------
C Local variables
C----------------------------------------------------------------------------

       INTEGER iAds_stg_index  ! plant component index of adsorption storage unit
       INTEGER iAds_stg_node   ! index of adsorber node
       INTEGER iHW_tank_index  ! plant component index of hot water tank
       INTEGER iHW_tank_node   ! index of hot water tank water node
       INTEGER iHS_start       ! heating season start day (1-365)
       INTEGER iHS_end         ! heating season end day (1-365)
       INTEGER iCS_start       ! cooling season start day (1-365)
       INTEGER iCS_end         ! cooling season end day (1-365)

       INTEGER iFC_index       ! plant component index of fuel cell
       INTEGER iFC_node        ! index of fuel cell heat exchanger, water side


       INTEGER iHold_node      ! user for input of node indices
       INTEGER n_plant_time    ! number of plant timesteps per hour

       INTEGER iFC_Ads_pump_index   ! plant component index of fuel cell - adsorber pump
       INTEGER iTank_Ads_pump_index ! plant component index of tank - adsorber pump
       INTEGER iFC_Tank_pump_index  ! plant component index of fuel cell - tank pump
       INTEGER iAuxTank_Evap_pump_index ! plant component index of aux-tank/evap pump
                           
       LOGICAL bError          ! flag indicating that simulation must be halted
       LOGICAL bHS_flag        ! heating season flag (0-1)
       LOGICAL bCS_flag        ! cooling season flag (0-1)
       LOGICAL bInStartup      ! function used to determine whether simulation is in startup mode
       LOGICAL bStartup        ! variable used to store bInStarup function result

       REAL    fHS_HWT_T_su    ! hot water tank upper setpoint during heating season (discharge)
       REAL    fHS_HWT_T_sl    ! hot water tank lower setpoint during heating season (discharge)
       REAL    fCS_HWT_T_su    ! hot water tank upper setpoint during cooling season (charge)
       REAL    fCS_HWT_T_sl    ! hot water tank lower setpoint during cooling season (charge)

       REAL    fHS_HWT_under_sl_t ! minutes hot water tank temperature is below lower
                                  ! setpoint during heating season (discharge)
       REAL    fCS_HWT_over_su_t  ! minutes hot water tank temperature is above upper
                                  ! setpoint during cooling season (charge)

       REAL    dt_timestep        ! number of minutes per timestep

       REAL    fReset_period      ! reset period in minutes
       PARAMETER( fReset_period = 25. )

       REAL    fHS_min_Ads_temp   ! minimum adsorber temperature during discharge (oC)

       REAL    fCS_max_Ads_temp   ! maximum adsorber temperature during charging (oC)

       REAL    fAds_Min_temp_diff ! minimum temperature difference between adsorber
                                  ! and heat source or heat sink. This is an input 
                                  ! for adsorption storage unit plant component

       REAL    fHWT_temp          ! temperature of water in tank (oC)
       REAL    fAds_temp          ! temperature of adsorber (oC)
       REAL    fFC_temp           ! temperature of water-side of fuel cell heat exchanger (oC)

       REAL    fAdsChargeMinTemp  ! minimum temperature of heat source required for
                                  ! for charging of the adsorption storage unit (oC)

       REAL    fFC_tank_pump_flow       ! flow rate of fuel cell - tank pump (m3/s)
       REAL    fFC_Ads_pump_flow        ! flow rate of fuel cell - adsorber pump (m3/s)
       REAL    fTank_Ads_pump_flow      ! flow rate of tank - adsorber pump (m3/s)
       REAL    fAuxTank_Evap_pump_flow  ! flow rate of aux tank - evap pump (m3/s)


C       INTEGER iFC_opr_signal     ! fuel cell operating signal (on/off)


       real    check     ! for debugging

C----------------------------------------------------------------------------
C Save data required on subsequent timesteps
C----------------------------------------------------------------------------

        SAVE   fHS_HWT_under_sl_t, fCS_HWT_over_su_t, iAds_stg_index,
     &         iAds_stg_node, iHW_tank_index, iHW_tank_node, 
     &         iFC_index, iFC_node,
     &         iHS_start, iHS_end, iCS_start, iCS_end,
     &         fHS_HWT_T_su, fHS_HWT_T_sl, fCS_HWT_T_su, fCS_HWT_T_sl,
     &         iFC_Ads_pump_index, iTank_Ads_pump_index,
     &         iFC_Tank_pump_index, iAuxTank_Evap_pump_index,
     &         fFC_tank_pump_flow, fFC_Ads_pump_flow,
     &         fTank_Ads_pump_flow,fAuxTank_Evap_pump_flow


C----------------------------------------------------------------------------

C If this is the first time the subroutine is called then initialize all 
C values.

        if ( .not. bADS_ctl_init ) then

C Check that correct number of data items have been provided
C pmisc(?,?,?,1) defines total number of control data to be read
            if ( int(pmiscd(icfp,idtypp,iperp,1) ) .ne. 18 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &           'fatal error! Incorrect number of control data ',
     &           'specified. 18 items were expected, but ',
     &           pmiscd(icfp,idtypp,iperp,1), ' items were found.'
            endif

C Assign user-specified inputs and check input data

            iAds_stg_index = int(pmiscd(icfp,idtypp,iperp,2))
C npcdat(i,4) holds the pointer to the appropriate coefficient 
C generator routine for plant component i (also the plant db component code/10)
            if ( npcdat(iAds_stg_index,4) .ne.  115 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &           'fatal error! Incorrect plant index number specified ',
     &           'for adsorption storage unit in control data. ' 
            endif

            iHold_node = int(pmiscd(icfp,idtypp,iperp,3))
            if ( iHold_node .ne. 1 ) then        ! adsorber is first node
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &           'fatal error! Incorrect node number specified ',
     &           'for adsorption storage unit in control data. '
            else
C Get node number of adsorber (node 1 of adsorption storage unit).
C npcdat(i,9) holds the index number of the first node of plant component i;
C increment this counter by (node#-1) to get the appropriate node index.
                iAds_stg_node = npcdat(iAds_stg_index,9) +
     &                          (iHold_node - 1)
            endif


            iHW_tank_index = int(pmiscd(icfp,idtypp,iperp,4))
            if ( (npcdat(iHW_tank_index,4) .ne. 113) .and.
     &           (npcdat(iHW_tank_index,4) .ne. 114) ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &           'fatal error! Incorrect plant index number specified ',
     &           'for hot water tank in control data. '
            endif

            iHold_node = int(pmiscd(icfp,idtypp,iperp,5))
            if ( iHold_node .ne. 1 ) then        ! tank water represented by first node
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &           'fatal error! Incorrect node number specified ',
     &           'for water tank in control data. '
            else
                 iHW_tank_node = npcdat(iHW_tank_index,9) +
     &                           (iHold_node - 1)
            endif


            iFC_index = int(pmiscd(icfp,idtypp,iperp,6))
            if ( npcdat(iFC_index,4) .ne. 112 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &          'fatal error! Incorrect plant index number specified ',
     &          'for fuel cell in control data. '
            endif

            iHold_node = int(pmiscd(icfp,idtypp,iperp,7))
            if ( iHold_node .ne. 3 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &          'fatal error! Incorrect node number specified ',
     &          'for fuel cell in control data. '
            else
                iFC_node = npcdat(iFC_index,9) +
     &                     (iHold_node - 1)
            endif



            iHS_start = int(pmiscd(icfp,idtypp,iperp,8))
            iHS_end   = int(pmiscd(icfp,idtypp,iperp,9))
            if (iHS_start .lt. 1 .or. iHS_start .gt. 366 ) then
                bError = .true.
                write(iuout,*) ' Adsorption storage unit controller: ',
     &            'fatal error! Heating season start day is invalid. ',
     &            'Please specify a value between 1 and 366. ' 
            endif
            if (iHS_end .lt. 1 .or. iHS_end .gt. 366 ) then
                bError = .true.
                write(iuout,*) ' Adsorption storage unit controller: ',
     &            'fatal error! Heating season end day is invalid. ',
     &            'Please specify a value between 1 and 366. ' 
            endif            

            iCS_start = int(pmiscd(icfp,idtypp,iperp,10))
            iCS_end   = int(pmiscd(icfp,idtypp,iperp,11))
            if (iCS_start .lt. 1 .or. iCS_start .gt. 366 ) then
                bError = .true.
                write(iuout,*) ' Adsorption storage unit controller: ',
     &            'fatal error! Cooling season start day is invalid. ',
     &            'Please specify a value between 1 and 366. ' 
            endif
            if (iCS_end .lt. 1 .or. iCS_end .gt. 366 ) then
                bError = .true.
                write(iuout,*) ' Adsorption storage unit controller: ',
     &            'fatal error! Cooling season end day is invalid. ',
     &            'Please specify a value between 1 and 366. ' 
            endif            


            fHS_HWT_T_su = pmiscd(icfp,idtypp,iperp,12)
            fHS_HWT_T_sl = pmiscd(icfp,idtypp,iperp,13)
            if ( fHS_HWT_T_su .lt. fHS_HWT_T_sl ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &            'fatal error! Hot water tank upper set point during ',
     &            'heating season is less than lower set point. '
            endif

            fCS_HWT_T_su = pmiscd(icfp,idtypp,iperp,14)
            fCS_HWT_T_sl = pmiscd(icfp,idtypp,iperp,15)
            if ( fCS_HWT_T_su .lt. fCS_HWT_T_sl ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &            'fatal error! Hot water tank upper set point during ',
     &            'cooling season is less than lower set point. '
            endif


            iFC_Ads_pump_index = int(pmiscd(icfp,idtypp,iperp,16))
            if ( npcdat(iFC_Ads_pump_index,4) .ne. 117 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &            'fatal error! Incorrect plant index number specified',
     &            ' for fuel cell - adsorber pump. '
            endif

            iTank_Ads_pump_index = int(pmiscd(icfp,idtypp,iperp,17))
            if ( npcdat(iTank_Ads_pump_index,4) .ne. 117 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &            'fatal error! Incorrect plant index number specified',
     &            ' for tank-adsorber pump. '
            endif

            iFC_Tank_pump_index = int(pmiscd(icfp,idtypp,iperp,18))
            if ( npcdat(iFC_tank_pump_index,4) .ne. 117 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &            'fatal error! Incorrect plant index number specified',
     &            ' for fuel cell - tank pump. '
            endif

            iAuxTank_Evap_pump_index = int(pmiscd(icfp,idtypp,iperp,19))
            if ( npcdat(iFC_tank_pump_index,4) .ne. 117 ) then
                bError = .true.
                write(iuout,*) 'Adsorption storage unit controller: ',
     &            'fatal error! Incorrect plant index number specified',
     &            ' aux-tank/evap pump. '
            endif



C Set HWT under-setpoint (discharge) and over-setpoint (charge) counters to zero.
            fHS_HWT_under_sl_t = 0.
            fCS_HWT_over_su_t = 0.

C Initialize the control signals for adsorption storage unit
            iCharge_signal = 0             
            iDischarge_signal = 0

            bADS_ctl_init = .true.

C Obtain flow rates for network pumps. These are the flow rates that are
C assigned to the pumps by the controller when they are on. The pumps are
C on/off depending on whether the adsorption storage unit is charging or
C discharging.

            fFC_tank_pump_flow = CDATA(iFC_tank_pump_index,1)
            fFC_Ads_pump_flow = CDATA(iFC_Ads_pump_index,1)
            fTank_Ads_pump_flow = CDATA(iTank_Ads_pump_index,1)
            fAuxTank_Evap_pump_flow =CDATA(iAuxTank_Evap_pump_index,1)


        endif 
C Values initialized for adsorption storage controller.
C-------------------------------------------------------------------------------------


C--------------------------------------------------------------------------------
C     ONLY determine control signal if temperature matrix is being solved 
C     (istats = 1), plant has converged (ie. 2nd phase mass flow matrix sol'n
C     is complete, and this is the first go at the temperature matrix.) and simulation
C     is not during the startup period.
C--------------------------------------------------------------------------------

        bStartup = bInStartup()


        IF ( istats .eq. 1 .AND. bpltcon .AND. .not. bStartup ) then

C---------------------------------------------------------------------------------
C        Determine if we're in the heating season. This is dependent on how HS_start
C        and HS_end are specified:
C            HS_start > HS_end - Winter in the nothern hemisphere
C            HS_start < HS_end - Winter in the southern hemisphere 
C        The heating season start/end dates are compared agianst the variable IDYP 
C        which holds the present simulation day.
C This is copied from DG_controller subroutine developed by Alex.
C---------------------------------------------------------------------------------

            if( iHS_start .gt. iHS_end) then
C..............Typical N.H. winter...
               if(IDYP .ge. iHS_start.or.IDYP .le.iHS_end) then
C................In heating season - set flag
                  bHS_flag = .true.
               else
                  bHS_flag = .false.
               endif
            else
C..............Typical S.H. winter...
               if(IDYP .ge. iHS_start.AND. IDYP .le. iHS_end) then
C.................In heating season - set flag
                  bHS_flag = .true.
               else
                  bHS_flag = .false.
               endif 
            endif

C---------------------------------------------------------------------------------
C        Determine if we're in the cooling season. This is dependent on how CS_start
C        and CS_end are specified:
C            CS_start < CS_end - Summer in the nothern hemisphere
C            CS_start > CS_end - Summer in the southern hemisphere 
C        The cooling season start/end dates are compared agianst the variable IDYP 
C        which holds the present simulation day.
C This is copied from DG_controller subroutine developed by Alex.
C---------------------------------------------------------------------------------

            if(iCS_start .lt. iCS_end) then
C..............Typical N.H. summer...
               if(IDYP .ge. iCS_start.AND.IDYP .le. iCS_end) then
C.................In cooling season - set flag
                  bCS_flag = .true.
               else
                  bCS_flag = .false.
               endif
            else
C..............Typical S.H. summer...
               if(IDYP.ge.iCS_start.or.IDYP.le.iCS_end ) then
C.................In cooling season - set flag
                  bCS_flag = .true.
               else
                  bCS_flag = .false.
               endif 
            endif



C---------------------------------------------------------------------------------
C Determine temperature of hot water tank
C---------------------------------------------------------------------------------

            fHWT_temp = CSVF(iHW_tank_node,1)

C---------------------------------------------------------------------------------
C Determine temperature of adsorption storage unit's adsorber
C---------------------------------------------------------------------------------

            fAds_temp = CSVF(iAds_stg_node,1)

C--------------------------------------------------------------------------------
C Determine temperature of fuel cell heat exchanger, water side
C--------------------------------------------------------------------------------

            fFC_temp = CSVF(iFC_node,1)

C--------------------------------------------------------------------------------
C Determine fuel cell operating signal
C--------------------------------------------------------------------------------

C            iFC_opr_signal = int(CDATA(iFC_index,1))

C--------------------------------------------------------------------------------
C Determine minimum adsorber temperature during heating season (discharge) and
C maximum adsorber temperature during cooling season (charge)
C--------------------------------------------------------------------------------

            fAds_Min_temp_diff = ADATA(iAds_stg_index,20)
            fHS_min_Ads_temp = fHWT_temp + fAds_Min_temp_diff
            fCS_max_Ads_temp = fFC_temp - fAds_Min_temp_diff

C--------------------------------------------------------------------------------
C        Determine length of timestep (minutes)
C This is copied from DG_controller subroutine developed by Alex.
C--------------------------------------------------------------------------------
            n_plant_time = NTSTEP * NTSTPP ! # plant timesteps / hour
            dt_timestep = 60.0/real(n_plant_time) ! # of minutes / plant timestep


C--------------------------------------------------------------------------------
C Determine minimum temperature of heat source required for the charging of
C the adsorption storage unit. 
C--------------------------------------------------------------------------------

             fAdsChargeMinTemp = ADATA(iAds_stg_index,21)



C for debugging
C        if ( nsinc .eq. 331 ) then
C           check = 1.
C        endif
C end debugging




C--------------------------------------------------------------------------------
C Check if it is the heating season. If so, check if the temperature of 
C the hot water tank is below the lower setpoint and tally up the minutes
C if it is. 
C--------------------------------------------------------------------------------

            if ( bHS_flag ) then

               if ( fHWT_temp .lt. fHS_HWT_T_sl ) then
                   fHS_HWT_under_sl_t = fHS_HWT_under_sl_t +
     &                                  dt_timestep
               else
                   fHS_HWT_under_sl_t = 0.
               endif

            else        ! cooling season

               fHS_HWT_under_sl_t = 0.

            endif


C------------------------------------------------------------------------------
C Check if it is the cooling season. If so, check if the temperature of
C the hot water tank is above the upper setpoint and tally up the minutes
C if it is.
C------------------------------------------------------------------------------

            if ( bCS_flag ) then

               if ( fHWT_temp .gt. fCS_HWT_T_su ) then
                  fCS_HWT_over_su_t = fCS_HWT_over_su_t +
     &                                dt_timestep
               else
                  fCS_HWT_over_su_t = 0.
               endif

            else

               fCS_HWT_over_su_t = 0.

            endif


C--------------------------------------------------------------------------------
C Control logic for starting discharge or charge of adsorption storage unit.
C Reset period is set to 25 minutes.
C Heating season:
C HWT temp < Tsl for more than 25 minutes and adsorber temp > min ads temp -> discharge
C Cooling season:
C HWT temp > Tsu for more than 25 minutes and adsorber temp < max ads temp -> charge
C--------------------------------------------------------------------------------

            if ( bHS_flag ) then
C...........heating season, check if adsorption storage unit is already in discharge mode
                if ( iDischarge_signal .ne. 1 ) then
C...............adsorption storage unit not discharging, check if it should start discharging
C...............at this timestep
                   if ( fHS_HWT_under_sl_t .gt. fReset_period .and. 
     &                 fAds_temp .ge. fHS_min_Ads_temp
C     &                 .and. iFC_opr_signal .ne. 1
     &                 ) then

                      iDischarge_signal = 1
                   endif
                else
C...............adsorption storage unit is discharging, check if it should stop
                   if ( fHWT_temp .gt. fHS_HWT_T_su .or.
     &                 fAds_temp .lt. fHS_min_Ads_temp
C     &                 .or. iFC_opr_signal .eq. 1
     &                 ) then
                 
                       iDischarge_signal = 0

                   endif
                endif

C............adsorption storage unit not permitted to charge during heating season
                iCharge_signal = 0
C the previous statement is commented if want to allow heating and cooling seasons to overlap.
             endif



             if ( bCS_flag ) then
C............cooling season, check if adsorption storage unit is already in charging mode
                if ( iCharge_signal .ne. 1 ) then
C...............adsorption storage unit not charging, check if it should start charging
C...............at this timestep
                   if ( fCS_HWT_over_su_t .gt. fReset_period .and.
     &                  fAds_temp .le. fCS_max_Ads_temp .and.
     &                  fFC_temp .ge. fAdsChargeMinTemp .and.
     &                  iFC_opr_signal .eq. 1 ) then

                      iCharge_signal = 1
                   endif
                else
C...............adsorption storage unit is charging, check if it should stop
                   if ( fHWT_temp .lt. fCS_HWT_T_sl .or.
     &                 fAds_temp .gt. fCS_max_Ads_temp .or.
     &                 fFC_temp .lt. fAdsChargeMinTemp .or.
     &                 iFC_opr_signal .ne. 1 ) then

                      iCharge_signal = 0
                   endif

                endif
C............adsorption storage unit not permitted to discharge during cooling season
                iDischarge_signal = 0
C the previous statement is commented if want to allow heating and cooling seasons to overlap.
             endif


C This code would be necessary to allow overlapping heating and cooling seasons.
C Check the charging and discharging signals. If both have been set, give priority to
C discharging mode.
C             if ( iCharge_signal .eq. 1 .AND.
C     &            iDischarge_signal .eq. 1 ) then
C
C                  iCharge_signal = 0
C
C             endif



C If it is neither the heating season or cooling season, ensure that adsorption storage
C unit control signals are off.
             if ( .not. bHS_flag .and. .not. bCS_flag ) then

                 iCharge_signal = 0
                 iDischarge_signal = 0

             endif

        ENDIF          ! (istats .eq. 1 and bpltcon and not bInStartup)


C--------------------------------------------------------------------------------------
C Set control signals: write data into CDATA array for adsorption storage unit.
C
C Note: if plant energy solution has not converged, (ie. ISTATS .ne. 1 and/or 
C bpltcon = .FALSE.) control signal variables will remain unchanged 
C from the last iteration. These unchanged control variables are still
C passed to the adsorption storage coefficient generator.
C--------------------------------------------------------------------------------------

       CDATA(iAds_stg_index,1) = float(iCharge_signal)
       CDATA(iAds_stg_index,2) = float(iDischarge_signal)

C--------------------------------------------------------------------------------------
C Set control signal (flow rate) for:
C - pump connecting fuel cell to adsorber,
C - pump connecting fuel cell to water tank,
C - pump connecting tank to adsorption storage unit and
C - pump connecting aux tank to evaporator.
C--------------------------------------------------------------------------------------


C The pump connecting fuel cell to tank only operates if the adsorption storage
C unit is not charging.
        if ( iCharge_signal .ne. 1 ) then 
C  the adsorption storage unit is not charging
           CDATA(iFC_tank_pump_index,1) = fFC_tank_pump_flow
        else
C the adsorption storage unit is charging
           CDATA(iFC_tank_pump_index,1) = 0.
        endif


C The pump connecting the fuel cell to the adsorber is only operating when
C the adsorption storage unit is in charging mode.
        if ( iCharge_signal .eq. 1 ) then
           CDATA(iFC_Ads_pump_index,1) = fFC_Ads_pump_flow
        else
           CDATA(iFC_Ads_pump_index,1) = 0.
        endif

C The pump connecting the adsorber to the tank only operates if the
C adsorption storage unit is in discharge mode.
        if ( iDischarge_signal .eq. 1 ) then
           CDATA(iTank_Ads_pump_index,1) = fTank_Ads_pump_flow
        else
           CDATA(iTank_Ads_pump_index,1) = 0.
        endif

C The pump connecting the evaporator to the auxiliary water tank
C only operates if the adsorption storage unit is in discharge mode.
        if ( iDischarge_signal .eq. 1) then
           CDATA(iAuxTank_Evap_pump_index,1) = fAuxTank_Evap_pump_flow
        else
           CDATA(iAuxTank_Evap_pump_index,1) = fAuxTank_Evap_pump_flow    ! setting to zero causes segmentation
                                                                          ! fault; leave it be for now.
        endif



C Pump flow rates used in (Mottillo, 2006), directly assigned to CDATA above:
C fFC_tank_pump_flow = 0.000126
C fFC_Ads_pump_flow = 0.000005
C fTank_Ads_pump_flow =  0.000007
C fAuxTank_Evap_pump_flow = 0.00003



C--------------------------------------------------------------------------------------
C If there are any errors, stop simulation.
C--------------------------------------------------------------------------------------

       if ( bError ) then

          stop 'Adsorption storage controller: unresolvable error. '

       endif



       RETURN
       END

