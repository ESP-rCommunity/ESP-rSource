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

C-------------------------------------------------------------------------------------
C
C  This file contains the following routines:
C
C  Elec_Net_Calculator ( mode, Excluded plant component, Excluded power-only component)
C
C    -> Performs loads / generation / net balance calculations, and is capable
C       of excluding the contribution of a given plant and/or power only component
C       from this sum
C
C  Elec_Net_Plant_Calculator ( mode, Excluded plant component)
C
C    -> Interface to Elec_Net_Calculator that permits excluding a single plant
C       component from the aggregate sum.
C
C  Elec_Net_Load_Calc ( mode )
C
C    -> Legacy interface to Elec_Net_Caclulator that does not permit exclusion
C       of any components form the aggregate sum.
C-------------------------------------------------------------------------------------

C.....Interface to Elec_Net_Calculator -> exclude a plant component 
      real function Elec_Net_Plant_Calc (
     &                   iMode, iExcluded_plant_comp )
      implicit none

      integer iMode, iExcluded_plant_comp, iExcluded_power_comp
      real Elec_Net_Calculator
C.....Set iExcluded_power_comp to less than zero -> Elec_Net_Calculator will
C.....not exclude any power-only components.
      iExcluded_power_comp = -99

      Elec_Net_Plant_Calc = Elec_Net_Calculator(
     &             iMode,
     &             iExcluded_plant_comp,
     &             iExcluded_power_comp )
      return
      end

C.....Interface to Elec_Net_Calculator - >exclude a power-only component
      real function Elec_Net_Power_Calc (
     &                   iMode, iExcluded_power_comp )
      implicit none

      integer iMode, iExcluded_plant_comp, iExcluded_power_comp
      real Elec_Net_Calculator
C.....Set iExcluded_plant_comp to less than zero; Elec_Net_Calculator will
C.....not exclude any plant components.
      iExcluded_plant_comp = -99

      Elec_Net_Power_Calc = Elec_Net_Calculator(
     &             iMode,
     &             iExcluded_plant_comp,
     &             iExcluded_power_comp )
      return
      end

      
C.....Interface to Elec_Net_Calcuator
      real function Elec_Net_Load_Calc ( iMode )
      implicit none

      integer iMode, iExcluded_plant_comp, iExcluded_power_comp
      real Elec_Net_Calculator
C.....Set iExcluded_power_comp and iExcluded_plant_comp to less than
C.....zero; Elec_Net_Calculator will not exclude any components.
      iExcluded_power_comp = -99
      iExcluded_plant_comp = -99
      
      Elec_Net_Load_Calc= Elec_Net_Calculator(
     &             iMode,
     &             iExcluded_plant_comp,
     &             iExcluded_power_comp )
      return
      end      

      
      
C************************ Elec_Net_Calculator     ************************************
C
C Created by: Phylroy Lopez
C Initial Creation Date: Jan  16, 2003.
C Changed: Jul 05, 2014 ag
C Copyright CETC 2003
C
C This function is used to perform various operations using data caculated by the 
C electrical network. It presently supports 8 operations, as determined by 
C the 'mode' argument passed to it:
C
C - mode = ( total_load | hvac_load | occpant_load | cluster_load )
C   Function will collect all electical demands from HVAC equipment and/or
C   occupant loads, cluster_load and return the total.
C
C - mode = (total_gen | offsite_gen )
C   Function will collect all of the electrical power produced in on-site 
C   generation sources or offsite generation sources and return the total.
C
C - mode = balance
C   Function will balance load and generation for all components of the
C   electrical network, taking battery charge (?)/discharge into account
C 
C - mode = save_data
C   Function reads data used by ESP-r's electrical network 
C   solver and saves this data in common blocks that can be accessed by 
C   elec_net_load_calc() later. This functionality is required, because as 
C   the plant domnain  iterates, these common blocks are over-written with the 
C   most recent data from the plant domain, thus preventing elec_net_load_calc
C   from determining the state of the electrical network at the start of the present 
C   iteration.
C
C************************************************************************************

      REAL FUNCTION Elec_Net_Calculator(mode,
     &                                 iExclude_Plt_Comp,
     &                                 iExclude_Ele_Comp)
      implicit none
#include "plant.h"
#include "building.h"
#include "power.h"
#include "CETC_definitions.h"
C provides pba, vrb and lion
#include "BATTERY.h"

C---------------------------------------------------------------------
C     Common for writing data 
C---------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin
C---------------------------------------------------------------------
C     Storage common blocks used for plant->electrical network 
C     coupling (W). Used to transport network solution back to explicit 
C     plant domain.
C---------------------------------------------------------------------
      COMMON/PLANT_ENET_TRANSPORT/
     &     Hybrid_Power_Real(MHYCOM), ! Hybrid real power
     &     Elec_Power_Real(MHYCOM)   ! Power only component real power    
      REAL Hybrid_Power_Real, Elec_Power_Real


C---------------------------------------------------------------------
C     Passed data
C---------------------------------------------------------------------
      INTEGER mode              ! flag indicating which calculation
                                ! should be performed
      
      INTEGER iExclude_Plt_Comp ! plant component to be excluded
                                ! from sum

      INTEGER iExclude_Ele_Comp ! power-only component to be
                                ! excluded from sum.

C---------------------------------------------------------------------
C     Flags used to determine which data should be reported
C---------------------------------------------------------------------
      LOGICAL report_hvac       ! report HVAC loads 
      LOGICAL report_occupant   ! report occupant loads 
      LOGICAL report_gains      ! report loads from elec-coupled 
                                ! casual gains (presently not used
                                ! in Hot3000)
      LOGICAL report_pv         ! electrical production of pv 
                                ! components 
      LOGICAL report_battery    ! loads covered by battery or 
                                ! electrical export from battery
      LOGICAL report_cogen      ! electrical production of cogen
                                ! components
      LOGICAL report_offsite_loads    ! external electrial loads
      LOGICAL report_offsite_gen ! off-site electrical generation
      LOGICAL report_offsite_all ! balance of external loads/gen
C---------------------------------------------------------------------
C    local  Plant component  power consumption variables. 
C---------------------------------------------------------------------
      
      CHARACTER*124 OUTS
C Pointers to various components / loads
      INTEGER iHybridComponent
      INTEGER iComponentIndex
      INTEGER iComponentType

      INTEGER iIndex
      INTEGER iLoadIndex

      REAL    fElec_net_result

      integer iPOWCO            ! counter

C---------------------------------------------------------------------
C     Begin characterization of electrical network
C---------------------------------------------------------------------
C     Determine what data is being requested, based on the passed 
C     variable 'mode'
      if ( mode .eq. total_load ) then
C     report hvac + occupant-driven electric loads
         report_hvac     = .true.
         report_occupant = .true.
         report_gains    = .false. 
         report_cogen    = .false.
         report_pv       = .false.
         report_battery  = .false.
         report_offsite_all  = .false.
         report_offsite_gen  = .false.
         report_offsite_loads = .true.
      elseif ( mode .eq. hvac_load ) then
C     report hvac loads only
         report_hvac     = .true.
         report_occupant = .false.
         report_cogen    = .false. 
         report_gains    = .false.
         report_pv       = .false.
         report_battery  = .false.
         report_offsite_all  = .false.
         report_offsite_gen  = .false.
         report_offsite_loads = .false.
      elseif ( mode .eq. occupant_load ) then
C     report occupant-driven loads onlu
         report_hvac     = .false.
         report_occupant = .true.
         report_cogen    = .false.
         report_gains    = .false.
         report_pv       = .false.
         report_battery  = .false.
         report_offsite_all  = .false.
         report_offsite_gen  = .false.
         report_offsite_loads = .false.
      elseif ( mode .eq. total_gen ) then
C     report power generation only
         report_hvac     = .false.
         report_occupant = .false.
         report_gains    = .false.
         report_cogen    = .true.
         report_pv       = .true.
         report_battery  = .false.
         report_offsite_all  = .false.
         report_offsite_gen  = .false.
         report_offsite_loads = .false.
      elseif ( mode .eq. cluster_load ) then
C     report power generation only
         report_hvac     = .false.
         report_occupant = .false.
         report_gains    = .false.
         report_cogen    = .false.
         report_pv       = .false.
         report_battery  = .false.
         report_offsite_all  = .false.
         report_offsite_gen  = .false.
         report_offsite_loads = .true.
      elseif ( mode .eq. offsite_gen ) then 
         report_hvac     = .false.
         report_occupant = .false.
         report_gains    = .false.
         report_cogen    = .false.
         report_pv       = .false.
         report_battery  = .false.
         report_offsite_all  = .false.
         report_offsite_gen  = .true.
         report_offsite_loads = .false.
      elseif ( mode .eq. balance ) then
C     report net import/export
         report_hvac     = .true.
         report_occupant = .true.
         report_gains    = .false.
         report_cogen    = .true.
         report_pv       = .true.
         report_battery  = .true.
         report_offsite_all  = .true.
         report_offsite_gen  = .false.
         report_offsite_loads = .false.
      elseif ( mode .eq. store_data ) then
C     Do nothing  
      else
         write (OUTS,'(A)') 'Fatal error!'
         call edisp(iuout,outs)
         write (OUTS,'(A)') 'Function Elec_Net_Load_Calc in '
         call edisp(iuout,outs)
         write (OUTS,'(A)') 'elec_net_utilities.F: data requested'
         call edisp(iuout,outs)
         write (OUTS,'(A,I1,A)') '(',mode,') is presently unsupported'
         call edisp(iuout,outs)
         write (OUTS,'(A)') 'ABORTING SIMULATION!!!'
         call edisp(iuout,outs)
         STOP
      endif
      
C.....Initialize sum variable 
      fElec_net_result = 0.0
C.....Iterate through all hybrid components
      DO iHybridComponent=1,nHYBcom
C........Get index for component.
         iComponentIndex = HYBCOMNO(iHybridComponent)
C........Determine the type of component.
         iComponentType = HYCOMTYP(iHybridComponent)
C........Get required component index values.
         iIndex = HYLOC(iHybridComponent,1)
         iLoadIndex = HYLOC(iHybridComponent,2)

C........Determine if we're reporting or saving data (check value of mode)
         IF ( mode .eq. store_data ) THEN
C...........We're saving data. 
C...........Determine component type, and collect 
C...........Power consumption from appropriate commonblock

            IF ( iComponentType .eq. 1 ) THEN
C..............Zone casual gains (ESRU coupling)
               Hybrid_Power_Real(iHybridComponent) =  
     &              PEZON( iIndex, iLoadIndex ) ! (W)
            ELSEIF ( iComponentType .eq. 2 ) THEN
C..............Special Materials, including PV
C..............PV generation
               Hybrid_Power_Real(iHybridComponent) =  
     &              PSPM(iIndex) ! (W)

            ELSEIF ( iComponentType .eq. 3 ) THEN
C..............Plant component. 
               Hybrid_Power_Real(iHybridComponent) =
     &            PPCMP( iIndex )
            ENDIF
         ELSE
C...........We're reporting data. Add the component's electrical load 
C...........to the running sum, if it is requested (check report_X variables)
            IF ( (                                                    !___________________________
     &             ( iComponentType .eq. 1 )                          ! Casual gains Loads
     &             .AND. (report_gains) )                             !___________________________
     &       .OR.( ( iComponentType .eq. 2 )                          ! Photovoltaics generation
     &             .AND. (report_pv   ) )                             !___________________________
     &       .OR.( ( ( iComponentType .eq. 3 )                        ! HVAC loads 
     &             .AND. (report_hvac )  )                            ! (loads are -ive)
     &             .AND. (Hybrid_Power_Real(iHybridComponent).LT.0) ) !___________________________
     &       .OR.( ( ( iComponentType .eq. 3 )                        ! Cogeneration 
     &             .AND. (report_cogen ) )                            ! (generation is +ive)
     &             .AND. (Hybrid_Power_Real(iHybridComponent).GE.0) ) !___________________________
     &           ) THEN 
  
C..............This data type has been requested. - Check to see if component
C..............should be included in sum:
               IF ( iIndex .ne. iExclude_plt_comp ) THEN
C................Component to be added to running sum
                 fElec_net_result = fElec_net_result +
     &             Hybrid_Power_Real(iHybridComponent) !(W)
               ELSE
C................Component to be excluded               
               ENDIF
            ENDIF
         ENDIF
      END DO

C.....Note: non-HVAC or internal gain loads are treated as power-only components in the
C.....electrical network.
      DO iPOWCO=1,NPOWCOM
C........Has exclusion been requested for the component?
         IF (iPOWCO .ne. iExclude_Ele_Comp ) then
          IF ( ( POWCOMID(iPOWCO) .eq. 10
     &             .or. POWCOMID(iPOWCO) .eq. 18 ) ! This is a non-HVAC load type power-only component
     &          .and. (report_occupant) ) THEN
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF
          IF ( POWCOMID(iPOWCO) .eq. 11  ! This is an idealized CETC HVAC model elec. load
     &          .and. (report_hvac) ) THEN
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF
          IF ( POWCOMID(iPOWCO) .eq. 15  ! This is an on-site power-conditioning unit 
     &          .and. (report_cogen) ) THEN
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF

C << what about the losses of power only component type 20? >>

C.........Load covered by battery or charge load on electrical net
          IF ( ( (POWCOMID(iPOWCO) .eq. pba)
     &           .or. (POWCOMID(iPOWCO) .eq. vrb)
     &           .or. (POWCOMID(iPOWCO) .eq. lion) ) ! this is a battery
     &           .and. (report_battery)
     &          ) THEN
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF

C.....Following if statement is for the case when the electric component is a power 
C.....only component for loads from the outside such as in the case of a cluster of 
C.....houses served by the electrical network in one building
C.....The same report variable for HVAC is used here also. This can be changed in the
C.....future so that a new report variable is used. This for now guarantees that these loads
C.....are imposed on the electric network and that they are not included as part of 
C.....the internal gains to the space
          IF ( ( POWCOMID(iPOWCO) .eq. 12 ) ! this is an off-site electrical load
     &           .and. (report_offsite_all .or. report_offsite_loads ) 
     &          ) THEN
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF
C........Losses associated with transformer at off-site electrical generation 
C........site
          IF ( ( POWCOMID(iPOWCO) .eq. 13 ) 
     &           .and.  (report_offsite_all .or. report_offsite_gen ) 
     &          ) THEN  
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF
C........Off-site transmission losses associated with distribution of electricity 
C........to house        
          IF ( ( POWCOMID(iPOWCO) .eq. 14 )  
     &           .and. (report_offsite_all .or. report_offsite_gen ) 
     &          ) THEN  
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF
C........Electricity generated off-site
          IF ( ( POWCOMID(iPOWCO) .eq. 16 ) ! this is an off-site generation source
     &           .and. (report_offsite_all .or. report_offsite_gen ) 
     &          ) THEN 
              fElec_net_result = fElec_net_result + PPOWOC(iPOWCO)
          ENDIF

        ENDIF ! < - matches if (iPOWOC .ne. iExclude_Ele_Comp) then

      END DO

C     Store the absolute value of the energy consumption in the returned variable.
      if ( mode .eq. total_load .or.
     &     mode .eq. hvac_load .or. 
     &     mode .eq. occupant_load ) then
     
C........Loads cannot be greater than zero
         if ( fElec_net_result .gt. 0.0 ) fElec_net_result = 0.0 
         
         Elec_Net_Calculator = abs (  fElec_net_result )

      else

         Elec_Net_Calculator = fElec_net_result

      endif

      END
