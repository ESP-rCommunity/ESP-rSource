C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Alex Ferguson for details 
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
C-----------------------------------------------------------------------
C
C This file contains routines to allow switching the model between 
C scheduled air flow, AIM-2 and air-flow networks according to 
C some control logic
C
C  It contains the following routines:
C
C  AirFlowInitializePriority: Sets initial flags determining 
C     in which order the various flow models should be called
C 
C-----------------------------------------------------------------------

C================ AirFlowInitializePriority ============================
C Created by: Alex Ferguson
C Created on: March 2, 2011 
C Inquiries to Alex Ferguson (aferguso@nrcan.gc.ca)
C 
C Synopsys: This routine initializes the zone's air-flow modelling flag
C           according to ESP-r's standard priority. These flags determine
C           which of the air flow computation methods ( no-flow, 
C           scheduled flows from .opr file, AIM-2 model, air-flow network)
C           should be applied to a given zone. The method selected here
C           will prevail throughout the simulation unless control action
C           changes the flags in response climate or building conditions.
C
C           As per ESP-r's legacy conventions, the default priority for 
C           air-flow computation methods is:
C
C                 1. AIM-2 model         (highest)  
C                 2. Air-flow network               
C                 3. OPR scheduled                  
C                 4. No flow.            (lowest)    
C
C           A specified  air flow computation method will always 
C           superseeded by higher-priority method provided:
C           
C              - the higher-priority method has been specified for 
C                the current zone
C 
C              - control action has not specified the lower priority 
C                method over the course of the simulation
C            
C=======================================================================

      subroutine AirFlowInitializePriority()
C.....AIM-2 parameters
      USE AIM2_InputData, ONLY:iAIM2,
     &                         AIM2_zone_infil,
     &                         AIM2_num_zones_infil
      implicit none 
      
C.....Commons and parameters needed to configure the air-flow models          
      include "building.h"
      include "blc25_open_windows.h"
      
C.....Building dimensions      
      common/c1/nComp, nCon ! Number of zones and connections in the 
                            ! model
      integer nComp, nCon                            
C.....AFN parameters                        
      common/afn/iAirN,lAprob,iCAAS(mcom)
      integer :: iairn,icaas    ! Flags for Air-flow network,
                                ! AFN nodes that correspond to zones      
      character*72 lAprob       ! Airflow network description file. 
                                ! (not used in current context)
      
C.....Indicies
      integer iZoneIndex  ! Index of current zone
      integer iAIM2Zone   ! Counter for looping through AFN definitions
      
      logical bAIM2, bAFN, bOPR
      
C.....Loop through zones, and determine which air flow computation methods
C.....have been specified for this zone. Assign flags according to 
C.....priority.

      ZoneLoop: do iZoneIndex = 1, nComp 

C.......Initialize air-flow computation method spec flags             
        bAIM2   = .false. 
        bAFN    = .false. 
        bOPR    = .false. 
      
C.......Check for AIM-2 model 
        CheckForAIM2: if ( iAIM2 == 1 ) then
C.........AIM-2 is specified. See if this is one of the zones that 
C.........recieves infiltration. Loop through list of zones 
C.........flagged fro AIM2 infiltration, and check for match.
          AIM2Loop: do iAIM2Zone=1, AIM2_num_zones_infil
          
            AIM2InfilToThisZone: 
     &      if ( AIM2_zone_infil(iAIM2Zone) == iZoneIndex ) then 
C.............Zones match. Flag for AIM-2.     
              bAIM2 = .true. 
            
            endif AIM2InfilToThisZone
          
          enddo AIM2Loop
        endif CheckForAIM2
        
C.......Check for Air-flow network
C        
        CheckForAFN: if ( iAirN > 0 .and. iCAAS(iZoneIndex) /= 0 ) then 
          bAFN = .true. 
        endif CheckForAFN
        
C.......Opr schedules are always present, as ESP-r presently requires 
C.......them. Note: it's possible to have a .opr file with no flows!
        bOPR = .true.
        
C.......Now assign flags according to priority 

        AssignFlag: if ( bAIM2 ) then 
          iAirFlowModel(iZoneIndex) = iAIM2model
        
        elseif( bAFN ) then 
          iAirFlowModel(iZoneIndex) = iFlowNetwork 
          
        elseif ( bOPR ) then 
          iAirFlowModel(iZoneIndex) = iOPRschedule
          
        else 
          ! This statement will never be executed, because bOPR is always 
          ! true.
          iAirFlowModel(iZoneIndex) = iNoFlow
          
        endif AssignFlag

      enddo ZoneLoop
      
      return
      end subroutine AirFlowInitializePriority
      
      
      