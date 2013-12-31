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

      implicit none 
      
C.....Commons and parameters needed to configure the air-flow models          
      include "building.h"
      include "net_flow.h"
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
C        
        CheckForAFN: if ( iAirN > 0 ) then 
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
      
C------------------ bFlowConnToOutside ---------------------------------
C     Created by: Alex Ferguson
C     Created on: March 17, 2011 (Happy St.Patty's Day!)
C     Copyright:  Natural Resources Canada
C
C     Synopsys: This function tests whether a given flow connection 
C               couples a zone to the outside. It returns true if so,
C               and passes back the appoprriate index of the coupled 
C               zone. 
C
C-----------------------------------------------------------------------
C
      logical function bFlowConnToOutside(iFlowConnection,
     &                                    iCoupledZone     )
      implicit none
      
      include "building.h"
      include "net_flow.h"
      
      ! Passed variables 
      integer iFlowConnection !- index of connection under consideration
                              !  (input)
      integer iCoupledZone    !- Index of coupled zone (if any, output)
      
      bFlowConnToOutside = .false.
      
      return 
      end function bFlowConnToOutside
      

C------------------ bFindAFN600Coupling --------------------------------
C     Created by: Alex Ferguson
C     Created on: March 25, 2011 
C     Copyright:  Natural Resources Canada
C
C     This routine searches through all of the flow connections defined 
C     in an air-flow network and returns true if:
C     
C       - one of the connections couples the given zone to the 
C         outside, and 
C
C       - the coupling component for that connection is type 600, 
C         which imposes AIM-2 computed infiltration on the air flow 
C         network model. 
C
C      This function is primarely intended to be used in subsys.F, where
C      bps must determine if the AIM-2 computed infiltration should be 
C      imposed directly on the building thermal model, or as a 
C      bidirectional flow in the air-flow network (via type 600 / 
C      MF600_AIM2_coupling).
C      
C      It returns true if a suitable type 600 component is found for 
C      the current zone in the air flow network, indicating the AIM-2 
C      computed infiltration should not be imposed directly on the 
C      building thermal model. 
C-----------------------------------------------------------------------
      
      logical function bFindAFN600Coupling(iZone)       
      implicit none 
      
      include "building.h"
      include "net_flow.h"      
      
C.....Passed variables        
      
      integer iZone                ! Index of current zone 

      bFindAFN600Coupling = .false. 
      
      return 
      end function bFindAFN600Coupling 
      
      
      
C------------------ iWindowsForZone ------------------------------------
C     Created by: Alex Ferguson
C     Created on: March 17, 2011 (Happy St.Patty's Day!)
C     Copyright:  Natural Resources Canada
C
C     Synopsys: This function tests whether a given flow connection 
C               couples a zone to the outside. It returns true if so,
C               and passes back the appoprriate index of the coupled 
C               zone. 
C
C-----------------------------------------------------------------------




      integer function iWindowsForZone(iZone,
     &                                 iFlowConnection )
      implicit none
      
      
           
      include "building.h"
      include "net_flow.h"
      
      ! Passed variables 
      integer iFlowConnection(mcnn) !- indicies of window flow connections
                                    !  for this zone 
      integer iZone                 !- Index of zone (if any, output)
      
      iZone = 0
      iWindowsForZone = 0 
     
      return 
      end function iWindowsForZone
      

C------------------ MF600_AIM2_coupling --------------------------------
C     Created by: Alex Ferguson
C     Created on: March 25, 2011 
C     Copyright:  Natural Resources Canada
C
C     This is a simple, bidirectional flow component for use with the 
C     the air-flow network. It imposes the AIM-2 computed air 
C     infiltration for the corresponding zone as a flow into and out of 
C     its coupled node. 
C     
C-----------------------------------------------------------------------
      SUBROUTINE MF600_AIM2_coupling( iConnection, 
     &                                fDeltaP, 
     &                                fDerivitive )
      
      IMPLICIT NONE
      
      include "building.h"
      include "net_flow.h"

C.....Passed parameters
      integer iConnection     ! Index of connection 
      double precision fDeltaP         ! Pressure difference across component (Pa)
      double precision fDerivitive
      
C.....ESP-r commons 
      common/mflow5/rhon(mnod)
      
      real RhoN     ! Density of air at flow network nodes (kg/m3)
      
      common/mflow3/ndfld(mnod),ndtyp(mnod),hnod(mnod),supnod(mnod,mnds)      
      
      integer ndfld, ! Flow node fluid type
     &        ndtyp  ! Flow node type 
      
      real hnod, supnod  ! not used 
      
      common/mflw10/nodps(mcnn),hgtps(mcnn),nodne(mcnn),hgtne(mcnn),
     &              itpcon(mcnn),ndscnn(mcnn,mcns)
      
      integer NodPS, NodNE   ! indicies of positive and negative nodes 
      
          
      integer iTpCon         ! Indicies of coupling flow components 
      integer NdScnn         ! Data for flow comonents (not used) 
      
      real HgtPS, HgtNE      ! Height of positive and negative nodes 
                             ! (m, not used) 

      
      common/mflres/flw1(mcnn),flw2(mcnn),pres(mnod),
     &              resid(mnod),saflw(mnod)
     
      double precision Flw1, Flw2  ! Component flow data (in & out, kg/s)
      
      double precision PRES, RESID, SAFLW ! Other flow data (not used)

C.....Named constants       
      real fSMALL 
      PARAMETER       (fSMALL=1.0E-15)

C.....indicies of positive and negative nodes. 
      integer iPositiveNode
      integer iNegativeNode 
      integer iZoneNode 

C.....AIM-2 computed infiltration for this zone       
      real fAIM2InfilFlow                         ! (m3/s) 
      
C.....Index of coupled zone (if any)
      integer iCoupledZone 
      logical bCoupledZone 

      
      Flw1(iConnection) =  0.
      Flw2(iConnection) =  0.
      
C.....Compute derivitive (to help flow solver?) 
      fDerivitive = 0.
      
      return
      end subroutine MF600_AIM2_coupling 
      
C------------------ MF610_circ_fan_coupling ----------------------------
C     Created by: Alex Ferguson
C     Created on: March 25, 2011 
C     Copyright:  Natural Resources Canada
C
C     This is a simple, bidirectional flow component serves as a crude
C     approximation of circulation fan modelling in flow networks.
C     
C-----------------------------------------------------------------------
      SUBROUTINE MF610_circ_fan_coupling( iConnection, 
     &                                    fDeltaP, 
     &                                    fDerivitive )
      IMPLICIT NONE
      
      include "building.h"
      include "net_flow.h"

C.....Passed parameters
      integer iConnection     ! Index of connection 
      double precision fDeltaP         ! Pressure difference across component (Pa)
      double precision fDerivitive
      
C.....ESP-r commons 
      common/mflow5/rhon(mnod)
      
      real RhoN     ! Density of air at flow network nodes (kg/m3)
      
      common/mflow3/ndfld(mnod),ndtyp(mnod),hnod(mnod),supnod(mnod,mnds)      
      
      integer ndfld, ! Flow node fluid type
     &        ndtyp  ! Flow node type 
      
      real hnod, supnod  ! not used 
      
      common/mflow9/itpcmp(mcmp),isdcmp(mcmp),isdcnn(mcmp),
     &              supcmp(mcmp,mcms)
     
      real supcmp     ! Supplementary data for components 
      integer isdcmp, isdcnn, itpcmp  ! Not used 
      
      common/mflw10/nodps(mcnn),hgtps(mcnn),nodne(mcnn),hgtne(mcnn),
     &              itpcon(mcnn),ndscnn(mcnn,mcns)
      
      integer NodPS, NodNE   ! indicies of positive and negative nodes 
      
          
      integer iTpCon         ! Indicies of coupling flow components 
      integer NdScnn         ! Data for flow comonents (not used) 
      
      real HgtPS, HgtNE      ! Height of positive and negative nodes 
                             ! (m, not used) 

      
      common/mflres/flw1(mcnn),flw2(mcnn),pres(mnod),
     &              resid(mnod),saflw(mnod)
     
      double precision Flw1, Flw2  ! Component flow data (in & out, kg/s)
      
      double precision PRES, RESID, SAFLW ! Other flow data (not used)

C.....Named constants       
      real fSMALL 
      PARAMETER       (fSMALL=1.0E-15)

C.....indicies of positive and negative nodes. 
      integer iPositiveNode
      integer iNegativeNode 
      integer iZoneNode 

      integer iComponent
      
      real fFlow
      
C.....Start calculations: Get positive and negative nodes:      
      iPositiveNode = NODPS(iConnection)
      iNegativeNode = NODNE(iConnection)
      
      iComponent = itpcon( iConnection )
      fFlow = supcmp ( iComponent, 2 ) 

      
      Flw1(iConnection) =  fFlow * RHON(iPositiveNode)
      Flw2(iConnection) = -fFlow * RHON(iPositiveNode)
      
C.....Compute derivitive (to help flow solver?) 
      
      call MFNPDV(iConnection, fDeltaP, fDerivitive )

           
      return
      end subroutine MF610_circ_fan_coupling     
      
      
      