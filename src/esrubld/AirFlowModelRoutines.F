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
     &                                    iCoupledZone )
      implicit none
      
      include "building.h"
      include "net_flow.h"
      
      ! Passed variables 
      integer iFlowConnection !- index of connection under consideration
                              !  (input)
      integer iCoupledZone    !- Index of coupled zone (if any, output)
      integer iSearchType     !- Connection type to search for (not used)
      
C.....ESP-r commons 

      ! Building model data 
      integer ncomp,ncon    ! number of zones, connections in building model
      COMMON/C1/NCOMP,NCON
 

      ! Flow network connection data.
      common/mflw10/nodps(mcnn),hgtps(mcnn),nodne(mcnn),hgtne(mcnn),
     &              itpcon(mcnn),ndscnn(mcnn,mcns)
      
      integer nodps  , ! Indicies of positive coupled nodes
     &        nodne  , ! Indicies of negative coupled nodes
     &        itpcon , ! Indicies of coupling flow components 
     &        ndscnn   ! Supplementary connection node indicies (not used)
                     
      real hgtps     , ! Height of positive node (m, not used)
     &     hgtne       ! Height of negative node (m, not used)
     
     
      common/afn/iAirN,lAprob,icaas(mcom)
      integer iAirN, iCaas ! Flag for air flow network, map 
                           ! between network nodes and building zones
      character*72 lAprob  ! Airflow network file name 
     
C.....Local variables 
      ! Map to save some CPU cycles
           
C.....Flag indicating if we've initialized each connection.            
      logical bInitialized(MCNN)
      data bInitialized / MCNN * .false. /
      save bInitialized
      
C.....Status of flow connection: saved to reduce duplicate calculations       
      logical bFlowConnStatus(MCNN)
      save bFlowConnStatus
      
C.....Map connections to coupled zones (if any)      
      integer iCoupledZoneMap(MCNN)
      save iCoupledZoneMap
      

      
C.....Flag for matched status       
      logical bMatched
      
      
C.....Index of positive and negative nodes from connection       
      integer iPositiveNode, iNegativeNode
      
C.....Flags indicating if the positive and negative nodes are 
C.....Zone coupled.       
      logical bPositiveNodeZoneCoupled
      logical bNegativeNodeZoneCoupled
      
      integer iZone ! Iterator     
      
C.....Check to see if we've already tested this connection and 
C.....return saved results, if so. 
      NoSavedData: if ( .not. bInitialized(iFlowConnection ) ) then 
      
C.......Get positive and negative nodes.         
        iPositiveNode = nodps(iFlowConnection)
        iNegativeNode = nodne(iFlowConnection) 
        
C.......Initialize coupling flags.         
        bPositiveNodeZoneCoupled = .false.
        bNegativeNodeZoneCoupled = .false.          
        
        
C.......Loop through zones and compare coupled-node index to positive
C.......and negative nodes for this conneciton.        
        ZoneLoop: do iZone = 1, ncomp 
        
C.........Test if positive or negative node matches the current zone. 
        
          bMatched = .false. 
        
C.........Does positive node match this zone?        
          if  ( iCaas(iZone) == iPositiveNode ) then 
            bPositiveNodeZoneCoupled = .true. 
            bMatched = .true. 
          endif 

C.........Does negative node match this zone?          
          if  ( iCaas(iZone) == iNegativeNode ) then 
            bPositiveNodeZoneCoupled = .true.      
            bMatched = .true. 
          endif 


C.........Save zone index in map variable. This may happen twice for 
C.........zone->zone connections, but we set the global flag to false 
C.........below in this case.
          if ( bMatched ) iCoupledZoneMap(iFlowConnection) = iZone
          
        enddo ZoneLoop

C.......Determine if connection couples zone to outside, and set flag 
C.......to false as needed. Connection only couples to the outside of 
C.......either the positive or negative nodes are zone coupled. If both 
C.......nodes are zone-coupled, this is a zone-zone connection and 
C.......function should return false. 

        TestCoupling: if ( bPositiveNodeZoneCoupled .and. 
     &                     bNegativeNodeZoneCoupled       )  then
     
C.........Zone-zone coupling     
          
          bFlowConnStatus(iFlowConnection) = .false. 

          iCoupledZoneMap(iFlowConnection) = 0
          
        elseif ( bPositiveNodeZoneCoupled .or. 
     &           bNegativeNodeZoneCoupled      ) then 
     
C.........Zone-outdoors coupling         

          bFlowConnStatus(iFlowConnection) = .true. 
          
          ! ( iCoupledZoneMap already set above )
        
        else 

C.........Some other coupling           

          bFlowConnStatus(iFlowConnection) = .false. 

          iCoupledZoneMap(iFlowConnection) = 0 

        endif TestCoupling
          
C.......Set flag to ensure connection data is not reinitialized        
        bInitialized(iFlowConnection) = .true.
      
      endif NoSavedData
     
C.....Update data to be passed back to calling routine.      
      bFlowConnToOutside = bFlowConnStatus (iFlowConnection)
      if ( bFlowConnToOutside ) 
     &   iCoupledZone = iCoupledZoneMap (iFlowConnection)
      

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
      
C.....ESP-r commons: flow network connection data.
      common/mflw10/nodps(mcnn),hgtps(mcnn),nodne(mcnn),hgtne(mcnn),
     &              itpcon(mcnn),ndscnn(mcnn,mcns)
      
      
      integer nodps  , ! Indicies of positive coupled nodes
     &        nodne  , ! Indicies of negative coupled nodes
     &        itpcon , ! Indicies of coupling flow components 
     &        ndscnn   ! Supplementary connection node indicies (not used)
                     
      real hgtps     , ! Height of positive node (m, not used)
     &     hgtne       ! Height of negative node (m, not used)
           
C.....flow component data           
      common/mflow9/itpcmp(mcmp),isdcmp(mcmp),isdcnn(mcmp),
     &              supcmp(mcmp,mcms)
      
      integer itpcmp,   ! Indicies of coupling components
     &        isdcmp,   ! (?, not used) 
     &        isdcnn    ! (?, not used)
      real supcmp       ! Supplementary component data (not used)

C.....Local variables       
      
      
      integer iWindowCount, ! # of windows 
     &        iWindow, iConnection, iComponent ! Pointers 

      integer iWindowIndicies(mcnn)  ! Array storing those indicies                         
      
      logical bFound
      
C.....Storage to save CPU cycles
      logical bZoneInitialized(mcom)
      save bZoneInitialized
      data bZoneInitialized / mcom * .false.  /
      
      logical bZoneCouplingFound(mcom)
      save bZoneCouplingFound
      data bZoneCouplingFound / mcom * .false. /
      
C.....References       
      integer iWindowsForZone   ! Function returning the # and indicies
                                ! of the flow connections to the outside 
                                ! for a given zone 
      
      
C.....Check to see if zone-couplings have previously been examined 
      CheckZone: if ( .not. bZoneInitialized(iZone) ) then 

C.......Recover the number of exterior connections, and their 
C.......indicies using function iWindowsForZone              
        iWindowCount = iWindowsForZone(iZone,iWindowIndicies) 
        
        bFound = .false. 
C.......Loop through connections, and check if coupling component 
C.......is type 600
        WindowLoop: do iWindow = 1, iWindowCount 
          
C.........Get connection index, and coupling component type          
          iConnection = iWindowIndicies(iWindow) 
          iComponent  = iTpCon(iConnection )

C.........Check if component is type 600          
          if ( iTpCmp(iComponent) == 600 ) bFound = .true.
          
        enddo WindowLoop

C.......Store coupling value 
        bZoneCouplingFound( iZone ) = bFound
        
      endif CheckZone
      
C.....Report stored value      
      bFindAFN600Coupling =  bZoneCouplingFound(iZone) 
      
      return 
      end function bFindAFN600Coupling 
      
C------------------ iCountAFN110Couplings --------------------------------
C     Created by: Alex Ferguson
C     Created on: March 25, 2011 
C     Copyright:  Natural Resources Canada
C
C     This routine searches through all of the flow connections defined 
C     and returns the number and indicies of type 110 couplings. 
C
C-----------------------------------------------------------------------
      
      integer function iCountAFN110Couplings(iZone,iConnections)       
      implicit none 
      
      include "building.h"
      include "net_flow.h"      
      
C.....Passed variables        
      
      integer iZone                ! Index of current zone 
      integer iConnections(mcnn)   ! Connections for zone 
      
C.....ESP-r commons: flow network connection data.
      common/mflw10/nodps(mcnn),hgtps(mcnn),nodne(mcnn),hgtne(mcnn),
     &              itpcon(mcnn),ndscnn(mcnn,mcns)
      
      
      integer nodps  , ! Indicies of positive coupled nodes
     &        nodne  , ! Indicies of negative coupled nodes
     &        itpcon , ! Indicies of coupling flow components 
     &        ndscnn   ! Supplementary connection node indicies (not used)
                     
      real hgtps     , ! Height of positive node (m, not used)
     &     hgtne       ! Height of negative node (m, not used)
           
C.....flow component data           
      common/mflow9/itpcmp(mcmp),isdcmp(mcmp),isdcnn(mcmp),
     &              supcmp(mcmp,mcms)
      
      integer itpcmp,   ! Indicies of coupling components
     &        isdcmp,   ! (?, not used) 
     &        isdcnn    ! (?, not used)
      real supcmp       ! Supplementary component data (not used)

C.....Local variables       
      
      
      integer iWindowCount, ! # of windows 
     &        iWindow, iConnection, iComponent ! Pointers 

      integer iWindowIndicies(mcnn)  ! Array storing those indicies                         
      
      logical bFound
      
C.....Storage to save CPU cycles
      logical bZoneInitialized(mcom)
      save bZoneInitialized
      data bZoneInitialized / mcom * .false.  /
      

      integer iAFN110CouplingsFound(mcom)
      save iAFN110CouplingsFound 
      data iAFN110CouplingsFound / mcom * 0 /
      
      integer iAFN110CoulingIndicies(mcom,mcnn)
      save iAFN110CoulingIndicies 
      
      
      
C.....References       
      integer iWindowsForZone   ! Function returning the # and indicies
                                ! of the flow connections to the outside 
                                ! for a given zone 
      
      
C.....Check to see if zone-couplings have previously been examined 
      CheckZone: if ( .not. bZoneInitialized(iZone) ) then 
        
C.......Recover the number of exterior connections, and their 
C.......indicies using function iWindowsForZone              
        iWindowCount = iWindowsForZone(iZone,iWindowIndicies) 
        bFound = .false. 
C.......Loop through connections, and check if coupling component 
C.......is type 600
        WindowLoop: do iWindow = 1, iWindowCount 
C.........Get connection index, and coupling component type          
          iConnection = iWindowIndicies(iWindow) 
          iComponent  = iTpCon(iConnection)

C.........Check if component is type 110          
          if ( iTpCmp(iComponent) == 110 ) then 
            
            iAFN110CouplingsFound(iZone) = 
     &            iAFN110CouplingsFound(iZone) + 1 
     
            iAFN110CoulingIndicies(iZone,iAFN110CouplingsFound(iZone))
     &            = iConnection

          endif 
        enddo WindowLoop
        
        bZoneInitialized(iZone)  = .true. 
        
      endif CheckZone
      
C.....Report stored values      
      iCountAFN110Couplings = iAFN110CouplingsFound(iZone) 
      
      Window2Loop: do iWindow = 1, iCountAFN110Couplings
      
        iConnections(iWindow) = iAFN110CoulingIndicies(iZone,iWindow) 
!        print*, '>>>>>', iZone, iWindow, 
!     &                   iAFN110CoulingIndicies(iZone,iWindow) 
      enddo Window2Loop
      
      return 
      end function iCountAFN110Couplings 

      
      
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
      
C.....ESP-r commons 

 
      ! Flow network connection data.
      common/mflw10/nodps(mcnn),hgtps(mcnn),nodne(mcnn),hgtne(mcnn),
     &              itpcon(mcnn),ndscnn(mcnn,mcns)
      
          
      
      integer nodps  , ! Indicies of positive coupled nodes
     &        nodne  , ! Indicies of negative coupled nodes
     &        itpcon , ! Indicies of coupling flow components 
     &        ndscnn   ! Supplementary connection node indicies (not used)
                     
      real hgtps     , ! Height of positive node (m, not used)
     &     hgtne       ! Height of negative node (m, not used)
     
     
      common/mflow1/nnod,ncmp,ncnn     
      integer nnod, ncmp, ncnn
     
      common/mflow3/ndfld(mnod),ndtyp(mnod),hnod(mnod),supnod(mnod,mnds)
      integer ndfld, ndtyp
      real hnod, supnod      
          
      common/afn/iAirN,lAprob,icaas(mcom)
      integer iAirN, iCaas ! Flag for air flow network, map 
                           ! between network nodes and building zones
      character*72 lAprob  ! Airflow network file name 
     
C.....Local variables 
      ! Map to save some CPU cycles
      
           
      logical bInitialized(MCOM)
      data bInitialized / MCOM * .false. /
      save bInitialized
      
      integer iNumberOfWindows(MCOM)
      save iNumberOfWindows
      
      integer iCoupledConnMap(MCOM,MCNN)
      save iCoupledConnMap
      
      logical bMatched
      
      integer iPositiveNode, iNegativeNode

      integer iConnection ! Iterator     
      
      
      
C.....Check to see if we've already tested this connection and 
C.....return saved results, if so. 
      NoSavedData: if ( .not. bInitialized(iZone) ) then 
        
        iNumberOfWindows(iZone) = 0 

        ConnLoop: do iConnection  = 1, ncnn
        
          iPositiveNode = nodps(iConnection)
          iNegativeNode = nodne(iConnection) 
C.........Test if positive or negative node matches the current zone, 
C.........and if one of the nodes us type 3: boundary condition.
        
          ConnToOutside: if ( ( iCaas(iZone) == iPositiveNode .or. 
     &                          iCaas(iZone) == iNegativeNode      ) 
     &                        .and. 
     &                        ( NdTyp(iPositiveNode) == 3     .or. 
     &                          NdTyp(iNegativeNode) == 3          )
     &                       ) then 
          

             
            iNumberOfWindows(iZone) = iNumberOfWindows(iZone) + 1
            
            iCoupledConnMap(iZone,iNumberOfWindows(iZone)) = iConnection

          endif ConnToOutside
        
          
        enddo ConnLoop

         
C.......Set flag to ensure connection data is not reinitialized
        bInitialized(iZone) = .true.
      
      endif NoSavedData
     
C.....Update data to be passed back to calling routine.      
      iWindowsForZone = iNumberOfWindows(iZone)
      
      do iConnection = 1, iWindowsForZone
      
        iFlowConnection(iConnection) = 
     &         iCoupledConnMap( iZone, iConnection )
      
      enddo 
      
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
      
      USE AIM2_TIMESTEP, ONLY:AIM2_CONTROL_volume
      IMPLICIT NONE
      
      include "building.h"
      include "net_flow.h"

C.....Passed parameters
      integer iConnection     ! Index of connection 
      double precision fDeltaP         ! Pressure difference across component (Pa)
      double precision fDerivitive
      
C.....ESP-r commons 

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      REAL TFA,QFA

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

      
      real fScale                 ! Ratio for multiplying AIM-2 computed 
                                  ! flow (typically 1.)
      
      common/mflres/flw1(mcnn),flw2(mcnn),pres(mnod),
     &              resid(mnod),saflw(mnod)
     
      double precision Flw1, Flw2  ! Component flow data (in & out, kg/s)
      
      double precision PRES, RESID, SAFLW ! Other flow data (not used)

      common/mflow9/itpcmp(mcmp),isdcmp(mcmp),isdcnn(mcmp),
     &              supcmp(mcmp,mcms)
     
      real supcmp     ! Supplementary data for components 
      integer isdcmp, isdcnn, itpcmp  ! Not used       
      
      real fRhoAir


C.....indicies of positive and negative nodes. 
      integer iPositiveNode
      integer iNegativeNode 
      integer iZoneNode 

C.....AIM-2 computed infiltration for this zone       
      real fAIM2InfilFlow                         ! (m3/s) 
      
C.....Index of coupled zone (if any)
      integer iCoupledZone 
      logical bCoupledZone 
      integer iComponent

      real airden
      
C.....References
      logical bFlowConnToOutside

C.....Start calculations: Get positive and negative nodes:      
      iPositiveNode = NODPS(iConnection)
      iNegativeNode = NODNE(iConnection)
      
      iComponent = itpcon( iConnection )      
      
C.....Detemine if connection couples zone to outside via 
C.....function bFlowConnToOutside, and get coupled zone 
C.....index (iCoupledZone).      
      bCoupledZone = bFlowConnToOutside( iConnection,
     &                                   iCoupledZone )

      
     
C.....For coupled zones: Call AIM2_CONTROL_volume to compute 
C.....infiltration air flow rate for the coupled zone      
      if ( bCoupledZone ) then 
        if ( NdTyp(iPositiveNode) == 0 ) then 
          iZoneNode = iPositiveNode
        elseif ( NdTyp(iPositiveNode) == 0 ) then 
          iZoneNode = iNegativeNode
        else
          ! should be impossible 
        endif 
        
        call AIM2_CONTROL_volume(iCoupledZone, fAIM2InfilFlow)
      else 
C.......Non-coupled zone: set infiltration air flow to zero.      
        fAIM2InfilFlow = 0.
             
      endif 
  
      fScale = supcmp ( iComponent, 2 ) 

C.....Save infiltration rate as bidirectional mass flow in arrays 
C     Flw1 and Flw2
C
C        Flow rate (kg/s) = Infil. Air flow (m3/s) * density (kg/m3) 
C
      
      fRhoAir = AIRDEN(TF)
      
      Flw1(iConnection) =  fAIM2InfilFlow * fRhoAir
     &                     * dble ( fScale )
      Flw2(iConnection) = -fAIM2InfilFlow * fRhoAir
     &                     * dble ( fScale )

C.....Compute derivitive (to help flow solver?) 
      
      
      call MFNPDV(iConnection, fDeltaP, fDerivitive )

           
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

      
      Flw1(iConnection) = dble( fFlow ) * RHON(iPositiveNode)
      Flw2(iConnection) = dble(-fFlow ) * RHON(iPositiveNode)
      
C.....Compute derivitive (to help flow solver?) 
      
      call MFNPDV(iConnection, fDeltaP, fDerivitive )

           
      return
      end subroutine MF610_circ_fan_coupling     
      
