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
C======================================================================
C=============== plt_zone_gain_coupling.F =============================
C======================================================================
C
C Created by: Alex Ferguson
C Created on: August 02, 2005
C Copyright: Natural Resources Canada 2005
C
C This file contains routines used to store heat gains associated
C with plant components, and to recover this data to allowing the
C corresponding casual gains to be injected into the corresponding
C zone.
C
C - plt_zone_gain_inventory : Principle storage/retreval routine.
C
C - plt_zone_gain_zero: Interface for reinitiaizing gain storage
C       arrays
C
C - plt_zone_gain_calc: Interface for performing gain summation
C       calculations
C
C - store_plt_gain: Interface allowing storage of a
C       heat gain from a plant component a zone air
C       point, surface or layer 
C
C - fZone_convective_gain: Interface allowing retrival of convective
C       gains from plant components to specified zone
C
C - fZone_radiant_gain: Interface allowing retrevial of radiant
C       gains from plant components to specified zone
C
C - fSurface_convective_gain: Interface allowing retrevial of convective
C       gains from plant components to specified surface
C
C - fSurface_convective_gain: Interface allowing retrevial of radiant
C       gains from plant components to specified surface
C
C - fLayer_embedded_gain: interface allowing retrevial of embedded
C       (conductive) gains from plant components to a specified
C       construction layer.
C
C======================================================================
C

C---------------- plt_zone_gain_inventory() ---------------------------
C
C ABSTRACT:
C
C This routine implements a simple inventory for storing/recovering
C the casual gain data associated with plant components.
C
C Arguements
C
C   iAction: flag indicating which action should be performed
C            ( store / recover / calculate )
C
C   iIndex1/2/3: index of zone/surface/layer or plant compomnent
C           data is associated with
C
C   fGain_Conductive: Value of conductive (embedded) gain (input/output)
C   fGain_Convective: Value of convective gain (input/output)
C   fGain_Radiant: Value of radiant gain (input/output)
C
C----------------------------------------------------------------------
      subroutine plt_zone_gain_inventory (
     &                iAction,
     &                iIndex1, iIndex2, iIndex3,
     &                fGain_Value, 
     &                iGain_Type,
     &                iTimeRow,
     &                cContext   )
     
      implicit none
#include "building.h"
#include "plant.h"

C----------------------------------------------------------------------
C     Passed variables
C----------------------------------------------------------------------
      integer iAction, iIndex1, iIndex2, iIndex3
      real fGain_Value
      integer iGain_Type
      integer iTimeRow
      character*124 cContext
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C.....Input/output
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C.....Simulation type
      common/c6/indcfg
      integer indcfg
      
C.....Building data 
      common/c1/ncomp,ncon
      integer ncomp                  ! # of zones
      integer ncon                   ! ? (not used)

      common/c20/nzsur(mcom),nztv(mcom)
      integer nzsur           ! # of surfaces in a zone
      integer nztv            ! # of verticies in a zone (not used)

      common/prec9/nConst(mcom),nELts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      integer nConst          ! # of surfaces per zone
      integer nELts           ! # number of layers per surface (who knew?)
      integer ngaps           ! # number of air gaps witin surface (not used)
      integer npgap           ! Location of air gap. (not used)

C.....Gridding common 
      common/gr1d01/nnds,nndz(mcom),nndc(mcom,ms),nndl(mcom,ms,me)      
      integer nnds            ! ? (not used)
      integer nndz            ! ? (not used)
      integer nndc            ! # number of nodes within a construction
      integer nndl            ! # of nodes per construction layer
                               
C.....Containment data      
      common/c11/ncont,ipcc(mpcom),indcp(mpcom),cntdat(mpcom,3)
      integer nCont                  ! # of containments
      integer iPCC                   ! Index of plant component assoc. 
                                     !   w/ each containment i
      integer indcp                  ! Containment type
      real cntdat                    ! Containment supplementary data

      common / plt_zone_coupling /
     &                             bContained,
     &                             bSupported

      logical bContained (mpcom)     ! Flag indicating that containment
                                     !   has been defined for specified
                                     !   plant component.
     
      logical bSupported (mpcom,3)   ! Flag indicating that defined
                                     !   containment supports convective 
                                     !   heat gain

C.....Plant network data
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      integer npcomp                 ! # of plant components
      integer nci                    ! # of control loops associated w/
                                     !   each component
      real cdata                     ! Control data associated w/
                                     !   each component

C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
      integer iContainment_Type        ! # containment type
      real fNode_injection_fraction    ! fraction of heat injected into
                                       !   each interconstruction node (-)
C.....Gain inventories
      real fPltComp_Gains ( MPCom ,3 ) ! Plant component gains
      real fZone_Gains ( MCom,2,2)     ! Gains from plt components into
                                       !    corresponding zone air point
                                       !    (includes conv/rad components)
      real fSurf_Gains ( MCom, MS, 2,2) ! Gains from plt components onto
                                       !    at surface interface (includes
                                       !    conv/rad components)
      real fLayer_Gains ( MPCom, MS, MN,2) ! Gains from plt components into
                                       !    layers within surfaces.
                                       !    (conductive gains only!)

                                       
      save fPltComp_Gains, fZone_Gains, fSurf_Gains, fLayer_Gains
      

      
C.....Status flags
      logical bContainments               ! Do containments exist?
      save bContainments
      data bContainments / .false. /


      logical bInitialized                ! Has initalization been completed?
      save bInitialized
      data bInitialized / .false. /

      logical bFatal_Error                ! Was an error encountered?
      data bFatal_Error / .false. /
      save bFatal_Error
      
C.....Counters      
      integer iZone, iSurface, iLayer, iComponent, iContainment
      integer iLoop_Layer, iNode, iStart_node, iEnd_node

C.....Character buffer
      character*124 cMsg
      
      
C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)

C.....Timerows
      integer iPresent, iFuture
      parameter (iPresent=1, iFuture=2) 
          
      integer iBld_Containment        ! named constant for construction
                                      ! containment
      parameter ( iBld_Containment = 3 )                                       

      integer iBldPlt                 ! Named constant for bld+plt simulation
      parameter ( iBldPlt = 3 )
      
C----------------------------------------------------------------------
C     Initialize, if necessary
C----------------------------------------------------------------------
      if ( .not. bInitialized ) then
C----------------------------------------------------------------------
C        Check for plant containments and set flag if containments 
C        are found 
C----------------------------------------------------------------------
         if ( iNdCfg .eq. iBldPlt .and. nCont .gt. 0 ) 
     &      bContainments = .true.
         if ( bContainments ) then
C-------------------------------------------------------------------------
C           Zero Storage arrays
C-------------------------------------------------------------------------
            do iZone = 1, nComp
C..............Zone air point gains
               fZone_Gains ( iZone, iConvective, iPresent ) = 0.0
               fZone_Gains ( iZone, iRadiant,    iPresent ) = 0.0
               fZone_Gains ( iZone, iConvective, iFuture  ) = 0.0
               fZone_Gains ( iZone, iRadiant,    iFuture  ) = 0.0
               
               
               do iSurface = 1, nConst( iZone )
C.................Surface gains
                  fSurf_Gains(iZone, iSurface, iConvective,iPresent )
     &                   = 0.0
                  fSurf_Gains(iZone, iSurface, iRadiant,   iPresent )
     &                   = 0.0
                  fSurf_Gains(iZone, iSurface, iConvective, iFuture )
     &                   = 0.0
                  fSurf_Gains(iZone, iSurface, iRadiant,    iFuture )
     &                   = 0.0

                  do iNode = 1, nndc( iZone, iSurface)
C....................layer gains (embedded - conductive only)
                     fLayer_Gains(iZone,iSurface,iNode, iPresent ) = 0.0
                     fLayer_Gains(iZone,iSurface,iNode, iFuture  ) = 0.0

                  enddo

               enddo

            enddo

            
C-------------------------------------------------------------------------
C           Loop through containments, and validate zone/surface/layer
C           indicies. Set common bSupported flags to allow interfaces
C           to warn user when specified gain is not supported by defined
C           containments.
C-------------------------------------------------------------------------
            do iContainment = 1, nCont
C..............Get plant component index
               iComponent = ipcc ( iContainment )

C..............Check containment type
               iContainment_Type = iNdCp(iContainment)            
               if ( iContainment_Type .eq. iBld_Containment ) then
C.................Get corresponding zone/surface/layer data
                  iZone    = int ( CntDat(iContainment,1) )
                  iSurface = int ( CntDat(iContainment,2) )
                  iLayer   = int ( CntDat(iContainment,3) )

C.................Check if zones/surfaces/layers have been defined
C.................and are valid
                  if ( iZone    .ne. 0 .and.
     &                 iSurface .eq. 0 .and.
     &                 iLayer   .eq. 0       ) then
C....................Zone air point containment.
                     if ( iZone .lt. 1 .or. iZone .gt. nComp ) then
                        bFatal_Error = .true.
                     else
                        bContained ( iComponent )           = .true.
                        bSupported (iComponent,iConductive) = .false.
                        bSupported (iComponent,iConvective) = .true.
                        bSupported (iComponent,iRadiant   ) = .true.
                     endif

                  elseif ( iZone    .ne. 0 .and.
     &                     iSurface .ne. 0 .and.
     &                     iLayer   .eq. 0       ) then
C....................Surface containment
                     if (      iZone    .lt. 1
     &                    .or. iZone    .gt. ncomp 
     &                    .or. iSurface .lt. 1
     &                    .or. iSurface .gt. nConst(iZone) ) then
                        bFatal_Error = .true.
                     else
                        bContained ( iComponent )           = .true.
                        bSupported (iComponent,iConductive) = .false.
                        bSupported (iComponent,iConvective) = .false.
                        bSupported (iComponent,iRadiant   ) = .true.
                     endif                   
                     
                  elseif ( iZone    .ne. 0 .and.
     &                     iSurface .ne. 0 .and.
     &                     iLayer   .ne. 0       ) then
C....................Layer containment 
                     if (      iZone    .lt. 1
     &                    .or. iZone    .gt. ncomp 
     &                    .or. iSurface .lt. 1
     &                    .or. iSurface .gt. nConst(iZone)
     &                    .or. iLayer   .lt. 1
     &                    .or. iLayer   .gt. nELts(iZone,iSurface)
     &                   ) then
                        bFatal_Error = .true.
                     else
                        bContained ( iComponent )           = .true.
                        bSupported (iComponent,iConductive) = .true.
                        bSupported (iComponent,iConvective) = .false.
                        bSupported (iComponent,iRadiant   ) = .false.
                                             
                     endif

                  else
C....................Zone/surface/layer data incorrectly defined
                     bFatal_Error = .true.
                  endif 

                  if ( bFatal_Error ) then
C....................Warn user
                     write (cMsg, '(A,I2,A,I2,A,I2,A,I2,A)' )
     &                 'Plant containment #', iContainment,
     &                 ': Zone #', iZone,
     &                 ' Surface #', iSurface,
     &                 ' Layer #', iLayer, ' are invalid.'
     
                  endif             

               else
C.................Containment does not correspond to a zone/surface/layer.
C.................do nothing
               endif 
            
            enddo  ! end containment loop
            
         endif ! <- matches if ( bContainments )...

C........Set initialization flag
         bInitialized = .true.
         
      endif  ! <- matches if (.not. bInitialized ...
C----------------------------------------------------------------------
C     End of initialization 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C     Check value of iAction, and perform necessary operations
C----------------------------------------------------------------------      
      if ( iAction .eq. iZero .and. bContainments ) then
C----------------------------------------------------------------------
C        Transport time row data and reinitialize future storage arrays
C---------------------------------------------------------------------- 
         do iZone = 1, nComp
C...........Zone air point gains
            fZone_Gains ( iZone, iConvective, iPresent ) 
     &        = fZone_Gains ( iZone, iConvective, iFuture )
            fZone_Gains ( iZone, iRadiant,    iPresent ) 
     &        = fZone_Gains ( iZone, iRadiant,    iFuture )
            fZone_Gains ( iZone, iConvective, iFuture ) = 0.0
            fZone_Gains ( iZone, iRadiant,    iFuture ) = 0.0
            
            do iSurface = 1, nConst( iZone )
C..............Surface gains
               fSurf_Gains(iZone, iSurface, iConvective, iPresent ) 
     &           = fSurf_Gains(iZone, iSurface, iConvective, iFuture )
               fSurf_Gains(iZone, iSurface, iRadiant,    iPresent ) 
     &           = fSurf_Gains(iZone, iSurface, iRadiant,    iFuture )
               fSurf_Gains(iZone, iSurface, iConvective, iFuture ) = 0.0
               fSurf_Gains(iZone, iSurface, iRadiant,    iFuture ) = 0.0

               do iNode = 1, nndc(iZone, iSurface)
C.................layer gains (embedded - conductive only)
                  fLayer_Gains(iZone,iSurface,iNode, iPresent )
     &               = fLayer_Gains(iZone,iSurface,iNode, iFuture )
                  fLayer_Gains(iZone,iSurface,iNode, iFuture ) = 0.0

               enddo

            enddo

         enddo

      elseif ( iAction .eq. iStore .and. bContainments ) then
C----------------------------------------------------------------------
C        Data is to be stored.
c
C        Check if the passed arguement (plant component index)
C        is within bounds
C----------------------------------------------------------------------
         iComponent = iIndex1
         
         if ( iComponent .lt. 1 .or.
     &        iComponent .gt. npcomp ) then
            bFatal_Error = .true.
            write (cMsg, '(A,I2,A,I2,A,I2,A,I2,A)' )
     &              'Specified plant component index (',
     &              iComponent,') is invalid.'
         else

C----------------------------------------------------------------------
C           Store gain flux (note: 3 calls are needed to populate
C           all gain types
C----------------------------------------------------------------------
            fPltComp_Gains(iComponent,iGain_Type) = fGain_Value
         endif

      elseif ( iAction .eq. iCalculate .and. bContainments) then
C----------------------------------------------------------------------
C        Perform summation calculations
C----------------------------------------------------------------------       
C----------------------------------------------------------------------
C        Loop through containments, check types and
C        aggregate into appropriate zones
C----------------------------------------------------------------------
         do iContainment = 1, nCont
C...........Check type
            iContainment_Type = iNdCp ( iContainment )
C...........Get plant component index
            iComponent = ipcc ( iContainment )
            
            if ( iContainment_Type .eq. iBld_Containment ) then
C..............Containment corresponds to building space.
C..............Get corresponding zone/surface/layer data
               iZone    = int ( CntDat(iContainment,1) )
               iSurface = int ( CntDat(iContainment,2) )
               iLayer   = int ( CntDat(iContainment,3) )
C..............Determine if containment describes
C..............Zone/surface/layer. Append value to sum for current
C..............containment.Assume that zone/surface/layer numbers
C..............have been error trapped above.
               if ( iZone    .ne. 0 .and.
     &              iSurface .eq. 0 .and.
     &              iLayer   .eq. 0       ) then
C.................Zone air point containment.
                  fZone_Gains(iZone,iConvective,iFuture) =
     &               fZone_Gains(iZone,iConvective,iFuture)
     &             + fPltComp_Gains( iComponent, iConvective )
                  fZone_Gains(iZone,iRadiant ,iFuture) =
     &               fZone_Gains(iZone,iRadiant ,iFuture)
     &             + fPltComp_Gains( iComponent, iRadiant )

               elseif ( iZone    .ne. 0 .and.
     &                  iSurface .ne. 0 .and.
     &                  iLayer   .eq. 0       ) then
C..................Surface containment
                   fSurf_Gains(iZone,iSurface,iConvective ,iFuture) =
     &                fSurf_Gains(iZone,iSurface,iConvective ,iFuture) 
     &              + fPltComp_Gains( iComponent, iConvective )
                   fSurf_Gains(iZone,iSurface,iRadiant ,iFuture) =
     &                fSurf_Gains(iZone,iSurface,iRadiant ,iFuture) +
     &                fPltComp_Gains( iComponent, iRadiant )  

               elseif ( iZone    .ne. 0 .and.
     &                  iSurface .ne. 0 .and.
     &                  iLayer   .ne. 0       ) then
C.................Layer containment. Note: heat is not injected
C.................into layers, but rather into an arbitrary # of
C.................nodes within the layers. We need to determine
C.................which nodes are relevant, and the fraction
C.................of heat that is injected into each node. 

C.................Fraction of total gain injectected in to each
C.................inter-construction node:
                  if ( nndl ( iZone, iSurface, iLayer) .lt. 3 ) then
C....................There's only one inter-construction node                     
                     fNode_injection_fraction = 1.0
                  else
C....................Assume heat injection is uniform into all
C....................inter-construction nodes                  
                     fNode_injection_fraction = 1.0 /
     &                  float ( nndl ( iZone, iSurface, iLayer) - 1)
                  endif 

C----------------------------------------------------------------------
C                 Identify the first interconstruction node in the
C                 current layer. 
C----------------------------------------------------------------------                  
                  iStart_node = 1
                  if ( iLayer .gt. 1 ) then
                     do iLoop_Layer = 1, iLayer -1

                        iStart_node = iStart_node +
     &                                nndl(iZone,iSurface,iLoop_Layer)

                     enddo

                  endif 

C.................Start node now decribes boundary of current layer.                  
C.................Increment start node for first inter-consturction node
                  iStart_node = iStart_node + 1

C----------------------------------------------------------------------
C                 Determine last interconstruction node.
C----------------------------------------------------------------------
                  if ( nndl(iZone,iSurface,iLayer) .lt. 3 ) then
C....................There's only 1 interconstruction node
                     iEnd_node = iStart_node
                  else                                        
                     iEnd_node = iStart_node
     &                           + nndl(iZone, iSurface, iLayer) - 1
                  endif 

C--------------------------------------------------------------------
C                 Loop through interconstruction nodes, and append
C                 heat injection.
C--------------------------------------------------------------------                                    
                  do iNode = iStart_node, iEnd_node

                    fLayer_Gains(iZone,iSurface,iNode,iFuture) =
     &                 fLayer_Gains(iZone,iSurface,iNode,iFuture)
     &                 + fPltComp_Gains( iComponent, iConductive )
     &                 * fNode_injection_fraction

                  enddo
     
               endif

            else
C..............Containment does not have an associated zone/surface/layers
C..............do nothing.
            endif

         enddo

      elseif( iAction .eq. iRetrive .and. bContainments ) then
C----------------------------------------------------------------------
C        Recover data from inventory
C----------------------------------------------------------------------
         iZone    = iIndex1
         iSurface = iIndex2
         iNode    = iIndex3

         if ( iZone    .gt. 0 .and.
     &        iSurface .eq. 0 .and.
     &        iNode    .eq. 0       ) then
C...........Gain is to zone air point     
            fGain_Value = fZone_Gains( iZone, iGain_Type, iTimeRow )
         
         elseif ( iZone    .gt. 0 .and.
     &            iSurface .gt. 0 .and.
     &            iNode    .eq. 0       ) then
C...........Gain on a surface   
            fGain_Value =
     &              fSurf_Gains( iZone, iSurface, iGain_Type, iTimeRow)

         elseif ( iZone    .gt. 0 .and.
     &            iSurface .gt. 0 .and.
     &            iNode    .gt. 0       ) then
C...........Gain is embedded within a layer. Conductive only!
            fGain_Value = fLayer_Gains(iZone,iSurface,iNode,iTimeRow)

         else
C...........Containment index incorrectly assigned.
            bFatal_Error = .true.
            write (cMsg, '(A,I2,A,I2,A,I2,A)' )
     &              ' Zone #', iZone,
     &              ' Surface #', iSurface,
     &              ' Node #', iNode, ' are invalid.'
         endif

      elseif ( .not. bContainments ) then
C........Return a gain of 0.
         fGain_Value = 0.0         
         
      else
C........Action type not supported.
         bFatal_Error = .true. 
         write (cMsg, '(A,I2,A)' )
     &              'Passed action (', iAction, ') is not supported'

      endif             

C----------------------------------------------------------------------
C     Error handling      
C----------------------------------------------------------------------      
      if ( bFatal_Error ) then
         call edisp (iuout, cContext )
         call edisp (iuout, cMsg )
         stop "plt_zone_gain_inventory(): Unresolvable error"
      endif


      return
      end 




C---------------- plt_zone_gain_zero() ------------------------------------
C
C ABSTRACT:
C
C This is a simple interface that activates the plant->zone gain
C inventory's initialization facility
C
C INPUTS:
C
C   None
C
C OUTPUTS:
C
C   NONE
C
C----------------------------------------------------------------------
      subroutine plt_zone_gain_zero ()
      implicit none
#include "plant.h"           
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
C.....Dummy arguments
      integer iDummy1,iDummy2, iDummy3, iDummy5, iDummy6
      real fDummy4

C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)

C-------------------------------------------------------------------
C     Pass gain data to inventory
C-------------------------------------------------------------------
      call plt_zone_gain_inventory(
     &             iZero,
     &             iDummy1, iDummy2, iDummy3,
     &             fDummy4,
     &             iDummy5,
     &             iDummy6,
     &             'Initializing plant heat gain inventory.'   )



      return
      end

C---------------- plt_zone_gain_calc() ------------------------------------
C
C ABSTRACT:
C
C This is a simple interface that activates the plant->zone gain
C inventory's calculation facility
C
C INPUTS:
C
C   None
C
C OUTPUTS:
C
C   NONE
C
C----------------------------------------------------------------------
      subroutine plt_zone_gain_calc ()
      implicit none
#include "plant.h"           
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
C.....Dummy arguments
      integer iDummy1,iDummy2, iDummy3, iDummy5, iDummy6
      real fDummy4

C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)

C-------------------------------------------------------------------
C     Pass gain data to inventory
C-------------------------------------------------------------------
      call plt_zone_gain_inventory(
     &             iCalculate,
     &             iDummy1, iDummy2, iDummy3,
     &             fDummy4,
     &             iDummy5,
     &             iDummy6,
     &             'Calculating containment gains from'
     &             // ' plant components.'   )



      return
      end

C---------------- store_plt_gain() ------------------------------------
C
C ABSTRACT:
C
C This is a simple interface that stores the specified convective gain
C into the plant->zone gain inventory
C
C INPUTS:
C
C   iComponent: plant component for which the gain has been specified
C
C   fGain: Calculated heat gain
C
C   iGain_Type: Flag indicating gain type.
C
C OUTPUTS:
C
C   NONE
C
C----------------------------------------------------------------------
      subroutine store_plt_gain (
     &                iComponent,
     &                fGain,
     &                iGain_Type)
      implicit none
#include "plant.h"           
C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iComponent            ! Plant component index
      real fGain                    ! Calculated gain
      integer iGain_Type            ! Gain type
C----------------------------------------------------------------------
C     Commons 
C----------------------------------------------------------------------
C.....Inlet/outlet 
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen


      common / plt_zone_coupling /
     &                             bContained,
     &                             bSupported

      logical bContained (mpcom)     ! Flag indicating that containment 
                                     !   has been defined for given
                                     !   plant component.
      logical bSupported (mpcom,3)   ! Flag indicating that defined
                                     !   containment supports convective 
                                     !   heat gain

C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
C.....Dummy arguments
      integer iDummy2, iDummy3, iDummy6
C.....Context buffer
      character*124 cMsg
      character*124 cContext
      save cContext
      
C.....Status flags
      logical bInitialized
      data bInitialized / .false. /
         
      logical bWarned (mpcom,3)
      save bInitialized, bWarned

C.....Counters
      integer iCount       
C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)


C----------------------------------------------------------------------
C     Initialize
C----------------------------------------------------------------------
      if ( .not. bInitialized ) then
         write (cContext, '(A,A)')
     &        'store_convective_plt_gain(): Saving calculated ',
     &        ' gain for plant component.'
         do iCount=1,mpcom
            bWarned (iCount,iConvective ) = .false.
            bWarned (iCount,iConductive ) = .false.
            bWarned (iCount,iRadiant )    = .false.
         enddo
         bInitialized = .true.
      endif

C----------------------------------------------------------------------
C     Check that convective gains are supported by current containment,
C     and if not, warn user.
C----------------------------------------------------------------------      
      if ( bContained ( iComponent )                  .and.
     &     .not. bSupported ( iComponent,iGain_Type ) .and.
     &     .not. bWarned ( iComponent,iGain_Type )           ) then

         if ( iGain_Type .eq. iConvective ) then
            write(cMsg,'(A,A,I2)')
     &         'Warning: A convective gain has been calculated for',
     &         ' Component ',iComponent
         
            call edisp (iuout, cMsg)

            write(cMsg,'(A,A)')
     &         '         but the specified containment does not',
     &         ' support convective gains.'

            call edisp (iuout, cMsg)


         elseif ( iGain_Type .eq. iConductive ) then


            write(cMsg,'(A,A,I2)')
     &         'Warning: A conductive gain has been calculated for',
     &         ' Component ',iComponent
         
            call edisp (iuout, cMsg)

            write(cMsg,'(A,A)')
     &         '         but the specified containment does not',
     &         ' support conductive gains.'

            call edisp (iuout, cMsg)



         elseif ( iGain_Type .eq. iRadiant ) then

            write(cMsg,'(A,A,I2)')
     &         'Warning: A radiant gain has been calculated for',
     &         ' Component ',iComponent
         
            call edisp (iuout, cMsg)

            write(cMsg,'(A,A)')
     &         '         but the specified containment does not',
     &         ' support radiant gains.'

            call edisp (iuout, cMsg)

         endif
         
         bWarned (iComponent,iGain_Type) = .true.

         write(cMsg,'(A)')
     &         '         The calculated gain will be ignored. '

         call edisp (iuout, cMsg)

      elseif ( bContained ( iComponent )           .and.
     &         bSupported( iComponent, iGain_Type )      ) then
C----------------------------------------------------------------------
C        Pass gain data to inventory
C----------------------------------------------------------------------     
         call plt_zone_gain_inventory(
     &                iStore,
     &                iComponent, iDummy2, iDummy3,
     &                fGain,
     &                iGain_Type,
     &                iDummy6,
     &                cContext   )
      

      endif 


      return
      end


C---------------- fPltZone_convective_gain() -----------------------------
C
C ABSTRACT:
C
C This is a simple function that queries the plant->zone casual gain
C coupling inventory and returns the assocaited gains for a given zone.
C
C INPUTS:
C
C   iZone: Zone index
C
C
C OUTPUTS:
C
C   fZone_convective_gain: convective gain for the specified zone (W)
C
C----------------------------------------------------------------------
      real function fPltZone_convective_gain ( iZone, iTimeRow )
      implicit none
#include "plant.h"      
C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iZone        ! Zone index
      integer iTimeRow     ! Timerow (present/future)
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
C.....Dummy arguments
      integer iSurface, iLayer
      parameter (iSurface = 0, iLayer=0)
      real fGain_Value

C.....Context buffer
      character*124 cContext
      save cContext
      
C.....Status flags
      logical bInitialized
      data bInitialized / .false. /
      save bInitialized

C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)

C----------------------------------------------------------------------
C     Initialize
C----------------------------------------------------------------------
      if ( .not. bInitialized ) then
         write (cContext, '(2A)')
     &     'fZone_convective_gain(): Retrieving ',
     &     ' convective heat gain from plant components.'
        bInitialized = .true.
      endif
 
C-------------------------------------------------------------------
C     Recover gain from inventory 
C-------------------------------------------------------------------
      call plt_zone_gain_inventory(
     &                iRetrive,
     &                iZone, iSurface, iLayer,
     &                fGain_Value,
     &                iConvective,
     &                iTimeRow,
     &                cContext   )
      
      fPltZone_convective_gain = fGain_Value

      return
      end


C---------------- fPltZone_radiant_gain() -----------------------------
C
C ABSTRACT:
C
C This is a simple function that queries the plant->zone casual gain
C coupling inventory and returns the assocaited gains for a given zone.
C
C INPUTS:
C
C   iZone: Zone index
C
C
C OUTPUTS:
C
C   fZone_radiative_gain: radiative gain for the specified zone (W)
C
C----------------------------------------------------------------------
      real function fPltZone_radiant_gain ( iZone, iTimeRow )
      implicit none
#include "plant.h"
C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iZone        ! Zone index
      integer iTimeRow     ! time row
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
C.....Dummy arguments
      integer iSurface, iLayer
      parameter (iSurface = 0, iLayer=0)
      real fGain_Value

C.....Context buffer
      character*124 cContext
      save cContext
      
C.....Status flags
      logical bInitialized
      data bInitialized / .false. /
      save bInitialized

C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)

C----------------------------------------------------------------------
C     Initialize
C----------------------------------------------------------------------
      if ( .not. bInitialized ) then
         write (cContext, '(2A)')
     &     'fZone_radiant_gain(): Retrieving ',
     &     'radiant heat gain from plant components.'
         bInitialized = .true.
      endif

C-------------------------------------------------------------------
C     Recover gain from inventory
C-------------------------------------------------------------------
      call plt_zone_gain_inventory(
     &                iRetrive,
     &                iZone, iSurface, iLayer,
     &                fGain_Value,
     &                iRadiant,
     &                iTimeRow,
     &                cContext   )
      
      fPltZone_radiant_gain = fGain_Value
     
      return
      end




C---------------- fSurface_radiant_gain() -----------------------------
C
C ABSTRACT:
C
C This is a simple function that queries the plant->Surface casual gain
C coupling inventory and returns the assocaited gains for a given Surface.
C
C INPUTS:
C
C   iZone: Zone index
C   iSurface: Surface index
C
C
C OUTPUTS:
C
C   fSurface_radiative_gain: radiative gain for the specified Surface (W)
C
C----------------------------------------------------------------------
      real function fSurface_radiant_gain ( iZone, iSurface, iTimeRow )
      implicit none
#include "plant.h"      
C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iZone           ! Zone index
      integer iSurface        ! Surface index
      integer iTimeRow        ! Time row 
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
C.....Dummy arguments
      integer iLayer
      parameter (iLayer=0)
      real fGain_Value

C.....Context buffer
      character*124 cContext
      save cContext
      
C.....Status flags
      logical bInitialized
      data bInitialized / .false. /
      save bInitialized

C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)
C----------------------------------------------------------------------
C     Initialize
C----------------------------------------------------------------------
      if ( .not. bInitialized ) then
         write (cContext, '(2A)')
     &     'fSurface_radiant_gain(): Retrieving ',
     &     'radiant heat gain from plant components.'
         bInitialized = .true.
      endif

C-------------------------------------------------------------------
C     Recover gain from inventory
C-------------------------------------------------------------------
      call plt_zone_gain_inventory(
     &                iRetrive,
     &                iZone, iSurface, iLayer,
     &                fGain_Value,
     &                iRadiant,
     &                iTimeRow,
     &                cContext   )
      
      fSurface_radiant_gain = fGain_Value
     
      return
      end      
      

C---------------- fNode_embedded_gain() -----------------------------
C
C ABSTRACT:
C
C This is a simple function that queries the plant->Surface casual gain
C coupling inventory and returns the assocaited gains for a given
C construction node
C
C INPUTS:
C
C   iZone: Zone index
C   iSurface: Surface index
C   inODE: Layer index
C
C
C OUTPUTS:
C
C   fLayer_embedded_gain: embedded gain for specified layer (W)
C
C----------------------------------------------------------------------
      real function fNode_embedded_gain ( iZone,
     &                                     iSurface,
     &                                     iNode,
     &                                     iTimeRow )
      implicit none
#include "plant.h"
C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iZone           ! Zone index
      integer iSurface        ! Surface index
      integer iNode           ! Construction node index
      integer iTimeRow        ! Timerow of data to be returned 
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
      real fGain_Value

C.....Context buffer
      character*124 cContext
      save cContext
      
C.....Status flags
      logical bInitialized
      data bInitialized / .false. /
      save bInitialized

C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)
C----------------------------------------------------------------------
C     Initialize
C----------------------------------------------------------------------

      if ( .not. bInitialized ) then
         write (cContext, '(2A)')
     &     'fNode_embedded_gain(): Retrieving ',
     &     'embedded heat gain from plant components.'
         bInitialized = .true. 
      endif
C-------------------------------------------------------------------
C     Recover gain from inventory
C-------------------------------------------------------------------
      call plt_zone_gain_inventory(
     &                iRetrive,
     &                iZone, iSurface, iNode,
     &                fGain_Value,
     &                iConductive,
     &                iTimeRow,
     &                cContext   )
      
      fNode_embedded_gain = fGain_Value
      
      return
      end


C---------------- MiscPlantHeatGain()--------------------------------
C
C ABSTRACT:
C
C This is a dummy plant component that does nothing more than impose
C a heat gain into its containing zone/surface/construction layer.
C
C Inputs:
C
C    iIndex: Index of plant component
C    iMatrix: Index of matrix to be solved
C
C Outputs:
C
C    fCoefficients: Array of matrix coefficients
C----------------------------------------------------------------------
      subroutine MiscPlantHeatGain(iIndex,fCoefficients,iMatrix)
      implicit none
#include "plant.h"

      integer iIndex
      real fCoefficients(mpcoe)
      integer iMatrix

      real fGain_Conductive, fGain_Convective, fGain_Radiant

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component
      
C----------------------------------------------------------------------
C     Named constants 
C----------------------------------------------------------------------      
C.....Actions      
      integer iZero, iStore, iCalculate, iRetrive
      parameter (iZero=0, iStore=1, iCalculate=2, iRetrive=3)

C----------------------------------------------------------------------
C     Collect gains from adata common 
C----------------------------------------------------------------------
      fGain_Convective = ADATA ( iIndex, 1 )  ! (W)
      fGain_Radiant    = ADATA ( iIndex, 2 )  ! (W)
      fGain_Conductive = ADATA ( iIndex, 3 )  ! (W)

C----------------------------------------------------------------------
C     Store gains 
C----------------------------------------------------------------------
      call store_plt_gain ( iIndex, fGain_Radiant, iRadiant)
      call store_plt_gain ( iIndex, fGain_Convective, iConvective)
      call store_plt_gain ( iIndex, fGain_Conductive, iConductive)

C----------------------------------------------------------------------
C     Calculate dummy coefficients for all matricies
C----------------------------------------------------------------------
      fCoefficients(1) = 1.0
      fCoefficients(2) = 0.0

      return
      end
      
            
      
