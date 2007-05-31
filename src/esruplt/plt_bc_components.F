C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

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
C Boston, MA 02111-1307 USA.C
C
C================= plt_bc_components.F ================================
C
C Author:    Alex Ferguson
C Created:   June 30, 2005
C Copyright: Natural Resources Canada 2005
C
C This file contains procedures used to impose arbitrary boundary
C conditions in the plant domain.
C
C Boundry_Condition_Control: This procedure imposes a specified
C   boundary condition as an arbitrary plant component control.
C
C----------------- Boundry_Condition_Control()-------------------------
C
C This procedure reads a specified boundary condition from the
C boundary condition management facility and applies it as
C a control signal for a specified plant component 
C
C Inputs:
C
C   iterp - # of itertions copleted in the plant network for the
C           current time step
C
C Outputs:
C
C   none
C
C----------------------------------------------------------------------
      subroutine Boundry_Condition_Control()
      implicit none

      INCLUDE 'building.h'
      INCLUDE 'plant.h'

C----------------------------------------------------------------------
C     ESP-r commons 
C----------------------------------------------------------------------
C.....I/O
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C.....Control parameters      
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),NPCDT(MCL),
     &            IPCDV(MCL,MPCDT,2),NPCDP(MCL,MPCDT),
     &            TPCPS(MCL,MPCDT,MPCDP),IPCTYP(MCL,MPCDT,MPCDP),
     &            IPCLAW(MCL,MPCDT,MPCDP),PMISCD(MCL,MPCDT,MPCDP,MISC)
      
      integer ncl                ! # of control loops in model,
      integer ipsn               ! sensor data
      integer ipan               ! actuator data 
      integer npcdt              ! (not used)
      integer ipcdv              ! (not used)
      integer npcdp              ! (not used)
      real tpcps                 ! (not used)
      integer ipctyp             ! (not used)
      integer ipclaw             ! (not used)
      real pmiscd                ! miscellaneous data describing comp. 
      
C.....More control parameters 
      common/pclsol/icfp,idtypp,iperp
      integer icfp          ! index of current cotnrol loop
      integer idtypp        ! (not used)
      integer iperp         ! (not used)

C.....Plant network & control data
      common/c9/npcomp,nci,cdata
      integer npcomp            ! number of plant components
      integer nci(mpcom)        ! number of controls / component
      real cdata(mpcom,mmiscd)  ! control data for each component

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fDay                  ! real # describing the currend day,
                                 ! (& fraction thereof)

      integer iTarget_index      ! Index of target component                                  
      integer iTarget_var        ! Control variable to be manipulated


      integer iInterp_Method     ! Interpolation method.
      integer iData_Count        ! # of data expected.
      integer iBC_column         ! Column to be used.

      real fBC_value             ! Value of boundary condition

      character*124 cContext     ! Buffers used to write messages to
      character*124 cMsg         ! screen.
      
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------                                 
      real fConvert_current_TS_to_DAY ! Fn converting current timestep to day
      real fGet_BC_data_by_col        ! Fn collecting value of BC 

C-----------------------------------------------------------------------
C     Context buffer
C-----------------------------------------------------------------------
      write (cContext, '(A,A,I2)') 'Boundry_Condition_Control(): ',
     &        'Collecting BC data for control loop ', icfp


C-----------------------------------------------------------------------
C     Determine actuated component index, and actuated variable
C-----------------------------------------------------------------------
      iTarget_index = ipan(icfp,2)
      iTarget_var   = ipan(icfp,3)

C-----------------------------------------------------------------------
C     Collect control inputs
C-----------------------------------------------------------------------      
C.....# of data expected - should be 2.
      iData_Count    = int( pmiscd( icfp, idtypp, iperp, 1) )
C.....Column containing boundary condition data     
      iBC_column     = int( pmiscd( icfp, idtypp, iperp, 2) )
C.....Interpolation method (step/linear)      
      iInterp_Method = int( pmiscd( icfp, idtypp, iperp, 3) )

C-----------------------------------------------------------------------
C     Error trap # of data. Note: function fGet_BC_data_by_col will
C     error trap the other inputs.
C-----------------------------------------------------------------------

      if ( iData_Count .ne. 2 ) then
         write (cMsg, '(A,I2,A)') '2 data items were expected, but ',
     &       iData_Count, ' were provided.'

         call edisp( iuout, cContext )
         call edisp( iuout, cMsg )
         stop 'Fatal error!'

      endif

C-----------------------------------------------------------------------
C     Use function fConvert_current_TS_to_DAY to collect the current
C     time in the plt domain and convert to a real representing
C     the current day ( & fraction thereof )
C-----------------------------------------------------------------------
      fDay = fConvert_current_TS_to_DAY()
      
C-----------------------------------------------------------------------
C     Use function fGet_BC_data_by_col to return current value of
C     requested boundary condition column.
C-----------------------------------------------------------------------

       fBC_value = fGet_BC_data_by_col( iBC_column,
     &                                  fDay,
     &                                  iInterp_Method,
     &                                  cContext          )

C----------------------------------------------------------------------
C     Set plant component control signal 
C----------------------------------------------------------------------
      cdata ( iTarget_index, iTarget_var ) = fBC_value
      
      return
      end

C----------------- plt_source_component () ----------------------------
C
C This procedure generates ESP-r's matrix coefficients for a plant
C water flow source component. The temperature and flow rate of
C the component can be actuated using the component's
C control variables:
C
C  - CDATA(index,1): source temperature (oC)
C  - CDATA(index,2): source flow rate (kg/s)
C  
C
C Inputs:
C
C    iIndex: index of component in network
C    iPlant_matrix: Index of matrix to be solved.
C 
C Outputs:
C
C    fCoefficients: Array containing ESP-r matrix coefficients
C
C----------------------------------------------------------------------
      subroutine plt_source_component( iIndex,
     &                                 fCoefficients,
     &                                 iPlant_matrix  )
      implicit none

      INCLUDE 'plant.h'
      INCLUDE 'CETC_definitions.h'

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iIndex                      ! index of component
      integer iPlant_matrix               ! index of matrix to be solved
      real fCoefficients(mpcoe)           ! matrix coeffieients

C----------------------------------------------------------------------
C     ESP-r Commons
C----------------------------------------------------------------------

C.....Network & control data
      common/c9/npcomp,nci,cdata
      integer npcomp            ! number of plant components
      integer nci(mpcom)        ! number of controls / component
      real cdata(mpcom,mmiscd)  ! control data for each component

C.....Plant component configuration data - read from input file
      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------

      real fMaxTemp            ! Maximum source temperature (oC)
      real fMinTemp            ! Minimum source temperature (oC)

      real fMaxFlow            ! Maximum source flow (kg/s)
      real fMinFlow            ! Minimum source flow (kg/s)

      real fControlTemp        ! Temperature requested by controller (oC)
      real fControlFlow        ! Flow rate requested by controller (oC)

      logical bClose           ! Flag for numerical comparisons

      logical bNoMaxTemp       ! Logical flags indicating that minimums
      logical bNoMinTemp       ! are not in effect
      logical bNoMinFlow
      logical bNoMaxFlow

      real fClose_tolerance           ! Tolerance for numerical comparisons
      parameter (fClose_tolerance=0.1) 
C----------------------------------------------------------------------
C     Get parameters
C----------------------------------------------------------------------      

      fMinTemp = AData ( iIndex , 1 )
      fMaxTemp = AData ( iIndex , 2 )
      fMinFlow = AData ( iIndex , 3 )
      fMaxFlow = AData ( iIndex , 4 )

C----------------------------------------------------------------------
C     Get control data
C----------------------------------------------------------------------      
      fControlTemp = CData ( iIndex, 1 )  ! oC
      fControlFlow = CData ( iIndex, 2 )  ! Kg/s

C----------------------------------------------------------------------
C     Check that ctl signal is within limits 
C----------------------------------------------------------------------      
      call eclose ( fMinTemp, -99.0, fClose_tolerance, bNoMinTemp )
      call eclose ( fMaxTemp, -99.0, fClose_tolerance, bNoMaxTemp )
      call eclose ( fMinFlow, -99.0, fClose_tolerance, bNoMinFlow )
      call eclose ( fMaxFlow, -99.0, fClose_tolerance, bNoMaxFlow )

      if ( fControlTemp .lt. fMinTemp .and. .not. bNoMinTemp )
     &         fControlTemp = fMinTemp

      if ( fControlTemp .gt. fMaxTemp .and. .not. bNoMaxTemp )
     &         fControlTemp = fMaxTemp

      if ( fControlFlow .lt. fMinFlow .and. .not. bNoMinFlow )
     &         fControlFlow = fMinFlow

      if ( fControlFlow .gt. fMaxFlow .and. .not. bNoMaxFlow )
     &         fControlFlow = fMaxFlow


C----------------------------------------------------------------------
C     Set matrix coefficients
C----------------------------------------------------------------------
      if ( iPlant_matrix .eq. iProp1stFlow ) then
         fCoefficients (1) = 1.0
         fCoefficients (2) = fControlFlow
      elseif ( iPlant_matrix .eq. iProp2ndFlow ) then
         fCoefficients (1) = 1.0
         fCoefficients (2) = 0.0
      elseif ( iPlant_matrix .eq. iPropH2Flow )  then
         fCoefficients (1) = 1.0
         fCoefficients (2) = 0.0
      elseif ( iPlant_matrix .eq. iPropTemp )  then
         fCoefficients (1) = 1.0
         fCoefficients (2) = fControlTemp
      endif

      return
      end

 
