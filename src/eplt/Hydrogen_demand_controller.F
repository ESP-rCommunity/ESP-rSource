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
C
C======================================================================
C================= Hydrogen_demand_controller.F =======================
C
C Author:    Alex Ferguson
C Created:   March 22, 2005
C Copyright: Natural Resources Canada 2005
C
C This procedure is used to determine the amount of hydrogen required
C by one, or more components in a plant network and sets the
C appropriate control parameters on a hydrogen supply device,
C such as a electrolyzer or high-pressure storage tank.
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
      subroutine H2_demand_controller ( iterp )
      implicit none

#include "building.h"
#include "plant.h"
#include "Hydrogen_demand_controller.h"
#include "control.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iterp      ! # of iterations completed on this TS

C----------------------------------------------------------------------
C     ESP-r commons 
C----------------------------------------------------------------------

C.....More control parameters ?
      common/pclsol/icfp,idtypp,iperp
      integer icfp, idtypp, iperp

C.....Plant network & control data
      common/c9/npcomp,nci,cdata
      integer npcomp            ! number of plant components
      integer nci(mpcom)        ! number of controls / component
      real cdata(mpcom,mmiscd)  ! control data for each component

      
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iH2_supply_comp   ! Hydrogen supply component
      
      integer iCount            ! counter for plant component
      integer iLocation         ! counter for misc. data
      integer iComponent        ! index of component in network
      integer iData_Count       ! # of data items
      integer iIMP_comp_count   ! # of implicitly modelled components
                                !    requiring hydrogen
      integer iEXP_comp_count   ! # of explicitly modelled components
                                !    requiring hydrogen                                                                   

      integer iIMP_comp_index (MPCom) ! indicies of implicitly modelled
                                      !    components requiring
                                      !    hydrogen

      integer iEXP_comp_index (MPCom) ! indicies of explicitly modelled
                                      !    components requiring
                                      !    hydrogen

      real fH2_total_demand_EXP ! total explicit demand (kg/s)
      real fH2_total_demand_IMP ! total implicit demand (kg/s)


      
      
C----------------------------------------------------------------------
C     Determine number of data items 
C----------------------------------------------------------------------


C.....Total number of data       
      iData_Count = pmiscd( icfp, idtypp, iperp, 1)
      
C.....# of data associated with implicit components      
      iEXP_comp_count = pmiscd(icfp,idtypp, iperp, 2)
      
C.....# of data associated with implicit components 
      iIMP_comp_count = pmiscd(icfp,idtypp,iperp,iEXP_comp_count + 3 )


      
C.....Error trap here!


C----------------------------------------------------------------------
C     Determine actuated component index
C----------------------------------------------------------------------
      iH2_supply_comp = ipan(icfp,2)
      
C----------------------------------------------------------------------
C     Collect indicies of explicit components
C----------------------------------------------------------------------
      if ( iEXP_comp_count .gt. 0 ) then
         do iCount = 1, iEXP_comp_count

C...........Explicit components start at array location # 3           
            iLocation = iCount + 2

            iEXP_comp_index ( iCount ) =
     &           pmiscd(icfp,idtypp, iperp, iLocation )

         enddo
      endif 

C----------------------------------------------------------------------
C     Collect indicies of implicit components
C----------------------------------------------------------------------
      if ( iIMP_comp_count .gt. 0 ) then 
         
         do iCount = 1, iIMP_comp_count
         
C...........Explicit components start at array location
C...........# 2 + iEXP_comp_count + 2

            iLocation = 2 + iEXP_comp_count + 1 + iCount

            iIMP_comp_index ( iCount ) =
     &           pmiscd(icfp,idtypp, iperp, iLocation) 

         enddo

      endif 
      
C----------------------------------------------------------------------
C     Initialize variables
C----------------------------------------------------------------------
      fH2_total_demand_EXP = 0.0
      fH2_total_demand_IMP = 0.0

C----------------------------------------------------------------------
C     Collect explicit hydrogen flow demand 
C----------------------------------------------------------------------
      if ( iEXP_comp_count .gt. 0 ) then
      
         do iCount = 1, iEXP_comp_count

            iComponent = iEXP_comp_index ( iCount )
            
            fH2_total_demand_EXP = fH2_total_demand_EXP
     &            + fH2_plt_EXP_demand ( iComponent )

         enddo 

      endif 
         
C----------------------------------------------------------------------
C     Collect implicit hydrogen flow demand
C----------------------------------------------------------------------
      if ( iIMP_comp_count .gt. 0 ) then
      
         do iCount = 1, iIMP_comp_count

            iComponent = iIMP_comp_index ( iCount )
         
            fH2_total_demand_IMP = fH2_total_demand_IMP
     &            + fH2_plt_IMP_demand ( iComponent )

         enddo

      endif 
         
C----------------------------------------------------------------------
C     Set control variables for hydrogen supply device      
C----------------------------------------------------------------------

C.....Flow through explicit connections
      CDATA ( iH2_supply_comp, 1 ) = fH2_total_demand_EXP   ! (kg/s)

C.....Flow through implicit connections       
      CDATA ( iH2_supply_comp, 2 ) = fH2_total_demand_IMP   ! (kg/s)


      return
      end





