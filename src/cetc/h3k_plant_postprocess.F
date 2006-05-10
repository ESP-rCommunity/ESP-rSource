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
C
C********************************************************************************************
C********************************************************************************************
C********************************************************************************************
C********************************************************************************************
C
C h3k_plant_postprocess.F
C
C This file contains miscellanous routines that can be called to perform additional operations
C for plant component models after the plant domain matrix solution has converged. Post-
C processing routines for CETC plant component models are typically stored within the file 
C containing that model (ie FC_components.F, water_tanks.F). Since ESRU plant component model
C routines are lumped together into common files (pcomp1.F->pcomp4.F, pcomps.F & pcomps1.F)
C this additional file was created by CETC to store post-processing routines authored by CETC
C for ESRU plant component models.
C
C Included:
C
C   - H3K_HeatingCoil_PostPro - Post-processor for ESRU heating coil
C
C 
C
C
C
C================================ H3K_HeatingCoil_PostPro  ====================================
C
C
C
C Created by: Alex Ferguson
C Created on: July 5, 2004
C Copyright: CETC
C ________
C ABSTRACT: 
C 
C This routine calculates interesting data for ESRU's 3-node heating coil (db code 41)
C using the values of the component's state variables after the plant domain matrix solution 
C has converged 
C
C Inputs:
C
C   - iComponentIndex:  Index of component in plant network
C
C Outputs:
C
C==============================================================================================

      subroutine H3K_HeatingCoil_PostPro(iComponentIndex)
      
      implicit none

#include"plant.h"

C.....ESP-r commons
C (See documentation elsewhere)
      common/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      integer NPCDAT, IPOFS1, IPOFS2

      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15, pcname
      
      common/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      real CSVF, CSVP

C CONVAR(MPCON,MCONVR) ! state variables for connections
C ICONTP(MPCON)        ! connection types
C ICONDX(MPCOM,MNODEC,MPCONC) ! index of connections
      common/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &     ICONDX(MPCOM,MNODEC,MPCONC)
      integer icondx, icontp
      real convar
      integer lnblnk


C.....Passed variables
      integer iComponentIndex   ! index of component in plant network

C.....Node syntax
      integer iNode1, iNode2, iNode3 ! indicies of component nodes

C.....Connection syntax
      integer iWater_Connect    ! index of water connection
      integer iAir_Connect      ! index of air connection

C.....Named constants
      integer iProp_temp        ! named constant for temperature properity 
      integer iProp_1st_flow    ! named constant for first phase flow properity
      integer iProp_2nd_flow    ! named constant for second phase flow properity      
      parameter ( iProp_temp     = 1,
     &            iProp_1st_flow = 2, 
     &            iProp_2nd_flow = 3)
      logical bNo_Air_Flow      ! flag for close-to-zero comparisons
      logical bNo_Water_Flow    ! flag for close-to-zero comparisons
      real fSmall               ! tolerance of  close-to-zero comparisons
      parameter (fSmall = 1E-10)
           

C.....State variables / Temperatures
      real fT_air_in            ! Air temperature at inlet (oC)
      real fT_air_out           ! Air temperature at outlet (oC)
      real fT_water_in          ! Water temperature at inlet (oC)
      real fT_water_out         ! Water temperature at inlet (oC)
      
C.....State variables / Mass flow 
      real fMdot_dry_air        ! Dry air flow (kg/s)
      real fMdot_vapor          ! vapor flow (kg/s)
      real fMdot_water          ! water flwo (kg/s)

C.....Misc. Calculated data
      real fHumidity_ratio      ! Air side humidity ratio
      real fHeat_Xfer_to_air    ! Air side heat transfer (W)
      real fHeat_Xfer_to_water  ! water side heat transfer (W)     
      real fMCp_water_in        ! water heat capacity at inlet (W/K)
      real fMCp_water_out       ! water heat capacity at outlet (W/K)

      real fMCp_air             ! average dry air heat capacity (W/K)
      real fMCp_vapor           ! average vapor heat capacity (W/K)

C.....Reporting object handles
      character*128 hold_name  ! holding variable 
      character*128 root_name   ! component base name  
      character*128 H3K_rep_NAME ! XML leaf name


C.....References
      real h3k_connect_property ! function returning the properties of a connection
      real shtfld               ! function returning specific heat capacity of a fluid

C.....Get node #'s NPCDAT miscellaneous array
      iNode1 = NPCDAT(iComponentIndex,9)
      iNode2 = NPCDAT(iComponentIndex,9) + 1
      iNode3 = NPCDAT(iComponentIndex,9) + 2

C.....Get connection #'s from ICONDX array 
      iAir_Connect   = ICONDX(iComponentIndex,2,1) ! <- First connection to node 2
      iWater_Connect = ICONDX(iComponentIndex,3,1) ! <- First connection to node 3

C.....Inlet connection temperatures (get connection temperatures 
C.....using function h3k_connect_property:
C.....
C.....   Property value =  H3k_connect_property (
C.....                        Connection index # , 
C.....                        Requested Property  
C.....                     )

      fT_air_in    = h3k_connect_property(iAir_connect,iProp_temp)    ! oC
      fT_water_in  = h3k_connect_property(iWater_connect,iProp_temp)  ! oC

C.....outlet connection temperatatures (get node temperatures)
      fT_air_out   = CSVF(iNode2,iProp_temp) ! (oC)
      fT_water_out = CSVF(iNode3,iProp_temp) ! (oC)



C.....Get mass flow rates - 
      fMdot_dry_air = CSVF(iNode2,iProp_1st_flow) ! kg/s
      fMdot_vapor   = CSVF(iNode2,iProp_2nd_flow) ! kg/s
      fMdot_water   = CSVF(iNode3,iProp_1st_flow) ! kg/s 


C.....Calculate humidity ratio:
C.....
C.....                      Vapor Mass flow 
C.....   Humidity Ratio =  -----------------
C.....                     Dry Air Mass flow
C.....
C.....   BUT make sure  Dry Air Mass flow is not zero!

      call eclose ( fMdot_dry_air , 0., fSmall, bNo_Air_Flow )
      if ( .not. bNo_Air_Flow ) then
         fHumidity_ratio =  fMdot_vapor / fMdot_dry_air 
      else 
         fHumidity_ratio = 0.
      end if


C.....Calculate heat transfered to air: use SHTFLD function:
C.....
C.....  Heat capacity =   mass flow rate 
C.....                  * SHTFLD(fluid type, temperature) (W/K)
C.....  
C.....  fluid type = 1 for dry air, =2 for vapor.
C.....
C.....
C..... NOTE: This analyis ignores latent effects associated
C.....       with condensation and will produce erroneous 
C.....       results if condensation occurs.
C.....

         fMCp_air    = fMdot_dry_air 
     &        *  shtfld( 1, (fT_air_in + fT_air_out) / 2.0  )
         fMCp_vapor  = fMdot_vapor 
     &        *  shtfld( 2, (fT_air_in + fT_air_out) / 2.0  )

      if ( .not.  bNo_Air_Flow ) then 
         fHeat_Xfer_to_air =
     &         ( fMCp_air + fMCp_vapor ) * (  fT_air_out -  fT_air_in )
      else 
         fHeat_Xfer_to_air = 0.
      endif

C.....Calculate heat transferred to water: determine heat capacity 
C.....of water at inlet and outlet temperatures with function SHTFLD:
C.....
C.....  Spec. Heat Cap. = SHTFLD ( fluid type, temperature)  (J/kgK)
C.....
C.....  (use fluid type = 3) for water.
   
      call eclose ( fMdot_water , 0., fSmall, bNo_Water_Flow )
      if ( .not.  bNo_Water_Flow ) then
         fMCp_water_in  = fMdot_water * shtfld( 3, fT_water_in  ) ! (W/K)
         fMCp_water_out = fMdot_water * shtfld( 3, fT_water_out ) ! (W/K)

         fHeat_Xfer_to_water = ( fMCp_water_in +  fMCp_water_out ) / 2.0
     &        * ( fT_water_out -  fT_water_in )
      else

         fHeat_Xfer_to_water = 0.0

      endif

C.....H3K_reports: transport data to reporting object

C.....Get component name
      hold_name = pcname(iComponentIndex) 
C.....Format string as: 'plant/NAME/misc_data'

      WRITE(root_name,'(A,A,A)') 
     &     'plant/',
     &     hold_name(1:lnblnk(hold_name)),
     &     '/misc_data'
   


C.....Heat transfer to air
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/heat_transfer_to_air'
C.....-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'units',
     &        '(W)'
     &        )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'WattsToGJ',
     &        ''
     &        )
C.......-> update data 
         call REP_REPORT(
     &        fHeat_Xfer_to_air,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )


C.....Air side humidity ratio
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/air_humidity_ratio'
C.....-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'units',
     &        '(kg/kg)'
     &        )
C.......-> update data 
         call REP_REPORT(
     &        fHumidity_Ratio,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )


C.....Heat transfer to water
C.....-> set name
         WRITE(H3K_rep_NAME,'(A,A)') 
     &        root_name(1:lnblnk(root_name)), '/heat_transfer_to_water'
C.....-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'units',
     &        '(W)'
     &        )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'WattsToGJ',
     &        ''
     &        )
C.......-> update data 
         call REP_REPORT(
     &        fHeat_Xfer_to_water,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )




    
      return
      end

     
