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

C mech_vent_control is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine mech_vent_control ( dummy_1, dummy_2)
      implicit none
      integer dummy_1 
      real dummy_2 
 
      return
      end

C h3kstore_fndtn_losses is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine h3kstore_fndtn_losses ( dummy_1, dummy_2, dummy_3)
      implicit none
      integer dummy_1, dummy_2 
      real dummy_3 
 
      return
      end

C  h3k_transport_electric_data is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine h3k_transport_electric_data 
      implicit none
 
      return
      end

C zero_hvac_var is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine zero_hvac_var 
      implicit none
 
      return
      end

C aim2_control is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine aim2_control ( dummy_1, dummy_2)
      implicit none
      integer dummy_1 
      real dummy_2 
 
      return
      end

C powoc_calc is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine powoc_calc ( dummy_1)
      implicit none
      integer dummy_1 
 
      return
      end

C elec_net_load_calc is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      real function elec_net_load_calc (dummy_1)
      implicit none
      integer dummy_1 
      elec_net_load_calc = 0.0
      return
      end

C hvacinput is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine hvacinput(dummy_1)
      implicit none
      character dummy_1*72
 
      return
      end

C arewindowsopen is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      logical function arewindowsopen ()
      implicit none
      arewindowsopen = .false.
      return
      end

C aim2_readin is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      subroutine aim2_readin
      implicit none
 
      return
      end

C shtfld is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      real function shtfld (iDummy_1, fDummy2)
      implicit none
      integer iDummy_1
      real fDummy2
 
      shtfld = 1.0

      return
      end

C RESH2_Ctl_Read is a dummy routine used to permit compilation of
C ESP-r binaries without CETC files
      subroutine RESH2_Ctl_Read
      implicit none
 
      return
      end

C RESH2_Ctl is a dummy routine used to permit compilation of
C ESP-r binaries without CETC files
      subroutine RESH2_Ctl
      implicit none

      return
      end

C fPltZone_Convective_Gain is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      real function fPltZone_Convective_Gain (iDummy_1, iDummy_2)
      implicit none
      integer iDummy_1, iDummy_2

      fPltZone_Convective_Gain = 0.0 

      return
      end

C fPltZone_Radiant_Gain is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      real function fPltZone_Radiant_Gain (iDummy_1, iDummy_2)
      implicit none
      integer iDummy_1, iDummy_2

 
      fPltZone_Radiant_Gain = 0.0

      return
      end
      
C fSurface_radiant_gain is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      real function fSurface_radiant_gain( iDummy_1, iDummy_2,iDummy_3)
      implicit none
      integer iDummy_1, iDummy_2, iDummy_3

 
      fSurface_radiant_gain = 0.0

      return
      end
      
C fNode_embedded_gain is a dummy routine used to permit compliation of
C ESP-r binaries without CETC files
      real function fNode_embedded_gain
     &             ( iDummy_1, iDummy_2,iDummy_3, iDummy_4)
      implicit none
      integer iDummy_1, iDummy_2, iDummy_3, iDummy_4

 
      fNode_embedded_gain = 0.0

      return
      end

