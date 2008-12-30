
C This file is part of the ESP-r system.
C Copyright CanmetENERGY, Natural Resources Canada 
C 2008.

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

C This file contains the following routines:
C
C   bRTCEnabled: logical function returning true if run-time 
C     couplings are supported, and if any couplings are enabled.
C   
C   initializeRTC: Routine responsible for initializing 
C     child processess for run-time couplings.
C
C  
C=======================================================================

C===================== bRTCenabled =====================================
C
C Created by Alex Ferguson
C Created on Dec 30, 2008
C Copyright: Natural Resouces Canada 2008.
C
C SYNOPSYS: 
C Logical function returning true if RT coupling is enabled
C
C=======================================================================

      logical function bRTCenabled()
      implicit none 
               
      include "RTC_data.h"         
               
      return 
      end function bRTCenabled

C===================== initializeRTC ===================================
C
C Created by Alex Ferguson
C Created on Dec 30, 2008
C Copyright: Natural Resouces Canada 2008.
C
C SYNOPSYS: 
C
C=======================================================================

       subroutine initializeRTC()
       implicit none 
      
       include "RTC_data.h"
            
       return 
       end subroutine initializeRTC
