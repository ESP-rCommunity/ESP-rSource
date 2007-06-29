C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004--2006. Please Contact Ian
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
C================== mimo_laws.F ========================================
C
C Date:      June 28, 2007
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada, (2007)
C
C This file contains control laws associated with the MIMO control 
C facility. New control loops should be appended to this file.
C
C=======================================================================

C=======================================================================
C
C  Functions:
C
C    - MIMO_invoke_law: Simple function to invoke the specified 
C         control law.
C
C=======================================================================

C------------ MIMO_invoke_law ------------------------------------------
C
C     This function invokes a control loop's corresponding law
C
C     inputs: iLoop - index of control loop;
C
C     outputs: none;
C
C-----------------------------------------------------------------------
      subroutine MIMO_invoke_law ( iLoop )
      implicit none
#include "mimo_ctl.h"
C-----------------------------------------------------------------------
C     Passed arguements 
C-----------------------------------------------------------------------
      
      integer iLoop                   ! index of control loop 
      
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Invoke loop's corresponding law
C-----------------------------------------------------------------------
      if ( iLoopLaw( iLoop ) == 1 ) then 
      
        call MIMO_AIMS_Ctl_Law ( iLoop ) 
        
      else
        
        write ( cContext, '(A,I2,A)') 
     &       ' Invoking law for control loop # ', iLoop, '.'
        write ( cMsg, '(A)') 
     &       ' Fatal error: Unknown control type! '
        call UsrMsg ( cContext, cMsg, 'F' )
        
      endif 


      return 
      end 

C------------------- MIMO_AIMS_Ctl_Law ---------------------------------
C
C     This function applies the AIMS control law
C
C     Inputs: iLoop - index of current loop;
C     Outputs: None
C
C-----------------------------------------------------------------------
      subroutine MIMO_AIMS_Ctl_Law ( iLoop ) 
      implicit none 
#include "mimo_ctl.h"
C-----------------------------------------------------------------------
C     Passed arguements 
C-----------------------------------------------------------------------
      
      integer iLoop                   ! index of control loop 
      
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------

      !call MIMO_debug (" Invoked AIMS control ")

      return 
      end 