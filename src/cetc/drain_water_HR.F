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
C================== BC_data.F ======================================
C
C Date: March 18, 2010
C Author: Alex Ferguson
C Copyright: Natural Resources Canada
C
C This file contains procedures to model drain-water heat recovery 
C equipment.
C
C GENERAL ROUTINES:
C 
C   process_dwhr_inputs: Read a dwhr model definition
C
C----------------------------------------------------------------------
C
C------------------ process_dwhr_inputs -------------------------------
C
C  ABSTRACT:
C  
C  process_dwhr_inputs reads
C
C  Inputs
C
C    bAutoOK: flag indicating simulator is in silent running mode 
C
C  Outputs
C
C    bFatal_Error: flag indicating that error was encountered
C
C----------------------------------------------------------------------

      subroutine process_dwhr_inputs( bAutoOK, bFatal_Error )
      implicit none
      include "drain_water_HR.h"
      
      logical bFatal_Error
      logical bAutoOK
      
      return
      end 

