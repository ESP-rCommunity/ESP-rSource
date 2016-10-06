C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C This file contains the following routines:
C
C    PFS_TimeRow_Manipulate: manages present & future
C       time-row variables in the electrical domain.
C
C======================================================================

C---------------- PFS_TimeRow_Manipulate ----------------------------
C
C ENet_Time_Row_Manipulate provides rudamentary time-row transport
C for state variables in the electrical network domain.
C
C This subroutine peforms three basic operations:
C
C   - Stores electrical network variables for the current present
C     time-row
C
C   - Overwrites the data in the present time-row storage
C     arrays with future time-row data
C
C   - Restores the present time-row state varaibles to their
C     values at the start of the time step, and the future
C     time-row state variables to their most recent values
C     calcualted at the end of the last plant iteration
C     (effectively re-winding the plant to the point where
C     iteration was adjourned.)
C
C Inputs:
C
C   - iOperation: integer flag used to specify which operation
C     should be performed. Compared to named constants defined
C     below (iSaving = 1, iWinding = 2, iRewinding = 3).
C
C   + Various ESP-r plant domain common blocks defined below.
C----------------------------------------------------------------------
      subroutine PFS_TimeRow_Manipulate(iOperation)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "CETC_definitions.h"

C----------------------------------------------------------------------
C     Passed variables
C----------------------------------------------------------------------
      integer iOperation       ! type of operation to be performed


C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iComponent           ! Counter
      integer iData                ! Counter

C.....Present time-row storage variable.
      real fPow_Present_Store ( MPOWCOM, MEnet_Time_Row_MAX )
      save fPow_Present_Store

C----------------------------------------------------------------------
C     Loop through each component, and through each variable associated
C     with that component.
C----------------------------------------------------------------------

C.....for each component...
      do iComponent = 1, nPowCom

C........for each variable...
         do iData = 1, iPow_Time_Row_Count ( iComponent )

C----------------------------------------------------------------------
C           store / wind / rewind as necessary
C----------------------------------------------------------------------
            if ( iOperation .eq. iSaving ) then

C..............Store
               fPow_Present_Store ( iComponent, iData )
     &              = fPow_Time_Row_Present ( iComponent, iData )

            elseif ( iOperation .eq. iWinding ) then

C..............Wind
               fPow_Time_Row_Present ( iComponent, iData )
     &              = fPow_Time_Row_Future ( iComponent, iData )


            elseif ( iOperation .eq. iRewinding ) then

C..............Rewind
               fPow_Time_Row_Future ( iComponent, iData ) =
     &              fPow_Time_Row_Present ( iComponent, iData )

            else
C..............Can't happen; iOperation set programically.
               stop 'Enet_Time_Row_Manipulate(): fatal error'

            endif
         enddo
      enddo

      return
      end

