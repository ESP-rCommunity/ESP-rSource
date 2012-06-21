C This file is part of the ESP-r system.
C Copyright Carleton University 2011-2012.
C Please Contact Ian Beausoliel-Morrison for details
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

C************************************************************************************
C This file contains a number of functions that are called by the Harmonizer
C when co-simulation with TRNSYS is active.  These functions report on ESP-r's
C simulation period and time-step.  This information is used by the Harmonizer to
C ensure that ESP-r and TRNSYS are simulating the same time period and at the same
C time-step.
C************************************************************************************

      integer function GetSimulationStart()
      IMPLICIT NONE
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer :: isd1,ism1,isd2,ism2,isds,isdf,ntstep
      GetSimulationStart = isds
      end function GetSimulationStart

      
      integer function GetSimulationEnd()
      IMPLICIT NONE
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer :: isd1,ism1,isd2,ism2,isds,isdf,ntstep
      GetSimulationEnd = isdf
      end function GetSimulationEnd

            
      integer function GetTimeSteps()
      IMPLICIT NONE
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer :: isd1,ism1,isd2,ism2,isds,isdf,ntstep
      GetTimeSteps = ntstep
      end function GetTimeSteps

      
      integer function GetStartup()
      IMPLICIT NONE
      COMMON/PREC7/ITCNST
      integer :: ITCNST
      GetStartup = ITCNST
      end function GetStartup
