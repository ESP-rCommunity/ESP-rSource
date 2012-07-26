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

!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
! Purpose: This provides dummy routines for all the subroutines in MODULE DLL_Functions
!          (which are only required by bps for co-simulations with TRNSYS)
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************


      MODULE DLL_Functions
      USE COSIMDATATYPES
      IMPLICIT NONE

      INTEGER, PARAMETER :: HCC = 1
      INTEGER, PARAMETER :: ACC = 2
      SAVE
      LOGICAL :: COSIM = .false.

      CONTAINS
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      Subroutine LoadDll
      END Subroutine LoadDll
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      subroutine PassDataToTRNSYS(ESPrData)
      TYPE(EsprTrnsysData), intent(in) :: ESPrData
      end subroutine PassDataToTRNSYS

!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      subroutine GetTRNSYSData(ESPrData)
      TYPE(EsprTrnsysData), intent(out) :: ESPrData
      end subroutine GetTRNSYSData
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      subroutine GetSystemConv(Conv)
      integer, intent(out) :: Conv
      end subroutine GetSystemConv
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      subroutine SetArraySizes(Sizes)
      integer, dimension(5), intent(out) :: Sizes
      end subroutine SetArraySizes
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      END MODULE DLL_Functions
