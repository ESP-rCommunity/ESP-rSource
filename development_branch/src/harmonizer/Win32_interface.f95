! This file is part of the ESP-r system.
! Copyright Energy Systems Research Unit, University of
! Strathclyde, Glasgow Scotland, 2001.

! ESP-r and the harmonizer are free software.  You can redistribute
! them and/or modify them under the terms of the GNU General Public
! License as published by the Free Software Foundation
! (version 2 orlater).

! ESP-r and the harmonizer are distributed in the hope that they will
! be useful but WITHOUT ANY WARRANTY; without even the implied warranty
! of MERCHANTABILITY or FITNESS FOR A PARTICULAR
! PURPOSE. See the GNU General Public License for more  details.

! You should have received a copy of the GNU General Public
! License along with ESP-r. If not, write to the Free
! Software Foundation, Inc., 59 Temple Place, Suite 330,
! Boston, MA 02111-1307 USA.

! This file  Win32Interface.f95 contains the module Win32Interface which provides the interface required to use
! the Windows API functions.  The interface block specifies all of the interface characteristics of an external
! procedure, and the compiler uses the information in the interface block to perform its consistency checks.
! This module is used in conjunction with launch_dll.f95 to load the harmonizer.dll and run a co-simulation.
!
! FindFirstFile: Searches a directory for a file or subdirectory with a name that matches a specific name
! LoadLibrary : Loads the specified module into the address space of the calling process. The specified module
!               may cause other modules to be loaded.
! GetLastError : Retrieves the calling thread's last-error code value. The last-error code is maintained on a
!                per-thread basis. Multiple threads do not overwrite each other's last-error code.
! GetProcAddress : Retrieves the address of an exported function or variable from the specified dynamic-link library (DLL).

!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
MODULE Win32Interface_Module
  interface
    function FindFirstFile(lpFileName, WFD) bind(C,name='FindFirstFileA')
      use ISO_C_BINDING
      implicit none
!GCC$ ATTRIBUTES STDCALL :: FindFirstFile
      integer(C_INTPTR_T) :: FindFirstFile
      character(kind=C_CHAR) :: lpFileName(*)
      type(C_PTR)  :: WFD
    end function FindFirstFile

    function LoadLibrary(lpFileName) bind(C,name='LoadLibraryA')
      use ISO_C_BINDING
      implicit none
!GCC$ ATTRIBUTES STDCALL :: LoadLibrary
      integer(C_INTPTR_T) :: LoadLibrary
      character(kind=C_CHAR) :: lpFileName(*)
    end function LoadLibrary

    function GetLastError() bind(C,name='GetLastError')
      use ISO_C_BINDING
      implicit none
!GCC$ ATTRIBUTES STDCALL :: GetLastError
      integer(C_LONG) :: GetLastError
    end function GetLastError

    function GetProcAddress(hModule, lpProcName) bind(C,name='GetProcAddress')
      use ISO_C_BINDING
      implicit none
!GCC$ ATTRIBUTES STDCALL :: GetProcAddress
      type(C_FUNPTR) :: GetProcAddress
      integer(C_INTPTR_T), value :: hModule
      character(kind=C_CHAR) :: lpProcName(*)
    end function GetProcAddress
  end interface
END MODULE Win32Interface_Module
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
