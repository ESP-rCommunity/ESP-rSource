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

C This file harmonizer.F contains two modules DLL_Functions, containing all the subroutines required to
C communicate with the harmonizer and Win32Interface, which provides the interface required to use the
C Windows API functions.
C Both modules are for use in co-simulation with TRNSYS using the Harmonizer.
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************

! The module Win32Interface provides the interface required to use the Windows API functions
!
!The interface block specifies all of the interface characteristics of an external procedure, and the compiler
!uses the information in the interface block to perform its consistency checks.

! FindFirstFile: Searches a directory for a file or subdirectory with a name that matches a specific name
! LoadLibrary : Loads the specified module into the address space of the calling process. The specified module
!               may cause other modules to be loaded.
! GetLastError : Retrieves the calling thread's last-error code value. The last-error code is maintained on a
!                per-thread basis. Multiple threads do not overwrite each other's last-error code.
! GetProcAddress : Retrieves the address of an exported function or variable from the specified dynamic-link library (DLL).


!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      MODULE Win32Interface
      interface
      function FindFirstFile(lpFileName,WFD)
     &  bind(C,name='FindFirstFileA')
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

      function GetProcAddress(hModule, lpProcName)
     &   bind(C,name='GetProcAddress')
        use ISO_C_BINDING
        implicit none
!GCC$ ATTRIBUTES STDCALL :: GetProcAddress
        type(C_FUNPTR) :: GetProcAddress
        integer(C_INTPTR_T), value :: hModule
        character(kind=C_CHAR) :: lpProcName(*)
      end function GetProcAddress
      end interface
      END MODULE Win32Interface
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************

! Purpose: DLL_Functions module contains all the subroutines required to communicate with the harmonizer.
! Every call made to the harmonizer requires the use of cray pointers (which provide a C-like pointer in Fortran).
! Cray pointers use a pair of variables: an integer "pointer" that holds a memory address, and a "pointee" that is
! used to dereference the pointer i.e. pointer ( <pointer> , <pointee> )
! The pointer is an integer that is intended to hold a memory address.
! The pointee may be an array or scalar.

! Cray pointers are used to point to a function or a subroutine in the harmonizer, and the Windows API function
! SetProcAddress is used to call that subroutine in the harmonizer.
!
! LoadDll:  Uses Windows API functions LoadLibrary (to load the harmonizer dll into bps),
!           and GetLastError (to retrieve an error code, in the event of an error).  The handle of the
!           dll is hmodule, which is subsequently used in accessing subroutines within the dll.
! SetProcAddress: assigns FunAddress with a pointer to the subroutine address (in the harmonizer) called
!                 'FunctionName' in the harmonizer dll, using the windows API function GetProcAddress
!**********************************************************************************************************
! PassDataToTRNSYS: this subroutine sets FunctionName to 'PASSDATATOTRNSYS', and then calls DLLCall_PassDataToTRNSYS to use
!                   that FunctionName in a call to the harmonizer dll.  The two subroutines have to be seperate
!                   because of the craypointers, otherwise the compiler will complain that the VARIABLE attribute
!                   of FunctionName conflicts with the PROCEDURE attribute.  PassDataToTRNSYS sets FunctionName
!                   to 'PASSDATATOTRNSYS', and DLLCall_PassDataToTRNSYS calls FunctionName.
! DLLCall_PassDataToTRNSYS: this subroutine calls the subroutine FunctionName in the harmonizer dll.
!                   Cray poiners enable calling a subroutine by it`s name, since it`s name is pointing to
!                   it`s address.  This subroutine depends on a prior call to LoadDll.
!**********************************************************************************************************
! GetTRNSYSData: this subroutine sets FunctionName to 'GETTRNSYSDATA', and then calls DLLCall_GetTRNSYSData to use that
!                   FunctionName in a call to the harmonizer dll.  The two subroutines have to be seperate
!                   because of the craypointers, otherwise the compiler will complain that the VARIABLE attribute
!                   of FunctionName conflicts with the PROCEDURE attribute.  GetTRNSYSData sets FunctionName
!                   to 'GETTRNSYSDATA' and DLLCall_GetTRNSYSData calls FunctionName.
! DLLCall_GetTRNSYSData: this subroutine calls the subroutine FunctionName in the harmonizer dll.
!                   Cray poiners enable calling a subroutine by it`s name, since it`s name is pointing to
!                   it`s address.  This subroutine depends on a prior call to LoadDll.
!**********************************************************************************************************
! SetArraySizes: this subroutine sets FunctionName to 'SETARRAYSIZES' and then calls DLLCall_SetArraySizes to use that
!                   FunctionName in a call to the harmonizer dll.  The two subroutines have to be seperate
!                   because of the craypointers, otherwise the compiler will complain that the VARIABLE attribute
!                   of FunctionName conflicts with the PROCEDURE attribute.  SetArraySizes sets FunctionName to
!                   'SETARRAYSIZES' and DLLCall_SetArraySizes calls FunctionName.  Together they tell the harmonizer
!                   the size of the arrays being passed between ESP-r and TRNSYS so that it can minimize the amount of data copied.
! DLLCall_SetArraySizes: this subroutine calls the subroutine FunctionName in the harmonizer dll.
!                   Cray poiners enable calling a subroutine by it`s name, since it`s name is pointing to
!                   it`s address.  This subroutine depends on a prior call to LoadDll.
!**********************************************************************************************************
! GetSystemConv: this subroutine sets FunctionName to 'GETSYSTEMCONV' and then calls DLLCall_GetSystemConv to use that
!                   FunctionName in a call to the harmonizer dll.  The two subroutines have to be seperate
!                   because of the craypointers, otherwise the compiler will complain that the VARIABLE attribute
!                   of FunctionName conflicts with the PROCEDURE attribute.  GetSystemConv sets FunctionName
!                   to 'GETSYSTEMCONV' and DLLCall_GetSystemConv calls FunctionName.  Together they ask the harmonizer
!                   to check if the whole system (i.e. ESP-r and TRNSYS) has converged (simulataneously).
! DLLCall_GetSystemConv: this subroutine calls the subroutine FunctionName in the harmonizer dll.
!                   Cray poiners enable calling a subroutine by it`s name, since it`s name is pointing to
!                   it`s address.  This subroutine depends on a prior call to LoadDll.

!**********************************************************************************************************
!**********************************************************************************************************
**********************************************************************************************************
      MODULE DLL_Functions
      USE Win32Interface
      USE ISO_C_BINDING
      USE COSIMDATATYPES
      IMPLICIT NONE

      PRIVATE
! Declare everything to be private except the subroutines for loading the dll and calling the dll function
      PUBLIC::LoadDll,GetTRNSYSData,PassDataToTRNSYS,GetSystemConv,
     &        SetArraySizes
      PUBLIC::FunctionName,hmodule,FunAddress,TimeStepItteration
! hmodule: The handle of the loaded harmonizer dll
! FunctionName: The name of the subroutine to be called in the loaded dll
! FunAddress: The address of the subroutine 'FunctionName'
!---------------------------------------------------------------------------------
! Declare parameters
!---------------------------------------------------------------------------------

      SAVE !ensures values do not change between sucessive invocations
      integer TimeStepItteration     !Tempory count of itterations
      integer(C_INTPTR_T) :: hmodule !Handel on loaded library
      type(C_FUNPTR) :: FunAddress   !Address harmonizer subroutine
      integer, parameter :: maxlength = 300
      character(maxlength) :: FunctionName !Name of harmonizer subroutine

      CONTAINS
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
! LoadDll:  Uses Windows API functions LoadLibrary (to load the harmonizer dll into bps),
!           and GetLastError (to retrieve an error code, in the event of an error).  The handle of the
!           dll is hmodule, which is subsequently used in accessing subroutines within the dll.
      Subroutine LoadDll
      ! Variable Declaration
      integer(C_LONG) :: error       !Error code

      ! Load in the dll
      hmodule = LoadLibrary('harmonizer.dll'//achar(0)) !append null char for c functions
      ! If the dll couldn't be loaded, get the error code
      if(hModule == 0) then
        error = GetLastError()! GetLastError is a windows API function
        write(*,*)  'error loading harmonizer.dll = ',error
      end if
      END Subroutine LoadDll
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
! SetProcAddress assigns FunAddress with a pointer to the subroutine address called 'FunctionName'
! in the harmonizer dll, using the windows API function GetProcAddress
      Function SetProcAddress()
      ! Variable Declaration
      integer :: SetProcAddress     !will return 0 if sucessfull,

      SetProcAddress = 0
      ! Check that the harmonizer dll has been loaded.
      if (hmodule > 0) then
      ! GetProcAddress is a windows API function used to call the subroutine 'FUNCTIONNAME' in the harmonizer.
      ! append '\n' to FunctionName as the harmonizer is C++
        FunAddress = GetProcAddress(hmodule,TRIM(FunctionName)//CHAR(0))
      !check that the pointer FunAddress is pointing to a subroutine, if not return the error code.
      if(.NOT.C_ASSOCIATED(FunAddress)) SetProcAddress = GetLastError() ! GetLastError is a windows API function
      else
        write(*,*)'harmonizer dll not loaded'
        SetProcAddress = 30
      end if
      End Function SetProcAddress


!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
! PassDataToTRNSYS: this subroutine sets FunctionName (to 'PASSDATATOTRNSYS'), and then calls DLLCall_PassDataToTRNSYS
! to use that FunctionName in a call to the harmonizer dll.
! The two subroutines have to be seperate because of the craypointers, otherwise the compiler will complain
! that the VARIABLE attribute of FunctionName conflicts with the PROCEDURE attribute.
! PassDataToTRNSYS sets FunctionName and DLLCall_PassDataToTRNSYS calls FunctionName.
      subroutine PassDataToTRNSYS(ESPrData)
      implicit none
      ! Declare calling parameters
      TYPE(EsprTrnsysData), intent(in) :: ESPrData !Data passing from ESP-r
      ! Declare local parameters
      integer :: ErrCode, i
      ! set FunctionName to the subroutine we want to call (in the harmonizer dll)
      FunctionName ='PASSDATATOTRNSYS'
      call DLLCall_PassDataToTRNSYS(ErrCode, ESPrData)
      if (ErrCode.NE.0) write(*,*)'Error in PassDataToTRNSYS:', Errcode
      end subroutine PassDataToTRNSYS
!**********************************************************************************************************
! DLLCall_PassDataToTRNSYS: this subroutine calls the subroutine FunctionName in the harmonizer dll.
! FunctionName is set to 'PASSDATATOTRNSYS' in subroutine PassDataToTRNSYS which is the subroutine calling this subroutine) .
! Cray poiners enable calling a subroutine by it's name, since it`s name is pointing to it's address.
! The two subroutines have to be seperate because of the craypointers, otherwise the compiler will complain
! that the VARIABLE attribute of FunctionName conflicts with the PROCEDURE attribute.
! PassDataToTRNSYS sets FunctionName and DLLCall_PassDataToTRNSYS calls FunctionName.
! This subroutine depends on a prior call to LoadDll.
      Subroutine DLLCall_PassDataToTRNSYS(ErrCode, ESPrData)
      ! Declare calling parameters
      integer, intent(out)  :: ErrCode    !will return 0 if sucessfull,
      TYPE(EsprTrnsysData), intent(in) :: ESPrData !Data passing from ESP-r
      ! Variable Declaration
      ! Declare a pointer/pointee structure - this allows us to later "call" a pointer by its pointee (subroutine) name.
      ! This is not "pure Fortran" but apparently is implemented in CVF, IVF and GNU Fortran. Once you declare these pointers the command CALL TRNSYS(...) refers to the pointer address trnsysAddress
      integer :: FuncAddress          !Integer version of FunAddress (needed for cray pointer)
      Pointer(FuncAddress ,FunctionName)      !pointer(<pointer>,<pointee>)thus TrnsysAddress will be pointing to the address of the TRNSYS routine in TRNDll

      ErrCode = SetProcAddress()
      if (ErrCode==0) then
        !This sets up the cray-pointer to enable calling the function by its address
        FuncAddress = transfer(FunAddress,1_C_INTPTR_T) !cast type(C_FUNPTR) to integer
        ! call the subroutine in the harmonizer dll
        call FunctionName(ESPrData)!, Done)
      else
        write(*,*)'Error in DLLCall_PassDataToTRNSYS:',ErrCode
      end if
      End Subroutine DLLCall_PassDataToTRNSYS
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************


! GetTRNSYSData: this subroutine sets FunctionName (to 'GETTRNSYSDATA'), and then calls DLLCall_GetTRNSYSData
! to use that FunctionName in a call to the harmonizer dll.
! The two subroutines have to be seperate because of the craypointers, otherwise the compiler will complain
! that the VARIABLE attribute of FunctionName conflicts with the PROCEDURE attribute.
! GetTRNSYSData sets FunctionName and DLLCall_GetTRNSYSData calls FunctionName.
      subroutine GetTRNSYSData(TrnsysData)
      implicit none
      ! Declare calling parameters
      TYPE(EsprTrnsysData), intent(out) :: TrnsysData  ! Data comming from TRNSYS
      ! Declare local parameters
      integer :: ErrCode,i
      ! set FunctionName to the subroutine we want to call (in the harmonizer dll)
      FunctionName ='GETTRNSYSDATA'
      call DLLCall_GetTRNSYSData(ErrCode, TrnsysData)
      if (ErrCode.NE.0) write(*,*)'Error in GetTRNSYSData:', Errcode
      end subroutine GetTRNSYSData
!**********************************************************************************************************
! DLLCall_GetTRNSYSData: this subroutine calls the subroutine FunctionName in the harmonizer dll.
! FunctionName is set to 'GETTRNSYSDATA' in subroutine GetTRNSYSData which is the subroutine calling this subroutine) .
! Cray poiners enable calling a subroutine by it's name, since it`s name is pointing to it's address.
! The two subroutines have to be seperate because of the craypointers, otherwise the compiler will complain
! that the VARIABLE attribute of FunctionName conflicts with the PROCEDURE attribute.
! GetTRNSYSData sets FunctionName and DLLCall_GetTRNSYSData calls FunctionName.
! This subroutine depends on a prior call to LoadDll.
      Subroutine DLLCall_GetTRNSYSData(ErrCode, TrnsysData)
! Declare calling parameters
      integer, intent(out)  :: ErrCode    !will return 0 if sucessfull,
      TYPE(EsprTrnsysData), intent(out) :: TrnsysData  ! Data comming from TRNSYS
! Variable Declaration
  ! Declare a pointer/pointee structure - this allows us to later "call" a pointer by its pointee (subroutine) name.
  ! This is not "pure Fortran" but apparently is implemented in CVF, IVF and GNU Fortran. Once you declare these pointers the command CALL TRNSYS(...) refers to the pointer address trnsysAddress
      integer :: funcAddress            !Integer version of FunAddress (needed for cray pointer)
      Pointer(funcAddress ,FunctionName)        !pointer(<pointer>,<pointee>)thus TrnsysAddress will be pointing to the address of the TRNSYS routine in TRNDll

      ErrCode = SetProcAddress()
      if (ErrCode==0) then
      !This sets up the cray-pointer to enable calling the function by its address
      FuncAddress = transfer(FunAddress,1_C_INTPTR_T) !cast type(C_FUNPTR) to integer
      ! call the subroutine in the harmonizer dll
      call FunctionName(TrnsysData)
      else
        write(*,*)'Error in DLLCall_GetTRNSYSData:', ErrCode
      end if
      End Subroutine DLLCall_GetTRNSYSData


!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      Subroutine GetSystemConv(Conv)
      implicit none
      ! Declare calling parameters
      integer, intent(out) :: Conv  ! TRNSYS converged flag
      ! Declare local parameters
      integer :: ErrCode,i
      ! set FunctionName to the subroutine we want to call (in the harmonizer dll)
      FunctionName ='GETSYSTEMCONV'
      call DLLCall_GetSystemConv(ErrCode, Conv)
      if (ErrCode.NE.0) write(*,*)'Error in GetSystemConv:', Errcode
      end subroutine GetSystemConv
!**********************************************************************************************************
! DLLCall_GetSystemConv: this subroutine calls the subroutine FunctionName in the harmonizer dll.
! FunctionName is set to 'GETTRNSYSCONV' in subroutine GetTRNSYSConv which is the subroutine calling this subroutine) .
! Cray poiners enable calling a subroutine by it's name, since it`s name is pointing to it's address.
! The two subroutines have to be seperate because of the craypointers, otherwise the compiler will complain
! that the VARIABLE attribute of FunctionName conflicts with the PROCEDURE attribute.
! GetTRNSYSConv sets FunctionName and DLLCall_GetTRNSYSConv calls FunctionName.
! This subroutine depends on a prior call to LoadDll.
      Subroutine DLLCall_GetSystemConv(ErrCode, Conv)
! Declare calling parameters
      integer, intent(out) :: ErrCode    !will return 0 if sucessfull,
      integer, intent(out) :: Conv  !Convergence flag from the harmonizer
! Variable Declaration
  ! Declare a pointer/pointee structure - this allows us to later "call" a pointer by its pointee (subroutine) name.
  ! This is not "pure Fortran" but apparently is implemented in CVF, IVF and GNU Fortran. Once you declare these pointers the command CALL TRNSYS(...) refers to the pointer address trnsysAddress
      integer :: funcAddress            !Integer version of FunAddress (needed for cray pointer)
      Pointer(funcAddress ,FunctionName)        !pointer(<pointer>,<pointee>)thus TrnsysAddress will be pointing to the address of the TRNSYS routine in TRNDll

      ErrCode = SetProcAddress()
      if (ErrCode==0) then
      !This sets up the cray-pointer to enable calling the function by its address
      FuncAddress = transfer(FunAddress,1_C_INTPTR_T) !cast type(C_FUNPTR) to integer
      ! call the subroutine in the harmonizer dll
      call FunctionName(Conv)
      else
        write(*,*)'Error in DLLCall_GetSystemConv:', ErrCode
      end if
      End Subroutine DLLCall_GetSystemConv
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
      Subroutine SetArraySizes(Sizes)
      implicit none
      ! Declare calling parameters
      integer, dimension(5), intent(out) :: Sizes  ! integer array of array sizes
      ! Declare local parameters
      integer :: ErrCode,i
      ! set FunctionName to the subroutine we want to call (in the harmonizer dll)
      FunctionName ='SETARRAYSIZES'
      call DLLCall_SetArraySizes(ErrCode, Sizes)
      if (ErrCode.NE.0) write(*,*)'Error in SetArraySizes:', Errcode
      end subroutine SetArraySizes
!**********************************************************************************************************
! DLLCall_SetArraySizes: this subroutine calls the subroutine FunctionName in the harmonizer dll.
! FunctionName is set to 'SETARRAYSIZES' in subroutine SetArraySizes which is the subroutine calling this subroutine) .
! Cray poiners enable calling a subroutine by it's name, since it`s name is pointing to it's address.
! The two subroutines have to be seperate because of the craypointers, otherwise the compiler will complain
! that the VARIABLE attribute of FunctionName conflicts with the PROCEDURE attribute.
! SetArraySizes sets FunctionName and DLLCall_SetArraySizes calls FunctionName.
! This subroutine depends on a prior call to LoadDll.
      Subroutine DLLCall_SetArraySizes(ErrCode, Sizes)
! Declare calling parameters
      integer, intent(out)  :: ErrCode    !will return 0 if sucessfull,
      integer, dimension(5), intent(out) :: Sizes  !Convergence flag from the harmonizer
! Variable Declaration
  ! Declare a pointer/pointee structure - this allows us to later "call" a pointer by its pointee (subroutine) name.
  ! This is not "pure Fortran" but apparently is implemented in CVF, IVF and GNU Fortran. Once you declare these pointers the command CALL TRNSYS(...) refers to the pointer address trnsysAddress
      integer :: funcAddress            !Integer version of FunAddress (needed for cray pointer)
      Pointer(funcAddress ,FunctionName)        !pointer(<pointer>,<pointee>)thus TrnsysAddress will be pointing to the address of the TRNSYS routine in TRNDll

      ErrCode = SetProcAddress()
      if (ErrCode==0) then
      !This sets up the cray-pointer to enable calling the function by its address
      FuncAddress = transfer(FunAddress,1_C_INTPTR_T) !cast type(C_FUNPTR) to integer
      ! call the subroutine in the harmonizer dll
      call FunctionName(Sizes)
      else
        write(*,*)'Error in DLLCall_SetArraySizes:', ErrCode
      end if
      End Subroutine DLLCall_SetArraySizes
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************

      END MODULE DLL_Functions















