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

! This file launch_dll.f95 contains the module DLL_Module which contains the subroutines required to access
! functions in the harmonizer dll, together with the program launch_dlls which takes the harmonizer input
! file name from the command line and opens the file to read the location of the harmonizer dll.  It then
! loads the harmonizer dll and calls the function 'SetHarmonizerInput' (passing it the harmonizer input FileName)
! to initialize the harmonizer dll and then calls 'main' in the harmonizer dll to run the co-simulation.
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
! MODULE DLL_Module uses the Win32Interface_Module module to access the Windows API functions required to
! co-operate with other dlls.  Every call made to the harmonizer dll requires the use of cray pointers
! (which provide a C-like pointer in Fortran).
! Cray pointers use a pair of variables: an integer "pointer" that holds a memory address, and a "pointee"
! that is used to dereference the pointer i.e. pointer ( <pointer> , <pointee> )
! The pointer is an integer that is intended to hold a memory address.
! The pointee may be an array or scalar.

! Cray pointers are used to point to a function or a subroutine in the harmonizer dll, and the Windows API function
! SetProcAddress is used to call that subroutine in the harmonizer dll.
!
! LoadDll:  Uses Windows API functions LoadLibrary (to load the harmonizer dll into bps),
!           and GetLastError (to retrieve an error code, in the event of an error).  The handle of the
!           dll is hmodule, which is subsequently used in accessing subroutines within the dll.
! SetProcAddress: assigns FunAddress with a pointer to the subroutine address (in the harmonizer) called
!                 'FunctionName' in the harmonizer dll, using the windows API function GetProcAddress
! DLLCall: this subroutine uses FunctionName (set to 'main' in the calling code) to call the 'main' function
!          in the harmonizer dll.
! DLLCall_String: this subroutine uses FunctionName (set to 'SetHarmonizerInput' in the calling code) to call
!                 the 'SetHarmonizerInput' function in the harmonizer dll passing in the string 'String').
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
MODULE DLL_Module
USE Win32Interface_Module
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
! Declare everything to be private except the SUBROUTINEs for loading the dll and calling the dll FUNCTION
PUBLIC :: LoadDll,DLLCall,DLLCall_String
PUBLIC :: FunctionName,hmodule,FunAddress,maxlength
!---------------------------------------------------------------------------------
! Declare parameters
!---------------------------------------------------------------------------------

SAVE !ensures values do not change between sucessive invocations
LOGICAL :: DllLoaded                    !DLL sucessfully loaded flag
TYPE(C_FUNPTR) :: FunAddress            !Proceedure address of bps main SUBROUTINE
INTEGER(C_INTPTR_T) :: hmodule          !Handel on loaded library
INTEGER, parameter :: maxlength = 260   !In the Windows API the maximum length for a path isdefined as 260 characters.
CHARACTER(maxlength) :: FunctionName    !The name of the function to be called in the harmonizer dll

 CONTAINS
!**********************************************************************************************************
!LoadDll loads the harmonizer dll
  SUBROUTINE LoadDll(DLL)
    ! Declare calling parameters
	CHARACTER(maxlength), intent(in) :: DLL !Name of the dll

    ! Variable Declaration
    INTEGER :: libFile             !Handel on library file
    TYPE(C_PTR) :: WFD             !POINTER to WIN32_FIND_DATA structure that receives information about the found file
    INTEGER(C_LONG) :: error       !Error code

	!TRIM() removes the trailing blanks
    hmodule = LoadLibrary(TRIM(DLL)//achar(0)) !append null char for c functions
    if(hModule == 0) then
      error = GetLastError()
      write(*,*)  'Harmonizer error loading harmonizer.dll: ',error
    else
      write(*,*) 'Harmonizer success loading harmonizer.dll: ', hmodule
    end if
  END SUBROUTINE LoadDll
!**********************************************************************************************************
!SetProcAddress sets the proceedure address (of 'FunctionName')to be called.
!SetProcAddress has to be treated as a seperate SUBROUTINE because of the cray-POINTERs
!Otherwise the compiler will get confused between FuncAddress being both a TYPE(C_FUNPTR)
!and an INTEGER (as is required for the cray POINTER).
  FUNCTION SetProcAddress()

    ! Variable Declaration
    INTEGER :: SetProcAddress     !will return 0 if sucessfull,

    SetProcAddress = 0
    if (hmodule > 0) then
      ! Get a POINTER to the procedure address that we want to call
      FunAddress = GetProcAddress(hmodule,TRIM(FunctionName)//CHAR(0))
      if (.NOT.C_ASSOCIATED(FunAddress)) then
        SetProcAddress = GetLastError()
        write(*,*)'Harmonizer error with Procceedure address: ',SetProcAddress
      else
        write(*,*)'Harmonizer found procceedure address ',TRIM(FunctionName)
      end if
    else
      write(*,*)'Harmonizer dll not loaded. Could not set proceedure address.'
      SetProcAddress = 30
    end if
  END FUNCTION SetProcAddress
!**********************************************************************************************************
!DLLCall calls the proceedure set in SetProcAddress (FunctionName).
  SUBROUTINE DLLCall(ErrCode)
    ! Declare calling parameters
    INTEGER, INTENT(OUT)  :: ErrCode    !will return 0 if sucessfull,
    ! Variable Declaration
    ! Declare a POINTER/pointee structure - this allows us to later "call" a POINTER by its pointee (SUBROUTINE) name.
    ! This is not "pure Fortran" but apparently is implemented in CVF, IVF and GNU Fortran. Once you declare these POINTERs the command CALL TRNSYS(...) refers to the POINTER address trnsysAddress
    INTEGER :: FuncAddress          !INTEGER version of FunAddress (needed for cray POINTER)
    POINTER(FuncAddress ,FunctionName)      !POINTER(<POINTER>,<pointee>)thus TrnsysAddress will be pointing to the address of the TRNSYS routine in TRNDll

	!SetProcAddress sets the proceedure address (of 'FunctionName')to be called.
    ErrCode = SetProcAddress()
    if (ErrCode==0) then
      !This sets up the cray-POINTER to enable calling the FUNCTION by its address
      FuncAddress = transfer(FunAddress,1_C_INTPTR_T) !cast TYPE(C_FUNPTR) to INTEGER
      call FunctionName
    end if
  END SUBROUTINE DLLCall
!**********************************************************************************************************
!DLLCall_String calls the proceedure set in SetProcAddress (FunctionName) passing the string 'String'.
  SUBROUTINE DLLCall_String(ErrCode, String)
    IMPLICIT NONE
    ! Declare  calling parameter TYPEs
    INTEGER, INTENT(OUT)  :: ErrCode    !will return 0 if sucessfull,
    CHARACTER(maxlength), intent(in) :: String  !

    ! Variable Declaration
    ! Declare a POINTER/pointee structure - this allows us to later "call" a POINTER by its pointee (SUBROUTINE) name.
    ! This is not "pure Fortran" but apparently is implemented in CVF, IVF and GNU Fortran. Once you declare these POINTERs the command CALL TRNSYS(...) refers to the POINTER address trnsysAddress
    INTEGER :: funcAddress          !INTEGER version of FunAddress (needed for cray POINTER)
    POINTER(funcAddress ,FunctionName)      !POINTER(<POINTER>,<pointee>)thus TrnsysAddress will be pointing to the address of the TRNSYS routine in TRNDll
    !SetProcAddress sets the proceedure address (of 'FunctionName')to be called.
    ErrCode = SetProcAddress()
    if (ErrCode==0) then
      !This sets up the cray-POINTER to enable calling the FUNCTION by its address
      FuncAddress = transfer(FunAddress,1_C_INTPTR_T) !cast TYPE(C_FUNPTR) to INTEGER
      call FunctionName(TRIM(String)//achar(0))
    end if
  END SUBROUTINE DLLCall_String
!**********************************************************************************************************

END MODULE DLL_Module
!**********************************************************************************************************
!**********************************************************************************************************
!**********************************************************************************************************
PROGRAM launch_dlls

USE DLL_Module

IMPLICIT NONE
INTEGER :: CLargument_count
INTEGER :: i, Errcode, argc, startstring,ENDstring
CHARACTER(maxlength) :: FileName
INTEGER :: status ! i/o status
CHARACTER(maxlength) :: HarmonizerPath

! get the input file name (the second command line argument)
call get_command_argument(1, FileName)
!open the file to get the location of the harmonizer dll
!Open the file. and check for errors on open
OPEN(Unit=3,File=FileName, STATUS='OLD',ACTION='READ', IOSTAT=status)
if (status == 0) then
  !open was OK so read the second line which holds the path to the harmonizer
  do i=1,3
    read(3,'(a)')HarmonizerPath
  end do
  write(*,*)'HarmonizerPath: ',HarmonizerPath
else
  write(*,*)'Problem opening harmonizer input file'
end if
!load the harmonizer dll
call LoadDll(HarmonizerPath)
FunctionName ='SetHarmonizerInput'
!call 'SetHarmonizerInput' in the harmonizer dll (passing it the FileName)
call DLLCall_String(Errcode,FileName)
FunctionName ='main'
!call 'main' in the harmonizer dll
call DLLCall(Errcode)

if (ErrCode.NE.0) write(*,*)' There was a problem with the harmonizer:', Errcode

END PROGRAM launch_dlls
