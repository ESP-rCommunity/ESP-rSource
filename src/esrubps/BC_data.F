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
C Date: June 28, 2005
C Author: Alex Ferguson
C Copyright: Natural Resources Canada
C
C This file contains procedures for managing user-specified
C boundary conditions.
C
C GENERAL ROUTINES:
C
C   process_BC_data_file: Read a boundry condition data definition
C      file, and write out a binary database containing the necessary
C      records.
C
C   get_BC_data_by_col: Collect boundry condition data for the current
C      timestep, and return the data corresponding to a given
C      column number.
C
C   get_BC_data_by_name: Collect boundry condition data for the current
C      timestep, and return the data corresponding to a given
C      column header name.
C
C   report_BC_data: transport boundry condition data to the H3K
C      xml reporting facility.
C
C   cleanup_BC_data: Delete temporary files and free I/O channels
C
C----------------------------------------------------------------------
C
C------------------ process_BC_data_file ------------------------------
C
C  ABSTRACT:
C
C  process_BC_data_file parses a boundary condition definition file, checks
C  it for validity, and writes a binary database containing the
C  relevant records.
C
C  Inputs
C
C    iASCII_num: file number to use for ascii file input
C    iBC_TmpFile: file number to use for binary file output
C    bAutoOK: flag indicating simulator is in silent running mode
C
C  Outputs
C
C    bFatal_Error: flag indicating that error was encountered
C
C----------------------------------------------------------------------

      subroutine process_BC_data_file(
     &    iBase_File_num, bAutoOK, bFatal_Error )
      implicit none
#include "building.h"
#include "bc_data.h"
#include "MultiYear_simulations.h"
#include "plant.h" !needed for offsite utilities definitions
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C External functions.
      integer lnblnk

C Passed arguements
      integer iBase_File_num              ! Basic I/O file number
      logical bAutoOK                     ! flag for silent-running mode
      logical bFatal_Error                ! fatal error flag

C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C.....flag indicating startup period spans new year
      common/sus/issny
      integer issny

C----------------------------------------------------------------------
C     local variables
C----------------------------------------------------------------------
      integer iASCII_num       ! Ascii file number

      integer iProfile_file_num ! hourly profiles temporary file #

      integer iFile_read_err   ! flag indicating that error was
                               ! encountered while reading from a file
      integer iFile_write_err  ! Flag indicating that error was encountered
                               ! while writing to a file


      character*248 cLnBuffer     ! Line buffer
      character*2500 cLnBuffer2500   ! Line buffer (2500 characters) 
      character*248 cLnWords(124) ! words on the line
      integer iWordCount          ! # of words in a string
      integer iDummy

      integer iProfile            ! Counters
      integer iRow                !
      integer iCol                !

      integer iMiscError          ! Miscellaneous error

      logical bParsing_Data       ! flag indicating that parser
                                  ! is in the '*BC_data' section of the
                                  ! file

C.....Data used for hourly profiles
      logical bProfile_err        ! Flag indicating that error has occured
                                  ! during processing of hourly load profile
                                  ! data .

      integer iEndRow             ! End row, if a period has been defined.

      integer iProfile_count      ! Profile counter

      real fSeconds_Lapsed        ! Time variables describing the time that
      real fHours_Lapsed          ! has lapsed from the beginning of the BC
      real fDays_Lapsed           ! definition period to the current row

      real fRow_Hour              ! Time variables describing the current row
      integer iRow_day            !
      integer iRow_Year           !

      real fProfile_Value         ! Value of profile at requested row


C.....Meta data flags
      logical bFrequency_defined  ! Flags indicating that the
      logical bPeriod_defined     ! frequency/period/version/header/units/data
      logical bPeriod_end_defined ! have been defined
      logical bVersion_defined    !
      logical bHeader_defined     !
      logical bUnits_defined      !
      logical bData_defined       !
      logical bProfiles_defined   !
      
C.....16-Oct-2011 BAL: Adding local fuel cost variables 
      logical bParsingElecFuelCosts
      logical bElecCosts_defined
      logical bParsingNGFuelCosts
      logical bNGCosts_defined
      logical bParsingOilFuelCosts
      logical bOilCosts_defined
      logical bParsingPropFuelCosts
      logical bPropCosts_defined
      logical bParsingWoodFuelCosts
      logical bWoodCosts_defined
      integer iMonth
      integer iBlock     
      
C.....Profile data:
      character*248 cProfile_Names(mBC_max_count)    ! names
      character*248 cProfile_Units(mBC_max_count)    ! units
      character*72  cProfile_Files(mBC_max_count)    ! files


C.....File flags
      logical bFile_Exists        ! Flag indicating a file exists.
      logical bFile_Open          ! Flag indicating a file is open

C.....Line buffers
      real fLnData(mBC_max_count) ! Boundry condition data

C.....Start and end of BC period
      integer iBC_start_DOM        ! Day of month (1->31)
      integer iBC_start_month      ! Month (1->12)
      integer iBC_start_year       ! Year
      integer iBC_start_DOY        ! day of year (1->365)

      integer iBC_end_DOM          ! Day of month (1->31)
      integer iBC_end_month        ! Month (1->12)
      integer iBC_end_year         ! Year
      integer iBC_End_DOY          ! day of year (1->365)

C.....Warning messages
      character*124 cMsg          ! message to be displayed to buffer
      character*124 cContext      ! contextual message

C.....Days of the month -
      integer iDays_of_month(12)
      data  iDays_of_month / 31, 28, 31, 30,
     &                       31, 30, 31, 31,
     &                       30, 31, 30, 31 /
      save iDays_of_month

C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      integer iEGetArrW        ! returns an array containing the
                               ! words found in a string.
      integer iEGetArrW_2500   ! returns an array containing the
                               ! words found in a string 2500 ch long.      
      real fRecover_lds_from_bin_file ! Recovers values from the
                                      ! binary profile file.
C---------------------------------------------------------------------
C     Initialize
C---------------------------------------------------------------------
      iASCII_num          = iBase_File_num + 1
      iBC_TmpFile         = iBase_File_num + 76
      iProfile_file_num   = iBase_File_num + 77

      bFrequency_defined  = .false.
      bPeriod_defined     = .false.
      bVersion_defined    = .false.
      bHeader_defined     = .false.
      bUnits_defined      = .false.
      bData_defined       = .false.
      bParsing_Data       = .false.
      bPeriod_end_defined = .false.
      bProfiles_defined   = .false.
      
C.....16-Oct-2011 BAL: Initializing fuel cost flags   
      bIncFuelCostCalcs          = .false.
      bTOUelecRates              = .false.
      bParsingElecFuelCosts      = .false.
      bElecCosts_defined         = .false. 
      bParsingNGFuelCosts        = .false.
      bNGCosts_defined           = .false.
      bParsingOilFuelCosts       = .false.
      bOilCosts_defined          = .false.
      bParsingPropFuelCosts      = .false.
      bPropCosts_defined         = .false.
      bParsingWoodFuelCosts      = .false.
      bWoodCosts_defined         = .false.

C.....Counter for current data row
      iBC_row_count       = 0
      iBC_col_count       = 0
      iProfile_count      = 0
C.....File error flags
      iMiscError          = 0
      iFile_read_err      = 0
      iFile_write_err     = 0

C----------------------------------------------------------------------
C     Contextual message
C----------------------------------------------------------------------
      write (cContext, '(A,A)') ' Parsing BC data file: ',
     &   cBC_data_file_name(1:lnblnk(cBC_data_file_name))
      call edisp ( iuout, ' ' )
      call edisp ( iuout, cContext)
      cContext = ' '

C----------------------------------------------------------------------
C     Open ASCII file
C----------------------------------------------------------------------

C.....Close the file, if it is open
      call erpfree (iASCII_num, iFile_read_err)

      call EFOPSEQ(iASCII_num,cBC_data_file_name,0,iFile_read_err)

C.....Was file found/opened successfully?
      if ( iFile_read_err .eq. 0  ) then

C........YES! Do nothing.

      else

C........NO! Write warning message.
         write(cMsg,'(A,A,A)') ' File ',
     &      cBC_data_file_name(1:lnblnk(cBC_data_file_name)),
     &      ' could not be read!'

C........Set parse error flag.
         bFatal_Error = .true.

C........If running silently, stop simulator. Otherwise, warn user.
         if ( bAutoOK ) then
            !call edisp ( iuout, cContext)
            call edisp ( iuout, cMsg )

         else
            CALL UsrMsg (cContext, cMsg, 'W')

         endif ! matches 'if ( .not. bAutoOK ...'

      endif    ! matches 'if ( iFile_read_err .eq. 0...'


C----------------------------------------------------------------------
C     Open up a temporary binary file for direct access
C----------------------------------------------------------------------
      inquire ( UNIT=iBC_TmpFile, OPENED=bFile_Open )
C.....Warn if I/O unit number is in use!
      if ( bFile_Open ) then
         close ( UNIT=iBC_TmpFile )
         write (cMsg,'(A,A,I2,A)')
     &     ' Warining: Unit number for temporary ',
     &     ' BCD file (', iBC_TmpFile, ') was in use!'
         if ( bAutoOK ) then
            call edisp(iuout, cMsg)
         else
            call UsrMsg (cContext, cMsg, 'W')
         endif

      endif

      open ( UNIT=iBC_TmpFile,
     &       FILE=cBC_TmpFile_name
     &             (1:lnblnk(cBC_TmpFile_name)),
     &       ACCESS='DIRECT',
     &       RECL=mBC_max_count*4,
     &       FORM='UNFORMATTED',
     &       STATUS='REPLACE',
     &       IOSTAT=iFile_write_err)

      inquire ( UNIT=iBC_TmpFile, OPENED=bFile_Open )
      if ( iFile_write_err .eq. 0 ) then
C.......File opened ok!

      else
C........File could not be opened!
         bFatal_Error   = .true.
C........Let user know!
         write (cMsg,'(A,A,A)')
     &     ' Temporary BCD file (',
     &        cBC_TmpFile_name
     &             (1:lnblnk(cBC_TmpFile_name)),
     &     ') could not be opened for writing!'

         if ( bAutoOK ) then
            !call edisp(iuout, cContext)
            call edisp(iuout, cMsg)
         else
            call UsrMsg (cContext, cMsg, 'W')
         endif

      endif ! <- Matches 'if ( iFile_write_err .eq...


C----------------------------------------------------------------------
C     If files were opened successfully, parse metadata
C---------------------------------------------------------------------
      if ( .not. bFatal_Error ) then
C----------------------------------------------------------------------
C        Get first line
C          -> stripc: stips comments from line. Synopsys:
C             stripc ( file #     (input),
C                      line data (output),
C                      # items expected (input, 0 to disable checking),
C                      Error handling (input, 0 for silent)
C                      Contextual message (input)
C                      error status (output)
C                    )
C----------------------------------------------------------------------
         call LStripc (iASCII_num, cLnBuffer, 0, iDummy, 0,
     &                 'BC file header',
     &                 iFile_read_err)
C----------------------------------------------------------------------
C        Get space/comma/tab separated words
C          -> iEGetArrW: returns the number of words encountered
C             and a character array containing the words. Synopsys
C             # of words = ( string, array of words)
C
C----------------------------------------------------------------------
         iWordCount = iEGetArrW ( cLnBuffer, cLnWords )

C----------------------------------------------------------------------
C        Is file a valid BC data file? First record *MUST* be
C        '*bc_data_file'
C----------------------------------------------------------------------
         if (  cLnWords(1)(1:13) .ne. '*bc_data_file' ) then

C----------------------------------------------------------------------
C           File is not a valid BC data file.
C----------------------------------------------------------------------
            write (cMsg,'(A,A)')
     &         cBC_data_file_name(1:lnblnk(cBC_data_file_name)),
     &         ' is not a valid BC data file! '
            if ( bAutoOK ) then
              !call edisp(iuout, cContext)
              call edisp(iuout, cMsg)
            else
               call UsrMsg (cContext, cMsg, 'W')
            endif

         else
C----------------------------------------------------------------------
C           File is valid. Parse file line by line.
C----------------------------------------------------------------------
            do while ( iFile_read_err  .eq. 0 .and.
     &                 iFile_write_err .eq. 0         )
C..............Empty buffer
               cLnBuffer2500 = ' '
C..............Get line & strip comments
               call STRIPC2500 (iASCII_num, cLnBuffer2500, 0, iDummy, 0,
     &                 'BC data file',
     &                 iFile_read_err)

C..............Split words
               iWordCount = 
     &           iEGetArrW_2500 ( cLnBuffer2500, cLnWords )
C----------------------------------------------------------------------
C              Determine which record type line describes
C----------------------------------------------------------------------
               if ( iWordCount .eq. 0 ) then
C..............Skip empty line
               elseif ( cLnWords(1)(1:8) .eq. '*version' ) then
C----------------------------------------------------------------------
C                 Line denotes version number.
C----------------------------------------------------------------------
C.................Convert version # to real
                  read (cLnWords(2),*, IOSTAT=iMiscError)
     &                  fBC_file_version

                  if ( iMiscError .ne. 0 ) then
C....................Version is not a real.
                     write (cMsg,'(A,A)')
     &                  ' Could not read version number: ',
     &                  cLnWords(3)(1:lnblnk(cLnWords(3)))

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                  else

C....................02-Dec-2011 BAL: Adding version 0.2 which supports
C....................fuel cost data.
C....................Version number is a real. Is it supported?
                     if ( fBC_file_version .lt. 0. .and.
     &                    fBC_file_version .gt. 0.21 ) then
C.......................Version # is unsupported
                        write (cMsg,'(A,F4.2,A)')
     &                     ' File version number (',fBC_file_version,
     &                     ') is unsupported.'

                        if (bAutoOK) then
                           !call edisp(iuout, cContext)
                           call edisp(iuout, cMsg)
                        else
                           call UsrMsg (cContext, cMsg, 'W')
                        endif

                     else

C.......................Version number defined successfully
                        bVersion_defined = .true.

                     endif

                  endif ! <- matches 'if ( iMiscError .ne. 0 ) then'

               elseif ( cLnWords(1)(1:13) .eq. '*period_start' ) then
C----------------------------------------------------------------------
C                 Record defines BC period start
C----------------------------------------------------------------------

                  bPeriod_defined = .true.

C.................Convert start day to real
                  read (cLnWords(2),*, IOSTAT=iMiscError)
     &                  iBC_start_DOM

                  if ( iMiscError .ne. 0 ) bPeriod_defined = .false.

C.................Convert start month to real
                  read (cLnWords(3),*, IOSTAT=iMiscError)
     &                  iBC_start_month

                  if ( iMiscError .ne. 0 ) bPeriod_defined = .false.

C.................Convert start year to real
                  read (cLnWords(4),*, IOSTAT=iMiscError)
     &                  iBC_start_year

                  if ( iMiscError .ne. 0 )  bPeriod_defined = .false.

                  if ( bPeriod_defined ) then

C....................Start date defined successfully - check if it
C....................is valid.
                     if ( iBC_start_month .gt. 12 .or.
     &                    iBC_start_month .lt. 1       ) then

                        bPeriod_defined = .false.

                     elseif ( iBC_start_DOM .lt. 1 .or.
     &                        iBC_start_DOM .gt.
     &                        iDays_of_month(iBC_start_month) ) then

                        bPeriod_defined = .false.

                     endif

                  endif

               elseif ( cLnWords(1)(1:11) .eq. '*period_end' ) then
C----------------------------------------------------------------------
C                 Record defines BC period end
C----------------------------------------------------------------------
                  bPeriod_end_defined = .true.

C.................Convert end day to real
                  read (cLnWords(2),*, IOSTAT=iMiscError)
     &                  iBC_end_DOM

                  if ( iMiscError .ne. 0 ) bPeriod_end_defined = .false.

C.................Convert end month to real
                  read (cLnWords(3),*, IOSTAT=iMiscError)
     &                  iBC_end_month

                  if ( iMiscError .ne. 0 ) bPeriod_end_defined = .false.

C.................Convert end year to real
                  read (cLnWords(4),*, IOSTAT=iMiscError)
     &                  iBC_end_year

                  if ( iMiscError .ne. 0 )
     &                  bPeriod_end_defined = .false.

                  if ( bPeriod_end_defined ) then

C....................end date defined successfully - check if it
C....................is valid.
                     if ( iBC_end_month .gt. 12 .or.
     &                    iBC_end_month .lt. 1       ) then

                        bPeriod_defined = .false.

                     elseif ( iBC_end_DOM .lt. 1 .or.
     &                        iBC_end_DOM .gt.
     &                        iDays_of_month(iBC_end_month) ) then

                        bPeriod_end_defined = .false.

                     endif

                  endif

                  if ( .not. bPeriod_end_defined ) then
C....................end date was not defined successfully

                     write (cMsg,'(A,A,A,A,A,A,A)')
     &                  ' BC period end date is not valid: ',
     &                  cLnWords(2)(1:lnblnk(cLnWords(2))),'/',
     &                  cLnWords(3)(1:lnblnk(cLnWords(3))),'/',
     &                  cLnWords(4)(1:lnblnk(cLnWords(4))),
     &                  ' (DD/MM/YYYY)'

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                  endif

               elseif ( cLnWords(1)(1:10) .eq. '*frequency' ) then
C----------------------------------------------------------------------
C                 Record describes sample frequency
C----------------------------------------------------------------------
C.................Convert frequency  to real
                  read (cLnWords(2),*, IOSTAT=iMiscError)
     &                  fBC_frequency

                  if ( iMiscError .ne. 0 ) then
C....................Frequency could not be read
                     write (cMsg,'(A,A)')
     &                  ' Could not read sample frequency: ',
     &                  cLnWords(2)(1:lnblnk(cLnWords(2)))

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                  elseif ( fBC_frequency .lt. 1.0 ) then
C....................Frequency is too small
                     write (cMsg,'(A)')
     &                  ' Sample frequency must be at least '
     &                  // '1 second. '

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                  elseif ( fBC_frequency .gt. 60.0*60.0 ) then
C....................Frequency is too large
                     write (cMsg,'(A)')
     &                  ' Sample frequency cannot be greater '
     &                  // 'than 3600 seconds. '

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                  else

C....................Frequency is ok
                     bFrequency_defined = .true.

                  endif
C-----------------------------------------------------------------------
C              Record describes hourly load profile
C-----------------------------------------------------------------------
               elseif ( cLnWords(1)(1:15)  .eq. '*hourly_profile' ) then
C.................Increment counter
                  iProfile_count = iProfile_count + 1
                  if ( iWordCount .ne. 4 ) then
                     write (cMsg,'(A,A,I2,A)')
     &                   'Insufficient data items for ',
     &                   'hourly profile #',iProfile_count,
     &                   '. NAME UNITS FILE expected.'
                     bFatal_Error = .true.
                     bProfiles_defined = .false.
                  else

C....................Set flag
                     bProfiles_defined = .true.
C....................Get profile name
                     write (  cProfile_Names(iProfile_count),'(A)')
     &                   cLnWords(2) (1:lnblnk(cLnWords(2)))
C....................Get profile units
                     write (  cProfile_Units(iProfile_count),'(A)')
     &                   cLnWords(3) (1:lnblnk(cLnWords(3)))
C....................Get profile file name
                     write (  cProfile_Files(iProfile_count),'(A)')
     &                   cLnWords(4) (1:lnblnk(cLnWords(4)))
                  endif
C----------------------------------------------------------------------
C              Record describes boundary condition names
C----------------------------------------------------------------------
               elseif ( cLnWords(1)(1:12) .eq. '*data_header' ) then

                  bHeader_defined = .true.

                  if ( ( bUnits_defined .or. bData_defined ) .and.
     &                 iWordCount - 1 .ne. iBC_col_count ) then

                        write (cMsg,'(A,I2,A,I2,A)')
     &                  ' Mismatch between # of header columns (',
     &                  iWordCount,') and # of units/data columns (',
     &                  iBC_col_count,').'

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                     bHeader_defined = .false.

                  elseif ( .not. ( bUnits_defined .or.
     &                             bData_defined       ) ) then

                     iBC_col_count = iWordCount - 1

                  endif

                  if ( bHeader_defined ) then

                     bHeader_defined = .true.
                     do iCol = 1, iBC_col_count
                        write(cBC_names(iCol), '(A)')
     &                     cLnWords(iCol+1)
     &                           (1:lnblnk(cLnWords(iCol+1)))
                     enddo
                  endif

               elseif ( cLnWords(1)(1:11) .eq. '*data_units' ) then
C----------------------------------------------------------------------
C                 Record describes boundry condition units.
C----------------------------------------------------------------------
                  bUnits_defined = .true.
                  if ( ( bHeader_defined .or. bData_defined ) .and.
     &                   iWordCount - 1 .ne. iBC_col_count ) then

                     write (cMsg,'(A,I2,A,I2,A)')
     &                  ' Mismatch between # of unit columns (',
     &                  iWordCount,') and # of header/data columns (',
     &                  iBC_col_count,').'

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                     bUnits_defined = .false.

                  elseif ( .not. ( bHeader_defined .or.
     &                             bData_defined        ) ) then
                     iBC_col_count = iWordCount - 1
                  endif

                  if ( bUnits_defined ) then

                     do iCol = 1, iBC_col_count
                         write(cBC_units(iCol), '(A)')
     &                     cLnWords(iCol+1)
     &                           (1:lnblnk(cLnWords(iCol+1)))
                     enddo
                  endif

               elseif ( cLnWords(1)(1:11) .eq. '*data_start' ) then
C----------------------------------------------------------------------
C                 Record denotes the start of boundry condition data
C----------------------------------------------------------------------
                  bData_defined = .true.
                  bParsing_Data = .true.

               elseif ( cLnWords(1)(1:9) .eq. '*data_end' ) then
C----------------------------------------------------------------------
C                 Record denotes the end of boundry condition data.
C                 Note: this tag is optional, as long as no other
C                 non-data records follow in the file.
C----------------------------------------------------------------------
                  bParsing_Data = .false.

               elseif ( bParsing_Data ) then
C----------------------------------------------------------------------
C                 Record contains boundry condition data.
C----------------------------------------------------------------------

C.................Increment record counter
C.................( note: counter initialized as 0 )
                  iBC_row_count = iBC_row_count + 1

                  if ( iBC_row_count .eq. 1 ) then
C----------------------------------------------------------------------
C                    This is the first data record.
C----------------------------------------------------------------------

C....................If the header or units have not been defined,
C....................save the number fo columns
                     if ( .not. ( bHeader_defined .or.
     &                     bUnits_defined ) ) iBC_col_count = iWordCount

                  endif ! <- Matches  'if ( iBC_row_count .eq. 1 ) then

C----------------------------------------------------------------------
C                 Collect values from ascii input file
C----------------------------------------------------------------------
C.................Check that number of columns is ok
                  if ( iWordCount .ne. iBC_col_count ) then
C....................Mismatch in colum count
                     write (cMsg,'(A,I2,A,I2,A,I2,A)')
     &                  ' Mismatch between # of data columns (',
     &                  iWordCount,
     &                  ') and # of header/unit columns (',
     &                  iBC_col_count,') on data row ',
     &                  iBC_row_count,'.'

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

C....................Data has been incorrectly defined
                     bData_defined = .false.

                  endif ! <- Matches 'if ( iWordCount .ne. iBC_col_count...

C.................20-Oct-2011 BAL: Added check for mismatch in TOU column index                  
C.................If TOU elec. rates, check that the column is <= number of data columns
                  if ( bTOUelecRates
     &                  .and. iTOUcolumn .gt. iBC_col_count) then
     
C....................TOU column index is greater than no. of data columns
                     write (cMsg,'(A,I2,A,I2,A)')
     &                  ' TOU column index (',
     &                  iTOUcolumn,
     &                  ') > # of data columns (',
     &                  iBC_col_count, ').'

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                     
C....................Elec fuel cost data has been incorrectly defined                     
                     bElecCosts_defined = .false.                  
                  endif
                  
C.................If colun count is ok, read values
                  if ( bData_defined ) then

                     do iCol = 1, iBC_col_count
                        read (cLnWords(iCol),*, IOSTAT=iMiscError)
     &                  fLnData(iCol)

                        if ( iMiscError .ne. 0 ) then

                           bData_defined = .false.

                           write (cMsg,'(A,A,A,I2,A)')
     &                        ' Could not convert data record (',
     &                        cLnWords(iCol)
     &                        (1:lnblnk(cLnWords(iCol))),
     &                        ') on data row ', iBC_row_count, '.'

                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)

                           else
                              call UsrMsg (cContext, cMsg, 'W')

                           endif

                        endif ! <- Matches ' if ( iMiscError .ne. 0 ) then

                     enddo    ! <- Matches 'do iCol = 1, iBC_col_count'

                  endif       ! <- Matches 'if ( bData_defined ) then
C----------------------------------------------------------------------
C                 Write values to temp file
C----------------------------------------------------------------------
                  if ( iFile_write_err .eq. 0 ) then

                     write ( UNIT=iBC_TmpFile,
     &                       REC=iBC_row_count,
     &                       IOSTAT=iMiscError )
     &                     ( fLnData(iCol), iCol=1, mBC_max_count )

                  endif ! <- Matches 'if ( iFile_write_err .eq. 0 ) then

               elseif ( cLnWords(1)(1:16) .eq. '*elec_data_start' ) then
C----------------------------------------------------------------------
C                 Record describes beginning of fuel rate data for
C                 electricity.
C----------------------------------------------------------------------
                  bParsingElecFuelCosts = .true.
                  bElecCosts_defined    = .true.                  
                  iMonth = 0
                  
                  bIncFuelCostCalcs     = .true.                  
                  
               elseif ( cLnWords(1)(1:14) .eq. '*elec_data_end' ) then
C----------------------------------------------------------------------
C                 Record describes end of fuel rates data for 
C                 electricity.
C----------------------------------------------------------------------
                  bParsingElecFuelCosts = .false.                  

               elseif ( cLnWords(1)(1:14) .eq. '*ng_data_start' ) then
C----------------------------------------------------------------------
C                 Record describes beginning of fuel rate data for 
C                 natural gas.
C----------------------------------------------------------------------
                  bParsingNGFuelCosts = .true.
                  bNGCosts_defined    = .true.                   
                  iMonth = 0                  
                  
               elseif ( cLnWords(1)(1:12) .eq. '*ng_data_end' ) then
C----------------------------------------------------------------------
C                 Record describes end of fuel rates data for natural
C                 gas.
C----------------------------------------------------------------------
                  bParsingNGFuelCosts = .false.                    

               elseif ( cLnWords(1)(1:15) .eq. '*oil_data_start' ) then
C----------------------------------------------------------------------
C                 Record describes beginning of fuel rate data for
C                 oil.
C----------------------------------------------------------------------
                  bParsingOilFuelCosts = .true.
                  bOilCosts_defined    = .true.                   
                  iMonth = 0                  
                  
               elseif ( cLnWords(1)(1:13) .eq. '*oil_data_end' ) then
C----------------------------------------------------------------------
C                 Record describes end of fuel rate data for oil.
C----------------------------------------------------------------------
                  bParsingOilFuelCosts = .false.  

               elseif ( cLnWords(1)(1:16) .eq. '*prop_data_start' ) then
C----------------------------------------------------------------------
C                 Record describes beginning of fuel rate data for
C                 propane.
C----------------------------------------------------------------------
                  bParsingPropFuelCosts = .true.
                  bPropCosts_defined    = .true.                   
                  iMonth = 0                  
                  
               elseif ( cLnWords(1)(1:14) .eq. '*prop_data_end' ) then
C----------------------------------------------------------------------
C                 Record describes end of fuel rate data for propane.
C----------------------------------------------------------------------
                  bParsingPropFuelCosts = .false.                       

               elseif ( cLnWords(1)(1:16) .eq. '*wood_data_start' ) then
C----------------------------------------------------------------------
C                 Record describes beginning of fuel rate data for
C                 wood.
C----------------------------------------------------------------------
                  bParsingWoodFuelCosts = .true.
                  bWoodCosts_defined    = .true.                   
                  iMonth = 0                  
                  
               elseif ( cLnWords(1)(1:14) .eq. '*wood_data_end' ) then
C----------------------------------------------------------------------
C                 Record describes end of fuel rate data for wood.
C----------------------------------------------------------------------
                  bParsingWoodFuelCosts = .false.                        
                  
               elseif ( bParsingElecFuelCosts ) then 

C.................Check if time of use (TOU) rates have been specified. If not, 
C.................then read monthly block rates.               
                  IF ( cLnWords(1)(1:11) .eq. '*TOU_column' ) THEN

C....................Set TOU flag                  
                     bTOUelecRates = .true.
                     
C....................Read the column index of the TOU schedule in bc data
                     read (cLnWords(2),*,IOSTAT = iMiscError)
     &                     iTOUcolumn
     
                       if ( iMiscError .ne. 0 ) then
                           bElecCosts_defined = .false. 
                           write (cMsg,'(A,A,A)')
     &                      'Elec. fuel costs: could not convert',
     &                      ' TOU column index (', cLnWords(2)
     &                      (1:lnblnk(cLnWords(2)))
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                       endif                         
                  
                  ELSE
                  
C...................Increment month counter
C...................( note: counter initialized as 0 )
                    iMonth = iMonth + 1

C----------------------------------------------------------------------
C                   Collect values from ascii input file                                                                       
C----------------------------------------------------------------------                  
C...................Check that number of data items in row is ok
C...................Should be: 2 items for min. units and min charge, 8 items for rate blocks = 10 items total
                    if ( iWordCount .ne. 10) then
C......................Mismatch in colum count
                       write (cMsg,'(A,I2,A,I2)')
     &                  'In elec. fuel cost data, # of items (',
     &                  iWordCount,
     &                  ') does not match the expected 10 in month',
     &                  iMonth

                       if ( bAutoOK ) then
                          !call edisp(iuout, cContext)
                          call edisp(iuout, cMsg)
                       else
                          call UsrMsg (cContext, cMsg, 'W')
                       endif
                     
C......................Elec. fuel cost data has been incorrectly defined                     
                       bElecCosts_defined = .false.
                     
                    endif
                  
C...................If column count is ok, read values
                    if ( bElecCosts_defined ) then

C......................First read the min. units (item 1)
                       read (cLnWords(1),*,IOSTAT = iMiscError)
     &                    fFuelMinUnits( iElectricity, iMonth )
     
                         if ( iMiscError .ne. 0 ) then
                             bElecCosts_defined = .false. 
                             write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Elec. fuel costs: could not convert',
     &                        ' data record (', cLnWords(1)
     &                        (1:lnblnk(cLnWords(1))),
     &                        ') in month ', iMonth, '.'
     
                             if ( bAutoOK ) then
                                !call edisp(iuout, cContext)
                                call edisp(iuout, cMsg)
                             else
                                call UsrMsg (cContext, cMsg, 'W')
                             endif
                         endif                    
                    
C......................Then read the min. fuel cost (item 2)
                       read (cLnWords(2),*,IOSTAT = iMiscError)
     &                    fFuelMinCharge( iElectricity, iMonth )
     
                         if ( iMiscError .ne. 0 ) then
                             bElecCosts_defined = .false. 
                             write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Elec. fuel costs: could not convert',
     &                        ' data record (', cLnWords(2)
     &                        (1:lnblnk(cLnWords(2))),
     &                        ') in month ', iMonth, '.'
     
                             if ( bAutoOK ) then
                                !call edisp(iuout, cContext)
                                call edisp(iuout, cMsg)
                             else
                                call UsrMsg (cContext, cMsg, 'W')
                             endif
                         endif 
     
C......................Next read the four fuel blocks (number of energy units and cost/unit)
                       do iBlock = 1, 4
                       read (cLnWords(iBlock*2+1),*,IOSTAT=iMiscError)
     &                    fFuelBlockUnits(iElectricity,iMonth,iBlock)
     
                          if ( iMiscError .ne. 0 ) then
                             bElecCosts_defined = .false. 
                             write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Elec. fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+1)
     &                        (1:lnblnk(cLnWords(iBlock*2+1))),
     &                        ') in month ', iMonth, '.'
     
                             if ( bAutoOK ) then
                                !call edisp(iuout, cContext)
                                call edisp(iuout, cMsg)
                             else
                                call UsrMsg (cContext, cMsg, 'W')
                             endif
                          endif      
     
                          read (cLnWords((iBlock*2)+2),*, 
     &                       IOSTAT=iMiscError)
     &                       fFuelBlockCost(iElectricity,iMonth,iBlock)
     
                          if ( iMiscError .ne. 0 ) then
                             bElecCosts_defined = .false. 
                             write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Elec. fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+2)
     &                        (1:lnblnk(cLnWords(iBlock*2+2))),
     &                        ') in month ', iMonth, '.'
     
                             if ( bAutoOK ) then
                                !call edisp(iuout, cContext)
                                call edisp(iuout, cMsg)
                             else
                                call UsrMsg (cContext, cMsg, 'W')
                             endif
                          endif     
     
                       end do
                     
                    endif
                  
                  ENDIF
                  
               elseif ( bParsingNGFuelCosts ) then 

C.................Increment month counter
C.................( note: counter initialized as 0 )
                  iMonth = iMonth + 1

C----------------------------------------------------------------------
C                 Collect values from ascii input file                                                                       
C----------------------------------------------------------------------                  
C.................Check that number of data items in row is ok
C...................Should be: 2 items for min. units and min charge, 8 items for rate blocks = 10 items total
                    if ( iWordCount .ne. 10) then
C....................Mismatch in colum count
                     write (cMsg,'(A,I2,A,I2)')
     &                  'In natural gas fuel cost data, # of items (',
     &                  iWordCount,
     &                  ') does not match the expected 10 in month',
     &                  iMonth

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                     
C....................Natural gas fuel cost data has been incorrectly defined                     
                     bNGCosts_defined = .false.
                     
                  endif
                  
C.................If column count is ok, read values
                  if ( bNGCosts_defined ) then
                  
C....................First read the min. units (item 1)
                     read (cLnWords(1),*,IOSTAT = iMiscError)
     &                  fFuelMinUnits( iNaturalGas, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bNGCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Natural gas fuel costs: could not',
     &                        ' convert data record (', cLnWords(1)
     &                        (1:lnblnk(cLnWords(1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif 
                        
C....................Then read the min. fuel cost (item 2)
                     read (cLnWords(2),*,IOSTAT = iMiscError)
     &                  fFuelMinCharge( iNaturalGas, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bNGCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Natural gas fuel costs: could not',
     &                        ' convert data record (', cLnWords(2)
     &                        (1:lnblnk(cLnWords(2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif                        
     
C....................Next read the four fuel blocks (number of energy units and cost/unit)
                     do iBlock = 1, 4
                        read (cLnWords(iBlock*2+1),*,IOSTAT=iMiscError)
     &                     fFuelBlockUnits(iNaturalGas,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bNGCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Natural gas fuel costs: could not',
     &                        ' convert data record (',
     &                        cLnWords(iBlock*2+1)
     &                        (1:lnblnk(cLnWords(iBlock*2+1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif      
     
                        read (cLnWords(iBlock*2+2),*, 
     &                     IOSTAT=iMiscError)
     &                     fFuelBlockCost(iNaturalGas,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bNGCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Natural gas fuel costs: could not',
     &                        ' convert data record (',
     &                        cLnWords(iBlock*2+2)
     &                        (1:lnblnk(cLnWords(iBlock*2+2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif    
     
                     end do
                     
                  endif
                  
               elseif ( bParsingOilFuelCosts ) then 

C.................Increment month counter
C.................( note: counter initialized as 0 )
                  iMonth = iMonth + 1

C----------------------------------------------------------------------
C                 Collect values from ascii input file                                                                       
C----------------------------------------------------------------------                  
C.................Check that number of data items in row is ok
C...................Should be: 2 items for min. units and min charge, 8 items for rate blocks = 10 items total
                    if ( iWordCount .ne. 10) then
C....................Mismatch in colum count
                     write (cMsg,'(A,I2,A,I2)')
     &                  'In oil fuel cost data, # of items (',
     &                  iWordCount,
     &                  ') does not match the expected 10 in month',
     &                  iMonth

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                     
C....................Oil fuel cost data has been incorrectly defined                     
                     bOilCosts_defined = .false.
                     
                  endif
                  
C.................If column count is ok, read values
                  if ( bOilCosts_defined ) then
                  
C....................First read the min. units (item 1)
                     read (cLnWords(1),*,IOSTAT = iMiscError)
     &                  fFuelMinUnits( iOil, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bOilCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Oil fuel costs: could not convert',
     &                        ' data record (', cLnWords(1)
     &                        (1:lnblnk(cLnWords(1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif 
                        
C....................Then read the min. fuel cost (item 2)
                     read (cLnWords(2),*,IOSTAT = iMiscError)
     &                  fFuelMinCharge( iOil, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bOilCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Oil fuel costs: could not convert',
     &                        ' data record (', cLnWords(2)
     &                        (1:lnblnk(cLnWords(2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif                        
     
C....................Next read the four fuel blocks (number of energy units and cost/unit)
                     do iBlock = 1, 4
                        read (cLnWords(iBlock*2+1),*,IOSTAT=iMiscError)
     &                     fFuelBlockUnits(iOil,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bOilCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Oil fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+1)
     &                        (1:lnblnk(cLnWords(iBlock*2+1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif      
     
                        read (cLnWords(iBlock*2+2),*, 
     &                     IOSTAT=iMiscError)
     &                     fFuelBlockCost(iOil,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bOilCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Oil fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+2)
     &                        (1:lnblnk(cLnWords(iBlock*2+2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif     
     
                     end do
                     
                  endif
                  
               elseif ( bParsingPropFuelCosts ) then 

C.................Increment month counter
C.................( note: counter initialized as 0 )
                  iMonth = iMonth + 1

C----------------------------------------------------------------------
C                 Collect values from ascii input file                                                                       
C----------------------------------------------------------------------                  
C.................Check that number of data items in row is ok
C...................Should be: 2 items for min. units and min charge, 8 items for rate blocks = 10 items total
                    if ( iWordCount .ne. 10) then
C....................Mismatch in colum count
                     write (cMsg,'(A,I2,A,I2)')
     &                  'In propane fuel cost data, # of items (',
     &                  iWordCount,
     &                  ') does not match the expected 10 in month',
     &                  iMonth

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                     
C....................Propane fuel cost data has been incorrectly defined                     
                     bPropCosts_defined = .false.
                     
                  endif
                  
C.................If column count is ok, read values
                  if ( bPropCosts_defined ) then               
               
C....................First read the min. units (item 1)
                     read (cLnWords(1),*,IOSTAT = iMiscError)
     &                  fFuelMinUnits( iPropane, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bPropCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Propane fuel costs: could not convert',
     &                        ' data record (', cLnWords(1)
     &                        (1:lnblnk(cLnWords(1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif 
                        
C....................Then read the min. fuel cost (item 2)
                     read (cLnWords(2),*,IOSTAT = iMiscError)
     &                  fFuelMinCharge( iPropane, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bPropCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Propane fuel costs: could not convert',
     &                        ' data record (', cLnWords(2)
     &                        (1:lnblnk(cLnWords(2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif                        
     
C....................Next read the four fuel blocks (number of energy units and cost/unit)
                     do iBlock = 1, 4
                        read (cLnWords(iBlock*2+1),*,IOSTAT=iMiscError)
     &                     fFuelBlockUnits(iPropane,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bPropCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Propane fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+1)
     &                        (1:lnblnk(cLnWords(iBlock*2+1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif      
     
                        read (cLnWords(iBlock*2+2),*, 
     &                     IOSTAT=iMiscError)
     &                     fFuelBlockCost(iPropane,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bPropCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Propane fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+2)
     &                        (1:lnblnk(cLnWords(iBlock*2+2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif     
     
                     end do
                     
                  endif               
               
               elseif ( bParsingWoodFuelCosts ) then 
               
C.................Increment month counter
C.................( note: counter initialized as 0 )
                  iMonth = iMonth + 1

C----------------------------------------------------------------------
C                 Collect values from ascii input file                                                                       
C----------------------------------------------------------------------                  
C.................Check that number of data items in row is ok
C...................Should be: 2 items for min. units and min charge, 8 items for rate blocks = 10 items total
                    if ( iWordCount .ne. 10) then
C....................Mismatch in colum count
                     write (cMsg,'(A,I2,A,I2)')
     &                  'In wood fuel cost data, # of items (',
     &                  iWordCount,
     &                  ') does not match the expected 10 in month',
     &                  iMonth

                     if ( bAutoOK ) then
                        !call edisp(iuout, cContext)
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                     
C....................Wood fuel cost data has been incorrectly defined                     
                     bWoodCosts_defined = .false.
                     
                  endif
                  
C.................If column count is ok, read values
                  if ( bWoodCosts_defined ) then                  
               
C....................First read the min. units (item 1)
                     read (cLnWords(1),*,IOSTAT = iMiscError)
     &                  fFuelMinUnits( iMixedWood, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bWoodCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Wood fuel costs: could not convert',
     &                        ' data record (', cLnWords(1)
     &                        (1:lnblnk(cLnWords(1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif 
                        
C....................First read the min. fuel cost (item 1)
                     read (cLnWords(2),*,IOSTAT = iMiscError)
     &                  fFuelMinCharge( iMixedWood, iMonth )
     
                        if ( iMiscError .ne. 0 ) then
                           bWoodCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Wood fuel costs: could not convert',
     &                        ' data record (', cLnWords(2)
     &                        (1:lnblnk(cLnWords(2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif                         
     
C....................Next read the four fuel blocks (number of energy units and cost/unit)
                     do iBlock = 1, 4
                        read (cLnWords(iBlock*2+1),*,IOSTAT=iMiscError)
     &                     fFuelBlockUnits(iMixedWood,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bWoodCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Wood fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+1)
     &                        (1:lnblnk(cLnWords(iBlock*2+1))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif      
     
                        read (cLnWords(iBlock*2+2),*, 
     &                     IOSTAT=iMiscError)
     &                     fFuelBlockCost(iMixedWood,iMonth,iBlock)
     
                        if ( iMiscError .ne. 0 ) then
                           bWoodCosts_defined = .false. 
                           write (cMsg,'(A,A,A,A,I2,A)')
     &                        'Wood fuel costs: could not convert',
     &                        ' data record (', cLnWords(iBlock*2+2)
     &                        (1:lnblnk(cLnWords(iBlock*2+2))),
     &                        ') in month ', iMonth, '.'
     
                           if ( bAutoOK ) then
                              !call edisp(iuout, cContext)
                              call edisp(iuout, cMsg)
                           else
                              call UsrMsg (cContext, cMsg, 'W')
                           endif
                        endif     
     
                     end do
                     
                  endif
                  
               else
C----------------------------------------------------------------------
C                 Unknown record type: Warn user and move on.
C----------------------------------------------------------------------
                  write (cMsg,'(A,A)')
     &              ' Warning - unknown record type:',
     &               cLnWords(1)(1:lnblnk(cLnWords(1)))

                  if ( bAutoOK ) then
                     !call edisp(iuout, cContext)
                     call edisp(iuout, cMsg)
                  else
                     call UsrMsg (cContext, cMsg, 'W')
                  endif

               endif ! <- Matches 'if ( cLnWords(1)(1:8) .eq. '*version'...

            enddo !<- Matches 'do while ( iFile_read_err .eq. 0 )'

         endif ! <- matches  ' if (  cLnWords(1:13)(1) .eq. ...'

      endif  ! <- matches 'if ( .not. bFatal_Error ) then'


C----------------------------------------------------------------------
C     Close ascii file
C----------------------------------------------------------------------
      close ( UNIT=iASCII_num )

C.....Convert start date to days
      call eday(iBC_start_DOM,iBC_start_month,iBC_Start_DOY)
C.....Calculate start & end day (negitive #'s are ok for MY simulations)
      if ( bMY_sim_enabled ) then

         iBC_Start_day = iBC_Start_DOY
     &               + 365 * ( iBC_start_year - iMY_start_year )

      else
         iBC_Start_day = iBC_Start_DOY
         if ( issny .eq. 1 ) then
            iBC_Start_day = iBC_Start_day + 365

         endif
      endif


C----------------------------------------------------------------------
C     Check if the user specified an end period
C----------------------------------------------------------------------
      if ( bPeriod_end_defined .and.
     &     ( bData_defined .or. bProfiles_defined ).and.
     &    .not. bFatal_Error ) then

C........Calculate end day (negitive #'s are ok for MY simulations)
         call eday(iBC_end_DOM, iBC_end_month, iBC_End_DOY )
         if ( bMY_sim_enabled ) then
            iBC_End_day = iBC_End_DOY
     &                  + 365 * ( iBC_end_year - iMY_start_year )
         else
            iBC_End_day   = iBC_End_DOY
            if ( issny .eq. 1 ) then
               iBC_End_day   = iBC_End_day   + 365
            endif

         endif
C----------------------------------------------------------------------
C        Calculate the number of rows needed to fill the period
C----------------------------------------------------------------------
C........Determine rows
         iEndRow = int ( float ( iBC_End_day - iBC_Start_day ) )
     &                * 24.0 * 3600.0  / fBC_frequency
C----------------------------------------------------------------------
C        IF sufficent rows have not been defined, add some more.
C        (array fLnData will contain the BC values from the
C        last row in the ascii file).
C----------------------------------------------------------------------
         if ( iEndRow .gt. iBC_row_count ) then
            do iRow = iBC_row_count+1, iEndRow
               write ( UNIT=iBC_TmpFile,
     &                 REC=iRow,
     &                 IOSTAT=iMiscError )
     &               ( fLnData(iCol), iCol=1, mBC_max_count )

            enddo
            iBC_row_count = iEndRow
         endif
      endif
C----------------------------------------------------------------------
C     Check if hourly profiles have been defined
C----------------------------------------------------------------------

      if ( bProfiles_defined ) then
C----------------------------------------------------------------------
C        Loop through profiles, adding each to BC data set
C----------------------------------------------------------------------
         do iProfile = 1, iProfile_count
C...........Increment column counter
            iBC_col_count = iBC_col_count + 1
C...........Assign name
            write ( cBC_names ( iBC_col_count ), '(A)')
     &        cProfile_Names(iProfile)
     &           (1:lnblnk(cProfile_Names(iProfile)))
C...........Assign units
            write ( cBC_units ( iBC_col_count ), '(A)')
     &        cProfile_units(iProfile)
     &           (1:lnblnk(cProfile_units(iProfile)))

C-----------------------------------------------------------------------
C           Convert ascii load input file into temporary binary file
C           for random access
C-----------------------------------------------------------------------
            call convert_lds_to_bin_file( cProfile_Files(iProfile),
     &                                    iProfile_file_num,
     &                                    bProfile_err )
            if ( bProfile_err ) then
               bFatal_Error = .true.
               bProfiles_defined = .false.
               write (cMsg, '(A,A)')
     &          ' Error converting hourly profile ',
     &          cProfile_Files(iProfile)
     &                (1:lnblnk(cProfile_Files(iProfile)))
            else
C--------------------------------------------------------------------------
C              Loop through rows, and append profile values to
C              existing row
C-------------------------------------------------------------------------
               do iRow = 1, iBC_row_count
C-------------------------------------------------------------------------
C                 Convert row into appropriate hour/day_of_year/year
C-------------------------------------------------------------------------
C.................Determine the number of hours and days that have
C.................lapsed
                  fSeconds_Lapsed = float ( iRow - 1 ) * fBC_frequency
                  fHours_Lapsed   = fSeconds_Lapsed / 3600.0
                  fDays_Lapsed    = fHours_Lapsed / 24.0

C.................Current hour of day
                  fRow_Hour = fHours_Lapsed - 24.0 * aint(fDays_Lapsed)
C.................Current day of year
                  iRow_day  =  iBC_start_DOY + int( fDays_Lapsed )
C.................Shift iRow_day to ensure it does not exceed 365,
C.................and increment row year as necessary
                  iRow_Year =  iBC_start_year
                  do while (iRow_day .gt. 365)
                     iRow_day = iRow_day - 365
                     iRow_Year = iRow_Year + 1
                  enddo

C------------------------------------------------------------------------
C                 Recover profile value from binary storage file,
C                 and append to current row
C------------------------------------------------------------------------
                  fProfile_Value = fRecover_lds_from_bin_file(
     &                        iProfile_file_num,
     &                        fRow_Hour,
     &                        iRow_day,
     &                        iRow_Year  )

C.................Collect existing data
                  read ( UNIT=iBC_TmpFile,
     &                   REC=iRow,
     &                   IOSTAT=iFile_read_err )
     &                 ( fLnData(iCol), iCol = 1, mBC_max_count )

C.................Append profile value to array
                  fLnData(iBC_col_count) = fProfile_Value

C.................Write values back to file
                  write ( UNIT=iBC_TmpFile,
     &                    REC=iRow,
     &                    IOSTAT=iMiscError )
     &                  ( fLnData(iCol), iCol=1, mBC_max_count )
               enddo


            endif

C----------------------------------------------------------------------
C           Close and delete temporary loads file
C----------------------------------------------------------------------
            call eFDelet ( iProfile_file_num, iMiscError )

         enddo

      endif

C----------------------------------------------------------------------
C     If profiles were successfully defined, set data definition flag
C     to true.
C----------------------------------------------------------------------
      if (bProfiles_defined .and. .not. bFatal_Error ) then
          bData_defined = .true.
          bUnits_defined = .true.
          bHeader_defined = .true.
      endif

C----------------------------------------------------------------------
C     Check that essential data was collected
C----------------------------------------------------------------------
      if ( .not. bHeader_defined    .or.
     &     .not. bData_defined      .or.
     &     .not. bUnits_defined     .or.
     &     .not. bFrequency_defined .or.
     &     .not. bPeriod_defined    .or.
     &     .not. bVersion_defined   .or. 
     &     ( bIncFuelCostCalcs      .and.
     &        (   .not. bElecCosts_defined .or.
     &            .not. bNGCosts_defined   .or. 
     &            .not. bOilCosts_defined  .or. 
     &            .not. bPropCosts_defined .or. 
     &            .not. bWoodCosts_defined )  ) )bFatal_Error = .true.      



C----------------------------------------------------------------------
C     Were errors encountered?
C----------------------------------------------------------------------

      if ( bFatal_Error ) then
C........Yes. Warn user and delete temporary file.
         close ( UNIT=iBC_TmpFile, STATUS='DELETE')
         call edisp(iUout, ' Boundry condition file was not processed'
     &         // ' successfully.')

         if ( bAutoOK ) then

            stop 'Fatal Error!'

         endif

      else

         !call edisp(iUout, cContext )
         call edisp(iUout, ' Boundry condition data file was processed'
     &         // ' successfully.')
      endif


      return
      end


C---------------  bInquire_BC_Name_Exists -------------------------------
C
C ABSTRACT:
C
C bInquire_BC_Name_Exists determines if a requested boundary condition
C has been defined.
C
C Inputs:
C
C    - cCol_name: Name of desired boundary condition
C
C Outputs:
C
C    - bInquire_BC_Name_Exists: logical result.
C
C------------------------------------------------------------------------
      logical function bInquire_BC_Name_Exists ( cCol_name )
      implicit none
#include "bc_data.h"
C------------------------------------------------------------------------
C     Passed arguements
C------------------------------------------------------------------------
      character*248 cCol_name

C------------------------------------------------------------------------
C     Local variables
C------------------------------------------------------------------------
      integer iCol               ! counter

      bInquire_BC_Name_Exists = .false.

      if ( bBC_data_defined ) then

         do icol = 1, iBC_col_count

            if ( cCol_name .eq. cBC_names (iCol) )
     &           bInquire_BC_Name_Exists = .true.

         enddo

      endif

      return
      end

C---------------  fGet_BC_data_by_name ----------------------------------
C
C ABSTRACT:
C
C fGet_BC_data_by_name is a short-hand interface to  fGet_BC_data_by_col
C which allows the boundary condition's name to be used to identify
C the appropriate column.
C
C Inputs:
C
C   cCol_name: Desired BC column number
C   fDay: real number denoting day (& fraction thereof) for which
C      the boundry condition should be recovered.
C   iInterp_method: method for interpolation
C      1 -> step-wise
C      2 -> linear
C
C Outputs
C
C   Value of BC at future time row
C
C----------------------------------------------------------------------
      real function fGet_BC_data_by_name(
     &                                   cCol_name,
     &                                   fDay,
     &                                   iInterp_method,
     &                                   cContext       )
      implicit none
#include "bc_data.h"

C External functions.
      integer lnblnk

C Passed arguements
      character*248 cCol_name
      real fDay
      integer iInterp_method
      character*124 cContext

C------------------------------------------------------------------------
C     ESP-r commons
C------------------------------------------------------------------------
C.....I/O
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C----------------------------------------------------------------------
C     Local varaiables
C----------------------------------------------------------------------
      character*124 cMsg
      logical bFatal_Error
      logical bCol_Found
      integer iCount
      integer iCol_number

C---------------------------------------------------------------------------------
C     References
C---------------------------------------------------------------------------------
      real fGet_BC_data_by_col        ! long hand interface for this function

      bFatal_Error = .false.
      bCol_Found   = .false.

C-------------------------------------------------------------------------
C     Check that boundary conditions exist
C-------------------------------------------------------------------------

      if ( .not. bBC_data_defined ) then
         write (cMsg,'(A)') 'Boundary conditions have not been defined!'
         bFatal_Error = .true.
      else

C-------------------------------------------------------------------------
C        Loop through boundary conditions and retrive column associated
C        with name
C-------------------------------------------------------------------------
         do iCount = 1, iBC_col_count

            if ( cCol_name .eq. cBC_names (iCount) ) then

               iCol_number = iCount
               bCol_Found = .true.

            endif

         enddo

C----------------------------------------------------------------------
C        Check that column has been defined
C----------------------------------------------------------------------
         if ( .not. bCol_Found ) then
            write (cMsg,'(A,A,A)') 'Boundary condition ',
     &         cCol_name(1:lnblnk(cCol_name)), ' has not been defined!'
            bFatal_Error = .true.
         else

C----------------------------------------------------------------------
C           Retrive boundary condition
C----------------------------------------------------------------------
            fGet_BC_data_by_name = fGet_BC_data_by_col ( iCol_number,
     &                                                fDay,
     &                                                iInterp_method,
     &                                                cContext       )

         endif
      endif

      if ( bFatal_Error ) then
         call edisp(iuout, cContext)
         call edisp(iuout, cMsg)
         stop 'fGet_BC_data_by_col(): Fatal error!'
      endif

      return
      end


C---------------  fGet_BC_data_by_col ----------------------------------
C
C ABSTRACT:
C
C fGet_BC_data_by_col returns boundry-condition data corresponding
C to the current time step and user-specified column number.
C
C Inputs:
C
C   iCol_number: Desired BC column number
C   fDay: real number denoting day (& fraction thereof) for which
C      the boundry condition should be recovered.
C   iInterp_method: method for interpolation
C      1 -> step-wise
C      2 -> linear
C
C Outputs
C
C   Value of BC at future time row
C
C----------------------------------------------------------------------
      real function fGet_BC_data_by_col(
     &                                   iCol_number,
     &                                   fDay,
     &                                   iInterp_method,
     &                                   cContext       )
      implicit none
#include "bc_data.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iCol_number     ! column number.
      integer iInterp_method  ! interpolation method
      real fDay               ! day number
      character*124 cContext  ! Contextual message
C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/simsdy/iSS,iSF
      integer iSS            ! simulation start day
      integer iSF            ! simulation end day

C.....Simulator status
      common/spfl/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical spfileok,perok,tstepok,saveok,autook,exitok,startupok

C.....Time
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1              ! not used in current context
      integer ism1              ! not used in current context
      integer isd2              ! not used in current context
      integer ism2              ! not used in current context
      integer isds              ! not used in current context
      integer isdf              ! not used in current context
      integer ntstep            ! number of building steps/hour



      common/pctstp/ntstpp
      integer ntstpp            ! number of plant timesteps / building timestep


C.....I/O
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      real fValues_P(mBC_max_count)  ! values for present BC row
      real fValues_F(mBC_max_count)  ! values for future BC row

      logical bInitialized         ! flag indicating procedures have
                                   ! been initialized.
      data bInitialized / .false. /
      save bInitialized

      integer iTS_requested       ! requested time row
      real    fRow                ! real (fracional) BC row corresponding
                                  !   to timestep
      integer iRow_P              ! recovered present BC row
      integer iRow_F              ! recovered future BC row

      real fRowRemainder          ! remainder for row
      real fRowPercision          ! percision to which row must be determined
      save fRowPercision


      logical bCloseToZero        ! Flags for numerical comparisons
      logical bCloseToOne

C.....Warning messages
      character*124 cMsg          ! message to be displayed to buffer
      logical bFatal_Error



C----------------------------------------------------------------------
C     If this is the first call, determine req'd percision
C----------------------------------------------------------------------
      if ( .not. bInitialized ) then

         bInitialized = .true.

C----------------------------------------------------------------------
C        Calculate percsion to which row must be determined
C
C
C        Percision = rows / timestep * 10 %
C
C                    rows / second * seconds / hour
C                  = ------------------------------ * 0.1
C                          timesteps / hour
C
C
C
C----------------------------------------------------------------------
         fRowPercision = 1.0 / fBC_frequency * 3600.0
     &                       / float ( ntstep * ntstpp ) * 0.1

      endif  ! <- matches 'if ( .not. bInitialized ) then'

C----------------------------------------------------------------------
C     Check that BC file has been defined.
C----------------------------------------------------------------------
      if ( .not. bBC_data_defined ) then
         write (cMsg,'(A)')
     &      'Boundary conditions have not been defined!'
         bFatal_Error = .true.
      endif
C----------------------------------------------------------------------
C     Check that supplied column number is within bounds
C----------------------------------------------------------------------
      if ( bBC_data_defined .and.
     &     iCol_number .le. 0 ) then
         write (cMsg,'(A,I3,A)')
     &     'Specified column (', iCol_number,
     &     ') must be greater than zero!'
         bFatal_Error = .true.
      elseif ( bBC_data_defined .and.
     &     iCol_number .gt. iBC_col_count ) then
         write (cMsg, '(A,I2,A,I2,A)')
     &      'Column ', iCol_number,' was requested, but only ',
     &      iBC_col_count,' columns have been defined.'
         bFatal_Error = .true.

C----------------------------------------------------------------------
C        Check that interpolation scheme is supported
C----------------------------------------------------------------------

      elseif ( bBC_data_defined .and.
     &     iInterp_method .ne. iStep_Interp   .and.
     &     iInterp_method .ne. iLinear_Interp       ) then
         write (cMsg,'(A,i2,A)')
     &     'Specified interpolation method (', iInterp_method,
     &     ') is not supported!'
         bFatal_Error = .true.
      endif

      if ( .not. bFatal_Error ) then
C-----------------------------------------------------------------------
C        Calculate fractional BC row corresponding to current time
C
C                   ( Day - start day ) * ( seconds / day )
C        row = 1 +  --------------------------------------
C                             seconds / row
C
C-----------------------------------------------------------------------
         fRow = 1.0 + ( fDay - float ( iBC_Start_day ))
     &                 * ( 3600.0 * 24.0 )
     &                 / fBC_frequency

C-----------------------------------------------------------------------
C        Machine error can cause us to pick the wrong bounding rows
C        for fRow when fRow is close to 0 or 1. Calculate remainder,
C        & compare to 0 and 1
C-----------------------------------------------------------------------
         fRowRemainder = fRow - float( int( fRow ) )

C........Is remainder close to zero, or one?
         call eclose ( fRowRemainder, 0.0, fRowPercision, bCloseToZero )
         call eclose ( fRowRemainder, 1.0, fRowPercision, bCloseToOne  )

         if ( bCloseToZero ) then
C...........Round down
            iRow_P = int (fRow)
            iRow_F = iRow_P + 1
            fRow   = float( iRow_P )
         elseif ( bCloseToOne ) then
C...........Round up
            iRow_P = int (fRow) + 1
            iRow_F = iRow_P + 1
            fRow   = float( iRow_P )
         else
C...........There's no need to round.
            iRow_P = int (fRow)
            iRow_F = iRow_P + 1
         endif


         if ( fRow .lt. 1.0) fRow = 1.0
         if ( fRow .gt. float(iBC_row_count) )
     &                         fRow = float(iBC_row_count)

C........Convert fractional row to corresponding present row
         iRow_P = int ( fRow )

C........iRow_P should be between 1 and iBC_row_count-1, but machine error
C........can do funny things...

         if ( iRow_P .lt. 1 ) iRow_P = 1
C........Calculate correponding future row
         iRow_F = iRow_P + 1

C........Check that rows are not beyond defined data period. Use
C........last defined datapoints, if so.
         if ( iRow_P .gt. iBC_row_count ) iRow_P = iBC_row_count
         if ( iRow_F .gt. iBC_row_count ) iRow_F = iBC_row_count



C----------------------------------------------------------------------
C        Recover values from boundary condition rows
C----------------------------------------------------------------------
         call get_BC_row ( iRow_P, fValues_P, cContext)
         call get_BC_row ( iRow_F, fValues_F, cContext)

C----------------------------------------------------------------------
C        Interpolate for intermediate value
C----------------------------------------------------------------------
         if ( iInterp_method .eq. iLinear_Interp ) then
C...........Linear interpolation
            fGet_BC_data_by_col = fValues_P(iCol_number)
     &          + ( fRow - float(iRow_P) )
     &          * ( fValues_F(iCol_number) - fValues_P(iCol_number) )

         elseif ( iInterp_method .eq. iStep_Interp ) then
C...........Step interpolation.
            fGet_BC_data_by_col = fValues_P(iCol_number)
         else
C...........Can't happen - iInterp_method error trapped above.

         endif ! <- Matches 'if ( iInterp_method .eq. ...


      endif ! <- Matches 'if ( .not. bBC_data_defined ) then'

C----------------------------------------------------------------------
C     Were errors encountered?
C----------------------------------------------------------------------
      if ( bFatal_Error ) then

         if ( autook ) then
            call edisp(iuout, cContext)
            call edisp(iuout, cMsg)
         else
            call UsrMsg (cContext, cMsg, 'W')
         endif

         stop 'Fatal Error!'

      endif

      return
      end


C---------------  get_BC_row -------------------------------------
C
C ABSTRACT:
C
C get_BC_row returns an array containing boundary condition values
C corresponding to a given boundary condition row number.

C Inputs:
C
C   iRow: row number
C
C Outputs
C
C   fValues: array containing the BC values for the given row number.
C
C----------------------------------------------------------------------

      subroutine get_BC_row( iRow, fValues, cContext )
      implicit none
#include "bc_data.h"

C External functions.
      integer lnblnk

C Passed arguements
      integer iRow                      ! Row number
      real fValues(mBC_max_count)       ! Values from file

C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
C.....Simulator status
      common/spfl/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical spfileok,perok,tstepok,saveok,autook,exitok,startupok
C.....I/O
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iFile_read_err      ! Error flag
      logical bFile_Open

      integer iCount              ! counter
      character*72 cFileName      ! temporary buffer for file name

C.....Warning messages
      character*124 cMsg          ! message to be displayed to buffer
      character*124 cContext      ! contextual message

C----------------------------------------------------------------------
C     Make sure file is attached
C----------------------------------------------------------------------
      inquire ( UNIT=iBC_TmpFile, NAME=cFileName, OPENED=bFile_Open )

      if ( bFile_Open )  then
         read ( UNIT=iBC_TmpFile,
     &          REC=iRow,
     &          IOSTAT=iFile_read_err )
     &        ( fValues(iCount), iCount = 1, mBC_max_count )

         if ( iFile_read_err .ne. 0 ) then
            write (cMsg, '(A,A)')
     &      'Could not read file ',
     &      cBC_TmpFile_name(1:lnblnk(cBC_TmpFile_name))

         endif
      else
         write (cMsg, '(A,A,A)')
     &     'File ', cBC_TmpFile_name(1:lnblnk(cBC_TmpFile_name)),
     &     ' has been closed by another procedure!'
         iFile_read_err = 1
      endif

C----------------------------------------------------------------------
C     Were errors encountered?
C----------------------------------------------------------------------

      if ( iFile_read_err .ne. 0 ) then
         if ( autook ) then
            call edisp(iuout, cContext)
            call edisp(iuout, cMsg)
         else
            call UsrMsg (cContext, cMsg, 'W')
         endif
         stop 'get_BC_row(): Fatal error!'
      endif


      return
      end


C---------------  report_BC_data -------------------------------------
C
C ABSTRACT:
C
C report_BC_data reports the values of the boundary condition data to
C the XML reporting facility
C
C Inputs:
C
C   None
C
C Outputs
C
C   None
C
C----------------------------------------------------------------------
      subroutine report_BC_data()
      use h3kmodule
      implicit none

#include "bc_data.h"

C External functions.
      integer lnblnk

C Local variables
      logical bInitialized          ! initialization flag.
      data bInitialized /.false./

      integer iCount              ! counter

      character*124 cContext      ! contextual message


      real fBC_Value              ! value of boundary condition

      integer iNameLength, iUnitsLength ! Length of strings

C.....Current day (& fraction thereof)
      real fBld_Day

C----------------------------------------------------------------------
C     References:
C----------------------------------------------------------------------
      real fGet_BC_data_by_col  ! function returing interpolated values
                                ! for a given boundary condition

      real fConvert_bld_TS_to_DAY ! Function returning the time of day

C----------------------------------------------------------------------
C     Get current day in building domain
C----------------------------------------------------------------------
      fBld_Day = fConvert_bld_TS_to_DAY()

C-----------------------------------------------------------------------
C     Context buffer
C-----------------------------------------------------------------------

      write (cContext, '(A,A)') 'report_BC_data(): ',
     &        'Retrieving boundary condition data for H3KReports.'

C----------------------------------------------------------------------
C     Loop through boundary conditions
C----------------------------------------------------------------------
      if ( bBC_data_defined ) then

         do iCount = 1, iBC_col_count
C----------------------------------------------------------------------
C           Report data
C----------------------------------------------------------------------
C...........Report interpolated values: Step interpolation
            iNameLength = lnblnk(cBC_names(iCount))

            fBC_Value = fGet_BC_data_by_col( iCount,
     &                                       fBld_Day,
     &                                       iStep_Interp,
     &                                       cContext)
            call AddToReport(rvBndCndStpInt%Identifier,
     &         fBC_Value,
     &         cBC_names(iCount)(1:iNameLength))


C...........Report interpolated values: Step interpolation
            fBC_Value = fGet_BC_data_by_col( iCount,
     &                                       fBld_Day,
     &                                       iLinear_Interp,
     &                                       cContext)
            Call AddToReport(rvBndCndLnInt%Identifier,
     &         fBC_Value,
     &         cBC_names(iCount)(1:iNameLength))


         if ( .not. bInitialized ) then
            iUnitsLength = lnblnk(cBC_units(iCount))
C...........Store the variables' description
            Call AddToReportDetails(rvBndCndStpInt%Identifier,
     &         cBC_names(iCount)(1:iNameLength),
     &         'units',
     &         '(' // cBC_units(iCount)(1:iUnitsLength) // ')',
     &         'Boundary condition data: step-interpolated values')

            Call AddToReportDetails(rvBndCndLnInt%Identifier,
     &         cBC_names(iCount)(1:iNameLength),
     &         'units',
     &         '(' // cBC_units(iCount)(1:iUnitsLength) // ')',
     &         'Boundary condition data: linearly-interpolated values')

C..............For the last boundary condition, set initialization flag
               if ( iCount .eq. iBC_col_count ) bInitialized = .true.
         endif

         enddo

      endif

      return
      end

C---------------  cleanup_BC_data -------------------------------------
C
C ABSTRACT:
C
C cleanup_BC_data merely deletes the temporary file created by
C the boundary condition management facility.
C
C Inputs:
C
C   None
C
C Outputs
C
C   None
C
C----------------------------------------------------------------------

      subroutine cleanup_BC_data()
      implicit none
#include "bc_data.h"

C External functions.
      integer lnblnk

      logical bError
      logical bFile_Open
      character cFileName*72
      integer iFile_Close_Err
C.....Warning messages
      character*124 cMsg          ! message to be displayed to buffer
      character*124 cContext      ! contextual message

C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
C.....Simulator status
      common/spfl/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical spfileok,perok,tstepok,saveok,autook,exitok,startupok
C.....I/O
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

      bError = .false.

      write (cContext,'(A,A)')
     & 'Closing boundary condition temporary file ',
     &    cBC_TmpFile_name(1:lnblnk(cBC_TmpFile_name))

      inquire ( UNIT=iBC_TmpFile, Name=cFileName, OPENED=bFile_Open )

      if ( .not. bFile_Open ) then

         open ( UNIT=iBC_TmpFile,
     &          FILE=cBC_TmpFile_name
     &                (1:lnblnk(cBC_TmpFile_name)),
     &          ACCESS='DIRECT',
     &          RECL=iBC_col_count*4,
     &          FORM='UNFORMATTED',
     &          STATUS='REPLACE',
     &          IOSTAT=iFile_Close_Err)

      endif

      close (iBC_TmpFile, STATUS='Delete', IOSTAT=iFile_Close_Err)

      if ( iFile_Close_Err .ne. 0 ) then
         write ( cMsg, '(A)' )
     &     'File could not be closed!'
         bError = .true.
      endif

      if ( bError ) then
         if ( autook ) then
            call edisp(iuout, cContext)
            call edisp(iuout, cMsg)
         else
            call UsrMsg (cContext, cMsg, 'W')
         endif
      endif

      return
      end


