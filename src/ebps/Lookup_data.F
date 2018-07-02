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

C==========================Lookup_data.F=========================
C
C Date: November 23, 2006
C Author: Maria Mottillo, Natural Resources Canada
C
C This file contains subroutines and functions for managing a user-specified
C data table.
C
C General routines:
C
C process_Lookup_data_file: Reads a file that contains the
C data characteristics and data in ASCII format. Writes out a
C binary database containing the data records (binary file will
C be accessed by get* subroutines.
C
C iGet_Lookup_col_num: This function returns the column number that
C corresponds to a column's header.
C
C fGet_Lookup_value_by_col_num: This function returns data
C from a lookup table that corresponds to a user-specified
C reference value, reference column number and value column number.
C
C fGet_Lookup_value_by_col_name: This function returns data
C from a lookup table that corresponds
C to a user-specified reference value, reference column name 
C and value column name. This procedure is an interface to the 
C function fGet_Lookup_value_by_col_num which allows the column names
C (versus column numbers) to identify the reference and value columns.
C
C cleanup_Lookup_data: Deletes the temporary file created by the 
C lookup table facility.
C
C References:
C Alex Ferguson's source code for the Boundary Condition Definition
C facility in ESP-r.
C
C==================================================================
C
C
C-------------------------process_Lookup_data_file-----------------
C
C Abstract:
C process_Lookup_data_file parses a lookup data file, checks that the contents
C of the file are valid and writes a binary database containing the
C table of data.
C
C Inputs:
C
C iBase_file_num: Basic I/O file number; ASCII and binary files will
C                 be set relative to this number.
C
C Output:
C
C bFatal_Error: flag indicating that error was encountered
C
C--------------------------------------------------------------------


       subroutine process_Lookup_data_file(iBase_file_num,
     &                                     bAutook, 
     &                                     bFatal_error)
       implicit none

#include "lookup_data.h"

C External functions.     
      integer lnblnk

C Passed arguments
       integer iBase_file_num        ! basic I/O file number
       logical bAutook               ! flag for silent-running mode
       logical bFatal_error          ! fatal error flag

C ESP-r variables

       common/outin/iuout,iuin
       integer iuout, iuin      ! channels for writing messages to screen

C Local variables

       character*124 cMsg            ! message to be displayed to buffer
       character*124 cContext        ! contextual message

       character*248 cLnBuffer       ! line buffer
       character*248 cLnWords(124)   ! words on the line

       integer iASCII_file_num       ! ASCII temporary file number

       integer iFile_read_err        ! flag indicating that error was
                                     ! encountered while reading from a file
       integer iFile_write_err       ! flag indicating that error was encountered
                                     ! while writing to a file

       integer iDummy

       integer iMiscError            ! miscellaneous error

       integer iCol                  ! column counter

       integer iWordCount            ! number of words in a string

       logical bFile_Open            ! flag indicating a file is open

       logical bVersion_defined      ! flag indicating that the version number is defined
       logical bHeader_defined       ! flag indicating that the column headers are defined
       logical bUnits_defined        ! flag indicating that the column units are defined
       logical bData_defined         ! flag indicating that the table data are defined

       logical bParsing_data         ! flag indicating that parser is in the data
                                     ! section of the file

       real fLnData(iLookup_max_cols) ! line buffer containing row data

C References

       integer iEGetArrW             ! returns an array containing the words found in a string


C-------------------------------------------------------------------------
C Initialize
C-------------------------------------------------------------------------
       iASCII_file_num = iBase_file_num + 1
       iLookup_TmpFile = iBase_file_num + 81


C Counters
       iLookup_col_count = 0
       iLookup_row_count = 0

C File reading/parsing flags
       bVersion_defined = .false.
       bHeader_defined  = .false.
       bUnits_defined   = .false.
       bData_defined    = .false.
       bParsing_data    = .false.

C Error flags
       iMiscError      = 0
       iFile_read_err  = 0
       iFile_write_err = 0


C-------------------------------------------------------------------------
C Contextual message
C-------------------------------------------------------------------------

       write(cContext, '(A,A)') 'Parsing lookup table data file:',
     &   cLookup_data_file_name(1:lnblnk(cLookup_data_file_name))
       call edisp (iuout, ' ')
       call edisp (iuout, cContext)
       cContext = ' '


C-------------------------------------------------------------------------
C Open ASCII file
C-------------------------------------------------------------------------

C Close the file, if it is open
      call erpfree (iASCII_file_num, iFile_read_err)

      call EFOPSEQ(iASCII_file_num,
     &             cLookup_data_file_name,
     &             0,
     &             iFile_read_err)

C Check if file found/opened successfully
      if ( iFile_read_err .eq. 0  ) then

C File found/opened successfully, therefore do nothing.

      else

C Error finding/opening file; write warning message.
         write(cMsg,'(A,A,A)') ' File ',
     &      cLookup_data_file_name(1:lnblnk(cLookup_data_file_name)),
     &      ' could not be read!'

C Set error flag.
         bFatal_Error = .true.

C If running silently, stop simulator. Otherwise, warn user.
         if ( bAutoOK ) then
            call edisp ( iuout, cMsg )
         else
            CALL UsrMsg (cContext, cMsg, 'W')
         endif ! matches 'if ( .not. bAutoOK ...'

      endif    ! matches 'if ( iFile_read_err .eq. 0...'

C-------------------------------------------------------------------------
C Open up a temporary binary file for direct access
C-------------------------------------------------------------------------

      inquire ( UNIT=iLookup_TmpFile, OPENED=bFile_Open )
C Warn if I/O unit number is in use
      if ( bFile_Open ) then
         close ( UNIT=iLookup_TmpFile )
         write (cMsg,'(A,A,I2,A)')
     &     ' Warning: Unit number for temporary ',
     &     ' Lookup data file (', iLookup_TmpFile, ') was in use!'
         if ( bAutoOK ) then
            call edisp(iuout, cMsg)
         else
            call UsrMsg (cContext, cMsg, 'W')
         endif

      endif

      open ( UNIT=iLookup_TmpFile,
     &       FILE=cLookup_TmpFile_name
     &             (1:lnblnk(cLookup_TmpFile_name)),
     &       ACCESS='DIRECT',
     &       RECL=iLookup_max_cols*4,
     &       FORM='UNFORMATTED',
     &       STATUS='REPLACE',
     &       IOSTAT=iFile_write_err)

      
      if ( iFile_write_err .eq. 0 ) then
C File opened successfully, do nothing

      else
C File could not be opened
         bFatal_Error   = .true.
C.Let user know
         write (cMsg,'(A,A,A)')
     &     ' Temporary Lookup table data file (',
     &        cLookup_TmpFile_name
     &             (1:lnblnk(cLookup_TmpFile_name)),
     &     ') could not be opened!'

         if ( bAutoOK ) then
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
         call LStripc (iASCII_file_num,
     &                 cLnBuffer,
     &                 0,
     &                 iDummy,
     &                 0,
     &                 'Lookup table file header',
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
C        Is file a valid lookup table data file? First record *MUST* be
C        '*lookup_data_file'
C----------------------------------------------------------------------
         if (  cLnWords(1)(1:17) .ne. '*lookup_data_file' ) then

C----------------------------------------------------------------------
C           File is not a valid lookup table data file.
C----------------------------------------------------------------------
            write (cMsg,'(A,A)')
     &         cLookup_data_file_name(1:lnblnk(cLookup_data_file_name)),
     &         ' is not a valid lookup table data file! '
            if ( bAutoOK ) then
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
               cLnBuffer = ' '
C..............Get line & strip comments
               call LStripc (iASCII_file_num,
     &                       cLnBuffer,
     &                       0,
     &                       iDummy,
     &                       0,
     &                       'Lookup table data file',
     &                       iFile_read_err)

C..............Split words
               iWordCount = iEGetArrW ( cLnBuffer, cLnWords )
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
     &                  fLookup_file_version

                  if ( iMiscError .ne. 0 ) then
C....................Version is not a real.
                     write (cMsg,'(A,A)')
     &                  ' Could not read version number: ',
     &                  cLnWords(3)(1:lnblnk(cLnWords(3)))

                     if ( bAutoOK ) then
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                  else

C....................Version number is a real. Is it supported?
                     if ( fLookup_file_version .lt. 0. .and.
     &                    fLookup_file_version .gt. 0.11 ) then
C.......................Version # is unsupported
                        write (cMsg,'(A,F4.2,A)')
     &                     ' File version number (',
     &                     fLookup_file_version,
     &                     ') is unsupported.'

                        if (bAutoOK) then
                           call edisp(iuout, cMsg)
                        else
                           call UsrMsg (cContext, cMsg, 'W')
                        endif

                     else

C.......................Version number defined successfully
                        bVersion_defined = .true.

                     endif

                  endif ! <- matches 'if ( iMiscError .ne. 0 ) then'


               elseif ( cLnWords(1)(1:12) .eq. '*data_header' ) then
C----------------------------------------------------------------------
C              Record describes column names within table
C----------------------------------------------------------------------

                  bHeader_defined = .true.
                  
                  if ( ( bUnits_defined .or. bData_defined ) .and.
     &                 iWordCount - 1 .ne. iLookup_col_count ) then

                        write (cMsg,'(A,I2,A,I2,A)')
     &                  ' Mismatch between # of header columns (',
     &                  iWordCount,') and # of units/data columns (',
     &                  iLookup_col_count,').'

                     if ( bAutoOK ) then
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                     bHeader_defined = .false.

                  elseif ( .not. ( bUnits_defined .or.
     &                             bData_defined       ) ) then
     
                     iLookup_col_count = iWordCount - 1

                  endif                 
                     
                  if ( bHeader_defined ) then
                  
                     bHeader_defined = .true.
                     do iCol = 1, iLookup_col_count
                        write(cLookup_col_names(iCol), '(A)')
     &                     cLnWords(iCol+1)
     &                           (1:lnblnk(cLnWords(iCol+1)))
                     enddo
                  endif

               elseif ( cLnWords(1)(1:11) .eq. '*data_units' ) then
C----------------------------------------------------------------------
C                 Record describes lookup table column units.
C----------------------------------------------------------------------
                  bUnits_defined = .true.
                  if ( ( bHeader_defined .or. bData_defined ) .and.
     &                   iWordCount - 1 .ne. iLookup_col_count ) then

                     write (cMsg,'(A,I2,A,I2,A)')
     &                  ' Mismatch between # of unit columns (',
     &                  iWordCount,') and # of header/data columns (',
     &                  iLookup_col_count,').'

                     if ( bAutoOK ) then
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif

                     bUnits_defined = .false.

                  elseif ( .not. ( bHeader_defined .or.
     &                             bData_defined        ) ) then 
                     iLookup_col_count = iWordCount - 1
                  endif

                  if ( bUnits_defined ) then 
                
                     do iCol = 1, iLookup_col_count
                         write(cLookup_col_units(iCol), '(A)')
     &                     cLnWords(iCol+1)
     &                           (1:lnblnk(cLnWords(iCol+1)))
                     enddo
                  endif

               elseif ( cLnWords(1)(1:11) .eq. '*data_start' ) then
C----------------------------------------------------------------------
C                 Record denotes the start of lookup table data
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
C                 Record contains lookup table data.
C----------------------------------------------------------------------

C.................Increment record counter
C.................( note: counter initialized as 0 )
                  iLookup_row_count = iLookup_row_count + 1

                  if ( iLookup_row_count .eq. 1 ) then
C----------------------------------------------------------------------
C                    This is the first data record.                                                                          
C----------------------------------------------------------------------

C....................If the header or units have not been defined,
C....................save the number for columns 
                     if ( .not. ( bHeader_defined .or.
     &                     bUnits_defined ) )
     &                      iLookup_col_count = iWordCount
                       
                  endif ! <- Matches  'if ( iLookup_row_count .eq. 1 ) then

C----------------------------------------------------------------------
C                 Collect values from ascii input file                                                                       
C----------------------------------------------------------------------                  
C.................Check that number of columns is ok
                  if ( iWordCount .ne. iLookup_col_count ) then
C....................Mismatch in column count
                     write (cMsg,'(A,I2,A,I2,A,I2,A)')
     &                  ' Mismatch between # of data columns (',
     &                  iWordCount,
     &                  ') and # of header/unit columns (',
     &                  iLookup_col_count,') on data row ',
     &                  iLookup_row_count,'.'

                     if ( bAutoOK ) then
                        call edisp(iuout, cMsg)
                     else
                        call UsrMsg (cContext, cMsg, 'W')
                     endif
                     
C....................Data has been incorrectly defined                     
                     bData_defined = .false.
                     
                  endif ! <- Matches 'if ( iWordCount .ne. iLookup_col_count...
                  
C.................If column count is ok, read values
                  if ( bData_defined ) then

                     do iCol = 1, iLookup_col_count
                        read (cLnWords(iCol),*, IOSTAT=iMiscError)
     &                  fLnData(iCol) 

                        if ( iMiscError .ne. 0 ) then

                           bData_defined = .false. 
                           
                           write (cMsg,'(A,A,A,I2,A)')
     &                        ' Could not convert data record (',
     &                        cLnWords(iCol)
     &                        (1:lnblnk(cLnWords(iCol))),
     &                        ') on data row ', iLookup_row_count, '.'
     
                           if ( bAutoOK ) then
                              call edisp(iuout, cMsg)

                           else
                              call UsrMsg (cContext, cMsg, 'W')

                           endif

                        endif ! <- Matches ' if ( iMiscError .ne. 0 ) then
                        
                     enddo    ! <- Matches 'do iCol = 1, iLookup_col_count'
                     
                  endif       ! <- Matches 'if ( bData_defined ) then

C----------------------------------------------------------------------
C                 Write values to temp file                                                                          
C----------------------------------------------------------------------
                  if ( iFile_write_err .eq. 0 ) then

                     write ( UNIT=iLookup_TmpFile,
     &                       REC=iLookup_row_count,
     &                       IOSTAT=iMiscError )
     &                     ( fLnData(iCol), iCol=1, iLookup_max_cols )
     
                  endif ! <- Matches 'if ( iFile_write_err .eq. 0 ) then
                  
               else
C----------------------------------------------------------------------
C                 Unknown record type: Warn user and move on.
C----------------------------------------------------------------------
                  write (cMsg,'(A,A)')
     &              ' Warning - unknown record type:',
     &               cLnWords(1)(1:lnblnk(cLnWords(1)))

                  if ( bAutoOK ) then
                     call edisp(iuout, cMsg)
                  else
                     call UsrMsg (cContext, cMsg, 'W')
                  endif
                  
               endif ! <- Matches 'if ( cLnWords(1)(1:8) .eq. '*version'...

            enddo !<- Matches 'do while ( iFile_read_err .eq. 0 )'

         endif ! <- matches  ' if (  cLnWords(1:17)(1) .eq. ...'

      endif  ! <- matches 'if ( .not. bFatal_Error ) then'


C----------------------------------------------------------------------
C     Close ascii file
C----------------------------------------------------------------------
      close ( UNIT=iASCII_file_num )


C----------------------------------------------------------------------
C     Check that essential data was collected
C----------------------------------------------------------------------
      if ( .not. bHeader_defined    .or.
     &     .not. bData_defined      .or.
     &     .not. bUnits_defined     .or.
     &     .not. bVersion_defined        ) bFatal_Error = .true.
     

C----------------------------------------------------------------------
C     Were errors encountered?
C----------------------------------------------------------------------

      if ( bFatal_Error ) then
C........Yes. Warn user and delete temporary file.
         close ( UNIT=iLookup_TmpFile, STATUS='DELETE')
         call edisp(iUout, ' Lookup table data file was not processed'
     &         // ' successfully.')

         if ( bAutoOK ) then
            
            stop 'Fatal Error!'
             
         endif                

      else
      
         call edisp(iUout, ' Lookup table data file was processed'
     &         // ' successfully.')
      endif 


      return
      end

C----------------------------iGet_Lookup_col_num--------------------
C
C Abstract:
C iGet_Lookup_col_num returns the column number that corresponds to
C the column header specified by the user
C
C Inputs:
C
C cCol_name: Column name for which column number is to be determined
C 
C
C Output:
C
C iCol_num: Columnn number corresponding to column name
C
C--------------------------------------------------------------------

       integer function iGet_Lookup_col_num( cCol_name,
     &                                        cContext   )

       implicit none
#include "lookup_data.h"

C Passed arguments

       character*248 cCol_name  ! column header/name
       character*124 cContext   ! contextual message

C External functions.     
       integer lnblnk

C ESP-r commons

       common/outin/iuout,iuin
       integer iuout, iuin      ! channels for writing messages to screen

C Local variables

       character*124 cMsg       ! string used for feedback messages

       integer iCount           ! counter
       integer iCol_number      ! column number

       logical bFatal_error     ! flag indicating fatal error has occurred
       logical bCol_found       ! flag indicating column found

C Initialize

       bFatal_error = .false.
       bCol_found   = .false.


C Check that a lookup table exists

      if ( .not. bLookup_data_defined ) then
         write (cMsg,'(A)') 'Lookup table data have not been defined!'
         bFatal_Error = .true.
      else 

C Loop through lookup table and find column number associated with name

         do iCount = 1, iLookup_col_count

            if ( cCol_name .eq. cLookup_col_names(iCount) ) then

                iCol_number = iCount
                bCol_found = .true.

            endif

         enddo

C Check that column has been defined

         if ( .not. bCol_found ) then

            write (cMsg,'(A,A,A)') 'Lookup table column ',
     &         cCol_name(1:lnblnk(cCol_name)), ' has not been defined!'
            bFatal_Error = .true.

         else

C----------------------------------------------------------------------
C           Return column number
C----------------------------------------------------------------------      
            iGet_Lookup_col_num = iCol_number

         endif
      endif 

      if ( bFatal_Error ) then
         call edisp(iuout, cContext)
         call edisp(iuout, cMsg)
         stop 'fGet_Lookup_col_num(): Fatal error!'
      endif

      return
      end


C----------------------fGet_Lookup_value_by_col_num-----------------
C
C Abstract:
C fGet_Lookup_value_by_col_num returns data from a lookup table that corresponds
C to a user-specified reference value, reference column number 
C and value column number.
C
C Inputs:
C iCol_ref : Reference column number
C iCol_value: Value column number
C fRef_value: Reference value
C iInterp_method: method for interpolation
C                 1 -> step-wise
C                 2 -> linear
C
C Outputs
C Value from lookup table corresponding reference value
C
C---------------------------------------------------------------------

      real function fGet_Lookup_value_by_col_num(
     &                                           iCol_ref,
     &                                           iCol_value,
     &                                           fRef_value,
     &                                           iInterp_method,
     &                                           cContext )
      implicit none
#include "lookup_data.h"
#include "bc_data.h"


C Passed arguments
      integer iCol_ref        ! reference column number
      integer iCol_value      ! value column number
      real fRef_value         ! reference value
      integer iInterp_method  ! interpolation method
      character*124 cContext  ! contextual message

C External functions.     
      integer lnblnk

C ESP-r variables

      common/outin/iuout,iuin
      integer iuout, iuin     ! channels for writing messages to screen

C Simulator status variables
      common/spfl/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      logical spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok


C Local variables

      character*124 cMsg      ! string used for feedback messages

      character*72 cFileName  ! temporary buffer for file name

      integer iCount          ! counters
      integer jCount

      integer iFile_read_err  ! error flag

      logical bFatal_Error    ! flag indicating error has occurred
      logical bFile_Open
      logical bValue_found    ! flag indicating value found in lookup table
      logical bClose

      real fTempRow(iLookup_max_cols)        ! temporary array containing rows of data
      real fRef_col_data(iLookup_max_rows)   ! array containing reference column data
      real fValue_col_data(iLookup_max_rows) ! array containing value column data

      real fGet_value         ! value in lookup table


C Initialize

       bFatal_error = .false.


C Check that lookup table data file has been defined

       if ( .not. bLookup_data_defined ) then
         write (cMsg,'(A)') 'Lookup table data have not been defined!'
         bFatal_Error = .true.
       endif 

C Check that supplied column numbers are within bounds and not equal

        if ( bLookup_data_defined .and.
     &       ( ( iCol_ref .le. 0 ) .or. 
     &         ( iCol_value .le. 0 ) ) ) then 
            write (cMsg,'(A,I3,I3,A)')
     &      'Specified columns (', iCol_ref,iCol_value,
     &      ') must be greater than zero!'
            bFatal_Error = .true.
        elseif ( bLookup_data_defined .and.
     &       ( ( iCol_ref .gt. iLookup_col_count ) .or. 
     &         ( iCol_value .gt. iLookup_col_count ) ) ) then 
            write (cMsg, '(A,I2,I2,A,I2,A)')
     &      'Columns (', iCol_ref, iCol_value,
     &      ' ) were requested, but only ',
     &      iLookup_col_count,' columns have been defined.'
            bFatal_Error = .true. 
        elseif ( bLookup_data_defined .and.
     &          ( iCol_ref .eq. iCol_value ) ) then
            write (cMsg, '(A,I2,A,I2,A)')
     &      'Reference column ', iCol_ref,
     &      'and value column ', iCol_value,
     &      'cannot be equal!'
            bFatal_Error = .true.

C Check that interpolation scheme is supported
        elseif ( bLookup_data_defined .and.
     &             iInterp_method .ne. iStep_Interp .and.
     &             iInterp_method .ne. iLinear_Interp ) then
            write (cMsg,'(A,i2,A)')
     &     'Specified interpolation method (', iInterp_method,
     &     ') is not supported!'
            bFatal_Error = .true.               
        endif 

        if ( .not. bFatal_Error ) then


C Check that temporary file exists

           inquire ( UNIT=iLookup_TmpFile, NAME=cFileName,
     &               OPENED=bFile_Open )


           if ( bFile_Open .and.
     &          cFileName .eq. cLookup_TmpFile_name ) then

C Read rows of data and save reference and value columns in arrays

              do jCount = 1, iLookup_row_count

                 read( UNIT = iLookup_TmpFile,
     &                 REC = jCount,
     &                 IOSTAT = iFile_read_err )
     &                 ( fTempRow(iCount), iCount = 1,iLookup_max_cols )

                 if ( iFile_read_err .ne. 0 ) then

                   write (cMsg, '(A,A)')
     &            'Could not read file ',
     &             cLookup_TmpFile_name(1:lnblnk(cLookup_TmpFile_name))

                   bFatal_Error = .true.

                 endif 

                 if ( .not. bFatal_Error ) then
                    fRef_col_data(jCount) = fTempRow(iCol_ref)
                    fValue_col_data(jCount) = fTempRow(iCol_value)

                 endif

              enddo

           else

              write (cMsg, '(A,A,A)')
     &        'File ',
     &        cLookup_TmpFile_name(1:lnblnk(cLookup_TmpFile_name)),
     &        ' has been closed by another procedure!'

              bFatal_error = .true.

           endif      ! ... bFile_Open .and.....


        endif           !.... .not. bFatal_Error.......



C Check that reference value requested is within the range of values provided 
C in the reference column

C Note: It is assumed that the values provided in the reference column are
C in ascending order.

        if ( .not. bFatal_Error ) then

            if ( fRef_value .lt. fRef_col_data(1) .or.
     &           fRef_value .gt. fRef_col_data(iLookup_row_count) ) then

                 write(cMsg, '(A,A)')
     &           'The reference value provided is not within the ',
     &           'range of values provided in the lookup table!'

                 bFatal_error = .true.

            else

C Find upper & lower range, then find value

                 iCount = 0
                 bValue_found = .false.
                 do while ( .not. bValue_found .and.
     &                      iCount .lt. iLookup_max_rows )
                     iCount = iCount + 1


                     if ( fRef_value .ge. fRef_col_data(iCount) .and.
     &                  fRef_value .le. fRef_col_data(iCount + 1) ) then


                          if ( iInterp_method .eq. iStep_Interp ) then
C........................ Step interpolation
                             call eclose ( fRef_value,
     &                                     fRef_col_data(iCount+1),
     &                                     0.01,
     &                                     bClose )
                             if ( bClose )then
                                 fGet_value = fValue_col_data(iCount+1)
                             else
                                 fGet_value = fValue_col_data(iCount)
                             endif
                          elseif ( iInterp_method .eq.
     &                                  iLinear_Interp ) then
C......................... Linear interpolation
                             fGet_value = fValue_col_data(iCount) +
     &                       ( ( fRef_value - fRef_col_data(iCount) ) * 
     &                         ( fValue_col_data(iCount+1) - 
     &                                 fValue_col_data(iCount) ) /
     &                         ( fRef_col_data(iCount+1) -
     &                                 fRef_col_data(iCount) ) )
                          endif

                          bValue_found = .true.

                     endif    ! ...fRef_value .ge. fRef_col_data(iCount) .and. ....

                 enddo

            endif       ! ... fRef_value .lt. fRef_col_data(1) .or.....



        endif           !.... .not. bFatal_Error.......


        if ( .not. bValue_found ) then

            write(cMsg, '(A)')
     &           'Could not find value in lookup table!'
            bFatal_Error = .true.

        else 

            fGet_Lookup_value_by_col_num = fGet_value

        endif      



C Verify if errors encountered

        if ( bFatal_Error ) then
         
           if ( autook ) then
              call edisp(iuout, cContext)
              call edisp(iuout, cMsg)
           else
              call UsrMsg (cContext, cMsg, 'W')
           endif

           stop 'fGet_Lookup_value_by_col_num(): Fatal Error!'
         
        endif



        return
        end


C----------------------fGet_Lookup_value_by_col_name-----------------
C
C Abstract:
C fGet_Lookup_value_by_col_name returns data from a lookup table that corresponds
C to a user-specified reference value, reference column name 
C and value column name. This procedure is an interface to the 
C function fGet_Lookup_value_by_col_num which allows the column names
C (versus column numbers) to identify the reference and value columns.
C
C Inputs:
C cCol_ref : Reference column name
C cCol_value: Value column name
C fRef_value: Reference value
C iInterp_method: method for interpolation
C                 1 -> step-wise
C                 2 -> linear
C
C Outputs
C Value from lookup table corresponding to reference value
C
C---------------------------------------------------------------------

      real function fGet_Lookup_value_by_col_name(
     &                                           cCol_ref,
     &                                           cCol_value,
     &                                           fRef_value,
     &                                           iInterp_method,
     &                                           cContext )
      implicit none
#include "lookup_data.h"
#include "bc_data.h"


C Passed arguments
      character*248 cCol_ref     ! reference column name
      character*248 cCol_value   ! value column name
      character*124 ctext        ! context message
      real fRef_value            ! reference value
      integer iInterp_method     ! interpolation method
      character*124 cContext     ! contextual message

C ESP-r variables

      common/outin/iuout,iuin
      integer iuout, iuin     ! channels for writing messages to screen

C Local variables

      integer iCol_ref        ! reference column number
      integer iCol_value      ! value column number


C References

      integer iGet_Lookup_col_num
      real fGet_Lookup_value_by_col_num


C Get column numbers

       ctext = 'Getting ref column number'

       iCol_ref = iGet_Lookup_col_num( cCol_ref,
     &                                 ctext )

       ctext = 'Getting value column number'

       iCol_value = iGet_Lookup_col_num( cCol_value,
     &                                   ctext )

C Get value from lookup table


       fGet_Lookup_value_by_col_name = fGet_Lookup_value_by_col_num(
     &                               iCol_ref,
     &                               iCol_value,
     &                               fRef_value,
     &                               iInterp_method,
     &                               cContext )



       return
       end


C----------------------cleanup_Lookup_data-----------------
C
C Abstract:
C cleanup_Lookup_data deletes the temporary file created by
C the lookup table facility.
C
C---------------------------------------------------------------------

       subroutine cleanup_Lookup_data()
       implicit none
#include "lookup_data.h"

C External functions.     
       integer lnblnk

C ESP-r variables

       common/outin/iuout,iuin
       integer iuout, iuin     ! channels for writing messages to screen

C Simulator status 
       common/spfl/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
       logical spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok

C Local variables

       integer iFile_Close_Err

       logical bError                  
       logical bFile_Open

       character*72   cFileName
       character*124  cContext        
       character*124  cMsg           



       bError = .false.
      
       write (cContext,'(A,A)')
     &    'Closing lookup table temporary file ',
     &    cLookup_TmpFile_name(1:lnblnk(cLookup_TmpFile_name))
     
       inquire ( UNIT=iLookup_TmpFile, Name=cFileName,
     &                               OPENED=bFile_Open )

       if ( cFileName .ne. cLookup_TmpFile_name ) then
          write ( cMsg, '(A)' )
     &      'Temporary file no longer exists!'
          bError = .true. 
       elseif ( .not. bFile_Open ) then


         open ( UNIT=iLookup_TmpFile,
     &          FILE=cLookup_TmpFile_name
     &                (1:lnblnk(cLookup_TmpFile_name)),
     &          ACCESS='DIRECT',
     &          RECL=iLookup_col_count*4,
     &          FORM='UNFORMATTED',
     &          STATUS='REPLACE',
     &          IOSTAT=iFile_Close_Err)

       endif


       close (iLookup_TmpFile, STATUS='Delete', IOSTAT=iFile_Close_Err)

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
