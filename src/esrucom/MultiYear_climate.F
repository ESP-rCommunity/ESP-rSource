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
C================== MultiYear_climate.F ===============================
C
C Date: May 5, 2005
C Author: Alex Ferguson
C Copyright: Natural Resources Canada
C
C This file contains procedures for handling climate data during
C multi-year simulations.
C
C GENERAL ROUTINES:
C
C Parse_MY_clm_db: Parses the multi-year climate index file
C
C Check_MY_clm_db: Performs error checking on a multi-climate db.
C
C MY_clm_db_menu:  Displays a menu allowing configuration of
C                  the multi-year database
C
C Spec_MY_clm_db:  Asks a user for a multi-year database file
C
C Save_MY_clm_db:  Controls saving a multi-year database file 
C
C Sort_MY_clm_db:  Sorts the records within a multi-year db.
C
C Fill_MY_clm_db:  Checks the records within a multi-year db
C                  for continuity, and fills in missing records
C
C Prune_MY_clm_db: Removes duplicate records from the database
C
C Write_MY_clm_db: Writes the database to disk 
C
C======================================================================

C------------------ Parse_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This procedure parses the multi-year climate index file. It:
C
C  - Checks if the multiyear climate file has been properly
C    defined.
C  - Saves the multi-year climate database in memory
C
C Inputs:
C
C  iFile: current file index.
C  AutoOK: logical flag indicating that the the simulator is
C    running in silent running mode
C
C Outputs:
C    bCriticalErr     ! flag incidating a critical error has been encountered
C
C----------------------------------------------------------------------
      subroutine Parse_MY_clm_db( iFile_num,
     &                            bAutoOK,
     &                            bCriticalErr )

      implicit none
#include "building.h"     
#include "MultiYear_simulations.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iFile_num        ! climate file number used by bps
      logical bAutoOK          ! flag for silent-running mode
      logical bCriticalErr     ! flag incidating a critical
                               ! error has been encountered

C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iFile_read_err   ! flag indicating that error was
                               ! encountered

                               
      character*248 cLnBuffer     ! Line buffer
      character*248 cLnWords(124) ! words on the line
      integer iWordCount          ! # of words in a string
      integer iDummy
      
      integer iMiscError          ! Miscellaneous error

      logical bParsingClmRecords  ! flag indicating that parser
                                  ! is in the '*rec' section of the
                                  ! file

      integer iYr_buf             ! year buffer
      
      
      !logical bSorted, bInOrder   ! flags for sorting loop
      !logical bClmFileExists      ! Flag indicating climate
                                  ! file has been found

C.....Flags for file status
      !logical bContigious, bNoDuplicates, bNoOrderChanged
                                  

      character*124 cMsg          ! message to be displayed to buffer
      character*124 cContext      ! contextual message

      !logical bDone               ! Loop control
      
C----------------------------------------------------------------------      
C     References
C----------------------------------------------------------------------
      integer iEGetArrW        ! returns an array containing the
                               ! words found in a string. 

C----------------------------------------------------------------------
C     Initialize
C--------------------------------------------------------------------
      bParsingClmRecords = .false.
      bCriticalErr       = .false.
      iFile_read_err     = 0
      iMY_clm_file_count = 0

      call edisp (iuout,' ')

      write (cContext, '(A,A)') ' Parsing multi-year climate db: ',
     &   cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))
           
C----------------------------------------------------------------------
C     Open ASCII multi-year climate database
C----------------------------------------------------------------------

C.....Close the file, if it is open
      call erpfree (iFile_num, iFile_read_err)
    
      call EFOPSEQ(iFile_num,cMY_climate_db_name,0,iFile_read_err)

C.....Was file found/opened successfully?
      if ( iFile_read_err .eq. 0  ) then

C........YES! Do nothing.
        continue
         
      else

C........NO! Write warning message.
        write(cMsg,'(A,A,A)') ' File ',
     &    cMY_climate_db_name(1:lnblnk(cMY_climate_db_name)),
     &    ' could not be read!'

C........Set parse error flag.
        bCriticalErr = .true.
     
C........If running silently, stop simulator. Otherwise, warn user.
        if ( bAutoOK ) then
          call edisp ( iuout, cContext)
          call edisp ( iuout, cMsg )

        else
          CALL UsrMsg (cContext, cMsg, 'W')

        endif ! matches 'if ( .not. bAutoOK ...'

      endif    ! matches 'if ( iFile_read_err .eq. 0...'


C----------------------------------------------------------------------
C     If file was opened successfully, start parsing
C----------------------------------------------------------------------
      if ( .not. bCriticalErr ) then
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
        call LStripc (iFile_num, cLnBuffer, 0, iDummy, 0,
     &                'MY climate header',
     &                iFile_read_err)
C----------------------------------------------------------------------
C        Get space/comma/tab separated words
C          -> iEGetArrW: returns the number of words encountered
C             and a character array containing the words. Synopsys
C             # of words = ( string, array of words)
C          
C----------------------------------------------------------------------
        iWordCount = iEGetArrW ( cLnBuffer, cLnWords )

C----------------------------------------------------------------------
C        Is the file a valid MY_Climate_db? First record *MUST*
C        be '* MY_climate_db'
C----------------------------------------------------------------------
        if ( cLnWords(1)(1:1) .eq. '*' .and.
     &       cLnWords(2)(1:13) .eq. 'MY_climate_db' ) then

C----------------------------------------------------------------------
C           File is a valid MY_Climate_db. Loop through file and read
C           contents
C----------------------------------------------------------------------
          do while ( iFile_read_err .eq. 0 .and. .not. bCriticalErr ) 

            call LStripc (iFile_num, cLnBuffer, 0, iDummy, 0,
     &        'MY climate db records',iFile_read_err)
     
            iWordCount = iEGetArrW ( cLnBuffer, cLnWords )
                     
C----------------------------------------------------------------------
C              Does line denote file version?
C----------------------------------------------------------------------
            if ( iWordCount .eq. 3 .and.
     &           cLnWords(1)(1:1) .eq. '*' .and.
     &           cLnWords(2)(1:7) .eq. 'VERSION' ) then

C.................Convert version # to real
              read (cLnWords(3),*, IOSTAT=iMiscError)
     &              fMY_version_number
                  
              if ( iMiscError .ne. 0 ) then
C....................Version is not a real.
                bCriticalErr = .true.
                write (cMsg,'(A,A)')
     &            ' Could not read version number: ',
     &            cLnWords(3)(1:lnblnk(cLnWords(3)))

                if ( bAutoOK ) then
                  call edisp(iuout, cContext)
                  call edisp(iuout, cMsg)
                else
                  call UsrMsg (cContext, cMsg, 'W')
                endif 
                  
              else  

C....................Version number is a real. Is it supported?
                if ( fMY_version_number .lt. 0. .and.
     &               fMY_version_number .gt. 0.11 ) then
C.......................Version # is unsupported
                  bCriticalErr = .true.
                  write (cMsg,'(A,F4.2,A)')
     &              ' File version number (',fMY_version_number,
     &              ') is unsupported.'
     
                  if (bAutoOK) then
                    call edisp(iuout, cContext)
                    call edisp(iuout, cMsg)
                  else
                    call UsrMsg (cContext, cMsg, 'W')
                  endif 
 
                endif 

              endif 
                                                  
C----------------------------------------------------------------------
C              Does line denote start / stop of clm file records?
C----------------------------------------------------------------------
            elseif ( iWordCount .eq. 2 .and.
     &               cLnWords(1)(1:19) .eq. '*my_climate_records'
     &             ) then

C.................Set / reset record parsing flag
              if ( cLnWords(2)(1:5) .eq. 'begin' ) then
                bParsingClmRecords = .true.
              elseif ( cLnWords(2)(1:3) .eq. 'end' ) then
                     
                bParsingClmRecords = .false.
              else

C....................Database cannot be read.                  
                bCriticalErr = .true.
                  
                call edisp( iUout, cContext)
                call edisp( iUout, 
     &            ' Error reading climate record marker: ')
                call edisp( iUout, '  -> '
     &            // cLnBuffer( 1:lnblnk(cLnBuffer(1:115))) )
                     
                if ( .not. bAutoOK ) then

                  call UsrMsg(cContext, 'Parsing error!', 'W' )
               
                endif 
                                
              endif 

     
C----------------------------------------------------------------------
C              Are we expecting a clm file record?
C----------------------------------------------------------------------
            elseif ( bParsingClmRecords ) then

C.................Does record appear to be valid?
              if ( iWordCount .eq. 3 .and.
     &             cLnWords(1)(1:4) .eq. '*rec' .and.
     &             iMY_clm_file_count + 1 .le. iMax_Climate_Years
     &           ) then


C....................Convert year to integer
                read (cLnWords(2),*, IOSTAT=iMiscError) iYr_buf
     
C....................Increment clm file counter
                if ( iMiscError .ne. 0 ) then

                  call edisp (iuout, ' ')
                  call edisp (iUout, cContext)
                  call edisp (iUout,
     &              ' Warning: *rec year not parsed correctly:')
                  call edisp( iUout, '  -> '
     &              // cLnBuffer( 1:lnblnk(cLnBuffer(1:115))) )
                  call edisp (iUout, ' This is a non-fatal error;'
     &              //  ' record will be ignored.')

                else
                     
                  iMY_clm_file_count = iMY_clm_file_count + 1     

C.......................Assign climate year
                  iMY_clm_file_years(iMY_clm_file_count) = iYr_buf
                        
C.......................Save climate file location
                  cMY_clm_file_names ( iMY_clm_file_count ) =
     &                     cLnWords(3)(1:lnblnk(cLnWords(3)))
                endif
              else

                  
C....................Warn user that record will be ignored                     
                call edisp (iuout, ' ')
                call edisp (iUout, cContext)
                call edisp (iUout,
     &            ' Warning: *rec record not parsed correctly:')
                call edisp( iUout, '  -> '
     &            // cLnBuffer( 1:lnblnk(cLnBuffer(1:115))) )
                call edisp (iUout,
     &           ' This is a non-fatal error; record will be ignored.')

              endif
     
            endif 

          enddo 
     
        else

C----------------------------------------------------------------------
C           File is not a valid MY_Climate_db. Error trap as necessary.
C----------------------------------------------------------------------
          call edisp (iuout, ' ')
          write (cMsg, '(A,A,A)') ' Error: ',
     &           cMY_climate_db_name( 1:lnblnk( cMY_climate_db_name )),
     &           ' is not a valid multi-year climate database.' 

C...........If running silently, stop simulator. Otherwise, issue
C...........warning.    
          if ( bAutoOK ) then
            
            call edisp (iuout, cMsg )
           
          else

            call UsrMsg (' ', cMsg, 'W')

          endif 
            
          bCriticalErr = .true.

        endif      
         
      endif

C.....Close myclm file
      call erpfree (iFile_num, iFile_read_err)


C----------------------------------------------------------------------
C     Were errors encountered?
C----------------------------------------------------------------------

      if ( bCriticalErr ) then

        call edisp(iUout, 
     &    ' Multi-year climate db was not parsed correctly.')

        if ( bAutoOK ) then

          stop 'Fatal Error!'
             
        endif

      else 
        call edisp(iUout, 
     &    ' Multi-year climate db was parsed successfully.')
      endif 
      
      return
      end



C------------------ Check_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This procedure checks that the multi-year climate database
C data stored in memory is correct.
C
C  - Checks if the listed year records are contigious and
C    in order, and
C  - Checks if the clm files exist. 
C
C Inputs:
C
C  - bAutoOK: status of simulatior (silent/interactive modes)
C  - cAction: action to be taken if recoverable errors are
C    found.
C
C Outputs:
C
C  - bCriticalErr: flag indicating if a critical error was
C    encountered.
C
C----------------------------------------------------------------------      
      subroutine Check_MY_clm_db(bAutoOK,cActionPass,bCriticalErr)
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"      

C External functions.     
      integer lnblnk

C Passed arguements
      logical bCriticalErr
      logical bAutoOK
      character*(*) cActionPass

C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iCount, iNext       ! counters
      
      logical bClmFileExists      ! Flag indicating climate
                                  ! file has been found

C.....Flags for file status
      logical bNonContigious, bDuplicates, bOrderChanged

      integer iYr_buf             ! year buffer
      character*124 cMsg          ! message to be displayed to buffer
      character*124 cContext      ! contextual message
      character*16 cAction

C----------------------------------------------------------------------
C     Initialze context buffer
C----------------------------------------------------------------------
      write (cContext, '(A)') ' Verifying multi-year climate db: '

C----------------------------------------------------------------------
C     Truncate cAction, if necessary
C----------------------------------------------------------------------
      write (cAction, '(A)') cActionPass(1:min(lnblnk(cActionPass),16))

C----------------------------------------------------------------------
C.    Have sufficient records been defined?
C----------------------------------------------------------------------
      if ( iMY_clm_file_count .lt. 1 ) then

         write (cMsg, '(A,A)') ' Error: at least one valid year '
     &        // 'record (*rec) is required. '
         call edisp(iuout, cContext)
         call edisp(iuout, cMsg)

         if ( .not. bAutoOK ) then
            call UsrMsg ( cContext, cMsg, 'W')
         endif

         bCriticalErr = .true.
      endif

         
C----------------------------------------------------------------------
C     Check if clm files have been defined in order, and sort if
C     if necessary.
C----------------------------------------------------------------------
      if (.not. bCriticalErr)
     &        call Sort_MY_clm_db (cContext,cAction,bOrderChanged )

C----------------------------------------------------------------------
C     Check for contigunity in records
C----------------------------------------------------------------------
      if ( .not. bCriticalErr)
     &        call Fill_MY_clm_db (cContext, cAction,
     &                             bCriticalErr, bNonContigious)

C----------------------------------------------------------------------
C     Prune database of duplicates, if necessary
C----------------------------------------------------------------------
      if ( .not. bCriticalErr)
     &        call Prune_MY_clm_db(cContext,cAction,bDuplicates)

C----------------------------------------------------------------------
C     Report status
C----------------------------------------------------------------------

      call edisp(iuout, ' ')

      call edisp(iuout,
     &           ' A multi-year climate database has been defined')

      write (cMsg, '(A,A)') '  - multi-year climate db file: ',
     &      cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))
      call edisp (iuout, cMsg)
       
      write (cMsg, '(A,f4.2)') '  - multi-year climate db version: ',
     &      fMY_version_number
      call edisp (iuout, cMsg )

      write (cMsg, '(A,I4,A,I4)')
     &    '  - multi-year climate db years: ',
     &    iMY_clm_file_years(1),' to ',
     &    iMY_clm_file_years(iMY_clm_file_count)
      call edisp (iuout,cMsg)


      write (cMsg, '(A,I2)' ) '  - number of climate files defined:',
     &    iMY_clm_file_count
      call edisp (iuout, cMsg )

      write (cMsg, '(A)') '  - defined clm files:'
      call edisp (iuout, cMsg)

      do iCount = 1, iMY_clm_file_count

         write(cMsg, '(A,i4,A,A)' ) '      ',
     &         iMY_clm_file_years(iCount), ': ',
     &         cMY_clm_file_names(iCount)
     &                  (1:lnblnk(cMY_clm_file_names(iCount)))

         call edisp (iuout, cMsg)

      enddo


C----------------------------------------------------------------------
C     Are climate files valid?
C----------------------------------------------------------------------
      do iCount = 1, iMY_clm_file_count

         INQUIRE (FILE=cMY_clm_file_names(iCount)
     &               (1:lnblnk(cMY_clm_file_names(iCount))),
     &            EXIST=bClmFileExists )

         if ( .not. bClmFileExists ) then

C...........Error trap missing file
            write(cMsg,'(A,A)') 'Cannot find file: ',
     &         cMY_clm_file_names(iCount)
     &            (1:lnblnk(cMY_clm_file_names(iCount)))
            call edisp(iuout, cContext)
            call edisp(iuout, cMsg)

            bCriticalErr = .true.

         endif

      enddo

C----------------------------------------------------------------------
C     Initialize climate file status flags
C----------------------------------------------------------------------
      do iCount = 1, iMY_clm_file_count

         bMY_clm_file_open(iCount) = .false.

      enddo

C----------------------------------------------------------------------
C     Were errors encountered?
C----------------------------------------------------------------------
      
      if ( bCriticalErr ) then

         call edisp(iUout, ' Multi-year climate db was not defined'
     &         // ' correctly.')

         if ( bAutoOK ) then

            stop 'Fatal Error!'
             
         endif

      else
      
         call edisp(iUout, ' Multi-year climate db was defined'
     &         // ' correctly.')

      endif 

      return
      end

      
C------------------ MY_clm_db_menu ------------------------------------
C
C This procedure is used to control specification of a multi-year
C climate database
C
C Inputs - none; outputs - none 
C
C---------------------------------------------------------------------- 

      subroutine MY_clm_db_menu ()
      implicit none 
#include "building.h"
#include "MultiYear_simulations.h"

C External functions.     
      integer lnblnk
      character achar

C ESP-r variables
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen     

      common/pophelp/h(60)
      character h*72       ! help text      

      common/spad/mmod,limit,limtty
      integer mmod, limit, limtty

      common/fileP/iFil
      integer iFil                ! base file unit number.      

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
C.....File status
      logical bExists, bCreate, bMY_clm_err, bSaveOk, bOK

C.....Menu loop controls
      logical bDone, bFileExists, bPrompt_Finished

C.....File status flags 
      logical bOrderChanged, bNonContigious, bDuplicates, bCriticalErr
            
C.....Menu
      character*72 cMenu(50)  ! Array containing menu text
      integer iChoice         ! Selected option
      integer iErr            ! Error flag
      integer iMenu_count     ! # of menu prompts.


      character*124 cMsg, cContext
      
C.....Counter
      integer iCount,iNext       

C.....Display of records      
      integer iMaxDisp              ! max number of records to display
      parameter ( iMaxDisp = 10 )
      integer iStartDisp            ! First record to be displayed
      integer iStopDisp             ! Last record to be displayed 

      integer iRecStart             ! Start of records in menu
      integer iRecStop              ! End of records in menu
      integer iPage, iPageCount     ! Page indicies

      integer iRec_index            ! index of selected record


      integer iYr_buf               ! year buffer
      integer iAnswer               ! user response
      character*72 cAnswer          ! User response
      logical bAnswer               ! User response
C----------------------------------------------------------------------
C     Initialize
C----------------------------------------------------------------------      
      bDone   = .false.
      bSaveOk = .true.
C----------------------------------------------------------------------
C     Has db been defined?
C----------------------------------------------------------------------
      if ( bMY_climates_defined ) then
C........Yes - Parse file and check for errors
C........(file unit number iFile+1 is used to open a temporary file)
         call parse_my_clm_db( iFil+1, .false., bMY_clm_err )
C........If file was parsed successfully, check
C........for errors in definitions
         if (.not. bMY_clm_err )
     &      call check_my_clm_db ( .false.,'warn', bMY_clm_err )
C........If errors were encountered, set database
C........definition status
         if ( bMY_clm_err ) bMY_climates_defined = .false.                
         
      endif


C.....Default position for record display
      iStartDisp = 1 

      
      do while ( .not. bDone )
C........Initialize flags
         iChoice = -4
         iErr = 0
         write (cContext, '(A)') 'Verifying multi-year climate db: '

C........Check status of database and warn if out-of-order, or
C........non-contigious. Note: the 'warn' tag is used to prevent
C........any action from being taken.
         call Sort_MY_clm_db(cContext,'warn',bOrderChanged)
         call Fill_MY_clm_db(cContext,'warn',
     &                       bCriticalErr, bNonContigious )
         call Prune_MY_clm_db(cContext,'warn', bDuplicates)

C........Set flags for later
         bOrderChanged  = .false.
         bDuplicates    = .false.
         bNonContigious = .false.

C........Update context          
         write (cContext, '(A)') 'Editing multi-year climate db: '
C----------------------------------------------------------------------
C        Prepare menu
C----------------------------------------------------------------------
C........DB file name
         if ( bMY_climates_defined ) then
            cTemp = cMY_climate_db_name
         else
            cTemp = 'None'
         endif

         cMenu(1) = 'a Multi-year climate db: ' // cTemp(1:44)
         iMenu_count = 1

C........DB statistics                  
         if ( bMY_climates_defined ) then
C...........Save status
            if ( bSaveOk ) then 
               write (cMenu(iMenu_count+1),'(A)')
     &          '  File status          : unchanged'
            else
               write (cMenu(iMenu_count+1),'(A)')
     &          '  File status          : modified'
            endif
            
            write (cMenu(iMenu_count+2),'(A,f4.2)')
     &          '  File version numnber : ', fMY_version_number
            write (cMenu(iMenu_count+3),'(A,I2)')
     &          '  Number of clm files  : ', iMY_clm_file_count
            
            iMenu_count = iMenu_count + 3
            
            if (iMY_clm_file_count .gt. 0) then

               write (cMenu(iMenu_count+1),'(A,I4,A,I4)')
     &            '  Database period      : ', iMY_clm_file_years(1),
     &            ' to ', iMY_clm_file_years(iMY_clm_file_count)

            else

               cMenu(iMenu_count+1) = ' '
            
            endif 
            
C...........Record list
            cMenu ( iMenu_count + 2 ) =
     &               '  =============================== '

            cMenu ( iMenu_count + 3 ) =
     &               '  | year | climate file           '

            cMenu ( iMenu_count + 4 ) =
     &               '  =============================== '

            iMenu_count = iMenu_count + 4

C...........First record to be displayed            
            if ( iStartDisp .gt. iMY_clm_file_count ) then
               iStartDisp = MAX(iStartDisp - iMaxDisp, 1)
            endif
C...........Last record to be displayed
            iStopDisp = (iStartDisp - 1) + iMaxDisp

C...........Start of records in menu
            iRecStart = iMenu_count + 1

C...........End of records in menu
            iRecStop = iRecStart + ( iStopDisp - iStartDisp )

            do iCount = iStartDisp, iStopDisp

               iMenu_count = iMenu_count + 1

               if ( iCount .le. iMY_clm_file_count ) then

C.................start record indicies at 'b', and increment
                  cTemp = AChar ( 98 + (iCount -iStartDisp) )

                  write ( cMenu( iMenu_count ), '(A,I4,A,A)')
     &               cTemp(1:1) // ' | ',
     &               iMY_clm_file_years(iCount), ' | ',
     &               cMY_clm_file_names(iCount)(1:61)
               else
                  write (cMenu( iMenu_count ) , '(A)')
     &                 '  |      | '
               endif

            enddo

            cMenu ( iMenu_count + 1 ) =
     &               '  =============================== '

C...........Page index, and total # of pages
            iPage = 1
            iPageCount = 1
            do while ( iPageCount*iMaxDisp .lt. iMY_clm_file_count )
               iPageCount = iPageCount + 1
               if (iPage*iMaxDisp .lt. iStartDisp) iPage = iPage + 1
            enddo

            write ( cMenu (iMenu_count+2), '(A,I2,A,I2)')
     &         '  Page ', iPage, ' of ', iPageCount


C...........Page naviagation
            if ( iStartDisp .gt. 1 ) then
               cMenu ( iMenu_count + 3 ) =
     &               '< Previous page                   '
            else
               cMenu ( iMenu_count + 3 ) = ' '
            endif

            if (iStopDisp .lt. iMY_clm_file_count ) then
               cMenu ( iMenu_count + 4 ) =
     &               '> Next page                       '
            else
               cMenu ( iMenu_count + 4 ) = ' '
            endif

            iMenu_count = iMenu_count + 4

C...........Add / Sort / Prune / Save options
            cMenu(iMenu_count+1) = '  _______________________________ '
            cMenu(iMenu_count+2) = '+ add new record                  '
            cMenu(iMenu_count+3) = 'p sort records by year            '
            cMenu(iMenu_count+4) = 'q fill in missing records         '
            cMenu(iMenu_count+5) = 'r remove duplicate records        '
            cMenu(iMenu_count+6) = '  _______________________________ '
            cMenu(iMenu_count+7) = 's save database                   '

            iMenu_count = iMenu_count + 7
         endif 
C........Standard options         
         cMenu(iMenu_count + 1) = '  _______________________________ '
         cMenu(iMenu_count + 2) = '? help                            '
         cMenu(iMenu_count + 3) = '- exit this menu                  '

         iMenu_count = iMenu_count + 3 
         
         call emenu('Multi-year Climate db',
     &              cMenu,iMenu_count, iChoice)

C Respond to user's choice.
         if ( iChoice .eq. 1 ) then

C User wants to change file name.

C Save changes?
            if ( .not. bSaveOk ) then
               call eAskABC (cContext,
     &            'Do you wish to save your changes?',
     &            'yes','no','cancel', iAnswer, 0 )

               if ( iAnswer .eq. 1 ) then

                  call Save_MY_clm_db(iFil+1, bCriticalErr, bSaveOk)
                  bOk = .true.
                  
               elseif (iAnswer .eq. 2 ) then
                  bOk = .true.
               elseif (iAnswer .eq. 3 ) then
                  bOk = .false.
               endif
            else
               bOk = .true.
            endif 


            if ( bOk ) then

C Allow user to specify a new file name.
               call spec_my_clm_db('Create', bExists, bCreate )

C Does file exist?
               if ( bExists ) then

C Yes - parse file and check for errors
C (iFil+1 used to open a temporary file).
                  call parse_my_clm_db( iFil + 1, .false., bMY_clm_err )

C If file parsed successfully, check
C for errors in definitions.
                  if (.not. bMY_clm_err )
     &             call check_my_clm_db ( .false., 'warn', bMY_clm_err )

C.................If errors were encountered, set database
C.................definition status
                  if ( bMY_clm_err ) then
                     bMY_climates_defined = .false.
                  else
                     bMY_climates_defined = .true.
                     bSaveOk = .true.
                  endif
            
               elseif ( bCreate ) then
C.................File doesn't exist, but will be created
                  bMY_climates_defined = .true.
                  fMY_version_number = fMY_default_version
                  iMY_clm_file_count = 0

C.................Empty file / year buffers
                  do iCount = 1, iMax_Climate_Years
                     iMY_clm_file_years(iCount) = 0
                     cMY_clm_file_names(iCount) = ' '
                  enddo
            
               else 

C.................database definitions have not been defined
C.................successfully
                  bMY_climates_defined = .false.
                  bDone = .true.
               
               endif
            endif 
         elseif ( iChoice .ge. iRecStart .and.
     &            iChoice .le. iRecStop ) then
C----------------------------------------------------------------------
C           User wants to edit/delete existing record.
C----------------------------------------------------------------------
C...........Is there actually a record in this row?
            if ( (iChoice - iRecStart + 1 )+ iMaxDisp * (iPage - 1)
     &         .le. iMY_clm_file_count ) then


C..............Get the record's index:
               iRec_index = ( iChoice - iRecStart + 1 )
     &                           + iMaxDisp * ( iPage - 1 )
               write (cContext, '(A,I4,A,A,A)')
     &           'Record: ', iMY_clm_file_years(iRec_index),
     &           ' (',cMY_clm_file_names(iRec_index)
     &                 (1:lnblnk(cMY_clm_file_names(iRec_index))),
     &           ') '
               
               write (cMsg, '(A)') 'Edit/delete this record?'

               call eAskABC(cContext,cMsg,
     &                      'edit','delete','cancel',iAnswer,0)

               if ( iAnswer .eq. 1 ) then
C.................Edit the record.

C.................Prompt text:
                  write (cMsg,'(A)')'Enter new year (YYYY):'
C.................Existing value:
                  iAnswer = iMY_clm_file_years(iRec_index)
C.................Get new value
                  call eAskI ( iAnswer, cContext, cMsg,
     &                         1900,'W', 2100,'W', iAnswer,
     &                         'year', iErr,0)
C.................If no errors were encountered, update record.
                  if (iErr .eq. 0 )
     &                   iMY_clm_file_years (iRec_index) = iAnswer

C.................Update contextual message
                  write (cContext, '(A,I4,A,A,A)')
     &              'Record: ', iMY_clm_file_years(iRec_index),
     &              ' (',cMY_clm_file_names(iRec_index)
     &                 (1:lnblnk(cMY_clm_file_names(iRec_index))),
     &              ') '

C.................Prompt text:
                  write (cMsg,'(A)')'Enter climate file database:'

                  
C.................Existing value:
                  cAnswer = cMY_clm_file_names(iRec_index)

                  bPrompt_Finished = .false.
                  do while ( .not. bPrompt_Finished )
C....................Prompt user for file
                     call eAskS (cAnswer,cContext,cMsg,72,cAnswer,
     &                           'climate file name',iErr,0)
                     bFileExists = .false.
                     if ( iErr .eq. 0 ) then
                        INQUIRE ( FILE=cAnswer(1:lnblnk(cAnswer)),
     &                            EXIST=bFileExists )
                     endif

                     if ( bFileExists ) then
                        
C.......................File found. Exit loop
                        bPrompt_Finished = .true.
                        cMY_clm_file_names(iRec_index) = cAnswer
                        
                     else
C.......................File not found. Ask user if they want to
C.......................try again
                        write(cMsg,'(A,A,A)') 'Specified file (',
     &                      cAnswer(1:lnblnk(cAnswer)),
     &                      ') could not be found.'
                        call edisp(iUout,' ')
                        call edisp(iUout,cContext)
                        call edisp(iUout,cMsg)

                        call eAskABC(cContext,cMsg,
     &                      'Try Agin','Use anyway','Cancel',iAnswer,0)
                        
                        if ( iAnswer .eq. 1 )then
C..........................Do nothing. Code will loop
                        elseif (iAnswer .eq. 2 ) then
C..........................Use value anyway
                           cMY_clm_file_names(iRec_index) = cAnswer
                           bPrompt_Finished = .true.
                        else
C..........................exit prompt.
                           bPrompt_Finished = .true.
                        endif
                     endif
                  enddo 
C.................Update save status
                  bSaveOk = .false.
                  
               elseif (iAnswer .eq. 2 ) then
C.................Delete the record
                  do iNext = iRec_index, iMax_Climate_Years - 1

                     iMY_clm_file_years (iNext) =
     &                  iMy_clm_file_years( iNext + 1 )

                     cMY_clm_file_names ( iNext ) =
     &                  cMY_clm_file_names ( iNext + 1 )

                  enddo
C.................Insert empty record at end of array
                  iMy_clm_file_years ( iMax_Climate_Years ) = 0
                  cMY_clm_file_names ( iMax_Climate_Years ) = ' '

C.................Decrement record counter
                  iMY_clm_file_count = iMY_clm_file_count - 1
                  
C.................Update save status
                  bSaveOk = .false.
                  
               elseif ( iAnswer .eq. 3 ) then

C.................Do nothing - return to menu
                              
               endif
              

            endif

         elseif ( iChoice .eq. iRecStop + 3 ) then
C----------------------------------------------------------------------
C           User wants to scroll back page
C----------------------------------------------------------------------
            if ( iStartDisp .gt. 1 ) then

               iStartDisp = iStartDisp - iMaxDisp

               if ( iStartDisp .lt. 1 ) iStartDisp = 1
            
            endif             

         elseif ( iChoice .eq. iRecStop + 4 ) then
C----------------------------------------------------------------------
C           User wants to scroll forward page 
C----------------------------------------------------------------------
            if ( iStopDisp .lt. iMY_clm_file_count ) then

               iStartDisp = ( iStartDisp ) + iMaxDisp
   
            endif


         elseif ( iChoice .eq. iMenu_count - 8 ) then
C----------------------------------------------------------------------
C           User wants to add a new record
C----------------------------------------------------------------------
            cContext = 'Add a new record'

            if ( iMY_clm_file_count .lt. iMax_Climate_Years ) then

C..............Edit the record.

C..............Prompt text:
               write (cMsg,'(A)')'Enter year (YYYY):'
C..............Existing values - last in array
               if ( iMY_clm_file_count .gt. 0) then
                  iAnswer =
     &               iMY_clm_file_years(iMY_clm_file_count) + 1
                  cAnswer = cMY_clm_file_names(iMY_clm_file_count)
               else
                  iAnswer = 2005
                  cAnswer = ' '
               endif
C..............Get new value
               call eAskI ( iAnswer, cContext, cMsg,
     &                      1900,'W', 2100,'W', iAnswer,
     &                      'year', iErr,0)

C..............Should error trap here     
               iYr_buf = iAnswer

C..............Prompt text:
               write (cMsg,'(A)')'Enter climate file database:'

               bPrompt_Finished = .false.
               do while ( .not. bPrompt_Finished )
C.................Prompt user for file
                  call eAskS (cAnswer,cContext,cMsg,72,cAnswer,
     &                        'climate file name',iErr,0)
                  bFileExists = .false.
                  if ( iErr .eq. 0 ) then
                     INQUIRE ( FILE=cAnswer(1:lnblnk(cAnswer)),
     &                         EXIST=bFileExists )
                  endif

                  if ( bFileExists ) then

C....................File found. Create record and exit loop
                     iMY_clm_file_count = iMY_clm_file_count + 1
                     cMY_clm_file_names(iMY_clm_file_count) = cAnswer
                     iMY_clm_file_years(iMY_clm_file_count) = iYr_buf
                     
                     bPrompt_Finished = .true.

C....................Update save status
                     bSaveOk = .false.
                  
                  else
C....................File not found. Ask user if they want to
C....................try again
                     write(cMsg,'(A,A,A)') 'Specified file (',
     &                   cAnswer(1:lnblnk(cAnswer)),
     &                   ') could not be found.'
                     call edisp(iUout,' ')
                     call edisp(iUout,cContext)
                     call edisp(iUout,cMsg)

                     call eAskABC(cContext,cMsg,
     &                   'Try Agin','Use anyway','Cancel',iAnswer,0)

                     if ( iAnswer .eq. 1 )then
C.......................Do nothing. Code will loop

                     elseif (iAnswer .eq. 2 ) then
C.......................Use value anyway. Create record and exit loop
                        iMY_clm_file_count = iMY_clm_file_count + 1
                        cMY_clm_file_names(iMY_clm_file_count) = cAnswer
                        iMY_clm_file_years(iMY_clm_file_count) = iYr_buf
                     
                        bPrompt_Finished = .true.

C.......................Update save status
                        bSaveOk = .false.
                     
                     else
C.......................exit loop.
                        bPrompt_Finished = .true.
                        
                     endif
                  endif
               enddo

            else
                write(cMsg,'(A,I2,A)') 'Maximum number of records (',
     &                iMax_Climate_Years, ') already specified. '
                call edisp(iUout,' ')
                call edisp(iUout,cContext)
                call edisp(iUout,cMsg)

            endif        
         elseif ( iChoice .eq. iMenu_count - 7 ) then
C----------------------------------------------------------------------
C           User wants to sort records
C----------------------------------------------------------------------
            call AskOk('This operation will sort the records by year.',
     &                  'Continue?', bAnswer, .true., 0)
            if (bAnswer) then
               call Sort_MY_clm_db(cContext,'fix',bOrderChanged)
C..............Update save status
               if (bOrderChanged )bSaveOk = .false.
            endif
                
         elseif ( iChoice .eq. iMenu_count - 6 ) then
C----------------------------------------------------------------------
C           User wants to check for continuity and fill in missing
C           records 
C----------------------------------------------------------------------
            call AskOk('This operation will sort the records by year',
     &                 'and fill in any missing records. Continue?',
     &                 bAnswer, .true., 0)
            if (bAnswer) then
C..............Sort records and fill in missing ones
               call Sort_MY_clm_db(cContext,'fix',bOrderChanged)
               call Fill_MY_clm_db(cContext,'fix',
     &                       bCriticalErr, bNonContigious )

C..............Update save status
               if (bOrderChanged.or.bNonContigious) bSaveOk = .false.

            endif

         elseif ( iChoice .eq. iMenu_count - 5 ) then
C----------------------------------------------------------------------
C           User wants to delete duplicate records
C----------------------------------------------------------------------
            call AskOk('This operation will sort the records by year',
     &                 'and delete duplicate records. Continue?',
     &                 bAnswer, .true., 0)
            if (bAnswer) then
               bOrderChanged  = .false.
               bDuplicates    = .false.
               bNonContigious = .false.
C..............Sort records and prune extras.
               call Sort_MY_clm_db(cContext,'fix',bOrderChanged)
               call Prune_MY_clm_db(cContext,'fix', bDuplicates)
C..............Update save status
               if (bOrderChanged.or.bDuplicates) bSaveOk = .false.
            endif

         elseif ( iChoice .eq. iMenu_count - 3 ) then

C----------------------------------------------------------------------
C           User wants to save the database
C----------------------------------------------------------------------         
            call Save_MY_clm_db ( iFil+1, bCriticalErr, bSaveOk )
            
         elseif ( iChoice .eq. iMenu_count ) then
C----------------------------------------------------------------------
C           User wants to quit
C----------------------------------------------------------------------
C...........Do they want to save their changes?
            if ( .not. bSaveOk ) then
               call eAskABC (
     &            'Returning to previous menu.',
     &            'Save your changes?',
     &            'yes','no','cancel', iAnswer, 0 )
               if ( iAnswer .eq. 1 ) then

                  call Save_MY_clm_db(iFil+1, bCriticalErr, bSaveOk)
                  bDone = .true.
                  
               elseif (iAnswer .eq. 2 ) then
                  bDone = .true.
               elseif (iAnswer .eq. 3 ) then
                  bDone = .false. 
               endif
            else
               bDone = .true.
            endif                   

         endif 

           
      enddo
      return
      end


C------------------ Spec_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This procedure is used to specify an existing, or new
C multi-year climate file. 
C
C Inputs 
C  - cAction - Action to be taken if file not found
C
C Outputs:
C
C  - bFileExists - flag indicating that file was found
C  - bCreate  - flag indicating file should be created
C
C----------------------------------------------------------------------      
      subroutine Spec_MY_clm_db(cActionPass,bFileExists,bCreate)
      implicit none

#include "building.h"
#include "MultiYear_simulations.h"      

C External functions.     
      integer lnblnk

C Passed arguements
      logical bFileExists
      logical bCreate
      character*(*) cActionPass
      character*16 cAction
      
C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen     

      common/pophelp/h(60)
      character h*72       ! help text
      
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      logical bOptOut           ! Flag indicating user can opt-out
                                ! of defining file

      logical bDone             ! loop control

      integer iAsk_AB_result    ! Result from yes/no question

      logical bError            ! error flag
      
      character*128 cMsg        ! Character buffers
      character*128 cContext

      integer iEr               ! error signal 

C----------------------------------------------------------------------
C     Create help text
C----------------------------------------------------------------------
      H(1) = 'The multi-year climate database (*.myclm) '
      H(2) = 'defines the sequence in which climate files '
      H(3) = 'should be read during a multi-year simulation.'
      H(4) = 'Climate files can be added to a multi-year'
      H(5) = 'database using the project manager (prj).'

      write (cAction,'(A)') cActionPass(1:min(lnblnk(cActionPass),16))

C----------------------------------------------------------------------
C     User must attempt to define the database. If they fail on
C     the first try, they can choose not to try agian.
C----------------------------------------------------------------------

C.....Initialize
      bFileExists = .false.
      bCreate = .false. 

C.....Loop controls
      bOptOut = .false.
      bDone   = .false.
      
      do while ( .not. bDone )



C........Prompt user for file
         CALL EASKS(cMY_climate_db_name,
     &     ' ','Multi-year db name?',
     &     72,cMY_climate_db_name,'file name',IER,5)

         if ( ier .ne. 0 ) then

C...........Possibly error trap input here? As is, code will loop.
            bOptOut = .true.

         else

C-------------------------------------------------------------------
C        Does file exist?
C-------------------------------------------------------------------
            INQUIRE (
     &         FILE=cMY_climate_db_name
     &                  (1:lnblnk(cMY_climate_db_name)),
     &         EXIST=bFileExists
     &      )


            if ( bFileExists ) then
C-------------------------------------------------------------------
C              File exists.
C-------------------------------------------------------------------
               write (cMsg,'(3A)') 'Existing file (',
     &            cMY_climate_db_name(1:lnblnk(cMY_climate_db_name)),
     &            ') was found.'

               call edisp ( iUout, cMsg )

               bCreate = .false.
               bDone = .true.

            else

C-------------------------------------------------------------------
C              File doesn't exist. What action should be taken?
C-------------------------------------------------------------------
               write (cContext,'(A)') 'Multi-year climate db:'

               write (cMsg,'(3A)') 'File (',
     &            cMY_climate_db_name(1:lnblnk(cMY_climate_db_name)),
     &            ') could not be found!'

               bOptOut = .true.
               if ( cAction(1:4) .eq. 'none' ) then

C.................Do nothing
                  bDone  = .true.

               
               elseif ( cAction(1:4) .eq. 'fail' ) then
C.................Warn user, and return to menu
                  bCreate = .false.
                  bDone = .false.

                  call edisp ( iUout, cContext)
                  call edisp ( iUout, cMsg )

                  call UsrMsg ( cContext, cMsg, 'W' )

               elseif ( cAction(1:6) .eq. 'Create' ) then

C.................Perhaps create new file.

                  CALL EASKABC(cContext,cMsg,
     &               'create file', 'try agian', 'cancel',
     &               iAsk_AB_result,5)

                  if ( iAsk_AB_result .eq. 1 ) then

C....................Create new file
                     bCreate = .true.
                     bDone = .true.


                  elseif (iAsk_AB_result .eq. 2 ) then

C....................Try agian
                     bDone = .false.
                     bCreate = .false.



                  elseif (iAsk_AB_result .eq. 3 ) then

C....................Leave file undefined
                     bCreate = .false.
                     bDone = .true.

                  endif   ! <- matches 'if ( iAsk_AB_result .eq. 1 ...

               endif      ! <- matches 'if ( cAction(1:4) .eq. ...

            endif         ! <- matches 'if ( bFileExists )...'

         endif            ! <- matches 'if ( ier .ne. 0 ) then'

      enddo                  ! <- matches 'do while ( .not. bDone )'


      return
      end



C------------------ Sort_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This routine checks to see if the climate records are in order, and
C sorts them if necessary
C
C Inputs:
C
C  - cContext: contextual buffer
C  - cAction: flag indicating what actions should be taken 
C             on out-of-order records
C
C Outputs:
C
C  - bOrderChanged: flag indicating if records were out of order
C
C----------------------------------------------------------------------      
      subroutine Sort_MY_clm_db(cContext,cActionPass,bOrderChanged)
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"      

C----------------------------------------------------------------------
C     Passed variable
C----------------------------------------------------------------------
      logical bOrderChanged
C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iCount, iNext       ! counters
      
      logical bSorted, bInOrder   ! flags for sorting loop

      character*(*) cActionPass
      integer iYr_buf             ! year buffer
      character*72 cFile_buf      ! file name buffer
      character*124 cContext      ! contextual buffer
      character*16 cAction        ! Action to be undertaken

C External functions.     
      integer lnblnk

      write (cAction,'(A)') cActionPass(1:min(lnblnk(cActionPass),16))

C----------------------------------------------------------------------
C     Check if clm files have been defined in order, and sort if
C     if necessary.
C----------------------------------------------------------------------

C.....Initialize sorting array
      do iCount = 1, iMY_clm_file_count
         iMY_clm_file_order (iCount) = iCount
      enddo

C.....Sort array
      iCount = 1
      bSorted = .false.
      bInOrder = .false.
      bOrderChanged = .false.
      
      do while ( .not. bSorted )

         if ( iMY_clm_file_years ( iCount ) .gt.
     &        iMY_clm_file_years ( iCount + 1 ) ) then

            bOrderChanged = .true.

            if ( cAction(1:3) .eq. 'fix' ) then
C..............Swap year
               iYr_buf = iMY_clm_file_years (iCount + 1 )

               iMY_clm_file_years (iCount + 1 ) =
     &                iMY_clm_file_years (iCount )

               iMY_clm_file_years (iCount ) = iYr_buf

C..............swap file buffer
               cFile_buf = cMY_clm_file_names ( iCount + 1)

               cMY_clm_file_names ( iCount + 1) =
     &                 cMY_clm_file_names ( iCount )

               cMY_clm_file_names ( iCount ) = cFile_buf

C..............Set flag indicating another pass is req'd
               bInOrder = .false.
            else
C..............Exit loop
               bSorted = .true.
            endif             
            
         endif

         if ( iCount .eq. iMY_clm_file_count -1 ) then
            if ( bInOrder ) then
               bSorted = .true.
            else
               bInOrder = .true.
               iCount = 1
            endif
         else
            iCount = iCount + 1
         endif

      enddo

C.....Warn user as necessary      
      if ( bOrderChanged ) then

        call edisp(iuout, ' ')
        call edisp(iuout, cContext)
        call edisp(iuout, ' Warning: Climate file records were not')
        call edisp(iuout, '          in order. ')
        if (cAction(1:3).eq.'fix')
     &        call edisp(iUout, 'Records have been sorted by year.')
             
        call edisp(iuout, ' ')

      endif

      
      return
      end


C------------------ Fill_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This procedure checks for continuity in the multi-year climate
C database, and fills in missing years, if necessary
C
C Inputs:
C  - cContext: contextual buffer
C  - cAction: flag indicating what actions should be taken 
C             on out-of-order records
C Outputs:
C
C  - bCriticalErr: flag indicating if a critical error was
C    encountered.
C  - bContigious: Flag indicating if file was contigious 
C
C----------------------------------------------------------------------      
      subroutine Fill_MY_clm_db(cContext,cActionPass,
     &                          bCriticalErr,bNonContigious)
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"      
C----------------------------------------------------------------------
C     Passed variable
C----------------------------------------------------------------------
      logical bCriticalErr
      logical bNonContigious
C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin         ! channels for writing messages to screen

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iCount, iNext       ! counters

      logical bClmFileExists      ! Flag indicating climate
                                  ! file has been found

      integer iYr_buf             ! year buffer
      character*248 cFile_buf     ! file name buffer
      character*124 cMsg, cContext !msg buffers
      character*16 cAction        ! action flag
      character*(*) cActionPass
      logical bDone               ! Loop control
      logical bOK                 ! Misc flag

C External functions.     
      integer lnblnk

      write (cAction,'(A)') cActionPass(1:min(lnblnk(cActionPass),16))

      bNonContigious   = .false.

C---------------------------------------------------------------------
C     Have too many years been specified?
C----------------------------------------------------------------------      
      if ( iMY_clm_file_years(iMY_clm_file_count) -
     &     iMY_clm_file_years(1) .lt. iMax_Climate_Years ) then

C........No! Loop through records and check for continuity     
         do iCount = 1, iMax_Climate_Years -1

C...........Is a new record required?
            if ( iMY_clm_file_years ( iCount + 1 ) .gt.
     &           iMY_clm_file_years ( iCount ) + 1 .and.
     &           iMY_clm_file_years ( iCount + 1 ) .ne. 0 ) then
C..............Yes!
               bNonContigious = .true.
               if (cAction(1:3).eq.'fix')then
               
               
C.................Make space for new record
                  do iNext = iMax_Climate_Years -1 , iCount + 1, -1

                     iMy_clm_file_years (iNext + 1 ) =
     &                  iMy_clm_file_years( iNext )

                     cMY_clm_file_names ( iNext + 1 ) =
     &                  cMY_clm_file_names ( iNext )

                  enddo

C.................Insert new record. Use last climate database.
                  iMY_clm_file_years ( iCount + 1 ) =
     &                iMy_clm_file_years ( iCount ) + 1

                  cMY_clm_file_names ( iCount + 1 ) =
     &                cMY_clm_file_names ( iCOunt )

C.................Increment record counter
                  iMY_clm_file_count = iMY_clm_file_count + 1
               endif 
            endif
         enddo

      else
C........Insufficient array locations to fill in missing years
         call edisp (iUout, ' ')
         call edisp (iUout, cContext) 
         call edisp (iUout, ' Error: Difference between first year')
         call edisp (iuout, '        (' 
     &             //    char ( iMY_clm_file_years(1) ) // ') and last '
     &             //    'year (' 
     &             //    char ( iMY_clm_file_years(iMY_clm_file_count))
     &             //    ')' )
         call edisp (iUout, ' exceeds limit (' 
     &             // char(iMax_Climate_Years ) // ').')
         call edisp (iUout, ' ')
         bCriticalErr = .true.
     
      endif

      if ( bNonContigious ) then
        call edisp(iuout, ' ')
        call edisp(iuout, cContext)
        call edisp(iuout, ' Warning: Climate file records were non-')
        call edisp(iuout, '          contigious.')
        if (cAction(1:3).eq.'fix')
     &     call edisp(iUout,
     &                'New records were created for missing years.')
        call edisp(iuout, ' ')
      endif

      return
      end


C------------------ Prune_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This procedure checks for duplicate records, and optionally deletes
C them
C
C Inputs:
C
C  - cContext: contextual message
C
C  - cAction: flag indicating the action that should be undertaken
C    if duplicates are found 
C 
C Outputs:
C
C  - bDuplicates: Flag indicating if file was contigious
C
C----------------------------------------------------------------------      
      subroutine Prune_MY_clm_db(cContext,cActionPass,bDuplicates)
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"      
C----------------------------------------------------------------------
C     Passed variable
C----------------------------------------------------------------------
      logical bDuplicates
      character*(*) cActionPass
      character*16 cAction
C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin         ! channels for writing messages to screen
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iCount, iNext       ! counters

      logical bClmFileExists      ! Flag indicating climate
                                  ! file has been found

C.....Flags for file status
      logical bNoDuplicates

      integer iYr_buf             ! year buffer

      character*124 cContext      ! context message
      logical bDone               ! Loop control
      logical bOK                 ! Misc flag

C External functions.     
      integer lnblnk

      write (cAction,'(A)') cActionPass(1:min(lnblnk(cActionPass),16))
C----------------------------------------------------------------------
C     Are years contigious?
C----------------------------------------------------------------------
      bDuplicates = .false.

      do iCount = 1, iMax_Climate_Years -1
C........Is the next record a duplicate?
         if ( iMY_clm_file_years ( iCount + 1 ) .eq.
     &        iMY_clm_file_years ( iCount ) .and.
     &        iMY_clm_file_years ( iCount + 1 ) .ne. 0) then
C...........Yes!
            bDuplicates = .true.

C...........Should the record be deleted?
            if (cAction(1:3) .eq. 'fix') then
C.............delete the second instance of this record
              do iNext = iCount + 1, iMax_Climate_Years

                 iMY_clm_file_years (iNext) =
     &              iMy_clm_file_years( iNext + 1 )

                 cMY_clm_file_names ( iNext ) =
     &             cMY_clm_file_names ( iNext + 1 )

              enddo
C.............Insert empty record at end of array
              iMy_clm_file_years ( iMax_Climate_Years ) = 0
              cMY_clm_file_names ( iMax_Climate_Years ) = ' '

C.............Decrement record counter
              iMY_clm_file_count = iMY_clm_file_count - 1

            endif

         endif
      enddo


      if ( bDuplicates ) then
        
        call edisp(iuout, ' ')
        call edisp(iuout, cContext)
        call edisp(iuout, ' Warning: Duplicate climate file records')
        call edisp(iuout, '          were found.')
        if (cAction(1:3).eq.'fix' ) then
          call edisp(iuout, '        Duplicate records were deleted')
        endif 
        call edisp(iuout, ' ')
        
      endif

      return
      end 



      
C------------------ Save_MY_clm_db ------------------------------------
C
C ABSTRACT:
C
C This procedure controls saving a multi-climate database to
C disk
C
C Inputs:
C
C  - iFile_num: file number to use
C  
C Outputs:
C
C  - bCriticalErr: Flag indicating if error was encountered.
C  - bSaveOk: flag indicating that file was saved successfully.
C
C----------------------------------------------------------------------

      subroutine Save_MY_clm_db ( iFile_num, bCriticalErr,bSaveOk )
      implicit none

#include "building.h"
#include "MultiYear_simulations.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iFile_num
      logical bCriticalErr

      logical bSaveOk

C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iUout,iuin
      integer iUout, iuin         ! channels for writing messages to screen
      
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
      integer iCount 
      integer iFile_read_err
      character*124 cContext, cMsg  ! Message buffers
      logical bFileExists
      logical bDone                 ! Loop control
      integer iAnswer               ! User response
      logical bDummy                ! dummy variable
      logical bNoPrompt             ! flag indicating if prompt needed.
C----------------------------------------------------------------------
C     Initialize 
C----------------------------------------------------------------------
      write (cContext, '(A,A)') 'Saving multi-year climate db: ',
     &   cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))
      bSaveOk = .false.
      bCriticalErr = .false.
      bDone = .false.
      bNoPrompt = .false.                                                                         
C----------------------------------------------------------------------
C     Check that there is a multi-year database to save
C----------------------------------------------------------------------     
      if ( .not. bMY_climates_defined ) then

         call edisp(iUout, ' ')
         call edisp(iUout, cContext)
         call edisp(iUout, 'Error: a multi-year climate db has' //
     &                     ' not been defined')
         bSaveOk = .false.
         bCriticalErr = .true.

      else
         do while ( .not. bDone )

C----------------------------------------------------------------------
C           Update contextual message with latest file name 
C----------------------------------------------------------------------         
            write (cContext, '(A,A)') 'Saving multi-year climate db: ',
     &            cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))

C----------------------------------------------------------------------
C           Find out if the file exists.
C----------------------------------------------------------------------     
            inquire ( FILE  = cMY_climate_db_name
     &                          (1:lnblnk(cMY_climate_db_name)),
     &                EXIST = bFileExists )

            if ( bFileExists ) then

C..............Does user want to overwrite existing file?
C..............Keep asking this question until we are told
C..............to overwrite a file, or are given
C..............a non-existant file!
               write(cMsg,'(A)') 'File already exists.'   
               call eAskABC(cContext,cMsg,
     &                      'Overwrite','Specify another file',
     &                      'Cancel',iAnswer,0)

            elseif( .not. bNoPrompt ) then
C..............Does user want save the file? Only ask
C..............this question once!
               write(cMsg,'(A)') 'Save file?'   
               call eAskABC(cContext,cMsg,
     &                      'Save','Specify another file',
     &                      'Cancel',iAnswer,0)

            endif

            bNoPrompt = .true.
                        
            if ( iAnswer .eq. 1 ) then
C..............Overwrite file & exit loop
               call Write_MY_clm_db
     &                  ( iFile_num, bCriticalErr, bSaveOk )


               bDone = .true.

            elseif ( iAnswer .eq. 2 ) then

C..............Allow user to specify alternate file, and loop
               call Spec_MY_clm_db('none',bFileExists,bDummy)

            elseif ( iAnswer .eq. 3 ) then

C..............Exit loop without saving
               bDone = .true.
            endif
            
         enddo
     
      endif

      call edisp (iUout, ' ')
      call edisp (iUout, cContext)

      if ( bSaveOk ) then
         
         call edisp (iUout, 'File was saved successfully')

      else

         call edisp (iUout, 'File was not saved. ')
         
      endif

      return
      end
      
C------------------ Write_MY_clm_db -----------------------------------
C
C ABSTRACT:
C
C This procedure writes out a climate file database to disk.
C
C Inputs:
C
C  - iFile_num: file number to use
C  
C Outputs:
C
C  - bCriticalErr: Flag indicating if error was encountered.
C  - bSaveOk: flag indicating that file was saved successfully.
C
C----------------------------------------------------------------------

      subroutine Write_MY_clm_db ( iFile_num, bCriticalErr,bSaveOk )
      implicit none

#include "building.h"
#include "MultiYear_simulations.h"

C External functions.     
      integer lnblnk

C Passed arguements
      integer iFile_num
      logical bCriticalErr
      logical bFileExists
      logical bSaveOk

C----------------------------------------------------------------------
C     ESP-r variables 
C----------------------------------------------------------------------
      common/outin/iUout,iuin
      integer iUout, iuin         ! channels for writing messages to screen
      
C----------------------------------------------------------------------
C     Local variables 
C----------------------------------------------------------------------
      integer iCount 
      integer iFile_read_err


      character*124 cContext, cMsg  ! Message buffers
      
C----------------------------------------------------------------------
C     Initialize 
C----------------------------------------------------------------------
      write (cContext, '(A,A)') 'Writing multi-year climate db: ',
     &   cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))

      bCriticalErr = .false.
      bSaveOk = .false. 

C----------------------------------------------------------------------
C     Open ASCII multi-year climate database
C----------------------------------------------------------------------     


C.....Attempt to close file
      call erpfree ( iFile_num, iFile_read_err )

      call EFOPSEQ(iFile_num,cMY_climate_db_name,3,iFile_read_err)

      if ( iFile_read_err .ne. 0 ) then
         bCriticalErr = .true.
         call edisp(iUout,' ')
         call edisp(iUout,cContext)
         call edisp(iUout,'Could not create file.')

      else
C-------------------------------------------------------------------
C        Write file to disk
C-------------------------------------------------------------------
C........File identifier
         write (iFile_num,'(A)')  '* MY_climate_db'
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
         write (iFile_num,'(A)')
     &        '# Multi-year climate database file '
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
C........Version control
         write (iFile_num,'(A)') '# Versioning control:'
         write (iFile_num,'(A,f4.2)')
     &        '* VERSION ', fMY_version_number
C........Start file records
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
         write (iFile_num,'(A)')
     &        '# Flag for starting climate records'
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
         write (iFile_num,'(A)')
     &        '*my_climate_records begin'
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
         write (iFile_num,'(A)')
     &        '# Climate file database:'
         write (iFile_num,'(A)')
     &        '# Each record contains the year for which'
         write (iFile_num,'(A)')
     &        '# the file should be used, and the path'
         write (iFile_num,'(A)')
     &        '# to the corresponding climate file.'
         write (iFile_num,'(A)')
     &        '#'
         write (iFile_num,'(A)')
     &        '# flag| year | Path to clm database file'
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'

C........List file records
         do iCount = 1, iMY_clm_file_count
            write (iFile_num,'(A,I4,A,A)')
     &        '*rec     ', iMy_clm_file_years(iCount),
     &        '   ', cMY_clm_file_names(iCount)
     &                 (1:lnblnk(cMY_clm_file_names(iCount)))

         enddo

         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
         write (iFile_num,'(A)')
     &        '# End of climate file records'
         write (iFile_num,'(A)')
     &        '#-------------------------------------------'
         write (iFile_num,'(A)')
     &        '*my_climate_records end'


         bSaveOk = .true.

      endif

C.....Close file
      call eRPFree (iFile_num, iFile_read_err)

      return
      end
      
