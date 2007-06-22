C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004--2006. Please Contact Ian
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
C================== mimo_ctl.F =========================
C
C Date:      June 22, 2007
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada, (2007)
C
C=======================================================

C--------------- mimo_ctl_read -----------------------------------------
C
C SYNOPSYS: 
C
C Read the MIMO portion of a control description, contained
C in file iUnit (already open), and return iER=0 if successful.
C
C INPUTS: 
C   - iUnit: Unit number of ctl file (already open)
C
C OUTPUTS: 
C   - iER: Error flag. 
C
C-----------------------------------------------------------------------
      subroutine mimo_ctl_read ( iUnit, iER )
      implicit none
#include "mimo_ctl.h"      
C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iUnit 
      integer iER
C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen      
      integer lnblnk      
C-----------------------------------------------------------------------
C     Local variables.
C-----------------------------------------------------------------------
      logical bMIMO_block_open         ! Flags indicating which 
      logical bSENSOR_block_open       !   portion of the MIMO description
      logical bACTUATOR_block_open     !   is currently being parsed
      logical bPERIOD_block_open       !
      logical bDAYTYPE_block_open      !
      logical bCONTROLS_block_open     !
      logical bLOOP_block_open         !
      logical bLOOPDATA_block_open     ! 
      
      integer iFile_read_err           ! Flag indicating error in file read
      logical bFatal_Error, bError 
      
      character*248 cLnBuffer          ! Line buffer
      character*248 cLnWords(124)      ! words on the line      
      integer iWdCount                 ! # of words on line 
      
      integer iDummy                   ! Dummy variables.
      integer ii                       ! counter

C.....Warning messages
      character*124 cMsg          ! message to be displayed to buffer
      character*124 cContext      ! contextual message


C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------  
      integer iEGetArrW                ! Split string into words.
      integer iCtoI                    ! Turn a string into an integer
      real    fCtoR                    ! Turn a string into a float 
      
C.....Initailize flags      
      bMIMO_block_open        = .true.
      bSENSOR_block_open      = .false.
      bACTUATOR_block_open    = .false.
      bPERIOD_block_open      = .false.
      bDAYTYPE_block_open     = .false.
      bCONTROLS_block_open    = .false.
      bLOOP_block_open        = .false.
      bLOOPDATA_block_open    = .false. 
      
      iFile_read_err = 0
      
      bFatal_Error = .false. 

C.....Initialize counters.
      iSenCount = 0
      
C----------------------------------------------------------------------
C     Contextual message
C----------------------------------------------------------------------
      write (cContext, '(A)') ' Parsing MIMO section of control file'
      call edisp ( iuout, ' ' )
      call edisp ( iuout, cContext)
      cContext = ' ' 

C-----------------------------------------------------------------------
C     Loop through file 
C-----------------------------------------------------------------------      
      
      do while ( iFile_read_err == 0 .and. 
     &           bMIMO_block_open    .and.
     &           .not. bFatal_Error        )
      
C.......Empty buffer
        cLnBuffer = ' '
C.......Get line & strip comments
        call LStripc (iUnit, cLnBuffer, 0, iDummy, 0,
     &          'READ MIMO CONTROLS',
     &          iFile_read_err)
        
        

C.......Split words
        iWdCount = iEGetArrW ( cLnBuffer, cLnWords )

!         print*, 'MIMO:(',iWdCount,')',cLnBuffer

       

C-----------------------------------------------------------------------
C       Parse block-open/block-close flags beginning with '*'
C-----------------------------------------------------------------------
        if ( cLnWords(1)(1:1) == '*' ) then
C.........MIMO block          
          if ( cLnWords(2)(1:4) == 'MIMO' .and.
     &         cLnWords(3)(1:3) == 'END'         ) then

            bMIMO_block_open = .false. 
            call mimo_debug ('MIMO block closed')
          endif 
        
C.........SENSOR block
          if ( cLnWords(2)(1:7) == 'SENSORS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bSENSOR_block_open = .false. 
              call mimo_debug('SENSOR block closed')
            else 
              bSENSOR_block_open = .true. 
              call mimo_debug('SENSOR block opened')
            endif 
          endif 

C.........ACTUATOR block
          if ( cLnWords(2)(1:9) == 'ACTUATORS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bACTUATOR_block_open = .false. 
              call mimo_debug('ACTUATOR block closed')
            else 
              bACTUATOR_block_open = .true. 
              call mimo_debug('ACTUATOR block opened')
            endif 
          endif 
          
C.........PERIOD block
          if ( cLnWords(2)(1:7) == 'PERIODS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bPERIOD_block_open = .false. 
              call mimo_debug('PERIOD block closed')
            else 
              bPERIOD_block_open = .true.
              call mimo_debug('PERIOD block opened') 
            endif 
          endif           
          
C.........DAYTYPES block
          if ( cLnWords(2)(1:9) == 'DAY-TYPES' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bDAYTYPE_block_open = .false. 
              call mimo_debug('DAYTYPES block closed')
            else 
              bDAYTYPE_block_open = .false. 
              call mimo_debug('DAYTYPES block opened')
            endif 
          endif           

C.........CONTROL LOOPS block
          if ( cLnWords(2)(1:) == 'CONTROL-LOOPS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bCONTROLS_block_open = .false. 
              call mimo_debug('CONTROLS block closed')
            else 
              bCONTROLS_block_open = .false. 
              call mimo_debug('CONTROLS block opened')
            endif 
          endif   
          
C.........LOOP block
          if ( cLnWords(2)(1:) == 'LOOP' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bLOOP_block_open = .false. 
              call mimo_debug('LOOP block closed')
            else 
              bLOOP_block_open = .false. 
              call mimo_debug('LOOP block opened')
            endif 
          endif   
                    
C.........LOOP DATA block
          if ( cLnWords(2)(1:) == 'LOOP-DATA' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bLOOPDATA_block_open = .false. 
              call mimo_debug('LOOP DATA block closed')
            else 
              bLOOPDATA_block_open = .false. 
              call mimo_debug('LOOP DATA block opened')
            endif 
          endif   

C-----------------------------------------------------------------------
C       If line isn't a flag, parse data according to currently-open 
C       block 
C-----------------------------------------------------------------------
C.......Parse sensors:
        elseif ( bSENSOR_block_open ) then 
C..........EACH line contains 6 data items describing a sensor:
C.......... NAME, w, x, y, z, sensed-property
            if ( iWdCount == 6 ) then 
              iSenCount = iSenCount + 1
              cSenName ( iSenCount ) = cLnWords(1)(1:24)
C.............Location data: 1-4              
              do ii = 1, 4
                iSenLoc  ( iSenCount, ii) =
     &                       iCtoI ( cLnWords(ii+1), bError )
                if (bError) then 
                  bFatal_Error = .true.
                   write (cMsg,'(A,I2,A,I2,A)')
     &                        ' Could not convert location data #',
     &                        ii, ' in sensor #',iSenCount,'.'
                   print*, ' >>> ', iSenLoc  ( iSenCount, ii)
                endif 
              enddo
C.............Sensed property
              cSenName( iSenCount ) = cLnWords(6)(1:12)
              ! Test for sensed property keyword to go here...
            else 
C..............Error trap here...            
                bFatal_Error = .true.
                 write (cMsg,'(A,A,I2,A,I2,A)')
     &                 ' Incorrect number of data items',
     &                 ' provided to sensor #',iSenCount+1,
     &                 ' (expected 6, found ', iWdCount,').'

            endif           
        
        elseif ( bACTUATOR_block_open ) then 
        
C..........EACH line contains 7 data items describing a sensor:
C.......... NAME, w, x, y, z, sensed-property
            if ( iWdCount == 7 ) then 
              iActCount = iActCount + 1
              cActName ( iActCount ) = cLnWords(1)(1:24)
C.............Location data: 1-4              
              do ii = 1, 4
                iActLoc  ( iActCount, ii) =
     &                       iCtoI ( cLnWords(ii+1), bError )
                if (bError) then 
                  bFatal_Error = .true.
                   write (cMsg,'(A,I2,A,I2,A)')
     &                        ' Could not convert location data #',
     &                        ii, ' in actuator #',iActCount,'.'
                   print*, ' >>> ', iActLoc  ( iActCount, ii)
                endif 
              enddo
C.............MAX and min values for act throttling range 

            else 
C..............Error trap here...            
                bFatal_Error = .true.
                 write (cMsg,'(A,A,I2,A,I2,A)')
     &                 ' Incorrect number of data items',
     &                 ' provided to actuator #',iSenCount+1,
     &                 ' (expected 7, found ', iWdCount,').'

            endif                   
        
        
        endif ! <- matches if ( cLnWords(1)(1:1) == '*' ) then...

         
            
      enddo
C-----------------------------------------------------------------------
C     Were fatal errors encountered?
C-----------------------------------------------------------------------      
      if ( bFatal_Error ) then
C........Yes. Warn user and delete temporary file.
         call edisp(iUout, cMsg)
         call edisp(iUout, ' MIMO controls not processed successfully')
         
         iER = 1
      else
      

         call edisp(iUout, ' MIMO controls processed successfully')
         iER = 0
         
      endif 

      
      
      return
      end 
      
C-----------------------------------------------------------------------
C     Rudamentry debugging...
C-----------------------------------------------------------------------      
      subroutine mimo_debug(cDebug_msg)
      implicit none 
C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen      
      integer lnblnk
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------      
      character*72 cDebug_msg
      character*124 cBuffer
      logical bDebug
      parameter ( bDebug = .true. )
      cBuffer=' '
      if (bDebug) then
        
         write(cBuffer, '(A,A)') '>>>>> MIMO: ', 
     &            cDebug_msg(1:lnblnk(cDebug_msg))
     
       call edisp ( iuout, cBuffer )
       
      endif 
      
      
      return 
      end 
      
C-----------------------------------------------------------------------
C     Attempt to coax a string into an integer. 
C-----------------------------------------------------------------------       
      integer function  iCtoI ( cBuffer, bError )
      implicit none 
      character*248 cBuffer 
      logical bError 
      integer iMiscError , lnblnk
      
      read (cBuffer,*, IOSTAT=iMiscError) iCtoI
      if ( iMiscError /= 0 ) then
        bError = .true. 
      else
        bError = .false. 
      endif 
      return 
      end 
      
      
C-----------------------------------------------------------------------
C     Attempt to coax a string into a float. 
C-----------------------------------------------------------------------       
      real function  fCtoR ( cBuffer, bError )
      implicit none 
      character*248 cBuffer 
      logical bError 
      integer iMiscError 
      
      read (cBuffer,*, IOSTAT=iMiscError) fCtoR
      
      if ( iMiscError /= 0 ) then
        bError = .true. 
      else
        bError = .false. 
      endif 
      return 
      end       
      