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

C=======================================================
C  Functions:
C
C    - mimo_ctl_read: Read MIMO portion of a .ctl file
C
C    - mimo_ctl_write: Write MIMO portion of a .ctl file
C
C    - fMIMOSensor: Return sensed value for given sensor
C
C    - fMIMOActuator: Apply specified control signal to 
C          given actuator.
C
C    - MIMO_Invoke_Ctl: Invoke control loops 
C
C    - MIMO_Invoke_Plt_Ctl, 
C      MIMO_Invoke_Bld_Ctl: 
C          domain specific interfaces to MIMO_Invoke_Ctl
C
C    - bLawApplicable: Determine if control law is 
C          applicable in current domain 
C
C    - mimo_debug: interface for rudimentary debugging
C
C    - bParseDate: Parse an ISO date string 
C
C    - bMatchNameToIndex: Rerurn indicies of matching rows 
C          in two arrays 
C
C    - fCtoR: coax a string into a float 
C
C    - iCtoi: coax a string into an integer
C
C=======================================================

C--------------- MIMO_ctl_read -----------------------------------------
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
      subroutine MIMO_ctl_read ( iUnit, iER )
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
      character*32 cBuffer        ! misc buffer


C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------  
      integer iEGetArrW                ! Split string into words.
      integer iCtoI                    ! Turn a string into an integer
      real    fCtoR                    ! Turn a string into a float 
      logical bParseDate               ! Parse an ISO date (DD-MM-YYYY)
      logical bMatchNameToIndex        ! Search for strings in a list 
C-----------------------------------------------------------------------  
C-----------------------------------------------------------------------  
      
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
      iSenCount  = 0
      iActCount  = 0
      iDayCount  = 0
      iPerCount  = 0
      iLoopCount = 0
      
C----------------------------------------------------------------------
C     Contextual message
C----------------------------------------------------------------------
      write (cContext, '(A)') ' Parsing MIMO section of control file...'
      call edisp ( iuout, ' ' )
      call edisp ( iuout, cContext)
      cContext = ' ' 

C-----------------------------------------------------------------------
C     Loop through file 
C-----------------------------------------------------------------------      
      
      do while ( iFile_read_err == 0 .and. 
     &           bMIMO_block_open           )
      
C.......Empty buffer
        cLnBuffer = ' '
C.......Get line & strip comments
        call LStripc (iUnit, cLnBuffer, 0, iDummy, 0,
     &          'READ MIMO CONTROLS',
     &          iFile_read_err)
        
        

C.......Split words
        iWdCount = iEGetArrW ( cLnBuffer, cLnWords )

       

C-----------------------------------------------------------------------
C       Parse block-open/block-close flags beginning with '*'
C-----------------------------------------------------------------------
        if ( cLnWords(1)(1:1) == '*' ) then

C.........VERSION NUMBER 
          if ( cLnWords(2)(1:7) == 'VERSION' ) then
            fMIMOVersion = fCtoR ( cLnWords(3), bError ) 
            if ( bError ) then 
              bFatal_Error = .true. 
              write (cMsg,'(A,A,A)')
     &             ' Could not parse version number (',
     &             cLnWords(3)(1:lnblnk(cLnWords(3))),
     &             ').'
              call edisp(iUout, cMsg)
            endif 
          endif 

C.........MIMO block          
          if ( cLnWords(2)(1:4) == 'MIMO' .and.
     &         cLnWords(3)(1:3) == 'END'         ) then
            bMIMO_block_open = .false. 
          endif 
        
C.........SENSOR block
          if ( cLnWords(2)(1:7) == 'SENSORS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bSENSOR_block_open = .false. 
            else 
              bSENSOR_block_open = .true. 
            endif 
          endif 

C.........ACTUATOR block
          if ( cLnWords(2)(1:9) == 'ACTUATORS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bACTUATOR_block_open = .false. 
            else 
              bACTUATOR_block_open = .true. 
            endif 
          endif 
          
C.........PERIOD block
          if ( cLnWords(2)(1:7) == 'PERIODS' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bPERIOD_block_open = .false. 
            else 
              bPERIOD_block_open = .true.
            endif 
          endif           
          
C.........DAYTYPES block
          if ( cLnWords(2)(1:9) == 'DAY-TYPES' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bDAYTYPE_block_open = .false. 
            else 
              bDAYTYPE_block_open = .true. 
            endif 
          endif           
          
C.........LOOP block
          if ( cLnWords(2)(1:4) == 'LOOP' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bLOOP_block_open = .false. 
            else 
              bLOOP_block_open = .true. 
C.............Increment loop counter
              iLoopCount = iLoopCount + 1
              
C.............Initialize data counter for this loop. 
              iLoopDataCount(iLoopCount) = 0
              
            endif 
          endif   
                    
C.........LOOP DATA block
          if ( cLnWords(2)(1:4) == 'DATA' ) then
            if ( cLnWords(3)(1:3) == 'END' ) then 
              bLOOPDATA_block_open = .false. 
              call mimo_debug('DATA block closed')
            else 
              bLOOPDATA_block_open = .true. 
              call mimo_debug('DATA block opened')
            endif 
          endif   

C-----------------------------------------------------------------------
C       If line isn't a flag, parse data according to currently-open 
C       block 
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C       Parse sensors:
C-----------------------------------------------------------------------
        elseif ( bSENSOR_block_open ) then 
C..........EACH line contains 7 data items describing a sensor:
C.......... NAME, DOMAIN, OBJECT w, x, y, sensed-property
            if ( iWdCount == 7 ) then 
              iSenCount = iSenCount + 1
              cSenName ( iSenCount ) = cLnWords(1)(1:24)
              cSenDomain (iSenCount) = cLnWords(2)(1:3)
              cSenObject (iSenCount) = cLnWords(3)(1:12)
C.............Location data: 1-3              
              do ii = 1, mMIMOLoc
                iSenLoc  ( iSenCount, ii) =
     &                       iCtoI ( cLnWords(ii+3), bError )
                if (bError) then 
                  bFatal_Error = .true.
                   write (cMsg,'(A,I2,A,I2,A)')
     &                        ' Could not parse location data #',
     &                        ii, ' in sensor #',iSenCount,'.'
                   call edisp(iUout, cMsg)
                endif 
              enddo
C.............Sensed property
              cSenProp( iSenCount ) = cLnWords(7)(1:12)
              ! Test for sensed property keyword to go here...
            else 
C..............Error trap here...            
                bFatal_Error = .true.
                write (cMsg,'(A,A,I2,A,I2,A)')
     &                 ' Incorrect number of data items',
     &                 ' provided to sensor #',iSenCount+1,
     &                 ' (expected 7, found ', iWdCount,').'
                call edisp(iUout, cMsg)

            endif           
C-----------------------------------------------------------------------
C       Parse actuators
C-----------------------------------------------------------------------        
        elseif ( bACTUATOR_block_open ) then 
        
C.........EACH line contains 8 data items describing an actuator:
C.........NAME, DOMAIN, OBJECT,  w, x, y, min val, max val
          if ( iWdCount == 8 ) then 
            iActCount = iActCount + 1
            cActName ( iActCount ) = cLnWords(1)(1:24)
            cActDomain ( iActCount ) = cLnWords(2)(1:3)
            cActObject ( iActCount ) = cLnWords(3)(1:3)
C...........Location data: 1-3
            do ii = 1, mMIMOLoc
              iActLoc  ( iActCount, ii) =
     &                     iCtoI ( cLnWords(ii+3), bError )
              
              if (bError) then 
                bFatal_Error = .true.
                 write (cMsg,'(A,I2,A,I2,A)')
     &                      ' Could not parse location data #',
     &                      ii, ' in actuator #',iActCount,'.'
                 call edisp(iUout, cMsg)
                
              endif
              
            enddo
            
            
C...........MAX and min values for act throttling range 

            fActMIN ( iActCount ) = fCtoR ( cLnWords(7), bError )
            if (bError) then 
              bFatal_Error = .true.
              write (cMsg,'(A,A,I2,A)')
     &                    ' Could not parse MIN value',
     &                    'in actuator #',iActCount,'.'
              call edisp(iUout, cMsg)
            endif
            
            fActMAX ( iActCount ) = fCtoR ( cLnWords(8), bError )
            if (bError) then 
              bFatal_Error = .true.
               write (cMsg,'(A,A,I2,A)')
     &                    ' Could not parse MAX value',
     &                    'in actuator #',iActCount,'.'
              call edisp(iUout, cMsg)
            endif            
            
          else 
C............Error trap here...            
              bFatal_Error = .true.
              write (cMsg,'(A,A,I2,A,I2,A)')
     &               ' Incorrect number of data items',
     &               ' provided to actuator #',iActCount+1,
     &               ' (expected 8, found ', iWdCount,').'
              call edisp(iUout, cMsg)
          
          endif                   
C-----------------------------------------------------------------------
C       Parse Daytypes 
C-----------------------------------------------------------------------
        elseif ( bDAYTYPE_block_open ) then
C.........Each line contains 3 data items describing a day-type:
C.........   NAME Start-day (DD-MM-YYYY) End-day (DD-MM-YYYY)
          if ( iWdCount == 3 ) then
            iDayCount  = iDayCount + 1
            cDayName(iDayCount)     = cLnWords(1)(1:24)
C...........Use function bParseDate to turn ISO date into ESP-r-friendly            
C...........data
            bError = bParseDate ( cLnWords(2), 
     &                            iDayStartDD(iDayCount), 
     &                            iDayStartMM(iDayCount), 
     &                            iDayStartYYYY(iDayCount), 
     &                            iDayStartDOY(iDayCount)    )
            
            if ( bError ) then 
              bFatal_Error = .true. 
              write (cMsg,'(A,A,A,I2,A)')
     &             ' Could not parse start date (',
     &             cLnWords(2)(1:lnblnk(cLnWords(2))),
     &             ') provided with day-type #',iDayCount+1,'.'
              call edisp(iUout, cMsg)            
            endif 
            
C...........Use function bParseDate to turn ISO date into ESP-r-friendly            
C...........data
            bError = bParseDate ( cLnWords(3), 
     &                            iDayEndDD(iDayCount), 
     &                            iDayEndMM(iDayCount), 
     &                            iDayEndYYYY(iDayCount), 
     &                            iDayEndDOY(iDayCount)    )
            
            if ( bError ) then 
              bFatal_Error = .true. 
              write (cMsg,'(A,A,A,I2,A)')
     &             ' Could not parse end date (',
     &             cLnWords(3)(1:lnblnk(cLnWords(3))),
     &             ') provided with day-type #', iDayCount+1,'.'
              call edisp(iUout, cMsg)            
            endif            
            
          else 
            bFatal_Error = .true.
             write (cMsg,'(A,A,I2,A,I2,A)')
     &             ' Incorrect number of data items',
     &             ' provided to day-type #',iDayCount+1,
     &             ' (expected 3, found ', iWdCount,').'
             call edisp(iUout, cMsg)
          endif 
        
C-----------------------------------------------------------------------
C       Parse periods 
C-----------------------------------------------------------------------
        elseif ( bPERIOD_block_open ) then 
C.........Each line has 3 data items:
C.........  NAME, start-hr, end-hr
            if ( iWdCount == 3 ) then
              iPerCount = iPerCount + 1 
C.............Period name
              cPerName(iPerCount)  = cLnWords(1)(1:24)
C.............Period start hour 
              iPerStarthr(iPerCount) = iCtoI ( cLnWords(2), bError )
              if ( bError ) then 
                bFatal_Error = .true. 
                write (cMsg, '(A,A,A,I2,A)')
     &             ' Could not parse start hour (',
     &             cLnWords(2)(1:lnblnk(cLnWords(2))),
     &             ') in period #',iPerCount,'.'
                call edisp(iUout, cMsg)
              endif 
C.............Period end hour 
              iPerEndhr(iPerCount) = iCtoI ( cLnWords(3), bError )
              if ( bError ) then 
                bFatal_Error = .true. 
                write (cMsg, '(A,A,A,I2,A)')
     &             ' Could not parse end hour (',
     &             cLnWords(3)(1:lnblnk(cLnWords(3))),
     &             ') in period #',iPerCount,'.'
                call edisp(iUout, cMsg)
              endif 
            else 
C.............Wrong number of data items provided.
              bFatal_Error = .true.
              write (cMsg,'(A,A,I2,A,I2,A)')
     &             ' Incorrect number of data items',
     &             ' provided to period #',iPerCount+1,
     &             ' (expected 3, found ', iWdCount,').'
              call edisp(iUout, cMsg)
            endif 
        
C-----------------------------------------------------------------------
C      Process Loops:
C 
C        Unlike all other blocks, a loop may contain multiple lines, 
C        in TOKEN VALUE VALUE VALUE format. 
C
C        Valid tokens: NAME; TYPE; PRIORITY; DAYTYPES; PERIODS;
C                      SENSORS; ACTUATORS; 
C
C        A Loop block may also contain a data block, denoted with the 
C        '* DATA' / '* DATA END' demarcations.  If a data block 
C        is open, the bLOOPDATA_block_open flag will be set above.
C
C-----------------------------------------------------------------------
      elseif ( bLOOP_block_open ) then 
C.......Check if loopdata block is open
        if ( bLOOPDATA_block_open ) then 
C.........Loop through data items on line, and append to data array
          do ii=1, iWdCount 
            fLoopData(iLoopCount, iLoopDataCount(iLoopCount)+ii)
     &             = fCtoR ( cLnWords ( ii ), bError )
            
            if (bError) then 
              
              bFatal_Error = .true. 
              write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &          ' Could not convert data item #', 
     &                 iLoopDataCount(iLoopCount)+ii,
     &          '(',cLnWords ( ii )(1:lnblnk(cLnWords(ii))),
     &          ') in control loop #', iLoopCount, '.'
              call edisp(iUout, cMsg)
            
            endif 
            
          enddo  ! <- matches do ii=1, iWdCount 
            
C.........Add the data items on this line to iLoopDataCount        
          iLoopDataCount(iLoopCount) = iLoopDataCount(iLoopCount) 
     &                                        + iWdCount
        
        else
        
C.........Line describes token:value-list. 
C.........Valid tokens begin with 'LOOP-'
          
          if     ( cLnWords(1)(1:4)  == 'NAME'      ) then

C...........LOOP NAME: read single word
            cLoopName(iLoopCount) = cLnWords(2)(1:24)
         
          elseif ( cLnWords(1)(1:3)  == 'LAW'      ) then
          
C...........LOOP LAW: read single word 
            cLoopLaw(iLoopCount) = cLnWords(2)(1:24)
            
C...........Match word to defined keywords.
            do ii = 1, iMIMOLawCount
            
              bError = .true.
              if ( cLoopLaw(iLoopCount)
     &                  (1:lnblnk(cLoopLaw(iLoopCount)))
     &             == cControlLawNames(ii)
     &                  (1:lnblnk(cControlLawNames(ii))) ) then
                
                iLoopLaw(iLoopCount) = ii
                bError = .false. 
              endif  
            enddo
            
            if (bError) then 
              
              bFatal_Error = .true. 
              write (cMsg, '(A,A,A,I2,A)')
     &          ' Could not resolve control law (', 
     &                 cLnWords(2)(1:lnblnk(cLnWords(2))),
     &          ') in control loop #', iLoopCount, '.'
              call edisp(iUout, cMsg)

            endif            
            
          elseif ( cLnWords(1)(1:8) == 'PRIORITY'  ) then
          
C...........LOOP PRIORITY: Read a single integer value
            iLoopPriority(iLoopCount) = iCtoI ( cLnWords(2), bError )
            
            if (bError) then 
              
              bFatal_Error = .true. 
              write (cMsg, '(A,A,A,I2,A)')
     &          ' Could not convert loop priority (', 
     &                 cLnWords(2)(1:lnblnk(cLnWords(2))),
     &          ') in control loop #', iLoopCount, '.'
              call edisp(iUout, cMsg)

            endif

          elseif ( cLnWords(1)(1:8) == 'DAYTYPES'  ) then
          
C...........LOOP DAYTYPES: Read a list of daytypes
            if ( iWdCount - 1 <= mMIMOList ) then
                
                do ii = 2, iWdCount
                
                  cLoopDayTypes(iLoopCount, ii-1) = cLnWords(ii)(1:24)
                
                enddo
                
                iLoopDayTypeCount ( iLoopCount ) = iWdCount - 1
                
C...............Attempt to match specified day-types to list
                
                write (cBuffer, '(A)' ) 'day-types'
                if (.not. bMatchNameToIndex ( 
     &                      cBuffer,
     &                      iLoopCount,
     &                      iLoopDayTypeCount,
     &                      iDayCount,
     &                      cLoopDayTypes,
     &                      cDayName,
     &                      iLoopDayTypes      ) ) then 

                  bFatal_Error = .true. 
                  write (cMsg, '(A,I2)')
     &              ' Could not resolve daytypes for loop #', 
     &                    iLoopCount
                  call edisp(iUout, cMsg)
                endif 
                
            else
             
              bFatal_Error = .true. 
              write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &          ' Too many daytypes were specified for loop #', 
     &                iLoopCount, ' (', mMIMOList, 
     &          ' are permitted, but ',iWdCount - 1,
     &          ' were provided).'
     
              call edisp(iUout, cMsg)
              
            endif ! <- matches if ( iWdCount - 1 <= mMIMOList ) then
          
          elseif ( cLnWords(1)(1:7) == 'PERIODS'   ) then
          
C...........LOOP PERIODS: Read a list of period names
            if ( iWdCount - 1 <= mMIMOList ) then
                
                do ii = 2, iWdCount
                
                  cLoopPeriods(iLoopCount, ii-1) = cLnWords(ii)(1:24)
                
                enddo
                
                iLoopPeriodCount ( iLoopCount ) = iWdCount - 1
                
C...............Attempt to match periods to list
                
                write (cBuffer, '(A)' ) 'period'
                if (.not. bMatchNameToIndex ( 
     &                      cBuffer,
     &                      iLoopCount,
     &                      iLoopPeriodCount,
     &                      iPerCount,
     &                      cLoopPeriods,
     &                      cPerName,
     &                      iLoopPeriods      ) ) then 
                
                  bFatal_Error = .true. 
                  write (cMsg, '(A,I2)')
     &              ' Could not resolve periods for loop #', 
     &                    iLoopCount
                  call edisp(iUout, cMsg)
                endif                 
                
            else
             
              bFatal_Error = .true. 
              
              write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &          ' Too many periods were specified for loop #', 
     &                iLoopCount, ' (', mMIMOList, 
     &          ' are permitted, but ',iWdCount - 1,
     &          ' were provided).'
     
              call edisp(iUout, cMsg)
            
            endif ! <- matches if ( iWdCount - 1 <= mMIMOList ) then
            

          
          elseif ( cLnWords(1)(1:7) == 'SENSORS'   ) then
          
          
C...........LOOP SENSORS: Read a list of sensor names 
            if ( iWdCount - 1 <= mMIMOList ) then
                
                do ii = 2, iWdCount
                
                  cLoopSensors(iLoopCount, ii-1) = cLnWords(ii)(1:24)
                
                enddo
                
                iLoopSensorCount ( iLoopCount ) = iWdCount - 1
                
C...............Attempt to match specified sensors to list
                
                write (cBuffer, '(A)' ) 'sensor'
                if (.not. bMatchNameToIndex ( 
     &                      cBuffer,
     &                      iLoopCount,
     &                      iLoopSensorCount,
     &                      iSenCount,
     &                      cLoopSensors,
     &                      cSenName,
     &                      iLoopSensors      ) ) then 
                
                  bFatal_Error = .true. 
                  write (cMsg, '(A,I2)')
     &              ' Could not resolve sensors for loop #', 
     &                    iLoopCount
                  call edisp(iUout, cMsg)
                endif 
                
                
            else
             
              bFatal_Error = .true. 
              
              write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &          ' Too many sensors were specified for loop #', 
     &                iLoopCount, ' (', mMIMOList, 
     &          ' are permitted, but ',iWdCount - 1,
     &          ' were provided).'
     
              call edisp(iUout, cMsg)
            
            endif ! <- matches if ( iWdCount - 1 <= mMIMOList ) then
          
          
          elseif ( cLnWords(1)(1:9) == 'ACTUATORS' ) then

C...........LOOP ACTUATORS: Read a list of actuator names 
            if ( iWdCount - 1 <= mMIMOList ) then
                
                do ii = 2, iWdCount
                
                  cLoopActuators(iLoopCount, ii-1) = cLnWords(ii)(1:24)
                
                enddo
                
                iLoopActuatorCount ( iLoopCount ) = iWdCount - 1
                
C...............Attempt to match specified actuators to list
                
                write (cBuffer, '(A)' ) 'actuator'
                if (.not. bMatchNameToIndex ( 
     &                      cBuffer,
     &                      iLoopCount,
     &                      iLoopActuatorCount,
     &                      iActCount,
     &                      cLoopActuators,
     &                      cActName,
     &                      iLoopActuators      ) ) then 
                
                  bFatal_Error = .true. 
                  write (cMsg, '(A,I2)')
     &              ' Could not resolve actuators for loop #', 
     &                    iLoopCount
                  call edisp(iUout, cMsg)
                endif 
                
            else
             
              bFatal_Error = .true. 
              
              write (cMsg, '(A,I2,A,I2,A,I2,A)')
     &          ' Too many actuators were specified for loop #', 
     &                iLoopCount, ' (', mMIMOList, 
     &          ' are permitted, but ',iWdCount - 1,
     &          ' were provided).'
   
              call edisp(iUout, cMsg)

            endif 
            
          else 
          
C.,,........Token name is invalid            
            bFatal_Error = .true.
            write (cMsg, '(A,A)')
     &        ' Unknown LOOP token: ',cLnWords(1)(1:lnblnk(cLnWords(1)))
            call edisp(iUout, cMsg)
            call UsrMsg(cMsg,cMsg,'W') 
          endif ! <- matches if ( cLnWords(1)(1:5) == 'LOOP-NAME' ) then
          
        endif ! <- matches if ( bLOOPDATA_block_open ) then

      endif ! <- matches if ( cLnWords(1)(1:1) == '*' ) then...

         
            
      enddo
C-----------------------------------------------------------------------
C     Were fatal errors encountered?
C-----------------------------------------------------------------------      
      if ( bFatal_Error ) then
C........Yes. Warn user and delete temporary file.
         call edisp(iUout, ' MIMO controls: Parsing error')
         
         iER = 1
      else
      

         call edisp(iUout, ' MIMO controls processed successfully')
         iER = 0
         
      endif 

      call edisp ( iuout, ' ' )
      
      
      return
      end 
      
      
C--------------------- procedure MIMO_Invoke_Ctl -----------------------
C     Determine which control loops are relevant in the current context,
C     and invoke them      
C-----------------------------------------------------------------------
      subroutine MIMO_Invoke_Ctl ( cDomain, iLoc_1, iLoc_2, iLoc_3 )
      implicit none 
#include "mimo_ctl.h"
C-----------------------------------------------------------------------      
C     Passed variables 
C-----------------------------------------------------------------------            
      
      character*3 cDomain             ! String describing the current domain
      integer iLoc_1, iLoc_2, iLoc_3  ! Integers specifying location within 
                                      !    domain
      
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      
      integer ii                      ! Counter
      character*248 cTemp             ! Temp buffer
      
C-----------------------------------------------------------------------
C     Write contextual buffer. cTemp is used to guard against 
C     potential buffer overflow. 
C-----------------------------------------------------------------------
      
      write ( cTemp, '(A,I2,A,I2,A,I2,A,A,A)' ) 
     &  ' Invoking MIMO control functions for location ', iLoc_1, ',',
     &  iLoc_2,',', iLoc_3, ' in domain "', cDomain(1:3),'"'
     
      write ( cContext, '(A)' ) cTemp(1:124)
      call mimo_debug ( cContext )

C-----------------------------------------------------------------------
C     Loop through specified controls. If 
C-----------------------------------------------------------------------
      if ( cDomain(1:3) == 'bld' ) then
      
      elseif ( cDomain(1:3) == 'plt' ) then
      
      endif 

      return 
      end 
      
C--------------------- procedure MIMO_Invoke_Plt_Ctl -------------------
C     This is a simple interface to MIMO_Invoke_Ctl.      
C-----------------------------------------------------------------------
      subroutine MIMO_Invoke_Plt_Ctl (  )
      implicit none 
C-----------------------------------------------------------------------      
C     Local variables 
C-----------------------------------------------------------------------            
      
      integer iLoc_1, iLoc_2, iLoc_3  ! Integers specifying location within 
                                      !    domain (dummy variables )
C.....Initialize dummy varaibles                                      
      iLoc_1 = 0
      iLoc_2 = 0 
      iLoc_3 = 0 

C.....Invoke plant control loops.
      call MIMO_Invoke_Ctl ( 'plt', iLoc_1, iLoc_2, iLoc_3  )
      
      return
      end 

      
      
C--------------------- function fMIMOSensor ----------------------------
C     Return the most recent sensed value for the sensor given by 
C     iSensor_Index (input)
C-----------------------------------------------------------------------      
      real function fMIMOSensor ( iSensor )
      implicit none 
#include "mimo_ctl.h"
#include "building.h"
#include "plant.h"
C-----------------------------------------------------------------------      
C     Passed arguements 
C-----------------------------------------------------------------------      
      
      integer iSensor              ! Index of current sensor
      
C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen      
      integer lnblnk

C-----------------------------------------------------------------------      
C     Local variables 
C-----------------------------------------------------------------------
      
      integer iPltComp             ! Index of plant component
      integer iPltCompVar          ! Component specific index: 
                                   !     node/variable/connection index
      logical bFatal_Error         ! Error flag
      
C-----------------------------------------------------------------------      
C     Determine where sensor is located, and collect desired property.
C     
C       iSenLoc( , 1) >= 0 : Sensor is in specified zone, or in 
C                            the current zone.
C
C-----------------------------------------------------------------------      
      
      write (cContext , '(A,I2,A)') ' MIMO control sensor # ', 
     &           iSensor, ':'
      
            
      if ( iSenLoc( iSensor , 1 ) >= 0 ) then
      
      
      elseif ( iSenLoc ( iSensor, 1 ) == -1 ) then  
C-----------------------------------------------------------------------
C       Sensor location is in a plant component.
C-----------------------------------------------------------------------      
        iPltComp    = iSenLoc ( iSensor, 2 )   ! Component index
        iPltCompVar = iSenLoc ( iSensor, 3 )   ! Pointer to sensed location
        
C.......The actual sesnor location depends on the sensed property
        if  ( cSenProp( iSensor )(1:4) == 'TEMP' ) then 
           
        elseif  ( cSenProp( iSensor )(1:4) == 'FLOW' )  then
C...........          
        elseif  ( cSenProp( iSensor )(1:7) ==  'ADD-OUT' ) then
          
        elseif ( cSenProp( iSensor )(1:7) ==  'CTL-VAR' ) then
          
        else
          
          bFatal_Error = .true. 
          write ( cMsg, '(A,A,A)' ) ' Sensed property "',
     &      cSenProp( iSensor )(1:lnblnk( cSenProp( iSensor ) ) ),
     &      '" is invalid.'
          call UsrMsg (cContext, cMsg, 'W')
          call UsrMsg (cContext, cMsg, 'F')
          
        endif 
        
      
      endif  ! <- matches ' if ( iSenLoc( iSensor , 1 ) >= 0 ) then'

      return
      end
C--------------------- subroutine mimo_debug ---------------------------
C     Rudamentry debugging
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
      character*(*) cDebug_msg
      character*124 cBuffer
      logical bDebug
      parameter ( bDebug = .true. )
      integer iLen
      iLen = max(1, min(116,lnblnk(cDebug_msg)))
      if (bDebug) then
        
         write(cBuffer, '(A,A)') ' MIMO: ', 
     &            cDebug_msg(1:iLen)
     
       call edisp ( iuout, cBuffer )
       
      endif 

      return 
      end 
C----------------------- function bParseDate ---------------------------      
C     Read an ISO date (DD-MM or DD-MM-YYYY), and attempt to parse. 
C
C     input: cDate - Character string containing date
C 
C     output: bParseDate - True for error, false for OK. 
C             iDD        - Day of month (1-29/30/31)
C             iMM        - Month  (1-12)
C             iYYYY      - YEAR 
C             iDOY       - Day of year (1-365)
C
C-----------------------------------------------------------------------      
      logical function bParseDate ( cDate, iDD, iMM, iYYYY, iDOY)
      implicit none
C     Passed arguements
      character*248 cDate 
      character*124 cMSG
      integer iDD, iMM, iYYYY, iDOY 
      
C     Local vars:
      integer iLen
      logical bYrSpecd
      logical bFormatErr
      
     
C     References
      integer lnblnk      
      integer iCtoI        ! Convert a string into an integer.

C     Initialize bFormatErr
      bFormatErr = .false. 
      
      bYrSpecd = .false.
      
C----------------------------------------------------------------------- 
C     Check that date conforms to expected format.      
C----------------------------------------------------------------------- 
      iLen = lnblnk (cDate) 
C     Date must be 5  or 10 characters ling       
      if ( iLen == 5 .or. iLen == 10 ) then 
C       Has year been provided?
        if ( iLEN == 10 ) bYrSpecd = .true.
      else
C       Date is wrong length!      
        bFormatErr = .true. 
      endif
      
C     Now check if date has requisite dashes      
      if ( .not. bFormatErr ) then
        if ( cDate(3:3) /= '-' )               bFormatErr = .true. 
        if ( bYrSpecd .and. cDate(6:6) /= '-') bFormatErr = .true.
      endif 
C----------------------------------------------------------------------- 
C     If format is ok, try to convert to date into numbers
C----------------------------------------------------------------------- 
      if ( .not. bFormatErr )  iDD   = iCtoI ( cDate(1:2),  bFormatErr )
      if ( .not. bFormatErr )  iMM   = iCtoI ( cDate(4:5),  bFormatErr )
      if ( bYrSpecd ) then 
        if ( .not. bFormatErr )  
     &            iYYYY = iCtoI ( cDate(7:10), bFormatErr )
      else 
        iYYYY = 0 
      endif 
      
C----------------------------------------------------------------------- 
C     If format is ok and data was read correctly, try to convert 
C     day-month data into day-of-year. 
C-----------------------------------------------------------------------       
      if ( .not. bFormatErr ) then
        call eday(iDD,iMM,iDOY)
      endif
      
C     (We should put some range-checking here...)
      
C     Update function result
      bParseDate = bFormatErr
      
      return 
      end 
      
C-----------------------------------------------------------------------
C     Return array iIndicies containing the index of each row in 
C     array cSearchList that matches array cSourceList
C-----------------------------------------------------------------------

      logical function bMatchNameToIndex ( cNameType, 
     &                                     iInstance,
     &                                     iSourceLength,
     &                                     iSearchLength,
     &                                     cSourceList, 
     &                                     cSearchList, 
     &                                     iIndicies    )
      implicit none 
#include "mimo_ctl.h"            
C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen      
      integer lnblnk
      
C-----------------------------------------------------------------------
C     Passed arguements 
C-----------------------------------------------------------------------
      
      character*32 cNameType    ! (input) description of the type 
                                !         of text to be matched.
      
      integer iInstance         ! (input) current loop 
      
      integer iSourceLength     ! (input) Length of Source list 
      
      integer iSearchLength     ! (input) Length of Search list 
      
      character*24 cSourceList(mMIMOLoop,mMIMOList) 
                                          ! (input) 
                                          ! List of strings to be located
                                          
      character*24 cSearchList(mMIMOList)    
                                          ! (input) 
                                          ! List of strings to seach in
                                             
      integer iIndicies (mMIMOLoop, mMIMOList ) 
                                          ! (output)
                                          ! List of matching indicies
                                             
C-----------------------------------------------------------------------
C     Local arguements
C-----------------------------------------------------------------------
      
      integer ii,jj                ! counters
      logical bMatchesOk           ! global match flag 
      logical bMatched             ! list match flag
      integer iL1                  ! Source string length
      integer iL2                  ! Search string length 
C-----------------------------------------------------------------------
C     Search for matches
C-----------------------------------------------------------------------
      
      bMatchesOk = .true.

C.....For each item in source list...     
      do ii = 1, iSourceLength 
      
       bMatched = .false. 
      
       iL1 = lnblnk( cSourceList(iInstance,ii) )
        
C.........Loop through search list, and check for match         
        do jj = 1, iSearchLength
        
          iL2 = lnblnk( cSearchList(jj) )
          if ( cSourceList(iInstance,ii)(1:iL1) 
     &                ==  cSearchList(jj)(1:iL2) ) then
            
            bMatched = .true. 
            
            iIndicies ( iInstance, ii ) = jj
            
          endif 
        enddo 

C.......Was source string matched?
        if ( .not. bMatched ) then
        
          write ( cMsg, '(A,A,A,A,A,A,A)' )
     &      '   - Could not match specified ', 
     &      cNameType(1:lnblnk(cNameType)), ' "', 
     &      cSourceList(iInstance,ii)(1:iL1),
     &      '" in list of ',cNameType(1:lnblnk(cNameType)),'s.'
     
     
          call edisp ( iuout, cMsg )

          bMatchesOk = .false. 
          
        endif 
      
      enddo 
      
      
      bMatchNameToIndex = bMatchesOk
      
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
      