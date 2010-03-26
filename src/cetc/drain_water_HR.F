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
C================== drain_water_HR.F ==================================
C
C Date: March 18, 2010
C Author: Alex Ferguson
C Copyright: Natural Resources Canada
C
C This file contains procedures to model drain-water heat recovery
C equipment.
C
C GENERAL ROUTINES:
C
C   process_dwhr_inputs: Read a dwhr model definition and validate 
C      inputs.
C
C   predict_DWHR_outlet_temp: Apply DWHR model to predict outlet 
C      temperature
C
C   bStringsMatch: Logical function comparing two strings and returning 
C      'true' if they are equivlent
C
C   iMatchTokens: Integer function that searches a list of tokens and 
C      returns the index of of a matching value
C
C   dwhr_error_handler: Routine that handles reporting errors from 
C      the drain-water HR model.
C
C REFERENCES: 
C
C   Ferguson, A. 2010. Modelling Drain Water Heat Recovery Equipment 
C      in Building Energy simulation. NRCan report. 
C
C----------------------------------------------------------------------
C
C------------------ process_dwhr_inputs -------------------------------
C
C  ABSTRACT:
C
C  process_dwhr_inputs reads
C
C  Inputs
C
C    bAutoOK: flag indicating simulator is in silent running mode
C
C  Outputs
C
C    bFatalError : flag indicating that error was encountered
C
C----------------------------------------------------------------------

      subroutine process_dwhr_inputs( iBaseFileNumber,
     &                                bAutoOK,
     &                                bFatalError  )
      implicit none
      include "drain_water_HR.h"

C.....Passed arguements
      integer iBaseFileNumber          ! Base file number for I/O
      logical bFatalError              ! flag for fatal parsing error.
      logical bAutoOK

C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C----------------------------------------------------------------------
C     ESP-r references
C----------------------------------------------------------------------

      integer lnblnk           ! returns # of non-blank characters in
                               ! a string.

      integer iEGetArrW        ! returns an array containing the
                               ! words found in a string.

      real fRecover_lds_from_bin_file ! Recovers values from the
                                      ! binary profile file.

      logical bStringsMatch    ! Function comparing two strings for
                               ! equivlence.

      integer iCtoI_err        ! Converts a string to int.
      real    fCtoR_err        ! Converts a string to float.

      integer iMatchTokens     ! Finds a string in an array of tokens

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------

      integer iFileIOError     ! Status of file I/O actions.

      logical bFileFinished    ! Flag denoting the end of a file was
                               ! reached.


      integer iSystem

C.....Variables for reading contents of a line.
      character*248 cLnBuffer     ! Line buffer
      character*248 cLnWords(124) ! words on the line
      integer iWordCount          ! # of words in a string
      integer iDummy              ! Not used
      integer iLineCount
      integer iNonCommentLineCount
      logical bError
      logical bFound

C.....Counters for tokens
      integer iTokenCount(iMaxNumDwhr)  ! Max number of tokens per system
      integer iToken


C.....Flags for various components of the DWHR model
      logical bValidFileTag
      logical bValidVersion

      logical bSystemDefOpen

      logical bValidname               (iMaxNumDwhr)
      logical bValidRating             (iMaxNumDwhr)
      logical bValidCoeffA0            (iMaxNumDwhr)
      logical bValidCoeffA1            (iMaxNumDwhr)
      logical bValidCoeffA2            (iMaxNumDwhr)
      logical bValidCoeffA3            (iMaxNumDwhr)
      logical bValidGroundTempFlag     (iMaxNumDwhr)
      logical bValidSpecGroundTemp     (iMaxNumDwhr)
      logical bValidShowerSupplyTemp   (iMaxNumDwhr)
      logical bValidDrainReturnTemp    (iMaxNumDwhr)

C.....Flags for matched & un-matched tokens
      logical bTokenMatched(iMaxNumDwhr, iMaxTokens)

C.....Warning messages
      character*124 cMsg       ! message to be displayed to buffer
      character*124 cContext   ! contextual message



C----------------------------------------------------------------------
C     Begin parsing file
C----------------------------------------------------------------------

      bFatalError  = .false.


C.....File handle: base file number + 1 for any temporary-opened
C.....file (as per ESP-r's convention)
      iDwhrFileNumber = iBaseFileNumber + 1

C----------------------------------------------------------------------
C     Contextual message
C----------------------------------------------------------------------
      write (cContext, '(A,A)') ' Parsing drain-water HR input file: ',
     &   cDwhrFilename(1:lnblnk(cDwhrFilename))
      call edisp ( iuout, ' ' )
      call edisp ( iuout, cContext)


C----------------------------------------------------------------------
C     Open ASCII file
C----------------------------------------------------------------------

C.....Close the file, if it is open
      call erpfree(iDwhrFileNumber, iFileIOError)

      call EFOPSEQ(iDwhrFileNumber,cDwhrFilename,0,iFileIOError)

C.....Was file found/opened successfully?
      File_Open_Error: if ( iFileIOError == 0  ) then

C........YES! Do nothing.

      else

C........NO! Write warning message.
         write(cMsg,'(A,A,A)') ' File ',
     &      cDwhrFilename(1:lnblnk(cDwhrFilename)),
     &      ' could not be read!'

C........Set parse error flag.
         bFatalError  = .true.

C........Call error handler.
         call dwhr_error_handler(bAutoOK, cContext, cMsg)

      endif File_Open_Error

C----------------------------------------------------------------------
C     Parse file line-by-line
C----------------------------------------------------------------------
C.....Set parsing flags
      bFileFinished            = .false.            ! Flag for end-of-file

      bValidFileTag            = .false.
      bValidVersion            = .false.
      bSystemDefOpen           = .false.

      do  iDwhrSystemCount = 1, iMaxNumDwhr, 1
        bValidName              (iDwhrSystemCount) = .false.
        bValidRating            (iDwhrSystemCount) = .false.
        bValidCoeffA0           (iDwhrSystemCount) = .false.
        bValidCoeffA1           (iDwhrSystemCount) = .false.
        bValidCoeffA2           (iDwhrSystemCount) = .false.
        bValidCoeffA3           (iDwhrSystemCount) = .false.
        bValidGroundTempFlag    (iDwhrSystemCount) = .false.
        bValidSpecGroundTemp    (iDwhrSystemCount) = .false.
        bValidShowerSupplyTemp  (iDwhrSystemCount) = .false.
        bValidDrainReturnTemp   (iDwhrSystemCount) = .false.
      enddo

      iDwhrSystemCount = 0

      iLineCount           = 0
      iNonCommentLineCount = 0

C.....Loop that reads file contents.
      ParseLines: do while ( .not. bFileFinished )

C----------------------------------------------------------------------
C       Read a line from the file using 'Lstripc'
C          -> stripc: stips comments from line. Synopsys:
C             stripc ( file #     (input),
C                      line data (output),
C                      # items expected (input, 0 to disable checking),
C                      Error handling (input, 0 for silent)
C                      Contextual message (input)
C                      error status (output)
C                    )
C----------------------------------------------------------------------
        call LStripc (iDwhrFileNumber, cLnBuffer, 0, iDummy, 0,
     &                'DWHR file ',
     &                iFileIOError)

C.......Check status of file-read.
        FileReadError: if ( iFileIOError /= 0 ) then

C.........If read failed, set file-finished flag to true
C.........(causes loop to terminate at next 'done')
          bFileFinished = .true.

        else

C----------------------------------------------------------------------
C         There's another line to read!
C         Get space/comma/tab separated words
C          -> iEGetArrW: returns the number of words encountered
C             and a character array containing the words. Synopsys
C             # of words = ( string, array of words)
C----------------------------------------------------------------------
          iLineCount = iLineCount + 1
          iWordCount = iEGetArrW ( cLnBuffer, cLnWords )

          LineNotEmpty: if ( iWordCount > 0 ) then

C...........Line is not empty, or is not all comments.
            iNonCommentLineCount = iNonCommentLineCount + 1

            ParseLine:
     &      if ( bStringsMatch(cLnWords(1),'*dwhr_system') .and.
     &               bStringsMatch(cLnWords(2),'start')           ) then

C.............Leading tag for DWHR system definition
              if ( bSystemDefOpen ) then

                call dwhr_error_handler( bAutoOK, " ",
     &            '"*dwhr_system, start" entry found without ' //
     &            '"*dwhr_system, end."')
                bFatalError = .true.

              else

                 bSystemDefOpen = .true.
                 iDwhrSystemCount = iDwhrSystemCount + 1

              endif

            elseif( bStringsMatch(cLnWords(1),'*dwhr_system') .and.
     &              bStringsMatch(cLnWords(2),'end')             ) then

C.............Tag closes DWHR system definition
              bSystemDefOpen = .false.


            elseif ( bSystemDefOpen .and.
     &           bStringsMatch(cLnWords(1)(1:1),'*') ) then

C.............Increment token count for this system.
              iTokenCount(iDwhrSystemCount )
     &            = iTokenCount(iDwhrSystemCount ) + 1

C.............Save entries as part of systems definition.

C              cTokens(iDwhrSystemCount,iTokenCount) =
C     &                     cLnWords(1)(2:lnblnk(cLnWords(1)))

              write( cTokens(iDwhrSystemCount,
     &                       iTokenCount(iDwhrSystemCount)), '(A,A)' )
     &               cLnWords(1)(2:lnblnk(cLnWords(1)))

C              cValues(iDwhrSystemCount,iTokenCount) =
C     &                     cLnWords(2)(2:lnblnk(cLnWords(2)))

              write( cValues(iDwhrSystemCount,
     &                       iTokenCount(iDwhrSystemCount)), '(A,A)' )
     &               cLnWords(2)(1:lnblnk(cLnWords(2)))


            elseif( bStringsMatch(cLnWords(1), '*drain_water_HR')) then
C.............Tag defining file as drain-water input file. Must be
C.............first non-comment line in file.

              FirstLineTag: if ( iNonCommentLineCount == 1 ) then

                bValidFileTag = .true.

              else
                write(cMsg, '(A)' )
     &             ' Tag *drain_water_HR must be first entry in file.'
                call dwhr_error_handler( bAutoOK, ' ', cMsg )

                bFatalError = .true.

              endif FirstLineTag

            elseif( bStringsMatch(cLnWords(1), '*version') ) then
C.............Version number. Read in integer.

              VersionWordCount: if ( iWordCount /= 2 ) then
                write (cMsg, '(A,A,I2,A)')
     &            ' Error reading *version tag. ',
     &            'Expected 1 value but found ', iWordCount-1, 'values.'
                call dwhr_error_handler(bAutoOK, ' ', cMsg)
                bFatalError = .true.

              else

                iDwhrFileVersion = iCtoI_err ( cLnWords(2),
     &              'Could not parse file version (*version)', bError )


                if ( bError ) then
                  write (cMsg, '(A,A)') ' Version number (',
     &                cLnWords(2)(1:lnblnk(cLnWords(2))),
     &                ') not understood.'
                  call dwhr_error_handler(bAutoOK, ' ', cMsg)
                  bFatalError = .true.

                endif

              endif VersionWordCount


            else
C.............Unknown entry in file.
              write (cMsg,'(A,A)') ' Unknown Entry:',
     &                cLnWords(1)(1:min(100,lnblnk(cLnWords(1))))

              call dwhr_error_handler( bAutoOK, ' ', cMsg)

              bFatalError = .true.

            endif ParseLine

          endif LineNotEmpty

        endif FileReadError

      enddo ParseLines

C----------------------------------------------------------------------
C     Now loop through each system, and validate inputs
C----------------------------------------------------------------------


      SystemTokenLoop: do iSystem = 1, iDwhrSystemCount

C.......Zero matching token flags for this system
        ZeroTokens: do iToken = 1, iMaxTokens
          bTokenMatched(iSystem,iToken) = .false.
        enddo ZeroTokens

C----------------------------------------------------------------------
C       Search token list for known tokens
C----------------------------------------------------------------------

C.......Extract name token
        iToken = iMatchTokens('name', iSystem, bFound )

        FoundName: if ( bFound ) then

          cDwhrSystemName(iSystem) = cValues(iSystem,iToken)(1:72)
          bTokenMatched(iSystem,iToken) = .true.
          bValidName(iSystem) = .true.

        endif FoundName

C.......Extract rated efficiency (%) token
        iToken = iMatchTokens('rated_efficiency_percent',
     &                        iSystem, bFound )
        FoundRating: if ( bFound ) then

          bTokenMatched(iSystem,iToken) = .true.

          fDwhrRatedEfficiency(iSystem) = fCtoR_err(
     &         cValues(iSystem,iToken)(1:72),
     &         'DWHR Rated efficiency',
     &         bError )

          if (bError) then
            write(cMsg, '(A,A,I2)')
     &      ' Could not convert rated_efficiency_percent value in DWHR',
     &      ' system ', iSystem

            call dwhr_error_handler(bAutoOK,' ', cMsg)
            bFatalError=.true.
          else
            bValidRating(iSystem) = .true.
          endif

        endif FoundRating


C.......Extract model coefficents, if specified.
C.......Coefficient a0
        iToken = iMatchTokens('model_coefficient_a0',iSystem, bFound)

        FoundA0: if ( bFound ) then

          bTokenMatched(iSystem,iToken) = .true.

          fDwhrModelCoeff_a0(iSystem) = fCtoR_err(
     &         cValues(iSystem,iToken)(1:72),
     &         'DWHR specified model coefficient a0',
     &         bError )

          if (bError) then
            write(cMsg, '(A,A,I2)')
     &      ' Could not convert model_coefficient_a0 value in DWHR',
     &      ' system ', iSystem

            call dwhr_error_handler(bAutoOK,' ', cMsg)
            bFatalError=.true.
          else

            bValidCoeffA0(iSystem) = .true.

          endif

        endif FoundA0

C.......Coefficient a1
        iToken = iMatchTokens('model_coefficient_a1',iSystem, bFound)

        FoundA1:if ( bFound ) then

          bTokenMatched(iSystem,iToken) = .true.

          fDwhrModelCoeff_a1(iSystem) = fCtoR_err(
     &         cValues(iSystem,iToken)(1:72),
     &         'DWHR specified model coefficient a1',
     &         bError )

          if (bError) then
            write(cMsg, '(A,A,I2)')
     &      ' Could not convert model_coefficient_a1 value in DWHR',
     &      ' system ', iSystem

            call dwhr_error_handler(bAutoOK,' ', cMsg)
            bFatalError = .true.
          else

            bValidCoeffA1(iSystem) = .true.

          endif

        endif FoundA1

C.......Coefficient a2
        iToken = iMatchTokens('model_coefficient_a2',iSystem, bFound)

        FoundA2:if ( bFound ) then

          bTokenMatched(iSystem,iToken) = .true.

          fDwhrModelCoeff_a2(iSystem) = fCtoR_err(
     &         cValues(iSystem,iToken)(1:72),
     &         'DWHR specified model coefficient a2',
     &         bError )

          if (bError) then
            write(cMsg, '(A,A,I2)')
     &      ' Could not convert model_coefficient_a2 value in DWHR',
     &      ' system ', iSystem

            call dwhr_error_handler(bAutoOK,' ', cMsg)
            bFatalError = .true.
          else

            bValidCoeffA2(iSystem) = .true.

          endif

        endif FoundA2

C.......Coefficient a3
        iToken = iMatchTokens('model_coefficient_a3',iSystem, bFound)

        FoundA3:if ( bFound ) then

          bTokenMatched(iSystem,iToken) = .true.

          fDwhrModelCoeff_a3(iSystem) = fCtoR_err(
     &         cValues(iSystem,iToken)(1:72),
     &         'DWHR specified model coefficient a3',
     &         bError )

          if (bError) then
            write(cMsg, '(A,A,I2)')
     &      ' Could not convert model_coefficient_a3 value in DWHR',
     &      ' system ', iSystem

            call dwhr_error_handler(bAutoOK,' ', cMsg)
            bFatalError = .true.
          else

            bValidCoeffA3(iSystem) = .true.

          endif

        endif FoundA3

C.......DWHR configuration 
        iToken = iMatchTokens('configuration', iSystem, bFound ) 
        
        FoundConfig: if ( bFound ) then 
        
          bTokenMatched(iSystem, iToken) = .true. 
          
          ConfigMatch: if ( bStringsMatch( cValues(iSystem,iToken),
     &                                     "equal_flow"         ) )then

            iDwhrConfiguation(iSystem) =  iEqualFlow

          elseif ( bStringsMatch( cValues(iSystem,iToken),
     &                            "unequal_flow"           ) )then
     
            iDwhrConfiguation(iSystem) =  iUnequalFlow

          else 

C...........Unknown configuration.
            bFatalError = .true.

            write(cMsg, '(A,A,A)')
     &      ' Unknown DWHR configuration ("',
     &      cValues(iSystem,iToken) 
     &            (1:min(50,lnblnk(cValues(iSystem,iToken)))),
     &      '"). Valid values are:'
     
            call edisp(iuout, cMsg)

            write(cMsg, '(A)')
     &    '  -equal_flow'
            call edisp(iuout, cMsg)

            write(cMsg, '(A)')
     &    '  -unequal_flow'
            call edisp(iuout, cMsg)

            write(cMsg, '(A)')
     &    '  -moore_model (Advanced ground temperature estimator)'
            call edisp(iuout, cMsg)

          endif ConfigMatch
          
        endif FoundConfig
        
C.......Ground temperature calculation method
        iToken = iMatchTokens('ground_temp_calculation',iSystem, bFound)

        FoundGTemp:if ( bFound ) then

          bTokenMatched(iSystem,iToken) = .true.
          bValidGroundTempFlag(iSystem) = .true.

C.........Match value against known keywords.
          GndTempMatch: if ( bStringsMatch( cValues(iSystem,iToken),
     &                                 "specified_constant"     ) )then

             iDwhrGroundTempCalc(iSystem) = iUserSpecified

          elseif ( bStringsMatch( cValues(iSystem,iToken),
     &                                   "moore_model"     ) )then
            iDwhrGroundTempCalc(iSystem) = iMooreModel

          elseif ( bStringsMatch( cValues(iSystem,iToken),
     &                                 "esp-r_profile"     ) )then
C...........Use ESP-r standard profile
            iDwhrGroundTempCalc(iSystem) = iEsprStandard


          else

C...........Unknown profile.
            bFatalError = .true.

            write(cMsg, '(A,A,A)')
     &      ' Unknown ground temperature calculation method ("',
     &      cValues(iSystem,iToken) 
     &            (1:min(50,lnblnk(cValues(iSystem,iToken)))),
     &      '"). Valid values are:'
     
            call edisp(iuout, cMsg)

            write(cMsg, '(A)')
     &    '  -specified_constant (User-specified constant temperature)'
            call edisp(iuout, cMsg)

            write(cMsg, '(A)')
     &    '  -esp-r_profile (ESP-r ground temperature calculation)'
            call edisp(iuout, cMsg)

            write(cMsg, '(A)')
     &    '  -moore_model (Advanced ground temperature estimator)'
            call edisp(iuout, cMsg)

            bValidGroundTempFlag(iSystem) = .false.

                        
          endif GndTempMatch

        endif FoundGTemp

C.......User specified ground temperature
        iToken = iMatchTokens('specified_ground_temperature_oC',
     &                        iSystem, bFound)
     
        SpecGrndTempFound: if ( bFound ) then 
          bTokenMatched(iSystem,iToken) = .true.
C.........Specified Gound temperature flag must be used with 
C.........ground_temp_calculation         
          GndTempContextOK: 
     &    if ( iDwhrGroundTempCalc(iSystem) == iUserSpecified ) then
C...........Ground temperature tag is being used correctly.     

C...........Read value 
            fGroundTempSpecC(iSystem) = 
     &       fCtoR_err( 
     &       cValues(iSystem,iToken),
     &       'Specified ground temperature',
     &       bError )
     
             if ( bError ) then
             
              call dwhr_error_handler(bAutoOK, ' ', 
     &        ' Error parsing Entry "*specified_ground_temperature_oC"')
             
             endif 
     
          else 
     
            call  dwhr_error_handler(bAutoOK, 
     &     ' Error: *specified_ground_temperature_oC must be used with', 
     &     '        "*ground_temp_calculation, specified_constant "')
             
          
          
          endif GndTempContextOK
        
        endif SpecGrndTempFound

C.......Shower supply temperature 
        iToken = iMatchTokens('shower_supply_temperature_oC',
     &                        iSystem, bFound)
     
        ShowerTempFound: if ( bFound ) then 
          bTokenMatched(iSystem,iToken) = .true.

C...........Read value 
            fShowerSupplyTempC(iSystem) = 
     &       fCtoR_err( 
     &       cValues(iSystem,iToken),
     &       'Shower supply temperature',
     &       bError )
     
             if ( bError ) then
             
              call dwhr_error_handler(bAutoOK, ' ', 
     &        ' Error parsing Entry "*shower_supply_temperature_oC"')
             
             endif 
       
        endif ShowerTempFound
        
C.......Shower supply temperature 
        iToken = iMatchTokens('drain_water_return_temperature_oC',
     &                        iSystem, bFound)
     
        DrainTempFound: if ( bFound ) then 
          bTokenMatched(iSystem,iToken) = .true.

C...........Read value 
            fDrainWaterTempC(iSystem) = 
     &       fCtoR_err( 
     &       cValues(iSystem,iToken),
     &       'Drain water return temperature',
     &       bError )
     
             if ( bError ) then
             
              call dwhr_error_handler(bAutoOK, ' ', 
     &      ' Error parsing Entry "*drain_water_return_temperature_oC"')
             
             endif 
       
        endif DrainTempFound
 
C.......Shower flow rate (lpm)
        iToken = iMatchTokens('shower_flow_rate_lpm',
     &                        iSystem, bFound)
     
        ShowerFlowFound: if ( bFound ) then 
          bTokenMatched(iSystem,iToken) = .true.

C...........Read value 
            fShowerFlowRateLPM(iSystem) = 
     &       fCtoR_err( 
     &       cValues(iSystem,iToken),
     &       'Shower flow rate',
     &       bError )
     
             if ( bError ) then
             
              call dwhr_error_handler(bAutoOK, ' ', 
     &      ' Error parsing Entry "*shower_flow_rate_lpm"')
             
             endif 
       
        endif ShowerFlowFound 
        
C-----------------------------------------------------------------------
C       We've processed all known tokens, but there may be other tokens
C       in the file that did not match above strings because of
C       user error (typo) or using an optional string out of context.
C-----------------------------------------------------------------------

        CheckTokens: do iToken = 1, iTokenCount(iSystem) 
C.........Found an unmatched token: flag token.
          TokenMatched: if ( .not. bTokenMatched(iSystem, iToken) ) then

            write (cMsg,'(A,I2,A,A,A)')
     &            ' Unknown or out-of-context entry in DWHR system ',
     &            iSystem,': "*', cTokens(iSystem,iToken)(1:60),'"'

            call dwhr_error_handler( bAutoOK, ' ', cMsg)

            bFatalError = .true.

          endif TokenMatched

        enddo CheckTokens


      enddo SystemTokenLoop




C.....Close the file, if it is open
      call erpfree(iDwhrFileNumber, iFileIOError)

      ReportStatus: if ( .not. bFatalError ) then
         write(cMsg, '(A)' )
     &    ' Drain-water HR input file processed successfully.'
         call edisp ( iuout, cMsg )

      elseif ( bFatalError ) then
        call dwhr_error_handler(
     &          bAutoOK, cContext,
     &          ' Fatal error: Could not parse DWHR file.')
         if (bAutoOK) stop

      endif ReportStatus


      STOP
      return
      end subroutine process_dwhr_inputs

C------------------ DWHR_Predict_Outlet_Temp --------------------------
C
C  ABSTRACT:
C
C    This routine imposes the drain-water HR model described by 
C    Ferguson (2010) to predict the heat-recovery and supply 
C    temperature to the hot-water tank.
C
C  Inputs
C
C     
C
C  Outputs
C
C
C----------------------------------------------------------------------      
     
      subroutine DWHR_Predict_Outlet_Temp( fMainTemperature,
     &                                     fDrainWaterTempC,
     &                                     fMakeUpWaterFlowRateLPM,
     &                                     fShowerFlowRateLPM,
     &                                    )
     
      implicit none
      
      include "drain_water_HR.h"
      
      
      
      
      return
      end subroutine DWHR_Predict_Outlet_Temp
     

     
C------------------ iMatchTokens --------------------------------------
C
C  ABSTRACT:
C
C    This routine searches a list of tokens for a given string and 
C    returns the index of the matching token
C
C  Inputs
C
C    cSearchL: String to search for 
C    iSystem:  Index of system that we should search the tokens for 
C
C  Outputs
C
C    bFound:   Flag indicating the string was found
C
C----------------------------------------------------------------------
      integer function iMatchTokens(cSearchL,iSystem,bFound)
      implicit none

      include "drain_water_HR.h"

      character*(*) cSearchL
      character*248 cSearch


      integer iToken
      integer iSystem
      logical bFound

C.....Reference
      logical bStringsMatch

      bFound = .false.

      write (cSearch, '(A)') cSearchL(1:min(248,lnblnk(cSearchL)))

      do iToken = 1, iMaxTokens

        if ( bStringsMatch( cSearch, cTokens(iSystem, iToken) ) ) then

          iMatchTokens = iToken

          bFound = .true.

        endif

      enddo

      return
      end function iMatchTokens
      






C------------------ dwhr_error_handler --------------------------------
C
C  ABSTRACT:
C
C    This is a minimal error handler for the DWRH routines
C
C  Inputs
C
C    bAutoOK: flag indicating simulator is in silent running mode
C    cContext: Contextual text message
C    cMsg:     Error specific message.
C
C  Outputs
C
C    bFatalError : flag indicating that error was encountered
C
C----------------------------------------------------------------------
      subroutine dwhr_error_handler( bAutoOK, cContextL, cMsgL)
      implicit none
      include "drain_water_HR.h"

C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C----------------------------------------------------------------------
C     Local Variables.
C----------------------------------------------------------------------

      logical bAutoOK
      integer iFileIOError
      character*(*) cContextL, cMsgL
      character*124 cContext, cMsg

      write(cContext, '(A)') cContextL(1:min(lnblnk(cContextL),124))
      write(cMsg, '(A)') cMsgL(1:min(lnblnk(cMsgL),124))

C.....If running silently, stop simulator. Otherwise, warn user.
      if ( bAutoOK ) then

        if (lnblnk(cContext)>0) call edisp ( iuout, cContext)
        call edisp ( iuout, cMsg )

        ! Fatal stop here? or later?

      else

        CALL UsrMsg (cContext, cMsg, 'W')

      endif ! matches 'if ( .not. bAutoOK ...'

      return
      end subroutine dwhr_error_handler

