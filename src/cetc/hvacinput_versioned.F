C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2007. Please contact Alex Ferguson
C for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

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
C================== HVACinput_Versioned.F ==============================
C
C Date:      Sep 13, 2007
C Author:    Alex Ferguson
C Copyright: Natural Resources Canada, 2007
C
C These routines re-implement functionality in hvacinput.F to
C provide a vastly more readable, parse-able and stream-able
C format for the .hvac file. hvacinput.F will be retained
C for the time being to support legacy .hvac files
C (that is, pre-2.0 versions)
C
C ROUTINES:
C
C   HVACINPUT_improved: Collects and validates model inputs.
C
C=======================================================================

C======================= HVACINPUT_improved  ===========================
C
C SYNOPSYS: Collects and saves inputs used the HVAC model
C
C
C INPUTS:
C
C   - iUnit: File unit number
C   - cFileName: File name
C
C OUTPUTS:
C
C=======================================================================

      SUBROUTINE HVACINPUT_improved ( iUnit, cFileName  )
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"
C-----------------------------------------------------------------------
C     Passed arguements
C-----------------------------------------------------------------------
      integer iUnit            ! <- file unit number.
      character*(*) cFileName  ! <- buffer for file name

C-----------------------------------------------------------------------
C     Simulator parameters
C-----------------------------------------------------------------------
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical spfileok,perok,tstepok,saveok,autook,exitok,startupok
      ! (autook is true in silent-running mode)
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      character*248 cLnBuffer     ! Line buffer
      character*248 cLnWords(124) ! words on the line
      integer iWordCount          ! # of words in a string

      character*248 cMsg          ! Character buffer

      logical bConvErr            ! Error flags
      integer iFileErr
      integer iDummy

      integer iSystem             ! Counters
      integer iZone               !
      integer iDaytype            !
      integer iPeriod             !
      integer iData               !
      integer iBackupSystem       !
      integer iHour, iDay         !
      integer iHourInYear         !
      integer iParam              ! 

      integer iFileLine           ! Line number in file

C.....Parsing flags
      logical bVersionOK
      logical bSystemBlockOpen
      logical bZoneBlockOpen
      logical bDataBlockOpen
      logical bMiscBlockOpen
      logical bOASchedBlockOpen
      logical bDayTypeBlockOpen
      logical bSiteAltitudeOK
      logical bFound

      logical bSysPriorityOK(max_sys)
      logical bSysZonesOK(max_sys)
      logical bSysOAPeriodsOK(max_sys)
      logical bSysDataOK(max_sys)
      logical bSysTypeOK(max_sys)
      logical bSysFuelOK(max_sys)
      logical bSysNameOK(max_sys)
      logical bSysFuncOK(max_sys)
      logical bSysBackOK(max_sys)

C.....Variables needed to parse outdoor air schedule
      integer iNum_OA_daytypes(max_sys)
      integer iNum_OA_periods(max_sys,10)
      integer iOA_DayType_EndHour(max_sys,10)
      integer iOA_Period_EndHour(max_sys,10,24)
      real fOA_Period_FlowRate(max_sys,10,24)

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer iEGetArrW
      real fCtoR_err             ! Convert a char to a real
      integer iCtoI_err          ! Convert a char to an int
      integer lnblnk

C-----------------------------------------------------------------------
C     Initialize parsing flags
C-----------------------------------------------------------------------
      iFileErr          = 0
      bParseError       = .false.
      num_hvac_sys      = 0
      bVersionOK        = .false.
      bSiteAltitudeOK   = .false.
      bSystemBlockOpen  = .false.
      bZoneBlockOpen    = .false.
      bDataBlockOpen    = .false.
      bOASchedBlockOpen = .false.
      bMiscBlockOpen    = .false. 

      do iSystem = 1, max_sys
        bMiscTokenValsDefined(iSystem) = .false.
        bSysPriorityOK(iSystem)  = .false.
        bSysZonesOK(iSystem)     = .false.
        bSysDataOK(iSystem)      = .false.
        bSysTypeOK(iSystem)      = .false.
        bSysFuelOK(iSystem)      = .false.
        bSysNameOK(iSystem)      = .false.
        bSysFuncOK(iSystem)      = .false.
        bSysBackOK(iSystem)      = .false.
        bSysOAPeriodsOK(iSystem) = .false.
        bFan_AutoSized(iSystem) = .false.
        bFan_Rated_AutoSized(iSystem) = .false.
        bFan_Auto_AutoSized(iSystem) = .false.

C.......Initialize OA daytype and period counters to zero.
        iNum_OA_daytypes(iSystem) = 0
        do iDaytype = 1, 10
          iNum_OA_periods(iSystem,iDaytype) = 0
        enddo
        
        iDataCount(iSystem) = 0

        
      enddo

C.....Collect HVAC file name
      write ( cMsg, '(A,A,A)') ' Parsing HVAC file ("',
     &     cFileName(1:min(lnblnk(cFileName),72)),'").'

C.....Inform user that we're about to parse the HVAC file.
      call edisp248(iUout, cMsg, 80)

C-----------------------------------------------------------------------
C     Loop through file and read each line
C-----------------------------------------------------------------------
      file_read: do while ( iFileErr == 0 )

C-------------------------------------------------------------------
C       Get current line
C         -> stripc: stips comments from line. Synopsys:
C            stripc ( file #     (input),
C                     line data (output),
C                     # items expected (input, 0 to disable checking),
C                     # of items returned (output)
C                     Error handling (input, 0 for silent)
C                     Contextual message (input)
C                     error status (output)
C                     line number (output)
C                    )
C-------------------------------------------------------------------
        call LStripc_LN (iUnit, cLnBuffer, 0, iDummy, 0,
     &            'HVAC file',
     &             iFileErr, iFileLine)

        ! Debugging statement
        ! print*, ">", iFileLine, iFileErr, cLnBuffer(1:lnblnk(cLnBuffer))

C-------------------------------------------------------------------
C       Get space/comma/tab separated words from line
C         -> iEGetArrW: returns the number of words encountered
C            and a character array containing the words. Synopsys:
C            # of words = ( string, array of words)
C
C-------------------------------------------------------------------
        iWordCount = iEGetArrW ( cLnBuffer, cLnWords )

C.......And make sure current line was parsed correctly before
C.......proceeding...
        read_ok: if ( iFileErr == 0 .or.
     &              ( iFileErr == 2 .and. iWordCount .gt. 0 ) ) then

C-----------------------------------------------------------------------
C         Now interpret first word on line:
C         Parse *version keyword, and associated version
C-----------------------------------------------------------------------
          parse_line: if ( cLnWords(1)(1:8) == '*version' ) then

C...........Check that one value is provided.
            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*version tag (should be "*version <value>").')

            else
C.............Attempt to convert data.
              iHVACFileVersion = iCtoI_err (
     &              cLnWords(2),
     &              'HVAC file version number',
     &              bConvErr )

              if ( bConvErr ) then
                call HVACinput_RepErr( iFileLine,
     &            'Could not interpret version number.'
     &            //' Version tag should be "*version <value>".' )

              elseif ( iHVACFileVersion < 2  ) then
                call HVACinput_RepErr( iFileLine,
     &              'Tag "*idealized-hvac" must be used with'
     &              //' hvac file format version 2 or greater.' )

              else
                bVersionOK = .true.

              endif  ! <- matches ' if ( bConvErr ) ... '

            endif    ! <- matches ' if ( iWordCount /= 2 ) ... '

C-----------------------------------------------------------------------
C         Parse *site-altitude keyword, and associated altitude
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:15) == '*site-altitude ' ) then

C...........Check that one value is provided.
            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*site-altitude tag '
     &           //'(should be "*site-altitude <value>").')

            else

              site_altitude = fCtoR_err (
     &             cLnWords(2),
     &             'HVAC file site altitude',
     &             bConvErr )

              if ( bConvErr ) then
                call HVACinput_RepErr( iFileLine,
     &            'Could not interpret altitude.'
     &            //' Altitude tag should be '
     &            //'"*site-altitude <value>".' )

              else

                bSiteAltitudeOK = .true.

              endif

            endif

C-----------------------------------------------------------------------
C         Parse *system-start keyword, and open system block
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:14) == '*system-start ' ) then

            if ( bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'System block already open!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*system-start tag are unsupported. ')

            else

C.............Increment number of systems, and open block
              num_hvac_sys = num_hvac_sys + 1
              iSystem = num_hvac_sys
              bSystemBlockOpen = .true.

            endif


C-----------------------------------------------------------------------
C         Parse *backup keyword, and collect associated backup (system
C         block must also be open!)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:8) == '*backup ' ) then

            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*backup tag '
     &           //'(should be "*backup <name>").')

            elseif ( bSysBackOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &          '"*backup" has already been defined for this system')

              bSysBackOK(iSystem) = .false.

            elseif ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*backup" tag must be located within  '
     &           //'system definition block.')

            else

              write (cSys_Backup(iSystem),'(A)') cLnWords(2)(1:72)
              bSysBackOK(iSystem) = .true.

            endif 

C-----------------------------------------------------------------------
C         Parse *name keyword, and collect associated name (system
C         block must also be open!)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:6) == '*name ' ) then

            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*name tag '
     &           //'(should be "*name <name>").')

            elseif ( bSysNameOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &          '"*name" has already been defined for this system')

              bSysNameOK(iSystem) = .false.

            elseif ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*name" tag must be located within  '
     &           //'system definition block.')

            else

              write (cSys_name(iSystem),'(A)') cLnWords(2)(1:72)
              bSysNameOK(iSystem) = .true.

            endif

C-----------------------------------------------------------------------
C         Parse *type keyword, and collect associated type (system
C         block must also be open!)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:6) == '*type ' ) then

            type_keyword: if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*type tag '
     &           //'(should be "*type <keyword>").')

            elseif ( bSysTypeOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &          '"*type" has already been defined for this system')

              bSysTypeOK(iSystem) = .false.

            elseif ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*type" tag must be located within  '
     &           //'system definition block.')

            else

C.............System type must be one of the following.
C                1 - furnace
C                2 - boilder
C                3 - baseboard/hydronics/plenum heater
C                4 - Integrated mechanical system
C                7 - Air-source heat pump
C                8 - Ground-source heat pump
C                9 - Ground-source heat pump
C                    (Ecole Polytechnique ground coupling)
C

C.............Default type flag to ok.

              bSysTypeOK(iSystem) = .true.

C.............Collect HVAC type

              type_parse: if ( cLnWords(2)(1:8) == 'furnace ' ) then
                ihvac_type(iSystem) = iSysFurnace

              elseif ( cLnWords(2)(1:7) == 'boiler ' ) then
                ihvac_type(iSystem) = iSysBoiler

              elseif ( cLnWords(2)(1:10) == 'baseboard ' ) then
                ihvac_type(iSystem) = iSysBaseboard

              elseif ( cLnWords(2)(1:4) == 'IMS ' ) then
                ihvac_type(iSystem) = iSysIMS_P10

              elseif ( cLnWords(2)(1:5) == 'ASHP ' ) then
                ihvac_type(iSystem) = iSysASHP

              elseif ( cLnWords(2)(1:5) == 'GSHP ' ) then
                ihvac_type(iSystem) = iSysGSHP

              elseif ( cLnWords(2)(1:9) == 'GSHP-GCEP ' ) then
                ihvac_type(iSystem) = iSysGSHP_GCEP

C.............Unknown type
               else

                bSysTypeOK(iSystem) = .false.

                write (cMsg, '(A,A,A,I2,A)')
     &           ' Error parsing system type ("',
     &            cLnWords(2)(1:lnblnk(cLnWords(2))),
     &           '") for HVAC system #', iSystem,
     &           '. Value must be one of the following:'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "furnace"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "boiler"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "baseboard" (including hydronics & plenum heaters)'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "IMS" (integrated mechanical system)'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "ASHP" (Air source heat pump)'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "GSHP" (Ground source heat pump)'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A,A)')
     &    '     - "GSHP-GCEP" (Ground-source heat pump using ',
     &    'Ecole Polytechnique advanced ground coupling model.)'
                call edisp248(iuout,cMsg,80)

              endif  type_parse ! <- matches 'if ( cLnWords(2)(1:8) == 'furnace ' )'

            endif type_keyword


C-----------------------------------------------------------------------
C         Parse *fuel tag.
C-----------------------------------------------------------------------

          elseif (cLnWords(1)(1:6) == '*fuel ' ) then

            fuel_keyword: if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*fuel tag '
     &           //'(should be "*fuel <keyword>").')

            elseif ( bSysFuelOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &          '"*fuel" has already been defined for'
     &          // ' this system')

              bSysFuelOK(iSystem) = .false.

            elseif ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*fuel" tag must be located within  '
     &           //'system definition block.')

            else

C.............System Fuel must be one of the following:
C.............   "electricity"
C.............   "gas"
C.............   "oil"
C.............   "propane"
C.............   "wood"


C.............Default priority flag to OK
              bSysFuelOK(iSystem) = .true.

C.............interpret keyword.
              fuel_parse: if (cLnWords(2)(1:12) == 'electricity ') then

                ifuel_type(iSystem) = iElectricity

              elseif ( cLnWords(2)(1:4) == 'gas ') then

                ifuel_type(iSystem) = iNaturalGas

              elseif ( cLnWords(2)(1:4) == 'oil ') then

                ifuel_type(iSystem) = iOil

              elseif ( cLnWords(2)(1:8) == 'propane ') then

                ifuel_type(iSystem) = iPropane

              elseif ( cLnWords(2)(1:5) == 'wood ') then

                ifuel_type(iSystem) = iWood

              else

                bSysFuelOK(iSystem) = .false.

                write (cMsg, '(A,A,A,I2,A)')
     &          ' Error parsing system fuel. ("',
     &          cLnWords(2)(1:lnblnk(cLnWords(2))),
     &          '") for HVAC system #', iSystem,
     &          '. Value must be one of the following:'
                  call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "electricity"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "gas"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "oil"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "propane"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "wood"'
                call edisp248(iuout,cMsg,80)

              endif fuel_parse !<- matches if ( cLnWords(2)(1:12) == 'elec'

            endif fuel_keyword

C-----------------------------------------------------------------------
C         Parse *function keyword, and collect associated function (system
C         block must also be open!)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:10) == '*function ' ) then

            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*function tag '
     &           //'(should be "*function <keyword>").')

            elseif ( bSysFuncOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &          '"*function" has already been defined for this system')

              bSysFuncOK(iSystem) = .false.

            elseif ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*function" tag must be located within  '
     &           //'system definition block.')

            else

C.............System function must be one of the following.
C                1 - heating
C                2 - cooling
C

C.............Default type flag to ok.

              bSysFuncOK(iSystem) = .true.

C.............Collect system function 

              if ( cLnWords(2)(1:8) == 'heating ' ) then
                iSys_Function(iSystem) = iFuncHeating

              elseif ( cLnWords(2)(1:8) == 'cooling ' ) then
                iSys_Function(iSystem) = iFuncCooling

C.............Unknown function
               else

                bSysFuncOK(iSystem) = .false.

                write (cMsg, '(A,A,A,I2,A)')
     &           ' Error parsing system function ("',
     &            cLnWords(2)(1:lnblnk(cLnWords(2))),
     &           '") for HVAC system #', iSystem,
     &           '. Value must be one of the following:'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "heating"'
                call edisp248(iuout,cMsg,80)

                write (cMsg, '(A)')
     &    '     - "cooling"'
                call edisp248(iuout,cMsg,80)

                call HVACinput_RepErr( iFileLine,
     &            '"*function" value could not be interpreted.')

              endif ! <- matches 'if ( cLnWords(2)(1:8) == 'furnace ' )'

            endif

C-----------------------------------------------------------------------
C         Parse *priority keyword, and collect associated priority
C         (system block must also be open!)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:10) == '*priority ' ) then

            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*priority tag '
     &           //'(should be "*priority <keyword>").')

            elseif ( bSysPriorityOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &          '"*priority" has already been defined for'
     &          // ' this system')

            elseif ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*priority" tag must be located within  '
     &           //'system definition block.')

            else

C.............System priority must be one 1 (primary), or 2 (backup).

C.............Default priority flag to OK
              bSysPriorityOK(iSystem) = .true.

C.............interpret keyword.
              if ( cLnWords(2)(1:8) == 'primary ' ) then

                ipriority(iSystem) = iHVACPrimarySys

              elseif ( cLnWords(2)(1:7) == 'backup ') then

                ipriority(iSystem) = iHVACBackupSys

              else

                write (cMsg, '(A,A,A,I2,A)')
     &          ' Error parsing system priority ("',
     &          cLnWords(2)(1:lnblnk(cLnWords(2))),
     &          '") for HVAC system #', iSystem,
     &          '. Value must be "primary" or "backup".'
                  call edisp248(iuout,cMsg,80)

                call HVACinput_RepErr(iFileLine,
     &               'Could not interpret system priority')

C...............Set error flag
                bParseError = .true.

              endif

            endif


C-----------------------------------------------------------------------
C         Parse *zone-links-start keyword, and open zone links block
C         (system block must also be open)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:18) == '*zone-links-start ' ) then

            if ( bZoneBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'Zone block already open!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*zone-links-start tag are unsupported. ')

            elseif ( bSysZonesOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &            'Zone links have already been defined for this'
     &            //' system.')


            else

              bZoneBlockOpen = .true.

C.............Defaut zone validation tag to true - set to false
C.............on parsing failure.
              bSysZonesOK(iSystem) = .true.

            endif


C-----------------------------------------------------------------------
C         Parse *zone-link keyword, and collect associated zone index
C         and fraction. (Both system and zone-link blocks must be
C         open!)
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:11) == '*zone-link '  ) then

            if ( iWordCount /= 3 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*zone-link tag '
     &           //'(should be "*zone-link <value> <value>").')

            elseif ( .not. bZoneBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*zone-link" tag must be located within  '
     &           //'zone-links definition block.')

            else

C.............Increment zone counter
              num_sys_zones (iSystem) = num_sys_zones (iSystem) + 1
              iZone = num_sys_zones (iSystem)

C.............Get zone index
              isys_zone_num(iSystem, iZone) =
     &            iCtoI_err(cLnWords(2),'Zone index',bConvErr)

              if ( bConvErr ) then
                call HVACinput_RepErr ( iFileLine,
     &            'Could not interpret zone index provided with '
     &            //'"*zone-link" tag.' )
              endif

C.............Get zone fraction
              sys_zone_cap_frac(iSystem,iZone) =
     &            fCtoR_err(cLnWords(3), 'System capacity fraction',
     &            bConvErr)
              if ( bConvErr ) then
                call HVACinput_RepErr ( iFileLine,
     &            'Could not interpret system capcacity fraction '
     &            //' provided with "*zone-link" tag.' )
              endif

            endif    !<- matches "if ( iWordCount /= 3 ... "


C-----------------------------------------------------------------------
C         Parse *zone-links-end keyword, and close zone block if open
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:16) == '*zone-links-end ' ) then

            if ( .not. bZoneBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'There`s no open zone-links block to close!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*zone-links-start tag are unsupported. ')

            else

              bZoneBlockOpen = .false.

            endif

C-----------------------------------------------------------------------
C         Parse *misc-param-start keyword, and open miscellanous
C         parameters block.
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:18) == '*misc-param-start ' ) then

            if ( bMiscBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           'A miscellanous parameter block is already open!')

            elseif ( iWordCount /= 1 ) then
              
              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*misc-param-start tag are unsupported. ')

            else
              bMiscBlockOpen = .true.

            endif 

C-----------------------------------------------------------------------
C         Parse *data-end keyword, and close data block if open
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:16) == '*misc-param-end ' ) then

            if ( .not. bMiscBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'There`s no open miscellaneous parameter'
     &             // ' block to close!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*misc-param-end- tag are unsupported. ')

            else

              bMiscBlockOpen = .false.

            endif



C-----------------------------------------------------------------------
C         Parse *data-start keyword, and open data block
C         (system block must also be open)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:16) == '*sys-data-start ' ) then

            if ( bZoneBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'A data block is already open!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*sys-data-start tag are unsupported. ')

            elseif ( bSysDataOK(iSystem) ) then

              call HVACinput_RepErr( iFileLine,
     &            'Data have already been defined for this'
     &            //' system.')

              bSysDataOK(iSystem) = .false.

            else

              bDataBlockOpen = .true.

C.............Defaut data validation tag to true - set to false
C.............on parsing failure.
              bSysDataOK(iSystem) = .true.
            
            endif

C-----------------------------------------------------------------------
C         Parse *data-end keyword, and close data block if open
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:14) == '*sys-data-end ' ) then

            if ( .not. bDataBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'There`s no open data block to close!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*data-start tag are unsupported. ')

            else

              bDataBlockOpen = .false.

            endif



C-----------------------------------------------------------------------
C         Parse *OA-schedule-start keyword, and open system block
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:19) == '*OA-schedule-start ' ) then

            if ( bOASchedBlockOpen) then

              call HVACinput_RepErr( iFileLine,
     &            'OA schedule block already open!' )

            elseif ( .not. bSystemBlockOpen ) then
            
              call HVACinput_RepErr( iFileLine,
     &            'OA schedule block must be embedded'
     &             //' inside system block' )
     
            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*OA-schedule-start tag are unsupported. ')

            else

C.............open block
              bOASchedBlockOpen = .true.

C.............Set OA schedule specification flag
              bSysOAPeriodsOK(iSystem) = .true.

            endif

C-----------------------------------------------------------------------
C         Parse *day-type-start keyword, and open day-type block
C         (OA-schedule block must also be open)
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:16) == '*day-type-start ' ) then

            if ( bDayTypeBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'Day-type block already open!' )

            elseif ( .not. bOASchedBlockOpen ) then

              call HVACinput_RepErr ( iFileLine,
     &             'Day-type block must be embedded inside '
     &             //'OA-schedule block!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*day-type-start tag are unsupported. ')

            else

              bDayTypeBlockOpen = .true.

C.............Increment total number of OA day types
              iNum_OA_daytypes(iSystem) = iNum_OA_daytypes(iSystem) + 1 
              iDaytype = iNum_OA_daytypes(iSystem)

            endif


C-----------------------------------------------------------------------
C         Parse *end-hour keyword, and collect associated OA schedule
C         data. (Daytype block must be open 
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:10) == '*end-hour '  ) then

            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*end-hour tag '
     &           //'(should be '
     &           //'"*end-hour <end-hour>").')

            elseif ( .not. bDayTypeBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*end-hour" tag must be located within  '
     &           //'OA-schedule definition block.')

            else

              iOA_DayType_EndHour(iSystem,iDaytype)
     &          = iCtoI_err(cLnWords(2),'Day-type end hour', bConvErr)

              if ( bConvErr ) then

                call HVACinput_RepErr( iFileLine,
     &               'Could not interpret day-type end-hour.' )
  
              endif 

              iNum_OA_periods(iSystem,iDaytype) =
     &           iNum_OA_periods(iSystem,iDaytype)

            endif    !<- matches "if ( iWordCount /= 3 ... "

C-----------------------------------------------------------------------
C         Parse *period keyword, and collect associated OA schedule
C         data. (OA block must be open!)
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:8) == '*period '  ) then

            if ( iWordCount /= 3 ) then

              call HVACinput_RepErr( iFileLine,
     &           'Incorrect number of items following '
     &           //'*period tag '
     &           //'(should be '
     &           //'"*period <end-hour> <flow-rate>").')

            elseif ( .not. bDayTypeBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &           '"*period" tag must be located within  '
     &           //'OA-schedule definition block.')

            else


C.............Increment period counter
              iNum_OA_periods(iSystem,iDaytype) =
     &           iNum_OA_periods(iSystem,iDaytype) + 1
              
              iPeriod = iNum_OA_periods(iSystem,iDaytype)

C.............Get end hour (0-24)
              iOA_Period_EndHour(iSystem, iDaytype, iPeriod) =
     &           iCtoI_err(cLnWords(2),'period end hour',bConvErr)

              if ( bConvErr ) then

                call HVACinput_RepErr(iFileLine,
     &            'Could not interpret period end hour (item #1'
     &            //' following "*period" tag')

              endif

              fOA_Period_FlowRate(iSystem, iDaytype, iPeriod) =
     &          fCtoR_err(cLnWords(3), 'period flow rate', bConvErr)

              if ( bConvErr ) then

                call HVACinput_RepErr(iFileLine,
     &            'Could not interpret period OA flow rate (item #2'
     &            //' following "*period" tag')

              endif 

            endif    !<- matches "if ( iWordCount /= 3 ... "


C-----------------------------------------------------------------------
C         Parse *day-type-end keyword, and close zone block if open
C-----------------------------------------------------------------------
          elseif ( cLnWords(1)(1:14) == '*day-type-end ' ) then

            if ( .not. bDayTypeBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'There`s no open day-type block to close!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*day-type-end tag are unsupported. ')

            else

              bDayTypeBlockOpen = .false.

            endif

C-----------------------------------------------------------------------
C         Parse *OA-schedule-end keyword, and close system block if open
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:17) == '*OA-schedule-end ' ) then

            if ( .not. bOASchedBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'There`s no open OA-schedule block to close!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*OA-schedule-end tag are unsupported. ')

            else

C.............Close block
              bOASchedBlockOpen = .false.

            endif

C-----------------------------------------------------------------------
C         Parse *system-end keyword, and close system block if open
C-----------------------------------------------------------------------
          elseif (cLnWords(1)(1:12) == '*system-end ' ) then

            if ( .not. bSystemBlockOpen ) then

              call HVACinput_RepErr( iFileLine,
     &            'There`s no open system block to close!' )

            elseif ( iWordCount /= 1 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Additional data items following '
     &          //'*system-end tag are unsupported. ')

            else

C.............Close block
              bSystemBlockOpen = .false.

            endif

C-----------------------------------------------------------------------
C         Allow empty lines in the file
C-----------------------------------------------------------------------
          elseif ( iWordCount == 0 ) then

            ! Do nothing.

C-----------------------------------------------------------------------
C         Parse block data (that is, data sandwiched between
C         *data-start and *data-end tags)
C-----------------------------------------------------------------------
          elseif ( bDataBlockOpen ) then

C...........Line must have 2 words: Token Value
            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &          'System data must be in token-value format '
     &          // '("<token> <value>")' )

            else

C.............Increment system data counter
              iDataCount(iSystem) = iDataCount(iSystem) + 1
              iData = iDataCount(iSystem)

C.............Attempt to read token name
              write(cTokens(iSystem,iData), '(A)')
     &           cLnWords(1)(1:min(72,lnblnk(cLnWords(1))))

C.............Attempt to read token value
              fTokenVals(iSystem,iData) = fCtoR_err(
     &               cLnWords(2),
     &               'interpreting token "'
     &                  //cLnWords(2)(1:lnblnk(cLnWords(2)))//'"',
     &               bConvErr)

              if ( bConvErr ) then

                call HVACinput_RepErr( iFileLine,
     &                'Could not interpret value for token "'
     &              //cLnWords(1)(1:min(72,lnblnk(cLnWords(1)))) //'"')

              endif

            endif

C-----------------------------------------------------------------------
C         Parse misc parameter block (that is, data sandwiched between
C         *misc-param-start and *data-param-end tags)
C-----------------------------------------------------------------------
          elseif ( bMiscBlockOpen ) then 

C...........Line must have 2 words: Token Value
            if ( iWordCount /= 2 ) then

              call HVACinput_RepErr( iFileLine,
     &          'Miscellaneous parameters must be in token-value '
     &          // 'format ("<token> <value>")' )

            elseif ( iMiscParamCount(iSystem) == iMaxTokenCount ) then

              write (cMsg, '(A,I2,A,I2,A)')
     &            ' The maximum number of miscellaneous tokens (',
     &            iMaxTokenCount,
     &            ') have already been defined for system number ',
     &            iSystem,
     &            '.'
              call edisp248(iUout,cMsg,80)
              call HVACinput_RepErr( iFileLine,
     &           "Too many miscellaneous tokens already defined." )
              
            else

C.............Increment system data counter
              iMiscParamCount(iSystem) = iMiscParamCount(iSystem) + 1
              iParam = iMiscParamCount(iSystem)

C.............Attempt to read token name
              write(cMiscTokens(iSystem,iParam), '(A)')
     &           cLnWords(1)(1:min(72,lnblnk(cLnWords(1))))

C.............Attempt to read token value
              write(cMiscVals(iSystem,iParam), '(A)')
     &           cLnWords(2)(1:min(72,lnblnk(cLnWords(2))))

            endif
C-----------------------------------------------------------------------
C         Default case: if line does not start with a known keyword and
C         block data is not open, it must be an erroneous record.
C-----------------------------------------------------------------------
          else
            write (cMsg, '(A,A,A)') ' Unknown keyword ("',
     &        cLnWords(1)(1:min(lnblnk(cLnWords(1)),80)),'")'
            call edisp248(iUout,cMsg,80)
            call HVACinput_RepErr( iFileLine,
     &           "Could not interpret line." )

          endif parse_line !<- matches "if ( cLnWords(1)(1:X) == ... "

        endif read_ok      !<- matches "if ( iFileErr == 0 )"

      enddo file_read      !<- matches "do while ( iFileErr == 0 )..."
C-----------------------------------------------------------------------
C     Now loop through systems and validate data.
C-----------------------------------------------------------------------
      system_loop: do iSystem = 1, num_hvac_sys

         ! Debugging statements

!          print*, "System: ", iSystem
!          do iData = 1, iDataCount(iSystem)
!             print*, "TV: ",
!      &      cTokens(iSystem, iData)(1:lnblnk(cTokens(iSystem,iData))),
!      &      " --- ", fTokenVals(iSystem,iData)

!          enddo

C-----------------------------------------------------------------------
C       Parse and validate data and miscellaneous parameter blocks.
C-----------------------------------------------------------------------
        parse_data_by_HVAC_type: select case ( ihvac_type (iSystem) )

          case ( iSysFurnace )

            call HVAC_FuranaceBoiler_Input(iSystem,iSysFurnace)

          case ( iSysBoiler )

            call HVAC_FuranaceBoiler_Input(iSystem,iSysBoiler)

          case ( iSysBaseboard )

            call HVAC_Baseboard_Input(iSystem)

          case ( iSysIMS_P10 )

            call HVAC_IMS_P10_Input(iSystem)

          case ( iSysASHP )

            call HVAC_HP_Input(iSystem,iSysASHP)

          case ( iSysGSHP )

            call HVAC_HP_Input(iSystem,iSysGSHP)

          case ( iSysGSHP_GCEP )

            call HVAC_HP_Input(iSystem,iSysGSHP_GCEP)

          case default

            ! Can't happen --- iHVAC_type trapped above.

        end select parse_data_by_HVAC_type

C-----------------------------------------------------------------------
C       Determine backup system, if any
C-----------------------------------------------------------------------
        backup_system_speced: if ( bSysBackOK(iSystem) ) then

          bFound = .false.

          backup_system_search_loop: do iBackupSystem = 1, num_hvac_sys

            backup_match_name:
     &      if( cSys_Backup(iSystem) == cSys_name(iBackupSystem) ) then

C..............Record backup system #
              ibackup_sys_num(iSystem) = iBackupSystem

C.............Ensure backup system is valid (that is,
C.............one of furnace/baseboards/boiler)

              bFound = .true.

              backup_type: select case ( ihvac_type( iBackupSystem ) )

              case (iSysFurnace, iSysBoiler, iSysBaseboard )

                iBackup_Heat(iSystem) = ihvac_type( iBackupSystem )

              case default 

                 write (cMsg, '(A,A,I2,A,A)') 'Backup system for HVAC',
     &             ' system # ', iSystem, ' must be furnace, boiler',
     &             ' or heat pump.'
                 call edisp248( iUout, cMsg, 80 )
                 bParseError = .true.
                 
              end select backup_type

            endif backup_match_name

          enddo backup_system_search_loop

          backup_found: if ( .not. bFound ) then

            write (cMsg, '(A,A,A,A,I2,A)') 'Backup system "',
     &             cSys_Backup(iSystem)(1:lnblnk(cSys_Backup(iSystem))),
     &             '" for HVAC',
     &             ' system # ', iSystem, ' could not be found!'
            call edisp248( iUout, cMsg, 80 )
            bParseError = .true.

          endif backup_found

C-----------------------------------------------------------------------
C         If back-up and primary fans have been auto-sized,
C         compare fan sizes and pick largest.
C-----------------------------------------------------------------------
          iBackupSystem = iBackup_sys_num(iSystem)

          if ( bFan_AutoSized(iSystem)       .and.
     &         bFan_AutoSized(iBackupSystem)
     &       ) then

            fan_power(iSystem) = max ( fan_power(iSystem),
     &                                 fan_power(iBackupSystem) )
            fan_power(iBackupSystem) = fan_power(iSystem)

          endif

          if ( bFan_Rated_AutoSized(iSystem)      .and.
     &         bFan_Rated_AutoSized(iBackupSystem)
     &       ) then

            fan_power_r(iSystem) = max ( fan_power_r(iSystem),
     &                                   fan_power_r(iBackupSystem) )
            fan_power_r(iBackupSystem) = fan_power_r(iSystem)

          endif

          if ( bFan_Auto_AutoSized(iSystem)      .and.
     &         bFan_Auto_AutoSized(iBackupSystem)
     &       ) then

            fan_power_auto(iSystem) 
     &                = max ( fan_power_auto(iSystem),
     &                        fan_power_auto(iBackupSystem) )
     &
            fan_power_auto(iBackupSystem) = fan_power_auto(iSystem)

          endif

          
     

        endif backup_system_speced

C-----------------------------------------------------------------------
C       Parse outdoor air schedule (if any)
C-----------------------------------------------------------------------
        OA_schedule_speced: if ( bSysOAPeriodsOK(iSystem) ) then
          iDayType = 1
C-----------------------------------------------------------------------
C         Loop through each day in a year
C-----------------------------------------------------------------------
          DayLoop: do iDay = 1, 365
            iPeriod  = 1
C-----------------------------------------------------------------------
C           Loop though each hour in a day
C-----------------------------------------------------------------------
            HourLoop: do iHour = 1, 24

C-----------------------------------------------------------------------
C             Compute hour in year
C-----------------------------------------------------------------------
              iHourInYear = ( iDay - 1 ) * 24 + iHour 

C-----------------------------------------------------------------------
C             Is current period valid?
C-----------------------------------------------------------------------
              Period_Valid:
     &        if ( iHour
     &            > iOA_Period_EndHour(iSystem, iDaytype, iPeriod)
     &            )then
C...............No. Increment period
                iPeriod = iPeriod + 1

C...............Does incrementing the period exceed the total number 
C...............of specified periods? If so, reset counter
                if ( iPeriod > iNum_OA_periods(iSystem,iDaytype) )
     &            iPeriod = 1
      
              endif Period_Valid

C-----------------------------------------------------------------------
C             Is current daytype valid?
C-----------------------------------------------------------------------

              DayType_Valid:
     &        if ( iHourInYear
     &               > iOA_DayType_EndHour(iSystem,iDaytype) )  then

C...............No. Move to next daytype
                iDaytype = iDaytype + 1
                iPeriod  = 1

C...............But check that we haven't exhausted all possible
C...............daytypes, and reset counter if so.
                if ( iDaytype > iNum_OA_daytypes(iSystem) )
     &            iDaytype = 1

              endif DayType_Valid

C-----------------------------------------------------------------------
C             Now assign flow-rate from OA schedule to hvac common
C             for the current hour
C-----------------------------------------------------------------------
              outdoor_air_flow(iSystem, iHourInYear) =
     &            fOA_Period_FlowRate( iSystem, iDaytype, iPeriod )

            enddo HourLoop

          enddo DayLoop

        endif OA_schedule_speced
C-----------------------------------------------------------------------
C     Validate furnace/fuel combination
C-----------------------------------------------------------------------
        validate_fuel: select case ( ihvac_type (iSystem) )

          case ( iSysFurnace )

          case ( iSysBoiler )

          case ( iSysIMS_P10 )

          case default

        end select validate_fuel

      enddo system_loop
C-----------------------------------------------------------------------
C     Error trapping: Were errors encountered?
C-----------------------------------------------------------------------

      if ( bParseError ) then
C........Yes. Warn user, and possibly stop simulation.
         call edisp(iUout, ' HVAC file was not processed'
     &         // ' successfully.')
         if ( AutoOK ) then

            stop 'Ideal HVAC models: Fatal Error!'

         endif

      else
C........No. Notify user.
         call edisp(iUout, ' HVAC file was processed'
     &         // ' successfully.')
      endif

      RETURN
      END

C---------------------- HVACinput_RepErr -------------------------------
C     Report an error while parsing the HVAC file
C
C     INPUTS:
C
C        - iLineNumber : Line number
C        - cMsg:         Message buffer.
C
C     OUTPUTS:
C
C        - None,.
C-----------------------------------------------------------------------
      subroutine HVACinput_RepErr(iLineNumber, cMsg)
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"

C-----------------------------------------------------------------------
C     Passed arguements
C-----------------------------------------------------------------------
      integer iLineNumber
      character*(*) cMsg
C-----------------------------------------------------------------------
C     Input/Output channel
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C-----------------------------------------------------------------------
C     Local Variables
C-----------------------------------------------------------------------
      character*248 cBuffer
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer lnblnk

C-----------------------------------------------------------------------
C     Report error
C-----------------------------------------------------------------------

C.....Report line number:
      write (cBuffer, '(A,I3,A)')
     & ' ERROR: HVAC file line #,',iLineNumber,':'
      call edisp (iUout, cBuffer)

C.....Report error message
      write (cBuffer, '(A,A)') '  - ', cMsg(1:min(lnblnk(cMsg),244))

      call edisp248 (iUout, cBuffer, 80)

C.....Set parsing error flag
      bParseError = .true.

      return
      end

C================== HVAC_IMS_P10_Input =================================
C
C  This routine parses the data block for a P10-compliant IMS system
C
C  Inputs:
C    - iSystem: index of current IMS system
C=======================================================================
      subroutine HVAC_IMS_P10_Input( iSystem )
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "IMS_common.h"
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      integer iSystem     ! System index
C-----------------------------------------------------------------------
C     Input/Output channel
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      character*248 cMsg  ! Message buffer
      logical bFound      ! Result of string search
      logical bInLimits   ! Flag indicating if value is within limits

C.....Named constants
      logical bRequired, bNotRequired
      parameter (bRequired = .true., bNotRequired = .false.)
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fHVAC_FindTokenNoLimits,
     &     fHVAC_FindTokenWithLimits
      character*72 cHVAC_FindCToken

C-----------------------------------------------------------------------
C     First, check that all tokens are valid
C-----------------------------------------------------------------------
      call AreTokensValid (iSystem) 

C-----------------------------------------------------------------------
C     Now read system capacity
C-----------------------------------------------------------------------
      fIMS_SpaceHCapacity(iSystem) = fHVAC_FindTokenWithLimits(
     &                  iSystem,
     &                  'SH-capacity',
     &                  'IMS unit space-heating capacity (W)',
     &                  bRequired,
     &                  0.0, 1.0e07,
     &                  bFound,
     &                  bInLimits )
C-----------------------------------------------------------------------
C     Nominal burner output
C-----------------------------------------------------------------------
      fIMS_NominalBurnerInput(iSystem)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'nominal-burner-input',
     &       'Nominal output of IMS burner (W)',
     &       bRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Space heating net efficiency
C-----------------------------------------------------------------------
      fIMS_SpaceHNetEff(iSystem,iP10_PLR_15)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'SH-net-eff-15',
     &       'IMS unit space-heating HHV net efficiency @ 15% load (-)',
     &       bRequired,
     &       0.0, 1.0,
     &       bFound,
     &       bInLimits )

      fIMS_SpaceHNetEff(iSystem,iP10_PLR_40)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'SH-net-eff-40',
     &       'IMS unit space-heating HHV net efficiency @ 40% load (-)',
     &       bRequired,
     &       0.0, 1.0,
     &       bFound,
     &       bInLimits )

      fIMS_SpaceHNetEff(iSystem,iP10_PLR_100)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'SH-net-eff-100',
     &      'IMS unit space-heating HHV net efficiency @ 100% load (-)',
     &       bRequired,
     &       0.0, 1.0,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Power consumption in space heating 
C-----------------------------------------------------------------------
      fP10_SpaceH_Power(iSystem,iP10_PLR_15)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'SH-power-15',
     &       'IMS unit space-heating power use @ 15% load (W)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_SpaceH_Power(iSystem,iP10_PLR_40)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'SH-power-40',
     &       'IMS unit space-heating power use @ 40% load (W)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_SpaceH_Power(iSystem,iP10_PLR_100)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'SH-power-100',
     &      'IMS unit space-heating power use @ 100% load (W)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_WaterH_PF(iSystem)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'WHPF',
     &      'IMS unit water heater performance factor (-)',
     &       bRequired,
     &       0.0, 1.0,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Fresh air ventilation rates 
C-----------------------------------------------------------------------
      fP10_Vent_FA_Flow(iSystem, iMode_LowVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-FA-flow-C',
     &      'IMS unit fresh air ventilation in continuious mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_Vent_FA_Flow(iSystem, iMode_HighVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-FA-flow-H',
     &      'IMS unit fresh air ventilation in high-vent mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Exhaust air ventilation rates
C-----------------------------------------------------------------------
      fP10_Vent_EA_Flow(iSystem, iMode_LowVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-EA-flow-C',
     &      'IMS unit exh. air ventilation in continuious mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_Vent_EA_Flow(iSystem, iMode_HighVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-EA-flow-H',
     &      'IMS unit exh. air ventilation in high-vent mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Fresh air fan power
C-----------------------------------------------------------------------
      fP10_Vent_FA_Power(iSystem, iMode_LowVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-FA-fan-power-C',
     &      'IMS unit fresh air fan power in continuious mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_Vent_FA_Flow(iSystem, iMode_HighVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-FA-fan-power-C',
     &      'IMS unit fresh air fan power in high-vent mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Heat source in IMS unit
C-----------------------------------------------------------------------
      iIMS_HeatSource(iSystem)
     &   = int( fHVAC_FindTokenNoLimits (
     &          iSystem,
     &          'heat-source-type',
     &          'IMS unit primary heat source (-)',
     &          bRequired,
     &          bFound )
     &        )

C.....Check that IMS heat source is supported
      CheckIMSHeatSource: if (
     &  iIMS_HeatSource(iSystem) /= iIMS_CondensingTank      .and.
     &  iIMS_HeatSource(iSystem) /= iIMS_ConventionalTank    .and.
     &  iIMS_HeatSource(iSystem) /= iIMS_InducedDraftFanTank .and. 
     &  iIMS_HeatSource(iSystem) /= iIMS_InstantaneousHeater .and.
     &  iIMS_HeatSource(iSystem) /= iIMS_TanklessHeater       ) then

        write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting IMS unit heat source ',
     &    '(tag "heat-source-type") for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )

        call edisp248( iUout, '   - 1 (condensing tank)', 80 )
        call edisp248( iUout, '   - 2 (conventional tank)', 80 )
        call edisp248( iUout, '   - 3 (induced draft fan tank)', 80 )
        call edisp248( iUout, '   - 4 (instantaneous heater)', 80 )
        call edisp248( iUout, '   - 5 (tankless water heater)', 80 )

C.......Set error flag:
        bParseError = .true. 

      endif CheckIMSHeatSource
C-----------------------------------------------------------------------
C     Exhaust air fan power
C-----------------------------------------------------------------------
      fP10_Vent_EA_Power(iSystem, iMode_LowVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-EA-fan-power-C',
     &      'IMS unit exh. fan power in continuious mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

      fP10_Vent_EA_Flow(iSystem, iMode_HighVent)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-EA-fan-power-C',
     &      'IMS unit exh. fan power in high-vent mode (l/s)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Ventilation heat recovery efficiency (SRE)
C-----------------------------------------------------------------------
      fP10_Vent_SRE(iSystem)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'vent-SRE',
     &      'IMS unit sensible recovery efficiency (SRE,-)',
     &       bRequired,
     &       0.0, 1.0,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Controls power
C-----------------------------------------------------------------------
      fIMS_ControlsPower(iSystem)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'controls-power',
     &      'IMS unit controls power consumption (W)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Circulation fan power
C-----------------------------------------------------------------------
      fP10_PowerCirc(iSystem)
     &   = fHVAC_FindTokenWithLimits(
     &       iSystem,
     &       'circ-fan-power',
     &      'IMS unit circulation fan power consumption (W)',
     &       bRequired,
     &       0.0, 1.0E04,
     &       bFound,
     &       bInLimits )


C-----------------------------------------------------------------------
C     Derive general IMS model inputs from P.10 data
C-----------------------------------------------------------------------
      call IMS_ConvertP10Data( iSystem )

C-----------------------------------------------------------------------
C     Search for other supported (but optional) inputs in
C     the misc-param block.
C
C     Determine if DHW loads should be read from a bcd file, and
C     collect apporpraite column names if so.
C 
C-----------------------------------------------------------------------

C.....Default BCD flags to true; reset them to false if a
C.....critical column is not found.
      bIMS_DHWLoadsDefinedInBCD( iSystem ) = .true.
      bIMS_SHLoadsDefinedInBCD( iSystem )  = .true.
      bIMS_OATempDefinedInBCD( iSystem )   = .true.

C.....DHW flow rates
      cIMS_BcdDhwFlow( iSystem ) =
     &        cHVAC_FindCToken( iSystem,
     &                          'dhw-bcd-flow',
     &                          'User imposed DHW flow rate',
     &                          .false.,
     &                          bFound )

      if ( .not. bFound ) bIMS_DHWLoadsDefinedInBCD( iSystem ) = .false.

      cIMS_BcdDhwDeliveryTemp( iSystem ) =
     &        cHVAC_FindCToken( iSystem,
     &                          'dhw-bcd-del-temp',
     &                          'User imposed DHW delivery temperature',
     &                          .false.,
     &                          bFound )

      if ( .not. bFound ) bIMS_DHWLoadsDefinedInBCD( iSystem ) = .false.

      cIMS_BcdDhwMakeupTemp( iSystem ) =
     &        cHVAC_FindCToken( iSystem,
     &                          'dhw-bcd-makeup-temp',
     &                          'User imposed DHW supply temperature',
     &                          .false.,
     &                          bFound )

      if ( .not. bFound ) bIMS_DHWLoadsDefinedInBCD( iSystem ) = .false.

      cIMS_BcdDhwLoadDuration( iSystem ) =
     &        cHVAC_FindCToken( iSystem,
     &                          'dhw-bcd-duration',
     &                          'User imposed DHW load duration',
     &                          .false.,
     &                          bFound )

      if ( .not. bFound ) bIMS_DHWLoadsDefinedInBCD( iSystem ) = .false.
C.....Space heating load
      cIMS_BcdSHLoad( iSystem ) =
     &        cHVAC_FindCToken( iSystem,
     &                          'sh-bcd-load',
     &                          'User imposed space-heating load (W)',
     &                          .false.,
     &                          bFound )

      if ( .not. bFound ) bIMS_SHLoadsDefinedInBCD( iSystem ) = .false.

C.....Outside air temperature 
      cIMS_BcdOATemp( iSystem ) =
     &        cHVAC_FindCToken( iSystem,
     &                          'OA-bcd-temp',
     &                          'User imposed OA temperature (oC)',
     &                          .false.,
     &                          bFound )

      if ( .not. bFound ) bIMS_OATempDefinedInBCD( iSystem ) = .false.

      return
      end
C================== HVAC_Baseboard_Input ===============================
C
C This routine parses the data block for baseboards
C
C Inputs:
C   -iSystem: System index
C
C=======================================================================
      subroutine HVAC_Baseboard_Input ( iSystem )
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"

C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem     ! System index
C-----------------------------------------------------------------------
C     Input/Output channel
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      character*248 cMsg  ! Message buffer
      logical bFound      ! Result of string search
      logical bInLimits   ! Flag indicating if value is within limits

C.....Named constants
      logical bRequired, bNotRequired
      parameter (bRequired = .true., bNotRequired = .false.)
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fHVAC_FindTokenNoLimits, fHVAC_FindTokenWithLimits
      real Fan_AutoSize_Improved

C-----------------------------------------------------------------------
C     Check to make sure all specified tokens match known list
C-----------------------------------------------------------------------
      call AreTokensValid ( iSystem )

C.....Steady-state capacity

      ss_capacity(iSystem) = fHVAC_FindTokenWithLimits(
     &                             iSystem,
     &                             'capacity',
     &                             'baseboard steady-state capacity',
     &                             bRequired,
     &                             0.0, 1.0e07,
     &                             bFound,
     &                             bInLimits )

C.....Steady-state efficency

      ss_efficiency(iSystem) = fHVAC_FindTokenWithLimits(
     &                            iSystem,
     &                            'efficiency',
     &                            'baseboard steady-state efficiency',
     &                            bRequired,
     &                            0.0, 1.0,
     &                            bFound,
     &                            bInLimits )

C.....Fan operation
      iFan_Operation(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                             iSystem,
     &                             'circ-fan-mode',
     &                             'furnace circulation fan mode',
     &                             bNotRequired,
     &                             bFound) )

      
      baseboard_fanfound: if ( bFound
     &    .and. iFan_Operation( iSystem ) /= iFan_None
     &    .and. iFan_Operation( iSystem ) /= iFan_Auto
     &    .and. iFan_Operation( iSystem ) /= iFan_Cont
     &    ) then

        write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting baseboard fan operation ',
     &    '(data item 1) for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 0  (no fan associated with baseboards)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 1  (Automatic fan operation)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 2  (continuious fan operation)'
        call edisp248( iUout, cMsg, 80 )
C.......Set error flag
        bParseError = .true.

      elseif ( bFound ) then

C.......If fan has been specified, read fan power.
        fan_power(iSystem) = fHVAC_FindTokenWithLimits
     &   ( iSystem,
     &     'circ-fan-power',
     &     'baseboard circulation fan power',
     &     bNotRequired,
     &     0.0, 1.0e07,
     &     bFound,
     &     bInLimits )

        if ( .not. bFound ) then
          fan_power( iSystem ) = Fan_AutoSize_Improved( iSystem )
          bFan_AutoSized(iSystem) = .true.
        endif 

      else

        iFan_Operation(iSystem) = iFan_None
        fan_power(iSystem) = 0.0

      endif baseboard_fanfound

      return
      end 



C================== HVAC_FuranaceBoiler_Input ==========================
C
C This routine parses the data block for furnaces and boilers.
C
C Inputs:
C   -iSystem: System index
C   -iType: system type keyword (iFurnace/iBoiler)
C
C=======================================================================
      subroutine HVAC_FuranaceBoiler_Input ( iSystem, iType )
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"

C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem     ! System index
      integer iType       ! Keyword indicating system type.
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      character*248 cMsg  ! Message buffer
      logical bFound      ! Result of string search
      logical bInLimits   ! Flag indicating if value is within limits

C.....Named constants
      logical bRequired, bNotRequired
      parameter (bRequired = .true., bNotRequired = .false.)

C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fHVAC_FindTokenNoLimits, fHVAC_FindTokenWithLimits
      real Fan_AutoSize_Improved
C-----------------------------------------------------------------------
C     Process data in fDataBlock
C
C     Furnace --- Expect 7 values:
C
C       - Furnace type (value between 1-14)
C       - Equipment steady-state capacity
C       - Equipment steady-state efficiency
C       - Furnace fan operation (value between 0-2)
C       - Circulation fan power
C       - Draft fan power
C       - Pilot light power
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Check to make sure all specified tokens match known list
C-----------------------------------------------------------------------
      call AreTokensValid ( iSystem )

C.....System type
      isys_type ( iSystem )
     &     = int ( fHVAC_FindTokenNoLimits( iSystem,
     &                                      'furnace-type',
     &                                      'Furnace type',
     &                                      bRequired,
     &                                      bFound ) )

      if (    isys_type ( iSystem ) /= iFurn_AdvWoodStove
     &  .and. isys_type ( iSystem ) /= iFurn_CatConverter
     &  .and. isys_type ( iSystem ) /= iFurn_FlameRetenHead
     &  .and. isys_type ( iSystem ) /= iFurn_FlueVentDamper
     &  .and. isys_type ( iSystem ) /= iFurn_SparkIgn
     &  .and. isys_type ( iSystem ) /= iFurn_SparkIgnVentDamper
     &  .and. isys_type ( iSystem ) /= iFurn_ContPilot
     &  .and. isys_type ( iSystem ) /= iFurn_Condensing
     &  .and. isys_type ( iSystem ) /= iFurn_NonCondensingDirVent
     &  .and. isys_type ( iSystem ) /= iFurn_InducedDraft
     &  .and. isys_type ( iSystem ) /= iFurn_MidEfficiency
     &  .and. isys_type ( iSystem ) /= iFurn_ElecForcedAir
     &  .and. isys_type ( iSystem ) /= iFurn_Wood
     &  .and. isys_type ( iSystem ) /= iFurn_Oil    ) then

        write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting furnace type (data item 1) ',
     &    'for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 1  (advanced air tight wood stove)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 2  (catalytic converter)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 3  (flame retention head)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 4  (flue vent damper)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 5  (spark ignition)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 6  (spark ignition with flue vent damper)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 7  (continuious pilot) '
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 8  (condensing)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 9  (direct vent, non condensing)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 10 (induced draft)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 11 (mid-efficiency)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 12 (electric forced-air)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 13 (wood furnace)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 14 (oil furnace)'
        call edisp248( iUout, cMsg, 80 )

C.......Set error flag
        bParseError = .true.

      endif
C---------------------------------------------------------------------
C     Set the correlation coefficients for the part-load performance by
c     calling FURNACE_COEFF. The correlation coefficients are stored in
c     common furnace_corr
C---------------------------------------------------------------------
      call FURNACE_COEFF(iSystem,isys_type(iSystem))

C.....Steady-state capacity

      ss_capacity(iSystem) = fHVAC_FindTokenWithLimits(
     &                             iSystem,
     &                             'capacity',
     &                             'furnace steady-state capacity',
     &                             bRequired,
     &                             0.0, 1.0e07,
     &                             bFound,
     &                             bInLimits )

C.....Steady-state efficency

      ss_efficiency(iSystem) = fHVAC_FindTokenWithLimits(
     &                            iSystem,
     &                            'efficiency',
     &                            'furnace steady-state efficiency',
     &                            bRequired,
     &                            0.0, 1.0,
     &                            bFound,
     &                            bInLimits )

C.....Fan operation
      iFan_Operation(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                             iSystem,
     &                             'circ-fan-mode',
     &                             'furnace circulation fan mode',
     &                             bRequired,
     &                             bFound) )

      if ( bFound
     &    .and. iFan_Operation( iSystem ) /= iFan_None
     &    .and. iFan_Operation( iSystem ) /= iFan_Auto
     &    .and. iFan_Operation( iSystem ) /= iFan_Cont
     &    ) then

        write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting furnace fan operation ',
     &    '(data item 1) for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 0  (no fan associated with this furance)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 1  (Automatic fan operation)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 2  (continuious fan operation)'
        call edisp248( iUout, cMsg, 80 )
C.......Set error flag
        bParseError = .true.
      endif

C.....Indoor fan power
      fan_power(iSystem) = fHVAC_FindTokenWithLimits
     &   ( iSystem,
     &     'circ-fan-power',
     &     'furnace circulation fan power',
     &     bNotRequired,
     &     0.0, 1.0e07,
     &     bFound,
     &     bInLimits )

      if ( .not. bFound ) then
        fan_power(iSystem) = Fan_AutoSize_Improved(iSystem)
        bFan_AutoSized(iSystem) = .true.
      endif


C.....Draft fan power
      draft_fan_power(iSystem) = fHVAC_FindTokenWithLimits(
     &                           iSystem,
     &                             'draft-fan-power',
     &                             'furnace draft fan power',
     &                             bNotRequired,
     &                             0.0, 1.0e07,
     &                             bFound,
     &                             bInLimits )

      if ( .not. bFound ) draft_fan_power(iSystem) = 0.0


C.....Pilot power
      pilot_power(iSystem) = fHVAC_FindTokenWithLimits(
     &                             iSystem,
     &                             'pilot-power',
     &                             'furnace pilot power',
     &                             bRequired,
     &                             0.0, 1.0e07,
     &                             bFound,
     &                             bInLimits )

C.....Duct system flag
      iduct_sys_flag(iSystem) = int( fHVAC_FindTokenWithLimits(
     &                             iSystem,
     &                             'duct-system-flag',
     &                             'duct-system-flag',
     &                             bNotRequired,
     &                             0.0, 1.0e07,
     &                             bFound,
     &                             bInLimits ) )

      if ( .not. bFound ) iduct_sys_flag(iSystem) = 1


      return
      end

C================== HVAC_HP_Input ==========================
C
C This routine parses the data block for ASHP, GSHP, GCEP models
C
C Inputs:
C   -iSystem: System index
C   -iType: system type keyword (iFurnace/iBoiler)
C
C=======================================================================
      subroutine HVAC_HP_Input ( iSystem, iType )
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "ashp_common.h"

C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem     ! System index
      integer iType       ! Keyword indicating system type.
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      character*248 cMsg  ! Message buffer
      logical bFound      ! Search result
      integer iToken_1    ! Counter
      integer iToken_2    ! Counter
      integer iLength_S   ! Length of a string
      logical bInLimits   ! Flag indicating if value is within limits
C.....Named constants
      logical bRequired, bNotRequired
      parameter (bRequired = .true., bNotRequired = .false.)
      logical bRequiredHeating, bRequiredCooling
C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer lnblnk
      real fHVAC_FindTokenNoLimits, fHVAC_FindTokenWithLimits
      real SYS_FLOW_RATE, Fan_AutoSize_Improved
C-----------------------------------------------------------------------
C     Parse data in fDataBlock.
C
C     ASHP models expect X inputs
C       - mode of operation (1-heating; 2-cooling)
C       - units performance (1-typical; 2-good; 3-poor)
C       - steady-state capacity
C       - steady-state COP
C       - circulation fan flow rate
C       - circulation fan rated flow rate
C       - fan operating mode (0-no fan; 1-auto; 2-continuous)
C       - circulation fan power
C       - outdoor fan power
C       - faction of total fan power in auto mode
C       - position of circulation fan in heat exchanger (1-blow-through;
C         2-draw-through)
C
C     GSHP/GCEP models expect X inputs
C       - mode of operation (1-heating; 2-cooling)
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Assign heating and cooling function flags
C-----------------------------------------------------------------------
      if ( iSys_Function(iSystem) == iFuncHeating ) then
        bRequiredHeating = .true.
        bRequiredCooling = .false.
      else 
        bRequiredHeating = .false.
        bRequiredCooling = .true.
      endif

C.....Update HP-specific iUnitFunc flag
      iUnit_Function ( iSystem ) = iSys_Function( iSystem )


C-----------------------------------------------------------------------
C     Check to make sure all specified tokens match known list
C-----------------------------------------------------------------------
      call AreTokensValid ( iSystem )
      
C-----------------------------------------------------------------------
C     Check performance keyword
C-----------------------------------------------------------------------


      hvac_type: if ( ihvac_type(iSystem) == iSysGSHP       .or.
     &                ihvac_type(iSystem) == iSysGSHP_GCEP       ) then

C-----------------------------------------------------------------------
C       If a GSHP or a GCEP system is being modelled, the correlation
C       coefficients will be determined in a slightly different way than
C       for ASHP systems.  GSHP coefficients will be based on entering
C       water temperature (EWT) and as such will vary every day of
C       the simulation. For that reason, they are calculated within the
C       time-step simulation, instead of here.  The following initializes
C       the correlation coefficients to zero.
C-----------------------------------------------------------------------  

           ahp(iSystem) = 0.0
           bhp(iSystem) = 0.0
           chp(iSystem) = 0.0
           dhp(iSystem) = 0.0
           isys_type(iSystem) = 4

      else 

     
        isys_type(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                          iSystem,
     &                          'performance',
     &                          'heat pump performance characteristics',
     &                          bRequired,
     &                          bFound ) )

        if ( bFound                .and.
     &     isys_type(iSystem) /= 1 .and.
     &     isys_type(iSystem) /= 2 .and.
     &     isys_type(iSystem) /= 3 ) then
          write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting "performance" token ',
     &    'for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
          call edisp248( iUout, cMsg, 80 )
          call edisp248( iUout,'   - 1  (typical unit)',80)
          call edisp248( iUout,'   - 2  (good unit)',   80)
          call edisp248( iUout,'   - 3  (poor unit)',   80)

        else
          call ASHP_COEFF(iSystem)
        end if

      endif hvac_type 



C-----------------------------------------------------------------------
C     Read capacity
C-----------------------------------------------------------------------
      ss_capacity(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'capacity',
     &       'heat pump steady-state capacity',
     &       bRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )


C-----------------------------------------------------------------------
C     Read COP
C-----------------------------------------------------------------------
      ss_COP(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'COP',
     &       'heat pump steady-state COP',
     &       bRequired,
     &       0.0, 100.0,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Read circulation fan flow rate
C-----------------------------------------------------------------------
      flow_rate(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'circ-fan-flow',
     &       'heat pump circulation fan flow rate',
     &       bNotRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C.....If the flow rate was not specified, estimate it.
      if ( .not. bFound ) flow_rate(iSystem) = SYS_FLOW_RATE(iSystem)
  
C-----------------------------------------------------------------------
C     Read circulation fan rated-flow rate
C-----------------------------------------------------------------------
      flow_rate_r(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'circ-fan-rated-flow',
     &       'heat pump circulation fan flow rate at rating conditions',
     &       bNotRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C.....If the flow rate was not specified, estimate it.
      if ( .not. bFound ) flow_rate_r(iSystem) = SYS_FLOW_RATE(iSystem)

C-----------------------------------------------------------------------
C     Fan operation
C-----------------------------------------------------------------------
        iFan_Operation(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                               iSystem,
     &                               'circ-fan-mode',
     &                               'furnace circulation fan mode',
     &                               bRequired,
     &                               bFound) )

        if ( bFound 
     &      .and. iFan_Operation( iSystem ) /= iFan_None
     &      .and. iFan_Operation( iSystem ) /= iFan_Auto
     &      .and. iFan_Operation( iSystem ) /= iFan_Cont
     &      ) then

          write (cMsg, '(A,A,I2,A)' )
     &      ' Error interpreting furnace fan operation ',
     &      '(data item 1) for HVAC system # ',
     &      iSystem,
     &      '. Value must be one of the following:'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 0  (no fan associated with this furance)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 1  (Automatic fan operation)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 2  (continuious fan operation)'
          call edisp248( iUout, cMsg, 80 )
C.........Set error flag
          bParseError = .true.
        endif


C-----------------------------------------------------------------------
C     Check fan location keyword
C-----------------------------------------------------------------------
      ifan_position(iSystem) = int( fHVAC_FindTokenNoLimits
     &     ( iSystem,
     &       'circ-fan-position',
     &       'heat pump circulation fan position',
     &       bNotRequired,
     &       bFound ) )

      if (ifan_position(iSystem) /= iFan_BlowThrough .and.
     &    ifan_position(iSystem) /= iFan_DrawThrough        ) then
        write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting "circ-fan-position" token ',
     &    'for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )
        call edisp248( iUout,'   - 1  (blow-through)',80)
        call edisp248( iUout,'   - 2  (draw-through)',   80)

C.......Set error flag
        bParseError = .true.
        
      endif 
C-----------------------------------------------------------------------
C     Read circulation fan power
C-----------------------------------------------------------------------
      fan_power(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'circ-fan-power',
     &       'heat pump circulation fan flow power',
     &       bNotRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C.....If not found, set value to less than 0. This will cause
C.....Fan_AutoSize_Improved routine to estimate its value
      if ( .not. bFound ) then
        fan_power(iSystem) = Fan_AutoSize_Improved(iSystem)
        bFan_AutoSized(iSystem) = .true.
      endif

C-----------------------------------------------------------------------
C     Read circulation fan rated power
C-----------------------------------------------------------------------
      fan_power_r(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'circ-fan-rated-power',
     &       'heat pump circulation fan rated power',
     &       bNotRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C.....If not found, set value to less than 0. This will cause
C.....Fan_AutoSize_Improved routine to estimate its value
      if ( .not. bFound ) then
          fan_power_r(iSystem) = Fan_AutoSize_Improved(iSystem)
          bFan_Rated_AutoSized(iSystem) = .true.
        endif

C-----------------------------------------------------------------------
C     Read outdoor fan power
C-----------------------------------------------------------------------
      outdoor_fan_power(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'outdoor-fan-power',
     &       'heat pump outdoor fan power',
     &       bRequiredCooling,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Check fan location during rating keyword
C-----------------------------------------------------------------------
      ifan_position_r(iSystem) = int( fHVAC_FindTokenNoLimits
     &     ( iSystem,
     &       'circ-fan-rated-position',
     &       'heat pump circulation location during rating',
     &       bNotRequired,
     &       bFound ) )

      if ( bFound                        .and.
     &     ifan_position_r(iSystem) /= 1 .and.
     &     ifan_position_r(iSystem) /= 2  ) then
        write (cMsg, '(A,A,I2,A)' )
     &    ' Error interpreting "circ-fan-rated-position" token ',
     &    'for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )
        call edisp248( iUout,'   - 1  (blow-through)',80)
        call edisp248( iUout,'   - 2  (draw-through)',   80)

C.......Set error flag
        bParseError = .true.

      endif 
      
C-----------------------------------------------------------------------
C     Read fan power in auto
C-----------------------------------------------------------------------

      
      fan_power_auto(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'fan-power-in-auto',
     &       'heat pump circulation fan power use in auto',
     &       bNotRequired,
     &       0.0, 1.0e07,
     &       bFound,
     &       bInLimits )

C.....If not found, set value to less than 0. This will cause
C.....Fan_AutoSize_Improved routine to estimate its value
      if ( iFan_Operation(iSystem) == iFan_Auto
     &     .and. .not. bFound
     &   ) then
          fan_power_auto(iSystem) = Fan_AutoSize_Improved(iSystem)
          bFan_Auto_AutoSized(iSystem) = .true.
      elseif ( .not. bFound ) then
          fan_power_auto(iSystem) = 0.0
      endif

C-----------------------------------------------------------------------
C     Heating temperature control  
C-----------------------------------------------------------------------

      iTemp_Control(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                     iSystem,
     &                     'temp-control-mode',
     &                     'heat pump temperature control strategy',
     &                     bRequiredHeating,
     &                     bFound) )

      if ( bFound
     &    .and. iTemp_Control( iSystem ) /= iTemp_BalancedPoint
     &    .and. iTemp_Control( iSystem ) /= iTemp_Restricted
     &    .and. iTemp_Control( iSystem ) /= iTemp_Unrestricted
     &    ) then

        write (cMsg, '(A,A,I2,A)' )
     &    'Error interpreting heating temperature control strategy ',
     &    '(data item 1) for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 1  (Balanced point)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 2  (Restricted)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 3  (Unrestricted)'
        call edisp248( iUout, cMsg, 80 )
C.......Set error flag
        bParseError = .true.
      endif

C-----------------------------------------------------------------------
C     Cut-off temperature
C-----------------------------------------------------------------------
      if ( bFound .and.
     &     bRequiredHeating .and.
     &     iTemp_Control(iSystem) == iTemp_Restricted
     &    ) then
        cutoff_temp(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'cut-off-temperature',
     &       'heat pump control cut-off temperature',
     &       bRequired,
     &       -99.0, 99.0,
     &       bFound,
     &       bInLimits )
      endif 

C-----------------------------------------------------------------------
C     Sensible heat ratio 
C-----------------------------------------------------------------------
      shrr(iSystem) = fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'SHR',
     &       'heat pump sensible heat ratio',
     &       bRequiredCooling,
     &       0.0, 99.0,
     &       bFound,
     &       bInLimits )

C-----------------------------------------------------------------------
C     Cooling control strategy
C-----------------------------------------------------------------------

      iCooling_Type(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                     iSystem,
     &                     'cooling-control-mode',
     &                     'heat pump cooling control strategy',
     &                     bRequiredCooling,
     &                     bFound) )

      if ( bFound
     &    .and. iCooling_Type( iSystem ) /= iCool_Conventional
     &    .and. iCooling_Type( iSystem ) /= iCool_Economizer
     &    ) then

        write (cMsg, '(A,A,I2,A)' )
     &    'Error interpreting cooling control strategy ',
     &    '(data item 1) for HVAC system # ',
     &    iSystem,
     &    '. Value must be one of the following:'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 1  (Conventional system)'
        call edisp248( iUout, cMsg, 80 )

        write (cMsg, '(A)')
     &    '   - 2  (Economizer)'
        call edisp248( iUout, cMsg, 80 )

C.......Set error flag
        bParseError = .true.
      endif

      economizer: if ( iCooling_Type(iSystem) == iCool_Economizer ) then
C-------------------------------------------------------------------------
C       Economizer control strategy
C-------------------------------------------------------------------------
        iEconomizer_Control(iSystem) = int( fHVAC_FindTokenNoLimits(
     &                       iSystem,
     &                       'economizer-control-type',
     &                       'economizer control strategy',
     &                       bRequired,
     &                       bFound) )

        if ( bFound
     &   .and. iEconomizer_Control( iSystem ) /= iEcon_TempIntegrated
     &   .and. iEconomizer_Control( iSystem ) /= iEcon_TempNonIntegrated
     &   .and. iEconomizer_Control( iSystem ) /= iEcon_EnthIntegrated
     &   .and. iEconomizer_Control( iSystem ) /= iEcon_EnthNonIntegrated
     &   ) then

          write (cMsg, '(A,A,I2,A)' )
     &      'Error interpreting economizer control strategy ',
     &      '(token "economizer-control-mode") for HVAC system # ',
     &      iSystem,
     &      '. Value must be one of the following:'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 1  (Indoor/outdoor temperature integrated compressor'
          call edisp248( iUout, cMsg, 80 )
          write (cMsg, '(A)')
     &    '         control and option for outdoor dry-bulb temp. limit'
          call edisp248( iUout, cMsg, 80 )
          write (cMsg, '(A)')
     &      '         control.)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &   '   - 2  (Indoor/outdoor temperature non-integrated compressor'
          call edisp248( iUout, cMsg, 80 )
          write (cMsg, '(A)')
     &   '         control and option for outdoor dry-bulb temp. limit'
          call edisp248( iUout, cMsg, 80 )
          write (cMsg, '(A)')
     &     '         control.)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &   '   - 3  (Enthalpy-integrated compressor control and enthalpy'
          call edisp248( iUout, cMsg, 80 )
          write (cMsg, '(A)')
     &     '         limit control.)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &   '   - 4  (Enthalpy non-integrated compressor control and '
          call edisp248( iUout, cMsg, 80 )
          write (cMsg, '(A)')
     &     '         enthalpy limit control.)'
          call edisp248( iUout, cMsg, 80 )          

C.........Set error flag
          bParseError = .true.
        endif


C-------------------------------------------------------------------------
C       Economizer minimum outdoor air flow
C-------------------------------------------------------------------------
        economizer_min_out_air_flow(iSystem) = fHVAC_FindTokenWithLimits
     &       ( iSystem,
     &         'economizer-min-OA-flow',
     &         'economizer minimum outdoor air flow',
     &         bRequired,
     &         0.0, 99.0,
     &         bFound,
     &         bInLimits )

C-------------------------------------------------------------------------
C       Economizer indoor temperature set point
C-------------------------------------------------------------------------
        economizer_ind_temp_setpoint(iSystem)
     &     = fHVAC_FindTokenWithLimits
     &       ( iSystem,
     &         'economizer-indoor-set-point',
     &         'economizer indoor temperature set point',
     &         bRequired,
     &         -99.0, 99.0,
     &         bFound,
     &         bInLimits )

        economizer_limit: select case ( iEconomizer_Control(iSystem) )
C-------------------------------------------------------------------------
C       Economizer outdoor temperature limit
C-------------------------------------------------------------------------
        case ( iEcon_TempIntegrated, iEcon_TempNonIntegrated ) 
          economizer_out_temp_limitcon(iSystem)
     &      = fHVAC_FindTokenWithLimits
     &       ( iSystem,
     &         'economizer-OA-temp-limit',
     &         'economizer indoor temperature limit',
     &         bRequired,
     &         -99.0, 99.0,
     &         bFound,
     &         bInLimits )

C-------------------------------------------------------------------------
C       Economizer outdoor enthalpy limit
C-------------------------------------------------------------------------
        case ( iEcon_EnthIntegrated, iEcon_EnthNonIntegrated )
        economizer_out_temp_limitcon(iSystem)
     &      = fHVAC_FindTokenWithLimits
     &       ( iSystem,
     &         'economizer-OA-enth-limit',
     &         'economizer indoor enthalpy limit',
     &         bRequired,
     &         0.0, 1.0E06,
     &         bFound,
     &         bInLimits )
        end select economizer_limit
      endif economizer 
C-----------------------------------------------------------------------
C     Corresponding system number for heating mode
C-----------------------------------------------------------------------
      isys_num_heating(iSystem) = int ( fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'sys-num-for-heating-mode',
     &       'corresponding system # for heating mode',
     &       bNotRequired,
     &       0.0, real(max_sys),
     &       bFound,
     &       bInLimits ) )

C.....This kludge has no affect on the model, but ensures parity
C.....in .h3k files.     
      if ( iUnit_Function ( iSystem ) == iFuncCooling .and.
     &     .not. bFound ) isys_num_heating(iSystem) = iSystem

C-----------------------------------------------------------------------
C     Corresponding control function for cooling mode:
C-----------------------------------------------------------------------
      icontrol_function(iSystem) = int ( fHVAC_FindTokenWithLimits
     &     ( iSystem,
     &       'ctl-file-function-num',
     &       'corresponding function number from .ctl file',
     &       bRequiredCooling,
     &       0.0, 20.0,
     &       bFound,
     &       bInLimits ) )
C-----------------------------------------------------------------------
C     For a heat pump in the cooling mode, it is possible using the
C     information at rating conditions to determine the bypass factor
C     of the unit at these rating conditions. This will be used in
C     subroutine ASHP_COOLING to estimate the energy consumption of unit.
C-----------------------------------------------------------------------
      if ( iUnit_Function ( iSystem ) == iFuncCooling ) then
        
        call BYPASS_FACTOR_R( iSystem )

      endif 

      return
      end

C================== fHVAC_FindToken ====================================
C
C This routine searches for a string inside the array 'cTokens',
C and returns the corresponding value inside 'fTokenVals'
C
C
C Inputs:
C   -iSystem:      Index of the system
C   -cToken:       String to search for
C   -cDescription: Contextual description
C   -bLimitCheck:  Flag indicating if limits should be checked
C   -fLLimit, fULimit:
C                  Lower and upper limits
C   -bRequired:    Flag indicating if token is required (that is,
C                  should a fatal error occur if it isn't found?)
C
C Outputs:
C   -fHVAC_FindToken: float value from fTokenVals
C   -bFound: flag indicating if token was found.
C   -bInLimits: flag indicating if token was found to be within limits.
C
C fHVAC_FindToken also sets the global variable, bParseError, if an
C input fails its limit check (provided limit-checking is specified),
C or if a token is not found (provided 'bRequired' is 'true').
C
C=======================================================================


C-----------------------------------------------------------------------
C     Interface function: Call fHVAC_FindToken without optional
C     limit-checking
C-----------------------------------------------------------------------
      real function fHVAC_FindTokenNoLimits
     &                             (iSystem,
     &                              cToken,
     &                              cDescription,
     &                              bReqired,
     &                              bFound)
      implicit none
#include "hvac_parameters.h"
      integer iSystem
      character*(*) cToken, cDescription
      logical bFound, bReqired, bInLimits
      real fLLimit, fULimit, fTokenVal
      character*72 cTokenVal

      fLLimit = 0.0
      fULimit = 0.0

      call HVAC_FindToken(iSystem,
     &                     cToken,
     &                     cDescription,
     &                     fLLimit,fULimit,
     &                     .false.,
     &                     bReqired,
     &                     iSearchDataTokens,
     &                     bFound,
     &                     fTokenVal,
     &                     cTokenVal,
     &                     bInLimits)

      fHVAC_FindTokenNoLimits = fTokenVal

      return
      end function fHVAC_FindTokenNoLimits

C-----------------------------------------------------------------------
C     Interface function: Call fHVAC_FindToken with optional
C     limit-checking
C-----------------------------------------------------------------------
      real function fHVAC_FindTokenWithLimits
     &                             (iSystem,
     &                              cToken,
     &                              cDescription,
     &                              bRequired,
     &                              fLLimit, fULimit,
     &                              bFound,
     &                              bInLimits)
      implicit none
#include "hvac_parameters.h"

      integer iSystem
      character*(*) cToken, cDescription
      logical bFound, bRequired, bInLimits
      real fLLimit, fULimit, fTokenVal
      character*72 cTokenVal

      call HVAC_FindToken(iSystem,
     &                     cToken,
     &                     cDescription,
     &                     fLLimit,fULimit,
     &                     .true.,
     &                     bRequired,
     &                     iSearchDataTokens,
     &                     bFound,
     &                     fTokenVal,
     &                     cTokenVal,
     &                     bInLimits)

      fHVAC_FindTokenWithLimits = fTokenVal

      return
      end function fHVAC_FindTokenWithLimits

C-----------------------------------------------------------------------
C     Interface function: call HVAC_FindToken to locate
C     character value in misc  parameter block.
C-----------------------------------------------------------------------
      character*72 function cHVAC_FindCToken( iSystem,
     &                                        cToken,
     &                                        cDescription,
     &                                        bRequired,
     &                                        bFound       )
      implicit none
#include "hvac_parameters.h"
      integer iSystem
      character*(*) cToken, cDescription
      logical bFound, bRequired, bInLimits
      real fLLimit, fULimit, fTokenVal
      character*72 cTokenVal

      fLLimit = 0.
      fULimit = 0.

      call HVAC_FindToken(iSystem,
     &                     cToken,
     &                     cDescription,
     &                     fLLimit,fULimit,
     &                     .false.,
     &                     bRequired,
     &                     iSearchParamTokens,
     &                     bFound,
     &                     fTokenVal,
     &                     cTokenVal,
     &                     bInLimits)

      write( cHVAC_FindCToken, '(A)' )  cTokenVal(1:72)

      return
      end 


C-----------------------------------------------------------------------
C     Principal function
C-----------------------------------------------------------------------
      subroutine HVAC_FindToken(iSystem,
     &                              cToken,
     &                              cDescription,
     &                              fLLimit, fULimit,
     &                              bLimitCheck,
     &                              bRequired,
     &                              iSearchSet,
     &                              bFound,
     &                              fTokenVal,
     &                              cTokenVal,
     &                              bInLimits)
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"

C-----------------------------------------------------------------------
C     Passed arguements.
C-----------------------------------------------------------------------
      integer iSystem                 ! System #
      character*(*) cToken            ! Search token
      character*(*) cDescription      ! Description
      logical bFound                  ! Result of search
      logical bLimitCheck             ! Should limits be checked?
      logical bInLimits               ! Is input in limits?
      integer iSearchSet              ! Pointer indicating the
                                      !  array to search - real data
                                      !  block or character misc. param
                                      !  block.
      logical bRequired               ! Is token critical?
      real fLLimit, fULimit           ! Lower and upper limits for
                                      !   bounds check.
      real fTokenVal                  ! Token value, if numeric
      character*72 cTokenVal          ! Token value, if alphanumeric

C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      integer iData                   ! counter
      integer iStrLength              ! Length of search string
      character*248 cMsg              ! Msg buffer
      logical bNumsAreClose           ! flag for close-to-zero comps.

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer lnblnk



C.....Initialize result flag
      bFound = .false.

C.....Get length of search-string
      iStrLength = lnblnk( cToken )

      SelectSearchSource: select case ( iSearchSet )

        case ( iSearchDataTokens ) 
C-----------------------------------------------------------------------
C         Now loop through cTokens array, and search for cToken
C-----------------------------------------------------------------------
          do iData = 1, iDataCount ( iSystem )

C...........Do tokens match?
            if ( cToken(1:iStrLength) ==
     &          cTokens(iSystem,iData)(1:lnblnk(cTokens(iSystem,iData)))
     &          ) then
C.............Yes! Recover value
              fTokenVal = fTokenVals( iSystem, iData)

C.............Set 'found' result
              bFound = .true.

            else
C.............No! Do nothing

            endif

          enddo

        case ( iSearchParamTokens)

C-----------------------------------------------------------------------
C         Now loop through cMiscTokens array, and search for cToken
C-----------------------------------------------------------------------
          do iData = 1, iMiscParamCount ( iSystem )

C...........Do tokens match?
            if ( cToken(1:iStrLength) ==
     &          cMiscTokens(iSystem,iData)
     &              (1:lnblnk(cMiscTokens(iSystem,iData)))
     &          ) then
C.............Yes! Recover value
              write ( cTokenVal, '(A)' ) cMiscVals(iSystem, iData)

C.............Set 'found' result
              bFound = .true.

            else
C.............No! Do nothing

            endif

          enddo

        case default

C..........Can't happen; iSearchSet set programmically.

      end select SelectSearchSource

C-----------------------------------------------------------------------
C     Check if token was found 
C-----------------------------------------------------------------------
      token_found: if ( .not. bFound ) then

C.......No! default value to zero.
        fTokenVal = 0.0

C-----------------------------------------------------------------------
C       If token was not found, then check if it was required.
C-----------------------------------------------------------------------
        token_required: if ( bRequired ) then
C.........Yes! Set parse error, and warn user.
          bParseError = .true.

          write (cMsg, '(A,A,A,I2,A,A,A)')
     &    'Error: token "',
     &    cToken(1:min(48,lnblnk(cToken))),
     &    '" was not found while parsing data block for HVAC system #',
     &    iSystem,'. This token describes the ',
     &    cDescription(1:min(72,lnblnk(cDescription))),
     &    '. Add token to .hvac file.'

          call edisp248 (iUout, cMsg, 80)

        endif token_required
C-----------------------------------------------------------------------
C     Token was found. Check limits, if requested.
C-----------------------------------------------------------------------
      elseif ( bLimitCheck ) then 

        call eclose ( fTokenVal, fLLimit, 1.0e-07, bNumsAreClose)

        if ( fTokenVal < fLLimit .and. .not. bNumsAreClose ) then

C.........Value is less than lower-limit. Set flag and warn user.
          bParseError = .true.

          write (cMsg, '(A,I2,A,A,A,F12.4,A,F12.4,A,A,A)')
     &    'Error: HVAC system # ', iSystem,' - ',
     &     cDescription(1:min(72,lnblnk(cDescription))), 
     &    ' (', fTokenVal,') must be greater-than or equal to ',
     &    fLLimit,'. Check value of token "',
     &    cToken(1:min(48,lnblnk(cToken))),'".'

          call edisp248 (iUout, cMsg, 80)

        endif

        call eclose ( fTokenVal, fULimit, 1.0e-07, bNumsAreClose)

        if ( fTokenVal > fULimit .and. .not. bNumsAreClose ) then

C.........Value is greater than upper-limit. Set flag and warn user.
          bParseError = .true.

          write (cMsg, '(A,I2,A,A,A,F12.4,A,F12.4,A,A,A)')
     &    'Error: HVAC system # ', iSystem,' - ',
     &     cDescription(1:min(72,lnblnk(cDescription))), 
     &    ' (', fTokenVal,') must be less-than or equal to ',
     &    fULimit,'. Check value of token "',
     &    cToken(1:min(48,lnblnk(cToken))),'".'

          call edisp248 (iUout, cMsg, 80)

        endif

      endif token_found

      return
      end subroutine 


C===================== AreTokensValid ==================================
C This routine compares two lists of tokens to ensure all the 
C specified tokens match known values
C
C inputs:
C  - iSystem - index of current system
C
C outputs:
C  - None.
C
C=======================================================================
      subroutine AreTokensValid (iSystem)
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"

C-----------------------------------------------------------------------
C     Passed variables
C-----------------------------------------------------------------------
      integer iSystem

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      integer iUserToken, iValidToken
      integer iValidTokenCount
      logical bFound, bTokenError
      character*72 cValidToken
      character*248 cMsg
C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer lnblnk

C-----------------------------------------------------------------------
C     Find length of valid token array according to system type
C-----------------------------------------------------------------------
      find_length: select case ( iHVAC_type(iSystem) )

        case ( iSysFurnace, iSysBoiler )
          iValidTokenCount = iFurnaceTokenCount

        case ( iSysIMS_P10 )
          iValidTokenCount = iP10_IMSTokenCount

        case ( iSysBaseboard )
          iValidTokenCount = iBaseboardTokenCount

        case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP )
          iValidTokenCount = iHPTokenCount
          
        case default
          ! Can't happen - iHVAC_type error trapped above.

      end select find_length
      
C-----------------------------------------------------------------------
C     Check that all tokens match known values
C-----------------------------------------------------------------------
      bTokenError = .false.
      search_tokens: do iUserToken = 1, iDataCount( iSystem )

C.......Initialize found flag
        bFound = .false.

C.......Look for strings in known token list
        valid_token_loop: do iValidToken = 1, iValidTokenCount

C.........Check system type and collect token from appropriate
C.........array

          collect_valid_token: select case ( iHVAC_type (iSystem) )
            case ( iSysFurnace, iSysBoiler )
              cValidToken = cFurnaceTokenNames(iValidToken)
    
            case ( iSysIMS_P10 )
              cValidToken  = cP10_IMSTokenNames(iValidToken)
    
            case ( iSysBaseboard )
              cValidToken  = cBaseboardTokenNames(iValidToken)
    
            case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP )
              cValidToken  = cHPTokenNames(iValidToken)

          end select collect_valid_token

          if ( cTokens(iSystem,iUserToken) == cValidToken  )
     &         bFound = .true.

        enddo valid_token_loop

C.......If token wasn't found, then report message to user,
C.......and provide list of valid tokens
        token_not_found: if ( .not. bFound ) then

          write (cMsg, '(A,A,A,I2)') ' Error: Unknown token "',
     &      cTokens(iSystem,iUserToken)
     &             (1:lnblnk(cTokens(iSystem,iUserToken))),
     &      '" encountered in HVAC system #', iSystem
          call edisp248(iUout, cMsg, 80)

C.........Set error flag
          bTokenError = .true.

        endif token_not_found

      enddo search_tokens

C-----------------------------------------------------------------------
C     If an error occured, list valid tokens
C-----------------------------------------------------------------------
      token_error: if ( bTokenError ) then

        call edisp248(iUout,' Valid tokens are:', 80)
        
        list_token_loop: do iValidToken = 1, iValidTokenCount
          get_valid_token: select case ( iHVAC_type (iSystem) )
            case ( iSysFurnace, iSysBoiler )
              cValidToken = cFurnaceTokenNames(iValidToken)

            case ( iSysIMS_P10 )
              cValidToken  = cP10_IMSTokenNames(iValidToken)

            case ( iSysBaseboard )
              cValidToken  = cBaseboardTokenNames(iValidToken)

            case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP )
              cValidToken  = cHPTokenNames(iValidToken)

          end select get_valid_token
          
          write (cMsg, '(A,A)') '  - ',
     &      cValidToken(1:lnblnk(cValidToken))

          call edisp248(iUout, cMsg, 80)

        enddo list_token_loop
C.......Set gloabal error flag
        bParseError = .true. 

      endif token_error

      return
      end subroutine AreTokensValid
C===================== Initialize_tokens ===============================
C-----------------------------------------------------------------------
C Initialize token lists. These arrays describe the various
C tokens that can be specified for each of the three primary
C system topologies (furnace/boiler, baseboard & GSHP/ASHP)
C-----------------------------------------------------------------------
      blockdata initialize_tokens
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"

      integer iName,iSys

      data (cP10_IMSTokenNames(iName),iName=1,iP10_IMSTokenCount)
     &                         /'SH-capacity          ',
     &                          'nominal-burner-input ',
     &                          'SH-net-eff-15        ',
     &                          'SH-net-eff-40        ',
     &                          'SH-net-eff-100       ',
     &                          'SH-power-15          ',
     &                          'SH-power-40          ',
     &                          'SH-power-100         ',
     &                          'heat-source-type     ',
     &                          'WHPF                 ',
     &                          'vent-FA-flow-C       ',
     &                          'vent-EA-flow-C       ',
     &                          'vent-FA-flow-H       ',
     &                          'vent-EA-flow-H       ',
     &                          'vent-FA-fan-power-C  ',
     &                          'vent-EA-fan-power-C  ',
     &                          'vent-FA-fan-power-H  ',
     &                          'vent-EA-fan-power-H  ',
     &                          'controls-power       ',
     &                          'circ-fan-power       ',
     &                          'vent-SRE             ' /
     


      data (cFurnaceTokenNames(iName),iName=1,iFurnaceTokenCount)
     &                         /'furnace-type    ',
     &                          'capacity        ',
     &                          'efficiency      ',
     &                          'circ-fan-mode   ',
     &                          'circ-fan-power  ',
     &                          'draft-fan-power ',
     &                          'pilot-power     ',
     &                          'duct-system-flag'  /

      data (cHPTokenNames(iName),iName=1,iHPTokenCount)
     &                   /  'performance                ',
     &                      'capacity                   ',
     &                      'COP                        ',
     &                      'circ-fan-mode              ',
     &                      'circ-fan-flow              ',
     &                      'circ-fan-rated-flow        ',
     &                      'circ-fan-position          ',
     &                      'circ-fan-power             ',
     &                      'outdoor-fan-power          ',
     &                      'fan-power-in-auto          ',
     &                      'circ-fan-rated-position    ',
     &                      'circ-fan-rated-power       ',
     &                      'circ-fan-mode              ',
     &                      'temp-control-mode          ',
     &                      'cut-off-temperature        ',
     &                      'SHR                        ',
     &                      'cooling-control-mode       ',
     &                      'economizer-control-type    ',
     &                      'economizer-min-OA-flow     ',
     &                      'economizer-indoor-set-point',
     &                      'economizer-OA-temp-limit   ',
     &                      'economizer-OA-enth-limit   ',
     &                      'sys-num-for-heating-mode   ',
     &                      'ctl-file-function-num      '  /

      data (cBaseboardTokenNames(iName),iName=1,iBaseboardTokenCount)
     &                          / 'circ-fan-mode  ',
     &                            'circ-fan-power ',
     &                            'capacity       ',
     &                            'efficiency     '  /

      end blockdata initialize_tokens