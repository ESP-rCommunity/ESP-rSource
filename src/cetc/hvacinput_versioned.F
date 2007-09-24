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

      integer iFileLine           ! Line number in file

C.....Parsing flags
      logical bVersionOK
      logical bSystemBlockOpen
      logical bZoneBlockOpen
      logical bDataBlockOpen
      logical bOASchedBlockOpen
      logical bDayTypeBlockOpen
      logical bSiteAltitudeOK

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
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer iEGetArrW
      real fCtoR_err             ! Convert a char to a real
      integer iCtoI_err          ! Convert a char to an int

C-----------------------------------------------------------------------
C     Initialize token lists
C-----------------------------------------------------------------------
      integer iName,iSys
      data (cFurnaceTokenNames(iName),iName=1,iFurnaceTokenCount)
     &                         /'furnace-type   ',
     &                          'capacity       ',
     &                          'efficiency     ',
     &                          'circ-fan-mode  ',
     &                          'circ-fan-power ',
     &                          'draft-fan-power',
     &                          'pilot-fan-power'  /

      integer    iFurnaceLocs
      parameter (iFurnaceLocs=max_sys*iFurnaceTokenCount)
      data ( (bFurnaceTokenSet(iSys,iName),
     &        iName=1,iFurnaceTokenCount),
     &        iSys=1,max_sys )
     &      / iFurnaceLocs * .false. /




      data (cHPTokenNames(iName),iName=1,iHPTokenCount)
     &                   /  'performance              ',
     &                      'capacity                 ',
     &                      'COP                      ',
     &                      'circ-fan-flow            ',
     &                      'circ-fan-rated-flow      ',
     &                      'circ-fan-position        ',
     &                      'circ-fan-power           ',
     &                      'outdoor-fan-power        ',
     &                      'fan-power-in-auto        ',
     &                      'circ-fan-rated-position  ',
     &                      'circ-ran-rated-power     ',
     &                      'temp-control-mode        ',
     &                      'cut-off-temperature      ',
     &                      'SHR                      ',
     &                      'cooling-control-mode     ',
     &                      'cooling-control-mode     ',
     &                      'econ-control-type        ',
     &                      'econ-min-OA-flow         ',
     &                      'econ-space-set-point     ',
     &                      'econ-OA-temp-limit       ',
     &                      'econ-OA-enth-limit       ',
     &                      'sys-num-for-heating-mode ',
     &                      'ctl-file-function-num    '  /


      integer    iHPLocs
      parameter (iHPLocs=max_sys*iHPTokenCount)
      data ( (bHPTokenSet(iSys,iName),
     &        iName=1,iHPTokenCount),
     &        iSys=1,max_sys )
     &      / iHPLocs * .false. /


      data (cBaseboardTokenNames(iName),iName=1,iBaseboardTokenCount)
     &                          / 'circ-fan-mode  ',
     &                            'circ-fan-power ',
     &                            'draft-fan-power',
     &                            'pilot-fan-power'  /

      integer    iBaseboardLocs
      parameter (iBaseboardLocs=max_sys*iBaseboardTokenCount)
      data ( (bBaseboardTokenSet(iSys,iName),
     &        iName=1,iBaseboardTokenCount),
     &        iSys=1,max_sys )
     &      / iBaseboardLocs * .false. /

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

      do iSystem = 1, max_sys
        bSysPriorityOK(iSystem)  = .false.
        bSysZonesOK(iSystem)     = .false.
        bSysDataOK(iSystem)      = .false.
        bSysTypeOK(iSystem)      = .false.
        bSysFuelOK(iSystem)      = .false.
        bSysNameOK(iSystem)      = .false.
        bSysFuncOK(iSystem)      = .false.
        bSysBackOK(iSystem)      = .false.
        bSysOAPeriodsOK(iSystem) = .false.
        

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
      do while ( iFileErr == 0 )

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
        if ( iFileErr == 0 .or.
     &       ( iFileErr == 2 .and. iWordCount .gt. 0 ) ) then

C-----------------------------------------------------------------------
C         Now interpret first word on line:
C         Parse *version keyword, and associated version
C-----------------------------------------------------------------------
          if ( cLnWords(1)(1:8) == '*version' ) then

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

            if ( iWordCount /= 2 ) then

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

              if ( cLnWords(2)(1:8) == 'furnace ' ) then
                ihvac_type(iSystem) = iSysFurnace

              elseif ( cLnWords(2)(1:7) == 'boiler ' ) then
                ihvac_type(iSystem) = iSysBoiler

              elseif ( cLnWords(2)(1:10) == 'baseboard ' ) then
                ihvac_type(iSystem) = iSysBaseboard

              elseif ( cLnWords(2)(1:4) == 'IMS ' ) then
                ihvac_type(iSystem) = iSysIMS

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

              endif ! <- matches 'if ( cLnWords(2)(1:8) == 'furnace ' )'

            endif



C-----------------------------------------------------------------------
C         Parse *fuel tag.
C-----------------------------------------------------------------------

          elseif (cLnWords(1)(1:6) == '*fuel ' ) then

            if ( iWordCount /= 2 ) then

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
              if ( cLnWords(2)(1:12) == 'electricity ' ) then

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

              endif  !<- matches if ( cLnWords(2)(1:12) == 'elec'

            endif

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

                ipriority(iSystem) = 1

              elseif ( cLnWords(2)(1:7) == 'backup ') then

                ipriority(iSystem) = 2

              else

                write (cMsg, '(A,A,A,I2,A)')
     &          ' Error parsing system priority ("',
     &          cLnWords(2)(1:lnblnk(cLnWords(2))),
     &          '") for HVAC system #', iSystem,
     &          '. Value must be "primary" or "backup".'
                  call edisp248(iuout,cMsg,80)

                call HVACinput_RepErr(iFileLine,
     &               'Could not interpret system priority')
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
                bSysZonesOK(iSystem) = .false.
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
     &          //'*data-end tag are unsupported. ')

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

C............. open block
              bOASchedBlockOpen = .true.


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
     &                'Could not interpret token value')

              endif

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

          endif      !<- matches "if ( cLnWords(1)(1:X) == ... "

        endif        !<- matches "if ( iFileErr == 0 )"

      enddo          !<- matches "do while ( iFileErr == 0 )..."
C-----------------------------------------------------------------------
C     Now loop through systems and validate data.
C-----------------------------------------------------------------------
      do iSystem = 1, num_hvac_sys

         ! Debugging statements

         print*, "System: ", iSystem
         do iData = 1, iDataCount(iSystem)
            print*, "TV: ",
     &      cTokens(iSystem, iData)(1:lnblnk(cTokens(iSystem,iData))),
     &      " --- ", fTokenVals(iSystem,iData)

         enddo


C-----------------------------------------------------------------------
C       Parse and validate block data
C-----------------------------------------------------------------------
        select case ( ihvac_type (iSystem) )

          case ( iSysFurnace )

            call HVAC_FuranaceBoiler_Input(iSystem,iSysFurnace)

          case ( iSysBoiler )

            call HVAC_FuranaceBoiler_Input(iSystem,iSysBoiler)

          case ( iSysBaseboard )

          case ( iSysIMS )

          case ( iSysASHP )

          case ( iSysGSHP )

          case ( iSysGSHP_GCEP )

          case default

        end select

C-----------------------------------------------------------------------
C     Validate furnace/fuel combination
C-----------------------------------------------------------------------


        select case ( ihvac_type (iSystem) )

          case ( iSysFurnace )

          case ( iSysBoiler )

          case ( iSysIMS )

          case default

        end select

      enddo
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
C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen
C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      real fHVAC_FindToken
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

C.....Count data items
      if ( iDataCount( iSystem ) /= 7 ) then
        write (cMsg, '(A,A,I2,A,I2,A)' )
     &    ' Error interpreting furnace data ',
     &    'for HVAC system # ',
     &    iSystem,
     &    '. ', iDataCount ( iSystem ),
     &    ' items were specified but 7 items are expected.'
        call edisp248( iUout, cMsg, 80 )
        bParseError = .true.

      else

C.......System type
!         isys_type ( iSystem ) = int ( fDataBlock( iSystem, 1 ) )
        isys_type ( iSystem )
     &       = int ( fHVAC_FindToken( iSystem,
     &                                'furnace-type',
     &                                bFound ) )

        if ( .not. bFound ) then

          write (cMsg, '(A,A,I2,A,A)' )
     &      ' Error parsing data for ',
     &      'for HVAC system # ',
     &      iSystem,
     &      '. Furnace type not specified! Add "furnace-type" ',
     &      'token to HVAC file.'
          call edisp248( iUout, cMsg, 80 )

          bParseError = .true.

        elseif (    isys_type ( iSystem ) /= iFurn_AdvWoodStove
     &    .and. isys_type ( iSystem ) /= iFurn_CatConverter
     &    .and. isys_type ( iSystem ) /= iFurn_FlameRetenHead
     &    .and. isys_type ( iSystem ) /= iFurn_FlueVentDamper
     &    .and. isys_type ( iSystem ) /= iFurn_SparkIgn
     &    .and. isys_type ( iSystem ) /= iFurn_SparkIgnVentDamper
     &    .and. isys_type ( iSystem ) /= iFurn_ContPilot
     &    .and. isys_type ( iSystem ) /= iFurn_Condensing
     &    .and. isys_type ( iSystem ) /= iFurn_NonCondensingDirVent
     &    .and. isys_type ( iSystem ) /= iFurn_InducedDraft
     &    .and. isys_type ( iSystem ) /= iFurn_MidEfficiency
     &    .and. isys_type ( iSystem ) /= iFurn_ElecForcedAir
     &    .and. isys_type ( iSystem ) /= iFurn_Wood
     &    .and. isys_type ( iSystem ) /= iFurn_Oil    ) then

          write (cMsg, '(A,A,I2,A)' )
     &      ' Error interpreting furnace type (data item 1) ',
     &      'for HVAC system # ',
     &      iSystem,
     &      '. Value must be one of the following:'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 1  (advanced air tight wood stove)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 2  (catalytic converter)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 3  (flame retention head)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 4  (flue vent damper)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 5  (spark ignition)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 6  (spark ignition with flue vent damper)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 7  (continuious pilot) '
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 8  (condensing)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 9  (direct vent, non condensing)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 10 (induced draft)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 11 (mid-efficiency)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 12 (electric forced-air)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 13 (wood furnace)'
          call edisp248( iUout, cMsg, 80 )

          write (cMsg, '(A)')
     &      '   - 14 (oil furnace)'
          call edisp248( iUout, cMsg, 80 )

C.........Set error flag
          bParseError = .true.

        endif

C.......Steady-state capacity
!         ss_capacity(iSystem) = fDataBlock( iSystem, 2 )
        if ( ss_capacity(iSystem) <= 0 ) then

          write (cMsg, '(A,A,I2,A)' )
     &      ' Error interpreting furnace capacity (data item 2) ',
     &      'for HVAC system # ',
     &      iSystem,
     &      '. Value must be greater than zero.'
          call edisp248( iUout, cMsg, 80 )

C.........Set error flag
          bParseError = .true.

        endif

C.......Steady-state efficency
!         ss_efficiency(iSystem) = fDataBlock( iSystem, 3 )

        if ( ss_efficiency(iSystem) <= 0.0 .or.
     &       ss_efficiency(iSystem) >  1.0     ) then

          write (cMsg, '(A,A,I2,A,A)' )
     &      ' Error interpreting furnace efficiency (data item 3) ',
     &      'for HVAC system # ',
     &      iSystem,
     &      '. Value must be greater than zero and less than or ',
     &      ' equal to one.'
          call edisp248( iUout, cMsg, 80 )
C.........Set error flag
          bParseError = .true.

        endif

C.......Fan operation
!         iFan_Operation  ( iSystem )
!      &        = int ( fDataBlock( iSystem, 4 ) )

        if (    iFan_Operation( iSystem ) /= iFan_None
     &    .and. iFan_Operation( iSystem ) /= iFan_Auto
     &    .and. iFan_Operation( iSystem ) /= iFan_Cont
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


C.......Indoor fan power
!         fan_power(iSystem) = fDataBlock( iSystem, 5 )
        if ( fan_power(iSystem) < 0 ) then

          write (cMsg, '(A,A,I2,A)' )
     &      ' Error interpreting furnace fan power (data item 5) ',
     &      'for HVAC system # ',
     &      iSystem,
     &      '. Value must be equal-to or greater-than zero.'
          call edisp248( iUout, cMsg, 80 )
C.........Set error flag
          bParseError = .true.

        endif

C.......Draft fan power
!         draft_fan_power(iSystem) = fDataBlock( iSystem, 6 )
        if ( draft_fan_power(iSystem) < 0 ) then

          write (cMsg, '(A,A,I2,A)' )
     &      ' Error interpreting furnace draft fan power ',
     &      '(data item 6) for HVAC system # ',
     &      iSystem,
     &      '. Value must be equal-to or greater-than zero.'
          call edisp248( iUout, cMsg, 80 )
C.........Set error flag
          bParseError = .true.
        endif

C.......Pilot power
!         pilot_power(iSystem) = fDataBlock( iSystem, 7 )
        if ( pilot_power(iSystem) < 0 ) then

          write (cMsg, '(A,A,I2,A)' )
     &      ' Error interpreting furnace pilot power ',
     &      '(data item 7) for HVAC system # ',
     &      iSystem,
     &      '. Value must be equal-to or greater-than zero.'
          call edisp248( iUout, cMsg, 80 )
C.........Set error flag
          bParseError = .true.

        endif

      endif

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
C-----------------------------------------------------------------------
C     ESP-r commons
C-----------------------------------------------------------------------
      common/outin/iuout,iuin
      integer iuout, iuin      ! channels for writing messages to screen

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


      return
      end

C================== fHVAC_FindToken ====================================
C
C This routine searches for a string inside the array 'cTokens',
C and returns the corresponding value inside 'fTokenVals'
C
C
C Inputs:
C   -iSystem: Index of the system
C   -cToken:  String to search for
C
C Outputs:
C   -fHVAC_FindToken: float value from fTokenVals
C   -bFound: flag indicating if token was found.
C
C=======================================================================

      real function fHVAC_FindToken(iSystem, cToken, bFound)
      implicit none
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"

C-----------------------------------------------------------------------
C     Passed arguements.
C-----------------------------------------------------------------------
      integer iSystem                 ! System #
      character*(*) cToken            ! Search token
      logical bFound                  ! Result of search
C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      integer iData                   ! counter
      integer iStrLength              ! Length of search string

C-----------------------------------------------------------------------
C     References
C-----------------------------------------------------------------------
      integer lnblnk

C.....Initialize result flag
      bFound = .false.

C.....Get length of search-string
      iStrLength = lnblnk( cToken )


C-----------------------------------------------------------------------
C     Now loop through cTokens array, and search for cToken
C-----------------------------------------------------------------------
      do iData = 1, iDataCount ( iSystem )

C.......Do tokens match?
        if ( cToken(1:iStrLength) ==
     &        cTokens(iSystem,iData)(1:lnblnk(cTokens(iSystem,iData)))
     &       ) then
C.........Yes! Recover value
          fHVAC_FindToken = fTokenVals( iSystem, iData)

C.........Set 'found' result
          bFound = .true.

        else
C.........No! Do nothing

        endif

      enddo

      if ( .not. bFound ) fHVAC_FindToken = 0.0

      return
      end

