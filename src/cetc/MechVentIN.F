C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

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

      Subroutine MECH_VENT_READIN ( )
c
c
c Copyright 2001: NRCan Buildings Group
c Created August  8, 2001
c Edited: BB Aug 8, 14, 15, 17, Sep 18, 27, Oct 2, 3, 5, 19, Nov 2
c ---

c
      Implicit None
#include "building.h"
#include "Ventilation_Data.h"
c
c Declarations for local variables and variables passed in common blocks.
      character outs*124,OUTSTR*124
      integer iUnit, ISTAT, iErr, ND, k, iVal, i
      real val
      Integer LENGTH_OF_STRING
c
      Integer IUOUT, IUIN
      Common /OUTIN/ IUOUT, IUIN
c
      Integer IFIL
      Common /FILEP/ IFIL
c
      Integer iMech
      Character *72 LMech
      Common /mechvdef/ iMech, LMech
      
      save outs,OUTSTR, iUnit, ISTAT, iErr, ND, k, iVal, i
      save val
      
c ---
c All mechanical ventilation system input data initialized prior to data entry.
c
      Call MECH_VENT_INITIALIZE ( )
c ---
c Open input data file.
      iUnit = IFIL
      Call GET_NEXT_UNIT ( iUnit, iErr )
c
      If ( iErr .eq. 0 ) Then
         Call ERPFREE ( iUnit, ISTAT )
         Call EFOPSEQ ( iUnit, LMech, 1, iErr )
      Else ! Unable to assign unit #
        Call edisp (IUOUT,' Mech Vent: Cannot assign input unit#.')
      Endif
      k = LENGTH_OF_STRING (LMech)
c     Write(OUTSTR,'(a,a)') ' MechVentIN: start to read=',LMech(1:k)
c     Call WINMSG (OUTSTR)
c ---
      If ( iErr .eq. 0 ) Then
c Read central ventilation system type.
         Call STRIPC ( iUnit, OUTSTR, 0, ND, 1, ' Mech Vent data', iErr)
      Endif
c
      If ( iErr .eq. 0 ) Then
         k = 0
c --- 27-08-2014: Patrice Pinel: Changed  maximum value of iVal from 3 to 4 to allow for read in of CVS type 4 (ERV)
         Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 4, 'F',
     x ' central ventilation system type', IUOUT, iErr )
      Endif
c
      If ( iErr .eq. 0 ) Then
c
c --- Central ventilation system type DEFINED
         iCVSType = iVal
c ---
c
c --- 27-08-2014: Patrice Pinel: Changed if statement to allow for type 4 (ERV) input read-in
c        If ( iCVSType .eq. 2 ) Then ! HRV
         If ( iCVSType == 2 .or. iCVSType == 4) Then ! HRV or ERV
c
            Call STRIPC (iUnit, OUTSTR, 0, ND, 1,' HVI HSP test data',
     x                   iErr)
c
            If ( iErr .eq. 0 ) Then
c --- 27-08-2014: Patrice Pinel: Updated comments to reflect new HRV/ERV data inputs aligned with HVI testing standards
c --- HRV test data (temperature C, SRE(%) , power consumed watts)
c --- ERV test data (temperature C, SRE(%) , LRMT(frac), power consumed watts)
c Set 1: "high" temperature
               k = 0
               i = 1
               Call MSG_EGETWR ( OUTSTR, k, VAL, -99., 99., 'F',
     x ' "high" temperature', IUOUT, iErr)
               If ( iCVSType == 2 ) HRVTest(i,1) = VAL
               If ( iCVSType == 4 ) ERVTest(i,1) = VAL
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 99.9, 'F',
     x ' efficiency ', IUOUT, iErr)
               If ( iCVSType == 2 ) HRVTest(i,2) = VAL
               If ( iCVSType == 4 ) ERVTest(i,2) = VAL
            Endif
c
            If ( iCVSType == 2 ) Then ! HRV, read in power consumption
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' power consumption data', IUOUT, iErr)
                  HRVTest(i,3) = VAL
               Endif
            Else ! ERV, read in LRMT and power consumption
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 1., 'F',
     x ' LRMT data', IUOUT, iErr)
                  ERVTest(i,3) = VAL
               Endif
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' power consumption data', IUOUT, iErr)
                  ERVTest(i,4) = VAL
               Endif
            Endif			   
c --- Continue HVI test data (varies with model, type, rated flow rate
c
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' HVI Low T test data', iErr )
            Endif
c Set 2: "low" temperature
            If ( iErr .eq. 0 ) Then
               k = 0
               i = 2
               Call MSG_EGETWR ( OUTSTR, k, VAL, -99., 99., 'F',
     x ' "low" temperature', IUOUT, iErr)
               If ( iCVSType == 2 ) HRVTest(i,1) = VAL
               If ( iCVSType == 4 ) ERVTest(i,1) = VAL
			   
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 99.9, 'F',
     x ' SRE ', IUOUT, iErr)
               If ( iCVSType == 2 ) HRVTest(i,2) = VAL
               If ( iCVSType == 4 ) ERVTest(i,2) = VAL
            Endif
c
            If ( iCVSType == 2 ) Then ! HRV, read in power consumption
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' power consumption data', IUOUT, iErr)
                  HRVTest(i,3) = VAL
                Endif
            Else ! ERV, read in LRMT and power consumption
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 1., 'F',
     x ' LRMT data', IUOUT, iErr)
                  ERVTest(i,3) = VAL
               Endif
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' power consumption data', IUOUT, iErr)
                  ERVTest(i,4) = VAL
               Endif
            Endif
c
c ---  Central ventilation system supply flow rate (L/s)
c
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' HRV/ERV Supply/Exhaust flow rate', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' HRV/ERV Supply/Exhaust flow rate', IUOUT, iErr)
c
c HRV assumed to be BALANCED, supply = exhaust flow rate
               CVSSupplyRate = VAL
               CVSExhaustRate = VAL
            Endif
c           Write(OUTSTR,'(a,f8.1)')' MechVentIN: HRV supply rate=',VAL
c           Call WINMSG (OUTSTR)
c
c ---  HRV total cooling efficiency
c
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' HRV/ERV total cooling efficiency', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' CoolEF', IUOUT, iErr)
               CoolEF = VAL
            Endif
c
c --- Preheater capacity (watts)
c
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' HRV Preheater capacity', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 25000., 'F',
     x ' PreHtrCap', IUOUT, iErr)
               PreHtrCap = VAL
            Endif
c ---
c # CVS temperature control data (flag,low temperature trip, high temperature trip)
c 7   0.    0.  # flag 3 = Temperature controlled, or 1,2,4,5,6,7 = N/A
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' CVS temperature control data', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 7, 'F',
     x ' CVS temperature control flag', IUOUT, iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
c
               iCVSOpType = iVal
c
               If ( iCVSOpType .eq. 3 ) Then
c
                  Call MSG_EGETWR ( OUTSTR, k, VAL, -999., 999., 'F',
     x ' CVS-low temperature trip', IUOUT, iErr)
                  rCVSLowTempTrip = VAL
c
                  If ( iErr .eq. 0 ) Then
                     Call MSG_EGETWR ( OUTSTR, k, VAL, -999., 999., 'F',
     x ' CVS-high temperature trip', IUOUT, iErr)
                     rCVSHighTempTrip = VAL
                  Endif
               Endif
            Endif
c --- HRV/ERV duct data
c Location,Type,Sealing, Length(m), Diameter(mm),Insul.RSI
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' HRV/ERV supply duct data', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 4, 'F',
     x ' HRV/ERV supply duct location', IUOUT, iErr )
               IntDD(1,1) = iVal
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 3, 'F',
     x ' HRV/ERV supply duct type', IUOUT, iErr )
               IntDD(1,2) = iVal
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 3, 'F',
     x ' HRV/ERV supply duct sealing', IUOUT, iErr )
               IntDD(1,3) = iVal
            Endif
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' HRV/ERV supply duct length', IUOUT, iErr)
               RealDD(1,1) = VAL
            Endif
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 9999., 'F',
     x ' HRV/ERV supply duct diameter', IUOUT, iErr)
               RealDD(1,2) = VAL
            Endif
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' HRV/ERV supply duct Insul.', IUOUT, iErr)
               RealDD(1,3) = VAL
            Endif
c
c --- HRV/ERV Exhaust duct
c
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' HRV/ERV exhaust duct data', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 4, 'F',
     x ' HRV/ERV exhaust duct location', IUOUT, iErr )
               IntDD(2,1) = iVal
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 3, 'F',
     x ' HRV/ERV exhaust duct type', IUOUT, iErr )
               IntDD(2,2) = iVal
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 3, 'F',
     x ' HRV/ERV exhaust duct sealing', IUOUT, iErr )
               IntDD(2,3) = iVal
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' HRV/ERV exhaust duct length', IUOUT, iErr)
               RealDD(2,1) = VAL
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 9999., 'F',
     x ' HRV/ERV exhaust duct diameter', IUOUT, iErr)
               RealDD(2,2) = VAL
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' HRV/ERV exhaust duct Insul.', IUOUT, iErr)
               RealDD(2,3) = VAL
            Endif
c           Write(OUTSTR,'(a,f8.1,i3)')' MechVentIN: HRV ED insul=',VAL,
c    x iErr
c           Call WINMSG (OUTSTR)
c ---
         Else If ( iCVSType .eq. 3 ) Then ! Central Ventilation Fans (no H/R)
c
            Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' CVS Fans (no H/R)', iErr )
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' Fans (no H/R) supply', IUOUT, iErr)
               CVSSupplyRate = VAL
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 9999., 'F',
     x ' Fans (no H/R) exhaust', IUOUT, iErr)
               CVSExhaustRate = VAL
            Endif
c
            If ( iErr .eq. 0 ) Then
               Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' Fans (no H/R) Fan power', IUOUT, iErr)
               HRVTest(1,3) = VAL
            Endif
c
c # CVS temperature control data (flag,low temperature trip, high temperature trip)
c 7   0.    0.  # flag 3 = Temperature controlled, or 1,2,4,5,6,7 = N/A
            If ( iErr .eq. 0 ) Then
               Call STRIPC ( iUnit, OUTSTR, 0, ND, 1,
     x ' CVS temperature control data', iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
               k = 0
               Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 7, 'F',
     x ' CVS temperature control flag', IUOUT, iErr )
            Endif
c
            If ( iErr .eq. 0 ) Then
c
               iCVSOpType = iVal
c
               If ( iCVSOpType .eq. 3 ) Then
c
                  Call MSG_EGETWR ( OUTSTR, k, VAL, -999., 999., 'F',
     x ' CVS-low temperature trip', IUOUT, iErr)
                  rCVSLowTempTrip = VAL
c
                  If ( iErr .eq. 0 ) Then
                     Call MSG_EGETWR ( OUTSTR, k, VAL, -999., 999., 'F',
     x ' CVS-high temperature trip', IUOUT, iErr)
                     rCVSHighTempTrip = VAL
                  Endif
               Endif
            Endif
c ---
c        Else
c No central ventilation system (None)
         Endif
c
c --- Other fans
         If ( iErr .eq. 0 ) Then
            Call STRIPC ( iUnit, OUTSTR, 0, ND, 1, ' Other fans', iErr )
         Endif
c
         If ( iErr .eq. 0 ) Then
            k = 0
            Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 4, 'F',
     x ' Other fans type', IUOUT, iErr )
               iOtherFans = iVal
c
            If ( iErr .eq. 0 .and. iVal .eq. 2 ) Then
               If ( iErr .eq. 0 ) Then
                  Call STRIPC ( iUnit, OUTSTR, 0, ND, 1, ' Other fans',
     x  iErr )
               Endif
c
               If ( iErr .eq. 0 ) Then
                  k = 0
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' Other fans supply rate', IUOUT, iErr)
                  OtherFansSupplyRate = VAL
               Endif
c
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 9999., 'F',
     x ' Other fans exhaust rate', IUOUT, iErr)
                  OtherFansExhaustRate = VAL
               Endif
c
               If ( iErr .eq. 0 ) Then
                  Call MSG_EGETWR ( OUTSTR, k, VAL, 0., 999., 'F',
     x ' Other fans total power', IUOUT, iErr)
                  OtherFansTotalPower = VAL
               Endif
            Endif
         Endif
c
      Endif
c --- Moved to AIM2_READIN  BB
c     If ( iErr .eq. 0 ) Then
c Read zone number of the zone that is a crawl space.
c        Call STRIPC ( iUnit, OUTSTR, 0, ND, 1, ' Crawl Space zone #',
c    x iErr)
c     Endif
c
c     If ( iErr .eq. 0 ) Then
c        k = 0
c        Call MSG_EGETWI ( OUTSTR, k, iVal, 1, 3, 'F',
c    x ' Crawl Space zone #', IUOUT, iErr )
c        If ( iErr .eq. 0 ) iCrawl_Space_Zone = iVal
c     Endif
c ---
c Error handling.
      If ( iErr .ne. 0 ) Then
         iErr=1
         write(outs,7)
    7 Format (' Mechanical ventilation input file error.')
         call edisp ( IUOUT, outs )
c     Else
c        Write(OUTSTR,'(a,i5)')' MechVentIN: End Error code=', iErr
c        Call WINMSG (OUTSTR)
      Endif
c
c Close input data file.
c
 1001 Call ERPFREE(iUnit,ISTAT)
c
      RETURN
      END
c ===
      Subroutine MECH_VENT_INITIALIZE (  )
c
c Copyright 2001: NRCan Buildings Group
c Created August  8, 2001
c Edited: BB Aug 8, 15, 17, Oct 25
c
C      USE Ventilation_data
c
      IMPLICIT NONE
#include "building.h"
#include "Ventilation_Data.h"
      Integer i, j
      INTEGER ICRAWL_SPACE_ZONE
c ----The following include shall become redundant
c     Include 'Vnt_Data.Cmn'
c ---
c     Record / Vnt_Data / TheData
c ---
c All mechanical ventilation system input data initialized prior to data entry.
c
      iCVSType = 1 ! No central ventilation system
c
      Do i = 1, 2
         Do j = 1, 4
            If ( j .lt. 4 ) Then
               HRVTest(i,j) = 0.
               IntDD(i,j) = 0
            Endif
            RealDD(i,j) = 0.
         End Do
      End Do
c
      CoolEF = 0.
      CVSSupplyRate = 0.
      CVSExhaustRate = 0.
      PERV5= 0.
      MCP = 0.
      HCF = 0.
      MVAC = 0.
      PreHtrCap = 0.
      CVSOpTime = 1.
      SupplyDuctRoomTemp = 0.
      ExhaustDuctRoomTemp = 0.
      iCVSOpType = 7 ! N/A
      rCVSLowTempTrip = 0.
      rCVSHighTempTrip = 0.
      TMAIN = 0.
      SensibleEff = 0.
      HRVventHLR = 0.
      TotalPower = 0.
      ExhaustFanPower = 0.
      PreheaterPower = 0.
c --- 27-08-2014: Patrice Pinel: Changed  HRVSupDuctHG and  HRVExhDuctHG for CVSSupDuctHG and CVSExhDuctHG
      !HRVSupDuctHG = 0.
      !HRVExhDuctHG = 0.
      CVSSupDuctHG = 0.
      CVSExhDuctHG = 0.
      CVSSupplyTemp = 0.
      iOtherFans = 1
      OtherFansSupplyRate = 0.
      OtherFansExhaustRate = 0.
      OtherFansTotalPower = 0.
      iCrawl_Space_Zone = 0
c ---
      Return
      END
c
      Subroutine GET_NEXT_UNIT ( iU, iErr )
c
c  Starting from iU+1 and so on up to iU+10, try to find a unit
c  number that is available for I/O.
c
c Created August 1, 2001
c Edited: BB Aug 1
c ------------------------------------
c Parameters
      Integer iU    ! Fortran unit #
      Integer iErr  ! Error indicator
c                      0 = Unit iU (returned) is free to use for I/O.
c                     >0 = Not able to find a free unit.
c ------------------------------------
c
      Integer i, j, k
      Logical Yup
c
      iErr = 1 ! Assume failure
c
      k = iU
      Do i = k + 1, k + 10
         Inquire ( UNIT = i, Opened = Yup, IoStat = j )
         If ( .not. Yup ) Then
            iU = i
            iErr = 0 ! Success
            Exit
         Endif
      End Do
      Return
      END
c ---
      Subroutine MSG_EGETWI ( InputString, iSeq, iVal, iValMin,
     x iValMax, ChFlag, MsgString, iUout, iErr )
c
c  Same as esp_r procedure EGETWI, except that it inserts
c " Cannot read " to the start of the message string and calls
c  edisp when errors occur.
c
c Created August 1, 2001
c Edited: BB Aug 1
c ------------------------------------
c Parameters
      Character InputString*(*)  ! string from which data will be extracted
      Integer iSeq               ! var counter ?
      Integer iVal               ! integer value returned
      Integer iValMin            ! minimum value allowed
      Integer iValMax            ! maximum value allowed
      Character ChFlag*1         ! flag ?
      Character *(*) MsgString   ! definition of data to be read
      Integer iUout              ! unit # ?
      Integer iErr               ! Error indicator (if greater than 0)
c
c ------------------------------------
c
c Local variables
      Integer i
      Integer LENGTH_OF_STRING
      Character ErrorMessage*256
c ---
c
      Call EGETWI ( InputString, iSeq, iVal, iValMin, iValMax, ChFlag,
     x MsgString, iErr )
c
      If ( iErr .ne. 0 ) Then ! Complain on error
c
        i = LENGTH_OF_STRING ( MsgString )
c
        If ( i + 14 .gt. 256 ) i = 242 ! protect against string overflow
c
        ErrorMessage = ' Cannot read ' // MsgString(1:i) // '.'
        i = i + 14
c
        Call edisp ( iUout, ErrorMessage(1:i) )
c
      Endif
c ---
      Return
      END
c ---
      Subroutine MSG_EGETWR ( InputString, iSeq, rVal, rValMin,
     x rValMax, ChFlag, MsgString, iUout, iErr )
c
c  Same as esp_r procedure EGETWI, except that it inserts
c " Cannot read " to the start of the message string and calls
c  edisp when errors occur.
c
c Created August 1, 2001
c Edited: BB Aug 1, 6
c ------------------------------------
c Parameters
      Character InputString*(*)  ! string from which data will be extracted
      Integer iSeq               ! var counter ?
      Real    rVal               ! real value returned
      Real    rValMin            ! minimum value allowed
      Real    rValMax            ! maximum value allowed
      Character ChFlag*1         ! flag ?
      Character *(*) MsgString   ! definition of data to be read
      Integer iUout              ! unit # ?
      Integer iErr               ! Error indicator (if greater than 0)
c
c ------------------------------------
c
c Local variables
      Integer i
      Integer LENGTH_OF_STRING
      Character ErrorMessage*256
c ---
c
      Call EGETWR ( InputString, iSeq, rVal, rValMin,
     x rValMax, ChFlag, MsgString, iErr )
c
      If ( iErr .ne. 0 ) Then ! Complain on error
c
        i = LENGTH_OF_STRING ( MsgString )
c
        If ( i + 14 .gt. 256 ) i = 242 ! protect against string overflow
c
        ErrorMessage = ' Cannot read ' // MsgString(1:i) // '.'
        i = i + 14
c
        Call edisp ( iUout, ErrorMessage(1:i) )
c
      Endif
c ---
      Return
      END
c ---
      Integer Function LENGTH_OF_STRING ( String )
c
c  Return the length of the string, as defined by the last non-blank
c  character, excluding NULL's.
c
c Created August 1, 2001
c Edited: BB Aug 1
c ------------------------------------
c Parameters
      Character String*(*)  ! the string to be measured
c
c LENGTH_OF_STRING returns # of characters (or 1 if it is all blank)
c Note: this function is redundant if Len_Trim is available.
c
c ------------------------------------
c
c Local variables
      Integer i, j, k
      Character Ch1*1
c
      i = Len ( String )
      k = i
      Do j = i, 1, -1
         Ch1 = String(j:j)
         If ( Ch1 .ne. ' ' .and. Ch1 .ne. Char(0) ) Then
c Neither blank nor NULL, this is the end of the string.
            k = j
            Exit
         Endif
      End Do
c
      LENGTH_OF_STRING = k
c ---
      Return
      END
