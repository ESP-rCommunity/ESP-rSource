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

C h3k_utils.F
C
C This file contains various utility routines/functions that return information
C about the current simulation. Included:
C
C Set_bPltCon:    Function to set flag indicating if plant domain solution
C                 has converged
C
C bInStartup:     Function returning true if simulation is in startup period
C
C bCETC_OldStart: Obsolete implementation of bInStartup. Preserved for debugging
C                 purposes only
C
C TrackBuildingTimeSteps: Determines how many timesteps have lapsed in each
C                 month.
C
C h3k_month:      Returns the current month corresponding to the present day
C
C ISV_convert:    Breaks data contined in ESP-r's ISV common block array into
C                 managable pieces
C


C *************************************************************************************
C *****************************LOGICAL Function bInStartup ****************************
C Created by: Phylroy Lopez
C Initial Creation Date: Nov 9, 2001
C Copyright CETC 2001
C This subroutine returns  'true' if the simulation is still in start-up mode, or if
C the end of the simulation period has been reached. Under these circumstances, the
C results should not be written out to output files.
C
C INPUTS: NONE
C OUTPUTS: LOGICAL
C
C REVISIONS:
C - Code heavily revised to conform with ESRU's start-up period definitions:
C
C   1. The simulation starts at 0100 hours on the first day of the start-up period,
C      and first day is only 23 hours long.
C   2. The simulation continues for one hour after the end of the simulation period.
C
C   For more information, See Ferguson 2004, 'Testing CETC's XML reporting facilities',
C   CETC internal Report.
C
C     ( Alex Ferguson, May 04)
C -------------------------------------------------------------------------------------

      LOGICAL Function bInStartup()
      IMPLICIT NONE
#include "building.h"
#include "MultiYear_simulations.h"

C iss simulation start day  (1->365) as derived in bmatsv.F
C isf  simulation end day  (1->365)
      common/simsdy/iss,isf
      integer iss, isf

C itcnst: Number of start-up period days
      common/prec7/itcnst
      integer itcnst

C ihrp present hour (1->24)
C ihrf furure hour  (1->24)
C idyp present day  (1->365)
C idyf future day   (1->365)
C idwp flag for weekday/weekend (present)
C idwf flag for weekday/weekend (future)
C nsinc timestep counter
C its timestep counter (for current hour)
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp, ihrf, idyp, idyf, idwp, idwf, nsinc, its

C isd1,ism1,isd2,ism2 are not used in current context
C isds simulation reporting period start day (1->365)
C isdf simulation reporting period end day (1->365)
C ntstep number of steps/hour
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1,ism1,isd2,ism2,isds,isdf,ntstep


C     Local variables
      integer iDay_current_TS   ! Present day for current timestep
      integer iDay_startup_end  ! end of start-up period
      logical bStarting_up      ! saved flag indicating startup period is active
      logical bSim_done         ! saved flag indicating simulation is complete
      logical debug             ! flag for debugging

      logical bIncremented      ! flag indicating if day timestep has not
                                ! been incremented

      save bStarting_up, bSim_done, iDay_current_TS,iDay_startup_end,
     &     bIncremented


C     CETC's old startup function
      LOGICAL  bCETC_OldStart

C----------------------------------------------------------------------
C     If this is the first timestep, initialize bStartup
C----------------------------------------------------------------------
      if (nsinc .eq. 1) then

         bStarting_up  = .true.
         bSim_done = .false.
C----------------------------------------------------------------------
C     Get current day
C----------------------------------------------------------------------
         iDay_current_TS = idyp
C----------------------------------------------------------------------
C     Assign start-up period end day:
C
C        end day = start day + startup period duration
C
C----------------------------------------------------------------------
         iDay_Startup_end  = iss + itcnst

C----------------------------------------------------------------------
C     Flag indicating that there is no need to increment
C     current day variable
C----------------------------------------------------------------------
         bIncremented = .true.
      endif

      if (.not. bSim_done ) then
C----------------------------------------------------------------------
C     Determine present day:
C----------------------------------------------------------------------
         if ( ( ihrp  .eq. 24 ) .and.
     &        ( its   .eq. 1  ) .and.
     &        ( .not. bIncremented ) ) then
C----------------------------------------------------------------------
C     Currrent timestep corresponds to midnight of
C     next day - increment current day
C----------------------------------------------------------------------
            iDay_current_TS = iDay_current_TS + 1
C----------------------------------------------------------------------
C     set flag to prevent day from being incremented
C     on successive calls within present timestep
C----------------------------------------------------------------------
            bIncremented = .true.
         elseif ( ( ihrp  .eq. 1 ) .and.
     &            ( its   .eq. 1  ) ) then
            bIncremented = .false.
         endif

         if ( bStarting_up ) then
C----------------------------------------------------------------------
C     Determine if simulation is in start-up period.
C----------------------------------------------------------------------
            if (  iDay_Current_TS .GE. iDay_startup_end ) then
               bStarting_up = .false.
            else
               bStarting_up = .true.
            endif
         else
C----------------------------------------------------------------------
C     Startup period is over
C----------------------------------------------------------------------
            bStarting_up = .false.
         endif
C----------------------------------------------------------------------
C     Check if end of simulation has been reached.
C     -> IF start-up period is greater or equal to
C        a day, the last hour of the simulation
C        (corresponding to the first hour of the
C        day after the simulation end day) is not
C        reported.
C----------------------------------------------------------------------
         if ( (.not. bStarting_up)
     &        .and. (iDay_Current_TS .gt. isf)) then
            bSim_Done = .true.
         endif

      endif
C----------------------------------------------------------------------
C     Set function result - don't output results
C     if in start-up period or simulation is complete
C----------------------------------------------------------------------
      if ( bStarting_up .or. bSim_done ) then
         bInStartup = .true.
      else
         bInStartup = .false.
      endif

C-----------------------------------------------------------------
C     To invoke the old (incorrect) inplementation, uncomment the
C     following line
C-----------------------------------------------------------------
C      bInStartup = bCETC_OldStart()

      RETURN
      END

C *************************************************************************************
C *****************************LOGICAL Function bCETC_OldStart **************************
C Created by: Phylroy Lopez
C Initial Creation Date: Nov 9, 2001
C Copyright CETC 2001
C This subroutine returns  'true' if the
C simulation is still in start-up mode.
C INPUTS: NONE
C OUTPUTS: LOGICAL
C
C This function is obsolete, and is included for debugging purposes only.
C -------------------------------------------------------------------------------------

        LOGICAL Function bCETC_OldStart()
        IMPLICIT NONE
	common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
	COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
	common/sus/issny
        common/ptime/ptimep,ptimef
        REAL ptimep,ptimef
        INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
	INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
	INTEGER ISSNY
c Check if in startup period.
        bCETC_OldStart=.true.
        IF((ISSNY.EQ.1.AND.IDYP.EQ.365.AND.IHRP.EQ.24.AND.ISDS.EQ.1)
     &    .OR.(ISSNY.EQ.0.AND.(IDYP.GE.ISDS.OR.
     &        (IDYP.EQ.ISDS-1.AND.IHRP.EQ.24))))bCETC_OldStart =.false.
c If in startup period.
        RETURN
        END

C *************************************************************************************
C *****************************SUBROUTINE TrackBuildingTimeSteps **********************
C Created by: Phylroy Lopez
C Initial Creation Date: Nov 9, 2001
C Copyright CETC 2001
C This subroutine keeps track of the number of plant timesteps the simulator encounter on a
c monthly basis
C INPUTS: NONE
C OUTPUTS:NONE
C -------------------------------------------------------------------------------------

        SUBROUTINE TrackBuildingTimeSteps()
        IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
	common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
        common/bldconpp/iBLTS
        INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
        INTEGER IDAYN,iMonth,iBLTS
	logical bInStartUp, bTimeHasTimstep
        IF(iBLTS == nsinc) then
          bTimeHasTimstep = .false.    !Timestep has not changed
        ELSE
c Timestep had changed. Return true and reset LastTSTP to current step
           bTimeHasTimstep = .true.
           iBLTS = nsinc
        ENDIF
        if (.not.bINStartUp() .and.bTimeHasTimstep ) then
           CALL EDAYR(idyp,IDAYN,iMonth)
           iNumberOfBuildingTimeSteps(iMonth) =
     &                    iNumberOfBuildingTimeSteps(iMonth) + 1
        ENDIF
        RETURN
        END


C *************************************************************************************
C *****************************SUBROUTINE TrackPlantTimeSteps **********************
C Created by: Phylroy Lopez
C Initial Creation Date: Nov 9, 2001
C Copyright CETC 2001
C This subroutine keeps track of the number of building timesteps the simulator encounters on a
c monthly basis
C INPUTS: NONE
C OUTPUTS:NONE
C -------------------------------------------------------------------------------------


        SUBROUTINE TrackPlantTimeSteps()
         IMPLICIT NONE

#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
	common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
        common/pltcon/bPltCon,LastTSTP
        Real LastTSTP
        Logical bPltCon      !Is it really a new timestep?
        INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,i1
        INTEGER IDAYN,iMonth
	logical bInStartUp   !Are we in Startup?
        if (.not.bINStartUp() .and.bPltCon ) then
c Get month of year and bin the timestep.
           CALL EDAYR(idyp,IDAYN,iMonth)
         iNumberOfPlantTimeSteps(iMonth) =
     &                            iNumberOfPlantTimeSteps(iMonth) + 1
         i1 = iNumberOfPlantTimeSteps(iMonth)
        ENDIF
        RETURN
        END

C *************************************************************************************
C ***************************** recip **********************
C Created by: Phylroy Lopez
C Initial Creation Date: Nov 9, 2001
C Copyright CETC 2001
C Safe divider
C INPUTS: NONE
C OUTPUTS:NONE
C -------------------------------------------------------------------------------------


        FUNCTION recip(fNumber)
        IMPLICIT NONE
        Real fNumber
        REAL recip
        IF (fNumber.eq.0.0)then
           recip = 0.0
           Call DivideByZeroWarning
        ELSE
           recip = 1.0 / fNumber
        ENDIF
        RETURN
        END


        SUBROUTINE DivideByZeroWarning
        IMPLICIT NONE
        CHARACTER*124 outs

        WRITE (OUTS,'(A)')'Division By Zero Warning!
     &      Debug with stop at DivideByZeroWarning function'
        CALL EDISP(0,OUTS)
        RETURN
        END


C ***************************** h3k_month *********************************************
C Created by: Alex ferguson
C Initial Creation Date: June 4th, 2004
C Copyright CETC 2004
C
C Returns the index of the month corresponding to the current day
C
C INPUTS: day (real, 1.0->365.999...)
C OUTPUTS:month index (integer, 1->12)
C -------------------------------------------------------------------------------------
      function h3k_month(day)

      implicit none

      real day                  ! current day
      integer h3k_month         ! current month

      if ( day .lt. 32.0 ) then
         h3k_month = 1          ! january
      elseif ( day .lt. 60.0 ) then
         h3k_month = 2          ! february
      elseif ( day .lt. 91.0 ) then
         h3k_month = 3          ! march
      elseif ( day .lt. 121.0) then
         h3k_month = 4          ! april
      elseif ( day .lt. 152.0) then
         h3k_month = 5          ! may
      elseif ( day .lt. 182.0) then
         h3k_month = 6          ! june
      elseif ( day .lt. 213.0) then
         h3k_month = 7          ! july
      elseif ( day .lt. 244.0) then
         h3k_month = 8          ! august
      elseif ( day .lt. 274.0) then
         h3k_month = 9          ! september
      elseif ( day .lt. 305.0) then
         h3k_month = 10         ! october
      elseif ( day .lt. 335.0) then
         h3k_month = 11         ! november
      else
         h3k_month = 12         ! december
      endif

      return
      end


C---------------------------  ISV_convert   -------------------------------
C     Created by: Alex Ferguson
C     Creation date: June 2003
C     Copyright: CETC 2003
C--------------------------------------------------------------------------
C     REVISIONS:
C
C--------------------------------------------------------------------------
C     ABSTRACT:
C
C     This function takes an ESP-r ISV value and returns two integer values
C     (state, SIMtype) that describe (a) the type of material the node
C     represents (water/air/solid) and (b) the type of simulation that the
C     node may be used in.
C
C     ESP-r uses the ISV common variable to hold two data:
C       - the type of material the node represents, and
C       - the simulation type(s) supported by the node
C     These information are described by the value of the ISV variable
C
C              ISV    =  0,10,20 node represents water + ....
C                     =  1,11,21 node represents dry air + ....
C                     =  9,19,29 node represents some solid material only
C                     =   0 - 9  model suitable for energy balance only
C                     =  10- 19  model suitable for energy
C                                + single phase mass balance
C                     =  20 - 19 model suitable for energy
C                                + two phase mass balances
C
C     This approach represents a two dimensional matrix of options using
C     a single variable. It is cumbersome, since it requires multiple tests
C     to percisely determine the matrix. Furthermore, if ESP-r is one day
C     extended to permit alternative mediums (such as refridgerent or
C     propylene glycol), signigicant portions of the code will require
C     re-writing to account for additional ranks in the ISV matrix.
C
C     Therefore, this simple function was written to break the ISV
C     data into two separate variables:
C         - The first digit (typically 0, 1 or 2) of ISV is returned in a
C           variable called SIMtype, which describes the types of simulations
C           that can be performed using the node, and
C         - the second digit (typically 0, 1, or 9) of ISV is returned in a
C           variable called state, which describes the type of matter
C           (water/air/solid) that the node represents
C
C     This function will accept any ISV value between 0 and 99, which permits
C     seven additional fluid mediums to be defined in ESP-r, and 7 additional
C     simulation types.
C
C-------------------------------------------------------------------------------
      SUBROUTINE ISV_convert(ISV,state,SIMtype)
      IMPLICIT NONE

C-------------------------------------------------------------------------------
C     Declare local variables
C-------------------------------------------------------------------------------
      INTEGER i                 ! counter
      INTEGER ISV               ! ISV variable passed from ESP-r - SCALAR only!
      INTEGER state             ! type of material node represents
      INTEGER SIMtype           ! type of material node represents

      DO i = 0,9
         IF ( ((ISV - (i*10))<10).AND.
     &        ((ISV - (i*10))>=0) ) THEN
            SIMtype = i
            state = ISV-(i*10)

         ENDIF
      ENDDO
      RETURN
      END


C----------------- bValue_GE_Min --------------------------------------
C
C This is a very simple function that checks to see if a number is
C not less than a minimum value, and issues an warning/error message
C if the value is out of range.
C
C Inputs
C  - fValue: value to be checked
C  - fMin: Range minimum value
C  - cContext: Contextual message for the error/warning
C  - cMsg: actual warning message
C  - iComponent: index of plant component generating error
C  - iInput: index of input generating error
C  - iAction: action to be taken (error / warn / reset)
C
C Outputs:
C
C  - bValue_GE_Min: Flag indicating if input is in range.
C
C----------------------------------------------------------------------
      logical function bValue_GE_Min (
     &   fValue,
     &   fMin,
     &   cContext,
     &   cMsg,
     &   iComponent,
     &   iInput,
     &   iAction
     &    )
      implicit none
#include "building.h"

C External functions.
      integer lnblnk

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      real fValue
      real fMin

      character*128 cContext
      character*128 cMsg

      integer iComponent
      integer iInput

      integer iAction

C----------------------------------------------------------------------
C     Named constants & local variables
C----------------------------------------------------------------------
      integer iWarning
      integer iFatal
      integer iReset

      parameter (iWarning=1, iReset=2, iFatal=3)

      character*128 cCharTemp
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu

C-----------------------------------------------------------------------
C     Check bounds
C-----------------------------------------------------------------------
      if ( fValue .ge. fMin ) then

         bValue_GE_Min = .true.

      else

         bValue_GE_Min = .false.

         if ( iAction .eq. iFatal ) then
            cCharTemp = ': FATAL ERROR!'
         else
            cCharTemp = ': warning '
         endif

         write (itu,'(A)') ' '

         write (itu,'(A,A,A)') ' ',cContext(1:lnblnk(cContext)),
     &                       cCharTemp(1:lnblnk(cCharTemp))
         write (itu,'(A,A)') ' - ',cMsg(1:lnblnk(cMsg))
         write (itu,'(A,E10.3,A,E10.3,A)') ' - Value (', fValue,
     &                  ') cannot be less than min (', fMin,') '
         write (itu,'(A,I2,A,I2)') ' - Check input #', iInput,
     &                  ' in component #', iComponent

         if ( iAction .eq. iReset ) then
            write (itu,'(A,E10.3,A)')
     &            ' - Value reset to minimum (', fMin,'). '

            fValue = fMin

         endif

         write (itu,'(A)') ' '

      endif


      return
      end

C----------------- bValue_GT_Min --------------------------------------
C
C This is a very simple function that checks to see if a number is
C greater than a minimum value, and issues an warning/error message
C if the value is out of range.
C
C Inputs
C  - fValue: value to be checked
C  - fMin: Range minimum value
C  - cContext: Contextual message for the error/warning
C  - cMsg: actual warning message
C  - iComponent: index of plant component generating error
C  - iInput: index of input generating error
C  - iAction: action to be taken (error / warn / reset)
C
C Outputs:
C
C  - bValue_GE_Min: Flag indicating if input is in range.
C
C----------------------------------------------------------------------
      logical function bValue_GT_Min (
     &   fValue,
     &   fMin,
     &   cContext,
     &   cMsg,
     &   iComponent,
     &   iInput,
     &   iAction
     &    )
      implicit none
#include "building.h"

C External functions.
      integer lnblnk

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      real fValue
      real fMin

      character*128 cContext
      character*128 cMsg

      integer iComponent
      integer iInput

      integer iAction

C----------------------------------------------------------------------
C     Named constants & local variables
C----------------------------------------------------------------------
      integer iWarning
      integer iFatal
      integer iReset

      parameter (iWarning=1, iReset=2, iFatal=3)

      character*128 cCharTemp
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu

C-----------------------------------------------------------------------
C     Check bounds
C-----------------------------------------------------------------------
      if ( fValue .gt. fMin ) then

         bValue_GT_Min = .true.

      else

         bValue_GT_Min = .false.

         if ( iAction .eq. iFatal ) then
            cCharTemp = ': FATAL ERROR!'
         else
            cCharTemp = ': warning '
         endif

         write (itu,'(A)') ' '

         write (itu,'(A,A,A)')' ',cContext(1:lnblnk(cContext)),
     &                       cCharTemp(1:lnblnk(cCharTemp))
         write (itu,'(A,A)') ' - ',cMsg(1:lnblnk(cMsg))
         write (itu,'(A,E10.3,A,E10.3,A)') ' - Value (', fValue,
     &                  ') must be greater than min (', fMin,') '
         write (itu,'(A,I2,A,I2)') ' - Check input #', iInput,
     &                  ' in component #', iComponent

         if ( iAction .eq. iReset ) then
            write (itu,'(A,E10.3,A)')
     &            ' - Value reset to minimum (', fMin+0.01,'). '

            fValue = fMin+0.01

         endif
         write (itu,'(A)') ' '

      endif

      return
      end

C----------------- bValue_LE_Max --------------------------------------
C
C This is a very simple function that checks to see if a number is
C not greater than a maximum value, and issues an warning/error message
C if the value is out of range.
C
C Inputs
C  - fValue: value to be checked
C  - fMin: Range minimum value
C  - cContext: Contextual message for the error/warning
C  - cMsg: actual warning message
C  - iComponent: index of plant component generating error
C  - iInput: index of input generating error
C  - iAction: action to be taken (error / warn / reset)
C
C Outputs:
C
C  - bValue_GE_Min: Flag indicating if input is in range.
C
C----------------------------------------------------------------------
      logical function bValue_LE_Max (
     &   fValue,
     &   fMax,
     &   cContext,
     &   cMsg,
     &   iComponent,
     &   iInput,
     &   iAction
     &    )
      implicit none
#include "building.h"

C External functions.
      integer lnblnk

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      real fValue
      real fMax

      character*128 cContext
      character*128 cMsg

      integer iComponent
      integer iInput

      integer iAction

C----------------------------------------------------------------------
C     Named constants & local variables
C----------------------------------------------------------------------
      integer iWarning
      integer iFatal
      integer iReset

      parameter (iWarning=1, iReset=2, iFatal=3)

      character*128 cCharTemp
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu

C-----------------------------------------------------------------------
C     Check bounds
C-----------------------------------------------------------------------
      if ( fValue .le. fMax ) then

         bValue_LE_Max = .true.

      else

         bValue_LE_Max = .false.

         if ( iAction .eq. iFatal ) then
            cCharTemp = ': FATAL ERROR!'
         else
            cCharTemp = ': warning '
         endif

         write (itu,'(A)') ' '

         write (itu,'(A,A,A)') ' ',cContext(1:lnblnk(cContext)),
     &                       cCharTemp(1:lnblnk(cCharTemp))
         write (itu,'(A,A)') ' - ',cMsg(1:lnblnk(cMsg))
         write (itu,'(A,E10.3,A,E10.3,A)') ' - Value (', fValue,
     &                  ') cannot be greater than max (', fMax,') '
         write (itu,'(A,I2,A,I2)') ' - Check input #', iInput,
     &                  ' in component #', iComponent

         if ( iAction .eq. iReset ) then
            write (itu,'(A,E10.3,A)')
     &            ' - Value reset to maximuim (', fMax,'). '

            fValue = fMax

         endif
         write (itu,'(A)') ' '

      endif

      return
      end

C----------------- bValue_LT_Max --------------------------------------
C
C This is a very simple function that checks to see if a number is
C less than a minimum value, and issues an warning/error message
C if the value is out of range.
C
C Inputs
C  - fValue: value to be checked
C  - fMin: Range minimum value
C  - cContext: Contextual message for the error/warning
C  - cMsg: actual warning message
C  - iComponent: index of plant component generating error
C  - iInput: index of input generating error
C  - iAction: action to be taken (error / warn / reset)
C
C Outputs:
C
C  - bValue_GE_Min: Flag indicating if input is in range.
C
C----------------------------------------------------------------------
      logical function bValue_LT_Max (
     &   fValue,
     &   fMax,
     &   cContext,
     &   cMsg,
     &   iComponent,
     &   iInput,
     &   iAction
     &    )
      implicit none
#include "building.h"

C External function.
      integer lnblnk

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      real fValue
      real fMax

      character*128 cContext
      character*128 cMsg

      integer iComponent
      integer iInput

      integer iAction

C----------------------------------------------------------------------
C     Named constants & local variables
C----------------------------------------------------------------------
      integer iWarning
      integer iFatal
      integer iReset

      parameter (iWarning=1, iReset=2, iFatal=3)

      character*128 cCharTemp
C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu

C-----------------------------------------------------------------------
C     Check bounds
C-----------------------------------------------------------------------
      if ( fValue .lt. fMax ) then

         bValue_LT_Max = .true.

      else

         bValue_LT_Max = .false.

         if ( iAction .eq. iFatal ) then
            cCharTemp = ': FATAL ERROR!'
         else
            cCharTemp = ': warning '
         endif

         write (itu,'(A)') ' '

         write (itu,'(A,A,A)') ' ', cContext(1:lnblnk(cContext)),
     &                       cCharTemp(1:lnblnk(cCharTemp))
         write (itu,'(A,A)') ' - ',cMsg(1:lnblnk(cMsg))
         write (itu,'(A,E10.3,A,E10.3,A)') ' - Value (', fValue,
     &                  ') must be less than max (', fMax,') '
         write (itu,'(A,I2,A,I2)') ' - Check input #', iInput,
     &                  ' in component #', iComponent

         if ( iAction .eq. iReset ) then
            write (itu,'(A,E10.3,A)')
     &            ' - Value reset to maximum (', fMax-0.01,'). '

            fValue = fMax-0.01

         endif
         write (itu,'(A)') ' '
      endif

      return
      end



C---------------- fConvert_current_TS_to_DAY ----------------------------
C
C  ABSTRACT:
C
C  This is a short-hand function that collects the current time step
C  variables from esp-r commons, and calls fConvert_TS_to_DAY to
C  convert these data to a real number representing the current time.
C
C  INPUTS:
C
C    - None
C
C  Outputs:
C
C    - Current day in plant domain
C
C----------------------------------------------------------------------

      real function fConvert_current_TS_to_DAY ()
      implicit none

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Simulation timestep
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp              ! not used in current context
      integer ihrf              ! not used in current context
      integer idyp              ! present day #
      integer idyf              ! not used in current context
      integer idwp              ! not used in current context
      integer idwf              ! not used in current context
      integer nsinc             ! time step number
      integer its               ! not used in current context
      common/pctinc/ipts        ! Plt timestep number
      integer ipts
C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real fConvert_TS_to_DAY


      fConvert_current_TS_to_DAY = fConvert_TS_to_DAY( nsinc, ipts )
      return
      end

C---------------- fConvert_bld_TS_to_DAY ---------------------
C
C  ABSTRACT:
C
C  This is a short-hand function that collects the current building
C  time step variables from esp-r commons, and calls
C  fConvert_TS_to_DAY to convert this data to a real number
C  representing the current building time
C
C  INPUTS:
C
C    - None
C
C  Outputs:
C
C    - Current day in building domain
C
C----------------------------------------------------------------------

      real function fConvert_bld_TS_to_DAY ()
      implicit none

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Simulation timestep
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp              ! not used in current context
      integer ihrf              ! not used in current context
      integer idyp              ! present day #
      integer idyf              ! not used in current context
      integer idwp              ! not used in current context
      integer idwf              ! not used in current context
      integer nsinc             ! time step number
      integer its               ! not used in current context

C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------
      real fConvert_TS_to_DAY

      fConvert_bld_TS_to_DAY = fConvert_TS_to_DAY( nsinc, 1 )
      return
      end


C---------------- fConvert_TS_to_DAY ----------------------------------
C
C  ABSTRACT:
C
C  This function converts the current timestep into a real number
C  represeting the current day, and fraction thereof.
C
C  INPUTS:
C
C   iTS_bld - Building timestep #
C   iTS_plt - Plt timestep #
C
C  Outputs:
C
C    - Corresponding day
C
C----------------------------------------------------------------------

      real function fConvert_TS_to_DAY ( iTS_bld, iTS_plt )
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"

C----------------------------------------------------------------------
C     Passed arguements
C----------------------------------------------------------------------
      integer iTS_bld  ! building timestep
      integer iTS_plt  ! plant timestep

C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
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

      common/simsdy/iSS,iSF
      integer iSS            ! simulation start day
      integer iSF            ! simulation end day

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
C.....Hours skipped by ESP-r at the start of the simulation
      real fMissing_hrs
      parameter (fMissing_hrs = 1.0)
      integer localntstpp   ! local varable for ntstpp

C----------------------------------------------------------------------
C     Convert TS to day:
C
C     Day = start day
C
C           bld Timesteps lapsed + timesteps skipped by esp-r
C         + -------------------------------------------------
C                      bld timesteps / day
C
C         + Additional timesteps in the plt domain
C           --------------------------------------
C                      plt timesteps / day
C
C
C
C----------------------------------------------------------------------
C If ntstpp is zero then there will be a divide
C by zero. In this case ensure that localntstpp is 1.
       localntstpp = ntstpp
       if (localntstpp.eq.0) localntstpp = 1

       fConvert_TS_to_DAY =
     &
     &    float(iss)
     &
     &  + ( float(iTS_bld) + fMissing_hrs * float(ntstep))
     &  / ( float(ntstep) * 24.0 )
     &
     &  + float( iTS_plt - 1 ) / ( float ( ntstep * localntstpp )*24.0)

      return
      end
