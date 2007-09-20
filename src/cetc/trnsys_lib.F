
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


C This file contains functions and subroutines used to couple
C TRNSYS types with esp-r. Generally, these functions or 
C subroutines are provided here for one of the following two
C reasons:
C 1). A function supported by the fortran compiler of trnsys
C    but it is not supported by the GNU fortran 77, currently
C    used by esp-r.
C 2). A function or subroutine is available in trnsys kernel.
C    A similar function or sub needs to be rewritten in the 
C    context of esp-r.
C
C The functions written due to the first reason might need to
C be removed if the fortran compiler used by esp-r is updated.
C 
C The functions or subs written due to the second reason may 
C be more simple and concise than the corresponding trnsys
C functions or subs. Their purpose is to make the coupling
C works while avoid too much rippling effect.

C The functions and subroutines is NOT complete for all trnsys
C types. More functions and subroutines are expected if
C new trnsys types are coupled.
C
C This file contains the following functions:
C JFIX
C TRIM
C ADJUSTL
C getSimulationStartTime
C getSimulationTimeStep
C getListingFileLogicalUnit
C ErrorFound
C
C This file contains the following subroutines:
C Messages
C TYPECK







C Rounds a real value to its nearest integer
C Function JFIX() is used in trnsys, but it is not supported 
C by the GNU Fortran 77 compiler.
C Rewrite it via the intrinsic function NINT()
      INTEGER FUNCTION JFIX(x)
      IMPLICIT NONE

      DOUBLE PRECISION x
      JFIX = NINT(x)
      RETURN
      END FUNCTION JFIX



C Remove trailing blank characters
C TRIM is used frequently in trnsys. However, it is not implemented
C yet in GNU fortran 77 although it is reserved as an intrinsic.
C Note that the implementation of TRIM cannot play the
C intended role where it appear in trnsys. This is mainly because of
C the difference between intrinsic and external character functions.
      CHARACTER(LEN=*) FUNCTION TRIM(string)
      IMPLICIT NONE

C local variables
      CHARACTER(LEN=*) string
C Reference
      INTEGER  lnblnk

      TRIM = string(1:lnblnk(string))
      RETURN
      END FUNCTION TRIM



C Adjust string left
C ADJUSTL is used frequently in trnsys. However, it is not implemented
C yet in GNU fortran 77 although it is reserved as an intrinsic.
C Note that the implementation of ADJUSTL cannot play the
C intended role where it appear in trnsys. This is mainly because of
C the difference between intrinsic and external character functions.
      CHARACTER(LEN=*) FUNCTION ADJUSTL(string)
      IMPLICIT NONE

      CHARACTER(LEN=*) string
      INTEGER          lengthSou, lengthDes
      INTEGER          i, nonspace, ival

      lengthSou = len(string)
      lengthDes = len(ADJUSTL)

      DO i=1, lengthSou
         IF(string(i:i) .NE. ' ') THEN
            nonspace = i
            EXIT
         END IF
      END DO

      IF(nonspace .LE. lengthDes) THEN
         ival = MIN(lengthSou, lengthDes)
         ADJUSTL(1:ival) = string(nonspace:ival)
      END IF

      END FUNCTION ADJUSTL



C Originally a utility function in trnsys
C Return the hour of the year at which the simulation begins.
      REAL FUNCTION getSimulationStartTime()
      IMPLICIT NONE

C Common block for simulation start day and finish day
      COMMON/simsdy/iSS,iSF
      INTEGER iSS, iSF

C External function
      REAL fConvert_TS_to_DAY

C  esp-r starts at Hour 1, so 1 is added.
C To avoid potential numerical problems (real values compared in 
C quite different dimensions), iss is deducted.
C
C      getSimulationStartTime = iSS * 24. + 1.
      getSimulationStartTime = (fConvert_TS_to_DAY(1,1) - iSS) * 24.


      END FUNCTION getSimulationStartTime




C Originally a utility function in trnsys
C Return the simulation time step in hours
      REAL FUNCTION getSimulationTimeStep()
      IMPLICIT NONE

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

C TIMSEC is in second in esp-r
C Simulation time step is in hour in trnsys
C So, make conversion below:
      getSimulationTimeStep = TIMSEC/3600.

      END FUNCTION getSimulationTimeStep




C Originally a utility function in trnsys
C Return the logical unit of the trnsys list file
      INTEGER FUNCTION getListingFileLogicalUnit()
      IMPLICIT NONE

C ifil: basic file unit number
      COMMON/filep/ifil
      INTEGER ifil

C Since this function is usually used in trnsys for debugging purpose,
C use any temporarily opened file for the coupling project.
C
      getListingFileLogicalUnit = ifil + 1

      END FUNCTION getListingFileLogicalUnit




C Originally a utility function in trnsys
C Return 'TRUE' if trnsys errors have been found
      LOGICAL FUNCTION ErrorFound()
      IMPLICIT NONE

      COMMON/TRNERR/iWARN, iERROR
      INTEGER iWARN, iERROR

      IF(iERROR .gt. 0) THEN
         ErrorFound = .true.
      ELSE
         ErrorFound = .false.
      END IF

      END FUNCTION ErrorFound




C Originally a subroutine in trnsys
C prints TRNSYS messages
C Rewrite for esp-r
      SUBROUTINE Messages(errorCode,message,severity,unitNo,typeNo)
      IMPLICIT NONE
#include "trnsys.h"
C========================
C initialization to be done somewhere
C========================
      COMMON/TRNERR/iWARN, iERROR
      INTEGER iWARN, iERROR


C  Transferred parameters
      CHARACTER (len=*) message, severity
      INTEGER           errorCode, unitNo, typeNo

C Local variables
      CHARACTER(len=1)                 level
      CHARACTER(len=124)               msg1
      CHARACTER (len=maxMessageLength) msg2
      CHARACTER(len=30)                word

C Reference
      INTEGER  lnblnk

      WRITE(msg2,'(A,I3)') " Generated by Unit or Component : ",unitNo
      WRITE(word, '(A,I3)') " TRNSYS type : ", typeNo

      msg1 = msg2(1: lnblnk(msg2)) // word


      IF( errorCode.GT.1 .AND. errorCode.LT.nMaxErrorMessages) THEN
         WRITE(msg2,'(A,I3)') "TRNSYS standard message ", errorCode
      ELSE IF(errorCode .EQ. -1000 .OR. errorCode .EQ. -500 .OR.
     &        errorCode .EQ. -400) THEN
C-1000:Main TRNSYS header
C-500: End of the simulation (without errors)
C-400: Simulation stopped with errors in TRNSYS main routine
C The above three cases does not occur within a trnsys type
C so, they cannot happen for the project of coupling trnsys
         iERROR = iERROR + 1
         level = 'F'
         msg2 = "Crazy error code returned from trnsys type"
      ELSE
         msg2 = message
      END IF


      IF(severity .EQ. 'Notice' .OR. severity .EQ. 'notice'
     &   .OR. severity .EQ. 'NOTICE') THEN
         level = '-'
      ELSE IF(severity .EQ. 'Warning' .OR. severity .EQ. 'warning'
     &   .OR. severity .EQ. 'WARNING') THEN
         iWARN = iWARN + 1
         level = 'W'
      ELSE
         iERROR = iERROR + 1
         level = 'F'
      END IF

C If there is a limitation on the number of trnsys warnings,
C some code can be added here to check whether iWARN is greater
C than the predefined value.



C The following code is not supported by g77
C Kept here for future use
C      SELECT CASE (severity)
C         CASE('Notice','notice','NOTICE')
C         level = '-'
C         CASE('Warning','warning','WARNING')
C         iWARN = iWARN+1   ! Update main counter in Common block
C         level = 'W'
C         CASE('Fatal','fatal','FATAL')
C         iERROR = iERROR+1 ! Update main counter in Common block
C         level = 'F'
C         CASE('Stop','stop','STOP')
C         iERROR = iERROR+1 ! Update main counter in Common block
C         level = 'F'
C         CASE DEFAULT
C         iERROR = iERROR+1 ! Update main counter in Common block
C         level = 'F'
C      END SELECT

      CALL LUSRMSG(msg1,msg2,level)

      END SUBROUTINE Messages



C Originally a subroutine in trnsys. Rewrite for esp-r. A large
C portion of the code in this function come directly from trnsys.
C The function aims to perform some simple checks on the
C trnsys type input to ensure the proper number of INPUTS,
C PARAMETERS, and DERIVATIVES have been specified.
      SUBROUTINE TYPECK(IOPT,INFO,NI,NP,ND)
      IMPLICIT NONE

C trnsferred parameters
      INTEGER IOPT,INFO,NI,NP,ND
      DIMENSION INFO(15)

C local variables
      INTEGER       NOPT
      CHARACTER*5   STRING1,STRING2
      CHARACTER*360 ERROR

C If fortran compiler is updated to support ADJUSTL and TRIM
C functions, two changes are necessary to have nice output.
C
C Change 1: Recover the line for ADJUSTL
C Change 2: Apply TRIM on STRING1 and STRING2 in ERROR


C    IF IOPT IS LESS THAN -1, OR ERRORS FOUND WHEN IOPT = -1, THE SIMULATION WILL BE TERMINATED.
      NOPT=IABS(IOPT)
      GO TO (100,200,300,400,500,600,700,800), NOPT

C    CHECK NUMBER OF INPUTS
  100 IF (NI.EQ.INFO(3).OR.NI.LT.0) GO TO 110
      WRITE(STRING1,*) NI
      WRITE(STRING2,*) INFO(3)
C      STRING1=ADJUSTL(STRING1)
C      STRING2=ADJUSTL(STRING2)
      ERROR='An illegal number of INPUTS has been specified. '
     &     // STRING1 //' INPUTS are required and '
     &     // STRING2 //' were specified.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))

C    CHECK NUMBER OF PARAMETERS
  110 IF (NP.EQ.INFO(4).OR.NP.LT.0) GO TO 130
      WRITE(STRING1,*) NP
      WRITE(STRING2,*) INFO(4)
C      STRING1=ADJUSTL(STRING1)
C      STRING2=ADJUSTL(STRING2)
      ERROR='An illegal number of PARAMETERS has been specified.'
     &     // STRING1 //' PARAMETERS are required and '
     &     //STRING2 //' were specified.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))

C    CHECK NUMBER OF DERIVATIVES
  130 IF (ND.EQ.INFO(5).OR.ND.LT.0) GO TO 150
      WRITE(STRING1,*) ND
      WRITE(STRING2,*) INFO(5)
C      STRING1=ADJUSTL(STRING1)
C      STRING2=ADJUSTL(STRING2)
      ERROR='An illegal number of DERIVATIVES has been specified.'
     &     // STRING1 //' DERIVATIVES are required and '
     &     // STRING2 //' were specified.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))

C    ALLOCATE SPACE IN STORE COMMON BLOCK
  150 IF (INFO(10) .LE. 0) THEN
         RETURN
      ELSE
         ERROR='Obsolete storage method is currently not supported '
     &         // ' by the coupling project.'
         CALL MESSAGES(-1,ERROR,'Notice',INFO(1),INFO(2))
         RETURN
      END IF

C    UNSPECIFIED ERROR
  200 ERROR='An unspecified error. Check the input file for possible'
     &      // ' sources of error.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))
      RETURN

C    COMPONENT INPUT ERROR
  300 WRITE(STRING1,*) NI
C      STRING1=ADJUSTL(STRING1)
      ERROR='An illegal INPUT value has been specified.'
     &      // ' Input '//STRING1//' might be the problem.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))
      RETURN

C    COMPONENT PARAMETER ERROR
  400 WRITE(STRING1,*) NP
C      STRING1=ADJUSTL(STRING1)
      ERROR='An illegal PARAMETER value has been specified.'
     &      // ' Parameter '//STRING1//' might be the problem.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))
      RETURN

C    COMPONENT DERIVATIVE ERROR
  500 WRITE(STRING1,*) ND
C      STRING1=ADJUSTL(STRING1)
      ERROR='An illegal DERIVATIVE value has been specified.'
     &    //' DERIVATIVE '//STRING1//' might be the problem.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))
      RETURN

C    THE MAXIMUM NUMBER OF SOME COMPONENT TYPE HAS BEEN EXCEEDED
  600 ERROR='The maximum number of occurences of this component'
     &  // ' model in a TRNSYS input file has been exceeded.'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))
      RETURN

C    CHECK ERROR COUNT - NOT USED ANYMORE
  700 RETURN

C    UNDEFINED COMPONENT TYPE - FLAG ERROR IF LINK CHECKING IS ON
  800 ERROR='Error occurs that need to call LINKCK, not supported'
      CALL MESSAGES(103,ERROR,'FATAL',INFO(1),INFO(2))
      RETURN

      END SUBROUTINE TYPECK








