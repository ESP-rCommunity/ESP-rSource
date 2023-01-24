C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following routine.
C mzcstr - control loop definition.

C ******************** mzcstr ********************
C Allows the user to select functions to control the
C flow network throughout the simulation. 

      subroutine mzcstr(ier)
#include "help.h"

      common/outin/iuout,iuin,ieout
      common/filep/ifil
      common/cctlnm/ctldoc,lctlf

      LOGICAL OK

      character LCTLF*72,DFILE*72,LTMP*72,ctldoc*248

      helpinsub='fctlc' ! set for subroutine

      helptopic='confirm_flow_control'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Apply control to the flow network?',OK,nbhelp)

      IF(.NOT.OK)THEN
        CALL EZCTLI
        CTLDOC='NONE '
        RETURN
      ENDIF

      IUNIT=IFIL+1
      helptopic='describe_flow_control'
      call gethelptext(helpinsub,helptopic,nbhelp)
      DFILE=' '
      LTMP=LCTLF
   47 CALL EASKS(LTMP,' Zone/flow/plant control file: ',
     &   ' ',72,DFILE,'control file name',IER,nbhelp)
      IF(LTMP(1:2).NE.'  ')THEN
        LCTLF=LTMP
      ELSE
        CALL EZCTLI
        CTLDOC='NONE '
        RETURN
      ENDIF

      IER=0

C Read in the control file (close before returning from EZCTLR).
      ITRC=1
      CALL EZCTLR(IUNIT,ITRC,IUOUT,IER)
      IF(IER.NE.0)THEN
        helptopic='error_in_flow_control'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK('Problem in control file!','Retry?',
     &       OK,nbhelp)
        IF(OK)THEN
          GOTO 47
        ELSE
          RETURN
        ENDIF
      ENDIF

C Check to ensure that sensed and actuator nodes
C exist in the associated zone.
      call ezctlc(ier)

      return
      end
