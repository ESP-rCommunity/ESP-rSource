C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C This file contains the following routines:
C      mzcstr: control loop definition.

C ******************** mzcstr

C `Mzcstr` allows the user to define the various mass flow 
C control functions which will control the subsequent 
C simulation. Each mass flow control function 
C is read from the system control strategy file.

      subroutine mzcstr(ier)

      common/outin/iuout,iuin
      common/filep/ifil
      common/cctlnm/ctldoc,lctlf

      LOGICAL OK,dok

      character LCTLF*72,DFILE*72,LTMP*72,ctldoc*248

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='mzcstr' ! set for subroutine

      dok=.false.
      helptopic='confirm_flow_control'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' Does a mass flow control strategy exist for',
     &  ' the current problem? ',OK,dok,nbhelp)

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
        dok=.false.
        helptopic='error_in_flow_control'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Problem detected in control file...try again?',
     &    OK,dok,nbhelp)
        IF(OK)THEN
          GOTO 47
        ELSE
          RETURN
        ENDIF
      ENDIF

C Now check to ensure that control and actuator nodes
C actually exist in the associated zone.
      call ezctlc(ier)

      return
      end
