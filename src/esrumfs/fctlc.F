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
      common/pophelp/h(60)
      common/cctlnm/ctldoc,lctlf

      LOGICAL OK,dok

      character LCTLF*72,H*72,DFILE*72,LTMP*72,ctldoc*248

      dok=.false.
      h(1)='Although you might have defined control for this flow'
      h(2)='network, this application asks you to confirm whether'
      h(3)='this is the case. '
      CALL ASKOK(' Does a mass flow control strategy exist for',
     &  ' the current problem? ',OK,dok,3)

      IF(.NOT.OK)THEN
        CALL EZCTLI
        CTLDOC='NONE '
        RETURN
      ENDIF

      IUNIT=IFIL+1
      H(1)='The model control file holds the definition of all of'
      H(2)='the zone/flow/plant controls which will be imposed on'
      H(3)='the model at simulation time. If one is associated with'
      H(4)='this flow network provide its name.'
      DFILE=' '
      LTMP=LCTLF
   47 CALL EASKS(LTMP,' Zone/flow/plant control file: ',
     &   ' ',72,DFILE,'control file name',IER,6)
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
        h(1)='An error was reported while scanning the zone/flow/plant'
        h(2)='control file. Check for warning that might have been '
        h(3)='given and if you want to try again say yes. '
        CALL ASKOK(' ','Problem detected in control file...try again?',
     &    OK,dok,3)
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
