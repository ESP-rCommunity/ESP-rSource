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
C      MZCSTR

c ******************** MZCSTR ********************

c MZCSTR allows the user to define the various control
c loops which will control the subsequent simulation.
c Each control loop is read from the system control
c strategy file.


      SUBROUTINE MZCSTR(IER)
#include "plant.h"
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/CCTLNM/CTLDOC,LCTLF

      COMMON/PBYDAT/PBYFIL,NSCVP,IPS 

      common/pcfngr/clfgsd(MCF,6),pcfngn,IDAYPD

      logical ok,dok,PBYFIL,pcfngn

      CHARACTER LCTLF*72,dfile*72,h*72,ltmp*72,LBND*72,dpbnd*72
      CHARACTER CTLDOC*248

      dok=.false.
      h(1)='If your model includes a zone/flow/plant control'
      h(2)='say yes and supply the name of the file.'
      h(3)=' '
      h(4)='A no will result in no control actions'
      CALL ASKOK(' Does a plant control strategy exist for',
     &  ' the current problem?',OK,dok,4)
      IF(.NOT.OK)THEN
c No!
        CALL EZCTLI
        CTLDOC='NONE '
        RETURN
      endif

c Yes!
      IUNIT=IFIL+1
      H(1)='The system control file holds the definition of all of'
      H(2)='the building/plant controls which will be imposed on'
      H(3)='the model at simulation time. Pleaes supply the name'
      H(4)='of the file.'
      DFILE=' '
      LTMP=LCTLF
   47 CALL EASKS(LTMP,' System control file:',
     &   ' ',72,DFILE,'control file name',IER,4)
      IF(LTMP.NE.' ')THEN
        LCTLF=LTMP
      ELSE
        CALL EZCTLI
        CTLDOC='NONE '
        RETURN
      ENDIF

      IER=0
      NCL=0

c Read in the control file (close before returning from EZCTLR).
      ITRC=1
      CALL EZCTLR(IUNIT,ITRC,IUOUT,IER)
      IF(IER.NE.0)THEN
        dok=.false.
        h(1)='There should not be a problem with the control file'
        h(2)='at simulation time. Please check the folder and file'
        h(3)='name given. '
        CALL ASKOK(' ',
     &    ' Problem detected reading control...try again?',OK,dok,3)
        IF(OK)GOTO 47
      ENDIF

c Possibly read in function generator supplementary data items.
      if(pcfngn)call pfgrsd      

C Possibly open ascii plant boundry data file.
      if(pbyfil)then
        h(1)='This ascii file can have one or more columns of data '
        h(2)='(space separated) wherein each line is related to a '
        h(3)='simulation timestep (including the pre-conditioning '
        h(4)='period.  Any line beginning with a # is considered a '
        h(5)='comment.'
        dpbnd='pby.asc '
        CALL EASKS(lbnd,' Timestep boundary data file: ',
     &   ' ',72,dpbnd,'plant boundry file',IER,5)
        iunit=ifil+5
        CALL EFOPSEQ(IUNIT,lbnd,1,IER)
        rewind(iunit)
      endif
      RETURN
      END

c ******************** PFGRSD ********************

C This routine is used to obtain the supplementary
C data items required for the plant-side type -6 
C `Function Generator` sensor .

      SUBROUTINE PFGRSD
#include "plant.h"
#include "building.h"
#include "control.h"
      
      common/outin/iuout,iuin
      common/pophelp/h(60)
      common/pcfngr/clfgsd(MCF,6),pcfngn,IDAYPD

      character outs*124,H*72
      character hold*40,nterm*1
      logical pcfngn

      nterm=CHAR(0)

C Read in data for the user-specified function, IFUNC
      
      call edisp(iuout,' ')
      call edisp(iuout,' Function generator plant sensor referenced:')
      call edisp(iuout,' ')
      do 20 i=1,ncl                  
         if(int(clfgsd(i,1)).eq.1)then
          ifunc=ipsn(i,2)
          call edisp(iuout,' Input sensor supplemenatary data for:- ')
          write(outs,70)i
          call edisp(iuout,outs)
70        format(' plant control loop no.',i3)
          
          H(1)=' The data items required for each function are:'
          H(2)=' '
          H(3)='   Function          supp1    supp2    supp3   supp4'
          H(4)='   ================================================ '
          H(5)='   1. Step:          start    finish   max     min  '
          H(6)='   2. Ramp:          start    finish   max     min  '
          H(7)='   3. Square sine:   max      min      freq    ---  '
          H(8)='   4. Square cosine: max      min      freq    ---  '
          H(9)='   5: Triangular:    max      min      freq    ---  '
          H(10)='  6: Saw-tooth:     max      min      freq    ---  '
          H(11)='  7: Sine:          max      min      freq    shift'
          H(12)='  8: Cosine:        max      min      freq    shift'
          H(13)='  9: Sensed prop`y: inc/dec  ---      ---     ---  '
          H(14)=' - Refer ESP-r manual section 3.9 ' 

          if(ifunc.eq.1.or.ifunc.eq.2) then 
  60       WRITE(HOLD,'(4f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),clfgsd(i,4),nterm
           CALL EASKS(HOLD,' start, finish, max and min  values for',
     &     ' step/ramp/sine/cosine function : ',40,' 0. 24. 1. -1. ',
     &     'Step: start, fin, max, min',IER,14)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 0.,24.,'F','Start',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 0.,24.,'F','Finish',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -999999.,99999.,'F','Max',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,4),
     &                 -999999.,99999.,'F','Min',IER)
           if(ier.ne.0)goto 60
          elseif(ifunc.eq.3.or.ifunc.eq.4.or.ifunc.eq.5.or.ifunc.eq.6)
     &          then

  63       WRITE(HOLD,'(3f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),nterm
           CALL EASKS(HOLD,' maximum, minimum, frequency ',
     &     ' for square/tri/saw-tooth functions : ',40,' 1. 0. 1. ',
     &     'square/tri/saw-tooth functions: max, min, frqy',IER,14)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 -99999.,999999.,'F','Str',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 -99999.,999999.,'F','Fin',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -99999.,999999.,'F','Frq',IER)
           if(ier.ne.0)goto 63
          elseif(ifunc.eq.7.or.ifunc.eq.8)then
  90       WRITE(HOLD,'(4f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),clfgsd(i,4),nterm
           CALL EASKS(HOLD,' max, min, freq and shift  values for',
     &     ' sine/cosine function : ',40,' 1. 1. 1. -1. ',
     &     'Sine: max, min, freq, phase',IER,14)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 0.,99999.,'F','Max',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 0.,99999.,'F','Min',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -999999.,99999.,'F','Freq',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,4),
     &                 -999999.,99999.,'F','Shift',IER)
           if(ier.ne.0)goto 90
          elseif(ifunc.eq.9)then

           H(1)=' The items are essentially the same as required '
           H(2)=' in a control point sensor. '
           H(3)=' - Refer ESP-r manual section 3.9 '
  66       WRITE(HOLD,'(3f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),nterm
           CALL EASKS(HOLD,' Sensor details for auxiliary ',
     &     ' sensor? ',40,' 1. 0. 1. ',
     &     ' sensor details for auxiliary sensor',IER,3)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 -99999.,999999.,'F','1st sen',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 -99999.,999999.,'F','2nd sen',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -99999.,999999.,'F','3rd sen',IER)
           if(ier.ne.0)goto 66

           H(1)=' The scaling factor and the offset value (if any)'
           H(2)=' are applied to the sensed value as follows:'
           H(3)='    [SCALING_FACTOR*SENSED_VALUE]+OFFSET '
           H(4)=' If no scaling factor required, input 1;'
           H(5)=' If no offset value required, input 0.'
           H(6)=' - Refer ESP-r manual section 3.9 '
 69        WRITE(HOLD,'(2f7.2,a1)')clfgsd(i,5),clfgsd(i,6),nterm
           CALL EASKS(HOLD,' Scaling factor, and offset',
     &     ' to apply to sensed value: ',40,' 1. 0. 1. ',
     &     ' offset, Scaling factor',IER,6)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,5),
     &                 -99999.,999999.,'F','Mult. fact',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,6),
     &                 -99999.,999999.,'F','Offset',IER)
           if(ier.ne.0)goto 69
          endif
         
         endif
 20   continue

      return
      end
