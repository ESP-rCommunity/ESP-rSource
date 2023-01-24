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

C This file contains the following routines:

C      mzcstr: control loop definition.
C      BFGRSD: read type -6 building sensor sup'y data.
C      PFGRSD: read type -6 plant sensor sup'y data.
C      FFGRSD: read type -6 flow sensor sup'y data.

C ******************** mzcstr

C `Mzcstr` allows the user to define the various control
C functions and control loops which will control the
C subsequent simulation.   Each control function and loop
C is read from the system control strategy file.

      subroutine mzcstr(ier)

#include "building.h"
#include "net_flow.h"
#include "espriou.h"
#include "control.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin,ieout
      common/filep/ifil

      common/cctlnm/ctldoc,lctlf

      common/bfngr/cfgsd(MCF,6),ibsnv(mcf,3),bfngn
      common/pcfngr/clfgsd(MCF,6),pcfngn,IDAYPD
      common/ffngr/cfgsf(MCF,6),ifsnv(MCF,3),ffngn
     
      COMMON/PBYDAT/PBYFIL,NSCVP,IPS 

      COMMON/UA5/IMET,ISIM,NRSIM

      COMMON/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      LOGICAL     spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok

      COMMON/C23a/BPF

      character LCTLF*72,DFILE*72,LTMP*72,LBND*72
      character DPBND*72
      character BPF*72
      character ctldoc*248,outs*124
      logical OK,pbyfil,bfngn,pcfngn,ffngn

      helpinsub='bctlf'  ! set for subroutine

C If in autoexec mode as defined by the simulator parameter file then 
C use the control file specified in the cfg file.
      IUNIT=IFIL+1
      if(spfileok.or.ISIM.gt.1) then
        if(LCTLF(1:7).eq.'UNKNOWN') then
          ok=.false.
        else
          ok=.true.
        endif
      else
        if(LCTLF(1:7).eq.'UNKNOWN') then
          helptopic='no_control_found'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(
     &    ' Does a building/plant/mass flow control strategy exist for',
     &    ' the current model?',OK,nbhelp)
        else
          helptopic='control_was_found'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(outs,'(2a)')' Use ',LCTLF(1:lnblnk(LCTLF))
          CALL EASKOK(outs,' with the current model? ',
     &      OK,nbhelp)
          if(OK)then

C Do initial setup of ier, ncf and if running silently then turn off trace
C or if interactive then only set trace if the first simulation.
            ier=0
            ncf=0
            if(autook)then
              itrc=0
            else
              if(isim.gt.1)then
                itrc=0
              else
                itrc=1
              endif
            endif
            goto 42
          endif
        endif
      endif

      IF(.NOT.OK)THEN
        CALL EZCTLI
        CTLDOC='NONE '
        RETURN
      ENDIF

      helptopic='confirm_control_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
  47  if((.not.autook).AND.(ISIM.le.1)) then
        DFILE=' '
        LTMP=LCTLF

C Test of not confirming the control file name.
C        CALL EASKS(LTMP,' System control file: ',
C     &     ' ',72,DFILE,'control file name',IER,nbhelp)
        IF(LTMP.NE.' ')THEN

C Assign the control file name, reset ier and ncf and avoid
C report contents.
          LCTLF=LTMP
          ier=0
          ncf=0
          if (ISIM.gt.1) then
            ITRC=0
          else
            ITRC=0
          endif
        ELSE
          CALL EZCTLI
          CTLDOC='NONE '
          RETURN
        ENDIF
      else

C In silent mode or subsequent simulation of a set.
        ier=0
        ncf=0
        ITRC=0
      endif

C Read in the control file (close before returning from EZCTLR).
  42  CALL EZCTLR(IUNIT,ITRC,IUOUT,IER)
      IF(IER.NE.0)THEN
        CALL EASKOK('Problem reading control file!','Retry?',
     &    OK,nbhelp)
        IF(OK)THEN

C If there was a problem ask user to confirm control file and
C then try and load again.
          DFILE=' '
          LTMP=LCTLF
          CALL EASKS(LTMP,' System control file: ',
     &      ' ',72,DFILE,'control file name',IER,nbhelp)
          LCTLF=LTMP
          ier=0
          ncf=0
          GOTO 47
        ELSE
          RETURN
        ENDIF
      ENDIF

C Now check to ensure that control and actuator nodes
C actually exist in the associated zone.
      call ezctlc(ier)

C Possibly read in building-side function generator
C supplementary data items.
      if(bfngn)call bfgrsd

C Possibly read in plant function generator supplementary data items.
      if(pcfngn)call pfgrsd
      
C Possibly read in flow function generator supplementary data items.
      if(ffngn)call FFGRSD   

C Possibly open ascii plant-side boundary data file.
      if(pbyfil)then
C Check for boundary file defined in configuration file
        if(BPF(1:3).eq.'../')then
          iunit=ifil+9
          CALL EFOPSEQ(IUNIT,BPF,1,IER)
          rewind(iunit)
          write(currentfile,'(a)') BPF(1:lnblnk(BPF))
        else
C User input of boundary file location
          helptopic='timestep_boundary_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          dpbnd='pby.asc '
          CALL EASKS(lbnd,' ','Plant boundary data file?',
     &        72,dpbnd,'plant boundary file',IER,nbhelp)
          iunit=ifil+9
          CALL EFOPSEQ(IUNIT,lbnd,1,IER)
          rewind(iunit)
          write(currentfile,'(a)') lbnd(1:lnblnk(lbnd))
        endif
      endif
      return
      end

c ******************** BFGRSD ********************

C This routine is used to obtain the supplementary
C data items required for the building-side type -6 
C `Function Generator` sensor .

      SUBROUTINE BFGRSD

#include "building.h"
#include "control.h"
#include "help.h"
      
      common/outin/iuout,iuin,ieout

      common/bfngr/cfgsd(MCF,6),ibsnv(mcf,3),bfngn

      character outs*124
      character hold*40,nterm*1
      logical bfngn

      helpinsub='bctlf'  ! set for subroutine

      nterm=CHAR(0)

C Read in data for the user-specified function, IFUNC
      
      call edisp(iuout,' ')
      call edisp(iuout,' Function generator building sensor referenced')
      call edisp(iuout,' ')
      do 20 i=1,ncf                  
         if(int(cfgsd(i,1)).eq.1)then
          ifunc=ibsn(i,2)
          call edisp(iuout,' Input sensor supplemenatary data for:- ')
          write(outs,70)i
          call edisp(iuout,outs)
70        format(' building control function no.',i3)
          
          helptopic='function_generator_opt'
          call gethelptext(helpinsub,helptopic,nbhelp)

          if(ifunc.eq.1.or.ifunc.eq.2)then
  60       WRITE(HOLD,'(4f7.2,a1)')cfgsd(i,1),cfgsd(i,2),
     &                             cfgsd(i,3),cfgsd(i,4),nterm
           CALL EASKS(HOLD,' start, finish, max and min  values for',
     &     ' step/ramp function : ',40,' 0. 24. 1. -1. ',
     &     'Step: start, fin, max, min',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsd(i,1),
     &                 0.,24.,'F','Start',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,2),
     &                 0.,24.,'F','Finish',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,3),
     &                 -999999.,99999.,'F','Max',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,4),
     &                 -999999.,99999.,'F','Min',IER)
           if(ier.ne.0)goto 60
          elseif(ifunc.eq.3.or.ifunc.eq.4.or.ifunc.eq.5.or.ifunc.eq.6)
     &          then

  63       WRITE(HOLD,'(3f7.2,a1)')cfgsd(i,1),cfgsd(i,2),
     &                             cfgsd(i,3),nterm
           CALL EASKS(HOLD,' maximum, minimum, frequency ',
     &     ' for square/tri/saw-tooth functions : ',40,' 1. 0. 1. ',
     &     'square/tri/saw-tooth functions: max, min, frqy',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsd(i,1),
     &                 -99999.,999999.,'F','Str',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,2),
     &                 -99999.,999999.,'F','Fin',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,3),
     &                 -99999.,999999.,'F','Frq',IER)
           if(ier.ne.0)goto 63
          elseif(ifunc.eq.7.or.ifunc.eq.8)then
  76       WRITE(HOLD,'(4f7.2,a1)')cfgsd(i,1),cfgsd(i,2),
     &                             cfgsd(i,3),cfgsd(i,4),nterm
           CALL EASKS(HOLD,' vmax, vmin, freq, shift  values for',
     &     ' sine/cosine function : ',40,' 0. 24. 1. -1. ',
     &     'vmax, vmin, freq, shift',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsd(i,1),
     &                 -999999.,999999.,'F','vmax',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,2),
     &                 -999999.,999999.,'F','vmin',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,3),
     &                 -999999.,99999.,'F','freq',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,4),
     &                 -999999.,99999.,'F','Shift',IER)
           if(ier.ne.0)goto 76 
          elseif(ifunc.eq.9)then

  66       WRITE(HOLD,'(3f7.2,a1)')cfgsd(i,1),cfgsd(i,2),
     &                             cfgsd(i,3),nterm
           CALL EASKS(HOLD,' Sensor details for auxiliary ',
     &     ' sensor? ',40,' 1. 0. 1. ',
     &     ' sensor details for auxiliary sensor',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsd(i,1),
     &                 -99999.,999999.,'F','1st sen',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,2),
     &                 -99999.,999999.,'F','2nd sen',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,3),
     &                 -99999.,999999.,'F','3rd seb',IER)
           if(ier.ne.0)goto 66

 69        WRITE(HOLD,'(2f7.2,a1)')cfgsd(i,5),cfgsd(i,6),nterm
           CALL EASKS(HOLD,' Scaling factor, and offset',
     &     ' to apply to sensed value: ',40,' 1. 0. 1. ',
     &     ' offset, Scaling factor',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsd(i,5),
     &                 -99999.,999999.,'F','Mult. fact',IER)
           CALL EGETWR(HOLD,K,cfgsd(i,6),
     &                 -99999.,999999.,'F','Offset',IER)
           if(ier.ne.0)goto 69

          endif
         
         endif
 20   continue

      return
      end

C ******************** PFGRSD ********************
C PFGRSD is used to obtain the supplementary data items required for 
C the plant-side type -6 `Function Generator` sensor.

      SUBROUTINE PFGRSD

#include "building.h"
#include "control.h"
#include "help.h"
      
      common/outin/iuout,iuin,ieout
      
      common/pcfngr/clfgsd(MCF,6),pcfngn,IDAYPD

      character outs*124
      character hold*40,nterm*1
      logical pcfngn

      helpinsub='bctlf'  ! set for subroutine

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
          
          helptopic='function_generator_opt'
          call gethelptext(helpinsub,helptopic,nbhelp)

          if(ifunc.eq.1.or.ifunc.eq.2)then
  60       WRITE(HOLD,'(4f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),clfgsd(i,4),nterm
           CALL EASKS(HOLD,' start, finish, max and min  values for',
     &     ' step/ramp function : ',40,' 0. 24. 1. -1. ',
     &     'Step: start, fin, max, min',IER,nbhelp)
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
     &     'square/tri/saw-tooth functions: max, min, frqy',
     &     IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 -99999.,999999.,'F','Str',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 -99999.,999999.,'F','Fin',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -99999.,999999.,'F','Frq',IER)
           if(ier.ne.0)goto 63
          elseif(ifunc.eq.7.or.ifunc.eq.8)then
  76       WRITE(HOLD,'(4f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),clfgsd(i,4),nterm
           CALL EASKS(HOLD,' vmax. vmin, freq, shift values for',
     &     ' sine/cosine function : ',40,' 0. 24. 1. -1. ',
     &     'vmax, vmin, freq, shift',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 -99999.,999999.,'F','vmax',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 -99999.,999999.,'F','vmin',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -999999.,99999.,'F','freq',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,4),
     &                 -999999.,99999.,'F','shift',IER)
           if(ier.ne.0)goto 76
          elseif(ifunc.eq.9)then

  66       WRITE(HOLD,'(3f7.2,a1)')clfgsd(i,1),clfgsd(i,2),
     &                             clfgsd(i,3),nterm
           CALL EASKS(HOLD,' Sensor details for auxiliary ',
     &     ' sensor? ',40,' 1. 0. 1. ',
     &     ' sensor details for auxiliary sensor',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,clfgsd(i,1),
     &                 -99999.,999999.,'F','1st sen',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,2),
     &                 -99999.,999999.,'F','2nd sen',IER)
           CALL EGETWR(HOLD,K,clfgsd(i,3),
     &                 -99999.,999999.,'F','3rd sen',IER)
           if(ier.ne.0)goto 66

 69        WRITE(HOLD,'(2f7.2,a1)')clfgsd(i,5),clfgsd(i,6),nterm
           CALL EASKS(HOLD,' Scaling factor, and offset',
     &     ' to apply to sensed value: ',40,' 1. 0. 1. ',
     &     ' offset, Scaling factor',IER,nbhelp)
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

C ******************** FFGRSD ********************
C FFGRSD is used to assign the supplementary data items required for
C  mass flow sensor type -6 `Function Generator` sensor.

      SUBROUTINE FFGRSD

#include "building.h"
#include "net_flow.h"
#include "control.h"
#include "help.h"
      
      common/outin/iuout,iuin,ieout
      
      common/ffngr/cfgsf(MCF,6),ifsnv(MCF,3),ffngn

      character outs*124,hold*40,nterm*1
      logical ffngn

      helpinsub='bctlf'  ! set for subroutine

      nterm=CHAR(0)

C Read in data for the user-specified function, IFUNC
C jmq:
      write(29,*)' in bctl.F'
      
      call edisp(iuout,' ')
      call edisp(iuout,' Function generator m. flow sensor referenced')
      call edisp(iuout,' ')
      do 20 i=1,ncc                  
         if(int(cfgsf(i,1)).eq.1)then
C jmq:
           write(29,*)' in defn'
          ifunc=ifsn(i,2)
          call edisp(iuout,' Input sensor supplemenatary data for:- ')
          write(outs,70)i
          call edisp(iuout,outs)
70        format(' mass flow control function no.',i3)
          
          helptopic='function_generator_opt'
          call gethelptext(helpinsub,helptopic,nbhelp)

          if(ifunc.eq.1.or.ifunc.eq.2)then
  60       WRITE(HOLD,'(4f7.2,a1)')cfgsf(i,1),cfgsf(i,2),
     &                             cfgsf(i,3),cfgsf(i,4),nterm
           CALL EASKS(HOLD,' start, finish, max and min  values for',
     &     ' step/ramp function : ',40,' 0. 24. 1. -1. ',
     &     'Step: start, fin, max, min',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsf(i,1),
     &                 0.,24.,'F','Start',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,2),
     &                 0.,24.,'F','Finish',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,3),
     &                 -999999.,99999.,'F','Max',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,4),
     &                 -999999.,99999.,'F','Min',IER)
           if(ier.ne.0)goto 60
          elseif(ifunc.eq.3.or.ifunc.eq.4.or.ifunc.eq.5.or.ifunc.eq.6)
     &          then

  63       WRITE(HOLD,'(3f7.2,a1)')cfgsf(i,1),cfgsf(i,2),
     &                             cfgsf(i,3),nterm
           CALL EASKS(HOLD,' maximum, minimum, frequency ',
     &     ' for square/tri/saw-tooth functions : ',40,' 1. 0. 1. ',
     &     'square/tri/saw-tooth functions: max, min, frqy',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsf(i,1),
     &                 -99999.,999999.,'F','Str',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,2),
     &                 -99999.,999999.,'F','Fin',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,3),
     &                 -99999.,999999.,'F','Frq',IER)
           if(ier.ne.0)goto 63
          elseif(ifunc.eq.7.or.ifunc.eq.8)then
  76       WRITE(HOLD,'(4f7.2,a1)')cfgsf(i,1),cfgsf(i,2),
     &                             cfgsf(i,3),cfgsf(i,4),nterm
           CALL EASKS(HOLD,' vmax, vmin, freq, shift  values for',
     &     ' sine/cosine function : ',40,' 0. 24. 1. -1. ',
     &     'vmax, vmin, freq, shift',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsf(i,1),
     &                 -99999.,99999.,'F','Start',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,2),
     &                 -99999.,999999.,'F','Finish',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,3),
     &                 -999999.,99999.,'F','Max',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,4),
     &                 -999999.,99999.,'F','Min',IER)
           if(ier.ne.0)goto 76
          elseif(ifunc.eq.9)then

  66       WRITE(HOLD,'(3f7.2,a1)')cfgsf(i,1),cfgsf(i,2),
     &                             cfgsf(i,3),nterm
           CALL EASKS(HOLD,' Sensor details for auxiliary ',
     &     ' sensor? ',40,' 1. 0. 1. ',
     &     ' sensor details for auxiliary sensor',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsf(i,1),
     &                 -99999.,999999.,'F','1st sen',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,2),
     &                 -99999.,999999.,'F','2nd sen',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,3),
     &                 -99999.,999999.,'F','3rd seb',IER)
           if(ier.ne.0)goto 66

 69        WRITE(HOLD,'(2f7.2,a1)')cfgsf(i,5),cfgsf(i,6),nterm
           CALL EASKS(HOLD,' Scaling factor, and offset',
     &     ' to apply to sensed value: ',40,' 1. 0. 1. ',
     &     ' offset, Scaling factor',IER,nbhelp)
           K=0
           CALL EGETWR(HOLD,K,cfgsf(i,5),
     &                 -99999.,999999.,'F','Mult. fact',IER)
           CALL EGETWR(HOLD,K,cfgsf(i,6),
     &                 -99999.,999999.,'F','Offset',IER)
           if(ier.ne.0)goto 69

          endif
         
         endif
 20   continue

      return
      end
