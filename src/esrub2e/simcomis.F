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

C RUNSIM  - the routine which deals with running the simulations.
C CTLUPDATE - updates the system control file based on the latest information
C             passed from BEMS and the simnulation type required. 

C ********************* RUNSIM  ****************************************
C This routine runs the simulation, according to the data read from the simulation
C pattern in RDBSIM

      SUBROUTINE RUNSIM

# include "b2e.h"
# include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Commom for the optimim start/stop simulation. 
      COMMON/OPPSIM/NOPSSPT,OPSSPT(MCOM),NOPSSTIM,OPSSTIM(MCOM),
     &NOPSSPCP,OPSSPCP(MCOM),OPSSPST(MCOM)
       
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      COMMON/RBEMDAT/SIMTSTP,NLINE,NCOLS,CURTM,BVAR(MBROW,MBCOL),
     &SENMAP(MBCOL)

      COMMON/B2EI3/MSIM,NZINT,IZINT(MCOM)

      COMMON/B2EI/MODELCFG,BFILE,TDFUD,RSFIL,BREPT

      CHARACTER*72 MODELCFG,BFILE,TDFUD,RSFIL,BREPT
      CHARACTER*124 DOIT,OUTS
      character spfdescr*8,MSIM*4
      INTEGER SENMAP

C Simulation for optimum start stop.
      IF(MSIM.EQ.'OPSS') THEN

C Work out the number of simulations required and their frequency: increment
C back in time to current time or midnight if required.
C If the simulation day > the current time (curr time > time of interest) then 
C run plant backwards to a last possible time of 0.00 am if required.
        CALL EDAY(isstday(1),isstmon(1),IDYST)
        IF(INT(CURTM).LT.IDYST)THEN
          NSIMUL=INT((OPSSTIM(1)-0.0)*isbnstep)
          WRITE(OUTS,*)'No. of simulations ',NSIMUL
          CALL EDISP(IUOUT,OUTS)
        ELSE
C If the current time is in the simulation day (curr time < time of interest)
C then run the plant back to the current time.
          CURHR=(CURTM-INT(CURTM))*24.
          NSIMUL=INT((OPSSTIM(1)-CURHR)*isbnstep)
          WRITE(OUTS,*)'No. of simulations ',NSIMUL
          CALL EDISP(IUOUT,OUTS)
        ENDIF

        DO 10 ISIMUL=1,NSIMUL

C Update the control file for the run. 
           CALL CTLUPDATE(ISIMUL,MSIM)

C Call res to dump out the required simulation data to a temporary file
           WRITE(DOIT,'(7a)')'bld -mode text -file ',
     &     MODELCFG(1:LNBLNK(MODELCFG)),' -p ',
     &     spfdescr(1)(1:LNBLNK(spfdescr(1))),' silent',
     &     ' -actf ',' bems'         
         CALL EDISP(IUOUT,' ')
         CALL EDISP(IUOUT,DOIT)
         CALL RUNIT(DOIT,'-')    

C Analyse the results of the simulation based on the callling mode. 
         CALL SCANRES(MSIM,ISIMUL,NSIMUL)    

  10   CONTINUE

       ELSE
         CALL EDISP(IUOUT,'RUNSIM: Unknown simulation mode!')
         STOP
       ENDIF

       RETURN
       END

