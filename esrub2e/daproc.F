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

C The routines in the model associated with the BEMS/ESP-r inteaction
C and reads the data made available from the BEMS program.
C The subroutines are as follows:
C RINTERA - reads the information assocoted with the interaction
C           between ESP-r and labview.
C READBTF - reads the file made available by the BEMS system.
C TDFUDWR - writes out a TDF update file for use in simulation.
C BHDECODE - decides the data held in the BEMS file header data. 
C TDFUPDT - updates the tdf file based on the calling mode.
C CLOOKUP - deals with the generation of future time climate or boundary data.
C SCANRES - scans the results file produced for b2e. 
C CTLUPDT - updates the control file for the simulation.
C ********************* RINTERA ****************************************

      SUBROUTINE RINTERA(INTER,IERROR,CFGSCN)

#include "building.h"

C Open the file passed via the command line for b2e - usually "b2e_inter"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Files for the simulation
      COMMON/B2EI/MODELCFG,BFILE,TDFUD,RSFIL,BREPT

C Data on the simulation - the mode of simulation and the no of zones 
C looked at in the simulation. 
      COMMON/B2EI3/MSIM,NZINT,IZINT(MCOM)

C Commom for the optimim start/stop simulation. 
      COMMON/OPPSIM/NOPSSPT,OPSSPT(MCOM),NOPSSTIM,OPSSTIM(MCOM),
     &NOPSSPCP,OPSSPCP(MCOM),OPSSPST(MCOM)
      common/curfile/currentfile

      CHARACTER*20 WORD
      character*72 MODELCFG*72,BFILE*72,TDFUD*72,RSFIL*72,BREPT*72
      character INTER*144
      CHARACTER OUTSTR*124,OUTS*124,OUTS248*248
      CHARACTER*72 TMPSTR

      CHARACTER MSIM*4,currentfile*72

      LOGICAL CFGSCN

C Open the named file using a temporary file unit
      IIUNIT=IFIL+1
      CALL EFOPSEQ(IIUNIT,INTER,1,IER)
      IF(IER.LT.0) THEN
        WRITE(OUTS248,'(3A)')
     &  'ERROR: Problem opening ',INTER(1:LNBLNK(INTER)),'.'
        call edisp248(iuout,OUTS248,80)
        call epwait
        call epagend
        STOP
      ENDIF
      currentfile=INTER(1:72)

      WRITE(OUTS248,'(A,A)') 'Opened b2e transfer file ',
     &INTER(1:LNBLNK(INTER))
      call edisp248(iuout,OUTS248,80)

C Read the interaction file
C Read the type of the file to obtain the reading mode.
      CALL STRIPC(IIUNIT,OUTSTR,99,ND,0,'check type',IER)
      IF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'*B2EINTER') THEN

10    CALL STRIPC(IIUNIT,OUTSTR,99,ND,0,'seq strip',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','tags',IER)
        IF(WORD(1:15).EQ.'*file_from_BEMS') THEN 
           CALL EGETRM(OUTSTR,K,BFILE,'W','bems update file',IER)
           
C Report 
          call edisp(iuout,' ')
          WRITE(OUTS,'(A,A)') 'Update file passed from BEMS is : ',
     &BFILE(1:LNBLNK(BFILE))
          call edisp(iuout,OUTS)
          IF(IER.EQ.0) GOTO 10 
   
        ELSEIF(WORD(1:15).EQ.'*model_for_BEMS') THEN
          CALL EGETRM(OUTSTR,K,MODELCFG,'W','model cfg',IER)

C Report
          call edisp(iuout,' ')
          WRITE(OUTS,'(A,A)') 'Model for BEMS/ESP simulation is: ',
     &MODELCFG(1:LNBLNK(MODELCFG))
          call edisp(iuout,OUTS)
          IF(IER.EQ.0) GOTO 10

        ELSEIF(WORD(1:15).EQ.'*update_for_TDF') THEN
          CALL EGETRM(OUTSTR,K,TDFUD,'W','tdf update',IER)

C Report.
          call edisp(iuout,' ')
          WRITE(OUTS,'(A,A)') 'TDF update file is: ',
     &TDFUD(1:LNBLNK(TDFUD))
          call edisp(iuout,OUTS)
          IF(IER.EQ.0) GOTO 10
        ELSEIF(WORD(1:15).EQ.'*results_output') THEN
          CALL EGETRM(OUTSTR,K,RSFIL,'W','results file',IER)

C Report.
          call edisp(iuout,' ')
          WRITE(OUTS,'(A,A)') 'Results dump file: ',
     &RSFIL(1:LNBLNK(RSFIL))
          call edisp(iuout,OUTS)
          IF(IER.EQ.0) GOTO 10

        ELSEIF(WORD(1:15).EQ.'*report_to_BEMS') THEN
          CALL EGETRM(OUTSTR,K,BREPT,'W','report file',IER)

C Report.
          call edisp(iuout,' ')
          WRITE(OUTS,'(A,A)') 'Report to BEMS: ',
     &BREPT(1:LNBLNK(BREPT))
          call edisp(iuout,OUTS)

          IF(IER.EQ.0) GOTO 10

        ELSEIF(WORD(1:17).EQ.'*zone_of_interest') THEN
          CALL STRIPC(IIUNIT,OUTSTR,99,ND,0,'seq strip',IER)
          k=0
          CALL EGETWI(OUTSTR,K,NZINT,0,MCOM,'W','nzint',IER)
          DO 40 IZ=1,NZINT
            CALL EGETWI(OUTSTR,K,IZINT(IZ),0,MCOM,'W',
     &      'model cfg',IER)
            WRITE(OUTS,*) 'Zone(s) of interest: ',IZINT(IZ)    
            CALL EDISP(IUOUT,OUTS)       
 40       CONTINUE
          IF(IER.EQ.0) GOTO 10

        ELSEIF(WORD(1:4).EQ.'*sim') THEN
          CALL EGETRM(OUTSTR,K,MSIM,'W','report file',IER)
          CALL EDISP(IUOUT,'Reading simulation data ...')  

C Read the optimum start stop data. 
          IF(MSIM.EQ.'OPSS') THEN
            CALL EDISP(IUOUT,'Optimum start stop ...')  
C Get the temperature set point(s)
            CALL STRIPC(IIUNIT,OUTSTR,99,ND,0,'seq strip',IER)
            k=0
            CALL EGETWI(OUTSTR,K,NOPSSPT,0,0,'-','ntspt',IER)
            IF(NOPSSPT.NE.NZINT) THEN
              CALL EDISP(IUOUT,
     &'ERROR RINTERA: the no of set point temps <> no of ctl zones')
              STOP
            ELSE
              DO 42 I=1,NOPSSPT
                CALL EGETWR(OUTSTR,K,OPSSPT(I),0.,0.,'-',
     &'var of interest',IER)     
                WRITE(OUTS,*) 'Temp required: ',OPSSPT(I)   
                CALL EDISP(IUOUT,OUTS)           
 42           CONTINUE
            ENDIF

C Read the available plant capacity.
            CALL STRIPC(IIUNIT,OUTSTR,99,ND,0,'seq strip',IER)
            k=0
            CALL EGETWI(OUTSTR,K,NOPSSPCP,0,0,'-','no tim',IER)
            IF(NOPSSPCP.NE.NZINT) THEN
              CALL EDISP(IUOUT,
     &'ERROR RINTERA: the no of plant capacities <> no of ctl zones')
              STOP
            ELSE
              DO 43 I=1,NOPSSPCP
                CALL EGETWR(OUTSTR,K,OPSSPCP(I),0.,0.,
     &          '-','plant cap',IER)       
                WRITE(OUTS,*) 'Plant capacity: ',OPSSPCP(I)
                CALL EDISP(IUOUT,OUTS) 
  43          CONTINUE
            ENDIF

C Read the required time to reach set point.
            CALL STRIPC(IIUNIT,OUTSTR,99,ND,0,'seq strip',IER)
            k=0
            CALL EGETWI(OUTSTR,K,NOPSSTIM,0,0,'-','no tim',IER)
            IF(NOPSSTIM.NE.NZINT) THEN
              CALL EDISP(IUOUT,
     &'ERROR RINTERA: the no of set point times <> no of ctl zones')
              STOP
            ELSE
              DO 44 I=1,NOPSSTIM
                CALL EGETWR(OUTSTR,K,OPSSTIM(I),0.,0.,
     &          '-','tim',IER)       
                WRITE(OUTS,*) 'Time of interest: ',OPSSTIM(I)
                CALL EDISP(IUOUT,OUTS) 
  44          CONTINUE
            ENDIF 

C End of data for Optimum start stop 
            IF(IER.EQ.0) GOTO 10
          ENDIF
        ELSEIF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'*end') THEN

C Scan the cfg file (once only) to get the name of the associated tdf database
C and other simulation related data. 
          IFCFG=IFIL+1
          IF(.NOT.CFGSCN) THEN
            TMPSTR=MODELCFG(1:LNBLNK(MODELCFG))
            IAPROB=IFIL+22
            ifcfg=IFIL+5
            ITRC=0
            CALL ERSYS(TMPSTR,ifcfg,IAPROB,'NONE',ITRC,IER)
            IF(IER.NE.0) THEN
              WRITE(OUTS,*)'b2e ERROR opening the cfg file',
     &MODELCFG(1:LNBLNK(MODELCFG))
              CALL EDISP(IUOUT,OUTS)
              call epwait
              call epagend
              STOP
            ENDIF
            CFGSCN=.TRUE.
          ENDIF

          RETURN
C Report 
        ELSE
          call edisp(iuout,' ')
          WRITE(OUTS,'(A)') OUTSTR
          call edisp(iuout,OUTS)
          WRITE(OUTS248,'(3A)') 'ERROR: Problem with ',
     &      INTER(1:LNBLNK(INTER)),', not a known data tag.'
          call edisp248(iuout,OUTS248,80)
          call epwait
          call epagend
          STOP 
        ENDIF
      ELSE

        WRITE(OUTS,'(3A)') 'ERROR: Problem with ',
     &    INTER(1:LNBLNK(INTER)),
     &    ', this is not a bems -> esp-r interaction file.'
          call edisp248(iuout,OUTS248,80)
        call epwait
        call epagend
        STOP 

      ENDIF

      WRITE(OUTS248,'(A,A)')
     &  'ERROR: Problem with ',INTER(1:LNBLNK(INTER))
      call edisp248(iuout,OUTS248,80)
      call epwait
      call epagend
      STOP 

      END

C **************************** READBTF ********************************
C This routine reads the data file transfered from a BEMS systems the variables
C used in the routine are:
C SIMTSTP - the simulation time step (calculated after reading the transfer file.
C NLINE - the number of lines of data in the transfer file
C NCOLS - the number of columns of data held in the transfer file.
C CURTM - the current time (the last line of data sent by the BEMS)
C BVAR - an array holding the data sent by the BEMS routine. 
C BHSTR - an array holding a descriptive string for each column of data.

      SUBROUTINE READBTF(IERROR)

#include "b2e.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/RBEMDAT/SIMTSTP,NLINE,NCOLS,CURTM,BVAR(MBROW,MBCOL),
     &SENMAP(MBCOL)

      COMMON/B2EI/MODELCFG,BFILE,TDFUD,RSFIL,BREPT

C Simulation parameter periods.
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave

      common/C21/IFCFG,cfgroot,LCFGF

      COMMON/RBEMSTR/BHSTR(MBCOL)   

      COMMON/RBEMDATF/NFTSTP,BFVAR(MBROW,MBCOL)

C Commom for the optimim start/stop simulation. 
      COMMON/OPPSIM/NOPSSPT,OPSSPT(MCOM),NOPSSTIM,OPSSTIM(MCOM),
     &NOPSSPCP,OPSSPCP(MCOM),OPSSPST(MCOM)
      common/curfile/currentfile

      CHARACTER*8 BHSTR
      CHARACTER cfgroot*24,currentfile*72
      character*72 MODELCFG,TDFUD,BFILE,RSFIL,BREPT,LCFGF
      CHARACTER*124 OUTSTR,OUTS
      
      INTEGER IBUNIT,NCOLS,NLINE,SENMAP

      REAL BVAR,SIMTSTP,CURTM

      SMALL=1E-15
      ierror=0

C Open the named file using a temporary file unit.
      IBUNIT=IFIL+1
      CALL EFOPSEQ(IBUNIT,BFILE,1,IER)
      IF(IER.LT.0) THEN
        WRITE(OUTS,'(A,A,A)')
     &  'ERROR: Problem opening ',BFILE(1:LNBLNK(BFILE)),'.'
        call edisp(iuout,OUTS)
        call epwait
        call epagend
        ierror=1
        STOP
      ENDIF
      currentfile=BFILE

C Firstly clear bvar and bfvar arrays.
      DO 11 I=1,MBROW
        DO 21 J=2,MBCOL
          BVAR(I,J)=0.0
          BFVAR(I,J)=0.0
  21    continue
  11  continue

C Read the type of the file to obtain the reading mode.
      CALL STRIPC(IBUNIT,OUTSTR,99,ND,0,'check type',IER)
      IF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'*LABVIEW') THEN
        CALL STRIPC(IBUNIT,OUTSTR,99,ND,0,'format marker',IER)
C Read the file format header string
        CALL STRIPC(IBUNIT,OUTSTR,99,ND,0,'check header',IER)
          NCOLS=ND
          K=0
          DO 10 ICOL=1,NCOLS
            CALL EGETW(OUTSTR,K,BHSTR(ICOL),'W','header',IER)
  10      CONTINUE

C Check the first string is the time.
        IF(BHSTR(1)(1:LNBLNK(BHSTR(1))).NE.'TIME') THEN
          WRITE(OUTS,'(A,A,A)')
     &    'ERROR: Problem with ',BFILE(1:LNBLNK(BFILE)),
     &    ' first column is not a time column.'
          call edisp(iuout,OUTS)
          call epwait
          call epagend
          STOP
        ENDIF    

C Read the first two lines and obtain the time step
        DO 20 ILINE=1,2
          CALL STRIPC(IBUNIT,OUTSTR,99,ND,0,'first line',IER)
          K=0
          DO 30 ICOL=1,NCOLS
            CALL EGETWR(OUTSTR,K,BVAR(ILINE,ICOL),0.,0.,'-',
     &      'row 1',IER)
  30      CONTINUE
  20    CONTINUE 

C Calculate the simulation time step(s).        
        SIMTSTP=(BVAR(2,1)-BVAR(1,1))*24.*3600.
C Check the calculated time
        IF(SIMTSTP.LT.SMALL) THEN
          WRITE(OUTS,'(A,A,A)')
     &    'ERROR: Problem with ',BFILE(1:LNBLNK(BFILE)),
     &    ' calculated timestep is close to zero'
          call edisp(iuout,OUTS)
          call epwait
          call epagend
          STOP
        ELSEIF(INT(SIMTSTP/3600.).GT.1) THEN
          WRITE(OUTS,'(A,A,A)')
     &    'ERROR: Problem with ',BFILE(1:LNBLNK(BFILE)),
     &    ' calculated timestep is > 1 hour.'
          call edisp(iuout,OUTS)
          call epwait
          call epagend
          STOP
        ENDIF
                 

C Continue reading to the end of the file, checking all data for
C errors
        ILINE=3
        TIMEP=-1.0
        TIMEC=0.0
  5     CALL STRIPC(IBUNIT,OUTSTR,99,ND,0,'next line',IER)
        K=0
        IF(IER.EQ.2) THEN
          WRITE(OUTS,'(A,A,A)')
     &    'END: Finished reading ',BFILE(1:LNBLNK(BFILE)),'.'
          WRITE(6,'(A)') OUTS           
          NLINE=ILINE-1

C Write the current time 
          CURTM=BVAR(NLINE,1)
          GOTO 7 
        ELSE         
          DO 40 ICOL=1,NCOLS
            CALL EGETWR(OUTSTR,K,BVAR(ILINE,ICOL),0.,0.,'-',
     &      'row 1',IER)
  40      CONTINUE

C Check that last part of the file is not 2 duplicate lines.
          TIMEC=BVAR(ILINE,1)
          if(TIMEP.eq.TIMEC)then
            goto 5
          endif
          TIMEP=TIMEC
          ILINE=ILINE+1  
          GOTO 5
        ENDIF
      ELSE
        WRITE(OUTS,'(A,A,A)')
     &  'ERROR: Problem with ',BFILE(1:LNBLNK(BFILE)),
     &', not a known file type.'
        call edisp(iuout,OUTS)
        call epwait
        call epagend
        STOP  
      ENDIF

C Close the file
 7    CLOSE(IBUNIT)

C Update the configuration file to suit the time window in the transfer file. 

C If the current time is > than the time of interest (1) then set the simulation 
C day to next day.
          CURHR=(CURTM-INT(CURTM))*24.
          IF(CURHR.GT.OPSSTIM(1))THEN
          
            CALL EDISP(IUOUT,'Setting simulation day ....') 

C If the current time is > than the time of interest then set the simulation
C day to next day.
            IDAYOPSS=INT(CURTM)+1
            CALL EDAYR(IDAYOPSS,isstday(1),isstmon(1))
            WRITE(OUTS,'(a,2i3,a,i3,a)')
     &      'Simulation start day month: ',isstday(1),isstmon(1),
     &' ( ',IDAYOPSS,' )'
            CALL EDISP(IUOUT,OUTS) 
            CALL EDAYR(IDAYOPSS,isfnday(1),isfnmon(1))
            WRITE(OUTS,'(a,2i3,a,i3,a)')
     &      'Simulation end day month: ',isfnday(1),isfnmon(1),
     &' ( ',IDAYOPSS,' )'
            CALL EDISP(IUOUT,OUTS) 
          ELSE

C If the current time is < than the time of interest then set the simulation
C day to today.
            IDAYOPSS=INT(CURTM)
            CALL EDAYR(IDAYOPSS,isstday(1),isstmon(1))
            WRITE(OUTS,'(a,2i3,a,i3,a)')
     &      'Simulation end day month: ',isstday(1),isstmon(1),
     &' ( ',IDAYOPSS,' )'
            CALL EDISP(IUOUT,OUTS) 
            CALL EDAYR(IDAYOPSS,isfnday(1),isfnmon(1))
            WRITE(OUTS,'(a,2i3,a,i3,a)')
     &      'Simulation start day month: ',isstday(1),isstmon(1),
     &' ( ',IDAYOPSS,' )'
            CALL EDISP(IUOUT,OUTS) 
          ENDIF

C Set the simulation timestep to that in the tdf database and file.
          isbnstep=NINT(3600./SIMTSTP)

C Update the config file with this data. 
          CALL EDISP(IUOUT,'Updating configuration file with the start')
          CALL EDISP(IUOUT,'and stop dates ....')  
          LCFGF=MODELCFG(1:LNBLNK(MODELCFG))
          IFCFG=IFIL+5
          CALL EMKCFG('-',IER)
      RETURN
      END

C ************************* TDFUDWR ***********************************
C This routine uses data read from the BEMS file to create a TDF update file
C MODE=1 Write out a tdf exchange file;
C MODE=2 Read a tdf extract file.
C TDF sample 
 
CTDF exchange
C*task,update
C*stepperhour,   4
C*starting,  3.50000
C*noofstep,   1
C*nooffield,  10
C# column,description,item
C   2  Diff hor solar :,allclm15                                         
C   3  Ext DB temp    :,allclm15                                         
C   4  Dirn:glob solar:,allclm15                                         
C   5  Wind velocity  :,allclm15                                         
C   6  Wind direction :,allclm15                                         
C   7  Relative humid :,allclm15                                         
C   8  Sens convec W:  ,cellcas                                          
C   9  Sens radiant W: ,cellcas                                          
C  10  Latent  W:      ,cellcas                                          
C  11  Set point temp :,ctlset                                           
C*data
C240.2434,0.000,45.970,3.160,0.000,4.990,131.980,80.000,36.700,0.000,0.000,14.000,0.000,
C*end_data

      SUBROUTINE TDFUDWR(MODE,IERROR)

#include "b2e.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/B2EI/MODELCFG,BFILE,TDFUD,RSFIL,BREPT

      COMMON/RBEMDAT/SIMTSTP,NLINE,NCOLS,CURTM,BVAR(MBROW,MBCOL),
     &SENMAP(MBCOL)

      COMMON/RBEMDATF/NFTSTP,BFVAR(MBROW,MBCOL)
      common/curfile/currentfile
     
      CHARACTER*1 SEP1,SEP2,MODE     
      CHARACTER*24 BHDSTR1(MBCOL),BHDSTR2(MBCOL)
      character*72 MODELCFG,TDFUD,BFILE,RSFIL,BREPT
      CHARACTER OUTS*124,currentfile*72
      
      INTEGER IBUNIT,NCOLS,NLINE,SENMAP,ISTPH,NFTSTP

      REAL BVAR,BFVAR,SIMTSTP,CURTM

      IF(MODE.EQ.'U') THEN

C Set the data seperators
        SEP1=','
        SEP2=':'

C Open the named file using a temporary file unit
        IBUNIT=IFIL+1
        BFILE=TDFUD(1:LNBLNK(TDFUD))
        CALL EFOPSEQ(IBUNIT,BFILE,3,IER)
        IF(IER.NE.0) THEN
          WRITE(OUTS,'(A,A,A)')
     &    'ERROR: Problem opening ',BFILE(1:LNBLNK(BFILE)),'.'
          call edisp(iuout,OUTS)
          call epwait
          call epagend
          STOP
        ENDIF
        currentfile=BFILE

C Write out the header data
        WRITE(IBUNIT,'(A)')'TDF exchange'
        WRITE(IBUNIT,'(A)')'*task,update'

C Write out the integer value of the timestep.
        ISTPH=NINT(3600./SIMTSTP) 
        IF(ISTPH.EQ.0) ISTPH=1
        WRITE(IBUNIT,'(A,I5)')'*stepperhour,',ISTPH
        WRITE(IBUNIT,'(A,F10.4)')'*starting,',BVAR(1,1)
        WRITE(IBUNIT,'(A,I5)')'*noofstep,',NLINE+NFTSTP
        WRITE(IBUNIT,'(A,I5)')'*nooffield,',NCOLS-1

        WRITE(IBUNIT,'(A,F10.4)')'# column,description,item'

C Determine descriptive strings according to the header strings also
C get the sensor mapping ID's
        CALL BHDECODE(BHDSTR1,BHDSTR2,IERROR)

C Write out the field headers
        DO 10,ICOL=2,NCOLS
          WRITE(IBUNIT,'(I4,2X,A,1X,A1,A1,A)') ICOL,
     &BHDSTR1(ICOL),SEP2,SEP1,BHDSTR2(ICOL)
  10    CONTINUE

        WRITE(IBUNIT,'(A)')'*data'   
C Now write out the data read from the bems transfer file.
        DO 20,ILINE=1,NLINE+NFTSTP
        WRITE(OUTS,'(F8.4,A1,A8,30(A1,F8.4))')
     &BFVAR(ILINE,1),SEP1,'  0.0000',(SEP1,BFVAR(ILINE,J),J=2,NCOLS)  
        WRITE(IBUNIT,'(A)') OUTS(1:LNBLNK(OUTS)) 
  20    CONTINUE 
        WRITE(IBUNIT,'(A)')'*end_data' 


      ELSEIF(MODE.EQ.'E') THEN
        CONTINUE
      ENDIF

        CLOSE(IBUNIT)

      RETURN
      END
C
C ******************************** BHDECODE ****************************
C This subroutine decodes the header strings from labview

      SUBROUTINE BHDECODE(BHDSTR1,BHDSTR2,IERROR)

#include "b2e.h"

      COMMON/RBEMDAT/SIMTSTP,NLINE,NCOLS,CURTM,BVAR(MBROW,MBCOL),
     &SENMAP(MBCOL)


      COMMON/RBEMSTR/BHSTR(MBCOL)

      CHARACTER*8 BHSTR
      CHARACTER*24 BHDSTR1(MBCOL),BHDSTR2(MBCOL),TMPSTR

      INTEGER NLINE,NCOLS,SENMAP
      REAL SIMTSTP,CURTM,BVAR

      DO 10 ICOL=2,NCOLS

C Check the form of the header:
C Form ADDDDXXX
C 1st character A
C Z - zone
C P - plant
C E - external (i.e. climate)
C Next characters DDDD : 
C TEMP - temperature (oC)
C MFLW - flow rate   (kg/s) 
C ......
C Next characters (external variables) DDDD : 
C DIFF - diffuse horizontal radiation (W/m2)
C DIRN - direct normal solar radiation (W/m2) 
C TEMP - temperature (oC)
C WVEL - wind velocity (m/s)
C WDIR - wind direction (o from N)
C RELH - relative humidity (%)


C Decode the header 
        IF(BHSTR(ICOL)(1:1).EQ.'Z') THEN
C Zone parameters
          IF(BHSTR(ICOL)(2:5).EQ.'TEMP') THEN
            WRITE(BHDSTR2(ICOL),'(A,A3)')'ZoneTmp',BHSTR(ICOL)(6:8)
            BHDSTR1(ICOL)='Zone temp spt.'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'FLOW') THEN
            WRITE(BHDSTR2(ICOL),'(A,A3)') 'ZoneFlw',BHSTR(ICOL)(6:8)
            WRITE(BHDSTR1(ICOL),'(A)') 'Zone flow spt.'
          ENDIF

C Add mapping variables here XXX-refers to zone sensor number
          READ(BHSTR(ICOL)(6:8),*)SENMAP(ICOL)
        ELSEIF(BHSTR(ICOL)(1:1).EQ.'B') THEN
          IF(BHSTR(ICOL)(2:5).EQ.'TEMP') THEN
            WRITE(BHDSTR2(ICOL),'(A,A3)')'BoneTmp',BHSTR(ICOL)(6:8)
            BHDSTR1(ICOL)='Bound temp.'
          ENDIF
        ELSEIF(BHSTR(ICOL)(1:1).EQ.'P') THEN
          IF(BHSTR(ICOL)(2:5).EQ.'TEMP') THEN
            WRITE(BHDSTR2(ICOL),'(A,A3)')'PlantTmp',BHSTR(ICOL)(6:8)
            BHDSTR1(ICOL)='Plant temp spt.'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'FLOW') THEN
            WRITE(BHDSTR2(ICOL),'(A,A3)') 'PlantFlw',BHSTR(ICOL)(6:8)
            WRITE(BHDSTR1(ICOL),'(A)') 'Plant flow spt.'
          ENDIF
C Add mapping variables here XXX-refers to plant sensor number, convert 
C this part of the string to an integer
          READ(BHSTR(ICOL)(6:8),*)SENMAP(ICOL)

C External (climate parameters)
        ELSEIF(BHSTR(ICOL)(1:1).EQ.'E') THEN
          BHDSTR2(ICOL)='ClimateData'
          IF(BHSTR(ICOL)(2:5).EQ.'TEMP') THEN
            BHDSTR1(ICOL)='Ext air temp (oC)'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'DIRN') THEN
            BHDSTR1(ICOL)='Direct normal rad (W)'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'DIFF') THEN
            BHDSTR1(ICOL)='Diffuse horiz rad (W)'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'WVEL') THEN
            BHDSTR1(ICOL)='Wind speed (m/s)'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'WDIR') THEN
            BHDSTR1(ICOL)='Wind direction (o)'
          ELSEIF(BHSTR(ICOL)(2:5).EQ.'RELH') THEN
            BHDSTR1(ICOL)='Rel Humidity (%)'
          ELSE
            BHDSTR1(ICOL)='UNKNOWN parameter'
          ENDIF
        ELSE

          WRITE(6,'(A)') 'Error reading the BEMS data ',
     &'- not a known measured variable '
          IERROR=-1

        ENDIF

C Write out the strings properly        
        TMPSTR=' '
        WRITE(TMPSTR,'(A)') BHDSTR1(ICOL)(1:LNBLNK(BHDSTR1(ICOL)))
        WRITE(BHDSTR1(ICOL),'(A)') TMPSTR
        TMPSTR=' '
        WRITE(TMPSTR,'(A)') BHDSTR2(ICOL)(1:LNBLNK(BHDSTR2(ICOL)))
        WRITE(BHDSTR2(ICOL),'(A)') TMPSTR

  10  CONTINUE

      RETURN
      END

C ********************* TDFUPDT ****************************************
C This routine calls tdf in update mode. There are two calling modes.
C BEMS - updates tdf with in formation from a BEMS system
C CLIM - updates tdf with information from a climate lookahead routine. 

      SUBROUTINE TDFUPDT(MODE)

C Commons
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/B2EI/MODELCFG,BFILE,TDFUD,RSFIL,BREPT

C Temporal definition commons.
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

      CHARACTER*4 MODE
      CHARACTER*72 LTDF,LTDFA,MODELCFG,BFILE,TDFUD,RSFIL,BREPT
      CHARACTER*124 DOIT
    
C Update tdf based on the callimg mode
      IF(MODE.EQ.'BEMS') THEN

C Make up the command line argument and write to string
        WRITE(DOIT,'(4A)')'tdf -mode text -file ',
     &LTDF(1:LNBLNK(LTDF)),' -act update -actf ',TDFUD(1:LNBLNK(TDFUD))

        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,DOIT)
        CALL RUNIT(DOIT(1:LNBLNK(DOIT)),'-')
      ELSE
        CALL EDISP(IUOUT,'b2e ERROR: not a known tdf update mode')
        call epwait
        call epagend
        STOP
      ENDIF 

      RETURN
      END

C ******************** CLOOKUP ****************************************
C This subroutine handles the climate prediction for b2e, the calling
C mode determines  the type of prediction done. 

      SUBROUTINE CLOOKUP(MODE)

C Common blocks

#include "building.h"
#include "b2e.h"

      COMMON/OUTIN/IUIN,IUOUT

      COMMON/RBEMSTR/BHSTR(MBCOL)

      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)

      COMMON/RBEMDAT/SIMTSTP,NLINE,NCOLS,CURTM,BVAR(MBROW,MBCOL),
     &SENMAP(MBCOL)

      COMMON/RBEMDATF/NFTSTP,BFVAR(MBROW,MBCOL)

      CHARACTER*8 BHSTR
      CHARACTER*4 MODE

      DIMENSION FVAR(MBROW),PVAR(MBROW)
 
      INTEGER NCOLS,NLINE,SENMAP

      REAL BVAR,BFVAR,SIMTSTP,CURTM
      logical today

C Calculate the number of future predicted timesteps required.
      CALL EDAY(isfnday(1),isfnmon(1),IEDOY)
      ENDTIM=(FLOAT(IEDOY)+1.)*24.*3600.
      CURTIM=CURTM*24.*3600.
      NFTSTP=NINT((ENDTIM-CURTIM)/SIMTSTP)

C Create an array of time-stamped zero elements, which will be used to 
C store the future timestep climate data. 
      DO 10 I=1,MBROW

C Firstly copy the time stamp of each line.
        BFVAR(I,1)=BVAR(I,1)

C For times > present time calculate the time step
        IF(I.GT.NLINE) BFVAR(I,1)=BFVAR(I-1,1)+(SIMTSTP/(24.*3600.))
        DO 20 J=2,MBCOL
          BFVAR(I,J)=BVAR(I,J)

C Zero all elements which are > the current time
          IF(I.GT.NLINE) BFVAR(I,J)=0.0
  20    CONTINUE
  10    CONTINUE

C Determine how many columns of data in the transfer file are columns of 
C climate data.
      DO 30 I=2,NCOLS
        IF(BHSTR(I)(1:1).EQ.'E'.OR.BHSTR(I)(1:1).EQ.'B') THEN

C Write the column of data to a temporary array and pass it into
C the interpolation routine
          DO 40 J=1,MBROW
            FVAR(J)=BVAR(J,I)
            PVAR(J)=BVAR(J,I)
  40      CONTINUE

C Based on the calling mode calculate fututre values for each climate
C row.
          IF(MODE.EQ.'OFFS') THEN

C Calculate the future temperatures based on the previous temperatures with
C a 24 hr offset. 
C Firstly check that there are sufficient data items to do the job
            IF(NFTSTP.GT.NLINE) THEN
              CALL EDISP(IUOUT,
     &' CLOOKUP ERROR: insufficient monitored timesteps supplied')
              CALL EDISP(IUOUT,'to run a simulation')
              STOP
            ELSEIF(NFTSTP.GT.MBROW) THEN
               CALL EDISP(IUOUT,
     &' CLOOKUP ERROR: too many time steps required')
              CALL EDISP(IUOUT,'in this simulation: increase time step')
              STOP             
            ENDIF

            CALL EDAY(isstday(1),isstmon(1),ip1d)

C Check whether simulation is performed for current or future day
            today=.false.
            IF(INT(CURTM).EQ.ip1d) today=.true.            
            days=2.0
            IF(today) days=1.0            
            CALL CLSPLINE(PVAR,NLINE+NFTSTP,NLINE,NFTSTP
     &,real(days*24.*(3600./SIMTSTP)),0.,0.,'d',FVAR,IER)
          ELSE
            call edisp(iuout,
     &        'b2e ERROR: unknown climate interpolation mode')
          ENDIF

C Write the future time steps climate data into the future array
          DO 50 J=NLINE+1,NLINE+NFTSTP
            BFVAR(J,I)=FVAR(J)      
  50      CONTINUE
        ENDIF

  30  CONTINUE

      RETURN
      END 
      
C *****************************************************************************
C Subroutine SCANRES scans the dumped results output file and based on the 
C simulation pattern selects suitable data to return to the bems system

      SUBROUTINE SCANRES(MODE,ISIMUL,NSIMUL)

# include "building.h"
C Commons
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/FILEP/IFIL

      COMMON/B2EI/MODELCFG,BFILE,TDFUD,RSFIL,BREPT

      COMMON/B2EI3/MSIM,NZINT,IZINT(MCOM)

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave

C Commom for the optimim start/stop simulation. 
      COMMON/OPPSIM/NOPSSPT,OPSSPT(MCOM),NOPSSTIM,OPSSTIM(MCOM),
     &NOPSSPCP,OPSSPCP(MCOM),OPSSPST(MCOM)
      common/curfile/currentfile

      DIMENSION RESULT(MCOM)

      CHARACTER MODE*4,MSIM*4,currentfile*72
      CHARACTER*72 MODELCFG,BFILE,TDFUD,RSFIL,BREPT
      CHARACTER*124 OUTSTR,OUTS

      LOGICAL RESULT,TCLOSE,RESULTG

      TCLOSE=.FALSE.
      RESULTG=.TRUE.
  

      DO 5 I=1,MCOM
        RESULT(I)=.FALSE.
  5   CONTINUE

C Open the results output file 
      ier=0
      IRFIL=60
      open(IRFIL,file='fort.60',status='UNKNOWN',err=1)
      rewind(IRFIL,err=1)

C Based on the simulation pattern scan through the data. 
C Pass back the relevant data to the bems system. 

      IF(MODE.EQ.'OPSS') THEN
C Scan through the file 
  25    CALL STRIPC(IRFIL,OUTSTR,99,ND,0,'bems res strip',IER)
        IF(IER.EQ.2) THEN
          CALL EDISP(IUOUT,'End of file - No control solution!') 
          RETURN
        ELSEIF(IER.EQ.1) THEN
          CALL EDISP(IUOUT,'Error opening results file ...abort') 
          STOP
        ENDIF
        K=0
C Recover the zone ID
        CALL EGETWI(OUTSTR,K,IDZ,0,0,'-','zone idz',IER)
        CALL EGETWR(OUTSTR,K,DAY,0.,0.,'-','zone day',IER)
        CALL EGETWR(OUTSTR,K,TIMEH,0.,0.,'-','time h',IER)
        CALL EGETWR(OUTSTR,K,TEMP,0.,0.,'-','time t',IER)

Check if criteria have been met for this zone. 
        NTSPH=isbnstep
        toler=(1./(24*float(NTSPH)))*0.5
        DO 10 IZ=1,NZINT
          IZI=IZINT(IZ)
          IF(IDZ.EQ.IZI) THEN
            TIMINT=OPSSTIM(IZI)
            TIMEP=TIMEH+toler
            TIMEM=TIMEH-toler
            if(TIMINT.le.(TIMEP).and.TIMINT.gt.(TIMEM))then
              tclose=.true.
            endif
C            CALL ECLOSE(TIMEH,TIMINT,toler,TCLOSE)
            IF(TCLOSE) THEN
              TCLOSE=.FALSE.
              CALL ECLOSE(TEMP,OPSSPT(IZI),0.1,TCLOSE)
              IF(TCLOSE.OR.TEMP.GT.OPSSPT(IZI)) THEN
                RESULT(IZI)=.TRUE.
                TCLOSE=.FALSE.
              ENDIF
            ENDIF
          ENDIF
10      CONTINUE

C If the temperature is close to the set point at the
C time of interest and in all the zones of interest
C then return the start time to BEMS.
        RESULTG=.FALSE.
        DO 30 IZ=1,NZINT
          IZI=IZINT(IZ)
          IF(RESULT(IZI)) THEN
            IE2BFIL=IFIL+1
            CALL EFOPSEQ(IE2BFIL,BREPT,3,IER)
            currentfile=BREPT
            WRITE(OUTS,*) 'Zone ',IZI
            CALL EDISP(IUOUT,OUTS)
            CALL EDISP(IUOUT,'Returning start time to BEMS')
            WRITE(IE2BFIL,*) '# Zone ',IZI
            WRITE(IE2BFIL,*) '#Start heating at:'
            ISIM=ISIMUL
            WRITE(IE2BFIL,*) OPSSTIM(IZI)-
     &(float(ISIM)/float(isbnstep))
            write(outs,'(a,f7.3)')'#Start heating at:',OPSSTIM(IZI)-
     &(float(ISIM)/float(isbnstep))
            call edisp(iuout,outs)
            RESULTG=.TRUE.
          ELSE
          ENDIF
  30    CONTINUE


C Check for global success in controlling all zones.
        IF(RESULTG)THEN
           CALL EDISP(IUOUT,'Returned optimum start ...')
           STOP
        ELSE
           GOTO 25
        ENDIF

      ELSE

        CALL EDISP(IUOUT,'Scanres: error, not a known mode')
        STOP

      ENDIF

      RETURN

1     call edisp(iuout,'Error opening BEMS file!.')
      STOP
      END

C ********************* CTLUPDT ****************************************
C Updates the problem control files with the latest information passed from the BEMS via
C the BEMS update file. Control law start times and dates of validity. 

      SUBROUTINE CTLUPDATE(ISIM,MODE)

# include "b2e.h"
# include "building.h"

      COMMON/OUTIN/IUOUT,IUIN  
      COMMON/FILEP/IFIL
      COMMON/Vld20/Vldtng

C Commom for the optimim start/stop simulation. 
      COMMON/OPPSIM/NOPSSPT,OPSSPT(MCOM),NOPSSTIM,OPSSTIM(MCOM),
     &NOPSSPCP,OPSSPCP(MCOM),OPSSPST(MCOM)

      COMMON/B2EI3/MSIM,NZINT,IZINT(MCOM)

      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave

      COMMON/RBEMDAT/SIMTSTP,NLINE,NCOLS,CURTM,BVAR(MBROW,MBCOL),
     &SENMAP(MBCOL)

C Commons for control variables
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)

C Control law mapping
      common/cctl/icascf(mcom)
      
      INTEGER SENMAP
      CHARACTER*4 MODE, MSIM

C Set validation logical true so user not prompted when control file updated.
      logical Vldtng, today,tclose
      Vldtng=.true.

C Firstly read the control file
      ICTLF=IFIL+1 
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,'b2e reading in the control file ...')
      CALL EZCTLR(ICTLF,0,IUOUT,IER)
      IF(IER.NE.0) THEN
        CALL EDISP(IUOUT,'b2e ERROR reading control file')
        call epwait
        call epagend
        STOP
      ENDIF

C Handcraft a control file for the particular mode here.

      IF(MODE.EQ.'OPSS') THEN
        CALL EDAY(isstday(1),isstmon(1),ip1d)

C Check whether simulation is performed for current or future day
        today=.false.
        IF(INT(CURTM).EQ.ip1d) today=.true.

C Create a control file for the optimum start stop. 
C Sensor and actuator location on zone of interest.
        DO 10 IZ=1,NZINT
          IZI=IZINT(IZ)
          icascf(IZI)=IZ

          ibsn(IZ,1)=IZI 
          ibsn(IZ,2)=0 
          ibsn(IZ,3)=0 
          ibsn(IZ,4)=0
          iban(IZ,1)=-2
          iban(IZ,2)=IZI 
          iban(IZ,3)=50 
          
C 3 control day types are required
          nbcdt(IZ)=3
C Day type one (previous day) - temp match control.
          ibcdv(IZ,1,1)=1
          call eday(isstday(1),isstmon(1),ip1d)
          ibcdv(IZ,1,2)=ip1d-1
          if(today)then

C Only one control period required.
            nbcdp(IZ,1)=1
            ibctyp(IZ,1,1)=0
            ibclaw(IZ,1,1)=11
            tbcps(IZ,1,1)=0.0

C Misc data for temp match control.
            bmiscd(IZ,1,1,1)=10.0
            bmiscd(IZ,1,1,2)=100000.0
            bmiscd(IZ,1,1,3)=0.0
            bmiscd(IZ,1,1,4)=100000.0
            bmiscd(IZ,1,1,5)=0.0
            bmiscd(IZ,1,1,6)=1.0
            bmiscd(IZ,1,1,7)=1.0
            bmiscd(IZ,1,1,8)=-5.00000
            bmiscd(IZ,1,1,9)=1.00000
            bmiscd(IZ,1,1,10)=0.00000
            bmiscd(IZ,1,1,11)=0.00000
          else
            nbcdp(IZ,1)=2
            ibctyp(IZ,1,1)=0
            ibclaw(IZ,1,1)=11
            tbcps(IZ,1,1)=0.0

C Misc data for temp match control.
            bmiscd(IZ,1,1,1)=10.0
            bmiscd(IZ,1,1,2)=100000.0
            bmiscd(IZ,1,1,3)=0.0
            bmiscd(IZ,1,1,4)=100000.0
            bmiscd(IZ,1,1,5)=0.0
            bmiscd(IZ,1,1,6)=1.0
            bmiscd(IZ,1,1,7)=1.0
            bmiscd(IZ,1,1,8)=-5.00000
            bmiscd(IZ,1,1,9)=1.00000
            bmiscd(IZ,1,1,10)=0.00000
            bmiscd(IZ,1,1,11)=0.00000

C Misc data for free float control 
            ibctyp(IZ,1,2)=0
            ibclaw(IZ,1,2)=2
            tbcps(IZ,1,2)=(CURTM-INT(CURTM))*24.
            bmiscd(IZ,1,2,1)=0.0
          endif
          
C Day two - two or three or four periods required
C Work out the number of required control periods
          ibcdv(IZ,2,1)=ip1d
          ibcdv(IZ,2,2)=ip1d
          if(today) then 

C Four control periods required.
            nbcdp(IZ,2)=4

C Temp match period
            ibctyp(IZ,2,1)=0
            ibclaw(IZ,2,1)=11
            tbcps(IZ,2,1)=0.0

C Misc data for temp match control.
            bmiscd(IZ,2,1,1)=10.0
            bmiscd(IZ,2,1,2)=100000.0
            bmiscd(IZ,2,1,3)=0.0
            bmiscd(IZ,2,1,4)=100000.0
            bmiscd(IZ,2,1,5)=0.0
            bmiscd(IZ,2,1,6)=1.0
            bmiscd(IZ,2,1,7)=1.0
            bmiscd(IZ,2,1,8)=-5.00000
            bmiscd(IZ,2,1,9)=1.00000
            bmiscd(IZ,2,1,10)=0.00000
            bmiscd(IZ,2,1,11)=0.00000

C Free float period
            ibctyp(IZ,2,2)=0
            ibclaw(IZ,2,2)=2
            tbcps(IZ,2,2)=(CURTM-INT(CURTM))*24.
            bmiscd(IZ,2,2,1)=0.0

C Ideal control period. 
            ibctyp(IZ,2,3)=0
            ibclaw(IZ,2,3)=1
            tbcps(IZ,2,3)=OPSSTIM(IZI)-(float(ISIM)/float(isbnstep))
            OPSSPST(IZ)=tbcps(IZ,2,3)

            bmiscd(IZ,2,3,1)=6.0  
            bmiscd(IZ,2,3,2)=OPSSPCP(IZI)
            bmiscd(IZ,2,3,3)=0.0  
            bmiscd(IZ,2,3,4)=0.0
            bmiscd(IZ,2,3,5)=0.0  
            bmiscd(IZ,2,3,6)=OPSSPT(IZI) 
            bmiscd(IZ,2,3,7)= 99.0  

C Final Free float period
            ibctyp(IZ,2,4)=0
            ibclaw(IZ,2,4)=2
            tbcps(IZ,2,4)=OPSSTIM(IZI)
            bmiscd(IZ,2,4,1)=0.0

C Check control periods do not clash.
            call eclose(tbcps(IZ,2,3),tbcps(IZ,2,2),0.0001,tclose)
            if(tbcps(IZ,2,2).gt.tbcps(IZ,2,3).or.tclose)then
              tbcps(IZ,2,3)=tbcps(IZ,2,2)+(1./float(isbnstep))
            endif
            call eclose(tbcps(IZ,2,3),tbcps(IZ,2,4),0.0001,tclose)
            if(tbcps(IZ,2,3).gt.tbcps(IZ,2,4).or.tclose)then
              tbcps(IZ,2,3)=tbcps(IZ,2,4)-(1./float(isbnstep))
            endif

          else 

C Three control periods required.
            nbcdp(IZ,2)=3

C Free float period
            ibctyp(IZ,2,1)=0
            ibclaw(IZ,2,1)=2
            tbcps(IZ,2,1)=0.0
            bmiscd(IZ,2,1,1)=0.0

C Ideal control period. 
            ibctyp(IZ,2,2)=0
            ibclaw(IZ,2,2)=1
            tbcps(IZ,2,2)=OPSSTIM(IZI)-(float(ISIM)/float(isbnstep))
            OPSSPST(IZ)=tbcps(IZ,2,2)

            bmiscd(IZ,2,2,1)=6.0  
            bmiscd(IZ,2,2,2)=OPSSPCP(IZI)
            bmiscd(IZ,2,2,3)=0.0  
            bmiscd(IZ,2,2,4)=0.0
            bmiscd(IZ,2,2,5)=0.0  
            bmiscd(IZ,2,2,6)=OPSSPT(IZI) 
            bmiscd(IZ,2,2,7)= 99.0  

C Final Free float period
            ibctyp(IZ,2,3)=0
            ibclaw(IZ,2,3)=2
            tbcps(IZ,2,3)=OPSSTIM(IZI)
            bmiscd(IZ,2,3,1)=0.0

C Check for control period overlap. 
            call eclose(tbcps(IZ,2,2),tbcps(IZ,2,1),0.0001,tclose)
            if(tbcps(IZ,2,2).lt.tbcps(IZ,2,1).or.tclose)then
              tbcps(IZ,2,2)=tbcps(IZ,2,1)+(1./float(isbnstep))
            endif
            call eclose(tbcps(IZ,2,2),tbcps(IZ,2,3),0.0001,tclose)
            if(tbcps(IZ,2,2).gt.tbcps(IZ,2,3).or.tclose)then
              tbcps(IZ,2,2)=tbcps(IZ,2,3)-(1./float(isbnstep))
            endif


          endif

C Day three - one period required (free float)
          ibcdv(IZ,3,1)=ip1d+1
          ibcdv(IZ,3,2)=365
          nbcdp(IZ,3)=1
          ibctyp(IZ,3,1)=0
          ibclaw(IZ,3,1)=2
          tbcps(IZ,3,1)=0.0
          bmiscd(IZ,3,1,1)=0.0

C Update the zone-control links 
  10    CONTINUE
      ELSE
        CALL EDISP(IUOUT,'CTLUPDATE ERROR - unknown mode type!')
        STOP
      ENDIF



C Write out the control file. 
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,'b2e updating the control file ...')

      CALL CTLWRT(ICTLF,IER)

      RETURN 
      END



