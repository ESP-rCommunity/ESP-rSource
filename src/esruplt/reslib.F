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
C      MZLIBF
C      MZDEL
C      MZSAVE

c ******************** MZLIBF ********************

c MZLIBF assigns any user-specified library in which
c the simulation results are stored.
c If any specified library already contains simulation
c results then a check is performed to ensure that any
c further simulation results entered relate to the same
c design information as those results already stored.
c This avoids confusion during output since information
c about each simulation is obtained from a header block
c containing all the relevant design details.
c If the simulations to be entered relate to different design
c information than simulations already stored then another
c library must be specified to which any subsequent
c simulation results will be transferred.

      SUBROUTINE MZLIBF
#include "building.h"
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/C6/INDCFG
      common/reclen/nzrl,nprl,nerl
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      common/APRES/LAPRES,IFPRE
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/FFC/IMFRES

      character LAPROB*72,LAPRES*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character outs*124,ltmp*72,H*72
      logical OK,dok

C Skip building library if plant only configuration
      IF(INDCFG.EQ.2) GOTO 121


c Test for plant.
  121 IF(INDCFG.EQ.1) GOTO 17

c If a plant library has already been assigned
c within current run of program then 'free' it.
  100 IUNIT=IFIL+3
      CALL ERPFREE(IUNIT,ISTAT)

c Now assign user-specified plant library.
  102 H(1)='The plant results library contains one '
      H(2)='or more simulation result sets. Each result '
      H(3)='set is comprised of the simulation time-series '
      H(4)='for the same building configuration but, '
      H(5)='potentially, for different periods, climate '
      H(6)='data and control. This file is passed to '
      H(7)='res for analysis. '
      H(8)=' '
      H(9)='This file is normally in the same folder '
      H(10)='as the configuration file however the user is free '
      H(11)='to choose an alternative placement. '
      ltmp=' '
      CALL EASKS(ltmp,' Plant results library name ?',
     &   ' ',72,'/tmp/esru_resp','plnt results lib',IER,11)
      IF(ltmp.EQ.' ')GOTO 102
      pfile=ltmp
      CALL FPRAND(IUNIT,ISTAT,nprl,3,PFILE)
      IF(ISTAT.LT.0)goto 102

c Does this library contain results ?
  105 IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=107)NSIM

c Response according to number of existing results sets.
      dok=.true.
      h(1)='It is possible to save several different sets of '
      h(2)='system performance to the same results file if the '
      h(3)='model is the same. If you want to add another set'
      h(4)='to the current file say yes. If you want to discard'
      h(5)='the current contents say no. '
      IF(NSIM.EQ.1)then
        CALL ASKOK(' Library contains one result set.',
     &             ' Preserve?',OK,dok,5)
      elseif(NSIM.gt.1)then
        write(outs,'(A,I3,A)')' Library contains',NSIM,' result sets.'
        CALL ASKOK(outs,' Preserve?',OK,dok,5)
      elseif(NSIM.eq.0)then
        goto 108
      endif

      IF(OK)then

c Determine if design data has been changed.
        dok=.false.
        h(1)='It is possible to save several different sets of '
        h(2)='system performance to the same results file if the '
        h(3)='model is the same. If the model has changed you are'
        h(4)='advised to save the performace data to a different'
        h(5)='file. '
          CALL ASKOK(' Is the current model different in any way',
     &  ' from the data relating to these stored results?',OK,dok,5)
        if(.NOT.OK)goto 17
        GOTO 100
      else

c Delete all simulations by setting NSIM=0.
        IREC=1
        NSIM=0
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nprl
      endif

c Write header block information to plant library.
  108 CALL MZPLS1

C Open the mass flow results file on ifil+4, if it exists check with
C user before overwriting.
        if(iflwn.ne.0) then
           IUNIT=IFIL+4
           CALL EFOPSEQ(IUNIT,LAFRES,4,IER)
           IF(IER.LT.0)THEN
             call usrmsg('Problem opening mass flow results...',
     &                LAFRES,'W')
             IER=1
             RETURN
           ENDIF
           IMFRES=IUNIT
        endif

   17 RETURN

C Error handling
 1000 WRITE(outs,'(A,I5,A)')' MZLIBF: error at record',IREC,'.'
      call edisp(iuout,outs)
      goto 17

 107  IREC=1
      NSIM=0
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nprl
      goto 108

      END

c ******************** MZDEL ********************

c MZDEL DELETES THE LAST RESULT-SET SAVED IN THE SOLUTION FILE.

      SUBROUTINE MZDEL(ISIMLP)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/reclen/nzrl,nprl,nerl

      IUNIT=IFIL+3

C READ NUMBER OF SIMULATIONS CURRENTLY HELD WITHIN SOLUTION FILE.
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM
      IF(NSIM.EQ.0)GOTO 5

C DE-INCREMENT BY 1.
      NSIM=NSIM-1

C WRITE THIS TO SOLUTION FILE.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nprl
      ISIMLP=NSIM
      GOTO 2

C NO SIMULATIONS IN SOLUTION FILE PRIOR TO ATTEMPTED DELETION.
    5 ISIMLP=-1
    2 RETURN
 1000 call elinc(2)
      WRITE(IUOUT,6)IREC
    6 FORMAT(' MZDEL: plant results library error' /
     &       ' (record number',I4,').')
      call epwait
      GOTO 2
      END
c ******************** MZSAVE ********************

c MZSAVE SAVES THE LAST IMULATION RESULTS SET PERMANENTLY IN THE
c SOLUTION FILE. FAILURE TO SELECT THE '4: Save' COMMAND FROM THE
c 'SIMUL' MENU IMMEDIATELY AFTER ANY SIMULATION IS COMPLETE RESULTS
c IN THE OVERWRITING OF THESE RESULTS.

      SUBROUTINE MZSAVE(ISIMN1,ISIMN2)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/reclen/nzrl,nprl,nerl

      IUNIT=IFIL+3
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM
      NSIM=NSIM+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nprl
      ISIMN2=NSIM
    2 RETURN
 1000 call elinc(1)
      WRITE(IUOUT,4)IREC
    4 FORMAT(' MZSAVE: error in plant results file (record',I4,').')
      call epwait
      GOTO 2
      END

