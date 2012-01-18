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
C      MZPLS1
C      MZPLS2
C      MZPLS3
C      MZPLS4

C Layout of the plant results library
C Record (number or range start:end), contents (variable name)  # comments
C  1          NSIM # number of simulaton sets
C  2          IRCPLP # start record of the CURRENT result set
C  3:31       NST, IRCPLP # pointer to period information, pointer to data start record
C  32         -2,ireccfg  # index to signal it is plant library and pointer to cfg file name
C             older files will have -1 in this record
C  33         NCOMP,NSUR,NELT,NWIN,NDOOR  # to match zone results data fields
C  34:+nrecs  NPCDAT() # a sequence of nrecs data lines for the components
C  +2         lclim  # climate file (over two records)
C  +2         lpcdb  # plant template file (over two records)
C  +2         LCFGF  # model configuration file (over two records)
C irec=NST    ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,irdact,IS,SCNAME,IDAVER
C irec=NST+1  # nothing on this record so as to match zone library layout
C irec=NST+2  LCTLF  # control file if it exists (one record)
C irec=IRCPLP ?:? CSV1M(),CSV2M(),CSV3M() for each node, continue on subsequent records
C irec= ?:?   spcaout()  # array of additional output data

C ******************** MZPLS1 ********************

C MZPLS1 has the same function as MZLS1; namely the transfer
C of header information to the plant results database.
C At present this data is cursory.

      SUBROUTINE  MZPLS1
#include "building.h"
#include "plant.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPCDB/IPCDB (for plant template database)
C ICLIM,LCLIM (for climate file)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/LIBRCP/IRCPLP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      common/C21/IFCFG,cfgroot,LCFGF

      character firstbit*72,secondbit*72
      character cfgroot*24,longtfile*144,LCFGF*72
      logical concat
      integer ireccfg   ! where the cfg file name starts.

      IUNIT=IFIL+3
      firstbit=' '   ! to represent 1:72 of string
      secondbit=' '   ! to represent 73:144 of string
      longtfile=' '   ! make sure not a null string
      ireccfg=0       ! we do not yet know where cfg is written

C -1 or -2 in record 32 tells res that the results
C database is for the plant and not building. The -2
C signals that long file names are included as well as
C the model configuration file.
      IREC=32
      IXX=-2
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) IXX
      IREC=IREC+1

C Plant results database format is identical to a building database.
C We assume one "building zone", NPCOMP "surfaces" each representing
C a plant component, ME "elements per surface" each element holding
C resp. temperature, 1th and 2nd phase mass flow rate for a plant
C component node, and one window, and one door.
      NCOMP=1   ! consider renaming so no clash with common c1
      NSUR=NPCOMP
      NELT=ME
      NWIN=1
      NDOOR=1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) NCOMP,NSUR,NELT,
     &                                           NWIN,NDOOR
      IREC=IREC+1

C Write components record entry number in data base in support of
C later results analysis. Note that each record holds
C a maximum of NPRECL items. So for more items use next
C record(s).
C First find required number of records.
      nrecs=npcomp/nprecl+1

C Debug.
C      write(6,*) nrecs,npcomp,nprecl

C Initialise first item number and number of items left
C respectively.
      items=1
      iteml=npcomp

C Start writing data, perhaps using more than one record.
      do 10 ir=1, nrecs
         if(iteml.le.nprecl) then
            iteme=npcomp
         else
            iteme=items+nprecl-1
         endif
         WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &                          (NPCDAT(i,2),i=items,iteme)
         IREC=IREC+1
         items=nprecl*ir+1
         iteml=npcomp-items+1
   10 continue

C << If the climate file and the plant component databases
C << are going to be 144 characters the file needs to be
C << EITHER opened at 36 records wide or we need two records
C << to write each string

C Store climate file name.
      write(firstbit,'(a)')LCLIM(1:72)
      write(secondbit,'(a)')LCLIM(73:144)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) firstbit
      IREC=IREC+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) secondbit
      IREC=IREC+1

C Store plant component data base file name.
      write(firstbit,'(a)')lpcdb(1:72)
      write(secondbit,'(a)')lpcdb(73:144)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) firstbit
      IREC=IREC+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) secondbit
      IREC=IREC+1
      
C Add the full path to the model configuration file and then write
C the first and 2nd halfs of the file name.
      call addpath(LCFGF,longtfile,concat)
      if(concat)then
        call edisp(iuout,' ')
        call edisp(iuout,'Simulation run with configuration file: ')
        call edisp(iuout,longtfile)
        call edisp(iuout,' ')
      endif
      write(firstbit,'(a)')longtfile(1:72)
      write(secondbit,'(a)')longtfile(73:144)

C Debug.
C      write(6,*) firstbit
C      write(6,*) secondbit

      ireccfg=irec       ! we know where cfg is written
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999)firstbit
      IREC=IREC+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999)secondbit
      IREC=IREC+1

C Set start address for current results set as well as the location of
C the cfg file name as 2nd item on record 32.
      IRCPLP=IREC

C Debug.
C      write(6,*) ' start for current results set ',IRCPLP,ireccfg

      IREC=2
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999)IRCPLP
      IREC=32
      IXX=-2
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) IXX,ireccfg

  100 RETURN

C Error handling.
  999 WRITE(IUOUT,*) ' MZPLS1: plant results library error',
     &               ' at record ',IREC
      call epwait
      GOTO 100

      END

C ******************** MZPLS2 ********************

C MZPLS2 writes the simulation control file name to the plant
C results library. This information is stored in a header
C block associated with the current simulation save set.

      SUBROUTINE MZPLS2
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PCTSTP/NTSTPP
      COMMON/LIBRCP/IRCPLP

      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/AVRAGE/IDAVER

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact
      common/rdcplt/irdcp,sCSV1M(MPNODE),sCSV2M(MPNODE),
     &              sCSV3M(MPNODE),spcaout(MPCOM,MPCRES)

      CHARACTER LCTLF*72,CTLDOC*248,scname*12

C Initialize once per hour storage of results counter.
      irdcp=1


C Read number of simulation result sets currently stored in library.
      IUNIT=IFIL+3
      IREC=1
   10 READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) NSIM

C If solution library full, open new library.
      IF(NSIM.EQ.30) THEN
         WRITE(IUOUT,1010)
 1010    FORMAT(' Results library full; please specify another')
         call epwait
         CALL MZLIBF
         GOTO 10
      END IF

C Determine start address of the result-set about to be entered.
      IREC=NSIM+2
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) NST
      IREC=NST

C Now write the simulation control information to the library.
      NTS=NTSTEP*NTSTPP
      IS=3
      write(scname,'(a)')CTLDOC(1:12)
      if(irdact.gt.0) then
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) ISD1,ISM1,ISD2,ISM2,
     &  ISDS,ISDF,irdact,IS,SCNAME,IDAVER
      else
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) ISD1,ISM1,ISD2,ISM2,
     &  ISDS,ISDF,NTS,IS,SCNAME,IDAVER
      endif
      IREC=IREC+1

C In case of no configuration control file active, skip one record
C to allow match with building library for results recovery.
      IF(SCNAME(1:4).NE.'NONE'.AND.SCNAME(1:4).NE.'None'.AND.
     &   SCNAME(1:4).NE.'none')THEN
         IREC=IREC+1
         WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) LCTLF

C Debug.
C         write(6,*) 'control file written to record ',irec

         IREC=IREC+1
      ELSE
         IREC=IREC+1

C Debug.
C         write(6,*) 'skipped control file write at record ',irec

         IREC=IREC+1     
      END IF

C Transfer current record position to library and
C hold in memory for subsequent simulation result
C entry to library.
      IRCPLP=IREC
      IREC=NSIM+2
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) NST,IRCPLP

C Debug.
C      write(6,*) ' two start records are ',nst,ircplp

  100 RETURN

C Error handling.
  999 WRITE(IUOUT,*) ' MZPLS2: plant results library error',
     &               ' at record',IREC
      call epwait
      GOTO 100

      END

C ******************** MZPLS3 ********************

C MZPLS3 transfers the plant nodal states to the plant results
C library at each simulation time-step

C Library format is equivalent to a building-side save option 3.
C The first three records are reserved for miscellaneous entries
C relating to the whole network, the remaining NPCOMP records hold
C the computed state variables for each component - in order energy,
C first phase mass and second phase mass for each node in turn
C (note that at present there is only space for up to 'nprecl/3'
C  nodes)

      SUBROUTINE MZPLS3(IDAY,iflag)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/FILEP/IFIL
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTSTP/NTSTPP
      COMMON/PCTINC/IPTS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SUS/ISSNY

      COMMON/LIBRCP/IRCPLP
      COMMON/AVRAGE/IDAVER

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP

      common/csv123/CSV1M(MPNODE),CSV2M(MPNODE),CSV3M(MPNODE)
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact
      common/rdcplt/irdcp,sCSV1M(MPNODE),sCSV2M(MPNODE),
     &              sCSV3M(MPNODE),spcaout(MPCOM,MPCRES)
      logical bInStartup 

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine MZPLS3'
      END IF

C Establish if still in start-up period; if so then jump
C While in startup check if ISSNY is 1 and if IDAY is 1 reset ISSNY.
      if (bInStartup())then
        if(issny.eq.1.and.iday.eq.1) issny=0
        iflag=0
        GOTO 100
      endif

C Check if simulation spans the New Year period.
C      IF(ISSNY.EQ.1) THEN
C         IF(IDAY.EQ.365.AND.IHRP.EQ.24.AND.
C     &      ISDS.EQ.1) GOTO 10
C         IF(IDAY.EQ.1) THEN
C            ISSNY=0
C         ELSE
C            iflag=0
C            GOTO 100
C         END IF
C      END IF
C      IF(ISSNY.EQ.0.AND.(IDAY.GE.ISDS.OR.
C     &   (IDAY.EQ.ISDS-1.AND.IHRP.EQ.24))) THEN
C         GOTO 10
C      ELSE
C         iflag=0
C         GOTO 100
C      END IF

C Plant results transfer is required.
   10 IUNIT=IFIL+3
      IREC=IRCPLP

C Once per hour results storage. Determine whether storage at this
C time step is needed; ie only during last plant time step in
C last building time step within current hour.
      if(irdact.gt.0) then
        if(its.eq.ntstep.and.ipts.eq.ntstpp) then
           irdnow=1
        else
           irdnow=0
        endif
      endif

C If state variable(s) monitor flag is active then
C go and set up x & y axes for subsequent plotting.
C iflag ensures that this is done only once.
      if(monitr.eq.1.and.iflag.eq.0) then
         if(issny.eq.1) then
            call mgraph(1,isdf,0,1)
         else
            call mgraph(iday+1,isdf,0,1)
         endif
         call usrmsg(' ',' Monitoring now in progress...','-')
         iflag=1
      endif

C Establish average time-step value for temperature
C and mass flow rates, reset to future time-row value
C if averaging=off (IDAVER=1).
      DO 15 IPNOD=1,NPNOD

C Temperature.
      CSV1M(IPNOD)=(CSVF(IPNOD,1)+CSVP(IPNOD,1))*0.5
      IF(IDAVER.EQ.1) CSV1M(IPNOD)=CSVF(IPNOD,1)

C First phase mass flowrate.
      CSV2M(IPNOD)=(CSVF(IPNOD,2)+CSVP(IPNOD,2))*0.5
      IF(IDAVER.EQ.1) CSV2M(IPNOD)=CSVF(IPNOD,2)

C Second phase mass flowrate.
      CSV3M(IPNOD)=(CSVF(IPNOD,3)+CSVP(IPNOD,3))*0.5
      IF(IDAVER.EQ.1) CSV3M(IPNOD)=CSVF(IPNOD,3)
   15 CONTINUE

C Find out maximum number of nodes per record.
C The denominator is the number of state variables.
      mxnode= nprecl/3
      DO 30 IPCOMP=1,NPCOMP
         II=NPCDAT(IPCOMP,9)-1
         NNODES=NPCDAT(IPCOMP,8)

C Find number of records required.
         nrecs=(nnodes*3)/(nprecl+1)+1

C Initialise first item number and number of items left
C respectively.
         items=1
         iteml=nnodes

C Start writing data, perhaps using more than one record.
         do 200 ir=1, nrecs
            if(iteml.le.mxnode) then
               iteme=nnodes
            else
               iteme=items+mxnode-1
            endif

C Write each node's temperature, 1th and 2nd phase mass flow rate.
            if(irdact.eq.0) then
            WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &        (CSV1M(II+INOD),CSV2M(II+INOD),CSV3M(II+INOD),
     &         inod=items,iteme)
            IREC=IREC+1
            else
              do 201 inod=items,iteme
                sCSV1M(II+INOD)=sCSV1M(II+INOD)+CSV1M(II+INOD)
                sCSV2M(II+INOD)=sCSV2M(II+INOD)+CSV2M(II+INOD)
                sCSV3M(II+INOD)=sCSV3M(II+INOD)+CSV3M(II+INOD)
  201         continue
              if(irdnow.eq.1) then
                WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &            (sCSV1M(II+INOD)/irdcp,sCSV2M(II+INOD)/irdcp,
     &             sCSV3M(II+INOD)/irdcp,inod=items,iteme)
                IREC=IREC+1
                do 202 inod=items,iteme
                  sCSV1M(II+INOD)=0.
                  sCSV2M(II+INOD)=0.
                  sCSV3M(II+INOD)=0.
  202           continue
              endif
            endif
            items=mxnode*ir+1
            iteml=nnodes-items+1
  200    continue

C Find number of additional output for this component.
         naout=napdat(ipcomp)

C Reset number of additional output if more than MPCRES.
         if(naout.gt.mpcres) naout=mpcres

C Write additional output data.
         if(naout.gt.0) then
            if(irdact.eq.0) then
            write(iunit,rec=irec,iostat=istat,err=999)
     &        (pcaout(ipcomp,j),j=1,naout)
            irec=irec+1
            else
              do 301 j=1,naout
                spcaout(ipcomp,j)=spcaout(ipcomp,j)+pcaout(ipcomp,j)
  301         continue
              if(irdnow.eq.1) then
                write(iunit,rec=irec,iostat=istat,err=999)
     &            (spcaout(ipcomp,j)/irdcp,j=1,naout)
                irec=irec+1
                do 302 j=1,naout
                  spcaout(ipcomp,j)=0.
  302           continue
              endif
            endif
         endif
   30 CONTINUE

C Update storage counter if "once per hour write" active.
      if(irdact.gt.0) then
         if(irdnow.eq.1) then
           irdcp=1
         else
           irdcp=irdcp+1
         endif
      endif

C Remember plant results library start address for next result-set.
      IRCPLP=IREC

C Trace output.
  100 IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   (ITRACE(31).NE.0.OR.ITRACE(38).NE.0)) THEN
         WRITE(ITU,*) ' Subroutine MZPLS3     Trace output ',ICNT
         ICNT=ICNT+1
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Three state variables for each component/node',
     &                ' in turn:'
         DO 10100 I=1,NPCOMP
         II=NPCDAT(I,9)-1
         NNODES=NPCDAT(I,8)
         WRITE(ITU,*) (CSV1M(II+J),CSV2M(II+J),CSV3M(II+J),J=1,NNODES)
10100    CONTINUE
         IF(ITU.EQ.IUOUT) call epagew
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MZPLS3'

  500 RETURN

C Error handling.
  999 WRITE(IUOUT,*) ' MZPLS3: library error at record ',IREC
      GOTO 500

      END

C ******************** MZPLS4 ********************

C MZPLS4 saves the start address for the next result-set in
C the appropriate record (2-31) of the plant results library

      SUBROUTINE MZPLS4

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/LIBRCP/IRCPLP

      IUNIT=IFIL+3
      IREC=1

C Read number of result sets currently stored in results library.
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) NSIM

C Compute record start address of next available
C 'start address' record, unless library is already full.
      IF(NSIM.LT.29) THEN
         IREC=NSIM+3
C Save start address of next result-set.
         WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=999) IRCPLP
      ELSE
         WRITE(IUOUT,*) ' MZPLS4: plant results library is full'
         call epwait
      END IF

   10 RETURN

C Error handling.
  999 WRITE(IUOUT,*) ' MZPLS4: library error at record ',IREC
      GOTO 10

      END
