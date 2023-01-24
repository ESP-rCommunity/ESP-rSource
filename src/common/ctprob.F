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


C This file contains the following subroutines:
C CTWRIT Writes contaminant model to file
C CTPROB Main file where contaminant descriptions are defined.
C CSIACR Add, delete, edit chemical reaction information
C SCNLNK To link source/sink models with nodes and contaminants
C CSIADC Add, delete, edit source/sink models
C CTIADC Add, delete, edit contaminants
C CTLIST List contaminant information to text feedback area

C TODO LIST
C << introduction of 'cancel' option only partially implemented
C << and instantiation of data edited in a sequence should be
C << done only after all of the dialogs have been accepted.

C ******************** CTWRIT ********************
C Dump contaminant model to file.
C Definitions of most variables given in ctread.F.

      SUBROUTINE CTWRIT(IFCTM)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      integer lnblnk  ! function definition

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)
      COMMON/CONTM6/CNCNI(MCONTM,MNOD)

      CHARACTER*10 SOUTS
      CHARACTER*124 CNTMDESC,LOUTS,TOUTS
      CHARACTER CNTMFIL*72,LAPROB*72
      CHARACTER*12 CONTMNAM,SSNAME
      CHARACTER MOUTS*12
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP
      LOGICAL CLOSER

      REWIND(IFCTM,ERR=99)
      ISTAT=0

C Write airflow network file, number of contaminants
      WRITE(IFCTM,1000,IOSTAT=ISTAT,ERR=99)
 1000 FORMAT('#ESP-r contaminant file')
      WRITE(IFCTM,1001,IOSTAT=ISTAT,ERR=99)LAPROB(1:LNBLNK(LAPROB))
 1001 FORMAT(A,1X,'#fluid flow file for which this file is defined')
      WRITE(IFCTM,1002,IOSTAT=ISTAT,ERR=99)
 1002 FORMAT('SECTION_1')
      WRITE(IFCTM,1003,IOSTAT=ISTAT,ERR=99)
 1003 FORMAT('#no. of contaminants,timesteps/hour')
      WRITE(IFCTM,1004,IOSTAT=ISTAT,ERR=99)NCONTM,NTSTEPC
 1004 FORMAT(I5,4X,I3)
      WRITE(IFCTM,1005,IOSTAT=ISTAT,ERR=99)
 1005 FORMAT('SECTION_2')
      WRITE(IFCTM,1006,IOSTAT=ISTAT,ERR=99)
 1006 FORMAT('#name and ambient concentrations (kg/kg) of contaminants')
      WRITE(IFCTM,1056,IOSTAT=ISTAT,ERR=99)
 1056 FORMAT('#(max conc, min conc, hour at which max conc occurs)')
      WRITE(IFCTM,1007,IOSTAT=ISTAT,ERR=99)
 1007 FORMAT('#followed by no. of source/sink models and model no.s')

C Write name, ambient concentrations, source/sink models
      DO 500 ICONTM=1,NCONTM
        NSSLINC=0
        LOUTS=' '
        ISPMNO=1
 501    IF(SSLINK1(ISPMNO,ICONTM).NE.0)THEN
          NSSLINC=NSSLINC+1
          CALL INTSTR(SSLINK1(ISPMNO,ICONTM),SOUTS,I,J)
          IF(NSSLINC.EQ.1)THEN
            write(LOUTS,'(a)') SOUTS(1:lnblnk(SOUTS))
          ELSE

C Append to LOUTS the SSLINK1 index by using TOUTS and LOUTS
C string variables.
            TOUTS=' '
            write(TOUTS,'(a)') LOUTS(1:LNBLNK(LOUTS))
            write(LOUTS,'(3a)') TOUTS(1:LNBLNK(TOUTS)),' ',
     &        SOUTS(1:lnblnk(SOUTS))
          ENDIF
          ISPMNO=ISPMNO+1
          IF(SSLINK1(ISPMNO,ICONTM).NE.0)GOTO 501
        ENDIF
        WRITE(IFCTM,1008,IOSTAT=ISTAT,ERR=99)CONTMNAM(ICONTM)
        WRITE(IFCTM,1108,IOSTAT=ISTAT,ERR=99)(CNCAA(ICONTM,I),I=1,12)
        WRITE(IFCTM,1108,IOSTAT=ISTAT,ERR=99)(CNCAA(ICONTM,I),I=13,MT)
        WRITE(IFCTM,1208,IOSTAT=ISTAT,ERR=99)NSSLINC,LOUTS
 1008 FORMAT(A12)
 1108 FORMAT(24(F8.6,1X))
 1208 FORMAT(I3,1X,A)
 500  CONTINUE

C Write out first order chemical rate constants
      WRITE(IFCTM,1009,IOSTAT=ISTAT,ERR=99)
 1009 FORMAT('SECTION_3')
      WRITE(IFCTM,1010,IOSTAT=ISTAT,ERR=99)
 1010 FORMAT('#first order rate constants (if defined)')

C Determine how many rates are defined and write contaminants and rates
      NCHEMRR=0
      DO 503 ICONTM1=1,NCONTM
        DO 504 ICONTM2=1,NCONTM
          CALL ECLOSE(FORCAB(ICONTM1,ICONTM2),0.0,1E-14,CLOSER)
          IF(.NOT.CLOSER)THEN
            NCHEMRR=NCHEMRR+1
            WRITE(IFCTM,1011,IOSTAT=ISTAT,ERR=99)
     &      CONTMNAM(ICONTM1),CONTMNAM(ICONTM2),FORCAB(ICONTM1,ICONTM2)
 1011       FORMAT(A12,1X,A12,1X,E16.3E3)
          ENDIF
 504    CONTINUE
 503  CONTINUE

C Write out node based information
      WRITE(IFCTM,1012,IOSTAT=ISTAT,ERR=99)
 1012 FORMAT('SECTION_4')
      WRITE(IFCTM,1013,IOSTAT=ISTAT,ERR=99)
 1013 FORMAT('#node based information for contaminant')

      DO 505 ICONTM=1,NCONTM
        WRITE(IFCTM,1014,IOSTAT=ISTAT,ERR=99)CONTMNAM(ICONTM)
 1014   FORMAT(A)
        WRITE(IFCTM,1015,IOSTAT=ISTAT,ERR=99)
 1015   FORMAT('#node,node no.,initial conc.')
        DO 506 INOD=1,NNOD
          IF(NDTYP(INOD).GT.1)GOTO 506
          WRITE(IFCTM,1016,IOSTAT=ISTAT,ERR=99)
     &      NDNAM(INOD),INOD,CNCNI(ICONTM,INOD)
 1016     FORMAT(A12,1X,I3,1X,F10.6,1X)
 506    CONTINUE
 505  CONTINUE

C Write component based information
      WRITE(IFCTM,1017,IOSTAT=ISTAT,ERR=99)
 1017 FORMAT('SECTION_5')
      WRITE(IFCTM,1018,IOSTAT=ISTAT,ERR=99)
 1018 FORMAT('#component based information')
      WRITE(IFCTM,1019,IOSTAT=ISTAT,ERR=99)
 1019 FORMAT('#all filter efficiencies other than nought are listed')

C Find number of components having efficiencies other than nought
      NFILEFA=0
      DO 508 ICONTM=1,NCONTM
        DO 509 ICNN=1,NCNN
          CALL ECLOSE(FILEFA(ICONTM,ICNN),0.0,1E-8,CLOSER)
          IF(.NOT.CLOSER)NFILEFA=NFILEFA+1
 509    CONTINUE
 508  CONTINUE
      WRITE(IFCTM,1020,IOSTAT=ISTAT,ERR=99)NFILEFA
 1020 FORMAT(I4,1X,
     & '#no. of components having efficiency other than nought')
      WRITE(IFCTM,1021,IOSTAT=ISTAT,ERR=99)
 1021 FORMAT('#contaminant name and no.,component,filter efficiency')

C Write component name and filter efficiency
      DO 510 ICONTM=1,NCONTM
        DO 511 ICNN=1,NCNN
          CALL ECLOSE(FILEFA(ICONTM,ICNN),0.0,1E-8,CLOSER)
          IF(.NOT.CLOSER)THEN
            WRITE(IFCTM,1022,IOSTAT=ISTAT,ERR=99)
     &        CONTMNAM(ICONTM),ICONTM,CMNAM(ITPCON(ICNN))
     &        ,FILEFA(ICONTM,ICNN)
 1022       FORMAT(A12,1X,I2,1X,A12,1X,F3.2)
          ENDIF
 511    CONTINUE
 510  CONTINUE

C Write source/sink models data
      WRITE(IFCTM,1023,IOSTAT=ISTAT,ERR=99)
 1023 FORMAT('SECTION_6')
      WRITE(IFCTM,1024,IOSTAT=ISTAT,ERR=99)
 1024 FORMAT('#source and sink models')
      WRITE(IFCTM,1025,IOSTAT=ISTAT,ERR=99)
 1025 FORMAT('#number of sources and sinks')
      WRITE(IFCTM,1026,IOSTAT=ISTAT,ERR=99)NSPMNO
 1026 FORMAT(I3)
      WRITE(IFCTM,'(A)',IOSTAT=ISTAT,ERR=99)
     &'# Source/sink name, number, type, supplementary data items'
      DO 512 ISPMNO=1,NSPMNO
        LOUTS=' '
        IF(SPMTYP(ISPMNO).EQ.1)THEN
          NCSD=1
        ELSEIF(SPMTYP(ISPMNO).EQ.2)THEN
          NCSD=2
        ELSEIF(SPMTYP(ISPMNO).EQ.3)THEN
          NCSD=5
        ELSEIF(SPMTYP(ISPMNO).EQ.4)THEN
          NCSD=6
        ELSEIF(SPMTYP(ISPMNO).EQ.5)THEN
          NCSD=7
        ELSEIF(SPMTYP(ISPMNO).EQ.6)THEN
          NCSD=9
        ENDIF
        DO 513 ICSD=1,NCSD
          CALL RELSTR(SPMSUP(ISPMNO,ICSD),MOUTS,ISSMODL,I)
          IF(ICSD.EQ.1)THEN
            write(LOUTS,'(a)') MOUTS(1:lnblnk(MOUTS))
          ELSE

C Append to LOUTS the SSLINK1 index by using TOUTS and LOUTS
C string variables.
            TOUTS=' '
            write(TOUTS,'(a)') LOUTS(1:LNBLNK(LOUTS))
            write(LOUTS,'(3a)') TOUTS(1:LNBLNK(TOUTS)),' ',
     &        MOUTS(1:lnblnk(MOUTS))
          ENDIF
 513    CONTINUE
      WRITE(IFCTM,1027,IOSTAT=ISTAT,ERR=99)
     &SSNAME(ISPMNO),ISPMNO,SPMTYP(ISPMNO),LOUTS
 1027 FORMAT(A12,1X,I3,1X,I2,1X,A)
 512  CONTINUE

C Write source/sink models linkage with nodes
      WRITE(IFCTM,1028,IOSTAT=ISTAT,ERR=99)
 1028 FORMAT('SECTION_7')
      WRITE(IFCTM,1029,IOSTAT=ISTAT,ERR=99)
 1029 FORMAT('#source/sink linkage with nodes')
      WRITE(IFCTM,1030,IOSTAT=ISTAT,ERR=99)
 1030 FORMAT('#node,node no.,no. of source/sink models linked,model no')
      DO 514 INOD=1,NNOD
        IF(NDTYP(INOD).GT.1)GOTO 514
        NSSLINC=0
        LOUTS=' '
        ISPMNO=1
 515    IF(SSLINK2(ISPMNO,INOD).NE.0)THEN
          NSSLINC=NSSLINC+1
          CALL INTSTR(SSLINK2(ISPMNO,INOD),SOUTS,I,J)
          IF(NSSLINC.EQ.1)THEN
            write(LOUTS,'(a)') SOUTS(1:lnblnk(SOUTS))
          ELSE

C Append to LOUTS the SSLINK1 index by using TOUTS and LOUTS
C string variables.
            TOUTS=' '
            write(TOUTS,'(a)') LOUTS(1:LNBLNK(LOUTS))
            write(LOUTS,'(3a)') TOUTS(1:LNBLNK(TOUTS)),' ',
     &        SOUTS(1:lnblnk(SOUTS))
          ENDIF
          ISPMNO=ISPMNO+1
          IF(SSLINK2(ISPMNO,INOD).NE.0)GOTO 515
        ENDIF
        WRITE(IFCTM,1031,IOSTAT=ISTAT,ERR=99)
     &  NDNAM(INOD),INOD,NSSLINC,LOUTS
 1031   FORMAT(A12,1X,I3,1X,I3,1X,A)
 514  CONTINUE

 100  RETURN

C Error trap on write error
  99  CALL USRMSG(' ',
     &  ' CTWRIT: error writing contaminant model file!','W')
      CALL USRMSG(' ',' ','-')
      GOTO 100
      END

C ******************** CTPROB ********************
C Is the main file where contaminant descriptions are defined.
C This subroutine fills most I/O common blocks related to contam-
C -inant definition
C Definitions of most variables is given in ctread.F

      SUBROUTINE CTPROB
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/USER/BROWSE
      COMMON/FILEP/IFIL

      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)

      CHARACTER*124 CNTMDESC
      CHARACTER*72 CNTMFIL,CTMF
      CHARACTER CONTMNAM*12,SSNAME*12
      CHARACTER FS*1,outs*124
      CHARACTER*33 ITEMS(MCNN+20)
      CHARACTER*55 ITEML(MCNN+20)
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP
      LOGICAL OK,CLKOK,XST,CLOSER,UNIXOK
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,BROWSE,CFCDBOK

      integer NTSTEPCT   ! for local editing.
      integer NITEMS,NITEML,INO,INNO,ISO ! max items and current menu item
      integer ISTRW

C set contaminant file
      IUNIT=IFIL+72
      helpinsub='ctprob'  ! set for subroutine
      helptopic='contam_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display a help message to user explaining they should have defined a
C flow network prior to defining a contaminant model
      IF(NOCNTM.LE.0) THEN
        CALL PHELPD('contamn model def',10,'-',0,0,IER)
        CALL EASKOK(' ',
     &    'Proceed with contaminant model definition?',OK,nbhelp)
        IF(.NOT.OK) RETURN
      ENDIF

C Set folder separator (fs) to \ or / as required.
      CALL ISUNIX(UNIXOK)
      IF(UNIXOK)THEN
        FS = CHAR(47)
      ELSE
        FS = CHAR(92)
      ENDIF

C Check if a file has been defined previously
      IF(NOCNTM.GT.0) THEN
        CTMF=CNTMFIL
      ELSE
        CTMF='  '
      ENDIF

C Attempt to read in a contaminant file. If file has yet to be named
C then base it on cfgroot and place it in the netpth folder
C (differienciate between unix and non-unix machine types).
      IF(CTMF(1:2).EQ.'  '.OR.CTMF(1:4).EQ.'UNKN')THEN
        IF(UNIXOK)THEN
          IF(NETPTH(1:2).EQ.'  '.OR.NETPTH(1:2).EQ.'./')THEN
            WRITE(CTMF,'(A,A4)')CFGROOT(1:LNBLNK(CFGROOT)),'.ctm'
          ELSE
            WRITE(CTMF,'(A,A,A,A4)') NETPTH(1:LNBLNK(NETPTH)),FS,
     &      CFGROOT(1:LNBLNK(CFGROOT)),'.ctm'
          ENDIF
        ELSE
          IF(NETPTH(1:2).EQ.'  '.OR.(ICHAR(NETPTH(1:1)).EQ.46.AND.
     &       ICHAR(NETPTH(2:2)).EQ.92))THEN
            WRITE(CTMF,'(A,A4)')CFGROOT(1:LNBLNK(CFGROOT)),'.ctm'
          ELSE
            WRITE(CTMF,'(A,A,A,A4)') NETPTH(1:LNBLNK(NETPTH)),FS,
     &      CFGROOT(1:LNBLNK(CFGROOT)),'.ctm'
          ENDIF
        ENDIF
      ENDIF
      ISTRW=72
 2001 CALL EASKSCMD(CTMF,' contaminant model file ?',
     &  ' ','dereference',clkok,ISTRW,' ','contaminant model file',
     &  IER,nbhelp)
      call usrmsg(' ',' ','- ')

C If user wishes to deselect the current file name to
C blank and update the configuration file.
      IF(CLKOK)THEN
        IF(CFGOK.AND.(.NOT.BROWSE))THEN
          CNTMFIL='  '
          NCONTM=0
          NOCNTM=0
          CALL EMKCFG('-',IER)
          CALL USRMSG(' ',' ','- ')
          RETURN
        ENDIF
      ENDIF

C If specified file is a new (not created) file
      CALL FINDFIL(CTMF,XST)
      IF(.NOT.XST) THEN
        helptopic='ct_file_not_found'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' File not found.','Options:',
     &    'specify another','make new file','cancel',IW,nbhelp)
        IF(IW.EQ.1)THEN
          GOTO 2001
        ELSEIF(IW.EQ.2)THEN
          GOTO 2000
        ELSEIF(IW.EQ.3)THEN
          RETURN
        ENDIF
      ELSE
        CNTMFIL=CTMF
        outs='Scanning contaminants file...'
        call edisp(iuout,outs)
        CALL CTREAD(iier)
        if(iier.ne.0)then
          outs='There was a problem with scanning contaminants file.'
          call edisp(iuout,outs)
        else
          outs='Scanning contaminants file...done.'
          call edisp(iuout,outs)
        endif
        helptopic='ct_list_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Synopsis of contaminant model?',OK,nbhelp)
        IF(OK) THEN
          INDEX=0
          CALL CTLIST(INDEX)
        ENDIF
      ENDIF

C Setup Contaminant Model network
 2000 helptopic='ct_file_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

      INO=-3
      WRITE(ITEMS(1),'(A8,A25)') 'a file: ',CTMF(1:25)
      ITEMS(2) = 'b simulation timestep'
      ITEMS(3) = ' --------------------'
      WRITE(ITEMS(4),'(A,I3,A)')'no. of contaminants: (',NCONTM,')'
      ITEMS(5) = 'c contaminants       '
      ITEMS(6) = 'd source/sink models '
      ITEMS(7) = ' --------------------'
      ITEMS(8) = 'e chemical reactions '
      ITEMS(9) = ' --------------------'
      ITEMS(10)= 'f filters            '
      ITEMS(11)= ' --------------------'
      ITEMS(12)= '@ display information'
      ITEMS(13)= '? help               '
      ITEMS(14)= '- exit               '
      NITEMS=14
      CALL EMENU('Contaminant Description',ITEMS,NITEMS,INO)

      IF(INO.EQ.14)GOTO 5000
      IF(INO.EQ.13)THEN
        CALL PHELPD('contamn model def',nbhelp,'-',0,0,IER)
        GOTO 2000
      ELSEIF(INO.EQ.12)THEN
        INDEX=0
        CALL CTLIST(INDEX)
        GOTO 2000
      ELSEIF(INO.EQ.1) THEN
        CALL EASKOK('This will overwrite existing data!',
     &              'Proceed?',OK,nbhelp)
        IF(.NOT.OK)THEN
          GOTO 2000
        ELSE
          GOTO 2001
        ENDIF
      ELSEIF(INO.EQ.5)THEN
 2002   INNO=-2
        ITEMS(1) =' name:   ambient (max) conc:'
        ITEMS(2) ='              (kg/kg)    '
        DO 20 ICONTM=1,NCONTM
          AMBCONC=0.0
          AMBMAX=0.0
          DO 258 IT=2,MT
            AMBCONC=MAX(CNCAA(ICONTM,IT),CNCAA(ICONTM,IT-1))
            AMBMAX=MAX(AMBMAX,AMBCONC)
 258      CONTINUE
          WRITE(ITEMS(2+ICONTM),'(2A,A12,A,F8.6)')
     &      CHAR(96+ICONTM),' ',CONTMNAM(ICONTM),' ',AMBMAX
 20     CONTINUE
        ITEMS(NCONTM+3) =' ------------------------'
        ITEMS(NCONTM+4) ='+ add/delete/copy        '
        ITEMS(NCONTM+5) ='@ display information    '
        ITEMS(NCONTM+6) ='? help                   '
        ITEMS(NCONTM+7) ='- exit                   '
        NITEMS=NCONTM+7
        CALL EMENU('Contaminants',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.(NCONTM+7))GOTO 2000
        IF(INNO.EQ.(NCONTM+6))THEN
          CALL PHELPD('contaminants',nbhelp,'-',0,0,IER)
          GOTO 2002
        ELSEIF(INNO.EQ.(NCONTM+5))THEN
          INDEX=1
          CALL CTLIST(INDEX)
          GOTO 2002
        ELSEIF(INNO.EQ.(NCONTM+4))THEN
          IF(.NOT.BROWSE)THEN
            CALL EASKABCD(' ','Options:','add',
     &      'delete','copy','cancel',IADC,nbhelp)
            IF(IADC.EQ.4)THEN
              GOTO 2002
            ELSE
              CALL CTIADC(IADC,0)
              GOTO 2002
            ENDIF
          ELSE
            CALL PHELPD('contaminants',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(INNO.GT.2.AND.INNO.LT.(NCONTM+3))THEN
          IF(.NOT.BROWSE)THEN
            CALL CTIADC(0,INNO-2)
            GOTO 2002
          ELSE
            CALL PHELPD('contaminants ',nbhelp,'-',0,0,IER)
          ENDIF
        ELSE
          GOTO 2002
        ENDIF
      ELSEIF(INO.EQ.6) THEN
 2006   helptopic='ct_sources_sinks'
        call gethelptext(helpinsub,helptopic,nbhelp)
        INNO=-2
        ITEMS(1) =' Source/sink Name:     Type:'
        DO 100 ISPMNO=1,NSPMNO
          WRITE(ITEMS(ISPMNO+1),'(2A,A12,I12)')CHAR(96+ISPMNO),' ',
     &               SSNAME(ISPMNO),SPMTYP(ISPMNO)
 100    CONTINUE
        ITEMS(NSPMNO+2)=' ------------------------------- '
        ITEMS(NSPMNO+3)='+ add/delete/copy        '
        ITEMS(NSPMNO+4)='# link to contaminants   '
        ITEMS(NSPMNO+5)='! link to nodes          '
        ITEMS(NSPMNO+6)='@ display information    '
        ITEMS(NSPMNO+7)='? help                   '
        ITEMS(NSPMNO+8)='- exit                   '
        NITEMS=NSPMNO+8
        CALL EMENU('Source/Sink models',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.(NSPMNO+8))GOTO 2000
        IF(INNO.EQ.(NSPMNO+7))THEN
          CALL PHELPD('source/sink models',nbhelp,'-',0,0,IER)
          GOTO 2006
        ELSEIF(INNO.EQ.(NSPMNO+6)) THEN
          INDEX=2
          CALL CTLIST(INDEX)
          GOTO 2006
        ELSEIF(INNO.GT.1.AND.INNO.LT.(NSPMNO+2))THEN

C Use CSIADC to edit a specific source or sink (inno-1)
C The first parameter is zero (it is not used).
          IF(.NOT.BROWSE)THEN
            CALL CSIADC(0,INNO-1)
            GOTO 2006
          ELSE
            CALL PHELPD('sources-sinks',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(INNO.EQ.(NSPMNO+3))THEN

C User asked to add or delete or copy.
          IF(.NOT.BROWSE)THEN
            CALL EASKABCD(' ','Options:','add',
     &        'delete','copy','cancel',IADC,nbhelp)
            IF(IADC.EQ.4)THEN
              GOTO 2006
            ELSE

C Use CSIADC for add/delete/copy only. 2nd parameter is zero to
C disallow editing.
              CALL CSIADC(IADC,0)
              GOTO 2006
            ENDIF
          ELSE
            CALL PHELPD('sources-sinks',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(INNO.EQ.(NSPMNO+4))THEN
          IF(.NOT.BROWSE)THEN
            CALL SCNLNK(1)
            GOTO 2006
          ELSE
            CALL PHELPD('sources-sinks',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(INNO.EQ.(NSPMNO+5))THEN
          IF(.NOT.BROWSE)THEN
            CALL SCNLNK(2)
            GOTO 2006
          ELSE
            CALL PHELPD('sources-sinks',nbhelp,'-',0,0,IER)
          ENDIF
        ELSE
          GOTO 2006
        ENDIF

C Now put in chemical reaction constants if any
      ELSEIF(INO.EQ.8)THEN
 2020   helptopic='ct_constraints'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ISO=-2
        ITEML(1) ='    cntm1:      cntm2:     rate constant'
        NCHEMRR=0
C Determine number of rates if specified
        DO 501 ICONTM1=1,NCONTM
          DO 502 ICONTM2=1,NCONTM
            CALL ECLOSE(FORCAB(ICONTM1,ICONTM2),0.0,1E-14,CLOSER)
            IF(.NOT.CLOSER)THEN
              NCHEMRR=NCHEMRR+1
              WRITE(ITEML(1+NCHEMRR),'(2A1,2A12,E16.3E3)')
     &          CHAR(96+NCHEMRR),' ',
     &          CONTMNAM(ICONTM1),CONTMNAM(ICONTM2),
     &          FORCAB(ICONTM1,ICONTM2)
            ENDIF
 502      CONTINUE
 501    CONTINUE
        ITEML(NCHEMRR+2) =' ---------------------'
        ITEML(NCHEMRR+3) ='+ add/delete          '
        ITEML(NCHEMRR+4) ='@ display information '
        ITEML(NCHEMRR+5) ='? help                '
        ITEML(NCHEMRR+6) ='- exit                '
        NITEML=NCHEMRR+6
        CALL EMENU('chemical reactions',ITEML,NITEML,ISO)
        IF(ISO.EQ.(NCHEMRR+6))GOTO 2000
        IF(ISO.EQ.(NCHEMRR+5))THEN
          CALL PHELPD('chemical reactions',nbhelp,'-',0,0,IER)
          GOTO 2020
        ELSEIF(ISO.EQ.(NCHEMRR+4))THEN
          INDEX=4
          CALL CTLIST(INDEX)
          GOTO 2020
        ELSEIF(ISO.EQ.(NCHEMRR+3))THEN
          IF(.NOT.BROWSE)THEN
            CALL EASKABC(' ','Options:','add',
     &         'delete','cancel',IADC,nbhelp)
            IF(IADC.EQ.3)THEN
              GOTO 2020
            ELSE
              CALL CSIACR(IADC,0)
              GOTO 2020
            ENDIF
          ELSE
            CALL PHELPD(' ',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(ISO.LT.(NCHEMRR+2).AND.ISO.GT.1)THEN
          IF(.NOT.BROWSE)THEN
            CALL CSIACR(0,ISO)
            GOTO 2020
          ELSE
            CALL PHELPD(' ',nbhelp,'-',0,0,IER)
          ENDIF
        ELSE
          GOTO 2020
        ENDIF


C Put in filter efficiencies if any.
      ELSEIF(INO.EQ.10)THEN
 2017   helptopic='ct_filter'
        call gethelptext(helpinsub,helptopic,nbhelp)
        INNO=-2
        ITEMS(1) =' name:   ambient (max) conc:'
        ITEMS(2) ='              (kg/kg)    '
        DO 722 ICONTM=1,NCONTM
          AMBCONC=0.0
          AMBMAX=0.0
          DO 256 IT=2,MT
            AMBCONC=MAX(CNCAA(ICONTM,IT),CNCAA(ICONTM,IT-1))
            AMBMAX=MAX(AMBMAX,AMBCONC)
 256      CONTINUE
          WRITE(ITEMS(2+ICONTM),'(2A,A12,A,F8.6)')
     &      CHAR(96+ICONTM),' ',CONTMNAM(ICONTM),' ',AMBMAX
 722    CONTINUE
        ITEMS(NCONTM+3) =' ------------------------'
        ITEMS(NCONTM+4) ='@ display information    '
        ITEMS(NCONTM+5) ='? help                   '
        ITEMS(NCONTM+6) ='- exit                   '
        NITEMS=NCONTM+6
        CALL EMENU('Choose Contaminant ',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.(NCONTM+6))GOTO 2000
        IF(INNO.EQ.(NCONTM+5))THEN
          CALL PHELPD('filter efficiencies',nbhelp,'-',0,0,IER)
          GOTO 2017
        ELSEIF(INNO.EQ.(NCONTM+4))THEN
          INDEX=3
          CALL CTLIST(INDEX)
          GOTO 2017
        ELSEIF(INNO.GT.2.AND.INNO.LT.(NCONTM+3))THEN
          ICONTM=INNO-2
        ELSE
          GOTO 2017
        ENDIF

 2018   INNO=-2
        ITEML(1)='  from        to          via        efficiency'
        DO 723 ICNN=1,NCNN
          WRITE(ITEML(ICNN+1),'(2A1,3A12,1X,F3.2)')CHAR(96+ICNN),' ',
     &      NDNAM(NODPS(ICNN)),NDNAM(NODNE(ICNN)),CMNAM(ITPCON(ICNN)),
     &      FILEFA(ICONTM,ICNN)
 723    CONTINUE
        ITEML(NCNN+2) =' ------------------------'
        ITEML(NCNN+3) ='@ display information    '
        ITEML(NCNN+4) ='? help                   '
        ITEML(NCNN+5) ='- exit                   '
        NITEML=NCNN+5
        CALL EMENU('Choose connection ',ITEML,NITEML,INNO)
        IF(INNO.EQ.(NCNN+5))GOTO 2017
        IF(INNO.EQ.(NCNN+4))THEN
          CALL PHELPD('filter efficiencies',nbhelp,'-',0,0,IER)
          GOTO 2018
        ELSEIF(INNO.EQ.(NCNN+3))THEN
          INDEX=3
          CALL CTLIST(INDEX)
          GOTO 2018
        ELSEIF(INNO.GT.1.AND.INNO.LT.(NCNN+2))THEN
          IF(.NOT.BROWSE)THEN
            ICNN=INNO-1
            VAL=FILEFA(ICONTM,ICNN)
            CALL EASKR(VAL,' ','Enter filter efficiency (0 to 1)
     &        ',0.0,'F',1.0,'F',0.01,'filter eff',IER,nbhelp)
            FILEFA(ICONTM,ICNN)=VAL
            CALL EASKOK(' ',
     &        'Edit more filter efficiencies for this contaminant?',
     &        OK,nbhelp)
            IF(OK)THEN
              GOTO 2018
            ELSE
              GOTO 2017
            ENDIF
          ELSE
            CALL PHELPD(' ',nbhelp,'-',0,0,IER)
          ENDIF
        ELSE
          GOTO 2018
        ENDIF
      ELSEIF(INO.EQ.2)THEN
        helptopic='ct_timestep'
        call gethelptext(helpinsub,helptopic,nbhelp)
        NTSTEPCT=NTSTEPC
        CALL EASKI(NTSTEPCT,' ',
     &    'no of contaminant simulation timesteps / hour? ',
     &    1,'W',120,'W',12,'contaminant simul tstep ',IERI,nbhelp)
        if(ieri.eq.-3) goto 2000
        NTSTEPC=NTSTEPCT
        GOTO 2000
      ELSE
        GOTO 2000
      ENDIF
      GOTO 5000
 7000 CALL PHELPD(' ',nbhelp,'-',0,0,IER)
      GOTO 2000

C Ask if changes need to be saved
 5000 helptopic='ct_save'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Save changes?',OK,nbhelp)
      IF(OK)THEN
        IF(NTSTEPC.LE.0)GOTO 7000
   81   CALL EASKS(CTMF,' Contaminant model file ? ',
     &    ' ',72,' ','contaminant model file',IER,nbhelp)
        IF(CTMF(1:2).NE.'  '.AND.CTMF(1:4).NE.'UNKN')THEN
          CNTMFIL=CTMF
        ELSE
          GOTO 81
        ENDIF
        CALL EFOPSEQ(IUNIT,CNTMFIL,4,IER)
        IF(IER.NE.0)GOTO 81
        CALL CTWRIT(IUNIT)
        CALL ERPFREE(IUNIT,ISTAT)
        write(outs,'(2a)') ' Saved contaminant model in ',
     &    CNTMFIL(1:LNBLNK(CNTMFIL))
        CALL EDISP(IUOUT,outs)
      ENDIF

C Update the configuration file so that contmainant information is known
      helptopic='ct_flow_changes'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK('Update mass flow links to capture',
     &            'changes in the contaminant model?',OK,nbhelp)
      IF(OK)THEN
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,' Updating configuration contaminant data...')
        NOCNTM=1
        CALL EMKCFG('-',IER)
      ELSE
        RETURN
      ENDIF

      RETURN
      END

C ******************** CSIACR ********************
C Add, delete, edit chemical reaction information
C Definitions of most variables is given in ctread.F

      SUBROUTINE CSIACR(NADC,MEDIT)
#include "building.h"
#include "net_flow.h"
#include "help.h"

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)

      CHARACTER OUTS*124,CONTMNAM*12
      CHARACTER*55 ITEML(MCONTM+20)
      LOGICAL OK,EDIT,CLOSER,FOUND

      helpinsub='ctprob'  ! set for subroutine
      helptopic='contam_editing'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF(MEDIT.NE.0)THEN
        CALL EASKOK(' ','Edit this chemical reaction',OK,nbhelp)
        IF(.NOT.OK)RETURN
        EDIT=.TRUE.
        IADC=1

C Get contaminant 1 and 2 for the chemical reaction
        NCHEMRR=0
        ICONTM1=0
        ICONTM2=0
        FOUND=.FALSE.
        DO WHILE(.NOT.FOUND)
          ICONTM1=ICONTM1+1
          DO WHILE(.NOT.FOUND)
            ICONTM2=ICONTM2+1
            CALL ECLOSE(FORCAB(ICONTM1,ICONTM2),0.0,1E-14,CLOSER)
            IF(.NOT.CLOSER)THEN
              NCHEMRR=NCHEMRR+1
              IF(NCHEMRR.EQ.(MEDIT-1))THEN
                ICONTME1=ICONTM1
                ICONTME2=ICONTM2
                FOUND=.TRUE.
              ENDIF
            ENDIF
            IF(ICONTM2.GT.NCONTM)FOUND=.TRUE.
          END DO
        END DO

      ELSE
        IADC=NADC
      ENDIF

      IF(IADC.EQ.1)THEN
 2015   INNO=-2
        ITEML(1) =' name:   ambient (max) conc:'
        ITEML(2) ='              (kg/kg)    '
        DO 720 ICONTM=1,NCONTM
          AMBCONC=0.0
          AMBMAX=0.0
          DO 255 IT=2,MT
            AMBCONC=MAX(CNCAA(ICONTM,IT),CNCAA(ICONTM,IT-1))
            AMBMAX=MAX(AMBMAX,AMBCONC)
 255      CONTINUE
          WRITE(ITEML(2+ICONTM),'(2A,A12,A,F8.6)')
     &      CHAR(96+ICONTM),' ',CONTMNAM(ICONTM),' ',AMBMAX
 720    CONTINUE
        ITEML(NCONTM+3) =' ------------------------'
        ITEML(NCONTM+4) ='@ display information    '
        ITEML(NCONTM+5) ='? help                   '
        ITEML(NCONTM+6) ='- exit                   '
        NITEML=NCONTM+6
        IF(EDIT)THEN
          WRITE(OUTS,'(3A)')'Choose 1st Cntmnt (currently ',
     &         CONTMNAM(ICONTME1),' )'
          CALL EMENU(OUTS,ITEML,NITEML,INNO)
        ELSE
          CALL EMENU('Choose 1st Contaminant',ITEML,NITEML,INNO)
        ENDIF
        IF(INNO.EQ.(NCONTM+6))RETURN
        IF(INNO.EQ.(NCONTM+5))THEN
          CALL PHELPD('chem reactions',nbhelp,'-',0,0,IER)
          GOTO 2015
        ELSEIF(INNO.EQ.(NCONTM+4))THEN
          INDEX=4
          CALL CTLIST(INDEX)
          GOTO 2015
        ELSEIF(INNO.GT.2.AND.INNO.LT.(NCONTM+3))THEN
          ICTCR1=INNO-2
        ELSE
          GOTO 2015
        ENDIF

        INNO=-2
        IF(EDIT)THEN
          WRITE(OUTS,'(3A)')'Choose 2nd Cntmnt (currently ',
     &         CONTMNAM(ICONTME2),' )'
          CALL EMENU(OUTS,ITEML,NITEML,INNO)
        ELSE
          CALL EMENU('Choose 2nd Contaminant',ITEML,NITEML,INNO)
        ENDIF
        IF(INNO.EQ.(NCONTM+6))RETURN
        IF(INNO.EQ.(NCONTM+5))THEN
          CALL PHELPD('chem reactions',nbhelp,'-',0,0,IER)
          GOTO 2015
        ELSEIF(INNO.EQ.(NCONTM+4))THEN
          INDEX=4
          CALL CTLIST(INDEX)
          GOTO 2015
        ELSEIF(INNO.GT.2.AND.INNO.LT.(NCONTM+3))THEN
          ICTCR2=INNO-2
          IF(EDIT)THEN
            VAL=FORCAB(ICONTME1,ICONTME2)
          ELSE
            VAL=0.0
          ENDIF
          CALL EASKR(VAL,' ',
     &      'Enter 1st order chemical reaction rate constant(/s)',
     &      1E-14,'W',1.0,'F',0.01,'chem rate const',IER,nbhelp)
          FORCAB(ICTCR1,ICTCR2)=VAL
          FORCAB(ICTCR2,ICTCR1)=-VAL
          RETURN
        ELSE
          GOTO 2015
        ENDIF

      ELSEIF(IADC.EQ.2)THEN
 2021   INNO=-2
        ITEML(1) ='    cntm1:      cntm2:     rate constant'
        NCHEMRR=0

C Determine number of rates if specified
        DO 510 ICONTM1=1,NCONTM
          DO 520 ICONTM2=1,NCONTM
            CALL ECLOSE(FORCAB(ICONTM1,ICONTM2),0.0,1E-14,CLOSER)
            IF(.NOT.CLOSER)THEN
              NCHEMRR=NCHEMRR+1
              WRITE(ITEML(1+NCHEMRR),'(2A1,2A12,E16.3E3)')
     &          CHAR(96+NCHEMRR),' ',CONTMNAM(ICONTM1),CONTMNAM(ICONTM2)
     &          ,FORCAB(ICONTM1,ICONTM2)
            ENDIF
 520      CONTINUE
 510    CONTINUE
        ITEML(NCHEMRR+2) =' ---------------------'
        ITEML(NCHEMRR+3) ='? help'
        ITEML(NCHEMRR+4) ='- exit'
        NITEML=NCHEMRR+4
        CALL EMENU('delete chemical reactions',ITEML,NITEML,INNO)
        IF(INNO.EQ.(NCHEMRR+4))RETURN
        IF(INNO.EQ.(NCHEMRR+3))THEN
          CALL PHELPD('chemical reactions',nbhelp,'-',0,0,IER)
          GOTO 2021
        ELSEIF(INNO.LT.(NCHEMRR+2).AND.INNO.GT.1)THEN

C Get contaminant for the chemioal reaction
          NCHEMRR=0
          DO 505 ICONTM1=1,NCONTM
            DO 506 ICONTM2=1,NCONTM
              CALL ECLOSE(FORCAB(ICONTM1,ICONTM2),0.0,1E-14,CLOSER)
              IF(.NOT.CLOSER)THEN
                NCHEMRR=NCHEMRR+1
                IF(NCHEMRR.EQ.(INNO-1))THEN
                  ICONTMD1=ICONTM1
                  ICONTMD2=ICONTM2
                ENDIF
              ENDIF
 506        CONTINUE
 505      CONTINUE
          FORCAB(ICONTMD1,ICONTMD2)=0.0
        ELSE
          GOTO 2021
        ENDIF
      ENDIF

      RETURN
      END

C ******************** SCNLNK ********************
C Subroutine to link source/sink models with nodes and contaminants
C It is called with either NLP=1 (link source/sink to contaminant)
C or NLP=2 (link source/sink to node)
C Definitions of most variables is given in ctread.F

      SUBROUTINE SCNLNK(NLP)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "epara.h"
#include "help.h"

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)

      CHARACTER CONTMNAM*12,SSNAME*12,KEY*1
      CHARACTER*33 ITEMS(MCNN+20)
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP
      LOGICAL SS1,SS2

      SS1=.TRUE.
      SS2=.TRUE.
      helpinsub='ctprob'  ! set for subroutine
      helptopic='sink_links'
      call gethelptext(helpinsub,helptopic,nbhelp)

      MHEAD=1
      MCTL=3

C Linking contaminants.
      IF(NLP.EQ.1)THEN

 703    ILEN=NSPMNO
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        M=MHEAD

C Get source/sink to be linked
        INNO=-2
        ITEMS(1) =' Source Name:          type:'
        DO 100 I=1,ILEN
          if (I.GE.IST.AND.(I.LE.(IST+MIFULL))) then
            M=M+1
            call EMKEY(I,KEY,IER)
            WRITE(ITEMS(M),'(2A,A12,I12)')KEY,' ',SSNAME(I),SPMTYP(I)
          endif
 100    CONTINUE

C Number of actual items displayed.
        NITEMS=M+MCTL

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          ITEMS(M+1)=' ------------------------------- '
        ELSE
          WRITE(ITEMS(M+1),'(a,i2,a,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &      ' --------------- '
        ENDIF
        ITEMS(M+2)='? Help'
        ITEMS(M+3)='- exit'

        CALL EMENU('Choose source/sink model',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.NITEMS)THEN
          RETURN
        ELSEIF(INNO.EQ.(NITEMS-1))THEN
          CALL PHELPD('source/sink-cntmnt-node linkage',nbhelp,'-',
     &      0,0,IER)
          GOTO 703
        elseif (INNO.eq.(NITEMS-2)) then
 
C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF

        ELSEIF(INNO.GT.MHEAD.AND.INNO.LE.NITEMS)THEN

          CALL KEYIND(NITEMS,INNO,IINNO,IO)
          IDSSLN1=IINNO

        ELSE
          GOTO 703
        ENDIF

 702    ILEN=NCONTM
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        M=MHEAD

C Get contaminant to be linked.
        INNO=-2
        ITEMS(1) =' name:   ambient (max) conc:'
        DO 200 I=1,ILEN
          if (I.GE.IST.AND.(I.LE.(IST+MIFULL))) then
            M=M+1
            call EMKEY(I,KEY,IER)
            AMBCONC=0.0
            AMBMAX=0.0
            DO 254 IT=2,MT
              AMBCONC=MAX(CNCAA(I,IT),CNCAA(I,IT-1))
              AMBMAX=MAX(AMBMAX,AMBCONC)
 254        CONTINUE
            WRITE(ITEMS(M),'(2A,A12,A,F8.6)')KEY,' ',CONTMNAM(I),' ',
     &        AMBMAX
          endif
 200    CONTINUE

C Number of actual items displayed.
        NITEMS=M+MCTL

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          ITEMS(M+1)=' ------------------------------- '
        ELSE
          WRITE(ITEMS(M+1),'(a,i2,a,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &      ' --------------- '
        ENDIF
        ITEMS(M+2)='? Help'
        ITEMS(M+3)='- exit'

        CALL EMENU('Choose contaminants',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.NITEMS)THEN
          RETURN
        ELSEIF(INNO.EQ.(NITEMS-1))THEN
          CALL PHELPD('source/sink-cntmnt-node linkage',nbhelp,'-',
     &      0,0,IER)
          GOTO 702

        elseif (INNO.eq.(NITEMS-2)) then
 
C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF

        ELSEIF(INNO.GT.MHEAD.AND.INNO.LE.NITEMS)THEN

          CALL KEYIND(NITEMS,INNO,IINNO,IO)
          ICONTM=IINNO
          ISPMNO=1
          DO WHILE(SS1)
            IF(SSLINK1(ISPMNO,ICONTM).NE.0)THEN
              GOTO 704
            ELSE
              SSLINK1(ISPMNO,ICONTM)=IDSSLN1
              SS1=.FALSE.
            ENDIF
 704        ISPMNO=ISPMNO+1
          END DO
        ELSE
          GOTO 702
        ENDIF
      ELSE

 701    ILEN=NSPMNO
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        M=MHEAD

C Get source/sink to be linked.
        INNO=-2
        ITEMS(1) =' Source Name:          type:'
        DO 101 I=1,ILEN
          if (I.GE.IST.AND.(I.LE.(IST+MIFULL))) then
            M=M+1
            call EMKEY(I,KEY,IER)
            WRITE(ITEMS(M),'(2A,A12,I12)')KEY,' ',SSNAME(I),SPMTYP(I)
          endif
 101    CONTINUE

C Number of actual items displayed.
        NITEMS=M+MCTL

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          ITEMS(M+1)=' ------------------------------- '
        ELSE
          WRITE(ITEMS(M+1),'(a,i2,a,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &      ' --------------- '
        ENDIF
        ITEMS(M+2)='? Help'
        ITEMS(M+3)='- exit'

        CALL EMENU('Choose source/sink model',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.NITEMS)THEN
          RETURN
        ELSEIF(INNO.EQ.(NITEMS-1))THEN
          CALL PHELPD('source/sink-cntmnt-node linkage',nbhelp,'-',
     &      0,0,IER)
          GOTO 701

        elseif (INNO.eq.(NITEMS-2)) then
 
C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF

        ELSEIF(INNO.GT.MHEAD.AND.INNO.LE.NITEMS)THEN

          CALL KEYIND(NITEMS,INNO,IINNO,IO)
          IDSSLN2=IINNO
        ELSE
          GOTO 701
        ENDIF

 708    ILEN=NNOD
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        M=MHEAD

c Get node to be linked.
        INNO=-2
        ITEMS(1) =' Node name:'
        DO 201 I=1,ILEN
          if (I.GE.IST.AND.(I.LE.(IST+MIFULL))) then
            IF(NDTYP(I).LT.2)THEN
              M=M+1
              call EMKEY(I,KEY,IER)
              WRITE(ITEMS(M),'(2A1,A12)')KEY,' ',NDNAM(I)
            ENDIF

          endif
 201    CONTINUE

C Number of actual items displayed.
        NITEMS=M+MCTL

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          ITEMS(M+1)=' ------------------------------- '
        ELSE
          WRITE(ITEMS(M+1),'(a,i2,a,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &      ' --------------- '
        ENDIF
        ITEMS(M+2)='? Help'
        ITEMS(M+3)='- exit'

        CALL EMENU('Choose nodes',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.NITEMS)THEN
          RETURN
        ELSEIF(INNO.EQ.(NITEMS-1))THEN
          CALL PHELPD('source/sink-cntmnt-node linkage',nbhelp,'-',
     &      0,0,IER)
          GOTO 708

        elseif (INNO.eq.(NITEMS-2)) then
 
C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF

        ELSEIF(INNO.GT.MHEAD.AND.INNO.LE.NITEMS)THEN

          CALL KEYIND(NITEMS,INNO,IINNO,IO)
          INODEN=0
          DO 717 INOD=1,NNOD
            IF(NDTYP(INOD).LT.2)THEN
              INODEN=INODEN+1
              IF(INODEN.EQ.(IINNO))IJK=INOD
            ENDIF
 717      CONTINUE
          ISPMNO=1
          DO WHILE(SS2)
            IF(SSLINK2(ISPMNO,IJK).NE.0)THEN
              GOTO 707
            ELSE
              SSLINK2(ISPMNO,IJK)=IDSSLN2
              SS2=.FALSE.
            ENDIF
 707        ISPMNO=ISPMNO+1
          END DO
        ELSE
          GOTO 708
        ENDIF
      ENDIF
      RETURN
      END

C ******************** CSIADC ********************
C Add, delete, copy, edit source/sink models
C Definitions of most variables is given in ctread.F
C NADC is the action to take 1 = add, 2 = delete, 3 = copy
C ISPMNU is ??
      SUBROUTINE CSIADC(NADC,ISPMNU)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)

      CHARACTER CONTMNAM*12,SSNAME*12,T12*12,SSID*12
      CHARACTER*33 ITEMS(MCNN+20)
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP
      LOGICAL OK,EDIT
      integer iadc   ! local variable signaling edit choice.

      helpinsub='ctprob'  ! set for subroutine
      helptopic='manage_sinks_sources'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If the ispmnu parameter is non-zero then give the user the option
C to edit the contaminant details. If ispmnu is zero details should
C not be edited (just allow add copy delete).
      IF(ISPMNU.NE.0)THEN
        EDIT=.TRUE.
        IADC=1
        CALL EASKOK(' ','Edit this source/ sink?',OK,nbhelp)
        IF(.NOT.OK)RETURN
      ELSE
        EDIT=.FALSE.
        IADC=NADC
      ENDIF

      IF(IADC.EQ.1) THEN

C Add a source or sink, begin by asking for a name. And if user is
C editing then also use this facility.
        if(EDIT)then
          T12=SSNAME(ISPMNU)
        else
          T12='source'
        endif
 141    CALL EASKS(T12,' ',' source/sink name (<= 12 characters): ',12,
     &' source ','source/sink',IER,nbhelp)
        CALL ST2NAME(T12,SSID)
        DO 10 ISPMNO=1,NSPMNO
         IF((.NOT.EDIT.AND.(SSID(1:12).EQ.SSNAME(ISPMNO)(1:12))).OR.
     &     (EDIT.AND.(SSID(1:12).EQ.SSNAME(ISPMNO)(1:12)
     &     .AND.ISPMNO.NE.ISPMNU)))THEN
            CALL EASKOK('Duplicate source/ sink name!',
     &        'Retry?',OK,nbhelp)
            IF(.NOT.OK)RETURN
            GOTO 141
          ENDIF
 10     CONTINUE


C Now get source/sink type.
C Remind the user what the current source/sink type is!
        IF(EDIT) THEN
          if(SPMTYP(ISPMNU).eq.1) then
           call edisp(iuout,'Currently a constant coefficient')
          elseif(SPMTYP(ISPMNU).eq.2) then
           call edisp(iuout,'Currently a cut-off concentration')
          elseif(SPMTYP(ISPMNU).eq.3) then
           call edisp(iuout,'Currently an exponential decay/increase')
          elseif(SPMTYP(ISPMNU).eq.4) then
           call edisp(iuout,'Currently a bounday layer diffusion')
          elseif(SPMTYP(ISPMNU).eq.5) then
           call edisp(iuout,'Currently a time dependant constant mass')
          elseif(SPMTYP(ISPMNU).eq.6) then
           call edisp(iuout,'Currently a personal CO2 emission')
          else
           call edisp(iuout,'Currently undefined.')
          endif
        else
          call edisp(iuout,'Source sink type not yet defined.')
        endif

 2007   INNO=-2

        ITEMS(1) ='a constant coefficient           '
        ITEMS(2) ='b cut-off concentration          '
        ITEMS(3) ='c exponential decay/increase     '
        ITEMS(4) ='d bndry layer diffusion          '
        ITEMS(5) ='e time dependant constant mass   '
        ITEMS(6) ='f personal CO2 emission          '
        ITEMS(7) =' ------------------------------- '
        ITEMS(8) ='? help'
        ITEMS(9) ='- exit'
        NITEMS=9
        CALL EMENU('Choose Source/Sink models',ITEMS,NITEMS,INNO)
        IF(INNO.EQ.9)RETURN
        IF(INNO.EQ.8) THEN
          CALL PHELPD('source/sink models',nbhelp,'-',0,0,IER)
          GOTO 2007
        ELSEIF(INNO.GE.1.AND.INNO.LE.6)THEN

C OK source name is unique,add it to the list of names and
C increment NSPMNO
          IF(.NOT.EDIT) THEN
            IF(NSPMNO.GE.MSPMNO)THEN
              CALL EDISP(IUOUT,'No more sources/sinks allowed')
              RETURN
            ENDIF
            NSPMNO=NSPMNO+1
            SSNAME(NSPMNO)=SSID(1:12)
            ISSTYP=INNO
            SPMTYP(NSPMNO)=ISSTYP
          ELSE
            SSNAME(ISPMNU)=SSID(1:12)
            ISSTYP=INNO
            SPMTYP(ISPMNU)=ISSTYP
          ENDIF
        ELSE
          GOTO 2007
        ENDIF

C Now get supplementary data items for constant coefficient.
C << To put in checks for range checking of suppl data items
        IF(ISSTYP.EQ.1)THEN
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,1)
          CALL EASKR(VAL,' ',
     &      'Constant contaminant generation rate (kg/s)?',
     &      0.0,'W',1.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,1)=VAL
          ELSE
            SPMSUP(ISPMNU,1)=VAL
          ENDIF
        ENDIF

C Now get supplementary data items for cut-off concentration.
        IF(ISSTYP.EQ.2)THEN
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,1)
          CALL EASKR(VAL,' ',
     &      'Initial contaminant generation rate (kg/s)?',
     &      0.0,'W',1.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,1)=VAL
          ELSE
            SPMSUP(ISPMNU,1)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,2)
          CALL EASKR(VAL,' ','Cutoff concentration (kg/kg)?',
     &      0.0,'W',1.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,2)=VAL
          ELSE
            SPMSUP(ISPMNU,2)=VAL
          ENDIF
        ENDIF

C Now get supplementary data items for exponential decay/increase.
        IF(ISSTYP.EQ.3)THEN
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,1)
          CALL EASKR(VAL,' ',
     &      'Initial contaminant generation rate (kg/s)?',
     &      0.0,'W',1.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,1)=VAL
          ELSE
            SPMSUP(ISPMNU,1)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,2)
          CALL EASKR(VAL,' ','Time constant (hr)?',
     &      0.0,'W',10.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,2)=VAL
          ELSE
            SPMSUP(ISPMNU,2)=VAL
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,3))
          CALL EASKI(IV,' ','Generation start time (day of month)?',
     &        1,'W',31,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,3)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,3)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,4))
          CALL EASKI(IV,' ','Generation start time (month)?',
     &        1,'W',12,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,4)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,4)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,5))
          CALL EASKI(IV,' ','Generation start time (hour of day)?',
     &        0,'W',24,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,5)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,5)=REAL(IV)
          ENDIF

C Place to instantiate data if user did not cancel.
C << to be done. >>
        ENDIF

C Now get supplementary data items for bndry layer diffusion.
        IF(ISSTYP.EQ.4)THEN
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,1)
          CALL EASKR(VAL,' ','Average film mass transfer coeff (m/s)?',
     &      0.0,'W',1.0,'W',0.00001,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,1)=VAL
          ELSE
            SPMSUP(ISPMNU,1)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,2)
          CALL EASKR(VAL,' ','Film density of air (kg/m^3)?',
     &      1.0,'W',2.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,2)=VAL
          ELSE
            SPMSUP(ISPMNU,2)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,3)
          CALL EASKR(VAL,' ','Area of emitting surface (m^2)?',
     &      0.0,'W',100.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,3)=VAL
          ELSE
            SPMSUP(ISPMNU,3)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,4)
          CALL EASKR(VAL,' ',
     &      'Total mass of adsorbant per unit area (kg/m2)?',
     &      0.0,'W',20.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,4)=VAL
          ELSE
            SPMSUP(ISPMNU,4)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,5)
          CALL EASKR(VAL,' ','Henry adsorption/partition coefficient?',
     &      0.0,'W',10.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,5)=VAL
          ELSE
            SPMSUP(ISPMNU,5)=VAL
          ENDIF
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,6)
          CALL EASKR(VAL,' ','Initial concentration in adsorbant?',
     &      0.0,'W',1.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,6)=VAL
          ELSE
            SPMSUP(ISPMNU,6)=VAL
          ENDIF

C Place to instantiate data if user did not cancel.
C << to be done. >>
        ENDIF

C Now get supplementary data items for time dependant constant mass.
        IF(ISSTYP.EQ.5)THEN
          VAL=0.0
          if(EDIT) VAL=SPMSUP(ISPMNU,1)
          CALL EASKR(VAL,' ',
     &      'Constant contaminant generation rate (kg/s)?',
     &      0.0,'W',1.0,'W',0.01,'source/sink suppl data',IER,nbhelp)
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,1)=VAL
          ELSE
            SPMSUP(ISPMNU,1)=VAL
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,2))
          CALL EASKI(IV,' ','Generation start time (day of month)?',
     &        1,'W',31,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,2)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,2)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,3))
          CALL EASKI(IV,' ','Generation start time (month)?',
     &        1,'W',12,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,3)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,3)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,4))
          CALL EASKI(IV,' ','Generation start time (hour of day)?',
     &        0,'W',24,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,4)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,4)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,5))
          CALL EASKI(IV,' ','Generation stop time (day of month)?',
     &        1,'W',31,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,5)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,5)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,6))
          CALL EASKI(IV,' ','Generation stop time (month)?',
     &      1,'W',12,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,6)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,6)=REAL(IV)
          ENDIF
          IV=1
          if(EDIT) IV=NINT(SPMSUP(ISPMNU,7))
          CALL EASKI(IV,' ','Generation stop time (hour of day)?',
     &        0,'W',24,'W',1,'source/sink suppl data',IERI,nbhelp)
          if(ieri.eq.-3) goto 2007
          IF(.NOT.EDIT) THEN
            SPMSUP(NSPMNO,7)=REAL(IV)
          ELSE
            SPMSUP(ISPMNU,7)=REAL(IV)
          ENDIF

C Place to instantiate data if user did not cancel.
C << to be done. >>
        ENDIF

C Now get supplementary data items for personal CO2 emission.
        IF(ISSTYP.EQ.6)THEN
          helptopic='contam_co2_rate'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB(' Take metabolic rates from occupancy casual ',
     &      'gains (defined in operation files) ','yes','no',JC,nbhelp)
          IF(JC.EQ.2)THEN
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,1))
            CALL EASKI(IV,' ','Number of people ? ',
     &        1,'-',1,'-',1,'source/sink suppl data',IERI,11)
            if(ieri.eq.-3) goto 2007
            SPMSUP(NSPMNO,8)=REAL(IV)
            helptopic='contam_co2_levels'
            call gethelptext(helpinsub,helptopic,nbhelp)
            IV=1
            CALL EASKI(IV,' ','Choose activity level?',
     &        1,'F',6,'F',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,1)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,1)=REAL(IV)
            ENDIF
            IF(IV.EQ.6)THEN
              IV=100
              if(EDIT) IV=NINT(SPMSUP(ISPMNU,9))
              CALL EASKI(IV,' ','Metabolic rate (W)?',100,
     &          '-',1000,'-',1,'source/sink suppl data',IERI,nbhelp)
              if(ieri.eq.-3) goto 2007
              IF(.NOT.EDIT) THEN
                SPMSUP(NSPMNO,9)=REAL(IV)
              ELSE
                SPMSUP(ISPMNU,9)=REAL(IV)
              ENDIF
            ENDIF
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,2))
            CALL EASKI(IV,' ','Occupation start day?',
     &        1,'W',31,'W',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,2)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,2)=REAL(IV)
            ENDIF
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,3))
            CALL EASKI(IV,' ','Occupation start time month?',
     &        1,'W',12,'W',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,3)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,3)=REAL(IV)
            ENDIF
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,4))
            CALL EASKI(IV,' ','Occupation start hour?',
     &        0,'W',24,'W',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,4)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,4)=REAL(IV)
            ENDIF
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,5))
            CALL EASKI(IV,' ','Occupation stop day?',
     &        1,'W',31,'W',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,5)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,5)=REAL(IV)
            ENDIF
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,6))
            CALL EASKI(IV,' ','Occupation stop month?',
     &        1,'W',12,'W',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,6)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,6)=REAL(IV)
            ENDIF
            IV=1
            if(EDIT) IV=NINT(SPMSUP(ISPMNU,7))
            CALL EASKI(IV,' ','Occupation stop hour?',
     &        0,'W',24,'W',1,'source/sink suppl data',IERI,nbhelp)
            if(ieri.eq.-3) goto 2007
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,7)=REAL(IV)
            ELSE
              SPMSUP(ISPMNU,7)=REAL(IV)
            ENDIF
          ELSEIF(JC.EQ.1)THEN
            IF(.NOT.EDIT) THEN
              SPMSUP(NSPMNO,1)=-1.0
            ELSE
              SPMSUP(ISPMNU,1)=-1.0
            ENDIF
          ENDIF

C Place to instantiate data if user did not cancel.
C << to be done. >>
        ENDIF
      ELSEIF(IADC.EQ.2)THEN

C To delete a source/sink model.
 2011   IIO=-2
        helptopic='contam_source_delete'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ITEMS(1) =' Source Name:          type:'
        DO 110 ISPMNO=1,NSPMNO
          WRITE(ITEMS(ISPMNO+1),'(2A,A12,I12)')CHAR(96+ISPMNO),' ',
     &               SSNAME(ISPMNO),SPMTYP(ISPMNO)
 110    CONTINUE
        ITEMS(NSPMNO+2) =' ------------------------'
        ITEMS(NSPMNO+3) ='? help                   '
        ITEMS(NSPMNO+4) ='- exit                   '
        NITEMS=NSPMNO+4
        CALL EMENU('Delete source/sink ',ITEMS,NITEMS,IIO)
        IF(IIO.EQ.(NSPMNO+4))RETURN
        IF(IIO.EQ.(NCONTM+3))THEN
          CALL PHELPD('Delete source/sink',nbhelp,'-',0,0,IER)
          GOTO 2011
        ELSEIF(IIO.GT.1.AND.IIO.LT.(NSPMNO+2))THEN
          CALL EASKOK(' ','Delete source/sink model?',OK,nbhelp)
          IF(.NOT.OK)RETURN
          ISSDEL=IIO-1

C Remove this source/sink from common block elements and
C Move remaining source/sink models 'up' in relevant arrays
          IF(ISSDEL.NE.MSPMNO)THEN
            DO 36 I=ISSDEL,NSPMNO
              DO 37 ICSD=1,MCSD
                SPMSUP(I,ICSD)=SPMSUP(I+1,ICSD)
 37           CONTINUE
              SPMTYP(I)=SPMTYP(I+1)
              SSNAME(I)=SSNAME(I+1)
 36         CONTINUE
            DO 39 ISPMNO=1,MSPMNO
              DO 38 ICONTM=1,NCONTM
                IF(SSLINK1(ISPMNO,ICONTM).EQ.0)GOTO 38
                IF(SSLINK1(ISPMNO,ICONTM).EQ.ISSDEL)THEN
                  JSPMNO=ISPMNO
                  DO 40 I=JSPMNO,MSPMNO-1
                    SSLINK1(I,ICONTM)=SSLINK1(I+1,ICONTM)
 40               CONTINUE
                ENDIF
                  IF(SSLINK1(ISPMNO,ICONTM).GT.ISSDEL)THEN
                    SSLINK1(ISPMNO,ICONTM)=SSLINK1(ISPMNO,ICONTM)-1
                  ENDIF
 38           CONTINUE
              DO 41 INOD=1,NNOD
                IF(SSLINK2(ISPMNO,INOD).EQ.0)GOTO 41
                IF(SSLINK2(ISPMNO,INOD).EQ.ISSDEL)THEN
                  JSPMNO=ISPMNO
                  DO 42 I=JSPMNO,MSPMNO-1
                    SSLINK2(I,INOD)=SSLINK2(I+1,INOD)
 42               CONTINUE
                ENDIF
                  IF(SSLINK2(ISPMNO,INOD).GT.ISSDEL)THEN
                    SSLINK2(ISPMNO,INOD)=SSLINK2(ISPMNO,INOD)-1
                  ENDIF
 41           CONTINUE
 39         CONTINUE
          ELSE
            DO 33 I=1,MCSD
              SPMSUP(ISSDEL,ICSD)=0.0
 33         CONTINUE
            SPMTYP(ISSDEL)=0
            SSNAME(ISSDEL)=' '
            DO 34 INOD=1,NNOD
              SSLINK2(ISSDEL,INOD)=0
 34         CONTINUE
            DO 35 ICONTM=1,NCONTM
              SSLINK1(ISSDEL,ICONTM)=0
 35         CONTINUE
          ENDIF
          NSPMNO=NSPMNO-1
        ELSE

          GOTO 2011
        ENDIF
      ELSEIF(IADC.EQ.3)THEN

C Copy a source/sink.
 2012   IIO=-2
        helptopic='contam_source_copy'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ITEMS(1) =' Source Name:          type:'
        DO 100 ISPMNO=1,NSPMNO
          WRITE(ITEMS(ISPMNO+1),'(2A,A12,I12)')CHAR(96+ISPMNO),' ',
     &               SSNAME(ISPMNO),SPMTYP(ISPMNO)
 100    CONTINUE
        ITEMS(NSPMNO+2) =' ------------------------'
        ITEMS(NSPMNO+3) ='? help                   '
        ITEMS(NSPMNO+4) ='- exit                   '
        NITEMS=NSPMNO+4
        CALL EMENU('Copy source/sink ',ITEMS,NITEMS,IIO)
        IF(IIO.EQ.(NSPMNO+4))RETURN
        IF(IIO.EQ.(NCONTM+3))THEN
          CALL PHELPD('Copy source/sink ',nbhelp,'-',0,0,IER)
          GOTO 2012
        ELSEIF(IIO.GT.1.AND.IIO.LT.(NSPMNO+2))THEN
          CALL EASKOK(' ','Copy contaminant?',OK,nbhelp)
          IF(.NOT.OK)RETURN
          ISSCPY=IIO-1

C Get source/sink name
          T12=SSNAME(ISSCPY)
 151      CALL EASKS(T12,' ',' source/sink name (<= 12 characters): ',12
     &      ,' source ','source/sink',IER,nbhelp)
          CALL ST2NAME(T12,SSID)
          DO 9 ISPMNO=1,NSPMNO
            IF(SSID(1:12).EQ.SSNAME(ISPMNO)(1:12))THEN
              CALL EASKOK('Suplicate source/ sink name!',
     &          'Retry?',OK,nbhelp)
              IF(.NOT.OK)RETURN
              GOTO 151
            ENDIF
 9       CONTINUE

C OK source name is unique,add it to the list of names and
C increment NSPMNO
          NSPMNO=NSPMNO+1
          SSNAME(NSPMNO)=SSID(1:12)

C Copy common block information
          DO 61 ICSD=1,MCSD
            SPMSUP(NSPMNO,ICSD)=SPMSUP(ISSCPY,ICSD)
 61       CONTINUE
          SPMTYP(NSPMNO)=SPMTYP(ISSCPY)
          DO 62 INOD=1,NNOD
            SSLINK2(NSPMNO,INOD)=SSLINK2(ISSCPY,INOD)
 62       CONTINUE
          DO 63 ICONTM=1,NCONTM
            SSLINK1(NSPMNO,ICONTM)=SSLINK1(ISSCPY,ICONTM)
 63       CONTINUE
        ELSE
          GOTO 2012
        ENDIF
      ENDIF

      RETURN
      END


C ******************** CTIADC ********************
C Add, delete, copy, edit contaminants
C Definitions of most variables is given in ctread.F
C NADC takes the following values:
C 0 = edit
C 1 = add
C 2 = delete
C 3 = copy
C MEDIT is contaminant number to edit.
      SUBROUTINE CTIADC(NADC,MEDIT)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)
      COMMON/CONTM6/CNCNI(MCONTM,MNOD)

      CHARACTER OUTS*124,STR*62
      CHARACTER*12 CONTMNAM,SSNAME,T12,CNTMID,CNTMN
      CHARACTER*33 ITEMS(MCNN+20)
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP,WCONCA(MT)
      LOGICAL OK,EDIT
      integer IW  ! for radio button

      helpinsub='ctprob'  ! set for subroutine
      helptopic='contam_CTIADC'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF(MEDIT.NE.0)THEN
        EDIT=.TRUE.
        ICONTAM=MEDIT
        CALL EASKOK(' ','Edit contaminant?',OK,nbhelp)
        IF(.NOT.OK)RETURN
        IADC=1
      ELSE
        EDIT=.FALSE.
        IADC=NADC
      ENDIF
      IF(IADC.EQ.1) THEN

C Get Contaminant name and check to be unique
        IF(EDIT) THEN
          T12=CONTMNAM(ICONTAM)
        ELSE
          T12='cntmnt1'
        ENDIF
 11     CALL EASKS(T12,' ','Contaminant name (<= 12 characters)?',12,
     &' cntmnt1 ','contaminant',IER,nbhelp)
        CALL ST2NAME(T12,CNTMN)
        K=0
        CALL EGETRM(CNTMN,K,CNTMID,'W','contaminant name',IER)
        DO 10 ICONTM=1,NCONTM
          IF(CNTMID(1:12).EQ.CONTMNAM(ICONTM)(1:12).AND.ICONTM.NE.
     &    MEDIT)THEN
            CALL EASKOK('Duplicate containment name!',
     &        'Retry?',OK,nbhelp)
            IF(.NOT.OK)RETURN
            GOTO 11
          ENDIF
 10     CONTINUE

C OK contaminant name is unique,add it to the list of names and
C increment NCONTM
        IF(EDIT)THEN
          CONTMNAM(ICONTAM)=CNTMID(1:12)
        ELSE
          IF(NCONTM.GE.MCONTM)THEN
            CALL EDISP(IUOUT,'No more contaminants allowed')
            RETURN
          ENDIF
          NCONTM=NCONTM+1
          CONTMNAM(NCONTM)=CNTMID(1:12)
        ENDIF

C Ask for default concentration
        CALL EASKAB('Which best describes the average hourly',
     &    ' contaminant concentration','Variable','Constant',
     &    IHORC,nbhelp)
        IF(IHORC.EQ.1)GOTO 1
        IF(IHORC.EQ.2)GOTO 2
 1      ITIM=1
 4      WRITE(STR,'(A25,I3,A8)')'Concentration (kg/kg) at ',ITIM,
     &     ' hours?'
        CALL EASKR(CONC,' ',STR,0.,'F',0.,'-',1.,'conc',IER,nbhelp)
        IF(ITIM.LT.MT)THEN
          CALL EASKI(NTIM,' ','Til when (1-24 hours) is this valid ?',
     &     ITIM,'F',MT,'F',ITIM+1,'ambieNt concentration',IERI,nbhelp)
          if(ieri.eq.-3) then

C << to be done - return data to previous value before exit >>
            return
          endif
        ELSE
          NTIM=MT
        ENDIF
        DO 1754 IT=ITIM,NTIM
          WCONCA(IT)=CONC
 1754   CONTINUE
        ITIM=NTIM+1
        IF(NTIM.LT.24)GOTO 4
        GOTO 3
 2      VAL=CNCAA(ICONTAM,1)
        CALL EASKR(VAL,' ','Ambient concentration kg/kg ? ',
     &    0.0,'F',1.0,'F',0.01,'amb conc of contaminant',IER,nbhelp)
 3      DO 256 IT=1,MT
          IF(IHORC.EQ.2)THEN
            IF(EDIT)THEN
              CNCAA(ICONTAM,IT)=VAL
            ELSE
              CNCAA(NCONTM,IT)=VAL
            ENDIF
          ELSEIF(IHORC.EQ.1)THEN
            IF(EDIT)THEN
              CNCAA(ICONTAM,IT)=WCONCA(IT)
            ELSE
              CNCAA(NCONTM,IT)=WCONCA(IT)
            ENDIF
          ENDIF
 256    CONTINUE

C Ask for concentration of this contaminant at all internal airpoints
        IW=1
        CALL EASKATOG('Is initial conc at each node equal to ambient?',
     & ' ','yes','no','default (yes)',' ',' ',' ',' ',IW,nbhelp)
        IF(IW.EQ.1.OR.IW.EQ.3)THEN
          DO 20 INOD=1,NNOD
            IF(EDIT)THEN
              IF(IHORC.EQ.2)THEN
                CNCNI(ICONTAM,INOD)=VAL
              ELSEIF(IHORC.EQ.1)THEN
                CNCNI(ICONTAM,INOD)=-1.0
              ENDIF
            ELSE
              IF(IHORC.EQ.2)THEN
                CNCNI(NCONTM,INOD)=VAL
              ELSEIF(IHORC.EQ.1)THEN
                CNCNI(NCONTM,INOD)=-1.0
              ENDIF
            ENDIF
 20       CONTINUE
        ELSE
          DO 30 INOD=1,NNOD
            IF(NDTYP(INOD).LT.2)THEN
              WRITE(OUTS,'(A12)')NDNAM(INOD)
              IF(EDIT)VAL=CNCNI(ICONTAM,INOD)
              CALL EASKR(VAL,' Initial concentration kg/kg in ',OUTS,
     &          0.0,'F',1.0,'F',0.01,'init conc of contaminant',
     &          IER,nbhelp)
              IF(EDIT)THEN
                CNCNI(ICONTAM,INOD)=VAL
              ELSE
                CNCNI(NCONTM,INOD)=VAL
              ENDIF
            ENDIF
 30       CONTINUE
        ENDIF
      ELSEIF(IADC.EQ.2) THEN

C To delete a contaminant
 2013   IIO=-2
        ITEMS(1) =' name:  ambient (max) conc:'
        ITEMS(2) ='              (kg/kg)    '
        DO 21 ICONTM=1,NCONTM
          AMBCONC=0.0
          AMBMAX=0.0
          DO 257 IT=2,MT
            AMBCONC=MAX(CNCAA(ICONTM,IT),CNCAA(ICONTM,IT-1))
            AMBMAX=MAX(AMBMAX,AMBCONC)
 257      CONTINUE
          WRITE(ITEMS(2+ICONTM),'(2A,A12,A,F8.6)')
     &      CHAR(96+ICONTM),' ',CONTMNAM(ICONTM),' ',AMBMAX
 21     CONTINUE
        ITEMS(NCONTM+3) =' ------------------------'
        ITEMS(NCONTM+4) ='? help                   '
        ITEMS(NCONTM+5) ='- exit                   '
        NITEMS=NCONTM+5
        CALL EMENU('Delete contaminant ',ITEMS,NITEMS,IIO)
        IF(IIO.EQ.(NCONTM+5))RETURN
        IF(IIO.EQ.(NCONTM+4))THEN
          CALL PHELPD('Delete contaminant',nbhelp,'-',0,0,IER)
          GOTO 2013
        ELSEIF(IIO.GT.2.AND.IIO.LT.(NCONTM+3))THEN
          CALL EASKOK(' ','Delete contaminant',OK,nbhelp)
          IF(.NOT.OK)RETURN
          ICTDEL=IIO-2
          CONTMNAM(ICTDEL)=' '
          DO 70 IT=1,MT
            CNCAA(ICTDEL,IT)=0.0
 70       CONTINUE
          DO 31 ICNN=1,NCNN
            FILEFA(ICTDEL,ICNN)=1.0
 31       CONTINUE
          DO 40 ICONTM=1,NCONTM
            FORCAB(ICTDEL,ICONTM)=0.0
 40       CONTINUE
          DO 50 INOD=1,NNOD
            CNCNI(ICTDEL,INOD)=0.0
 50       CONTINUE
          DO 60 ISPMNO=1,NSPMNO
            SSLINK1(ISPMNO,ICTDEL)=0
 60       CONTINUE

C Shift remaining contaminants 'up' if deleted contaminant is not last
          IF(ICTDEL.NE.MCONTM)THEN
            DO 22 I=ICTDEL,NCONTM
              CONTMNAM(I)=CONTMNAM(I+1)
              DO 27 IT=1,MT
                CNCAA(I,IT)=CNCAA(I+1,IT)
 27           CONTINUE
              DO 23 ICNN=1,NCNN
                FILEFA(I,ICNN)=FILEFA(I+1,ICNN)
 23           CONTINUE
              DO 24 ICONTM=1,NCONTM
                FORCAB(I,ICONTM)=FORCAB(I+1,ICONTM)
 24           CONTINUE
              DO 25 INOD=1,NNOD
                CNCNI(I,INOD)=CNCNI(I+1,INOD)
                DO 26 ISPMNO=1,NSPMNO
                  SSLINK1(ISPMNO,I)=SSLINK1(ISPMNO,I+1)
 26             CONTINUE
 25           CONTINUE
 22         CONTINUE
          ENDIF
          NCONTM=NCONTM-1
        ELSE
          GOTO 2013
        ENDIF

C Copy a contaminant
      ELSEIF(IADC.EQ.3) THEN
 2014   IIO=-2
        ITEMS(1) =' name:   ambient (max) conc:'
        ITEMS(2) ='              (kg/kg)    '
        DO 41 ICONTM=1,NCONTM
          AMBCONC=0.0
          AMBMAX=0.0
          DO 259 IT=2,MT
            AMBCONC=MAX(CNCAA(ICONTM,IT),CNCAA(ICONTM,IT-1))
            AMBMAX=MAX(AMBMAX,AMBCONC)
 259      CONTINUE
          WRITE(ITEMS(2+ICONTM),'(2A,A12,A,F8.6)')
     &      CHAR(96+ICONTM),' ',CONTMNAM(ICONTM),' ',AMBMAX
 41     CONTINUE
        ITEMS(NCONTM+3) =' ------------------------'
        ITEMS(NCONTM+4) ='? help                   '
        ITEMS(NCONTM+5) ='- exit                   '
        NITEMS=NCONTM+5
        CALL EMENU('Copy contaminant ',ITEMS,NITEMS,IIO)
        IF(IIO.EQ.(NCONTM+5))RETURN
        IF(IIO.EQ.(NCONTM+4))THEN
          CALL PHELPD('Copy contaminant ',nbhelp,'-',0,0,IER)
          GOTO 2014
        ELSEIF(IIO.GT.2.AND.IIO.LT.(NCONTM+3))THEN
          CALL EASKOK(' ','Copy contaminant',OK,nbhelp)
          IF(.NOT.OK)RETURN
          ICTCPY=IIO-2

C Get Contaminant name and check to be unique
          T12='cntmnt1'
 121      CALL EASKS(T12,' ',' Contaminant name (<= 12 characters):',12
     &      ,' cntmnt1 ','contaminant',IER,nbhelp)
          CALL ST2NAME(T12,CNTMID)
          DO 43 ICONTM=1,NCONTM
            IF(CNTMID(1:12).EQ.CONTMNAM(ICONTM)(1:12))THEN
              CALL EASKOK('Duplicate containment name!',
     &          'Retry?',OK,nbhelp)
              IF(.NOT.OK)RETURN
              GOTO 121
            ENDIF
 43       CONTINUE

C OK contaminant name is unique,add it to the list of names and
C increment NCONTM
          NCONTM=NCONTM+1
          CONTMNAM(NCONTM)=CNTMID(1:12)
          DO 315 IT=1,MT
            CNCAA(NCONTM,IT)=CNCAA(ICTCPY,IT)
 315      CONTINUE
          DO 42 INOD=1,NNOD
            CNCNI(NCONTM,INOD)=CNCNI(ICTCPY,INOD)
 42       CONTINUE
        ELSE
          GOTO 2014
        ENDIF
      ENDIF

      RETURN
      END

C ******************** CTLIST ********************
C List (To text feedback) common block elements of contaminant model
C Definitions of most variables is given in ctread.F

      SUBROUTINE CTLIST(INDEX)

#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &      NSSNO(MNOD),SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,
     &      SSLINK1(MSPMNO,MCONTM)
      COMMON/CONTM6/CNCNI(MCONTM,MNOD)

      CHARACTER OUTS*124,CNTMDESC*124,LOUT*124,OUT*124
      CHARACTER CNTMFIL*72
      CHARACTER CONTMNAM*12,SSNAME*12
      CHARACTER SSTYPE*21,sout(mcsd)*44
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP
      LOGICAL CLOSER,RRDEF,FEDEF,SSLCDEF,SSLNDEF,ONCE,SSDEF

      RRDEF=.FALSE.
      FEDEF=.FALSE.
      SSLCDEF=.FALSE.
      SSLNDEF=.FALSE.
      SSDEF=.FALSE.

      IF(INDEX.EQ.1)GOTO 1001
      IF(INDEX.EQ.2)GOTO 1002
      IF(INDEX.EQ.3)GOTO 1003
      IF(INDEX.EQ.4)GOTO 1004

      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,' Contaminant Model ')
      CALL EDISP(IUOUT,' ')

C Display number of contaminants
      WRITE(OUTS,'(A,I5)')' Number of Contaminants = ',NCONTM
      CALL EDISP(IUOUT,OUTS)

C Contaminant side timesteps per hour
      WRITE(OUTS,'(A,I5)')' Timesteps / hour = ',NTSTEPC
      CALL EDISP(IUOUT,OUTS)
      CALL EDISP(IUOUT,' ')

C Contaminant names and ambient concentrations
 1001 IF(NCONTM.GT.0)THEN
        CALL EDISP(IUOUT,' Contaminant information:')
        DO 8 ICONTM=1,NCONTM
          CALL EDISP(IUOUT,' Name:   amb conc(max):   amb conc(min):')
          AMBMAX=CNCAA(ICONTM,1)
          AMBMIN=CNCAA(ICONTM,1)
          DO 258 IT=2,MT
            AMBCONC=CNCAA(ICONTM,IT)
            AMBMAX=MAX(AMBMAX,AMBCONC)
            AMBMIN=MIN(AMBMIN,AMBCONC)
 258      CONTINUE
          WRITE(OUTS,'(1X,A12,F8.6,A,F8.6)')CONTMNAM(ICONTM),
     &      AMBMAX,' ',AMBMIN
          CALL EDISP(IUOUT,OUTS)

C Display initial concentrations in each node
          WRITE(OUTS,'(A)')' Node name:   init. conc.:'
          CALL EDISP(IUOUT,OUTS)
          DO 9 INOD=1,NNOD
            IF(NDTYP(INOD).LT.2)THEN
              IF(CNCNI(ICONTM,INOD).GE.0.0)THEN
                WRITE(OUT,'(1X,A12,F8.6)')NDNAM(INOD),CNCNI(ICONTM,INOD)
              ELSE
                WRITE(OUT,'(1X,A12,A)')NDNAM(INOD),' Equal to ambient'
              ENDIF
              CALL EDISP(IUOUT,OUT)
            ENDIF
 9        CONTINUE
        CALL EDISP(IUOUT,' ')
 8      CONTINUE
      ELSE
        CALL EDISP(IUOUT,' No contaminants defined')
      ENDIF
      CALL EDISP(IUOUT,' ')
      IF(INDEX.NE.0)GOTO 1099


C Display First order rate constants
 1004 CALL EDISP(IUOUT,' Chemical reaction information:')
      ONCE=.TRUE.
      DO 11 ICONTM1=1,NCONTM
        DO 10 ICONTM2=1,NCONTM
          CALL ECLOSE(FORCAB(ICONTM1,ICONTM2),0.0,1E-14,CLOSER)
          IF(.NOT.CLOSER)THEN
            IF(ONCE)THEN
              OUTS=' 1st contmnt: 2nd contmnt: reaction rate:'
              CALL EDISP(IUOUT,OUTS)
              ONCE=.FALSE.
              RRDEF=.TRUE.
            ENDIF
            WRITE(OUTS,'(1X,A12,1X,A12,1X,E16.3E3)')CONTMNAM(ICONTM1),
     &      CONTMNAM(ICONTM2),FORCAB(ICONTM1,ICONTM2)
            CALL EDISP(IUOUT,OUTS)
          ENDIF
 10     CONTINUE
 11   CONTINUE
      IF(.NOT.RRDEF)CALL EDISP(IUOUT,' No chemical reactions defined')
      CALL EDISP(IUOUT,' ')
      IF(INDEX.NE.0)GOTO 1099

C Component based information
 1003 CALL EDISP(IUOUT,' Component based information:')
      CALL EDISP(IUOUT,' Only non-zero filter efficiencies are listed')
      DO 18 ICONTM=1,NCONTM
        ONCE=.TRUE.
        DO 19 ICNN=1,NCNN
          CALL ECLOSE(FILEFA(ICONTM,ICNN),0.0,0.001,CLOSER)
          IF(.NOT.CLOSER)THEN
            IF(ONCE)THEN
              WRITE(OUTS,'(A,A12)')' for contaminant: ',CONTMNAM(ICONTM)
              CALL EDISP(IUOUT,OUTS)
              CALL EDISP(IUOUT,
     &        ' from:        to:          via:         efficiency:')
              ONCE=.FALSE.
              FEDEF=.TRUE.
            ENDIF
            WRITE(OUTS,'(1X,A12,1X,A12,1X,A12,1X,F3.2)')
     &      NDNAM(NODPS(ICNN)),NDNAM(NODNE(ICNN)),CMNAM(ITPCON(ICNN)),
     &      FILEFA(ICONTM,ICNN)
            CALL EDISP(IUOUT,OUTS)
          ENDIF
 19     CONTINUE
 18   CONTINUE
      IF(.NOT.FEDEF)THEN
        OUTS=' No filter efficiencies (other than nought) present'
        CALL EDISP(IUOUT,OUTS)
      ENDIF
      CALL EDISP(IUOUT,' ')
      IF(INDEX.NE.0)GOTO 1099

C Source/sink information
 1002 CALL EDISP(IUOUT,' Source/sink information:')
      ONCE=.TRUE.
      DO 20 ISPMNO=1,NSPMNO
        IF(SPMTYP(ISPMNO).NE.0)THEN
          IF(ONCE)THEN
            CALL EDISP(IUOUT,' no: name:        type:')
            ONCE=.FALSE.
            SSDEF=.TRUE.
          ENDIF
          IF(SPMTYP(ISPMNO).EQ.1)THEN
            SSTYPE='constant coefficient'
            WRITE(OUTS,'(1X,I3,1X,A12,1X,A21)')
     &       ISPMNO,SSNAME(ISPMNO),SSTYPE
            CALL EDISP(IUOUT,OUTS)
            SOUT(1)='Constant contaminant generation rate kg/s: '
            WRITE(LOUT,'(5X,A,1X,F14.10)')SOUT(1),SPMSUP(ISPMNO,1)
            CALL EDISP(IUOUT,LOUT)
          ELSEIF(SPMTYP(ISPMNO).EQ.2)THEN
            SSTYPE='cutoff concentration'
            WRITE(OUTS,'(1X,I3,1X,A12,1X,A21)')
     &       ISPMNO,SSNAME(ISPMNO),SSTYPE
            CALL EDISP(IUOUT,OUTS)
            SOUT(1)='Initial contaminant generation rate kg/s : '
            SOUT(2)='Cutoff concentration kg/kg               : '
            DO 1302 IJ=1,2
              WRITE(LOUT,'(5X,A,1X,F14.10)')SOUT(IJ),SPMSUP(ISPMNO,IJ)
              CALL EDISP(IUOUT,LOUT)
 1302       CONTINUE
          ELSEIF(SPMTYP(ISPMNO).EQ.3)THEN
            SSTYPE='exponential model'
            WRITE(OUTS,'(1X,I3,1X,A12,1X,A21)')
     &       ISPMNO,SSNAME(ISPMNO),SSTYPE
            CALL EDISP(IUOUT,OUTS)
            SOUT(1)='Initial contaminant generation rate kg/s : '
            SOUT(2)='Time Constant hr                         : '
            SOUT(3)='Generation start time (day of month)     : '
            SOUT(4)='Generation start time (month)            : '
            SOUT(5)='Generation start time (hour of day)      : '
            DO 1303 IJ=1,5
              WRITE(LOUT,'(5X,A,1X,F14.10)')SOUT(IJ),SPMSUP(ISPMNO,IJ)
              CALL EDISP(IUOUT,LOUT)
 1303       CONTINUE
          ELSEIF(SPMTYP(ISPMNO).EQ.4)THEN
            SSTYPE='bndry layer diffusion'
            WRITE(OUTS,'(1X,I3,1X,A12,1X,A21)')
     &       ISPMNO,SSNAME(ISPMNO),SSTYPE
            CALL EDISP(IUOUT,OUTS)
            SOUT(1)='Average film mass transfer coeff m/s     : '
            SOUT(2)='Film density of air kg/m^3               : '
            SOUT(3)='Area of emitting surface m^2             : '
            SOUT(4)='Total mass of adsorbant/unit area kg/m^2 : '
            SOUT(5)='Henry adsorption/partition coefficient   : '
            SOUT(6)='Initial concentration in adsorbant kg/kg : '
            DO 1304 IJ=1,6
              WRITE(LOUT,'(5X,A,1X,F14.10)')SOUT(IJ),SPMSUP(ISPMNO,IJ)
              CALL EDISP(IUOUT,LOUT)
 1304       CONTINUE
          ELSEIF(SPMTYP(ISPMNO).EQ.5)THEN
            SSTYPE='time dependant'
            WRITE(OUTS,'(1X,I3,1X,A12,1X,A21)')
     &       ISPMNO,SSNAME(ISPMNO),SSTYPE
            CALL EDISP(IUOUT,OUTS)
            SOUT(1)='Constant contaminant generation rate kg/s: '
            SOUT(2)='Generation start time (day of month)     : '
            SOUT(3)='Generation start time (month)            : '
            SOUT(4)='Generation start time (hour of day)      : '
            SOUT(5)='Generation stop time (day of month)      : '
            SOUT(6)='Generation stop time (month)             : '
            SOUT(7)='Generation stop time (hour of day)       : '
            DO 1305 IJ=1,7
              WRITE(LOUT,'(5X,A,1X,F14.10)')SOUT(IJ),SPMSUP(ISPMNO,IJ)
              CALL EDISP(IUOUT,LOUT)
 1305       CONTINUE
          ELSEIF(SPMTYP(ISPMNO).EQ.6)THEN
            SSTYPE='personal CO2 emission'
            WRITE(OUTS,'(1X,I3,1X,A12,1X,A21)')
     &       ISPMNO,SSNAME(ISPMNO),SSTYPE
            CALL EDISP(IUOUT,OUTS)
            IF(ABS(SPMSUP(ISPMNO,1)+1.).LT.0.01)THEN
              WRITE(LOUT,'(5X,2A)')'CO2 mass injections taken from '
     &        ,'metabolic rates defined in operations file for the zone'
              CALL EDISP(IUOUT,LOUT)
            ELSE
              SOUT(1)='Activity level                         : '
              SOUT(2)='Occupation start time (day of month)   : '
              SOUT(3)='Occupation start time (month)          : '
              SOUT(4)='Occupation start time (hour of day)    : '
              SOUT(5)='Occupation stop time (day of month)    : '
              SOUT(6)='Occupation stop time (month)           : '
              SOUT(7)='Occupation stop time (hour of day)     : '
              SOUT(8)='Number of people                       : '
              SOUT(9)='Metabolic rate/person W                : '
              NSUP=8
              IF(ABS(SPMSUP(ISPMNO,1)-6.0).LT..01)NSUP=9
              DO 1306 IJ=1,NSUP
                WRITE(LOUT,'(5X,A,1X,F14.10)')SOUT(IJ),SPMSUP(ISPMNO,IJ)
                CALL EDISP(IUOUT,LOUT)
 1306         CONTINUE
            ENDIF
          ENDIF
          CALL EDISP(IUOUT,' ')
        ENDIF
 20   CONTINUE
      IF(.NOT.SSDEF)THEN
        OUTS=' No source/sink models present'
        CALL EDISP(IUOUT,OUTS)

C Source/sink models linked with each contaminant
      ELSE
        CALL EDISP(IUOUT,' ')
        ONCE=.TRUE.
        CALL EDISP(IUOUT,
     &   ' Source/sink models linked with contaminants:')
        DO 16 ICONTM=1,NCONTM
          ISPMNO=1
 17       IF(SSLINK1(ISPMNO,ICONTM).NE.0)THEN
            IF(ONCE)THEN
              CALL EDISP(IUOUT,' contmnt name: source/sink # & model:')
              ONCE=.FALSE.
              SSLCDEF=.TRUE.
            ENDIF
            WRITE(OUTS,'(1X,A12,1X,I3,1X,A12)')
     &        CONTMNAM(ICONTM),SSLINK1(ISPMNO,ICONTM),SSNAME
     &        (SSLINK1(ISPMNO,ICONTM))
            CALL EDISP(IUOUT,OUTS)
            ISPMNO=ISPMNO+1
            IF(SSLINK1(ISPMNO,ICONTM).NE.0)GOTO 17
          ENDIF
 16     CONTINUE
        IF(.NOT.SSLCDEF)THEN
          OUTS=' No source/sink linkages to contaminants present'
          CALL EDISP(IUOUT,OUTS)
        ENDIF

C Source/sink models linked with each node
        CALL EDISP(IUOUT,' ')
        ONCE=.TRUE.
        CALL EDISP(IUOUT,' Source/sink models linked with nodes:')
        DO 14 INOD=1,NNOD
          IF(NDTYP(INOD).GT.1)GOTO 14
          ISPMNO=1
 15       IF(SSLINK2(ISPMNO,INOD).NE.0)THEN
            IF(ONCE)THEN
              CALL EDISP(IUOUT,' node name:   source/sink # & model:')
              ONCE=.FALSE.
              SSLNDEF=.TRUE.
            ENDIF
            WRITE(OUTS,'(1X,A12,1X,I3,1X,A12)')
     &        NDNAM(INOD),SSLINK2(ISPMNO,INOD),SSNAME
     &        (SSLINK2(ISPMNO,INOD))
            CALL EDISP(IUOUT,OUTS)
            ISPMNO=ISPMNO+1
            IF(SSLINK2(ISPMNO,INOD).NE.0)GOTO 15
          ENDIF
 14     CONTINUE
        IF(.NOT.SSLNDEF)THEN
          OUTS=' No source/sink linkages to nodes present'
          CALL EDISP(IUOUT,OUTS)
        ENDIF
        CALL EDISP(IUOUT,' ')
      ENDIF
      IF(INDEX.NE.0)GOTO 1099

      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,' End of Contaminant Model')
      CALL EDISP(IUOUT,' ')

 1099 RETURN
      END
