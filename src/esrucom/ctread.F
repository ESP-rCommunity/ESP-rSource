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

C ******************** CTREAD ********************
C Reads in a contaminant .ctm file.  Most checks on validaty and range
C of parameters are made here.

C Variables used are:
C NOCNTM: = 0 if contaminant network not defined otherwise = 1
C NCONTM: number of contaminants
C CNCAA : Ambient concentration kg/kg of air
C GERATA: Generation rate within each INOD kg/s
C RECCOA: Recovery coefficient within each INOD kg/s
C FILEFA: Filter efficiency
C FORCAB: First order chemical reaction rate constant /s
C CONTMNAM: Array holding names of contaminants
C NTSTEPC: Timestep at which contaminant simulation runs (should be
C significantly lower than building side timestep)
C NSPMNO: Number of sources/sinks
C SSLINK1: Holds sources/sinks associated with contaminants
C SSLINK2: Holds sources/sinks associated with nodes
C SPMTYP: Holds source/sink type
C SPMSUP: Supplementary data items for each source/sink
C SSNAME: Source/sink name
C CNCNI: Initial contaminant concentration in each node (may be
C different from ambient concentration)

      SUBROUTINE CTREAD(ier)
#include "net_flow.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/ER1/IER1
      COMMON/FILEP/IFIL
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,SSLINK1(MSPMNO,MCONTM)
      COMMON/CONTM6/CNCNI(MCONTM,MNOD)

      CHARACTER*124 OUTSTR,CNTMDESC
      CHARACTER CNTMFIL*72,LAPROB*72
      CHARACTER CONTMNAM*12,COMPNAM*12,CMNAM*12,LTPCMP*60,SSNAME*12
      CHARACTER*12 CONTM1,CONTM2,PHRASE
      INTEGER SPMTYP,SSLINK1,SSLINK2
      REAL SPMSUP

      IER1=0
      IER=0
      IUNIT=IFIL+72
      NCONTM=0
      NOCNTM=0

      CALL EFOPSEQ(IUNIT,CNTMFIL,0,IER)
      IF(IER.NE.0) GOTO 1009

C Check if valid flow network file is referenced
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,'flow network file',IER)
      IF(IER.NE.0) GOTO 1009
c      IF(MFK.EQ.1)THEN
        IF(OUTSTR(1:LNBLNK(OUTSTR)).NE.LAPROB(1:lnblnk(LAPROB)))then
          call edisp(iuout,'Reference to flow network in ctm file is:')
          call edisp(iuout,OUTSTR)
          call edisp(iuout,'Flow network in configuration file is:')
          call edisp(iuout,LAPROB)
C          GOTO 1011  ! In GTK LAPROB seems to have a trailing /
        ENDIF
C << code to update contaminant file if flow network is not valid
C << it might look to see if there were more than one node/component/connection
c      ENDIF

C Skip section header
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_1') GOTO 1012

C Read number of contaminants
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      K=0
      CALL EGETWI(OUTSTR,K,NCONTM,0,MCONTM,'F','no of contaminants',IER)
      IF(IER.NE.0) GOTO 1009
      IF(NCONTM.EQ.0) GOTO 1013

C Read number of timesteps per hour
      CALL EGETWI(OUTSTR,K,NTSTEPC,1,120,'F','no of timesteps',IER)
      IF(IER.NE.0) GOTO 1009
C      IF(NTSTEPC.EQ.0) GOTO 1013

C Set flag indicating presence of contaminant file
      NOCNTM=1

C Skip section header
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_2') GOTO 1014

C Read contaminant name and ambient concentrations
      DO 10 ICONTM=1,NCONTM
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,'contaminant name',IER)
        IF(IER.NE.0) GOTO 1009
        K=0
        CALL EGETW(OUTSTR,K,PHRASE,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        CONTMNAM(ICONTM)=PHRASE

C Now get ambient concentrations.
C << if this is reading a long list of numbers we could use an
C << alternative technique to fill the array.
        DO 987 IT=1,MT
          IF(IT.EQ.1.OR.IT.EQ.13)THEN
            CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,'ambient conc',IER)
            IF(IER.NE.0) GOTO 1009
            K=0
          ENDIF
          CALL EGETWR(OUTSTR,K,CNCAA(ICONTM,IT),0.0,1.0,'-',' ',IER)
          IF(IER.NE.0) GOTO 1009
 987    CONTINUE

C Now get source/sink models 
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,'ambient conc',IER)
        IF(IER.NE.0) GOTO 1009
        K=0
        CALL EGETWI(OUTSTR,K,NSSNO,0,MSPMNO,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009

C Fill common block for sources/sinks associated with each contaminant
        IF(NSSNO.EQ.0) THEN
          CONTINUE
        ELSE
          DO 188 ISSNO=1,NSSNO
            CALL EGETWI(OUTSTR,K,N,0,MSPMNO,'F',' ',IER)
            SSLINK1(ISSNO,ICONTM)=N
 188      CONTINUE
        ENDIF

 10   CONTINUE

C Skip section header
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_3') GOTO 1015

C Skip contaminant names if rates are defined otherwise 
C this is SECTION_4
  7   CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).EQ.'SECTION_4') THEN
        GOTO 2000

C Now read first order chemical reaction rate constants
      ELSE
        IF(IER.NE.0) GOTO 1009
        K=0
        CALL EGETW(OUTSTR,K,CONTM1,'F',' ',IER)
        CALL EGETW(OUTSTR,K,CONTM2,'F',' ',IER)
        CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'-',' ',IER)

        DO 191 ICONTM=1,NCONTM
          IF(CONTMNAM(ICONTM).EQ.CONTM1)ICONTM1=ICONTM
          IF(CONTMNAM(ICONTM).EQ.CONTM2)ICONTM2=ICONTM
 191    CONTINUE
        FORCAB(ICONTM1,ICONTM2)=READOFF
        GOTO 7
      ENDIF

C Now go on to node based information
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_4') GOTO 1016

C Determine number of internal nodes (KNOD)
 2000 KNOD=0
      DO 12 INOD=1,NNOD
        IF(NDTYP(INOD).LT.2) KNOD=KNOD+1
 12   CONTINUE

C Read node based information contaminant wise
      DO 13 ICONTM=1,NCONTM

C Skip contaminant name
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
        IF(IER.NE.0) GOTO 1009

C Now read node name,number,initial concentration,
        DO 14 JNOD=1,KNOD
          CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
          IF(IER.NE.0) GOTO 1009
          K=0
          CALL EGETW(OUTSTR,K,PHRASE,'F',' ',IER)
          IF(IER.NE.0) GOTO 1009
          CALL EGETWI(OUTSTR,K,INOD,0,MNOD,'F',' ',IER)
          IF(IER.NE.0) GOTO 1009
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'-',' ',IER)
          IF(IER.NE.0) GOTO 1009

C Set initial concnetration for INOD              
          CNCNI(ICONTM,INOD)=READOFF
 14     CONTINUE
 13   CONTINUE

C Now go on to component based information
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_5') GOTO 1019

C Read number of components having efficiencies other than unity
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      K=0
      CALL EGETWI(OUTSTR,K,NCMPT,0,MCMP,'F',' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(NCMPT.EQ.0) GOTO 20
      DO 15 ICMPT=1,NCMPT
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
        K=0
        CALL EGETW(OUTSTR,K,PHRASE,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        CALL EGETWI(OUTSTR,K,ICONTM,0,NCONTM,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        CALL EGETW(OUTSTR,K,PHRASE,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        COMPNAM=PHRASE(1:LNBLNK(PHRASE))
        CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        DO 16 ICNN=1,NCNN
          IF(COMPNAM.EQ.CMNAM(ICNN))THEN
          FILEFA(ICONTM,ICNN)=READOFF
          goto 16
          ENDIF
 16     CONTINUE
 15   CONTINUE

C Now go on to sources and sinks
 20   CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_6') GOTO 1020

C Read number of sources/sinks
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NSPMNO,0,MSPMNO,'F',' ',IER)
      IF(IER.NE.0) GOTO 1009

C Read name and number of source/sink
      DO 19 ISPMNO=1,NSPMNO
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' source/sink name ',IER)
        K=0
        CALL EGETW(OUTSTR,K,PHRASE,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        SSNAME(ISPMNO)=PHRASE

C Skip source/sink number and get source/sink type
        CALL EGETWI(OUTSTR,K,N,0,MSPMNO,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        CALL EGETWI(OUTSTR,K,N,0,MSRC,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        SPMTYP(ISPMNO)=N

C Read Supplementary data items depending upon type
        IF(N.EQ.1) THEN
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,1)=READOFF
        ELSEIF(N.EQ.2) THEN
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,1)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,2)=READOFF
        ELSEIF(N.EQ.3) THEN
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,1)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.0,10.0,'W',' ',IER)
          SPMSUP(ISPMNO,2)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,31.,'W',' ',IER)
          SPMSUP(ISPMNO,3)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,12.,'W',' ',IER)
          SPMSUP(ISPMNO,4)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.,24.,'W',' ',IER)
          SPMSUP(ISPMNO,5)=READOFF
        ELSEIF(N.EQ.4) THEN
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,1)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.0,2.0,'W',' ',IER)
          SPMSUP(ISPMNO,2)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.0,100.0,'W',' ',IER)
          SPMSUP(ISPMNO,3)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.0,20.0,'W',' ',IER)
          SPMSUP(ISPMNO,4)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.0,10.0,'W',' ',IER)
          SPMSUP(ISPMNO,5)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,6)=READOFF
        ELSEIF(N.EQ.5) THEN
          CALL EGETWR(OUTSTR,K,READOFF,0.0,1.0,'W',' ',IER)
          SPMSUP(ISPMNO,1)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,31.,'F',' ',IER)
          SPMSUP(ISPMNO,2)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,12.,'F',' ',IER)
          SPMSUP(ISPMNO,3)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.,24.,'F',' ',IER)
          SPMSUP(ISPMNO,4)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,31.,'F',' ',IER)
          SPMSUP(ISPMNO,5)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,12.,'F',' ',IER)
          SPMSUP(ISPMNO,6)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.,24.,'F',' ',IER)
          SPMSUP(ISPMNO,7)=READOFF
        ELSEIF(N.EQ.6) THEN
          CALL EGETWR(OUTSTR,K,READOFF,-2.,6.,'F',' ',IER)
          SPMSUP(ISPMNO,1)=READOFF
          IF(READOFF.LT.0.)GOTO 23
          CALL EGETWR(OUTSTR,K,READOFF,1.,31.,'F',' ',IER)
          SPMSUP(ISPMNO,2)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,12.,'F',' ',IER)
          SPMSUP(ISPMNO,3)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.,24.,'F',' ',IER)
          SPMSUP(ISPMNO,4)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,31.,'F',' ',IER)
          SPMSUP(ISPMNO,5)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,12.,'F',' ',IER)
          SPMSUP(ISPMNO,6)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,0.,24.,'F',' ',IER)
          SPMSUP(ISPMNO,7)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,1.,20.,'W',' ',IER)
          SPMSUP(ISPMNO,8)=READOFF
          CALL EGETWR(OUTSTR,K,READOFF,50.,1000.,'W',' ',IER)
          SPMSUP(ISPMNO,9)=READOFF
 23     ENDIF
        IF(IER.NE.0) GOTO 1017
 19   CONTINUE

C Now go on to source/sink linkage with nodes
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
      IF(IER.NE.0) GOTO 1009
      IF(OUTSTR(1:9).NE.'SECTION_7') GOTO 1021

C Determine number of internal nodes (KNOD)
      KNOD=0
      DO 201 INOD=1,NNOD
        IF(NDTYP(INOD).LT.2) KNOD=KNOD+1
 201  CONTINUE

C Now read sources/sinks linked with the nodes
      DO 202 JNOD=1,KNOD
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
        IF(IER.NE.0) GOTO 1009
        K=0
        CALL EGETW(OUTSTR,K,PHRASE,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        CALL EGETWI(OUTSTR,K,INOD,0,MNOD,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        CALL EGETWI(OUTSTR,K,NSSNO,0,MSPMNO,'F',' ',IER)
        IF(IER.NE.0) GOTO 1009
        IF(NSSNO.EQ.0)GOTO 202
        DO 203 ISSNO=1,NSSNO
          CALL EGETWI(OUTSTR,K,N,0,MSPMNO,'F',' ',IER)
          IF(IER.NE.0) GOTO 1009
          SSLINK2(ISSNO,INOD)=N
 203    CONTINUE
 202  CONTINUE      

C Close contaminant file
      CALL ERPFREE(IUNIT,ISTAT)

      RETURN

C Error Messages
 1009 CALL USRMSG(' error reading contaminant file',' ','W')
      ier=1
      RETURN
c 1011 CALL USRMSG('The flow network file on which the contaminant',
c     &  'model is based has been changed/derefernced','W')
c      ier=1
c      RETURN
 1012 CALL USRMSG(' problem reading no. of contaminants',' ','W')
      ier=1
      RETURN
 1013 CALL USRMSG(' no contaminants defined',' ','W')
      ier=1
      RETURN
 1014 CALL USRMSG(' problem reading ambient concentrations',' ','W')
      ier=1
      RETURN
 1015 CALL USRMSG(' problem reading chemical rate constants',' ','W')
      ier=1
      RETURN
 1016 CALL USRMSG(' problem reading node based information',' ','W')
      ier=1
      RETURN
 1017 CALL USRMSG(' prob reading source sink suppl data items',' ','W')
      ier=1
      RETURN
 1019 CALL USRMSG(' problem reading filter efficiencies',' ','W')
      ier=1
      RETURN
 1020 CALL USRMSG(' problem reading source/sink models',' ','W')
      ier=1
      RETURN
 1021 CALL USRMSG(' problem reading source/sink - node links',' ','W')
      ier=1
      RETURN
      END
