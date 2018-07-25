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

C emfnetw.F holds common code for reading and listing mass flow
C descriptive files and contains the following routines:
C  EMFREAD: Reads a mass flow network description file.
C  EMF3DREAD:  reads a 3D mass flow network description file.
C Mfcmpsupcheck: check flow components for correct number of supplemental
C           data items.
C  MFERR:   Error trap routine.
C  MFLIST:  List mass flow network desciption file.
C  GETND:   Returns mass flow node index matching NAME.
C  GETCN:   Returns mass flow connection index matching two node
C           names and associated component.
C  GETCMP:  Returns mass flow component index matching name ,
C  MFWRIT:  Fluid flow model file: dump common data to file.
C  MFWRIT:  3D Fluid flow model file: dump common block data to file.

C ************************* EMFREAD 
C EMFREAD reads a mass flow network description file.
C Reads allow comments and most input data checks are performed.
C It assumes that the file is already open.
      SUBROUTINE EMFREAD(IFPRB,IER)

#include "net_flow.h"
#include "net_flow_data.h"
      
      integer lnblnk  ! function definition

C Network flow commons are as follows:
C as defined in net_flow_data.h)
C     NNOD   - number of nodes (building zones and/or plant components)
C     NCMP   - number of fluid flow components (flow restrictions)
C     NCNN   - number of interconnections (branches)
C Nodes: fixed input data
C     NDNAM  - identifier/name
C     NDFLD  - fluid type (1=air, 2=water)
C     NDTYP  - type (0=internal; unknown pressure
C                    1=internal; known total pressure
C                    2=boundary; known total pressure
C                    3=boundary; wind induced pressure; implies NDFLD=1)
C     HNOD   - XYZ position (m)
C     SUPNOD - supplementary data items (max. MNDS)
C              if NDTYP=0 none
C                 NDTYP=1 total pressure (Pa)
C                 NDTYP=2 total pressure (Pa)
C                         node fluid temperature flag, indicating:
C                         0: TNOD is constant
C                         1: TNOD equals DRYB
C                 NDTYP=3 wind pressure coefficients index
C                         surface azimuth (degrees clockwise from north)
C Nodes: variable input data
C     ITND   - node temperature index; if ITND(I)=N then TNOD(I)=TNOD(N)
C     TNOD   - node fluid temperature (C)
C Nodes: variable calculated input data
C     RHON   - node fluid density (kg/m^3)
C Components: fixed description data
C     IVALCM - valid component type numbers
C     LVALCM - short description of each valid component type
C Components: fixed input data
C     CMNAM  - identifier/name
C     ITPCMP - type number
C     LTPCMP - short description of that type
C     ISDCMP - number of supplementary data items (max. MCMS)
C     ISDCNN - number of connection level supplementary items (max. MCNS)
C     SUPCMP - component supplementary data items (1st item = fluid type)
C Connections: fixed input data
C     NODPS  - node number on positive side of connection
C     HGTPS  - height of +ve linkage point relative to NODPS (m)
C     NODNE  - node number on negative side of connection
C     HGTNE  - height of -ve linkage point relative to NODNE (m)
C     ITPCON - number of linking fluid flow component
C     NDSCNN - connection level component supplementary node numbers
C Connections: variable calculated input data and history mechanisms
C     PSTACK - stack pressure (Pa)
C     HDP    - previous iteration delta pressure across conn. (Pa)
C     HDV    - previous iteration partial derivative dF/d(dP) (kg/s/Pa)
C     HFL    - previous iteration flows through connection (kg/s)
C Output data
C     FLW1   - 1st fluid flow through connection; positive if flow
C              from NODPS to NODNE (kg/s)
C     FLW2   - 2nd fluid flow through connection (applicable in case of
C              e.g. a door); positive if flow from NODPS to NODNE (kg/s)
C     PRES   - node total pressure (Pa)
C     RESID  - node fluid mass flow residual (kg/s)
C     SAFLW  - node coupled sum of absolute mass flow rates (kg/s)
C Climate data
C     DRYB   - dry bulb temperature (C)
C     QDIF   - diffuse horizontal solar radiation (W/m^2)
C     QDNR   - direct normal solar radiation (W/m^2)
C     IRVH   - relative humidity of outdoor air (%)
C     WDIR   - wind direction (degrees clockwise from north)
C     WSPD   - wind speed (m/s)
C     WRED   - local wind speed reduction factor (-)
C Wind pressure data
C     NPRE   - number of wind pressure coefficients sets
C     FPRE   - surface wind pressure coefficients (-)
C Simulation control data
C     IRY    - year (read from climate file)
C     IRM    - month of the year
C     IRD    - day of the month
C     IRH    - hour of day
C     FLWTIM - time of day  (real for ESRUbps usage)
C     IHOUR  - time step counter (for ESRUmfs usage)
C     IYD    - day number of current day
C     IFYD   - day number of first day to be simulated
C     ILYD   - day number of last day to be simulated
C     IPROG  - simulation progress flag
C Calculation control parameters
C     IPSMOD - stack pressure difference calculation model based on:
C               1 = most recently computed flow direction
C               2 = average density of connected nodes
C     MSLVTP - matrix solver type indicator:
C               1 = Gaussian elimination with backsubstitution
C                   and no pivoting
C               2 = LU decomposition with implicit pivoting (Crout)
C Iteration control parameters
C     MAXITF - maximum number of iterations
C     FERREL - maximum relative error in flows calculation (-)
C     FERMFL - maximum absolute error in flows calculation (kg/s)
C     PMAX   - maximum pressure correction in flows calculation (Pa)
C     STEFFR - ratio of successive pressure corrections below which
C              Steffensen relaxation is used to prevent oscillations (-)
C     MFTRAC - trace output flag, indicating:
C              -1 = no trace output at all (for use in bps)
C               0 = no trace output, just indicate no. of iterations
C               1 = press., resid. and relative error worst relative node
C               2 = as 1 + pressure and residual at all nodes
C                   + pointer at worst relative and abolute node(s)
C               3 = as 2 + network matrix solving information
C     ITER   - iteration counter
C     IOK    - flag indicating successful iteration process

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

C To signal that simulation preferences (globals in graphic
C network file) are available at the end of the file.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

C Simulation preferences as per simcon.F in esrubps, to allow the user to
C alter preferences. Typically filled with standard values.
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      DIMENSION RVA(MCMS),NDID(MNOD)
      CHARACTER OUTSTR*124,WORD*20,NODID*12,NDID*12,CMPID*12,OUTS*124

C 'Reset' error indicator and clear node & component names.
      IER=0
      INOD=0; ICMP=0; ICNN=0
      do i=1,MNOD
        NDNAM(i)=' '; NODASSOC(i,1)='-'; NODASSOC(i,2)='-'
        NDFLD(i)=0; NDTYP(i)=0; ITND(i)=0; TNOD(i)=0.
        HNOD(i,1)=0.0; HNOD(i,2)=0.0; HNOD(i,3)=0.0
      enddo
      do i=1,MCMP
        CMNAM(i)=' '; CMPASSOC(i,1)='-'; CMPASSOC(i,2)='-'
        ITPCMP(i)=0; ISDCMP(i)=0; ISDCNN(i)=0; NWPCMP(i)=0
        HCMP(i,1,1)=0.0; HCMP(i,1,2)=0.0; HCMP(i,1,3)=0.0;
        HCMP(i,2,1)=0.0; HCMP(i,2,2)=0.0; HCMP(i,2,3)=0.0;
      enddo

C Read number of nodes, components, interconnections and wind reduction.
      CALL STRIPC(IFPRB,OUTSTR,99,ND,1,
     &            'mfs nodes, components & connections',IER)
      IF(IER.NE.0)RETURN
      IF(ND.GE.4)THEN
        K=0
        CALL EGETWI(OUTSTR,K,NNOD,0,MNOD,'F','no of nodes',IER)
        CALL EGETWI(OUTSTR,K,NCMP,0,MCMP,'F','no of compnt',IER)
        CALL EGETWI(OUTSTR,K,NCNN,0,MCNN,'F','no of connct',IER)
        CALL EGETWR(OUTSTR,K,WRED,0.,10.,'W','wind reduction',IER)
        IF(IER.NE.0) GOTO 1001
      ELSE
        CALL USRMSG('looking for at least 4 items in',OUTSTR,'W')
      ENDIF

C First skip node header.
C Read Node name, fluid type, node type, node height.
      CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'node definition',IER)

C If no nodes then skip the read. 
      IF(NNOD.EQ.0) GOTO 23 
      DO 19 INOD=1,NNOD
        CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'node definition',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','node name',IER)
        NDNAM(INOD)=WORD(1:LNBLNK(WORD))
        CALL EGETWI(OUTSTR,K,IV,1,2,'F','fluid type',IER)
        NDFLD(INOD)=IV
        CALL EGETWI(OUTSTR,K,IV,0,3,'F','node type',IER)
        NDTYP(INOD)=IV
        CALL EGETWR(OUTSTR,K,VAL,-1000.,1000.,'F','node ht',IER)
        HNOD(INOD,3)=VAL
        HNOD(INOD,1)=0.0; HNOD(INOD,2)=0.0  ! unknown X & Y
        IF(IER.NE.0) GOTO 1001
        CALL EGETW(OUTSTR,K,WORD,'W','node temp/ref zone',IER)
        NDID(INOD)=WORD(1:12)

C Node supplementary data items
        CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','node suppl 1',IER)
        SUPNOD(INOD,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','node suppl 2',IER)
        SUPNOD(INOD,2)=VAL
        IF(IER.NE.0) GOTO 1001
   19 CONTINUE

Check the nodal temperature.
      ITND(INOD)=0
      DO 18 I=1,NNOD
          DO 17 J=1,NNOD
C See if this node name has already been defined.
          IF(NDID(I)(1:12).EQ.NDNAM(J)(1:12)) THEN
            ITND(I)=J
          ENDIF
  17    CONTINUE
C No, it is not. Now interpret what we have as a number.
        IF(ITND(I).EQ.0) THEN
          ITND(I)=0
          read(NDID(I),*,ERR=1001)TNOD(I)
        ENDIF
  18  CONTINUE
      
C Check node data (same checks as in editing).
C Start by checking if current node does not already exist.
      INOD=0
   10 INOD=INOD+1
      I=0
   11 I=I+1
      IF(I.GT.1.AND.NDNAM(I)(1:12).EQ.NDNAM(INOD)(1:12))then
        write(outs,'(A,A,A)')' duplicate names',NDNAM(I),NDNAM(INOD)
        call edisp(iuout,outs)
        GOTO 999
      endif
      IF(I.LT.INOD-1) GOTO 11

C Check node type
      IF(NDTYP(INOD).EQ.3.AND.NDFLD(INOD).NE.1)then
        call edisp(iuout,' Cannot have water at this node!')
        GOTO 999
      endif

C Check node temperature index and/or temperature
      IF(ITND(INOD).LT.0.OR.ITND(INOD).GT.NNOD)then
        call edisp(iuout,' Node temperature linked node doesn`t exist!')
        GOTO 999
      endif
      IF(ITND(INOD).GT.0) THEN
        IF(TNOD(INOD).LT.-100..OR.TNOD(INOD).GT.1000.)then
          call edisp(iuout,' Node temperature out of range!')
          GOTO 999
        endif
      END IF

C Check boundary node supplementary data items.
      if(NDTYP(INOD).eq.3)then
        IF(SUPNOD(INOD,2).LT.0..OR.SUPNOD(INOD,2).GT.360.)then
          call edisp(iuout,' Orientation out of range!')
          GOTO 999
        endif
      endif
      IF(INOD.LT.NNOD) GOTO 10

  23  CONTINUE
C Read component data, first skip header.
      CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'component headder',IER)

      IF(NCMP.EQ.0) GOTO 37
      DO 25 ICMP=1,NCMP
        CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'component data',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','comp name',IER)
        CMNAM(ICMP)=WORD(1:12)
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','component type',IER)
        ITPCMP(ICMP)=IV 

C Find matching component text descriptor and remember it.
        IC=0
   31   IC=IC+1
        IF(ITPCMP(ICMP).EQ.IVALCM(IC)) GOTO 45
        IF(IC.LT.MCMV) GOTO 31
   45   LTPCMP(ICMP)=LVALCM(IC)

C Number of component level supplementary data items. 
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','comp supplem data',IER)
        ISDCMP(ICMP)=IV
        IF(IER.NE.0) GOTO 999

C Connection level supplementary data items.
C If not successful this is probably an early version mfs-file
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','conn supplem data',IER)
        ISDCNN(ICMP)=IV
        IF(IER.NE.0) THEN
          CALL EDISP(iuout,' ')
          CALL EDISP(iuout,
     &    ' Probably an early version mass flow network file! ')
          CALL EDISP(iuout,
     &    ' Number of connection level supplementary data items set to')
          CALL EDISP(iuout,
     &    ' zero. Please refer to manual and change file. ')
         ISDCNN(ICMP)=0
        END IF

C Read supplementary data, in one or more lines until ISDMCP(ICMP) items
C have been obtained.
        IRVA=ISDCMP(ICMP)
        IF(IRVA.GT.0) THEN
          CALL EGETWRA(IFPRB,RVA,IRVA,0.,0.,'-','conn suppl',IER)
          DO 35 KV=1,IRVA
            SUPCMP(ICMP,KV)=RVA(KV)
   35     CONTINUE
        ENDIF
   25 CONTINUE

C Check component data, beginning with possible duplication. 
      ICMP=0
   30 ICMP=ICMP+1
      I=0
   32 I=I+1
      IF(I.GT.1.AND.CMNAM(I).EQ.CMNAM(ICMP))then
        call edisp(iuout,' Duplicate component!')
        GOTO 999
      endif
      IF(I.LT.ICMP-1) GOTO 32

C Check if this is a valid component type.
      IC=0
   33 IC=IC+1
      IF(ITPCMP(ICMP).EQ.IVALCM(IC)) GOTO 34
      IF(IC.LE.MCMV) GOTO 33
      call edisp(iuout,' Invalid component type!')
      IF(ITPCMP(ICMP).EQ.450) GOTO 34
      GOTO 999

C Check component level number of data items.
   34 call mfcmpsupcheck(ICMP,IER)

      IF(ICMP.LT.NCMP) GOTO 30

  37  CONTINUE

C Read connections data, First skip header, then read line by line.
      CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'connection data',IER)
      IF(NCNN.EQ.0) GOTO 43
      DO 50 ICNN=1,NCNN
        NDSCNN(ICNN,1)=0
        NDSCNN(ICNN,2)=0
        CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'connect data',IER)
        K=0

        CALL EGETW(OUTSTR,K,WORD,'W','+ve node',IER)
        NODID=WORD(1:12)
        call GETND(NODID,iv,1,'+ve node')
        if(iv.eq.0)goto 999
        NODPS(ICNN)=iv

        CALL EGETWR(OUTSTR,K,HGTPS(ICNN),-100.,100.,'W',
     &    '+ve linkage pt',IER)
        IF(IER.NE.0)then 
          call edisp(iuout,' could not convert +ve linkage point...')
          GOTO 999
        endif

        CALL EGETW(OUTSTR,K,WORD,'W','-ve node',IER)
        NODID=WORD(1:12)
        call GETND(NODID,iv,1,'-ve node')
        if(iv.eq.0)goto 999
        NODNE(ICNN)=iv

        CALL EGETWR(OUTSTR,K,HGTNE(ICNN),-100.,100.,'W',
     &    '-ve linkage pt',IER)
        IF(IER.NE.0)then
          call edisp(iuout,' could not convert -ve linkage point...')
          GOTO 999
        endif

C Name of linking fluid flow component and its index.
        CALL EGETW(OUTSTR,K,WORD,'W','linking comp',IER)
        CMPID=WORD(1:12)
        call GETCMP(CMPID,iv,1,'linked component name')
        if(iv.eq.0)GOTO 999
        ITPCON(ICNN)=iv

C If supplementary node names for components 220, 230 240 250 read the
C first one and if necessary read the second one.
C << ? NDSCNN has been depreciated in favour of nfsup() & iasocc(). >>
        IF(ISDCNN(ITPCON(ICNN)).LT.1) GOTO 50
        CALL EGETW(OUTSTR,K,WORD,'W','suppl cnn node',IER)
        NODID=WORD(1:12)
        call GETND(NODID,iv,1,'suppl node')
        if(iv.eq.0)goto 999
        NDSCNN(ICNN,1)=iv
        IF(ISDCNN(ITPCON(ICNN)).LT.2) GOTO 50
        CALL EGETW(OUTSTR,K,WORD,'W','2nd suppl cnn node',IER)
        NODID=WORD(1:12)
        call GETND(NODID,iv,1,'2nd suppl cnn node')
        if(iv.eq.0)goto 999
        NDSCNN(ICNN,2)=iv
   50 CONTINUE

C Check connections data (perform same checks as editing code)
C Start by checking if current connection has valid nodes numbers.
      ICNN=0
   51 ICNN=ICNN+1
      IF(NODPS(ICNN).EQ.NODNE(ICNN))then
        call edisp(iuout,' Connection cannot be to same node!')
        GOTO 999
      endif
      IF(NODPS(ICNN).LT.1.OR.NODPS(ICNN).GT.NNOD)then
        call edisp(iuout,' First node in connection not known!')
        GOTO 999
      endif
      IF(NODNE(ICNN).LT.1.OR.NODNE(ICNN).GT.NNOD)then
        call edisp(iuout,' 2nd node in connection not known!')
        GOTO 999
      endif

C Check if both nodes have identical fluid type.
      IF(NDFLD(NODPS(ICNN)).NE.NDFLD(NODNE(ICNN)))then
        call edisp(iuout,' Cannot mix fluid types!')
        GOTO 999
      endif

C Check if linking flow component is a known component.
      IF(ITPCON(ICNN).LT.1.OR.ITPCON(ICNN).GT.NCMP)then
        call edisp(iuout,' Linking component unknown type!')
        GOTO 999
      endif

C Check if fluid type identical to fluid type of node on 'one' side.
      IF(NDFLD(NODPS(ICNN)).NE.INT(SUPCMP(ITPCON(ICNN),1)))then
        call edisp(iuout,' Linking component different fluid!')
        GOTO 999
      endif
      IF(ICNN.LT.NCNN) GOTO 51
    
  43  CONTINUE

C There may be supplemental data for simulation preferences. Attempt
C to read tag:data lines until the end of the file.
      CALL STRIPC(IFPRB,OUTSTR,99,ND,0,'preferences',IER)
C      write(6,*) 'prefs ',OUTSTR(1:lnblnk(OUTSTR))
      if(ier.ne.0)then
        ier=0   ! reset error state
        return
      endif
      if(ND.eq.0) return
      k=0
      CALL EGETW(OUTSTR,K,WORD,'W','preference tag',IER)
      if(WORD(1:7).eq.'*IPSMOD')then
        CALL EGETWI(OUTSTR,K,IPSMOD,0,0,'-','pref IPSMOD',IER)
        haveglobal=.TRUE.  ! remember this
      elseif(WORD(1:7).eq.'*MFBSNC')then
        CALL EGETWI(OUTSTR,K,MFBSNC,0,0,'-','pref MFBSNC',IER)
        haveglobal=.TRUE.  ! remember this
      elseif(WORD(1:7).eq.'*MSLVTP')then
        CALL EGETWI(OUTSTR,K,MSLVTP,0,0,'-','pref MSLVTP',IER)
        haveglobal=.TRUE.  ! remember this
      elseif(WORD(1:7).eq.'*MFTRAC')then
        CALL EGETWI(OUTSTR,K,MFTRAC,0,0,'-','pref MFTRAC',IER)
        haveglobal=.TRUE.  ! remember this
      endif

C Note: some variables in the commons are not included and are currently
C set to the standard values used in bps.
      MAXITF=100    ! iteration limit as per bps
      FERREL=0.01   ! relative error as per bps
      FERMFL=0.0005 ! abs error as per bps
      PMAX=50.0     ! max pressure correction
      STEFFR= -0.5  ! ratio of successive pressure corrections

C Debug.
C      write(6,*) 'MFBSNC',MFBSNC
C      write(6,*) 'IPSMOD MSLVTP',IPSMOD,MSLVTP
C      write(6,*) 'MAXITF FERREL FERMFL PMAX STEFFR MFTRAC'
C      write(6,*)  MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC
      goto 43  ! attempt to read another line

  100 RETURN

C Error trap on read error
  999 IER=1
      CALL EDISP(iuout,'EMFREAD: error reading mass flow network file!')
      WRITE(OUTS,9991) INOD,ICMP,ICNN
 9991 FORMAT(' in node:',I3,' component:',I3,' and connection:',I3)
      CALL EDISP(iuout,OUTS)
      WRITE(OUTS,9992)WORD
 9992 FORMAT(' from: ',A20)
      CALL EDISP(iuout,OUTS)
      CALL EDISP(iuout,OUTSTR)
      GOTO 100

C File read errors.
 1001 CALL USRMSG('EMFREAD: conversion error in',OUTSTR,'W')
      IER=1
      GOTO 100
      END


C ************************* EMF3DREAD 
C EMF3DREAD reads a 3D mass flow network description file.
C Reads allow comments and most input data checks are performed.
C It assumes that the file is already open.
      SUBROUTINE EMF3DREAD(IFPRB,MODE,IER)

#include "building.h"
#include "esprdbfile.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "espriou.h"
C espriou.h provides currentfile.
      INTEGER, PARAMETER :: MNWKTYP=6  ! from gnetwk.h
      
      integer lnblnk  ! function definition
      integer IFPRB
      character MODE*1
      integer ier

C Network flow commons are as follows:
C as defined in net_flow_data.h)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      character LAPROB*72
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12
      COMMON/NWKTYP/INWKTYP,vergnf
      INTEGER inwktyp
      REAL vergnf   ! 1.0 is 3D variant of ASCII network
      REAL VAL

C To signal that simulation preferences (globals in graphic
C network file) are available at the end of the file.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

C Simulation preferences as per simcon.F in esrubps, to allow the user to
C alter preferences. Typically filled with standard values.
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      DIMENSION RVA(MCMS),NDID(MNOD)
      CHARACTER OUTSTR*124,WORD*20,NODID*12,NDID*12,CMPID*12,OUTS*124
      CHARACTER LFIL*72,H*72,WORD2*24,ectime*24
      CHARACTER*72 DFILE
      LOGICAL CONT,OK,dok

C 'Reset' error indicator and clear node & component names
C and attributes.
      IER=0
      INOD=0; ICMP=0; ICNN=0
      NNOD=0; NCMP=0; NCNN=0
      WRED=1.0; fndegc=20.0
      CONT=.TRUE.
      do i=1,MNOD
        NDNAM(i)=' '; NODASSOC(i,1)='-'; NODASSOC(i,2)='-'
        NDFLD(i)=0; NDTYP(i)=0; ITND(i)=0; TNOD(i)=0.
        HNOD(i,1)=0.0; HNOD(i,2)=0.0; HNOD(i,3)=0.0
      enddo
      do i=1,MCMP
        CMNAM(i)=' '; CMPASSOC(i,1)='-'; CMPASSOC(i,2)='-'
        ITPCMP(i)=0; ISDCMP(i)=0; ISDCNN(i)=0; NWPCMP(i)=0
        HCMP(i,1,1)=0.0; HCMP(i,1,2)=0.0; HCMP(i,1,3)=0.0;
        HCMP(i,2,1)=0.0; HCMP(i,2,2)=0.0; HCMP(i,2,3)=0.0;
      enddo

C Open the file with user defined or existing filename
  2   DFILE='UNKNOWN'
      IF(LAPROB(1:3).NE.'   '.AND.LAPROB(1:3).NE.'UNK')THEN
        LFIL=LAPROB
      ELSE
        LFIL='UNKNOWN'
      ENDIF
      H(1)='A 3D flow network description file holds information '
      H(2)='on network topology: the components, their connectivity, '
      H(3)='component spatial position, component data and connection'
      H(4)='routing.                                                 '
      IF(MODE.EQ.'R'.OR.MODE.EQ.'N')THEN
        CALL EASKS(LFIL,'3D flow network description file? ',' ',72,
     &    DFILE,'file open',IER,4)
        IF(LFIL(1:3).NE.'   '.AND.LFIL(1:3).NE.'UNK') THEN
          LAPROB=LFIL
        ELSE
          dok=.true.
          h(1)='You gave a blank file name or UNKNOWN for the name. '
          CALL ASKOK('Filename blank - try again?',' ',OK,dok,1)
          IF(OK)THEN
            GOTO 2
          ELSE
            RETURN
          ENDIF
        ENDIF
      ELSE

C Read in silently.
        IF(LFIL(1:3).NE.'   '.AND.LFIL(1:3).NE.'UNK') THEN
          WRITE(OUTS,'(3A)')' Reading 3D flow network...',
     &      LAPROB(1:LNBLNK(LAPROB)),'.'
          CALL USRMSG(OUTS,' ','-')
        ELSE
          CALL EDISP(IUOUT,'Warning: 3D flow network filename was')
          CALL EDISP(IUOUT,'blank - network has not been read in.')
          RETURN
        ENDIF
      ENDIF

C Open the 3D flow network file.
      CALL EFOPSEQ(IFPRB,LAPROB,1,IER)
      IF(IER.EQ.-301) THEN
        IF(MODE.NE.'S') THEN
          WRITE(OUTS,'(3A)')
     &  'File ',LFIL(1:LNBLNK(LFIL)),' is a new network description.'
          dok=.true.
          h(1)='The file does not already exist and will be assumed to'
          h(2)='be for a new network unless you say no. '
          CALL ASKOK(OUTS,'Define a new network using this name?',
     &      OK,dok,2)     
          IF(OK) THEN
            RETURN
          ELSE
            GOTO 2
          ENDIF
        ELSE
          CALL EDISP(IUOUT,'Warning: 3D flow network filename was')
          CALL EDISP(IUOUT,'blank - network has not been read in')
          RETURN
        ENDIF
      ELSEIF(IER.NE.0.AND.IER.NE.-301) THEN
        WRITE(OUTS,'(3A)')
     &    ' Problem opening ',LFIL(1:LNBLNK(LFIL)),'.'
        dok=.true.
        h(1)='The file was expected to exist, but was not found or'
        h(2)='could not be read. Double check the name before trying'
        h(3)='again.  '
        CALL ASKOK(OUTS,'Try again?',OK,dok,3)
        IF(OK) THEN
          GOTO 2
        ELSE
          RETURN
        ENDIF
      ENDIF
      write(currentfile,'(a)') LAPROB(1:lnblnk(LAPROB))

C Read the file header and check for first-line tag.
      CALL STRIPC(IFPRB,OUTSTR,99,ND,0,'1st line of 3Dafn',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(OUTSTR(1:13).EQ.'*Flow_network')THEN
        vergnf=1.0
        if(ND.gt.1)then
          K=13
          CALL EGETWR(OUTSTR,K,vergnf,0.,2.,'-','version',IER)
        endif
        IAIRN = 3
        CALL EDISP(IUOUT,' ')    
        WRITE(OUTS,'(3A)') 
     &    'Opened 3D flow network file: ',LFIL(1:LNBLNK(LFIL)),'.' 
        CALL USRMSG(OUTS,' ','-') 
      else
        WRITE(OUTS,'(3A)') 'File: ',LFIL(1:LNBLNK(LFIL)), 
     &    ' is not a 3D flow network file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        return
      endif 
   
C Read in the header lines of the file, look for date, & network
C description, wind pressure.
  20  CALL STRIPC(IFPRB,OUTSTR,99,ND,0,'afn header lines',IER)
      IF(IER.NE.0) goto 42  ! at EOF so process
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','afn header tags',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then

C Read date stamp << not yet used >>.
        CALL EGETRM(OUTSTR,K,ectime,'W','afn date stamp',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:4).EQ.'*Doc')then

C Read doncumentation phrase.
        CALL EGETRM(OUTSTR,K,NWKDSC,'W','afn documentation',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:7).EQ.'*Domain')then

C Read the network type string and index and set NWKTYPSTR.
        WORD2='  '
        CALL EGETW(OUTSTR,K,WORD2,'W','network type string',IER)
        CALL EGETWI(OUTSTR,K,INWKTYP,1,MNWKTYP,'W','afn type index',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:13).EQ.'*windpressure')then
        CALL EGETWR(OUTSTR,K,WRED,0.0,10.,'W','wind red',IER)
      elseif(WORD(1:5).EQ.'*node')then
        NNOD=NNOD+1
        k=5
        CALL EGETW(OUTSTR,K,WORD,'W','node name',IER)
        write(NDNAM(NNOD),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETWI(OUTSTR,K,IVAL,1,2,'W','node fluid',IER)
        NDFLD(NNOD)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,3,'W','node type',IER)
        NDTYP(NNOD)=IVAL
        CALL EGETW(OUTSTR,K,WORD,'W','assoc zone',IER)
        write(NODASSOC(NNOD,1),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','assoc surf',IER)
        write(NODASSOC(NNOD,2),'(a)') WORD(1:lnblnk(WORD))

C Read the next line with position info.
        CALL STRIPC(IFPRB,OUTSTR,99,ND,0,'node position',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','Node X',IER)
        HNOD(NNOD,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','Node Y',IER)
        HNOD(NNOD,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','Node Z',IER)
        HNOD(NNOD,3)=VAL
        CALL EGETW(OUTSTR,K,WORD,'W','node temp/ref zone',IER)
        NDID(NNOD)=WORD(1:12)
        CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','node suppl 1',IER)
        SUPNOD(NNOD,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','node suppl 2',IER)
        SUPNOD(NNOD,2)=VAL

      elseif(WORD(1:4).EQ.'*cmp')then
        NCMP=NCMP+1
        k=4
        CALL EGETW(OUTSTR,K,WORD,'W','comp name',IER)
        write(CMNAM(NCMP),'(a)')WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','assoc zone',IER)
        write(CMPASSOC(NCMP,1),'(a)')WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','assoc surf',IER)
        write(CMPASSOC(NCMP,2),'(a)')WORD(1:lnblnk(WORD))
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','component type',IER)
        ITPCMP(NCMP)=IV 

C Find matching component text descriptor and remember it.
        IC=0
   31   IC=IC+1
        IF(ITPCMP(NCMP).EQ.IVALCM(IC)) GOTO 45
        IF(IC.LT.MCMV) GOTO 31
   45   LTPCMP(NCMP)=LVALCM(IC)

C Number of component level supplementary data items. 
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','comp supplem data',IER)
        ISDCMP(NCMP)=IV
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','conn supplem data',IER)
        ISDCNN(NCMP)=IV

C Read position. Always read start but if NWPCMP is 1 then
C also read an end point.
        CALL STRIPC(IFPRB,OUTSTR,99,ND,0,'cmp position',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETWI(OUTSTR,K,IV,0,0,'-','comp way points',IER)
        NWPCMP(NCMP)=IV
        CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','CMP X',IER)
        HCMP(NCMP,1,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','CMP Y',IER)
        HCMP(NCMP,1,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','CMP Z',IER)
        HCMP(NCMP,1,3)=VAL
        if(NWPCMP(NCMP).eq.1)then
          CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','CMP eX',IER)
          HCMP(NCMP,2,1)=VAL
          CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','CMP eY',IER)
          HCMP(NCMP,2,2)=VAL
          CALL EGETWR(OUTSTR,K,VAL,-9.0,999.,'W','CMP eZ',IER)
          HCMP(NCMP,2,3)=VAL
        endif
 
C Read supplementary data, in one or more lines until ISDMCP(ICMP) items
C have been obtained.
        IRVA=ISDCMP(NCMP)
        IF(IRVA.GT.0) THEN
          CALL EGETWRA(IFPRB,RVA,IRVA,0.,0.,'-','conn suppl',IER)
          DO KV=1,IRVA
            SUPCMP(NCMP,KV)=RVA(KV)
          ENDDO
        ENDIF
   
      elseif(WORD(1:4).EQ.'*cnn')then
        NCNN=NCNN+1
        NDSCNN(NCNN,1)=0
        NDSCNN(NCNN,2)=0
        k=4
        CALL EGETW(OUTSTR,K,WORD,'W','+ve node',IER)
        NODID=WORD(1:12)
        call GETND(NODID,iv,1,'+ve node')
        if(iv.eq.0)goto 999
        NODPS(NCNN)=iv
        CALL EGETWR(OUTSTR,K,HGTPS(NCNN),-100.,100.,'W',
     &    '+ve linkage pt',IER)
        IF(IER.NE.0)then 
          call edisp(iuout,' could not convert +ve linkage point...')
          GOTO 999
        endif

        CALL EGETW(OUTSTR,K,WORD,'W','-ve node',IER)
        NODID=WORD(1:12)
        call GETND(NODID,iv,1,'-ve node')
        if(iv.eq.0)goto 999
        NODNE(NCNN)=iv

        CALL EGETWR(OUTSTR,K,HGTNE(NCNN),-100.,100.,'W',
     &    '-ve linkage pt',IER)
        IF(IER.NE.0)then
          call edisp(iuout,' could not convert -ve linkage point...')
          GOTO 999
        endif

C Name of linking fluid flow component and its index.
        CALL EGETW(OUTSTR,K,WORD,'W','linking comp',IER)
        CMPID=WORD(1:12)
        call GETCMP(CMPID,iv,1,'linked component name')
        if(iv.eq.0)GOTO 999
        ITPCON(NCNN)=iv
      elseif(WORD(1:12).eq.'*End_network')then

C Reached file end so do checks.
        goto 42
      endif

C If there were no errors in reading header line then read another.
      if(CONT)then
        goto 20
      else
        call usrmsg('Error reading network graphics file @',outstr,'W')
        ier=1
        CLOSE(IFPRB)
        RETURN
      endif

C Checks to run after we have all the nodes and components.
C Check the nodal temperature.
  42  CLOSE(IFPRB)
      ITND(NNOD)=0
      DO 18 I=1,NNOD
        DO J=1,NNOD
C See if this node name has already been defined.
          IF(NDID(I)(1:12).EQ.NDNAM(J)(1:12)) THEN
            ITND(I)=J
          ENDIF
        ENDDO  ! of J
C No, it is not. Now interpret what we have as a number.
        IF(ITND(I).EQ.0) THEN
          ITND(I)=0
          read(NDID(I),*,ERR=1001)TNOD(I)
        ENDIF
  18  CONTINUE
      
C Check node data (same checks as in editing).
C Start by checking if current node does not already exist.
      INOD=0
   10 INOD=INOD+1
      I=0
   11 I=I+1
      IF(I.GT.1.AND.NDNAM(I)(1:12).EQ.NDNAM(INOD)(1:12))then
        write(outs,'(3A)')' duplicate names',NDNAM(I),NDNAM(INOD)
        call edisp(iuout,outs)
        GOTO 999
      endif
      IF(I.LT.INOD-1) GOTO 11

C Check node type
      IF(NDTYP(INOD).EQ.3.AND.NDFLD(INOD).NE.1)then
        call edisp(iuout,' Cannot have water at this node!')
        GOTO 999
      endif

C Check node temperature index and/or temperature
      if(ITND(INOD).LT.0.OR.ITND(INOD).GT.NNOD)then
        call edisp(iuout,'Node temperature linked node doesn`t exist!')
        GOTO 999
      endif
      if(ITND(INOD).GT.0) then
        IF(TNOD(INOD).LT.-100..OR.TNOD(INOD).GT.1000.)then
          call edisp(iuout,' Node temperature out of range!')
          GOTO 999
        endif
      endif

C Check boundary node supplementary data items.
      if(NDTYP(INOD).eq.3)then
        IF(SUPNOD(INOD,2).LT.0..OR.SUPNOD(INOD,2).GT.360.)then
          call edisp(iuout,' Orientation out of range!')
          GOTO 999
        endif
      endif

C Check component data, beginning with possible duplication. 
      ICMP=0
   30 ICMP=ICMP+1
      I=0
   32 I=I+1
      IF(I.GT.1.AND.CMNAM(I).EQ.CMNAM(ICMP))then
        call edisp(iuout,' Duplicate component!')
        GOTO 999
      endif
      IF(I.LT.ICMP-1) GOTO 32

C Check if this is a valid component type.
      IC=0
   33 IC=IC+1
      IF(ITPCMP(ICMP).EQ.IVALCM(IC)) GOTO 34
      IF(IC.LE.MCMV) GOTO 33
      call edisp(iuout,' Invalid component type!')
      IF(ITPCMP(ICMP).EQ.450) GOTO 34
      GOTO 999

C Check component level number of data items.
   34 call mfcmpsupcheck(ICMP,IER)
      IF(ICMP.LT.NCMP) GOTO 30

C Check connections data (perform same checks as editing code)
C Start by checking if current connection has valid nodes numbers.
      ICNN=0
   51 ICNN=ICNN+1
      IF(NODPS(ICNN).EQ.NODNE(ICNN))then
        call edisp(iuout,' Connection cannot be to same node!')
        GOTO 999
      endif
      IF(NODPS(ICNN).LT.1.OR.NODPS(ICNN).GT.NNOD)then
        call edisp(iuout,' First node in connection not known!')
        GOTO 999
      endif
      IF(NODNE(ICNN).LT.1.OR.NODNE(ICNN).GT.NNOD)then
        call edisp(iuout,' 2nd node in connection not known!')
        GOTO 999
      endif

C Check if both nodes have identical fluid type.
      IF(NDFLD(NODPS(ICNN)).NE.NDFLD(NODNE(ICNN)))then
        call edisp(iuout,' Cannot mix fluid types!')
        GOTO 999
      endif

C Check if linking flow component is a known component.
      IF(ITPCON(ICNN).LT.1.OR.ITPCON(ICNN).GT.NCMP)then
        call edisp(iuout,' Linking component unknown type!')
        GOTO 999
      endif

C Check if fluid type identical to fluid type of node on 'one' side.
      IF(NDFLD(NODPS(ICNN)).NE.INT(SUPCMP(ITPCON(ICNN),1)))then
        call edisp(iuout,' Linking component different fluid!')
        GOTO 999
      endif
      IF(ICNN.LT.NCNN) GOTO 51
    
  43  CONTINUE

C There may be supplemental data for simulation preferences. Attempt
C to read tag:data lines until the end of the file.
      CALL STRIPC(IFPRB,OUTSTR,99,ND,0,'preferences',IER)
C      write(6,*) 'prefs ',OUTSTR(1:lnblnk(OUTSTR))
      if(ier.ne.0)then
        ier=0   ! reset error state
        return
      endif
      if(ND.eq.0) return
      k=0
      CALL EGETW(OUTSTR,K,WORD,'W','preference tag',IER)
      if(WORD(1:7).eq.'*IPSMOD')then
        CALL EGETWI(OUTSTR,K,IPSMOD,0,0,'-','pref IPSMOD',IER)
        haveglobal=.TRUE.  ! remember this
      elseif(WORD(1:7).eq.'*MFBSNC')then
        CALL EGETWI(OUTSTR,K,MFBSNC,0,0,'-','pref MFBSNC',IER)
        haveglobal=.TRUE.  ! remember this
      elseif(WORD(1:7).eq.'*MSLVTP')then
        CALL EGETWI(OUTSTR,K,MSLVTP,0,0,'-','pref MSLVTP',IER)
        haveglobal=.TRUE.  ! remember this
      elseif(WORD(1:7).eq.'*MFTRAC')then
        CALL EGETWI(OUTSTR,K,MFTRAC,0,0,'-','pref MFTRAC',IER)
        haveglobal=.TRUE.  ! remember this
      endif

C Note: some variables in the commons are not included and are currently
C set to the standard values used in bps.
      MAXITF=100    ! iteration limit as per bps
      FERREL=0.01   ! relative error as per bps
      FERMFL=0.0005 ! abs error as per bps
      PMAX=50.0     ! max pressure correction
      STEFFR= -0.5  ! ratio of successive pressure corrections

C Debug.
C      write(6,*) 'MFBSNC',MFBSNC
C      write(6,*) 'IPSMOD MSLVTP',IPSMOD,MSLVTP
C      write(6,*) 'MAXITF FERREL FERMFL PMAX STEFFR MFTRAC'
C      write(6,*)  MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC
      goto 43  ! attempt to read another line

  100 RETURN

C Error trap on read error
  999 IER=1
      CALL EDISP(iuout,'EMF3DREAD: error reading mass flow file!')
      WRITE(OUTS,9991) INOD,ICMP,ICNN
 9991 FORMAT(' in node:',I3,' component:',I3,' and connection:',I3)
      CALL EDISP(iuout,OUTS)
      WRITE(OUTS,9992)WORD
 9992 FORMAT(' from: ',A20)
      CALL EDISP(iuout,OUTS)
      CALL EDISP(iuout,OUTSTR)
      GOTO 100

C File read errors.
 1001 CALL USRMSG('EMF3DREAD: conversion error in',OUTSTR,'W')
      IER=1
      GOTO 100
      END


C ****************** MFCMPSUPCHECK 
C Mfcmpsupcheck: check flow components for correct number of supplemental
C data items.
      subroutine mfcmpsupcheck(ICMP,IER)

#include "net_flow.h"
#include "net_flow_data.h"
      COMMON/OUTIN/IUOUT,IUIN
      character outs*124
      logical close1,close2,close3

      PI=4.*ATAN(1.)

C Check component level number of data items.
      IS1=INT(SUPCMP(ICMP,1))
      NSDC=ISDCMP(ICMP)
      IF(ITPCMP(ICMP).EQ. 10) THEN

C Type 10 power law volume flow resistance component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)
     &    call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow coef not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow exponent not allowed.',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 11) THEN

C Type 11 - trickle ventilator (see notes in esrumfs/mfmach.F MF011C).
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)
     &    call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        CALL ECLOSE(SUPCMP(ICMP,2),15.0,0.01,close1)
        CALL ECLOSE(SUPCMP(ICMP,2),30.0,0.01,close2)
        if(close1)then
          continue
        elseif(close2)then
          continue
        else
          call MFERR(ICMP,' Device rating should be 15.0 or 30.0',IER)
        endif
        if(SUPCMP(ICMP,3).LE.0.)
     &    call MFERR(ICMP,' Number of devices cannot be zero.',IER)

      ELSE IF(ITPCMP(ICMP).EQ. 12) THEN
C Type 12 power law volume flow resistance component with upper limit
C or closing-setpoint.
        if(NSDC.EQ.6.OR.NSDC.EQ.12)CALL CNTERR(ICMP,5)
        if((NSDC.NE.5).OR.ISDCNN(ICMP).NE.0)
     &    call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow coef not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow exponent not allowed.',IER)
        if(SUPCMP(ICMP,4).LT.0.)
     &    call MFERR(ICMP,' Negative flow/dp setpnt not allowed.',IER)
        if((SUPCMP(ICMP,5).LT.0.).or.(SUPCMP(ICMP,5).GT.1.))
     &    call MFERR(ICMP,' Switch 0 or 1!',IER)

      ELSE IF(ITPCMP(ICMP).EQ. 15) THEN

C Type 15 power law mass flow resistance component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow coef not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow exponent not allowed.',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 17) THEN

C Type 17 power law mass flow resistance component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow coef not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow exponent not allowed.',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 20) THEN

C Type 20 quadratic law volume flow resistance component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow coef not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow exponent not allowed.',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 25) THEN

C Type 25 quadratic law mass flow resistance component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow coef not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow exponent not allowed.',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 30) THEN

C Type 30 constant volume flow rate component.
        if(NSDC.EQ.3.OR.NSDC.EQ.9)CALL CNTERR(ICMP,2)
        if((NSDC.NE.2).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 35) THEN

C Type 35 constant mass flow rate component.
        if(NSDC.EQ.3.OR.NSDC.EQ.9)CALL CNTERR(ICMP,2)
        if((NSDC.NE.2).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 40) THEN

C Type 40 common orifice flow component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative opening area not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0..OR.SUPCMP(ICMP,3).GT.1.)
     &    call MFERR(ICMP,' Discharge factor not > 0. or < 1.',IER)
      ELSE IF(ITPCMP(ICMP).EQ. 50) THEN

C Type 50 laminar pipe volume flow rate component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative length not allowed.',IER)
        if(SUPCMP(ICMP,3).LT.0.001)
     &    call MFERR(ICMP,' Radius must be > 0.001 m',IER)
      ELSE IF(ITPCMP(ICMP).EQ.110) THEN

C Type 110 specific air flow opening.
        if(NSDC.EQ.3.OR.NSDC.EQ.9)CALL CNTERR(ICMP,2)
        if((NSDC.NE.2).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.NE.1)call MFERR(ICMP,' Fluid type must be 1. ',IER)
        if(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative opening area not allowed.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.120) THEN

C Type 120 specific air flow crack component.
        if(NSDC.EQ.4.OR.NSDC.EQ.10)CALL CNTERR(ICMP,3)
        if((NSDC.NE.3).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.NE.1)call MFERR(ICMP,' Fluid type must be 1. ',IER)
        if(SUPCMP(ICMP,2).LT.0.0001.OR.SUPCMP(ICMP,2).GT.0.1)
     &    call MFERR(ICMP,' Crack width out of range.',IER)
        if(SUPCMP(ICMP,3).LT.0.001)
     &    call MFERR(ICMP,' Crack length out of range.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.130) THEN

C Type 130 specific air flow door component.
        if(NSDC.EQ.6.OR.NSDC.EQ.12)CALL CNTERR(ICMP,5)
        if((NSDC.NE.5).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.NE.1)call MFERR(ICMP,' Fluid type must be 1. ',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Door width out of range.',IER)
        if(SUPCMP(ICMP,3).LT.0.001)
     &    call MFERR(ICMP,' Door height out of range.',IER)
        if(SUPCMP(ICMP,5).LT.0.001.OR.SUPCMP(ICMP,5).GT.1.0)
     &    call MFERR(ICMP,' Door discharge factor out of range.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.210) THEN

C Type 210 general flow conduit.
        if(NSDC.EQ.7.OR.NSDC.EQ.13)CALL CNTERR(ICMP,6)
        if((NSDC.NE.6).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1. or 2.',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Conduit hydraulic diam out of range.',IER)
        if(SUPCMP(ICMP,3).LE.0..OR.
     &     SUPCMP(ICMP,3).LT.(.99*PI*SUPCMP(ICMP,2)**2/4.))
     &     call MFERR(ICMP,' area must be >= hydraulic diam area',IER)
        if(SUPCMP(ICMP,4).LT.0.001)
     &    call MFERR(ICMP,' Conduit length out of range.',IER)
        if(SUPCMP(ICMP,5).LT.0..OR.SUPCMP(ICMP,5).GE.SUPCMP(ICMP,2)/2.)
     &    call MFERR(ICMP,' Roughness factor out of range.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.211) THEN

C Type 211 - Cowl roof ventilator (typical ceramic unit).
        if(NSDC.EQ.7.OR.NSDC.EQ.13)CALL CNTERR(ICMP,6)
        if((NSDC.NE.6).OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif

C Error checks.
        if(IS1.NE.1)
     &    call MFERR(ICMP,' Fluid type must be 1.',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Cowl hydraulic diam < 0.001.',IER)
        if(SUPCMP(ICMP,4).LT.0.001)
     &    call MFERR(ICMP,' Cowel coefficien B < 0.001.',IER)
        if(SUPCMP(ICMP,5).LT.0.001)
     &    call MFERR(ICMP,' Cowel coefficien n < 0.001.',IER)
        if(SUPCMP(ICMP,6).LT.-0.1.OR.SUPCMP(ICMP,6).GT.1.1)
     &    call MFERR(ICMP,' Include wind effects not zero or one.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.220) THEN

C Type 220 flow conduit ending in 3-leg junction.
        if(NSDC.NE.13.OR.ISDCNN(ICMP).NE.1)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2.',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Conduit hydraulic diam out of range.',IER)
        if(SUPCMP(ICMP,3).LE.0..OR.
     &     SUPCMP(ICMP,3).LT.(.99*PI*SUPCMP(ICMP,2)**2/4.))
     &     call MFERR(ICMP,' area must be >= hydraulic diam area',IER)
        if(SUPCMP(ICMP,4).LT.0.001)
     &    call MFERR(ICMP,' Conduit length out of range.',IER)
        if(SUPCMP(ICMP,5).LT.0..OR.SUPCMP(ICMP,5).GE.SUPCMP(ICMP,2)/2.)
     &    call MFERR(ICMP,' Roughness factor out of range.',IER)
        if(SUPCMP(ICMP,7).LE.0.)
     &    call MFERR(ICMP,' Negative X-sect area of junct common!',IER)
      ELSE IF(ITPCMP(ICMP).EQ.230) THEN

C Type 230 flow conduit starting in a diverging 3-leg junction.
        if(NSDC.NE.13.OR.ISDCNN(ICMP).NE.1)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2.',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Conduit hydraulic diam out of range.',IER)
        if(SUPCMP(ICMP,3).LE.0..OR.
     &     SUPCMP(ICMP,3).LT.(.99*PI*SUPCMP(ICMP,2)**2/4.))
     &     call MFERR(ICMP,' area must be >= hydraulic diam area',IER)
        if(SUPCMP(ICMP,4).LT.0.001)
     &    call MFERR(ICMP,' Conduit length out of range.',IER)
        if(SUPCMP(ICMP,5).LT.0..OR.SUPCMP(ICMP,5).GE.SUPCMP(ICMP,2)/2.)
     &    call MFERR(ICMP,' Roughness factor out of range.',IER)
        if(SUPCMP(ICMP,7).LE.0.)
     &    call MFERR(ICMP,' Negative X-sect area of junct common!',IER)
      ELSE IF(ITPCMP(ICMP).EQ.240) THEN

C Type 240 flow conduit ending in a converging 4-leg junction.
        if(NSDC.NE.17.OR.ISDCNN(ICMP).NE.2)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2.',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Conduit hydraulic diam out of range.',IER)
        if(SUPCMP(ICMP,3).LE.0..OR.
     &     SUPCMP(ICMP,3).LT.(.99*PI*SUPCMP(ICMP,2)**2/4.))
     &     call MFERR(ICMP,' area must be >= hydraulic diam area',IER)
        if(SUPCMP(ICMP,4).LT.0.001)
     &    call MFERR(ICMP,' Conduit length out of range.',IER)
        if(SUPCMP(ICMP,5).LT.0..OR.SUPCMP(ICMP,5).GE.SUPCMP(ICMP,2)/2.)
     &    call MFERR(ICMP,' Roughness factor out of range.',IER)
        if(SUPCMP(ICMP,7).LE.0.)
     &    call MFERR(ICMP,' Negative X-sect area of junct common!',IER)
      ELSE IF(ITPCMP(ICMP).EQ.250) THEN

C Type 250 flow conduit starting in a converging 4-leg junction.
        if(NSDC.NE.17.OR.ISDCNN(ICMP).NE.2)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2.',IER)
        if(SUPCMP(ICMP,2).LT.0.001)
     &    call MFERR(ICMP,' Conduit hydraulic diam out of range.',IER)
        if(SUPCMP(ICMP,3).LE.0..OR.
     &     SUPCMP(ICMP,3).LT.(.99*PI*SUPCMP(ICMP,2)**2/4.))
     &     call MFERR(ICMP,' area must be >= hydraulic diam area',IER)
        if(SUPCMP(ICMP,4).LT.0.001)
     &    call MFERR(ICMP,' Conduit length out of range.',IER)
        if(SUPCMP(ICMP,5).LT.0..OR.SUPCMP(ICMP,5).GE.SUPCMP(ICMP,2)/2.)
     &    call MFERR(ICMP,' Roughness factor out of range.',IER)
        if(SUPCMP(ICMP,7).LE.0.)
     &    call MFERR(ICMP,' Negative X-sect area of junct common!',IER)
      ELSE IF(ITPCMP(ICMP).EQ.310) THEN

C Type 310 general flow inducer.
        if(NSDC.NE.7.OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        if(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1 or 2. ',IER)
        if(SUPCMP(ICMP,3).LE.SUPCMP(ICMP,2))
     &    call MFERR(ICMP,' Upper and lower range cross',IER)

        CALL ECLOSE(SUPCMP(ICMP,5),0.0,0.001,close1)
        CALL ECLOSE(SUPCMP(ICMP,6),0.0,0.001,close2)
        CALL ECLOSE(SUPCMP(ICMP,7),0.0,0.001,close3)
        if(close1.and.close2.and.close3)
     &    call MFERR(ICMP,' all coefficients equal zero',IER)
      ELSE IF(ITPCMP(ICMP).EQ.410) THEN

C Type 410 general flow corrector.
        if(NSDC.EQ.17)CALL CNTERR(ICMP,7)
        if(NSDC.NE.7.OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        IF(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1. or 2.',IER)
        IF(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Standard density rho out of range.',IER)
        IF(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative pressure dif not allowed.',IER)
        CALL ECLOSE(SUPCMP(ICMP,4),1.0,0.001,close1)
        CALL ECLOSE(SUPCMP(ICMP,4),2.0,0.001,close2)
        IF(close1.or.close2)then
        else
          call MFERR(ICMP,' Index must be linear or logartihmic.',IER)
        endif
        IF(SUPCMP(ICMP,5).LT.0.)
     &    call MFERR(ICMP,' Negative open vol flow not allowed.',IER)
        IF(SUPCMP(ICMP,6).LT.0..OR.SUPCMP(ICMP,6).GT.100.)
     &    call MFERR(ICMP,' Percentage flow closed out of range.',IER)
        IF(SUPCMP(ICMP,7).LT.0..OR.SUPCMP(ICMP,7).GT.100.)
     &    call MFERR(ICMP,' Lower validity limit out of range.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.420) THEN

C Type 420 flow corrector with polynomial flow resistance.
        if(NSDC.EQ.16)CALL CNTERR(ICMP,6)
        if(NSDC.NE.6.OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        IF(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1. or 2.',IER)
        IF(SUPCMP(ICMP,2).LE.0.)
     &    call MFERR(ICMP,' Negative x-section not allowed. ',IER)

C Type 450 does not exist anymore.
      ELSE IF(ITPCMP(ICMP).EQ.450) THEN
        CALL CNTERR(ICMP,0)
        call MFERR(450,
     &    ' Component type 450 is not available anymore.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.460) THEN

C Type 460 fixed flow rate controller.
        if(NSDC.EQ.8)CALL CNTERR(ICMP,3)
        if(NSDC.NE.3.OR.ISDCNN(ICMP).NE.0)then
          call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
          goto 999
        endif
        IF(IS1.LT.1.OR.IS1.GT.2)
     &    call MFERR(ICMP,' Fluid type must be 1. or 2.',IER)
        IF(SUPCMP(ICMP,2).LT.0.)
     &    call MFERR(ICMP,' Negative flow rate below setpoint.',IER)
        IF(SUPCMP(ICMP,3).LT.0.)
     &    call MFERR(ICMP,' Negative flow rate above setpoint.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.470) THEN

C Type 470 range based flow rate controller (defunct type).
        call MFERR(ICMP,' Use type 30 or 35 comp with range ctl.',IER)
      ELSE IF(ITPCMP(ICMP).EQ.500) THEN

C Type 500 multi configuration controller
        IF(NSDC.NE.6)
     &  call MFERR(ICMP,' Number of suppl data items incorrect.',IER)
      ELSE
        call edisp(iuout,' Unknown component type...')
        goto 999
      ENDIF
      return

C Error trap on read error
  999 IER=1
      CALL EDISP(iuout,'component check: error in network file!')
      WRITE(OUTS,9991) ICMP
 9991 FORMAT(' in component:',I3)
      CALL EDISP(iuout,OUTS)
      return
      end

C ****************** MFERR 
C MFERR Error trap routine.
      SUBROUTINE MFERR(ICMP,MSG,IER)

#include "net_flow.h"
#include "net_flow_data.h"
      COMMON/OUTIN/IUOUT,IUIN
      character*(*) MSG
      character outs*124

      IER=1
      call edisp(iuout,' ')
      call edisp(iuout,MSG)
      WRITE(outs,9991) ITPCMP(ICMP),ICMP,CMNAM(ICMP)
 9991 FORMAT(' FC',I3,'I: illegal data, component ',I3,' = ',A12)
      call edisp(iuout,outs)
      return
      end

C ************************* MFLIST 
C MFLIST Fluid flow file: list common block contents.
      SUBROUTINE MFLIST(itru)

#include "net_flow.h"
#include "net_flow_data.h"
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
 
C Markdown flag.
      logical markdown
      common/markdownflag/markdown
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72
      CHARACTER outs*124,fldtyp*6,nodtyp*20,dat1*4,dat2*4,tmnam*12
      logical written

      CALL EDISP(itru,' ')
      CALL EDISP(itru,' Flow network description. ')
      CALL EDISP(itru,' ')

C Number of nodes, components and interconnections.
      WRITE(outs,1010) NNOD,NCMP,NCNN,WRED
 1010 FORMAT(1X,I3,' nodes, ',I3,' components, ',I3,' connections;',
     &       5X,'wind reduction = ',F6.3)
      CALL EDISP(itru,outs)

C Node data.
      IF(NNOD.EQ.0) GOTO 999
      WRITE(outs,'(A,A)')'   # Node      Fluid   Node Type',
     &  '           Height Temperature    Data_1       Data_2'
      CALL EDISP(itru,outs)

      DO 30 INOD=1,NNOD
        if(NDFLD(INOD).eq.1)then
          fldtyp='air   '
        elseif(NDFLD(INOD).eq.2)then
          fldtyp='water '
        endif
        if(NDTYP(INOD).eq.0)then
          nodtyp='internal & unknown  '
          dat1='(-) '
          dat2='vol '
        elseif(NDTYP(INOD).eq.1)then
          nodtyp='internal & known    '
          dat1='(Pa)'
          dat2='vol '
        elseif(NDTYP(INOD).eq.2)then
          nodtyp='boundary & known    '
          dat1='(Pa)'
          dat2='(-) '
        elseif(NDTYP(INOD).eq.3)then
          nodtyp='boundary & wind ind '
          dat1='coef'
          dat2='azim'
        endif
        IF(ITND(INOD).EQ.0) THEN
          WRITE(outs,1030)
     &      INOD,NDNAM(INOD),fldtyp,nodtyp,HNOD(INOD,1),HNOD(INOD,2),
     &      HNOD(INOD,3),TNOD(INOD),dat1,SUPNOD(INOD,1),dat2,
     &      SUPNOD(INOD,2)
        ELSE
          WRITE(outs,10301)
     &      INOD,NDNAM(INOD),fldtyp,nodtyp,HNOD(INOD,1),HNOD(INOD,2),
     &      HNOD(INOD,3),NDNAM(ITND(INOD)),dat1,SUPNOD(INOD,1),dat2,
     &      SUPNOD(INOD,2)
        END IF
        call edisp(itru,outs)
   30 CONTINUE
 1030 FORMAT(I4,1X,A,1x,2a,3F8.4,1X,G12.5,1X,a,F9.3,2X,a,F8.3)
10301 FORMAT(I4,1X,A,1x,2a,3F8.4,1X,A12,1X,a,F9.3,2X,a,F8.3)

C Component data.
      IF(NCMP.gt.0)then
        CALL EDISP(itru,' ')
        if(IAIRN.eq.3)then
          CALL EDISP(itru,
     &    ' Component    Type C+ L+ X Y Z Position Description')
        else
          CALL EDISP(itru,
     &    ' Component    Type C+ L+ Description')
        endif

        DO 50 ICMP=1,NCMP
          call edisp(itru,' ')
          if(IAIRN.eq.3)then
            WRITE(outs,'(1x,a,i5,i3,i3,3f6.1,1x,a)')
     &        CMNAM(ICMP),ITPCMP(ICMP),
     &        ISDCMP(ICMP),ISDCNN(ICMP),HCMP(ICMP,1,1),
     &        HCMP(ICMP,1,2),HCMP(ICMP,1,3),LTPCMP(ICMP)
          else
            WRITE(outs,'(1x,a,i5,i3,i3,1x,a)')CMNAM(ICMP),
     &        ITPCMP(ICMP),ISDCMP(ICMP),ISDCNN(ICMP),LTPCMP(ICMP)
          endif
          CALL EDISP(itru,outs)
          if(IAIRN.eq.3)then
            lnczn=lnblnk(CMPASSOC(ICMP,1))
            lncsn=lnblnk(CMPASSOC(ICMP,2))
            write(outs,'(4a)') ' Associated with ',
     &        CMPASSOC(ICMP,1)(1:lnczn),':',
     &        CMPASSOC(ICMP,2)(1:lncsn)
            CALL EDISP(ITRU,OUTS)
          endif

C Provide a heading string for each different component type.
          written=.false.
          IF(ITPCMP(ICMP).EQ. 10) THEN
            WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a ',SUPCMP(ICMP,2),' exponent b ',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 11) THEN
            WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' Rated flow ',SUPCMP(ICMP,2),' Nb devices ',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 12) THEN
            WRITE(outs,'(a,F4.1,3(a,G12.5),a,F4.1)')
     &      ' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a ',SUPCMP(ICMP,2),' exponent b ',SUPCMP(ICMP,3),
     &      ' max val ',SUPCMP(ICMP,4),' switch val',SUPCMP(ICMP,5)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 15) THEN
            WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a ',SUPCMP(ICMP,2),' exponent b ',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 17) THEN
            WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a ',SUPCMP(ICMP,2),' exponent b ',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 20) THEN
            WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a (Pa.s/m^3)',SUPCMP(ICMP,2),
     &      ' exponent b (Pa.(s/m^3)^2)',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 25) THEN
            WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a (Pa.s/kg)',SUPCMP(ICMP,2),
     &      ' exponent b (Pa.(s/kg)^2)',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 30) THEN
            WRITE(outs,'(a,F4.1,a,G12.5)')' Fluid ',SUPCMP(ICMP,1),
     &      ' flow rate (m^3/s) ',SUPCMP(ICMP,2)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 35) THEN
            WRITE(outs,'(a,F4.1,a,G12.5)')' Fluid ',SUPCMP(ICMP,1),
     &      ' flow rate (kg/s) ',SUPCMP(ICMP,2)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 40) THEN
            WRITE(outs,'(a,F4.1,a,F7.3,a,F6.3)')' Fluid ',
     &      SUPCMP(ICMP,1),' opening area (m^2)',SUPCMP(ICMP,2),
     &      ' discharge factor (-)',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
            WRITE(outs,'(a,F4.1,a,F7.3,a,F7.3)')' Fluid ',
     &      SUPCMP(ICMP,1),' path length(m)',SUPCMP(ICMP,2),
     &      ' opening radius(m)',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ.110) THEN
            WRITE(outs,'(a,F4.1,a,F7.3)')' Fluid ',SUPCMP(ICMP,1),
     &      ' opening area(m)',SUPCMP(ICMP,2)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ.120) THEN
            WRITE(outs,'(a,F4.1,a,F8.4,a,F7.3)')' Fluid ',
     &      SUPCMP(ICMP,1),' crack width(m)',SUPCMP(ICMP,2),
     &      ' crack length(m)',SUPCMP(ICMP,3)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ.130) THEN
            WRITE(outs,'(a,F4.1,a,F6.3,a,F6.3,a,F6.3,a,F5.3)')' Fluid ',
     &      SUPCMP(ICMP,1),' width ',SUPCMP(ICMP,2),' height',
     &      SUPCMP(ICMP,3),' ref ht ',SUPCMP(ICMP,4),
     &      ' discharge factor ',SUPCMP(ICMP,5)
            CALL EDISP(itru,outs)
            written=.true.
          ELSE IF(ITPCMP(ICMP).EQ.210) THEN
            call edisp(itru,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
          ELSE IF(ITPCMP(ICMP).EQ.211) THEN
            call edisp(itru,
     &  '   Fluid, cowl hyd dia, loc loss, coef B coef n, include wind')
          ELSE IF(ITPCMP(ICMP).EQ.220) THEN
            call edisp(itru,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
            call edisp(itru,' x-sect of junct, Ccp coef a0 - a5.')
          ELSE IF(ITPCMP(ICMP).EQ.230) THEN
            call edisp(itru,
     &    ' Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
            call edisp(itru,' x-sect of junct, Ccp coef a0 - a5.')
          ELSE IF(ITPCMP(ICMP).EQ.240) THEN
            call edisp(itru,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
            call edisp(itru,'   x-sect of junct, Ccp coef a0 - a9.')
          ELSE IF(ITPCMP(ICMP).EQ.250) THEN
            call edisp(itru,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
            call edisp(itru,'   x-sect of junct, Ccp coef a0 - a9.')
          ELSE IF(ITPCMP(ICMP).EQ.310) THEN
            call edisp(itru,
     &  '   Fluid, lower & upper vol rate, flow coef a0 - a3')
          ELSE IF(ITPCMP(ICMP).EQ.410) THEN
            call edisp(itru,
     &  '   Fluid, density, pres dif, lin/log, open & closed vol flow,')
            call edisp(itru,
     &  '   low validity limit')
          ELSE IF(ITPCMP(ICMP).EQ.420) THEN
            call edisp(itru,'   Fluid, x-sec, coef a0-a3')
          ELSE IF(ITPCMP(ICMP).EQ.460) THEN
            call edisp(itru,
     &  '   Fluid, flow when S < Ssp, flow when S > Ssp.')
          ELSE IF(ITPCMP(ICMP).EQ.500) THEN
            call edisp(itru,
     &  '   multi configuration flow component ')
            ICSP=NINT(SUPCMP(ICMP,2))
            TMNAM=CMNAM(ICSP)
            write(outs,'(3A,F5.2)')
     &      '  ',TMNAM(1:lnblnk(TMNAM)),
     &      ' above control action ',SUPCMP(ICMP,3)
            call edisp(itru,outs)
            ICSP=NINT(SUPCMP(ICMP,4))
            TMNAM=CMNAM(ICSP)
            write(outs,'(3A,F5.2,A,F5.2)')
     &      '  ',TMNAM(1:lnblnk(TMNAM)),
     &      ' between control action ',
     &      SUPCMP(ICMP,3),' and ',SUPCMP(ICMP,5)
            call edisp(itru,outs)
            ICSP=NINT(SUPCMP(ICMP,6))
            TMNAM=CMNAM(ICSP)
            write(outs,'(3A,F5.2)')
     &      '  ',TMNAM(1:lnblnk(TMNAM)),
     &      ' below control action ',SUPCMP(ICMP,5)
            call edisp(itru,outs)
            WRITTEN=.TRUE.
          endif

C If not already written out.
          if(.NOT.written)then
            if(ISDCMP(ICMP).LE.9)then
              WRITE(outs,'(F5.1,8F9.3)')
     &          (SUPCMP(ICMP,I),I=1,ISDCMP(ICMP))
              CALL EDISP(itru,outs)
            elseif(ISDCMP(ICMP).GT.9)then
              WRITE(outs,'(F5.1,8F8.2)')(SUPCMP(ICMP,I),I=1,9)
              CALL EDISP(itru,outs)
              WRITE(outs,'(5x,9F8.2)')
     &          (SUPCMP(ICMP,I),I=10,ISDCMP(ICMP))
              CALL EDISP(itru,outs)
            endif
          endif
   50   CONTINUE
      endif

C Connections data
      IF(NCNN.gt.0) then
        CALL EDISP(itru,' ')
        WRITE(outs,'(2a)') 
     &    '    # +Node         dHght   -Node         dHght',
     &    '   Component       Z @+    Z @-'
        CALL EDISP(itru,outs)

        DO 70 ICNN=1,NCNN
          zplus=HNOD(NODPS(ICNN),3)+HGTPS(ICNN)
          zminus=HNOD(NODNE(ICNN),3)+HGTNE(ICNN)
          WRITE(outs,1070)
     &    ICNN,NDNAM(NODPS(ICNN)),HGTPS(ICNN),NDNAM(NODNE(ICNN)),
     &    HGTNE(ICNN),CMNAM(ITPCON(ICNN)),zplus,zminus
 1070     FORMAT(1X,I4,1X,A12,F7.3,3X,A12,F7.3,3X,A12,1X,2F8.3)
          CALL EDISP(itru,outs)
   70   CONTINUE

C Thermal zone - air flow node mapping (if any)
C<< Plant and air flow node mapping to be added
        IHEAD1=0
        IHEAD2=0
        DO 234 IC=1,NCOMP
          IF(IHEAD1.NE.1)THEN
            CALL EDISP(ITRU,'thermal zone to air flow node mapping:')
            CALL EDISP(ITRU,'thermal zone -> air flow node')
            IHEAD1=1
          ENDIF
          IF(ICAAS(IC).NE.0)THEN
            WRITE(OUTS,'(3A)')ZNAME(IC),' -> ',NDNAM(ICAAS(IC))
            CALL EDISP(ITRU,OUTS)
          ELSE
            WRITE(OUTS,'(3A)')ZNAME(IC),' -> ','node not defined'
            CALL EDISP(ITRU,OUTS)
          ENDIF
 234    CONTINUE
      endif
 100  RETURN

C Error trap
  999 CALL USRMSG(' ',' MFLIST: no more information available!','W')
      GOTO 100

      END

C***** Error checking.
      SUBROUTINE CNTERR(ICMP,NEWDI)

#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/OUTIN/IUOUT,IUIN

      IF(ISDCMP(ICMP)-1.EQ.NEWDI)THEN
        IF(ABS(SUPCMP(ICMP,NEWDI+1)).LT.0.00001)
     &    ISDCMP(ICMP)=ISDCMP(ICMP)-1
      ELSE
        call edisp(IUOUT,' ')
        call edisp(IUOUT,
     &    ' This file has an old mass flow control format.')
        call edisp(IUOUT,' The file must be converted to new format.')
      ENDIF

      RETURN
      END

C**** GETND returns mass flow node index matching NAME. If not found
C return 0 index with the message ermsg if in verbose mode (ivbs=1).
      subroutine GETND(NAME,index,ivbs,MSG)
#include "net_flow.h"
#include "net_flow_data.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      character*(*),MSG,NAME
      character NODE*12,outs*124

      index=0
      NODE=NAME(1:12)
      DO 10 I=1,NNOD
        IF(NODE.EQ.NDNAM(I))THEN
          index=I
          GOTO 100
        ENDIF
   10 CONTINUE

      if(ivbs.eq.1)then
        write(outs,'(4a)')' could not find ',MSG(1:lnblnk(MSG)),
     &                    ' node... ',NODE
        call edisp(IUOUT,outs)
      endif
 100  return
      end

C**** GETCN returns mass flow connection index matching two node names 
C and associated component. If not found return 0 index
C with the message msg if in verbose mode (ivbs=1).
      subroutine GETCN(NDPOS,NDNEG,CMPID,index,ivbs,MSG)
#include "net_flow.h"
#include "net_flow_data.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      character*(*),MSG,NDPOS,NDNEG
      character NODLE*12,NODRI*12,CMPNM*12,outs*124,CMPID*12

      index=0
      NODLE=NDPOS(1:12)
      NODRI=NDNEG(1:12)
      CMPNM=CMPID(1:12)
      IF(NODLE.EQ.NODRI)then
        call edisp(iuout,' Connection cannot be to same node!')
        return
      endif
      DO 10 ICNN=1,NCNN
        IF(NODLE.EQ.NDNAM(NODPS(ICNN)).AND.NODRI.EQ.NDNAM(NODNE(ICNN))
     &    .AND.CMPNM.EQ.CMNAM(ITPCON(ICNN)))THEN
          index=ICNN
          GOTO 100
        ENDIF
   10 CONTINUE

      if(ivbs.eq.1)then
        write(outs,'(4a,2x,a,2x,a)')' could not find ',
     &    MSG(1:lnblnk(MSG)),' connection... ',NODLE,NODRI,CMPNM
        call edisp(iuout,outs)
      endif
  100 RETURN
      END

C**** GETCMP returns mass flow component index matching name 
C If not found return 0 index with the message
C msg if in verbose mode (ivbs=1).
      subroutine GETCMP(NAME,index,ivbs,MSG)
#include "net_flow.h"
#include "net_flow_data.h"
      COMMON/OUTIN/IUOUT,IUIN

      character*(*),MSG,NAME
      character CMPNM*12,outs*124

      index=0
      CMPNM=NAME(1:12)
      DO 10 ICNN=1,NCMP
        IF(CMPNM.EQ.CMNAM(ICNN))THEN
          index=ICNN
          GOTO 100
        ENDIF
   10 CONTINUE

      if(ivbs.eq.1)then
        write(outs,'(4a)')' could not find ',MSG(1:lnblnk(MSG)),
     &                    ' component... ',NAME
        call edisp(iuout,outs)
      endif
  100 RETURN
      END

C ********************* MFWRIT
C MFWRIT Fluid flow model file: dump common block data to file.
C If called from graphic network (haveglobal true) simulation 
C parameters also included at end of the file

      SUBROUTINE MFWRIT(IFPRB)

#include "net_flow.h"
#include "net_flow_data.h"

C To signal to MFWRIT that globals are available from graphic network.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

C Simulation preferences as per simcon.F in esrubps, some of which filled via a
C graphic network file.
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

C      write(6,*) 'haveglobal',haveglobal

      REWIND(IFPRB,ERR=999)

C Number of nodes, components, interconnections and wind reduction.
      WRITE(IFPRB,1010,IOSTAT=IOS,ERR=999) NNOD,NCMP,NCNN,WRED
 1010 FORMAT(3I5,4X,F5.3,4X,
     &      '(nodes, components, connections, wind reduction)')

C Node data.
C      IF(NNOD.EQ.0) GOTO 999
      WRITE(IFPRB,1020,IOSTAT=IOS,ERR=999)
 1020 FORMAT(' Node         Fld. Type',
     &       '   Height    Temperature    Data_1       Data_2')

      DO 30 INOD=1,NNOD
        IF(ITND(INOD).EQ.0) THEN
          WRITE(IFPRB,1030,IOSTAT=IOS,ERR=999)
     &         NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),HNOD(INOD,3),
     &         TNOD(INOD),SUPNOD(INOD,1),SUPNOD(INOD,2)
        ELSE
          WRITE(IFPRB,10301,IOSTAT=IOS,ERR=999)
     &         NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),HNOD(INOD,3),
     &         NDNAM(ITND(INOD)),SUPNOD(INOD,1),SUPNOD(INOD,2)
        END IF
   30 CONTINUE
 1030 FORMAT(1X,A12,I5,I5,1X,G12.5,1X,G12.5,   1X,G12.5,1X,G12.5)
10301 FORMAT(1X,A12,I5,I5,1X,G12.5,1X,3X,A12,3X,1X,G12.5,1X,G12.5)

C Component data
C      IF(NCMP.EQ.0) GOTO 999
      WRITE(IFPRB,1040,IOSTAT=IOS,ERR=999)
 1040 FORMAT(' Component    Type C+ L+ Description')

      DO 50 ICMP=1,NCMP
        WRITE(IFPRB,1050,IOSTAT=IOS,ERR=999)
     &      CMNAM(ICMP),ITPCMP(ICMP),ISDCMP(ICMP),ISDCNN(ICMP),
     &      LTPCMP(ICMP)
        WRITE(IFPRB,   *,IOSTAT=IOS,ERR=999)
     &      (SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
   50 CONTINUE
 1050 FORMAT(1X,A12,I5,I3,I3,1X,A)

C Connections data
C << check if NDNAM(NDSCNN(ICNN,1)) and NDNAM(NDSCNN(ICNN,2))
C << are used. Probably does not have to be written.
      WRITE(IFPRB,1060,ERR=999)
 1060 FORMAT(' +Node         dHght   -Node         dHght',
     &'   via Component')

C Implied do loop in the write statement for connections.
      WRITE(IFPRB,1070,IOSTAT=IOS,ERR=999)
     &     (NDNAM(NODPS(ICNN)),HGTPS(ICNN),
     &      NDNAM(NODNE(ICNN)),HGTNE(ICNN),
     &       CMNAM(ITPCON(ICNN)),
     &      NDNAM(NDSCNN(ICNN,1)),NDNAM(NDSCNN(ICNN,2)),
     &      ICNN=1,NCNN)
 1070 FORMAT(1X,A12,F7.3,3X,A12,F7.3,3X,A12,3X,A12,1X,A12)

C If simulation preferences were defined write tag:data at end of the file.
      if(haveglobal)then
        write(IFPRB,'(a,i2,a)') '*IPSMOD',IPSMOD,'  # stack calc'
        write(IFPRB,'(a,i2,a)') '*MFBSNC',MFBSNC,'  # timing'
        write(IFPRB,'(a,i2,a)') '*MSLVTP',MSLVTP,'  # flow solution'
        write(IFPRB,'(a,i2,a)') '*MFTRAC',MFTRAC,'  # trace level'
      endif
  100 RETURN

C Error trap on write error
  999 if(IOS.eq.2)then
        CALL USRMSG(' ',
     &  ' MFWRIT: no permission to write fluid flow file!','W')
      else
        CALL USRMSG(' ',
     &  ' MFWRIT: error writing fluid flow file!','W')
      endif
      GOTO 100

      END

C ********************* MF3DWRIT
C MFWRIT 3D Fluid flow model file: dump common block data to file.
C If called from graphic network (haveglobal true) simulation 
C parameters also included at end of the file
      SUBROUTINE MF3DWRIT(IFPRB)

#include "net_flow.h"
#include "net_flow_data.h"
      integer lnblnk  ! function definition
      INTEGER, PARAMETER :: MNWKTYP=6  ! from gnetwk.h

C To signal to MF3DWRIT that globals are available from graphic network.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

C Simulation preferences as per simcon.F in esrubps, some of which filled via a
C graphic network file.
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12
      COMMON/NWKTYP/INWKTYP,vergnf
      INTEGER inwktyp
      REAL vergnf   ! 1.0 is 3D variant of ASCII network
      
      CHARACTER outs*124,outsd*124,tab*1,dstmp*24
      character position*52,dposition*52,supl*32,supld*32
      character com*124

C      write(6,*) 'haveglobal',haveglobal

      REWIND(IFPRB,ERR=999)
      vergnf=1.0
      tab=','
      WRITE(IFPRB,'(A,f4.1)',iostat=ios,err=999)'*Flow_network',
     &  vergnf
      call dstamp(dstmp)
      write(IFPRB,'(3a)',iostat=ios,err=999) '*Date',tab,dstmp

C Write out the network description
      write(IFPRB,'(3a)',iostat=ios,err=999) '*Doc',tab,
     &  NWKDSC(1:LNBLNK(NWKDSC))

C Write out the network type
      write(IFPRB,'(4a,i1,a)',iostat=ios,err=999) '*Domain',tab,
     &  'Flow',tab,INWKTYP,'  # domain for this network'

C Write out windspeed reduction.
      write(IFPRB,'(2a,F5.3,a)',iostat=ios,err=999) '*windpressure',
     &  tab,WRED,'  # wind speed reduction'

C Node data.
      IF(NNOD.gt.0)then

C Write tag line.
        write(IFPRB,'(a)',iostat=ios,err=999) 
     &    '# tag name fluid type assoc zone & surface'
        write(IFPRB,'(a)',iostat=ios,err=999) '# position & data'

        DO INOD=1,NNOD

C Include which zone & surface the node is associated with. For
C internal nodes the surface name will be '-'.
          ln_nn=lnblnk(NDNAM(INOD))
          ln_nza=lnblnk(NODASSOC(INOD,1))
          ln_nsa=lnblnk(NODASSOC(INOD,2))
          if(NDTYP(INOD).eq.0)then
            write(com,'(a)') '   # internal unknown pressure'
          elseif(NDTYP(INOD).eq.1)then
            write(com,'(a)') '   # internal known pressure'
          elseif(NDTYP(INOD).eq.2)then
            write(com,'(a)') '   # boundary known pressure'
          elseif(NDTYP(INOD).eq.3)then
            write(com,'(a)') '   # boundary wind pressure'
          endif
          write(position,'(3f8.4)') HNOD(INOD,1),HNOD(INOD,2),
     &      HNOD(INOD,3)
          call SDELIM(position,dposition,'C',IW)  ! make , separated
          IF(ITND(INOD).EQ.0) THEN
            write(ifprb,'(4a,i1,a,i1,5a)') '*node',tab,
     &        NDNAM(INOD)(1:ln_nn),tab,NDFLD(INOD),tab,NDTYP(INOD),
     &        tab,NODASSOC(INOD,1)(1:ln_nza),tab,
     &        NODASSOC(INOD,2)(1:ln_nsa),com(1:lnblnk(com))
            write(supl,'(3F9.4)') TNOD(INOD),SUPNOD(INOD,1),
     &        SUPNOD(INOD,2)
            call SDELIM(supl,supld,'C',IW)  ! make , separated
            write(ifprb,'(4a)') dposition(1:lnblnk(dposition)),
     &        tab,supld(1:lnblnk(supld)),'  # position temp data'
          ELSE
            write(ifprb,'(4a,i1,a,i1,5a)') '*node',tab,
     &        NDNAM(INOD)(1:ln_nn),tab,NDFLD(INOD),tab,NDTYP(INOD),
     &        tab,NODASSOC(INOD,1)(1:ln_nza),tab,
     &        NODASSOC(INOD,2)(1:ln_nsa),com(1:lnblnk(com))
            write(supl,'(2F9.4)') SUPNOD(INOD,1),SUPNOD(INOD,2)
            call SDELIM(supl,supld,'C',IW)  ! make , separated
            write(ifprb,'(6a)') dposition(1:lnblnk(dposition)),
     &        tab,NDNAM(ITND(INOD)),tab,supld(1:lnblnk(supld)),
     &        '  # position ref node data' 
          ENDIF
        ENDDO
      ENDIF

C Component data
      IF(NCMP.gt.0)then
        WRITE(IFPRB,'(a)',IOSTAT=IOS,ERR=999)
     &    '# tag name assoc zone & surface type C+ L+'
        WRITE(IFPRB,'(a)',IOSTAT=IOS,ERR=999)
     &    'start&end positions  description'
        WRITE(IFPRB,'(a)',IOSTAT=IOS,ERR=999) '# data'

        DO ICMP=1,NCMP
          lncnn=lnblnk(CMNAM(ICMP))
          ln_nza=lnblnk(CMPASSOC(ICMP,1))
          ln_nsa=lnblnk(CMPASSOC(ICMP,2))

C Make up comment for each component type.
          IF(ITPCMP(ICMP).EQ. 10) THEN
            WRITE(com,'(a)')'  # Fluid coef a exponent b'
          ELSE IF(ITPCMP(ICMP).EQ. 11) THEN
            WRITE(com,'(a)')'  # Fluid rated flow nb devices'
          ELSE IF(ITPCMP(ICMP).EQ. 12) THEN
            WRITE(com,'(a)')
     &      ' Fluid coef a exponent b max val switch val'
          ELSE IF(ITPCMP(ICMP).EQ. 15) THEN
            WRITE(com,'(a)')'  # Fluid coef a exponent b'
          ELSE IF(ITPCMP(ICMP).EQ. 17) THEN
            WRITE(com,'(a)')' # Fluid coef a exponent b'
          ELSE IF(ITPCMP(ICMP).EQ. 20) THEN
            WRITE(com,'(2a)')'  # Fluid coef a (Pa.s/m^3)',
     &      ' exponent b (Pa.(s/m^3)^2)'
          ELSE IF(ITPCMP(ICMP).EQ. 25) THEN
            WRITE(com,'(2a)')'  # Fluid coef a (Pa.s/kg)',
     &        ' exponent b (Pa.(s/kg)^2)'
          ELSE IF(ITPCMP(ICMP).EQ. 30) THEN
            WRITE(com,'(a)')'  # Fluid flow rate (m^3/s)'
          ELSE IF(ITPCMP(ICMP).EQ. 35) THEN
            WRITE(com,'(a)')'  # Fluid flow rate (kg/s)'
          ELSE IF(ITPCMP(ICMP).EQ. 40) THEN
            WRITE(com,'(2a)')'  # Fluid opening area (m^2)',
     &        ' discharge factor (-)'
          ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
            WRITE(com,'(a)')'  # Fluid path length(m) opening radius(m)'
          ELSE IF(ITPCMP(ICMP).EQ.110) THEN
            WRITE(com,'(a)')'  # Fluid opening area(m)'
          ELSE IF(ITPCMP(ICMP).EQ.120) THEN
            WRITE(com,'(a)')'  # Fluid crack width(m) crack length(m)'
          ELSE IF(ITPCMP(ICMP).EQ.130) THEN
            WRITE(com,'(2a)')'  # Fluid width height ref ht',
     &        ' discharge factor'
          ELSE IF(ITPCMP(ICMP).EQ.210) THEN
            WRITE(com,'(a)')
     &      '  # Fluid hydr diam x-sect conduit ln roughness loss fac.'
          ELSE IF(ITPCMP(ICMP).EQ.211) THEN
            WRITE(com,'(a)')
     &      '  # Fluid cowl hyd dia loc loss coef B coef n include wind'
          ELSE IF(ITPCMP(ICMP).EQ.220) THEN
            WRITE(com,'(2a)')
     &      '  # Fluid hydr diam x-sect conduit ln roughness loss fac.',
     &      ' x-sect of junct, Ccp coef a0 - a5.'
          ELSE IF(ITPCMP(ICMP).EQ.230) THEN
            WRITE(com,'(2a)')
     &      '  # Fluid hydr diam x-sect conduit ln roughness loss fac.',
     &      ' x-sect of junct, Ccp coef a0 - a5.'
          ELSE IF(ITPCMP(ICMP).EQ.240) THEN
            WRITE(com,'(2a)')
     &      '  # Fluid hydr diam x-sect conduit ln roughness loss fac.',
     &      ' x-sect of junct, Ccp coef a0 - a9.'
          ELSE IF(ITPCMP(ICMP).EQ.250) THEN
            WRITE(com,'(2a)')
     &      '  # Fluid hydr diam x-sect conduit ln roughness loss fac.',
     &      ' x-sect of junct, Ccp coef a0 - a9.'
          ELSE IF(ITPCMP(ICMP).EQ.310) THEN
            WRITE(com,'(a)')
     &      '  # Fluid lower & upper vol rate flow coef a0 - a3'
          ELSE IF(ITPCMP(ICMP).EQ.410) THEN
            WRITE(com,'(2a)')
     &      '  # Fluid density pres dif lin/log open & closed vol flow',
     &      ' low validity limit'
          ELSE IF(ITPCMP(ICMP).EQ.420) THEN
            WRITE(com,'(a)')'  # Fluid, x-sec, coef a0-a3'
          ELSE IF(ITPCMP(ICMP).EQ.460) THEN
            WRITE(com,'(a)')
     &      '  # Fluid, flow when S < Ssp, flow when S > Ssp.'
          ELSE IF(ITPCMP(ICMP).EQ.500) THEN
            WRITE(com,'(a)')
     &      '  # multi configuration flow component '
          endif

C Write three lines for each component.
C << TAKE note of any non ISDCNN for use when writing *cnn >>
          write(ifprb,'(8a,i3,a,i1,a,i1)') '*cmp',tab,
     &      CMNAM(ICMP)(1:lncnn),tab,CMPASSOC(ICMP,1)(1:ln_nza),tab,
     &      CMPASSOC(ICMP,2)(1:ln_nsa),tab,ITPCMP(ICMP),tab,
     &      ISDCMP(ICMP),tab,ISDCNN(ICMP)

C If start=end then write start only, if they differ write 6
C numbers for position. << additional way points not yet done >>
          if(NWPCMP(ICMP).eq.0)then
            write(position,'(a,3f8.4)') '0 ',HCMP(ICMP,1,1),
     &        HCMP(ICMP,1,2),HCMP(ICMP,1,3)
          elseif(NWPCMP(ICMP).eq.1)then 
            write(position,'(a,6f8.4)') '1 ',HCMP(ICMP,1,1),
     &        HCMP(ICMP,1,2),HCMP(ICMP,1,3),HCMP(ICMP,2,1),
     &        HCMP(ICMP,2,2),HCMP(ICMP,2,3)
          endif
          call SDELIM(position,dposition,'C',IW)  ! make , separated
          write(ifprb,'(3a)') dposition(1:lnblnk(dposition)),
     &      tab,LTPCMP(ICMP)
          WRITE(outs,*,IOSTAT=IOS,ERR=999)
     &      (SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
          call SDELIM(outs,outsd,'C',IW)  ! make , separated
          write(ifprb,'(2a)') outsd(1:lnblnk(outsd)),
     &      com(1:lnblnk(com))
        ENDDO
      ENDIF

C Connections data
C << check if NDNAM(NDSCNN(ICNN,1)) and NDNAM(NDSCNN(ICNN,2))
C << are used. Probably does not have to be written.

C Implied do loop in the write statement for connections.
C      WRITE(IFPRB,1070,IOSTAT=IOS,ERR=999)
C     &     (NDNAM(NODPS(ICNN)),HGTPS(ICNN),
C     &      NDNAM(NODNE(ICNN)),HGTNE(ICNN),
C     &       CMNAM(ITPCON(ICNN)),
C     &      NDNAM(NDSCNN(ICNN,1)),NDNAM(NDSCNN(ICNN,2)),
C     &      ICNN=1,NCNN)
C 1070 FORMAT(1X,A12,F7.3,3X,A12,F7.3,3X,A12,3X,A12,1X,A12)

      if(NCNN.gt.0)then
        WRITE(ifprb,'(a)')
     &  '# connection +node deltaZ  -node deltaZ via component'
        DO ICNN=1,NCNN
          ln_pos=lnblnk(NDNAM(NODPS(ICNN)))
          ln_neg=lnblnk(NDNAM(NODNE(ICNN)))
          ln_cmp=lnblnk(CMNAM(ITPCON(ICNN)))

C Debug.
C          write(6,*) 'connection additional ',icnn,itpcon(icnn),
C     &      ISDCNN(ITPCON(ICNN))
          if(ISDCNN(ITPCON(ICNN)).eq.0)then
            write(outs,'(4a,F7.3,3a,F7.3,2a)') '*cnn',tab,
     &        NDNAM(NODPS(ICNN))(1:ln_pos),tab,HGTPS(ICNN),tab,
     &        NDNAM(NODNE(ICNN))(1:ln_neg),tab,HGTNE(ICNN),tab,
     &        CMNAM(ITPCON(ICNN))(1:ln_cmp)
          else
            ln_scnn1=lnblnk(NDNAM(NDSCNN(ICNN,1)))
            ln_scnn2=lnblnk(NDNAM(NDSCNN(ICNN,2)))
            write(outs,'(4a,F7.3,3a,F7.3,6a)') '*cnn',tab,
     &        NDNAM(NODPS(ICNN))(1:ln_pos),tab,HGTPS(ICNN),tab,
     &        NDNAM(NODNE(ICNN))(1:ln_neg),tab,HGTNE(ICNN),tab,
     &        CMNAM(ITPCON(ICNN))(1:ln_cmp),tab,
     &        NDNAM(NDSCNN(ICNN,1))(1:ln_scnn1),tab,
     &        NDNAM(NDSCNN(ICNN,2))(1:ln_scnn2)
          endif
          write(ifprb,'(a)') outs(1:lnblnk(outs))
        ENDDO
      endif

C If simulation preferences were defined write tag:data at end of the file.
      if(haveglobal)then
        write(IFPRB,'(a,i2,a)') '*IPSMOD',IPSMOD,'  # stack calc'
        write(IFPRB,'(a,i2,a)') '*MFBSNC',MFBSNC,'  # timing'
        write(IFPRB,'(a,i2,a)') '*MSLVTP',MSLVTP,'  # flow solution'
        write(IFPRB,'(a,i2,a)') '*MFTRAC',MFTRAC,'  # trace level'
      endif
      WRITE(ifprb,'(a)') '*End_network'
      CALL ERPFREE(ifprb,IOS)

  100 RETURN

C Error trap on write error
  999 if(IOS.eq.2)then
        CALL USRMSG(' ',
     &  ' MFWRIT: no permission to write 3D fluid flow file!','W')
      else
        CALL USRMSG(' ',
     &  ' MFWRIT: error writing 3D fluid flow file!','W')
      endif
      GOTO 100

      END

