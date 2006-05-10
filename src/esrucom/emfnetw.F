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
C Mfcmpsupcheck: check flow components for correct number of supplemental
C           data items.
C  MFERR:   Error trap routine.
C  MFLIST:  List mass flow network desciption file.
C  GETND:   Returns mass flow node index matching NAME.
C  GETCN:   Returns mass flow connection index matching two node
C           names and associated component.
C  GETCMP:  Returns mass flow component index matching name ,
C  MFWRIT:  Fluid flow model file: dump common data to file.

C ************************* EMFREAD 
C EMFREAD reads a mass flow network description file.
C Reads allow comments and most input data checks are performed.
C It assumes that the file is already open.
      SUBROUTINE EMFREAD(IFPRB,ITRC,IER)

#include "net_flow.h"

C Network flow commons are as follows:
C Network size
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
C     HNOD   - height above datum (m)
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
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW6/IVALCM(MCMV)
      COMMON/MFLOW7/LVALCM(MCMV)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

      DIMENSION RVA(MCMS),NDID(MNOD)
      CHARACTER OUTSTR*124,WORD*20,NODID*12,NDID*12,CMPID*12,OUTS*124
      CHARACTER LVALCM*60,LTPCMP*60,CMNAM*12,NDNAM*12

C 'Reset' error indicator and clear node names.
      IER=0
      INOD=0
      ICMP=0
      ICNN=0
      do 9 i=1,MNOD
        NDNAM(i)=' '
  9   continue

C Read number of nodes, components, interconnections and wind reduction.
      CALL STRIPC(IFPRB,OUTSTR,99,ND,1,
     &            'mfs nodes, components & connections',IER)
      IF(IER.NE.0)RETURN
      IF(ND.GE.4)THEN
        K=0
        CALL EGETWI(OUTSTR,K,NNOD,0,MNOD,'F','no of nodes',IER)
        CALL EGETWI(OUTSTR,K,NCMP,0,MCMP,'F','no of compnt',IER)
        CALL EGETWI(OUTSTR,K,NCNN,0,MCNN,'F','no of connct',IER)
        CALL EGETWR(OUTSTR,K,WRED,0.,1.,'F','wind reduction',IER)
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
        HNOD(INOD)=VAL
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
 1001 CALL USRMSG(' Conversion error in',OUTSTR,'W')
      IER=1
      GOTO 100
      END

C ****************** MFCMPSUPCHECK 
C Mfcmpsupcheck: check flow components for correct number of supplemental
C data items.
      subroutine mfcmpsupcheck(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
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

C Type 40 common orifice volume flow rate component.
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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      character*(*) MSG
      CHARACTER LTPCMP*60,CMNAM*12
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
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12
      CHARACTER outs*124,fldtyp*6,nodtyp*20,dat1*4,dat2*4
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
      WRITE(outs,'(A,A)')'    Node      Fluid   Node Type',
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
     &         NDNAM(INOD),fldtyp,nodtyp,HNOD(INOD),
     &         TNOD(INOD),dat1,SUPNOD(INOD,1),dat2,SUPNOD(INOD,2)
        ELSE
          WRITE(outs,10301)
     &         NDNAM(INOD),fldtyp,nodtyp,HNOD(INOD),
     &         NDNAM(ITND(INOD)),dat1,SUPNOD(INOD,1),dat2,SUPNOD(INOD,2)
        END IF
        call edisp(itru,outs)
   30 CONTINUE
 1030   FORMAT(1X,A12,1x,2a,F8.4,1X,G12.5,1X,a,F9.3,2X,a,F8.3)
10301   FORMAT(1X,A12,1x,2a,F8.4,1X,A12,1X,a,F9.3,2X,a,F8.3)

C Component data.
      IF(NCMP.EQ.0) GOTO 999
      CALL EDISP(itru,' ')
      CALL EDISP(itru,' Component    Type C+ L+ Description')

      DO 50 ICMP=1,NCMP
        call edisp(itru,' ')
        WRITE(outs,1050)CMNAM(ICMP),ITPCMP(ICMP),
     &      ISDCMP(ICMP),ISDCNN(ICMP),LTPCMP(ICMP)
 1050   FORMAT(1X,A12,I5,I3,I3,1X,A60)
        CALL EDISP(itru,outs)

C Provide a heading string for each different component type.
        written=.false.
        IF(ITPCMP(ICMP).EQ. 10) THEN
          WRITE(outs,'(a,F4.1,2(a,G12.5))')' Fluid ',SUPCMP(ICMP,1),
     &      ' coef a ',SUPCMP(ICMP,2),' exponent b ',SUPCMP(ICMP,3)
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
          WRITE(outs,'(a,F4.1,a,F7.3,a,F6.3)')' Fluid ',SUPCMP(ICMP,1),
     &      ' opening area (m^2)',SUPCMP(ICMP,2),
     &      ' discharge factor (-)',SUPCMP(ICMP,3)
          CALL EDISP(itru,outs)
          written=.true.
        ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
          WRITE(outs,'(a,F4.1,a,F7.3,a,F7.3)')' Fluid ',SUPCMP(ICMP,1),
     &      ' path length (m)',SUPCMP(ICMP,2),
     &      ' opening radius (m)',SUPCMP(ICMP,3)
          CALL EDISP(itru,outs)
          written=.true.
        ELSE IF(ITPCMP(ICMP).EQ.110) THEN
          WRITE(outs,'(a,F4.1,a,F7.3)')' Fluid ',SUPCMP(ICMP,1),
     &      ' opening area (m)',SUPCMP(ICMP,2)
          CALL EDISP(itru,outs)
          written=.true.
        ELSE IF(ITPCMP(ICMP).EQ.120) THEN
          WRITE(outs,'(a,F4.1,a,F8.4,a,F7.3)')' Fluid ',SUPCMP(ICMP,1),
     &      ' crack width (m)',SUPCMP(ICMP,2),' crack length (m)',
     &      SUPCMP(ICMP,3)
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
          call edisp(itru,
     &  '   Fluid, x-sec, coef a0-a3')
        ELSE IF(ITPCMP(ICMP).EQ.460) THEN
          call edisp(itru,
     &  '   Fluid, flow when S < Ssp, flow when S > Ssp.')
        endif

C If not already written out.
        if(.NOT.written)then
          if(ISDCMP(ICMP).LE.9)then
            WRITE(outs,'(F5.1,8F9.3)')(SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
            CALL EDISP(itru,outs)
          elseif(ISDCMP(ICMP).GT.9)then
            WRITE(outs,'(F5.1,8F8.2)')(SUPCMP(ICMP,I), I=1,9)
            CALL EDISP(itru,outs)
            WRITE(outs,'(5x,9F8.2)')(SUPCMP(ICMP,I), I=10,ISDCMP(ICMP))
            CALL EDISP(itru,outs)
          endif
        endif
   50 CONTINUE

C Connections data
      IF(NCNN.EQ.0) GOTO 999
      CALL EDISP(itru,' ')
      WRITE(outs,'(2a)') ' +Node         dHght   -Node         dHght',
     &  '   Component       Z @+    Z @-'
      CALL EDISP(itru,outs)

      DO 70 ICNN=1,NCNN
        zplus=HNOD(NODPS(ICNN))+HGTPS(ICNN)
        zminus=HNOD(NODNE(ICNN))+HGTNE(ICNN)
        WRITE(outs,1070)
     &    NDNAM(NODPS(ICNN)),HGTPS(ICNN),NDNAM(NODNE(ICNN)),
     &    HGTNE(ICNN),CMNAM(ITPCON(ICNN)),zplus,zminus
 1070   FORMAT(1X,A12,F7.3,3X,A12,F7.3,3X,A12,1X,2F8.3)
        CALL EDISP(itru,outs)
   70 CONTINUE

  100 RETURN

C Error trap
  999 CALL USRMSG(' ',' MFLIST: no more information available!','W')
      GOTO 100

      END

C***** Error checking.
      SUBROUTINE CNTERR(ICMP,NEWDI)

#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      IF(ISDCMP(ICMP)-1.EQ.NEWDI)THEN
        IF(ABS(SUPCMP(ICMP,NEWDI+1)).LT.0.00001)ISDCMP(ICMP)=
     &    ISDCMP(ICMP)-1
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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)

      character*(*),MSG,NAME
      character NODE*12,NDNAM*12,outs*124

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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      character*(*),MSG,NDPOS,NDNEG
      character NODLE*12,NODRI*12,CMPNM*12,outs*124,CMPID*12
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12

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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)

      character*(*),MSG,NAME
      character CMPNM*12,outs*124
      CHARACTER LTPCMP*60,CMNAM*12

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
C MFWRIT Fluid flow model file: dump data to file.
      SUBROUTINE MFWRIT(IFPRB)

#include "net_flow.h"
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12

      REWIND(IFPRB,ERR=999)

C Number of nodes, components, interconnections and wind reduction.
      WRITE(IFPRB,1010,IOSTAT=ISTAT,ERR=999) NNOD,NCMP,NCNN,WRED
 1010 FORMAT(3I5,4X,F5.3,4X,
     &      '(nodes, components, connections, wind reduction)')

C Node data.
C      IF(NNOD.EQ.0) GOTO 999
      WRITE(IFPRB,1020,IOSTAT=ISTAT,ERR=999)
 1020 FORMAT(' Node         Fld. Type',
     &       '   Height    Temperature    Data_1       Data_2')

      DO 30 INOD=1,NNOD
      IF(ITND(INOD).EQ.0) THEN
         WRITE(IFPRB,1030,IOSTAT=ISTAT,ERR=999)
     &         NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),HNOD(INOD),
     &         TNOD(INOD),SUPNOD(INOD,1),SUPNOD(INOD,2)
      ELSE
         WRITE(IFPRB,10301,IOSTAT=ISTAT,ERR=999)
     &         NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),HNOD(INOD),
     &         NDNAM(ITND(INOD)),SUPNOD(INOD,1),SUPNOD(INOD,2)
      END IF
   30 CONTINUE
 1030 FORMAT(1X,A12,I5,I5,1X,G12.5,1X,G12.5,   1X,G12.5,1X,G12.5)
10301 FORMAT(1X,A12,I5,I5,1X,G12.5,1X,3X,A12,3X,1X,G12.5,1X,G12.5)

C Component data
C      IF(NCMP.EQ.0) GOTO 999
      WRITE(IFPRB,1040,IOSTAT=ISTAT,ERR=999)
 1040 FORMAT(' Component    Type C+ L+ Description')

      DO 50 ICMP=1,NCMP
        WRITE(IFPRB,1050,IOSTAT=ISTAT,ERR=999)
     &      CMNAM(ICMP),ITPCMP(ICMP),ISDCMP(ICMP),ISDCNN(ICMP),
     &      LTPCMP(ICMP)
        WRITE(IFPRB,   *,IOSTAT=ISTAT,ERR=999)
     &      (SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
   50 CONTINUE
 1050 FORMAT(1X,A12,I5,I3,I3,1X,A)

C Connections data
C << check if NDNAM(NDSCNN(ICNN,1)) and NDNAM(NDSCNN(ICNN,2))
C << are used. Probably does not have to be written.
      WRITE(IFPRB,1060,ERR=999)
 1060 FORMAT(' +Node         dHght   -Node         dHght',
     &'   via Component')

      WRITE(IFPRB,1070,IOSTAT=ISTAT,ERR=999)
     &     (NDNAM(NODPS(ICNN)),HGTPS(ICNN),
     &      NDNAM(NODNE(ICNN)),HGTNE(ICNN),
     &       CMNAM(ITPCON(ICNN)),
     &      NDNAM(NDSCNN(ICNN,1)),NDNAM(NDSCNN(ICNN,2)),
     &      ICNN=1,NCNN)
 1070 FORMAT(1X,A12,F7.3,3X,A12,F7.3,3X,A12,3X,A12,1X,A12)

  100 RETURN

C Error trap on write error
  999 CALL USRMSG(' ',
     &  ' MFWRIT: error writing fluid flow file!','W')
      call usrmsg(' ',' ','-')
      GOTO 100

      END

