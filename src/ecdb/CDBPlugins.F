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
C
C The routines in this file translate data read from the generic
C components database for use within the appropriate ESP-r technical
C domains.
C CDB2Plant -  plant systems translator
C CDB2Power -  electrical systems translator
C CDB2Flow  -  flow translator
C CDB2REM   -  active materials/building integrated RE translator

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      SUBROUTINE CDB2Plant(ID_NO,DETAIL)
C ----------------------------------------------------------------------
C This routine establishes the plant commons for a component IPCOMP.
C detail determines what to pull from the data: 
C 'light' - get heading data only [i.e. data descriptors]
C 'detailed'  - get numerical data as well.
C
C The routine needs to be used immediately after a call to CDBGetItemData
C ***********************************************************************
C The main plant matrix description COMMON variables are established:
C NPCDAT    : general array holding miscellaneous data on each plant
C             component ? as follows:
C       ?,1 : code number as assigned in the configuration file
C       ?,2 : start record address (in plant database) for component ?
C       ?,3 : plant component model type number for this component
C       ?,4 : pointer to appropriate CMPnnS and CMPnnC routine for
C             component ? (obtained from NPCDAT(?,3)/10)
C       ?,5 : index which defines the location of the first coefficient
C             of component ? within the IPOFS1, IPOFS2 & PC vectors
C       ?,6 : number of non-zero matrix entries (coefficients)
C             associated with this component
C       ?,7 : number of coefficients external to component ?. That is,
C             the number of inter-component coupling coefficients
C       ?,8 : number of nodes comprising component ?
C       ?,9 : row number (thus column number!) defining location of
C             component sub-matrix template in overal plant matrix; also
C             the ZMAT index for component ?
C IPOFS1 &  : are held for each network matrix coefficient and
C  IPOFS2     define the row and column positions respectively
C NPMCOE    : is the total number of coefficients in the network matrix
C NPNOD     : is the total number of nodes in the plant configuration
C             = number of coupling coefficients in the network matrix
C             = number of equations coupling coefficients
C NPMTYP    : dictates the number of matrices which will exist:
C             1 - energy balance only
C             2 - energy balance + single phase mass balance
C             3 - energy balance + two phase mass balance
C NDCON     : defines, node wise , whether connection potential exists
C             >0 - gives expected number of receiving connections
C             =0 - no entering or leaving external connections allowed
C             -1 - only leaving connections allowed
C ISV       : holds, node wise, fluid type & coefficient generator
C             model capabilities:
C             0   10   20 : node represents water + ....
C             1   11   21 : node represents dry/moist air + ....
C             9   19   29 : node represents some solid material only
C             /\----------- model suitable for energy balance only
C                 /\------- model suitable for energy balance +
C                           single phase mass balance
C                      /\-  model suitable for energy balance +
C                           two phase mass balances
C IDENC &   : identify a component and its internal node from
C  IDENN      a node's (equation's) network number
C ADATA     : static manufacturers data associated with a component
C ICONDX    : defines - for any component, node and coupling (there
C             may be more than one) - the connection number
C ICONTP    : defines the state variable type of the sending node, in
C             order to be able to calculate the fluid specific heat,
C             fluid enthalpy etc. (equals ISV of sending node)
C PCONDR    : holds (irrespective of connection type) the ratio between
C             the mass flow rate entering the receiving node and the
C             mass flow rate through the connection (=1 when mfs active)
C nrowc     : holds the following information:
C      ?,1,1: number of coefficients in each row for energy.
C      ?,1,2: number of coefficients in each row for 1 phase mass flow.
C      ?,1,3: number of coefficients in each row for 2 phase mass flow.
C      ?,2,1: location of first coefficient in each row in 'iptrod' for
C             energy.
C      ?,2,2: location of first coefficients in each row for
C             1 phase mass flow.
C      ?,2,3: location of first coefficients in each row for
C             2 phase mass flow.
C iptrd     : Holds pointers (i.e location) to diagonal terms in
C             coefficient matrix. Required by sparse matrix solver.
C iptrod    :
C        ?,1: Holds pointers to energy coefficients in array 'PC'.
C        ?,2: Holds pointers to 1 phase mass balance coefficients in
C             array 'PC'.
C        ?,3: Holds pointers to 2 phase mass balance coefficients in
C             array 'PC'.
C ntcoe     : Total number of coefficients in compact matrix including
C             new non-zero elements.

#include "building.h"
#include "gencompDB.h"
#include "gencompDB_common.h"
#include "plant.h"
#include "net_flow.h"

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
 
      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)

      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

      common/adopdesc/adopdsc(mpcom,15)

      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)

      COMMON/C9plus/UCdbNam(MPCOM),CdbCat(MPCOM),CDBDesc(MPCOM)
      COMMON/IsTRNSYS/Is_TRNSYS(MPCOM)


      CHARACTER adopdsc*30, CdbCat*32, CDBDesc*72
      character mscdsc*68, cvrdsc*68

      character outs*124, detail*12, UCdbNam*16

      REAL QDATA,PCAOUT
      INTEGER napdat
      LOGICAL Is_TRNSYS

C Transferred parameters
      INTEGER  LOCCON(MPCOM)

      IPCOMP=ID_NO

C Initialisation
C First zero the plant data (initially all held in the ADATA array)
      IF(DETAIL(1:LNBLNK(DETAIL)).EQ.'detailed') THEN
        NADATA=0
        NBDATA=0
        NCDATA=0
        DO 10 IADATA=1,MADATA
          ADATA(IPCOMP,IADATA)=0.0
   10   CONTINUE
      ELSE
        IF(NPI(IPCOMP).GT.0)THEN
          DO 12 IADATA=1,MADATA
            ADATA(IPCOMP,IADATA)=TADATA(IPCOMP,IADATA)
   12     CONTINUE
        ENDIF
      ENDIF

C Decode static data.
      K=0
      J=0
      L=0

      NAD=0
      NAA=0
      NAC=0

      s_MISCD=0

      NXT=1

      CDBCat(IPCOMP)=S_ITEM_CAT
      CDBDesc(IPCOMP)=S_ITEM_MENU
      DO 105 IDAT=1,S_NATRIBS

C Read the number of nodes.
      IF(S_MENUATR(IDAT)(1:12).EQ.'no. of nodes')THEN
        READ(S_ATRIB(IDAT,1),'(I3)') s_NNODE 
c        write(*,*) 'writing nnode ',S_ATRIB(IDAT,1)

C Read in the component CODE
      ELSEIF(S_MENUATR(IDAT)(1:14).EQ.'component code')THEN
        READ(S_ATRIB(IDAT,1),'(I3)') s_ICODE 
c        write(*,*) 'writing icode ',S_ATRIB(IDAT,1)        

      ELSEIF(S_MENUATR(IDAT)(1:12).EQ.'no. non zero')THEN
        READ(S_ATRIB(IDAT,1),'(I3)') s_NMATX  
c        write(*,*) 'writing nmatx ',S_ATRIB(IDAT,1)

      ELSEIF(S_MENUATR(IDAT)(1:15).EQ.'matrix position')THEN
        IF(K+1.LE.s_NMATX) THEN
          K=K+1
          READ(S_ATRIB(IDAT,1),'(I3)') s_NDPOS(K) 
c          write(*,*) 'writing ndpos ',S_ATRIB(IDAT,1)
        ELSE
          WRITE(OUTS,*) 'cdb2plant: error decoding matrix element', K
          CALL USRMSG(OUTS,' ','W')
        ENDIF

C Decode the external connections potential.
      ELSEIF(S_MENUATR(IDAT)(1:15).EQ.'no. connections')THEN
        IF(J+1.LE.s_NNODE) THEN
          J=J+1
          READ(S_ATRIB(IDAT,1),'(I3)') NDCON(IPCOMP,J) 
c          write(*,*) 'writing ndcon',S_ATRIB(IDAT,1)
        ELSE
          WRITE(OUTS,*) 'cdb2plant: error decoding nodal connection', J
          CALL USRMSG(OUTS,' ','W')
        ENDIF

C Read the ISV of each node.
      ELSEIF(S_MENUATR(IDAT)(1:11).EQ.'ISV of node')THEN
        IF(L+1.LE.s_NNODE) THEN
          L=L+1
          READ(S_ATRIB(IDAT,1),'(I3)')ISV(IPCOMP,L) 
c          write(*,*) 'writing isv ',S_ATRIB(IDAT,1)
        ELSE
          WRITE(OUTS,*) 'cdb2plant: error decoding nodal connection', J
          CALL USRMSG(OUTS,' ','W')
        ENDIF

C Read the # of ADATA
      ELSEIF(S_MENUATR(IDAT)(1:9).EQ.'no. adata')THEN
        READ(S_ATRIB(IDAT,1),'(I3)') NADATA
c        write(*,*) 'writing nadata ',NADATA
        s_MISCD=s_MISCD+NADATA
c        write(*,*) 'writing nmisc ',s_MISCD
C Read the # of BDATA
      ELSEIF(S_MENUATR(IDAT)(1:9).EQ.'no. bdata')THEN
          READ(S_ATRIB(IDAT,1),'(I3)') NBDATA
c          write(*,*) 'writing nbdata ',S_ATRIB(IDAT,1)
C            NMISC(IPCOMP)=s_MISC
          s_MISCD=s_MISCD+NBDATA
c          write(*,*) 'nmisc ',s_miscd            

C Read the # of CDATA
      ELSEIF(S_MENUATR(IDAT)(1:9).EQ.'no. cdata')THEN
          READ(S_ATRIB(IDAT,1),'(I3)') NCDATA 
c          write(*,*) 'writing ncdata ',S_ATRIB(IDAT,1)
          NCI(IPCOMP)=NCDATA
        
C Special for TRNSYS component
C For a trnsys component, read its data from a data file.
C Note that in plant database, the trnsys has dummy values only.
      ELSEIF(S_MENUATR(IDAT)(1:14).EQ.'component code')THEN
c        iend=lnblnk(UCdbNam(IPCOMP))
        IF(UCdbNam(IPCOMP)(1:2).EQ.'TR')THEN
          Is_TRNSYS(IPCOMP)=.TRUE.
          LOCCON=s_NMATX+NXT
          CALL TRNPMXT(IPCOMP,s_ICODE,NXT,s_MATPOS,LOCCON)
        END IF

C Read in the data values and descriptions
      ELSEIF(s_TAGATR(IDAT,1)(1:12).EQ.'thermal_data')THEN
        NAD=NAD+1
        MSCDSC(NAD)=s_MENUATR(IDAT)
c        write(*,*) 'writing desc ',s_MENUATR(IDAT)
        IF(DETAIL(1:LNBLNK(DETAIL)).EQ.'detailed')THEN
          READ(S_ATRIB(IDAT,1),'(F14.5)') TADATA(IPCOMP,NAD) 
c          write(*,*) 'writing adata ',S_ATRIB(IDAT,1)
        ENDIF
        

C Decode control parameters and description
      ELSEIF(s_TAGATR(IDAT,1)(1:7).EQ.'control')THEN
        NAC=NAC+1
        CVRDSC(NAC)=S_MENUATR(IDAT)
        IF(DETAIL(1:LNBLNK(DETAIL)).EQ.'detailed')THEN
          READ(S_ATRIB(IDAT,1),'(F14.5)') CDATA(IPCOMP,NAC)
c          write(*,*) 'writing cdata ',S_ATRIB(IDAT,1)
        ENDIF

C Read additional output. 
      ELSEIF(s_TAGATR(IDAT,1)(1:7).EQ.'output')THEN
        NAA=NAA+1
        NAPDAT(IPCOMP)=NAA
        ADOPDSC(IPCOMP,NAA)=S_MENUATR(IDAT)(1:30)
c        write(*,*) 'writing aditional ',S_MENUATR(IDAT)
      ENDIF

105   CONTINUE
      
      RETURN
      END
