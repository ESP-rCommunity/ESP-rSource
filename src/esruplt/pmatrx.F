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
C      MZPMXT
C      MZPMRX
C      MZPMSU
C      MZPMSV
C      MZNASS
C      JSOLVS
C      LUDCMS
C      LUBKSS
C      cmabld
C      cmaslv

C ******************** MZPMXT ********************
C MZPMXT accesses the plant components database (opened on IUNIT) and
C extracts the data needed to establish the templates for the plant
C network energy, 1st phase mass, and 2nd phase mass balance.
C MZPMXT also checks that component connections are legally defined,
C sets up the pointer array ICONDX which defines - for any component,
C node and coupling (perhaps more than one) - the connection number,
C sets up the ICONTP array which defines for each plant component
C interconnection, the state variable type of the sending node, and
C in case the encapsulated version of mfs is active:
C resets the PCONDR array (mass diversion ratios) to unity, and
C checks plant/mfs connection mapping regarding fluid types
C
C This routine should check for plant component inter-connection
C types 1,2 and 4 if referenced component/nodes are downstream from
C a source of mass. This is not yet done at present.
C
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
      SUBROUTINE MZPMXT(IUNIT)
#include "plant.h"
#include "net_flow.h"
#include "building.h"

      PARAMETER (SMALL=1.0E-20)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      COMMON/C16PS/IDENC(MPNODE),IDENN(MPNODE)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

      common/packa/nrowc(mpnode,2,mpvar),iptrd(mpnode),
     &             iptrod(mcoefg,mpvar),ntcoe

C Save plant component matrix positions globally, so that they
C they can be accessed later by generic 'Zero_h2_flow' routine
      common / plant_coeff / iPMC_locations(mpcom,mpcoe)
      integer iPMC_locations

      COMMON /pcddsc/ pcdesc(maxpc), npref(mpcom)
      CHARACTER pcdesc*80
      INTEGER NPREF


C Common for primitive parts transport delay flag. Initialize to
C False.
      COMMON/PP_DELAY/bDELAY_FLAG
      logical bDELAY_FLAG

      DIMENSION   NMISCD(MPCOM),KNHIT(MPNODE),ZDR(MPNODE),
     &            LOCCON(MPCOM),NDPOS(MPCOE),IC(MPNODE)
      INTEGER     ZZ(4)
      CHARACTER*5 A(MPNODE,MPNODE)
      character*68 dumstr
      character outs*124

C Suggested improvement:
C Initialize PP transport delay flag to FALSE. If the subroutine
C TRNSDLAY is called, it will set bDELAY_FLAG = TRUE. Otherwise,
C primitive part historical data (in the DELAY common) will
C not be transproted by subtoutines MZNASS / plt_TimeRow_Manipulate
      bDELAY_FLAG = .FALSE.

C Trace output
      IF(ITC.GT.0.AND.(ITRACE(34).NE.0.OR.ITRACE(40).NE.0))
     &   WRITE(ITU,*) ' Entering subroutine MZPMXT'

C First initialize
      MATPOS=1
      NXT=1

C Now, component by component, get data needed for establishing
C the templates for the plant network matrix
      DO 50 IPCOMP=1,NPCOMP
C
C First zeroize ADATA
      DO 10 IADATA=1,MADATA
      ADATA(IPCOMP,IADATA)=0.0
   10 CONTINUE
C
C Read component code ICODE, number of nodes NNODE, number of non-zero
C matrix entries NMATX and number of miscelaneous data items NMISC
      IREC=NPCDAT(IPCOMP,2)
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=994)
     &     AA,ICODE,ZZ,NNODE,NMATX,NMISC

C For a trnsys component, read its data from a data file.
C Note that in plant database, the trnsys has dummy values only.
C The code number in the plant database is used here to judge whether
C it is a trnsys component. Since the code number (=85) is hard-coded
C here, any change of this code number in the database will require
C the changes in the source code. This may be potentially improved
C by not referring to the code number.
      IF(NPREF(IPCOMP) .EQ. 85) THEN
	CALL TRNPMXT(IPCOMP, ICODE, NXT, MATPOS, LOCCON)
        CYCLE
      END IF

      NMISCD(IPCOMP)=NMISC

C Read new record data related to meta components
      irec=irec+1
      read(iunit,rec=irec,iostat=istat,err=994)
     &    ictype,mncmp,mncon,ndum1,ndum2,ndum3,napout
      napdat(ipcomp)=napout

C For single components, do the following:
C Read external connection potential for each node
      if(ictype.eq.0) then
         IREC=IREC+2
         READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=994)
     &     (NDCON(IPCOMP,INOD),INOD=1,NNODE)

C Read each node's state variable identifier
         IREC=IREC+1
         READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=994)
     &     (ISV(IPCOMP,INOD),INOD=1,NNODE)
C Check that the component model is capable of generating coefficients
C for all state variables as implied by the number of matrices called
C for in the system configuration file.
C
C HOT3000:
C Note: Coefficient generators are not necessarly required for all
C components in a hydrogen mass flow matrix solution (NPTYMP = 4).
C If hydrogen mass flow matrix is active (NPMTYP > 3), then assume
C that minumum matrix threshold is equal to or greater then
C (NPMTYP - 1)
         if (NPMTYP .gt. 3 ) then
            min_PM_threshold = NPMTYP - 1
         else
            min_PM_threshold = NPMTYP
         endif


         DO 20 INOD=1,NNODE
           IF(ISV(IPCOMP,INOD).LT.( min_PM_threshold -1 )*10) THEN
             call elinc(2)
             WRITE(IUOUT,*) ' Plant matrix type ',NPMTYP,' cannot be',
     &                  ' established from'
             WRITE(IUOUT,*) ' component ',IPCOMP,' node ',INOD,
     &                  ' coefficient generator'
             STOP ' MZPMXT: unresolvable error'
           END IF
   20    CONTINUE

C Read matrix coefficient positions NDPOS and static manufacturers data
C The assumption is made here that the plant component database
C has 20 word records. If this is changed the following assignment
C must be altered
         IRECLN=20
         NR=1
   22    NRR=NR+IRECLN-1
         IREC=IREC+1
         READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=994) (NDPOS(J),J=NR,NRR)
         NR=NR+IRECLN
         IF(NR.LE.NMATX) GOTO 22

C See if new parameters have been specified for this component
         IF(NPI(IPCOMP).GT.0) THEN
            DO 707 ITEM=1,NMISC
  707          ADATA(IPCOMP,ITEM)=TADATA(IPCOMP,ITEM)

C If no then read default values.
         ELSE
            do 24 item=1, nmisc
               irec=irec+1
               read(iunit,rec=irec,iostat=istat,err=994)
     &             dumstr,adata(ipcomp,item)
   24       continue
         ENDIF

C Fill the general plant component array
         NPCDAT(IPCOMP,3)=ICODE
         NPCDAT(IPCOMP,4)=ICODE/10
         NPCDAT(IPCOMP,5)=NXT
         NPCDAT(IPCOMP,6)=NMATX
         NPCDAT(IPCOMP,8)=NNODE
         NPCDAT(IPCOMP,9)=MATPOS

C Establish the coefficient location counter LOCCON for this component
         LOCCON(IPCOMP)=NXT+NMATX

C Set coefficient positions
C Establish the plant coefficient off-set vectors for this component
         DO 26 J=1,NMATX
            K=NDPOS(J)-1
            IPOFS1(NXT)=MATPOS+K/NNODE
            ICOL=MATPOS+MOD(K,NNODE)
            IPOFS2(NXT,1)=ICOL
            IPOFS2(NXT,2)=ICOL
            IPOFS2(NXT,3)=ICOL
            IPOFS2(NXT,4)=ICOL
            NXT=NXT+1

C Save matrix locations for later use
            iPMC_locations(ipcomp,j) =  NDPOS(J)
   26    CONTINUE
         MATPOS=MATPOS+NNODE

C Loop over all plant component inter-connections to get connection
C coefficients; in defining connections between nodes, the operative
C word is 'receive'.
C First reset each node's connection counter KNHIT
         NCNS=0
         DO 30 INOD=1,NNODE
            KNHIT(INOD)=0
   30    CONTINUE
         IF(NPCON.GT.0) THEN
            DO 39 IPCON=1,NPCON

C Check if connection relates to current component as the receiving one
               IF(IPC1(IPCON).NE.IPCOMP) GOTO 32
               N=IPN1(IPCON)
               IF(N.LT.1.OR.N.GT.NNODE.OR.NDCON(IPCOMP,N).LE.0) THEN
                  call elinc(1)
                  WRITE(IUOUT,*) ' No receiving connection allowed',
     &                     ' to component ',IPCOMP,' , node ',N
                  STOP ' MZPMXT: unresolvable error'
               END IF
               KNHIT(N)=KNHIT(N)+1
               IF(KNHIT(N).GT.NDCON(IPCOMP,N)) THEN
                  call elinc(2)
                  WRITE(IUOUT,*) ' Max. number of connections exceeded',
     &                     ' for component ',IPCOMP,', node ',N
                  STOP ' MZPMXT: unresolvable error'
               END IF
               NCNS=NCNS+1
               GOTO 39

C Check if connection relates to current component as the sending one,
C which is only possible in case of a type 3 inter-connection
  32           IF(IPC2(IPCON).NE.IPCOMP.OR.IPCT(IPCON).NE.3) GOTO 39
               N=IPN2(IPCON)
               IF(N.LT.1.OR.N.GT.NNODE.OR.NDCON(IPCOMP,N).EQ.0) THEN
                  call elinc(1)
                  WRITE(IUOUT,*) ' No connection allowed to component ',
     &                     IPCOMP,' , node ',N
                  STOP ' MZPMXT: unresolvable error'
               END IF
   39       CONTINUE
         END IF

C Establish component/node identification arrays IDENC and IDENN
         DO 40 INOD=1,NNODE
            INN=NPCDAT(IPCOMP,9)+INOD-1
            IDENC(INN)=IPCOMP
            IDENN(INN)=INOD
   40    CONTINUE

C Now set number of coefficients external to component
         NPCDAT(IPCOMP,7)=NCNS

C and increment NXT to leave space for the IPOFS1 & IPOFS2
C array external connection entries
         NXT=NXT+NCNS

C For meta components, read sub-components data and
C their inter-connections. ( future work ).
      elseif(ictype.eq.1) then
         write(iuout,*) ' Meta components not implemented yet '
         stop ' Program terminated.'
      endif

C Proceed with next component
   50 CONTINUE

C Now check for each component inter-connection, the fluid type
C of the receiving node with the type of fluid being send
C NB this is done separate of the component/connection loop above
C    to enable random user definition of connections
      IF(NPCON.GT.0) THEN
         DO 64 IPCOMP=1,NPCOMP

C Loop over all inter-connections
         DO 62 IPCON=1,NPCON

C Check if connection relates to current component as the receiving one
         IF(IPC1(IPCON).NE.IPCOMP) GOTO 62
         N=IPN1(IPCON)

C Compare fluid which will be send IFLD2 to fluid type of receiving
C node IFLD1 (ie. only air may be send to a air-node etc); when IPCT
C 1: always OK (self connection case)
C 2: always OK (connected to a constant temp./hum. fluid source)
C 3: node fluid types must match
C 4: OK if receiving node is air (connected to building zone/ambient)
C IFLD1/2 will be 0 for water, 1 for air, 9 for solid nodes
         IFLD1=MOD(ISV(IPCOMP,N),10)
         IF(IPCT(IPCON).EQ.1.OR.IPCT(IPCON).EQ.2) THEN
            IFLD2=IFLD1
         ELSE IF(IPCT(IPCON).EQ.3) THEN
            IFLD2=MOD(ISV(IPC2(IPCON),IPN2(IPCON)),10)
         ELSE IF(IPCT(IPCON).EQ.4) THEN
            IFLD2=1
         ELSE
            STOP ' MZPMXT: unresolvable error on plant connection type'
         END IF
         IF(IFLD1.NE.IFLD2) THEN
            call elinc(2)
            WRITE(IUOUT,*) ' Nodes with different fluid types',
     &                     ' connected by connection : ',IPCON
            STOP ' MZPMXT: unresolvable error'
         END IF
   62    CONTINUE
   64    CONTINUE
      END IF

C Get component connection coefficient positions IPOFS1 & IPOFS2,
C first reset KNHIT
      DO 72 IPNODE=1,MPNODE
      KNHIT(IPNODE)=0
   72 CONTINUE
      IF(NPCON.GT.0) THEN
         DO 75 IPCON=1,NPCON
         IP1=IPC1(IPCON)

C If IP1 is TRNSYS type, skip the remaining of this loop
C This is because trnsys components do not have cross-coupling coeff.
         IF(NPREF(IP1) .EQ. 85) CYCLE

         NXT=LOCCON(IP1)
         LOCCON(IP1)=LOCCON(IP1)+1
         IPOFS1(NXT)=NPCDAT(IP1,9)+IPN1(IPCON)-1

C In case of connection type 3, IPOFS2 holds the column index
         IF(IPCT(IPCON).EQ.3) THEN
            IP2=IPC2(IPCON)
            ICOL=NPCDAT(IP2,9)+IPN2(IPCON)-1
            IPOFS2(NXT,1)=ICOL
            IPOFS2(NXT,2)=ICOL
            IPOFS2(NXT,3)=ICOL
            IPOFS2(NXT,4)=ICOL

C If mfs active, negate ipofs2 so that coefficient
C will be moved to the matrix right-hand-side by MZPMSU.
            if(iflwn.ne.0) then
               INN=NPCDAT(IP1,9)+IPN1(IPCON)-1
               knh=KNHIT(INN)+1
               ipofs2(nxt,2)=-knh
            endif

C For "boundary connection" types 1,2 and 4 IPOFS2 is negated so that
C the coefficient will be moved to the matrix right-hand side by MZPMSU
C Note that connection types 1,2 and 4 will be processed as a type 3
C connection in case of ISTATS=2 (1st phase mass) and mfs not active
         ELSE
            INN=NPCDAT(IP1,9)+IPN1(IPCON)-1
            KNHIT(INN)=KNHIT(INN)+1
            KNH=KNHIT(INN)
            IPOFS2(NXT,1)=-KNH
            IPOFS2(NXT,3)=-KNH
            IPOFS2(NXT,4)=-KNH
            IF(IFLWN.EQ.0) THEN
               IP2=IPC2(IPCON)
               ICOL=NPCDAT(IP2,9)+IPN2(IPCON)-1
               IPOFS2(NXT,2)=ICOL
            ELSE
               IPOFS2(NXT,2)=-KNH
            END IF
         END IF
   75    CONTINUE
      END IF

C Calculate array sizes: NPMCOE is total number of matrix
C coefficient entries.
C NPNOD is the number of nodes and therefore the number of equations
C in the matrix
      NPMCOE=NPCDAT(NPCOMP,5)+NPCDAT(NPCOMP,6)+NPCDAT(NPCOMP,7)-1
      NPNOD=NPCDAT(NPCOMP,9)+NPCDAT(NPCOMP,8)-1

C Set up pointer arrays ICONDX and ICONTP. First zeroize IC and ICONDX
      DO 80 IPNODE=1,MPNODE
      IC(IPNODE)=0
   80 CONTINUE
      DO 82 IPCOM=1,MPCOM
      DO 82 INODEC=1,MNODEC
      DO 82 IPCONC=1,MPCONC
      ICONDX(IPCOM,INODEC,IPCONC)=0
   82 CONTINUE

C IN defines the row location of the receiving node, that is the unique
C node number. And so IC(IN) gives a counter for node connections in
C case there are more than one
      IF(NPCON.GT.0) THEN
         DO 84 IPCON=1,NPCON
         I1=IPC1(IPCON)
         I2=IPN1(IPCON)
         I4=IPC2(IPCON)
         I5=IPN2(IPCON)
         IN=NPCDAT(I1,9)+I2-1
         IC(IN)=IC(IN)+1
         ICONDX(I1,I2,IC(IN))=IPCON

         IF(ISV(I1,I2).EQ.0) GOTO 990

C Type 1 connection: to identical (ie temp. and humidity ratio) node
         IF(IPCT(IPCON).EQ.1) THEN
            ICONTP(IPCON)=ISV(I1,I2)

C Type 2 connection: to known fluid temperature and humidity ratio
         ELSE IF(IPCT(IPCON).EQ.2) THEN
            ICONTP(IPCON)=ISV(I1,I2)

C Type 3 connection: to another component node
         ELSE IF(IPCT(IPCON).EQ.3) THEN
            ICONTP(IPCON)=ISV(I4,I5)

            IF(ISV(I4,I5).EQ.0) GOTO 990

C Type 4 connection: to building zone air (or ambient air when zone=0)
         ELSE IF(IPCT(IPCON).EQ.4) THEN
            ICONTP(IPCON)=21

C New connector types go here!
         ELSE
            GOTO 990
         END IF

C In case fluid flow network solver active, check
C connection mapping regarding type of fluid (ie. water to water etc).
C IMFCTP is fluid through corresponding mfs connection (1=air, 2=water)
         IF(IFLWN.EQ.1) THEN
            IMFCTP=int(SUPCMP(ITPCON(ICFFS(IPCON)),1))
            IF((IMFCTP.EQ.1.AND.
     &          ICONTP(IPCON).NE.11.AND.ICONTP(IPCON).NE.21).OR.
     &         (IMFCTP.EQ.2.AND.
     &          ICONTP(IPCON).NE.10.AND.ICONTP(IPCON).NE.20)) GOTO 992
         END IF
   84    CONTINUE
      END IF

C Adjust PCONDR-sum to enable check that sending node (IPC2, IPN2) does
C not have mass diversion ratios, PCONDR, which do not sum to unity.
C If it does, this means that some mass is being lost from this node.
C Note that this is not a fatal error. For example, there may be air
C exhaust to outside. First reset each node's PCONDR-sum ZDR
      DO 90 IPNOD=1,NPNOD
      ZDR(IPNOD)=0.0
   90 CONTINUE

C Find pointers to diagonal terms.
C Store in array iptrd.
      irow=1
      do 120 icn=1,npmcoe
         if(ipofs2(icn,1).eq.ipofs1(icn)) then
            iptrd(irow)=icn
            irow=irow+1
         endif
  120 continue

C For each balance state, find required number
C of new non-zero coefficients. Store pointers
C to their location at end of original IPOFS1
C and IPOFS2. Finally update arrays NROWC and
C IPTROD. This information is required for
C the compact matrix solver routine 'cmaslv'.
      nmax=npmcoe
      do 717 istat=1,npmtyp

C Call subroutine to Biuld up arrays NROWC, IPTROD for
C number of coefficents in a row and pointer to their location
C respectively.
         call cmabld(npmcoe,npnod,istat)

C Simulate Guassian elimination without modifying
C coefficients to determine number of new non-zero
C elements to be generated during forward reduction process.
C Note that this happens because we are solving a compact
C store matrix.
C The row and column position of the new non-zero elements
C will be stored at end of arrays 'ipofs1' and 'ipofs2'.
         ntcoe=npmcoe
         jadd=ntcoe

C Start with row k.
         do 105 k=1,npnod-1
            kk=k+1
            kcol=nrowc(k,2,istat)-1

C Simulate procedure to zeroise coefficient under column k.
            do 107 i=kk, npnod
               kkcol=nrowc(i,2,istat)-1

C then for a row which has a coefficient under column
C k, find new non-zero coefficients when its coefficients
C are added to row k.
               do 106 jj=1,nrowc(i,1,istat)
                  jndx=iptrod(jj+kkcol,istat)

C Consider this row only if it has a coefficient
C under column k.
                  if(ipofs2(jndx,istat).eq.k) then
                     do 109 ii=1,nrowc(k,1,istat)
                        ndx=iptrod(ii+kcol,istat)
                        if(ipofs2(ndx,istat).gt.k) then

C This loops is used to determine if there is a new
C non-zero coefficient to be added to row i.
                           do 111 j=1,nrowc(i,1,istat)
                              ndx1=iptrod(kkcol+j,istat)
                              if(ipofs2(ndx,istat).eq.
     &                        ipofs2(ndx1,istat))goto 109
  111                      continue

C A new non-zero coefficient located. So update total number
C of coefficients counter and store row and column location
C of this coefficient.
                           ntcoe=ntcoe+1
                           ipofs1(ntcoe)=i
                           ipofs2(ntcoe,istat)=ipofs2(ndx,istat)
                        endif
  109                continue
                  endif
  106          continue
  107       continue

C Reassign new values for arrays NROWC and IPTROD
C for new non-zero coefficients.
            if(ntcoe.ne.jadd) then
               call cmabld(ntcoe,npnod,istat)
               jadd=ntcoe
            endif
  105    continue
         if(ntcoe.gt.nmax) nmax=ntcoe
  717 continue
      ntcoe=nmax

C Note that PCONDR is set to 1 in case fluid mass flow solver active
      IF(NPCON.GT.0.AND.IFLWN.EQ.0) THEN
         DO 92 IPCON=1,NPCON
         IF(IPCT(IPCON).NE.3) GOTO 92
         K=NPCDAT(IPC2(IPCON),9)+IPN2(IPCON)-1
         ZDR(K)=ZDR(K)+PCONDR(IPCON)
   92    CONTINUE

         DO 94 IPNOD=1,NPNOD
         IPCMP=IDENC(IPNOD)
         ICNOD=IDENN(IPNOD)
         IF(NDCON(IPCMP,ICNOD).EQ.0) GOTO 94
         IF(ZDR(IPNOD).LT.0.OR.ZDR(IPNOD).GT.1.) THEN
            call elinc(3)
            write(outs,904) ipcmp,icnod
            call edisp(iuout,outs)
  904       format(' MZPMXT error: component ',i5,' node ',i5)
            write(outs,905) zdr(ipnod)
            call edisp(iuout,outs)
  905       format(' exit mass diversion ratio erroneous : ',f10.5)
            GOTO 999
         ELSE IF(ZDR(IPNOD).GT.0..AND.ZDR(IPNOD).LT.1.) THEN
            call elinc(3)
            write(outs,906) ipcmp,icnod
            call edisp(iuout,outs)
  906       format(' MZPMXT warning: component ',i5,' node ',i5)
            write(outs,907) zdr(ipnod)
            call edisp(iuout,outs)
  907       format(' exit mass diversion ratios do not summate',
     &                     ' to unity, but ',f10.5)
         ELSE IF(ZDR(IPNOD).LT.SMALL.AND.ICNOD.NE.1) THEN
            call elinc(3)
            write(outs,908) ipcmp,icnod
            call edisp(iuout,outs)
  908       format(' MZPMXT warning: component ',i5,' node ',i5)
            call edisp(iuout,' is a connector with no exit connection')
         END IF
   94    CONTINUE
      END IF

C Trace output
      IF(ITC.GT.0.AND.ITRACE(34).NE.0) THEN
         WRITE(ITU,*) ' Subroutine MZPMXT     Trace output ',ICNT
         ICNT=ICNT+1
         WRITE(ITU,*) ' NPCDAT - data for plant components'
         WRITE(ITU,*) '  config  pcl    pcl    comp  P arr.   no.  ',
     &                '  no.    no.  A arr'
         WRITE(ITU,*) '   code   IREC   code   type  offset  coeffs',
     &                ' conns  nodes offset'
         DO 110 IPCOMP=1,NPCOMP
         WRITE(ITU,10110) (NPCDAT(IPCOMP,J),J=1,9)
  110    CONTINUE
10110    FORMAT(9I7)
         IF(ITU.EQ.IUOUT) call epagew
         WRITE(ITU,*) ' Manufacturers data read from plant component',
     &                ' library'
         DO 112 IPCOMP=1,NPCOMP
         JJ=NMISCD(IPCOMP)
         WRITE(ITU,*) ' Comp. ',IPCOMP,'  No. of data items: ',JJ
         WRITE(ITU,*) (ADATA(IPCOMP,J),J=1,JJ)
         IF((IPCOMP/5)*5.EQ.IPCOMP.AND.ITU.EQ.IUOUT) call epagew
  112    CONTINUE
         IF(ITU.EQ.IUOUT) call epagew

C 'Draw' the energy matrix entry structure
         WRITE(ITU,*) ' Schematic of 1st phase mass balance matrix'
         DO 115 IPNODE=1,MPNODE
         DO 114 JPNODE=1,MPNODE
         A(IPNODE,JPNODE)='_____'
  114    CONTINUE
  115    CONTINUE
         DO 116 IPNODE=1,NPMCOE
            J=IPOFS1(IPNODE)
            K=IPOFS2(IPNODE,2)
            if(j.gt.0.and.k.gt.0) A(J,K)='XXXXX'
  116    CONTINUE
         DO 118 IPNOD=1,NPNOD
         WRITE(ITU,10118) (A(IPNOD,JPNOD),JPNOD=1,NPNOD)
  118    CONTINUE
10118    FORMAT(' ',70A1)
         IF(ITU.EQ.IUOUT) call epagew
      END IF
C
C
      RETURN
C
C Error handling
  990 call elinc(1)
      WRITE(IUOUT,*) ' MZPMXT: connection ',IPCON,' is erroneous '
      GOTO 999
  992 call elinc(1)
      WRITE(IUOUT,*) ' MZPMXT: mapping of connection ',IPCON,
     &               ' to fluid flow network is erroneous '
      GOTO 999
  994 call elinc(1)
      WRITE(IUOUT,*) ' MZPMXT: error reading plant library for',
     &               ' component ',IPCOMP
      GOTO 999
C
  999 STOP ' MZPMXT: unresolvable error'
C
      END
C
C ******************** MZPMRX ********************
C
C MZPMRX controls setting up, solution and results assignment of the
C plant energy (ISTATS=1), first phase mass (ISTATS=2), and second
C phase mass (ISTATS=3) matrix equation at each time-step
C ITERA signals to the calling module that iteration of the whole
C building & plant configuration solution process is necessary
C ITERP is an iteration counter inside the plant solution process
C
      SUBROUTINE MZPMRX(ITERA)
#include "plant.h"
#include "building.h"
#include "CETC_definitions.h"

      COMMON/SHOUT/ICOUT
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

C Tolerance for iteration of hydrogen flow matrix
      common/PITER_H2/fH2_iteration_tolerance
      real fH2_iteration_tolerance

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Additional data
      common/pcdat/ pcdatf(mpcom,mpcdat), pcdatp(mpcom,mpcdat)
      real pcdatf                ! component additional data (future)
      real pcdatp                ! component additional data (present)

      common/pcres/
     &     qdata(mpcom),
     &     pcaout(mpcom,mpcres),
     &     napdat(mpcom)
      real qdata     ! not used in current context
      real pcaout    ! not used in current context
      integer napdat ! # of plant additional outputs
      common/c9/ npcomp,nci(mpcom), cdata(mpcom,mmiscd)
      integer npcomp, nci
      real cdata
C HOT3000:begin
C     place the plant matrix pointer (istats) in common to permit controllers to
C     determine if they should take action depending on the matrix being solved.
      COMMON/PltMtrx/ISTATS


c Logical variable that checks to see if the plt has converged.
      common/pltcon/bPltCon,LastTSTP
      LOGICAL bPltCon
      REAL LastTSTP

C Common for additional iteration criteria. These data structures permit bps
C to iterate until the data in the pcdatf array has converged.
      common / pAdd_iter_criteria /
     &   iPlt_Output_Iter_Flag,
     &   fPlt_Output_Init_Val,
     &   fPlt_Output_Tol
      integer iPlt_Output_Iter_Flag (mpcom,mpcdat) ! flag denoting iteration is req'd
                                                   ! for plt additional outputs
      real fPlt_Output_Init_Val ( mpcom,mpcdat)    ! initial value of plt additional output
      real fPlt_Output_Tol (mpcom,mpcdat)          ! tolerance for convergence

      real fInitial, fFuture                       ! initial & future values for iteration
                                                   ! criteria

      real fTolerance                              ! Tolerance for convergence

C Logical flag indicating numbers are close
      logical bNums_Are_Close

C Logical flag indicating if iteration is required
      logical bIteration_Required

C HOT30000:end
C Common for plant side pid controllers
      common/pstat/istatp

C
      PARAMETER (SMALL=1.0E-20)
      character outs*124
C
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' Entering subroutine MZPMRX'
         CALL DAYCLK(IDYP,PTIMEF,ITU)
      END IF
C
C Initialize and/or adjust the plant solution process iteration count
      ITERP=0

C set flag for plant iteration - initialize to true
      bPltCon = .true.
   10 ITERP=ITERP+1
C
C Loop over all three ISTATS balances
C Note that first phase mass balance will be handled first
      DO 20 IMRX=1,MPVAR
      ISTATS=IMRX+1
      IF(ISTATS.GT.MPVAR) ISTATS=ISTATS-MPVAR
C First check if it is necessary to solve this balance
      IF(ISTATS.GT.NPMTYP) GOTO 20

C Reset mass diversion ratios to 1 for istats=1 or 2
C if mfs is active.
      if((istats.eq.1.or.istats.eq.2).and.iflwn.eq.1) then
         do 272 icon=1,npcon
            pcondr(icon)=1.0
  272    continue

C For istats=3 and if mfs active, calculate mass
C diversion ratios for connection type 3 only.
      elseif(istats.eq.3.and.iflwn.eq.1) then
         do 373 icon=1,npcon
            ict=ipct(icon)
            inod=npcdat(ipc2(icon),9)+ipn2(icon)-1
            if(ict.eq.3) then
               if(csvf(inod,2).ne.0.0) then
                  pcondr(icon)=convar(icon,2)/csvf(inod,2)
               else
                  pcondr(icon)=0.0
               endif
            endif
            if(pcondr(icon).gt.1.0) pcondr(icon)=1.0
  373    continue
      endif

C Set up connections data based on most recent temperatures and flows
      CALL MZPADJ(ISTATS)
C If appropriate, invoke mass flow solver. In that case control of
C fluid mass flow is also handled by the fluid flow solver
      IF(ISTATS.EQ.2.AND.IFLWN.EQ.1) THEN
         CALL MFLWCA(2,PTIMEF)

C else CONTRL determines control status based on most recent results
C (by invoking any defined pcl?? control laws to establish CDATA for
C current time-step).
      ELSE
         if(iterp.le.itrclp) CALL CONTRL(iterp)
      END IF
C Note that in both cases control status is determined prior to
C generating the balance equations. This means that control data is
C one time-step behind matrix solution and so acts only to formulate
C the current time-step matrix equations. An alternative procedure
C is to include the control equations within the matrices to directly
C influence the simultaneous solution. This is very problematic in
C the case of non-linear control
C
C Establish and solve plant matrix, and assign results to CSVF array
      CALL MZPMSU(ISTATS)
      CALL MZPMSV(ISTATS)
   20 CONTINUE

C set flag for iteration - set as active
      bpltcon = .false.


C Exact plant solutions may require iteration. Set iteration flag to false
      bIteration_Required = .false.

C Check for each node & each state variable if iteration marker set
      DO 32 IPNOD=1,NPNOD
        DO 30 ISTATS=1,MPVAR
C Check if iteration is requested for this node
          IF(ICSV(IPNOD,ISTATS).NE.0) THEN
C If so, reset marker
            ICSV(IPNOD,ISTATS)=0
C establish initial CIN and future value CSV for state variable
            CIN=CSVI(IPNOD,ISTATS)
            CSV=CSVF(IPNOD,ISTATS)
C Now see if difference less then maximum allowable relative error
            IF (ABS((CSV-CIN)/AMAX1(SMALL,CIN)).LE.PERREL) THEN
               ! Assume solution is converged.
            ELSE
C Then check absolute error temperature or mass flow rate, respectively
              IF((ISTATS.EQ.1.AND.ABS(CSV-CIN).GT.PERTMP).OR.
     &           (ISTATS.EQ.2.AND.ABS(CSV-CIN).GT.PERMFL).OR.
     &           (ISTATS.EQ.3.AND.ABS(CSV-CIN).GT.PERMFL).OR.
     &           (ISTATS.EQ.4.AND.ABS(CSV-CIN).GT.
     &                                 fH2_iteration_tolerance)) THEN
C State variable is outside absolute bound! Set flag for iteration!
                bIteration_Required = .true.

C Trace output
                IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &            ITRACE(38).NE.0)
     &            WRITE(ITU,*) ' MZPMRX: iteration caused by ',
     &                         ' node ',IPNOD,' ISTATS ',ISTATS,
     &                         '   CIN = ',CIN,'   CSV = ',CSV

              ENDIF ! <- matches IF ((ISTATS.EQ. ...
            ENDIF   ! <- matches IF (ABS((CSV-CIN)...
          ENDIF     ! <- matches IF(ICSV(IPNOD...

C This statement is a substitute for the F90 'exit' command. Should save
C some resources on large plant networks.
          IF ( bIteration_Required ) GOTO 33

   30   CONTINUE
   32 CONTINUE

C Now check if convergence has been stipulated for plant component
C additional data arrays (PCdatf), and set call for iteration
C if outside tolerance
   33 IF ( .not. bIteration_Required ) THEN
C Loop through components

        DO ii = 1, NPCOMP
C Loop through additional data items (there is no variable that
C holds the number of additional data items specified for each plant
C component.  The addition of such a variable could reduce excessive
C looping in the following DO block, although a limited test on
C this showed that there is little to be gained in run time).
            DO jj=1,MPCDAT
C Has iteration for this plant additional output been requested?
              IF ( iPlt_Output_Iter_Flag(ii,jj) .ne. 1 ) THEN
                ! this data not flagged for iteration

              ELSE
C Collect initial & future values, and convergence tolerance
                fInitial   =  fPlt_Output_Init_Val(ii,jj)
                fFuture    =  PCDatF(ii,jj)
                fTolerance =  fPlt_Output_Tol(ii,jj)
C Check if difference is within tolerance. Note - take abs() of tolerance
C incase absent-minded developer sends negative number in future
                IF ( ABS ( fInitial - fFuture )
     &                    .GT. ABS( fTolerance ) ) THEN
C Value is outside tolerance! Set flag for iteration
                bIteration_Required = .true.
C Traceout
                IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &              ITRACE(38).NE.0)
     &              WRITE(ITU,*) ' MZPMRX: iteration caused by ',
     &                           ' component ',ii,' Data #',jj,
     &                           '   inital = ',fInitial,
     &                           '   future = ',fFuture

                ENDIF

              ENDIF   ! <- matches  IF ( iPlt_Output_Iter_Flag(ii,jj)...

C This statement is a substitute for the F90 'exit' command. Should save
C some resources on large plant networks.
              IF ( bIteration_Required ) GOTO 34

            ENDDO     ! <- matches DO jj = 1, ...
        ENDDO         ! <- matches DO ii = 1,
      ENDIF           ! <- matches IF ( .not. bIterationRequired ...

C     Is iteration required?
   34 IF ( bIteration_Required ) THEN

C Use successive substitution method with relaxation factor = 0.5
C to establish "new" initial value;
C refrain from iteration if maximum number of iterations exceeded.
        IF(ITERP.LT.MAXITP) THEN
          CSVF(IPNOD,ISTATS)=(CIN+CSV)/2.
          GOTO 10
        ELSE
          CALL DAYCLK(IDYP,PTIMEF,ICOUT)
          write(outs,1111) maxitp
 1111     format(' MZPMRX: warning maximum number of plant',
     &                  ' iterations (',i5,') exceeded')
          call edisp(iuout,outs)
          GOTO 40
        END IF
      ENDIF

C HOT3000:begin
C Save state variables in h3kreports library for later use
 40   CALL h3k_transport_plant_data()
C HOT3000:end
C Function call to MZNASS replaced with more flexible
C routine that provides time-row winding and rewinding
C operations, in support of inter-domain iteration.
C   - use flag 'iWinding' to indicate that solution s
C     should step forward in time.
      call Plt_TimeRow_Manipulate(iWinding)
C
C Exact building & plant solutions may require iteration, which might
C be signaled by ITERA; this feature is disabled at present
      ITERA=0
C
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(38).NE.0)
     &   WRITE(ITU,*) ' MZPMRX: no. of iterations = ',ITERP
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0)
     &   WRITE(ITU,*) ' Leaving subroutine MZPMRX'
C
C
      RETURN
      END
C
C ******************** MZPMSU ********************
C
C MZPMSU sets up the plant matrix equations. ISTATS dictates whether
C generated equation coefficients are for energy balance (1), or
C first phase mass balance (2), or second phase mass balance (3)
C
      SUBROUTINE MZPMSU(ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/C15PS/PC(MCOEFG),ZMAT(MPNODE)
      COMMON/C16PS/IDENC(MPNODE),IDENN(MPNODE)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
c Logical variable that checks to see if the plt has converged.
      common/pltcon/bPltCon,LastTSTP
      LOGICAL bPltCon
C Logical array indicating if plant components have support for 4th,
C hydrogen matrix
      common  / H2_lib / h2flowsupport
      logical h2flowsupport(MPCOM)
C Storage for plant time (last time-row)
      REAL LastTSTP
      REAL OUT(MPCOE)
C
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine MZPMSU'
      END IF

c This subroutine keeps track of plantsteps per month.
      IF(ISTATS.eq.1) CALL TrackPlantTimeSteps()

C
C For each defined plant component, call the appropriate coefficient
C generator (CMP??C) to generate the balance equations as required.
C Finally locate the generated coefficients in the network matrix which
C is held as two vectors PC (future time-row state variable coefficients
C matrix) and ZMAT (present time-row coefficients)
C NB NCOE: total number of component equation coefficients
C    NXTC: PC index for component IPCOMP
C    NXTZ: ZMAT index for component IPCOMP
C
      DO 19 IPCOMP=1,NPCOMP
C
      NCOE=NPCDAT(IPCOMP,6)+NPCDAT(IPCOMP,7)
      NXTC=NPCDAT(IPCOMP,5)
      NXTZ=NPCDAT(IPCOMP,9)

C HOT3000:
C IF ISTATS = 4 (hydrogen flow matrix), check if component
C supports hydrogen flow.
C  - If support is present, call coefficient generator.
C  - Otherwise, call 'Zero_h2_flow' routine to produce
C    'AX=0' identity matrix. Thus will effectively zero
C    hydrogen flow through the comoonent.
C

      IF ( ISTATS .EQ. 4 .AND.
     &    .NOT. h2flowsupport(IPCOMP)
     &   ) THEN
C Call function 'Zero_h2_flow'
         call Zero_h2_flow(IPCOMP,OUT)
      ELSE
C Call appropriate coefficient generator:

C Determine appropriate equation generator for current component.
C Remember NPCDAT(IPCOMP,4) is obtained from the component's
C database code (NPCDAT(IPCOMP,3)) divided by 10
         ITYPE=NPCDAT(IPCOMP,4)

C 1 node (ISV=21) AC  converging 2-leg junction
         IF(ITYPE.EQ.1) THEN
            CALL CMP01C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  humidifier
         ELSE IF(ITYPE.EQ.2) THEN
            CALL CMP02C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  fan
         ELSE IF(ITYPE.EQ.3) THEN
            CALL CMP03C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  cooling coil & flux control
         ELSE IF(ITYPE.EQ.4) THEN
            CALL CMP04C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  heating coil & flux control
         ELSE IF(ITYPE.EQ.5) THEN
            CALL CMP05C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  duct & condensation ?
         ELSE IF(ITYPE.EQ.6) THEN
            CALL CMP06C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  damper
         ELSE IF(ITYPE.EQ.7) THEN
            CALL CMP07C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  cooling coil & water mfr control
C uses TRNSYS type32 model.
         ELSE IF(ITYPE.EQ.9) THEN
            CALL CMP09C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  cooling coil & water mfr control
         ELSE IF(ITYPE.EQ.10) THEN
            CALL CMP10C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  heating coil & water mfr control
         ELSE IF(ITYPE.EQ.11) THEN
            CALL CMP11C(IPCOMP,OUT,ISTATS)
C 2 node (ISV=21) AC  plate heat exchanger
         ELSE IF(ITYPE.EQ.12) THEN
            CALL CMP12C(IPCOMP,OUT,ISTATS)
C 2 node (ISV=21) Cooling tower model
         ELSE IF(ITYPE.EQ.14) THEN
            CALL CMP14C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) WCH boiler with flux control
         ELSE IF(ITYPE.EQ.20) THEN
            CALL CMP20C(IPCOMP,OUT,ISTATS)
C 2 node (ISV=20) WCH radiator
         ELSE IF(ITYPE.EQ.21) THEN
            CALL CMP21C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) WCH pipe
         ELSE IF(ITYPE.EQ.22) THEN
            CALL CMP22C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) WCH converging 2-leg junction
         ELSE IF(ITYPE.EQ.23) THEN
            CALL CMP23C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) WCH pump
         ELSE IF(ITYPE.EQ.24) THEN
            CALL CMP24C(IPCOMP,OUT,ISTATS)
C 2 node (ISV=20) WCH boiler with on/off control
         ELSE IF(ITYPE.EQ.25) THEN
            CALL CMP25C(IPCOMP,OUT,ISTATS)
C 2 node (ISV=20) WCH boiler with aquastat control
         ELSE IF(ITYPE.EQ.26) THEN
            CALL CMP26C(IPCOMP,OUT,ISTATS)
C 8 node (ISV=20) WCH radiator
         ELSE IF(ITYPE.EQ.27) THEN
            CALL CMP27C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=9) Oil-filled electric panel radiator
         ELSE IF(ITYPE.EQ.28) THEN
            CALL CMP28C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) WCH flow control valve
         ELSE IF(ITYPE.EQ.29) THEN
            CALL CMP29C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) Water cooler
         ELSE IF(ITYPE.EQ.30) THEN
            CALL CMP30C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) WCH calorifier
         ELSE IF(ITYPE.EQ.31) THEN
            CALL CMP31C(IPCOMP,OUT,ISTATS)
C 2 node (ISV=20) WCH Generic heat exchanger
         ELSE IF(ITYPE.EQ.32) THEN
            CALL CMP32C(IPCOMP,OUT,ISTATS)
C 2 node (ISV>20) WCH Generic heat exchanger
         ELSE IF(ITYPE.EQ.33) THEN
            CALL CMP33C(IPCOMP,OUT,ISTATS)
C 3 node (ISV>20) Heated water storage
         ELSE IF(ITYPE.EQ.34) THEN
            CALL CMP34C(IPCOMP,OUT,ISTATS)
C 3 node (ISV>19) AC  cooling coil fed by WCH system
         ELSE IF(ITYPE.EQ.40) THEN
            CALL CMP40C(IPCOMP,OUT,ISTATS)
C 3 node (ISV>19) AC  heating coil fed by WCH system
         ELSE IF(ITYPE.EQ.41) THEN
            CALL CMP41C(IPCOMP,OUT,ISTATS)
C 2 node (ISV>20) AC  cooling coil fed by WCH system
         ELSE IF(ITYPE.EQ.43) THEN
            CALL CMP43C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=29) WCH thermostatic radiator valve
         ELSE IF(ITYPE.EQ.50) THEN
            CALL CMP50C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=29)     mechanical room thermostat
         ELSE IF(ITYPE.EQ.51) THEN
            CALL CMP51C(IPCOMP,OUT,ISTATS)
C 4 node (ISV>19) AC  heat transfer tube with transport delay
         ELSE IF(ITYPE.EQ.53) THEN
            CALL CMP53C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20)     water circuit flow multiplier
         ELSE IF(ITYPE.EQ.54) THEN
            CALL CMP54C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  air flow multiplier
         ELSE IF(ITYPE.EQ.55) THEN
            CALL CMP55C(IPCOMP,OUT,ISTATS)
C 4 node (ISV>19) AC  fan
         ELSE IF(ITYPE.EQ.56) THEN
            CALL CMP56C(IPCOMP,OUT,ISTATS)
C 4 node (ISV>19) WCH  water pipe
         ELSE IF(ITYPE.EQ.57) THEN
            CALL CMP57C(IPCOMP,OUT,ISTATS)
C 4 node (ISV>19) AC  air duct
         ELSE IF(ITYPE.EQ.58) THEN
            CALL CMP58C(IPCOMP,OUT,ISTATS)
C 5 node (ISV>19) AC air electric heater
         ELSE IF(ITYPE.EQ.60) THEN
            CALL CMP60C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) SH  water/steam flow converger
         ELSE IF(ITYPE.EQ.64) THEN
            CALL CMP64C(IPCOMP,OUT,ISTATS)
C 5 node (ISV>19) AC  heat transfer tube built from primitive parts
         ELSE IF(ITYPE.EQ.65) THEN
            CALL CMP65C(IPCOMP,OUT,ISTATS)
C 3 node (ISV>19) AC  heat transfer tube built from primitive parts
         ELSE IF(ITYPE.EQ.66) THEN
            CALL CMP66C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) AC  air flow converger
         ELSE IF(ITYPE.EQ.68) THEN
            CALL CMP68C(IPCOMP,OUT,ISTATS)
C 3 node (ISV>19) AC  heat transfer tube.
         ELSE IF(ITYPE.EQ.67) THEN
            CALL CMP67C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) flat plate collector (type1).
         ELSE IF(ITYPE.EQ.70) THEN
            CALL CMP70C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) flat plate collector segment.
         ELSE IF(ITYPE.EQ.71) THEN
            CALL CMP71C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) waterstorage tank layer.
         ELSE IF(ITYPE.EQ.72) THEN
            CALL CMP72C(IPCOMP,OUT,ISTATS)
C 2 node (ISV>19)     air & water temperature source
         ELSE IF(ITYPE.EQ.90) THEN
            CALL CMP90C(IPCOMP,OUT,ISTATS)
C 2 node (ISV>19) imaginary building-like plant load.
         ELSE IF(ITYPE.EQ.91) THEN
            CALL CMP91C(IPCOMP,OUT,ISTATS)
C 1 node (ISV=21) fictitious boundary component.
         ELSE IF(ITYPE.EQ.92) THEN
            CALL CMP92C(IPCOMP,OUT,ISTATS)
C 10 nodes (ISV=21) heat exchanger component.
         ELSE IF(ITYPE.EQ.93) THEN
            CALL CMP93C(IPCOMP,OUT,ISTATS)
C 3  node (ISV=20) heat exchanger segment.
         ELSE IF(ITYPE.EQ.94) THEN
            CALL CMP94C(IPCOMP,OUT,ISTATS)
C 1  node (ISV=20) Radiant Heating/Cooling System
         ELSE IF(ITYPE.EQ.95) THEN
            CALL RadFloor_CoeffGen(IPCOMP,OUT,ISTATS)
C 1  node (ISV=20) CHP engine unit.
         ELSE IF(ITYPE.EQ.98) THEN
            CALL CMP98C(IPCOMP,OUT,ISTATS)
C 1  node (ISV=20) CHP plant network link.
         ELSE IF(ITYPE.EQ.99) THEN
            CALL CMP99C(IPCOMP,OUT,ISTATS)
C 3 node (ISV=21) Fuel cell
         ELSE IF(ITYPE.EQ.80) THEN
            CALL FC_coeff_generator(IPCOMP,OUT,ISTATS)
C 3 node (ISV=20) gas-fired water tank for coupling to fuel cell
         ELSE IF(ITYPE.EQ.81) THEN
            CALL FC_tank_fuel_coeff_gen(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) electrically heated water tank for coupling to fuel cell
         ELSE IF(ITYPE.EQ.82) THEN
            CALL FC_tank_elec_coeff_gen(IPCOMP,OUT,ISTATS)
C 3 node (ISV=20) gas-fired water tank for first phase of AIMS work.
         ELSE IF(ITYPE.EQ.83) THEN
            CALL AIMS_tank1_fuel_coeff_gen(IPCOMP,OUT,ISTATS)
C 3 node (ISV=20) first phase thermally activated cooling component.
         ELSE IF(ITYPE.EQ.84) THEN
            CALL TAC_1_coeff_gen(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) cold water storage tank for TAC.
         ELSE IF(ITYPE.EQ.85) THEN
            CALL TAC_coldtank_coeff_gen(IPCOMP,OUT,ISTATS)
C 1 node (ISV=20) PEM cogeneration fuel cell
         ELSE IF(ITYPE.EQ.86) THEN
            CALL PEM_coeff_gen(IPCOMP,OUT,ISTATS)
         ELSE IF (ITYPE .EQ. 87
     &           .OR. ITYPE .EQ. 89 ) THEN
C 1-node electrolyzer / H2->electricity converter
            CALL  H2_Elec_converter( IPCOMP, OUT, ISTATS )
         ELSE IF (ITYPE .EQ. 88 ) THEN
C 1-node hydrogen compressor / decompressor
            CALL H2_compressor_model(IPCOMP, OUT, ISTATS)
         ELSE IF ( ITYPE .EQ. 101 ) THEN
C 1-node hydrogen storage model
            CALL  H2_storage_model( IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .EQ. 102 .OR. ITYPE .EQ. 103 ) THEN
C IEA/ECBCS Annex 42 model for combustion-based CHP devices.

C Note: ITYPE = 103 describes the Annex 42 combined Stirling
C           engine and internal combustion engine CHP model,
C       ITYPE = 102 describes an earlier model developed
C           within Annex 42 for exclusive modelling of
C           Stirling engines 
            
            CALL  A42_combustion_chp_coeff_gen
     &                ( IPCOMP, ITYPE, OUT, ISTATS )            

         ELSE IF ( ITYPE .EQ. 104 ) THEN
C 3-node electrolyser model
            CALL H2_electrolyser_coeff_gen( IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .EQ. 105 ) THEN
C 1-node compressed gas cylinder model
            CALL Comp_cyl_coeff_gen (  IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .EQ. 106 ) THEN
C 2-node metal hydride storage model
            CALL H2_MH_coeff_gen (  IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .EQ. 107 ) THEN
C 2-node hydrogen PEM fuel cell model
            CALL H2_PEMFC_coeff_gen (  IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .EQ. 108) THEN
C Hydrogen burning appliance model
            CALL H2_appliance_coeff_gen ( IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .EQ. 109) THEN
C Miscellanous plant heat gain (for testing purposes)
            CALL MiscPlantHeatGain ( IPCOMP, OUT, ISTATS )
         ELSE IF ( ITYPE .eq. 110 ) THEN
C Simple plant temperature/flow source
            CALL plt_source_component ( IPCOMP, OUT, ISTATS )
         ELSE IF (ITYPE.EQ.111) THEN
C IEA/ECBCS Annex 42 SOFC-cogeneration device.
            CALL SOFC_A42_coeffgen(IPCOMP,OUT,ISTATS)
         ELSE IF (ITYPE.EQ.112) THEN
C 3-node description of a residential SOFC
            CALL FC_coeff_generator(IPCOMP,OUT,ISTATS)
         ELSE IF ( ITYPE .EQ. 113) THEN
C 3 node gas-fired water tank for coupling to fuel cell & adsorption storage
            CALL ADS_tank_fuel_coeff_gen(IPCOMP,OUT,ISTATS)
         ELSE IF ( ITYPE .EQ. 114) THEN
C 3 node electric water tank for coupling to fuel cell & adsorption storage
            CALL ADS_tank_elec_coeff_gen(IPCOMP,OUT,ISTATS)
         ELSE IF ( ITYPE .EQ. 115 ) THEN
C 3-node adsorption storage unit
            CALL ADS_stg_coeff_gen(IPCOMP, OUT, ISTATS)
         ELSE IF ( ITYPE .EQ. 116 ) THEN
C Pressurized WCH pipe
            CALL Press_WCH_pipe_coeff_gen(IPCOMP, OUT, ISTATS)
         ELSE IF ( ITYPE .EQ. 117 ) THEN
C Pressurized WCH pump
            CALL Press_WCH_pump_coeff_gen(IPCOMP, OUT, ISTATS)
         ELSE IF (ITYPE .EQ. 118) THEN
C 1 node simple solar collector model
            CALL simple_solar_coll_coeff_gen(IPCOMP,OUT,ISTATS)
         ELSE IF (ITYPE .EQ. 119) THEN
C X-node generic TRNSYS type
            CALL trnsys_coeff_gen(IPCOMP,OUT,ISTATS)

C New component equation generators inserted here

         ELSE
            call elinc(1)
            WRITE(IUOUT,*) ' No coefficient generator for component '
     &           ,IPCOMP
            STOP ' MZPMSU: unresolvable error on component type'
         END IF
      ENDIF

C
C Now locate equation coefficients in network coefficients
C matrix (actually vector PC) according to template
      DO 10 ICOE=1,NCOE
        PC(NXTC)=OUT(ICOE)
        NXTC=NXTC+1
   10 CONTINUE
C
C Now insert the present time-row coefficient (for each component node)
C in ZMAT vector
      J01=NCOE+1
      J02=NCOE+NPCDAT(IPCOMP,8)
      DO 12 JCOE=J01,J02
        ZMAT(NXTZ)=OUT(JCOE)
        NXTZ=NXTZ+1
   12 CONTINUE
C Next component
   19 CONTINUE

C If any PC entries relates to a boundary condition state variable,
C which will not appear as a system node, then it must be removed
C to the matrix equation right-hand side. We therefore re-assign
C the PC coefficient to ZMAT if the corresponding IPOFS2 vector entry
C is negative indicating this case
C
C Search by IPOFS2 list
      DO 20 IPMCOE=1,NPMCOE
      IF(IPOFS2(IPMCOE,ISTATS).EQ.0) THEN
         STOP ' MZPMSU: illegal column reference in offset 2 vector'
      END IF
      IF(IPOFS2(IPMCOE,ISTATS).GT.0) GOTO 20
C
C Negative entry so re-assignment is necessary
C First determine component and node numbers
      INN=IPOFS1(IPMCOE)
      ICM=IDENC(INN)
      ICN=IDENN(INN)
      IHIT=ABS(IPOFS2(IPMCOE,ISTATS))
C Establish IP1 pointer to CONVAR
      IP1=ICONDX(ICM,ICN,IHIT)
C
C Remember that CONVAR(IP1,ISTATS) holds temperature (ISTATS=1),
C 1st phase mass flow rate (ISTATS=2), or 2nd phase mass flow
C rate (ISTATS=3). It is already checked that the source variable
C is of the same fluid type and matrix capability mode as the
C receiving node.
C Move matrix coefficient (* current value state variable) to
C equation RHS because IPOFS2 flag is -ve
      ZMAT(INN)=ZMAT(INN)-PC(IPMCOE)*CONVAR(IP1,ISTATS)
   20 CONTINUE
C
C  Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(39).NE.0) THEN
         WRITE(ITU,*) ' Subroutine MZPMSU   Trace output ',ICNT
         ICNT=ICNT+1
         CALL DAYCLK(IDYP,PTIMEF,ITU)
C
         WRITE(ITU,*) ' Plant matrix coefficient locations and values',
     &                ' for ISTATS = ',ISTATS
         DO 110 IPMCOE=1,NPMCOE
         WRITE(ITU,*) ' Coeff ',IPMCOE,'  row ',IPOFS1(IPMCOE),
     &                                 '  col ',IPOFS2(IPMCOE,ISTATS),
     &                                 '  PC =',PC(IPMCOE)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPMCOE/30)*30
            IF(IX1.EQ.IPMCOE.OR.IPMCOE.EQ.NPMCOE) call epagew
         END IF
  110    CONTINUE
C
         WRITE(ITU,*) ' Z matrix : known values'
         DO 112 IPNOD=1,NPNOD
         WRITE(ITU,*) ' Node ',IPNOD,'  ZMAT = ',ZMAT(IPNOD)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPNOD/30)*30
            IF(IX1.EQ.IPNOD.OR.IPNOD.EQ.NPNOD) call epagew
         END IF
  112    CONTINUE
      END IF
C
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MZPMSU'
C
C
      RETURN
      END
C
C ******************** MZPMSV ********************
C
C MZPMSV solves the plant matrix equation as generated by MZPMSU
C at each time-step. A sparse matrix solver is invoked to solve the
C matrix equation {PC} {SSV} = {RHS} for the solution vector SSV
C (which contains the values of the state variables), in terms of the
C known RHS vector. PC holds the future time-row coefficients.
C MZPMSV then relocates matrix solution vector results in CSV? arrays.
C Arrau ACOEFS is used instead of PC if solver type 2 or 3 is used.
C In order to ease invocation of a different solver, the actual matrix
C solution process is set up to be carried out by a separate subroutine.
C INDX stores pivoting information in case of LU-decomposition method

      SUBROUTINE MZPMSV(ISTATS)

#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/C15PS/PC(MCOEFG),ZMAT(MPNODE)

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PMSLV/IPMSLV
      COMMON/PMSV/SSV(MPNODE)

      common/packa/nrowc(mpnode,2,mpvar),iptrd(mpnode),
     &             iptrod(mcoefg,mpvar),ntcoe

      DIMENSION ACOEFS(MPNODE,MPNODE),RHS(MPNODE)
      DIMENSION INDX(MPNODE)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine MZPMSV'
      END IF

C Create RHS vector from ZMAT vector
      DO 14 I=1,NPNOD
      RHS(I)=ZMAT(I)
   14 CONTINUE

C Create ACOEFS matrix from IPOFS1, IPOFS2 and PC vectors,
C Do this only for solvers type 2 or 3.
      if(ipmslv.eq.2.or.ipmslv.eq.3) then
         DO 10 I=1,NPNOD
            DO 10 J=1,NPNOD
            ACOEFS(I,J)=0.0
   10    CONTINUE
         DO 102 I=1,NPMCOE
            IF(IPOFS2(I,ISTATS).LT.0) GOTO 102
            J=IPOFS1(I)
            K=IPOFS2(I,ISTATS)
            ACOEFS(J,K)=PC(I)
  102    CONTINUE
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(38).NE.0) THEN
         WRITE(ITU,*) ' Subroutine MZPMSV        Trace output ',ICNT
         ICNT=ICNT+1
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         DO 101 I=1,NPNOD
           DO 101 J=1,NPNOD
              ACOEFS(I,J)=0.0
  101    CONTINUE

C Create ACOEFS matrix from IPOFS1, IPOFS2 and PC vectors,
         DO 112 I=1,NPMCOE
            IF(IPOFS2(I,ISTATS).LT.0) GOTO 112
            J=IPOFS1(I)
            K=IPOFS2(I,ISTATS)
            ACOEFS(J,K)=PC(I)
  112    CONTINUE

C Dump ACOEFS matrix
         WRITE(ITU,*) ' ACOEFS matrix (first 10 coeff./equation only)',
     &                ' for ISTATS = ',ISTATS
         DO 16 IPNOD=1,NPNOD
         WRITE(ITU,1016) (ACOEFS(IPNOD,JPNOD),JPNOD=1,MIN(100,NPNOD))
   16    CONTINUE
 1016    FORMAT(10G11.4)
C Dump RHS vector
         WRITE(ITU,*) ' RHS vector'
         WRITE(ITU,*) (RHS(IPNOD),IPNOD=1,NPNOD)
      END IF

C Now solve {SSV} from matrix equation {PC} {SSV} = {RHS}
      IF(IPMSLV.EQ.1) THEN

C Initialise new non-zero elements beyond npmcoe.
         do 777 j=npmcoe+1, ntcoe
 777     pc(j)=0.0
         call cmaslv(pc,rhs,ntcoe,npnod,istats)

C Relocate matrix solution vector results in CSVF arrays
C Loop over all nodes in the plant network
         DO 300 IPNOD=1,NPNOD
            CSVF(IPNOD,ISTATS)=rhs(IPNOD)
  300    CONTINUE
         goto 717

C Or solve {SSV} from matrix equation {ACOEFS} {SSV} = {RHS}
      ELSE IF(IPMSLV.EQ.2) THEN
         CALL LUDCMS(ACOEFS,NPNOD,MPNODE,INDX,D)
         CALL LUBKSS(ACOEFS,NPNOD,MPNODE,INDX,RHS)
         DO 20 IPNOD=1,NPNOD
         SSV(IPNOD)=RHS(IPNOD)
   20    CONTINUE
      ELSEIF(IPMSLV.EQ.3) THEN
         CALL JSOLVS(ACOEFS,NPNOD,MPNODE,SSV,RHS)
      else
         STOP ' MZPMSV: unresolvable error !'
      END IF

C Relocate matrix solution vector results in CSVF arrays
C Loop over all nodes in the plant network
      DO 30 IPNOD=1,NPNOD
      CSVF(IPNOD,ISTATS)=SSV(IPNOD)
   30 CONTINUE

C Trace output continued
 717     IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(38).NE.0) THEN

C Dump solution vector
         IF(IPMSLV.EQ.1) THEN
            WRITE(ITU,*) ' CMASLV calculated future time-row SSV'
         ELSE IF(IPMSLV.EQ.2) THEN
            WRITE(ITU,*) ' LUDCMS calculated future time-row SSV'
         ELSE IF(IPMSLV.EQ.3) THEN
            WRITE(ITU,*) ' JSOLVS calculated future time-row SSV'
         END IF
         WRITE(ITU,*) (CSVF(IPNOD,ISTATS),IPNOD=1,NPNOD)
         IF(ITU.EQ.IUOUT) call epagew
      END IF

C Trace output continued
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MZPMSV'

      RETURN
      END

C ******************** MZNASS ********************
C
C NOTE: MZNASS is no longer called.
C it has been supplanted by a more flexible implementation,
C Plt_TimeRow_manipulate, at the bottom of this file.
C
C MZNASS adjusts all plant related history variables (incl. CSVP)
C
C      SUBROUTINE MZNASS
C#include "plant.h"
C#include "building.h"
C
C      COMMON/TC/ITC,ICNT
C      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
C      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
C      COMMON/PTIME/PTIMEP,PTIMEF
C
C      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
C      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
C     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
C      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
C
C      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
C      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
C     &             PCNTMF(MPCOM),
C     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
C     &             PCNTMP(MPCOM)
C      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
C
C      common/pcres/
C     &     qdata(mpcom),        ! not used in current context
C     &     pcaout(mpcom,mpcres), ! not used in current context
C     &     napdat(mpcom)        ! # of plant additional outputs
C
C
CC COMMON DELAY required only by primitive parts components.
C      COMMON/DELAY/TEMPF(MPCOM,MDLY),YPOSF(MPCOM,MDLY),NSECTF(MPCOM),
C     &             TEMPP(MPCOM,MDLY),YPOSP(MPCOM,MDLY),NSECTP(MPCOM)
C      common/pp_delay/
C     &     bDelay_Flag          ! flag for PP varaiable transport
C      logical bDelay_Flag
C
CC Trace output
C      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
C     &   ITRACE(40).NE.0) THEN
C         CALL DAYCLK(IDYP,PTIMEF,ITU)
C         WRITE(ITU,*) ' Entering subroutine MZNASS'
C      END IF
C
CC Loop over all nodes in the plant network to:
C      DO 12 IPNOD=1,NPNOD
C
CC adjust all three state variable's present time value
C      DO 10 ISTATS=1,3
C      CSVP(IPNOD,ISTATS)=CSVF(IPNOD,ISTATS)
C   10 CONTINUE
C
CC adjust plant node related present time values for future usage in
CC component coefficient generators
C      PUAP(IPNOD)=PUAF(IPNOD)
C      PCQP(IPNOD)=PCQF(IPNOD)
CC Next node
C   12 CONTINUE
C
CC Loop over all connections in the plant network to
CC adjust all connection related present time values for future usage
C      IF(NPCON.GT.0) THEN
C         DO 20 IPCON=1,NPCON
C         PCTP(IPCON)=PCTF(IPCON)
C         PCRP(IPCON)=PCRF(IPCON)
C   20    CONTINUE
C      END IF
C
CC Loop over all components in the plant network to
CC adjust all component related present time values for future usage
C      DO 32 IPCOMP=1,NPCOMP
C      PCNTMP(IPCOMP)=PCNTMF(IPCOMP)
C
C
CC Adjust transport delay variables: sections, positions and temperatures.
CC HOT3000 suggested improvement: Only transport primitive part delay
CC variables if PP transport delay has been involked, as flagged by
CC the  TRNSDLAY routine
C      IF ( bDelay_Flag ) THEN
C         NSECTP(IPCOMP)=NSECTF(IPCOMP)
C         DO 31 IDLY=1,MDLY
C            YPOSP(IPCOMP,IDLY)=YPOSF(IPCOMP,IDLY)
C            TEMPP(IPCOMP,IDLY)=TEMPF(IPCOMP,IDLY)
C 31      CONTINUE
C      ENDIF
C
CC This includes the specific component model variables. Obviously
CC we are transfering a lot of zeros here. Improvements possible
CC     DO 30 IPCDAT=1,MPCDAT
CC HOT3000 suggested improvement: limit loop to actual number
CC of additioal data in use:
C      DO 30 IPCDAT=1, napdat(IPCOMP)
C      PCDATP(IPCOMP,IPCDAT)=PCDATF(IPCOMP,IPCDAT)
C   30 CONTINUE
C   32 CONTINUE
C
CC HOT3000 ---------------------------------------------------------------
CC Update data for radiant heating/cooling
C       CALL RadFloor_store(2)
CC End HOT3000 ---------------------------------------------------------
C
CC Trace output
C      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
C     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MZNASS'
C
C      RETURN
C      END

C ******************** JSOLVS ********************

C JSOLVS solves the matrix equation {A} {X} = {B} based
C on Gaussian elimination with backsubstitution and no pivoting.
C An efficient matrix technique is used to forward reduce the
C N*N matrix A (physical size NP*NP) halfway, to a matrix whose
C components on the diagonal and above remain nontrivial.
C The solution vector X (size N) is then generated through
C backsubstitution of the known right hand side vector B (size N)
C
      SUBROUTINE JSOLVS(A,N,NP,X,B)

#include "plant.h"

C Automatic arrays are a g77 extention providing
C pseudo-dynamic memory allocation, and permit the
C array to be automatically sized the first time
C the procedure is called. But what happens if
C another procedure is written in the future
C to call this routine before solution of the
C plant domain? If dimensions N,NP are set smaller
C than the values used in the plant matrix solution,
C bps will seg-fault.
C
C     DIMENSION A(NP,NP),X(N),B(N)
C
C Better to use static allocation instead!

      DIMENSION A(MPNODE,MPNODE),X(MPNODE),B(MPNODE)

C Initialise static variable. On entry to this routine, ising=0
C so that the plant matrix is checked for singularity before
C the backward substitution phase.
      data ising/0/

C Forward reduce matrix A.
      DO 10 I=2,N
   10 A(1,I)=A(1,I)/A(1,1)
      DO 50 K=2,N
      K1=K-1
      DO 20 IK=K,N
      DO 20 J=1,K1
      AB=A(IK,J)
      AC=A(J,K)
      AD=AB*AC
   20 A(IK,K)=A(IK,K)-AD
      J1=K+1
      DO 30 J=J1,N
      DO 40 MJ=1,K1
      AB=A(K,MJ)
      AC=A(MJ,J)
      AD=AB*AC
   40 A(K,J)=A(K,J)-AD
   30 A(K,J)=A(K,J)/A(K,K)
   50 CONTINUE

C First check that matrix is nonsingular. Stop if singular.
C The matrix determinant is evaluated by multiplying all diagonal
C terms of the reduced matrix. if det = 0.0 then matrix is singular.
C This check is done only once when 'ising=0'.
      if (ising.eq.0) then
         det=1.0
         do 707 i=1,n
  707       det=det*a(i,i)
         if(det.eq.0.0) then
            write(*,710)
  710       format(/,'JSOLVS: Unresolvable error.',/
     &               'Plant matrix is found to be singular.')
            stop
         endif
         ising=1
      endif

C Conduct backward substitution to establish solution vextor X.
      B(1)=B(1)/A(1,1)
      DO 70 I=2,N
      K1=I-1
      DO 60 J=1,K1
      AB=B(J)
      AC=A(I,J)
      AD=AB*AC
   60 B(I)=B(I)-AD
   70 B(I)=B(I)/A(I,I)
      X(N)=B(N)
      N1=N-1
      DO 90 JJ=1,N1
      I=N-JJ
      IK=I+1
      DO 80 J=IK,N
      AB=A(I,J)
      AC=B(J)
      AD=AB*AC
   80 B(I)=B(I)-AD
   90 X(I)=B(I)

      RETURN
      END

C *** LUDCMS ***********************************************************
C Given an N x N matrix A, with physical dimension NP, this routine
C replaces it by the LU decomposition of a rowwise permutation of
C itself. A and N are input. On output A is the LU decomposition of
C itself; INDX is an output vector which records the row permutation
C effected by the partial pivoting; D is output as +/- 1 depending
C on whether the number of row interchanges was even or odd, respect.
C This routine is used in combination with LUBKSS to solve linear
C equations or invert a matrix.

C NB LUDCMS is the single precision version of LUDCMP

      SUBROUTINE LUDCMS(A,N,NP,INDX,D)

      DIMENSION A(NP,NP)
      DIMENSION INDX(N)

      PARAMETER (SMALL=1.0E-20)
      PARAMETER (NMAX=100)
      DIMENSION VV(NMAX)

      IF(N.GT.NMAX) STOP ' LUDCMS: local array VV too small !'

      D=1.0

C Loop over rows to get implicit scaling information
      DO 12 I=1,N
      AAMAX=0.0
      DO 11 J=1,N
      IF(ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
   11 CONTINUE
      IF(AAMAX.EQ.0.0) STOP ' LUDCMS: Singular matrix !'
      VV(I)=1.0/AAMAX
   12 CONTINUE

C Start loop over columns of Crout's method
      DO 19 J=1,N
      DO 14 I=1,J-1
      SUM=A(I,J)
      DO 13 K=1,I-1
      SUM=SUM-A(I,K)*A(K,J)
   13 CONTINUE
      A(I,J)=SUM
   14 CONTINUE

C Start search for largest pivot element
      AAMAX=0.0
      DO 16 I=J,N
      SUM=A(I,J)
      DO 15 K=1,J-1
      SUM=SUM-A(I,K)*A(K,J)
   15 CONTINUE
      A(I,J)=SUM
      DUM=VV(I)*ABS(SUM)
      IF(DUM.GE.AAMAX) THEN
         IMAX=I
         AAMAX=DUM
      END IF
   16 CONTINUE
C Perhaps interchange rows and scale factor
      IF(J.NE.IMAX) THEN
         DO 17 K=1,N
         DUM=A(IMAX,K)
         A(IMAX,K)=A(J,K)
         A(J,K)=DUM
   17    CONTINUE
         D=-D
         VV(IMAX)=VV(J)
      END IF

      INDX(J)=IMAX
      IF(A(J,J).EQ.0.0) A(J,J)=SMALL
      IF(J.NE.N) THEN
         DUM=1.0/A(J,J)
         DO 18 I=J+1,N
         A(I,J)=A(I,J)*DUM
   18    CONTINUE
      END IF
   19 CONTINUE

      RETURN
      END

C *** LUBKSS ***********************************************************
C Solves the set of N linear equations {A}{x}={b}. Here A is input, not
C as the matrix A but rather as its LU decompositions, determined by
C the routine LUDCMS. INDX is input as the permutation vector returned
C by LUDCMS. B is input as the right-hand side vector B, and returns
C with the solution vector X. A, N, NP and INDX are not modified by
C this routine and can be left in place for successive calls with
C different right-hand sides B. This routine takes into account the
C possibility that B will begin with many zero elements, so it is
C efficient for use in matrix inversion.

C NB LUBKSS is the single precision version of LUBKSB

      SUBROUTINE LUBKSS(A,N,NP,INDX,B)

      DIMENSION A(NP,NP),B(N)
      DIMENSION INDX(N)
      small=1.0e-30

C C Start forward substitution
      II=0
      DO 12 I=1,N
      LL=INDX(I)
      SUM=B(LL)
      B(LL)=B(I)
      IF(II.NE.0) THEN
         DO 11 J=II,I-1
         SUM=SUM-A(I,J)*B(J)
   11    CONTINUE
      ELSE IF(SUM.NE.0.0) THEN
         II=I
      END IF
      B(I)=SUM
   12 CONTINUE

C Start backsubstitution
      DO 14 I=N,1,-1
      SUM=B(I)
      if(abs(sum).lt.small) sum=0.0
      IF(I.LT.N) THEN
         DO 13 J=I+1,N
         SUM=SUM-A(I,J)*B(J)
   13    CONTINUE
      END IF
      B(I)=SUM/A(I,I)
   14 CONTINUE

      RETURN
      END

C ************************* C M A B L D **************************************
C This subroutine assigns number of coefficients in each row to
C array 'nrowc(?,1,?)' and pointer to their location in 'iptrod(?,?)'.
C The column number of row coefficients is sorted in ascending order.
C In effect the subroutine builds a compact matrix by assigning
C appropriate pointers and data to integer arrays.
      subroutine cmabld(npmcoe,npnod,istat)
#include "plant.h"

      common/packa/nrowc(mpnode,2,mpvar),iptrd(mpnode),
     &             iptrod(mcoefg,mpvar),ntcoe
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

      do 105 i=1,npnod
         nrowc(i,1,istat)=0
  105 continue

C Find number of non-zero coefficients in each row
C and pointer to their location.
      indx=1
      iloc=1
      do 107 i=1,npnod
         do 111 icn=1,npmcoe
            if(ipofs1(icn).eq.i) then
               nrowc(i,1,istat)=nrowc(i,1,istat)+1
               iptrod(indx,istat)=icn
               indx=indx+1
            endif
  111    continue
         if(i.gt.1) iloc=iloc+nrowc(i-1,1,istat)
         nrowc(i,2,istat)=iloc
  107 continue

C Sort row coefficients in ascending column order.
      do 200 i=1,npnod
         icol=nrowc(i,2,istat)-1
         do 210 j=1,nrowc(i,1,istat)-1
            ndx1=icol+j
            imin=ipofs2(iptrod(ndx1,istat),istat)
            indx=ndx1
            do 215 jj=j+1,nrowc(i,1,istat)
               ndx2=icol+jj
               icl2=ipofs2(iptrod(ndx2,istat),istat)
               if(icl2.lt.imin)  then
                  imin=icl2
                  indx=ndx2
               endif
  215       continue
            if(indx.ne.ndx1) then
               itemp=iptrod(ndx1,istat)
               iptrod(ndx1,istat)=iptrod(indx,istat)
               iptrod(indx,istat)=itemp
            endif
  210    continue
  200 continue
      return
      end

C ************************* C M A S L V **************************************
C This subroutine solves  A {x}=b using Gaussian elimination with no
C pivoting. A is compact in that only its coefficients are stored.
C Access to these coefficients is via array pointers 'iptrod' and
C 'iptrd'. Note that the location of new non-zero coefficients has
C been determinrd in subroutine 'mzpmxt'.
C First A is reduced to an upper triangular state 'U' then the
C solution vector b (size npnod) is then generated through
C backsubstitution of the known right hand side vector b.
      subroutine cmaslv(a,b,npmcoe,npnod,istats)
#include "plant.h"

      dimension a(npmcoe),b(npnod)

      common/packa/nrowc(mpnode,2,mpvar),iptrd(mpnode),
     &             iptrod(mcoefg,mpvar),ntcoe
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      small=1e-30

C Forward reduce A.
C Start with row k.
      do 10 k=1,npnod-1
         kk=k+1
         kcol=nrowc(k,2,istats)-1

C then for the following rows, zeroise coefficients
C under column k.
         do 20 i=kk, npnod
            kkcol=nrowc(i,2,istats)-1

C If there is a coefficient under
C column k, check to see if it is not
C a zero. Skip to next row if zero.
            do 30 ii=1,nrowc(i,1,istats)
               ndx=iptrod(ii+kkcol,istats)
               if(ipofs2(ndx,istats).gt.k) goto 20
               if(ipofs2(ndx,istats).ne.k) goto 30
               x=a(ndx)
               if(abs(x).lt.small) goto 20

C First check for zero diagonals.
               if(abs(a(iptrd(k))).lt.small) then
                  write(*,*)'cmaslv:Forward reduction phase.'
                  write(*,*)'       Fatal error :Zero diagonal in row',i
                  stop
               endif

C If it is not, zeroise it.
               x=x/a(iptrd(k))

C Check to see if a column in row i
C is to be added to the same column of row k.
C Start with coefficient ii in row i.
               do 40 jj=ii+1,nrowc(i,1,istats)
                  ndx1=iptrod(kkcol+jj,istats)
                  if(ipofs2(ndx1,istats).lt.0) goto 40

C Then look for a coefficient in row k with the
C same column position.
                  do 50 j=1,nrowc(k,1,istats)
                     ndx2=iptrod(j+kcol,istats)

C If one exists, add them up.
                     if(ipofs2(ndx1,istats).eq.ipofs2(ndx2,istats)) then
                        a(ndx1)=a(ndx1)-a(ndx2)*x
                        goto 40
                     endif
   50             continue
   40          continue

C Modify right hand side vector of row i.
               b(i)=b(i)-b(k)*x

C column k of row i is now zero, so move to next row.
               goto 20
   30       continue
   20    continue
   10 continue

C Back substition. Note that
C b is also the solution vector.
      do 60 i=npnod,1,-1

C Check for zero diagonals which could have resulted
C from the forward reduction phase.
         if(abs(a(iptrd(i))).lt.small) then
            write(*,*)'cmaslv: Backsubstitution phase.'
            write(*,*)'        Fatal error :Zero diagonal in row',i
            stop
         endif
         x=b(i)

C the following statement ensures that no ieee arithmetic
C underflow signal occurrs.
         if(abs(x).lt.small) x=0.0
         if(i.ne.npnod) then
            kcol=nrowc(i,2,istats)-1
            do 70 k=nrowc(i,1,istats),1,-1
               ndx=iptrod(kcol+k,istats)
               icl=ipofs2(ndx,istats)
            if(icl.le.i) goto 60
   70       x=x-a(ndx)*b(icl)
         endif
   60    b(i)=x/a(iptrd(i))
      return
      end


C ************* Plt_TimeRow_Manipulate ****************************
C
C Created by: Alex Ferguson
C Created on: July 07, 2004
C
C ABSTRACT:
C
C Plt_TimeRow_Manipulate is a more versitile implementation
C of ESP-r's MZNASS procedure. In addition to transporting
C future time-row data to the present time-row storage arrays,
C Plt_TimeRow_Manipulate can be used to "re-wind"
C the plant domain solution for the current timestep *after*
C solution of the plant domain has been completed. Each time
C routine MZPMRX is called, the plant domain state-equation set is
C solved and procedure MZNASS is invoked to transfer data from the
C future time row storage arrays to the present time row storage
C arrays.
C
C This structure is problematic when inter-domain iteration is
C enabled, as when MZNUMA invokes MZPMRX for each subsequent
C iteration, the present time row state variables are over-written
C with values from the future time row, effectively stepping the
C plant domain forward in time. As a result, the temporal response
C of the hvac network is substantially quicker ( and erroneous )
C when inter-domain iteration is active.
C
C
C This subroutine peforms three basic operations:
C
C   - Stores plant state variables for the current present
C     time-row
C
C   - Overwrites the data in the present time-row storage
C     arrays with
C
C   - Restores the present time-row state varaibles to their
C     values at the start of the time step, and the future
C     time-row state variables to their most recent values
C     calcualted at the end of the last plant iteration
C     (effectively re-winding the plant to the point where
C     iteration was adjourned.)
C
C Inputs:
C
C   - iOperation: integer flag used to specify which operation
C     should be performed. Compared to named constants defined
C     below (iSaving = 1, iWinding = 2, iRewindin = 3).
C
C   + Various ESP-r plant domain common blocks defined below.
C
C-----------------------------------------------------------------

      subroutine Plt_TimeRow_Manipulate(iOperation)

      implicit none

#include "plant.h"
#include "building.h"
#include "CETC_definitions.h"

      common/c9/
     &     npcomp,              ! # of components in network
     &     nci(mpcom),          ! not used in current context
     &     cdata(mpcom,mmiscd)  ! not used in current context
      integer npcomp, nci
      real cdata

      common/c10/
     &     npcon,               ! # of connections in network
     &     ipc1(mpcon),         ! not used in current context
     &     ipn1(mpcon),         ! not used in current context
     &     ipct(mpcon),         ! not used in current context
     &     ipc2(mpcon),         ! not used in current context
     &     ipn2(mpcon),         ! not used in current context
     &     pcondr(mpcon),       ! not used in current context
     &     pconsd(mpcon,2)      ! not used in current context
      integer npcon, ipc1, ipn1, ipc2, ipn2, ipct
      real pcondr, pconsd

      common/c13ps/
     &     npmcoe,              ! not used in current context
     &     npnod,               ! # of nodes in network
     &     npmtyp               ! not used in current context
      integer npmcoe, npnod, npmtyp


      common/pcval/
     &     csvf(mpnode,mpvar),  ! plant state variable future values
     &     csvp(mpnode,mpvar)   ! plant state variable present values
      real csvp, csvf

      common/pcvar/
     &     pctf(mpcon),         ! connection future temperatures
     &     pcrf(mpcon),         ! connection future mass diversion ratio
     &     puaf(mpnode),        ! plant node UA modulus, future value
     &     pcqf(mpnode),        ! plant node heat flux, future value
     &     pcntmf(mpcom),       ! component containmment temp, future value
     &     pctp(mpcon),         ! connection present temperature
     &     pcrp(mpcon),         ! connection present mass diversion ratio
     &     puap(mpnode),        ! plant node UA modulus, presnt value
     &     pcqp(mpnode),        ! node heat flux. present value
     &     pcntmp(mpcom)        ! componet containment temp, present value
      real pctf, pcrf, puaf, pcqf, pcntmf, pctp
      real pcrp, puap, pcqp, pcntmp

      common/pcdat/
     &     pcdatf(mpcom,mpcdat), ! component additional data (present)
     &     pcdatp(mpcom,mpcdat)  ! component additional data (future)
      real pcdatf, pcdatp

C.....Primitive parts componets
      common/pp_delay/
     &     bDelay_Flag          ! flag for PP varaiable transport
      logical bDelay_Flag

      common/delay/
     &     tempf(mpcom,mdly),   ! primitive part future value ??
     &     yposf(mpcom,mdly),   ! primitive part future value ??
     &     nsectf(mpcom),       ! primitive part future value ??
     &     tempp(mpcom,mdly),   ! primitive part future value ??
     &     yposp(mpcom,mdly),   ! primitive part future value ??
     &     nsectp(mpcom)        ! primitive part future value ??
      real tempf, yposf,  tempp, yposp
      integer nsectp, nsectf

C.....Passed Variable
      integer iOperation        ! flag for operation to be performed

C.....Local Variables
      integer iNode             ! counter
      integer iMatrix           ! counter
      integer iConn             ! counter
      integer iComp             ! counter
      integer iMisc             ! counter
      integer iDelay            ! counter

      logical bSave             ! flag for saving data (shortens 'if' statements)
      logical bRewind           ! falg for restoring


C.....Storage variables
      real CSVP_store(mpnode,mpvar) ! storage for present state variable values
      real PUAP_store(mpnode)       ! storage for present nodal UA modulus
      real PCQP_store(mpnode)       ! storage for present nodal heat flux

      REAL PCTP_store(mpcon)    ! storage for present connection temp
      REAL PCRP_store(mpcon)    ! storage for present conn. diversion ratio

      REAL PCNTMP_store(mpcom)  ! storage for present component containment temp

      REAL PCDATP_store(mpcom,mpcdat)  ! storage for present component misc data.

      REAL tempp_store(mpcom,mdly) ! storage for primitive part future value ??
      REAL yposp_store(mpcom,mdly) ! storage for primitive part future value ?
      REAL nsectp_store(mpcom)  ! storage for primitive part future value ?

      SAVE CSVP_store, PUAP_store, PCQP_store, PCTP_store,PCRP_store
      SAVE PCNTMP_store, PCDATP_store, tempp_store, yposp_store
      SAVE nsectp_store


C-----Electrical load that DG unit responds to (W)
      common/TSCON7dat/plt_elec_load,plt_elec_gen,plt_elec_balance
      real plt_elec_load, plt_elec_gen, plt_elec_balance

C-----Function used to determine total load on network:
      real elec_net_load_calc



      if ( iOperation .eq. iSaving ) then
C------------------------------------------------------------------
C        Save present time-row data for plant network:
C
C        Variable stored value = Variable Present Value
C
C------------------------------------------------------------------

C--------Nodal data
         do iNode = 1, npnod    ! <- npnod's in plant network

C...........Misc node data
            PUAP_store(iNode) =  PUAP(iNode) ! UA modulus
            PCQP_store(iNode) =  PCQP(iNode) ! heat flux

C...........NODE State variables (for each matrix)
            do iMatrix = 1, mpvar ! <- mpvar = # of state variabes

               CSVP_store( iNode, iMatrix ) = CSVP( iNode, iMatrix )

            enddo

         enddo

C--------Connection data: Note - this loop will be executed once
C--------if npcon = 0 (unlikely). Transferring 2 array locations
C--------is only marginally more expensive than checking if
C--------npcon>0.
         do iConn = 1, npcon ! <- npcon = # connections in plant

            PCTP_store(iConn) = PCTP(iConn) ! temperature
            PCRP_store(iConn) = PCRP(iconn) ! diversion-ratio

         enddo

C--------Component data

         do iComp = 1, npcomp ! # of components

            PCNTMP_store(iComp) =  PCNTMP(iComp) ! containment temperature

C...........Additional data
            do iMisc = 1, MPCDAT ! component has a maximum of 
                                 ! MPCDAT additional data.

               PCDATP_store(iComp,iMisc) = PCDATP(iComp,iMisc)

            enddo

C...........Primitive-part data. Check to see if primitive part
C...........transport delay is active (set in subroutine TRNSDLAY)
            IF ( bDelay_Flag ) then
               nsectp_store(iComp) = nsectp(iComp)
               do iDelay = 1, mdly
                   tempP_store( iComp, iDelay ) = tempP( iComp, iDelay )
                   yposP_store( iComp, iDelay ) = yposP( iComp, iDelay )
                enddo
             endif

         enddo

      elseif(iOperation .eq. iWinding ) then
C------------------------------------------------------------------
C        Move plant network solution forward in time. Overwite
C        present time-row data with most-recently calcualted values
C        for future time row:
C
C          Variable present value  = Variable future value
C
C------------------------------------------------------------------

C--------Nodal data
         do iNode = 1, npnod    ! <- npnod's in plant network

C...........Misc node data
            PUAP(iNode) =  PUAF(iNode) ! UA modulus
            PCQP(iNode) =  PCQF(iNode) ! heat flux

C...........NODE State variables (for each matrix)
            do iMatrix = 1, mpvar ! <- mpvar = # of state variabes

               CSVP( iNode, iMatrix ) = CSVF( iNode, iMatrix )

            enddo

         enddo

C--------Connection data: Note - this loop will be executed once
C--------if npcon = 0 (unlikely). Transferring 2 array locations
C--------is only marginally more expensive than checking if
C--------npcon>0.
         do iConn = 1, npcon ! <- npcon = # connections in plant

            PCTP(iConn) = PCTF(iConn) ! temperature
            PCRP(iConn) = PCRF(iconn) ! diversion-ratio

         enddo

C--------Component data

         do iComp = 1, npcomp ! # of components

            PCNTMP(iComp) =  PCNTMF(iComp) ! containment temperature

C...........Additional data
            do iMisc = 1, MPCDAT ! component has a maximum of 
                                 ! MPCDAT additional data. 

               PCDATP(iComp,iMisc) = PCDATF(iComp,iMisc)


            enddo

C...........Primitive-part data. Check to see if primitive part
C...........transport delay is active (set in subroutine TRNSDLAY)
            IF ( bDelay_Flag ) then
               nsectp(iComp) = nsectf(iComp)
               do iDelay = 1, mdly
                   tempP( iComp, iDelay ) = tempF( iComp, iDelay )
                   yposP( iComp, iDelay ) = yposF( iComp, iDelay )
                enddo
             endif

         enddo

C------------------------------------------------------------------------------
C     .  Save load associated with electrical network: this data will be
C     .  used to determine if iteration between the electical and plant
C     .  domains is required.
C------------------------------------------------------------------------------
         plt_elec_load    = elec_net_load_calc(total_load)
         plt_elec_gen     = elec_net_load_calc(total_gen)
         plt_elec_balance = elec_net_load_calc(balance)

      elseif(iOperation .eq. iRewinding ) then
C------------------------------------------------------------------
C        Restore plant network time row data to state when last
C        iteration was ajourned (ie. undo data transport effected
C        by operation iWinding above)
C
C          Variable future value  = Variable present value
C
C          Variable present value = Variable stored value
C
C------------------------------------------------------------------

C--------Nodal data
         do iNode = 1, npnod    ! <- npnod's in plant network

C...........Misc node data

            PUAF(iNode) =  PUAP(iNode) ! UA modulus
            PCQF(iNode) =  PCQP(iNode) ! heat flux

            PUAP(iNode) =  PUAP_store(iNode) ! UA modulus
            PCQP(iNode) =  PCQP_store(iNode) ! heat flux

C...........NODE State variables (for each matrix)
            do iMatrix = 1, mpvar ! <- mpvar = # of state variabes

               CSVF( iNode, iMatrix ) = CSVF( iNode, iMatrix )
               CSVP( iNode, iMatrix ) = CSVP_store( iNode, iMatrix )

            enddo

         enddo

C--------Connection data: Note - this loop will be executed once
C--------if npcon = 0 (unlikely). Transferring 2 array locations
C--------is only marginally more expensive than checking if
C--------npcon>0.
         do iConn = 1, npcon ! <- npcon = # connections in plant

            PCTF(iConn) = PCTP(iConn) ! temperature
            PCRF(iConn) = PCRP(iconn) ! diversion-ratio

            PCTP(iConn) = PCTP_store(iConn) ! temperature
            PCRP(iConn) = PCRP_store(iconn) ! diversion-ratio

         enddo

C--------Component data

         do iComp = 1, npcomp ! # of components

            PCNTMF(iComp) =  PCNTMP(iComp) ! containment temperature
            PCNTMP(iComp) =  PCNTMP_store(iComp) ! containment temperature

C...........Additional data
            do iMisc = 1, MPCDAT ! component has a maximum of 
                                 ! MPCDAT additional data.

               PCDATF(iComp,iMisc) = PCDATP(iComp,iMisc)
               PCDATP(iComp,iMisc) = PCDATP_store(iComp,iMisc)

            enddo

C...........Primitive-part data. Check to see if primitive part
C...........transport delay is active (set in subroutine TRNSDLAY)
            IF ( bDelay_Flag ) then
               nsectF(iComp) = nsectp(iComp)
               nsectp(iComp) = nsectp_store(iComp)
               do iDelay = 1, mdly
                   tempF( iComp, iDelay ) = tempP( iComp, iDelay )
                   yposF( iComp, iDelay ) = yposP( iComp, iDelay )
                   tempP( iComp, iDelay ) = tempP_store( iComp, iDelay )
                   yposP( iComp, iDelay ) = yposP_store( iComp, iDelay )
                enddo
             endif

         enddo

      endif

C.....Call subbordiante routine RadFloor_store to handle time-row
C.....manipulation for radiant heating and cooling model.
C.....RadFlooor_store uses the same iOperation syntax, permitting
C.....variable iOperation to be passed through.
C.....
C.....NOTE: the operations preformed by RadFloor_store are *VERY*
C.....expensive, and will produce a marked slow-down in simulation
C.....times whenever the radiant floor model is active. HUGE
C.....opportunities for improvement here!

      CALL RadFloor_store(iOperation)

      return
      end


