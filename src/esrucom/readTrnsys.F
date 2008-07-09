
C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.



C This file contains the following routines related to coupling
C trnsys types.

C TRNPMXT               Read the trnsys input file to establish the
C                       plant matrix template.
C readTrnsysNodes       Read the trnsys input file to get the number
C                       of nodes for a trnsys plant component.



C *********************** TRNPMXT ***********************************
C TRNPMXT is called by MZPMXT in the process of establishing the plant
C matrix template. Normally, MZPMXT accesses the plant components
C database to extract the required data. However, for a trnsys plant
C component, only dummy values are provided in the plant components
C database while actual meaningful values are provided via a text input
C file. The subroutine TRNPMXT reads the text input file and set the
C coefficient positions IPOFS1 and IPOFS2 for that trnsys component.


      SUBROUTINE TRNPMXT(IPCOMP, ICODE, NXT, MATPOS, LOCCON)
      IMPLICIT NONE

#include "plant.h"
#include "trnsys.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/FILEP/IFIL
      INTEGER IFIL

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      INTEGER NPMCOE, NPNOD, NPMTYP

      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      INTEGER NDCON, ISV

      COMMON/C16PS/IDENC(MPNODE),IDENN(MPNODE)
      INTEGER IDENC, IDENN

      COMMON / plant_coeff / iPMC_locations(mpcom,mpcoe)
      INTEGER iPMC_locations

C Common for TRNSYS general information
C     MTRNCOM: Maximum number of TRNSYS components in a plant network
C              (defined in trnsys.h)
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP       !number of trnsys components
      INTEGER ITRNTYPE      !trnsys type ID

C Declare local variables
      CHARACTER  MSG*124, outstr*124, word*124, trnsysFile*72
      INTEGER    IUNIT,ISTAT,ier,nd,ival
      INTEGER    nnode, nmatx
      INTEGER    ndpos(MPCOE),knhit(MPNODE)
      INTEGER    min_PM_threshold
      INTEGER    i, j, k, n
      INTEGER    icol, inod, ipcon, inn

C Transferred parameters
      INTEGER  IPCOMP, ICODE, NXT, MATPOS, LOCCON(MPCOM)

C Reference
      INTEGER  lnblnk



      NTRNCMP = NTRNCMP + 1
      IF(NTRNCMP .GT. MTRNCOM) THEN
         call elinc(1)
         WRITE(IUOUT,*) ' Number of TRNSYS components more than the',
     &         ' maximum value: ', MTRNCOM
         STOP 'TRNPMXT: unresolvable error on num of TRNSYS component'
      END IF

C Assign TRNSYS input data file name
C The code number [NPCDAT(?,1)]as assigned in the plant network is 
C used to define the trnsys input data file
C Assume the data file locates at ../trnsys/
      ival = NPCDAT(IPCOMP, 1)

      IF(ival .LT. 10) THEN
         WRITE(word,'(A,I1,A)') "trnsysInput",ival,".txt"
      ELSE
         WRITE(word,'(A,I2,A)') "trnsysInput",ival,".txt"
      END IF

      trnsysFile = "../trnsys/" // word(1:lnblnk(word))


C Open TRNSYS input data file
      IUNIT=IFIL+10
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EFOPSEQ(IUNIT,trnsysFile,1,ier)
      IF(IER .NE. 0) GOTO 1000
      WRITE(currentfile,'(A)') trnsysFile(1:lnblnk(trnsysFile))


C Read the TRNSYS input data file
C The data file has a predefined template
C The file has the same format as other esp-r text files

C------------------
C Read trnsys type 
C------------------
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS Data File',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETW(outstr,k,word,'W','header tags',ier)
      IF (word(1:11) .NE. 'TRNSYS-Type') THEN
         CALL edisp(IUOUT,' Header TRNSYS-Type wrong.')
      END IF

      CALL EGETWI(outstr,k,ival,1,999,'F',' TRNSYS type ',ier)
      IF(ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read TRNSYS type ')
        GOTO 1000
      END IF

      ITRNTYPE(NTRNCMP) = ival

C----------------------
C Read number of nodes 
C----------------------
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS Data File',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETW(outstr,k,word,'W','header tags',ier)
      IF (word(1:12) .NE. 'Nodes-Number') THEN
         CALL edisp(IUOUT,' Header Nodes-Number wrong.')
         GOTO 1000
      END IF

      CALL EGETWI(outstr,k,nnode,1,MNODEC,'F',' Number of nodes',ier)
      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read number of nodes ')
        GOTO 1000
      END IF

C-----------------------
C Read node connections
C-----------------------
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS Data File',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETW(outstr,k,word,'W','header tags',ier)
      IF (word(1:15) .NE. 'Node-Connection') THEN
         CALL edisp(IUOUT,' Header Node-Connection wrong.')
         GOTO 1000
      END IF

      DO i=1, nnode
         CALL EGETWI(outstr,k,ival,-1,MPCONC,'F',
     &               ' Node connections ',ier)

          IF (ier .NE. 0) THEN
          CALL edisp(IUOUT,' Cannot read node connection ')
          GOTO 1000
          END IF

          NDCON(IPCOMP, i) = ival
      END DO

C----------------------------------
C Read variable types (ISV values)
C----------------------------------
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS Data File',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETW(outstr,k,word,'W','header tags',ier)
      IF (word(1:13) .NE. 'Variable-Type') THEN
         CALL edisp(IUOUT,' Header Variable-Type wrong.')
         GOTO 1000
      END IF

      DO i=1, nnode
         CALL EGETWI(outstr,k,ival,0,29,'F',' variable type ',ier)

         IF (ier .NE. 0) THEN
         CALL edisp(IUOUT,' Cannot read variable type (ISV) ')
         GOTO 1000
         END IF

         ISV(IPCOMP, i) = ival

         ival = MOD(ival,10)

C      Check the validity of ISV values
         IF (ival .NE. 0 .AND. ival .NE. 1 .AND. ival .NE. 9) THEN 
         CALL edisp(IUOUT,' Variable type (ISV value) is invalid.')
         GOTO 1000
         END IF
      END DO

C------------------------
C A loop is used to read trnsys parameters, inputs, derivatives,
C           outputs, and additional outputs
C With this loop, the sequence of these parts does not need to
C  be predefined.
C------------------------
      DO
        k=0
        CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS Data File',ier)
        IF (ier .NE. 0) GOTO 1000

        CALL EGETW(outstr,k,word,'W','header tags',ier)

        IF(word(1:10) .EQ. 'Parameters') THEN
           CALL readTrnsysPar(IPCOMP, IUNIT, nnode)
        ELSE IF(word(1:6) .EQ. 'Inputs') THEN
           CALL readTrnsysInput(IPCOMP, IUNIT, nnode)
        ELSE IF(word(1:11) .EQ. 'Derivatives') THEN
           CALL readTrnsysDer(IUNIT)
        ELSE IF(word(1:7) .EQ. 'Outputs') THEN
           CALL readTrnsysOutput(IUNIT, nnode)
        ELSE IF(word(1:18) .EQ. 'Additional-Outputs') THEN
           CALL readTrnsysAddOut(IPCOMP, IUNIT)
        ELSE IF(word(1:4) .EQ. '*END' .OR. word(1:4) .EQ. '*end') THEN
           EXIT
        ELSE
           MSG = "Illegal identifier  "// word(1:30)
           CALL edisp(IUOUT, MSG)
           GOTO 1000
        END IF
      END DO


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


      DO 20 inod=1,nnode
         IF(ISV(IPCOMP,inod) .LT. ( min_PM_threshold -1 )*10) THEN
           call elinc(2)
           WRITE(IUOUT,*) ' Plant matrix type ',NPMTYP,' cannot be',
     &                ' established from'
           WRITE(IUOUT,*) ' component ',IPCOMP,' node ',INOD,
     &                ' coefficient generator'
           STOP ' MZPMXT: unresolvable error'
         END IF
   20 CONTINUE


C Assign matrix coefficient positions NDPOS
C TRNSYS type cpmponent has a diagonal matrix structure
      nmatx = nnode

      DO i=1, nmatx
         ndpos(i) = (i-1) * nmatx + i
      END DO


C Fill the general plant component array
      NPCDAT(IPCOMP,3)=ICODE
      NPCDAT(IPCOMP,4)=ICODE/10
      NPCDAT(IPCOMP,5)=NXT
      NPCDAT(IPCOMP,6)=nmatx
      NPCDAT(IPCOMP,8)=nnode
      NPCDAT(IPCOMP,9)=MATPOS

C Establish the coefficient location counter LOCCON for this component
      LOCCON(IPCOMP)=NXT+nmatx

C Set coefficient positions
C Establish the plant coefficient off-set vectors for this component
      DO 26 j=1,nmatx
         k = ndpos(j)-1
         IPOFS1(NXT) = MATPOS + k/nnode
         icol = MATPOS + MOD(k,nnode)
         IPOFS2(NXT,1) = icol
         IPOFS2(NXT,2) = icol
         IPOFS2(NXT,3) = icol
         IPOFS2(NXT,4) = icol
         NXT=NXT+1

C Save matrix locations for later use
         iPMC_locations(ipcomp,j) =  ndpos(j)
   26 CONTINUE
      MATPOS=MATPOS+NNODE

C Loop over all plant component inter-connections to get connection
C coefficients; in defining connections between nodes, the operative
C word is 'receive'.
C First reset each node's connection counter KNHIT

C      NCNS=0          !not required for TRNSYS type
      DO 30 inod=1,nnode
         knhit(inod)=0
   30 CONTINUE
      IF(NPCON.GT.0) THEN
         DO 39 ipcon=1,NPCON

C Check if connection relates to current component as the receiving one
            IF(IPC1(IPCON).NE.IPCOMP) GOTO 32
            n=IPN1(IPCON)
            IF(n.LT.1 .OR. n.GT.nnode .OR. NDCON(IPCOMP,n).LE.0) THEN
               call elinc(1)
               WRITE(IUOUT,*) ' No receiving connection allowed',
     &                  ' to component ',IPCOMP,' , node ',n
               STOP ' MZPMXT: unresolvable error'
            END IF
            knhit(n) = knhit(n)+1
            IF(knhit(n) .GT. NDCON(IPCOMP,n)) THEN
               call elinc(2)
               WRITE(IUOUT,*) ' Max. number of connections exceeded',
     &                  ' for component ',IPCOMP,', node ',n
               STOP ' MZPMXT: unresolvable error'
            END IF
C            NCNS=NCNS+1       !not required for TRNSYS type
            GOTO 39

C Check if connection relates to current component as the sending one,
C which is only possible in case of a type 3 inter-connection
  32        IF(IPC2(IPCON).NE.IPCOMP .OR. IPCT(IPCON).NE.3) GOTO 39
            n=IPN2(IPCON)
            IF(n.LT.1 .OR. n.GT.nnode .OR. NDCON(IPCOMP,n).EQ.0) THEN
               call elinc(1)
               WRITE(IUOUT,*) ' No connection allowed to component ',
     &                  IPCOMP,' , node ',n
               STOP ' MZPMXT: unresolvable error'
            END IF
   39    CONTINUE
      END IF

C Establish component/node identification arrays IDENC and IDENN
      DO 40 inod=1,nnode
         inn = NPCDAT(IPCOMP,9)+inod-1
         IDENC(inn) = IPCOMP
         IDENN(inn) = inod
   40 CONTINUE

C Now set number of coefficients external to component
C TRNSYS type can get the node state by itself. 
C Cross-coupling coefficiencts are not required for TRNSYS type
C         NPCDAT(IPCOMP,7)=NCNS
      NPCDAT(IPCOMP, 7) = 0

C and increment NXT to leave space for the IPOFS1 & IPOFS2
C array external connection entries
C Cross-coupling coefficiencts are not required for TRNSYS type
C so, the following line is commented out
C         NXT=NXT+NCNS


C Close the trnsys input file
      CALL ERPFREE(IUNIT,ISTAT)

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", currentfile
      CALL edisp(iuout,outstr)
      STOP ' Error in TRNSYS input file.'


      END 



C********************* readTrnsysPar **************************
C This subroutine read in trnsys parameters from the wrapper's
C data input file

      SUBROUTINE readTrnsysPar(IPCOMP, IUNIT, nnode)
      IMPLICIT NONE

#include "plant.h"
#include "trnsys.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      INTEGER NDCON, ISV

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA, BDATA

C Common for TRNSYS general information
C     MTRNCOM: Maximum number of TRNSYS components in a plant network
C              (defined in trnsys.h)
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP       !number of trnsys components
      INTEGER ITRNTYPE      !trnsys type ID

C Common for TRNSYS parameters
C     MTRNPARV: Maximum number of trnsys parameters that will be 
C               changed by esp-r (defined in trnsys.h)
      COMMON/TRNSYSPAR/NTRNPAR(MTRNCOM),
     &                 ITRNP_POS(MTRNCOM,MTRNPARV),
     &                 ITRNP_CAT(MTRNCOM,MTRNPARV),
     &                 ITRNP_LNK1(MTRNCOM,MTRNPARV),
     &                 ITRNP_LNK2(MTRNCOM,MTRNPARV)
      INTEGER NTRNPAR    !number of parameters
      INTEGER ITRNP_POS  !positions for those parameters changed by esp-r
      INTEGER ITRNP_CAT  !category of the changeable parameter
C                        ! (ie.,specific heat or density)
      INTEGER ITRNP_LNK1  !information linked to changeable parameters
C                        !This information may be a variable position,
C                        !an additional output position of this componnet
C                        !or a component number.
      INTEGER ITRNP_LNK2 !information linked to changeable parameters
C                        !Currently, this information is used for the
C                        !additional output position of another componnet

C Common for unit conversions between TRNSYS and esp-r
      COMMON/TRNSYSUNT/TRNP_UNT(MTRNCOM,MTRNPARV),
     &                 TRNX_UNT(MTRNCOM,MTRNXIN),
     &                 TRNO_T_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS1_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS2_UNT(MTRNCOM,MNODEC),
     &                 TRNO_ADD_UNT(MTRNCOM,MPCRES)
      CHARACTER*3 TRNP_UNT     !unit codes for chnageable parameters
      CHARACTER*3 TRNX_UNT     !unit codes for inputs
      CHARACTER*3 TRNO_T_UNT   !unit codes for temperature outputs 
      CHARACTER*3 TRNO_PS1_UNT !unit codes for 1st phase mass outputs
      CHARACTER*3 TRNO_PS2_UNT !unit codes for 2nd phase mass outputs
      CHARACTER*3 TRNO_ADD_UNT !unit codes for additional outputs


C Declare local variables
      CHARACTER  MSG*124, msgTemp*10, outstr*124, word*124
      INTEGER    ier,ival,nd, itemp
      INTEGER    iParv
      INTEGER    i, k
      REAL       val

C Transferred parameters
      INTEGER  IPCOMP, IUNIT, nnode


C Initialization
      iParv = 0

C Initialize ITRNP_POS in the TRNSYS common TRNSYSPAR
C This initialization is necessary because in trnsys_wrapper, -1 is
C used to mark the end of changeable parameters
      DO i=1, MTRNPARV
         ITRNP_POS(NTRNCMP,i) = -1
      END DO

      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS parameters',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETWI(outstr,k,ival,0,MADATA,'F',
     &             ' Number of parameters',ier)

      IF (ier .NE. 0) THEN
      CALL edisp(IUOUT,' Cannot read number of parameters ')
      GOTO 1000
      END IF

      NTRNPAR(NTRNCMP) = ival

C     Read trnsys parameter one by one
      DO i=1, NTRNPAR(NTRNCMP)
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS parameters',ier)
      IF (ier .NE. 0) GOTO 1000

      WRITE(msgTemp, '(I3)') i

      CALL EGETWI(outstr,k,ival, i, i,'W',' Parameter ID ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' parameter ID.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      CALL EGETP(outstr,k,word,'W',' Parameter description ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' parameter description.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
      END IF

      CALL EGETW(outstr,k,word,'W',' Parameter type ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' parameter type.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
      END IF

      IF (word(1:1) .EQ. 'Y' .OR. word(1:1) .EQ. 'y')  THEN
C     Parameter will not change within esp-r
        CALL EGETWR(outstr,k,val,0.,100.,'-',' Parameter value ',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' parameter value.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
        ADATA(IPCOMP,i) = val
      ELSE IF (word(1:1) .EQ. 'N' .OR. word(1:1) .EQ. 'n')  THEN
C     Parameter will change within esp-r
C     A changeable paprameter may be the surrounding temperature,
C     the thermal physical properties, or the additional output of
C     another component.
C     If the parameter refers to a thermal physical property, the
C     associated temperature needs to be known. This temperature
C     could be that of the connected fluid or the fluid of the trnsys
C     component itself. 
C       If the temperature is of the connected fluid, the node and
C       coupling are needed. In this case, ITRNP_LNK1 >0 is the node
C       and ITRNP_LNK2 is the coupling
C       If the temperature refers to somewhere of the trnsys component,
C       the additional output is employed to track the
C       temperature of the fluid. In this case, ITRNP_LNK1 = 0 and
C       ITRNP_LNK2 is the index of the additional output of this TRNSYS
C       component.
C
C    If the parameter refers to the additional output of another
C    component, the component no and the index of that additional
C    ouput should be provided.

        iParv = iParv + 1
        ITRNP_POS(NTRNCMP,iParv) = i

        CALL EGETWI(outstr,k,ival,1,8,'-',' parameter type',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' parameter type.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
        ITRNP_CAT(NTRNCMP,iParv) = ival

C    The case that thermal physical parameters vary with temperature
        IF(ITRNP_CAT(NTRNCMP,iParv) .GE. 1 .AND.
     &     ITRNP_CAT(NTRNCMP,iParv) .LE. 6) THEN
C   The linked node = 0 has special meaning: the temperaure should be
C   referred through an additional output of the component itself.
          CALL EGETWI(outstr,k,ival,0,nnode,'F','linked node',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' par linked node.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if

          ITRNP_LNK1(NTRNCMP,iParv) = ival

          IF(ITRNP_LNK1(NTRNCMP,iParv) .GT. 0) THEN
C       The thermal-physical parameter changes with the temperature of
C        externally-connected fluid
           itemp = NDCON(IPCOMP, ITRNP_LNK1(NTRNCMP,iParv)) !number of couplings for a node
           CALL EGETWI(outstr,k,ival,1,itemp,'F',
     &             ' linked connection ',ier)
           if (ier .NE. 0) then
           MSG = ' Cannot read ' // msgTemp(1:3) // 
     &          ' par linked connection.'
           CALL edisp(IUOUT,MSG)
           GOTO 1000
           end if
          ELSE   !   ITRNP_LNK1(NTRNCMP,iParv) .EQ. 0
C       The thermal-physical parameter changes with the temperature at somewhere in the
C       plant component itself. This temperaure should be referred through this component's
C       additional output.
C
C    MPCRES: Maximum number plant component additional output variables
C             defined in plant.h
C   Since the number of the additional outputs of this component is not known yet,
C    MPCRES is employed here to set the range check.
           CALL EGETWI(outstr,k,ival,1,MPCRES,'F',
     &             ' linked additional output ',ier)
           if (ier .NE. 0) then
           MSG = ' Cannot read ' // msgTemp(1:3) // 
     &          ' par linked additional ouput.'
           CALL edisp(IUOUT,MSG)
           GOTO 1000
           end if

          END IF  !  match --> IF(ITRNP_LNK1(NTRNCMP,i) .NE. 0)

          ITRNP_LNK2(NTRNCMP,iParv) = ival

C    The case that the parameter is the additional output of another component
        ELSEIF(ITRNP_CAT(NTRNCMP,iParv) .EQ. 8) THEN
          CALL EGETWI(outstr,k,ival,1,NPCOMP,'F','linked comp.',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' par linked comp.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
          ITRNP_LNK1(NTRNCMP,iParv) = ival

          CALL EGETWI(outstr,k,ival,1,MPCRES,'F',
     &             ' linked add. output ',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read ' // msgTemp(1:3) // 
     &          ' par linked additional output.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
          ITRNP_LNK2(NTRNCMP,iParv) = ival
        END IF

        CALL EGETW(outstr,k,word,'W',' Unit code',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' parameter unit code.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if

C        check the unit code is correct
C        Note that ival=ITRNP_CAT(NTRNCMP,iParv)
          ival = ITRNP_CAT(NTRNCMP,iParv)

          if(ival .GE. 1 .AND. ival .LE. 3) then
C          specific heat
             if(word(1:2) .NE. 'CP' .AND. word(1:2) .NE. 'cp') then
                MSG = msgTemp(1:3) //' par. indicates specific heat.'
     &               // ' Its unit code is wrong.'
             CALL edisp(IUOUT,MSG)
             GOTO 1000
             end if
          else if(ival .GE. 4 .AND. ival .LE. 6) then
C           density
             if(word(1:2) .NE. 'DN' .AND. word(1:2) .NE. 'dn') then
                MSG = msgTemp(1:3)//' parameter indicates density.'
     &               // ' Its unit code is wrong.'
             CALL edisp(IUOUT,MSG)
             GOTO 1000
             end if
          else if(ival .EQ. 7) then
C           temperature
             if(word(1:2) .NE. 'TE' .AND. word(1:2) .NE. 'te') then
                MSG = msgTemp(1:3)//' parameter indicates temperature.'
     &               // ' Its unit code is wrong.'
             CALL edisp(IUOUT,MSG)
             GOTO 1000
             end if

C        additional cases can be added here
          end if   ! ---> match if(ival .GE. 1 .AND. ival .LE. 3)

        TRNP_UNT(NTRNCMP,iParv) = word(1:3)

      ELSE
        CALL edisp(IUOUT, ' Invalid parameter type, neither N nor Y')
        GOTO 1000
      END IF        ! ---> if (word(1:1) .EQ. 'Y') 
      END DO        ! ---> DO i=1, NTRNPAR(NTRNCMP)

C  Check that the number of changeable parameters is not beyond the limit
      IF(iParv .GT. MTRNPARV) THEN
         MSG = 'Number of changeable parameters beyond the limit'
         CALL edisp(IUOUT, MSG)
         GOTO 1000
      END IF

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", currentfile
      CALL edisp(iuout,outstr)
      STOP ' Error in Reading TRNSYS Parameters.'


      END  ! of subrotuine readTrnsysPar




C********************* readTrnsysInput **************************
C This subroutine read in trnsys inputs from the wrapper's
C data input file

      SUBROUTINE readTrnsysInput(IPCOMP, IUNIT, nnode)
      IMPLICIT NONE

#include "plant.h"
#include "trnsys.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      INTEGER NDCON, ISV

C Common for TRNSYS general information
C     MTRNCOM: Maximum number of TRNSYS components in a plant network
C              (defined in trnsys.h)
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP       !number of trnsys components
      INTEGER ITRNTYPE      !trnsys type ID

C Common for TRNSYS inputs
C     MTRNXIN: Maximum number of variables for a TRNSYS type
C              (defined in trnsys.h)
      COMMON/TRNSYSINP/TRNXIN(MTRNCOM, MTRNXIN),
     &                 NTRNXIN(MTRNCOM),
     &                 ITRNX_CAT(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK1(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK2(MTRNCOM,MTRNXIN)
      DOUBLE PRECISION  TRNXIN   !trnsys input values
      INTEGER NTRNXIN            !number of trnsys inputs
      INTEGER ITRNX_CAT          !type of trnsys inputs
      INTEGER ITRNX_LNK1         !associated receiving node or
C                                ! component ID or
C                                ! control ID
      INTEGER ITRNX_LNK2         !associated coupling or
C                                ! the additional output ID

C Common for unit conversions between TRNSYS and esp-r
      COMMON/TRNSYSUNT/TRNP_UNT(MTRNCOM,MTRNPARV),
     &                 TRNX_UNT(MTRNCOM,MTRNXIN),
     &                 TRNO_T_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS1_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS2_UNT(MTRNCOM,MNODEC),
     &                 TRNO_ADD_UNT(MTRNCOM,MPCRES)
      CHARACTER*3 TRNP_UNT     !unit codes for chnageable parameters
      CHARACTER*3 TRNX_UNT     !unit codes for inputs
      CHARACTER*3 TRNO_T_UNT   !unit codes for temperature outputs 
      CHARACTER*3 TRNO_PS1_UNT !unit codes for 1st phase mass outputs
      CHARACTER*3 TRNO_PS2_UNT !unit codes for 2nd phase mass outputs
      CHARACTER*3 TRNO_ADD_UNT !unit codes for additional outputs


C Declare local variables
      CHARACTER  MSG*124, msgTemp*10, outstr*124, word*124
      INTEGER    iCtrl, iFunc
      INTEGER    iCat
      INTEGER    ier,ival,nd, itemp
      INTEGER    i, k
      REAL       val

C Transferred parameters
      INTEGER  IPCOMP, IUNIT, nnode


C Initialization
      iCtrl = 0
      iFunc = 0

      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS inputs',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETWI(outstr,k,ival,1,MTRNXIN,'F',' Num of inputs',ier)
      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read number of inputs.')
        GOTO 1000
      END IF

      NTRNXIN(NTRNCMP) = ival

      DO i=1, NTRNXIN(NTRNCMP)
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS inputs',ier)
      IF (ier .NE. 0) GOTO 1000

      WRITE(msgTemp,'(I3)') i

      CALL EGETWI(outstr,k,ival, i, i,'W',' Input ID ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' input ID.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      CALL EGETP(outstr,k,word,'W',' Input description ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' input description.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
      END IF

      CALL EGETWI(outstr,k,ival,-3, 6,'-',' Input type ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' input type.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
      END IF

C     ITRNX_CAT options:
C       =-3 input is outdoor temperature
C       =-2 input is a control variable (regard unchanged control as constant)
C       =-1 input is environment temperature
C       = 0 input is not linked with other infomation, user input is used directly
C       = 1 input is temperature, linked with specified node and connection
C       = 2 input is 1st phase mass flow, linked with specified node and connection
C       = 3 input is 2nd phase mass flow, linked with specified node and connection
C       = 4 input is the sum of 1st and 2nd phase mass flow, linked with specified 
C                    node and connection
C       = 5 input is the additional output, linked with specified comp. and id
C       = 6 input is the return value of a function
      ITRNX_CAT(NTRNCMP,i) = ival

      iCat = ITRNX_CAT(NTRNCMP,i)

      IF (iCat .EQ. 0)  THEN 
C     user input value is used directly
        CALL EGETWR(outstr,k,val,0.,100.,'-',' Input value ',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' input value.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
        TRNXIN(NTRNCMP,i) = val
      END IF 

      IF(iCat .GE. 1 .AND. iCat .LE. 4 ) THEN
C      input linked with specified node and coupling
        CALL EGETWI(outstr,k,ival,1,nnode,'F','linked node',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' input linked node.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if

        ITRNX_LNK1(NTRNCMP,i) = ival
        itemp = NDCON(IPCOMP, ival)   !number of couplings for a node

        CALL EGETWI(outstr,k,ival,1,itemp,'F',
     &             ' linked connection ',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read ' // msgTemp(1:3) // 
     &          ' input linked connection.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
        ITRNX_LNK2(NTRNCMP,i) = ival
      END IF    !----> match IF(iCat .GE. 1 .AND. iCat .LE. 4)

      IF(iCat .EQ. 5) THEN
C     input is the additional output of specified component
        CALL EGETWI(outstr,k,ival,1,NPCOMP,'F','linked comp.',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' input linked comp.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if

        ITRNX_LNK1(NTRNCMP,i) = ival
C      The following lines of code are incorrect because not all 
C      components have their NAPDAT() defined. Note that NAPDAT 
C      is defined by MZPMXT, instead of pltcfg. Therefore, 
C      MPCRES is used as the upper bound.
C        itemp = NAPDAT(ival)   ! number of additional outputs for
C                              ! the specified component
C        CALL EGETWI(outstr,k,ival,1,itemp,'F',
C     &             ' linked add. output ',ier)

        CALL EGETWI(outstr,k,ival,1,MPCRES,'F',
     &             ' linked add. output ',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read ' // msgTemp(1:3) // 
     &          ' input linked additional output.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
        ITRNX_LNK2(NTRNCMP,i) = ival
      END IF    !----> match IF(iCat .EQ. 4)

      IF(iCat .EQ. 6) THEN
C     input is the return value of a function
        iFunc = iFunc + 1
        ITRNX_LNK1(NTRNCMP, i) = iFunc
      ENDIF


      IF(iCat .EQ. -2) THEN
C       input is a control variable
        iCtrl = iCtrl + 1
        ITRNX_LNK1(NTRNCMP,i) = iCtrl
C       Read initial control value
        CALL EGETWR(outstr,k,val,0.,100.,'-',' Initial control',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' input value.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if
        CDATA(IPCOMP,iCtrl) = val
      END IF

C     Read unit code if a nonzero iCat exists
C     Note that user provided input (iCat =0) and a function value (iCat =6)
C               do not need unit conversion factors
      IF(iCat .NE. 0 .AND. iCat .NE. 6) THEN
        CALL EGETW(outstr,k,word,'W',' Unit code',ier)
          if (ier .NE. 0) then
          MSG = ' Cannot read '//msgTemp(1:3)//' input unit code.'
          CALL edisp(IUOUT,MSG)
          GOTO 1000
          end if

C        check the unit code is correct
          if(iCat .EQ. -3 .OR. iCat .EQ. -1 .OR. iCat .EQ. 1) then
             if(word(1:2) .NE. 'TE' .AND. word(1:2) .NE. 'te') then
             MSG = msgTemp(1:3) //' input unit code is wrong '
             CALL edisp(IUOUT,MSG)
             GOTO 1000
             end if
          else if(iCat .EQ. 2 .OR. iCat .EQ. 3) then
             if(word(1:2) .NE. 'MF' .AND. word(1:2) .NE. 'mf') then
             MSG = msgTemp(1:3)//' input unit code is wrong '
             CALL edisp(IUOUT,MSG)
             GOTO 1000
             end if
C         For the case of iCat=4, unit code is required but it cannot 
C         be checked for validity because the additional output unit is 
C         not predefined.
          end if

        TRNX_UNT(NTRNCMP,i) = word(1:3)
      END IF  ! match ---> IF(iCat .NE. 0)

      END DO        ! ---> DO i=1, NTRNXIN(NTRNCMP)

C  Check that the number of control variables is not beyond the limit MCL
C  MCL: maximum number of control loops as defined in plant.h
      IF(iCtrl .GT. MCL) THEN
         MSG = 'Number of control variables beyond the limit'
         CALL edisp(IUOUT, MSG)
         GOTO 1000
      END IF

C   Assign value to NCI
      NCI(IPCOMP) = iCtrl

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", currentfile
      CALL edisp(iuout,outstr)
      STOP ' Error in Reading TRNSYS inputs.'

      END   ! of subroutine readTrnsysInput




C********************* readTrnsysDer **************************
C This subroutine read in trnsys derivatives, if available,
C from the wrapper's data input file

      SUBROUTINE readTrnsysDer(IUNIT)
      IMPLICIT NONE

#include "trnsys.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Common for TRNSYS general information
C     MTRNCOM: Maximum number of TRNSYS components in a plant network
C              (defined in trnsys.h)
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP       !number of trnsys components
      INTEGER ITRNTYPE      !trnsys type ID

C Common for TRNSYS derivatives
C    MTRNDER: Maximum number of derivatives for a TRNSYS type
      COMMON/TRNSYSDER/NTRNDER(MTRNCOM),
     &                 TRNDER_SET(MTRNCOM),
     &                 TRNDER_INT(MTRNCOM,MTRNDER) 
      INTEGER NTRNDER        !number of derivatives
      LOGICAL TRNDER_SET     !indicates whether the initial values are
C                            ! user inputs or initialized by esp-r
      DOUBLE PRECISION TRNDER_INT !initial values


C Declare local variables
      CHARACTER  MSG*124, msgTemp*10, outstr*124
      INTEGER    ier,ival,nd
      INTEGER    i, k
      REAL       val

C Transferred parameters
      INTEGER  IUNIT


      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS derivatives',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETWI(outstr,k,ival,0,MTRNDER,'F',
     &             ' Number of derivatives',ier)
      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read number of derivatives.')
        GOTO 1000
      END IF

      NTRNDER(NTRNCMP) = ival

      CALL EGETWI(outstr,k,ival,0,1,'F',' Initialization method',ier)
      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read initialization method.')
        GOTO 1000
      END IF

      IF(ival .EQ. 0) THEN
         TRNDER_SET(NTRNCMP) = .false. ! initialized by esp-r
      ELSE
         TRNDER_SET(NTRNCMP) = .true.  ! initial values provided
      END IF

C    User provided initial values do not need unit conversions
      IF(TRNDER_SET(NTRNCMP)) THEN
      DO i=1, NTRNDER(NTRNCMP)
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS initial values',ier)
      IF (ier .NE. 0) GOTO 1000

      WRITE(msgTemp, '(I3)') i

      CALL EGETWI(outstr,k,ival, i, i,'W',' Initial value ID ',ier)

      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' initial value ID.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      CALL EGETWR(outstr,k,val,0.,100.,'-',' Initial value ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' initial value.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
      END IF

      TRNDER_INT(NTRNCMP,i) = val

      END DO       !----> DO i=1, NTRNDER(NTRNCMP) 
      END IF       !----> IF(TRNDER_SET(NTRNCMP)) 

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", currentfile
      CALL edisp(iuout,outstr)
      STOP ' Error in Reading TRNSYS Derivatives.'

      END   ! of subroutine readTrnsysDer




C********************* readTrnsysOutput **************************
C This subroutine read in the section for coefficients outputs
C from the wrapper's data input file

      SUBROUTINE readTrnsysOutput(IUNIT, nnode)
      IMPLICIT NONE

#include "plant.h"
#include "trnsys.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Common for TRNSYS general information
C     MTRNCOM: Maximum number of TRNSYS components in a plant network
C              (defined in trnsys.h)
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP       !number of trnsys components
      INTEGER ITRNTYPE      !trnsys type ID

C Common for TRNSYS outputs
C     MNODEC: maximum number of nodes per plant component
C     MCPRES: maximum number of plant additional outputs
C            (Both are defined in plant.h)
      COMMON/TRNSYSOUT/ITRNO_T_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_PS1_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_PS2_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_ADD_LNK(MTRNCOM,MPCRES),
     &                 TRN_ADD_DESC(MTRNCOM,MPCRES)
      INTEGER ITRNO_T_LNK      !trnsys output position for temperature
      INTEGER ITRNO_PS1_LNK    !trnsys output or input position for 1st phase mass
      INTEGER ITRNO_PS2_LNK    !trnsys output or input position for 2nd phase mass
      INTEGER ITRNO_ADD_LNK    !trnsys output position for esp-r additional outputs
      CHARACTER TRN_ADD_DESC*80 !description for additional outputs

C Common for TRNSYS inputs
C     MTRNXIN: Maximum number of variables for a TRNSYS type
C              (defined in trnsys.h)
      COMMON/TRNSYSINP/TRNXIN(MTRNCOM, MTRNXIN),
     &                 NTRNXIN(MTRNCOM),
     &                 ITRNX_CAT(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK1(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK2(MTRNCOM,MTRNXIN)
      DOUBLE PRECISION  TRNXIN   !trnsys input values
      INTEGER NTRNXIN            !number of trnsys inputs
      INTEGER ITRNX_CAT          !type of trnsys inputs
      INTEGER ITRNX_LNK1         !associated receiving node or
C                                ! component ID or
C                                ! control ID
      INTEGER ITRNX_LNK2         !associated coupling or
C                                ! the additional output ID

C Common for unit conversions between TRNSYS and esp-r
      COMMON/TRNSYSUNT/TRNP_UNT(MTRNCOM,MTRNPARV),
     &                 TRNX_UNT(MTRNCOM,MTRNXIN),
     &                 TRNO_T_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS1_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS2_UNT(MTRNCOM,MNODEC),
     &                 TRNO_ADD_UNT(MTRNCOM,MPCRES)
      CHARACTER*3 TRNP_UNT     !unit codes for chnageable parameters
      CHARACTER*3 TRNX_UNT     !unit codes for inputs
      CHARACTER*3 TRNO_T_UNT   !unit codes for temperature outputs 
      CHARACTER*3 TRNO_PS1_UNT !unit codes for 1st phase mass outputs
      CHARACTER*3 TRNO_PS2_UNT !unit codes for 2nd phase mass outputs
      CHARACTER*3 TRNO_ADD_UNT !unit codes for additional outputs


C Declare local variables
      CHARACTER  MSG*124, msgTemp*10, outstr*124, word*124
      INTEGER    ier,ival,nd
      INTEGER    i, k

C Transferred parameters
      INTEGER  IUNIT, nnode


      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS outputs',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETWI(outstr,k,ival,nnode,nnode,'F',
     &             ' Number of outputs map',ier)

      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read number of outputs map.')
        GOTO 1000
      END IF

C     Read outputs map one by one, each map occupies one line
      DO i=1, nnode
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS outputs map',ier)
      IF (ier .NE. 0) GOTO 1000

      WRITE(msgTemp,'(I3)') i

      CALL EGETWI(outstr,k,ival, i, i,'W',' Output ID ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' output ID.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      CALL EGETWI(outstr,k,ival, 0, MTRNOUT,'F',' Output map ',ier)
      IF (ier .NE. 0) THEN
        MSG = 'Cannot read '//msgTemp(1:3)//' temperature output map.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      ITRNO_T_LNK(NTRNCMP,i) = ival      ! temperature link

      CALL EGETW(outstr,k,word,'W',' Unit code',ier)
        if (ier .NE. 0) then
        MSG = ' Cannot read '//msgTemp(1:3)//' output temperature ' //
     &        'unit code.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if

C        check the temperature unit code is correct
        if(word(1:2) .NE. 'TE' .AND. word(1:2) .NE. 'te') then
        MSG = msgTemp(1:3)//' output temperature ' //
     &        'unit code is wrong.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if

      TRNO_T_UNT(NTRNCMP,i) = word(1:3)

C     In some cases, the required mass flow is not reported in trnsys
C     output. Thus, it may be necessary to link trnsys inputs.
C     Use negative values to indicate that the output is linked to 
C     trnsys inputs.
      CALL EGETWI(outstr,k,ival, -NTRNXIN(NTRNCMP), MTRNOUT,'F',
     &           ' Output map ',ier)
      IF (ier .NE. 0) THEN
        MSG =' Cannot read '//msgTemp(1:3)//' phase 1 mass output map.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      ITRNO_PS1_LNK(NTRNCMP,i) = ival    ! First phase mass link

      CALL EGETW(outstr,k,word,'W',' Unit code',ier)
        if (ier .NE. 0) then
        MSG = ' Cannot read '//msgTemp(1:3)//' output phase 1 mass ' //
     &        'unit code.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if

C        check the unit code of mass flow rate is correct
        if(word(1:2) .NE. 'MF' .AND. word(1:2) .NE. 'mf') then
        MSG = msgTemp(1:3) //' output phase 1 mass ' //
     &        'unit code is wrong.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if

      TRNO_PS1_UNT(NTRNCMP,i) = word(1:3)

      CALL EGETWI(outstr,k,ival, -NTRNXIN(NTRNCMP), MTRNOUT,'F',
     &          ' Output map ',ier)
      IF (ier .NE. 0) THEN
        MSG =' Cannot read '//msgTemp(1:3)//' phase 2 mass output map.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      ITRNO_PS2_LNK(NTRNCMP,i) = ival    ! second phase mass link

      CALL EGETW(outstr,k,word,'W',' Unit code',ier)
        if (ier .NE. 0) then
        MSG = ' Cannot read '//msgTemp(1:3)//' output phase 2 mass ' //
     &        'unit code.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if

C        check the unit code of mass flow rate is correct
        if(word(1:2) .NE. 'MF' .AND. word(1:2) .NE. 'mf') then
        MSG = msgTemp(1:3)//' output phase 2 mass ' //
     &        'unit code is wrong.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if

      TRNO_PS2_UNT(NTRNCMP,i) = word(1:3)

      END DO     !----> DO i=1, nnode

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", currentfile
      CALL edisp(iuout,outstr)
      STOP ' Error in Reading TRNSYS outputs for ESP-r coefficiencts.'

      END     ! of subroutine readTrnsysOutput




C********************* readTrnsysAddOut **************************
C This subroutine read in the section for additional outputs
C from the wrapper's data input file

      SUBROUTINE readTrnsysAddOut(IPCOMP, IUNIT)
      IMPLICIT NONE

#include "plant.h"
#include "trnsys.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

C Common for TRNSYS general information
C     MTRNCOM: Maximum number of TRNSYS components in a plant network
C              (defined in trnsys.h)
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP       !number of trnsys components
      INTEGER ITRNTYPE      !trnsys type ID

C Common for TRNSYS outputs
C     MNODEC: maximum number of nodes per plant component
C     MCPRES: maximum number of plant additional outputs
C            (Both are defined in plant.h)
      COMMON/TRNSYSOUT/ITRNO_T_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_PS1_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_PS2_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_ADD_LNK(MTRNCOM,MPCRES),
     &                 TRN_ADD_DESC(MTRNCOM,MPCRES)
      INTEGER ITRNO_T_LNK      !trnsys output position for temperature
      INTEGER ITRNO_PS1_LNK    !trnsys output or input position for 1st phase mass
      INTEGER ITRNO_PS2_LNK    !trnsys output or input position for 2nd phase mass
      INTEGER ITRNO_ADD_LNK    !trnsys output position for esp-r additional outputs
      CHARACTER TRN_ADD_DESC*80 !description for additional outputs

C Common for unit conversions between TRNSYS and esp-r
      COMMON/TRNSYSUNT/TRNP_UNT(MTRNCOM,MTRNPARV),
     &                 TRNX_UNT(MTRNCOM,MTRNXIN),
     &                 TRNO_T_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS1_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS2_UNT(MTRNCOM,MNODEC),
     &                 TRNO_ADD_UNT(MTRNCOM,MPCRES)
      CHARACTER*3 TRNP_UNT     !unit codes for chnageable parameters
      CHARACTER*3 TRNX_UNT     !unit codes for inputs
      CHARACTER*3 TRNO_T_UNT   !unit codes for temperature outputs 
      CHARACTER*3 TRNO_PS1_UNT !unit codes for 1st phase mass outputs
      CHARACTER*3 TRNO_PS2_UNT !unit codes for 2nd phase mass outputs
      CHARACTER*3 TRNO_ADD_UNT !unit codes for additional outputs


C Declare local variables
      CHARACTER  MSG*124, msgTemp*10, outstr*124, word*124
      INTEGER    ier,ival,nd
      INTEGER    i, k

C Transferred parameters
      INTEGER  IPCOMP, IUNIT



      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS additional outputs',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETWI(outstr,k,ival,0,MPCRES,'F',
     &           ' Number of additional outputs',ier)
      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read number of additional outputs.')
        GOTO 1000
      END IF

      NAPDAT(IPCOMP) = ival

C     Read additional output map one by one. Each occupies a line.
      DO i=1, NAPDAT(IPCOMP)
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS additional outputs',ier)
      IF (ier .NE. 0) GOTO 1000

      WRITE(msgTemp,'(I3)') i

      CALL EGETWI(outstr,k,ival,i,i,'W',' Additional output ID ',ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' additional output ID.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      CALL EGETP(outstr,k,word,'W',' Additional output description ',
     &          ier)
      IF (ier .NE. 0) THEN
        MSG = ' Cannot read '//msgTemp(1:3)//' additional output desc.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
      END IF

      TRN_ADD_DESC(NTRNCMP,i) = word(1:80)

      CALL EGETWI(outstr,k,ival, 1, MTRNOUT,'F',
     &           ' Additional output position ',ier)
      IF (ier .NE. 0) THEN
        MSG= ' Cannot read '//msgTemp(1:3)//' add. output position.'
        CALL edisp(IUOUT, MSG )
        GOTO 1000
      END IF

      ITRNO_ADD_LNK(NTRNCMP,i) = ival

      CALL EGETW(outstr,k,word,'W',' Unit code',ier)
        if (ier .NE. 0) then
        MSG = ' Cannot read '//msgTemp(1:3)//' additional output ' //
     &        'unit code.'
        CALL edisp(IUOUT,MSG)
        GOTO 1000
        end if
      TRNO_ADD_UNT(NTRNCMP,i) = word(1:3)
      END DO       !----> DO i=1, NAPDAT(IPCOMP) 

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", currentfile
      CALL edisp(iuout,outstr)
      STOP ' Error in Reading TRNSYS additional outputs 
     &         for ESP-r coefficiencts.'

      END     ! of subroutine readTrnsysAddOut





C********************* readTrnsysNodes **************************
C This subroutine read the number of nodes for a trnsys
C component from its data input file


      SUBROUTINE readTrnsysNodes(id_trnsysCMP,NNODES)
      IMPLICIT NONE
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/FILEP/IFIL
      INTEGER IFIL

C Trnsferred parameters
      INTEGER  id_trnsysCMP, NNODES

C local variables
      CHARACTER  word*124, outstr*124, trnsysFile*72
      INTEGER    IUNIT,ISTAT,ier,nd,ival, k

C Reference
      INTEGER  lnblnk


      IF(id_trnsysCMP .LT. 10) THEN
        WRITE(word,'(A,I1,A)') "trnsysInput",id_trnsysCMP,".txt"
      ELSE
        WRITE(word,'(A,I2,A)') "trnsysInput",id_trnsysCMP,".txt"
      END IF

      trnsysFile = "../trnsys/" // word(1:lnblnk(word))


C Open TRNSYS input data file
      IUNIT=IFIL+10
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EFOPSEQ(IUNIT,trnsysFile,1,ier)
      IF(IER .NE. 0) GOTO 1000

C     Read trnsys type 
      DO
      k=0
      CALL STRIPC(IUNIT,outstr,0,nd,1,'TRNSYS Data File',ier)
      IF (ier .NE. 0) GOTO 1000

      CALL EGETW(outstr,k,word,'W','header tags',ier)
      IF (word(1:12) .NE. 'Nodes-Number') CYCLE

      CALL EGETWI(outstr,k,ival,1,MNODEC,'F',' Number of nodes',ier)
      IF (ier .NE. 0) THEN
        CALL edisp(IUOUT,' Cannot read number of nodes.')
        GOTO 1000
      END IF

      NNODES = ival
      EXIT
      END DO

C Close the trnsys input file
      CALL ERPFREE(IUNIT,ISTAT)

      RETURN

C Error handling.
 1000 ier=1
      WRITE(outstr,'(2A)') "TRNSYS input file error: ", trnsysFile
      CALL edisp(iuout,outstr)
      STOP ' Error in TRNSYS input file.'

      END   ! of subroutine readTrnsysNodes

