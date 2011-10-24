
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

C trnsys_static_temp    Static template for trnsys components.
C trnsys_coeff_gen      Coefficient generator for trnsys components.
C getConversionFactors  Get the unit conversion factors between esp-r
C                       and trnsys
C checkTrnsysError      Check whether error occurs in trnsys
C                       ! not used in the current stage.


C Reference:
C Weimin Wang and Ian Beausoleil-Morrison,2008. "An extensive methodology
C    for coupling TRNSYS components to ESP-r". In Proceedings of eSim
C    Conference. Quebec City, CANADA. 8 pages.




C*********************** trnsys_static_temp********************************
C This subroutine is the static template for a trnsys plant component. Due to
C the generic characteristics of trnsys wrapper, this subroutine checks the
C following aspects:
C 1. Validity for the trnsys inputs linked to fluid connections
C 2. Validity check for those trnsys inputs linked to additional outputs
C 3. Check whether the connections to each node are of the correct type.
C 4. Check whether the containment of the trnsys component should be defined


      SUBROUTINE trnsys_static_temp(IPCOMP)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "trnsys.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON /pcddsc/ pcdesc(maxpc), npref(mpcom)
      CHARACTER pcdesc*80
      INTEGER NPREF

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)
      INTEGER NCONT, IPCC, INDCP
      REAL CNTDAT

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      INTEGER NDCON, ISV

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

C Please refer to SUBROUTINE TRNPMXT at esruplt/pmatrix.F for the 
C comments about the following TRNSYS commons.
C
C Common for TRNSYS general information
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP,ITRNTYPE

C Common for TRNSYS parameters
      COMMON/TRNSYSPAR/NTRNPAR(MTRNCOM),
     &                 ITRNP_POS(MTRNCOM,MTRNPARV),
     &                 ITRNP_CAT(MTRNCOM,MTRNPARV),
     &                 ITRNP_LNK1(MTRNCOM,MTRNPARV),
     &                 ITRNP_LNK2(MTRNCOM,MTRNPARV)
      INTEGER NTRNPAR, ITRNP_POS,ITRNP_CAT,ITRNP_LNK1,ITRNP_LNK2

C Common for TRNSYS inputs
      COMMON/TRNSYSINP/TRNXIN(MTRNCOM, MTRNXIN),
     &                 NTRNXIN(MTRNCOM),
     &                 ITRNX_CAT(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK1(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK2(MTRNCOM,MTRNXIN)
      DOUBLE PRECISION    TRNXIN
      INTEGER NTRNXIN, ITRNX_CAT,ITRNX_LNK1,ITRNX_LNK2


C Transferred parameter
      INTEGER  IPCOMP

C Local variables
      INTEGER  nnode, iTRNCOM, nTYPE, numADATA
      INTEGER  i, itemp, iPos, iLnk1, iLnk2
      INTEGER  iCat, iNod, iCon, net_cndx, iPC
      INTEGER  num_ISV
      LOGICAL  mistake, mark_NC(MNODEC,MPCONC), environment



C Get number of nodes
      nnode = NPCDAT(IPCOMP,8)


C initialize mark_NC(::)
C mark_NC is used to mark whether a node and connection is linked with trnsys inputs
C linked = true, unlinked = false
      DO iNod=1, nnode
         DO iCon=1, MPCONC
            mark_NC(iNod, iCon) = .false.
         END DO
      END DO


C Find the index of the trnsys component and its type
      iTRNCOM = 0      ! initialization
      DO i=1, IPCOMP
         IF(NPREF(i) .EQ. 85) iTRNCOM = iTRNCOM+1
      END DO

      nTYPE = ITRNTYPE(iTRNCOM)

C Write out ADATA if there is a trace output.
C Trnsys type does not have BDATA
      IF(ITC.GT.0 .AND. ITRACE(35).NE.0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for '
        WRITE(ITU,*) ' the TRNSYS type: ', nTYPE
        numADATA = NTRNPAR(iTRNCOM)    ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,i),i=1,numADATA)
        IF(ITU.EQ.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp.EQ.IPCOMP .OR. IPCOMP.EQ.NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C The number of contrls are defined by TRNPMXT in esruplt/pmatrix.F
C Since the number of controls is not pre-defined for a generic trnsys
C component, control is not check here.


C--------------------------
C Validity check for those trnsys inputs linked to fluid connections
C--------------------------

C Check that each node in the component has the correct number of connections
C to other components. A trnsys component needs to follow two rules:
C 1st rule: if a trnsys input is linked to a connection, this connection 
C           must be defined.
C 2nd rule: if a trnsys componet defines a (receiving) connection, this connection
C           must be reflected in its input.
C
C Based on the above two rules, the check of node connections has two steps:
C Step 1: Input-linked connections must be defined.
C Step 2: Input-unlinked connections must not exist.

C Step 1: Input-linked connections must be defined.
      mistake = .false.
C  NTRNXIN(): number of inputs for a trnsys type
      DO i=1, NTRNXIN(iTRNCOM)
         iCat = ITRNX_CAT(iTRNCOM,i)
         IF(iCat .GE. 1 .AND. iCat .LE. 3) THEN 
C        the input is linked to a specified connection
            iLnk1 = ITRNX_LNK1(iTRNCOM,i)    ! node
            iLnk2 = ITRNX_LNK2(iTRNCOM,i)    ! coupling
            mark_NC(iLnk1, iLnk2) = .true.
            IF(ICONDX(IPCOMP,iLnk1,iLnk2) .EQ. 0 ) THEN
               mistake = .true.
               EXIT
            END IF
         END IF
      END DO

      IF(mistake)THEN
        WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
        WRITE(IUOUT,*) ' input-linked connection unavailable',
     &              ' for component: ',IPCOMP, '; trnsys type: ',nTYPE
        STOP ' trnsys_static_temp: unresolvable error'
      END IF

C Step 2: Input-unlinked connections must not exist.
      mistake = .false.
      DO iNod=1, nnode
         DO iCon=1, MPCONC
            IF(mark_NC(iNod, iCon)) CYCLE
            IF(ICONDX(IPCOMP,iNod, iCon) .NE. 0) THEN
               mistake = .true.
               EXIT
            END IF
         END DO
         IF( mistake) EXIT
      END DO

      IF(mistake)THEN
        WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
        WRITE(IUOUT,*) ' input un-linked connection available',
     &              ' for component: ',IPCOMP, ';trnsys type: ',nTYPE
        STOP ' trnsys_static_temp: unresolvable error'
      END IF


C--------------------------
C Validity check for those trnsys inputs linked to additional outputs
C--------------------------

C This check includes two aspects:
C 1. If the trnsys input is an additional output of a component, this componet
C    must be connected to the trnsys component.
C 2. The additional output must exist for that component.

      mistake = .true.
C  NTRNXIN(): number of inputs for a trnsys type
      DO i=1, NTRNXIN(iTRNCOM)
         iCat = ITRNX_CAT(iTRNCOM,i)
         IF(iCat .EQ. 5) THEN 
C        input is linked to the additional output of a specified component
            iLnk1 = ITRNX_LNK1(iTRNCOM,i)   ! component
            iLnk2 = ITRNX_LNK2(iTRNCOM,i)   ! additional output id

C          Check the first aspect
            DO iNod=1, nnode
               DO iCon=1, NDCON(IPCOMP, iNod)
                  net_cndx = ICONDX(IPCOMP,iNod,iCon)
                  if(iLnk1 .EQ. IPC2(net_cndx)) then
                     mistake = .false.
                     EXIT
                  end if
               END DO

               IF(mistake .EQV. .false.) EXIT

            END DO    ! match--> DO iNod=1, nnode

            IF( mistake) THEN
              WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
              WRITE(IUOUT,*) ' input-linked additional output belongs',
     &             ' to a component not connected to trnsys',
     &             ' component: ',IPCOMP, '; trnsys type: ',nTYPE
              STOP ' trnsys_static_temp: unresolvable error'
            ELSE
C           Check the second aspect
              if( iLnk2 .GT. NAPDAT(iLnk1)) THEN
                 mistake = .true.
                 WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
                 WRITE(IUOUT,*) ' input-linked additional output ',
     &             ' does not exist'
                 STOP ' trnsys_static_temp: unresolvable error'
              end if
            END IF  ! match--> IF( mistake)
         END IF     ! match --> IF(iCat .EQ. 4)
      END DO        ! match --> DO i=1, NTRNXIN(iTRNCOM)



C-------------------------------
C Check that the connections to each node are of the correct type.
C-------------------------------

C Since the match of state vriable index (ISV) between sending and receiving nodes
C for each connection has been checked by MZPMXT, the check here focuses
C on those trnsys parameters that need to be updated within esp-r.
      mistake = .false.

      DO i=1, MTRNPARV
        iPos = ITRNP_POS(iTRNCOM,i)

C        iPos = -1 indicates the end of changable parameters
        IF(iPos .EQ. -1) EXIT

        iCat = ITRNP_CAT(iTRNCOM,i)

C      iCat = 7 means the parameter refers to environment temperature.
C      No links for this case.
        IF( iCat .EQ. 7) CYCLE

        iLnk1 = ITRNP_LNK1(iTRNCOM,i)
        iLnk2 = ITRNP_LNK2(iTRNCOM,i)

        IF(iCat .EQ. 8) THEN
C   The parameter is the additional output of another component.
C   Check that additional output exists.
C
           IF( iLnk2 .GT. NAPDAT(iLnk1)) THEN
           WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
           WRITE(IUOUT,*) ' parameter-linked additional output ',
     &             ' does not exist'
           STOP ' trnsys_static_temp: unresolvable error'
           END IF
        ELSE IF(iCat .GE. 1 .AND. iCat .LE. 6) THEN
C  The thermal physical parameter changes with a temperature.
C  This temperature may be with the fluid of an external conncetion
C  or with a specified location in the component itself.
C  These two cases are implemented separately.
C
C    Changeable parameter linked to external connection
          IF(iLnk1 .NE. 0) THEN
C      Get the variable index for the referred connection
            iNod = iLnk1
            iCon = iLnk2
            net_cndx = ICONDX(IPCOMP,iNod,iCon)
            num_ISV = ICONTP(net_cndx)

C    Changeable parameter linked to additional output
          ELSE IF(iLnk1 .EQ. 0) THEN
C    Check that the linked additional output exist
C     This check was not performed in TRNPMXT because parameters
C     are read before additional outputs
            IF(iLnk2 .GT. NAPDAT(IPCOMP)) THEN
            WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
            WRITE(IUOUT,*)  i, ' parameter linked additional output',
     &                         ' NOT exist'
            STOP ' trnsys_static_temp: unresolvable error'
            END IF

C        Since there is no way to derive the node from additional output,
C        the first node is assumed to be a representative to obtain the ISV
C        value. However, this may be not the case if the nodes have different
C        ISV values. This needs further improvement.
C        Potential source of error
            num_ISV = ISV(IPCOMP, 1)

          ELSE
            WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
            WRITE(IUOUT,*)  i, ' parameter iLnk1 cannot be negative'
            STOP ' trnsys_static_temp: unresolvable error'
          END IF   ! --> IF(iLnk1 .NE. 0)

C If a parameter corresponds to water properties, the sending
C node or the representative node of this component should be water.
C If a parameter corresponds to dry air properties, the sending
C node or the representative node of this component should be air.
C Water vapor needs further consideration in the future

          IF(iCat .EQ. 3 .OR. iCat .EQ. 6) THEN
C         water properties required, so sending node should be water
            IF(num_ISV .NE. 20) THEN
               mistake=.true.
               EXIT
            END IF
          ELSE IF(iCat .EQ. 1 .OR. iCat .EQ. 4) THEN
C         dry air properties required, so sending node should be air
            IF(num_ISV .NE. 21) THEN
               mistake=.true.
               EXIT
            END IF
          ELSE
C         other situations to be considered
          END IF  ! --> IF(iCat .EQ. 3 .OR. iCat .EQ. 6)
        END IF   ! --> IF(iCat .EQ. 8)
      END DO     ! --> DO i=1, MTRNPARV



      IF(mistake)THEN
        WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
        WRITE(IUOUT,*) 'The type of connection ',iCon, ' with node '
        WRITE(IUOUT,*)  iNod, ' is incorrect for comp: ', IPCOMP
        WRITE(IUOUT,*) ' trnsys type: ', nTYPE
        STOP ' trnsys_static_temp: unresolvable error'
      END IF


C---------------------------------
C Check whether the containment of the trnsys component should be defined
C---------------------------------

C To determine whether a trnsys parameter is environment temperature
C environment = false : no parameter is environment temperature
C environment = true : a parameter is environment temperature
      environment = .false.
      DO i=1, MTRNPARV
         iPos = ITRNP_POS(iTRNCOM,i)

C        iPos = -1 indicates the end of changable parameters
         IF(iPos .EQ. -1) EXIT

C        iCat: the changable parameter type, 7 implies environment temp.
         iCat = ITRNP_CAT(iTRNCOM,i)

         IF(iCat .EQ. 7) THEN
            environment = .true.
            EXIT
         END IF
      END DO


C To determine whether a trnsys input is environment temperature
C This check is performed only if environment = false by now
      IF(environment .EQV. .false.) THEN
         DO i=1, NTRNXIN(iTRNCOM)
            iCat = ITRNX_CAT(iTRNCOM,i)

            IF(iCat .EQ. -1) THEN
               environment = .true.
               EXIT
            END IF
         END DO
      END IF

C If a trnsys parameter or input is environment temperature, the
C containment of this trnsys component must be defined; otherwise,
C a value of -99. is assumed by esp-r, causing error.
      IF(environment) THEN
        mistake = .true.
        DO i=1,NCONT
           iPC=IPCC(i)
           IF(iPC .EQ. IPCOMP) THEN
              mistake = .false.
              EXIT
           END IF
        END DO
      END IF

      IF(mistake) THEN
         WRITE(IUOUT,*) ' trnsys_static_temp: incorrect'
         WRITE(IUOUT,*) ' the comp ',IPCOMP, ' must be contained',
     &                 ' within a room.'
         STOP ' trnsys_static_temp: unresolvable error'
      END IF


C The validity of ADATA for trnsys component is checked by trnsys type.
C At the beginning of simulation (TIME=TIME0), trnsys type will check 
C the validity of parameters. We can call trnsys type here
C to check ADATA as other static_template do. However, many operations
C are involved each time calling trnsys types. Thus, we move the
C (TIME=TIME0) call to the coefficient generator, where trnsys is called
C more than once at the beginning of simulation.
C 
      RETURN

      END SUBROUTINE trnsys_static_temp




C********************** trnsys_coeff_gen *****************************
C This subroutine is the coefficient generator for the trnsys plant
C components. It prepares the trnsys parameters, inputs, and
C derivatives; and then it calls the trnsys subroutine and maps the
C trnsys outputs to right-hand coefficients of the component.
C TRNSYS coefficienct matrix has the following significant characteristics:
C 1. its self-coupling coefficients form an entity matrix
C 2. it has no cross-coupling coefficients
C 3. its right-hand coefficients are from trnsys output (or input).


      SUBROUTINE trnsys_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "trnsys.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON /pcddsc/ pcdesc(maxpc), npref(mpcom)
      CHARACTER pcdesc*80
      INTEGER NPREF

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF, HP, HF

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     & ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP
      REAL PERREL,PERTMP,PERFLX,PERMFL
      INTEGER itrclp
      INTEGER ICSV
      REAL CSVI

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     & IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      INTEGER NPMCOE, NPNOD, NPMTYP

      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      INTEGER NDCON, ISV

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP


      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),
     & PCQF(MPNODE),PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),
     & PUAP(MPNODE),PCQP(MPNODE),PCNTMP(MPCOM)

      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP
      REAL PCQP,PCNTMP

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     & ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

C Common block for simulation start day and finish day
      COMMON/simsdy/iSS,iSF
      INTEGER iSS, iSF


      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/pstctr/nsincp,nsncpr
      INTEGER nsincp, nsncpr

C     Common for determining if the plant domain has converged
      COMMON/pltcon/bpltcon,lastTStp
      LOGICAL bpltcon
      REAL lastTStp

      COMMON/pcnam/pcname(mpcom)       ! Plant component names
      CHARACTER*15 pcname

C Please refer to SUBROUTINE TRNPMXT at esruplt/pmatrix.F for the 
C comments about the following TRNSYS commons.
C
C Common for TRNSYS general information
      COMMON/TRNSYSGEN/NTRNCMP,ITRNTYPE(MTRNCOM)
      INTEGER NTRNCMP,ITRNTYPE

C Common for TRNSYS parameters
      COMMON/TRNSYSPAR/NTRNPAR(MTRNCOM),
     &                 ITRNP_POS(MTRNCOM,MTRNPARV),
     &                 ITRNP_CAT(MTRNCOM,MTRNPARV),
     &                 ITRNP_LNK1(MTRNCOM,MTRNPARV),
     &                 ITRNP_LNK2(MTRNCOM,MTRNPARV)
      INTEGER NTRNPAR, ITRNP_POS, ITRNP_CAT,ITRNP_LNK1, ITRNP_LNK2

C Common for TRNSYS inputs
      COMMON/TRNSYSINP/TRNXIN(MTRNCOM, MTRNXIN),
     &                 NTRNXIN(MTRNCOM),
     &                 ITRNX_CAT(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK1(MTRNCOM,MTRNXIN),
     &                 ITRNX_LNK2(MTRNCOM,MTRNXIN)
      DOUBLE PRECISION    TRNXIN
      INTEGER NTRNXIN, ITRNX_CAT,ITRNX_LNK1,ITRNX_LNK2

C Common for TRNSYS outputs
      COMMON/TRNSYSOUT/ITRNO_T_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_PS1_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_PS2_LNK(MTRNCOM,MNODEC),
     &                 ITRNO_ADD_LNK(MTRNCOM,MPCRES),
     &                 TRN_ADD_DESC(MTRNCOM,MPCRES)
      INTEGER ITRNO_T_LNK, ITRNO_PS1_LNK,
     &        ITRNO_PS2_LNK, ITRNO_ADD_LNK
      CHARACTER TRN_ADD_DESC*80

C Common for TRNSYS derivatives
      COMMON/TRNSYSDER/NTRNDER(MTRNCOM),
     &                 TRNDER_SET(MTRNCOM),
     &                 TRNDER_INT(MTRNCOM,MTRNDER) 
      INTEGER NTRNDER
      LOGICAL TRNDER_SET
      DOUBLE PRECISION    TRNDER_INT

C Common for unit conversions between TRNSYS and esp-r
      COMMON/TRNSYSUNT/TRNP_UNT(MTRNCOM,MTRNPARV),
     &                 TRNX_UNT(MTRNCOM,MTRNXIN),
     &                 TRNO_T_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS1_UNT(MTRNCOM,MNODEC),
     &                 TRNO_PS2_UNT(MTRNCOM,MNODEC),
     &                 TRNO_ADD_UNT(MTRNCOM,MPCRES)
      CHARACTER*3  TRNP_UNT,TRNX_UNT,TRNO_T_UNT,
     &             TRNO_PS1_UNT,TRNO_PS2_UNT,TRNO_ADD_UNT

C Common for TRNSYS types
      COMMON/TRNSYSTYPE/INFO_SHORT(MTRNCOM,4)
      INTEGER INFO_SHORT    !store info(6), info(9), info(10) and info(12)

C Common for TRNSYS inputs and outputs
      COMMON/TRNSYSSTORE/outStore(MTRNCOM,MTRNOUT),
     &                   inStore(MTRNCOM,MTRNXIN),
     &                   storageVariable(MTRNCOM,MTRNSTORE)
      DOUBLE PRECISION outStore, inStore, storageVariable

C Common for cross domain loop counters
      common / Cross_Domain_Loop / plt_elec_loop_counter
      integer plt_elec_loop_counter


C Transferred parameters
      INTEGER IPCOMP, ISTATS
      REAL COUT(MPCOE)

C External functions
      REAL SHTFLD
      REAL RHOFLD
      REAL fConvert_current_TS_to_DAY

C Local variables
      INTEGER  nnode, iTRNCOM, nTYPE
      INTEGER  iPos, iCat, iNod, iCon, iOut, net_cndx
      INTEGER  iLnk1, iLnk2
      INTEGER  iCall_State    !the condition to call trnsys type
      SAVE     iCall_State

      INTEGER  i, j, itemp

      REAL     parTemp   ! the temperature on which parameter properties are based
      DOUBLE PRECISION  valueMultiply, valueAdd

      DOUBLE PRECISION  simtime, par(MADATA), xin(MTRNXIN), 
     &                    out(MTRNOUT),Tinit(MTRNDER), dtdt(MTRNDER)
      INTEGER  icntrl, info(15)
      SAVE     info       !info(7) relies on its previous value

      CHARACTER*3   unitCode        ! TRNSYS unit code
      CHARACTER*128 H3K_rep_NAME    ! H3Kreporting object name
      CHARACTER*128 root_name       ! root name of component node 
      CHARACTER*128 hold_name       ! temporary variable for string manipulation

      LOGICAL toTrnsys  !the direction of unit conversion between esp-r and trnsys
                        ! .true. means from esp-r to trnsys
                        ! .false. means from trnsys to esp-r

C Reference
      INTEGER  lnblnk
      DOUBLE PRECISION TRNX_FUN


      icntrl = 0  ! initialization
C                 ! not used. declared just to satisfy the trnsys interface


C Start trace if trace output requested.
      IF(ITC.gt.0 .AND. NSINC.gt.ITC .AND.NSINC.le.ITCF.AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Entering sub trnsys_coeff_gen'
      END IF

C  NPMTYP      - type of plant matrix required for configuration
C                1 = energy balance only
C                2 = mass balance, single phase
C                3 = mass balance, two phase
C
C To determine the value of iCall_State according to NPMTYP
C The value iCall_State needs to be determined only for the first
C time step since the SAVE attribute is used.
      IF(nsincp .EQ. 1) THEN
         IF(NPMTYP .EQ. 1) THEN
C        energy balance only
            iCall_State = 1
         ELSE
C        energy plus mass (1st or 2nd) balance
C        This is because in subroutine MZPMRX, the first phase mass
C        balance is handled first.
            iCall_State = 2
         END IF
      END IF


C  Get current simulation time in hour
C    nsincp: number of plant time-step since start of simulation
C    "nsincp=1" indicates that this is the first plant time-step
C    "bPltcon=true" indicates that this is the first trnsys call in a time step
C
C  For the iterative calls (ie.,not the first call) in the first time step,
C  the simulation time is moved one step further in order to perform 
C  trnsys simulaton. Otherwise, trnsys will perform TIME=TIME0 manipulations.
C
C iss is deducted to avoid potential numerical problems
C The numerical problem might exist if iss is large while TIMSEC is small.
C In this case, two large real values (time0 and time0+TIMSEC/2) with small
C differences (TIMSEC/2) will be compared in trnsys.
      simtime = (fConvert_current_TS_to_DAY() - iss) * 24.d0
      IF(nsincp .EQ. 1 .AND. bPltcon .EQV. .false.) THEN
         simtime = simtime + TIMSEC/3600.
      END IF



C Since it is difficult to judge which node needs iteration,
C all nodes are maked for iteration in terms of temperature.
C
C NPCDAT(IPCOMP,9): To determine the location of the current component's
C nodes within the global plant matrix
      nnode = NPCDAT(IPCOMP,8)
      itemp = NPCDAT(IPCOMP,9)
      DO i=1, nnode
         iNod = itemp+i-1
         ICSV(iNod,1) = 1
         CSVI(iNod,1) = CSVF(iNod,1)
      END DO

C Find the index of the trnsys component and its type
      iTRNCOM = 0      ! initialization
      DO i=1, IPCOMP
         IF(NPREF(i) .EQ. 85) iTRNCOM = iTRNCOM+1
      END DO

      nTYPE = ITRNTYPE(iTRNCOM)


C Define the INFO array for trnsys types
      info(1) = iTRNCOM           ! Unit number
      info(2) = nTYPE             ! Type number
      info(3) = NTRNXIN(iTRNCOM)  ! Number of inputs
      info(4) = NTRNPAR(iTRNCOM)  ! Number of parameters
      info(5) = NTRNDER(iTRNCOM)  ! Number of derivatives

C info(6):number of outputs
C info(6) will be assigned by trnsys type at initialization call
C with info(7) =-1. Same for info(9) and info(10)
      info(6) = INFO_SHORT(iTRNCOM,1)

C info(7): number of iterative calls to a trnsys component in the
C          current time step
C Use bPltcon to judge whether this is the first call in a time step
      IF(bPltcon) THEN
         info(7) = 0          ! First call in a time step
      ELSE
C     Increments info(7) for the first trnsys type only
C     Multiple trnsys types have the same info(7) value in a time step
         if(iTRNCOM .EQ. 1) then
            info(7) = info(7)+1  ! Iterative calls in a time step
         end if
      END IF

C info(8): Total number of calls to a trnsys component
C Does not need to be cared for trnsys coupling. So set an arbitrary
C value equal not equal to -1.
      info(8) = 1
C
C info(9): indicate whether or not the trnsys type depends on the
C         passage of time
C info(10): used in storage management for trnsys 15
      info(9) = INFO_SHORT(iTRNCOM,2)
      info(10) = INFO_SHORT(iTRNCOM,3)

C info(11) indicates the number of discrete control variables
C set as zero in the current stage
C In the future, this number might need to be read from the input file
      info(11) = 0

C info(12): trnsys version for which the type was written.
C info(12) will be assigned by trnsys type at initialization call
C with info(7) =-2. 
      info(12) = INFO_SHORT(iTRNCOM,4)


C info(13): indicates when all trnsys components have converged in
C          the current time step.
C  info(13) = 1: Converged
C  info(13) = 0: Not converged
C Since the program control belongs to esp-r, info(13) is set as 1
C when the plant loop has converged for the current time step.
C If the plant-electrical cross domain iteration exists, info(13)
C   is set as 1 only after both plant and electrical domain converge
C   in the current plant time step.
C======================
C Future work needed
C The building-plant cross domain iteration has not been consiered in
C  the current stage. In this case, more work is required to store
C  the TRNSYS storage variables at the beginning of each building
C  time step and rewind in the following building-plant iterative steps.
C======================
C
      IF(bPltcon .AND. plt_elec_loop_counter .EQ. 1) THEN
          IF(nsincp .EQ. 1 ) THEN
             info(13) = 0
          ELSE
             info(13) = 1
          END IF
      ELSE
        info(13) = 0
      END IF

C info(14) and info(15) are reserved for future use by TRNSYS, so
C they do not need to be cared.


C Map ADATA to trnsys parameters
      DO i=1, MADATA
         par(i) = DBLE(ADATA(IPCOMP, i) )
      END DO

C Update those time-dependent parameters 
      DO i=1, MTRNPARV
         iPos = ITRNP_POS(iTRNCOM,i)

         IF(iPos .EQ. -1) EXIT

         iCat = ITRNP_CAT(iTRNCOM,i)

         IF(iCat .NE. 7) THEN

           iLnk1 = ITRNP_LNK1(iTRNCOM,i)
           iLnk2 = ITRNP_LNK2(iTRNCOM,i)

           IF(iCat .GE. 1 .AND. iCat .LE. 6) THEN
C   A thermal physical paramter that varies with temperature
             IF(iLnk1 .NE. 0) THEN
C        temperature of external connected fluids
              iNod = iLnk1
              iCon = iLnk2
              net_cndx = ICONDX(IPCOMP,iNod,iCon)
              parTemp = CONVAR(net_cndx,1)
             ELSE IF(iLnk1 .EQ. 0) THEN
C        temperature within this component
C        this temperature is specified by the additional output
C        of this trnsys component
              parTemp = PCAOUT(IPCOMP,iLnk2)
             END IF  ! matches--> IF(iLnk1 .NE. 0)
           END IF  ! matches --> IF(iCat .GE. 1 .AND. iCat .LE. 6)
         END IF   ! matches --> IF(iCat .NE. 7)

C   Change the parameter's value according to its type
         IF(iCat .EQ. 1) THEN
C           specific heat of dry air
            par(iPos) = DBLE(SHTFLD(1, parTemp))
         ELSE IF(iCat .EQ. 2) THEN
C           specific heat of water vapour
            par(iPos) = DBLE(SHTFLD(2, parTemp))
         ELSE IF(iCat .EQ. 3) THEN
C           specific heat of water
            par(iPos) = DBLE(SHTFLD(3, parTemp))
         ELSE IF(iCat .EQ. 4) THEN
C           density of dry air
            par(iPos) = DBLE(RHOFLD(1, parTemp))
         ELSE IF(iCat .EQ. 5) THEN
C           density of water vapour
            par(iPos) = DBLE(RHOFLD(2, parTemp))
         ELSE IF(iCat .EQ. 6) THEN
C           density of water
            par(iPos) = DBLE(RHOFLD(3, parTemp))
         ELSE IF(iCat .EQ. 7) THEN
C           environment temperature
            par(iPos) = DBLE(PCNTMF(IPCOMP))
         ELSE IF(iCat .EQ. 8) THEN
C           additional output of a specified component
            par(iPos) = DBLE(PCAOUT(iLnk1,iLnk2))

C New category of time-dependent parameters inserted here

         ELSE
            WRITE(IUOUT,*) ' No mapping defined for parameter type ',
     &                     iCat
            STOP ' trnsys_coeff_gen: unresolvable error on parameters'
         END IF

C        Unit conversion
         unitCode = TRNP_UNT(iTRNCOM,i)
         toTrnsys = .true.
         CALL getConversionFactors(toTrnsys, unitcode,
     &                             valueMultiply, valueAdd)

         par(iPos) = valueMultiply * par(iPos) + valueAdd

      END DO


C Map external connections to trnsys inputs
      DO i=1, NTRNXIN(iTRNCOM)
         iCat = ITRNX_CAT(iTRNCOM,i)

         IF(iCat .EQ. 0) THEN
C        Not linked to connection, use user provided value
C        No unit conversion is involved.
            xin(i) = TRNXIN(iTRNCOM,i)
         ELSE IF(iCat .EQ. -1) THEN
C        linked to environment temperature
            xin(i) = DBLE(PCNTMF(IPCOMP))
         ELSE IF(iCat .EQ. -2) THEN
C        linked to control
            j = ITRNX_LNK1(iTRNCOM,i)  ! get the control ID
            xin(i) = DBLE(CDATA(IPCOMP, j))
         ELSE IF(iCat .EQ. -3) THEN
C        linked to outdoor temperature
            xin(i) = DBLE(TF)
         ELSE IF(iCat .GE. 1 .AND. iCat .LE. 4) THEN
C        linked to a given connection
            iNod = ITRNX_LNK1(iTRNCOM,i)
            iCon = ITRNX_LNK2(iTRNCOM,i)
            net_cndx = ICONDX(IPCOMP,iNod,iCon)
            IF(iCat .EQ. 1) THEN
C           temperature
            xin(i) = DBLE(CONVAR(net_cndx,1))
            ELSE IF (iCat .EQ. 2) THEN
C           1st phase mass flow
            xin(i) = DBLE(PCONDR(net_cndx) * CONVAR(net_cndx,2))
            ELSE IF (iCat .EQ. 3) THEN
C           2nd phas mass flow
            xin(i) = DBLE(PCONDR(net_cndx) * CONVAR(net_cndx,3))
            ELSE   ! (iCat =4)
C           the sum of 1st and 2nd mass flow
            xin(i) = DBLE(PCONDR(net_cndx) * CONVAR(net_cndx,2)
     &                    + PCONDR(net_cndx) * CONVAR(net_cndx,3))
            END IF
         ELSE IF(iCat .EQ. 5) THEN
C        linked to additional ouput
            iLnk1 = ITRNX_LNK1(iTRNCOM,i)   ! component
            iLnk2 = ITRNX_LNK2(iTRNCOM,i)   ! additional output id
            xin(i) = DBLE(PCAOUT(iLnk1,iLnk2))
         ELSE IF(iCat .EQ. 6) THEN
C        linked to a function value
            iLnk1 = ITRNX_LNK1(iTRNCOM,i)
            xin(i) = TRNX_FUN(iTRNCOM, iLnk1)

C New input type inserted here

         ELSE 
            WRITE(IUOUT,*) ' No mapping mechanism defined for ',
     &                     'input type ',iCat
            STOP ' trnsys_coeff_gen: unresolvable error on input'
         END IF    ! --> IF(iCat .EQ. 0) 

C        Unit conversion
         IF(iCat .NE. 0 .AND. iCat .NE. 6) THEN
            unitCode = TRNX_UNT(iTRNCOM,i)
            toTrnsys = .true.
            CALL getConversionFactors(toTrnsys, unitCode,
     &                               valueMultiply, valueAdd)

            xin(i) = valueMultiply * xin(i) + valueAdd
         END IF
      END DO


C Map derivatives and initial values for dependent variables
C    This condition is valid for plant domain only 
C    This condition may be not sufficient if cross-domain iteration exists
      IF(nsincp .EQ. 1 .AND. bPltcon ) THEN
         IF(TRNDER_SET(iTRNCOM) ) THEN
C           user provided initial values
            DO i=1,NTRNDER(iTRNCOM)
               Tinit(i) = TRNDER_INT(iTRNCOM,i)
            END DO
         ELSE
C          Follow esp-r rules to initialize node states
C An assumption made here is that all dependent variables for which
C the derivatives are evaluated are temperatures.
C This assumption may be invalid for some TRNSYS types
C Should be updated in the future
            DO i=1,NTRNDER(iTRNCOM)
               Tinit(i) = 20.d0
            END DO
         END IF
      END IF


C Call trnsys subroutine
C Since trnsys types are capable of producing all outputs related to 
C energy, firt and second phase mass equations, each trnsys subroutine
C should be called only once for a given plant component coefficienct
C call. Otherwise, the trnsys subroutine may be called more than once
C for each plant component coefficient call, mistakely running more
C simulation time steps for trnsys component.
C The above purpose is achieved by applying the following condition
C for trnsys calls:  if (ISTATS .EQ. iCall_State)
C
C  At the beginning of simulation, the trnsys is called four times
C    in total within this subrotuine.
C     1st call: version signing call, info(7) = -2
C     2nd call: initialization call, info(7) = -1
C     3rd call: TIME=TIME0 call, info(7) = 0
C     4th call: real simulation, info(7) = 0
C  For all other time steps, the trnsys is called once within
C    this subroutine.
C
C    The alternate return used by trnsys subroutine aims to detect the 
C    error of unlinked type. Since DLL is not used here, this type of 
C    error will never occur.

      IF(ISTATS .EQ. iCall_State) THEN 
        IF(nTYPE .EQ. 60) THEN         ! stratified water tank
          IF(nsincp .EQ. 1 .AND. bPltcon) THEN
C     Three trnsys type calls are needed prior to real simulation
C     First call: version signing call, info(7) = -2
C     Second call: initialization call, info(7) = -1
C     Third call: TIME=TIME0 call, info(7) = 0
            info(7) = -2
            DO i=1, 3
               CALL type60(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*500)

               CALL edisp(iuout, ' Impossible to come here: type 60')
               STOP ' Error in sub trnsys_coeff_gen'

  500         CONTINUE
C It is usually unnecessary to check trnsys error since trnsys types
C call ErrorFound() function when a message is reported. This is
C especially the case at initialization step.
C            CALL checkTrnsysError(iTRNCOM, nType)
              IF(i .EQ. 1) THEN
C             Version signing call, i.e., info(7) = -2
                 INFO_SHORT(iTRNCOM,4) = info(12)
              ELSE IF(i .EQ. 2) THEN
C             Initialization call, i.e., info(7) = -1
                 INFO_SHORT(iTRNCOM,1) = info(6)
                 INFO_SHORT(iTRNCOM,2) = info(9)
                 INFO_SHORT(iTRNCOM,3) = info(10)
              ELSE IF( i .EQ. 3) THEN
C             TIME = TIME0 manipulations
C           After TIME=TIME0 call, increase simtime to perform 
C           real simulation afterwards.
                 simtime = simtime + TIMSEC/3600.
              END IF

              IF(i .LT. 3) info(7) = info(7) + 1

            END DO   !--> DO i=1, 3
          END IF

          IF(info(13) .EQ. 1) THEN
             CALL type60(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*510)

  510        CONTINUE
             info(13) = 0
             CALL type60(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*1000)
          ELSE
             CALL type60(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*1000)
          END IF

        ELSE IF(nTYPE .EQ. 940) THEN   ! tankless water heater
          IF(nsincp .EQ. 1 .AND. bPltcon) THEN
C     Three trnsys type calls are needed at the beginning of simulation
C     First call: version signing call, info(7) = -2
C     Second call: initialization call, info(7) = -1
C     Third call: TIME=TIME0 call, info(7) = 0
            info(7) = -2
            DO i=1, 3
               CALL type940(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*600)

               CALL edisp(iuout, ' Impossible to come here: type 940')
               STOP ' Error in sub trnsys_coeff_gen'

  600         CONTINUE
C It is usually unnecessary to check trnsys error since trnsys types
C call ErrorFound() function when a message is reported. This is
C especially the case at initialization step.
C            CALL checkTrnsysError(iTRNCOM, nType)
              IF(i .EQ. 1) THEN
C             Version signing call, i.e., info(7) = -2
                 INFO_SHORT(iTRNCOM,4) = info(12)
              ELSE IF(i .EQ. 2) THEN
C             Initialization call, i.e., info(7) = -1
                 INFO_SHORT(iTRNCOM,1) = info(6)
                 INFO_SHORT(iTRNCOM,2) = info(9)
                 INFO_SHORT(iTRNCOM,3) = info(10)
              ELSE IF( i .EQ. 3) THEN
C             TIME = TIME0 manipulations
C           After TIME=TIME0 call, increase simtime to perform 
C           real simulation afterwards.
                 simtime = simtime + TIMSEC/3600.
              END IF

              IF(i .LT. 3) info(7) = info(7) + 1

            END DO   !--> DO i=1, 3
          END IF

          IF(info(13) .EQ. 1) THEN
             CALL type940(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*610)

  610        CONTINUE
             info(13) = 0
             CALL type940(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*1000)
          ELSE
             CALL type940(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*1000)
          END IF

        ELSE IF(nTYPE .EQ. 152) THEN   ! Annex 42 FC-cogeneration wrapper
          IF(nsincp .EQ. 1 .AND. bPltcon) THEN
C     Three trnsys type calls are needed at the beginning of simulation
C     First call: version signing call, info(7) = -2
C     Second call: initialization call, info(7) = -1
C     Third call: TIME=TIME0 call, info(7) = 0
            info(7) = -2
            DO i=1, 3
               CALL type152(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*700)

               CALL edisp(iuout, ' Impossible to come here: type 152')
               STOP ' Error in sub trnsys_coeff_gen'

  700         CONTINUE
C It is usually unnecessary to check trnsys error since trnsys types
C call ErrorFound() function when a message is reported. This is
C especially the case at initialization step.
C            CALL checkTrnsysError(iTRNCOM, nType)
              IF(i .EQ. 1) THEN
C             Version signing call, i.e., info(7) = -2
                 INFO_SHORT(iTRNCOM,4) = info(12)
              ELSE IF(i .EQ. 2) THEN
C             Initialization call, i.e., info(7) = -1
                 INFO_SHORT(iTRNCOM,1) = info(6)
                 INFO_SHORT(iTRNCOM,2) = info(9)
                 INFO_SHORT(iTRNCOM,3) = info(10)
              ELSE IF( i .EQ. 3) THEN
C             TIME = TIME0 manipulations
C           After TIME=TIME0 call, increase simtime to perform 
C           real simulation afterwards.
                 simtime = simtime + TIMSEC/3600.
              END IF

              IF(i .LT. 3) info(7) = info(7) + 1

            END DO   !--> DO i=1, 3
          END IF

          IF(info(13) .EQ. 1) THEN
             CALL type152(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*710)

  710        CONTINUE
             info(13) = 0
             CALL type152(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*1000)
          ELSE
             CALL type152(simtime,xin,out,Tinit,dtdt,par,
     &                            info,icntrl,*1000)
          END IF

C  Calling other trnsys type subroutines inserted here

        ELSE
          WRITE(IUOUT,*) ' No subroutine available for trnysys type '
     &        ,nTYPE
          STOP ' trnsys_coeff_gen: unresolvable error on trnsys sub'
        END IF    !----> match  IF(nTYPE .EQ. 60)

        CALL edisp(IUOUT, ' Impossible to come here: general')
        STOP ' Error in sub trnsys_coeff_gen'

      END IF  !---> match  IF(ISTATS .EQ. iCall_State)


 1000 CONTINUE       ! alternate return from trnsys type


C  Save TRNSYS inputs and outputs in the common block. This operation
C  is essential if multiple TRNSYS componets are used in a plant network.
      IF(ISTATS .EQ. iCall_State) THEN
         DO i=1, INFO_SHORT(iTRNCOM,1)
            outStore(iTRNCOM,i) = out(i)
         END DO

         DO i=1, NTRNXIN(iTRNCOM)
            inStore(iTRNCOM, i) = xin(i)
         END DO
      END IF


C It is usually unnecessary to check trnsys error since trnsys types
C call ErrorFound() function when a message is reported.
C      CALL checkTrnsysError(iTRNCOM, nType)


C Map trnsys outputs to COUT, carried out in two steps:
C     First step: assign 1. to all self-coefficiencts
      DO i=1, nnode
         COUT(i) = 1.
      END DO

C     Second step: Assign trnsys outputs, inputs or zero
C                  to right hand values

      toTrnsys = .false.    !unit conversion from trnsys to esp-r

      IF(ISTATS .EQ. 1) THEN
C     Energy balance
         DO i=1, nnode
            unitCode = TRNO_T_UNT(iTRNCOM,i)
            CALL getConversionFactors(toTrnsys, unitCode,
     &                               valueMultiply, valueAdd)

            iOut = ITRNO_T_LNK(iTRNCOM,i)
            COUT(nnode+i) = REAL(outStore(iTRNCOM,iOut)
     &                         * valueMultiply + valueAdd)
         END DO
      ELSE IF(ISTATS .EQ. 2) THEN
C     First phase mass balance
         DO i=1, nnode
            unitCode = TRNO_PS1_UNT(iTRNCOM,i)
            CALL getConversionFactors(toTrnsys, unitCode,
     &                               valueMultiply, valueAdd)

            iOut = ITRNO_PS1_LNK(iTRNCOM,i)
            if(iOut .GT. 0) then         ! map to trnsys output
               COUT(nnode+i) = REAL(outStore(iTRNCOM,iOut)
     &                             * valueMultiply + valueAdd)
            else if(iOut .LT. 0) then    ! map to trnsys input
               COUT(nnode+i) = REAL(inStore(iTRNCOM,-iOut)
     &                              * valueMultiply + valueAdd)
            else                         ! map to zero
               COUT(nnode+i) = 0.
            end if
         END DO
      ELSE IF(ISTATS .EQ. 3) THEN
C     Second phase mass balance
         DO i=1, nnode
            unitCode = TRNO_PS2_UNT(iTRNCOM,i)
            CALL getConversionFactors(toTrnsys, unitCode,
     &                               valueMultiply, valueAdd)

            iOut = ITRNO_PS2_LNK(iTRNCOM,i)
            if(iOut .GT. 0) then         ! map to trnsys output
               COUT(nnode+i) = REAL(outStore(iTRNCOM,iOut)
     &                              * valueMultiply + valueAdd)
            else if(iOut .LT. 0) then    ! map to trnsys input
               COUT(nnode+i) = REAL(inStore(iTRNCOM,-iOut)
     &                              * valueMultiply + valueAdd)
            else                         ! map to zero
               COUT(nnode+i) = 0.
            end if
         END DO
      END IF     ! ---> IF(ISTATS .EQ. 1)


C Map trnsys outputs to PCAOUT
C    Unit conversion is not carried out purposely here to 
C    facilitate future validation work
      DO i=1, NAPDAT(IPCOMP)
         iOut = ITRNO_ADD_LNK(iTRNCOM,i)
         PCAOUT(IPCOMP,i) = REAL(outStore(iTRNCOM,iOut))
      END DO



C Complete trace if trace output requested.
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP
        WRITE(ITU,*) ' TRNSYS TYPE    ',nTYPE,':'
        IF(ISTATS .eq. 1) THEN
           WRITE(ITU,*) '--- Node Temperature ---'
           DO i=1, nnode
              WRITE(ITU,*) 'Node ',i,' Temp (p) = ',CSVP(i,1),' (oC)'
              WRITE(ITU,*) 'Node ',i,' Temp (f) = ',CSVF(i,1),' (oC)'
           END DO

           WRITE(ITU,*) '--- Connection Temperature ---'
           DO i=1, nnode
             WRITE(ITU,*) ' Node ', i, ' Linked Connections below.'
             DO j=1, NDCON(IPCOMP, i)
               iCon = ICONDX(IPCOMP, i, j)
               WRITE(ITU,*) 'Conn ',j,' Temp(p) = ',PCTP(iCon),' (oC)'
             END DO
           END DO

           WRITE(ITU,*) '--- Additional Output ---'
           DO i=1, NAPDAT(IPCOMP)
              WRITE(ITU,*) TRN_ADD_DESC(iTRNCOM,i),' = ',
     &                    PCAOUT(IPCOMP,i), TRNO_ADD_UNT(iTRNCOM,i)
           END DO
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(i),i=1,2*nnode)

        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 4 lines at a time.
        END IF

        WRITE(ITU,*) ' Leaving sub trnsys_coeff_gen'
      END IF


C XML output
C     Get component name
      hold_name = pcname(IPCOMP) 
C     Format string as: 'plant/NAME/misc_data'

      WRITE(root_name,'(A,A,A)') 
     &     'plant/',
     &     hold_name(1:lnblnk(hold_name)),
     &     '/misc_data'

      DO i=1, NAPDAT(IPCOMP)
         WRITE(H3K_rep_NAME,'(A,A,A)') 
     &     root_name(1:lnblnk(root_name)), '/',TRN_ADD_DESC(iTRNCOM,i)

         CALL add_to_xml_reporting(
     &                   PCAOUT(IPCOMP,i),
     &                   H3K_rep_name,
     &                   'units',
     &                   TRNO_ADD_UNT(iTRNCOM,i),
     &                   TRN_ADD_DESC(iTRNCOM,i) )
      END DO


      RETURN

      END SUBROUTINE trnsys_coeff_gen




C********************* getConversionFactors****************************

C This subroutine gets the unit conversion factors based on unitCode.
C Note that unitCode is trnsys-oriented, which means that the user-
C indicated unitCodes are for trnsys parameters, inputs or outputs.
C Since the mapping between esp-r and trnsys is bidirectional, the 
C unit conversion can be either from esp-r to trnsys or the other way.
C The direction is indicated by the logical variable "toTrnsys".
C
C The unitCodes come from trnsys manual vol.8 (page 79-82)
C Not all possible unitCodes are implemented here. If necessary,
C the code can be easily expanded to incorporate other unimplemented
C cases.
C
C Implemented unit codes and their meanings are listed below:
C Temperature
C   TE1   oC
C   TE2   F
C   TE3   K
C Density
C   DN1   kg/m^3
C   DN2   kg/L
C Energy
C   EN1   kJ
C   EN2   kWh
C Power
C   PW1   kJ/hr
C   PW2   W
C   PW3   kW
C Specific heat
C   CP1   kJ/kg-K
C Mass flow rate
C   MF1   kg/hr
C   MF2   kg/s
C Miscellaneous
C   DM1   dimensionless


      SUBROUTINE getConversionFactors(toTrnsys, unitCode,
     &                               valueMultiply, valueAdd)
      IMPLICIT NONE


      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C     Transferred parameters
      LOGICAL toTrnsys   !true: converting units from esp-r to trnsys
C                         false: converting units from trnsys to esp-r
      CHARACTER*3       unitCode
      DOUBLE PRECISION  valueMultiply, valueAdd

C     Local variables
      CHARACTER*2    unitName
      CHARACTER*1    codeIndex
      CHARACTER*124  MSG

      unitName = unitCode(1:2)
      codeIndex = unitCode(3:3)

      IF(toTrnsys) THEN
C    converting units from esp-r to trnsys
         IF(unitName .EQ. 'TE' .OR. unitName .EQ. 'te') THEN
C        units of temperature
            if(codeIndex .EQ. '1') then       ! oC
               valueMultiply =  1.d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! F
               valueMultiply = 1.8d0
               valueAdd = 32.d0
            else if(codeIndex .EQ. '3') then  ! K
               valueMultiply = 1.d0
               valueAdd = 273.15d0
            else
               MSG = codeIndex // ' temperature conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'DN' .OR. unitName .EQ. 'dn') THEN
C        units of density
            if(codeIndex .EQ. '1') then       ! kg/m^3
               valueMultiply =  1.d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! kg/L
               valueMultiply = 1.d-3
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' density conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'EN' .OR. unitName .EQ. 'en') THEN
C        units of energy
            if(codeIndex .EQ. '1') then       ! kJ
               valueMultiply =  1.d-3
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! kWh
               valueMultiply = 2.77778d-7
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' energy conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'PW' .OR. unitName .EQ. 'pw') THEN
C        units of power
            if(codeIndex .EQ. '1') then       ! kJ/hr
               valueMultiply =  3.6d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! W
               valueMultiply = 1.d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '3') then  ! kW
               valueMultiply = 1.d-3
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' power conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'CP' .OR. unitName .EQ. 'cp') THEN
C        units of specific heat
            if(codeIndex .EQ. '1') then       ! kJ/kg-K
               valueMultiply =  1.d-3
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' specific heat conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'MF' .OR. unitName .EQ. 'mf') THEN
C        units of mass flow rate
            if(codeIndex .EQ. '1') then       ! kg/hr
               valueMultiply =  3.6d3
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! kg/s
               valueMultiply =  1.d0
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' mass flow rate undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'DM' .OR. unitName .EQ. 'dm') THEN
C        dimensionless 
            valueMultiply =  1.d0
            valueAdd = 0.d0
            RETURN
         END IF

C       Undefined units
         MSG = unitName // ' is an undefined unit'
         CALL edisp(IUOUT,MSG)
         GOTO 1000
      END IF   !---> match   IF(toTrnsys)


      IF(toTrnsys .EQV. .false.) THEN
C    converting units from trnsys to esp-r
         IF(unitName .EQ. 'TE' .OR. unitName .EQ. 'te') THEN
C        units of temperature
            if(codeIndex .EQ. '1') then       ! oC
               valueMultiply =  1.d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! F
               valueMultiply = 5.5555556d-1
               valueAdd = -1.77777778d1
            else if(codeIndex .EQ. '3') then  ! K
               valueMultiply = 1.d0
               valueAdd = -273.15d0
            else
               MSG = codeIndex // ' temperature conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'DN' .OR. unitName .EQ. 'dn') THEN
C        units of density
            if(codeIndex .EQ. '1') then       ! kg/m^3
               valueMultiply =  1.d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! kg/L
               valueMultiply = 1.d3
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' density conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'EN' .OR. unitName .EQ. 'en') THEN
C        units of energy
            if(codeIndex .EQ. '1') then       ! kJ
               valueMultiply =  1.d3
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! kWh
               valueMultiply = 3.6d6
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' energy conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'PW' .OR. unitName .EQ. 'pw') THEN
C        units of power
            if(codeIndex .EQ. '1') then       ! kJ/hr
               valueMultiply =  2.777777778d-1
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! W
               valueMultiply = 1.d0
               valueAdd = 0.d0
            else if(codeIndex .EQ. '3') then  ! kW
               valueMultiply = 1.d3
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' power conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'CP' .OR. unitName .EQ. 'cp') THEN
C        units of specific heat
            if(codeIndex .EQ. '1') then       ! kJ/kg-K
               valueMultiply =  1.d3
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' specific heat conversion undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'MF' .OR. unitName .EQ. 'mf') THEN
C        units of mass flow rate
            if(codeIndex .EQ. '1') then       ! kg/hr
               valueMultiply =  2.777777778d-4
               valueAdd = 0.d0
            else if(codeIndex .EQ. '2') then  ! kg/s
               valueMultiply =  1.d0
               valueAdd = 0.d0
            else
               MSG = codeIndex // ' mass flow rate undefined'
               CALL edisp(IUOUT,MSG)
               GOTO 1000
            end if

            RETURN
         END IF

         IF(unitName .EQ. 'DM' .OR. unitName .EQ. 'dm') THEN
C        dimensionless 
            valueMultiply =  1.d0
            valueAdd = 0.d0
            RETURN
         END IF


C       Undefined units
         MSG = unitName // ' is an undefined unit'
         CALL edisp(IUOUT,MSG)
         GOTO 1000
      END IF   !---> match   IF(toTrnsys .EQV. .false.)

C Error handling.
 1000 CONTINUE
      STOP ' Error in sub getConversionFactors.'

      END SUBROUTINE getConversionFactors





C*********************** checkTrnsysError********************************
C
C This subroutine checks whether trnsys error has been generated.


      SUBROUTINE checkTrnsysError(iUnit, iType)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Transferred parameters
      INTEGER  iUnit, iType

C External functions
      LOGICAL  ErrorFound

      IF(ErrorFound()) THEN
         WRITE(IUOUT,*) ' Error Generated by Unit or Component : ',
     &               iUnit, ', which is TRNSYS type : ', iType
         STOP ' Check error in sub trnsys_coeff_gen'
      END IF

      END SUBROUTINE checkTrnsysError



