C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C ************************************************************************************
C This file conrtains the main routines for the solution of the power flow network
C and also the routines necessary for the setting up and processing of the solution
C matrix.
C The routines contained in the file are as follows:
C ENETSOLV - The supervisory routine.
C ENETMATX - The matrix solution routine.
C ENETGAUS - A simple Gauss-Siedel solver for the network (used for testing main
C            NR solver modifications).
C EJMTXSET - Sets up the Jacobian matrix for the solution of the network.
C ENETNEWR - Performs the Newton-Raphson solution on the electrical network.
C ENETCOVG - Checks if the electrical network solution has converged.
C ENETSTPH - This routine accelerates the convergence of the Newton-Raphson solution.
C                              **<<to be added>>**
C ************************************************************************************
C Subroutine ENETSOLV is the main supervisory routine for the power flow simulation
C The routine sets up the boundary conditions for the current network, makes
C control-related adjustments to the electrical network, solves the network,
C post-processes and stores the results.

      SUBROUTINE ENETSOLV
#include "CETC_definitions.h"


C Setting up the boundary conditions and network state.
C  Firstly, perform control actions based on the previous time step solution.
C     CALL ENETCTLF

C Calculate the output of the appropriate power-only component models for this
C time step.
C     CALL ENETMODL

C C H2-dep_controller: start
C The residential hydrogen system controller is experimental.
C This code should be used with caution, and has been disabled
C in the standard version of ESP-r.
C Call the residential hydrogen cogen controller routine.
      CALL RESH2_Ctl
C H2-dep_controller: end


C Assign the boundary conditions for this timestep and convert to per-unit.
      CALL ENETASGN
      CALL ENETB2PU

C Set up and solve the matrix for the electrical network at the the current timestep.
      CALL ENETMATX

C Process the raw results and calculate derived parameters.
      CALL ENETPOST

C Some models, such as a battery, may require time-row transport of state-variables.
C Use procedure PFS_TimeRow_Manipulate() to peform these tasks.
      CALL PFS_TimeRow_Manipulate ( iWinding )

C End of electrical network solution
      RETURN
      END

C *************************************************************************************
C Subroutine ENET_RESULTS calls post-processing procedures for the electrical network.
C These procedures were previously called from  ENETSOLV, but the approprate calls
C have been relocated here to permit the electical network to be processed on
C successive iterations during a single timestep.

      SUBROUTINE ENET_RESULTS

C Save electrical network solution and derived parameters to the results library.
      CALL ENETLIBSV


      RETURN
      END


C ************************************************************************************
C Subroutine ENETMATX sets up and solves the electrical network solution matrix based
C on the solution parameters specified in ENETPARAM. Currently two solution routes
C are available: the default Newton-Raphson solution and the simpler but slower
C Gauss-Siedel solution (used mainly as a reference solver).

      SUBROUTINE ENETMATX
#include "power.h"
#include "building.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network solution parameters.
      COMMON/ENETSIMP/ENETMAXI,ENETSLVT,ENETVCVG,ENETACVG,ENETVLIM,
     &ENODCVGF(MENOD),CVGCHK

      COMMON/OUTIN/IUOUT,IUIN

      INTEGER ENETMAXI,ENETSLVT,ENODCVGF,IENTER,NENOD,
     &ENODNO,ENODPHTYP,ENODPH,ENODTYP

      REAL ENETVCVG,ENETACVG,ENETVLIM,ENODBASEV,ENODBASEP

      LOGICAL CVGCHK

      CHARACTER*124 OUTS

C Set the iteration counter
      IENTER=1

C The start of the iterative solution loop
 10   CONTINUE

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0)THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Subroutine ENETMATX'
        WRITE(ITU,*)' ITERATION .. ',IENTER
      ENDIF

C Process the electrical network solution matrix based on the solver chosen
C in ENETPARAM.

C Set up the Gauss-Siedel solution.
      IF(ENETSLVT.EQ.1)THEN
        CALL ENETGAUS


C Set up the Newton-Raphson solution.
      ELSEIF(ENETSLVT.EQ.2)THEN

C Set up the matrix for the Newton-Raphson solution.
        CALL EJMTXSET
        CALL ENETNEWR('N')

      ELSEIF(ENETSLVT.EQ.3)THEN

C Set up the matrix for the Newton-Raphson solution.
        CALL EJMTXSET
        CALL ENETNEWR('P')

      ELSEIF(ENETSLVT.EQ.4)THEN

C Set up the matrix for the decoupled Newton-Raphson solution.
C        CALL EDJMTXSET
C        CALL ENETNEWR('D')

      ENDIF

C Check for convergence
      CALL ENETCOVG

      IF(CVGCHK) THEN

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0)THEN
        WRITE(ITU,*)' Converged on a solution '
      ENDIF

C Terminate the solution process.
        RETURN
      ELSEIF(.NOT.CVGCHK.AND.IENTER.LE.ENETMAXI)THEN

C Continue the solution process after incrementing the simulation counter.
        IENTER=IENTER+1
        GOTO 10

C Issue a warning message if convergence has failed and the maximum allowable
C iterations has been exceeded.

      ELSEIF(.NOT.CVGCHK.AND.IENTER.GT.ENETMAXI)THEN
        IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0)THEN
          WRITE(ITU,*)' No solution in specified no of iterations! '
        ENDIF
        WRITE(OUTS,'(A)')
     &  'WARNING: The electrical network solution has exceeded the '
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A)')
     &  'maximum number of iterations. Please check for errors in the '
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A)')
     &  'network. If no errors are found relax the solution parameters'
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A)')
     &'i.e. increase the maximum number of iterations and the votage '
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A)')
     &'and power flow convergence criteria. '
        CALL EDISP(IUOUT,OUTS)

C Loop through the nodes and check whick are close to convergence and which
C are not.
        DO 30 INOD=1,NENOD
          IF(ENODCVGF(INOD).NE.1)THEN
            WRITE(OUTS,'(A)')
     &'The following electrical nodes have not converged: '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(A,I3)') 'Node ',INOD
            CALL EDISP(IUOUT,OUTS)
          ENDIF
  30    CONTINUE
      ENDIF

      RETURN
      END

C ************************************************************************************
C Subroutine EJMTXSET, this routine sets up the Jacobian matrix for the solution
C of the electrical network. The solution is as follows, and depends on the
C network type.

C d.c. - only network
C      [dP/dV][P]=[dV]
C
C all other networks
C      [ dP/dV | dP/dA ] [dV]    [SUM P]
C      [ --------------] [  ] = -[ --- ]
C      [ dQ/dV | dQ/dA ] [dA]    [SUM Q]
C
C Variables introduces in this routine are:
C JACOBEL - an element of the jacobian matrix
C EMTXZ   - a RHS element of the matrix equation
C EDELTA  - the change in solution variables between iterations
C VOLTPUP - the voltage of each node in the network, previous iteration (per-unit)
C VOLTPUF - the voltage of each node in the network, current iteration (per-unit)
C ************************************************************************************

      SUBROUTINE EJMTXSET

#include "power.h"
#include "building.h"
      
      integer lnblnk  ! function definition

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical network
      COMMON/ENET/ENTYPE

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The per-unit nodal boundary conditions
      COMMON/ENODBCPU/PENODPU(MENOD),QENODPU(MENOD)

C The array of driving port and transfer admittances
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

C The electrical network jacobian solution matrix
      COMMON/ENETMTRX/JACOBEL(MJROW,MJCOL),EMATJZ(MJROW),
     &EDELTA(MJROW)


C The PER-UNIT electrical network state variables used in the iteration
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

      COMPLEX VOLTPUP,VOLTPUF,YADM,SUMVY,SENODPU
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,ENTYPE,NYELM
      REAL ENODBASEV,ENODBASEP,PENODPU,QENODPU
      DOUBLE PRECISION JACOBEL,EMATJZ,EDELTA

      CHARACTER OUTS*124
      PARAMETER (SMALL=1E-10)


C Set up the matrix dimensions, this is equal to 2X the number of network
C nodes. Note initialisation of the electrical network matrix elements is
C done in ENETSINI

      IF(ENTYPE.NE.1) THEN

C Fill up the elements for an a.c.,mixed a.c./d.c. solution.
C The size of the required Jacobian matrix is (2 NENOD) x (2 NENOD)
C elements, the matrix elements are of consists of four characteristic types:
C in the upper left area of the matrix the partial derivatives are dP/d0,
C in the upper right dP/dV, in the lower left dQ/d0, lower right dQ/dV.
        NJROW=2*NENOD
        NJCOL=2*NENOD
      ELSE

C Fill up the elements for a d.c. only-solution.
C The size of the required Jacobian matrix is (NENOD) x (NENOD)
C elements, the matrix elements are of one characteristic type: dP/dV
        NJROW=NENOD
        NJCOL=NENOD
      ENDIF

C Set up the coefficients of the jacobian matrix
      DO 10 IROW=1,NJROW
        DO 20 ICOL=1,NJCOL
          IF(ENTYPE.NE.1) THEN

C Formulations for a.c. and mixed network types.

C ** Upper left elements **
            IF(IROW.LE.NENOD.AND.ICOL.LE.NENOD)THEN

C Upper left - diagonal elements dPi/d0i
              IF(IROW.EQ.ICOL)THEN

C Reset the summation variable.
                SUMVY=CMPLX(0.0,0.0)

C Diagonal elements.
                DO 101 K=1,NENOD
                  IF(K.NE.IROW)
     &SUMVY=SUMVY+(VOLTPUP(IROW)*CONJG(VOLTPUP(K)*YADM(IROW,K)))
  101           CONTINUE
              JACOBEL(IROW,ICOL)=DBLE(-AIMAG(SUMVY))

C Upper left off-diagonal elements dPi/d0j
              ELSE
                JACOBEL(IROW,ICOL)=
     &DBLE(AIMAG(VOLTPUP(IROW)*CONJG(VOLTPUP(ICOL)*YADM(IROW,ICOL))))
              ENDIF

C ** Upper right elements **
            ELSEIF(IROW.LE.NENOD.AND.ICOL.GT.NENOD)THEN

C Upper right - diagonal elements dPi/dVi
              ICOL2=ICOL-NENOD
              IF(IROW.EQ.ICOL2)THEN

C Reset the summation variable.
                SUMVY=CMPLX(0.0,0.0)
                DO 111 K=1,NENOD
                  SUMVY=SUMVY+
     &(VOLTPUP(IROW)*CONJG(VOLTPUP(K)*YADM(IROW,K))/CABS(VOLTPUP(IROW)))
  111           CONTINUE
                JACOBEL(IROW,ICOL)=
     &DBLE(CABS(VOLTPUP(IROW))*REAL(YADM(IROW,IROW))+REAL(SUMVY))

C Upper right off-diagonal elements dPi/dVj
              ELSE
                JACOBEL(IROW,ICOL)=
     &DBLE(REAL(VOLTPUP(IROW)*CONJG(VOLTPUP(ICOL2)*YADM(IROW,ICOL2))/
     &          CABS(VOLTPUP(ICOL2))))
              ENDIF

C ** Lower left elements **
            ELSEIF(IROW.GT.NENOD.AND.ICOL.LE.NENOD)THEN
              IROW2=IROW-NENOD

C Lower left - diagonal elements dQi/d0i
              IF(IROW2.EQ.ICOL)THEN

C Reset the summation variable.
                SUMVY=CMPLX(0.0,0.0)
                DO 121 K=1,NENOD
                  IF(IROW2.NE.K) THEN
                     SUMVY=SUMVY+
     &               VOLTPUP(IROW2)*CONJG(VOLTPUP(K)*YADM(IROW2,K))
                  ENDIF
  121           CONTINUE
                JACOBEL(IROW,ICOL)=DBLE(REAL(SUMVY))

C Lower left off-diagonal elements dQi/d0j
              ELSE
                JACOBEL(IROW,ICOL)=
     &DBLE(-REAL(VOLTPUP(IROW2)*CONJG(VOLTPUP(ICOL)*YADM(IROW2,ICOL))))
              ENDIF

C ** Lower right elements ** CHECK
            ELSEIF(IROW.GT.NENOD.AND.ICOL.GT.NENOD)THEN
              IROW2=IROW-NENOD
              ICOL2=ICOL-NENOD

C Lower right - diagonal elements dQi/dVi
              IF(IROW2.EQ.ICOL2)THEN

C Reset the summation variable.
                SUMVY=CMPLX(0.0,0.0)
                DO 131 K=1,NENOD
                  SUMVY=SUMVY+
     &VOLTPUP(IROW2)*CONJG(VOLTPUP(K)*YADM(IROW2,K))/
     &CABS(VOLTPUP(IROW2))
 131            CONTINUE
                JACOBEL(IROW,ICOL)=
     &DBLE(CABS(VOLTPUP(IROW2))*AIMAG(CONJG(YADM(IROW2,IROW2)))
     &+AIMAG(SUMVY))

C Lower right off-diagonal elements dQi/dVj
              ELSE
                JACOBEL(IROW,ICOL)=
     &DBLE(AIMAG(VOLTPUP(IROW2)*CONJG(VOLTPUP(ICOL2)*YADM(IROW2,ICOL2))/
     &CABS(VOLTPUP(ICOL2))))
              ENDIF
            ENDIF
          ELSE

C Formulations for d.c. and mixed network types.

C All elements dP/dV

C Upper right - diagonal elements dPi/dVi
            IF(IROW.EQ.ICOL)THEN

C Reset the summation variable.
              SUMDC=0.0
              DO 141 K=1,NENOD
                SUMDC=SUMDC+
     &REAL(VOLTPUP(IROW)*(VOLTPUP(K)*YADM(IROW,K))/
     &REAL(VOLTPUP(IROW)))
  141         CONTINUE
              JACOBEL(IROW,ICOL)=
     &DBLE(REAL(VOLTPUP(IROW))*REAL(YADM(IROW,IROW))+SUMDC)

C Upper right off-diagonal elements dPi/dVj
            ELSE
              JACOBEL(IROW,ICOL)=
     &DBLE(REAL(VOLTPUP(IROW)*(REAL(VOLTPUP(ICOL))*
     &REAL(YADM(IROW,ICOL)))/REAL(VOLTPUP(IROW))))
            ENDIF
          ENDIF


   20   CONTINUE
   10 CONTINUE

C Formulate the RHS of the power flow equation (the power flow residuals at each node).
C Calculated using the previous iteration voltage values.
      IF(ENTYPE.NE.1)THEN
        DO 30 IZ=1,NENOD
          SUMVY=CMPLX(0.0,0.0)
          DO 40 JZ=1,NENOD
            SUMVY=SUMVY+VOLTPUP(IZ)*CONJG(VOLTPUP(JZ)*YADM(IZ,JZ))
   40     CONTINUE
          SENODPU=CMPLX(0.0,0.0)
          SENODPU=CMPLX(PENODPU(IZ),QENODPU(IZ))
          EMATJZ(IZ)=DBLE(REAL(SENODPU-SUMVY))
          EMATJZ(IZ+NENOD)=DBLE(AIMAG(SENODPU-SUMVY))
   30   CONTINUE
      ELSE
        DO 50 IZ=1,NENOD
          SUMVY=CMPLX(0.0,0.0)
          DO 60 JZ=1,NENOD
            SUMVY=SUMVY+VOLTPUP(IZ)*CONJG(VOLTPUP(JZ)*YADM(IZ,JZ))
   60     CONTINUE
          EMATJZ(IZ)=DBLE(PENODPU(IZ)-REAL(SUMVY))
   50   CONTINUE
      ENDIF


C Trace
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*) ' '
        WRITE(ITU,*) 'Subroutine EJMTXSET'
        WRITE(ITU,*) 'Matrix Size: ',2*NENOD,' by ',2*NENOD
        WRITE(ITU,*) '  '
        WRITE(ITU,*) 'Jacobian matrix coefficients: '
        DO 70 IROW=1,NJROW
          DO 80 ICOL=1,NJCOL
            WRITE(ITU,*) 'Coefficient ',IROW,',',ICOL,JACOBEL(IROW,ICOL)
  80      CONTINUE
  70    CONTINUE
        WRITE(ITU,*) 'Residual coefficients: '
        DO 90 IROW=1,NJROW
          WRITE(ITU,*) 'Coefficient ',IROW,EMATJZ(IROW)
  90    CONTINUE
        WRITE(ITU,*)

C Write out matrix pattern if NENOD<=30 i.e. the matrix pattern will fit into
C the OUTS string.
        IF(NENOD.LE.30) THEN
          DO 100 IROW=1,NJROW
            OUTS=' '
            ISTRP=1
            DO 110 ICOL=1,NJCOL
              IF(ABS(SNGL(JACOBEL(IROW,ICOL))).GT.SMALL)THEN
                WRITE(OUTS(ISTRP:ISTRP+1),'(A2)')'X '
              ELSE
                WRITE(OUTS(ISTRP:ISTRP+1),'(A2)')'. '
              ENDIF
              ISTRP=ISTRP+2
              IF(ICOL.EQ.NJCOL/2) THEN
                WRITE(OUTS(ISTRP:ISTRP),'(A1)')' '
                ISTRP=ISTRP+1
              ENDIF
 110        CONTINUE
            WRITE(ITU,'(A)')OUTS(1:LNBLNK(OUTS))
            IF(IROW.EQ.NJROW/2) WRITE(ITU,'(A)')' '
 100      CONTINUE
        ENDIF
      ENDIF

      RETURN
      END

C ************************************************************************************
C Solution Routines: ENETNEWR and ENETGAUS. Thses two routines are the basic solution
C algorithms for the electrical network. In each the fundamental state variable of the
C electrical network (complex voltage) is calculated. All the calculations are done in
C per unit.
C
C ENETNEWR uses the same matrix solution routines as the flow network. These are
C contained in MFMACH.F
C ************************************************************************************
C Subroutine ENETGAUS is the Gauss-Siedel algorithm for the solution of electrical
C network power flow.

      SUBROUTINE ENETGAUS

#include "power.h"
#include "building.h"
#include "plant.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS


C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The per-unit nodal boundary conditions
      COMMON/ENODBCPU/PENODPU(MENOD),QENODPU(MENOD)

C The array of driving port and transfer admittances
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

C The PER-UNIT electrical network state variables used in the iteration
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

      COMMON/ENETSIMP/ENETMAXI,ENETSLVT,ENETVCVG,ENETACVG,ENETVLIM,
     &ENODCVGF(MENOD),CVGCHK

      REAL PENODPU,QENODPU,ENETVLIMPU,ENODBASEV,ENODBASEP,ENETVCVG,
     &ENETACVG,ENETVLIM
      COMPLEX VOLTPUP,VOLTPUF,YADM,ENODAP,ENODI
      INTEGER NYELM,NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,
     &ENETSLVT,ENODCVGF,ENETMAXI
      LOGICAL CLOSE,CVGCHK
      CHARACTER*124 OUTS

C Hybrid components that calculate nodal voltages
      COMMON/calcV_LOC/iplant_calcV_LOC(MENOD),iPV_calcV_LOC(MENOD)

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)

C Plant component electrical data
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

C Typecasting
      COMPLEX VSPM
      REAL PSPM,QSPM,PFP,PWRP,BVOLTP
      INTEGER iplant_calcV_LOC,iPV_calcV_LOC,NPEL,IPFP,IPHP


C Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine ENETGAUS'
      ENDIF

C Set the temporary variables
      ENODAP=CMPLX(0.0,0.0)
      ENODI=CMPLX(0.0,0.0)

      DO 10 INOD=1,NENOD

C Formulate the apparent boundary power flow associated with the node.
        ENODAP=CMPLX(PENODPU(INOD),QENODPU(INOD))

C Check for division by zero - fatal error.
        CALL ECLOSE(CABS(YADM(INOD,INOD)),0.0,1E-6,CLOSE)
        IF(CLOSE)THEN
          CALL EDISP(IUOUT,'Fatal error: ENETGAUS division by zero')
          WRITE(OUTS,*) 'Node ',INOD
          CALL EDISP(IUOUT,OUTS)
          CALL EPWAIT
          STOP
        ENDIF

C Calculate the new nodal voltage voltage (if variable voltage node).
        IF(ENODTYP(INOD).EQ.1)THEN
          DO 20 JNOD=1,NENOD
            IF(JNOD.NE.INOD)THEN
              ENODI=ENODI+YADM(INOD,JNOD)*VOLTPUP(JNOD)/YADM(INOD,INOD)
            ENDIF
  20      CONTINUE
          VOLTPUF(INOD)=
     &-ENODI+(CONJG(ENODAP)/(YADM(INOD,INOD))/CONJG(VOLTPUP(INOD)))

C Trace.
          IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
            WRITE(ITU,*)' '
            WRITE(ITU,*)'Per Unit voltage solution for node: ',INOD
            WRITE(ITU,*)'Previous iteration pu voltage k ',VOLTPUP(INOD)
            WRITE(ITU,*)'This iteration pu voltage k+1   ',VOLTPUF(INOD)
            WRITE(ITU,*)'Voltage change                  ',
     &VOLTPUF(INOD)-VOLTPUP(INOD)
          ENDIF

C  Check the maximum change in the voltage has not been exceeded, if so
C  then limit the voltage change to the specified value.
          ENETVLIMPU=ENETVLIM/ENODBASEV(MENOD)
          IF(CABS(VOLTPUF(INOD)-VOLTPUP(INOD)).GT.ENETVLIMPU) THEN
            VRAT=ENETVLIMPU/CABS(VOLTPUF(INOD))
            DREAL=REAL(VOLTPUF(INOD)-VOLTPUP(INOD))*VRAT
            DIMAG=AIMAG(VOLTPUF(INOD)-VOLTPUP(INOD))*VRAT
            VOLTPUF(INOD)=VOLTPUP(INOD)+CMPLX(DREAL,DIMAG)

C Trace.
            IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
              WRITE(ITU,*)' '
              WRITE(ITU,*)'Node ', INOD,' voltage change limit applied.'
              WRITE(ITU,*)'This iteration pu voltage k+1 ',VOLTPUF(INOD)
            ENDIF
          ENDIF

C If the node is of fixed voltage then maintain that voltage.
        ELSEIF(ENODTYP(INOD).EQ.2)THEN
          VOLTPUF(INOD)=VOLTPUP(INOD)

C If the node's voltage is set by a special material in the building domain
C (e.g. PV) then impose the voltage calculated by the building domain.
        ELSEIF(ENODTYP(INOD).EQ.3)THEN
          VOLTPUF(INOD) = VSPM( iPV_calcV_LOC(INOD) ) / ENODBASEV(INOD)

C If the node's voltage is set by a plant component in the plant domain
C then impose the voltage calculated by the plant domain.
        ELSEIF(ENODTYP(INOD).EQ.4)THEN
          VOLTPUF(INOD) = CMPLX( BVOLTP(iplant_calcV_LOC(INOD))
     &                           /ENODBASEV(INOD) , 0. )

C All nodal types have been examined.
        ENDIF

C Save the calculated voltage for this node and iteration
        VOLTPUP(INOD)=VOLTPUF(INOD)

  10  CONTINUE
      RETURN
      END

C ************************************************************************************
C Subroutine ENETNEWR is the Newton-Raphson algorithm for the solution of electrical
C network power flow.

      SUBROUTINE ENETNEWR(MODE)

#include "power.h"
#include "building.h"
#include "plant.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical network
      COMMON/ENET/ENTYPE

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The PER-UNIT electrical network state variables used in the iteration
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

C The electrical network jacobian solution matrix
      COMMON/ENETMTRX/JACOBEL(MJROW,MJCOL),EMATJZ(MJROW),
     &EDELTA(MJROW)

      COMMON/ENETSIMP/ENETMAXI,ENETSLVT,ENETVCVG,ENETACVG,ENETVLIM,
     &ENODCVGF(MENOD),CVGCHK

C Plant component electrical data
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)

C Hybrid components that calculate nodal voltages
      COMMON/calcV_LOC/iplant_calcV_LOC(MENOD),iPV_calcV_LOC(MENOD)

      DIMENSION RJACOBEL(MJROW,MJCOL),REMATJZ(MJROW),INDX(MJROW)

      DOUBLE PRECISION JACOBEL,EMATJZ,EDELTA,RJACOBEL,REMATJZ
      REAL ENETVLIMPU,ENODBASEV,ENODBASEP,ENETVCVG,ENETACVG,ENETVLIM,
     &MVDELT
      COMPLEX VOLTPUP,VOLTPUF
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,ENETMAXI,
     &ENETSLVT,ENODCVGF,ENTYPE
      CHARACTER*1 MODE
      LOGICAL CVGCHK
      REAL PSPM,QSPM,PFP,PWRP,BVOLTP
      COMPLEX VSPM
      INTEGER iplant_calcV_LOC,iPV_calcV_LOC,NPEL,IPFP,IPHP


C Solve the basic matrix equation:
C      [ dP/d0 | dP/dV ] [d0]    [SUM P] 0 - voltage phase angle
C      [ --------------] [  ] = -[ --- ]
C      [ dQ/d0 | dQ/dV ] [dV]    [SUM Q] V - voltage magnitude

C Firstly reduce the admittance matrix such that rows and column elements
C relating to fixed voltage nodes are removed. JACOBEL elements are moved
C into the RJACOBEL matrix (reduced Jacobian). Right-hand-side elements
C are moved into the REMATJZ (reduced-right-hand-side).
      IELM=0
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*) ' '
        WRITE(ITU,*) 'Subroutine ENETNEWR '
        WRITE(ITU,*) ' '
        WRITE(ITU,*) 'Matrix set up ... '
      ENDIF

      DO 10 INOD=1,NENOD*2
        IF(INOD.LE.NENOD) IANOD=INOD
        IF(INOD.GT.NENOD) IANOD=INOD-NENOD
        IF(ENODTYP(IANOD).EQ.1)THEN
          IELM=IELM+1

C Reduce the right-hand-side matrix
          REMATJZ(IELM)=EMATJZ(INOD)
          IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
                WRITE(ITU,*) 'Reduced RHS ',IELM,EMATJZ(INOD)
          ENDIF
          JELM=0
          DO 20 JNOD=1,NENOD*2
            IF(JNOD.LE.NENOD) JANOD=JNOD
            IF(JNOD.GT.NENOD) JANOD=JNOD-NENOD

C Reduce the Jacobian matrix.
            IF(ENODTYP(JANOD).EQ.1)THEN
              JELM=JELM+1
              RJACOBEL(IELM,JELM)=JACOBEL(INOD,JNOD)
C Trace.
              IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
                WRITE(ITU,*) 'Reduced Jacobian ',IELM,JELM,
     &JACOBEL(INOD,JNOD)

              ENDIF
            ENDIF
 20       CONTINUE
        ENDIF
10    CONTINUE


      IF(ENTYPE.NE.1)THEN
        NELM=IELM
        MELM=2*MENOD
      ELSE
        NELM=NENOD
        MELM=MENOD
      ENDIF

      IF(MODE.EQ.'N')THEN

C Solve with [RJACOBEL][EDELTA]=[REMATJZ] no pivoting
        if ( NELM > 0 ) THEN
          CALL JSOLVD(RJACOBEL,NELM,MELM,EDELTA,REMATJZ)
        ENDIF
      ELSE

C Solve [RJACOBEL][EDELTA]=[REMATJZ] with pivoting.
        CALL LUDCMP(RJACOBEL,NELM,MELM,INDX,D)
        CALL LUBKSB(RJACOBEL,NELM,MELM,INDX,REMATJZ)
        DO 30 I=1,NELM
          EDELTA(I)=REMATJZ(I)
  30     CONTINUE
      ENDIF

C The matrix EDELTA now contains the modifiers for the voltage
C at each node in the network. Check that in each case the maximum
C alowable.

      IELM=0
      DO 40 INOD=1,NENOD

C Check if the node is of variable or fixed voltage.
        IF(ENODTYP(INOD).EQ.1)THEN
          IELM=IELM+1
          ENETVLIMPU=ENETVLIM/ENODBASEV(MENOD)

C Calculate the new voltage value.
          VARG=ATAN2(AIMAG(VOLTPUP(INOD)),REAL(VOLTPUP(INOD)))+
     &SNGL(EDELTA(IELM))
          VMOD=CABS(VOLTPUP(INOD))+SNGL(EDELTA(IELM+NELM/2))
          VOLTPUF(INOD)=CMPLX(VMOD*COS(VARG),VMOD*SIN(VARG))
          MVDELT=CABS(VOLTPUF(INOD)-VOLTPUP(INOD))

C Trace.
          IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
            WRITE(ITU,*)' '
            WRITE(ITU,*)'Per Unit voltage solution for node: ',INOD
            WRITE(ITU,*)'Previous iteration pu voltage k ',VOLTPUP(INOD)
            WRITE(ITU,*)'This iteration pu voltage k+1   ',VOLTPUF(INOD)
            WRITE(ITU,*)'Voltage change                  ',
     &    VOLTPUF(INOD)-VOLTPUP(INOD)
          ENDIF

C Check that the voltage limit has not been exceeded.
          IF(MVDELT.GT.ENETVLIMPU) THEN
            VRAT=ENETVLIMPU/MVDELT
            DREAL=REAL(VOLTPUF(INOD)-VOLTPUP(INOD))*VRAT
            DIMAG=AIMAG(VOLTPUF(INOD)-VOLTPUP(INOD))*VRAT
            VOLTPUF(INOD)=VOLTPUP(INOD)+CMPLX(DREAL,DIMAG)
C Trace.
            IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
              WRITE(ITU,*)' '
              WRITE(ITU,*)'Node ', INOD,' voltage change limit applied.'
              WRITE(ITU,*)'This iteration pu voltage k+1 ',VOLTPUF(INOD)
            ENDIF
          ENDIF

C If the node is of fixed voltage then maintain that voltage.
        ELSEIF(ENODTYP(INOD).EQ.2)THEN
          VOLTPUF(INOD)=VOLTPUP(INOD)

C If the node's voltage is set by a special material in the building domain
C (e.g. PV) then impose the voltage calculated by the building domain.
        ELSEIF(ENODTYP(INOD).EQ.3)THEN
          VOLTPUF(INOD) = VSPM( iPV_calcV_LOC(INOD) ) / ENODBASEV(INOD)

C If the node's voltage is set by a plant component in the plant domain
C then impose the voltage calculated by the plant domain.
        ELSEIF(ENODTYP(INOD).EQ.4)THEN
          VOLTPUF(INOD) = CMPLX( BVOLTP(iplant_calcV_LOC(INOD))
     &                           /ENODBASEV(INOD) , 0. )

C All nodal types have been examined.
        ENDIF

 40   CONTINUE
      RETURN
      END

C ************************************************************************************
C Subroutine ENETCOVG determines whether or not the electrical network simulation
C has converged, based on the ctiteria set by the user in ENETPARAM

      SUBROUTINE ENETCOVG

#include "power.h"
#include "building.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The per-unit nodal boundary conditions
      COMMON/ENODBCPU/PENODPU(MENOD),QENODPU(MENOD)

C The PER-UNIT electrical network state variables used in the iteration
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)


C The array of driving port and transfer admittances
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

      COMMON/ENETSIMP/ENETMAXI,ENETSLVT,ENETVCVG,ENETACVG,ENETVLIM,
     &ENODCVGF(MENOD),CVGCHK

      REAL ENETVCVG,ENETACVG,ENETVLIM,ENODBASEV,ENODBASEP,
     &PENODPU,QENODPU

      COMPLEX VOLTPUP,VOLTPUF,YADM,SUMVY,SENODPU

      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,ENETMAXI,ENETSLVT,
     &ENODCVGF,NYELM

C Set the convergence flag to true
      LOGICAL CVGCHK

      CVGCHK=.TRUE.

C Loop through each node and determine if voltage and resudual convergence
C criteria have been met.

      DO 10 INOD=1,NENOD

C Set the individual nodal convergence flags
        ENODCVGF(INOD)=1
        IF(ENODTYP(INOD).EQ.1)THEN

C Firstly check if the voltages at each node have converged
          VRESL=ENETVCVG/ENODBASEV(INOD)
          VRESI=CABS(VOLTPUF(INOD)-VOLTPUP(INOD))
          IF(VRESI.GT.VRESL) THEN
            ENODCVGF(INOD)=0
            CVGCHK=.FALSE.
          ENDIF

C Check if the flow residuals have converged (comparisons done in per-unit).
          PRESL=ENETACVG/ENODBASEP
          SUMVY=CMPLX(0.0,0.0)
          SENODPU=CMPLX(PENODPU(INOD),QENODPU(INOD))
          DO 20 JNOD=1,NENOD
            SUMVY=SUMVY+
     &VOLTPUF(INOD)*CONJG(VOLTPUF(JNOD)*YADM(INOD,JNOD))
20        CONTINUE
          PRESI=CABS(SENODPU-SUMVY)
          IF(PRESI.GT.PRESL) THEN
            ENODCVGF(INOD)=0
            CVGCHK=.FALSE.
          ENDIF
        ENDIF

C Save the calculated voltage for the next iteration
        VOLTPUP(INOD)=VOLTPUF(INOD)

C Trace.
        IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
          IF(ENODTYP(INOD).EQ.1) THEN
            WRITE(ITU,*)' '
            WRITE(ITU,*)' Residuals ...'
            WRITE(ITU,*)' Node ', INOD, ' voltage residual ',VRESI
            WRITE(ITU,*)' Node ', INOD, ' power residual   ',PRESI
            IF(ENODCVGF(INOD).EQ.0)THEN
              WRITE(ITU,*)' node has not converged'
            ELSE
              WRITE(ITU,*)' node has converged '
            ENDIF
          ENDIF
        ENDIF

  10  CONTINUE

      RETURN
      END
