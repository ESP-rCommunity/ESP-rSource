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

C *************** MFMACH
C This file contains the following (multi-routine) modules
C which make up the calculation core of mfs:
C     MFSOLV
C     MFNPDV
C     MF010C
C     MF011C  Air inlet device self-adjustable, (polinomial)
C     MF015C
C     MF017C
C     MF020C
C     MF025C
C     MF030C
C     MF035C
C     MF040C
C     MF050C
C     MF110C
C     MF120C
C     MF130C
C     MF210C
C     MF211C  Cowl Roof Outlet
C     MF220C
C     MF230C
C     MF240C
C     MF250C
C     MF310C
C     MF410C
C     MF420C
C     MF460C
C     MF500C
C     MFOUTPB Writes timestep results to binary file
C     MFOUTPH Writes out header for binary results file
C     MFOUTP  Writes out trace information

C Whenever a new fluid flow component is added, a new MFnnnC
C subroutine should be provided. In addition a new MFnnnI, a
C new entry in MFCDAT and extra lines in MFPROB, MFSOLV and
C MFLOAD have to be added.
C MFnnnC(ICNN,DP) calculates the fluid mass flow rate through
C connection ICNN, which is a fluid flow component of type 'nnn'
C and has a pressure difference DP between its +ve and -ve
C sides.

C ************** MFSOLV 
C MFSOLV uses an iterative, whole network, Newton-Raphson method
C to solve the nodal pressures and the fluid mass flows through
C each interconnection of a nodal network which is submitted
C to one or more (pressure) boundary conditions.

C This subroutine starts out with some pressure at each node.
C Depending on the type of connection and its associated data,
C it then calculates the fluid mass flow rates through each
C connection and the mass flow residuals (= mass_in - mass_out)
C at each node. If the maximum absolute or relative residual is
C smaller than the error criterium (FERMFL or FERREL respectively),
C or the maximum number of iterations is exceeded, iteration stops.
C Else, the node pressures are adjusted and the whole process is re-
C started.

C All common block variables are described in emfnetw.F
C The main local variables in this subroutine are:

C     DFDDP  - (sum) of partial derivative(s) of flow(s) through a
C              connection relative to the pressure difference across it
C     DP     - pressure difference across an interconnection
C     FLWA   - absolute value of fluid mass flow through a connection
C     HPJMTX - history mechanism for PJMTX (needed for MFTRAC >= 3)
C     HPRC   - history mechanism for PRC (needed for Steffensen iter.)
C     INDX   - holds pivoting information in case of LU-decomposition
C     NDWABS - worst node with respect to absolute residual
C     NDWREL - worst node with respect to relative residual
C     PJMTX  - Jacobian matrix with 'Newton-Raphson coefficients'
C     PRC    - node pressure corrections vector
C     RATIO  - ratio of successive pressure corrections
C     RELAX  - pressure correction relaxation factor
C     RHS    - right hand side vector of matrix eq {PJMTX} {PRC} = {RHS}
C     RESR   - residual relative to SAFLW
C     RESAMX - maximum absolute residual
C     RESRMX - maximum relative residual (relative to SAFLW for NDWREL)

      SUBROUTINE MFSOLV

#include "building.h"
#include "net_flow.h"
#include "cfd.h"
      COMMON/MFTRA/IMFTU
      COMMON/MFLOW2/NDNAM(0:MNOD)

      COMMON/ACT/ACTCNN(MCNN)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/mfctl/ctlpos(MCNN)

      PARAMETER       (SMALL=1.0E-15)
      PARAMETER       (RLARG=1.0E+15)
      DIMENSION        INDX(MNOD)
      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      DOUBLE PRECISION DFDDP,DP,FLW1old(MCNN),FLW2old(MCNN)
      DOUBLE PRECISION FLWA,HPJMTX(MNOD,MNOD),HPRC(MNOD)
      DOUBLE PRECISION PJMTX(MNOD,MNOD),PRC(MNOD),RELAX(MNOD),RHS(MNOD)
      DOUBLE PRECISION RESR,RESAMX,RESRMX
      character outs*124,outw*254
      CHARACTER NDNAM*12,CONTMNAM*12
      LOGICAL ACTCNN,close

      IOK=1
      NDWABS=1
      NDWREL=1
      
C Save flow values from previous time step to use in case solution fails
C during an integrated simulation.
      do 1 I=1,NCNN
        FLW1old(I)=FLW1(I)
        FLW2old(I)=FLW2(I)
 1    continue


C Call control subroutine
      call MFCNTR

C Global controllers may be active so invoke these as well
      call MZGCTL
      
C Start of iteration loop
C First zeroise node residual, sum of flows and Jacobian matrix and
C set diagonal elements of the Jacobian matrix for known pressure nodes
      ITER=0
   20 ITER=ITER+1
      DO 24 INOD=1,NNOD
        RESID(INOD)=0.0
        SAFLW(INOD)=0.0
        DO 22 JNOD=1,NNOD
          IF(NDTYP(INOD).NE.0.AND.INOD.EQ.JNOD) THEN
            PJMTX(INOD,JNOD)=DBLE(1.0)
          ELSE
            PJMTX(INOD,JNOD)=DBLE(0.0)
          END IF
   22   CONTINUE
   24 CONTINUE

C Start of connections fluid mass flow calculations
      DO 30 ICNN=1,NCNN

C Set up node identifiers and connection type
        IDPS=NODPS(ICNN)
        IDNE=NODNE(ICNN)
        ITCN=ITPCMP(ITPCON(ICNN))

C Set up connection pressure difference; include buoyancy effect
        DP=PRES(IDPS)-PRES(IDNE)+PSTACK(ICNN)

C If connection is not active set the flow rate equal to zero.
        IF(.NOT.ACTCNN(ICNN))THEN
          FLW1(ICNN)=0d0
          FLW2(ICNN)=0d0
          DFDDP=0d0
        ELSE
        
C If multi configuration flow component then change it to 
C relevant flow component
          IF(ITCN.EQ.500)THEN
            icontc=1
            ICMP=ITPCON(ICNN)
            IF(CTLPOS(ICNN).LE.SUPCMP(ICMP,5))THEN
              ICOMPNO=INT(SUPCMP(ICMP,6))
              ITPCON(ICNN)=INT(SUPCMP(ICMP,6))
            ELSEIF(CTLPOS(ICNN).LE.SUPCMP(ICMP,3).AND.
     &        CTLPOS(ICNN).GT.SUPCMP(ICMP,5))THEN
              ICOMPNO=INT(SUPCMP(ICMP,4))
              ITPCON(ICNN)=INT(SUPCMP(ICMP,4))
            ELSE
              ICOMPNO=INT(SUPCMP(ICMP,2))
              ITPCON(ICNN)=INT(SUPCMP(ICMP,2))
            ENDIF
            ITCN=ITPCMP(ICOMPNO)

C Allow this replaced component to operate fully i.e. ctlpos=1
            ctlposp=ctlpos(icnn)
            ctlpos(icnn)=1.
          ELSE
            icontc=0
          ENDIF

C Calculate, depending on flow component type, fluid mass flow(s)
C and partial derivative dFlow/d(dPressure)
          IF(ITCN.EQ. 10) THEN
            CALL MF010C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 11) THEN
            CALL MF011C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 15) THEN
            CALL MF015C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 17) THEN
            CALL MF017C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 20) THEN
            CALL MF020C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 25) THEN
            CALL MF025C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 30) THEN
            CALL MF030C(ICNN,DFDDP)
          ELSE IF(ITCN.EQ. 35) THEN
            CALL MF035C(ICNN,DFDDP)
          ELSE IF(ITCN.EQ. 40) THEN
            CALL MF040C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ. 50) THEN
            CALL MF050C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.110) THEN
            CALL MF110C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.120) THEN
            CALL MF120C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.130) THEN
            CALL MF130C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.210) THEN
            CALL MF210C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.211) THEN
            CALL MF211C(ICNN,DP,DFDDP) 
          ELSE IF(ITCN.EQ.220) THEN
            CALL MF220C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.230) THEN
            CALL MF230C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.240) THEN
            CALL MF240C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.250) THEN
            CALL MF250C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.310) THEN
            CALL MF310C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.410) THEN
            CALL MF410C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.420) THEN
            CALL MF420C(ICNN,DP,DFDDP)
          ELSE IF(ITCN.EQ.460) THEN
            CALL MF460C(ICNN,DP,DFDDP)
          ELSE
            call edisp(IMFTU,' MFSOLV: unresolvable error !')
            call epwait
            call epagend
            STOP 
          END IF

C Restore original values if multi configuration control
          if(icontc.eq.1)then
            ITPCON(ICNN)=ICMP
            ctlpos(icnn)=ctlposp
          endif
        ENDIF

C Adjust fluid mass flow residual of connected nodes
        RESID(IDPS)=RESID(IDPS)-FLW1(ICNN)-FLW2(ICNN)
        RESID(IDNE)=RESID(IDNE)+FLW1(ICNN)+FLW2(ICNN)

C And their sum of absolute flows
        FLWA=DABS(FLW1(ICNN))+DABS(FLW2(ICNN))
        SAFLW(IDPS)=SAFLW(IDPS)+FLWA
        SAFLW(IDNE)=SAFLW(IDNE)+FLWA

C Set up the elements of the Jacobian matrix
        IF(NDTYP(IDPS).EQ.0.AND.NDTYP(IDNE).EQ.0) THEN
          PJMTX(IDPS,IDPS)=PJMTX(IDPS,IDPS)+DFDDP
          PJMTX(IDPS,IDNE)=PJMTX(IDPS,IDNE)-DFDDP
          PJMTX(IDNE,IDNE)=PJMTX(IDNE,IDNE)+DFDDP
          PJMTX(IDNE,IDPS)=PJMTX(IDNE,IDPS)-DFDDP
        ELSE IF(NDTYP(IDPS).EQ.0.AND.NDTYP(IDNE).NE.0) THEN
          PJMTX(IDPS,IDPS)=PJMTX(IDPS,IDPS)+DFDDP
          PJMTX(IDPS,IDNE)=PJMTX(IDPS,IDNE)-DFDDP
        ELSE IF(NDTYP(IDPS).NE.0.AND.NDTYP(IDNE).EQ.0) THEN
          PJMTX(IDNE,IDNE)=PJMTX(IDNE,IDNE)+DFDDP
          PJMTX(IDNE,IDPS)=PJMTX(IDNE,IDPS)-DFDDP
        END IF
   30 CONTINUE

C Find worst node(s)
      RESAMX=0.
      RESRMX=0.
      DO 40 INOD=1,NNOD

C Internal nodes only
        IF(NDTYP(INOD).GE.1) GOTO 40

C Calculate residual relative to sum of mass flow absolute values
C Make sure that: SMALL < | RESID / SAFLW | < RLARG
        IF(DABS(RESID(INOD)).LE.SMALL*SAFLW(INOD)) THEN
          RESR=DSIGN(DBLE(SMALL),RESID(INOD))
        ELSE IF(DABS(RESID(INOD)).GE.RLARG*SAFLW(INOD)) THEN 
          RESR=DSIGN(DBLE(RLARG),RESID(INOD))
        ELSE 
          RESR=RESID(INOD)/SAFLW(INOD)
        END IF

C Update worst node values if this is the worst node up to now
        IF(DABS(RESID(INOD)).GT.DABS(RESAMX)) THEN
          NDWABS=INOD
          RESAMX=RESID(INOD)
        END IF
        IF(DABS(RESR).GT.DABS(RESRMX)) THEN
          NDWREL=INOD
          RESRMX=RESR
        END IF
   40 CONTINUE

C Write trace output if applicable
      IF(MFTRAC.GE.1)then
         WRITE(outs,1050) ITER,NDNAM(NDWREL),PRES(NDWREL),
     &                    RESID(NDWREL),SAFLW(NDWREL),RESRMX
         call edisp(IMFTU,outs)
      endif
      IF(MFTRAC.GE.2) THEN
        DO 50 INOD=1,NNOD
          IF(INOD.NE.NDWABS.AND.INOD.NE.NDWREL) THEN
            WRITE(outs,1052) '      ',NDNAM(INOD),PRES(INOD),
     &                         RESID(INOD),SAFLW(INOD)
            call edisp(IMFTU,outs)
          ELSE
            WRITE(outs,1052) '*****>',NDNAM(INOD),PRES(INOD),
     &                         RESID(INOD),SAFLW(INOD)
            call edisp(IMFTU,outs)
          END IF
   50   CONTINUE
      END IF
 1050 FORMAT(' It',I4,1X,A12,
     &       '  P: ',G12.5,
     &       '  R: ',G11.4,
     &       '  Sa: ',G11.4,
     &       '  Er: ',G11.4)
 1052 FORMAT(' ',A6,1X,A12,
     &       '  P: ',G12.5,
     &       '  R: ',G11.4,
     &       '  Sa: ',G11.4)

C If RESRMX or RESID(NDWABS) within tolerance, stop iteration
      IF(DABS(RESRMX).LE.FERREL.OR.
     &   DABS(RESID(NDWABS)).LE.FERMFL) GOTO 100

C If maximum number of iterations reached, IOK=0 and stop iteration
      IF(ITER.GE.MAXITF) THEN
         IOK=0
         GOTO 100
      END IF

C Calculation of pressure corrections by matrix equation processing
C Start with generating more trace output if applicable
      IF(MFTRAC.GE.3) THEN
        call edisp(IMFTU,' PJMTX matrix')
        DO 60 I=1,NNOD
          WRITE(outw,*) (PJMTX(I,J),J=1,NNOD)
          call edisp(IMFTU,outw)
          DO 60 J=1,NNOD
            HPJMTX(I,J)=PJMTX(I,J)
   60   CONTINUE
        call edisp(IMFTU,' RESID vector')
        WRITE(outw,*) (RESID(I),I=1,NNOD)
        call edisp(IMFTU,outw)
      END IF

C Set right hand side of matrix equation {PJMTX} {PRC} = {RESID}
C To avoid calculating a pressure correction for known pressure nodes
C zeroise RHS elements for known pressure nodes
C In some cases it is possible that an unknown pressure node's diagonal
C coefficient is equal to zero, e.g.:
C - when there is no flow through its connections
C - when the partial derivatives of its connections are effectively zero
C to avoid numerical problems when solving the matrix for these cases,
C set the diagonal element to 1. and the RHS to 0. (ie. don't change
C this node's pressure for the time being)
      DO 62 INOD=1,NNOD
      IF(NDTYP(INOD).EQ.0) THEN
         call eclose(REAL(PJMTX(INOD,INOD)),0.00,0.00001,close)
         IF(.NOT.close) THEN
            RHS(INOD)=RESID(INOD)
         ELSE
            PJMTX(INOD,INOD)=DBLE(1.0)
            RHS(INOD)=0.0
            IF(MFTRAC.GE.3)then
              WRITE(outs,'(A,I3,A)') ' Node ',INOD,
     &                               ' DIAG. -> 1. ; RHS -> 0.'
              call edisp(IMFTU,outs)
           endif
         END IF
      ELSE
         RHS(INOD)=0.0
         IF(MFTRAC.GE.3)then
            WRITE(outs,'(A,I3,A)') ' Node ',INOD,' RHS   -> 0.'
            call edisp(IMFTU,outs)
         endif
      END IF
   62 CONTINUE

C Now solve {PRC} from matrix equation {PJMTX} {PRC} = {RHS}
      IF(MSLVTP.EQ.1) THEN
         CALL JSOLVD(PJMTX,NNOD,MNOD,PRC,RHS)
      ELSE IF(MSLVTP.EQ.2) THEN
         CALL LUDCMP(PJMTX,NNOD,MNOD,INDX,D)
         CALL LUBKSB(PJMTX,NNOD,MNOD,INDX,RHS)
         DO 64 INOD=1,NNOD
         PRC(INOD)=RHS(INOD)
   64    CONTINUE
      ELSE
        call edisp(IMFTU,' MFSOLV: unresolvable error !')
        call epwait
        call epagend
        STOP 
      END IF

C If trace output then dump solution vector
      IF(MFTRAC.GE.3) THEN
         call edisp(IMFTU,'  PRC solution vector')
         WRITE(outw,*) (PRC(I),I=1,NNOD)
         call edisp(IMFTU,outw)

C Dump {RHS} recalculated from {HPJMTX}*{PRC}
         WRITE(IMFTU,*) ' RHS vector recalculated from PJMTX * PRC'
         DO 65 I=1,NNOD
         RHS(I)=0.0
         DO 65 J=1,NNOD
         RHS(I)=RHS(I)+HPJMTX(I,J)*PRC(J)
   65    CONTINUE
         WRITE(outw,*) (RHS(I),I=1,NNOD)
         call edisp(IMFTU,outw)
      END IF

C Revise pressure correction vector by Steffensen's method to
C handle oscillating corrections
C Make sure that: SMALL < | PRC / HPRC | < RLARG 
      DO 70 INOD=1,NNOD
      IF(NDTYP(INOD).NE.0) GOTO 70
      call eclose(REAL(RELAX(INOD)),1.00,0.00001,close)
      IF(close) THEN
         IF(DABS(PRC(INOD)).LE.SMALL*DABS(HPRC(INOD))) THEN
            RATIO=REAL(DSIGN(DBLE(SMALL),PRC(INOD)*HPRC(INOD)))
         ELSE IF(DABS(PRC(INOD)).GE.RLARG*DABS(HPRC(INOD))) THEN
            RATIO=REAL(DSIGN(DBLE(RLARG),PRC(INOD)*HPRC(INOD)))
         ELSE
            RATIO=REAL(PRC(INOD)/HPRC(INOD))
         END IF
         call eclose(RATIO,1.00,0.00001,close)
         IF(RATIO.LT.STEFFR.AND.(.NOT.close)) THEN
            RELAX(INOD)=1.0/(1.0-RATIO)
            IF(MFTRAC.GE.3)then
               WRITE(outs,*) ' Node ',INOD,' RELAX  = ',RELAX(INOD)
               call edisp(IMFTU,outs)
            endif
         END IF
      ELSE
         RELAX(INOD)=DBLE(1.0)
      END IF
      HPRC(INOD)=PRC(INOD)
      PRC(INOD)=PRC(INOD)*RELAX(INOD)
   70 CONTINUE

C Adjust node pressure of NDTYP=0 nodes, but limit correction to PMAX
      DO 80 INOD=1,NNOD
      IF(NDTYP(INOD).NE.0) GOTO 80
      IF(DABS(PRC(INOD)).GT.PMAX) THEN
         RELAX(INOD)=RELAX(INOD)*PMAX/DABS(PRC(INOD))
         IF(PRC(INOD).GT.0.0) PRC(INOD)= PMAX
         IF(PRC(INOD).LT.0.0) PRC(INOD)=-PMAX
         IF(MFTRAC.GE.3)then
            WRITE(outs,*) ' Node ',INOD,' PRC   -> ',PRC(INOD)
            call edisp(IMFTU,outs)
         endif
      END IF
      PRES(INOD)=PRES(INOD)+PRC(INOD)
   80 CONTINUE

C Back to start of iteration loop
      GOTO 20

C Output message. If solution failed over-ride MFTRAC.
  100 IF(IOK.EQ.1) THEN
        IF(MFTRAC.GE.0)then
           WRITE(outs,1100) IRD,IRM,FLWTIM,ITER,'successful'
           call edisp(IMFTU,outs)
        endif
      ELSE
        WRITE(outs,1100) IRD,IRM,FLWTIM,ITER,'failed    '
        WRITE(outs,1100) IRD,IRM,FLWTIM,ITER,'failed    '
        call edisp(IMFTU,outs)
        call edisp(IMFTU,' ')
        call edisp(IMFTU,'Please check (in the following order) that:')
        call edisp(IMFTU,
     &    ' - each node has some flow path to a boundary pressure and')
        call edisp(IMFTU,
     &    ' - iteration convergence control is not too strict.')
  
C Do not use these flow values (use values from previous time step).
        do 1111 I=1,NCNN
          FLW1(I)=FLW1old(I)
          FLW2(I)=FLW2old(I)
 1111   continue
      END IF
 1100 FORMAT(' Day ',I2,' Month ',I2,' Hour ',F6.3,':',
     &       I5,' iterations; flows solution ',A10)

C Relax the new flow values if time step controller 4 active.
C      if (itrpas.gt.0) then
C        write(6,*) 'relaxing'
C        frac=0.25
C        do 2222 I=1,NCNN
C          FLW1(I)=(FLW1(I)-FLW1old(I))*frac+FLW1old(I)
C          FLW2(I)=(FLW2(I)-FLW2old(I))*frac+FLW2old(I)
C 2222   continue
C      endif

C Invoke contaminant simulation if contaminants are defined
      IF(NOCNTM.EQ.1) THEN
        CALL MZCONT
      ENDIF

      RETURN

      END

C ************ MFNPDV 
C Numerical calculation of partial derivative dFlw[s]/d(dPress)
C which is to be used when the partial derivative cannot be
C calculated analytically by MFxxxC
C The partial derivative is approximated by (Fnow-Fprev)/(dPnow-dPprev)
C If the partial derivative turns out to be close to zero then the
C previous value is returned instead; this is done to avoid numerical
C problems when using the derivative in Newton-Raphson technique

      SUBROUTINE MFNPDV(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)
      DOUBLE PRECISION DDP

C Calculate d(dPressure) and set history mechanism for dPressure
      DDP=DELP-HDP(ICNN)
      HDP(ICNN)=DELP

C If current partial derivative dFlow/d(dPressure) not close to zero
C recalculate and set history mechanism, otherwise use previous value
      IF(DABS(FLW1(ICNN)+FLW2(ICNN)-HFL(ICNN)).GT.DBLE(SMALL).AND.
     &   DABS(DDP).GT.DBLE(SMALL)) THEN
         DERIV=(FLW1(ICNN)+FLW2(ICNN)-HFL(ICNN))/DDP
         HDV(ICNN)=DERIV
      ELSE
         DERIV=HDV(ICNN)
      END IF

C Set history mechanism for flow
      HFL(ICNN)=FLW1(ICNN)+FLW2(ICNN)

      RETURN
      END

C ************* MF010C 
C Fluid mass flow calculation routine for flow component type:
C power law volume flow resistance (m = rho.a.dP^b)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FCOF = SUPCMP(ICMP,2) - flow coefficient a (m^3/s/Pa^b)
C     FEXP = SUPCMP(ICMP,3) - flow exponent b (-)

      SUBROUTINE MF010C(ICNN,DELP,DERIV)

#include "net_flow.h"


      COMMON/mfctl/ctlpos(MCNN)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      FCOF=SUPCMP(ICMP,2)
      FEXP=SUPCMP(ICMP,3)

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        FLW1(ICNN)=RHON(IDPS)*FCOF*(DELP**FEXP)*ctlpos(icnn)
      ELSE
        FLW1(ICNN)=-RHON(IDNE)*FCOF*(DABS(DELP)**FEXP)*ctlpos(icnn)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=FEXP*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If component is closed the derivative is set equal to zero.
      if((dabs(delp).gt.DBLE(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.DBLE(small))) deriv=0.0

      RETURN
      END

C ************* MF011C
C Method after Armando Pinto which approximates self regulating
C devices which have a rated flow rate of 15m3/h at 20 Pa. or
C 30m3/h at 20 Pa. These devices are designed to maintain flow
C close to these rates over a range of pressure differences.
C The component uses a polynomial to predict flows at
C other pressures. This device is typical of units
C mfg for use in the French market. Further information can
C be found at <http://www.france-air.com/franceair/doc_prod/PV/PV1_E311.pdf>

C Fluid mass flow calculation routine for flow component type:
C Air inlet device self-adjustable, given by polinomial
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FCOF = SUPCMP(ICMP,2) - air flow at 20 Pa, 15 or 30 m3/h
C     FEXP = SUPCMP(ICMP,3) - number of devices

      SUBROUTINE MF011C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      DOUBLE PRECISION a6,a5,a4,a3,a2,a1,RO,CVol
      logical close15,close30

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

c FCOF = 15.0 for 15 m3/h  FCOF=30.0 for 30 m3/h unit.
      FCOF=SUPCMP(ICMP,2)
      call eclose(FCOF,15.00,0.001,close15)
      call eclose(FCOF,30.00,0.001,close30)

c FEXP = number of devices (??)
      FEXP=SUPCMP(ICMP,3)

C Depending on which FCOF set parameters. If neither assume values for 15.
      IF (close15) then
        a6=-1.1873D-9
        a5=4.7143D-7
        a4=-7.2433D-5
        a3=5.3879D-3
        a2=-1.9706D-1
        a1=DBLE(3.1833)
      elseif (close30) then
        a6=-2.4621D-10
        a5=1.2947D-7
        a4=-2.6885D-5
        a3=2.7794D-3
        a2=-1.4663D-1
        a1=DBLE(3.6182)
      else
        a6=-1.1873D-9
        a5=4.7143D-7
        a4=-7.2433D-5
        a3=5.3879D-3
        a2=-1.9706D-1
        a1=DBLE(3.1833)
      endif

      CVol=FEXP*(a6*DABS(DELP)**6+a5*DABS(DELP)**5+a4*DABS(DELP)**4+
     &           a3*DABS(DELP)**3+a2*DABS(DELP)**2+a1*DABS(DELP)) 

      if (dabs(delp).gt.150.) then
        CVol=FEXP*(a6*(150.)**6+a5*(150.)**5+a4*(150.)**4+
     &             a3*(150.)**3+a2*(150.)**2+a1*(150.)) 
      end if
      
C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0.) THEN
        FLW1(ICNN)=RHON(IDPS)*CVol/3600.
        RO=RHON(IDPS)
      ELSE
        FLW1(ICNN)=-RHON(IDNE)*CVol/3600.
        RO=RHON(IDNE)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=RO/3600.*FEXP*(6.*a6*DABS(DELP)**5+5.*a5*DABS(DELP)**4+
     &    4.*a4*DABS(DELP)**3+3.*a3*DABS(DELP)**2+2.*a2*DABS(DELP)+a1)
        IF (dabs(delp).gt.150.) DERIV=0.0
      ELSE
        CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If component is closed the derivative is set equal to zero.
      if((dabs(delp).gt.DBLE(small)).and.((dabs(flw1(icnn))+
     &    dabs(flw2(icnn))).lt.DBLE(small))) deriv=0.0

      RETURN
      END

C *************** MF015C 
C Fluid mass flow calculation routine for flow component type:
C power law mass flow resistance (m = a.dP^b)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FCOF = SUPCMP(ICMP,2) - flow coefficient a (kg/s/Pa^b)
C     FEXP = SUPCMP(ICMP,3) - flow exponent b (-)

      SUBROUTINE MF015C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER       (SMALL=1.0E-15)
      DOUBLE PRECISION DELP,DERIV

      ICMP=ITPCON(ICNN)

      FCOF=SUPCMP(ICMP,2)
      FEXP=SUPCMP(ICMP,3)

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        FLW1(ICNN)=FCOF*(DELP**FEXP)*ctlpos(icnn)
      ELSE
        FLW1(ICNN)=-FCOF*(DABS(DELP)**FEXP)*ctlpos(icnn)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=FEXP*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If component is closed the derivative is set equal to zero.
      if((dabs(delp).gt.DBLE(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.DBLE(small))) deriv=0.0

      RETURN
      END

C *************** MF017C 
C Fluid mass flow calculation routine for flow component type:
C power law mass flow resistance (m = a.rho^1/2.dP^b)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FCOF = SUPCMP(ICMP,2) - flow coefficient a (kg.m^3)^.5/s/Pa^b)
C     FEXP = SUPCMP(ICMP,3) - flow exponent b (-)

      SUBROUTINE MF017C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER       (SMALL=1.0E-15)
      DOUBLE PRECISION DELP,DERIV

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      FCOF=SUPCMP(ICMP,2)
      FEXP=SUPCMP(ICMP,3)

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        FLW1(ICNN)=FCOF*SQRT(RHON(IDPS))*(DELP**FEXP)*ctlpos(icnn)
      ELSE
        FLW1(ICNN)=-FCOF*SQRT(RHON(IDNE))*(DABS(DELP)**FEXP)*
     &              ctlpos(icnn)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=FEXP*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If component is closed the derivative is set equal to zero.
      if((dabs(delp).gt.DBLE(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.DBLE(small))) deriv=0.0 

      RETURN
      END

C *************** MF020C 
C Fluid mass flow calculation routine for flow component type:
C quadratic law volume flow resistance.
C (dP = a.m/rho + b.(m/rho)^2)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     COFA = SUPCMP(ICMP,2) - flow coefficient a (Pa/m^3/s)
C     COFB = SUPCMP(ICMP,3) - flow coefficient b (Pa/(m^3/s)^2)

      SUBROUTINE MF020C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      DOUBLE PRECISION DSCR
      logical closea,closeb

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      COFA=SUPCMP(ICMP,2)
      COFB=SUPCMP(ICMP,3)
      DSCR=DSQRT(COFA*COFA+4.*COFB*DABS(DELP))

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        call eclose(COFA,0.00,0.00001,closea)
        call eclose(COFB,0.00,0.00001,closeb)
        IF((.NOT.closea).AND.(.NOT.closeb)) THEN
           FLW1(ICNN)=RHON(IDPS)*(-COFA+DSCR)/(2.*COFB)*ctlpos(icnn)
        ELSE IF(.NOT.closea) THEN
           FLW1(ICNN)=RHON(IDPS)*DELP/COFA*ctlpos(icnn)
        ELSE
           FLW1(ICNN)=RHON(IDPS)*DSQRT(DELP/COFB)*ctlpos(icnn)
        END IF
      ELSE
        call eclose(COFA,0.00,0.00001,closea)
        call eclose(COFB,0.00,0.00001,closeb)
        IF((.NOT.closea).AND.(.NOT.closeb)) THEN
           FLW1(ICNN)=-RHON(IDNE)*(-COFA+DSCR)/(2.*COFB)*ctlpos(icnn)
        ELSE IF(.NOT.closea) THEN
           FLW1(ICNN)=-RHON(IDNE)*DABS(DELP)/COFA*ctlpos(icnn)
        ELSE
           FLW1(ICNN)=-RHON(IDNE)*DSQRT(DABS(DELP)/COFB)*ctlpos(icnn)
        END IF
      END IF

      FLW2(ICNN)=0.0

      IF(DSCR.GT.DBLE(SMALL)) THEN
         IF(DELP.GE.0) THEN
            DERIV=RHON(IDPS)/DSCR
         ELSE
            DERIV=-RHON(IDNE)/DSCR
         END IF
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If component is closed the derivative is set equal to zero.
      if((dabs(delp).gt.DBLE(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.DBLE(small))) deriv=0.0    

      RETURN
      END

C *************** MF025C 
C Fluid mass flow calculation routine for flow component type:
C quadratic law mass flow resistance (dP = a.m+b.m^2).
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     COFA = SUPCMP(ICMP,2) - flow coefficient a (Pa/(kg/s))
C     COFB = SUPCMP(ICMP,3) - flow coefficient b (Pa/(kg/s)^2)

      SUBROUTINE MF025C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      DOUBLE PRECISION DSCR
      logical closea,closeb


      PARAMETER       (SMALL=1.0E-15)

      ICMP=ITPCON(ICNN)

      COFA=SUPCMP(ICMP,2)
      COFB=SUPCMP(ICMP,3)
      DSCR=DSQRT(COFA*COFA+4.*COFB*DABS(DELP))

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        call eclose(COFA,0.00,0.00001,closea)
        call eclose(COFB,0.00,0.00001,closeb)
        IF((.NOT.closea).AND.(.NOT.closeb)) THEN
           FLW1(ICNN)=(-COFA+DSCR)/(2.*COFB)*ctlpos(icnn)
        ELSE IF(.NOT.closea) THEN
           FLW1(ICNN)=DELP/COFA*ctlpos(icnn)
        ELSE
           FLW1(ICNN)=DSQRT(DELP/COFB)*ctlpos(icnn)
        END IF
      ELSE
        call eclose(COFA,0.00,0.00001,closea)
        call eclose(COFB,0.00,0.00001,closeb)
        IF((.NOT.closea).AND.(.NOT.closeb)) THEN
           FLW1(ICNN)=-(-COFA+DSCR)/(2.*COFB)*ctlpos(icnn)
        ELSE IF(.NOT.closea) THEN
           FLW1(ICNN)=-DABS(DELP)/COFA*ctlpos(icnn)
        ELSE
           FLW1(ICNN)=-DSQRT(DABS(DELP)/COFB)*ctlpos(icnn)
        END IF
      END IF

      FLW2(ICNN)=0.0

      IF(DSCR.GT.DBLE(SMALL)) THEN
         IF(DELP.GE.0) THEN
            DERIV=1.0/DSCR
         ELSE
            DERIV=-1.0/DSCR
         END IF
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If component is closed the derivative is set equal to zero.
      if((dabs(delp).gt.dble(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0    

      RETURN
      END

C ****************** MF030C 
C Fluid mass flow calculation routine for flow component type:
C constant volume flow rate (m = ro a)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FLOW = SUPCMP(ICMP,2) - volume flow rate a (m^3/s)
C In the case of range based control FLOW is multiplied by the
C control ratio returned in ctlpos.

      SUBROUTINE MF030C(ICNN,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DERIV

      IDPS=NODPS(ICNN)
      ICMP=ITPCON(ICNN)

      FLOW=SUPCMP(ICMP,2)

C Because FLOW is specified in a particular direction (from
C positive to negative side of the connection), the density
C of the positive connection is used.
      FLW1(ICNN)=RHON(IDPS)*FLOW*ctlpos(icnn)
      FLW2(ICNN)=0.

      DERIV=0.0

      RETURN
      END

C ****************** MF035C 
C Fluid mass flow calculation routine for flow component type:
C constant mass flow rate (m = a)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FLOW = SUPCMP(ICMP,2) - flow a (kg/s)
C In the case of range based control FLOW is multiplied by the
C control ratio returned in ctlpos.

      SUBROUTINE MF035C(ICNN,DERIV)

#include "net_flow.h"

      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DERIV

      ICMP=ITPCON(ICNN)
      FLOW=SUPCMP(ICMP,2)

C Because FLOW already expressed as mass flow, direction is unimportant.
      FLW1(ICNN)=FLOW*ctlpos(icnn)
      FLW2(ICNN)=0.
      DERIV=0.0
      RETURN
      END

C ***************** MF040C 
C Fluid mass flow calculation routine for flow component type:
C common orifice flow component (m = rho.Cd.A(2/rho.dP)**.5)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     AREA = SUPCMP(ICMP,2) - opening area (m^2)
C     CDIS = SUPCMP(ICMP,3) - discharge factor (-)
C In the case of range based control FLOW is multiplied by the
C control ratio returned in ctlpos.

      SUBROUTINE MF040C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,2)
      CDIS=SUPCMP(ICMP,3)

C If DELP >= 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0.0) THEN
         FLW1(ICNN)= CDIS*AREA*DSQRT(2*RHON(IDPS)*DELP)*ctlpos(icnn)
      ELSE
         FLW1(ICNN)=-CDIS*AREA*DSQRT(2*RHON(IDNE)*DABS(DELP))*
     &              ctlpos(icnn)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=.5*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If orifice is closed the derivative is set equal to zero.
      if((dabs(delp).gt.dble(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0    

      RETURN
      END

C ****************** MF050C 
C Fluid mass flow calculation routine for flow component type:
C laminar pipe volume flow rate (m = rho(dP/(8 mu L))pi.R**4)
C     IFLD = SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     OLNG = SUPCMP(ICMP,2) - length of flow path (m)
C     ORAD = SUPCMP(ICMP,3) - radius of opening   (m)

      SUBROUTINE MF050C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)
      character outs*124

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      IFLD=INT(SUPCMP(ICMP,1))
      OLNG=SUPCMP(ICMP,2)
      ORAD=SUPCMP(ICMP,3)

      PI=4.*ATAN(1.)

C If DELP >= 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0.0) THEN
         FMU=DYVISC(IFLD,TNOD(IDPS))
         DERIV= RHON(IDPS)*PI*ORAD**4/(8.*FMU*OLNG)
      ELSE
         FMU=DYVISC(IFLD,TNOD(IDNE))
         DERIV=-RHON(IDNE)*PI*ORAD**4/(8.*FMU*OLNG)
      END IF

      FLW1(ICNN)=DERIV*DELP*ctlpos(icnn)

      FLW2(ICNN)=0.

C Warn if flow is probably turbulent; ie. Reynolds >= 2320.
      REY=2.*REAL(FLW1(ICNN))/(PI*ORAD*FMU)
      IF(REY.GE.2320.0) THEN
         CALL DAYCLK(IYD,FLWTIM,ICOUT)
         call edisp(icout,' MF050C warning: flow probably not laminar.')
         WRITE(outs,'(A,F10.3)') ' Re = ',REY
         call edisp(icout,outs)
      END IF

C If duct is closed the derivative is set equal to zero.
      if((dabs(delp).gt.dble(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0   

      RETURN
      END

C ************** MF110C 
C Fluid mass flow calculation routine for flow component type:
C specific air flow opening
C            SUPCMP(ICMP,1) - fluid type (=1 - air)
C     AREA = SUPCMP(ICMP,2) - opening area (m^2)
C In the case of range based control FLOW is multiplied by the
C control ratio returned in ctlpos.

      SUBROUTINE MF110C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,2) 

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        FLW1(ICNN)=0.65*AREA*DSQRT(2.*RHON(IDPS)*DELP)*ctlpos(icnn)
      ELSE
        FLW1(ICNN)=-0.65*AREA*DSQRT(2.*RHON(IDNE)*DABS(DELP))*
     &              ctlpos(icnn)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.dble(SMALL)) THEN
         DERIV=0.5*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If opening is closed the derivative is set equal to zero.
      if((dabs(delp).gt.dble(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0 

      RETURN
      END

C *************** MF120C 
C Fluid mass flow calculation routine for flow component type:
C specific air flow crack component
C            SUPCMP(ICMP,1)    - fluid type (=1 - air)
C     FEXP = f{SUPCMP(ICMP,2)} - crack width  (m)
C     FCOF = f{SUPCMP(ICMP,3)} - crack length (m)

      SUBROUTINE MF120C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      FEXP=0.5+0.5*EXP(-1.E3*SUPCMP(ICMP,2)/2)
      FCOF=1.E-3*SUPCMP(ICMP,3)*9.7*(0.0092)**FEXP

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
        FLW1(ICNN)=RHON(IDPS)*FCOF*DELP**FEXP*ctlpos(icnn)
      ELSE
        FLW1(ICNN)=-RHON(IDNE)*FCOF*DABS(DELP)**FEXP*ctlpos(icnn)
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=FEXP*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If crack is closed the derivative is set to zero.
      if((dabs(delp).gt.dble(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0  

      RETURN
      END

C ************** MF130C 
C Fluid mass flow calculation routine for flow component type:
C specific air flow door
C            SUPCMP(ICMP,1) - fluid type (=1 - air)
C     WDTH = SUPCMP(ICMP,2) - door width (m) which will be modified by the
C                             value of the ctlpos().
C     HGHT = SUPCMP(ICMP,3) - door height (m)
C     DHRF = SUPCMP(ICMP,4) - adjacent nodes height above base of doorway (m)
C                             (both adj nodes assumed to be at same height)
C     CDIS = SUPCMP(ICMP,5) - door discharge factor (-)
      
C     CQ, CT, CA, CB        - (complex) intermediate variables
C     DHRF                  - ratio between node height above base of
C                             doorway and door height (-)
C     PATM                  - atmospheric pressure (Pa)
C     RGAS                  - gas constant (J/kg.K)
C     TPOS                  - absolute temperature +ve side (K)
C     TNEG                  - absolute temperature -ve side (K)

      SUBROUTINE MF130C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      
      PARAMETER       (SMALL=1.0E-15)
      COMPLEX          CQ,CA,CB
      logical close

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

C If uncontrolled then ctlpos will be 1.0, if so just set width.
      call eclose(ctlpos(icnn),1.00,0.01,close)
      if(close)then

C Standard working of bi-directional component.
        WDTH=SUPCMP(ICMP,2)
      else

C Control is being applied to bi-directional component.
C Apply value of ctlpos to WDTH if component controlled.
        if(ctlpos(icnn).lt.0.0)then
          WDTH=0.00

C Debug.
C          write(6,*) 'door width reset to 0mm'

        else
          WDTH=SUPCMP(ICMP,2)*ctlpos(icnn)

C Debug.
C          write(6,*) 'door width reset to ',WDTH,ctlpos(icnn)

        endif
      endif

      HGHT=SUPCMP(ICMP,3)
      DHRF=SUPCMP(ICMP,4)/SUPCMP(ICMP,3)
      CDIS=SUPCMP(ICMP,5)

      RGAS=287.035
      PATM=REAL(PRES(IDPS))+101325.
      TPOS=TNOD(IDPS)+273.15
      TNEG=TNOD(IDNE)+273.15

C If height or temperature difference very small, then treat door
C as ordinary air flow opening. If DELP > 0 , flow is from + to -.
      IF(HGHT.LT.0.01.OR.ABS(TPOS-TNEG).LT.0.01) THEN
        IF(DELP.GE.0) THEN
          FLW1(ICNN)=RHON(IDPS)*CDIS*WDTH*HGHT*DSQRT(2.*DELP)
        ELSE
          FLW1(ICNN)=-RHON(IDNE)*CDIS*WDTH*HGHT*DSQRT(2.*DABS(DELP))
        END IF
        FLW2(ICNN)=0.

        IF(DABS(DELP).GT.DBLE(SMALL)) THEN
          DERIV=0.5*FLW1(ICNN)/DELP
        ELSE
          CALL MFNPDV(ICNN,DELP,DERIV)
        END IF
        GOTO 100
      END IF
      
C Door flows calculation acc. Cockroft (1979).
C Door width (WDTH) will reflect control position.
      CT=9.81*PATM*HGHT/RGAS*((1./TNEG)-(1./TPOS))
      CA=CMPLX((1.-DHRF)*CT+DELP)
      CB=CMPLX(DELP-DHRF*CT)
      CQ=CMPLX(2./3.*CDIS*WDTH*HGHT*SQRT(4./(RHON(IDPS)+RHON(IDNE)))
     &   *(CA*CSQRT(CA)-CB*CSQRT(CB))/CMPLX(CT))
     
C This generates real and imaginary parts, indicating volume flow rate
C in +ve and -ve direction, respectively.
      IF(REAL(CQ).GE.0.0) THEN
        FLW1(ICNN)= RHON(IDPS)* REAL(CQ)
      ELSE
        FLW1(ICNN)= RHON(IDNE)* REAL(CQ)
      END IF
      IF(AIMAG(CQ).GE.0.0) THEN
        FLW2(ICNN)=-RHON(IDNE)*AIMAG(CQ)
      ELSE
        FLW2(ICNN)=-RHON(IDPS)*AIMAG(CQ)
      END IF

      CALL MFNPDV(ICNN,DELP,DERIV)

C If door is closed the derivative is set equal to zero.
  100 if((dabs(delp).gt.DBLE(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0  
            
      RETURN
      END

C ***************** MF210C 
C Fluid mass flow calculation routine for flow component type:
C general flow conduit (ie. duct or pipe).
C     IFLD = SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     DIAH = SUPCMP(ICMP,2) - conduit hydraulic diameter (m)
C     AREA = SUPCMP(ICMP,3) - cross-sectional area (m^2)
C     CLEN = SUPCMP(ICMP,4) - conduit length (m)
C            SUPCMP(ICMP,5) - absolute wall roughness (m)
C     SLDL = SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)

C     FLH                   - history mechanism mass flow rate (kg/s)
C     FRIC                  - friction factor (-)
C     FNU                   - fluid kinematic viscosity (m^2/s)
C     LNIT                  - local iteration counter
C     REY                   - Reynolds number (-)
C     RROU                  - relative (to diameter) wall roughness (-)
C     RHO                   - fluid density (kg/m^3)
C     VEL                   - velocity (m/s)

      SUBROUTINE MF210C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      DOUBLE PRECISION FLH
      character outs*124

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      IFLD=INT(SUPCMP(ICMP,1))
      DIAH=SUPCMP(ICMP,2)
      AREA=SUPCMP(ICMP,3)
      CLEN=SUPCMP(ICMP,4)
      RROU=SUPCMP(ICMP,5)/DIAH
      SLDL=SUPCMP(ICMP,6)

C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0) THEN
         RHO=RHON(IDPS)
         FNU=DYVISC(IFLD,TNOD(IDPS))/RHO
      ELSE
         RHO=RHON(IDNE)
         FNU=DYVISC(IFLD,TNOD(IDNE))/RHO
      END IF

C Begin mass flow rate calculation iteration (fixed point method).
C Velocity, Reynolds & friction based on the most recent flow rate;
C to avoid divergence problems: REY,min = 1.
      DO 10 LNIT=1,MAXITF
        VEL=REAL(DABS(FLW1(ICNN))/(RHO*AREA))
        FLH=FLW1(ICNN)
        REY=VEL*DIAH/FNU
        IF(REY.LT.1.) REY=1.

C Friction factor is a function of Reynolds dependent on type of flow;
C according to ISSO-17 1986, 'Air duct calculation', Rotterdam:
        IF(REY.LE.2300.0) THEN

C Laminar flow.
          FRIC=64./REY
        ELSE IF(REY.LE.3500.0) THEN

C Transition flow; linear interpolation between laminar and turbulent.
C ie. FRIC={FRIC@2300(3500-REY)+FRIC@3500(REY-2300)}/(3500-2300)
          FRIC=2.319E-5*(3500.-REY)+((REY-2300.)/1200.)*
     &        (1./(2.*ALOG10(RROU/3.72+3.679E-3)))**2.
        ELSE

CTurbulent flow; explicit approximation of Colebrook-White formula.
          FRIC=(1./(2.*ALOG10(RROU/3.72+5.74/REY**0.901)))**2.
        END IF

C Mass flow rate based on this friction factor.
        FLW1(ICNN)=AREA*DSQRT(2.*RHO*DABS(DELP)/(FRIC*CLEN/DIAH+SLDL))*
     &           ctlpos(icnn)

C Check if both new and old velocity close to zero, if so then
C we can forget about iteration.
        VLN=REAL(FLW1(ICNN)/(RHO*AREA))
        IF(VEL.LT.0.10.AND.VLN.LT.0.10) GOTO 20

C Check error <= tolerance (relative to initial mass flow rate (& REY)).
C This is not desirable in the  case where the velocity is small.
        IF(DABS(FLW1(ICNN)-FLH).LE.FERREL*DABS(FLH).OR.
     &     DABS(FLW1(ICNN)-FLH).LE.FERMFL) GOTO 20
   10 CONTINUE

      CALL DAYCLK(IYD,FLWTIM,ICOUT)
      call edisp(icout,' MF210C warning: max. number of iterations')
      WRITE(outs,'(A,I4,A,I4)') ' exceeded: ',LNIT,' connection', ICNN
      call edisp(icout,outs)

C Flow is now known; change sign if dP is -ve.
   20 IF(DELP.LT.0.0) FLW1(ICNN)=-FLW1(ICNN)

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=0.5*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C If duct is closed the derivative is set equal to zero.
      if((dabs(delp).gt.dble(small)).and.((dabs(flw1(icnn))+
     & dabs(flw2(icnn))).lt.dble(small))) deriv=0.0 

      RETURN
      END
    
C ***************** MF211C
C Cowl roof ventilator using a method after Armando Pinto based on
C experimental results of:
C Gonzalez M.A. On the aerodynamics of natural ventilators. London
C Pergamon Press, 1984, Building and environment. Vol 19 No 3.

C The paper mentions 7 different models of cowels tested and table
C one gives several of the coefficients.
C Model 1 coef B varies between 0.72 and 1.28 depending on direction
C         coef n varies between 0.51 and 0.26 depending on direction
C Model 2 coef B varies between 0.37 and 0.80 depending on direction
C         coef n varies between 0.31 and 0.40 depending on direction
C Model 3 coef B 0.25  coef n 0.16
C Model 4 coef B 0.12  coef n 0.17
C Model 5 coef B 0.37  coef n 0.10
C Model 6 coef B 0.21  coef n 0.19
C Model 7 coef B 0.28  coef n 0.45

C Fluid mass flow calculation routine for flow component type:
C Cowl Roof Outlet
C     FLUID =   SUPCMP(ICMP,1) - fluid type (1=air)
C     DIAH =    SUPCMP(ICMP,2) - Cowl hydraulic diameter (m)
C     K =       SUPCMP(ICMP,3) - local dynamic loss factors (-)
C     B =       SUPCMP(ICMP,4) - coefficient of cowl
C     n =       SUPCMP(ICMP,5) - coefficient of cowl
C     WINDEFF = SUPCMP(ICMP,6) - FLAG =1 CONSIDER WIND EFFECT, =0 DONT
C     LNIT                  - local iteration counter
C     RHO                   - fluid density (kg/m^3)

      SUBROUTINE MF211C(ICNN,DELP,DERIV)

#include "net_flow.h"

      PARAMETER (SMALL=1.0E-15)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

C Climate commons. 
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

C Kloss is the local loss dynamic factor.
C Bb is the 1st cowl coefficient, Nn is the 2nd cowl coefficient.
C Vi is average air flow velocity, ROE is fluid density (kg/m^3)
C AREA is ?
      DOUBLE PRECISION Kloss, Bb, Nn, AREA, Vi,PI,ROE, DPDV
      DOUBLE PRECISION DERIV1, vi0, vi1, viX, f0,f1,fx
      character outs*124
      logical close1,close2
      integer WINDEFF

C Local variables for start node of the connection (NODPS) the
C end node of the connection (NODNE) and the component type (ITPCON)
      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

C WINDEFF flag: 0 - no wind effect on cowl, 1 - consider wind effect. 
      DIAH=SUPCMP(ICMP,2)
      Kloss=SUPCMP(ICMP,3)
      Bb=SUPCMP(ICMP,4)
      Nn=SUPCMP(ICMP,5)
      WINDEFF=NINT(SUPCMP(ICMP,6))
      PI=ATAN(1.0)*4.0
      AREA=PI*DIAH**2/4. 
      ROE=DENSIT(1,DRYB)   
      
C If DELP > 0 , flow is +ve; ie +ve side to -ve side.
      IF(DELP.GE.0.) THEN
        RHO=RHON(IDPS)
      ELSE
        RHO=RHON(IDNE)
      ENDIF
      
C Use eclose to test if WSPD is zero.
      CALL ECLOSE(WSPD,0.0,0.001,close1)

C If we are considering wind effects AND the wind speed is not close
C to zero AND the flow direction is positive then iterate up to 200
C times to calculate the average air flow velocity Vi.
      if ((WINDEFF.EQ.1).AND.(.NOT.close1).and.(DELP.GT.0.0)) then
        Vi0=0.0
        Vi1=(DABS(DELP)*2./RHO/Kloss)**(0.5)
        DO 100 i = 1, 200
          ViX=(Vi0+Vi1)/2.
          f0=-DELP+Kloss/2.*RHO*Vi0**2+
     &       1./2.*RHO*(WSPD**2)*Bb*((RHO/ROE*Vi0**2/WSPD**2)**Nn)
          f1=-DELP+Kloss/2.*RHO*Vi1**2+
     &       1./2.*RHO*(WSPD**2)*Bb*((RHO/ROE*Vi1**2/WSPD**2)**Nn)
          fX=-DELP+Kloss/2.*RHO*ViX**2+
     &       1./2.*RHO*(WSPD**2)*Bb*((RHO/ROE*ViX**2/WSPD**2)**Nn)     
          if ((fx*f1).lt.0.0) then
            Vi0=ViX
          else
            Vi1=ViX
          endif
          if ((abs(Vi0-Vi1).lt.0.0001).and.
     &        (abs(fx).lt.0.0001)) goto 110
  100   CONTINUE
  
  110   CONTINUE     
        Vi=(Vi0+Vi1)/2.0     
      ELSE

C Do not take wind effects into account.
        Vi=(DABS(DELP)*2./RHO/Kloss)**(0.5)
      END IF
        
      FLW1(ICNN)=Vi*AREA*RHO

C Flow is now known; change sign if dP is -ve.
   20 IF(DELP.LT.0.0) FLW1(ICNN)=-FLW1(ICNN)

      FLW2(ICNN)=0.0

C Calculate DERIV1
      CALL MFNPDV(ICNN,DELP,DERIV1)
      If (DERIV1.lt.0.0) DERIV1=DBLE(0.1)

C Use eclose to test if Vi is zero.
      CALL ECLOSE(REAL(Vi),0.0,0.001,close2)
      
      DPDV=Kloss*RHO*Vi
      IF ((WINDEFF.eq.1).AND.(.NOT.close1).AND.(.NOT.close2)) then
        DPDV=DPDV+ROE*(WSPD**(2.-2.*Nn))*Bb*2.*Nn*(Vi**(2.*Nn-1.))
      end if

      IF ((DABS(DELP).GT.DBLE(SMALL)).AND.(DPDV.NE.0.0)) THEN
        DERIV=Area*RHO/DPDV
      ELSE
        DERIV=DERIV1
      ENDIF
          
      RETURN
      END

C **************** MF220C 
C Fluid mass flow calculation routine for flow component type:
C flow conduit ending in converging 3-leg junction
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water) -\
C            SUPCMP(ICMP,2) - conduit hydraulic diameter (m)\
C     AREA = SUPCMP(ICMP,3) - cross-sectional area (m^2)    |= type
C            SUPCMP(ICMP,4) - conduit length (m)            |  210
C            SUPCMP(ICMP,5) - absolute wall roughness (m)   /
C     SIGC = SUPCMP(ICMP,6) - local loss factors excl. junction (-)
C     ACOM = SUPCMP(ICMP,7) - cross-sectional area of common (m^2)
C     A0   = SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C     ""   =  "  "   "   "     "    "  "  "    "  "
C     A5   = SUPCMP(ICMP,13) - Ccp coefficient a5 (-)

C     IDNS           - connection level supplementary node number
C     ICNS           - connection referenced by IDNS
C     CCP            - local dynamic loss factor (-)
C     QP             - volume flow in current connection (m^3/s)
C     QPA            - volume flow in other entrance of junction (m^3/s)
C     QC             - volume flow in common section of junction (m^3/s)
C     QPQC           - ratio of Qp over Qc (-)
C     QPAQC          - ratio of Qp' over Qc (-)

      SUBROUTINE MF220C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      character outs*124

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,3)
      SIGC=SUPCMP(ICMP,6)
      ACOM=SUPCMP(ICMP,7)
      A0=SUPCMP(ICMP,8)
      A1=SUPCMP(ICMP,9)
      A2=SUPCMP(ICMP,10)
      A3=SUPCMP(ICMP,11)
      A4=SUPCMP(ICMP,12)
      A5=SUPCMP(ICMP,13)

C Find out which connection is referenced by IDNS.
      IDNS=NDSCNN(ICNN,1)
      ICNS=0
   10 ICNS=ICNS+1
      IF(IDNS.EQ.NODPS(ICNS).AND.IDNE.EQ.NODNE(ICNS)) GOTO 20
      IF(ICNS.LT.NCNN) GOTO 10
      STOP ' MF220C: unresolvable error regarding junction location'

C Check flow directions and if invalid skip Ccp calculation.
   20 IF(FLW1(ICNN).LE.0.OR.FLW1(ICNS).LT.0.) THEN
         CCP=0.
         CALL DAYCLK(IYD,FLWTIM,ICOUT)
         WRITE(outs,'(A,I2)') ' MF220C warning: connection ',ICNN
         call edisp(icout,outs)
         call edisp(icout,
     &   ' reverse flow; Ccp for junction reset to zero')
         GOTO 30
      END IF

C Start calculation of Ccp; first the volume flow(s) (ratios) based
C on most recent calculated mass flow rates.
      QP=REAL(FLW1(ICNN)/RHON(IDPS))
      QPA=REAL(FLW1(ICNS)/RHON(IDNS))
      QC=QP+QPA
      QPQC=QP/QC
      QPAQC=QPA/QC
      CCP=A0+A1*QPQC+A2*QPQC*QPQC+A3*QPAQC+A4*QPAQC*QPAQC+A5*QPQC*QPAQC

C Convert into Ccp referenced to velocity pressure in current connection.
      CCP=CCP*RHON(IDPS)/RHON(IDNE)*
     &    ((real(FLW1(ICNN)+FLW1(ICNS))/ACOM)/real(FLW1(ICNN)/AREA))**2

C Temporarily add Ccp to the conduit sum of local dynamic loss factors
C and proceed as if this component were a general flow conduit.
   30 SUPCMP(ICMP,6)=SIGC+CCP
      CALL MF210C(ICNN,DELP,DERIV)

C Reset the conduit local dynamic loss factors to original value.
      SUPCMP(ICMP,6)=SIGC

      RETURN
      END

C ****************** MF230C 
C Fluid mass flow calculation routine for flow component type:
C flow conduit starting in diverging 3-leg junction
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water) -\
C            SUPCMP(ICMP,2) - conduit hydraulic diameter (m)\
C     AREA = SUPCMP(ICMP,3) - cross-sectional area (m^2)    |= type
C            SUPCMP(ICMP,4) - conduit length (m)            |  210
C            SUPCMP(ICMP,5) - absolute wall roughness (m)   /
C     SIGC = SUPCMP(ICMP,6) - local loss factors excl. junction (-)
C     ACOM = SUPCMP(ICMP,7) - cross-sectional area of common (m^2)
C     A0   = SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C     ""   =  "  "   "   "     "    "  "  "    "  "
C     A5   = SUPCMP(ICMP,13) - Ccp coefficient a5 (-)
C
C     IDNS           - connection level supplementary node number
C     ICNS           - connection referenced by IDNS
C     CCP            - local dynamic loss factor (-)
C     QP             - volume flow in current connection (m^3/s)
C     QPA            - volume flow in other exit of junction (m^3/s)
C     QC             - volume flow in common section of junction (m^3/s)
C     QPQC           - ratio of Qp over Qc (-)
C     QPAQC          - ratio of Qp' over Qc (-)

      SUBROUTINE MF230C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      character outs*124

      IDPS=NODPS(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,3)
      SIGC=SUPCMP(ICMP,6)
      ACOM=SUPCMP(ICMP,7)
      A0=SUPCMP(ICMP,8)
      A1=SUPCMP(ICMP,9)
      A2=SUPCMP(ICMP,10)
      A3=SUPCMP(ICMP,11)
      A4=SUPCMP(ICMP,12)
      A5=SUPCMP(ICMP,13)

C Find out which connection is referenced by IDNS.
      IDNS=NDSCNN(ICNN,1)
      ICNS=0
   10 ICNS=ICNS+1
      IF(IDPS.EQ.NODPS(ICNS).AND.IDNS.EQ.NODNE(ICNS)) GOTO 20
      IF(ICNS.LT.NCNN) GOTO 10
      STOP ' MF230C: unresolvable error regarding junction location'

C Check flow directions and if invalid skip Ccp calculation.
   20 IF(FLW1(ICNN).LE.0.OR.FLW1(ICNS).LT.0.) THEN
         CCP=0.
         CALL DAYCLK(IYD,FLWTIM,ICOUT)
         WRITE(outs,'(A,I2)') ' MF230C warning: connection ',ICNN
         call edisp(icout,outs)
         call edisp(icout,
     &   ' reverse flow; Ccp for junction reset to zero')
         GOTO 30
      END IF

C Start calculation of Ccp; first the volume flow(s) (ratios) based
C on most recent calculated mass flow rates.
      QP=REAL(FLW1(ICNN)/RHON(IDPS))
      QPA=REAL(FLW1(ICNS)/RHON(IDPS))
      QC=QP+QPA
      QPQC=QP/QC
      QPAQC=QPA/QC
      CCP=A0+A1*QPQC+A2*QPQC*QPQC+A3*QPAQC+A4*QPAQC*QPAQC+A5*QPQC*QPAQC

C Convert into Ccp referenced to velocity pressure in current connection.
      CCP=CCP*((real(FLW1(ICNN)+FLW1(ICNS))/ACOM)/
     &          real(FLW1(ICNN)/AREA))**2

C Temporarily add Ccp to the conduit sum of local dynamic loss factors
C and proceed as if this component were a general flow conduit.
   30 SUPCMP(ICMP,6)=SIGC+CCP
      CALL MF210C(ICNN,DELP,DERIV)

C Reset the conduit local dynamic loss factors to original value.
      SUPCMP(ICMP,6)=SIGC

      RETURN
      END

C ******************* MF240C 
C Fluid mass flow calculation routine for flow component type:
C flow conduit ending in converging 4-leg junction
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water) -\
C            SUPCMP(ICMP,2) - conduit hydraulic diameter (m)\
C     AREA = SUPCMP(ICMP,3) - cross-sectional area (m^2)    |= type
C            SUPCMP(ICMP,4) - conduit length (m)            |  210
C            SUPCMP(ICMP,5) - absolute wall roughness (m)   /
C     SIGC = SUPCMP(ICMP,6) - local loss factors excl. junction (-)
C     ACOM = SUPCMP(ICMP,7) - cross-sectional area of common (m^2)
C     A0   = SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C     ""   =  "  "   "   "     "    "  "  "    "  "
C     A9   = SUPCMP(ICMP,17) - Ccp coefficient a9 (-)
C
C     IDNS1,IDNS2    - connection level supplementary node numbers
C     ICNS1,ICNS2    - connections referenced by IDNS1,2
C     CCP            - local dynamic loss factor (-)
C     QP             - volume flow in current connection (m^3/s)
C     QPA1,QPA2      - volume flows in other junction entrances (m^3/s)
C     QC             - volume flow in common section of junction (m^3/s)
C     QPQC           - ratio of Qp over Qc (-)
C     QPA1QC,QPA2QC  - ratio of Qp',Qp' over Qc (-)

      SUBROUTINE MF240C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      character outs*124

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,3)
      SIGC=SUPCMP(ICMP,6)
      ACOM=SUPCMP(ICMP,7)
      A0=SUPCMP(ICMP,8)
      A1=SUPCMP(ICMP,9)
      A2=SUPCMP(ICMP,10)
      A3=SUPCMP(ICMP,11)
      A4=SUPCMP(ICMP,12)
      A5=SUPCMP(ICMP,13)
      A6=SUPCMP(ICMP,14)
      A7=SUPCMP(ICMP,15)
      A8=SUPCMP(ICMP,16)
      A9=SUPCMP(ICMP,17)

C Find out which connections are referenced by IDNS1,IDNS2.
      IDNS1=NDSCNN(ICNN,1)
      IDNS2=NDSCNN(ICNN,2)
      ICNS1=0
   10 ICNS1=ICNS1+1
      IF(IDNS1.EQ.NODPS(ICNS1).AND.IDNE.EQ.NODNE(ICNS1)) GOTO 12
      IF(ICNS1.LT.NCNN) GOTO 10
      GOTO 15
   12 ICNS2=0
   14 ICNS2=ICNS2+1
      IF(IDNS2.EQ.NODPS(ICNS2).AND.IDNE.EQ.NODNE(ICNS2)) GOTO 20
      IF(ICNS2.LT.NCNN) GOTO 14
   15 STOP ' MF240C: unresolvable error regarding junction location'

C Check flow directions and if invalid skip Ccp calculation.
   20 IF(FLW1(ICNN).LE.0.OR.FLW1(ICNS1).LT.0..OR.FLW1(ICNS2).LT.0.) THEN
         CCP=0.
         CALL DAYCLK(IYD,FLWTIM,ICOUT)
         WRITE(outs,'(A,I2)') ' MF240C warning: connection ',ICNN
         call edisp(icout,outs)
         call edisp(icout,
     &   ' reverse flow; Ccp for junction reset to zero')
         GOTO 30
      END IF

C Start calculation of Ccp; first the volume flow(s) (ratios) based
C on most recent calculated mass flow rates
      QP=REAL(FLW1(ICNN)/RHON(IDPS))
      QPA1=REAL(FLW1(ICNS1)/RHON(IDNS1))
      QPA2=REAL(FLW1(ICNS2)/RHON(IDNS2))
      QC=QP+QPA1+QPA2
      QPQC=QP/QC
      QPA1QC=QPA1/QC
      QPA2QC=QPA2/QC
      CCP=A0+A1*QPQC+A2**QPQC*QPQC+
     &    A3*QPA1QC+A4*QPA1QC*QPA1QC+A5*QPQC*QPA1QC+
     &    A6*QPA2QC+A7*QPA2QC*QPA2QC+A8*QPQC*QPA2QC+A9*QPA1QC*QPA2QC

C Convert into Ccp referenced to velocity pressure in current connection
      CCP=CCP*RHON(IDPS)/RHON(IDNE)*
     &    ((real(FLW1(ICNN)+FLW1(ICNS1)+FLW1(ICNS2))/ACOM)/
     &     (real(FLW1(ICNN))/AREA))**2

C Temporarily add Ccp to the conduit sum of local dynamic loss factors
C and proceed as if this component were a general flow conduit
   30 SUPCMP(ICMP,6)=SIGC+CCP
      CALL MF210C(ICNN,DELP,DERIV)

C Reset the conduit local dynamic loss factors to original value
      SUPCMP(ICMP,6)=SIGC

      RETURN
      END

C ******************* MF250C 
C Fluid mass flow calculation routine for flow component type:
C flow conduit starting in diverging 4-leg junction
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water) -\
C            SUPCMP(ICMP,2) - conduit hydraulic diameter (m)\
C     AREA = SUPCMP(ICMP,3) - cross-sectional area (m^2)    |= type
C            SUPCMP(ICMP,4) - conduit length (m)            |  210
C            SUPCMP(ICMP,5) - absolute wall roughness (m)   /
C     SIGC = SUPCMP(ICMP,6) - local loss factors excl. junction (-)
C     ACOM = SUPCMP(ICMP,7) - cross-sectional area of common (m^2)
C     A0   = SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C     ""   =  "  "   "   "     "    "  "  "    "  "
C     A9   = SUPCMP(ICMP,17) - Ccp coefficient a9 (-)
C
C     IDNS1,IDNS2    - connection level supplementary node numbers
C     ICNS1,ICNS2    - connections referenced by IDNS1,2
C     CCP            - local dynamic loss factor (-)
C     QP             - volume flow in current connection (m^3/s)
C     QPA1,QPA2      - volume flows in other junction exits (m^3/s)
C     QC             - volume flow in common section of junction (m^3/s)
C     QPQC           - ratio of Qp over Qc (-)
C     QPA1QC,QPA2QC  - ratio of Qp',Qp" over Qc (-)

      SUBROUTINE MF250C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      character outs*124

      IDPS=NODPS(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,3)
      SIGC=SUPCMP(ICMP,6)
      ACOM=SUPCMP(ICMP,7)
      A0=SUPCMP(ICMP,8)
      A1=SUPCMP(ICMP,9)
      A2=SUPCMP(ICMP,10)
      A3=SUPCMP(ICMP,11)
      A4=SUPCMP(ICMP,12)
      A5=SUPCMP(ICMP,13)
      A6=SUPCMP(ICMP,14)
      A7=SUPCMP(ICMP,15)
      A8=SUPCMP(ICMP,16)
      A9=SUPCMP(ICMP,17)

C Find out which connections are referenced by IDNS1,IDNS2
      IDNS1=NDSCNN(ICNN,1)
      IDNS2=NDSCNN(ICNN,2)
      ICNS1=0
   10 ICNS1=ICNS1+1
      IF(IDPS.EQ.NODPS(ICNS1).AND.IDNS1.EQ.NODNE(ICNS1)) GOTO 12
      IF(ICNS1.LT.NCNN) GOTO 10
      GOTO 15
   12 ICNS2=0
   14 ICNS2=ICNS2+1
      IF(IDPS.EQ.NODPS(ICNS2).AND.IDNS2.EQ.NODNE(ICNS2)) GOTO 20
      IF(ICNS2.LT.NCNN) GOTO 14
   15 STOP ' MF250C: unresolvable error regarding junction location'

C Check flow directions and if invalid skip Ccp calculation
   20 IF(FLW1(ICNN).LE.0.OR.FLW1(ICNS1).LT.0..OR.FLW1(ICNS2).LT.0.) THEN
         CCP=0.
         CALL DAYCLK(IYD,FLWTIM,ICOUT)
         WRITE(outs,'(A,I2)') ' MF250C warning: connection ',ICNN
         call edisp(icout,outs)
         call edisp(icout,
     &   ' reverse flow; Ccp for junction reset to zero')
         GOTO 30
      END IF

C Start calculation of Ccp; first the volume flow(s) (ratios) based
C on most recent calculated mass flow rates
      QP=REAL(FLW1(ICNN)/RHON(IDPS))
      QPA1=REAL(FLW1(ICNS1)/RHON(IDPS))
      QPA2=REAL(FLW1(ICNS2)/RHON(IDPS))
      QC=QP+QPA1+QPA2
      QPQC=QP/QC
      QPA1QC=QPA1/QC
      QPA2QC=QPA2/QC
      CCP=A0+A1*QPQC+A2**QPQC*QPQC+
     &    A3*QPA1QC+A4*QPA1QC*QPA1QC+A5*QPQC*QPA1QC+
     &    A6*QPA2QC+A7*QPA2QC*QPA2QC+A8*QPQC*QPA2QC+A9*QPA1QC*QPA2QC

C Convert into Ccp referenced to velocity pressure in current connection
      CCP=CCP*
     &    ((real(FLW1(ICNN)+FLW1(ICNS1)+FLW1(ICNS2))/ACOM)/
     &     (real(FLW1(ICNN))/AREA))**2

C Temporarily add Ccp to the conduit sum of local dynamic loss factors
C and proceed as if this component were a general flow conduit
   30 SUPCMP(ICMP,6)=SIGC+CCP
      CALL MF210C(ICNN,DELP,DERIV)

C Reset the conduit local dynamic loss factors to original value
      SUPCMP(ICMP,6)=SIGC

      RETURN
      END

C ***************** MF310C 
C Fluid mass flow calculation routine for flow component type:
C general flow inducer (ie. pump or fan) described by:
C dP = a0 + a1.(m/rho) + a2.(m/rho)^2 + a3.(m/rho)^3 (Pa)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C            SUPCMP(ICMP,2) - lower polynomial validity limit (m^3/s)
C            SUPCMP(ICMP,3) - upper polynomial validity limit (m^3/s)
C     A0   = SUPCMP(ICMP,4) - fit coefficient a0 (Pa/(m^3/s)^0)
C            SUPCMP(ICMP,5) - fit coefficient a1 (Pa/(m^3/s)^1)
C            SUPCMP(ICMP,6) - fit coefficient a2 (Pa/(m^3/s)^2)
C            SUPCMP(ICMP,7) - fit coefficient a3 (Pa/(m^3/s)^3)
C
C     Solves m from           g(m)= C0 + C1.m + C2.m^2 + C3.m^3 = 0
C     C0 = a0 - (-dP)       - constant in g(m)=0 *
C     Ci = a,i/(rho^i)      - polynomial coefficients in g(m)=0
C     DX,DXOLD              - search interval length
C     DPFMAX                - pressure *loss* at FMAX (Pa)
C     DPFMIN                - pressure *loss* at FMIN (Pa)
C     FMAX                  - upper polynomial validity limit (kg/s)
C     FMIN                  - lower polynomial validity limit (kg/s)
C     GM,DG                 - g(m), g'(m)
C     LNIT                  - local iteration counter
C     MFL                   - most recent calculated mass flow (kg/s)
C     M1,GM1                - lower bound search interval m1 and g(m1)
C     M2,GM2                - upper bound search interval m2 and g(m2)
C     RHO                   - fluid density (kg/m^3)

      SUBROUTINE MF310C(ICNN,DELP,DERIV)

#include "net_flow.h"
      COMMON/SHOUT/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV
      DOUBLE PRECISION MFL,M1,M2,XL,XH,DX,DXOLD,GM1,GM2,GM,DG
      character outs*124
      logical close

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      A0=SUPCMP(ICMP,4)

C*Recall that this is a flow inducer where dP in the dP(m) relation
C is interpreted - by the user - as pressure rise instead of loss,
C so  if DELP < 0 , flow is +ve; ie +ve side to -ve side
C     if DELP > 0 , would idicate reverse flow (we wouldn't like that !)
      IF(DELP.LE.0) THEN
         RHO=RHON(IDPS)
      ELSE
         RHO=RHON(IDNE)
      END IF

C Set validity limits
      FMIN=RHO*SUPCMP(ICMP,2)
      FMAX=RHO*SUPCMP(ICMP,3)

      C0=A0+REAL(DELP)
      C1=SUPCMP(ICMP,5)/RHO
      C2=SUPCMP(ICMP,6)/(RHO**2)
      C3=SUPCMP(ICMP,7)/(RHO**3)

C Begin of mass flow rate calculation iteration based on a fail-safe
C combination of bisection method and Newton-Raphson method. This
C hybrid approach takes a bisection step whenever N-R would take the
C solution out of bounds, or whenever N-R is not reducing the size of
C the brackets (ie. search interval bounds) rapidly enough.
C See also: Press, W.H. et al. 1986. 'Numerical Recipes', pp. 258
C First initialize the brackets such that g(m1)*g(m2)<0
      MFL=FLW1(ICNN)
      IF(MFL.LT.FMIN) THEN
         M1=MFL
         M2=FMAX
      ELSE IF(MFL.GT.FMAX) THEN
         M1=FMIN
         M2=MFL
      ELSE
         M1=FMIN
         M2=FMAX
      END IF

C double search interval if there's no root in current one
      DO 10 IEXPAND=1,MAXITF
      GM1=C0+M1*(C1+M1*(C2+M1*C3))
      GM2=C0+M2*(C1+M2*(C2+M2*C3))
      IF(GM1*GM2.LE.0) GOTO 15
      IF(MOD(IEXPAND,2).EQ.1) THEN
         M2=M2+(FMAX-FMIN)
      ELSE
         M1=M1-(FMAX-FMIN)
      END IF
   10 CONTINUE

C Root could not be bracketed in MAXITF iterations !
C This probably means that there is no root at all for current DELP
C Set mass flow rate to m,min if DELP < dP @ m,min and
C                    to m,max if DELP > dP @ m,max
      DPFMIN=-A0-FMIN*(C1+FMIN*(C2+FMIN*C3))
      DPFMAX=-A0-FMAX*(C1+FMAX*(C2+FMAX*C3))
      IF(DELP.LE.DPFMIN) THEN
         MFL=FMIN
      ELSE IF(DELP.LE.DPFMAX) THEN
         MFL=FMAX
      ELSE
        call edisp(iuout,' MF310C: error on root bracketing !')
        call epwait
        call epagend
        STOP 
      END IF

C Give warning and then continue as if mass flow rate was found
      CALL DAYCLK(IYD,FLWTIM,ICOUT)
      call edisp(icout,' MF310C warning: could not bracket root;')
      write(outs,'(A,I4,A)')' mass flow set to ',MFL,' (kg/s) '
      call edisp(icout,outs)
      GOTO 30

C OK, root is bracketed. Now orient the search so that g(ml) < 0
   15 IF(GM1.LT.0.) THEN
         XL=M1
         XH=M2
      ELSE
         XL=M2
         XH=M1
      END IF

C First guess for root already done (ie. MFL = FLW1(ICNN)) so just
C initialize 'step-size before last' and the last step
      DXOLD=DABS(M2-M1)
      DX=DXOLD
      GM=C0+MFL*(C1+MFL*(C2+C3*MFL))
      DG=C1+MFL*(2*C2+3*C3*MFL)

C Now start loop over allowed iterations
      DO 20 LNIT=1,MAXITF

C bisect if N-R out of range or not decreasing fast enough
      IF((((MFL-XH)*DG-GM)*((MFL-XL)*DG-GM)).GE.0.
     &   .OR.DABS(2.*GM).GT.DABS(DXOLD*DG)) THEN
         DXOLD=DX
         DX=0.5*(XH-XL)
         MFL=XL+DX

C ready if change in root is negligible
         call eclose(REAL(XL),REAL(MFL),0.00001,close)
         IF(close) GOTO 30
C N-R step acceptable. Take it.
      ELSE
         DXOLD=DX
         DX=GM/DG
         TEMP=REAL(MFL)
         MFL=MFL-DX
         call eclose(TEMP,REAL(MFL),0.00001,close)
         IF(close) GOTO 30
      END IF

C Check convergence criteria
      IF(DABS(DX).LE.FERMFL.OR.DABS(DX).LE.FERREL*DABS(MFL)) GOTO 30
C Now re-evaluate the function and maintain the bracket on the root
      GM=C0+MFL*(C1+MFL*(C2+C3*MFL))
      DG=C1+MFL*(2*C2+3*C3*MFL)
      IF(GM.LT.0.) THEN
         XL=MFL
      ELSE
         XH=MFL
      END IF
   20 CONTINUE

      CALL DAYCLK(IYD,FLWTIM,ICOUT)
      WRITE(ICOUT,*) ' MF310C warning: max. number of iterations',
     &               ' exceeded: ',LNIT

C Flow is now known
   30 FLW1(ICNN)=MFL*ctlpos(ICNN)

      FLW2(ICNN)=0.
C Give warning if mass flow rate outside range of validity
      IF(FLW1(ICNN).LT.FMIN.OR.FLW1(ICNN).GT.FMAX) THEN
         CALL DAYCLK(IYD,FLWTIM,ICOUT)
         WRITE(ICOUT,*) ' MF310C warning: flow outside valid range = ',
     &                  FLW1(ICNN)/RHO,' (m^3/s)'
      END IF

C Calculate derivative for latest mass flow rate
      DG=C1+MFL*(2*C2+3*C3*MFL)
      IF(DABS(DG).GT.DBLE(SMALL)) THEN
         DERIV=1/DG
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

C And change sign because in the original function dP should be -dP
      DERIV=-DERIV

      RETURN
      END

C ***************** MF410C 
C Fluid mass flow calculation routine for flow component type:
C general flow corrector (ie. valve or damper)
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     STRHO =SUPCMP(ICMP,2) - standard density rho 0 (kg/m^3)
C     STDLP =SUPCMP(ICMP,3) - standard pressure difference dP0 (Pa)
C     ICHAR =SUPCMP(ICMP,4) - characteristic; 1=linear, 2=logartihmic
C     FLWS  =SUPCMP(ICMP,5) - volume flow when fully open k,vs (m^3/s)
C            SUPCMP(ICMP,6) - theoretical flow when closed, k,v0 (%)
C            SUPCMP(ICMP,7) - lower validity limit for charac., k,vr (%)
C
C     FLW             - normalized volume flow at HH100 (m^3/s) (ie. kv)
C     FLW0            - normalized volume flow at H0   (m^3/s) (ie. kv0)
C     FLWR            - normalized volume flow at H0   (m^3/s) (ie. kvr)
C     HR              - relative valve position H/H100 'at' FLWR (-)
C     HH100           - relative valve position H/H100 (-)

      SUBROUTINE MF410C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      STRHO=SUPCMP(ICMP,2)
      STDLP=SUPCMP(ICMP,3)
      ICHAR=INT(SUPCMP(ICMP,4))
      FLWS=SUPCMP(ICMP,5)
      FLW0=FLWS*SUPCMP(ICMP,6)/100.
      FLWR=FLWS*SUPCMP(ICMP,7)/100.

      HH100=CTLPOS(ICNN)

C Calculate the normalized volume flow rate through the valve/damper
C depending on the type of characteristic
      IF(ICHAR.EQ.1) THEN
         FLW=FLW0+(FLWS-FLW0)*HH100
      ELSE IF(ICHAR.EQ.2) THEN
         FLW=FLW0*EXP(LOG(FLWS/FLW0)*HH100)
      ELSE
        call edisp(iuout,' MF410C: error on valve characteristic.')
        call epwait
        call epagend
        STOP 
      END IF

C If flow < kvr , find HR and recalculate flow from linear interpolation
      IF(FLW.LT.FLWR) THEN
         IF(ICHAR.EQ.1) THEN
            HR=(FLWR-FLW0)/(FLWS-FLW0)
         ELSE IF(ICHAR.EQ.2) THEN
            HR=LOG(FLWR/FLW0)/LOG(FLWS/FLW0)
         END IF
         FLW=FLWR*HH100/HR
      END IF

C Calculate the mass flow rate for the actual pressure drop and density
C If DELP > 0 , flow is +ve; ie +ve side to -ve side
      IF(DELP.GE.0) THEN
         RHO=RHON(IDPS)
         FLW1(ICNN)=RHO*FLW*
     &              SQRT(DELP*STRHO/(STDLP*RHO))
      ELSE
         RHO=RHON(IDNE)
         FLW1(ICNN)=-RHO*FLW*
     &              SQRT(DABS(DELP)*STRHO/(STDLP*RHO))
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=0.5*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

      RETURN
      END

C ******************* MF420C
C Fluid mass flow calculation routine for flow component type:
C flow corrector (ie. valve or damper) with polynomial flow resistance
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     AREA  =SUPCMP(ICMP,2) - area of section cont. corrector (m^2)
C     A0    =SUPCMP(ICMP,3) - polynomial coefficient a0 (-)
C     A1    =SUPCMP(ICMP,4) - polynomial coefficient a1 (-)
C     A2    =SUPCMP(ICMP,5) - polynomial coefficient a2 (-)
C     A3    =SUPCMP(ICMP,6) - polynomial coefficient a3 (-)
C
C     COEFF           - coefficient for calculating zero resistance flow
C     HH100           - relative valve position H/H100 (-)
C     ZETA            - local loss coefficient due to controller (-)
C
      SUBROUTINE MF420C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      PARAMETER       (SMALL=1.0E-15)
      PARAMETER       (COEFF=1.0E+3)

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      AREA=SUPCMP(ICMP,2)
      A0=SUPCMP(ICMP,3)
      A1=SUPCMP(ICMP,4)
      A2=SUPCMP(ICMP,5)
      A3=SUPCMP(ICMP,6)

      HH100=ctlpos(ICNN)

C Calculate local loss factor for current relative valve position
      ZETA=A0+A1*HH100+A2*HH100*HH100+A3*HH100*HH100*HH100

C Calculate the mass flow rate for the actual pressure drop and density
C If DELP > 0 , flow is +ve; ie +ve side to -ve side
C If ZETA < SMALL then assume 'zero flow resistance' 
      IF(DELP.GE.0) THEN
         RHO=RHON(IDPS)
         IF(ZETA.GE.DBLE(SMALL)) THEN
            FLW1(ICNN)=AREA*SQRT(2.*RHO*DELP/ZETA)
         ELSE
            FLW1(ICNN)=RHO*COEFF*SQRT(DELP)
         END IF
      ELSE
         RHO=RHON(IDNE)
         IF(ZETA.GE.DBLE(SMALL)) THEN
            FLW1(ICNN)=-AREA*SQRT(2.*RHO*DABS(DELP)/ZETA)
         ELSE
            FLW1(ICNN)=-RHO*COEFF*SQRT(DABS(DELP))
         END IF
      END IF

      FLW2(ICNN)=0.

      IF(DABS(DELP).GT.DBLE(SMALL)) THEN
         DERIV=0.5*FLW1(ICNN)/DELP
      ELSE
         CALL MFNPDV(ICNN,DELP,DERIV)
      END IF

      RETURN
      END

C ***************** MF460C 
C Fluid mass flow calculation routine for flow component type:
C fixed flow rates controller
C            SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     FLWLOW=SUPCMP(ICMP,2) - flow rate when S <  Ssp (m^3/s)
C     FLWUP =SUPCMP(ICMP,3) - flow rate when S >= Ssp (m^3/s)
C
      SUBROUTINE MF460C(ICNN,DELP,DERIV)

#include "net_flow.h"

      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/mfctl/ctlpos(MCNN)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION DELP,DERIV

      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      ICMP=ITPCON(ICNN)

      FLWLOW=SUPCMP(ICMP,2)
      FLWUP=SUPCMP(ICMP,3)

C  determine lower or upper flow rate
      FLOW=(FLWUP-FLWLOW)*CTLPOS(ICNN)+FLWLOW

C If DELP > 0 , flow is +ve; ie +ve side to -ve side
      IF(DELP.GE.0) THEN
         FLW1(ICNN)=RHON(IDPS)*FLOW
      ELSE
         FLW1(ICNN)=RHON(IDNE)*FLOW
      END IF

      FLW2(ICNN)=0.

      DERIV=0.0

      RETURN
      END

C ***************** MFXSLV 
C This module contains the following subroutines:
C     JSOLVD
C     LUDCMP
C     LUBKSB

C The LUDCMP and LUBKSB subroutines are described in:
C Press, W.H. et al. 'Numerical Recipes. The Art of Scientific
C Computing', Cambridge University Press, Cambridge, 1988

C ******************* JSOLVD 
C JSOLVD solves the matrix equation {A} {X} = {B} based
C on Gaussian elimination with backsubstitution and no pivoting.
C An efficient matrix technique is used to forward reduce the
C N*N matrix A (physical size NP*NP) halfway, to a matrix whose
C components on the diagonal and above remain nontrivial.
C The solution vector X (size N) is then generated through
C backsubstitution of the known right hand side vector B (size N)

      SUBROUTINE JSOLVD(A,N,NP,X,B)

      DOUBLE PRECISION A(NP,NP),X(N),B(N)
      DOUBLE PRECISION AB,AC,AD

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

C **************** LUDCMP 
C Given an N x N matrix A, with physical dimension NP, this routine
C replaces it by the LU decomposition of a row-wise permutation of
C itself. A and N are input. On output A is the LU decomposition of
C itself; INDX is an output vector which records the row permutation
C effected by the partial pivoting; D is output as +/- 1 depending
C on whether the number of row interchanges was even or odd, respect.
C This routine is used in combination with LUBKSB to solve linear
C equations or invert a matrix.

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      COMMON/OUTIN/IUOUT,IUIN

      DOUBLE PRECISION A(NP,NP)
      DIMENSION        INDX(N)

      PARAMETER       (SMALL=1.0E-15)
      PARAMETER       (NMAX=100)
      DOUBLE PRECISION VV(NMAX),AAMAX,SUM,DUM
      logical close

      IF(N.GT.NMAX) STOP ' LUDCMP: local array VV too small !'

      D=1.0

C Loop over rows to get implicit scaling information.
      DO 12 I=1,N
        AAMAX=0.0
        DO 11 J=1,N
          IF(DABS(A(I,J)).GT.AAMAX) AAMAX=DABS(A(I,J))
   11   CONTINUE
        call eclose(REAL(AAMAX),0.00,0.00001,close)
        IF(close)then
          call edisp(iuout,'LUDCMP: Singular matrix !')
          call epwait
          call epagend
          STOP
        endif
        VV(I)=1.0/AAMAX
   12 CONTINUE

C Start loop over columns of Crout's method.
      DO 19 J=1,N
      DO 14 I=1,J-1
        SUM=A(I,J)
        DO 13 K=1,I-1
          SUM=SUM-A(I,K)*A(K,J)
   13   CONTINUE
        A(I,J)=SUM
   14 CONTINUE

C Start search for largest pivot element.
      AAMAX=0.0
      DO 16 I=J,N
        SUM=A(I,J)
        DO 15 K=1,J-1
          SUM=SUM-A(I,K)*A(K,J)
   15   CONTINUE
        A(I,J)=SUM
        DUM=VV(I)*DABS(SUM)
        IF(DUM.GE.AAMAX) THEN
          IMAX=I
          AAMAX=DUM
        END IF
   16 CONTINUE

C Perhaps interchange rows and scale factor.
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
      call eclose(REAL(A(J,J)),0.00,0.00001,close)
      IF(close) A(J,J)=DBLE(SMALL)
      IF(J.NE.N) THEN
         DUM=1.0/A(J,J)
         DO 18 I=J+1,N
           A(I,J)=A(I,J)*DUM
   18    CONTINUE
      END IF
   19 CONTINUE

      RETURN
      END

C ******************** LUBKSB 
C Solves the set of N linear equations {A}{x}={b}. Here A is input, not
C as the matrix A but rather as its LU decompositions, determined by
C the routine LUDCMP. INDX is input as the permutation vector returned
C by LUDCMP. B is input as the right-hand side vector B, and returns
C with the solution vector X. A, N, NP and INDX are not modified by
C this routine and can be left in place for successive calls with
C different right-hand sides B. This routine takes into account the
C possibility that B will begin with many zero elements, so it is
C efficient for use in matrix inversion.

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)

      DOUBLE PRECISION A(NP,NP),B(N)
      DIMENSION        INDX(N)

      DOUBLE PRECISION SUM
      logical close

C Start forward substitution.
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        call eclose(REAL(SUM),0.00,0.00001,close)
        IF(II.NE.0) THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
   11     CONTINUE
        ELSE IF(.NOT.close) THEN
          II=I
        END IF
        B(I)=SUM
   12 CONTINUE

C Start backsubstitution.
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N) THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
   13     CONTINUE
        END IF
        B(I)=SUM/A(I,I)
   14 CONTINUE

      RETURN
      END

C *************** MFOUTP 
C MFOUTP writes fluid flow simulation results to trace file.
C (the same trace file as used for other issues).
      SUBROUTINE MFOUTP(IFRES)

#include "net_flow.h"

      common/c6/indcfg
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PCTSTP/NTSTPP
      COMMON/PCTINC/IPTS

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact
      common/rdcmfs/irdcm,sFLWTIM,sDRYB,sWDIR,sWSPD,sITER,sIOK,
     &              sTNOD(MNOD)
      common/rdcmfsd/sPRES(MNOD),sRESID(MNOD),sSAFLW(MNOD),
     &              sPSTACK(MCNN),sFLW1(MCNN),sFLW2(MCNN)

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      DOUBLE PRECISION sFLW1,sFLW2,sPRES,sPSTACK,sRESID,sSAFLW
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12

C Once per hour results storage. Determine whether storage at this
C time step is needed; ie only during last plant time step in
C last building time step within current hour.
      if(irdact.gt.0) then
        if((indcfg.eq.0).or.
     &     (indcfg.eq.1.and.its.eq.ntstep).or.
     &     (indcfg.eq.2.and.ipts.eq.ntstpp).or.
     &     (indcfg.eq.3.and.its.eq.ntstep.and.ipts.eq.ntstpp)) then
          irdnow=1
        else
          irdnow=0
        endif
      endif


C Date, time, climate data, # of iterations and solution success flag
      if(irdact.eq.0) then
        WRITE(IFRES,1000) IRD,IRM,IRY,FLWTIM,DRYB,WDIR,WSPD,ITER,
     &    real(IOK)
      else
        sFLWTIM=sFLWTIM+FLWTIM
        sDRYB=sDRYB+DRYB
        sWDIR=sWDIR+WDIR
        sWSPD=sWSPD+WSPD
        sITER=sITER+real(ITER)
        sIOK=sIOK+real(IOK)
        if(irdnow.eq.1) then
          xx= real(irdcm)
          WRITE(IFRES,1000) IRD,IRM,IRY,sFLWTIM/xx,sDRYB/xx,
     &      sWDIR/xx,sWSPD/xx,nint(sITER/xx),sIOK/xx
          sFLWTIM=0.
          sDRYB=0.
          sWDIR=0.
          sWSPD=0.
          sITER=0.
          sIOK=0.
        endif
      endif
 1000 FORMAT(' Date: ',I2,'/',I2,'/',I4,1X,F6.3,2X,
     &       'Dryb=',F5.1,'  Wdir=',F5.0,'  Wvel=',F5.1,3X,'It=',I5,
     &       ' OK=',F4.2)

C Node name, fluid type, node type, temperature, height, pressure,
C mass flow residual and node coupled sum of absolute mass flow rates
      if(irdact.eq.0) then
        WRITE(IFRES,1010)
        WRITE(IFRES,1011)
     &     (NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),TNOD(INOD),
     &      HNOD(INOD),PRES(INOD),RESID(INOD),SAFLW(INOD),
     &      INOD=1,NNOD)
      else
        do 100 INOD=1,NNOD
          sTNOD(INOD)=sTNOD(INOD)+TNOD(INOD)
          sPRES(INOD)=sPRES(INOD)+PRES(INOD)
          sRESID(INOD)=sRESID(INOD)+RESID(INOD)
          sSAFLW(INOD)=sSAFLW(INOD)+SAFLW(INOD)
  100   continue
        if(irdnow.eq.1) then
          WRITE(IFRES,1010)
          WRITE(IFRES,1011)
     &     (NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),sTNOD(INOD)/irdcm,
     &     HNOD(INOD),sPRES(INOD)/irdcm,sRESID(INOD)/irdcm,
     &     sSAFLW(INOD)/irdcm,INOD=1,NNOD)
          do 102 INOD=1,NNOD
            sTNOD(INOD)=0.
            sRESID(INOD)=0.
            sSAFLW(INOD)=0.
  102     continue
        endif
      endif
 1010 FORMAT('    Node      F T  Temperature    Height   ',
     &       '   Pressure     Residual     Sabs(Flw)')
 1011 FORMAT(1X,A12,1X,I1,1X,I1,1X,G12.5,1X,G12.5,
     &       1X,G12.5,1X,G12.5,1X,G12.5)

C Connection node and component names, component type, fluid type,
C pressure difference across connection, stack pressure and mass flows
      if(irdact.eq.0) then
      WRITE(IFRES,1020)
      WRITE(IFRES,1021)
     &     (NDNAM(NODPS(ICNN)),NDNAM(NODNE(ICNN)),CMNAM(ITPCON(ICNN)),
     &      ITPCMP(ITPCON(ICNN)),NINT(SUPCMP(ITPCON(ICNN),1)),
     &      PRES(NODPS(ICNN))-PRES(NODNE(ICNN)),PSTACK(ICNN),
     &      FLW1(ICNN),FLW2(ICNN),
     &      ICNN=1,NCNN)
      else
        do 110 ICNN=1,NCNN
          sPSTACK(ICNN)=sPSTACK(ICNN)+PSTACK(ICNN)
          sFLW1(ICNN)=sFLW1(ICNN)+FLW1(ICNN)
          sFLW2(ICNN)=sFLW2(ICNN)+FLW2(ICNN)
  110   continue
        if(irdnow.eq.1) then
          WRITE(IFRES,1020)
          WRITE(IFRES,1021)
     &     (NDNAM(NODPS(ICNN)),NDNAM(NODNE(ICNN)),CMNAM(ITPCON(ICNN)),
     &      ITPCMP(ITPCON(ICNN)),NINT(SUPCMP(ITPCON(ICNN),1)),
     &      (sPRES(NODPS(ICNN))-sPRES(NODNE(ICNN)))/irdcm,
     &      sPSTACK(ICNN)/irdcm,sFLW1(ICNN)/irdcm,sFLW2(ICNN)/irdcm,
     &      ICNN=1,NCNN)
          do 112 INOD=1,NNOD
            sPRES(INOD)=0.
  112     continue
          do 114 ICNN=1,NCNN
            sPSTACK(ICNN)=0.
            sFLW1(ICNN)=0.
            sFLW2(ICNN)=0.
  114     continue
        endif
      endif
 1020 FORMAT('    From         To           Comp      Typ F',
     &       '     Pi-Pj       Pstack       Flow 1       Flow 2')
 1021 FORMAT(1X,A12,1X,A12,1X,A12,1X,I3,1X,I1,
     &       1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)

C Update storage counter if 'once per hour write' active.
      if(irdact.gt.0) then
         if(irdnow.eq.1) then
           irdcm=1
         else
           irdcm=irdcm+1
         endif
      endif

      RETURN
      END

C *************** MFOUTPH
C MFOUTP writes header for fluid flow simulation binary results file.

      SUBROUTINE MFOUTPH(IFRES)
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES2/IMFREC
      COMMON/MFLRESV/FLRESVER
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12,outs*124,CONTMNAM*12

      ISTAT=0

C Number of nodes, components and interconnections, version and contaminants
C Version index set to 2.0 in the first record
      FLRESVER=2.0
      IMFREC=1

C Debug.
C      write(6,*) IFRES

      IF(NOCNTM.NE.1)NCONTM=0
      write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) NNOD,NCMP,NCNN,
     &  FLRESVER,NCONTM

C Node data:
C Name, fluid, type, height, temp of other node, def temp, sup data 1 & 2.
      do 10 INOD=1,NNOD
        IMFREC=IMFREC+1
        write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)
     &       NDNAM(INOD),NDFLD(INOD),NDTYP(INOD),HNOD(INOD),
     &       ITND(INOD),TNOD(INOD),SUPNOD(INOD,1),SUPNOD(INOD,2)
 10   continue

C Component data:
C Name, Type, No of misc components data items, No of misc connection 
C data items, misc components data.
      do 20 ICMP=1,NCMP
        IMFREC=IMFREC+1
        write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &      CMNAM(ICMP),ITPCMP(ICMP),ISDCMP(ICMP),ISDCNN(ICMP),
     &      (SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
 20   continue

C Connections data:
C Pos node, Pos height, Neg node, Neg height, Component, sup node 1,2
      do 30 ICNN=1,NCNN
        IMFREC=IMFREC+1
        write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)
     &      NODPS(ICNN),HGTPS(ICNN),
     &      NODNE(ICNN),HGTNE(ICNN),
     &      ITPCON(ICNN),NDSCNN(ICNN,1),NDSCNN(ICNN,2)
 30   continue

C Contaminant data:
C Name
      if(NOCNTM.EQ.1)then
        do 40 ICONTM=1,NCONTM
          IMFREC=IMFREC+1
          WRITE(IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)CONTMNAM(ICONTM)
 40     continue
      endif

 50   return
 1000 write(outs,'(A,I5,A)')' MFOUTPH: error at record',IMFREC,'.'
      call edisp(iuout,outs)
      goto 50

      end

C *************** MFOUTPB 
C MFOUTP writes fluid flow simulation results to binary results file.

      SUBROUTINE MFOUTPB(IFRES)

#include "building.h"
#include "net_flow.h"

      common/c6/indcfg
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLRES2/IMFREC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM8/CCONC(MNOD,MCONTM)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PCTSTP/NTSTPP
      COMMON/PCTINC/IPTS

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact
      common/rdcmfs/irdcm,sFLWTIM,sDRYB,sWDIR,sWSPD,sITER,sIOK,
     &              sTNOD(MNOD)
      common/rdcmfsd/sPRES(MNOD),sRESID(MNOD),sSAFLW(MNOD),
     &              sPSTACK(MCNN),sFLW1(MCNN),sFLW2(MCNN)

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW
      DOUBLE PRECISION sFLW1,sFLW2,sPRES,sPSTACK,sRESID,sSAFLW
      CHARACTER outs*124,CONTMNAM*12
      logical dotrace

C If trace required set dotrace=.true.
      dotrace=.false.
      if(ITRACE(7).eq.1)dotrace=.true.

C Once per hour results storage. Determine whether storage at this
C time step is needed; ie only during last plant time step in
C last building time step within current hour.
      if(irdact.gt.0) then
        if((indcfg.eq.0).or.
     &     (indcfg.eq.1.and.its.eq.ntstep).or.
     &     (indcfg.eq.2.and.ipts.eq.ntstpp).or.
     &     (indcfg.eq.3.and.its.eq.ntstep.and.ipts.eq.ntstpp)) then
          irdnow=1
        else
          irdnow=0
        endif
      endif

C Node date: Pressure, Temperature.
      if(irdact.eq.0) then
        IMFREC=IMFREC+1
        write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &                                         (real(PRES(I)),I=1,NNOD)

C debug ascii output matching binary file data.
C        if(dotrace)write (106,*) IMFREC,(real(PRES(I)),I=1,NNOD)
        IMFREC=IMFREC+1
        write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &                                         (real(TNOD(I)),I=1,NNOD)
C debug ascii output matching binary file data.
C        if(dotrace)write (106,*) IMFREC,(real(TNOD(I)),I=1,NNOD)
      else
        do 100 INOD=1,NNOD
          sTNOD(INOD)=sTNOD(INOD)+TNOD(INOD)
          sPRES(INOD)=sPRES(INOD)+PRES(INOD)
  100   continue
        if(irdnow.eq.1) then
          IMFREC=IMFREC+1
          write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &                            (real(sPRES(I))/real(irdcm),I=1,NNOD)
          IMFREC=IMFREC+1
          write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &                            (real(sTNOD(I))/real(irdcm),I=1,NNOD)
          do 110 INOD=1,NNOD
            sTNOD(INOD)=0.
            sPRES(INOD)=0.
  110     continue
        endif
      endif

C write out contaminant data if contaminant simulation was invoked
      IF(NOCNTM.GT.0)THEN
        DO 300 ICONTM=1,NCONTM
          IMFREC=IMFREC+1
          write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000)
     &                            (real(CCONC(I,ICONTM)*1000),I=1,NNOD)

C Debug.
C          write(6,*) imfrec,(CCONC(I,ICONTM),I=1,nnod),nsinc

 300    CONTINUE
      ENDIF

C Connection node and component names, component type, fluid type,
C pressure difference across connection, stack pressure and mass flows
      if(irdact.eq.0) then
        do 200 IC=1,NCNN
          IMFREC=IMFREC+1
          write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &              real(PSTACK(IC)),real(FLW1(IC)),real(FLW2(IC)),
     &                   ' misc data e.g. control state'

C debug asci output matching binary data.
C          if(dotrace)then
C       write(106,*)IMFREC,real(PSTACK(IC)),real(FLW1(IC)),real(FLW2(IC))
C          endif
 200    continue
      else
        do 210 IC=1,NCNN
          sPSTACK(IC)=sPSTACK(IC)+PSTACK(IC)
          sFLW1(IC)=sFLW1(IC)+FLW1(IC)
          sFLW2(IC)=sFLW2(IC)+FLW2(IC)
 210    continue
        if(irdnow.eq.1) then
          do 220 IC=1,NCNN
            IMFREC=IMFREC+1
            write (IFRES,REC=IMFREC,IOSTAT=ISTAT,ERR=1000) 
     &                real(sPSTACK(IC))/real(irdcm),
     &          real(sFLW1(IC))/real(irdcm),real(sFLW2(IC))/real(irdcm),
     &                 ' misc data e.g. control state'
 220      continue
          do 230 IC=1,NCNN
            sPSTACK(IC)=0.
            sFLW1(IC)=0.
            sFLW2(IC)=0.
 230      continue
        endif
      endif

C Update storage counter if 'once per hour write' active.
      if(irdact.gt.0) then
        if(irdnow.eq.1) then
          irdcm=1
        else
          irdcm=irdcm+1
        endif
      endif

 40   return
 1000 write(outs,'(A,I5,A)')' MFOUTPB: error at record',IMFREC,'.'
      call edisp(iuout,outs)
      goto 40
      END
