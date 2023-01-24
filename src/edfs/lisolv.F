C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C lisolv1 - solve linear equations sweeping forward and backward
C           in y and z directions.
C lisolv2 - solve linear equations sweeping forward and backward
C           in x and z directions.
C lisolv3 - solve linear equations sweeping forward and backward
C           in x, y and z directions.
C lisolv4 - linear solver for calling biconjugate gradient method.
C           Sets up arrays for prosessing in solver routine NBAND7.
C Subroutines supporting lisolv4:
C PRPMATR - prepares all matrices to solve AY=F
C IND1D   - converts 3D array index to 1D
C IND3D   - converts 1D array index to 3D
C NBAND7  - Solver applying biconjugate gradient method.


C ******************** LISOLV1 ********************
C Solve the linear system of equations sweeping forward in y and z
C directions.

      SUBROUTINE LISOLV1(ISTART,JSTART,KSTART,NI,NJ,NK,PHI,NSWP)
#include "building.h"
#include "cfd.h"

      DIMENSION PHI(ntcelx,ntcely,ntcelz),A(mcel1d),B(mcel1d),C(mcel1d),
     &          D(mcel1d)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     &            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     &            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     &            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     &            SP(ntcelx,ntcely,ntcelz)     

      NIM1=NI-1
      NJM1=NJ-1
      NKM1=NK-1                                                    
      JSTM1=JSTART-1
      KSTM1=KSTART-1

C Loop for correct number of sweeps.
      do 5 ISWP=1,NSWP

C Commence W-E sweep.
      DO 100 I=ISTART,NIM1

        A(JSTM1)=0.0      
C Commence H-L sweep.
        DO 103 K=KSTART,NKM1
          C(JSTM1)=PHI(I,JSTM1,K)

C Commence S-N traverse.                                             
          DO 101 J=JSTART,NJM1

C Assemble TDMA coefficients.                                        
            A(J)=AN(I,J,K)
            B(J)=AS(I,J,K)
            C(J)=AE(I,J,K)*PHI(I+1,J,K)+AW(I,J,K)*PHI(i-1,j,K)+
     &           AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+SU(I,J,K)
            D(J)=AP(I,J,K)

C Calculate coefficients of recurrence formula.
            TERM=1./(D(J)-B(J)*A(J-1))
            A(J)=A(J)*TERM
  101       C(J)=(C(J)+B(J)*C(J-1))*TERM

C Obtain new PHI.                                               
          DO 102 JJ=JSTART,NJM1
            J=NJ+JSTM1-JJ
  102       PHI(i,j,K)=A(J)*PHI(I,J+1,K)+C(J)  
  103   CONTINUE

        A(KSTM1)=0.0

C Commence N-S sewwp.
        DO 104 J=JSTART,NJM1
          C(KSTM1)=PHI(I,J,KSTM1)

C Commence H-L traverse.                                             
          DO 105 K=KSTART,NKM1

C Assemble TDMA coefficients.                                        
            A(K)=AH(I,J,K)
            B(K)=AL(I,J,K)
            C(K)=AE(I,J,K)*PHI(I+1,J,K)+AW(I,J,K)*PHI(i-1,j,K)+
     &           AN(I,J,K)*PHI(I,J+1,K)+AS(I,J,K)*PHI(i,j-1,K)+SU(I,J,K)
            D(K)=AP(I,J,K)                                                      

C Calculate coefficients of recurrence formula.
            TERM=1./(D(K)-B(K)*A(K-1))
            A(K)=A(K)*TERM
  105       C(K)=(C(K)+B(K)*C(K-1))*TERM

C Obtain new PHI.                                               
          DO 106 KK=KSTART,NKM1
            K=NK+KSTM1-KK
  106       PHI(i,j,K)=A(K)*PHI(I,J,K+1)+C(K)
  104   CONTINUE

  100 CONTINUE
 5    continue

      RETURN                                                            
      END                                                               


C ******************** LISOLV2 ********************
C Solve the linear system of equations sweeping forward and backward in 
C x and z directions.

      SUBROUTINE LISOLV2(ISTART,JSTART,KSTART,IEND,JEND,KEND,PHI,NSWP)
#include "building.h"
#include "cfd.h"

      DIMENSION PHI(ntcelx,ntcely,ntcelz),A(mcel1d),B(mcel1d),C(mcel1d),
     &          D(mcel1d)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     &            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     &            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     &            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     &            SP(ntcelx,ntcely,ntcelz)     

      IENDM1=IEND-1
      JENDM1=JEND-1
      KENDM1=KEND-1

      JSTM1=JSTART-1

C Loop for correct number of sweeps.
      do 5 ISWP=1,NSWP

C Foward sweep 
C Commence W-E sweep.
      DO 100 I=ISTART,IENDM1

        A(JSTM1)=0.0

C Commence H-L sweep.
        DO 103 K=KSTART,KENDM1
          C(JSTM1)=PHI(I,JSTM1,K)

C Commence S-N traverse.
          DO 101 J=JSTART,JENDM1

C Assemble TDMA coefficients.
            A(J)=AN(I,J,K)
            B(J)=AS(I,J,K)
            C(J)=AE(I,J,K)*PHI(I+1,J,K)+AW(I,J,K)*PHI(i-1,j,K)+
     &           AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+SU(I,J,K)
            D(J)=AP(I,J,K)

C Calculate coefficients of recurrence formula.
            TERM=1./(D(J)-B(J)*A(J-1))
            A(J)=A(J)*TERM
            C(J)=(C(J)+B(J)*C(J-1))*TERM
  101     CONTINUE

C Obtain new PHI.
          DO 102 JJ=JSTART,JENDM1
            J=JEND+JSTM1-JJ
            PHI(i,j,K)=A(J)*PHI(I,J+1,K)+C(J)
  102     CONTINUE
  103   CONTINUE
  100 CONTINUE

C Backward sweep.
C Commence W-E sweep.
      DO 200 I=IENDM1,ISTART,-1

        A(JSTM1)=0.0

C Commence H-L sweep.
        DO 203 K=KENDM1,KSTART,-1
          C(JSTM1)=PHI(I,JSTM1,K)

C Commence S-N traverse.
          DO 201 J=JSTART,JENDM1

C Assemble TDMA coefficients.
            A(J)=AN(I,J,K)
            B(J)=AS(I,J,K)
            C(J)=AE(I,J,K)*PHI(I+1,J,K)+AW(I,J,K)*PHI(i-1,j,K)+
     &           AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+SU(I,J,K)
            D(J)=AP(I,J,K)

C Calculate coefficients of recurrence formula.
            TERM=1./(D(J)-B(J)*A(J-1))
            A(J)=A(J)*TERM
            C(J)=(C(J)+B(J)*C(J-1))*TERM
  201     continue

C Obtain new PHI.                                                 
          DO 202 JJ=JSTART,JENDM1
            J=JEND+JSTM1-JJ
            PHI(i,j,K)=A(J)*PHI(I,J+1,K)+C(J)
  202     CONTINUE
  203   CONTINUE
  200 CONTINUE
 5    continue

      RETURN
      END


C ******************** LISOLV3 ********************
C Solve the linear system of equations sweeping forward and backward in 
C x, y and z directions.

      SUBROUTINE LISOLV3(ISTART,JSTART,KSTART,IEND,JEND,KEND,PHI,NSWP)
#include "building.h"
#include "cfd.h"

      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     &            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     &            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     &            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     &            SP(ntcelx,ntcely,ntcelz)

      dimension A(mcel1d),B(mcel1d),C(mcel1d),D(mcel1d)
      dimension PHI(ntcelx,ntcely,ntcelz)

      COMMON/CONST/GREAT,small,GRAV
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     &              IZEROT,IZanKE,IMITZ

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ

      IENDM1=IEND-1
      JENDM1=JEND-1
      KENDM1=KEND-1

      JSTM1=JSTART-1
      ISTM1=ISTART-1

C Loop for correct number of sweeps.
      do 5 ISWP=1,NSWP

      IF(INCALU) THEN

C Forward sweep.
C Commence S->N sweep.
      DO 300 J=JSTART,JENDM1
        A(ISTM1)=0.0

C Commence L->H sweep.
        DO 303 K=KSTART,KENDM1
          C(ISTM1)=PHI(ISTM1,J,K)

C Commence W-E traverse.
          DO 301 I=ISTART,IENDM1

C Assemble TDMA coefficients.
            A(I)=AE(I,J,K)
            B(I)=AW(I,J,K)
            C(I)=AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+
     &           AN(I,J,K)*PHI(I,J+1,K)+AS(I,J,K)*PHI(i,j-1,K)+SU(I,J,K)
            D(I)=AP(I,J,K)

C Calculate coefficients of recurrence formula.
            TE=(D(I)-B(I)*A(I-1))
            TERM=ANOTZERO(TE)
            A(I)=A(I)/TERM
            C(I)=(C(I)+B(I)*C(I-1))/TERM
  301     continue

C Obtain new PHI.
          DO 302 II=ISTART,IENDM1
            I=IEND+ISTM1-II
            PHI(i,j,K)=A(I)*PHI(I+1,J,K)+C(I)
  302     CONTINUE
  303   CONTINUE
  300 CONTINUE

C Backward sweep.
C Commence S->N sweep.
      DO 400 J=JENDM1,JSTART,-1

        A(ISTM1)=0.0

C Commence L->H sweep.
        DO 403 K=KENDM1,KSTART,-1
          C(ISTM1)=PHI(ISTM1,J,K)

C Commence W-E TRAVERSE.
          DO 401 I=ISTART,IENDM1

C Assemble TDMA coefficients.
            A(I)=AE(I,J,K)
            B(I)=AW(I,J,K)
            C(I)=AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+
     &           AN(I,J,K)*PHI(I,J+1,K)+AS(I,J,K)*PHI(i,j-1,K)+SU(I,J,K)
            D(I)=AP(I,J,K)

C Calculate coefficients of recurrence formula.
            TE=(D(I)-B(I)*A(I-1))
            TERM=ANOTZERO(TE)
            A(I)=A(I)/TERM
            C(I)=(C(I)+B(I)*C(I-1))/TERM
  401     continue

C Obtain new PHI.
          DO 402 II=ISTART,IENDM1
            I=IEND+ISTM1-II
            PHI(i,j,K)=A(I)*PHI(I+1,J,K)+C(I)
  402     CONTINUE
  403   CONTINUE
  400 CONTINUE
      ENDIF

      IF(INCALV) THEN

C Foward sweep.
C Commence W->E sweep.
      DO 100 I=ISTART,IENDM1

C Commence L->H sweep.
        A(JSTM1)=0.0
        DO 103 K=KSTART,KENDM1
          C(JSTM1)=PHI(I,JSTM1,K)

C Commence S-N TRAVERSE.
          DO 101 J=JSTART,JENDM1

C Assemble TDMA coefficients.
            A(J)=AN(I,J,K)
            B(J)=AS(I,J,K)
            C(J)=AE(I,J,K)*PHI(I+1,J,K)+AW(I,J,K)*PHI(i-1,j,K)+
     &           AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+SU(I,J,K)
            D(J)=AP(I,J,K)

C Calculate coefficient of recurrence formula.
            TE=(D(J)-B(J)*A(J-1))
            TERM=ANOTZERO(TE)
            A(J)=A(J)/TERM
            C(J)=(C(J)+B(J)*C(J-1))/TERM
  101     continue

C Obtain new PHI.
          DO 102 JJ=JSTART,JENDM1
            J=JEND+JSTM1-JJ
            PHI(i,j,K)=A(J)*PHI(I,J+1,K)+C(J)
  102     CONTINUE
  103   CONTINUE
  100 CONTINUE

C Backward sweep.
C Commence E->W sweep.
      DO 200 I=IENDM1,ISTART,-1

C Commence H->L sweep.
        A(JSTM1)=0.0
        DO 203 K=KENDM1,KSTART,-1
          C(JSTM1)=PHI(I,JSTM1,K)

C Commence S-N traverse.
          DO 201 J=JSTART,JENDM1

C Assemble TDMA coefficients.
            A(J)=AN(I,J,K)
            B(J)=AS(I,J,K)
            C(J)=AE(I,J,K)*PHI(I+1,J,K)+AW(I,J,K)*PHI(i-1,j,K)+
     &           AH(I,J,K)*PHI(I,J,K+1)+AL(I,J,K)*PHI(i,j,K-1)+SU(I,J,K)
            D(J)=AP(I,J,K)

C Calculate coefficients of recurrence formula.
            TE=(D(J)-B(J)*A(J-1))
            TERM=ANOTZERO(TE)
            A(J)=A(J)/TERM
            C(J)=(C(J)+B(J)*C(J-1))/TERM
  201     continue

C Obtain new PHI.
          DO 202 JJ=JSTART,JENDM1
            J=JEND+JSTM1-JJ
            PHI(i,j,K)=A(J)*PHI(I,J+1,K)+C(J)
  202     CONTINUE
  203   CONTINUE
  200 CONTINUE
      ENDIF
 5    continue

      RETURN
      END


C ******************** PRPMATR ********************
C Pepares all matrices to solve AY=F filling the data
C from the discretisation coefficient matrices.
C Prepares 1D-matrx A for solving AY=F by NBAND7
C  Y: initial approx. vector & final solution vector
C  F: free members vector      
C  PHI: transport variable (to be calculated) 3D-array
C The  discretisation coefficients are in 3D arrays
C AW, AE, AS, AN, AL, AH, AP and SU.

      SUBROUTINE PRPMATR(ISTART,IEND,JSTART,JEND,KSTART,KEND,A,Y,F,PHI)
#include "building.h"
#include "cfd.h"

      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     &            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     &            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     &            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     &            SP(ntcelx,ntcely,ntcelz)

      COMMON/NB7/IDEFAULT,I1L,I1R,I2L,I2R,I3L,I3R,IFREE

      DIMENSION A(12*ntcelx*ntcely*ntcelz)
      DIMENSION Y(ntcelx*ntcely*ntcelz)
      DIMENSION F(ntcelx*ntcely*ntcelz)
      DIMENSION PHI(ntcelx,ntcely,ntcelz)

C All nodes of the matrix are assumed to be internal.
      DO 10 K=KSTART,KEND
        DO 10 J=JSTART,JEND
           I=ISTART
           SU(I,J,K)=SU(I,J,K)+AW(I,J,K)*PHI(I-1,J,K)
           AW(I,J,K)=0.0
           I=IEND
           SU(I,J,K)=SU(I,J,K)+AE(I,J,K)*PHI(I+1,J,K)
           AE(I,J,K)=0.0
 10   CONTINUE

      DO 20 K=KSTART,KEND
        DO 20 I=ISTART,IEND
           J=JSTART
           SU(I,J,K)=SU(I,J,K)+AS(I,J,K)*PHI(I,J-1,K)
           AS(I,J,K)=0.0
           J=JEND
           SU(I,J,K)=SU(I,J,K)+AN(I,J,K)*PHI(I,J+1,K)
           AN(I,J,K)=0.0
 20   CONTINUE

      DO 30 J=JSTART,JEND
        DO 30 I=ISTART,IEND
           K=KSTART
           SU(I,J,K)=SU(I,J,K)+AL(I,J,K)*PHI(I,J,K-1)
           AL(I,J,K)=0.0
           K=KEND
           SU(I,J,K)=SU(I,J,K)+AH(I,J,K)*PHI(I,J,K+1)
           AH(I,J,K)=0.0
 30   CONTINUE

C Find the span of indices for all space directions.
      ISPAN=IEND-ISTART+1
      JSPAN=JEND-JSTART+1
      KSPAN=KEND-KSTART+1

      NumPtsTtl=ISPAN*JSPAN*KSPAN

C Distances from main diagonal to other diagonals (1,2,3 and
C left and right).
      I1L=NumPtsTtl
      I1R=2*NumPtsTtl
      I2L=3*NumPtsTtl
      I2R=4*NumPtsTtl
      I3L=5*NumPtsTtl
      I3R=6*NumPtsTtl
C      IG=7*NumPtsTtl

      DO 100 K=KSTART,KEND
        DO 101 J=JSTART,JEND
          DO 102 I=ISTART,IEND 
            IPNT=IND1D(ISTART,IEND,JSTART,JEND,KSTART,KEND,I,J,K) 
            A(IPNT)=AP(I,J,K)
            A(I1L+IPNT)=AW(I,J,K)
            A(I1R+IPNT)=AE(I,J,K)
            A(I2L+IPNT)=AS(I,J,K)
            A(I2R+IPNT)=AN(I,J,K)
            A(I3L+IPNT)=AL(I,J,K)
            A(I3R+IPNT)=AH(I,J,K)

C RHS vector.
            F(IPNT)=SU(I,J,K)

C Initial approximation for solution vector.
            Y(IPNT)=PHI(I,J,K)
 102      CONTINUE
 101    CONTINUE
 100  CONTINUE

      return
      END


C ******************** IND1D ********************
C Converts 3D array index to 1D.

      INTEGER FUNCTION IND1D(ISTART,IEND,JSTART,JEND,KSTART,KEND,I,J,K)
      ISPAN=IEND-ISTART+1
      JSPAN=JEND-JSTART+1
C      KSPAN=KEND-KSTART+1
      IND1D=(K-KSTART)*ISPAN*JSPAN+(J-JSTART)*ISPAN+(I-ISTART+1)
      return
      END


C ******************** IND3D ********************
C Converts 1D array index to 3D.

C      SUBROUTINE IND3D(ISTART,IEND,JSTART,JEND,KSTART,KEND,IND,I,J,K)
C #include "cfd.h"
c      ISPAN=IEND-ISTART+1
c      JSPAN=JEND-JSTART+1
c      KSPAN=KEND-KSTART+1
c      I=MOD(IND,ISPAN)
c      IF(I.EQ.0) THEN
c        I=ISPAN
c      ENDIF
c      J=MOD(((IND-I)/ISPAN),JSPAN)+1
c      K=(IND-I-(J-1)*ISPAN)/(ISPAN*JSPAN)+1
c      I=ISTART+I-1
c      J=JSTART+J-1
c      K=KSTART+K-1
C      return
c      END


C ******************** LISOLV4 ********************
C Linear solver for calling biconjugate gradient method.  Sets 
C up arrays for prosessing in solver routine NBAND7.

      SUBROUTINE LISOLV4(ISTART,JSTART,KSTART,IEND,JEND,KEND,PHI,
     &                                                     NITMXN)
#include "building.h"
#include "cfd.h"

      COMMON/NB7/IDEFAULT,I1L,I1R,I2L,I2R,I3L,I3R,IFREE

      DIMENSION PHI(ntcelx,ntcely,ntcelz)
      DIMENSION A(12*ntcelx*ntcely*ntcelz)      
      DIMENSION Y(ntcelx*ntcely*ntcelz)
      DIMENSION F(ntcelx*ntcely*ntcelz)

C IDEFAULT=1 means I1L,I1R... to be calculated outside nband7, i.e. in these 
C procedures here but no problems if IDEFAULT=0 (do it this way!) simply 
C these values will be calculated twice: once here and twice in nband7.
      IDEFAULT=0
      ISPAN=IEND-ISTART+1
      JSPAN=JEND-JSTART+1
      KSPAN=KEND-KSTART+1

C The size of the matrices Y and F.
      N=ISPAN*JSPAN*KSPAN

C Calculate band distances.
C  M: between main diag and further right.
C  L: between main diag and middle right.
      I=ISTART+1
      J=JSTART+1
      K=KSTART+1
      IND1=IND1D(ISTART,IEND,JSTART,JEND,KSTART,KEND,I,J,K+1)
      IND=IND1D(ISTART,IEND,JSTART,JEND,KSTART,KEND,I,J,K)
      M=IND1-IND
      IND1=IND1D(ISTART,IEND,JSTART,JEND,KSTART,KEND,I,J+1,K)
      L=IND1-IND
      CALL PRPMATR(ISTART,IEND,JSTART,JEND,KSTART,KEND,A,Y,F,PHI)

C Set nband7 parameters.
      IREPTN=0

C EPSR originally= 1E-6, EPSA originally= 1E-8
      EPSR=1.0E-09
      EPSA=1.0E-10
      EPSNIT=0

C Finished setting up arrays - now solve by calling nband7.
      CALL NBAND7(N,M,L,A,Y,F,EPSR,EPSA,EPSNIT,NIT,IREPTN,NITMXN)

C Transfering solution from Y to PHI
      DO 10 K=KSTART, KEND
        DO 11 J=JSTART,JEND
          DO 12 I=ISTART,IEND 
            IPNT=IND1D(ISTART,IEND,JSTART,JEND,KSTART,KEND,I,J,K)
            PHI(I,J,K)=Y(IPNT)
  12      CONTINUE
  11    CONTINUE
  10  CONTINUE

      return
      END


C ******************** NBAND7 ********************
C Solver applying biconjugate gradient method.
C Single precision version.

      SUBROUTINE NBAND7(N,M,L,A,Y,F,EPSR,EPSA,epsnit,nit,IREPTN,NITMXN)
#include "building.h"
#include "cfd.h"

      COMMON/NB7/IDEFAULT,I1L,I1R,I2L,I2R,I3L,I3R,IFREE

      DIMENSION A(12*ntcelx*ntcely*ntcelz) 
      DIMENSION Y(ntcelx*ntcely*ntcelz) 
      DIMENSION F(ntcelx*ntcely*ntcelz)

      IF (IDEFAULT.EQ.0) THEN
        I1L = N
        I1R = 2*N
        I2L = 3*N
        I2R = 4*N
        I3L = 5*N
        I3R = 6*N
        IG  = 7*N
      ELSEIF(IDEFAULT.NE.0) THEN
        IG = IFREE + M
      ENDIF

C Debug.
C Dump A matrix
C      WRITE(6,'(a)')'dump A'
C      WRITE(6,201) N,L,M,I1L,I1R,I2L,I2R,I3L,I3R,IG
C 201  FORMAT('N:',I3,' L:',I2,' M:',I2,' I1L:',I3,' I1R:',I3,
C     &           ' I2L:',I3,' I2R:',I3,' I3L:',I3,' I3R:',I3,' IG:',I3)
C      DO 5 I=1,N
C        WRITE(6,202) I,A(I),I1L+I,A(I1L+I),I1R+I,A(I1R+I),
C     &               I2L+I,A(I2L+I),I2R+I,A(I2R+I),
C     &               I3L+I,A(I3L+I),I3R+I,A(I3R+I),
C     &               IG+I,A(IG+I),I,F(I)    
C   5  CONTINUE 
C 202  FORMAT(8(I3,':',F7.4,' '),' F[',I3,']:',F7.4)
C      WRITE(6,'(a)') 'END DUMP A'

      IT = IG + N
      ITM = IT + M
      IQ = ITM + N
      IP = IQ + N
      IF (IREPTN.EQ.0) THEN
        A(I1L+1) = 0.E0
        A(I1R+N) = 0.E0
        DO 8 J = 1,L
          A(I2L+J) = 0.E0
          A(I2R+J+N-L) = 0.E0
  8     CONTINUE
        DO 9 J = 1,M
          A(I3L+J) = 0.E0
          A(I3R+J+N-M) = 0.E0
  9     CONTINUE
        DO 10 J = 1,N

C Original code improved by next line.
C          G = A(J) - A(I1L+J)*A(IT+J-1)
          G = A(J) - A(I1L+J)*A(IT+J-1) + 0.01E0*A(J) - 
     &        A(I2L+J)*A(IT+J-L) - A(I3L+J)*A(IT+J-M)
          G = 1.E0 / G
          A(IG+J) = SQRT(G)              
          A(IT+J) = G*( A(I1R+J) + A(I2R+J) + A(I3R+J))
  10    CONTINUE
        DO 20 J = 1,N
          A(J) = A(IG+J)*A(J)*A(IG+J) - 2.E0
          A(I1L+J) = A(IG+J)*A(I1L+J)*A(IG+J-1)
          A(I1R+J) = A(IG+J)*A(I1R+J)*A(IG+J+1)
          A(I2L+J) = A(IG+J)*A(I2L+J)*A(IG+J-L)
          A(I2R+J) = A(IG+J)*A(I2R+J)*A(IG+J+L)
          A(I3L+J) = A(IG+J)*A(I3L+J)*A(IG+J-M)
          A(I3R+J) = A(IG+J)*A(I3R+J)*A(IG+J+M)
  20    CONTINUE
        IREPTN = 1
      ENDIF
      DO 30 J = N, 1, -1
        A(ITM+J) = Y(J) / A(IG+J)
        Y(J) = A(ITM+J) - A(I1R+J)*A(ITM+J+1) - 
     &         A(I2R+J)*A(ITM+J+L) - A(I3R+J)*A(ITM+J+M)
  30  CONTINUE
      RRO = 0.E0
      DO 40 J = 1,N
        A(IT+J) = F(J)*A(IG+J) - A(J)*A(ITM+J) - Y(J) + 
     &            A(I1L+J)*A(IT+J-1) + A(I2L+J)*A(IT+J-L) +
     &            A(I3L+J)*A(IT+J-M)
        F(J) = A(IT+J) - A(ITM+J)
        RRO = RRO + F(J)*F(J)
  40  CONTINUE
      NIT = 0
      EPSNIT = 1.E0
      RDROP = 0.E0
      RR = RRO
      QQI = 0.E0

  50  RROLD = RR

C Debug.
C      WRITE(6,110) NIT, EPSNIT, RRO, RR
C 110  FORMAT(' NBAND7: NIT= ', I4, ' EPSNIT= ', F12.4,
C     &                  ' RRO= ', F9.5, ' RR=', F9.5)

C A minimum number of inner iterations (NIT.LT.1) is
C required (at least for PP) to achieve convergence at all.
C A minimum number of 2 iterations (NIT.LT.2) improves the 
C behaviour when approaching convergence.
      IF (EPSNIT.GE.EPSR.AND.RR.GE.EPSA.AND.(1.E0-RDROP).GE.1.E-6.AND.
     &            NIT.LT.NITMXN.OR.NIT.LT.2) THEN
        DO 60 J = N, 1, -1
          A(ITM+J) = F(J) + A(I1R+J)*A(ITM+J+1) + 
     &             A(I2R+J)*A(ITM+J+L) + A(I3R+J)*A(ITM+J+M)
  60    CONTINUE
        TQ = 0.E0
        DO 70 J = 1, N
          G = A(ITM+J)
          A(ITM+J) = F(J) + A(J)*G + A(I1L+J)*A(ITM+J-1) + 
     &               A(I2L+J)*A(ITM+J-L) + A(I3L+J)*A(ITM+J-M)
          A(IT+J) = G + A(ITM+J)
          TQ = TQ + A(IT+J)*A(IQ+J)
  70    CONTINUE
        BK = TQ*QQI
        RQ = 0.E0
        QQ = 0.E0
        DO 80 J = 1,N
          A(IQ+J) = A(IT+J) - BK*A(IQ+J)
          A(IP+J) = F(J) - BK*A(IP+J)
          RQ = RQ + F(J) * A(IQ+J)
          QQ = QQ +A(IQ+J)*A(IQ+J)
  80    CONTINUE

C Add 1.E-20 to prevent division by zero (user doesn't need to know
C and has no effect on solution).
        QQI = 1.E0 / (QQ+1.E-20)
        AK = RQ * QQI
        RR = 0.E0
        DO 90 J = 1,N
          Y(J) = Y(J) + AK*A(IP+J)
          F(J) = F(J) - AK*A(IQ+J)
          RR = RR + F(J)*F(J)
  90    CONTINUE
        NIT = NIT + 1
        EPSNIT = SQRT( RR/RRO )      
        RDROP = SQRT( RR/RROLD )
        GOTO 50
      ENDIF

      DO 100 J = N, 1, -1
        A(ITM+J) = Y(J) + A(I1R+J)*A(ITM+J+1) + 
     &             A(I2R+J)*A(ITM+J+L) + A(I3R+J)*A(ITM+J+M)
        Y(J) = A(IG+J)*A(ITM+J)
 100  CONTINUE

      RETURN
      END
