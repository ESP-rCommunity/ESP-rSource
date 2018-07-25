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

C ************************************************************************************
C The maths routines containes in this files are used in the solution of the
C electrical network. 
C The routines are as follows: 
C CINVRLU - Subroutine to invert a matrix through LU decomposition. The
C           routine is taken from Fortran 77 by Donald M Munro, Edward 
C           Arnold & Sons.
C
C CFACTLU - Subroutine to factorise a complex N x N matrix
C           into upper and lower triangular elements. The
C           diagonal elements U are unity and are not stored.
C
C ************** CINVRLU
C Subroutine to invery a matrix through LU decomposition. The
C routine is taken from Fortran 77 by Donald M Munro, Edward 
C Arnold & Sons.
      SUBROUTINE CINVRLU(A,B,N)

C A is a matrix of complex elements of dimensions N*N

      DIMENSION A(N,N),B(N,N)

      COMPLEX A,B,SUM

C Read the complex A array into the eventual output array B.
      DO 5 I=1,N
        DO 10 J=1,N
          B(I,J)=A(I,J)
  10    CONTINUE
  5   CONTINUE

C Factorise that matrix into upper and lower elements.
      CALL CFACTLU(B,N)

C Invert the U matrix within B
      DO 20 K=N,2,-1
        DO 30 J=K-1,1,-1
          SUM=-B(J,K)
          DO 40 I=J+1,K-1
            SUM=SUM-B(J,I)*B(I,K)
   40     CONTINUE
          B(J,K)=SUM
   30   CONTINUE
   20 CONTINUE

C Invert the L matrix within B
      DO 50 K=1,N-1
        B(K,K)=1.0/B(K,K)
        DO 60 J=K+1,N
          SUM=CMPLX(0.0,0.0)
          DO 70 I=K,J-1
            SUM=SUM-B(J,I)*B(I,K)
  70      CONTINUE
          B(J,K)=SUM/B(J,J)
  60    CONTINUE
  50  CONTINUE
      B(N,N)=1.0/B(N,N)

C Form the product of the U and L inverses. 
      DO 80 K=1,N
        DO 90 J=1,N-1
          IF(J.GE.K) THEN
            LOLIM=J+1
            SUM=B(J,K)
          ELSE
            LOLIM=K
            SUM=CMPLX(0.0,0.0)
          ENDIF
          DO 100 I=LOLIM,N
            SUM=SUM+B(J,I)*B(I,K)
  100     CONTINUE
          B(J,K)=SUM
   90   CONTINUE
   80 CONTINUE

      RETURN
      END

C *************** CFACTLU
C Subroutine to factorise a complex N x N matrix
C into upper and lower triangular elements. The
C diagonal elements U are unity and are not stored.

      SUBROUTINE CFACTLU(A,N)

      DIMENSION A(N,N)

      COMPLEX A,SUM,PIV

C Column 1 of L is column 1 of A
C Find row 1 of U.
      PIV=A(1,1)
      DO 10 J=2,N
        A(1,J)=A(1,J)/PIV
 10   CONTINUE

C Now work through the other matrix elements. 
      DO 20 K=2,N

C Find column K of XL
        DO 30 J=K,N
          SUM=A(J,K)
          DO 40 I=1,K-1
            SUM=SUM-A(J,I)*A(I,K)
 40       CONTINUE
          A(J,K)=SUM
 30     CONTINUE

C Find row K of U except for K=N. diagonal elements
C are unity. 
        PIV=A(K,K)
        DO 50 J=K+1,N
          SUM=A(K,J)
          DO 60 I=1,K-1
            SUM=SUM-A(K,I)*A(I,J)
 60       CONTINUE
          A(K,J)=SUM/PIV
 50     CONTINUE
 20   CONTINUE
    
      RETURN
      END
