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

C Contains:
C  MZMRX5 - The main controller for the matrix handling.
C  MZCMS5 - Reduces the surface node coefficient matrix.
C  CFMZBK - Evaluate surface temperatures by back substitution every CFD sweep.


C ********************* MZMRX5 *********************
C MZMRX5 - The main controller for the matrix handling.
C  Invokes the CFD algorithm. Deals only with Free-floating calculations.
      SUBROUTINE MZMRX5(ICOMP)
# include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

C Forward reduce construction matrices.
      NC=NCONST(ICOMP)
      DO 10 I=1,NC
        call MZCNF1(ICOMP,I)

C Ajust surface equation.
        call MZSAD1(ICOMP,I)
 10   CONTINUE

C Invert and solve intra-zone matrix.
      call MZCMS5(ICOMP)

C Back substitute in reduced construction matrices.
      do 20 J=1,NC
        call MZCNB1(ICOMP,J)
 20   CONTINUE

      RETURN
      END


C ********************* MZCMS5 *********************
C MZCMS5 - Reduces the coefficients of the surface 
C  node equations to the end of the forward reduction stage. 
C  For example, for a 6-sided zone:

C X X X X X X X             X     -
C X X X X X X X             X      |
C X X X X X X X             X      | Zone surface
C X X X X X X X             X      | node equations
C X X X X X X X             X      |
C X X X X X X X             X     -

C |         | |             |
C  ---------  |             |        Matrix e holds the X's
C  Surface    |             |
C coefficients|             |
C             |             |
C             |             |
C             Convection    |
C              heat transf. |
C                           |
C                          RHS

C This is reduced to:

C + + + + + + +             +
C   + + + + + +             +
C     + + + + +             +
C       + + + +             +
C         + + +             +
C           + +             +
C             +             +    This last equation in the
C             |             |    reduction relates the zone
C             b1           b3    convection heat transfer terms.

      Subroutine MZCMS5(ICOMP)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/zoneqn/e(meq,mtr)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      common/cfdctl/qfcfd

      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF

C N1 relates to the convection heat transfer.
C N2 relates to the plant term.
C N3 relates to the present term.
      NC=NCONST(ICOMP)
      N1=NC+1
      N2=NC+2
      N3=NC+3

      DO 10 I=1,NC-1
        DO 20 J=I+1,NC
          X3=e(j,i)/e(i,i)
          DO 30 k=i,N3
            sub=e(i,k)*x3
            e(j,k)=e(j,k)-sub
 30       CONTINUE
 20     CONTINUE
 10   CONTINUE

C Air temperature coefficient.
      b1=e(n1,n1)

C Plant coefficient.
      b2=e(n1,n2)

C Present term coefficient.
      b3=e(n1,n3)

C Now solve air point/plant equation as a function
C of the user-imposed control strategy.
      call mzpst1(b1,b2,b3,icomp,qfcfd)

      DO 60 I=1,NC
        II=N1-i
        SUM=e(ii,n3)-e(ii,n2)*qfcfd
        DO 70 j=ii,nc
          l=n1+ii-j
          if(l.ne.n1) sum=sum-e(ii,l)*TFS(ICOMP,l)
          if(j.ge.nc) TFS(ICOMP,ii)=sum/e(ii,ii)
 70     CONTINUE
 60   CONTINUE

      icfd=0
 98   icfd=icfd+1
      if(icfdnd(icfd).eq.ICOMP) CALL CFMNGE(ICOMP,icfd)
      if(icfd.lt.NCONF) goto 98

      RETURN
      END


C ********************* CFMZBK *********************
C CFMZBK - Evaluates surface temperatures by back substitution.
C  Surface temperatures are recalculate at each CFD iteration
C  based on convection heat transfer.
      Subroutine CFMZBK
#include "building.h"
#include "geometry.h"
#include "cfd.h"

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp) 
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/zoneqn/e(meq,mtr)

      common/cfdctl/qfcfd
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CFDARE/ATCFD(MNSBZ)
      COMMON/CFDCON/CONVF(MNZ,MNSBZ)
      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)
      COMMON/CONST/GREAT,small,GRAV

      NC=NCONST(ICP)
      N1=NC+1
      N2=NC+2
      N3=NC+3

      ALPHA=1.0
      DO 10 I=1,NC
        II=N1-I
        IF(ATCFD(II).GT.SMALL)QCFDF(ICP,II)=ALPHA*CONVF(ICFD,II)/
     &                ATCFD(II)+(1.0-ALPHA)*QCFDF(ICP,II)
        SUM=e(ii,n3)-e(ii,n2)*qfcfd-QCFDF(icp,ii)*e(ii,n1)
        DO 20 j=ii,nc
          l=n1+ii-j
          if(l.ne.n1)sum=sum-e(ii,l)*TFS(ICP,l)
          if(j.ge.nc)TFS(ICP,ii)=sum/e(ii,ii)
 20     CONTINUE
 10   CONTINUE

      RETURN
      END
