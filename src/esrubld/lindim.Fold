C    This file contains subroutines

c    W2D(ICOMP)
c    W2DMOD(NTSTEP)
c    W2DCOEF(N1,M1)
c    THLNGTH(IZ)
c    LINECL(ICOMP)
c    LAYCL(ICOMP)
c    MZCOE1L(ICOMP,LIN)
c    MZSETUL(ICOMP)
c    TOMAS2(ICOMP)
c    BRTR(ICOMP)

C This subroutine calculates the contribution of inter-construction
C heat transfers to W(I,J,5).
      SUBROUTINE W2D(ICOMP)

#include "building.h"
#include "tbridge.h"

C  Each link is characterised by the number of the LINE construction
C  (NCONL), which is even when communicating to the first surface
C  of the line and odd when communicating to the second surface
C  THE NODE OF THE FIRST CONSTRUCTION (NNOD1) AND THE NODE OF THE SECOND
C  construction (NNOD2).
      COMMON/U2D/UC2D(MLINKS),ICOML(MCOM),NCONL(MLINKS),
     &           NNOD1(MLINKS),NNOD2(MLINKS)

      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/OUTIN/IUOUT,IUIN

c  I2DIM is 1 for convex angles between surfaces (heat transfer
c  through line constructions) and 0 for concave and plane angles
c  (direct heat transfer between existing constructions).
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)
C  Common FVALCL contains the temperature array for the
c  line construction
      COMMON/FVALCL/TFCL(MCOM,MTL,MNN)
      IF(ICOMP .EQ. 1)THEN
        N1=1
      ELSE
        N1=ICOML(ICOMP-1)+1
      ENDIF
      N2=ICOML(ICOMP)
c the additional terms due to two dimensional intra-component
c  interactions is calculated and stored in
c  modified W(I,J,5) coefficients
      do 300 k=n1,n2
      J1=NCONL(K)/2
      J2=NCONL(K)-J1*2+1
      J1=J1+1
       IF(J2 .EQ. 1)THEN
         JC1=JVSS1(ICOMP,J1)
       ELSE
         JC1=JVSS2(ICOMP,J1)
       ENDIF
      JN1=NNOD1(K)
      JN2=NNOD2(K)
      IF(I2DIM(ICOMP,J1) .EQ. 1)THEN
c convex angle - communication with line construction
        bef2=w(jc1,jn1,2)
        bef5=w(jc1,jn1,5)
      jcl=2*nelts(icomp,jc1)+1
c      if(jn1 .ne. jcl)then
c      w(jc1,jn1,2)=w(jc1,jn1,2)+uc2d(k)*0.50
c      w(jc1,jn1,5)=w(jc1,jn1,5)+uc2d(k)*(tfcl(icomp,j1,jn2)
c     1-0.5*tfc(icomp,jc1,jn1))
c       else
c      w(jc1,jn1,2)=w(jc1,jn1,2)
c      w(jc1,jn1,5)=w(jc1,jn1,5)+uc2d(k)*(tfcl(icomp,j1,jn2)
c     1-tfc(icomp,jc1,jn1))
c      if(jn1 .ne. jcl .and. jn1 .ne. jcl-2
c     1.and. jn1 .ne. 1 .and. jn1 .ne. 3)then
c      w(jc1,jn1,2)=w(jc1,jn1,2)+uc2d(k)
      w(jc1,jn1,5)=w(jc1,jn1,5)+uc2d(k)*(tfcl(icomp,j1,jn2)
     1-tfc(icomp,jc1,jn1))
c      endif
      else
c convex angle - communication with line construction
       IF(J2 .EQ. 1)THEN
         JC1=JVSS1(ICOMP,J1)
         JC2=JVSS2(ICOMP,J1)
       ELSE
         JC1=JVSS2(ICOMP,J1)
         JC2=JVSS1(ICOMP,J1)
       ENDIF
      jcl=2*nelts(icomp,jc1)+1
        bef2=w(jc1,jn1,2)
        bef5=w(jc1,jn1,5)
c      w(jc1,jn1,2)=w(jc1,jn1,2)+uc2d(k)*0.50
c     w(jc1,jn1,5)=w(jc1,jn1,5)+uc2d(k)*(tfc(icomp,jc2,jn2)
c     1-0.5*tfc(icomp,jc1,jn1))
c      else
c      w(jc1,jn1,5)=w(jc1,jn1,5)+uc2d(k)*(tfc(icomp,jc2,jn2)
c     1-tfc(icomp,jc1,jn1))
c      w(jc1,jn1,2)=w(jc1,jn1,2)
c      if(jn1 .ne. jcl .and. jn1 .ne. jcl-1
c     1.and. jn1 .ne. 1 .and. jn1 .ne. 3)then
c      w(jc1,jn1,2)=w(jc1,jn1,2)+uc2d(k)
      w(jc1,jn1,5)=w(jc1,jn1,5)+uc2d(k)*(tfc(icomp,jc2,jn2)
     1-tfc(icomp,jc1,jn1))
c      endif
      endif
  300 continue
      return
      end

      SUBROUTINE W2DMOD(NTSTEP)
#include "building.h"
c  modify inter-node heat transfer coefficients
c  as well as line C coefficients for time step
c  less than an hour.
#include "tbridge.h"
      COMMON/U2D/UC2D(MLINKS),ICOML(MCOM),NCONL(MLINKS),
     &NNOD1(MLINKS),NNOD2(MLINKS)
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/U2DL/UC2DL(MLINKS),ICOMLL(MCOM),NCONLL(MLINKS),
     &NNOD1L(MLINKS),NNOD2L(MLINKS)
c    Each link between a line construction and a surface construction
c is characterised by the number of the LINE construction (NCONLL/2+1),
c    NCONLL being  even when communicating with the first surface and odd
c    when communicating with the second surface of the line construction
C  THE NODE OF THE LINE CONSTRUCTION (NNOD1L) AND THE NODE OF THE SURFACE
c    construction (NNOD2L)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PRECL/CL(MCOM,MTL,MNN,2),
     &LELTL(MCOM,MTL),QCL(MCOM,MTL)
      TSTEP=1./FLOAT(NTSTEP)
      N=ICOML(NCOMP)
      DO 10 I=1,N
   10 UC2D(I)=UC2D(i)*TSTEP
      N=ICOMLL(NCOMP)
      DO 20 I=1,N
   20 UC2DL(I)=UC2DL(I)*TSTEP
      do 30 I=1,ncomp
      n=nlin(i)
      do 30 j=1,n
      qcl(i,j)=qcl(i,j)*tstep
      do 190 k=1,LELTL(I,J)
      do 190 l=1,2
      cl(i,j,k,l)=cl(i,j,k,l)*tstep
  190 continue
   30 CONTINUE
      RETURN
      END


      SUBROUTINE W2DCOEF(N1,M1)
c  modify inter-node heat transfer coefficients
c  as well as line C coefficients for time step
c  less than an hour.
#include "building.h"
#include "tbridge.h"
      COMMON/U2D/UC2D(MLINKS),ICOML(MCOM),NCONL(MLINKS),
     &           NNOD1(MLINKS),NNOD2(MLINKS)
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/U2DL/UC2DL(MLINKS),ICOMLL(MCOM),NCONLL(MLINKS),
     &            NNOD1L(MLINKS),NNOD2L(MLINKS)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PRECL/CL(MCOM,MTL,MNN,2),LELTL(MCOM,MTL),QCL(MCOM,MTL)
      TSTEP=FLOAT(N1)/FLOAT(M1)
      N=ICOML(NCOMP)
      DO 10 I=1,N
   10 UC2D(I)=UC2D(i)*TSTEP
      N=ICOMLL(NCOMP)
      DO 20 I=1,N
   20 UC2DL(I)=UC2DL(I)*TSTEP
      do 30 I=1,ncomp
      n=nlin(i)
      do 30 j=1,n
      qcl(i,j)=qcl(i,j)*tstep
      do 190 k=1,LELTL(I,J)
      do 190 l=1,2
      cl(i,j,k,l)=cl(i,j,k,l)*tstep
  190 continue
   30 CONTINUE
      RETURN
      END


      SUBROUTINE THLNGTH(IZ)
C     This subroutine calculates the
c     lateral heat transfer characteristic length scale for each
c     surface of the zone, as well as the total thickness of the
c     construction.
c     The internal and the external thermal resistances are not included
#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/THRLENG/THRL(MS),TOTH(MS)

      DO 10 I=1,NSUR
C     THE VALUE OF THS ( CROSS-WALL CONDUCTANCE =
C      SUM OF CONDUCTIVITY*THICKNESS)
C     THRLO (DEFINED BY HASSID) AND THRL (DEFINED BY HASSID) FOR EACH
C       CONSTRUCTION IS FOUND
C       TOTH IS THE TOTAL THICKNESS
        NELT=NE(I)
        NGPS=NAIRG(I)
        THRLO=0
        TOTH(I)=0
        RES=0
        THS=0
        IP=1
        DO 20 J=1,NELT
        IF(NGPS .GT. 0 .AND. J .EQ. IPAIRG(I,IP))THEN
                RES=RES+RAIRG(I,IP)
c   Convert air gaps into layers
                IF(THK(I,J) .LT. 0.001)THK(I,J)=0.001
                CON(I,J)=THK(I,J)/RAIRG(I,IP)
                IP=IP+1
        ELSE
          THRLO=THRLO+(RES+.5*THK(I,J)/CON(I,J))*CON(I,J)*THK(I,J)
                RES=RES+THK(I,J)/CON(I,J)
                THS=THS+CON(I,J)*THK(I,J)
        ENDIF
        TOTH(I)=TOTH(I)+THK(I,J)
   20   CONTINUE
        THRL(I)=SQRT(THRLO*(1.0-THRLO/RES/THS))
   10   CONTINUE
        RETURN
        END

      SUBROUTINE LINECL(ICOMP)
c   This subroutine classifies the vertices according to the
c   lines. each line is defined by two vertices (JVL1 and JVL2)
c   as well as two surfaces (JVS1 and JVS2).
#include "building.h"
#include "tbridge.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/L1/JVL1(MTL),JVL2(MTL),JVS1(MTL),JVS2(MTL),NLINS
      COMMON/NORMV/XSUM(MS),YSUM(MS),ZSUM(MS)
      COMMON/PREC1J/GSA(MCOM,MS),GWA(MCOM,MS)
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)
c    If I2DIM is equal to zero, then the surfaces joining in line
c    MTL make an angle concave (or zero) outwards, and there is only
c    a junction between elements of neighbouring surfaces
c    If it's equal to one, then there is a construction connected to
c    the two adjacent surfaces, which has it's own storage properties
c    and external surfaces.
c    TANGLH is the tangent of half the angle between the surfaces joining
c    in line MTL
      COMMON/THBON/ITHBDG
      L=0
c    Find the vertices and the surfaces delimiting each line in the zone
      DO 900 K=1,NSUR-1
      NV=NVER(K)
      DO 901 J=1,NV
      JVN1=JVN(K,J)
      J2=J+1
      IF(J2 .GT. NV)J2=1
      JVN2=JVN(K,J2)
C   Search to find the other surface
      DO 902 KK=K+1,NSUR
      NVK=NVER(KK)
      DO 903 JJ=1,NVK
      JJ2=JJ+1
      IF(JJ2 .GT. NVK)JJ2=1
      IF((JVN1 .EQ. JVN(KK,JJ) .AND. JVN2 .EQ. JVN(KK,JJ2)) .OR.
     &(JVN1 .EQ. JVN(KK,JJ2) .AND. JVN2 .EQ. JVN(KK,JJ)))THEN
      L=L+1
      IF(L .GT. MTL)THEN
         write(iuout,*)' not enough line constructions',
     1L,'>',MTL
         write(iuout,*)' 2-d algorithm switched off'
         ithbdg=0
         return
      endif
      JVL1(L)=JVN(K,J)
      JVL2(L)=JVN(K,J2)
      JVS1(L)=K
      JVS2(L)=KK
      GO TO 905
      ENDIF
  903 CONTINUE
  902 CONTINUE
      IF(K .EQ. 1)GO TO 959
      DO 967 LL=1,L
      IF((JVL1(LL) .EQ. JVN1 .AND. JVL2(LL) .EQ. JVN2) .OR.
     &(JVL1(LL) .EQ. JVN2 .AND. JVL2(LL) .EQ. JVN1))GO TO 901
  967 CONTINUE
  959 WRITE(IUOUT,909)JVN(K,J),JVN(K,J2),K
  909 FORMAT('  Error in Geometry File',/
     &' No Matching surface for vertices ',I2,' and ',I2,
     &' of surface ',I2)
      write(iuout,*)' 2-d algorithm switched off'
      ITHBDG=0
      RETURN
  905 CONTINUE
  901 CONTINUE
  900 CONTINUE
      NLINS=L
c    find the angle between the surfaces joining in line L
      DO 846 L=1,NLINS
      K1=JVS1(L)
      K2=JVS2(L)
c   the cosine of the angle between the two surfaces is equal
c   to the dot pruduct of the surface vectors (divided by
c   half the areas)
c   the minus sign converts the angle between the lines
c   perpendicular to the surfaces to the angle between the
c   surfaces
      COSAN=(XSUM(K1)*XSUM(K2)+YSUM(K1)*YSUM(K2)+ZSUM(K1)*ZSUM(K2))
      CD=sqrt(xsum(k1)**2+ysum(k1)**2+zsum(k1)**2)
      cd=cd*sqrt(xsum(k2)**2+ysum(k2)**2+zsum(k2)**2)
      COSAN=-COSAN/CD
c   To find whether the angle between the two surfaces is concave
c   or convex,  one finds a point situated in the vicinity of
c   the middle of the line on each surface. One then
c   checks whether this point is in front of the other surface.
c   First, find in which order the two vertices are in surface K1
      NV=NVER(K1)
      DO 500 J=1,NV
      IF(JVN(K1,J) .NE. JVL1(L))GO TO 500
      IF(J .NE. NV)JN=J+1
      IF(J .EQ. NV)JN=1
      IF(JVN(K1,JN) .EQ. JVL2(L))THEN
              JJVL1=JVL1(L)
              JJVL2=JVL2(L)
              GO TO 501
      ELSE
              IF(J .NE. 1)JN=J-1
              IF(J .EQ. 1)JN=NV
              IF(JVN(K1,JN) .EQ. JVL2(L))THEN
                 JJVL1=JVL2(L)
                 JJVL2=JVL1(L)
                 GO TO 501
              ELSE
                 write(iuout,*)' something wrong with vertices',
     &' of line',l,' surface ',k1,'(inside)'
                 write(iuout,*)jvl1(l),jvl2(l)
                 write(iuout,*)(jvn(k1,k),k=1,nv)
                 write(iuout,*)' 2-d algorithm switched off'
                 ITHBDG=0
                 RETURN
              ENDIF
      ENDIF
 500  CONTINUE
      write(iuout,*)' something wrong with vertices',
     &' of line',l,' surface ',k1,'(outside)'
      write(iuout,*)' 2-d algorithm switched off'
       ITHBDG=0
       RETURN
 501  XX=YSUM(K1)*(Z(JJVL2)-Z(JJVL1))-ZSUM(K1)*(Y(JJVL2)-Z(JJVL1))
      YY=ZSUM(K1)*(X(JJVL2)-X(JJVL1))-XSUM(K1)*(Z(JJVL2)-Z(JJVL1))
      ZZ=XSUM(K1)*(Y(JJVL2)-Y(JJVL1))-YSUM(K1)*(X(JJVL2)-X(JJVL1))
c Now check whether the XX,YY,ZZ vector is "in front" of the
c K2 surface (ie if the dot product of XX and XSUM, the area vector
c of K2, is positive)
      CC=XX*XSUM(K2)+YY*YSUM(K2)+ZZ*ZSUM(K2)
      CD=0.001*(sqrt(gsa(icomp,k2)))**5
c  angle is concave or plane if CC>0 .or CC=0  - else it is convex
      IF(CC .LT. -CD)GO TO 939
      I2DIM(ICOMP,L)=0
      GO TO 846
  939 CONTINUE
      I2DIM(ICOMP,L)=1
c  now calculate the tangent of half the angle between surfaces
      IF(ABS(COSAN) .GT. 0.001)THEN
               TANAN=SQRT(1.-COSAN**2)/COSAN
c  calculate the tangent of half the angle
               TANGLH(L)=(SQRT(1.+TANAN**2)-1.)/TANAN
      ELSE
c  Valid if angle is right,
c  when value of cosine of full angle is almost zero
               TANGLH(L)=1.-COSAN
      ENDIF
  846 CONTINUE
c   The sine of the angle is the dot product of the two surface
c   direction vectors
c   store the line information for the zone
      NLIN(ICOMP)=NLINS
      DO 400 K=1,NLINS
      JVSS1(ICOMP,K)=JVS1(K)
  400 JVSS2(ICOMP,K)=JVS2(K)
      RETURN
      END

      SUBROUTINE LAYCL(ICOMP)
#include "building.h"
#include "tbridge.h"
      COMMON/U2D/UC2D(MLINKS),ICOML(MCOM),NCONL(MLINKS),
     &NNOD1(MLINKS),NNOD2(MLINKS)
      COMMON/U2DL/UC2DL(MLINKS),ICOMLL(MCOM),NCONLL(MLINKS),
     &NNOD1L(MLINKS),NNOD2L(MLINKS)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/THRLENG/THRL(MS),TOTH(MS)
      COMMON/L1/JVL1(MTL),JVL2(MTL),JVS1(MTL),JVS2(MTL),NLINS
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/PREC1/SNA(MCOM,MS),GA(MCOM,MS,MG),DA(MCOM,MS,MD),
     &ZOA(MCOM),ZGA(MCOM),ZDA(MCOM)
c  X1 and X2 of the layers of each of the two adjacent constructions
c  and are use to construct the new layers
c  NOD1 and NOD2 are the nodes of the old construction adjacent to the
c  line constructions
c  IPAIR1 and IPAR2 are used to detremine whether adjacent nodes are
c  air gap nodes (=1) or not (=0)
c  RLENG is the length of the line
      COMMON/LAYL/X1(MNN),X2(MNN),X1N(MNN),X2N(MNN),NOD1(MNN),
     &NOD2(MNN),CON11(MNN),CON22(MNN),CON1(MNN),CON2(MNN)
     &,IPAIR1(MNN),IPAIR2(MNN),RLENG
      COMMON/THBON/ITHBDG
      character*1 ANS
c  Common PRECL cintains the C coefficients for the line
c  constructions, as well as LELTL, the number of elements
c  in each line construction
      COMMON/PRECL/CL(MCOM,MTL,MNN,2),
     &LELTL(MCOM,MTL),QCL(MCOM,MTL)
      COMMON/UTL/UTEMP(MN,MN1,2),NOD2D(MN,2),NNOD2D(MN,MN1,2)
      COMMON/CONEQNL/WL(MTL,MNN,4),ITBTR
      IF(ICOMP .EQ. 1)THEN
         ITBTR=0
   9     write(6,*) ' Write internal surface ',
     &              'temperatures on channel 20 ?'
         read(5,99)ANS
  99  format(a1)
         IF(ANS .NE. 'Y' .AND. ANS .NE. 'N' .AND.
     1ANS .NE. 'y' .AND. ANS .NE. 'n')GO TO 9
         IF(ANS .EQ. 'Y' .OR. ANS .EQ. 'y')ITBTR=1
      ENDIF
      dt=3600.
      IF(ICOMP .EQ. 1)THEN
        ICOML(1)=0
        icomll(1)=0
      ELSE
        ICOML(ICOMP)=ICOML(ICOMP-1)
        icomll(icomp)=icomll(icomp-1)
      ENDIF
      DO 1 LIN=1,NLINS
c   make all the relevant quantities equal to zero
      do 190 j=1,mn
      do 190 i=1,2
      nod2d(j,i)=0
      do 190 l=1,mn1
      nnod2d(j,l,i)=0
  190 utemp(j,l,i)=0
      L1=JVL1(LIN)
      L2=JVL2(LIN)
      K1=JVS1(LIN)
      K2=JVS2(LIN)
c     length of line calculated here
      RLENG=SQRT((X(L2)-X(L1))**2+(Y(L2)-Y(L1))**2+(Z(L2)-Z(L1))**2)
c     find the coordinates of the nodes of the new construction
c     Start by finding the coordinates of the old construction
      NE1=NE(K1)
      NE1N=2*NE1+1
      NEK=0
      KK=1
      X1N(1)=0
      IF(NAIRG(K1) .NE. 0)IAIR=1
      IPAIR1(1)=0
      DO 2 K=1,NE1
      KK=KK+1
      IPAIR1(KK-1)=0
      IPAIR1(KK)=0
      NEK=NEK+1
      THKH=.5*THK(K1,NEK)
      IF(NAIRG(K1) .EQ. 0)THEN
         CON1(KK-1)=CON(K1,NEK)
      ELSE
         IF(IAIR .LE. NAIRG(K1) .AND.
     &        IPAIRG(K1,IAIR) .EQ. NEK)THEN
             CON1(KK-1)=THK(K1,NEK)/RAIRG(K1,IAIR)
             IPAIR1(KK-1)=1
C Air gap.
             IAIR=IAIR+1
         ELSE
             CON1(KK-1)=CON(K1,NEK)
         ENDIF
      ENDIF
      X1N(KK)=X1N(KK-1)+THKH
      CON1(KK)=CON1(KK-1)
      IPAIR1(KK)=IPAIR1(KK-1)
      KK=KK+1
    2 X1N(KK)=X1N(KK-1)+THKH
      X1N(NE1N)=TOTH(K1)
      CON1(NE1N)=CON1(NE1N-1)
      NE2=NE(K2)
      NE2N=2*NE2+1
      NEK=0
      KK=1
      X2N(1)=0
      IF(NAIRG(K2) .NE. 0)IAIR=1
      IPAIR2(1)=0
      DO 92 K=1,NE2
      KK=KK+1
      IPAIR2(KK-1)=0
      IPAIR2(KK)=0
      NEK=NEK+1
      THKH=.5*THK(K2,NEK)
      IF(NAIRG(K2) .EQ. 0)THEN
         CON2(KK-1)=CON(K2,NEK)
      ELSE
         IF(IAIR .LE. NAIRG(K2) .AND.
     &        IPAIRG(K2,IAIR) .EQ. NEK)THEN
             CON2(KK-1)=THK(K2,NEK)/RAIRG(K2,IAIR)
             IPAIR2(KK-1)=1
             IAIR=IAIR+1
         ELSE
             CON2(KK-1)=CON(K2,NEK)
         ENDIF
      ENDIF
      X2N(KK)=X2N(KK-1)+THKH
      CON2(KK)=CON2(KK-1)
      IPAIR2(KK)=IPAIR2(KK-1)
      KK=KK+1
   92 X2N(KK)=X2N(KK-1)+THKH
      X2N(NE2N)=TOTH(K2)
      CON2(NE2N)=CON2(NE2N-1)

C Now calculate the nodes of the new line constructions.
      J1=1
      J2=1
      IN=1
      NOD1(1)=1
      NOD2(1)=1
      CON11(1)=CON1(1)
      CON22(1)=CON2(1)
      X1(1)=0
      X2(1)=0
      TOTR=TOTH(K2)/TOTH(K1)
    3 IF(X1N(J1+1)/TOTH(K1)-X2N(J2+1)/TOTH(K2))4,5,6

C The x of the second construction is bigger - 1st construction relevant.
    4 J1=J1+1
      IN=IN+1
      X1(IN)=X1N(J1)
      X2(IN)=X1N(J1)*TOTR
      GO TO 7

C The new x is equal for both constructions - increment both.
    5 J1=J1+1
      J2=J2+1
      IN=IN+1
      X1(IN)=X1N(J1)
      X2(IN)=X2N(J2)
      GO TO 7

C The x of the first construction is bigger - 2nd construction relevant.
    6 J2=J2+1
      IN=IN+1
      X2(IN)=X2N(J2)
      X1(IN)=X2N(J2)/TOTR
    7 NOD1(IN)=J1
      NOD2(IN)=J2
      CON11(IN)=CON1(J1)
      CON22(IN)=CON2(J2)

C NOD1 and NOD2 are used to determine the material on each side of the
C junction.
      IF(J2.EQ.NE2N.AND.J1.LT.NE1N)GOTO 4
      IF(J1.EQ.NE1N.AND.J2.LT.NE2N)GOTO 6
      IF(J1.LT.NE1N.AND.J2.LT.NE2N)GOTO 3
      IF(IN.GT.MNN)THEN
           WRITE(IUOUT,*)' Not enough layers for ',K1,'-',K2,' junction'
           write(iuout,*)' 2-d algorithm switched off.'
           ITHBDG=0
           RETURN
      ENDIF
      LELTL(ICOMP,LIN)=IN
      IF(I2DIM(ICOMP,LIN) .EQ. 1)THEN

c  In case of non-concave and non-zero angle, calculate line
c  construction coefficients
                CALL MZCOE1L(ICOMP,LIN)
                IF(ITHBDG .eq. 0)return
                GO TO 152
      ENDIF

c  In case of concave or zero angle, calculate heat transfer
c  coefficients between nodes of surfaces
      DO 10 J=1,IN-1

c Find the U between the adjacent layers with node K+1 and K
      JP=J+1
c      R1=1./CON11(J)
c      R2=1./CON22(J)
      r1=toth(k1)/(x1(jp)-x1(j))/con11(j)
      r2=toth(k2)/(x2(jp)-x2(j))/con22(j)
      U=RLENG/(R1+R2)
      NOD1K=NOD1(J)
      NOD1KP=NOD1K+1
      NOD2K=NOD2(J)
      NOD2KP=NOD2K+1

c Now divide, for each side U , between the nodes JP and J
      XNODIF=X1N(NOD1KP)-X1N(NOD1K)
      U1UP=(X1(JP)-X1N(NOD1K))
      U1UM=(X1(J)-X1N(NOD1K))
      U1LP=(X1N(NOD1KP)-X1(JP))
      U1LM=(X1N(NOD1KP)-X1(J))
      U2UP=(X2(JP)-X2N(NOD2K))
      U2UM=(X2(J)-X2N(NOD2K))
      U2LP=(X2N(NOD2KP)-X2(JP))
      U2LM=(X2N(NOD2KP)-X2(J))
      XNODIF=(X1N(NOD1KP)-X1N(NOD1K))*(X2N(NOD2KP)-X2N(NOD2K))
      U1U2U=U*(U1UP*U2UP+U1UM*U2UM)/XNODIF
      U1U2L=U*(U1UP*U2LP+U1UM*U2LM)/XNODIF
      U1L2U=U*(U1LP*U2UP+U1LM*U2UM)/XNODIF
      U1L2L=U*(U1LP*U2LP+U1LM*U2LM)/XNODIF
c   check if there's already a communication between NOD1K
c   and NOD2K
c   Start with surface K1 - connections NOD1K - NOD2K
c   For each node, calculate
c    UTEMP, the heat transfer coefficient between the two nodes
c    NNOD2D, the node of other surface connected with the present node
c    NOD2D, the number of nodes of the second surface connected
c    with the node
      IF(U1L2L .LT. 0.001*U)GO TO 22
      NN1=NOD2D(NOD1K,1)
      IF(NN1 .EQ. 0)GO TO 20
      DO 21 L=1,NN1
      IF(NNOD2D(NOD1K,L,1) .EQ. NOD2K)THEN
        UTEMP(NOD1K,L,1)=UTEMP(NOD1K,L,1)+U1L2L
        GO TO 22
      ENDIF
   21 CONTINUE
   20 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD1K,1)=NN1
      NNOD2D(NOD1K,NN1,1)=NOD2K
      UTEMP(NOD1K,NN1,1)=U1L2L
   22 IF(U1L2U .LT. 0.001*U)GO TO 32
      NN1=NOD2D(NOD1K,1)
      IF(NN1 .EQ. 0)GO TO 30
      DO 31 L=1,NN1
      IF(NNOD2D(NOD1K,L,1) .EQ. NOD2KP)THEN
        UTEMP(NOD1K,L,1)=UTEMP(NOD1K,L,1)+U1L2U
        GO TO 32
      ENDIF
   31 CONTINUE
   30 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD1K,1)=NN1
      NNOD2D(NOD1K,NN1,1)=NOD2KP
      UTEMP(NOD1K,NN1,1)=U1L2U
   32 IF(U1U2L .LT. 0.001*U)GO TO 42
      NN1=NOD2D(NOD1KP,1)
      IF(NN1 .EQ. 0)GO TO 40
      DO 41 L=1,NN1
      IF(NNOD2D(NOD1KP,L,1) .EQ. NOD2K)THEN
        UTEMP(NOD1KP,L,1)=UTEMP(NOD1KP,L,1)+U1U2L
        GO TO 42
      ENDIF
   41 CONTINUE
   40 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD1KP,1)=NN1
      NNOD2D(NOD1KP,NN1,1)=NOD2K
      UTEMP(NOD1KP,NN1,1)=U1U2L
   42 IF(U1U2U .LT. 0.001*U)GO TO 52
      NN1=NOD2D(NOD1KP,1)
      IF(NN1 .EQ. 0)GO TO 50
      DO 51 L=1,NN1
      IF(NNOD2D(NOD1KP,L,1) .EQ. NOD2KP)THEN
        UTEMP(NOD1KP,L,1)=UTEMP(NOD1KP,L,1)+U1U2U
        GO TO 52
      ENDIF
   51 CONTINUE
   50 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD1KP,1)=NN1
      NNOD2D(NOD1KP,NN1,1)=NOD2KP
      UTEMP(NOD1KP,NN1,1)=U1U2U
   52 IF(U1L2L .LT. 0.001*U)GO TO 122
c    Surface K2
      NN1=NOD2D(NOD2K,2)
      IF(NN1 .EQ. 0)GO TO 120
      DO 121 L=1,NN1
      IF(NNOD2D(NOD2K,L,2) .EQ. NOD1K)THEN
        UTEMP(NOD2K,L,2)=UTEMP(NOD2K,L,2)+U1L2L
        GO TO 122
      ENDIF
  121 CONTINUE
  120 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD2K,2)=NN1
      NNOD2D(NOD2K,NN1,2)=NOD1K
      UTEMP(NOD2K,NN1,2)=U1L2L
  122 IF(U1U2L .LT. 0.001*U)GO TO 132
      NN1=NOD2D(NOD2K,2)
      IF(NN1 .EQ. 0)GO TO 130
      DO 131 L=1,NN1
      IF(NNOD2D(NOD2K,L,2) .EQ. NOD1KP)THEN
        UTEMP(NOD2K,L,2)=UTEMP(NOD2K,L,2)+U1U2L
        GO TO 132
      ENDIF
  131 CONTINUE
  130 NN1=NN1+1
      if(nn1 .gt. mn1)go to  1000
      NOD2D(NOD2K,2)=NN1
      NNOD2D(NOD2K,NN1,2)=NOD1KP
      UTEMP(NOD2K,NN1,2)=U1U2L
  132 IF(U1L2U .LT. 0.001*U)GO TO 142
      NN1=NOD2D(NOD2KP,2)
      IF(NN1 .EQ. 0)GO TO 140
      DO 141 L=1,NN1
      IF(NNOD2D(NOD2KP,L,2) .EQ. NOD1K)THEN
        UTEMP(NOD2KP,L,2)=UTEMP(NOD2KP,L,2)+U1L2U
        GO TO 142
      ENDIF
  141 CONTINUE
  140 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD2KP,2)=NN1
      NNOD2D(NOD2KP,NN1,2)=NOD1K
      UTEMP(NOD2KP,NN1,2)=U1L2U
  142 IF(U1U2U .LT. 0.001*U)GO TO 152
      NN1=NOD2D(NOD2KP,2)
      IF(NN1 .EQ. 0)GO TO 150
      DO 151 L=1,NN1
      IF(NNOD2D(NOD2KP,L,2) .EQ. NOD1KP)THEN
        UTEMP(NOD2KP,L,2)=UTEMP(NOD2KP,L,2)+U1U2U
        GO TO 152
      ENDIF
  151 CONTINUE
  150 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD2KP,2)=NN1
      NNOD2D(NOD2KP,NN1,2)=NOD1KP
      UTEMP(NOD2KP,NN1,2)=U1U2U
  152 CONTINUE
   10 CONTINUE

c   divide the coefficients by the areas of the elements
      do 201 j=1,2*ne(k1)+1
      do 201 i=1,nod2d(j,1)
      utemp(j,i,1)=utemp(j,i,1)/sna(icomp,k1)
  201 continue
      do 202 j=1,2*ne(k2)+1
      do 202 i=1,nod2d(j,2)
      utemp(j,i,2)=utemp(j,i,2)/sna(icomp,k2)
  202 continue

c  divide by the heat capacity of the element
c  to make it the term suitable for introduction in the
c  construction node equation
      if(nairg(k1) .ne. 0)ip=1
      j=0
      nc=2*ne(k1)+1
      do 250 j=1,nc
      jj=j/2
      jjp=jj+1
      if(j .eq. 1)then
           csht=den(k1,1)*sht(k1,1)*thk(k1,1)/4.
           cskt=csht
           go to 252
      elseif(j .eq. nc)then
           cskt=csht
           go to 252
      else
           cshtp=csht
           if(jj*2 .ne. j)then
              if(nairg(k1) .ne. 0 .and. jjp .eq. ipairg(k1,ip))then
                   csht=1.3*1005.*thk(k1,jjp)/4.
                   ip=ip+1
              else
                   csht=den(k1,jjp)*sht(k1,jjp)*thk(k1,jjp)/4.
              endif
           endif
           cskt=csht+cshtp
           go to 252
      endif
  252 do 250 i=1,nod2d(j,1)
      utemp(j,i,1)=utemp(j,i,1)/cskt*dt
  250 continue
      if(nairg(k2) .ne. 0)ip=1
      j=0
      nc=2*ne(k2)+1
      do 260 j=1,nc
      jj=j/2
      jjp=jj+1
      if(j .eq. 1)then
           csht=den(k2,1)*sht(k2,1)*thk(k2,1)/4.
           cskt=csht
           go to 262
      elseif(j .eq. nc)then
           cskt=csht
           go to 262
      else
           cshtp=csht
           if(jj*2 .ne. j)then
              if(nairg(k2) .ne. 0 .and. jjp .eq. ipairg(k2,ip))then
                   csht=1.3*1005.*thk(k2,jjp)/4.
                   ip=ip+1
              else
                   csht=den(k2,jjp)*sht(k2,jjp)*thk(k2,jjp)/4.
              endif
           endif
           cskt=csht+cshtp
           go to 262
      endif
  262 do 260 i=1,nod2d(j,2)
      utemp(j,i,2)=utemp(j,i,2)/cskt*dt
  260 continue

      DO 351 K=1,2*NE(K1)+1
      DO 350 J=1,NOD2D(K,1)
      ICOML(ICOMP)=ICOML(ICOMP)+1
      IF(ICOML(ICOMP) .GT. MLINKS)GO TO 2000
      JJ=ICOML(ICOMP)
      NCONL(JJ)=2*(LIN-1)
      NNOD1(JJ)=K
      NNOD2(JJ)=NNOD2D(K,J,1)
  350 UC2D(JJ)=2*UTEMP(K,J,1)
  351 continue
      DO 356 K=1,2*NE(K2)+1
      DO 355 J=1,NOD2D(K,2)
      ICOML(ICOMP)=ICOML(ICOMP)+1
      IF(ICOML(ICOMP) .GT. MLINKS)GO TO 2000
      JJ=ICOML(ICOMP)
      NCONL(JJ)=2*LIN-1
      NNOD1(JJ)=K
      NNOD2(JJ)=NNOD2D(K,J,2)
  355 UC2D(JJ)=2*UTEMP(K,J,2)
  356 continue
    1 CONTINUE
      write(6,*) ' Up to zone',icomp,icoml(icomp),' links used (max.',
     &mlinks,')'
      write(6,*) ' Up to zone',icomp,icomlL(icomp),
     &' line links used (max.',mlinks,')'
C      IF(ICOMP .EQ. 1)THEN
C        N1=1
C      ELSE
C        N1=ICOML(ICOMP-1)+1
C      ENDIF
C      N2=ICOML(ICOMP)
C      do 390 k=n1,n2
C      J1=NCONL(K)/2
C      J2=NCONL(K)-J1*2+1
C      J1=J1+1
C       IF(J2 .EQ. 1)THEN
C         JC1=JVSS1(ICOMP,J1)
C       ELSE
C         JC1=JVSS2(ICOMP,J1)
C       ENDIF
C      JN1=NNOD1(K)
C      JN2=NNOD2(K)
C      IF(I2DIM(ICOMP,J1) .EQ. 1)then
C      write(20,12)icomp,k,jc1,jn1,uc2d(k),j1,jn2
C 12    format(' ',i2,3i5,1pe10.3,2i5)
c convex angle - communication with line construction
C      else
C       IF(J2 .EQ. 1)THEN
C         JC1=JVSS1(ICOMP,J1)
C         JC2=JVSS2(ICOMP,J1)
C       ELSE
C         JC1=JVSS2(ICOMP,J1)
C         JC2=JVSS1(ICOMP,J1)
C       ENDIF
C       write(20,12)icomp,k,jc1,jn1,uc2d(k),jc2,jn2
C      endif
C  390 continue
      return
 1000 write(iuout,*)'nn1 > mn1 (',nn1,')'
      write(iuout,*)' 2-d algorithm switched off'
      ITHBDG=0
      RETURN
 2000 WRITE(IUOUT,*)' Not enough links for inter-construction',
     &' heat transfer (',MLINKS,')'
      write(iuout,*)' 2-d algorithm switched off'
      ITHBDG=0
      RETURN
      end

c ******************** MZCOE1L

c MZCOE1L is equivalent to MZCOE1, but for LINE (corner)
c constructions (not surfaces). The width of the layers of
c a line construction is different (as opposed to the
c surface constructions). The widths are calculated
c by LAYCL and transfered through COMMON LAYL
c Only convex angles are considered.

c Common block variables are:

c CL     - Contains the positive coefficients of the temperature terms
c         relating to nodes in thermal contact with the line
c         intra-construction node in question.
c QCL    - equal to DT divided by the heat capacity of the line
c         layer (kept only for first node, set equal to 0 for other
c         nodes

c Contrary to what is the case with the surfaces, forward
c differenceing is used (to avoid instability, due to the
c intra construction terms).
c In general terms any node within a multilayered construction
c is constrained to conform to the following form:

c A*TF1 + B*TF + C*TF2  =  TP + H*QP + Cross-Construction Transfers

c where:

c      A=-C(MCOM,MS,MN,1)
c      B=1+C(MCOM,MS,MN,1)+C(MCOM,MS,MN,2)
c      C=-C(MCOM,MS,MN,2)
c      H=QCL1

c      TF1 is the future temperature at 'outer' node in thermal
c      contact with node in question.

c      TF is the future temperature at node in question.

c      TF2 is the future temperature at 'inner' node in thermal
c      contact with node in question.


c      and  TP,QP are the present temperatures
c      and heat generation at the above nodes.

c      Additionally, the intraconstruction heat transfer conductances
c      are calculated
      SUBROUTINE MZCOE1L(ICOMP,LIN)
#include "building.h"
#include "tbridge.h"
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/U2DL/UC2DL(MLINKS),ICOMLL(MCOM),NCONLL(MLINKS),
     &NNOD1L(MLINKS),NNOD2L(MLINKS)
c  Common PRECL cintains the C coefficients for the line
c  constructions, as well as LELTL, the number of elements
c  in each line construction
      COMMON/UTL/UTEMP(MN,MN1,2),NOD2D(MN,2),NNOD2D(MN,MN1,2)
      COMMON/UTLL/UTEMPL(MNN,2,2),NNOD2DL(MNN,2,2)
      COMMON/PRECL/CL(MCOM,MTL,MNN,2),LELTL(MCOM,MTL),QCL(MCOM,MTL)
      COMMON/LAYL/X1(MNN),X2(MNN),X1N(MNN),X2N(MNN),NOD1(MNN),
     &  NOD2(MNN),CON11(MNN),CON22(MNN),CON1(MNN),CON2(MNN),
     &  IPAIR1(MNN),IPAIR2(MNN),RLENG

c In arrays JVSS1 and JVSS2 the surfaces bordering on the MTLth line
c of the MCOMthe surface are held. NLIN is the number of lines
c in zone MCOM
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)

c If I2DIM is equal to zero, then the surfaces joining in line
c MTL make an angle concave (or zero) outwards, and there is only
c a junction between elements of neighbouring surfaces
c If it's equal to one, then there is a construction connected to
c the two adjacent surfaces, which has it's own storage properties
c and external surfaces.
c TANGLH is the tangent of half the angle between the surfaces joining
c in line MTL.
      COMMON/THRLENG/THRL(MS),TOTH(MS)
      COMMON/FVALCL/TFCL(MCOM,MTL,MNN)
      COMMON/THBON/ITHBDG
      DT=3600.

c Make all the relevant quantities equal to zero.
      do 190 j=1,mnn
      do 190 l=1,2
      do 190 i=1,2
      nnod2dl(j,l,i)=0
  190 UTEMPL(j,l,i)=0
      do 198 j=1,mn
      do 198 i=1,2
      nod2d(j,i)=0
      do 198 l=1,mn1
      nnod2d(j,l,i)=0
  198 utemp(j,l,i)=0
      K1=JVSS1(ICOMP,LIN)
      K2=JVSS2(ICOMP,LIN)
      NN1=NELTS(ICOMP,K1)
C      NN2=NELTS(ICOMP,K2)
      NN=LELTL(ICOMP,LIN)
      TOTHH=.5*(TOTH(K1)+TOTH(K2))
      RTOTH1=TOTH(K1)/TOTHH
C      RTOTH2=TOTH(K2)/TOTHH
      WID1=TOTH(K1)
      WID2=TOTH(K2)
      CONDD=0
      CAP1=0
      CAP2=0
      DO 300 J=1,NN
      JP=J+1
      JM=J-1
      NOD1K=NOD1(J)
      NOD1KP=NOD1K+1
      IN1=(NOD1K+1)/2
      NOD2K=NOD2(J)
      NOD2KP=NOD2K+1
      IN2=(NOD2K+1)/2
      IF(J .EQ. NN)THEN
         NOD1KP=NOD1K
         NOD2KP=NOD2K
      ENDIF

c Find the effective conductance of each side.
c The conductance is found by diving the conductivity by the
c thickness of the layer associated with the node
c and multiplying it by the width of the line construction
c on each part of the node (found by half the angle between
c the constructions.
      CONDDP=CONDD
      WID1P=WID1
C      WID2P=WID2
      IF(J .LT. NN)THEN
         WID1=TOTH(K1)-.5*(X1(JP)+X1(J))
         WID2=TOTH(K2)-.5*(X2(JP)+X2(J))
         CONDD=(CON11(J)/(X1(JP)-X1(J))*WID1
     &+CON22(J)/(X2(JP)-X2(J))*WID2)*TANGLH(LIN)
      ELSE
         WID1=0
         WID2=0
         CONDD=0
      ENDIF

c Now find the effective capacity, equal to the
c thickness*specific heat*density*width product , for each
c side of the node.
      WID1M=TOTH(K1)-X1(J)
C      WID2M=TOTH(K2)-X2(J)
      CAP1P=CAP1
      CAP2P=CAP2
      CAP1=SHT(K1,IN1)*DEN(K1,IN1)
      CAP2=SHT(K2,IN2)*DEN(K2,IN2)
      IF(J .EQ. 1 .OR. J .EQ. NN)GO TO 301
      IF(IPAIR1(NOD1K) .EQ. 1)CAP1=1.3*1005.
      IF(IPAIR2(NOD2K) .EQ. 1)CAP2=1.3*1005.
 301  IF(J .LT. NN .AND. J .GT. 1)THEN
        CAPP=((CAP1+CAP2)*(X1(JP)-X1(J))*(WID1+WID1M)+
     1(CAP1P+CAP2P)*(X1(J)-X1(JM))*(WID1M+WID1P))/RTOTH1**2
     1*TANGLH(LIN)*.25
        DTC=DT/CAPP
      CL(ICOMP,LIN,J,2)=DTC*CONDD
      CL(ICOMP,LIN,J,1)=DTC*CONDDP
      ELSEIF(J .EQ. 1)THEN
        CAPP=(CAP1+CAP2)*(X1(JP)-X1(J))*(WID1+WID1M)/RTOTH1**2
     1*TANGLH(LIN)*.25
        DTC=DT/CAPP

c QCL is the flux term, which is non-zero only for the
c first (outside) node.
c       QCL(ICOMP,LIN)=DTC/CAPP*2.*TOTHH
        QCL(ICOMP,LIN)=DTC*2.*TOTHH
        CL(ICOMP,LIN,J,2)=DTC*CONDD
      ELSE
        CAPP=(CAP1P+CAP2P)*(X1(J)-X1(JM))*(WID1M+WID1P)/
     1RTOTH1**2*TANGLH(LIN)*.25
        DTC=DT/CAPP
        CL(ICOMP,LIN,J,1)=DTC*CONDDP
      ENDIF

c Now calculate the conductances to the neighbouring surfaces
      IF(J .EQ. 1)THEN
          U11=0
          U22=0
      ENDIF
      IF(J .NE. NN)THEN
          U1P=U11
          U2P=U22
c          U11=0.5*CON11(J)
c          U22=0.5*CON22(J)
           u11=con11(j)*(x1(jP)-x1(j))/toth(k1)
           u22=con22(j)*(x2(jP)-x2(j))/toth(k2)
         U1=U11+U1P
          U2=U22+U2P
      ELSE
          U1=U11
          U2=U22
      ENDIF

c Now divide, for each side U1,U2 , between the nodes NOD1K and NOD1KP
c (and nodes NOD2K and NOD2KP in the other surface)
      IF(J .NE. 1 .AND. J .NE. NN)THEN
           XNODIF=X1N(NOD1KP)-X1N(NOD1K)
           U1U=U1*(X1(J)-X1N(NOD1K))/XNODIF
           U1L=U1*(X1N(NOD1KP)-X1(J))/XNODIF
           XNODIF=X2N(NOD2KP)-X2N(NOD2K)
           U2U=U2*(X2(J)-X2N(NOD2K))/XNODIF
           U2L=U2*(X2N(NOD2KP)-X2(J))/XNODIF
           UTEMPL(J,1,1)=U1L*DTC
           UTEMPL(J,1,2)=U1U*DTC
           NNOD2DL(J,1,1)=NOD1K
           NNOD2DL(J,1,2)=NOD1KP
           UTEMPL(J,2,1)=U2L*DTC
           UTEMPL(J,2,2)=U2U*DTC
           NNOD2DL(J,2,1)=NOD2K
           NNOD2DL(J,2,2)=NOD2KP
      ELSEIF(J .EQ. 1)THEN
           NNOD2DL(J,1,1)=1
           NNOD2DL(J,1,2)=0
           UTEMPL(J,1,2)=0
           UTEMPL(J,1,1)=U1*DTC
           U1L=U1
           U1U=0
           NNOD2DL(J,2,2)=0
           NNOD2DL(J,2,1)=1
           UTEMPL(J,2,2)=0
           UTEMPL(J,2,1)=U2*DTC
           U2L=u2
           U2U=0
      ELSEIF(J .EQ. NN)THEN
           NNOD2DL(J,1,2)=NOD1KP
           NNOD2DL(J,1,1)=0
           UTEMPL(J,1,2)=U1*DTC
           UTEMPL(J,1,1)=0
           U1L=0
           U1U=U1
           NNOD2DL(J,2,2)=NOD2KP
           NNOD2DL(J,2,1)=0
           UTEMPL(J,2,2)=U2*DTC
           UTEMPL(J,2,1)=0
           U2L=0
           U2U=U2
      ENDIF
      U1U=U1U*RLENG
      U1L=U1L*RLENG
      U2U=U2U*RLENG
      U2L=U2L*RLENG
      IF(U1L .LT. 0.001*U1 .OR. J .EQ. NN)GO TO 22
      NN1=NOD2D(NOD1K,1)
      IF(NN1 .EQ. 0)GO TO 20
      DO 21 L=1,NN1
      IF(NNOD2D(NOD1K,L,1) .EQ. J)THEN
        UTEMP(NOD1K,L,1)=UTEMP(NOD1K,L,1)+U1L
        GO TO 22
      ENDIF
   21 CONTINUE
   20 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD1K,1)=NN1
      NNOD2D(NOD1K,NN1,1)=J
      UTEMP(NOD1K,NN1,1)=U1L
   22 IF(U1U .LT. 0.001*U1 .OR. J .EQ. 1)GO TO 32
      NN1=NOD2D(NOD1KP,1)
      IF(NN1 .EQ. 0)GO TO 30
      DO 31 L=1,NN1
      IF(NNOD2D(NOD1KP,L,1) .EQ. J)THEN
        UTEMP(NOD1KP,L,1)=UTEMP(NOD1KP,L,1)+U1U
        GO TO 32
      ENDIF
   31 CONTINUE
   30 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD1KP,1)=NN1
      NNOD2D(NOD1KP,NN1,1)=J
      UTEMP(NOD1KP,NN1,1)=U1U
   32 IF(U2L .LT. 0.001*U2 .OR. J .EQ. NN)GO TO 42
      NN1=NOD2D(NOD2K,2)
      IF(NN1 .EQ. 0)GO TO 40
      DO 41 L=1,NN1
      IF(NNOD2D(NOD2K,L,2) .EQ. J)THEN
        UTEMP(NOD2K,L,2)=UTEMP(NOD2K,L,2)+U2L
        GO TO 42
      ENDIF
   41 CONTINUE
   40 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD2K,2)=NN1
      NNOD2D(NOD2K,NN1,2)=J
      UTEMP(NOD2K,NN1,2)=U2L
   42 IF(U2U .LT. 0.001*U2 .OR. J .EQ. 1)GO TO 52
      NN1=NOD2D(NOD2KP,2)
      IF(NN1 .EQ. 0)GO TO 50
      DO 51 L=1,NN1
      IF(NNOD2D(NOD2KP,L,2) .EQ. J)THEN
        UTEMP(NOD2KP,L,2)=UTEMP(NOD2KP,L,2)+U2U
        GO TO 52
      ENDIF
   51 CONTINUE
   50 NN1=NN1+1
      if(nn1 .gt. mn1)go to 1000
      NOD2D(NOD2KP,2)=NN1
      NNOD2D(NOD2KP,NN1,2)=J
      UTEMP(NOD2KP,NN1,2)=U2U
   52 CONTINUE
  300 CONTINUE
      DO 450 K=1,NN
      DO 450 J=1,2
      IF(K .EQ. 1)THEN
        I2=1
      ELSE
        I2=2
      ENDIF
      IF(K .EQ. NN)THEN
        I1=2
      ELSE
        I1=1
      ENDIF
      DO 459 I=I1,I2
      JJ=ICOMLL(ICOMP)+1
      IF(UTEMPL(K,J,I) .EQ. 0)GO TO 459
      IF(JJ .GT. MLINKS)GO TO 2000
      NNOD2L(JJ)=NNOD2DL(K,J,I)
      NCONLL(JJ)=2*(LIN-1)+J/2
      NNOD1L(JJ)=K
      UC2DL(JJ)=UTEMPL(K,J,I)
      ICOMLL(ICOMP)=JJ
  459 CONTINUE
  450 CONTINUE
C      init=1
C      if(icomp .gt. 1)init=icomll(icomp-1)+1
C      do 900 k=init,icomll(icomp)
C      J1=NCONLL(K)/2
C      J2=NCONLL(K)-J1*2+1
C      J1=J1+1
C       IF(J2 .EQ. 1)THEN
C         JC1=JVSS1(ICOMP,J1)
C       ELSE
C         JC1=JVSS2(ICOMP,J1)
C       ENDIF
C      JN1=NNOD1L(K)
C      JN2=NNOD2L(K)
C       write(21,901)icomp,k,j1,jn1,jc1,jn2,cl(icomp,j1,jn1,1),
C     1cl(icomp,j1,jn1,2),uc2dl(k)
C 900  continue
C 901  format(6i5,1p3e11.4)

c Finally, all line node temperatures are initialized
c to 15 degC (as the construction temperatures)
      DO 500 J=1,MNN
  500 TFCL(ICOMP,LIN,J)=15.0
      return
 1000 write(iuout,*)'nn1 > mn1 (',nn1,')'
      write(iuout,*)' 2-d algorithm switched off'
      ITHBDG=0
      RETURN
 2000 WRITE(IUOUT,*)' Not enough links for inter-construction',
     &' heat transfer (',MLINKS,')'
      write(iuout,*)' 2-d algorithm switched off'
      ITHBDG=0
      RETURN
      END

c ******************** MZSETUL 
c MZSETUL is the equivalent of MZSETU for line constructions
c MZSETUL completes the setting-up of the time-dependent
c coefficients of the difference equations commenced in
c MZCOE1L.   The complete equation-set can then be
c solved before  MZSETU is called to solve the surface constructions.
c  simultaneously at the current simulation time-step.
c  Contrary to MZSETU, MZSETUL is not influenced by control
c  strategy - it just compute new node surfaces for the
c  line constructions from the old values of temperature.

      SUBROUTINE MZSETUL(ICOMP)
#include "building.h"
#include "tbridge.h"
      COMMON/U2DL/UC2DL(MLINKS),ICOMLL(MCOM),NCONLL(MLINKS),
     &            NNOD1L(MLINKS),NNOD2L(MLINKS)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/COE32/QSOLA(2),QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE33/QELWP(MS),QELWF(MS)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &            ARF(MCOM,MS)

      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/PRECL/CL(MCOM,MTL,MNN,2),LELTL(MCOM,MTL),QCL(MCOM,MTL)
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)

C  Common FVALCL contains the temperature array for the
c  line construction
      COMMON/FVALCL/TFCL(MCOM,MTL,MNN)

c  COMMON CONEQQNL contains the connectivity matrix and present
c  term and is equivalent to CONEQN for the surfaces
c  Since no plant interaction term is present, only four components
c  are necessary (instead of five)
c  Note also that forward differenecing (not Crank-Nicolson)
c  is used, for stability
      COMMON/CONEQNL/WL(MTL,MNN,4),ITBTR

c      common/hrout/hrof(mcom,ms)
      NCL=NLIN(ICOMP)
      DO 25 I=1,NCL

c  If the angle between the adjacent surfaces is convex or 180 degrees,
c  there is no surface construction - just a link between the
c  adjacent surfaces. In this case, go to next line
      IF(I2DIM(ICOMP,I) .EQ. 0)GO TO 25
      NN=LELTL(ICOMP,I)
      K1=JVSS1(ICOMP,I)
      K2=JVSS2(ICOMP,I)

c Consider each node in turn.
      DO 20 J=1,NN

c 'Outside' surface node. ?
      IF(J.EQ.1)goto 2

c Inside surface node ?
      IF(J.EQ.NN)goto 3

c Node is within multilayered construction: set present temperature
c of the two adjacent nodes in thermal contact with currently
c considered node.
c Complete the future coefficient set-up for this nodal equation.
      WL(I,J,1)=-CL(ICOMP,I,J,1)
      WL(I,J,2)=1.+CL(ICOMP,I,J,1)+CL(ICOMP,I,J,2)
      WL(I,J,3)=-CL(ICOMP,I,J,2)

c Set present coefficient.
      WL(I,J,4)=TFCL(ICOMP,I,J)
      goto 20

c Node is situated at 'outside' surface.
c First set adjacent (internal or external) temperatures, for
c each one of the two surfaces bordering the line construction
    2 TT1=ATF(ICOMP,K1)
      TT2=ATF(ICOMP,K2)

c ADBMO1 and ABDMO2 invoke the adiabatic case if
c IE(ICOMP,K1) or IE(ICOMP,K2) have been
c set to -5 in MZADJC.
      ADBMO1=0.
      ADBMO2=0.
      IF(IE(ICOMP,K1).EQ.-5)ADBMO1=1.
      IF(IE(ICOMP,K2).EQ.-5)ADBMO2=1.

      WL(I,J,1)=0.
      WL(I,J,2)=1.+CL(ICOMP,I,J,2)+.5*QCL(ICOMP,I)
     &*(HCOF(ICOMP,K1)*(1.-ADBMO1)+HCOF(ICOMP,K2)*(1.-ADBMO2))
      WL(I,J,3)=-CL(ICOMP,I,J,2)

C Set up present coefficient.
c (there are contributions from both surfaces joining in
c line I)
      QELWPF=QELWF(K1)*(1.-ADBMO1)+QELWF(K2)*(1.-ADBMO2)
      WL(I,J,4)=TFCL(ICOMP,I,J)+.5*QCL(ICOMP,I)
     &          *(HCOF(ICOMP,K1)*(1.-ADBMO1)*TT1+HCOF(ICOMP,K2)
     &          *(1.-ADBMO2)*TT2+QSOLE(K1,2)+QELWPF
     &          +QSOLE(K2,2)+ARF(ICOMP,K1)+ARF(ICOMP,K2))
c      if(ie(icomp,k1) .eq. 0)then
c       hrof1=(1-adbmo1)*hrof(icomp,k1)*0.5*qcl(icomp,i)
c       wl(i,j,2)=wl(i,j,2)+hrof1
c       wl(i,j,5)=wl(i,j,5)+hrof1*tfc(icomp,k1,1)
c      endif
c      if(ie(icomp,k2) .eq. 0)then
c       hrof2=(1-adbmo2)*hrof(icomp,k2)*0.5*qcl(icomp,i)
c       wl(i,j,2)=wl(i,j,2)+hrof2
c       wl(i,j,5)=wl(i,j,5)+hrof2*tfc(icomp,k2,1)
c      endif

      goto 20

c Node is situated at an inside surface
   3  WL(I,J,1)=-CL(ICOMP,I,J,1)
      WL(I,J,2)=1.+CL(ICOMP,I,J,1)
      WL(I,J,3)=0.

c Set present coefficient.
      WL(I,J,4)=TFCL(ICOMP,I,J)
   20 CONTINUE

c Add contributions of connections to neighbouring surfaces
   25 CONTINUE
      IF(ICOMP .EQ. 1)THEN
        N1=1
      ELSE
        N1=ICOMLL(ICOMP-1)+1
      ENDIF
      N2=ICOMLL(ICOMP)

c  now set the construction internal surface temperatures
c  TFC to the TFS values (bps ignores TFC(ICOMP,K,NN)
      NC=NCONST(ICOMP)
      DO 400 K=1,NC
      NN=2*NELTS(ICOMP,K)+1
 400  TFC(ICOMP,K,NN)=TFS(ICOMP,K)
      do 925 i=1,ncl
        K1=JVSS1(ICOMP,I)
        K2=JVSS2(ICOMP,I)
 925  continue

c the additional terms due to two dimensional intra-component
c  interactions is calculated and stored in
c  modified W(I,J,5) coefficients
      do 300 k=n1,n2
      J1=NCONLL(K)/2
      J2=NCONLL(K)-J1*2+1
      J1=J1+1
       IF(J2 .EQ. 1)THEN
         JC1=JVSS1(ICOMP,J1)
       ELSE
         JC1=JVSS2(ICOMP,J1)
       ENDIF
      JN1=NNOD1L(K)
      JN2=NNOD2L(K)
c      if(jn1 .eq. 1 .or. jn1 .eq. 3 .or. jn1 .eq. leltl(icomp,j1)
c     1.or. jn1 .eq. leltl(icomp,j1)-2)go to 300
      WL(J1,JN1,4)=WL(J1,JN1,4)+UC2DL(K)*TFC(ICOMP,JC1,JN2)
      WL(J1,JN1,2)=WL(J1,JN1,2)+UC2DL(K)
  300 continue

c  contrary to the original MZSETU for surfaces constructions,
c  there is no provision for plant interaction and internal
      RETURN
      END

      SUBROUTINE TOMAS2(ICOMP)

c This subroutine inverts the matrices the tri-diagonal established by
c MZSETUL, to find the temperatures of the nodes
c inside the line constructions
#include "building.h"
#include "tbridge.h"

c Common FVALCL contains the temperature array for the line construction
      COMMON/FVALCL/TFCL(MCOM,MTL,MNN)

c COMMON CONEQQNL contains the connectivity matrix and present
c term and is equivalent to CONEQN for the surfaces
c Since no plant interaction term is present,
c only four componentsc  are necessary (instead of five
      COMMON/PRECL/CL(MCOM,MTL,MNN,2),
     &LELTL(MCOM,MTL),QCL(MCOM,MTL)
      COMMON/CONEQNL/WL(MTL,MNN,4),ITBTR
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)
      NC=NLIN(ICOMP)
      DO 1 I=1,NC

C If there is no line construction (concave angle etc), go to
c next construction
      IF(I2DIM(ICOMP,I) .EQ. 0)GO TO 1
      NN=LELTL(ICOMP,I)
      DO 2 K=2,NN
      KM=K-1
      R=WL(I,K,1)/WL(I,KM,2)
      WL(I,K,2)=WL(I,K,2)-R*WL(I,KM,3)
    2 WL(I,K,4)=WL(I,K,4)-R*WL(I,KM,4)
      TFCL(ICOMP,I,NN)=WL(I,NN,4)/WL(I,NN,2)
      DO 3 K=NN-1,1,-1
      KP=K+1
    3 TFCL(ICOMP,I,K)=(WL(I,K,4)-WL(I,K,3)*TFCL(ICOMP,I,KP))
     1/WL(I,K,2)
    1 CONTINUE
      IF(ITBTR .EQ. 1)CALL BRTR(ICOMP)
      RETURN
      END

      SUBROUTINE BRTR(ICOMP)
C  This subroutine writes the values of the internal surface temperature
#include "building.h"
#include "tbridge.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALCL/TFCL(MCOM,MTL,MNN)
      COMMON/L2/JVSS1(MCOM,MTL),JVSS2(MCOM,MTL),NLIN(MCOM)
      COMMON/L3/I2DIM(MCOM,MTL),TANGLH(MTL)
      COMMON/PRECL/CL(MCOM,MTL,MNN,2),LELTL(MCOM,MTL),QCL(MCOM,MTL)

      IF(IDYP .LT. ISDS)RETURN
      NCL=NLIN(ICOMP)
      WRITE(20,*)' Day of year =',IDYP,' hour =',IHRP,
     1' zone =',ICOMP
      DO 25 I=1,NCL
      IF(I2DIM(ICOMP,I) .EQ. 0)GO TO 25
      NN=LELTL(ICOMP,I)
      K1=JVSS1(ICOMP,I)
      K2=JVSS2(ICOMP,I)
      write(20,26)I,tfcl(icomp,I,NN),K1,TFS(ICOMP,K1),K2,TFS(ICOMP,K2)
   25 CONTINUE
   26 FORMAT(' ',3(I2,1X,F7.3,1X))
      RETURN
      END

