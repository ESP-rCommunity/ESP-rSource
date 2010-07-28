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

C Miscel routines needed by grd.
C MZMISC - equivalent to subroutine in esrubld/precal.F
C MESH - equivalent to subroutine in esruish/miscel.F
C FACDRW - equivalent to subrouinte in esruish/graph.F
C POINT1 - equivalent to subroutine in esruish/point.F
C TRNSF4 & TRNSF5 - set up the local face coordinate system

c ******************** MZMISC ********************

c MZMISC computes miscellaneous data required throughout
c the simulation and for transfer to the results library
c for use during the later output retrieval.

c Common block variables are:

c ZNAME   - the name of each zone comprising the building system (from geometry.h)

c SLAT &  - site latitude and longitude
c SLON

c NCONST  - number of constructions in each component

c NELT    - number of elements in each construction

c NGAPS   - number of air gaps in each construction

c NPGAP   - the position of each gap (element number counting
c           from 'outside')

c NWINS   - number of windows in each construction

c EI & EE - emissivity of each internal and external surface
c           respectively

c AI & AE - solar absorptivity of each internal and external
c           surface respectively

      SUBROUTINE MZMISC(ICOMP)
#include "building.h"
#include "site.h"
#include "geometry.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/PREC8/SLAT,SLON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC10/NWINS(MCOM,MS)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      character outs*124

      SLAT=sitelat
      SLON=sitelongdif
      NCONST(ICOMP)=NSUR
      DO 20 I=1,NSUR
        NELTS(ICOMP,I)=NE(I)
        NGAPS(ICOMP,I)=NAIRG(I)
        NG=NAIRG(I)
        IF(NG.EQ.0)goto 1
        DO 30 J=1,NG
          NPGAP(ICOMP,I,J)=IPAIRG(I,J)
          RGAPS(ICOMP,I,J)=RAIRG(I,J)
   30   CONTINUE
    1   NWINS(ICOMP,I)=0
        NW=0
    3   EI(ICOMP,I)=EMISI(I)
        EE(ICOMP,I)=EMISE(I)
        AI(ICOMP,I)=ABSI(I)
        AE(ICOMP,I)=ABSE(I)
   20 CONTINUE

c Trace output ?
      IF(ITRACE(14).EQ.1.AND.IZNTRC(ICOMP).EQ.1)GOTO 5555
      GOTO 9999
 5555 write(outs,'(A,I4)')' Subroutine MZMISC     Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23X,A,I3,a,a)')'Zone',ICOMP,' named: ',zname(icomp)
      call edisp(itu,outs)
      ICNT=ICNT+1
      call edisp(itu," ")
      call edisp(itu,outs)
      write(outs,99941)SLAT
99941 format(' and is located at',F5.1,' latitude')
      call edisp(itu,outs)
      write(outs,99942)SLON
99942 format(18X,F5.1,' longitude')
      call edisp(itu,outs)
      write(outs,9993)NCONST(ICOMP)
 9993 format(' No.of multilayered constructions = ',I2)
      call edisp(itu,outs)
      call edisp(itu," Constr. No.of    No.of    Air Gap")
      call edisp(itu,"         Elements Air Gaps Position")
      NN=NCONST(ICOMP)
      DO 9992 I=1,NN
        I1=NELTS(ICOMP,I)
        I2=NGAPS(ICOMP,I)
        IF(I2.GT.0)goto 9991
        write(outs,9990)I,I1,I2
 9990   format(I5,I10,I9)
        call edisp(itu,outs)
        goto 9992
 9991   write(outs,9989)I,I1,I2,(NPGAP(ICOMP,I,J),J=1,I2)
 9989   format(I5,I10,I9,13I9)
        call edisp(itu,outs)
 9992 CONTINUE
      write(outs,9988)
 9988 FORMAT(' Constr. No.of   Direct                  Total',
     &'                   U-value')
      call edisp(itu," ")
      call edisp(itu,outs)
      write(outs,19988)
19988 FORMAT(9X,'Windows Solar Transmittance     Solar Transmittance')
      call edisp(itu,outs)
      call edisp(itu," ")
      call edisp(itu," ")
      call edisp(itu," Constr. Internal External Internal External")
      call edisp(itu,"          Emiss.   Emiss.   Absorp.  Absorp.")
      call edisp(itu," ")
      DO 9979 I=1,NN
      write(outs,9978)I,EI(ICOMP,I),EE(ICOMP,I),AI(ICOMP,I),
     &                AE(ICOMP,I)
 9978 format(I5,4F9.2)
      call edisp(itu,outs)
 9979 CONTINUE
 9999 RETURN
      END

C *************** MESH ***********************
C 'MESH' Computes the centre point X & Z coordinate of each rectangular
C grid square of a mesh superimposed on the opaque surface in question.
C (equivalent to subroutine in esruish/miscel.F)
      SUBROUTINE MESH(IZONE,IS)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/TC/ITC,ICNT
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/GRID11/XOP(MOX,MOZ),ZOP(MOX,MOZ)
      COMMON/GRID3/OGRIDA(MS)
      CHARACTER outs*124,h*72
      logical OK,dok

C Find max and min X & Z values for this surface.
      XMAX=-1.E+10
      ZMAX=-1.E+10
      XMIN=1.E+10
      ZMIN=1.E+10
      NV=NVER(IS)
      DO 10 I=1,NV
        XMAX=AMAX1(XMAX,XFT(I))
        ZMAX=AMAX1(ZMAX,ZFT(I))
        XMIN=AMIN1(XMIN,XFT(I))
        ZMIN=AMIN1(ZMIN,ZFT(I))
   10 CONTINUE

C Mesh length and height is given by:
      DXMESH=XMAX-XMIN
      DZMESH=ZMAX-ZMIN

C Establish grid interval for this surface.
      DIVX=DXMESH/NOX(IZONE)
      DIVZ=DZMESH/NOZ(IZONE)
      OGRIDA(IS)=DIVX*DIVZ

C Compute X & Z values for centre point of each grid square.
      DO 20 I=1,NOX(IZONE)
        DO 30 J=1,NOZ(IZONE)
          XOP(I,J)=DIVX/2.+(I-1)*DIVX
          ZOP(I,J)=DIVZ/2.+(J-1)*DIVZ
   30   CONTINUE
   20 CONTINUE

C Set up each window mesh and compute X & Z values for centre point of
C each grid square.
      NG=0

C Trace if verbose level.
    1 IF(ITC.LE.1)goto 9985
      dok=.false.
      h(1)='If you want a detailed report on the MESH generated '
      h(2)='answer yes to the question. '
      CALL ASKOK(' ',' View details of MESH coordinates?',OK,dok,2)
      IF(.NOT.OK)goto 9985

      WRITE(outs,9986)IS
 9986 FORMAT(
     &' Opaque surface grid centre coordinates for surface ',I2,':')
      call edisp(iuout,outs)
      call edisp(iuout,' NOX|NOZ|     X   |    Z')
      DO 9996 I=1,NOX(IZONE)
        DO 9995 J=1,NOZ(IZONE)
          WRITE(outs,9994)I,J,XOP(I,J),ZOP(I,J)
 9994     FORMAT(I3,',',I3,2F10.2)
          call edisp(iuout,outs)
 9995   CONTINUE
 9996 CONTINUE
      call edisp(iuout,'  ')

 9985 RETURN
      END

C ************* FACDRW *****************
C FACDRW draws the elevation of the surface.
      SUBROUTINE FACDRW(IHOUR,ICOMP,ISUR,NV,XMIN,XMAX,ZMIN,ZMAX,
     &            Xadd,Zadd,SCA)
#include "building.h"
#include "geometry.h"

      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      DIMENSION MTHNAM(12)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
#endif

      CHARACTER*72 ETEXT
      CHARACTER*3 MTHNAM
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Display header information.
      call startbuffer()
      CALL win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt

      icon=IZSTOCN(icomp,isur)
      WRITE(ETEXT,1)MTHNAM(MON),IHOUR,ISUR,SSNAME(icon)
    1 FORMAT(' Shading patterns: ',A3,' at hour',I3,' for surface',I3,
     &       ' ',a)
      iside=1
      isize=1
      ifont=2
      call viewtext(etext,iside,isize,ifont)

C Draw a scale horizontal axis, move to beginning point.
      CALL horaxis(XMIN,XMAX,igl,igr,igb,Xadd,sca,0,'X axis ->')

C Draw a scale vertical axis.
      CALL vrtaxis(ZMIN,ZMAX,igl,igb,igt,Zadd,sca,0,0,'^ Z axis')


C DRAW ZONE FACE.
      iupdown=0
      isym=0
      call etplot(XFT(1),ZFT(1),iupdown,isym)

      iupdown=1
      DO 10 I=2,NV
        call etplot(XFT(I),ZFT(I),iupdown,isym)
   10 CONTINUE
      call etplot(XFT(1),ZFT(1),iupdown,isym)
      call forceflush()

      RETURN
      END

C ************* POINT1 **********************
C 'POINT1' determines if a specified point is within or outwith any
C specified object body face boundary.
      SUBROUTINE POINT1(IS,XX,ZZ,IANS)
#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION C(26)
      IANS=0

C Determine if point XX,ZZ is within this face.
      NP=NVER(IS)
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XFT(I)
        II=II+1
        C(II)=ZFT(I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XFT(1)
      C(LAST2)=ZFT(1)
      LN=0
      IP=NP+1
      DO 20 I=2,IP
        N=2*I
        IF((C(N-2)-ZZ)*(ZZ-C(N)))20,1,2
    2   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,4
    1   IF(C(N-2)-C(N))5,6,7
    6   IF((C(N-3)-XX)*(XX-C(N-1)))20,3,3
    5   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,8
    7   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,9
    9   LN=LN-2
    8   LN=LN-1
    4   LN=LN+2
   20 CONTINUE
      IF((LN/4)*4.NE.LN)goto 3
      goto 11
    3 IANS=1
   11 RETURN
      END

C *****************************************************************
      SUBROUTINE TRNSF4(IS)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/GR3D09/XP(3),YP(3),ZP(3)

      DIMENSION  XX(MV),YY(MV),ZZ(MV)

      NVRIS=NVER(IS)
      IP1=JVN(IS,1)
      XOR=X(IP1)
      YOR=Y(IP1)
      ZOR=Z(IP1)

C Transform object body face coordinates.
      DO 10 I=1,NVRIS
        IP=JVN(IS,I)
        XX(I)=X(IP)-XOR
        YY(I)=Y(IP)-YOR
        ZZ(I)=Z(IP)-ZOR
   10 CONTINUE

C Define the local X,Y,and Z axes for the current surface.
      CALL TRNSF5(NVRIS,XX,YY,ZZ)

C Perform rotation.
      XMIN=0.
      ZMIN=0.
      DO 20 I=1,NVRIS
        XFT(I)=XX(I)*XP(1)+YY(I)*XP(2)+ZZ(I)*XP(3)
        ZFT(I)=XX(I)*ZP(1)+YY(I)*ZP(2)+ZZ(I)*ZP(3)
        XMIN=AMIN1(XMIN,XFT(I))
        ZMIN=AMIN1(ZMIN,ZFT(I))
   20 CONTINUE
      DO 30 I=1,NVRIS
        XFT(I)=XFT(I)-XMIN
        ZFT(I)=ZFT(I)-ZMIN
   30 CONTINUE
      RETURN
      END

C *****************************************************************
      SUBROUTINE TRNSF5(NVRIS,XX,YY,ZZ)
#include "building.h"

      COMMON/GR3D09/XP(3),YP(3),ZP(3)

      DIMENSION  XX(MV),YY(MV),ZZ(MV)

C Define the two vectors defining the current surface.
      XP(1)=XX(2)
      XP(2)=YY(2)
      XP(3)=ZZ(2)
      DIST=SQRT(XP(1)**2+XP(2)**2+XP(3)**2)
      XP(1)=XP(1)/DIST
      XP(2)=XP(2)/DIST
      XP(3)=XP(3)/DIST
      ZP(1)=XX(NVRIS)
      ZP(2)=YY(NVRIS)
      ZP(3)=ZZ(NVRIS)
      DIST=SQRT(ZP(1)**2+ZP(2)**2+ZP(3)**2)
      ZP(1)=ZP(1)/DIST
      ZP(2)=ZP(2)/DIST
      ZP(3)=ZP(3)/DIST
      CALL CROSS(XP,ZP,YP)
      DIST=SQRT(YP(1)**2+YP(2)**2+YP(3)**2)
      YP(1)=YP(1)/DIST
      YP(2)=YP(2)/DIST
      YP(3)=YP(3)/DIST
      CALL CROSS(YP,XP,ZP)
      DIST=SQRT(ZP(1)**2+ZP(2)**2+ZP(3)**2)
      ZP(1)=ZP(1)/DIST
      ZP(2)=ZP(2)/DIST
      ZP(3)=ZP(3)/DIST
      RETURN
      END
