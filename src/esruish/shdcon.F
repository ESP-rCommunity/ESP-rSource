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

C SHDCON.F Contains the following routines of ESRUshd:

C SHDCON: Controls shading computation.
C SHDTST: Tests grid centre points to see if within surface.
C DECOMP: Reduces the projected 8 pt shadow 'box' to equivalent outline.

C **************** SHDCON *************************
C 'SHDCON' controls shading computation.
      SUBROUTINE SHDCON(ICOMP)
#include "building.h"
      COMMON/TC/ITC,ICNT
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GS5/NB,XO(MB),Y0(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      COMMON/SANGL1/SAZI1,SALT1
      COMMON/PREC8/SLAT,SLON
      COMMON/SHAD1/ISUNUP
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/IHTIME/IHOUR
      COMMON/SHAD3/IOSHD(MOX,MOZ)
      COMMON/SANGL2/SAZI2,SALT2
      common/ract/paction
      CHARACTER BLKNAME*8,BLKMAT*12,outs*124,paction*16
      CHARACTER*3 MTHNAM(12)

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Commence computation for each object body surface in turn.
      KOUNT=0
      DO 300 I=1,NSUR
        IF(ISC(I).EQ.1)KOUNT=KOUNT+1
  300 CONTINUE
      KOUNT=KOUNT*24
      KIND=0
      INDCOM=0
      DO 10 I=1,NSUR
        II=I
        IF(ISC(I).EQ.0)goto 10

C Partially derive transformation equations by computing 
C time independent part of equation coefficients - 
C equivalent to translation plus three-dimensional plane rotation.
        CALL TRNSF1(II)

C Transfer transformed surface face coordinates to transitional
C shading file. (only place tfile2 is called).
        CALL TFILE2(II)

C Set up surface mesh. Then proceed hour by hour to compute 
C the solar angles (relative to original coordinate system).
        CALL MESH(II)
        DO 20 J=1,24
          JJ=J
          IHOUR=J
          STIME=FLOAT(JJ)
          CALL EAZALTS(STIME,IYD,SLAT,SLON,ISUN,SAZI,SALT)
          SAZI1=SAZI
          SALT1=SALT
          ISUNUP=ISUN
          IF(ISUNUP.EQ.0)goto 1

C Compute new solar angles relative to local face coordinate system.
          CALL TRNSF2(JJ)

C Is surface self-shaded?
          IF(SAZI2.LE.90..OR.SAZI2.GE.270.)goto 2

C Complete projection transformation on a block-by-block basis.
          if(NB.eq.0)then
            call usrmsg(' No obstruction block loaded so',
     &                  ' exiting from calculation...','W')
            return
          endif
          DO 30 L=1,NB
            KK=L
            CALL TRNSF3(KK)
            CALL DECOMP(ITC,ICOMP,KK,II,JJ)
            CALL SHDTST(II)
   30     CONTINUE
          goto 1

C Surface is self-shaded.
    2     DO 40 L=1,NOX
            DO 50 M=1,NOZ
              IOSHD(L,M)=1
   50       CONTINUE
   40     CONTINUE

C Assume no default windows.

C Transfer results to transitional shading file.
    1     CALL TFILE3(ICOMP,II)
          KIND=KIND+1
          CALCOM=FLOAT(KIND)/FLOAT(KOUNT)
          WRITE(outs,'(1X,A3,A)')MTHNAM(MON),' calculation commenced'
          IF(CALCOM.GT.0.95)THEN
            IF(INDCOM.EQ.4)goto 20
            INDCOM=INDCOM+1
            if(paction(1:13).ne.'update_silent')then
              CALL USRMSG(outs,' Almost Complete','-')
            endif
          ELSEIF(CALCOM.GT.0.75)THEN
            IF(INDCOM.EQ.3)goto 20
            INDCOM=INDCOM+1
            if(paction(1:13).ne.'update_silent')then
              CALL USRMSG(outs,' 75% Complete','-')
            endif
          ELSEIF(CALCOM.GT.0.50)THEN
            IF(INDCOM.EQ.2)goto 20
            INDCOM=INDCOM+1
            if(paction(1:13).ne.'update_silent')then
              CALL USRMSG(outs,' 50% Complete','-')
            endif
          ELSEIF(CALCOM.GT.0.25)THEN
            IF(INDCOM.EQ.1)goto 20
            INDCOM=INDCOM+1
            if(paction(1:13).ne.'update_silent')then
              CALL USRMSG(outs,' 25% Complete','-')
            endif
          ENDIF
   20   CONTINUE
   10 CONTINUE
      RETURN
      END

C ****************** SHDTST *****************
C 'SHDTST' tests each grid centre point of a user-specified 
C grid (for opaque and transparent surfaces) to establish if
C the point is within or outwith the object body face and
C shadow polygon.
      SUBROUTINE SHDTST(IS)
#include "building.h"
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/SHAD2/ISHD,XS(6),ZS(6)
      COMMON/GRID11/XOP(MOX,MOZ),ZOP(MOX,MOZ)
      COMMON/SHAD3/IOSHD(MOX,MOZ)
      CHARACTER BLKNAME*8,BLKMAT*12

      IF(ISHD.EQ.0)RETURN

C For each opaque surface grid point, test whether it is within 
C the object body face boundary.  If it is, then test for whether
C it is within the shadow polygon boundary.
      DO 10 I=1,NOX
        DO 20 J=1,NOZ

C Is point within face?
          CALL POINT1(IS,XOP(I,J),ZOP(I,J),IANS)
          IF(IANS.EQ.0)goto 20

C Is point within shadow polygon?
          CALL POINT2(XOP(I,J),ZOP(I,J),IANS)
          IF(IANS.EQ.0)goto 20
          IOSHD(I,J)=1
   20   CONTINUE
   10 CONTINUE

C Assuming no default windows so return.
      RETURN
      END

C **********************  DECOMP **********************
C 'DECOMP' reduces the projected 8 point shadow 'box' to the
C equivalent shadow polygon outline.  Note points 1 to 4 refer
C to the top of the box and 5 to 8 to the bottom (anti-clockwise
C order from the top).
      SUBROUTINE DECOMP(ITRC,ICOMP,IB,II,JJ)
#include "building.h"
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/PROJ/XP(8),YP(8),ZP(8)
      COMMON/SHAD2/ISHD,XS(6),ZS(6)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      common/ract/paction
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character outs*124,BLKNAME*8,BLKMAT*12,paction*16

C If any projected point YP value > 0 then the associated obstruction
C block is deemed to be behind the object body and cannot cause shading.
C If all points >0 (ISCNT=8), then do not print the warning message.
      ICNT=0
      ISCNT=0
      ISHD=1
      DO 10 I=1,8
        IF(YP(I).GT.0.)THEN
          ISHD=0
          ISCNT=ISCNT+1
        ENDIF
   10 CONTINUE
C Debug...
C      write(6,*) 'decomp XP:',XP 
C      write(6,*) 'decomp YP:',YP 
C      write(6,*) 'decomp ZP:',ZP 
C      write(6,*) 'decomp iscnt ishd:',iscnt,ishd 
      IF(ISCNT.EQ.8)goto  11
      IF(ISHD.EQ.0)GOTO 1

C Compute shadow polygon outline.
      IF(XP(1).LE.XP(2).AND.XP(1).LE.XP(3).AND.XP(1).LE.XP(4))goto 2
      IF(XP(2).LE.XP(1).AND.XP(2).LE.XP(3).AND.XP(2).LE.XP(4))goto 3
      IF(XP(3).LE.XP(1).AND.XP(3).LE.XP(2).AND.XP(3).LE.XP(4))goto 4
      IF(XP(4).LE.XP(1).AND.XP(4).LE.XP(2).AND.XP(4).LE.XP(3))goto 5
      goto 6

C XP(1) is less:
    2 ICNT=ICNT+1
      IF(ICNT.GE.5)goto 6
      XS(1)=XP(1)
      ZS(1)=ZP(1)
      XS(2)=XP(4)
      ZS(2)=ZP(4)
      XS(3)=XP(3)
      ZS(3)=ZP(3)
      XS(4)=XP(7)
      ZS(4)=ZP(7)
      XS(5)=XP(6)
      ZS(5)=ZP(6)
      XS(6)=XP(5)
      ZS(6)=ZP(5)
      X1=XP(2)
      Z1=ZP(2)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 3
      X1=XP(8)
      Z1=ZP(8)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 3
      goto 7

C XP(2) is less:
    3 ICNT=ICNT+1
      IF(ICNT.GE.5)goto 4
      XS(1)=XP(2)
      ZS(1)=ZP(2)
      XS(2)=XP(1)
      ZS(2)=ZP(1)
      XS(3)=XP(4)
      ZS(3)=ZP(4)
      XS(4)=XP(8)
      ZS(4)=ZP(8)
      XS(5)=XP(7)
      ZS(5)=ZP(7)
      XS(6)=XP(6)
      ZS(6)=ZP(6)
      X1=XP(3)
      Z1=ZP(3)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 4
      X1=XP(5)
      Z1=ZP(5)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 4
      goto 7

C XP(3) is less:
    4 ICNT=ICNT+1
      IF(ICNT.GE.5)goto 6
      XS(1)=XP(3)
      ZS(1)=ZP(3)
      XS(2)=XP(2)
      ZS(2)=ZP(2)
      XS(3)=XP(1)
      ZS(3)=ZP(1)
      XS(4)=XP(5)
      ZS(4)=ZP(5)
      XS(5)=XP(8)
      ZS(5)=ZP(8)
      XS(6)=XP(7)
      ZS(6)=ZP(7)
      X1=XP(4)
      Z1=ZP(4)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 5
      X1=XP(6)
      Z1=ZP(6)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 5
      goto 7

C XP(4) is less:
    5 ICNT=ICNT+1
      IF(ICNT.GE.5)goto 6
      XS(1)=XP(4)
      ZS(1)=ZP(4)
      XS(2)=XP(3)
      ZS(2)=ZP(3)
      XS(3)=XP(2)
      ZS(3)=ZP(2)
      XS(4)=XP(6)
      ZS(4)=ZP(6)
      XS(5)=XP(5)
      ZS(5)=ZP(5)
      XS(6)=XP(8)
      ZS(6)=ZP(8)
      X1=XP(1)
      Z1=ZP(1)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 2
      X1=XP(7)
      Z1=ZP(7)
      CALL POINT2(X1,Z1,IANS)
      IF(IANS.EQ.0)goto 2
      goto 7

C Block crosses plane of face under consideration.
    1 continue
      ICN=IZSTOCN(icomp,ii)
      WRITE(outs,'(5A,I3)')' Block ',BLKNAME(IB),
     &' ignored (X face of ',SSNAME(ICN),') @ time',JJ
      if(paction(1:13).ne.'update_silent')then
        call edisp(iuout,outs)
      endif
      goto 11

    6 WRITE(outs,'(A,I3)')' DECOMP error - block',IB
      call edisp(icout,outs)
      goto 11

    7 CONTINUE

   11 IF(ITRC.LT.2)goto 9999
      WRITE(outs,'(A,I2,A,I4,A)')' DECOMP: Decomposition of block ',IB,
     &           ' was arrived at after ',ICNT,' iterations.'
      call edisp(icout,outs)
      IF(ICNT.GT.0)THEN
        do 12 I=1,6
          WRITE(outs,'(A,I2,3X,2F10.3)')' POINT',I,XS(I),ZS(I)
          call edisp(icout,outs)
   12   continue
      ENDIF

 9999 RETURN
      END
