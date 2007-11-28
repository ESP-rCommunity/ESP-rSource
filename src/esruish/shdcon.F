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

C SHDCON.F contains the following routines:

C SHDCON: Controls shading computation.
C SHDTST: Tests each grid centre point of a user-specified grid to
C         establish if it is within the target surface and shadow
C         polygon.
C DECOMP: Generates a modified shadow polygon for the case where the
C         projected surface crosses the plane of the target surface.
C INTERSECT: Determines the intersection point between the YT=0
C            plane and a line.

C **************** SHDCON
C Main controller for the shading computation.

      SUBROUTINE SHDCON(ICOMP)
#include "building.h"

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

      if(NB.eq.0)then
         call usrmsg('No obstruction blocks defined so',
     &               'exiting from shading calculation.','W')
         return
      endif

C Commence computation for each target surface in turn.
      KOUNT=0
      DO 10 I=1,NSUR
        IF(ISC(I).EQ.1)KOUNT=KOUNT+1
   10 CONTINUE
      KOUNT=KOUNT*24
      KIND=0
      INDCOM=0
      DO 20 ITSUR=1,NSUR
         IF(ISC(ITSUR).EQ.0)goto 20

C Partially derive transformation equations by computing time
C independent part of equation coefficients - equivalent to
C translation plus three-dimensional plane rotation.
         CALL TRNSF1(ITSUR)

C Transfer transformed surface coordinates to transitional
C shading file.
         CALL TFILE2(ITSUR)

C Set up target surface mesh.
         CALL MESH(ITSUR)

C Proceed hour-by-hour.
         DO 30 IHR=1,24
            IHOUR=IHR
            STIME=FLOAT(IHOUR)

C Determine the solar azimuth and altitude angles relative to the
C original coordinate system.
            CALL EAZALTS(STIME,IYD,SLAT,SLON,ISUN,SAZI,SALT)
            SAZI1=SAZI
            SALT1=SALT
            ISUNUP=ISUN

C Abandon shading calculation if before sunrise or after sunset.
            IF(ISUNUP.EQ.0)goto 1

C Compute pseudo solar angles relative to the local surface
C coordinate system.
            CALL TRNSF2(IHOUR)

C Terminate shading calculation if transformed surface faces away
C from the sun.
            IF(SAZI2.LE.90.0.OR.SAZI2.GE.270.0.
     &           OR.ABS(SALT2).GT.90.)goto 2

C Complete projection transformation on an obstruction block (IBN)
C and surface-by-surface (IBS) basis.
            DO 40 IBN=1,NB

C Compute X, Y and Z coordinates of obstruction block corners 
C relative to the original site coordinate system.
              CALL CORNR(IBN)

C Project obstruction block surface, form intersection with plane
C of target surface if necessary, then determine target surface
C shading polygon.
              do 50 IBS=1,6
                CALL TRNSF3(IBN,IBS)
                CALL DECOMP(IBN,IBS,ITSUR)
                CALL SHDTST(ITSUR)
   50        continue 
   40       CONTINUE
            goto 1

C Surface cannot see sun.
    2       DO 60 I=1,NOX
              DO 70 J=1,NOZ
                IOSHD(I,J)=1
   70         CONTINUE
   60       CONTINUE

C Transfer results to transitional shading file.
    1       CALL TFILE3(ICOMP,ITSUR)
            KIND=KIND+1
            CALCOM=FLOAT(KIND)/FLOAT(KOUNT)
            WRITE(outs,'(A,A3,A)')'`',MTHNAM(MON),
     &                            '` shading calculation commenced:'
            IF(CALCOM.GT.0.95)THEN
               IF(INDCOM.EQ.4)goto 30
               INDCOM=INDCOM+1
               if(paction(1:13).ne.'update_silent')then
                  CALL USRMSG(outs,' almost complete','P')
               endif
            ELSEIF(CALCOM.GT.0.75)THEN
               IF(INDCOM.EQ.3)goto 30
               INDCOM=INDCOM+1
               if(paction(1:13).ne.'update_silent')then
                  CALL USRMSG(outs,' 75% Complete','-')
               endif
            ELSEIF(CALCOM.GT.0.50)THEN
               IF(INDCOM.EQ.2)goto 30
               INDCOM=INDCOM+1
               if(paction(1:13).ne.'update_silent')then
                  CALL USRMSG(outs,' 50% Complete','-')
               endif
            ELSEIF(CALCOM.GT.0.25)THEN
               IF(INDCOM.EQ.1)goto 30
               INDCOM=INDCOM+1
               if(paction(1:13).ne.'update_silent')then
                  CALL USRMSG(outs,' 25% Complete','-')
               endif
            ENDIF
   30    CONTINUE
   20 CONTINUE

      RETURN
      END

C ****************** SHDTST *****************
C SHDTST tests each grid centre point of a user-specified grid to establish
C if it is within the target surface and shadow polygon.
      SUBROUTINE SHDTST(ITSUR)
#include "building.h"

      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/SHAD2/ISHD,NPOINT,XS(5),ZS(5)
      COMMON/GRID11/XOP(MOX,MOZ),ZOP(MOX,MOZ)
      COMMON/SHAD3/IOSHD(MOX,MOZ)
      CHARACTER BLKNAME*8,BLKMAT*12

      IF(ISHD.EQ.0)RETURN

C For each opaque surface grid point, test whether it is within 
C the target surface. If it is, then test for whether also within
C the shadow polygon.
      DO 10 I=1,NOX
        DO 20 J=1,NOZ

C Is point within face ITSUR?
          CALL POINT1(ITSUR,XOP(I,J),ZOP(I,J),IANS)
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
C If a projected surface crosses the target plane the intersection
C points are located and a modified shadow polygon produced.
      SUBROUTINE DECOMP(IBN,IBS,ITSUR)
#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/TRANSFORM/XT(4),YT(4),ZT(4)
      COMMON/PROJ/XP(4),YP(4),ZP(4)
      COMMON/SHAD2/ISHD,NPOINT,XS(5),ZS(5)
      COMMON/GS5/NB,XO(MB),Y0(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      CHARACTER CASE*40
      character outs*124
      logical tok
      dimension IPC(4)

      tok=.false.
      if(icout.eq.33)tok=.true.

C If one or more of the 4 projected point YP values are > 0 then the
C obstruction block surface crosses the X`Z` plane. Depending on the
C number of points that satisfy this condition, the surface is either
C partially or fully behind the target surface.

C Initialise array (IPC) that holds points that cross plane.
      DO 10 I=1,4
         IPC(I)=0
   10 CONTINUE

C Count number of points that cross plane (ISCNT) and set
C ISHD=1 to indicate that SHDTST should process the generated
C shadow polygon.
      ISHD=1
      ISCNT=0
      DO 20 I=1,4
        XS(I)=XP(I)
        ZS(I)=ZP(I)
        IF(YT(I).GT.0.0)THEN
          ISCNT=ISCNT+1
          IPC(ISCNT)=I
        ELSE
C Remember point not excluded for use in following 3 point
C exclusion case vertex reordering.
          INX3=I
        ENDIF
   20 CONTINUE

C For the case of 3 excluded points, possibly rearrange order
C to ensure correct intersection line selection.
      IF(ISCNT.EQ.3)THEN
         IF(INX3.eq.2)THEN
            IPC(1)=3
            IPC(2)=4
            IPC(3)=1
         ELSEIF(INX3.eq.3)THEN
            IPC(1)=4
            IPC(2)=1
            IPC(3)=2
         ENDIF
      ENDIF

C *** CASE A: No points cross => 4 point shadow polygon.
      IF(ISCNT.EQ.0)then
         CASE='Intersection case: no points cross.'
         NPOINT=4
         do 30 I=1,4
C Shadow polygon is projected surface.
            XS(I)=XP(I)
            ZS(I)=ZP(I)
   30    continue
         goto 1

C *** CASE B: 1 point crosses => 2 intersection points and a 5 point
C shadow polygon.
      ELSEIF(ISCNT.EQ.1)THEN
         CASE='Intersection case: 1 point crosses.'
         NPOINT=5
C 1st line intersection.
         IP1=IPC(1)
         IP2=IP1-1
         if(IP2.eq.0)IP2=4
         call intersect(IP1,IP2,XI,ZI)
C Point replaces excluded point.
         XS(IP1)=XI
         ZS(IP1)=ZI
C 2nd line intersection.
         IP1=IPC(1)
         IP2=IP1+1
         if(IP2.eq.5)IP2=1
         call intersect(IP1,IP2,XI,ZI)
C Point replaces next (anticlockwise) ordered point with subsequent
C points reordered.
         xs(IP2)=XI
         zs(IP2)=ZI
         if(IP2.EQ.1)then
            xs(5)=xs(4)
            xs(4)=xs(3)
            xs(3)=xs(2)
            xs(2)=xs(1)
            zs(5)=zs(4)
            zs(4)=zs(3)
            zs(3)=zs(2)
            zs(2)=zs(1)
         endif 
         if(IP2.EQ.2)then
            xs(5)=xs(4)
            xs(4)=xs(3)
            xs(3)=xs(2)
            zs(5)=zs(4)
            zs(4)=zs(3)
            zs(3)=zs(2)
         endif 
         if(IP2.EQ.3)then
            xs(5)=xs(4)
            xs(4)=xs(3)
            zs(5)=zs(4)
            zs(4)=zs(3)
         endif 
         if(IP2.EQ.4)then
            xs(5)=xs(4)
            zs(5)=zs(4)
         endif 
         goto 1

C *** CASE C: 2 points cross => 2 intersection points and a 4 point
C shadow polygon.
      ELSEIF(ISCNT.EQ.2)THEN
         CASE='Intersection case: 2 points cross.'
         NPOINT=4
C 1st line intersection.
         IP1=IPC(1)
         IP2=IP1-1
         if(IP2.eq.0)IP2=4
         if(IP2.eq.IPC(2))then
            IP2=IP1+1
            if(IP2.eq.5)IP2=1
         endif
         call intersect(IP1,IP2,XI,ZI)
C Point replaces 1st excluded point.
         XS(IP1)=XI
         ZS(IP1)=ZI
C 2nd line intersection.
         IP1=IPC(2)
         IP2=IP1+1
         if(IP2.eq.5)IP2=1
         if(IP2.eq.IPC(1))then
            IP2=IP1-1
            if(IP2.eq.0)IP2=4
         endif
         call intersect(IP1,IP2,XI,ZI)
C Point replaces 2nd excluded point.
         XS(IP1)=XI
         ZS(IP1)=ZI
         goto 1

C *** CASE D: 3 points cross => 2 intersection points and a 3 point
C shadow polygon.
      ELSEIF(ISCNT.EQ.3)THEN
         CASE='Intersection case: 3 points cross.'
         NPOINT=3
         do 40 i=1,4
            if(IPC(i).eq.0)then
C Non-excluded point becomes 1st shadow point.
               xs(1)=xp(i)
               zs(1)=zp(i)
            endif
   40    continue
C 1st line intersection.
         IP1=IPC(1)
         IP2=IP1-1
         if(IP2.eq.0)IP2=4
C         CALL ADJVN(IP1,IP2,1)
         call intersect(IP1,IP2,XI,ZI)
C Point becomes 2nd shadow point.
         XS(2)=XI
         ZS(2)=ZI
C 2nd line intersection.
         IP1=IPC(3)
         IP2=IP1+1
         if(IP2.eq.5)IP2=1
C         CALL ADJVN(IP1,IP2,2)
         call intersect(IP1,IP2,XI,ZI)
C Point becomes 2nd shadow point.
         XS(3)=XI
         ZS(3)=ZI
         goto 1

C *** CASE E: Entire surface crosses plane => abandon.
      ELSEIF(ISCNT.EQ.4)then
C Tell SHDTST to abandon shadow polygon.
         ISHD=0
         CASE='Intersection case: all points cross.'
         NPOINT=0
         goto 1
      ENDIF

C Trace output.
   1  IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      write(outs,9998)ITSUR,IBN,IBS
 9998 FORMAT('*** DECOMP: Target surface ',I2,
     &       '; obstruction block ',I2,', surface ',I2,'.')
      call edisp(icout,outs)
      call edisp(icout,case)
      if(ISCNT.NE.0)then
         write(outs,'(A,4I2)')'Excluded vertices =',(IPC(I),I=1,ISCNT)
         call edisp(icout,outs)
      endif
      call edisp(icout,'Shadow polygon (XS,ZS):')
      call edisp(icout,'     X-Coord   Y-Coord   Z-Coord')
      if(NPOINT.eq.0)then
         call edisp(icout,'None')
         goto 9999
      endif
      do 9997 I=1,NPOINT
         write(outs,'(I2,F10.1,F20.1)')I,XS(I),ZS(I)
         call edisp(icout,outs)
 9997 continue

 9999 RETURN
      END

C **********************  INTERSECT
C Determines coordinates of intersection point of YT=0 plane and
C a given line. Uses formula:
C
C X = X1 + (Y1.X1-Y1.X2)
C          -------------  
C             (Y2-Y1)

      subroutine intersect(IP1,IP2,XI,ZI)

      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRANSFORM/XT(4),YT(4),ZT(4)
      COMMON/SHAD2/ISHD,NPOINT,XS(5),ZS(5)
      character outs*124
      logical tok

      tok=.false.
      if(icout.eq.33)tok=.true.

C Determine intersection points in transformed coordinate system. Note
C that the transformed coordinates are also the projected coordinates
C because the intersection points are in the plane of the target surface.
C In all cases YI=0.0.

C Ensure that line crosses YT=0 plane by ensuring that end point
C YT values have different sign.
      if((YT(IP1).gt.0.0.and.YT(IP2).gt.0.0).or.
     &  (YT(IP1).lt.0.0.and.YT(IP2).lt.0.0))then
         call edisp(iuout,
     &      'INTERSECT ERROR: line does not cross YT=0 plane!')

C Tell SHDTST that shadow polygon should be abandoned.
         ISHD=0
         goto 1
      endif

C Avoid divide by 0.
      diff=YT(IP2)-YT(IP1)
      if(ABS(diff).lt.1.E-5)diff=1.E-5

C Intersection point.
       XI=XT(IP1)+(((YT(IP1)*XT(IP1))-(YT(IP1)*XT(IP2)))/diff)
       ZI=ZT(IP1)+(((YT(IP1)*ZT(IP1))-(YT(IP1)*ZT(IP2)))/diff)

C Trace output.
   1  IF(.NOT.tok)goto 9999
      call edisp(icout,' ')
      if(ISHD.eq.1)call edisp(icout,
     &       '*** INTERSECT: line crosses YT=0 plane.')
      if(ISHD.eq.0)call edisp(icout,
     &       '*** INTERSECT: line does not cross YT=0 plane.')
      write(outs,'(A,I1,A,I1)')'Line = ',IP1,' - ',IP2
      call edisp(icout,outs)
      write(outs,'(A,3F10.3)')'Point 1 = ',XT(IP1),YT(IP1),ZT(IP1)
      call edisp(icout,outs)
      write(outs,'(A,3F10.3)')'Point 2 = ',XT(IP2),YT(IP2),ZT(IP2)
      call edisp(icout,outs)
      write(outs,'(A,2F10.3)')'Intersection = ',XI,ZI
      call edisp(icout,outs)

 9999 RETURN
      END
